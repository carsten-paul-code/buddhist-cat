;;; tibetan-zettel-highlight.el --- Highlight and link words that have a zettel -*- lexical-binding: t -*-

;;; Commentary:
;;
;; While reading, every word that has a zettel in the Denote zettelkasten
;; is underlined; RET / mouse on it opens the zettel — in the Research
;; Emacs instance when `shared-route' is loaded (the zettel lands there
;; with its Begriffskontext panel), otherwise via the research server
;; socket, otherwise locally.  First test project: the Tantrāloka
;; reading (Sanskrit, IAST).
;;
;; The index is ONE JSON file written by `skos_index.py' in the tools
;; repo (normalized key → zettel ids, several ids per homonym key);
;; `tibetan-zettel-highlight-reload' (C-c u z H) regenerates it.  The
;; Wylie-keyed `tibetan-zettel.el' index stays what it is — it feeds the
;; gloss tier; this module only highlights.
;;
;; Inflection, not sandhi, is the matching problem: zettel keys are
;; lemmas, the text is inflected.  So the matchers never guess stems:
;;
;;   analysis buffers  — the lemma half of `- WORD — LEMMA; MORPH'
;;                       bullets under `** Word Analysis' (exact),
;;                       row 1 of the `** Gloss Tables' tables and the
;;                       `** Claude Vocabulary' headwords (surface →
;;                       lemma through the file's OWN Word Analysis),
;;                       hyphenated compounds member by member;
;;   source buffers    — the sentence's Word Analysis is PROJECTED back
;;                       onto the fused segment line: each surface member
;;                       that has a zettel is located by its middle
;;                       (first and last letter dropped, so sandhi at the
;;                       word edges does not hide it).  A verse lights up
;;                       once it has been analysed (C-c u G).
;;
;; Font-lock function matchers (the §5.35 pattern) with the progress
;; guard of §5.38-H3; the facespec is an evaluated form so each match
;; carries its ids, a keymap and help text as text properties, all
;; registered in `font-lock-extra-managed-props' so turning the mode off
;; leaves no clickable residue.

;;; Code:

(require 'cl-lib)
(require 'json)
(require 'org)
(require 'ucs-normalize)
(require 'tibetan-sanskrit-reading nil t)

(declare-function tibetan-sanskrit-reading-parse-word-analysis "tibetan-sanskrit-reading" (body))
(declare-function tibetan-analysis--analysis-buffer-name-p "tibetan-analysis-persist" (filename))
(declare-function tibetan-sentence--filepath "tibetan-sentence-persist" (sent-num &optional folder source-file))
(declare-function tibetan-cat--server-socket-path "tibetan-cat" (server-name))
(declare-function shared-route-follow "shared-route" (link))
(defvar tibetan-cat-zettel-target-server)
(defvar server-socket-dir)
(defvar tibetan-cascade-after-land-functions)

(defgroup tibetan-zettel-highlight nil
  "Highlight and link words that have a zettel."
  :group 'buddhist-cat
  :prefix "tibetan-zettel-highlight-")

(defcustom tibetan-zettel-highlight-index-file
  (expand-file-name "~/.cache/buddhist-studies/zettel-index.json")
  "JSON index written by skos_index.py (tools repo)."
  :type 'file)

(defcustom tibetan-zettel-highlight-index-script
  (expand-file-name "~/buddhist-studies/scripts/zettelkasten/skos_index.py")
  "skos_index.py (submodule mount of the tools repo)."
  :type 'file)

(defcustom tibetan-zettel-highlight-python "python3"
  "Python interpreter for the index script."
  :type 'string)

(defcustom tibetan-zettel-highlight-auto-enable t
  "Enable the mode automatically in CAT source and analysis buffers."
  :type 'boolean)

(defcustom tibetan-zettel-highlight-min-needle 3
  "Shortest projected word middle that is searched in a source line."
  :type 'integer)

(defface tibetan-zettel-highlight-face
  '((t :underline (:style line :color "SteelBlue")))
  "Face of a word that has a zettel.")

;; ============================================================================
;; Normalisation — mirrors skos_core.norm_lemma / skos_index.norm_tibetan
;; ============================================================================

(defconst tibetan-zettel-highlight--tibetan-re "[ༀ-࿿]"
  "One Tibetan-block character.")

(defun tibetan-zettel-highlight--norm-latin (s)
  "Lowercase S, strip Latin diacritics, keep only a-z0-9 (= norm_lemma)."
  (let ((nfd (ucs-normalize-NFD-string (downcase (or s "")))))
    (replace-regexp-in-string "[^a-z0-9]+" ""
                              (replace-regexp-in-string "[̀-ͯ]" "" nfd))))

(defun tibetan-zettel-highlight--norm-tibetan (s)
  "Drop tsheg, shad, spaces and punctuation from Tibetan S; keep vowel signs."
  (replace-regexp-in-string "[ \t\n༄-༔༺-༽[:punct:]]+" ""
                            (ucs-normalize-NFC-string (or s ""))))

(defun tibetan-zettel-highlight--norm-key (s)
  "Normalized index key for the term S, by script."
  (if (string-match-p tibetan-zettel-highlight--tibetan-re (or s ""))
      (tibetan-zettel-highlight--norm-tibetan s)
    (tibetan-zettel-highlight--norm-latin s)))

;; ============================================================================
;; Index
;; ============================================================================

(defvar tibetan-zettel-highlight--index nil
  "Hash: normalized key → list of zettel ids (file order).")
(defvar tibetan-zettel-highlight--entries nil
  "Hash: zettel id → plist (:id :file :title :kind :stub :sanskrit :wylie
:tibetan :german :english :etymology).")
(defvar tibetan-zettel-highlight--index-mtime nil
  "Modification time of the index file the tables were built from.")
(defvar tibetan-zettel-highlight--zk-dir nil
  "Zettelkasten directory the index was built from.")
(defvar tibetan-zettel-highlight--warned nil)

(defun tibetan-zettel-highlight--load-index (file)
  "Build the lookup tables from the JSON FILE.  Returns the entry count."
  (let* ((data (with-temp-buffer
                 (insert-file-contents file)
                 (goto-char (point-min))
                 (json-parse-buffer :object-type 'alist :array-type 'list
                                    :null-object nil :false-object nil)))
         (index (make-hash-table :test 'equal))
         (entries (make-hash-table :test 'equal))
         (n 0))
    (dolist (e (alist-get 'entries data))
      (let ((id (alist-get 'id e)))
        (cl-incf n)
        (puthash id (list :id id :file (alist-get 'file e) :title (alist-get 'title e)
                          :kind (alist-get 'kind e) :stub (alist-get 'stub e)
                          :sanskrit (alist-get 'sanskrit e) :wylie (alist-get 'wylie e)
                          :tibetan (alist-get 'tibetan e) :german (alist-get 'german e)
                          :english (alist-get 'english e)
                          :etymology (alist-get 'etymology e))
                 entries)
        (dolist (k (alist-get 'keys e))
          (puthash k (append (gethash k index) (list id)) index))))
    (setq tibetan-zettel-highlight--index index
          tibetan-zettel-highlight--entries entries
          tibetan-zettel-highlight--zk-dir (let ((d (alist-get 'zk_dir data)))
                                             (and d (not (string-empty-p d)) d))
          tibetan-zettel-highlight--index-mtime (file-attribute-modification-time
                                                 (file-attributes file)))
    n))

(defun tibetan-zettel-highlight--run-index-script ()
  "Regenerate the index file with skos_index.py; non-nil on success."
  (when (and (file-exists-p tibetan-zettel-highlight-index-script)
             (executable-find tibetan-zettel-highlight-python))
    (make-directory (file-name-directory tibetan-zettel-highlight-index-file) t)
    (zerop (call-process tibetan-zettel-highlight-python nil nil nil
                         tibetan-zettel-highlight-index-script
                         "-o" tibetan-zettel-highlight-index-file))))

(defun tibetan-zettel-highlight--ensure-index ()
  "Load the index when missing or stale; build the file first if absent.
Returns non-nil when an index is available."
  (let ((file tibetan-zettel-highlight-index-file))
    (unless (file-exists-p file)
      (tibetan-zettel-highlight--run-index-script))
    (cond
     ((not (file-exists-p file))
      (unless tibetan-zettel-highlight--warned
        (setq tibetan-zettel-highlight--warned t)
        (message "tibetan-zettel-highlight: kein Index (%s) — C-c u z H erzeugt ihn" file))
      nil)
     ((or (null tibetan-zettel-highlight--index)
          (not (equal tibetan-zettel-highlight--index-mtime
                      (file-attribute-modification-time (file-attributes file)))))
      (tibetan-zettel-highlight--load-index file)
      t)
     (t t))))

(defun tibetan-zettel-highlight-lookup (key)
  "Zettel ids under the normalized KEY, or nil."
  (and tibetan-zettel-highlight--index key
       (gethash key tibetan-zettel-highlight--index)))

(defun tibetan-zettel-highlight-entry (id)
  "The index plist of zettel ID."
  (and tibetan-zettel-highlight--entries (gethash id tibetan-zettel-highlight--entries)))

(defun tibetan-zettel-highlight--entry-path (id)
  "Absolute path of zettel ID, from the index."
  (let ((e (tibetan-zettel-highlight-entry id)))
    (and e (expand-file-name (plist-get e :file)
                             (or tibetan-zettel-highlight--zk-dir
                                 (expand-file-name "~/buddhist-studies/knowledge/zettelkasten/"))))))

(defun tibetan-zettel-highlight--ids-for-term (term)
  "Ids for TERM as written (any script), or nil."
  (let ((k (tibetan-zettel-highlight--norm-key term)))
    (and (>= (length k) 2) (tibetan-zettel-highlight-lookup k))))

(defun tibetan-zettel-highlight--strip-lemma (lemma)
  "LEMMA without a leading root sign."
  (string-trim-left (or lemma "") "[√ ]+"))

(defun tibetan-zettel-highlight--stem-in-lemma-ids (member fused)
  "Ids of the stem the inflected last MEMBER shares with the FUSED lemma.
The last member of a hyphenated compound is inflected (`kulam', `hṛdayena')
while the analysis gives the compound as one fused lemma — as a stem
(`śūnyahṛdaya') or itself inflected (`anuttarāmṛtakulam').  A stem counts
when it has a zettel, occurs inside the lemma, and the member starts with it
(its final vowel may be swallowed by the ending: hṛdaya → hṛdayena).
Candidates are the prefixes of the member and the tails of the lemma,
longest first; nothing is guessed across sandhi."
  (let* ((lemma-key (tibetan-zettel-highlight--norm-key fused))
         (member-key (tibetan-zettel-highlight--norm-key member))
         (cands (append (cl-loop for n from (length member-key) downto 3 collect (substring member-key 0 n))
                        (cl-loop for n from (length lemma-key) downto 3
                                 collect (substring lemma-key (- (length lemma-key) n)))))
         ids)
    (dolist (stem (sort (delete-dups cands) (lambda (a b) (> (length a) (length b)))))
      (unless ids
        (let ((common (let ((i 0))
                        (while (and (< i (length stem)) (< i (length member-key))
                                    (eq (aref stem i) (aref member-key i)))
                          (setq i (1+ i)))
                        i)))
          (when (and (>= common 3) (>= common (1- (length stem)))
                     (string-match-p (regexp-quote stem) lemma-key))
            (setq ids (tibetan-zettel-highlight-lookup stem))))))
    ids))

(defun tibetan-zettel-highlight--member-hits (surface lemma-map)
  "Hits in SURFACE, member by member of a hyphenated compound.
Each hit is (OFFSET LENGTH IDS).  A member resolves by its own key, else
through LEMMA-MAP (surface → lemma) when given; the last member of a
compound whose lemma is fused resolves by its stem inside that lemma
(`tibetan-zettel-highlight--stem-in-lemma-ids')."
  (let* ((members (split-string surface "-"))
         (fused (and lemma-map (cdr members)
                     (let ((lem (gethash surface lemma-map)))
                       (and lem (tibetan-zettel-highlight--strip-lemma lem)))))
         (pos 0) hits)
    (dolist (member members)
      (let ((len (length member)))
        (when (> len 0)
          (let ((ids (or (tibetan-zettel-highlight--ids-for-term member)
                         (and lemma-map
                              (let ((lem (gethash member lemma-map)))
                                (and lem (tibetan-zettel-highlight--ids-for-term
                                          (tibetan-zettel-highlight--strip-lemma lem)))))
                         (and fused (eq member (car (last members)))
                              (tibetan-zettel-highlight--stem-in-lemma-ids member fused)))))
            (when ids
              (push (list pos len ids) hits))))
        (setq pos (+ pos len 1))))
    (nreverse hits)))

;; ============================================================================
;; Buffer kinds
;; ============================================================================

(defconst tibetan-zettel-highlight--source-header-re
  "^#\\+\\(?:TIBETAN_LAYOUT\\|SOURCE_LANG\\|TIBETAN_TARGET_LANG\\|TIBETAN_TEXT_TYPE\\):"
  "A CAT source document header (within the first 4 KB).")

(defun tibetan-zettel-highlight--analysis-name-p (filename)
  (and filename
       (if (fboundp 'tibetan-analysis--analysis-buffer-name-p)
           (tibetan-analysis--analysis-buffer-name-p filename)
         (string-match-p "analysis/\\(?:seg\\|sent\\)-[0-9][^/]*\\.org$" filename))))

(defun tibetan-zettel-highlight--buffer-kind (&optional filename)
  "`analysis' for a per-segment/sentence analysis file, `source' for a CAT
source document, else nil.  Analysis files carry the same headers as their
source, so the name decides first.  FILENAME defaults to the buffer's."
  (let ((filename (or filename buffer-file-name)))
    (cond
     ((null filename) nil)
     ((tibetan-zettel-highlight--analysis-name-p filename) 'analysis)
     ((save-excursion
        (save-restriction
          (widen)
          (goto-char (point-min))
          (re-search-forward tibetan-zettel-highlight--source-header-re
                             (min (point-max) 4096) t)))
      'source))))

;; ============================================================================
;; Analysis buffers — lemma map + matcher
;; ============================================================================

(defvar-local tibetan-zettel-highlight--lemma-map-cache nil
  "(TICK . HASH) — surface → lemma from this file's Word Analysis.")

(defun tibetan-zettel-highlight--section-body (heading-re)
  "Body text of the level-2 section whose heading matches HEADING-RE, or nil."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (re-search-forward heading-re nil t)
        (forward-line 1)
        (let ((beg (point)))
          (if (re-search-forward "^\\*\\{1,2\\} " nil t)
              (buffer-substring-no-properties beg (line-beginning-position))
            (buffer-substring-no-properties beg (point-max))))))))

(defun tibetan-zettel-highlight--word-analysis-body ()
  (tibetan-zettel-highlight--section-body "^\\*\\* Word Analysis[ \t]*$"))

(defun tibetan-zettel-highlight--lemma-pairs (body)
  "((SURFACE . LEMMA)…) from a Word Analysis BODY."
  (cond
   ((or (null body) (string-empty-p (string-trim body))) nil)
   ((fboundp 'tibetan-sanskrit-reading-parse-word-analysis)
    (cl-loop for (_seg . plist) in (tibetan-sanskrit-reading-parse-word-analysis body)
             append (plist-get plist :lemma)))
   (t
    (let (pairs)
      (dolist (line (split-string body "\n"))
        (when (string-match "\\`[ \t]*-[ \t]+\\(.+?\\)[ \t]+—[ \t]+\\(.+?\\);" line)
          (push (cons (match-string 1 line) (string-trim (match-string 2 line))) pairs)))
      (nreverse pairs)))))

(defun tibetan-zettel-highlight--lemma-map ()
  "Hash surface → lemma for the current analysis buffer (cached per edit tick)."
  (let ((tick (buffer-chars-modified-tick)))
    (unless (and tibetan-zettel-highlight--lemma-map-cache
                 (eq (car tibetan-zettel-highlight--lemma-map-cache) tick))
      (let ((map (make-hash-table :test 'equal)))
        (dolist (p (tibetan-zettel-highlight--lemma-pairs
                    (tibetan-zettel-highlight--word-analysis-body)))
          (puthash (car p) (cdr p) map))
        (setq tibetan-zettel-highlight--lemma-map-cache (cons tick map))))
    (cdr tibetan-zettel-highlight--lemma-map-cache)))

(defun tibetan-zettel-highlight--l2-section-at-line ()
  "Symbol of the enclosing level-1/2 section: `word-analysis',
`gloss-tables', `claude-vocabulary' or nil."
  (save-excursion
    (goto-char (line-beginning-position))
    (when (re-search-backward "^\\*\\{1,2\\} " nil t)
      (cond ((looking-at-p "^\\*\\* Word Analysis[ \t]*$") 'word-analysis)
            ((looking-at-p "^\\*\\* Gloss Tables?[ \t]*$") 'gloss-tables)
            ((looking-at-p "^\\*\\* Claude Vocabulary[ \t]*$") 'claude-vocabulary)))))

(defun tibetan-zettel-highlight--table-row1-p ()
  "Non-nil when the current line is the first word row of a table band."
  (save-excursion
    (goto-char (line-beginning-position))
    (and (looking-at-p "[ \t]*|[^-]")
         (or (bobp)
             (progn (forward-line -1)
                    (or (not (looking-at-p "[ \t]*|"))
                        (looking-at-p "[ \t]*|-")))))))

(defun tibetan-zettel-highlight--analysis-line-hits (bol eol)
  "Hits (BEG END IDS) on the analysis line BOL..EOL, sorted by BEG."
  (let ((line (buffer-substring-no-properties bol eol))
        (section (tibetan-zettel-highlight--l2-section-at-line))
        hits)
    (cl-flet ((add (offset surface map)
                (dolist (h (tibetan-zettel-highlight--member-hits surface map))
                  (push (list (+ bol offset (nth 0 h)) (+ bol offset (nth 0 h) (nth 1 h)) (nth 2 h))
                        hits))))
      (pcase section
        ('word-analysis
         (when (string-match "\\`[ \t]*-[ \t]+\\(.+?\\)[ \t]+—[ \t]+\\(.+?\\);" line)
           (let* ((surface (match-string 1 line))
                  (sbeg (match-beginning 1))
                  (raw (match-string 2 line))
                  (start (match-beginning 2))
                  (skip (- (length raw) (length (tibetan-zettel-highlight--strip-lemma raw)))))
             ;; a hyphenated surface lights member by member — the fused lemma of a
             ;; compound (anuttarāmṛtakulam) has no key of its own
             (when (string-match-p "-" surface)
               (add sbeg surface (tibetan-zettel-highlight--lemma-map)))
             (add (+ start skip) (tibetan-zettel-highlight--strip-lemma raw) nil))))
        ('gloss-tables
         (when (tibetan-zettel-highlight--table-row1-p)
           (let ((map (tibetan-zettel-highlight--lemma-map))
                 (pos 0))
             (while (string-match "|\\([^|\n]*\\)" line pos)
               ;; save the match data first: `string-trim' is regex-based
               (let* ((cbeg (match-beginning 1))
                      (cend (match-end 0))
                      (cell (match-string 1 line))
                      (lead (- (length cell) (length (string-trim-left cell))))
                      (word (string-trim cell)))
                 (when (and (> (length word) 0)
                            (not (string-match-p "\\`[-+]+\\'" word)))
                   (add (+ cbeg lead) word map))
                 (setq pos cend))))))
        ('claude-vocabulary
         (when (string-match "\\`\\([^,\n]+?\\),[^\n]*\"" line)
           (add (match-beginning 1) (match-string 1 line)
                (tibetan-zettel-highlight--lemma-map))))))
    (sort hits (lambda (a b) (< (car a) (car b))))))

(defvar tibetan-zettel-highlight--current-ids nil
  "Ids of the match the facespec is being evaluated for.")

(defun tibetan-zettel-highlight--run-matcher (limit line-hits-fn)
  "Font-lock matcher core: next hit at or after point before LIMIT.
LINE-HITS-FN gets (BOL EOL) and returns sorted hits (BEG END IDS)."
  (let (found)
    (while (and (not found) (< (point) limit))
      (let* ((bol (line-beginning-position))
             (eol (line-end-position))
             (hit (cl-find-if (lambda (h) (and (>= (car h) (point)) (<= (nth 1 h) limit)))
                              (funcall line-hits-fn bol eol))))
        (if hit
            (progn
              (set-match-data (list (nth 0 hit) (nth 1 hit)))
              (setq tibetan-zettel-highlight--current-ids (nth 2 hit))
              (goto-char (nth 1 hit))
              (setq found t))
          ;; §5.38-H3: guarantee progress on a final line without newline.
          (let ((before (point)))
            (forward-line 1)
            (when (<= (point) before)
              (goto-char limit))))))
    found))

(defun tibetan-zettel-highlight--analysis-matcher (limit)
  "Font-lock matcher for analysis buffers."
  (tibetan-zettel-highlight--run-matcher limit #'tibetan-zettel-highlight--analysis-line-hits))

;; ============================================================================
;; Source buffers — projection from the sentence's Word Analysis
;; ============================================================================

(defvar-local tibetan-zettel-highlight--projections nil
  "Hash sentence number → list of (NEEDLE . IDS), or `none'.")

(defun tibetan-zettel-highlight--needle (member)
  "MEMBER without its first and last character (sandhi happens at the edges)."
  (if (>= (length member) 4)
      (substring member 1 -1)
    member))

(defun tibetan-zettel-highlight--projection-from-body (body)
  "((NEEDLE . IDS)…) for the Word Analysis BODY: every surface member that
has a zettel (by its own key or through its lemma), as the middle of the
written form."
  (let* ((pairs (tibetan-zettel-highlight--lemma-pairs body))
         (map (make-hash-table :test 'equal))
         out)
    (dolist (p pairs) (puthash (car p) (cdr p) map))
    (dolist (p pairs)
      (let* ((surface (car p))
             (members (split-string surface "-" t)))
        (dolist (h (tibetan-zettel-highlight--member-hits surface map))
          (let* ((member (cl-find-if (lambda (m) (= (length m) (nth 1 h))) members))
                 (member (or member (substring surface (nth 0 h) (+ (nth 0 h) (nth 1 h)))))
                 (needle (tibetan-zettel-highlight--needle member))
                 (item (cons needle (nth 2 h))))
            (when (and (>= (length needle) tibetan-zettel-highlight-min-needle)
                       (not (member item out)))
              (push item out))))))
    (nreverse out)))

(defun tibetan-zettel-highlight--analysis-folder ()
  (and buffer-file-name
       (expand-file-name "analysis/" (file-name-directory buffer-file-name))))

(defun tibetan-zettel-highlight--sentence-word-analysis (sent-num)
  "The `** Word Analysis' body of the analysis file of sentence SENT-NUM, or nil."
  (let* ((folder (tibetan-zettel-highlight--analysis-folder))
         (file (and folder
                    (if (fboundp 'tibetan-sentence--filepath)
                        (tibetan-sentence--filepath sent-num folder buffer-file-name)
                      (expand-file-name (format "sent-%03d.org" sent-num) folder)))))
    (when (and file (file-readable-p file))
      (with-temp-buffer
        (insert-file-contents file)
        (tibetan-zettel-highlight--word-analysis-body)))))

(defun tibetan-zettel-highlight--projection (sent-num)
  "Cached projection for SENT-NUM (nil when the sentence has no Word Analysis)."
  (unless tibetan-zettel-highlight--projections
    (setq tibetan-zettel-highlight--projections (make-hash-table :test 'eql)))
  (let ((cached (gethash sent-num tibetan-zettel-highlight--projections 'missing)))
    (if (not (eq cached 'missing))
        (and (not (eq cached 'none)) cached)
      (let ((proj (condition-case nil
                      (tibetan-zettel-highlight--projection-from-body
                       (tibetan-zettel-highlight--sentence-word-analysis sent-num))
                    (error nil))))
        (puthash sent-num (or proj 'none) tibetan-zettel-highlight--projections)
        proj))))

(defun tibetan-zettel-highlight--segment-line-sentence ()
  "Sentence number when the current line is a body line of a `**** Segment'
heading, else nil."
  (save-excursion
    (goto-char (line-beginning-position))
    (when (and (not (looking-at-p "^\\*+ "))
               (not (looking-at-p "^[ \t]*\\(?::\\|#\\)"))
               (re-search-backward "^\\*+ " nil t)
               (looking-at-p "^\\*\\*\\*\\* Segment "))
      (when (re-search-backward "^\\*\\*\\* Sentence[ \t]+\\([0-9]+\\)" nil t)
        (string-to-number (match-string 1))))))

(defconst tibetan-zettel-highlight--iast-char "[[:alpha:]āīūṛṝḷḹṃḥśṣṭḍṇñṅ']"
  "One IAST letter (incl. avagraha).")

(defun tibetan-zettel-highlight--source-line-hits (bol eol)
  "Hits (BEG END IDS) on the source line BOL..EOL from the sentence's projection."
  (let ((sent (tibetan-zettel-highlight--segment-line-sentence)))
    (when sent
      (let ((proj (tibetan-zettel-highlight--projection sent))
            (line (buffer-substring-no-properties bol eol))
            (case-fold-search nil)
            hits)
        (dolist (item proj)
          (let ((start 0) (needle (car item)))
            (while (and (< start (length line)) (string-match (regexp-quote needle) line start))
              (let* ((b (match-beginning 0)) (e (match-end 0)) (mend e))
                ;; extend by one letter on each side — the edges the needle dropped
                (when (and (> b 0) (string-match-p tibetan-zettel-highlight--iast-char (substring line (1- b) b)))
                  (setq b (1- b)))
                (when (and (< e (length line)) (string-match-p tibetan-zettel-highlight--iast-char (substring line e (1+ e))))
                  (setq e (1+ e)))
                (unless (cl-find-if (lambda (h) (and (< (car h) (+ bol e)) (> (nth 1 h) (+ bol b)))) hits)
                  (push (list (+ bol b) (+ bol e) (cdr item)) hits))
                (setq start mend)))))
        (sort hits (lambda (a b) (< (car a) (car b))))))))

(defun tibetan-zettel-highlight--source-matcher (limit)
  "Font-lock matcher for CAT source buffers."
  (tibetan-zettel-highlight--run-matcher limit #'tibetan-zettel-highlight--source-line-hits))

(defun tibetan-zettel-highlight--invalidate-projections (&rest _)
  "Forget cached projections in every source buffer and refontify."
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when (and (bound-and-true-p tibetan-zettel-highlight-mode)
                 tibetan-zettel-highlight--projections)
        (setq tibetan-zettel-highlight--projections nil)
        (when font-lock-mode (font-lock-flush))))))

;; ============================================================================
;; Following a marked word
;; ============================================================================

(defun tibetan-zettel-highlight-ids-at-point ()
  "Zettel ids of the marked word at point, or nil."
  (get-text-property (point) 'tibetan-zettel-id))

(defun tibetan-zettel-highlight--choose-id (ids)
  "One id from IDS — asked by title when there are several (homonyms)."
  (if (cdr ids)
      (let* ((pairs (mapcar (lambda (id) (cons (or (plist-get (tibetan-zettel-highlight-entry id) :title) id) id)) ids))
             (choice (completing-read "Zettel: " (mapcar #'car pairs) nil t)))
        (or (cdr (assoc choice pairs)) (car ids)))
    (car ids)))

(defun tibetan-zettel-highlight--research-reachable-p ()
  "Non-nil when the Research instance's server socket exists."
  (let ((server (if (boundp 'tibetan-cat-zettel-target-server) tibetan-cat-zettel-target-server "research-emacs")))
    (if (fboundp 'tibetan-cat--server-socket-path)
        (tibetan-cat--server-socket-path server)
      (require 'server)
      (file-exists-p (expand-file-name server server-socket-dir)))))

(defun tibetan-zettel-highlight--open-id (id)
  "Open zettel ID: shared-route (Research), else research socket, else here."
  (let ((path (tibetan-zettel-highlight--entry-path id)))
    (cond
     ((and (fboundp 'shared-route-follow) (fboundp 'denote-get-path-by-id))
      (shared-route-follow id))
     ((and path (tibetan-zettel-highlight--research-reachable-p))
      (call-process "emacsclient" nil 0 nil
                    "-s" (if (boundp 'tibetan-cat-zettel-target-server) tibetan-cat-zettel-target-server "research-emacs")
                    "--no-wait" path))
     (path (find-file path))
     (t (user-error "Zettel %s nicht im Index" id)))))

(defun tibetan-zettel-highlight-open (&optional ids)
  "Open the zettel of the marked word at point (IDS when given)."
  (interactive)
  (let ((ids (or ids (tibetan-zettel-highlight-ids-at-point))))
    (unless ids (user-error "Kein Zettel-Wort am Punkt"))
    (tibetan-zettel-highlight--open-id (tibetan-zettel-highlight--choose-id ids))))

(defun tibetan-zettel-highlight-return ()
  "RET on a marked word opens its zettel; elsewhere behaves like `org-return'."
  (interactive)
  (let ((ids (tibetan-zettel-highlight-ids-at-point)))
    (if ids
        (tibetan-zettel-highlight-open ids)
      (org-return))))

(defun tibetan-zettel-highlight-mouse-open (event)
  "Open the zettel of the word clicked in EVENT."
  (interactive "e")
  (mouse-set-point event)
  (tibetan-zettel-highlight-open))

(defun tibetan-zettel-highlight--open-at-point-function ()
  "For `org-open-at-point-functions': open the marked word, non-nil when done."
  (when (tibetan-zettel-highlight-ids-at-point)
    (tibetan-zettel-highlight-open)
    t))

(defvar tibetan-zettel-highlight-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'tibetan-zettel-highlight-return)
    (define-key map [mouse-1] #'tibetan-zettel-highlight-mouse-open)
    (define-key map [mouse-2] #'tibetan-zettel-highlight-mouse-open)
    map)
  "Keymap active on a marked word.")

(defun tibetan-zettel-highlight--entry-line (id)
  "`title — German / English' of zettel ID (just the id when unknown)."
  (let ((e (tibetan-zettel-highlight-entry id)))
    (if e
        (string-join (delq nil (list (plist-get e :title)
                                     (let ((de (plist-get e :german)) (en (plist-get e :english)))
                                       (cond ((and de en (not (string-empty-p de)) (not (string-empty-p en)))
                                              (format "— %s / %s" de en))
                                             ((and de (not (string-empty-p de))) (format "— %s" de))
                                             ((and en (not (string-empty-p en))) (format "— %s" en))))))
                     " ")
      id)))

(defun tibetan-zettel-highlight--etymology (id)
  "The Wortbildung of zettel ID (`:ETYMOLOGY:` in the drawer), or nil when empty."
  (let ((ety (plist-get (tibetan-zettel-highlight-entry id) :etymology)))
    (and ety (not (string-empty-p ety)) ety)))

(defun tibetan-zettel-highlight--help-echo (ids)
  "Tooltip for IDS: one `title — de / en' per zettel, its Wortbildung underneath."
  (mapconcat (lambda (id)
               (let ((ety (tibetan-zettel-highlight--etymology id)))
                 (concat (tibetan-zettel-highlight--entry-line id)
                         (if ety (concat "\nWortbildung: " ety) ""))))
             ids "\n"))

(defun tibetan-zettel-highlight-etymology (&optional kill)
  "Show the Wortbildung of the marked word at point: lemma — de / en — parts.
The decomposition comes from the zettel's `:ETYMOLOGY:' (via the index), so
it is the same line the Research panel and the browser view show.  Several
zettels (homonyms) give one line each.  With prefix KILL the text also goes
to the kill ring.  Without a zettel at point: /term creates one."
  (interactive "P")
  (let ((ids (tibetan-zettel-highlight-ids-at-point)))
    (unless ids
      (user-error "Kein Zettel-Wort am Punkt — /term legt einen Zettel an"))
    (let ((text (mapconcat (lambda (id)
                             (concat (tibetan-zettel-highlight--entry-line id) " — "
                                     (or (tibetan-zettel-highlight--etymology id)
                                         "keine Wortbildung im Zettel")))
                           ids "\n")))
      (when kill (kill-new text))
      (message "%s" text))))

(defconst tibetan-zettel-highlight--props
  '(tibetan-zettel-id keymap mouse-face help-echo rear-nonsticky)
  "Text properties a match carries; font-lock manages them.")

(defun tibetan-zettel-highlight--facespec ()
  "Facespec for the match just found: face plus the clickable properties."
  (let ((ids tibetan-zettel-highlight--current-ids))
    (list 'face 'tibetan-zettel-highlight-face
          'tibetan-zettel-id ids
          'keymap tibetan-zettel-highlight-map
          'mouse-face 'highlight
          'help-echo (concat (tibetan-zettel-highlight--help-echo ids) "\nRET: Zettel öffnen")
          'rear-nonsticky '(tibetan-zettel-id keymap mouse-face help-echo))))

;; ============================================================================
;; Minor mode
;; ============================================================================

(defconst tibetan-zettel-highlight--analysis-keywords
  '((tibetan-zettel-highlight--analysis-matcher (0 (tibetan-zettel-highlight--facespec) prepend))))
(defconst tibetan-zettel-highlight--source-keywords
  '((tibetan-zettel-highlight--source-matcher (0 (tibetan-zettel-highlight--facespec) prepend))))

(defvar-local tibetan-zettel-highlight--keywords nil
  "The keyword list this buffer added, for removal.")

(defun tibetan-zettel-highlight--enable (kind)
  "Install matcher and properties for buffer KIND (`analysis' or `source')."
  (tibetan-zettel-highlight--ensure-index)
  (setq tibetan-zettel-highlight--keywords
        (if (eq kind 'source)
            tibetan-zettel-highlight--source-keywords
          tibetan-zettel-highlight--analysis-keywords))
  (setq-local font-lock-extra-managed-props
              (cl-union font-lock-extra-managed-props tibetan-zettel-highlight--props))
  (font-lock-add-keywords nil tibetan-zettel-highlight--keywords 'append)
  (add-hook 'org-open-at-point-functions #'tibetan-zettel-highlight--open-at-point-function nil t)
  (when font-lock-mode (font-lock-flush)))

(defun tibetan-zettel-highlight--disable ()
  (when tibetan-zettel-highlight--keywords
    (font-lock-remove-keywords nil tibetan-zettel-highlight--keywords)
    (setq tibetan-zettel-highlight--keywords nil))
  (remove-hook 'org-open-at-point-functions #'tibetan-zettel-highlight--open-at-point-function t)
  (with-silent-modifications
    (remove-list-of-text-properties (point-min) (point-max) tibetan-zettel-highlight--props))
  (setq tibetan-zettel-highlight--projections nil)
  (when font-lock-mode (font-lock-flush)))

;;;###autoload
(define-minor-mode tibetan-zettel-highlight-mode
  "Underline and link words that have a zettel (RET / mouse opens it)."
  :lighter " Zk"
  :group 'tibetan-zettel-highlight
  (if tibetan-zettel-highlight-mode
      (let ((kind (tibetan-zettel-highlight--buffer-kind)))
        (if kind
            (tibetan-zettel-highlight--enable kind)
          (setq tibetan-zettel-highlight-mode nil)
          (user-error "Kein CAT-Quelltext und keine Analyse-Datei")))
    (tibetan-zettel-highlight--disable)))

(defun tibetan-zettel-highlight--maybe-enable ()
  "Turn the mode on in CAT source and analysis buffers (org-mode-hook)."
  (when (and tibetan-zettel-highlight-auto-enable
             buffer-file-name
             (tibetan-zettel-highlight--buffer-kind))
    (tibetan-zettel-highlight-mode 1)))

(add-hook 'org-mode-hook #'tibetan-zettel-highlight--maybe-enable)

(with-eval-after-load 'tibetan-cascade
  (add-hook 'tibetan-cascade-after-land-functions #'tibetan-zettel-highlight--invalidate-projections))

;;;###autoload
(defun tibetan-zettel-highlight-reload ()
  "Regenerate the index with skos_index.py and refontify every mode buffer."
  (interactive)
  (unless (tibetan-zettel-highlight--run-index-script)
    (user-error "skos_index.py nicht gefunden oder fehlgeschlagen: %s" tibetan-zettel-highlight-index-script))
  (setq tibetan-zettel-highlight--index nil)
  (tibetan-zettel-highlight--ensure-index)
  (tibetan-zettel-highlight--invalidate-projections)
  (dolist (b (buffer-list))
    (with-current-buffer b
      (when (and (bound-and-true-p tibetan-zettel-highlight-mode) font-lock-mode)
        (font-lock-flush))))
  (message "Zettel-Index: %d Zettel, %d Schlüssel"
           (hash-table-count tibetan-zettel-highlight--entries)
           (hash-table-count tibetan-zettel-highlight--index)))

(provide 'tibetan-zettel-highlight)

;;; tibetan-zettel-highlight.el ends here
