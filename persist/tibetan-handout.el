;;; tibetan-handout.el --- Printable reading-class handout from cascade files -*- lexical-binding: t -*-

;;; Commentary:
;; §5.59 (2026-10-09): Carsten works in the reading classes with a
;; printed sheet next to the class text — quick look-up plus room
;; for handwritten notes.  Per sentence of a § (or any org subtree of
;; a cascade source): the full Wylie (Uchen above it), the
;; translation, the vocabulary (★ class wordlist first, the context
;; gloss small behind it only where it differs) and the grammar
;; (only the labels Carsten reads in class).  Output is a GENERATED,
;; editable .org file (strike vocabulary before printing) exported
;; to PDF via LuaLaTeX.
;;
;; Layering: the DATA layer (`tibetan-handout--sentence-data') reads
;; the cascade sentence files; the RENDERER turns the data into org;
;; the COMMAND resolves the scope at point, writes the file and
;; builds the PDF.  Reference translations (Lopez / W&M), DharmaMitra
;; and Carsten's own Working Translation never appear — the handout
;; reads only the tool's Translation/Vocabulary/Grammar slots.

;;; Code:

(require 'cl-lib)
(require 'tibetan-wylie)
(require 'tibetan-vocabulary)
(require 'tibetan-analysis-claude)
(require 'tibetan-sanskrit-reading)
(require 'tibetan-cascade)
(require 'tibetan-translation-doc)
(require 'tibetan-translation-discussion)  ; §5.62 :discussion
(require 'tibetan-sanskrit-metre)          ; §5.65 recitation pauses

(declare-function tibetan-cascade--scope-at-point "tibetan-cascade" ())
(declare-function tibetan-cascade--clean-slot-text "tibetan-cascade" (body))
(declare-function tibetan-cascade--collect-sentences "tibetan-cascade" ())
(declare-function tibetan-analysis-make-short-name "tibetan-analysis-persist" (source-file))
(declare-function tibetan-discussion-read "tibetan-translation-discussion" (file))
(declare-function tibetan-metre-label "tibetan-sanskrit-metre" (segments))
(declare-function tibetan-metre-table "tibetan-sanskrit-metre" (segments))

;; ============================================================================
;; B1 — data layer
;; ============================================================================

(defun tibetan-handout--vocab-fields (line)
  "Split a comma-format vocabulary LINE (`term, pos, \"gloss\", note').
Returns (:term :pos :gloss :skt) — of the note only the Sanskrit
equivalent (`Skt. *x*') survives; the handout is a look-up sheet.
A leading list bullet (C6a model drift) is tolerated; a line
without a quoted gloss takes the third comma field.  nil for blank,
`---' or comma-less lines."
  (let ((l (string-trim (replace-regexp-in-string
                         "\\`[ \t]*[-*][ \t]+" "" (or line "")))))
    (unless (or (string-empty-p l) (string-prefix-p "---" l)
                (not (string-match-p "," l)))
      (let (term pos gloss note)
        (if (string-match
             (concat "\\`\\([^,]+\\),[ \t]*\\(.*?\\),?[ \t]*[\"“]"
                     "\\([^\"”]*\\)[\"”]\\(?:[ \t]*,[ \t]*\\(.*\\)\\)?\\'")
             l)
            (setq term (match-string 1 l) pos (match-string 2 l)
                  gloss (match-string 3 l) note (match-string 4 l))
          (let ((fields (mapcar #'string-trim (split-string l ","))))
            (setq term (nth 0 fields)
                  pos (or (nth 1 fields) "")
                  gloss (mapconcat #'identity (nthcdr 2 fields) ", "))))
        (list :term (string-trim term)
              :pos (string-trim (or pos ""))
              :gloss (string-trim (or gloss ""))
              :skt (and note
                        (cond
                         ((string-match "Skt\\.[ \t]+\\*\\([^*]+\\)\\*" note)
                          (string-trim (match-string 1 note)))
                         ((string-match "Skt\\.[ \t]+\\([^;,()]+\\)" note)
                          (string-trim (match-string 1 note))))))))))

(defvar tibetan-handout--particle-syllables nil
  "Cache of `tibetan-handout--particle-syllables'.")

(defun tibetan-handout--particle-syllables ()
  "Wylie particle SYLLABLES a vocabulary term may end with (`yin pas').
Derived ONCE from the tool's case/converb tail list
\(`tibetan-extract-vocab--particle-tails') — no parallel list.  The
one-letter clitics come out as `ra'/`sa'/`'i' and are excluded: as
a syllable `sa' is the noun \"earth\"; the clitics are handled by
the part-of-speech-gated rules instead."
  (or tibetan-handout--particle-syllables
      (setq tibetan-handout--particle-syllables
            (cl-set-difference
             (delq nil (mapcar (lambda (p)
                                 (ignore-errors
                                   (string-trim (tibetan-to-wylie-fixed p))))
                               tibetan-extract-vocab--particle-tails))
             '("ra" "sa" "'i")
             :test #'equal))))

(defconst tibetan-handout--clitic-pos-rules
  '(("'ang" . "concessive\\|emphatic")
    ("'is" . "ergative\\|instrumental\\|agentive")
    ("'i" . "genitive")
    ("'o" . "final\\|declarative\\|assertive")
    ("s" . "ergative\\|instrumental\\|agentive")
    ("r" . "terminative\\|dative\\|locative"))
  "Merged clitics the ★ lookup may strip — each ONLY when Claude's
part of speech names its function (longest first).  `blos'
\(ergative/instrumental) → `blo'; `rgyas' as a verb never becomes
`rgya' — a blind s-strip would hand a wrong wordlist meaning.")

(defun tibetan-handout--curated-gloss (term pos)
  "The class-wordlist (★) entry for vocabulary TERM, or nil.
Probes `tibetan-vocab--curated-exact-entry' (Resources + Custom as
currently bound): TERM exactly; TERM minus a trailing particle
syllable (`yin pas' → `yin'); TERM minus a merged clitic licensed by
POS (`tibetan-handout--clitic-pos-rules').  Returns the entry
verbatim — the bilingual DE // EN pair is never collapsed."
  (let ((term (string-trim (or term "")))
        (pos (downcase (or pos ""))))
    (unless (string-empty-p term)
      (or (tibetan-vocab--curated-exact-entry term)
          (let ((syls (split-string term " " t)))
            (when (and (> (length syls) 1)
                       (member (car (last syls))
                               (tibetan-handout--particle-syllables)))
              (tibetan-vocab--curated-exact-entry
               (mapconcat #'identity (butlast syls) " "))))
          (cl-loop for (clitic . pos-re) in tibetan-handout--clitic-pos-rules
                   when (and (string-suffix-p clitic term)
                             (> (length term) (length clitic))
                             (string-match-p pos-re pos))
                   thereis (let ((base (string-trim-right
                                        (substring term 0 (- (length term)
                                                             (length clitic))))))
                             (and (not (string-empty-p base))
                                  (tibetan-vocab--curated-exact-entry base))))))))

(defun tibetan-handout--gloss-alternatives (gloss)
  "GLOSS split into normalized alternatives (both language halves)."
  (delete ""
          (mapcar (lambda (a)
                    (downcase (string-trim
                               (replace-regexp-in-string "[\"“”„…]" "" a))))
                  (split-string (or gloss "") "//\\|[/;,]"))))

(defun tibetan-handout--context-differs-p (context curated)
  "Non-nil when the CONTEXT gloss adds something to the ★ CURATED
entry: some alternative of CONTEXT is not among CURATED's
alternatives (Carsten 09.10.: the context gloss stands small behind
the ★ meaning, but only where it differs)."
  (and context curated
       (let ((cur (tibetan-handout--gloss-alternatives curated)))
         (cl-some (lambda (a) (not (member a cur)))
                  (tibetan-handout--gloss-alternatives context)))))

(defcustom tibetan-handout-grammar-labels
  '("Verb backbone" "Notable constructions")
  "Grammar bullet labels the handout keeps (case-insensitive).
A segment whose grammar carries NONE of them keeps all its bullets —
the filter never empties a segment (the Sanskrit schema has free
labels)."
  :type '(repeat string)
  :group 'tibetan-cat)

(defun tibetan-handout--grammar-bullets (text)
  "Labelled grammar bullets of one segment's TEXT: ((LABEL . BODY) …).
Bullets look like `- *Label:* body' (also `**Label:**'); indented
continuation lines join their bullet.  Only labels from
`tibetan-handout-grammar-labels' survive — unless NONE of them occurs,
then every bullet stays (the filter never empties a segment).  Text
without labelled bullets comes back as ((nil . PROSE))."
  (when (and text (not (string-empty-p (string-trim text))))
    (let ((items '()) (label nil) (body nil) (prose '()))
      (cl-flet ((flush ()
                  (when body
                    (push (cons label (string-trim body)) items))
                  (setq label nil body nil)))
        (dolist (line (split-string text "\n"))
          (cond
           ((string-match (concat "\\`[ \t]*[-+][ \t]+\\*\\{1,2\\}\\([^*\n]+?\\)"
                                  ":?\\*\\{1,2\\}:?[ \t]*\\(.*\\)\\'")
                          line)
            (flush)
            (setq label (string-trim (match-string 1 line) nil ":[ \t]*")
                  body (match-string 2 line)))
           ((string-match "\\`[ \t]*[-+][ \t]+\\(.*\\)\\'" line)
            (flush)
            (setq body (match-string 1 line)))
           ((string-empty-p (string-trim line))
            (flush))
           (body
            (setq body (concat body " " (string-trim line))))
           (t (push (string-trim line) prose))))
        (flush))
      (setq items (nreverse items))
      (if (null items)
          (and prose
               (list (cons nil (mapconcat #'identity (nreverse prose) " "))))
        (let* ((wanted (mapcar #'downcase tibetan-handout-grammar-labels))
               (kept (cl-remove-if-not
                      (lambda (it) (and (car it)
                                        (member (downcase (car it)) wanted)))
                      items)))
          (or kept items))))))

(defun tibetan-handout--load-curated (source-file)
  "The class wordlist (★) hash of SOURCE-FILE's Resources folder.
Loaded with `default-directory' = the source's directory under a
`let' on the vocabulary state, so nothing leaks into the session's
global lookup (§5.34 lesson: locate per-document assets from the
source path, never from the caller's buffer).  nil without
Resources."
  (let ((default-directory (file-name-directory
                            (expand-file-name source-file)))
        (tibetan-current-resources-vocab nil))
    (with-temp-buffer
      (tibetan-load-resources-vocab)
      tibetan-current-resources-vocab)))

(defun tibetan-handout--translation (file seg-nums sections)
  "The translation of the sentence FILE for the handout, or nil.
\(:by-seg ((N . TEXT) …)) when every segment of SEG-NUMS has a
landed ⟦N⟧ line — the handout numbers the pieces; else (:whole
TEXT) from the Translation slot of SECTIONS (machine label
`(Sentence N — …)' stripped); nil when only placeholders exist."
  (let ((rends (tibetan-translation-doc--renderings file)))
    (if (and seg-nums
             (cl-every (lambda (n)
                         (tibetan-cascade--clean-slot-text
                          (cdr (assq n rends))))
                       seg-nums))
        (list :by-seg (mapcar (lambda (n) (cons n (cdr (assq n rends))))
                              seg-nums))
      (let ((tr (tibetan-cascade--clean-slot-text
                 (plist-get sections :translation))))
        (when tr (list :whole tr))))))

(defun tibetan-handout--vocab (body sa word-analysis)
  "Per-segment vocabulary entries from the Claude Vocabulary BODY.
Returns ((SEG . ENTRIES) …), ENTRY = (:term :gloss :star :context
:info).  bo: the ★ wordlist entry replaces the gloss, the context
gloss stays only where it differs; info = part of speech (+ Skt.).
SA (Sanskrit): NO ★ lookup ever — the wordlist is Wylie-keyed and
`na'/`ca'/`ma' would hit Tibetan entries (§5.57 poison class); info
= \"lemma; MORPH\" from WORD-ANALYSIS (the parsed `** Word Analysis')
when the word is listed there."
  (let ((out '()))
    (dolist (g (tibetan-analysis--split-body-by-segment body))
      (let* ((wa (cdr (assq (car g) word-analysis)))
             (entries
              (delq nil
                    (mapcar
                     (lambda (line)
                       (let ((f (tibetan-handout--vocab-fields line)))
                         (when f
                           (let* ((term (plist-get f :term))
                                  (pos (plist-get f :pos))
                                  (star (and (not sa)
                                             (tibetan-handout--curated-gloss
                                              term pos)))
                                  (lemma (cdr (assoc term (plist-get wa :lemma))))
                                  (morph (cdr (assoc term (plist-get wa :morph)))))
                             (list :term term
                                   :gloss (or star (plist-get f :gloss))
                                   :star (and star t)
                                   :context (and star
                                                 (tibetan-handout--context-differs-p
                                                  (plist-get f :gloss) star)
                                                 (plist-get f :gloss))
                                   :info (cond
                                          ((and sa morph)
                                           (if lemma (format "%s; %s" lemma morph)
                                             morph))
                                          ((plist-get f :skt)
                                           (if (string-empty-p pos)
                                               (format "Skt. %s" (plist-get f :skt))
                                             (format "%s; Skt. %s" pos
                                                     (plist-get f :skt))))
                                          (t pos)))))))
                     (split-string (cdr g) "\n")))))
        (when entries
          (push (cons (car g) entries) out))))
    (nreverse out)))

(defun tibetan-handout--grammar (body)
  "Per-segment filtered grammar of the Claude Grammar BODY:
\((SEG . ((LABEL . TEXT) …)) …).  The cross-clause preamble is
dropped (Carsten 09.10.: only the configured labels) — unless the
body has no segment subsections at all, then the preamble is the
only grammar there is and goes through the same filter."
  (let* ((groups (tibetan-analysis--split-body-by-segment body))
         (segs (cl-remove-if-not #'car groups))
         (use (or segs groups))
         (out '()))
    (dolist (g use)
      (let ((bullets (tibetan-handout--grammar-bullets
                      ;; `**Cross-clause overview:**' prose and the
                      ;; L3 overview heading are no bullets anyway.
                      (cdr g))))
        (when bullets
          (push (cons (car g) bullets) out))))
    (nreverse out)))

(defun tibetan-handout--seg-list (segs sa &optional plain)
  "SEGS ((N . TEXT) …) of a sentence as the renderer's plists (:num
:wylie :uchen): Wylie from the Uchen for Tibetan, the IAST itself as
:wylie and no :uchen for Sanskrit (SA) — for a Sanskrit VERSE with
its recitation pauses (` | ' pāda end, ` ¦ ' yati; §5.65), unless
PLAIN (the synopsis prints the edition's wording, §5.68).  Shared
with the discussion paper (§5.64)."
  (let ((marked (and sa (not plain)
                     (tibetan-metre-mark-segments
                      (mapcar (lambda (s) (string-trim (cdr s))) segs)))))
    (cl-loop for s in segs for i from 0
             collect (let ((text (string-trim (cdr s))))
                       (list :num (car s)
                             :wylie (if sa (or (nth i marked) text)
                                      (string-trim (tibetan-to-wylie-fixed text)))
                             :uchen (unless sa text))))))

(defun tibetan-handout--metre (segs sa)
  "Label and syllable table of the Sanskrit verse SEGS (§5.65):
\(LABEL . TABLE), or nil (Tibetan, prose).  Shared with the
discussion paper."
  (when sa
    (let ((texts (mapcar (lambda (s) (string-trim (cdr s))) segs)))
      (when-let ((label (tibetan-metre-label texts)))
        (cons label (tibetan-metre-table texts))))))

(defun tibetan-handout--sentence-data (sent-num source-file)
  "Everything the handout needs for sentence SENT-NUM of SOURCE-FILE.
Plist: :sent-num :lang (\"bo\"/\"sa\") :segs ((:num :wylie :uchen) …)
— the source text itself, Uchen only for bo — and, when the cascade
file exists, :translation (`tibetan-handout--translation'), :vocab
\(`--vocab'), :grammar (`--grammar') and :discussion (§5.62,
`tibetan-discussion-read' — quotes and points, never the marked full
reference); otherwise :missing t (a
visible gap, never an error).  ★ lookups use the wordlist state the
CALLER binds (`tibetan-handout--load-curated').  Reads only the
tool's Translation/Vocabulary/Grammar slots — Provided Translations
\(Lopez/W&M/DM) and the Working Translation never reach the handout."
  (let* ((lang (tibetan-analysis--resolve-source-lang source-file))
         (sa (equal lang "sa"))
         (segs (tibetan-cascade--segs-for-sentence source-file sent-num))
         (folder (file-name-as-directory
                  (expand-file-name "analysis"
                                    (file-name-directory source-file))))
         (file (tibetan-sentence--filepath sent-num folder source-file))
         (seg-list (tibetan-handout--seg-list segs sa))
         (metre (tibetan-handout--metre segs sa))
         (base (list :sent-num sent-num :lang lang :segs seg-list
                     :metre (car metre) :metre-table (cdr metre))))
    (if (not (file-exists-p file))
        (append base (list :missing t))
      (let ((sections (tibetan-analysis--read-claude-sections file))
            (wa (and sa (tibetan-sanskrit-reading-parse-word-analysis
                         (tibetan-sentence--read-l2-body file
                                                         "Word Analysis")))))
        (append base
                (list :translation (tibetan-handout--translation
                                    file (mapcar #'car segs) sections)
                      :vocab (tibetan-handout--vocab
                              (plist-get sections :vocabulary) sa wa)
                      :grammar (tibetan-handout--grammar
                                (plist-get sections :grammar))
                      :discussion (tibetan-discussion-read file)))))))

;; ----------------------------------------------------------------------------
;; §5.68 — synopsis data: sibling cascades by Section title, quotations
;; ----------------------------------------------------------------------------
;; Carsten (21.10.), after the PhD students' preparation sheet for
;; MSA IX.60 (Zhoulin Li): the handout gets a front part in their
;; genre — sigla, the edition locators of the verse, the synoptic
;; table of the parallel cascades (Sanskrit | Tibetan), and the
;; commentary (Sthiramati) with the words it quotes from the verse in
;; bold and those from the bhāṣya underlined.  Everything comes from
;; the cascade files themselves (same Section titles, `:FOLIO:',
;; `:VERSE:'); nothing needs an API call.  The quotation marking is a
;; syllable heuristic — the sheet's human markup is the gold standard
;; its test is measured against.

(defun tibetan-handout--section-sentences (source-file label)
  "The sentences of SOURCE-FILE under the Section heading LABEL
\(e.g. \"Section IX.60\" — the same title in every cascade of a
reading): the walker's plists (:sent-num :segs :verse :folios …) in
file order; nil when the file or the Section is missing."
  (when (and source-file (file-readable-p source-file))
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents source-file)
          (org-mode)
          (cl-remove-if-not
           (lambda (p) (equal (plist-get p :section-label) label))
           (tibetan-cascade--collect-sentences)))
      (error nil))))

(defun tibetan-handout--section-id (label)
  "\"IX.60\" for the Section heading LABEL \"Section IX.60\"."
  (string-trim (replace-regexp-in-string "\\`Section[ \t]+" "" (or label ""))))

(defconst tibetan-handout--quote-function-syllables
  '("pa" "ba" "po" "bo" "ma" "pa'i" "ba'i" "po'i" "yang" "kyang" "'ang"
    "yin" "min" "'o" "no" "to" "do" "ro" "so" "go" "ngo" "lo" "mo"
    "zhes" "ces" "shes" "bya" "smos")
  "Wylie syllables that never count as CONTENT when a quotation is
weighed (§5.68) — on top of the case/converb particles of
`tibetan-handout--particle-syllables': nominalisers and their
genitives, concessives, the copula and final particles, the quote
markers themselves.")

(defun tibetan-handout--quote-function-p (syl)
  "Non-nil when the Wylie syllable SYL carries no content."
  (or (member syl tibetan-handout--quote-function-syllables)
      (member syl (tibetan-handout--particle-syllables))))

(defun tibetan-handout--wylie-tokens (wylie)
  "The tokens of WYLIE as ((BEG . END) …): syllables and the shad `/'
\(its own token — the converter attaches it: `dbye ba/ /')."
  (let ((pos 0) (out '()))
    (while (string-match "[^ \t\n/]+\\|/" wylie pos)
      (push (cons (match-beginning 0) (match-end 0)) out)
      (setq pos (match-end 0)))
    (nreverse out)))

(defun tibetan-handout--wylie-syllables (strings)
  "The syllable sequence of the Wylie STRINGS (shad kept as `/' so a
run never crosses a pāda or daṇḍa boundary)."
  (cl-loop for s in strings
           append (mapcar (lambda (p) (substring s (car p) (cdr p)))
                          (tibetan-handout--wylie-tokens s))))

(defun tibetan-handout--subseq-p (sub seq)
  "Non-nil when the token list SUB occurs consecutively in SEQ."
  (let ((n (length sub)) (found nil))
    (while (and (not found) seq (>= (length seq) n))
      (when (cl-every #'equal sub seq)
        (setq found t))
      (setq seq (cdr seq)))
    found))

(defun tibetan-handout--mark-quotes (segments verse bhasya)
  "SEGMENTS (the Wylie strings of a commentary passage, in order)
with the commentary's quotations marked: per segment a list of
\(TEXT . KIND) runs that concatenate to the segment verbatim, KIND
`verse' where the run is the wording of VERSE (Wylie strings of the
kārikā), `bhasya' where it is that of BHASYA (the base text's prose),
nil otherwise.

Two rules, applied to the whole passage (lemma and marker are often
split by a shad and a sentence boundary — `… yin te/ /' `zhes bya ba
la/'): (1) the LEMMA before a quote marker `zhes/ces/shes bya' or
`… smos' (back to the previous shad, at most 16 syllables; not the
closing formula `zhes bya ba'i don to' / `zhes bya ste') is a verse
quotation when at least two thirds of its content syllables stand in
the verse (ties go to the verse: a lemma quotes the kārikā first),
else a bhāṣya quotation by the same measure; (2) elsewhere a RUN of at
least two content syllables found consecutively in the bhāṣya is a
bhāṣya echo (ties go to the bhāṣya: prose terms the commentary takes
up), one found only in the verse a verse echo.  Known limit: the
commentator's own rendering of the verse (Sthiramati: `thog ma … gnas'
for `dang po … rten') is not recognised."
  (let* ((vseq (tibetan-handout--wylie-syllables verse))
         (bseq (tibetan-handout--wylie-syllables bhasya))
         (vset (cl-remove "/" vseq :test #'equal))
         (bset (cl-remove "/" bseq :test #'equal))
         (toks (apply #'vector
                      (cl-loop for s in segments for i from 0
                               append (mapcar (lambda (p)
                                                (list i (car p) (cdr p)
                                                      (substring s (car p) (cdr p))
                                                      nil))
                                              (tibetan-handout--wylie-tokens s)))))
         (n (length toks))
         (str (lambda (i) (nth 3 (aref toks i))))
         (kind (lambda (i) (nth 4 (aref toks i))))
         (mark (lambda (i k) (setcar (nthcdr 4 (aref toks i)) k)))
         (ratio (lambda (content set)
                  (/ (float (cl-count-if (lambda (s) (member s set)) content))
                     (length content)))))
    ;; (1) lemma before a quote marker.
    (dotimes (i n)
      (when (and (< (1+ i) n)
                 (member (funcall str i) '("zhes" "ces" "shes"))
                 (member (funcall str (1+ i)) '("bya" "smos"))
                 (not (and (< (+ i 2) n)
                           (member (funcall str (+ i 2)) '("ba'i" "ste")))))
        (let ((j (1- i)) (lemma '()))
          (while (and (>= j 0) (equal (funcall str j) "/"))
            (setq j (1- j)))
          (while (and (>= j 0) (< (length lemma) 16)
                      (not (equal (funcall str j) "/")))
            (push j lemma)
            (setq j (1- j)))
          (let* ((content (cl-remove-if #'tibetan-handout--quote-function-p
                                        (mapcar str lemma)))
                 (vr (if content (funcall ratio content vset) 0))
                 (br (if content (funcall ratio content bset) 0))
                 (k (cond ((and content (>= vr 0.66) (>= vr br)) 'verse)
                          ((and content (>= br 0.66)) 'bhasya))))
            (when k
              (dolist (idx lemma) (funcall mark idx k)))))))
    ;; (2) runs of the base text's wording — content syllable to
    ;; content syllable (`rang bzhin ni chos kyi sku' is two echoes,
    ;; `rang bzhin' and `chos kyi sku', not one).
    (let ((i 0))
      (while (< i n)
        (if (or (funcall kind i) (equal (funcall str i) "/")
                (tibetan-handout--quote-function-p (funcall str i)))
            (setq i (1+ i))
          (let ((best 0) (best-kind nil) (len 2))
            (while (and (<= (+ i len) n) (<= len 12))
              (let* ((idxs (number-sequence i (+ i len -1)))
                     (run (mapcar str idxs)))
                (if (or (member "/" run)
                        (cl-some kind idxs))
                    (setq len 100)
                  (when (and (>= (cl-count-if-not #'tibetan-handout--quote-function-p run) 2)
                             (not (tibetan-handout--quote-function-p (car (last run)))))
                    (cond ((tibetan-handout--subseq-p run bseq)
                           (setq best len best-kind 'bhasya))
                          ((tibetan-handout--subseq-p run vseq)
                           (setq best len best-kind 'verse))))
                  (setq len (1+ len)))))
            (if (zerop best)
                (setq i (1+ i))
              (dotimes (d best) (funcall mark (+ i d) best-kind))
              (setq i (+ i best)))))))
    ;; Runs per segment, the gaps verbatim.
    (cl-loop
     for s in segments for si from 0
     collect
     (let ((pos 0) (runs '()) (cur nil) (beg nil) (end nil))
       (cl-flet ((flush ()
                   (when cur
                     (when (> beg pos)
                       (push (cons (substring s pos beg) nil) runs))
                     (push (cons (substring s beg end) cur) runs)
                     (setq pos end cur nil))))
         (dotimes (i n)
           (when (= (nth 0 (aref toks i)) si)
             (let ((k (funcall kind i)))
               (cond ((and k (eq k cur)) (setq end (nth 2 (aref toks i))))
                     (k (flush) (setq cur k beg (nth 1 (aref toks i))
                                      end (nth 2 (aref toks i))))
                     (t (flush))))))
         (flush)
         (when (< pos (length s))
           (push (cons (substring s pos) nil) runs))
         (nreverse runs))))))

(defun tibetan-handout--siglum (file)
  "The siglum of the cascade FILE: its `#+TIBETAN_SIGLUM:', else the
short name of the file."
  (or (plist-get (tibetan-analysis--read-source-metadata file) :siglum)
      (tibetan-analysis-make-short-name file)))

(defun tibetan-handout--folio-locator (sentences)
  "\"135b5–136a1\" from the first and last `:folios' of SENTENCES (the
edition letter stripped — the siglum carries it), \"135b5\" when they
coincide, nil without folios."
  (let ((folios (cl-loop for s in sentences
                         append (mapcar #'cdr (plist-get s :folios)))))
    (when folios
      (let* ((strip (lambda (f) (string-trim
                                 (replace-regexp-in-string "\\`[A-Za-z]+" "" f))))
             (a (funcall strip (car folios)))
             (b (funcall strip (car (last folios)))))
        (if (equal a b) a (format "%s–%s" a b))))))

(defun tibetan-handout--synopsis-column (file label)
  "One column of the synopsis: Section LABEL of the cascade FILE as
\(:siglum :lang :locator :sentences), :sentences ((:sent-num :verse
:segs ((:num :wylie) …)) …) — Wylie for Tibetan, the IAST without
recitation pauses for Sanskrit; :locator the folio range, else the
verse id, else the Section id."
  (let* ((lang (tibetan-analysis--resolve-source-lang file))
         (sa (equal lang "sa"))
         (sents (tibetan-handout--section-sentences file label))
         (verse (cl-find-if (lambda (s) (plist-get s :verse)) sents)))
    (list :siglum (tibetan-handout--siglum file)
          :lang lang
          :locator (or (tibetan-handout--folio-locator sents)
                       (and verse (plist-get verse :verse))
                       (tibetan-handout--section-id label))
          :sentences (mapcar (lambda (s)
                               (list :sent-num (plist-get s :sent-num)
                                     :verse (plist-get s :verse)
                                     :segs (tibetan-handout--seg-list
                                            (plist-get s :segs) sa t)))
                             sents))))

(defun tibetan-handout--synopsis-commentary (file label reference)
  "The commentary column of Section LABEL in the cascade FILE: a
`tibetan-handout--synopsis-column' whose segments carry :runs
\(`tibetan-handout--mark-quotes' against the verse and prose wording
of the column REFERENCE — the same language's base text; no marks
without one) and whose sentences carry :translation — the tool's own
\(`tibetan-handout--translation'), never a reference translation."
  (let* ((col (tibetan-handout--synopsis-column file label))
         (sents (plist-get col :sentences))
         (wylie-of (lambda (s) (mapcar (lambda (g) (plist-get g :wylie))
                                       (plist-get s :segs))))
         (ref-sents (plist-get reference :sentences))
         (verse (cl-loop for s in ref-sents when (plist-get s :verse)
                         append (funcall wylie-of s)))
         (bhasya (cl-loop for s in ref-sents unless (plist-get s :verse)
                          append (funcall wylie-of s)))
         (marked (tibetan-handout--mark-quotes
                  (cl-loop for s in sents append (funcall wylie-of s))
                  verse bhasya))
         (folder (file-name-as-directory
                  (expand-file-name "analysis" (file-name-directory file)))))
    (plist-put
     col :sentences
     (mapcar (lambda (s)
               (let* ((segs (mapcar (lambda (g)
                                      (append g (list :runs (pop marked))))
                                    (plist-get s :segs)))
                      (sent-file (tibetan-sentence--filepath
                                  (plist-get s :sent-num) folder file))
                      (tr (and (file-exists-p sent-file)
                               (tibetan-handout--translation
                                sent-file
                                (mapcar (lambda (g) (plist-get g :num)) segs)
                                (tibetan-analysis--read-claude-sections sent-file)))))
                 (list :sent-num (plist-get s :sent-num)
                       :verse (plist-get s :verse)
                       :segs segs
                       :translation tr)))
             sents))))

(defun tibetan-handout--synopsis-data (source-file label)
  "The synopsis of Section LABEL for the handout of SOURCE-FILE:
\(:id :label :lead :parallels :commentaries) — the lead column, one
column per `#+TIBETAN_SYNOPSIS:', one commentary per
`#+TIBETAN_COMMENTARY:' (quotations marked against the first column
of its language).  Paths resolve relative to the source."
  (let* ((meta (tibetan-analysis--read-source-metadata source-file))
         (dir (file-name-directory (expand-file-name source-file)))
         (lead (tibetan-handout--synopsis-column source-file label))
         (parallels (mapcar (lambda (rel)
                              (tibetan-handout--synopsis-column
                               (expand-file-name rel dir) label))
                            (plist-get meta :synopsis)))
         (columns (cons lead parallels))
         (commentaries
          (mapcar (lambda (rel)
                    (let* ((f (expand-file-name rel dir))
                           (lang (tibetan-analysis--resolve-source-lang f))
                           (ref (cl-find lang columns
                                         :key (lambda (c) (plist-get c :lang))
                                         :test #'equal)))
                      (tibetan-handout--synopsis-commentary f label ref)))
                  (plist-get meta :commentary))))
    (list :id (tibetan-handout--section-id label)
          :label label
          :lead lead
          :parallels parallels
          :commentaries commentaries)))

(defun tibetan-handout--scope-sections (source-file sent-nums)
  "The Section labels of SENT-NUMS in SOURCE-FILE — each once, in
order of first appearance; sentences outside any Section skipped."
  (when (and source-file (file-readable-p source-file))
    (condition-case nil
        (with-temp-buffer
          (insert-file-contents source-file)
          (org-mode)
          (let ((all (tibetan-cascade--collect-sentences))
                (labels '()))
            (dolist (n sent-nums)
              (let ((label (plist-get (cl-find n all :key (lambda (p) (plist-get p :sent-num)))
                                      :section-label)))
                (when (and label (not (member label labels)))
                  (push label labels))))
            (nreverse labels)))
      (error nil))))

(defun tibetan-handout--sigla-text (source-file meta)
  "The body of the `#+TIBETAN_SIGLA:' file of SOURCE-FILE (META its
metadata) — org as Carsten wrote it, keyword and comment lines
\(`#…') dropped; nil when absent or empty."
  (let* ((rel (plist-get meta :sigla))
         (file (and rel (expand-file-name
                         rel (file-name-directory (expand-file-name source-file))))))
    (when (and file (file-readable-p file))
      (let ((body (with-temp-buffer
                    (insert-file-contents file)
                    (string-trim
                     (mapconcat #'identity
                                (cl-remove-if (lambda (l) (string-prefix-p "#" l))
                                              (split-string (buffer-string) "\n"))
                                "\n")))))
        (unless (string-empty-p body) body)))))

;; ============================================================================
;; B2 — renderer (pure: spec → org string)
;; ============================================================================

(defconst tibetan-handout-generated-marker
  "# GENERATED by tibetan-handout"
  "First line of every generated handout.  The overwrite guard
refuses an existing target without it (a hand-owned file is never
clobbered); a generated handout may be shortened before printing —
the next C-c u H regenerates it from the analysis files.")

(defcustom tibetan-handout-tibetan-font "Noto Serif Tibetan"
  "Font for the Uchen line of the handout (fontspec name, HarfBuzz)."
  :type 'string
  :group 'tibetan-cat)

(defconst tibetan-handout--latex-commands
  (concat
   "#+LATEX_HEADER: \\newcommand{\\segno}[1]{\\noindent\\llap{\\color{seg}\\footnotesize #1\\hspace{3mm}}}\n"
   "#+LATEX_HEADER: \\newcommand{\\segsup}[1]{\\textsuperscript{\\color{seg}\\scriptsize #1}\\,}\n"
   "#+LATEX_HEADER: \\newcommand{\\ctx}[1]{{\\footnotesize\\color{dim}(#1)}}\n"
   "#+LATEX_HEADER: \\newcommand{\\luecke}[1]{{\\color{dim}\\textit{— #1 —}}}\n"
   "#+LATEX_HEADER: \\newcommand{\\seglabel}[1]{\\par\\smallskip\\Needspace{3\\baselineskip}{\\color{seg}\\footnotesize #1}\\par\\nopagebreak}\n"
   "#+MACRO: n @@latex:\\segsup{$1}@@\n"
   "#+MACRO: seg @@latex:\\seglabel{$1}@@\n"
   "#+MACRO: ctx @@latex:\\ctx{@@$1@@latex:}@@\n"
   "#+MACRO: luecke @@latex:\\luecke{@@$1@@latex:}@@\n")
  "The LaTeX commands and org macros the handout body uses (segment
numbers, context glosses, gaps) — shared with the discussion paper
\(§5.64), whose geometry differs but whose blocks are the same.")

(defun tibetan-handout--latex-escape (s)
  "S with the LaTeX special characters escaped (raw-LaTeX blocks)."
  (mapconcat (lambda (c)
               (pcase c
                 (?\\ "\\textbackslash{}")
                 (?~ "\\textasciitilde{}")
                 (?^ "\\textasciicircum{}")
                 ((or ?& ?% ?# ?_ ?$ ?{ ?}) (string ?\\ c))
                 (_ (string c))))
             (or s "") ""))

(defun tibetan-handout--emphasis (body org-marker latex-cmd prev next)
  "BODY as org emphasis with ORG-MARKER when org will recognise it
between the characters PREV and NEXT (nil = line edge); otherwise
an export snippet with LATEX-CMD — the output must be right even
where org's emphasis rules would leave the markers standing."
  (if (and (not (string-match-p "[/*\n]" body))
           (not (string-match-p "\\`[ \t]\\|[ \t]\\'" body))
           (or (null prev) (memq prev '(?\s ?\t ?\( ?- ?' ?\" ?{)))
           (or (null next) (memq next '(?\s ?\t ?. ?, ?\; ?: ?! ?? ?'
                                       ?\) ?} ?- ?\" ?\\))))
      (concat org-marker body org-marker)
    (format "@@latex:\\%s{@@%s@@latex:}@@" latex-cmd body)))

(defun tibetan-handout--md-to-org (s)
  "Claude's markdown emphasis in S as org: `**x**' → bold, `*x*' →
italic.  Lines that org would read as a heading, comment or table
are defused with an empty export snippet in front."
  (let ((out "") (pos 0) (s (or s "")))
    (while (string-match "\\*\\*\\([^*\n]+\\)\\*\\*\\|\\*\\([^*\n]+\\)\\*"
                         s pos)
      (let* ((beg (match-beginning 0)) (end (match-end 0))
             (bold (match-string 1 s)) (ital (match-string 2 s))
             (prev (and (> beg 0) (aref s (1- beg))))
             (next (and (< end (length s)) (aref s end))))
        (setq out (concat out (substring s pos beg)
                          (if bold
                              (tibetan-handout--emphasis bold "*" "textbf"
                                                         prev next)
                            (tibetan-handout--emphasis ital "/" "emph"
                                                       prev next)))
              pos end)))
    (setq out (concat out (substring s pos)))
    (mapconcat (lambda (line)
                 (if (string-match-p "\\`\\(?:\\*+ \\|#\\|[ \t]*|\\)" line)
                     (concat "@@latex:{}@@" line)
                   line))
               (split-string out "\n") "\n")))

(defun tibetan-handout--cell (s)
  "S as a single org table cell: one line, `|' as \\vert{}."
  (replace-regexp-in-string
   "|" "\\\\vert{}"
   (replace-regexp-in-string "[ \t]*\n[ \t]*" " "
                             (tibetan-handout--md-to-org (string-trim (or s ""))))
   t))

(defun tibetan-handout--macro-arg (s)
  "S safe as an org macro argument (commas escaped)."
  (replace-regexp-in-string "," "\\\\," (or s "") t))

(defun tibetan-handout--seg-range (segs)
  "\"1970–1974\" / \"1975\" for the :segs of a sentence."
  (let ((nums (mapcar (lambda (s) (plist-get s :num)) segs)))
    (if (cdr nums)
        (format "%d–%d" (car nums) (car (last nums)))
      (format "%d" (car nums)))))

(defun tibetan-handout--sentence-range (sentences)
  "\"Satz 654\" / \"Sätze 654–656\" for the handout's SENTENCES."
  (let ((first (plist-get (car sentences) :sent-num))
        (last (plist-get (car (last sentences)) :sent-num)))
    (if (equal first last)
        (format "Satz %d" first)
      (format "Sätze %d–%d" first last))))

(defcustom tibetan-handout-cjk-font "Hiragino Mincho ProN"
  "Fallback font for CJK characters in the handout (fontspec name),
e.g. Japanese titles in the sigla (Nagao 2007) — §5.68; nil for
none."
  :type '(choice (const nil) string)
  :group 'tibetan-cat)

(defun tibetan-handout--latex-fonts ()
  "The font lines of the handout/paper preamble: Libertinus as main
font WITH `tibetan-handout-tibetan-font' (and
`tibetan-handout-cjk-font') as luaotfload fallbacks — every Tibetan
character anywhere in the document (discussion point titles, concept
notes) renders; before 20.10.2026 only the explicit \\tibfont Uchen
line had Tibetan glyphs, the rest showed boxes.  `\\tibfont' stays
for the Uchen line."
  (concat
   "#+LATEX_HEADER: \\usepackage{fontspec}\n"
   "#+LATEX_HEADER: \\usepackage{libertinus}\n"
   (format "#+LATEX_HEADER: \\directlua{luaotfload.add_fallback(\"tibfallback\", {\"%s:mode=harf;script=tibt;\"%s})}\n"
           tibetan-handout-tibetan-font
           (if tibetan-handout-cjk-font
               (format ", \"%s:mode=node;\"" tibetan-handout-cjk-font)
             ""))
   "#+LATEX_HEADER: \\setmainfont{Libertinus Serif}[RawFeature={fallback=tibfallback}]\n"
   (format "#+LATEX_HEADER: \\newfontfamily\\tibfont{%s}[Renderer=HarfBuzz,Script=Tibetan]\n"
           tibetan-handout-tibetan-font)))

(defun tibetan-handout--header (spec)
  "The org header (marker, keywords, LaTeX preamble, macros) of SPEC."
  (let* ((title (plist-get spec :title))
         (scope (plist-get spec :scope))
         (range (tibetan-handout--sentence-range (plist-get spec :sentences)))
         (head (mapconcat #'tibetan-handout--latex-escape
                          (delete-dups (delq nil (list scope range)))
                          " · ")))
    (concat
     tibetan-handout-generated-marker
     " — darf vor dem Druck gekürzt werden; C-c u H erzeugt neu\n"
     (format "# Quelle: %s · %s · generiert %s\n"
             (plist-get spec :source-name) scope (plist-get spec :date))
     (format "#+TITLE: %s — %s\n" title scope)
     "#+LANGUAGE: de\n"
     "#+OPTIONS: toc:nil num:nil title:nil author:nil date:nil ^:nil\n"
     "#+LATEX_COMPILER: lualatex\n"
     "#+LATEX_CLASS: article\n"
     "#+LATEX_CLASS_OPTIONS: [11pt,a4paper]\n"
     "#+LATEX_HEADER: \\usepackage[a4paper,left=2cm,right=6cm,top=2.2cm,bottom=2.2cm,headsep=8mm]{geometry}\n"
     (tibetan-handout--latex-fonts)
     "#+LATEX_HEADER: \\usepackage{xcolor,fancyhdr,setspace,needspace,newunicodechar,longtable,array,enumitem,titlesec,pdflscape}\n"
     ;; §5.68: landscape synopsis pages, breakable underline for the
     ;; bhāṣya quotations (org's `_x_' exports to \uline as well).
     "#+LATEX_HEADER: \\usepackage[normalem]{ulem}\n"
     "#+LATEX_HEADER: \\newunicodechar{★}{\\ensuremath{\\star}}\n"
     "#+LATEX_HEADER: \\newunicodechar{⏑}{\\ensuremath{\\smile}}\n"
     "#+LATEX_HEADER: \\definecolor{seg}{rgb}{0.55,0.10,0.10}\\definecolor{dim}{gray}{0.40}\n"
     "#+LATEX_HEADER: \\pagestyle{fancy}\\fancyhf{}\\renewcommand{\\headrulewidth}{0.4pt}\n"
     (format "#+LATEX_HEADER: \\fancyhead[L]{\\small\\textit{%s} · %s}\\fancyhead[R]{\\small\\thepage}\n"
             (tibetan-handout--latex-escape title) head)
     (format "#+LATEX_HEADER: \\fancyfoot[L]{\\scriptsize\\color{dim}Stand %s}\n"
             (plist-get spec :date))
     "#+LATEX_HEADER: \\setlength{\\parindent}{0pt}\n"
     "#+LATEX_HEADER: \\titleformat{\\section}{\\Needspace{6\\baselineskip}\\large\\bfseries}{}{0pt}{}\n"
     "#+LATEX_HEADER: \\titleformat{\\subsection}{\\Needspace{5\\baselineskip}\\normalsize\\bfseries\\color{seg}}{}{0pt}{}\n"
     "#+LATEX_HEADER: \\titlespacing*{\\section}{0pt}{2.2ex plus .5ex}{1ex}\\titlespacing*{\\subsection}{0pt}{1.4ex plus .3ex}{.5ex}\n"
     "#+LATEX_HEADER: \\setlist[description]{font=\\normalfont\\scshape\\color{seg}\\footnotesize,leftmargin=0pt,labelsep=0.5em,itemsep=2pt,topsep=2pt,parsep=0pt}\n"
     tibetan-handout--latex-commands
     "\n")))

(defun tibetan-handout--text-part (sentences)
  "`* Text': every segment — Uchen above Wylie (bo) or the IAST (sa),
segment number in the margin, wide line spacing for handwriting."
  (concat
   "* Text\n#+BEGIN_EXPORT latex\n{\\setstretch{1.9}\n"
   (mapconcat
    (lambda (d)
      (concat
       (if (plist-get d :metre)
           (format "\\noindent{\\footnotesize\\color{dim}\\textit{%s}}\\par\n"
                   (tibetan-handout--latex-escape (plist-get d :metre)))
         "")
       (mapconcat
        (lambda (s)
          (let ((num (plist-get s :num))
                (wylie (tibetan-handout--latex-escape (plist-get s :wylie)))
                (uchen (plist-get s :uchen)))
            (if uchen
                (format "\\segno{%d}{\\tibfont\\large %s}\\par\n\\noindent{}%s\\par\\smallskip\n"
                        num (tibetan-handout--latex-escape uchen) wylie)
              (format "\\segno{%d}%s\\par\\smallskip\n" num wylie))))
        (plist-get d :segs) "")
       "\\medskip\n"))
    sentences "")
   ;; The passage stands on its own page(s) — it lies next to the
   ;; class text; the per-sentence look-up part starts fresh.
   "}\\clearpage\n#+END_EXPORT\n\n"))

(defconst tibetan-handout--discussion-labels
  '(("Issue" . "Problem") ("Assessment" . "Einschätzung")
    ("Question" . "Frage"))
  "German handout labels of the discussion's prose items (the handout
headings are German whatever the document language).")

(defun tibetan-handout--unclaude (s)
  "S with Claude named like its own handout section: /Übersetzung/.
The handout never says \"Claude\" (§5.59); its Übersetzung IS that
translation, so the discussion's label and prose mentions follow."
  (let ((case-fold-search nil))
    (replace-regexp-in-string "\\bClaude\\(?:'s\\|’s\\|s\\)?\\b"
                              "/Übersetzung/" s t t)))

(defun tibetan-handout--discussion-text (s)
  "Prose/quote S of a discussion item as handout org."
  (tibetan-handout--unclaude (tibetan-handout--md-to-org s)))

(defun tibetan-handout--discussion-block (disc)
  "`** Diskussion' for DISC (`tibetan-discussion-read'), \"\" when nil:
the assessment, then per point a bold title line and a description
list — the quotes (reference translators, /Übersetzung/, DharmaMitra)
followed by Problem / Einschätzung / Frage."
  (if (null disc)
      ""
    (concat
     "** Diskussion\n"
     (let ((a (plist-get disc :assessment)))
       (if (and a (not (string-empty-p a)))
           (concat (tibetan-handout--discussion-text a) "\n\n")
         ""))
     (cl-loop
      for p in (plist-get disc :points) for i from 1
      concat
      (let* ((items (plist-get p :items))
             (title (plist-get p :title))
             (text (cdr (assoc "Text" items)))
             (prose (cl-remove-if-not
                     (lambda (it) (assoc (car it) tibetan-handout--discussion-labels))
                     items))
             (quotes (cl-remove-if
                      (lambda (it) (or (equal (car it) "Text")
                                       (assoc (car it) tibetan-handout--discussion-labels)))
                      items)))
        (concat
         ;; The title is Claude prose like the items — markdown
         ;; emphasis inside it must become org italics BEFORE the
         ;; bold wrapper, or org shows raw stars (Tantrāloka V. 1).
         (format "*%d. %s*%s\n" i (tibetan-handout--discussion-text title)
                 (if (and text (not (equal text title)))
                     (format " — %s" (tibetan-handout--discussion-text text))
                   ""))
         (mapconcat (lambda (it)
                      (format "- %s :: “%s”"
                              (tibetan-handout--unclaude (car it))
                              (tibetan-handout--discussion-text (cdr it))))
                    quotes "\n")
         (if (and quotes prose) "\n" "")
         (mapconcat (lambda (it)
                      (format "- %s :: %s"
                              (cdr (assoc (car it) tibetan-handout--discussion-labels))
                              (tibetan-handout--discussion-text (cdr it))))
                    prose "\n")
         "\n\n"))))))

(defun tibetan-handout--metre-block (d)
  "`** Metrum' for the sentence data D (§5.65): the metre line and the
syllable table (syllables over – / ⏑, `¦' at the caesura) — \"\"
for Tibetan and prose.  Needs no analysis, so it stands even before
C-c u A."
  (let ((table (plist-get d :metre-table)))
    (if (not table)
        ""
      (let ((cells (- (length (split-string (car (split-string table "\n"))
                                            "|"))
                      2)))
        (concat
         "** Metrum\n"
         (plist-get d :metre) "\n\n"
         (format "#+ATTR_LATEX: :font \\scriptsize\\setlength{\\tabcolsep}{1.5pt} :align @{}l%s@{}\n"
                 (make-string (max 0 (1- cells)) ?c))
         table "\n\n")))))

(defun tibetan-handout--sentence-block (d)
  "`* Satz N (Seg. …)' for the sentence data D: compact Wylie,
Übersetzung, Vokabular, Grammatik, Diskussion (when there is one,
§5.62) — or the visible gap."
  (let* ((segs (plist-get d :segs))
         (multi (cdr segs))
         (tr (plist-get d :translation)))
    (concat
     (format "* Satz %d (Seg. %s)\n" (plist-get d :sent-num)
             (tibetan-handout--seg-range segs))
     "#+BEGIN_EXPORT latex\n{\\small\\itshape "
     (mapconcat (lambda (s)
                  (format "\\segsup{%d}%s" (plist-get s :num)
                          (tibetan-handout--latex-escape (plist-get s :wylie))))
                segs " ")
     "}\\par\n#+END_EXPORT\n\n"
     (tibetan-handout--metre-block d)
     (if (plist-get d :missing)
         "{{{luecke(Analysedatei fehlt — C-c u A auf dem Satz)}}}\n\n"
       (concat
        "** Übersetzung\n"
        (cond
         ((plist-get tr :by-seg)
          (concat (mapconcat (lambda (p)
                               (format "{{{n(%d)}}}%s" (car p)
                                       (tibetan-handout--md-to-org (cdr p))))
                             (plist-get tr :by-seg) " ")
                  "\n\n"))
         ((plist-get tr :whole)
          (concat (replace-regexp-in-string
                   "\n" " \\\\\\\\\n"
                   (tibetan-handout--md-to-org (plist-get tr :whole)))
                  "\n\n"))
         (t "{{{luecke(noch nicht übersetzt)}}}\n\n"))
        "** Vokabular\n"
        (if (plist-get d :vocab)
            (mapconcat
             (lambda (g)
               (concat
                (if (and multi (car g)) (format "{{{seg(%d)}}}\n\n" (car g)) "")
                "#+ATTR_LATEX: :environment longtable :align "
                "@{}>{\\raggedright\\bfseries}p{3.0cm}>{\\raggedright}p{6.6cm}"
                ">{\\raggedright\\arraybackslash\\footnotesize\\leavevmode\\color{dim}}p{3.1cm}@{}\n"
                (mapconcat
                 (lambda (e)
                   (format "| %s | %s%s | %s |"
                           (tibetan-handout--cell (plist-get e :term))
                           (concat (if (plist-get e :star) "★ " "")
                                   (tibetan-handout--cell (plist-get e :gloss)))
                           (if (plist-get e :context)
                               (format " {{{ctx(%s)}}}"
                                       (tibetan-handout--macro-arg
                                        (tibetan-handout--cell
                                         (plist-get e :context))))
                             "")
                           (tibetan-handout--cell (plist-get e :info))))
                 (cdr g) "\n")
                "\n\n"))
             (plist-get d :vocab) "")
          "{{{luecke(noch kein Vokabular)}}}\n\n")
        "** Grammatik\n"
        (if (plist-get d :grammar)
            (mapconcat
             (lambda (g)
               (concat
                (if (and multi (car g)) (format "{{{seg(%d)}}}\n\n" (car g)) "")
                (mapconcat
                 (lambda (b)
                   (let ((text (replace-regexp-in-string
                                "[ \t]*\n[ \t]*" " "
                                (tibetan-handout--md-to-org (cdr b)))))
                     (if (car b)
                         (format "- %s :: %s" (car b) text)
                       text)))
                 (cdr g) "\n")
                "\n\n"))
             (plist-get d :grammar) "")
          "{{{luecke(noch keine Grammatik)}}}\n\n")
        (tibetan-handout--discussion-block (plist-get d :discussion)))))))

(defun tibetan-handout--md-to-latex (s)
  "Claude's markdown emphasis in S as LaTeX, the rest escaped —
for the raw export blocks of the synopsis (§5.68)."
  (let ((out "") (pos 0) (s (or s "")))
    (while (string-match "\\*\\*\\([^*\n]+\\)\\*\\*\\|\\*\\([^*\n]+\\)\\*" s pos)
      (setq out (concat out
                        (tibetan-handout--latex-escape
                         (substring s pos (match-beginning 0)))
                        (if (match-string 1 s)
                            (format "\\textbf{%s}"
                                    (tibetan-handout--latex-escape (match-string 1 s)))
                          (format "\\emph{%s}"
                                  (tibetan-handout--latex-escape (match-string 2 s)))))
            pos (match-end 0)))
    (concat out (tibetan-handout--latex-escape (substring s pos)))))

(defconst tibetan-handout--conventions
  "/Konventionen:/ *fett* = Wortlaut des Verses, _unterstrichen_ = Wortlaut des Bhāṣya (Zitate im Kommentar); hochgestellte Zahlen = Segmentnummern der jeweiligen Datei."
  "The conventions line of the synopsis (§5.68), after the PhD
students' sheet (bold = verse, underlined = bhāṣya).")

(defun tibetan-handout--sigla-part (sigla synopsis)
  "`* Siglen' — SIGLA (Carsten's org body) and, when there is a
SYNOPSIS, the conventions line; \"\" without both."
  (cond ((and (null sigla) (null synopsis)) "")
        ((null sigla)
         (concat tibetan-handout--conventions "\n\n"))
        (t (concat "* Siglen\n" (string-trim-right sigla) "\n\n"
                   (if synopsis (concat tibetan-handout--conventions "\n\n") "")))))

(defun tibetan-handout--synopsis-cell (col)
  "The LaTeX of the synopsis column COL: sentences separated by
\\par, every segment with its number, the verse in bold."
  (mapconcat
   (lambda (s)
     (mapconcat (lambda (g)
                  (let ((text (tibetan-handout--latex-escape (plist-get g :wylie))))
                    (format "\\segsup{%d}%s" (plist-get g :num)
                            (if (plist-get s :verse)
                                (format "\\textbf{%s}" text)
                              text))))
                (plist-get s :segs) " "))
   (plist-get col :sentences) "\\par\n"))

(defun tibetan-handout--runs-latex (runs)
  "The LaTeX of the marked RUNS ((TEXT . KIND) …) of a segment."
  (mapconcat (lambda (r)
               (let ((text (tibetan-handout--latex-escape (car r))))
                 (pcase (cdr r)
                   ('verse (format "\\textbf{%s}" text))
                   ('bhasya (format "\\uline{%s}" text))
                   (_ text))))
             runs ""))

(defun tibetan-handout--commentary-latex (com)
  "The LaTeX of the commentary column COM: its siglum and locator,
then per sentence the marked Wylie and, when landed, the tool's
translation in italics."
  (concat
   (format "\\medskip\n{\\color{seg}\\footnotesize\\bfseries %s %s}\\par\\smallskip\n"
           (tibetan-handout--latex-escape (plist-get com :siglum))
           (tibetan-handout--latex-escape (plist-get com :locator)))
   (mapconcat
    (lambda (s)
      (let ((tr (plist-get s :translation)))
        (concat
         (mapconcat (lambda (g)
                      (format "\\segsup{%d}%s" (plist-get g :num)
                              (tibetan-handout--runs-latex
                               (or (plist-get g :runs)
                                   (list (cons (plist-get g :wylie) nil))))))
                    (plist-get s :segs) " ")
         "\\par\n"
         (cond
          ((plist-get tr :by-seg)
           (concat "{\\small\\itshape "
                   (mapconcat (lambda (p)
                                (format "\\segsup{%d}%s" (car p)
                                        (tibetan-handout--md-to-latex (cdr p))))
                              (plist-get tr :by-seg) " ")
                   "}\\par\\smallskip\n"))
          ((plist-get tr :whole)
           (format "{\\small\\itshape %s}\\par\\smallskip\n"
                   (tibetan-handout--md-to-latex (plist-get tr :whole))))
          (t "\\smallskip\n")))))
    (plist-get com :sentences) "")))

(defun tibetan-handout--synopsis-part (syn)
  "`* Synopse <id>' for the synopsis data SYN
\(`tibetan-handout--synopsis-data'): landscape pages with the locator
line (siglum + folios of every column and commentary), the synoptic
longtable of the columns and the commentaries below it."
  (let* ((columns (cons (plist-get syn :lead) (plist-get syn :parallels)))
         (coms (plist-get syn :commentaries))
         (width (/ 0.98 (max 1 (length columns)))))
    (concat
     ;; The landscape environment opens BEFORE the heading: pdflscape
     ;; starts a new page, and a heading set before that would stay
     ;; behind alone on the portrait page.
     "#+BEGIN_EXPORT latex\n\\begin{landscape}\n#+END_EXPORT\n"
     (format "* Synopse %s\n" (plist-get syn :id))
     "#+BEGIN_EXPORT latex\n"
     (format "{\\footnotesize\\color{dim}%s}\\par\\medskip\n"
             (mapconcat (lambda (c)
                          (tibetan-handout--latex-escape
                           (format "%s %s" (plist-get c :siglum) (plist-get c :locator))))
                        (append columns coms) " · "))
     (format "\\begin{longtable}{@{}%s@{}}\n"
             (mapconcat (lambda (_) (format "p{%.2f\\linewidth}" width)) columns ""))
     (mapconcat (lambda (c) (format "\\textbf{%s}"
                                    (tibetan-handout--latex-escape (plist-get c :siglum))))
                columns " & ")
     " \\\\ \\hline\n"
     (mapconcat #'tibetan-handout--synopsis-cell columns " &\n")
     " \\\\\n\\end{longtable}\n"
     (mapconcat #'tibetan-handout--commentary-latex coms "")
     "\\end{landscape}\n#+END_EXPORT\n\n")))

(defun tibetan-handout-render (spec)
  "The handout org text for SPEC — a plist (:title :scope :source-name
:date :sentences [:sigla :synopsis]), :sentences being
`tibetan-handout--sentence-data' plists, :synopsis a list of
`tibetan-handout--synopsis-data' plists (§5.68).  Pure: no file
access.  Layout (Carsten 09.10., 21.10.): `* Siglen' and per Section
`* Synopse' (landscape: locators, synoptic table, commentary with its
quotations marked), `* Text' (the whole passage: Uchen + Wylie per
segment, room for handwriting, 6 cm free right margin on every page),
then per sentence `* Satz N' with Übersetzung / Vokabular / Grammatik.
The word \"Claude\" never appears (headings are German; the slots are
named by content)."
  (let ((sentences (plist-get spec :sentences)))
    (concat (tibetan-handout--header spec)
            (tibetan-handout--sigla-part (plist-get spec :sigla)
                                         (plist-get spec :synopsis))
            (mapconcat #'tibetan-handout--synopsis-part
                       (plist-get spec :synopsis) "")
            (tibetan-handout--text-part sentences)
            (mapconcat #'tibetan-handout--sentence-block sentences ""))))

;; ============================================================================
;; B3 — scope, file, PDF, command
;; ============================================================================


(defun tibetan-handout--output-file (scope &optional kind)
  "`<source dir>/handouts/<short>-par-NNN.org' for a Lopez § SCOPE,
else `<short>-sent-N[-M].org'.  The source short name keeps
multi-source folders collision-free (§5.23 lesson).  KIND (a word,
e.g. \"disk\" for the discussion paper, §5.64) goes between the
short name and the range: `<short>-disk-par-NNN.org'."
  (let* ((src (plist-get scope :source))
         (nums (plist-get scope :sent-nums))
         (short (tibetan-analysis-make-short-name src)))
    (expand-file-name
     (concat "handouts/" short "-" (if kind (concat kind "-") "")
             (cond ((plist-get scope :lopez)
                    (format "par-%d" (plist-get scope :lopez)))
                   ((cdr nums)
                    (format "sent-%03d-%03d" (car nums) (car (last nums))))
                   (t (format "sent-%03d" (car nums))))
             ".org")
     (file-name-directory src))))

(defun tibetan-handout--source-title (source-file)
  "The work title: SOURCE-FILE's #+TITLE before \" — \", or its base name."
  (or (with-temp-buffer
        (insert-file-contents source-file nil 0 4096)
        (goto-char (point-min))
        (when (re-search-forward "^#\\+TITLE:[ \t]*\\(.+\\)$" nil t)
          (string-trim (car (split-string (match-string 1) " — ")))))
      (file-name-base source-file)))

(defun tibetan-handout-build (scope &optional date)
  "Write the handout .org for SCOPE (`tibetan-cascade--scope-at-point'
shape) and return its path.  The class wordlist (★) of the source's
Resources is bound for the whole build; DATE (default today) goes
into the footer.  An existing target WITHOUT the GENERATED marker is
never overwritten (user-error) — a generated one is regenerated."
  (let* ((src (plist-get scope :source))
         (out (tibetan-handout--output-file scope)))
    (when (and (file-exists-p out)
               (not (with-temp-buffer
                      (insert-file-contents out nil 0 512)
                      (string-prefix-p tibetan-handout-generated-marker
                                       (buffer-string)))))
      (user-error "%s existiert und trägt keinen GENERATED-Marker — nicht überschrieben"
                  (file-name-nondirectory out)))
    (let* ((tibetan-current-resources-vocab (tibetan-handout--load-curated src))
           (tibetan-current-custom-vocab nil)
           (meta (tibetan-analysis--read-source-metadata src))
           ;; §5.68: the synopsis only where the source names a
           ;; synopsis column or a commentary — one per Section of
           ;; the scope.
           (labels (and (or (plist-get meta :synopsis)
                            (plist-get meta :commentary))
                        (tibetan-handout--scope-sections
                         src (plist-get scope :sent-nums))))
           (spec (list :title (tibetan-handout--source-title src)
                       :scope (plist-get scope :label)
                       :source-name (file-name-nondirectory src)
                       :date (or date (format-time-string "%Y-%m-%d"))
                       :sentences (mapcar (lambda (n)
                                            (tibetan-handout--sentence-data n src))
                                          (plist-get scope :sent-nums))
                       :sigla (tibetan-handout--sigla-text src meta)
                       :synopsis (mapcar (lambda (l)
                                           (tibetan-handout--synopsis-data src l))
                                         labels))))
      (make-directory (file-name-directory out) t)
      (with-temp-file out
        (insert (tibetan-handout-render spec)))
      out)))

(defvar org-latex-pdf-process)
(declare-function org-latex-export-to-pdf "ox-latex")

(defun tibetan-handout-export-pdf (org-file)
  "Export ORG-FILE to PDF beside it and return the PDF path.
LuaLaTeX (the file's #+LATEX_COMPILER), two runs for longtable —
bound locally, so the user's own `org-latex-pdf-process' (latexmk …)
neither breaks nor is changed."
  (require 'ox-latex)
  (let ((buf (find-file-noselect org-file)))
    (unwind-protect
        (with-current-buffer buf
          (let ((org-latex-pdf-process
                 '("%latex -interaction nonstopmode -output-directory %o %f"
                   "%latex -interaction nonstopmode -output-directory %o %f")))
            (expand-file-name (org-latex-export-to-pdf)
                              (file-name-directory org-file))))
      (unless (buffer-modified-p buf)
        (kill-buffer buf)))))

(defun tibetan-handout--generated-buffer-p ()
  "Non-nil when the current buffer is a generated handout."
  (save-excursion
    (goto-char (point-min))
    (looking-at-p (regexp-quote tibetan-handout-generated-marker))))

;;;###autoload
(defun tibetan-handout (&optional org-only)
  "Reading-class handout for the scope at point (C-c u H).
In a cascade source: the sentence at point, or every sentence under
the heading at point (a § …); in a sent-NNN analysis buffer: that
sentence.  Writes `handouts/<short>-….org' beside the source (may be
shortened before printing; regenerating overwrites it), exports it
to PDF and opens the PDF.  With ORG-ONLY (C-u): only the .org, which
is opened for editing.  INSIDE a generated handout the buffer is
saved and exported as it is — shortening before printing is never
undone by a regeneration.  No network, no API call."
  (interactive "P")
  (if (and (buffer-file-name) (tibetan-handout--generated-buffer-p))
      (progn
        (save-buffer)
        (let ((pdf (tibetan-handout-export-pdf (buffer-file-name))))
          (org-open-file pdf)
          (message "Handout: %s" (file-name-nondirectory pdf))
          pdf))
    (tibetan-handout--from-scope org-only)))

(defun tibetan-handout--from-scope (org-only)
  "Body of `tibetan-handout' outside a generated handout buffer."
  (let ((scope (tibetan-cascade--scope-at-point)))
    (when (and (> (length (plist-get scope :sent-nums)) 50)
               (not (y-or-n-p (format "Handout über %d Sätze erzeugen? "
                                      (length (plist-get scope :sent-nums))))))
      (user-error "Abgebrochen"))
    (let ((org (tibetan-handout-build scope)))
      (if org-only
          (progn (find-file org)
                 (message "Handout (org): %s" (file-name-nondirectory org)))
        (let ((pdf (tibetan-handout-export-pdf org)))
          (org-open-file pdf)
          (message "Handout: %s" (file-name-nondirectory pdf))
          pdf)))))

(provide 'tibetan-handout)
;;; tibetan-handout.el ends here
