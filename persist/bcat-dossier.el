;;; bcat-dossier.el --- The reading dossier of a verse -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Carsten 5.11.2026: „Im Mahāyānasūtrālaṃkāra Reading sind wir ziemlich
;; schnell zwischen Vers, Vasubandhu und Sthiramati hin und her
;; gesprungen.  Da fiel es mir schwer mitzukommen.“  His model, a study
;; edition of MSA IX.60–66: per verse the layers one under the other,
;; each with its siglum.  Decisions: always per verse; the org file
;; first — „das .org, das ja mein 1st Screen beim Reading ist“; the
;; Abhidharmasamuccaya too (root sentence + its bhāṣya sentence).
;;
;; The dossier of a verse (or of an AS root sentence) is written from
;; the files that exist — no request:
;;   01 the text: Sanskrit and Tibetan lines, the translation
;;   02 the bhāṣya: Sanskrit and Tibetan, its translation (or a note
;;      that it comes with the next C-c u G), the hint
;;   03 the commentary (MSA: Sthiramati): per sentence „N · folio“,
;;      Uchen over Wylie — the lines that take up the verse in bold —,
;;      → its translation, the lemma
;; and „* Meine Notizen“, kept across every rebuild.  Template
;; dossier.org (cascade); file <text>/dossier/<name>.org beside
;; handouts/.  C-c u V; C-c u G ends with it.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'bcat-sentence)
(require 'bcat-template)

(declare-function bcat-relayout-file-context "bcat-relayout" (file))
(declare-function bcat-export-info "bcat-export" (file))
(declare-function bcat-export--split-proposal "bcat-export" (proposal))
(declare-function bcat-export-views "bcat-export" (file))
(declare-function bcat-commentary-source "bcat-commentary" (source-file))
(declare-function bcat-commentary-section "bcat-commentary" (label lead commentary-source))
(declare-function bcat-commentary-file "bcat-commentary" (section commentary-source))
(declare-function bcat-commentary--author "bcat-commentary" (commentary-source))
(declare-function tibetan-sentence--source-file-from-analysis "tibetan-sentence-persist" (analysis-file))
(declare-function tibetan-sentence--sent-id-from-filename "tibetan-sentence-persist" (filename))
(declare-function tibetan-sentence--filepath "tibetan-sentence-persist" (sent-num &optional folder source-file))
(declare-function tibetan-cascade--scope-at-point "tibetan-cascade" ())
(declare-function tibetan-discussion-paper--temp-source-p "tibetan-discussion-paper" (file))

(defcustom bcat-dossier-auto t
  "When non-nil, C-c u G ends with the dossier of the verse."
  :type 'boolean
  :group 'buddhist-cat)

(defconst bcat-dossier--notes "Meine Notizen"
  "The heading of the user's notes in a dossier — kept across rebuilds.")

(defconst bcat-dossier--missing-translation
  "[noch nicht übersetzt — kommt mit dem nächsten C-c u G]"
  "What stands for a layer's translation not fetched yet.")

(defun bcat-dossier--filled (v)
  "V when it is a string not blank, else nil."
  (and (stringp v) (not (string-blank-p v)) v))

;; ----------------------------------------------------------------------------
;; Layers
;; ----------------------------------------------------------------------------

(defun bcat-dossier--block (pairs)
  "The verse block of PAIRS ((LINE . TRANSCRIPTION) …), the empty parts
left out — or nil."
  (let ((lines (delq nil (apply #'append (mapcar (lambda (p) (list (bcat-dossier--filled (car p))
                                                                   (bcat-dossier--filled (cdr p))))
                                                 pairs)))))
    (and lines (string-join lines "\n"))))

(defun bcat-dossier--tibetan-pairs (items line-key)
  "(UCHEN . WYLIE) of ITEMS (alists with LINE-KEY and `wylie'); a missing
Wylie made from the Uchen."
  (mapcar (lambda (s)
            (let ((u (alist-get line-key s)))
              (cons u (or (bcat-dossier--filled (alist-get 'wylie s))
                          (and (bcat-dossier--filled u) (bcat-sentence--wylie u))))))
          items))

(defun bcat-dossier--translation (context)
  "The translation of the text of CONTEXT as it is read first: the
proposal without its decisions, else „*Translation:* …“; nil without."
  (let* ((proposal (car (bcat-export--split-proposal (or (alist-get 'proposal context) "")))))
    (or (bcat-dossier--filled proposal)
        (let ((tr (bcat-dossier--filled (alist-get 'translation context))))
          (and tr (concat "*Translation:* " tr))))))

(defun bcat-dossier-text-layer (context)
  "The text layer of a sentence file's CONTEXT: (verses . (BLOCK …))
— the Sanskrit lines (Devanāgarī, IAST), the Tibetan (Uchen, Wylie; of
the file itself or of its partner) — and (translation . TEXT)."
  (let* ((segs (alist-get 'segments context))
         (skt (bcat-dossier--block
               (append (mapcar (lambda (s) (cons (alist-get 'devanagari s) (alist-get 'samhita s))) segs)
                       (mapcar (lambda (s) (cons (alist-get 'devanagari s) (alist-get 'samhita s)))
                               (alist-get 'sanskrit context)))))
         (tib (bcat-dossier--block
               (append (bcat-dossier--tibetan-pairs (alist-get 'tibetan-lines context) 'line)
                       (bcat-dossier--tibetan-pairs
                        (cl-remove-if-not (lambda (s) (bcat-dossier--filled (alist-get 'uchen s))) segs)
                        'uchen)))))
    `((verses . ,(mapcar (lambda (b) `((text . ,b))) (delq nil (list skt tib))))
      (translation . ,(bcat-dossier--translation context)))))

(defun bcat-dossier-bhasya-layer (context)
  "The bhāṣya layer of a verse file's CONTEXT, or nil without bhāṣya:
its Sanskrit and Tibetan lines, its translation (`bhasya-translation',
else the note that it comes with the next fetch), the hint."
  (when (alist-get 'bhasya-p context)
    (let ((skt (bcat-dossier--block (mapcar (lambda (s) (cons (alist-get 'devanagari s) (alist-get 'samhita s)))
                                            (alist-get 'bhasya-sanskrit context))))
          (tib (bcat-dossier--block (bcat-dossier--tibetan-pairs (alist-get 'bhasya-tibetan context) 'uchen))))
      `((verses . ,(mapcar (lambda (b) `((text . ,b))) (delq nil (list skt tib))))
        (translation . ,(concat "*Translation:* "
                                (or (bcat-dossier--filled (alist-get 'bhasya-translation context))
                                    bcat-dossier--missing-translation)))
        (note . ,(bcat-dossier--filled (alist-get 'bhasya-hint context)))))))

(defun bcat-dossier--folios (commentary-source)
  "Hash: segment number → :FOLIO: of COMMENTARY-SOURCE."
  (let ((table (make-hash-table)))
    (when (and commentary-source (file-readable-p commentary-source))
      (with-temp-buffer
        (insert-file-contents commentary-source)
        (goto-char (point-min))
        (while (re-search-forward "^\\*+ Segment \\([0-9]+\\)[ \t]*\n[ \t]*:PROPERTIES:[ \t]*\n\\(?:[ \t]*:[^\n]*\n\\)*?[ \t]*:FOLIO:[ \t]*\\(\\S-+\\)" nil t)
          (puthash (string-to-number (match-string 1)) (match-string 2) table))))
    table))

(defun bcat-dossier--relink (text from to)
  "TEXT with its relative file links (written in a file of the folder
FROM) as seen from the folder TO."
  (replace-regexp-in-string
   "\\[\\[file:\\([^]:][^]:]*\\)"
   (lambda (m)
     (let ((path (match-string 1 m)))
       (concat "[[file:" (if (file-name-absolute-p path) path
                           (file-relative-name (expand-file-name path from) to)))))
   text t t))

(defun bcat-dossier-commentary-items (doc folios to &optional pdf)
  "The sentences of the commentary document DOC as items: (head . „N ·
folio“) with FOLIOS (`bcat-dossier--folios'), (lines . the Uchen and
Wylie lines, a line of a segment that takes up the verse — *Lemma:* or
„(lemma)“ in its analysis — in bold), (item-translation . …), (lemma .
the *Lemma:* line, its links seen from the folder TO), (item-link . into
the analysis of the sentence — the document, or with PDF its jump
target comm-N)."
  (let ((text (with-temp-buffer (insert-file-contents doc) (buffer-string)))
        (from (file-name-directory doc))
        (items '()) (start 0))
    (while (string-match "^\\* Sentence \\([0-9]+\\)[ \t]*\n" text start)
      (let* ((n (match-string 1 text))
             (beg (match-end 0))
             (end (if (string-match "^\\* " text beg) (match-beginning 0) (length text)))
             (body (substring text beg end))
             (verse (and (string-match "#\\+begin_verse\n\\(\\(?:.\\|\n\\)*?\\)\n#\\+end_verse" body)
                         (split-string (match-string 1 body) "\n")))
             (segs (let ((s 0) (out '()))
                     (while (string-match "^\\*\\* Segment \\([0-9]+\\)[^\n]*\n" body s)
                       (let* ((num (string-to-number (match-string 1 body)))
                              (sb (match-end 0))
                              (se (if (string-match "^\\*\\* " body sb) (match-beginning 0) (length body))))
                         (push (cons num (string-match-p "^\\*Lemma:\\*\\|(lemma)" (substring body sb se))) out)
                         (setq s se)))
                     (nreverse out)))
             (folio (and segs (gethash (car (car segs)) folios)))
             (lines '()))
        (cl-loop for (u w) on verse by #'cddr
                 for i from 0
                 do (push (if (and (cdr (nth i segs)) (bcat-dossier--filled u)) (format "*%s*" (string-trim u)) u)
                          lines)
                 (when w (push w lines)))
        (push `((head . ,(if folio (format "%s · %s" n folio) n))
                (item-link . ,(if pdf (format "[[#comm-%s][Analyse ↓]]" n)
                                (format "[[file:%s::*Sentence %s][Analyse]]" (file-relative-name doc to) n)))
                (lines . ,(string-join (nreverse lines) "\n"))
                (item-translation . ,(and (string-match "^\\*Translation:\\* \\(.+\\)$" body)
                                          (match-string 1 body)))
                (lemma . ,(and (string-match "^\\*Lemma:\\* \\(.+\\)$" body)
                               (bcat-dossier--relink (match-string 1 body) from to))))
              items)
        (setq start end)))
    (nreverse items)))

;; ----------------------------------------------------------------------------
;; The dossier of a sentence file
;; ----------------------------------------------------------------------------

(defun bcat-dossier--asbh-p (n source)
  "Non-nil when sentence N of SOURCE is a bhāṣya sentence (tag :asbh:)."
  (with-temp-buffer
    (insert-file-contents source)
    (goto-char (point-min))
    (re-search-forward (format "^\\*+ Sentence %d\\b[^\n]*:asbh:" n) nil t)))

(defun bcat-dossier--neighbour (n source direction)
  "The number of the sentence after (DIRECTION 1) or before (-1) N in
SOURCE, or nil."
  (let* ((nums (mapcar (lambda (s) (plist-get s :sent-num)) (bcat-sentence--sentences source)))
         (pos (cl-position n nums)))
    (and pos (nth (+ pos direction) nums))))

(defun bcat-dossier-files (file)
  "The files of the dossier the sentence FILE belongs to: (:lead FILE
:source S :sent-num N :bhasya FILE-or-nil) — the lead is the Sanskrit
file of a pair and the root sentence of an AS bhāṣya sentence."
  (let* ((source (tibetan-sentence--source-file-from-analysis file))
         (n (tibetan-sentence--sent-id-from-filename file)))
    (when (and source n)
      (when (equal (bcat-sentence-template-name source) "bilingual-tibetan")
        (let ((partner (bcat-sentence-partner-file n source)))
          (when partner
            (setq file partner
                  source (tibetan-sentence--source-file-from-analysis partner)
                  n (tibetan-sentence--sent-id-from-filename partner)))))
      (when (bcat-dossier--asbh-p n source)
        (let ((root (bcat-dossier--neighbour n source -1)))
          (when root
            (setq n root
                  file (tibetan-sentence--filepath root (bcat-sentence--analysis-folder source) source)))))
      (let* ((next (bcat-dossier--neighbour n source 1))
             (bhasya (and next (bcat-dossier--asbh-p next source)
                          (tibetan-sentence--filepath next (bcat-sentence--analysis-folder source) source))))
        (list :lead file :source source :sent-num n
              :bhasya (and bhasya (file-exists-p bhasya) bhasya))))))

(defun bcat-dossier--siglum (source)
  "The #+TIBETAN_SIGLUM of SOURCE, or nil."
  (and source (bcat-dossier--filled (bcat-sentence--header-value source "TIBETAN_SIGLUM"))))

(defun bcat-dossier--heading (no title sigla)
  "„01 TITLE (SIGLA joined by ·)“."
  (let ((sigla (delq nil sigla)))
    (format "%02d %s%s" no title (if sigla (format " (%s)" (string-join sigla " · ")) ""))))

(defun bcat-dossier-path (lead)
  "The dossier file of the LEAD sentence file: <text>/dossier/<name>.org,
named like its PDF in handouts/."
  (let ((handout (plist-get (bcat-export-info lead) :handout)))
    (and handout
         (expand-file-name (concat (file-name-base handout) ".org")
                           (expand-file-name "dossier" (file-name-directory
                                                        (directory-file-name (file-name-directory handout))))))))

(defun bcat-dossier-context (file &optional pdf)
  "The fields of the dossier the sentence FILE belongs to, or nil:
label, path, links to the analyses, layers (text, bhāṣya, commentary),
each with its link into the analysis — the file, or with PDF the jump
target of the dossier's PDF (#segments, #bhasya, #commentary, #comm-N);
`commentary-doc' and `commentary-author' for the PDF."
  (require 'bcat-relayout)
  (require 'bcat-export)
  (require 'bcat-commentary)
  (let* ((files (bcat-dossier-files file))
         (lead (plist-get files :lead))
         (read (and lead (file-exists-p lead) (bcat-relayout-file-context lead))))
    (when (and read (not (plist-get read :error)))
      (let* ((source (plist-get read :source))
             (ctx (plist-get read :context))
             (n (plist-get files :sent-num))
             (path (bcat-dossier-path lead))
             (to (file-name-directory path))
             (partner-source (bcat-sentence-parallel-source source))
             (verse-p (bcat-sentence--verse-mode-p source))
             (sigla (list (bcat-dossier--siglum source) (bcat-dossier--siglum partner-source)))
             (layers '())
             (comm-doc nil) (comm-author nil)
             (links (list (cons "Text" lead)))
             (partner (bcat-sentence-partner-file n source)))
        (when (and partner (file-exists-p partner)) (push (cons "Tib." partner) links))
        (push `((heading . ,(bcat-dossier--heading 1 (if verse-p "Vers" "Text") sigla))
                (link . ,(if pdf "[[#segments][Analyse ↓]]"
                           (format "[[file:%s][Analyse]]" (file-relative-name lead to))))
                ,@(bcat-dossier-text-layer ctx))
              layers)
        (let ((bh (bcat-dossier-bhasya-layer ctx))
              (bh-file (plist-get files :bhasya)))
          (cond
           (bh (push `((heading . ,(bcat-dossier--heading 2 "Bhāṣya" sigla)) ,@bh) layers))
           (bh-file
            (let ((bread (bcat-relayout-file-context bh-file)))
              (unless (plist-get bread :error)
                (push (cons "Bhāṣya" bh-file) links)
                (push `((heading . ,(bcat-dossier--heading 2 "Bhāṣya" sigla))
                        (link . ,(if pdf "[[#bhasya][Analyse ↓]]"
                                   (format "[[file:%s][Analyse]]" (file-relative-name bh-file to))))
                        ,@(bcat-dossier-text-layer (plist-get bread :context)))
                      layers))))))
        (let* ((commentary (and verse-p (bcat-commentary-source source)))
               (verse (and commentary (bcat-sentence--sentence n source)))
               (section (and verse (bcat-commentary-section (plist-get verse :section-label) source commentary)))
               (doc (and section (bcat-commentary-file section commentary))))
          (when (and doc (file-exists-p doc))
            (push (cons (bcat-commentary--author commentary) doc) links)
            (push `((heading . ,(bcat-dossier--heading (1+ (length layers)) (bcat-commentary--author commentary)
                                                       (list (bcat-dossier--siglum commentary))))
                    (items . ,(bcat-dossier-commentary-items doc (bcat-dossier--folios commentary) to pdf)))
                  layers)
            (setq comm-doc doc comm-author (bcat-commentary--author commentary))))
        `((label . ,(plist-get (bcat-export-info lead) :label))
          (path . ,path)
          (analysis . ,(mapconcat (lambda (l) (format "[[file:%s][%s]]" (file-relative-name (cdr l) to) (car l)))
                                  (nreverse links) " · "))
          (layers . ,(nreverse layers))
          (commentary-doc . ,comm-doc)
          (commentary-author . ,comm-author))))))

(defun bcat-dossier--keep-notes (path)
  "The body of „* Meine Notizen“ in the dossier PATH, or nil."
  (when (file-readable-p path)
    (bcat-sentence--l1-body (with-temp-buffer (insert-file-contents path) (buffer-string))
                            bcat-dossier--notes)))

(defun bcat-dossier-write (file)
  "Write the dossier the sentence FILE belongs to (template dossier,
cascade of its source); the notes stay.  Returns its path, or nil."
  (let ((ctx (bcat-dossier-context file)))
    (when ctx
      (let* ((path (alist-get 'path ctx))
             (notes (bcat-dossier--keep-notes path))
             (source (tibetan-sentence--source-file-from-analysis file))
             (text (bcat-template-render-named "dossier" (cons (cons 'my-notes (or notes "")) ctx) source)))
        (make-directory (file-name-directory path) t)
        (let ((coding-system-for-write 'utf-8-unix))
          (write-region text nil path nil 'silent))
        (let ((buf (get-file-buffer path)))
          (when buf (with-current-buffer buf (revert-buffer t t t))))
        path))))

;; ----------------------------------------------------------------------------
;; The PDF of the dossier (step 3)
;; ----------------------------------------------------------------------------

(defun bcat-dossier--split-parts (view)
  "VIEW (org text) as ((ID TITLE TEXT) …) per heading of level 1 — ID
its CUSTOM_ID or nil, TEXT from the heading on; what stands before the
first heading is left out."
  (let ((parts '()) (start 0))
    (while (string-match "^\\* \\(.+\\)$" view start)
      (let* ((beg (match-beginning 0))
             (title (match-string 1 view))
             (end (if (string-match "^\\* " view (match-end 0)) (match-beginning 0) (length view)))
             (text (substring view beg end))
             (id (and (string-match "\\`[^\n]*\n:PROPERTIES:\n\\(?:[^\n]*\n\\)*?:CUSTOM_ID: \\(\\S-+\\)" text)
                      (match-string 1 text))))
        (push (list id title text) parts)
        (setq start end)))
    (nreverse parts)))

(defun bcat-dossier--demote (text)
  "TEXT with every heading one level down."
  (replace-regexp-in-string "^\\(\\*+\\) " "*\\1 " text t))

(defconst bcat-dossier--page-break "#+LATEX: \\clearpage\n"
  "A new page in the PDF (as in the pdf-bilingual templates).")

(defun bcat-dossier--page-breaks (reading)
  "The reading page READING (the layers demoted to `** ') with a new page
before every layer but the first and before the second language of a
layer (a verse block right after another) — Carsten 9.11.: room for
notes on the reMarkable.  The commentary's sentences (`*** ') stay
together."
  (let ((first t))
    (replace-regexp-in-string
     "^#\\+end_verse\n\\(?:[ \t]*\n\\)*\\(#\\+begin_verse\\)"
     (lambda (m) (replace-regexp-in-string "#\\+begin_verse\\'"
                                           (concat bcat-dossier--page-break "#+begin_verse") m t t))
     (replace-regexp-in-string
      "^\\*\\* "
      (lambda (m) (if first (progn (setq first nil) m) (concat bcat-dossier--page-break m)))
      reading t t)
     t t)))

(defun bcat-dossier--nest-view (view title id parts)
  "The parts PARTS (CUSTOM_IDs, in VIEW's order) of the PDF view VIEW of
a second sentence file one level down under „* TITLE“ (CUSTOM_ID ID),
their CUSTOM_IDs and jumps prefixed „ID-“ so that they cannot meet the
first file's (AS: the bhāṣya sentence)."
  (let ((body (mapconcat (lambda (p) (nth 2 p))
                         (cl-remove-if-not (lambda (p) (member (car p) parts)) (bcat-dossier--split-parts view))
                         "")))
    (concat "* " title "\n:PROPERTIES:\n:CUSTOM_ID: " id "\n:END:\n"
            (replace-regexp-in-string
             "\\(:CUSTOM_ID: \\|\\[\\[#\\)\\([^]\n ]+\\)" (concat "\\1" id "-\\2")
             (bcat-dossier--demote body) t))))

(defun bcat-dossier--commentary-part (doc author)
  "The sentences of the commentary document DOC as the part „* AUTHOR“
of the PDF (CUSTOM_ID commentary), one level down, each a jump target
comm-N."
  (let ((text (with-temp-buffer (insert-file-contents doc) (buffer-string))))
    (concat "* " author "\n:PROPERTIES:\n:CUSTOM_ID: commentary\n:END:\n"
            (mapconcat (lambda (p)
                         (let ((title (nth 1 p)) (body (nth 2 p)))
                           (if (string-match "\\`Sentence \\([0-9]+\\)" title)
                               (let ((n (match-string 1 title)))
                                 (replace-regexp-in-string
                                  "\\`\\(\\*\\* [^\n]*\n\\)"
                                  (concat "\\1:PROPERTIES:\n:CUSTOM_ID: comm-" n "\n:END:\n")
                                  (bcat-dossier--demote body) t))
                             "")))
                       (bcat-dossier--split-parts text) ""))))

(defun bcat-dossier--lead-of (file)
  "The sentence file whose dossier FILE belongs to: for a commentary
document the first verse of its #+CAT_VERSE, else FILE."
  (if (equal (bcat-sentence-file-p file) "commentary")
      (let ((v (bcat-sentence--header-value file "CAT_VERSE")))
        (and v (string-match "\\[\\[file:\\([^]:]+\\)" v)
             (expand-file-name (match-string 1 v) (file-name-directory file))))
    file))

(defun bcat-dossier-pdf-text (file)
  "The PDF of the dossier FILE belongs to (Carsten 5.11.): (:lead LEAD
:text ORG) — the reading page first (the layers as in the dossier, each
with its jump into the analysis), then the verse's Segments and Grammar,
the bhāṣya sentence's analysis (AS), the commentary's sentences, the
verse's Concepts, Discussion, Further Reading, Notes; the dossier's
notes at the end.  nil for a sentence with one layer — it keeps its own
view (`bcat-export-views')."
  (require 'bcat-export)
  (let* ((file (bcat-dossier--lead-of file))
         (ctx (and file (file-exists-p file) (bcat-dossier-context file t))))
    (when (and ctx (> (length (alist-get 'layers ctx)) 1))
      (let* ((files (bcat-dossier-files file))
             (lead (plist-get files :lead))
             (source (tibetan-sentence--source-file-from-analysis lead))
             (reading (let ((text (bcat-template-render-named "dossier" (cons '(my-notes . "") ctx) source)))
                        (bcat-dossier--page-breaks
                         (bcat-dossier--demote
                          (substring text (string-match "^\\* " text)
                                     (string-match (concat "^\\* " bcat-dossier--notes) text))))))
             (parts (bcat-dossier--split-parts (plist-get (bcat-export-views lead) :view)))
             (part (lambda (id) (or (nth 2 (assoc id parts)) "")))
             (bh-file (plist-get files :bhasya))
             (bh (and bh-file (let ((v (plist-get (bcat-export-views bh-file) :view)))
                                (and v (bcat-dossier--nest-view v "Bhāṣya" "bhasya"
                                                                '("segments" "grammar" "concepts" "discussion"))))))
             (doc (alist-get 'commentary-doc ctx))
             (notes (bcat-dossier--filled (bcat-dossier--keep-notes (alist-get 'path ctx)))))
        (list :lead lead
              :text (concat
                     (format "#+TITLE: %s — Lesedossier\n#+OPTIONS: toc:nil num:nil title:nil author:nil date:nil\n\n"
                             (alist-get 'label ctx))
                     "* Reading\n:PROPERTIES:\n:CUSTOM_ID: reading\n:END:\n" reading
                     (funcall part "segments") (funcall part "grammar")
                     (or bh "")
                     (if doc (bcat-dossier--commentary-part doc (alist-get 'commentary-author ctx)) "")
                     (funcall part "concepts") (funcall part "discussion")
                     (funcall part "further-reading") (funcall part "notes")
                     (if notes (concat "* " bcat-dossier--notes "\n:PROPERTIES:\n:CUSTOM_ID: dossier-notes\n:END:\n" notes "\n") "")))))))

(defvar bcat-commentary--runs)
(declare-function bcat-sentence-fetch-state "bcat-sentence" (file seg-nums))

(defvar bcat-dossier--awaiting (make-hash-table :test #'equal)
  "Verse file → the commentary document C-c u G fetches with it.  Set
before the verse is sent, cleared when the commentary lands: until
then the verse's own landing does not export the dossier — whichever
lands first (5.11.).")

(defun bcat-dossier-await (verse-file doc)
  "Note that C-c u G fetches the commentary document DOC with the verse
VERSE-FILE (see `bcat-dossier--awaiting')."
  (puthash (expand-file-name verse-file) doc bcat-dossier--awaiting))

(defun bcat-dossier-export-pending-p (file)
  "Non-nil when the PDF of FILE's dossier should wait — Carsten 5.11.:
„doppelten Upload weg“.  One C-c u G on a verse with commentary lands
twice; the first to land waits for the other: a verse file while its
commentary document is still being fetched (`bcat-commentary--runs'),
the commentary document while its verse has not landed
\(`bcat-sentence-fetch-state' of the verse file non-nil).  The landing
of the commentary clears the verse's note (`bcat-dossier-await')."
  (require 'bcat-commentary)
  (if (equal (bcat-sentence-file-p file) "commentary")
      (let* ((lead (bcat-dossier--lead-of file))
             (_ (when lead (remhash (expand-file-name lead) bcat-dossier--awaiting)))
             (source (and lead (tibetan-sentence--source-file-from-analysis lead)))
             (n (and lead (tibetan-sentence--sent-id-from-filename lead)))
             (verse (and source n (bcat-sentence--sentence n source))))
        (and verse (file-exists-p lead)
             (bcat-sentence-fetch-state lead (mapcar #'car (plist-get verse :segs)))
             t))
    (or (and (gethash (expand-file-name file) bcat-dossier--awaiting) t)
        (let* ((ctx (ignore-errors (bcat-dossier-context file)))
               (doc (alist-get 'commentary-doc ctx)))
          (and doc (gethash doc bcat-commentary--runs) t)))))

(defun bcat-dossier-after-fetch (file)
  "Hook for `bcat-sentence-after-fetch-functions': C-c u G ends with the
dossier (`bcat-dossier-auto'); never for a fixture in the temp folder;
never signals."
  (require 'tibetan-discussion-paper)
  (when (and bcat-dossier-auto (not (tibetan-discussion-paper--temp-source-p file)))
    (condition-case err
        (bcat-dossier-write file)
      (error (message "Lesedossier (%s): %s" (file-name-nondirectory file) (error-message-string err))))))

(add-hook 'bcat-sentence-after-fetch-functions #'bcat-dossier-after-fetch)

;;;###autoload
(defun bcat-dossier ()
  "C-c u V: the reading dossier of the verse — of the sentence file at
point, or in a source of the sentence at point — written anew from the
files and shown."
  (interactive)
  (let* ((file (buffer-file-name))
         (file (if (and file (bcat-sentence-file-p file))
                   file
                 (require 'tibetan-cascade)
                 (let* ((scope (tibetan-cascade--scope-at-point))
                        (source (plist-get scope :source)))
                   (tibetan-sentence--filepath (car (plist-get scope :sent-nums))
                                               (bcat-sentence--analysis-folder source) source))))
         (path (and file (file-exists-p file) (bcat-dossier-write file))))
    (if path (find-file path) (user-error "Kein Lesedossier für %s" (and file (file-name-nondirectory file))))))

(provide 'bcat-dossier)
;;; bcat-dossier.el ends here
