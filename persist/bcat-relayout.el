;;; bcat-relayout.el --- Set a sentence file anew without a fetch -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Carsten 31.10.2026 abends, TĀ 1006: the sentence files got a new
;; order — tables and translation first, then grammar, concepts,
;; references, the rest, commentary, further reading (984d377).  For the
;; files analysed already: „Ohne Abruf umbauen“.  No answer of Claude's
;; is kept for them, so the fields are read back from the file itself —
;; in the old order (tables and labels per segment) and in the new one
;; (⟦N⟧ marks under * Text, Concepts, * Tibetan/* Sanskrit) — and the
;; file is rendered from its template again.  C-c u L.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'bcat-sentence)

(declare-function tibetan-cascade--scope-at-point "tibetan-cascade" ())
(declare-function tibetan-sentence--filepath "tibetan-sentence-persist" (sent-num &optional folder source-file))
(declare-function tibetan-sentence--sent-id-from-filename "tibetan-sentence-persist" (filepath))
(declare-function tibetan-sentence--source-file-from-analysis "tibetan-sentence-persist" (analysis-file))

;; ----------------------------------------------------------------------------
;; Reading a sentence file back
;; ----------------------------------------------------------------------------

(defun bcat-relayout--section (text level title)
  "The body of the heading of LEVEL titled TITLE (a regexp matching the
whole title) in TEXT, up to the next heading of that level or higher;
nil when absent."
  (let ((re (format "^%s %s[ \t]*\n" (make-string level ?*) title)))
    (when (string-match re text)
      (let ((start (match-end 0)))
        (substring text start
                   (if (string-match (format "^\\*\\{1,%d\\} " level) text start)
                       (match-beginning 0)
                     (length text)))))))

(defun bcat-relayout--before-l2 (body)
  "BODY up to its first L2 heading."
  (if (and body (string-match "^\\*\\* " body)) (substring body 0 (match-beginning 0)) body))

(defun bcat-relayout--paragraphs (body)
  "The paragraphs of BODY, trimmed."
  (and body (split-string body "\n[ \t]*\n" t "[ \t\n]+")))

(defun bcat-relayout--labelled (p)
  "(FIELD . VALUE) when the paragraph P is one of a segment's labelled
fields, else nil."
  (cond
   ((string-match "\\`\\*Forms:\\*\n" p) (cons 'forms (substring p (match-end 0))))
   ((string-match "\\`\\*\\(Sandhi\\|Compounds\\|Structure\\|Note\\|Concepts\\|Differences\\):\\* " p)
    (cons (intern (downcase (match-string 1 p))) (substring p (match-end 0))))))

(defun bcat-relayout--verse-body (p)
  "The lines of the verse block that is (or ends) the paragraph P."
  (when (and p (string-match "^#\\+begin_verse\n\\(\\(?:.\\|\n\\)*?\\)\n#\\+end_verse\\'" p))
    (match-string 1 p)))

(defun bcat-relayout--put (table num field value &optional append)
  "Set FIELD of segment NUM in the hash TABLE to VALUE — joined to an
existing value by a blank line when APPEND."
  (let* ((fields (gethash num table))
         (old (alist-get field fields)))
    (puthash num (cons (cons field (if (and append old) (concat old "\n\n" value) value))
                       (assq-delete-all field fields))
             table)))

(defun bcat-relayout--segments (text block-field)
  "The segment fields of the sentence file TEXT as a hash NUM → alist:
from the `** Segment N' blocks (both orders: the tables there in the
old one, ⟦N⟧ translations, the labelled fields, the verse block of the
other language as BLOCK-FIELD, the heading's „ · Tib. …“), and in the
new order from the ⟦N⟧ marks — the tables under * Text, the concepts,
the other language's blocks and differences."
  (let ((table (make-hash-table))
        (segments (bcat-relayout--section text 1 "Segments")))
    ;; ** Segment N blocks.
    (when segments
      (let ((pos 0))
        (while (string-match "^\\*\\* Segment \\([0-9]+\\)\\(.*\\)\n" segments pos)
          (let* ((num (string-to-number (match-string 1 segments)))
                 (parallel (match-string 2 segments))
                 (start (match-end 0))
                 (end (if (string-match "^\\*\\* " segments start) (match-beginning 0) (length segments)))
                 (tables '()))
            (setq pos end)
            (bcat-relayout--put table num 'parallel parallel)
            (dolist (p (bcat-relayout--paragraphs (substring segments start end)))
              (let ((labelled (bcat-relayout--labelled p)))
                (cond
                 ((string-prefix-p "|" p) (push p tables))
                 ((string-match-p "\\`⟦[0-9]+⟧ " p) (bcat-relayout--put table num 'translations p))
                 ((bcat-relayout--verse-body p)
                  (bcat-relayout--put table num block-field (bcat-relayout--verse-body p)))
                 (labelled (bcat-relayout--put table num (car labelled) (cdr labelled))))))
            (when tables
              (bcat-relayout--put table num 'gloss-table (string-join (nreverse tables) "\n\n")))))))
    ;; ⟦N⟧ under * Text: the tables — the mark on a line of its own, or
    ;; (files of 31.10. before the PDF showed it beside the table) right
    ;; above the first one.
    (let ((current nil))
      (dolist (p (bcat-relayout--paragraphs (bcat-relayout--section text 1 "Text")))
        (cond
         ((string-match "\\`⟦\\([0-9]+\\)⟧\\'" p)
          (setq current (string-to-number (match-string 1 p))))
         ((string-match "\\`⟦\\([0-9]+\\)⟧\n" p)
          (setq current (string-to-number (match-string 1 p)))
          (bcat-relayout--put table current 'gloss-table (substring p (match-end 0))))
         ((and current (string-prefix-p "|" p))
          (bcat-relayout--put table current 'gloss-table p t))
         (t (setq current nil)))))
    ;; ⟦N⟧ under Concepts.
    (let ((current nil))
      (dolist (p (bcat-relayout--paragraphs
                  (bcat-relayout--section (or (bcat-relayout--section text 1 "Sentence") "") 2 "Concepts")))
        (if (string-match "\\`⟦\\([0-9]+\\)⟧ " p)
            (progn (setq current (string-to-number (match-string 1 p)))
                   (bcat-relayout--put table current 'concepts (substring p (match-end 0))))
          (when current (bcat-relayout--put table current 'concepts p t)))))
    ;; ⟦N⟧ under * Tibetan / * Sanskrit: the other language, its differences.
    (let ((current nil))
      (dolist (p (bcat-relayout--paragraphs
                  (bcat-relayout--before-l2
                   (or (bcat-relayout--section text 1 "\\(?:Tibetan\\|Sanskrit\\)") ""))))
        (cond
         ((string-match "\\`⟦\\([0-9]+\\)⟧.*\n" p)
          (setq current (string-to-number (match-string 1 p)))
          (let ((body (bcat-relayout--verse-body p)))
            (when body (bcat-relayout--put table current block-field body))))
         ((and current (string-match "\\`\\*Differences:\\* " p))
          (bcat-relayout--put table current 'differences (substring p (match-end 0)))))))
    table))

(defun bcat-relayout--assignment (text)
  "The assignment ((TIB . SKT) …) the segment headings of TEXT name —
„Segment 23 · Tib. 78–79“ (the Sanskrit file), „Segment 78 · Skt. 23“
\(the Tibetan one); nil when none does."
  (let ((pos 0) (out '()))
    (while (string-match "^\\*\\* Segment \\([0-9]+\\) · \\(Tib\\|Skt\\)\\. \\(.+\\)$" text pos)
      (let ((num (string-to-number (match-string 1 text)))
            (kind (match-string 2 text))
            (spec (match-string 3 text)))
        (setq pos (match-end 0))
        (if (equal kind "Skt")
            (push (cons num (string-to-number spec)) out)
          (dolist (part (split-string spec ", *" t))
            (let* ((ends (mapcar #'string-to-number (split-string part "–")))
                   (from (car ends)) (to (or (cadr ends) from)))
              (cl-loop for tib from from to to do (push (cons tib num) out)))))))
    (nreverse out)))

(defun bcat-relayout--sentence-fields (text base)
  "The sentence-level fields of the sentence file TEXT: the translation
and the one in the second language (under * Text), grammar, comparison,
rendering, word analysis, the hint on the bhāṣya, the date of the
analysis — and for prose (BASE has no scansion) the recitation Claude
divided into sense units."
  (let* ((sentence (or (bcat-relayout--section text 1 "Sentence") ""))
         (field (lambda (key body) (and body (not (string-blank-p body))
                                        (cons key (string-trim body)))))
         (out '()))
    (dolist (p (bcat-relayout--paragraphs (bcat-relayout--section text 1 "Text")))
      (cond
       ((string-match "\\`\\*Translation:\\* " p)
        (push (cons 'translation (substring p (match-end 0))) out))
       ((string-match "\\`\\*\\(Deutsch\\|English\\):\\* " p)
        (push (cons 'second-label (match-string 1 p)) out)
        (push (cons 'second-translation (substring p (match-end 0))) out))))
    (when (string-match "^\\*Hint:\\* \\(.*\\(?:\n.+\\)*\\)" text)
      (push (cons 'bhasya-hint (match-string 1 text)) out))
    (when (string-match "^#\\+LAST_ANALYZED:[ \t]*\\(.+\\)$" text)
      (push (cons 'last-analyzed (string-trim (match-string 1 text))) out))
    (unless (alist-get 'scansion base)
      (let ((verse (bcat-relayout--verse-body
                    (car (last (bcat-relayout--paragraphs
                                (bcat-relayout--section text 1 "Recitation")))))))
        (when verse (push (cons 'recitation verse) out))))
    (append
     (delq nil
           (list (funcall field 'grammar (bcat-relayout--section sentence 2 "Grammar"))
                 (funcall field 'comparison (bcat-relayout--section sentence 2 "Translations compared"))
                 (funcall field 'rendering (or (bcat-relayout--section text 2 "Tibetan rendering")))
                 (funcall field 'word-analysis
                          (bcat-relayout--section (or (bcat-relayout--section text 1 "Segments") "")
                                                  2 "Word Analysis"))))
     (nreverse out))))

(defun bcat-relayout-context (base text template)
  "The template fields of the sentence file TEXT (template TEMPLATE):
BASE — its rump, what C-c u A knows — completed from what the file
holds, read back in the old order or the new one.  Segments by number;
the assignment of the other language from the segment headings."
  (let* ((block-field (if (equal template "bilingual-tibetan") 'sanskrit-block 'tibetan-block))
         (read (bcat-relayout--segments text block-field))
         (assignment (bcat-relayout--assignment text))
         (segments (mapcar (lambda (seg)
                             (append (gethash (bcat-analysis--num seg) read) seg))
                           (alist-get 'segments base))))
    (append `((segments . ,segments))
            (and assignment `((assignment . ,assignment)))
            (bcat-relayout--sentence-fields text base)
            base)))

;; ----------------------------------------------------------------------------
;; C-c u L
;; ----------------------------------------------------------------------------

(defun bcat-relayout-file (file)
  "Set the template sentence FILE anew from its template, without a
fetch (`bcat-relayout-context'): user slots, the file's own sections,
#+CREATED and #+LAST_ANALYZED stay; the answer kept at the fetch
\(`bcat-sentence--answers') fills what the file does not show.
Returns FILE, or nil (with a message) when its source has another
template now — C-c u G rebuilds it."
  (let* ((source (tibetan-sentence--source-file-from-analysis file))
         (template (bcat-sentence-file-p file))
         (sent-num (tibetan-sentence--sent-id-from-filename file)))
    (cond
     ((not (and source template sent-num))
      (message "C-c u L: %s ist keine Satzdatei einer Vorlage" (file-name-nondirectory file))
      nil)
     ((not (equal template (bcat-sentence-template-name source)))
      (message "C-c u L: %s hat die Vorlage %s, die Quelle %s — C-c u G baut sie neu"
               (file-name-nondirectory file) template (bcat-sentence-template-name source))
      nil)
     (t
      (bcat-sentence--save-visit file)
      (let* ((text (bcat-sentence--text file))
             (sentence (bcat-sentence--sentence sent-num source))
             (segs (plist-get sentence :segs))
             (base (bcat-sentence-rump-context sent-num segs source nil (bcat-sentence--keep file)))
             ;; The answer kept at the fetch fills what the file does
             ;; not show; what it shows wins (a hand correction stays).
             (record (bcat-sentence--answers file))
             (base (if (plist-get record :parsed)
                       (bcat-sentence--fetched-context base sentence source (plist-get record :refs)
                                                       (plist-get record :parsed) (plist-get record :tibetan))
                     base))
             (context (bcat-relayout-context base text template))
             (assignment (alist-get 'assignment context))
             (context (if assignment
                          (bcat-sentence--commentary-fields sent-num source context assignment)
                        context)))
        (bcat-sentence--write file (bcat-sentence--render template context source))
        file)))))

;;;###autoload
(defun bcat-relayout ()
  "C-c u L: set sentence files anew from their template, without a
fetch (Carsten 31.10.: „Ohne Abruf umbauen“) — after a change of the
template, the files analysed before it.  In a sentence file: that file
\(in a bilingual pair its partner too); in a source: every existing
file of the sentence at point or of the sentences under the heading at
point."
  (interactive)
  (require 'tibetan-cascade)
  (let* ((scope (tibetan-cascade--scope-at-point))
         (source (plist-get scope :source))
         (folder (file-name-as-directory (expand-file-name "analysis" (file-name-directory source))))
         (files '()))
    (dolist (n (plist-get scope :sent-nums))
      (let* ((s (or (bcat-sentence-owner n source) (list :sent-num n)))
             (file (tibetan-sentence--filepath (plist-get s :sent-num) folder source))
             (partner (and (bcat-sentence--paired-p source)
                           (bcat-sentence-partner-file (plist-get s :sent-num) source))))
        (dolist (f (list file partner))
          (when (and f (file-exists-p f) (bcat-sentence-file-p f))
            (push f files)))))
    (setq files (delete-dups (nreverse files)))
    (let ((done (delq nil (mapcar #'bcat-relayout-file files))))
      (message "C-c u L: %d Datei(en) neu gesetzt%s" (length done)
               (if (= (length done) (length files)) ""
                 (format ", %d übersprungen (siehe *Messages*)" (- (length files) (length done)))))
      done)))

(provide 'bcat-relayout)
;;; bcat-relayout.el ends here
