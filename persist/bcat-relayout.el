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
                 ((string-match "\\`⟦[0-9]+⟧ \\(?:\\*Translation:\\* \\)?\\(.*?\\)\\(?: \\\\\\\\\\)?$" p)
                  ;; Its first line is the segment's own translation —
                  ;; what * Text shows after the tables since 1.11., for
                  ;; a file written before.
                  (bcat-relayout--put table num 'segment-translation (match-string 1 p))
                  (bcat-relayout--put table num 'translations p))
                 ((bcat-relayout--verse-body p)
                  (bcat-relayout--put table num block-field (bcat-relayout--verse-body p)))
                 (labelled (bcat-relayout--put table num (car labelled) (cdr labelled))))))
            (when tables
              (bcat-relayout--put table num 'gloss-table (string-join (nreverse tables) "\n\n")))))))
    ;; ⟦N⟧ under * Text: the tables — the mark on a line of its own
    ;; (with „ · Tib. …“ in a bilingual file), or (files of 31.10. before
    ;; the PDF showed it beside the table) right above the first one —
    ;; then „→“ the segment's translation.  What follows in a bilingual
    ;; file (1.11.: the other language's tables and translations) comes
    ;; from the partner file (`bcat-relayout-context'); the differences
    ;; belong to the segment.
    (let ((current nil) (own nil))
      (dolist (p (bcat-relayout--paragraphs (bcat-relayout--section text 1 "Text")))
        (cond
         ((string-match "\\`\\(?:<<[^>\n]*>>\\)?⟦\\([0-9]+\\)⟧[^\n]*\\'" p)
          (setq current (string-to-number (match-string 1 p)) own t))
         ((string-match "\\`\\(?:<<[^>\n]*>>\\)?⟦\\([0-9]+\\)⟧[^\n]*\n" p)
          (setq current (string-to-number (match-string 1 p)) own t)
          (bcat-relayout--put table current 'gloss-table (substring p (match-end 0))))
         ((and current (string-prefix-p "|" p))
          (when own (bcat-relayout--put table current 'gloss-table p t)))
         ((and current (string-match "\\`→ " p))
          (when own
            (bcat-relayout--put table current 'segment-translation (substring p (match-end 0)))
            (setq own nil)))
         ((and current (string-match "\\`\\*Differences:\\* " p))
          (bcat-relayout--put table current 'differences (substring p (match-end 0))))
         (t (setq current nil)))))
    ;; ⟦N⟧ under Concepts.
    (let ((current nil))
      (dolist (p (bcat-relayout--paragraphs
                  (bcat-relayout--section (or (bcat-relayout--section text 1 "Sentence") "") 2 "Concepts")))
        (if (string-match "\\`⟦\\([0-9]+\\)⟧ " p)
            (progn (setq current (string-to-number (match-string 1 p)))
                   (bcat-relayout--put table current 'concepts (substring p (match-end 0))))
          (when current (bcat-relayout--put table current 'concepts p t)))))
    ;; ⟦N⟧ under * Tibetan / * Sanskrit, the other language: there its
    ;; differences.  Since 1.11. mittags the marks are the other
    ;; language's („<<tib-78>>⟦78⟧ · [[skt-23][Skt. 23]]“) — the
    ;; differences after them belong to the Sanskrit segment the link
    ;; names; before (7f0ee89) ⟦N⟧ was this file's segment.
    (let ((current nil))
      (dolist (p (bcat-relayout--paragraphs
                  (bcat-relayout--before-l2
                   (or (bcat-relayout--section text 1 "\\(?:Tibetan\\|Sanskrit\\)") ""))))
        (cond
         ((string-match "\\`\\(?:<<[^>\n]*>>\\)?⟦\\([0-9]+\\)⟧\\([^\n]*\\)" p)
          (let ((mark (match-string 2 p)) (n (string-to-number (match-string 1 p))))
            (setq current (if (string-match "Skt\\. \\([0-9]+\\)" mark)
                              (string-to-number (match-string 1 mark))
                            n)))
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

(defconst bcat-relayout--proposal-words
  "\\(?:Proposed translation\\|Übersetzungsvorschlag\\)"
  "The words of the proposal, English and German
\(`bcat-analysis--comparison-labels').")

(defun bcat-relayout--old-proposal (comparison)
  "(COMPARISON-WITHOUT . PROPOSAL) for a Translations compared of the
layout before 4.11.: its subsection *** Proposed translation (or ***
Übersetzungsvorschlag) taken out and set as it stands under the tables
now — each quote with „*WORD — LABEL:*“ or „*WORD:*“, the decisions as
they are; (COMPARISON . nil) without one."
  (if (not (and comparison
                (string-match (concat "^\\*\\*\\* \\(" bcat-relayout--proposal-words "\\)[ \t]*\n")
                              comparison)))
      (cons comparison nil)
    (let* ((word (match-string 1 comparison))
           (start (match-beginning 0))
           (body-start (match-end 0))
           (end (if (string-match "^\\*\\*\\* " comparison body-start) (match-beginning 0) (length comparison)))
           (paras (bcat-relayout--paragraphs (substring comparison body-start end))))
      (cons (concat (substring comparison 0 start) (substring comparison end))
            (string-join
             (mapcar (lambda (p)
                       (cond ((string-match "\\`\\*\\([^*\n]+\\):\\*\\(\n#\\+begin_quote\\)" p)
                              (concat "*" word " — " (match-string 1 p) ":*" (substring p (match-beginning 2))))
                             ((string-prefix-p "#+begin_quote" p) (concat "*" word ":*\n" p))
                             (t p)))
                     paras)
             "\n\n")))))

(defun bcat-relayout--sentence-fields (text base)
  "The sentence-level fields of the sentence file TEXT: the translation
and the one in the second language, grammar, proposal (since 4.11. all
under the tables in * Text; before, grammar under * Sentence and the
proposal inside Translations compared — both read and moved),
comparison, rendering, word analysis, the hint on the bhāṣya, the date
of the analysis — and for prose (BASE has no scansion) the recitation
Claude divided into sense units."
  (let* ((sentence (or (bcat-relayout--section text 1 "Sentence") ""))
         (field (lambda (key body) (and body (not (string-blank-p body))
                                        (cons key (string-trim body)))))
         (grammar nil) (proposal '())
         (out '()))
    (dolist (p (bcat-relayout--paragraphs (bcat-relayout--section text 1 "Text")))
      (cond
       ((string-match "\\`\\*Translation:\\* " p)
        (push (cons 'translation (substring p (match-end 0))) out))
       ((string-match "\\`\\*\\(Deutsch\\|English\\):\\* " p)
        (push (cons 'second-label (match-string 1 p)) out)
        (push (cons 'second-translation (substring p (match-end 0))) out))
       ((string-match "\\`\\*Grammar:\\* " p)
        (setq grammar (substring p (match-end 0))))
       ((string-match (concat "\\`\\*\\(?:" bcat-relayout--proposal-words "\\|Decisions\\|Entscheidungen\\)"
                              "\\(?: — [^*\n]*\\)?:\\*")
                      p)
        (push p proposal))))
    (when (string-match "^\\*Hint:\\* \\(.*\\(?:\n.+\\)*\\)" text)
      (push (cons 'bhasya-hint (match-string 1 text)) out))
    (when (string-match "^#\\+LAST_ANALYZED:[ \t]*\\(.+\\)$" text)
      (push (cons 'last-analyzed (string-trim (match-string 1 text))) out))
    (unless (alist-get 'scansion base)
      (let ((verse (bcat-relayout--verse-body
                    (car (last (bcat-relayout--paragraphs
                                (bcat-relayout--section text 1 "Recitation")))))))
        (when verse (push (cons 'recitation verse) out))))
    (let* ((compared (bcat-relayout--old-proposal
                      (bcat-relayout--section sentence 2 "Translations compared")))
           (proposal (or (and proposal (string-join (nreverse proposal) "\n\n"))
                         (cdr compared)))
           ;; the own translation: under the tables, else (since 4.11.,
           ;; with a proposal) the first paragraph of Translations compared
           (own (and (car compared)
                     (string-match "\\`[ \t\n]*\\*Translation:\\* \\(.*\\)" (car compared))
                     (match-string 1 (car compared)))))
      (append
       (delq nil
             (list (funcall field 'grammar (or grammar (bcat-relayout--section sentence 2 "Grammar")))
                   (funcall field 'proposal proposal)
                   (funcall field 'comparison (bcat-analysis-footnotes-last (car compared)))
                   (funcall field 'rendering (or (bcat-relayout--section text 2 "Tibetan rendering")))
                   (funcall field 'word-analysis
                            (bcat-relayout--section (or (bcat-relayout--section text 1 "Segments") "")
                                                    2 "Word Analysis"))))
       (nreverse out)
       (and own (not (assq 'translation out)) (list (cons 'translation own)))))))

(defun bcat-relayout--pair-fields (template segments assignment other other-text base)
  "The two parts of a bilingual file of TEMPLATE (Carsten 1.11.,
`bcat-analysis-pair-lists') set anew: SEGMENTS its own, merged; the
other language's segments from BASE (its rump: `tibetan' or `sanskrit')
completed from OTHER — the fields read back from the partner file
OTHER-TEXT (the Tibetan tables with the Uchen row) — and the partner's
sentence translation.  ASSIGNMENT from the segment headings."
  (let* ((sanskrit-file (equal template "bilingual"))
         (others (mapcar (lambda (o)
                           (let* ((f (gethash (bcat-analysis--num o) other))
                                  (table (alist-get 'gloss-table f)))
                             (append
                              (and table
                                   ;; 4.11./5.11.: compact, as in the partner file.
                                   `((gloss-table . ,(car (bcat-analysis-compact-tables
                                                           (if sanskrit-file
                                                               (bcat-analysis-uchen-table table (alist-get 'uchen o))
                                                             table))))))
                              (and (alist-get 'segment-translation f)
                                   `((segment-translation . ,(alist-get 'segment-translation f))))
                              (and (alist-get 'differences f)
                                   `((differences . ,(alist-get 'differences f))))
                              o)))
                         (alist-get (if sanskrit-file 'tibetan 'sanskrit) base)))
         (sanskrit (if sanskrit-file segments others))
         (tibetan (if sanskrit-file others segments))
         (diffs (mapcar (lambda (s) (cons (bcat-analysis--num s) (or (alist-get 'differences s) "")))
                        sanskrit))
         (lists (bcat-analysis-pair-lists sanskrit tibetan assignment diffs))
         (translation (alist-get 'translation (bcat-relayout--sentence-fields other-text base))))
    (if sanskrit-file
        `((segments . ,(nth 0 lists))
          (tibetan-segments . ,(nth 1 lists))
          (tibetan-lines . ,(nth 2 lists))
          (tibetan-translation . ,(or translation "")))
      `((segments . ,(nth 1 lists))
        (sanskrit-segments . ,(nth 0 lists))
        (sanskrit-translation . ,(or translation ""))))))

(defun bcat-relayout-context (base text template &optional other-text)
  "The template fields of the sentence file TEXT (template TEMPLATE):
BASE — its rump, what C-c u A knows — completed from what the file
holds, read back in the old order or the new one.  Segments by number;
the assignment of the other language from the segment headings.  For a
bilingual pair OTHER-TEXT is the partner file: the other language's
segments per segment come from there (Carsten 1.11.)."
  (let* ((block-field (if (equal template "bilingual-tibetan") 'sanskrit-block 'tibetan-block))
         (read (bcat-relayout--segments text block-field))
         (assignment (bcat-relayout--assignment text))
         (other (and other-text
                     (member template '("bilingual" "bilingual-tibetan"))
                     (bcat-relayout--segments other-text (if (equal template "bilingual")
                                                             'sanskrit-block 'tibetan-block))))
         (segments (mapcar (lambda (seg)
                             (let ((fields (gethash (bcat-analysis--num seg) read))
                                   (uchen (alist-get 'uchen seg)))
                               (when (alist-get 'gloss-table fields)
                                 ;; Tables written before 31.10. abends get
                                 ;; the Uchen row (Tibetisch V).
                                 (when uchen
                                   (setf (alist-get 'gloss-table fields)
                                         (bcat-analysis-uchen-table (alist-get 'gloss-table fields) uchen)))
                                 ;; 4.11.: compact tables — a long function
                                 ;; cut, its rest a line of Structure; 5.11.
                                 ;; every language (Sanskrit: the form, wide
                                 ;; cells broken at their members).
                                 (let ((compact (bcat-analysis-compact-tables
                                                 (alist-get 'gloss-table fields))))
                                   (setf (alist-get 'gloss-table fields) (car compact))
                                   (when (cdr compact)
                                     (setf (alist-get 'structure fields)
                                           (bcat-analysis--with-overflow (alist-get 'structure fields)
                                                                         (cdr compact))))))
                               (append fields seg)))
                           (alist-get 'segments base)))
         (pair (and other (bcat-relayout--pair-fields template segments assignment
                                                       other other-text base))))
    (append (or pair `((segments . ,segments)))
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
             ;; A bilingual pair: the other language from the partner.
             (partner (and (member template '("bilingual" "bilingual-tibetan"))
                           (bcat-sentence-partner-file sent-num source)))
             (context (bcat-relayout-context base text template
                                             (and partner (file-readable-p partner)
                                                  (bcat-sentence--text partner))))
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
