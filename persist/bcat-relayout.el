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

(defconst bcat-relayout--proposal-words
  "\\(?:Proposed translation\\|Übersetzungsvorschlag\\)"
  "The words of the proposal, English and German
\(`bcat-analysis--comparison-labels').")

(defconst bcat-relayout--wrappers '("Tibetan" "Sanskrit" "Commentary")
  "The headings under * Philology whose content stands one level down:
the other language of a pair and the commentary — before 18.11.
sections of the first level.")

(defun bcat-relayout--legacy-text (text template)
  "TEXT of a sentence file of TEMPLATE as the readers of this file know
it — the sections of the first level as they were until 18.11.:
* Recitation (Sanskrit), * Text with the segments, * Tibetan or
* Sanskrit, * Sentence, * Segments, * Commentary.  Since 18.11. a file
has three parts (Carsten: Content, Philologie, „Worum es geht“): * Text
is the original with, as its last part, the translation of the sentence
\(the proposed one, else the own); * Philology holds the rest with its
headings as they were, the other language and the commentary under a
heading of their own, their content one level down.  A file in that
form is taken apart again: the original becomes * Recitation (the
Sanskrit file: its lines are read back) or * Original (no reader asks
for it); * Philology becomes * Text — the translation that closed the
original first in it —, with * Sentence before its Concepts, * Segments
before its first Segment N, and each of `bcat-relayout--wrappers' a
section of the first level, its headings one level up.  TEXT without
* Philology comes back as it is."
  (if (not (and text (string-match-p "^\\* Philology[ \t]*$" text)))
      text
    (let ((part nil) (wrapper nil) (sentence nil) (segments nil) (out '())
          (translation '()) (moving nil))
      (dolist (line (split-string text "\n"))
        (cond
         ;; a section of the first level
         ((string-match "\\`\\* \\(.*?\\)[ \t]*\\'" line)
          (let ((title (match-string 1 line)))
            (setq wrapper nil moving nil)
            (cond ((equal title "Philology")
                   (setq part 'philology)
                   (push "* Text" out)
                   ;; the translation that closed the original
                   (dolist (l (nreverse translation)) (push l out))
                   (setq translation nil))
                  ((and (equal title "Text") (null part))
                   (setq part 'original)
                   (push (if (equal template "sanskrit") "* Recitation" "* Original") out))
                  (t (setq part 'other)
                     (push line out)))))
         ;; the original: from the label of the translation on, its last part
         ((eq part 'original)
          (when (and (not moving)
                     (string-match-p (concat "\\`\\*\\(?:" bcat-relayout--proposal-words "\\|Translation\\)"
                                             "\\(?: — [^*\n]*\\)?:\\*")
                                     line))
            (setq moving t))
          (if moving (push line translation) (push line out)))
         ((not (eq part 'philology)) (push line out))
         ;; under * Philology: a heading of the second level
         ((string-match "\\`\\*\\* \\(.*?\\)[ \t]*\\'" line)
          (let ((title (match-string 1 line)))
            (cond ((member title bcat-relayout--wrappers)
                   (setq wrapper t)
                   (push (concat "* " title) out))
                  (t
                   (setq wrapper nil)
                   (when (and (not sentence) (not segments)
                              (member title '("Concepts" "Tibetan rendering" "Translations compared")))
                     (setq sentence t)
                     (push "* Sentence" out))
                   (when (and (not segments)
                              (string-match-p "\\`\\(?:Segment [0-9]\\|Word Analysis\\'\\)" title))
                     (setq segments t)
                     (push "* Segments" out))
                   (push line out)))))
         ;; under the other language or the commentary: one level up
         ((and wrapper (string-match-p "\\`\\*\\*\\*+ " line))
          (push (substring line 1) out))
         (t (push line out))))
      (string-join (nreverse out) "\n"))))

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

(defun bcat-relayout--paragraphs (body)
  "The paragraphs of BODY, trimmed."
  (and body (split-string body "\n[ \t]*\n" t "[ \t\n]+")))

(defun bcat-relayout--text-paragraphs (body)
  "The paragraphs of BODY with every heading line a paragraph of its own
and the property drawers left out — since 9.11. (TĀ 1009) a Sanskrit
segment under * Text is a heading ⟦N⟧ with the original right under
it, its parts under a heading Word Formation with a drawer
\(:VISIBILITY: folded), the sentence's translation under ** Translation."
  (and body
       (bcat-relayout--paragraphs
        (replace-regexp-in-string
         "^\\(\\*+ .*\\)$" "\n\\1\n"
         (replace-regexp-in-string
          "^[ \t]*:PROPERTIES:[ \t]*\n\\(?:.*\n\\)*?[ \t]*:END:[ \t]*$" "" body)))))

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
    ;; then „→“ the segment's translation, and (Sanskrit, 5.11.) the
    ;; tables and lines of the words' parts, as they are.  What follows
    ;; in a bilingual file (1.11.: the other language's tables and
    ;; translations) comes from the partner file (`bcat-relayout-context');
    ;; the differences belong to the segment.
    ;; 9.11.: ⟦N⟧ a heading (the bold line under it the original, read
    ;; back as the padapāṭha — a hand correction stays), the parts under
    ;; *** Word Formation, the sentence under ** Translation.
    (let ((current nil) (own nil) (parts nil) (original nil))
      (dolist (p (bcat-relayout--text-paragraphs (bcat-relayout--section text 1 "Text")))
        (let ((after-mark original))
          (setq original nil)
          (cond
           ((string-match "\\`\\(?:\\*\\* \\)?\\(?:<<[^>\n]*>>\\)?⟦\\([0-9]+\\)⟧[^\n]*\\'" p)
            (setq current (string-to-number (match-string 1 p)) own t parts nil original t))
           ((and current after-mark (string-match "\\`\\*\\([^*\n]+\\)\\*\\'" p))
            (bcat-relayout--put table current 'padapatha (match-string 1 p)))
           ((and current (string-match-p "\\`\\*\\*\\* Word Formation[ \t]*\\'" p))
            (setq own nil parts t))
           ((string-match-p "\\`\\*+ " p)
            (setq current nil parts nil))
           ((string-match "\\`\\(?:<<[^>\n]*>>\\)?⟦\\([0-9]+\\)⟧[^\n]*\n" p)
            (setq current (string-to-number (match-string 1 p)) own t parts nil)
            (bcat-relayout--put table current 'gloss-table (substring p (match-end 0))))
           ((and current (string-prefix-p "|" p))
            (cond (own (bcat-relayout--put table current 'gloss-table p t))
                  (parts (bcat-relayout--put table current 'formation-tables p t))))
           ((and current parts (bcat-analysis-parts-line-table p))
            ;; 9.11.: the line of a word of one member (draft B) becomes
            ;; its table of one column, the parts as the line has them.
            (bcat-relayout--put table current 'formation-tables (bcat-analysis-parts-line-table p) t))
           ((and current (string-match "\\`→ " p))
            (when own
              (bcat-relayout--put table current 'segment-translation (substring p (match-end 0)))
              (setq own nil parts t)))
           ((and current (string-match "\\`\\*Differences:\\* " p))
            (bcat-relayout--put table current 'differences (substring p (match-end 0))))
           ;; 18.11.: the line that marks a point for the discussion — as
           ;; the file has it (a hand's wording stays).
           ((and current (string-match-p "\\`\\*\\(?:Discuss\\|Zu besprechen\\):\\* " p))
            (bcat-relayout--put table current 'discuss p t))
           ;; 16.11.: the check of the padapāṭha — computed anew, no end
           ;; of the segment.
           ((and current (string-match-p "\\`\\*Prüfen:\\* " p)))
           (t (setq current nil parts nil))))))
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
    ;; 9.11.: the Sanskrit segments of a Tibetan file (* Sanskrit) are
    ;; headings ⟦N⟧ too; any other heading ends the part, as before.
    (let ((current nil) (done nil))
      (dolist (p (bcat-relayout--text-paragraphs
                  (or (bcat-relayout--section text 1 "\\(?:Tibetan\\|Sanskrit\\)") "")))
        (cond
         (done nil)
         ((string-match "\\`\\*\\* \\(?:<<[^>\n]*>>\\)?⟦" p)
          (string-match "⟦\\([0-9]+\\)⟧\\([^\n]*\\)" p)
          (let ((mark (match-string 2 p)) (n (string-to-number (match-string 1 p))))
            (setq current (if (string-match "Skt\\. \\([0-9]+\\)" mark)
                              (string-to-number (match-string 1 mark))
                            n))))
         ((string-match-p "\\`\\*\\*\\*+ " p))
         ((string-match-p "\\`\\*\\* " p) (setq done t))
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
    (dolist (p (bcat-relayout--text-paragraphs (bcat-relayout--section text 1 "Text")))
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
    ;; 5.11.: Vasubandhu's translation, under ** Bhāṣya only.
    (let ((bhasya (bcat-relayout--section (or (bcat-relayout--section text 1 "Commentary") "") 2 "Bhāṣya")))
      (when (and bhasya (string-match "^\\*Translation:\\* \\(.*\\(?:\n.+\\)*\\)" bhasya))
        (push (cons 'bhasya-translation (match-string 1 bhasya)) out)))
    (when (string-match "^#\\+LAST_ANALYZED:[ \t]*\\(.+\\)$" text)
      (push (cons 'last-analyzed (string-trim (match-string 1 text))) out))
    (unless (alist-get 'verse base)
      (let ((verse (bcat-relayout--verse-body
                    (car (last (bcat-relayout--paragraphs
                                (bcat-relayout--section text 1 "Recitation")))))))
        (when verse
          ;; 16.11.: a verse quoted in the prose gets its metrical lines
          ;; (the IAST lines; the Devanāgarī follows from them).
          (when (alist-get 'quotes base)
            (setq verse (mapconcat (lambda (line)
                                     (concat (tibetan-sanskrit-script-iast-to-devanagari line) "\n" line))
                                   (bcat-analysis-quote-lines
                                    (cl-remove-if (lambda (l) (or (string-blank-p l)
                                                                  (string-match-p "[ऀ-ॿ]" l)))
                                                  (split-string verse "\n"))
                                    (alist-get 'quotes base))
                                   "\n")))
          (push (cons 'recitation verse) out))))
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
                   ;; 18.11.: a machine translation among the translations
                   ;; goes last, under its heading
                   (funcall field 'comparison
                            (bcat-relayout--machine-last
                             (bcat-analysis-footnotes-last (car compared))
                             (or bcat-analysis-comparison-language (alist-get 'target-lang base) "en")))
                   (funcall field 'rendering (or (bcat-relayout--section text 2 "Tibetan rendering")))
                   ;; 19.11.: without the drawer that folds it — the
                   ;; template sets it
                   (funcall field 'word-analysis
                            (let ((wa (bcat-relayout--section (or (bcat-relayout--section text 1 "Segments") "")
                                                              2 "Word Analysis")))
                              (and wa (replace-regexp-in-string
                                       "\\`[ \t\n]*:PROPERTIES:[ \t]*\n\\(?:.*\n\\)*?[ \t]*:END:[ \t]*\n?" "" wa))))))
       (nreverse out)
       (and own (not (assq 'translation out)) (list (cons 'translation own)))))))

(defun bcat-relayout--with-padapatha (fields seg)
  "FIELDS read back for the segment SEG (its rump), with the padapāṭha
line from the overview when the file does not show the original yet
\(`bcat-analysis-table-padapatha'; 9.11.) — what the file shows wins."
  (let ((line (and (not (alist-get 'padapatha fields))
                   (bcat-analysis-table-padapatha (alist-get 'samhita seg)
                                                  (alist-get 'gloss-table fields)))))
    (if line (cons (cons 'padapatha line) fields) fields)))

(defun bcat-relayout--pair-fields (template segments assignment other other-text base)
  "The two parts of a bilingual file of TEMPLATE (Carsten 1.11.,
`bcat-analysis-pair-lists') set anew: SEGMENTS its own, merged; the
other language's segments from BASE (its rump: `tibetan' or `sanskrit')
completed from OTHER — the fields read back from the partner file
OTHER-TEXT (the Tibetan tables with the Uchen row) — and the partner's
sentence translation.  ASSIGNMENT from the segment headings."
  (let* ((sanskrit-file (equal template "bilingual"))
         (others (mapcar (lambda (o)
                           (let* ((f (bcat-relayout--with-padapatha (gethash (bcat-analysis--num o) other) o))
                                  (table (alist-get 'gloss-table f))
                                  ;; 5.11.: Sanskrit tables of five rows →
                                  ;; overview and parts tables.
                                  (split (and table (bcat-analysis-split-word-tables table)))
                                  (table (if split (nth 0 split) table))
                                  ;; 18.11.: the other language is Sanskrit (this
                                  ;; file the Tibetan one) — the parts of the
                                  ;; words under them in the table, also what a
                                  ;; file written before holds under Word Formation
                                  (new (and table (not sanskrit-file)
                                            (bcat-analysis-sanskrit-relayout-tables
                                             table (alist-get 'formation-tables f))))
                                  (formation (if new (nth 1 new) (alist-get 'formation-tables f))))
                             (append
                              (and table
                                   ;; 4.11./5.11.: compact, as in the partner file.
                                   `((gloss-table . ,(if new
                                                         (nth 0 new)
                                                       (car (bcat-analysis-compact-tables
                                                             (bcat-analysis-uchen-table table (alist-get 'uchen o))))))))
                              (and formation (not (string-empty-p formation))
                                   `((formation-tables . ,formation)))
                              (and (alist-get 'segment-translation f)
                                   `((segment-translation . ,(alist-get 'segment-translation f))))
                              ;; 9.11.: the original in bold under ⟦N⟧
                              (and (alist-get 'padapatha f)
                                   `((padapatha . ,(alist-get 'padapatha f))))
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

(defconst bcat-relayout--rated-heading
  "^\\*\\*\\* \\(?:Overview\\|Übersicht\\)[ \t]*\n\\(?:Sigla\\|Sigel\\): "
  "How the rated comparison of 2.11.–18.11. begins under Translations
compared: the overview with the sigla.")

(defun bcat-relayout--squeeze (text)
  "TEXT with every run of blank space a single space — to compare two
texts whatever their line breaks."
  (string-trim (replace-regexp-in-string "[ \t\n]+" " " (or text ""))))

(defun bcat-relayout--rated-key (text)
  "TEXT, a rated comparison of 2.11.–18.11., without what the versions
of its renderer wrote differently — so that one written by an older
version is known as the answer's, a hand's change is not: the line of
the sigla (it follows the references), the rules of the table and the
padding of its cells, „because because“ / „weil weil“ (the conjunction
doubled until 4.11.).  Squeezed (`bcat-relayout--squeeze')."
  (bcat-relayout--squeeze
   (replace-regexp-in-string
    "\\_<\\(because\\|weil\\) \\1\\_>" "\\1"
    (replace-regexp-in-string
     "^\\(?:Sigla\\|Sigel\\): .*$" ""
     (replace-regexp-in-string
      "[ \t]*|[ \t]*" " | "
      (replace-regexp-in-string "^[ \t]*|[-+|]*[ \t]*$" "" (or text "")))))))

(defun bcat-relayout--comparison-parts (comparison)
  "(TOP MIDDLE MACHINES) of COMPARISON, the text under Translations
compared: TOP the translations, before the first heading; MACHINES from
the heading of the first machine translation — a heading right above a
paragraph labelled with one of `bcat-analysis-machine-references' — to
the end; MIDDLE what stands between: the points for the discussion, or
the rated part of 2.11.–18.11.  Each trimmed, \"\" when missing."
  (let* ((cut (or (string-match "^\\*\\*\\* " comparison) (length comparison)))
         (mcut (let ((pos cut) (found nil))
                 (while (and (not found)
                             (string-match "^\\*\\*\\* [^\n]*\n\\*\\([^*\n]+?\\):\\* " comparison pos))
                   (if (member (match-string 1 comparison) bcat-analysis-machine-references)
                       (setq found (match-beginning 0))
                     (setq pos (match-end 0))))
                 (or found (length comparison)))))
    (list (string-trim (substring comparison 0 cut))
          (string-trim (substring comparison cut mcut))
          (string-trim (substring comparison mcut)))))

(defun bcat-relayout--machine-last (comparison lang)
  "COMPARISON, the text under Translations compared, with the machine
translations among its translations put last, each under its heading
\(Carsten 18.11.: DharmaMitra „nur der Vollständigkeit halber“) — a
machine's paragraphs run from its label to the next labelled paragraph;
footnote definitions stay with the translations (as does, in a file of
before 2.11., the paragraph that compares them).  The heading in LANG.
COMPARISON itself when its translations have none: a file in the new
form, a rump."
  (if (or (not (stringp comparison)) (string-blank-p comparison))
      comparison
    (pcase-let ((`(,top ,middle ,machines) (bcat-relayout--comparison-parts comparison)))
      (let ((bcat-analysis--comparison-lang (or lang "en"))
            ;; A machine's text may run over several paragraphs (TĀ 1012:
            ;; prose around a verse).  Before 2.11. the comparison itself
            ;; was a paragraph after the translations — there only the
            ;; labelled paragraph is the machine's; since then (headings
            ;; below) what follows its label up to the next label is.
            (runs-on (not (and (string-empty-p middle) (string-empty-p machines))))
            (keep '()) (moved '()) (current nil))
        (dolist (p (bcat-relayout--paragraphs top))
          (cond
           ((string-match "\\`\\*\\([^*\n]+?\\):\\* " p)
            (if (member (match-string 1 p) bcat-analysis-machine-references)
                (progn (setq current (list p (match-string 1 p)))
                       (push current moved))
              (setq current nil)
              (push p keep)))
           ((and current runs-on (not (string-prefix-p "[fn:" p)))
            (setcar current (concat (car current) "\n\n" p)))
           (t (setq current nil) (push p keep))))
        (if (null moved)
            comparison
          (string-join
           (delq nil
                 (append
                  (and keep (list (string-join (nreverse keep) "\n\n")))
                  (and (not (string-empty-p middle)) (list middle))
                  (and (not (string-empty-p machines)) (list machines))
                  (mapcar (lambda (m)
                            (concat "*** " (format (bcat-analysis--comparison-label 'machine) (cadr m))
                                    "\n" (car m)))
                          (nreverse moved))))
           "\n\n"))))))

(defun bcat-relayout--pointed (context record)
  "CONTEXT (read back from a sentence file) with its Translations
compared on the point (Carsten 18.11.: only what is worth discussing,
no ratings) where the file still has the rated form of 2.11.–18.11.: the
points set anew from RECORD, the answer kept at the fetch
\(`bcat-sentence--answers', `bcat-analysis-comparison-units') — the
translations stay as the file shows them, the machine's last
\(`bcat-relayout--machine-last').  Only when the rated part is the one
the answer gave, word for word whatever the version of its renderer
\(`bcat-relayout--rated-key'): one changed by hand stays as it is, as
does one without an answer — and then no segment keeps a „Discuss:“
line from the answer (its link would lead nowhere).  Any other
comparison — the new form, a paragraph of before 2.11., none — comes
back untouched."
  (let* ((target (alist-get 'target-lang context))
         (lang (or bcat-analysis-comparison-language target "en"))
         (comparison (bcat-relayout--machine-last (alist-get 'comparison context) lang))
         (parts (and (stringp comparison) (bcat-relayout--comparison-parts comparison)))
         (middle (nth 1 parts)))
    (if (not (and middle (eq 0 (string-match bcat-relayout--rated-heading middle))))
        (if (equal comparison (alist-get 'comparison context))
            context
          (cons (cons 'comparison comparison) context))
      (let* ((parsed (plist-get record :parsed))
             (refs (plist-get record :refs))
             (untouched (and parsed
                             (equal (bcat-relayout--rated-key middle)
                                    (bcat-relayout--rated-key
                                     (bcat-analysis-comparison-units-rated parsed refs target))))))
        (if (not untouched)
            (append `((comparison . ,comparison)
                      (segments . ,(mapcar (lambda (seg) (assq-delete-all 'discuss (copy-alist seg)))
                                           (alist-get 'segments context))))
                    context)
          (let ((units (bcat-analysis-comparison-units parsed refs target context)))
            (cons (cons 'comparison
                        (string-join
                         (cl-remove-if #'string-empty-p (list (nth 0 parts) units (nth 2 parts)))
                         "\n\n"))
                  context)))))))

(defun bcat-relayout-context (base text template &optional other-text)
  "The template fields of the sentence file TEXT (template TEMPLATE):
BASE — its rump, what C-c u A knows — completed from what the file
holds, read back in the old order or the new one.  Segments by number;
the assignment of the other language from the segment headings.  For a
bilingual pair OTHER-TEXT is the partner file: the other language's
segments per segment come from there (Carsten 1.11.).  TEXT and
OTHER-TEXT in the form of three parts (18.11.) or in an earlier one
\(`bcat-relayout--legacy-text')."
  (let* (;; 18.11.: a file of three parts read as before
         (text (bcat-relayout--legacy-text text template))
         (other-text (bcat-relayout--legacy-text
                      other-text (if (equal template "bilingual") "bilingual-tibetan" "bilingual")))
         (block-field (if (equal template "bilingual-tibetan") 'sanskrit-block 'tibetan-block))
         (read (bcat-relayout--segments text block-field))
         (assignment (bcat-relayout--assignment text))
         (other (and other-text
                     (member template '("bilingual" "bilingual-tibetan"))
                     (bcat-relayout--segments other-text (if (equal template "bilingual")
                                                             'sanskrit-block 'tibetan-block))))
         (segments (mapcar (lambda (seg)
                             (let ((fields (bcat-relayout--with-padapatha
                                            (gethash (bcat-analysis--num seg) read) seg))
                                   (uchen (alist-get 'uchen seg)))
                               (when (alist-get 'gloss-table fields)
                                 ;; Tables written before 31.10. abends get
                                 ;; the Uchen row (Tibetisch V).
                                 (when uchen
                                   (setf (alist-get 'gloss-table fields)
                                         (bcat-analysis-uchen-table (alist-get 'gloss-table fields) uchen)))
                                 ;; 5.11.: Sanskrit tables of five rows →
                                 ;; the overview, the parts tables (under
                                 ;; the translation), a cut form's rest a
                                 ;; line of Structure.
                                 (let ((split (bcat-analysis-split-word-tables (alist-get 'gloss-table fields))))
                                   (when split
                                     (setf (alist-get 'gloss-table fields) (nth 0 split))
                                     (unless (alist-get 'formation-tables fields)
                                       (setf (alist-get 'formation-tables fields) (nth 1 split)))
                                     (when (nth 2 split)
                                       (setf (alist-get 'structure fields)
                                             (bcat-analysis--with-overflow (alist-get 'structure fields)
                                                                           (nth 2 split))))))
                                 ;; 4.11.: compact tables — a long function
                                 ;; cut, its rest a line of Structure; 5.11.
                                 ;; every language (Sanskrit: the form, wide
                                 ;; cells broken at their members).  18.11.:
                                 ;; a Sanskrit segment has the parts of its
                                 ;; words under them in the table — also what
                                 ;; a file written before holds under Word
                                 ;; Formation; what belongs to no word stays there.
                                 (if (member template '("sanskrit" "bilingual"))
                                     (let ((new (bcat-analysis-sanskrit-relayout-tables
                                                 (alist-get 'gloss-table fields)
                                                 (alist-get 'formation-tables fields))))
                                       (setf (alist-get 'gloss-table fields) (nth 0 new))
                                       (setf (alist-get 'formation-tables fields) (nth 1 new))
                                       (when (nth 2 new)
                                         (setf (alist-get 'structure fields)
                                               (bcat-analysis--with-overflow (alist-get 'structure fields)
                                                                             (nth 2 new)))))
                                   (let ((compact (bcat-analysis-compact-tables
                                                   (alist-get 'gloss-table fields))))
                                     (setf (alist-get 'gloss-table fields) (car compact))
                                     (when (cdr compact)
                                       (setf (alist-get 'structure fields)
                                             (bcat-analysis--with-overflow (alist-get 'structure fields)
                                                                           (cdr compact)))))))
                               (append fields seg)))
                           (alist-get 'segments base)))
         (pair (and other (bcat-relayout--pair-fields template segments assignment
                                                       other other-text base))))
    (let ((fields (append (or pair `((segments . ,segments)))
                          (and assignment `((assignment . ,assignment)))
                          (bcat-relayout--sentence-fields text base)
                          base)))
      ;; 18.11.: the translation closes * Text, the decisions stay in
      ;; * Philology — from what the file shows (a hand correction of the
      ;; proposal stays)
      (append (bcat-analysis-reading-fields (alist-get 'proposal fields) (alist-get 'translation fields))
              fields))))

;; ----------------------------------------------------------------------------
;; C-c u L
;; ----------------------------------------------------------------------------

(defun bcat-relayout-file-context (file)
  "What the template sentence FILE is set anew from, without a fetch
\(`bcat-relayout-context'): (:template :source :context) — user slots,
#+CREATED and #+LAST_ANALYZED included; the answer kept at the fetch
\(`bcat-sentence--answers') fills what the file does not show.  C-c u L
renders the file from it, C-c u H the PDF (Carsten 5.11.).  When FILE is
no sentence file of a template, or its source has another template now
\(C-c u G rebuilds it): (:error TEXT), nothing read."
  (let* ((source (tibetan-sentence--source-file-from-analysis file))
         (template (bcat-sentence-file-p file))
         (sent-num (tibetan-sentence--sent-id-from-filename file)))
    (cond
     ((not (and source template sent-num))
      (list :error (format "%s ist keine Satzdatei einer Vorlage" (file-name-nondirectory file))))
     ((not (equal template (bcat-sentence-template-name source)))
      (list :error (format "%s hat die Vorlage %s, die Quelle %s — C-c u G baut sie neu"
                           (file-name-nondirectory file) template (bcat-sentence-template-name source))))
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
             ;; 18.11.: the comparison on the point, from the answer kept
             (context (bcat-relayout--pointed context record))
             (assignment (alist-get 'assignment context))
             (context (if assignment
                          (bcat-sentence--commentary-fields sent-num source context assignment)
                        context)))
        (list :template template :source source :context context))))))

(defun bcat-relayout-file (file)
  "Set the template sentence FILE anew from its template, without a
fetch (`bcat-relayout-file-context'): user slots, the file's own
sections, #+CREATED and #+LAST_ANALYZED stay.  Returns FILE, or nil
\(with a message) when its source has another template now — C-c u G
rebuilds it."
  (let ((read (bcat-relayout-file-context file)))
    (if (plist-get read :error)
        (progn (message "C-c u L: %s" (plist-get read :error)) nil)
      (bcat-sentence--write file (bcat-sentence--render (plist-get read :template)
                                                        (plist-get read :context)
                                                        (plist-get read :source)))
      file)))

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
