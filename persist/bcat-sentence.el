;;; bcat-sentence.el --- Template-built sentence files (Buddhist CAT) -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;;; Commentary:
;; §5.74 (Carsten 28.10.2026): sentence files in the structure whole
;; original / segments / sentence, rendered from a template
;; (`bcat-template').  Step 2: C-c u A creates them WITHOUT any AI
;; request — Wylie, dictionary gloss tables, the reference translations
;; at sentence level.  Everything else arrives with C-c u G (step 3);
;; the old renderers never touch such a file (guards in
;; tibetan-cascade.el, keyed on `bcat-sentence-file-p').
;;
;; Scope for now: Tibetan cascade sources (template "tibetan").
;; Sanskrit and bilingual follow with their own templates.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'bcat-template)

(declare-function tibetan-analysis--cascade-p "tibetan-analysis-claude" (file))
(declare-function tibetan-analysis--resolve-source-lang "tibetan-analysis-claude" (file))
(declare-function tibetan-analysis--read-source-metadata "tibetan-analysis-claude" (source-file))
(declare-function tibetan-to-wylie-fixed "tibetan-wylie" (tibetan-text))
(declare-function tibetan-gloss-table--unit-rows "tibetan-gloss-table" (unit-text &optional vocab-alist))
(declare-function tibetan-gloss-table--format-rows "tibetan-gloss-table" (rows))
(declare-function tibetan-sentence--compute-hash "tibetan-sentence-persist" (concatenated-text))
(declare-function tibetan-cascade--sentence-lopez-section "tibetan-cascade" (source-file sent-num))
(declare-function tibetan-cascade--refs-file "tibetan-cascade" (source-file key))
(declare-function tibetan-cascade--section-ref-bodies "tibetan-cascade" (refs-file secnum))
(declare-function tibetan-cascade--sentence-ref-bodies "tibetan-cascade" (source-file sent-num))

(defvar tibetan-analysis--target-lang)
(defvar tibetan-analysis--source-lang)

(defcustom bcat-sentence-templates t
  "When non-nil, C-c u A creates new sentence files from the template
\(§5.74) — for the languages that have one (Tibetan so far).  nil
keeps the old layout; the test suite switches it off for the tests of
the old pipeline."
  :type 'boolean
  :group 'buddhist-cat)

(defun bcat-sentence-template-name (source-file)
  "The template a new sentence file of SOURCE-FILE is built from, or
nil for the old layout: \"tibetan\" for Tibetan cascade sources while
`bcat-sentence-templates' is on."
  (when (and bcat-sentence-templates source-file
             (fboundp 'tibetan-analysis--cascade-p)
             (tibetan-analysis--cascade-p source-file)
             (not (equal (tibetan-analysis--resolve-source-lang source-file) "sa")))
    "tibetan"))

(defun bcat-sentence-file-p (file)
  "The template FILE was built from (its #+CAT_TEMPLATE:), or nil for a
file in the old layout."
  (when (and file (file-readable-p file))
    (with-temp-buffer
      (insert-file-contents file nil 0 4096)
      (goto-char (point-min))
      (when (re-search-forward "^#\\+CAT_TEMPLATE:[ \t]*\\([^ \t\n]+\\)" nil t)
        (match-string 1)))))

;; ----------------------------------------------------------------------------
;; Field values without AI (C-c u A)
;; ----------------------------------------------------------------------------

(defmacro bcat-sentence--with-source (source-file &rest body)
  "Run BODY with the dictionary lookups bound to SOURCE-FILE: its folder
\(the Resources wordlist is found relative to it — headless lesson
2026-06-03), its target and source language."
  (declare (indent 1))
  `(let* ((default-directory (file-name-directory (expand-file-name ,source-file)))
          (tibetan-analysis--target-lang
           (or (ignore-errors
                 (plist-get (tibetan-analysis--read-source-metadata ,source-file)
                            :target-lang))
               (and (boundp 'tibetan-analysis--target-lang)
                    tibetan-analysis--target-lang)))
          (tibetan-analysis--source-lang
           (ignore-errors (tibetan-analysis--resolve-source-lang ,source-file))))
     ,@body))

(defun bcat-sentence--wylie (text)
  "TEXT (Tibetan Unicode) as one Wylie line."
  (string-trim (tibetan-to-wylie-fixed (string-trim text))))

(defun bcat-sentence--dictionary-table (text)
  "The three-row gloss table of the segment TEXT from the dictionaries
\(row 1 Wylie with Steinert link, row 2 gloss, row 3 label; \"?\" where
nothing is known — C-c u G replaces it).  \"\" when nothing renders."
  (let ((rows (ignore-errors (tibetan-gloss-table--unit-rows text nil))))
    (if rows
        (string-trim-right (tibetan-gloss-table--format-rows rows))
      "")))

(defun bcat-sentence--references (sent-num source-file)
  "The reference translations of sentence SENT-NUM: ((NAME . BODY) …)
from the §-references of its Section and the sentence references —
never Carsten's own „Übersetzung …“ rubric (as in the materializer)."
  (let* ((secnum (ignore-errors (tibetan-cascade--sentence-lopez-section source-file sent-num)))
         (sec-file (and secnum (tibetan-cascade--refs-file source-file :section-refs)))
         (bodies (append (and sec-file (tibetan-cascade--section-ref-bodies sec-file secnum))
                         (ignore-errors (tibetan-cascade--sentence-ref-bodies source-file sent-num)))))
    (cl-remove-if (lambda (nb) (string-prefix-p "Übersetzung" (car nb))) bodies)))

(defun bcat-sentence--comparison (refs)
  "REFS ((NAME . BODY) …) as the paragraphs of Translations compared."
  (mapconcat (lambda (nb) (format "*%s:* %s" (car nb) (string-trim (cdr nb))))
             refs "\n\n"))

(defun bcat-sentence-rump-context (sent-num segs source-file)
  "Template fields of the new sentence file SENT-NUM (SEGS = its
\((GLOBAL-NUM . TEXT) …) from SOURCE-FILE), all without AI."
  (bcat-sentence--with-source source-file
    (let* ((name (file-name-nondirectory source-file))
           (date (format-time-string "%Y-%m-%d")))
      `((sent-num . ,sent-num)
        (source-link . ,(format "[[file:../%s::*Sentence %d][%s / Sentence %d]]"
                                name sent-num name sent-num))
        (segment-list . ,(mapconcat (lambda (s) (number-to-string (car s))) segs ", "))
        (hash . ,(ignore-errors (tibetan-sentence--compute-hash (mapconcat #'cdr segs ""))))
        (created . ,date)
        (last-analyzed . ,date)
        (segments . ,(mapcar (lambda (s)
                               `((num . ,(car s))
                                 (wylie . ,(bcat-sentence--wylie (cdr s)))
                                 (gloss-table . ,(bcat-sentence--dictionary-table (cdr s)))))
                             segs))
        (comparison . ,(bcat-sentence--comparison
                        (bcat-sentence--references sent-num source-file)))))))

(defun bcat-sentence-create-file (sent-num segs source-file filepath)
  "Write the template-built sentence file SENT-NUM to FILEPATH (C-c u A,
no AI) and return FILEPATH."
  (let ((text (bcat-template-render-named
               (bcat-sentence-template-name source-file)
               (bcat-sentence-rump-context sent-num segs source-file)
               source-file)))
    (make-directory (file-name-directory filepath) t)
    (with-temp-file filepath (insert text))
    filepath))

(provide 'bcat-sentence)
;;; bcat-sentence.el ends here
