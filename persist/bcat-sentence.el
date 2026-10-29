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
(require 'bcat-analysis)

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
\(§5.74) — for the languages that have one (Tibetan, Sanskrit).  nil
keeps the old layout; the test suite switches it off for the tests of
the old pipeline."
  :type 'boolean
  :group 'buddhist-cat)

(defun bcat-sentence--template-line (file)
  "The value of FILE's #+CAT_TEMPLATE: line, or nil."
  (when (and file (file-readable-p file))
    (with-temp-buffer
      (insert-file-contents file nil 0 4096)
      (goto-char (point-min))
      (when (re-search-forward "^#\\+CAT_TEMPLATE:[ \t]*\\([^ \t\n]+\\)" nil t)
        (match-string 1)))))

(defun bcat-sentence-template-name (source-file)
  "The template new sentence files of SOURCE-FILE are built from, or nil
for the old layout.  A source switches it on explicitly with
#+CAT_TEMPLATE: NAME in its header (Carsten 28.10.2026: the reading
classes; the Masterarbeit sources keep the old layout) — only for
cascade sources and while `bcat-sentence-templates' is on."
  (when (and bcat-sentence-templates source-file
             (fboundp 'tibetan-analysis--cascade-p)
             (tibetan-analysis--cascade-p source-file))
    (bcat-sentence--template-line source-file)))

(defun bcat-sentence--sanskrit-p (source-file)
  "Non-nil when SOURCE-FILE builds its sentence files from the Sanskrit
template (§5.74, 29.10.)."
  (equal "sanskrit" (bcat-sentence-template-name source-file)))

(defun bcat-sentence-file-p (file)
  "The template the sentence FILE (sent-NNN…) was built from (its
#+CAT_TEMPLATE:), or nil for a file in the old layout — and for
anything that is no sentence file (a source carries the line too)."
  (when (and file (string-match-p "\\`sent-[0-9]" (file-name-nondirectory file)))
    (bcat-sentence--template-line file)))

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

(defun bcat-sentence--dictionary-rows (text)
  "The gloss-table rows of the segment TEXT from the dictionaries (row 1
Wylie with Steinert link, row 2 gloss, row 3 label; \"?\" where nothing
is known — C-c u G replaces it), or nil."
  (ignore-errors (tibetan-gloss-table--unit-rows text nil)))

(defun bcat-sentence--dictionary-table (rows)
  "ROWS (`bcat-sentence--dictionary-rows') as an org table; \"\" for none."
  (if rows (string-trim-right (tibetan-gloss-table--format-rows rows)) ""))

(defun bcat-sentence--plain-cell (cell)
  "CELL of a gloss table as plain text: links reduced to their text,
`\\vert' back to a bar."
  (replace-regexp-in-string
   "\\\\vert\\(?:{}\\)?" "|"
   (replace-regexp-in-string "\\[\\[[^]]*\\]\\[\\([^]]*\\)\\]\\]" "\\1" (or cell "") t)
   t t))

(defun bcat-sentence--dictionary-words (rows)
  "ROWS as word alists (wylie, gloss, function) for
`bcat-analysis-glossed-line': row 1, the gloss row (next to last) and
the label row (last)."
  (when (cdr rows)
    (cl-mapcar (lambda (w g l)
                 `((wylie . ,(bcat-sentence--plain-cell w))
                   (gloss . ,(bcat-sentence--plain-cell g))
                   (function . ,(bcat-sentence--plain-cell l))))
               (car rows) (nth (- (length rows) 2) rows) (car (last rows)))))

(defun bcat-sentence--recitation (texts)
  "Recitation fields of a Sanskrit sentence with the segment lines TEXTS
\(Carsten 29.10.: „Es geht ja mit der Rezitation los“): metre (its
header lines with org line breaks), recitation (the verse with its
pauses), scansion (without the pāda letters) — nil for prose."
  (require 'tibetan-sanskrit-metre)
  (when-let ((r (tibetan-metre-recitation texts t)))
    `((metre . ,(mapconcat #'identity (plist-get r :header) " \\\\\n"))
      (recitation . ,(plist-get r :verse))
      (scansion . ,(plist-get r :table)))))

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

(defun bcat-sentence-rump-context (sent-num segs source-file
                                            &optional extra-refs keep)
  "Template fields of the new sentence file SENT-NUM (SEGS = its
\((GLOBAL-NUM . TEXT) …) from SOURCE-FILE), all without AI.
EXTRA-REFS ((NAME . TEXT) …) are references beyond the reference
files (pasted by hand); KEEP an alist of fields carried over from an
existing file (user slots, `created')."
  (bcat-sentence--with-source source-file
    (let* ((name (file-name-nondirectory source-file))
           (date (format-time-string "%Y-%m-%d"))
           (sanskrit (bcat-sentence--sanskrit-p source-file))
           (segments
            (mapcar (lambda (s)
                      (if sanskrit
                          ;; Without AI there is no padapāṭha: the line
                          ;; as transmitted.
                          `((num . ,(car s)) (samhita . ,(string-trim (cdr s))))
                        (let ((wylie (bcat-sentence--wylie (cdr s)))
                              (rows (bcat-sentence--dictionary-rows (cdr s))))
                          `((num . ,(car s))
                            (wylie . ,wylie)
                            (glossed . ,(bcat-analysis-glossed-line
                                         wylie (bcat-sentence--dictionary-words rows)))
                            (gloss-table . ,(bcat-sentence--dictionary-table rows))))))
                    segs)))
      (append
       keep
       (and sanskrit
            (bcat-sentence--recitation (mapcar (lambda (s) (string-trim (cdr s))) segs)))
       `((sent-num . ,sent-num)
         (source-link . ,(format "[[file:../%s::*Sentence %d][%s / Sentence %d]]"
                                 name sent-num name sent-num))
         (segment-list . ,(mapconcat (lambda (s) (number-to-string (car s))) segs ", "))
         (hash . ,(ignore-errors (tibetan-sentence--compute-hash (mapconcat #'cdr segs ""))))
         (created . ,date)
         (last-analyzed . ,date)
         (segments . ,segments)
         (glosses . ,(bcat-analysis-glosses-p segments))
         (comparison . ,(bcat-sentence--comparison
                         (append (bcat-sentence--references sent-num source-file)
                                 extra-refs))))))))

(defun bcat-sentence-create-file (sent-num segs source-file filepath
                                           &optional extra-refs keep)
  "Write the template-built sentence file SENT-NUM to FILEPATH (C-c u A,
no AI) and return FILEPATH.  EXTRA-REFS and KEEP as in
`bcat-sentence-rump-context'."
  (bcat-sentence--write
   filepath
   (bcat-template-render-named
    (bcat-sentence-template-name source-file)
    (bcat-sentence-rump-context sent-num segs source-file extra-refs keep)
    source-file))
  filepath)

;; ----------------------------------------------------------------------------
;; Reading a sentence file (either layout)
;; ----------------------------------------------------------------------------

(defun bcat-sentence--text (file)
  "The contents of FILE (\"\" when missing)."
  (if (file-readable-p file)
      (with-temp-buffer (insert-file-contents file) (buffer-string))
    ""))

(defun bcat-sentence--l1-body (text heading)
  "Trimmed body of `* HEADING' in TEXT (up to the next L1 heading), or
nil when absent or empty."
  (when (string-match (format "^\\* %s\n" (regexp-quote heading)) text)
    (let* ((start (match-end 0))
           (end (if (string-match "^\\* " text start) (match-beginning 0) (length text)))
           (body (string-trim (substring text start end))))
      (unless (string-empty-p body) body))))

(defconst bcat-sentence--user-slots
  '((working-translation . "Working Translation")
    (my-notes . "My Notes")
    (footnotes . "Footnotes"))
  "The user's own sections: never written by the tool, carried over.")

(defun bcat-sentence--keep (file)
  "Fields of FILE that a rebuild carries over: the user slots and the
#+CREATED date."
  (let ((text (bcat-sentence--text file)))
    (append
     (delq nil (mapcar (lambda (slot)
                         (let ((body (bcat-sentence--l1-body text (cdr slot))))
                           (and body (cons (car slot) body))))
                       bcat-sentence--user-slots))
     (and (string-match "^#\\+CREATED:[ \t]*\\(.+\\)$" text)
          (list (cons 'created (string-trim (match-string 1 text))))))))

(defun bcat-sentence--compared (file)
  "The paragraphs under `** Translations compared' of the template FILE
as ((NAME . TEXT) …) — every `*NAME:* TEXT' paragraph, in order."
  (let ((text (bcat-sentence--text file)))
    (when (string-match "^\\*\\* Translations compared\n" text)
      (let* ((start (match-end 0))
             (end (if (string-match "^\\*\\{1,2\\} " text start)
                      (match-beginning 0)
                    (length text))))
        (delq nil
              (mapcar (lambda (p)
                        (when (string-match "\\`\\*\\([^*\n]+?\\):\\* \\(\\(?:.\\|\n\\)*\\)" p)
                          (cons (match-string 1 p) (string-trim (match-string 2 p)))))
                      (split-string (substring text start end) "\n[ \t]*\n" t)))))))

(defun bcat-sentence--old-references (file)
  "References pasted into the old layout's `* Provided Translations' of
FILE: ((NAME . BODY) …) of its `** NAME' children, without the machine
slots (DharmaMitra, Discussion)."
  (let ((pt (bcat-sentence--l1-body (bcat-sentence--text file) "Provided Translations")))
    (when pt
      (delq nil
            (mapcar (lambda (chunk)
                      (when (string-match "\\`\\([^\n]+\\)\n\\(\\(?:.\\|\n\\)*\\)" chunk)
                        (let ((name (string-trim (match-string 1 chunk)))
                              (body (string-trim
                                     (replace-regexp-in-string
                                      "^:PROPERTIES:\n\\(?:.*\n\\)*?:END:\n?" ""
                                      (match-string 2 chunk)))))
                          (unless (or (string-match-p "\\`DharmaMitra\\|\\`Discussion" name)
                                      (string-empty-p body))
                            (cons name body)))))
                    (split-string (concat "\n" pt) "\n\\*\\* " t))))))

(defun bcat-sentence--extra-references (names refs)
  "Those of REFS ((NAME . TEXT) …) whose NAME is not in NAMES and is not
a machine name (Translation, DharmaMitra) — marks stripped."
  (delq nil (mapcar (lambda (r)
                      (unless (or (member (car r) names)
                                  (member (car r) '("Translation" "DharmaMitra")))
                        (cons (car r) (bcat-analysis-strip-marks (cdr r)))))
                    refs)))

(defun bcat-sentence--write (file text)
  "Write TEXT to FILE — through a visiting buffer when there is one, so
buffer and file stay in step (its unsaved edits are saved first and
were read by the caller)."
  (make-directory (file-name-directory file) t)
  (let ((buf (get-file-buffer file)))
    (if (not buf)
        (let ((make-backup-files nil))
          (with-temp-file file (insert text)))
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (make-backup-files nil))
          ;; The disk version was just read by the caller (user slots,
          ;; references) — a stale visit must not raise Emacs's "changed
          ;; on disk" question in the middle of a fetch.
          (unless (verify-visited-file-modtime buf)
            (set-visited-file-modtime))
          (erase-buffer)
          (insert text)
          (save-buffer))))))

(defun bcat-sentence--save-visit (file)
  "Save FILE's visiting buffer when it has unsaved edits, so the user's
typing is read (and kept) by a rebuild."
  (let ((buf (get-file-buffer file)))
    (when (and buf (buffer-modified-p buf))
      (with-current-buffer buf (save-buffer)))))

;; ----------------------------------------------------------------------------
;; C-c u G (§5.74 step 3b)
;; ----------------------------------------------------------------------------

(declare-function tibetan-cascade--collect-sentences "tibetan-cascade" ())
(declare-function tibetan-cascade--create-file "tibetan-cascade" (sent-num segs source-file))
(declare-function tibetan-sentence--filepath "tibetan-sentence-persist" (sent-num &optional folder source-file))
(declare-function tibetan-analysis--defer-mt-p "tibetan-analysis-claude" (file))
(declare-function tibetan-discussion--submit "tibetan-translation-discussion" (kind file prompts land))
(declare-function tibetan-dharmamitra-api-chat-translate "tibetan-dharmamitra-api" (text &rest args))
(declare-function tibetan-dharmamitra-translation--target-lang "tibetan-dharmamitra-translation" (file))
(declare-function tibetan-dharmamitra-translation--input-encoding "tibetan-dharmamitra-translation" (file))
(defvar tibetan-cascade-subtree-confirm-threshold)
(defvar tibetan-dharmamitra-sentence-request-delay)
(defvar tibetan-sentence-claude--dm-schedule-count)

(defvar bcat-sentence-after-fetch-functions nil
  "Abnormal hook run with the sentence FILE after C-c u G has written
it (§5.74) — C-c u H (PDF + reMarkable) hangs here.")

(defun bcat-sentence--unaligned (file)
  "References under Translations compared of FILE that no segment shows
yet (added by hand after the analysis): ((NAME . TEXT) …), marks
stripped."
  (let ((segments (car (split-string (bcat-sentence--text file) "^\\* Sentence$"))))
    (delq nil (mapcar (lambda (r)
                        (unless (or (equal (car r) "Translation")
                                    (string-match-p (format "^\\*%s:\\* " (regexp-quote (car r)))
                                                    segments))
                          (cons (car r) (bcat-analysis-strip-marks (cdr r)))))
                      (bcat-sentence--compared file)))))

(defun bcat-sentence-fetch-state (file seg-nums)
  "What C-c u G still has to do for the template FILE with SEG-NUMS:
`full' while a segment has no ⟦N⟧ translation; `align' when a reference
under Translations compared appears in no segment (added by hand);
nil when the file is complete."
  (let ((text (bcat-sentence--text file)))
    (cond
     ((cl-some (lambda (n) (not (string-match-p (format "^⟦%d⟧ " n) text))) seg-nums)
      'full)
     ((bcat-sentence--unaligned file) 'align))))

;; ----------------------------------------------------------------------------
;; Step 3c — placing references added by hand
;; ----------------------------------------------------------------------------

(defun bcat-sentence--own-translations (text)
  "The ⟦N⟧ translations in TEXT: ((N . TRANSLATION) …), without the
„*Translation:*“ label (files of 28.10. have none)."
  (let ((pos 0) (out '()))
    (while (string-match "^⟦\\([0-9]+\\)⟧ \\(?:\\*Translation:\\* \\)?\\(.*?\\)\\(?: \\\\\\\\\\)?$" text pos)
      (push (cons (string-to-number (match-string 1 text)) (match-string 2 text)) out)
      (setq pos (match-end 0)))
    (nreverse out)))

(defun bcat-sentence--block-lines (block)
  "The lines of a translation BLOCK without their org line breaks."
  (mapcar (lambda (l) (replace-regexp-in-string " \\\\\\\\\\'" "" l))
          (split-string block "\n")))

(defun bcat-sentence--align-merge (text parsed)
  "TEXT of a template file with the alignment answer PARSED put in:
per segment the new reference parts after the existing references and
the new „→“ line after the existing ones; under Translations compared
each new reference's own paragraph marked from its segment parts (the
text stays the one pasted) and the comparison sentence added.
Everything else stays as it is."
  (with-temp-buffer
    (insert text)
    (dolist (seg (alist-get 'segments parsed))
      (let ((num (alist-get 'num seg)))
        (goto-char (point-min))
        (when (re-search-forward (format "^⟦%s⟧ " num) nil t)
          (let* ((start (line-beginning-position))
                 (end (if (re-search-forward "\n[ \t]*\n" nil t) (match-beginning 0) (point-max)))
                 (lines (bcat-sentence--block-lines (buffer-substring start end)))
                 (arrows (cl-remove-if-not (lambda (l) (string-prefix-p "→ " l)) lines))
                 (others (cl-remove-if (lambda (l) (string-prefix-p "→ " l)) lines))
                 (new-refs (bcat-analysis--reference-lines (alist-get 'references seg)))

                 (all (append others new-refs arrows
                              (bcat-analysis-shift-lines seg))))
            (delete-region start end)
            (goto-char start)
            (insert (mapconcat #'identity all " \\\\\n"))))))
    (goto-char (point-min))
    (when (re-search-forward "^\\*\\* Translations compared\n" nil t)
      (let ((region-start (point)))
        (dolist (name (delete-dups
                       (mapcan (lambda (seg)
                                 (mapcar (lambda (r) (bcat-analysis--field r 'name))
                                         (alist-get 'references seg)))
                               (copy-sequence (alist-get 'segments parsed)))))
          (goto-char region-start)
          (when (and (not (string-empty-p name))
                     (re-search-forward (format "^\\*%s:\\* " (regexp-quote name)) nil t))
            (let* ((pstart (line-beginning-position))
                   (tstart (point))
                   (pend (if (re-search-forward "\n[ \t]*\n\\|\n\\*" nil t)
                             (match-beginning 0)
                           (point-max)))
                   (own (bcat-analysis-strip-marks (buffer-substring tstart pend))))
              (delete-region pstart pend)
              (goto-char pstart)
              (insert (format "*%s:* %s" name
                              (bcat-analysis-mark-reference
                               own (bcat-analysis--reference-parts
                                    (alist-get 'segments parsed) name)))))))
        (let ((comparison (bcat-analysis--field parsed 'comparison)))
          (unless (string-empty-p comparison)
            (goto-char region-start)
            (let ((region-end (if (re-search-forward "^\\*\\{1,2\\} " nil t)
                                  (line-beginning-position)
                                (point-max))))
              (goto-char region-end)
              (skip-chars-backward " \t\n")
              (insert "\n\n" comparison))))))
    (buffer-string)))

(defun bcat-sentence--unreadable (what file response problem)
  "Report that the answer of kind WHAT for FILE could not be read
\(PROBLEM says why) and keep RESPONSE for diagnosis.  Returns nil."
  (message "C-c u G: %s für %s nicht lesbar (%s) — Datei unverändert; Antwort: %s"
           what (file-name-nondirectory file) problem
           (bcat-analysis-keep-response file response))
  nil)

(defun bcat-sentence--align-land (file response)
  "Put the alignment RESPONSE into FILE; nil and an untouched file when
the answer is unreadable."
  (let* ((read (bcat-analysis-read response))
         (parsed (car read)))
    (if (not parsed)
        (bcat-sentence--unreadable "Zuordnung" file response (cdr read))
      (bcat-sentence--save-visit file)
      (bcat-sentence--write file (bcat-sentence--align-merge (bcat-sentence--text file) parsed))
      (run-hook-with-args 'bcat-sentence-after-fetch-functions file)
      file)))

(defun bcat-sentence--align-one (file sentence source-file)
  "ONE small request placing the references added by hand in FILE —
no DharmaMitra, no new analysis."
  (let* ((refs (bcat-sentence--unaligned file))
         (own (bcat-sentence--own-translations (bcat-sentence--text file)))
         (meta (ignore-errors (tibetan-analysis--read-source-metadata source-file))))
    (require 'tibetan-translation-discussion)
    (tibetan-discussion--submit
     "bcat-align" file
     (lambda ()
       (let ((sanskrit (bcat-sentence--sanskrit-p source-file)))
         (cons (bcat-analysis-align-system-prompt (or (plist-get meta :target-lang) "en")
                                                  (if sanskrit "Sanskrit" "Classical Tibetan"))
               (bcat-analysis-align-user-prompt
                (plist-get sentence :sent-num) (plist-get sentence :verse)
                (mapcar (lambda (s) (list (car s) (cdr s)
                                          (unless sanskrit (bcat-sentence--wylie (cdr s)))
                                          (cdr (assq (car s) own))))
                        (plist-get sentence :segs))
                refs
                (plist-get meta :claude-context)))))
     (lambda (f response) (bcat-sentence--align-land f response)))
    'fired))

(defun bcat-sentence--references-for (file sent-num source-file)
  "The reference translations C-c u G sends for sentence SENT-NUM: the
reference files' plus every one added by hand under Translations
compared of FILE."
  (let ((refs (bcat-sentence--references sent-num source-file)))
    (append refs
            (bcat-sentence--extra-references (mapcar #'car refs)
                                             (bcat-sentence--compared file)))))

(defun bcat-sentence--prepare (file sentence source-file)
  "Make FILE a template file before a fetch: create a missing one, and
rebuild one still in the old layout (Carsten: „komplett neu bauen“) —
user slots, #+CREATED and references pasted by hand carried over."
  (let ((sent-num (plist-get sentence :sent-num))
        (segs (plist-get sentence :segs)))
    (cond
     ((not (file-exists-p file))
      (tibetan-cascade--create-file sent-num segs source-file))
     ((not (bcat-sentence-file-p file))
      (bcat-sentence--save-visit file)
      (let ((names (mapcar #'car (bcat-sentence--references sent-num source-file))))
        (bcat-sentence-create-file
         sent-num segs source-file file
         (bcat-sentence--extra-references names (bcat-sentence--old-references file))
         (bcat-sentence--keep file)))))))

(defun bcat-sentence--dharmamitra (file sentence)
  "DharmaMitra's translation of SENTENCE for FILE, or nil (an outage
must not stop the analysis)."
  (condition-case err
      (progn
        (require 'tibetan-dharmamitra-api)
        (require 'tibetan-dharmamitra-translation)
      (let ((tr (tibetan-dharmamitra-api-chat-translate
                 (mapconcat #'cdr (plist-get sentence :segs) "")
                 :target-lang (tibetan-dharmamitra-translation--target-lang file)
                 :input-encoding (tibetan-dharmamitra-translation--input-encoding file))))
        (and (stringp tr) (not (string-blank-p tr)) (string-trim tr))))
    (error (message "DharmaMitra (Satz %s): %s" (plist-get sentence :sent-num)
                    (error-message-string err))
           nil)))

(defun bcat-sentence--prompts (sentence source-file refs)
  "(SYSTEM . USER) for SENTENCE of SOURCE-FILE with the references REFS —
the Sanskrit prompts for a Sanskrit source, the Tibetan ones else."
  (let* ((meta (ignore-errors (tibetan-analysis--read-source-metadata source-file)))
         (target (or (plist-get meta :target-lang) "en"))
         (segs (plist-get sentence :segs)))
    (if (bcat-sentence--sanskrit-p source-file)
        (cons (bcat-analysis-sanskrit-system-prompt target)
              (bcat-analysis-sanskrit-user-prompt
               (plist-get sentence :sent-num) (plist-get sentence :verse)
               (mapcar (lambda (s) (list (car s) (cdr s))) segs)
               refs
               (plist-get meta :claude-context)))
      (cons (bcat-analysis-system-prompt target)
            (bcat-analysis-user-prompt
             (plist-get sentence :sent-num) (plist-get sentence :verse)
             (mapcar (lambda (s) (list (car s) (cdr s) (bcat-sentence--wylie (cdr s)))) segs)
             refs
             (plist-get meta :claude-context))))))

(defun bcat-sentence--land (file sentence source-file refs response)
  "Write Claude's RESPONSE for SENTENCE into the template FILE; nil and
an untouched file when the answer is unreadable.  User slots and
#+CREATED are read from the file NOW (typing during the request
survives); the references under Translations compared are REFS as sent,
marked from the answer's segment parts."
  (let* ((read (bcat-analysis-read response))
         (parsed (car read)))
    (if (not parsed)
        (bcat-sentence--unreadable "Antwort" file response (cdr read))
      (bcat-sentence--save-visit file)
      (let* ((sent-num (plist-get sentence :sent-num))
             (segs (plist-get sentence :segs))
             (base (bcat-sentence-rump-context sent-num segs source-file nil
                                               (bcat-sentence--keep file)))
             ;; LAST_ANALYZED is today, whatever was kept.
             (base (cons (cons 'last-analyzed (format-time-string "%Y-%m-%d")) base)))
        (bcat-sentence--write
         file
         (bcat-template-render-named (bcat-sentence-file-p file)
                                     (funcall (if (bcat-sentence--sanskrit-p source-file)
                                                  #'bcat-analysis-sanskrit-context
                                                #'bcat-analysis-context)
                                              base parsed refs)
                                     source-file))
        (run-hook-with-args 'bcat-sentence-after-fetch-functions file)
        file))))

(defun bcat-sentence--fetch-one (file sentence source-file)
  "Schedule the analysis of SENTENCE into FILE: DharmaMitra first
\(staggered like every sentence-level DharmaMitra call), then ONE Claude
request through the queue."
  (let ((delay (* (if (boundp 'tibetan-sentence-claude--dm-schedule-count)
                      tibetan-sentence-claude--dm-schedule-count 0)
                  (if (boundp 'tibetan-dharmamitra-sentence-request-delay)
                      tibetan-dharmamitra-sentence-request-delay 6.5))))
    (when (boundp 'tibetan-sentence-claude--dm-schedule-count)
      (cl-incf tibetan-sentence-claude--dm-schedule-count))
    (run-at-time
     delay nil
     (lambda ()
       (let* ((refs (bcat-sentence--references-for file (plist-get sentence :sent-num)
                                                   source-file))
              (dm (bcat-sentence--dharmamitra file sentence))
              (refs (if dm (append refs (list (cons "DharmaMitra" dm))) refs)))
         (require 'tibetan-translation-discussion)
         (tibetan-discussion--submit
          "bcat-analysis" file
          (lambda () (bcat-sentence--prompts sentence source-file refs))
          (lambda (f response)
            (bcat-sentence--land f sentence source-file refs response))))))
    'fired))

(defun bcat-sentence-fetch (scope)
  "C-c u G for a source with a template (§5.74): every sentence of
SCOPE gets its file in the template form (created, or rebuilt from the
old layout) and — when still incomplete — one analysis (DharmaMitra,
then Claude).  A filled file is left alone.  More than
`tibetan-cascade-subtree-confirm-threshold' requests ask first.
Returns (:sentences N :fired N :complete N :align N)."
  (let* ((src (plist-get scope :source))
         (label (plist-get scope :label))
         (nums (plist-get scope :sent-nums))
         (folder (file-name-as-directory
                  (expand-file-name "analysis" (file-name-directory src))))
         (sentences (cl-remove-if-not
                     (lambda (s) (memq (plist-get s :sent-num) nums))
                     (with-temp-buffer
                       (insert-file-contents src)
                       (org-mode)
                       (tibetan-cascade--collect-sentences))))
         (file-of (lambda (s) (tibetan-sentence--filepath (plist-get s :sent-num) folder src))))
    (if (ignore-errors (tibetan-analysis--defer-mt-p src))
        (progn (message "%s: #+TIBETAN_DEFER_MT — kein Abruf" label)
               (list :sentences (length sentences) :fired 0 :complete 0 :align 0))
      (dolist (s sentences) (bcat-sentence--prepare (funcall file-of s) s src))
      (let* ((states (mapcar (lambda (s)
                               (cons s (bcat-sentence-fetch-state
                                        (funcall file-of s) (mapcar #'car (plist-get s :segs)))))
                             sentences))
             (todo (cl-remove-if-not #'cdr states))
             (align (cl-count-if (lambda (c) (eq (cdr c) 'align)) states))
             (go (and todo
                      (or (<= (length todo) tibetan-cascade-subtree-confirm-threshold)
                          (y-or-n-p (format "%s: %d Sätze an Claude senden (%d neue Analyse(n), %d Zuordnung(en))? "
                                            label (length todo) (- (length todo) align) align)))))
             (fired 0))
        (when go
          (dolist (c todo)
            (when (eq 'fired
                      (if (eq (cdr c) 'align)
                          (bcat-sentence--align-one (funcall file-of (car c)) (car c) src)
                        (bcat-sentence--fetch-one (funcall file-of (car c)) (car c) src)))
              (cl-incf fired))))
        (message "%s: %d Satz/Sätze — %d abgerufen (davon %d Zuordnung neuer Vergleichsübersetzungen), %d vollständig%s"
                 label (length sentences) fired (if go align 0)
                 (cl-count-if-not #'cdr states)
                 (if (and todo (not go)) " (Abruf abgelehnt)" ""))
        (list :sentences (length sentences) :fired fired
              :complete (cl-count-if-not #'cdr states) :align align)))))

(provide 'bcat-sentence)
;;; bcat-sentence.el ends here
