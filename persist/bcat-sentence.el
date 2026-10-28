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

(defun bcat-sentence-rump-context (sent-num segs source-file
                                            &optional extra-refs keep)
  "Template fields of the new sentence file SENT-NUM (SEGS = its
\((GLOBAL-NUM . TEXT) …) from SOURCE-FILE), all without AI.
EXTRA-REFS ((NAME . TEXT) …) are references beyond the reference
files (pasted by hand); KEEP an alist of fields carried over from an
existing file (user slots, `created')."
  (bcat-sentence--with-source source-file
    (let* ((name (file-name-nondirectory source-file))
           (date (format-time-string "%Y-%m-%d")))
      (append
       keep
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

(defun bcat-sentence--strip-marks (text)
  "TEXT without org bold marks (the meaning-shift marks of a fetch)."
  (replace-regexp-in-string "\\*\\([^*\n]+\\)\\*" "\\1" text t))

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
                        (cons (car r) (bcat-sentence--strip-marks (cdr r)))))
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

(defun bcat-sentence-fetch-state (file seg-nums)
  "What C-c u G still has to do for the template FILE with SEG-NUMS:
`full' while a segment has no ⟦N⟧ translation; `align' when a reference
under Translations compared appears in no segment (added by hand);
nil when the file is complete."
  (let ((text (bcat-sentence--text file)))
    (cond
     ((cl-some (lambda (n) (not (string-match-p (format "^⟦%d⟧ " n) text))) seg-nums)
      'full)
     ((cl-some (lambda (r)
                 (and (not (member (car r) '("Translation")))
                      (not (string-match-p (format "^\\*%s:\\* " (regexp-quote (car r)))
                                           (car (split-string text "^\\* Sentence$"))))))
               (bcat-sentence--compared file))
      'align))))

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
  "(SYSTEM . USER) for SENTENCE of SOURCE-FILE with the references REFS."
  (let ((meta (ignore-errors (tibetan-analysis--read-source-metadata source-file))))
    (cons (bcat-analysis-system-prompt (or (plist-get meta :target-lang) "en"))
          (bcat-analysis-user-prompt
           (plist-get sentence :sent-num) (plist-get sentence :verse)
           (mapcar (lambda (s) (list (car s) (cdr s) (bcat-sentence--wylie (cdr s))))
                   (plist-get sentence :segs))
           refs
           (plist-get meta :claude-context)))))

(defun bcat-sentence--land (file sentence source-file refs response)
  "Write Claude's RESPONSE for SENTENCE into the template FILE; nil and
an untouched file when the answer is unreadable.  User slots and
#+CREATED are read from the file NOW (typing during the request
survives); references Claude left out of the comparison are kept."
  (let ((parsed (bcat-analysis-parse response)))
    (if (not parsed)
        (progn (message "C-c u G: Antwort für %s nicht lesbar — Datei unverändert"
                        (file-name-nondirectory file))
               nil)
      (bcat-sentence--save-visit file)
      (let* ((sent-num (plist-get sentence :sent-num))
             (segs (plist-get sentence :segs))
             (returned (mapcar (lambda (r) (alist-get 'name r))
                               (alist-get 'references parsed)))
             (missing (cl-remove-if (lambda (r) (member (car r) returned)) refs))
             (parsed (if missing
                         (cons (cons 'references
                                     (append (alist-get 'references parsed)
                                             (mapcar (lambda (r) `((name . ,(car r)) (text . ,(cdr r))))
                                                     missing)))
                               parsed)
                       parsed))
             (base (bcat-sentence-rump-context sent-num segs source-file nil
                                               (bcat-sentence--keep file)))
             ;; LAST_ANALYZED is today, whatever was kept.
             (base (cons (cons 'last-analyzed (format-time-string "%Y-%m-%d")) base)))
        (bcat-sentence--write
         file
         (bcat-template-render-named (bcat-sentence-file-p file)
                                     (bcat-analysis-context base parsed)
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
             (todo (cl-remove-if-not (lambda (c) (eq (cdr c) 'full)) states))
             (align (cl-count-if (lambda (c) (eq (cdr c) 'align)) states))
             (go (and todo
                      (or (<= (length todo) tibetan-cascade-subtree-confirm-threshold)
                          (y-or-n-p (format "%s: %d Sätze an DharmaMitra + Claude senden? "
                                            label (length todo))))))
             (fired 0))
        (when go
          (dolist (c todo)
            (when (eq 'fired (bcat-sentence--fetch-one (funcall file-of (car c)) (car c) src))
              (cl-incf fired))))
        (message "%s: %d Satz/Sätze — %d abgerufen, %d vollständig%s"
                 label (length sentences) fired
                 (cl-count-if-not #'cdr states)
                 (if (> align 0)
                     (format ", %d mit neuer Vergleichsübersetzung (Zuordnung folgt)" align)
                   ""))
        (list :sentences (length sentences) :fired fired
              :complete (cl-count-if-not #'cdr states) :align align)))))

(provide 'bcat-sentence)
;;; bcat-sentence.el ends here
