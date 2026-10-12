;;; tibetan-translation-discussion.el --- Seminar discussion of provided translations -*- lexical-binding: t -*-

;;; Commentary:
;; §5.62 (2026-10-12).  Carsten reads texts in seminars against
;; published translations (Dyczkowski for the Tantrāloka, Lopez /
;; Wangjié & Mulligan for the Rgyan).  Claude and DharmaMitra usually
;; translate differently — and exactly that is discussed in class,
;; next to the content.  This module produces, per sentence/verse of a
;; cascade file, a `** Discussion' at the end of `* Provided
;; Translations':
;;
;;   - an ASSESSMENT of the reference translation(s) for the passage,
;;   - the reference translation(s) with the discussed passages MARKED
;;     (⟨N⟩ after the passage; the tool places the markers itself at
;;     the verbatim quotes Claude gives — a quote it cannot find stays
;;     unmarked, a wrong passage can never be marked),
;;   - numbered DISCUSSION POINTS: source words, each translation's
;;     rendering verbatim, what is at stake, which reading the source
;;     supports, one open question for the seminar.
;;
;; It fires AUTOMATICALLY once Claude AND DharmaMitra have landed for
;; the sentence (C-c u G — whichever lands second triggers, via the
;; §5.62 hooks), and on demand with C-c u c (re-create for the sentence
;; or every sentence under the heading at point).  Language = the
;; document's target language (Carsten's decision).  The editable
;; reference slots are never modified; Working Translation is not
;; compared.

;;; Code:

(require 'cl-lib)
(require 'tibetan-analysis-claude)     ; defer-MT, gptel readiness, rate limit
(require 'tibetan-cascade)
(require 'tibetan-sanskrit-reading)
(require 'tibetan-wylie)

(defvar gptel-max-tokens)
(defvar gptel-cache)
(defvar tibetan-claude-max-tokens)
(declare-function gptel-request "gptel")
(declare-function tibetan-claude-queue-submit "tibetan-claude-queue")
(declare-function tibetan-translation-comparison-refresh "tibetan-analysis-claude")

(defcustom tibetan-discussion-auto t
  "When non-nil, the seminar discussion of a cascade sentence is
requested automatically as soon as Claude AND DharmaMitra have landed
\(and reference translations exist).  One extra Claude call per
sentence.  C-c u c always works on demand."
  :type 'boolean
  :group 'tibetan-cat)

(defconst tibetan-discussion--heading "Discussion"
  "The `** ' heading of the discussion under * Provided Translations.")

(defconst tibetan-discussion--labels
  '(("de" . (("Issue" . "Problem") ("Assessment" . "Einschätzung")
             ("Question" . "Frage") ("marked" . "markiert")))
    ("en" . (("Issue" . "Issue") ("Assessment" . "Assessment")
             ("Question" . "Question") ("marked" . "marked"))))
  "Display labels per target language (the response uses the English
keys; the tool renders them in the document's language).")

(defun tibetan-discussion--label (key target)
  "KEY rendered for TARGET language."
  (or (cdr (assoc key (cdr (assoc target tibetan-discussion--labels))))
      key))

;; ============================================================================
;; Inputs
;; ============================================================================

(defun tibetan-discussion--pt-children (file)
  "((NAME . BODY) …) of the `** NAME' children of * Provided
Translations in FILE, in file order; BODY up to the next `*'/`**'."
  (when (and file (file-exists-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (goto-char (point-min))
      (when (re-search-forward "^\\* Provided Translations[ \t]*$" nil t)
        (let ((end (save-excursion
                     (if (re-search-forward "^\\* " nil t)
                         (line-beginning-position)
                       (point-max))))
              (out '()))
          (while (re-search-forward "^\\*\\* \\(.+?\\)[ \t]*$" end t)
            (let* ((name (match-string 1))
                   (start (1+ (line-end-position)))
                   (stop (save-excursion
                           (goto-char start)
                           (if (re-search-forward "^\\*\\{1,2\\} " end t)
                               (line-beginning-position)
                             end))))
              (push (cons name (buffer-substring-no-properties
                                (min start stop) stop))
                    out)))
          (nreverse out))))))

(defun tibetan-discussion--references (file)
  "The reference translations of FILE: every `** NAME' under * Provided
Translations except the DharmaMitra slot, Claude Particles, the
discussion itself and Carsten's own rubrics (`Übersetzung …').
\((NAME . TEXT) …), drawers stripped, empty slots dropped."
  (delq nil
        (mapcar (lambda (nb)
                  (let ((name (car nb)))
                    (unless (or (member name (list "DharmaMitra Translation"
                                                   "Claude Particles"
                                                   tibetan-discussion--heading))
                                (string-prefix-p "Übersetzung" name))
                      (let ((tx (tibetan-cascade--clean-slot-text (cdr nb))))
                        (and tx (cons name tx))))))
                (tibetan-discussion--pt-children file))))

(defun tibetan-discussion--inputs (file)
  "Everything the discussion of the cascade sentence FILE needs, as a
plist: :file :source-file :sent-num :lang :target :source-text
:wylie (bo) :padapatha (sa, from Word Analysis) :claude :dm :refs."
  (when (and file (file-exists-p file))
    (let* ((src (tibetan-sentence--source-file-from-analysis file))
           (n (tibetan-sentence--sent-id-from-filename file))
           (meta (and src (tibetan-analysis--read-source-metadata src)))
           (lang (if src (tibetan-analysis--resolve-source-lang src) "bo"))
           (sa (equal lang "sa"))
           (segs (and src n (tibetan-cascade--segs-for-sentence src n)))
           (sections (tibetan-analysis--read-claude-sections file)))
      (list :file file :source-file src :sent-num n :lang lang
            :target (or (plist-get meta :target-lang) "en")
            :source-text (mapconcat (lambda (s) (string-trim (cdr s)))
                                    segs "\n")
            :wylie (unless sa
                     (mapconcat (lambda (s)
                                  (string-trim
                                   (tibetan-to-wylie-fixed (cdr s))))
                                segs "\n"))
            :padapatha (when sa
                         (let ((wa (tibetan-sanskrit-reading-parse-word-analysis
                                    (tibetan-sentence--read-l2-body
                                     file "Word Analysis"))))
                           (when wa
                             (mapconcat (lambda (g)
                                          (mapconcat #'identity
                                                     (plist-get (cdr g) :pada)
                                                     " "))
                                        wa "\n"))))
            :claude (tibetan-cascade--clean-slot-text
                     (plist-get sections :translation))
            :dm (tibetan-cascade--clean-slot-text
                 (tibetan-sentence--read-l2-body file "DharmaMitra Translation"))
            :refs (tibetan-discussion--references file)))))

(defun tibetan-discussion--ready-p (inputs)
  "Non-nil when INPUTS carry reference translation(s), Claude's AND
DharmaMitra's translation — the discussion compares all three."
  (and inputs
       (plist-get inputs :refs)
       (plist-get inputs :claude)
       (plist-get inputs :dm)
       t))

(defun tibetan-discussion--present-p (file)
  "Non-nil when FILE already carries a `** Discussion'."
  (assoc tibetan-discussion--heading (tibetan-discussion--pt-children file)))

;; ============================================================================
;; Prompt
;; ============================================================================

(defun tibetan-discussion--system-prompt (target)
  "The system prompt — constant per TARGET language (cache prefix)."
  (format
   "You are preparing a university reading seminar on Buddhist and Indian
philosophical texts (Sanskrit or Classical Tibetan) that are read
against published translations.  For ONE sentence/verse you receive
the source text and several translations: one or more published
REFERENCE translations (labelled with translator and year), Claude's
translation and DharmaMitra's machine translation.

Assess the reference translation(s) and single out the passages worth
discussing in class — places where the translations differ in
SUBSTANCE: construal of case, compound or syntax; choice or
understanding of a technical term; interpretive additions (e.g.
bracketed glosses); omissions; a different reading of the text.
Ignore merely stylistic variants.  Be fair: a published translation
may be right where the machine translations are wrong — say which
reading the source supports and why, citing the source words.

Write in %s (the seminar language of this text); quote the
translations verbatim in their own language.

Produce EXACTLY these two sections:

## Assessment
2-4 sentences: the character of the reference translation(s) for this
passage (literal or free, additions, interpretive tendencies) and how
the machine translations relate to it.

## Discussion Points
2-6 points in text order (fewer when the translations largely agree),
each in this exact shape:
### N. <short title: the source word(s)>
- Text: <the source word(s)>
- <reference label exactly as given>: \"<verbatim quote>\"   (one line per reference translation)
- Claude: \"<verbatim quote>\"
- DharmaMitra: \"<verbatim quote>\"   (write \"(not rendered)\" when a translation omits it)
- Issue: <what is at stake>
- Assessment: <which rendering the source supports and why>
- Question: <one open question for the seminar>
Keep the line labels Text / Claude / DharmaMitra / Issue / Assessment /
Question in English as shown.  Every quote MUST be an exact,
contiguous substring of that translation as given — the quotes are
located automatically.  If there is nothing substantial to discuss,
write `[No substantial divergences]' under ## Discussion Points."
   (if (equal target "de") "German" "English")))

(defun tibetan-discussion--build-prompts (inputs)
  "(SYSTEM . USER) for the discussion of INPUTS."
  (cons
   (tibetan-discussion--system-prompt (plist-get inputs :target))
   (concat
    (if (equal (plist-get inputs :lang) "sa")
        "Source (Sanskrit, IAST):\n"
      "Source (Classical Tibetan):\n")
    (plist-get inputs :source-text) "\n"
    (when (plist-get inputs :wylie)
      (concat "\nWylie:\n" (plist-get inputs :wylie) "\n"))
    (when (plist-get inputs :padapatha)
      (concat "\nPadapāṭha (sandhi resolved):\n"
              (plist-get inputs :padapatha) "\n"))
    "\nTranslations:\n"
    (mapconcat (lambda (nb) (format "\n=== %s ===\n%s\n" (car nb) (cdr nb)))
               (append (plist-get inputs :refs)
                       (list (cons "Claude" (plist-get inputs :claude))
                             (cons "DharmaMitra" (plist-get inputs :dm))))
               "")
    "\nProduce the two sections now.")))

;; ============================================================================
;; Parse + render
;; ============================================================================

(defun tibetan-discussion--parse (response)
  "Parse RESPONSE into (:assessment STR :points (POINT …)); POINT =
\(:title STR :items ((LABEL . TEXT) …))."
  (let ((assessment nil) (points '()))
    (when (and response
               (string-match "^## Assessment[ \t]*\n\\(\\(?:.\\|\n\\)*?\\)\\(?:^## \\|\\'\\)"
                             response))
      (setq assessment (string-trim (match-string 1 response))))
    (when (string-match "^## Discussion Points[ \t]*\n" (or response ""))
      (let ((body (substring response (match-end 0))))
        (dolist (chunk (cdr (split-string body "^### " )))
          (let* ((lines (split-string chunk "\n"))
                 (title (string-trim
                         (replace-regexp-in-string "\\`[0-9]+\\.?[ \t]*" ""
                                                   (car lines))))
                 (items '()))
            (dolist (l (cdr lines))
              (cond
               ((string-match "^-[ \t]+\\([^:\n]+?\\):[ \t]*\\(.*\\)$" l)
                (push (cons (string-trim (match-string 1 l))
                            (string-trim (match-string 2 l)))
                      items))
               ((and items (string-match-p "^[ \t]+\\S-" l))
                (setcdr (car items)
                        (concat (cdar items) " " (string-trim l))))))
            (when items
              (push (list :title title :items (nreverse items)) points))))))
    (list :assessment assessment :points (nreverse points))))

(defun tibetan-discussion--unquote (s)
  "S without surrounding quote marks."
  (string-trim (or s "") "[ \t\"“”‘’'„]+" "[ \t\"“”‘’'„]+"))

(defun tibetan-discussion--org-safe (s)
  "S with line-leading `*' runs and `#' defused for org."
  (replace-regexp-in-string "^\\(\\*+\\|#\\)" " \\1" (or s "")))

(defun tibetan-discussion--mark (body quotes)
  "BODY with each (N . QUOTE) of QUOTES marked: the first verbatim
\(whitespace-tolerant) occurrence gets ⟨N⟩ appended — and is bold,
marker included, where org's emphasis rules allow.  Quotes not found
stay unmarked."
  (let ((case-fold-search nil))
    (dolist (nq quotes body)
      (let* ((words (split-string (cdr nq) "[ \t\n]+" t))
             (re (and words (mapconcat #'regexp-quote words "[ \t\n]+"))))
        (when (and re (string-match re body))
          (let* ((beg (match-beginning 0)) (end (match-end 0))
                 (hit (match-string 0 body))
                 (pre (and (> beg 0) (aref body (1- beg))))
                 (post (and (< end (length body)) (aref body end)))
                 (bold (and (not (string-match-p "[*\n]" hit))
                            (or (null pre)
                                (memq pre '(?\s ?\t ?\n ?- ?\( ?' ?\" ?{)))
                            (or (null post)
                                (memq post '(?\s ?\t ?\n ?- ?. ?, ?: ?! ??
                                                 ?\; ?' ?\" ?\) ?} ?\[)))))
                 (marked (format "%s⟨%d⟩" hit (car nq))))
            ;; The marker goes INSIDE the emphasis: after a closing
            ;; `*' org needs whitespace or punctuation, and `⟨' is
            ;; neither — `*x*⟨1⟩' would not be bold at all.
            (setq body (concat (substring body 0 beg)
                               (if bold (concat "*" marked "*") marked)
                               (substring body end)))))))))

(defun tibetan-discussion--ref-label-p (key ref-name)
  "Non-nil when the response line label KEY names the reference
REF-NAME — exactly, or shortened/extended (\"Dyczkowski\" for
\"Dyczkowski 2023\"); models drift from the label contract."
  (let ((k (downcase (string-trim key)))
        (r (downcase ref-name)))
    (or (string= k r)
        (string-prefix-p (concat k " ") r)
        (string-prefix-p (concat r " ") k))))

(defun tibetan-discussion--render (parsed inputs date)
  "The `** Discussion' subtree for PARSED with INPUTS, stamped DATE."
  (let* ((target (plist-get inputs :target))
         (points (plist-get parsed :points))
         (refs (plist-get inputs :refs)))
    (concat
     "** " tibetan-discussion--heading "\n"
     ":PROPERTIES:\n:GENERATED: " date "\n:END:\n"
     (tibetan-discussion--org-safe (or (plist-get parsed :assessment) "")) "\n\n"
     (mapconcat
      (lambda (ref)
        (let ((quotes (cl-loop for p in points for i from 1
                               for q = (cdr (cl-find-if
                                             (lambda (it)
                                               (tibetan-discussion--ref-label-p
                                                (car it) (car ref)))
                                             (plist-get p :items)))
                               when q collect (cons i (tibetan-discussion--unquote q)))))
          (format "*** %s — %s\n%s\n\n" (car ref)
                  (tibetan-discussion--label "marked" target)
                  (tibetan-discussion--org-safe
                   (tibetan-discussion--mark (cdr ref) quotes)))))
      refs "")
     (cl-loop
      for p in points for i from 1
      concat (concat
              (format "*** %d. %s\n" i (plist-get p :title))
              (mapconcat
               (lambda (it)
                 (let* ((key (car it))
                        (quoted (or (member key '("Claude" "DharmaMitra"))
                                    (cl-some (lambda (ref)
                                               (tibetan-discussion--ref-label-p
                                                key (car ref)))
                                             refs))))
                   (format "- %s :: %s"
                           (tibetan-discussion--label key target)
                           (tibetan-discussion--org-safe
                            (if quoted
                                (format "“%s”" (tibetan-discussion--unquote (cdr it)))
                              (cdr it))))))
               (plist-get p :items) "\n")
              "\n\n")))))

(defun tibetan-discussion--write (file block)
  "Put BLOCK as the last child of * Provided Translations in FILE,
replacing an existing `** Discussion'.  t on success."
  (let ((buf (tibetan-fresh-file-buffer file)))
    (with-current-buffer buf
      (save-excursion
        (goto-char (point-min))
        (when (re-search-forward "^\\* Provided Translations[ \t]*$" nil t)
          (let ((pt-end (save-excursion
                          (if (re-search-forward "^\\* " nil t)
                              (line-beginning-position)
                            (point-max)))))
            ;; Remove an existing discussion subtree.
            (save-excursion
              (when (re-search-forward
                     (format "^\\*\\* %s[ \t]*$"
                             (regexp-quote tibetan-discussion--heading))
                     pt-end t)
                (let ((beg (line-beginning-position))
                      (stop (save-excursion
                              (forward-line 1)
                              (if (re-search-forward "^\\*\\{1,2\\} " nil t)
                                  (line-beginning-position)
                                (point-max)))))
                  (delete-region beg stop))))
            (goto-char (point-min))
            (re-search-forward "^\\* Provided Translations[ \t]*$" nil t)
            (goto-char (if (re-search-forward "^\\* " nil t)
                           (line-beginning-position)
                         (point-max)))
            (skip-chars-backward " \t\n")
            (delete-region (point) (save-excursion
                                     (skip-chars-forward " \t\n")
                                     (point)))
            (insert "\n\n" (string-trim-right block) "\n\n")
            (save-buffer)
            t))))))

(defun tibetan-discussion--land (file response)
  "Parse RESPONSE and write the discussion into FILE.  t on success."
  (let ((parsed (tibetan-discussion--parse response))
        (inputs (tibetan-discussion--inputs file)))
    (when (and inputs (or (plist-get parsed :points)
                          (plist-get parsed :assessment)))
      (tibetan-discussion--write
       file (tibetan-discussion--render parsed inputs
                                        (format-time-string "%Y-%m-%d"))))))

;; ============================================================================
;; Firing
;; ============================================================================

(defvar tibetan-discussion--inflight (make-hash-table :test #'equal)
  "Files (truenames) whose discussion request is in flight.")

(defun tibetan-discussion--request (file &optional force)
  "Queue the discussion request for the cascade sentence FILE.
No-op while one is in flight, when the inputs are incomplete, or
when a discussion exists and FORCE is nil."
  (let ((key (file-truename file)))
    (unless (gethash key tibetan-discussion--inflight)
      (let ((inputs (tibetan-discussion--inputs file)))
        (when (and (tibetan-discussion--ready-p inputs)
                   (or force (not (tibetan-discussion--present-p file))))
          (puthash key t tibetan-discussion--inflight)
          (require 'tibetan-claude-queue)
          (tibetan-claude-queue-submit
           (lambda (done)
             (condition-case err
                 (progn
                   (unless (and (featurep 'gptel) (fboundp 'gptel-request))
                     (error "gptel not loaded"))
                   (tibetan-analysis--ensure-gptel-ready)
                   (let* ((prompts (tibetan-discussion--build-prompts
                                    (tibetan-discussion--inputs file)))
                          (gptel-cache '(system))
                          (gptel-max-tokens tibetan-claude-max-tokens))
                     (gptel-request
                      (cdr prompts)
                      :system (car prompts)
                      :callback
                      (lambda (response info)
                        (cond
                         ((and (stringp response)
                               (not (string-empty-p response)))
                          ;; Land OUTSIDE the gptel sentinel (§5.40).
                          (run-at-time
                           0 nil
                           (lambda ()
                             (unwind-protect
                                 (condition-case e
                                     (tibetan-discussion--land file response)
                                   (error (message "Discussion landing failed (%s): %s"
                                                   (file-name-nondirectory file)
                                                   (error-message-string e))))
                               (remhash key tibetan-discussion--inflight))))
                          (funcall done '(:status ok)))
                         ((tibetan-analysis--claude-status-rate-limited-p info)
                          (funcall done '(:status rate-limited)))
                         (t
                          (remhash key tibetan-discussion--inflight)
                          (funcall done (list :status 'error
                                              :error "no response"))))))))
               (error
                (remhash key tibetan-discussion--inflight)
                (funcall done (list :status 'error
                                    :error (error-message-string err))))))
           :label (format "discussion %s" (file-name-nondirectory file))
           :on-fail (lambda (_status)
                      (remhash key tibetan-discussion--inflight)))
          t)))))

(defun tibetan-discussion--maybe-fire (file)
  "Hook function: request the discussion of the cascade sentence FILE
once Claude AND DharmaMitra are there — unless switched off
\(`tibetan-discussion-auto'), MT is deferred, or it exists."
  (when (and tibetan-discussion-auto file (file-exists-p file)
             (tibetan-cascade-file-p file)
             (not (tibetan-analysis--defer-mt-p file))
             (not (tibetan-discussion--present-p file))
             (tibetan-discussion--ready-p (tibetan-discussion--inputs file)))
    (tibetan-discussion--request file)))

(add-hook 'tibetan-cascade-after-land-functions #'tibetan-discussion--maybe-fire)
(add-hook 'tibetan-dharmamitra-translation-after-write-functions
          #'tibetan-discussion--maybe-fire)

;;;###autoload
(defun tibetan-translation-compare-dwim ()
  "C-c u c: discuss the translations of the unit at point.
In a cascade source or sent-NNN analysis buffer: (re-)create the
seminar discussion (§5.62) for the sentence at point, or for every
sentence under the heading at point (confirmed above
`tibetan-cascade-subtree-confirm-threshold').  In an old par-NNN.org
file: the pairwise similarity matrix (`tibetan-translation-
comparison-refresh') as before."
  (interactive)
  (let ((f (buffer-file-name)))
    (if (and f (string-match-p "\\`par-[0-9]+" (file-name-nondirectory f)))
        (tibetan-translation-comparison-refresh)
      (let* ((scope (tibetan-cascade--scope-at-point))
             (src (plist-get scope :source))
             (folder (file-name-as-directory
                      (expand-file-name "analysis" (file-name-directory src))))
             (files (cl-remove-if-not
                     #'file-exists-p
                     (mapcar (lambda (n) (tibetan-sentence--filepath n folder src))
                             (plist-get scope :sent-nums))))
             (ready (cl-remove-if-not
                     (lambda (file) (tibetan-discussion--ready-p
                                     (tibetan-discussion--inputs file)))
                     files)))
        (when (and (> (length ready) tibetan-cascade-subtree-confirm-threshold)
                   (not (y-or-n-p (format "%s: %d Diskussionen bei Claude anfordern? "
                                          (plist-get scope :label)
                                          (length ready)))))
          (user-error "Abgebrochen"))
        (dolist (file ready)
          (tibetan-discussion--request file t))
        (message "%s: %d Diskussion(en) angefordert, %d ohne Claude/DharmaMitra/Referenz (erst C-c u G)"
                 (plist-get scope :label) (length ready)
                 (- (length (plist-get scope :sent-nums)) (length ready)))))))

(provide 'tibetan-translation-discussion)
;;; tibetan-translation-discussion.el ends here
