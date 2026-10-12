;;; tibetan-translation-discussion-test.el --- §5.62 seminar discussion -*- lexical-binding: t -*-

;;; Commentary:
;; Carsten (12.10.2026): an assessment of the provided translation
;; (Dyczkowski, Lopez, …) with the passages worth discussing in the
;; reading seminar — where Claude and DharmaMitra translate
;; differently.  Fixtures in temp dirs; every AI entry point stubbed.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((base-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../persist" base-dir))
  (add-to-list 'load-path (expand-file-name "../core" base-dir))
  (add-to-list 'load-path (expand-file-name "../analysis" base-dir)))

(require 'tibetan-translation-discussion)
(require 'tibetan-sentence-persist)

(defconst tibetan-discussion-test--dyc
  (concat "“I bow to the goddess Parā (the Supreme One), who is the creative "
          "intuition of consciousness (citpratibhā). Bhairava’s Yoginī, She "
          "has made (her) place on (top of) the lotuses.” (2)")
  "Fixture reference translation (Dyczkowski-like).")

(defun tibetan-discussion-test--sent (target &optional dm claude)
  "Sent-file text with Claude, DM (unless DM is `:none') and the
Dyczkowski slot; TARGET the document language."
  (ignore target)
  (concat
   "#+TITLE: Sentence 2 Analysis\n#+TIBETAN_LAYOUT: cascade\n"
   "#+SOURCE_LANG: sa\n"
   "#+SOURCE: [[file:../TA.org::*Sentence 2][TA.org / Sentence 2]]\n"
   "#+SEGMENTS: 3, 4\n\n"
   "* Translation\n"
   (or claude "I bow to the goddess Parā, the luminous intuition of consciousness.")
   "\n\n* Reading\n** Gloss Tables\n*** Segment 3\n| naumi |\n- ⟦3⟧ I bow\n\n"
   "* Sanskrit Analysis\n:PROPERTIES:\n:GENERATED: t\n:END:\n\n"
   "** Claude Vocabulary\nnaumi, verb, \"I bow\", note\n\n"
   "** Claude Grammar\n- *Syntax:* x\n\n"
   "** Concept Notes\n- note\n\n"
   "** Word Analysis\n*** Segment 3\nnaumi cit-pratibhām devīm\n\n"
   "- naumi — √nu; V.PRS.1SG\n\n"
   "*** Segment 4\nmātṛ-māna-prameya-aṃśa\n\n"
   "* Provided Translations\n"
   "** DharmaMitra Translation\n"
   (if (eq dm :none)
       "[Awaiting DharmaMitra…]\n\n"
     (concat ":PROPERTIES:\n:LAST_TRANSLATED: 2026-10-12\n:END:\n\n"
             "(Sentence 2 — segments 3–4)\n\n"
             (or dm "I praise the goddess Parā, the radiance of consciousness.")
             "\n\n"))
   "** Dyczkowski 2023\n" tibetan-discussion-test--dyc "\n\n"
   "* Working Translation\n\n* My Notes\n\n* Footnotes\n"))

(defmacro tibetan-discussion-test--with-corpus (target &rest body)
  "Temp corpus: source TA.org (sa, TARGET language) with Sentence 2
\(segments 3, 4) and its analysis file.  Binds DIR, SRC, FILE."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "discussion-" t))
          (src (expand-file-name "TA.org" dir))
          (folder (file-name-as-directory (expand-file-name "analysis" dir)))
          file)
     (unwind-protect
         (progn
           (make-directory folder t)
           (with-temp-file src
             (insert "#+TITLE: TA\n#+TIBETAN_LAYOUT: cascade\n#+SOURCE_LANG: sa\n"
                     (format "#+TIBETAN_TARGET_LANG: %s\n\n" ,target)
                     "* Text\n** Section Praise\n*** Sentence 2\n"
                     "**** Segment 3\nnaumi citpratibhāṃ devīṃ parāṃ bhairavayoginīm /\n"
                     "**** Segment 4\nmātṛmānaprameyāṃśaśūlāmbujakṛtāspadām // 2\n"))
           (setq file (tibetan-sentence--filepath 2 folder src))
           (with-temp-file file (insert (tibetan-discussion-test--sent ,target)))
           ,@body)
       (dolist (b (buffer-list))
         (when (and (buffer-file-name b)
                    (string-prefix-p dir (buffer-file-name b)))
           (with-current-buffer b (set-buffer-modified-p nil))
           (kill-buffer b)))
       (delete-directory dir t))))

(defun tibetan-discussion-test--read (file)
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(defconst tibetan-discussion-test--response
  "## Assessment
Dyczkowski renders freely and adds explanatory parentheses; Claude and DharmaMitra stay closer to the compound.

## Discussion Points
### 1. citpratibhā
- Text: citpratibhāṃ
- Dyczkowski 2023: \"the creative intuition of consciousness\"
- Claude: \"the luminous intuition of consciousness\"
- DharmaMitra: \"the radiance of consciousness\"
- Issue: pratibhā as intuition or as radiance.
- Assessment: A genitive tatpuruṣa; both readings are lexically possible.
- Question: Which sense of pratibhā fits the Trika context?

### 2. bhairavayoginī
- Text: bhairavayoginīm
- Dyczkowski 2023: \"A PHRASE HE NEVER WROTE\"
- Claude: \"the yoginī of Bhairava\"
- DharmaMitra: \"(not rendered)\"
- Issue: DharmaMitra omits the epithet.
- Assessment: The accusative epithet qualifies devīm.
- Question: Is yoginī here consort or practitioner?"
  "Canned discussion response (point 2 quotes a phrase absent from the
reference — it must stay a point without a marker).")

;; ----------------------------------------------------------------------------
;; Inputs / readiness
;; ----------------------------------------------------------------------------

(ert-deftest tibetan-discussion-inputs-and-ready ()
  "The inputs: every reference translation under * Provided Translations
\(NOT the DM slot, never a discussion), Claude's and DharmaMitra's
translation as plain text (machine label / drawer stripped).  Ready
only when all three kinds are there."
  (tibetan-discussion-test--with-corpus "en"
    (let ((in (tibetan-discussion--inputs file)))
      (should (equal (list (cons "Dyczkowski 2023" tibetan-discussion-test--dyc))
                     (plist-get in :refs)))
      (should (string-prefix-p "I bow to the goddess" (plist-get in :claude)))
      (should (equal "I praise the goddess Parā, the radiance of consciousness."
                     (plist-get in :dm)))
      (should (equal "sa" (plist-get in :lang)))
      (should (equal "en" (plist-get in :target)))
      (should (string-match-p "naumi citpratibhāṃ" (plist-get in :source-text)))
      (should (string-match-p "naumi cit-pratibhām devīm" (plist-get in :padapatha)))
      (should (tibetan-discussion--ready-p in)))
    ;; DM still a placeholder → not ready.
    (with-temp-file file (insert (tibetan-discussion-test--sent "en" :none)))
    (should-not (tibetan-discussion--ready-p (tibetan-discussion--inputs file)))))

(ert-deftest tibetan-discussion-prompt-language-and-contract ()
  "System prompt: seminar role, the output contract, verbatim-quote
rule, the DOCUMENT's target language (English for the Tantrāloka,
German for a de document).  User prompt: source + padapāṭha and every
translation under its label."
  (tibetan-discussion-test--with-corpus "en"
    (let* ((p (tibetan-discussion--build-prompts
               (tibetan-discussion--inputs file)))
           (sys (car p)) (user (cdr p)))
      (dolist (needle '("## Assessment" "## Discussion Points" "verbatim"
                        "English" "seminar"))
        (should (string-match-p (regexp-quote needle) sys)))
      (dolist (needle '("naumi citpratibhāṃ" "naumi cit-pratibhām devīm"
                        "=== Dyczkowski 2023 ===" "=== Claude ==="
                        "=== DharmaMitra ===" "radiance of consciousness"))
        (should (string-match-p (regexp-quote needle) user)))))
  (tibetan-discussion-test--with-corpus "de"
    (should (string-match-p
             "German"
             (car (tibetan-discussion--build-prompts
                   (tibetan-discussion--inputs file)))))))

;; ----------------------------------------------------------------------------
;; Landing
;; ----------------------------------------------------------------------------

(ert-deftest tibetan-discussion-land-renders-and-marks ()
  "Landing writes `** Discussion' as the LAST child of * Provided
Translations: assessment, the reference translation with the
discussed passages marked (⟨N⟩ after the verbatim quote; a quote
not found stays unmarked), the numbered points with labels in the
document language.  The editable Dyczkowski slot is untouched."
  (tibetan-discussion-test--with-corpus "en"
    (should (tibetan-discussion--land file tibetan-discussion-test--response))
    (let ((s (tibetan-discussion-test--read file)))
      (should (< (string-match "^\\* Provided Translations$" s)
                 (string-match "^\\*\\* Dyczkowski 2023$" s)
                 (string-match "^\\*\\* Discussion$" s)
                 (string-match "^\\* Working Translation$" s)))
      (should (string-match-p "Dyczkowski renders freely" s))
      (should (string-match-p "^\\*\\*\\* Dyczkowski 2023 — marked$" s))
      (should (string-match-p
               (regexp-quote "who is *the creative intuition of consciousness⟨1⟩* (citpratibhā)")
               s))
      (should-not (string-match-p "⟨2⟩" s))
      (should (string-match-p "^\\*\\*\\* 1\\. citpratibhā$" s))
      (should (string-match-p "^\\*\\*\\* 2\\. bhairavayoginī$" s))
      (should (string-match-p "^- Issue :: pratibhā as intuition" s))
      (should (string-match-p "^- Question :: Which sense" s))
      (should (string-match-p "^- DharmaMitra :: “(not rendered)”" s))
      ;; The PT slot itself carries no markers.
      (should (string-match-p
               (regexp-quote (concat "** Dyczkowski 2023\n"
                                     tibetan-discussion-test--dyc))
               s))
      ;; A second landing replaces, never duplicates.
      (should (tibetan-discussion--land file tibetan-discussion-test--response))
      (should (= 1 (cl-count "** Discussion"
                             (split-string (tibetan-discussion-test--read file) "\n")
                             :test #'equal))))))

(defun tibetan-discussion-test--bold-texts (line)
  "The bold objects org itself parses in LINE (the arbiter: a marker
org does not recognise is no highlight)."
  (with-temp-buffer
    (org-mode)
    (insert line "\n")
    (org-element-map (org-element-parse-buffer) 'bold
      (lambda (b)
        (buffer-substring-no-properties
         (org-element-property :contents-begin b)
         (org-element-property :contents-end b))))))

(ert-deftest tibetan-discussion-mark-org-emphasis ()
  "A marked passage must be BOLD as org parses it: the ⟨N⟩ marker
goes inside the emphasis (after the closing `*' org requires
whitespace or punctuation — `⟨' would kill the bold).  Where org's
emphasis rules forbid bold (passage inside a word), the passage
still gets its ⟨N⟩, unbolded."
  (let ((m (tibetan-discussion--mark
            "and his body is full. Then (the end)"
            '((1 . "his body is full") (2 . "the end")))))
    (should (equal m "and *his body is full⟨1⟩*. Then (*the end⟨2⟩*)"))
    (should (equal (tibetan-discussion-test--bold-texts m)
                   '("his body is full⟨1⟩" "the end⟨2⟩"))))
  (let ((m (tibetan-discussion--mark "unhappy" '((3 . "happy")))))
    (should (equal m "unhappy⟨3⟩"))
    (should-not (tibetan-discussion-test--bold-texts m))))

(ert-deftest tibetan-discussion-ref-label-drift ()
  "Claude may shorten the reference label (\"Dyczkowski:\" for
\"Dyczkowski 2023\") — the quote is still found and marked, and the
bullet is still rendered as a quote."
  (tibetan-discussion-test--with-corpus "en"
    (should (tibetan-discussion--land
             file
             (replace-regexp-in-string "^- Dyczkowski 2023:" "- Dyczkowski:"
                                       tibetan-discussion-test--response)))
    (let ((s (tibetan-discussion-test--read file)))
      (should (string-match-p "consciousness⟨1⟩\\*" s))
      (should (string-match-p "^- Dyczkowski :: “the creative intuition" s)))))

(ert-deftest tibetan-discussion-land-german-labels ()
  "A German document gets German labels (Problem / Einschätzung /
Frage) and `— markiert'."
  (tibetan-discussion-test--with-corpus "de"
    (tibetan-discussion--land file tibetan-discussion-test--response)
    (let ((s (tibetan-discussion-test--read file)))
      (should (string-match-p "^- Problem :: " s))
      (should (string-match-p "^- Einschätzung :: " s))
      (should (string-match-p "^- Frage :: " s))
      (should (string-match-p "— markiert$" s)))))

(ert-deftest tibetan-discussion-survives-regenerate ()
  "The discussion lives in * Provided Translations — the cascade
regenerate preserves it (PT rest), exactly once."
  (tibetan-discussion-test--with-corpus "en"
    (tibetan-discussion--land file tibetan-discussion-test--response)
    (cl-letf (((symbol-function 'tibetan-analysis-generate-content)
               (lambda (&rest _) "")))
      (tibetan-cascade--regenerate
       file 2 (tibetan-cascade--segs-for-sentence src 2) src))
    (let ((s (tibetan-discussion-test--read file)))
      (should (= 1 (cl-count "** Discussion" (split-string s "\n")
                             :test #'equal)))
      (should (string-match-p "⟨1⟩" s)))))

;; ----------------------------------------------------------------------------
;; Firing
;; ----------------------------------------------------------------------------

(ert-deftest tibetan-discussion-auto-fires-when-both-landed ()
  "Whichever lands second (Claude or DharmaMitra) triggers the
discussion — through the §5.62 hooks; never when inputs are missing,
a discussion exists, the switch is off or MT is deferred."
  (tibetan-discussion-test--with-corpus "en"
    (let ((calls '()))
      (cl-letf (((symbol-function 'tibetan-discussion--request)
                 (lambda (f &optional force) (push (cons f force) calls))))
        (run-hook-with-args 'tibetan-cascade-after-land-functions file)
        (should (equal (list (cons file nil)) calls))
        (setq calls nil)
        (run-hook-with-args 'tibetan-dharmamitra-translation-after-write-functions
                            file)
        (should (= 1 (length calls)))
        ;; switch off
        (setq calls nil)
        (let ((tibetan-discussion-auto nil))
          (tibetan-discussion--maybe-fire file))
        (should-not calls)
        ;; defer-MT
        (cl-letf (((symbol-function 'tibetan-analysis--defer-mt-p)
                   (lambda (&rest _) t)))
          (tibetan-discussion--maybe-fire file))
        (should-not calls)
        ;; discussion already present
        (tibetan-discussion--land file tibetan-discussion-test--response)
        (tibetan-discussion--maybe-fire file)
        (should-not calls)
        ;; inputs missing (DM placeholder)
        (with-temp-file file (insert (tibetan-discussion-test--sent "en" :none)))
        (tibetan-discussion--maybe-fire file)
        (should-not calls)))))

(ert-deftest tibetan-discussion-request-queues-and-lands ()
  "The request runs through the Claude queue (budget
`tibetan-claude-max-tokens'), defers the landing out of the gptel
callback and lands the discussion; a second request for the same
file while one is in flight is a no-op."
  (tibetan-discussion-test--with-corpus "en"
    (let ((added (not (featurep 'gptel)))
          (submits 0) (pending nil))
      (when added (provide 'gptel))
      (unwind-protect
          (cl-letf (((symbol-function 'tibetan-claude-queue-submit)
                     (lambda (job &rest _) (cl-incf submits)
                       (setq pending job)))
                    ((symbol-function 'tibetan-analysis--ensure-gptel-ready)
                     (lambda () t))
                    ((symbol-function 'run-at-time)
                     (lambda (_s _r fn &rest args) (apply fn args)))
                    ((symbol-function 'gptel-request)
                     (lambda (_prompt &rest args)
                       (funcall (plist-get args :callback)
                                tibetan-discussion-test--response nil))))
            (tibetan-discussion--request file)
            (tibetan-discussion--request file)   ; in flight → no-op
            (should (= 1 submits))
            (funcall pending #'ignore)
            (should (string-match-p "^\\*\\* Discussion$"
                                    (tibetan-discussion-test--read file)))
            ;; landed → no longer in flight
            (tibetan-discussion--request file t)
            (should (= 2 submits)))
        (when added (setq features (delq 'gptel features)))))))

(ert-deftest tibetan-discussion-command-at-point ()
  "C-c u c in a cascade source re-creates the discussion for the
sentence at point (FORCE); in an old par-NNN file the old
similarity matrix keeps its key."
  (tibetan-discussion-test--with-corpus "en"
    (let ((calls '()) (old nil))
      (cl-letf (((symbol-function 'tibetan-discussion--request)
                 (lambda (f &optional force) (push (cons f force) calls)))
                ((symbol-function 'tibetan-translation-comparison-refresh)
                 (lambda () (setq old t))))
        (with-current-buffer (find-file-noselect src)
          (goto-char (point-min))
          (re-search-forward "^naumi")
          (tibetan-translation-compare-dwim))
        (should (equal (list (cons (file-truename file) t))
                       (mapcar (lambda (c) (cons (file-truename (car c)) (cdr c)))
                               calls)))
        (let ((par (expand-file-name "par-007.org" folder)))
          (with-temp-file par (insert "* Reference Translations\n"))
          (with-current-buffer (find-file-noselect par)
            (tibetan-translation-compare-dwim)))
        (should old)))))

(provide 'tibetan-translation-discussion-test)
;;; tibetan-translation-discussion-test.el ends here
