;;; tibetan-sanskrit-comparison-test.el --- §5.64 Sanskrit–Tibetan comparison -*- lexical-binding: t -*-

;;; Commentary:
;; Carsten (20.10.2026, Reading MSA): bei zweisprachigen Texten ein
;; Vergleich des Tibetischen mit dem Sanskrit — eigener Abschnitt in
;; der tibetischen Satzdatei, mit C-c u G geholt.  Die tibetische
;; Kaskade zeigt mit `#+TIBETAN_PARALLEL_SOURCE:' auf die
;; Sanskrit-Kaskade; gleiche `** Section'-Titel verbinden die Verse.
;; Fixtures in temp dirs; every AI entry point stubbed.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((base-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../persist" base-dir))
  (add-to-list 'load-path (expand-file-name "../core" base-dir))
  (add-to-list 'load-path (expand-file-name "../analysis" base-dir)))

(require 'tibetan-sanskrit-comparison)
(require 'tibetan-translation-discussion)
(require 'tibetan-sentence-persist)

(defconst tibetan-comparison-test--response
  (concat
   "## Correspondence\n"
   "The Tibetan renders MSA I.1 (Sanskrit sentence 1); the prose sentence 2 "
   "has no Tibetan counterpart in this verse.\n\n"
   "## Divergences\n"
   "### 1. don shes pa\n"
   "- Tibetan: don shes pa\n"
   "- Sanskrit: arthajñaḥ\n"
   "- Divergence: the Tibetan nominalises with pa where the Sanskrit has a bahuvrīhi,\n"
   "  continued on a second line.\n"
   "- Significance: subject vs. epithet.\n\n"
   "### 2. ched du\n"
   "- Tibetan: sems can gyi ched du\n"
   "- Sanskrit: sattveṣu tadgāmiṣu\n"
   "- Divergence: locative rendered as purpose.\n"
   "- Significance: the nimittasaptamī reading is decided by the translator.\n\n"
   "## Assessment\n"
   "The Tibetan follows the Sanskrit closely; the purposive ched du is the one "
   "interpretive choice.\n")
  "Fixture Claude response.")

(defun tibetan-comparison-test--sent ()
  "Sent-file text of the Tibetan sentence 2 (no Claude content yet)."
  (concat
   "#+TITLE: Sentence 2 Analysis\n#+TIBETAN_LAYOUT: cascade\n"
   "#+SOURCE: [[file:../mdo.org::*Sentence 2][mdo.org / Sentence 2]]\n"
   "#+SEGMENTS: 6, 7\n\n"
   "* Translation\n[Requesting translation...]\n\n"
   "* Reading\n** Gloss Tables\n*** Segment 6\n| don |\n- ⟦6⟧ [Awaiting sentence translation…]\n\n"
   "* Tibetan Analysis\n:PROPERTIES:\n:GENERATED: t\n:END:\n\n"
   "** Claude Vocabulary\n[Awaiting Claude…]\n\n"
   "** Concept Notes\n[Awaiting Claude…]\n\n"
   "* Provided Translations\n"
   "** DharmaMitra Translation\n[Awaiting DharmaMitra…]\n\n"
   "* Working Translation\n\n* My Notes\n\n* Footnotes\n"))

(defmacro tibetan-comparison-test--with-corpus (target &rest body)
  "Temp corpus: Tibetan cascade mdo.org (TARGET language, parallel
source msa.org) with Section Titel / Section I.1 (Sentence 2) /
Section I.2 (Sentence 3), the Sanskrit cascade msa.org with Section
I.1 (Sentences 1, 2) and I.2 (Sentence 3), and the sent file of mdo
Sentence 2.  Binds DIR, SRC, SA-SRC, FOLDER, FILE."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "comparison-" t))
          (src (expand-file-name "mdo.org" dir))
          (sa-src (expand-file-name "msa.org" dir))
          (folder (file-name-as-directory (expand-file-name "analysis" dir)))
          file)
     (unwind-protect
         (progn
           (make-directory folder t)
           (with-temp-file src
             (insert "#+TITLE: mdo sde'i rgyan — CAT-Quelle\n"
                     "#+TIBETAN_LAYOUT: cascade\n"
                     (format "#+TIBETAN_TARGET_LANG: %s\n" ,target)
                     "#+TIBETAN_PARALLEL_SOURCE: msa.org\n\n"
                     "* Tibetan Text\n"
                     "** Section Titel\n*** Sentence 1\n**** Segment 1\nརྒྱ་གར་སྐད་དུ།\n\n"
                     "** Section I.1\n:PROPERTIES:\n:TOPIC: alaṃkāra\n:END:\n"
                     "*** Sentence 2\n"
                     "**** Segment 6\nདོན་ཤེས་པས་དོན་གསལ་བར་བྱེད།\n\n"
                     "**** Segment 7\nསེམས་ཅན་གྱི་ཆེད་དུ་སྟོན་པ།\n\n"
                     "** Section I.2\n*** Sentence 3\n**** Segment 8\nདཔེ་ལྔ།\n\n"))
           (with-temp-file sa-src
             (insert "#+TITLE: Mahāyānasūtrālaṃkāra I — CAT-Quelle\n"
                     "#+TIBETAN_LAYOUT: cascade\n#+SOURCE_LANG: sa\n\n"
                     "* Text\n"
                     "** Section I.1\n"
                     "*** Sentence 1\n**** Segment 1\narthajño 'rthavibhāvanāṃ prakurute\n\n"
                     "**** Segment 2\nsattveṣu tadgāmiṣu darśayan // 1\n\n"
                     "*** Sentence 2\n**** Segment 3\nko 'laṃkaroti / arthajñaḥ /\n\n"
                     "** Section I.2\n*** Sentence 3\n**** Segment 4\npañca dṛṣṭāntāḥ\n\n"))
           (setq file (tibetan-sentence--filepath 2 folder src))
           (with-temp-file file (insert (tibetan-comparison-test--sent)))
           ,@body)
       (dolist (b (buffer-list))
         (when (and (buffer-file-name b)
                    (string-prefix-p dir (buffer-file-name b)))
           (with-current-buffer b (set-buffer-modified-p nil))
           (kill-buffer b)))
       (delete-directory dir t))))

(defun tibetan-comparison-test--read (file)
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

;; ----------------------------------------------------------------------------
;; Parallel source and inputs
;; ----------------------------------------------------------------------------

(ert-deftest tibetan-comparison-parallel-source ()
  "`#+TIBETAN_PARALLEL_SOURCE:' resolves relative to the source; absent
→ nil."
  (tibetan-comparison-test--with-corpus "en"
    (should (equal (file-truename sa-src)
                   (file-truename (tibetan-cascade-parallel-source src))))
    (should-not (tibetan-cascade-parallel-source sa-src))))

(ert-deftest tibetan-comparison-inputs-and-ready ()
  "The inputs carry the Tibetan text + Wylie of the sentence, its
Section label and EVERY Sanskrit sentence of the same-titled Section
of the parallel source (not those of other Sections); without a
parallel source, or when the Section has no counterpart, the
comparison is not ready."
  (tibetan-comparison-test--with-corpus "de"
    (let ((in (tibetan-sanskrit-comparison--inputs file)))
      (should (equal 2 (plist-get in :sent-num)))
      (should (equal "de" (plist-get in :target)))
      (should (equal "Section I.1" (plist-get in :section-label)))
      (should (string-match-p "དོན་ཤེས་པས" (plist-get in :tibetan)))
      (should (string-match-p "don shes pas" (plist-get in :wylie)))
      (should (equal '(1 2) (mapcar #'car (plist-get in :sanskrit))))
      (should (string-match-p "arthajño" (cdr (assq 1 (plist-get in :sanskrit)))))
      (should (string-match-p "sattveṣu" (cdr (assq 1 (plist-get in :sanskrit)))))
      (should-not (string-match-p "pañca" (format "%S" (plist-get in :sanskrit))))
      (should (tibetan-sanskrit-comparison--ready-p in)))
    ;; Section Titel has no Sanskrit counterpart.
    (let ((f1 (tibetan-sentence--filepath 1 folder src)))
      (with-temp-file f1
        (insert (replace-regexp-in-string "Sentence 2" "Sentence 1"
                                          (tibetan-comparison-test--sent))))
      (should-not (tibetan-sanskrit-comparison--ready-p
                   (tibetan-sanskrit-comparison--inputs f1))))
    ;; No parallel keyword → not ready.
    (with-temp-file src
      (insert (replace-regexp-in-string "^#\\+TIBETAN_PARALLEL_SOURCE:.*\n" ""
                                        (tibetan-comparison-test--read src))))
    (should-not (tibetan-sanskrit-comparison--ready-p
                 (tibetan-sanskrit-comparison--inputs file)))))

(ert-deftest tibetan-comparison-prompt-contract ()
  "The user prompt carries Tibetan, Wylie and the numbered Sanskrit
sentences; the system prompt fixes the three `## ' sections and the
seminar language."
  (tibetan-comparison-test--with-corpus "de"
    (let* ((ps (tibetan-sanskrit-comparison--build-prompts
                (tibetan-sanskrit-comparison--inputs file)))
           (sys (car ps)) (user (cdr ps)))
      (should (string-match-p "## Correspondence" sys))
      (should (string-match-p "## Divergences" sys))
      (should (string-match-p "## Assessment" sys))
      (should (string-match-p "German" sys))
      (should (string-match-p "དོན་ཤེས་པས" user))
      (should (string-match-p "don shes pas" user))
      (should (string-match-p "\\[1\\] arthajño" user))
      (should (string-match-p "\\[2\\] ko 'laṃkaroti" user))
      (should (string-match-p "Section I.1" user)))
    (should (string-match-p "English"
                            (car (tibetan-sanskrit-comparison--build-prompts
                                  (plist-put (tibetan-sanskrit-comparison--inputs file)
                                             :target "en")))))))

(ert-deftest tibetan-comparison-counterpart-by-sentence-number ()
  "Abhidharmasamuccaya (19.10.): both cascades carry the SAME sentence
numbers.  When the parallel Section has a sentence of the Tibetan
sentence's number, the inputs name it (:counterpart) and the prompt
says so; the MSA case (no such sentence) has no :counterpart and the
prompt leaves the identification to Claude."
  (tibetan-comparison-test--with-corpus "en"
    ;; mdo Sentence 2 ↔ msa Section I.1 has Sentences 1 and 2 → counterpart 2.
    (let* ((in (tibetan-sanskrit-comparison--inputs file))
           (user (cdr (tibetan-sanskrit-comparison--build-prompts in))))
      (should (equal 2 (plist-get in :counterpart)))
      (should (string-match-p "\\[2\\] carries the same number" user)))
    ;; Renumber the Sanskrit sentences (11, 12): no counterpart.
    (with-temp-file sa-src
      (insert (replace-regexp-in-string
               "Sentence 2\\b" "Sentence 12"
               (replace-regexp-in-string "Sentence 1\\b" "Sentence 11"
                                         (tibetan-comparison-test--read sa-src)))))
    (let* ((in (tibetan-sanskrit-comparison--inputs file))
           (user (cdr (tibetan-sanskrit-comparison--build-prompts in))))
      (should-not (plist-get in :counterpart))
      (should (equal '(11 12) (mapcar #'car (plist-get in :sanskrit))))
      (should-not (string-match-p "carries the same number" user))
      (should (string-match-p "Identify the corresponding Sanskrit" user)))))

;; ----------------------------------------------------------------------------
;; Landing, reading, exclusion
;; ----------------------------------------------------------------------------

(ert-deftest tibetan-comparison-land-renders ()
  "Landing writes `** Sanskrit Comparison' under * Provided
Translations (before * Working Translation): correspondence, the
numbered points with labels in the document language, the
assessment; a second landing replaces, never duplicates."
  (tibetan-comparison-test--with-corpus "de"
    (should (tibetan-sanskrit-comparison--land file tibetan-comparison-test--response))
    (let ((s (tibetan-comparison-test--read file)))
      (should (< (string-match "^\\* Provided Translations$" s)
                 (string-match "^\\*\\* Sanskrit Comparison$" s)
                 (string-match "^\\* Working Translation$" s)))
      (should (string-match-p ":GENERATED: 20" s))
      (should (string-match-p "renders MSA I.1" s))
      (should (string-match-p "^\\*\\*\\* 1\\. don shes pa$" s))
      (should (string-match-p "^\\*\\*\\* 2\\. ched du$" s))
      (should (string-match-p "^- Tibetisch :: don shes pa$" s))
      (should (string-match-p "^- Sanskrit :: arthajñaḥ$" s))
      (should (string-match-p "^- Abweichung :: the Tibetan nominalises .* continued on a second line\\.$" s))
      (should (string-match-p "^- Bedeutung :: subject vs\\. epithet\\.$" s))
      (should (string-match-p "follows the Sanskrit closely" s)))
    (should (tibetan-sanskrit-comparison--land file tibetan-comparison-test--response))
    (should (= 1 (cl-count "** Sanskrit Comparison"
                           (split-string (tibetan-comparison-test--read file) "\n")
                           :test #'equal)))))

(ert-deftest tibetan-comparison-land-english-labels ()
  (tibetan-comparison-test--with-corpus "en"
    (tibetan-sanskrit-comparison--land file tibetan-comparison-test--response)
    (let ((s (tibetan-comparison-test--read file)))
      (should (string-match-p "^- Tibetan :: don shes pa$" s))
      (should (string-match-p "^- Divergence :: " s))
      (should (string-match-p "^- Significance :: " s)))))

(ert-deftest tibetan-comparison-read-roundtrip ()
  "`tibetan-sanskrit-comparison-read' returns the written comparison
as data with canonical keys; nil when absent."
  (tibetan-comparison-test--with-corpus "de"
    (should-not (tibetan-sanskrit-comparison-read file))
    (should-not (tibetan-sanskrit-comparison--present-p file))
    (tibetan-sanskrit-comparison--land file tibetan-comparison-test--response)
    (should (tibetan-sanskrit-comparison--present-p file))
    (let ((c (tibetan-sanskrit-comparison-read file)))
      (should (string-match-p "renders MSA I.1" (plist-get c :correspondence)))
      (should (string-match-p "follows the Sanskrit closely" (plist-get c :assessment)))
      (should (= 2 (length (plist-get c :points))))
      (let ((p (car (plist-get c :points))))
        (should (equal "don shes pa" (plist-get p :title)))
        (should (equal "arthajñaḥ" (cdr (assoc "Sanskrit" (plist-get p :items)))))
        (should (equal "subject vs. epithet."
                       (cdr (assoc "Significance" (plist-get p :items)))))))))

(ert-deftest tibetan-comparison-is-not-a-reference-translation ()
  "The discussion never treats the comparison as a reference
translation."
  (tibetan-comparison-test--with-corpus "en"
    (tibetan-sanskrit-comparison--land file tibetan-comparison-test--response)
    (should-not (assoc "Sanskrit Comparison" (tibetan-discussion--references file)))))

(ert-deftest tibetan-comparison-survives-regenerate ()
  "The comparison lives in * Provided Translations — the cascade
regenerate preserves it, exactly once."
  (tibetan-comparison-test--with-corpus "en"
    (tibetan-sanskrit-comparison--land file tibetan-comparison-test--response)
    (cl-letf (((symbol-function 'tibetan-analysis-generate-content)
               (lambda (&rest _) "")))
      (tibetan-cascade--regenerate
       file 2 (tibetan-cascade--segs-for-sentence src 2) src))
    (let ((s (tibetan-comparison-test--read file)))
      (should (= 1 (cl-count "** Sanskrit Comparison" (split-string s "\n")
                             :test #'equal)))
      (should (string-match-p "^\\*\\*\\* 2\\. ched du$" s)))))

;; ----------------------------------------------------------------------------
;; Firing
;; ----------------------------------------------------------------------------

(ert-deftest tibetan-comparison-request-gates ()
  "`--request' submits once (through the shared `--submit'), is a
no-op while in flight or when a comparison exists (unless FORCE),
and lands the comparison."
  (tibetan-comparison-test--with-corpus "en"
    (let ((added (not (featurep 'gptel)))
          (submits 0) (pending nil))
      (when added (provide 'gptel))
      (unwind-protect
          (cl-letf (((symbol-function 'tibetan-claude-queue-submit)
                     (lambda (job &rest _) (cl-incf submits) (setq pending job)))
                    ((symbol-function 'tibetan-analysis--ensure-gptel-ready)
                     (lambda () t))
                    ((symbol-function 'run-at-time)
                     (lambda (_s _r fn &rest args) (apply fn args)))
                    ((symbol-function 'gptel-request)
                     (lambda (_prompt &rest args)
                       (funcall (plist-get args :callback)
                                tibetan-comparison-test--response nil))))
            (should (tibetan-sanskrit-comparison--request file))
            (should-not (tibetan-sanskrit-comparison--request file)) ; in flight
            (should (= 1 submits))
            (funcall pending #'ignore)
            (should (tibetan-sanskrit-comparison--present-p file))
            (should-not (tibetan-sanskrit-comparison--request file)) ; present
            (should (tibetan-sanskrit-comparison--request file t))
            (should (= 2 submits)))
        (when added (setq features (delq 'gptel features)))))))

(ert-deftest tibetan-comparison-fetched-by-c-c-u-g ()
  "`tibetan-cascade-fetch' requests the comparison for every sentence
of the scope that has a parallel Section and no comparison yet —
also for sentences whose Claude/DM slots are already complete; never
without a parallel source."
  (tibetan-comparison-test--with-corpus "en"
    (let ((requested '()))
      (cl-letf (((symbol-function 'tibetan-cascade--fire-sentence)
                 (lambda (&rest _) nil))
                ((symbol-function 'tibetan-sanskrit-comparison--request)
                 (lambda (f &optional _force) (push (file-name-nondirectory f) requested) t)))
        (let ((r (tibetan-cascade-fetch (list :source src :sent-nums '(1 2)
                                              :label "alles"))))
          (should (equal '("sent-002-mdo.org") requested))
          (should (equal 1 (plist-get r :companion))))
        ;; Present → not requested again.
        (setq requested '())
        (tibetan-sanskrit-comparison--land file tibetan-comparison-test--response)
        (tibetan-cascade-fetch (list :source src :sent-nums '(2) :label "Satz 2"))
        (should-not requested)
        ;; Sanskrit source itself: no parallel → nothing.
        (tibetan-cascade-fetch (list :source sa-src :sent-nums '(1) :label "Satz 1"))
        (should-not requested)))))


(ert-deftest tibetan-comparison-after-land-hook ()
  (tibetan-comparison-test--with-corpus "en"
    (let ((landed '()))
      (let ((tibetan-sanskrit-comparison-after-land-functions
             (list (lambda (f) (push (file-name-nondirectory f) landed)))))
        (tibetan-sanskrit-comparison--land file tibetan-comparison-test--response))
      (should (equal '("sent-002-mdo.org") landed)))))

(ert-deftest tibetan-comparison-command-at-point ()
  "`tibetan-sanskrit-comparison-dwim' (re)requests the comparison for
the sentence at point / every sentence under the heading."
  (tibetan-comparison-test--with-corpus "en"
    (let ((requested '()))
      (cl-letf (((symbol-function 'tibetan-sanskrit-comparison--request)
                 (lambda (f &optional force)
                   (push (cons (file-name-nondirectory f) force) requested) t)))
        (with-current-buffer (find-file-noselect src)
          (goto-char (point-min))
          (re-search-forward "^\\*\\* Section I.1")
          (tibetan-sanskrit-comparison-dwim)
          (should (equal '(("sent-002-mdo.org" . t)) requested)))))))

(provide 'tibetan-sanskrit-comparison-test)
;;; tibetan-sanskrit-comparison-test.el ends here
