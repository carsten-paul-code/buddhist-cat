;;; tibetan-zettel-highlight-test.el --- Tests for core/tibetan-zettel-highlight.el -*- lexical-binding: t -*-

;;; Commentary:
;;
;; Words that have a zettel are highlighted and linked (2026-10-18).  The
;; index is a JSON fixture (no real zettelkasten); the matchers are called
;; directly (`font-lock-ensure' is unreliable in batch — §5.31); the
;; sentence-file resolver, `shared-route-follow', `find-file' and
;; `org-return' are stubbed.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'org)

(let ((dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../core" dir))
  (add-to-list 'load-path (expand-file-name "../analysis" dir))
  (add-to-list 'load-path (expand-file-name "../persist" dir)))

(require 'tibetan-zettel-highlight nil t)

;; ============================================================================
;; Fixtures
;; ============================================================================

(defconst tzh-test--hrdaya "20261012T123036")
(defconst tzh-test--anuttara "20261012T123035")
(defconst tzh-test--kula "20261012T123039")
(defconst tzh-test--sunya-b "20250824T091030")
(defconst tzh-test--sunya-t "20261016T132711")
(defconst tzh-test--stong "20250101T000010")

(defconst tzh-test--index-json
  (format "{\"generated\": \"2026-10-18T10:00:00\", \"zk_dir\": \"/x/zk\", \"count\": 6, \"entries\": [
{\"id\": \"%s\", \"file\": \"%s--hrdaya__type-concept.org\", \"title\": \"hṛdaya\", \"kind\": \"concept\", \"stub\": false,
 \"sanskrit\": \"hṛdaya\", \"wylie\": \"\", \"tibetan\": \"\", \"german\": \"Herz\", \"english\": \"heart\", \"etymology\": \"hṛd (heart; primary noun) + -aya (noun suffix)\",
 \"aliases\": [], \"tags\": [\"type_concept\"], \"schemes\": [], \"keys\": [\"hrdaya\"]},
{\"id\": \"%s\", \"file\": \"%s--anuttara__type-concept.org\", \"title\": \"anuttara\", \"kind\": \"concept\", \"stub\": false,
 \"sanskrit\": \"anuttara\", \"wylie\": \"\", \"tibetan\": \"\", \"german\": \"das Unübertreffliche\", \"english\": \"\", \"etymology\": \"an- (not) + uttara (higher; comparative of ud- up)\",
 \"aliases\": [], \"tags\": [\"type_concept\"], \"schemes\": [], \"keys\": [\"anuttara\"]},
{\"id\": \"%s\", \"file\": \"%s--kula__type-concept.org\", \"title\": \"kula\", \"kind\": \"concept\", \"stub\": false,
 \"sanskrit\": \"kula\", \"wylie\": \"\", \"tibetan\": \"\", \"german\": \"Gesamtheit\", \"english\": \"\", \"etymology\": \"\",
 \"aliases\": [], \"tags\": [\"type_concept\"], \"schemes\": [], \"keys\": [\"kula\"]},
{\"id\": \"%s\", \"file\": \"%s--sunya__type-concept.org\", \"title\": \"śūnya\", \"kind\": \"concept\", \"stub\": false,
 \"sanskrit\": \"śūnya\", \"wylie\": \"\", \"tibetan\": \"\", \"german\": \"leer\", \"english\": \"empty\", \"etymology\": \"\",
 \"aliases\": [], \"tags\": [\"type_concept\"], \"schemes\": [], \"keys\": [\"sunya\"]},
{\"id\": \"%s\", \"file\": \"%s--sunya-trika__type-concept.org\", \"title\": \"śūnya (Trika)\", \"kind\": \"concept\", \"stub\": false,
 \"sanskrit\": \"śūnya\", \"wylie\": \"\", \"tibetan\": \"\", \"german\": \"Leere\", \"english\": \"void\", \"etymology\": \"\",
 \"aliases\": [], \"tags\": [\"type_concept\", \"tradition_saiva\"], \"schemes\": [], \"keys\": [\"sunya\"]},
{\"id\": \"%s\", \"file\": \"%s--stong-pa-nyid__type-concept.org\", \"title\": \"stong pa nyid\", \"kind\": \"concept\", \"stub\": true,
 \"sanskrit\": \"śūnyatā\", \"wylie\": \"stong pa nyid\", \"tibetan\": \"སྟོང་པ་ཉིད\", \"german\": \"Leerheit\", \"english\": \"\", \"etymology\": \"\",
 \"aliases\": [], \"tags\": [\"type_concept\"], \"schemes\": [], \"keys\": [\"sunyata\", \"stongpanyid\", \"སྟོངཔཉིད\"]}]}"
          tzh-test--hrdaya tzh-test--hrdaya tzh-test--anuttara tzh-test--anuttara
          tzh-test--kula tzh-test--kula tzh-test--sunya-b tzh-test--sunya-b
          tzh-test--sunya-t tzh-test--sunya-t tzh-test--stong tzh-test--stong))

(defconst tzh-test--word-analysis-body
  "*** Segment 2

tad-ubhaya-yāmala-sphurita-bhāva-visarga-mayam hṛdayam anuttara-amṛta-kulam mama sasphuratāt

- hṛdayam — hṛdaya; N.NOM.SG.N
- anuttara-amṛta-kulam — anuttarāmṛtakulam; N.NOM.SG.N
- kulam — kula; N.NOM.SG.N
- mama — asmad; PRON.GEN.SG
- sasphuratāt — √sphur; V.IMP.3SG
")

(defconst tzh-test--analysis-text
  (concat "#+TITLE: Sentence 1 Analysis
#+TIBETAN_LAYOUT: cascade
#+SOURCE_LANG: sa

* Reading
** Gloss Tables
:PROPERTIES:
:GENERATED_HASH: abc
:END:
*** Segment 2
| hṛdayam    | anuttara-amṛta-kulam | mama        |
| heart      | the body of the…     | my          |
| N.NOM.SG.N | N.NOM.SG.N           | PRON.GEN.SG |
- ⟦2⟧ may my Heart vibrate

* Sanskrit Analysis
** Claude Vocabulary
*** Segment 2
hṛdayam, noun neuter, \"heart\", the Heart as absolute consciousness

mama, pronoun, \"my\"

** Word Analysis
" tzh-test--word-analysis-body "
* Footnotes
"))

(defconst tzh-test--source-text
  "#+TITLE: Tantrāloka, Kapitel 1
#+TIBETAN_LAYOUT: cascade
#+SOURCE_LANG: sa

* Einleitende Verse
** Section The Initial Invocation
*** Sentence 1
:PROPERTIES:
:VERSE: TĀ 1.1
:END:
**** Segment 1
vimalakalāśrayābhinavasṛṣṭimahā jananī bharitatanuśca pañcamukhaguptarucirjanakaḥ /
**** Segment 2
tadubhayayāmalasphuritabhāvavisargamayaṃ hṛdayamanuttarāmṛtakulaṃ mama sasphuratāt // 1
**** Working Translation
hṛdaya kula anuttara — these words here must NOT match
")

(defvar tzh-test--index-file nil)

(defmacro tzh-test--with-index (&rest body)
  "Run BODY with the fixture index written to a temp file and loaded."
  (declare (indent 0))
  `(let ((tzh-test--index-file (make-temp-file "tzh-index-" nil ".json")))
     (unwind-protect
         (progn
           (with-temp-file tzh-test--index-file (insert tzh-test--index-json))
           (let ((tibetan-zettel-highlight-index-file tzh-test--index-file)
                 (tibetan-zettel-highlight--index nil)
                 (tibetan-zettel-highlight--entries nil)
                 (tibetan-zettel-highlight--index-mtime nil))
             (tibetan-zettel-highlight--ensure-index)
             ,@body))
       (delete-file tzh-test--index-file))))

(defun tzh-test--collect-matches (matcher)
  "Call MATCHER from point-min until it returns nil; list of (TEXT . IDS)."
  (goto-char (point-min))
  (let ((limit (point-max)) (guard 0) out)
    (while (and (< guard 200) (funcall matcher limit))
      (cl-incf guard)
      (push (cons (buffer-substring-no-properties (match-beginning 0) (match-end 0))
                  tibetan-zettel-highlight--current-ids)
            out))
    (should (< guard 200))
    (nreverse out)))

;; ============================================================================
;; Normalisation — parity with skos_core.norm_lemma / skos_index.norm_tibetan
;; ============================================================================

(ert-deftest tzh-norm-latin-matches-python-norm-lemma ()
  (should (equal (tibetan-zettel-highlight--norm-latin "śūnyatā") "sunyata"))
  (should (equal (tibetan-zettel-highlight--norm-latin "Hṛdaya") "hrdaya"))
  (should (equal (tibetan-zettel-highlight--norm-latin "stong pa nyid") "stongpanyid"))
  (should (equal (tibetan-zettel-highlight--norm-latin "anuttara-amṛta") "anuttaraamrta"))
  (should (equal (tibetan-zettel-highlight--norm-latin "'pi") "pi"))
  (should (equal (tibetan-zettel-highlight--norm-latin "") "")))

(ert-deftest tzh-norm-tibetan-drops-tsheg-and-shad-keeps-vowels ()
  (should (equal (tibetan-zettel-highlight--norm-tibetan "སྟོང་པ་ཉིད།") "སྟོངཔཉིད"))
  (should (equal (tibetan-zettel-highlight--norm-tibetan "ལྷུན་ གྲུབ") "ལྷུནགྲུབ")))

(ert-deftest tzh-norm-key-picks-script ()
  (should (equal (tibetan-zettel-highlight--norm-key "Śūnya") "sunya"))
  (should (equal (tibetan-zettel-highlight--norm-key "སྟོང་པ་ཉིད") "སྟོངཔཉིད")))

;; ============================================================================
;; Index
;; ============================================================================

(ert-deftest tzh-index-loads-and-resolves-keys ()
  (tzh-test--with-index
    (should (equal (tibetan-zettel-highlight-lookup "anuttara") (list tzh-test--anuttara)))
    (should (equal (sort (copy-sequence (tibetan-zettel-highlight-lookup "sunya")) #'string<)
                   (sort (list tzh-test--sunya-b tzh-test--sunya-t) #'string<)))
    (should (equal (tibetan-zettel-highlight-lookup "stongpanyid") (list tzh-test--stong)))
    (should-not (tibetan-zettel-highlight-lookup "mama"))
    (should (equal (plist-get (tibetan-zettel-highlight-entry tzh-test--hrdaya) :title) "hṛdaya"))
    (should (equal (tibetan-zettel-highlight--entry-path tzh-test--hrdaya)
                   (format "/x/zk/%s--hrdaya__type-concept.org" tzh-test--hrdaya)))))

(ert-deftest tzh-ids-for-term-normalises-and-splits-compound-members ()
  (tzh-test--with-index
    (should (equal (tibetan-zettel-highlight--ids-for-term "Anuttara") (list tzh-test--anuttara)))
    ;; members of a hyphenated compound: (OFFSET LENGTH IDS) per hit
    (should (equal (tibetan-zettel-highlight--member-hits "anuttara-amṛta-kulam" nil)
                   (list (list 0 8 (list tzh-test--anuttara)))))
    ;; with a lemma map the inflected member resolves through its lemma
    (let ((map (make-hash-table :test 'equal)))
      (puthash "kulam" "kula" map)
      (should (equal (tibetan-zettel-highlight--member-hits "anuttara-amṛta-kulam" map)
                     (list (list 0 8 (list tzh-test--anuttara))
                           (list 15 5 (list tzh-test--kula))))))))

;; ============================================================================
;; Buffer kinds
;; ============================================================================

(ert-deftest tzh-buffer-kind-analysis-before-source ()
  (with-temp-buffer
    (insert tzh-test--analysis-text)
    ;; the analysis file carries the same headers as the source: name wins
    (should (eq (tibetan-zettel-highlight--buffer-kind "/x/WS/analysis/sent-001-tantralo.org") 'analysis))
    (should (eq (tibetan-zettel-highlight--buffer-kind "/x/WS/analysis/seg-013-kbgp.org") 'analysis)))
  (with-temp-buffer
    (insert tzh-test--source-text)
    (should (eq (tibetan-zettel-highlight--buffer-kind "/x/WS/tantraloka-1.org") 'source)))
  (with-temp-buffer
    (insert "#+TITLE: notes\n* Plain org\n")
    (should-not (tibetan-zettel-highlight--buffer-kind "/x/notes.org"))
    (should-not (tibetan-zettel-highlight--buffer-kind nil))))

;; ============================================================================
;; Analysis buffers
;; ============================================================================

(ert-deftest tzh-lemma-map-from-word-analysis ()
  (with-temp-buffer
    (insert tzh-test--analysis-text)
    (org-mode)
    (let ((map (tibetan-zettel-highlight--lemma-map)))
      (should (equal (gethash "hṛdayam" map) "hṛdaya"))
      (should (equal (gethash "kulam" map) "kula"))
      (should (equal (gethash "sasphuratāt" map) "√sphur"))
      (should-not (gethash "heart" map)))))

(ert-deftest tzh-analysis-matcher-hits-lemmas-row1-and-headwords-only ()
  (tzh-test--with-index
    (with-temp-buffer
      (insert tzh-test--analysis-text)
      (org-mode)
      (setq buffer-file-name nil)
      (let ((hits (tzh-test--collect-matches #'tibetan-zettel-highlight--analysis-matcher)))
        ;; Gloss Tables row 1: hṛdayam (via lemma map), anuttara (member), kulam (member via map)
        ;; Claude Vocabulary: headword hṛdayam
        ;; Word Analysis: lemma hṛdaya, member anuttara (of the lemma? no — lemma is fused), kula
        (should (equal (mapcar #'car hits)
                       '("hṛdayam" "anuttara" "kulam" "hṛdayam" "hṛdaya" "kula")))
        (should (equal (cdr (nth 0 hits)) (list tzh-test--hrdaya)))
        (should (equal (cdr (nth 2 hits)) (list tzh-test--kula)))
        ;; rows 2-3 of the table, the ⟦2⟧ line and the pronoun never match
        (should-not (cl-find "heart" hits :key #'car))
        (should-not (cl-find "mama" hits :key #'car))))))

(ert-deftest tzh-matcher-makes-progress-on-final-line-without-newline ()
  (tzh-test--with-index
    (with-temp-buffer
      (insert "* Reading\n** Gloss Tables\n| mama | asmad |")   ; no trailing newline, no hits
      (org-mode)
      (goto-char (point-min))
      (let ((calls 0) result)
        (setq result (tibetan-zettel-highlight--analysis-matcher (point-max)))
        (should-not result)
        (should (= (point) (point-max)))
        (ignore calls)))))

;; ============================================================================
;; Source buffers — projection from the sentence's Word Analysis
;; ============================================================================

(ert-deftest tzh-projection-needles-from-word-analysis ()
  (tzh-test--with-index
    (let ((needles (tibetan-zettel-highlight--projection-from-body tzh-test--word-analysis-body)))
      ;; (NEEDLE . IDS): surface member minus first and last char
      (should (member (cons "ṛdaya" (list tzh-test--hrdaya)) needles))
      (should (member (cons "nuttar" (list tzh-test--anuttara)) needles))
      (should (member (cons "ula" (list tzh-test--kula)) needles))
      (should-not (cl-find "am" needles :key #'car))       ; mama → asmad: no zettel
      (should-not (cl-find "asphurat" needles :key #'car)))))

(ert-deftest tzh-source-matcher-marks-fused-words-of-analysed-sentences-only ()
  (tzh-test--with-index
    (cl-letf (((symbol-function 'tibetan-zettel-highlight--sentence-word-analysis)
               (lambda (sent-num) (and (= sent-num 1) tzh-test--word-analysis-body))))
      (with-temp-buffer
        (insert tzh-test--source-text)
        (org-mode)
        (let ((hits (tzh-test--collect-matches #'tibetan-zettel-highlight--source-matcher)))
          (should (equal (mapcar #'car hits) '("hṛdayam" "anuttarā" "kulaṃ")))
          (should (equal (cdr (nth 1 hits)) (list tzh-test--anuttara)))
          ;; the Working Translation sibling is not a segment body
          (should-not (cl-find "hṛdaya" hits :key #'car)))))))

(ert-deftest tzh-source-matcher-without-word-analysis-marks-nothing ()
  (tzh-test--with-index
    (cl-letf (((symbol-function 'tibetan-zettel-highlight--sentence-word-analysis) (lambda (_) nil)))
      (with-temp-buffer
        (insert tzh-test--source-text)
        (org-mode)
        (should-not (tzh-test--collect-matches #'tibetan-zettel-highlight--source-matcher))))))

;; ============================================================================
;; Mode, properties, following
;; ============================================================================

(ert-deftest tzh-facespec-carries-id-keymap-and-is-managed ()
  (tzh-test--with-index
    (with-temp-buffer
      (insert tzh-test--analysis-text)
      (org-mode)
      (tibetan-zettel-highlight--enable 'analysis)
      (should (memq 'tibetan-zettel-id font-lock-extra-managed-props))
      (should (memq 'keymap font-lock-extra-managed-props))
      (let ((tibetan-zettel-highlight--current-ids (list tzh-test--hrdaya)))
        (let ((spec (tibetan-zettel-highlight--facespec)))
          (should (eq (plist-get spec 'face) 'tibetan-zettel-highlight-face))
          (should (equal (plist-get spec 'tibetan-zettel-id) (list tzh-test--hrdaya)))
          (should (keymapp (plist-get spec 'keymap)))
          (should (string-match-p "hṛdaya" (plist-get spec 'help-echo)))))
      (should (memq 'tibetan-zettel-highlight--analysis-matcher (flatten-tree font-lock-keywords)))
      (tibetan-zettel-highlight--disable)
      (should-not (memq 'tibetan-zettel-highlight--analysis-matcher (flatten-tree font-lock-keywords))))))

(ert-deftest tzh-open-routes-to-research-when-available-else-find-file ()
  (tzh-test--with-index
    (let (routed opened)
      (cl-letf (((symbol-function 'shared-route-follow) (lambda (id) (setq routed id)))
                ((symbol-function 'denote-get-path-by-id) (lambda (_) "/x"))
                ((symbol-function 'find-file) (lambda (f) (setq opened f))))
        (tibetan-zettel-highlight--open-id tzh-test--hrdaya)
        (should (equal routed tzh-test--hrdaya))
        (should-not opened))
      (setq routed nil)
      (cl-letf (((symbol-function 'find-file) (lambda (f) (setq opened f)))
                ((symbol-function 'tibetan-zettel-highlight--research-reachable-p) (lambda () nil)))
        (fmakunbound 'shared-route-follow)
        (tibetan-zettel-highlight--open-id tzh-test--hrdaya)
        (should (equal opened (format "/x/zk/%s--hrdaya__type-concept.org" tzh-test--hrdaya)))))))

(ert-deftest tzh-return-opens-only-on-a-marked-word ()
  (tzh-test--with-index
    (let (opened returned)
      (cl-letf (((symbol-function 'tibetan-zettel-highlight-open) (lambda (&optional ids) (setq opened ids)))
                ((symbol-function 'org-return) (lambda (&rest _) (setq returned t))))
        (with-temp-buffer
          (insert "plain hṛdaya text")
          (org-mode)
          (put-text-property 7 13 'tibetan-zettel-id (list tzh-test--hrdaya))
          (goto-char 9)
          (tibetan-zettel-highlight-return)
          (should (equal opened (list tzh-test--hrdaya)))
          (should-not returned)
          (goto-char 2)
          (tibetan-zettel-highlight-return)
          (should returned))))))

(ert-deftest tzh-choose-among-homonyms-uses-titles ()
  (tzh-test--with-index
    (let (prompted)
      (cl-letf (((symbol-function 'completing-read)
                 (lambda (_prompt coll &rest _) (setq prompted coll) (car coll))))
        (should (equal (tibetan-zettel-highlight--choose-id (list tzh-test--sunya-b tzh-test--sunya-t))
                       tzh-test--sunya-b))
        (should (member "śūnya (Trika)" prompted)))
      (should (equal (tibetan-zettel-highlight--choose-id (list tzh-test--kula)) tzh-test--kula)))))

(ert-deftest tzh-entry-and-tooltip-carry-the-wortbildung ()
  (tzh-test--with-index
    (should (equal (plist-get (tibetan-zettel-highlight-entry tzh-test--hrdaya) :etymology)
                   "hṛd (heart; primary noun) + -aya (noun suffix)"))
    (let ((echo (tibetan-zettel-highlight--help-echo (list tzh-test--hrdaya))))
      (should (string-match-p "hṛdaya — Herz / heart" echo))
      (should (string-match-p "\nWortbildung: hṛd (heart; primary noun) \\+ -aya (noun suffix)" echo)))
    ;; no Wortbildung in the zettel: no line
    (should-not (string-match-p "Wortbildung" (tibetan-zettel-highlight--help-echo (list tzh-test--kula))))))

(ert-deftest tzh-etymology-command-shows-the-wortbildung-of-the-marked-word ()
  (tzh-test--with-index
    (let (said)
      (cl-letf (((symbol-function 'message) (lambda (fmt &rest args) (setq said (apply #'format fmt args)))))
        (with-temp-buffer
          (insert "plain hṛdaya kula")
          (put-text-property 7 13 'tibetan-zettel-id (list tzh-test--hrdaya))
          (put-text-property 14 18 'tibetan-zettel-id (list tzh-test--kula))
          (goto-char 9)
          (tibetan-zettel-highlight-etymology)
          (should (equal said "hṛdaya — Herz / heart — hṛd (heart; primary noun) + -aya (noun suffix)"))
          ;; C-u puts the same text on the kill ring
          (tibetan-zettel-highlight-etymology t)
          (should (equal (current-kill 0) said))
          ;; a zettel without Wortbildung says so
          (goto-char 15)
          (tibetan-zettel-highlight-etymology)
          (should (equal said "kula — Gesamtheit — keine Wortbildung im Zettel"))
          ;; no zettel at point: a user-error pointing at /term
          (goto-char 2)
          (should-error (tibetan-zettel-highlight-etymology) :type 'user-error))))))

(provide 'tibetan-zettel-highlight-test)
;;; tibetan-zettel-highlight-test.el ends here
