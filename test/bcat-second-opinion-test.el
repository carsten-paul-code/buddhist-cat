;;; bcat-second-opinion-test.el --- Tests for the second opinion  -*- lexical-binding: t -*-

;;; Commentary:
;; Carsten 27.11.2026: „Zweite Runde mit Mehrdeutigkeitsfeld und Befehl
;; „Fable fragen“ bauen“ — the analysis (Opus 5.5) names the places where
;; the original admits more than one analysis; Fable 5.2 gives a second
;; opinion on them, written under Translations compared.  Then, after the
;; run on TĀ 1012 (Fable agreed with Opus with a false argument, though on
;; its own it had read the place the other way): „Blind fragen und Stelle
;; straffen“ — Fable gets the places without the analysis' choice and
;; reason; the review of the whole analysis is C-c u Z.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'bcat-second-opinion)

(defconst bcat-second-opinion-test--line
  "- /kāryamṛta/ (⟦695⟧): /kārya-mṛta/ ‘dead products’ · /kāri-amṛta/ ‘nectars that produce’ (highly unlikely given context) — chosen: /kārya-mṛta/ ‘dead products’. The commentary contrasts the lasting with the perishable."
  "An ambiguity as the analysis names it.")

(defconst bcat-second-opinion-test--file
  (concat "#+TITLE: Sentence 1012 Analysis
#+CAT_TEMPLATE: sanskrit
#+SOURCE: [[file:../tantraloka-1.org::*Sentence 1012][tantraloka-1.org / Sentence 1012]]

* Text
*Ort im Ganzen:* TĀ 1.1 › Deutung 1 › Herz

#+begin_verse
anuttaraṃ katipayakāladārḍhyakāryamṛtāntaravailakṣaṇyād utkṛṣṭaṃ ca tat
#+end_verse

*Proposed translation:*
#+begin_quote
The Unsurpassable, most excellent.
#+end_quote

* Philology
** ⟦695⟧
*anuttaram katipaya-kāla-dārḍhya-kārya-mṛta-antara-vailakṣaṇyāt utkṛṣṭam ca tat*

** Translations compared
*Translation:* The Unsurpassable, by contrast with dead products …

*Dyczkowski 2023:* (By) ‘Absolute’ is meant what is most excellent …

*** Discussion points
None: the translations agree in substance.

*** Ambiguities
" bcat-second-opinion-test--line "

*** DharmaMitra (machine translation, for completeness)
*DharmaMitra:* The unsurpassed transient …

** Segment 695
*Sandhi:* …

* Worum es geht
Text.

* My Notes
Meine Notiz.
")
  "A sentence file with one ambiguity.")

(defconst bcat-second-opinion-test--answer
  "{\"opinions\": [{\"place\": 1, \"choice\": 1, \"reading\": \"\", \"reason\": \"/dārḍhya/ is the firmness the elixirs give.\"}]}"
  "Fable's blind answer: the first of the readings as listed (sorted).")

(defconst bcat-second-opinion-test--review
  "{\"points\": [{\"passage\": \"/avidyamānam/\", \"segment\": 698, \"point\": \"A present passive participle, not a ppp.\"}]}"
  "Fable's review of the analysis.")

(defmacro bcat-second-opinion-test--with-file (var &rest body)
  "Run BODY with VAR bound to a sentence file outside the temp folder."
  (declare (indent 1))
  `(let* ((dir (make-temp-file "bcat-second-" t))
          (temporary-file-directory (file-name-as-directory (make-temp-file "bcat-other-tmp-" t)))
          (,var (expand-file-name "sent-1012-tantralo.org" (expand-file-name "analysis" dir))))
     (unwind-protect
         (progn
           (make-directory (file-name-directory ,var) t)
           (with-temp-file ,var (insert bcat-second-opinion-test--file))
           (clrhash bcat-second-opinion--pending)
           ,@body)
       (delete-directory dir t)
       (delete-directory temporary-file-directory t))))

(defun bcat-second-opinion-test--without-ambiguities (file)
  "Write FILE without its *** Ambiguities."
  (with-temp-file file
    (insert (replace-regexp-in-string "\\*\\*\\* Ambiguities\n- [^\n]*\n\n" "" bcat-second-opinion-test--file))))

(defun bcat-second-opinion-test--read (file)
  "The text of FILE."
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(ert-deftest bcat-second-opinion-reads-the-ambiguities ()
  "The places the analysis named are read from its *** Ambiguities
\(German: Mehrdeutigkeiten) under Translations compared; none, nil."
  (should (equal (list bcat-second-opinion-test--line)
                 (bcat-second-opinion-ambiguities-in bcat-second-opinion-test--file)))
  (should (equal '("- /x/ (⟦1⟧): a · b")
                 (bcat-second-opinion-ambiguities-in
                  "** Translations compared\n*** Mehrdeutigkeiten\n- /x/ (⟦1⟧): a · b\n\n** Segment 1\n")))
  (should-not (bcat-second-opinion-ambiguities-in "** Translations compared\n*Translation:* T\n")))

(ert-deftest bcat-second-opinion-place-is-blind ()
  "A place as the second opinion is given it: the passage, its segment,
the readings in no particular order (sorted — the analysis lists its
choice first) and without a judgement in brackets; the analysis' choice
only as the number of the reading, for the verdict after the answer —
read word for word, else by the form of the original."
  (let ((p (bcat-second-opinion--place bcat-second-opinion-test--line)))
    (should (equal "/kāryamṛta/" (plist-get p :passage)))
    (should (equal 695 (plist-get p :segment)))
    (should (equal '("/kāri-amṛta/ ‘nectars that produce’" "/kārya-mṛta/ ‘dead products’")
                   (plist-get p :readings)))
    (should (equal 2 (plist-get p :chosen))))
  ;; chosen not word for word: the form of the original decides, when it
  ;; names one reading only
  (should (equal 1 (plist-get (bcat-second-opinion--place
                               "- /x/ (⟦3⟧): /a-b/ ‘one’ · /ab/ ‘two’ — chosen: /a-b/, since …")
                              :chosen)))
  ;; German, no segment, no choice that names a reading
  (let ((p (bcat-second-opinion--place "- /x/: /p/ als Subjekt · /q/ als Objekt — gewählt: Subjekt. Weil …")))
    (should (equal "/x/" (plist-get p :passage)))
    (should-not (plist-get p :segment))
    (should (equal '("/p/ als Subjekt" "/q/ als Objekt") (plist-get p :readings)))
    (should-not (plist-get p :chosen)))
  (should-not (bcat-second-opinion--place "- no readings here")))

(ert-deftest bcat-second-opinion-prompts-are-blind ()
  "The request on the places: the original, the place in the work, the
published translations, the places with their readings — never the
analysis: not its choice, its reason, its word division, its
translation, the machine translation or the user's notes."
  (bcat-second-opinion-test--with-file file
    (cl-letf (((symbol-function 'bcat-second-opinion--language) (lambda (_f) "English")))
      (let* ((places (bcat-second-opinion-places (bcat-second-opinion-test--read file)))
             (ps (bcat-second-opinion-prompts file places))
             (user (cdr ps)))
        (should (string-match-p "second opinion" (car ps)))
        (should (string-match-p "no particular order" (car ps)))
        (should (string-match-p "\"choice\"" (car ps)))
        (should (string-match-p "reason in English" (car ps)))
        ;; the run on TĀ 1012: „reading 1 would require …“ — in the file
        ;; the readings stand in the analysis' order, the numbers not at all
        (should (string-match-p "name a reading by its form, never by its number" (car ps)))
        (should (string-match-p "katipayakāladārḍhyakāryamṛtāntaravailakṣaṇyād" user))
        (should (string-match-p "TĀ 1\\.1 › Deutung 1 › Herz" user))
        (should (string-match-p "Dyczkowski 2023: (By) ‘Absolute’" user))
        (should (string-match-p (concat "Place 1: /kāryamṛta/ (⟦695⟧)\n"
                                        "  1) /kāri-amṛta/ ‘nectars that produce’\n"
                                        "  2) /kārya-mṛta/ ‘dead products’")
                                user))
        (dolist (seen '("chosen" "contrasts the lasting" "highly unlikely" "kārya-mṛta-antara"
                        "by contrast with dead products" "The Unsurpassable, most excellent"
                        "DharmaMitra" "transient" "Meine Notiz"))
          (should-not (string-match-p (regexp-quote seen) user)))))))

(ert-deftest bcat-second-opinion-prompts-take-the-segments-of-the-source ()
  "With its source at hand the original goes segment by segment, with the
document's context and the sentences before and after — as the analysis
had them."
  (bcat-second-opinion-test--with-file file
    (cl-letf (((symbol-function 'bcat-second-opinion--language) (lambda (_f) "English"))
              ((symbol-function 'tibetan-sentence--source-file-from-analysis)
               (lambda (_f) "/nonexistent/tantraloka-1.org"))
              ((symbol-function 'bcat-sentence--sentence)
               (lambda (n _src) (and (= n 1012) (list :sent-num 1012 :segs '((695 . " anuttaraṃ katipaya ")
                                                                            (696 . "yatrāsti na bhayaṃ"))))))
              ((symbol-function 'bcat-sentence--neighbours-block)
               (lambda (_s _src) "Before: the verse 1.1."))
              ((symbol-function 'tibetan-analysis--read-source-metadata)
               (lambda (_src) '(:claude-context ("Tantrāloka with Jayaratha's Viveka.")))))
      (let ((user (cdr (bcat-second-opinion-prompts
                        file (bcat-second-opinion-places (bcat-second-opinion-test--read file))))))
        (should (string-match-p "⟦695⟧ anuttaraṃ katipaya\n⟦696⟧ yatrāsti na bhayaṃ" user))
        (should (string-match-p "Tantrāloka with Jayaratha's Viveka\\." user))
        (should (string-match-p "Before: the verse 1\\.1\\." user))
        (should-not (string-match-p "katipayakāladārḍhya" user))))))

(ert-deftest bcat-second-opinion-review-prompts ()
  "The review (C-c u Z): the analysis as plain lines, without an earlier
answer of the second model and without the user's notes; the passage of
a point only the word or compound."
  (bcat-second-opinion-test--with-file file
    (cl-letf (((symbol-function 'bcat-second-opinion--language) (lambda (_f) "English"))
              ((symbol-function 'bcat-second-opinion--language-code) (lambda (_f) "en")))
      (bcat-second-opinion--land-review file bcat-second-opinion-test--review "claude-fable-5-2")
      (let ((ps (bcat-second-opinion-review-prompts file)))
        (should (string-match-p "\"points\"" (car ps)))
        (should (string-match-p "only the word or compound the point concerns" (car ps)))
        (should (string-match-p "never a whole segment or line" (car ps)))
        (should (string-match-p "point in English" (car ps)))
        (should (string-match-p "kārya-mṛta-antara" (cdr ps)))
        (should (string-match-p "contrasts the lasting" (cdr ps)))
        (should-not (string-match-p "present passive participle" (cdr ps)))
        (should-not (string-match-p "Meine Notiz" (cdr ps)))))))

(ert-deftest bcat-second-opinion-lands-blind ()
  "The blind answer stands as *** Second opinion (Fable 5.2, blind) after
the ambiguities, before the machine translation: per place the reading
Fable chose and whether the analysis chose it too; asked again, it
replaces the earlier one; the user's sections stay."
  (bcat-second-opinion-test--with-file file
    (cl-letf (((symbol-function 'bcat-second-opinion--language-code) (lambda (_f) "en")))
      (let ((places (bcat-second-opinion-places (bcat-second-opinion-test--read file))))
        (bcat-second-opinion--land file bcat-second-opinion-test--answer "claude-fable-5-2" places)
        (let ((text (bcat-second-opinion-test--read file)))
          (should (string-match-p (concat "\\*\\*\\* Ambiguities\n- [^\n]*\n\n"
                                          "\\*\\*\\* Second opinion (Fable 5\\.2, blind)\n"
                                          "- /kāryamṛta/ (⟦695⟧): /kāri-amṛta/ ‘nectars that produce’ — differs from the analysis\\. "
                                          "/dārḍhya/ is the firmness the elixirs give\\.\n\n"
                                          "\\*\\*\\* DharmaMitra")
                                  text))
          (should (string-match-p "Meine Notiz" text)))
        (bcat-second-opinion--land file "{\"opinions\": [{\"place\": 1, \"choice\": 2, \"reading\": \"\", \"reason\": \"Fine.\"}]}"
                                   "claude-fable-5-2" places)
        (let ((text (bcat-second-opinion-test--read file)))
          (should (= 1 (cl-count-if (lambda (l) (string-prefix-p "*** Second opinion" l))
                                    (split-string text "\n"))))
          (should (string-match-p "(⟦695⟧): /kārya-mṛta/ ‘dead products’ — as the analysis\\. Fine\\." text))
          (should-not (string-match-p "elixirs" text)))))))

(ert-deftest bcat-second-opinion-block-verdicts ()
  "Fable's choice against the analysis': the same reading, another one of
the list, one not listed, the place left open; the analysis' choice not
known — the reading without a verdict; in German."
  (let* ((place (list :passage "/x/" :segment 3 :readings '("/a/ one" "/b/ two") :chosen 1))
         (block (lambda (opinion &optional code chosen)
                  (bcat-second-opinion--block
                   `((opinions . [,opinion])) "claude-fable-5-2" (or code "en")
                   (list (plist-put (copy-sequence place) :chosen (if chosen (car chosen) 1)))))))
    (should (string-match-p "- /x/ (⟦3⟧): /a/ one — as the analysis\\. R\\."
                            (funcall block '((place . 1) (choice . 1) (reading . "") (reason . "R")))))
    (should (string-match-p "- /x/ (⟦3⟧): /b/ two — differs from the analysis\\. R\\."
                            (funcall block '((place . 1) (choice . 2) (reading . "") (reason . "R")))))
    (should (string-match-p "- /x/ (⟦3⟧): /c/ three — differs from the analysis\\. R\\."
                            (funcall block '((place . 1) (choice . 0) (reading . "/c/ three") (reason . "R")))))
    (should (string-match-p "- /x/ (⟦3⟧): open\\. R\\."
                            (funcall block '((place . 1) (choice . 0) (reading . "") (reason . "R")))))
    (should (string-match-p "- /x/ (⟦3⟧): /b/ two\\. R\\."
                            (funcall block '((place . 1) (choice . 2) (reading . "") (reason . "R")) "en" '(nil))))
    (should (string-match-p "\\*\\*\\* Zweite Meinung (Fable 5\\.2, blind)\n- /x/ (⟦3⟧): /b/ two — anders als die Analyse\\. R\\."
                            (funcall block '((place . 1) (choice . 2) (reading . "") (reason . "R")) "de")))))

(ert-deftest bcat-second-opinion-review-lands-after-the-second-opinion ()
  "The review stands as *** Review (Fable 5.2) after the second opinion
\(after the ambiguities when there is none); each answer replaces only
its own block."
  (bcat-second-opinion-test--with-file file
    (cl-letf (((symbol-function 'bcat-second-opinion--language-code) (lambda (_f) "en")))
      (let ((places (bcat-second-opinion-places (bcat-second-opinion-test--read file))))
        (bcat-second-opinion--land-review file bcat-second-opinion-test--review "claude-fable-5-2")
        (should (string-match-p (concat "\\*\\*\\* Ambiguities\n- [^\n]*\n\n"
                                        "\\*\\*\\* Review (Fable 5\\.2)\n"
                                        "- /avidyamānam/ (⟦698⟧): A present passive participle, not a ppp\\.\n\n"
                                        "\\*\\*\\* DharmaMitra")
                                (bcat-second-opinion-test--read file)))
        (bcat-second-opinion--land file bcat-second-opinion-test--answer "claude-fable-5-2" places)
        (bcat-second-opinion--land-review file "{\"points\": []}" "claude-fable-5-2")
        (let ((text (bcat-second-opinion-test--read file)))
          (should (string-match-p (concat "\\*\\*\\* Second opinion (Fable 5\\.2, blind)\n- [^\n]*\n\n"
                                          "\\*\\*\\* Review (Fable 5\\.2)\nNo further points\\.\n\n"
                                          "\\*\\*\\* DharmaMitra")
                                  text))
          (should-not (string-match-p "present passive" text)))))))

(ert-deftest bcat-second-opinion-after-fetch-asks-fable ()
  "After a fetch: a file whose analysis names ambiguities goes blind to
the second-opinion model (Fable), and its export waits for the answer
\(the export runs when it has landed); none named, the second-opinion
model itself the CAT's model, `bcat-second-opinion-auto' off — no
request."
  (bcat-second-opinion-test--with-file file
    (let ((sent '()) (land nil) (exported '()))
      (cl-letf (((symbol-function 'tibetan-discussion--submit)
                 (lambda (kind f prompts l &optional _fail)
                   (push (list kind (file-name-nondirectory f) (bcat-model)
                               (string-match-p "contrasts the lasting" (cdr (funcall prompts))))
                         sent)
                   (setq land l) t))
                ((symbol-function 'bcat-export-after-fetch)
                 (lambda (f) (push (file-name-nondirectory f) exported)))
                ((symbol-function 'bcat-second-opinion--language) (lambda (_f) "English"))
                ((symbol-function 'bcat-second-opinion--language-code) (lambda (_f) "en")))
        (let ((bcat-model "claude-opus-5-5") (bcat-second-opinion-model "claude-fable-5-2")
              (bcat-second-opinion-auto t))
          (should (bcat-second-opinion-after-fetch file))
          (should (equal '(("second-opinion" "sent-1012-tantralo.org" claude-fable-5-2 nil)) sent))
          (should (bcat-second-opinion-pending-p file))
          (funcall land file bcat-second-opinion-test--answer)
          (should-not (bcat-second-opinion-pending-p file))
          (should (equal '("sent-1012-tantralo.org") exported))
          (should (string-match-p "Second opinion (Fable 5\\.2, blind)" (bcat-second-opinion-test--read file)))
          ;; the CAT's model is Fable itself: no second round
          (setq sent '())
          (let ((bcat-model "claude-fable-5-2"))
            (should-not (bcat-second-opinion-after-fetch file)))
          (let ((bcat-second-opinion-auto nil))
            (should-not (bcat-second-opinion-after-fetch file)))
          (should-not sent))))
    ;; no ambiguities
    (bcat-second-opinion-test--without-ambiguities file)
    (cl-letf (((symbol-function 'tibetan-discussion--submit) (lambda (&rest _) (error "No request"))))
      (let ((bcat-model "claude-opus-5-5") (bcat-second-opinion-auto t))
        (should-not (bcat-second-opinion-after-fetch file))))))

(ert-deftest bcat-second-opinion-command-reviews ()
  "C-c u Z („Fable fragen“) in a sentence file: a review of the analysis
— also when it names ambiguities (those went blind after the fetch); the
export is not touched (C-c u H when wanted)."
  (bcat-second-opinion-test--with-file file
    (let ((sent '()) (land nil) (exported '()))
      (cl-letf (((symbol-function 'tibetan-discussion--submit)
                 (lambda (kind f _prompts l &optional _fail)
                   (push (list kind (file-name-nondirectory f) (bcat-model)) sent)
                   (setq land l) t))
                ((symbol-function 'bcat-export-after-fetch)
                 (lambda (f) (push f exported)))
                ((symbol-function 'bcat-second-opinion--language-code) (lambda (_f) "en")))
        (let ((bcat-second-opinion-model "claude-fable-5-2"))
          (with-current-buffer (find-file-noselect file)
            (unwind-protect (bcat-second-opinion) (kill-buffer)))
          (should (equal '(("review" "sent-1012-tantralo.org" claude-fable-5-2)) sent))
          (funcall land file bcat-second-opinion-test--review)
          (should (string-match-p "\\*\\*\\* Review (Fable 5\\.2)" (bcat-second-opinion-test--read file)))
          (should-not (bcat-second-opinion-pending-p file))
          (should-not exported))))))

(ert-deftest bcat-second-opinion-command-with-prefix-asks-blind ()
  "C-u C-c u Z: the blind second opinion on the named places again, the
export not touched; without named places, an error and no request."
  (bcat-second-opinion-test--with-file file
    (let ((sent '()) (land nil) (exported '()))
      (cl-letf (((symbol-function 'tibetan-discussion--submit)
                 (lambda (kind f _prompts l &optional _fail)
                   (push (list kind (file-name-nondirectory f)) sent)
                   (setq land l) t))
                ((symbol-function 'bcat-export-after-fetch)
                 (lambda (f) (push f exported)))
                ((symbol-function 'bcat-second-opinion--language-code) (lambda (_f) "en")))
        (with-current-buffer (find-file-noselect file)
          (unwind-protect (bcat-second-opinion '(4)) (kill-buffer)))
        (should (equal '(("second-opinion" "sent-1012-tantralo.org")) sent))
        (funcall land file bcat-second-opinion-test--answer)
        (should-not exported)
        (bcat-second-opinion-test--without-ambiguities file)
        (setq sent '())
        (with-current-buffer (find-file-noselect file)
          (unwind-protect (should-error (bcat-second-opinion '(4)) :type 'user-error)
            (kill-buffer)))
        (should-not sent)))))

(ert-deftest bcat-second-opinion-export-waits-while-pending ()
  "The export after a fetch waits while the second opinion of the file
is still to come — it runs when the answer has landed."
  (require 'bcat-export)
  (bcat-second-opinion-test--with-file file
    (let ((exported '()))
      (cl-letf (((symbol-function 'bcat-export-file) (lambda (f) (push f exported) f))
                ((symbol-function 'bcat-dossier-export-files) (lambda (f) (list f))))
        (let ((bcat-export-auto t))
          (puthash (file-truename file) 'auto bcat-second-opinion--pending)
          (bcat-export-after-fetch file)
          (should-not exported)
          (remhash (file-truename file) bcat-second-opinion--pending)
          (bcat-export-after-fetch file)
          (should (equal (list file) exported)))))))

(ert-deftest bcat-second-opinion-model-name ()
  "The model as the heading names it."
  (should (equal "Fable 5.2" (bcat-second-opinion--model-name "claude-fable-5-2")))
  (should (equal "Opus 5.5" (bcat-second-opinion--model-name 'claude-opus-5-5))))

(provide 'bcat-second-opinion-test)
;;; bcat-second-opinion-test.el ends here
