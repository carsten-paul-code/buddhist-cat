;;; bcat-second-opinion-test.el --- Tests for the second opinion  -*- lexical-binding: t -*-

;;; Commentary:
;; Carsten 27.11.2026: „Zweite Runde mit Mehrdeutigkeitsfeld und Befehl
;; „Fable fragen“ bauen“ — the analysis (Opus 5.5) names the places where
;; the original admits more than one analysis; Fable 5.2 gives a second
;; opinion on them, written under Translations compared.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'bcat-second-opinion)

(defconst bcat-second-opinion-test--file
  "#+TITLE: Sentence 1012 Analysis
#+CAT_TEMPLATE: sanskrit
#+SOURCE: [[file:../tantraloka-1.org::*Sentence 1012][tantraloka-1.org / Sentence 1012]]

* Text
#+begin_verse
anuttaraṃ katipayakāladārḍhyakāryamṛtāntaravailakṣaṇyād utkṛṣṭaṃ ca tat
#+end_verse

* Philology
** ⟦695⟧
*anuttaram katipaya-kāla-dārḍhya-kārya-mṛta-antara-vailakṣaṇyāt utkṛṣṭam ca tat*

** Translations compared
*Translation:* The Unsurpassable …

*Dyczkowski 2023:* (By) ‘Absolute’ …

*** Discussion points
None: the translations agree in substance.

*** Ambiguities
- /kāryamṛta/ (⟦695⟧): /kārya-mṛta/ ‘dead products’ · /kāri-amṛta/ ‘nectars that produce’ — chosen: /kārya-mṛta/. The commentary contrasts the lasting with the perishable.

*** DharmaMitra (machine translation, for completeness)
*DharmaMitra:* The unsurpassed …

** Segment 695
*Sandhi:* …

* Worum es geht
Text.

* My Notes
Meine Notiz.
"
  "A sentence file with one ambiguity.")

(defconst bcat-second-opinion-test--answer
  "{\"opinions\": [{\"passage\": \"/kāryamṛta/\", \"segment\": 695, \"verdict\": \"other\", \"reading\": \"/kāri-amṛta/ ‘nectars that produce firmness’\", \"reason\": \"/dārḍhya/ is the firmness the elixirs give.\"}], \"further\": [{\"passage\": \"/tam/\", \"segment\": 697, \"point\": \"Only m. acc. sg.\"}]}"
  "Fable's answer.")

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

(ert-deftest bcat-second-opinion-reads-the-ambiguities ()
  "The places the analysis named are read from its *** Ambiguities
\(German: Mehrdeutigkeiten) under Translations compared; none, nil."
  (should (equal '("- /kāryamṛta/ (⟦695⟧): /kārya-mṛta/ ‘dead products’ · /kāri-amṛta/ ‘nectars that produce’ — chosen: /kārya-mṛta/. The commentary contrasts the lasting with the perishable.")
                 (bcat-second-opinion-ambiguities-in bcat-second-opinion-test--file)))
  (should (equal '("- /x/ (⟦1⟧): a · b")
                 (bcat-second-opinion-ambiguities-in
                  "** Translations compared\n*** Mehrdeutigkeiten\n- /x/ (⟦1⟧): a · b\n\n** Segment 1\n")))
  (should-not (bcat-second-opinion-ambiguities-in "** Translations compared\n*Translation:* T\n")))

(ert-deftest bcat-second-opinion-prompts ()
  "The request: the analysis as plain lines and the ambiguities as the
first analysis named them; the reasons in the language of the
comparison; without ambiguities a review of the analysis."
  (bcat-second-opinion-test--with-file file
    (cl-letf (((symbol-function 'bcat-second-opinion--language) (lambda (_f) "English")))
      (let ((ps (bcat-second-opinion-prompts file)))
        (should (string-match-p "second opinion" (car ps)))
        (should (string-match-p "\"opinions\"" (car ps)))
        (should (string-match-p "reason and point in English" (car ps)))
        (should (string-match-p "<ambiguities>\n- /kāryamṛta/" (cdr ps)))
        (should (string-match-p "kārya-mṛta-antara" (cdr ps)))
        (should-not (string-match-p "Meine Notiz" (cdr ps))))
      (with-temp-file file
        (insert (replace-regexp-in-string "\\*\\*\\* Ambiguities\n- [^\n]*\n" "" bcat-second-opinion-test--file)))
      (should (string-match-p "named no such place — review" (cdr (bcat-second-opinion-prompts file)))))))

(ert-deftest bcat-second-opinion-lands-under-translations-compared ()
  "Fable's answer stands as *** Second opinion (Fable 5.2) after the
ambiguities, before the machine translation; asked again, it replaces
the earlier one; the user's sections stay."
  (bcat-second-opinion-test--with-file file
    (cl-letf (((symbol-function 'bcat-second-opinion--language-code) (lambda (_f) "en")))
      (bcat-second-opinion--land file bcat-second-opinion-test--answer "claude-fable-5-2")
      (let ((text (with-temp-buffer (insert-file-contents file) (buffer-string))))
        (should (string-match-p (concat "\\*\\*\\* Ambiguities\n- [^\n]*\n\n"
                                        "\\*\\*\\* Second opinion (Fable 5\\.2)\n"
                                        "- /kāryamṛta/ (⟦695⟧): another reading — /kāri-amṛta/ ‘nectars that produce firmness’\\. "
                                        "/dārḍhya/ is the firmness the elixirs give\\.\n"
                                        "\\*Also:\\*\n- /tam/ (⟦697⟧): Only m\\. acc\\. sg\\.\n\n"
                                        "\\*\\*\\* DharmaMitra")
                                text))
        (should (string-match-p "Meine Notiz" text)))
      (bcat-second-opinion--land file "{\"opinions\": [{\"passage\": \"/kāryamṛta/\", \"segment\": 695, \"verdict\": \"agree\", \"reading\": \"\", \"reason\": \"Fine.\"}], \"further\": []}" "claude-fable-5-2")
      (let ((text (with-temp-buffer (insert-file-contents file) (buffer-string))))
        (should (= 1 (cl-count-if (lambda (l) (string-prefix-p "*** Second opinion" l))
                                  (split-string text "\n"))))
        (should (string-match-p "(⟦695⟧): agrees\\. Fine\\." text))
        (should-not (string-match-p "nectars that produce firmness" text))))))

(ert-deftest bcat-second-opinion-after-fetch-asks-fable ()
  "After a fetch: a file whose analysis names ambiguities goes to the
second-opinion model (Fable), and its export waits for the answer
\(the export runs when it has landed); none named, the second-opinion
model itself the CAT's model, `bcat-second-opinion-auto' off or a
fixture in the temp folder — no request."
  (bcat-second-opinion-test--with-file file
    (let ((sent '()) (land nil) (exported '()))
      (cl-letf (((symbol-function 'tibetan-discussion--submit)
                 (lambda (kind f _prompts l &optional _fail)
                   (push (list kind (file-name-nondirectory f) (bcat-model)) sent)
                   (setq land l) t))
                ((symbol-function 'bcat-export-after-fetch)
                 (lambda (f) (push (file-name-nondirectory f) exported)))
                ((symbol-function 'bcat-second-opinion--language-code) (lambda (_f) "en")))
        (let ((bcat-model "claude-opus-5-5") (bcat-second-opinion-model "claude-fable-5-2")
              (bcat-second-opinion-auto t))
          (should (bcat-second-opinion-after-fetch file))
          (should (equal '(("second-opinion" "sent-1012-tantralo.org" claude-fable-5-2)) sent))
          (should (bcat-second-opinion-pending-p file))
          (funcall land file bcat-second-opinion-test--answer)
          (should-not (bcat-second-opinion-pending-p file))
          (should (equal '("sent-1012-tantralo.org") exported))
          ;; the CAT's model is Fable itself: no second round
          (setq sent '())
          (let ((bcat-model "claude-fable-5-2"))
            (should-not (bcat-second-opinion-after-fetch file)))
          (let ((bcat-second-opinion-auto nil))
            (should-not (bcat-second-opinion-after-fetch file)))
          (should-not sent))))
    ;; no ambiguities
    (with-temp-file file
      (insert (replace-regexp-in-string "\\*\\*\\* Ambiguities\n- [^\n]*\n" "" bcat-second-opinion-test--file)))
    (cl-letf (((symbol-function 'tibetan-discussion--submit) (lambda (&rest _) (error "No request"))))
      (let ((bcat-model "claude-opus-5-5") (bcat-second-opinion-auto t))
        (should-not (bcat-second-opinion-after-fetch file))))))

(ert-deftest bcat-second-opinion-command-asks-even-without-ambiguities ()
  "C-c u Z („Fable fragen“) in a sentence file: the second opinion on the
named places, or a review of the analysis when it names none; the
export is not touched (C-c u H when wanted)."
  (bcat-second-opinion-test--with-file file
    (with-temp-file file
      (insert (replace-regexp-in-string "\\*\\*\\* Ambiguities\n- [^\n]*\n" "" bcat-second-opinion-test--file)))
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
          (should (equal '(("second-opinion" "sent-1012-tantralo.org" claude-fable-5-2)) sent))
          (funcall land file bcat-second-opinion-test--answer)
          (should-not exported))))))

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
