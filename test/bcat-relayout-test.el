;;; bcat-relayout-test.el --- Tests for setting a sentence file anew without a fetch  -*- lexical-binding: t; -*-

;;; Commentary:
;; Carsten 31.10.2026 abends, TĀ 1006: the sentence files got a new order
;; (984d377); for the files analysed already „Ohne Abruf umbauen“.  The
;; spec: an old file read back and rendered with the new template IS the
;; new sample — the reviewed old samples (test/fixtures/old-layout-*.org,
;; the samples of 78a26ed) against the reviewed new ones; and a file in
;; the new order comes out unchanged.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((root (expand-file-name ".." (file-name-directory
                                    (or load-file-name buffer-file-name)))))
  (dolist (d '("persist" "analysis" "core" "test" "test/fixtures"))
    (add-to-list 'load-path (expand-file-name d root))))

(require 'bcat-analysis-test)
(require 'bcat-sentence-test)
(require 'bcat-relayout)

(defun bcat-relayout-test--file (name)
  "The text of FILE NAME under the package root."
  (with-temp-buffer
    (insert-file-contents (expand-file-name name bcat-analysis-test--root))
    (buffer-string)))

(defun bcat-relayout-test--render (template base text)
  "TEXT read back into BASE and rendered with the tool's TEMPLATE."
  (let ((bcat-template-directory (expand-file-name "Templates/analysis" bcat-analysis-test--root)))
    (bcat-template-render-named template (bcat-relayout-context base text template))))

(defun bcat-relayout-test--partner-base ()
  "The rump of the pair's Tibetan file of AS 3, with the Sanskrit
segments the rump has (Devanāgarī, the line as transmitted)."
  (append '((sanskrit-p . t))
          `((sanskrit . ,(alist-get 'segments (bcat-analysis-test--base-003))))
          (bcat-analysis-test--base-tib-003)))

(ert-deftest bcat-relayout-old-sanskrit ()
  "TĀ 1007 in the old order → the new sample: tables under * Text with
⟦N⟧, the concepts under * Sentence, the rest per segment."
  (should (equal (bcat-relayout-test--file "Templates/analysis/samples/Sanskrit.org")
                 (bcat-relayout-test--render
                  "sanskrit" (bcat-analysis-test--base-1007)
                  (bcat-relayout-test--file "test/fixtures/old-layout-Sanskrit.org")))))

(ert-deftest bcat-relayout-old-tibetan ()
  "bDen 9 in the old order → the new sample."
  (should (equal (bcat-relayout-test--file "Templates/analysis/samples/Tibetan.org")
                 (bcat-relayout-test--render
                  "tibetan" (bcat-analysis-test--base)
                  (bcat-relayout-test--file "test/fixtures/old-layout-Tibetan.org")))))

(ert-deftest bcat-relayout-old-bilingual ()
  "AS 3 (Sanskrit with the Tibetan) in the old order → the new sample:
the Tibetan per segment with its differences under * Tibetan — read
from the old segments, the assignment from their headings."
  (should (equal (bcat-relayout-test--file "Templates/analysis/samples/Bilingual.org")
                 (bcat-relayout-test--render
                  "bilingual" (bcat-analysis-test--base-003)
                  (bcat-relayout-test--file "test/fixtures/old-layout-Bilingual.org")))))

(ert-deftest bcat-relayout-old-partner ()
  "The Tibetan file of the pair in the old order → the new sample: the
Sanskrit per group under * Sanskrit."
  (should (equal (bcat-relayout-test--file "Templates/analysis/samples/Bilingual-Tibetan.org")
                 (bcat-relayout-test--render
                  "bilingual-tibetan" (bcat-relayout-test--partner-base)
                  (bcat-relayout-test--file "test/fixtures/old-layout-Bilingual-Tibetan.org")))))

(ert-deftest bcat-relayout-new-order-unchanged ()
  "A file in the new order read back and set anew is the same file —
for each template."
  (dolist (case `(("sanskrit" "Sanskrit" ,(bcat-analysis-test--base-1007))
                  ("tibetan" "Tibetan" ,(bcat-analysis-test--base))
                  ("bilingual" "Bilingual" ,(bcat-analysis-test--base-003))
                  ("bilingual-tibetan" "Bilingual-Tibetan" ,(bcat-relayout-test--partner-base))))
    (let ((sample (bcat-relayout-test--file
                   (format "Templates/analysis/samples/%s.org" (nth 1 case)))))
      (should (equal sample (bcat-relayout-test--render (nth 0 case) (nth 2 case) sample))))))

(ert-deftest bcat-relayout-label-on-its-own-line ()
  "31.10. abends, the first PDF of TĀ 1006 in the new order: „⟦683⟧“
directly above a table stood beside it (LaTeX took both as one
paragraph) — the mark gets a line of its own.  Files written before
\(mark and table without a blank line) read back the same."
  (let* ((sample (bcat-relayout-test--file "Templates/analysis/samples/Tibetan.org"))
         (glued (replace-regexp-in-string "^\\(⟦[0-9]+⟧\\)\n\n|" "\\1\n|" sample)))
    (should-not (equal glued sample))
    (should (string-match-p "^⟦38⟧\n\n| " sample))
    (should (equal sample (bcat-relayout-test--render "tibetan" (bcat-analysis-test--base) glued)))))

(ert-deftest bcat-relayout-second-translation ()
  "The translation in the second language (*Deutsch:* under * Text)
is read back and stays."
  (let* ((sample (bcat-relayout-test--file "Templates/analysis/samples/Tibetan.org"))
         (text (replace-regexp-in-string
                "^\\(\\*Translation:\\* Indescribable.*\\)$"
                "\\1\n\n*Deutsch:* Unsagbar und unsichtbar." sample t))
         (out (bcat-relayout-test--render "tibetan" (bcat-analysis-test--base) text)))
    (should (string-match-p "\n\n\\*Deutsch:\\* Unsagbar und unsichtbar\\.\n\n\\* Sentence\n" out))))

(ert-deftest bcat-relayout-file ()
  "C-c u L on an analysed file in the old order: the file in the new
order — My Notes, a section of its own (* Handout), #+CREATED and
#+LAST_ANALYZED kept (setting anew is no analysis), no request."
  (bcat-sentence-test--with-source
    (bcat-sentence-test--create src)
    (bcat-sentence-test--with-stubs bcat-sentence-test--answer
      (bcat-sentence-test--fetch src))
    (let* ((file (tibetan-sentence--filepath 9 folder src))
           (new (bcat-sentence-test--text-of file))
           (fired nil))
      ;; Back to the old order: tables and concepts in the segments.
      (with-temp-file file
        (insert (replace-regexp-in-string
                 "^#\\+LAST_ANALYZED: .*$" "#+LAST_ANALYZED: 2026-10-20"
                 (concat
                  (car (split-string new "^\\* Text\n"))
                  "* Text\n#+begin_verse\nབརྗོད་དུ་མེད་པ་བལྟར་མེད་པ། །\nའགྱུར་བ་མེད་པ་འདུས་མ་བྱས། །\n#+end_verse\n\n"
                  "*Translation:* Indescribable and invisible, immutable and uncompounded\n\n"
                  "* Segments\n** Segment 38\n| [[steinert:brjod du med pa][brjod du med pa]] |\n| indescribable |\n| ADJ |\n\n"
                  "⟦38⟧ *Translation:* Indescribable and invisible,\n\n*Structure:* no finite verb\n\n"
                  "** Segment 39\n| [[steinert:'dus ma byas]['dus ma byas]] |\n| uncompounded |\n| ADJ |\n\n"
                  "⟦39⟧ *Translation:* immutable and uncompounded\n\n*Structure:* continues the chain\n\n"
                  "*Concepts:* *'dus ma byas* (asaṃskṛta)\n\n"
                  "* Sentence\n** Grammar\nTwo predicate pādas.\n\n** Translations compared\n*Lindtner 1981:* L.\n\n"
                  "* Working Translation\n\n* My Notes\nMeine Notiz.\n\n* Footnotes\n\n* Handout\nH\n")
                 t)))
      (cl-letf (((symbol-function 'tibetan-discussion--submit)
                 (lambda (&rest _) (setq fired t))))
        (with-current-buffer (find-file-noselect file)
          (unwind-protect (bcat-relayout)
            (kill-buffer))))
      (let ((text (bcat-sentence-test--text-of file)))
        (should-not fired)
        (should (string-match-p (regexp-quote "#+end_verse\n\n⟦38⟧\n\n| [[steinert:brjod du med pa][brjod du med pa]] |\n| indescribable |") text))
        (should (string-match-p (regexp-quote "** Concepts\n⟦39⟧ *'dus ma byas* (asaṃskṛta)\n") text))
        (should (string-match-p (regexp-quote "* Segments\n** Segment 38\n⟦38⟧ *Translation:* Indescribable and invisible,\n\n*Structure:* no finite verb\n") text))
        (should (string-match-p "^#\\+LAST_ANALYZED: 2026-10-20$" text))
        (should (string-match-p "^\\* My Notes\nMeine Notiz\\.$" text))
        (should (string-match-p "^\\* Handout\nH$" text))
        (should (= 1 (with-temp-buffer (insert text) (how-many "^\\* Handout$" (point-min) (point-max)))))
        ;; Still analysed: the ⟦N⟧ translations under * Segments.
        (should-not (eq 'full (bcat-sentence-fetch-state file '(38 39))))))))

(ert-deftest bcat-relayout-answers-kept ()
  "C-c u G keeps Claude's answer beside the file (Carsten 31.10.: „ab
jetzt: Antwort aufbewahrt“ — a later change of the template needs no
new fetch): analysis/.answers/<file>.eld, Lisp data read back exactly —
the answer and the references sent; for a bilingual sentence the
Tibetan answer and its references too."
  (bcat-sentence-test--with-source
    (bcat-sentence-test--create src)
    (bcat-sentence-test--with-stubs bcat-sentence-test--answer
      (bcat-sentence-test--fetch src))
    (let* ((file (tibetan-sentence--filepath 9 folder src))
           (record (bcat-sentence--answers file)))
      (should (file-exists-p (expand-file-name ".answers/sent-009-bden.eld" folder)))
      (should (equal (bcat-analysis-parse bcat-sentence-test--answer) (plist-get record :parsed)))
      (should (assoc "Lindtner 1981" (plist-get record :refs)))))
  (bcat-sentence-test--with-bil-source
    (bcat-sentence-test--create-n src 3)
    (bcat-sentence-test--with-stubs #'bcat-sentence-test--bil-answers
      (bcat-sentence-test--fetch-n src 3))
    (let ((record (bcat-sentence--answers (tibetan-sentence--filepath 3 folder src))))
      (should (assq 'segments (plist-get record :parsed)))
      (should (assq 'rendering (plist-get record :tibetan)))
      (should (listp (plist-get record :trefs))))))

(ert-deftest bcat-relayout-fills-from-answers ()
  "Setting anew with a kept answer: what the file shows wins — a hand
correction stays —, the answer fills what the file does not show (here
a class template that brings back the glossed verse lines)."
  (bcat-sentence-test--with-source
    (bcat-sentence-test--create src)
    (bcat-sentence-test--with-stubs bcat-sentence-test--answer
      (bcat-sentence-test--fetch src))
    (let ((file (tibetan-sentence--filepath 9 folder src)))
      ;; A hand correction in the file.
      (with-temp-file file
        (insert (replace-regexp-in-string "^\\*Structure:\\* no finite verb$"
                                          "*Structure:* corrected by hand"
                                          (bcat-sentence-test--text-of file) t t)))
      ;; The class's own template (the cascade): the glossed lines under * Text.
      (make-directory (expand-file-name "Templates/analysis" dir) t)
      (with-temp-file (expand-file-name "Templates/analysis/tibetan.org" dir)
        (insert (replace-regexp-in-string
                 "^\\* Text\n" "* Text\n{{#segments}}\nGLOSSED {{glossed}}\n{{/segments}}\n"
                 (with-temp-buffer
                   (insert-file-contents (expand-file-name "tibetan.org" bcat-template-directory))
                   (buffer-string))
                 t t)))
      (bcat-relayout-file file)
      (let ((text (bcat-sentence-test--text-of file)))
        (should (string-match-p "^\\*Structure:\\* corrected by hand$" text))
        (should (string-match-p "^GLOSSED brjod du med pa \\[indescribable\\]" text))))))

(ert-deftest bcat-relayout-key-and-menu ()
  "C-c u L sets anew; the menu offers it next to the jumps."
  (require 'tibetan-keybindings)
  (require 'tibetan-menu)
  (should (eq (key-binding (kbd "C-c u L")) 'bcat-relayout))
  (should (string-match-p "bcat-relayout"
                          (prin1-to-string (symbol-value 'tibetan-cat-menu)))))

(provide 'bcat-relayout-test)
;;; bcat-relayout-test.el ends here
