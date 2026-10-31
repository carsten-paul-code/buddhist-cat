;;; bcat-faces-test.el --- Tests for the colours of the sentence files in Emacs  -*- lexical-binding: t; -*-

;;; Commentary:
;; Carsten 31.10.2026: reading a segment „fällt es mir schwer … die
;; einzelnen Wörter im Überblick zu halten. Im .org geht's besser, ist
;; aber auch nicht so richtig optimal. Farbliche Hervorhebung wäre da
;; schön“ — word against gloss, as in the PDF (673e39e).

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((root (expand-file-name ".." (file-name-directory
                                    (or load-file-name buffer-file-name)))))
  (dolist (d '("persist" "core" "analysis" "philology" "data" "config" "doc-prep" "test"))
    (add-to-list 'load-path (expand-file-name d root))))

(require 'org)
(require 'bcat-faces)

(defconst bcat-faces-test--text
  (concat "* Text\n#+begin_verse\n"
          "tad [that] an-ākarṇya [not having heard] /\n"
          "#+end_verse\n\n"
          "*Structure:* /tad/ [object] → protasis\n\n"
          "* Segments\n** Segment 683\n"
          "| tad  | an-ākarṇya       |\n"
          "| —    | an- + ā-√karṇ-ya |\n"
          "| that | not having heard |\n")
  "A piece of a Sanskrit sentence file.")

(defun bcat-faces-test--matches (matcher group)
  "The texts of GROUP of every match of MATCHER in the current buffer."
  (goto-char (point-min))
  (let ((out '()))
    (while (funcall matcher (point-max))
      (push (match-string-no-properties group) out))
    (nreverse out)))

(ert-deftest bcat-faces-word-and-gloss ()
  "In the line with the vocabulary (a verse block) each unit: the word
\(group 1) and its gloss in brackets (group 2) — not the brackets of a
Structure line outside the verse."
  (with-temp-buffer
    (insert bcat-faces-test--text)
    (org-mode)
    (should (equal '("tad" "an-ākarṇya")
                   (bcat-faces-test--matches #'bcat-faces--unit-matcher 1)))
    (should (equal '("[that]" "[not having heard]")
                   (bcat-faces-test--matches #'bcat-faces--unit-matcher 2)))))

(ert-deftest bcat-faces-word-row ()
  "In a table the first row — the words — and no other."
  (with-temp-buffer
    (insert bcat-faces-test--text)
    (org-mode)
    (should (equal '("| tad  | an-ākarṇya       |")
                   (bcat-faces-test--matches #'bcat-faces--word-row-matcher 0)))))

(ert-deftest bcat-faces-only-in-sentence-files ()
  "Switched on in a sentence file of a template (sent-…, comm-… with
#+CAT_TEMPLATE:), not in other org files."
  (let ((dir (make-temp-file "bcat-faces-" t)))
    (unwind-protect
        (let ((sent (expand-file-name "sent-1006-tantralo.org" dir))
              (other (expand-file-name "notes.org" dir)))
          (with-temp-file sent (insert "#+CAT_TEMPLATE: sanskrit\n\n" bcat-faces-test--text))
          (with-temp-file other (insert bcat-faces-test--text))
          (dolist (f (list sent other))
            (with-current-buffer (find-file-noselect f)
              (unwind-protect
                  (should (eq (equal f sent) (and bcat-faces-mode t)))
                (kill-buffer)))))
      (delete-directory dir t))))

(provide 'bcat-faces-test)
;;; bcat-faces-test.el ends here
