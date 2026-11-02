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
          "| tad               | an-ākarṇya               |\n"
          "| —                 | an- + ā-√karṇ-ya         |\n"
          "|                   | an- not · √karṇ (10) ear |\n"
          "| that              | not having heard         |\n"
          "| pron. n. acc. sg. | gerund                   |\n")
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

(ert-deftest bcat-faces-detail-rows ()
  "In a table the rows that explain the words (formation, parts, form) —
not the words' row (Carsten 31.10.: the words stay black, the rest
blue), not the gloss row, next to last (same evening, TĀ 1006: „auch die
Übersetzung der einzelnen Terme schwarz … dann sehe ich gleich was
wichtig ist“), and not the rows of the metre table (second row only –
and ⏑).  A table in bands (hline between): each band on its own."
  (with-temp-buffer
    (insert bcat-faces-test--text
            "\n* Recitation\n| ta | da |\n| ⏑  | –  |\n"
            "\n| bden  |\n| truth |\n| N     |\n|-------|\n| gnyis |\n| two   |\n| NUM   |\n")
    (org-mode)
    (should (equal '("| —                 | an- + ā-√karṇ-ya         |"
                     "|                   | an- not · √karṇ (10) ear |"
                     "| pron. n. acc. sg. | gerund                   |"
                     "| N     |"
                     "| NUM   |")
                   (bcat-faces-test--matches #'bcat-faces--detail-row-matcher 0)))))

(ert-deftest bcat-faces-tibetan-table ()
  "A Tibetan table (Uchen, Wylie, gloss, function — Carsten 31.10.:
Uchen and Wylie black): only the function row is blue."
  (with-temp-buffer
    (insert "* Text\n⟦66⟧\n\n| དེ        | ལས   |\n| de        | las  |\n| that/them | from |\n| DEM       | ABL  |\n")
    (org-mode)
    (should (equal '("| DEM       | ABL  |")
                   (bcat-faces-test--matches #'bcat-faces--detail-row-matcher 0)))))

(ert-deftest bcat-faces-comparison-table-plain ()
  "2.11.: the overview table of the comparison (a header row and an hline
under it — no table of words has one) stays in the text colour: no
detail rows there."
  (with-temp-buffer
    (insert "** Translations compared\n*** Übersicht\n"
            "| Nr. | Stelle | Einstufung                   | Kern |\n"
            "|-----+--------+------------------------------+------|\n"
            "|   1 | /a/    | @@latex:\\cellcolor{bad}@@rot | x    |\n"
            "|   2 | /b/    | @@latex:\\cellcolor{ok}@@grün | y    |\n"
            "|   3 | /c/    | @@latex:\\cellcolor{mid}@@gelb | z    |\n"
            "\n| bden  |\n| truth |\n| N     |\n")
    (org-mode)
    (should (equal '("| N     |")
                   (bcat-faces-test--matches #'bcat-faces--detail-row-matcher 0)))))

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
