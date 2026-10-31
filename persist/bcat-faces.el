;;; bcat-faces.el --- The sentence files in colour: words against glosses  -*- lexical-binding: t -*-

;;; Commentary:
;; Carsten 31.10.2026, reading TĀ 1006: „wenn ich vom Remarkable oder PDF
;; lese fällt es mir schwer im Segment die einzelnen Wörter im Überblick
;; zu halten. Im .org geht's besser, ist aber auch nicht so richtig
;; optimal. Farbliche Hervorhebung wäre da schön“ — word against gloss.
;; The PDF does it in LaTeX (bcat-export, \bcatg); here the same in
;; Emacs.  First try: words blue — on the reMarkable Paper Pro the blue
;; came out greyish and took contrast from the words; „kehren wir es
;; einfach so um“: the words keep the text colour, blue are the gloss in
;; brackets in the line with the vocabulary and in every table the rows
;; below the words (formation, parts, gloss, form) — not the metre table
;; of the Recitation.  `bcat-faces-mode' switches itself on in every
;; sentence file of a template (sent-…, comm-… with #+CAT_TEMPLATE:).

;;; Code:

(require 'org)

(defface bcat-gloss-face
  '((((background dark)) :foreground "#8FB4F0")
    (t :foreground "#1F4E9E"))
  "What explains the words: the glosses in brackets in the line with the
vocabulary and the rows below the words in the tables (as
`bcat-export-gloss-color' in the PDF)."
  :group 'buddhist-cat)

(defun bcat-faces--unit-matcher (limit)
  "Font-lock matcher: the next unit of a line with the vocabulary before
LIMIT — group 1 the word(s), group 2 the gloss in brackets; only inside a
verse block (a Structure line's brackets are no glosses)."
  (let ((found nil))
    (while (and (not found)
                (re-search-forward "[ \t]*\\([^][\n]*?\\)[ \t]*\\(\\[[^]\n]*\\]\\)" limit t))
      (when (save-match-data
              (org-between-regexps-p "^[ \t]*#\\+begin_verse" "^[ \t]*#\\+end_verse"))
        (setq found t)))
    found))

(defun bcat-faces--metre-row-p (line)
  "Non-nil when the table LINE holds only metre signs (– ⏑ ¦)."
  (string-match-p "\\`[ \t]*|[–⏑¦| \t]*\\'" line))

(defun bcat-faces--detail-row-matcher (limit)
  "Font-lock matcher: the next table row below a table's first row (the
words) before LIMIT — group 0 the row; none of a metre table (its second
row only – and ⏑)."
  (let ((found nil))
    (while (and (not found) (re-search-forward "^[ \t]*|[^-\n].*$" limit t))
      (let ((row-end (match-end 0)) (row-beg (match-beginning 0)))
        (save-excursion
          (goto-char row-beg)
          (when (and (not (bobp))
                     (progn (forward-line -1) (looking-at "[ \t]*|")))
            ;; Up to the table's first row; its second row decides.
            (while (and (not (bobp))
                        (save-excursion (forward-line -1) (looking-at "[ \t]*|")))
              (forward-line -1))
            (forward-line 1)
            (unless (bcat-faces--metre-row-p
                     (buffer-substring-no-properties (line-beginning-position) (line-end-position)))
              (setq found t))))
        (when found (set-match-data (list row-beg row-end)))))
    found))

(defconst bcat-faces--keywords
  '((bcat-faces--unit-matcher (2 'bcat-gloss-face prepend))
    (bcat-faces--detail-row-matcher
     ("[^|\n]+" (progn (goto-char (match-beginning 0)) (match-end 0)) nil
      (0 'bcat-gloss-face prepend))))
  "The font-lock keywords of `bcat-faces-mode'.")

;;;###autoload
(define-minor-mode bcat-faces-mode
  "Words against glosses in a sentence file: the words in the text
colour, blue the glosses in the line with the vocabulary and the rows
below the words in the tables (Carsten 31.10.)."
  :lighter nil
  (if bcat-faces-mode
      (font-lock-add-keywords nil bcat-faces--keywords 'append)
    (font-lock-remove-keywords nil bcat-faces--keywords))
  (when font-lock-mode (font-lock-flush)))

(defun bcat-faces-maybe ()
  "`org-mode-hook': switch `bcat-faces-mode' on in a sentence file of a
template — sent-… or comm-… with a #+CAT_TEMPLATE: line."
  (when (and buffer-file-name
             (string-match-p "\\`\\(?:sent-[0-9]\\|comm-\\)" (file-name-nondirectory buffer-file-name))
             (save-excursion
               (goto-char (point-min))
               (re-search-forward "^#\\+CAT_TEMPLATE:" (min (point-max) 4000) t)))
    (bcat-faces-mode 1)))

(add-hook 'org-mode-hook #'bcat-faces-maybe)

(provide 'bcat-faces)
;;; bcat-faces.el ends here
