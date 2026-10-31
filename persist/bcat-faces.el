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
;; that explain the words (formation, parts, form) — not the gloss row
;; (TĀ 1006, same evening: „auch die Übersetzung der einzelnen Terme
;; schwarz … dann sehe ich gleich was wichtig ist“), not the metre table
;; of the Recitation.  `bcat-faces-mode' switches itself on in every
;; sentence file of a template (sent-…, comm-… with #+CAT_TEMPLATE:).

;;; Code:

(require 'org)
(require 'cl-lib)

(defface bcat-gloss-face
  '((((background dark)) :foreground "#8FB4F0")
    (t :foreground "#1F4E9E"))
  "What explains the words: the glosses in brackets in the line with the
vocabulary and the table rows that explain the words (as
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

(defun bcat-faces--band (pos)
  "The rows of the table band around the table row at POS (rows between
hlines), as a list of their line starts."
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (while (and (not (bobp))
                (save-excursion (forward-line -1) (looking-at "[ \t]*|[^-\n]")))
      (forward-line -1))
    (let ((rows '()))
      (while (looking-at "[ \t]*|[^-\n]")
        (push (point) rows)
        (forward-line 1))
      (nreverse rows))))

(defun bcat-faces--metre-table-p (pos)
  "Non-nil when the table row at POS belongs to a metre table — the
table's second row holds only – ⏑ ¦."
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (while (and (not (bobp))
                (save-excursion (forward-line -1) (looking-at "[ \t]*|")))
      (forward-line -1))
    (forward-line 1)
    (and (looking-at "[ \t]*|")
         (bcat-faces--metre-row-p
          (buffer-substring-no-properties (line-beginning-position) (line-end-position))))))

(defun bcat-faces--detail-row-matcher (limit)
  "Font-lock matcher: the next table row before LIMIT that explains the
words — group 0 the row: in its band (rows between hlines) neither the
first (the words) nor the one next to last (the gloss); none of a metre
table."
  (let ((found nil))
    (while (and (not found) (re-search-forward "^[ \t]*|[^-\n].*$" limit t))
      (let* ((beg (match-beginning 0)) (end (match-end 0))
             (band (bcat-faces--band beg))
             (i (cl-position beg band)))
        (when (and i (/= i 0) (/= i (- (length band) 2))
                   (not (bcat-faces--metre-table-p beg)))
          (set-match-data (list beg end))
          (setq found t))))
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
that explain the words — formation, parts, form — in the tables
\(Carsten 31.10.)."
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
