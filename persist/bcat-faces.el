;;; bcat-faces.el --- The sentence files in colour: words against glosses  -*- lexical-binding: t -*-

;;; Commentary:
;; Carsten 31.10.2026, reading TĀ 1006: „wenn ich vom Remarkable oder PDF
;; lese fällt es mir schwer im Segment die einzelnen Wörter im Überblick
;; zu halten. Im .org geht's besser, ist aber auch nicht so richtig
;; optimal. Farbliche Hervorhebung wäre da schön“ — word against gloss.
;; The PDF does it in LaTeX (bcat-export, \bcatw / \bcatg); here the
;; same in Emacs: in the line with the vocabulary (a verse block) the
;; word blue and its gloss in brackets grey, in every table the row of
;; the words blue.  `bcat-faces-mode' switches itself on in every
;; sentence file of a template (sent-…, comm-… with #+CAT_TEMPLATE:).

;;; Code:

(require 'org)

(defface bcat-word-face
  '((((background dark)) :foreground "#8FB4F0")
    (t :foreground "#1F4E9E"))
  "The words in the line with the vocabulary and in the word row of the
tables (as `bcat-export-word-color' in the PDF)."
  :group 'buddhist-cat)

(defface bcat-gloss-face
  '((((background dark)) :foreground "#9A9A9A")
    (t :foreground "#6E6E6E"))
  "The glosses in brackets in the line with the vocabulary (as
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

(defun bcat-faces--word-row-matcher (limit)
  "Font-lock matcher: the next first row of a table before LIMIT (the
words) — group 0 the row."
  (let ((found nil))
    (while (and (not found) (re-search-forward "^[ \t]*|[^-\n].*$" limit t))
      (when (save-excursion
              (beginning-of-line)
              (or (bobp) (progn (forward-line -1) (not (looking-at "[ \t]*|")))))
        (setq found t)))
    found))

(defconst bcat-faces--keywords
  '((bcat-faces--unit-matcher (1 'bcat-word-face prepend) (2 'bcat-gloss-face prepend))
    (bcat-faces--word-row-matcher
     ("[^|\n]+" (progn (goto-char (match-beginning 0)) (match-end 0)) nil
      (0 'bcat-word-face prepend))))
  "The font-lock keywords of `bcat-faces-mode'.")

;;;###autoload
(define-minor-mode bcat-faces-mode
  "Words against glosses in a sentence file: in the line with the
vocabulary the word blue and its gloss grey, in the tables the row of
the words blue (Carsten 31.10.)."
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
