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

(defun bcat-faces--header-table-p (pos)
  "Non-nil when the table row at POS belongs to a table with a header
row — its second line an hline: the comparison's overview (2.11.); no
table of words has one."
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (while (and (not (bobp))
                (save-excursion (forward-line -1) (looking-at "[ \t]*|")))
      (forward-line -1))
    (and (looking-at "[ \t]*|[^-\n]")
         (progn (forward-line 1) (looking-at "[ \t]*|-")))))

(defun bcat-faces--detail-row-matcher (limit)
  "Font-lock matcher: the next table row before LIMIT that explains the
words — group 0 the row: in its band (rows between hlines) neither the
first (the words), nor the second when the first is Uchen (its Wylie),
nor the one next to last (the gloss); none of a metre table or of a
table with a header row."
  (let ((found nil))
    (while (and (not found) (re-search-forward "^[ \t]*|[^-\n].*$" limit t))
      (let* ((beg (match-beginning 0)) (end (match-end 0))
             (band (bcat-faces--band beg))
             (i (cl-position beg band))
             ;; A Tibetan table: Uchen first, its Wylie black too.
             (uchen (and band (save-excursion
                                (goto-char (car band))
                                (string-match-p "[\u0F00-\u0FFF]"
                                                (buffer-substring-no-properties
                                                 (point) (line-end-position)))))))
        (when (and i (/= i 0) (/= i (- (length band) 2))
                   (not (and uchen (= i 1)))
                   (not (bcat-faces--metre-table-p beg))
                   (not (bcat-faces--header-table-p beg)))
          (set-match-data (list beg end))
          (setq found t))))
    found))

(defface bcat-rating-ok-face
  '((t :background "#C8EBC8" :foreground "black"))
  "A unit of the comparison rated green — no difference in meaning (the
Muster's colour ok, 2.11.)."
  :group 'buddhist-cat)

(defface bcat-rating-mid-face
  '((t :background "#FAEBA0" :foreground "black"))
  "A unit of the comparison rated yellow — a possible other
interpretation (the Muster's colour mid)."
  :group 'buddhist-cat)

(defface bcat-rating-bad-face
  '((t :background "#F5BEB9" :foreground "black"))
  "A unit of the comparison rated red — different meanings (the Muster's
colour bad)."
  :group 'buddhist-cat)

(defun bcat-faces--rating-face (rating)
  "The face of RATING — a cell colour (ok, mid, bad) or the rating word
\(green/grün, yellow/gelb, red/rot) —, or nil."
  (pcase (downcase (or rating ""))
    ((or "ok" "green" "grün") 'bcat-rating-ok-face)
    ((or "mid" "yellow" "gelb") 'bcat-rating-mid-face)
    ((or "bad" "red" "rot") 'bcat-rating-bad-face)))

(defun bcat-faces--rating-matcher (limit)
  "Font-lock matcher: the next rating of the comparison before LIMIT
\(2.11.) — in the overview table the word after
@@latex:\\cellcolor{…}@@, at the end of a unit's heading (**** N. … —
gelb) and of an item of „Für die Klasse“ (- *N. … — rot*) the rating
word.  Group 1 the colour or the word, group 2 the word
to colour, group 3 the LaTeX snippet (only in the table)."
  (re-search-forward
   (concat "\\(?:\\(?3:@@latex:\\\\cellcolor{\\(?1:ok\\|mid\\|bad\\)}@@\\)\\(?2:[^ \t|\n]+\\)\\)"
           "\\|\\(?:^\\*\\{4\\} [0-9]+\\. .* — \\(?1:\\(?2:grün\\|gelb\\|rot\\|green\\|yellow\\|red\\)\\)[ \t]*$\\)"
           ;; „Für die Klasse“: - *3. /stelle/ — rot*
           "\\|\\(?:^- \\*[0-9]+\\. .* — \\(?1:\\(?2:grün\\|gelb\\|rot\\|green\\|yellow\\|red\\)\\)\\*[ \t]*$\\)")
   limit t))

(defun bcat-faces--header-end ()
  "End of the keyword block at the top of the buffer (its last line,
before the newline), or nil when the buffer does not begin with one."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (looking-at "#\\+[A-Z_]+:")
        (while (and (zerop (forward-line 1)) (looking-at "#\\+[A-Z_]+:")))
        (skip-chars-backward "\n")
        (point)))))

(defun bcat-faces--header-matcher (limit)
  "Font-lock matcher: the keyword block at the top after its first line
\(#+TITLE) — group 0 from the end of the first line to the end of the
block, folded behind „…“ (Carsten 4.11.: „ziemlich viel verschenkter
Platz“).  Matches whenever the fontified region starts inside the block,
whole, so a later chunk of jit-lock never unfolds a part of it."
  (ignore limit)
  (let ((end (bcat-faces--header-end))
        (start (save-excursion (save-restriction (widen) (goto-char (point-min)) (line-end-position)))))
    (when (and end (< start end) (< (point) end))
      (set-match-data (list start end))
      (goto-char end)
      t)))

(defun bcat-faces-toggle-header ()
  "Show or fold the keyword lines at the top of the sentence file."
  (interactive)
  (if (assq 'bcat-header buffer-invisibility-spec)
      (remove-from-invisibility-spec '(bcat-header . t))
    (add-to-invisibility-spec '(bcat-header . t)))
  (force-window-update (current-buffer)))

(defconst bcat-faces--keywords
  '((bcat-faces--unit-matcher (2 'bcat-gloss-face prepend))
    (bcat-faces--detail-row-matcher
     ("[^|\n]+" (progn (goto-char (match-beginning 0)) (match-end 0)) nil
      (0 'bcat-gloss-face prepend)))
    (bcat-faces--rating-matcher
     (3 'shadow prepend t)
     (2 (bcat-faces--rating-face (match-string 1)) prepend))
    (bcat-faces--header-matcher (0 '(face nil invisible bcat-header) prepend)))
  "The font-lock keywords of `bcat-faces-mode'.")

;;;###autoload
(define-minor-mode bcat-faces-mode
  "Words against glosses in a sentence file: the words in the text
colour, blue the glosses in the line with the vocabulary and the rows
that explain the words — formation, parts, form — in the tables
\(Carsten 31.10.); the ratings of the comparison in green, yellow, red
\(2.11.); the keyword lines at the top folded behind #+TITLE (4.11.,
`bcat-faces-toggle-header')."
  :lighter nil
  (if bcat-faces-mode
      (progn
        (font-lock-add-keywords nil bcat-faces--keywords 'append)
        (setq-local font-lock-extra-managed-props
                    (cl-adjoin 'invisible font-lock-extra-managed-props))
        (add-to-invisibility-spec '(bcat-header . t)))
    (font-lock-remove-keywords nil bcat-faces--keywords)
    (remove-from-invisibility-spec '(bcat-header . t)))
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

(defcustom bcat-faces-line-numbers nil
  "Non-nil: line numbers in sentence files and cascade sources as in
other buffers.  Carsten 4.11. (reading on the MacBook, „ziemlich viel
verschenkter Platz“): off — the source window beside the sentence file
was 22 characters wide."
  :type 'boolean
  :group 'buddhist-cat)

(defun bcat-faces--reading-file-p ()
  "Non-nil in a sentence file of a template or a cascade source."
  (and buffer-file-name
       (derived-mode-p 'org-mode)
       (save-excursion
         (goto-char (point-min))
         (re-search-forward "^#\\+\\(?:CAT_TEMPLATE\\|TIBETAN_LAYOUT\\):[ \t]*\\S-"
                            (min (point-max) 4000) t))))

(defun bcat-faces--reading-view ()
  "`after-change-major-mode-hook' (late, after the globalized modes):
no line numbers in a reading file unless `bcat-faces-line-numbers'."
  (when (and (not bcat-faces-line-numbers)
             (bound-and-true-p display-line-numbers-mode)
             (bcat-faces--reading-file-p))
    (display-line-numbers-mode -1)))

(add-hook 'after-change-major-mode-hook #'bcat-faces--reading-view 90)

(provide 'bcat-faces)
;;; bcat-faces.el ends here
