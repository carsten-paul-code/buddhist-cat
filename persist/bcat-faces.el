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
;;
;; 18.11.2026: „Gibt es eine Möglichkeit die Tabellen im .org besser
;; darzustellen? ich komme klar, aber ‚verirre‘ mich dennoch hin und
;; wieder.“  In his theme the words did NOT keep the text colour: every
;; row of an org table has the face `org-table' (modus-vivendi: a pale
;; blue), words and glosses looked the same.  Of three variants on TĀ
;; 1012 he chose „B mit Spaltenhervorhebung“: the row of the words in
;; Latin script bold in the text colour, the grid grey, the column of
;; the word at point marked.  The Uchen row stays as it is (its font and
;; height were tuned on 4.11.).

;;; Code:

(require 'org)
(require 'cl-lib)
(require 'face-remap)

(declare-function set-fontset-font "fontset.c" (fontset characters font-spec &optional frame add))
(declare-function fontset-list "fontset.c" ())
(declare-function create-fontset-from-fontset-spec "fontset" (fontset-spec &optional style-variant noerror))

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

(defun bcat-faces--parts-table-p (pos)
  "Non-nil when the table row at POS belongs to a table of a word's
parts (Carsten 5.11.: under a Sanskrit segment's translation, the
members as a header row): a table with a header row
\(`bcat-faces--header-table-p') that colours no ratings — the
comparison's overview has @@latex:\\cellcolor{…}@@ in every row."
  (and (bcat-faces--header-table-p pos)
       (save-excursion
         (goto-char pos)
         (not (string-match-p "\\\\cellcolor"
                              (buffer-substring-no-properties (org-table-begin) (org-table-end)))))))

(defun bcat-faces--parts-line-matcher (limit)
  "Font-lock matcher: the next line before LIMIT that gives the parts of
a word of one member under * Text or * Sanskrit (Carsten 5.11.:
„/jananī/: /√jan/ (1) be born · /-anī/ f. agent“) — group 1 the parts
\(blue; the word black).  A line of the same shape in a Structure
\(under * Segments) is none."
  (let ((found nil))
    (while (and (not found) (re-search-forward "^/[^/\n]+/: \\(.+\\)$" limit t))
      (when (save-excursion
              (save-match-data
                (and (re-search-backward "^\\* \\(.*\\)$" nil t)
                     (member (string-trim (match-string 1)) '("Text" "Sanskrit")))))
        (setq found t)))
    found))

(defun bcat-faces--table-rows (pos)
  "The lines of the table around POS, its rules included."
  (save-excursion
    (goto-char pos)
    (split-string (buffer-substring-no-properties (org-table-begin) (org-table-end)) "\n" t)))

(defun bcat-faces--resolution-table-p (pos)
  "Non-nil when the table row at POS belongs to the table of a
compound's resolution (18.11., TĀ 1016): no rule, three cells a row,
the middle one — the relation — in brackets."
  (let ((rows (bcat-faces--table-rows pos)))
    (and rows
         (cl-every (lambda (l)
                     (string-match-p "\\`[ \t]*|[^|\n]*|[ \t]*\\[[^|\n]*\\][ \t]*|[^|\n]*|[ \t]*\\'" l))
                   rows))))

(defun bcat-faces--relation-cell-matcher (limit)
  "Font-lock matcher: the next relation of a compound's resolution
before LIMIT — group 1 the cell in brackets (blue) — in a table of
resolution (`bcat-faces--resolution-table-p') under * Text or
* Sanskrit; the members and the words of the translation stay black."
  (let ((found nil))
    (while (and (not found)
                (re-search-forward "^[ \t]*|[^|\n]*|[ \t]*\\(\\[[^|\n]*\\]\\)[ \t]*|[^|\n]*|[ \t]*$" limit t))
      (when (save-match-data
              (and (bcat-faces--resolution-table-p (match-beginning 0))
                   (save-excursion
                     (and (re-search-backward "^\\* \\(.*\\)$" nil t)
                          (member (string-trim (match-string 1)) '("Text" "Sanskrit" "Philology"))))))
        (setq found t)))
    found))

(defconst bcat-faces--segment-heading "\\*+ \\(?:<<[^>\n]*>>\\)?⟦[0-9]+⟧"
  "A heading that is a Sanskrit segment (since 9.11.): ⟦N⟧, in a pair
with the target <<skt-N>> before it — at the second level, in the
Tibetan file of a pair since 18.11. at the third.")

(defun bcat-faces--sanskrit-table-p (pos)
  "Non-nil when a table at POS is one of a Sanskrit segment: the
heading it stands under is the segment's, ⟦N⟧
\(`bcat-faces--segment-heading').  A Tibetan segment is no heading — a
mark on a line of its own —, so no template and no section has to be
asked (until 18.11.: under * Text of a Sanskrit file, under * Sanskrit
of the Tibetan file of a pair; since then all under * Philology)."
  (save-excursion
    (save-match-data
      (goto-char pos)
      (end-of-line)
      (and (re-search-backward "^\\*+ " nil t)
           (looking-at bcat-faces--segment-heading)))))

(defun bcat-faces--sub-row-p (pos)
  "Non-nil when the table row at POS stands under the rule of a Sanskrit
segment's table (18.11.): it explains the words above — their parts, a
compound's members and its resolution."
  (save-excursion
    (save-match-data
      (goto-char pos)
      (beginning-of-line)
      (and (looking-at "[ \t]*|[^-\n]")
           (let ((rule nil))
             (while (and (not rule) (zerop (forward-line -1)) (looking-at "[ \t]*|"))
               (when (looking-at "[ \t]*|-") (setq rule t)))
             rule)
           (bcat-faces--sanskrit-table-p pos)))))

(defun bcat-faces--sub-cell-blue (cell)
  "(START . END) of what is blue in CELL, a cell under the rule of a
Sanskrit segment's table, or nil: of a step of a compound's resolution
\(„MEMBERS [RELATION] WORDS“) the relation; of a member's line
\(„MEMBER: MEANING ← PARTS“) from the arrow on, without a meaning
\(„MEMBER: PARTS“) the parts — a member with only its meaning stays
black —; any other cell (a part of a word) whole."
  (cond
   ((string-match "\\`[^ ]+ \\(\\[[^]]*\\]\\) " cell)
    (cons (match-beginning 1) (match-end 1)))
   ((string-match "\\`[^ /:|]+: " cell)
    (let ((rest (match-end 0)))
      (cond ((string-match "←" cell rest) (cons (match-beginning 0) (length cell)))
            ((string-match-p "/[^/ \t\n][^/\n]*/" (substring cell rest)) (cons rest (length cell))))))
   (t (cons 0 (length cell)))))

(defun bcat-faces--sub-cell-matcher (limit)
  "Font-lock matcher: the next piece before LIMIT that is blue under the
rule of a Sanskrit segment's table (`bcat-faces--sub-row-p',
`bcat-faces--sub-cell-blue') — group 0."
  (let ((found nil))
    (while (and (not found)
                (re-search-forward "|[ \t]*\\([^|\n]*[^|\n \t]\\)[ \t]*|" limit t))
      (let* ((b (match-beginning 1))
             (bar (1- (match-end 0)))
             (cell (match-string-no-properties 1))
             (blue (and (bcat-faces--sub-row-p b) (bcat-faces--sub-cell-blue cell))))
        ;; the closing bar opens the next cell
        (goto-char bar)
        (when blue
          (set-match-data (list (+ b (car blue)) (+ b (cdr blue))))
          (setq found t))))
    found))

(defun bcat-faces--detail-row-matcher (limit)
  "Font-lock matcher: the next table row before LIMIT that explains the
words — group 0 the row: in its band (rows between hlines) neither the
first (the words), nor the second when the first is Uchen (its Wylie),
nor the one next to last (the gloss); none of a metre table or of a
table with a header row — but in a table of a word's parts every row
under the members (5.11., `bcat-faces--parts-table-p'), save the row of
their meanings right under them (18.11.); none of a compound's
resolution (`bcat-faces--resolution-table-p')."
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
        (when (and i
                   ;; 18.11.: the rows under the rule of a Sanskrit
                   ;; segment's table have colours of their own
                   ;; (`bcat-faces--sub-cell-matcher')
                   (not (bcat-faces--sub-row-p beg))
                   (if (bcat-faces--parts-table-p beg)
                       ;; 5.11.: a table of a word's parts — all but the
                       ;; members' row; 18.11.: nor the row of their
                       ;; meanings — the first under the members, no
                       ;; /form/ in it.
                       (save-excursion
                         (goto-char beg)
                         (forward-line -1)
                         (and (looking-at "[ \t]*|")
                              (not (and (looking-at "[ \t]*|-")
                                        (not (string-match-p
                                              "/[^/ \n][^/\n]*/"
                                              (buffer-substring-no-properties beg end)))))))
                     (and (/= i 0) (/= i (- (length band) 2))
                          ;; 18.11.: the resolution of a compound
                          (not (bcat-faces--resolution-table-p beg))
                          (not (and uchen (= i 1)))
                          (not (bcat-faces--metre-table-p beg))
                          (not (bcat-faces--header-table-p beg)))))
          (set-match-data (list beg end))
          (setq found t))))
    found))

;; ----------------------------------------------------------------------------
;; The tables at a glance (18.11.): the row of the words, the grid, the
;; column of the word at point
;; ----------------------------------------------------------------------------

(defface bcat-word-face
  '((((background dark)) :foreground "#ffffff" :weight bold)
    (t :foreground "#000000" :weight bold))
  "The row of the words in Latin script — IAST, Wylie — in the table of a
segment.  Carsten 18.11.: „verirre mich dennoch hin und wieder.
Vielleicht auch die Zeile mit dem Wylie oder IAST in weiß, das ich sie
schneller finde, wenn wir Teile diskutieren“.  A colour of its own, not
the text colour by default: his theme (modus-vivendi) gives every row of
a table the pale blue of `org-table' — the words and their glosses
looked alike.  Bold as the original line above the tables."
  :group 'buddhist-cat)

(defface bcat-grid-face
  '((((background dark)) :foreground "#555555")
    (t :foreground "#b5b5b5"))
  "The bars and rules of the tables in a sentence file: they step back,
the words carry (18.11., variant B)."
  :group 'buddhist-cat)

(defface bcat-column-face
  '((((background dark)) :background "#2d4466")
    (t :background "#d8e4f6"))
  "The column of the word at point in a table of words
\(`bcat-faces-highlight-column')."
  :group 'buddhist-cat)

(defcustom bcat-faces-highlight-column t
  "Non-nil: in a sentence file the column of the word at point is marked
\(`bcat-column-face') — the word with its gloss, form and parts.
Carsten 18.11., of three variants on TĀ 1012: „B mit
Spaltenhervorhebung“ — to find a word again „wenn wir Teile
diskutieren“.  `bcat-faces-toggle-column' switches it."
  :type 'boolean
  :group 'buddhist-cat)

(defun bcat-faces--word-row-p (pos)
  "Non-nil when the table row at POS shows the words of a segment in
Latin script: the first row of its band (the rows between rules) — the
second when the first is their Uchen, or empty (the column of a shad
alone) —, a band of three rows or more.  None under the rule of a
Sanskrit segment's table (the parts of the words), of a metre table, of
a table with a header row (the comparison; a table of parts left under
Word Formation) or of a compound's resolution."
  (save-excursion
    (save-match-data
      (goto-char pos)
      (beginning-of-line)
      (let* ((beg (point))
             (band (bcat-faces--band beg))
             (i (cl-position beg band)))
        (and i (< i 2) (>= (length band) 3)
             (let ((first (progn (goto-char (car band))
                                 (buffer-substring-no-properties (point) (line-end-position)))))
               (= i (if (or (string-match-p "[ༀ-࿿]" first)
                            (string-match-p "\\`[ \t|]*\\'" first))
                        1 0)))
             (not (bcat-faces--sub-row-p beg))
             (not (bcat-faces--metre-table-p beg))
             (not (bcat-faces--header-table-p beg))
             (not (bcat-faces--resolution-table-p beg)))))))

(defun bcat-faces--word-row-matcher (limit)
  "Font-lock matcher: the next table row before LIMIT with the words of
a segment in Latin script (`bcat-faces--word-row-p') — group 0 the row."
  (let ((found nil))
    (while (and (not found) (re-search-forward "^[ \t]*|[^-\n].*$" limit t))
      (let ((beg (match-beginning 0)) (end (match-end 0)))
        (when (bcat-faces--word-row-p beg)
          (set-match-data (list beg end))
          (setq found t))))
    found))

(defun bcat-faces--grid-matcher (limit)
  "Font-lock matcher: the next piece of a table's grid before LIMIT —
group 0 a rule from its first bar to its end, or one bar of a row.  A
bar in a line that is no table row (the caesura in a verse) is none."
  (let ((found nil))
    (while (and (not found) (re-search-forward "|" limit t))
      (let* ((bar (match-beginning 0))
             (first (save-excursion (beginning-of-line) (skip-chars-forward " \t") (point))))
        (cond
         ;; not a table row
         ((not (eq (char-after first) ?|)))
         ;; a rule: one piece
         ((save-excursion (goto-char first) (looking-at "|-[-+|]*"))
          (set-match-data (list (match-beginning 0) (match-end 0)))
          (goto-char (match-end 0))
          (setq found t))
         (t
          (set-match-data (list bar (1+ bar)))
          (setq found t)))))
    found))

(defun bcat-faces--cell-separator ()
  "The regexp of what ends a cell in the table line at point: a bar; in
a rule a bar or a plus — a plus in a cell is text („DEM+QUANT“, „COP +
CONV“, Kadam 2003)."
  (save-excursion
    (beginning-of-line)
    (if (looking-at "[ \t]*|-") "[|+]" "|")))

(defun bcat-faces--cell-bounds (col)
  "(BEG . END) of the cell number COL (from 0) of the table line at
point — between its bars, in a rule between bar and plus —, or nil."
  (save-excursion
    (beginning-of-line)
    (skip-chars-forward " \t")
    (when (eq (char-after) ?|)
      (let ((end (line-end-position)) (i 0) (res nil) (start (1+ (point)))
            (sep (bcat-faces--cell-separator)))
        (forward-char 1)
        (while (and (not res) (re-search-forward sep end t))
          (if (= i col)
              (setq res (cons start (1- (point))))
            (setq i (1+ i) start (point))))
        res))))

(defun bcat-faces--column-cells (pos)
  "The cells (BEG . END), top to bottom, of the table column at POS that
belong to the word there, or nil.  In the table of a Sanskrit segment
the whole column — word, gloss, form, the rule, the parts under it
\(`bcat-faces--sanskrit-table-p'); in any other table of words the rows
of the band at POS: an old Tibetan table continues its words in a new
band.  nil beside the bars, outside a table, in a metre table and in a
table with a header row (the comparison)."
  (save-excursion
    (save-match-data
      (goto-char pos)
      (let ((first (save-excursion (beginning-of-line) (skip-chars-forward " \t") (point))))
        (when (and (eq (char-after first) ?|)
                   (> pos first)
                   (not (bcat-faces--metre-table-p pos))
                   (not (bcat-faces--header-table-p pos)))
          (let ((col -1) (sep (bcat-faces--cell-separator)))
            (goto-char first)
            (while (re-search-forward sep pos t) (setq col (1+ col)))
            (goto-char pos)
            (when (and (>= col 0) (bcat-faces--cell-bounds col))
              (let ((rows (if (bcat-faces--sanskrit-table-p pos)
                              (let ((end (org-table-end)) (rows '()))
                                (goto-char (org-table-begin))
                                (while (< (point) end)
                                  (push (point) rows)
                                  (forward-line 1))
                                (nreverse rows))
                            (if (save-excursion (goto-char first) (looking-at "|-"))
                                (list (line-beginning-position))
                              (bcat-faces--band pos)))))
                (delq nil (mapcar (lambda (row) (goto-char row) (bcat-faces--cell-bounds col))
                                  rows))))))))))

(defvar-local bcat-faces--column-overlays nil
  "The overlays that mark the column of the word at point.")

(defun bcat-faces--column-update ()
  "`post-command-hook' of `bcat-faces-mode': mark the column of the word
at point (`bcat-faces--column-cells') in the selected window, or take
the mark away — with `bcat-faces-highlight-column' nil always."
  (mapc #'delete-overlay bcat-faces--column-overlays)
  (setq bcat-faces--column-overlays nil)
  (when bcat-faces-highlight-column
    (dolist (cell (ignore-errors (bcat-faces--column-cells (point))))
      (let ((o (make-overlay (car cell) (cdr cell))))
        (overlay-put o 'face 'bcat-column-face)
        ;; below the region and the search matches
        (overlay-put o 'priority -10)
        (overlay-put o 'window (selected-window))
        (push o bcat-faces--column-overlays)))))

(defun bcat-faces-toggle-column ()
  "Switch the mark on the column of the word at point on or off
\(`bcat-faces-highlight-column')."
  (interactive)
  (setq bcat-faces-highlight-column (not bcat-faces-highlight-column))
  (bcat-faces--column-update)
  (message "Spalte des Worts unter dem Cursor: %s" (if bcat-faces-highlight-column "an" "aus")))

(defface bcat-discuss-face
  '((((background dark)) :foreground "#FFB454" :weight bold)
    (t :foreground "#B45F06" :weight bold))
  "What is to be discussed: the label of the „Discuss:“ line at a
segment and the title of the points under Translations compared.
Carsten 18.11.: „das grün/gelb/rot hilft mir dabei nicht wirklich weiter
und macht es eher unübersichtlich. Eigentlich also nur vielleicht
farblich hervorheben was es zu beachten gilt“ — one colour, used for
nothing else."
  :group 'buddhist-cat)

(defun bcat-faces--discuss-matcher (limit)
  "Font-lock matcher: the next mark of a point for the discussion before
LIMIT — group 1 the label „*Discuss:*“ („*Zu besprechen:*“) of a line,
or the title „Discussion points“ („Diskussionspunkte“) of their heading."
  (re-search-forward
   (concat "^\\(?:\\(?1:\\*\\(?:Discuss\\|Zu besprechen\\):\\*\\)"
           "\\|\\*\\*\\* \\(?1:Discussion points\\|Diskussionspunkte\\)[ \t]*$\\)")
   limit t))

(defface bcat-translation-face
  '((((background dark)) :slant italic :foreground "#7FCF9A")
    (t :slant italic :foreground "#1E6B3A"))
  "The „→“ line of a segment's translation under * Text — italic, set off
in colour (as `bcat-export-translation-color' in the PDF).  Draft B
\(5.11.): „einzeilig, kursiv“; Carsten 9.11., TĀ 1009: „hatten wir
nicht mal Original und Übersetzung hervorgehoben?“."
  :group 'buddhist-cat)

(defun bcat-faces--translation-line-p (pos)
  "Non-nil when the „→“ line at POS is a segment's translation.  In a
file of three parts (18.11.) it stands under * Philology, and the
heading right above it is * Philology itself or ** Tibetan / ** Sanskrit
\(a Tibetan segment: a mark on its line) or a Sanskrit segment's
\(`bcat-faces--segment-heading') — not ** Segment N: there a translator's
line.  In a file of the earlier form: under * Text, * Tibetan or
* Sanskrit; the translators stood under * Segments."
  (save-excursion
    (save-match-data
      (goto-char pos)
      (let ((section (save-excursion
                       (and (re-search-backward "^\\* \\(.*\\)$" nil t)
                            (string-trim (match-string-no-properties 1))))))
        (cond
         ((member section '("Text" "Tibetan" "Sanskrit")) t)
         ((equal section "Philology")
          (and (re-search-backward "^\\*+ " nil t)
               (or (looking-at "\\* Philology[ \t]*$")
                   (looking-at "\\*\\* \\(?:Tibetan\\|Sanskrit\\)[ \t]*$")
                   (looking-at bcat-faces--segment-heading)))))))))

(defun bcat-faces--translation-matcher (limit)
  "Font-lock matcher: the next „→“ line before LIMIT that is a segment's
translation (`bcat-faces--translation-line-p'), every language; group 0
the line."
  (let ((found nil))
    (while (and (not found) (re-search-forward "^→ .*$" limit t))
      (when (bcat-faces--translation-line-p (match-beginning 0))
        (setq found t)))
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

(defface bcat-tibetan-face
  '((t :family "Kailasa" :height 1.3))
  "Tibetan script in a sentence file.  Carsten 4.11. (reading on the
MacBook, „ziemlich viel verschenkter Platz“): with Tibetan Machine Uni 20
an Uchen line was 43 px high against 19 for Latin; of four variants he
chose Kailasa 18 (font height ≈ 23 px).  Relative height, so it follows
the default font and text scaling."
  :group 'buddhist-cat)

(defun bcat-faces--tibetan-matcher (limit)
  "Font-lock matcher: the next run of Tibetan script before LIMIT.  A
function — a Tibetan character range as a string keyword is dropped
beside others (§5.31)."
  (re-search-forward "[\u0F00-\u0FFF]+" limit t))

(defun bcat-faces--tibetan-fontset ()
  "The fontset of `bcat-tibetan-face' — the Tibetan block in the face's
family — set on the face; nil without a graphic display.  Carsten's
init fixes Tibetan Machine Uni 20 for U+0F00–U+0FFF in the default
fontset, and a fontset entry beats a face's family: measured 4.11.,
the face alone left the lines at 43 px, with its own fontset 27."
  (when (display-graphic-p)
    (let* ((name "-*-Menlo-regular-normal-normal-*-*-*-*-*-m-0-fontset-bcattibetan")
           (fs (or (car (member (downcase name) (mapcar #'downcase (fontset-list))))
                   (create-fontset-from-fontset-spec name))))
      (set-fontset-font fs '(#x0F00 . #x0FFF)
                        (font-spec :family (face-attribute 'bcat-tibetan-face :family nil t)))
      (set-face-attribute 'bcat-tibetan-face nil :fontset fs)
      fs)))

(defvar-local bcat-faces--shad-remap nil
  "The face remapping of `cp/tibetan-shad-face' in this buffer, or nil.")

(defun bcat-faces--remap-shads (on)
  "ON: the gold shads of Carsten's init (display overlays in
`cp/tibetan-shad-face') in `bcat-tibetan-face' — its family, its fontset
\(`bcat-faces--tibetan-fontset'), its height — they alone would keep the
lines high; off: as before.  The height is absolute, else it compounds
with the shad face's own 1.4.  The fontset comes by the face's name: a
face property list may not name one (6.11.: Emacs dropped the whole
remapping and logged „Invalid face attribute :fontset“ at every
redisplay of a shad)."
  (when bcat-faces--shad-remap
    (face-remap-remove-relative bcat-faces--shad-remap)
    (setq bcat-faces--shad-remap nil))
  (when (and on (facep 'cp/tibetan-shad-face))
    (setq bcat-faces--shad-remap
          (face-remap-add-relative
           'cp/tibetan-shad-face
           (list :height (round (* (face-attribute 'bcat-tibetan-face :height nil t)
                                   (face-attribute 'default :height))))
           'bcat-tibetan-face))))

(defun bcat-faces--bar-columns (line)
  "The columns of the bars of the table LINE as it is seen: org links by
their text (whether or not font-lock has hidden the rest yet)."
  (let ((seen (replace-regexp-in-string
               "\\[\\[\\(?:[^]]*\\]\\[\\)?\\([^]]*\\)\\]\\]" "\\1" line))
        (cols '()))
    (dotimes (i (length seen))
      (when (eq (aref seen i) ?|)
        (push (string-width (substring seen 0 i)) cols)))
    (nreverse cols)))

(defun bcat-faces--uchen-row-matcher (limit)
  "Font-lock matcher, applying itself: each table row before LIMIT with
Tibetan script, followed by a table row, gets on the padding before
each of its bars (but the first) `(space :align-to COL)' — one stretch
to the column of that bar in the row below, plus the width of the
line's prefix (org-indent).  Carsten 4.11.: the Uchen row did not
line up, Tibetan glyphs are not as wide as the monospace grid counts
them.  Returns nil."
  (while (re-search-forward "^[ \t]*|[^-\n].*[\u0F00-\u0FFF].*$" limit t)
    (let* ((beg (line-beginning-position)) (end (line-end-position))
           (below (save-excursion
                    (forward-line 1)
                    (and (looking-at "[ \t]*|[^-\n]")
                         (buffer-substring-no-properties (point) (line-end-position)))))
           (prefix (let ((p (get-text-property beg 'line-prefix)))
                     (if (stringp p) (string-width p) 0))))
      (when below
        (let ((cols (cdr (bcat-faces--bar-columns below))))
          (save-excursion
            (goto-char beg)
            (skip-chars-forward " \t")
            (forward-char 1)
            (while (and cols (search-forward "|" end t))
              (let* ((bar (1- (point)))
                     (pad (save-excursion (goto-char bar) (skip-chars-backward " ") (point)))
                     (col (+ prefix (pop cols))))
                ;; The whole padding as ONE stretch (one `eq' value): the
                ;; spaces would count in full beside the wider glyphs.
                (when (< pad bar)
                  (put-text-property pad bar 'display (list 'space :align-to col))))))))))
  nil)

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
    (bcat-faces--parts-line-matcher (1 'bcat-gloss-face prepend))
    (bcat-faces--relation-cell-matcher (1 'bcat-gloss-face prepend))
    (bcat-faces--sub-cell-matcher (0 'bcat-gloss-face prepend))
    (bcat-faces--word-row-matcher
     ("[^|\n]+" (progn (goto-char (match-beginning 0)) (match-end 0)) nil
      (0 'bcat-word-face prepend)))
    (bcat-faces--grid-matcher (0 'bcat-grid-face prepend))
    (bcat-faces--translation-matcher (0 'bcat-translation-face prepend))
    (bcat-faces--discuss-matcher (1 'bcat-discuss-face prepend))
    (bcat-faces--rating-matcher
     (3 'shadow prepend t)
     (2 (bcat-faces--rating-face (match-string 1)) prepend))
    (bcat-faces--header-matcher (0 '(face nil invisible bcat-header) prepend))
    (bcat-faces--tibetan-matcher (0 'bcat-tibetan-face prepend))
    (bcat-faces--uchen-row-matcher))
  "The font-lock keywords of `bcat-faces-mode'.")

;;;###autoload
(define-minor-mode bcat-faces-mode
  "Words against glosses in a sentence file: the words in the text
colour, blue the glosses in the line with the vocabulary and the rows
that explain the words — formation, parts, form — in the tables
\(Carsten 31.10.); the ratings of the comparison in green, yellow, red
\(2.11.); the keyword lines at the top folded behind #+TITLE (4.11.,
`bcat-faces-toggle-header'); Tibetan in `bcat-tibetan-face' (4.11.);
the row of the words in IAST or Wylie bold in `bcat-word-face', bars and
rules grey, the column of the word at point marked (18.11.,
`bcat-faces-toggle-column')."
  :lighter nil
  (if bcat-faces-mode
      (progn
        (font-lock-add-keywords nil bcat-faces--keywords 'append)
        (setq-local font-lock-extra-managed-props
                    (cl-union '(invisible display) font-lock-extra-managed-props))
        (add-to-invisibility-spec '(bcat-header . t))
        (bcat-faces--tibetan-fontset)
        (bcat-faces--remap-shads t)
        (add-hook 'post-command-hook #'bcat-faces--column-update nil t))
    (font-lock-remove-keywords nil bcat-faces--keywords)
    (remove-from-invisibility-spec '(bcat-header . t))
    (bcat-faces--remap-shads nil)
    (remove-hook 'post-command-hook #'bcat-faces--column-update t)
    (mapc #'delete-overlay bcat-faces--column-overlays)
    (setq bcat-faces--column-overlays nil))
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
