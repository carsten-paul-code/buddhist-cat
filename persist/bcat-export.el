;;; bcat-export.el --- C-c u H: the sentence file as PDF on the reMarkable -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;;; Commentary:
;; §5.74 step 4 (Carsten 28.10.2026): "Das Handout sollte keine eigene
;; Funktion mehr sein, sondern genau das Analyse File ins PDF exportieren
;; und auf den Remarkable schieben" — one PDF per sentence; C-c u G ends
;; with it, by hand only after own additions.  The PDF: no title block
;; (#+OPTIONS of the template), tables flush left at natural width (no
;; centring, no tabularx from Carsten's init), a running head
;; "folder · work · Satz N · verse" with the page number.  All export
;; settings are bound here; the file stays free of them.
;;
;; For sources without a template yet (Sanskrit) C-c u H still runs the
;; old handout until their template exists.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'bcat-sentence)
(require 'bcat-status)
(require 'ucs-normalize)

(declare-function tibetan-sentence--source-file-from-analysis "tibetan-sentence-persist" (analysis-file))
(declare-function tibetan-sentence--sent-id-from-filename "tibetan-sentence-persist" (filename))
(declare-function tibetan-sentence--filepath "tibetan-sentence-persist" (sent-num &optional folder source-file))
(declare-function tibetan-cascade--collect-sentences "tibetan-cascade" ())
(declare-function bcat-commentary-lead "bcat-commentary" (commentary-source))
(declare-function tibetan-analysis-make-short-name "tibetan-analysis-persist" (source-file))
(declare-function tibetan-cascade--scope-at-point "tibetan-cascade" ())
(declare-function tibetan-discussion-paper--upload "tibetan-discussion-paper" (pdf title label &optional done))
(declare-function tibetan-discussion-paper--temp-source-p "tibetan-discussion-paper" (file))
(declare-function tibetan-handout "tibetan-handout" (&optional org-only))
(declare-function bcat-relayout-file-context "bcat-relayout" (file))
(declare-function org-latex-export-to-pdf "ox-latex")
(declare-function org-end-of-subtree "org" (&optional invisible-ok to-heading element))
(declare-function org-export-derived-backend-p "ox" (backend &rest backends))
(defvar org-latex-compiler)
(defvar org-latex-remove-logfiles)
(defvar org-latex-logfiles-extensions)
(defvar org-latex-pdf-process)
(defvar org-latex-tables-centered)
(defvar org-export-filter-table-functions)
(defvar org-export-filter-verse-block-functions)
(defvar org-export-filter-paragraph-functions)
(defvar org-export-before-processing-functions)
(defvar org-export-headline-levels)
(defvar org-latex-prefer-user-labels)
(defvar tibetan-cascade-subtree-confirm-threshold)

(defcustom bcat-export-auto t
  "When non-nil, C-c u G ends with C-c u H: the freshly analysed
sentence file goes to PDF and onto the reMarkable."
  :type 'boolean
  :group 'buddhist-cat)

(defcustom bcat-export-upload t
  "When non-nil, C-c u H uploads the PDF with rmapi (folder = the work,
document „Satz N“); nil only writes the PDF next to the file."
  :type 'boolean
  :group 'buddhist-cat)

;; ----------------------------------------------------------------------------
;; Running head
;; ----------------------------------------------------------------------------

(defun bcat-export--latex-escape (s)
  "S with the LaTeX special characters escaped."
  (mapconcat (lambda (c)
               (pcase c
                 (?\\ "\\textbackslash{}")
                 (?~ "\\textasciitilde{}")
                 (?^ "\\textasciicircum{}")
                 ((or ?& ?% ?# ?_ ?$ ?{ ?}) (string ?\\ c))
                 (_ (string c))))
             (or s "") ""))

(defun bcat-export-head-lines (folder work sent-num verse)
  "The #+LATEX_HEADER lines of the running head: FOLDER · WORK
\(italic) · Satz SENT-NUM · VERSE (left), the page number (right);
without SENT-NUM (a commentary document) no „Satz“."
  (concat
   "#+LATEX_HEADER: \\usepackage{fancyhdr}\\pagestyle{fancy}\\fancyhf{}\\renewcommand{\\headrulewidth}{0.4pt}\n"
   (format "#+LATEX_HEADER: \\fancyhead[L]{\\small %s}\\fancyhead[R]{\\small\\thepage}\n"
           (mapconcat #'identity
                      (delq nil (list (bcat-export--latex-escape folder)
                                      (format "\\textit{%s}" (bcat-export--latex-escape work))
                                      (and sent-num (format "Satz %d" sent-num))
                                      (and verse (bcat-export--latex-escape verse))))
                      " · "))))

(defun bcat-export--title (file)
  "FILE's #+TITLE up to „ — “, or nil."
  (let ((title (and file (bcat-sentence--header-value file "TITLE"))))
    (and title (string-trim (car (split-string title " — "))))))

(defcustom bcat-export-semester-regexp "\\`\\(?:WS\\|SS\\)[0-9]"
  "Folder names of semesters (WS26-27, SS26): a text's folder is the one
right below (`bcat-export--text-folder')."
  :type 'regexp
  :group 'buddhist-cat)

(defun bcat-export--text-folder (source)
  "The top folder of SOURCE's text: the one right below a semester folder
\(`bcat-export-semester-regexp') — Tantraloka/, Tibetisch V/, and for AS
and MSA with their language folders Abhidharmasamuccaya/,
Mahayanasutralamkara/; without a semester SOURCE's own folder."
  (let* ((own (file-name-directory (expand-file-name source)))
         (dir own)
         (found nil))
    (while (and dir (not found))
      (let ((parent (file-name-directory (directory-file-name dir))))
        (cond ((string-match-p bcat-export-semester-regexp
                               (file-name-nondirectory (directory-file-name parent)))
               (setq found dir))
              ((equal parent dir) (setq dir nil))
              (t (setq dir parent)))))
    (or found own)))

(defun bcat-export--handout (source name)
  "Where the PDF named NAME of SOURCE goes: NAME.pdf in the handouts
folder at the top of the text (`bcat-export--text-folder'; Carsten
31.10.: „Handouts kommen in den handouts Ordner auf der obersten Ebene
des Textes“), or in the folder SOURCE names with #+CAT_HANDOUTS:.  The
name ASCII like the old handouts (msa-par-60.pdf): diacritics dropped,
anything else but letters, digits and dots a hyphen; nil without SOURCE."
  (when source
    (let ((ascii (replace-regexp-in-string
                  "[^A-Za-z0-9.]+" "-"
                  (replace-regexp-in-string "[^[:ascii:]]" "" (ucs-normalize-NFD-string name))))
          (own (and (file-readable-p source) (bcat-sentence--header-value source "CAT_HANDOUTS"))))
      (expand-file-name (concat (string-trim ascii "-+" "-+") ".pdf")
                        (if own
                            (expand-file-name own (file-name-directory source))
                          (expand-file-name "handouts" (bcat-export--text-folder source)))))))

(defun bcat-export--commentary-info (file)
  "`bcat-export-info' for the commentary document FILE: the folder of
the commentary source, the work of the verses it comments on (beside
their PDFs on the reMarkable), the verses from #+CAT_VERSE, named after
the document; on disk beside the verses' PDFs too (savbh-IX.60.pdf in
the handouts folder of the text)."
  (let* ((src (tibetan-sentence--source-file-from-analysis file))
         (lead (and src (bcat-commentary-lead src)))
         (verse-line (bcat-sentence--header-value file "CAT_VERSE"))
         (verses (let ((pos 0) (out '()))
                   (while (and verse-line (string-match "\\]\\[\\([^]]+\\)\\]\\]" verse-line pos))
                     (push (match-string 1 verse-line) out)
                     (setq pos (match-end 0)))
                   (nreverse out))))
    (list :source src
          :folder (and src (file-name-nondirectory
                            (directory-file-name (file-name-directory src))))
          :work (or (bcat-export--title lead) (and lead (file-name-base lead)) (file-name-base file))
          :sent-num nil
          :verse (and verses (string-join verses ", "))
          :label (or (bcat-sentence--header-value file "TITLE") (file-name-base file))
          :handout (let* ((link (bcat-sentence--header-value file "SOURCE"))
                          (section (and link (string-match "::\\*\\(?:Section \\)?\\([^]]+\\)\\]" link)
                                        (match-string 1 link))))
                     ;; Beside the verses' PDFs (Carsten 30.10.): their
                     ;; source's handouts/, the commentary's own name.
                     (and src section
                          (bcat-export--handout
                           (or lead src) (concat (tibetan-analysis-make-short-name src) "-" section)))))))

(defun bcat-export-info (file)
  "What the PDF of the sentence FILE is labelled with: (:source :folder
:work :sent-num :verse :label :handout) — folder and work title from the
source \(its #+TITLE before „ — “), the verse from its :VERSE:; LABEL
the name on the reMarkable (the verse, else „Satz N“), HANDOUT the copy
in the source's handouts/ (`bcat-export--handout').  A commentary
document: `bcat-export--commentary-info'."
  (if (equal (bcat-sentence-file-p file) "commentary")
      (bcat-export--commentary-info file)
    (bcat-export--sentence-info file)))

(defun bcat-export--sentence-info (file)
  "`bcat-export-info' for the sentence FILE."
  (let* ((src (tibetan-sentence--source-file-from-analysis file))
         (n (tibetan-sentence--sent-id-from-filename file))
         (text (if (and src (file-readable-p src))
                   (with-temp-buffer (insert-file-contents src) (buffer-string))
                   ""))
         (title (and src (file-readable-p src) (bcat-export--title src)))
         (verse (and src
                     (plist-get (cl-find n (with-temp-buffer
                                             (insert text)
                                             (org-mode)
                                             (tibetan-cascade--collect-sentences))
                                         :key (lambda (s) (plist-get s :sent-num)))
                                :verse))))
    (list :source src
          :folder (and src (file-name-nondirectory
                            (directory-file-name (file-name-directory src))))
          :work (or title (and src (file-name-base src)) (file-name-base file))
          :sent-num n
          :verse verse
          :label (or verse (format "Satz %d" n))
          :handout (and src
                        (bcat-export--handout
                         src (concat (tibetan-analysis-make-short-name src) "-"
                                     (if verse (car (last (split-string verse)))
                                       (format "satz-%d" n))))))))

;; ----------------------------------------------------------------------------
;; Export and upload
;; ----------------------------------------------------------------------------

(defcustom bcat-export-tibetan-font "Noto Serif Tibetan"
  "Font of the Uchen lines in the PDF (fontspec name, HarfBuzz) — the
font of the old handout's Uchen line."
  :type 'string
  :group 'buddhist-cat)

(defcustom bcat-export-devanagari-font "Noto Sans Devanagari"
  "Font of the Devanāgarī lines in the PDF (fontspec name, HarfBuzz).
A fallback font without Indic shaping shows the virāma instead of the
conjunct (29.10.: Arial Unicode MS)."
  :type 'string
  :group 'buddhist-cat)

(defcustom bcat-export-gloss-color "1F4E9E"
  "Colour (HTML hex) of what explains the words: the glosses in brackets
in the line with the vocabulary and, in the tables, the rows that
explain the words (formation, parts, form; not the gloss, TĀ 1006).
Carsten 31.10.: on the reMarkable Paper Pro the words of a
segment were hard to keep apart from their glosses; words in blue came
out greyish and lost contrast — „kehren wir es einfach so um“: the
words black, the rest blue."
  :type 'string
  :group 'buddhist-cat)

(defun bcat-export--font-lines ()
  "#+LATEX_HEADER lines defining \\bcattib and \\bcatdeva, the colour
\\bcatg (gloss, `bcat-export-gloss-color'), loading adjustbox
\(`bcat-export--table-filter'), and colortbl with the colours ok, mid,
bad of the ratings in the comparison's overview (2.11., Muster
vergleich-ta1-1.org, `bcat-analysis--rating-colours')."
  (concat
   "#+LATEX_HEADER: \\usepackage{fontspec}\n"
   "#+LATEX_HEADER: \\usepackage{adjustbox}\n"
   "#+LATEX_HEADER: \\usepackage{xcolor}\n"
   "#+LATEX_HEADER: \\usepackage{colortbl}\n"
   "#+LATEX_HEADER: \\definecolor{ok}{RGB}{200,235,200}\\definecolor{mid}{RGB}{250,235,160}\\definecolor{bad}{RGB}{245,190,185}\n"
   (format "#+LATEX_HEADER: \\definecolor{bcatgloss}{HTML}{%s}\n" bcat-export-gloss-color)
   "#+LATEX_HEADER: \\newcommand{\\bcatg}[1]{\\textcolor{bcatgloss}{#1}}\n"
   (format "#+LATEX_HEADER: \\newfontfamily\\bcattib{%s}[Script=Tibetan,Renderer=HarfBuzz]\n"
           bcat-export-tibetan-font)
   (format "#+LATEX_HEADER: \\newfontfamily\\bcatdeva{%s}[Script=Devanagari,Renderer=HarfBuzz]\n"
           bcat-export-devanagari-font)))

(defun bcat-export--script-of (line)
  "`tibetan' or `devanagari' when LINE is written in that script (no
Latin letters), else nil."
  (unless (string-match-p "[A-Za-z]" (replace-regexp-in-string "\\\\[a-z]+" "" line))
    (cond ((string-match-p "[\u0F00-\u0FFF]" line) 'tibetan)
          ((string-match-p "[\u0900-\u097F]" line) 'devanagari))))

(defun bcat-export--foreign-signs (body script)
  "BODY of a line in SCRIPT (`tibetan' or `devanagari') with every run of
signs of neither the script nor ASCII set in the main font — 30.10.,
TĀ 1001: Noto Sans Devanagari has no ¦, the recitation's pause sign,
and the PDF showed an empty box."
  (replace-regexp-in-string
   (if (eq script 'tibetan)
       "[^[:ascii:]\u0F00-\u0FFF\u200C\u200D]+"
     "[^[:ascii:]\u0900-\u097F\u1CD0-\u1CFF\uA8E0-\uA8FF\u200C\u200D]+")
   (lambda (signs) (format "{\\normalfont %s}" signs))
   body t t))

(defun bcat-export--verse-scripts (latex)
  "LATEX of a verse block with each line in the original script set in
its own font, larger (\\bcattib / \\bcatdeva, \\large — the old
handout's Uchen line), no page break before the transcription line
that follows it and a little space after that one (Carsten 29.10.:
Uchen above Wylie, Devanāgarī above IAST).  Roman-only lines stay as
they are."
  (let ((after-script nil))
    (mapconcat
     (lambda (line)
       (let* ((end (if (string-match "\\\\\\\\[ \t]*\\'" line)
                       (match-beginning 0)
                     (length line)))
              (body (substring line 0 end))
              (brk (substring line end))
              (script (and (not (string-prefix-p "\\" line))
                           (bcat-export--script-of body))))
         (cond
          (script
           (setq after-script t)
           ;; `\\*': no page break before the transcription (30.10.).
           (format "{%s\\large %s}%s"
                   (if (eq script 'tibetan) "\\bcattib" "\\bcatdeva")
                   (bcat-export--foreign-signs body script)
                   (if (string-prefix-p "\\\\" brk) (concat "\\\\*" (substring brk 2)) brk)))
          ((and after-script (string-prefix-p "\\\\" brk))
           (setq after-script nil)
           (concat body "\\\\[1ex]"))
          (t (setq after-script nil) line))))
     (split-string latex "\n")
     "\n")))

(defun bcat-export--verse-units (latex)
  "LATEX of a verse block with each glossed line made of unbreakable
units — every word with its [gloss] in an \\mbox, the shad tied to the
last by `~' — so a long line breaks only between units: 29.10., the
glossed bDen 9 broke inside „[ob-struction]“ and left „/“ alone on a
line.  Such a verse is set ragged right; justified, TeX let the
longest line run into the margin rather than stretch its few spaces.
Lines without a gloss (or starting with a command) stay as they are."
  (let ((out (bcat-export--verse-lines latex)))
    (if (equal out latex)
        latex
      (replace-regexp-in-string "\\\\begin{verse}\n"
                                "\\&\\\\rightskip=0pt plus 1fil\\\\relax%\n"
                                out t))))

(defcustom bcat-export-unit-max 50
  "Longest word+gloss unit (characters) that a glossed verse line keeps
unbroken; a longer one may break between the words of its gloss —
30.10., TĀ 1001: one unit of 110 characters ran into the margin."
  :type 'integer
  :group 'buddhist-cat)

(defun bcat-export--unit-boxes (unit)
  "UNIT (\"word [gloss]\") as \\mbox text: whole, or — longer than
`bcat-export-unit-max' — one box per gloss word, the word with the
first, the bracket with the last."
  (let ((open (string-match " \\[" unit)))
    (cond
     ((null open) (format "\\mbox{%s}" unit))
     ;; The gloss in \\bcatg, the word as it is (Carsten 31.10.).
     ((<= (string-width unit) bcat-export-unit-max)
      (format "\\mbox{%s \\bcatg{%s}}" (substring unit 0 open) (substring unit (1+ open))))
     (t
      (let ((words (split-string (substring unit (+ open 2)))))
        (concat (format "\\mbox{%s \\bcatg{[%s}}" (substring unit 0 open) (car words))
                (mapconcat (lambda (w) (format " \\mbox{\\bcatg{%s}}" w)) (cdr words) "")))))))

(defun bcat-export--verse-lines (latex)
  "LATEX with each glossed verse line made of \\mbox units (see
`bcat-export--verse-units')."
  (mapconcat
   (lambda (line)
     (if (or (not (string-match-p "]" line))
             (string-prefix-p "\\" line))
         line
       (let* ((end (if (string-match "\\\\\\\\[ \t]*\\'" line)
                       (match-beginning 0)
                     (length line)))
              (body (substring line 0 end))
              (units '())
              (pos 0))
         (while (string-match "[^]]*?]" body pos)
           ;; Read the match before `string-trim', which matches too.
           (let ((unit (match-string 0 body)))
             (setq pos (match-end 0))
             (push (bcat-export--unit-boxes (string-trim unit)) units)))
         (concat (mapconcat #'identity (nreverse units) " ")
                 (replace-regexp-in-string "[ \t]+" "~"
                                           (string-trim-right (substring body pos)))
                 (substring line end)))))
   (split-string latex "\n")
   "\n"))

(defun bcat-export--verse-filter (text backend _info)
  "`org-export-filter-verse-block-functions' entry: TEXT through
`bcat-export--verse-units' and `bcat-export--verse-scripts' when
exporting to LaTeX (BACKEND)."
  (if (org-export-derived-backend-p backend 'latex)
      (bcat-export--verse-scripts (bcat-export--verse-units text))
    text))

(defun bcat-export--table-filter (text backend _info)
  "`org-export-filter-table-functions' entry: a LaTeX tabular no wider
than the line — scaled down when wider (adjustbox, max width), else as
it is.  30.10., TĀ 1001: the metre table (19 syllables and the pause)
ran off the page."
  (if (and (org-export-derived-backend-p backend 'latex)
           (string-match-p "\\\\begin{tabular}" text))
      ;; The blank line after the table stays (30.10.: cut, the next
      ;; paragraph ran on beside the box — „Translation:“ in the table).
      (let ((end (if (string-match "\n*\\'" text) (match-beginning 0) (length text))))
        (concat "\\begin{adjustbox}{max width=\\linewidth}\n"
                (bcat-export--detail-rows (bcat-export--table-scripts (substring text 0 end))) "\n"
                "\\end{adjustbox}"
                (let ((tail (substring text end)))
                  (if (string-empty-p tail) "\n" tail))))
    text))

(defun bcat-export--table-scripts (tabular)
  "TABULAR with each Tibetan run in the Tibetan font, larger — the Uchen
row of a Tibetan table (Carsten 31.10. abends, Tibetisch V); the main
font has no Tibetan."
  (replace-regexp-in-string "[\u0F00-\u0FFF]+"
                            (lambda (run) (format "{\\bcattib\\large %s}" run))
                            tabular t t))

(defun bcat-export--metre-row-p (row)
  "Non-nil when the LaTeX table ROW holds only metre signs (– ⏑ ¦)."
  (string-match-p "\\`[–⏑¦&|\\\\ \t]*\\'" row))

(defun bcat-export--cells (row)
  "The cells of the LaTeX table ROW (without its \\\\), split at every
& that is not escaped (\\&) — also between empty cells: 2.11., TĀ 1006,
the parts row of three pronouns, „ &  & “, became „ & \\bcatg{&}“ when
the space-greedy separator ate the room before the second &."
  (let ((cells '()) (start 0) (pos 0))
    (while (string-match "&" row pos)
      (let ((i (match-beginning 0)))
        (unless (and (> i 0) (eq (aref row (1- i)) ?\\))
          (push (substring row start i) cells)
          (setq start (1+ i)))
        (setq pos (1+ i))))
    (nreverse (cons (substring row start) cells))))

(defun bcat-export--parts-line-filter (text backend _info)
  "`org-export-filter-paragraph-functions' entry: a paragraph that is
the line of a word of one member — „\\emph{jananī}: \\emph{√jan} (1) be
born · …“ (Carsten 5.11., the parts under a Sanskrit segment's
translation) — with its parts in \\bcatg, the word black.  A Structure
paragraph with lines of that shape stays as it is."
  (if (and (org-export-derived-backend-p backend 'latex)
           (string-match "\\`\\(\\\\emph{[^}\n]+}:\\) \\([^\n]+\\)\\(\n*\\)\\'" text))
      (concat (match-string 1 text) " \\bcatg{" (match-string 2 text) "}" (match-string 3 text))
    text))

(defun bcat-export--detail-rows (tabular)
  "TABULAR (LaTeX) with the cells of the rows that explain the words —
formation, parts, form — in \\bcatg: in each band of the table (rows
between \\hline) every row but the first (the words), the second when
the first is Uchen (its Wylie) and the one next to last (the gloss).
Carsten 31.10.: words black, the rest blue; the same evening, TĀ 1006:
the gloss black too — „dann sehe ich gleich was wichtig ist“;
Tibetisch V: Uchen and Wylie black.  A metre table (its second row only
– and ⏑) stays as it is, as does the comparison's overview (2.11.: a
header row — one row, then \\hline — and \\cellcolor); a table of a
word's parts (5.11.: the members as a header row) has every row but the
members' in \\bcatg."
  (let* ((lines (split-string tabular "\n"))
         (row-p (lambda (l) (string-match-p "\\\\\\\\[ \t]*\\'" l)))
         (rows (cl-remove-if-not row-p lines))
         (header (let ((first (cl-position-if row-p lines)))
                   (and first (nth (1+ first) lines)
                        (string-match-p "\\`[ \t]*\\\\hline" (nth (1+ first) lines))))))
    (if (or (< (length rows) 2)
            (and header (string-match-p "\\\\cellcolor" tabular))
            (bcat-export--metre-row-p (nth 1 rows)))
        tabular
      (let ((blue '()) (band '()))
        (if header
            ;; 5.11.: a table of a word's parts — the members, \hline, the
            ;; parts: every row but the members'.
            (setq blue (cdr rows))
          ;; The bands, each closed by an \hline or the end.
          (dolist (l (append lines (list "\\hline")))
            (cond ((funcall row-p l) (push l band))
                  ((string-match-p "\\`[ \t]*\\\\hline" l)
                   (let* ((b (nreverse band)) (n (length b))
                          (uchen (and b (string-match-p "[\u0F00-\u0FFF]" (car b)))))
                     (cl-loop for r in b for i from 0
                              unless (or (= i 0) (and uchen (= i 1)) (= i (- n 2)))
                              do (push r blue)))
                   (setq band '())))))
        (mapconcat
         (lambda (l)
           (if (not (memq l blue))
               l
             (let* ((body (replace-regexp-in-string "\\\\\\\\[ \t]*\\'" "" l))
                    (cells (bcat-export--cells body)))
               (if (cl-every #'string-blank-p cells)
                   l
                 (concat (mapconcat (lambda (c)
                                      (let ((c (string-trim c)))
                                        (if (string-empty-p c) "" (format "\\bcatg{%s}" c))))
                                    cells " & ")
                         "\\\\")))))
         lines "\n")))))

(defvar bcat-export--missing nil
  "The missing characters of the last `bcat-export-pdf', ((CHAR . FONT) …).")

(defun bcat-export--missing-characters (log)
  "The characters LuaLaTeX's LOG reports missing, ((CHAR . FONT) …) in
order, each once — 30.10., TĀ 1001: ¦ came out as a box, and the status
said „PDF ✓“.  The log quotes the font: in font \"name:Times New Roman:…\"."
  (let ((pos 0) (out '()))
    (while (string-match "Missing character: There is no \\(.\\) (U\\+[0-9A-F]+) in font \"?\\(?:name:\\)?\\([^:/;!\"]+\\)" log pos)
      (let ((hit (cons (match-string 1 log) (string-trim (match-string 2 log)))))
        (setq pos (match-end 0))
        (unless (member hit out) (push hit out))))
    (nreverse out)))

(defun bcat-export--missing-text (missing)
  "MISSING ((CHAR . FONT) …) for the status line: „¦ (Noto Sans
Devanagari)“, the characters of one font together, an invisible one
by its code („U+009A“)."
  (mapconcat (lambda (font)
               (format "%s (%s)"
                       (mapconcat (lambda (m)
                                    ;; An invisible character by its code
                                    ;; (30.10.: U+009A from a mis-decoded „Ś“).
                                    (let ((c (string-to-char (car m))))
                                      (if (memq (get-char-code-property c 'general-category) '(Cc Cf Zs))
                                          (format "U+%04X" c)
                                        (car m))))
                                  (cl-remove-if-not (lambda (m) (equal (cdr m) font)) missing)
                                  " ")
                       font))
             (delete-dups (mapcar #'cdr missing))
             ", "))

(define-obsolete-variable-alias 'bcat-analysis-sanskrit-table-width
  'bcat-export-wide-table-width "5.11.2026")

(defcustom bcat-export-wide-table-width 120
  "Widest cell (plus 4) of any table in the PDF that the export does not
lay out anew (`bcat-export--rechunk-gloss-tables' takes the tables of
the segments): a wider cell is broken (`bcat-export--wrap-wide-tables').
120 (30.10., then the width of the Sanskrit tables): in the PDF of AS
sentence 3 a table of 115 characters fits the page, one of 134 runs
into the margin.  5.11.: every language, its old name an alias."
  :type 'integer
  :group 'buddhist-cat)

(defun bcat-export--wrap-wide-tables (&rest _)
  "In the current (export) buffer: every org table with a cell wider than
`bcat-export-wide-table-width' gets that cell broken at its „·“
into further lines (`bcat-analysis-wrap-rows') — 31.10., TĀ 1008 and
five more files written before: a one-column table wider than the page,
shrunk unreadable in the PDF.  Tables with rules (|---) and narrow ones
stay as they are; the file is not touched.  For
`org-export-before-processing-functions'."
  (let ((max (- bcat-export-wide-table-width 4)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "^|" nil t)
        (let* ((beg (line-beginning-position))
               (end (progn (while (and (zerop (forward-line 1)) (looking-at "|"))) (point)))
               (lines (split-string (buffer-substring-no-properties beg end) "\n" t))
               (rows (mapcar (lambda (l) (mapcar #'string-trim
                                                 (split-string (string-trim l "[ \t]*|" "|[ \t]*") "|")))
                             lines)))
          (when (and (not (cl-some (lambda (l) (string-prefix-p "|-" l)) lines))
                     (cl-some (lambda (row) (cl-some (lambda (c) (> (string-width c) max)) row)) rows))
            (delete-region beg end)
            (goto-char beg)
            (insert (string-trim-right (tibetan-gloss-table--format-rows
                                        (bcat-analysis-wrap-rows rows max)))
                    "\n")))))))

(define-obsolete-variable-alias 'bcat-export-tibetan-table-width
  'bcat-export-table-width "5.11.2026")

(defcustom bcat-export-table-width 130
  "Widest table of a segment's words in the PDF — every language (5.11.;
before, Tibetan only) —, counted as each column's widest line plus 6
\(in LaTeX a column costs more than its characters).  30.10.,
Sthiramati's prose: a segment of 23 words ran far into the margin;
measured: an AS table of 6 columns and 71 characters fills 60 % of the
page (and a Sanskrit table of 115 characters, 120 by this measure,
fits).  The sentence file is wider (`bcat-analysis-table-width'), the
export lays the tables out anew (`bcat-export--rechunk-gloss-tables')."
  :type 'integer
  :group 'buddhist-cat)

(defun bcat-export--gloss-run-start-p ()
  "Non-nil when the table at point begins the tables of a segment: the
line above it — blank lines skipped — is a ⟦N⟧ mark, or its first row
is in Tibetan script (files without marks)."
  (or (looking-at "|[^-\n].*[\u0F00-\u0FFF]")
      (save-excursion
        (and (zerop (forward-line -1))
             (progn (while (and (looking-at "[ \t]*$") (zerop (forward-line -1))))
                    (looking-at "\\(?:<<[^>\n]*>>\\)?⟦[0-9]+⟧[^\n]*$"))))))

(defun bcat-export--rechunk-gloss-tables (&rest _)
  "In the current (export) buffer: the tables of each segment — a run of
tables under a ⟦N⟧ mark (or with a first row in Tibetan script), only
blank lines between, none with a rule — laid out anew for the page in
the generic layout (`bcat-export-table-width', 6 per column, wide cells
broken as far as that saves lines, `bcat-analysis--best-chunks').
4.11. Tibetan, 5.11. every language: the file fills the Emacs window,
the PDF keeps its width.  For `org-export-before-processing-functions'
— the file stays as it is."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^|" nil t)
      (goto-char (line-beginning-position))
      (if (or (save-excursion (and (zerop (forward-line -1)) (looking-at "[ \t]*|")))
              (not (bcat-export--gloss-run-start-p)))
          (forward-line 1)
        (let ((start (point)) (end nil) (tables '()))
          (while (and (looking-at "|")
                      (not (save-excursion
                             (let ((rule nil))
                               (while (and (not rule) (looking-at "|"))
                                 (setq rule (looking-at "|-"))
                                 (forward-line 1))
                               rule))))
            (let ((tb (point)))
              (while (looking-at "|") (forward-line 1))
              (push (buffer-substring-no-properties tb (point)) tables)
              (setq end (point))
              (skip-chars-forward "\n")))
          (if (null tables)
              (forward-line 1)
            (let* ((columns (apply #'append (mapcar #'bcat-analysis--table-columns (nreverse tables))))
                   (best (bcat-analysis--best-chunks columns bcat-export-table-width 6
                                                     bcat-analysis-cell-width)))
              (delete-region start end)
              (goto-char start)
              (insert (bcat-analysis--format-chunks (car best) (cdr best)) "\n"))))))))

(defun bcat-export--rechunk-header-tables (&rest _)
  "In the current (export) buffer: every table of the parts of a word —
a header row, its rule, rows below, no other rule — wider than
`bcat-export-table-width' (measured as the tables of the words: each
column's widest line plus 6) split into tables of its own, each with
the header and rule of its columns; a row empty in one of them is left
out (it holds parts of members in another).  5.11., TĀ 1.1 ⟦2⟧: seven
members side by side were 558 pt on a line of 466 and shrunk to 84 %.
A table under #+ATTR_… (the comparison's overview, fixed columns)
stays as it is; the file is not touched.  For
`org-export-before-processing-functions'."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^|" nil t)
      (let* ((beg (line-beginning-position))
             (end (progn (while (and (zerop (forward-line 1)) (looking-at "|"))) (point)))
             (lines (split-string (buffer-substring-no-properties beg end) "\n" t))
             (attr (save-excursion (goto-char beg)
                                   (and (zerop (forward-line -1)) (looking-at-p "[ \t]*#\\+ATTR_")))))
        (when (and (not attr) (> (length lines) 2)
                   (string-prefix-p "|-" (nth 1 lines))
                   (not (cl-some (lambda (l) (string-prefix-p "|-" l)) (cddr lines))))
          (let* ((rows (mapcar (lambda (l) (mapcar #'string-trim (butlast (cdr (split-string l "|")))))
                               (cons (car lines) (cddr lines))))
                 (columns (apply #'cl-mapcar #'list rows))
                 (chunks (bcat-analysis--chunk-columns columns bcat-export-table-width 6
                                                       bcat-export-table-width)))
            (when (cdr chunks)
              (delete-region beg end)
              (goto-char beg)
              (insert (mapconcat (lambda (chunk)
                                   (bcat-analysis--header-table
                                    (cl-loop for r below (length rows)
                                             for row = (mapcar (lambda (col) (nth r col)) chunk)
                                             unless (and (> r 0) (cl-every #'string-empty-p row))
                                             collect row)))
                                 chunks "\n\n")
                      "\n"))))))))

(define-obsolete-function-alias 'bcat-export--rechunk-tibetan-tables
  #'bcat-export--rechunk-gloss-tables "5.11.2026")

(defun bcat-export--marks-apart (&rest _)
  "In the current (export) buffer: a blank line between a ⟦N⟧ mark line
and the table right under it.  Carsten 4.11.: in the file the mark
stands right above its table (no line wasted while reading); in the PDF
it would sit beside the table (ad89035).  For
`org-export-before-processing-functions' — the file stays as it is."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\(?:<<[^>\n]*>>\\)?⟦[0-9]+⟧[^\n]*\n|" nil t)
      (goto-char (1- (match-end 0)))
      (insert "\n"))))

(defun bcat-export--plain-zettel-links (&rest _)
  "In the current (export) buffer: every link to a zettel (denote:ID,
5.11., the terms of the concept notes, bcat-zettel.el) as its text —
the reMarkable cannot open the Kasten, and org would stop at a link it
cannot resolve.  For `org-export-before-processing-functions'."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "\\[\\[denote:[^]\n]+\\]\\[\\([^]\n]+\\)\\]\\]" nil t)
      (replace-match (match-string 1) t t))))

(defun bcat-export--drop-machine-sections (&rest _)
  "Delete from the current (export) buffer the `** Word Analysis'
subtree — machine data for the zettel highlighting and the padapāṭha
PDF, which the table and the Forms say for the reader (§5.74, 29.10.)
— and `* Handout', the input of handout-synopse.py kept in the AS
sentence files (Carsten 30.10.: not in the PDF).  For
`org-export-before-processing-functions' — the file stays as it is."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^\\(?:\\*\\* Word Analysis\\|\\* Handout\\)[ \t]*$" nil t)
      (let ((beg (line-beginning-position)))
        (org-end-of-subtree t t)
        (delete-region beg (point))
        (goto-char beg)))))

;; ----------------------------------------------------------------------------
;; One document with jump marks (Carsten 5.11.2026)
;; ----------------------------------------------------------------------------
;; „Es könnte ja auch ein PDF bleiben, wenn ich Sprungmarken hätte“ — the
;; PDF of a sentence file is no longer the file in its order but a VIEW
;; rendered from the template pdf-<template> with the fields C-c u L reads
;; back: Reading, Segments, Grammar, Concepts, Discussion, … each part on
;; a page of its own, a bar of links to the parts at the foot of every
;; page, on the page of the reMarkable Paper Pro.

(defcustom bcat-export-paper '(180 . 240)
  "Width and height of the PDF page in mm.  Carsten 5.11.: reMarkable
Paper Pro — its screen (2160 × 1620 px at 229 ppi) is 180 × 240 mm, so
the page is shown at its size (an A4 page was shown at 70 %, its wide
margins wasted 40 % of the screen's width)."
  :type '(cons integer integer)
  :group 'buddhist-cat)

(defcustom bcat-export-margins "left=8mm,right=8mm,top=13mm,bottom=13mm,headsep=3mm,footskip=7mm"
  "Margins of the PDF page (options of \\geometry): narrow — the
reMarkable's frame is margin enough; room for the running head and the
bar of parts at the foot."
  :type 'string
  :group 'buddhist-cat)

(defcustom bcat-export-font-size 10
  "Font size of the PDF in pt — the page is shown at its size."
  :type 'integer
  :group 'buddhist-cat)

(defun bcat-export-page-lines ()
  "The lines setting the page of the PDF (`bcat-export-paper',
`bcat-export-margins', `bcat-export-font-size') — for the export copy,
never the file."
  (concat (format "#+LATEX_CLASS_OPTIONS: [%dpt]\n" bcat-export-font-size)
          (format "#+LATEX_HEADER: \\geometry{paperwidth=%dmm,paperheight=%dmm,%s}\n"
                  (car bcat-export-paper) (cdr bcat-export-paper) bcat-export-margins)))

(defun bcat-export--filled-p (value)
  "Non-nil when VALUE is something to show: a string not blank, any
other non-nil value."
  (if (stringp value) (not (string-blank-p value)) (and value t)))

(defconst bcat-export--detail-fields
  '(formation-tables sandhi forms compounds structure note)
  "The fields of a segment that go into the part Grammar of the PDF.")

(defun bcat-export--lecture-paragraphs (body)
  "The paragraphs of the lecture BODY as ((SECONDS . PARAGRAPH) …) — each
begins with its time mark, [[url][H:MM:SS]]; one without keeps the
time of the one before."
  (let ((time 0))
    (mapcar (lambda (p)
              (when (string-match "\\`\\[\\[[^]\n]*\\]\\[\\([0-9]+\\):\\([0-9][0-9]\\):\\([0-9][0-9]\\)\\]\\]" p)
                (setq time (+ (* 3600 (string-to-number (match-string 1 p)))
                              (* 60 (string-to-number (match-string 2 p)))
                              (string-to-number (match-string 3 p)))))
              (cons time p))
            (split-string (or body "") "\n[ \t]*\n" t "[ \t\n]+"))))

(defun bcat-export--lecture-excerpt (body name cited)
  "The excerpt of the lecture NAME (BODY) for the reading PDF (Carsten
5.11.: „Im Reading PDF nur ein Exzerpt“): the paragraphs whose time the
text CITED cites — „(S, I 0:24:13)“; a mark inside a paragraph takes
that paragraph — in order, each once, blank-line separated.  A mark of
another lecture (its numeral, „II“, not the one NAME ends with) does
not count.  Mechanical — the transcript goes to no one."
  (let* ((numeral (and name (string-match "\\b\\([IVXL]+\\)[ \t]*\\'" name) (match-string 1 name)))
         (paras (bcat-export--lecture-paragraphs body))
         (times '()) (start 0))
    (while (string-match "\\(?:\\b\\([IVXL]+\\) \\)?\\([0-9]+\\):\\([0-9][0-9]\\):\\([0-9][0-9]\\)" cited start)
      (setq start (match-end 0))
      (unless (and numeral (match-string 1 cited) (not (equal numeral (match-string 1 cited))))
        (push (+ (* 3600 (string-to-number (match-string 2 cited)))
                 (* 60 (string-to-number (match-string 3 cited)))
                 (string-to-number (match-string 4 cited)))
              times)))
    (string-join
     (cl-loop for (p . rest) on paras
              for next = (if rest (car (car rest)) most-positive-fixnum)
              when (cl-some (lambda (tm) (and (>= tm (car p)) (< tm next))) times)
              collect (cdr p))
     "\n\n")))

(defun bcat-export--view-context (context)
  "CONTEXT (`bcat-relayout-file-context') with the fields of the PDF view:
the proposed translation without its decisions — they belong to the
discussion (`decisions'), the translation is read first —; per segment
`detail-p' and `grammar-link' (the link from ⟦N⟧ to its grammar, only
when there is some); whether the parts Grammar, Concepts, Discussion,
Notes have anything (`grammar-p', `concepts-p', `discussion-p',
`translations-p', `notes-p')."
  (let* ((proposal (or (alist-get 'proposal context) ""))
         (cut (and (string-match "^\\*\\(?:Decisions\\|Entscheidungen\\)\\(?: — [^*\n]*\\)?:\\*" proposal)
                   (match-beginning 0)))
         (decisions (if cut (string-trim (substring proposal cut)) ""))
         (proposal (string-trim (if cut (substring proposal 0 cut) proposal)))
         (segments (mapcar (lambda (s)
                             (let ((detail (cl-some (lambda (f) (bcat-export--filled-p (alist-get f s)))
                                                    bcat-export--detail-fields)))
                               (append `((detail-p . ,detail)
                                         (grammar-link . ,(if detail
                                                              (format " · [[#grammar-%s][Grammar]]"
                                                                      (alist-get 'num s))
                                                            "")))
                                       s)))
                           (alist-get 'segments context)))
         (filled (lambda (key) (bcat-export--filled-p (alist-get key context))))
         (any (lambda (key items) (cl-some (lambda (s) (bcat-export--filled-p (alist-get key s))) items))))
    (append
     `((segments . ,segments)
       (lectures . ,(let ((cited (prin1-to-string (cl-remove 'lectures context :key #'car))))
                      (mapcar (lambda (l)
                                (cons (cons 'excerpt (bcat-export--lecture-excerpt
                                                      (alist-get 'lecture-body l)
                                                      (alist-get 'lecture-name l) cited))
                                      l))
                              (alist-get 'lectures context))))
       (proposal . ,proposal)
       (decisions . ,decisions)
       (grammar-p . ,(or (funcall filled 'grammar)
                         (cl-some (lambda (s) (alist-get 'detail-p s)) segments)
                         (funcall any 'formation-tables (alist-get 'sanskrit-segments context))))
       (concepts-p . ,(funcall any 'concepts segments))
       (translations-p . ,(funcall any 'translations segments))
       (discussion-p . ,(or (bcat-export--filled-p decisions)
                            (funcall filled 'comparison) (funcall filled 'rendering)
                            (funcall any 'translations segments)))
       (notes-p . ,(or (funcall filled 'working-translation) (funcall filled 'my-notes))))
     context)))

(defun bcat-export-view (file)
  "The PDF view of the sentence FILE as org text: the template
pdf-<its template> (cascade of its source) rendered with the fields C-c u
L reads back from FILE (`bcat-relayout-file-context', completed by
`bcat-export--view-context').  The file stays as it is.  nil when there
is no such template (a commentary document) or FILE is no sentence file
of a template — C-c u H then exports the file itself."
  (plist-get (bcat-export-views file) :view))

(defun bcat-export-views (file)
  "The views of the sentence FILE (`bcat-export-view'): (:view TEXT
:lectures TEXT :lecturer NAME) — the reading PDF (template
pdf-<template>) and, when the sentence has lectures (#+CAT_LECTURES),
the document of the whole lectures (template pdf-lectures, Carsten
5.11.: „das was einen konkreten Vers betrifft in einem
Extradokument“), NAME the lecturer (the first lecture's name up to its
comma).  nil without a pdf template."
  (require 'bcat-relayout)
  (let* ((template (bcat-sentence-file-p file))
         (source (and template (tibetan-sentence--source-file-from-analysis file)))
         (name (and template (concat "pdf-" template))))
    (when (and name (bcat-template-find name source))
      (let ((read (bcat-relayout-file-context file)))
        (unless (plist-get read :error)
          (let* ((context (bcat-export--view-context (plist-get read :context)))
                 (lectures (alist-get 'lectures context)))
            (list :view (bcat-template-render-named name context (plist-get read :source))
                  :lectures (and lectures (bcat-template-find "pdf-lectures" source)
                                 (bcat-template-render-named "pdf-lectures" context (plist-get read :source)))
                  :lecturer (and lectures
                                 (string-trim (car (split-string (alist-get 'lecture-name (car lectures))
                                                                 "[,(]")))))))))))

(defun bcat-export--parts-pages (&rest _)
  "In the current (export) buffer: every part — a heading of level 1 with
a CUSTOM_ID (the PDF view) — after the first starts a new page, and the
foot of every page carries a bar of links to the parts (the reMarkable
follows internal links since 2.6), placed after the running head
\(whose \\fancyhf{} would clear it).  Without parts nothing changes.
For `org-export-before-processing-functions'."
  (save-excursion
    (goto-char (point-min))
    (let ((parts '()))
      (while (re-search-forward "^\\* \\(.+?\\)[ \t]*$" nil t)
        (let* ((title (match-string-no-properties 1))
               (beg (copy-marker (line-beginning-position)))
               (id (save-excursion
                     (forward-line 1)
                     (when (looking-at "[ \t]*:PROPERTIES:[ \t]*$")
                       (let ((end (save-excursion (re-search-forward "^[ \t]*:END:[ \t]*$" nil t))))
                         (and end (re-search-forward "^[ \t]*:CUSTOM_ID:[ \t]*\\(\\S-+\\)" end t)
                              (match-string-no-properties 1)))))))
          (when id (push (list id title beg) parts))))
      (when parts
        (setq parts (nreverse parts))
        (dolist (p (cdr parts))
          (goto-char (nth 2 p))
          (insert "#+LATEX: \\clearpage\n"))
        (goto-char (point-min))
        (let ((last nil))
          (while (re-search-forward "^#\\+LATEX_HEADER:.*$" nil t)
            (setq last (point)))
          (if last (progn (goto-char last) (forward-line 1)) (goto-char (point-min))))
        (insert "#+LATEX_HEADER: \\fancyfoot[C]{"
                (mapconcat (lambda (p)
                             (format "\\hyperref[%s]{%s}" (nth 0 p) (bcat-export--latex-escape (nth 1 p))))
                           parts "\\enspace·\\enspace ")
                "}\n")
        (dolist (p parts) (set-marker (nth 2 p) nil))))))

(defcustom bcat-export-keep-tex nil
  "Keep the .tex beside the PDF — only for looking into the LaTeX.
Carsten 30.10.: „das .tex können wir an sich auch wegräumen, wenn das
PDF erzeugt ist“; a failed run keeps it anyway (no PDF, nothing
removed)."
  :type 'boolean
  :group 'buddhist-cat)

(defun bcat-export-remove-tex (pdf)
  "Remove the .tex of PDF once PDF exists (unless `bcat-export-keep-tex');
return PDF.  Shared by every LaTeX export of the tool (sentence files,
old handout, discussion paper)."
  (let ((tex (concat (file-name-sans-extension pdf) ".tex")))
    (when (and (not bcat-export-keep-tex) (file-exists-p pdf) (file-exists-p tex))
      (delete-file tex)))
  pdf)

(defun bcat-export-pdf (file &optional head)
  "Export the sentence FILE to PDF beside it and return the PDF path.
HEAD (lines from `bcat-export-head-lines') goes into the export copy,
never into the file.  LuaLaTeX, two runs (longtable), tables flush
left at natural width, glossed verse lines broken only between word
units, tables no wider than the line — bound here, Carsten's own export
settings (centring, tabularx) stay untouched outside.  The characters
LuaLaTeX could not set land in `bcat-export--missing'."
  (require 'ox-latex)
  (setq bcat-export--missing nil)
  (let* ((visited (get-file-buffer file))
         (buf (or visited (find-file-noselect file)))
         (remove-logs org-latex-remove-logfiles)
         (insert-head (lambda (&rest _)
                        (goto-char (point-min))
                        (insert (bcat-export--font-lines) (bcat-export-page-lines) (or head "")))))
    (unwind-protect
        (with-current-buffer buf
          (when (buffer-modified-p) (save-buffer))
          (let* ((pdf (let ((org-latex-compiler "lualatex")
                            ;; Both runs in ONE command: org checks the
                            ;; output from the last „This is …TeX“ on, and
                            ;; with two commands that was the FIRST run's —
                            ;; the jump marks of a bilingual file, unknown
                            ;; there, gave „[undefined reference]“ (2.11.).
                            (org-latex-pdf-process
                             '("%latex -interaction nonstopmode -output-directory %o %f; %latex -interaction nonstopmode -output-directory %o %f"))
                            (org-latex-tables-centered nil)
                            ;; **** (a unit of the comparison, 2.11.) as a
                            ;; heading, not as a list item.
                            (org-export-headline-levels 4)
                            ;; The parts of the view are linked by their
                            ;; CUSTOM_ID (the bar at the foot, ⟦N⟧ →
                            ;; grammar-N): those are the labels.
                            (org-latex-prefer-user-labels t)
                            ;; The log is read first (missing characters),
                            ;; then cleared as org would.
                            (org-latex-remove-logfiles nil)
                            (org-export-filter-table-functions
                             (append (remq 'org-latex-auto-tabularx org-export-filter-table-functions)
                                     (list #'bcat-export--table-filter)))
                            (org-export-filter-verse-block-functions
                             (cons #'bcat-export--verse-filter org-export-filter-verse-block-functions))
                            (org-export-filter-paragraph-functions
                             (cons #'bcat-export--parts-line-filter org-export-filter-paragraph-functions))
                            (org-export-before-processing-functions
                             (append (list insert-head #'bcat-export--drop-machine-sections
                                           #'bcat-export--plain-zettel-links
                                           #'bcat-export--rechunk-gloss-tables
                                           #'bcat-export--rechunk-header-tables
                                           #'bcat-export--wrap-wide-tables
                                           #'bcat-export--marks-apart
                                           #'bcat-export--parts-pages)
                                     org-export-before-processing-functions)))
                        (expand-file-name (org-latex-export-to-pdf) (file-name-directory file))))
                 (base (file-name-sans-extension pdf))
                 (log (concat base ".log")))
            (when (file-readable-p log)
              (setq bcat-export--missing
                    (bcat-export--missing-characters
                     (with-temp-buffer (insert-file-contents log) (buffer-string)))))
            (when remove-logs
              (dolist (f (directory-files (file-name-directory pdf) t
                                          (concat "\\`" (regexp-quote (file-name-nondirectory base))
                                                  "\\(?:\\.[0-9]+\\)?\\."
                                                  (regexp-opt org-latex-logfiles-extensions) "\\'")))
                (delete-file f)))
            (bcat-export-remove-tex pdf)))
      (unless visited (kill-buffer buf)))))

(defun bcat-export-file (file)
  "C-c u H for the sentence FILE: PDF with running head, moved into the
handouts folder under its speaking name (Carsten 30.10.: „Das PDF
brauchen wir nicht noch mal im Analyse Ordner“), then onto the
reMarkable (`bcat-export-upload').  Exported is the view (Carsten
5.11.: one document with jump marks, `bcat-export-view'), written
beside FILE as .pdfview-<name>.org — a name no folder scan takes for a
sentence file — and removed after the export; without a view the file
itself.  Returns the PDF path."
  (let* ((info (bcat-export-info file))
         (head (bcat-export-head-lines (plist-get info :folder) (plist-get info :work)
                                       (plist-get info :sent-num) (plist-get info :verse)))
         (views (condition-case err (bcat-export-views file)
                  (error (message "PDF-Ansicht von %s nicht gesetzt (%s) — die Datei selbst"
                                  (file-name-nondirectory file) (error-message-string err))
                         nil)))
         (view (plist-get views :view))
         (lectures (bcat-export--lectures-pdf file views head (plist-get info :handout)))
         (view-file (and view (expand-file-name (concat ".pdfview-" (file-name-nondirectory file))
                                                (file-name-directory file))))
         (pdf (if (not view)
                  (bcat-export-pdf file head)
                (unwind-protect
                    (progn (let ((coding-system-for-write 'utf-8-unix))
                             (with-temp-file view-file (insert view)))
                           (bcat-export-pdf view-file head))
                  (when (file-exists-p view-file) (delete-file view-file))))))
    (let ((target (or (plist-get info :handout)
                      ;; No handouts folder: beside FILE under its name.
                      (and view (concat (file-name-sans-extension file) ".pdf")))))
      ;; Only the PDF the export made beside FILE — never another file.
      (when (and target (file-exists-p pdf)
                 (file-equal-p (file-name-directory pdf) (file-name-directory file)))
        (condition-case err
            (progn (make-directory (file-name-directory target) t)
                   (rename-file pdf target t)
                   (setq pdf target))
          (error (message "PDF nicht nach handouts/ verschoben (%s): %s"
                          (file-name-nondirectory target) (error-message-string err))))))
    (bcat-status-step file :pdf (if bcat-export--missing
                                    (concat "PDF ✓ · fehlende Zeichen: "
                                            (bcat-export--missing-text bcat-export--missing))
                                  "PDF ✓"))
    (if bcat-export-upload
        (progn
          (require 'tibetan-discussion-paper)
          (bcat-status-step file :upload "reMarkable …")
          (tibetan-discussion-paper--upload
           pdf (plist-get info :work) (plist-get info :label)
           (lambda (ok what)
             (let ((done (if ok (concat "reMarkable ✓ " what) (concat "reMarkable: Fehler — " what))))
               (bcat-status-step file :upload done)
               ;; The lectures after the PDF — rmapi one at a time.
               (when (and ok lectures)
                 (tibetan-discussion-paper--upload
                  lectures (plist-get info :work)
                  (format "%s %s" (plist-get info :label) (plist-get views :lecturer))
                  (lambda (ok2 what2)
                    (bcat-status-step file :upload
                                      (concat done (if ok2 " · Vortrag ✓" (concat " · Vortrag: Fehler — " what2)))))))))))
      (message "PDF: %s" pdf))
    pdf))

(defun bcat-export--lectures-pdf (file views head target)
  "The document of the lectures of the sentence FILE (:lectures of VIEWS,
Carsten 5.11.) as PDF beside TARGET (the handout: <name>-lectures.pdf),
or nil.  Returns its path only when it is NEW — then C-c u H puts it on
the reMarkable; later it is only written anew (the transcript does not
change; a copy per export would crowd the device)."
  (let ((text (plist-get views :lectures)))
    (when (and text target)
      (let* ((out (concat (file-name-sans-extension target) "-lectures.pdf"))
             (new (not (file-exists-p out)))
             (src (expand-file-name (concat ".pdfview-" (file-name-base file) "-lectures.org")
                                    (file-name-directory file))))
        (unwind-protect
            (let ((pdf (progn (let ((coding-system-for-write 'utf-8-unix))
                                (with-temp-file src (insert text)))
                              (bcat-export-pdf src head))))
              (when (and pdf (file-exists-p pdf)
                         (file-equal-p (file-name-directory pdf) (file-name-directory file)))
                (make-directory (file-name-directory out) t)
                (rename-file pdf out t)
                (and new out)))
          (when (file-exists-p src) (delete-file src)))))))

(defun bcat-export-after-fetch (file)
  "Hook for `bcat-sentence-after-fetch-functions': C-c u G ends with
C-c u H (when `bcat-export-auto').  Never for a FILE under
`temporary-file-directory' — a test fixture; an ERT run in a live Emacs
put such papers on the device (§5.69).  Never signals — a LaTeX or
rmapi failure must not spoil the landed analysis."
  (require 'tibetan-discussion-paper)
  (when (and bcat-export-auto
             (not (tibetan-discussion-paper--temp-source-p file)))
    (condition-case err
        (bcat-export-file file)
      (error (message "C-c u H nach C-c u G fehlgeschlagen (%s): %s"
                      (file-name-nondirectory file) (error-message-string err))
             (bcat-status-step file :pdf (concat "PDF: Fehler — " (error-message-string err)))))))

(add-hook 'bcat-sentence-after-fetch-functions #'bcat-export-after-fetch)

;;;###autoload
(defun bcat-export ()
  "C-c u H: the sentence file(s) as PDF onto the reMarkable (§5.74).
In a template sentence file: that file (saved first).  In a source
with a template: the file of the sentence at point, or of every
sentence under the heading at point — files still in the old layout
are skipped (C-c u G first).  Sources without a template yet keep the
old handout."
  (interactive)
  (let ((file (buffer-file-name)))
    (cond
     ((and file (bcat-sentence-file-p file))
      (when (buffer-modified-p) (save-buffer))
      (bcat-status-begin "C-c u H" (file-name-base file)
                         (list (cons file (plist-get (bcat-export-info file) :label))))
      (bcat-export-file file))
     ((and file (string-match-p "\\`sent-[0-9]" (file-name-nondirectory file)))
      (let ((src (tibetan-sentence--source-file-from-analysis file)))
        (if (bcat-sentence-template-name src)
            (message "%s ist noch im alten Layout — erst C-c u G"
                     (file-name-nondirectory file))
          (call-interactively #'tibetan-handout))))
     (t
      (let* ((scope (tibetan-cascade--scope-at-point))
             (src (plist-get scope :source)))
        (if (not (bcat-sentence-template-name src))
            (call-interactively #'tibetan-handout)
          (let* ((folder (file-name-as-directory
                          (expand-file-name "analysis" (file-name-directory src))))
                 (files (mapcar (lambda (n) (tibetan-sentence--filepath n folder src))
                                (plist-get scope :sent-nums)))
                 (ready (cl-remove-if-not #'bcat-sentence-file-p files))
                 (old (- (length files) (length ready))))
            (when (and (> (length ready) tibetan-cascade-subtree-confirm-threshold)
                       (not (y-or-n-p (format "%d PDFs erzeugen und hochladen? " (length ready)))))
              (user-error "Abgebrochen"))
            (bcat-status-begin "C-c u H" (plist-get scope :label)
                               (mapcar (lambda (f) (cons f (plist-get (bcat-export-info f) :label)))
                                       ready))
            (mapc #'bcat-export-file ready)
            (message "C-c u H: %d PDF(s)%s" (length ready)
                     (if (> old 0)
                         (format ", %d Satzdatei(en) noch ohne Vorlage — erst C-c u G" old)
                       "")))))))))

(provide 'bcat-export)
;;; bcat-export.el ends here
