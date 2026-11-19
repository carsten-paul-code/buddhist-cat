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
(declare-function bcat-dossier-export-pending-p "bcat-dossier" (file))
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

(defcustom bcat-export-original-color "8B1E1E"
  "Colour (HTML hex) of the original under ⟦N⟧ in the PDF — the line of
the segment in IAST (padapāṭha), bold (\\bcatorig).  Carsten 9.11., TĀ
1009: „hatten wir nicht mal Original und Übersetzung hervorgehoben?“ —
the original bold, in the PDF in colour."
  :type 'string
  :group 'buddhist-cat)

(defcustom bcat-export-translation-color "1E6B3A"
  "Colour (HTML hex) of the segment's translation („→“) in the PDF,
italic (\\bcattrans) — draft B (5.11.): „einzeilig, kursiv“, since
9.11. set off in colour (`bcat-translation-face' in Emacs)."
  :type 'string
  :group 'buddhist-cat)

(defun bcat-export--font-lines ()
  "#+LATEX_HEADER lines defining \\bcattib and \\bcatdeva, the colour
\\bcatg (gloss, `bcat-export-gloss-color'), \\bcatorig and \\bcattrans
\(the original under ⟦N⟧ and the segment's translation, 9.11.) and
\\bcatsub (a small title), loading adjustbox
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
   ;; 18.11.: black inside a row of parts (member and meaning)
   "#+LATEX_HEADER: \\newcommand{\\bcatk}[1]{\\textcolor{black}{#1}}\n"
   ;; 9.11. (TĀ 1009): the original under ⟦N⟧ on a line of its own
   ;; (\\\\: the mark before it in the same paragraph), the segment's
   ;; translation, the small title over the parts of the words.
   (format "#+LATEX_HEADER: \\definecolor{bcatorig}{HTML}{%s}\\definecolor{bcattrans}{HTML}{%s}\n"
           bcat-export-original-color bcat-export-translation-color)
   "#+LATEX_HEADER: \\newcommand{\\bcatorig}[1]{\\\\{\\bfseries\\color{bcatorig}#1}}\n"
   "#+LATEX_HEADER: \\newcommand{\\bcattrans}[1]{{\\itshape\\color{bcattrans}#1}}\n"
   "#+LATEX_HEADER: \\newcommand{\\bcatsub}[1]{\\par\\smallskip\\noindent{\\small\\bfseries #1}\\par\\nopagebreak}\n"
   ;; 18.11. (TĀ 1016): the tables of a segment in ONE size.  Every
   ;; table is measured (bcattable) and shrunk to the line only when
   ;; wider; between \\bcatrunbegin and \\bcatrunend
   ;; (`bcat-export--table-runs') all by the widest of the run — its
   ;; width goes to the .aux in the first run and is used in the second.
   "#+LATEX_HEADER: \\makeatletter\n"
   "#+LATEX_HEADER: \\newcounter{bcatrun}\\newdimen\\bcatrunmax\\newsavebox{\\bcattabbox}\\newif\\ifbcatinrun\n"
   "#+LATEX_HEADER: \\newcommand{\\bcatrunbegin}{\\stepcounter{bcatrun}\\global\\bcatrunmax=\\z@\\global\\bcatinruntrue}\n"
   "#+LATEX_HEADER: \\newcommand{\\bcatrunend}{\\ifbcatinrun\\immediate\\write\\@auxout{\\string\\global\\string\\@namedef{bcatrun@\\thebcatrun}{\\the\\bcatrunmax}}\\global\\bcatinrunfalse\\fi}\n"
   (concat "#+LATEX_HEADER: \\newenvironment{bcattable}{\\begin{lrbox}{\\bcattabbox}}{\\end{lrbox}\\dimen@=\\wd\\bcattabbox"
           "\\ifbcatinrun\\ifdim\\dimen@>\\bcatrunmax\\global\\bcatrunmax=\\dimen@\\fi"
           "\\@ifundefined{bcatrun@\\thebcatrun}{}{\\dimen@=\\@nameuse{bcatrun@\\thebcatrun}\\relax}\\fi"
           ;; flush left like the text (adjustbox did not indent either)
           "\\ifvmode\\noindent\\fi"
           "\\ifdim\\dimen@>\\linewidth"
           ;; the factor first: \\scalebox uses \\dimen@ itself
           "\\edef\\bcat@s{\\strip@pt\\dimexpr 1pt*\\linewidth/\\dimen@\\relax}\\scalebox{\\bcat@s}{\\usebox{\\bcattabbox}}"
           "\\else\\usebox{\\bcattabbox}\\fi}\n")
   "#+LATEX_HEADER: \\makeatother\n"
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
than the line — scaled down when wider, else as it is (the environment
bcattable of `bcat-export--font-lines'; until 18.11. adjustbox, max
width).  30.10., TĀ 1001: the metre table (19 syllables and the pause)
ran off the page.  18.11., TĀ 1016: the tables of a segment
\(`bcat-export--table-runs') all by the widest of them — each shrunk on
its own, they stood in four sizes."
  (if (and (org-export-derived-backend-p backend 'latex)
           (string-match-p "\\\\begin{tabular}" text))
      ;; The blank line after the table stays (30.10.: cut, the next
      ;; paragraph ran on beside the box — „Translation:“ in the table).
      (let ((end (if (string-match "\n*\\'" text) (match-beginning 0) (length text))))
        (concat "\\begin{bcattable}\n"
                (bcat-export--detail-rows (bcat-export--table-scripts (substring text 0 end))) "\n"
                "\\end{bcattable}"
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
members' in \\bcatg — but not the row of the members' meanings right
under them (18.11.); the table of a compound's resolution has only the
relation, in brackets, in \\bcatg.  The rules of booktabs (\\toprule, \\midrule,
\\bottomrule — `org-latex-tables-booktabs', Carsten's setting) count
like \\hline (18.11.)."
  (let* ((lines (split-string tabular "\n"))
         (row-p (lambda (l) (string-match-p "\\\\\\\\[ \t]*\\'" l)))
         (rows (cl-remove-if-not row-p lines))
         ;; 18.11.: the rules of booktabs (Carsten's export setting)
         ;; count like \hline — without them no table of parts was seen,
         ;; its rows came out blue, black, blue.
         (rule-p (lambda (l) (string-match-p
                              "\\`[ \t]*\\\\\\(?:hline\\|toprule\\|midrule\\|bottomrule\\)\\b" l)))
         (header (let ((first (cl-position-if row-p lines)))
                   (and first (nth (1+ first) lines)
                        (funcall rule-p (nth (1+ first) lines))))))
    (cond
     ((or (< (length rows) 2)
          (and header (string-match-p "\\\\cellcolor" tabular))
          (bcat-export--metre-row-p (nth 1 rows)))
      tabular)
     ;; 18.11.: the resolution of a compound — no header, three cells a
     ;; row, the relation in brackets in the middle: only that in \bcatg.
     ((and (not header)
           (cl-every (lambda (l)
                       (let ((cells (bcat-export--cells
                                     (replace-regexp-in-string "\\\\\\\\[ \t]*\\'" "" l))))
                         (and (= 3 (length cells))
                              (string-match-p "\\`\\[.*\\]\\'" (string-trim (nth 1 cells))))))
                     rows))
      (mapconcat
       (lambda (l)
         (if (not (memq l rows))
             l
           (let ((cells (bcat-export--cells (replace-regexp-in-string "\\\\\\\\[ \t]*\\'" "" l))))
             (concat (string-trim (nth 0 cells)) " & \\bcatg{" (string-trim (nth 1 cells)) "} & "
                     (string-trim (nth 2 cells)) "\\\\"))))
       lines "\n"))
     (t
      (let ((blue '()) (band '()))
        (if header
            ;; 5.11.: a table of a word's parts — the members, \hline, the
            ;; parts: every row but the members'.  18.11.: nor the row
            ;; of the members' meanings — the first under the members,
            ;; no Sanskrit form in italics in it.
            (setq blue (if (and (cdr rows) (not (string-match-p "\\\\emph{" (nth 1 rows))))
                           (cddr rows)
                         (cdr rows)))
          ;; The bands, each closed by an \hline or the end.
          (dolist (l (append lines (list "\\hline")))
            (cond ((funcall row-p l) (push l band))
                  ((funcall rule-p l)
                   (let* ((b (nreverse band)) (n (length b))
                          (uchen (and b (string-match-p "[\u0F00-\u0FFF]" (car b)))))
                     (cl-loop for r in b for i from 0
                              unless (or (= i 0) (and uchen (= i 1)) (= i (- n 2))
                                         ;; 18.11.: a row that carries its
                                         ;; colours (the parts under the words)
                                         (string-match-p "\\\\bcat[gk]{" r))
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
         lines "\n"))))))

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

(defun bcat-export--under-mark-p ()
  "Non-nil when above the table at point — blank lines skipped — stands
a ⟦N⟧ mark, alone or as a heading (the file since 9.11.).  Between the
mark and the table may stand the original (9.11.: a bold line in the
file, \\bcatorig in the view) and under it „Prüfen:“ (16.11.) — 18.11.,
TĀ 1016: with the original in between no table was a segment's any
more, none was laid out for the page."
  (save-excursion
    (let ((up (lambda ()
                (and (zerop (forward-line -1))
                     (progn (while (and (looking-at "[ \t]*$") (zerop (forward-line -1))))
                            t)))))
      (and (funcall up)
           (progn
             (when (looking-at "\\*Prüfen:\\* ") (funcall up))
             (when (looking-at "@@latex:\\\\bcatorig{@@\\|\\*[^* \n][^\n]*\\*[ \t]*$") (funcall up))
             (looking-at "\\(?:\\*+ \\)?\\(?:<<[^>\n]*>>\\)?⟦[0-9]+⟧[^\n]*$"))))))

(defun bcat-export--gloss-run-start-p ()
  "Non-nil when the table at point begins the tables of a segment: it
stands under a ⟦N⟧ mark (`bcat-export--under-mark-p'), or its first row
is in Tibetan script (files without marks)."
  (or (looking-at "|[^-\n].*[\u0F00-\u0FFF]")
      (bcat-export--under-mark-p)))

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

(defun bcat-export--table-runs (&rest _)
  "In the current (export) buffer: the tables of each segment marked as
one run for LaTeX — „#+LATEX: \\bcatrunbegin“ before the first,
„#+LATEX: \\bcatrunend“ after the last —, so all of them are set in one
size, that of the widest (`bcat-export--font-lines').  18.11., TĀ 1016:
each table wider than the line was shrunk on its own, the tables of one
segment stood in four sizes.  The run of a segment under a ⟦N⟧ mark
reaches to the next mark or heading — the overview and, under its
translation, the parts of its words (the heading Word Formation of the
file does not end it); without marks (Tibetan, a first row in Uchen) it
is the tables with only blank lines between.  For
`org-export-before-processing-functions', after
`bcat-export--marks-apart' — the file stays as it is."
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^|" nil t)
      (goto-char (line-beginning-position))
      (if (or (save-excursion (and (zerop (forward-line -1)) (looking-at "[ \t]*|")))
              (not (bcat-export--gloss-run-start-p)))
          (forward-line 1)
        (let* ((start (point))
               (marked (bcat-export--under-mark-p))
               (block (save-excursion
                        (let ((e nil))
                          (while (looking-at "|")
                            (while (looking-at "|") (forward-line 1))
                            (setq e (point))
                            (skip-chars-forward "\n"))
                          e)))
               (limit (save-excursion
                        (forward-line 1)
                        (let ((found nil))
                          (while (and (not found)
                                      (re-search-forward
                                       "^\\(?:\\*+ \\|\\(?:<<[^>\n]*>>\\)?⟦[0-9]+⟧\\)" nil t))
                            (unless (save-excursion (goto-char (line-beginning-position))
                                                    (looking-at "\\*+ Word Formation[ \t]*$"))
                              (setq found (line-beginning-position))))
                          (or found (point-max)))))
               (end (if (not marked)
                        block
                      (save-excursion
                        (let ((e block))
                          (goto-char block)
                          (while (re-search-forward "^|.*$" limit t)
                            (setq e (min (point-max) (1+ (point)))))
                          e)))))
          (goto-char end)
          (unless (bolp) (insert "\n"))
          (insert "#+LATEX: \\bcatrunend\n")
          (let ((after (point-marker)))
            (goto-char start)
            (insert "#+LATEX: \\bcatrunbegin\n")
            (goto-char after)
            (set-marker after nil)))))))

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

(defvar bcat-export-page t
  "Non-nil: `bcat-export-pdf' sets the page of the Paper Pro
\(`bcat-export-page-lines'); nil: the document keeps its own (Carsten
5.11., the documents of the readings: „in Handouts A4 ist sehr fein“).")

(defconst bcat-export--geometry "\\usepackage{geometry}"
  "Loads geometry before \\geometry{…}: without options — when the
LaTeX class loads it already (Carsten's, a4paper), neither an „Option
clash“ nor a second load; when it does not (batch, another machine),
\\geometry is defined.  Bug 9.11.: \\geometry alone gave „Undefined
control sequence“ there — the PDF came out with errors, the page of
the Paper Pro not set.")

(defun bcat-export-page-lines ()
  "The lines setting the page of the PDF (`bcat-export-paper',
`bcat-export-margins', `bcat-export-font-size') — for the export copy,
never the file."
  (concat (format "#+LATEX_CLASS_OPTIONS: [%dpt]\n" bcat-export-font-size)
          (format "#+LATEX_HEADER: %s\\geometry{paperwidth=%dmm,paperheight=%dmm,%s}\n"
                  bcat-export--geometry
                  (car bcat-export-paper) (cdr bcat-export-paper) bcat-export-margins)))

(defun bcat-export--filled-p (value)
  "Non-nil when VALUE is something to show: a string not blank, any
other non-nil value."
  (if (stringp value) (not (string-blank-p value)) (and value t)))

(defconst bcat-export--detail-fields
  '(sandhi forms compounds structure note)
  "The fields of a segment that go into the part Grammar of the PDF —
since 18.11. not the parts of its words (`formation-tables'): they
stand under its translation in the part Segments.")

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

(defun bcat-export--tex (s)
  "S, the text of a table cell, as LaTeX: the special characters escaped
\(`bcat-export--latex-escape'), /x/ in italics, the bar of a cell
\(\\vert) a bar — for a cell set in an export snippet, which org does
not read."
  (let ((re "\\(\\`\\|[ \t('\"{-]\\)/\\([^/ \t\n]\\(?:[^/\n]*[^/ \t\n]\\)?\\)/\\(\\'\\|[ \t.,:;!?')}\"-]\\)")
        (plain (lambda (text)
                 (mapconcat #'bcat-export--latex-escape
                            (split-string text "\\\\vert\\(?:{}\\)?") "\\textbar{}")))
        (s (or s "")) (pos 0) (out ""))
    (while (string-match re s pos)
      (setq out (concat out
                        (funcall plain (substring s pos (match-end 1)))
                        "\\emph{" (funcall plain (match-string 2 s)) "}")
            pos (match-beginning 3)))
    (concat out (funcall plain (substring s pos)))))

(defun bcat-export--resolution-line-p (cell)
  "Non-nil when CELL under a compound is a step of its resolution —
„MEMBERS [RELATION] WORDS“ (`bcat-analysis--resolution-lines')."
  (string-match-p "\\`[^ ]+ \\[[^]]*\\] " cell))

(defun bcat-export--sub-cell (cell)
  "CELL, what stands under a word in the table of a Sanskrit segment, for
the PDF — an export snippet with its colours: the parts of a word in
\\bcatg; of a compound's line „MEMBER: MEANING ← PARTS“ member and
meaning in \\bcatk (black), „← PARTS“ in \\bcatg; „MEMBER: PARTS“ the
member black, the parts in \\bcatg; „MEMBER: MEANING“ black."
  (let ((cell (string-trim (or cell ""))))
    (cond
     ((string-empty-p cell) "")
     ((string-match "\\`\\([^ /:|]+:\\) \\(.*\\)\\'" cell)
      (let ((head (match-string 1 cell)) (rest (match-string 2 cell)))
        (cond ((string-match "[ \t]*←" rest)
               (format "@@latex:\\bcatk{%s} \\bcatg{%s}@@"
                       (bcat-export--tex (concat head " " (substring rest 0 (match-beginning 0))))
                       (bcat-export--tex (string-trim (substring rest (match-beginning 0))))))
              ((not (equal rest (bcat-analysis--strip-emphasis rest)))
               (format "@@latex:\\bcatk{%s} \\bcatg{%s}@@" (bcat-export--tex head) (bcat-export--tex rest)))
              (t (format "@@latex:\\bcatk{%s}@@" (bcat-export--tex cell))))))
     (t (format "@@latex:\\bcatg{%s}@@" (bcat-export--tex cell))))))

(defun bcat-export--band-table (chunk cell-width)
  "CHUNK — columns (OVER . SUB) of a Sanskrit segment — as an org table
for the export: the overview (a cell wider than CELL-WIDTH broken), a
rule, the rows under it with their colours (`bcat-export--sub-cell').
Not aligned — nobody reads the export copy."
  (let* ((over (bcat-analysis-wrap-rows
                (cl-loop for r below (apply #'max 0 (mapcar (lambda (c) (length (car c))) chunk))
                         collect (mapcar (lambda (c) (or (nth r (car c)) "")) chunk))
                cell-width))
         (depth (apply #'max 0 (mapcar (lambda (c) (length (cdr c))) chunk)))
         (sub (cl-loop for r below depth
                       collect (mapcar (lambda (c) (bcat-export--sub-cell (or (nth r (cdr c)) ""))) chunk)))
         (line (lambda (row) (concat "| " (string-join row " | ") " |"))))
    (string-join (append (mapcar line over) (and sub (cons "|-" (mapcar line sub)))) "\n")))

(defun bcat-export-sanskrit-page-tables (tables)
  "TABLES, the tables of a Sanskrit segment as its file holds them — the
parts of the words under them (18.11.) —, for the page of the PDF: laid
out anew for its width (`bcat-export-table-width', 6 per column,
`bcat-analysis--band-chunks'), the rows under the rule with their
colours (`bcat-export--band-table'), without the steps of a compound's
resolution (Carsten 18.11.: „Für das PDF ist das 1. Bild ok“ — they
made the segment run over its page; the file has them).  Tables without
anything under their words, or in another form, come back as they are:
`bcat-export--rechunk-gloss-tables' lays them out."
  (let ((columns (bcat-analysis-band-columns tables)))
    (if (or (null columns)
            (cl-some (lambda (c) (/= 3 (length (car c)))) columns)
            (cl-notany #'cdr columns))
        tables
      (let* ((columns (mapcar (lambda (c)
                                (cons (car c) (cl-remove-if #'bcat-export--resolution-line-p (cdr c))))
                              columns))
             (best (bcat-analysis--band-chunks columns bcat-export-table-width 6 bcat-analysis-cell-width)))
        (mapconcat (lambda (chunk) (bcat-export--band-table chunk (cdr best))) (car best) "\n\n")))))

(defun bcat-export--view-context (context &optional template)
  "CONTEXT (`bcat-relayout-file-context') with the fields of the PDF view:
the proposed translation without its decisions — they belong to the
discussion (`decisions'), the translation is read first —; per segment
`detail-p' and `grammar-link' (the link from ⟦N⟧ to its grammar, only
when there is some); whether the parts Grammar, Concepts, Discussion,
Notes have anything (`grammar-p', `concepts-p', `discussion-p',
`translations-p', `notes-p').  TEMPLATE, the template of the file:
the tables of its Sanskrit segments are laid out for the page (18.11.)."
  (let* ((split (bcat-analysis-split-proposal (alist-get 'proposal context)))
         (proposal (car split))
         (decisions (cdr split))
         ;; 18.11.: the tables of a Sanskrit segment for the page — the
         ;; parts under the words, coloured (`bcat-export-sanskrit-page-tables')
         (page (lambda (s)
                 (if (bcat-export--filled-p (alist-get 'gloss-table s))
                     (cons (cons 'gloss-table (bcat-export-sanskrit-page-tables (alist-get 'gloss-table s))) s)
                   s)))
         (segments (mapcar (lambda (s)
                             (let ((detail (cl-some (lambda (f) (bcat-export--filled-p (alist-get f s)))
                                                    bcat-export--detail-fields)))
                               (append `((detail-p . ,detail)
                                         (grammar-link . ,(if detail
                                                              (format " · [[#grammar-%s][Grammar]]"
                                                                      (alist-get 'num s))
                                                            "")))
                                       (if (member template '("sanskrit" "bilingual")) (funcall page s) s))))
                           (alist-get 'segments context)))
         (filled (lambda (key) (bcat-export--filled-p (alist-get key context))))
         (any (lambda (key items) (cl-some (lambda (s) (bcat-export--filled-p (alist-get key s))) items))))
    (append
     `((segments . ,segments)
       ,@(and (equal template "bilingual-tibetan")
              `((sanskrit-segments . ,(mapcar page (alist-get 'sanskrit-segments context)))))
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
                         (cl-some (lambda (s) (alist-get 'detail-p s)) segments)))
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
:overview TEXT :lectures TEXT :lecturer NAME) — the reading PDF
\(template pdf-<template>); when the file has „* Worum es geht“, that
section as a document of its own (template pdf-overview, Carsten
19.11.: „ein PDF mit dem Philologie Part und eines mit Worum es
geht“); and, when the sentence has lectures (#+CAT_LECTURES), the
document of the whole lectures (template pdf-lectures, Carsten 5.11.:
„das was einen konkreten Vers betrifft in einem Extradokument“), NAME
the lecturer (the first lecture's name up to its comma).  nil without
a pdf template."
  (require 'bcat-relayout)
  (let* ((template (bcat-sentence-file-p file))
         (source (and template (tibetan-sentence--source-file-from-analysis file)))
         (name (and template (concat "pdf-" template))))
    (when (and name (bcat-template-find name source))
      (let ((read (bcat-relayout-file-context file)))
        (unless (plist-get read :error)
          (let* ((context (bcat-export--view-context
                           ;; 16.11.: „Prüfen:“ in the PDF as in the file
                           (bcat-analysis-with-padapatha-checks (plist-get read :context))
                           template))
                 (lectures (alist-get 'lectures context)))
            (list :view (bcat-template-render-named name context (plist-get read :source))
                  :overview (and (bcat-export--filled-p (alist-get 'overview context))
                                 (bcat-template-find "pdf-overview" source)
                                 (bcat-template-render-named "pdf-overview" context (plist-get read :source)))
                  :lectures (and lectures (bcat-template-find "pdf-lectures" source)
                                 (bcat-template-render-named "pdf-lectures" context (plist-get read :source)))
                  :lecturer (and lectures
                                 (string-trim (car (split-string (alist-get 'lecture-name (car lectures))
                                                                 "[,(]")))))))))))

(defun bcat-export-view-with-overview (views)
  "The view of VIEWS (`bcat-export-views') with „Worum es geht“ as its
first part again — the one document of 16.11.–19.11., what the PDF of a
dossier still is (`bcat-dossier-pdf-text'): the part of :overview set
before the first part of :view.  Without :overview the view as it is."
  (let ((view (plist-get views :view))
        (overview (plist-get views :overview)))
    (if (and view overview (string-match "^\\* " overview))
        (let ((part (substring overview (match-beginning 0)))
              (at (or (string-match "^\\* " view) (length view))))
          (concat (substring view 0 at) part (substring view at)))
      view)))

(defcustom bcat-export-bar-width 90
  "Characters the titles of the bar may take together before they are
shortened (`bcat-export--bar-labels') — one line at the foot."
  :type 'integer
  :group 'buddhist-cat)

(defun bcat-export--bar-labels (titles)
  "The labels of the bar for TITLES: the titles themselves, or — when
together longer than `bcat-export-bar-width' (5.11., Einführung MSA IX:
eight titles over three lines) — for a numbered title its number
\(„1.“, „IX.60“), for another its first word."
  (if (<= (apply #'+ (mapcar #'length titles)) bcat-export-bar-width)
      titles
    (mapcar (lambda (title)
              (if (string-match "\\`\\([0-9IVXL]+\\(?:[.)][0-9]*\\)*\\)\\(?:[ \t]\\|\\'\\)" title)
                  (match-string 1 title)
                (car (split-string title))))
            titles)))

(defun bcat-export--parts-pages (&rest _)
  "In the current (export) buffer: every part — a heading of level 1 with
a CUSTOM_ID (the PDF view) — after the first starts a new page, and the
foot of every page carries a bar of links to the parts (the reMarkable
follows internal links since 2.6), placed after the running head
\(whose \\fancyhf{} would clear it).  Without parts, or with one alone
\(19.11.: „Worum es geht“ as a document of its own — one needs no bar),
nothing changes.  For `org-export-before-processing-functions'."
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
      (when (cdr parts)
        (setq parts (nreverse parts))
        (dolist (p (cdr parts))
          ;; a part of a view starts a page; a heading of a document
          ;; (nav-N, `bcat-export--auto-parts') does not
          (unless (string-prefix-p "nav-" (nth 0 p))
            (goto-char (nth 2 p))
            (insert "#+LATEX: \\clearpage\n")))
        (goto-char (point-min))
        (let ((last nil))
          (while (re-search-forward "^#\\+LATEX_HEADER:.*$" nil t)
            (setq last (point)))
          (if last (progn (goto-char last) (forward-line 1)) (goto-char (point-min))))
        ;; the bar on every page — the title page's plain style too
        (insert "#+LATEX_HEADER: \\newcommand{\\bcatbar}{"
                (mapconcat #'identity
                           (cl-mapcar (lambda (p label)
                                        (format "\\hyperref[%s]{%s}" (nth 0 p) (bcat-export--latex-escape label)))
                                      parts (bcat-export--bar-labels (mapcar (lambda (p) (nth 1 p)) parts)))
                           "\\enspace·\\enspace ")
                "}\\fancyfoot[C]{\\bcatbar}\\fancypagestyle{plain}{\\fancyhf{}\\fancyfoot[C]{\\bcatbar}}\n")
        (dolist (p parts) (set-marker (nth 2 p) nil))))))

(defvar bcat-export-navbar-auto nil
  "Non-nil: `bcat-export--auto-parts' gives a document without parts of
its own the bar of links (as #+CAT_NAVBAR: t in the document does).")

(defun bcat-export--auto-parts (&rest _)
  "In the current (export) buffer of a document of the readings without
parts of its own (Carsten 5.11.: the bar „überall“ — Einführung,
Schlüsselbegriffe, the lectures; #+CAT_NAVBAR: t or
`bcat-export-navbar-auto'): its headings of level 1 get the jump
targets nav-1 … (into the drawer they have, or one of their own), so
that `bcat-export--parts-pages' sets the bar — without a new page per
heading.  Only with 2 to 9 headings: one needs no bar, ten and more
make it too long.  For `org-export-before-processing-functions'."
  (save-excursion
    (goto-char (point-min))
    (when (and (or bcat-export-navbar-auto
                   (re-search-forward "^#\\+CAT_NAVBAR:[ \t]*t[ \t]*$" nil t))
               (progn (goto-char (point-min))
                      (not (re-search-forward "^[ \t]*:CUSTOM_ID:" nil t))))
      (goto-char (point-min))
      (let ((heads '()))
        (while (re-search-forward "^\\* " nil t)
          (push (copy-marker (line-end-position)) heads))
        (setq heads (nreverse heads))
        (when (<= 2 (length heads) 9)
          (cl-loop for m in heads for i from 1
                   do (goto-char m)
                   (forward-line 1)
                   (if (looking-at "[ \t]*:PROPERTIES:[ \t]*$")
                       (progn (forward-line 1) (insert (format ":CUSTOM_ID: nav-%d\n" i)))
                     (insert (format ":PROPERTIES:\n:CUSTOM_ID: nav-%d\n:END:\n" i)))))
        (dolist (m heads) (set-marker m nil))))))

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
                        (insert (bcat-export--font-lines) (if bcat-export-page (bcat-export-page-lines) "")
                                (or head "")))))
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
                            (org-export-before-processing-functions
                             (append (list insert-head #'bcat-export--drop-machine-sections
                                           #'bcat-export--plain-zettel-links
                                           #'bcat-export--rechunk-gloss-tables
                                           #'bcat-export--rechunk-header-tables
                                           #'bcat-export--wrap-wide-tables
                                           #'bcat-export--marks-apart
                                           #'bcat-export--table-runs
                                           #'bcat-export--auto-parts
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

(defconst bcat-export--copy-prefix "pdfview-"
  "The start of the name of an export copy beside its file (the view of
a sentence file, the lectures, a document of the readings) — no dot:
LuaLaTeX writes no file whose name begins with one (6.11., „I can't
write on file `.pdfview-….log'“); no „sent-“: no folder scan takes it
for a sentence file.")

(defconst bcat-export--device-index ".bcat-remarkable.eld"
  "The file in a folder of PDFs (handouts/) that remembers what C-c u H
has put on the reMarkable of the documents that go there only when
they change: ((PDF-NAME . MD5) …), MD5 of the text the PDF was set from
\(`bcat-export--on-device-p', `bcat-export--note-on-device').")

(defun bcat-export--device-entries (pdf)
  "The entries of `bcat-export--device-index' in the folder of PDF, or nil."
  (let ((path (expand-file-name bcat-export--device-index (file-name-directory pdf))))
    (and (file-readable-p path)
         (ignore-errors
           (with-temp-buffer
             (insert-file-contents path)
             (goto-char (point-min))
             (let ((value (read (current-buffer)))) (and (listp value) value)))))))

(defun bcat-export--on-device-p (pdf hash)
  "Non-nil when the reMarkable has PDF as set from the text with HASH —
C-c u H put it there (`bcat-export--note-on-device')."
  (equal hash (cdr (assoc (file-name-nondirectory pdf) (bcat-export--device-entries pdf)))))

(defun bcat-export--note-on-device (pdf hash)
  "Remember that PDF, set from the text with HASH, is on the reMarkable.
Never signals: remembering is no reason to spoil an export."
  (condition-case err
      (let ((entries (cons (cons (file-name-nondirectory pdf) hash)
                           (cl-remove (file-name-nondirectory pdf) (bcat-export--device-entries pdf)
                                      :key #'car :test #'equal)))
            (coding-system-for-write 'utf-8-unix)
            (print-length nil) (print-level nil))
        (with-temp-file (expand-file-name bcat-export--device-index (file-name-directory pdf))
          (insert ";; -*- mode: lisp-data; coding: utf-8 -*-\n"
                  ";; Buddhist CAT: was C-c u H aufs reMarkable gelegt hat (PDF . MD5 des Texts).\n")
          (prin1 (sort entries (lambda (a b) (string< (car a) (car b)))) (current-buffer))
          (insert "\n")))
    (error (message "reMarkable-Stand nicht vermerkt (%s): %s"
                    (file-name-nondirectory pdf) (error-message-string err)))))

(defconst bcat-export--overview-suffix "-worum-es-geht"
  "What the name of the PDF of „Worum es geht“ adds to that of its file's.")

(defun bcat-export--overview-pdf (file views head target)
  "The document „Worum es geht“ of the sentence FILE (:overview of VIEWS,
Carsten 19.11.: „ein PDF mit dem Philologie Part und eines mit Worum es
geht“) as PDF beside TARGET, the PDF of the file
\(<name>-worum-es-geht.pdf), or nil.  Returns (PDF . HASH) only when the
reMarkable does not have this text yet (`bcat-export--on-device-p') —
then C-c u H puts it there and notes HASH; a section nobody changed (a
hand text at a new fetch) is neither set anew nor sent again: a copy
per export would crowd the device."
  (let ((text (plist-get views :overview)))
    (when (and text target)
      (let ((out (concat (file-name-sans-extension target) bcat-export--overview-suffix ".pdf"))
            (hash (md5 text nil nil 'utf-8)))
        (unless (and (file-exists-p out) (bcat-export--on-device-p out hash))
          (let ((src (expand-file-name (concat bcat-export--copy-prefix (file-name-base file)
                                               bcat-export--overview-suffix ".org")
                                       (file-name-directory file))))
            (unwind-protect
                (let ((pdf (progn (let ((coding-system-for-write 'utf-8-unix))
                                    (with-temp-file src (insert text)))
                                  (bcat-export-pdf src head))))
                  (when (and pdf (file-exists-p pdf)
                             (file-equal-p (file-name-directory pdf) (file-name-directory file)))
                    (make-directory (file-name-directory out) t)
                    (rename-file pdf out t)
                    (cons out hash)))
              (when (file-exists-p src) (delete-file src)))))))))

(defun bcat-export--upload-more (status-file title done more)
  "Put the documents MORE on the reMarkable one after the other — rmapi
one at a time — after the PDF whose status line is DONE: each (PDF
LABEL TAG AFTER), LABEL its name in the folder of TITLE, TAG its word
in the status of STATUS-FILE („ · TAG ✓“), AFTER called without
arguments when it is there.  A failure ends the row: after a refusal
nothing follows."
  (when more
    (pcase-let ((`(,pdf ,label ,tag ,after) (car more)))
      (tibetan-discussion-paper--upload
       pdf title label
       (lambda (ok what)
         (let ((done (concat done (if ok (format " · %s ✓" tag) (format " · %s: Fehler — %s" tag what)))))
           (bcat-status-step status-file :upload done)
           (when ok
             (when after (funcall after))
             (bcat-export--upload-more status-file title done (cdr more)))))))))

(defun bcat-export-file (file)
  "C-c u H for the sentence FILE: PDF with running head, moved into the
handouts folder under its speaking name (Carsten 30.10.: „Das PDF
brauchen wir nicht noch mal im Analyse Ordner“), then onto the
reMarkable (`bcat-export-upload').  Exported is the view (Carsten
5.11.: one document with jump marks, `bcat-export-view'), written
beside FILE as pdfview-<name>.org (`bcat-export--copy-prefix') — no
folder scan takes it for a sentence file — and removed after the
export; without a view the file itself.  A file of a verse with more
than one layer (bhāṣya, commentary — Carsten 5.11.: the reading
dossier) exports the dossier's PDF instead (`bcat-dossier-pdf-text'),
under the name and label of the verse — from any of its files, the
commentary document too.  19.11.: „Worum es geht“ of a sentence file
is a second PDF beside the first (`bcat-export--overview-pdf'), put on
the reMarkable after it (and a new document of the lectures) as „<label>
Worum es geht“ when its text is new there; the dossier keeps the
section in its one PDF.  Returns the PDF path (the first's)."
  (let* ((status-file file)
         (dossier (and (fboundp 'bcat-dossier-pdf-text)
                       (condition-case err (bcat-dossier-pdf-text file)
                         (error (message "Lesedossier-PDF von %s nicht gesetzt (%s)"
                                         (file-name-nondirectory file) (error-message-string err))
                                nil))))
         (file (or (plist-get dossier :lead) file))
         (info (bcat-export-info file))
         (head (bcat-export-head-lines (plist-get info :folder) (plist-get info :work)
                                       (plist-get info :sent-num) (plist-get info :verse)))
         (views (condition-case err (bcat-export-views file)
                  (error (message "PDF-Ansicht von %s nicht gesetzt (%s) — die Datei selbst"
                                  (file-name-nondirectory file) (error-message-string err))
                         nil)))
         (view (or (plist-get dossier :text) (plist-get views :view)))
         ;; 19.11.: „Worum es geht“ as a PDF of its own — beside the
         ;; file's, in handouts/ or (without that folder) beside the file
         (overview (and view (not dossier)
                        (bcat-export--overview-pdf
                         file views head
                         (or (plist-get info :handout) (concat (file-name-sans-extension file) ".pdf")))))
         ;; only what that export lacked — without it the variable holds
         ;; an earlier export's
         (overview-missing (and overview bcat-export--missing))
         (lectures (bcat-export--lectures-pdf file views head (plist-get info :handout)))
         (view-file (and view (expand-file-name (concat bcat-export--copy-prefix (file-name-nondirectory file))
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
    ;; the characters the second PDF lacks are reported with the first's
    (dolist (m overview-missing)
      (unless (member m bcat-export--missing)
        (setq bcat-export--missing (append bcat-export--missing (list m)))))
    (bcat-status-step status-file :pdf (if bcat-export--missing
                                    (concat "PDF ✓ · fehlende Zeichen: "
                                            (bcat-export--missing-text bcat-export--missing))
                                  "PDF ✓"))
    (if bcat-export-upload
        (progn
          (require 'tibetan-discussion-paper)
          (bcat-status-step status-file :upload "reMarkable …")
          (tibetan-discussion-paper--upload
           pdf (plist-get info :work) (plist-get info :label)
           (lambda (ok what)
             (let ((done (if ok (concat "reMarkable ✓ " what) (concat "reMarkable: Fehler — " what))))
               (bcat-status-step status-file :upload done)
               ;; The lectures (as before) and „Worum es geht“ after
               ;; the PDF — rmapi one at a time.
               (when ok
                 (bcat-export--upload-more
                  status-file (plist-get info :work) done
                  (delq nil
                        (list (and lectures
                                   (list lectures
                                         (format "%s %s" (plist-get info :label) (plist-get views :lecturer))
                                         "Vortrag" nil))
                              (and overview
                                   (list (car overview)
                                         (format "%s Worum es geht" (plist-get info :label))
                                         "Worum es geht"
                                         (lambda () (bcat-export--note-on-device (car overview) (cdr overview)))))))))))))
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
             (src (expand-file-name (concat bcat-export--copy-prefix (file-name-base file) "-lectures.org")
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

(defun bcat-export-reading-document (file)
  "C-c u H in a document of the readings that is neither a cascade source
nor a sentence file (Carsten 5.11.: Einführung, Schlüsselbegriffe —
„nur die Dokumente für die Readings“): the PDF on its own page (A4 —
„in Handouts A4 ist sehr fein“; `bcat-export-page' nil) with the bar of
its headings (#+CAT_NAVBAR: t, in an export copy; FILE stays as it
is) — into handouts/ of its text, onto the reMarkable under its title.
Returns the PDF."
  (let* ((text (with-temp-buffer (insert-file-contents file) (buffer-string)))
         (title (or (bcat-export--title file) (file-name-base file)))
         (full (or (bcat-sentence--header-value file "TITLE") title))
         (folder (file-name-nondirectory (directory-file-name (file-name-directory file))))
         (copy (expand-file-name (concat bcat-export--copy-prefix (file-name-nondirectory file)) (file-name-directory file)))
         (target (bcat-export--handout file (file-name-base file)))
         (pdf (unwind-protect
                  (progn
                    (let ((coding-system-for-write 'utf-8-unix))
                      (with-temp-file copy
                        ;; 6.11. (Carsten: „es gab PDF error“): the class
                        ;; loads geometry already — a second \usepackage
                        ;; with options is an „Option clash“; its options
                        ;; go to \geometry, which may follow (9.11.: after
                        ;; the package without options, `bcat-export--geometry').
                        (insert "#+CAT_NAVBAR: t\n"
                                (replace-regexp-in-string
                                 "^\\(#\\+LATEX_HEADER:[ \t]*\\)\\\\usepackage\\[\\([^]\n]*\\)\\]{geometry}"
                                 (lambda (m)
                                   (concat (match-string 1 m) bcat-export--geometry
                                           "\\geometry{" (match-string 2 m) "}"))
                                 text t t))))
                    (let ((bcat-export-page nil))
                      (bcat-export-pdf copy (bcat-export-head-lines folder title nil nil))))
                (when (file-exists-p copy) (delete-file copy)))))
    (when (and target pdf (file-exists-p pdf))
      (make-directory (file-name-directory target) t)
      (rename-file pdf target t)
      (setq pdf target))
    (if bcat-export-upload
        (progn (require 'tibetan-discussion-paper)
               (tibetan-discussion-paper--upload pdf folder full))
      (message "PDF: %s" pdf))
    pdf))

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
        (if (and (fboundp 'bcat-dossier-export-pending-p) (bcat-dossier-export-pending-p file))
            ;; 5.11.: the other half of the verse is still coming — its
            ;; landing exports the dossier, once.
            (bcat-status-step file :pdf "PDF folgt mit dem Rest des Verses")
          (bcat-export-file file))
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
     ;; 5.11.: a document of the readings (Einführung …), no cascade
     ;; source, no sentence file — its copy for the device.
     ((and file (string-match-p "\\.org\\'" file)
           (not (string-match-p "\\`sent-[0-9]" (file-name-nondirectory file)))
           (not (equal "cascade" (bcat-sentence--header-value file "TIBETAN_LAYOUT"))))
      (when (buffer-modified-p) (save-buffer))
      (bcat-export-reading-document file))
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
