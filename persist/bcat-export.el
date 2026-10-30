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
(defvar org-export-before-processing-functions)
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

(defun bcat-export--handout (source name)
  "Where the PDF named NAME goes beside SOURCE: <its folder>/handouts/
NAME.pdf (Carsten 30.10.: „zusätzlich im Handouts Ordner“), ASCII like
the old handouts there (msa-par-60.pdf): diacritics dropped, anything
else but letters, digits and dots a hyphen; nil without SOURCE."
  (when source
    (let ((ascii (replace-regexp-in-string
                  "[^A-Za-z0-9.]+" "-"
                  (replace-regexp-in-string "[^[:ascii:]]" "" (ucs-normalize-NFD-string name)))))
      (expand-file-name (concat (string-trim ascii "-+" "-+") ".pdf")
                        (expand-file-name "handouts" (file-name-directory source))))))

(defun bcat-export--commentary-info (file)
  "`bcat-export-info' for the commentary document FILE: the folder of
the commentary source, the work of the verses it comments on (beside
their PDFs on the reMarkable), the verses from #+CAT_VERSE, named after
the document; on disk beside the verses' PDFs too (savbh-IX.60.pdf in
the handouts of the Sanskrit source)."
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

(defun bcat-export--font-lines ()
  "#+LATEX_HEADER lines defining \\bcattib and \\bcatdeva, and loading
adjustbox (`bcat-export--table-filter')."
  (concat
   "#+LATEX_HEADER: \\usepackage{fontspec}\n"
   "#+LATEX_HEADER: \\usepackage{adjustbox}\n"
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
    (if (or (<= (string-width unit) bcat-export-unit-max) (null open))
        (format "\\mbox{%s}" unit)
      (let ((words (split-string (substring unit (+ open 2)))))
        (mapconcat (lambda (w) (format "\\mbox{%s}" w))
                   (cons (concat (substring unit 0 (+ open 2)) (car words)) (cdr words))
                   " ")))))

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
                (substring text 0 end) "\n"
                "\\end{adjustbox}"
                (let ((tail (substring text end)))
                  (if (string-empty-p tail) "\n" tail))))
    text))

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
                        (insert (bcat-export--font-lines) (or head "")))))
    (unwind-protect
        (with-current-buffer buf
          (when (buffer-modified-p) (save-buffer))
          (let* ((pdf (let ((org-latex-compiler "lualatex")
                            (org-latex-pdf-process
                             '("%latex -interaction nonstopmode -output-directory %o %f"
                               "%latex -interaction nonstopmode -output-directory %o %f"))
                            (org-latex-tables-centered nil)
                            ;; The log is read first (missing characters),
                            ;; then cleared as org would.
                            (org-latex-remove-logfiles nil)
                            (org-export-filter-table-functions
                             (append (remq 'org-latex-auto-tabularx org-export-filter-table-functions)
                                     (list #'bcat-export--table-filter)))
                            (org-export-filter-verse-block-functions
                             (cons #'bcat-export--verse-filter org-export-filter-verse-block-functions))
                            (org-export-before-processing-functions
                             (append (list insert-head #'bcat-export--drop-machine-sections)
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
reMarkable (`bcat-export-upload').  Returns the PDF path."
  (let* ((info (bcat-export-info file))
         (pdf (bcat-export-pdf file (bcat-export-head-lines
                                     (plist-get info :folder) (plist-get info :work)
                                     (plist-get info :sent-num) (plist-get info :verse)))))
    (let ((target (plist-get info :handout)))
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
             (bcat-status-step file :upload (if ok (concat "reMarkable ✓ " what)
                                              (concat "reMarkable: Fehler — " what))))))
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
