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

(declare-function tibetan-sentence--source-file-from-analysis "tibetan-sentence-persist" (analysis-file))
(declare-function tibetan-sentence--sent-id-from-filename "tibetan-sentence-persist" (filename))
(declare-function tibetan-sentence--filepath "tibetan-sentence-persist" (sent-num &optional folder source-file))
(declare-function tibetan-cascade--collect-sentences "tibetan-cascade" ())
(declare-function tibetan-cascade--scope-at-point "tibetan-cascade" ())
(declare-function tibetan-discussion-paper--upload "tibetan-discussion-paper" (pdf title label))
(declare-function tibetan-discussion-paper--temp-source-p "tibetan-discussion-paper" (file))
(declare-function tibetan-handout "tibetan-handout" (&optional org-only))
(declare-function org-latex-export-to-pdf "ox-latex")
(defvar org-latex-compiler)
(defvar org-latex-pdf-process)
(defvar org-latex-tables-centered)
(defvar org-export-filter-table-functions)
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
\(italic) · Satz SENT-NUM · VERSE (left), the page number (right)."
  (concat
   "#+LATEX_HEADER: \\usepackage{fancyhdr}\\pagestyle{fancy}\\fancyhf{}\\renewcommand{\\headrulewidth}{0.4pt}\n"
   (format "#+LATEX_HEADER: \\fancyhead[L]{\\small %s}\\fancyhead[R]{\\small\\thepage}\n"
           (mapconcat #'identity
                      (delq nil (list (bcat-export--latex-escape folder)
                                      (format "\\textit{%s}" (bcat-export--latex-escape work))
                                      (format "Satz %d" sent-num)
                                      (and verse (bcat-export--latex-escape verse))))
                      " · "))))

(defun bcat-export-info (file)
  "What the PDF of the sentence FILE is labelled with: (:source :folder
:work :sent-num :verse :label) — folder and work title from the source
\(its #+TITLE before „ — “), the verse from its :VERSE:."
  (let* ((src (tibetan-sentence--source-file-from-analysis file))
         (n (tibetan-sentence--sent-id-from-filename file))
         (text (if (and src (file-readable-p src))
                   (with-temp-buffer (insert-file-contents src) (buffer-string))
                   ""))
         (title (and (string-match "^#\\+TITLE:[ \t]*\\(.+\\)$" text)
                     (string-trim (car (split-string (match-string 1 text) " — ")))))
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
          :label (format "Satz %d" n))))

;; ----------------------------------------------------------------------------
;; Export and upload
;; ----------------------------------------------------------------------------

(defun bcat-export-pdf (file &optional head)
  "Export the sentence FILE to PDF beside it and return the PDF path.
HEAD (lines from `bcat-export-head-lines') goes into the export copy,
never into the file.  LuaLaTeX, two runs (longtable), tables flush
left at natural width — bound here, Carsten's own export settings
\(centring, tabularx) stay untouched outside."
  (require 'ox-latex)
  (let* ((visited (get-file-buffer file))
         (buf (or visited (find-file-noselect file)))
         (insert-head (lambda (&rest _)
                        (when head
                          (goto-char (point-min))
                          (insert head)))))
    (unwind-protect
        (with-current-buffer buf
          (when (buffer-modified-p) (save-buffer))
          (let ((org-latex-compiler "lualatex")
                (org-latex-pdf-process
                 '("%latex -interaction nonstopmode -output-directory %o %f"
                   "%latex -interaction nonstopmode -output-directory %o %f"))
                (org-latex-tables-centered nil)
                (org-export-filter-table-functions
                 (remq 'org-latex-auto-tabularx org-export-filter-table-functions))
                (org-export-before-processing-functions
                 (cons insert-head org-export-before-processing-functions)))
            (expand-file-name (org-latex-export-to-pdf) (file-name-directory file))))
      (unless visited (kill-buffer buf)))))

(defun bcat-export-file (file)
  "C-c u H for the sentence FILE: PDF with running head, then onto the
reMarkable (`bcat-export-upload').  Returns the PDF path."
  (let* ((info (bcat-export-info file))
         (pdf (bcat-export-pdf file (bcat-export-head-lines
                                     (plist-get info :folder) (plist-get info :work)
                                     (plist-get info :sent-num) (plist-get info :verse)))))
    (if bcat-export-upload
        (progn
          (require 'tibetan-discussion-paper)
          (tibetan-discussion-paper--upload pdf (plist-get info :work) (plist-get info :label)))
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
                      (file-name-nondirectory file) (error-message-string err))))))

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
            (mapc #'bcat-export-file ready)
            (message "C-c u H: %d PDF(s)%s" (length ready)
                     (if (> old 0)
                         (format ", %d Satzdatei(en) noch ohne Vorlage — erst C-c u G" old)
                       "")))))))))

(provide 'bcat-export)
;;; bcat-export.el ends here
