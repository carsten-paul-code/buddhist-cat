;;; bcat-export-test.el --- Tests for C-c u H on template files  -*- lexical-binding: t; -*-

;;; Commentary:
;; §5.74 step 4 (Carsten 28.10.2026): the handout is no function of its
;; own any more — C-c u H exports exactly the sentence file to PDF and
;; puts it on the reMarkable; C-c u G ends with it.  PDF: no title
;; block, tables flush left at natural width, a running head
;; "folder · work · Satz N · verse".  rmapi and LaTeX are stubbed except
;; in the skip-unless smoke test.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((root (expand-file-name ".." (file-name-directory
                                    (or load-file-name buffer-file-name)))))
  (dolist (d '("persist" "core" "analysis" "philology" "data" "config" "doc-prep" "test"))
    (add-to-list 'load-path (expand-file-name d root))))

(require 'org)
(require 'bcat-export)
(require 'bcat-sentence-test)

(ert-deftest bcat-export-running-head ()
  "The running head: folder of the source · work title (from the
source's #+TITLE before „ — “, italic) · Satz N · verse — LaTeX-safe."
  (should (equal (concat "#+LATEX_HEADER: \\usepackage{fancyhdr}\\pagestyle{fancy}\\fancyhf{}\\renewcommand{\\headrulewidth}{0.4pt}\n"
                         "#+LATEX_HEADER: \\fancyhead[L]{\\small Tibetisch V · \\textit{bDen pa gnyis la 'jug pa (Satyadvayāvatāra)} · Satz 9 · SDA 9}\\fancyhead[R]{\\small\\thepage}\n")
                 (bcat-export-head-lines "Tibetisch V" "bDen pa gnyis la 'jug pa (Satyadvayāvatāra)" 9 "SDA 9")))
  (should (string-match-p "Satz 3}" (bcat-export-head-lines "K" "W" 3 nil)))
  (should (string-match-p (regexp-quote "A\\&B 50\\%") (bcat-export-head-lines "A&B 50%" "W" 1 nil))))

(ert-deftest bcat-export-info-from-file ()
  "Folder, work, sentence and verse come from the sentence file and its
source."
  (bcat-sentence-test--with-source
    (bcat-sentence-test--create src)
    (let ((info (bcat-export-info (tibetan-sentence--filepath 9 folder src))))
      (should (equal (file-name-nondirectory (directory-file-name dir)) (plist-get info :folder)))
      (should (equal "bDen pa gnyis la 'jug pa" (plist-get info :work)))
      (should (equal 9 (plist-get info :sent-num)))
      (should (equal "SDA 9" (plist-get info :verse)))
      (should (equal "Satz 9" (plist-get info :label))))))

(ert-deftest bcat-export-after-fetch-uploads ()
  "C-c u G ends with C-c u H: after the analysis has landed, the file is
exported and uploaded once — folder = the work, document „Satz N“.
Switched off (`bcat-export-auto' nil): nothing."
  (bcat-sentence-test--with-source
    (bcat-sentence-test--create src)
    (let ((exported '()) (uploaded '()))
      (cl-letf (((symbol-function 'bcat-export-pdf)
                 (lambda (file &rest _) (push file exported) "/tmp/x.pdf"))
                ((symbol-function 'tibetan-discussion-paper--upload)
                 (lambda (pdf title label &optional _done) (push (list pdf title label) uploaded) t))
                ;; The fixture lives under the temp directory, which the
                ;; hook skips (next test) — here it stands for a real source.
                ((symbol-function 'tibetan-discussion-paper--temp-source-p) #'ignore))
        (let ((bcat-export-auto t) (bcat-export-upload t))
          (bcat-sentence-test--with-stubs bcat-sentence-test--answer
            (add-hook 'bcat-sentence-after-fetch-functions #'bcat-export-after-fetch)
            (bcat-sentence-test--fetch src)))
        (should (equal (list (tibetan-sentence--filepath 9 folder src)) exported))
        (should (equal '(("/tmp/x.pdf" "bDen pa gnyis la 'jug pa" "Satz 9")) uploaded))
        (setq exported nil uploaded nil)
        (let ((bcat-export-auto nil))
          (bcat-export-after-fetch (tibetan-sentence--filepath 9 folder src)))
        (should-not exported)
        (should-not uploaded)))))

(ert-deftest bcat-export-status ()
  "Carsten 29.10.: a status after C-c u G.  The export writes on the
sentence's status line — PDF, then the upload with the document name
as placed (or the reason it failed)."
  (bcat-sentence-test--with-source
    (bcat-sentence-test--create src)
    (let ((file (tibetan-sentence--filepath 9 folder src))
          (bcat-status-show nil)
          (bcat-status--entries (make-hash-table :test 'equal)))
      (when (get-buffer bcat-status-buffer-name) (kill-buffer bcat-status-buffer-name))
      (bcat-status-begin "C-c u G" "Satz 9" (list (cons file "Satz 9")))
      (cl-letf (((symbol-function 'bcat-export-pdf) (lambda (&rest _) "/tmp/x.pdf"))
                ((symbol-function 'tibetan-discussion-paper--upload)
                 (lambda (_pdf _title _label &optional done)
                   (funcall done t "/Reading/bDen/Satz 9 2026-10-29 1802") t)))
        (let ((bcat-export-upload t)) (bcat-export-file file)))
      (should (string-match-p
               "^  Satz 9  PDF ✓ · reMarkable ✓ /Reading/bDen/Satz 9 2026-10-29 1802$"
               (with-current-buffer bcat-status-buffer-name (buffer-string))))
      (kill-buffer bcat-status-buffer-name))))

(ert-deftest bcat-export-after-fetch-skips-fixtures ()
  "A sentence file under `temporary-file-directory' is a test fixture:
C-c u G's export hook never builds or uploads it, even with
`bcat-export-auto' on — an ERT run in a live Emacs (where the batch
runner's switch does not apply) must not put fixtures on the device
(the §5.69 incident, 26.10.: `Reading/D/Satz 4')."
  (bcat-sentence-test--with-source
    (bcat-sentence-test--create src)
    (let ((exported 0) (uploaded 0))
      (cl-letf (((symbol-function 'bcat-export-pdf)
                 (lambda (&rest _) (cl-incf exported) "/tmp/x.pdf"))
                ((symbol-function 'tibetan-discussion-paper--upload)
                 (lambda (&rest _) (cl-incf uploaded) t)))
        (let ((bcat-export-auto t) (bcat-export-upload t))
          (bcat-export-after-fetch (tibetan-sentence--filepath 9 folder src))))
      (should (= 0 exported))
      (should (= 0 uploaded)))))

(ert-deftest bcat-export-command ()
  "C-c u H: in a template sentence file it exports that file; on a
sentence in the source it exports that sentence's file; an old-layout
file is not exported (C-c u G first)."
  (bcat-sentence-test--with-source
    (bcat-sentence-test--create src)
    (let ((file (tibetan-sentence--filepath 9 folder src))
          (exported '()))
      (cl-letf (((symbol-function 'bcat-export-pdf)
                 (lambda (f &rest _) (push f exported) "/tmp/x.pdf"))
                ((symbol-function 'tibetan-discussion-paper--upload) (lambda (&rest _) t)))
        (with-current-buffer (find-file-noselect file)
          (bcat-export))
        (should (equal (list file) exported))
        (setq exported nil)
        (with-current-buffer (find-file-noselect src)
          (goto-char (point-min))
          (re-search-forward "^\\*\\*\\*\\* Segment 38")
          (bcat-export))
        (should (equal (list file) exported))
        (setq exported nil)
        (let ((bcat-sentence-templates nil))
          (delete-file file)
          (bcat-sentence-test--create src))
        (with-current-buffer (find-file-noselect src)
          (goto-char (point-min))
          (re-search-forward "^\\*\\*\\*\\* Segment 38")
          (bcat-export))
        (should-not exported)))))

(ert-deftest bcat-export-replaces-discussion-paper ()
  "C-c u D (the old discussion paper) in a source with a template: the
paper is gone (its sections no longer exist) — C-c u H runs instead."
  (bcat-sentence-test--with-source
    (bcat-sentence-test--create src)
    (let ((exported 0) (built 0))
      (cl-letf (((symbol-function 'bcat-export) (lambda () (interactive) (cl-incf exported)))
                ((symbol-function 'tibetan-discussion-paper-build)
                 (lambda (&rest _) (cl-incf built) "/tmp/x.org")))
        (require 'tibetan-discussion-paper)
        (with-current-buffer (find-file-noselect src)
          (goto-char (point-min))
          (re-search-forward "^\\*\\*\\*\\* Segment 38")
          (tibetan-discussion-paper)))
      (should (= 1 exported))
      (should (= 0 built)))))

(ert-deftest bcat-export-verse-units ()
  "29.10.: the glossed verse lines are longer than the text width; LaTeX
broke them inside a gloss (hyphenated „[ob-struction]“, „[CONV“ / „·
cond.]“) and left the shad „/“ alone on a line.  A glossed line is
made of unbreakable word+gloss units — a break falls only between
them — and the shad is tied to the last; such a verse is set ragged
right (justified, TeX let the longest line run into the margin rather
than stretch three spaces).  Plain lines and plain verses stay as they
are."
  (should (equal (concat "\\begin{verse}\n"
                         "\\rightskip=0pt plus 1fil\\relax%\n"
                         "\\mbox{rnal 'byor pa [yogin]} \\mbox{yis [ERG]} "
                         "\\mbox{na [CONV · cond.]}~/~/\\\\\n"
                         "brjod du med pa/ /\\\\\n"
                         "\\end{verse}\n")
                 (bcat-export--verse-units
                  (concat "\\begin{verse}\n"
                          "rnal 'byor pa [yogin] yis [ERG] na [CONV · cond.] / /\\\\\n"
                          "brjod du med pa/ /\\\\\n"
                          "\\end{verse}\n"))))
  (let ((plain "\\begin{verse}\nbrjod du med pa/ /\\\\\n\\end{verse}\n"))
    (should (equal plain (bcat-export--verse-units plain)))))

(ert-deftest bcat-export-verse-scripts ()
  "29.10.: Uchen above Wylie, Devanāgarī above IAST.  Through the
fallback font the Devanāgarī lost its conjuncts (visible virāmas) and
the Uchen came out small.  A line in the original script gets its own
font, larger (as the old handout's Uchen line); after the transcription
line of each pair a little space.  Roman-only verses stay as they are."
  (should (equal (concat "\\begin{verse}\n"
                         "{\\bcattib\\large བརྗོད་དུ། །}\\\\\n"
                         "brjod du/ /\\\\[1ex]\n"
                         "{\\bcatdeva\\large अत्र ।}\\\\\n"
                         "atra /\\\\[1ex]\n"
                         "\\end{verse}\n")
                 (bcat-export--verse-scripts
                  (concat "\\begin{verse}\n"
                          "བརྗོད་དུ། །\\\\\n"
                          "brjod du/ /\\\\\n"
                          "अत्र ।\\\\\n"
                          "atra /\\\\\n"
                          "\\end{verse}\n"))))
  (let ((roman "\\begin{verse}\nbrjod du/ /\\\\\n\\end{verse}\n"))
    (should (equal roman (bcat-export--verse-scripts roman)))))

(ert-deftest bcat-export-drops-word-analysis ()
  "A Sanskrit sentence file keeps its `** Word Analysis' (surface →
lemma, read by the zettel highlighting, the discussion and the
padapāṭha PDF) — the PDF leaves it out: machine data, the table and
the Forms say it for the reader.  The rest stays."
  (with-temp-buffer
    (insert "* Sentence\n** Grammar\nG\n\n** Translations compared\nT\n\n"
            "** Word Analysis\n*** Segment 1\nka\n\n- ka — ka; ind.\n\n"
            "* Working Translation\nW\n")
    (org-mode)
    (bcat-export--drop-machine-sections)
    (should (equal (concat "* Sentence\n** Grammar\nG\n\n** Translations compared\nT\n\n"
                           "* Working Translation\nW\n")
                   (buffer-string)))))

(ert-deftest bcat-export-pdf-smoke ()
  "Real LuaLaTeX run (only where lualatex is installed): the reviewed
Tibetan sample becomes a PDF; its .tex has no title, no centred and no
tabularx tables, and carries the running head."
  (skip-unless (executable-find "lualatex"))
  (let* ((dir (make-temp-file "bcat-export-" t))
         (file (expand-file-name "sent-009-bden.org" dir)))
    (unwind-protect
        (progn
          (copy-file (expand-file-name "../Templates/analysis/samples/Tibetan.org"
                                       (file-name-directory (locate-library "bcat-export-test")))
                     file)
          (let ((pdf (bcat-export-pdf file (bcat-export-head-lines "Tibetisch V" "bDen" 9 "SDA 9"))))
            (should (file-exists-p pdf)))
          (let ((tex (with-temp-buffer
                       (insert-file-contents (expand-file-name "sent-009-bden.tex" dir))
                       (buffer-string))))
            (should-not (string-match-p "\\\\maketitle" tex))
            (should-not (string-match-p "\\\\begin{center}" tex))
            (should-not (string-match-p "\\\\begin{tabularx}" tex))
            (should (string-match-p "Satz 9 · SDA 9" tex))
            (should (string-match-p (regexp-quote "\\mbox{brjod du med pa [indescribable]}") tex))
            (should (string-match-p (regexp-quote "\\newfontfamily\\bcattib{Noto Serif Tibetan}") tex))
            (should (string-match-p (regexp-quote "{\\bcattib\\large བརྗོད་དུ་མེད་པ་བལྟར་མེད་པ། །}") tex))))
      (let ((buf (get-file-buffer file))) (when buf (kill-buffer buf)))
      (delete-directory dir t))))

(provide 'bcat-export-test)
;;; bcat-export-test.el ends here
