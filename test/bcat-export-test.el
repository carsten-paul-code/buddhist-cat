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
                 (lambda (pdf title label) (push (list pdf title label) uploaded) t)))
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
            (should (string-match-p "Satz 9 · SDA 9" tex))))
      (let ((buf (get-file-buffer file))) (when buf (kill-buffer buf)))
      (delete-directory dir t))))

(provide 'bcat-export-test)
;;; bcat-export-test.el ends here
