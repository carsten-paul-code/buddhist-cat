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
(require 'bcat-analysis-test)

(defun bcat-export-test--view-of (file)
  "The file C-c u H writes the view of the sentence FILE to (5.11.)."
  (expand-file-name (concat ".pdfview-" (file-name-nondirectory file)) (file-name-directory file)))

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
      (should (equal "SDA 9" (plist-get info :label))))))

(ert-deftest bcat-export-speaking-names ()
  "Carsten 30.10.: „dass die PDFs möglichst sprechend benannt sind z.B.
...IX.60.pdf und zusätzlich im Handouts Ordner gespeichert sind“.  The
document on the reMarkable is named after the verse (SDA 9, MSA
IX.60), without one „Satz N“; the copy in the source's handouts/ is
<short>-<verse number> (bden-9.pdf, msa-IX.60.pdf), without a verse
<short>-satz-N — beside the old handouts (<short>-par-60 …)."
  (bcat-sentence-test--with-source
    (bcat-sentence-test--create src)
    (let ((info (bcat-export-info (tibetan-sentence--filepath 9 folder src))))
      (should (equal "SDA 9" (plist-get info :label)))
      (should (equal (expand-file-name "handouts/bden-9.pdf" dir) (plist-get info :handout))))
    (with-temp-file src
      (insert (replace-regexp-in-string ":VERSE: SDA 9\n" "" bcat-sentence-test--source)))
    (let ((info (bcat-export-info (tibetan-sentence--filepath 9 folder src))))
      (should (equal "Satz 9" (plist-get info :label)))
      (should (equal (expand-file-name "handouts/bden-satz-9.pdf" dir) (plist-get info :handout))))))

(ert-deftest bcat-export-handouts-at-text-level ()
  "Carsten 31.10. (AS Satz 1002, the PDF in Sanskrit/handouts/): „Das
Muster sollte … immer gleich sein … Handouts kommen in den handouts
Ordner auf der obersten Ebene des Textes.“  The text's folder is the one
right below the semester (WS26-27/, SS26/) — the source's own folder
for TĀ and Tibetisch V, the one above for AS and MSA with their language
folders; without a semester the source's folder.  #+CAT_HANDOUTS:
names the handouts folder itself (relative to the source)."
  (dolist (c '(("/x/WS26-27/Abhidharmasamuccaya/Sanskrit/asam-1-cat.org" . "/x/WS26-27/Abhidharmasamuccaya/")
               ("/x/WS26-27/Mahayanasutralamkara/Sthiramati-9/savbh-9-cat.org" . "/x/WS26-27/Mahayanasutralamkara/")
               ("/x/WS26-27/Tantraloka/tantraloka-1.org" . "/x/WS26-27/Tantraloka/")
               ("/x/WS26-27/Tibetisch V/bden-gnyis-cat.org" . "/x/WS26-27/Tibetisch V/")
               ("/x/SS26/Tibetisch IV/work in progress/Milarepa-prepared.org" . "/x/SS26/Tibetisch IV/")
               ("/tmp/bcat-x/src.org" . "/tmp/bcat-x/")))
    (should (equal (cdr c) (bcat-export--text-folder (car c)))))
  (let* ((dir (make-temp-file "bcat-handouts-" t))
         (src (expand-file-name "WS26-27/Werk/Sanskrit/src.org" dir)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory src) t)
          (with-temp-file src (insert "#+TITLE: W\n"))
          (should (equal (expand-file-name "WS26-27/Werk/handouts/src-1.1.pdf" dir)
                         (bcat-export--handout src "src-1.1")))
          (with-temp-file src (insert "#+TITLE: W\n#+CAT_HANDOUTS: ../../Andere/handouts\n"))
          (should (equal (expand-file-name "WS26-27/Andere/handouts/src-1.1.pdf" dir)
                         (bcat-export--handout src "src-1.1"))))
      (delete-directory dir t))))

(ert-deftest bcat-export-wraps-wide-tables ()
  "31.10., TĀ Satz 1008 (and TĀ 1, 1000, 1001, MSA 130, 132): tables
written before the wrapping of long cells have a cell wider than the
page; the PDF shrank them to an unreadable size.  The export copy
breaks such a cell at its „·“ into further lines of the table (the
file stays as it is); a table without a long cell is left alone."
  (let ((wide (concat "| vakṣyamāṇa-ṣaḍardha-śāstra-artha-garbhīkāreṇa |\n"
                      "| √vac · ṣaḍ-ardha · śāstra · artha · garbhī-√kṛ-a |\n"
                      "| √vac (2) speak · -ṣyamāṇa fut. pass. part. · ṣaḍ six · ardha half · śāstra treatise · artha meaning · garbhī- womb/containing · √kṛ (8) make · -a action noun |\n"
                      "| by incorporating the meaning of the Ṣaḍardhaśāstra to be expounded |\n"
                      "| m. inst. sg. |\n"))
        (narrow "| iha | khalu |\n| —   | —     |\n"))
    (with-temp-buffer
      (insert "* Segments\n** Segment 687\n" wide "\n" narrow "\n⟦687⟧ *Translation:* Here.\n")
      (org-mode)
      (bcat-export--wrap-wide-tables)
      (let ((lines (split-string (buffer-string) "\n")))
        (should (cl-every (lambda (l) (or (not (string-prefix-p "|" l))
                                          (<= (string-width l) bcat-export-wide-table-width)))
                          lines))
        (should (member "| iha | khalu |" lines))
        (should (string-match-p "-a action noun" (buffer-string)))
        (should (string-match-p "^⟦687⟧ \\*Translation:\\* Here\\.$" (buffer-string)))))))

(ert-deftest bcat-export-file-moves-only-its-own-pdf ()
  "30.10.: the first version moved whatever path the export returned —
a test stub's /tmp/x.pdf existed on Carsten's machine and was moved
into the fixture and deleted with it.  Only the PDF beside the sentence
file (the one the export made) goes to handouts/; any other file stays
where it is."
  (bcat-sentence-test--with-source
    (bcat-sentence-test--create src)
    (let ((elsewhere (make-temp-file "bcat-foreign-" nil ".pdf" "FOREIGN")))
      (unwind-protect
          (cl-letf (((symbol-function 'bcat-export-pdf) (lambda (&rest _) elsewhere)))
            (let ((bcat-export-upload nil))
              (bcat-export-file (tibetan-sentence--filepath 9 folder src))
              (should (file-exists-p elsewhere))
              (should-not (file-exists-p (expand-file-name "handouts/bden-9.pdf" dir)))))
        (when (file-exists-p elsewhere) (delete-file elsewhere))))))

(ert-deftest bcat-export-file-moves-to-handouts ()
  "C-c u H (and so C-c u G) puts the PDF into the handouts folder under
its speaking name — Carsten 30.10.: „Das PDF brauchen wir nicht noch
mal im Analyse Ordner“ — each export anew, the latest wins."
  (bcat-sentence-test--with-source
    (bcat-sentence-test--create src)
    (let* ((file (tibetan-sentence--filepath 9 folder src))
           (pdf (concat (file-name-sans-extension file) ".pdf"))
           (copy (expand-file-name "handouts/bden-9.pdf" dir))
           (content "PDF 1"))
      (cl-letf (((symbol-function 'bcat-export-pdf)
                 (lambda (&rest _) (with-temp-file pdf (insert content)) pdf)))
        (let ((bcat-export-upload nil))
          (should (equal copy (bcat-export-file file)))
          (should-not (file-exists-p pdf))
          (should (equal "PDF 1" (with-temp-buffer (insert-file-contents copy) (buffer-string))))
          (setq content "PDF 2")
          (bcat-export-file file)
          (should-not (file-exists-p pdf))
          (should (equal "PDF 2" (with-temp-buffer (insert-file-contents copy) (buffer-string)))))))))

(ert-deftest bcat-export-after-fetch-uploads ()
  "C-c u G ends with C-c u H: after the analysis has landed, the file is
exported and uploaded once — folder = the work, document named after
the verse („SDA 9“).
Switched off (`bcat-export-auto' nil): nothing."
  (bcat-sentence-test--with-source
    (bcat-sentence-test--create src)
    (let ((exported '()) (uploaded '()))
      (cl-letf (((symbol-function 'bcat-export-pdf)
                 (lambda (file &rest _) (push file exported) "/nonexistent/bcat-test.pdf"))
                ((symbol-function 'tibetan-discussion-paper--upload)
                 (lambda (pdf title label &optional _done) (push (list pdf title label) uploaded) t))
                ;; The fixture lives under the temp directory, which the
                ;; hook skips (next test) — here it stands for a real source.
                ((symbol-function 'tibetan-discussion-paper--temp-source-p) #'ignore))
        (let ((bcat-export-auto t) (bcat-export-upload t))
          (bcat-sentence-test--with-stubs bcat-sentence-test--answer
            (add-hook 'bcat-sentence-after-fetch-functions #'bcat-export-after-fetch)
            (bcat-sentence-test--fetch src)))
        ;; 5.11.: exported is the file's view (one document with jump marks).
        (should (equal (list (bcat-export-test--view-of (tibetan-sentence--filepath 9 folder src))) exported))
        (should (equal '(("/nonexistent/bcat-test.pdf" "bDen pa gnyis la 'jug pa" "SDA 9")) uploaded))
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
      (cl-letf (((symbol-function 'bcat-export-pdf) (lambda (&rest _) "/nonexistent/bcat-test.pdf"))
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
                 (lambda (&rest _) (cl-incf exported) "/nonexistent/bcat-test.pdf"))
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
                 (lambda (f &rest _) (push f exported) "/nonexistent/bcat-test.pdf"))
                ((symbol-function 'tibetan-discussion-paper--upload) (lambda (&rest _) t)))
        (with-current-buffer (find-file-noselect file)
          (bcat-export))
        ;; 5.11.: exported is the file's view.
        (should (equal (list (bcat-export-test--view-of file)) exported))
        (setq exported nil)
        (with-current-buffer (find-file-noselect src)
          (goto-char (point-min))
          (re-search-forward "^\\*\\*\\*\\* Segment 38")
          (bcat-export))
        (should (equal (list (bcat-export-test--view-of file)) exported))
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
                         "\\mbox{rnal 'byor pa \\bcatg{[yogin]}} \\mbox{yis \\bcatg{[ERG]}} "
                         "\\mbox{na \\bcatg{[CONV · cond.]}}~/~/\\\\\n"
                         "brjod du med pa/ /\\\\\n"
                         "\\end{verse}\n")
                 (bcat-export--verse-units
                  (concat "\\begin{verse}\n"
                          "rnal 'byor pa [yogin] yis [ERG] na [CONV · cond.] / /\\\\\n"
                          "brjod du med pa/ /\\\\\n"
                          "\\end{verse}\n"))))
  (let ((plain "\\begin{verse}\nbrjod du med pa/ /\\\\\n\\end{verse}\n"))
    (should (equal plain (bcat-export--verse-units plain)))))

(ert-deftest bcat-export-verse-units-long-gloss ()
  "30.10., TĀ Satz 1001: „śiva-śāsana-āgama-rahasya-ācchādana-dhvaṃsi
[destroying the concealment of the secret of Śiva's teaching-tradition]“
is one unit longer than the line and ran into the margin.  A unit
longer than `bcat-export-unit-max' may break between the words of its
gloss — never inside a word; the bracket stays with its word and its
last gloss word.  Shorter units stay whole (29.10.)."
  (let ((bcat-export-unit-max 50))
    (should (equal (concat "\\begin{verse}\n"
                           "\\rightskip=0pt plus 1fil\\relax%\n"
                           "\\mbox{me \\bcatg{[my]}} \\mbox{dhvaṃsi \\bcatg{[destroying}} \\mbox{\\bcatg{the}} \\mbox{\\bcatg{concealment}}"
                           " \\mbox{\\bcatg{of}} \\mbox{\\bcatg{Śiva's}} \\mbox{\\bcatg{teaching-tradition]}}~//\\\\\n"
                           "\\end{verse}\n")
                   (bcat-export--verse-units
                    (concat "\\begin{verse}\n"
                            "me [my] dhvaṃsi [destroying the concealment of Śiva's teaching-tradition] //\\\\\n"
                            "\\end{verse}\n"))))))

(ert-deftest bcat-export-tables-fit ()
  "30.10., TĀ Satz 1001: the metre table (19 syllables and the pause)
ran off the page.  A table wider than the line is scaled down to it
\(adjustbox, max width) — narrower ones keep their size; the package
comes with the header."
  (should (equal (concat "\\begin{adjustbox}{max width=\\linewidth}\n"
                         "\\begin{tabular}{ll}\na & b\\\\\n\\end{tabular}\n"
                         "\\end{adjustbox}\n")
                 (bcat-export--table-filter "\\begin{tabular}{ll}\na & b\\\\\n\\end{tabular}\n"
                                            'latex nil)))
  (should (string-match-p (regexp-quote "\\usepackage{adjustbox}") (bcat-export--font-lines))))

(ert-deftest bcat-export-table-ends-paragraph ()
  "30.10., TĀ 1002–1007: since 1f6da5b „⟦683⟧ Translation:“ hung on
the segment table in the PDF — the table filter cut the blank line
after the table, and the next paragraph ran on beside the adjustbox.
The table keeps its blank line: in the LaTeX of a table and a
paragraph, a blank line stands between them."
  (should (string-suffix-p "\\end{adjustbox}\n\n"
                           (bcat-export--table-filter
                            "\\begin{tabular}{l}\na\\\\\n\\end{tabular}\n\n" 'latex nil)))
  (require 'ox-latex)
  (let* ((org-export-filter-table-functions (list #'bcat-export--table-filter))
         (latex (org-export-string-as "| a | b |\n\n⟦683⟧ *Translation:* Not having heard\n"
                                      'latex t)))
    (should (string-match-p "\\\\end{adjustbox}\n\n+⟦683⟧" latex))))

(ert-deftest bcat-export-missing-characters ()
  "30.10.: Satz 1001 showed boxes for ¦ and the status said „PDF ✓“ — a
missing glyph is only a LaTeX warning.  The export reads the log:
the status line names the missing characters."
  (should (equal '(("¦" . "Noto Sans Devanagari") ("⏑" . "Times New Roman"))
                 (bcat-export--missing-characters
                  (concat "Missing character: There is no ¦ (U+00A6) in font name:Noto Sans Devanagari:mode=harf;script=dev2;language=dflt;!\n"
                          "Missing character: There is no ¦ (U+00A6) in font name:Noto Sans Devanagari:mode=harf;!\n"
                          "Missing character: There is no ⏑ (U+23D1) in font Times New Roman/OT:script=latn;!\n"))))
  ;; The real log quotes the font (30.10., TĀ 1.1 with Sanderson): the
  ;; first version read the font as „"name“.  An invisible character
  ;; (U+009A, from a mis-decoded „Ś“ in a transcript) is named by its
  ;; code in the status.
  (let ((real (concat "Missing character: There is no \u009a (U+009A) in font \"name:Times New Roman:mode=node;script=latn;language=dflt;\"!\n"
                      "Missing character: There is no ¦ (U+00A6) in font \"name:Noto Sans Devanagari:mode=harf;script=dev2;\"!\n")))
    (should (equal '(("\u009a" . "Times New Roman") ("¦" . "Noto Sans Devanagari"))
                   (bcat-export--missing-characters real)))
    (should (equal "U+009A (Times New Roman), ¦ (Noto Sans Devanagari)"
                   (bcat-export--missing-text (bcat-export--missing-characters real)))))
  (should-not (bcat-export--missing-characters "Output written on x.pdf (7 pages).\n"))
  (bcat-sentence-test--with-source
    (bcat-sentence-test--create src)
    (let* ((file (tibetan-sentence--filepath 9 folder src))
           (pdf (concat (file-name-sans-extension file) ".pdf")))
      (cl-letf (((symbol-function 'bcat-export-pdf)
                 (lambda (&rest _)
                   (setq bcat-export--missing '(("¦" . "Noto Sans Devanagari")))
                   (with-temp-file pdf (insert "PDF")) pdf)))
        (let ((bcat-export-upload nil)
              (bcat-status-show nil)
              (bcat-status--entries (make-hash-table :test 'equal)))
          (bcat-status-begin "C-c u H" "Satz 9" (list (cons file "SDA 9")))
          (bcat-export-file file)
          (should (string-match-p "^  SDA 9  PDF ✓ · fehlende Zeichen: ¦ (Noto Sans Devanagari)$"
                                  (with-current-buffer bcat-status-buffer-name (buffer-string))))
          (kill-buffer bcat-status-buffer-name))))))

(ert-deftest bcat-export-colours ()
  "Carsten 31.10.: reading the segment on the reMarkable (Paper Pro,
colour) „fällt es mir schwer … die einzelnen Wörter im Überblick zu
halten“ — colour, word against gloss.  The first try (words blue) came
out greyish on the device and took contrast from the words: „kehren wir
es einfach so um“ — the words stay black; blue (\bcatg) are the glosses
in the line with the vocabulary and in every table the rows below the
words (formation, parts, gloss, form).  The metre table of the
Recitation stays uncoloured (its second row only – and ⏑).  Same
evening, TĀ 1006: „In den Segmenttabellen sollte dann auch die
Übersetzung der einzelnen Terme schwarz sein, dann sehe ich gleich was
wichtig ist“ — the gloss row (next to last, as in
`bcat-sentence--dictionary-words') is black too; per band of a table
cut by \\hline."
  (let ((head (bcat-export--font-lines)))
    (should (string-match-p (regexp-quote "\\usepackage{xcolor}") head))
    (should (string-match-p (regexp-quote "\\definecolor{bcatgloss}{HTML}{1F4E9E}") head))
    (should (string-match-p (regexp-quote "\\newcommand{\\bcatg}[1]{\\textcolor{bcatgloss}{#1}}") head)))
  (let ((out (bcat-export--table-filter
              (concat "\\begin{tabular}{ll}\n"
                      "iha & \\href{steinert:x}{khalu}\\\\\n"
                      "here & indeed\\\\\n"
                      "ind. & \\\\\n"
                      "\\end{tabular}\n\n")
              'latex nil)))
    (should (string-match-p (regexp-quote "\niha & \\href{steinert:x}{khalu}\\\\\n") out))
    (should (string-match-p (regexp-quote "\nhere & indeed\\\\\n") out))
    (should (string-match-p (regexp-quote "\n\\bcatg{ind.} & \\\\\n") out)))
  ;; Sanskrit: word, formation, parts, gloss, form — formation, parts
  ;; and form blue.
  (let ((out (bcat-export--table-filter
              (concat "\\begin{tabular}{l}\n"
                      "an-ākarṇya\\\\\n"
                      "an- + ā-√karṇ-ya\\\\\n"
                      "an- not · √karṇ (10) listen\\\\\n"
                      "not having heard\\\\\n"
                      "gerund\\\\\n"
                      "\\end{tabular}\n\n")
              'latex nil)))
    (should (string-match-p (regexp-quote "\nan-ākarṇya\\\\\n") out))
    (should (string-match-p (regexp-quote "\n\\bcatg{an- + ā-√karṇ-ya}\\\\\n") out))
    (should (string-match-p (regexp-quote "\n\\bcatg{an- not · √karṇ (10) listen}\\\\\n") out))
    (should (string-match-p (regexp-quote "\nnot having heard\\\\\n") out))
    (should (string-match-p (regexp-quote "\n\\bcatg{gerund}\\\\\n") out)))
  ;; A table in bands (\\hline between): each band its own words and gloss.
  (let ((out (bcat-export--table-filter
              (concat "\\begin{tabular}{l}\n"
                      "bden\\\\\n" "truth\\\\\n" "N\\\\\n"
                      "\\hline\n"
                      "gnyis\\\\\n" "two\\\\\n" "NUM\\\\\n"
                      "\\end{tabular}\n\n")
              'latex nil)))
    (should (string-match-p (regexp-quote "\ngnyis\\\\\ntwo\\\\\n\\bcatg{NUM}\\\\\n") out))
    (should (string-match-p (regexp-quote "\nbden\\\\\ntruth\\\\\n\\bcatg{N}\\\\\n") out)))
  (let* ((metre (concat "\\begin{tabular}{lll}\n"
                        "ta & da & nā\\\\\n"
                        "⏑ & – & ¦\\\\\n"
                        "\\end{tabular}\n\n"))
         (out (bcat-export--table-filter metre 'latex nil)))
    (should-not (string-match-p "bcatg" out))))

(ert-deftest bcat-export-tibetan-tables ()
  "Carsten 31.10. abends, Tibetisch V: the tables carry the Uchen of
each word above the Wylie, both black — only the function row blue.
The Uchen is set in the Tibetan font (the main font has none)."
  (let ((out (bcat-export--table-filter
              (concat "\\begin{tabular}{ll}\n"
                      "དེ & བརྒྱུད་པའི\\\\\n"
                      "\\href{steinert:de}{de} & brgyud pa'i\\\\\n"
                      "that & transmitted\\\\\n"
                      "DEM & V pf.\\\\\n"
                      "\\end{tabular}\n\n")
              'latex nil)))
    (should (string-match-p (regexp-quote "\n{\\bcattib\\large དེ} & {\\bcattib\\large བརྒྱུད་པའི}\\\\\n") out))
    (should (string-match-p (regexp-quote "\n\\href{steinert:de}{de} & brgyud pa'i\\\\\n") out))
    (should (string-match-p (regexp-quote "\nthat & transmitted\\\\\n") out))
    (should (string-match-p (regexp-quote "\n\\bcatg{DEM} & \\bcatg{V pf.}\\\\\n") out))))

(ert-deftest bcat-export-verse-scripts-foreign-signs ()
  "30.10., TĀ Satz 1001: in the recitation the pause sign ¦ stands in
the Devanāgarī line, and Noto Sans Devanagari has no ¦ — an empty box
in the PDF (LuaLaTeX: „Missing character: There is no ¦ (U+00A6) in
font Noto Sans Devanagari“, 12 times).  A sign of neither the line's
script nor ASCII is set in the main font, inside the line."
  (should (equal (concat "\\begin{verse}\n"
                         "{\\bcatdeva\\large यस्मात् {\\normalfont ¦} ह्यानन्द ।}\\\\*\n"
                         "yasmāt ¦ hyānanda /\\\\[1ex]\n"
                         "\\end{verse}\n")
                 (bcat-export--verse-scripts
                  (concat "\\begin{verse}\n"
                          "यस्मात् ¦ ह्यानन्द ।\\\\\n"
                          "yasmāt ¦ hyānanda /\\\\\n"
                          "\\end{verse}\n")))))

(ert-deftest bcat-export-verse-scripts ()
  "29.10.: Uchen above Wylie, Devanāgarī above IAST.  Through the
fallback font the Devanāgarī lost its conjuncts (visible virāmas) and
the Uchen came out small.  A line in the original script gets its own
font, larger (as the old handout's Uchen line); after the transcription
line of each pair a little space.  Roman-only verses stay as they are.
30.10.: no page break between a line in the original script and its
transcription (AS 3, Segment 22: Uchen at the foot of one page, its
Wylie at the top of the next) — `\\\\*' after the script line."
  (should (equal (concat "\\begin{verse}\n"
                         "{\\bcattib\\large བརྗོད་དུ། །}\\\\*\n"
                         "brjod du/ /\\\\[1ex]\n"
                         "{\\bcatdeva\\large अत्र ।}\\\\*\n"
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

(ert-deftest bcat-export-drops-handout ()
  "Carsten 30.10.: the AS sentence files keep `* Handout' (the input of
handout-synopse.py, carried through every rebuild) — the PDF of the
sentence file leaves it out, with all its children; it stays in the
file.  Other sections at the end stay in the PDF."
  (with-temp-buffer
    (insert "* Working Translation\nW\n\n* My Notes\n\n* Footnotes\n\n"
            "* Handout\n# Eingabe für handout-synopse.py\n** Segment 22\n*** Skt.\nWhy?\n"
            "** SG 2015 (Japanese, English draft)\n[Q] Why?\n\n"
            "* Eigene Liste\nL\n")
    (org-mode)
    (bcat-export--drop-machine-sections)
    (should (equal (concat "* Working Translation\nW\n\n* My Notes\n\n* Footnotes\n\n"
                           "* Eigene Liste\nL\n")
                   (buffer-string)))))

(ert-deftest bcat-export-detail-rows-empty-cells ()
  "Bug (Carsten 2.11.: „es kommen jetzt wieder PDF Warnungen“ — „PDF
file produced with errors“ nach C-c u G auf TĀ 1006 und 1007): eine
Zeile mit zwei leeren Zellen nebeneinander — die Teile-Zeile, wenn alle
Wörter Pronomen sind (anyaiḥ yat tena), LaTeX „ &  & \\\\“ — wurde am
Trenner falsch zerlegt: das Leerzeichen-gierige [ \t]+&[ \t]+ fraß den
Raum vor dem zweiten &, das & selbst wurde zur Zelle \\bcatg{&}, LaTeX
meldete „Missing } inserted“.  Leere Zellen bleiben leer, ein \\& im
Text bleibt in seiner Zelle."
  (let ((out (bcat-export--detail-rows
              (concat "\\begin{tabular}{lll}\n"
                      "anyaiḥ & yat & tena\\\\\n"
                      "— & — & —\\\\\n"
                      " &  & \\\\\n"
                      "R\\&D & a & b\\\\\n"
                      "by others & whatever & by that\\\\\n"
                      "pron. & pron. & pron.\\\\\n"
                      "\\end{tabular}"))))
    (should-not (string-match-p (regexp-quote "\\bcatg{&}") out))
    (should (string-match-p (regexp-quote "\n &  & \\\\\n") out))
    (should (string-match-p (regexp-quote "\n\\bcatg{R\\&D} & \\bcatg{a} & \\bcatg{b}\\\\\n") out))
    (should (string-match-p (regexp-quote "\n\\bcatg{—} & \\bcatg{—} & \\bcatg{—}\\\\\n") out))))

(ert-deftest bcat-export-marks-apart ()
  "4.11.: in the file ⟦N⟧ stands right above its table; in the PDF it
must not sit beside it (ad89035) — the export copy gets the blank line
back.  Other lines before a table stay as they are."
  (with-temp-buffer
    (insert "* Text\n⟦66⟧\n| a |\n\n| b |\n\n<<skt-44>>⟦44⟧ · [[tib-110][Tib. 110]]\n| c |\n"
            "#+ATTR_LATEX: :align r\n| d |\n")
    (bcat-export--marks-apart)
    (should (equal (concat "* Text\n⟦66⟧\n\n| a |\n\n| b |\n\n<<skt-44>>⟦44⟧ · [[tib-110][Tib. 110]]\n\n| c |\n"
                           "#+ATTR_LATEX: :align r\n| d |\n")
                   (buffer-string)))))

(ert-deftest bcat-export-rechunk-tibetan-tables ()
  "4.11.: the tables of a Tibetan segment fill the Emacs window in the
file; the export copy chunks them anew for the page (6 per column,
`bcat-export-tibetan-table-width' 130 — the width the PDF had).  Other
tables (the comparison's, Sanskrit ones) and other segments' tables stay
apart."
  (with-temp-buffer
    (insert "⟦70⟧\n"
            "| ཆོས་ཉིད               | འདི                | ལ             | གཞོལ           | ཞིང                  | འབབ                  |\n"
            "| chos nyid           | 'di              | la            | gzhol          | zhing               | 'bab                |\n"
            "| nature of phenomena | this             | toward / into | incline toward | and                 | flow into / lead to |\n"
            "| N · goal/locus      | DEM · specifying | DAT/LOC       | V pres.        | CONV · coordinative | V pres. · main verb |\n"
            "\n→ t\n\n| Nr. | Stelle |\n|-----+--------|\n|   1 | /a/    |\n")
    (bcat-export--rechunk-gloss-tables)
    (let ((text (buffer-string)))
      ;; two tables now, the first five columns, 'bab alone
      (should (string-match-p "\n| ཆོས་ཉིད +| འདི +| ལ +| གཞོལ +| ཞིང +|\n" text))
      (should (string-match-p "|\n\n| འབབ +|\n| 'bab +|\n" text))
      (should (string-match-p "\n\n→ t\n\n| Nr\\. | Stelle |\n|-----\\+--------|\n|   1 | /a/    |\n\\'" text)))))

(defun bcat-export-test--fits-page (table)
  "Non-nil when the org TABLE fits the PDF page by the export's measure:
each column's widest line plus 6, within `bcat-export-table-width'."
  (<= (1+ (apply #'+ (mapcar (lambda (col)
                               (+ 6 (apply #'max (mapcar #'string-width (split-string (string-join col "\n") "\n")))))
                             (let* ((rows (mapcar (lambda (l) (mapcar #'string-trim (butlast (cdr (split-string l "|")))))
                                                  (split-string table "\n" t))))
                               (cl-loop for j below (length (car rows))
                                        collect (mapcar (lambda (r) (nth j r)) rows))))))
      bcat-export-table-width))

(ert-deftest bcat-export-rechunk-sanskrit-tables ()
  "5.11. (generic): the tables of a segment fill the Emacs window in the
file — Sanskrit as well since 5.11. —; the export copy lays them out
anew for the page, whatever the language: the run of tables under a
⟦N⟧ mark, wide cells broken as far as it saves lines.  Nothing lost:
read back, the columns are those of the file."
  (let* ((file-tables (bcat-analysis-sanskrit-tables ; 5.11.: one table in the file, too wide for the page
                       (append (seq-take bcat-analysis-test--ta-1-1-words 5)
                               (last bcat-analysis-test--ta-1-1-words))))
         (columns (apply #'append (mapcar #'bcat-analysis--table-columns (split-string file-tables "\n\n")))))
    (should-not (cl-every #'bcat-export-test--fits-page (split-string file-tables "\n\n")))
    (with-temp-buffer
      (insert "* Text\n⟦1⟧\n" file-tables "\n\n→ t\n\n| Nr. | Stelle |\n|-----+--------|\n|   1 | /a/    |\n")
      (bcat-export--rechunk-gloss-tables)
      (let* ((text (buffer-string))
             (run (progn (string-match "⟦1⟧\n\\(\\(?:|.*\n\\|\n\\)+\\)\n→ t" text)
                         (string-trim (match-string 1 text))))
             (tables (split-string run "\n\n")))
        (should (cl-every #'bcat-export-test--fits-page tables))
        (should (equal columns (apply #'append (mapcar #'bcat-analysis--table-columns tables))))
        (should (string-match-p "\n→ t\n\n| Nr\\. | Stelle |\n|-----\\+--------|\n|   1 | /a/    |\n\\'" text))))))

(ert-deftest bcat-export-rechunk-header-tables ()
  "The tables of the parts of a word (a header row, its rule, the parts
below) are laid out for the page too (5.11., TĀ 1.1 ⟦2⟧: seven members
side by side, 558 pt on a line of 466, shrunk to 84 %): wider than
`bcat-export-table-width', split into tables of their own, each with its
header and rule, a row empty in a part left out.  A narrow one, and the
overview of a comparison (#+ATTR_LATEX, fixed columns), stay as they
are."
  (with-temp-buffer
    (insert "** ⟦2⟧\n"
            "| tad  | ubhaya | yāmala      | sphurita                 |\n"
            "|------+--------+-------------+--------------------------|\n"
            "| that | both   | pair, union | /√sphur/ (6) flash forth |\n"
            "|      |        |             | /-ita/ ppp               |\n"
            "\n"
            "| hṛd |\n|-----|\n| heart |\n\n"
            "#+ATTR_LATEX: :align rp{4cm}\n"
            "| No. | Passage that is long enough to be wide |\n"
            "|-----+----------------------------------------|\n"
            "|   1 | /vimalā-kalā-āśrayā/ and more words here |\n")
    (let ((bcat-export-table-width 40))
      (bcat-export--rechunk-header-tables))
    (should (equal (concat "** ⟦2⟧\n"
                           "| tad  | ubhaya | yāmala      |\n"
                           "|------+--------+-------------|\n"
                           "| that | both   | pair, union |\n"
                           "\n"
                           "| sphurita                 |\n"
                           "|--------------------------|\n"
                           "| /√sphur/ (6) flash forth |\n"
                           "| /-ita/ ppp               |\n"
                           "\n"
                           "| hṛd |\n|-----|\n| heart |\n\n"
                           "#+ATTR_LATEX: :align rp{4cm}\n"
                           "| No. | Passage that is long enough to be wide |\n"
                           "|-----+----------------------------------------|\n"
                           "|   1 | /vimalā-kalā-āśrayā/ and more words here |\n")
                   (buffer-string)))))

(ert-deftest bcat-export-parts-tables-blue ()
  "5.11. (Carsten, draft B): in the PDF the tables of the words' parts —
a header row (the members, black), \\hline, the parts — have the parts
in \\bcatg, as the formation and parts rows before; the comparison's
overview, also a table with a header row, keeps its \\cellcolor and no
\\bcatg (`bcat-export-comparison-table')."
  (should (equal (concat "\\begin{tabular}{ll}\nvimala & kalā\\\\\n\\hline\n"
                         "\\bcatg{\\emph{vi-} apart} & \\bcatg{part, power}\\\\\n"
                         "\\bcatg{\\emph{-a} adj} & \\\\\n\\end{tabular}")
                 (bcat-export--detail-rows
                  (concat "\\begin{tabular}{ll}\nvimala & kalā\\\\\n\\hline\n"
                          "\\emph{vi-} apart & part, power\\\\\n"
                          "\\emph{-a} adj & \\\\\n\\end{tabular}")))))

(ert-deftest bcat-export-parts-line-blue ()
  "5.11.: the line of a word of one member — the word black, its parts in
\\bcatg.  Only a paragraph that is that one line: a Structure paragraph
with lines of the same shape stays as it is, as does every other
backend."
  (should (equal "\\emph{jananī}: \\bcatg{\\emph{√jan} (1) be born · \\emph{-anī} f. agent}\n"
                 (bcat-export--parts-line-filter
                  "\\emph{jananī}: \\emph{√jan} (1) be born · \\emph{-anī} f. agent\n" 'latex nil)))
  (let ((structure "\\textbf{Structure:} x \\\\\n\\emph{a}: b\n"))
    (should (equal structure (bcat-export--parts-line-filter structure 'latex nil))))
  (should (equal "\\emph{a}: b\n" (bcat-export--parts-line-filter "\\emph{a}: b\n" 'html nil))))

(ert-deftest bcat-export-comparison-table ()
  "2.11. (Muster vergleich-ta1-1.org): the overview table of the
comparison — the ratings with \\cellcolor{ok|mid|bad} — needs colortbl
and the Muster's three colours in the header; as a table with a header
row (one row, then \\hline) it is no table of words: no row in
\\bcatg."
  (let ((head (bcat-export--font-lines)))
    (should (string-match-p (regexp-quote "\\usepackage{colortbl}") head))
    (should (string-match-p (regexp-quote "\\definecolor{ok}{RGB}{200,235,200}") head))
    (should (string-match-p (regexp-quote "\\definecolor{mid}{RGB}{250,235,160}") head))
    (should (string-match-p (regexp-quote "\\definecolor{bad}{RGB}{245,190,185}") head)))
  (let ((tabular (concat "\\begin{tabular}{rp{4.3cm}p{1.6cm}p{8cm}}\n"
                         "Nr. & Stelle & Einstufung & Kern\\\\\n"
                         "\\hline\n"
                         "1 & \\emph{a} & \\cellcolor{bad}rot & x\\\\\n"
                         "2 & \\emph{b} & \\cellcolor{ok}grün & y\\\\\n"
                         "3 & \\emph{c} & \\cellcolor{mid}gelb & z\\\\\n"
                         "\\end{tabular}")))
    (should (equal tabular (bcat-export--detail-rows tabular)))))

(ert-deftest bcat-export-comparison-smoke ()
  "Real LuaLaTeX run: the comparison's units (**** headings) export as
headings, not as list items (H:4 bound for the export), the cell colours
reach the LaTeX, and the PDF is made."
  (skip-unless (executable-find "lualatex"))
  (let* ((dir (make-temp-file "bcat-export-" t))
         (file (expand-file-name "sent-001-tantralo.org" dir)))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "#+TITLE: Sentence 1 Analysis\n#+OPTIONS: toc:nil num:nil title:nil author:nil date:nil\n"
                    "#+CAT_TEMPLATE: sanskrit\n\n* Sentence\n** Translations compared\n"
                    "*Translation:* May my heart flash forth.\n\n"
                    "*** Übersicht\nSigel: C = Translation (oben), D = Dyczkowski 2023\n\n"
                    "#+ATTR_LATEX: :align rp{4.3cm}p{1.6cm}p{8cm} :font \\small\n"
                    "| Nr. | Stelle | Einstufung                   | Kern |\n"
                    "|-----+--------+------------------------------+------|\n"
                    "|   1 | /a/    | @@latex:\\cellcolor{bad}@@rot | x    |\n\n"
                    "*** Die Stellen im Einzelnen\n**** 1. /a/ — rot\n- D :: „a“\n*Befund:* b\n"))
          (let ((bcat-export-keep-tex t))
            (should (file-exists-p (bcat-export-pdf file))))
          (let ((tex (with-temp-buffer
                       (insert-file-contents (expand-file-name "sent-001-tantralo.tex" dir))
                       (buffer-string))))
            (should (string-match-p (regexp-quote "\\cellcolor{bad}rot") tex))
            (should (string-match-p (regexp-quote "\\paragraph*{1. \\emph{a} — rot}") tex))
            (should-not (string-match-p (regexp-quote "\\item 1. \\emph{a}") tex))
            (should-not (string-match-p "\\\\bcatg{[23] &" tex))))
      (let ((buf (get-file-buffer file))) (when buf (kill-buffer buf)))
      (delete-directory dir t))))

(ert-deftest bcat-export-no-false-reference-warning ()
  "Bug (Carsten 2.11.: „Warning (ox-latex): PDF file produced with
warnings: [undefined reference]“ bei jedem zweisprachigen AS-Satz): die
Sprungmarken (<<skt-N>>, [[tib-M][Tib. M]]) sind im ersten LaTeX-Lauf
noch unbekannt, im zweiten aufgelöst — org prüfte aber die Ausgabe des
ersten Laufs (die zwei Läufe lagen in umgekehrter Folge in *Org PDF
LaTeX Output*, die Suche ab der letzten „This is …TeX“-Zeile traf den
ersten).  Ein Dokument mit interner Verknüpfung gibt keine Warnung."
  (skip-unless (executable-find "lualatex"))
  (let* ((dir (make-temp-file "bcat-export-" t))
         (file (expand-file-name "sent-006-asam.org" dir))
         (warnings '()))
    (unwind-protect
        (progn
          (with-temp-file file
            (insert "#+TITLE: Sentence 6\n#+OPTIONS: toc:nil num:nil title:nil\n\n* Text\n"
                    "<<skt-44>>⟦44⟧ · [[tib-110][Tib. 110]]\n\nOne.\n\n"
                    "* Tibetan\n<<tib-110>>⟦110⟧ · [[skt-44][Skt. 44]]\n\nTwo.\n"))
          (cl-letf (((symbol-function 'display-warning)
                     (lambda (type message &rest _) (push (cons type message) warnings))))
            (should (file-exists-p (bcat-export-pdf file))))
          (should-not (cl-some (lambda (w) (string-match-p "undefined reference" (cdr w))) warnings)))
      (let ((buf (get-file-buffer file))) (when buf (kill-buffer buf)))
      (delete-directory dir t))))

;; ----------------------------------------------------------------------------
;; One document with jump marks (Carsten 5.11.2026)
;; ----------------------------------------------------------------------------

(defun bcat-export-test--parts (view)
  "The level-1 headings of VIEW, in order."
  (let ((out '()) (start 0))
    (while (string-match "^\\* \\(.+\\)$" view start)
      (push (match-string 1 view) out)
      (setq start (match-end 0)))
    (nreverse out)))

(defun bcat-export-test--part (view title)
  "The text of the level-1 part TITLE of VIEW (heading included)."
  (when (string-match (concat "^\\* " (regexp-quote title) "\n") view)
    (let ((start (match-beginning 0)))
      (substring view start (if (string-match "^\\* " view (1+ start)) (match-beginning 0) (length view))))))

(ert-deftest bcat-export-view-sanskrit ()
  "Carsten 5.11.: the PDF is one document with jump marks, not the file
in its order — Reading (the recitation, the translation), Segments (the
overview of the words, → per segment), Grammar (the parts of the words,
the grammar, per segment sandhi, forms, compounds, structure), Concepts,
Discussion; each part with a CUSTOM_ID (a page of its own, a link in
the bar).  Rendered from the fields C-c u L reads back; the file stays
as it is; no request; no Word Analysis."
  (bcat-sentence-test--with-sa-source
    (bcat-sentence-test--create-n src 1007)
    (bcat-sentence-test--with-stubs (bcat-sentence-test--sa-answer)
      (bcat-sentence-test--fetch-n src 1007))
    (let* ((file (tibetan-sentence--filepath 1007 folder src))
           (before (bcat-sentence-test--text-of file))
           (fired nil)
           (view (cl-letf (((symbol-function 'tibetan-discussion--submit)
                            (lambda (&rest _) (setq fired t))))
                   (bcat-export-view file))))
      (should (stringp view))
      (should-not fired)
      (should (equal before (bcat-sentence-test--text-of file)))
      (should (equal '("Reading" "Segments" "Grammar" "Concepts" "Discussion")
                     (bcat-export-test--parts view)))
      (dolist (id '("reading" "segments" "grammar" "concepts" "discussion"))
        (should (string-match-p (concat "^\\* .+\n:PROPERTIES:\n:CUSTOM_ID: " id "\n:END:$") view)))
      (let ((reading (bcat-export-test--part view "Reading"))
            (segments (bcat-export-test--part view "Segments"))
            (grammar (bcat-export-test--part view "Grammar")))
        (should (string-match-p "^#\\+begin_verse\nअत्र" reading))
        (should (string-match-p "^\\*Translation:\\* Although my speech (vāc)" reading))
        ;; The metre, not the tables of the words.
        (should-not (string-match-p "^| atra" reading))
        ;; The overview, its link into the grammar, the translation of
        ;; the segment — the parts of the words not here.
        (should (string-match-p (regexp-quote "⟦685⟧ · [[#grammar-685][Grammar]]\n| atra |") segments))
        (should (string-match-p "^→ " segments))
        (should-not (string-match-p "^|-\\|^/mad-vāk/\\|\\*Forms:\\*" segments))
        (should (string-match-p "^\\*Grammar:\\* Two clauses\\." grammar))
        (should (string-match-p "^\\*\\* ⟦685⟧\n:PROPERTIES:\n:CUSTOM_ID: grammar-685\n:END:$" grammar))
        (should (string-match-p "^\\*Forms:\\*\n- /mad-vāk/ — vāc- f\\." grammar))
        (should (string-match-p "my\\b.*speech" grammar)))
      (should (string-match-p "^⟦685⟧ \\*niyantraṇa\\*" (bcat-export-test--part view "Concepts")))
      (should (string-match-p "^\\*Dyczkowski 2023:\\* Although my speech is powerless here"
                              (bcat-export-test--part view "Discussion")))
      (should-not (string-match-p "Word Analysis\\|^\\* Working Translation\\|^\\* My Notes" view)))))

(ert-deftest bcat-export-view-tibetan ()
  "The Tibetan file as one document: Reading with every segment's Uchen
over its Wylie, Segments with the tables, Grammar with structure and
note per segment; a part with nothing in it is left out (no Concepts
without concepts); My Notes with text: a part Notes at the end."
  (bcat-sentence-test--with-source
    (bcat-sentence-test--create src)
    (bcat-sentence-test--with-stubs bcat-sentence-test--answer
      (bcat-sentence-test--fetch src))
    (let ((file (tibetan-sentence--filepath 9 folder src)))
      (with-temp-file file
        (insert (replace-regexp-in-string "^\\* My Notes\n" "* My Notes\nMeine Notiz.\n"
                                          (bcat-sentence-test--text-of file) t t)))
      (let* ((view (bcat-export-view file))
             (reading (and view (bcat-export-test--part view "Reading"))))
        (should (stringp view))
        (should (equal "Reading" (car (bcat-export-test--parts view))))
        (should (equal "Notes" (car (last (bcat-export-test--parts view)))))
        (should (string-match-p "^བརྗོད་དུ་མེད་པ་བལྟར་མེད་པ། །\nbrjod du med pa" reading))
        (should (string-match-p (regexp-quote "⟦38⟧ · [[#grammar-38][Grammar]]\n| བརྗོད་དུ་མེད་པ")
                                (bcat-export-test--part view "Segments")))
        (should (string-match-p "^\\*Structure:\\* no finite verb$" (bcat-export-test--part view "Grammar")))
        (should (string-match-p "^Meine Notiz\\.$" (bcat-export-test--part view "Notes")))))))

(ert-deftest bcat-export-view-pair ()
  "Both files of a bilingual pair as one document each: every jump mark
\(<<skt-N>>, <<tib-N>>) once, every link to one of them with its
target — the export would stop at a broken link; the other language's
lines in Reading, its tables in Segments."
  (bcat-sentence-test--with-pair
    (bcat-sentence-test--create-n src 3)
    (bcat-sentence-test--with-stubs #'bcat-sentence-test--pair-answers
      (bcat-sentence-test--fetch-n src 3))
    (dolist (file (list (tibetan-sentence--filepath 3 folder src)
                        (bcat-sentence-partner-file 3 src)))
      (let ((view (bcat-export-view file)) (start 0))
        (should (stringp view))
        (should (equal '("Reading" "Segments") (cl-subseq (bcat-export-test--parts view) 0 2)))
        (should (string-match-p "^<<skt-22>>⟦22⟧" view))
        (should (string-match-p "^<<tib-77>>⟦77⟧" view))
        (while (string-match "\\[\\[\\(\\(?:skt\\|tib\\)-[0-9]+\\)\\]" view start)
          (let ((target (match-string 1 view)))
            (setq start (match-end 0))
            (should (= 1 (with-temp-buffer
                           (insert view)
                           (how-many (concat "<<" (regexp-quote target) ">>") (point-min) (point-max)))))))
        (let ((reading (bcat-export-test--part view "Reading")))
          (should (string-match-p "kasmāt" reading))
          (should (string-match-p "ཅིའི་ཕྱིར" reading)))))))

(ert-deftest bcat-export-view-decisions ()
  "The proposed translation is read first (Reading), the decisions that
justify it belong to the discussion (Carsten 5.11.)."
  (let ((ctx (bcat-export--view-context
              '((proposal . "*Proposed translation:*\n#+begin_quote\nX.\n#+end_quote\n\n*Decisions:* Unit 1: kalā.")
                (segments . (((num . 1) (concepts . "") (sandhi . "s"))
                             ((num . 2) (concepts . "*kalā* — power")))))))
        (render (lambda (tpl ctx) (bcat-template-render tpl ctx))))
    (should (equal "*Proposed translation:*\n#+begin_quote\nX.\n#+end_quote" (alist-get 'proposal ctx)))
    (should (equal "*Decisions:* Unit 1: kalā." (alist-get 'decisions ctx)))
    (should (alist-get 'concepts-p ctx))
    (should (alist-get 'discussion-p ctx))
    (should (equal "[1 · [[#grammar-1][Grammar]]]\n[2]\n"
                   (funcall render "{{#segments}}\n[{{num}}{{grammar-link}}]\n{{/segments}}\n" ctx)))
    (should-not (alist-get 'notes-p ctx))))

(ert-deftest bcat-export-view-none ()
  "No PDF template for the file (a commentary document, a sentence file
of no template): no view — C-c u H exports the file as it is."
  (let ((dir (make-temp-file "bcat-view-" t)))
    (unwind-protect
        (let ((file (expand-file-name "notes.org" dir)))
          (with-temp-file file (insert "* Notes\n"))
          (should-not (bcat-export-view file)))
      (delete-directory dir t))))

(ert-deftest bcat-export-parts-pages ()
  "In the export buffer every part of level 1 with a CUSTOM_ID after the
first starts a new page, and the foot of every page carries the bar of
links to the parts (the reMarkable follows internal links); the bar
after the running head (fancyhdr clears it).  A buffer without parts
stays as it is."
  (with-temp-buffer
    (insert "#+LATEX_HEADER: \\usepackage{fancyhdr}\\pagestyle{fancy}\\fancyhf{}\n"
            "#+LATEX_HEADER: \\fancyhead[L]{X}\n"
            "* Reading\n:PROPERTIES:\n:CUSTOM_ID: reading\n:END:\nR.\n"
            "* Segments\n:PROPERTIES:\n:CUSTOM_ID: segments\n:END:\nS.\n"
            "** ⟦1⟧\n:PROPERTIES:\n:CUSTOM_ID: grammar-1\n:END:\n"
            "* Further Reading\n:PROPERTIES:\n:CUSTOM_ID: further-reading\n:END:\nF.\n")
    (bcat-export--parts-pages)
    (let ((text (buffer-string)))
      (should (string-match-p "^#\\+LATEX_HEADER: \\\\fancyhead\\[L\\]{X}\n#\\+LATEX_HEADER: \\\\fancyfoot\\[C\\]{" text))
      (should (string-match-p (regexp-quote "\\hyperref[reading]{Reading}") text))
      (should (string-match-p (regexp-quote "\\hyperref[further-reading]{Further Reading}") text))
      (should-not (string-match-p "hyperref\\[grammar-1\\]" text))
      (should-not (string-match-p "clearpage\n\\* Reading" text))
      (should (string-match-p "^#\\+LATEX: \\\\clearpage\n\\* Segments$" text))
      (should (string-match-p "^#\\+LATEX: \\\\clearpage\n\\* Further Reading$" text))
      (should-not (string-match-p "clearpage\n\\*\\* " text))))
  (with-temp-buffer
    (insert "* Verse\nV.\n* Commentary\nC.\n")
    (bcat-export--parts-pages)
    (should (equal "* Verse\nV.\n* Commentary\nC.\n" (buffer-string)))))

(ert-deftest bcat-export-page-format ()
  "Carsten 5.11.: reMarkable Paper Pro — the page is its screen (180 ×
240 mm, 3:4), shown at its size; narrow margins, 10 pt.  The format
goes into the export copy only."
  (let ((lines (bcat-export-page-lines)))
    (should (string-match-p "^#\\+LATEX_CLASS_OPTIONS: \\[10pt\\]$" lines))
    (should (string-match-p "\\\\geometry{paperwidth=180mm,paperheight=240mm," lines))))

(ert-deftest bcat-export-file-exports-the-view ()
  "C-c u H exports the view (one document with jump marks), not the
file: written beside the file under a name no folder scan takes for a
sentence file, gone after the export; the PDF into handouts/ under the
speaking name.  Without a view: the file itself, as before."
  (bcat-sentence-test--with-source
    (bcat-sentence-test--create src)
    (bcat-sentence-test--with-stubs bcat-sentence-test--answer
      (bcat-sentence-test--fetch src))
    (let* ((file (tibetan-sentence--filepath 9 folder src))
           (exported nil) (seen nil))
      (cl-letf (((symbol-function 'bcat-export-pdf)
                 (lambda (f &rest _)
                   (setq exported f
                         seen (with-temp-buffer (insert-file-contents f) (buffer-string)))
                   (let ((pdf (concat (file-name-sans-extension f) ".pdf")))
                     (with-temp-file pdf (insert "PDF")) pdf))))
        (let ((bcat-export-upload nil))
          (should (equal (expand-file-name "handouts/bden-9.pdf" dir) (bcat-export-file file)))
          (should-not (string-prefix-p "sent-" (file-name-nondirectory exported)))
          (should (equal folder (file-name-directory exported)))
          (should (string-match-p "^\\* Reading\n" seen))
          (should-not (file-exists-p exported))
          (should-not (directory-files folder nil "\\.pdf\\'"))
          ;; No view: the file itself.
          (cl-letf (((symbol-function 'bcat-export-views) #'ignore))
            (bcat-export-file file)
            (should (equal file exported))))))))

(defconst bcat-export-test--lecture
  (concat "[[https://www.youtube.com/watch?v=x&t=0s][0:00:00]] Opening words.\n\n"
          "[[https://www.youtube.com/watch?v=x&t=180s][0:03:00]] Benedictory verses.\n\n"
          "[[https://www.youtube.com/watch?v=x&t=1453s][0:24:13]] The heart is the seat of the emotions.\n\n"
          "[[https://www.youtube.com/watch?v=x&t=1527s][0:25:27]] So the heart of I, the core.\n\n"
          "[[https://www.youtube.com/watch?v=x&t=1650s][0:27:30]] Whoops!")
  "A lecture body as #+CAT_LECTURES gives it: paragraphs with time marks.")

(ert-deftest bcat-export-lecture-excerpt ()
  "Carsten 5.11.: in the reading PDF only an excerpt of the lecture, the
whole in a document of its own.  The excerpt: the paragraphs the
analysis cites by time mark — „(S, I 0:24:13)“; a mark inside a
paragraph takes that paragraph — in their order, each once; a mark of
another lecture (II) not.  Mechanical: the transcript goes to no one."
  (let* ((ctx (bcat-export--view-context
               `((comparison . "Sanderson (S, I 0:24:13) rejects heart; (S, I 0:26:00); (S, II 0:03:00).")
                 (segments . (((num . 1) (concepts . "*hṛdaya* (S, I 0:24:13)"))))
                 (lectures-p . t)
                 (lectures . (((lecture-name . "Sanderson, Readings in the Tantrāloka I")
                               (lecture-body . ,bcat-export-test--lecture)))))))
         (lecture (car (alist-get 'lectures ctx))))
    (should (equal (concat "[[https://www.youtube.com/watch?v=x&t=1453s][0:24:13]] The heart is the seat of the emotions.\n\n"
                           "[[https://www.youtube.com/watch?v=x&t=1527s][0:25:27]] So the heart of I, the core.")
                   (alist-get 'excerpt lecture)))
    (let ((view (bcat-template-render-named "pdf-part-further-reading" ctx)))
      (should (string-match-p "^\\*\\* Sanderson, Readings in the Tantrāloka I$" view))
      (should (string-match-p "0:24:13\\]\\] The heart" view))
      (should-not (string-match-p "Opening words\\|Whoops" view))
      (should (string-match-p "a document of its own" view)))
    (let ((doc (bcat-template-render-named "pdf-lectures" ctx)))
      (should (string-match-p "^\\* Sanderson, Readings in the Tantrāloka I$" doc))
      (should (string-match-p "Opening words" doc))
      (should (string-match-p "Whoops!" doc)))))

(ert-deftest bcat-export-file-lecture-document ()
  "C-c u H with a lecture: a second PDF beside the first in handouts/
\(<name>-lectures.pdf), onto the reMarkable after the first (rmapi one
at a time) — only when it is new: the transcript does not change, a
copy per export would crowd the device."
  (bcat-sentence-test--with-source
    (bcat-sentence-test--create src)
    (let* ((file (tibetan-sentence--filepath 9 folder src))
           (lectures (expand-file-name "handouts/bden-9-lectures.pdf" dir))
           (uploaded '()))
      (cl-letf (((symbol-function 'bcat-export-views)
                 (lambda (_f) (list :view "* Reading\nR.\n" :lectures "* Sanderson\nS.\n"
                                   :lecturer "Sanderson")))
                ((symbol-function 'bcat-export-pdf)
                 (lambda (f &rest _)
                   (let ((pdf (concat (file-name-sans-extension f) ".pdf")))
                     (with-temp-file pdf (insert "PDF")) pdf)))
                ((symbol-function 'tibetan-discussion-paper--upload)
                 (lambda (pdf _title label &optional done)
                   (push (cons (file-name-nondirectory pdf) label) uploaded)
                   (when done (funcall done t label)) t)))
        (let ((bcat-export-upload t))
          (bcat-export-file file)
          (should (file-exists-p lectures))
          (should (equal '(("bden-9.pdf" . "SDA 9") ("bden-9-lectures.pdf" . "SDA 9 Sanderson"))
                         (reverse uploaded)))
          (setq uploaded nil)
          (bcat-export-file file)
          (should (equal '(("bden-9.pdf" . "SDA 9")) uploaded))
          (should-not (directory-files folder nil "\\.pdf\\'\\|\\`\\.pdfview")))))))

(ert-deftest bcat-export-auto-parts ()
  "Carsten 5.11.: the bar of links „überall“ — in the documents of the
readings.  A document without parts of its own (Einführung,
Schlüsselbegriffe, the lectures) gets jump targets for its headings of
level 1 (nav-1 …, a drawer of its own or the one it has) and the bar
— but no new page per heading (short sections would leave half pages).
One heading, or ten and more: no bar (useless, or too long)."
  (with-temp-buffer
    (insert "#+LATEX_HEADER: \\fancyhead[L]{X}\n"
            "* Einleitung\nE.\n"
            "* Text und Kommentar\n:PROPERTIES:\n:FOLIO: 1\n:END:\nT.\n"
            "** Unterabschnitt\nU.\n"
            "* Literatur\nL.\n")
    (let ((bcat-export-navbar-auto t))
      (bcat-export--auto-parts)
      (bcat-export--parts-pages))
    (let ((text (buffer-string)))
      (should (string-match-p "^\\* Einleitung\n:PROPERTIES:\n:CUSTOM_ID: nav-1\n:END:\nE\\.$" text))
      (should (string-match-p "^\\* Text und Kommentar\n:PROPERTIES:\n:CUSTOM_ID: nav-2\n:FOLIO: 1\n:END:$" text))
      (should (string-match-p (regexp-quote "\\hyperref[nav-3]{Literatur}") text))
      (should-not (string-match-p "clearpage" text))
      (should-not (string-match-p "CUSTOM_ID: nav-[0-9]+\n:END:\nU" text))))
  (with-temp-buffer
    (insert "* Nur einer\nE.\n")
    (let ((bcat-export-navbar-auto t))
      (bcat-export--auto-parts)
      (bcat-export--parts-pages))
    (should-not (string-match-p "fancyfoot" (buffer-string))))
  (with-temp-buffer
    (insert "* A\n* B\n")
    (bcat-export--auto-parts)
    (should (equal "* A\n* B\n" (buffer-string)))))

(ert-deftest bcat-export-reading-document ()
  "C-c u H in a document of the readings that is no cascade source and
no sentence file (Einführung-MSA-9.org): its own page — Carsten 5.11.:
„in Handouts A4 ist sehr fein“, no Paper Pro page here —, the bar of
its headings, into handouts/ beside the text, onto the reMarkable under
its title; the file untouched."
  (let* ((dir (make-temp-file "bcat-reading-" t))
         (text-dir (expand-file-name "WS26-27/Mahayanasutralamkara" dir))
         (file (expand-file-name "Einfuehrung-MSA-9.org" text-dir))
         (exported nil) (uploaded nil) (page 'unset))
    (unwind-protect
        (progn
          (make-directory text-dir t)
          (with-temp-file file
            (insert "#+TITLE: Mahāyānasūtrālaṃkāra IX — Einführung\n"
                    "#+LATEX_CLASS_OPTIONS: [11pt,a4paper]\n"
                    "#+LATEX_HEADER: \\usepackage[a4paper,left=2.5cm]{geometry}\n"
                    "#+LATEX_HEADER: \\usepackage{libertinus}\n\n"
                    "* Der Text\nT.\n* Die Klasse\nK.\n"))
          (let ((before (with-temp-buffer (insert-file-contents file) (buffer-string))))
            (cl-letf (((symbol-function 'bcat-export-pdf)
                       (lambda (f &rest _)
                         (setq exported (with-temp-buffer (insert-file-contents f) (buffer-string))
                               page bcat-export-page)
                         (let ((pdf (concat (file-name-sans-extension f) ".pdf")))
                           (with-temp-file pdf (insert "PDF")) pdf)))
                      ((symbol-function 'tibetan-discussion-paper--upload)
                       (lambda (pdf _title label &optional _done) (setq uploaded (cons pdf label)) t)))
              (let ((bcat-export-upload t))
                (with-current-buffer (find-file-noselect file)
                  (unwind-protect (bcat-export) (kill-buffer)))
                (should-not page)))
            (should (equal before (with-temp-buffer (insert-file-contents file) (buffer-string)))))
          (should (string-match-p "a4paper" exported))
          (should (string-match-p "libertinus" exported))
          (should (string-match-p "^#\\+CAT_NAVBAR: t$" exported))
          (should (equal (expand-file-name "handouts/Einfuehrung-MSA-9.pdf" text-dir) (car uploaded)))
          (should (equal "Mahāyānasūtrālaṃkāra IX — Einführung" (cdr uploaded))))
      (delete-directory dir t))))

(ert-deftest bcat-export-pdf-smoke ()
  "Real LuaLaTeX run (only where lualatex is installed): the reviewed
Tibetan sample becomes a PDF and the .tex is cleared away; kept on
request, the .tex has no title, no centred and no tabularx tables, and
carries the running head."
  (skip-unless (executable-find "lualatex"))
  (let* ((dir (make-temp-file "bcat-export-" t))
         (file (expand-file-name "sent-009-bden.org" dir)))
    (unwind-protect
        (progn
          (copy-file (expand-file-name "../Templates/analysis/samples/Tibetan.org"
                                       (file-name-directory (locate-library "bcat-export-test")))
                     file)
          ;; Carsten 30.10.: „das .tex können wir … wegräumen, wenn das
          ;; PDF erzeugt ist“ — kept only on request (for these checks).
          (let ((pdf (bcat-export-pdf file (bcat-export-head-lines "Tibetisch V" "bDen" 9 "SDA 9"))))
            (should (file-exists-p pdf))
            (should-not (file-exists-p (expand-file-name "sent-009-bden.tex" dir))))
          (let ((bcat-export-keep-tex t))
            (bcat-export-pdf file (bcat-export-head-lines "Tibetisch V" "bDen" 9 "SDA 9")))
          (let ((tex (with-temp-buffer
                       (insert-file-contents (expand-file-name "sent-009-bden.tex" dir))
                       (buffer-string))))
            (should-not (string-match-p "\\\\maketitle" tex))
            (should-not (string-match-p "\\\\begin{center}" tex))
            (should-not (string-match-p "\\\\begin{tabularx}" tex))
            (should (string-match-p "Satz 9 · SDA 9" tex))
            ;; 31.10. abends: the tables under * Text — words and gloss
            ;; black, the function row blue.
            (should (string-match-p (regexp-quote "\\bcatg{ADJ · predicate}") tex))
            (should-not (string-match-p (regexp-quote "\\bcatg{indescribable}") tex))
            (should (string-match-p (regexp-quote "\\newfontfamily\\bcattib{Noto Serif Tibetan}") tex))
            (should (string-match-p (regexp-quote "{\\bcattib\\large བརྗོད་དུ་མེད་པ་བལྟར་མེད་པ། །}") tex))))
      (let ((buf (get-file-buffer file))) (when buf (kill-buffer buf)))
      (delete-directory dir t))))

(provide 'bcat-export-test)
;;; bcat-export-test.el ends here
