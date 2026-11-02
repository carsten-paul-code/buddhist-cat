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
                                          (<= (string-width l) bcat-analysis-sanskrit-table-width)))
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
        (should (equal (list (tibetan-sentence--filepath 9 folder src)) exported))
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
