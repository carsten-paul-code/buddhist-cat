;;; bcat-template-test.el --- Tests for the analysis-file templates  -*- lexical-binding: t; -*-

;;; Commentary:
;; §5.74 (Carsten 28.10.2026): sentence files are built from templates
;; ("Template basierend arbeiten, gerne kaskadierend") — an org skeleton
;; with {{placeholders}}, looked up work folder → its Resources → the
;; tool default.  The golden test renders the tool's Tibetan skeleton
;; with the data of bDen sentence 9 and must reproduce the reviewed
;; sample Templates/analysis/samples/Tibetan.org character for
;; character.  Temp dirs only; no network.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((root (expand-file-name ".." (file-name-directory
                                    (or load-file-name buffer-file-name)))))
  (add-to-list 'load-path (expand-file-name "persist" root))
  (add-to-list 'load-path (expand-file-name "test/fixtures" root)))

(require 'bcat-template)
(require 'bcat-template-sent-009)
(require 'bcat-template-sent-1007)

(defconst bcat-template-test--root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name)))
  "Repository root, captured at load time.")

;; ----------------------------------------------------------------------------
;; Placeholders
;; ----------------------------------------------------------------------------

(ert-deftest bcat-template-placeholders-and-empty-lines ()
  "{{name}} is replaced (also multi-line, also numbers); a line whose
placeholders are ALL empty disappears with its literal label; the blank
line it leaves behind is not doubled; a line with one filled and one
empty placeholder stays."
  (should (equal "Satz 9\n"
                 (bcat-template-render "Satz {{n}}\n" '((n . 9)))))
  (should (equal "A\nzwei\nZeilen\nB\n"
                 (bcat-template-render "A\n{{x}}\nB\n" '((x . "zwei\nZeilen")))))
  (should (equal "*S:* s\n\n*C:* c\n"
                 (bcat-template-render "*S:* {{s}}\n\n*N:* {{note}}\n\n*C:* {{c}}\n"
                                       '((s . "s") (note . "") (c . "c")))))
  ;; nil counts as empty, too; a trailing empty field leaves no blank tail.
  (should (equal "*S:* s\n"
                 (bcat-template-render "*S:* {{s}}\n\n*N:* {{note}}\n"
                                       '((s . "s") (note . nil)))))
  ;; Heading + empty body: the heading keeps one blank line below it.
  (should (equal "* WT\n\n* Notes\n"
                 (bcat-template-render "* WT\n{{wt}}\n\n* Notes\n{{n}}\n"
                                       '((wt . "") (n . "")))))
  ;; All placeholders empty → the whole line goes, literal text included.
  (should (equal ""
                 (bcat-template-render "x {{a}} y\n" '((a . "")))))
  (should (equal "a-b\n"
                 (bcat-template-render "{{a}}-{{b}}\n" '((a . "a") (b . "b"))))))

;; ----------------------------------------------------------------------------
;; Sections
;; ----------------------------------------------------------------------------

(ert-deftest bcat-template-sections ()
  "{{#list}} repeats its block per item (item fields first, then the
outer context); {{#x}} with a plain value renders once, {{^x}} only
when x is empty; an empty list renders nothing.  Section tags stand
alone on their lines and leave no trace."
  (let ((tpl (concat "{{title}}\n"
                     "{{#segments}}\n"
                     "** Segment {{num}} ({{title}})\n"
                     "{{/segments}}\n"
                     "{{#flag}}\n"
                     "flag set\n"
                     "{{/flag}}\n"
                     "{{^flag}}\n"
                     "no flag\n"
                     "{{/flag}}\n")))
    (should (equal "T\n** Segment 1 (T)\n** Segment 2 (T)\nflag set\n"
                   (bcat-template-render
                    tpl '((title . "T") (flag . t)
                          (segments ((num . 1)) ((num . 2)))))))
    (should (equal "T\nno flag\n"
                   (bcat-template-render
                    tpl '((title . "T") (flag . nil) (segments)))))
    ;; Nested sections of different names.
    (should (equal "1:\n- a\n- b\n2:\n"
                   (bcat-template-render
                    "{{#segs}}\n{{n}}:\n{{#refs}}\n- {{r}}\n{{/refs}}\n{{/segs}}\n"
                    '((segs ((n . 1) (refs ((r . "a")) ((r . "b"))))
                            ((n . 2) (refs)))))))
    ;; A section tag must stand alone on its line.
    (should-error (bcat-template-render "a {{#x}}\nb\n{{/x}}\n" '((x . t)))
                  :type 'user-error)
    (should-error (bcat-template-render "{{#x}}\nb\n" '((x . t)))
                  :type 'user-error)))

;; ----------------------------------------------------------------------------
;; Comment blocks, partials, cascade
;; ----------------------------------------------------------------------------

(defmacro bcat-template-test--with-dirs (&rest body)
  "Temp tool template dir TOOL (bound as `bcat-template-directory') and a
temp work folder WORK with source file SRC.  Helper (put PATH TEXT)."
  (declare (indent 0))
  `(let* ((tool (make-temp-file "bcat-tpl-tool-" t))
          (work (make-temp-file "bcat-tpl-work-" t))
          (src (expand-file-name "werk-cat.org" work))
          (bcat-template-directory tool))
     (cl-flet ((put (path text)
                 (make-directory (file-name-directory path) t)
                 (with-temp-file path (insert text))))
       (unwind-protect
           (progn (put src "* Text\n") ,@body)
         (delete-directory tool t)
         (delete-directory work t)))))

(ert-deftest bcat-template-skipped-section-blank-lines ()
  "A section that does not render counts like a dropped line: the blank
lines around it become one (29.10.: an empty segment of the Sanskrit
template left two blank lines where *Forms:* was missing).  Positive
and inverted sections alike."
  (should (equal "a\n\nb\n"
                 (bcat-template-render "a\n\n{{#x}}\nX\n{{/x}}\n\nb\n" '((x)))))
  (should (equal "a\n\nb\n"
                 (bcat-template-render "a\n\n{{^x}}\nX\n{{/x}}\n\nb\n" '((x . "1")))))
  (should (equal "a\n\nX\n\nb\n"
                 (bcat-template-render "a\n\n{{#x}}\nX\n{{/x}}\n\nb\n" '((x . "1"))))))

(ert-deftest bcat-template-comment-blocks-and-partials ()
  "#+begin_comment blocks document the template and never reach the
output; {{> name}} on its own line inserts template NAME (found like
the main one)."
  (bcat-template-test--with-dirs
    (put (expand-file-name "main.org" tool)
         "#+begin_comment\nErklärung\n#+end_comment\nKopf {{a}}\n{{> teil}}\nEnde\n")
    (put (expand-file-name "teil.org" tool)
         "#+begin_comment\nTeil-Erklärung\n#+end_comment\nTeil {{a}}\n")
    (should (equal "Kopf 1\nTeil 1\nEnde\n"
                   (bcat-template-render-named "main" '((a . 1)) src)))
    (should-error (bcat-template-read "fehlt" src) :type 'user-error)))

(ert-deftest bcat-template-cascade ()
  "Lookup order: <source folder>/Templates/analysis/ → <source folder>/
Resources/Templates/analysis/ → the tool default.  Partials follow the
same cascade: a class can override only one part of a tool template."
  (bcat-template-test--with-dirs
    (put (expand-file-name "t.org" tool) "Werkzeug\n{{> teil}}\n")
    (put (expand-file-name "teil.org" tool) "Teil Werkzeug\n")
    (should (equal "Werkzeug\nTeil Werkzeug\n" (bcat-template-render-named "t" nil src)))
    (should (equal (expand-file-name "t.org" tool) (bcat-template-find "t" src)))
    ;; Resources overrides the tool …
    (put (expand-file-name "Resources/Templates/analysis/teil.org" work) "Teil Resources\n")
    (should (equal "Werkzeug\nTeil Resources\n" (bcat-template-render-named "t" nil src)))
    ;; … and Templates/analysis in the work folder overrides both.
    (put (expand-file-name "Templates/analysis/teil.org" work) "Teil Werk\n")
    (put (expand-file-name "Templates/analysis/t.org" work) "Werk\n{{> teil}}\n")
    (should (equal "Werk\nTeil Werk\n" (bcat-template-render-named "t" nil src)))
    ;; Without a source file only the tool default counts.
    (should (equal "Werkzeug\nTeil Werkzeug\n" (bcat-template-render-named "t" nil)))))

;; ----------------------------------------------------------------------------
;; Golden test: the reviewed Tibetan sample
;; ----------------------------------------------------------------------------

(ert-deftest bcat-template-tibetan-golden ()
  "The tool's Tibetan skeleton (Templates/analysis/tibetan.org) filled
with bDen sentence 9 reproduces the sample Carsten reviewed
\(Templates/analysis/samples/Tibetan.org) character for character —
including the dropped Note of segment 39 and Concepts of segment 40."
  (let ((bcat-template-directory
         (expand-file-name "Templates/analysis" bcat-template-test--root))
        (expected (with-temp-buffer
                    (insert-file-contents
                     (expand-file-name "Templates/analysis/samples/Tibetan.org"
                                       bcat-template-test--root))
                    (buffer-string))))
    (should (equal expected
                   (bcat-template-render-named "tibetan" bcat-template-test--sent-009)))))

(ert-deftest bcat-template-sanskrit-golden ()
  "The tool's Sanskrit skeleton (Templates/analysis/sanskrit.org) filled
with TĀ sentence 1007 reproduces the sample Carsten reviewed
\(Templates/analysis/samples/Sanskrit.org, 29.10.2026) character for
character: Recitation first, Text with the saṃhitā and the glossed
padapāṭha, per segment the tables, translations, Sandhi, Forms,
Compounds — the Note of segment 686 dropped."
  (let ((bcat-template-directory
         (expand-file-name "Templates/analysis" bcat-template-test--root))
        (expected (with-temp-buffer
                    (insert-file-contents
                     (expand-file-name "Templates/analysis/samples/Sanskrit.org"
                                       bcat-template-test--root))
                    (buffer-string))))
    (should (equal expected
                   (bcat-template-render-named "sanskrit" bcat-template-test--sent-1007)))))

(ert-deftest bcat-template-sanskrit-prose ()
  "Viveka prose (no metre): the Recitation holds only the Devanāgarī
original (no metre line, no scansion); without a padapāṭha (the rump)
the Text shows the line as transmitted, no glossed verse, no
translation line."
  (let* ((bcat-template-directory
          (expand-file-name "Templates/analysis" bcat-template-test--root))
         (out (bcat-template-render-named
               "sanskrit"
               '((sent-num . "1008") (segment-list . "687")
                 (recitation . "इह खलु शास्त्रादौ …")
                 (segments ((num . "687") (padapatha . "iha khalu śāstrādau …")))))))
    (should (string-match-p
             (regexp-quote (concat "#+SEGMENTS: 687\n\n* Recitation\n#+begin_verse\nइह खलु शास्त्रादौ …\n"
                                   "#+end_verse\n\n* Text\n#+begin_verse\niha khalu śāstrādau …\n"
                                   "#+end_verse\n\n* Segments\n** Segment 687\n\n* Sentence\n"))
             out))
    (should-not (string-match-p "Translation:" out))))

(provide 'bcat-template-test)
;;; bcat-template-test.el ends here
