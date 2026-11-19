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
(require 'tibetan-sanskrit-reading nil t)
(require 'tibetan-sentence-persist nil t)
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

(defun bcat-template-test--with-reading (context)
  "CONTEXT (a fixture: the fields as an analysis gives them) with what
closes * Text since 18.11. — the translation of the sentence — and the
decisions (`bcat-analysis-reading-fields'), as every context of a
sentence file has them."
  (require 'bcat-analysis)
  (append (bcat-analysis-reading-fields (alist-get 'proposal context) (alist-get 'translation context))
          context))

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
                   (bcat-template-render-named
                    "tibetan" (bcat-template-test--with-reading bcat-template-test--sent-009))))))

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
                   (bcat-template-render-named
                    "sanskrit" (bcat-template-test--with-reading bcat-template-test--sent-1007))))))

(ert-deftest bcat-template-translation-under-the-overview ()
  "Carsten 5.11.: „Übersetzung unter die Übersicht“ — per Sanskrit
segment the overview, „→“ its translation, then the parts tables; in
the Sanskrit file, the bilingual one and the Sanskrit part of the
Tibetan file of a pair.  9.11. (TĀ 1009: „Zumindest bräuchte es noch
eine subsection headline. Und hatten wir nicht mal Original und
Übersetzung hervorgehoben?“): the segment a heading ⟦N⟧, under it the
line of the original in IAST (the padapāṭha) in bold, the parts under
their own heading Word Formation; the sentence's translation under a
heading of its own — else it would sit in the last Word Formation.
18.11. (Carsten: „erstmal standardmäßig nicht eingeklappt“): the parts
stand open; with the field `formation-folded' (the option
`bcat-sentence-word-formation-folded') the heading gets its drawer
:VISIBILITY: folded again."
  (let* ((bcat-template-directory
          (expand-file-name "Templates/analysis" bcat-template-test--root))
         (seg '((num . "1") (shown . t) (mark . "") (gloss-table . "| a |") (padapatha . "p q //")
                (segment-translation . "t") (formation-tables . "| b |\n|---|\n| c |")))
         (block (concat "⟦1⟧\n*p q //*\n\n| a |\n\n→ t\n\n*** Word Formation\n"
                        "| b |\n|---|\n| c |\n\n"))
         (folded (concat "⟦1⟧\n*p q //*\n\n| a |\n\n→ t\n\n*** Word Formation\n:PROPERTIES:\n"
                         ":VISIBILITY: folded\n:END:\n| b |\n|---|\n| c |\n\n"))
         ;; 18.11.: in the Tibetan file of a pair the Sanskrit segments
         ;; stand under ** Sanskrit — every heading one level down
         (down (lambda (s) (replace-regexp-in-string (regexp-quote "\n*** Word Formation") "\n**** Word Formation" s t t)))
         (sanskrit (bcat-template-render-named
                    "sanskrit" (bcat-template-test--with-reading `((segments ,seg) (translation . "T")))))
         (bilingual (bcat-template-render-named
                     "bilingual" (bcat-template-test--with-reading `((segments ,seg) (translation . "T")))))
         (partner (bcat-template-render-named "bilingual-tibetan"
                                              `((sanskrit-p . t) (sanskrit-segments ,seg)
                                                (sanskrit-translation . "T")))))
    (should (string-match-p (concat "^\\*\\* " (regexp-quote block)) sanskrit))
    (should (string-match-p (concat "^\\*\\* " (regexp-quote folded))
                            (bcat-template-render-named
                             "sanskrit" `((formation-folded . "t") (segments ,seg) (translation . "T")))))
    (should (string-match-p (concat "^\\*\\* <<skt-1>>" (regexp-quote folded))
                            (bcat-template-render-named
                             "bilingual" `((formation-folded . "t") (segments ,seg) (translation . "T")))))
    (should (string-match-p (concat "^\\*\\* Sanskrit\n\\*\\*\\* <<skt-1>>" (regexp-quote (funcall down folded)))
                            (bcat-template-render-named
                             "bilingual-tibetan" `((formation-folded . "t") (sanskrit-p . t)
                                                   (sanskrit-segments ,seg) (sanskrit-translation . "T")))))
    (should (string-match-p (concat "^\\*\\* <<skt-1>>" (regexp-quote block)) bilingual))
    (should (string-match-p (concat "^\\*\\* Sanskrit\n\\*\\*\\* <<skt-1>>" (regexp-quote (funcall down block))) partner))
    ;; 18.11.: the translation of the sentence closes * Text; the heading
    ;; Translation after the segments holds what belongs to it
    (dolist (out (list sanskrit bilingual))
      (should (string-match-p "^\\*Translation:\\* T\n\n\\* Philology$" out))
      (should (string-match-p "^→ t\n\n\\(?:\\*\\*\\* Word Formation\n\\(?:.*\n\\)*?\n\\)?\\*\\* Translation\n\\*\\* Concepts$" out)))
    (should (string-match-p "^\\*\\*\\* Translation\n\\*Translation:\\* T$" partner))))

(ert-deftest bcat-template-proposal-first ()
  "Carsten 9.11. (TĀ 1009: „ich vermisse jetzt die proposed
translation“ — it stood after the Grammar, behind the folded Word
Formation): the proposal before the Grammar (the order of reading,
31.10.: translation and German first).  9.11. to 18.11. under a heading
** Proposed translation after the segments; since 18.11. the proposal
closes * Text and what follows the segments is its decisions, the
second language, the Grammar — the Tibetan file too, without a heading
\(its segments are none)."
  (let* ((bcat-template-directory
          (expand-file-name "Templates/analysis" bcat-template-test--root))
         (seg '((num . "1") (shown . t) (mark . "") (gloss-table . "| a |") (padapatha . "p")
                (segment-translation . "t")))
         (proposal "*Proposed translation:*\n#+begin_quote\nP\n#+end_quote\n\n*Decisions:* D")
         (with (bcat-template-test--with-reading
                `((segments ,seg) (translation . "T") (grammar . "G") (proposal . ,proposal)
                  (second-label . "Deutsch") (second-translation . "Ü"))))
         (without (bcat-template-test--with-reading `((segments ,seg) (translation . "T") (grammar . "G")))))
    ;; 18.11.: the proposal itself closes * Text
    ;; (bcat-template-translation-closes-the-text); after the segments
    ;; its decisions, then the second language, then the Grammar — the
    ;; order of 9.11. —, in a Sanskrit file under ** Translation; no
    ;; heading Proposed translation any more.
    (dolist (template '("sanskrit" "bilingual"))
      (let ((out (bcat-template-render-named template with)))
        (should (string-match-p
                 (regexp-quote "\n** Translation\n*Decisions:* D\n\n*Deutsch:* Ü\n\n*Grammar:* G\n\n** Concepts\n")
                 out))
        (should (string-match-p (regexp-quote "\n#+begin_quote\nP\n#+end_quote\n\n* Philology\n") out))
        (should-not (string-match-p "^\\*\\* Proposed translation$" out)))
      (should (string-match-p (regexp-quote "\n** Translation\n*Grammar:* G\n\n** Concepts\n")
                              (bcat-template-render-named template without))))
    (let ((out (bcat-template-render-named "tibetan" with)))
      (should (string-match-p (regexp-quote "→ t\n\n*Decisions:* D\n\n*Deutsch:* Ü\n\n*Grammar:* G\n\n** Concepts\n")
                              out))
      (should-not (string-match-p "^\\*\\* \\(?:Proposed translation\\|Translation\\)$" out)))))

(defun bcat-template-test--sample (name)
  "The text of the sample NAME under Templates/analysis/samples/."
  (with-temp-buffer
    (insert-file-contents (expand-file-name (concat "Templates/analysis/samples/" name)
                                            bcat-template-test--root))
    (buffer-string)))

(ert-deftest bcat-template-reading-structure ()
  "9.11. (TĀ 1009) as org reads it, in the reviewed samples: where the
Sanskrit segments stand each is a heading ⟦N⟧ with the original in bold
right under it; its „→“ translation belongs to that heading; the
translation of the sentence to a heading Translation.  18.11.: the
parts of the words stand under them in the table of the segment, below
a rule — no heading Word Formation in the samples any more (9.11. to
18.11. the parts had one).  18.11., three parts: the segments stand
under * Philology (until then * Text), in the Tibetan file of a pair
one level down under ** Sanskrit (until then * Sanskrit)."
  (require 'org)
  ;; 18.11.: under the heading Translation the Grammar of the sentence
  ;; (its translation closes * Text); in the Sanskrit part of the Tibetan
  ;; file of a pair the Sanskrit translation, as before
  (dolist (case '(("Sanskrit.org" "^\\* Philology$" "^\\*\\* Concepts$" 2 ("Philology" "Translation")
                   "^\\*Grammar:\\* ")
                  ("Bilingual.org" "^\\* Philology$" "^\\*\\* Tibetan$" 2 ("Philology" "Translation")
                   "^\\*Grammar:\\* ")
                  ("Bilingual-Tibetan.org" "^\\*\\* Sanskrit$" "^\\*\\* Concepts$" 3
                   ("Philology" "Sanskrit" "Translation") "^\\*Translation:\\* ")))
    (with-temp-buffer
      (insert (bcat-template-test--sample (car case)))
      (let ((org-mode-hook nil)) (org-mode))
      (goto-char (point-min))
      (should (re-search-forward (nth 1 case) nil t))
      (let ((start (point))
            (end (save-excursion (re-search-forward (nth 2 case)) (match-beginning 0)))
            (depth (nth 3 case))
            (segments 0) (arrows 0) (formations 0))
        (while (re-search-forward (format "^\\*\\{%d\\} .*⟦[0-9]+⟧" depth) end t)
          (cl-incf segments)
          (should (save-excursion (forward-line 1) (looking-at "\\*[^* \n][^\n]*[^ \n]\\*$"))))
        (goto-char start)
        (while (re-search-forward "^→ " end t)
          (cl-incf arrows)
          (let ((path (org-get-outline-path t)))
            (should (= depth (length path)))
            (should (string-match-p "⟦[0-9]+⟧" (car (last path))))))
        (goto-char start)
        (should-not (re-search-forward "^\\*+ Word Formation$" end t))
        ;; 18.11.: the parts under the words — a table with a rule,
        ;; under the heading of its segment
        (goto-char start)
        (while (re-search-forward "^|-" end t)
          (cl-incf formations)
          (should (string-match-p "⟦[0-9]+⟧" (car (last (org-get-outline-path t))))))
        (goto-char start)
        (should (re-search-forward (nth 5 case) end t))
        (should (equal (nth 4 case) (org-get-outline-path t)))
        (should (> segments 0))
        (should (= segments arrows))
        (should (> formations 0))))))

(defun bcat-template-test--outline (text)
  "The headings of TEXT as strings, stars included."
  (let ((pos 0) (out '()))
    (while (string-match "^\\*+ .*$" text pos)
      (push (match-string 0 text) out)
      (setq pos (match-end 0)))
    (nreverse out)))

(ert-deftest bcat-template-three-parts ()
  "18.11. (Carsten: „Wir sollten umstrukturieren und das für alle
Templates. Als erstes der Content (Originaltext in Script und Wylie
bzw. IAST) und optional die Rezitationstabelle, wenn es ein Vers zum
Rezitieren ist. Als zweites dann die Philologie. Das sind dann auch die
beiden Teile die ich auf dem Macbook in den Readings nutzen werde. Und
schließlich als Drittes ‚Worum es geht‘“ — „drei Oberabschnitte
passen“, as * Text, * Philology, * Worum es geht).  Every sentence file
has these three at the top level, in this order, then Further Reading
and the user's slots.  * Text is the original alone: script over
transliteration, for both languages of a pair, the table of the metre
only where the answer has one.  * Philology takes what stood under
* Text (the segments), * Sentence and * Segments, the headings below as
they were — only the other language of a pair and the commentary go one
level down under a heading of their own."
  (let* ((bcat-template-directory
          (expand-file-name "Templates/analysis" bcat-template-test--root))
         (l1 (lambda (text)
               (cl-remove-if-not (lambda (h) (string-match-p "\\`\\* " h))
                                 (bcat-template-test--outline text))))
         (tail '("* Worum es geht" "* Further Reading" "* Working Translation" "* My Notes" "* Footnotes"))
         (common '((overview . "*Erklärt wird:* x") (translation . "T") (comparison . "*Translation:* T")
                   (lectures-p . t) (lectures ((lecture-name . "L") (lecture-body . "b")))))
         (sa-seg '((num . "1") (shown . t) (mark . "") (devanagari . "अ") (samhita . "a b")
                   (padapatha . "a b") (gloss-table . "| a |\n| g |\n| f |") (segment-translation . "t")
                   (concepts . "c") (translations . "⟦1⟧ *Translation:* t")))
         (bo-seg '((num . "7") (shown . t) (mark . "") (uchen . "ཆོས།") (wylie . "chos/")
                   (gloss-table . "| ཆོས |\n| chos |\n| dharma |\n| N |") (segment-translation . "t")
                   (concepts . "c") (translations . "⟦7⟧ *Translation:* t"))))
    ;; Sanskrit: the Recitation is * Text, with its table when a verse
    (let ((out (bcat-template-render-named
                "sanskrit" `((recitation . "अ\na b") (scansion . "| a |\n| – |")
                             (segments ,sa-seg) (word-analysis . "*** Segment 1\na b") ,@common))))
      (should (equal `("* Text" "* Philology" ,@tail) (funcall l1 out)))
      (should (string-match-p (regexp-quote "* Text\n#+begin_verse\nअ\na b\n#+end_verse\n\n| a |\n| – |\n\n* Philology\n** ⟦1⟧\n*a b*\n")
                              out))
      (should (equal '("* Text" "* Philology" "** ⟦1⟧" "** Translation" "** Concepts" "** Translations compared"
                       "** Segment 1" "** Word Analysis" "*** Segment 1" "* Worum es geht" "* Further Reading" "** L"
                       "* Working Translation" "* My Notes" "* Footnotes")
                     (bcat-template-test--outline out)))
      ;; prose: no table
      (should (string-match-p (regexp-quote "#+end_verse\n\n* Philology\n")
                              (bcat-template-render-named
                               "sanskrit" `((recitation . "अ\na b") (segments ,sa-seg) ,@common)))))
    ;; Tibetan: Uchen over Wylie for every segment — also once it has its table
    (let ((out (bcat-template-render-named "tibetan" `((segments ,bo-seg) ,@common))))
      (should (equal `("* Text" "* Philology" ,@tail) (funcall l1 out)))
      (should (string-match-p (regexp-quote "* Text\n#+begin_verse\nཆོས།\nchos/\n#+end_verse\n\n* Philology\n⟦7⟧\n| ཆོས |")
                              out))
      (should (equal '("* Text" "* Philology" "** Concepts" "** Translations compared" "** Segment 7")
                     (seq-take (bcat-template-test--outline out) 5))))
    ;; Sanskrit with the Tibetan: both originals in * Text; the Tibetan
    ;; segments and the commentary one level down in * Philology
    (let ((out (bcat-template-render-named
                "bilingual"
                `((segments ((parallel . " · Tib. 7") (commentary . "k") ,@sa-seg))
                  (tibetan-p . t) (tibetan-lines ((line . "ཆོས།") (wylie . "chos/")))
                  (tibetan-segments ,bo-seg) (tibetan-translation . "TT") (rendering . "r")
                  (commentary-p . t) (bhasya-p . t) (bhasya-translation . "B") ,@common))))
      (should (equal `("* Text" "* Philology" ,@tail) (funcall l1 out)))
      (should (string-match-p (regexp-quote (concat "* Text\n#+begin_verse\nअ\na b\n#+end_verse\n\n"
                                                    "#+begin_verse\nཆོས།\nchos/\n#+end_verse\n\n* Philology\n"
                                                    "** <<skt-1>>⟦1⟧\n"))
                              out))
      (should (equal '("* Text" "* Philology" "** <<skt-1>>⟦1⟧" "** Translation" "** Tibetan" "** Concepts"
                       "** Tibetan rendering" "** Translations compared" "** Segment 1 · Tib. 7"
                       "** Commentary" "*** Bhāṣya" "* Worum es geht")
                     (seq-take (bcat-template-test--outline out) 12)))
      (should (string-match-p (regexp-quote "** Tibetan\n<<tib-7>>⟦7⟧\n| ཆོས |") out)))
    ;; the Tibetan file of the pair: mirrored, the Sanskrit segments
    ;; headings one level down
    (let ((out (bcat-template-render-named
                "bilingual-tibetan"
                `((segments ((parallel . " · Skt. 1") (commentary . "k") ,@bo-seg))
                  (sanskrit-p . t) (sanskrit ((devanagari . "अ") (samhita . "a b")))
                  (sanskrit-segments ((formation-tables . "| x |\n|---|\n| y |") ,@sa-seg))
                  (sanskrit-translation . "ST")
                  (commentary-p . t) (bhasya-p . t) (bhasya-translation . "B") ,@common))))
      (should (equal `("* Text" "* Philology" ,@tail) (funcall l1 out)))
      (should (string-match-p (regexp-quote (concat "* Text\n#+begin_verse\nཆོས།\nchos/\n#+end_verse\n\n"
                                                    "#+begin_verse\nअ\na b\n#+end_verse\n\n* Philology\n"
                                                    "<<tib-7>>⟦7⟧\n"))
                              out))
      (should (equal '("* Text" "* Philology" "** Sanskrit" "*** <<skt-1>>⟦1⟧" "**** Word Formation"
                       "*** Translation" "** Concepts" "** Translations compared" "** Segment 7 · Skt. 1"
                       "** Commentary" "*** Bhāṣya" "* Worum es geht")
                     (seq-take (bcat-template-test--outline out) 12))))
    ;; without an overview: no third part
    (should (equal '("* Text" "* Philology" "* Working Translation" "* My Notes" "* Footnotes")
                   (funcall l1 (bcat-template-render-named "tibetan" `((segments ,bo-seg))))))))

(ert-deftest bcat-template-translation-closes-the-text ()
  "18.11. (Carsten, before the philology part: „die proposed Translation
sollte als letzter Teil im Text Abschnitt kommen“): in every sentence
file * Text ends with the translation of the sentence — the proposed
translation(s), else the own one (`reading-translation',
`bcat-analysis-reading-fields') —, after the original and the table of
the metre.  What justifies it stays in * Philology after the segments:
the decisions, the translation in the second language, the grammar — in
a Sanskrit file under ** Translation (its segments are headings), in a
Tibetan one right after the last segment.  A sentence not analysed has
neither."
  (let* ((bcat-template-directory
          (expand-file-name "Templates/analysis" bcat-template-test--root))
         (quotes "*Proposed translation:*\n#+begin_quote\nP\n#+end_quote")
         (with `((reading-translation . ,quotes) (decisions . "*Decisions:* D")
                 (proposal . ,(concat quotes "\n\n*Decisions:* D"))
                 (translation . "T") (grammar . "G") (second-label . "Deutsch") (second-translation . "Ü")))
         (without '((reading-translation . "*Translation:* T") (decisions . "")
                    (translation . "T") (grammar . "G")))
         (sa-seg '((num . "1") (shown . t) (mark . "") (devanagari . "अ") (samhita . "a b")
                   (padapatha . "a b") (gloss-table . "| a |") (segment-translation . "t")))
         (bo-seg '((num . "7") (shown . t) (mark . "") (uchen . "ཆོས།") (wylie . "chos/")
                   (gloss-table . "| ཆོས |") (segment-translation . "t")))
         (render (lambda (template fields extra)
                   (bcat-template-render-named
                    template (append fields extra
                                     (if (member template '("tibetan" "bilingual-tibetan"))
                                         `((segments ,bo-seg))
                                       `((segments ,sa-seg) (recitation . "अ\na b")))))))
         (text-part (lambda (out)
                      (substring out (string-match "^\\* Text$" out) (string-match "^\\* Philology$" out))))
         (philology (lambda (out)
                      (substring out (string-match "^\\* Philology$" out) (string-match "^\\*\\* Concepts$" out)))))
    (dolist (template '("sanskrit" "tibetan" "bilingual" "bilingual-tibetan"))
      (ert-info ((format "Vorlage %s" template))
        (let ((out (funcall render template with nil)))
          ;; the last of * Text
          (should (string-suffix-p (concat "\n#+end_verse\n\n" quotes "\n\n") (funcall text-part out)))
          (should-not (string-match-p "Decisions\\|Deutsch\\|Grammar" (funcall text-part out)))
          ;; * Philology: no translation any more, what justifies it after the segments
          (should-not (string-match-p "Proposed translation\\|^\\*Translation:\\*" (funcall philology out)))
          (should (string-suffix-p
                   (concat "→ t\n\n"
                           (if (member template '("sanskrit" "bilingual")) "** Translation\n" "")
                           "*Decisions:* D\n\n*Deutsch:* Ü\n\n*Grammar:* G\n\n")
                   (funcall philology out))))
        (let ((out (funcall render template without nil)))
          (should (string-suffix-p "\n#+end_verse\n\n*Translation:* T\n\n" (funcall text-part out)))
          (should-not (string-match-p "^\\*Translation:\\*" (funcall philology out)))
          (should (string-suffix-p
                   (concat "→ t\n\n"
                           (if (member template '("sanskrit" "bilingual")) "** Translation\n" "")
                           "*Grammar:* G\n\n")
                   (funcall philology out))))
        ;; not analysed: neither
        (let ((out (funcall render template nil nil)))
          (should (string-suffix-p "\n#+end_verse\n\n" (funcall text-part out)))
          (should-not (string-match-p "^\\*\\* Translation$" out)))))
    ;; Sanskrit, a verse: after the table of the metre
    (should (string-match-p
             (regexp-quote (concat "#+end_verse\n\n| a |\n| – |\n\n" quotes "\n\n* Philology\n"))
             (funcall render "sanskrit" with '((scansion . "| a |\n| – |")))))
    ;; a pair: after both originals
    (should (string-match-p
             (regexp-quote (concat "#+begin_verse\nཆོས།\nchos/\n#+end_verse\n\n" quotes "\n\n* Philology\n"))
             (funcall render "bilingual" with '((tibetan-p . t) (tibetan-lines ((line . "ཆོས།") (wylie . "chos/")))))))
    (should (string-match-p
             (regexp-quote (concat "#+begin_verse\nअ\na b\n#+end_verse\n\n" quotes "\n\n* Philology\n"))
             (funcall render "bilingual-tibetan" with
                      '((sanskrit-p . t) (sanskrit ((devanagari . "अ") (samhita . "a b")))))))))

(ert-deftest bcat-template-discuss-line ()
  "18.11.: the line that marks a point for the discussion (`discuss')
stands at its segment, right after the „→“ translation — in every
template of a sentence file; a segment without a point has none."
  (let* ((bcat-template-directory
          (expand-file-name "Templates/analysis" bcat-template-test--root))
         (line "*Discuss:* /a/ — D: „x“; here: „y“ (→ [[point-1][point 1]])")
         (seg (lambda (num &optional discuss)
                `((num . ,num) (shown . t) (mark . "") (uchen . "ཀ།") (wylie . "ka/") (devanagari . "अ")
                  (samhita . "a") (padapatha . "a") (gloss-table . "| a |") (segment-translation . "t")
                  ,@(and discuss `((discuss . ,discuss)))))))
    (dolist (template '("sanskrit" "tibetan" "bilingual" "bilingual-tibetan"))
      (ert-info ((format "Vorlage %s" template))
        (let ((out (bcat-template-render-named
                    template `((segments ,(funcall seg "1" line) ,(funcall seg "2")) (recitation . "अ\na")))))
          (should (string-match-p (regexp-quote (concat "\n→ t\n\n" line "\n\n")) out))
          (should (= 1 (with-temp-buffer (insert out) (how-many "^\\*Discuss:\\*" (point-min) (point-max))))))))))

(ert-deftest bcat-template-word-analysis-folded ()
  "19.11. (Carsten: the Word Analysis „redundant … keinen Mehrwert“ for
reading — the padapāṭha is the bold line, the forms are in the table;
the tool needs it, for the lemmata of the zettel links and C-c u L —
„gute Idee“ to fold it): ** Word Analysis opens folded, a drawer
:VISIBILITY: folded under its heading, in the Sanskrit file and the
Sanskrit file of a pair; the readers skip the drawer (the padapāṭha and
the words of each segment as before), and C-c u L does not double it."
  (let* ((bcat-template-directory
          (expand-file-name "Templates/analysis" bcat-template-test--root))
         (wa "*** Segment 1\na b\n\n- a — a; ind.")
         (drawer (concat "** Word Analysis\n:PROPERTIES:\n:VISIBILITY: folded\n:END:\n" wa "\n")))
    (dolist (template '("sanskrit" "bilingual"))
      (ert-info ((format "Vorlage %s" template))
        (let ((out (bcat-template-render-named template `((word-analysis . ,wa)))))
          (should (string-match-p (regexp-quote drawer) out))
          (should (equal (tibetan-sanskrit-reading-parse-word-analysis wa)
                         (tibetan-sanskrit-reading-parse-word-analysis
                          (with-temp-buffer
                            (insert out)
                            (let ((file (make-temp-file "bcat-wa-" nil ".org")))
                              (unwind-protect
                                  (progn (write-region (point-min) (point-max) file nil 0)
                                         (tibetan-sentence--read-l2-body file "Word Analysis"))
                                (delete-file file)))))))
          (should (equal '("a" "b") (plist-get (cdar (tibetan-sanskrit-reading-parse-word-analysis wa)) :pada))))))))

(ert-deftest bcat-template-sanskrit-prose ()
  "Viveka prose (no metre): * Text holds only the Devanāgarī original
\(no metre line, no scansion); in the rump * Philology has no segments —
no tables before C-c u G, no padapāṭha any more (31.10. abends: „Der
Text kann ganz weg“), no translation line —, the Concepts and
Translations compared before the segments' details."
  (let* ((bcat-template-directory
          (expand-file-name "Templates/analysis" bcat-template-test--root))
         (out (bcat-template-render-named
               "sanskrit"
               '((sent-num . "1008") (segment-list . "687")
                 (recitation . "इह खलु शास्त्रादौ …")
                 (segments ((num . "687") (padapatha . "iha khalu śāstrādau …")))))))
    (should (string-match-p
             (regexp-quote (concat "#+SEGMENTS: 687\n\n* Text\n#+begin_verse\nइह खलु शास्त्रादौ …\n"
                                   "#+end_verse\n\n* Philology\n** Concepts\n"
                                   "** Translations compared\n\n** Segment 687\n\n"))
             out))
    (should-not (string-match-p "Translation:" out))))

(provide 'bcat-template-test)
;;; bcat-template-test.el ends here
