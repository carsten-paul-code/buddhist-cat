;;; bcat-relayout-test.el --- Tests for setting a sentence file anew without a fetch  -*- lexical-binding: t; -*-

;;; Commentary:
;; Carsten 31.10.2026 abends, TĀ 1006: the sentence files got a new order
;; (984d377); for the files analysed already „Ohne Abruf umbauen“.  The
;; spec: an old file read back and rendered with the new template IS the
;; new sample — the reviewed old samples (test/fixtures/old-layout-*.org,
;; the samples of 78a26ed) against the reviewed new ones; and a file in
;; the new order comes out unchanged.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((root (expand-file-name ".." (file-name-directory
                                    (or load-file-name buffer-file-name)))))
  (dolist (d '("persist" "analysis" "core" "test" "test/fixtures"))
    (add-to-list 'load-path (expand-file-name d root))))

(require 'bcat-analysis-test)
(require 'bcat-sentence-test)
(require 'bcat-relayout)

(defun bcat-relayout-test--file (name)
  "The text of FILE NAME under the package root."
  (with-temp-buffer
    (insert-file-contents (expand-file-name name bcat-analysis-test--root))
    (buffer-string)))

(defun bcat-relayout-test--render (template base text &optional other)
  "TEXT read back into BASE and rendered with the tool's TEMPLATE —
OTHER the text of the partner file of a bilingual pair."
  (let ((bcat-template-directory (expand-file-name "Templates/analysis" bcat-analysis-test--root)))
    (bcat-template-render-named template (bcat-relayout-context base text template other))))

(defun bcat-relayout-test--partner-base ()
  "The rump of the pair's Tibetan file of AS 3, with the Sanskrit
segments the rump has (Devanāgarī, the line as transmitted)."
  (append '((sanskrit-p . t))
          `((sanskrit . ,(alist-get 'segments (bcat-analysis-test--base-003))))
          (bcat-analysis-test--base-tib-003)))

(ert-deftest bcat-relayout-old-sanskrit ()
  "TĀ 1007 in the old order → the new sample: tables under * Text with
⟦N⟧, the concepts under * Sentence, the rest per segment."
  (should (equal (bcat-relayout-test--file "Templates/analysis/samples/Sanskrit.org")
                 (bcat-relayout-test--render
                  "sanskrit" (bcat-analysis-test--base-1007)
                  (bcat-relayout-test--file "test/fixtures/old-layout-Sanskrit.org")))))

(ert-deftest bcat-relayout-old-tibetan ()
  "bDen 9 in the old order → the new sample."
  (should (equal (bcat-relayout-test--file "Templates/analysis/samples/Tibetan.org")
                 (bcat-relayout-test--render
                  "tibetan" (bcat-analysis-test--base)
                  (bcat-relayout-test--file "test/fixtures/old-layout-Tibetan.org")))))

(ert-deftest bcat-relayout-old-bilingual ()
  "AS 3 (Sanskrit with the Tibetan) in the old order → the new sample:
per segment the Sanskrit tables and translation, the Tibetan tables
(Uchen added) and translation — read from the partner file —, the
differences; the assignment from the segment headings (1.11.)."
  (should (equal (bcat-relayout-test--file "Templates/analysis/samples/Bilingual.org")
                 (bcat-relayout-test--render
                  "bilingual" (bcat-analysis-test--base-003)
                  (bcat-relayout-test--file "test/fixtures/old-layout-Bilingual.org")
                  (bcat-relayout-test--file "test/fixtures/old-layout-Bilingual-Tibetan.org")))))

(ert-deftest bcat-relayout-old-partner ()
  "The Tibetan file of the pair in the old order → the new sample:
mirrored, the Sanskrit segment (tables, translation, differences — from
the Sanskrit file) after the last Tibetan segment of its group."
  (should (equal (bcat-relayout-test--file "Templates/analysis/samples/Bilingual-Tibetan.org")
                 (bcat-relayout-test--render
                  "bilingual-tibetan" (bcat-relayout-test--partner-base)
                  (bcat-relayout-test--file "test/fixtures/old-layout-Bilingual-Tibetan.org")
                  (bcat-relayout-test--file "test/fixtures/old-layout-Bilingual.org")))))

(ert-deftest bcat-relayout-new-order-unchanged ()
  "A file in the new order read back and set anew is the same file —
for each template."
  (dolist (case `(("sanskrit" "Sanskrit" ,(bcat-analysis-test--base-1007) nil)
                  ("tibetan" "Tibetan" ,(bcat-analysis-test--base) nil)
                  ("bilingual" "Bilingual" ,(bcat-analysis-test--base-003) "Bilingual-Tibetan")
                  ("bilingual-tibetan" "Bilingual-Tibetan" ,(bcat-relayout-test--partner-base) "Bilingual")))
    (let ((sample (bcat-relayout-test--file
                   (format "Templates/analysis/samples/%s.org" (nth 1 case))))
          (other (and (nth 3 case)
                      (bcat-relayout-test--file
                       (format "Templates/analysis/samples/%s.org" (nth 3 case))))))
      (should (equal sample (bcat-relayout-test--render (nth 0 case) (nth 2 case) sample other))))))

(ert-deftest bcat-relayout-label-on-its-own-line ()
  "31.10. abends, the first PDF of TĀ 1006 in the new order: „⟦683⟧“
directly above a table stood beside it (LaTeX took both as one
paragraph) — the mark got a blank line under it (ad89035).  4.11.
(„ziemlich viel verschenkter Platz“): in the file the mark stands right
above its table again, the export puts the blank line in
\(`bcat-export--marks-apart').  Files written in between (mark, blank
line, table) read back to the new form."
  (let* ((sample (bcat-relayout-test--file "Templates/analysis/samples/Tibetan.org"))
         (spaced (replace-regexp-in-string "^\\(⟦[0-9]+⟧\\)\n|" "\\1\n\n|" sample)))
    (should-not (equal spaced sample))
    (should (string-match-p "^⟦38⟧\n| " sample))
    (should (equal sample (bcat-relayout-test--render "tibetan" (bcat-analysis-test--base) spaced)))))

(ert-deftest bcat-relayout-sanskrit-subtables ()
  "5.11.: C-c u L sets a Sanskrit file written before (tables of five
rows) with the overview, „→“, and the parts tables; read back, the parts
tables stay as they are — a second run changes nothing."
  (let* ((words bcat-analysis-test--ta-1-1-words)
         (old (concat "* Text\n⟦1⟧\n" (bcat-analysis-test--old-sanskrit-tables words)
                      "\n\n→ t\n\n* Segments\n** Segment 1\n\n*Structure:* s\n"))
         (base '((segments ((num . 1) (samhita . "vimala…"))) (sent-num . "1")))
         (seg (car (alist-get 'segments (bcat-relayout-context base old "sanskrit")))))
    (should (equal (bcat-analysis-sanskrit-tables words) (alist-get 'gloss-table seg)))
    (should (string-prefix-p "| vimala " (alist-get 'formation-tables seg)))
    (should (string-match-p "^| janakaḥ +|\n|-+|\n| /√jan/ (1) beget |" (alist-get 'formation-tables seg)))
    (let* ((once (bcat-relayout-test--render "sanskrit" base old))
           (again (bcat-relayout-test--render "sanskrit" base once)))
      (should (string-match-p "^→ t\n\n\\*\\*\\* Word Formation\n:PROPERTIES:\n:VISIBILITY: folded\n:END:\n| vimala " once))
      (should (equal once again)))))

(ert-deftest bcat-relayout-parts-lines-to-tables ()
  "Carsten 9.11., TĀ 1009 („der Formatwechsel (Tabelle/Fließtext)
verwirrt mich“): C-c u L sets a file of 5.11. (draft B) with the line
of a word of one member as its table of one column — the parts as the
line has them, a hand correction stays —, in the order of the words;
the tables of compounds stay as they are."
  (let* ((old (concat "* Text\n⟦1⟧\n| hṛdayam | a-b |\n| heart | g |\n| n. | f |\n\n→ t\n\n"
                      "/hṛdayam/: /hṛd/ heart · /-aya/ my suffix\n\n"
                      "| a  | b  |\n|----+----|\n| x  | y  |\n\n"
                      "* Segments\n** Segment 1\n\n*Structure:* s\n"))
         (base '((segments ((num . 1) (samhita . "hṛdayam ab"))) (sent-num . "1")))
         (seg (car (alist-get 'segments (bcat-relayout-context base old "sanskrit")))))
    (should (equal (concat "| hṛdayam          |\n|------------------|\n| /hṛd/ heart      |\n"
                           "| /-aya/ my suffix |\n\n| a  | b  |\n|----+----|\n| x  | y  |")
                   (alist-get 'formation-tables seg)))))

(ert-deftest bcat-relayout-segment-headings ()
  "9.11. (TĀ 1009): C-c u L sets a file of 5.11. (⟦N⟧ a line, the parts
right after „→“, the sentence's translation after the last segment) in
the new structure — ⟦N⟧ a heading, the original in bold under it, the
parts under a folded Word Formation, the sentence under ** Translation;
read back, a second run changes nothing, and a hand correction of the
bold line stays."
  (let* ((old (concat "* Text\n⟦1⟧\n| hṛdayam | ab |\n| heart | g |\n| n. | f |\n\n→ t\n\n"
                      "/hṛdayam/: /hṛd/ heart · /-aya/ suffix\n\n"
                      "*Grammar:* g\n\n*Translation:* T\n\n"
                      "* Segments\n** Segment 1\n\n*Structure:* s\n"))
         (base '((segments ((num . 1) (samhita . "hṛdayam ab /") (padapatha . "hṛdayam ab /")))
                 (sent-num . "1")))
         (once (bcat-relayout-test--render "sanskrit" base old))
         (again (bcat-relayout-test--render "sanskrit" base once)))
    (should (string-match-p (regexp-quote "\n** ⟦1⟧\n*hṛdayam ab /*\n\n| hṛdayam | ab |") once))
    (should (string-match-p (regexp-quote "→ t\n\n*** Word Formation\n:PROPERTIES:\n:VISIBILITY: folded\n:END:\n| hṛdayam ") once))
    (should (string-match-p (regexp-quote "\n** Translation\n*Translation:* T\n\n*Grammar:* g\n") once))
    (should-not (string-match-p "^/hṛdayam/:" once))
    (should (equal once again))
    (let ((fixed (replace-regexp-in-string "^\\*hṛdayam ab /\\*$" "*hṛdayam a-b /*" once)))
      (should-not (equal fixed once))
      (should (equal fixed (bcat-relayout-test--render "sanskrit" base fixed))))))

(ert-deftest bcat-relayout-compacts-tables ()
  "4.11.: C-c u L sets a Tibetan file written before with compact tables
— the long function cut, its rest a line of the segment's Structure;
a second run changes nothing."
  (let* ((old (concat "* Text\n⟦66⟧\n"
                      "| དེ                  | བརྒྱུད་པའི                                                           |\n"
                      "| [[steinert:de][de]]                 | [[steinert:brgyud pa'i][brgyud pa'i]]                                                      |\n"
                      "| that/him           | transmitted/lineage of                                           |\n"
                      "| DEM · oblique base | V pf. (brgyud pa) · relative clause, GEN attributive to man ngag |\n\n"
                      "→ t\n\n* Segments\n** Segment 66\n⟦66⟧ *Translation:* t\n\n*Structure:* de [source]\n"))
         (base '((segments ((num . 66) (uchen . "དེ་བརྒྱུད་པའི"))) (sent-num . "16")))
         (ctx (bcat-relayout-context base old "tibetan"))
         (seg (car (alist-get 'segments ctx))))
    (should (string-match-p "| V pf\\. +|" (alist-get 'gloss-table seg)))
    (should (equal "de [source] \\\\\n/brgyud pa'i/: relative clause, GEN attributive to man ngag"
                   (alist-get 'structure seg)))
    (let* ((again (bcat-relayout-test--render "tibetan" base
                                               (bcat-relayout-test--render "tibetan" base old)))
           (once (bcat-relayout-test--render "tibetan" base old)))
      (should (equal once again)))))

(ert-deftest bcat-relayout-proposal-under-the-tables ()
  "4.11.: C-c u L sets a file of the layout before (Grammar under
* Sentence, the own translation under the tables, the proposal inside
Translations compared) in the new one — grammar, proposal and decisions
under the tables, the proposal's words those of its old heading; a
second run changes nothing."
  (let* ((old (concat "* Text\n⟦38⟧\n| a |\n| b |\n| c |\n\n→ s\n\n*Translation:* Own.\n\n"
                      "* Sentence\n** Grammar\nTwo pādas.\n\n** Concepts\n\n** Translations compared\n"
                      "*Translation:* Own.\n\n*Lindtner 1981:* L.\n\n"
                      "*** Übersicht\nSigel: C = Translation (oben), L = Lindtner 1981\n\n"
                      "*** Übersetzungsvorschlag\n*Erster Sinn:*\n#+begin_quote\nEins.\n#+end_quote\n\n"
                      "#+begin_quote\nZwei.\n#+end_quote\n\n*Entscheidungen:* Nr. 1.\n\n"
                      "*** Berichtigungen zur Analyse\n- k.\n\n* Segments\n** Segment 38\n⟦38⟧ *Translation:* s\n"))
         (base '((segments ((num . 38))) (sent-num . "9")))
         (once (bcat-relayout-test--render "tibetan" base old))
         (twice (bcat-relayout-test--render "tibetan" base once)))
    (should (string-match-p
             (regexp-quote (concat "→ s\n\n"
                                   "*Übersetzungsvorschlag — Erster Sinn:*\n#+begin_quote\nEins.\n#+end_quote\n\n"
                                   "*Übersetzungsvorschlag:*\n#+begin_quote\nZwei.\n#+end_quote\n\n"
                                   "*Entscheidungen:* Nr. 1.\n\n*Grammar:* Two pādas.\n\n* Sentence\n"))
             once))
    (should-not (string-match-p "^\\*\\*\\* Übersetzungsvorschlag" once))
    (should (string-match-p "^\\*\\*\\* Berichtigungen zur Analyse\n- k\\." once))
    (should (string-match-p "^\\*\\* Translations compared\n\\*Translation:\\* Own\\." once))
    (should (equal once twice))))

(ert-deftest bcat-relayout-adds-uchen ()
  "Tables of a Tibetan file written before 31.10. abends (Wylie, gloss,
function) get the Uchen row when set anew — from the segment's Uchen;
a table that has it keeps it."
  (let* ((old "* Text\n⟦38⟧\n\n| [[steinert:brjod du med pa][brjod du med pa]] | [[steinert:bltar med pa][bltar med pa]] |\n| indescribable | invisible |\n| ADJ | ADJ |\n\n*Translation:* t\n")
         (ctx (bcat-relayout-context
               '((segments ((num . 38) (uchen . "བརྗོད་དུ་མེད་པ་བལྟར་མེད་པ། །"))))
               old "tibetan"))
         (table (alist-get 'gloss-table (car (alist-get 'segments ctx)))))
    (should (string-match-p "\\`| བརྗོད་དུ་མེད་པ +| བལྟར་མེད་པ +|\n| \\[\\[steinert:brjod" table))
    (should (equal table (bcat-analysis-uchen-table table "བརྗོད་དུ་མེད་པ་བལྟར་མེད་པ། །")))))

(ert-deftest bcat-relayout-segment-translation ()
  "Carsten 1.11.: „nach der Segmenttabelle eine Übersetzung (nur
Zielsprache) des Segments“ — a file written before gets it from the
first line of the segment's translation block; a line corrected by hand
under * Text stays."
  (let* ((sample (bcat-relayout-test--file "Templates/analysis/samples/Tibetan.org"))
         (edited (replace-regexp-in-string "^→ if a yogin .*$" "→ if the yogin realizes it," sample t t))
         (out (bcat-relayout-test--render "tibetan" (bcat-analysis-test--base) edited)))
    (should (string-match-p "^→ if the yogin realizes it,$" out))
    (should (string-match-p "^⟦40⟧ \\*Translation:\\* if a yogin (rnal 'byor pa: yogin) realizes that, " out))
    (should (string-match-p "| CONV · cond\\. |\n\n→ if the yogin realizes it,\n\n⟦41⟧"
                            out))))

(ert-deftest bcat-relayout-footnotes-last ()
  "SDA 9–20 (1.11.): a footnote definition in the middle of Translations
compared swallowed the paragraphs after it — set anew, it stands at the
end of the block."
  (let* ((text (concat "* Sentence\n** Grammar\nG\n\n** Concepts\n** Translations compared\n"
                       "*Lindtner 1981:* L. [fn:l10]\n\n[fn:l10] Lindtner, Anm. 10.\n\n"
                       "*Apple 2019:* A.\n\nThey differ.\n\n* Segments\n"))
         (ctx (bcat-relayout-context '((segments)) text "tibetan")))
    (should (equal "*Lindtner 1981:* L. [fn:l10]\n\n*Apple 2019:* A.\n\nThey differ.\n\n[fn:l10] Lindtner, Anm. 10."
                   (alist-get 'comparison ctx)))))

(ert-deftest bcat-relayout-second-translation ()
  "The translation in the second language (*Deutsch:* under * Text)
is read back and stays."
  (let* ((sample (bcat-relayout-test--file "Templates/analysis/samples/Tibetan.org"))
         (text (replace-regexp-in-string
                "^\\(\\*Translation:\\* Indescribable.*\\)$"
                "\\1\n\n*Deutsch:* Unsagbar und unsichtbar." sample t))
         (out (bcat-relayout-test--render "tibetan" (bcat-analysis-test--base) text)))
    (should (string-match-p "\n\n\\*Deutsch:\\* Unsagbar und unsichtbar\\.\n\n\\*Grammar:\\* " out))))

(ert-deftest bcat-relayout-file ()
  "C-c u L on an analysed file in the old order: the file in the new
order — My Notes, a section of its own (* Handout), #+CREATED and
#+LAST_ANALYZED kept (setting anew is no analysis), no request."
  (bcat-sentence-test--with-source
    (bcat-sentence-test--create src)
    (bcat-sentence-test--with-stubs bcat-sentence-test--answer
      (bcat-sentence-test--fetch src))
    (let* ((file (tibetan-sentence--filepath 9 folder src))
           (new (bcat-sentence-test--text-of file))
           (fired nil))
      ;; Back to the old order: tables and concepts in the segments.
      (with-temp-file file
        (insert (replace-regexp-in-string
                 "^#\\+LAST_ANALYZED: .*$" "#+LAST_ANALYZED: 2026-10-20"
                 (concat
                  (car (split-string new "^\\* Text\n"))
                  "* Text\n#+begin_verse\nབརྗོད་དུ་མེད་པ་བལྟར་མེད་པ། །\nའགྱུར་བ་མེད་པ་འདུས་མ་བྱས། །\n#+end_verse\n\n"
                  "*Translation:* Indescribable and invisible, immutable and uncompounded\n\n"
                  "* Segments\n** Segment 38\n| [[steinert:brjod du med pa][brjod du med pa]] |\n| indescribable |\n| ADJ |\n\n"
                  "⟦38⟧ *Translation:* Indescribable and invisible,\n\n*Structure:* no finite verb\n\n"
                  "** Segment 39\n| [[steinert:'dus ma byas]['dus ma byas]] |\n| uncompounded |\n| ADJ |\n\n"
                  "⟦39⟧ *Translation:* immutable and uncompounded\n\n*Structure:* continues the chain\n\n"
                  "*Concepts:* *'dus ma byas* (asaṃskṛta)\n\n"
                  "* Sentence\n** Grammar\nTwo predicate pādas.\n\n** Translations compared\n*Lindtner 1981:* L.\n\n"
                  "* Working Translation\n\n* My Notes\nMeine Notiz.\n\n* Footnotes\n\n* Handout\nH\n")
                 t)))
      (cl-letf (((symbol-function 'tibetan-discussion--submit)
                 (lambda (&rest _) (setq fired t))))
        (with-current-buffer (find-file-noselect file)
          (unwind-protect (bcat-relayout)
            (kill-buffer))))
      (let ((text (bcat-sentence-test--text-of file)))
        (should-not fired)
        ;; The old table gets its Uchen row (31.10. abends, Tibetisch V).
        (should (string-match-p "#\\+end_verse\n\n⟦38⟧\n| བརྗོད་དུ་མེད་པ +|\n| \\[\\[steinert:brjod du med pa\\]\\[brjod du med pa\\]\\] |\n| indescribable +|" text))
        (should (string-match-p (regexp-quote "** Concepts\n⟦39⟧ *'dus ma byas* (asaṃskṛta)\n") text))
        (should (string-match-p (regexp-quote "* Segments\n** Segment 38\n⟦38⟧ *Translation:* Indescribable and invisible,\n\n*Structure:* no finite verb\n") text))
        (should (string-match-p "^#\\+LAST_ANALYZED: 2026-10-20$" text))
        (should (string-match-p "^\\* My Notes\nMeine Notiz\\.$" text))
        (should (string-match-p "^\\* Handout\nH$" text))
        (should (= 1 (with-temp-buffer (insert text) (how-many "^\\* Handout$" (point-min) (point-max)))))
        ;; Still analysed: the ⟦N⟧ translations under * Segments.
        (should-not (eq 'full (bcat-sentence-fetch-state file '(38 39))))))))

(ert-deftest bcat-relayout-answers-kept ()
  "C-c u G keeps Claude's answer beside the file (Carsten 31.10.: „ab
jetzt: Antwort aufbewahrt“ — a later change of the template needs no
new fetch): analysis/.answers/<file>.eld, Lisp data read back exactly —
the answer and the references sent; for a bilingual sentence the
Tibetan answer and its references too."
  (bcat-sentence-test--with-source
    (bcat-sentence-test--create src)
    (bcat-sentence-test--with-stubs bcat-sentence-test--answer
      (bcat-sentence-test--fetch src))
    (let* ((file (tibetan-sentence--filepath 9 folder src))
           (record (bcat-sentence--answers file)))
      (should (file-exists-p (expand-file-name ".answers/sent-009-bden.eld" folder)))
      (should (equal (bcat-analysis-parse bcat-sentence-test--answer) (plist-get record :parsed)))
      (should (assoc "Lindtner 1981" (plist-get record :refs)))))
  (bcat-sentence-test--with-bil-source
    (bcat-sentence-test--create-n src 3)
    (bcat-sentence-test--with-stubs #'bcat-sentence-test--bil-answers
      (bcat-sentence-test--fetch-n src 3))
    (let ((record (bcat-sentence--answers (tibetan-sentence--filepath 3 folder src))))
      (should (assq 'segments (plist-get record :parsed)))
      (should (assq 'rendering (plist-get record :tibetan)))
      (should (listp (plist-get record :trefs))))))

(ert-deftest bcat-relayout-fills-from-answers ()
  "Setting anew with a kept answer: what the file shows wins — a hand
correction stays —, the answer fills what the file does not show (here
a class template that brings back the glossed verse lines)."
  (bcat-sentence-test--with-source
    (bcat-sentence-test--create src)
    (bcat-sentence-test--with-stubs bcat-sentence-test--answer
      (bcat-sentence-test--fetch src))
    (let ((file (tibetan-sentence--filepath 9 folder src)))
      ;; A hand correction in the file.
      (with-temp-file file
        (insert (replace-regexp-in-string "^\\*Structure:\\* no finite verb$"
                                          "*Structure:* corrected by hand"
                                          (bcat-sentence-test--text-of file) t t)))
      ;; The class's own template (the cascade): the glossed lines under * Text.
      (make-directory (expand-file-name "Templates/analysis" dir) t)
      (with-temp-file (expand-file-name "Templates/analysis/tibetan.org" dir)
        (insert (replace-regexp-in-string
                 "^\\* Text\n" "* Text\n{{#segments}}\nGLOSSED {{glossed}}\n{{/segments}}\n"
                 (with-temp-buffer
                   (insert-file-contents (expand-file-name "tibetan.org" bcat-template-directory))
                   (buffer-string))
                 t t)))
      (bcat-relayout-file file)
      (let ((text (bcat-sentence-test--text-of file)))
        (should (string-match-p "^\\*Structure:\\* corrected by hand$" text))
        (should (string-match-p "^GLOSSED brjod du med pa \\[indescribable\\]" text))))))

(ert-deftest bcat-relayout-file-pair ()
  "C-c u L on the Sanskrit file of a pair (Carsten 1.11.): the Tibetan
tables and translations per segment come from the Tibetan file of the
pair; no request."
  (bcat-sentence-test--with-pair
    (bcat-sentence-test--create-n src 3)
    (bcat-sentence-test--with-stubs #'bcat-sentence-test--pair-answers
      (bcat-sentence-test--fetch-n src 3))
    (let ((file (tibetan-sentence--filepath 3 folder src))
          (fired nil))
      ;; A pair written before: no Tibetan part, no answer kept.
      (delete-file (bcat-sentence--answers-file file))
      (with-temp-file file
        (insert (replace-regexp-in-string "^\\* Tibetan\n\\(?:.\\|\n\\)*?\\(^\\* Sentence$\\)" "\\1"
                                          (bcat-sentence-test--text-of file) t)))
      (should-not (string-match-p "^| ཅིའི་ཕྱིར" (bcat-sentence-test--text-of file)))
      (cl-letf (((symbol-function 'tibetan-discussion--submit)
                 (lambda (&rest _) (setq fired t))))
        (bcat-relayout-file file))
      (should-not fired)
      (should (string-match-p "^\\*\\* <<skt-22>>⟦22⟧ · \\[\\[tib-77\\]\\[Tib\\. 77\\]\\]\n\\*kasmāt pañca eva skandhāḥ\\.\\*\n\n| kasmāt"
                              (bcat-sentence-test--text-of file)))
      (should (string-match-p "^\\* Tibetan\n\\(?:.\\|\n\\)*\n| ཅིའི་ཕྱིར" (bcat-sentence-test--text-of file))))))

(ert-deftest bcat-relayout-file-context ()
  "What C-c u L sets a file anew from, on its own (Carsten 5.11.: the
PDF of C-c u H is rendered from the same fields): the template, the
source and the fields read back from the file — a hand correction
included; no request.  Not a sentence file of a template: an error text,
no fields."
  (bcat-sentence-test--with-source
    (bcat-sentence-test--create src)
    (bcat-sentence-test--with-stubs bcat-sentence-test--answer
      (bcat-sentence-test--fetch src))
    (let ((file (tibetan-sentence--filepath 9 folder src))
          (fired nil))
      (with-temp-file file
        (insert (replace-regexp-in-string "^\\*Structure:\\* no finite verb$"
                                          "*Structure:* corrected by hand"
                                          (bcat-sentence-test--text-of file) t t)))
      (let ((read (cl-letf (((symbol-function 'tibetan-discussion--submit)
                             (lambda (&rest _) (setq fired t))))
                    (bcat-relayout-file-context file))))
        (should-not fired)
        (should (equal "tibetan" (plist-get read :template)))
        (should (equal (expand-file-name src) (expand-file-name (plist-get read :source))))
        (should (equal "corrected by hand"
                       (alist-get 'structure (cl-find 38 (alist-get 'segments (plist-get read :context))
                                                      :key #'bcat-analysis--num))))
        (should-not (plist-get read :error))))
    (let ((other (expand-file-name "notes.org" folder)))
      (with-temp-file other (insert "* Notes\n"))
      (let ((read (bcat-relayout-file-context other)))
        (should (stringp (plist-get read :error)))
        (should-not (plist-get read :context))))))

(ert-deftest bcat-relayout-key-and-menu ()
  "C-c u L sets anew; the menu offers it next to the jumps."
  (require 'tibetan-keybindings)
  (require 'tibetan-menu)
  (should (eq (key-binding (kbd "C-c u L")) 'bcat-relayout))
  (should (string-match-p "bcat-relayout"
                          (prin1-to-string (symbol-value 'tibetan-cat-menu)))))

(provide 'bcat-relayout-test)
;;; bcat-relayout-test.el ends here
