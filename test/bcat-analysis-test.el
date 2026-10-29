;;; bcat-analysis-test.el --- Tests for the C-c u G analysis (template files)  -*- lexical-binding: t; -*-

;;; Commentary:
;; §5.74 step 3a: prompts, the JSON answer, and the template fields
;; built from it.  The golden test feeds a canned answer for bDen
;; sentence 9 (test/fixtures/bcat-analysis-sent-009.json — drawn from
;; the reviewed sample) through the field builder and the tool's
;; Tibetan template: the result must BE the sample.  Pure; no network.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((root (expand-file-name ".." (file-name-directory
                                    (or load-file-name buffer-file-name)))))
  (dolist (d '("persist" "analysis" "core" "test/fixtures"))
    (add-to-list 'load-path (expand-file-name d root))))

(require 'bcat-template)
(require 'bcat-analysis)
(require 'tibetan-gloss-table)
(require 'bcat-template-sent-009)

(defconst bcat-analysis-test--root
  (expand-file-name ".." (file-name-directory (or load-file-name buffer-file-name))))

(defun bcat-analysis-test--answer ()
  "The canned Claude answer for bDen sentence 9 (JSON text)."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "test/fixtures/bcat-analysis-sent-009.json" bcat-analysis-test--root))
    (buffer-string)))

(defun bcat-analysis-test--base ()
  "The rump fields of sentence 9: header, segments (num, wylie), user
slots — what C-c u A knows without AI."
  (let ((fix bcat-template-test--sent-009))
    (cons (cons 'segments (mapcar (lambda (s) (list (assq 'num s) (assq 'wylie s)))
                                  (alist-get 'segments fix)))
          (cl-remove-if (lambda (c) (memq (car c) '(segments grammar comparison))) fix))))

(defconst bcat-analysis-test--refs
  '(("Lindtner 1981" . "[It is also] inexpressible (avācya) and invisible (adraṣṭavya), unchangeable (avikāra) and uncompounded (asaṃskṛta). When a yogi understands it he abandons the obstructions of defilement and cognizable (kleśajñeyāvaraṇa).")
    ("DharmaMitra" . "Inexpressible and invisible, / Unchanging and unconditioned— / When a yogi realizes this, / The obscurations of afflictions and the knowable are abandoned."))
  "The reference translations of sentence 9 as given to the request —
the sentence-level paragraphs are these texts, marked from the parts.")

(ert-deftest bcat-analysis-golden-sentence-009 ()
  "Claude's answer for bDen 9 → template fields → the Tibetan template
= the reviewed sample, character for character: tables without \"?\"
\(particles without a dictionary link), the translation blocks with the
reference parts and the „→“ line, Note/Concepts only where given, the
sentence comparison."
  (let ((bcat-template-directory
         (expand-file-name "Templates/analysis" bcat-analysis-test--root))
        (expected (with-temp-buffer
                    (insert-file-contents
                     (expand-file-name "Templates/analysis/samples/Tibetan.org"
                                       bcat-analysis-test--root))
                    (buffer-string))))
    (should (equal expected
                   (bcat-template-render-named
                    "tibetan"
                    (bcat-analysis-context (bcat-analysis-test--base)
                                           (bcat-analysis-parse (bcat-analysis-test--answer))
                                           bcat-analysis-test--refs))))))

(ert-deftest bcat-analysis-parse-tolerant ()
  "The JSON object is found inside a code fence or prose; anything
without a parsable object with segments is nil."
  (let ((obj "{\"segments\": [{\"num\": 1, \"words\": []}], \"grammar\": \"g\"}"))
    (should (equal "g" (alist-get 'grammar (bcat-analysis-parse obj))))
    (should (equal 1 (alist-get 'num (car (alist-get 'segments
                                                     (bcat-analysis-parse
                                                      (concat "```json\n" obj "\n```")))))))
    (should (bcat-analysis-parse (concat "Here you go:\n" obj "\nHope this helps.")))
    (should-not (bcat-analysis-parse "no json here"))
    (should-not (bcat-analysis-parse "{\"grammar\": \"g\"}"))
    (should-not (bcat-analysis-parse "{\"segments\": [1, 2"))
    (should-not (bcat-analysis-parse nil))))

(ert-deftest bcat-analysis-parse-repairs-copied-text ()
  "The prompt asks for the reference translations verbatim inside JSON
strings — Lindtner's verse 9 with its note (a paragraph break; Eimer
quoted as \"Um die Hemmnisse, nämlich die \"Befleckungen\", …\").  A raw
line break or tab, or an unescaped inner quote, must not cost the whole
answer (28.10., Satz 9 „nicht lesbar“).  Valid JSON is left alone."
  (let* ((lindtner (concat "When a yogi understands it he abandons the obstructions. [fn:9]\n\n"
                           "[fn:9] Eimer's translation \"Um die Hemmnisse, nämlich die "
                           "\"Befleckungen\", restlos aufzugeben, . . . \" is wrong."))
         (answer (concat "{\"segments\": [{\"num\": 41, \"note\": \"a\tb\"}], "
                         "\"references\": [{\"name\": \"Lindtner 1981\", \"text\": \""
                         lindtner "\"}], \"grammar\": \"g\"}"))
         (parsed (bcat-analysis-parse answer)))
    (should parsed)
    (should (equal lindtner (alist-get 'text (car (alist-get 'references parsed)))))
    (should (equal "a\tb" (alist-get 'note (car (alist-get 'segments parsed)))))
    (should (equal "g" (alist-get 'grammar parsed)))
    (let ((golden (bcat-analysis-test--answer)))
      (should (equal golden (bcat-analysis--repair-json golden))))))

(ert-deftest bcat-analysis-mark-reference ()
  "The sentence-level paragraph of a reference is OUR text, note
included; its bold marks come from Claude's segment parts, in order: a
word marked in a later part is bolded where that part stands, not at an
earlier occurrence; whitespace may differ; a passage not found changes
nothing."
  (let ((text (concat "When it is so. When a yogi understands\nit he abandons the "
                      "obstructions (kleśajñeyāvaraṇa). [fn:9]\n\n"
                      "[fn:9] Eimer's \"Befleckungen\" is wrong.")))
    (should (equal (concat "When it is so. *When* a yogi understands\nit *he abandons* the "
                           "obstructions *(kleśajñeyāvaraṇa)*. [fn:9]\n\n"
                           "[fn:9] Eimer's \"Befleckungen\" is wrong.")
                   (bcat-analysis-mark-reference
                    text '("When it is so."
                           "*When* a yogi understands it"
                           "*he abandons* the obstructions *(kleśajñeyāvaraṇa)*."
                           "*nowhere*"))))
    (should (equal text (bcat-analysis-mark-reference text nil)))
    (should (equal text (bcat-analysis-mark-reference text '("no *such* part"))))))

(ert-deftest bcat-analysis-glossed-line ()
  "Carsten 29.10.: translating segment by segment meant scrolling (on
the reMarkable: turning pages) between the verse and the tables — „die
Vokabeln in Klammern beim Text“.  Each verse line carries its words'
glosses in brackets, a particle its function; a line the words do not
cover exactly stays plain Wylie (the text is never altered)."
  (let ((words '(((wylie . "rnal 'byor pa") (gloss . "yogin") (function . "N · agent"))
                 ((wylie . "yis") (gloss . "") (function . "ERG"))
                 ((wylie . "de") (gloss . "that (= paramārtha)") (function . "DEM · object"))
                 ((wylie . "rtogs") (gloss . "realizes") (function . "V pf. (rtogs pa) · main verb"))
                 ((wylie . "na") (gloss . "if") (function . "CONV · cond.")))))
    (should (equal (concat "rnal 'byor pa [yogin] yis [ERG] de [that (= paramārtha)] "
                           "rtogs [realizes] na [CONV · cond.] / /")
                   (bcat-analysis-glossed-line "rnal 'byor pa yis de rtogs na/ /" words)))
    (should (equal "rnal 'byor pa yis de rtogs na/ /"
                   (bcat-analysis-glossed-line "rnal 'byor pa yis de rtogs na/ /" (butlast words))))
    ;; „?“ = unknown (the rump before C-c u G): no bracket at all.
    (should (equal "ka kha [k] /"
                   (bcat-analysis-glossed-line
                    "ka kha/" '(((wylie . "ka") (gloss . "?") (function . "?"))
                                ((wylie . "kha") (gloss . "k") (function . "N"))))))
    (should (equal "ka/ /" (bcat-analysis-glossed-line "ka/ /" nil)))))

(ert-deftest bcat-analysis-prompts ()
  "System prompt: the JSON schema and the target language; user prompt:
the document context, every segment with Tibetan and Wylie, each
reference translation by name (or \"none\")."
  (let ((sys-en (bcat-analysis-system-prompt "en"))
        (sys-de (bcat-analysis-system-prompt "de"))
        (user (bcat-analysis-user-prompt
               9 "SDA 9"
               '((38 "བརྗོད་དུ་མེད་པ། །" "brjod du med pa/ /")
                 (39 "འགྱུར་བ་མེད་པ། །" "'gyur ba med pa/ /"))
               '(("Lindtner 1981" . "inexpressible") ("DharmaMitra" . "Inexpressible"))
               '("Atiśa, Satyadvayāvatāra"))))
    (dolist (key '("\"segments\"" "\"words\"" "\"gloss\"" "\"function\"" "\"shift\""
                   "\"structure\"" "\"note\"" "\"concepts\"" "\"grammar\"" "\"comparison\""))
      (should (string-match-p (regexp-quote key) sys-en)))
    ;; 29.10.: Claude no longer copies whole reference translations
    ;; (verbatim copies broke the JSON and dropped Lindtner's note) —
    ;; the sentence-level marks come from the segment parts.
    (should-not (string-match-p "in full" sys-en))
    (should-not (string-match-p "in full" (bcat-analysis-align-system-prompt "en")))
    (should (string-match-p "not its notes" sys-en))
    (should (string-match-p "not its notes" (bcat-analysis-align-system-prompt "en")))
    (should (string-match-p "in English" sys-en))
    (should (string-match-p "in German" sys-de))
    (should (string-match-p "\\`Atiśa, Satyadvayāvatāra" user))
    (should (string-match-p "Sentence 9 (SDA 9)" user))
    (should (string-match-p (regexp-quote "[38] Tibetan: བརྗོད་དུ་མེད་པ། །\n     Wylie: brjod du med pa/ /") user))
    (should (string-match-p "=== Lindtner 1981 ===\ninexpressible" user))
    (should (string-match-p "=== DharmaMitra ===\nInexpressible" user))
    (should (string-match-p "Reference translations:\nnone"
                            (bcat-analysis-user-prompt 1 nil '((1 "ཀ" "ka")) nil)))))

(ert-deftest bcat-analysis-gloss-table-cells ()
  "Particles (function = a case or converb label) get no dictionary
link; a long gloss is cut to the table budget; a `|' in a cell is
escaped so the table holds."
  (let ((table (bcat-analysis-gloss-table
                '(((wylie . "rnal 'byor pa") (gloss . "yogin") (function . "N · agent"))
                  ((wylie . "yis") (gloss . "by") (function . "ERG"))
                  ((wylie . "na") (gloss . "if") (function . "CONV · cond."))
                  ((wylie . "de") (gloss . "a very long gloss that keeps on going and going")
                   (function . "DEM | object"))))))
    (should (string-match-p "\\[\\[steinert:rnal 'byor pa\\]\\[rnal 'byor pa\\]\\]" table))
    (should (string-match-p "| yis " table))
    (should-not (string-match-p "steinert:yis\\|steinert:na\\]" table))
    (should (string-match-p "\\[\\[steinert:de\\]" table))
    (should (string-match-p "…" table))
    (should (string-match-p "DEM \\\\vert object" table))
    (should (= 3 (length (split-string table "\n")))))
  (should (equal "" (bcat-analysis-gloss-table nil))))

(ert-deftest bcat-analysis-context-partial-answer ()
  "A segment Claude did not return keeps its rump table; empty fields
of a returned segment drop their lines; no references → no reference
lines and no „→“."
  (let* ((base '((segments ((num . 1) (wylie . "ka") (gloss-table . "| RUMPF |"))
                           ((num . 2) (wylie . "kha") (gloss-table . "| RUMPF2 |")))))
         (parsed '((segments ((num . 1)
                              (words ((wylie . "ka") (gloss . "k") (function . "N")))
                              (translation . "one") (references) (shift . "")
                              (structure . "s1") (note . "") (concepts . "")))
                   (grammar . "G") (translation . "one two") (references)
                   (comparison . "")))
         (ctx (bcat-analysis-context base parsed))
         (segs (alist-get 'segments ctx)))
    (should (string-match-p "steinert:ka" (alist-get 'gloss-table (nth 0 segs))))
    (should (equal "⟦1⟧ *Translation:* one" (alist-get 'translations (nth 0 segs))))
    (should (equal "| RUMPF2 |" (alist-get 'gloss-table (nth 1 segs))))
    (should-not (alist-get 'translations (nth 1 segs)))
    (should (equal "*Translation:* one two" (alist-get 'comparison ctx)))
    (should (equal "G" (alist-get 'grammar ctx)))))

(provide 'bcat-analysis-test)
;;; bcat-analysis-test.el ends here
