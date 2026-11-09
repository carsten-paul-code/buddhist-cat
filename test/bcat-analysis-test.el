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
(require 'bcat-template-sent-1007)

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
    (cons (cons 'segments (mapcar (lambda (s) (list (assq 'num s) (assq 'uchen s) (assq 'wylie s)))
                                  (alist-get 'segments fix)))
          (cl-remove-if (lambda (c) (memq (car c) '(segments grammar comparison glosses translation)))
                        fix))))

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

(defun bcat-analysis-test--answer-1007 ()
  "Claude's answer for TĀ sentence 1007 in the Sanskrit contract (drawn
from the reviewed sample, test/fixtures/bcat-analysis-sent-1007.json)."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "test/fixtures/bcat-analysis-sent-1007.json" bcat-analysis-test--root))
    (buffer-string)))

(defun bcat-analysis-test--base-1007 ()
  "The rump fields of sentence 1007: header, recitation, segments with
num and samhita, user slots — what C-c u A knows without AI."
  (let ((fix bcat-template-test--sent-1007))
    (cons (cons 'segments (mapcar (lambda (s) (list (assq 'num s) (assq 'samhita s)))
                                  (alist-get 'segments fix)))
          (cl-remove-if (lambda (c) (memq (car c) '(segments grammar comparison glosses translation)))
                        fix))))

(defconst bcat-analysis-test--refs-1007
  '(("Dyczkowski 2023" . "Although my speech is powerless here (in this task), whatever restriction may arise (on one's understanding of the teachings is not my fault, rather) that is (all) the venerable expansion of the Supreme Lord's great bliss (which both hides and reveals the truth). (7)")
    ("DharmaMitra" . "In this matter, whatever unfettered expression may emerge, even though my own voice is powerless, that is the unfolding of the glorious Great Bliss of the Supreme Lord."))
  "The reference translations of sentence 1007 as sent with the request.")

(ert-deftest bcat-analysis-sanskrit-golden-1007 ()
  "Claude's answer for TĀ 1007 (Sanskrit contract) → template fields →
the Sanskrit template = the reviewed sample, character for character:
padapāṭha with vocabulary under * Text, the tables split by width,
Sandhi, Forms and Compounds per segment, a „→“ line per translator."
  (let ((bcat-template-directory
         (expand-file-name "Templates/analysis" bcat-analysis-test--root))
        (expected (with-temp-buffer
                    (insert-file-contents
                     (expand-file-name "Templates/analysis/samples/Sanskrit.org"
                                       bcat-analysis-test--root))
                    (buffer-string))))
    (should (equal expected
                   (bcat-template-render-named
                    "sanskrit"
                    (bcat-analysis-sanskrit-context (bcat-analysis-test--base-1007)
                                                    (bcat-analysis-parse (bcat-analysis-test--answer-1007))
                                                    bcat-analysis-test--refs-1007))))))

(ert-deftest bcat-analysis-sanskrit-prompts ()
  "Sanskrit system prompt: the JSON contract (padapāṭha words with
formation, parts, gloss, form; sandhi, forms, compounds), the
Devavāṇīpraveśikā terminology (prathama puruṣa = 3rd person), the
escaping rule, the target language; user prompt: context, every
segment as transmitted, the references."
  (let ((sys (bcat-analysis-sanskrit-system-prompt "en"))
        (user (bcat-analysis-sanskrit-user-prompt
               1007 nil
               '((685 "atra madvāgaśaktāpi yanniyantraṇamullaset /")
                 (686 "tatpārameśvaraṃ śrīmanmahānandavijṛmbhitam // 7"))
               '(("Dyczkowski 2023" . "Although my speech"))
               '("Abhinavagupta, Tantrāloka"))))
    (dolist (key '("\"words\"" "\"lemma\"" "\"formation\"" "\"parts\"" "\"gloss\"" "\"form\""
                   "\"sandhi\"" "\"forms\"" "\"compounds\"" "\"shift\"" "\"structure\""
                   "\"grammar\"" "\"units\""))
      (should (string-match-p (regexp-quote key) sys)))
    (should (string-match-p "Devavāṇīpraveśikā" sys))
    (should (string-match-p "prathama puruṣa is the 3rd person" sys))
    (should (string-match-p "madvāgaśaktāpi = mad-vāk aśaktā api" sys))
    (should (string-match-p "never a backslash before an apostrophe" sys))
    (should (string-match-p "not its notes" sys))
    (should-not (string-match-p "in full" sys))
    ;; 29.10., TĀ 1007 (first real answer): glosses of 60 characters with
    ;; comments split a segment into five tables; a „Dyczkowski 2023: []“
    ;; line; a sandhi said to be written apart that the text joins.
    (should (string-match-p "at most 30 characters" sys))
    (should (string-match-p "never a line for a translator without marks" sys))
    (should (string-match-p "as it stands in the transmitted text" sys))
    (should (string-match-p "in English" sys))
    (should (string-match-p "in German" (bcat-analysis-sanskrit-system-prompt "de")))
    (should (string-match-p "\\`Abhinavagupta, Tantrāloka" user))
    (should (string-match-p (regexp-quote "[685] atra madvāgaśaktāpi yanniyantraṇamullaset /\n[686] tatpārameśvaraṃ") user))
    (should (string-match-p "=== Dyczkowski 2023 ===\nAlthough my speech" user))))

(ert-deftest bcat-analysis-uchen-words ()
  "Carsten 31.10. abends, Tibetisch V: the Uchen of each word above its
Wylie in the tables — from the segment's Uchen, syllable by syllable
against Claude's Wylie (bDen 9–20: 216 words, all found).  A word that
does not match gets nil, the next ones still theirs."
  (should (equal '("དེ" "ལས" "བརྒྱུད་པའི" "མན་ངག" "གིས")
                 (bcat-analysis-uchen-words "དེ་ལས་བརྒྱུད་པའི་མན་ངག་གིས། །"
                                            '("de" "las" "brgyud pa'i" "man ngag" "gis"))))
  ;; A word spelled otherwise („lXX“): the next word is found further on
  ;; and the syllables skipped are the odd word's.
  (should (equal '("དེ" "ལས" "བརྒྱུད་པའི")
                 (bcat-analysis-uchen-words "དེ་ལས་བརྒྱུད་པའི། །" '("de" "lXX" "brgyud pa'i"))))
  (should (equal '("དེ" nil) (bcat-analysis-uchen-words "དེ་ལས། །" '("de" "foo"))))
  (should (equal '(nil) (bcat-analysis-uchen-words nil '("de")))))

(ert-deftest bcat-analysis-gloss-table-uchen ()
  "With the segment's Uchen the table has four rows: Uchen, Wylie (the
dictionary link), gloss, function; without, three as before."
  (let ((words '(((wylie . "de") (gloss . "that") (function . "DEM"))
                 ((wylie . "las") (gloss . "from") (function . "ABL")))))
    (should (equal '(("དེ" "ལས") ("[[steinert:de][de]]" "las") ("that" "from") ("DEM" "ABL"))
                   (mapcar (lambda (row) (mapcar #'string-trim (butlast (cdr (split-string row "|")))))
                           (split-string (bcat-analysis-gloss-table words "དེ་ལས། །") "\n"))))
    (should (= 3 (length (split-string (bcat-analysis-gloss-table words) "\n"))))))

(ert-deftest bcat-analysis-uchen-table-bands ()
  "An older table in bands (SDA 11: the fifth word under an hline, the
rest of its band empty) gets the Uchen row too: its columns read band
after band, the empty padding dropped, set again."
  (let* ((old (concat "| [[steinert:mu stegs][mu stegs]] | kyis |\n"
                      "| tirthika | ERG-by |\n"
                      "| N | ERG |\n"
                      "|----------+------|\n"
                      "| [[steinert:kyang][kyang]] |   |\n"
                      "| even | |\n"
                      "| CONJ | |"))
         (new (bcat-analysis-uchen-table old "མུ་སྟེགས་ཀྱིས་ཀྱང་། །"))
         (rows (mapcar (lambda (l) (mapcar #'string-trim (butlast (cdr (split-string l "|")))))
                       (cl-remove-if (lambda (l) (string-match-p "\\`|-" l)) (split-string new "\n")))))
    (should (equal '("མུ་སྟེགས" "ཀྱིས" "ཀྱང") (car rows)))
    (should (equal '("[[steinert:mu stegs][mu stegs]]" "kyis" "[[steinert:kyang][kyang]]") (nth 1 rows)))
    (should (equal '("N" "ERG" "CONJ") (nth 3 rows)))))

(ert-deftest bcat-analysis-comparison-footnotes-last ()
  "Carsten 1.11., SDA 10: „Dharma Mitra und Apple [sind] mit in die
Fußnote gewandert“ — Lindtner's note comes as an org footnote
definition inside his reference; a definition runs on to the next
heading, definition or two blank lines, so it swallowed Apple,
DharmaMitra and the comparison.  The definitions go to the end of
Translations compared, the marker stays in the text."
  (let* ((out (bcat-analysis-comparison
               '((translation . "Own.") (comparison . "All three agree.") (segments))
               '(("Lindtner 1981" . "Perception and inference. [fn:l10]\n\n[fn:l10] Lindtner 1981, Anm. 10: a note.")
                 ("Apple 2019" . "Direct perception and inference.")
                 ("DharmaMitra" . "Direct perception.")))))
    (should (equal (concat "*Translation:* Own.\n\n"
                           "*Lindtner 1981:* Perception and inference. [fn:l10]\n\n"
                           "*Apple 2019:* Direct perception and inference.\n\n"
                           "*DharmaMitra:* Direct perception.\n\n"
                           "All three agree.\n\n"
                           "[fn:l10] Lindtner 1981, Anm. 10: a note.")
                   out))
    (should (equal "A [fn:x]\n\nB\n\n[fn:x] n.\n\n[fn:y] m."
                   (bcat-analysis-footnotes-last "A [fn:x]\n\n[fn:x] n.\n\nB\n\n[fn:y] m.")))))

(ert-deftest bcat-analysis-second-language ()
  "Carsten 31.10. abends, TĀ 1006: the sentence translation in the
other language (*Deutsch:* for English, *English:* for German) „damit
ich mich leichter in die Konzepte rein denken kann“; 1.11.: „nicht als
default …, sondern nur als Option (wahrscheinlich sogar auf
Satzebene)“.  The system prompts no longer ask for it (they stay the
same per document — the cache); the user prompt of a sentence with the
option does (translation_de for English, translation_en for German) —
and asks the comparison to name each reading in that language too
\(1006: „Bei der Unterscheidung auch was das auf Deutsch heißen würde“;
1.11.: „ja, bitte auch nur auf Wunsch“), as does the small request for a
reference added by hand.  The field lands under the translation."
  (dolist (sys (list (bcat-analysis-sanskrit-system-prompt "en")
                     (bcat-analysis-system-prompt "en")
                     (bcat-analysis-tibetan-system-prompt "en")))
    (should-not (string-match-p "translation_de" sys))
    (should-not (string-match-p "what each reading would be" sys)))
  (dolist (sys (list (bcat-analysis-sanskrit-system-prompt "de")
                     (bcat-analysis-system-prompt "de")))
    (should-not (string-match-p "translation_en\\|what each reading would be" sys)))
  (should-not (string-match-p "what each reading would be" (bcat-analysis-align-system-prompt "en")))
  (should (string-match-p "what each reading would be in German"
                          (bcat-analysis-align-user-prompt 9 nil '((38 "ཀ" "ka" "t")) '(("R" . "r")) nil "en")))
  (should-not (string-match-p "what each reading would be"
                              (bcat-analysis-align-user-prompt 9 nil '((38 "ཀ" "ka" "t")) '(("R" . "r")))))
  (let ((segs '((685 "atra madvāgaśaktāpi yanniyantraṇamullaset /"))))
    (should-not (string-match-p "translation_de" (bcat-analysis-sanskrit-user-prompt 1007 nil segs nil)))
    (should (string-match-p "\"translation_de\": the sentence translation once more in German"
                            (bcat-analysis-sanskrit-user-prompt 1007 nil segs nil nil nil "en")))
    (should (string-match-p "\"translation_en\": the sentence translation once more in English"
                            (bcat-analysis-sanskrit-user-prompt 1007 nil segs nil nil nil "de")))
    (should (string-match-p "what each reading would be in German"
                            (bcat-analysis-sanskrit-user-prompt 1007 nil segs nil nil nil "en")))
    (should-not (string-match-p "what each reading would be"
                                (bcat-analysis-sanskrit-user-prompt 1007 nil segs nil)))
    (should (string-match-p "translation_de"
                            (bcat-analysis-user-prompt 9 nil '((38 "ཀ" "ka")) nil nil "en")))
    (should (string-match-p "translation_de"
                            (bcat-analysis-tibetan-user-prompt 3 nil '((22 "x" "t")) '((77 "ཀ" "ka")) nil nil "en")))
    (should (string-match-p "translation_de.*\n\nReturn the JSON object now\\.\\'"
                            (bcat-analysis-sanskrit-user-prompt 1007 nil segs nil nil nil "en"))))
  (let ((ctx (bcat-analysis-sanskrit-context
              (bcat-analysis-test--base-1007)
              (bcat-analysis-parse
               (replace-regexp-in-string
                "\"comparison\":" "\"translation_de\": \"Obwohl meine Rede hier unfähig ist …\", \"comparison\":"
                (bcat-analysis-test--answer-1007) t t))
              bcat-analysis-test--refs-1007))
        (bcat-template-directory (expand-file-name "Templates/analysis" bcat-analysis-test--root)))
    (should (equal "Deutsch" (alist-get 'second-label ctx)))
    (should (string-match-p
             (regexp-quote "glorious great bliss (mahānanda).\n\n*Deutsch:* Obwohl meine Rede hier unfähig ist …\n\n* Sentence\n")
             (bcat-template-render-named "sanskrit" ctx))))
  (let ((ctx (bcat-analysis-context (bcat-analysis-test--base)
                                    '((segments) (translation . "t") (translation_en . "In English.")))))
    (should (equal '("English" . "In English.")
                   (cons (alist-get 'second-label ctx) (alist-get 'second-translation ctx))))))

(ert-deftest bcat-analysis-shift-lines-skip-empty ()
  "29.10., TĀ 1007 (first real Sanskrit answer): Claude listed the
translator without marks as \"Dyczkowski 2023: []\" and the file showed
„→ Dyczkowski 2023: []“.  A shift line with nothing after the name
\(empty, [] or \"\") is dropped; real lines stay."
  (should (equal '("→ DharmaMitra: niyantraṇa reversed")
                 (bcat-analysis-shift-lines
                  '((shift "Dyczkowski 2023: []" "DharmaMitra: niyantraṇa reversed"
                           "Apple 2019:" "Lindtner 1981: \"\"" "")))))
  (should (equal '("→ Lindtner 1981: active, with the yogin as agent")
                 (bcat-analysis-shift-lines
                  '((shift . "Lindtner 1981: active, with the yogin as agent")))))
  (should-not (bcat-analysis-shift-lines '((shift . "")))))

(ert-deftest bcat-analysis-prose-recitation ()
  "31.10., TĀ Satz 1008 (Jayaratha's prose): „hat keine Rezitation“ — a
prose sentence has no metre, so the recitation was the text as one
long line.  Carsten chose sense units: for the Sanskrit template the
request asks for `recitation' — the transmitted text unchanged, in lines
of sense units, ¦ for a short breath pause —; the Recitation shows each
line in Devanāgarī over IAST under a short header.  Taken only when the
lines are the text (breaks at its spaces); a verse keeps its metre; the
prompt of the other templates stays as it was."
  (should (string-match-p "\"recitation\"" (bcat-analysis-sanskrit-system-prompt "en" nil t)))
  (should-not (string-match-p "recitation" (bcat-analysis-sanskrit-system-prompt "en")))
  (let* ((text "iha khalu śāstrādāvalaukikāśīrvādamukhena vakṣyamāṇaṣaḍardhaśāstrārthagarbhīkāreṇa samuciteṣṭadevatāṃ śāstrakāraḥ parāmṛśati")
         (base `((recitation . "PLAIN")
                 (segments ((num . 687) (samhita . ,text)))))
         (lines '("iha khalu ¦ śāstrādāvalaukikāśīrvādamukhena"
                  "vakṣyamāṇaṣaḍardhaśāstrārthagarbhīkāreṇa"
                  "samuciteṣṭadevatāṃ ¦ śāstrakāraḥ parāmṛśati"))
         (ctx (bcat-analysis-sanskrit-context base `((segments) (recitation . ,lines)))))
    (should (string-match-p (regexp-quote "\niha khalu ¦ śāstrādāvalaukikāśīrvādamukhena\n")
                            (alist-get 'recitation ctx)))
    (should (string-match-p (regexp-quote "इह खलु ¦ शास्त्रादावलौकिकाशीर्वादमुखेन")
                            (alist-get 'recitation ctx)))
    (should (= 6 (length (split-string (alist-get 'recitation ctx) "\n"))))
    (should (string-match-p "\\`Prosa" (alist-get 'metre ctx)))
    ;; Not the text (a word changed, or a break inside a word): as it was.
    (should (equal "PLAIN" (alist-get 'recitation
                                      (bcat-analysis-sanskrit-context
                                       base `((segments) (recitation "iha khalu" "śāstrādāv alaukika"))))))
    ;; A verse (its scansion in the rump): the metre's recitation stays.
    (should (equal "PLAIN" (alist-get 'recitation
                                      (bcat-analysis-sanskrit-context
                                       (cons '(scansion . "| – |") base)
                                       `((segments) (recitation . ,lines))))))))

(ert-deftest bcat-analysis-sanskrit-tables-wrap-long-cell ()
  "31.10., TĀ Satz 1008: the parts of „vakṣyamāṇa-ṣaḍardha-śāstra-artha-
garbhīkāreṇa“ (160 characters) made a one-column table wider than the
page; split by columns there was nothing to split, and the PDF shrank
the table to an unreadable size (1f6da5b).  5.11.: the parts have a
table of their own, a column per member — no line of the overview or
of the parts table wider than the table width, every part's meaning
there."
  (let* ((parts "√vac (2) speak · -ṣyamāṇa fut. pass. part. · ṣaḍ six · ardha half · śāstra treatise · artha meaning · garbhī- womb/containing · √kṛ (8) make · -a action noun")
         (words `(((word . "vakṣyamāṇa-ṣaḍardha-śāstra-artha-garbhīkāreṇa")
                   (formation . "√vac · ṣaḍ-ardha · śāstra · artha · garbhī-√kṛ-a")
                   (parts . ,parts)
                   (gloss . "by incorporating the meaning of the Ṣaḍardhaśāstra to be expounded")
                   (form . "m. inst. sg."))))
         (out (concat (bcat-analysis-sanskrit-tables words) "\n\n" (bcat-analysis-formation-tables words))))
    (dolist (line (split-string out "\n" t))
      (should (<= (string-width line) bcat-analysis-table-width)))
    (should (equal '("vakṣyamāṇa" "ṣaḍardha" "śāstra" "artha" "garbhīkāreṇa")
                   (car (bcat-analysis-test--rows (bcat-analysis-formation-tables words)))))
    (dolist (part (split-string parts " · "))
      (should (string-search (cdr (bcat-analysis--part-item part)) out)))))

(ert-deftest bcat-analysis-sanskrit-tables ()
  "The overview of a Sanskrit segment (word / gloss / form — 5.11.,
the formation and parts rows gone) is split into separate tables, none wider than
the table width (`bcat-analysis-table-width', 5.11. every language) — a
shared column grid made the long compound widen every column (29.10.);
nothing is cut."
  (let* ((bcat-analysis-table-width 30)
         (words '(((word . "atra") (formation . "—") (parts . "") (gloss . "here") (form . "ind."))
                  ((word . "ullaset") (formation . "ud-√las") (parts . "ud- up · √las (1) shine")
                   (gloss . "may shine forth") (form . "opt. 3 sg. P"))
                  ((word . "api") (formation . "—") (parts . "") (gloss . "although") (form . "ind."))))
         (out (bcat-analysis-sanskrit-tables words))
         (tables (split-string out "\n\n")))
    (should (= 2 (length tables)))
    (should (string-prefix-p "| atra | ullaset" (nth 0 tables)))
    (should (string-prefix-p "| api" (nth 1 tables)))
    ;; 5.11.: the parts are not in the overview — they have their own
    ;; table (9.11.: of one column, not a line).
    (should-not (string-match-p "√las" out))
    (should (equal "| ullaset          |\n|------------------|\n| /ud-/ up         |\n| /√las/ (1) shine |"
                   (bcat-analysis-formation-tables words)))
    (dolist (tb tables)
      (dolist (row (split-string tb "\n"))
        (should (<= (string-width row) 30))))
    (should (equal "" (bcat-analysis-sanskrit-tables nil)))))

(ert-deftest bcat-analysis-sanskrit-table-width-default ()
  "Carsten 30.10.: the tables as in the reviewed preview of AS sentence
3 — compound and upādāya side by side.  Measured in its PDF: a table of
115 characters (segment 24) fits the page, one of 134 (segment 27) runs
into the margin.  5.11.: in the file (the width of every language, wide
cells broken) segments 23, 24 and 27 keep one table each; the PDF
chunks anew (`bcat-export-rechunk-gloss-tables')."
  (let ((segments (alist-get 'segments
                             (bcat-analysis-parse (bcat-analysis-test--answer-003 "asam")))))
    (cl-flet ((tables (n) (length (split-string
                                   (bcat-analysis-sanskrit-tables
                                    (alist-get 'words (cl-find n segments
                                                               :key (lambda (s) (alist-get 'num s)))))
                                   "\n\n"))))
      (should (= 1 (tables 23)))
      (should (= 1 (tables 24)))
      (should (= 1 (tables 27))))))

(ert-deftest bcat-analysis-padapatha-line ()
  "The padapāṭha line under * Text: each word with its gloss in
brackets, the daṇḍas of the transmitted line at the end (the verse
number stays with the transmitted line)."
  (should (equal "atra [here] mad-vāk [my speech] api /"
                 (bcat-analysis-padapatha-line
                  "atra madvāgapi /"
                  '(((word . "atra") (gloss . "here")) ((word . "mad-vāk") (gloss . "my speech"))
                    ((word . "api") (gloss . ""))))))
  (should (equal "tat [that] //"
                 (bcat-analysis-padapatha-line "tat // 7" '(((word . "tat") (gloss . "that"))))))
  (should (equal "iha [here]"
                 (bcat-analysis-padapatha-line "iha" '(((word . "iha") (gloss . "here"))))))
  ;; PLAIN (the Text in pure IAST, 29.10.): no glosses, daṇḍas AND verse number
  (should (equal "tat pārameśvaram // 7"
                 (bcat-analysis-padapatha-line
                  "tatpārameśvaraṃ // 7"
                  '(((word . "tat") (gloss . "that")) ((word . "pārameśvaram") (gloss . "the Lord's")))
                  t)))
  (should-not (bcat-analysis-padapatha-line "tat // 7" nil))
  ;; 30.10., AS sentence 3: the AS edition writes the daṇḍa as a full
  ;; stop; it closes the padapāṭha like `/' — set close, as in the text.
  (let ((words '(((word . "pañca") (gloss . "five")) ((word . "eva") (gloss . "only"))
                 ((word . "skandhāḥ") (gloss . "aggregates")))))
    (should (equal "pañca eva skandhāḥ."
                   (bcat-analysis-padapatha-line "pañcaiva skandhāḥ." words t)))
    (should (equal "pañca [five] eva [only] skandhāḥ [aggregates]."
                   (bcat-analysis-padapatha-line "pañcaiva skandhāḥ." words)))))

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

(ert-deftest bcat-analysis-read-reports-truncation ()
  "30.10., AS sentence 2: the answer ended after 41 283 characters in the
middle of „grammar“ — Claude had reached the output limit.  C-c u G
reported „JSON-Fehler bei Zeichen 40932“, which says nothing about the
cause.  An answer whose object never closes is reported as cut off at
the token limit; a complete but broken one keeps its JSON error."
  (let ((cut (concat "{\"segments\": [{\"num\": 15, \"words\": []},"
                     " {\"num\": 16, \"words\": [{\"word\": \"pañca\"}]}],"
                     " \"grammar\": \"Sentence 2 is a complete catechetical unit {sic} with")))
    (should-not (car (bcat-analysis-read cut)))
    (should (string-match-p "abgeschnitten" (cdr (bcat-analysis-read cut))))
    (should (string-match-p "tibetan-claude-max-tokens" (cdr (bcat-analysis-read cut)))))
  (should (string-match-p "JSON-Fehler"
                          (cdr (bcat-analysis-read "{\"segments\": [1 2], \"grammar\": \"g\"}")))))

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

(ert-deftest bcat-analysis-parse-repairs-invalid-escape ()
  "29.10., bDen 9 (kept answer): in the structure line Claude escaped the
apostrophe of the genitive — /shes bya/ =\\='i= [GEN] — and that is
no JSON escape, so the whole answer was lost.  A backslash before a
character JSON does not escape is dropped; valid escapes stay."
  (let ((parsed (bcat-analysis-parse
                 (concat "```json\n{\"segments\": [{\"num\": 41, \"structure\": "
                         "\"/shes bya/ =\\'i= [GEN] \\u00b7 \\\"x\\\" a\\nb\"}]}\n```"))))
    (should parsed)
    (should (equal "/shes bya/ ='i= [GEN] · \"x\" a\nb"
                   (alist-get 'structure (car (alist-get 'segments parsed)))))))

(ert-deftest bcat-analysis-parse-repairs-quote-before-bracket ()
  "30.10., bDen 16 (kept answer, „JSON-Fehler bei Zeichen 988“): in the
structure line Claude escaped the opening quote of a gloss but not the
closing one, and a bracket followed — =\\='gyur= [TERM + auxiliary,
\\\"come to realize\"].  A quote before `]' ended the string, though the
string is a value in an object, where `]' cannot close anything.
Whether a quote can end a string depends on where the string stands: a
key before `:', a value in an object before `}' or `,', an element of
an array before `]' or `,'."
  (let ((parsed (bcat-analysis-parse
                 (concat "{\"segments\": [{\"num\": 66, "
                         "\"shift\": \"Apple 2019: \"from them\" (plural) — singular.\", "
                         "\"structure\": \"/rtogs par/ ='gyur= [TERM + auxiliary → future, "
                         "\\\"come to realize\"] → main clause of 16ab\",\n"
                         "\"note\": \"n\", \"words\": [\"a\", \"b\"]}]}"))))
    (should parsed)
    (let ((seg (car (alist-get 'segments parsed))))
      (should (equal "/rtogs par/ ='gyur= [TERM + auxiliary → future, \"come to realize\"] → main clause of 16ab"
                     (alist-get 'structure seg)))
      (should (equal "Apple 2019: \"from them\" (plural) — singular." (alist-get 'shift seg)))
      (should (equal "n" (alist-get 'note seg)))
      (should (equal '("a" "b") (alist-get 'words seg))))))

(ert-deftest bcat-analysis-parse-repairs-missing-key ()
  "30.10., MSA IX.60 (Tibetan answer of sent-128, „JSON-Fehler bei
Zeichen 6210“): one reference came without its key —
{\"name\": \"DharmaMitra\", \"*The first is the support for the second.*\"}.
A string where a key belongs that is followed by `}' or `,' is a value
without key; in our answers the only bare text is a reference's, so it
becomes \"text\"."
  (let ((parsed (bcat-analysis-parse
                 (concat "{\"segments\": [{\"num\": 462, \"references\": ["
                         "{\"name\": \"Radich 2007\", \"text\": \"the first is the basis\"},\n"
                         "        {\"name\": \"DharmaMitra\", \"*The first is the support for the second.*\"}\n"
                         "      ], \"shift\": \"DharmaMitra: \\\"the second\\\"\"}], \"grammar\": \"g\"}"))))
    (should parsed)
    (let ((seg (car (alist-get 'segments parsed))))
      (should (equal '("the first is the basis" "*The first is the support for the second.*")
                     (mapcar (lambda (r) (alist-get 'text r)) (alist-get 'references seg))))
      (should (equal "DharmaMitra: \"the second\"" (alist-get 'shift seg))))
    (should (equal "g" (alist-get 'grammar parsed)))))

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

(ert-deftest bcat-analysis-no-links-keeps-compound-brackets ()
  "30.10.: 8646ec1 made every doubled bracket single — and would have
flattened Claude's bracketing of Sanskrit compounds (TĀ sent-209, AS
sent-3, TĀ sent-1015), which org never reads as a link.  Only what org
takes for a link loses its doubled brackets."
  (dolist (s '("parsed as [[tad-upāsā]-antika]-dūrataḥ."
               "tatpuruṣa: [[[pañca-ākāra] ātmavastu] udbhāvanatā]-tām."
               "[[[[kula + akula] + ubhaya]-cchaṭā]-ātmaka] + [hakāra + [ardha + ardha]-rūpa]]"))
    (should (equal s (bcat-analysis-no-links s))))
  (should (equal "ni [topic] chos" (bcat-analysis-no-links "ni [[topic]] chos")))
  (should (equal "see [x][y] here" (bcat-analysis-no-links "see [[x][y]] here"))))

(ert-deftest bcat-analysis-answers-carry-no-links ()
  "Carsten 30.10.: „broken links sollten eigentlich nicht entstehen
können“.  No text from outside — Claude's answer, DharmaMitra's
translation — can form an org link in a file: every string of a parsed
answer that org would read as a link loses its doubled brackets (IX.60:
„[[topic]]“ aborted the export); single brackets stay."
  (let* ((parsed (bcat-analysis-parse
                  (concat "{\"segments\": [{\"num\": 1, \"words\": [{\"wylie\": \"ni\", \"gloss\": \"[[topic]]\"}],"
                          " \"note\": \"see [[x][y]] and [the other] two\","
                          " \"references\": [{\"name\": \"M\", \"text\": \"the [[other]] two.]]\"}]}],"
                          " \"grammar\": \"[[g]]\"}")))
         (strings '()))
    (should parsed)
    (cl-labels ((walk (x) (cond ((stringp x) (push x strings))
                                ((consp x) (walk (car x)) (walk (cdr x))))))
      (walk parsed))
    (should-not (cl-some (lambda (s) (string-match-p org-link-bracket-re s)) strings))
    (should (member "see [x][y] and [the other] two" strings))
    (should (member "[topic]" strings)))
  (should (equal "the [other] two." (bcat-analysis-no-links "the [[other]] two.")))
  (cl-letf (((symbol-function 'tibetan-dharmamitra-api-chat-translate)
             (lambda (&rest _) "The first [[body]] is the support."))
            ((symbol-function 'tibetan-dharmamitra-translation--target-lang) (lambda (&rest _) "english"))
            ((symbol-function 'tibetan-dharmamitra-translation--input-encoding) (lambda (&rest _) "auto")))
    (should (equal "The first [body] is the support."
                   (bcat-sentence--dharmamitra "/nonexistent/sent-001.org"
                                               '(:sent-num 1 :segs ((1 . "x"))))))))

(ert-deftest bcat-analysis-glossed-line-never-a-link ()
  "30.10., MSA IX.60 (Sthiramati 1783): Claude glossed the particle /ni/
as \"[topic]\" — brackets included — and the verse line read
„ni [[topic]]“, an org link; the export of the commentary aborted
(„Unable to resolve link: \"topic\"“) and no PDF came.  A gloss in
its own brackets loses them; brackets inside a gloss become
parentheses — in the Tibetan and in the Sanskrit line."
  (let ((line (bcat-analysis-glossed-line
               "rang bzhin ni chos/"
               '(((wylie . "rang bzhin") (gloss . "[nature]") (function . "N"))
                 ((wylie . "ni") (gloss . "[topic]") (function . "TOP"))
                 ((wylie . "chos") (gloss . "the [other] two") (function . "N"))))))
    (should (equal "rang bzhin [nature] ni [topic] chos [the (other) two] /" line))
    (should-not (string-match-p "\\[\\[\\|\\]\\]" line)))
  (should (equal "atra [here] /"
                 (bcat-analysis-padapatha-line "atra /" '(((word . "atra") (gloss . "[here]")))))))

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
                   "\"structure\"" "\"note\"" "\"concepts\"" "\"grammar\"" "\"units\""))
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

(ert-deftest bcat-analysis-word-with-case-function ()
  "29.10., bDen 9 seg. 41: Claude gave the word „shes bya'i“ (the
knowable + genitive clitic) the function „GEN · second member of
compound“.  It was taken for a particle: no dictionary link in the
table, and the verse showed the label instead of its gloss.  Only a
single syllable can be a particle; a word of several syllables keeps
its link and its gloss whatever its function begins with."
  (let ((words '(((wylie . "nyon mongs") (gloss . "afflictions") (function . "N · first member"))
                 ((wylie . "shes bya'i") (gloss . "of the knowable")
                  (function . "GEN · second member of compound"))
                 ((wylie . "sgrib pa") (gloss . "obscurations") (function . "N · object, ABS"))
                 ((wylie . "spangs") (gloss . "abandoned") (function . "V pf. (spong ba)")))))
    (should (string-match-p (regexp-quote "[[steinert:shes bya'i][shes bya'i]]")
                            (bcat-analysis-gloss-table words)))
    (should (equal (concat "nyon mongs [afflictions] shes bya'i [of the knowable] "
                           "sgrib pa [obscurations] spangs [abandoned] / /")
                   (bcat-analysis-glossed-line "nyon mongs shes bya'i sgrib pa spangs/ /" words)))))

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

(ert-deftest bcat-analysis-gloss-table-split ()
  "30.10., Sthiramati's prose: a segment of 23 words gave one table that
ran far into the margin of the PDF.  The words go into several tables,
in order, none wider than `bcat-analysis-tibetan-table-width' (since
4.11. as seen in the file: each column its widest cell + 3, up to 158;
the PDF chunks anew); a short segment stays one table."
  (let* ((words (cl-loop for i from 1 to 23
                         collect `((wylie . ,(format "rnam par shes %02d" i))
                                   (gloss . "consciousness")
                                   (function . "N · ABS"))))
         (table (bcat-analysis-gloss-table words))
         (tables (split-string table "\n\n")))
    ;; 3 + 16 = 19 per column: eight columns (153) fit in 158, nine (172) do not.
    (should (= 3 (length tables)))
    (should (equal '(8 8 7)
                   (mapcar (lambda (tb) (1- (length (split-string tb "steinert:")))) tables)))
    (should (equal (cl-loop for i from 1 to 23 collect (format "rnam par shes %02d" i))
                   (let ((pos 0) (out '()))
                     (while (string-match "\\]\\[\\([^]]+\\)\\]\\]" table pos)
                       (push (match-string 1 table) out)
                       (setq pos (match-end 0)))
                     (nreverse out))))
    (should (= 1 (length (split-string (bcat-analysis-gloss-table (last words 3)) "\n\n"))))))

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

;; ----------------------------------------------------------------------------
;; Bilingual — Sanskrit with the Tibetan translation (§5.74, Carsten 30.10.)
;; ----------------------------------------------------------------------------

(defun bcat-analysis-test--answer-003 (name)
  "Claude's answer NAME (\"asam\": the Sanskrit request, \"kunbtus\": the
Tibetan one) for AS sentence 3, drawn from the reviewed sample."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name (format "test/fixtures/bcat-analysis-sent-003-%s.json" name)
                       bcat-analysis-test--root))
    (buffer-string)))

(defun bcat-analysis-test--base-003 ()
  "The rump of AS sentence 3: header, the Sanskrit segments as
transmitted with their Devanāgarī, the Tibetan segments (Uchen, Wylie)."
  `((sent-num . "3")
    (source-link . "[[file:../asam-1-cat.org::*Sentence 3][asam-1-cat.org / Sentence 3]]")
    (parallel-link . "[[file:../../Tibetisch/analysis/sent-003-kunbtus.org][sent-003-kunbtus.org]]")
    (segment-list . "22, 23, 24, 25, 26, 27, 28")
    (parallel-segment-list . "77, 78, 79, 80, 81, 82, 83")
    (hash . "a845778a9a58bcb25a9de0b4f56596dd")
    (created . "2026-10-19")
    (last-analyzed . "2026-10-30")
    (tibetan-p . t)
    (segments
     . ,(cl-mapcar (lambda (n sa deva) `((num . ,n) (samhita . ,sa) (devanagari . ,deva)))
                   '(22 23 24 25 26 27 28)
                   '("kasmāt pañcaiva skandhāḥ."
                     "pañcākārātmavastūdbhāvanatām upādāya."
                     "saparigrahadehātmavastūdbhāvanatām upādāya."
                     "upabhogātmavastūdbhāvanatām upādāya."
                     "vyavahārātmavastūdbhāvanatām upādāya."
                     "dharmādharmābhisaṃskārātmavastūdbhāvanatām upādāya."
                     "tadāśrayātmavastūdbhāvanatām upādāya.")
                   '("कस्मात् पञ्चैव स्कन्धाः।"
                     "पञ्चाकारात्मवस्तूद्भावनताम् उपादाय।"
                     "सपरिग्रहदेहात्मवस्तूद्भावनताम् उपादाय।"
                     "उपभोगात्मवस्तूद्भावनताम् उपादाय।"
                     "व्यवहारात्मवस्तूद्भावनताम् उपादाय।"
                     "धर्माधर्माभिसंस्कारात्मवस्तूद्भावनताम् उपादाय।"
                     "तदाश्रयात्मवस्तूद्भावनताम् उपादाय।")))
    (tibetan
     . ,(cl-mapcar (lambda (n uchen wylie) `((num . ,n) (uchen . ,uchen) (wylie . ,wylie)))
                   '(77 78 79 80 81 82 83)
                   '("ཅིའི་ཕྱིར་ཕུང་པོ་རྣམས་ལྔ་ཁོ་ན་ཞེ་ན།"
                     "བདག་གི་གཞི་རྣམ་པ་ལྔ་བརྗོད་པའི་ཕྱིར་ཏེ།"
                     "བདག་གི་གཞི་ལུས་ཡོངས་སུ་གཟུང་བ་དང་བཅས་པ་བརྗོད་པ་དང་།"
                     "བདག་གི་གཞི་ལོངས་སྤྱོད་བརྗོད་པ་དང་།"
                     "བདག་གི་གཞི་ཐ་སྙད་བརྗོད་པ་དང་།"
                     "བདག་གི་གཞི་ཆོས་དང་ཆོས་མ་ཡིན་པ་མངོན་པར་འདུ་བྱེད་པ་བརྗོད་པ་དང་།"
                     "བདག་གི་གཞི་དེ་དག་གི་གནས་བརྗོད་པའི་ཕྱིར་རོ། །")
                   '("ci'i phyir phung po rnams lnga kho na zhe na/"
                     "bdag gi gzhi rnam pa lnga brjod pa'i phyir te/"
                     "bdag gi gzhi lus yongs su gzung ba dang bcas pa brjod pa dang /"
                     "bdag gi gzhi longs spyod brjod pa dang /"
                     "bdag gi gzhi tha snyad brjod pa dang /"
                     "bdag gi gzhi chos dang chos ma yin pa mngon par 'du byed pa brjod pa dang /"
                     "bdag gi gzhi de dag gi gnas brjod pa'i phyir ro/ /")))))

(defconst bcat-analysis-test--refs-003
  '(("Rahula/Boin-Webb 2001" . "Why are there only five aggregates? Because of the five methods by means of which [the idea] of self (ātman) makes its appearance: self as physical apprehension, self as experience, self as expression, self as the agent of all right and all wrong, and self as the basis of all that.")
    ("DharmaMitra" . "Why are there exactly five aggregates (skandhas)?
Because they serve to manifest the objective basis of the self (ātmavastu) in five ways:
1. By manifesting the objective basis of the self as the body together with its possessions;
2. By manifesting the objective basis of the self as the object of experience;
3. By manifesting the objective basis of the self as the ground for conventional designation (vyavahāra);
4. By manifesting the objective basis of the self as the formative activity (abhisaṃskāra) of merit and demerit (dharma-adharma);
5. By manifesting the objective basis of the self as the support (āśraya) for those [preceding aggregates]."))
  "The reference translations of AS sentence 3 as sent with the request.")

(ert-deftest bcat-analysis-bilingual-golden-003 ()
  "Carsten 30.10.: AS sentence 3 in both languages — the Sanskrit answer
and the Tibetan answer through the field builders into the bilingual
template = the reviewed sample, character for character: Devanāgarī
over IAST, padapāṭha, Uchen over Wylie under * Text; per Sanskrit
segment its Tibetan segment(s) in the heading and as Uchen over glossed
Wylie, the differences only where there are some, then the
translations; the Tibetan rendering of the whole sentence."
  (let ((bcat-template-directory
         (expand-file-name "Templates/analysis" bcat-analysis-test--root))
        (expected (with-temp-buffer
                    (insert-file-contents
                     (expand-file-name "Templates/analysis/samples/Bilingual.org"
                                       bcat-analysis-test--root))
                    (buffer-string))))
    (should (equal expected
                   (bcat-template-render-named
                    "bilingual"
                    (bcat-analysis-bilingual-context
                     (bcat-analysis-sanskrit-context
                      (bcat-analysis-test--base-003)
                      (bcat-analysis-parse (bcat-analysis-test--answer-003 "asam"))
                      bcat-analysis-test--refs-003)
                     (bcat-analysis-parse (bcat-analysis-test--answer-003 "kunbtus"))))))))

(defun bcat-analysis-test--bilingual-003 ()
  "AS sentence 3 rendered with the bilingual template from both answers."
  (let ((bcat-template-directory (expand-file-name "Templates/analysis" bcat-analysis-test--root)))
    (bcat-template-render-named
     "bilingual"
     (bcat-analysis-bilingual-context
      (bcat-analysis-sanskrit-context
       (bcat-analysis-test--base-003)
       (bcat-analysis-parse (bcat-analysis-test--answer-003 "asam"))
       bcat-analysis-test--refs-003)
      (bcat-analysis-parse (bcat-analysis-test--answer-003 "kunbtus"))))))

(ert-deftest bcat-analysis-bilingual-two-parts ()
  "Carsten 1.11.: both languages per segment (0269fe5) were „nicht gut
handelbar“ (AS 2: 41 Tibetan segments, 21 pages) — „Dann trennen wir
das bitte wieder. Erst Sanskrit, dann Tibetisch … so handeln, wie bei
den einsprachigen, nur dass zu den Tibetisch Übersetzungen noch die
Differenz zum Sanskrit ergänzt wird“, with jump marks.  * Text: the
Sanskrit as in a Sanskrit file — Devanāgarī over IAST, per segment its
mark with a link to its Tibetan segment(s), the tables, „→“; the
sentence translation.  * Tibetan: the Uchen, one line per Sanskrit
segment; per Tibetan segment its mark (link back), the tables, „→“;
after the last of a group the differences; the Tibetan translation of
the sentence."
  (let* ((out (bcat-analysis-test--bilingual-003))
         (text (substring out (string-match "^\\* Text$" out) (string-match "^\\* Tibetan$" out)))
         (tib (substring out (string-match "^\\* Tibetan$" out) (string-match "^\\* Sentence$" out))))
    (should (string-match-p "\\`\\* Text\n#\\+begin_verse\nकस्मात्" text))
    (should (string-match-p
             (concat "^<<skt-23>>⟦23⟧ · \\[\\[tib-78\\]\\[Tib\\. 78\\]\\]\n| pañca-ākāra-ātma-vastu-udbhāvanatām .*\n\\(?:|.*\n\\)*"
                     "\n→ On account of their bringing to light .*\n\n\\(?:\\(?:[|/].*\n\\)+\n\\)*<<skt-24>>")
             text))
    (should-not (string-match-p "བདག\\|Differences" text))
    (should (string-match-p "\n\\*Translation:\\* Why are there just five aggregates" text))
    (should (string-match-p "\\`\\* Tibetan\n#\\+begin_verse\nཅིའི་ཕྱིར་ཕུང་པོ་རྣམས་ལྔ་ཁོ་ན་ཞེ་ན།\nབདག་གི་གཞི་རྣམ་པ" tib))
    (should (string-match-p
             (concat "^<<tib-78>>⟦78⟧ · \\[\\[skt-23\\]\\[Skt\\. 23\\]\\]\n| བདག་གི་གཞི .*\n| \\[\\[steinert:bdag gi gzhi\\].*\n\\(?:|.*\n\\)*"
                     "\n→ because they state the basis of self .*\n\n"
                     "\\*Differences:\\* /udbhāvanatā/.*\n\n<<tib-79>>")
             tib))
    (should (string-match-p "\n\\*Translation:\\* Why are the aggregates only five" tib))
    (should (< (string-match "^\\* Sentence$" out) (string-match "^\\*\\* Tibetan rendering$" out)
               (string-match "^\\*\\* Translations compared$" out) (string-match "^\\* Segments$" out)))))

(ert-deftest bcat-analysis-pair-lists-reordered ()
  "MSA IX.60: the Tibetan reorders the pādas — 459 renders Sanskrit 287,
460–461 286, 462 287 again.  The differences of a Sanskrit segment
stand once, after the last Tibetan segment that renders it; the
Sanskrit marks link the first."
  (let* ((lists (bcat-analysis-pair-lists
                 '(((num . 286)) ((num . 287)))
                 '(((num . 459) (uchen . "A")) ((num . 460) (uchen . "B"))
                   ((num . 461) (uchen . "C")) ((num . 462) (uchen . "D")))
                 '((459 . 287) (460 . 286) (461 . 286) (462 . 287))
                 '((286 . "D286") (287 . "D287"))))
         (tibs (nth 1 lists)))
    (should (equal '("" "" "D286" "D287") (mapcar (lambda (s) (alist-get 'differences s)) tibs)))
    (should (equal '(" · [[tib-460][Tib. 460–461]]" " · [[tib-459][Tib. 459, 462]]")
                   (mapcar (lambda (s) (alist-get 'mark s)) (nth 0 lists))))
    (should (equal '("B C" "A D") (mapcar (lambda (l) (alist-get 'line l)) (nth 2 lists))))))

(ert-deftest bcat-analysis-partner-two-parts ()
  "The Tibetan file of the pair mirrored (Carsten 1.11.: „Spiegeln:
Tibetisch zuerst“): * Text the Tibetan — Uchen, per segment mark (link
to the Sanskrit), tables, „→“, after the last of a group the
differences, the sentence translation —; * Sanskrit the Sanskrit —
Devanāgarī over IAST, per segment mark (link to the Tibetan), tables,
„→“, its sentence translation."
  (let* ((bcat-template-directory (expand-file-name "Templates/analysis" bcat-analysis-test--root))
         (tibetan (bcat-analysis-parse (bcat-analysis-test--answer-003 "kunbtus")))
         (out (bcat-template-render-named
               "bilingual-tibetan"
               (bcat-analysis-partner-context
                (bcat-analysis-context (bcat-analysis-test--base-tib-003) tibetan
                                       bcat-analysis-test--refs-tib-003)
                tibetan (bcat-analysis-test--sanskrit-003)
                "Why are there just five aggregates? — On account of …")))
         (text (substring out (string-match "^\\* Text$" out) (string-match "^\\* Sanskrit$" out)))
         (skt (substring out (string-match "^\\* Sanskrit$" out) (string-match "^\\* Sentence$" out))))
    (should (string-match-p "\\`\\* Text\n#\\+begin_verse\nཅིའི་ཕྱིར" text))
    (should (string-match-p
             (concat "^<<tib-78>>⟦78⟧ · \\[\\[skt-23\\]\\[Skt\\. 23\\]\\]\n| བདག་གི་གཞི .*\n\\(?:|.*\n\\)*"
                     "\n→ because they state the basis of self .*\n\n"
                     "\\*Differences:\\* /udbhāvanatā/")
             text))
    (should (string-match-p "\\`\\* Sanskrit\n#\\+begin_verse\nकस्मात्" skt))
    (should (string-match-p
             (concat "^<<skt-23>>⟦23⟧ · \\[\\[tib-78\\]\\[Tib\\. 78\\]\\]\n| pañca-ākāra-ātma-vastu-udbhāvanatām .*\n\\(?:|.*\n\\)*"
                     "\n→ On account of their bringing to light ")
             skt))
    (should (string-match-p "\n\\*Translation:\\* Why are there just five aggregates\\? — On account of …" skt))
    (should-not (string-match-p "Differences" skt))))

(ert-deftest bcat-analysis-num-ranges ()
  "Segment numbers for a heading: runs as ranges, gaps by comma."
  (should (equal "77" (bcat-analysis--num-ranges '(77))))
  (should (equal "40–44" (bcat-analysis--num-ranges '(40 41 42 43 44))))
  (should (equal "77, 79–80" (bcat-analysis--num-ranges '(77 79 80))))
  (should (equal "" (bcat-analysis--num-ranges nil))))

(ert-deftest bcat-analysis-bilingual-assignment ()
  "AS sentence 2: Tibetan segments finer than the Sanskrit (7 against
41).  Each Sanskrit segment gets the Tibetan segments Claude assigns to
it — heading „ · Tib. 36–38“, its mark a link to the first; a Tibetan
segment without a valid assignment goes with the one before it; a
Sanskrit segment without Tibetan has none.  The Tibetan part (1.11.):
each segment its mark back, the last of a group the group's
differences (two of one segment joined); the Uchen one line per
Sanskrit segment."
  (let* ((base `((segments ((num . 15) (samhita . "kati skandhāḥ."))
                           ((num . 16) (samhita . "pañca skandhāḥ."))
                           ((num . 17) (samhita . "aṣṭādaśa dhātavaḥ.")))
                 (tibetan ((num . 36) (uchen . "U36") (wylie . "phung po rnams ni du/"))
                          ((num . 37) (uchen . "U37") (wylie . "khams rnams ni du/"))
                          ((num . 38) (uchen . "U38") (wylie . "skye mched rnams ni du zhe na/"))
                          ((num . 39) (uchen . "U39") (wylie . "phung po rnams ni lnga ste/")))))
         (parsed '((segments ((num . 36) (sanskrit . 15)
                              (words ((wylie . "phung po") (gloss . "aggregate") (function . "N"))
                                     ((wylie . "rnams") (gloss . "plural") (function . "PL"))
                                     ((wylie . "ni") (gloss . "") (function . "TOP"))
                                     ((wylie . "du") (gloss . "how many") (function . "INT"))))
                             ((num . 37) (sanskrit . 99))
                             ((num . 38))
                             ((num . 39) (sanskrit . "16")))
                   (differences ((sanskrit . 15) (text . "One question in Tibetan, three in Sanskrit."))
                                ((sanskrit . 15) (text . "/zhe na/ closes it.")))
                   (rendering . "Close.")))
         (ctx (bcat-analysis-bilingual-context base parsed))
         (segs (alist-get 'segments ctx)))
    (should (equal " · Tib. 36–38" (alist-get 'parallel (nth 0 segs))))
    (should (equal " · Tib. 39" (alist-get 'parallel (nth 1 segs))))
    (should (equal "" (alist-get 'parallel (nth 2 segs))))
    (should (equal '(" · [[tib-36][Tib. 36–38]]" " · [[tib-39][Tib. 39]]" "")
                   (mapcar (lambda (s) (alist-get 'mark s)) segs)))
    (let ((tibs (alist-get 'tibetan-segments ctx)))
      (should (equal '(" · [[skt-15][Skt. 15]]" " · [[skt-15][Skt. 15]]" " · [[skt-15][Skt. 15]]"
                       " · [[skt-16][Skt. 16]]")
                     (mapcar (lambda (s) (alist-get 'mark s)) tibs)))
      (should (equal '("" "" "One question in Tibetan, three in Sanskrit. /zhe na/ closes it." "")
                     (mapcar (lambda (s) (alist-get 'differences s)) tibs)))
      (should (string-match-p "phung po" (alist-get 'gloss-table (nth 0 tibs)))))
    (should (equal '("U36 U37 U38" "U39")
                   (mapcar (lambda (l) (alist-get 'line l)) (alist-get 'tibetan-lines ctx))))
    (should (equal "One question in Tibetan, three in Sanskrit. /zhe na/ closes it."
                   (alist-get 'differences (nth 0 segs))))
    (should (equal "" (alist-get 'differences (nth 1 segs))))
    (should (equal "Close." (alist-get 'rendering ctx)))))

(ert-deftest bcat-analysis-tibetan-prompts ()
  "The Tibetan request of a bilingual sentence — Carsten 30.10.: „das
volle Programm“, the Tibetan file of the pair gets the whole Tibetan
analysis: the fields of the Tibetan contract (same rules) and, per
Tibetan segment, the Sanskrit segment it translates (in order, never
decreasing); differences per Sanskrit segment only where the Tibetan
differs in substance („wenn nichts da ist bitte weg lassen“); the
rendering of the sentence.  User prompt: the Sanskrit segments as
transmitted with the translation of the Sanskrit answer, the Tibetan
with Wylie, the reference translations of the Tibetan."
  (let ((sys (bcat-analysis-tibetan-system-prompt "de"))
        (user (bcat-analysis-tibetan-user-prompt
               3 nil
               '((22 "kasmāt pañcaiva skandhāḥ." "Why are there just five aggregates? —"))
               '((77 "ཅིའི་ཕྱིར། །" "ci'i phyir/ /"))
               '(("Rahula/Boin-Webb 2001" . "Why are there only five aggregates?"))
               '("Asaṅga, Abhidharmasamuccaya"))))
    (dolist (key '("\"segments\"" "\"sanskrit\"" "\"words\"" "\"wylie\"" "\"gloss\""
                   "\"function\"" "\"translation\"" "\"references\"" "\"shift\"" "\"structure\""
                   "\"note\"" "\"concepts\"" "\"differences\"" "\"text\"" "\"grammar\""
                   "\"units\"" "\"rendering\""))
      (should (string-match-p (regexp-quote key) sys)))
    (should (string-match-p (regexp-quote (bcat-analysis--tibetan-rules "German")) sys))
    (should (string-match-p "never decrease" sys))
    (should (string-match-p "goes with the first" sys))
    (should (string-match-p "No entry where the Tibetan renders the Sanskrit faithfully" sys))
    (should (string-match-p "/italic/" sys))
    (should (string-match-p "never a backslash before an apostrophe" sys))
    (should (string-match-p "in German" sys))
    (should (string-match-p (regexp-quote "Asaṅga, Abhidharmasamuccaya\n\nSentence 3 — Sanskrit segments:\n[22] kasmāt pañcaiva skandhāḥ.\n     Translation: Why are there just five aggregates? —\n\nTibetan segments:\n[77] Tibetan: ཅིའི་ཕྱིར། །\n     Wylie: ci'i phyir/ /\n\nReference translations:\n=== Rahula/Boin-Webb 2001 ===\nWhy are there only five aggregates?\n\nSigla for the comparison: C = your translation, R = Rahula/Boin-Webb 2001 — only in the units (their readings); everywhere else, references and shift included, the full names.\n\nReturn the JSON object now.") user))))

(defun bcat-analysis-test--base-tib-003 ()
  "The rump of the Tibetan file of AS sentence 3 (the pair's partner):
header, the Tibetan segments (Uchen, Wylie)."
  `((sent-num . "3")
    (source-link . "[[file:../kunbtus-1-cat.org::*Sentence 3][kunbtus-1-cat.org / Sentence 3]]")
    (parallel-link . "[[file:../../Sanskrit/analysis/sent-003-asam.org][sent-003-asam.org]]")
    (segment-list . "77, 78, 79, 80, 81, 82, 83")
    (parallel-segment-list . "22, 23, 24, 25, 26, 27, 28")
    (hash . "6a0f1d0b7f6c2d3f0e5c9a8b7d6e5f4a")
    (created . "2026-10-19")
    (last-analyzed . "2026-10-30")
    (segments . ,(alist-get 'tibetan (bcat-analysis-test--base-003)))))

(defconst bcat-analysis-test--refs-tib-003
  '(("Rahula/Boin-Webb 2001" . "Why are there only five aggregates? Because of the five methods by means of which [the idea] of self (ātman) makes its appearance: self as physical apprehension, self as experience, self as expression, self as the agent of all right and all wrong, and self as the basis of all that."))
  "The reference translations of the Tibetan AS sentence 3 as sent.")

(defun bcat-analysis-test--sanskrit-003 ()
  "The Sanskrit segments of AS sentence 3 after the Sanskrit answer."
  (alist-get 'segments (bcat-analysis-sanskrit-context
                        (bcat-analysis-test--base-003)
                        (bcat-analysis-parse (bcat-analysis-test--answer-003 "asam"))
                        bcat-analysis-test--refs-003)))

(defun bcat-analysis-test--sanskrit-translation-003 ()
  "The Sanskrit sentence translation of AS sentence 3 (the Sanskrit answer)."
  (bcat-analysis--field (bcat-analysis-parse (bcat-analysis-test--answer-003 "asam")) 'translation))

(ert-deftest bcat-analysis-partner-golden-003 ()
  "Carsten 30.10.: the Tibetan file of the pair — the full Tibetan
analysis (tables, structure, concepts, translations of the Tibetan) and,
mirrored, the Sanskrit: Devanāgarī over IAST and the padapāṭha with
vocabulary under * Text; per Tibetan segment the Sanskrit segment it
translates (heading „ · Skt. 22“), at the first of a group the
Sanskrit lines and the differences; the rendering.  Both answers →
the bilingual-tibetan template = the sample, character for character."
  (let ((bcat-template-directory
         (expand-file-name "Templates/analysis" bcat-analysis-test--root))
        (tibetan (bcat-analysis-parse (bcat-analysis-test--answer-003 "kunbtus")))
        (expected (with-temp-buffer
                    (insert-file-contents
                     (expand-file-name "Templates/analysis/samples/Bilingual-Tibetan.org"
                                       bcat-analysis-test--root))
                    (buffer-string))))
    (should (equal expected
                   (bcat-template-render-named
                    "bilingual-tibetan"
                    (bcat-analysis-partner-context
                     (bcat-analysis-context (bcat-analysis-test--base-tib-003) tibetan
                                            bcat-analysis-test--refs-tib-003)
                     tibetan (bcat-analysis-test--sanskrit-003)
                     (bcat-analysis-test--sanskrit-translation-003)))))))

(ert-deftest bcat-analysis-partner-assignment ()
  "AS sentence 2 seen from the Tibetan: Tibetan 36–38 translate Sanskrit
15, 39 translates 16.  Each Tibetan segment names its Sanskrit one and
links it; the differences stand once, after the last segment of the
group (1.11.); a Tibetan segment without a valid assignment goes with
the one before; the Sanskrit part links back."
  (let* ((base '((segments ((num . 36)) ((num . 37)) ((num . 38)) ((num . 39)))))
         (sanskrit '(((num . 15) (devanagari . "D15") (samhita . "kati skandhāḥ.")
                      (glossed . "kati [how many] skandhāḥ [aggregates]."))
                     ((num . 16) (devanagari . "D16") (samhita . "pañca skandhāḥ."))))
         (parsed '((segments ((num . 36) (sanskrit . 15)) ((num . 37) (sanskrit . 99))
                             ((num . 38)) ((num . 39) (sanskrit . 16)))
                   (differences ((sanskrit . 15) (text . "Three questions in Sanskrit.")))
                   (rendering . "Close.")))
         (ctx (bcat-analysis-partner-context base parsed sanskrit))
         (segs (alist-get 'segments ctx)))
    (should (equal '(" · Skt. 15" " · Skt. 15" " · Skt. 15" " · Skt. 16")
                   (mapcar (lambda (s) (alist-get 'parallel s)) segs)))
    (should (equal '(" · [[skt-15][Skt. 15]]" " · [[skt-15][Skt. 15]]" " · [[skt-15][Skt. 15]]"
                     " · [[skt-16][Skt. 16]]")
                   (mapcar (lambda (s) (alist-get 'mark s)) segs)))
    (should (equal '("" "" "Three questions in Sanskrit." "")
                   (mapcar (lambda (s) (alist-get 'differences s)) segs)))
    (should (equal '(" · [[tib-36][Tib. 36–38]]" " · [[tib-39][Tib. 39]]")
                   (mapcar (lambda (s) (alist-get 'mark s)) (alist-get 'sanskrit-segments ctx))))
    (should (alist-get 'sanskrit-p ctx))
    (should (equal "Close." (alist-get 'rendering ctx)))))


;; ----------------------------------------------------------------------------
;; Translations compared: units, rating, proposal (Carsten 2.11.)
;; ----------------------------------------------------------------------------

(ert-deftest bcat-analysis-comparison-units-contract ()
  "Carsten 2.11. (Muster Resources/vergleich-ta1-1.org of the Tantrāloka
reading, „das Muster passt“; „Immer“ — every C-c u G): Translations
compared becomes a comparison unit by unit.  Every sentence analysis —
Sanskrit, Tibetan and the Tibetan request of a bilingual pair — asks
for the units (passage, rating green/yellow/red, core, the readings by
siglum, finding, weight), the proposal (a double sense in two
versions), the decisions and the errors of its own analysis.  The
finding argues from the original, never from a translator's standing
\(„es gibt da keine Autorität, auf die man sich verlassen kann“);
lectures are cited with their time mark.  The short comparison
paragraph is gone; the reasoning is in German
\(`bcat-analysis-comparison-language'), the renderings in the target
language.  Since the same day the reasoning follows the target
language unless `bcat-analysis-comparison-language' is set (here: de)."
  (dolist (sys (let ((bcat-analysis-comparison-language "de"))
                 (list (bcat-analysis-sanskrit-system-prompt "en")
                       (bcat-analysis-system-prompt "en")
                       (bcat-analysis-tibetan-system-prompt "en"))))
    (dolist (key '("\"units\"" "\"passage\"" "\"rating\"" "\"core\"" "\"readings\""
                   "\"sigla\"" "\"finding\"" "\"weight\"" "\"proposal\"" "\"label\""
                   "\"decisions\"" "\"corrections\""))
      (should (string-match-p (regexp-quote key) sys)))
    (should-not (string-match-p (regexp-quote "\"comparison\"") sys))
    (should (string-match-p "\"green\": no difference in meaning" sys))
    (should (string-match-p "\"yellow\": a possible other interpretation" sys))
    (should (string-match-p "\"red\": different meanings, not both right at once" sys))
    (should (string-match-p "never from a translator's standing" sys))
    (should (string-match-p (regexp-quote "(S, I 0:15:24)") sys))
    (should (string-match-p "double sense" sys))
    (should (string-match-p "core, finding, the notes in weight, reason, decisions and corrections in German" sys))
    (should (string-match-p "renderings in weight and proposal in English" sys)))
  (let ((bcat-analysis-comparison-language "en"))
    (should (string-match-p "decisions and corrections in English"
                            (bcat-analysis-sanskrit-system-prompt "de")))
    (should (string-match-p "renderings in weight and proposal in German"
                            (bcat-analysis-sanskrit-system-prompt "de")))))

(ert-deftest bcat-analysis-sigla ()
  "The sigla of the comparison from the reference names (Muster: D =
Dyczkowski 2023, W = Wallis 2015, S = Sanderson 2022, DM = DharmaMitra,
C = the own translation): the initial of the first word, its capitals
when it has several (DharmaMitra); C stays the own translation; a
collision takes the next letters (Sanderson 2022/2025 → S, Sa)."
  (should (equal '(("Dyczkowski 2023" . "D") ("Wallis 2015" . "W")
                   ("Sanderson 2022" . "S") ("DharmaMitra" . "DM"))
                 (bcat-analysis-sigla '("Dyczkowski 2023" "Wallis 2015" "Sanderson 2022" "DharmaMitra"))))
  (should (equal '(("Sanderson 2022" . "S") ("Sanderson 2025" . "Sa") ("Cox 2010" . "Co"))
                 (bcat-analysis-sigla '("Sanderson 2022" "Sanderson 2025" "Cox 2010"))))
  (should (equal '(("Rahula/Boin-Webb 2001" . "R") ("Lindtner 1981" . "L"))
                 (bcat-analysis-sigla '("Rahula/Boin-Webb 2001" "Lindtner 1981")))))

(ert-deftest bcat-analysis-label-rule-generic ()
  "The last cell of a word — Tibetan function, Sanskrit form — has the
same limit in both prompts, the remarks elsewhere (5.11., generic: TĀ
1.1 had forms of 83 characters, „adj. nom. sg. (bahuvrīhi, agrees
with …)“, though the rule asked for a compact label)."
  (let ((sys (bcat-analysis-sanskrit-system-prompt "en")))
    (should (string-match-p (format "form: a compact label, at most %d characters" bcat-analysis--label-limit) sys))
    (should (string-match-p "no remarks in parentheses" sys))
    (should (string-match-p "belong in compounds or structure" sys)))
  (should (string-match-p (format "function: .*at most %d characters" bcat-analysis--label-limit)
                          (bcat-analysis--words-rule "English"))))

(ert-deftest bcat-analysis-sanskrit-metre-in-prompts ()
  "The metre the tool scans goes into the user prompt as a fact; the
system prompt lets Claude name only that one.  5.11., TĀ 1.1: the
grammar said „in the Śālinī metre“ — the verse has 17 syllables a pāda,
the tool scans Nardaṭaka; the prompt had said nothing about the metre."
  (let ((segs '((1 "vimalakalāśrayābhinavasṛṣṭimahā jananī bharitatanuśca pañcamukhaguptarucirjanakaḥ /"))))
    (should (string-match-p
             (regexp-quote "Metre (scanned by the tool from the syllable quantities): Nardaṭaka")
             (bcat-analysis-sanskrit-user-prompt 1 nil segs nil nil nil nil nil "Nardaṭaka")))
    (should-not (string-match-p "Metre (scanned" (bcat-analysis-sanskrit-user-prompt 1 nil segs nil)))
    (should (string-match-p "Name a metre only as given under Metre"
                            (bcat-analysis-sanskrit-system-prompt "en")))))

(ert-deftest bcat-analysis-comparison-prompt-sigla-lectures ()
  "The user prompt of every sentence analysis names the sigla of the
comparison; Sanderson's lectures on the sentence (#+CAT_LECTURES) go
with it — Carsten 2.11.: „Ja, nur beim Vergleich“ (before: never in a
prompt) — their YouTube links reduced to the time mark."
  (let* ((refs '(("Dyczkowski 2023" . "my speech") ("Sanderson 2022" . "Literal: my speech")
                 ("DharmaMitra" . "my words")))
         (lectures '(("Sanderson, Readings in the Tantrāloka I"
                      . "[[https://www.youtube.com/watch?v=x&t=924s][0:15:24]] /kula/ means body here.")))
         (segs '((685 "atra madvāgaśaktāpi yanniyantraṇamullaset /")))
         (user (bcat-analysis-sanskrit-user-prompt 1007 nil segs refs nil nil nil lectures)))
    (should (string-match-p
             (regexp-quote "Sigla for the comparison: C = your translation, D = Dyczkowski 2023, S = Sanderson 2022, DM = DharmaMitra")
             user))
    (should (string-match-p
             (regexp-quote "=== Sanderson, Readings in the Tantrāloka I ===\n[0:15:24] /kula/ means body here.")
             user))
    (should (string-match-p "only for the comparison" user))
    (should-not (string-match-p "youtube" user))
    (should (string-match-p "Return the JSON object now\\.\\'" user))
    (should-not (string-match-p "Lectures" (bcat-analysis-sanskrit-user-prompt 1007 nil segs refs)))
    (let ((tib (bcat-analysis-user-prompt 9 nil '((38 "ཀ" "ka")) '(("Lindtner 1981" . "x")) nil nil lectures)))
      (should (string-match-p (regexp-quote "Sigla for the comparison: C = your translation, L = Lindtner 1981") tib))
      (should (string-match-p (regexp-quote "=== Sanderson, Readings in the Tantrāloka I ===") tib)))
    (should (string-match-p
             (regexp-quote "Sigla for the comparison: C = your translation, R = Rahula/Boin-Webb 2001")
             (bcat-analysis-tibetan-user-prompt 3 nil '((22 "x" "t")) '((77 "ཀ" "ka"))
                                                '(("Rahula/Boin-Webb 2001" . "y")))))))

(defconst bcat-analysis-test--units-answer
  "{\"segments\": [], \"translation\": \"May my heart flash forth.\",
 \"units\": [{\"passage\": \"/abhinavasṛṣṭimahā/\", \"rating\": \"red\", \"core\": \"/mahas/ (Glanz) gegen /mahā/ „groß“\",
              \"readings\": [{\"sigla\": \"D\", \"text\": \"„the glory“\"}, {\"sigla\": \"DM, C\", \"text\": \"„the great Mother“\"}],
              \"finding\": \"Zwei Analysen.\", \"weight\": \"„whose radiance is ever-new creation“\"},
             {\"passage\": \"/jananī/, /janakaḥ/\", \"rating\": \"green\", \"core\": \"Mutter | Vater\",
              \"readings\": [], \"finding\": \"Alle: Mutter, Vater.\", \"weight\": \"\"}],
 \"proposal\": [{\"label\": \"Erster Sinn\", \"text\": \"May my Heart flash forth in full.\"},
                {\"label\": \"Zweiter Sinn\", \"text\": \"… my mother Vimalā …\"}],
 \"decisions\": \"/mahas/ statt /mahā/ (Nr. 1).\",
 \"corrections\": [\"Die Grammar nennt /saṃsphuratāt/ āśīrliṅ — falsch, -tāt-Imperativ.\"]}"
  "A Sanskrit answer with the comparison of Carsten's Muster (TĀ 1.1).")

(ert-deftest bcat-analysis-comparison-units-render ()
  "Translations compared as in the Muster: the translations with their
marks (footnote definitions still before the first heading), then
*** Übersicht — the sigla, the table Nr. | Stelle | Einstufung | Kern
with fixed column widths and the cell colour of the rating
\(@@latex:\\cellcolor{ok|mid|bad}@@grün/gelb/rot), a bar in a cell
escaped —, *** Die Stellen im Einzelnen (**** per unit: readings as a
description list, *Befund:*, *Gewicht:*; a green unit without readings
only its finding), *** Berichtigungen zur Analyse.  The proposal (a quote
per sense, *Entscheidungen:*) stands under the tables since 4.11.
\(`bcat-analysis-comparison-proposal')."
  (let* ((parsed (bcat-analysis-parse bcat-analysis-test--units-answer))
         (out (bcat-analysis-comparison
               parsed '(("Dyczkowski 2023" . "the glory [fn:d1]\n\n[fn:d1] Dyczkowski's note.")
                        ("DharmaMitra" . "the great Mother"))
               "de"))
         (table (cl-remove-if-not (lambda (l) (string-prefix-p "|" l)) (split-string out "\n"))))
    (should (string-prefix-p
             (concat "*Translation:* May my heart flash forth.\n\n"
                     "*Dyczkowski 2023:* the glory [fn:d1]\n\n"
                     "*DharmaMitra:* the great Mother\n\n"
                     "[fn:d1] Dyczkowski's note.\n\n"
                     "*** Übersicht\n"
                     "Sigel: C = Translation, D = Dyczkowski 2023, DM = DharmaMitra\n\n"
                     "#+ATTR_LATEX: :align rp{4.3cm}p{1.6cm}p{8cm} :font \\small\n")
             out))
    (should (= 4 (length table)))
    (should (= 1 (length (delete-dups (mapcar #'string-width table)))))
    (should (string-match-p "\\`| Nr\\. | Stelle +| Einstufung +| Kern +|\\'" (nth 0 table)))
    (should (string-match-p "\\`|-----\\+-+\\+-+\\+-+|\\'" (nth 1 table)))
    (should (string-match-p (concat "\\`| +1 | /abhinavasṛṣṭimahā/ +| "
                                    (regexp-quote "@@latex:\\cellcolor{bad}@@rot")
                                    " +| /mahas/ (Glanz) gegen /mahā/ „groß“ +|\\'")
                            (nth 2 table)))
    (should (string-match-p (concat "\\`| +2 | /jananī/, /janakaḥ/ +| "
                                    (regexp-quote "@@latex:\\cellcolor{ok}@@grün")
                                    " +| Mutter \\\\vert{} Vater +|\\'")
                            (nth 3 table)))
    (should (string-suffix-p
             (concat "\n\n*** Die Stellen im Einzelnen\n"
                     "**** 1. /abhinavasṛṣṭimahā/ — rot\n"
                     "- D :: „the glory“\n"
                     "- DM, C :: „the great Mother“\n"
                     "*Befund:* Zwei Analysen.\n"
                     "*Gewicht:* „whose radiance is ever-new creation“\n\n"
                     "**** 2. /jananī/, /janakaḥ/ — grün\n"
                     "*Befund:* Alle: Mutter, Vater.\n\n"
                     "*** Berichtigungen zur Analyse\n"
                     "- Die Grammar nennt /saṃsphuratāt/ āśīrliṅ — falsch, -tāt-Imperativ.")
             out))
    ;; One sense: the quote without a label; no corrections: no heading.
    (let ((one (bcat-analysis-comparison
                (append '((proposal ((label . "") (text . "One."))) (corrections))
                        parsed)
                nil "de")))
      (should (equal "*Übersetzungsvorschlag:*\n#+begin_quote\nOne.\n#+end_quote\n\n*Entscheidungen:* /mahas/ statt /mahā/ (Nr. 1)."
                     (bcat-analysis-comparison-proposal
                      (append '((proposal ((label . "") (text . "One."))) (corrections)) parsed)
                      "de")))
      (should (equal (concat "*Übersetzungsvorschlag — Erster Sinn:*\n#+begin_quote\nMay my Heart flash forth in full.\n#+end_quote\n\n"
                             "*Übersetzungsvorschlag — Zweiter Sinn:*\n#+begin_quote\n… my mother Vimalā …\n#+end_quote\n\n"
                             "*Entscheidungen:* /mahas/ statt /mahā/ (Nr. 1).")
                     (bcat-analysis-comparison-proposal parsed "de")))
      (should-not (string-match-p "Übersetzungsvorschlag" out))
      (should-not (string-match-p "Berichtigungen" one)))
    ;; English labels.
    (let ((en (bcat-analysis-comparison parsed nil "en")))
      (dolist (s '("*** Overview\nSigla: C = Translation" "| No. | Passage" "| Rating" "| Point"
                   "@@latex:\\cellcolor{bad}@@red" "**** 1. /abhinavasṛṣṭimahā/ — red" "*Finding:* "
                   "*Weight:* " "*** Corrections to the analysis"))
        (should (string-match-p (regexp-quote s) en))))))

(ert-deftest bcat-analysis-comparison-doubts-contract ()
  "Carsten 2.11.: „die Auswertung der Comparison mit Schlußfolgerung …
immer, wenn möglich und es nötig erscheint. In der Klasse will ich dann
sagen können: x übersetzt so und so, ich bin mir nicht sicher, ob das
richtig ist weil … Dafür müsste ich es übersichtlich sehen.“  Every
yellow and red unit names its doubts — who renders how, how sure
\(doubtful / wrong), why, what is better — and the rating follows the
verdict: red as soon as a reading is wrong (TĀ 1.1: gupta „hidden“
rejected, yet rated yellow)."
  (dolist (sys (list (bcat-analysis-sanskrit-system-prompt "en")
                     (bcat-analysis-system-prompt "en")
                     (bcat-analysis-tibetan-system-prompt "en")))
    (dolist (key '("\"doubts\"" "\"who\"" "\"verdict\"" "\"reason\"" "\"better\""))
      (should (string-match-p (regexp-quote key) sys)))
    (should (string-match-p "\"wrong\": the grammar or the lexicon excludes it" sys))
    (should (string-match-p "\"doubtful\": " sys))
    (should (string-match-p "red as soon as one reading is wrong" sys))
    (should (string-match-p "draw the conclusion wherever the text allows one" sys))
    (should (string-match-p "reason: .*without the conjunction" sys))))

(defconst bcat-analysis-test--doubts-answer
  "{\"segments\": [], \"translation\": \"May my heart flash forth.\",
 \"units\": [{\"passage\": \"/vimalakalāśrayā/\", \"rating\": \"yellow\", \"core\": \"kalā\",
              \"readings\": [], \"finding\": \"f1\", \"weight\": \"w1\",
              \"doubts\": [{\"who\": \"D „(of the Moon)“\", \"verdict\": \"doubtful\",
                           \"reason\": \"das im Kompositum nicht steht\", \"better\": \"\"}]},
             {\"passage\": \"/jananī/\", \"rating\": \"green\", \"core\": \"Mutter\",
              \"readings\": [], \"finding\": \"Alle: Mutter.\", \"weight\": \"\", \"doubts\": []},
             {\"passage\": \"/pañcamukhaguptarucir/\", \"rating\": \"red\", \"core\": \"gupta\",
              \"readings\": [], \"finding\": \"f5\", \"weight\": \"w5\",
              \"doubts\": [{\"who\": \"D „the hidden (protected) light“, DM „concealed within the five faces“\",
                           \"verdict\": \"wrong\",
                           \"reason\": \"gupta (√gup) zuerst ‚bewahrt, geschützt‘ heißt (S, I 0:21:33)\",
                           \"better\": \"„whose radiance is maintained through his five faces“ (W, S)\"}]}],
 \"proposal\": [], \"decisions\": \"\", \"corrections\": []}"
  "An answer with doubts in a yellow and a red unit.")

(ert-deftest bcat-analysis-comparison-doubts-render ()
  "„Für die Klasse“ directly under the colour table (Carsten 2.11.:
„Liste direkt unter der Tabelle“): per yellow or red unit with doubts,
red first, its number, passage and rating, then one line per doubt —
who and how, *abzulehnen*/*fraglich*, weil …, Besser: …; nothing for
units without doubts, no section without any."
  (let* ((parsed (bcat-analysis-parse bcat-analysis-test--doubts-answer))
         (out (bcat-analysis-comparison parsed nil "de")))
    (should (string-match-p
             (regexp-quote
              (concat "|   3 | /pañcamukhaguptarucir/ | @@latex:\\cellcolor{bad}@@rot  | gupta  |\n\n"
                      "*** Für die Klasse\n"
                      "- *3. /pañcamukhaguptarucir/ — rot*\n"
                      "  - D „the hidden (protected) light“, DM „concealed within the five faces“ — "
                      "*abzulehnen*, weil gupta (√gup) zuerst ‚bewahrt, geschützt‘ heißt (S, I 0:21:33). "
                      "Besser: „whose radiance is maintained through his five faces“ (W, S).\n"
                      "- *1. /vimalakalāśrayā/ — gelb*\n"
                      "  - D „(of the Moon)“ — *fraglich*, weil das im Kompositum nicht steht.\n\n"
                      "*** Die Stellen im Einzelnen\n"))
             out))
    (should-not (string-match-p "2\\. /jananī/ — grün\\*" out))
    (let ((en (bcat-analysis-comparison parsed nil "en")))
      (should (string-match-p (regexp-quote "*** For the class\n- *3. /pañcamukhaguptarucir/ — red*") en))
      (should (string-match-p (regexp-quote "— *wrong*, because gupta") en))
      (should (string-match-p (regexp-quote "Better: „whose radiance") en)))
    (should-not (string-match-p "Für die Klasse"
                                (bcat-analysis-comparison
                                 (bcat-analysis-parse bcat-analysis-test--units-answer) nil "de")))))

(ert-deftest bcat-analysis-class-reason-no-double-because ()
  "Bug (4.11., bDen 15): „*wrong*, because because the genitive /yi/ …“
— Claude began the reason with the conjunction although asked not to,
and the line put its own before it.  A leading because/since/as/weil/da
of the reason is dropped."
  (dolist (lang '("en" "de"))
    (let ((out (bcat-analysis-comparison
                (bcat-analysis-parse
                 (concat "{\"segments\": [], \"units\": [{\"passage\": \"/x/\", \"rating\": \"yellow\", \"core\": \"c\","
                         " \"readings\": [], \"finding\": \"f\", \"weight\": \"w\", \"doubts\": ["
                         "{\"who\": \"L „a“\", \"verdict\": \"doubtful\", \"reason\": \"Because the genitive points back\", \"better\": \"\"},"
                         "{\"who\": \"A „b“\", \"verdict\": \"doubtful\", \"reason\": \"weil der Genitiv zurückweist\", \"better\": \"\"}]}]}"))
                nil lang)))
      (should-not (string-match-p "\\(?:because\\|weil\\) \\(?:because\\|Because\\|weil\\)" out))
      (should (string-match-p "the genitive points back\\." out))
      (should (string-match-p "der Genitiv zurückweist\\." out)))))

(ert-deftest bcat-analysis-comparison-grammar-certainty-impact ()
  "Carsten 4.11. on bDen 15 (the teacher argued grammatically for
Lindtner's and Apple's reading): „Die Frage ist einfach nur, ob beide
Lesarten grammatikalisch möglich sind“ — and a marker „wie sicher die
letztliche Entscheidung ist und was für Auswirkungen sie haben könnte“
\(here none: Nāgārjuna counts as realized anyway).  Per unit: whether
all readings are grammatically possible, how certain the decision, its
impact; red only when grammar or lexicon excludes a reading.  For the
class shows every yellow and red unit with them."
  (dolist (sys (list (bcat-analysis-sanskrit-system-prompt "en")
                     (bcat-analysis-system-prompt "en")
                     (bcat-analysis-tibetan-system-prompt "en")))
    (dolist (key '("\"grammatical\"" "\"grammar_note\"" "\"certainty\"" "\"impact\"" "\"impact_note\""))
      (should (string-match-p (regexp-quote key) sys)))
    (should (string-match-p "red only when the grammar or the lexicon excludes" sys))
    (should (string-match-p "\"wrong\": the grammar or the lexicon excludes it" sys)))
  (let* ((parsed (bcat-analysis-parse
                  (concat "{\"segments\": [], \"units\": ["
                          "{\"passage\": \"/klu sgrub slob ma zla grags yin/\", \"rating\": \"yellow\", \"core\": \"one or two persons\","
                          " \"readings\": [], \"finding\": \"f\", \"weight\": \"w\","
                          " \"grammatical\": \"all\", \"grammar_note\": \"/klu sgrub/ can stand as a second subject or as genitive to /slob ma/\","
                          " \"certainty\": \"open\", \"impact\": \"low\", \"impact_note\": \"Nāgārjuna counts as realized either way\", \"doubts\": []},"
                          "{\"passage\": \"/x/\", \"rating\": \"green\", \"core\": \"c\", \"readings\": [], \"finding\": \"f\", \"weight\": \"\"}]}")))
         (de (bcat-analysis-comparison parsed nil "de"))
         (en (bcat-analysis-comparison parsed nil "en")))
    (should (string-match-p
             (regexp-quote (concat "- *1. /klu sgrub slob ma zla grags yin/ — gelb*\n"
                                   "  - Grammatik: alle Lesarten möglich — /klu sgrub/ can stand as a second subject or as genitive to /slob ma/.\n"
                                   "  - Entscheidung: *offen* · Auswirkung: *gering* — Nāgārjuna counts as realized either way."))
             de))
    (should-not (string-match-p "2\\. /x/ — grün\\*" de))
    (should (string-match-p (regexp-quote "  - Grammar: all readings possible — ") en))
    (should (string-match-p (regexp-quote "  - Decision: *open* · Impact: *low* — ") en))))

(ert-deftest bcat-analysis-proposal-under-the-tables ()
  "Carsten 4.11.: „Darunter könnte ja dann die Grammar stehen und
danach die proposed translation und die decisions. Die ursprüngliche
\(Claude) translation dann nur in der Comparison.“  * Text: the tables,
then *Grammar:*, the proposal (a quote per sense) and *Decisions:*; the
own translation opens Translations compared (C); * Sentence has no
Grammar any more.  Without a proposal (no reference translation) the
own translation stays under the tables."
  (let* ((bcat-template-directory (expand-file-name "Templates/analysis" bcat-analysis-test--root))
         (answer (concat "{\"segments\": [{\"num\": 38, \"words\": [], \"translation\": \"Inexpressible\"}],"
                         " \"grammar\": \"Two predicate pādas.\", \"translation\": \"Own translation.\","
                         " \"units\": [{\"passage\": \"/x/\", \"rating\": \"yellow\", \"core\": \"c\", \"readings\": [],"
                         " \"finding\": \"f\", \"weight\": \"w\"}],"
                         " \"proposal\": [{\"label\": \"First sense\", \"text\": \"Proposed one.\"},"
                         " {\"label\": \"Second sense\", \"text\": \"Proposed two.\"}],"
                         " \"decisions\": \"Unit 1: kept.\", \"corrections\": []}"))
         (base (cons '(target-lang . "en") (bcat-analysis-test--base)))
         (out (bcat-template-render-named
               "tibetan" (bcat-analysis-context base (bcat-analysis-parse answer) bcat-analysis-test--refs)))
         (text (substring out (string-match "^\\* Text$" out) (string-match "^\\* Sentence$" out))))
    (should (string-match-p
             (regexp-quote (concat "*Grammar:* Two predicate pādas.\n\n"
                                   "*Proposed translation — First sense:*\n#+begin_quote\nProposed one.\n#+end_quote\n\n"
                                   "*Proposed translation — Second sense:*\n#+begin_quote\nProposed two.\n#+end_quote\n\n"
                                   "*Decisions:* Unit 1: kept.\n\n* Sentence\n** Concepts\n"))
             out))
    (should-not (string-match-p "^\\*Translation:\\*" text))
    (should-not (string-match-p "^\\*\\* Grammar$" out))
    (should (string-match-p "^\\*\\* Translations compared\n\\*Translation:\\* Own translation\\." out))
    (should (string-match-p "^Sigla: C = Translation, L = Lindtner 1981" out))
    (should-not (string-match-p "^\\*\\*\\* Proposed translation" out))
    ;; no proposal: the own translation under the tables
    (let* ((plain (bcat-template-render-named
                   "tibetan" (bcat-analysis-context base (bcat-analysis-parse (bcat-analysis-test--answer))
                                                    bcat-analysis-test--refs))))
      (should (string-match-p "^\\*Grammar:\\* .*\n\n\\*Translation:\\* Indescribable" plain)))))

(ert-deftest bcat-analysis-proposal-reads-aloud ()
  "Carsten 4.11. on the proposal of bDen 15 — „If one asks, \"By whom is
emptiness realized?\" — [it is Candrakīrti, disciple of] Nāgārjuna,
who … — it is Candrakīrti, disciple of Nāgārjuna.“ —: „ziemlich
holprig, oder?“  The decisions were put into the sentence (a bracket
to show the referent, the main clause twice).  The proposal is a
finished translation to read aloud; the reasons go to the decisions."
  (dolist (sys (list (bcat-analysis-sanskrit-system-prompt "en")
                     (bcat-analysis-system-prompt "de")
                     (bcat-analysis-tibetan-system-prompt "en")))
    (should (string-match-p "finished translation to be read aloud" sys))
    (should (string-match-p "every statement once" sys))
    (should (string-match-p "never to show a construction or to repeat" sys))))

(ert-deftest bcat-analysis-comparison-target-language ()
  "Carsten 2.11.: „bitte im Handout auch immer alles in der
Zielsprache“ — the comparison's reasoning and its words (headings,
table, verdicts) follow the document's target language; German only
when `bcat-analysis-comparison-language' says so (default nil).  The
target comes with the rump (`target-lang') into the context."
  (should (null (eval (car (get 'bcat-analysis-comparison-language 'standard-value)))))
  (let ((bcat-analysis-comparison-language nil))
    (should (string-match-p "reason, decisions and corrections in English"
                            (bcat-analysis-sanskrit-system-prompt "en")))
    (should (string-match-p "reason, decisions and corrections in German"
                            (bcat-analysis-sanskrit-system-prompt "de")))
    (let ((parsed (bcat-analysis-parse bcat-analysis-test--units-answer)))
      (should (string-match-p "^\\*\\*\\* Overview$" (bcat-analysis-comparison parsed nil "en")))
      (should (string-match-p "^\\*\\*\\* Übersicht$" (bcat-analysis-comparison parsed nil "de")))
      (should (string-match-p "^\\*\\*\\* Overview$"
                              (alist-get 'comparison
                                         (bcat-analysis-sanskrit-context
                                          (cons '(target-lang . "en") (bcat-analysis-test--base-1007))
                                          parsed nil))))
      (should (string-match-p "^\\*\\*\\* Overview$"
                              (alist-get 'comparison
                                         (bcat-analysis-context
                                          (cons '(target-lang . "en") (bcat-analysis-test--base))
                                          parsed nil))))))
  (let ((bcat-analysis-comparison-language "de"))
    (should (string-match-p "reason, decisions and corrections in German"
                            (bcat-analysis-sanskrit-system-prompt "en")))
    (should (string-match-p "^\\*\\*\\* Übersicht$"
                            (bcat-analysis-comparison
                             (bcat-analysis-parse bcat-analysis-test--units-answer) nil "en")))))

(ert-deftest bcat-analysis-mark-right-above-table ()
  "Carsten 4.11., reading on the MacBook: „ziemlich viel verschenkter
Platz“ — the ⟦N⟧ mark stands right above its table, no blank line
between (the PDF gets it from the export, `bcat-export--marks-apart').
A mark shown without a table (bilingual: translation or differences
only) keeps its blank line — „→ …“ right under it would be read as the
segment's table by C-c u L."
  (let* ((bcat-template-directory (expand-file-name "Templates/analysis" bcat-analysis-test--root))
         (tib (bcat-template-render-named
               "tibetan" (bcat-analysis-context (bcat-analysis-test--base)
                                                (bcat-analysis-parse (bcat-analysis-test--answer))
                                                bcat-analysis-test--refs)))
         (bil (bcat-analysis-test--bilingual-003)))
    (dolist (out (list tib bil))
      (should (string-match-p "^\\(?:<<[^>\n]*>>\\)?⟦[0-9]+⟧[^\n]*\n| " out))
      (should-not (string-match-p "^\\(?:<<[^>\n]*>>\\)?⟦[0-9]+⟧[^\n]*\n\n| " out)))
    (let ((bcat-template-directory bcat-template-directory))
      (should (string-match-p
               (regexp-quote "<<tib-77>>⟦77⟧ · [[skt-22][Skt. 22]]\n\n→ ")
               (bcat-template-render-named
                "bilingual"
                '((tibetan-p . t)
                  (tibetan-segments ((num . 77) (shown . t) (mark . " · [[skt-22][Skt. 22]]")
                                     (segment-translation . "Why?"))))))))))

(ert-deftest bcat-analysis-compact-function ()
  "Carsten 4.11. (bDen 16 on the MacBook, „ziemlich viel verschenkter
Platz“): a function cell like „V pf. (brgyud pa) · relative clause, GEN
attributive to man ngag“ (64 characters) widened its column, the table
broke at 130 and left „gis“ alone in a second table.  The prompt asks
for at most 25 characters; the table cuts a longer function at its „·“
\(`bcat-analysis-function-width', 30) and the rest goes to the
segment's Structure as a line of its own."
  (should (string-match-p "function: .*at most 25 characters" (bcat-analysis--words-rule "English")))
  (should (string-match-p "belongs in structure" (bcat-analysis--words-rule "English")))
  (should (equal '("V pf. (brgyud pa)" . "relative clause, GEN attributive to man ngag")
                 (bcat-analysis--function-split "V pf. (brgyud pa) · relative clause, GEN attributive to man ngag")))
  (should (equal '("V pf. ('pong ba) · main verb") (bcat-analysis--function-split "V pf. ('pong ba) · main verb")))
  (should (equal '("N" . "appositive or compound complement")
                 (bcat-analysis--function-split "N · appositive or compound complement")))
  (let* ((parsed (bcat-analysis-parse
                  (concat "{\"segments\": [{\"num\": 66, \"words\": ["
                          "{\"wylie\": \"de\", \"gloss\": \"that\", \"function\": \"DEM · oblique base\"},"
                          "{\"wylie\": \"brgyud pa'i\", \"gloss\": \"transmitted\", \"function\": \"V pf. (brgyud pa) · relative clause, GEN attributive to man ngag\"}],"
                          " \"translation\": \"t\", \"structure\": \"de [source] · brgyud pa'i [attr.]\"}],"
                          " \"translation\": \"t\", \"grammar\": \"g\"}")))
         (seg (car (alist-get 'segments (bcat-analysis-context '((segments ((num . 66) (wylie . "de brgyud pa'i")))) parsed)))))
    (should (string-match-p "| V pf\\. +|" (alist-get 'gloss-table seg)))
    (should-not (string-match-p "relative clause" (alist-get 'gloss-table seg)))
    (should (equal "de [source] · brgyud pa'i [attr.] \\\\\n/brgyud pa'i/: relative clause, GEN attributive to man ngag"
                   (alist-get 'structure seg)))))

(ert-deftest bcat-analysis-function-lemma-only-when-other-stem ()
  "Carsten 4.11.: „V pf. (brgyud pa) z.B. ist die Wiederholung des
Verbs nicht redundant?“ — under brgyud pa'i the dictionary form only
repeats the stem (54 of 99 verbs in the files).  It stays where the
text has another stem: spangs ('pong ba), gzung ('dzin pa), byas pa
\(byed pa), 'gyur (gyur pa).  The prompt says so; the table drops a
repeating one."
  (should (string-match-p "dictionary form .*only when" (bcat-analysis--words-rule "English")))
  (dolist (case '(("brgyud pa'i" "V pf. (brgyud pa)" "V pf.")
                  ("rtogs par" "V inf. (rtogs pa)" "V inf.")
                  ("mi shes" "NEG + V pres. (shes pa)" "NEG + V pres.")
                  ("smra" "V pres. (smra ba) · main verb" "V pres. · main verb")
                  ("'bab" "V pres. ('bab pa) · main verb" "V pres. · main verb")
                  ("spangs" "V pf. ('pong ba) · main verb" "V pf. ('pong ba) · main verb")
                  ("byas pa" "V pf. (byed pa)" "V pf. (byed pa)")
                  ("bslus par" "V pf. (bslu ba)" "V pf. (bslu ba)")
                  ("'gyur" "V pres./fut. (gyur pa)" "V pres./fut. (gyur pa)")
                  ("rtogs par" "V inf. (rtogs pa) · TERM (purposive)" "V inf. · TERM (purposive)")))
    (should (equal (nth 2 case) (bcat-analysis--trim-lemma (nth 1 case) (nth 0 case)))))
  (let ((table (bcat-analysis-gloss-table
                '(((wylie . "brgyud pa'i") (gloss . "transmitted") (function . "V pf. (brgyud pa)"))
                  ((wylie . "spangs") (gloss . "abandoned") (function . "V pf. ('pong ba)"))))))
    (should (string-match-p "| V pf\\. +| V pf\\. ('pong ba) |" table)))
  (let ((out (bcat-analysis-compact-tables "| [[steinert:rtogs par][rtogs par]] |\n| to realise |\n| V inf. (rtogs pa) |")))
    (should (string-match-p "| V inf\\. +|" (car out)))
    (should-not (cdr out))))

(ert-deftest bcat-analysis-function-split-generic ()
  "The last row of every table — Tibetan function, Sanskrit form — is cut
at `bcat-analysis-function-width' the same way (Carsten 5.11.: „alles
erstmal generisch“): at its „·“ parts, else without the remarks in
parentheses, else at a comma; the rest for Structure.  TĀ 1.1:
„adj. nom. sg. (bahuvrīhi, agrees with janakaḥ; by kākākṣi-nyāya also
with jananī)“ (83 characters) made its column the widest of the table."
  (should (equal '("adj. nom. sg." . "bahuvrīhi, agrees with janakaḥ; by kākākṣi-nyāya also with jananī")
                 (bcat-analysis--function-split
                  "adj. nom. sg. (bahuvrīhi, agrees with janakaḥ; by kākākṣi-nyāya also with jananī)")))
  (should (equal '("āśiṣi 3 sg. P" . "precative/benedictive; prathamapuruṣa ekavacana; 3rd sg.")
                 (bcat-analysis--function-split
                  "āśiṣi (precative/benedictive) 3 sg. P, prathamapuruṣa ekavacana (3rd sg.)")))
  (should (equal '("pron. gen. sg." . "ṣaṣṭhī vibhakti")
                 (bcat-analysis--function-split "pron. gen. sg. (ṣaṣṭhī vibhakti)")))
  (should (equal '("adj. f. nom. sg. (bahuvrīhi)") (bcat-analysis--function-split "adj. f. nom. sg. (bahuvrīhi)")))
  ;; Tibetan as before: the „·“ parts first.
  (should (equal '("V pf. (brgyud pa)" . "relative clause, GEN attributive to man ngag")
                 (bcat-analysis--function-split "V pf. (brgyud pa) · relative clause, GEN attributive to man ngag"))))

(ert-deftest bcat-analysis-wrap-cell-members ()
  "A cell wider than the limit breaks between the members of a compound
(„;“) — a member wider still at its „ · “ —, whole members kept on a
line; the separator stays at the end of the line (5.11., TĀ 1.1: the
parts row of pañca-mukha-gupta-ruciḥ).  A cell with a link is never
broken."
  (should (equal '("pañca five; mukha face, power;" "√gup (1) protect, maintain · -ta ppp;" "ruci light, radiance")
                 (bcat-analysis--wrap-cell
                  "pañca five; mukha face, power; √gup (1) protect, maintain · -ta ppp; ruci light, radiance" 45)))
  (should (equal '("vi- apart · √mal (1) be impure · -a adj;" "kalā part, power;"
                   "ā- unto · √śri (1) resort · -a whose basis")
                 (bcat-analysis--wrap-cell
                  "vi- apart · √mal (1) be impure · -a adj; kalā part, power; ā- unto · √śri (1) resort · -a whose basis" 45)))
  (should (equal '("an- not · uttara higher, surpassed ·" "(comparative);" "kula body")
                 (bcat-analysis--wrap-cell "an- not · uttara higher, surpassed · (comparative); kula body" 38)))
  (should (equal '("short") (bcat-analysis--wrap-cell "short" 45)))
  (let ((link "[[steinert:a b c][a b c]] · d · e · f · g · h · i · j · k · l · m · n"))
    (should (equal (list link) (bcat-analysis--wrap-cell link 20)))))

(ert-deftest bcat-analysis-wrap-cell-room-for-the-mark ()
  "A broken line is never wider than the limit — its separator („ ·“,
„;“) included.  The mark was added after the line had been measured:
„aaaa · bbbb ·“ at 11 has 13 characters (5.11., found with the width
60 of `bcat-analysis-layout-overrides': a line of 61)."
  (dolist (case '(("aaaa · bbbb · cccc" 11) ("aaaa; bbbb; cccc" 10)
                  ("an- not · uttara higher, surpassed · (comparative); kula body" 38)))
    (dolist (line (bcat-analysis--wrap-cell (car case) (cadr case)))
      (should (<= (string-width line) (cadr case))))))

(ert-deftest bcat-analysis-table-columns-continuations ()
  "Reading a table back joins the lines a wide cell was broken into:
a line ending in „ ·“ or „;“ goes on in the row below where that row
has nothing else — the cells as they were before the break (C-c u L,
the export)."
  (let* ((rows '(("tad-ubhaya" "hṛdayam")
                 ("tad · ubhaya · yāmala · vi-√sphur-ita · bhāva · maya" "hṛd-aya")
                 ("tad those; ubhaya both; yāmala pair, union; vi- forth · √sphur (6) flash · -ita ppp; bhāva state" "hṛd heart · -aya m. noun")
                 ("consisting of the emission" "heart")
                 ("adj. n. nom. sg." "n. nom. sg.")))
         (table (tibetan-gloss-table--format-rows (bcat-analysis-wrap-rows rows 45))))
    (should (> (length (split-string table "\n" t)) 5))
    (should (equal (list (mapcar #'car rows) (mapcar #'cadr rows))
                   (bcat-analysis--table-columns table)))))

(defconst bcat-analysis-test--ta-1-1-words
  '(((word . "vimala-kalā-āśrayā") (lemma . "vimala-kalā-āśraya") (formation . "vi-mala · kalā · ā-√śri-a")
     (parts . "vi- apart · √mal (1) be impure · -a adj; kalā part, power; ā- unto · √śri (1) resort · -a whose basis")
     (gloss . "whose basis is pure kalā") (form . "adj. f. nom. sg. (bahuvrīhi, agrees with jananī)"))
    ((word . "abhinava-sṛṣṭi-mahāḥ") (lemma . "abhinava-sṛṣṭi-mahas") (formation . "abhi-nava · sṛṣṭi · mahas")
     (parts . "abhi- toward · nava new, ever-new; sṛṣṭi creation (√sṛj); mahas radiance, glory")
     (gloss . "whose radiance is ever-new creation") (form . "adj. f. nom. sg. (bahuvrīhi, agrees with jananī)"))
    ((word . "jananī") (lemma . "jananī") (formation . "√jan-anī") (parts . "√jan (1) be born · -anī f. agent")
     (gloss . "mother") (form . "f. nom. sg."))
    ((word . "bharita-tanuḥ") (lemma . "bharita-tanu") (formation . "√bhṛ-ita · tanu")
     (parts . "√bhṛ (1) bear, be full · -ita ppp; tanu body, nature") (gloss . "whose nature is fullness")
     (form . "adj. nom. sg. (bahuvrīhi, agrees with janakaḥ; by kākākṣi-nyāya also with jananī)"))
    ((word . "ca") (lemma . "ca") (formation . "—") (parts . "") (gloss . "and") (form . "ind."))
    ((word . "pañca-mukha-gupta-ruciḥ") (lemma . "pañca-mukha-gupta-ruci") (formation . "pañca · mukha · √gup-ta · ruci")
     (parts . "pañca five; mukha face, power; √gup (1) protect, maintain · -ta ppp; ruci light, radiance")
     (gloss . "whose light is maintained through five faces") (form . "adj. m. nom. sg. (bahuvrīhi, agrees with janakaḥ)"))
    ((word . "janakaḥ") (lemma . "janaka") (formation . "√jan-aka") (parts . "√jan (1) beget · -aka m. agent")
     (gloss . "father") (form . "m. nom. sg.")))
  "The padapāṭha of TĀ 1.1, segment 1, as Claude gave it on 2.11.")

(ert-deftest bcat-analysis-sanskrit-tables-generic ()
  "Carsten 5.11. on the MacBook (TĀ 1.1): the Tibetan tables of 4.11.
fill the window, the Sanskrit ones stood five on top of each other —
„so prinzipiell anwendbare Pattern … generisch machen und mit
anwenden“.  The Sanskrit tables go through the same layout: the width
of the file (`bcat-analysis-table-width'), the form cut like a Tibetan
function, a wide cell broken at its members (`bcat-analysis-cell-width')
— segment 1 in two tables instead of five, nothing lost."
  (let* ((out (bcat-analysis-sanskrit-tables bcat-analysis-test--ta-1-1-words))
         (tables (split-string out "\n\n")))
    (should (= 2 (length tables)))
    (should (string-prefix-p "| vimala-kalā-āśrayā" (car tables)))
    (dolist (line (split-string out "\n" t))
      (should (<= (string-width line) bcat-analysis-table-width)))
    (should (string-match-p "| adj\\. nom\\. sg\\. +|" out))
    (should-not (string-match-p "kākākṣi" out))))

(ert-deftest bcat-analysis-sanskrit-form-overflow ()
  "What the form cell leaves out goes to the segment's Structure, a line
per word — as for a Tibetan function (4.11.)."
  (should (equal '("/vimala-kalā-āśrayā/: bahuvrīhi, agrees with jananī"
                   "/abhinava-sṛṣṭi-mahāḥ/: bahuvrīhi, agrees with jananī"
                   "/bharita-tanuḥ/: bahuvrīhi, agrees with janakaḥ; by kākākṣi-nyāya also with jananī"
                   "/pañca-mukha-gupta-ruciḥ/: bahuvrīhi, agrees with janakaḥ")
                 (bcat-analysis--function-overflow bcat-analysis-test--ta-1-1-words)))
  (let* ((base '((segments ((num . 1) (samhita . "vimalakalāśrayābhinavasṛṣṭimahā jananī /")))))
         (parsed `((segments ((num . 1) (words . ,(seq-take bcat-analysis-test--ta-1-1-words 4))
                              (structure . "/jananī/ [subject 1]") (translation . "t")))
                   (translation . "t") (grammar . "g")))
         (seg (car (alist-get 'segments (bcat-analysis-sanskrit-context base parsed)))))
    (should (string-prefix-p "/jananī/ [subject 1] \\\\\n/vimala-kalā-āśrayā/: bahuvrīhi"
                             (alist-get 'structure seg)))))

(defun bcat-analysis-test--rows (table)
  "The rows of the org TABLE as lists of trimmed cells, hlines skipped."
  (cl-loop for line in (split-string table "\n" t)
           unless (string-match-p "\\`|-" line)
           collect (mapcar #'string-trim (butlast (cdr (split-string line "|"))))))

(defun bcat-analysis-test--old-sanskrit-tables (words)
  "WORDS in the tables of five rows (word, formation, parts, gloss, form)
the sentence files held before 5.11."
  (bcat-analysis--layout-tables
   (mapcar (lambda (w)
             (append (mapcar (lambda (k) (tibetan-gloss-table--cell (bcat-analysis--field w k)))
                             '(word formation parts gloss))
                     (list (tibetan-gloss-table--cell
                            (car (bcat-analysis--function-split (bcat-analysis--word-function w) "sa"))))))
           words)
   "sa"))

(ert-deftest bcat-analysis-sanskrit-overview ()
  "Carsten 5.11. (draft B): the overview of a Sanskrit segment has three
rows — word, gloss, form (cut as before) —; the formation and parts rows
are gone, the parts have their own tables
\(`bcat-analysis-formation-tables')."
  (let* ((out (bcat-analysis-sanskrit-tables bcat-analysis-test--ta-1-1-words))
         (cols (apply #'append (mapcar #'bcat-analysis--table-columns (split-string out "\n\n")))))
    (should (equal (mapcar (lambda (w) (list (alist-get 'word w) (alist-get 'gloss w)
                                             (car (bcat-analysis--function-split (alist-get 'form w) "sa"))))
                           bcat-analysis-test--ta-1-1-words)
                   cols))))

(ert-deftest bcat-analysis-sanskrit-context-formation ()
  "C-c u G: each Sanskrit segment carries its parts tables as
`formation-tables' beside the overview."
  (let* ((base '((segments ((num . 1) (samhita . "vimalakalāśrayābhinavasṛṣṭimahā jananī /")))))
         (parsed `((segments ((num . 1) (words . ,(seq-take bcat-analysis-test--ta-1-1-words 3))
                              (translation . "t")))
                   (translation . "t") (grammar . "g")))
         (seg (car (alist-get 'segments (bcat-analysis-sanskrit-context base parsed)))))
    (should (equal (bcat-analysis-formation-tables (seq-take bcat-analysis-test--ta-1-1-words 3))
                   (alist-get 'formation-tables seg)))
    (should (equal (bcat-analysis-sanskrit-tables (seq-take bcat-analysis-test--ta-1-1-words 3))
                   (alist-get 'gloss-table seg)))))

(ert-deftest bcat-analysis-split-word-tables ()
  "C-c u L on a file written before 5.11.: the tables of five rows are
read back into words and set as overview and parts tables, what a form
cell left out for Structure; other tables (Tibetan, the overview
itself) are not touched (nil)."
  (let* ((words bcat-analysis-test--ta-1-1-words)
         (old (bcat-analysis-test--old-sanskrit-tables words))
         (split (bcat-analysis-split-word-tables old)))
    (should (equal (bcat-analysis-sanskrit-tables words) (nth 0 split)))
    (should (equal (bcat-analysis-formation-tables
                    (mapcar (lambda (w) (cons (cons 'form (car (bcat-analysis--function-split
                                                                (alist-get 'form w) "sa")))
                                              w))
                            words))
                   (nth 1 split)))
    (should-not (bcat-analysis-split-word-tables (bcat-analysis-sanskrit-tables words)))
    (should-not (bcat-analysis-split-word-tables
                 (bcat-analysis-gloss-table '(((wylie . "chos") (gloss . "dharma") (function . "N"))))))
    (should-not (bcat-analysis-split-word-tables nil))))

(ert-deftest bcat-analysis-formation-tables-ta-1-1 ()
  "Carsten 5.11. on TĀ 1.1 („So wie es jetzt ist ist es für mich schwer
nachvollziehbar“; of three drafts: „B … einzeilig, kursiv“): under the
overview each word with parts gets its own table — a column per member
of the compound, its parts below, one per row, the Sanskrit form in
italics before its meaning; a member that is one bare stem shows only
its meaning.  The formation row is gone: every part was in the table
twice.  A word of one member is a table of one column — the word, its
parts below (Carsten 9.11., TĀ 1009: „der Formatwechsel (Tabelle/
Fließtext) verwirrt mich“; before, a line „/jananī/: …“); a word
without parts (ca) nothing.  Expected text drawn up apart from the
code, from the agreed draft."
  (should (equal "| vimala               | kalā        | āśrayā            |
|----------------------+-------------+-------------------|
| /vi-/ apart          | part, power | /ā-/ unto         |
| /√mal/ (1) be impure |             | /√śri/ (1) resort |
| /-a/ adj             |             | /-a/ whose basis  |

| abhinava             | sṛṣṭi           | mahāḥ                   |
|----------------------+-----------------+-------------------------|
| /abhi-/ toward       | creation (√sṛj) | /mahas/ radiance, glory |
| /nava/ new, ever-new |                 |                         |

| jananī             |
|--------------------|
| /√jan/ (1) be born |
| /-anī/ f. agent    |

| bharita                  | tanuḥ               |
|--------------------------+---------------------|
| /√bhṛ/ (1) bear, be full | /tanu/ body, nature |
| /-ita/ ppp               |                     |

| pañca | mukha       | gupta                        | ruciḥ                  |
|-------+-------------+------------------------------+------------------------|
| five  | face, power | /√gup/ (1) protect, maintain | /ruci/ light, radiance |
|       |             | /-ta/ ppp                    |                        |

| janakaḥ          |
|------------------|
| /√jan/ (1) beget |
| /-aka/ m. agent  |"
                 (bcat-analysis-formation-tables bcat-analysis-test--ta-1-1-words))))

(ert-deftest bcat-analysis-formation-tables-members-from-the-word ()
  "Most answers separate every part by „ · “ — the members of a compound
cannot be read from the parts (395 words in the files on 5.11.: 6 with
„;“ between the members, 82 whose parts match the formation's pieces,
37 that match neither).  The members come from the word as written
\(„śrotra-dhātuḥ“), each part goes to the member its form is a piece of,
in order; a suffix stays with the part before it, a prefix with the one
after.  Parts that are only meanings („my · speech“, TĀ 1007) stand as
they are."
  (let ((rows (lambda (word formation parts)
                (bcat-analysis-test--rows
                 (bcat-analysis-formation-tables
                  `(((word . ,word) (formation . ,formation) (parts . ,parts) (gloss . "g") (form . "f"))))))))
    (should (equal '(("śrotra" "dhātuḥ") ("/√śru/ (5) hear" "/dhātu/ element") ("/-tra/ n.instr." ""))
                   (funcall rows "śrotra-dhātuḥ" "śrotra · dhātu" "√śru (5) hear · -tra n.instr. · dhātu element")))
    (should (equal '(("uddhura" "śakti" "vaibhavam")
                     ("/ud-/ up/forth" "power" "/vi-bhu/ greatness")
                     ("/√dhur/ (1) uphold, overflow" "" "/-a/ action noun")
                     ("/-a/ adj." "" ""))
                   (funcall rows "uddhura-śakti-vaibhavam" "ud-√dhur · śakti · vai-√bhav-a"
                            "ud- up/forth · √dhur (1) uphold, overflow · -a adj. · śakti power · vi-bhu greatness · -a action noun")))
    (should (equal '(("śrīmat" "mahā" "māheśvara" "ācārya" "varya" "śrīmat" "abhinavagupta" "viracitaḥ")
                     ("/śrī-/ fortune" "great" "/māha-/ of Maheśvara" "teacher" "best" "/śrī-/ fortune"
                      "/abhinava/ new" "/vi-/")
                     ("/-mat/ possessing" "" "/-īśvara/ lord" "" "" "/-mat/ possessing" "/gupta/ protected"
                      "/√rac/ (10) compose")
                     ("" "" "/-a/ adj." "" "" "" "" "/-ita/ ppp."))
                   (funcall rows "śrīmat-mahā-māheśvara-ācārya-varya-śrīmat-abhinavagupta-viracitaḥ"
                            "śrī-mat · mahā · māha-īśvara · ācārya · varya · śrī-mat · abhinava-gupta · vi-√rac-ita"
                            "śrī- fortune · -mat possessing · mahā- great · māha- of Maheśvara · -īśvara lord · -a adj. · ācārya teacher · varya best · śrī- fortune · -mat possessing · abhinava new · gupta protected · vi- · √rac (10) compose · -ita ppp.")))
    (should (equal '(("mad" "vāk") ("my" "speech"))
                   (funcall rows "mad-vāk" "mad · vāc" "my · speech")))
    ;; Forms are found without their diacritics (tantra in tantrālokaḥ,
    ;; sva in svātantrya); a root is never the bare member (√cit).
    (should (equal '(("śrī" "tantrālokaḥ") ("auspicious" "/tantra/ tantra") ("" "/ā-/ toward")
                     ("" "/√lok/ (1) see") ("" "/-a/ action noun"))
                   (funcall rows "śrī-tantrālokaḥ" "śrī · tantra · ā-√lok-a"
                            "śrī- auspicious · tantra tantra · ā- toward · √lok (1) see · -a action noun")))
    (should (equal '(("ānanda" "cit" "bhūmayaḥ")
                     ("/ā-/ toward" "/√cit/ (1) consciousness" "/bhūmi/ ground/plane")
                     ("/√nand/ (1) rejoice" "" "") ("/-a/ action noun" "" ""))
                   (funcall rows "ānanda-cit-bhūmayaḥ" "ā-√nand-a · √cit · bhūmi"
                            "ā- toward · √nand (1) rejoice · -a action noun · √cit (1) consciousness · bhūmi ground/plane")))
    ;; On a tie the part before the boundary decides: a suffix stays
    ;; with what precedes it, a prefix or a bare meaning goes with what
    ;; follows (AS 3: „not“ opens adharma).
    (should (equal '(("advaya" "mayam") ("/a-/ not" "/-maya/ made of/full of") ("/dva/ two" "")
                     ("/-ya/ abstract" ""))
                   (funcall rows "advaya-mayam" "a-dvaya · maya" "a- not · dva two · -ya abstract · -maya made of/full of")))
    (should (equal '(("dharma" "adharma" "abhisaṃskāra" "ātma" "vastu" "udbhāvanatām")
                     ("merit" "not" "/abhi-/ toward" "self" "thing" "bringing")
                     ("" "merit" "/sam-/ together" "" "" "")
                     ("" "" "/√kṛ/ (8) make" "" "" ""))
                   (funcall rows "dharma-adharma-abhisaṃskāra-ātma-vastu-udbhāvanatām"
                            "dharma · a-dharma · abhi-sam-√kṛ-a · ātman · √vas-tu · ud-√bhū-ana-tā"
                            "merit · not · merit · abhi- toward · sam- together · √kṛ (8) make · self · thing · bringing")))
    (should (equal '(("alaukika" "āśīrvāda" "mukhena") ("/a-/ not" "/āśīs/ blessing" "/mukha/ face/means")
                     ("/laukika/ worldly" "/√vad/ (1) speak" ""))
                   (funcall rows "alaukika-āśīrvāda-mukhena" "a-laukika · āśīr-vāda · mukha"
                            "a- not · laukika worldly · āśīs blessing · √vad (1) speak · mukha face/means")))
    (should (equal '(("svātantrya" "pūrṇa" "pratham")
                     ("/sva/ own" "full" "/√prath/ (1) spread/expand")
                     ("/tantra/ system/self" "" "/-a/ action noun") ("/-ya/ abstract noun" "" ""))
                   (funcall rows "svātantrya-pūrṇa-pratham" "sva-tantra-ya · pūrṇa · √prath-a"
                            "sva own · tantra system/self · -ya abstract noun · pūrṇa full · √prath (1) spread/expand · -a action noun")))))

(ert-deftest bcat-analysis-formation-tables-nothing-to-add ()
  "No table or line where it would only repeat the overview: no parts
\(ca), one part that is the word itself (kalā) or only a meaning (kāya:
„body“).  A stem other than the word stays (mahaḥ ← mahas)."
  (let ((block (lambda (word parts)
                 (bcat-analysis-formation-tables
                  `(((word . ,word) (formation . "—") (parts . ,parts) (gloss . "g") (form . "f")))))))
    (should (equal "" (funcall block "ca" "")))
    (should (equal "" (funcall block "kalā" "kalā part, power")))
    (should (equal "" (funcall block "kāya" "body")))
    (should (equal "| mahaḥ            |\n|------------------|\n| /mahas/ radiance |"
                   (funcall block "mahaḥ" "mahas radiance")))
    (should (equal "" (bcat-analysis-formation-tables nil)))))

(ert-deftest bcat-analysis-parts-line-table ()
  "C-c u L on a file of 5.11. (draft B): the line of a word of one
member, „/hṛdayam/: /hṛd/ heart · /-aya/ suffix“, becomes its table of
one column, the parts as the line has them (a hand correction stays);
anything else is no such line (nil)."
  (should (equal "| hṛdayam          |\n|------------------|\n| /hṛd/ heart      |\n| /-aya/ my suffix |"
                 (bcat-analysis-parts-line-table "/hṛdayam/: /hṛd/ heart · /-aya/ my suffix")))
  (should (equal "| pārameśvaram               |\n|----------------------------|\n| supreme                    |\n| lord                       |\n| /vṛddhi/ + -a belonging to |"
                 (bcat-analysis-parts-line-table "/pārameśvaram/: supreme · lord · /vṛddhi/ + -a belonging to")))
  (should-not (bcat-analysis-parts-line-table "| a |\n|---|\n| b |"))
  (should-not (bcat-analysis-parts-line-table "*Grammar:* g")))

(ert-deftest bcat-analysis-formation-tables-chunked ()
  "A compound wider than the table width is split into several tables,
each with the members it holds as its header row — none wider than the
width (`bcat-analysis--layout')."
  (let* ((bcat-analysis-layout-overrides '(("sa" (table-width . 40))))
         (out (bcat-analysis-formation-tables
               '(((word . "tad-ubhaya-yāmala-sphurita-bhāva-visarga-mayam") (formation . "—")
                  (parts . "tad those; ubhaya both; yāmala pair, union; vi- forth · √sphur (6) flash · -ita ppp; bhāva state; vi- forth · √sṛj (6) emit · -a m. emission; maya- full of, consisting of")
                  (gloss . "g") (form . "f")))))
         (tables (split-string out "\n\n")))
    (should (> (length tables) 1))
    (dolist (tb tables)
      (should (string-match-p "\\`|[^\n]*\n|-" tb))
      (dolist (line (split-string tb "\n"))
        (should (<= (string-width line) 40)))
      ;; no row of a table empty — the parts of members in another one
      (dolist (row (cdr (bcat-analysis-test--rows tb)))
        (should-not (cl-every #'string-empty-p row))))
    (should (equal '("tad" "ubhaya" "yāmala" "sphurita" "bhāva" "visarga" "mayam")
                   (cl-loop for tb in tables append (car (bcat-analysis-test--rows tb)))))))

(ert-deftest bcat-analysis-layout-generic-with-exceptions ()
  "One width for every language, an exception per source language over
it (`bcat-analysis-layout-overrides') — Carsten 5.11.: „alles erstmal
generisch und dann durch Ausnahmen überschrieben“."
  (should (= 158 (eval (car (get 'bcat-analysis-table-width 'standard-value)))))
  (should (= 45 (eval (car (get 'bcat-analysis-cell-width 'standard-value)))))
  (should (eq 'bcat-analysis-table-width (indirect-variable 'bcat-analysis-tibetan-table-width)))
  (let ((bcat-analysis-layout-overrides '(("sa" (table-width . 60)))))
    (should (= 60 (bcat-analysis--layout 'table-width "sa")))
    (should (= 158 (bcat-analysis--layout 'table-width "bo")))
    (dolist (line (split-string (bcat-analysis-sanskrit-tables bcat-analysis-test--ta-1-1-words) "\n" t))
      (should (<= (string-width line) 60)))
    (should (= 1 (length (split-string
                          (bcat-analysis-gloss-table
                           '(((wylie . "chos nyid") (gloss . "nature of phenomena") (function . "N · goal/locus"))
                             ((wylie . "'di") (gloss . "this") (function . "DEM · specifying"))
                             ((wylie . "la") (gloss . "toward / into") (function . "DAT/LOC"))))
                          "\n\n"))))))

(ert-deftest bcat-analysis-tables-fill-the-window ()
  "Carsten 4.11. (bDen 16, Segment 70): „müsste eigentlich nicht
umbrechen, oder?“ — six columns, 119 characters as the table stands,
in a window of 162; it broke because the width was the PDF's (6 per
column, limit 130: 137).  In the file a table is as wide as it is seen
\(cells + 3 per column), up to `bcat-analysis-table-width'
\(158, the MacBook window; 5.11. every language); the PDF chunks anew in the export
\(`bcat-export--rechunk-gloss-tables')."
  (should (= 158 (eval (car (get 'bcat-analysis-table-width 'standard-value)))))
  (let* ((cols '(("ཆོས་ཉིད" "chos nyid" "nature of phenomena" "N · goal/locus")
                 ("འདི" "'di" "this" "DEM · specifying")
                 ("ལ" "la" "toward / into" "DAT/LOC")
                 ("གཞོལ" "gzhol" "incline toward" "V pres.")
                 ("ཞིང" "zhing" "and" "CONV · coordinative")
                 ("འབབ" "'bab" "flow into / lead to" "V pres. · main verb"))))
    (should (= 1 (length (bcat-analysis--chunk-columns cols))))
    (should (= 2 (length (bcat-analysis--chunk-columns cols 130 6))))))

(ert-deftest bcat-analysis-compact-tables ()
  "C-c u L on a file written before 4.11.: the long function cells are
cut, the rest comes back for Structure, and the columns are chunked
anew — „gis“ joins the first table (bDen 16).  Tables without a long
cell stay as they are."
  (let* ((old (concat "| དེ                  | ལས   | བརྒྱུད་པའི                                                           | མན་ངག               |\n"
                      "| [[steinert:de][de]]                 | las  | [[steinert:brgyud pa'i][brgyud pa'i]]                                                      | [[steinert:man ngag][man ngag]]            |\n"
                      "| that/him           | from | transmitted/lineage of                                           | pith instructions   |\n"
                      "| DEM · oblique base | ABL  | V pf. (brgyud pa) · relative clause, GEN attributive to man ngag | N · instrument base |\n\n"
                      "| གིས          |\n| gis         |\n| by means of |\n| INST        |"))
         (out (bcat-analysis-compact-tables old)))
    (should (equal '("/brgyud pa'i/: relative clause, GEN attributive to man ngag") (cdr out)))
    (should-not (string-match-p "\n[ \t]*\n" (car out)))
    (should (string-match-p "\\`| དེ +| ལས +| བརྒྱུད་པའི +| མན་ངག +| གིས +|\n" (car out)))
    (should (string-match-p "| V pf\\. +| N · instrument base +| INST +|" (car out)))
    (let ((plain "| བརྗོད     |\n| brjod     |\n| say       |\n| V         |"))
      (should (equal (cons plain nil) (bcat-analysis-compact-tables plain))))))

(ert-deftest bcat-analysis-sigla-only-in-units ()
  "Bug (4.11., found in bDen 16 after C-c u L): since the user prompt
names the sigla of the comparison (5905615), Claude names the segment
parts of the references by siglum — „*L:* All of them …“, „→ L: …“ — and
the marks of Translations compared were lost: the parts are looked up
by the full name („Lindtner 1981“).  9 files since 2.11.  The prompt
keeps the sigla to the units; the answer's sigla are named again."
  (let ((user (bcat-analysis-user-prompt 9 nil '((38 "ཀ" "ka")) '(("Lindtner 1981" . "x")))))
    (should (string-match-p "only in the units" user)))
  (should (string-match-p "name: the full name as given" (bcat-analysis--tibetan-segment-rules "English")))
  (let* ((refs '(("Lindtner 1981" . "It is inexpressible. When a yogi understands it he abandons.")
                 ("DharmaMitra" . "Inexpressible. When a yogi realizes this.")))
         (parsed (bcat-analysis-parse
                  (concat "{\"segments\": [{\"num\": 38, \"words\": [], \"translation\": \"t\","
                          " \"references\": [{\"name\": \"L\", \"text\": \"*When* a yogi understands it\"},"
                          " {\"name\": \"DM\", \"text\": \"*When* a yogi realizes this.\"}],"
                          " \"shift\": \"L, DM: temporal for conditional\"}], \"translation\": \"t\", \"grammar\": \"g\"}")))
         (ctx (bcat-analysis-context '((segments ((num . 38) (wylie . "ka")))) parsed refs))
         (block (alist-get 'translations (car (alist-get 'segments ctx)))))
    (should (string-match-p (regexp-quote "*Lindtner 1981:* *When* a yogi understands it") block))
    (should (string-match-p (regexp-quote "*DharmaMitra:* *When* a yogi realizes this.") block))
    (should (string-match-p (regexp-quote "→ Lindtner 1981, DharmaMitra: temporal for conditional") block))
    (should (string-match-p (regexp-quote "*Lindtner 1981:* It is inexpressible. *When* a yogi understands it")
                            (alist-get 'comparison ctx)))))

(ert-deftest bcat-analysis-footnotes-last-before-headings ()
  "The footnote definitions of the references go to the end of the text
before the first heading — the units of the comparison (*** Übersicht …)
stay where they are; C-c u L reads the whole section with them."
  (should (equal "A [fn:x]\n\nB\n\n[fn:x] n.\n\n*** Übersicht\nT\n\nU"
                 (bcat-analysis-footnotes-last "A [fn:x]\n\n[fn:x] n.\n\nB\n\n*** Übersicht\nT\n\nU")))
  (should (equal "A\n\n*** Übersicht\nT"
                 (bcat-analysis-footnotes-last "A\n\n*** Übersicht\nT"))))

(provide 'bcat-analysis-test)
;;; bcat-analysis-test.el ends here
