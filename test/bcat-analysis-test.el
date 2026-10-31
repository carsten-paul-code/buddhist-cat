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
                   "\"grammar\"" "\"comparison\""))
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

(ert-deftest bcat-analysis-second-language ()
  "Carsten 31.10. abends, TĀ 1006: „Darunter hätte ich dann gerne die
Übersetzung in deutsch, damit ich mich leichter in die Konzepte rein
denken kann (wenn ich in deutsch übersetze, dann entspr. in Englisch)“
and on the references: „Bei der Unterscheidung auch was das auf Deutsch
heißen würde“.  Every analysis prompt asks for the sentence translation
in the other language (translation_de for English, translation_en for
German) and, in the comparison, the differences in that language too;
the field lands under the translation as *Deutsch:* / *English:*."
  (dolist (sys (list (bcat-analysis-sanskrit-system-prompt "en")
                     (bcat-analysis-system-prompt "en")
                     (bcat-analysis-tibetan-system-prompt "en")))
    (should (string-match-p "\"translation_de\"" sys))
    (should (string-match-p "translation_de: the sentence translation once more in German" sys))
    (should (string-match-p "what each reading would be in German" sys)))
  (dolist (sys (list (bcat-analysis-sanskrit-system-prompt "de")
                     (bcat-analysis-system-prompt "de")))
    (should (string-match-p "\"translation_en\"" sys))
    (should-not (string-match-p "translation_de" sys))
    (should (string-match-p "what each reading would be in English" sys)))
  ;; The small request for a reference added by hand compares too.
  (should (string-match-p "what each reading would be in German" (bcat-analysis-align-system-prompt "en")))
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
the table to an unreadable size (1f6da5b).  A cell wider than the table
width breaks at its „·“ into further lines of the table — nothing cut,
no line wider than `bcat-analysis-sanskrit-table-width'."
  (let* ((parts "√vac (2) speak · -ṣyamāṇa fut. pass. part. · ṣaḍ six · ardha half · śāstra treatise · artha meaning · garbhī- womb/containing · √kṛ (8) make · -a action noun")
         (table (bcat-analysis-sanskrit-tables
                 `(((word . "vakṣyamāṇa-ṣaḍardha-śāstra-artha-garbhīkāreṇa")
                    (formation . "√vac · ṣaḍ-ardha · śāstra · artha · garbhī-√kṛ-a")
                    (parts . ,parts)
                    (gloss . "by incorporating the meaning of the Ṣaḍardhaśāstra to be expounded")
                    (form . "m. inst. sg.")))))
         (lines (split-string table "\n" t)))
    (should (cl-every (lambda (l) (<= (string-width l) bcat-analysis-sanskrit-table-width)) lines))
    (should (= 1 (length (split-string table "\n\n"))))
    (should (= 6 (length lines)))
    ;; Nothing lost: the parts, read across their lines, are the cell.
    (should (equal parts
                   (replace-regexp-in-string
                    " *\n *" " "
                    (mapconcat (lambda (l) (string-trim (string-trim l "|" "|")))
                               (seq-subseq lines 2 4) "\n"))))))

(ert-deftest bcat-analysis-sanskrit-tables ()
  "The word table of a Sanskrit segment (word / formation / parts /
gloss / form) is split into separate tables, none wider than
`bcat-analysis-sanskrit-table-width' — a shared column grid made the
long compound widen every column (29.10.); nothing is cut."
  (let* ((bcat-analysis-sanskrit-table-width 40)
         (words '(((word . "atra") (formation . "—") (parts . "") (gloss . "here") (form . "ind."))
                  ((word . "ullaset") (formation . "ud-√las") (parts . "ud- up · √las (1) shine")
                   (gloss . "may shine forth") (form . "opt. 3 sg. P"))
                  ((word . "api") (formation . "—") (parts . "") (gloss . "although") (form . "ind."))))
         (out (bcat-analysis-sanskrit-tables words))
         (tables (split-string out "\n\n")))
    (should (= 2 (length tables)))
    (should (string-prefix-p "| atra | ullaset" (nth 0 tables)))
    (should (string-prefix-p "| api" (nth 1 tables)))
    (should (string-match-p "ud- up · √las (1) shine" out))
    (dolist (tb tables)
      (dolist (row (split-string tb "\n"))
        (should (<= (string-width row) 40))))
    (should (equal "" (bcat-analysis-sanskrit-tables nil)))))

(ert-deftest bcat-analysis-sanskrit-table-width-default ()
  "Carsten 30.10.: the tables as in the reviewed preview of AS sentence
3 — compound and upādāya side by side.  Measured in its PDF: a table of
115 characters (segment 24) fits the page, one of 134 (segment 27) runs
into the margin.  With the default width segments 23 and 24 keep one
table, segment 27 is split."
  (let ((segments (alist-get 'segments
                             (bcat-analysis-parse (bcat-analysis-test--answer-003 "asam")))))
    (cl-flet ((tables (n) (length (split-string
                                   (bcat-analysis-sanskrit-tables
                                    (alist-get 'words (cl-find n segments
                                                               :key (lambda (s) (alist-get 'num s)))))
                                   "\n\n"))))
      (should (= 1 (tables 23)))
      (should (= 1 (tables 24)))
      (should (= 2 (tables 27))))))

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
in order, none wider than `bcat-analysis-tibetan-table-width' (each
column its widest cell + 6); a short segment stays one table."
  (let* ((words (cl-loop for i from 1 to 23
                         collect `((wylie . ,(format "rnam par shes %02d" i))
                                   (gloss . "consciousness")
                                   (function . "N · ABS"))))
         (table (bcat-analysis-gloss-table words))
         (tables (split-string table "\n\n")))
    ;; 6 + 16 = 22 per column: five columns (111) fit in 130, six (133) do not.
    (should (= 5 (length tables)))
    (should (equal '(5 5 5 5 3)
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

(ert-deftest bcat-analysis-num-ranges ()
  "Segment numbers for a heading: runs as ranges, gaps by comma."
  (should (equal "77" (bcat-analysis--num-ranges '(77))))
  (should (equal "40–44" (bcat-analysis--num-ranges '(40 41 42 43 44))))
  (should (equal "77, 79–80" (bcat-analysis--num-ranges '(77 79 80))))
  (should (equal "" (bcat-analysis--num-ranges nil))))

(ert-deftest bcat-analysis-bilingual-assignment ()
  "AS sentence 2: Tibetan segments finer than the Sanskrit (7 against
41).  Each Sanskrit segment gets the Tibetan segments Claude assigns to
it — heading „ · Tib. 36–38“, Uchen over glossed Wylie; a Tibetan
segment without a valid assignment goes with the one before it; a
Sanskrit segment without Tibetan has none; two differences of one
segment are joined, a segment without any gets none."
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
    (should (equal (concat "U36\nphung po [aggregate] rnams [plural] ni du [how many] /\n"
                           "U37\nkhams rnams ni du/\n"
                           "U38\nskye mched rnams ni du zhe na/")
                   (alist-get 'tibetan-block (nth 0 segs))))
    (should (equal "" (alist-get 'tibetan-block (nth 2 segs))))
    (should (equal "One question in Tibetan, three in Sanskrit. /zhe na/ closes it."
                   (alist-get 'differences (nth 0 segs))))
    (should (equal "" (alist-get 'differences (nth 1 segs))))
    (should (equal "Close." (alist-get 'rendering ctx)))
    (should (alist-get 'tibetan-glosses ctx))
    (should (equal "phung po [aggregate] rnams [plural] ni du [how many] /"
                   (alist-get 'glossed (car (alist-get 'tibetan ctx)))))))

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
                   "\"comparison\"" "\"rendering\""))
      (should (string-match-p (regexp-quote key) sys)))
    (should (string-match-p (regexp-quote (bcat-analysis--tibetan-rules "German" "de")) sys))
    (should (string-match-p "never decrease" sys))
    (should (string-match-p "goes with the first" sys))
    (should (string-match-p "No entry where the Tibetan renders the Sanskrit faithfully" sys))
    (should (string-match-p "/italic/" sys))
    (should (string-match-p "never a backslash before an apostrophe" sys))
    (should (string-match-p "in German" sys))
    (should (string-match-p (regexp-quote "Asaṅga, Abhidharmasamuccaya\n\nSentence 3 — Sanskrit segments:\n[22] kasmāt pañcaiva skandhāḥ.\n     Translation: Why are there just five aggregates? —\n\nTibetan segments:\n[77] Tibetan: ཅིའི་ཕྱིར། །\n     Wylie: ci'i phyir/ /\n\nReference translations:\n=== Rahula/Boin-Webb 2001 ===\nWhy are there only five aggregates?\n\nReturn the JSON object now.") user))))

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
                     tibetan (bcat-analysis-test--sanskrit-003)))))))

(ert-deftest bcat-analysis-partner-assignment ()
  "AS sentence 2 seen from the Tibetan: Tibetan 36–38 translate Sanskrit
15, 39 translates 16.  Each Tibetan segment names its Sanskrit one; the
Sanskrit lines and the differences stand once, at the first segment of
the group; a Tibetan segment without a valid assignment goes with the
one before; the Sanskrit glossed line falls back to the line as
transmitted."
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
    (should (equal '("D15\nkati [how many] skandhāḥ [aggregates]." "" "" "D16\npañca skandhāḥ.")
                   (mapcar (lambda (s) (alist-get 'sanskrit-block s)) segs)))
    (should (equal '("Three questions in Sanskrit." "" "" "")
                   (mapcar (lambda (s) (alist-get 'differences s)) segs)))
    (should (alist-get 'sanskrit-p ctx))
    (should (alist-get 'sanskrit-glosses ctx))
    (should (equal "Close." (alist-get 'rendering ctx)))))

(provide 'bcat-analysis-test)
;;; bcat-analysis-test.el ends here
