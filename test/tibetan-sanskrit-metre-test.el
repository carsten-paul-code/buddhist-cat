;;; tibetan-sanskrit-metre-test.el --- Tests for Sanskrit scansion and metre -*- lexical-binding: t -*-

;;; Commentary:
;; §5.65 (2026-10-21).  Carsten (Tantrāloka-Reading, nach Satz 1001):
;; „Es geht mit einer Rezitation los, da bin ich mit der Metrik nicht
;; klar gekommen.  Ginge es, dass wir eine Kennzeichnung der Pausen
;; beim Rezitieren reinbekommen?"  Pure tests on the class's own
;; verses, scanned by hand: Jayaratha's opening verses (Śārdūla-
;; vikrīḍita 1001, Śloka 1002, Vasantatilakā 1004), TĀ 1.1 (Nardaṭaka),
;; TĀ 1.9 (Upagīti, 27 + 27 morae).  Patterns and yati: Macdonell,
;; Sanskrit Grammar, App. II.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((base-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../analysis" base-dir)))

(require 'tibetan-sanskrit-metre)

(defconst tibetan-metre-test--1001
  '("yasmādeṣaṇavitkriyā yaduditā hyānandacidbhūmayo"
    "yasyaivoddhuraśaktivaibhavamidaṃ sarvaṃ yadevaṃvidham /"
    "taddhāma trikatattvamadvayamayaṃ svātantryapūrṇaprathaṃ"
    "citte stācchivaśāsanāgamarahasyācchādanadhvaṃsi me // 1")
  "Jayaratha's first opening verse (Sentence 1001), one pāda per line.")

(defconst tibetan-metre-test--1002
  '("dehe vimukta evāsmi śrīmatkalyāṇavāridheḥ /"
    "yasya kāruṇyavipruḍbhiḥ sadguruṃ taṃ hṛdi śraye // 2")
  "Sentence 1002: a śloka, one half-verse per line.")

(defconst tibetan-metre-test--1004
  '("na granthakārapadamāptumathāsmyapūrvaṃ"
    "vākkauśalaṃ ca na nidarśayituṃ pravṛttaḥ /"
    "kiṃ tvetadarthapariśīlanato vikalpaḥ"
    "saṃskāravāṃśca samiyāditi vāñchitaṃ naḥ // 4")
  "Sentence 1004: Vasantatilakā.")

(defconst tibetan-metre-test--ta1
  '("vimalakalāśrayābhinavasṛṣṭimahā jananī bharitatanuśca pañcamukhaguptarucirjanakaḥ /"
    "tadubhayayāmalasphuritabhāvavisargamayaṃ hṛdayamanuttarāmṛtakulaṃ mama sasphuratāt // 1")
  "TĀ 1.1: Nardaṭaka, two pādas per line.")

(defconst tibetan-metre-test--ta9
  '("jayati gurureka eva śrīśrīkaṇṭho bhuvi prathitaḥ /"
    "tadaparamūrtirbhagavān maheśvaro bhūtirājaśca // 9")
  "TĀ 1.9: Upagīti (āryā family, 27 + 27 morae).")

;; ---------------------------------------------------------------- scansion

(ert-deftest tibetan-metre-scansion-rules ()
  "Heavy (G) = long vowel, or anusvāra/visarga, or two+ consonants
before the next vowel — across word boundaries; aspirates (bh, kh …)
are ONE consonant; avagraha makes no syllable; daṇḍas and the verse
number are no syllables."
  (should (equal (tibetan-metre-scheme
                  "yasmādeṣaṇavitkriyā yaduditā hyānandacidbhūmayo")
                 "GGGLLGLGLLLGGGLGGLG"))
  (should (equal (mapcar (lambda (s) (plist-get s :text))
                         (tibetan-metre-syllables "yasmādeṣaṇa"))
                 '("yas" "mā" "de" "ṣa" "ṇa")))
  (should (equal (tibetan-metre-scheme "sukham") "LG"))      ; kh = 1 C
  (should (equal (tibetan-metre-scheme "idaṃ sarvaṃ") "LGGG"))
  (should (equal (tibetan-metre-scheme "ke'pi") "GL"))       ; avagraha
  (should (equal (tibetan-metre-scheme "ma tri") "GL"))      ; across space
  (should (equal (tibetan-metre-scheme "/ śraye // 2") "LG"))
  (should (equal (tibetan-metre-scheme "va sṛṣṭi") "LGL")))  ; ṛ vowel

;; ---------------------------------------------------------------- metres

(ert-deftest tibetan-metre-identify-sardulavikridita ()
  "1001: four pādas of 19, yati after the 12th syllable."
  (let ((m (tibetan-metre-identify tibetan-metre-test--1001)))
    (should (equal (plist-get m :name) "Śārdūlavikrīḍita"))
    (should (equal (plist-get m :yati) '(12)))
    (should (= 4 (length (plist-get m :padas))))
    (should (cl-every (lambda (p) (= 19 (length p))) (plist-get m :padas)))))

(ert-deftest tibetan-metre-identify-sloka ()
  "1002: śloka — two half-verse lines, pādas of 8 (even pādas
⏑ – ⏑ in 5–7)."
  (let ((m (tibetan-metre-identify tibetan-metre-test--1002)))
    (should (string-prefix-p "Śloka" (plist-get m :name)))
    (should (= 4 (length (plist-get m :padas))))
    (should-not (plist-get m :yati))))

(ert-deftest tibetan-metre-identify-vasantatilaka-and-nardataka ()
  "No fixed caesura: only the pāda ends are marked."
  (let ((v (tibetan-metre-identify tibetan-metre-test--1004))
        (n (tibetan-metre-identify tibetan-metre-test--ta1)))
    (should (equal (plist-get v :name) "Vasantatilakā"))
    (should-not (plist-get v :yati))
    (should (equal (plist-get n :name) "Nardaṭaka"))
    (should (= 4 (length (plist-get n :padas))))))

(ert-deftest tibetan-metre-identify-arya-family ()
  "TĀ 1.9: morae 27 + 27 = Upagīti; caesura after the third foot
\(12 morae) in each half."
  (let ((m (tibetan-metre-identify tibetan-metre-test--ta9)))
    (should (equal (plist-get m :name) "Upagīti"))
    (should (equal (tibetan-metre-mark-segments tibetan-metre-test--ta9)
                   '("jayati gurureka eva ¦ śrīśrīkaṇṭho bhuvi prathitaḥ /"
                     "tadaparamūrtirbhagavān ¦ maheśvaro bhūtirājaśca // 9")))))

(ert-deftest tibetan-metre-prose-is-not-verse ()
  "A prose sentence (one long line) is no metre — even with a
syllable count divisible by four."
  (should-not (tibetan-metre-identify
               '("iha khalu śāstrādāvalaukikāśīrvādamukhena vakṣyamāṇaṣaḍardhaśāstrārthagarbhīkāreṇa samuciteṣṭadevatāṃ śāstrakāraḥ parāmṛśati")))
  (should-not (tibetan-metre-section
               '("iha khalu śāstrādāvalaukikāśīrvādamukhena"))))

;; ---------------------------------------------------------------- survey fixes

(ert-deftest tibetan-metre-prose-with-sloka-count-is-not-verse ()
  "Survey of TĀ 1 (2026-10-21): Viveka PROSE with exactly 32/48
syllables (Sätze 1081, 1099, 1106) was taken for a śloka.  One line
without a closing `//' is no verse."
  (should-not (tibetan-metre-identify
               '("iti nītyā bhīmo bhīmasenaḥ itivat atrāpi narasiṃhaguptasiṃhaguptapadayoḥ prayogaḥ /")))
  (should-not (tibetan-metre-identify
               '("atra ca saṃbhavantyapi vyākhyāntarāṇi na kṛtāni, granthagauravabhayāt prakṛtānupayogācca /")))
  ;; a long prose sentence ending in `//' gets no section either
  (should-not (tibetan-metre-section
               '("atha ca hṛdayaṃ nijabalasamudbhūtilakṣaṇaṃ tattvaṃ viśeṣānupādānāt sarvasya samyak prakhyopākhyāroheṇa sphuratāt vikasatāt ityarthaḥ //"))))

(ert-deftest tibetan-metre-half-sloka-line ()
  "TĀ 1.225 is a single half-verse line closed by `//'."
  (let ((m (tibetan-metre-identify
            '("yena buddhi-manobhūmāvapi bhāti paraṃ padam // 225"))))
    (should (string-prefix-p "Śloka" (plist-get m :name)))
    (should (= 2 (length (plist-get m :padas))))))

(ert-deftest tibetan-metre-aupacchandasika ()
  "TĀ 1.13: Aupacchandasika — mora-defined ardhasama (odd 6, even 8
morae + ra ya); each line holds an odd and an even pāda."
  (let ((v '("jayatājjagaduddhṛtikṣamo'sau bhagavatyā saha śaṃbhunātha ekaḥ /"
             "yadudīritaśāsanāṃśubhirme prakaṭo'yaṃ gahano'pi śāstramārgaḥ // 13")))
    (should (equal (plist-get (tibetan-metre-identify v) :name) "Aupacchandasika"))
    (should (equal (car (tibetan-metre-mark-segments v))
                   "jayatājjagaduddhṛtikṣamo'sau | bhagavatyā saha śaṃbhunātha ekaḥ /"))))

(ert-deftest tibetan-metre-aryagiti ()
  "TĀ 1.21: 32 + 32 morae = Āryāgīti; caesura after 12 morae, here
inside the compound (bhāskara¦caraṇa)."
  (let ((v '("śrīśambhunāthabhāskaracaraṇanipātaprabhāpagatasaṃkocam /"
             "abhinavaguptahṛdambujametadvicinuta maheśapūjanahetoḥ // 21")))
    (should (equal (plist-get (tibetan-metre-identify v) :name) "Āryāgīti"))
    (should (equal (car (tibetan-metre-mark-segments v))
                   "śrīśambhunāthabhāskara¦caraṇanipātaprabhāpagatasaṃkocam /"))))

;; ---------------------------------------------------------------- marks

(ert-deftest tibetan-metre-mark-pauses ()
  "Yati at a word boundary: ` ¦ '; inside a word at the syllable
boundary (the first consonant of a cluster closes the syllable:
rahas¦yā); pāda end inside a line: ` | '.  Daṇḍas stay."
  (should (equal (tibetan-metre-mark-segments tibetan-metre-test--1001)
                 '("yasmādeṣaṇavitkriyā yaduditā ¦ hyānandacidbhūmayo"
                   "yasyaivoddhuraśaktivaibhavamidaṃ ¦ sarvaṃ yadevaṃvidham /"
                   "taddhāma trikatattvamadvayamayaṃ ¦ svātantryapūrṇaprathaṃ"
                   "citte stācchivaśāsanāgamarahas¦yācchādanadhvaṃsi me // 1")))
  (should (equal (tibetan-metre-mark-segments tibetan-metre-test--1002)
                 '("dehe vimukta evāsmi | śrīmatkalyāṇavāridheḥ /"
                   "yasya kāruṇyavipruḍbhiḥ | sadguruṃ taṃ hṛdi śraye // 2")))
  (should (equal (car (tibetan-metre-mark-segments tibetan-metre-test--ta1))
                 "vimalakalāśrayābhinavasṛṣṭimahā jananī | bharitatanuśca pañcamukhaguptarucirjanakaḥ /")))

(ert-deftest tibetan-metre-section-renders ()
  "The `** Metre' section: name + pattern + pause rule, the marked
verse, a syllable table (syllables over – / ⏑, ¦ column at the yati).
Unrecognised verse (`//' at the end) → a visible note with the bare
scansion per line, never silence."
  (let ((s (tibetan-metre-section tibetan-metre-test--1001)))
    (should (string-prefix-p "** Metre\n" s))
    (should (string-match-p "Śārdūlavikrīḍita" s))
    (should (string-match-p "ma sa ja sa ta ta ga" s))
    (should (string-match-p "nach der 12\\. Silbe" s))
    (should (string-match-p "^yasmādeṣaṇavitkriyā yaduditā ¦ hyānandacidbhūmayo$" s))
    ;; ONE table per verse, columns shared by all pādas: the same
    ;; syllable position stands in one column, the pattern repeats
    ;; visibly; org-aligned (all rows the same width).
    (should (string-match-p "^| a +| yas +| mā +| de +|" s))
    (should (string-match-p "| tā +| ¦ +| hyā +|" s))
    (should (string-match-p "^| +| – +| – +| – +| ⏑ +| ⏑ +| – +|" s))
    (should (string-match-p "^| d +| cit +|" s))
    (let ((rows (cl-remove-if-not (lambda (l) (string-prefix-p "|" l))
                                  (split-string s "\n"))))
      (should (= 8 (length rows)))
      (should (= 1 (length (delete-dups
                            (mapcar #'string-width rows)))))))
  (let ((s (tibetan-metre-section '("abc def ghi /" "jkl // 3"))))
    (should (string-match-p "nicht erkannt" s))))

(ert-deftest tibetan-metre-label-and-table ()
  "Public pieces for printouts: a one-line label (name · pause rule)
and the syllable table; nil for prose."
  (should (equal (tibetan-metre-label tibetan-metre-test--1001)
                 "Śārdūlavikrīḍita · Pause (yati) nach der 12. Silbe"))
  (should (equal (tibetan-metre-label tibetan-metre-test--1002)
                 "Śloka (Anuṣṭubh) · keine feste Binnenpause — Pause am Pāda-Ende"))
  (should (string-prefix-p "| a " (tibetan-metre-table tibetan-metre-test--1001)))
  (should-not (tibetan-metre-label '("iha khalu śāstrādāv alaukika /")))
  (should-not (tibetan-metre-table '("iha khalu śāstrādāv alaukika /"))))

(ert-deftest tibetan-metre-recitation-parts ()
  "The recitation in parts for the sentence template (§5.74, Carsten
29.10.: „Es geht ja mit der Rezitation los“): header lines (metre with
pattern and pause rule, the sign legend), the verse with its pauses,
the scansion table — with BARE without the pāda letters a–d, which
„verwirren eher als sie helfen“.  An unrecognised verse: the note and
the bare scansion as an example block, no verse; prose: nil."
  (let* ((lines '("atra madvāgaśaktāpi yanniyantraṇamullaset /"
                  "tatpārameśvaraṃ śrīmanmahānandavijṛmbhitam // 7"))
         (r (tibetan-metre-recitation lines t)))
    (should (equal (list (concat "Śloka (Anuṣṭubh) — vier Pādas zu 8 Silben; in den geraden "
                                 "Pādas Silbe 5–7 ⏑ – ⏑; keine feste Binnenpause — Pause am Pāda-Ende.")
                         (concat "Zeichen: ¦ Pause (yati) · | Pāda-Ende in der Zeile · – schwer "
                                 "(guru) · ⏑ leicht (laghu)"))
                   (plist-get r :header)))
    (should (equal (concat "atra madvāgaśaktāpi | yanniyantraṇamullaset /\n"
                           "tatpārameśvaraṃ śrīman|mahānandavijṛmbhitam // 7")
                   (plist-get r :verse)))
    (should (string-prefix-p "| at  | ra | mad |" (plist-get r :table)))
    (should (string-prefix-p "| a | at  |" (plist-get (tibetan-metre-recitation lines) :table))))
  (let ((r (tibetan-metre-recitation '("abc def ghi /" "jkl // 3") t)))
    (should (string-match-p "nicht erkannt" (car (plist-get r :header))))
    (should-not (plist-get r :verse))
    (should (string-prefix-p "#+begin_example\n" (plist-get r :table))))
  (should-not (tibetan-metre-recitation '("iha khalu śāstrādāv alaukika /") t)))

(provide 'tibetan-sanskrit-metre-test)
;;; tibetan-sanskrit-metre-test.el ends here
