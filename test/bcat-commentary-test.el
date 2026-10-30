;;; bcat-commentary-test.el --- Tests for verses with commentary (MSA)  -*- lexical-binding: t; -*-

;;; Commentary:
;; §5.74, Carsten 30.10.: the MSA reading — Sanskrit and Tibetan verse
;; read together, the bhāṣya below the verse („als zusätzliche Section
;; (unten) mit Text und hint“), Sthiramati's commentary as a document of
;; its own, both linked; C-c u G analyses both and puts both on the
;; reMarkable.  Fixture: a small MSA trio (Sanskrit, Tibetan, Sthiramati)
;; with the real text of IX.16, IX.15 and IX.17 short.  The sources do
;; not share sentence numbers — only Sections and :VERSE:.  Temp dirs
;; only; no network; no AI call.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'bcat-sentence-test)
(require 'bcat-commentary)

(defconst bcat-commentary-test--sanskrit
  (concat "#+TITLE: Mahāyānasūtrālaṃkāra IX mit Bhāṣya — CAT-Quelle (Sanskrit)\n"
          "#+TIBETAN_LAYOUT: cascade\n"
          "#+CAT_TEMPLATE: bilingual\n"
          "#+CAT_PARALLEL: ../Tibetisch/mdo-cat.org\n"
          "#+CAT_COMMENTARY: ../Sthiramati/savbh-cat.org\n"
          "#+SOURCE_LANG: sa\n"
          "#+TIBETAN_TARGET_LANG: en\n"
          "#+TIBETAN_SENTENCE_REFS: refs.org\n"
          "#+TIBETAN_CLAUDE_CONTEXT: Asaṅga, Mahāyānasūtrālaṃkāra IX (bodhi), with the bhāṣya.\n\n"
          "* Sanskrit Text\n"
          "** Section IX.15\n"
          "*** Sentence 34\n:PROPERTIES:\n:VERSE: MSA IX.15\n:END:\n"
          "**** Segment 88\npañca skandhāḥ /\n"
          "**** Segment 89\naṣṭādaśa dhātavaḥ // 15\n"
          "** Section IX.16\n"
          "*** Sentence 35\n"
          "**** Segment 91\natha pañcamaḥ ślokaḥ /\n"
          "*** Sentence 36\n:PROPERTIES:\n:VERSE: MSA IX.16\n:END:\n"
          "**** Segment 92\nyathodabhājane bhinne candrabimbaṃ na dṛśyate /\n"
          "**** Segment 93\ntathā duṣṭeṣu sattveṣu buddhabimbaṃ na dṛśyate // 16\n"
          "*** Sentence 37\n"
          "**** Segment 94\nanena pañcamena sarvagatatve 'pyabhājanabhūteṣu sattveṣu abuddhabimbadarśanaṃ dṛṣṭāntena sādhayati /\n"
          "** Section IX.17\n"
          "*** Sentence 38\n:PROPERTIES:\n:VERSE: MSA IX.17\n:END:\n"
          "**** Segment 95\ndvādaśāyatanāni // 17\n")
  "Sanskrit: IX.15 (verse), IX.16 (intro, verse, bhāṣya), IX.17 (verse).")

(defconst bcat-commentary-test--tibetan
  (concat "#+TITLE: mdo sde'i rgyan, Kap. IX — CAT-Quelle\n"
          "#+TIBETAN_LAYOUT: cascade\n"
          "#+CAT_TEMPLATE: bilingual-tibetan\n"
          "#+CAT_PARALLEL: ../Sanskrit/msa-cat.org\n"
          "#+TIBETAN_TARGET_LANG: en\n"
          "#+TIBETAN_CLAUDE_CONTEXT: mdo sde'i rgyan (D4026), Tibetan of the MSA with the bhāṣya.\n\n"
          "* Tibetan Text\n"
          "** Section IX.15\n"
          "*** Sentence 62\n:PROPERTIES:\n:VERSE: MSA IX.15\n:END:\n"
          "**** Segment 135\nཕུང་པོ་ལྔ། །\n"
          "**** Segment 136\nཁམས་བཅོ་བརྒྱད། །\n"
          "** Section IX.16\n"
          "*** Sentence 64\n:PROPERTIES:\n:VERSE: MSA IX.16\n:END:\n"
          "**** Segment 139\nཇི་ལྟར་ཆུ་ཡི་སྣོད་ཆག་ན། །\n"
          "**** Segment 140\nཟླ་བའི་གཟུགས་བརྙན་མི་སྣང་བ། །\n"
          "**** Segment 141\nདེ་བཞིན་སེམས་ཅན་ངན་པ་ལ། །\n"
          "**** Segment 142\nསངས་རྒྱས་གཟུགས་ཀྱང་མི་སྣང་ངོ་། །\n"
          "*** Sentence 65\n"
          "**** Segment 143\nལྔ་པ་འདིས་ནི་ཐམས་ཅད་དུ་སོང་བ་ཡིན་ཡང་སྣོད་དུ་མ་གྱུར་པའི་སེམས་ཅན་རྣམས་ལ་སངས་རྒྱས་ཀྱི་སྐུ་མི་སྣང་བ་དཔེས་སྒྲུབ་པར་བྱེད་དོ། །\n"
          "** Section IX.17\n"
          "*** Sentence 66\n:PROPERTIES:\n:VERSE: MSA IX.17\n:END:\n"
          "**** Segment 144\nསྐྱེ་མཆེད་བཅུ་གཉིས། །\n")
  "Tibetan: the same verses, other sentence numbers.")

(defconst bcat-commentary-test--sthiramati
  (concat "#+TITLE: Sthiramati, mdo sde'i rgyan gyi 'grel bshad, Kap. IX — CAT-Quelle\n"
          "#+TIBETAN_LAYOUT: cascade\n"
          "#+CAT_TEMPLATE: commentary\n"
          "#+CAT_COMMENTS_ON: ../Sanskrit/msa-cat.org\n"
          "#+TIBETAN_TARGET_LANG: en\n"
          "#+TIBETAN_CLAUDE_CONTEXT: Sthiramati, Sūtrālaṃkāravṛttibhāṣya (D4034).\n\n"
          "* Tibetan Text\n"
          "** Section IX.15\n"
          "*** Sentence 250\n"
          "**** Segment 600\nཕུང་པོ་ལྔ། །\n"
          "**** Segment 601\nཞེས་བྱ་བ་སྨོས་ཏེ།\n"
          "** Section IX.16\n"
          "*** Sentence 258\n"
          "**** Segment 626\nདཔེ་བསྟན་པའི་ཕྱིར།\n"
          "**** Segment 627\nཇི་ལྟར་ཆུ་ཡི་སྣོད་ཆག་ན། །\n"
          "*** Sentence 259\n"
          "**** Segment 628\nཟླ་བའི་གཟུགས་བརྙན་མི་སྣང་བཞིན། །\n"
          "*** Sentence 260\n"
          "**** Segment 629\nཞེས་བྱ་བ་སྨོས་ཏེ།\n"
          "**** Segment 630\nདཔེར་ན་ཆུ་མེད་པའི་སྣོད་དུ་ཡང་ཟླ་བའི་གཟུགས་བརྙན་མི་འབྱུང་ལ་སྣོད་ཆག་པའི་ནང་དུ་ཡང་ཟླ་བའི་གཟུགས་བརྙན་མི་འབྱུང་སྟེ་འདི་ནི་དཔེའོ། །\n"
          "*** Sentence 261\n"
          "**** Segment 631\nདོན་བསྟན་པའི་ཕྱིར།\n"
          "**** Segment 632\nདེ་བཞིན་སེམས་ཅན་ངན་པ་ལ། །\n"
          "*** Sentence 262\n"
          "**** Segment 633\nསངས་རྒྱས་གཟུགས་ནི་མི་སྣང་ངོ་། །\n"
          "*** Sentence 263\n"
          "**** Segment 634\nཞེས་བྱ་བ་སྨོས་ཏེ།\n"
          "**** Segment 635\nདེ་དང་འདྲ་བར་སེམས་ཅན་གང་གི་སེམས་ཀྱི་རྒྱུད་ཆུའི་སྣོད་དང་འདྲ་བ་ལ་བསོད་ནམས་དང་ཡེ་ཤེས་ཀྱི་ཆུས་མ་བཀང་བ་དང་། །\n")
  "Sthiramati: IX.15 and IX.16 — his IX.16 covers IX.17 too (no Section of its own).")

(defmacro bcat-commentary-test--with-msa (&rest body)
  "The MSA trio in DIR: SRC (Sanskrit), TSRC (Tibetan), CSRC (Sthiramati);
FOLDER, TFOLDER, CFOLDER their analysis/; refs.org beside SRC with a
reference for verse IX.16.  No dictionary (nil rows)."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "bcat-msa-" t))
          (src (expand-file-name "Sanskrit/msa-cat.org" dir))
          (tsrc (expand-file-name "Tibetisch/mdo-cat.org" dir))
          (csrc (expand-file-name "Sthiramati/savbh-cat.org" dir))
          (folder (file-name-as-directory (expand-file-name "Sanskrit/analysis" dir)))
          (tfolder (file-name-as-directory (expand-file-name "Tibetisch/analysis" dir)))
          (cfolder (file-name-as-directory (expand-file-name "Sthiramati/analysis" dir)))
          (bcat-sentence-templates t)
          (bcat-analysis-response-directory (expand-file-name "responses" dir))
          (bcat-template-directory
           (expand-file-name "../Templates/analysis"
                             (file-name-directory (locate-library "bcat-sentence-test")))))
     (unwind-protect
         (progn
           (dolist (d '("Sanskrit" "Tibetisch" "Sthiramati"))
             (make-directory (expand-file-name d dir) t))
           (with-temp-file src (insert bcat-commentary-test--sanskrit))
           (with-temp-file tsrc (insert bcat-commentary-test--tibetan))
           (with-temp-file csrc (insert bcat-commentary-test--sthiramati))
           (with-temp-file (expand-file-name "Sanskrit/refs.org" dir)
             (insert "#+TITLE: Thurman\n\n** Sentence 36\n*** Thurman 2004\n"
                     "Just as the reflection of the moon cannot be seen in a broken water pot, so the buddha's reflection is not seen in evil beings.\n"))
           (cl-letf (((symbol-function 'tibetan-gloss-table--unit-rows) (lambda (&rest _) nil)))
             ,@body))
       (dolist (b (buffer-list))
         (when (and (buffer-file-name b)
                    (string-prefix-p (file-truename dir)
                                     (file-truename (buffer-file-name b))))
           (with-current-buffer b (set-buffer-modified-p nil))
           (kill-buffer b)))
       (delete-directory dir t))))

;; ----------------------------------------------------------------------------
;; The verse: Sanskrit and Tibetan by :VERSE:, the bhāṣya below
;; ----------------------------------------------------------------------------

(ert-deftest bcat-verse-pairs-by-verse ()
  "The MSA sources share Sections and :VERSE:, not sentence numbers:
the Sanskrit verse IX.16 (sentence 36) goes with the Tibetan verse
IX.16 (sentence 64) — its pādas, its file in the header link."
  (bcat-commentary-test--with-msa
    (let ((text (bcat-sentence-test--create-n src 36)))
      (should (string-match-p (regexp-quote (format "#+CAT_PARALLEL: [[file:../../Tibetisch/analysis/%s]"
                                                    (file-name-nondirectory
                                                     (tibetan-sentence--filepath 64 tfolder tsrc))))
                              text))
      (should (string-match-p "^#\\+CAT_PARALLEL_SEGMENTS: 139, 140, 141, 142$" text))
      (should (string-match-p (regexp-quote "ཇི་ལྟར་ཆུ་ཡི་སྣོད་ཆག་ན། །\nji ltar chu yi snod chag na/ /") text))
      (should-not (string-match-p "ལྔ་པ་འདིས" (car (split-string text "^\\* Bhāṣya")))))))

(ert-deftest bcat-verse-bhasya-section ()
  "Carsten 30.10.: the bhāṣya „im Vers als zusätzliche Section (unten)
mit Text und hint“.  The other sentences of the verse's Section — the
intro before it, the bhāṣya after — stand below the analysis, the
Sanskrit as Devanāgarī over IAST, the Tibetan as Uchen over Wylie; in
the Tibetan verse file too.  A verse without bhāṣya has no section."
  (bcat-commentary-test--with-msa
    (let ((text (bcat-sentence-test--create-n src 36)))
      (should (string-match-p
               (regexp-quote (concat "* Bhāṣya\n#+begin_verse\n"
                                     "अथ पञ्चमः श्लोकः ।\natha pañcamaḥ ślokaḥ /\n"
                                     "अनेन पञ्चमेन"))
               text))
      (should (string-match-p (regexp-quote "ལྔ་པ་འདིས་ནི་") text))
      (should (string-match-p (regexp-quote "lnga pa 'dis ni thams cad du song ba") text))
      (should (< (string-match "^\\* Sentence$" text) (string-match "^\\* Bhāṣya$" text)
                 (string-match "^\\* Working Translation$" text))))
    (should (string-match-p "^\\* Bhāṣya$" (bcat-sentence-test--create-n tsrc 64)))
    (should-not (string-match-p "Bhāṣya" (bcat-sentence-test--create-n src 38)))))

(ert-deftest bcat-verse-bhasya-sentence-opens-verse-file ()
  "A bhāṣya sentence has no file of its own: creating it (C-c u A, the
batch) gives the verse file of its Section — and never overwrites one
that exists."
  (bcat-commentary-test--with-msa
    (let ((verse-file (tibetan-sentence--filepath 36 folder src)))
      (should (file-equal-p verse-file (bcat-sentence-ensure-file 37 src)))
      (should (file-exists-p verse-file))
      (should-not (file-exists-p (tibetan-sentence--filepath 37 folder src)))
      (bcat-sentence-test--set-notes verse-file "Meine Notiz.")
      (bcat-sentence-ensure-file 35 src)
      (tibetan-cascade--ensure-files src (list (list :sent-num 37 :segs '((94 . "x")))))
      (should (string-match-p "^\\* My Notes\nMeine Notiz\\.$" (bcat-sentence-test--text-of verse-file)))
      (should-not (file-exists-p (tibetan-sentence--filepath 35 folder src))))))

(ert-deftest bcat-verse-fetch-from-bhasya-sentence ()
  "C-c u G on a bhāṣya sentence analyses its verse: the Sanskrit request
for sentence 36, then the Tibetan one with the Tibetan verse IX.16
\(139–142, not the bhāṣya 143); both verse files carry the bhāṣya."
  (bcat-commentary-test--with-msa
    (bcat-sentence-test--with-stubs
        (lambda (kind)
          (if (equal kind "bcat-tibetan")
              (concat "{\"segments\": [{\"num\": 139, \"sanskrit\": 92, \"words\": [], \"translation\": \"Just as, when\"},"
                      " {\"num\": 140, \"sanskrit\": 92, \"words\": [], \"translation\": \"the moon\"},"
                      " {\"num\": 141, \"sanskrit\": 93, \"words\": [], \"translation\": \"likewise\"},"
                      " {\"num\": 142, \"sanskrit\": 93, \"words\": [], \"translation\": \"the Buddha\"}],"
                      " \"differences\": [], \"grammar\": \"g\", \"translation\": \"t\", \"comparison\": \"\", \"rendering\": \"r\"}")
            (concat "{\"segments\": [{\"num\": 92, \"words\": [], \"translation\": \"Just as the image of the moon\"},"
                    " {\"num\": 93, \"words\": [], \"translation\": \"so the image of the Buddha\"}],"
                    " \"grammar\": \"g\", \"translation\": \"t\", \"comparison\": \"\"}")))
      (bcat-sentence-test--fetch-n src 37)
      (should (equal '("bcat-analysis" "bcat-tibetan") (seq-take (reverse kinds) 2)))
      (let ((user (cdr (nth (- (length prompts) 2) prompts))))
        (should (string-match-p (regexp-quote "[139] Tibetan: ཇི་ལྟར") user))
        (should-not (string-match-p "\\[143\\]" user)))
      (let ((text (bcat-sentence-test--text-of (tibetan-sentence--filepath 36 folder src))))
        (should (string-match-p "^\\*\\* Segment 92 · Tib\\. 139–140$" text))
        (should (string-match-p "^\\* Bhāṣya$" text)))
      (should (string-match-p "^\\* Bhāṣya$" (bcat-sentence-test--text-of
                                              (tibetan-sentence--filepath 64 tfolder tsrc)))))))

(defconst bcat-commentary-test--verse-answer-sa
  (concat "{\"segments\": [{\"num\": 92, \"words\": [], \"translation\": \"Just as the image of the moon\"},"
          " {\"num\": 93, \"words\": [], \"translation\": \"so the image of the Buddha\"}],"
          " \"grammar\": \"g\", \"translation\": \"t\", \"comparison\": \"\","
          " \"bhasya_hint\": \"The bhāṣya reads the broken pot as the being who is no vessel (/abhājanabhūta/).\"}")
  "The Sanskrit answer for verse IX.16, with the hint on the bhāṣya.")

(defconst bcat-commentary-test--verse-answer-bo
  (concat "{\"segments\": [{\"num\": 139, \"sanskrit\": 92, \"words\": [], \"translation\": \"Just as, when\"},"
          " {\"num\": 140, \"sanskrit\": 92, \"words\": [], \"translation\": \"the moon\"},"
          " {\"num\": 141, \"sanskrit\": 93, \"words\": [], \"translation\": \"likewise\"},"
          " {\"num\": 142, \"sanskrit\": 93, \"words\": [], \"translation\": \"the Buddha\"}],"
          " \"differences\": [], \"grammar\": \"g\", \"translation\": \"t\", \"comparison\": \"\", \"rendering\": \"r\"}")
  "The Tibetan answer for verse IX.16.")

(defun bcat-commentary-test--verse-answers (kind)
  (if (equal kind "bcat-tibetan") bcat-commentary-test--verse-answer-bo bcat-commentary-test--verse-answer-sa))

(ert-deftest bcat-verse-bhasya-hint ()
  "The *Hint:* under the bhāṣya: the Sanskrit request of a verse gets
its bhāṣya in both languages as context (it helps the analysis of the
verse too) and returns `bhasya_hint' — what in it matters for reading
the verse; it stands in both verse files.  A source without verses
keeps its prompt unchanged."
  (bcat-commentary-test--with-msa
    (bcat-sentence-test--with-stubs #'bcat-commentary-test--verse-answers
      (bcat-sentence-test--fetch-n src 36)
      (let* ((first (nth (1- (length prompts)) prompts))
             (sys (car first)) (user (cdr first)))
        (should (string-match-p "\"bhasya_hint\"" sys))
        (should (string-match-p (regexp-quote "Bhāṣya on this verse (context):\nSanskrit: atha pañcamaḥ ślokaḥ / anena pañcamena") user))
        (should (string-match-p (regexp-quote "Tibetan: lnga pa 'dis ni thams cad du song ba") user)))
      (dolist (f (list (tibetan-sentence--filepath 36 folder src)
                       (tibetan-sentence--filepath 64 tfolder tsrc)))
        (should (string-match-p "^\\*Hint:\\* The bhāṣya reads the broken pot"
                                (bcat-sentence-test--text-of f))))))
  (should-not (string-match-p "bhasya" (bcat-analysis-sanskrit-system-prompt "en"))))

;; ----------------------------------------------------------------------------
;; Lemmata: which pāda a commentary segment quotes
;; ----------------------------------------------------------------------------

(defconst bcat-commentary-test--padas
  '((139 . "ji ltar chu yi snod chag na/ /")
    (140 . "zla ba'i gzugs brnyan mi snang ba/ /")
    (141 . "de bzhin sems can ngan pa la/ /")
    (142 . "sangs rgyas gzugs kyang mi snang ngo / /"))
  "The Tibetan pādas of MSA IX.16.")

(ert-deftest bcat-commentary-lemma ()
  "Sthiramati quotes the pādas as lemmata, now and then with his own
reading.  A segment is the lemma of a pāda when it consists of the
pāda's wording (a closing quote marker aside) and covers most of it;
its readings are the syllables where it differs.  An explanation that
uses the pāda's words (630) is no lemma; neither is an introduction."
  (cl-flet ((lemma (w) (bcat-commentary-lemma w bcat-commentary-test--padas)))
    (should (equal '(139) (lemma "ji ltar chu yi snod chag na/ /")))
    (should (equal '(140 ("bzhin" . "ba")) (lemma "zla ba'i gzugs brnyan mi snang bzhin/ /")))
    (should (equal '(142 ("ni" . "kyang")) (lemma "sangs rgyas gzugs ni mi snang ngo / /")))
    (should (equal '(140) (lemma "zla ba'i gzugs brnyan mi snang ba zhes bya ba la/")))
    (should-not (lemma "dper na chu med pa'i snod du yang zla ba'i gzugs brnyan mi 'byung la snod chag pa'i nang du yang zla ba'i gzugs brnyan mi 'byung ste 'di ni dpe'o/ /"))
    (should-not (lemma "dpe bstan pa'i phyir/"))
    (should-not (lemma "zhes bya ba smos te/"))))

(defconst bcat-commentary-test--padas-60
  '((459 . "sangs rgyas rnams kyi sku dbye ba/ /")
    (460 . "rang bzhin longs spyod rdzogs pa dang / /")
    (461 . "gzhan ni sprul pa'i sku yin te/ /")
    (462 . "dang po gnyis pa'i rten yin no/ /"))
  "The Tibetan pādas of MSA IX.60.")

(ert-deftest bcat-commentary-lemmata-marked ()
  "MSA IX.60: Sthiramati quotes in his own wording and word order, often
only part of a pāda — what stands before his quote marker („… zhes bya
ba la/“, „… ces bya bas“; also in the next segment) is a lemma when at
least half of it stands in one pāda, „in his own wording“.  A quotation
of the bhāṣya is no verse lemma.  „thog ma gnyis po'i gnas“ for „dang
po gnyis pa'i rten“ is left to Claude."
  (let ((lemmata (bcat-commentary-lemmata
                  '((1780 . "sangs rgyas kyi sku rnam par dbye ba'i tshigs su bcad pa bdun te zhes bya ba la/")
                    (1781 . "gong du sku gsum tha dad du 'jug pa mdo tsam du bstan nas/")
                    (1783 . "rang bzhin dang ni zhes bya ni/")
                    (1786 . "rdzogs longs spyod ces bya bas rdzogs par longs spyod pa'i sku bstan te/")
                    (1790 . "sprul pa'i sku yang gzhan yin te/ /")
                    (1791 . "zhes bya ba la/")
                    (1795 . "sangs rgyas rnams kyi sku dbye ba/ /")
                    (1798 . "thog ma gnyis po'i gnas yin no/ /")
                    (1799 . "zhes bya ba la thog ma ni chos kyi sku la bya ste/"))
                  bcat-commentary-test--padas-60
                  '("sangs rgyas kyi sku rab tu dbye ba la tshigs su bcad pa bdun no/ /"))))
    (should (equal '((1783 460 . free) (1786 460 . free) (1790 461 . free) (1795 459))
                   lemmata))))

(ert-deftest bcat-commentary-reading-text ()
  "A lemma's readings as text for the files: „verbatim“ without any,
„in his own wording“ for a free quotation, else what Sthiramati reads
for the verse's wording."
  (should (equal "verbatim" (bcat-commentary-reading-text nil)))
  (should (equal "in his own wording" (bcat-commentary-reading-text 'free)))
  (should (equal "Sthiramati reads /bzhin/ for /ba/"
                 (bcat-commentary-reading-text '(("bzhin" . "ba")))))
  (should (equal "Sthiramati reads /ni/ for /kyang/; adds /dang/; omits /yi/"
                 (bcat-commentary-reading-text '(("ni" . "kyang") ("dang" . "") ("" . "yi"))))))

(provide 'bcat-commentary-test)
;;; bcat-commentary-test.el ends here
