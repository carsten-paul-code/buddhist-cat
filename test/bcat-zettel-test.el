;;; bcat-zettel-test.el --- Tests for the concept notes and the zettels  -*- lexical-binding: t; -*-

;;; Commentary:
;; Carsten 5.11.2026: „concept Notes sollten doch eigentlich immer auch in
;; Zetteln repräsentiert werden“ — the terms of the concept notes link
;; to their zettels, and the passage goes into the zettel as a marked
;; Beleg (automatically: „ja, bitte“).  Index and zettels are fixtures
;; in a temp directory; the real Zettelkasten is never touched.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((root (expand-file-name ".." (file-name-directory
                                    (or load-file-name buffer-file-name)))))
  (dolist (d '("persist" "core" "analysis" "philology" "data" "config" "doc-prep" "test"))
    (add-to-list 'load-path (expand-file-name d root))))

(require 'org)
(require 'bcat-zettel)
(require 'bcat-sentence-test)

(defconst bcat-zettel-test--kala
  (concat ":PROPERTIES:\n:ID: 20261027T135714\n:SANSKRIT: kalā\n:END:\n"
          "#+title: kalā\n\n/kalā/ · Teil, Kraft\n\n"
          "* Konzept\nKraft.\n\n"
          "* Belegstellen in der Literatur\n"
          "- [cite:@Baeumer2008 p. 215] „kalā: die Energie …“\n"
          "- TĀ 1.78: /kalātattva…/ — von Hand\n\n"
          "* Abgrenzungen\n- kañcuka\n\n* Verweise\n"))

(defconst bcat-zettel-test--visarga
  (concat ":PROPERTIES:\n:ID: 20261016T140000\n:END:\n"
          "#+title: visarga\n\n* Konzept\nEmission.\n\n* Verweise\n# /term\n"))

(defmacro bcat-zettel-test--with-kasten (&rest body)
  "Run BODY with a fixture Zettelkasten (KASTEN, kalā, visarga, a stub
for niyantraṇa) and its index loaded."
  (declare (indent 0))
  `(let* ((kasten (make-temp-file "bcat-kasten-" t))
          (index (expand-file-name "index.json" kasten)))
     (unwind-protect
         (progn
           (with-temp-file (expand-file-name "20261027T135714--kala.org" kasten)
             (insert bcat-zettel-test--kala))
           (with-temp-file (expand-file-name "20261016T140000--visarga.org" kasten)
             (insert bcat-zettel-test--visarga))
           (with-temp-file (expand-file-name "20250522T150000--skandha.org" kasten)
             (insert "#+title: skandha\n#+description: Die fünf Gruppen, aus denen die Person besteht.\n"))
           (with-temp-file (expand-file-name "20261001T000000--niyantrana.org" kasten)
             (insert "#+title: niyantraṇa\n#+description: Lenkung, Zügelung; bei Jayaratha die Ordnung des Kommentars.\n"))
           (with-temp-file index
             (insert (json-encode
                      `((generated . "x") (zk_dir . ,kasten) (count . 3)
                        (entries . [((id . "20261027T135714") (file . "20261027T135714--kala.org")
                                     (title . "kalā") (kind . "concept") (stub . :json-false)
                                     (tags . ["type_concept" "tradition_saiva"])
                                     (keys . ["kala" "vimalakala"]))
                                    ((id . "20261016T140000") (file . "20261016T140000--visarga.org")
                                     (title . "visarga") (kind . "concept") (stub . :json-false)
                                     (tags . ["tradition_saiva"])
                                     (keys . ["visarga"]))
                                    ((id . "20250522T150000") (file . "20250522T150000--skandha.org")
                                     (title . "skandha") (kind . "concept") (stub . :json-false)
                                     (sanskrit . "skandha") (wylie . "phung po")
                                     (keys . ["skandha" "phungpo" "ཕུངཔོ"]))
                                    ((id . "20250822T092830") (file . "20250822T092830--jnana.org")
                                     (title . "jñāna") (kind . "concept") (stub . :json-false)
                                     (tags . ["type_concept" "domain_epistemology"])
                                     (keys . ["jnana"]))
                                    ((id . "20250101T000001") (file . "20250101T000001--kula.org")
                                     (title . "kula") (kind . "concept") (stub . :json-false)
                                     (tags . ["tradition_yogacara"])
                                     (keys . ["kula"]))
                                    ((id . "20261012T123039") (file . "20261012T123039--kula.org")
                                     (title . "kula") (kind . "concept") (stub . :json-false)
                                     (tags . ["tradition_saiva"])
                                     (keys . ["kula"]))
                                    ((id . "20261001T000000") (file . "20261001T000000--niyantrana.org")
                                     (title . "niyantraṇa") (kind . "concept") (stub . :json-false)
                                     (sanskrit . "niyantraṇa") (english . "restraint, control")
                                     (keys . ["niyantrana"]))])))))
           (let ((tibetan-zettel-highlight-index-file index)
                 (tibetan-zettel-highlight--index nil)
                 (tibetan-zettel-highlight--entries nil)
                 (tibetan-zettel-highlight--index-mtime nil)
                 (tibetan-zettel-highlight--zk-dir nil)
                 (bcat-zettel-link-concepts t)
                 (bcat-zettel-in-prompt t))
             ,@body))
       (delete-directory kasten t))))

(defun bcat-zettel-test--read (file)
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(ert-deftest bcat-zettel-link ()
  "A term of a concept note (in bold) with a zettel links to it —
denote:ID, the Kasten's own links, RET leads to the Research instance;
case and diacritics do not matter; a term without zettel stays as it
is; linking again changes nothing."
  (bcat-zettel-test--with-kasten
    (let* ((text "*kalā* — power; ideation (*vimarśa*); *Visarga* the emission.")
           (linked (bcat-zettel-link text)))
      (should (equal (concat "*[[denote:20261027T135714][kalā]]* — power; ideation (*vimarśa*); "
                             "*[[denote:20261016T140000][Visarga]]* the emission.")
                     linked))
      (should (equal linked (bcat-zettel-link linked)))
      (should (equal '("20261027T135714" "20261016T140000") (bcat-zettel-ids linked))))))

(ert-deftest bcat-zettel-link-tradition ()
  "Homonyms (Carsten 5.11.: „beides, bitte“): the tradition most of a
sentence's linked terms share decides — TĀ 1.1 linked jñāna to the
zettel of Kramer's glossary (epistemology), the only one of that name;
in a Śaiva sentence a term without a Śaiva zettel stays unlinked, of
two zettels the Śaiva one.  Without such a majority: as before (the
first that is no stub)."
  (bcat-zettel-test--with-kasten
    (let* ((ctx '((segments . (((num . 1) (concepts . "*kalā* — power; the five: *jñāna* …"))
                               ((num . 2) (concepts . "*visarga* — emission; *kula* — body."))))))
           (linked (bcat-zettel-link-context ctx))
           (c1 (alist-get 'concepts (car (alist-get 'segments linked))))
           (c2 (alist-get 'concepts (cadr (alist-get 'segments linked)))))
      (should (string-match-p (regexp-quote "*[[denote:20261027T135714][kalā]]*") c1))
      (should (string-match-p (regexp-quote "the five: *jñāna* …") c1))
      (should (string-match-p (regexp-quote "*[[denote:20261012T123039][kula]]*") c2)))
    (let* ((ctx '((segments . (((num . 1) (concepts . "*jñāna* — knowledge; *kula* — family."))))))
           (c (alist-get 'concepts (car (alist-get 'segments (bcat-zettel-link-context ctx))))))
      (should (string-match-p (regexp-quote "*[[denote:20250822T092830][jñāna]]*") c))
      (should (string-match-p (regexp-quote "*[[denote:20250101T000001][kula]]*") c)))))

(ert-deftest bcat-zettel-link-tradition-of-the-text ()
  "The tradition comes from the concept notes of every sentence of the
text, not of one sentence alone (5.11.: a Viveka sentence with only
*bhava* had no majority and went to the Buddhist zettel)."
  (bcat-zettel-test--with-kasten
    (let* ((dir (make-temp-file "bcat-text-" t))
           (src (expand-file-name "tantraloka-1.org" dir)))
      (unwind-protect
          (progn
            (make-directory (expand-file-name "analysis" dir))
            (with-temp-file (expand-file-name "analysis/sent-001-tantralo.org" dir)
              (insert "* Sentence\n** Concepts\n⟦1⟧ *kalā* — power; *visarga* — emission.\n\n** Translations compared\n"))
            (let* ((ctx '((segments . (((num . 5) (concepts . "*kula* — body."))))))
                   (c (alist-get 'concepts (car (alist-get 'segments (bcat-zettel-link-context ctx src))))))
              (should (string-match-p (regexp-quote "*[[denote:20261012T123039][kula]]*") c))))
        (delete-directory dir t)))))

(ert-deftest bcat-zettel-label ()
  "The place of a Beleg: the verse, else the work (its title up to the
first comma or bracket) and the sentence — „Satz 1003“ alone is in the
Abhidharmasamuccaya and in the Tantrāloka."
  (should (equal "SDA 9" (bcat-zettel--label '(:verse "SDA 9" :work "bDen" :sent-num 9))))
  (should (equal "Tantrāloka Satz 1003"
                 (bcat-zettel--label '(:work "Tantrāloka, Kapitel 1 (vijñānabhid)" :sent-num 1003))))
  (should (equal "Abhidharmasamuccaya Satz 2"
                 (bcat-zettel--label '(:work "Abhidharmasamuccaya" :sent-num 2)))))

(ert-deftest bcat-zettel-link-without-index ()
  "No index (or linking switched off): the notes stay as they are."
  (let ((tibetan-zettel-highlight-index-file "/nonexistent/bcat-zettel-index.json")
        (tibetan-zettel-highlight--index nil)
        (bcat-zettel-link-concepts t))
    (should (equal "*kalā* — power." (bcat-zettel-link "*kalā* — power."))))
  (bcat-zettel-test--with-kasten
    (let ((bcat-zettel-link-concepts nil))
      (should (equal "*kalā* — power." (bcat-zettel-link "*kalā* — power."))))))

(ert-deftest bcat-zettel-fetch-links-concepts ()
  "C-c u G: the concept notes land linked in the sentence file; the PDF
shows the term without the link (the reMarkable cannot open the Kasten)."
  (bcat-zettel-test--with-kasten
    (bcat-sentence-test--with-sa-source
      (bcat-sentence-test--create-n src 1007)
      (bcat-sentence-test--with-stubs (bcat-sentence-test--sa-answer)
        (bcat-sentence-test--fetch-n src 1007))
      (let ((file (tibetan-sentence--filepath 1007 folder src)))
        (should (string-match-p (regexp-quote "⟦685⟧ *[[denote:20261001T000000][niyantraṇa]]*")
                                (bcat-sentence-test--text-of file)))
        (with-temp-buffer
          (insert "⟦685⟧ *[[denote:20261001T000000][niyantraṇa]]* — control.\n")
          (bcat-export--plain-zettel-links)
          (should (equal "⟦685⟧ *niyantraṇa* — control.\n" (buffer-string))))))))

(defun bcat-zettel-test--sentence (dir)
  "A sentence file in DIR as the write-back reads it: TĀ 1.1 ⟦1⟧ with
kalā and visarga in its concept notes and the overview of its words."
  (let ((file (expand-file-name "sent-001-tantralo.org" dir)))
    (with-temp-file file (insert "* Text\n"))
    file))

(defconst bcat-zettel-test--context
  '((segments . (((num . 1)
                  (gloss-table . "| vimalā-kalā-āśrayā        | jananī |\n| whose ground is pure kalā | mother |\n| adj. f. nom. sg.          | f. nom. sg. |")
                  (concepts . "*[[denote:20261027T135714][kalā]]* — power. *[[denote:20261016T140000][visarga]]* — emission."))
                 ((num . 2)
                  (gloss-table . "| tad-ubhaya-visarga-mayam |\n| consisting of the emission of both |\n| adj. |")
                  (concepts . "*[[denote:20261016T140000][Visarga]]* — the emission.")))))
  "The fields of the fixture sentence the write-back reads.")

(ert-deftest bcat-zettel-belege ()
  "The passage goes into each linked zettel as a marked Beleg (Carsten
5.11.: automatically, „ja, bitte“): under * Belegstellen …, after the
last one — the place (verse or Satz N, ⟦N⟧), the word of the passage
and what it means there, (CAT: link to the sentence file) — only where
the term stands in the text of the segment.  No such section: one
before * Verweise.  Again: nothing doubled; a changed
passage replaces its line; a Beleg of the same verse by hand wins."
  (bcat-zettel-test--with-kasten
    (let* ((dir (make-temp-file "bcat-sent-" t))
           (file (bcat-zettel-test--sentence dir))
           (kala (expand-file-name "20261027T135714--kala.org" kasten))
           (visarga (expand-file-name "20261016T140000--visarga.org" kasten))
           (link (format "(CAT: [[file:%s::*Concepts][Satz 1]])" (abbreviate-file-name file))))
      (unwind-protect
          (progn
            (bcat-zettel-write-belege file bcat-zettel-test--context "TĀ 1.1" "Satz 1")
            (should (string-match-p
                     (regexp-quote (concat "- TĀ 1.78: /kalātattva…/ — von Hand\n"
                                           "- TĀ 1.1 ⟦1⟧: /vimalā-kalā-āśrayā/ — whose ground is pure kalā "
                                           link "\n\n* Abgrenzungen"))
                     (bcat-zettel-test--read kala)))
            ;; ⟦1⟧ only names visarga in its note — not in its text: no
            ;; Beleg (5.11., TĀ 1.1: prakāśa, cit, icchā … got lines
            ;; without a word, jñāna even in the wrong zettel).
            (should (string-match-p
                     (regexp-quote (concat "* Belegstellen in der Literatur\n"
                                           "- TĀ 1.1 ⟦2⟧: /tad-ubhaya-visarga-mayam/ — consisting of the emission of both "
                                           link "\n\n* Verweise"))
                     (bcat-zettel-test--read visarga)))
            (should-not (string-match-p "⟦1⟧" (bcat-zettel-test--read visarga)))
            ;; Again: nothing doubled.
            (let ((before (bcat-zettel-test--read kala)))
              (bcat-zettel-write-belege file bcat-zettel-test--context "TĀ 1.1" "Satz 1")
              (should (equal before (bcat-zettel-test--read kala))))
            ;; Another label for the same place and file: replaced, not added.
            (bcat-zettel-write-belege file bcat-zettel-test--context "Tantrāloka Satz 1" "Satz 1")
            (let ((text (bcat-zettel-test--read kala)))
              (should (string-match-p "^- Tantrāloka Satz 1 ⟦1⟧: " text))
              (should-not (string-match-p "^- TĀ 1\\.1 ⟦1⟧" text)))
            (bcat-zettel-write-belege file bcat-zettel-test--context "TĀ 1.1" "Satz 1")
            ;; A changed passage replaces its line.
            (bcat-zettel-write-belege
             file `((segments . (((num . 1)
                                  (gloss-table . "| vimalā-kalā-āśrayā |\n| grounded in pure ideation |")
                                  (concepts . "*[[denote:20261027T135714][kalā]]* — power.")))))
             "TĀ 1.1" "Satz 1")
            (let ((text (bcat-zettel-test--read kala)))
              (should (string-match-p "grounded in pure ideation" text))
              (should-not (string-match-p "whose ground is pure kalā" text)))
            ;; A Beleg of the same verse by hand: no line of the tool.
            (with-temp-file kala
              (insert (replace-regexp-in-string "^- TĀ 1\\.1 ⟦1⟧.*\n" "- TĀ 1.1: von Hand\n"
                                                (bcat-zettel-test--read kala))))
            (bcat-zettel-write-belege file bcat-zettel-test--context "TĀ 1.1" "Satz 1")
            (should-not (string-match-p "(CAT:" (bcat-zettel-test--read kala))))
        (delete-directory dir t)))))

(ert-deftest bcat-zettel-passage ()
  "The word of the passage and what it means there: the term in the word
\(Sanskrit: the first row; Tibetan: the Uchen or the transcription
under it), not in a gloss."
  (bcat-zettel-test--with-kasten
    (should (equal '("vimalā-kalā-āśrayā" . "whose ground is pure kalā")
                   (bcat-zettel--passage "| jananī | vimalā-kalā-āśrayā |\n| mother | whose ground is pure kalā |" "kalā")))
    (should-not (bcat-zettel--passage "| jananī |\n| mother of the visarga |" "visarga"))
    ;; The overview of a segment in several tables (TĀ 1.1 ⟦2⟧: anuttara
    ;; in the second one) — each table on its own, not as one column.
    (should (equal '("anuttarā-amṛta-kulam" . "embodying the nectar of the Absolute")
                   (bcat-zettel--passage
                    (concat "| tad-ubhaya-mayam | hṛdayam |\n| consisting of both | heart |\n| adj. | n. |\n\n"
                            "| anuttarā-amṛta-kulam | mama |\n| embodying the nectar of the Absolute | my |\n| adj. | pron. |")
                    "Anuttara")))
    (should (equal '("'dus ma byas" . "uncompounded")
                   (bcat-zettel--passage "| འདུས་མ་བྱས |\n| [[steinert:'dus ma byas]['dus ma byas]] |\n| uncompounded |"
                                         "'dus ma byas")))))

(ert-deftest bcat-zettel-belege-after-fetch ()
  "C-c u G ends with the Belege (when `bcat-zettel-belege-auto'); never
for a fixture under the temp directory — an ERT run in a live Emacs
must not write into the Kasten."
  (let ((calls '()))
    (cl-letf (((symbol-function 'bcat-zettel-belege-file) (lambda (f) (push f calls))))
      (let ((bcat-zettel-belege-auto t))
        (bcat-zettel-after-fetch (expand-file-name "x/sent-001-tantralo.org" temporary-file-directory))
        (should-not calls)
        (cl-letf (((symbol-function 'tibetan-discussion-paper--temp-source-p) #'ignore))
          (bcat-zettel-after-fetch "/k/sent-001-tantralo.org"))
        (should (equal '("/k/sent-001-tantralo.org") calls)))
      (setq calls nil)
      (let ((bcat-zettel-belege-auto nil))
        (cl-letf (((symbol-function 'tibetan-discussion-paper--temp-source-p) #'ignore))
          (bcat-zettel-after-fetch "/k/sent-001-tantralo.org")))
      (should-not calls))))

(ert-deftest bcat-zettel-prompt-block ()
  "Mitgeben (Carsten 5.11.: „beides, bitte“): the zettels of terms that
stand in the sentence go to Claude with their short description — the
concept note says what the term means HERE, the general sense is the
zettel's.  Nothing when no term of the sentence has a zettel."
  (bcat-zettel-test--with-kasten
    (let ((block (bcat-zettel-prompt-block '("atra madvāgaśaktāpi yanniyantraṇamullaset /") nil)))
      (should (string-match-p "Zettelkasten" block))
      (should (string-match-p "niyantraṇa (Skt\\. niyantraṇa; restraint, control): Lenkung, Zügelung" block))
      (should (string-match-p "what the term means in THIS passage" block)))
    (should-not (bcat-zettel-prompt-block '("tad ubhayam") nil))))

(ert-deftest bcat-zettel-fetch-sends-zettels ()
  "C-c u G sends the block with the sentence (in the user prompt)."
  (bcat-zettel-test--with-kasten
    (let ((bcat-zettel-in-prompt t))
      (bcat-sentence-test--with-sa-source
        (bcat-sentence-test--create-n src 1007)
        (bcat-sentence-test--with-stubs (bcat-sentence-test--sa-answer)
          (bcat-sentence-test--fetch-n src 1007)
          (should (string-match-p "niyantraṇa (Skt\\. niyantraṇa" (cdr (car (last prompts))))))))))

(ert-deftest bcat-zettel-pair-sends-zettels ()
  "A bilingual pair: the Tibetan request (Claude writes the Tibetan
concept notes too) gets the zettels of the terms in the Tibetan
segments — Uchen matched against the Tibetan keys of the index."
  (bcat-zettel-test--with-kasten
    (bcat-sentence-test--with-pair
      (bcat-sentence-test--create-n src 3)
      (bcat-sentence-test--with-stubs #'bcat-sentence-test--pair-answers
        (bcat-sentence-test--fetch-n src 3)
        (let ((tib (nth (cl-position "bcat-tibetan" kinds :test #'equal) prompts)))
          (should tib)
          (should (string-match-p "- skandha (Skt\\. skandha; Tib\\. phung po): Die fünf Gruppen" (cdr tib))))))))

(ert-deftest bcat-zettel-key ()
  "C-c u z b writes the Belege of the sentence file at point (or of the
sentences under the heading at point in the source)."
  (require 'tibetan-keybindings)
  (should (eq (key-binding (kbd "C-c u z b")) 'bcat-zettel-belege)))

(provide 'bcat-zettel-test)
;;; bcat-zettel-test.el ends here
