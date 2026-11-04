;;; bcat-sentence-test.el --- Tests for template-built sentence files  -*- lexical-binding: t; -*-

;;; Commentary:
;; §5.74 step 2 (Carsten 28.10.2026): "Wichtig ist, dass dann auch für
;; C-c u A schon das Template (ohne KI requests) verwendet wird."  A new
;; Tibetan cascade sentence file is rendered from the template:
;; * Text (Wylie), * Segments (dictionary tables), * Sentence (the
;; reference translations under "Translations compared"), user slots.
;; Old renderers never touch such a file — only C-c u G will (step 3).
;; Temp dirs only; no network; no AI call.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((root (expand-file-name ".." (file-name-directory
                                    (or load-file-name buffer-file-name)))))
  (dolist (d '("persist" "core" "analysis" "philology" "data" "config" "doc-prep"))
    (add-to-list 'load-path (expand-file-name d root))))

(require 'org)
(require 'bcat-sentence)
(require 'tibetan-cascade)
(require 'tibetan-sentence-persist)

(defconst bcat-sentence-test--source
  (concat "#+TITLE: bDen pa gnyis la 'jug pa — CAT-Quelle\n"
          "#+TIBETAN_LAYOUT: cascade\n"
          "#+CAT_TEMPLATE: tibetan\n"
          "#+TIBETAN_TARGET_LANG: en\n"
          "#+TIBETAN_SENTENCE_REFS: refs.org\n\n"
          "* Tibetan Text\n** Section Verse 9\n"
          "*** Sentence 9\n:PROPERTIES:\n:VERSE: SDA 9\n:END:\n"
          "**** Segment 38\nབརྗོད་དུ་མེད་པ་བལྟར་མེད་པ། །\n\n"
          "**** Segment 39\nའགྱུར་བ་མེད་པ་འདུས་མ་བྱས། །\n\n")
  "A Tibetan cascade source with one verse (bDen 9, two pādas).")

(defconst bcat-sentence-test--refs
  (concat "#+TITLE: Lindtner 1981, versweise\n\n"
          "** Sentence 9\n*** Lindtner 1981\n"
          "[It is also] inexpressible (avācya) and invisible (adraṣṭavya).\n")
  "Sentence references for sentence 9.")

(defconst bcat-sentence-test--sa-source
  (concat "#+TITLE: Tantrāloka, Kapitel 1 — CAT-Quelle\n"
          "#+TIBETAN_LAYOUT: cascade\n"
          "#+SOURCE_LANG: sa\n"
          "#+CAT_TEMPLATE: sanskrit\n"
          "#+TIBETAN_TARGET_LANG: en\n"
          "#+TIBETAN_SENTENCE_REFS: refs.org\n\n"
          "* Einleitende Verse\n** Section Jayaratha: Einleitungsverse\n"
          "*** Sentence 1007\n:PROPERTIES:\n:VIVEKA: Einleitung\n:END:\n"
          "**** Segment 685\natra madvāgaśaktāpi yanniyantraṇamullaset /\n"
          "**** Segment 686\ntatpārameśvaraṃ śrīmanmahānandavijṛmbhitam // 7\n\n"
          "*** Sentence 1008\n:PROPERTIES:\n:VIVEKA: Uddeśa zu TĀ 1.1\n:END:\n"
          "**** Segment 687\niha khalu śāstrādāv alaukikāśīrvādamukhena parāmṛśati\n")
  "A Sanskrit cascade source: Jayaratha's seventh introductory verse
\(TĀ sentence 1007, a śloka) and a prose sentence of the Viveka.")

(defconst bcat-sentence-test--sa-refs
  (concat "#+TITLE: Dyczkowski 2023\n\n"
          "** Sentence 1007\n*** Dyczkowski 2023\n"
          "Although my speech is powerless here (in this task), whatever restriction may arise.\n")
  "Sentence references for TĀ sentence 1007.")

(defmacro bcat-sentence-test--with-sa-source (&rest body)
  "Like `bcat-sentence-test--with-source' with the Sanskrit source."
  (declare (indent 0))
  `(bcat-sentence-test--with-source
     (with-temp-file src (insert bcat-sentence-test--sa-source))
     (with-temp-file (expand-file-name "refs.org" dir) (insert bcat-sentence-test--sa-refs))
     ,@body))

(defun bcat-sentence-test--create-n (src n)
  "Create sentence N of SRC the way C-c u A does; return its text."
  (let ((file (tibetan-cascade--create-file
               n (tibetan-cascade--segs-for-sentence src n) src)))
    (with-temp-buffer (insert-file-contents file) (buffer-string))))

(defmacro bcat-sentence-test--with-source (&rest body)
  "Temp Tibetan cascade source SRC (+ refs.org) in DIR; FOLDER = its
analysis/.  Templates on, the tool templates from the repository."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "bcat-sent-" t))
          (src (expand-file-name "bden-cat.org" dir))
          (folder (file-name-as-directory (expand-file-name "analysis" dir)))
          (bcat-sentence-templates t)
          (bcat-template-directory
           (expand-file-name "../Templates/analysis"
                             (file-name-directory (locate-library "bcat-sentence-test")))))
     (unwind-protect
         (progn
           (with-temp-file src (insert bcat-sentence-test--source))
           (with-temp-file (expand-file-name "refs.org" dir)
             (insert bcat-sentence-test--refs))
           ,@body)
       (dolist (b (buffer-list))
         (when (and (buffer-file-name b)
                    (string-prefix-p (file-truename dir)
                                     (file-truename (buffer-file-name b))))
           (with-current-buffer b (set-buffer-modified-p nil))
           (kill-buffer b)))
       (delete-directory dir t))))

(defun bcat-sentence-test--create (src)
  "Create sentence 9 of SRC the way C-c u A does; return its text."
  (let ((file (tibetan-cascade--create-file
               9 (tibetan-cascade--segs-for-sentence src 9) src)))
    (with-temp-buffer (insert-file-contents file) (buffer-string))))

(ert-deftest bcat-sentence-create-glossed-text ()
  "C-c u A (no AI): under * Text the Uchen, then per segment ⟦N⟧ and
its table from the dictionaries (31.10. abends, TĀ 1006: „Ich lese und
übersetze dann aus den Segmenttabellen“ — no glossed Wylie line any
more; with a table no plain Wylie under the Uchen)."
  (bcat-sentence-test--with-source
    (cl-letf (((symbol-function 'tibetan-gloss-table--unit-rows)
               (lambda (text &rest _)
                 (if (string-prefix-p "བརྗོད" text)
                     '(("[[steinert:brjod du med pa][brjod du med pa]]" "[[steinert:bltar med pa][bltar med pa]]")
                       ("inexpressible; ineffable" "invisible")
                       ("?" "?"))
                   '(("[[steinert:'gyur ba med pa]['gyur ba med pa]]" "[[steinert:'dus]['dus]]" "ma byas")
                     ("immutable" "collect; assemble…" "")
                     ("?" "?" "?"))))))
      ;; No translation line before C-c u G.
      (let ((text (bcat-sentence-test--create src)))
        (should (string-match-p
                 (regexp-quote (concat "* Text\n#+begin_verse\n"
                                       "བརྗོད་དུ་མེད་པ་བལྟར་མེད་པ། །\n"
                                       "འགྱུར་བ་མེད་པ་འདུས་མ་བྱས། །\n"
                                       "#+end_verse\n\n⟦38⟧\n| བརྗོད་དུ་མེད་པ"))
                 text))
        ;; 31.10. abends, Tibetisch V: the Uchen above the Wylie.
        (should (string-match-p "^| བརྗོད་དུ་མེད་པ +| བལྟར་མེད་པ +|\n| \\[\\[steinert:brjod du med pa\\]" text))
        (should (string-match-p (regexp-quote "| inexpressible; ineffable | invisible") text))
        (should (string-match-p (regexp-quote "\n\n⟦39⟧\n| འགྱུར་བ་མེད་པ") text))
        (should-not (string-match-p "\\[inexpressible" text))
        (should (string-match-p "\n\n\\* Sentence\n" text))))))

(ert-deftest bcat-sentence-create-text-without-glosses ()
  "No word known to the dictionaries: no table — the Wylie under the
Uchen instead."
  (bcat-sentence-test--with-source
    (cl-letf (((symbol-function 'tibetan-gloss-table--unit-rows) #'ignore))
      (should (string-match-p
               (regexp-quote (concat "* Text\n#+begin_verse\n"
                                     "བརྗོད་དུ་མེད་པ་བལྟར་མེད་པ། །\nbrjod du med pa bltar med pa/ /\n"
                                     "འགྱུར་བ་མེད་པ་འདུས་མ་བྱས། །\n'gyur ba med pa 'dus ma byas/ /\n"
                                     "#+end_verse\n\n* Sentence"))
               (bcat-sentence-test--create src))))))

(ert-deftest bcat-sentence-create-sanskrit ()
  "C-c u A on a Sanskrit source with #+CAT_TEMPLATE: sanskrit (§5.74,
Carsten 29.10.): the Recitation first — the verse with its pauses, the
scansion without pāda letters; since 31.10. abends without the
description of the metre („Bei der Recitation kann die Beschreibung
weg“) —, * Text empty (the tables come with C-c u G), * Sentence with
the reference under Translations compared, the segments empty.  No AI."
  (bcat-sentence-test--with-sa-source
    (let ((fired nil))
      (cl-letf (((symbol-function 'tibetan-sentence-claude--request)
                 (lambda (&rest _) (setq fired t))))
        (let ((text (bcat-sentence-test--create-n src 1007)))
          (should (string-match-p "^#\\+CAT_TEMPLATE: sanskrit$" text))
          (should (string-match-p "^#\\+SOURCE_LANG: sa$" text))
          (should (string-match-p
                   (regexp-quote
                    (concat "\n\n* Recitation\n"
                            "#+begin_verse\nअत्र मद्वागशक्तापि | यन्नियन्त्रणमुल्लसेत् ।\n"
                            "atra madvāgaśaktāpi | yanniyantraṇamullaset /\n"
                            "तत्पारमेश्वरं श्रीमन्|महानन्दविजृम्भितम् ॥ ७\n"
                            "tatpārameśvaraṃ śrīman|mahānandavijṛmbhitam // 7\n#+end_verse\n\n"
                            "| at  | ra | mad |"))
                   text))
          (should (string-match-p
                   (regexp-quote "\n\n* Text\n\n* Sentence\n** Grammar\n")
                   text))
          (should-not (string-match-p "Śloka (Anuṣṭubh)" text))
          (should (string-match-p
                   (regexp-quote "* Segments\n** Segment 685\n\n** Segment 686\n\n* Working Translation\n")
                   text))
          (should (string-match-p
                   (regexp-quote "** Translations compared\n*Dyczkowski 2023:* Although my speech")
                   text))
          (should-not (string-match-p "steinert:\\|Translation:\\|Sanskrit Analysis\\|Word Analysis" text))
          (should-not fired))))))

(ert-deftest bcat-sentence-create-sanskrit-prose ()
  "Viveka prose: the Recitation holds only the Devanāgarī original (no
metre, no scansion) over the IAST as transmitted; * Text is empty until
C-c u G brings the tables."
  (bcat-sentence-test--with-sa-source
    (let ((text (bcat-sentence-test--create-n src 1008)))
      (should (string-match-p
               (regexp-quote (concat "* Recitation\n#+begin_verse\n"
                                     "इह खलु शास्त्रादाव् अलौकिकाशीर्वादमुखेन परामृशति\n"
                                     "iha khalu śāstrādāv alaukikāśīrvādamukhena parāmṛśati\n#+end_verse\n\n"
                                     "* Text\n\n* Sentence"))
               text)))))

(ert-deftest bcat-sentence-create-from-template ()
  "C-c u A on a Tibetan cascade source writes the template form: marker
#+CAT_TEMPLATE: tibetan, * Text with the Uchen per segment and a
dictionary table per segment (31.10. abends: the tables under * Text),
* Sentence with the reference translation under Translations compared,
* Segments, the three user slots — and none of the old sections.  No
AI request is made."
  (bcat-sentence-test--with-source
    (let ((fired nil))
      (cl-letf (((symbol-function 'tibetan-sentence-claude--request)
                 (lambda (&rest _) (setq fired t))))
        (let ((text (bcat-sentence-test--create src)))
          (should (string-match-p "^#\\+CAT_TEMPLATE: tibetan$" text))
          (should (string-match-p "^#\\+SEGMENTS: 38, 39$" text))
          ;; One Uchen line per pāda, then the tables (exact form:
          ;; create-glossed-text).
          (should (string-match-p
                   "#\\+begin_verse\nབརྗོད་དུ་མེད་པ་བལྟར་མེད་པ། །\nའགྱུར་བ་མེད་པ་འདུས་མ་བྱས། །\n#\\+end_verse\n\n⟦38⟧\n| "
                   text))
          (should (string-match-p "^⟦39⟧\n| " text))
          (should (string-match-p "^\\* Segments\n\\*\\* Segment 38\n\n\\*\\* Segment 39\n" text))
          (should (string-match-p "steinert:" text))
          (should (string-match-p
                   (regexp-quote "** Translations compared\n*Lindtner 1981:* [It is also] inexpressible (avācya) and invisible (adraṣṭavya).")
                   text))
          (dolist (h '("* Working Translation" "* My Notes" "* Footnotes"))
            (should (string-match-p (concat "^" (regexp-quote h) "$") text)))
          ;; Nothing of the old layout, no AI-only lines yet.
          (dolist (old '("^\\* Reading" "^\\* Tibetan Analysis" "^\\* Provided Translations"
                         "Claude" "Partikelkarte" "\\[Requesting" "\\[Awaiting"
                         "^\\*Structure:\\*" "^\\*Note:\\*" "^⟦[0-9]+⟧ "))
            (should-not (string-match-p old text)))
          (should-not fired))))))

(ert-deftest bcat-sentence-old-layout-where-no-template ()
  "Carsten 28.10.: the template only where a source switches it on
\(#+CAT_TEMPLATE: tibetan — the reading classes; the Masterarbeit
sources keep the old layout).  A source without the line, a Sanskrit
source and a switched-off `bcat-sentence-templates' keep the old
layout."
  (bcat-sentence-test--with-source
    (let ((bcat-sentence-templates nil))
      (should-not (string-match-p "CAT_TEMPLATE" (bcat-sentence-test--create src)))))
  (bcat-sentence-test--with-source
    (with-temp-file src
      (insert (replace-regexp-in-string "#\\+CAT_TEMPLATE: tibetan\n" ""
                                        bcat-sentence-test--source)))
    (should-not (bcat-sentence-template-name src))
    (should-not (string-match-p "CAT_TEMPLATE" (bcat-sentence-test--create src))))
  (bcat-sentence-test--with-source
    (with-temp-file src
      ;; A Sanskrit source as the reading classes have them today: no
      ;; #+CAT_TEMPLATE line yet (its template comes later).
      (insert (replace-regexp-in-string
               "#\\+CAT_TEMPLATE: tibetan\n" "#+SOURCE_LANG: sa\n"
               bcat-sentence-test--source)))
    (should-not (bcat-sentence-template-name src))
    (should-not (string-match-p "CAT_TEMPLATE" (bcat-sentence-test--create src)))))

(ert-deftest bcat-sentence-old-renderers-leave-new-files-alone ()
  "Carsten: changes only through C-c u G.  The regenerator (C-c u R,
after-land re-render, migrations) and the old fire path leave a
template-built file byte-identical; the Provided-Translations
materializer skips it."
  (bcat-sentence-test--with-source
    (let* ((text (bcat-sentence-test--create src))
           (file (tibetan-sentence--filepath 9 folder src))
           (fired nil))
      (should (equal "tibetan" (bcat-sentence-file-p file)))
      ;; The source carries the same line — it is no sentence file.
      (should-not (bcat-sentence-file-p src))
      (tibetan-cascade--regenerate file 9 (tibetan-cascade--segs-for-sentence src 9) src)
      (tibetan-cascade--regenerate-after-land file)
      (should (plist-get (tibetan-cascade-materialize-provided-translations file) :skipped))
      (cl-letf (((symbol-function 'tibetan-sentence-claude--request)
                 (lambda (&rest _) (setq fired t)))
                ((symbol-function 'tibetan-sentence-claude--claim)
                 (lambda (&rest _) t)))
        (tibetan-cascade--fire-sentence-1
         (list :sent-num 9 :seg-nums '(38 39)) src folder nil))
      (should-not fired)
      (should (equal text (with-temp-buffer (insert-file-contents file)
                                            (buffer-string)))))))

(ert-deftest bcat-sentence-work-folder-template-wins ()
  "The cascade end to end: a tibetan.org in the work folder's
Templates/analysis/ shapes the new file instead of the tool's."
  (bcat-sentence-test--with-source
    (make-directory (expand-file-name "Templates/analysis" dir) t)
    (with-temp-file (expand-file-name "Templates/analysis/tibetan.org" dir)
      (insert "#+CAT_TEMPLATE: tibetan\n* Nur Wylie\n{{#segments}}\n{{wylie}}\n{{/segments}}\n"))
    (should (equal "#+CAT_TEMPLATE: tibetan\n* Nur Wylie\nbrjod du med pa bltar med pa/ /\n'gyur ba med pa 'dus ma byas/ /\n"
                   (bcat-sentence-test--create src)))))

;; ----------------------------------------------------------------------------
;; Step 3b — C-c u G on template files
;; ----------------------------------------------------------------------------

(defconst bcat-sentence-test--answer
  (concat
   "{\"segments\": ["
   "{\"num\": 38, \"words\": ["
   "{\"wylie\": \"brjod du med pa\", \"gloss\": \"indescribable\", \"function\": \"ADJ · predicate\"},"
   "{\"wylie\": \"bltar med pa\", \"gloss\": \"invisible\", \"function\": \"ADJ · predicate\"}],"
   " \"translation\": \"Indescribable and invisible,\","
   " \"references\": [{\"name\": \"Lindtner 1981\", \"text\": \"[It is also] inexpressible (avācya) and invisible (adraṣṭavya).\"},"
   "                  {\"name\": \"DharmaMitra\", \"text\": \"Inexpressible and invisible,\"}],"
   " \"shift\": \"\", \"structure\": \"no finite verb\", \"note\": \"\", \"concepts\": \"\"},"
   "{\"num\": 39, \"words\": ["
   "{\"wylie\": \"'gyur ba med pa\", \"gloss\": \"immutable\", \"function\": \"ADJ · predicate\"},"
   "{\"wylie\": \"'dus ma byas\", \"gloss\": \"uncompounded\", \"function\": \"ADJ · predicate\"}],"
   " \"translation\": \"immutable and uncompounded\","
   " \"references\": [{\"name\": \"DharmaMitra\", \"text\": \"Unchanging and unconditioned—\"}],"
   " \"shift\": \"\", \"structure\": \"continues the chain\", \"note\": \"\","
   " \"concepts\": \"*'dus ma byas* (asaṃskṛta)\"}],"
   " \"grammar\": \"Two predicate pādas.\","
   " \"translation\": \"Indescribable and invisible, immutable and uncompounded\","
   " \"comparison\": \"\"}")
  "A canned Claude answer for the two-pāda test verse.")

(defmacro bcat-sentence-test--with-stubs (answer &rest body)
  "Run BODY with DharmaMitra returning a fixed translation, every timer
run at once, and the Claude submit landing ANSWER immediately — a
string, or a function of the request's kind returning one.  Binds
PROMPTS (the (SYSTEM . USER) pairs sent, latest first), SUBMITS, KINDS
\(latest first), DONE (files the after-fetch hook saw) and COMPANIONS
\(old companion hook calls)."
  (declare (indent 1))
  `(let ((prompts '()) (submits 0) (done '()) (companions 0) (kinds '())
         (answer-of ,answer)
         (bcat-sentence-after-fetch-functions nil)
         (tibetan-cascade-after-fetch-functions nil)
         (tibetan-cascade-fetch-done-functions nil))
     (add-hook 'bcat-sentence-after-fetch-functions (lambda (f) (push f done)))
     (add-hook 'tibetan-cascade-after-fetch-functions
               (lambda (&rest _) (cl-incf companions) nil))
     (add-hook 'tibetan-cascade-fetch-done-functions
               (lambda (&rest _) (cl-incf companions)))
     (cl-letf (((symbol-function 'tibetan-dharmamitra-api-chat-translate)
                (lambda (&rest _) "Inexpressible and invisible, / Unchanging and unconditioned—"))
               ((symbol-function 'run-at-time)
                (lambda (_time _repeat fn &rest args) (apply fn args) nil))
               ((symbol-function 'tibetan-discussion--submit)
                (lambda (kind file thunk land)
                  (cl-incf submits)
                  (push kind kinds)
                  (push (funcall thunk) prompts)
                  (funcall land file (if (functionp answer-of) (funcall answer-of kind) answer-of))
                  t)))
       (ignore kinds)
       ,@body)))

(defun bcat-sentence-test--fetch (src)
  "C-c u G on sentence 9 of SRC."
  (tibetan-cascade-fetch (list :source src :sent-nums '(9) :label "Satz 9")))

(defun bcat-sentence-test--file-text (src)
  (with-temp-buffer
    (insert-file-contents (tibetan-sentence--filepath
                           9 (file-name-as-directory
                              (expand-file-name "analysis" (file-name-directory src)))
                           src))
    (buffer-string)))

(defun bcat-sentence-test--set-notes (file text)
  "Write TEXT as the body of * My Notes in FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (re-search-forward "^\\* My Notes\n")
    (insert text "\n")
    (write-region (point-min) (point-max) file nil 0)))

(ert-deftest bcat-sentence-fetch-fills-template-file ()
  "C-c u G on a template-built rump: DharmaMitra first, then ONE Claude
request whose prompt carries the segments, Lindtner and DharmaMitra;
the answer fills the file in the template form — tables from Claude's
words (no \"?\"), translation blocks, structure, concepts, grammar,
Translations compared — while My Notes and #+CREATED stay.  The old
companion requests do not run; the after-fetch hook (C-c u H) sees the
file once."
  (bcat-sentence-test--with-source
    (bcat-sentence-test--create src)
    (let ((file (tibetan-sentence--filepath 9 folder src)))
      (bcat-sentence-test--set-notes file "Meine Notiz zu Vers 9.")
      (with-temp-buffer             ; pretend it was created earlier
        (insert-file-contents file)
        (goto-char (point-min))
        (re-search-forward "^#\\+CREATED: .*$")
        (replace-match "#+CREATED: 2026-10-14")
        (write-region (point-min) (point-max) file nil 0))
      (bcat-sentence-test--with-stubs bcat-sentence-test--answer
        (bcat-sentence-test--fetch src)
        (should (= 1 submits))
        (let ((user (cdr (car prompts))))
          (should (string-match-p "\\[38\\] Tibetan: བརྗོད" user))
          (should (string-match-p "Wylie: brjod du med pa bltar med pa/ /" user))
          (should (string-match-p "=== Lindtner 1981 ===" user))
          (should (string-match-p "=== DharmaMitra ===\nInexpressible and invisible" user)))
        (should (string-match-p "in English" (car (car prompts))))
        (let ((text (bcat-sentence-test--file-text src)))
          (should (string-match-p "^#\\+CAT_TEMPLATE: tibetan$" text))
          (should (string-match-p "^#\\+CREATED: 2026-10-14$" text))
          (should (string-match-p (format "^#\\+LAST_ANALYZED: %s$"
                                          (format-time-string "%Y-%m-%d"))
                                  text))
          (should (string-match-p "| indescribable +| invisible" text))
          (should-not (string-match-p "^| \\?" text))
          (should (string-match-p (regexp-quote "⟦38⟧ *Translation:* Indescribable and invisible, \\\\\n*Lindtner 1981:*") text))
          (should (string-match-p (regexp-quote "*DharmaMitra:* Unchanging and unconditioned—") text))
          (should (string-match-p "^\\*Structure:\\* no finite verb$" text))
          (should (string-match-p "^\\*\\* Concepts\n⟦39⟧ \\*'dus ma byas\\* (asaṃskṛta)$" text))
          (should (string-match-p "^\\*\\* Grammar\nTwo predicate pādas\\.$" text))
          (should (string-match-p "^\\*Translation:\\* Indescribable and invisible, immutable" text))
          (should (string-match-p "^\\* My Notes\nMeine Notiz zu Vers 9\\.$" text)))
        (should (equal (list (tibetan-sentence--filepath 9 folder src)) done))
        (should (= 0 companions))))))

(ert-deftest bcat-sentence-second-language-option ()
  "Carsten 1.11.: the translation in the second language only as an
option — per sentence (`:CAT_SECOND_LANGUAGE: t' in its drawer) or for
the whole source (#+CAT_SECOND_LANGUAGE: t); without, the request does
not ask for it."
  (bcat-sentence-test--with-source
    (bcat-sentence-test--create src)
    (bcat-sentence-test--with-stubs bcat-sentence-test--answer
      (bcat-sentence-test--fetch src)
      (should-not (string-match-p "translation_de" (cdr (car prompts)))))
    (with-temp-file src
      (insert (replace-regexp-in-string ":VERSE: SDA 9\n" ":VERSE: SDA 9\n:CAT_SECOND_LANGUAGE: t\n"
                                        bcat-sentence-test--source t t)))
    (clrhash bcat-sentence--sentence-cache)
    (bcat-sentence-test--with-stubs bcat-sentence-test--answer
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_q) t)))
        (tibetan-cascade-fetch (list :source src :sent-nums '(9) :label "Satz 9") t))
      (should (string-match-p "\"translation_de\": the sentence translation once more in German"
                              (cdr (car prompts)))))
    (with-temp-file src
      (insert (replace-regexp-in-string "#\\+TIBETAN_TARGET_LANG: en\n"
                                        "\\&#+CAT_SECOND_LANGUAGE: t\n"
                                        bcat-sentence-test--source)))
    (clrhash bcat-sentence--sentence-cache)
    (bcat-sentence-test--with-stubs bcat-sentence-test--answer
      (cl-letf (((symbol-function 'y-or-n-p) (lambda (_q) t)))
        (tibetan-cascade-fetch (list :source src :sent-nums '(9) :label "Satz 9") t))
      (should (string-match-p "translation_de" (cdr (car prompts)))))))

(ert-deftest bcat-sentence-fetch-complete-file-asks-nothing ()
  "A second C-c u G on a filled file sends nothing (only what is
missing is fetched) and leaves the file as it is."
  (bcat-sentence-test--with-source
    (bcat-sentence-test--create src)
    (bcat-sentence-test--with-stubs bcat-sentence-test--answer
      (bcat-sentence-test--fetch src)
      (let ((before (bcat-sentence-test--file-text src)))
        (setq submits 0 done nil)
        (bcat-sentence-test--fetch src)
        (should (= 0 submits))
        (should-not done)
        (should (equal before (bcat-sentence-test--file-text src)))))))

(ert-deftest bcat-sentence-fetch-complete-says-how-to-refetch ()
  "29.10., TĀ 1007: C-c u G on a filled file did nothing, and the only
sign was „0 abgerufen … 1 vollständig“.  The message now says plainly
that nothing was fetched and how to fetch anew."
  (bcat-sentence-test--with-source
    (bcat-sentence-test--create src)
    (bcat-sentence-test--with-stubs bcat-sentence-test--answer
      (bcat-sentence-test--fetch src)
      (let ((messages '()))
        (cl-letf (((symbol-function 'message)
                   (lambda (fmt &rest args) (push (apply #'format-message fmt args) messages))))
          (bcat-sentence-test--fetch src))
        (should (cl-some (lambda (m) (and (string-match-p "vollständig" m)
                                          (string-match-p "nichts abgerufen" m)
                                          (string-match-p "C-u C-c u G" m)))
                         messages))))))

(ert-deftest bcat-sentence-fetch-status ()
  "Carsten 29.10.: „So eine Statusanzeige nach C-c u G wäre eigentlich
auch ganz nett.“  The status buffer shows per sentence what happened:
the file built, DharmaMitra, Claude — and for a filled file that
nothing was fetched and how to fetch anew; an unreadable answer with
its reason."
  (bcat-sentence-test--with-source
    (let ((bcat-status-show nil)
          (bcat-status--entries (make-hash-table :test 'equal))
          (bcat-analysis-response-directory (expand-file-name "responses" dir)))
      (when (get-buffer bcat-status-buffer-name) (kill-buffer bcat-status-buffer-name))
      (bcat-sentence-test--with-stubs bcat-sentence-test--answer
        (bcat-sentence-test--fetch src)
        (bcat-sentence-test--fetch src))
      (let ((text (with-current-buffer bcat-status-buffer-name (buffer-string))))
        (should (string-match-p "^C-c u G · Satz 9 · [0-9:]+\n  Satz 9  angelegt · DharmaMitra ✓ · Claude ✓$" text))
        (should (string-match-p "^  Satz 9  vollständig — nichts abgerufen (neu abrufen: C-u C-c u G)$" text)))
      (kill-buffer bcat-status-buffer-name)
      (bcat-sentence-test--with-stubs "no json"
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (_q) t)))
          (tibetan-cascade-fetch (list :source src :sent-nums '(9) :label "Satz 9") t)))
      (should (string-match-p "^  Satz 9  DharmaMitra ✓ · Claude: nicht lesbar (kein JSON-Objekt) — Antwort: .*sent-009-bden-"
                              (with-current-buffer bcat-status-buffer-name (buffer-string))))
      (kill-buffer bcat-status-buffer-name))))

(ert-deftest bcat-sentence-fetch-force-refetches ()
  "C-u C-c u G: a filled file is analysed anew (DharmaMitra + Claude)
after a question naming the count — the user slots stay; declined,
nothing is sent."
  (bcat-sentence-test--with-source
    (bcat-sentence-test--create src)
    (let ((file (tibetan-sentence--filepath 9 folder src)))
      (bcat-sentence-test--with-stubs bcat-sentence-test--answer
        (bcat-sentence-test--fetch src))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (re-search-forward "^\\* My Notes\n")
        (insert "Meine Notiz.\n")
        (write-region (point-min) (point-max) file nil 0))
      (let ((asked nil) (dm 0))
        (bcat-sentence-test--with-stubs bcat-sentence-test--answer
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (q) (setq asked q) nil)))
            (tibetan-cascade-fetch (list :source src :sent-nums '(9) :label "Satz 9") t))
          (should (string-match-p "1 Satz.*neu analysieren" asked))
          (should (= 0 submits))
          (cl-letf (((symbol-function 'y-or-n-p) (lambda (_q) t))
                    ((symbol-function 'tibetan-dharmamitra-api-chat-translate)
                     (lambda (&rest _) (cl-incf dm) "DM")))
            (tibetan-cascade-fetch (list :source src :sent-nums '(9) :label "Satz 9") t))
          (should (= 1 submits))
          (should (= 1 dm))
          (should (equal (list file) done)))
        (should (string-match-p "^\\* My Notes\nMeine Notiz\\.$" (bcat-sentence-test--file-text src)))))))

(ert-deftest bcat-sentence-fetch-rebuilds-old-layout ()
  "Carsten: „bei C-c u G komplett neu bauen, wenn noch im alten Layout“.
An old-layout file is rebuilt in the template form and analysed; My
Notes survives, and a translation pasted by hand into the old
Provided Translations becomes a reference (prompt + Translations
compared)."
  (bcat-sentence-test--with-source
    (let ((bcat-sentence-templates nil))
      (bcat-sentence-test--create src))
    (let ((file (tibetan-sentence--filepath 9 folder src)))
      (should-not (bcat-sentence-file-p file))
      (bcat-sentence-test--set-notes file "Alte Notiz.")
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-min))
        (re-search-forward "^\\* Working Translation")
        (beginning-of-line)
        (insert "** Eigene 2026\nMy own rendering of verse nine.\n\n")
        (write-region (point-min) (point-max) file nil 0))
      (bcat-sentence-test--with-stubs bcat-sentence-test--answer
        (bcat-sentence-test--fetch src)
        (should (= 1 submits))
        (should (string-match-p "=== Eigene 2026 ===\nMy own rendering" (cdr (car prompts))))
        (let ((text (bcat-sentence-test--file-text src)))
          (should (equal "tibetan" (bcat-sentence-file-p file)))
          (should-not (string-match-p "^\\* Provided Translations\\|^\\* Tibetan Analysis" text))
          (should (string-match-p "^\\* My Notes\nAlte Notiz\\.$" text))
          (should (string-match-p "^\\*Eigene 2026:\\* My own rendering of verse nine\\.$" text)))))))

(ert-deftest bcat-sentence-lectures ()
  "Carsten 30.10.: Sanderson's lectures on TĀ 1.1–15 (transcripts with
time stamps) belong in the sentence file of their verse — „als
Vorbereitung das PDF auf dem Remarkable anschauen und mir
handschriftliche Notizen machen. Deshalb will ich Sanderson im PDF
haben.“  A source with #+CAT_LECTURES: names a file of `** Sentence N'
/ `*** <lecture>' passages; the sentence file gets them under
* Further Reading (31.10. abends: at the end, after the explanations —
„Sanderson u.ä. am Schluss“; in the PDF).  In the prompt since 2.11.,
for the comparison of the translations only („Ja, nur beim Vergleich“),
the YouTube links reduced to their time marks."
  (bcat-sentence-test--with-source
    (with-temp-file src
      (insert (replace-regexp-in-string "#\\+TIBETAN_SENTENCE_REFS: refs.org\n"
                                        "\\&#+CAT_LECTURES: lectures.org\n"
                                        bcat-sentence-test--source)))
    (with-temp-file (expand-file-name "lectures.org" dir)
      (insert "#+TITLE: Sanderson — Passagen je Vers\n\n* TĀ 1\n** Sentence 9\n"
              "*** Sanderson, Readings I (27.01.2022)\n"
              "[[https://www.youtube.com/watch?v=hxaz7FbtPFk&t=620s][0:10:20]] It's an immensely tricky verse.\n\n"
              "[[https://www.youtube.com/watch?v=hxaz7FbtPFk&t=688s][0:11:28]] And Jayaratha, our only surviving commentator.\n"))
    (let ((text (bcat-sentence-test--create-n src 9)))
      (should (string-match-p
               (regexp-quote (concat "* Further Reading\n** Sanderson, Readings I (27.01.2022)\n"
                                     "[[https://www.youtube.com/watch?v=hxaz7FbtPFk&t=620s][0:10:20]] It's an immensely tricky verse.\n\n"
                                     "[[https://www.youtube.com/watch?v=hxaz7FbtPFk&t=688s][0:11:28]] And Jayaratha"))
               text))
      (should (< (string-match "^\\* Sentence$" text) (string-match "^\\* Segments$" text)
                 (string-match "^\\* Further Reading$" text)
                 (string-match "^\\* Working Translation$" text))))
    (bcat-sentence-test--with-stubs bcat-sentence-test--answer
      (bcat-sentence-test--fetch src)
      (should (string-match-p
               (regexp-quote (concat "Lectures on this sentence — only for the comparison (the findings), cite with the time mark:\n"
                                     "=== Sanderson, Readings I (27.01.2022) ===\n"
                                     "[0:10:20] It's an immensely tricky verse.\n\n"
                                     "[0:11:28] And Jayaratha, our only surviving commentator."))
               (cdr (car prompts))))
      (should-not (string-match-p "youtube" (cdr (car prompts))))
      (should (string-match-p "^\\* Further Reading\n\\*\\* Sanderson"
                              (bcat-sentence-test--file-text src))))
    (with-temp-buffer
      (insert (bcat-sentence-test--file-text src))
      (org-mode)
      (bcat-export--drop-machine-sections)
      (should (string-match-p "^\\* Further Reading$" (buffer-string))))))

(ert-deftest bcat-sentence-rump-target-language ()
  "2.11.: the rump carries the source's target language (`target-lang')
— the comparison is rendered in it („im Handout auch immer alles in der
Zielsprache“)."
  (bcat-sentence-test--with-source
    (should (equal "en" (alist-get 'target-lang
                                   (bcat-sentence-rump-context
                                    9 (tibetan-cascade--segs-for-sentence src 9) src))))))

(ert-deftest bcat-sentence-no-lectures ()
  "Without #+CAT_LECTURES: no * Lectures."
  (bcat-sentence-test--with-source
    (should-not (string-match-p "Lectures" (bcat-sentence-test--create-n src 9)))))

(ert-deftest bcat-sentence-status-dharmamitra-limit ()
  "30.10., TĀ 1002–1007: „DharmaMitra –“ on every line, the reason
(DharmaMitra's daily limit) only in *Messages*.  The status line says
why."
  (bcat-sentence-test--with-source
    (let ((bcat-status-show nil)
          (bcat-status--entries (make-hash-table :test 'equal)))
      (bcat-sentence-test--with-stubs bcat-sentence-test--answer
        (cl-letf (((symbol-function 'tibetan-dharmamitra-api-chat-translate) (lambda (&rest _) nil))
                  ((symbol-function 'tibetan-dharmamitra-api-limit-reached-p) (lambda () t)))
          (bcat-sentence-test--fetch src)))
      (should (string-match-p "DharmaMitra: Tageslimit erreicht"
                              (with-current-buffer bcat-status-buffer-name (buffer-string))))
      (kill-buffer bcat-status-buffer-name))))

(ert-deftest bcat-sentence-fetch-keeps-own-sections ()
  "30.10.: AS and MSA sentence files carry a `* Handout' section (the
input of handout-synopse.py, corrected in class, with the SG 2015
reference — put there because C-c u G rebuilds * Provided
Translations).  The rebuild from the old layout dropped every L1
section it did not know.  A section the tool does not write is kept
verbatim at the end — through the rebuild and every later landing,
never twice; the old machine sections are not carried over."
  (bcat-sentence-test--with-source
    (let ((bcat-sentence-templates nil))
      (bcat-sentence-test--create src))
    (let ((file (tibetan-sentence--filepath 9 folder src))
          (handout (concat "* Handout\n# Eingabe für handout-synopse.py\n"
                           "** Segment 38\n:PROPERTIES:\n:BO: 38\n:END:\n*** Skt.\nInexpressible.\n"
                           "** SG 2015 (Japanese, English draft)\n[Q] Why?\n")))
      (with-temp-buffer
        (insert-file-contents file)
        (goto-char (point-max))
        (insert "\n" handout)
        (write-region (point-min) (point-max) file nil 0))
      (bcat-sentence-test--with-stubs bcat-sentence-test--answer
        (bcat-sentence-test--fetch src)
        (let ((text (bcat-sentence-test--file-text src)))
          (should (equal "tibetan" (bcat-sentence-file-p file)))
          (should (string-match-p (regexp-quote (concat "* Footnotes\n\n" handout)) text))
          (should-not (string-match-p "^\\* Provided Translations\\|^\\* Tibetan Analysis\\|^\\* Reading" text)))
        ;; A later landing (C-u C-c u G) keeps it once.
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (&rest _) t)))
          (tibetan-cascade-fetch (list :source src :sent-nums '(9) :label "Satz 9") t))
        (let ((text (bcat-sentence-test--file-text src)))
          (should (= 1 (cl-count-if (lambda (l) (equal l "* Handout")) (split-string text "\n"))))
          (should (string-suffix-p handout text)))))))

(ert-deftest bcat-sentence-fetch-keeps-reference-text ()
  "28.10., bDen 9: Claude copied Lindtner's verse without his note, and
the note was gone from the file.  The paragraphs under Translations
compared are the reference texts as given (note included); only the
bold marks come from the answer."
  (bcat-sentence-test--with-source
    (with-temp-file (expand-file-name "refs.org" dir)
      (insert "#+TITLE: Lindtner 1981, versweise\n\n** Sentence 9\n*** Lindtner 1981\n"
              "[It is also] inexpressible (avācya) and invisible (adraṣṭavya). [fn:l9]\n\n"
              "[fn:l9] Lindtner 1981, S. 197: Eimer's \"Befleckungen\" is wrong.\n"))
    (bcat-sentence-test--create src)
    ;; As on 28.10.: the answer still carries its own copy of the
    ;; reference — without the note.
    (bcat-sentence-test--with-stubs
        (replace-regexp-in-string
         (regexp-quote " \"comparison\": \"\"}")
         (concat " \"references\": [{\"name\": \"Lindtner 1981\", \"text\":"
                 " \"[It is also] inexpressible (avācya) and invisible (adraṣṭavya).\"}],"
                 " \"comparison\": \"\"}")
         bcat-sentence-test--answer t t)
      (bcat-sentence-test--fetch src))
    ;; The note stays — since 1.11. at the end of Translations compared,
    ;; where the next heading ends the footnote (SDA 10: in the middle
    ;; it swallowed the paragraphs after it).
    (let ((text (bcat-sentence-test--file-text src)))
      (should (string-match-p
               (regexp-quote (concat "*Lindtner 1981:* [It is also] inexpressible (avācya) and "
                                     "invisible (adraṣṭavya). [fn:l9]\n\n*DharmaMitra:*"))
               text))
      (should (string-match-p
               (regexp-quote "\n\n[fn:l9] Lindtner 1981, S. 197: Eimer's \"Befleckungen\" is wrong.\n\n* Segments\n")
               text)))))

(ert-deftest bcat-sentence-fetch-unreadable-answer-keeps-file ()
  "An answer without a readable JSON object changes nothing; the hook
for C-c u H does not run."
  (bcat-sentence-test--with-source
    (let ((before (bcat-sentence-test--create src))
          (bcat-analysis-response-directory (expand-file-name "responses" dir)))
      (bcat-sentence-test--with-stubs "Sorry, I cannot help with that."
        (bcat-sentence-test--fetch src)
        (should (= 1 submits))
        (should-not done)
        (should (equal before (bcat-sentence-test--file-text src)))))))

(ert-deftest bcat-sentence-fetch-unreadable-answer-is-kept ()
  "28.10., Satz 9: „Antwort … nicht lesbar“ and the answer was gone —
nothing to diagnose.  An unreadable answer is saved under
`bcat-analysis-response-directory' (sentence file name + time), and
the message names the JSON error and that file."
  (bcat-sentence-test--with-source
    (bcat-sentence-test--create src)
    (let* ((bcat-analysis-response-directory (expand-file-name "responses" dir))
           (answer "{\"segments\": [], \"t\": 1 2}")
           (messages '()))
      (cl-letf (((symbol-function 'message)
                 (lambda (fmt &rest args)
                   (push (apply #'format-message fmt args) messages))))
        (bcat-sentence-test--with-stubs answer
          (bcat-sentence-test--fetch src)))
      (let ((saved (directory-files bcat-analysis-response-directory t
                                    "\\`sent-009-bden-.*\\.txt\\'")))
        (should (= 1 (length saved)))
        (should (equal answer (with-temp-buffer
                                (insert-file-contents (car saved))
                                (buffer-string))))
        (should (cl-some (lambda (m)
                           (and (string-match-p "nicht lesbar" m)
                                (string-match-p "JSON-Fehler bei Zeichen 25" m)
                                (string-match-p (regexp-quote (car saved)) m)))
                         messages))))))

(ert-deftest bcat-sentence-fetch-respects-defer-mt ()
  "#+TIBETAN_DEFER_MT: t (Portfolio) — no request at all."
  (bcat-sentence-test--with-source
    (with-temp-file src
      (insert (replace-regexp-in-string
               "#\\+TIBETAN_LAYOUT: cascade\n"
               "#+TIBETAN_LAYOUT: cascade\n#+TIBETAN_DEFER_MT: t\n"
               bcat-sentence-test--source)))
    (bcat-sentence-test--with-stubs bcat-sentence-test--answer
      (bcat-sentence-test--fetch src)
      (should (= 0 submits)))))

(ert-deftest bcat-sentence-fetch-state ()
  "What C-c u G still has to do for a template file: `full' while a
segment has no ⟦N⟧ translation, nil when complete, `align' when a
reference was added under Translations compared that no segment shows."
  (bcat-sentence-test--with-source
    (bcat-sentence-test--create src)
    (let ((file (tibetan-sentence--filepath 9 folder src)))
      (should (eq 'full (bcat-sentence-fetch-state file '(38 39))))
      (bcat-sentence-test--with-stubs bcat-sentence-test--answer
        (bcat-sentence-test--fetch src))
      (should-not (bcat-sentence-fetch-state file '(38 39)))
      (with-temp-buffer
        (insert-file-contents file)
        (bcat-sentence-test--goto-compared-end)
        (insert "*Ruegg 1981:* A newly added rendering.\n\n")
        (write-region (point-min) (point-max) file nil 0))
      (should (eq 'align (bcat-sentence-fetch-state file '(38 39)))))))

;; ----------------------------------------------------------------------------
;; Step 3c — a reference added by hand gets its segment parts
;; ----------------------------------------------------------------------------

(defconst bcat-sentence-test--align-answer
  (concat "{\"segments\": ["
          "{\"num\": 38, \"references\": [{\"name\": \"Ruegg 1981\", \"text\": \"*Unsayable* and unseen,\"}],"
          " \"shift\": \"Ruegg 1981: modal „unsayable“ for „indescribable“\"},"
          "{\"num\": 39, \"references\": [{\"name\": \"Ruegg 1981\", \"text\": \"unchanging, unconditioned.\"}], \"shift\": \"\"}],"
          " \"comparison\": \"Ruegg reads a modal nuance.\"}")
  "Alignment answer for the reference Ruegg 1981 added by hand.")

(defun bcat-sentence-test--goto-compared-end ()
  "Point to the end of Translations compared in the current buffer: the
next L1 heading after it (since 31.10. * Segments or * Tibetan follows)."
  (goto-char (point-min))
  (re-search-forward "^\\*\\* Translations compared$")
  (re-search-forward "^\\* ")
  (beginning-of-line))

(defun bcat-sentence-test--add-reference (file name text)
  "Add the paragraph *NAME:* TEXT under Translations compared of FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (bcat-sentence-test--goto-compared-end)
    (insert (format "*%s:* %s\n\n" name text))
    (write-region (point-min) (point-max) file nil 0)))

(ert-deftest bcat-sentence-own-translations-labelled ()
  "The own translation of a segment is read with or without its
„*Translation:*“ label (29.10.; the files of 28.10. have none) — the
alignment request gets the bare sentence."
  (should (equal '((38 . "Inexpressible,") (39 . "unchanging."))
                 (bcat-sentence--own-translations
                  (concat "⟦38⟧ *Translation:* Inexpressible, \\\\\n"
                          "*Lindtner 1981:* x\n\n⟦39⟧ unchanging.\n")))))

(ert-deftest bcat-sentence-align-merge ()
  "The pure merge: the new reference's part goes into each segment's
translation block after the existing references, its „→“ line after
the existing ones, every line but the last with the org line break; the
plain paragraph under Translations compared becomes the marked one; the
comparison sentence is added; nothing else changes."
  (let* ((text (concat "* Segments\n** Segment 38\n| t |\n\n"
                       "⟦38⟧ Own. \\\\\n*Lindtner 1981:* L. \\\\\n→ Lindtner 1981: x\n\n"
                       "*Structure:* s\n\n"
                       "** Segment 39\n| t |\n\n⟦39⟧ Own two.\n\n"
                       "* Sentence\n** Grammar\nG\n\n** Translations compared\n"
                       "*Translation:* Own. Own two.\n\n*Lindtner 1981:* L.\n\n"
                       "*Ruegg 1981:* Unsayable and unseen, unchanging, unconditioned.\n\n"
                       "* Working Translation\n"))
         (out (bcat-sentence--align-merge text (bcat-analysis-parse bcat-sentence-test--align-answer))))
    (should (string-match-p
             (regexp-quote (concat "⟦38⟧ Own. \\\\\n*Lindtner 1981:* L. \\\\\n*Ruegg 1981:* *Unsayable* and unseen, \\\\\n"
                                   "→ Lindtner 1981: x \\\\\n→ Ruegg 1981: modal „unsayable“ for „indescribable“\n\n*Structure:* s"))
             out))
    (should (string-match-p (regexp-quote "⟦39⟧ Own two. \\\\\n*Ruegg 1981:* unchanging, unconditioned.\n\n* Sentence") out))
    (should (string-match-p (regexp-quote "*Ruegg 1981:* *Unsayable* and unseen, unchanging, unconditioned.\n\nRuegg reads a modal nuance.\n\n* Working Translation") out))
    (should (string-match-p (regexp-quote "*Lindtner 1981:* L.\n\n*Ruegg") out))
    (should (string-match-p "^\\*Structure:\\* s$" out))))

(ert-deftest bcat-sentence-align-merge-new-layout ()
  "31.10., the order of TĀ 1006: ⟦N⟧ labels the tables under * Text and
the concepts under * Sentence too, and * Sentence comes before
* Segments.  The new reference's part still goes into the segment's
translation block under * Segments; tables and concepts stay as they
are."
  (let* ((text (concat "* Text\n⟦38⟧\n| t |\n\n*Translation:* Own.\n\n"
                       "* Sentence\n** Grammar\nG\n\n** Concepts\n⟦38⟧ *term* — a note.\n\n"
                       "** Translations compared\n*Translation:* Own. Own two.\n\n*Lindtner 1981:* L.\n\n"
                       "*Ruegg 1981:* Unsayable and unseen, unchanging, unconditioned.\n\n"
                       "* Segments\n** Segment 38\n⟦38⟧ Own. \\\\\n*Lindtner 1981:* L.\n\n*Structure:* s\n\n"
                       "** Segment 39\n⟦39⟧ Own two.\n\n"
                       "* Working Translation\n"))
         (out (bcat-sentence--align-merge text (bcat-analysis-parse bcat-sentence-test--align-answer))))
    (should (string-match-p (regexp-quote "** Concepts\n⟦38⟧ *term* — a note.\n\n") out))
    (should (string-match-p (regexp-quote "* Text\n⟦38⟧\n| t |\n") out))
    (should (string-match-p
             (regexp-quote (concat "⟦38⟧ Own. \\\\\n*Lindtner 1981:* L. \\\\\n*Ruegg 1981:* *Unsayable* and unseen, \\\\\n"
                                   "→ Ruegg 1981: modal „unsayable“ for „indescribable“\n\n*Structure:* s"))
             out))
    (should (string-match-p (regexp-quote "Ruegg reads a modal nuance.\n\n* Segments") out))))

(ert-deftest bcat-sentence-align-merge-footnotes-last ()
  "The comparison sentence of a reference added by hand goes before the
footnote definitions at the end of Translations compared — after one it
would be swallowed into the footnote (SDA 10, 1.11.)."
  (let* ((text (concat "* Sentence\n** Translations compared\n"
                       "*Translation:* Own. Own two.\n\n*Lindtner 1981:* L. [fn:l]\n\n"
                       "*Ruegg 1981:* Unsayable and unseen, unchanging, unconditioned.\n\n"
                       "[fn:l] Lindtner's note.\n\n"
                       "* Segments\n** Segment 38\n⟦38⟧ Own.\n\n** Segment 39\n⟦39⟧ Own two.\n"))
         (out (bcat-sentence--align-merge text (bcat-analysis-parse bcat-sentence-test--align-answer))))
    (should (string-match-p (regexp-quote "Ruegg reads a modal nuance.\n\n[fn:l] Lindtner's note.\n\n* Segments") out))))

(ert-deftest bcat-sentence-align-merge-before-units ()
  "2.11.: Translations compared has the units of the comparison below the
translations (*** Übersicht …).  The marked paragraph of a reference
added by hand and its comparison sentence stay above them, the footnote
definitions after them; the units stay as they are."
  (let* ((text (concat "* Sentence\n** Translations compared\n"
                       "*Translation:* Own. Own two.\n\n*Lindtner 1981:* L. [fn:l]\n\n"
                       "*Ruegg 1981:* Unsayable and unseen, unchanging, unconditioned.\n\n"
                       "[fn:l] Lindtner's note.\n\n"
                       "*** Übersicht\nSigel: C = Translation (oben), L = Lindtner 1981\n\n"
                       "*** Die Stellen im Einzelnen\n**** 1. /brjod du med pa/ — gelb\n*Befund:* b\n\n"
                       "* Segments\n** Segment 38\n⟦38⟧ Own.\n\n** Segment 39\n⟦39⟧ Own two.\n"))
         (out (bcat-sentence--align-merge text (bcat-analysis-parse bcat-sentence-test--align-answer))))
    (should (string-match-p (regexp-quote "*Ruegg 1981:* *Unsayable* and unseen, unchanging, unconditioned.\n\nRuegg reads a modal nuance.\n\n[fn:l] Lindtner's note.\n\n*** Übersicht\n") out))
    (should (string-match-p (regexp-quote "*** Die Stellen im Einzelnen\n**** 1. /brjod du med pa/ — gelb\n*Befund:* b\n\n* Segments") out))))

(ert-deftest bcat-sentence-fetch-aligns-added-reference ()
  "Carsten: „Wenn ich weitere Vergleichsübersetzungen einfüge
funktioniert das dann automatisch bei C-c u G?“ — yes: a filled file
with a reference added under Translations compared gets ONE small
request (the new reference, the segments with their translations — no
DharmaMitra, no full analysis); the parts land in the segments, the
file is complete afterwards, the hook (C-c u H) runs."
  (bcat-sentence-test--with-source
    (bcat-sentence-test--create src)
    (let ((file (tibetan-sentence--filepath 9 folder src)))
      (bcat-sentence-test--with-stubs bcat-sentence-test--answer
        (bcat-sentence-test--fetch src))
      (bcat-sentence-test--add-reference file "Ruegg 1981"
                                         "Unsayable and unseen, unchanging, unconditioned.")
      (let ((dm-calls 0))
        (bcat-sentence-test--with-stubs bcat-sentence-test--align-answer
          (cl-letf (((symbol-function 'tibetan-dharmamitra-api-chat-translate)
                     (lambda (&rest _) (cl-incf dm-calls) "DM")))
            (bcat-sentence-test--fetch src))
          (should (= 1 submits))
          (should (= 0 dm-calls))
          (let ((user (cdr (car prompts))))
            (should (string-match-p "=== Ruegg 1981 ===\nUnsayable" user))
            (should-not (string-match-p "=== Lindtner 1981 ===" user))
            (should (string-match-p "Translation: Indescribable and invisible," user)))
          (let ((text (bcat-sentence-test--file-text src)))
            (should (string-match-p (regexp-quote "*Ruegg 1981:* *Unsayable* and unseen, \\\\") text))
            (should (string-match-p "^→ Ruegg 1981: modal" text)))
          (should-not (bcat-sentence-fetch-state file '(38 39)))
          (should (equal (list file) done)))))))

(defconst bcat-sentence-test--dm-align-answer
  (concat "{\"segments\": ["
          "{\"num\": 38, \"references\": [{\"name\": \"DharmaMitra\", \"text\": \"Inexpressible and invisible,\"}], \"shift\": \"\"},"
          "{\"num\": 39, \"references\": [{\"name\": \"DharmaMitra\", \"text\": \"unchanging and unconditioned\"}], \"shift\": \"\"}],"
          " \"comparison\": \"\"}")
  "Alignment answer placing DharmaMitra's translation.")

(defun bcat-sentence-test--without-dm (file)
  "Remove every DharmaMitra paragraph and line from FILE."
  (let ((text (bcat-sentence-test--text-of file)))
    (with-temp-file file
      (insert (replace-regexp-in-string "^\\*DharmaMitra:\\*.*\\(?:\n\\|\\'\\)" "" text)))))

(ert-deftest bcat-sentence-fetch-fills-missing-dharmamitra ()
  "Carsten 30.10.: „C-c u G sollte Dharma Mitra ggf. auch Claude
abholen, wenn sie leer sind.“  TĀ 1000–1007 were analysed while
DharmaMitra's daily limit held — complete, but without DharmaMitra, and
C-c u G called them complete.  Such a file is `dm': C-c u G fetches
DharmaMitra alone and places it with ONE small alignment request (no
new analysis); the file is complete afterwards, the hook (C-c u H) runs.
While the limit holds: no Claude request, the status says why."
  (bcat-sentence-test--with-source
    (bcat-sentence-test--create src)
    (let ((file (tibetan-sentence--filepath 9 folder src)))
      (bcat-sentence-test--with-stubs bcat-sentence-test--answer
        (bcat-sentence-test--fetch src))
      (bcat-sentence-test--without-dm file)
      (should (eq 'dm (bcat-sentence-fetch-state file '(38 39))))
      ;; The limit holds: nothing for Claude.
      (let ((bcat-status-show nil) (bcat-status--entries (make-hash-table :test 'equal)))
        (bcat-sentence-test--with-stubs bcat-sentence-test--dm-align-answer
          (cl-letf (((symbol-function 'tibetan-dharmamitra-api-chat-translate) (lambda (&rest _) nil))
                    ((symbol-function 'tibetan-dharmamitra-api-limit-reached-p) (lambda () t)))
            (bcat-sentence-test--fetch src))
          (should (= 0 submits))
          (should (string-match-p "DharmaMitra: Tageslimit erreicht"
                                  (with-current-buffer bcat-status-buffer-name (buffer-string))))
          (kill-buffer bcat-status-buffer-name)))
      (should (eq 'dm (bcat-sentence-fetch-state file '(38 39))))
      ;; DharmaMitra answers: one alignment request, no analysis.
      (let ((dm-calls 0))
        (bcat-sentence-test--with-stubs bcat-sentence-test--dm-align-answer
          (cl-letf (((symbol-function 'tibetan-dharmamitra-api-chat-translate)
                     (lambda (&rest _) (cl-incf dm-calls) "Inexpressible and invisible, unchanging and unconditioned.")))
            (bcat-sentence-test--fetch src))
          (should (= 1 dm-calls))
          (should (equal '("bcat-align") kinds))
          (should (string-match-p "=== DharmaMitra ===\nInexpressible" (cdr (car prompts))))
          (let ((text (bcat-sentence-test--file-text src)))
            (should (string-match-p "^\\*DharmaMitra:\\* Inexpressible and invisible, unchanging" text))
            (should (string-match-p "^\\*DharmaMitra:\\* Inexpressible and invisible,$" text))
            (should (< (string-match "^\\*\\* Translations compared" text)
                       (string-match "^\\*DharmaMitra:\\* Inexpressible and invisible, unchanging" text)
                       (string-match "^\\* Working Translation" text))))
          (should-not (bcat-sentence-fetch-state file '(38 39)))
          (should (equal (list file) done)))))))

;; ----------------------------------------------------------------------------
;; Sanskrit (§5.74, Carsten 29.10.)
;; ----------------------------------------------------------------------------

(defun bcat-sentence-test--sa-answer ()
  "Claude's answer for TĀ sentence 1007 (Sanskrit contract)."
  (with-temp-buffer
    (insert-file-contents
     (expand-file-name "fixtures/bcat-analysis-sent-1007.json"
                       (file-name-directory (locate-library "bcat-sentence-test"))))
    (buffer-string)))

(defun bcat-sentence-test--fetch-n (src n)
  "C-c u G on sentence N of SRC."
  (tibetan-cascade-fetch (list :source src :sent-nums (list n) :label (format "Satz %d" n))))

(defun bcat-sentence-test--file-text-n (src n)
  (with-temp-buffer
    (insert-file-contents (tibetan-sentence--filepath
                           n (file-name-as-directory
                              (expand-file-name "analysis" (file-name-directory src)))
                           src))
    (buffer-string)))

(ert-deftest bcat-sentence-fetch-sanskrit ()
  "C-c u G on a Sanskrit sentence: the Sanskrit prompt (padapāṭha,
Devavāṇīpraveśikā terms) with the segments as transmitted, ONE request;
the answer lands in the Sanskrit template — recitation kept, padapāṭha
with vocabulary under * Text, tables, Forms per segment — and the hook
\(C-c u H) runs."
  (bcat-sentence-test--with-sa-source
    (bcat-sentence-test--create-n src 1007)
    (bcat-sentence-test--with-stubs (bcat-sentence-test--sa-answer)
      (bcat-sentence-test--fetch-n src 1007)
      (should (= 1 submits))
      (let ((sys (car (car prompts))) (user (cdr (car prompts))))
        (should (string-match-p "Devavāṇīpraveśikā" sys))
        (should (string-match-p (regexp-quote "[685] atra madvāgaśaktāpi yanniyantraṇamullaset /") user))
        (should-not (string-match-p "Wylie" user)))
      (let ((text (bcat-sentence-test--file-text-n src 1007)))
        ;; 31.10. abends (TĀ 1006): the recitation without the metre's
        ;; description; no padapāṭha under * Text, the tables instead.
        (should (string-match-p "^\\* Recitation\n#\\+begin_verse\nअत्र" text))
        (should-not (string-match-p "Śloka (Anuṣṭubh)\\|atra \\[here\\]" text))
        (should (string-match-p (regexp-quote "* Text\n⟦685⟧\n| atra |") text))
        (should (string-match-p (regexp-quote "| atra | mad-vāk ") text))
        (should (string-match-p "^\\*Forms:\\*\n- /mad-vāk/ — vāc- f\\." text))
        (should (string-match-p "^⟦685⟧ \\*Translation:\\* Although my speech" text))
        (should (string-match-p "^\\*Dyczkowski 2023:\\* Although my speech is powerless here" text))
        (should-not (string-match-p "Wylie\\|steinert:" text)))
      (should (= 1 (length done))))))

(ert-deftest bcat-sentence-fetch-aligns-sanskrit-reference ()
  "A reference added by hand to a filled Sanskrit file: the small
alignment request speaks of a Sanskrit sentence and shows the segments
as transmitted with their translation — no Tibetan, no Wylie."
  (bcat-sentence-test--with-sa-source
    (bcat-sentence-test--create-n src 1007)
    (let ((file (tibetan-sentence--filepath 1007 folder src)))
      (bcat-sentence-test--with-stubs (bcat-sentence-test--sa-answer)
        (bcat-sentence-test--fetch-n src 1007))
      (bcat-sentence-test--add-reference file "Silburn 1998" "Although my speech is incapable here.")
      (bcat-sentence-test--with-stubs
          (concat "{\"segments\": [{\"num\": 685, \"references\": [{\"name\": \"Silburn 1998\","
                  " \"text\": \"Although my speech is incapable here.\"}], \"shift\": \"\"},"
                  " {\"num\": 686, \"references\": [], \"shift\": \"\"}], \"comparison\": \"\"}")
        (bcat-sentence-test--fetch-n src 1007)
        (should (= 1 submits))
        (let ((sys (car (car prompts))) (user (cdr (car prompts))))
          (should (string-match-p "a Sanskrit sentence" sys))
          (should-not (string-match-p "Tibetan" sys))
          (should (string-match-p (regexp-quote "[685] atra madvāgaśaktāpi yanniyantraṇamullaset /\n     Translation: Although my speech (vāc)") user))
          (should-not (string-match-p "Wylie\\|Tibetan" user)))
        (should (string-match-p "^\\*Silburn 1998:\\* Although my speech is incapable here\\.$"
                                (bcat-sentence-test--file-text-n src 1007)))))))

;; ----------------------------------------------------------------------------
;; Bilingual — Sanskrit with the Tibetan translation (§5.74, Carsten 30.10.)
;; ----------------------------------------------------------------------------

(defconst bcat-sentence-test--bil-source
  (concat "#+TITLE: Abhidharmasamuccaya — CAT-Quelle (Sanskrit)\n"
          "#+TIBETAN_LAYOUT: cascade\n"
          "#+CAT_TEMPLATE: bilingual\n"
          "#+CAT_PARALLEL: ../Tibetisch/kunbtus-cat.org\n"
          "#+SOURCE_LANG: sa\n"
          "#+TIBETAN_TARGET_LANG: en\n"
          "#+TIBETAN_SENTENCE_REFS: refs.org\n"
          "#+TIBETAN_CLAUDE_CONTEXT: Asaṅga, Abhidharmasamuccaya (Sanskrit).\n\n"
          "* Sanskrit Text\n** Section skandha\n"
          "*** Sentence 3\n"
          "**** Segment 22\nkasmāt pañcaiva skandhāḥ.\n"
          "**** Segment 23\npañcākārātmavastūdbhāvanatām upādāya.\n\n"
          "*** Sentence 4\n"
          "**** Segment 34\nkasmād aṣṭādaśaiva dhātavaḥ.\n")
  "A bilingual source: AS sentence 3 (two segments) in Sanskrit, and a
sentence 4 that the Tibetan lacks.")

(defconst bcat-sentence-test--bil-tibetan
  (concat "#+TITLE: Chos mngon pa kun las btus pa — CAT-Quelle\n"
          "#+TIBETAN_LAYOUT: cascade\n"
          "#+CAT_TEMPLATE: tibetan\n"
          "#+TIBETAN_TARGET_LANG: en\n"
          "#+TIBETAN_CLAUDE_CONTEXT: Asaṅga, chos mngon pa kun las btus pa (Toh 4049).\n\n"
          "* Tibetan Text\n** Section skandha\n"
          "*** Sentence 3\n"
          "**** Segment 77\nཅིའི་ཕྱིར་ཕུང་པོ་རྣམས་ལྔ་ཁོ་ན་ཞེ་ན།\n"
          "**** Segment 78\nབདག་གི་གཞི་རྣམ་པ་ལྔ་བརྗོད་པའི་ཕྱིར་ཏེ།\n"
          "**** Segment 79\nབདག་གི་གཞི་ལུས་ཡོངས་སུ་གཟུང་བ་དང་བཅས་པ་བརྗོད་པ་དང་།\n")
  "Its Tibetan translation: sentence 3 in three segments (77 for 22,
78–79 for 23).")

(defmacro bcat-sentence-test--with-bil-source (&rest body)
  "Temp bilingual pair: SRC (Sanskrit/asam-cat.org, refs.org beside it)
and its Tibetan (Tibetisch/kunbtus-cat.org) in DIR; FOLDER = SRC's
analysis/.  No dictionary (nil rows)."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "bcat-bil-" t))
          (src (expand-file-name "Sanskrit/asam-cat.org" dir))
          (folder (file-name-as-directory (expand-file-name "Sanskrit/analysis" dir)))
          (bcat-sentence-templates t)
          (bcat-analysis-response-directory (expand-file-name "responses" dir))
          (bcat-template-directory
           (expand-file-name "../Templates/analysis"
                             (file-name-directory (locate-library "bcat-sentence-test")))))
     (unwind-protect
         (progn
           (make-directory (expand-file-name "Sanskrit" dir) t)
           (make-directory (expand-file-name "Tibetisch" dir) t)
           (with-temp-file src (insert bcat-sentence-test--bil-source))
           (with-temp-file (expand-file-name "Sanskrit/refs.org" dir)
             (insert "#+TITLE: Rahula/Boin-Webb\n\n** Sentence 3\n*** Rahula/Boin-Webb 2001\n"
                     "Why are there only five aggregates? Because of the five methods.\n"))
           (with-temp-file (expand-file-name "Tibetisch/kunbtus-cat.org" dir)
             (insert bcat-sentence-test--bil-tibetan))
           (cl-letf (((symbol-function 'tibetan-gloss-table--unit-rows) (lambda (&rest _) nil)))
             ,@body))
       (dolist (b (buffer-list))
         (when (and (buffer-file-name b)
                    (string-prefix-p (file-truename dir)
                                     (file-truename (buffer-file-name b))))
           (with-current-buffer b (set-buffer-modified-p nil))
           (kill-buffer b)))
       (delete-directory dir t))))

(defconst bcat-sentence-test--bil-answer-sa
  (concat
   "{\"segments\": ["
   "{\"num\": 22, \"words\": [{\"word\": \"kasmāt\", \"lemma\": \"kim\", \"formation\": \"—\", \"parts\": \"\", \"gloss\": \"why\", \"form\": \"pron. abl. sg.\"},"
   " {\"word\": \"pañca\", \"lemma\": \"pañcan\", \"formation\": \"—\", \"parts\": \"\", \"gloss\": \"five\", \"form\": \"num.\"},"
   " {\"word\": \"eva\", \"lemma\": \"eva\", \"formation\": \"—\", \"parts\": \"\", \"gloss\": \"only\", \"form\": \"ind.\"},"
   " {\"word\": \"skandhāḥ\", \"lemma\": \"skandha\", \"formation\": \"—\", \"parts\": \"\", \"gloss\": \"aggregates\", \"form\": \"m. nom. pl.\"}],"
   " \"translation\": \"Why are there just five aggregates? —\","
   " \"references\": [{\"name\": \"Rahula/Boin-Webb 2001\", \"text\": \"Why are there only five aggregates?\"},"
   "                  {\"name\": \"DharmaMitra\", \"text\": \"Inexpressible and invisible,\"}],"
   " \"shift\": [], \"sandhi\": \"pañca + eva → pañcaiva\", \"forms\": [], \"compounds\": [],"
   " \"structure\": \"\", \"note\": \"\", \"concepts\": \"\"},"
   "{\"num\": 23, \"words\": [{\"word\": \"pañca-ākāra-ātma-vastu-udbhāvanatām\", \"lemma\": \"x\", \"formation\": \"x\", \"parts\": \"x\", \"gloss\": \"fivefold self-basis\", \"form\": \"f. acc. sg.\"},"
   " {\"word\": \"upādāya\", \"lemma\": \"upa-ā-√dā\", \"formation\": \"upa-ā-√dā-ya\", \"parts\": \"x\", \"gloss\": \"on account of\", \"form\": \"ger.\"}],"
   " \"translation\": \"On account of their bringing to light the basis of self in five modes.\","
   " \"references\": [{\"name\": \"Rahula/Boin-Webb 2001\", \"text\": \"Because of the five methods.\"},"
   "                  {\"name\": \"DharmaMitra\", \"text\": \"Unchanging and unconditioned—\"}],"
   " \"shift\": [], \"sandhi\": \"\", \"forms\": [], \"compounds\": [],"
   " \"structure\": \"\", \"note\": \"\", \"concepts\": \"\"}],"
   " \"grammar\": \"Question and answer.\","
   " \"translation\": \"Why are there just five aggregates? — On account of their bringing to light the basis of self in five modes.\","
   " \"comparison\": \"\"}")
  "The Sanskrit answer for the test sentence 3.")

(defconst bcat-sentence-test--bil-answer-bo
  (concat
   "{\"segments\": ["
   "{\"num\": 77, \"sanskrit\": 22, \"words\": [{\"wylie\": \"ci'i phyir\", \"gloss\": \"why\", \"function\": \"INT\"},"
   " {\"wylie\": \"phung po\", \"gloss\": \"aggregate\", \"function\": \"N\"}, {\"wylie\": \"rnams\", \"gloss\": \"plural\", \"function\": \"PL\"},"
   " {\"wylie\": \"lnga\", \"gloss\": \"five\", \"function\": \"NUM\"}, {\"wylie\": \"kho na\", \"gloss\": \"only\", \"function\": \"ADV\"},"
   " {\"wylie\": \"zhe na\", \"gloss\": \"if one asks\", \"function\": \"QUOT\"}]},"
   "{\"num\": 78, \"sanskrit\": 23, \"words\": []},"
   "{\"num\": 79, \"sanskrit\": 23, \"words\": []}],"
   " \"differences\": [{\"sanskrit\": 23, \"text\": \"/udbhāvanatā/ is rendered /brjod pa/ („stating“).\"}],"
   " \"rendering\": \"The Tibetan states the self-basis rather than bringing it to light.\"}")
  "The Tibetan answer for the test sentence 3: 77 with 22, 78–79 with 23.")

(defun bcat-sentence-test--bil-answers (kind)
  "The canned answer for the request KIND."
  (if (equal kind "bcat-tibetan") bcat-sentence-test--bil-answer-bo bcat-sentence-test--bil-answer-sa))

(ert-deftest bcat-sentence-create-bilingual ()
  "C-c u A (no AI) on a bilingual source: the bilingual template — the
Sanskrit as Devanāgarī over IAST, the Tibetan of the same sentence from
#+CAT_PARALLEL as Uchen over Wylie, a link to it in the header; no
recitation; the segments have no Tibetan yet (Claude assigns it)."
  (bcat-sentence-test--with-bil-source
    (let ((text (bcat-sentence-test--create-n src 3)))
      (should (string-match-p "^#\\+CAT_TEMPLATE: bilingual$" text))
      (should (string-match-p (regexp-quote "#+CAT_PARALLEL: [[file:../../Tibetisch/kunbtus-cat.org::*Sentence 3][kunbtus-cat.org / Sentence 3]]") text))
      (should (string-match-p "^#\\+CAT_PARALLEL_SEGMENTS: 77, 78, 79$" text))
      (should (string-match-p (regexp-quote "* Text\n#+begin_verse\nकस्मात् पञ्चैव स्कन्धाः।\nkasmāt pañcaiva skandhāḥ.\n") text))
      (should (string-match-p (regexp-quote "#+begin_verse\nཅིའི་ཕྱིར་ཕུང་པོ་རྣམས་ལྔ་ཁོ་ན་ཞེ་ན།\nci'i phyir phung po rnams lnga kho na zhe na/\n") text))
      (should (string-match-p "^\\*\\* Segment 22$" text))
      (should-not (string-match-p "Recitation\\|· Tib\\.\\|Differences" text))
      ;; Carsten 30.10.: no IAST block without Devanāgarī — before C-c u G
      ;; it only repeated the lines under the Devanāgarī.
      (should (= 1 (with-temp-buffer (insert text)
                     (how-many "^kasmāt pañcaiva skandhāḥ\\.$" (point-min) (point-max))))))))

(ert-deftest bcat-sentence-create-bilingual-without-tibetan ()
  "A sentence the Tibetan lacks: the Sanskrit alone, no empty Tibetan
block, no parallel link."
  (bcat-sentence-test--with-bil-source
    (let ((text (bcat-sentence-test--create-n src 4)))
      (should (string-match-p "kasmād aṣṭādaśaiva dhātavaḥ\\." text))
      (should-not (string-match-p "CAT_PARALLEL\\|[ༀ-࿿]" text))
      (should-not (string-match-p "#\\+begin_verse\n#\\+end_verse" text)))))

(ert-deftest bcat-sentence-fetch-bilingual ()
  "C-c u G on a bilingual sentence: first the Sanskrit request (as for a
Sanskrit source), then — with its answer — the Tibetan request: the
Sanskrit lines with the translations of the Sanskrit answer, the
Tibetan with Wylie, the Tibetan source's context.  The file gets each
Sanskrit segment's Tibetan (heading, Uchen over glossed Wylie), the
differences, the rendering; C-c u H runs once, at the end."
  (bcat-sentence-test--with-bil-source
    (bcat-sentence-test--create-n src 3)
    (bcat-sentence-test--with-stubs #'bcat-sentence-test--bil-answers
      (bcat-sentence-test--fetch-n src 3)
      (should (equal '("bcat-tibetan" "bcat-analysis") kinds))
      (should (string-match-p "Devavāṇīpraveśikā" (car (nth 1 prompts))))
      (let ((sys (car (car prompts))) (user (cdr (car prompts))))
        (should (string-match-p "never decrease" sys))
        (should (string-match-p (regexp-quote "Asaṅga, chos mngon pa kun las btus pa (Toh 4049).\n\nSentence 3 — Sanskrit segments:\n[22] kasmāt pañcaiva skandhāḥ.\n     Translation: Why are there just five aggregates? —\n") user))
        (should (string-match-p (regexp-quote "Tibetan segments:\n[77] Tibetan: ཅིའི་ཕྱིར་ཕུང་པོ་རྣམས་ལྔ་ཁོ་ན་ཞེ་ན།\n     Wylie: ci'i phyir phung po rnams lnga kho na zhe na/\n") user)))
      (let ((text (bcat-sentence-test--file-text-n src 3)))
        (should (string-match-p "^\\*\\* Segment 22 · Tib\\. 77$" text))
        (should (string-match-p "^\\*\\* Segment 23 · Tib\\. 78–79$" text))
        ;; 1.11.: the Sanskrit part, then the Tibetan part, jump marks
        ;; both ways; the differences after the last Tibetan segment of
        ;; a group.
        (should (string-match-p (regexp-quote "<<skt-23>>⟦23⟧ · [[tib-78][Tib. 78–79]]\n| ") text))
        (should (string-match-p (regexp-quote "<<tib-77>>⟦77⟧ · [[skt-22][Skt. 22]]\n| ཅིའི་ཕྱིར") text))
        (should (< (string-match "^\\* Tibetan$" text) (string-match "^<<tib-77>>" text)))
        ;; (78 and 79 have neither words nor translation in this answer;
        ;; 79, the last of the group, still gets its mark for the
        ;; differences.)
        (should (string-match-p (regexp-quote "<<tib-79>>⟦79⟧ · [[skt-23][Skt. 23]]\n\n*Differences:* /udbhāvanatā/ is rendered /brjod pa/ („stating“).\n\n* Sentence") text))
        (should (string-match-p (regexp-quote "** Segment 23 · Tib. 78–79\n⟦23⟧ *Translation:* On account of") text))
        (should (string-match-p "^\\*\\* Tibetan rendering\nThe Tibetan states the self-basis" text))
        (should (string-match-p "^⟦22⟧ \\*Translation:\\* Why are there just five aggregates" text)))
      (should (= 1 (length done)))
      (should-not (bcat-sentence-fetch-state (tibetan-sentence--filepath 3 folder src) '(22 23))))))

(ert-deftest bcat-sentence-fetch-bilingual-rebuilds-sanskrit-file ()
  "A sentence file built from the Sanskrit template whose source then
switches to bilingual: C-c u G rebuilds it in the bilingual form (as
an old-layout file — notes kept) and runs both requests; left as it
was, the Tibetan answer would have had no place in it."
  (bcat-sentence-test--with-bil-source
    (with-temp-file src
      (insert (replace-regexp-in-string "#\\+CAT_TEMPLATE: bilingual" "#+CAT_TEMPLATE: sanskrit"
                                        bcat-sentence-test--bil-source)))
    (bcat-sentence-test--create-n src 3)
    (let ((file (tibetan-sentence--filepath 3 folder src)))
      (should (equal "sanskrit" (bcat-sentence-file-p file)))
      (bcat-sentence-test--set-notes file "Meine Notiz.")
      (with-temp-file src (insert bcat-sentence-test--bil-source))
      (bcat-sentence-test--with-stubs #'bcat-sentence-test--bil-answers
        (bcat-sentence-test--fetch-n src 3)
        (should (equal '("bcat-tibetan" "bcat-analysis") kinds))
        (should (equal "bilingual" (bcat-sentence-file-p file)))
        (let ((text (bcat-sentence-test--file-text-n src 3)))
          (should (string-match-p "^\\*\\* Segment 23 · Tib\\. 78–79$" text))
          (should (string-match-p "^\\* My Notes\nMeine Notiz\\.$" text)))))))

(ert-deftest bcat-sentence-fetch-bilingual-tibetan-unreadable ()
  "The Tibetan answer unreadable: the file keeps the Sanskrit analysis,
the answer is kept, no PDF — and C-c u G still has the sentence to do
\(both requests again: the Tibetan one needs the Sanskrit answer)."
  (bcat-sentence-test--with-bil-source
    (bcat-sentence-test--create-n src 3)
    (bcat-sentence-test--with-stubs (lambda (kind)
                                      (if (equal kind "bcat-tibetan")
                                          "Sorry, no JSON."
                                        bcat-sentence-test--bil-answer-sa))
      (bcat-sentence-test--fetch-n src 3)
      (let ((text (bcat-sentence-test--file-text-n src 3)))
        (should (string-match-p "^⟦22⟧ \\*Translation:\\* Why are there just five" text))
        (should-not (string-match-p "· Tib\\." text)))
      (should (null done))
      (should (directory-files bcat-analysis-response-directory nil "\\`sent-003"))
      (should (eq 'full (bcat-sentence-fetch-state (tibetan-sentence--filepath 3 folder src) '(22 23)))))))

;; ----------------------------------------------------------------------------
;; The pair — both files from one C-c u G, linked (Carsten 30.10.)
;; ----------------------------------------------------------------------------

(defconst bcat-sentence-test--pair-tibetan
  (replace-regexp-in-string
   "#\\+CAT_TEMPLATE: tibetan\n"
   "#+CAT_TEMPLATE: bilingual-tibetan\n#+CAT_PARALLEL: ../Sanskrit/asam-cat.org\n#+TIBETAN_SENTENCE_REFS: refs-bo.org\n"
   bcat-sentence-test--bil-tibetan)
  "The Tibetan source as the partner of the bilingual Sanskrit one.")

(defmacro bcat-sentence-test--with-pair (&rest body)
  "Like `bcat-sentence-test--with-bil-source', the Tibetan source as its
partner (#+CAT_TEMPLATE: bilingual-tibetan, #+CAT_PARALLEL back) with
a reference of its own; TSRC is the Tibetan source, TFOLDER its
analysis/."
  (declare (indent 0))
  `(bcat-sentence-test--with-bil-source
     (let ((tsrc (expand-file-name "Tibetisch/kunbtus-cat.org" dir))
           (tfolder (file-name-as-directory (expand-file-name "Tibetisch/analysis" dir))))
       (with-temp-file tsrc (insert bcat-sentence-test--pair-tibetan))
       (with-temp-file (expand-file-name "Tibetisch/refs-bo.org" dir)
         (insert "#+TITLE: Rahula/Boin-Webb\n\n** Sentence 3\n*** Rahula/Boin-Webb 2001\n"
                 "Why are there only five aggregates? Because of the five methods.\n"))
       (ignore tfolder)
       ,@body)))

(defconst bcat-sentence-test--pair-answer-bo
  (concat
   "{\"segments\": ["
   "{\"num\": 77, \"sanskrit\": 22, \"words\": [{\"wylie\": \"ci'i phyir\", \"gloss\": \"why\", \"function\": \"INT\"},"
   " {\"wylie\": \"phung po\", \"gloss\": \"aggregate\", \"function\": \"N\"}, {\"wylie\": \"rnams\", \"gloss\": \"plural\", \"function\": \"PL\"},"
   " {\"wylie\": \"lnga\", \"gloss\": \"five\", \"function\": \"NUM\"}, {\"wylie\": \"kho na\", \"gloss\": \"only\", \"function\": \"ADV\"},"
   " {\"wylie\": \"zhe na\", \"gloss\": \"if one asks\", \"function\": \"QUOT\"}],"
   " \"translation\": \"Why are the aggregates only five? — if one asks:\","
   " \"references\": [{\"name\": \"Rahula/Boin-Webb 2001\", \"text\": \"Why are there only five aggregates?\"},"
   "                  {\"name\": \"DharmaMitra\", \"text\": \"Inexpressible and invisible,\"}],"
   " \"shift\": \"\", \"structure\": \"the question\", \"note\": \"\", \"concepts\": \"\"},"
   "{\"num\": 78, \"sanskrit\": 23, \"words\": [], \"translation\": \"because they state the basis of self in five modes:\","
   " \"references\": [{\"name\": \"Rahula/Boin-Webb 2001\", \"text\": \"Because of the five methods.\"},"
   "                  {\"name\": \"DharmaMitra\", \"text\": \"Unchanging and unconditioned—\"}],"
   " \"shift\": \"\", \"structure\": \"\", \"note\": \"\", \"concepts\": \"\"},"
   "{\"num\": 79, \"sanskrit\": 23, \"words\": [], \"translation\": \"the body with what it appropriated,\","
   " \"references\": [], \"shift\": \"\", \"structure\": \"\", \"note\": \"\", \"concepts\": \"\"}],"
   " \"differences\": [{\"sanskrit\": 23, \"text\": \"/udbhāvanatā/ is rendered /brjod pa/ („stating“).\"}],"
   " \"grammar\": \"Question and causal answer.\","
   " \"translation\": \"Why are the aggregates only five? — because they state the basis of self.\","
   " \"comparison\": \"\","
   " \"rendering\": \"The Tibetan states the self-basis rather than bringing it to light.\"}")
  "The Tibetan answer of the pair test: the full Tibetan analysis with
the assignment 77 → 22, 78–79 → 23.")

(defun bcat-sentence-test--pair-answers (kind)
  "The canned answer of the pair test for the request KIND."
  (if (equal kind "bcat-tibetan") bcat-sentence-test--pair-answer-bo bcat-sentence-test--bil-answer-sa))

(defun bcat-sentence-test--text-of (file)
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(ert-deftest bcat-sentence-create-pair-links ()
  "Carsten 30.10.: „mit dem Link kann ich aus dem Sanskrit auf denselben
Satz im Tibetischen springen“.  In a pair each sentence file links the
other's sentence file in its header; C-c u A on the Tibetan source
builds its file from the bilingual-tibetan template — the Sanskrit of
the sentence as Devanāgarī over IAST under * Sanskrit, after the
Tibetan part (1.11.)."
  (bcat-sentence-test--with-pair
    (let ((text (bcat-sentence-test--create-n src 3)))
      (should (string-match-p (regexp-quote "#+CAT_PARALLEL: [[file:../../Tibetisch/analysis/sent-003-kunbtus.org][sent-003-kunbtus.org]]") text)))
    (let ((text (bcat-sentence-test--create-n tsrc 3)))
      (should (string-match-p "^#\\+CAT_TEMPLATE: bilingual-tibetan$" text))
      (should (string-match-p (regexp-quote "#+CAT_PARALLEL: [[file:../../Sanskrit/analysis/sent-003-asam.org][sent-003-asam.org]]") text))
      (should (string-match-p "^#\\+CAT_PARALLEL_SEGMENTS: 22, 23$" text))
      ;; 1.11.: the Tibetan part first, then the Sanskrit part.
      (should (string-match-p (regexp-quote "* Sanskrit\n#+begin_verse\nकस्मात् पञ्चैव स्कन्धाः।\nkasmāt pañcaiva skandhāḥ.\n") text))
      (should (string-match-p "^\\*\\* Segment 77$" text)))))

(ert-deftest bcat-sentence-fetch-pair ()
  "C-c u G on a sentence of the pair: the Sanskrit request, then the
Tibetan one with the whole Tibetan analysis — its references are the
Tibetan source's and DharmaMitra on the Tibetan.  Its landing writes
BOTH files: the bilingual Sanskrit file and the Tibetan file (full
analysis, per segment its Sanskrit segment, the Sanskrit lines and the
differences at the first of a group); notes in the Tibetan file stay.
The PDF is made of the Sanskrit file only („nur die zweisprachige“)."
  (bcat-sentence-test--with-pair
    (bcat-sentence-test--create-n src 3)
    (let ((tfile (tibetan-sentence--filepath 3 tfolder tsrc))
          (file (tibetan-sentence--filepath 3 folder src)))
      (bcat-sentence-test--create-n tsrc 3)
      (bcat-sentence-test--set-notes tfile "Tibetische Notiz.")
      (bcat-sentence-test--with-stubs #'bcat-sentence-test--pair-answers
        (bcat-sentence-test--fetch-n src 3)
        (should (equal '("bcat-tibetan" "bcat-analysis") kinds))
        (let ((sys (car (car prompts))) (user (cdr (car prompts))))
          (should (string-match-p "\"concepts\"" sys))
          (should (string-match-p (regexp-quote "Reference translations:\n=== Rahula/Boin-Webb 2001 ===\nWhy are there only five aggregates? Because of the five methods.\n=== DharmaMitra ===\nInexpressible and invisible") user)))
        (should (equal (list file) done))
        (let ((text (bcat-sentence-test--text-of file)))
          (should (string-match-p "^\\*\\* Segment 23 · Tib\\. 78–79$" text)))
        (let ((text (bcat-sentence-test--text-of tfile)))
          (should (equal "bilingual-tibetan" (bcat-sentence-file-p tfile)))
          (should (string-match-p "^\\*\\* Segment 77 · Skt\\. 22$" text))
          (should (string-match-p "^\\*\\* Segment 79 · Skt\\. 23$" text))
          ;; 1.11., mirrored: both originals at the top, per Tibetan
          ;; segment its table and translation, after the last of a
          ;; group the Sanskrit segment and the differences.
          (should (string-match-p (regexp-quote "#+begin_verse\nकस्मात् पञ्चैव स्कन्धाः।\nkasmāt pañcaiva skandhāḥ.\n") text))
          ;; (78 and 79 have no words in this answer: no table, so no
          ;; mark — their translations, then the group's differences.)
          (should (string-match-p (regexp-quote "→ the body with what it appropriated,\n\n*Differences:* /udbhāvanatā/") text))
          (should (string-match-p (regexp-quote "<<skt-23>>⟦23⟧ · [[tib-78][Tib. 78–79]]\n| pañca-ākāra-ātma-vastu-udbhāvanatām") text))
          (should (< (string-match "^→ the body with what it appropriated,$" text) (string-match "^\\*Differences:\\*" text)))
          (should (string-match-p (regexp-quote "*Differences:* /udbhāvanatā/ is rendered /brjod pa/ („stating“).\n\n*Translation:*") text))
          (should (string-match-p (regexp-quote "** Segment 78 · Skt. 23\n⟦78⟧ *Translation:* because they state") text))
          (should (= 1 (with-temp-buffer (insert text) (how-many "^\\*Differences:\\*" (point-min) (point-max)))))
          (should (string-match-p "^⟦77⟧ \\*Translation:\\* Why are the aggregates only five" text))
          (should (string-match-p "^\\*\\* Tibetan rendering\nThe Tibetan states" text))
          (should (string-match-p "^\\* My Notes\nTibetische Notiz\\.$" text)))
        (should-not (bcat-sentence-fetch-state tfile '(77 78 79)))
        (should-not (bcat-sentence-fetch-state file '(22 23)))))))

(ert-deftest bcat-sentence-fetch-pair-from-tibetan ()
  "C-c u G in the Tibetan source of a pair goes through the Sanskrit:
the same two requests, both files, the PDF of the Sanskrit file."
  (bcat-sentence-test--with-pair
    (bcat-sentence-test--with-stubs #'bcat-sentence-test--pair-answers
      (bcat-sentence-test--fetch-n tsrc 3)
      (should (equal '("bcat-tibetan" "bcat-analysis") kinds))
      (should (equal (list (tibetan-sentence--filepath 3 folder src)) done))
      (should (string-match-p "^\\*\\* Segment 77 · Skt\\. 22$"
                              (bcat-sentence-test--text-of (tibetan-sentence--filepath 3 tfolder tsrc)))))))

(ert-deftest bcat-sentence-fetch-pair-completes-partner ()
  "A complete Sanskrit file whose Tibetan partner is missing or not yet
analysed: the sentence is still to do — both requests again (the
Tibetan one needs the Sanskrit answer)."
  (bcat-sentence-test--with-pair
    (bcat-sentence-test--with-stubs #'bcat-sentence-test--pair-answers
      (bcat-sentence-test--fetch-n src 3))
    (let ((tfile (tibetan-sentence--filepath 3 tfolder tsrc)))
      (delete-file tfile)
      (bcat-sentence-test--with-stubs #'bcat-sentence-test--pair-answers
        (bcat-sentence-test--fetch-n src 3)
        (should (= 2 submits))
        (should (file-exists-p tfile))))))

(ert-deftest bcat-sentence-force-question-counts-pair-requests ()
  "C-u C-c u G asks before analysing anew and names the requests: for a
sentence of the pair two DharmaMitra and two Claude requests (Sanskrit,
Tibetan), not „one each“."
  (bcat-sentence-test--with-pair
    (let ((questions '()))
      (bcat-sentence-test--with-stubs #'bcat-sentence-test--pair-answers
        (cl-letf (((symbol-function 'y-or-n-p) (lambda (q) (push q questions) nil)))
          (tibetan-cascade-fetch (list :source src :sent-nums '(3) :label "Satz 3") t)))
      (should (string-match-p "je zwei DharmaMitra- und zwei Claude-Abrufe (Sanskrit, Tibetisch)"
                              (car questions))))))

(provide 'bcat-sentence-test)
;;; bcat-sentence-test.el ends here
