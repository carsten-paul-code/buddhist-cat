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
  "C-c u A (no AI): the verse under * Text already carries the
dictionary glosses in brackets — the same units as the tables
(particles their label, unknown words none)."
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
      ;; Carsten 29.10.: first the plain verse, then the one with the
      ;; vocabulary; no translation line before C-c u G.
      (should (string-match-p
               (regexp-quote (concat "* Text\n#+begin_verse\n"
                                     "brjod du med pa bltar med pa/ /\n"
                                     "'gyur ba med pa 'dus ma byas/ /\n"
                                     "#+end_verse\n\n#+begin_verse\n"
                                     "brjod du med pa [inexpressible; ineffable] bltar med pa [invisible] / /\n"
                                     "'gyur ba med pa [immutable] 'dus [collect; assemble…] ma byas / /\n"
                                     "#+end_verse\n\n* Segments"))
               (bcat-sentence-test--create src))))))

(ert-deftest bcat-sentence-create-text-without-glosses ()
  "No word known to the dictionaries: the plain verse only — the same
lines twice would be noise."
  (bcat-sentence-test--with-source
    (cl-letf (((symbol-function 'tibetan-gloss-table--unit-rows) #'ignore))
      (should (string-match-p
               (regexp-quote (concat "* Text\n#+begin_verse\n"
                                     "brjod du med pa bltar med pa/ /\n"
                                     "'gyur ba med pa 'dus ma byas/ /\n"
                                     "#+end_verse\n\n* Segments"))
               (bcat-sentence-test--create src))))))

(ert-deftest bcat-sentence-create-from-template ()
  "C-c u A on a Tibetan cascade source writes the template form: marker
#+CAT_TEMPLATE: tibetan, * Text with the Wylie per segment (verse
lines), * Segments with a dictionary table per segment, * Sentence with
the reference translation under Translations compared, the three user
slots — and none of the old sections.  No AI request is made."
  (bcat-sentence-test--with-source
    (let ((fired nil))
      (cl-letf (((symbol-function 'tibetan-sentence-claude--request)
                 (lambda (&rest _) (setq fired t))))
        (let ((text (bcat-sentence-test--create src)))
          (should (string-match-p "^#\\+CAT_TEMPLATE: tibetan$" text))
          (should (string-match-p "^#\\+SEGMENTS: 38, 39$" text))
          ;; One verse line per pāda (with the dictionary glosses in
          ;; brackets where known — exact form: create-glossed-text).
          (should (string-match-p
                   "#\\+begin_verse\nbrjod du med pa .*/ /\n'gyur ba med pa .*/ /\n#\\+end_verse"
                   text))
          (should (string-match-p "^\\* Segments\n\\*\\* Segment 38\n| " text))
          (should (string-match-p "^\\*\\* Segment 39\n| " text))
          (should (string-match-p "steinert:" text))
          (should (string-match-p
                   (regexp-quote "** Translations compared\n*Lindtner 1981:* [It is also] inexpressible (avācya) and invisible (adraṣṭavya).")
                   text))
          (dolist (h '("* Working Translation" "* My Notes" "* Footnotes"))
            (should (string-match-p (concat "^" (regexp-quote h) "$") text)))
          ;; Nothing of the old layout, no AI-only lines yet.
          (dolist (old '("^\\* Reading" "^\\* Tibetan Analysis" "^\\* Provided Translations"
                         "Claude" "Partikelkarte" "\\[Requesting" "\\[Awaiting"
                         "^\\*Structure:\\*" "^\\*Note:\\*" "^⟦"))
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
run at once, and the Claude submit landing ANSWER immediately.  Binds
PROMPTS (the (SYSTEM . USER) pairs sent), SUBMITS, DONE (files the
after-fetch hook saw) and COMPANIONS (old companion hook calls)."
  (declare (indent 1))
  `(let ((prompts '()) (submits 0) (done '()) (companions 0)
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
                (lambda (_kind file thunk land)
                  (cl-incf submits)
                  (push (funcall thunk) prompts)
                  (funcall land file ,answer)
                  t)))
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
          (should (string-match-p "^\\*Concepts:\\* \\*'dus ma byas\\* (asaṃskṛta)$" text))
          (should (string-match-p "^\\*\\* Grammar\nTwo predicate pādas\\.$" text))
          (should (string-match-p "^\\*Translation:\\* Indescribable and invisible, immutable" text))
          (should (string-match-p "^\\* My Notes\nMeine Notiz zu Vers 9\\.$" text)))
        (should (equal (list (tibetan-sentence--filepath 9 folder src)) done))
        (should (= 0 companions))))))

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
    (should (string-match-p
             (regexp-quote (concat "*Lindtner 1981:* [It is also] inexpressible (avācya) and "
                                   "invisible (adraṣṭavya). [fn:l9]\n\n"
                                   "[fn:l9] Lindtner 1981, S. 197: Eimer's \"Befleckungen\" is wrong."))
             (bcat-sentence-test--file-text src)))))

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
        (goto-char (point-min))
        (re-search-forward "^\\* Working Translation")
        (beginning-of-line)
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

(defun bcat-sentence-test--add-reference (file name text)
  "Add the paragraph *NAME:* TEXT under Translations compared of FILE."
  (with-temp-buffer
    (insert-file-contents file)
    (goto-char (point-min))
    (re-search-forward "^\\* Working Translation")
    (beginning-of-line)
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

(provide 'bcat-sentence-test)
;;; bcat-sentence-test.el ends here
