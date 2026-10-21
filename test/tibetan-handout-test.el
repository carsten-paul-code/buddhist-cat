;;; tibetan-handout-test.el --- Tests for the reading-class handout -*- lexical-binding: t -*-

;;; Commentary:
;; §5.59 (2026-10-09): printable handout per § / sentence of a
;; cascade source — Wylie (+ Uchen), translation, vocabulary (★
;; wordlist first), grammar (filtered labels).  All fixtures in temp
;; dirs — never the live corpus; no network, no LaTeX run except the
;; one skip-unless smoke test.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((base-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../persist" base-dir))
  (add-to-list 'load-path (expand-file-name "../core" base-dir))
  (add-to-list 'load-path (expand-file-name "../analysis" base-dir)))

(require 'tibetan-handout)
(require 'tibetan-sentence-persist)

;; ----------------------------------------------------------------------------
;; Fixtures
;; ----------------------------------------------------------------------------

(defconst tibetan-handout-test--bo-source
  (concat
   "#+TITLE: Klu sgrub dgongs rgyan — CAT-Quelle (B2)\n"
   "#+TIBETAN_LAYOUT: cascade\n\n"
   "* Tibetan Text\n"
   "** Section §220\n"
   ":PROPERTIES:\n:LOPEZ_SECTION: 220\n:END:\n\n"
   "*** Sentence 654\n"
   "**** Segment 1970\nབློས་མཐོང་བ་དང་།\n\n"
   "**** Segment 1971\nཀུན་རྫོབ་ཀྱི་བདེན་པ།\n\n"
   "*** Sentence 655\n"
   "**** Segment 1975\nབཅོས་མར་གྱུར་པའི་དངོས་ནི།\n\n"
   "** Section §221\n"
   ":PROPERTIES:\n:LOPEZ_SECTION: 221\n:END:\n\n"
   "*** Sentence 656\n"
   "**** Segment 1979\nཚད་མ་དང་རིགས་པ།\n\n"))

(defconst tibetan-handout-test--bo-sent-654
  (concat
   "#+TITLE: Sentence 654 Analysis\n"
   "#+TIBETAN_LAYOUT: cascade\n"
   "#+SOURCE: [[file:../Rgyan-cat.org::*Sentence 654][Rgyan-cat.org / Sentence 654]]\n"
   "#+SEGMENTS: 1970, 1971\n\n"
   "* Translation\n"
   "Ganzer Satz deutsch.\n\n"
   "* Reading\n** Gloss Tables\n"
   "*** Segment 1970\n| blos |\n- ⟦1970⟧ Durch den Verstand gesehen,\n\n"
   "*** Segment 1971\n| kun rdzob |\n- ⟦1971⟧ die scheinbare Wahrheit.\n\n"
   "* Tibetan Analysis\n"
   "** Claude Vocabulary\n"
   "*** Segment 1970\n"
   "blos, ergative/instrumental, \"durch den Verstand\", instrumental of *blo*; Skt. *buddhi*\n"
   "dang, particle, \"und\", coordinating\n\n"
   "*** Segment 1971\n"
   "kun rdzob, noun, \"scheinbar\", Skt. *saṃvṛti*; concealer\n\n"
   "** Claude Grammar\n"
   "**Cross-clause overview:** OVERVIEW TEXT.\n\n"
   "*** Segment 1970\n"
   "- *Verb backbone:* mthong is the verb.\n"
   "- *Case frame:* CASEFRAME TEXT.\n"
   "- *Notable constructions:* blos instrumental,\n"
   "  continued on a second line.\n"
   "- *Translation justifications:* JUSTIFICATION TEXT.\n\n"
   "*** Segment 1971\n"
   "- *Case frame:* only case frame here.\n\n"
   "** Concept Notes\n- **kun rdzob** — note.\n\n"
   "* Provided Translations\n"
   "** DharmaMitra Translation\nDM POISON TEXT\n\n"
   "** Lopez 2006\nLOPEZ POISON TEXT\n\n"
   "** Discussion\n:PROPERTIES:\n:GENERATED: 2026-10-12\n:END:\n"
   "Lopez glättet; ASSESSMENT TEXT.\n\n"
   "*** Lopez 2006 — markiert\nLOPEZ POISON TEXT *marked⟨1⟩*\n\n"
   "*** 1. blos\n"
   "- Text :: blos\n"
   "- Lopez 2006 :: “by the mind”\n"
   "- Claude :: “durch den Verstand”\n"
   "- DharmaMitra :: “mit dem Geist”\n"
   "- Problem :: Claude's instrumental vs. Lopez' agentive reading.\n"
   "- Einschätzung :: EINSCHAETZUNG TEXT.\n"
   "- Frage :: FRAGE TEXT?\n\n"
   "* Working Translation\nWT POISON TEXT\n\n"
   "* My Notes\n\n* Footnotes\n"))

(defconst tibetan-handout-test--bo-wordlist
  (concat "| Term | Bedeutung |\n|------+-----------|\n"
          "| blo | Verstand // mind / awareness |\n"
          "| kun rdzob | scheinbar / scheinbare Wahrheit // conventional (truth) |\n"
          "| yin | sein // to be |\n"
          "| na | POISON-NA |\n"))

(defconst tibetan-handout-test--sa-source
  (concat
   "#+TITLE: MAv VI.28\n"
   "#+TIBETAN_LAYOUT: cascade\n"
   "#+SOURCE_LANG: sa\n\n"
   "* Tibetan Text\n"
   "** Section MAv VI.28\n\n"
   "*** Sentence 1\n"
   "**** Segment 1\nmohaḥ svabhāvāvaraṇād dhi saṃvṛtiḥ |\n\n"))

(defconst tibetan-handout-test--sa-sent-1
  (concat
   "#+TITLE: Sentence 1 Analysis\n"
   "#+TIBETAN_LAYOUT: cascade\n"
   "#+SOURCE: [[file:../Mav-sa.org::*Sentence 1][Mav-sa.org / Sentence 1]]\n\n"
   "* Translation\nDer moha ist saṃvṛti.\n\n"
   "* Reading\n** Gloss Tables\n*** Segment 1\n| mohaḥ |\n"
   "- ⟦1⟧ Der moha ist saṃvṛti.\n\n"
   "* Tibetan Analysis\n"
   "** Claude Vocabulary\n*** Segment 1\n"
   "mohaḥ, noun (m.), \"Verblendung\", root affliction\n"
   "na, indeclinable, \"nicht\", negation\n\n"
   "** Claude Grammar\n*** Segment 1\n"
   "- *Syntax:* nominal sentence.\n\n"
   "** Concept Notes\n- note\n\n"
   "** Word Analysis\n*** Segment 1\nmohaḥ na\n\n"
   "- mohaḥ — moha; N.NOM.SG.M\n- na — na; IND.\n\n"
   "* Provided Translations\n\n* Working Translation\n\n"
   "* My Notes\n\n* Footnotes\n"))

(defmacro tibetan-handout-test--with-corpus (&rest body)
  "Temp corpus: bo source Rgyan-cat.org (§220: Sentences 654, 655;
§221: 656) with Resources wordlist and the sent file of 654 only;
sa source Mav-sa.org with Resources (poison `na') and its sent file.
Binds DIR, SRC, SA-SRC, FOLDER, SA-FOLDER."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "handout-" t))
          (src (expand-file-name "Rgyan-cat.org" dir))
          (folder (file-name-as-directory (expand-file-name "analysis" dir)))
          (sa-dir (expand-file-name "sa" dir))
          (sa-src (expand-file-name "Mav-sa.org" sa-dir))
          (sa-folder (file-name-as-directory
                      (expand-file-name "analysis" sa-dir))))
     (unwind-protect
         (progn
           (make-directory folder t)
           (make-directory (expand-file-name "Resources" dir) t)
           (make-directory sa-folder t)
           (make-directory (expand-file-name "Resources" sa-dir) t)
           (with-temp-file src (insert tibetan-handout-test--bo-source))
           (with-temp-file (expand-file-name "Resources/Wortliste-test.org" dir)
             (insert tibetan-handout-test--bo-wordlist))
           (with-temp-file (tibetan-sentence--filepath 654 folder src)
             (insert tibetan-handout-test--bo-sent-654))
           (with-temp-file sa-src (insert tibetan-handout-test--sa-source))
           (with-temp-file (expand-file-name "Resources/Wortliste-sa.org" sa-dir)
             (insert tibetan-handout-test--bo-wordlist))
           (with-temp-file (tibetan-sentence--filepath 1 sa-folder sa-src)
             (insert tibetan-handout-test--sa-sent-1))
           ,@body)
       (delete-directory dir t))))

(defun tibetan-handout-test--curated (&rest pairs)
  "Hash of curated wordlist PAIRS (KEY VALUE …)."
  (let ((h (make-hash-table :test 'equal)))
    (while pairs
      (puthash (pop pairs) (pop pairs) h))
    h))

;; ----------------------------------------------------------------------------
;; B1 — data layer
;; ----------------------------------------------------------------------------

(ert-deftest tibetan-handout-vocab-fields ()
  "The comma-format vocabulary line splits into term / part of speech
/ gloss; only the Skt. equivalent survives from the note.  A stray
leading bullet (C6a model drift) is tolerated."
  (let ((f (tibetan-handout--vocab-fields
            "blos, ergative/instrumental, \"durch den Verstand\", instrumental of *blo*; Skt. *buddhi*")))
    (should (equal "blos" (plist-get f :term)))
    (should (equal "ergative/instrumental" (plist-get f :pos)))
    (should (equal "durch den Verstand" (plist-get f :gloss)))
    (should (equal "buddhi" (plist-get f :skt))))
  (let ((f (tibetan-handout--vocab-fields "- ste, particle, \"—\", converb")))
    (should (equal "ste" (plist-get f :term)))
    (should (equal "—" (plist-get f :gloss)))
    (should-not (plist-get f :skt)))
  ;; No quoted gloss: third comma field is the gloss.
  (should (equal "Wahrheit"
                 (plist-get (tibetan-handout--vocab-fields
                             "bden pa, noun, Wahrheit")
                            :gloss)))
  (should-not (tibetan-handout--vocab-fields ""))
  (should-not (tibetan-handout--vocab-fields "---")))

(ert-deftest tibetan-handout-curated-gloss-pos-gated ()
  "★ lookup: exact key, a trailing particle SYLLABLE, or a merged
clitic — the latter ONLY when Claude's part of speech licenses it
\(blos ergative → blo; rgyas as a verb must NOT become rgya)."
  (let ((tibetan-current-resources-vocab
         (tibetan-handout-test--curated
          "blo" "Verstand // mind / awareness"
          "kun rdzob" "scheinbar // conventional"
          "rgya" "weit // vast"
          "yin" "sein // to be"
          "'di" "dies // this"))
        (tibetan-current-custom-vocab nil))
    (should (equal "scheinbar // conventional"
                   (tibetan-handout--curated-gloss "kun rdzob" "noun")))
    (should (equal "Verstand // mind / awareness"
                   (tibetan-handout--curated-gloss
                    "blos" "ergative/instrumental")))
    (should-not (tibetan-handout--curated-gloss "rgyas" "verb"))
    (should (equal "sein // to be"
                   (tibetan-handout--curated-gloss "yin pas"
                                                   "copula + converb")))
    (should (equal "dies // this"
                   (tibetan-handout--curated-gloss
                    "'di'i" "demonstrative pronoun + genitive")))
    (should-not (tibetan-handout--curated-gloss "'di'i" "noun"))
    (should-not (tibetan-handout--curated-gloss "gti mug" "noun"))))

(ert-deftest tibetan-handout-context-differs-p ()
  "The context gloss is shown only when its alternatives are not
already among the ★ entry's alternatives (either language half)."
  (should-not (tibetan-handout--context-differs-p
               "Verstand" "Verstand // mind / awareness"))
  (should-not (tibetan-handout--context-differs-p
               "scheinbar" "scheinbar / scheinbare Wahrheit // conventional (truth)"))
  (should (tibetan-handout--context-differs-p
           "durch den Verstand" "Verstand // mind / awareness"))
  (should (tibetan-handout--context-differs-p
           "Schein / scheinbar" "scheinbar / scheinbare Wahrheit // conventional (truth)")))

(ert-deftest tibetan-handout-grammar-bullets-filtered ()
  "Only the configured labels survive; continuation lines join their
bullet; a segment carrying none of the labels keeps ALL bullets (the
filter never empties a segment)."
  (let ((b (tibetan-handout--grammar-bullets
            (concat "- *Verb backbone:* mthong is the verb.\n"
                    "- *Case frame:* CASEFRAME.\n"
                    "- *Notable constructions:* blos instrumental,\n"
                    "  continued on a second line.\n"
                    "- *Translation justifications:* JUST."))))
    (should (equal '(("Verb backbone" . "mthong is the verb.")
                     ("Notable constructions"
                      . "blos instrumental, continued on a second line."))
                   b)))
  (should (equal '(("Case frame" . "only case frame here."))
                 (tibetan-handout--grammar-bullets
                  "- *Case frame:* only case frame here.")))
  ;; Free prose (no labelled bullets) survives unlabelled.
  (should (equal '((nil . "A nominal sentence."))
                 (tibetan-handout--grammar-bullets "A nominal sentence.")))
  (should-not (tibetan-handout--grammar-bullets nil)))

(ert-deftest tibetan-handout-load-curated-from-source-resources ()
  "The ★ source is the Resources folder BESIDE THE SOURCE, loaded
without leaking into the global vocabulary state (§5.34 lesson:
locate assets from the source path, never from the caller's
buffer)."
  (tibetan-handout-test--with-corpus
    (let ((before tibetan-current-resources-vocab)
          (h (tibetan-handout--load-curated src)))
      (should (hash-table-p h))
      (should (equal "Verstand // mind / awareness" (gethash "blo" h)))
      (should (eq before tibetan-current-resources-vocab)))))

(ert-deftest tibetan-handout-sentence-data-bo ()
  "Full bo sentence: segments with Wylie + Uchen, translation by
segment from the ⟦N⟧ lines, vocabulary per segment with ★ and the
context gloss only where it differs, grammar filtered (overview
dropped, 1971 falls back to its only bullet)."
  (tibetan-handout-test--with-corpus
    (let* ((tibetan-current-resources-vocab
            (tibetan-handout--load-curated src))
           (tibetan-current-custom-vocab nil)
           (d (tibetan-handout--sentence-data 654 src))
           (segs (plist-get d :segs)))
      (should (equal "bo" (plist-get d :lang)))
      (should-not (plist-get d :missing))
      (should (equal '(1970 1971) (mapcar (lambda (s) (plist-get s :num)) segs)))
      (should (string-match-p "\\`blos mthong ba dang"
                              (plist-get (car segs) :wylie)))
      (should (string-match-p "བློས" (plist-get (car segs) :uchen)))
      (should (equal '((1970 . "Durch den Verstand gesehen,")
                       (1971 . "die scheinbare Wahrheit."))
                     (plist-get (plist-get d :translation) :by-seg)))
      (let* ((v1970 (cdr (assq 1970 (plist-get d :vocab))))
             (blos (car v1970))
             (dang (cadr v1970))
             (kun (car (cdr (assq 1971 (plist-get d :vocab))))))
        (should (plist-get blos :star))
        (should (equal "Verstand // mind / awareness" (plist-get blos :gloss)))
        (should (equal "durch den Verstand" (plist-get blos :context)))
        (should (string-match-p "Skt\\. buddhi" (plist-get blos :info)))
        (should-not (plist-get dang :star))
        (should (equal "und" (plist-get dang :gloss)))
        (should (plist-get kun :star))
        (should-not (plist-get kun :context)))
      (let ((g (plist-get d :grammar)))
        (should-not (assq nil g))
        (should (equal '("Verb backbone" "Notable constructions")
                       (mapcar #'car (cdr (assq 1970 g)))))
        (should (equal '("Case frame") (mapcar #'car (cdr (assq 1971 g)))))))))

(ert-deftest tibetan-handout-sentence-data-gaps ()
  "A sentence without analysis file is flagged :missing (visible gap,
never an error); incomplete renderings fall back to the whole
Translation with the machine label stripped; a placeholder
Translation yields no translation at all."
  (tibetan-handout-test--with-corpus
    (let ((d (tibetan-handout--sentence-data 655 src)))
      (should (plist-get d :missing))
      (should (equal '(1975) (mapcar (lambda (s) (plist-get s :num))
                                     (plist-get d :segs)))))
    (let ((file (tibetan-sentence--filepath 654 folder src)))
      (with-temp-file file
        (insert (replace-regexp-in-string
                 "- ⟦1971⟧ die scheinbare Wahrheit\\."
                 "- ⟦1971⟧ [Awaiting Claude — span]"
                 (replace-regexp-in-string
                  "Ganzer Satz deutsch\\."
                  "(Sentence 654 — §220 chunk)\nGanzer Satz deutsch."
                  tibetan-handout-test--bo-sent-654))))
      (should (equal '(:whole "Ganzer Satz deutsch.")
                     (plist-get (tibetan-handout--sentence-data 654 src)
                                :translation)))
      (with-temp-file file
        (insert (replace-regexp-in-string
                 "- ⟦1971⟧ die scheinbare Wahrheit\\."
                 "- ⟦1971⟧ [Awaiting Claude — span]"
                 (replace-regexp-in-string
                  "Ganzer Satz deutsch\\."
                  "[Requesting translation...]"
                  tibetan-handout-test--bo-sent-654))))
      (should-not (plist-get (tibetan-handout--sentence-data 654 src)
                             :translation)))))

(ert-deftest tibetan-handout-sentence-data-sa ()
  "Sanskrit: no ★ lookup EVER (the wordlist is Wylie-keyed — `na'
would hit a Tibetan entry, §5.57 poison class), no Uchen line, the
info column carries the Word-Analysis morphology."
  (tibetan-handout-test--with-corpus
    (let* ((tibetan-current-resources-vocab
            (tibetan-handout--load-curated sa-src))
           (tibetan-current-custom-vocab nil)
           (d (tibetan-handout--sentence-data 1 sa-src))
           (v (cdr (assq 1 (plist-get d :vocab))))
           (na (cadr v)))
      (should (equal "sa" (plist-get d :lang)))
      (should-not (plist-get (car (plist-get d :segs)) :uchen))
      (should (string-match-p "\\`mohaḥ" (plist-get (car (plist-get d :segs))
                                                     :wylie)))
      (should-not (cl-some (lambda (e) (plist-get e :star)) v))
      (should (equal "nicht" (plist-get na :gloss)))
      (should (equal "moha; N.NOM.SG.M" (plist-get (car v) :info)))
      (should (equal '(("Syntax" . "nominal sentence."))
                     (cdr (assq 1 (plist-get d :grammar))))))))

;; ----------------------------------------------------------------------------
;; B2 — renderer (pure: data → org string)
;; ----------------------------------------------------------------------------

(defconst tibetan-handout-test--spec
  (list
   :title "Klu sgrub dgongs rgyan" :scope "§220" :source-name "Rgyan-cat.org"
   :date "2026-10-09"
   :sentences
   (list
    (list :sent-num 654 :lang "bo"
          :segs '((:num 1970 :wylie "blos mthong ba ~M_x" :uchen "བློས་མཐོང་བ།")
                  (:num 1971 :wylie "kun rdzob/" :uchen "ཀུན་རྫོབ།"))
          :translation '(:by-seg ((1970 . "Durch den Verstand gesehen,")
                                  (1971 . "die scheinbare Wahrheit.")))
          :vocab '((1970 (:term "blos" :gloss "Verstand // mind / awareness"
                          :star t :context "durch den Verstand, dadurch"
                          :info "ergative/instrumental; Skt. buddhi")
                         (:term "dang" :gloss "und | mit" :star nil
                          :context nil :info "particle"))
                   (1971 (:term "kun rdzob" :gloss "scheinbar" :star t
                          :context nil :info "noun")))
          :grammar '((1970 ("Verb backbone" . "*mthong* is the verb.")
                           ("Notable constructions" . "**blos** instrumental."))
                     (1971 (nil . "Free prose."))))
    (list :sent-num 655 :lang "bo" :missing t
          :segs '((:num 1975 :wylie "bcos mar" :uchen "བཅོས་མར།")))
    (list :sent-num 656 :lang "bo"
          :segs '((:num 1976 :wylie "zhes" :uchen "ཞེས།"))
          :translation '(:whole "Ganzer *Satz*.")
          :vocab nil :grammar nil)))
  "A hand-built handout spec: full sentence, missing file, gaps.")

(ert-deftest tibetan-handout-render-header ()
  "Kopf: GENERATED-Marker zuerst, LuaLaTeX, Tibetisch-Font aus der
defcustom, Kopfzeile Werk · Umfang · Sätze, NIRGENDS „Claude“."
  (let ((org (tibetan-handout-render tibetan-handout-test--spec)))
    (should (string-prefix-p tibetan-handout-generated-marker org))
    (should (string-match-p "^#\\+LATEX_COMPILER: lualatex$" org))
    (should (string-match-p (regexp-quote tibetan-handout-tibetan-font) org))
    (should (string-match-p "Klu sgrub dgongs rgyan} · §220 · Sätze 654–656"
                            org))
    (should (string-match-p "Stand 2026-10-09" org))
    (should-not (string-match-p "Claude" org))))

(ert-deftest tibetan-handout-header-tibetan-fallback-font ()
  "Bug (Carsten 20.10., Diskussionspapier bDen Satz 2): Tibetan Unicode
outside the Uchen line — discussion point titles, concept notes —
fell back to Libertinus (no Tibetan glyphs → boxes).  The main font
carries the Tibetan font as luaotfload fallback, so every Tibetan
character in the document renders; the explicit \\tibfont stays for
the Uchen line."
  (let ((h (tibetan-handout--header
            (list :title "T" :scope "§1" :source-name "x.org" :date "2026-10-20"
                  :sentences (list (list :sent-num 1 :segs nil))))))
    (should (string-match-p "luaotfload.add_fallback(\"tibfallback\"" h))
    (should (string-match-p (regexp-quote tibetan-handout-tibetan-font) h))
    (should (string-match-p "setmainfont{Libertinus Serif}\\[RawFeature={fallback=tibfallback}\\]" h))
    (should (string-match-p "newfontfamily\\\\tibfont" h))))

(ert-deftest tibetan-handout-render-text-part ()
  "`* Text': je Segment Uchen (in \\tibfont) über dem Wylie, die
Segmentnummer am Rand; Wylie LaTeX-sicher (~ _ escaped), in einem
Export-Block (org-Markup kann es nicht verbiegen)."
  (let ((org (tibetan-handout-render tibetan-handout-test--spec)))
    (should (string-match-p "^\\* Text$" org))
    (should (string-match-p "\\\\segno{1970}{\\\\tibfont[^}]*བློས་མཐོང་བ།}" org))
    (should (string-match-p (regexp-quote
                             "blos mthong ba \\textasciitilde{}M\\_x")
                            org))
    (should (string-match-p "#\\+BEGIN_EXPORT latex" org))
    ;; Der Text steht auf eigenen Seiten (neben den Klassentext legen).
    (should (string-match-p "}\\\\clearpage\n#\\+END_EXPORT" org))))

(ert-deftest tibetan-handout-render-sentence-blocks ()
  "Je Satz: Übersetzung mit Segmentnummern-Makro bzw. ganz bzw. Lücke,
Vokabular als Tabelle je Segment (★, Kontextglosse im ctx-Makro mit
escapten Kommas, `|' als \\vert{}), Grammatik als Beschreibungsliste
mit org-Kursiv statt Markdown-Sternen; fehlende Datei = sichtbare
Lücke."
  (let ((org (tibetan-handout-render tibetan-handout-test--spec)))
    (should (string-match-p "^\\* Satz 654 (Seg\\. 1970–1971)$" org))
    (should (string-match-p (regexp-quote
                             "{{{n(1970)}}}Durch den Verstand gesehen, {{{n(1971)}}}die scheinbare Wahrheit.")
                            org))
    (should (string-match-p
             (regexp-quote
              "| blos | ★ Verstand // mind / awareness {{{ctx(durch den Verstand\\, dadurch)}}} | ergative/instrumental; Skt. buddhi |")
             org))
    (should (string-match-p (regexp-quote "| dang | und \\vert{} mit | particle |")
                            org))
    (should (string-match-p (regexp-quote "{{{seg(1971)}}}") org))
    (should (string-match-p
             (regexp-quote "- Verb backbone :: /mthong/ is the verb.") org))
    (should (string-match-p
             (regexp-quote "- Notable constructions :: *blos* instrumental.")
             org))
    (should (string-match-p "^Free prose\\.$" org))
    ;; Satz 655: Analysedatei fehlt.
    (should (string-match-p "Analysedatei fehlt" org))
    ;; Satz 656: ganze Übersetzung, Lücken für Vokabular/Grammatik.
    (should (string-match-p "^Ganzer /Satz/\\.$" org))
    (should (string-match-p "{{{luecke(noch kein Vokabular)}}}" org))
    (should (string-match-p "{{{luecke(noch keine Grammatik)}}}" org))))

(ert-deftest tibetan-handout-render-parses-as-org ()
  "Das Ergebnis ist gültiges org: die Satz-Überschriften sind echte
Headlines, jede Vokabelzeile hat genau drei Zellen."
  (let ((org (tibetan-handout-render tibetan-handout-test--spec)))
    (with-temp-buffer
      (insert org)
      (org-mode)
      (let ((tree (org-element-parse-buffer)))
        (should (equal '("Text" "Satz 654 (Seg. 1970–1971)"
                         "Satz 655 (Seg. 1975)" "Satz 656 (Seg. 1976)")
                       (org-element-map tree 'headline
                         (lambda (h) (when (= 1 (org-element-property :level h))
                                       (org-element-property :raw-value h))))))
        (should (cl-every (lambda (n) (= n 3))
                          (org-element-map tree 'table-row
                            (lambda (r)
                              (when (eq 'standard (org-element-property :type r))
                                (length (org-element-contents r)))))))))))

(ert-deftest tibetan-handout-render-sanskrit-no-uchen ()
  "Sanskrit: keine Uchen-Zeile, das IAST steht direkt mit Nummer."
  (let ((org (tibetan-handout-render
              (list :title "MAv" :scope "MAv VI.28" :source-name "x.org"
                    :date "2026-10-09"
                    :sentences
                    (list (list :sent-num 1 :lang "sa"
                                :segs '((:num 1 :wylie "mohaḥ svabhāva" :uchen nil))
                                :translation nil :vocab nil :grammar nil))))))
    (should-not (string-match-p "\\\\tibfont[^a-z]*[ༀ-࿿]" org))
    (should (string-match-p "\\\\segno{1}mohaḥ svabhāva" org))))

;; ----------------------------------------------------------------------------
;; B3 — scope at point, file writing, command
;; ----------------------------------------------------------------------------

(defmacro tibetan-handout-test--in-source (&rest body)
  "Inside `--with-corpus': visit SRC (BUF current) for BODY."
  (declare (indent 0))
  `(let ((buf (find-file-noselect src)))
     (unwind-protect
         (with-current-buffer buf ,@body)
       (with-current-buffer buf (set-buffer-modified-p nil))
       (kill-buffer buf))))

(defun tibetan-handout-test--scope-at (regexp)
  "The handout scope with point on the line matching REGEXP."
  (goto-char (point-min))
  (re-search-forward regexp)
  (beginning-of-line)
  (tibetan-cascade--scope-at-point))

(ert-deftest tibetan-handout-scope-at-point ()
  "Umfang wie bei C-c u A (§5.59 A1): Segment → sein Satz; Satz → Satz;
Section → ihre Sätze (+ Lopez-§ fürs Label); `* Tibetan Text' →
alle; in einer sent-Analysedatei → dieser Satz."
  (tibetan-handout-test--with-corpus
    (tibetan-handout-test--in-source
      (let ((s (tibetan-handout-test--scope-at "^\\*\\* Section §220")))
        (should (equal '(654 655) (plist-get s :sent-nums)))
        (should (equal "§220" (plist-get s :label)))
        (should (equal 220 (plist-get s :lopez)))
        (should (equal src (plist-get s :source))))
      (should (equal '(655) (plist-get (tibetan-handout-test--scope-at
                                        "^\\*\\*\\* Sentence 655")
                                       :sent-nums)))
      (let ((s (tibetan-handout-test--scope-at "^\\*\\*\\*\\* Segment 1971")))
        (should (equal '(654) (plist-get s :sent-nums)))
        (should (equal "Satz 654" (plist-get s :label))))
      (should (equal '(654 655 656)
                     (plist-get (tibetan-handout-test--scope-at
                                 "^\\* Tibetan Text")
                                :sent-nums))))
    (let ((buf (find-file-noselect (tibetan-sentence--filepath 654 folder src))))
      (unwind-protect
          (with-current-buffer buf
            (let ((s (tibetan-cascade--scope-at-point)))
              (should (equal '(654) (plist-get s :sent-nums)))
              (should (equal (file-truename src)
                             (file-truename (plist-get s :source))))))
        (kill-buffer buf)))))

(ert-deftest tibetan-handout-discussion-block ()
  "§5.62: the sentence's `** Discussion' reaches the handout as
`** Diskussion' after the Grammatik — assessment, then per point the
title and the quotes with German labels; Claude's rendering is named
like its own handout section (/Übersetzung/), in labels AND prose.
The marked full-text reference never reaches it (build test: Lopez
poison, no \"Claude\")."
  (tibetan-handout-test--with-corpus
    (let* ((d (tibetan-handout--sentence-data 654 src))
           (org (tibetan-handout-render
                 (list :title "T" :scope "§220" :source-name "Rgyan-cat.org"
                       :date "2026-10-12" :sentences (list d)))))
      (should (plist-get d :discussion))
      (should (< (string-match "^\\*\\* Grammatik$" org)
                 (string-match "^\\*\\* Diskussion$" org)))
      (should (string-match-p "^Lopez glättet; ASSESSMENT TEXT\\.$" org))
      (should (string-match-p "^\\*1\\. blos\\*$" org))
      (should (string-match-p "^- Lopez 2006 :: “by the mind”$" org))
      (should (string-match-p "^- /Übersetzung/ :: “durch den Verstand”$" org))
      (should (string-match-p "^- DharmaMitra :: “mit dem Geist”$" org))
      (should (string-match-p
               "^- Problem :: /Übersetzung/ instrumental vs\\. Lopez' agentive" org))
      (should (string-match-p "^- Einschätzung :: EINSCHAETZUNG TEXT\\.$" org))
      (should (string-match-p "^- Frage :: FRAGE TEXT\\?$" org))
      (should-not (string-match-p "POISON\\|Claude\\|⟨" org))))
  ;; No discussion → no block (sa fixture has none).
  (tibetan-handout-test--with-corpus
    (let ((org (tibetan-handout-render
                (list :title "T" :scope "Satz 1" :source-name "Mav-sa.org"
                      :date "2026-10-12"
                      :sentences (list (tibetan-handout--sentence-data 1 sa-src))))))
      (should-not (string-match-p "Diskussion" org)))))

(ert-deftest tibetan-handout-discussion-point-title-markdown ()
  "Bug (20.10.2026, Tantrāloka V. 1 im Diskussionspapier): a point
title with markdown emphasis (`*mahā jananī* — \"great mother\"') was
wrapped in a second pair of `*' — org then shows the raw stars.  The
title goes through the markdown→org converter like every other
discussion text, so the result is bold with italics inside."
  (let ((org (tibetan-handout--discussion-block
              (list :assessment ""
                    :points (list (list :title "*mahā jananī* — \"great mother\" or \"Vimalā\""
                                        :items '(("Text" . "mahā jananī")
                                                 ("Issue" . "Claude reads it as a compound."))))))))
    (should (string-match-p (regexp-quote "*1. /mahā jananī/ — \"great mother\" or \"Vimalā\"*") org))
    (should-not (string-match-p (regexp-quote "*1. *") org))
    (should (string-match-p "^- Problem :: /Übersetzung/ reads" org))))

(defmacro tibetan-handout-test--with-sa-verse (&rest body)
  "A one-verse Sanskrit cascade source (Jayaratha's śloka, Satz 1002
of the Tantrāloka reading) in a temp dir; binds VSRC."
  (declare (indent 0))
  `(let* ((vdir (make-temp-file "handout-verse-" t))
          (vsrc (expand-file-name "Verse-sa.org" vdir)))
     (unwind-protect
         (progn
           (make-directory (expand-file-name "analysis" vdir) t)
           (with-temp-file vsrc
             (insert "#+TITLE: Verse\n#+TIBETAN_LAYOUT: cascade\n#+SOURCE_LANG: sa\n\n"
                     "* Tibetan Text\n** Section Einleitung\n"
                     "*** Sentence 1002\n"
                     "**** Segment 673\ndehe vimukta evāsmi śrīmatkalyāṇavāridheḥ /\n"
                     "**** Segment 674\nyasya kāruṇyavipruḍbhiḥ sadguruṃ taṃ hṛdi śraye // 2\n"))
           ,@body)
       (delete-directory vdir t))))

(ert-deftest tibetan-handout-metre-pauses ()
  "§5.65: a Sanskrit verse is printed with its recitation pauses
\(` | ' pāda end, ` ¦ ' yati) and a metre line above it; the sentence
block carries `** Metrum' with the syllable table — even before any
analysis exists (the metre needs none).  Tibetan stays unmarked."
  (tibetan-handout-test--with-sa-verse
    (let* ((d (tibetan-handout--sentence-data 1002 vsrc))
           (org (tibetan-handout-render
                 (list :title "T" :scope "Satz 1002" :source-name "Verse-sa.org"
                       :date "2026-10-21" :sentences (list d)))))
      (should (equal (plist-get (car (plist-get d :segs)) :wylie)
                     "dehe vimukta evāsmi | śrīmatkalyāṇavāridheḥ /"))
      (should (string-prefix-p "Śloka (Anuṣṭubh)" (plist-get d :metre)))
      (should (string-match-p "Śloka (Anuṣṭubh)" org))
      (should (string-match-p "evāsmi | śrīmatkalyāṇavāridheḥ" org))
      (should (string-match-p "^\\*\\* Metrum$" org))
      (should (string-match-p "^| a +| de +| he +|" org))))
  (tibetan-handout-test--with-corpus
    (let ((d (tibetan-handout--sentence-data 654 src)))
      (should-not (plist-get d :metre))
      (should-not (string-match-p "[¦|]" (plist-get (car (plist-get d :segs)) :wylie))))))

(ert-deftest tibetan-handout-build-writes-guarded-file ()
  "Build schreibt `handouts/<short>-par-220.org' neben die Quelle:
GENERATED-Marker, Werktitel aus #+TITLE, Lopez/W&M/DM/Working
Translation NIE (Poison-Lock wie §5.54), kein „Claude“.  Eine
vorhandene Datei OHNE Marker wird nie überschrieben; mit Marker
schon (Regenerieren)."
  (tibetan-handout-test--with-corpus
    (let* ((scope (list :source src :sent-nums '(654 655 656)
                        :label "§220" :lopez 220))
           (out (tibetan-handout-build scope "2026-10-09"))
           (text (with-temp-buffer (insert-file-contents out) (buffer-string))))
      (should (equal (expand-file-name "handouts/rgyan-par-220.org" dir) out))
      (should (string-prefix-p tibetan-handout-generated-marker text))
      (should (string-match-p "Klu sgrub dgongs rgyan} · §220" text))
      (dolist (poison '("LOPEZ POISON" "DM POISON" "WT POISON" "Claude"))
        (should-not (string-match-p poison text)))
      ;; Regenerieren überschreibt die generierte Datei …
      (should (equal out (tibetan-handout-build scope "2026-10-10")))
      (should (string-match-p "Stand 2026-10-10"
                              (with-temp-buffer (insert-file-contents out)
                                                (buffer-string))))
      ;; … eine handeigene Datei nie.
      (with-temp-file out (insert "* Meine eigene Fassung\n"))
      (should-error (tibetan-handout-build scope "2026-10-11")
                    :type 'user-error)
      (should (equal "* Meine eigene Fassung\n"
                     (with-temp-buffer (insert-file-contents out)
                                       (buffer-string)))))))

(ert-deftest tibetan-handout-output-names ()
  "Dateiname: Lopez-§ → par-NNN, ein Satz → sent-NNN, mehrere ohne §
→ sent-FIRST-LAST; immer mit dem Quell-Kurznamen (Mehrquellen-
Ordner kollidieren nicht, §5.23-Lektion)."
  (tibetan-handout-test--with-corpus
    (should (string-suffix-p "handouts/rgyan-sent-654.org"
                             (tibetan-handout--output-file
                              (list :source src :sent-nums '(654)))))
    (should (string-suffix-p "handouts/rgyan-sent-654-656.org"
                             (tibetan-handout--output-file
                              (list :source src :sent-nums '(654 655 656)))))))

(ert-deftest tibetan-handout-command-org-only-and-pdf ()
  "C-c u H: in der Quelle mit C-u nur die .org (kein LaTeX-Lauf, die
.org wird geöffnet); ohne Präfix exportiert und das PDF geöffnet.  IN
einer erzeugten Handout-.org exportiert C-c u H die Datei SO WIE SIE
IST (gespeichert) — Carstens Kürzungen vor dem Druck dürfen nie durch
Neu-Generieren verloren gehen."
  (tibetan-handout-test--with-corpus
    (let ((exported nil) (opened nil)
          (org-file (expand-file-name "handouts/rgyan-par-220.org" dir)))
      (cl-letf (((symbol-function 'tibetan-handout-export-pdf)
                 (lambda (org) (setq exported org)
                   (concat (file-name-sans-extension org) ".pdf")))
                ((symbol-function 'org-open-file)
                 (lambda (f &rest _) (setq opened f))))
        (tibetan-handout-test--in-source
          (goto-char (point-min))
          (re-search-forward "^\\*\\* Section §220")
          (save-window-excursion
            (tibetan-handout '(4))
            (should (file-exists-p org-file))
            (should-not exported)
            ;; Jetzt IN der Handout-.org: kürzen, C-c u H → Export der
            ;; bearbeiteten Datei, kein Neu-Generieren.
            (should (equal (file-truename org-file)
                           (file-truename (buffer-file-name))))
            (goto-char (point-max))
            (insert "\nMEINE KÜRZUNG\n")
            (tibetan-handout nil)
            (should (equal (file-truename org-file) (file-truename exported)))
            (should (string-match-p "MEINE KÜRZUNG"
                                    (with-temp-buffer
                                      (insert-file-contents org-file)
                                      (buffer-string))))
            (set-buffer-modified-p nil)
            (kill-buffer))
          ;; Aus der Quelle ohne Präfix: generieren + exportieren + öffnen.
          (setq exported nil opened nil)
          (tibetan-handout nil)
          (should (string-suffix-p "rgyan-par-220.org" exported))
          (should (string-suffix-p "rgyan-par-220.pdf" opened)))))))

(ert-deftest tibetan-handout-export-pdf-smoke ()
  "Echter LuaLaTeX-Lauf (nur wo lualatex + der Tibetisch-Font
vorhanden sind): die erzeugte .org wird zu einem PDF."
  (skip-unless (and (executable-find "lualatex")
                    (executable-find "luaotfload-tool")
                    (zerop (call-process "luaotfload-tool" nil nil nil
                                         "--find" tibetan-handout-tibetan-font))))
  (tibetan-handout-test--with-corpus
    (let* ((out (tibetan-handout-build
                 (list :source src :sent-nums '(654) :label "Satz 654")
                 "2026-10-09"))
           (pdf (tibetan-handout-export-pdf out)))
      (should (file-exists-p pdf))
      (should (string-suffix-p "rgyan-sent-654.pdf" pdf)))))

;; ----------------------------------------------------------------------------
;; §5.68 — Synopse-Vorspann (Siglen, Stellen, Parallele, Kommentar)
;; ----------------------------------------------------------------------------

(defconst tibetan-handout-test--syn-sa
  (concat
   "#+TITLE: Mahāyānasūtrālaṃkāra IX (bodhi) mit Bhāṣya — CAT-Quelle (Sanskrit)\n"
   "#+TIBETAN_LAYOUT: cascade\n#+SOURCE_LANG: sa\n#+TIBETAN_TARGET_LANG: en\n"
   "#+TIBETAN_SIGLUM: L\n"
   "#+TIBETAN_SYNOPSIS: ../bo/mdo.org\n"
   "#+TIBETAN_COMMENTARY: ../savbh/savbh.org\n"
   "#+TIBETAN_SIGLA: ../Resources/siglen.org\n\n"
   "* Tibetan Text\n"
   "** Section IX.60\n:PROPERTIES:\n:LOPEZ_SECTION: 60\n"
   ":TOPIC: buddhakāyavibhāge saptaślokāḥ\n:END:\n"
   "*** Sentence 127\n**** Segment 285\nbuddhakāyavibhāge saptaślokāḥ /\n\n"
   "*** Sentence 128\n:PROPERTIES:\n:VERSE: MSA IX.60\n:END:\n"
   "**** Segment 286\nsvābhāviko 'tha sāṃbhogyaḥ kāyo nairmāṇiko 'paraḥ /\n\n"
   "**** Segment 287\nkāyabhedā hi buddhānāṃ prathamastu dvayāśrayaḥ // 60\n\n"
   "*** Sentence 129\n**** Segment 288\ntrividhaḥ kāyo buddhānām /\n\n"
   "** Section IX.61\n:PROPERTIES:\n:LOPEZ_SECTION: 61\n:END:\n"
   "*** Sentence 130\n:PROPERTIES:\n:VERSE: MSA IX.61\n:END:\n"
   "**** Segment 292\nsarvadhātuṣu sāṃbhogyo bhinno gaṇaparigrahaiḥ /\n\n")
  "Sanskrit-Leitquelle mit den §5.68-Keywords (Vers IX.60 des MSA).")

(defconst tibetan-handout-test--syn-bo
  (concat
   "#+TITLE: mdo sde rgyan IX — CAT-Quelle (D4026)\n"
   "#+TIBETAN_LAYOUT: cascade\n#+TIBETAN_SIGLUM: D4026\n\n"
   "* Tibetan Text\n"
   "** Section IX.60\n:PROPERTIES:\n:LOPEZ_SECTION: 60\n:END:\n"
   "*** Sentence 182\n**** Segment 458\n:PROPERTIES:\n:FOLIO: D159a5\n:END:\n"
   "སངས་རྒྱས་ཀྱི་སྐུ་རབ་ཏུ་དབྱེ་བ་ལ་ཚིགས་སུ་བཅད་པ་བདུན་ནོ། །\n\n"
   "*** Sentence 183\n:PROPERTIES:\n:VERSE: MSA IX.60\n:END:\n"
   "**** Segment 459\n:PROPERTIES:\n:FOLIO: D159a5\n:END:\nསངས་རྒྱས་རྣམས་ཀྱི་སྐུ་དབྱེ་བ། །\n\n"
   "**** Segment 460\n:PROPERTIES:\n:FOLIO: D159a5\n:END:\nརང་བཞིན་ལོངས་སྤྱོད་རྫོགས་པ་དང་། །\n\n"
   "**** Segment 461\n:PROPERTIES:\n:FOLIO: D159a5\n:END:\nགཞན་ནི་སྤྲུལ་པའི་སྐུ་ཡིན་ཏེ། །\n\n"
   "**** Segment 462\n:PROPERTIES:\n:FOLIO: D159a6\n:END:\nདང་པོ་གཉིས་པའི་རྟེན་ཡིན་ནོ། །\n\n"
   "*** Sentence 184\n**** Segment 463\n:PROPERTIES:\n:FOLIO: D159a6\n:END:\n"
   "སངས་རྒྱས་རྣམས་ཀྱི་སྐུ་ནི་རྣམ་པ་གསུམ་སྟེ།\n\n"
   "**** Segment 464\n:PROPERTIES:\n:FOLIO: D159a6\n:END:\n"
   "རང་བཞིན་པ་ནི་ཆོས་ཀྱི་སྐུ་གནས་ཡོངས་སུ་གྱུར་པའི་མཚན་ཉིད་དོ། །\n\n"
   "*** Sentence 185\n**** Segment 465\n:PROPERTIES:\n:FOLIO: D159a6\n:END:\n"
   "ལོངས་སྤྱོད་རྫོགས་པ་ནི་གང་གིས་འཁོར་གྱི་དཀྱིལ་འཁོར་དག་ཏུ་ཆོས་ལ་ལོངས་སྤྱོད་རྫོགས་པར་མཛད་པའོ། །\n\n"
   "*** Sentence 186\n**** Segment 466\n:PROPERTIES:\n:FOLIO: D159a7\n:END:\n"
   "སྤྲུལ་པ་ནི་གང་གིས་སེམས་ཅན་གྱི་དོན་མཛད་པའོ། །\n\n"
   "** Section IX.61\n:PROPERTIES:\n:LOPEZ_SECTION: 61\n:END:\n"
   "*** Sentence 187\n:PROPERTIES:\n:VERSE: MSA IX.61\n:END:\n"
   "**** Segment 467\n:PROPERTIES:\n:FOLIO: D159a7\n:END:\nལོངས་སྤྱོད་རྫོགས་པ་ཁམས་ཀུན་དུ། །\n\n")
  "Tibetische Parallele (D4026) mit :FOLIO: je Segment und :VERSE:.")

(defconst tibetan-handout-test--syn-savbh
  (concat
   "#+TITLE: Sthiramati IX — CAT-Quelle (D4034)\n"
   "#+TIBETAN_LAYOUT: cascade\n#+TIBETAN_SIGLUM: D4034\n\n"
   "* Tibetan Text\n"
   "** Section IX.60\n:PROPERTIES:\n:LOPEZ_SECTION: 60\n:END:\n"
   "*** Sentence 754\n"
   "**** Segment 1780\n:PROPERTIES:\n:FOLIO: D135b5\n:END:\n"
   "སངས་རྒྱས་ཀྱི་སྐུ་རྣམ་པར་དབྱེ་བའི་ཚིགས་སུ་བཅད་པ་བདུན་ཏེ་ཞེས་བྱ་བ་ལ།\n\n"
   "**** Segment 1781\n:PROPERTIES:\n:FOLIO: D135b5\n:END:\n"
   "གོང་དུ་སྐུ་གསུམ་ཐ་དད་དུ་འཇུག་པ་མདོ་ཙམ་དུ་བསྟན་ནས།\n\n"
   "*** Sentence 755\n"
   "**** Segment 1783\n:PROPERTIES:\n:FOLIO: D135b6\n:END:\nརང་བཞིན་དང་ནི་ཞེས་བྱ་ནི།\n\n"
   "**** Segment 1784\n:PROPERTIES:\n:FOLIO: D135b6\n:END:\nརང་བཞིན་ནི་ཆོས་ཀྱི་སྐུ་ལ་བྱ་སྟེ།\n\n"
   "*** Sentence 757\n**** Segment 1790\n:PROPERTIES:\n:FOLIO: D136a1\n:END:\n"
   "སྤྲུལ་པའི་སྐུ་ཡང་གཞན་ཡིན་ཏེ། །\n\n"
   "*** Sentence 758\n**** Segment 1791\n:PROPERTIES:\n:FOLIO: D136a1\n:END:\nཞེས་བྱ་བ་ལ།\n\n"
   "**** Segment 1792\n:PROPERTIES:\n:FOLIO: D136a1\n:END:\n"
   "གཞན་གྱི་སྒྲས་ནི་ཆོས་ཀྱི་སྐུ་དང་ལོངས་སྤྱོད་རྫོགས་པའི་སྐུ་འབའ་ཞིག་ཏུ་མ་ཟད་ཀྱི་སྤྲུལ་པའི་སྐུ་ཡང་ཡོད་དོ། །\n\n"
   "** Section IX.61\n:PROPERTIES:\n:LOPEZ_SECTION: 61\n:END:\n"
   "*** Sentence 768\n**** Segment 1808\n:PROPERTIES:\n:FOLIO: D136a6\n:END:\n"
   "རྫོགས་ལོངས་སྤྱོད་པའི་སྐུ་ཆེར་བཤད་པའི་ཕྱིར། །\n\n")
  "Sthiramatis Kommentar (D4034): Lemma + Marker über Satzgrenzen hinweg.")

(defconst tibetan-handout-test--syn-sent-755
  (concat
   "#+TITLE: Sentence 755 Analysis\n#+TIBETAN_LAYOUT: cascade\n"
   "#+SOURCE: [[file:../savbh.org::*Sentence 755][savbh.org / Sentence 755]]\n"
   "#+SEGMENTS: 1783, 1784\n\n"
   "* Translation\nRegarding \"the intrinsic body and\", the intrinsic body refers to the *dharma* body.\n\n"
   "* Reading\n** Gloss Tables\n"
   "*** Segment 1783\n| rang bzhin |\n- ⟦1783⟧ Regarding \"the intrinsic body and\",\n\n"
   "*** Segment 1784\n| chos |\n- ⟦1784⟧ the intrinsic body refers to the *dharma* body.\n\n"
   "* Tibetan Analysis\n** Claude Vocabulary\n*** Segment 1783\nrang bzhin, noun, \"nature\", Skt. *svabhāva*\n\n"
   "** Claude Grammar\n*** Segment 1783\n- *Verb backbone:* bya.\n\n"
   "** Concept Notes\n\n"
   "* Provided Translations\n** DharmaMitra Translation\nDM POISON\n\n"
   "** Thurman 2004\nTHURMAN POISON TEXT\n\n"
   "* Working Translation\nWT POISON\n\n* My Notes\n\n* Footnotes\n")
  "Gelandete Übersetzung von Sthiramati-Satz 755; Referenzen als Gift.")

(defconst tibetan-handout-test--syn-sigla
  (concat "- L :: Lévi, Sylvain (1907). /Mahāyāna-Sūtrālaṃkāra/. Paris.\n"
          "- D4026 :: Derge, sems tsam, phi 129b–260a (Bhāṣya).\n"
          "- D4034 :: Derge, sems tsam, mi 1b–283a (Sthiramati).\n")
  "Carstens Siglen-Datei: org-Body ohne Überschriften.")

(defmacro tibetan-handout-test--with-synopsis (&rest body)
  "Temp-Korpus für §5.68: sa/msa.org (Leitquelle, Keywords), bo/mdo.org
\(Parallele, D4026), savbh/savbh.org (Kommentar, D4034; Satzdatei 755
mit Übersetzung), Resources/siglen.org.  Bindet DIR, SA, BO, SAVBH."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "handout-syn-" t))
          (sa (expand-file-name "sa/msa.org" dir))
          (bo (expand-file-name "bo/mdo.org" dir))
          (savbh (expand-file-name "savbh/savbh.org" dir)))
     (unwind-protect
         (progn
           (dolist (d '("sa/analysis" "bo/analysis" "savbh/analysis"
                        "Resources" "sa/Resources"))
             (make-directory (expand-file-name d dir) t))
           (with-temp-file sa (insert tibetan-handout-test--syn-sa))
           (with-temp-file bo (insert tibetan-handout-test--syn-bo))
           (with-temp-file savbh (insert tibetan-handout-test--syn-savbh))
           (with-temp-file (expand-file-name "Resources/siglen.org" dir)
             (insert tibetan-handout-test--syn-sigla))
           (with-temp-file (tibetan-sentence--filepath
                            755 (file-name-as-directory
                                 (expand-file-name "savbh/analysis" dir))
                            savbh)
             (insert tibetan-handout-test--syn-sent-755))
           ,@body)
       (delete-directory dir t))))

(ert-deftest tibetan-handout-synopsis-metadata-keywords ()
  "§5.68: die Quelle nennt Synopse-Spalte(n), Kommentar(e), Siglen-Datei
und ihr eigenes Siglum im Kopf — Pfade relativ zur Quelle, SYNOPSIS und
COMMENTARY wiederholbar (nicht zu verwechseln mit
#+TIBETAN_PARALLEL_SOURCE, §5.64); ohne die Keywords bleibt alles nil."
  (tibetan-handout-test--with-synopsis
    (let ((m (tibetan-analysis--read-source-metadata sa)))
      (should (equal '("../bo/mdo.org") (plist-get m :synopsis)))
      (should (equal '("../savbh/savbh.org") (plist-get m :commentary)))
      (should (equal "../Resources/siglen.org" (plist-get m :sigla)))
      (should (equal "L" (plist-get m :siglum))))
    (let ((m (tibetan-analysis--read-source-metadata bo)))
      (should (equal "D4026" (plist-get m :siglum)))
      (should-not (plist-get m :synopsis))
      (should-not (plist-get m :commentary))
      (should-not (plist-get m :sigla)))))

(ert-deftest tibetan-handout-section-sentences ()
  "§5.68: die Sätze einer Section (gleicher Titel in allen Kaskaden)
mit :verse (die :VERSE:-Property des Satzes) und :folios je Segment
\(:FOLIO:); fremder Titel → nil."
  (tibetan-handout-test--with-synopsis
    (let ((ss (tibetan-handout--section-sentences bo "Section IX.60")))
      (should (equal '(182 183 184 185 186)
                     (mapcar (lambda (s) (plist-get s :sent-num)) ss)))
      (should-not (plist-get (nth 0 ss) :verse))
      (should (equal "MSA IX.60" (plist-get (nth 1 ss) :verse)))
      (should (equal '((459 . "D159a5") (460 . "D159a5") (461 . "D159a5")
                       (462 . "D159a6"))
                     (plist-get (nth 1 ss) :folios)))
      (should (equal 459 (car (car (plist-get (nth 1 ss) :segs))))))
    (should (equal '(187)
                   (mapcar (lambda (s) (plist-get s :sent-num))
                           (tibetan-handout--section-sentences bo "Section IX.61"))))
    (should-not (tibetan-handout--section-sentences bo "Section IX.99"))
    ;; Sanskrit: keine Folios, der Vers trägt :verse.
    (let ((ss (tibetan-handout--section-sentences sa "Section IX.60")))
      (should (equal "MSA IX.60" (plist-get (nth 1 ss) :verse)))
      (should-not (plist-get (nth 1 ss) :folios)))))

(defconst tibetan-handout-test--syn-verse-wylie
  '("sangs rgyas rnams kyi sku dbye ba/ /"
    "rang bzhin longs spyod rdzogs pa dang / /"
    "gzhan ni sprul pa'i sku yin te/ /"
    "dang po gnyis pa'i rten yin no/ /")
  "D4026, Vers IX.60 (Wylie, wie der Konverter es liefert).")

(defconst tibetan-handout-test--syn-bhasya-wylie
  '("sangs rgyas kyi sku rab tu dbye ba la tshigs su bcad pa bdun no/ /"
    "sangs rgyas rnams kyi sku ni rnam pa gsum ste/"
    "rang bzhin pa ni chos kyi sku gnas yongs su gyur pa'i mtshan nyid do/ /"
    "longs spyod rdzogs pa ni gang gis 'khor gyi dkyil 'khor dag tu chos la longs spyod rdzogs par mdzad pa'o/ /"
    "sprul pa ni gang gis sems can gyi don mdzad pa'o/ /")
  "D4026, Bhāṣya zu IX.60 (Wylie).")

(defun tibetan-handout-test--runs (segs)
  "Die markierten Läufe von `tibetan-handout--mark-quotes' als
\((TEXT . KIND) …) ohne die unmarkierten Stücke, über alle SEGS."
  (cl-loop for seg in segs
           append (cl-remove-if-not #'cdr seg)))

(ert-deftest tibetan-handout-mark-quotes-gold ()
  "§5.68, Goldstandard vom Blatt der Doktoranden (MSA 10.21, Sthiramati
zu IX.60): Lemma vor `zhes/ces bya' oder `zhes smos' aus dem Vers →
fett (verse), aus dem Bhāṣya → unterstrichen (bhasya) — auch wenn
Lemma und Marker durch Shad und Satzgrenze getrennt sind; Wortläufe ab
zwei Silben, die im Bhāṣya stehen → bhasya (bei Gleichstand gewinnt
das Bhāṣya: das sind Begriffsechos), nur im Vers → verse; reine
Partikelfolgen (`pa ni', `yin no') nie.  Sthiramatis eigene
Versübersetzung (`thog ma … gnas') wird NICHT erkannt — bekannte
Grenze.  Die Zwischenstücke bleiben wörtlich (Abstände, Shad)."
  (let* ((segs '("sangs rgyas kyi sku rnam par dbye ba'i tshigs su bcad pa bdun te zhes bya ba la/"
                 "gong du sku gsum tha dad du 'jug pa mdo tsam du bstan nas/"
                 "rang bzhin dang ni zhes bya ni/"
                 "rang bzhin ni chos kyi sku la bya ste/"
                 "rdzogs longs spyod ces bya bas rdzogs par longs spyod pa'i sku bstan te/"
                 "sprul pa'i sku yang gzhan yin te/ /"
                 "zhes bya ba la/"
                 "'dzam bu'i gling gi sems can yongs su smin par byed pa ni sprul pa'i sku yin no zhes bya ba'i don to/ /"
                 "thog ma gnyis po'i gnas yin no/ /"
                 "zhes bya ba la thog ma ni chos kyi sku la bya ste/"))
         (marked (tibetan-handout--mark-quotes
                  segs tibetan-handout-test--syn-verse-wylie
                  tibetan-handout-test--syn-bhasya-wylie))
         (runs (tibetan-handout-test--runs marked)))
    (should (= (length marked) (length segs)))
    ;; Jedes Segment kommt wörtlich wieder heraus.
    (should (equal segs (mapcar (lambda (seg) (mapconcat #'car seg "")) marked)))
    (should (member '("sangs rgyas kyi sku rnam par dbye ba'i tshigs su bcad pa bdun te" . bhasya) runs))
    (should (member '("rang bzhin dang ni" . verse) runs))
    (should (member '("rdzogs longs spyod" . verse) runs))
    (should (member '("sprul pa'i sku yang gzhan yin te" . verse) runs))
    (should (member '("chos kyi sku" . bhasya) runs))
    (should (member '("sems can" . bhasya) runs))
    (should (member '("longs spyod" . bhasya) runs))
    ;; Segment 2 trägt keine Markierung; Partikelfolgen und Sthiramatis
    ;; eigene Versübersetzung bleiben unmarkiert.
    (should-not (cl-some #'cdr (nth 1 marked)))
    (should-not (cl-some (lambda (r) (member (car r) '("pa ni" "yin no" "ba la"))) runs))
    (should-not (cl-some (lambda (r) (string-match-p "thog ma" (car r))) runs))))

(ert-deftest tibetan-handout-synopsis-data ()
  "§5.68: die Synopse einer Section — Leitspalte, Parallelen und
Kommentare mit Siglum, Stellenangabe aus den :FOLIO: (erstes–letztes
Segment), Vers markiert; der Kommentar trägt die markierten Läufe und
die Übersetzung des Werkzeugs, wo sie gelandet ist — Referenzen nie."
  (tibetan-handout-test--with-synopsis
    (let* ((d (tibetan-handout--synopsis-data sa "Section IX.60"))
           (lead (plist-get d :lead))
           (par (car (plist-get d :parallels)))
           (com (car (plist-get d :commentaries))))
      (should (equal "IX.60" (plist-get d :id)))
      (should (equal "L" (plist-get lead :siglum)))
      (should (equal "sa" (plist-get lead :lang)))
      (should (equal "MSA IX.60" (plist-get lead :locator)))
      (should (equal '(nil t nil)
                     (mapcar (lambda (s) (and (plist-get s :verse) t))
                             (plist-get lead :sentences))))
      (should (equal "svābhāviko 'tha sāṃbhogyaḥ kāyo nairmāṇiko 'paraḥ /"
                     (plist-get (car (plist-get (nth 1 (plist-get lead :sentences)) :segs))
                                :wylie)))
      (should (equal "D4026" (plist-get par :siglum)))
      (should (equal "bo" (plist-get par :lang)))
      (should (equal "159a5–159a7" (plist-get par :locator)))
      (should (equal "sangs rgyas rnams kyi sku dbye ba/ /"
                     (plist-get (car (plist-get (nth 1 (plist-get par :sentences)) :segs))
                                :wylie)))
      (should (equal "D4034" (plist-get com :siglum)))
      (should (equal "135b5–136a1" (plist-get com :locator)))
      (should (equal '(754 755 757 758)
                     (mapcar (lambda (s) (plist-get s :sent-num))
                             (plist-get com :sentences))))
      ;; Markierte Läufe über die Satzgrenze 757/758 (Lemma + Marker).
      (let ((runs (tibetan-handout-test--runs
                   (mapcar (lambda (s)
                             (apply #'append
                                    (mapcar (lambda (g) (plist-get g :runs))
                                            (plist-get s :segs))))
                           (plist-get com :sentences)))))
        (should (member '("sprul pa'i sku yang gzhan yin te" . verse) runs))
        (should (member '("rang bzhin dang ni" . verse) runs))
        (should (member '("chos kyi sku" . bhasya) runs)))
      ;; Übersetzung: 755 gelandet, 754 nicht; Gift nie.
      (should-not (plist-get (nth 0 (plist-get com :sentences)) :translation))
      (should (plist-get (nth 1 (plist-get com :sentences)) :translation))
      (should-not (string-match-p "POISON" (format "%S" d))))))

(ert-deftest tibetan-handout-render-synopsis ()
  "§5.68: vor `* Text' stehen `* Siglen' (Carstens Datei wörtlich +
Konventionszeile) und je Section `* Synopse <id>' als Querformat-Block:
Stellenzeile, zweispaltige Tabelle (Vers fett, Segmentnummern
hochgestellt), darunter der Kommentar mit Fett/Unterstreichung und der
Übersetzung des Werkzeugs; Präambel lädt pdflscape und ulem; nirgends
„Claude“, nie eine Referenz; ohne :synopsis bleibt das Handout wie
bisher."
  (tibetan-handout-test--with-synopsis
    (let* ((syn (tibetan-handout--synopsis-data sa "Section IX.60"))
           (sent (tibetan-handout--sentence-data 128 sa))
           (spec (list :title "MSA IX" :scope "Section IX.60" :source-name "msa.org"
                       :date "2026-10-21" :sentences (list sent)
                       :sigla tibetan-handout-test--syn-sigla
                       :synopsis (list syn)))
           (org (tibetan-handout-render spec)))
      (should (string-match-p "^#\\+LATEX_HEADER: .*pdflscape" org))
      (should (string-match-p "^#\\+LATEX_HEADER: \\\\usepackage\\[normalem\\]{ulem}" org))
      (should (string-match-p "^\\* Siglen$" org))
      (should (string-match-p "^- D4034 :: Derge, sems tsam, mi 1b–283a (Sthiramati)\\.$" org))
      (should (string-match-p "Konventionen" org))
      (should (string-match-p "^\\* Synopse IX\\.60$" org))
      (should (string-match-p "\\\\begin{landscape}" org))
      (should (string-match-p "L MSA IX\\.60 · D4026 159a5–159a7 · D4034 135b5–136a1" org))
      (should (string-match-p "\\\\begin{longtable}" org))
      (should (string-match-p "\\\\textbf{L} & \\\\textbf{D4026}" org))
      (should (string-match-p (regexp-quote "\\segsup{286}\\textbf{svābhāviko 'tha sāṃbhogyaḥ kāyo nairmāṇiko 'paraḥ /}") org))
      (should (string-match-p (regexp-quote "\\segsup{459}\\textbf{sangs rgyas rnams kyi sku dbye ba/ /}") org))
      (should (string-match-p (regexp-quote "\\segsup{288}trividhaḥ kāyo buddhānām /") org))
      (should (string-match-p "D4034 135b5–136a1" org))
      (should (string-match-p (regexp-quote "\\uline{sangs rgyas kyi sku rnam par dbye ba'i tshigs su bcad pa bdun te} zhes bya ba la/") org))
      (should (string-match-p (regexp-quote "\\textbf{rang bzhin dang ni} zhes bya ni/") org))
      (should (string-match-p (regexp-quote "the intrinsic body refers to the \\emph{dharma} body.") org))
      (should (string-match-p "\\\\end{landscape}" org))
      ;; Reihenfolge: Siglen → Synopse → Text → Satz.
      (should (< (string-match "^\\* Siglen$" org)
                 (string-match "^\\* Synopse IX\\.60$" org)
                 (string-match "^\\* Text$" org)
                 (string-match "^\\* Satz 128" org)))
      (should-not (string-match-p "POISON\\|Claude" org))
      (with-temp-buffer
        (insert org)
        (org-mode)
        (should (equal '("Siglen" "Synopse IX.60" "Text" "Satz 128 (Seg. 286–287)")
                       (org-element-map (org-element-parse-buffer) 'headline
                         (lambda (h) (when (= 1 (org-element-property :level h))
                                       (org-element-property :raw-value h)))))))))
  ;; Ohne :synopsis/:sigla: unverändert.
  (let ((org (tibetan-handout-render tibetan-handout-test--spec)))
    (should-not (string-match-p "Synopse\\|Siglen\\|landscape" org))))

(ert-deftest tibetan-handout-build-synopsis ()
  "§5.68: `tibetan-handout-build' liest Parallele, Kommentar und Siglen
aus den Keywords der Quelle und setzt den Vorspann für jede Section
der Sätze im Umfang (je Section einmal); eine Quelle ohne Keywords
baut wie bisher."
  (tibetan-handout-test--with-synopsis
    (let ((out (tibetan-handout-build
                (list :source sa :sent-nums '(127 128 129 130)
                      :label "Sätze 127–130")
                "2026-10-21")))
      (with-temp-buffer
        (insert-file-contents out)
        (should (string-match-p "^\\* Siglen$" (buffer-string)))
        (should (string-match-p "^- L :: Lévi" (buffer-string)))
        (should (= 1 (cl-count-if (lambda (l) (equal l "* Synopse IX.60"))
                                  (split-string (buffer-string) "\n"))))
        (should (= 1 (cl-count-if (lambda (l) (equal l "* Synopse IX.61"))
                                  (split-string (buffer-string) "\n"))))
        (should (string-match-p "D4026 159a5–159a7 · D4034 135b5–136a1" (buffer-string)))
        (should-not (string-match-p "POISON" (buffer-string))))))
  (tibetan-handout-test--with-corpus
    (let ((out (tibetan-handout-build
                (list :source src :sent-nums '(654) :label "Satz 654")
                "2026-10-09")))
      (with-temp-buffer
        (insert-file-contents out)
        (should-not (string-match-p "Synopse\\|Siglen\\|landscape"
                                    (buffer-string)))))))

(provide 'tibetan-handout-test)
;;; tibetan-handout-test.el ends here
