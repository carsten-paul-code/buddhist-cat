;;; bcat-outline-draft-test.el --- Tests for bcat-outline-draft -*- lexical-binding: t -*-

;;; Commentary:
;; Drafting the outline (Gliederung, sa bcad) of a document with Claude:
;; what goes into the requests, how the answers become the org file,
;; the run end to end.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'bcat-outline)
(require 'bcat-outline-draft)
(require 'bcat-sentence)
(require 'tibetan-cascade)
(require 'tibetan-sentence-persist)

(defconst bcat-outline-draft-test--items
  '((:num 1001 :verse nil :section "Section Titel" :folio-from "D72a3" :folio-to "D72a3"
          :text "rgya gar skad du / sa tya dwa ya a ba ta ra /" :translation "In the language of India: Satyadvayāvatāra.")
    (:num 1 :verse "SDA 1" :section "Section 1. Die zwei Wahrheiten (V. 1)" :folio-from "D72a3" :folio-to "D72a4"
          :text "sangs rgyas rnams kyis chos bstan pa / bden pa gnyis la yang dag brten /"
          :translation "The Buddhas' teaching of the Dharma relies on two truths.")
    (:num 2 :verse "SDA 2" :section "Section 2. Die verhüllende Wahrheit (V. 2–3)" :folio-from "D72a4" :folio-to "D72a4"
          :text "kun rdzob rnam pa gnyis su 'dod /" :translation nil)
    (:num 3 :verse "SDA 3" :section "Section 2. Die verhüllende Wahrheit (V. 2–3)" :folio-from "D72a4" :folio-to "D72a5"
          :text "ma brtags gcig pu nyams dga' ba'i /" :translation "Pleasing only as long as it is not examined.")
    (:num 1002 :verse nil :section "Section Kolophon" :folio-from "D73a7" :folio-to "D73a7"
          :text "bden pa gnyis la 'jug pa slob dpon …" :translation nil))
  "The sentences of a small source as a draft sees them (bDen gnyis, shortened).")

(defconst bcat-outline-draft-test--answer
  '((title . "bDen gnyis la 'jug pa: Gliederung")
    (intro . "Der Text hat 28 Verse. Er geht von den zwei Wahrheiten aus.")
    (work (idea . "Atiśa führt in die zwei Wahrheiten ein. Die höchste Wahrheit ist wohl nur durch Überlieferung zugänglich.")
          (groups ((title . "V. 1: Grundsatz") (text . "Die Lehre ruht auf zwei Wahrheiten."))
                  ((title . "V. 2–28: Ausführung") (text . ""))))
    (nodes ((title . "Titel und Verehrung") (short . "Titel") (from . 1001) (to . 1001)
            (text . "Sanskrit- und tibetischer Titel.") (transition . "")
            (sentences ((num . 1001) (role . "Titel") (text . "Nennt das Werk in beiden Sprachen."))))
           ((title . "I. Die zwei Wahrheiten") (short . "Zwei Wahrheiten") (from . 1) (to . 3)
            (text . "Die Lehre der Buddhas ruht auf zwei Wahrheiten.") (transition . "Nach dem Titel beginnt die Lehre.")
            (nodes ((title . "A. Grundsatz") (short . "Grundsatz") (from . 1) (to . 1)
                    (text . "Der Leitsatz des Werks.") (transition . "")
                    (sentences ((num . 1) (role . "these") (text . "These: zwei Wahrheiten. Die Einteilung folgt in 2."))))
                   ((title . "B. Die verhüllende Wahrheit: falsch und richtig") (short . "Verhüllende Wahrheit")
                    (from . 2) (to . 3)
                    (text . "Zwei Arten der /kun rdzob/-Wahrheit.") (transition . ""))))
           ((title . "Kolophon") (short . "Kolophon") (from . 1002) (to . 1002)
            (text . "Verfasser und Übersetzer.") (transition . "")
            (sentences ((num . 1002) (role . "Kolophon") (text . "Nennt Atiśa und rGya brTson 'grus seng ge."))))))
  "Claude's answer for `bcat-outline-draft-test--items', decoded.")

(ert-deftest bcat-outline-reads-several-runs ()
  "A node may cover sentences that are no single run of numbers — the
title 1001 before verse 1, the colophon 1002 after verse 28: „Sätze
1001, 1–28, 1002“.  The reader takes every run."
  (let* ((text (concat "* Gliederung\n** Der ganze Text\nSätze 1001, 1–28, 1002 · D72a3–D73a7\n\nAlles.\n\n"
                       "*** Titel\nSatz 1001 · D72a3\n\nTitel.\n\n"
                       "*** I. Lehre\nSätze 1–28 · SDA 1–28\n\nDie Verse.\n"))
         (whole (car (bcat-outline-nodes text))))
    (should (equal '((1001 . 1001) (1 . 28) (1002 . 1002)) (plist-get whole :ranges)))
    (should (equal "Der ganze Text › Titel" (bcat-outline-place text 1001)))
    (should (equal "Der ganze Text › I. Lehre" (bcat-outline-place text 14)))
    (should (equal "Der ganze Text" (bcat-outline-place text 1002)))
    (should-not (bcat-outline-place text 500))))

(ert-deftest bcat-outline-draft-parts ()
  "A document too large for one request is drafted in parts: consecutive
sentences up to `bcat-outline-part-size' characters of text and
translation, cut where the source itself begins a section; a section
larger than that is cut between sentences.  A small document is one
part."
  (let ((items bcat-outline-draft-test--items))
    (should (equal (list items) (bcat-outline-draft--parts items 100000)))
    ;; cut at the sections (the sentences bring 86, 131, 31, 76 and 34 characters)
    (should (equal '((1001 1) (2 3 1002))
                   (mapcar (lambda (p) (mapcar (lambda (i) (plist-get i :num)) p))
                           (bcat-outline-draft--parts items 220))))
    (should (equal '((1001) (1) (2 3) (1002))
                   (mapcar (lambda (p) (mapcar (lambda (i) (plist-get i :num)) p))
                           (bcat-outline-draft--parts items 120))))
    ;; a section larger than the size: between its sentences
    (should (equal '((1001) (1) (2) (3) (1002))
                   (mapcar (lambda (p) (mapcar (lambda (i) (plist-get i :num)) p))
                           (bcat-outline-draft--parts items 10))))
    (should-not (bcat-outline-draft--parts nil 100))
    ;; … and up to so many sentences: every sentence of a commentary gets
    ;; a line of its own in the answer, and the answer has a limit (a
    ;; part of 170 short sentences would not fit) — cut at the sections
    ;; where they fit, else between the sentences
    (should (equal '((1001 1) (2 3) (1002))
                   (mapcar (lambda (p) (mapcar (lambda (i) (plist-get i :num)) p))
                           (bcat-outline-draft--parts items 100000 2))))
    (should (equal '((1001) (1) (2) (3) (1002))
                   (mapcar (lambda (p) (mapcar (lambda (i) (plist-get i :num)) p))
                           (bcat-outline-draft--parts items 100000 1))))
    (should (equal (list items) (bcat-outline-draft--parts items 100000 5)))))

(ert-deftest bcat-outline-draft-prompts ()
  "What a draft asks of Claude (Carsten 20.11.: the tool drafts the
outline itself, „generisch und Teil des Workflows“; the form is the
Tantrāloka's): one JSON object — the nodes nested like a sa bcad, each
with title, short title, first and last sentence, two to four
sentences, a transition; one line per sentence of prose or commentary
\(what it does, what it explains, how it hangs together with its
neighbours); the work as a whole.  German, plain, uncertain things
marked „wohl“ or „nicht geprüft“, no doxographical labels.  The request
gives the document, its own sections, and every sentence with number,
verse, folio, text and the translation it has."
  (let ((sys (bcat-outline-draft-system-prompt))
        (user (bcat-outline-draft-user-prompt
               '(:title "bDen pa gnyis la 'jug pa" :context ("Atiśa, Satyadvayāvatāra (Toh 3902).")
                 :sections ("Section Titel" "Section 1. Die zwei Wahrheiten (V. 1)"))
               bcat-outline-draft-test--items)))
    (should (string-match-p "sa bcad" sys))
    (should (string-match-p
             (regexp-quote "{\"title\": \"…\", \"intro\": \"…\", \"nodes\": [{\"title\": \"…\", \"short\": \"…\", \"from\": 0, \"to\": 0, \"text\": \"…\", \"transition\": \"…\", \"nodes\": [], \"sentences\": [{\"num\": 0, \"role\": \"…\", \"text\": \"…\"}]}], \"work\": {\"idea\": \"…\", \"groups\": [{\"title\": \"…\", \"text\": \"…\"}]}}")
             sys))
    (dolist (rule '("^- nodes: " "^- title: " "^- short: " "^- text: " "^- transition: " "^- sentences: " "^- work: "
                    "two to four sentences" "exactly one innermost node"
                    "how it hangs together with its neighbours" "„wohl“" "„nicht geprüft“"
                    "in German" "no school" "Invent nothing"))
      (should (string-match-p rule sys)))
    (should (string-match-p "\\`Document: bDen pa gnyis la 'jug pa\n" user))
    ;; the notes on the document are those of the sentence analysis — they
    ;; may name another target language; the outline is German all the same
    (should (string-match-p
             (concat "^Notes on the document (written for the analysis of its sentences; "
                     "the outline is in German whatever they say of a target language):\n"
                     "Atiśa, Satyadvayāvatāra (Toh 3902)\\.$")
             user))
    (should (string-match-p "^The source's own sections, in order:\n- Section Titel\n- Section 1\\. Die zwei Wahrheiten (V\\. 1)$" user))
    ;; a section of the source is marked where it begins; a sentence
    ;; without a translation has none
    (should (string-match-p
             (regexp-quote (concat "— Section Titel —\n"
                                   "[1001] prose · D72a3: rgya gar skad du / sa tya dwa ya a ba ta ra /\n"
                                   "  = In the language of India: Satyadvayāvatāra.\n"
                                   "— Section 1. Die zwei Wahrheiten (V. 1) —\n"
                                   "[1] verse SDA 1 · D72a3–D72a4: sangs rgyas rnams kyis chos bstan pa / bden pa gnyis la yang dag brten /\n"
                                   "  = The Buddhas' teaching of the Dharma relies on two truths.\n"
                                   "— Section 2. Die verhüllende Wahrheit (V. 2–3) —\n"
                                   "[2] verse SDA 2 · D72a4: kun rdzob rnam pa gnyis su 'dod /\n"
                                   "[3] verse SDA 3 · D72a4–D72a5: ma brtags gcig pu nyams dga' ba'i /\n"
                                   "  = Pleasing only as long as it is not examined.\n"
                                   "— Section Kolophon —\n"
                                   "[1002] prose · D73a7: "))
             user))
    (should (string-match-p "Return the JSON object now\\.\\'" user))
    (should-not (string-match-p "This is part" user)))
  ;; a part of several: which one, what was outlined so far, no „work“
  (let ((user (bcat-outline-draft-user-prompt
               '(:title "T" :context nil :sections nil)
               (seq-take bcat-outline-draft-test--items 2) 2 3 '("Titel und Verehrung (1001)" "I. Die zwei Wahrheiten (1–3)"))))
    (should (string-match-p "^This is part 2 of 3 of the document" user))
    (should (string-match-p "^Outlined so far:\n- Titel und Verehrung (1001)\n- I\\. Die zwei Wahrheiten (1–3)$" user))
    (should (string-match-p "Leave \"work\", \"title\" and \"intro\" empty" user))))

(ert-deftest bcat-outline-draft-renders-the-reference-form ()
  "The answer becomes an outline in the form of the Tantrāloka's: the
head for the PDF, the work as a whole, the nodes as headings — each
with the line of what it covers (the sentences, counted in the order of
the source: „Sätze 1001“, „Sätze 1–3“; their verses and folios), its
sentences' lines „- *N*: Rolle. …“ —, and the list „Ort im Ganzen“ with
a line for EVERY sentence of the source: the way down the short titles,
with the role of a sentence that has a line of its own.  What it writes
it reads: `bcat-outline-place' finds every sentence again."
  (let* ((text (bcat-outline-draft-render bcat-outline-draft-test--answer
                                          '(:title "bDen pa gnyis la 'jug pa" :date "2026-11-20")
                                          bcat-outline-draft-test--items)))
    (should (string-match-p "\\`#\\+TITLE: bDen pa gnyis la 'jug pa — Gliederung\n" text))
    (should (string-match-p "^#\\+SUBTITLE: .*Entwurf des Werkzeugs · Stand 2026-11-20$" text))
    ;; the overview in front (toc:3) lists the nodes only when the
    ;; headings are numbered ones — unnumbered by secnumdepth, not by
    ;; num:nil, which leaves the list empty under „Contents“ (the PDF of
    ;; the first real draft, 20.11.)
    (should (string-match-p "^#\\+OPTIONS: toc:3 num:t author:nil date:nil \\^:nil H:5$" text))
    (should (string-match-p (regexp-quote "\\setcounter{secnumdepth}{0}") text))
    (should (string-match-p (regexp-quote "\\renewcommand{\\contentsname}{Übersicht}") text))
    (should (string-match-p "^#\\+LATEX_COMPILER: lualatex$" text))
    (should (string-match-p "^#\\+LATEX_HEADER: \\\\usepackage{libertinus}$" text))
    (should (string-match-p
             (regexp-quote (concat "* bDen pa gnyis la 'jug pa im Ganzen\n\n"
                                   "Atiśa führt in die zwei Wahrheiten ein. Die höchste Wahrheit ist wohl nur durch Überlieferung zugänglich.\n\n"
                                   "** V. 1: Grundsatz\nDie Lehre ruht auf zwei Wahrheiten.\n\n"
                                   "** V. 2–28: Ausführung\n\n"
                                   "* bDen gnyis la 'jug pa: Gliederung\n\n"
                                   "Der Text hat 28 Verse. Er geht von den zwei Wahrheiten aus.\n\n"
                                   "** Titel und Verehrung\nSatz 1001 · D72a3\n\n"
                                   "Sanskrit- und tibetischer Titel.\n\n"
                                   "- *1001*: Titel. Nennt das Werk in beiden Sprachen.\n\n"
                                   "** I. Die zwei Wahrheiten\nSätze 1–3 · SDA 1–3 · D72a3–D72a5\n\n"
                                   "Übergang: Nach dem Titel beginnt die Lehre.\n\n"
                                   "Die Lehre der Buddhas ruht auf zwei Wahrheiten.\n\n"
                                   "*** A. Grundsatz\nSatz 1 · SDA 1 · D72a3–D72a4\n\n"
                                   "Der Leitsatz des Werks.\n\n"
                                   "- *1*: These: zwei Wahrheiten. Die Einteilung folgt in 2.\n\n"
                                   "*** B. Die verhüllende Wahrheit: falsch und richtig\nSätze 2–3 · SDA 2–3 · D72a4–D72a5\n\n"
                                   "Zwei Arten der /kun rdzob/-Wahrheit.\n\n"
                                   "** Kolophon\nSatz 1002 · D73a7\n\n"
                                   "Verfasser und Übersetzer.\n\n"
                                   "- *1002*: Kolophon. Nennt Atiśa und rGya brTson 'grus seng ge.\n\n"
                                   "* Anhang: Ort im Ganzen je Satz\n"))
             text))
    (should (string-match-p
             (regexp-quote (concat "#+NAME: ort-im-ganzen\n"
                                   "- 1001: Titel\n"
                                   "- 1: Zwei Wahrheiten › Grundsatz: These\n"
                                   "- 2: Zwei Wahrheiten › Verhüllende Wahrheit\n"
                                   "- 3: Zwei Wahrheiten › Verhüllende Wahrheit\n"
                                   "- 1002: Kolophon\n"))
             text))
    ;; what it writes it reads
    (should (equal '(1001 1 2 3 1002) (mapcar #'car (bcat-outline-places text))))
    (should (equal "Zwei Wahrheiten › Grundsatz: These" (bcat-outline-place text 1)))
    (should (equal '(1001 1 1002) (mapcar #'car (bcat-outline-entries text))))
    (should (equal '((1 . 3)) (plist-get (cl-find "I. Die zwei Wahrheiten" (bcat-outline-nodes text)
                                                  :key (lambda (n) (plist-get n :title)) :test #'equal)
                                         :ranges)))
    ;; the stamp of a draft nobody touched
    (should (bcat-outline-draft-untouched-p text))
    (should-not (bcat-outline-draft-untouched-p (concat text "\nVon Hand ergänzt.\n")))
    (should-not (bcat-outline-draft-untouched-p "* Von Hand\nOhne Stempel.\n"))))

(ert-deftest bcat-outline-draft-covers-every-sentence ()
  "A sentence the answer's nodes leave out still has its line in the
list — under the node before it, marked „(nicht eingeordnet)“ — and the
draft says which ones: nothing is dropped in silence.  A node whose
first and last sentence are given in the order of the source covers
what lies between there (the title 1001 before verse 1)."
  (let* ((answer '((title . "T: Gliederung") (intro . "")
                   (nodes ((title . "A. Anfang") (short . "Anfang") (from . 1001) (to . 1) (text . "x"))
                          ((title . "B. Rest") (short . "Rest") (from . 3) (to . 1002) (text . "y")))))
         (text (bcat-outline-draft-render answer '(:title "T" :date "2026-11-20") bcat-outline-draft-test--items)))
    (should (string-match-p "^\\*\\* A\\. Anfang\nSätze 1001, 1 · SDA 1 · D72a3–D72a4$" text))
    (should (string-match-p "^\\*\\* B\\. Rest\nSätze 3, 1002 · SDA 3 · D72a4–D73a7$" text))
    (should (string-match-p "^- 2: Anfang (nicht eingeordnet)$" text))
    (should (equal '(2) (bcat-outline-draft--uncovered answer bcat-outline-draft-test--items)))
    (should-not (bcat-outline-draft--uncovered bcat-outline-draft-test--answer bcat-outline-draft-test--items))))

(ert-deftest bcat-outline-draft-merges-parts ()
  "A document drafted in parts: the nodes of the parts one after the
other, under the groups of the last request (the frame) where it gives
some — a node goes under the group that has its first sentence —, with
that request's title, introduction and view of the whole work."
  (let* ((part1 '((nodes ((title . "1. Titel") (short . "Titel") (from . 1001) (to . 1001) (text . "a"))
                         ((title . "2. Grundsatz") (short . "Grundsatz") (from . 1) (to . 1) (text . "b")))))
         (part2 '((nodes ((title . "3. Verhüllend") (short . "Verhüllend") (from . 2) (to . 3) (text . "c"))
                         ((title . "4. Kolophon") (short . "Kolophon") (from . 1002) (to . 1002) (text . "d")))))
         (frame '((title . "T: Gliederung") (intro . "Zwei Teile.")
                  (work (idea . "Idee.") (groups))
                  (groups ((title . "I. Anfang") (short . "Anfang") (from . 1001) (to . 1) (text . "Titel und Leitsatz.") (transition . ""))
                          ((title . "II. Ausführung") (short . "Ausführung") (from . 2) (to . 1002) (text . "Der Rest.") (transition . "Dann.")))))
         (merged (bcat-outline-draft--merge (list part1 part2) frame bcat-outline-draft-test--items)))
    (should (equal "T: Gliederung" (alist-get 'title merged)))
    (should (equal "Idee." (alist-get 'idea (alist-get 'work merged))))
    (should (equal '("I. Anfang" "II. Ausführung") (mapcar (lambda (n) (alist-get 'title n)) (alist-get 'nodes merged))))
    (should (equal '("1. Titel" "2. Grundsatz")
                   (mapcar (lambda (n) (alist-get 'title n)) (alist-get 'nodes (car (alist-get 'nodes merged))))))
    (should (equal '("3. Verhüllend" "4. Kolophon")
                   (mapcar (lambda (n) (alist-get 'title n)) (alist-get 'nodes (cadr (alist-get 'nodes merged))))))
    ;; without groups: the parts' nodes as they are
    (should (equal '("1. Titel" "2. Grundsatz" "3. Verhüllend" "4. Kolophon")
                   (mapcar (lambda (n) (alist-get 'title n))
                           (alist-get 'nodes (bcat-outline-draft--merge (list part1 part2) '((title . "T")) bcat-outline-draft-test--items)))))))

(defconst bcat-outline-draft-test--source
  (concat "#+TITLE: bDen pa gnyis la 'jug pa — CAT-Quelle\n"
          "#+TIBETAN_LAYOUT: cascade\n"
          "#+CAT_TEMPLATE: tibetan\n"
          "#+TIBETAN_TARGET_LANG: en\n"
          "#+TIBETAN_CLAUDE_CONTEXT: Atiśa, Satyadvayāvatāra (Toh 3902).\n\n"
          "* Tibetan Text\n"
          "** Section 1. Die zwei Wahrheiten (V. 1)\n"
          "*** Sentence 1\n:PROPERTIES:\n:VERSE: SDA 1\n:END:\n"
          "**** Segment 6\n:PROPERTIES:\n:FOLIO: D72a3\n:END:\nསངས་རྒྱས་རྣམས་ཀྱིས་ཆོས་བསྟན་པ། །\n\n"
          "** Section 2. Die verhüllende Wahrheit (V. 2–3)\n"
          "*** Sentence 2\n:PROPERTIES:\n:VERSE: SDA 2\n:END:\n"
          "**** Segment 10\n:PROPERTIES:\n:FOLIO: D72a4\n:END:\nཀུན་རྫོབ་རྣམ་པ་གཉིས་སུ་འདོད། །\n\n"
          "*** Sentence 3\n:PROPERTIES:\n:VERSE: SDA 3\n:END:\n"
          "**** Segment 14\n:PROPERTIES:\n:FOLIO: D72a5\n:END:\nམ་བརྟགས་གཅིག་པུ་ཉམས་དགའ་བའི། །\n\n")
  "A cascade source with three verses in two sections.")

(defmacro bcat-outline-draft-test--with-source (&rest body)
  "Run BODY with DIR a fresh folder and SRC the source in it; every
request lands at once with what ANSWER-OF (a function of the request's
kind and its prompts) returns.  Binds REQUESTS, the (KIND SYSTEM . USER)
sent, latest first."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "bcat-outline-draft-" t))
          (src (expand-file-name "bden-cat.org" dir))
          (requests '())
          (answer-of nil)
          (bcat-outline-draft--runs (make-hash-table :test 'equal))
          (bcat-outline-draft-done-functions nil))
     (unwind-protect
         (progn
           (with-temp-file src (insert bcat-outline-draft-test--source))
           (cl-letf (((symbol-function 'tibetan-discussion--submit)
                      (lambda (kind file thunk land &optional fail)
                        (let ((prompts (funcall thunk)))
                          (push (cons kind prompts) requests)
                          (let ((answer (funcall answer-of kind prompts)))
                            (if answer (funcall land file answer) (funcall fail file "kein Netz"))))
                        t)))
             ,@body))
       (delete-directory dir t))))

(ert-deftest bcat-outline-draft-end-to-end ()
  "C-c u S for a source (Carsten 20.11.: the tool drafts the outline
itself): the sentences of the source go to Claude — number, verse,
folio, text; the translation of a sentence that is analysed already
\(„reverse“) —, the answer becomes Gliederung-<source>.org beside the
source, in the form the reader knows; the sentence files can take their
place from it.  A draft nobody touched is replaced by the next one; an
outline someone worked on stays, the new draft goes beside it as
…-Entwurf.org.  An answer that cannot be read or a request without an
answer writes nothing."
  (bcat-outline-draft-test--with-source
    (let ((outline (expand-file-name "Gliederung-bden-cat.org" dir))
          (answer (concat "{\"title\": \"bDen gnyis: Gliederung\", \"intro\": \"Drei Verse.\","
                          " \"work\": {\"idea\": \"Zwei Wahrheiten.\", \"groups\": []},"
                          " \"nodes\": [{\"title\": \"I. Grundsatz\", \"short\": \"Grundsatz\", \"from\": 1, \"to\": 1,"
                          " \"text\": \"Der Leitsatz.\", \"transition\": \"\","
                          " \"sentences\": [{\"num\": 1, \"role\": \"These\", \"text\": \"Die Lehre ruht auf zwei Wahrheiten.\"}]},"
                          " {\"title\": \"II. Verhüllende Wahrheit\", \"short\": \"Verhüllend\", \"from\": 2, \"to\": 3,"
                          " \"text\": \"Zwei Arten.\", \"transition\": \"Dann die Einteilung.\"}]}")))
      ;; a sentence analysed already: its translation goes into the request
      (make-directory (expand-file-name "analysis" dir) t)
      (with-temp-file (tibetan-sentence--filepath 2 (file-name-as-directory (expand-file-name "analysis" dir)) src)
        (insert "#+CAT_TEMPLATE: tibetan\n* Text\n*Translation:* The concealing is held to be twofold.\n"))
      (setq answer-of (lambda (_kind _prompts) answer))
      (should (= 1 (bcat-outline-draft-source src)))
      (should (= 1 (length requests)))
      (let ((user (cddr (car requests))))
        (should (string-match-p "\\`Document: bDen pa gnyis la 'jug pa\nNotes on the document .*:\nAtiśa, Satyadvayāvatāra (Toh 3902)\\.$" user))
        (should (string-match-p "^\\[1\\] verse SDA 1 · D72a3: སངས་རྒྱས་རྣམས་ཀྱིས་ཆོས་བསྟན་པ། །$" user))
        (should (string-match-p "^\\[2\\] verse SDA 2 · D72a4: .*\n  = The concealing is held to be twofold\\.$" user))
        (should-not (string-match-p "This is part" user)))
      (should (equal (bcat-outline-draft-system-prompt) (cadr (car requests))))
      (let ((text (with-temp-buffer (insert-file-contents outline) (buffer-string))))
        (should (string-match-p "^\\*\\* I\\. Grundsatz\nSatz 1 · SDA 1 · D72a3$" text))
        (should (string-match-p "^\\*\\* II\\. Verhüllende Wahrheit\nSätze 2–3 · SDA 2–3 · D72a4–D72a5$" text))
        (should (equal "Grundsatz: These" (bcat-outline-place text 1)))
        (should (equal "Verhüllend" (bcat-outline-place text 3)))
        (should (bcat-outline-draft-untouched-p text))
        ;; the source finds it
        (should (equal text (bcat-outline-text src))))
      ;; a draft nobody touched: the next one replaces it
      (setq answer-of (lambda (_k _p) (replace-regexp-in-string "Der Leitsatz\\." "Der Leitsatz des Werks." answer t t)))
      (bcat-outline-draft-source src t)
      (should (string-match-p "Der Leitsatz des Werks\\." (bcat-outline-text src)))
      (should-not (file-exists-p (expand-file-name "Gliederung-bden-cat-Entwurf.org" dir)))
      ;; worked on by hand: it stays, the draft goes beside it
      (with-temp-file outline
        (insert (replace-regexp-in-string "Der Leitsatz des Werks\\." "Der Leitsatz, von Hand berichtigt."
                                          (bcat-outline-text src) t t)))
      (let ((by-hand (bcat-outline-text src)) (written nil))
        (add-hook 'bcat-outline-draft-done-functions (lambda (_s f) (setq written f)))
        (setq answer-of (lambda (_k _p) answer))
        (bcat-outline-draft-source src t)
        (should (equal by-hand (bcat-outline-text src)))
        (should (equal (expand-file-name "Gliederung-bden-cat-Entwurf.org" dir) written))
        (should (string-match-p "Der Leitsatz\\.$"
                                (with-temp-buffer (insert-file-contents written) (buffer-string))))
        ;; nothing readable, or no answer: nothing written
        (delete-file written)
        (setq answer-of (lambda (_k _p) "Das kann ich nicht."))
        (bcat-outline-draft-source src t)
        (should-not (file-exists-p written))
        (setq answer-of (lambda (_k _p) nil))
        (bcat-outline-draft-source src t)
        (should-not (file-exists-p written))
        (should (equal by-hand (bcat-outline-text src)))
        (should (zerop (hash-table-count bcat-outline-draft--runs)))))))

(ert-deftest bcat-outline-draft-in-parts ()
  "A document larger than `bcat-outline-part-size': one request per part
— each told which part it is and what was outlined so far —, then one
for the whole; the outline has the parts' nodes under the groups of
that last request."
  (bcat-outline-draft-test--with-source
    ;; the three verses bring 31, 29 and 31 characters: the first section
    ;; one part, the second another
    (let ((bcat-outline-part-size 70)
          (n 0))
      (setq answer-of
            (lambda (kind _prompts)
              (cl-incf n)
              (cond
               ((equal kind "bcat-outline-1")
                "{\"nodes\": [{\"title\": \"1. Grundsatz\", \"short\": \"Grundsatz\", \"from\": 1, \"to\": 1, \"text\": \"a\"}]}")
               ((equal kind "bcat-outline-2")
                "{\"nodes\": [{\"title\": \"2. Einteilung\", \"short\": \"Einteilung\", \"from\": 2, \"to\": 3, \"text\": \"b\"}]}")
               ((equal kind "bcat-outline-frame")
                (concat "{\"title\": \"bDen gnyis: Gliederung\", \"intro\": \"Zwei Teile.\","
                        " \"work\": {\"idea\": \"Idee.\", \"groups\": []},"
                        " \"groups\": [{\"title\": \"I. Die Lehre\", \"short\": \"Lehre\", \"from\": 1, \"to\": 3,"
                        " \"text\": \"Alles.\", \"transition\": \"\"}]}")))))
      (should (= 3 (bcat-outline-draft-source src)))
      (should (equal '("bcat-outline-1" "bcat-outline-2" "bcat-outline-frame") (reverse (mapcar #'car requests))))
      (let ((second (cddr (nth 1 requests)))
            (frame (cddr (car requests)))
            (text (bcat-outline-text src)))
        (should (string-match-p "^This is part 2 of 2 of the document" second))
        (should (string-match-p "^Outlined so far:\n- 1\\. Grundsatz (1): a$" second))
        (should (string-match-p "^\\[2\\] verse SDA 2" second))
        (should-not (string-match-p "^\\[1\\] " second))
        (should (string-match-p "^- 1\\. Grundsatz (1): a\n- 2\\. Einteilung (2–3): b$" frame))
        (should (string-match-p "^\\*\\* I\\. Die Lehre\nSätze 1–3 · SDA 1–3 · D72a3–D72a5$" text))
        (should (string-match-p "^\\*\\*\\* 1\\. Grundsatz\nSatz 1 · SDA 1 · D72a3$" text))
        (should (string-match-p "^\\*\\*\\* 2\\. Einteilung\nSätze 2–3 " text))
        (should (equal "Lehre › Einteilung" (bcat-outline-place text 3)))
        (should (string-match-p "^\\* bDen gnyis: Gliederung\n\nZwei Teile\\.$" text))))))

(ert-deftest bcat-outline-draft-keeps-the-answers ()
  "The answers of a draft are kept on disk as they land (beside the
sentence files' answers: analysis/.answers/<outline>.eld) — a draft of a
long commentary is many requests, and one that fails must not cost the
ones before it; a draft is set anew (after a correction of the layout)
without asking again.  So: a request that fails ends the run, nothing
is written, the answers so far stay; the next run asks only for what is
missing; a run with everything at hand asks nothing and sets the
outline again; FRESH asks for all of it again; a sentence that changed
in the source has its part and what follows asked again."
  (bcat-outline-draft-test--with-source
    (let* ((bcat-outline-part-size 70)
           (outline (expand-file-name "Gliederung-bden-cat.org" dir))
           (store (expand-file-name "analysis/.answers/Gliederung-bden-cat.eld" dir))
           (fail-at nil)
           (answers
            (lambda (kind _prompts)
              (cond
               ((equal kind fail-at) nil)
               ((equal kind "bcat-outline-1")
                "{\"nodes\": [{\"title\": \"1. Grundsatz\", \"short\": \"Grundsatz\", \"from\": 1, \"to\": 1, \"text\": \"a\"}]}")
               ((equal kind "bcat-outline-2")
                "{\"nodes\": [{\"title\": \"2. Einteilung\", \"short\": \"Einteilung\", \"from\": 2, \"to\": 3, \"text\": \"b\"}]}")
               ((equal kind "bcat-outline-frame")
                (concat "{\"title\": \"bDen gnyis: Gliederung\", \"intro\": \"Zwei Teile.\","
                        " \"work\": {\"idea\": \"Idee.\", \"groups\": []},"
                        " \"groups\": [{\"title\": \"I. Die Lehre\", \"short\": \"Lehre\", \"from\": 1, \"to\": 3,"
                        " \"text\": \"Alles.\", \"transition\": \"\"}]}")))))
           (kinds (lambda () (reverse (mapcar #'car requests)))))
      (setq answer-of answers)
      ;; the second part fails: nothing written, the first answer kept
      (setq fail-at "bcat-outline-2")
      (should (= 3 (bcat-outline-draft-pending src)))
      (bcat-outline-draft-source src)
      (should (equal '("bcat-outline-1" "bcat-outline-2") (funcall kinds)))
      (should-not (file-exists-p outline))
      (should (file-exists-p store))
      (should (zerop (hash-table-count bcat-outline-draft--runs)))
      ;; the next run: only what is missing — the second part, told what
      ;; the first one gave, and the whole
      (setq fail-at nil requests '())
      (should (= 2 (bcat-outline-draft-pending src)))
      (should (= 2 (bcat-outline-draft-source src)))
      (should (equal '("bcat-outline-2" "bcat-outline-frame") (funcall kinds)))
      (should (string-match-p "^Outlined so far:\n- 1\\. Grundsatz (1): a$" (cddr (cadr requests))))
      (let ((text (bcat-outline-text src)))
        (should (string-match-p "^\\*\\*\\* 1\\. Grundsatz\nSatz 1 · SDA 1 · D72a3$" text))
        (should (equal "Lehre › Einteilung" (bcat-outline-place text 3)))
        ;; everything at hand: no request, the outline set again
        (setq requests '())
        (delete-file outline)
        (should (= 0 (bcat-outline-draft-pending src)))
        (should (= 0 (bcat-outline-draft-source src)))
        (should-not requests)
        (should (equal text (bcat-outline-text src))))
      ;; FRESH: all of it again
      (should (= 3 (bcat-outline-draft-pending src t)))
      (should (= 3 (bcat-outline-draft-source src t)))
      (should (equal '("bcat-outline-1" "bcat-outline-2" "bcat-outline-frame") (funcall kinds)))
      ;; the source changed in its second section: that part and the
      ;; whole again, not the first part
      (setq requests '())
      (with-temp-file src
        (insert (replace-regexp-in-string "ཀུན་རྫོབ་རྣམ་པ་གཉིས་སུ་འདོད། །" "ཀུན་རྫོབ་རྣམ་གཉིས་འདོད། །"
                                          bcat-outline-draft-test--source t t)))
      (should (= 2 (bcat-outline-draft-source src)))
      (should (equal '("bcat-outline-2" "bcat-outline-frame") (funcall kinds))))))

(provide 'bcat-outline-draft-test)
;;; bcat-outline-draft-test.el ends here
