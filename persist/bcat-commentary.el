;;; bcat-commentary.el --- Commentary documents for verses (Buddhist CAT) -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;;; Commentary:
;; §5.74, Carsten 30.10.: the MSA reading — Sanskrit and Tibetan verse
;; read together (the bilingual pair, bcat-sentence.el, verse mode), and
;; Sthiramati's commentary (Tibetan only) as a document of its own per
;; verse, linked both ways; C-c u G analyses both and puts both on the
;; reMarkable.
;;
;; Sthiramati comments pāda by pāda: he quotes a verse line as a lemma
;; („ji ltar chu yi snod chag na/ /“), then explains it („… zhes bya ba
;; smos te/ …“), now and then with his own reading of the verse
;; („mi snang bzhin“ for „mi snang ba“).  `bcat-commentary-lemma'
;; recognises the lemmata by their syllables — no AI.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'bcat-sentence)
(require 'bcat-template)
(require 'bcat-analysis)

(require 'bcat-status)
(require 'bcat-parallel)

(declare-function tibetan-analysis-make-short-name "tibetan-analysis-persist" (source-file))
(declare-function tibetan-analysis--read-source-metadata "tibetan-analysis-claude" (source-file))
(declare-function tibetan-discussion--submit "tibetan-translation-discussion" (kind file prompts land))
(defvar tibetan-dharmamitra-sentence-request-delay)
(declare-function tibetan-dharmamitra-api-limit-reached-p "tibetan-dharmamitra-api" ())
(declare-function tibetan-sentence--filepath "tibetan-sentence-persist" (sent-num &optional folder source-file))

;; ----------------------------------------------------------------------------
;; Lemmata
;; ----------------------------------------------------------------------------

(defconst bcat-commentary--quote-markers '("zhes" "ces" "shes")
  "Syllables that close a quotation („… zhes bya ba“).")

(defun bcat-commentary--syllables (wylie)
  "The syllables of WYLIE, shad and spaces dropped."
  (split-string (replace-regexp-in-string "/" " " (or wylie "")) "[ \t\n]+" t))

(defun bcat-commentary--strip-quote (syllables)
  "SYLLABLES without a closing quote marker and what follows it — the
lemma before „zhes bya ba“."
  (let ((pos (cl-position-if (lambda (s) (member s bcat-commentary--quote-markers))
                             syllables)))
    (if (and pos (> pos 0)) (cl-subseq syllables 0 pos) syllables)))

(defun bcat-commentary--lcs (a b)
  "The matched index pairs ((I . J) …) of a longest common subsequence
of the lists A and B, in order."
  (let* ((n (length a)) (m (length b))
         (av (vconcat a)) (bv (vconcat b))
         (d (make-vector (* (1+ n) (1+ m)) 0)))
    (cl-flet ((at (i j) (aref d (+ (* i (1+ m)) j))))
      (cl-loop for i from (1- n) downto 0 do
               (cl-loop for j from (1- m) downto 0 do
                        (aset d (+ (* i (1+ m)) j)
                              (if (equal (aref av i) (aref bv j))
                                  (1+ (at (1+ i) (1+ j)))
                                (max (at (1+ i) j) (at i (1+ j)))))))
      (let ((i 0) (j 0) (out '()))
        (while (and (< i n) (< j m))
          (cond ((equal (aref av i) (aref bv j))
                 (push (cons i j) out)
                 (setq i (1+ i) j (1+ j)))
                ((>= (at (1+ i) j) (at i (1+ j))) (setq i (1+ i)))
                (t (setq j (1+ j)))))
        (nreverse out)))))

(defun bcat-commentary--readings (lemma verse pairs)
  "Where the LEMMA syllables differ from the VERSE syllables between the
matches PAIRS: ((LEMMA-WORDS . VERSE-WORDS) …), each side joined, \"\"
when empty.  The verse's words before the first and after the last
match count only when the lemma has words there too — a lemma may
quote part of a pāda."
  (let ((out '()) (i0 0) (j0 0) (k 0) (last (length pairs)))
    (dolist (pr (append pairs (list (cons (length lemma) (length verse)))))
      (let ((ls (cl-subseq lemma i0 (car pr)))
            (vs (cl-subseq verse j0 (cdr pr))))
        (when (or ls (and vs (> k 0) (< k last)))
          (push (cons (string-join ls " ") (string-join vs " ")) out)))
      (setq i0 (1+ (car pr)) j0 (1+ (cdr pr)) k (1+ k)))
    (nreverse out)))

(defun bcat-commentary-lemma (wylie padas)
  "(PADA-NUM . READINGS) when the commentary segment WYLIE is the lemma
of one of PADAS ((NUM . WYLIE) …, the verse lines), else nil.  A lemma
consists of the pāda's wording — a closing „zhes bya ba …“ aside — and
covers most of it: at least three quarters of its syllables stand in
the pāda (in order), and they make at least three fifths of the pāda.
An explanation that takes up the pāda's words among many others is no
lemma.  READINGS: `bcat-commentary--readings'."
  (let ((s (bcat-commentary--strip-quote (bcat-commentary--syllables wylie)))
        (best nil))
    (when (>= (length s) 2)
      (dolist (p padas)
        (let* ((v (bcat-commentary--syllables (cdr p)))
               (pairs (bcat-commentary--lcs s v))
               (l (length pairs)))
          (when (and (>= l (min 3 (length v)))
                     (>= l (* 0.75 (length s)))
                     (>= l (* 0.6 (length v)))
                     (or (null best) (> l (nth 1 best))))
            (setq best (list (car p) l v pairs))))))
    (when best
      (cons (nth 0 best) (bcat-commentary--readings s (nth 2 best) (nth 3 best))))))

(defun bcat-commentary--marked-quotation (syllables next)
  "The quoted words of a segment marked as a quotation: its SYLLABLES
before a quote marker, or all of them when the NEXT segment's syllables
begin with one (`… yin te/ /' `zhes bya ba la/'); nil when unmarked or
when the segment begins with the marker (the explanation after it)."
  (let ((pos (cl-position-if (lambda (s) (member s bcat-commentary--quote-markers))
                             syllables)))
    (cond ((and pos (> pos 0)) (cl-subseq syllables 0 pos))
          ((and (null pos) (member (car next) bcat-commentary--quote-markers)) syllables))))

(defun bcat-commentary-lemmata (segments padas &optional bhasya)
  "The lemmata among the commentary SEGMENTS ((NUM . WYLIE) …, in
order): ((NUM PADA . READINGS) …).  A segment is a lemma when it
consists of a pāda's wording (`bcat-commentary-lemma', READINGS its
readings), or — MSA IX.60: Sthiramati quotes in his own wording and
word order, often part of a pāda — when it is marked as a quotation
\(`bcat-commentary--marked-quotation') and at least half of the quoted
words, two at least, stand in one pāda (in order), more of them than in
the BHASYA (Wylie strings: a quotation of the bhāṣya is no verse lemma);
READINGS is then `free'."
  (let ((bseq (cl-mapcan #'bcat-commentary--syllables (copy-sequence bhasya)))
        (out '()))
    (cl-loop for (seg . rest) on segments
             for next = (and rest (bcat-commentary--syllables (cdar rest)))
             do (let ((hit (or (bcat-commentary-lemma (cdr seg) padas)
                               (let ((q (bcat-commentary--marked-quotation
                                         (bcat-commentary--syllables (cdr seg)) next))
                                     (best nil))
                                 (when (>= (length q) 2)
                                   (dolist (p padas)
                                     (let ((l (length (bcat-commentary--lcs
                                                       q (bcat-commentary--syllables (cdr p))))))
                                       (when (or (null best) (> l (cdr best)))
                                         (setq best (cons (car p) l)))))
                                   (when (and best (>= (cdr best) 2)
                                              (>= (cdr best) (* 0.5 (length q)))
                                              (> (cdr best) (length (bcat-commentary--lcs q bseq))))
                                     (cons (car best) 'free)))))))
                  (when hit (push (cons (car seg) hit) out))))
    (nreverse out)))

(defun bcat-commentary-reading-text (readings)
  "READINGS of a lemma as text: „verbatim“ without any, „in his own
wording“ for a free quotation (`free'), else what the commentator reads
for the verse's wording."
  (cond
   ((null readings) "verbatim")
   ((eq readings 'free) "in his own wording")
   (t
    (concat "Sthiramati "
            (mapconcat (lambda (r)
                         (cond ((string-empty-p (cdr r)) (format "adds /%s/" (car r)))
                               ((string-empty-p (car r)) (format "omits /%s/" (cdr r)))
                               (t (format "reads /%s/ for /%s/" (car r) (cdr r)))))
                       readings "; ")))))

;; ----------------------------------------------------------------------------
;; Sources, Sections, files
;; ----------------------------------------------------------------------------

(defun bcat-commentary--header-path (file key)
  "FILE's #+KEY: path resolved relative to FILE, or nil."
  (let ((v (and file (bcat-sentence--header-value file key))))
    (and v (expand-file-name v (file-name-directory (expand-file-name file))))))

(defun bcat-commentary-source (source-file)
  "The commentary source of the verse SOURCE-FILE — #+CAT_COMMENTARY of
the Sanskrit source (for the Tibetan one, of its parallel) — or nil."
  (bcat-commentary--header-path
   (if (equal (bcat-sentence-template-name source-file) "bilingual-tibetan")
       (bcat-sentence-parallel-source source-file)
     source-file)
   "CAT_COMMENTARY"))

(defun bcat-commentary-lead (commentary-source)
  "The Sanskrit verse source COMMENTARY-SOURCE comments on
\(#+CAT_COMMENTS_ON), or nil."
  (bcat-commentary--header-path commentary-source "CAT_COMMENTS_ON"))

(defun bcat-commentary--section-labels (file)
  "The Section labels of FILE's sentences, in order, each once."
  (delete-dups (delq nil (mapcar (lambda (s) (plist-get s :section-label))
                                 (bcat-sentence--sentences file)))))

(defun bcat-commentary-section (label lead commentary-source)
  "The Section of COMMENTARY-SOURCE that comments on the verse Section
LABEL of LEAD: the one of that name, else the nearest earlier one —
Sthiramati's IX.2 covers IX.3 —, nil before his first."
  (let ((own (bcat-commentary--section-labels commentary-source)))
    (if (member label own)
        label
      (let* ((order (bcat-commentary--section-labels lead))
             (pos (cl-position label order :test #'equal)))
        (and pos (cl-find-if (lambda (l) (member l own))
                             (reverse (cl-subseq order 0 pos))))))))

(defun bcat-commentary--covered (section lead commentary-source)
  "The verse Sections of LEAD that the commentary SECTION covers: it and
the following ones without a commentary Section of their own."
  (let* ((own (bcat-commentary--section-labels commentary-source))
         (order (bcat-commentary--section-labels lead))
         (pos (cl-position section order :test #'equal)))
    (when pos
      (cons section (cl-loop for l in (nthcdr (1+ pos) order)
                             until (member l own) collect l)))))

(defun bcat-commentary--verses (section lead commentary-source)
  "The verse sentences (plists) of LEAD in the Sections the commentary
SECTION covers."
  (let ((covered (bcat-commentary--covered section lead commentary-source)))
    (cl-remove-if-not (lambda (s) (and (plist-get s :verse)
                                       (member (plist-get s :section-label) covered)))
                      (bcat-sentence--sentences lead))))

(defun bcat-commentary--sentences (section commentary-source)
  "The sentences of COMMENTARY-SOURCE in SECTION."
  (cl-remove-if-not (lambda (s) (equal section (plist-get s :section-label)))
                    (bcat-sentence--sentences commentary-source)))

(defun bcat-commentary-file (section commentary-source)
  "The commentary document of SECTION: analysis/comm-IX.16-savbh.org
beside COMMENTARY-SOURCE."
  (expand-file-name
   (format "comm-%s-%s.org"
           (replace-regexp-in-string
            "[^A-Za-z0-9.-]+" "-"
            (replace-regexp-in-string "\\`Section[ \t]+" "" section))
           (tibetan-analysis-make-short-name commentary-source))
   (bcat-sentence--analysis-folder commentary-source)))

(defun bcat-commentary--rel (file from)
  "FILE relative to the folder of the file FROM."
  (file-relative-name file (file-name-directory (expand-file-name from))))

;; ----------------------------------------------------------------------------
;; The verse in the commentary, the lemmata
;; ----------------------------------------------------------------------------

(defun bcat-commentary--tibetan-verses (verses lead)
  "The Tibetan verse sentences (plists) that go with the Sanskrit VERSES
of LEAD."
  (delq nil (mapcar (lambda (v) (bcat-sentence--parallel-sentence (plist-get v :sent-num) lead))
                    verses)))

(defun bcat-commentary--padas (verses lead)
  "The Tibetan pādas ((NUM . WYLIE) …) of the Sanskrit VERSES of LEAD."
  (cl-loop for tv in (bcat-commentary--tibetan-verses verses lead)
           append (mapcar (lambda (g) (cons (car g) (bcat-sentence--wylie (cdr g))))
                          (plist-get tv :segs))))

(defun bcat-commentary--tibetan-bhasya (verses lead)
  "The Tibetan bhāṣya of the Sanskrit VERSES of LEAD, Wylie strings."
  (let ((parallel (bcat-sentence-parallel-source lead)))
    (cl-loop for tv in (bcat-commentary--tibetan-verses verses lead)
             append (mapcar (lambda (g) (bcat-sentence--wylie (cdr g)))
                            (bcat-sentence--bhasya-segs tv parallel)))))

(defun bcat-commentary-lemmata-for (section commentary-source)
  "The lemmata the tool finds in the commentary SECTION:
\((SEG PADA . READINGS) …), see `bcat-commentary-lemmata'."
  (let* ((lead (bcat-commentary-lead commentary-source))
         (verses (bcat-commentary--verses section lead commentary-source)))
    (bcat-commentary-lemmata
     (cl-loop for s in (bcat-commentary--sentences section commentary-source)
              append (mapcar (lambda (g) (cons (car g) (bcat-sentence--wylie (cdr g))))
                             (plist-get s :segs)))
     (bcat-commentary--padas verses lead)
     (bcat-commentary--tibetan-bhasya verses lead))))

(defun bcat-commentary--pada-file (pada verses lead)
  "The Tibetan verse file that holds the pāda PADA of the Sanskrit
VERSES of LEAD, and the Sanskrit segment it goes with when that file
already says so („Segment 140 · Skt. 92“): (FILE . SKT-SEG), or nil."
  (cl-loop for v in verses
           for tv = (bcat-sentence--parallel-sentence (plist-get v :sent-num) lead)
           when (assq pada (plist-get tv :segs))
           return (let ((file (bcat-sentence-partner-file (plist-get v :sent-num) lead)))
                    (cons file
                          (let ((text (bcat-sentence--text file)))
                            (and (string-match (format "^\\*\\* Segment %d · Skt\\. \\([0-9]+\\)" pada)
                                               text)
                                 (string-to-number (match-string 1 text))))))))

(defun bcat-commentary--lemma-line (hit verses lead doc)
  "The *Lemma:* text of a segment of the commentary document DOC from
HIT (PADA . READINGS): the pāda with a link into its Tibetan verse file
\(and the Sanskrit segment, once known), how the commentator reads it."
  (let* ((pada (car hit))
         (where (bcat-commentary--pada-file pada verses lead))
         (skt-file (let ((v (cl-find-if (lambda (v)
                                          (assq pada (plist-get (bcat-sentence--parallel-sentence
                                                                 (plist-get v :sent-num) lead)
                                                                :segs)))
                                        verses)))
                     (and v (tibetan-sentence--filepath (plist-get v :sent-num)
                                                        (bcat-sentence--analysis-folder lead)
                                                        lead)))))
    (concat "verse, "
            (if (car where)
                (format "[[file:%s::Segment %d][Tib. %d]]" (bcat-commentary--rel (car where) doc) pada pada)
              (format "Tib. %d" pada))
            (if (and (cdr where) skt-file)
                (format " (Skt. [[file:%s::Segment %d][%d]])"
                        (bcat-commentary--rel skt-file doc) (cdr where) (cdr where))
              "")
            " — " (bcat-commentary-reading-text (cdr hit)))))

;; ----------------------------------------------------------------------------
;; The document without AI (C-c u A)
;; ----------------------------------------------------------------------------

(defun bcat-commentary--author (commentary-source)
  "The commentator: COMMENTARY-SOURCE's #+TITLE up to the first comma."
  (let ((title (bcat-sentence--header-value commentary-source "TITLE")))
    (string-trim (car (split-string (or title (file-name-base commentary-source)) "[,—]")))))

(defun bcat-commentary-rump-context (section commentary-source &optional keep lemmata)
  "Template fields of the commentary document of SECTION, all without
AI: the verses it covers, Sthiramati sentence by sentence with the
dictionary tables, the lemmata (LEMMATA, default the tool's own,
`bcat-commentary-lemmata-for'); KEEP the fields carried over from an
existing document (user slots, `created')."
  (bcat-sentence--with-source commentary-source
    (let* ((lead (bcat-commentary-lead commentary-source))
           (doc (bcat-commentary-file section commentary-source))
           (verses (bcat-commentary--verses section lead commentary-source))
           (sentences (bcat-commentary--sentences section commentary-source))
           (lemmata (or lemmata (bcat-commentary-lemmata-for section commentary-source)))
           (date (format-time-string "%Y-%m-%d"))
           (name (file-name-nondirectory commentary-source))
           (verse-link
            (mapconcat (lambda (v)
                         (format "[[file:%s][%s]]"
                                 (bcat-commentary--rel
                                  (tibetan-sentence--filepath (plist-get v :sent-num)
                                                              (bcat-sentence--analysis-folder lead) lead)
                                  doc)
                                 (plist-get v :verse)))
                       verses " · ")))
      (append
       keep
       `((title . ,(format "%s zu %s" (bcat-commentary--author commentary-source)
                           (mapconcat (lambda (v) (plist-get v :verse)) verses ", ")))
         (source-link . ,(format "[[file:../%s::*%s][%s / %s]]" name section name section))
         (verse-links . ,verse-link)
         (verse-note . ,(and verses (concat "Verse: " verse-link)))
         (sentence-list . ,(mapconcat (lambda (s) (number-to-string (plist-get s :sent-num)))
                                      sentences ", "))
         (segment-list . ,(mapconcat (lambda (s) (mapconcat (lambda (g) (number-to-string (car g)))
                                                            (plist-get s :segs) ", "))
                                     sentences ", "))
         (created . ,date)
         (last-analyzed . ,date)
         (verse . ,(bcat-sentence--with-source (bcat-sentence-parallel-source lead)
                     (cl-loop for tv in (bcat-commentary--tibetan-verses verses lead)
                              append (mapcar (lambda (g) `((uchen . ,(string-trim (cdr g)))
                                                           (wylie . ,(bcat-sentence--wylie (cdr g)))))
                                             (plist-get tv :segs)))))
         (sentences
          . ,(mapcar
              (lambda (s)
                (let ((segs (mapcar (lambda (g)
                                      (let ((hit (cdr (assq (car g) lemmata))))
                                        (append (bcat-sentence--tibetan-segment g)
                                                `((lemma . ,(and hit (bcat-commentary--lemma-line
                                                                      hit verses lead doc)))))))
                                    (plist-get s :segs))))
                  `((num . ,(plist-get s :sent-num))
                    (segments . ,segs)
                    (glosses . ,(bcat-analysis-glosses-p segs)))))
              sentences)))))))

(defun bcat-commentary-create (section commentary-source &optional keep)
  "Write the commentary document of SECTION (no AI) and return it; KEEP
as in `bcat-commentary-rump-context'."
  (let ((file (bcat-commentary-file section commentary-source)))
    (bcat-sentence--write
     file (bcat-sentence--render "commentary"
                                 (bcat-commentary-rump-context section commentary-source keep)
                                 commentary-source))
    file))

(defun bcat-commentary-ensure-file (sent-num commentary-source)
  "The commentary document that holds sentence SENT-NUM of
COMMENTARY-SOURCE, created (no AI) when missing, never overwritten."
  (let* ((sentence (bcat-sentence--sentence sent-num commentary-source))
         (section (plist-get sentence :section-label)))
    (when section
      (let ((file (bcat-commentary-file section commentary-source)))
        (unless (file-exists-p file)
          (bcat-commentary-create section commentary-source))
        file))))

;; ----------------------------------------------------------------------------
;; Links from the verse files
;; ----------------------------------------------------------------------------

(defun bcat-commentary--doc-lemmata (doc commentary-source)
  "The lemmata a commentary document DOC carries: ((SEG PADA) …) from
its *Lemma:* lines — after C-c u G Claude's, which also catch
quotations in the commentator's own wording."
  (let ((text (bcat-sentence--text doc)) (pos 0) (out '()))
    (ignore commentary-source)
    (while (string-match "^\\*\\* Segment \\([0-9]+\\)$" text pos)
      (let* ((seg (string-to-number (match-string 1 text)))
             (start (match-end 0))
             (end (if (string-match "^\\*\\{1,2\\} " text start) (match-beginning 0) (length text)))
             (block (substring text start end)))
        (when (string-match "^\\*Lemma:\\* verse, \\(?:\\[\\[[^]]*\\]\\[\\)?Tib\\. \\([0-9]+\\)" block)
          (push (list seg (string-to-number (match-string 1 block))) out))
        (setq pos end)))
    (nreverse out)))

(defun bcat-commentary-verse-links (sent-num source-file assignment)
  "The Sthiramati lines of the verse SENT-NUM of the verse SOURCE-FILE
\(either language): ((SEG . LINE) …) for its segments — per group of
pādas that one Sanskrit segment holds (ASSIGNMENT ((TIB . SKT) …)), the
commentary sentences with their lemmata, linked; and the header link
\(:link).  The lemmata come from the commentary document when it
exists (Claude's after C-c u G), else from the tool."
  (let* ((commentary (bcat-commentary-source source-file))
         (tibetan-side (equal (bcat-sentence-template-name source-file) "bilingual-tibetan"))
         (lead (if tibetan-side (bcat-sentence-parallel-source source-file) source-file))
         (skt-num (if tibetan-side
                      (plist-get (bcat-sentence--parallel-sentence sent-num source-file) :sent-num)
                    sent-num))
         (verse (bcat-sentence--sentence skt-num lead))
         (section (and commentary verse
                       (bcat-commentary-section (plist-get verse :section-label) lead commentary))))
    (when section
      (let* ((doc (bcat-commentary-file section commentary))
             (own (tibetan-sentence--filepath sent-num (bcat-sentence--analysis-folder source-file)
                                              source-file))
             (rel (bcat-commentary--rel doc own))
             (sent-of (lambda (seg)
                        (plist-get (cl-find-if (lambda (s) (assq seg (plist-get s :segs)))
                                               (bcat-commentary--sentences section commentary))
                                   :sent-num)))
             (lemmata (if (file-exists-p doc)
                          (bcat-commentary--doc-lemmata doc commentary)
                        (mapcar (lambda (l) (list (car l) (cadr l)))
                                (bcat-commentary-lemmata-for section commentary))))
             (line (lambda (skt)
                     (let ((sents (sort (delete-dups
                                         (delq nil (mapcar (lambda (l)
                                                             (and (eql skt (cdr (assq (cadr l) assignment)))
                                                                  (funcall sent-of (car l))))
                                                           lemmata)))
                                        #'<)))
                       (and sents
                            (concat "*" (bcat-commentary--author commentary) ":* lemmata in "
                                    (mapconcat (lambda (n) (format "[[file:%s::*Sentence %d][%d]]" rel n n))
                                               sents ", ")))))))
        (cons (cons :link (format "[[file:%s][%s]]" rel
                                  (or (bcat-sentence--header-value doc "TITLE")
                                      (format "%s zu %s" (bcat-commentary--author commentary)
                                              (mapconcat (lambda (v) (plist-get v :verse))
                                                         (bcat-commentary--verses section lead commentary)
                                                         ", ")))))
              (delq nil
                    (mapcar (lambda (seg)
                              (let* ((skt (if tibetan-side (cdr (assq seg assignment)) seg))
                                     (text (and skt (funcall line skt))))
                                (and text (cons seg text))))
                            (mapcar #'car (plist-get (bcat-sentence--sentence sent-num source-file)
                                                     :segs)))))))))

;; ----------------------------------------------------------------------------
;; C-c u G: the commentary analysed (Carsten 30.10.: „das volle Programm“)
;; ----------------------------------------------------------------------------

(defcustom bcat-commentary-chunk-segments 30
  "Most segments of the commentary in one Claude request.  A longer
commentary (MSA IX.8: 72 sentences) goes in parts, cut at sentence
boundaries — the whole Tibetan analysis of 30 prose segments stays well
within `tibetan-claude-max-tokens'."
  :type 'integer
  :group 'buddhist-cat)

(defun bcat-commentary-system-prompt (target)
  "The system prompt of the commentary request for the target language
code TARGET — the segment rules of the Tibetan analysis, the lemma of
each segment, the grammar of each sentence.  Constant per document."
  (let ((lang (bcat-analysis--language target)))
    (format "You prepare a passage of a Tibetan commentary on a verse for a university reading class that reads the verse first, then the commentary, sentence by sentence. Work segment by segment. Answer with ONE JSON object and nothing else (no prose, no code fence):

{\"segments\": [{\"num\": <segment number>, \"lemma\": <verse line number or 0>, \"words\": [{\"wylie\": \"…\", \"gloss\": \"…\", \"function\": \"…\"}], \"translation\": \"…\", \"references\": [{\"name\": \"…\", \"text\": \"…\"}], \"shift\": \"…\", \"structure\": \"…\", \"note\": \"…\", \"concepts\": \"…\"}], \"sentences\": [{\"num\": <sentence number>, \"grammar\": \"…\"}]}

Rules:
- segments: one entry per given segment, same numbers, same order.
- lemma: when the segment quotes the verse — the commentator's lemma, also in his own wording or word order or only in part — the number in square brackets of the verse line it quotes (e.g. 459 for [459]); 0 when it quotes none.  A quotation of the prose commentary (bhāṣya) is no lemma.
%s
- In translations, give a quoted verse line in quotation marks — never a verse line's number (that belongs in lemma).
- sentences: one entry per given sentence; grammar: its syntax in one or two sentences — clauses and their linkage, the quotation markers (zhes bya ba …).
- JSON escaping: only \\\" and \\\\ — never a backslash before an apostrophe.
- Write glosses, notes and explanations in %s. Never mention yourself."
            (bcat-analysis--tibetan-segment-rules lang) lang)))

(defun bcat-commentary-user-prompt (verse sentences refs &optional context)
  "The user prompt of a commentary request: VERSE ((NUM . WYLIE) …) the
Tibetan verse lines, SENTENCES ((NUM (SEG TIBETAN WYLIE) …) …) the
commentary, REFS ((NAME . TEXT) …), CONTEXT the source's
#+TIBETAN_CLAUDE_CONTEXT lines."
  (concat
   (if context (concat (mapconcat #'identity context "\n") "\n\n") "")
   "The verse (Tibetan, by line):\n"
   (mapconcat (lambda (l) (format "[%d] %s" (car l) (cdr l))) verse "\n")
   "\n\nThe commentary — sentences and segments:\n"
   (mapconcat (lambda (s)
                (concat (format "Sentence %d\n" (car s))
                        (mapconcat (lambda (g) (bcat-analysis--segment-entry (nth 0 g) (nth 1 g) (nth 2 g)))
                                   (cdr s) "\n")))
              sentences "\n")
   (bcat-analysis--references-block refs)
   "\n\nReturn the JSON object now."))

(defun bcat-commentary--chunks (sentences)
  "SENTENCES (plists) in parts of at most `bcat-commentary-chunk-segments'
segments, cut between sentences."
  (let ((chunks '()) (chunk '()) (n 0))
    (dolist (s sentences)
      (let ((k (length (plist-get s :segs))))
        (when (and chunk (> (+ n k) bcat-commentary-chunk-segments))
          (push (nreverse chunk) chunks)
          (setq chunk nil n 0))
        (push s chunk)
        (setq n (+ n k))))
    (when chunk (push (nreverse chunk) chunks))
    (nreverse chunks)))

(defun bcat-commentary-incomplete-p (section commentary-source)
  "Non-nil while the commentary document of SECTION is missing or not
fully analysed."
  (let ((doc (bcat-commentary-file section commentary-source)))
    (or (not (file-exists-p doc))
        (bcat-sentence-fetch-state
         doc (cl-loop for s in (bcat-commentary--sentences section commentary-source)
                      append (mapcar #'car (plist-get s :segs))))
        nil)))

(defvar bcat-commentary--runs (make-hash-table :test #'equal)
  "Commentary document → the plist of its running C-c u G.")

(defun bcat-commentary--status (run text)
  "Show TEXT for the commentary RUN on its own status line (below its
verse, `bcat-commentary-fetch'), where its PDF and upload write too."
  (bcat-status-step (plist-get run :doc) :commentary text))

(defun bcat-commentary--merged (run)
  "The answers of RUN that have landed as one: their segments and
sentences in the order of the parts."
  (let ((answers (mapcar #'cdr (sort (copy-sequence (plist-get run :answers))
                                     (lambda (a b) (< (car a) (car b)))))))
    `((segments . ,(cl-mapcan (lambda (a) (copy-sequence (alist-get 'segments a))) answers))
      (sentences . ,(cl-mapcan (lambda (a) (copy-sequence (alist-get 'sentences a))) answers)))))

(defun bcat-commentary--lemma-pada (value padas)
  "The pāda of PADAS ((NUM . WYLIE) …) that Claude's `lemma' VALUE names:
its number (459), its place in the verse (2 → the second pāda) or a
text naming either („[460]“, „line 2“); nil for 0 or nothing.  30.10.,
IX.60: Claude named every quotation in the translations („verse line 2
[460]“), and none of them came through the field."
  (let ((n (cond ((integerp value) value)
                 ((and (stringp value) (string-match "[0-9]+" value))
                  (string-to-number (match-string 0 value))))))
    (cond ((null n) nil)
          ((assq n padas) n)
          ((<= 1 n (length padas)) (car (nth (1- n) padas))))))

(defun bcat-commentary--lemmata (section commentary-source parsed)
  "The lemmata of SECTION: Claude's from PARSED where it names a verse
line, else the tool's — with the readings where the segment gives the
line's wording (else `free')."
  (let* ((own (bcat-commentary-lemmata-for section commentary-source))
         (lead (bcat-commentary-lead commentary-source))
         (padas (bcat-commentary--padas (bcat-commentary--verses section lead commentary-source) lead))
         (wylie (cl-loop for s in (bcat-commentary--sentences section commentary-source)
                         append (mapcar (lambda (g) (cons (car g) (bcat-sentence--wylie (cdr g))))
                                        (plist-get s :segs))))
         (claude (delq nil (mapcar (lambda (c)
                                     (let ((pada (bcat-commentary--lemma-pada (alist-get 'lemma c) padas)))
                                       (and pada (cons (bcat-analysis--num c) pada))))
                                   (alist-get 'segments parsed)))))
    (append
     (mapcar (lambda (c)
               (let* ((seg (car c)) (pada (cdr c))
                      (mine (cdr (assq seg own))))
                 (cons seg
                       (cond ((eql pada (car mine)) mine)
                             (t (let ((strict (bcat-commentary-lemma (cdr (assq seg wylie))
                                                                     (list (assq pada padas)))))
                                  (if strict strict (cons pada 'free))))))))
             claude)
     (cl-remove-if (lambda (l) (assq (car l) claude)) own))))

(defun bcat-commentary--context (base parsed refs)
  "BASE (`bcat-commentary-rump-context') completed from PARSED: per
sentence its segments as in the Tibetan analysis, its translation (the
segment parts) and grammar; the whole translation."
  (let* ((by-num (mapcar (lambda (s) (cons (bcat-analysis--num s) s)) (alist-get 'sentences parsed)))
         (part (lambda (seg) (let ((c (cl-find (alist-get 'num seg) (alist-get 'segments parsed)
                                               :key #'bcat-analysis--num)))
                               (and c (bcat-analysis--field c 'translation)))))
         (sentences
          (mapcar (lambda (sent)
                    (let* ((segs (alist-get 'segments
                                            (bcat-analysis-context `((segments . ,(alist-get 'segments sent)))
                                                                   parsed refs)))
                           (parts (delq nil (mapcar (lambda (g) (let ((p (funcall part g)))
                                                                  (and p (not (string-empty-p p)) p)))
                                                    segs))))
                      (append `((segments . ,segs)
                                (glosses . ,(bcat-analysis-glosses-p segs))
                                (translation . ,(string-join parts " "))
                                (grammar . ,(bcat-analysis--field
                                             (cdr (assoc (alist-get 'num sent) by-num)) 'grammar)))
                              sent)))
                  (alist-get 'sentences base))))
    (append `((sentences . ,sentences)
              (translation-all . ,(mapconcat (lambda (s) (alist-get 'translation s))
                                             (cl-remove-if (lambda (s) (string-empty-p (alist-get 'translation s)))
                                                           sentences)
                                             " ")))
            base)))

(defun bcat-commentary--write (run)
  "Write the commentary document of RUN from the answers landed so far."
  (let* ((doc (plist-get run :doc))
         (section (plist-get run :section))
         (source (plist-get run :source))
         (parsed (bcat-commentary--merged run)))
    (bcat-sentence--save-visit doc)
    (let ((base (bcat-commentary-rump-context
                 section source
                 (cons (cons 'last-analyzed (format-time-string "%Y-%m-%d")) (bcat-sentence--keep doc))
                 (bcat-commentary--lemmata section source parsed))))
      (bcat-sentence--write doc (bcat-sentence--render "commentary"
                                                       (bcat-commentary--context base parsed (plist-get run :refs))
                                                       source)))))

(defun bcat-commentary--verse-assignment (verse-num lead)
  "The Tibetan pādas' Sanskrit segments of the verse VERSE-NUM of LEAD,
((TIB . SKT) …), as its Tibetan file says („Segment 140 · Skt. 92“)."
  (let ((text (bcat-sentence--text (bcat-sentence-partner-file verse-num lead))) (pos 0) (out '()))
    (while (string-match "^\\*\\* Segment \\([0-9]+\\) · Skt\\. \\([0-9]+\\)" text pos)
      (push (cons (string-to-number (match-string 1 text)) (string-to-number (match-string 2 text))) out)
      (setq pos (match-end 0)))
    (nreverse out)))

(defun bcat-commentary--patch-links (file sent-num source-file assignment)
  "Bring the commentary lines of the analysed verse FILE (sentence
SENT-NUM of SOURCE-FILE) up to date — before each segment's ⟦N⟧
translation; a file not yet analysed is left alone."
  (let ((text (bcat-sentence--text file)))
    (when (string-match-p "^⟦[0-9]+⟧ " text)
      (let* ((links (bcat-commentary-verse-links sent-num source-file assignment))
             (name (regexp-quote (bcat-commentary--author (bcat-commentary-source source-file)))))
        (with-temp-buffer
          (insert text)
          (goto-char (point-min))
          (while (re-search-forward (format "^\\*%s:\\* lemmata in .*\n\n" name) nil t)
            (replace-match ""))
          (dolist (l links)
            (when (integerp (car l))
              (goto-char (point-min))
              (when (re-search-forward (format "^⟦%d⟧ " (car l)) nil t)
                (beginning-of-line)
                (insert (cdr l) "\n\n"))))
          (unless (equal (buffer-string) text)
            (bcat-sentence--save-visit file)
            (bcat-sentence--write file (buffer-string))))))))

(defun bcat-commentary--finish (run)
  "All parts of RUN landed: status; when none failed, the verse files
get their links and the document goes to the reMarkable (C-c u H)."
  (let ((failed (plist-get run :failed)))
    (if failed
        (bcat-commentary--status run (format "%d von %d Teil(en) nicht lesbar — erneut C-c u G"
                                             (length failed) (plist-get run :chunks)))
      (bcat-commentary--status run "✓")
      (let* ((source (plist-get run :source))
             (lead (bcat-commentary-lead source))
             (parallel (bcat-sentence-parallel-source lead)))
        (dolist (v (bcat-commentary--verses (plist-get run :section) lead source))
          (let* ((n (plist-get v :sent-num))
                 (assignment (bcat-commentary--verse-assignment n lead))
                 (other (bcat-sentence--parallel-sentence n lead)))
            (bcat-commentary--patch-links
             (tibetan-sentence--filepath n (bcat-sentence--analysis-folder lead) lead) n lead assignment)
            (when other
              (bcat-commentary--patch-links (bcat-sentence-partner-file n lead)
                                            (plist-get other :sent-num) parallel assignment)))))
      (run-hook-with-args 'bcat-sentence-after-fetch-functions (plist-get run :doc)))))

(defun bcat-commentary--land (doc k response)
  "Part K of the commentary of DOC answered with RESPONSE."
  (let ((run (gethash doc bcat-commentary--runs)))
    (when run
      (let* ((read (bcat-analysis-read response)))
        (if (car read)
            (setq run (plist-put run :answers (cons (cons k (car read)) (plist-get run :answers))))
          (let ((path (bcat-analysis-keep-response doc response)))
            (message "C-c u G: Kommentar %s, Teil %d nicht lesbar (%s) — Antwort: %s"
                     (file-name-nondirectory doc) k (cdr read) path))
          (setq run (plist-put run :failed (cons k (plist-get run :failed)))))
        (puthash doc run bcat-commentary--runs)
        (when (car read) (bcat-commentary--write run))
        (let ((done (+ (length (plist-get run :answers)) (length (plist-get run :failed)))))
          (if (< done (plist-get run :chunks))
              (bcat-commentary--status run (format "%d/%d Teile" done (plist-get run :chunks)))
            (remhash doc bcat-commentary--runs)
            (bcat-commentary--finish run)))))))

(defun bcat-commentary--submit (run chunks)
  "Send the commentary of RUN in CHUNKS, one Claude request each."
  (require 'tibetan-translation-discussion)
  (let* ((source (plist-get run :source))
         (doc (plist-get run :doc))
         (meta (ignore-errors (tibetan-analysis--read-source-metadata source)))
         (lead (bcat-commentary-lead source))
         (verse (bcat-commentary--padas (bcat-commentary--verses (plist-get run :section) lead source) lead))
         (dm (plist-get run :dm))
         (k 0))
    (bcat-commentary--status run (format "angefragt (%d Teil%s)" (length chunks)
                                         (if (= 1 (length chunks)) "" "e")))
    (dolist (chunk chunks)
      (let* ((k (cl-incf k))
             (sentences (mapcar (lambda (s)
                                  (cons (plist-get s :sent-num)
                                        (mapcar (lambda (g) (list (car g) (cdr g) (bcat-sentence--wylie (cdr g))))
                                                (plist-get s :segs))))
                                chunk))
             (texts (delq nil (mapcar (lambda (s) (let ((tr (cdr (assq (plist-get s :sent-num) dm))))
                                                    (and tr (format "[%d] %s" (plist-get s :sent-num) tr))))
                                      chunk)))
             (refs (and texts (list (cons "DharmaMitra" (string-join texts "\n"))))))
        (tibetan-discussion--submit
         (format "bcat-commentary-%d" k) doc
         (lambda ()
           (cons (bcat-commentary-system-prompt (or (plist-get meta :target-lang) "en"))
                 (bcat-commentary-user-prompt verse sentences refs (plist-get meta :claude-context))))
         (lambda (_f response) (bcat-commentary--land doc k response)))))))

(defun bcat-commentary--dharmamitra (run sentences)
  "DharmaMitra on each of SENTENCES of RUN, one after the other with the
usual pause; then the Claude requests."
  (if (or (null sentences)
          ;; The daily limit reached: no more waiting for answers that
          ;; cannot come (30.10.).
          (and (fboundp 'tibetan-dharmamitra-api-limit-reached-p)
               (tibetan-dharmamitra-api-limit-reached-p)
               (progn (bcat-commentary--status run "DharmaMitra: Tageslimit erreicht") t)))
      (bcat-commentary--submit run (plist-get run :chunk-list))
    (let* ((s (car sentences))
           (tr (bcat-sentence--dharmamitra (plist-get run :doc) s))
           (total (length (plist-get run :chunk-list-sentences))))
      (when tr
        (setq run (plist-put run :dm (cons (cons (plist-get s :sent-num) tr) (plist-get run :dm))))
        (puthash (plist-get run :doc) run bcat-commentary--runs))
      (bcat-commentary--status run (format "DharmaMitra %d/%d" (- total (length (cdr sentences))) total))
      (run-at-time (if (boundp 'tibetan-dharmamitra-sentence-request-delay)
                       tibetan-dharmamitra-sentence-request-delay 6.5)
                   nil #'bcat-commentary--dharmamitra run (cdr sentences)))))

(defun bcat-commentary-fetch (section commentary-source &optional status-file)
  "C-c u G for the commentary of SECTION of COMMENTARY-SOURCE: the
document (created when missing), DharmaMitra per sentence, then Claude
— the whole commentary at once, in parts when long; each part lands as
it comes, and when all have, the verse files get their links and the
document goes to the reMarkable.  STATUS-FILE: the verse file below
whose status line the commentary gets its own (progress, PDF, upload).
Nothing while one runs already."
  (let ((doc (bcat-commentary-file section commentary-source)))
    (unless (gethash doc bcat-commentary--runs)
      (unless (file-exists-p doc)
        (bcat-commentary-create section commentary-source))
      (let* ((sentences (bcat-commentary--sentences section commentary-source))
             (run (list :doc doc :section section :source commentary-source
                        :status-file status-file
                        :chunk-list (bcat-commentary--chunks sentences)
                        :chunk-list-sentences sentences
                        :chunks (length (bcat-commentary--chunks sentences))
                        :answers nil :failed nil :dm nil :refs nil)))
        (puthash doc run bcat-commentary--runs)
        (when status-file
          (bcat-status-add doc (or (bcat-sentence--header-value doc "TITLE")
                                   (file-name-base doc))
                           status-file))
        (bcat-commentary--status run "DharmaMitra …")
        (run-at-time 0 nil #'bcat-commentary--dharmamitra run sentences)
        doc))))

;; ----------------------------------------------------------------------------
;; C-c u K: between verse and commentary (Carsten 30.10.: „Links in den
;; Versen und Kommentaren zu den entsprechenden Stellen“)
;; ----------------------------------------------------------------------------

(defun bcat-commentary--link-target (link from)
  "The file and search option of the org LINK „[[file:PATH::OPT][…]]“,
PATH relative to FROM's folder: (FILE . OPT), OPT nil when absent."
  (when (and link (string-match "\\[\\[file:\\([^]:]+\\)\\(?:::\\([^]]+\\)\\)?\\]" link))
    (cons (expand-file-name (match-string 1 link) (file-name-directory from))
          (match-string 2 link))))

(defun bcat-commentary--block-at-point ()
  "The `** Segment N' block point is in as (N . TEXT), or nil."
  (save-excursion
    (end-of-line)
    (when (and (re-search-backward "^\\*\\{1,2\\} " nil t)
               (looking-at "^\\*\\* Segment \\([0-9]+\\)\\b"))
      (let ((n (string-to-number (match-string 1)))
            (start (point)))
        (forward-line 1)
        (cons n (buffer-substring-no-properties
                 start (if (re-search-forward "^\\*\\{1,2\\} " nil t) (match-beginning 0) (point-max))))))))

(defun bcat-commentary--visit-lemma (doc seg tibetan)
  "Show the commentary document DOC at the first segment whose lemma is
the verse segment SEG (TIBETAN: a Tibetan pāda, else a Sanskrit
segment), or at its top."
  (bcat-parallel--visit doc nil)
  (when (and seg
             (re-search-forward
              (if tibetan
                  (format "^\\*Lemma:\\* verse, .*\\]\\[Tib\\. %d\\]\\]" seg)
                (format "^\\*Lemma:\\* verse, .*(Skt\\. \\[\\[[^]]*\\]\\[%d\\]\\])" seg))
              nil t)
             (re-search-backward "^\\*\\* Segment " nil t))
    (beginning-of-line)
    (when (fboundp 'org-reveal) (ignore-errors (org-reveal)))))

(defun bcat-commentary--source-jump (file)
  "C-c u K in the source FILE: verse ↔ the commentary's Section."
  (let* ((place (or (bcat-parallel--source-place) (user-error "Kein Satz an Punkt")))
         (label (plist-get (bcat-sentence--sentence (car place) file) :section-label)))
    (if (equal (bcat-sentence-template-name file) "commentary")
        (let* ((lead (bcat-commentary-lead file))
               (verse (car (and label (bcat-commentary--verses label lead file)))))
          (unless verse (user-error "Kein Vers zu %s" (or label "dieser Stelle")))
          (bcat-parallel--visit lead (format "^\\*+ Sentence %d\\b" (plist-get verse :sent-num))))
      (let* ((lead (if (equal (bcat-sentence-template-name file) "bilingual-tibetan")
                       (bcat-sentence-parallel-source file)
                     file))
             (csrc (and lead (bcat-commentary-source lead)))
             (section (and csrc label (bcat-commentary-section label lead csrc))))
        (unless csrc
          (user-error "%s nennt keinen Kommentar (#+CAT_COMMENTARY)" (file-name-nondirectory file)))
        (unless section (user-error "Kein Kommentar zu %s" (or label "dieser Stelle")))
        (bcat-parallel--visit csrc (format "^\\*+ %s$" (regexp-quote section)))))))

;;;###autoload
(defun bcat-commentary-jump ()
  "C-c u K: between a verse and its commentary — the counterpart of
C-c u J (Carsten 30.10.).  In a verse file: the commentary document, at
the first segment that quotes the segment at point.  In the commentary
document: from a segment with a lemma to the Tibetan pāda it quotes,
else to the first verse.  In a source: the other source — from a verse
to the commentary's Section, from the commentary to its verse."
  (interactive)
  (let* ((file (or (buffer-file-name) (user-error "Buffer besucht keine Datei")))
         (template (bcat-sentence-file-p file)))
    (cond
     ((equal template "commentary")
      (let* ((block (bcat-commentary--block-at-point))
             (lemma (and block (string-match "^\\*Lemma:\\* verse, \\(\\[\\[[^]]+\\]\\[Tib\\. [0-9]+\\]\\]\\)"
                                             (cdr block))
                         (bcat-commentary--link-target (match-string 1 (cdr block)) file)))
             (verse (bcat-commentary--link-target (bcat-sentence--header-value file "CAT_VERSE") file))
             (target (or lemma verse (user-error "%s nennt keinen Vers" (file-name-nondirectory file)))))
        (unless (file-exists-p (car target))
          (user-error "%s fehlt noch — C-c u G" (file-name-nondirectory (car target))))
        (bcat-parallel--visit (car target)
                              (and lemma (cdr lemma) (string-match "Segment \\([0-9]+\\)" (cdr lemma))
                                   (format "^\\*\\* Segment %s\\b" (match-string 1 (cdr lemma)))))))
     ((assoc template bcat-sentence--pair-templates)
      (let* ((doc (car (bcat-commentary--link-target
                        (bcat-sentence--header-value file "CAT_COMMENTARY") file))))
        (unless doc
          (user-error "%s hat keinen Kommentar (#+CAT_COMMENTARY)" (file-name-nondirectory file)))
        (unless (file-exists-p doc)
          (user-error "%s fehlt noch — C-c u A oder C-c u G" (file-name-nondirectory doc)))
        (bcat-commentary--visit-lemma doc (car (bcat-commentary--block-at-point))
                                      (equal template "bilingual-tibetan"))))
     ((bcat-sentence-template-name file)
      (bcat-commentary--source-jump file))
     (t (user-error "%s ist weder Vers noch Kommentar" (file-name-nondirectory file))))))

(provide 'bcat-commentary)
;;; bcat-commentary.el ends here
