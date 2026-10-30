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

(declare-function tibetan-analysis-make-short-name "tibetan-analysis-persist" (source-file))
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

(provide 'bcat-commentary)
;;; bcat-commentary.el ends here
