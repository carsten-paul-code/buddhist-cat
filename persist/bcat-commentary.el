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

(provide 'bcat-commentary)
;;; bcat-commentary.el ends here
