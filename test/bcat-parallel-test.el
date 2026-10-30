;;; bcat-parallel-test.el --- Tests for C-c u J, the jump to the other language  -*- lexical-binding: t; -*-

;;; Commentary:
;; Carsten 30.10.: „Mit dem Link kann ich dann aus dem Sanskrit auf
;; denselben Satz im Tibetischen springen.“  C-c u J goes from the
;; sentence or segment at point to its counterpart in the other
;; language — between the two sentence files of a bilingual pair and
;; between the two sources.  Fixtures: the pair of bcat-sentence-test
;; (AS sentence 3: Sanskrit 22–23, Tibetan 77–79; 77 → 22, 78–79 → 23).

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'bcat-sentence-test)
(require 'bcat-parallel)

(defun bcat-parallel-test--at (file heading-re)
  "Visit FILE with point on the first line matching HEADING-RE."
  (set-buffer (find-file-noselect file))
  (goto-char (point-min))
  (re-search-forward heading-re)
  (beginning-of-line))

(defun bcat-parallel-test--line ()
  "The line at point in the current buffer."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defmacro bcat-parallel-test--with-fetched-pair (&rest body)
  "The pair of bcat-sentence-test with sentence 3 fetched; FILE and
TFILE its Sanskrit and Tibetan sentence files."
  (declare (indent 0))
  `(bcat-sentence-test--with-pair
     (bcat-sentence-test--with-stubs #'bcat-sentence-test--pair-answers
       (bcat-sentence-test--fetch-n src 3))
     (let ((file (tibetan-sentence--filepath 3 folder src))
           (tfile (tibetan-sentence--filepath 3 tfolder tsrc)))
       (save-window-excursion ,@body))))

(ert-deftest bcat-parallel-jump-sentence-file-to-partner ()
  "From a segment of the Sanskrit sentence file to the first Tibetan
segment Claude assigned to it, in the Tibetan file — and back from a
Tibetan segment to its Sanskrit one."
  (bcat-parallel-test--with-fetched-pair
    (bcat-parallel-test--at file "^\\*\\* Segment 23 ")
    (forward-line 2)
    (bcat-parallel-jump)
    (should (file-equal-p tfile (buffer-file-name)))
    (should (equal "** Segment 78 · Skt. 23" (bcat-parallel-test--line)))
    (bcat-parallel-test--at tfile "^\\*\\* Segment 79 ")
    (bcat-parallel-jump)
    (should (file-equal-p file (buffer-file-name)))
    (should (equal "** Segment 23 · Tib. 78–79" (bcat-parallel-test--line)))))

(ert-deftest bcat-parallel-jump-sentence-file-outside-segments ()
  "Point under * Text (or * Sentence): the partner file opens at its
top — the same sentence."
  (bcat-parallel-test--with-fetched-pair
    (bcat-parallel-test--at file "^\\* Text")
    (forward-line 3)
    (bcat-parallel-jump)
    (should (file-equal-p tfile (buffer-file-name)))
    (should (= (point) (point-min)))))

(ert-deftest bcat-parallel-jump-source-to-source ()
  "In the sources: from a Sanskrit segment to the first Tibetan segment
that translates it (the assignment of the pair's Tibetan file), from a
Tibetan segment to its Sanskrit one."
  (bcat-parallel-test--with-fetched-pair
    (bcat-parallel-test--at src "^\\*\\*\\*\\* Segment 23")
    (forward-line 1)
    (bcat-parallel-jump)
    (should (file-equal-p tsrc (buffer-file-name)))
    (should (equal "**** Segment 78" (bcat-parallel-test--line)))
    (bcat-parallel-test--at tsrc "^\\*\\*\\*\\* Segment 77")
    (bcat-parallel-jump)
    (should (file-equal-p src (buffer-file-name)))
    (should (equal "**** Segment 22" (bcat-parallel-test--line)))))

(ert-deftest bcat-parallel-jump-source-before-fetch ()
  "Before C-c u G the assignment is unknown: the jump goes to the same
sentence in the other source."
  (bcat-sentence-test--with-pair
    (save-window-excursion
      (bcat-parallel-test--at src "^\\*\\*\\*\\* Segment 23")
      (bcat-parallel-jump)
      (should (file-equal-p tsrc (buffer-file-name)))
      (should (equal "*** Sentence 3" (bcat-parallel-test--line))))))

(ert-deftest bcat-parallel-jump-missing-partner ()
  "No partner file yet, or no bilingual file at all: a clear message,
nothing opened."
  (bcat-sentence-test--with-pair
    (save-window-excursion
      (let ((file (tibetan-cascade--create-file 3 (tibetan-cascade--segs-for-sentence src 3) src)))
        (bcat-parallel-test--at file "^\\* Text")
        (should-error (bcat-parallel-jump) :type 'user-error)
        (should (file-equal-p file (buffer-file-name))))))
  (with-temp-buffer
    (should-error (bcat-parallel-jump) :type 'user-error)))

(provide 'bcat-parallel-test)
;;; bcat-parallel-test.el ends here
