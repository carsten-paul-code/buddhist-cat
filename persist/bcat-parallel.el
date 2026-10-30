;;; bcat-parallel.el --- C-c u J: to the same place in the other language -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;;; Commentary:
;; §5.74, Carsten 30.10.: „Ein gegenseitiges Verlinken in Emacs wäre
;; schön … Mit dem Link kann ich dann aus dem Sanskrit auf denselben
;; Satz im Tibetischen springen.“  A bilingual pair (bcat-sentence.el:
;; a Sanskrit source with #+CAT_TEMPLATE: bilingual and a Tibetan one
;; with bilingual-tibetan, each naming the other in #+CAT_PARALLEL)
;; has two sentence files per sentence.  C-c u J goes from the sentence
;; or segment at point to its counterpart:
;;   - in a sentence file to the partner file, at the segment the
;;     heading names („** Segment 17 · Tib. 40–44“ → Segment 40;
;;     „** Segment 41 · Skt. 17“ → Segment 17);
;;   - in a source to the other source, at the counterpart segment when
;;     the pair's Tibetan file knows the assignment, else at the
;;     sentence.
;; The header link #+CAT_PARALLEL of each sentence file (C-c C-o) opens
;; the partner file too.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'bcat-sentence)

(declare-function tibetan-sentence--filepath "tibetan-sentence-persist" (sent-num &optional folder source-file))
(declare-function tibetan-sentence--sent-id-from-filename "tibetan-sentence-persist" (filepath))
(declare-function tibetan-sentence--source-file-from-analysis "tibetan-sentence-persist" (analysis-file))
(declare-function org-reveal "org" (&optional siblings))

(defconst bcat-parallel--segment-heading-re
  "^\\*\\* Segment \\([0-9]+\\)\\(?: · \\(Tib\\|Skt\\)\\. \\([0-9]+\\)\\)?"
  "A segment heading of a bilingual sentence file: its number, and the
first counterpart segment it names.")

(defun bcat-parallel--segment-at-point ()
  "In a bilingual sentence file: the counterpart segment named by the
segment heading point is under, or nil (point under * Text, * Sentence,
Word Analysis …)."
  (save-excursion
    (end-of-line)
    (when (and (re-search-backward "^\\*\\{1,2\\} " nil t)
               (looking-at bcat-parallel--segment-heading-re)
               (match-string 3))
      (string-to-number (match-string 3)))))

(defun bcat-parallel--source-place ()
  "In a cascade source: (SENT-NUM . SEG-NUM) at point — the sentence and,
when point is in one of its segments, the segment (else nil)."
  (save-excursion
    (end-of-line)
    (let ((seg (and (re-search-backward "^\\*+ \\(Segment\\|Sentence\\) \\([0-9]+\\)\\b" nil t)
                    (equal (match-string 1) "Segment")
                    (string-to-number (match-string 2)))))
      (when (or (not seg)
                (re-search-backward "^\\*+ Sentence \\([0-9]+\\)\\b" nil t))
        (when (looking-at "^\\*+ Sentence \\([0-9]+\\)\\b")
          (cons (string-to-number (match-string 1)) seg))))))

(defun bcat-parallel--assignment (sent-num source-file)
  "The assignment of the pair's sentence SENT-NUM as ((TIB . SKT) …), read
from the headings of its Tibetan sentence file („Segment 78 · Skt. 23“);
nil before C-c u G."
  (let ((tfile (if (equal (bcat-sentence-template-name source-file) "bilingual-tibetan")
                   (tibetan-sentence--filepath
                    sent-num (file-name-as-directory
                              (expand-file-name "analysis" (file-name-directory source-file)))
                    source-file)
                 (bcat-sentence-partner-file sent-num source-file))))
    (when (and tfile (file-readable-p tfile))
      (let ((text (bcat-sentence--text tfile)) (pos 0) (out '()))
        (while (string-match "^\\*\\* Segment \\([0-9]+\\) · Skt\\. \\([0-9]+\\)" text pos)
          (push (cons (string-to-number (match-string 1 text))
                      (string-to-number (match-string 2 text)))
                out)
          (setq pos (match-end 0)))
        (nreverse out)))))

(defun bcat-parallel--counterpart (seg sent-num source-file)
  "The segment of the other language for the segment SEG of SOURCE-FILE's
sentence SENT-NUM: for a Sanskrit segment the first Tibetan one that
translates it, for a Tibetan segment its Sanskrit one; nil when the
assignment is not known."
  (let ((assignment (bcat-parallel--assignment sent-num source-file)))
    (if (equal (bcat-sentence-template-name source-file) "bilingual-tibetan")
        (cdr (assq seg assignment))
      (car (rassq seg assignment)))))

(defun bcat-parallel--visit (file heading-re)
  "Show FILE — in its window when it is visible (as C-c u j) — with point
on the first heading matching HEADING-RE, or at the top."
  (pop-to-buffer (find-file-noselect file)
                 '((display-buffer-reuse-window
                    display-buffer-use-some-window
                    display-buffer-same-window)))
  (goto-char (point-min))
  (when (and heading-re (re-search-forward heading-re nil t))
    (beginning-of-line)
    (when (fboundp 'org-reveal) (ignore-errors (org-reveal)))))

;;;###autoload
(defun bcat-parallel-jump ()
  "C-c u J: to the same place in the other language of a bilingual pair
\(Carsten 30.10.).  In a sentence file: the partner file, at the
segment the heading at point names (Sanskrit → its first Tibetan
segment, Tibetan → its Sanskrit one), else at its top.  In a source:
the other source, at the counterpart segment when the pair's Tibetan
file knows the assignment (after C-c u G), else at the sentence."
  (interactive)
  (let ((file (buffer-file-name)))
    (unless file
      (user-error "Buffer besucht keine Datei"))
    (cond
     ((assoc (bcat-sentence-file-p file) bcat-sentence--pair-templates)
      (let* ((source (tibetan-sentence--source-file-from-analysis file))
             (partner (and source
                           (bcat-sentence-partner-file
                            (tibetan-sentence--sent-id-from-filename file) source)))
             (seg (bcat-parallel--segment-at-point)))
        (unless (and partner (file-exists-p partner))
          (user-error "Gegenstück %s fehlt noch — C-c u G legt beide Dateien an"
                      (if partner (file-name-nondirectory partner) "")))
        (bcat-parallel--visit partner
                              (and seg (format "^\\*\\* Segment %d\\b" seg)))))
     ((bcat-sentence-parallel-source file)
      (let* ((place (or (bcat-parallel--source-place)
                        (user-error "Kein Satz an Punkt")))
             (parallel (bcat-sentence-parallel-source file))
             (seg (and (cdr place) (bcat-parallel--counterpart (cdr place) (car place) file))))
        (unless (file-exists-p parallel)
          (user-error "Parallelquelle fehlt: %s" parallel))
        (bcat-parallel--visit parallel
                              (if seg
                                  (format "^\\*+ Segment %d\\b" seg)
                                (format "^\\*+ Sentence %d\\b" (car place))))))
     (t
      (user-error "Kein zweisprachiges Gegenstück: %s trägt kein #+CAT_PARALLEL"
                  (file-name-nondirectory file))))))

(provide 'bcat-parallel)
;;; bcat-parallel.el ends here
