;;; bcat-sentence.el --- Template-built sentence files (Buddhist CAT) -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;;; Commentary:
;; §5.74 (Carsten 28.10.2026): sentence files in the structure whole
;; original / segments / sentence, rendered from a template
;; (`bcat-template').  Step 2: C-c u A creates them WITHOUT any AI
;; request — Wylie, dictionary gloss tables, the reference translations
;; at sentence level.  Everything else arrives with C-c u G (step 3);
;; the old renderers never touch such a file (guards in
;; tibetan-cascade.el, keyed on `bcat-sentence-file-p').
;;
;; Templates: "tibetan", "sanskrit" (29.10.) and "bilingual" (30.10.: a
;; Sanskrit source with its Tibetan translation, #+CAT_PARALLEL — after
;; the Sanskrit request a second one for the Tibetan).  When the Tibetan
;; source names the Sanskrit one back with #+CAT_TEMPLATE:
;; bilingual-tibetan, the two form a pair: one C-c u G (in either
;; source) writes the sentence files of both, each linking the other.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'bcat-template)
(require 'bcat-analysis)
(require 'tibetan-sanskrit-script)
(require 'bcat-status)
(require 'bcat-outline)
(require 'bcat-zettel nil t)

(declare-function org-cycle-set-visibility-according-to-property "org-cycle" ())
(declare-function bcat-faces-maybe "bcat-faces" ())
(declare-function tibetan-analysis--cascade-p "tibetan-analysis-claude" (file))
(declare-function tibetan-analysis--resolve-source-lang "tibetan-analysis-claude" (file))
(declare-function tibetan-analysis--read-source-metadata "tibetan-analysis-claude" (source-file))
(declare-function tibetan-to-wylie-fixed "tibetan-wylie" (tibetan-text))
(declare-function tibetan-gloss-table--unit-rows "tibetan-gloss-table" (unit-text &optional vocab-alist))
(declare-function tibetan-gloss-table--format-rows "tibetan-gloss-table" (rows))
(declare-function tibetan-sentence--compute-hash "tibetan-sentence-persist" (concatenated-text))
(declare-function tibetan-cascade--sentence-lopez-section "tibetan-cascade" (source-file sent-num))
(declare-function tibetan-cascade--refs-file "tibetan-cascade" (source-file key))
(declare-function tibetan-cascade--section-ref-bodies "tibetan-cascade" (refs-file secnum))
(declare-function tibetan-cascade--sentence-ref-bodies "tibetan-cascade" (source-file sent-num))

(defvar tibetan-analysis--target-lang)
(defvar tibetan-analysis--source-lang)

(defcustom bcat-sentence-templates t
  "When non-nil, C-c u A creates new sentence files from the template
\(§5.74) — for the languages that have one (Tibetan, Sanskrit).  nil
keeps the old layout; the test suite switches it off for the tests of
the old pipeline."
  :type 'boolean
  :group 'buddhist-cat)

(defun bcat-sentence--header-value (file key)
  "The value of FILE's #+KEY: line (in its head), or nil."
  (when (and file (file-readable-p file))
    (with-temp-buffer
      (insert-file-contents file nil 0 4096)
      (goto-char (point-min))
      (when (re-search-forward (format "^#\\+%s:[ \t]*\\(.*[^ \t\n]\\)" (regexp-quote key))
                               nil t)
        (match-string 1)))))

(defun bcat-sentence--header-values (file key)
  "The values of every #+KEY: line in the head of FILE, in order."
  (when (and file (file-readable-p file))
    (with-temp-buffer
      (insert-file-contents file nil 0 4096)
      (goto-char (point-min))
      (let ((out '()))
        (while (re-search-forward (format "^#\\+%s:[ \t]*\\(.*[^ \t\n]\\)" (regexp-quote key)) nil t)
          (push (match-string 1) out))
        (nreverse out)))))

(defun bcat-sentence-template-name (source-file)
  "The template new sentence files of SOURCE-FILE are built from, or nil
for the old layout.  A source switches it on explicitly with
#+CAT_TEMPLATE: NAME in its header (Carsten 28.10.2026: the reading
classes; the Masterarbeit sources keep the old layout) — only for
cascade sources and while `bcat-sentence-templates' is on."
  (when (and bcat-sentence-templates source-file
             (fboundp 'tibetan-analysis--cascade-p)
             (tibetan-analysis--cascade-p source-file))
    (bcat-sentence--header-value source-file "CAT_TEMPLATE")))

(defun bcat-sentence--sanskrit-p (source-file)
  "Non-nil when SOURCE-FILE is a Sanskrit text with a template: the
Sanskrit one (§5.74, 29.10.) or the bilingual one (Sanskrit with its
Tibetan translation, 30.10.)."
  (member (bcat-sentence-template-name source-file) '("sanskrit" "bilingual")))

(defconst bcat-sentence--pair-templates
  '(("bilingual" . "bilingual-tibetan") ("bilingual-tibetan" . "bilingual"))
  "Each template of a bilingual pair and the one of its partner.")

(defun bcat-sentence-parallel-source (source-file)
  "The other language of the bilingual SOURCE-FILE — its #+CAT_PARALLEL:
resolved relative to it: the Tibetan translation of a Sanskrit source
\(template bilingual), the Sanskrit of a Tibetan one (bilingual-tibetan)
— or nil."
  (when (assoc (bcat-sentence-template-name source-file) bcat-sentence--pair-templates)
    (let ((v (bcat-sentence--header-value source-file "CAT_PARALLEL")))
      (and v (expand-file-name v (file-name-directory (expand-file-name source-file)))))))

(defvar bcat-sentence--sentence-cache (make-hash-table :test #'equal)
  "Source file → (MODTIME . SENTENCES), see `bcat-sentence--sentences'.")

(defun bcat-sentence--sentences (file)
  "The sentences of the cascade source FILE — the plists of
`tibetan-cascade--collect-sentences' (:sent-num :segs :verse
:section-label …) — read again only when FILE changed: the MSA
commentary has 1009 sentences, asked for again and again per verse."
  (let* ((key (file-truename file))
         (mtime (file-attribute-modification-time (file-attributes key)))
         (hit (gethash key bcat-sentence--sentence-cache)))
    (if (and hit (equal (car hit) mtime))
        (cdr hit)
      (let ((sentences (with-temp-buffer
                         (insert-file-contents key)
                         (org-mode)
                         (tibetan-cascade--collect-sentences))))
        (puthash key (cons mtime sentences) bcat-sentence--sentence-cache)
        sentences))))

(defun bcat-sentence--sentence (sent-num file)
  "Sentence SENT-NUM of the source FILE as a plist, or nil."
  (and file (file-readable-p file)
       (cl-find sent-num (bcat-sentence--sentences file)
                :key (lambda (s) (plist-get s :sent-num)))))

(defun bcat-sentence--paired-p (source-file)
  "Non-nil when SOURCE-FILE and its #+CAT_PARALLEL source form a pair —
a bilingual Sanskrit source and a bilingual-tibetan one, each naming the
other (Carsten 30.10.: one C-c u G builds the files of both)."
  (let ((parallel (bcat-sentence-parallel-source source-file)))
    (and parallel
         (file-readable-p parallel)
         (equal (bcat-sentence-template-name parallel)
                (cdr (assoc (bcat-sentence-template-name source-file)
                            bcat-sentence--pair-templates)))
         (let ((back (bcat-sentence-parallel-source parallel)))
           (and back (file-exists-p back) (file-equal-p back source-file))))))

(defun bcat-sentence--parallel-sentence (sent-num source-file)
  "The sentence of the other language that goes with SENT-NUM of
SOURCE-FILE (a plist), or nil: the one with the same :VERSE: when
SENT-NUM is a verse (MSA: the sources share Sections and verses, not
sentence numbers), else the one with the same number (AS)."
  (let ((parallel (bcat-sentence-parallel-source source-file)))
    (when parallel
      (let ((verse (plist-get (bcat-sentence--sentence sent-num source-file) :verse)))
        (if verse
            (cl-find verse (bcat-sentence--sentences parallel)
                     :key (lambda (s) (plist-get s :verse)) :test #'equal)
          (bcat-sentence--sentence sent-num parallel))))))

(defun bcat-sentence--parallel-segs (sent-num source-file)
  "The segments ((NUM . TEXT) …) of the sentence of the other language
that goes with SENT-NUM of SOURCE-FILE, or nil."
  (plist-get (bcat-sentence--parallel-sentence sent-num source-file) :segs))

(defun bcat-sentence--analysis-folder (source-file)
  "The analysis/ folder of SOURCE-FILE."
  (file-name-as-directory
   (expand-file-name "analysis" (file-name-directory (expand-file-name source-file)))))

(defun bcat-sentence-partner-file (sent-num source-file)
  "The sentence file in the other language of the bilingual SOURCE-FILE
that goes with SENT-NUM (in the analysis/ folder of its #+CAT_PARALLEL
source), or nil."
  (let ((parallel (bcat-sentence-parallel-source source-file))
        (other (bcat-sentence--parallel-sentence sent-num source-file)))
    (and parallel other
         (tibetan-sentence--filepath (plist-get other :sent-num)
                                     (bcat-sentence--analysis-folder parallel)
                                     parallel))))

;; ----------------------------------------------------------------------------
;; Verse mode (MSA, Carsten 30.10.): one file per verse, the bhāṣya in it
;; ----------------------------------------------------------------------------

(defun bcat-sentence--verse-mode-p (source-file)
  "Non-nil when the bilingual SOURCE-FILE is read verse by verse — its
sentences carry :VERSE: (MSA: per Section a kārikā and the bhāṣya).
Then each verse has a file; the other sentences of its Section, the
bhāṣya, stand in it."
  (and (bcat-sentence-parallel-source source-file)
       (cl-some (lambda (s) (plist-get s :verse)) (bcat-sentence--sentences source-file))
       t))

(defun bcat-sentence--section-sentences (sentence source-file)
  "The sentences of SOURCE-FILE in the Section of SENTENCE (a plist), in
order."
  (let ((label (plist-get sentence :section-label)))
    (and label
         (cl-remove-if-not (lambda (s) (equal label (plist-get s :section-label)))
                           (bcat-sentence--sentences source-file)))))

(defun bcat-sentence-owner (sent-num source-file)
  "The sentence (a plist) whose file holds SENT-NUM of SOURCE-FILE: in
verse mode the verse of its Section (a bhāṣya sentence has no file of
its own), else the sentence itself; nil when there is none."
  (let ((sentence (bcat-sentence--sentence sent-num source-file)))
    (if (and sentence (not (plist-get sentence :verse))
             (bcat-sentence--verse-mode-p source-file))
        (cl-find-if (lambda (s) (plist-get s :verse))
                    (bcat-sentence--section-sentences sentence source-file))
      sentence)))

(defun bcat-sentence-ensure-file (sent-num source-file)
  "The file that holds sentence SENT-NUM of the template source
SOURCE-FILE, created (no AI) when missing — in verse mode the file of
its verse, in a commentary source the commentary document of its
Section; never overwritten.  nil when SOURCE-FILE has no template or
no such sentence."
  (cond
   ((equal (bcat-sentence-template-name source-file) "commentary")
    (require 'bcat-commentary)
    (bcat-commentary-ensure-file sent-num source-file))
   ((bcat-sentence-template-name source-file)
    (let ((owner (bcat-sentence-owner sent-num source-file)))
      (when owner
        (let ((file (tibetan-sentence--filepath (plist-get owner :sent-num)
                                                (bcat-sentence--analysis-folder source-file)
                                                source-file)))
          (unless (file-exists-p file)
            (bcat-sentence-create-file (plist-get owner :sent-num) (plist-get owner :segs)
                                       source-file file))
          file))))))

(defun bcat-sentence--bhasya-segs (sentence source-file)
  "The segments of the other sentences of SENTENCE's Section in
SOURCE-FILE — the bhāṣya around the verse —, in order."
  (cl-loop for s in (bcat-sentence--section-sentences sentence source-file)
           unless (plist-get s :verse) append (plist-get s :segs)))

(defun bcat-sentence--bhasya-texts (sent-num source-file)
  "The bhāṣya of the verse SENT-NUM of the Sanskrit SOURCE-FILE for the
prompt: (SANSKRIT . TIBETAN), lines as transmitted, the Tibetan in
Wylie; nil when there is none."
  (let* ((own (bcat-sentence--sentence sent-num source-file))
         (other (bcat-sentence--parallel-sentence sent-num source-file))
         (skt (and own (bcat-sentence--bhasya-segs own source-file)))
         (tib (and other (bcat-sentence--bhasya-segs
                          other (bcat-sentence-parallel-source source-file)))))
    (when (or skt tib)
      (cons (mapcar (lambda (s) (string-trim (cdr s))) skt)
            (mapcar (lambda (s) (bcat-sentence--wylie (cdr s))) tib)))))

(defun bcat-sentence--bhasya-fields (sent-num source-file)
  "Verse mode: the bhāṣya of the verse SENT-NUM of SOURCE-FILE in both
languages (Carsten 30.10.: „im Vers als zusätzliche Section (unten) mit
Text und hint“) — the Sanskrit as Devanāgarī over IAST, the Tibetan as
Uchen over Wylie; nil when the verse has none."
  (when (bcat-sentence--verse-mode-p source-file)
    (let* ((own (bcat-sentence--sentence sent-num source-file))
           (other (bcat-sentence--parallel-sentence sent-num source-file))
           (own-b (and own (bcat-sentence--bhasya-segs own source-file)))
           (other-b (and other (bcat-sentence--bhasya-segs
                                other (bcat-sentence-parallel-source source-file))))
           (sanskrit (bcat-sentence--sanskrit-p source-file))
           (skt (if sanskrit own-b other-b))
           (tib (if sanskrit other-b own-b)))
      (when (or skt tib)
        `((bhasya-p . t)
          (bhasya-sanskrit . ,(mapcar #'bcat-sentence--sanskrit-segment skt))
          (bhasya-sanskrit-p . ,(and skt t))
          (bhasya-tibetan . ,(mapcar (lambda (s) `((num . ,(car s))
                                                   (uchen . ,(string-trim (cdr s)))
                                                   (wylie . ,(bcat-sentence--wylie (cdr s)))))
                                     tib))
          (bhasya-tibetan-p . ,(and tib t)))))))

(defun bcat-sentence-file-p (file)
  "The template the sentence FILE (sent-NNN…, or a commentary document
comm-…) was built from (its #+CAT_TEMPLATE:), or nil for a file in the
old layout — and for anything that is no sentence file (a source
carries the line too)."
  (when (and file (string-match-p "\\`\\(?:sent-[0-9]\\|comm-\\)" (file-name-nondirectory file)))
    (bcat-sentence--header-value file "CAT_TEMPLATE")))

;; ----------------------------------------------------------------------------
;; Field values without AI (C-c u A)
;; ----------------------------------------------------------------------------

(defmacro bcat-sentence--with-source (source-file &rest body)
  "Run BODY with the dictionary lookups bound to SOURCE-FILE: its folder
\(the Resources wordlist is found relative to it — headless lesson
2026-06-03), its target and source language."
  (declare (indent 1))
  `(let* ((default-directory (file-name-directory (expand-file-name ,source-file)))
          (tibetan-analysis--target-lang
           (or (ignore-errors
                 (plist-get (tibetan-analysis--read-source-metadata ,source-file)
                            :target-lang))
               (and (boundp 'tibetan-analysis--target-lang)
                    tibetan-analysis--target-lang)))
          (tibetan-analysis--source-lang
           (ignore-errors (tibetan-analysis--resolve-source-lang ,source-file))))
     ,@body))

(defun bcat-sentence--wylie (text)
  "TEXT (Tibetan Unicode) as one Wylie line."
  (string-trim (tibetan-to-wylie-fixed (string-trim text))))

(defun bcat-sentence--dictionary-rows (text)
  "The gloss-table rows of the segment TEXT from the dictionaries (row 1
Wylie with Steinert link, row 2 gloss, row 3 label; \"?\" where nothing
is known — C-c u G replaces it), or nil."
  (ignore-errors (tibetan-gloss-table--unit-rows text nil)))

(defun bcat-sentence--dictionary-table (rows)
  "ROWS (`bcat-sentence--dictionary-rows') as an org table; \"\" for none."
  (if rows (string-trim-right (tibetan-gloss-table--format-rows rows)) ""))

(defun bcat-sentence--dictionary-words (rows)
  "ROWS as word alists (wylie, gloss, function) for
`bcat-analysis-glossed-line': row 1, the gloss row (next to last) and
the label row (last)."
  (when (cdr rows)
    (cl-mapcar (lambda (w g l)
                 `((wylie . ,(bcat-analysis-plain-cell w))
                   (gloss . ,(bcat-analysis-plain-cell g))
                   (function . ,(bcat-analysis-plain-cell l))))
               (car rows) (nth (- (length rows) 2) rows) (car (last rows)))))

(defun bcat-sentence--tibetan-segment (s)
  "Template fields of the Tibetan segment S ((NUM . TEXT)) without AI:
Uchen (Carsten 29.10.: above the Wylie), Wylie, the glossed line and
the table from the dictionaries — its first row the Uchen of each word
\(31.10. abends, Tibetisch V)."
  (let* ((wylie (bcat-sentence--wylie (cdr s)))
         (rows (bcat-sentence--dictionary-rows (cdr s)))
         (uchens (and rows (bcat-analysis--uchen-row
                            (cdr s) (mapcar #'bcat-analysis-plain-cell (car rows))))))
    `((num . ,(car s))
      (uchen . ,(string-trim (cdr s)))
      (wylie . ,wylie)
      (glossed . ,(bcat-analysis-glossed-line wylie (bcat-sentence--dictionary-words rows)))
      (gloss-table . ,(bcat-sentence--dictionary-table (if uchens (cons uchens rows) rows))))))

(defun bcat-sentence--sanskrit-segment (s)
  "Template fields of the Sanskrit segment S ((NUM . TEXT)) without AI:
the line as transmitted, its Devanāgarī, and as padapāṭha the line as
transmitted too — C-c u G resolves the sandhis."
  (let ((samhita (string-trim (cdr s))))
    `((num . ,(car s))
      (samhita . ,samhita)
      (devanagari . ,(tibetan-sanskrit-script-iast-to-devanagari samhita))
      (padapatha . ,samhita))))

(defun bcat-sentence--parallel-fields (sent-num source-file)
  "Fields of the other language of the bilingual sentence SENT-NUM
\(Carsten 30.10.): a link — in a pair to the partner's sentence file
\(„mit dem Link kann ich aus dem Sanskrit auf denselben Satz im
Tibetischen springen“), else to the other source's sentence —, its
segment numbers and its segments: the Tibetan ones for a Sanskrit
source (looked up with the Tibetan source's dictionaries), the Sanskrit
ones for the Tibetan file of a pair; nil when the other source lacks
the sentence."
  (let* ((parallel (bcat-sentence-parallel-source source-file))
         (segs (bcat-sentence--parallel-segs sent-num source-file)))
    (when segs
      (let* ((own-folder (expand-file-name "analysis/" (file-name-directory
                                                        (expand-file-name source-file))))
             (partner (and (bcat-sentence--paired-p source-file)
                           (bcat-sentence-partner-file sent-num source-file))))
        (append
         `((parallel-link
            . ,(if partner
                   (format "[[file:%s][%s]]" (file-relative-name partner own-folder)
                           (file-name-nondirectory partner))
                 (format "[[file:%s::*Sentence %d][%s / Sentence %d]]"
                         (file-relative-name parallel own-folder)
                         sent-num (file-name-nondirectory parallel) sent-num)))
           (parallel-segment-list . ,(mapconcat (lambda (s) (number-to-string (car s))) segs ", ")))
         (if (equal (bcat-sentence-template-name source-file) "bilingual")
             (let ((tibetan (bcat-sentence--with-source parallel
                              (mapcar #'bcat-sentence--tibetan-segment segs))))
               `((tibetan . ,tibetan)
                 (tibetan-p . t)
                 (tibetan-glosses . ,(bcat-analysis-glosses-p tibetan))))
           `((sanskrit . ,(mapcar #'bcat-sentence--sanskrit-segment segs))
             (sanskrit-p . t))))))))

(defun bcat-sentence--pair-rump (template segments parallel)
  "The two parts of a pair's file of TEMPLATE before C-c u G (Carsten
1.11., `bcat-analysis-pair-lists'): its SEGMENTS and the other
language's from PARALLEL (`bcat-sentence--parallel-fields') — marks,
the dictionary tables of the Tibetan, the Uchen one line per segment;
nil for another template."
  (pcase template
    ("bilingual"
     (let ((lists (bcat-analysis-pair-lists segments (alist-get 'tibetan parallel) nil nil)))
       `((segments . ,(nth 0 lists))
         (tibetan-segments . ,(nth 1 lists))
         (tibetan-lines . ,(nth 2 lists)))))
    ("bilingual-tibetan"
     (let ((lists (bcat-analysis-pair-lists (alist-get 'sanskrit parallel) segments nil nil)))
       `((segments . ,(nth 1 lists))
         (sanskrit-segments . ,(nth 0 lists)))))))

(defun bcat-sentence--verse-run-p (lines)
  "Non-nil when LINES are a verse whose metre the tool knows."
  (plist-get (tibetan-metre-identify (mapcar #'string-trim lines)) :name))

(defun bcat-sentence--quoted-verses (texts)
  "The verses quoted in the prose sentence with the segment lines TEXTS
\(16.11., TĀ 1012: „in der Rezitation der Teil mit den Silben verloren
gegangen“ — since the Viveka is grouped by syntax, quotes stand inside
prose sentences): ((:lines ORIGINAL :verse METRICAL :table SCANSION) …),
one per block of consecutive lines that belong to a known metre (a run
of at most four lines; one line alone too when it is a whole verse).
A block is cut into such runs at the verse ends (`//') where it can be
— three ślokas are three verses, not 3 + 3 lines (TĀ 1025); its tables
are one org table.  Prose lines are never part of a run."
  (require 'tibetan-sanskrit-metre)
  (let* ((v (vconcat texts))
         (n (length v))
         (run-p (lambda (i j) (bcat-sentence--verse-run-p (append (cl-subseq v i (1+ j)) nil))))
         (covered (make-vector n nil))
         (blocks '()))
    (dotimes (i n)
      (cl-loop for j from i to (min (1- n) (+ i 3))
               when (funcall run-p i j)
               do (cl-loop for k from i to j do (aset covered k t))))
    ;; maximal blocks of covered lines
    (let ((i 0))
      (while (< i n)
        (if (not (aref covered i))
            (cl-incf i)
          (let ((j i))
            (while (and (< (1+ j) n) (aref covered (1+ j))) (cl-incf j))
            (push (cons i j) blocks)
            (setq i (1+ j))))))
    (delq nil
          (mapcar
           (lambda (b)
             ;; the block cut into runs: fewest runs, a run ending
             ;; without `//' inside the block costing more
             (let* ((s (car b)) (m (1+ (- (cdr b) s)))
                    (cost (make-vector (1+ m) nil)) (from (make-vector (1+ m) nil)))
               (aset cost 0 0)
               (cl-loop for k from 1 to m
                        do (cl-loop for len from 1 to (min 4 k)
                                    for a = (- k len)
                                    when (and (aref cost a) (funcall run-p (+ s a) (+ s k -1)))
                                    do (let ((c (+ (aref cost a) 1
                                                   (if (and (< k m)
                                                            (not (string-match-p "//[ \t0-9]*\\'" (string-trim (aref v (+ s k -1))))))
                                                       10 0))))
                                         (when (or (null (aref cost k)) (< c (aref cost k)))
                                           (aset cost k c) (aset from k a)))))
               (when (aref cost m)
                 (let ((runs '()) (k m))
                   (while (> k 0)
                     (push (cons (+ s (aref from k)) (+ s k -1)) runs)
                     (setq k (aref from k)))
                   (let ((rs (mapcar (lambda (r) (tibetan-metre-recitation
                                                  (append (cl-subseq v (car r) (1+ (cdr r))) nil) t))
                                     runs)))
                     (list :lines (append (cl-subseq v s (1+ (cdr b))) nil)
                           :verse (cl-mapcan (lambda (r) (split-string (plist-get r :verse) "\n")) rs)
                           :table (mapconcat (lambda (r) (plist-get r :table)) rs "\n")))))))
           (nreverse blocks)))))

(defun bcat-sentence--recitation (texts)
  "Recitation fields of a Sanskrit sentence with the segment lines TEXTS:
recitation — the original line by line in Devanāgarī with the IAST
under it, with the pauses for a verse (Carsten 29.10.: „Es geht ja mit
der Rezitation los“; „das Devanagari im Recitationsverse (also de facto
im Original)“; „das IAST unter dem Devanagari“) —, and for a verse
metre (its header lines with org line breaks) and scansion (IAST,
without the pāda letters)."
  (require 'tibetan-sanskrit-metre)
  (let* ((r (tibetan-metre-recitation texts t))
         ;; 16.11., TĀ 1012: prose with a quoted verse — its lines and
         ;; its table (`bcat-sentence--quoted-verses').
         (quotes (unless r (bcat-sentence--quoted-verses texts))))
    (append
     ;; Each line in Devanāgarī with the IAST under it (Carsten 29.10.:
     ;; „das IAST unter dem Devanagari, wie es vorher im Tibetischen war“).
     `((recitation . ,(mapconcat (lambda (line)
                                   (concat (tibetan-sanskrit-script-iast-to-devanagari line)
                                           "\n" line))
                                 (cond ((plist-get r :verse) (split-string (plist-get r :verse) "\n"))
                                       (quotes (bcat-analysis-quote-lines texts quotes))
                                       (t texts))
                                 "\n")))
     (when r
       `((verse . t)
         (metre . ,(mapconcat #'identity (plist-get r :header) " \\\\\n"))
         (scansion . ,(plist-get r :table))))
     (when quotes
       `((scansion . ,(mapconcat (lambda (q) (plist-get q :table)) quotes "\n\n"))
         (quotes . ,quotes))))))

(defun bcat-sentence--metre-name (segs)
  "The name of the metre the tool scans in the Sanskrit verse SEGS
\((NUM . TEXT) …) — for the prompt (5.11., TĀ 1.1: told nothing, Claude
named Śālinī for a Nardaṭaka); nil for prose and an unknown metre."
  (require 'tibetan-sanskrit-metre)
  (plist-get (tibetan-metre-identify (mapcar (lambda (s) (string-trim (cdr s))) segs)) :name))

(defun bcat-sentence--references (sent-num source-file)
  "The reference translations of sentence SENT-NUM: ((NAME . BODY) …)
from the §-references of its Section and the sentence references —
never Carsten's own „Übersetzung …“ rubric (as in the materializer)."
  (let* ((secnum (ignore-errors (tibetan-cascade--sentence-lopez-section source-file sent-num)))
         (sec-file (and secnum (tibetan-cascade--refs-file source-file :section-refs)))
         (bodies (append (and sec-file (tibetan-cascade--section-ref-bodies sec-file secnum))
                         (ignore-errors (tibetan-cascade--sentence-ref-bodies source-file sent-num)))))
    (cl-remove-if (lambda (nb) (string-prefix-p "Übersetzung" (car nb))) bodies)))

(defun bcat-sentence--comparison (refs)
  "REFS ((NAME . BODY) …) as the paragraphs of Translations compared —
footnote definitions in them at the end (`bcat-analysis-footnotes-last')."
  (bcat-analysis-footnotes-last
   (mapconcat (lambda (nb) (format "*%s:* %s" (car nb) (string-trim (cdr nb))))
              refs "\n\n")))

(declare-function bcat-commentary-source "bcat-commentary" (source-file))
(declare-function bcat-commentary-sources "bcat-commentary" (source-file))
(declare-function bcat-commentary-lead "bcat-commentary" (commentary-source))
(declare-function bcat-commentary--verses "bcat-commentary" (section lead commentary-source))
(declare-function bcat-commentary-section "bcat-commentary" (label lead commentary-source))
(declare-function bcat-commentary-sections "bcat-commentary" (label lead commentary-source))
(declare-function bcat-commentary-incomplete-p "bcat-commentary" (section commentary-source))
(declare-function bcat-commentary-file "bcat-commentary" (section commentary-source))
(declare-function bcat-commentary-fetch "bcat-commentary" (section commentary-source &optional status-file))
(declare-function bcat-dossier-await "bcat-dossier" (verse-file doc))
(declare-function bcat-commentary-verse-links "bcat-commentary" (sent-num source-file assignment))
(declare-function bcat-commentary-ensure-file "bcat-commentary" (sent-num commentary-source))

(defun bcat-sentence--commentary-fields (sent-num source-file context &optional assignment)
  "CONTEXT with the links to the commentary (MSA: Sthiramati, 30.10.)
for the verse SENT-NUM of SOURCE-FILE — the header link, and per segment
the commentary sentences with the lemmata of its pādas, once ASSIGNMENT
\((TIB . SKT) …, from the Tibetan request) is known; CONTEXT itself for
a source without commentary.  `commentary-p': the * Commentary section
has something — a segment's links or the bhāṣya (31.10.: „Kommentare,
wie Sthiramati im MSA, die unbedingt mit rein müssen“)."
  (let ((context
         (if (not (and (bcat-sentence--verse-mode-p source-file)
                       (progn (require 'bcat-commentary) (bcat-commentary-source source-file))))
             context
           (let ((links (bcat-commentary-verse-links sent-num source-file assignment)))
             (if (null links)
                 context
               (append
                `((segments . ,(mapcar (lambda (seg)
                                         (let ((line (cdr (assq (bcat-analysis--num seg) links))))
                                           (if line (cons (cons 'commentary line) seg) seg)))
                                       (alist-get 'segments context)))
                  (commentary-link . ,(cdr (assq :link links))))
                context))))))
    (cons (cons 'commentary-p
                (and (or (alist-get 'bhasya-p context)
                         (cl-some (lambda (seg) (let ((c (alist-get 'commentary seg)))
                                                  (and (stringp c) (not (string-blank-p c)))))
                                  (alist-get 'segments context)))
                     t))
          context)))

(declare-function tibetan-cascade--ref-bodies "tibetan-cascade" (refs-file heading-re))

(defun bcat-sentence--lectures (sent-num source-file)
  "The lectures on sentence SENT-NUM from the #+CAT_LECTURES: file of
SOURCE-FILE (`** Sentence N' / `*** <lecture>', as the verse
references): ((NAME . BODY) …), or nil."
  (let* ((v (bcat-sentence--header-value source-file "CAT_LECTURES"))
         (file (and v (expand-file-name v (file-name-directory source-file)))))
    (and file (tibetan-cascade--ref-bodies
               file (format "^\\*\\* Sentence %d\\b" sent-num)))))

(defun bcat-sentence--lecture-fields (sent-num source-file)
  "The lectures on sentence SENT-NUM of SOURCE-FILE as template fields:
\((lectures-p . t) (lectures ((lecture-name . N) (lecture-body . B)) …)),
or nil.  Carsten 30.10.: Sanderson's lectures on TĀ 1.1–15 in the
sentence file and its PDF — the preparation on the reMarkable, with
handwritten notes.  In the prompt only for the comparison
\(`bcat-sentence--prompts', 2.11.)."
  (let ((bodies (bcat-sentence--lectures sent-num source-file)))
    (when bodies
      `((lectures-p . t)
        (lectures . ,(mapcar (lambda (b) `((lecture-name . ,(car b)) (lecture-body . ,(cdr b))))
                             bodies))))))

(defun bcat-sentence-rump-context (sent-num segs source-file
                                            &optional extra-refs keep)
  "Template fields of the new sentence file SENT-NUM (SEGS = its
\((GLOBAL-NUM . TEXT) …) from SOURCE-FILE), all without AI.
EXTRA-REFS ((NAME . TEXT) …) are references beyond the reference
files (pasted by hand); KEEP an alist of fields carried over from an
existing file (user slots, `created')."
  (bcat-sentence--commentary-fields
   sent-num source-file
   (bcat-sentence--with-source source-file
    (let* ((name (file-name-nondirectory source-file))
           (date (format-time-string "%Y-%m-%d"))
           (template (bcat-sentence-template-name source-file))
           (sanskrit (bcat-sentence--sanskrit-p source-file))
           (segments
            (mapcar (if sanskrit #'bcat-sentence--sanskrit-segment #'bcat-sentence--tibetan-segment)
                    segs)))
      (append
       keep
       ;; No recitation in the bilingual file (Carsten 30.10.: „kann
       ;; wegfallen“); there the Tibetan instead.
       (and (equal template "sanskrit")
            (bcat-sentence--recitation (mapcar (lambda (s) (string-trim (cdr s))) segs)))
       (let ((parallel (and (assoc template bcat-sentence--pair-templates)
                            (bcat-sentence--parallel-fields sent-num source-file))))
         (append (and parallel (bcat-sentence--pair-rump template segments parallel))
                 parallel))
       (bcat-sentence--bhasya-fields sent-num source-file)
       (bcat-sentence--lecture-fields sent-num source-file)
       ;; 20.11.: where the sentence stands in the whole, from the
       ;; outline of the source — the tool's line, set at every write
       (let ((place (bcat-outline-place (bcat-outline-text source-file) sent-num)))
         (and place `((place . ,place))))
       `((sent-num . ,sent-num)
         ;; The comparison is rendered in it (2.11.: „im Handout auch
         ;; immer alles in der Zielsprache“).
         (target-lang . ,tibetan-analysis--target-lang)
         (source-link . ,(format "[[file:../%s::*Sentence %d][%s / Sentence %d]]"
                                 name sent-num name sent-num))
         (segment-list . ,(mapconcat (lambda (s) (number-to-string (car s))) segs ", "))
         (hash . ,(ignore-errors (tibetan-sentence--compute-hash (mapconcat #'cdr segs ""))))
         (created . ,date)
         (last-analyzed . ,date)
         (segments . ,segments)
         (glosses . ,(bcat-analysis-glosses-p segments))
         (comparison . ,(bcat-sentence--comparison
                         (append (bcat-sentence--references sent-num source-file)
                                 extra-refs)))))))))

(defun bcat-sentence-create-file (sent-num segs source-file filepath
                                           &optional extra-refs keep)
  "Write the template-built sentence file SENT-NUM to FILEPATH (C-c u A,
no AI) and return FILEPATH.  EXTRA-REFS and KEEP as in
`bcat-sentence-rump-context'."
  (bcat-sentence--write
   filepath
   (bcat-sentence--render
    (bcat-sentence-template-name source-file)
    (bcat-sentence-rump-context sent-num segs source-file extra-refs keep)
    source-file))
  filepath)

(defun bcat-sentence--template-sections (name source-file)
  "Regexps of the L1 headings template NAME (cascade for SOURCE-FILE)
writes, a placeholder matching anything (`* Sentence {{num}}')."
  (let ((text (bcat-template-read name source-file)) (pos 0) (out '()))
    (while (string-match "^\\* \\(.*\\)$" text pos)
      (let ((title (string-trim (match-string 1 text))))
        (setq pos (match-end 0))
        (push (concat "\\`" (mapconcat #'regexp-quote (split-string title "{{[^}]*}}") ".*") "\\'")
              out)))
    out))

(defcustom bcat-sentence-word-formation-folded nil
  "Non-nil: the parts of the words (*** Word Formation) of a Sanskrit
segment open folded — the heading gets the drawer :VISIBILITY: folded
\(9.11., TĀ 1009).  nil (18.11., Carsten after the Tantrāloka reading:
„erstmal standardmäßig nicht eingeklappt“): they stand open under the
translation of their segment.  C-c u L sets existing files anew."
  :type 'boolean
  :group 'buddhist-cat)

(defun bcat-sentence--render (name context source-file)
  "The template NAME rendered with CONTEXT (cascade for SOURCE-FILE),
followed by the file's own sections (`own-sections' in CONTEXT, from
`bcat-sentence--keep') unchanged — the template does not know them.  A
section the template writes itself (* Bhāṣya, a commentary's * Sentence
258) is no own section."
  (let* ((context (bcat-analysis-with-padapatha-checks context))
         ;; 18.11.: Word Formation open unless the option says folded
         (context (if bcat-sentence-word-formation-folded
                      (cons '(formation-folded . "t") context)
                    context))
         (text (bcat-template-render-named
                ;; 5.11.: the terms of the concept notes link to their
                ;; zettels (bcat-zettel.el) — fetch and relayout alike.
                ;; 16.11.: each padapāṭha checked against its text.
                name (if (fboundp 'bcat-zettel-link-context) (bcat-zettel-link-context context source-file) context)
                source-file))
         (tool (bcat-sentence--template-sections name source-file))
         (own (cl-remove-if (lambda (s) (cl-some (lambda (re) (string-match-p re (car s))) tool))
                            (alist-get 'own-sections context))))
    (if own
        (concat (string-trim-right text) "\n\n" (mapconcat #'cdr own "\n\n") "\n")
      text)))

;; ----------------------------------------------------------------------------
;; Reading a sentence file (either layout)
;; ----------------------------------------------------------------------------

(defun bcat-sentence--text (file)
  "The contents of FILE (\"\" when missing)."
  (if (file-readable-p file)
      (with-temp-buffer (insert-file-contents file) (buffer-string))
    ""))

(defun bcat-sentence--l1-body (text heading)
  "Trimmed body of `* HEADING' in TEXT (up to the next L1 heading), or
nil when absent or empty."
  (when (string-match (format "^\\* %s\n" (regexp-quote heading)) text)
    (let* ((start (match-end 0))
           (end (if (string-match "^\\* " text start) (match-beginning 0) (length text)))
           (body (string-trim (substring text start end))))
      (unless (string-empty-p body) body))))

(defconst bcat-sentence--user-slots
  '((working-translation . "Working Translation")
    (my-notes . "My Notes")
    (footnotes . "Footnotes"))
  "The user's own sections: never written by the tool, carried over.")

(defconst bcat-sentence--tool-sections
  '("Recitation" "Text" "Segments" "Sentence"
    ;; 18.11.: three parts — * Text (the original), * Philology,
    ;; * Worum es geht (kept as the overview, `bcat-sentence--keep').
    "Philology"
    ;; Since 31.10. (the order read → translate → grammar → concepts →
    ;; references → the rest → commentary → further reading), and the
    ;; L1 sections they replace — an old file's * Bhāṣya or * Lectures
    ;; must not survive a rebuild as its own section.
    "Tibetan" "Sanskrit" "Commentary" "Further Reading" "Bhāṣya" "Lectures"
    "Translation" "Reading" "Provided Translations" "Tibetan Text" "Tibetan Analysis"
    "Sanskrit Text" "Sanskrit Analysis" "Combined Analysis" "Auto-Analysis"
    "Reference Translations" "DharmaMitra Translation (Tibetan)"
    "DharmaMitra Translation (Sanskrit)" "Sanskrit (DharmaMitra)")
  "The L1 sections the tool writes, in the template form and in the old
layouts: a rebuild makes them anew.  Any other L1 section besides the
user slots belongs to someone and is kept (`bcat-sentence--own-sections').")

(defun bcat-sentence--own-sections (text)
  "The L1 sections of TEXT that the tool does not write and that are no
user slot — added by hand or for another script (AS and MSA, 26.10.:
* Handout, the input of handout-synopse.py, corrected in class) —
verbatim, in order, as ((TITLE . TEXT) …); nil when there are none.
`bcat-sentence--render' leaves out those its template writes."
  (let ((starts '()) (pos 0))
    (while (string-match "^\\* \\(.*\\)$" text pos)
      (push (cons (match-beginning 0) (string-trim (match-string 1 text))) starts)
      (setq pos (match-end 0)))
    (setq starts (nreverse starts))
    (let ((own (cl-loop for rest on starts
                        for (beg . title) = (car rest)
                        unless (or (member title bcat-sentence--tool-sections)
                                   (rassoc title bcat-sentence--user-slots))
                        collect (cons title
                                      (string-trim-right
                                       (substring text beg (if (cdr rest) (car (cadr rest)) (length text))))))))
      own)))

(defconst bcat-sentence--overview-heading "Worum es geht"
  "The L1 section in front of a sentence file and a commentary document
\(16.11., Carsten: he follows the translations but cannot join the
discussion in class): what the sentence explains, the train of thought,
the commentator's move, questions for the discussion — in German,
whatever the language of the rest.")

(defun bcat-sentence--overview (text)
  "(BODY . MARK) of „* Worum es geht“ in TEXT, a sentence file or a
commentary document: BODY its text without the drawer under the
heading, MARK the drawer's :GENERATED_HASH: — set for a generated
section (`bcat-analysis-overview-hash'), nil for one written by hand.
nil when the file has no such section or it is empty.  The mark is read
letter by letter, not by a syntax class: \\S- depends on the buffer that
happens to be current (in a Lisp buffer a line end is no whitespace)."
  (let ((body (bcat-sentence--l1-body text bcat-sentence--overview-heading)))
    (when body
      (if (string-match "\\`:PROPERTIES:[ \t]*\n\\(\\(?:.*\n\\)*?\\):END:[ \t]*\\(?:\n\\|\\'\\)" body)
          (let ((drawer (match-string 1 body))
                (rest (string-trim (substring body (match-end 0)))))
            (unless (string-empty-p rest)
              (cons rest (and (string-match "^:GENERATED_HASH:[ \t]*\\([^ \t\n]+\\)" drawer)
                              (match-string 1 drawer)))))
        (cons body nil)))))

(defconst bcat-sentence--neighbour-limit 600
  "Longest text of a neighbouring sentence in a request
\(`bcat-sentence--neighbours-block'); a longer one is cut with „…“.")

(defun bcat-sentence--neighbours-block (sentence source-file)
  "The sentences before and after SENTENCE in SOURCE-FILE as a block of
the request's context, or nil when it has neither — 16.11.: „Worum es
geht“ takes in the neighbours („zweite Runde“, „Rückverweis auf
1016“).  Their text as transmitted, marked as context only."
  (let* ((all (bcat-sentence--sentences source-file))
         (pos (cl-position (plist-get sentence :sent-num) all
                           :key (lambda (s) (plist-get s :sent-num))))
         (line (lambda (s where)
                 (let ((text (and s (string-join (mapcar (lambda (g) (string-trim (cdr g)))
                                                         (plist-get s :segs))
                                                 " "))))
                   (and text (not (string-blank-p text))
                        (format "Sentence %d (%s): %s" (plist-get s :sent-num) where
                                (truncate-string-to-width text bcat-sentence--neighbour-limit
                                                          nil nil "…"))))))
         (lines (and pos (delq nil (list (funcall line (and (> pos 0) (nth (1- pos) all)) "before")
                                         (funcall line (nth (1+ pos) all) "after"))))))
    (and lines
         (concat "Neighbouring sentences (context for the overview only — do not analyse them):\n"
                 (string-join lines "\n")))))

(defun bcat-sentence--keep (file)
  "Fields of FILE that a rebuild carries over: the user slots, the
#+CREATED date, the file's own sections (30.10.: the rebuild from the
old layout dropped the AS/MSA * Handout) and `overview', the text of
„* Worum es geht“ (16.11., Tantrāloka 1009–1040: written by hand —
the template sets it in front again, word for word), with
`overview-hash', its mark when it was generated
\(`bcat-sentence--overview')."
  (let* ((text (bcat-sentence--text file))
         (own (bcat-sentence--own-sections text))
         (overview (bcat-sentence--overview text)))
    (append
     (delq nil (mapcar (lambda (slot)
                         (let ((body (bcat-sentence--l1-body text (cdr slot))))
                           (and body (cons (car slot) body))))
                       bcat-sentence--user-slots))
     (and overview (cons (cons 'overview (car overview))
                         (and (cdr overview) (list (cons 'overview-hash (cdr overview))))))
     (and own (list (cons 'own-sections own)))
     (and (string-match "^#\\+CREATED:[ \t]*\\(.+\\)$" text)
          (list (cons 'created (string-trim (match-string 1 text))))))))

(defun bcat-sentence--compared (file)
  "The paragraphs under `** Translations compared' of the template FILE
as ((NAME . TEXT) …) — every `*NAME:* TEXT' paragraph above the units of
the comparison (its first *** subsection, 2.11.), in order.  5.11., TĀ
1.1: read down to the next L1/L2 heading, the decisions of an earlier
comparison (*Entscheidungen:* in its *** Übersetzungsvorschlag) became
a reference — sent to Claude as „E = Entscheidungen“."
  (let ((text (bcat-sentence--text file)))
    (when (string-match "^\\*\\* Translations compared\n" text)
      (let* ((start (match-end 0))
             (end (if (string-match "^\\*\\{1,3\\} " text start)
                      (match-beginning 0)
                    (length text))))
        (append
         (delq nil
               (mapcar (lambda (p)
                         (when (string-match "\\`\\*\\([^*\n]+?\\):\\* \\(\\(?:.\\|\n\\)*\\)" p)
                           (cons (match-string 1 p) (string-trim (match-string 2 p)))))
                       (split-string (substring text start end) "\n[ \t]*\n" t)))
         ;; 18.11.: a machine translation stands last, under a heading
         ;; of its own — its text whole (every paragraph), down to the
         ;; next heading or the next labelled paragraph: a reference
         ;; pasted at the very end of the section is one too.
         (let ((whole (if (string-match "^\\*\\{1,2\\} " text start) (match-beginning 0) (length text)))
               (pos start) (out '()))
           (while (and (string-match "^\\*\\*\\* [^\n]*\n\\*\\([^*\n]+?\\):\\* " text pos)
                       (< (match-beginning 0) whole))
             (let* ((name (match-string 1 text))
                    (b (progn (string-match "\n" text (match-beginning 0)) (match-end 0)))
                    (e (min whole (if (string-match "^\\*+ " text b) (match-beginning 0) (length text)))))
               (when (member name bcat-analysis-machine-references)
                 (let ((current nil))
                   (dolist (p (split-string (substring text b e) "\n[ \t]*\n" t))
                     (if (string-match "\\`\\*\\([^*\n]+?\\):\\* \\(\\(?:.\\|\n\\)*\\)" p)
                         (progn (setq current (cons (match-string 1 p) (string-trim (match-string 2 p))))
                                (push current out))
                       (when current (setcdr current (concat (cdr current) "\n\n" (string-trim p))))))))
               (setq pos e)))
           (nreverse out)))))))

(defun bcat-sentence--old-references (file)
  "References pasted into the old layout's `* Provided Translations' of
FILE: ((NAME . BODY) …) of its `** NAME' children, without the machine
slots (DharmaMitra, Discussion)."
  (let ((pt (bcat-sentence--l1-body (bcat-sentence--text file) "Provided Translations")))
    (when pt
      (delq nil
            (mapcar (lambda (chunk)
                      (when (string-match "\\`\\([^\n]+\\)\n\\(\\(?:.\\|\n\\)*\\)" chunk)
                        (let ((name (string-trim (match-string 1 chunk)))
                              (body (string-trim
                                     (replace-regexp-in-string
                                      "^:PROPERTIES:\n\\(?:.*\n\\)*?:END:\n?" ""
                                      (match-string 2 chunk)))))
                          (unless (or (string-match-p "\\`DharmaMitra\\|\\`Discussion" name)
                                      (string-empty-p body))
                            (cons name body)))))
                    (split-string (concat "\n" pt) "\n\\*\\* " t))))))

(defun bcat-sentence--extra-references (names refs)
  "Those of REFS ((NAME . TEXT) …) whose NAME is not in NAMES and is not
a machine name (Translation, DharmaMitra) — marks stripped."
  (delq nil (mapcar (lambda (r)
                      (unless (or (member (car r) names)
                                  (member (car r) '("Translation" "DharmaMitra")))
                        (cons (car r) (bcat-analysis-strip-marks (cdr r)))))
                    refs)))

;; ----------------------------------------------------------------------------
;; Reviewed files (20.11.2026)
;; ----------------------------------------------------------------------------
;; Carsten über die Tantrāloka-Sitzung, 20.11. („ja, beides so machen“):
;; until 19.11. new fetches reset the corrections of the Tantrāloka files
;; three times, in silence.  A sentence file now knows whether its
;; analysis was checked by hand:
;;   #+CAT_WRITTEN: <md5>      the tool's stamp on the generated part as
;;                             it wrote it — another text there was typed
;;                             by hand since;
;;   #+CAT_REVIEWED: <date>    said outright (by hand, with
;;                             `bcat-sentence-mark-reviewed', or by C-c u L
;;                             for a file it finds edited);
;;   #+CAT_REVIEWED_BEFORE: …  the reviewed version a fetch replaced, kept
;;                             in analysis/.reviewed/.
;; A fetch of a reviewed file sends that analysis with the request, keeps
;; the file, and says how far the new one departs (C-c u d).

(defconst bcat-sentence--reviewed-folder ".reviewed"
  "The folder beside the sentence files that keeps the reviewed versions
new fetches replaced („<date> <time> <name>.org“ — no name beginning
with „sent-“: no folder scan takes one for a sentence file).")

(defvar bcat-sentence--raw-write nil
  "Non-nil: `bcat-sentence--write' writes its text as given, without the
lines of `bcat-sentence--stamped' (`bcat-sentence-mark-reviewed').")

(defvar bcat-sentence--fresh nil
  "Non-nil while a fetch writes a sentence file — what is written is a
new analysis, not the one the file had: t, or the path (relative to the
file's folder) of the reviewed version it replaces
\(`bcat-sentence--keep-reviewed').")

(defun bcat-sentence--generated-body (text)
  "The part of the sentence file TEXT the tool generates and a reviewer
corrects: from its first heading of level 1 up to „* Worum es geht“,
„* Further Reading“ or „* Working Translation“, whichever comes first
— * Text and * Philology —, without the blanks at the ends of lines.
Not the header, the notes, the sections of the file's own."
  (let* ((start (string-match "^\\* " text))
         (end (and start
                   (string-match "^\\* \\(?:Worum es geht\\|Further Reading\\|Working Translation\\)[ \t]*$"
                                 text start))))
    (if start
        (replace-regexp-in-string "[ \t]+$" "" (substring text start (or end (length text))))
      "")))

(defun bcat-sentence--body-hash (text)
  "The stamp of the sentence file TEXT: the md5 of its generated part
\(`bcat-sentence--generated-body')."
  (md5 (bcat-sentence--generated-body text) nil nil 'utf-8))

(defun bcat-sentence--text-header (text key)
  "The value of #+KEY: in the header of the file TEXT (before its first
heading), or nil."
  (let ((head (substring text 0 (or (string-match "^\\* " text) (length text)))))
    (and (string-match (format "^#\\+%s:[ \t]*\\(.*?\\)[ \t]*$" (regexp-quote key)) head)
         (let ((value (match-string 1 head)))
           (and (not (string-empty-p value)) value)))))

(defun bcat-sentence--text-reviewed (text)
  "Non-nil when the analysis in the sentence file TEXT was checked by
hand: its #+CAT_REVIEWED, or — its generated part no longer has the
stamp the tool gave it (#+CAT_WRITTEN) — today's date with „(von Hand
geändert)“.  nil for a file as the tool wrote it, and for one from
before the stamp that does not say it is reviewed."
  (or (bcat-sentence--text-header text "CAT_REVIEWED")
      (let ((stamp (bcat-sentence--text-header text "CAT_WRITTEN")))
        (and stamp
             (not (equal stamp (bcat-sentence--body-hash text)))
             (concat (format-time-string "%Y-%m-%d") " (von Hand geändert)")))))

(defun bcat-sentence-reviewed (file)
  "Non-nil when the analysis in the sentence FILE was checked by hand
\(`bcat-sentence--text-reviewed') — a new fetch then sends it with the
request and keeps the file (`bcat-sentence--keep-reviewed')."
  (and file (file-readable-p file)
       (bcat-sentence--text-reviewed (bcat-sentence--text file))))

(defun bcat-sentence--stamped (text old)
  "TEXT of a sentence file as the tool is about to write it, with the
lines that say whether its analysis is reviewed, after #+CAT_TEMPLATE:
OLD is the text the file has now (nil: none).  Setting a file anew
\(C-c u L, a reference added) keeps what OLD says — its #+CAT_REVIEWED,
or today's when OLD was edited by hand since the tool wrote it, and its
#+CAT_REVIEWED_BEFORE; a fetch (`bcat-sentence--fresh') writes a new
analysis: not reviewed, #+CAT_REVIEWED_BEFORE the version it replaces.
#+CAT_WRITTEN is the stamp of what is written
\(`bcat-sentence--body-hash').  A text without #+CAT_TEMPLATE stays."
  (let ((clean (replace-regexp-in-string
                "^#\\+CAT_\\(?:REVIEWED\\|REVIEWED_BEFORE\\|WRITTEN\\):.*\n" "" text)))
    (if (not (string-match "^#\\+CAT_TEMPLATE:.*\n" clean))
        text
      (let ((at (match-end 0))
            (reviewed (and (not bcat-sentence--fresh) old (bcat-sentence--text-reviewed old)))
            (before (cond ((stringp bcat-sentence--fresh) bcat-sentence--fresh)
                          (bcat-sentence--fresh nil)
                          (old (bcat-sentence--text-header old "CAT_REVIEWED_BEFORE")))))
        (concat (substring clean 0 at)
                (if reviewed (format "#+CAT_REVIEWED: %s\n" reviewed) "")
                (if before (format "#+CAT_REVIEWED_BEFORE: %s\n" before) "")
                (format "#+CAT_WRITTEN: %s\n" (bcat-sentence--body-hash clean))
                (substring clean at))))))

(defun bcat-sentence--keep-reviewed (file)
  "Before a fetch overwrites the sentence FILE: the reviewed version the
new analysis replaces, as a path relative to FILE's folder, or nil.  A
reviewed FILE (`bcat-sentence--text-reviewed') is copied word for word
into `bcat-sentence--reviewed-folder'; a file that is not, but names a
reviewed version it replaced earlier (#+CAT_REVIEWED_BEFORE, the copy
still there), keeps that one — until someone reviews the new analysis,
the last reviewed version is the reference."
  (when (and file (file-readable-p file))
    (let ((text (bcat-sentence--text file))
          (dir (file-name-directory file)))
      (if (bcat-sentence--text-reviewed text)
          (let* ((rel (format "%s/%s %s.org" bcat-sentence--reviewed-folder
                              (format-time-string "%Y-%m-%d %H%M") (file-name-base file)))
                 (path (expand-file-name rel dir)))
            (make-directory (file-name-directory path) t)
            (copy-file file path t)
            rel)
        (let ((before (bcat-sentence--text-header text "CAT_REVIEWED_BEFORE")))
          (and before (file-readable-p (expand-file-name before dir)) before))))))

(defun bcat-sentence--reviewed-text (file)
  "The text of the reviewed version of the sentence FILE that a fetch
has to respect, or nil: FILE's own when it is reviewed, else that of
the version it names (#+CAT_REVIEWED_BEFORE)."
  (when (and file (file-readable-p file))
    (let ((text (bcat-sentence--text file)))
      (if (bcat-sentence--text-reviewed text)
          text
        (let* ((before (bcat-sentence--text-header text "CAT_REVIEWED_BEFORE"))
               (path (and before (expand-file-name before (file-name-directory file)))))
          (and path (file-readable-p path) (bcat-sentence--text path)))))))

(defun bcat-sentence--plain-lines (text)
  "The generated part of the sentence file TEXT as lines to compare or
to send: no drawers, no blank lines, links as their text, blanks
squeezed."
  (let ((body (replace-regexp-in-string
               "^[ \t]*:PROPERTIES:[ \t]*\n\\(?:[ \t]*:[^\n]*\n\\)*?[ \t]*:END:[ \t]*\n" ""
               (bcat-sentence--generated-body text))))
    (delete ""
            (mapcar (lambda (line)
                      (string-trim
                       (replace-regexp-in-string
                        "[ \t]+" " "
                        (replace-regexp-in-string "\\[\\[[^]\n]+\\]\\[\\([^]\n]+\\)\\]\\]" "\\1" line))))
                    (split-string body "\n")))))

(defun bcat-sentence--reviewed-block (file)
  "The block of a request's context with the reviewed analysis of the
sentence FILE (`bcat-sentence--reviewed-text'), or nil (Carsten 20.11.:
the reviewed version goes with the request — its decisions stand)."
  (let ((text (bcat-sentence--reviewed-text file)))
    (and text
         (concat "Reviewed analysis of this sentence — checked and corrected by hand. "
                 "The decisions in this reviewed analysis stand. Depart from one only if you state "
                 "explicitly why it is wrong — in the note of the segment concerned.\n"
                 "<reviewed>\n"
                 (string-join (bcat-sentence--plain-lines text) "\n")
                 "\n</reviewed>"))))

(defun bcat-sentence--departures (file)
  "How many lines of the reviewed version the sentence FILE names
\(#+CAT_REVIEWED_BEFORE) its generated part no longer has — links and
spacing aside —, or nil when it names none."
  (let* ((text (bcat-sentence--text file))
         (before (bcat-sentence--text-header text "CAT_REVIEWED_BEFORE"))
         (path (and before (expand-file-name before (file-name-directory file)))))
    (when (and path (file-readable-p path))
      (let ((now (bcat-sentence--plain-lines text)))
        (cl-count-if-not (lambda (line) (member line now))
                         (bcat-sentence--plain-lines (bcat-sentence--text path)))))))

(defun bcat-sentence--report-departures (file)
  "After a fetch wrote the sentence FILE: say in its status and as a
message how far the new analysis departs from the reviewed version it
replaced — nothing is overwritten in silence (Carsten 20.11.)."
  (let ((n (bcat-sentence--departures file)))
    (when n
      (let ((text (if (zerop n)
                      "geprüfte Fassung: nichts anders"
                    (format "geprüfte Fassung: %d Zeile%s anders — C-c u d" n (if (= n 1) "" "n")))))
        (bcat-status-step file :review text)
        (message "%s — %s (aufbewahrt: %s)" (file-name-nondirectory file) text
                 (bcat-sentence--text-header (bcat-sentence--text file) "CAT_REVIEWED_BEFORE"))))))

;;;###autoload
(defun bcat-sentence-show-departures ()
  "C-c u d in a sentence file: the reviewed version a fetch replaced
\(#+CAT_REVIEWED_BEFORE, kept in analysis/.reviewed/) against the file
as it is now — where the new analysis departs from what was checked by
hand (Carsten 20.11.: „danach anzeigen, wo die neue Fassung abweicht“)."
  (interactive)
  (let* ((file (buffer-file-name))
         (before (and file (bcat-sentence--text-header (buffer-string) "CAT_REVIEWED_BEFORE")))
         (path (and before (expand-file-name before (file-name-directory file)))))
    (cond ((not before) (message "Diese Datei nennt keine geprüfte Fassung, die ein Abruf ersetzt hätte"))
          ((not (file-readable-p path)) (message "Die geprüfte Fassung fehlt: %s" path))
          (t (when (buffer-modified-p) (save-buffer))
             (diff path file)))))

;;;###autoload
(defun bcat-sentence-mark-reviewed (&optional remove)
  "Mark the analysis in the sentence file of this buffer as checked by
hand: #+CAT_REVIEWED: <today> — a new fetch then sends it with the
request and keeps the file (Carsten 20.11.).  With a prefix argument
REMOVE the mark goes, and the file as it stands counts as the tool's
\(a new stamp): the next fetch replaces it like any unreviewed one."
  (interactive "P")
  (let ((file (buffer-file-name)))
    (unless (and file (bcat-sentence-file-p file))
      (user-error "Keine Satzdatei einer Vorlage"))
    (when (buffer-modified-p) (save-buffer))
    (let* ((text (bcat-sentence--text file))
           (clean (replace-regexp-in-string "^#\\+CAT_\\(?:REVIEWED\\|WRITTEN\\):.*\n" "" text))
           (at (and (string-match "^#\\+CAT_TEMPLATE:.*\n" clean) (match-end 0)))
           (new (concat (substring clean 0 at)
                        (if remove
                            (format "#+CAT_WRITTEN: %s\n" (bcat-sentence--body-hash clean))
                          (format "#+CAT_REVIEWED: %s\n" (format-time-string "%Y-%m-%d")))
                        (substring clean at))))
      (let ((bcat-sentence--raw-write t))
        (bcat-sentence--write file new))
      (message (if remove "%s: nicht mehr als geprüft geführt" "%s: als geprüft vermerkt")
               (file-name-nondirectory file)))))

(defun bcat-sentence--write (file text)
  "Write TEXT to FILE — through a visiting buffer when there is one, so
buffer and file stay in step (its unsaved edits are saved first and
were read by the caller).  20.11.: with the lines that say whether the
analysis is reviewed and the tool's stamp (`bcat-sentence--stamped')."
  (make-directory (file-name-directory file) t)
  (let ((buf (get-file-buffer file))
        (text (if bcat-sentence--raw-write
                  text
                (bcat-sentence--stamped text (and (file-readable-p file) (bcat-sentence--text file))))))
    (if (not buf)
        (let ((make-backup-files nil))
          (with-temp-file file (insert text)))
      (with-current-buffer buf
        (let ((inhibit-read-only t)
              (make-backup-files nil))
          ;; The disk version was just read by the caller (user slots,
          ;; references) — a stale visit must not raise Emacs's "changed
          ;; on disk" question in the middle of a fetch.
          (unless (verify-visited-file-modtime buf)
            (set-visited-file-modtime))
          (erase-buffer)
          (insert text)
          ;; 9.11.: what opens folded (Word Formation, :VISIBILITY:
          ;; folded) is folded after a rebuild too.
          (when (derived-mode-p 'org-mode)
            (org-cycle-set-visibility-according-to-property)
            ;; 9.11. (TĀ 1010): an old cascade file was open when it
            ;; was rebuilt after the template — `org-mode-hook' had
            ;; seen no #+CAT_TEMPLATE, the colours stayed off.
            (unless (bound-and-true-p bcat-faces-mode)
              (when (fboundp 'bcat-faces-maybe) (bcat-faces-maybe))))
          (save-buffer))))))

(defun bcat-sentence--save-visit (file)
  "Save FILE's visiting buffer when it has unsaved edits, so the user's
typing is read (and kept) by a rebuild."
  (let ((buf (get-file-buffer file)))
    (when (and buf (buffer-modified-p buf))
      (with-current-buffer buf (save-buffer)))))

;; ----------------------------------------------------------------------------
;; C-c u G (§5.74 step 3b)
;; ----------------------------------------------------------------------------

(declare-function tibetan-cascade--collect-sentences "tibetan-cascade" ())
(declare-function tibetan-cascade--create-file "tibetan-cascade" (sent-num segs source-file))
(declare-function tibetan-sentence--filepath "tibetan-sentence-persist" (sent-num &optional folder source-file))
(declare-function tibetan-analysis--defer-mt-p "tibetan-analysis-claude" (file))
(declare-function tibetan-discussion--submit "tibetan-translation-discussion" (kind file prompts land &optional fail))
(declare-function tibetan-dharmamitra-api-chat-translate "tibetan-dharmamitra-api" (text &rest args))
(declare-function tibetan-dharmamitra-translation--target-lang "tibetan-dharmamitra-translation" (file))
(declare-function tibetan-dharmamitra-translation--input-encoding "tibetan-dharmamitra-translation" (file))
(defvar tibetan-cascade-subtree-confirm-threshold)
(declare-function tibetan-sentence-claude-dm-delay "tibetan-sentence-claude" ())

(defvar bcat-sentence-after-fetch-functions nil
  "Abnormal hook run with the sentence FILE after C-c u G has written
it (§5.74) — C-c u H (PDF + reMarkable) hangs here.")

(defun bcat-sentence--segments-part (text)
  "The `* Segments' section of the sentence file TEXT, up to the next L1
heading — there stand the ⟦N⟧ translations with the references per
segment; since 31.10. ⟦N⟧ also labels the tables under * Text, the
concepts, the other language and the commentary links, and * Sentence
comes before * Segments.  18.11.: in a file of three parts they stand
under ** Segment N in * Philology — the part from the first of them to
the end of * Philology, with the headings (\"\" when it has none yet).
TEXT itself when it has neither section (a commentary document: its
segments under `* Sentence N')."
  (cond
   ((string-match "^\\* Segments\n" text)
    (let ((start (match-end 0)))
      (substring text start (if (string-match "^\\* " text start) (match-beginning 0) (length text)))))
   ((string-match "^\\* Philology[ \t]*\n" text)
    (let* ((start (match-end 0))
           (end (if (string-match "^\\* " text start) (match-beginning 0) (length text)))
           ;; 22.11.: in a commentary document a level deeper, under
           ;; ** Sentence N
           (first (and (string-match "^\\*\\*\\*? Segment [0-9]" text start)
                       (< (match-beginning 0) end)
                       (match-beginning 0))))
      (if first (substring text first end) "")))
   (t text)))

(defun bcat-sentence--goto-segments ()
  "Move point in the buffer of a sentence file to where the ⟦N⟧
translations of its segments begin: after the heading * Segments, or
\(18.11., three parts) to the first ** Segment N under * Philology.
Returns non-nil when found; point at the start of the buffer before."
  (goto-char (point-min))
  (or (re-search-forward "^\\* Segments$" nil t)
      (and (re-search-forward "^\\* Philology$" nil t)
           (re-search-forward "^\\*\\*\\*? Segment [0-9]" nil t))))

(defun bcat-sentence--unaligned (file)
  "References under Translations compared of FILE that no segment shows
yet (added by hand after the analysis): ((NAME . TEXT) …), marks
stripped."
  (let ((segments (bcat-sentence--segments-part (bcat-sentence--text file))))
    (delq nil (mapcar (lambda (r)
                        (unless (or (equal (car r) "Translation")
                                    (string-match-p (format "^\\*%s:\\* " (regexp-quote (car r)))
                                                    segments))
                          (cons (car r) (bcat-analysis-strip-marks (cdr r)))))
                      (bcat-sentence--compared file)))))

(defun bcat-sentence-fetch-state (file seg-nums)
  "What C-c u G still has to do for the template FILE with SEG-NUMS:
`full' while a segment has no ⟦N⟧ translation, or — a bilingual file
— while no segment names its counterpart („· Tib. 77“, „· Skt. 22“;
the Tibetan request needs the Sanskrit answer, so both again);
`dm' when DharmaMitra is missing (30.10.: analysed while its daily
limit held — fetched alone and placed by the alignment request);
`align' when a reference under Translations compared appears in no
segment (added by hand) — a machine translation does not count —; nil
when the file is complete."
  (let* ((text (bcat-sentence--text file))
         (segments (bcat-sentence--segments-part text)))
    (cond
     ((cl-some (lambda (n) (not (string-match-p (format "^⟦%d⟧ " n) segments))) seg-nums)
      'full)
     ((and (string-match-p "^#\\+CAT_PARALLEL_SEGMENTS:[ \t]*[0-9]" text)
           (not (string-match-p "^\\*\\* Segment [0-9]+ · \\(?:Tib\\|Skt\\)\\. " text)))
      'full)
     ((not (string-match-p "^\\*DharmaMitra:\\* " text)) 'dm)
     ;; 22.11.: not for a machine translation alone — since 18.11. it is
     ;; no reference and stands in no segment; counted here, it put
     ;; EVERY analysed file into this state (MSA XI: the verses of the
     ;; morning were about to be analysed again)
     ((cl-remove-if (lambda (r) (member (car r) bcat-analysis-machine-references))
                    (bcat-sentence--unaligned file))
      'align))))

;; ----------------------------------------------------------------------------
;; Step 3c — placing references added by hand
;; ----------------------------------------------------------------------------

(defun bcat-sentence--own-translations (text)
  "The ⟦N⟧ translations in TEXT: ((N . TRANSLATION) …), without the
„*Translation:*“ label (files of 28.10. have none)."
  (let ((pos 0) (out '()))
    (while (string-match "^⟦\\([0-9]+\\)⟧ \\(?:\\*Translation:\\* \\)?\\(.*?\\)\\(?: \\\\\\\\\\)?$" text pos)
      (push (cons (string-to-number (match-string 1 text)) (match-string 2 text)) out)
      (setq pos (match-end 0)))
    (nreverse out)))

(defun bcat-sentence--block-lines (block)
  "The lines of a translation BLOCK without their org line breaks."
  (mapcar (lambda (l) (replace-regexp-in-string " \\\\\\\\\\'" "" l))
          (split-string block "\n")))

(defun bcat-sentence--align-merge (text parsed)
  "TEXT of a template file with the alignment answer PARSED put in:
per segment the new reference parts after the existing references and
the new „→“ line after the existing ones; under Translations compared
each new reference's own paragraph marked from its segment parts (the
text stays the one pasted) and the comparison sentence added.
Everything else stays as it is."
  (with-temp-buffer
    (insert text)
    (dolist (seg (alist-get 'segments parsed))
      (let ((num (alist-get 'num seg)))
        (when (and (bcat-sentence--goto-segments)
                   (re-search-forward (format "^⟦%s⟧ " num) nil t))
          (let* ((start (line-beginning-position))
                 (end (if (re-search-forward "\n[ \t]*\n" nil t) (match-beginning 0) (point-max)))
                 (lines (bcat-sentence--block-lines (buffer-substring start end)))
                 (arrows (cl-remove-if-not (lambda (l) (string-prefix-p "→ " l)) lines))
                 (others (cl-remove-if (lambda (l) (string-prefix-p "→ " l)) lines))
                 (new-refs (bcat-analysis--reference-lines (alist-get 'references seg)))

                 (all (append others new-refs arrows
                              (bcat-analysis-shift-lines seg))))
            (delete-region start end)
            (goto-char start)
            (insert (mapconcat #'identity all " \\\\\n"))))))
    (goto-char (point-min))
    (when (re-search-forward "^\\*\\* Translations compared\n" nil t)
      (let ((region-start (point)))
        (dolist (name (delete-dups
                       (mapcan (lambda (seg)
                                 (mapcar (lambda (r) (bcat-analysis--field r 'name))
                                         (alist-get 'references seg)))
                               (copy-sequence (alist-get 'segments parsed)))))
          (goto-char region-start)
          (when (and (not (string-empty-p name))
                     (re-search-forward (format "^\\*%s:\\* " (regexp-quote name)) nil t))
            (let* ((pstart (line-beginning-position))
                   (tstart (point))
                   (pend (if (re-search-forward "\n[ \t]*\n\\|\n\\*" nil t)
                             (match-beginning 0)
                           (point-max)))
                   (own (bcat-analysis-strip-marks (buffer-substring tstart pend))))
              (delete-region pstart pend)
              (goto-char pstart)
              (insert (format "*%s:* %s" name
                              (bcat-analysis-mark-reference
                               own (bcat-analysis--reference-parts
                                    (alist-get 'segments parsed) name)))))))
        ;; The region: the translations up to the first heading — the
        ;; units of the comparison (*** Übersicht …, 2.11.) stay below.
        (let ((comparison (bcat-analysis--field parsed 'comparison)))
          (unless (string-empty-p comparison)
            (goto-char region-start)
            (let ((region-end (if (re-search-forward "^\\*+ " nil t)
                                  (line-beginning-position)
                                (point-max))))
              (goto-char region-end)
              (skip-chars-backward " \t\n")
              (insert "\n\n" comparison))))
        ;; Footnote definitions (Lindtner's notes) back to the end, after
        ;; the new paragraphs (SDA 10, 1.11.).
        (goto-char region-start)
        (let* ((region-end (if (re-search-forward "^\\*+ " nil t)
                               (line-beginning-position)
                             (point-max)))
               (old (buffer-substring region-start region-end))
               (new (bcat-analysis-footnotes-last old)))
          (unless (eq new old)
            (delete-region region-start region-end)
            (goto-char region-start)
            (insert (string-trim new) "\n\n")))))
    (buffer-string)))

(defun bcat-sentence--unreadable (what file response problem)
  "Report that the answer of kind WHAT for FILE could not be read
\(PROBLEM says why) and keep RESPONSE for diagnosis — as a message and
on FILE's status line.  Returns nil."
  (let ((path (bcat-analysis-keep-response file response)))
    (message "C-c u G: %s für %s nicht lesbar (%s) — Datei unverändert; Antwort: %s"
             what (file-name-nondirectory file) problem path)
    (bcat-status-step file (pcase what ("Zuordnung" :align) ("Tibetisch" :tibetan) (_ :claude))
                      (format "%s: nicht lesbar (%s) — Antwort: %s"
                              (if (equal what "Antwort") "Claude" what)
                              problem path)))
  nil)

(defun bcat-sentence--align-land (file response)
  "Put the alignment RESPONSE into FILE; nil and an untouched file when
the answer is unreadable."
  (let* ((read (bcat-analysis-read response))
         (parsed (car read)))
    (if (not parsed)
        (bcat-sentence--unreadable "Zuordnung" file response (cdr read))
      (bcat-sentence--save-visit file)
      (bcat-sentence--write file (bcat-sentence--align-merge (bcat-sentence--text file) parsed))
      (bcat-status-step file :align "Zuordnung ✓")
      (run-hook-with-args 'bcat-sentence-after-fetch-functions file)
      file)))

(defun bcat-sentence--align-one (file sentence source-file)
  "ONE small request placing the references added by hand in FILE —
no DharmaMitra, no new analysis."
  (let* ((refs (bcat-sentence--unaligned file))
         (own (bcat-sentence--own-translations
               (bcat-sentence--segments-part (bcat-sentence--text file))))
         (meta (ignore-errors (tibetan-analysis--read-source-metadata source-file))))
    (require 'tibetan-translation-discussion)
    (tibetan-discussion--submit
     "bcat-align" file
     (lambda ()
       (let ((sanskrit (bcat-sentence--sanskrit-p source-file)))
         (cons (bcat-analysis-align-system-prompt (or (plist-get meta :target-lang) "en")
                                                  (if sanskrit "Sanskrit" "Classical Tibetan"))
               (bcat-analysis-align-user-prompt
                (plist-get sentence :sent-num) (plist-get sentence :verse)
                (mapcar (lambda (s) (list (car s) (cdr s)
                                          (unless sanskrit (bcat-sentence--wylie (cdr s)))
                                          (cdr (assq (car s) own))))
                        (plist-get sentence :segs))
                refs
                (plist-get meta :claude-context)
                (bcat-sentence--second-language sentence source-file)))))
     (lambda (f response) (bcat-sentence--align-land f response)))
    (bcat-status-step file :align "Zuordnung angefragt")
    'fired))

(defun bcat-sentence--references-for (file sent-num source-file)
  "The reference translations C-c u G sends for sentence SENT-NUM: the
reference files' plus every one added by hand under Translations
compared of FILE."
  (let ((refs (bcat-sentence--references sent-num source-file)))
    (append refs
            (bcat-sentence--extra-references (mapcar #'car refs)
                                             (bcat-sentence--compared file)))))

(defun bcat-sentence--prepare (file sentence source-file)
  "Make FILE a file of its source's template before a fetch: create a
missing one, and rebuild one still in the old layout (Carsten:
„komplett neu bauen“) or in another template (30.10.: a Sanskrit file
whose source became bilingual) — user slots, #+CREATED, the file's own
sections and references added by hand carried over.  Returns
`created', `rebuilt' or nil (the file was in its template already)."
  (let ((sent-num (plist-get sentence :sent-num))
        (segs (plist-get sentence :segs)))
    (cond
     ((not (file-exists-p file))
      (tibetan-cascade--create-file sent-num segs source-file)
      'created)
     ((not (equal (bcat-sentence-file-p file) (bcat-sentence-template-name source-file)))
      (bcat-sentence--save-visit file)
      (let ((names (mapcar #'car (bcat-sentence--references sent-num source-file))))
        (bcat-sentence-create-file
         sent-num segs source-file file
         (bcat-sentence--extra-references names (append (bcat-sentence--old-references file)
                                                        (bcat-sentence--compared file)))
         (bcat-sentence--keep file)))
      'rebuilt))))

(declare-function tibetan-dharmamitra-api-limit-reached-p "tibetan-dharmamitra-api" ())

(defun bcat-sentence--dm-status (translation)
  "The status text of DharmaMitra's TRANSLATION (nil: none) — with the
reason when its daily limit is reached (30.10.: only „–“, the reason
in *Messages*)."
  (cond (translation "DharmaMitra ✓")
        ((and (fboundp 'tibetan-dharmamitra-api-limit-reached-p)
              (tibetan-dharmamitra-api-limit-reached-p))
         "DharmaMitra: Tageslimit erreicht")
        (t "DharmaMitra –")))

(defun bcat-sentence--dharmamitra (file sentence)
  "DharmaMitra's translation of SENTENCE for FILE, or nil (an outage
must not stop the analysis); never with an org link in it."
  (condition-case err
      (progn
        (require 'tibetan-dharmamitra-api)
        (require 'tibetan-dharmamitra-translation)
      (let ((tr (tibetan-dharmamitra-api-chat-translate
                 (mapconcat #'cdr (plist-get sentence :segs) "")
                 :target-lang (tibetan-dharmamitra-translation--target-lang file)
                 :input-encoding (tibetan-dharmamitra-translation--input-encoding file))))
        (and (stringp tr) (not (string-blank-p tr)) (bcat-analysis-no-links (string-trim tr)))))
    (error (message "DharmaMitra (Satz %s): %s" (plist-get sentence :sent-num)
                    (error-message-string err))
           nil)))

(defun bcat-sentence--second-language (sentence source-file)
  "The target language code when SENTENCE of SOURCE-FILE gets the
translation in the second language — its `:CAT_SECOND_LANGUAGE:' or the
source's #+CAT_SECOND_LANGUAGE (Carsten 1.11.: „nicht als default …,
sondern nur als Option (wahrscheinlich sogar auf Satzebene)“) —, else
nil."
  (let ((header (bcat-sentence--header-value source-file "CAT_SECOND_LANGUAGE")))
    (when (or (plist-get sentence :second-language)
              (and header (not (member (downcase header) '("" "nil" "no" "nein")))))
      (or (plist-get (ignore-errors (tibetan-analysis--read-source-metadata source-file))
                     :target-lang)
          "en"))))

(defun bcat-sentence--context-with-zettels (meta segs source-file &optional lines)
  "The document's #+TIBETAN_CLAUDE_CONTEXT lines (from META, or LINES)
and, Carsten 5.11., the zettels of the terms in SEGS ((NUM . TEXT) …) of
SOURCE-FILE (`bcat-zettel-prompt-block') — the concept notes build on
them."
  (let ((block (and (fboundp 'bcat-zettel-prompt-block)
                    (bcat-zettel-prompt-block (mapcar #'cdr segs) source-file))))
    (append (or lines (plist-get meta :claude-context)) (and block (list block)))))

(defun bcat-sentence--sanskrit-checklist (source-file)
  "The checklist for the Sanskrit analysis that a fetch for SOURCE-FILE
sends in its system prompt (Carsten 20.11.: „ja, beides so machen“ —
23 rules from the mistakes of TĀ 1.1 with the Viveka), or nil: the text
of checklist-sanskrit.org without its comment, found like a template —
in the folder of SOURCE-FILE first (a list of its own replaces the
tool's), then the tool's Templates/analysis/."
  (when (bcat-template-find "checklist-sanskrit" source-file)
    (let ((text (string-trim (bcat-template-read "checklist-sanskrit" source-file))))
      (unless (string-empty-p text) text))))

(defun bcat-sentence--outline-block (sentence source-file)
  "What the request for SENTENCE of SOURCE-FILE is told of the outline
of the document (`bcat-outline-request-block'), or nil when it has none
or the outline does not know the sentence — the sentences before and
after it are those of the source, whatever their numbers (the Viveka's
1009 follows verse 1)."
  (let ((text (bcat-outline-text source-file)))
    (when text
      (let* ((all (bcat-sentence--sentences source-file))
             (num (plist-get sentence :sent-num))
             (pos (cl-position num all :key (lambda (s) (plist-get s :sent-num)))))
        (bcat-outline-request-block
         text num
         (and pos (> pos 0) (plist-get (nth (1- pos) all) :sent-num))
         (and pos (plist-get (nth (1+ pos) all) :sent-num)))))))

(defun bcat-sentence--outline-report (file sentence source-file parsed)
  "After the answer PARSED for SENTENCE of SOURCE-FILE was written into
FILE: say in the status and as a message what touches the outline — the
answer's `outline_note' (its analysis contradicts the outline's line),
and a line of the outline for this sentence still marked „wohl“ or
„nicht geprüft“: with the analysis at hand it can be checked.  The
outline is not changed (Tantrāloka-Sitzung 20.11.: „Die Gliederung
ändert es nicht still“)."
  (let ((text (bcat-outline-text source-file)))
    (when text
      (let* ((num (plist-get sentence :sent-num))
             (note (bcat-analysis--field parsed 'outline_note))
             (entry (cdr (assq num (bcat-outline-entries text))))
             (parts (delq nil
                          (list (unless (string-empty-p note) (concat "Gliederung prüfen: " note))
                                (and entry (string-match-p "\\_<wohl\\_>\\|nicht geprüft" entry)
                                     "Zeile der Gliederung noch vorläufig („wohl“) — jetzt prüfen")))))
        (when parts
          (let ((said (string-join parts " · ")))
            (message "Satz %s: %s" num said)
            (bcat-status-step file :outline said)))))))

(defun bcat-sentence--prompts (sentence source-file refs &optional file)
  "(SYSTEM . USER) for SENTENCE of SOURCE-FILE with the references REFS —
the Sanskrit prompts for a Sanskrit source, the Tibetan ones else; the
lectures on the sentence (#+CAT_LECTURES) for the comparison of the
translations — Carsten 2.11.: „Ja, nur beim Vergleich“.  FILE, the
sentence file the answer goes into: when its analysis is reviewed, it
goes with the request (20.11., `bcat-sentence--reviewed-block')."
  (let* ((meta (ignore-errors (tibetan-analysis--read-source-metadata source-file)))
         (target (or (plist-get meta :target-lang) "en"))
         (second (bcat-sentence--second-language sentence source-file))
         (lectures (bcat-sentence--lectures (plist-get sentence :sent-num) source-file))
         (segs (plist-get sentence :segs))
         ;; 16.11.: the sentences before and after, for „Worum es geht“
         (context (append (bcat-sentence--context-with-zettels meta segs source-file)
                          (let ((block (bcat-sentence--neighbours-block sentence source-file)))
                            (and block (list block)))
                          ;; 20.11.: where the sentence stands in the outline
                          (let ((block (bcat-sentence--outline-block sentence source-file)))
                            (and block (list block)))
                          ;; 20.11.: the reviewed analysis, whose decisions stand
                          (let ((block (and file (bcat-sentence--reviewed-block file))))
                            (and block (list block))))))
    (if (bcat-sentence--sanskrit-p source-file)
        (let ((verse-mode (bcat-sentence--verse-mode-p source-file)))
          (cons (bcat-analysis-sanskrit-system-prompt
                 target verse-mode (equal (bcat-sentence-template-name source-file) "sanskrit")
                 (bcat-sentence--sanskrit-checklist source-file))
                (bcat-analysis-sanskrit-user-prompt
                 (plist-get sentence :sent-num) (plist-get sentence :verse)
                 (mapcar (lambda (s) (list (car s) (cdr s))) segs)
                 refs
                 context
                 (and verse-mode (bcat-sentence--bhasya-texts (plist-get sentence :sent-num)
                                                              source-file))
                 second lectures
                 (bcat-sentence--metre-name segs))))
      (cons (bcat-analysis-system-prompt target)
            (bcat-analysis-user-prompt
             (plist-get sentence :sent-num) (plist-get sentence :verse)
             (mapcar (lambda (s) (list (car s) (cdr s) (bcat-sentence--wylie (cdr s)))) segs)
             refs
             context
             second lectures)))))

(defun bcat-sentence--write-analysed (file sent-num segs source-file build)
  "Write the template FILE of sentence SENT-NUM (SEGS, from SOURCE-FILE)
from the fields BUILD makes of its rump, and return them.  User slots,
#+CREATED and the file's own sections are read from FILE NOW (typing
during a request survives); LAST_ANALYZED is today."
  (bcat-sentence--save-visit file)
  (let* ((base (bcat-sentence-rump-context sent-num segs source-file nil
                                           (bcat-sentence--keep file)))
         (context (funcall build (cons (cons 'last-analyzed (format-time-string "%Y-%m-%d"))
                                       base))))
    ;; 20.11.: a reviewed file is kept before the new analysis takes
    ;; its place, and the status says how far the new one departs
    (let ((bcat-sentence--fresh (or (bcat-sentence--keep-reviewed file) t)))
      (bcat-sentence--write
       file (bcat-sentence--render (bcat-sentence-template-name source-file) context source-file)))
    (bcat-sentence--report-departures file)
    context))

(defun bcat-sentence--fetched-context (base sentence source-file refs parsed &optional tibetan)
  "The fields of SENTENCE of SOURCE-FILE from its rump BASE and Claude's
answer PARSED — for a bilingual sentence also TIBETAN, the answer of the
Tibetan request; REFS the references sent with the request."
  (let ((context (funcall (if (bcat-sentence--sanskrit-p source-file)
                              #'bcat-analysis-sanskrit-context
                            #'bcat-analysis-context)
                          base parsed refs)))
    (if tibetan
        (let ((both (bcat-analysis-bilingual-context context tibetan)))
          (bcat-sentence--commentary-fields (plist-get sentence :sent-num) source-file both
                                            (alist-get 'assignment both)))
      context)))

(defun bcat-sentence--answers-file (file)
  "Where the answers for the sentence FILE are kept: .answers/ beside it."
  (expand-file-name (concat ".answers/" (file-name-base file) ".eld")
                    (file-name-directory file)))

(defun bcat-sentence--keep-answers (file record)
  "Keep RECORD — (:parsed P :refs R [:tibetan T :trefs TR]), the answers
FILE was built from and the references sent — beside FILE as Lisp data
\(Carsten 31.10.: a later change of the template should need no new
fetch; C-c u L fills from it what the file does not show).  Never
signals: keeping is no reason to lose a landing."
  (condition-case err
      (let ((path (bcat-sentence--answers-file file)))
        (make-directory (file-name-directory path) t)
        (let ((coding-system-for-write 'utf-8-unix)
              (print-length nil) (print-level nil)
              (print-escape-nonascii nil) (print-escape-multibyte nil))
          (with-temp-file path
            (insert ";; -*- mode: lisp-data; coding: utf-8 -*-\n"
                    ";; Buddhist CAT: Claude's answer for " (file-name-nondirectory file)
                    " (" (format-time-string "%Y-%m-%d") ") — read by C-c u L.\n")
            (prin1 record (current-buffer))
            (insert "\n"))))
    (error (message "Antwort für %s nicht aufbewahrt: %s"
                    (file-name-nondirectory file) (error-message-string err)))))

(defun bcat-sentence--answers (file)
  "The answers kept for the sentence FILE (`bcat-sentence--keep-answers'),
or nil."
  (let ((path (bcat-sentence--answers-file file)))
    (when (file-readable-p path)
      (ignore-errors
        (with-temp-buffer
          (insert-file-contents path)
          (goto-char (point-min))
          (read (current-buffer)))))))

(defun bcat-sentence--write-fetched (file sentence source-file refs parsed &optional tibetan)
  "Write the template FILE of SENTENCE built from Claude's answer PARSED
— and for a bilingual sentence from TIBETAN, the answer of the Tibetan
request — and return its fields.  The references under Translations
compared are REFS as sent, marked from the answer's segment parts."
  (prog1 (bcat-sentence--write-analysed
          file (plist-get sentence :sent-num) (plist-get sentence :segs) source-file
          (lambda (base)
            (bcat-sentence--fetched-context base sentence source-file refs parsed tibetan)))
    ;; 20.11. (TĀ 1012: „kalayет“): letters of another script in a word
    ;; are named, not changed
    (let ((mixed (delete-dups (append (bcat-analysis-mixed-script-words parsed)
                                      (bcat-analysis-mixed-script-words tibetan)))))
      (when mixed
        (message "Satz %s: Wörter mit fremden Schriftzeichen — %s"
                 (plist-get sentence :sent-num) (string-join mixed ", "))
        (bcat-status-step file :script (concat "Zeichen prüfen: " (string-join mixed ", ")))))
    ;; 20.11.: what the answer says against the outline
    (bcat-sentence--outline-report file sentence source-file parsed)))

(defun bcat-sentence--write-partner (sentence source-file tibetan trefs sanskrit &optional hint assignment sanskrit-translation bhasya-translation)
  "Write the Tibetan file of the bilingual SENTENCE of the paired
SOURCE-FILE (Carsten 30.10.: „das volle Programm“): the Tibetan analysis
from TIBETAN, the answer of the Tibetan request (TREFS the references
sent with it), with the Sanskrit segments SANSKRIT as the Sanskrit file
has them, HINT the hint on the bhāṣya from the Sanskrit answer,
ASSIGNMENT the Tibetan segments' Sanskrit ones (for the commentary
links), SANSKRIT-TRANSLATION the Sanskrit sentence's (its part, 1.11.).
Returns the file."
  (let* ((n (plist-get sentence :sent-num))
         (parallel (bcat-sentence-parallel-source source-file))
         ;; The Tibetan sentence's own number (MSA: 64 for the Sanskrit 36).
         (other (bcat-sentence--parallel-sentence n source-file))
         (tfile (bcat-sentence-partner-file n source-file)))
    (bcat-sentence--write-analysed
     tfile (plist-get other :sent-num) (plist-get other :segs) parallel
     (lambda (base)
       (cons (cons 'bhasya-hint hint)
             (cons (cons 'bhasya-translation bhasya-translation)
             (bcat-sentence--commentary-fields
              (plist-get other :sent-num) parallel
              (bcat-analysis-partner-context (bcat-analysis-context base tibetan trefs)
                                             tibetan sanskrit sanskrit-translation)
              assignment)))))
    tfile))

(defun bcat-sentence--padapatha-warnings (file sentence source-file parsed)
  "Say which segments of the Sanskrit SENTENCE have a padapāṭha in
Claude's answer PARSED that leaves out or adds to the text — a message
with sentence and segment, the status of FILE (16.11., Carsten: „nicht
still korrigieren“; the file shows „*Prüfen:*“ under ⟦N⟧).  Returns the
segment numbers."
  (when (bcat-sentence--sanskrit-p source-file)
    (let ((bad '()))
      (dolist (s (plist-get sentence :segs))
        (let* ((c (cl-find (car s) (alist-get 'segments parsed) :key #'bcat-analysis--num))
               (line (and c (bcat-analysis-padapatha-line (cdr s) (alist-get 'words c) t)))
               (check (and line (bcat-analysis-padapatha-check (cdr s) line))))
          (when check
            (message "Satz %d ⟦%d⟧: %s" (plist-get sentence :sent-num) (car s) check)
            (push (car s) bad))))
      (when bad
        (setq bad (nreverse bad))
        (bcat-status-step file :check
                          (concat "Padapāṭha prüfen: "
                                  (mapconcat (lambda (n) (format "⟦%d⟧" n)) bad ", "))))
      bad)))

(defun bcat-sentence--land (file sentence source-file refs response)
  "Write Claude's RESPONSE for SENTENCE into the template FILE; nil and
an untouched file when the answer is unreadable.  A bilingual sentence
with a Tibetan translation goes on to the Tibetan request, which
finishes the file; any other ends here (C-c u H)."
  (let* ((read (bcat-analysis-read response))
         (parsed (car read)))
    (if (not parsed)
        (bcat-sentence--unreadable "Antwort" file response (cdr read))
      (bcat-sentence--write-fetched file sentence source-file refs parsed)
      (bcat-sentence--keep-answers file (list :parsed parsed :refs refs))
      (bcat-status-step file :claude "Claude ✓")
      (bcat-sentence--padapatha-warnings file sentence source-file parsed)
      (if (bcat-sentence--parallel-segs (plist-get sentence :sent-num) source-file)
          (bcat-sentence--tibetan-one file sentence source-file refs parsed)
        (run-hook-with-args 'bcat-sentence-after-fetch-functions file))
      file)))

(defun bcat-sentence--tibetan-prompts (sentence source-file parsed trefs)
  "(SYSTEM . USER) of the Tibetan request for SENTENCE of the bilingual
SOURCE-FILE: the Sanskrit segments with the translations of the
Sanskrit answer PARSED, the Tibetan ones with Wylie, TREFS the
reference translations of the Tibetan, the Tibetan source's
#+TIBETAN_CLAUDE_CONTEXT and the zettels of the Tibetan terms
\(`bcat-sentence--context-with-zettels')."
  (let* ((n (plist-get sentence :sent-num))
         (meta (ignore-errors (tibetan-analysis--read-source-metadata source-file)))
         (partner (bcat-sentence-parallel-source source-file))
         (pmeta (ignore-errors (tibetan-analysis--read-source-metadata partner)))
         (tsegs (bcat-sentence--parallel-segs n source-file))
         (own (mapcar (lambda (c) (cons (bcat-analysis--num c) (bcat-analysis--field c 'translation)))
                      (alist-get 'segments parsed))))
    (cons (bcat-analysis-tibetan-system-prompt (or (plist-get meta :target-lang) "en"))
          (bcat-analysis-tibetan-user-prompt
           n (plist-get sentence :verse)
           (mapcar (lambda (s) (list (car s) (cdr s) (cdr (assq (car s) own))))
                   (plist-get sentence :segs))
           (mapcar (lambda (s) (list (car s) (cdr s) (bcat-sentence--wylie (cdr s))))
                   tsegs)
           trefs
           ;; 5.11.: the zettels of the Tibetan terms too — Claude writes
           ;; the Tibetan concept notes; the tradition from the Tibetan text.
           (bcat-sentence--context-with-zettels
            nil tsegs partner
            (or (plist-get pmeta :claude-context) (plist-get meta :claude-context)))
           (bcat-sentence--second-language sentence source-file)))))

(defun bcat-sentence--tibetan-land (file sentence source-file refs parsed trefs response)
  "Finish the bilingual FILE with the Tibetan answer RESPONSE (PARSED the
Sanskrit answer, REFS and TREFS the references sent with the two
requests) — in a pair the Tibetan file too —, then C-c u H, for the
Sanskrit file only (Carsten 30.10.: „nur die zweisprachige“); nil and
FILE as the Sanskrit answer left it when the answer is unreadable."
  (let* ((read (bcat-analysis-read response))
         (tibetan (car read)))
    (if (not tibetan)
        (bcat-sentence--unreadable "Tibetisch" file response (cdr read))
      (let ((context (bcat-sentence--write-fetched file sentence source-file refs parsed tibetan)))
        (bcat-sentence--keep-answers file (list :parsed parsed :refs refs :tibetan tibetan :trefs trefs))
        (when (bcat-sentence--paired-p source-file)
          (bcat-sentence--write-partner sentence source-file tibetan trefs
                                        (alist-get 'segments context)
                                        (alist-get 'bhasya-hint context)
                                        (alist-get 'assignment context)
                                        (alist-get 'translation context)
                                        (alist-get 'bhasya-translation context))))
      (bcat-status-step file :tibetan "Tibetisch ✓")
      (run-hook-with-args 'bcat-sentence-after-fetch-functions file)
      file)))

(defun bcat-sentence--tibetan-one (file sentence source-file refs parsed)
  "Queue the Tibetan request of the bilingual SENTENCE (Carsten 30.10.)
once the Sanskrit answer PARSED is in FILE: the whole Tibetan analysis,
which Tibetan segments translate which Sanskrit one, the differences,
the rendering.  Its references are those of the Tibetan sentence — in a
pair with the ones added by hand to the Tibetan file and DharmaMitra on
the Tibetan."
  (let* ((n (plist-get sentence :sent-num))
         (other (bcat-sentence--parallel-sentence n source-file))
         (tfile (bcat-sentence-partner-file n source-file))
         (trefs (bcat-sentence--references-for tfile (plist-get other :sent-num)
                                               (bcat-sentence-parallel-source source-file)))
         (dm (and (bcat-sentence--paired-p source-file)
                  (bcat-sentence--dharmamitra tfile other)))
         (trefs (if dm (append trefs (list (cons "DharmaMitra" dm))) trefs)))
    (require 'tibetan-translation-discussion)
    ;; Before the submit: the landing (later, or at once) overwrites it.
    (bcat-status-step file :tibetan "Tibetisch angefragt")
    (tibetan-discussion--submit
     "bcat-tibetan" file
     (lambda () (bcat-sentence--tibetan-prompts sentence source-file parsed trefs))
     (lambda (f response)
       (bcat-sentence--tibetan-land f sentence source-file refs parsed trefs response)))))

(defun bcat-sentence--prepare-partner (sentence source-file)
  "`bcat-sentence--prepare' for the Tibetan file of SENTENCE of the
paired SOURCE-FILE; nil when the Tibetan lacks the sentence."
  (let* ((n (plist-get sentence :sent-num))
         (other (bcat-sentence--parallel-sentence n source-file)))
    (when (plist-get other :segs)
      (bcat-sentence--prepare (bcat-sentence-partner-file n source-file)
                              other
                              (bcat-sentence-parallel-source source-file)))))

(defun bcat-sentence--pair-state (file sentence source-file)
  "What C-c u G still has to do for SENTENCE of the paired SOURCE-FILE
\(FILE its Sanskrit file): `bcat-sentence-fetch-state' — and `full'
while the Tibetan file has no analysis: both files come from the same
two requests."
  (let* ((n (plist-get sentence :sent-num))
         (tsegs (bcat-sentence--parallel-segs n source-file)))
    ;; The partner's missing DharmaMitra alone is no reason for both
    ;; Claude requests again (30.10.) — nor is a reference there that no
    ;; segment shows (22.11., MSA XI.15: „Lévi 1911“ unplaced in the
    ;; Tibetan file — both files were analysed anew on every C-c u G,
    ;; and the reference stayed unplaced).  A filled analysis is renewed
    ;; on request only (C-u C-c u G).
    (if (and tsegs (eq 'full (bcat-sentence-fetch-state (bcat-sentence-partner-file n source-file)
                                                        (mapcar #'car tsegs))))
        'full
      (bcat-sentence-fetch-state file (mapcar #'car (plist-get sentence :segs))))))

(defun bcat-sentence--add-compared (file name text)
  "Add the paragraph *NAME:* TEXT to ** Translations compared in FILE:
at the end of the translations (before the next heading); a machine
translation (`bcat-analysis-machine-references', 18.11.) last, after the
points for the discussion, under its own heading."
  (let ((content (bcat-sentence--text file)))
    (with-temp-buffer
      (insert content)
      (goto-char (point-min))
      (when (re-search-forward "^\\*\\* Translations compared\n" nil t)
        (if (member name bcat-analysis-machine-references)
            (progn
              (goto-char (if (re-search-forward "^\\*\\{1,2\\} " nil t) (line-beginning-position) (point-max)))
              (skip-chars-backward " \t\n")
              (insert (format "\n\n*** %s\n*%s:* %s" (format (bcat-analysis--comparison-label 'machine) name)
                              name text)))
          (goto-char (if (re-search-forward "^\\*+ " nil t) (line-beginning-position) (point-max)))
          (skip-chars-backward " \t\n")
          (insert (format "\n\n*%s:* %s" name text)))
        (bcat-sentence--save-visit file)
        (bcat-sentence--write file (buffer-string))))))

(defun bcat-sentence--dm-one (file sentence source-file)
  "DharmaMitra was missing in the analysed FILE (30.10.: its daily limit):
fetch it alone (staggered like every sentence-level DharmaMitra call);
with a translation add it under Translations compared and place it with
ONE small alignment request — no new analysis; without, the status says
why and nothing goes to Claude."
  (let ((delay (if (fboundp 'tibetan-sentence-claude-dm-delay)
                   (tibetan-sentence-claude-dm-delay)
                 0)))
    (bcat-status-step file :dm "DharmaMitra …")
    (run-at-time
     delay nil
     (lambda ()
       (let ((dm (bcat-sentence--dharmamitra file sentence)))
         (bcat-status-step file :dm (bcat-sentence--dm-status dm))
         (when dm
           (bcat-sentence--add-compared file "DharmaMitra" dm)
           (bcat-sentence--align-one file sentence source-file)))))
    'fired))

(defun bcat-sentence--fetch-one (file sentence source-file)
  "Schedule the analysis of SENTENCE into FILE: DharmaMitra first
\(staggered like every sentence-level DharmaMitra call), then ONE Claude
request through the queue."
  (let ((delay (if (fboundp 'tibetan-sentence-claude-dm-delay)
                   (tibetan-sentence-claude-dm-delay)
                 0)))
    (bcat-status-step file :dm "DharmaMitra …")
    (run-at-time
     delay nil
     (lambda ()
       (let* ((refs (bcat-sentence--references-for file (plist-get sentence :sent-num)
                                                   source-file))
              (dm (bcat-sentence--dharmamitra file sentence))
              (refs (if dm (append refs (list (cons "DharmaMitra" dm))) refs)))
         (bcat-status-step file :dm (bcat-sentence--dm-status dm))
         ;; Before the submit: the landing (later, or at once) overwrites it.
         (bcat-status-step file :claude "Claude angefragt")
         (require 'tibetan-translation-discussion)
         (tibetan-discussion--submit
          "bcat-analysis" file
          (lambda () (bcat-sentence--prompts sentence source-file refs file))
          (lambda (f response)
            (bcat-sentence--land f sentence source-file refs response))))))
    'fired))

(defun bcat-sentence-fetch (scope &optional force)
  "C-c u G for a source with a template (§5.74): every sentence of
SCOPE gets its file in the template form (created, or rebuilt from the
old layout) and — when still incomplete — one analysis (DharmaMitra,
then Claude).  A filled file is left alone — FORCE (C-u C-c u G)
analyses it anew, after a question naming the count (29.10., TĀ 1007:
nothing changed and nothing said so).  More than
`tibetan-cascade-subtree-confirm-threshold' requests ask first.
In the Tibetan source of a bilingual pair it runs through the Sanskrit
one: the Tibetan request needs the Sanskrit answer, and both files come
from the same requests.  Returns (:sentences N :fired N :complete N
:align N)."
  (let* ((given (plist-get scope :source))
         ;; A commentary (MSA, 30.10.): the verses its Sections cover.
         (from-commentary (and (equal (bcat-sentence-template-name given) "commentary")
                               (progn (require 'bcat-commentary) (bcat-commentary-lead given))))
         (scope (if from-commentary
                    (list :source from-commentary :label (plist-get scope :label)
                          :sent-nums (delete-dups
                                      (cl-loop for n in (plist-get scope :sent-nums)
                                               for sec = (plist-get (bcat-sentence--sentence n given) :section-label)
                                               append (and sec (mapcar (lambda (v) (plist-get v :sent-num))
                                                                       (bcat-commentary--verses sec from-commentary given))))))
                  scope))
         (given (plist-get scope :source))
         (redirect (and (equal (bcat-sentence-template-name given) "bilingual-tibetan")
                        (bcat-sentence--paired-p given)))
         (src (if redirect (bcat-sentence-parallel-source given) given))
         ;; 22.11.: every commentary named — the bhāṣya of the Tibetan
         ;; source, the commentator(s)
         (commentaries (and (bcat-sentence--verse-mode-p src)
                            (progn (require 'bcat-commentary) (bcat-commentary-sources src))))
         (pair (bcat-sentence--paired-p src))
         (label (plist-get scope :label))
         ;; From the Tibetan source: its sentence → its verse (verse
         ;; mode) → the Sanskrit sentence that goes with it.
         (nums (if redirect
                   (delq nil (mapcar (lambda (n)
                                       (plist-get (bcat-sentence--parallel-sentence
                                                   (or (plist-get (bcat-sentence-owner n given) :sent-num) n)
                                                   given)
                                                  :sent-num))
                                     (plist-get scope :sent-nums)))
                 (plist-get scope :sent-nums)))
         (folder (file-name-as-directory
                  (expand-file-name "analysis" (file-name-directory src))))
         ;; Verse mode: a bhāṣya sentence stands for its verse.
         (sentences (delete-dups
                     (delq nil (mapcar (lambda (n) (bcat-sentence-owner n src)) nums))))
         (file-of (lambda (s) (tibetan-sentence--filepath (plist-get s :sent-num) folder src))))
    (bcat-status-begin (if force "C-u C-c u G" "C-c u G") label
                       (mapcar (lambda (s) (cons (funcall file-of s)
                                                 (format "Satz %d" (plist-get s :sent-num))))
                               sentences))
    (if (ignore-errors (tibetan-analysis--defer-mt-p src))
        (progn (message "%s: #+TIBETAN_DEFER_MT — kein Abruf" label)
               (dolist (s sentences)
                 (bcat-status-step (funcall file-of s) :state "#+TIBETAN_DEFER_MT — kein Abruf"))
               (list :sentences (length sentences) :fired 0 :complete 0 :align 0))
      (dolist (s sentences)
        (let ((done (delq nil (list (pcase (bcat-sentence--prepare (funcall file-of s) s src)
                                      ('created "angelegt") ('rebuilt "neu gebaut"))
                                    (and pair
                                         (pcase (bcat-sentence--prepare-partner s src)
                                           ('created "Tib. angelegt") ('rebuilt "Tib. neu gebaut")))))))
          (when done
            (bcat-status-step (funcall file-of s) :file (string-join done " · ")))))
      (let* (;; 22.11.: every Section of the commentary that covers the
             ;; verse (Phywa pa: an introduction to the group and the
             ;; Section on the pair) — those still to be analysed
             ;; … as ((COMMENTARY . SECTION) …)
             (open-of (lambda (s)
                        (cl-loop for commentary in commentaries
                                 append (mapcar
                                         (lambda (sec) (cons commentary sec))
                                         (cl-remove-if-not
                                          (lambda (sec) (or force (bcat-commentary-incomplete-p sec commentary)))
                                          (bcat-commentary-sections (plist-get s :section-label)
                                                                    src commentary))))))
             (states (mapcar (lambda (s)
                               (cons s (or (cond
                                            (force 'full)
                                            (pair (bcat-sentence--pair-state (funcall file-of s) s src))
                                            (t (bcat-sentence-fetch-state
                                                (funcall file-of s) (mapcar #'car (plist-get s :segs)))))
                                           ;; The verse done, its commentary not.
                                           (and (funcall open-of s) 'commentary))))
                             sentences))
             (todo (cl-remove-if-not #'cdr states))
             (align (cl-count-if (lambda (c) (eq (cdr c) 'align)) states))
             (go (and todo
                      (cond
                       (force
                        (y-or-n-p (format "%s: %d %s neu analysieren — %s, deine Einträge bleiben? "
                                          label (length todo)
                                          (if (= 1 (length todo)) "Satz" "Sätze")
                                          (cond (pair "je zwei DharmaMitra- und zwei Claude-Abrufe (Sanskrit, Tibetisch)")
                                                ((bcat-sentence-parallel-source src)
                                                 "je ein DharmaMitra- und zwei Claude-Abrufe (Sanskrit, Tibetisch)")
                                                (t "je ein DharmaMitra- und ein Claude-Abruf")))))
                       ((<= (length todo) tibetan-cascade-subtree-confirm-threshold) t)
                       (t (y-or-n-p (format "%s: %d Sätze an Claude senden (%d neue Analyse(n), %d Zuordnung(en))? "
                                            label (length todo) (- (length todo) align) align))))))
             (fired 0))
        (dolist (c states)
          (cond ((null (cdr c))
                 (bcat-status-step (funcall file-of (car c)) :state
                                   "vollständig — nichts abgerufen (neu abrufen: C-u C-c u G)"))
                ((not go)
                 (bcat-status-step (funcall file-of (car c)) :state "Abruf abgelehnt"))))
        (when go
          (let ((open (mapcar (lambda (c) (cons (car c) (funcall open-of (car c)))) todo))
                (sections '()))
            ;; 5.11.: a commentary that comes with the verse — the
            ;; verse's landing leaves the dossier's PDF to it.  Noted
            ;; for every verse BEFORE anything is sent: a Section may
            ;; belong to several verses (22.11.), and each of them
            ;; waits for it.
            (when (fboundp 'bcat-dossier-await)
              (dolist (o open)
                (dolist (cs (cdr o))
                  (bcat-dossier-await (funcall file-of (car o)) (bcat-commentary-file (cdr cs) (car cs))))))
            ;; the verses …
            (dolist (c todo)
              (when (eq 'fired
                        (pcase (cdr c)
                          ('align (bcat-sentence--align-one (funcall file-of (car c)) (car c) src))
                          ('dm (bcat-sentence--dm-one (funcall file-of (car c)) (car c) src))
                          ('commentary 'fired)
                          (_ (bcat-sentence--fetch-one (funcall file-of (car c)) (car c) src))))
                (cl-incf fired)))
            ;; … then the commentary with them (Carsten 30.10.: „immer
            ;; beide“) — once per Section of the commentary.
            (dolist (o open)
              (dolist (cs (cdr o))
                (unless (member cs sections)
                  (push cs sections)
                  (bcat-commentary-fetch (cdr cs) (car cs) (funcall file-of (car o))))))))
        (if (null todo)
            (message "%s: vollständig — nichts abgerufen (neu abrufen: C-u C-c u G)" label)
          (message "%s: %d Satz/Sätze — %d abgerufen (davon %d Zuordnung neuer Vergleichsübersetzungen), %d vollständig%s%s"
                   label (length sentences) fired (if go align 0)
                   (cl-count-if-not #'cdr states)
                   (if (and todo (not go)) " (Abruf abgelehnt)" "")
                   ;; 20.11.: the outline is part of the workflow — a
                   ;; document without one says so
                   (if (bcat-outline-text src) ""
                     " — noch keine Gliederung des Dokuments: C-c u S entwirft sie")))
        (list :sentences (length sentences) :fired fired
              :complete (cl-count-if-not #'cdr states) :align align)))))

(provide 'bcat-sentence)
;;; bcat-sentence.el ends here
