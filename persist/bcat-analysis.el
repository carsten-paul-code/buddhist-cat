;;; bcat-analysis.el --- The C-c u G analysis for template-built files -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;;; Commentary:
;; §5.74 step 3 (Carsten 28.10.2026): one Claude request per sentence
;; yields everything the template shows — per segment the words
;; (gloss + function, so the tables lose their "?"), the segment's
;; share of the translation and of every reference translation (with
;; meaning-shifting passages marked), structure, an optional note and
;; concepts; for the sentence the grammar overview, the translation,
;; the marked references and a comparison.  "First the segments, then
;; the whole sentence/verse."  This module is pure: prompts, parser and
;; template fields — the C-c u G flow lives in bcat-sentence.el.
;;
;; The answer is one JSON object (robust to parse; Claude writes org
;; markup inside the strings: *bold* = meaning shift, /italic/ = Tibetan
;; words, =case= particles, ~converbs~).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'seq)

(declare-function tibetan-gloss-table--cell "tibetan-gloss-table" (s))
(declare-function tibetan-gloss-table--truncate-cell "tibetan-gloss-table" (gloss))
(declare-function tibetan-gloss-table--format-rows "tibetan-gloss-table" (rows))

;; ----------------------------------------------------------------------------
;; Prompts
;; ----------------------------------------------------------------------------

(defun bcat-analysis--language (target)
  "The language name for the target code TARGET (\"de\" → German)."
  (if (equal target "de") "German" "English"))

(defun bcat-analysis-system-prompt (target)
  "The system prompt of the sentence analysis for the target language
code TARGET — constant per document, so the prompt cache stays warm."
  (let ((lang (bcat-analysis--language target)))
    (format "You are a philologist preparing one sentence or verse of a Classical Tibetan Buddhist text for a university reading class. Work segment by segment, then on the whole sentence. Answer with ONE JSON object and nothing else (no prose, no code fence):

{\"segments\": [{\"num\": <segment number>, \"words\": [{\"wylie\": \"…\", \"gloss\": \"…\", \"function\": \"…\"}], \"translation\": \"…\", \"references\": [{\"name\": \"…\", \"text\": \"…\"}], \"shift\": \"…\", \"structure\": \"…\", \"note\": \"…\", \"concepts\": \"…\"}], \"grammar\": \"…\", \"translation\": \"…\", \"comparison\": \"…\"}

Rules:
- segments: one entry per given segment, same numbers, same order.
- words: the segment's words in order, together covering every syllable of its Wylie exactly once. A lexical compound or fixed verb phrase is one word (\"brjod du med pa\"); case and converb particles are words of their own. wylie: exactly as in the given Wylie. gloss: the meaning in THIS context, at most 25 characters, in %s. function: part of speech and syntactic function, compact — \"N · agent\", \"ADJ · predicate\", \"V pf. ('pong ba) · main verb\", \"DEM · object\", \"NEG + V pf. (byed pa)\"; a case or converb particle gets only its label: ERG, GEN, ABS, TERM, LOC, ABL, COM, DAT, or \"CONV · cond.\" etc.
- translation (segment): this segment's part of your translation; the parts read in order form the sentence translation. Give key terms with the Tibetan and the established Sanskrit in parentheses, e.g. \"uncompounded ('dus ma byas: asaṃskṛta)\".
- references (segment): for every reference translation given, the part that corresponds to this segment, quoted verbatim (keep its brackets and Sanskrit; not its notes). Mark with org bold *…* ONLY passages that shift the meaning against the Tibetan or against your reading; stylistic variation is not marked.
- shift: one line on what the marked passages shift, beginning with the translator name(s): \"Lindtner 1981: active, with the yogin as agent\"; \"\" when nothing is marked.
- structure: the segment's syntax in one line, org markup — case particles =yis=, converbs ~na~, Tibetan words /italic/: \"rnal 'byor pa =yis= [agent, ERG] · de [object, ABS] · rtogs [V] ~na~ [conditional] → protasis of 41.\"
- note: only for a grammatical peculiarity or a real ambiguity, one or two sentences; otherwise \"\".
- concepts: Buddhist technical concepts occurring in THIS segment — the term in *bold*, its Sanskrit, a short encyclopedic note of two or three sentences; \"\" when there is none.
- grammar: an overview of the whole sentence — verb backbone, clause linkage, agents, elisions.
- translation (sentence): your translation of the whole sentence (the segment parts joined).
- comparison: one to three sentences on the meaning-shifting differences between the translations including yours; \"\" when there are none.
- Write glosses, notes and explanations in %s. Never mention yourself."
            lang lang)))

(defun bcat-analysis-user-prompt (sent-num verse segs refs &optional context)
  "The user prompt for sentence SENT-NUM (VERSE its :VERSE: or nil):
SEGS ((NUM TIBETAN WYLIE) …), REFS ((NAME . TEXT) …) the reference
translations (DharmaMitra among them), CONTEXT the document's
#+TIBETAN_CLAUDE_CONTEXT lines."
  (concat
   (if context (concat (mapconcat #'identity context "\n") "\n\n") "")
   (format "Sentence %d%s — segments:\n" sent-num
           (if verse (format " (%s)" verse) ""))
   (mapconcat (lambda (s)
                (format "[%d] Tibetan: %s\n     Wylie: %s"
                        (nth 0 s) (string-trim (nth 1 s)) (nth 2 s)))
              segs "\n")
   "\n\nReference translations:\n"
   (if refs
       (mapconcat (lambda (r) (format "=== %s ===\n%s" (car r) (string-trim (cdr r))))
                  refs "\n")
     "none")
   "\n\nReturn the JSON object now."))

(defun bcat-analysis-align-system-prompt (target)
  "System prompt of the small request that places reference
translations added by hand (§5.74 step 3c) — target language TARGET."
  (let ((lang (bcat-analysis--language target)))
    (format "You align additional reference translations with the segments of a Classical Tibetan sentence that has already been analysed for a university reading class. Answer with ONE JSON object and nothing else:

{\"segments\": [{\"num\": <segment number>, \"references\": [{\"name\": \"…\", \"text\": \"…\"}], \"shift\": \"…\"}], \"comparison\": \"…\"}

Rules:
- segments: one entry per given segment; references: for every given reference translation the part that corresponds to this segment, quoted verbatim (not its notes). Mark with org bold *…* ONLY passages that shift the meaning against the Tibetan or against the given translation of the segment; stylistic variation is not marked.
- shift: one line on what the marked passages shift, beginning with the translator name; \"\" when nothing is marked.
- comparison: one or two sentences on the meaning-shifting differences of the new translation(s); \"\" when there are none.
- Write in %s. Never mention yourself."
            lang)))

(defun bcat-analysis-align-user-prompt (sent-num verse segs refs &optional context)
  "User prompt of the alignment request: SEGS ((NUM TIBETAN WYLIE
TRANSLATION) …) with the translation already in the file, REFS the new
reference translations ((NAME . TEXT) …)."
  (concat
   (if context (concat (mapconcat #'identity context "\n") "\n\n") "")
   (format "Sentence %d%s — segments:\n" sent-num (if verse (format " (%s)" verse) ""))
   (mapconcat (lambda (s)
                (format "[%d] Tibetan: %s\n     Wylie: %s\n     Translation: %s"
                        (nth 0 s) (string-trim (nth 1 s)) (nth 2 s) (or (nth 3 s) "")))
              segs "\n")
   "\n\nReference translations to align:\n"
   (mapconcat (lambda (r) (format "=== %s ===\n%s" (car r) (string-trim (cdr r)))) refs "\n")
   "\n\nReturn the JSON object now."))

;; ----------------------------------------------------------------------------
;; Parsing the answer
;; ----------------------------------------------------------------------------

(defcustom bcat-analysis-response-directory
  (expand-file-name "bcat-responses" temporary-file-directory)
  "Where an answer C-c u G could not read is kept, one file per answer
\(sentence file name + time) — outside the corpus, for diagnosis."
  :type 'directory
  :group 'buddhist-cat)

(defun bcat-analysis--json-problem (err start)
  "A short description of the JSON error ERR; positions count from the
start of the answer (the object began at START)."
  (let ((pos (car (last (seq-filter #'integerp (cdr err))))))
    (if pos
        (format "JSON-Fehler bei Zeichen %d" (+ start pos))
      "JSON-Fehler")))

(defun bcat-analysis--repair-json (text)
  "TEXT with the two defects Claude's JSON shows when it copies reference
texts verbatim mended: a raw control character inside a string is
escaped, and so is a quote inside a string that cannot end it — one not
followed by `:', `]', `}' or `,' plus the start of a value (Eimer's
\"Um die Hemmnisse, nämlich die \"Befleckungen\", …\" in Lindtner's
note).  Valid JSON comes back unchanged."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((in-string nil))
      (while (not (eobp))
        (let ((c (char-after)))
          (cond
           ((not in-string)
            (when (eq c ?\") (setq in-string t))
            (forward-char 1))
           ((eq c ?\\)
            (forward-char (min 2 (- (point-max) (point)))))
           ((eq c ?\")
            (if (save-excursion
                  (forward-char 1)
                  (looking-at "[ \t\n\r]*\\(?:[]:}]\\|\\'\\|,[ \t\n\r]*\\(?:[\"{[]\\|-?[0-9]\\|true\\_>\\|false\\_>\\|null\\_>\\)\\)"))
                (setq in-string nil)
              (insert "\\"))
            (forward-char 1))
           ((< c 32)
            (delete-char 1)
            (insert (pcase c
                      (?\n "\\n")
                      (?\t "\\t")
                      (?\r "\\r")
                      (_ (format "\\u%04x" c)))))
           (t (forward-char 1))))))
    (buffer-string)))

(defun bcat-analysis--decode (json)
  "JSON (a string) as an alist with symbol keys and lists for arrays."
  (json-parse-string json :object-type 'alist :array-type 'list
                     :null-object nil :false-object nil))

(defun bcat-analysis-read (response)
  "(OBJECT . PROBLEM) for Claude's RESPONSE: the JSON object as an alist
\(keys as symbols, arrays as lists) and nil, or nil and a short
description why there is none.  Tolerates a code fence or prose around
the object, and mends what `bcat-analysis--repair-json' mends."
  (if (not (and (stringp response) (string-match "{" response)))
      (cons nil "kein JSON-Objekt")
    (let* ((start (match-beginning 0))
           (end (string-match "}[^}]*\\'" response)))
      (if (not (and end (> end start)))
          (cons nil "JSON-Objekt unvollständig")
        (let* ((json (substring response start (1+ end)))
               (strict (condition-case err
                           (cons (bcat-analysis--decode json) nil)
                         (error (cons nil err))))
               (obj (or (car strict)
                        (and (cdr strict)
                             (ignore-errors
                               (bcat-analysis--decode (bcat-analysis--repair-json json)))))))
          (cond
           ((and (consp obj) (assq 'segments obj)) (cons obj nil))
           ((or obj (null (cdr strict))) (cons nil "JSON ohne segments"))
           (t (cons nil (bcat-analysis--json-problem (cdr strict) start)))))))))

(defun bcat-analysis-parse (response)
  "The JSON object in Claude's RESPONSE as an alist, or nil when there
is none (see `bcat-analysis-read')."
  (car (bcat-analysis-read response)))

(defun bcat-analysis-keep-response (file response)
  "Save RESPONSE, an answer for the sentence FILE that could not be
read, under `bcat-analysis-response-directory'; return its path."
  (make-directory bcat-analysis-response-directory t)
  (let ((path (expand-file-name
               (format "%s-%s.txt" (file-name-base file)
                       (format-time-string "%Y%m%d-%H%M%S"))
               bcat-analysis-response-directory)))
    (with-temp-file path (insert (or response "")))
    path))

;; ----------------------------------------------------------------------------
;; Template fields
;; ----------------------------------------------------------------------------

(defconst bcat-analysis--particle-labels
  '("ERG" "GEN" "ABS" "TERM" "LOC" "ABL" "COM" "DAT" "ALL" "INST" "CONV")
  "Function labels of case and converb particles — such a word gets no
dictionary link in the table.")

(defun bcat-analysis--particle-p (function)
  "Non-nil when FUNCTION (a word's function string) is a particle label."
  (and (stringp function)
       (string-match "\\`[ \t]*\\([A-Z]+\\)" function)
       (member (match-string 1 function) bcat-analysis--particle-labels)))

(defun bcat-analysis--field (alist key)
  "String value of KEY in ALIST, trimmed; \"\" when absent."
  (let ((v (alist-get key alist)))
    (if (stringp v) (string-trim v) "")))

(defun bcat-analysis-gloss-table (words)
  "The three-row table of WORDS (Claude's word list): row 1 Wylie (with
a Steinert link unless a particle), row 2 gloss, row 3 function.  \"\"
for no words."
  (if (null words)
      ""
    (let ((r1 '()) (r2 '()) (r3 '()))
      (dolist (w words)
        (let ((wy (bcat-analysis--field w 'wylie))
              (fn (bcat-analysis--field w 'function)))
          (push (if (bcat-analysis--particle-p fn)
                    (tibetan-gloss-table--cell wy)
                  (format "[[steinert:%s][%s]]" wy (tibetan-gloss-table--cell wy)))
                r1)
          (push (tibetan-gloss-table--cell
                 (or (tibetan-gloss-table--truncate-cell (bcat-analysis--field w 'gloss)) ""))
                r2)
          (push (tibetan-gloss-table--cell fn) r3)))
      (string-trim-right
       (tibetan-gloss-table--format-rows (list (nreverse r1) (nreverse r2) (nreverse r3)))))))

(defun bcat-analysis-glossed-line (wylie words)
  "WYLIE (one verse line, its shad at the end) with each of WORDS
\(alists with wylie, gloss, function) followed by its gloss in
brackets — a particle by its function label, an unknown (\"?\") or
empty gloss by nothing — and the shad after the last.  Carsten 29.10.:
„die Vokabeln in Klammern beim Text“, so a segment can be translated
from the verse without turning to its table.  WYLIE unchanged when
WORDS do not cover its syllables exactly: the text is never altered."
  (let* ((tail (if (string-match "\\(?:[ \t]*/\\)+[ \t]*\\'" wylie)
                   (match-string 0 wylie)
                 ""))
         (body (substring wylie 0 (- (length wylie) (length tail)))))
    (if (or (null words)
            (not (equal (split-string body)
                        (split-string (mapconcat (lambda (w) (bcat-analysis--field w 'wylie))
                                                 words " ")))))
        wylie
      (concat
       (mapconcat (lambda (w)
                    (let* ((wy (bcat-analysis--field w 'wylie))
                           (fn (bcat-analysis--field w 'function))
                           (gl (bcat-analysis--field w 'gloss))
                           (show (cond ((bcat-analysis--particle-p fn) fn)
                                       ((member gl '("" "?")) "")
                                       (t gl))))
                      (if (string-empty-p show) wy (format "%s [%s]" wy show))))
                  words " ")
       (if (string-empty-p tail) "" (concat " " (string-trim tail)))))))

(defun bcat-analysis--reference-lines (refs)
  "\"*NAME:* TEXT\" for each of REFS (alists with name and text)."
  (mapcar (lambda (r) (format "*%s:* %s" (bcat-analysis--field r 'name)
                              (bcat-analysis--field r 'text)))
          (cl-remove-if (lambda (r) (string-empty-p (bcat-analysis--field r 'text)))
                        refs)))

(defun bcat-analysis-strip-marks (text)
  "TEXT without org bold marks (the meaning-shift marks of a fetch)."
  (replace-regexp-in-string "\\*\\([^*\n]+\\)\\*" "\\1" text t))

(defun bcat-analysis--loose-regexp (text)
  "A regexp for TEXT that lets any run of whitespace stand for its spaces."
  (mapconcat #'regexp-quote (split-string text) "[ \t\n]+"))

(defun bcat-analysis--reference-parts (segments name)
  "The parts of the reference NAME in Claude's SEGMENTS, in segment
order — each with its passages in org bold."
  (delq nil
        (mapcar (lambda (seg)
                  (let ((r (cl-find name (alist-get 'references seg)
                                    :key (lambda (r) (bcat-analysis--field r 'name))
                                    :test #'equal)))
                    (and r (bcat-analysis--field r 'text))))
                (sort (copy-sequence segments)
                      (lambda (a b)
                        (< (string-to-number (format "%s" (alist-get 'num a)))
                           (string-to-number (format "%s" (alist-get 'num b)))))))))

(defun bcat-analysis-mark-reference (text parts)
  "TEXT, a reference translation as given (notes included), with the
meaning-shift marks of PARTS in org bold.  PARTS are Claude's parts of
that reference, segment by segment, their passages in org bold: each
part is looked for after the previous one and its passages are marked
where it stands — never at an earlier occurrence of the same words.
Whitespace may differ; a part or passage not found changes nothing.
The text itself is never taken from the answer (29.10.: Claude's copy
of Lindtner's verse 9 dropped his note)."
  (let ((case-fold-search nil) (cursor 0) (spans '()))
    (dolist (part parts)
      (let* ((plain (string-trim (bcat-analysis-strip-marks part)))
             (passages (let ((ps '()) (start 0))
                         (while (string-match "\\*\\([^*\n]+\\)\\*" part start)
                           (push (string-trim (match-string 1 part)) ps)
                           (setq start (match-end 0)))
                         (nreverse ps)))
             (region (and (not (string-empty-p plain))
                          (string-match (bcat-analysis--loose-regexp plain) text cursor)
                          (cons (match-beginning 0) (match-end 0))))
             (pos (if region (car region) cursor)))
        (dolist (p passages)
          (when (and (not (string-empty-p p))
                     (string-match (bcat-analysis--loose-regexp p) text pos)
                     (or (null region) (<= (match-end 0) (cdr region))))
            (push (cons (match-beginning 0) (match-end 0)) spans)
            (setq pos (match-end 0))))
        (setq cursor (if region (cdr region) pos))))
    ;; SPANS run from the last to the first: inserting from the end
    ;; keeps the earlier positions valid.
    (dolist (sp spans text)
      (setq text (concat (substring text 0 (car sp)) "*"
                         (substring text (car sp) (cdr sp)) "*"
                         (substring text (cdr sp)))))))

(defun bcat-analysis-translations-block (num seg)
  "The left-aligned translation block of segment NUM from SEG (Claude's
segment object): ⟦NUM⟧ own translation (labelled like the sentence's —
29.10.: unlabelled it did not read as a translation next to the named
references), the reference parts, the shift line — one line each, org
line breaks between."
  (let ((lines (append
                (let ((tr (bcat-analysis--field seg 'translation)))
                  (unless (string-empty-p tr) (list (format "⟦%d⟧ *Translation:* %s" num tr))))
                (bcat-analysis--reference-lines (alist-get 'references seg))
                (let ((sh (bcat-analysis--field seg 'shift)))
                  (unless (string-empty-p sh) (list (concat "→ " sh)))))))
    (mapconcat #'identity lines " \\\\\n")))

(defun bcat-analysis-comparison (parsed &optional refs)
  "Translations compared of the sentence: the own translation from
PARSED, each of REFS ((NAME . TEXT) …, the texts sent with the request)
with the marks of its segment parts, the comparison paragraph."
  (let ((segments (alist-get 'segments parsed)))
    (string-join
     (delq nil
           (append
            (let ((tr (bcat-analysis--field parsed 'translation)))
              (unless (string-empty-p tr) (list (concat "*Translation:* " tr))))
            (mapcar (lambda (r)
                      (let ((text (string-trim (or (cdr r) ""))))
                        (unless (string-empty-p text)
                          (format "*%s:* %s" (car r)
                                  (bcat-analysis-mark-reference
                                   text (bcat-analysis--reference-parts segments (car r)))))))
                    refs)
            (let ((c (bcat-analysis--field parsed 'comparison)))
              (unless (string-empty-p c) (list c)))))
     "\n\n")))

(defun bcat-analysis-context (base parsed &optional refs)
  "Template fields of a fetched sentence: BASE (the rump fields —
header, segments with num and wylie, user sections) completed from
PARSED (Claude's answer) and REFS, the reference translations sent
with the request ((NAME . TEXT) …).  Segments are matched by number; a
segment Claude skipped keeps its rump fields."
  (let ((by-num (mapcar (lambda (s) (cons (alist-get 'num s) s))
                        (alist-get 'segments parsed))))
    (append
     `((segments
        . ,(mapcar
            (lambda (seg)
              (let* ((num (alist-get 'num seg))
                     (c (cdr (assoc (if (stringp num) (string-to-number num) num)
                                    by-num))))
                (if (null c)
                    seg
                  (append
                   `((glossed . ,(let* ((wy (alist-get 'wylie seg))
                                        (g (and wy (bcat-analysis-glossed-line
                                                    wy (alist-get 'words c)))))
                                   ;; Words that do not cover the line:
                                   ;; whatever the rump had.
                                   (if (or (null g) (equal g wy))
                                       (or (alist-get 'glossed seg) wy)
                                     g)))
                     (gloss-table . ,(bcat-analysis-gloss-table (alist-get 'words c)))
                     (translations . ,(bcat-analysis-translations-block
                                       (if (stringp num) (string-to-number num) num) c))
                     (structure . ,(bcat-analysis--field c 'structure))
                     (note . ,(bcat-analysis--field c 'note))
                     (concepts . ,(bcat-analysis--field c 'concepts)))
                   seg))))
            (alist-get 'segments base)))
       (grammar . ,(bcat-analysis--field parsed 'grammar))
       (comparison . ,(bcat-analysis-comparison parsed refs)))
     base)))

(provide 'bcat-analysis)
;;; bcat-analysis.el ends here
