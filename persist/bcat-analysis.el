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

{\"segments\": [{\"num\": <segment number>, \"words\": [{\"wylie\": \"…\", \"gloss\": \"…\", \"function\": \"…\"}], \"translation\": \"…\", \"references\": [{\"name\": \"…\", \"text\": \"…\"}], \"shift\": \"…\", \"structure\": \"…\", \"note\": \"…\", \"concepts\": \"…\"}], \"grammar\": \"…\", \"translation\": \"…\", \"references\": [{\"name\": \"…\", \"text\": \"…\"}], \"comparison\": \"…\"}

Rules:
- segments: one entry per given segment, same numbers, same order.
- words: the segment's words in order, together covering every syllable of its Wylie exactly once. A lexical compound or fixed verb phrase is one word (\"brjod du med pa\"); case and converb particles are words of their own. wylie: exactly as in the given Wylie. gloss: the meaning in THIS context, at most 25 characters, in %s. function: part of speech and syntactic function, compact — \"N · agent\", \"ADJ · predicate\", \"V pf. ('pong ba) · main verb\", \"DEM · object\", \"NEG + V pf. (byed pa)\"; a case or converb particle gets only its label: ERG, GEN, ABS, TERM, LOC, ABL, COM, DAT, or \"CONV · cond.\" etc.
- translation (segment): this segment's part of your translation; the parts read in order form the sentence translation. Give key terms with the Tibetan and the established Sanskrit in parentheses, e.g. \"uncompounded ('dus ma byas: asaṃskṛta)\".
- references (segment): for every reference translation given, the part that corresponds to this segment, quoted verbatim (keep its brackets and Sanskrit). Mark with org bold *…* ONLY passages that shift the meaning against the Tibetan or against your reading; stylistic variation is not marked.
- shift: one line on what the marked passages shift, beginning with the translator name(s): \"Lindtner 1981: active, with the yogin as agent\"; \"\" when nothing is marked.
- structure: the segment's syntax in one line, org markup — case particles =yis=, converbs ~na~, Tibetan words /italic/: \"rnal 'byor pa =yis= [agent, ERG] · de [object, ABS] · rtogs [V] ~na~ [conditional] → protasis of 41.\"
- note: only for a grammatical peculiarity or a real ambiguity, one or two sentences; otherwise \"\".
- concepts: Buddhist technical concepts occurring in THIS segment — the term in *bold*, its Sanskrit, a short encyclopedic note of two or three sentences; \"\" when there is none.
- grammar: an overview of the whole sentence — verb backbone, clause linkage, agents, elisions.
- translation (sentence): your translation of the whole sentence (the segment parts joined).
- references (sentence): every reference translation in full, verbatim, with the same bold marks.
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

{\"segments\": [{\"num\": <segment number>, \"references\": [{\"name\": \"…\", \"text\": \"…\"}], \"shift\": \"…\"}], \"references\": [{\"name\": \"…\", \"text\": \"…\"}], \"comparison\": \"…\"}

Rules:
- segments: one entry per given segment; references: for every given reference translation the part that corresponds to this segment, quoted verbatim. Mark with org bold *…* ONLY passages that shift the meaning against the Tibetan or against the given translation of the segment; stylistic variation is not marked.
- shift: one line on what the marked passages shift, beginning with the translator name; \"\" when nothing is marked.
- references (top level): every given reference translation in full, verbatim, with the same marks.
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

(defun bcat-analysis-parse (response)
  "The JSON object in Claude's RESPONSE as an alist (keys as symbols,
arrays as lists), or nil when there is none.  Tolerates a code fence or
prose around the object."
  (when (and (stringp response)
             (string-match "{" response))
    (let* ((start (match-beginning 0))
           (end (string-match "}[^}]*\\'" response)))
      (when (and end (> end start))
        (condition-case nil
            (let ((obj (json-parse-string (substring response start (1+ end))
                                          :object-type 'alist
                                          :array-type 'list
                                          :null-object nil
                                          :false-object nil)))
              (and (consp obj) (assq 'segments obj) obj))
          (error nil))))))

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

(defun bcat-analysis--reference-lines (refs)
  "\"*NAME:* TEXT\" for each of REFS (alists with name and text)."
  (mapcar (lambda (r) (format "*%s:* %s" (bcat-analysis--field r 'name)
                              (bcat-analysis--field r 'text)))
          (cl-remove-if (lambda (r) (string-empty-p (bcat-analysis--field r 'text)))
                        refs)))

(defun bcat-analysis-translations-block (num seg)
  "The left-aligned translation block of segment NUM from SEG (Claude's
segment object): ⟦NUM⟧ own translation, the reference parts, the shift
line — one line each, org line breaks between."
  (let ((lines (append
                (let ((tr (bcat-analysis--field seg 'translation)))
                  (unless (string-empty-p tr) (list (format "⟦%d⟧ %s" num tr))))
                (bcat-analysis--reference-lines (alist-get 'references seg))
                (let ((sh (bcat-analysis--field seg 'shift)))
                  (unless (string-empty-p sh) (list (concat "→ " sh)))))))
    (mapconcat #'identity lines " \\\\\n")))

(defun bcat-analysis-comparison (parsed)
  "Translations compared of the sentence from PARSED: own translation,
the marked references, the comparison paragraph."
  (string-join
   (delq nil
         (append
          (let ((tr (bcat-analysis--field parsed 'translation)))
            (unless (string-empty-p tr) (list (concat "*Translation:* " tr))))
          (bcat-analysis--reference-lines (alist-get 'references parsed))
          (let ((c (bcat-analysis--field parsed 'comparison)))
            (unless (string-empty-p c) (list c)))))
   "\n\n"))

(defun bcat-analysis-context (base parsed)
  "Template fields of a fetched sentence: BASE (the rump fields —
header, segments with num and wylie, user sections) completed from
PARSED (Claude's answer).  Segments are matched by number; a segment
Claude skipped keeps its rump fields."
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
                   `((gloss-table . ,(bcat-analysis-gloss-table (alist-get 'words c)))
                     (translations . ,(bcat-analysis-translations-block
                                       (if (stringp num) (string-to-number num) num) c))
                     (structure . ,(bcat-analysis--field c 'structure))
                     (note . ,(bcat-analysis--field c 'note))
                     (concepts . ,(bcat-analysis--field c 'concepts)))
                   seg))))
            (alist-get 'segments base)))
       (grammar . ,(bcat-analysis--field parsed 'grammar))
       (comparison . ,(bcat-analysis-comparison parsed)))
     base)))

(provide 'bcat-analysis)
;;; bcat-analysis.el ends here
