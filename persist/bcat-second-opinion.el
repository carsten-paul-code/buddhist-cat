;;; bcat-second-opinion.el --- Second opinion on the ambiguities of a sentence  -*- lexical-binding: t -*-

;;; Commentary:
;; Carsten 27.11.2026, after the comparison of Sonnet 4.6, Opus 5.5 and
;; Fable 5.2 (WS26-27/Modellvergleich-2026-11-27): Opus 5.5 is the CAT's
;; model (`bcat-model'); asked „Kann Opus sagen, dass es unsicher ist und
;; Fable fragen?“, then: „Zweite Runde mit Mehrdeutigkeitsfeld und Befehl
;; „Fable fragen“ bauen“.
;;
;; The analysis names every place where the original admits more than
;; one grammatical analysis (the field `ambiguities', *** Ambiguities
;; under Translations compared — `bcat-analysis--ambiguities-block').  A
;; fact, not a feeling: TĀ 1012 ⟦695⟧ /kāryamṛta/ is /kārya-mṛta/ or
;; /kāri-amṛta/ — Opus followed Dyczkowski without a word, Fable read it
;; the other way.  After a fetch whose analysis names such places, ONE
;; short request goes to `bcat-second-opinion-model' (Fable).
;;
;; BLIND (Carsten 27.11.: „Blind fragen und Stelle straffen“): shown
;; Opus' analysis with its choice and reason, Fable agreed at ⟦695⟧ with
;; a false sandhi argument, though on its own it had read /kāri-amṛta/.
;; So the places go without the analysis: the original (segment by
;; segment from the source), the document's context, the place in the
;; work, the sentences around it, the published translations, and per
;; place the readings in no particular order, a judgement in brackets cut
;; off (`bcat-second-opinion--place') — and then („Nur die Formen
;; schicken“) only their forms, or their construction where they share
;; the form, without the analysis' glosses: its gloss of /kāri-amṛta/,
;; „the deathless made/caused“, was a weak version of the reading.  Fable
;; answers with the number of the reading it chooses and what it means;
;; whether the analysis chose it too is decided here and written beside
;; it: *** Second opinion (Fable 5.2, blind) —
;; nothing of the analysis is overwritten.  The PDF of the file waits for
;; it (`bcat-export-after-fetch' asks `bcat-second-opinion-pending-p').
;;
;; C-c u Z („Fable fragen“) asks for a REVIEW of the whole analysis, which
;; Fable then sees: *** Review (Fable 5.2), at most five points, each at
;; its word.  C-u C-c u Z asks blind on the named places again.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'tibetan-translation-discussion)

(declare-function bcat-analysis-read-object "bcat-analysis" (response))
(declare-function bcat-analysis--language "bcat-analysis" (code))
(declare-function bcat-sentence--plain-lines "bcat-sentence" (text))
(declare-function bcat-sentence--text "bcat-sentence" (file))
(declare-function bcat-sentence--write "bcat-sentence" (file text))
(declare-function bcat-sentence--save-visit "bcat-sentence" (file))
(declare-function bcat-sentence-file-p "bcat-sentence" (file))
(declare-function bcat-sentence--sentence "bcat-sentence" (sent-num file))
(declare-function bcat-sentence--neighbours-block "bcat-sentence" (sentence source-file))
(declare-function bcat-survey--references "bcat-survey" (comparison))
(declare-function bcat-status-step "bcat-status" (file key text))
(declare-function bcat-export-after-fetch "bcat-export" (file))
(declare-function tibetan-discussion-paper--temp-source-p "tibetan-discussion-paper" (file))
(declare-function tibetan-sentence--source-file-from-analysis "tibetan-sentence-persist" (file))
(declare-function tibetan-analysis--read-source-metadata "tibetan-analysis-claude" (file))
(defvar bcat-analysis-comparison-language)

(defcustom bcat-second-opinion-model "claude-fable-5-2"
  "The model that gives the second opinion on the ambiguities an analysis
names — Carsten 27.11.: Opus 5.5 analyses, Fable 5.2 is asked."
  :type 'string
  :group 'buddhist-cat)

(defcustom bcat-second-opinion-auto t
  "Non-nil: after a fetch whose analysis names ambiguities, ask
`bcat-second-opinion-model' (`bcat-second-opinion-after-fetch').  Never
when that model is the CAT's own (`bcat-model')."
  :type 'boolean
  :group 'buddhist-cat)

(defvar bcat-second-opinion--pending (make-hash-table :test #'equal)
  "Second opinions on their way, keyed by the truename of the sentence
file: `auto' (after a fetch — its export waits) or `manual' (C-c u Z).")

(defconst bcat-second-opinion--labels
  '(("de" (heading . "Zweite Meinung") (blind . "blind") (same . "wie die Analyse")
     (differs . "anders als die Analyse") (open . "offen") (silent . "keine Antwort")
     (review . "Durchsicht") (nothing . "Keine weiteren Punkte."))
    ("en" (heading . "Second opinion") (blind . "blind") (same . "as the analysis")
     (differs . "differs from the analysis") (open . "open") (silent . "no answer")
     (review . "Review") (nothing . "No further points.")))
  "The words of the second opinion and the review in the file.")

(defconst bcat-second-opinion--headings
  "\\(?:Second opinion\\|Zweite Meinung\\)"
  "The heading of a second opinion in either language.")

(defconst bcat-second-opinion--review-headings
  "\\(?:Review\\|Durchsicht\\)"
  "The heading of a review in either language.")

(defconst bcat-second-opinion--ambiguity-headings
  "\\(?:Ambiguities\\|Mehrdeutigkeiten\\)"
  "The heading of the ambiguities in either language.")

(defconst bcat-second-opinion--judgement
  (concat " *([^()]*\\(?:likely\\|probab\\|preferab\\|preferred\\|doubtful\\|forced"
          "\\|less natural\\|wahrscheinlich\\|vorzuziehen\\|zweifelhaft\\|gezwungen\\)[^()]*)")
  "A judgement in brackets after a reading — „(highly unlikely given
context)“ told Fable the analysis' choice (TĀ 1012 ⟦695⟧); cut off.")

(defconst bcat-second-opinion--gloss
  (concat "\\(?:‘[^’\n]*’\\|„[^“\n]*“\\|“[^”\n]*”\\|\"[^\"\n]*\""
          "\\|\\(?:\\`\\|[ \t]\\)'[^'\n]*'\\)")
  "A gloss of a reading in quotation marks — an ASCII apostrophe only
after a blank, since one inside a word of the original is an avagraha
\(/vyādhayo'pi/).")

;; ----------------------------------------------------------------------------
;; Reading the file
;; ----------------------------------------------------------------------------

(defun bcat-second-opinion--compared-bounds (text)
  "(BEG . END) of the body of ** Translations compared in TEXT, or nil."
  (when (string-match "^\\*\\* Translations compared[ \t]*\n" text)
    (let ((beg (match-end 0)))
      (cons beg (if (string-match "^\\*\\{1,2\\} " text beg) (match-beginning 0) (length text))))))

(defun bcat-second-opinion--block-bounds (text heading beg end)
  "(BEG . END) of the *** block whose heading matches HEADING between
BEG and END of TEXT — up to the next heading of three stars or fewer —,
or nil."
  (let ((case-fold-search nil))
    (when (and (string-match (concat "^\\*\\*\\* " heading "\\b[^\n]*\n") text beg)
               (< (match-beginning 0) end))
      (let ((b (match-beginning 0)) (body (match-end 0)))
        (cons b (if (and (string-match "^\\*\\{1,3\\} " text body) (< (match-beginning 0) end))
                    (match-beginning 0)
                  end))))))

(defun bcat-second-opinion-ambiguities-in (text)
  "The lines of the ambiguities the analysis in TEXT names (its
*** Ambiguities / Mehrdeutigkeiten under Translations compared), or nil."
  (let* ((compared (bcat-second-opinion--compared-bounds text))
         (block (and compared (bcat-second-opinion--block-bounds
                               text bcat-second-opinion--ambiguity-headings
                               (car compared) (cdr compared)))))
    (when block
      (cl-remove-if-not (lambda (l) (string-prefix-p "- " l))
                        (split-string (substring text (car block) (cdr block)) "\n")))))

(defun bcat-second-opinion--form (text)
  "The first form of the original in TEXT (/…/), or nil."
  (and (string-match "/[^/\n]+/" text) (match-string 0 text)))

(defun bcat-second-opinion--chosen-index (chosen readings)
  "The number of the reading in READINGS ((RAW . NEUTRAL) …) that CHOSEN
\(what follows „chosen:“) names: the longest reading it begins with, else
the one reading with its form of the original; nil when none or several."
  (let ((hits (cl-loop for (raw . neutral) in readings for i from 1
                       for len = (cond ((string-prefix-p raw chosen) (length raw))
                                       ((string-prefix-p neutral chosen) (length neutral)))
                       when len collect (cons i len))))
    (if hits
        (car (car (sort hits (lambda (a b) (> (cdr a) (cdr b))))))
      (let* ((form (bcat-second-opinion--form chosen))
             (same (and form (cl-loop for (raw . _) in readings for i from 1
                                      when (equal form (bcat-second-opinion--form raw))
                                      collect i))))
        (and same (null (cdr same)) (car same))))))

(defun bcat-second-opinion--bare (reading)
  "READING without its glosses in quotation marks
\(`bcat-second-opinion--gloss'): its form and its construction."
  (string-trim
   (replace-regexp-in-string
    "[ \t]+" " "
    (replace-regexp-in-string
     "([ \t]*)" ""
     (replace-regexp-in-string bcat-second-opinion--gloss " " reading)))
   "[ \t]+" "[ \t:;,—–-]+"))

(defun bcat-second-opinion--shown (readings)
  "What the second opinion is shown of READINGS (neutral, without a
judgement) — Carsten 27.11.: „Nur die Formen schicken“, since the
analysis' glosses frame the reading it did not choose (TĀ 1012 ⟦695⟧:
/kāri-amṛta/ „the deathless made/caused“): the forms of the original
when every reading begins with one and they differ; else the readings
without their glosses (the attachment of a word, the kind of a
compound) when these differ; else the readings whole."
  (let ((forms (mapcar (lambda (r) (and (string-match "\\`/[^/\n]+/" r) (match-string 0 r)))
                       readings))
        (bare (mapcar #'bcat-second-opinion--bare readings))
        (distinct (lambda (xs) (and (not (memq nil xs)) (not (member "" xs))
                                    (= (length xs) (length (delete-dups (copy-sequence xs))))))))
    (cond ((funcall distinct forms) forms)
          ((funcall distinct bare) bare)
          (t readings))))

(defun bcat-second-opinion--place (line)
  "The ambiguity LINE as the second opinion is given it: (:passage P
:segment N :readings (R …) :shown (S …) :chosen I) — R the readings
without a judgement in brackets (`bcat-second-opinion--judgement'), S
what Fable is shown of them (`bcat-second-opinion--shown'), both sorted
by S (the analysis names its choice first); the analysis' choice only
as the number I of its reading among them (nil when it cannot be
told); nil for a line without two readings."
  (let ((case-fold-search nil))
    (when (string-match "\\`- \\(.+?\\)\\(?: (⟦\\([0-9]+\\)⟧)\\)?: \\(.+\\)\\'" line)
      (let* ((passage (match-string 1 line))
             (seg (and (match-string 2 line) (string-to-number (match-string 2 line))))
             (rest (match-string 3 line))
             (cut (string-match " — \\(?:chosen\\|gewählt\\): " rest))
             (chosen (and cut (substring rest (match-end 0))))
             (raw (cl-remove-if #'string-blank-p
                                (mapcar #'string-trim
                                        (split-string (if cut (substring rest 0 cut) rest) " · "))))
             (neutral (mapcar (lambda (r)
                                (string-trim
                                 (let ((case-fold-search t))
                                   (replace-regexp-in-string bcat-second-opinion--judgement "" r))))
                              raw))
             ;; (RAW NEUTRAL SHOWN), in the order Fable sees them
             (rows (sort (cl-mapcar #'list raw neutral (bcat-second-opinion--shown neutral))
                         (lambda (a b) (string< (nth 2 a) (nth 2 b))))))
        (when (cdr rows)
          (list :passage passage :segment seg
                :readings (mapcar #'cadr rows)
                :shown (mapcar #'caddr rows)
                :chosen (and chosen (bcat-second-opinion--chosen-index
                                     chosen (mapcar (lambda (row) (cons (car row) (cadr row))) rows)))))))))

(defun bcat-second-opinion-places (text)
  "The places the analysis in TEXT names as ambiguous, each as
`bcat-second-opinion--place' reads it."
  (delq nil (mapcar #'bcat-second-opinion--place (bcat-second-opinion-ambiguities-in text))))

(defun bcat-second-opinion--language-code (file)
  "The language code the second opinion of FILE is written in: that of
the comparison (`bcat-analysis-comparison-language'), else the target
language of FILE's source, else English."
  (or (and (boundp 'bcat-analysis-comparison-language) bcat-analysis-comparison-language)
      (ignore-errors
        (plist-get (tibetan-analysis--read-source-metadata
                    (tibetan-sentence--source-file-from-analysis file))
                   :target-lang))
      "en"))

(defun bcat-second-opinion--language (file)
  "The name of the language the second opinion of FILE is written in."
  (require 'bcat-analysis)
  (bcat-analysis--language (bcat-second-opinion--language-code file)))

(defun bcat-second-opinion--material (file text)
  "What the blind request on the sentence FILE (TEXT) is given besides
the places: (:original O :context (LINE …) :references ((NAME . TEXT)
…)).  The original segment by segment from the source (⟦N⟧ text) —
the segment lines of the file carry the analysis' word division —, else
the verse of its * Text; the document's context lines, its place in the
work, the sentences before and after; the published translations under
Translations compared."
  (require 'bcat-survey)
  (let* ((source (ignore-errors (tibetan-sentence--source-file-from-analysis file)))
         (n (and (string-match "^#\\+SOURCE:.*::\\*Sentence \\([0-9]+\\)" text)
                 (string-to-number (match-string 1 text))))
         (sentence (and source n (ignore-errors (bcat-sentence--sentence n source))))
         (segs (plist-get sentence :segs))
         (meta (and source (ignore-errors (tibetan-analysis--read-source-metadata source))))
         (where (and (string-match "^\\*Ort im Ganzen:\\* *\\(.+\\)$" text) (match-string 1 text)))
         (neighbours (and sentence (ignore-errors (bcat-sentence--neighbours-block sentence source))))
         (compared (bcat-second-opinion--compared-bounds text)))
    (list :original (if segs
                        (mapconcat (lambda (s) (format "⟦%s⟧ %s" (car s) (string-trim (cdr s)))) segs "\n")
                      (if (string-match "^#\\+begin_verse[ \t]*\n\\(\\(?:.\\|\n\\)*?\\)\n#\\+end_verse" text)
                          (string-trim (match-string 1 text))
                        ""))
          :context (delq nil (append (plist-get meta :claude-context)
                                     (list (and where (concat "Place in the work: " where))
                                           neighbours)))
          :references (and compared (bcat-survey--references
                                     (substring text (car compared) (cdr compared)))))))

;; ----------------------------------------------------------------------------
;; The requests
;; ----------------------------------------------------------------------------

(defconst bcat-second-opinion--schema
  "{\"opinions\": [{\"place\": 1, \"choice\": 1, \"reading\": \"…\", \"sense\": \"…\", \"reason\": \"…\"}]}"
  "The JSON contract of the blind second opinion.")

(defconst bcat-second-opinion--review-schema
  "{\"points\": [{\"passage\": \"…\", \"segment\": 0, \"point\": \"…\"}]}"
  "The JSON contract of the review.")

(defun bcat-second-opinion-prompts (file places)
  "(SYSTEM . USER) of the blind second opinion on PLACES (from
`bcat-second-opinion-places') of the sentence FILE: the original, its
context and the published translations (`bcat-second-opinion--material'),
the places with their readings numbered — nothing of the analysis."
  (require 'bcat-sentence)
  (let* ((text (bcat-sentence--text file))
         (m (bcat-second-opinion--material file text))
         (refs (plist-get m :references)))
    (cons
     (format "You give a second opinion on places in one sentence of a Sanskrit or Tibetan text, read in a reading class at a university. At each place the original admits more than one grammatical analysis; the readings are listed in no particular order, each by its form alone — by its construction where the readings share their form —, so say yourself what the one you choose means. Decide each place from the original alone — grammar, sandhi, lexicon, the context in the work and its commentary; the published translations show how others read the sentence, they do not decide, and a translator's standing is no argument.
- opinions: one entry per place, in the order given. place: its number. choice: the number of the reading the original supports; 0 when it supports a reading that is not listed — give it in reading, the form of the original first — or when the place stays open (reading \"\"). sense: what the chosen reading says here, in a few words (\"\" when the place stays open). reason: one to three sentences; name a reading by its form, never by its number (your answer is shown without the numbers).
- Words of the original in /italic/; sense and reason in %s.
Answer with the JSON object alone: %s" (bcat-second-opinion--language file) bcat-second-opinion--schema)
     (concat (format "Sentence file: %s\n\n" (file-name-nondirectory file))
             (if (plist-get m :context)
                 (concat "<context>\n" (string-join (plist-get m :context) "\n") "\n</context>\n\n")
               "")
             "<original>\n" (plist-get m :original) "\n</original>\n\n"
             (if refs
                 (concat "<translations>\n"
                         (mapconcat (lambda (r) (format "%s: %s" (car r) (cdr r))) refs "\n\n")
                         "\n</translations>\n\n")
               "")
             "<places>\n"
             (string-join
              (cl-loop for p in places for i from 1
                       collect (concat (format "Place %d: %s" i (plist-get p :passage))
                                       (if (plist-get p :segment) (format " (⟦%d⟧)" (plist-get p :segment)) "")
                                       (cl-loop for r in (plist-get p :shown) for k from 1
                                                concat (format "\n  %d) %s" k r))))
              "\n\n")
             "\n</places>"))))

(defun bcat-second-opinion--without-answers (text)
  "TEXT without the second opinion and the review under Translations
compared — a review does not read the second model's earlier answers."
  (dolist (heading (list bcat-second-opinion--headings bcat-second-opinion--review-headings) text)
    (let* ((compared (bcat-second-opinion--compared-bounds text))
           (block (and compared (bcat-second-opinion--block-bounds
                                 text heading (car compared) (cdr compared)))))
      (when block
        (setq text (concat (substring text 0 (car block)) (substring text (cdr block))))))))

(defun bcat-second-opinion-review-prompts (file)
  "(SYSTEM . USER) of the review of the analysis of the sentence FILE
\(C-c u Z): its generated part as plain lines, without the second
model's earlier answers and without the user's notes."
  (require 'bcat-sentence)
  (cons
   (format "You review the philological analysis of one sentence of a Sanskrit or Tibetan text, written by another model for a reading class at a university; you see its generated part as plain lines.
- points: at most five places where you would decide differently from the analysis — a form, a sandhi, a construction, a rendering, a label in its tables — only where it matters for the meaning or would mislead a student; [] when there is none. Argue from the original alone: grammar, sandhi, lexicon, the context in the work and its commentary — never from a translator's standing.
- passage: only the word or compound the point concerns, in /italic/ as the text has it — never a whole segment or line; segment: its number.
- Words of the original in /italic/; point in %s.
Answer with the JSON object alone: %s" (bcat-second-opinion--language file) bcat-second-opinion--review-schema)
   (concat (format "Sentence file: %s\n\n<analysis>\n" (file-name-nondirectory file))
           (string-join (bcat-sentence--plain-lines
                         (bcat-second-opinion--without-answers (bcat-sentence--text file)))
                        "\n")
           "\n</analysis>")))

;; ----------------------------------------------------------------------------
;; The answers in the file
;; ----------------------------------------------------------------------------

(defun bcat-second-opinion--model-name (model)
  "MODEL as the heading names it: claude-fable-5-2 → „Fable 5.2“."
  (let ((parts (split-string (string-remove-prefix "claude-" (format "%s" model)) "-")))
    (concat (capitalize (car parts))
            (if (cdr parts) (concat " " (string-join (cdr parts) ".")) ""))))

(defun bcat-second-opinion--words (code)
  "A function from a key of `bcat-second-opinion--labels' to its word
in the language CODE (English when CODE has none)."
  (let ((words (cdr (or (assoc code bcat-second-opinion--labels)
                        (assoc "en" bcat-second-opinion--labels)))))
    (lambda (k) (alist-get k words))))

(defun bcat-second-opinion--field (alist key)
  "The field KEY of ALIST (an object of an answer) as a trimmed string."
  (string-trim (format "%s" (or (alist-get key alist) ""))))

(defun bcat-second-opinion--sentence (s)
  "S ending with a full stop (S itself when empty or already ended)."
  (if (or (string-empty-p s) (string-match-p "[.!?]\\'" s)) s (concat s ".")))

(defun bcat-second-opinion--block (parsed model code places)
  "The *** Second opinion block of PARSED (the blind answer) from MODEL
on PLACES, in the language CODE: per place the reading chosen as Fable
saw it, with what Fable says it means — never the analysis' gloss —,
and whether the analysis chose it too (open, when no reading is
chosen; no verdict when the analysis' choice cannot be told)."
  (let* ((word (bcat-second-opinion--words code))
         (lines
          (delq nil
                (mapcar
                 (lambda (o)
                   (let* ((n (alist-get 'place o))
                          (n (if (stringp n) (string-to-number n) n))
                          (place (and (integerp n) (> n 0) (nth (1- n) places))))
                     (when place
                       (let* ((readings (or (plist-get place :shown) (plist-get place :readings)))
                              (choice (alist-get 'choice o))
                              (choice (if (stringp choice) (string-to-number choice) choice))
                              (k (and (integerp choice) (<= 1 choice (length readings)) choice))
                              (own (bcat-second-opinion--field o 'reading))
                              (reading (cond (k (nth (1- k) readings))
                                             ((not (string-empty-p own)) own)))
                              (chosen (plist-get place :chosen))
                              (verdict (cond ((null reading) 'open)
                                             ((and k chosen) (if (= k chosen) 'same 'differs))
                                             (k nil)
                                             (t 'differs)))
                              (sense (string-trim (bcat-second-opinion--field o 'sense) "[ \t‘’'\"„“”]+" "[ \t‘’'\"„“”]+"))
                              (reason (bcat-second-opinion--field o 'reason)))
                         (concat "- " (plist-get place :passage)
                                 (if (plist-get place :segment) (format " (⟦%d⟧)" (plist-get place :segment)) "")
                                 ": "
                                 (if (eq verdict 'open)
                                     (funcall word 'open)
                                   (concat reading
                                           (if (string-empty-p sense) "" (format " ‘%s’" sense))
                                           (if verdict (concat " — " (funcall word verdict)) "")))
                                 (if (string-empty-p reason) ""
                                   (concat ". " (bcat-second-opinion--sentence reason))))))))
                 (append (alist-get 'opinions parsed) nil)))))
    (concat (format "*** %s (%s, %s)\n" (funcall word 'heading)
                    (bcat-second-opinion--model-name model) (funcall word 'blind))
            (if lines (string-join lines "\n") (funcall word 'silent))
            "\n")))

(defun bcat-second-opinion--review-block (parsed model code)
  "The *** Review block of PARSED (the review) from MODEL, in the
language CODE."
  (let* ((word (bcat-second-opinion--words code))
         (points (append (alist-get 'points parsed) nil)))
    (concat (format "*** %s (%s)\n" (funcall word 'review) (bcat-second-opinion--model-name model))
            (if points
                (mapconcat (lambda (p)
                             (let ((seg (alist-get 'segment p)))
                               (concat "- " (bcat-second-opinion--field p 'passage)
                                       (if (and (integerp seg) (> seg 0)) (format " (⟦%d⟧)" seg) "")
                                       ": " (bcat-second-opinion--sentence
                                             (bcat-second-opinion--field p 'point)))))
                           points "\n")
              (funcall word 'nothing))
            "\n")))

(defun bcat-second-opinion--insert-block (text block heading afters)
  "TEXT with BLOCK, whose heading matches HEADING, under Translations
compared: after the block of the first of AFTERS (heading regexps) that
is there, else before the machine translation's *** heading, else at
the end of the section; an earlier block of HEADING gives way.  TEXT
unchanged when it has no Translations compared."
  (let ((compared (bcat-second-opinion--compared-bounds text)))
    (if (not compared)
        text
      (let* ((old (bcat-second-opinion--block-bounds text heading (car compared) (cdr compared)))
             (text (if old (concat (substring text 0 (car old)) (substring text (cdr old))) text))
             (compared (bcat-second-opinion--compared-bounds text))
             (after (cl-some (lambda (h) (bcat-second-opinion--block-bounds
                                          text h (car compared) (cdr compared)))
                             afters))
             (machine (let ((case-fold-search nil))
                        (and (string-match "^\\*\\*\\* [^\n]*\\(?:machine translation\\|maschinelle Übersetzung\\)"
                                           text (car compared))
                             (< (match-beginning 0) (cdr compared))
                             (match-beginning 0))))
             (at (cond (after (cdr after)) (machine machine) (t (cdr compared)))))
        (concat (string-trim-right (substring text 0 at)) "\n\n" block "\n"
                (string-trim-left (substring text at) "\n+"))))))

(defun bcat-second-opinion-insert (text block)
  "TEXT with BLOCK as the second opinion: after *** Ambiguities
\(`bcat-second-opinion--insert-block')."
  (bcat-second-opinion--insert-block text block bcat-second-opinion--headings
                                     (list bcat-second-opinion--ambiguity-headings)))

(defun bcat-second-opinion--insert-review (text block)
  "TEXT with BLOCK as the review: after the second opinion, else after
*** Ambiguities (`bcat-second-opinion--insert-block')."
  (bcat-second-opinion--insert-block text block bcat-second-opinion--review-headings
                                     (list bcat-second-opinion--headings
                                           bcat-second-opinion--ambiguity-headings)))

(defun bcat-second-opinion--write (file response make)
  "Write into the sentence FILE the text MAKE returns for its text and
the object of RESPONSE; return it."
  (require 'bcat-analysis)
  (require 'bcat-sentence)
  (let ((parsed (bcat-analysis-read-object response)))
    (unless (consp parsed) (error "Antwort ohne JSON-Objekt"))
    (bcat-sentence--save-visit file)
    (let* ((text (bcat-sentence--text file))
           (new (funcall make text parsed (bcat-second-opinion--language-code file))))
      (unless (equal new text) (bcat-sentence--write file new))
      new)))

(defun bcat-second-opinion--land (file response model places)
  "Write the blind second opinion RESPONSE from MODEL on PLACES into the
sentence FILE."
  (bcat-second-opinion--write
   file response
   (lambda (text parsed code)
     (bcat-second-opinion-insert text (bcat-second-opinion--block parsed model code places)))))

(defun bcat-second-opinion--land-review (file response model)
  "Write the review RESPONSE from MODEL into the sentence FILE."
  (bcat-second-opinion--write
   file response
   (lambda (text parsed code)
     (bcat-second-opinion--insert-review text (bcat-second-opinion--review-block parsed model code)))))

(defun bcat-second-opinion-pending-p (file)
  "Non-nil while a second opinion on FILE is on its way."
  (and file (gethash (file-truename file) bcat-second-opinion--pending)))

(defun bcat-second-opinion--done (file how)
  "The second opinion on FILE has ended (landed or failed); after a
fetch (HOW `auto') the export that waited follows."
  (remhash (file-truename file) bcat-second-opinion--pending)
  (when (and (eq how 'auto) (fboundp 'bcat-export-after-fetch))
    (bcat-export-after-fetch file)))

(defun bcat-second-opinion--status (file text)
  "TEXT as FILE's step of the second opinion in the status buffer."
  (when (fboundp 'bcat-status-step)
    (ignore-errors (bcat-status-step file :second text))))

(defun bcat-second-opinion--ask (file how kind what prompts land)
  "Queue the request KIND (WHAT, its name in the messages) on the
sentence FILE to `bcat-second-opinion-model' (HOW: `auto' after a
fetch, `manual' by hand): PROMPTS makes (SYSTEM . USER), LAND writes
the answer (FILE RESPONSE MODEL).  Non-nil when queued."
  (let ((model bcat-second-opinion-model)
        (name (bcat-second-opinion--model-name bcat-second-opinion-model)))
    (puthash (file-truename file) how bcat-second-opinion--pending)
    (bcat-second-opinion--status file (format "%s gefragt …" name))
    (or (let ((bcat-model model))
          (tibetan-discussion--submit
           kind file prompts
           (lambda (f response)
             (unwind-protect
                 (condition-case err
                     (progn (funcall land f response model)
                            (bcat-second-opinion--status f (format "%s: %s ✓" name what))
                            (message "%s: %s von %s eingetragen" (file-name-nondirectory f) what name))
                   (error (bcat-second-opinion--status f (format "%s: Fehler — %s" name (error-message-string err)))
                          (message "%s (%s): %s" what (file-name-nondirectory f) (error-message-string err))))
               (bcat-second-opinion--done f how)))
           (lambda (f reason)
             (bcat-second-opinion--status f (format "%s: keine Antwort — %s" name reason))
             (bcat-second-opinion--done f how))))
        (progn (remhash (file-truename file) bcat-second-opinion--pending) nil))))

(defun bcat-second-opinion-request (file how)
  "Ask `bcat-second-opinion-model' blind on the places the analysis of
the sentence FILE names (HOW: `auto' after a fetch, `manual' by C-u
C-c u Z).  Returns non-nil when the request was queued."
  (require 'bcat-sentence)
  (let ((places (bcat-second-opinion-places (bcat-sentence--text file))))
    (bcat-second-opinion--ask
     file how "second-opinion" "zweite Meinung"
     (lambda () (bcat-second-opinion-prompts file places))
     (lambda (f response model) (bcat-second-opinion--land f response model places)))))

(defun bcat-second-opinion-review-request (file)
  "Ask `bcat-second-opinion-model' for a review of the analysis of the
sentence FILE (C-c u Z).  Returns non-nil when the request was queued."
  (bcat-second-opinion--ask
   file 'manual "review" "Durchsicht"
   (lambda () (bcat-second-opinion-review-prompts file))
   #'bcat-second-opinion--land-review))

(defun bcat-second-opinion-after-fetch (file)
  "Hook for `bcat-sentence-after-fetch-functions': when the analysis
just written into FILE names ambiguities, ask `bcat-second-opinion-model'
blind (`bcat-second-opinion-auto') — never when that model is the CAT's
own, never for a fixture in the temp folder.  The export of FILE waits
for the answer.  Returns non-nil when asked; never signals."
  (condition-case err
      (progn
        (require 'tibetan-discussion-paper)
        (require 'bcat-sentence)
        (when (and bcat-second-opinion-auto
                   (bcat-sentence-file-p file)
                   (not (tibetan-discussion-paper--temp-source-p file))
                   (not (eq (bcat-model) (intern bcat-second-opinion-model)))
                   (bcat-second-opinion-places (bcat-sentence--text file)))
          (bcat-second-opinion-request file 'auto)))
    (error (message "Zweite Meinung (%s): %s" (file-name-nondirectory file) (error-message-string err))
           nil)))

(add-hook 'bcat-sentence-after-fetch-functions #'bcat-second-opinion-after-fetch -50)

;;;###autoload
(defun bcat-second-opinion (&optional blind)
  "C-c u Z („Fable fragen“): a review of the analysis of the sentence
file at point by `bcat-second-opinion-model' — *** Review under
Translations compared.  With a prefix argument (BLIND), the blind
second opinion on the places the analysis names as ambiguous, again.
The PDF stays as it is (C-c u H)."
  (interactive "P")
  (require 'bcat-sentence)
  (let ((file (buffer-file-name))
        (name (bcat-second-opinion--model-name bcat-second-opinion-model)))
    (unless (and file (bcat-sentence-file-p file))
      (user-error "Nur in einer Satzdatei"))
    (when (buffer-modified-p) (save-buffer))
    (when (and blind (not (bcat-second-opinion-places (bcat-sentence--text file))))
      (user-error "Die Analyse nennt keine Mehrdeutigkeiten — ohne C-u: Durchsicht"))
    (if (and (not (bcat-second-opinion-pending-p file))
             (if blind
                 (bcat-second-opinion-request file 'manual)
               (bcat-second-opinion-review-request file)))
        (message "%s: %s gefragt (%s)" (file-name-nondirectory file) name
                 (if blind "Mehrdeutigkeiten, blind" "Durchsicht der Analyse"))
      (message "%s: %s ist schon gefragt" (file-name-nondirectory file) name))))

(provide 'bcat-second-opinion)
;;; bcat-second-opinion.el ends here
