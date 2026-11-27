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
;; short request goes to `bcat-second-opinion-model' (Fable): the
;; analysis as plain lines and the named places; its answer stands under
;; Translations compared as *** Second opinion (Fable 5.2) — nothing of
;; the analysis is overwritten.  The PDF of the file waits for it
;; (`bcat-export-after-fetch' asks `bcat-second-opinion-pending-p').
;; C-c u Z („Fable fragen“) asks for a file at point by hand — a review
;; of the analysis when it names no place.

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
  '(("de" (heading . "Zweite Meinung") (agree . "stimmt zu") (other . "andere Lesung")
     (open . "offen") (also . "Außerdem") (none . "stimmt der Analyse zu"))
    ("en" (heading . "Second opinion") (agree . "agrees") (other . "another reading")
     (open . "open") (also . "Also") (none . "agrees with the analysis")))
  "The words of the second opinion in the file.")

(defconst bcat-second-opinion--headings
  "\\(?:Second opinion\\|Zweite Meinung\\)"
  "The heading of a second opinion in either language.")

(defconst bcat-second-opinion--ambiguity-headings
  "\\(?:Ambiguities\\|Mehrdeutigkeiten\\)"
  "The heading of the ambiguities in either language.")

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

;; ----------------------------------------------------------------------------
;; The request
;; ----------------------------------------------------------------------------

(defconst bcat-second-opinion--schema
  "{\"opinions\": [{\"passage\": \"…\", \"segment\": 0, \"verdict\": \"agree|other|open\", \"reading\": \"…\", \"reason\": \"…\"}], \"further\": [{\"passage\": \"…\", \"segment\": 0, \"point\": \"…\"}]}"
  "The JSON contract of the second opinion.")

(defun bcat-second-opinion-prompts (file)
  "(SYSTEM . USER) of the second opinion on the sentence FILE: the
analysis as plain lines (its generated part, no notes of the user) and
the places it names as ambiguous; without such places a review."
  (require 'bcat-sentence)
  (let* ((text (bcat-sentence--text file))
         (places (bcat-second-opinion-ambiguities-in text))
         (lang (bcat-second-opinion--language file)))
    (cons
     (format "You give a second opinion on the philological analysis of one sentence of a Sanskrit or Tibetan text, written by another model for a reading class at a university. You see the analysis (its generated part, as plain lines) and the places where the original admits more than one grammatical analysis, as the analysis named them.
- opinions: one entry per named place, in the order given. Decide which reading the original supports, argued from the original alone: grammar, sandhi, lexicon, the context in the work and its commentary — never from a translator's standing. verdict: \"agree\" (the chosen reading is right), \"other\" (another reading is right — give it in reading), \"open\" (both remain defensible). reading: the reading you prefer, the form of the original first; \"\" with agree. reason: one to three sentences. passage and segment as the analysis gives them.
- further: at most three other places where you would decide differently from the analysis — a form, a sandhi, a construction, a rendering — only where it matters for the meaning; [] when there is none. When the analysis named no ambiguous place, opinions is [] and further holds your review.
- Words of the original in /italic/. reason and point in %s.
Answer with the JSON object alone: %s" lang bcat-second-opinion--schema)
     (concat (format "Sentence file: %s\n\n<analysis>\n" (file-name-nondirectory file))
             (string-join (bcat-sentence--plain-lines text) "\n")
             "\n</analysis>\n\n"
             (if places
                 (concat "<ambiguities>\n" (string-join places "\n") "\n</ambiguities>")
               "The analysis named no such place — review it: give in further where you would decide differently.")))))

(defun bcat-second-opinion--model-name (model)
  "MODEL as the heading names it: claude-fable-5-2 → „Fable 5.2“."
  (let ((parts (split-string (string-remove-prefix "claude-" (format "%s" model)) "-")))
    (concat (capitalize (car parts))
            (if (cdr parts) (concat " " (string-join (cdr parts) ".")) ""))))

(defun bcat-second-opinion--block (parsed model code)
  "The *** Second opinion block of PARSED (Fable's answer) from MODEL,
in the language CODE."
  (let* ((words (cdr (or (assoc code bcat-second-opinion--labels)
                         (assoc "en" bcat-second-opinion--labels))))
         (word (lambda (k) (alist-get k words)))
         (field (lambda (a k) (string-trim (format "%s" (or (alist-get k a) "")))))
         (where (lambda (a)
                  (let ((seg (alist-get 'segment a)))
                    (concat (funcall field a 'passage)
                            (if (and (integerp seg) (> seg 0)) (format " (⟦%d⟧)" seg) "")))))
         (sentence (lambda (s) (if (or (string-empty-p s) (string-match-p "[.!?]\\'" s)) s (concat s "."))))
         (opinions (append (alist-get 'opinions parsed) nil))
         (further (append (alist-get 'further parsed) nil)))
    (concat
     (format "*** %s (%s)\n" (funcall word 'heading) (bcat-second-opinion--model-name model))
     (if (or opinions further)
         (concat
          (mapconcat
           (lambda (o)
             (let* ((verdict (intern (downcase (funcall field o 'verdict))))
                    (reading (funcall field o 'reading))
                    (reason (funcall field o 'reason)))
               (concat "- " (funcall where o) ": "
                       (or (funcall word (if (memq verdict '(agree other open)) verdict 'open)) "")
                       (if (and (eq verdict 'other) (not (string-empty-p reading))) (concat " — " reading) "")
                       (if (string-empty-p reason) "" (concat ". " (funcall sentence reason))))))
           opinions "\n")
          (and opinions further "\n")
          (and further
               (concat (format "*%s:*\n" (funcall word 'also))
                       (mapconcat (lambda (f) (concat "- " (funcall where f) ": "
                                                      (funcall sentence (funcall field f 'point))))
                                  further "\n"))))
       (funcall word 'none))
     "\n")))

(defun bcat-second-opinion-insert (text block)
  "TEXT with BLOCK as the second opinion under Translations compared:
after *** Ambiguities, else before the machine translation's ***
heading, else at the end of the section; an earlier second opinion
gives way.  TEXT unchanged when it has no Translations compared."
  (let ((compared (bcat-second-opinion--compared-bounds text)))
    (if (not compared)
        text
      (let* ((old (bcat-second-opinion--block-bounds text bcat-second-opinion--headings
                                                     (car compared) (cdr compared)))
             (text (if old (concat (substring text 0 (car old)) (substring text (cdr old))) text))
             (compared (bcat-second-opinion--compared-bounds text))
             (amb (bcat-second-opinion--block-bounds text bcat-second-opinion--ambiguity-headings
                                                     (car compared) (cdr compared)))
             (machine (let ((case-fold-search nil))
                        (and (string-match "^\\*\\*\\* [^\n]*\\(?:machine translation\\|maschinelle Übersetzung\\)"
                                           text (car compared))
                             (< (match-beginning 0) (cdr compared))
                             (match-beginning 0))))
             (at (cond (amb (cdr amb)) (machine machine) (t (cdr compared))))
             (before (string-trim-right (substring text 0 at)))
             (after (substring text at)))
        (concat before "\n\n" block "\n" (string-trim-left after "\n+"))))))

(defun bcat-second-opinion--land (file response model)
  "Write the second opinion RESPONSE from MODEL into the sentence FILE."
  (require 'bcat-analysis)
  (require 'bcat-sentence)
  (let ((parsed (bcat-analysis-read-object response)))
    (unless (consp parsed) (error "Antwort ohne JSON-Objekt"))
    (bcat-sentence--save-visit file)
    (let* ((text (bcat-sentence--text file))
           (new (bcat-second-opinion-insert
                 text (bcat-second-opinion--block parsed model (bcat-second-opinion--language-code file)))))
      (unless (equal new text) (bcat-sentence--write file new))
      new)))

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

(defun bcat-second-opinion-request (file how)
  "Ask `bcat-second-opinion-model' for its opinion on the sentence FILE
\(HOW: `auto' after a fetch, `manual' by C-c u Z).  Returns non-nil when
the request was queued."
  (let ((model bcat-second-opinion-model)
        (name (bcat-second-opinion--model-name bcat-second-opinion-model)))
    (puthash (file-truename file) how bcat-second-opinion--pending)
    (bcat-second-opinion--status file (format "%s gefragt …" name))
    (or (let ((bcat-model model))
          (tibetan-discussion--submit
           "second-opinion" file
           (lambda () (bcat-second-opinion-prompts file))
           (lambda (f response)
             (unwind-protect
                 (condition-case err
                     (progn (bcat-second-opinion--land f response model)
                            (bcat-second-opinion--status f (format "%s: zweite Meinung ✓" name))
                            (message "%s: zweite Meinung von %s eingetragen" (file-name-nondirectory f) name))
                   (error (bcat-second-opinion--status f (format "%s: Fehler — %s" name (error-message-string err)))
                          (message "Zweite Meinung (%s): %s" (file-name-nondirectory f) (error-message-string err))))
               (bcat-second-opinion--done f how)))
           (lambda (f reason)
             (bcat-second-opinion--status f (format "%s: keine Antwort — %s" name reason))
             (bcat-second-opinion--done f how))))
        (progn (remhash (file-truename file) bcat-second-opinion--pending) nil))))

(defun bcat-second-opinion-after-fetch (file)
  "Hook for `bcat-sentence-after-fetch-functions': when the analysis
just written into FILE names ambiguities, ask `bcat-second-opinion-model'
\(`bcat-second-opinion-auto') — never when that model is the CAT's own,
never for a fixture in the temp folder.  The export of FILE waits for
the answer.  Returns non-nil when asked; never signals."
  (condition-case err
      (progn
        (require 'tibetan-discussion-paper)
        (require 'bcat-sentence)
        (when (and bcat-second-opinion-auto
                   (bcat-sentence-file-p file)
                   (not (tibetan-discussion-paper--temp-source-p file))
                   (not (eq (bcat-model) (intern bcat-second-opinion-model)))
                   (bcat-second-opinion-ambiguities-in (bcat-sentence--text file)))
          (bcat-second-opinion-request file 'auto)))
    (error (message "Zweite Meinung (%s): %s" (file-name-nondirectory file) (error-message-string err))
           nil)))

(add-hook 'bcat-sentence-after-fetch-functions #'bcat-second-opinion-after-fetch -50)

;;;###autoload
(defun bcat-second-opinion ()
  "C-c u Z („Fable fragen“): the second opinion of
`bcat-second-opinion-model' on the sentence file at point — on the
places its analysis names as ambiguous, or a review of the analysis
when it names none.  Written under Translations compared; the PDF
stays as it is (C-c u H)."
  (interactive)
  (require 'bcat-sentence)
  (let ((file (buffer-file-name)))
    (unless (and file (bcat-sentence-file-p file))
      (user-error "Nur in einer Satzdatei"))
    (when (buffer-modified-p) (save-buffer))
    (if (bcat-second-opinion-request file 'manual)
        (message "%s: %s gefragt (%s)" (file-name-nondirectory file)
                 (bcat-second-opinion--model-name bcat-second-opinion-model)
                 (if (bcat-second-opinion-ambiguities-in (bcat-sentence--text file))
                     "zu den Mehrdeutigkeiten" "Durchsicht der Analyse"))
      (message "%s: eine zweite Meinung ist schon unterwegs" (file-name-nondirectory file)))))

(provide 'bcat-second-opinion)
;;; bcat-second-opinion.el ends here
