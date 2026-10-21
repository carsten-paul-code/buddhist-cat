;;; tibetan-word-formation.el --- Word formation (prefix + root) for Sanskrit gloss tables -*- lexical-binding: t -*-

;;; Commentary:
;; §5.66 (2026-10-21).  Carsten: „Mein Sanskrit ist noch sehr, sehr
;; basic.  Ich möchte die Reading-Klassen auch dafür nutzen, das zu
;; ändern.  Für die Zettel haben wir eine Funktion, die Präfix und
;; Wurzel zerlegt.  Das hätte ich gerne in der Tabelle als zusätzliche
;; Zeile (als 2.)."
;;
;; The zettels' Wortbildung (:ETYMOLOGY:, SKOS-DESIGN.org) is written
;; by Claude when a term zettel is made — there is no rule engine.
;; The same here: a small Claude call per Sanskrit cascade sentence
;; gets the words of the `** Word Analysis' (form, lemma, label) and
;; answers, per segment, `- WORD — FORMATION' in a compact notation
;;
;;   vi-√muc-ta · sam-ā-√viś · √iṣ-ana · √vid · √kṛ-yā · sva-tantra-ya · —
;;
;; (prefixes hyphened before the root, √ marks the root, suffixes
;; hyphened after it, compound members joined by ` · ', no glosses,
;; no inflectional endings — those are rows 3 and 4).  When a lemma
;; has a zettel with a Wortbildung, Claude gets it as the CURATED
;; formation, so table and zettel agree.
;;
;; Landed as `** Word Formation' in * Sanskrit Analysis (after the
;; Word Analysis; the cascade regenerate keeps it and binds it for the
;; gloss tables, which then show it as row 2).  Fired after a Claude
;; landing (hook, switch `tibetan-word-formation-auto') and by C-c u G
;; for sentences analysed earlier; `tibetan-word-formation-dwim'
;; forces a new one.  The request machinery is the discussion's
;; (`tibetan-discussion--submit': queue, cache, landing outside the
;; gptel sentinel).

;;; Code:

(require 'cl-lib)
(require 'tibetan-cascade)
(require 'tibetan-sanskrit-reading)
(require 'tibetan-translation-discussion)  ; --submit, --inflight-p
(require 'tibetan-zettel-highlight)        ; the zettel index (curated formations)

(declare-function tibetan-sentence--filepath "tibetan-sentence-persist")
(declare-function tibetan-sanskrit-reading-parse-word-formation "tibetan-sanskrit-reading" (body))
(declare-function tibetan-sentence--read-l2-body "tibetan-sentence-persist")
(declare-function tibetan-sentence--source-file-from-analysis "tibetan-sentence-persist")
(declare-function tibetan-sentence--sent-id-from-filename "tibetan-sentence-persist")
(declare-function tibetan-analysis--resolve-source-lang "tibetan-analysis-claude")
(declare-function tibetan-analysis--defer-mt-p "tibetan-analysis-claude")
(declare-function tibetan-analysis--read-source-metadata "tibetan-analysis-claude")

(defcustom tibetan-word-formation-auto t
  "When non-nil, the word formation of a Sanskrit cascade sentence is
requested automatically after its Claude analysis has landed (one
small extra Claude call per sentence).  C-c u G asks for missing ones
in any case; `tibetan-word-formation-dwim' forces a new one."
  :type 'boolean
  :group 'tibetan-cat)

(defconst tibetan-word-formation--heading "Word Formation"
  "The `** ' heading in * Sanskrit Analysis.")

;; ============================================================================
;; Inputs
;; ============================================================================

(defun tibetan-word-formation--curated (lemma)
  "The zettels' Wortbildung (:ETYMOLOGY:) for LEMMA, or nil — read
through `tibetan-zettel-highlight--etymology', the reader behind the
tooltip and C-c u z w (one source for zettel, panel and CAT)."
  (when (and lemma (tibetan-zettel-highlight--ensure-index))
    (cl-some #'tibetan-zettel-highlight--etymology
             (tibetan-zettel-highlight--ids-for-term
              (tibetan-zettel-highlight--strip-lemma lemma)))))

(defun tibetan-word-formation--inputs (file)
  "(:file :source-file :sa :target :segments) for the cascade sentence FILE —
:segments the parsed `** Word Analysis' ((SEG :pada :morph :lemma) …)
— or nil when FILE is no cascade sentence file."
  (when (and file (file-exists-p file) (tibetan-cascade-file-p file))
    (let* ((src (tibetan-sentence--source-file-from-analysis file))
           (sa (and src (equal (tibetan-analysis--resolve-source-lang src) "sa"))))
      (list :file file :source-file src :sa sa
            :target (or (and src (plist-get (tibetan-analysis--read-source-metadata src)
                                            :target-lang))
                        "en")
            :segments (and sa (tibetan-sanskrit-reading-parse-word-analysis
                               (tibetan-sentence--read-l2-body file "Word Analysis")))))))

(defun tibetan-word-formation--ready-p (inputs)
  "Non-nil when INPUTS are Sanskrit with a Word Analysis that has words."
  (and inputs (plist-get inputs :sa)
       (cl-some (lambda (s) (plist-get (cdr s) :pada)) (plist-get inputs :segments))
       t))

(defun tibetan-word-formation--present-p (file)
  "Non-nil when FILE carries a `** Word Formation' with entries."
  (and (file-exists-p file)
       (tibetan-sanskrit-reading-parse-word-formation
        (tibetan-sentence--read-l2-body file tibetan-word-formation--heading))
       t))

;; ============================================================================
;; Prompt
;; ============================================================================

(defun tibetan-word-formation--system-prompt (target)
  "The system prompt for the TARGET language (\"de\" or other) —
constant per language (cache prefix)."
  (format
   "You decompose Sanskrit words for a reader who is learning Sanskrit.
For each word you get its form in the text (padapāṭha), its lemma and
its grammatical label.  Give its WORD FORMATION and the MEANINGS of
the parts.

FORMATION — compact notation, no glosses:
- verbal prefixes (upasarga) hyphened before the root, the root
  marked with √:  vi-√muc,  sam-ā-√viś,  ud-√i
- a derivational suffix hyphened after the root:  vi-√muc-ta,  √kṛ-ti,
  √jñā-ana,  √iṣ-ana
- secondary suffixes likewise after the stem:  sva-tantra-ya,  śūnya-tā
- compound members joined by \" · \", each decomposed the same way:
  √iṣ-ana · √vid · √kṛ-yā
- a word or member without a transparent verbal root: its stem
  (deva, hṛd, sva)
- inflectional endings are NOT shown (the grammatical label has them)
- pronouns, particles, indeclinables, numerals: —

MEANINGS — every part of the formation, in its order, as PART meaning,
joined by \" · \"; one to three words each: a prefix its basic sense,
a root its basic meaning, a suffix its function, a stem its meaning:
  vi- apart · √muc free · -ta past participle
  √iṣ wish · -ana action noun · √vid know · √kṛ do · -yā action noun
Write the meanings in %s.  For a word with formation —, write —.

When a CURATED word formation is given for a lemma, follow it
(converted to this notation).

Answer exactly in this shape, one line per word, in the given order,
each WORD exactly as given:

### Segment N
- WORD — FORMATION — MEANINGS"
   (if (equal target "de") "German" "English")))

(defun tibetan-word-formation--build-prompts (inputs)
  "(SYSTEM . USER) for INPUTS."
  (let* ((segments (plist-get inputs :segments))
         (lemmas (delete-dups
                  (cl-loop for (_ . e) in segments
                           append (mapcar #'cdr (plist-get e :lemma)))))
         (curated (delq nil (mapcar (lambda (l)
                                      (let ((c (tibetan-word-formation--curated l)))
                                        (and c (cons l c))))
                                    lemmas))))
    (cons
     (tibetan-word-formation--system-prompt (plist-get inputs :target))
     (concat
      "Words of the sentence, per segment (form — lemma; grammatical label):\n\n"
      (mapconcat
       (lambda (seg)
         (let ((e (cdr seg)))
           (concat (format "### Segment %d\n" (car seg))
                   (mapconcat
                    (lambda (w)
                      (let ((lem (cdr (assoc w (plist-get e :lemma))))
                            (lab (cdr (assoc w (plist-get e :morph)))))
                        (format "- %s — %s; %s" w (or lem w) (or lab "?"))))
                    (plist-get e :pada) "\n"))))
       segments "\n\n")
      (if curated
          (concat "\n\nCURATED word formations from the reader's notes:\n"
                  (mapconcat (lambda (c) (format "- %s: %s" (car c) (cdr c)))
                             curated "\n"))
        "")
      "\n\nGive the word formation of every word now."))))

;; ============================================================================
;; Landing
;; ============================================================================

(defun tibetan-word-formation--land (file response)
  "Write the formation of RESPONSE into FILE (`** Word Formation' in
* Sanskrit Analysis) and re-render the gloss tables.  t on success."
  (let ((parsed (tibetan-sanskrit-reading-parse-word-formation response)))
    (when parsed
      (let ((body (mapconcat
                   (lambda (seg)
                     (concat (format "*** Segment %d\n" (car seg))
                             (mapconcat
                              (lambda (p)
                                (let ((f (plist-get (cdr p) :formation))
                                      (m (plist-get (cdr p) :meaning)))
                                  (if m (format "- %s — %s — %s" (car p) f m)
                                    (format "- %s — %s" (car p) f))))
                              (cdr seg) "\n")))
                   parsed "\n\n")))
        (with-current-buffer (tibetan-fresh-file-buffer file)
          (unless (tibetan-cascade--set-body-in-buffer
                   2 tibetan-word-formation--heading body)
            (tibetan-cascade--insert-l2-in-analysis
             tibetan-word-formation--heading body))
          (save-buffer))
        ;; Row 2 of the gloss tables: the pure re-render (never fires).
        (tibetan-cascade--regenerate-after-land file)
        t))))

;; ============================================================================
;; Firing
;; ============================================================================

(defun tibetan-word-formation--request (file &optional force)
  "Queue the word-formation request for the Sanskrit cascade sentence
FILE.  t when queued; nil while one is in flight, without a Word
Analysis, or when present and FORCE is nil."
  (unless (tibetan-discussion--inflight-p "formation" file)
    (when (and (tibetan-word-formation--ready-p (tibetan-word-formation--inputs file))
               (or force (not (tibetan-word-formation--present-p file))))
      (tibetan-discussion--submit
       "formation" file
       (lambda () (tibetan-word-formation--build-prompts
                   (tibetan-word-formation--inputs file)))
       #'tibetan-word-formation--land))))

(defun tibetan-word-formation--maybe-fire (file)
  "Hook for `tibetan-cascade-after-land-functions': request the word
formation once the Claude analysis (with its Word Analysis) has
landed — unless switched off, MT deferred, or present."
  (when (and tibetan-word-formation-auto file (file-exists-p file)
             (not (tibetan-analysis--defer-mt-p file))
             (not (tibetan-word-formation--present-p file))
             (tibetan-word-formation--ready-p (tibetan-word-formation--inputs file)))
    (tibetan-word-formation--request file)))

(defun tibetan-word-formation-fetch (source-file sentences folder)
  "Hook for `tibetan-cascade-after-fetch-functions' (C-c u G): request
the missing word formation of every sentence of SENTENCES whose
cascade file in FOLDER has a Word Analysis — the sentences analysed
before §5.66 get it this way.  Returns nil on purpose: the
discussion paper does not show the tables and must not wait for it."
  (dolist (s sentences)
    (let ((file (tibetan-sentence--filepath (plist-get s :sent-num)
                                            folder source-file)))
      (when (and (file-exists-p file)
                 (not (tibetan-word-formation--present-p file))
                 (tibetan-word-formation--ready-p
                  (tibetan-word-formation--inputs file)))
        (tibetan-word-formation--request file))))
  nil)

(add-hook 'tibetan-cascade-after-land-functions #'tibetan-word-formation--maybe-fire)
(add-hook 'tibetan-cascade-after-fetch-functions #'tibetan-word-formation-fetch)

;;;###autoload
(defun tibetan-word-formation-dwim ()
  "(Re)request the word formation (gloss tables' row 2, §5.66) for
the Sanskrit sentence at point, or every sentence under the heading
at point (confirmed above `tibetan-cascade-subtree-confirm-threshold').
C-c u G fetches missing ones on its own — this forces new ones."
  (interactive)
  (let* ((scope (tibetan-cascade--scope-at-point))
         (src (plist-get scope :source))
         (folder (file-name-as-directory
                  (expand-file-name "analysis" (file-name-directory src))))
         (ready (cl-remove-if-not
                 (lambda (file)
                   (and (file-exists-p file)
                        (tibetan-word-formation--ready-p
                         (tibetan-word-formation--inputs file))))
                 (mapcar (lambda (n) (tibetan-sentence--filepath n folder src))
                         (plist-get scope :sent-nums)))))
    (when (and (> (length ready) tibetan-cascade-subtree-confirm-threshold)
               (not (y-or-n-p (format "%s: %d Wortbildungen bei Claude anfordern? "
                                      (plist-get scope :label) (length ready)))))
      (user-error "Abgebrochen"))
    (dolist (file ready)
      (tibetan-word-formation--request file t))
    (message "%s: %d Wortbildung(en) angefordert, %d ohne Word Analysis (erst C-c u G)"
             (plist-get scope :label) (length ready)
             (- (length (plist-get scope :sent-nums)) (length ready)))))

(provide 'tibetan-word-formation)
;;; tibetan-word-formation.el ends here
