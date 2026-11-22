;;; bcat-survey.el --- The sources on one passage side by side („Überblick“)  -*- lexical-binding: t; -*-

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Carsten 22.11.2026, when MSA XI.15–20 lay on the reMarkable with
;; verse, bhāṣya and Phywa pa as documents of their own: „Wenn es
;; mehrere Quellen gibt wäre so ein Übersichtsdokument noch gut.  Was
;; verschiebt sich z.B. im bhasya, bei Sthiramamti usw.  Welche
;; unterschiedlichen Gesichtspunkte betrachten die Übersetzungen“ —
;; „im Tantraloka z.B. auch Sanderson betont mehr das, Dyzkowki das“ —
;; „das Tibetische könnte, im Gegensatz zum Sanskrit, die
;; Übersetzung/Interpretation zu lassen“.  A hand-written sample for
;; XI.15 he confirmed: „Überblick ist genau das was ich meinte.“
;;
;; Every source of a passage is analysed on its own (the verse, each
;; commentary document, the translations compared).  The survey reads
;; ACROSS them — what each layer makes of the passage and what shifts
;; from one to the next, where the wording of one language allows what
;; the other does not, what each translator or lecturer attends to.
;;
;; The survey comes with the fetch: C-c u G of a verse ends with it
;; (Carsten 22.11.: „je Vers und nicht auf Zuruf, sondern wenn es was
;; gibt was beachtet werden sollte“) — one more request, and the survey
;; is written only when the answer finds something to note.  C-c u Q
;; gives it on demand, noteworthy or not.
;;
;; C-c u Q (`bcat-survey') asks ONE request per passage; it is given
;; only what is analysed already (`bcat-survey-material'): the layers
;; as the reading page of the dossier has them, the published
;; translations word for word, the lectures' excerpts.  The answer
;; becomes analysis/survey-<N>-<source>.org beside the sentence file
;; (template survey.org) and is kept (analysis/.answers/): the survey
;; is set anew without a request as long as its material is the same.
;; A survey someone worked on is never overwritten — the new draft goes
;; beside it.  C-c u H of the verse puts it on the reMarkable as a PDF
;; of its own („MSA XI.15 Überblick“, bcat-export.el).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'bcat-sentence)
(require 'bcat-template)
(require 'bcat-analysis)
(require 'bcat-dossier)

(declare-function bcat-relayout-file-context "bcat-relayout" (file))
(declare-function bcat-export-info "bcat-export" (file))
(declare-function bcat-export--view-context "bcat-export" (context &optional template))
(declare-function bcat-export--script-of "bcat-export" (line))
(declare-function bcat-sentence-partner-file "bcat-sentence" (sent-num source-file))
(declare-function tibetan-sentence--filepath "tibetan-sentence-persist" (sent-num &optional folder source-file))
(declare-function tibetan-sentence--source-file-from-analysis "tibetan-sentence-persist" (analysis-file))
(declare-function tibetan-discussion--submit "tibetan-translation-discussion" (kind file prompts land &optional fail))
(declare-function tibetan-cascade--scope-at-point "tibetan-cascade" ())

(defconst bcat-survey--stamp "CAT_SURVEY_WRITTEN"
  "The keyword of the stamp on a drafted survey
\(`bcat-analysis-draft-stamped').")

(defcustom bcat-survey-auto t
  "When non-nil, C-c u G of a passage ends with the survey of its
sources (`bcat-survey-after-fetch') — Carsten 22.11., asked whether it
should come on demand: „nicht auf Zuruf, sondern wenn es was gibt was
beachtet werden sollte“.  One more request per passage with at least
`bcat-survey-auto-sources' sources; the survey is written only when its
answer finds something to note (`bcat-survey-notable-p')."
  :type 'boolean
  :group 'buddhist-cat)

(defcustom bcat-survey-auto-sources 3
  "How many sources a passage must have for its survey to come with the
fetch (`bcat-survey-auto'): itself and two others — a commentary and a
published translation, two translations, a translation and a lecture.
With one published translation alone the comparison in the sentence
file says it all; C-c u Q still gives the survey on demand."
  :type 'integer
  :group 'buddhist-cat)

(defvar bcat-survey-done-functions nil
  "Called with (LEAD FILE) when a survey has been written — LEAD the
sentence file of the passage, FILE the survey, or the draft beside a
survey someone worked on.")

;; ----------------------------------------------------------------------------
;; What a survey is drawn from
;; ----------------------------------------------------------------------------

(defun bcat-survey--lead (file)
  "The sentence file whose passage FILE belongs to — the Sanskrit file
of a pair, the verse of a bhāṣya sentence, the first verse of a
commentary document; nil when FILE is none of these."
  (let ((file (if (equal (bcat-sentence-file-p file) "commentary")
                  (bcat-dossier--lead-of file)
                file)))
    (and file (plist-get (bcat-dossier-files file) :lead))))

(defun bcat-survey-file (file)
  "The survey of the passage the sentence FILE belongs to:
survey-<N>-<source>.org beside its sentence file (no name beginning
with „sent-“: no folder scan takes it for a sentence file)."
  (let ((lead (bcat-survey--lead file)))
    (and lead
         (expand-file-name
          (replace-regexp-in-string "\\`sent-" "survey-" (file-name-nondirectory lead))
          (file-name-directory lead)))))

(defun bcat-survey--plain (text)
  "TEXT of an analysis as it is given to a request: links as their
text, no block lines, no lines left blank at its ends."
  (string-trim
   (replace-regexp-in-string
    "^#\\+\\(?:begin\\|end\\)_[a-z]+[ \t]*\n?" ""
    (replace-regexp-in-string
     "\\[\\[[^]\n]+\\]\\[\\([^]\n]+\\)\\]\\]" "\\1"
     (or text "")))))

(defun bcat-survey--references (comparison)
  "The published translations in COMPARISON, the body of a file's
„Translations compared“: ((NAME . TEXT) …) as they stand there, word
for word — up to its first heading (the discussion of the points);
not the class's own „Translation“, not a machine translation."
  (let* ((head (substring comparison 0 (or (string-match "^\\*\\*\\* " comparison) (length comparison))))
         (pos 0) (marks '()) (out '()))
    (while (string-match "^\\*\\([^*\n]+\\):\\* " head pos)
      (push (list (match-string 1 head) (match-beginning 0) (match-end 0)) marks)
      (setq pos (match-end 0)))
    (setq marks (nreverse marks))
    (cl-loop for (mark next) on marks
             for name = (nth 0 mark)
             unless (or (equal name "Translation") (equal name "Deutsch")
                        (string-match-p "DharmaMitra" name))
             do (push (cons name (bcat-survey--plain
                                  (substring head (nth 2 mark) (if next (nth 1 next) (length head)))))
                      out))
    (nreverse out)))

(defun bcat-survey--transliterated (lines)
  "LINES (a text of Uchen over Wylie, Devanāgarī over IAST) with the
transliteration only: a line in the original script that is followed by
its transliteration is left out — a request needs the words once.  When
the script line was marked (*bold*: it takes up the passage), the
transliteration carries the mark."
  (let ((out '()) (rest (split-string (or lines "") "\n")))
    (while rest
      (let* ((line (pop rest))
             (bold (string-match "\\`\\*\\(.*\\)\\*\\'" line))
             (bare (if bold (match-string 1 line) line))
             (script (bcat-export--script-of bare))
             (next (car rest)))
        (if (and script next (not (string-blank-p next))
                 (not (bcat-export--script-of next)))
            (progn (pop rest)
                   (push (if bold (format "*%s*" (string-trim next)) next) out))
          (push line out))))
    (string-join (nreverse out) "\n")))

(defun bcat-survey--layer (layer)
  "The LAYER of a dossier (`bcat-dossier-context') as (NAME TEXT): its
lines (in transliteration, `bcat-survey--transliterated'), its
translation, its hint, its sentences each as „[N] lines → translation“."
  (let* ((heading (alist-get 'heading layer))
         (name (if (string-match "\\`[0-9]+ \\(.*?\\)\\(?: (.*)\\)?\\'" heading)
                   (match-string 1 heading)
                 heading))
         (parts (append
                 (mapcar (lambda (v) (bcat-survey--transliterated (alist-get 'text v)))
                         (alist-get 'verses layer))
                 (and (bcat-dossier--filled (alist-get 'translation layer))
                      (list (bcat-survey--plain (alist-get 'translation layer))))
                 (and (bcat-dossier--filled (alist-get 'note layer))
                      (list (concat "Hint of the analysis: " (alist-get 'note layer))))
                 (mapcar (lambda (item)
                           (concat (format "[%s]\n%s" (alist-get 'head item)
                                           (bcat-survey--transliterated (alist-get 'lines item)))
                                   (if (bcat-dossier--filled (alist-get 'item-translation item))
                                       (concat "\n→ " (alist-get 'item-translation item))
                                     "")))
                         (alist-get 'items layer)))))
    (list name (string-join (delq nil parts) "\n\n"))))

(defun bcat-survey-material (file)
  "What the survey of the passage of the sentence FILE is drawn from —
only what is analysed already, nothing is fetched: (:lead FILE
:lead-name NAME :source S :label L :work W :layers ((NAME TEXT) …)
:rendering TEXT :references ((NAME . TEXT) …) :lectures ((NAME . TEXT)
…) :documents (LABEL …)).  The layers as the reading page of the
dossier has them (`bcat-dossier-context': the passage in its languages
with the class's translation; the bhāṣya and the commentators sentence
by sentence with their translations); the note of the analysis on the
Tibetan rendering; the published translations word for word
\(`bcat-survey--references', of the file and of its partner); the
lectures' excerpts; the names of the documents all this stands in.
nil when FILE has no analysis to read."
  (require 'bcat-relayout)
  (require 'bcat-export)
  (let* ((lead (bcat-survey--lead file))
         (ctx (and lead (file-exists-p lead) (bcat-dossier-context lead t)))
         (read (and ctx (bcat-relayout-file-context lead))))
    (when (and ctx read (not (plist-get read :error)))
      (let* ((source (plist-get read :source))
             (context (plist-get read :context))
             (info (bcat-export-info lead))
             (n (plist-get (bcat-dossier-files lead) :sent-num))
             (partner (bcat-sentence-partner-file n source))
             (partner-read (and partner (file-exists-p partner)
                                (ignore-errors (bcat-relayout-file-context partner))))
             (references (bcat-survey--references (or (alist-get 'comparison context) "")))
             (references (append references
                                 (cl-remove-if
                                  (lambda (r) (assoc (car r) references))
                                  (bcat-survey--references
                                   (or (alist-get 'comparison (plist-get partner-read :context)) "")))))
             (lectures (delq nil
                             (mapcar (lambda (l)
                                       (let ((text (bcat-dossier--filled
                                                    (or (alist-get 'excerpt l) (alist-get 'lecture-body l)))))
                                         (and text (cons (alist-get 'lecture-name l) (bcat-survey--plain text)))))
                                     (alist-get 'lectures
                                                (bcat-export--view-context context (bcat-sentence-file-p lead)))))))
        (list :lead lead
              :lead-name (file-name-nondirectory lead)
              :source source
              :label (plist-get info :label)
              :work (plist-get info :work)
              :layers (mapcar #'bcat-survey--layer (alist-get 'layers ctx))
              :rendering (bcat-dossier--filled (alist-get 'rendering context))
              :references references
              :lectures lectures
              :documents (cons (plist-get info :label)
                               (mapcar (lambda (doc) (plist-get (bcat-export-info doc) :label))
                                       (alist-get 'commentary-docs ctx))))))))

(defun bcat-survey-sources (material)
  "The names of the sources in MATERIAL: its layers, the published
translations, the lecturers."
  (append (mapcar #'car (plist-get material :layers))
          (mapcar #'car (plist-get material :references))
          (mapcar #'car (plist-get material :lectures))))

;; ----------------------------------------------------------------------------
;; The request
;; ----------------------------------------------------------------------------

(defconst bcat-survey--schema
  "{\"notable\": \"yes\", \"gist\": \"…\", \"layers\": [{\"name\": \"…\", \"place\": \"…\", \"key\": \"…\", \"shift_key\": \"…\", \"says\": \"…\", \"shift\": \"…\"}], \"layers_point\": \"…\", \"wording\": [\"…\"], \"voices\": [{\"name\": \"…\", \"from\": \"…\", \"key\": \"…\", \"text\": \"…\"}], \"voices_line\": \"…\", \"questions\": [\"…\"]}"
  "The JSON contract of a survey (`bcat-survey-system-prompt').")

(defun bcat-survey-system-prompt ()
  "The system prompt of a survey — the same for every passage, so the
prompt cache stays warm.  Its wording was laid before Carsten
\(22.11.2026); change it only on his word."
  (concat
   "You compare the sources on one passage of a classical Indian or Tibetan text for a university reading class. You are given everything the class has on the passage: the passage itself, in one or two languages, with the class's own translation; its commentaries sentence by sentence, each with a translation; the published translations word for word; and, where there are any, excerpts of lectures. Each source has been analysed on its own already. What is missing is the view across them. Answer with ONE JSON object and nothing else (no prose, no code fence):\n\n"
   bcat-survey--schema
   "\n\nRules:\n"
   "- notable: \"yes\" when a reader of the single documents would miss something that shows only across them — a shift between the layers that changes what the passage is taken to say, a wording of one language that opens or closes a reading, translations that differ in more than words; \"no\" when the layers say the same in other words and the voices agree. Fill the other fields either way.\n"
   "- gist: what the passage itself says, in one or two sentences — only what it says, nothing a commentary adds.\n"
   "- layers: the passage and every commentary given, in the order given, the passage first. name: as given („Vers“, „Bhāṣya“, the commentator). place: the numbers of its sentences or segments as given. says: what this layer says about the passage, two to four sentences. shift: what changes against the layer before it — a term that comes in, something narrowed, widened, reinterpreted or dropped, the same image assigned differently, a new vocabulary; „—“ for the passage itself; where nothing shifts, say so. key and shift_key: says and shift in a few words each, for a table.\n"
   "- layers_point: the one shift a reader must not miss, and what would have to be clarified in class; two to four sentences. \"\" with one layer only.\n"
   "- wording: only when the passage is given in two languages — each entry one point where the wording of one language allows a reading the other does not, or settles what the other leaves open (a term that can be taken two ways, a case or a voice that changes, a reading of the text that one language decides). Name the words of both languages in /italic/ and say what follows for translation or interpretation. What makes no difference to the sense gets one entry at the end, together. [] with one language.\n"
   "- voices: every published translation and every lecturer given, then the class's own translation last. name: as given. from: what it translates from and whether it reads with a commentary, as far as the material shows („wohl“ when inferred). text: what this voice attends to or stresses — the decision it takes on the terms that matter, what it reads into the passage from a commentary, where it departs from the others; one to three sentences; a slip or an emendation is named as such. key: its characteristic choice in a few words, quoted where possible. A translation given twice (its original and a working translation of it) is one voice.\n"
   "- voices_line: the line that runs through the voices — who stands with whom, and what the difference rests on (the language translated from, a commentary followed, a term); and what all agree on. Two to four sentences. \"\" with fewer than two voices.\n"
   "- questions: two to four questions for the class that follow from the shifts and differences found — questions the material raises and does not settle.\n"
   "- Stay with what is given. Every statement about a source must be borne out by its words as given here; quote the decisive words (original in /italic/, a translation in „…“). A line of a commentary in *bold* takes up words of the passage. Bring in no other commentary, translation or doctrine from general knowledge; what you infer is marked „wohl“. Where the material leaves open who is right, do not decide — name what would decide it.\n"
   "- Assign nothing to a school or a doxographical category that the sources themselves do not name.\n"
   "- Write everything in German, plain, for a reader who is no specialist of the tradition; terms of the original in /italic/ with a rendering at their first use. No praise, no filler, nothing about your method.\n"
   "- JSON escaping: only \\\" and \\\\ — never a backslash before an apostrophe. Never mention yourself."))

(defun bcat-survey-user-prompt (material)
  "The user prompt of the survey of MATERIAL (`bcat-survey-material')."
  (concat
   (format "Passage: %s\n" (plist-get material :label))
   (if (plist-get material :work) (format "Work: %s\n" (plist-get material :work)) "")
   "\n"
   (mapconcat (lambda (layer) (format "== %s ==\n%s\n" (nth 0 layer) (nth 1 layer)))
              (plist-get material :layers) "\n")
   (if (plist-get material :rendering)
       (format "\n== Sanskrit and Tibetan (note of the analysis) ==\n%s\n" (plist-get material :rendering))
     "")
   (if (plist-get material :references)
       (concat "\n== Published translations ==\n"
               (mapconcat (lambda (r) (format "*%s:* %s\n" (car r) (cdr r)))
                          (plist-get material :references) "\n"))
     "")
   (if (plist-get material :lectures)
       (concat "\n== Lectures (excerpts) ==\n"
               (mapconcat (lambda (l) (format "*%s:* %s\n" (car l) (cdr l)))
                          (plist-get material :lectures) "\n"))
     "")
   "\nWrite the survey now."))

(defun bcat-survey--read (response)
  "The JSON object in Claude's RESPONSE as an alist, or nil."
  (bcat-analysis-read-object response))

;; ----------------------------------------------------------------------------
;; The survey as a file
;; ----------------------------------------------------------------------------

(defun bcat-survey--field (alist key)
  "The string under KEY in ALIST, trimmed; \"\" when missing."
  (let ((v (alist-get key alist)))
    (if (stringp v) (string-trim v) "")))

(defun bcat-survey--cell (text)
  "TEXT as the cell of a table: one line, no bar."
  (replace-regexp-in-string "|" "/" (replace-regexp-in-string "[ \t]*\n[ \t]*" " " text)))

(defun bcat-survey--list (value)
  "VALUE of an answer as a list (a vector of JSON, a list, or nothing)."
  (cond ((vectorp value) (append value nil))
        ((listp value) value)))

(defun bcat-survey--aligned (text)
  "The org TEXT with its tables aligned."
  (with-temp-buffer
    (insert text)
    (let ((org-inhibit-startup t))
      (delay-mode-hooks (org-mode)))
    (org-table-map-tables #'org-table-align t)
    (buffer-string)))

(defun bcat-survey-untouched-p (text)
  "Non-nil when the survey TEXT is a draft of the tool that nobody
changed since (`bcat-analysis-draft-untouched-p')."
  (bcat-analysis-draft-untouched-p text bcat-survey--stamp))

(defun bcat-survey-render (data material)
  "The survey of MATERIAL as org text (template survey), from the
answer DATA: the passage in one sentence; the layers — a table of key
words, then what each says and what shifts; Sanskrit and Tibetan; the
voices — a table, then each; the questions; where it all stands.  A
part without content is left out.  Stamped as a draft
\(`bcat-survey-untouched-p')."
  (let* ((empty (lambda (s) (member s '("" "—" "-" "–"))))
         (layers (mapcar (lambda (l)
                           (let ((shift (bcat-survey--field l 'shift)))
                             `((name . ,(bcat-survey--cell (bcat-survey--field l 'name)))
                               (place . ,(bcat-survey--field l 'place))
                               (key . ,(bcat-survey--cell (bcat-survey--field l 'key)))
                               (shift-key . ,(let ((k (bcat-survey--cell (bcat-survey--field l 'shift_key))))
                                               (if (string-empty-p k) "—" k)))
                               (says . ,(bcat-survey--field l 'says))
                               (shift . ,(if (funcall empty shift) "" shift)))))
                         (bcat-survey--list (alist-get 'layers data))))
         (wording (cl-remove-if #'string-empty-p
                                (mapcar (lambda (w) (if (stringp w) (string-trim w) ""))
                                        (bcat-survey--list (alist-get 'wording data)))))
         (voices (mapcar (lambda (v)
                           `((name . ,(bcat-survey--cell (bcat-survey--field v 'name)))
                             (from . ,(bcat-survey--cell (bcat-survey--field v 'from)))
                             (key . ,(bcat-survey--cell (bcat-survey--field v 'key)))
                             (text . ,(bcat-survey--field v 'text))))
                         (bcat-survey--list (alist-get 'voices data))))
         (questions (cl-loop for q in (bcat-survey--list (alist-get 'questions data))
                             for i from 1
                             when (and (stringp q) (not (string-blank-p q)))
                             collect `((num . ,(number-to-string i)) (text . ,(string-trim q)))))
         (label (plist-get material :label))
         (context
          `((label . ,label)
            (of-link . ,(format "[[file:%s][%s]]" (plist-get material :lead-name) label))
            (date . ,(format-time-string "%Y-%m-%d"))
            (gist . ,(bcat-survey--field data 'gist))
            (layers . ,layers)
            (layers-point . ,(bcat-survey--field data 'layers_point))
            (wording . ,(mapcar (lambda (w) `((text . ,w))) wording))
            (wording-p . ,(if wording "t" ""))
            (voices . ,voices)
            (voices-p . ,(if voices "t" ""))
            (voices-line . ,(bcat-survey--field data 'voices_line))
            (questions . ,questions)
            (questions-p . ,(if questions "t" ""))
            (documents . ,(mapcar (lambda (d) `((name . ,d))) (plist-get material :documents))))))
    (bcat-analysis-draft-stamped
     (bcat-survey--aligned
      (bcat-template-render-named "survey" context (plist-get material :source)))
     bcat-survey--stamp)))

(defun bcat-survey--target (survey)
  "Where a new draft for SURVEY is written: SURVEY itself — or, when
that file was worked on by hand, <name>-Entwurf.org beside it."
  (if (and (file-exists-p survey)
           (not (bcat-survey-untouched-p (bcat-sentence--text survey))))
      (concat (file-name-sans-extension survey) "-Entwurf.org")
    survey))

(defun bcat-survey-pdf-text (file)
  "The survey of the passage of the sentence FILE as the text of its
PDF — without the tool's stamp —, or nil when there is none."
  (let ((survey (bcat-survey-file file)))
    (and survey (file-readable-p survey)
         (replace-regexp-in-string
          (format "^#\\+%s:.*\n" bcat-survey--stamp) ""
          (bcat-sentence--text survey)))))

;; ----------------------------------------------------------------------------
;; The answers are kept
;; ----------------------------------------------------------------------------

(defun bcat-survey--store (survey)
  "Where the answer of the survey SURVEY is kept:
analysis/.answers/<its name>.eld — with those of the sentence files."
  (expand-file-name (concat ".answers/" (file-name-base survey) ".eld")
                    (file-name-directory survey)))

(defun bcat-survey--stored (survey)
  "What is kept for SURVEY: (:key MD5 :answer OBJ), or nil."
  (let ((path (bcat-survey--store survey)))
    (when (file-readable-p path)
      (ignore-errors
        (with-temp-buffer
          (insert-file-contents path)
          (let ((data (read (current-buffer))))
            (and (listp data) (plist-get data :answer) data)))))))

(defun bcat-survey--keep (survey key answer)
  "Keep ANSWER, given for the material with KEY, for SURVEY."
  (let ((path (bcat-survey--store survey))
        (coding-system-for-write 'utf-8-unix)
        (print-length nil) (print-level nil))
    (make-directory (file-name-directory path) t)
    (with-temp-file path
      (insert ";; Antwort des Überblicks — C-c u Q setzt aus ihr neu, C-u C-c u Q ruft neu ab.\n")
      (prin1 (list :key key :answer answer) (current-buffer))
      (insert "\n"))))

(defun bcat-survey--key (material)
  "What makes MATERIAL the same material again: its request."
  (md5 (bcat-survey-user-prompt material) nil nil 'utf-8))

(defun bcat-survey-pending-p (file &optional fresh)
  "Non-nil when the survey of the passage of FILE needs a request: no
answer is kept for the material as it is now, or FRESH asks for a new
one."
  (let* ((survey (bcat-survey-file file))
         (material (and survey (bcat-survey-material file)))
         (stored (and survey (bcat-survey--stored survey))))
    (or fresh
        (not (and material stored
                  (equal (plist-get stored :key) (bcat-survey--key material)))))))

;; ----------------------------------------------------------------------------
;; C-c u Q
;; ----------------------------------------------------------------------------

(defun bcat-survey--write (survey answer material)
  "Write the survey SURVEY from ANSWER and MATERIAL
\(`bcat-survey--target'); returns the file written."
  (let ((target (bcat-survey--target survey))
        (text (bcat-survey-render answer material))
        (coding-system-for-write 'utf-8-unix))
    (make-directory (file-name-directory target) t)
    (let ((buf (get-file-buffer target)))
      (if buf
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer) (insert text) (save-buffer)))
        (with-temp-file target (insert text))))
    (message "Überblick %s: %s geschrieben%s — prüfen; C-c u H legt ihn mit dem Vers aufs reMarkable"
             (plist-get material :label) (file-name-nondirectory target)
             (if (equal target survey) ""
               (format " — NEBEN %s, das von Hand bearbeitet ist" (file-name-nondirectory survey))))
    (run-hook-with-args 'bcat-survey-done-functions (plist-get material :lead) target)
    target))

(defun bcat-survey-notable-p (answer)
  "Non-nil unless ANSWER, a survey's, says outright that there is
nothing to note across the sources (its `notable' is „no“) — in doubt
the survey is shown."
  (let ((v (alist-get 'notable answer)))
    (not (and (stringp v)
              (member (downcase (string-trim v)) '("no" "nein" "false"))))))

(defun bcat-survey-fetch (file &optional fresh auto)
  "Write the survey of the passage of the sentence FILE: ONE request
that reads what is analysed (`bcat-survey-material') — or none, when
the answer for this material is kept and FRESH does not ask for a new
one: then the survey is set anew from it.  AUTO: it comes with the
fetch, unasked (`bcat-survey-after-fetch') — then a survey that finds
nothing to note (`bcat-survey-notable-p') is not written, only kept.
Asynchronous; returns the number of requests (1 or 0), nil while one is
under way.  A passage with one source only has no survey
\(`user-error')."
  (let* ((material (or (bcat-survey-material file)
                       (user-error "%s hat keine Analyse, aus der ein Überblick entstehen könnte"
                                   (file-name-nondirectory file))))
         (sources (bcat-survey-sources material)))
    (unless (cdr sources)
      (user-error "%s hat nur eine Quelle (%s) — kein Überblick"
                  (plist-get material :label) (or (car sources) "keine")))
    (let* ((survey (bcat-survey-file file))
           (key (bcat-survey--key material))
           (stored (bcat-survey--stored survey))
           (set (lambda (answer)
                  (if (and auto (not (bcat-survey-notable-p answer)))
                      (message "Überblick %s: nichts, was über die einzelnen Dokumente hinaus zu beachten wäre — nicht geschrieben (C-c u Q zeigt ihn trotzdem)"
                               (plist-get material :label))
                    (bcat-survey--write survey answer material)))))
      (if (and (not fresh) stored (equal key (plist-get stored :key)))
          (progn (funcall set (plist-get stored :answer)) 0)
        (require 'tibetan-translation-discussion)
        (message "Überblick %s: Abruf …" (plist-get material :label))
        (and (tibetan-discussion--submit
              "bcat-survey" (plist-get material :lead)
              (lambda () (cons (bcat-survey-system-prompt) (bcat-survey-user-prompt material)))
              (lambda (_file response)
                (let ((answer (bcat-survey--read response)))
                  (if (not answer)
                      (message "Überblick %s: Antwort nicht lesbar — nichts geschrieben"
                               (plist-get material :label))
                    (bcat-survey--keep survey key answer)
                    (funcall set answer))))
              (lambda (_file reason)
                (message "Überblick %s: kein Ergebnis (%s) — nichts geschrieben"
                         (plist-get material :label) (or reason "ohne Antwort"))))
             1)))))

(defvar bcat-export-passage-complete-functions)

(defun bcat-survey-after-fetch (file)
  "Hook for `bcat-export-passage-complete-functions': C-c u G ends with
the survey of the passage of the sentence FILE (`bcat-survey-auto') —
when it has sources enough (`bcat-survey-auto-sources') and no answer
is kept for its material as it is now.  Never signals: a survey is no
reason to spoil a fetch."
  (when bcat-survey-auto
    (condition-case err
        (let ((material (bcat-survey-material file)))
          (when (and material
                     (>= (length (bcat-survey-sources material)) bcat-survey-auto-sources)
                     (bcat-survey-pending-p file))
            (bcat-survey-fetch file nil t)))
      (error (message "Überblick nach C-c u G (%s): %s"
                      (file-name-nondirectory file) (error-message-string err))))))

(add-hook 'bcat-export-passage-complete-functions #'bcat-survey-after-fetch)

;;;###autoload
(defun bcat-survey (&optional fresh)
  "C-c u Q: the survey („Überblick“) of the sources on the passage at
hand — in a sentence file that passage, in a source the sentence at
point (Carsten 22.11.: what shifts from the verse to the bhāṣya to the
commentators, what the wording of Sanskrit and Tibetan allows, what
each translator or lecturer attends to).  Asks first, naming the
sources and the request; with the answer for this material at hand the
survey is only set anew, without a request.  With a prefix argument
\(C-u C-c u Q, FRESH) it is asked for again.  The result is a draft to
be checked; a survey worked on by hand is not overwritten."
  (interactive "P")
  (let* ((here (or (buffer-file-name) (user-error "Dieser Puffer hat keine Datei")))
         (file (if (bcat-sentence-file-p here)
                   here
                 (require 'tibetan-cascade)
                 (let* ((scope (tibetan-cascade--scope-at-point))
                        (source (plist-get scope :source)))
                   (tibetan-sentence--filepath (car (plist-get scope :sent-nums))
                                               (bcat-sentence--analysis-folder source) source))))
         (material (or (bcat-survey-material file)
                       (user-error "%s hat keine Analyse, aus der ein Überblick entstehen könnte"
                                   (file-name-nondirectory file))))
         (sources (bcat-survey-sources material))
         (pending (bcat-survey-pending-p file fresh))
         (survey (bcat-survey-file file))
         (beside (if (equal (bcat-survey--target survey) survey) ""
                   (format "; %s ist von Hand bearbeitet, der Entwurf kommt daneben"
                           (file-name-nondirectory survey)))))
    (when (y-or-n-p
           (if pending
               (format "Überblick für %s — %d Quellen (%s), 1 Abruf%s? "
                       (plist-get material :label) (length sources) (string-join sources ", ") beside)
             (format "Überblick für %s aus der vorliegenden Antwort neu setzen — kein Abruf (C-u C-c u Q ruft neu ab)%s? "
                     (plist-get material :label) beside)))
      (bcat-survey-fetch file fresh))))

(provide 'bcat-survey)
;;; bcat-survey.el ends here
