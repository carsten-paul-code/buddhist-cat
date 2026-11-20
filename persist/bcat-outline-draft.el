;;; bcat-outline-draft.el --- Draft the outline of a document with Claude -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Carsten 20.11.2026, on the outline (Gliederung, sa bcad) as a pattern
;; for every document: „selbst entwerfen und ggf. die Tantraloka
;; Gliederung überarbeiten.  Es soll generisch und Teil des Workflows
;; sein.  Wenn künftig ein neues Werk hinzu kommt soll in der
;; Vorbereitung für's CAT (Segmentierung etc.) auch gleich mit die
;; Gliederung.  Reverse für das was wir im Moment (WS26-27) in Arbeit
;; haben.“
;;
;; C-c u S drafts the outline of the source at hand: the sentences of
;; the source — number, verse, folio, text, and the translation its
;; sentence file or its references have („reverse“: what is analysed
;; already goes in) — are sent to Claude, in parts where the document
;; is large; the answer, one JSON object of nested nodes, is set as an
;; org file in the form of the Tantrāloka's outline
;; (bcat-outline.el reads it): the work as a whole, the nodes with the
;; line of what they cover, a line per sentence of prose or commentary,
;; the list „Ort im Ganzen“.  A draft is a draft: its subtitle says so,
;; and an outline someone worked on by hand is never overwritten — the
;; new draft goes beside it.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'bcat-outline)

(declare-function bcat-sentence--sentences "bcat-sentence" (file))
(declare-function bcat-sentence--references "bcat-sentence" (sent-num source-file))
(declare-function bcat-sentence--text "bcat-sentence" (file))
(declare-function bcat-sentence-template-name "bcat-sentence" (source-file))
(declare-function bcat-analysis--decode "bcat-analysis" (json))
(declare-function bcat-analysis--repair-json "bcat-analysis" (json))
(declare-function bcat-analysis--close-quotes "bcat-analysis" (value))
(declare-function tibetan-sentence--filepath "tibetan-sentence-persist" (sent-num &optional folder source-file))
(declare-function tibetan-sentence--source-file-from-analysis "tibetan-sentence-persist" (analysis-file))
(declare-function tibetan-analysis--read-source-metadata "tibetan-analysis-claude" (source-file))
(declare-function tibetan-discussion--submit "tibetan-translation-discussion" (kind file prompts land &optional fail))

(defgroup bcat-outline nil
  "The outline (Gliederung, sa bcad) of a document."
  :group 'buddhist-cat)

(defcustom bcat-outline-part-size 24000
  "Characters of text and translation one request of a draft takes
\(`bcat-outline-draft--parts').  A document within it is drafted in one
request; a larger one in parts, cut where the source begins a section."
  :type 'integer
  :group 'bcat-outline)

;; ----------------------------------------------------------------------------
;; What goes into a draft
;; ----------------------------------------------------------------------------

(defun bcat-outline-draft--squeeze (text)
  "TEXT on one line, blanks squeezed."
  (string-trim (replace-regexp-in-string "[ \t\n]+" " " (or text ""))))

(defun bcat-outline-draft--translation (source num)
  "The translation at hand for sentence NUM of SOURCE, or nil: the own
one of its sentence file („*Translation:* …“ — after an analysis), else
its first reference translation with the translator's name.  Read from
the files as they are; nothing is saved or changed."
  (let* ((folder (file-name-as-directory (expand-file-name "analysis" (file-name-directory source))))
         (file (ignore-errors (tibetan-sentence--filepath num folder source)))
         (text (and file (file-readable-p file) (bcat-sentence--text file)))
         (own (and text (string-match "^\\*Translation:\\*[ \t]+\\(.+\\)$" text)
                   (bcat-outline-draft--squeeze (match-string 1 text))))
         (ref (and (not own) (car (ignore-errors (bcat-sentence--references num source))))))
    (cond ((and own (not (string-empty-p own))) own)
          (ref (format "%s (%s)" (bcat-outline-draft--squeeze (cdr ref)) (car ref))))))

(defun bcat-outline-draft--items (source)
  "The sentences of SOURCE as a draft sees them, in the order of the
source: plists (:num :verse :section :folio-from :folio-to :text
:translation) — the text its segments joined, the translation what is
at hand (`bcat-outline-draft--translation')."
  (require 'bcat-sentence)
  (mapcar (lambda (s)
            (let ((num (plist-get s :sent-num))
                  (folios (delq nil (mapcar #'cdr (plist-get s :folios)))))
              (list :num num
                    :verse (plist-get s :verse)
                    :section (plist-get s :section-label)
                    :folio-from (car folios)
                    :folio-to (car (last folios))
                    :text (bcat-outline-draft--squeeze
                           (mapconcat (lambda (g) (cdr g)) (plist-get s :segs) " "))
                    :translation (bcat-outline-draft--translation source num))))
          (bcat-sentence--sentences source)))

(defun bcat-outline-draft--info (source)
  "What a draft knows of the document SOURCE: (:title :context :sections
:date) — its #+TITLE up to „ — “, its #+TIBETAN_CLAUDE_CONTEXT lines,
the labels of its sections in order."
  (require 'bcat-sentence)
  (let* ((title (bcat-outline--header source "TITLE"))
         (meta (ignore-errors (tibetan-analysis--read-source-metadata source)))
         (sections (delete-dups
                    (delq nil (mapcar (lambda (s) (plist-get s :section-label))
                                      (bcat-sentence--sentences source))))))
    (list :title (if title (string-trim (car (split-string title " — "))) (file-name-base source))
          :context (plist-get meta :claude-context)
          :sections sections
          :date (format-time-string "%Y-%m-%d"))))

(defun bcat-outline-draft--size (item)
  "The characters ITEM brings into a request."
  (+ (length (plist-get item :text)) (length (or (plist-get item :translation) ""))))

(defun bcat-outline-draft--parts (items size)
  "ITEMS (`bcat-outline-draft--items') as the parts of a draft: lists of
consecutive items, each up to SIZE characters of text and translation —
cut where the source begins a section; a section larger than SIZE is
cut between its sentences.  nil without ITEMS."
  (let ((sections '()) (parts '()) (current '()) (used 0))
    ;; the runs of sentences of one section
    (dolist (item items)
      (if (and sections (equal (plist-get item :section) (plist-get (car (car sections)) :section)))
          (push item (car sections))
        (push (list item) sections)))
    (setq sections (nreverse (mapcar #'nreverse sections)))
    (cl-flet ((flush () (when current (push (nreverse current) parts) (setq current '() used 0))))
      (dolist (section sections)
        (let ((weight (apply #'+ (mapcar #'bcat-outline-draft--size section))))
          (cond
           ;; fits with what is there
           ((<= (+ used weight) size)
            (setq current (append (reverse section) current) used (+ used weight)))
           ;; fits on its own
           ((<= weight size)
            (flush)
            (setq current (reverse section) used weight))
           ;; larger than a part: between its sentences
           (t
            (flush)
            (dolist (item section)
              (let ((w (bcat-outline-draft--size item)))
                (when (and current (> (+ used w) size)) (flush))
                (push item current)
                (setq used (+ used w))))
            (flush)))))
      (flush))
    (nreverse parts)))

;; ----------------------------------------------------------------------------
;; The requests
;; ----------------------------------------------------------------------------

(defconst bcat-outline-draft--schema
  "{\"title\": \"…\", \"intro\": \"…\", \"nodes\": [{\"title\": \"…\", \"short\": \"…\", \"from\": 0, \"to\": 0, \"text\": \"…\", \"transition\": \"…\", \"nodes\": [], \"sentences\": [{\"num\": 0, \"role\": \"…\", \"text\": \"…\"}]}], \"work\": {\"idea\": \"…\", \"groups\": [{\"title\": \"…\", \"text\": \"…\"}]}}"
  "The JSON contract of a draft (`bcat-outline-draft-system-prompt').")

(defun bcat-outline-draft-system-prompt ()
  "The system prompt of a draft — the same for every document and every
part, so the prompt cache stays warm.  The form asked for is that of
WS26-27/Tantraloka/Gliederung-TA1.org (Tantrāloka-Sitzung, 20.11.)."
  (concat
   "You prepare the outline of a classical Indian or Tibetan text for a university reading class — in German a Gliederung, built like a Tibetan sa bcad. When a session is prepared, it shows where the sentence being read stands in the whole and what belongs together. You are given the sentences of the text, or of a part of it, with their numbers. Answer with ONE JSON object and nothing else (no prose, no code fence):\n\n"
   bcat-outline-draft--schema
   "\n\nRules:\n"
   "- nodes: the given sentences divided into parts, sections and subsections, in the order of the text, nested at most four deep. Every given sentence belongs to exactly one innermost node. from, to: the numbers of the first and the last sentence of the node, as given (numbers need not rise: a title may be 1001 and stand before verse 1). Where the text divides itself — the source's own sections, a list of topics it announces, the lemmata a commentary takes up one after the other — follow that division; a commentary stands at its place, under the verse or passage it explains, divided by what it explains (several readings or rounds of glossing as branches of their own, below them lemma by lemma). Introductory verses (homage, purpose, modesty) each get a node of their own.\n"
   "- title: in German, short, numbered like a sa bcad (I., A., 1., a.) and saying what the node is about: „II. Wissen und Nichtwissen: Was bindet, was befreit?“; a node of a commentary after what it explains: „b. Erstes Beiwort des Herzens: /tadubhaya-yāmala/“.\n"
   "- short: the title in one to three words, for a path of places: „Deutung 1“, „Mutter“, „Beiwort 2 (abhinavasṛṣṭimahā)“.\n"
   "- text: two to four sentences on what the node says — its content and its point, not a list of its sentences.\n"
   "- transition: where it helps, one sentence on how the node follows from the one before; else \"\".\n"
   "- sentences: only in an innermost node — one entry for every sentence of prose or commentary, and for every verse that stands alone in its node. num: its number. role: what it does, in one or two words (Titel, Frage, Auflösung, Beleg, Exkurs, These, Einwand, Antwort, Beispiel, Zusammenfassung, Überleitung, Kolophon …). text: one to three sentences — what the sentence does, what it explains, and how it hangs together with its neighbours, named by their numbers („begründet, warum in 1017 …; der Beleg folgt in 1019“). A quotation with its source only when the text or a given translation names it. The verses of a root text that a node treats together need no entries: the node's text covers them.\n"
   "- work: the work as a whole. idea: the thought that orders it, three to five sentences. groups: its chapters or parts in their large groups — title: a line naming the group („Kap. 2–4: die drei höchsten Wege“), text: one or two sentences on how they hang together. From the text's own division where it gives one; what you know only from general knowledge of the work is marked „wohl“ or „nicht geprüft“.\n"
   "- title (of the object): the name of this outline, „<Werk oder Kapitel>: Gliederung“. intro: two or three sentences on how the text is divided and what the division rests on.\n"
   "- Write everything in German, plain, for a reader who is no specialist of the tradition; terms of the original in /italic/ with a rendering at their first use. What is not certain is marked „wohl“ or „nicht geprüft“. Assign the text to no school and no doxographical category (Svātantrika, Prāsaṅgika and the like).\n"
   "- Invent nothing: no source of a quotation, no parallel, no fact about author or work that neither the text, the translations given nor the notes on the document support. Build on the translations given („=“); where there is none, on the text itself.\n"
   "- JSON escaping: only \\\" and \\\\ — never a backslash before an apostrophe. Never mention yourself."))

(defun bcat-outline-draft--folios (item)
  "The folios of ITEM as „A“ or „A–B“, or nil."
  (let ((from (plist-get item :folio-from)) (to (plist-get item :folio-to)))
    (cond ((and from to (not (equal from to))) (format "%s–%s" from to))
          (from from)
          (to to))))

(defun bcat-outline-draft--sentence-lines (items)
  "ITEMS as the lines of a request: „[N] verse V · folio: text“ and, when
a translation is at hand, „  = translation“; before the first sentence
of a section of the source its label between dashes."
  (let ((section 'none) (lines '()))
    (dolist (item items)
      (unless (equal (plist-get item :section) section)
        (setq section (plist-get item :section))
        (when section (push (format "— %s —" section) lines)))
      (push (format "[%d] %s%s: %s"
                    (plist-get item :num)
                    (if (plist-get item :verse) (concat "verse " (plist-get item :verse)) "prose")
                    (let ((folios (bcat-outline-draft--folios item)))
                      (if folios (concat " · " folios) ""))
                    (plist-get item :text))
            lines)
      (when (plist-get item :translation)
        (push (concat "  = " (plist-get item :translation)) lines)))
    (string-join (nreverse lines) "\n")))

(defun bcat-outline-draft--notes (info)
  "The notes on the document INFO for a request, or \"\": its
#+TIBETAN_CLAUDE_CONTEXT lines under a line that says what they are —
they were written for the analysis of the sentences and may name a
target language; the outline is German all the same."
  (if (plist-get info :context)
      (concat "Notes on the document (written for the analysis of its sentences; "
              "the outline is in German whatever they say of a target language):\n"
              (string-join (plist-get info :context) "\n") "\n")
    ""))

(defun bcat-outline-draft-user-prompt (info items &optional part parts before)
  "The user prompt of a draft for the document INFO
\(`bcat-outline-draft--info') with the sentences ITEMS.  For a document
drafted in parts: PART of PARTS, BEFORE the titles of the nodes
outlined so far — such a request leaves the view of the whole to the
last one (`bcat-outline-draft-frame-prompt')."
  (concat
   "Document: " (plist-get info :title) "\n"
   (bcat-outline-draft--notes info)
   (if (plist-get info :sections)
       (concat "\nThe source's own sections, in order:\n"
               (mapconcat (lambda (s) (concat "- " s)) (plist-get info :sections) "\n")
               "\n")
     "")
   (if (and part parts)
       (concat (format "\nThis is part %d of %d of the document; outline only the sentences given here." part parts)
               (if before
                   (concat "\nOutlined so far:\n" (mapconcat (lambda (b) (concat "- " b)) before "\n")
                           "\nContinue its numbering and its depth.")
                 "")
               "\nLeave \"work\", \"title\" and \"intro\" empty — they follow when all parts are outlined.\n")
     "")
   "\nSentences — number, verse or prose, folio; „=“ a translation at hand:\n"
   (bcat-outline-draft--sentence-lines items)
   "\n\nReturn the JSON object now."))

(defun bcat-outline-draft-frame-prompt (info tops)
  "The user prompt of the last request of a document drafted in parts:
INFO the document, TOPS the outermost nodes of all parts as lines
„title (from–to): text“.  It asks for the frame — title, intro, the
work as a whole, and the large parts („groups“) the nodes fall into."
  (concat
   "Document: " (plist-get info :title) "\n"
   (bcat-outline-draft--notes info)
   "\nAll parts of the document are outlined. Their outermost nodes, in order:\n"
   (mapconcat (lambda (top) (concat "- " top)) tops "\n")
   "\n\nReturn ONE JSON object: {\"title\": \"…\", \"intro\": \"…\", \"work\": {\"idea\": \"…\", \"groups\": [{\"title\": \"…\", \"text\": \"…\"}]}, \"groups\": [{\"title\": \"…\", \"short\": \"…\", \"from\": 0, \"to\": 0, \"text\": \"…\", \"transition\": \"…\"}]} — title, intro and work as in the rules; groups (of the object): the large parts of the text that the nodes above fall into, each with the numbers of its first and its last sentence, covering the nodes without gap in their order; [] when the nodes above are the large parts already."))

;; ----------------------------------------------------------------------------
;; From the answers to the outline
;; ----------------------------------------------------------------------------

(defun bcat-outline-draft--read (response)
  "The JSON object in Claude's RESPONSE as an alist, or nil."
  (require 'bcat-analysis)
  (when (and (stringp response) (string-match "{" response))
    (let* ((start (match-beginning 0))
           (end (string-match "}[^}]*\\'" response)))
      (when (and end (> end start))
        (let* ((json (substring response start (1+ end)))
               (obj (or (ignore-errors (bcat-analysis--decode json))
                        (ignore-errors (bcat-analysis--decode (bcat-analysis--repair-json json))))))
          (and (consp obj) (bcat-analysis--close-quotes obj)))))))

(defun bcat-outline-draft--number (value)
  "VALUE (a number, or digits as a string) as a number, or nil."
  (cond ((integerp value) value)
        ((and (stringp value) (string-match-p "\\`[ \t]*[0-9]+[ \t]*\\'" value)) (string-to-number value))))

(defun bcat-outline-draft--field (alist key)
  "The string under KEY in ALIST, trimmed; \"\" when there is none."
  (let ((v (alist-get key alist)))
    (if (stringp v) (string-trim v) "")))

(defun bcat-outline-draft--span (node items)
  "(FIRST . LAST), the positions in ITEMS (the order of the source) that
NODE of an answer covers: from its first to its last sentence as given,
widened by what its nodes below cover; nil when it names no sentence
of ITEMS."
  (let* ((position (lambda (n) (and n (cl-position n items :key (lambda (i) (plist-get i :num))))))
         (from (funcall position (bcat-outline-draft--number (alist-get 'from node))))
         (to (funcall position (bcat-outline-draft--number (alist-get 'to node))))
         (below (delq nil (mapcar (lambda (c) (bcat-outline-draft--span c items)) (alist-get 'nodes node))))
         (points (append (delq nil (list from to)) (mapcar #'car below) (mapcar #'cdr below))))
    (and points (cons (apply #'min points) (apply #'max points)))))

(defun bcat-outline-draft--runs (numbers)
  "NUMBERS in the order of the source as runs: „1001, 1–3“."
  (let ((runs '()))
    (dolist (n numbers)
      (if (and runs (= n (1+ (cdr (car runs)))))
          (setcdr (car runs) n)
        (push (cons n n) runs)))
    (mapconcat (lambda (r) (if (= (car r) (cdr r)) (number-to-string (car r)) (format "%d–%d" (car r) (cdr r))))
               (nreverse runs) ", ")))

(defun bcat-outline-draft--verses (labels)
  "The verse LABELS of a node as one item of its line: „SDA 1–3“ when
they share their name and count on, „SDA 1“ for one, else first –
last."
  (let ((parse (lambda (l) (and (string-match "\\`\\(.*?\\)[ \t]*\\([0-9]+\\)\\'" l)
                                (cons (match-string 1 l) (string-to-number (match-string 2 l)))))))
    (cond ((null labels) nil)
          ((null (cdr labels)) (car labels))
          (t (let ((a (funcall parse (car labels))) (b (funcall parse (car (last labels)))))
               (if (and a b (equal (car a) (car b)))
                   (format "%s %d–%d" (car a) (cdr a) (cdr b))
                 (format "%s – %s" (car labels) (car (last labels)))))))))

(defun bcat-outline-draft--head-line (span items)
  "The line under a node's heading for SPAN (positions in ITEMS): its
sentences („Satz 1001“, „Sätze 1001, 1–3“ — what `bcat-outline--ranges'
reads), their verses, their folios, joined by „ · “."
  (let* ((covered (cl-subseq items (car span) (1+ (cdr span))))
         (numbers (mapcar (lambda (i) (plist-get i :num)) covered))
         (verses (bcat-outline-draft--verses (delq nil (mapcar (lambda (i) (plist-get i :verse)) covered))))
         (from (cl-some (lambda (i) (plist-get i :folio-from)) covered))
         (to (cl-some (lambda (i) (plist-get i :folio-to)) (reverse covered))))
    (string-join
     (delq nil (list (concat (if (cdr numbers) "Sätze " "Satz ") (bcat-outline-draft--runs numbers))
                     verses
                     (cond ((and from to (not (equal from to))) (format "%s–%s" from to))
                           (from from))))
     " · ")))

(defun bcat-outline-draft--role (entry)
  "The role of the sentence ENTRY with a capital, or \"\"."
  (let ((role (bcat-outline-draft--field entry 'role)))
    (if (string-empty-p role) "" (concat (upcase (substring role 0 1)) (substring role 1)))))

(defun bcat-outline-draft--entry-line (entry)
  "The line of a single sentence: „- *N*: Rolle. text“ — the role not
said twice when the text begins with it."
  (let* ((num (bcat-outline-draft--number (alist-get 'num entry)))
         (role (bcat-outline-draft--role entry))
         (text (bcat-outline-draft--squeeze (bcat-outline-draft--field entry 'text)))
         (case-fold-search t))
    (and num
         (format "- *%d*: %s" num
                 (cond ((string-empty-p role) text)
                       ((string-empty-p text) (concat role "."))
                       ((string-match-p (concat "\\`" (regexp-quote role) "\\(?:[.:]\\|\\_>\\)") text) text)
                       (t (concat role ". " text)))))))

(defun bcat-outline-draft--node-text (node items depth)
  "NODE of an answer as org text at DEPTH (1: a heading of level 2):
heading, the line of what it covers, transition, what it says, the
lines of its sentences, the nodes below.  Below the fifth level a node
is a bold line."
  (let* ((span (bcat-outline-draft--span node items))
         (title (bcat-outline-draft--squeeze (bcat-outline-draft--field node 'title)))
         (transition (bcat-outline-draft--squeeze (bcat-outline-draft--field node 'transition)))
         (text (bcat-outline-draft--squeeze (bcat-outline-draft--field node 'text)))
         (entries (delq nil (mapcar #'bcat-outline-draft--entry-line (alist-get 'sentences node))))
         (head (and span (bcat-outline-draft--head-line span items))))
    (concat
     (if (<= depth 4)
         (concat (make-string (1+ depth) ?*) " " title "\n" (if head (concat head "\n") ""))
       (concat "*" title (if head (concat " (" head ")") "") "*\n"))
     "\n"
     (if (string-empty-p transition) "" (concat "Übergang: " transition "\n\n"))
     (if (string-empty-p text) "" (concat text "\n\n"))
     (if entries (concat (string-join entries "\n") "\n\n") "")
     (mapconcat (lambda (child) (bcat-outline-draft--node-text child items (1+ depth)))
                (alist-get 'nodes node) ""))))

(defun bcat-outline-draft--place-of (nodes items position)
  "(PATH . ROLE) for the sentence at POSITION of ITEMS in the tree NODES:
the short titles on the way down to the innermost node that covers it,
and the role of its own line, when it has one; nil when no node covers
it."
  (let ((num (plist-get (nth position items) :num))
        (path '()) (role nil) (level nodes) (found t))
    (while (and level found)
      (setq found (cl-find-if (lambda (n)
                                (let ((span (bcat-outline-draft--span n items)))
                                  (and span (<= (car span) position (cdr span)))))
                              level))
      (when found
        (let ((short (bcat-outline-draft--squeeze (bcat-outline-draft--field found 'short))))
          (push (if (string-empty-p short)
                    (bcat-outline-draft--squeeze (bcat-outline-draft--field found 'title))
                  short)
                path))
        (let ((entry (cl-find num (alist-get 'sentences found)
                              :key (lambda (e) (bcat-outline-draft--number (alist-get 'num e))))))
          (when entry (setq role (bcat-outline-draft--role entry))))
        (setq level (alist-get 'nodes found))))
    (and path (cons (string-join (nreverse path) " › ") role))))

(defun bcat-outline-draft--uncovered (data items)
  "The numbers of the sentences of ITEMS that no node of the answer DATA
covers — to be said, never dropped in silence."
  (let ((nodes (alist-get 'nodes data)))
    (cl-loop for item in items for position from 0
             unless (bcat-outline-draft--place-of nodes items position)
             collect (plist-get item :num))))

(defun bcat-outline-draft--places (data items)
  "The list „Ort im Ganzen“ for the answer DATA: a line for every
sentence of ITEMS — the way down the short titles, „: Rolle“ when the
sentence has a line of its own; a sentence no node covers stands under
the node before it, marked „(nicht eingeordnet)“."
  (let ((nodes (alist-get 'nodes data)) (last nil))
    (cl-loop for item in items for position from 0
             collect (let ((place (bcat-outline-draft--place-of nodes items position)))
                       (format "- %d: %s" (plist-get item :num)
                               (cond (place
                                      (setq last (car place))
                                      (if (and (cdr place) (not (string-empty-p (cdr place))))
                                          (concat (car place) ": " (cdr place))
                                        (car place)))
                                     (last (concat last " (nicht eingeordnet)"))
                                     (t "(nicht eingeordnet)")))))))

(defconst bcat-outline-draft--latex-head
  (concat
   "#+LATEX_COMPILER: lualatex\n"
   "#+LATEX_CLASS: article\n"
   "#+LATEX_CLASS_OPTIONS: [11pt,a4paper]\n"
   "#+LATEX_HEADER: \\usepackage[a4paper,left=2.5cm,right=2.5cm,top=2.3cm,bottom=2.3cm]{geometry}\n"
   "#+LATEX_HEADER: \\usepackage{fontspec}\n"
   "#+LATEX_HEADER: \\usepackage{libertinus}\n"
   "#+LATEX_HEADER: \\usepackage{xcolor,titlesec,enumitem,booktabs,longtable,array}\n"
   "#+LATEX_HEADER: \\definecolor{seg}{rgb}{0.55,0.10,0.10}\n"
   "#+LATEX_HEADER: \\setlength{\\parindent}{0pt}\\setlength{\\parskip}{0.6ex}\n"
   "#+LATEX_HEADER: \\titleformat{\\section}{\\large\\bfseries\\color{seg}}{}{0pt}{}\n"
   "#+LATEX_HEADER: \\titleformat{\\subsection}{\\normalsize\\bfseries}{}{0pt}{}\n"
   "#+LATEX_HEADER: \\setlist[description]{font=\\normalfont\\itshape,leftmargin=1.2em,itemsep=2pt}\n"
   "#+LATEX_HEADER: \\setlist{itemsep=1pt,topsep=3pt}\n"
   "#+LATEX_HEADER: \\titleformat{\\subsubsection}{\\normalsize\\bfseries\\itshape}{}{0pt}{}\n"
   "#+LATEX_HEADER: \\titleformat{\\paragraph}[hang]{\\normalsize\\itshape}{}{0pt}{}\n"
   "#+LATEX_HEADER: \\titlespacing*{\\paragraph}{0pt}{1.2ex}{0.4ex}\n"
   "#+LATEX_HEADER: \\titleformat{\\subparagraph}[hang]{\\small\\itshape}{}{0pt}{}\n"
   "#+LATEX_HEADER: \\titlespacing*{\\subparagraph}{1em}{1ex}{0.3ex}\n"
   "#+LATEX_HEADER: \\setcounter{tocdepth}{3}\n")
  "The head of an outline for its PDF — that of the Tantrāloka's
\(lualatex, Libertinus, five levels of headings; the introductions of
the readings have the same).")

(defconst bcat-outline-draft--stamp "CAT_OUTLINE_WRITTEN"
  "The keyword of the stamp on a drafted outline: the md5 of its text
without that line.  An outline that still has its stamp is a draft
nobody worked on (`bcat-outline-draft-untouched-p').")

(defun bcat-outline-draft--stamped (text)
  "TEXT of a drafted outline with its stamp after the #+OPTIONS line."
  (let ((line (format "#+%s: %s\n" bcat-outline-draft--stamp (md5 text nil nil 'utf-8))))
    (if (string-match "^#\\+OPTIONS:.*\n" text)
        (concat (substring text 0 (match-end 0)) line (substring text (match-end 0)))
      (concat line text))))

(defun bcat-outline-draft-untouched-p (text)
  "Non-nil when the outline TEXT is a draft of the tool that nobody
changed since: it has the stamp, and the stamp fits what stands there.
An outline written or corrected by hand is not — a new draft never
overwrites it."
  (let ((re (format "^#\\+%s:[ \t]*\\([0-9a-f]+\\)[ \t]*\n" bcat-outline-draft--stamp)))
    (and (stringp text)
         (string-match re text)
         (let ((stamp (match-string 1 text)))
           (equal stamp (md5 (concat (substring text 0 (match-beginning 0)) (substring text (match-end 0)))
                             nil nil 'utf-8))))))

(defun bcat-outline-draft-render (data info items)
  "The outline of the document INFO as org text, from the answer DATA
\(title, intro, work, nodes) and the sentences ITEMS — in the form
bcat-outline.el reads: the head for the PDF, what it is, the work as a
whole, the nodes, the list „Ort im Ganzen“ for every sentence; stamped
as a draft."
  (let* ((work (alist-get 'work data))
         (idea (bcat-outline-draft--squeeze (bcat-outline-draft--field work 'idea)))
         (groups (alist-get 'groups work))
         (name (plist-get info :title))
         (title (let ((given (bcat-outline-draft--squeeze (bcat-outline-draft--field data 'title))))
                  (if (string-empty-p given) (concat name ": Gliederung") given)))
         (intro (bcat-outline-draft--squeeze (bcat-outline-draft--field data 'intro))))
    (bcat-outline-draft--stamped
     (concat
      "#+TITLE: " name " — Gliederung\n"
      "#+SUBTITLE: " name " · Entwurf des Werkzeugs · Stand " (plist-get info :date) "\n"
      "#+LANGUAGE: de\n"
      "#+OPTIONS: toc:3 num:nil author:nil date:nil ^:nil H:5\n"
      bcat-outline-draft--latex-head
      "\n"
      "Diese Gliederung ist wie ein /sa bcad/ gebaut. Sie soll beim Vorbereiten zeigen, wo der gerade gelesene Satz im Ganzen steht und was zusammengehört. Jeder Knoten nennt die Sätze, die er umfasst, dazu Verse und Blattangaben, wo die Quelle sie hat. Sie ist ein Entwurf des Werkzeugs aus dem Text und den vorhandenen Übersetzungen: Was mit „wohl“ oder „nicht geprüft“ markiert ist, steht noch nicht fest, und auch das Übrige ist zu prüfen.\n\n"
      (if (or (not (string-empty-p idea)) groups)
          (concat "* " name " im Ganzen\n\n"
                  (if (string-empty-p idea) "" (concat idea "\n\n"))
                  (mapconcat (lambda (g)
                               (let ((text (bcat-outline-draft--squeeze (bcat-outline-draft--field g 'text))))
                                 (concat "** " (bcat-outline-draft--squeeze (bcat-outline-draft--field g 'title)) "\n"
                                         (if (string-empty-p text) "" (concat text "\n"))
                                         "\n")))
                             groups ""))
        "")
      "* " title "\n\n"
      (if (string-empty-p intro) "" (concat intro "\n\n"))
      (mapconcat (lambda (node) (bcat-outline-draft--node-text node items 1)) (alist-get 'nodes data) "")
      "* Anhang: Ort im Ganzen je Satz\n"
      "Eine Zeile je Satz, zum schnellen Nachschlagen; aus ihr kommt die Zeile „Ort im Ganzen“ der Satzdateien.\n\n"
      "#+NAME: " bcat-outline--places-name "\n"
      (string-join (bcat-outline-draft--places data items) "\n")
      "\n"))))

(defun bcat-outline-draft--merge (answers frame items)
  "The answers of a draft as one: the nodes of ANSWERS (the parts in
order) one after the other — under the groups of FRAME (the last
request of a draft in parts; for one part its answer itself) where it
gives some: a node goes under the group that has its first sentence —,
with FRAME's title, introduction and view of the whole work."
  (let* ((nodes (apply #'append (mapcar (lambda (a) (copy-sequence (alist-get 'nodes a))) answers)))
         (groups (alist-get 'groups frame))
         (tops
          (if (not groups)
              nodes
            (let ((spans (mapcar (lambda (g) (cons g (bcat-outline-draft--span g items))) groups))
                  (members (make-hash-table :test 'eq))
                  (order '()))
              (dolist (node nodes)
                (let* ((span (bcat-outline-draft--span node items))
                       (group (and span
                                   (car (cl-find-if (lambda (gs)
                                                      (and (cdr gs) (<= (car (cdr gs)) (car span) (cdr (cdr gs)))))
                                                    spans)))))
                  (if group
                      (progn (unless (memq group order) (push group order))
                             (puthash group (append (gethash group members) (list node)) members))
                    (push node order))))
              (mapcar (lambda (top)
                        (if (memq top groups)
                            (append (cl-remove 'nodes top :key #'car) (list (cons 'nodes (gethash top members))))
                          top))
                      (nreverse order))))))
    (list (cons 'title (bcat-outline-draft--field frame 'title))
          (cons 'intro (bcat-outline-draft--field frame 'intro))
          (cons 'work (alist-get 'work frame))
          (cons 'nodes tops))))

;; ----------------------------------------------------------------------------
;; The run
;; ----------------------------------------------------------------------------

(defvar bcat-outline-draft--runs (make-hash-table :test 'equal)
  "Outline file → the draft under way: (:source :info :items :parts
:answers :frame).")

(defvar bcat-outline-draft-done-functions nil
  "Called with (SOURCE FILE) when a draft has been written — FILE the
outline, or the draft beside an outline someone worked on.")

(defun bcat-outline-draft--top-lines (answers items)
  "The outermost nodes of ANSWERS as lines „title (sentences): text“."
  (cl-loop for a in answers
           append (mapcar (lambda (n)
                            (let ((span (bcat-outline-draft--span n items)))
                              (format "%s (%s): %s"
                                      (bcat-outline-draft--field n 'title)
                                      (if span
                                          (bcat-outline-draft--runs
                                           (mapcar (lambda (i) (plist-get i :num))
                                                   (cl-subseq items (car span) (1+ (cdr span)))))
                                        "?")
                                      (bcat-outline-draft--squeeze (bcat-outline-draft--field n 'text)))))
                          (alist-get 'nodes a))))

(defun bcat-outline-draft--target (file)
  "Where a new draft for the outline FILE is written: FILE itself when
there is none yet or it is a draft nobody worked on
\(`bcat-outline-draft-untouched-p'); else <name>-Entwurf.org beside it —
an outline written or corrected by hand is never overwritten."
  (if (or (not (file-exists-p file))
          (bcat-outline-draft-untouched-p
           (with-temp-buffer (insert-file-contents file) (buffer-string))))
      file
    (concat (file-name-sans-extension file) "-Entwurf.org")))

(defun bcat-outline-draft--finish (file)
  "Write the outline of the draft under way for FILE and say what came
of it."
  (let* ((run (gethash file bcat-outline-draft--runs))
         (items (plist-get run :items))
         (answers (reverse (plist-get run :answers)))
         (frame (or (plist-get run :frame) (car answers)))
         (data (bcat-outline-draft--merge answers frame items))
         (text (bcat-outline-draft-render data (plist-get run :info) items))
         (target (bcat-outline-draft--target file))
         (uncovered (bcat-outline-draft--uncovered data items)))
    (remhash file bcat-outline-draft--runs)
    (make-directory (file-name-directory target) t)
    (let ((coding-system-for-write 'utf-8-unix)
          (buf (get-file-buffer target)))
      (if buf
          (with-current-buffer buf
            (let ((inhibit-read-only t))
              (erase-buffer) (insert text) (save-buffer)))
        (with-temp-file target (insert text))))
    (message "Gliederung %s: %s geschrieben (%d Sätze%s)%s — prüfen; C-c u L setzt „Ort im Ganzen“ in den Satzdateien, C-c u H das PDF"
             (plist-get (plist-get run :info) :title)
             (file-name-nondirectory target)
             (length items)
             (if uncovered
                 (format ", nicht eingeordnet: %s" (mapconcat #'number-to-string uncovered ", "))
               "")
             (if (equal target file) ""
               (format " — NEBEN der vorhandenen %s, die von Hand bearbeitet ist" (file-name-nondirectory file))))
    (run-hook-with-args 'bcat-outline-draft-done-functions (plist-get run :source) target)
    target))

(defun bcat-outline-draft--fail (file reason)
  "End the draft under way for FILE: REASON."
  (let ((run (gethash file bcat-outline-draft--runs)))
    (remhash file bcat-outline-draft--runs)
    (message "Gliederung %s: abgebrochen — %s (nichts geschrieben)"
             (plist-get (plist-get run :info) :title) reason)))

(defun bcat-outline-draft--next (file)
  "Send the next request of the draft under way for FILE — the next
part, then (for a draft in parts) the frame —, or finish."
  (let* ((run (gethash file bcat-outline-draft--runs))
         (info (plist-get run :info))
         (items (plist-get run :items))
         (parts (plist-get run :parts))
         (done (length (plist-get run :answers)))
         (land (lambda (store)
                 (lambda (_file response)
                   (let ((obj (bcat-outline-draft--read response))
                         (run (gethash file bcat-outline-draft--runs)))
                     (cond ((not run) nil)
                           ((not obj) (bcat-outline-draft--fail file "Antwort nicht lesbar"))
                           (t (funcall store run obj)
                              (bcat-outline-draft--next file)))))))
         (fail (lambda (_file reason) (bcat-outline-draft--fail file (or reason "kein Ergebnis")))))
    (require 'tibetan-translation-discussion)
    (cond
     ((< done (length parts))
      (message "Gliederung %s: Abruf %d von %d …" (plist-get info :title) (1+ done)
               (+ (length parts) (if (cdr parts) 1 0)))
      (tibetan-discussion--submit
       (format "bcat-outline-%d" (1+ done)) file
       (lambda ()
         (cons (bcat-outline-draft-system-prompt)
               (bcat-outline-draft-user-prompt
                info (nth done parts)
                (and (cdr parts) (1+ done)) (and (cdr parts) (length parts))
                (and (cdr parts)
                     (bcat-outline-draft--top-lines (reverse (plist-get (gethash file bcat-outline-draft--runs) :answers))
                                                    items)))))
       (funcall land (lambda (run obj)
                       (puthash file (plist-put run :answers (cons obj (plist-get run :answers)))
                                bcat-outline-draft--runs)))
       fail))
     ((and (cdr parts) (not (plist-get run :frame)))
      (message "Gliederung %s: Abruf %d von %d (das Ganze) …" (plist-get info :title)
               (1+ (length parts)) (1+ (length parts)))
      (tibetan-discussion--submit
       "bcat-outline-frame" file
       (lambda ()
         (cons (bcat-outline-draft-system-prompt)
               (bcat-outline-draft-frame-prompt
                info (bcat-outline-draft--top-lines (reverse (plist-get run :answers)) items))))
       (funcall land (lambda (run obj)
                       (puthash file (plist-put run :frame obj) bcat-outline-draft--runs)))
       fail))
     (t (bcat-outline-draft--finish file)))))

(defun bcat-outline-draft-source (source)
  "Draft the outline of the document SOURCE with Claude: one request for
a document within `bcat-outline-part-size', else one per part and one
for the whole; the answers are set as an org file in the form
bcat-outline.el reads (`bcat-outline-draft-render').  Written is the
outline of SOURCE (`bcat-outline-file') — or, when that one was worked
on by hand, <name>-Entwurf.org beside it.  Asynchronous; returns the
number of requests, nil when a draft for it is under way."
  (let ((file (bcat-outline-file source)))
    (if (gethash file bcat-outline-draft--runs)
        (progn (message "Für %s läuft schon ein Entwurf" (file-name-nondirectory file)) nil)
      (let* ((items (bcat-outline-draft--items source))
             (parts (bcat-outline-draft--parts items bcat-outline-part-size)))
        (unless items (user-error "%s hat keine Sätze" (file-name-nondirectory source)))
        (puthash file (list :source source :info (bcat-outline-draft--info source)
                            :items items :parts parts :answers nil :frame nil)
                 bcat-outline-draft--runs)
        (bcat-outline-draft--next file)
        (+ (length parts) (if (cdr parts) 1 0))))))

;;;###autoload
(defun bcat-outline-draft ()
  "C-c u S: draft the outline (Gliederung, sa bcad) of the document at
hand with Claude — in a cascade source that source, in a sentence file
its source (Carsten 20.11.: the tool drafts the outline, for a new
work with its preparation, for the ones in work „reverse“ from what is
analysed already).  Asks first, naming the requests.  The result is a
draft to be checked; an outline worked on by hand is not overwritten."
  (interactive)
  (require 'bcat-sentence)
  (let* ((file (or (buffer-file-name) (user-error "Dieser Puffer hat keine Datei")))
         (source (cond ((ignore-errors (bcat-sentence--sentences file)) file)
                       ((ignore-errors (tibetan-sentence--source-file-from-analysis file)))
                       (t (user-error "Keine Kaskaden-Quelle und keine Satzdatei"))))
         (items (bcat-outline-draft--items source))
         (parts (bcat-outline-draft--parts items bcat-outline-part-size))
         (requests (+ (length parts) (if (cdr parts) 1 0)))
         (outline (bcat-outline-file source)))
    (when (y-or-n-p (format "Gliederung für %s entwerfen — %d Sätze, %d Abruf%s%s? "
                            (file-name-nondirectory source) (length items) requests
                            (if (= requests 1) "" "e")
                            (if (equal (bcat-outline-draft--target outline) outline) ""
                              (format "; %s ist von Hand bearbeitet, der Entwurf kommt daneben"
                                      (file-name-nondirectory outline)))))
      (bcat-outline-draft-source source))))

(provide 'bcat-outline-draft)
;;; bcat-outline-draft.el ends here
