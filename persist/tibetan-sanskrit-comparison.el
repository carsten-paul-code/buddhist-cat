;;; tibetan-sanskrit-comparison.el --- Sanskrit–Tibetan comparison per cascade sentence -*- lexical-binding: t -*-

;;; Commentary:
;; §5.64 (2026-10-20): in the bilingual reading classes (MSA: Sanskrit
;; kārikā + bhāṣya and the Tibetan translation, two cascades with the
;; same `** Section' titles) Carsten wants, per Tibetan sentence, a
;; comparison of the Tibetan with the Sanskrit — where the translator
;; added, dropped, construed differently or chose another term.  It is
;; a Claude call of its own, stored as `** Sanskrit Comparison' under
;; * Provided Translations of the TIBETAN sentence file (survives the
;; regenerate like the discussion, §5.62), fetched by C-c u G together
;; with Claude/DharmaMitra and read by the discussion paper (C-c u D).
;;
;; Linking: the Tibetan cascade carries
;;   #+TIBETAN_PARALLEL_SOURCE: ../Sanskrit/msa-1-cat.org
;; (relative to the Tibetan source).  A Tibetan sentence is compared
;; with EVERY Sanskrit sentence of the same-titled Section — the
;; sentence-level alignment is not known (prose ≠ prose), so Claude is
;; given the Section and names the corresponding Sanskrit sentence(s)
;; in `## Correspondence'.
;;
;; Mechanics shared with the discussion (`tibetan-translation-
;; discussion'): `--section-body' / `--parse-points' (response
;; parsing), `--write-pt-child' (landing), `--submit' (queue, cache,
;; landing outside the gptel sentinel).

;;; Code:

(require 'cl-lib)
(require 'tibetan-translation-discussion)
(require 'tibetan-cascade)
(require 'tibetan-wylie)

(declare-function tibetan-sentence--filepath "tibetan-sentence-persist")
(declare-function tibetan-sentence--sent-id-from-filename "tibetan-sentence-persist")
(declare-function tibetan-sentence--source-file-from-analysis "tibetan-sentence-persist")
(declare-function tibetan-analysis--read-source-metadata "tibetan-analysis-claude")
(declare-function tibetan-cascade--scope-at-point "tibetan-cascade")

(defconst tibetan-sanskrit-comparison--heading
  tibetan-discussion--comparison-heading
  "The `** ' heading of the comparison under * Provided Translations.")

(defconst tibetan-sanskrit-comparison--labels
  '(("de" . (("Tibetan" . "Tibetisch") ("Sanskrit" . "Sanskrit")
             ("Divergence" . "Abweichung") ("Significance" . "Bedeutung")
             ("Assessment" . "Einschätzung")))
    ("en" . (("Tibetan" . "Tibetan") ("Sanskrit" . "Sanskrit")
             ("Divergence" . "Divergence") ("Significance" . "Significance")
             ("Assessment" . "Assessment"))))
  "Display labels per target language (the response uses the English
keys; the file carries them in the document's language).")

(defun tibetan-sanskrit-comparison--label (key target)
  "KEY rendered for TARGET language."
  (or (cdr (assoc key (cdr (assoc target tibetan-sanskrit-comparison--labels))))
      key))

(defun tibetan-sanskrit-comparison--label-key (label)
  "The canonical (English) key of the display LABEL."
  (or (cl-some (lambda (lang) (car (rassoc label (cdr lang))))
               tibetan-sanskrit-comparison--labels)
      label))

;; ============================================================================
;; Parallel source and inputs
;; ============================================================================

(defun tibetan-cascade-parallel-source (source-file)
  "The parallel-language cascade of SOURCE-FILE — the value of
`#+TIBETAN_PARALLEL_SOURCE:' resolved relative to it — or nil."
  (when (and source-file (file-exists-p source-file))
    (with-temp-buffer
      (insert-file-contents source-file nil 0 8192)
      (goto-char (point-min))
      (when (re-search-forward
             "^#\\+TIBETAN_PARALLEL_SOURCE:[ \t]*\\(.+?\\)[ \t]*$" nil t)
        (expand-file-name (match-string 1)
                          (file-name-directory source-file))))))

(defun tibetan-sanskrit-comparison--sentences (file)
  "The sentence plists (`tibetan-cascade--collect-sentences') of the
cascade FILE, read from disk."
  (when (and file (file-exists-p file))
    (with-temp-buffer
      (insert-file-contents file)
      (org-mode)
      (tibetan-cascade--collect-sentences))))

(defun tibetan-sanskrit-comparison--inputs (file)
  "Everything the comparison of the Tibetan cascade sentence FILE
needs, as a plist: :file :source-file :sent-num :target
:section-label :tibetan (segments joined) :wylie :parallel
\(the Sanskrit source) :sanskrit ((SENT-NUM . TEXT) …) — every
sentence of the same-titled Section of the parallel source, in
order; nil list when there is no parallel source or no such Section."
  (when (and file (file-exists-p file))
    (let* ((src (tibetan-sentence--source-file-from-analysis file))
           (n (tibetan-sentence--sent-id-from-filename file))
           (meta (and src (tibetan-analysis--read-source-metadata src)))
           (sentence (and src n
                          (cl-find n (tibetan-sanskrit-comparison--sentences src)
                                   :key (lambda (s) (plist-get s :sent-num)))))
           (label (plist-get sentence :section-label))
           (parallel (and src (tibetan-cascade-parallel-source src)))
           (segs (plist-get sentence :segs))
           (sanskrit
            (when (and parallel label (file-exists-p parallel))
              (mapcar (lambda (s)
                        (cons (plist-get s :sent-num)
                              (mapconcat (lambda (seg) (string-trim (cdr seg)))
                                         (plist-get s :segs) "\n")))
                      (cl-remove-if-not
                       (lambda (s) (equal (plist-get s :section-label) label))
                       (tibetan-sanskrit-comparison--sentences parallel))))))
      (list :file file :source-file src :sent-num n
            :target (or (plist-get meta :target-lang) "en")
            :section-label label
            :tibetan (and segs (mapconcat (lambda (s) (string-trim (cdr s)))
                                          segs "\n"))
            :wylie (and segs (mapconcat (lambda (s)
                                          (string-trim
                                           (tibetan-to-wylie-fixed (cdr s))))
                                        segs "\n"))
            :parallel parallel
            :sanskrit sanskrit))))

(defun tibetan-sanskrit-comparison--ready-p (inputs)
  "Non-nil when INPUTS has the Tibetan text and Sanskrit of its Section."
  (and inputs (plist-get inputs :tibetan) (plist-get inputs :sanskrit) t))

(defun tibetan-sanskrit-comparison--present-p (file)
  "Non-nil when FILE already has a `** Sanskrit Comparison'."
  (and (assoc tibetan-sanskrit-comparison--heading
              (tibetan-discussion--pt-children file))
       t))

;; ============================================================================
;; Prompts
;; ============================================================================

(defun tibetan-sanskrit-comparison--system-prompt (target)
  "The system prompt — constant per TARGET language (cache prefix)."
  (format
   "You are preparing a university reading seminar that reads a Buddhist
śāstra in the Sanskrit original and in its canonical Tibetan
translation side by side.  For ONE Tibetan sentence or verse you
receive the Tibetan (Unicode and Wylie) and ALL Sanskrit sentences of
the same section of the work (numbered; a section may hold the
kārikā and its commentary, so only part of it corresponds).

First identify the Sanskrit passage the Tibetan renders.  Then compare
the Tibetan with that Sanskrit and single out the divergences that
matter for understanding or translating the text: additions by the
translator (particles, glosses, explicit subjects), omissions, a
different construal (case, compound resolution, syntax, word order
that changes the sense), a different or more specific choice of a
technical term, a different reading of the Sanskrit.  Ignore what is
merely Tibetan grammar doing its ordinary work.  Be concrete: cite the
Tibetan words (Wylie) and the Sanskrit words (IAST).

Write in %s (the seminar language); keep Tibetan in Wylie and Sanskrit
in IAST.

Produce EXACTLY these three sections:

## Correspondence
1-2 sentences: which numbered Sanskrit sentence(s), and which part of
them, the Tibetan renders (e.g. \"the kārikā, sentence 1, pādas a-b\").

## Divergences
2-6 points in text order (fewer when the Tibetan is a close match),
each in this exact shape:
### N. <the Tibetan word(s) in Wylie>
- Tibetan: <the Tibetan phrase in Wylie>
- Sanskrit: <the corresponding Sanskrit in IAST, or \"(no counterpart)\">
- Divergence: <what differs>
- Significance: <what it means for reading or translating the passage>
Keep the line labels Tibetan / Sanskrit / Divergence / Significance in
English as shown.  If there is nothing substantial, write
`[No substantial divergences]' under ## Divergences.

## Assessment
2-3 sentences: how faithful the Tibetan is here and what the
translator's tendencies are in this passage."
   (if (equal target "de") "German" "English")))

(defun tibetan-sanskrit-comparison--build-prompts (inputs)
  "(SYSTEM . USER) for the comparison of INPUTS."
  (cons
   (tibetan-sanskrit-comparison--system-prompt (plist-get inputs :target))
   (concat
    (format "Tibetan sentence %s of the section \"%s\":\n%s\n\nWylie:\n%s\n\n"
            (plist-get inputs :sent-num) (plist-get inputs :section-label)
            (plist-get inputs :tibetan) (plist-get inputs :wylie))
    "Sanskrit of the same section (IAST), numbered sentences:\n"
    (mapconcat (lambda (nt) (format "[%d] %s" (car nt) (cdr nt)))
               (plist-get inputs :sanskrit) "\n")
    "\n\nIdentify the corresponding Sanskrit and produce the three sections now.")))

;; ============================================================================
;; Parse, render, land, read
;; ============================================================================

(defun tibetan-sanskrit-comparison--parse (response)
  "Parse RESPONSE into (:correspondence STR :points (POINT …)
:assessment STR); POINT = (:title STR :items ((LABEL . TEXT) …))."
  (list :correspondence (tibetan-discussion--section-body response "Correspondence")
        :points (tibetan-discussion--parse-points
                 (tibetan-discussion--section-body response "Divergences"))
        :assessment (tibetan-discussion--section-body response "Assessment")))

(defun tibetan-sanskrit-comparison--render (parsed target date)
  "The `** Sanskrit Comparison' subtree for PARSED in TARGET language,
stamped DATE: correspondence, the numbered points, the assessment as
a last `*** ' child."
  (concat
   "** " tibetan-sanskrit-comparison--heading "\n"
   ":PROPERTIES:\n:GENERATED: " date "\n:END:\n"
   (tibetan-discussion--org-safe (or (plist-get parsed :correspondence) "")) "\n\n"
   (cl-loop
    for p in (plist-get parsed :points) for i from 1
    concat (concat
            (format "*** %d. %s\n" i (plist-get p :title))
            (mapconcat
             (lambda (it)
               (format "- %s :: %s"
                       (tibetan-sanskrit-comparison--label (car it) target)
                       (tibetan-discussion--org-safe (cdr it))))
             (plist-get p :items) "\n")
            "\n\n"))
   (let ((a (plist-get parsed :assessment)))
     (if (and a (not (string-empty-p a)))
         (format "*** %s\n%s\n\n"
                 (tibetan-sanskrit-comparison--label "Assessment" target)
                 (tibetan-discussion--org-safe a))
       ""))))

(defun tibetan-sanskrit-comparison--land (file response)
  "Parse RESPONSE and write the comparison into FILE.  t on success."
  (let ((parsed (tibetan-sanskrit-comparison--parse response))
        (inputs (tibetan-sanskrit-comparison--inputs file)))
    (when (and inputs (or (plist-get parsed :points)
                          (plist-get parsed :correspondence)))
      (tibetan-discussion--write-pt-child
       file tibetan-sanskrit-comparison--heading
       (tibetan-sanskrit-comparison--render
        parsed (plist-get inputs :target)
        (format-time-string "%Y-%m-%d"))))))

(defun tibetan-sanskrit-comparison-read (file)
  "The written `** Sanskrit Comparison' of FILE as data, nil when
absent: (:correspondence STR :points ((:title STR :items ((KEY
. TEXT) …)) …) :assessment STR), KEY canonical (English)."
  (let ((body (cdr (assoc tibetan-sanskrit-comparison--heading
                          (tibetan-discussion--pt-children file)))))
    (when body
      (let* ((body (replace-regexp-in-string
                    "\\`[ \t\n]*:PROPERTIES:\\(?:.\\|\n\\)*?:END:[ \t]*\n" ""
                    body))
             (chunks (split-string body "^\\*\\*\\* "))
             (assessment nil)
             (points '()))
        (dolist (chunk (cdr chunks))
          (let* ((lines (split-string chunk "\n"))
                 (title (string-trim (car lines))))
            (if (equal (tibetan-sanskrit-comparison--label-key title) "Assessment")
                (setq assessment (string-trim (mapconcat #'identity (cdr lines) "\n")))
              (let ((items '()))
                (dolist (l (cdr lines))
                  (when (string-match "^- \\(.+?\\) :: \\(.*\\)$" l)
                    (push (cons (tibetan-sanskrit-comparison--label-key
                                 (match-string 1 l))
                                (string-trim (match-string 2 l)))
                          items)))
                (push (list :title (string-trim
                                    (replace-regexp-in-string
                                     "\\`[0-9]+\\.[ \t]*" "" title))
                            :items (nreverse items))
                      points)))))
        (list :correspondence (string-trim (car chunks))
              :points (nreverse points)
              :assessment assessment)))))

;; ============================================================================
;; Firing
;; ============================================================================

(defun tibetan-sanskrit-comparison--request (file &optional force)
  "Queue the comparison request for the Tibetan cascade sentence FILE.
t when queued; nil while one is in flight, when the inputs are
incomplete (no parallel source / Section), or when a comparison
exists and FORCE is nil."
  (unless (tibetan-discussion--inflight-p "comparison" file)
    (when (and (tibetan-sanskrit-comparison--ready-p
                (tibetan-sanskrit-comparison--inputs file))
               (or force (not (tibetan-sanskrit-comparison--present-p file))))
      (tibetan-discussion--submit
       "comparison" file
       (lambda () (tibetan-sanskrit-comparison--build-prompts
                   (tibetan-sanskrit-comparison--inputs file)))
       #'tibetan-sanskrit-comparison--land))))

(defun tibetan-sanskrit-comparison-fetch (source-file sentences folder)
  "Hook for `tibetan-cascade-after-fetch-functions': request the
comparison for every sentence of SENTENCES (walker plists) whose
cascade file in FOLDER exists and has none yet — only when
SOURCE-FILE declares a parallel source.  Returns the number
requested."
  (if (not (tibetan-cascade-parallel-source source-file))
      0
    (let ((n 0))
      (dolist (s sentences n)
        (let ((file (tibetan-sentence--filepath (plist-get s :sent-num)
                                                folder source-file)))
          ;; The gate lives here too (not only in `--request'): a
          ;; sentence whose Section has no Sanskrit counterpart (title,
          ;; colophon) or that already has its comparison is never
          ;; counted as a request.
          (when (and (file-exists-p file)
                     (not (tibetan-sanskrit-comparison--present-p file))
                     (tibetan-sanskrit-comparison--ready-p
                      (tibetan-sanskrit-comparison--inputs file))
                     (tibetan-sanskrit-comparison--request file))
            (cl-incf n)))))))

(add-hook 'tibetan-cascade-after-fetch-functions
          #'tibetan-sanskrit-comparison-fetch)

;;;###autoload
(defun tibetan-sanskrit-comparison-dwim ()
  "(Re)request the Sanskrit comparison for the unit at point: the
sentence at point, or every sentence under the heading at point
\(confirmed above `tibetan-cascade-subtree-confirm-threshold').
Needs `#+TIBETAN_PARALLEL_SOURCE:' in the Tibetan cascade; C-c u G
fetches missing comparisons on its own — this forces a new one."
  (interactive)
  (let* ((scope (tibetan-cascade--scope-at-point))
         (src (plist-get scope :source))
         (folder (file-name-as-directory
                  (expand-file-name "analysis" (file-name-directory src))))
         (ready (cl-remove-if-not
                 (lambda (file)
                   (and (file-exists-p file)
                        (tibetan-sanskrit-comparison--ready-p
                         (tibetan-sanskrit-comparison--inputs file))))
                 (mapcar (lambda (n) (tibetan-sentence--filepath n folder src))
                         (plist-get scope :sent-nums)))))
    (unless (tibetan-cascade-parallel-source src)
      (user-error "%s hat keine #+TIBETAN_PARALLEL_SOURCE: — kein Sanskrit zum Vergleichen"
                  (file-name-nondirectory src)))
    (when (and (> (length ready) tibetan-cascade-subtree-confirm-threshold)
               (not (y-or-n-p (format "%s: %d Sanskrit-Vergleiche bei Claude anfordern? "
                                      (plist-get scope :label) (length ready)))))
      (user-error "Abgebrochen"))
    (dolist (file ready)
      (tibetan-sanskrit-comparison--request file t))
    (message "%s: %d Sanskrit-Vergleich(e) angefordert, %d ohne Satzdatei/Section"
             (plist-get scope :label) (length ready)
             (- (length (plist-get scope :sent-nums)) (length ready)))))

(provide 'tibetan-sanskrit-comparison)
;;; tibetan-sanskrit-comparison.el ends here
