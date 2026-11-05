;;; bcat-zettel.el --- Concept notes and the Zettelkasten -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Carsten 5.11.2026: „concept Notes sollten doch eigentlich immer auch in
;; Zetteln repräsentiert werden, oder?“  Of the seven terms of TĀ 1.1's
;; concept notes five had a zettel in the Trika hub — and not one link;
;; the notes were a second, unchecked glossary beside the Kasten.
;;
;;   Linking   a term of a concept note (in bold) that has a zettel links
;;             to it: *[[denote:ID][kalā]]* — the Kasten's own links; RET
;;             leads to the Research instance.  Done where a sentence file
;;             is rendered (C-c u G, C-c u L), so every template has it.
;;   Belege    the passage goes into each linked zettel as a marked line
;;             under * Belegstellen … (Carsten: automatically, „ja,
;;             bitte“): the place, the word of the passage and what it
;;             means there, „(CAT: link)“ — the mark of the tool.  A line
;;             by hand for the same verse wins; again, nothing is doubled.
;;             After C-c u G, on demand C-c u z b.
;;
;; The index is the one of the zettel highlighting (skos_index.py,
;; `tibetan-zettel-highlight-index-file'); without it nothing happens.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'tibetan-zettel-highlight)

(declare-function bcat-relayout-file-context "bcat-relayout" (file))
(declare-function bcat-export-info "bcat-export" (file))
(declare-function bcat-analysis--table-columns "bcat-analysis" (table))
(declare-function bcat-analysis-plain-cell "bcat-analysis" (cell))
(declare-function bcat-sentence-file-p "bcat-sentence" (file))
(declare-function bcat-sentence-owner "bcat-sentence" (n source))
(declare-function bcat-sentence-partner-file "bcat-sentence" (sent-num source))
(declare-function bcat-sentence--paired-p "bcat-sentence" (source))
(declare-function tibetan-cascade--scope-at-point "tibetan-cascade" ())
(declare-function tibetan-sentence--filepath "tibetan-sentence-persist" (sent-num &optional folder source-file))
(declare-function tibetan-discussion-paper--temp-source-p "tibetan-discussion-paper" (file))

(defcustom bcat-zettel-link-concepts t
  "When non-nil, the terms of the concept notes link to their zettels
\(`bcat-zettel-link') wherever a sentence file is rendered."
  :type 'boolean
  :group 'buddhist-cat)

(defcustom bcat-zettel-belege-auto t
  "When non-nil, C-c u G ends with the Belege: the passage of every
linked concept goes into its zettel (`bcat-zettel-belege-file')."
  :type 'boolean
  :group 'buddhist-cat)

(defcustom bcat-zettel-belege-heading "Belegstellen in der Literatur"
  "The heading (level 1) of the zettel the Belege go under — the one of
the Termzettel (/term); created before * Verweise when missing."
  :type 'string
  :group 'buddhist-cat)

;; ----------------------------------------------------------------------------
;; Linking
;; ----------------------------------------------------------------------------

(defun bcat-zettel--index-p ()
  "Non-nil when the zettel index exists and is loaded — never builds it
\(a test, or a machine without the Kasten, links nothing)."
  (and (file-exists-p tibetan-zettel-highlight-index-file)
       (tibetan-zettel-highlight--ensure-index)))

(defun bcat-zettel--candidates (term)
  "The zettel ids for TERM as written (index key of three letters or
more), or nil."
  (and (>= (length (tibetan-zettel-highlight--norm-key term)) 3)
       (tibetan-zettel-highlight--ids-for-term term)))

(defun bcat-zettel--traditions (id)
  "The traditions of zettel ID (its tags tradition_…)."
  (cl-remove-if-not (lambda (tag) (string-prefix-p "tradition_" tag))
                    (append (plist-get (tibetan-zettel-highlight-entry id) :tags) nil)))

(defun bcat-zettel--tradition (terms)
  "The tradition most of TERMS (with a zettel) share — more than half of
them —, or nil."
  (let ((counts (make-hash-table :test 'equal)) (n 0) (best nil) (top 0))
    (dolist (term (delete-dups (mapcar #'tibetan-zettel-highlight--norm-key terms)))
      (let ((ids (bcat-zettel--candidates term)))
        (when ids
          (cl-incf n)
          (dolist (tr (delete-dups (apply #'append (mapcar #'bcat-zettel--traditions ids))))
            (puthash tr (1+ (gethash tr counts 0)) counts)))))
    (maphash (lambda (k v) (when (> v top) (setq best k top v))) counts)
    (and best (> (* 2 top) n) best)))

(defun bcat-zettel--id-for (term &optional tradition)
  "The zettel id for TERM as written, or nil: of several the first that
is no stub.  With TRADITION (the text's, `bcat-zettel--tradition') only
zettels of that tradition — 5.11., TĀ 1.1: jñāna, a Śaiva power, went
to the only zettel of that name, Kramer's epistemology."
  (let* ((ids (bcat-zettel--candidates term))
         (ids (if tradition
                  (cl-remove-if-not (lambda (id) (member tradition (bcat-zettel--traditions id))) ids)
                ids)))
    (or (cl-find-if-not (lambda (id) (plist-get (tibetan-zettel-highlight-entry id) :stub)) ids)
        (car ids))))

(defconst bcat-zettel--bold-re
  "\\(^\\|[[:space:](\"'„“‚/]\\)\\*\\([^*\n[]+?\\)\\*\\([[:space:].,;:!?)\"'“”‘’—–-]\\|$\\)"
  "A term in bold (org emphasis) not yet linked.")

(defun bcat-zettel--unlink (text)
  "TEXT with the tool's links in bold back to the bare term:
*[[denote:ID][term]]* → *term* (linking anew decides again)."
  (replace-regexp-in-string "\\*\\[\\[denote:[^]\n]+\\]\\[\\([^]\n]+\\)\\]\\]\\*" "*\\1*" text t))

(defun bcat-zettel--bold-terms (text)
  "The terms in bold of TEXT (not linked), in order."
  (let ((start 0) (out '()))
    (while (string-match bcat-zettel--bold-re text start)
      (push (match-string 2 text) out)
      (setq start (match-beginning 3)))
    (nreverse out)))

(defun bcat-zettel-link (text &optional tradition)
  "TEXT with every term in bold that has a zettel linked to it:
*[[denote:ID][term]]* — with TRADITION only to zettels of it
\(`bcat-zettel--id-for').  The tool's links are decided anew (a term
linked before is unlinked first), so linking again changes nothing.
Unchanged without index or when `bcat-zettel-link-concepts' is nil."
  (if (not (and bcat-zettel-link-concepts (stringp text) (bcat-zettel--index-p)))
      text
    (let ((text (bcat-zettel--unlink text)) (start 0) (out '()))
      (while (string-match bcat-zettel--bold-re text start)
        ;; the positions first: the lookup matches strings of its own
        (let* ((lead (match-end 1))
               (next (match-beginning 3))
               (term (match-string 2 text))
               (id (bcat-zettel--id-for term tradition)))
          (push (substring text start lead) out)
          (push (if id (format "*[[denote:%s][%s]]*" id term) (concat "*" term "*")) out)
          (setq start next)))
      (push (substring text start) out)
      (apply #'concat (nreverse out)))))

(defun bcat-zettel-ids (text)
  "The zettel ids TEXT links to (denote:ID), in order, each once."
  (let ((start 0) (ids '()))
    (while (and text (string-match "\\[\\[denote:\\([0-9]\\{8\\}T[0-9]\\{6\\}\\)\\]" text start))
      (cl-pushnew (match-string 1 text) ids :test #'equal)
      (setq start (match-end 0)))
    (nreverse ids)))

(defun bcat-zettel--text-terms (source-file)
  "The terms in bold of the concept notes of every sentence file of
SOURCE-FILE's text (its analysis/ folder) — what decides the text's
tradition."
  (let ((folder (and source-file (expand-file-name "analysis" (file-name-directory source-file)))))
    (when (and folder (file-directory-p folder))
      (apply #'append
             (mapcar (lambda (f)
                       (with-temp-buffer
                         (insert-file-contents f)
                         (goto-char (point-min))
                         (when (re-search-forward "^\\*\\* Concepts[ \t]*\n" nil t)
                           (let* ((beg (point))
                                  (end (if (re-search-forward "^\\*" nil t) (match-beginning 0) (point-max))))
                             (bcat-zettel--bold-terms
                              (bcat-zettel--unlink (buffer-substring-no-properties beg end)))))))
                     (directory-files folder t "\\`sent-[0-9].*\\.org\\'"))))))

(defun bcat-zettel-link-context (context &optional source-file)
  "CONTEXT with the concept notes of its segments linked
\(`bcat-zettel-link'), homonyms decided by the tradition most of the
terms share (`bcat-zettel--tradition') — of this sentence and, with
SOURCE-FILE, of every sentence of the text (5.11.: a Viveka sentence
with only *bhava* had no majority of its own)."
  (if (not (and bcat-zettel-link-concepts (assq 'segments context) (bcat-zettel--index-p)))
      context
    (let* ((notes (cl-remove-if-not (lambda (c) (and (stringp c) (not (string-blank-p c))))
                                    (mapcar (lambda (s) (alist-get 'concepts s))
                                            (alist-get 'segments context))))
           (tradition (bcat-zettel--tradition
                       (append (apply #'append (mapcar (lambda (c) (bcat-zettel--bold-terms (bcat-zettel--unlink c)))
                                                       notes))
                               (bcat-zettel--text-terms source-file)))))
      (cons (cons 'segments
                  (mapcar (lambda (s)
                            (let ((c (alist-get 'concepts s)))
                              (if (and (stringp c) (not (string-blank-p c)))
                                  (cons (cons 'concepts (bcat-zettel-link c tradition)) s)
                                s)))
                          (alist-get 'segments context)))
            context))))

;; ----------------------------------------------------------------------------
;; Mitgeben: the zettels go with the sentence to Claude
;; ----------------------------------------------------------------------------

(defcustom bcat-zettel-in-prompt t
  "When non-nil, C-c u G sends the zettels of the terms that stand in the
sentence (`bcat-zettel-prompt-block'): the concept notes say what a term
means in this passage, its general sense is the zettel's."
  :type 'boolean
  :group 'buddhist-cat)

(defcustom bcat-zettel-prompt-max 12
  "At most this many zettels go with one sentence (the longest terms first)."
  :type 'integer
  :group 'buddhist-cat)

(defun bcat-zettel--filled (v)
  "Non-nil when V is a string not blank."
  (and (stringp v) (not (string-blank-p v))))

(defun bcat-zettel--description (id)
  "The #+description: of zettel ID, or nil."
  (let ((path (tibetan-zettel-highlight--entry-path id)))
    (when (and path (file-readable-p path))
      (with-temp-buffer
        (insert-file-contents path nil 0 4000)
        (goto-char (point-min))
        (when (re-search-forward "^#\\+description:[ \t]*\\(.+\\)$" nil t)
          (string-trim (match-string 1)))))))

(defun bcat-zettel-prompt-block (texts source-file)
  "The block of zettels for a sentence whose segments are TEXTS (as
transmitted, any script), or nil: every zettel (no stub, with a
description) whose index key of five letters or more stands in the text
— of the text's tradition when it has one (SOURCE-FILE's sentences,
`bcat-zettel--tradition') —, at most `bcat-zettel-prompt-max', each
with its labels and description; an instruction how the concept notes
use them."
  (when (and bcat-zettel-in-prompt (bcat-zettel--index-p))
    (let* ((norm (mapcar (lambda (txt) (concat (tibetan-zettel-highlight--norm-latin txt) " "
                                               (tibetan-zettel-highlight--norm-tibetan txt)))
                         texts))
           (tradition (bcat-zettel--tradition (bcat-zettel--text-terms source-file)))
           (hits '()))
      (maphash (lambda (key ids)
                 (when (and (>= (length key) 5)
                            (cl-some (lambda (n) (string-match-p (regexp-quote key) n)) norm))
                   (dolist (id ids)
                     (let ((e (tibetan-zettel-highlight-entry id)))
                       (when (and e (not (plist-get e :stub))
                                  (or (null tradition) (member tradition (bcat-zettel--traditions id)))
                                  (not (assoc id hits)))
                         (push (cons id (length key)) hits))))))
               tibetan-zettel-highlight--index)
      (let* ((hits (seq-take (sort hits (lambda (a b) (> (cdr a) (cdr b)))) bcat-zettel-prompt-max))
             (lines (delq nil
                          (mapcar (lambda (h)
                                    (let* ((e (tibetan-zettel-highlight-entry (car h)))
                                           (desc (bcat-zettel--description (car h)))
                                           (labels (delq nil (list (and (bcat-zettel--filled (plist-get e :sanskrit))
                                                                        (concat "Skt. " (plist-get e :sanskrit)))
                                                                   (and (bcat-zettel--filled (plist-get e :wylie))
                                                                        (concat "Tib. " (plist-get e :wylie)))
                                                                   (and (bcat-zettel--filled (plist-get e :english))
                                                                        (plist-get e :english))))))
                                      (and desc
                                           (format "- %s%s: %s" (plist-get e :title)
                                                   (if labels (format " (%s)" (string-join labels "; ")) "")
                                                   desc))))
                                  hits))))
        (when lines
          (concat "Entries of the reader's Zettelkasten for terms of this sentence (for the field concepts: "
                  "when a note treats one of these terms, take its general sense from the entry and say "
                  "what the term means in THIS passage — do not repeat the entry; do not mention the "
                  "Zettelkasten):\n"
                  (string-join (sort lines #'string<) "\n")))))))

;; ----------------------------------------------------------------------------
;; Belege
;; ----------------------------------------------------------------------------

(defun bcat-zettel--bands (table)
  "The tables of TABLE — the overview of a segment, often several tables
one under the other (blank line) or bands (rule) — each as its own
text, so that a column stays one word with its gloss (5.11., TĀ 1.1
⟦2⟧: read as one, the second table's words hung under the first's)."
  (let ((bands '()) (band '()))
    (dolist (line (append (split-string table "\n") '("")))
      (if (string-match-p "\\`[ \t]*\\(|-\\|\\'\\)" line)
          (when band (push (string-join (nreverse band) "\n") bands) (setq band nil))
        (push line band)))
    (nreverse bands)))

(defun bcat-zettel--passage (table term)
  "(WORD . GLOSS) of the column of TABLE (a segment's overview) whose word
holds TERM, or nil: the word is the first cell — under a cell in
Tibetan script the transcription below it —, the gloss the next cell.
Only the word is searched: a term that turns up in a gloss is no word
of the text."
  (let ((key (tibetan-zettel-highlight--norm-key term)))
    (when (and table (>= (length key) 3))
      (cl-loop for col in (cl-mapcan #'bcat-analysis--table-columns (bcat-zettel--bands table))
               for cells = (mapcar #'bcat-analysis-plain-cell col)
               for w = (if (and (cdr cells) (string-match-p "[\u0F00-\u0FFF]" (car cells))) 1 0)
               for words = (cl-subseq cells 0 (1+ w))
               when (and (nth (1+ w) cells)
                         (cl-some (lambda (c) (string-match-p (regexp-quote key)
                                                              (tibetan-zettel-highlight--norm-key c)))
                                  words))
               return (cons (nth w cells) (nth (1+ w) cells))))))

(defun bcat-zettel--terms (text)
  "((ID . TERM) …) of the links in TEXT, in order."
  (let ((start 0) (out '()))
    (while (string-match "\\[\\[denote:\\([0-9]\\{8\\}T[0-9]\\{6\\}\\)\\]\\[\\([^]]+\\)\\]\\]" text start)
      (push (cons (match-string 1 text) (match-string 2 text)) out)
      (setq start (match-end 0)))
    (nreverse out)))

(defun bcat-zettel--write-line (zettel line label mark file-mark)
  "Put LINE into ZETTEL under `bcat-zettel-belege-heading': replace the
tool's line of the segment MARK (\"⟦1⟧\") from the same sentence
file (FILE-MARK, the start of its link) whatever its label, else —
unless a line by hand begins with LABEL, the same verse — add it after
the last line of the section (made before * Verweise when missing).
Non-nil when ZETTEL changed."
  (let ((before (with-temp-buffer (insert-file-contents zettel) (buffer-string)))
        (case-fold-search nil))
    (with-temp-buffer
      (insert before)
      (goto-char (point-min))
      (cond
       ;; the tool's line of this segment (MARK, „⟦N⟧“) from this file —
       ;; whatever its label was
       ((re-search-forward (concat "^- .* " (regexp-quote mark) "[: ].*"
                                   (regexp-quote file-mark) ".*$")
                           nil t)
        (replace-match line t t))
       ((progn (goto-char (point-min))
               (cl-loop while (re-search-forward (concat "^- " (regexp-quote label) "[: ].*$") nil t)
                        thereis (not (string-match-p "(CAT: " (match-string 0))))))
       (t (bcat-zettel--append line)))
      (let ((after (buffer-string)))
        (unless (equal before after)
          (let ((coding-system-for-write 'utf-8-unix))
            (write-region after nil zettel nil 'silent))
          t)))))

(defun bcat-zettel--append (line)
  "In the current buffer (a zettel): add LINE after the last item of the
Belege section, making the section when missing."
  (goto-char (point-min))
  (if (re-search-forward (concat "^\\* " (regexp-quote bcat-zettel-belege-heading) "[ \t]*$") nil t)
      (let ((end (save-excursion (if (re-search-forward "^\\* " nil t) (match-beginning 0) (point-max)))))
        (forward-line 1)
        (let ((last (point)))
          (while (re-search-forward "^- .*$" end t)
            (setq last (1+ (match-end 0))))
          (goto-char (min last (point-max)))
          (unless (bolp) (insert "\n"))
          (insert line "\n")))
    (goto-char (point-min))
    (if (re-search-forward "^\\* Verweise[ \t]*$" nil t)
        (goto-char (match-beginning 0))
      (goto-char (point-max))
      (unless (bolp) (insert "\n"))
      (insert "\n"))
    (insert "* " bcat-zettel-belege-heading "\n" line "\n\n")))

(defun bcat-zettel-write-belege (file context label link-label)
  "Write the Belege of the sentence FILE (fields CONTEXT, place LABEL —
the verse, else „Satz N“ —, LINK-LABEL the link's text) into the
zettels its concept notes link to: per segment and linked term that
stands in the segment's text the line „- LABEL ⟦N⟧: /word/ — what it
means there (CAT: link)“.  Returns the zettel files changed."
  (let* ((changed '())
         (file-mark (format "(CAT: [[file:%s::" (abbreviate-file-name file)))
         (target (format "[[file:%s::*Concepts][%s]]" (abbreviate-file-name file) link-label)))
    (dolist (s (and (bcat-zettel--index-p) (alist-get 'segments context)))
      (dolist (pair (bcat-zettel--terms (or (alist-get 'concepts s) "")))
        (let* ((path (tibetan-zettel-highlight--entry-path (car pair)))
               (passage (bcat-zettel--passage (alist-get 'gloss-table s) (cdr pair)))
               (place (format "%s ⟦%s⟧" label (alist-get 'num s)))
               (line (and passage
                          (format "- %s: /%s/ — %s (CAT: %s)" place (car passage) (cdr passage) target))))
          ;; Only where the term stands in the text of the segment — a
          ;; term the note merely names is no Beleg (5.11., TĀ 1.1).
          (when (and line path (file-exists-p path) (file-writable-p path)
                     (bcat-zettel--write-line path line label (format "⟦%s⟧" (alist-get 'num s)) file-mark))
            (cl-pushnew path changed :test #'equal)))))
    (nreverse changed)))

(defun bcat-zettel--label (info)
  "The place of a Beleg from INFO (`bcat-export-info'): the verse, else
the work — its title up to the first comma or bracket — and „Satz N“
\(5.11.: „Satz 1003“ alone is in the Abhidharmasamuccaya and in the
Tantrāloka)."
  (or (plist-get info :verse)
      (format "%s Satz %s"
              (string-trim (car (split-string (or (plist-get info :work) "") "[,(]")))
              (plist-get info :sent-num))))

(defun bcat-zettel-belege-file (file)
  "Write the Belege of the sentence FILE (`bcat-zettel-write-belege'),
its fields read back like C-c u L does; returns the zettels changed."
  (require 'bcat-relayout)
  (require 'bcat-export)
  (when (bcat-zettel--index-p)
    (let ((read (bcat-relayout-file-context file)))
      (unless (plist-get read :error)
        (let ((info (bcat-export-info file)))
          (bcat-zettel-write-belege file (bcat-zettel-link-context (plist-get read :context)
                                                                   (plist-get read :source))
                                    (bcat-zettel--label info)
                                    (format "Satz %s" (plist-get info :sent-num))))))))

(defun bcat-zettel-after-fetch (file)
  "Hook for `bcat-sentence-after-fetch-functions': C-c u G ends with the
Belege (when `bcat-zettel-belege-auto').  Never for a FILE under
`temporary-file-directory' — a test fixture.  Never signals."
  (require 'tibetan-discussion-paper)
  (when (and bcat-zettel-belege-auto
             (not (tibetan-discussion-paper--temp-source-p file)))
    (condition-case err
        (bcat-zettel-belege-file file)
      (error (message "Belege in die Zettel (%s): %s"
                      (file-name-nondirectory file) (error-message-string err))))))

(add-hook 'bcat-sentence-after-fetch-functions #'bcat-zettel-after-fetch)

;;;###autoload
(defun bcat-zettel-belege ()
  "C-c u z b: the passages of the concept notes into their zettels — the
sentence file at point, or in a source the files of the sentence at
point or of the sentences under the heading at point."
  (interactive)
  (require 'tibetan-cascade)
  (let* ((file (buffer-file-name))
         (files (if (and file (bcat-sentence-file-p file))
                    (list file)
                  (let* ((scope (tibetan-cascade--scope-at-point))
                         (source (plist-get scope :source))
                         (folder (file-name-as-directory
                                  (expand-file-name "analysis" (file-name-directory source)))))
                    (cl-loop for n in (plist-get scope :sent-nums)
                             for s = (or (bcat-sentence-owner n source) (list :sent-num n))
                             for f = (tibetan-sentence--filepath (plist-get s :sent-num) folder source)
                             for p = (and (bcat-sentence--paired-p source)
                                          (bcat-sentence-partner-file (plist-get s :sent-num) source))
                             append (cl-remove-if-not (lambda (x) (and x (file-exists-p x) (bcat-sentence-file-p x)))
                                                      (list f p))))))
         (changed (delete-dups (apply #'append (mapcar #'bcat-zettel-belege-file files)))))
    (message "C-c u z b: %d Satzdatei(en), %d Zettel geändert%s" (length files) (length changed)
             (if changed (concat ": " (mapconcat #'file-name-base changed ", ")) ""))
    changed))

(provide 'bcat-zettel)
;;; bcat-zettel.el ends here
