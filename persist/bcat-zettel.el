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

(defun bcat-zettel--id-for (term)
  "The zettel id for TERM as written, or nil: of several the first that
is no stub."
  (let ((ids (and (>= (length (tibetan-zettel-highlight--norm-key term)) 3)
                  (tibetan-zettel-highlight--ids-for-term term))))
    (or (cl-find-if-not (lambda (id) (plist-get (tibetan-zettel-highlight-entry id) :stub)) ids)
        (car ids))))

(defconst bcat-zettel--bold-re
  "\\(^\\|[[:space:](\"'„“‚/]\\)\\*\\([^*\n[]+?\\)\\*\\([[:space:].,;:!?)\"'“”‘’—–-]\\|$\\)"
  "A term in bold (org emphasis) not yet linked.")

(defun bcat-zettel-link (text)
  "TEXT with every term in bold that has a zettel linked to it:
*[[denote:ID][term]]*.  Unchanged without index or when
`bcat-zettel-link-concepts' is nil; linking again changes nothing."
  (if (not (and bcat-zettel-link-concepts (stringp text) (bcat-zettel--index-p)))
      text
    (let ((start 0) (out '()))
      (while (string-match bcat-zettel--bold-re text start)
        ;; the positions first: the lookup matches strings of its own
        (let* ((lead (match-end 1))
               (next (match-beginning 3))
               (term (match-string 2 text))
               (id (bcat-zettel--id-for term)))
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

(defun bcat-zettel-link-context (context)
  "CONTEXT with the concept notes of its segments linked (`bcat-zettel-link')."
  (if (not (and bcat-zettel-link-concepts (assq 'segments context)))
      context
    (cons (cons 'segments
                (mapcar (lambda (s)
                          (let ((c (alist-get 'concepts s)))
                            (if (and (stringp c) (not (string-blank-p c)))
                                (cons (cons 'concepts (bcat-zettel-link c)) s)
                              s)))
                        (alist-get 'segments context)))
          context)))

;; ----------------------------------------------------------------------------
;; Belege
;; ----------------------------------------------------------------------------

(defun bcat-zettel--passage (table term)
  "(WORD . GLOSS) of the column of TABLE (a segment's overview) whose word
holds TERM, or nil: the word is the first cell — under a cell in
Tibetan script the transcription below it —, the gloss the next cell.
Only the word is searched: a term that turns up in a gloss is no word
of the text."
  (let ((key (tibetan-zettel-highlight--norm-key term)))
    (when (and table (>= (length key) 3))
      (cl-loop for col in (bcat-analysis--table-columns table)
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

(defun bcat-zettel--write-line (zettel line label place file-mark)
  "Put LINE into ZETTEL under `bcat-zettel-belege-heading': replace the
tool's line of PLACE (\"TĀ 1.1 ⟦1⟧\") from the same sentence file
\(FILE-MARK, the start of its link), else — unless a line by hand
begins with LABEL, the same verse — add it after the last line of the
section (made before * Verweise when missing).  Non-nil when ZETTEL
changed."
  (let ((before (with-temp-buffer (insert-file-contents zettel) (buffer-string)))
        (case-fold-search nil))
    (with-temp-buffer
      (insert before)
      (goto-char (point-min))
      (cond
       ((re-search-forward (concat "^- " (regexp-quote place) "[: ].*"
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
                     (bcat-zettel--write-line path line label place file-mark))
            (cl-pushnew path changed :test #'equal)))))
    (nreverse changed)))

(defun bcat-zettel-belege-file (file)
  "Write the Belege of the sentence FILE (`bcat-zettel-write-belege'),
its fields read back like C-c u L does; returns the zettels changed."
  (require 'bcat-relayout)
  (require 'bcat-export)
  (when (bcat-zettel--index-p)
    (let ((read (bcat-relayout-file-context file)))
      (unless (plist-get read :error)
        (let* ((info (bcat-export-info file))
               (label (plist-get info :label)))
          (bcat-zettel-write-belege file (bcat-zettel-link-context (plist-get read :context))
                                    label (format "Satz %s" (plist-get info :sent-num))))))))

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
