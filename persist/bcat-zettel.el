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
;;
;; The index is the one of the zettel highlighting (skos_index.py,
;; `tibetan-zettel-highlight-index-file'); without it nothing happens.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'tibetan-zettel-highlight)


(defcustom bcat-zettel-link-concepts t
  "When non-nil, the terms of the concept notes link to their zettels
\(`bcat-zettel-link') wherever a sentence file is rendered."
  :type 'boolean
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

(provide 'bcat-zettel)
;;; bcat-zettel.el ends here
