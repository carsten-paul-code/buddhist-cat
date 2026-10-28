;;; bcat-template.el --- Templates for Buddhist CAT analysis files -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;;; Commentary:
;; §5.74 (Carsten 28.10.2026): sentence files are built from templates —
;; "wir sollten Template basierend arbeiten, gerne auch kaskadierend".
;; A template is an org skeleton (Templates/analysis/<name>.org) with
;; mustache-like placeholders; the structure whole original / segments
;; / sentence lives in the template, the code only supplies field
;; values.  First module with the `bcat-' prefix (§5.73).
;;
;; Syntax:
;;   {{name}}           value of field NAME (may span lines).  A line
;;                      whose placeholders are ALL empty is dropped,
;;                      literal label included ("*Note:* {{note}}"); a
;;                      blank line made redundant by the drop goes too.
;;   {{#name}} … {{/name}}  repeated per item when NAME is a list of
;;                      alists (item fields first, then the outer ones);
;;                      rendered once for any other non-empty value,
;;                      not at all for an empty one.
;;   {{^name}} … {{/name}}  rendered only when NAME is empty.
;;   {{> name}}         inserts template NAME (found like any template).
;;   #+begin_comment … #+end_comment   template documentation, never
;;                      written to the output.
;; Section tags and inclusions stand alone on their line.
;;
;; Lookup (the cascade): <folder of the source>/Templates/analysis/,
;; <folder of the source>/Resources/Templates/analysis/, then
;; `bcat-template-directory' (the tool default).  A class overrides a
;; whole template or a single included part by putting a file of the
;; same name in its folder.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst bcat-template--package-dir
  (file-name-directory
   (directory-file-name
    (file-name-directory (or load-file-name buffer-file-name default-directory))))
  "Root of the Buddhist CAT package (parent of persist/).")

(defcustom bcat-template-directory
  (expand-file-name "Templates/analysis" bcat-template--package-dir)
  "The tool's default analysis templates — last stage of the cascade."
  :type 'directory
  :group 'buddhist-cat)

(defconst bcat-template--max-depth 10
  "How deep {{> name}} inclusions may nest (guards against cycles).")

;; ----------------------------------------------------------------------------
;; Lookup
;; ----------------------------------------------------------------------------

(defun bcat-template-candidates (name &optional source-file)
  "The files the template NAME is looked for in, first match wins:
the source's folder, its Resources, the tool default."
  (let ((file (concat name ".org")))
    (append
     (when source-file
       (let ((dir (file-name-directory (expand-file-name source-file))))
         (list (expand-file-name (concat "Templates/analysis/" file) dir)
               (expand-file-name (concat "Resources/Templates/analysis/" file) dir))))
     (list (expand-file-name file bcat-template-directory)))))

(defun bcat-template-find (name &optional source-file)
  "The file of template NAME for SOURCE-FILE (see the cascade), or nil."
  (cl-find-if #'file-readable-p (bcat-template-candidates name source-file)))

(defun bcat-template--strip-comments (text)
  "TEXT without its #+begin_comment … #+end_comment blocks."
  (let ((case-fold-search t))
    (replace-regexp-in-string
     (concat "^[ \t]*#\\+begin_comment[^\n]*\n"
             "\\(?:.*\n\\)*?"
             "[ \t]*#\\+end_comment[^\n]*\\(?:\n\\|\\'\\)")
     "" text t t)))

(defun bcat-template-read (name &optional source-file depth)
  "The text of template NAME for SOURCE-FILE: comment blocks removed,
{{> part}} inclusions expanded through the same cascade.  Signals a
`user-error' when a template is missing or inclusions nest deeper than
`bcat-template--max-depth' (DEPTH counts the levels)."
  (let ((depth (or depth 0))
        (path (bcat-template-find name source-file)))
    (when (> depth bcat-template--max-depth)
      (user-error "Vorlage %s: Einbindungen zu tief verschachtelt (Zyklus?)" name))
    (unless path
      (user-error "Vorlage %s.org nicht gefunden (gesucht: %s)"
                  name (mapconcat #'abbreviate-file-name
                                  (bcat-template-candidates name source-file) ", ")))
    (let ((text (bcat-template--strip-comments
                 (with-temp-buffer
                   (insert-file-contents path)
                   (buffer-string)))))
      (replace-regexp-in-string
       "^[ \t]*{{>[ \t]*\\([-A-Za-z0-9_]+\\)[ \t]*}}[ \t]*\\(?:\n\\|\\'\\)"
       (lambda (m)
         (let ((part (bcat-template-read (match-string 1 m) source-file (1+ depth))))
           (if (string-suffix-p "\n" part) part (concat part "\n"))))
       text t t))))

;; ----------------------------------------------------------------------------
;; Rendering
;; ----------------------------------------------------------------------------

(defconst bcat-template--placeholder-re "{{\\([-A-Za-z0-9_]+\\)}}"
  "A field placeholder.")

(defconst bcat-template--open-re "\\`[ \t]*{{\\([#^]\\)\\([-A-Za-z0-9_]+\\)}}[ \t]*\\'"
  "A standalone section opening line.")

(defconst bcat-template--close-re "\\`[ \t]*{{/\\([-A-Za-z0-9_]+\\)}}[ \t]*\\'"
  "A standalone section closing line.")

(defun bcat-template--parse (lines closing)
  "Parse LINES into nodes up to the {{/CLOSING}} line (nil = top level).
A node is a template line (string) or (section POS-P NAME CHILDREN).
Returns (NODES . REMAINING-LINES)."
  (let ((nodes '()) (closed nil))
    (while (and lines (not closed))
      (let ((line (pop lines)))
        (cond
         ((string-match bcat-template--open-re line)
          (let* ((pos (equal (match-string 1 line) "#"))
                 (name (match-string 2 line))
                 (res (bcat-template--parse lines name)))
            (push (list 'section pos (intern name) (car res)) nodes)
            (setq lines (cdr res))))
         ((string-match bcat-template--close-re line)
          (unless (equal (match-string 1 line) closing)
            (user-error "Vorlage: {{/%s}} ohne passendes {{#%s}}"
                        (match-string 1 line) (match-string 1 line)))
          (setq closed t))
         ((string-match-p "{{[#^/>]" line)
          (user-error "Vorlage: Abschnittsmarke nicht allein auf ihrer Zeile: %s" line))
         (t (push line nodes)))))
    (when (and closing (not closed))
      (user-error "Vorlage: {{#%s}} wird nicht geschlossen" closing))
    (cons (nreverse nodes) lines)))

(defun bcat-template--value (name stack)
  "Field NAME from the context STACK (innermost alist first)."
  (cl-loop for ctx in stack
           for cell = (assq name ctx)
           when cell return (cdr cell)))

(defun bcat-template--scalar (value)
  "VALUE as the text a placeholder shows."
  (cond ((null value) "")
        ((stringp value) value)
        ((numberp value) (number-to-string value))
        ((eq value t) "")
        (t (format "%s" value))))

(defun bcat-template--contexts (value)
  "The contexts a {{#…}} section renders with for VALUE: one per item
of a list of alists, one (empty) context for any other non-empty value,
none for an empty one."
  (cond ((null value) nil)
        ((and (stringp value) (string-blank-p value)) nil)
        ((and (consp value) (consp (car value)) (symbolp (caar value)))
         (list value))                  ; a single alist
        ((and (consp value) (listp (car value))) value)
        (t (list nil))))

(defun bcat-template--walk (nodes stack emit)
  "Render NODES with the context STACK, calling EMIT with each output
line, or with the symbol `drop' for a line dropped as empty."
  (dolist (node nodes)
    (if (stringp node)
        (if (not (string-match-p bcat-template--placeholder-re node))
            (funcall emit node)
          (let* ((filled nil)
                 (text (replace-regexp-in-string
                        bcat-template--placeholder-re
                        (lambda (m)
                          (let ((v (bcat-template--scalar
                                    (bcat-template--value
                                     (intern (match-string 1 m)) stack))))
                            (unless (string-blank-p v) (setq filled t))
                            v))
                        node t t)))
            (funcall emit (if filled text 'drop))))
      (pcase-let ((`(section ,pos ,name ,children) node))
        (let ((contexts (bcat-template--contexts (bcat-template--value name stack))))
          (if pos
              (dolist (ctx contexts)
                (bcat-template--walk children (cons ctx stack) emit))
            (unless contexts
              (bcat-template--walk children stack emit))))))))

(defun bcat-template-render (template context)
  "TEMPLATE (text, comments and inclusions already resolved — see
`bcat-template-read') filled from the alist CONTEXT.  Pure."
  (let* ((final-newline (string-suffix-p "\n" template))
         (lines (split-string (if final-newline
                                  (substring template 0 -1)
                                template)
                              "\n"))
         (nodes (car (bcat-template--parse lines nil)))
         (out '())                      ; reversed
         (after-drop nil))
    (bcat-template--walk
     nodes (list context)
     (lambda (line)
       (cond
        ((eq line 'drop) (setq after-drop t))
        ;; The blank line after a dropped one is redundant when the line
        ;; before the drop was blank too (or nothing came before).
        ((and after-drop (string-blank-p line)
              (or (null out) (string-blank-p (car out))))
         (setq after-drop nil))
        (t (push line out) (setq after-drop nil)))))
    ;; A drop at the very end leaves no blank tail behind.
    (when (and after-drop out (string-blank-p (car out)))
      (pop out))
    (if (null out)
        ""
      (concat (mapconcat #'identity (nreverse out) "\n")
              (if final-newline "\n" "")))))

(defun bcat-template-render-named (name context &optional source-file)
  "Render the template NAME (cascade for SOURCE-FILE) with CONTEXT."
  (bcat-template-render (bcat-template-read name source-file) context))

(provide 'bcat-template)
;;; bcat-template.el ends here
