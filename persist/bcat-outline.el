;;; bcat-outline.el --- The outline (Gliederung, sa bcad) of a document -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;;; Commentary:
;; Carsten über die Tantrāloka-Sitzung, 20.11.2026: „gib das mit der
;; Gliederung bitte auch als Pattern mit.  Wenn ein Dokument neu ins CAT
;; kommt, soll die entsprechende Gliederung gleich mit erzeugt werden.
;; Für die bestehenden Dateien soll es es bitte reverse machen.“ — and to
;; this session: „selbst entwerfen …  Es soll generisch und Teil des
;; Workflows sein.“
;;
;; The outline of a document is an org file beside its source, built
;; like a sa bcad (the reference: WS26-27/Tantraloka/Gliederung-TA1.org):
;;
;;   * <Werk> im Ganzen           the idea of the work, its parts in groups
;;   * <Abschnitt>: Gliederung    the part that is read, nested —
;;   ** I. …                        a node: heading, then ONE line with what
;;   Sätze 1009–1025 · KSTS S. 4–7  it covers („Satz N“, „Sätze A–B“, or
;;                                  „V. A–B“ where a verse's number is its
;;   Two to four sentences.         sentence's), then what it says;
;;   - *1009*: …                    one line per sentence of a commentary:
;;                                  what it does, what it explains, how it
;;                                  hangs together with its neighbours
;;   * Anhang: Ort im Ganzen je Satz
;;   #+NAME: ort-im-ganzen
;;   - 1017: TĀ 1.1 › Deutung 1 › Mutter › Beiwort 2 (abhinavasṛṣṭimahā)
;;
;; This file reads such an outline: the nodes, the lines of the
;; sentences, the place of a sentence in the whole — for the line „Ort
;; im Ganzen“ of a sentence file and for what a fetch is told of the
;; structure (the analysis fits the outline; where it contradicts, it
;; says so — nothing is adapted in silence).

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defconst bcat-outline--places-name "ort-im-ganzen"
  "The #+NAME of the list „Ort im Ganzen“ at the end of an outline: one
line per sentence, „- NUM: place“.")

(defun bcat-outline--header (file key)
  "The value of #+KEY: in the head of FILE, or nil."
  (when (and file (file-readable-p file))
    (with-temp-buffer
      (insert-file-contents file nil 0 4096)
      (goto-char (point-min))
      (when (re-search-forward (format "^#\\+%s:[ \t]*\\(.*[^ \t\n]\\)" (regexp-quote key)) nil t)
        (match-string 1)))))

(defun bcat-outline-file (source)
  "The outline of the document SOURCE (whether it exists or not): the
file its #+CAT_OUTLINE names, relative to its folder — several sources
may share one (a pair with the same numbers) —, else
Gliederung-<name of SOURCE>.org beside it.  nil without SOURCE."
  (when source
    (let ((named (bcat-outline--header source "CAT_OUTLINE")))
      (expand-file-name (or named (concat "Gliederung-" (file-name-base source) ".org"))
                        (file-name-directory source)))))

(defun bcat-outline-text (source)
  "The text of the outline of SOURCE (`bcat-outline-file'), or nil when
it has none yet."
  (let ((file (bcat-outline-file source)))
    (and file (file-readable-p file)
         (with-temp-buffer (insert-file-contents file) (buffer-string)))))

(defun bcat-outline--range (line)
  "(FROM . TO) of the sentences the line LINE under a heading names, or
nil when it is no such line: „Satz N“ or „Sätze A–B“; without these „V.
N“ or „V. A–B“ (a verse's number is its sentence's — the Tantrāloka's
outline).  The line is one of items separated by „ · “."
  (let ((case-fold-search nil)
        (number "\\([0-9]+\\)\\(?:[ \t]*[–-][ \t]*\\([0-9]+\\)\\)?"))
    (when (and (not (string-match-p "\\`[ \t]*\\(?:[-*|#]\\|$\\)" line))
               (or (string-match (concat "\\(?:\\`\\| · \\)[ \t]*S\\(?:atz\\|ätze\\)[ \t]+" number) line)
                   (string-match (concat "\\`[ \t]*V\\.[ \t]*" number) line)))
      (let ((from (string-to-number (match-string 1 line))))
        (cons from (if (match-string 2 line) (string-to-number (match-string 2 line)) from))))))

(defun bcat-outline-nodes (text)
  "The nodes of the outline TEXT in order: plists (:level :title :from
:to :text) — a heading that has, as its next line, the sentences it
covers (`bcat-outline--range'); :text what it says, its paragraphs
joined (not its table, not the lines of its sentences).  A heading
without such a line is no node."
  (let ((lines (split-string (or text "") "\n"))
        (nodes '()))
    (while lines
      (let ((line (pop lines)))
        (when (string-match "\\`\\(\\*+\\) \\(.*?\\)[ \t]*\\'" line)
          (let* ((level (length (match-string 1 line)))
                 (title (match-string 2 line))
                 (range (and lines (bcat-outline--range (car lines)))))
            (when range
              (pop lines)
              (let ((said '()) (rest lines))
                ;; its paragraphs, up to the next heading
                (while (and rest (not (string-match-p "\\`\\*+ " (car rest))))
                  (let ((l (car rest)))
                    (cond
                     ;; a list (the sentences) or a table: not what it says —
                     ;; and with its continuation lines
                     ((string-match-p "\\`[ \t]*\\(?:- \\|[0-9]+[.)] \\||\\|#\\+\\)" l)
                      (setq rest (cdr rest))
                      (while (and rest (string-match-p "\\`[ \t]+[^ \t\n]" (car rest))
                                  (not (string-match-p "\\`\\*+ " (car rest))))
                        (setq rest (cdr rest))))
                     (t (unless (string-blank-p l) (push (string-trim l) said))
                        (setq rest (cdr rest))))))
                (push (list :level level :title title :from (car range) :to (cdr range)
                            :text (string-join (nreverse said) " "))
                      nodes)))))))
    (nreverse nodes)))

(defun bcat-outline-entries (text)
  "The lines of the single sentences in the outline TEXT, in order: ((NUM
. TEXT) …) — „- *1009*: …“, a line of several joined into one."
  (let ((pos 0) (out '()) (text (or text "")))
    (while (string-match "^- \\*\\([0-9]+\\)\\*:[ \t]*\\(.*\\(?:\n[ \t]+[^ \t\n].*\\)*\\)" text pos)
      (setq pos (match-end 0))
      (push (cons (string-to-number (match-string 1 text))
                  (string-trim (replace-regexp-in-string "[ \t]*\n[ \t]*" " " (match-string 2 text))))
            out))
    (nreverse out)))

(defun bcat-outline-places (text)
  "The list „Ort im Ganzen“ of the outline TEXT: ((NUM . PLACE) …) — the
lines „- NUM: place“ under #+NAME: ort-im-ganzen.  nil without it."
  (let ((text (or text "")) (out '()))
    (when (string-match (format "^#\\+NAME:[ \t]*%s[ \t]*\n" (regexp-quote bcat-outline--places-name)) text)
      (let ((pos (match-end 0)))
        (while (eq pos (string-match "- \\([0-9]+\\):[ \t]*\\(.*\\)\\(?:\n\\|\\'\\)" text pos))
          (setq pos (match-end 0))
          (push (cons (string-to-number (match-string 1 text)) (string-trim (match-string 2 text))) out))))
    (nreverse out)))

(defun bcat-outline--plain-title (title)
  "TITLE of a node without the range in parentheses at its end („A.
Bindung und Befreiung (V. 22–35)“ → „A. Bindung und Befreiung“)."
  (string-trim (replace-regexp-in-string
                "[ \t]*([ \t]*\\(?:V\\.\\|S\\(?:atz\\|ätze\\)\\)[^()]*)[ \t]*\\'" "" title)))

(defun bcat-outline--covering (text num)
  "The way down the outline TEXT to sentence NUM, outermost node first:
the deepest node whose own sentences include NUM, after the nodes above
it in the headings.  The nodes above need not include NUM themselves —
a commentary stands at its place under the verse it explains, its
sentences numbered apart („I. Einleitung (V. 1–21)“ over the Viveka's
1009–1059).  nil when no node has NUM."
  (let ((best '()) (above '()))
    (dolist (node (bcat-outline-nodes text))
      ;; a node ends the ones of its level and deeper
      (setq above (cl-remove-if (lambda (p) (>= (plist-get p :level) (plist-get node :level))) above))
      (when (and (<= (plist-get node :from) num (plist-get node :to))
                 (> (1+ (length above)) (length best)))
        (setq best (append above (list node))))
      (setq above (append above (list node))))
    best))

(defun bcat-outline-place (text num)
  "The place of sentence NUM in the whole, from the outline TEXT: its
line in the list „Ort im Ganzen“ (`bcat-outline-places'), else the way
down the nodes that cover it — their titles without the range in
parentheses, joined by „ › “.  nil when the outline does not know it."
  (or (cdr (assq num (bcat-outline-places text)))
      (let ((nodes (bcat-outline--covering text num)))
        (and nodes
             (mapconcat (lambda (n) (bcat-outline--plain-title (plist-get n :title))) nodes " › ")))))

(defun bcat-outline-request-block (text num before after)
  "What a fetch for sentence NUM is told of the outline TEXT, or nil
when the outline does not know the sentence (Tantrāloka-Sitzung 20.11.:
the outline goes with the request, the analysis fits the structure —
and says where it contradicts it): its place, the node it stands in
with what that says, the outline's lines for the sentence BEFORE, for
NUM and for the one AFTER (numbers, nil: none), and the request for the
field `outline_note'."
  (let ((place (bcat-outline-place text num)))
    (when place
      (let* ((entries (bcat-outline-entries text))
             (node (car (last (bcat-outline--covering text num))))
             (line (lambda (label n)
                     (let ((e (and n (cdr (assq n entries)))))
                       (and e (format "%s (%d): %s" label n e))))))
        (string-join
         (delq nil
               (list "Outline of the text (Gliederung, sa bcad) — where this sentence stands:"
                     (concat "Place: " place)
                     (and node
                          (concat "Section: " (bcat-outline--plain-title (plist-get node :title))
                                  (if (string-empty-p (plist-get node :text)) ""
                                    (concat " — " (plist-get node :text)))))
                     (funcall line "Before" before)
                     (funcall line "This sentence" num)
                     (funcall line "After" after)
                     (concat "Fit your analysis into this outline, but do not follow it against the text. "
                             "Add to your JSON object the field \"outline_note\": one sentence when your "
                             "analysis contradicts what the outline says of this sentence — another lemma, "
                             "another referent, another source of a quotation, another role (question, gloss, "
                             "proof, digression) —, else \"\". Do not adapt the outline silently.")))
         "\n")))))

(provide 'bcat-outline)
;;; bcat-outline.el ends here
