;;; bcat-status.el --- What C-c u G and C-c u H did, sentence by sentence  -*- lexical-binding: t -*-

;;; Commentary:
;; §5.74, Carsten 29.10.2026: „So eine Statusanzeige nach C-c u G wäre
;; eigentlich auch ganz nett“ — after C-c u G on a filled sentence
;; nothing changed and only a line in the echo area said why.
;;
;; The buffer `*Buddhist CAT*' (shown at the bottom, not selected; q
;; closes it): every run opens a block — command · label · time — with
;; a line per sentence, and every step writes on that line: the file
;; built, DharmaMitra, Claude (or why the answer was unreadable), the
;; alignment of added references, the PDF, the upload with the document
;; name as placed.  Steps of a file not in the newest block are ignored.

;;; Code:

(require 'cl-lib)
(require 'subr-x)

(defgroup bcat-status nil
  "The status buffer of C-c u G / C-c u H."
  :group 'buddhist-cat)

(defcustom bcat-status-show t
  "When non-nil, a run of C-c u G / C-c u H shows the status buffer."
  :type 'boolean
  :group 'bcat-status)

(defconst bcat-status-buffer-name "*Buddhist CAT*"
  "The status buffer.")

(defconst bcat-status--order '(:file :state :dm :claude :align :pdf :upload)
  "The steps of a sentence line, in the order they are shown.")

(defvar bcat-status--entries (make-hash-table :test 'equal)
  "Sentence file → (MARKER LABEL STEPS) of its line in the newest block.")

(define-derived-mode bcat-status-mode special-mode "Buddhist CAT"
  "The status of C-c u G / C-c u H, one block per run.")

(defun bcat-status--buffer ()
  (or (get-buffer bcat-status-buffer-name)
      (with-current-buffer (get-buffer-create bcat-status-buffer-name)
        (bcat-status-mode)
        (current-buffer))))

(defun bcat-status--line (label steps)
  "The text of a sentence line: LABEL and STEPS in the fixed order."
  (concat "  " label
          (let ((parts (delq nil (mapcar (lambda (k) (plist-get steps k))
                                         bcat-status--order))))
            (if parts (concat "  " (string-join parts " · ")) ""))))

(defun bcat-status-begin (command label items)
  "Open a block for a run of COMMAND (\"C-c u G\") on LABEL; ITEMS
\((FILE . SENTENCE-LABEL) …) get a line each.  Shows the buffer
\(`bcat-status-show') without selecting it."
  (with-current-buffer (bcat-status--buffer)
    (let ((inhibit-read-only t))
      (goto-char (point-max))
      (unless (bobp) (insert "\n"))
      (insert (format "%s · %s · %s\n" command label (format-time-string "%H:%M")))
      (dolist (it items)
        (let ((m (copy-marker (point))))
          (insert (bcat-status--line (cdr it) nil) "\n")
          (puthash (expand-file-name (car it)) (list m (cdr it) nil) bcat-status--entries)))))
  (when (and bcat-status-show (not noninteractive))
    (let ((win (display-buffer (bcat-status--buffer)
                               '((display-buffer-reuse-window display-buffer-at-bottom)
                                 (window-height . 10)))))
      (when win
        (with-selected-window win (goto-char (point-max)) (recenter -1))))))

(defun bcat-status-step (file key text)
  "Write step KEY (one of `bcat-status--order') of FILE's line as TEXT;
a later TEXT for the same step replaces the earlier one.  Nothing for a
file without a line."
  (when-let ((e (and file (gethash (expand-file-name file) bcat-status--entries))))
    (let ((buf (marker-buffer (nth 0 e))))
      (when (buffer-live-p buf)
        (setf (nth 2 e) (plist-put (nth 2 e) key text))
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (save-excursion
              (goto-char (nth 0 e))
              (delete-region (point) (line-end-position))
              (insert (bcat-status--line (nth 1 e) (nth 2 e))))))))))

(provide 'bcat-status)
;;; bcat-status.el ends here
