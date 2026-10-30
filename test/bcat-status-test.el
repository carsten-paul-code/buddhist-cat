;;; bcat-status-test.el --- Tests for the C-c u G status buffer  -*- lexical-binding: t; -*-

;;; Commentary:
;; Carsten 29.10.2026: „So eine Statusanzeige nach C-c u G wäre
;; eigentlich auch ganz nett.“  One block per run, one line per
;; sentence, written on by every step.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((root (expand-file-name ".." (file-name-directory
                                    (or load-file-name buffer-file-name)))))
  (add-to-list 'load-path (expand-file-name "persist" root)))

(require 'bcat-status)

(defmacro bcat-status-test--fresh (&rest body)
  "BODY with an empty status buffer and registry, never displayed."
  (declare (indent 0))
  `(let ((bcat-status-show nil)
         (bcat-status--entries (make-hash-table :test 'equal)))
     (when (get-buffer bcat-status-buffer-name)
       (kill-buffer bcat-status-buffer-name))
     (unwind-protect (progn ,@body)
       (when (get-buffer bcat-status-buffer-name)
         (kill-buffer bcat-status-buffer-name)))))

(defun bcat-status-test--text ()
  (with-current-buffer bcat-status-buffer-name (buffer-string)))

(ert-deftest bcat-status-lines ()
  "A run opens a block (command · label · time) with a line per
sentence; each step writes on the sentence's line — a later value of
the same step replaces the earlier one, the order of the steps is
fixed; a file not in the block is ignored."
  (bcat-status-test--fresh
    (bcat-status-begin "C-c u G" "Satz 1007–1008"
                       '(("/a/sent-1007.org" . "Satz 1007") ("/a/sent-1008.org" . "Satz 1008")))
    (bcat-status-step "/a/sent-1007.org" :claude "Claude angefragt")
    (bcat-status-step "/a/sent-1007.org" :dm "DharmaMitra ✓")
    (bcat-status-step "/a/sent-1007.org" :file "neu gebaut")
    (bcat-status-step "/a/sent-1007.org" :claude "Claude ✓")
    (bcat-status-step "/a/sent-1008.org" :state "vollständig — nichts abgerufen")
    (bcat-status-step "/a/other.org" :claude "Claude ✓")
    (let ((text (bcat-status-test--text)))
      (should (string-match-p "\\`C-c u G · Satz 1007–1008 · [0-9][0-9]:[0-9][0-9]\n" text))
      (should (string-match-p "^  Satz 1007  neu gebaut · DharmaMitra ✓ · Claude ✓$" text))
      (should (string-match-p "^  Satz 1008  vollständig — nichts abgerufen$" text))
      (should-not (string-match-p "other" text)))))

(ert-deftest bcat-status-runs-append ()
  "A second run appends its own block; the steps of a file go to its
line in the newest block."
  (bcat-status-test--fresh
    (bcat-status-begin "C-c u G" "Satz 9" '(("/a/sent-009.org" . "Satz 9")))
    (bcat-status-step "/a/sent-009.org" :claude "Claude ✓")
    (bcat-status-begin "C-c u H" "Satz 9" '(("/a/sent-009.org" . "Satz 9")))
    (bcat-status-step "/a/sent-009.org" :pdf "PDF ✓")
    (let ((text (bcat-status-test--text)))
      (should (string-match-p "C-c u G · Satz 9 · [0-9:]+\n  Satz 9  Claude ✓\n\nC-c u H · Satz 9 · [0-9:]+\n  Satz 9  PDF ✓\n?\\'"
                              text)))))

(ert-deftest bcat-status-add-line ()
  "A file that joins a run (the commentary of a verse, 30.10.) gets a
line of its own right below the line it belongs to; its steps go
there, the lines after it keep theirs."
  (bcat-status-test--fresh
    (bcat-status-begin "C-c u G" "§60"
                       '(("/a/sent-128.org" . "Satz 128") ("/a/sent-129.org" . "Satz 129")))
    (bcat-status-add "/a/comm-IX.60.org" "Sthiramati zu MSA IX.60" "/a/sent-128.org")
    (bcat-status-step "/a/sent-129.org" :claude "Claude ✓")
    (bcat-status-step "/a/comm-IX.60.org" :pdf "PDF: Fehler — link")
    (bcat-status-step "/a/sent-128.org" :dm "DharmaMitra ✓")
    (should (string-match-p (concat "^  Satz 128  DharmaMitra ✓\n"
                                    "  Sthiramati zu MSA IX.60  PDF: Fehler — link\n"
                                    "  Satz 129  Claude ✓$")
                            (bcat-status-test--text)))
    ;; Without the line it belongs to: nothing.
    (bcat-status-add "/a/comm-x.org" "X" "/a/other.org")
    (should-not (string-match-p "  X" (bcat-status-test--text)))))

(provide 'bcat-status-test)
;;; bcat-status-test.el ends here
