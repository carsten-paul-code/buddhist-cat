;;; tibetan-fetch-test.el --- §5.60: AI only on demand (C-c u G) -*- lexical-binding: t -*-

;;; Commentary:
;; Carsten (12.10.2026): „Ich will nicht gleich alle Verse komplett
;; analysieren" — C-c u A only opens/creates (everywhere, also the
;; Rgyan); Claude + DharmaMitra are fetched only by the explicit
;; `tibetan-fetch' (C-c u G).  Fixtures in temp dirs; every AI entry
;; point is stubbed — no network.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((base-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../persist" base-dir))
  (add-to-list 'load-path (expand-file-name "../core" base-dir))
  (add-to-list 'load-path (expand-file-name "../analysis" base-dir)))

(require 'tibetan-analysis-persist)
(require 'tibetan-auto-analysis)
(require 'tibetan-sentence-persist)
(require 'tibetan-cascade)

;; ----------------------------------------------------------------------------
;; A1 — opening never fires by default
;; ----------------------------------------------------------------------------

(ert-deftest tibetan-fetch-auto-fire-defaults-off ()
  "§5.60: KI nur auf Abruf — beide Schalter stehen per Default auf nil."
  (should (null (default-value 'tibetan-auto-fire-claude-on-create)))
  (should (null (default-value 'tibetan-auto-fire-dm-on-create))))

(defmacro tibetan-fetch-test--with-fire-spies (&rest body)
  "Run BODY with every AI entry point the openers reach stubbed;
CALLS collects the stubbed function names in call order."
  (declare (indent 0))
  `(let ((calls '()))
     (cl-letf (((symbol-function 'tibetan-analysis-generate-content)
                (lambda (text &rest _)
                  (format "** Wylie Transliteration\nW(%s)\n\n** Translation\n[Requesting translation...]\n\n" text)))
               ((symbol-function 'display-buffer-in-side-window)
                (lambda (&rest _) nil))
               ((symbol-function 'tibetan-analysis--request-claude-translation)
                (lambda (&rest _) (push 'claude calls)))
               ((symbol-function 'tibetan-dharmamitra-translation-fire-for-segment)
                (lambda (&rest _) (push 'dm calls)))
               ((symbol-function 'tibetan-analysis--fire-parallel-mode-claude-calls)
                (lambda (&rest _) (push 'parallel calls)))
               ((symbol-function 'tibetan-analysis--fire-sentence-level)
                (lambda (&rest _) (push 'sentence-level calls) t))
               ((symbol-function 'tibetan-sentence--request-claude)
                (lambda (&rest _) (push 'sentence-claude calls))))
       ,@body)))

(defun tibetan-fetch-test--visit (content)
  "Visit a fresh temp source file with CONTENT; return its buffer."
  (let* ((dir (make-temp-file "fetch-open-" t))
         (src (expand-file-name "doc.org" dir)))
    (with-temp-file src (insert content))
    (find-file-noselect src)))

(defun tibetan-fetch-test--cleanup (buf)
  "Kill BUF and delete its directory."
  (let ((dir (file-name-directory (buffer-file-name buf))))
    (with-current-buffer buf (set-buffer-modified-p nil))
    (kill-buffer buf)
    (dolist (b (buffer-list))
      (when (and (buffer-file-name b)
                 (string-prefix-p dir (buffer-file-name b)))
        (with-current-buffer b (set-buffer-modified-p nil))
        (kill-buffer b)))
    (delete-directory dir t)))

(ert-deftest tibetan-fetch-two-file-segment-open-no-fire ()
  "Zwei-Datei-Segment: C-c u A legt an bzw. öffnet — KEIN Claude/DM/
Parallel-Fire, solange die Schalter aus sind (vorher feuerten sowohl
der Anlege- als auch der Wiederöffnungs-Zweig ungeschaltet).  Mit
eingeschalteten Schaltern bleibt das alte Verhalten (Opt-in)."
  (let ((buf (tibetan-fetch-test--visit
              "#+TITLE: T\n\n* Text\n*** Segment 1\nབདག་གིས་ལས་བྱས།\n")))
    (unwind-protect
        (with-current-buffer buf
          (tibetan-fetch-test--with-fire-spies
            (let ((tibetan-auto-fire-claude-on-create nil)
                  (tibetan-auto-fire-dm-on-create nil))
              (goto-char (point-min))
              (re-search-forward "^\\*\\*\\* Segment 1")
              (tibetan-open-segment-analysis)   ; create
              (should (file-exists-p
                       (tibetan-analysis-get-filepath 1 (buffer-file-name))))
              (tibetan-open-segment-analysis)   ; reopen (placeholder)
              (should-not calls))
            (let ((tibetan-auto-fire-claude-on-create t)
                  (tibetan-auto-fire-dm-on-create t))
              (tibetan-open-segment-analysis)
              (should (memq 'claude calls))
              (should (memq 'dm calls)))))
      (tibetan-fetch-test--cleanup buf))))

(ert-deftest tibetan-fetch-two-file-sentence-open-no-fire ()
  "Zwei-Datei-Satz: das Anlegen per C-c u A feuert nicht mehr von
selbst (Dispatcher und Legacy-Request blieben bisher ungeschaltet)."
  (let ((buf (tibetan-fetch-test--visit
              (concat "#+TITLE: T\n\n* Text\n*** Sentence 1\n"
                      "**** Segment 1\nབདག་གིས་ལས་བྱས།\n"
                      "**** Segment 2\nཆོས་ཟབ་མོ་ཡིན།\n"))))
    (unwind-protect
        (with-current-buffer buf
          (tibetan-fetch-test--with-fire-spies
            (let ((tibetan-auto-fire-claude-on-create nil)
                  (tibetan-auto-fire-dm-on-create nil))
              (goto-char (point-min))
              (re-search-forward "^\\*\\*\\* Sentence 1")
              (tibetan-open-segment-analysis)
              (should-not calls))))
      (tibetan-fetch-test--cleanup buf))))

(ert-deftest tibetan-fetch-paragraph-open-no-fire ()
  "Zwei-Datei-Absatz (`** §N' + `*** Tibetisch'): das Anlegen feuert
nicht mehr von selbst."
  (let ((buf (tibetan-fetch-test--visit
              "#+TITLE: T\n\n* Text\n** §5\n*** Tibetisch\nབདག་གིས་ལས་བྱས།\n")))
    (unwind-protect
        (with-current-buffer buf
          (tibetan-fetch-test--with-fire-spies
            (let ((tibetan-auto-fire-claude-on-create nil)
                  (tibetan-auto-fire-dm-on-create nil))
              (goto-char (point-min))
              (re-search-forward "^\\*\\* §5")
              (tibetan-open-segment-analysis)
              (should-not calls))))
      (tibetan-fetch-test--cleanup buf))))

(provide 'tibetan-fetch-test)
;;; tibetan-fetch-test.el ends here
