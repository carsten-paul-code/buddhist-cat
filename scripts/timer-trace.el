;;; timer-trace.el --- Welcher Timer hängt? -*- lexical-binding: t -*-
;;
;; Diagnose für „Emacs hängt bei 100 % CPU, C-g hilft nicht" (26.10.2026):
;; protokolliert Start und Ende jeder Timer-Funktion in ~/timer-trace.log,
;; mit aktuellem Puffer; bei `org-indent-initialize-agent' zusätzlich die
;; Puffer, die der Agent noch bearbeitet, und die Position seines Markers.
;; Die letzte START-Zeile ohne END nennt den Täter.
;;
;; Laden: (load "~/buddhist-cat/scripts/timer-trace.el") — in der Init
;; oder per `emacsclient -e'.  Aus: M-x my/timer-trace-off.

(defvar my/timer-trace-file (expand-file-name "~/timer-trace.log"))

(defun my/timer-trace--clip (s n)
  (if (> (length s) n) (substring s 0 n) s))

(defun my/timer-trace--name (tm)
  (my/timer-trace--clip (format "%S" (timer--function tm)) 160))

(defun my/timer-trace--context (fn)
  "Current buffer, plus the org-indent agent's queue for FN."
  (concat
   (format " [buf=%s]" (buffer-name))
   (when (and (eq fn 'org-indent-initialize-agent)
              (boundp 'org-indent-agentized-buffers))
     (format " agentized=%S"
             (mapcar (lambda (b)
                       (when (buffer-live-p b)
                         (with-current-buffer b
                           (list (buffer-name) (buffer-size)
                                 (and (boundp 'org-indent--initial-marker)
                                      (markerp org-indent--initial-marker)
                                      (marker-position org-indent--initial-marker))))))
                     org-indent-agentized-buffers)))))

(defun my/timer-trace (orig tm)
  (let ((fn (timer--function tm)))
    (if (memq fn '(blink-cursor-timer-function blink-cursor-start blink-cursor-end
                   beacon--dec show-paren-function which-key--update))
        (funcall orig tm)
      (let ((name (my/timer-trace--name tm))
            (ctx (my/timer-trace--context fn)))
        (write-region (format "%s START %s%s\n" (format-time-string "%T") name ctx)
                      nil my/timer-trace-file t 0)
        (unwind-protect (funcall orig tm)
          (write-region (format "%s END   %s\n" (format-time-string "%T") name)
                        nil my/timer-trace-file t 0))))))

;; Backtrace-Fang: `debug-on-quit' an; wird der hängende Timer von außen
;; mit `kill -INT' ×3 (= C-g, beim dritten Mal erzwungen) abgebrochen,
;; öffnet sich der Debugger — und schreibt seinen Backtrace sofort nach
;; ~/backtrace.txt, auch wenn Emacs gleich danach wieder hängt.  Große
;; Argumente werden beim Drucken gekürzt (sonst hängt das Drucken selbst).
(defvar my/backtrace-file (expand-file-name "~/backtrace.txt"))

(defun my/backtrace-dump (&rest _)
  (when (get-buffer "*Backtrace*")
    (with-current-buffer "*Backtrace*"
      (write-region (point-min) (point-max) my/backtrace-file nil 0))))

(defun my/backtrace-setup (orig &rest args)
  (let ((print-length 30) (print-level 4) (backtrace-line-length 400))
    (prog1 (apply orig args)
      (my/backtrace-dump))))

(advice-add 'debugger-setup-buffer :around #'my/backtrace-setup)
(setq debug-on-quit t)

;; Timer-Funktionen laufen mit `inhibit-quit' t (timer-event-handler) —
;; deshalb greift C-g im hängenden Agenten nie.  Für den Verdächtigen
;; heben wir das auf: C-g (ggf. zweimal) bricht ihn ab, `debug-on-quit'
;; öffnet den Debugger, der Backtrace landet in ~/backtrace.txt.
;; Vorher: ein knapper Backtrace nur mit Funktionsnamen (schnell, kann
;; nicht hängen) nach ~/backtrace-names.txt.
(defun my/with-quit (orig &rest args)
  (let ((inhibit-quit nil))
    (apply orig args)))

(advice-add 'org-indent-initialize-agent :around #'my/with-quit)

;; Der org-element-Cache-Sync läuft seinerseits mit `inhibit-quit' t und
;; verschluckt C-g fünfmal (Meldung „Suppressed C-g … Press C-g N more
;; times").  `debug-on-message' öffnet den Debugger genau bei dieser
;; Meldung — also beim ERSTEN C-g, mitten in der Schleife.
(setq debug-on-message "Suppressed .*keyboard-quit\\|Suppressed `C-g'\\|Parsing aborted by user")

(defun my/backtrace-names (&rest _)
  (with-temp-buffer
    (mapbacktrace (lambda (_evald f _args _flags)
                    (insert (if (symbolp f) (symbol-name f)
                              (my/timer-trace--clip (format "%S" f) 120))
                            "\n")))
    (write-region (point-min) (point-max) (expand-file-name "~/backtrace-names.txt") nil 0)))

(advice-add 'debugger-setup-buffer :before #'my/backtrace-names)

(defun my/timer-trace-off ()
  (interactive)
  (advice-remove 'timer-event-handler #'my/timer-trace)
  (advice-remove 'debugger-setup-buffer #'my/backtrace-setup)
  (advice-remove 'debugger-setup-buffer #'my/backtrace-names)
  (advice-remove 'org-indent-initialize-agent #'my/with-quit)
  (setq debug-on-quit nil
        debug-on-message nil)
  (message "timer-trace aus"))

(advice-remove 'timer-event-handler #'my/timer-trace)
(advice-add 'timer-event-handler :around #'my/timer-trace)
(message "timer-trace an → %s" my/timer-trace-file)
