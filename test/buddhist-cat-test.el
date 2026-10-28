;;; buddhist-cat-test.el --- Tests for the Buddhist CAT package shell  -*- lexical-binding: t; -*-

;;; Commentary:

;; §5.73 (Carsten 28.10.2026): the tool is renamed from Tibetan CAT to
;; Buddhist CAT — it reads Sanskrit as well as Tibetan.  First step is
;; the shell only: feature `buddhist-cat', customize group
;; `buddhist-cat', menu title; the `tibetan-' symbols stay until their
;; module is rebuilt (new or rebuilt modules use `bcat-').  The old
;; feature name keeps working: init files that `(require 'tibetan-cat)'
;; load the same system.

;;; Code:

(require 'ert)

(let ((root (expand-file-name ".." (file-name-directory
                                    (or load-file-name buffer-file-name)))))
  (add-to-list 'load-path root)
  (dolist (d '("core" "analysis" "persist" "workspace" "philology"
               "doc-prep" "config" "data" "setup"))
    (add-to-list 'load-path (expand-file-name d root))))

(ert-deftest buddhist-cat-feature-and-old-name ()
  "`buddhist-cat' is the package feature; `tibetan-cat' still loads it."
  (should (require 'buddhist-cat nil t))
  (should (featurep 'buddhist-cat))
  (should (require 'tibetan-cat nil t))
  (should (featurep 'tibetan-cat)))

(ert-deftest buddhist-cat-old-file-loads-without-load-path ()
  "Carsten's init says (load-file \"~/tibetan-cat.el/tibetan-cat.el\")
with the package directory NOT on `load-path' — the old file must
find buddhist-cat.el next to itself.  Fresh `emacs -Q' subprocess."
  (let* ((root (expand-file-name ".." (file-name-directory
                                        (or load-file-name buffer-file-name
                                            (locate-library "buddhist-cat-test")))))
         (old (expand-file-name "tibetan-cat.el" root))
         (emacs (expand-file-name invocation-name invocation-directory)))
    (should (zerop (call-process
                    emacs nil nil nil "-Q" "--batch"
                    "--eval" (format "(load-file %S)" old)
                    "--eval" "(kill-emacs (if (and (featurep 'buddhist-cat) (featurep 'tibetan-cat)) 0 3))")))
    ;; The new way (Carsten's init since 28.10.: load-file buddhist-cat.el)
    ;; must ALSO announce the old feature — code that waits on it
    ;; (`with-eval-after-load 'tibetan-cat', `featurep') keeps working.
    (should (zerop (call-process
                    emacs nil nil nil "-Q" "--batch"
                    "--eval" (format "(load-file %S)" (expand-file-name "buddhist-cat.el" root))
                    "--eval" "(kill-emacs (if (and (featurep 'buddhist-cat) (featurep 'tibetan-cat)) 0 3))")))))

(ert-deftest buddhist-cat-customize-group ()
  "The options live in a defined customize group `buddhist-cat' (the
old `tibetan-cat' group was used 57 times but never defined)."
  (require 'buddhist-cat)
  (should (get 'buddhist-cat 'custom-group))
  (should (string-match-p "Buddhist" (get 'buddhist-cat 'group-documentation)))
  ;; A defcustom of the shell (cascade) and one of the analysis layer.
  (require 'tibetan-cascade)
  (should (assq 'tibetan-cascade-chunk-max-segments
                (get 'buddhist-cat 'custom-group))))

(ert-deftest buddhist-cat-menu-title ()
  "The menu bar entry and the context menu are called \"Buddhist CAT\"."
  (require 'tibetan-menu)
  (should (equal "Buddhist CAT" (keymap-prompt tibetan-cat-menu)))
  (should (equal "Buddhist CAT" (keymap-prompt tibetan-cat-context-menu-map))))

(provide 'buddhist-cat-test)
;;; buddhist-cat-test.el ends here
