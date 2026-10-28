;;; INIT-EXAMPLE.el --- Example init.el configuration for Buddhist CAT

;;; Commentary:
;; Add this to your ~/.emacs.d/init.el to load the Buddhist CAT system
;; Replace the old Tibetan CAT / Buddhist CAT section with this minimal configuration

;;; Code:

;; ============================================================================
;; TIBETAN CAT (Computer-Assisted Translation) SYSTEM
;; ============================================================================

(defun cp/setup-tibetan-cat ()
  "Setup Tibetan Computer-Assisted Translation system."
  (interactive)
  (let ((cat-dir (expand-file-name "~/buddhist-cat/")))
    (when (file-directory-p cat-dir)
      (add-to-list 'load-path cat-dir)
      (condition-case err
          (progn
            ;; Load Buddhist CAT system (all-in-one)
            (require 'buddhist-cat)

            ;; Load glossaries (required for vocabulary lookup)
            (let ((glossary-file (expand-file-name
                                  "~/buddhist-studies/translation-tools/load-comprehensive-glossaries.el")))
              (when (file-exists-p glossary-file)
                (load-file glossary-file)))

            ;; Load DharmaMitra API (optional, for vocabulary fallback)
            (let ((dharmamitra-dir (expand-file-name "~/emacs-pkgs/dharmamitra/")))
              (when (file-directory-p dharmamitra-dir)
                (add-to-list 'load-path dharmamitra-dir)
                (require 'dharmamitra nil t)))

            (message "✓ Buddhist CAT system loaded successfully"))
        (error
         (message "⚠ Buddhist CAT system loading failed: %s" err)
         (message "CAT tools directory: %s" cat-dir))))))

;; Initialize CAT system after packages load
(add-hook 'after-init-hook 'cp/setup-tibetan-cat)

;;; INIT-EXAMPLE.el ends here
