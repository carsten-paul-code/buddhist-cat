;;; tibetan-cat.el --- Old name of Buddhist CAT -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;;; Commentary:
;; The tool was renamed from Tibetan CAT to Buddhist CAT on 28.10.2026
;; (§5.73).  Init files that still say (require 'tibetan-cat) or
;; (load ".../tibetan-cat.el") load the same system through this file.
;; New configuration: (require 'buddhist-cat).

;;; Code:

;; Loaded by path — (load-file ".../tibetan-cat.el") — the package
;; directory need not be on `load-path' yet: take buddhist-cat from
;; right next to this file (it sets up `load-path' itself).
(unless (featurep 'buddhist-cat)
  (load (expand-file-name "buddhist-cat"
                          (file-name-directory (or load-file-name buffer-file-name)))
        nil t))

(provide 'tibetan-cat)
;;; tibetan-cat.el ends here
