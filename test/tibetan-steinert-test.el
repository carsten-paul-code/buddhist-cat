;;; tibetan-steinert-test.el --- Tests for tibetan-steinert.el -*- lexical-binding: t -*-

;;; Commentary:
;; Integration tests for the Steinert SQLite dictionary layer.
;; Skipped gracefully when the DB has not been built (CI without data).

;;; Code:

(require 'ert)

(let ((base-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../core" base-dir)))

(require 'tibetan-wylie)
(require 'tibetan-steinert nil t)

(defun tibetan-steinert-test--ready-p ()
  "Skip marker: t when DB is present and module loaded."
  (and (featurep 'tibetan-steinert)
       (fboundp 'tibetan-steinert-available-p)
       (tibetan-steinert-available-p)))

;; ============================================================================
;; Basic lookup — Tibetan and Wylie keys both work
;; ============================================================================

(ert-deftest tibetan-steinert-wylie-key-sangs-rgyas ()
  "A Wylie key returns multiple entries across sources."
  (skip-unless (tibetan-steinert-test--ready-p))
  (let ((hits (tibetan-steinert-lookup "sangs rgyas" 20)))
    (should hits)
    (should (> (length hits) 3))
    ;; At least one row should come from Mahavyutpatti (Sanskrit-indexed).
    (should (cl-some (lambda (h)
                       (equal (plist-get h :source) "21-Mahavyutpatti-Skt"))
                     hits))))

(ert-deftest tibetan-steinert-tibetan-key-sangs-rgyas ()
  "A Tibetan-script key returns the same sort of entries."
  (skip-unless (tibetan-steinert-test--ready-p))
  (let ((hits (tibetan-steinert-lookup "སངས་རྒྱས" 20)))
    (should hits)
    (should (cl-some (lambda (h)
                       (let ((gl (plist-get h :gloss)))
                         (and gl (string-match-p "buddha" gl))))
                     hits))))

;; ============================================================================
;; Sanskrit surfacing — the main point of the integration
;; ============================================================================

(ert-deftest tibetan-steinert-sanskrit-for-sangs-rgyas ()
  "sangs rgyas → buddha (or buddhaḥ) must appear in the combined Sanskrit."
  (skip-unless (tibetan-steinert-test--ready-p))
  (let ((skt (tibetan-steinert-sanskrit-for "sangs rgyas")))
    (should skt)
    (should (string-match-p "buddha" skt))))

(ert-deftest tibetan-steinert-sanskrit-for-bodhisattva ()
  "byang chub sems dpa' → bodhisattva."
  (skip-unless (tibetan-steinert-test--ready-p))
  (let ((skt (tibetan-steinert-sanskrit-for "byang chub sems dpa'")))
    (should skt)
    (should (string-match-p "bodhisattva" skt))))

(ert-deftest tibetan-steinert-sanskrit-for-bltams ()
  "Milarepa seg-007 biographical verb: bltams → niṣkramaṇa."
  (skip-unless (tibetan-steinert-test--ready-p))
  (let ((skt (tibetan-steinert-sanskrit-for "bltams")))
    (should skt)
    (should (string-match-p "niṣkramaṇa\\|niSkramaNa" skt))))

(ert-deftest tibetan-steinert-sanskrit-for-absent-returns-nil ()
  "Nonsense key returns nil, not an error or an empty string."
  (skip-unless (tibetan-steinert-test--ready-p))
  (should (null (tibetan-steinert-sanskrit-for "xxxzzznoway"))))

;; ============================================================================
;; Limit honoured — max-hits defends against runaway rows
;; ============================================================================

(ert-deftest tibetan-steinert-honours-limit ()
  "The explicit limit caps returned rows."
  (skip-unless (tibetan-steinert-test--ready-p))
  (let ((hits (tibetan-steinert-lookup "sangs rgyas" 3)))
    (should (<= (length hits) 3))))

;; ============================================================================
;; Integration with vocabulary-detailed — :sanskrit should surface
;; ============================================================================

(ert-deftest tibetan-steinert-enriches-vocab-lookup ()
  "Detailed lookup for sangs rgyas should carry :sanskrit after Steinert
enrichment (even if the primary gloss source had no Sanskrit field)."
  (skip-unless (tibetan-steinert-test--ready-p))
  (skip-unless (fboundp 'tibetan-vocab-lookup-detailed))
  (require 'tibetan-vocabulary-detailed)
  ;; Clear cache so previous test runs don't hide regressions.
  (when (fboundp 'tibetan-vocab-clear-cache)
    (tibetan-vocab-clear-cache))
  (let ((entry (tibetan-vocab-lookup-detailed "སངས་རྒྱས")))
    (should entry)
    (let ((skt (plist-get entry :sanskrit)))
      (should skt)
      (should (string-match-p "buddha" skt)))))

(ert-deftest tibetan-steinert-registers-cleanup-on-exit ()
  "The cached SQLite handle is released on Emacs exit: loading the
module registers `tibetan-steinert-close' on `kill-emacs-hook'."
  (should (memq 'tibetan-steinert-close kill-emacs-hook)))

(ert-deftest tibetan-steinert-url-emits-valid-json ()
  "The JSON hash fragment in a Steinert URL is valid JSON — including
when the term contains a double quote or backslash.  Regression:  the
fragment was built with a raw format string, so a quote/backslash in
the term produced malformed JSON."
  (skip-unless (fboundp 'tibetan-steinert-url))
  (let* ((url (tibetan-steinert-url "ab\"c\\d"))
         (frag (and url (substring url (1+ (string-match "#" url)))))
         (json (and frag (decode-coding-string (url-unhex-string frag) 'utf-8)))
         (parsed (and json (ignore-errors (json-parse-string json)))))
    (should url)
    (should parsed)                                     ; parses → valid JSON
    (should (equal (gethash "activeTerm" parsed) "ab\"c\\d"))))

(ert-deftest tibetan-steinert-url-plain-term-roundtrips ()
  "A normal Wylie term round-trips through the URL's JSON fragment."
  (skip-unless (fboundp 'tibetan-steinert-url))
  (let* ((url (tibetan-steinert-url "mnyam med"))
         (frag (substring url (1+ (string-match "#" url))))
         (parsed (json-parse-string
                  (decode-coding-string (url-unhex-string frag) 'utf-8))))
    (should (equal (gethash "activeTerm" parsed) "mnyam med"))
    (should (equal (gethash "lang" parsed) "tib"))))

(ert-deftest tibetan-steinert-url-preserves-wylie-capitals ()
  "B6b (§5.58): Wylie-Großbuchstaben kodieren Sanskrit-Laute
\(rwA, DhA, Sh…) — der URL-Bau downcaste den Term und zerstörte
damit den Suchbegriff für jedes Transliterations-Token.  Der Term
muss unverändert in den JSON-Fragment-Hash."
  (skip-unless (fboundp 'tibetan-steinert-url))
  (let* ((url (tibetan-steinert-url "rwA"))
         (frag (substring url (1+ (string-match "#" url))))
         (parsed (json-parse-string
                  (decode-coding-string (url-unhex-string frag) 'utf-8))))
    (should (equal (gethash "activeTerm" parsed) "rwA"))
    (should (equal (gethash "currentListTerm" parsed) "rwA"))))

;; ----------------------------------------------------------------------------
;; T1 (§5.58): der org-Linktyp `steinert:'
;; ----------------------------------------------------------------------------

(ert-deftest tibetan-steinert-link-type-registered ()
  "T1 (§5.58): `steinert:' ist ein registrierter org-Linktyp —
kurze Links ([[steinert:chags ldan][chags ldan]]) statt
~250-Zeichen-URLs in den Tabellen; die URL entsteht zur Klickzeit."
  (require 'ol)
  (tibetan-steinert-register-link-type)
  (should (eq #'tibetan-steinert-link-follow
              (org-link-get-parameter "steinert" :follow)))
  (should (eq #'tibetan-steinert-link-export
              (org-link-get-parameter "steinert" :export))))

(ert-deftest tibetan-steinert-link-follow-opens-url ()
  "Follow baut die URL zur Klickzeit (verbatim-Term, kein
downcase — B6b) und öffnet sie im Browser."
  (let (opened)
    (cl-letf (((symbol-function 'browse-url)
               (lambda (url &rest _) (setq opened url))))
      (tibetan-steinert-link-follow "rwA"))
    (should opened)
    (should (equal opened (tibetan-steinert-url "rwA")))))

(ert-deftest tibetan-steinert-link-export-emits-real-url ()
  "Export liefert echte URLs (HTML-Anker, LaTeX \\href) — sonst
exportierten die Tabellen tote steinert:-Links."
  (let ((html (tibetan-steinert-link-export "mnyam med" "mnyam med"
                                            'html))
        (latex (tibetan-steinert-link-export "mnyam med" "mnyam med"
                                             'latex))
        (other (tibetan-steinert-link-export "mnyam med" nil 'ascii)))
    (should (string-match-p "\\`<a href=\"https://dictionary\\.christian-steinert\\.de/#" html))
    (should (string-match-p ">mnyam med</a>\\'" html))
    ;; `#' geschützt (28.10., s. --latex-escapes-url).
    (should (string-prefix-p "\\href{https://dictionary.christian-steinert.de/\\#" latex))
    (should (equal other "mnyam med"))))

(ert-deftest tibetan-steinert-link-export-latex-escapes-url ()
  "Bug (Carsten 28.10.): C-c C-e l p auf sent-009-bden.org brach ab
\(„File ended while scanning use of \\TX@get@body“).  Die URL trägt `#'
und viele `%' (`#%7B%22activeTerm…'); roh in \\href ist das nur gut,
solange \\href nicht im Argument eines anderen Befehls steht.  Carstens
Export macht Tabellen ab drei Spalten zu tabularx — das liest seinen
Inhalt vorab als Argument, `%' wird zum Kommentar, frisst die
schließende Klammer, LaTeX läuft bis zum Dateiende.  Wie org es für
die eigenen Links tut (`org-latex--protect-text'): `%' und `#' als
\\% und \\#; hyperref schreibt die echte URL ins PDF."
  (let ((latex (tibetan-steinert-link-export "'gyur ba med pa" "'gyur ba med pa"
                                             'latex)))
    (should (string-prefix-p "\\href{https://dictionary.christian-steinert.de/\\#" latex))
    ;; Kein einziges ungeschütztes % oder #.
    (should-not (string-match-p "\\(?:^\\|[^\\]\\)[%#]" latex))
    ;; Entschützt ist es genau die URL.
    (should (equal (replace-regexp-in-string
                    "\\\\\\([%#]\\)" "\\1"
                    (progn (string-match "\\`\\\\href{\\([^}]*\\)}" latex)
                           (match-string 1 latex)))
                   (tibetan-steinert-url "'gyur ba med pa")))))

(ert-deftest tibetan-steinert-link-export-latex-compiles-in-tabularx ()
  "Der echte Fall aus sent-009: ein exportierter steinert:-Link in einer
tabularx-Zelle übersetzt (nur wo lualatex vorhanden ist)."
  (skip-unless (executable-find "lualatex"))
  (let* ((dir (make-temp-file "steinert-tex-" t))
         (tex (expand-file-name "t.tex" dir)))
    (unwind-protect
        (progn
          (with-temp-file tex
            (insert "\\documentclass{article}\n"
                    "\\usepackage{tabularx}\\usepackage{hyperref}\n"
                    "\\begin{document}\n"
                    "\\begin{tabularx}{\\textwidth}{lXX}\n"
                    (tibetan-steinert-link-export "'gyur ba med pa" "'gyur ba med pa" 'latex)
                    " & "
                    (tibetan-steinert-link-export "'dus" "'dus" 'latex)
                    " & x\\\\\n"
                    "\\end{tabularx}\n\\end{document}\n"))
          (let ((default-directory (file-name-as-directory dir)))
            (should (zerop (call-process "lualatex" nil nil nil
                                         "-interaction=nonstopmode" "-halt-on-error"
                                         "t.tex"))))
          (should (file-exists-p (expand-file-name "t.pdf" dir))))
      (delete-directory dir t))))

(provide 'tibetan-steinert-test)
;;; tibetan-steinert-test.el ends here
