;;; tibetan-word-formation-test.el --- §5.66 word formation row -*- lexical-binding: t -*-

;;; Commentary:
;; Carsten (21.10.2026): „Mein Sanskrit ist noch sehr basic … Für die
;; Zettel haben wir eine Funktion, die Präfix und Wurzel zerlegt.  Das
;; hätte ich gerne in der Tabelle als zusätzliche Zeile (als 2.)."
;; Fixture: the Sanskrit cascade corpus of tibetan-sanskrit-cascade-
;; test (temp dir); Claude never called (request/curated stubbed).

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((base-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../persist" base-dir))
  (add-to-list 'load-path (expand-file-name "../analysis" base-dir))
  (add-to-list 'load-path (expand-file-name "../core" base-dir))
  (add-to-list 'load-path base-dir))

(require 'tibetan-word-formation)
(require 'tibetan-sanskrit-cascade-test)

(defconst tibetan-word-formation-test--response
  "## Word Formation

### Segment 1
- dharmāṇām — √dhṛ-ma
- śūnyatā — śūnya-tā
- svabhāvaḥ — sva · √bhū-a

### Segment 2
- na — —
- svataḥ — sva-tas
- api — —
- parataḥ — para-tas"
  "A canned Claude answer for the fixture's two segments.")

(defun tibetan-word-formation-test--file (src &optional no-wa)
  "Create the fixture's sentence 1 file; land its Word Analysis
unless NO-WA.  Returns the path."
  (let ((file (tibetan-cascade--create-file
               1 '((1 . "dharmāṇāṃ śūnyatā svabhāvaḥ ।")
                   (2 . "na svato nāpi parataḥ ॥"))
               src)))
    (unless no-wa
      (with-current-buffer (tibetan-fresh-file-buffer file)
        (tibetan-cascade--insert-l2-in-analysis
         "Word Analysis" tibetan-sanskrit-cascade-test--wa-org)
        (save-buffer)
        (kill-buffer)))
    file))

(defun tibetan-word-formation-test--read (file)
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(ert-deftest tibetan-word-formation-inputs-and-prompt ()
  "Inputs = the Word Analysis words with lemma and label, per segment;
ready only with a Word Analysis.  The system prompt fixes the compact
notation; the user prompt lists the words and hands over a CURATED
zettel etymology (the zettels' Wortbildung) for consistency."
  (tibetan-sanskrit-cascade-test--with-source
    (let ((file (tibetan-word-formation-test--file src)))
      (cl-letf (((symbol-function 'tibetan-word-formation--curated)
                 (lambda (lemma)
                   (and (equal lemma "śūnyatā")
                        "śūnya (empty, √śū to swell) + -tā (abstract noun)"))))
        (let* ((in (tibetan-word-formation--inputs file))
               (ps (tibetan-word-formation--build-prompts in)))
          (should (tibetan-word-formation--ready-p in))
          (should-not (tibetan-word-formation--present-p file))
          (should (string-match-p "vi-√muc" (car ps)))
          (should (string-match-p " · " (car ps)))
          (should (string-match-p "^### Segment 1$" (cdr ps)))
          (should (string-match-p "^- dharmāṇām — dharma; N\\.GEN\\.PL$" (cdr ps)))
          (should (string-match-p "^### Segment 2$" (cdr ps)))
          (should (string-match-p "śūnya (empty, √śū to swell) \\+ -tā" (cdr ps))))))))

(ert-deftest tibetan-word-formation-not-ready-without-word-analysis ()
  (tibetan-sanskrit-cascade-test--with-source
    (let ((file (tibetan-word-formation-test--file src t)))
      (should-not (tibetan-word-formation--ready-p
                   (tibetan-word-formation--inputs file))))))

(ert-deftest tibetan-word-formation-land-renders-row-2 ()
  "Landing writes `** Word Formation' after the Word Analysis in
* Sanskrit Analysis and re-renders the gloss tables: row 2 = the
formation.  The regenerate keeps exactly one section and the row."
  (tibetan-sanskrit-cascade-test--with-source
    (let ((file (tibetan-word-formation-test--file src)))
      (should (tibetan-word-formation--land file tibetan-word-formation-test--response))
      (should (tibetan-word-formation--present-p file))
      (let ((s (tibetan-word-formation-test--read file)))
        (should (< (string-match "^\\* Sanskrit Analysis$" s)
                   (string-match "^\\*\\* Word Analysis$" s)
                   (string-match "^\\*\\* Word Formation$" s)
                   (string-match "^\\* Provided Translations$" s)))
        (should (string-match-p "^- svabhāvaḥ — sva · √bhū-a$" s))
        (should (string-match-p "^| dharmāṇām +| śūnyatā +| svabhāvaḥ +|$" s))
        (should (string-match-p "^| √dhṛ-ma +| śūnya-tā +| sva · √bhū-a +|$" s)))
      (tibetan-cascade--regenerate
       file 1 '((1 . "dharmāṇāṃ śūnyatā svabhāvaḥ ।") (2 . "na svato nāpi parataḥ ॥")) src)
      (let ((s (tibetan-word-formation-test--read file)))
        (should (= 1 (with-temp-buffer
                       (insert s)
                       (count-matches "^\\*\\* Word Formation$" (point-min) (point-max)))))
        (should (string-match-p "^| √dhṛ-ma +| śūnya-tā +| sva · √bhū-a +|$" s))))))

(ert-deftest tibetan-word-formation-auto-fire-gates ()
  "After a Claude landing (hook): request when the file is Sanskrit,
has a Word Analysis and no Word Formation; never when switched off,
without Word Analysis, or when present.  C-c u G (fetch hook) asks
for every such sentence of the scope; it reports no companion (the
paper does not wait for it)."
  (should (memq #'tibetan-word-formation--maybe-fire
                tibetan-cascade-after-land-functions))
  (should (memq #'tibetan-word-formation-fetch
                tibetan-cascade-after-fetch-functions))
  (tibetan-sanskrit-cascade-test--with-source
    (let ((file (tibetan-word-formation-test--file src))
          (calls '()))
      (cl-letf (((symbol-function 'tibetan-word-formation--request)
                 (lambda (f &optional force) (push (cons f force) calls) t)))
        (tibetan-word-formation--maybe-fire file)
        (should (equal (list (cons file nil)) calls))
        (setq calls nil)
        (let ((tibetan-word-formation-auto nil))
          (tibetan-word-formation--maybe-fire file))
        (should-not calls)
        (should-not (tibetan-word-formation-fetch
                     src (list (list :sent-num 1)) analysis-dir))
        (should (equal (list (cons file nil)) calls))
        (setq calls nil)
        (tibetan-word-formation--land file tibetan-word-formation-test--response)
        (tibetan-word-formation--maybe-fire file)
        (tibetan-word-formation-fetch src (list (list :sent-num 1)) analysis-dir)
        (should-not calls)))))

(provide 'tibetan-word-formation-test)
;;; tibetan-word-formation-test.el ends here
