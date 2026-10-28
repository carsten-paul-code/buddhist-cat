;;; bcat-sentence-test.el --- Tests for template-built sentence files  -*- lexical-binding: t; -*-

;;; Commentary:
;; §5.74 step 2 (Carsten 28.10.2026): "Wichtig ist, dass dann auch für
;; C-c u A schon das Template (ohne KI requests) verwendet wird."  A new
;; Tibetan cascade sentence file is rendered from the template:
;; * Text (Wylie), * Segments (dictionary tables), * Sentence (the
;; reference translations under "Translations compared"), user slots.
;; Old renderers never touch such a file — only C-c u G will (step 3).
;; Temp dirs only; no network; no AI call.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((root (expand-file-name ".." (file-name-directory
                                    (or load-file-name buffer-file-name)))))
  (dolist (d '("persist" "core" "analysis" "philology" "data" "config" "doc-prep"))
    (add-to-list 'load-path (expand-file-name d root))))

(require 'org)
(require 'bcat-sentence)
(require 'tibetan-cascade)
(require 'tibetan-sentence-persist)

(defconst bcat-sentence-test--source
  (concat "#+TITLE: bDen pa gnyis la 'jug pa — CAT-Quelle\n"
          "#+TIBETAN_LAYOUT: cascade\n"
          "#+TIBETAN_TARGET_LANG: en\n"
          "#+TIBETAN_SENTENCE_REFS: refs.org\n\n"
          "* Tibetan Text\n** Section Verse 9\n"
          "*** Sentence 9\n:PROPERTIES:\n:VERSE: SDA 9\n:END:\n"
          "**** Segment 38\nབརྗོད་དུ་མེད་པ་བལྟར་མེད་པ། །\n\n"
          "**** Segment 39\nའགྱུར་བ་མེད་པ་འདུས་མ་བྱས། །\n\n")
  "A Tibetan cascade source with one verse (bDen 9, two pādas).")

(defconst bcat-sentence-test--refs
  (concat "#+TITLE: Lindtner 1981, versweise\n\n"
          "** Sentence 9\n*** Lindtner 1981\n"
          "[It is also] inexpressible (avācya) and invisible (adraṣṭavya).\n")
  "Sentence references for sentence 9.")

(defmacro bcat-sentence-test--with-source (&rest body)
  "Temp Tibetan cascade source SRC (+ refs.org) in DIR; FOLDER = its
analysis/.  Templates on, the tool templates from the repository."
  (declare (indent 0))
  `(let* ((dir (make-temp-file "bcat-sent-" t))
          (src (expand-file-name "bden-cat.org" dir))
          (folder (file-name-as-directory (expand-file-name "analysis" dir)))
          (bcat-sentence-templates t)
          (bcat-template-directory
           (expand-file-name "../Templates/analysis"
                             (file-name-directory (locate-library "bcat-sentence-test")))))
     (unwind-protect
         (progn
           (with-temp-file src (insert bcat-sentence-test--source))
           (with-temp-file (expand-file-name "refs.org" dir)
             (insert bcat-sentence-test--refs))
           ,@body)
       (dolist (b (buffer-list))
         (when (and (buffer-file-name b)
                    (string-prefix-p (file-truename dir)
                                     (file-truename (buffer-file-name b))))
           (with-current-buffer b (set-buffer-modified-p nil))
           (kill-buffer b)))
       (delete-directory dir t))))

(defun bcat-sentence-test--create (src)
  "Create sentence 9 of SRC the way C-c u A does; return its text."
  (let ((file (tibetan-cascade--create-file
               9 (tibetan-cascade--segs-for-sentence src 9) src)))
    (with-temp-buffer (insert-file-contents file) (buffer-string))))

(ert-deftest bcat-sentence-create-from-template ()
  "C-c u A on a Tibetan cascade source writes the template form: marker
#+CAT_TEMPLATE: tibetan, * Text with the Wylie per segment (verse
lines), * Segments with a dictionary table per segment, * Sentence with
the reference translation under Translations compared, the three user
slots — and none of the old sections.  No AI request is made."
  (bcat-sentence-test--with-source
    (let ((fired nil))
      (cl-letf (((symbol-function 'tibetan-sentence-claude--request)
                 (lambda (&rest _) (setq fired t))))
        (let ((text (bcat-sentence-test--create src)))
          (should (string-match-p "^#\\+CAT_TEMPLATE: tibetan$" text))
          (should (string-match-p "^#\\+SEGMENTS: 38, 39$" text))
          (should (string-match-p
                   (regexp-quote "#+begin_verse\nbrjod du med pa bltar med pa/ /\n'gyur ba med pa 'dus ma byas/ /\n#+end_verse")
                   text))
          (should (string-match-p "^\\* Segments\n\\*\\* Segment 38\n| " text))
          (should (string-match-p "^\\*\\* Segment 39\n| " text))
          (should (string-match-p "steinert:" text))
          (should (string-match-p
                   (regexp-quote "** Translations compared\n*Lindtner 1981:* [It is also] inexpressible (avācya) and invisible (adraṣṭavya).")
                   text))
          (dolist (h '("* Working Translation" "* My Notes" "* Footnotes"))
            (should (string-match-p (concat "^" (regexp-quote h) "$") text)))
          ;; Nothing of the old layout, no AI-only lines yet.
          (dolist (old '("^\\* Reading" "^\\* Tibetan Analysis" "^\\* Provided Translations"
                         "Claude" "Partikelkarte" "\\[Requesting" "\\[Awaiting"
                         "^\\*Structure:\\*" "^\\*Note:\\*" "^⟦"))
            (should-not (string-match-p old text)))
          (should-not fired))))))

(ert-deftest bcat-sentence-old-layout-where-no-template ()
  "Sanskrit sources (their template comes later) and a switched-off
`bcat-sentence-templates' keep the old layout."
  (bcat-sentence-test--with-source
    (let ((bcat-sentence-templates nil))
      (should-not (string-match-p "CAT_TEMPLATE" (bcat-sentence-test--create src)))))
  (bcat-sentence-test--with-source
    (with-temp-file src
      (insert (replace-regexp-in-string
               "#\\+TIBETAN_LAYOUT: cascade\n"
               "#+TIBETAN_LAYOUT: cascade\n#+SOURCE_LANG: sa\n"
               bcat-sentence-test--source)))
    (should-not (bcat-sentence-template-name src))
    (should-not (string-match-p "CAT_TEMPLATE" (bcat-sentence-test--create src)))))

(ert-deftest bcat-sentence-old-renderers-leave-new-files-alone ()
  "Carsten: changes only through C-c u G.  The regenerator (C-c u R,
after-land re-render, migrations) and the old fire path leave a
template-built file byte-identical; the Provided-Translations
materializer skips it."
  (bcat-sentence-test--with-source
    (let* ((text (bcat-sentence-test--create src))
           (file (tibetan-sentence--filepath 9 folder src))
           (fired nil))
      (should (equal "tibetan" (bcat-sentence-file-p file)))
      (tibetan-cascade--regenerate file 9 (tibetan-cascade--segs-for-sentence src 9) src)
      (tibetan-cascade--regenerate-after-land file)
      (should (plist-get (tibetan-cascade-materialize-provided-translations file) :skipped))
      (cl-letf (((symbol-function 'tibetan-sentence-claude--request)
                 (lambda (&rest _) (setq fired t)))
                ((symbol-function 'tibetan-sentence-claude--claim)
                 (lambda (&rest _) t)))
        (tibetan-cascade--fire-sentence-1
         (list :sent-num 9 :seg-nums '(38 39)) src folder nil))
      (should-not fired)
      (should (equal text (with-temp-buffer (insert-file-contents file)
                                            (buffer-string)))))))

(ert-deftest bcat-sentence-work-folder-template-wins ()
  "The cascade end to end: a tibetan.org in the work folder's
Templates/analysis/ shapes the new file instead of the tool's."
  (bcat-sentence-test--with-source
    (make-directory (expand-file-name "Templates/analysis" dir) t)
    (with-temp-file (expand-file-name "Templates/analysis/tibetan.org" dir)
      (insert "#+CAT_TEMPLATE: tibetan\n* Nur Wylie\n{{#segments}}\n{{wylie}}\n{{/segments}}\n"))
    (should (equal "#+CAT_TEMPLATE: tibetan\n* Nur Wylie\nbrjod du med pa bltar med pa/ /\n'gyur ba med pa 'dus ma byas/ /\n"
                   (bcat-sentence-test--create src)))))

(provide 'bcat-sentence-test)
;;; bcat-sentence-test.el ends here
