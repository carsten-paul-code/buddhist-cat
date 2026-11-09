;;; bcat-dossier-test.el --- Tests for the reading dossier of a verse  -*- lexical-binding: t; -*-

;;; Commentary:
;; Carsten 5.11.2026: per verse the layers one under the other — text,
;; bhāṣya, commentary — as an org file (his first screen in class),
;; written from the files that exist; the notes stay.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((root (expand-file-name ".." (file-name-directory
                                    (or load-file-name buffer-file-name)))))
  (dolist (d '("persist" "core" "analysis" "philology" "data" "config" "doc-prep" "test"))
    (add-to-list 'load-path (expand-file-name d root))))

(require 'org)
(require 'bcat-dossier)
(require 'bcat-export)
(require 'bcat-sentence-test)
(require 'bcat-commentary-test)

(defconst bcat-dossier-test--context
  '((segments . (((num . 286) (devanagari . "स्वाभाविको ऽथ") (samhita . "svābhāviko 'tha"))))
    (tibetan-lines . (((line . "རང་བཞིན།") (wylie . ""))))
    (proposal . "*Proposed translation:*\n#+begin_quote\nThe nature body …\n#+end_quote\n\n*Decisions:* Unit 1.")
    (bhasya-p . t)
    (bhasya-sanskrit . (((devanagari . "त्रिविधः कायो") (samhita . "trividhaḥ kāyo"))))
    (bhasya-tibetan . (((uchen . "སྐུ་ནི་རྣམ་པ་གསུམ།") (wylie . "sku ni rnam pa gsum/"))))
    (bhasya-hint . "The bhāṣya names the svābhāvika kāya dharmakāya."))
  "The fields of a verse file as the dossier reads them.")

(ert-deftest bcat-dossier-text-layer ()
  "01: the Sanskrit lines (Devanāgarī over IAST), the Tibetan (Uchen
over Wylie — a missing Wylie made from the Uchen), the translation as
read first — the proposal without its decisions."
  (let ((layer (bcat-dossier-text-layer bcat-dossier-test--context)))
    (should (equal "स्वाभाविको ऽथ\nsvābhāviko 'tha"
                   (alist-get 'text (nth 0 (alist-get 'verses layer)))))
    (should (string-match-p "\\`རང་བཞིན།\nrang bzhin" (alist-get 'text (nth 1 (alist-get 'verses layer)))))
    (should (equal "*Proposed translation:*\n#+begin_quote\nThe nature body …\n#+end_quote"
                   (alist-get 'translation layer)))))

(ert-deftest bcat-dossier-bhasya-layer ()
  "02: the bhāṣya in Sanskrit and Tibetan, the hint; its translation —
until it is fetched, the note that it comes with the next C-c u G."
  (let ((layer (bcat-dossier-bhasya-layer bcat-dossier-test--context)))
    (should (equal "त्रिविधः कायो\ntrividhaḥ kāyo" (alist-get 'text (nth 0 (alist-get 'verses layer)))))
    (should (equal "སྐུ་ནི་རྣམ་པ་གསུམ།\nsku ni rnam pa gsum/" (alist-get 'text (nth 1 (alist-get 'verses layer)))))
    (should (string-match-p "nächsten C-c u G" (alist-get 'translation layer)))
    (should (string-match-p "dharmakāya" (alist-get 'note layer))))
  (let ((layer (bcat-dossier-bhasya-layer
                (cons '(bhasya-translation . "The body of the Buddhas is threefold.")
                      bcat-dossier-test--context))))
    (should (equal "*Translation:* The body of the Buddhas is threefold." (alist-get 'translation layer))))
  (should-not (bcat-dossier-bhasya-layer '((segments)))))

(ert-deftest bcat-dossier-commentary-items ()
  "03: per sentence of the commentary „N · folio“, Uchen over Wylie —
the line of a segment that takes up the verse (*Lemma:*) in bold —, the
translation, the lemma with its links seen from the dossier."
  (let* ((dir (make-temp-file "bcat-dossier-" t))
         (doc (expand-file-name "Sthiramati-9/analysis/comm-IX.60-savbh.org" dir))
         (to (expand-file-name "dossier/" dir))
         (folios (make-hash-table)))
    (unwind-protect
        (progn
          (make-directory (file-name-directory doc) t)
          (with-temp-file doc
            (insert "* Verse\nV\n\n"
                    "* Sentence 755\n#+begin_verse\nརང་བཞིན་དང་ནི་ཞེས་བྱ་ནི།\nrang bzhin dang ni zhes bya ni/\n"
                    "རང་བཞིན་ནི་ཆོས་ཀྱི་སྐུ།\nrang bzhin ni chos kyi sku/\n#+end_verse\n\n"
                    "*Translation:* Regarding „and the nature“: the dharmakāya.\n\n"
                    "** Segment 1783\n| x |\n\n*Lemma:* verse, [[file:../../Tibetisch-9/analysis/sent-183-mdo.org::Segment 460][Tib. 460]]\n\n"
                    "** Segment 1784\n| y |\n\n"))
          (puthash 1783 "D135b6" folios)
          (let ((item (car (bcat-dossier-commentary-items doc folios to))))
            (should (equal "755 · D135b6" (alist-get 'head item)))
            (should (equal (concat "*རང་བཞིན་དང་ནི་ཞེས་བྱ་ནི།*\nrang bzhin dang ni zhes bya ni/\n"
                                   "རང་བཞིན་ནི་ཆོས་ཀྱི་སྐུ།\nrang bzhin ni chos kyi sku/")
                           (alist-get 'lines item)))
            (should (equal "Regarding „and the nature“: the dharmakāya." (alist-get 'item-translation item)))
            (should (string-match-p (regexp-quote "[[file:../Tibetisch-9/analysis/sent-183-mdo.org::Segment 460][Tib. 460]]")
                                    (alist-get 'lemma item)))))
      (delete-directory dir t))))

(ert-deftest bcat-dossier-template ()
  "The page as Carsten approved it (Lesedossier-IX.60-MUSTER.org): the
links to the analyses, the layers as level-1 headings, the commentary's
sentences as level-2 headings, „* Meine Notizen“ at the end."
  (let ((text (bcat-template-render-named
               "dossier"
               `((label . "MSA IX.60")
                 (analysis . "[[file:x.org][Text]]")
                 (my-notes . "alte Notiz")
                 (layers . (((heading . "01 Vers (L · D4026)")
                             (verses . (((text . "स्वाभाविको\nsvābhāviko"))))
                             (translation . "*Translation:* The nature body."))
                            ((heading . "02 Sthiramati (D4034)")
                             (items . (((head . "755 · D135b6") (lines . "*རང་བཞིན།*\nrang bzhin/")
                                        (item-translation . "Regarding the nature.")
                                        (lemma . "verse, Tib. 460")))))))))))
    (should (string-match-p "^#\\+TITLE: MSA IX\\.60 — Lesedossier$" text))
    (should (string-match-p "^MSA IX\\.60 · Analyse: \\[\\[file:x\\.org\\]\\[Text\\]\\]$" text))
    (should (string-match-p "^\\* 01 Vers (L · D4026)\n#\\+begin_verse\nस्वाभाविको\nsvābhāviko\n#\\+end_verse\n\n\\*Translation:\\* The nature body\\.$" text))
    (should (string-match-p "^\\*\\* 755 · D135b6\n#\\+begin_verse\n\\*རང་བཞིན།\\*\nrang bzhin/\n#\\+end_verse\n\n→ Regarding the nature\\.\n/Lemma:/ verse, Tib\\. 460$" text))
    (should (string-match-p "^\\* Meine Notizen\nalte Notiz" text))))

(ert-deftest bcat-dossier-write-keeps-notes ()
  "Written anew, the dossier keeps what Carsten wrote under „* Meine
Notizen“."
  (let* ((dir (make-temp-file "bcat-dossier-" t))
         (path (expand-file-name "dossier/msa-IX.60.org" dir)))
    (unwind-protect
        (cl-letf (((symbol-function 'bcat-dossier-context)
                   (lambda (_f) `((label . "MSA IX.60") (path . ,path) (analysis . "") (layers . nil))))
                  ((symbol-function 'tibetan-sentence--source-file-from-analysis) #'ignore))
          (should (equal path (bcat-dossier-write "/x/sent-128-msa.org")))
          (with-temp-file path
            (insert (replace-regexp-in-string "^\\* Meine Notizen\n" "* Meine Notizen\nIm Unterricht: kāya!\n"
                                              (with-temp-buffer (insert-file-contents path) (buffer-string)))))
          (bcat-dossier-write "/x/sent-128-msa.org")
          (should (string-match-p "^\\* Meine Notizen\nIm Unterricht: kāya!"
                                  (with-temp-buffer (insert-file-contents path) (buffer-string)))))
      (delete-directory dir t))))

(ert-deftest bcat-dossier-asbh ()
  "The Abhidharmasamuccaya: a bhāṣya sentence (tag :asbh:) belongs to
the dossier of the root sentence before it."
  (let* ((dir (make-temp-file "bcat-dossier-" t))
         (src (expand-file-name "asam-cat.org" dir)))
    (unwind-protect
        (progn
          (with-temp-file src
            (insert "#+TIBETAN_LAYOUT: cascade\n#+SOURCE_LANG: sa\n\n* Text\n** Section 1\n"
                    "*** Sentence 2\n**** Segment 17\nkati skandhāḥ\n\n"
                    "*** Sentence 1002                     :asbh:\n**** Segment 40\nkatīti\n\n"
                    "*** Sentence 3\n**** Segment 22\nkasmāt\n"))
          (should (bcat-dossier--asbh-p 1002 src))
          (should-not (bcat-dossier--asbh-p 2 src))
          (should-not (bcat-dossier--asbh-p 3 src)))
      (delete-directory dir t))))

(defun bcat-dossier-test--parts (text)
  "The level-1 headings of TEXT."
  (let ((out '()) (start 0))
    (while (string-match "^\\* \\(.+\\)$" text start)
      (push (match-string 1 text) out)
      (setq start (match-end 0)))
    (nreverse out)))

(defun bcat-dossier-test--links-resolve (text)
  "Non-nil when every [[#ID] link of TEXT has its :CUSTOM_ID: ID."
  (let ((start 0) (ok t))
    (while (string-match "\\[\\[#\\([^]]+\\)\\]" text start)
      (setq start (match-end 0))
      (unless (string-match-p (concat "^:CUSTOM_ID: " (regexp-quote (match-string 1 text)) "$") text)
        (setq ok nil)))
    ok))

(ert-deftest bcat-dossier-pdf-msa ()
  "The PDF of the dossier (Carsten 5.11., step 3): one document — the
reading page (the layers, as in the org file) first, then the analysis
of the verse (Segments, Grammar …), the commentary (each sentence a
jump target, comm-N, reached from its place on the reading page), the
concepts and the discussion; no part twice (the verse file's own
Commentary and Reading out), every jump with its target.  From each
file of the verse: the verse files and the commentary document."
  (bcat-commentary-test--with-msa
    (bcat-sentence-test--with-stubs #'bcat-commentary-test--answers
      (bcat-sentence-test--fetch-n src 36))
    (let* ((file (tibetan-sentence--filepath 36 folder src))
           (doc (expand-file-name "comm-IX.16-savbh.org" cfolder))
           (pdf (bcat-dossier-pdf-text file))
           (text (plist-get pdf :text))
           (parts (bcat-dossier-test--parts text)))
      (should (equal file (plist-get pdf :lead)))
      (should (equal '("Reading" "Segments" "Grammar") (cl-subseq parts 0 3)))
      (should (member "Sthiramati" parts))
      (should-not (cl-intersection '("Commentary" "Meine Notizen") parts :test #'equal))
      (should (string-match-p "^\\*\\* 01 Vers" text))
      (should (string-match-p "^\\*\\* Sentence 258\n:PROPERTIES:\n:CUSTOM_ID: comm-258\n:END:$" text))
      (should (string-match-p "^\\*\\*\\* 258\\b.*\n\\[\\[#comm-258\\]" text))
      (should (bcat-dossier-test--links-resolve text))
      (should (equal file (plist-get (bcat-dossier-pdf-text doc) :lead)))
      (should (equal file (plist-get (bcat-dossier-pdf-text (tibetan-sentence--filepath 64 tfolder tsrc)) :lead))))))

(ert-deftest bcat-dossier-pdf-page-breaks ()
  "Carsten 9.11.: im PDF des Lesedossiers beginnt jede Schicht der
Reading-Seite (01 Vers, 02 Bhāṣya, 03 Kommentar) auf einer neuen Seite,
die erste nicht; in einer Schicht die zweite Sprache (der zweite
Versblock) ebenso; die Sätze des Kommentars untereinander nicht."
  (bcat-commentary-test--with-msa
    (bcat-sentence-test--with-stubs #'bcat-commentary-test--answers
      (bcat-sentence-test--fetch-n src 36))
    (let* ((text (plist-get (bcat-dossier-pdf-text (tibetan-sentence--filepath 36 folder src)) :text))
           (reading (substring text (string-match "^\\* Reading$" text)
                               (string-match "^\\* Segments$" text)))
           (layers (with-temp-buffer (insert reading)
                     (how-many "^\\*\\* 0[0-9] " (point-min) (point-max))))
           (breaks (with-temp-buffer (insert reading)
                     (how-many "^#\\+LATEX: \\\\clearpage$" (point-min) (point-max)))))
      (should (>= layers 2))
      (should-not (string-match-p "#\\+LATEX: \\\\clearpage\n\\*\\* 01 " reading))
      (should (string-match-p "^#\\+LATEX: \\\\clearpage\n\\*\\* 02 " reading))
      (should (string-match-p "^#\\+LATEX: \\\\clearpage\n\\*\\* 03 " reading))
      (should (string-match-p "^#\\+end_verse\n\n#\\+LATEX: \\\\clearpage\n#\\+begin_verse" reading))
      (should-not (string-match-p "^#\\+LATEX: \\\\clearpage\n\\*\\*\\* " reading))
      ;; die Segments des Verses aus seiner Ansicht: Tibetisch neu
      (should (string-match-p "^#\\+LATEX: \\\\clearpage\n\\*\\* Tibetan$" text))
      (should (> breaks (1- layers))))))

(ert-deftest bcat-dossier-pdf-none ()
  "A sentence with one layer (no bhāṣya, no commentary) has no dossier
PDF — C-c u H keeps its own view."
  (bcat-sentence-test--with-source
    (bcat-sentence-test--create src)
    (bcat-sentence-test--with-stubs bcat-sentence-test--answer
      (bcat-sentence-test--fetch src))
    (should-not (bcat-dossier-pdf-text (tibetan-sentence--filepath 9 folder src)))))

(ert-deftest bcat-dossier-nest-view ()
  "A second sentence file in the dossier (AS: the bhāṣya sentence):
its view's parts one level down under a part of its own, its jump
targets and links renamed so that they cannot meet the first file's."
  (should (equal (concat "* Bhāṣya\n:PROPERTIES:\n:CUSTOM_ID: bhasya\n:END:\n"
                         "** Segments\n:PROPERTIES:\n:CUSTOM_ID: bhasya-segments\n:END:\n"
                         "⟦40⟧ · [[#bhasya-grammar-40][Grammar]]\n| x |\n\n"
                         "** Grammar\n:PROPERTIES:\n:CUSTOM_ID: bhasya-grammar\n:END:\n"
                         "*** ⟦40⟧\n:PROPERTIES:\n:CUSTOM_ID: bhasya-grammar-40\n:END:\n*Note:* n\n")
                 (bcat-dossier--nest-view
                  (concat "* Reading\n:PROPERTIES:\n:CUSTOM_ID: reading\n:END:\nR\n"
                          "* Segments\n:PROPERTIES:\n:CUSTOM_ID: segments\n:END:\n"
                          "⟦40⟧ · [[#grammar-40][Grammar]]\n| x |\n\n"
                          "* Grammar\n:PROPERTIES:\n:CUSTOM_ID: grammar\n:END:\n"
                          "** ⟦40⟧\n:PROPERTIES:\n:CUSTOM_ID: grammar-40\n:END:\n*Note:* n\n")
                  "Bhāṣya" "bhasya" '("segments" "grammar")))))

(ert-deftest bcat-dossier-export ()
  "C-c u H on any file of a verse with commentary exports the dossier:
the PDF under the verse's name (msa-IX.16.pdf), uploaded once as the
verse — no PDF of its own for the commentary document any more."
  (bcat-commentary-test--with-msa
    (bcat-sentence-test--with-stubs #'bcat-commentary-test--answers
      (bcat-sentence-test--fetch-n src 36))
    (let ((doc (expand-file-name "comm-IX.16-savbh.org" cfolder))
          (exported nil) (uploaded '()))
      (cl-letf (((symbol-function 'bcat-export-pdf)
                 (lambda (f &rest _)
                   (setq exported (with-temp-buffer (insert-file-contents f) (buffer-string)))
                   (let ((pdf (concat (file-name-sans-extension f) ".pdf")))
                     (with-temp-file pdf (insert "PDF")) pdf)))
                ((symbol-function 'tibetan-discussion-paper--upload)
                 (lambda (pdf _title label &optional done)
                   (push (cons (file-name-nondirectory pdf) label) uploaded)
                   (when done (funcall done t label)) t)))
        (let ((bcat-export-upload t))
          (bcat-export-file doc)
          (should (string-match-p "^\\* Reading$" exported))
          (should (string-match-p "^\\* Sthiramati$" exported))
          (should (equal '(("msa-IX.16.pdf" . "MSA IX.16")) uploaded)))))))

(ert-deftest bcat-dossier-one-upload ()
  "Carsten 5.11.: „doppelten Upload weg“.  One C-c u G on a verse with
commentary lands twice — the verse pair, then the commentary (or the
other way round); each ended with C-c u H, and both exported the same
dossier.  Now the first to land waits for the other: one export, when
both are in."
  (bcat-commentary-test--with-msa
    (let ((exports '()))
      (cl-letf (((symbol-function 'bcat-export-file) (lambda (f) (push f exports) nil))
                ((symbol-function 'tibetan-discussion-paper--temp-source-p) #'ignore))
        (let ((bcat-export-auto t))
          (bcat-sentence-test--with-stubs #'bcat-commentary-test--answers
            (add-hook 'bcat-sentence-after-fetch-functions #'bcat-export-after-fetch)
            (bcat-sentence-test--fetch-n src 36))))
      (should (= 1 (length exports))))))

(ert-deftest bcat-dossier-key ()
  "C-c u V writes and shows the dossier of the verse at point."
  (require 'tibetan-keybindings)
  (should (eq (key-binding (kbd "C-c u V")) 'bcat-dossier)))

(provide 'bcat-dossier-test)
;;; bcat-dossier-test.el ends here
