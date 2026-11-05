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

(ert-deftest bcat-dossier-key ()
  "C-c u V writes and shows the dossier of the verse at point."
  (require 'tibetan-keybindings)
  (should (eq (key-binding (kbd "C-c u V")) 'bcat-dossier)))

(provide 'bcat-dossier-test)
;;; bcat-dossier-test.el ends here
