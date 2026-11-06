;;; bcat-faces-test.el --- Tests for the colours of the sentence files in Emacs  -*- lexical-binding: t; -*-

;;; Commentary:
;; Carsten 31.10.2026: reading a segment „fällt es mir schwer … die
;; einzelnen Wörter im Überblick zu halten. Im .org geht's besser, ist
;; aber auch nicht so richtig optimal. Farbliche Hervorhebung wäre da
;; schön“ — word against gloss, as in the PDF (673e39e).

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((root (expand-file-name ".." (file-name-directory
                                    (or load-file-name buffer-file-name)))))
  (dolist (d '("persist" "core" "analysis" "philology" "data" "config" "doc-prep" "test"))
    (add-to-list 'load-path (expand-file-name d root))))

(require 'org)
(require 'bcat-faces)

(defconst bcat-faces-test--text
  (concat "* Text\n#+begin_verse\n"
          "tad [that] an-ākarṇya [not having heard] /\n"
          "#+end_verse\n\n"
          "*Structure:* /tad/ [object] → protasis\n\n"
          "* Segments\n** Segment 683\n"
          "| tad               | an-ākarṇya               |\n"
          "| —                 | an- + ā-√karṇ-ya         |\n"
          "|                   | an- not · √karṇ (10) ear |\n"
          "| that              | not having heard         |\n"
          "| pron. n. acc. sg. | gerund                   |\n")
  "A piece of a Sanskrit sentence file.")

(defun bcat-faces-test--matches (matcher group)
  "The texts of GROUP of every match of MATCHER in the current buffer."
  (goto-char (point-min))
  (let ((out '()))
    (while (funcall matcher (point-max))
      (push (match-string-no-properties group) out))
    (nreverse out)))

(ert-deftest bcat-faces-word-and-gloss ()
  "In the line with the vocabulary (a verse block) each unit: the word
\(group 1) and its gloss in brackets (group 2) — not the brackets of a
Structure line outside the verse."
  (with-temp-buffer
    (insert bcat-faces-test--text)
    (org-mode)
    (should (equal '("tad" "an-ākarṇya")
                   (bcat-faces-test--matches #'bcat-faces--unit-matcher 1)))
    (should (equal '("[that]" "[not having heard]")
                   (bcat-faces-test--matches #'bcat-faces--unit-matcher 2)))))

(ert-deftest bcat-faces-detail-rows ()
  "In a table the rows that explain the words (formation, parts, form) —
not the words' row (Carsten 31.10.: the words stay black, the rest
blue), not the gloss row, next to last (same evening, TĀ 1006: „auch die
Übersetzung der einzelnen Terme schwarz … dann sehe ich gleich was
wichtig ist“), and not the rows of the metre table (second row only –
and ⏑).  A table in bands (hline between): each band on its own."
  (with-temp-buffer
    (insert bcat-faces-test--text
            "\n* Recitation\n| ta | da |\n| ⏑  | –  |\n"
            "\n| bden  |\n| truth |\n| N     |\n|-------|\n| gnyis |\n| two   |\n| NUM   |\n")
    (org-mode)
    (should (equal '("| —                 | an- + ā-√karṇ-ya         |"
                     "|                   | an- not · √karṇ (10) ear |"
                     "| pron. n. acc. sg. | gerund                   |"
                     "| N     |"
                     "| NUM   |")
                   (bcat-faces-test--matches #'bcat-faces--detail-row-matcher 0)))))

(ert-deftest bcat-faces-parts-tables ()
  "5.11. (Carsten, draft B): under a Sanskrit segment's translation the
tables of the words' parts — the members as a header row, an hline, the
parts below — have the parts blue and the members black, as the
formation and parts rows before; the line of a word of one member,
„/jananī/: …“, the word black and its parts blue.  The comparison's
overview (\\cellcolor) stays plain, a Structure line of the same shape
\(under * Segments) too."
  (with-temp-buffer
    (insert "* Text\n⟦1⟧\n| jananī      |\n| mother      |\n| f. nom. sg. |\n\n→ t\n\n"
            "| vimala      | kalā        |\n|-------------+-------------|\n"
            "| /vi-/ apart | part, power |\n| /-a/ adj    |             |\n\n"
            "/jananī/: /√jan/ (1) be born · /-anī/ f. agent\n\n"
            "* Sentence\n** Translations compared\n"
            "| Nr. | Stelle | Einstufung                   |\n|-----+--------+------------------------------|\n"
            "|   1 | /a/    | @@latex:\\cellcolor{bad}@@rot |\n\n"
            "* Segments\n** Segment 1\n*Structure:* x \\\\\n/jananī/: rest\n")
    (org-mode)
    (should (equal '("| f. nom. sg. |" "| /vi-/ apart | part, power |" "| /-a/ adj    |             |")
                   (bcat-faces-test--matches #'bcat-faces--detail-row-matcher 0)))
    (should (equal '("/√jan/ (1) be born · /-anī/ f. agent")
                   (bcat-faces-test--matches #'bcat-faces--parts-line-matcher 1)))))

(ert-deftest bcat-faces-tibetan-table ()
  "A Tibetan table (Uchen, Wylie, gloss, function — Carsten 31.10.:
Uchen and Wylie black): only the function row is blue."
  (with-temp-buffer
    (insert "* Text\n⟦66⟧\n\n| དེ        | ལས   |\n| de        | las  |\n| that/them | from |\n| DEM       | ABL  |\n")
    (org-mode)
    (should (equal '("| DEM       | ABL  |")
                   (bcat-faces-test--matches #'bcat-faces--detail-row-matcher 0)))))

(ert-deftest bcat-faces-comparison-table-plain ()
  "2.11.: the overview table of the comparison (a header row and an hline
under it — no table of words has one) stays in the text colour: no
detail rows there."
  (with-temp-buffer
    (insert "** Translations compared\n*** Übersicht\n"
            "| Nr. | Stelle | Einstufung                   | Kern |\n"
            "|-----+--------+------------------------------+------|\n"
            "|   1 | /a/    | @@latex:\\cellcolor{bad}@@rot | x    |\n"
            "|   2 | /b/    | @@latex:\\cellcolor{ok}@@grün | y    |\n"
            "|   3 | /c/    | @@latex:\\cellcolor{mid}@@gelb | z    |\n"
            "\n| bden  |\n| truth |\n| N     |\n")
    (org-mode)
    (should (equal '("| N     |")
                   (bcat-faces-test--matches #'bcat-faces--detail-row-matcher 0)))))

(ert-deftest bcat-faces-ratings ()
  "2.11.: the ratings of the comparison in their colour in Emacs too —
Carsten reads the .org first: the word after @@latex:\\cellcolor{…}@@
in the overview (the snippet itself greyed) and the rating at the end of
a unit's heading, green/yellow/red as in the PDF (the Muster's ok, mid,
bad)."
  (with-temp-buffer
    (insert "|   1 | /a/ | @@latex:\\cellcolor{bad}@@rot  | x |\n"
            "|   2 | /b/ | @@latex:\\cellcolor{ok}@@grün  | y |\n"
            "**** 1. /abhinavasṛṣṭimahā/ — gelb\n"
            "**** 2. /hṛdayam/ — green\n"
            "*Befund:* gelb ist hier kein Urteil.\n"
            "- *3. /pañcamukhaguptarucir/ — rot*\n")
    (org-mode)
    (should (equal '(("rot" . bcat-rating-bad-face) ("grün" . bcat-rating-ok-face)
                     ("gelb" . bcat-rating-mid-face) ("green" . bcat-rating-ok-face)
                     ("rot" . bcat-rating-bad-face))
                   (progn (goto-char (point-min))
                          (let ((out '()))
                            (while (bcat-faces--rating-matcher (point-max))
                              (push (cons (match-string-no-properties 2)
                                          (bcat-faces--rating-face (match-string-no-properties 1)))
                                    out))
                            (nreverse out)))))
    (should (eq 'bcat-rating-bad-face (bcat-faces--rating-face "bad")))
    (should (eq 'bcat-rating-mid-face (bcat-faces--rating-face "yellow")))
    (should-not (bcat-faces--rating-face "blau"))))

(ert-deftest bcat-faces-no-line-numbers-when-reading ()
  "Carsten 4.11., reading on the MacBook: „ziemlich viel verschenkter
Platz“ — no line numbers in sentence files and in the cascade source
\(its window was 22 characters wide), even with
`global-display-line-numbers-mode' on; other org files keep them."
  (let ((dir (make-temp-file "bcat-faces-" t))
        (was global-display-line-numbers-mode))
    (unwind-protect
        (let ((sent (expand-file-name "sent-016-bden.org" dir))
              (source (expand-file-name "bden-gnyis-cat.org" dir))
              (other (expand-file-name "notes.org" dir)))
          (global-display-line-numbers-mode 1)
          (with-temp-file sent (insert "#+CAT_TEMPLATE: tibetan\n\n" bcat-faces-test--text))
          (with-temp-file source (insert "#+TITLE: bDen\n#+TIBETAN_LAYOUT: cascade\n\n* Tibetan Text\n"))
          (with-temp-file other (insert bcat-faces-test--text))
          (dolist (f (list sent source other))
            (with-current-buffer (find-file-noselect f)
              (unwind-protect
                  (should (eq (equal f other) (and display-line-numbers-mode t)))
                (kill-buffer)))))
      (global-display-line-numbers-mode (if was 1 -1))
      (delete-directory dir t))))

(ert-deftest bcat-faces-header-folded ()
  "Carsten 4.11.: the ten header lines (#+TITLE … #+LAST_ANALYZED) take
room while reading — in Emacs they fold behind the #+TITLE line („…“),
the file stays as it is; `bcat-faces-toggle-header' shows them."
  (with-temp-buffer
    (insert "#+TITLE: Sentence 16 Analysis\n#+STARTUP: showall\n#+CAT_TEMPLATE: tibetan\n"
            "#+LAST_ANALYZED: 2026-11-02\n\n* Text\n#+begin_verse\nx\n#+end_verse\n")
    (org-mode)
    (bcat-faces-mode 1)
    (goto-char (point-min))
    (should (bcat-faces--header-matcher (point-max)))
    (should (equal (list (save-excursion (goto-char (point-min)) (line-end-position))
                         (save-excursion (goto-char (point-min)) (forward-line 3) (line-end-position)))
                   (list (match-beginning 0) (match-end 0))))
    ;; Only the top block: #+begin_verse is no header.
    (should-not (bcat-faces--header-matcher (point-max)))
    (should (member '(bcat-header . t) buffer-invisibility-spec))
    (should (memq 'invisible font-lock-extra-managed-props))
    (bcat-faces-toggle-header)
    (should-not (assq 'bcat-header buffer-invisibility-spec))
    (bcat-faces-toggle-header)
    (should (member '(bcat-header . t) buffer-invisibility-spec))))

(ert-deftest bcat-faces-tibetan-flatter ()
  "Carsten 4.11. (MacBook, „ziemlich viel verschenkter Platz“): Uchen
lines were 43 px high against 19 for Latin (Tibetan Machine Uni 20);
chosen: Kailasa 18 (≈ 23 px font height).  In a sentence file every
Tibetan run gets `bcat-tibetan-face'; the gold shads of Carsten's init
\(display overlays in `cp/tibetan-shad-face') are remapped to Kailasa
too, else they alone keep the lines high."
  (should (equal "Kailasa" (face-attribute 'bcat-tibetan-face :family nil t)))
  (should (equal 1.3 (face-attribute 'bcat-tibetan-face :height nil t)))
  (with-temp-buffer
    (insert "* Text\n#+begin_verse\nདེ་ལས། །\n#+end_verse\n| དེ | ལས |\n| de | las |\n")
    (org-mode)
    (goto-char (point-min))
    (should (bcat-faces--tibetan-matcher (point-max)))
    (should (equal "དེ་ལས།" (match-string 0)))
    (should (bcat-faces--tibetan-matcher (point-max)))
    (should (equal "།" (match-string 0)))
    (let ((had (facep 'cp/tibetan-shad-face)))
      (unless had (defface cp/tibetan-shad-face '((t :foreground "#D4A843")) "test"))
      (bcat-faces-mode 1)
      (should (assq 'cp/tibetan-shad-face face-remapping-alist))
      (bcat-faces-mode -1)
      (should-not (assq 'cp/tibetan-shad-face face-remapping-alist)))))

(defun bcat-faces-test--merged (face attr)
  "ATTR (:family, :height, :foreground) of FACE as Emacs merges it in the
current buffer — through the buffer's face remapping, by the code
redisplay uses (`face-attributes-as-vector'; the index is the one of
xfaces.c)."
  (aref (face-attributes-as-vector face)
        (pcase attr (:family 1) (:height 4) (:foreground 9))))

(ert-deftest bcat-faces-shad-remap-valid ()
  "Bug 6.11.: *Messages* in Carsten's Emacs full of, alternating,
  Invalid face attribute :fontset \"-*-menlo-…-fontset-bcattibetan\"
  Invalid face reference: cp/tibetan-shad-face
— the redisplay of the gold shads in 25 open sentence files, on graphic
frames.  A face property list (the remapping) may not name a fontset,
only a named face takes one (`set-face-attribute'): Emacs dropped the
whole remapping, the shads came out in no face of their own — not
Kailasa, not gold — and kept the Uchen lines at 43 px instead of 27.
On a graphic display (the fontset stubbed: the batch Emacs has none):
merging the shad face in a sentence file logs nothing and gives Kailasa
at an absolute height in the shads' gold; the fontset comes through the
named `bcat-tibetan-face'."
  (unless (facep 'cp/tibetan-shad-face)
    (defface cp/tibetan-shad-face
      '((t :foreground "#D4A843" :family "Jomolhari" :height 1.4)) "test"))
  (cl-letf (((symbol-function 'bcat-faces--tibetan-fontset)
             (lambda () "-*-menlo-regular-normal-normal-*-*-*-*-*-m-0-fontset-bcattibetan")))
    (with-temp-buffer
      (insert "* Text\n#+begin_verse\nདེ་ལས། །\n#+end_verse\n")
      (org-mode)
      (bcat-faces-mode 1)
      (let* ((message-log-max t)
             (mark (progn (message "bcat-faces-test: merging the shads")
                          (with-current-buffer (messages-buffer) (copy-marker (point-max)))))
             (family (bcat-faces-test--merged 'cp/tibetan-shad-face :family))
             (height (bcat-faces-test--merged 'cp/tibetan-shad-face :height))
             (fg (bcat-faces-test--merged 'cp/tibetan-shad-face :foreground)))
        (should-not (string-match-p "Invalid face"
                                    (with-current-buffer (messages-buffer)
                                      (buffer-substring-no-properties mark (point-max)))))
        (should (equal "Kailasa" family))
        (should (integerp height))
        (should (equal (face-attribute 'cp/tibetan-shad-face :foreground nil t) fg))
        (should (memq 'bcat-tibetan-face
                      (flatten-tree (assq 'cp/tibetan-shad-face face-remapping-alist))))))))

(ert-deftest bcat-faces-uchen-row-aligned ()
  "4.11.: the Uchen row of a table did not line up with the rows under
it — Tibetan glyphs are not as wide as the monospace grid counts them.
The space before each bar of the Uchen row aligns to the column of that
bar in the row below (`:align-to', plus the line's org-indent prefix);
other rows and tables without Uchen get nothing."
  (with-temp-buffer
    (insert "* Text\n| དེ                  | ལས   |\n| [[steinert:de][de]]                 | las  |\n| that/him           | from |\n\n| a | b |\n| c | d |\n")
    (org-mode)
    (goto-char (point-min))
    (should-not (bcat-faces--uchen-row-matcher (point-max)))
    (let ((cols '()) (pos (save-excursion (goto-char (point-min)) (forward-line 1) (point))))
      (save-excursion
        (goto-char pos)
        (while (search-forward "|" (line-end-position) t)
          (let ((d (get-text-property (- (point) 2) 'display)))
            (when d
              (push d cols)
              ;; the whole padding of the cell is one stretch
              (should (eq d (get-text-property
                             (save-excursion (backward-char) (skip-chars-backward " ") (point))
                             'display)))))))
      (should (equal '((space :align-to 21) (space :align-to 28)) (nreverse cols))))
    (should-not (text-property-not-all (save-excursion (goto-char (point-min)) (forward-line 2) (point))
                                       (point-max) 'display nil))
    (bcat-faces-mode 1)
    (should (memq 'display font-lock-extra-managed-props))))

(ert-deftest bcat-faces-only-in-sentence-files ()
  "Switched on in a sentence file of a template (sent-…, comm-… with
#+CAT_TEMPLATE:), not in other org files."
  (let ((dir (make-temp-file "bcat-faces-" t)))
    (unwind-protect
        (let ((sent (expand-file-name "sent-1006-tantralo.org" dir))
              (other (expand-file-name "notes.org" dir)))
          (with-temp-file sent (insert "#+CAT_TEMPLATE: sanskrit\n\n" bcat-faces-test--text))
          (with-temp-file other (insert bcat-faces-test--text))
          (dolist (f (list sent other))
            (with-current-buffer (find-file-noselect f)
              (unwind-protect
                  (should (eq (equal f sent) (and bcat-faces-mode t)))
                (kill-buffer)))))
      (delete-directory dir t))))

(provide 'bcat-faces-test)
;;; bcat-faces-test.el ends here
