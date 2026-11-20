;;; bcat-outline-test.el --- Tests for bcat-outline -*- lexical-binding: t -*-

;;; Commentary:
;; The outline (Gliederung, sa bcad) of a document: reading one, the
;; place of a sentence in the whole, what a fetch is told of it.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'bcat-outline)

(defconst bcat-outline-test--text
  (concat
   "#+TITLE: Tantrāloka — Gliederung\n#+OPTIONS: toc:3 num:nil\n\n"
   "Diese Gliederung ist wie ein /sa bcad/ gebaut.\n\n"
   "* Der Tantrāloka im Ganzen\n\nDer Gedanke, der das ganze Werk ordnet …\n\n"
   "** Kap. 1: Überblick\n1. /vijñānabhid/ …\n\n"
   "* Kapitel 1 (vijñānabhid): Gliederung\n\nDer Titel heißt …\n\n"
   "** I. Einleitung: Segen, Lob, Anlass (V. 1–21)\n"
   "V. 1–21 · KSTS S. 1–51 · D. S. 5–115\n\n"
   "Bevor die Lehre beginnt, steht kunstvolle Dichtung.\nDie Verse stellen die Hauptbegriffe vor.\n\n"
   "*** Titel\nSatz 1000 · KSTS S. 1\n\n"
   "Der Kolophon steht am Anfang.\n\n"
   "- *1000*: Titelei. Sie nennt Werk, Verfasser\n  und Kommentator.\n\n"
   "*** A. Segensvers: das pulsierende Herz (V. 1)\n"
   "V. 1 · Satz 1 · KSTS S. 3–15 · D. S. 5–33\n\n"
   "Mutter und Vater, zugleich Śakti und Śiva.\n\n"
   "| Glied | Bezug |\n|-------+-------|\n| /jananī/ | Mutter |\n\n"
   "*** Jayaratha: Kommentar zu V. 1 in drei Deutungen\n"
   "Sätze 1009–1059 · KSTS S. 4–15\n\n"
   "**** 1. Erste Deutung: das Herz als Einheit von Śiva und Śakti (Trika)\n"
   "Sätze 1009–1025 · KSTS S. 4–7\n\n"
   "Das Herz ist die Vereinigung von Śiva und Śakti.\n\n"
   "***** a. Das Herz und der Wunsch\nSatz 1009 · KSTS S. 4\n\n"
   "- *1009*: Erklärt /mama … hṛdayaṃ saṃsphuratāt/.\n  Danach kommen die Beiwörter (1010).\n\n"
   "***** b. Erstes Beiwort des Herzens: /tadubhaya-yāmala/\nSätze 1010–1011 · KSTS S. 4\n\n"
   "Das längste Kompositum des Verses.\n\n"
   "- *1010*: Frage: „Und wie ist es beschaffen?“ Die Auflösung folgt in 1011.\n"
   "- *1011*: Auflösung. Zwei Zitate stützen sie (wohl Kap. 3; nicht geprüft).\n\n"
   "** II. Wissen und Nichtwissen: Was bindet, was befreit? (V. 22–51)\n"
   "V. 22–51 · KSTS S. 54–88 · D. S. 116–173\n\n"
   "Übergang: Nach der Einleitung beginnt die Argumentation.\n\n"
   "*** A. Bindung und Befreiung (V. 22–35)\nV. 22–35 · KSTS S. 54–72 · D. S. 116–143\n\n"
   "Alle Schriften sagen: Nichtwissen ist die Ursache des Kreislaufs.\n\n"
   "* Anhang: Ort im Ganzen je Satz\nEine Zeile je Viveka-Satz.\n\n"
   "#+NAME: ort-im-ganzen\n"
   "- 1000: Titel\n"
   "- 1009: TĀ 1.1 › Deutung 1 (Trika) › Herz und Wunsch (hṛdaya, saṃsphuratāt)\n"
   "- 1010: TĀ 1.1 › Deutung 1 › Herz, Beiwort 1 (tadubhaya…visargamaya): Frage\n")
  "An outline in the form of WS26-27/Tantraloka/Gliederung-TA1.org
\(20.11.2026, the reference of the pattern), shortened.")

(ert-deftest bcat-outline-reads-the-reference-form ()
  "20.11. (Carsten über die Tantrāloka-Sitzung: „gib das mit der
Gliederung bitte auch als Pattern mit“): an outline in the form of the
Tantrāloka's is read — its nodes (a heading with, on the next line, the
sentences or verses it covers: „Sätze 1009–1025 · KSTS S. 4–7“, „V.
22–35 · …“ — a verse's number is its sentence's; where a line names
both, the sentences), the lines of the single sentences („- *1009*:
…“, over several lines), and the list „Ort im Ganzen“ (#+NAME:
ort-im-ganzen)."
  (let* ((text bcat-outline-test--text)
         (nodes (bcat-outline-nodes text))
         (node (lambda (title) (cl-find title nodes :key (lambda (n) (plist-get n :title)) :test #'string-prefix-p))))
    (should (equal '(1 21 2) (let ((n (funcall node "I. Einleitung")))
                               (list (plist-get n :from) (plist-get n :to) (plist-get n :level)))))
    (should (equal '(1000 1000 3) (let ((n (funcall node "Titel")))
                                    (list (plist-get n :from) (plist-get n :to) (plist-get n :level)))))
    ;; „V. 1 · Satz 1“: the sentences
    (should (equal '(1 1) (let ((n (funcall node "A. Segensvers")))
                            (list (plist-get n :from) (plist-get n :to)))))
    (should (equal '(1009 1025 4) (let ((n (funcall node "1. Erste Deutung")))
                                    (list (plist-get n :from) (plist-get n :to) (plist-get n :level)))))
    (should (equal '(22 35) (let ((n (funcall node "A. Bindung")))
                              (list (plist-get n :from) (plist-get n :to)))))
    ;; what a node says: its paragraphs, not the line of its range, not its sentences' lines
    (should (equal "Bevor die Lehre beginnt, steht kunstvolle Dichtung. Die Verse stellen die Hauptbegriffe vor."
                   (plist-get (funcall node "I. Einleitung") :text)))
    (should (equal "Das längste Kompositum des Verses." (plist-get (funcall node "b. Erstes Beiwort") :text)))
    ;; a heading without such a line is no node
    (should-not (funcall node "Kapitel 1"))
    (should-not (funcall node "Kap. 1: Überblick"))
    ;; the single sentences
    (should (equal '((1000 . "Titelei. Sie nennt Werk, Verfasser und Kommentator.")
                     (1009 . "Erklärt /mama … hṛdayaṃ saṃsphuratāt/. Danach kommen die Beiwörter (1010).")
                     (1010 . "Frage: „Und wie ist es beschaffen?“ Die Auflösung folgt in 1011.")
                     (1011 . "Auflösung. Zwei Zitate stützen sie (wohl Kap. 3; nicht geprüft)."))
                   (bcat-outline-entries text)))
    ;; the list at the end
    (should (equal '((1000 . "Titel")
                     (1009 . "TĀ 1.1 › Deutung 1 (Trika) › Herz und Wunsch (hṛdaya, saṃsphuratāt)")
                     (1010 . "TĀ 1.1 › Deutung 1 › Herz, Beiwort 1 (tadubhaya…visargamaya): Frage"))
                   (bcat-outline-places text)))
    (should-not (bcat-outline-nodes "* Text\nNichts.\n"))
    (should-not (bcat-outline-entries ""))
    (should-not (bcat-outline-places "* Text\n- 1: kein Anhang\n"))))

(ert-deftest bcat-outline-place-of-a-sentence ()
  "The place of a sentence in the whole: its line in the list „Ort im
Ganzen“; without one (the Tantrāloka's list has the Viveka's sentences,
the verses stand in the nodes) the way down the nodes that cover it —
their titles without the range in parentheses, joined by „ › “.  nil
for a sentence the outline does not know."
  (let ((text bcat-outline-test--text))
    (should (equal "TĀ 1.1 › Deutung 1 (Trika) › Herz und Wunsch (hṛdaya, saṃsphuratāt)"
                   (bcat-outline-place text 1009)))
    ;; no line of its own: from the nodes
    (should (equal (concat "I. Einleitung: Segen, Lob, Anlass › Jayaratha: Kommentar zu V. 1 in drei Deutungen › "
                           "1. Erste Deutung: das Herz als Einheit von Śiva und Śakti (Trika) › "
                           "b. Erstes Beiwort des Herzens: /tadubhaya-yāmala/")
                   (bcat-outline-place text 1011)))
    (should (equal "II. Wissen und Nichtwissen: Was bindet, was befreit? › A. Bindung und Befreiung"
                   (bcat-outline-place text 25)))
    (should (equal "II. Wissen und Nichtwissen: Was bindet, was befreit?" (bcat-outline-place text 40)))
    (should (equal "I. Einleitung: Segen, Lob, Anlass › A. Segensvers: das pulsierende Herz"
                   (bcat-outline-place text 1)))
    (should-not (bcat-outline-place text 700))
    (should-not (bcat-outline-place "" 1))))

(ert-deftest bcat-outline-request-block ()
  "What a fetch is told of the outline (Tantrāloka-Sitzung, 20.11.: „Die
Gliederung geht beim Abruf als Kontext mit … Dann passt die Analyse in
den Aufbau, statt dass die Gliederung der Analyse hinterherläuft“): the
place of the sentence, what the outline says of it and of its
neighbours, the node it stands in — and the request to say in
`outline_note' where the analysis contradicts the outline, not to adapt
in silence.  nil when the outline does not know the sentence."
  (let* ((text bcat-outline-test--text)
         (block (bcat-outline-request-block text 1010 1009 1011)))
    (should (string-match-p "\\`Outline of the text (Gliederung, sa bcad) — where this sentence stands:\n" block))
    (should (string-match-p "^Place: TĀ 1\\.1 › Deutung 1 › Herz, Beiwort 1 (tadubhaya…visargamaya): Frage$" block))
    (should (string-match-p "^Section: b\\. Erstes Beiwort des Herzens: /tadubhaya-yāmala/ — Das längste Kompositum des Verses\\.$"
                            block))
    (should (string-match-p "^This sentence (1010): Frage: „Und wie ist es beschaffen\\?“ Die Auflösung folgt in 1011\\.$" block))
    (should (string-match-p "^Before (1009): Erklärt /mama … hṛdayaṃ saṃsphuratāt/\\. Danach kommen die Beiwörter (1010)\\.$" block))
    (should (string-match-p "^After (1011): Auflösung\\. Zwei Zitate stützen sie" block))
    (should (string-match-p "\"outline_note\"" block))
    (should (string-match-p "Do not adapt the outline silently" block))
    (should (< (string-match "^Place: " block) (string-match "^Section: " block)
               (string-match "^Before (1009)" block) (string-match "^This sentence (1010)" block)
               (string-match "^After (1011)" block) (string-match "\"outline_note\"" block))))
  ;; a verse in a node, no line of its own, no neighbours given
  (let ((block (bcat-outline-request-block bcat-outline-test--text 25 nil nil)))
    (should (string-match-p "^Place: II\\. Wissen und Nichtwissen: Was bindet, was befreit\\? › A\\. Bindung und Befreiung$" block))
    (should (string-match-p "^Section: A\\. Bindung und Befreiung — Alle Schriften sagen" block))
    (should-not (string-match-p "^This sentence\\|^Before\\|^After" block)))
  (should-not (bcat-outline-request-block bcat-outline-test--text 700 699 701))
  (should-not (bcat-outline-request-block "" 1 nil nil)))

(ert-deftest bcat-outline-file-of-a-source ()
  "The outline of a source: the file its #+CAT_OUTLINE names (relative
to its folder), else Gliederung-<name of the source>.org beside it."
  (let* ((dir (make-temp-file "bcat-outline-" t))
         (src (expand-file-name "bden-cat.org" dir)))
    (unwind-protect
        (progn
          (with-temp-file src (insert "#+TITLE: x\n#+CAT_TEMPLATE: tibetan\n* Tibetan Text\n"))
          (should (equal (expand-file-name "Gliederung-bden-cat.org" dir) (bcat-outline-file src)))
          (should-not (bcat-outline-text src))
          (with-temp-file src (insert "#+TITLE: x\n#+CAT_OUTLINE: ../Gliederung-TA1.org\n* Tibetan Text\n"))
          (should (equal (expand-file-name "../Gliederung-TA1.org" dir) (bcat-outline-file src)))
          (with-temp-file src (insert "#+TITLE: x\n* Tibetan Text\n"))
          (with-temp-file (expand-file-name "Gliederung-bden-cat.org" dir) (insert bcat-outline-test--text))
          (should (equal bcat-outline-test--text (bcat-outline-text src))))
      (delete-directory dir t))))

(provide 'bcat-outline-test)
;;; bcat-outline-test.el ends here
