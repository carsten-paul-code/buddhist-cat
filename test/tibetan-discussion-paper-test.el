;;; tibetan-discussion-paper-test.el --- §5.64 discussion paper for the reMarkable -*- lexical-binding: t -*-

;;; Commentary:
;; Carsten (20.10.2026): in der Klasse ist der Text im CAT-Tool offen;
;; daneben auf dem reMarkable ein Diskussionspapier — je Satz
;; Text + Übersetzung, Concept Notes, Sanskrit-Vergleich (bei
;; zweisprachigen Texten) und Diskussion; reMarkable-Seitenformat,
;; möglichst eine Seite; nach dem Export per rmapi hochgeladen.
;; Fixtures: das Handout-Korpus (temp dirs); kein LaTeX, kein rmapi.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((base-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../persist" base-dir))
  (add-to-list 'load-path (expand-file-name "../core" base-dir))
  (add-to-list 'load-path (expand-file-name "../analysis" base-dir))
  (add-to-list 'load-path base-dir))

(require 'tibetan-discussion-paper)
(require 'tibetan-handout-test)

(defconst tibetan-paper-test--comparison
  (concat
   "** Sanskrit Comparison\n:PROPERTIES:\n:GENERATED: 2026-10-20\n:END:\n"
   "Der Vers entspricht Sanskrit-Satz 1.\n\n"
   "*** 1. blos\n"
   "- Tibetisch :: blos mthong ba\n"
   "- Sanskrit :: buddhyā dṛṣṭam\n"
   "- Abweichung :: Instrumental statt Lokativ.\n"
   "- Bedeutung :: VERGLEICH-BEDEUTUNG.\n\n"
   "*** Einschätzung\nEng am Sanskrit.\n\n")
  "A landed comparison block (de labels).")

(defun tibetan-paper-test--add-comparison (file)
  "Append the fixture comparison to FILE's * Provided Translations."
  (tibetan-discussion--write-pt-child file "Sanskrit Comparison"
                                      tibetan-paper-test--comparison))

(defun tibetan-paper-test--read (file)
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

;; ----------------------------------------------------------------------------
;; Data
;; ----------------------------------------------------------------------------

(ert-deftest tibetan-discussion-paper-sentence-data ()
  "Per sentence: segments (Uchen + Wylie), the translation, the
Concept Notes, the discussion and the comparison (nil when absent)
— no vocabulary, no grammar; a sentence without analysis file is a
visible gap."
  (tibetan-handout-test--with-corpus
    (let ((d (tibetan-discussion-paper--sentence-data 654 src)))
      (should (equal 654 (plist-get d :sent-num)))
      (should (equal '(1970 1971)
                     (mapcar (lambda (s) (plist-get s :num)) (plist-get d :segs))))
      (should (string-match-p "བློས" (plist-get (car (plist-get d :segs)) :uchen)))
      (should (string-match-p "\\`blos mthong" (plist-get (car (plist-get d :segs)) :wylie)))
      (should (equal '((1970 . "Durch den Verstand gesehen,")
                       (1971 . "die scheinbare Wahrheit."))
                     (plist-get (plist-get d :translation) :by-seg)))
      (should (string-match-p "kun rdzob" (plist-get d :concepts)))
      (should (plist-get d :discussion))
      (should-not (plist-get d :comparison))
      ;; Carsten 26.10.: das Vokabular kommt ins Papier (★ Wortliste zuerst).
      (let ((blos (car (cdr (assq 1970 (plist-get d :vocab))))))
        (should (plist-get blos :star))
        (should (equal "Verstand // mind / awareness" (plist-get blos :gloss)))
        (should (equal "durch den Verstand" (plist-get blos :context))))
      (should-not (plist-member d :grammar)))
    (tibetan-paper-test--add-comparison (tibetan-sentence--filepath 654 folder src))
    (let ((c (plist-get (tibetan-discussion-paper--sentence-data 654 src) :comparison)))
      (should (string-match-p "Sanskrit-Satz 1" (plist-get c :correspondence)))
      (should (equal "blos" (plist-get (car (plist-get c :points)) :title)))
      (should (equal "Eng am Sanskrit." (plist-get c :assessment))))
    (should (plist-get (tibetan-discussion-paper--sentence-data 655 src) :missing))))

;; ----------------------------------------------------------------------------
;; Renderer
;; ----------------------------------------------------------------------------

(defun tibetan-paper-test--render (src folder &optional nums)
  (let ((spec (list :title "Rgyan" :scope "§220" :source-name "Rgyan-cat.org"
                    :date "2026-10-20"
                    :sentences (mapcar (lambda (n)
                                         (tibetan-discussion-paper--sentence-data n src))
                                       (or nums '(654 655))))))
    (ignore folder)
    (tibetan-discussion-paper-render spec)))

(ert-deftest tibetan-discussion-paper-render ()
  "reMarkable page (A5, small margins), no `* Text' part; per sentence
the Uchen/Wylie line, the Übersetzung, `** Begriffe', `**
Sanskrit-Vergleich' (when present) and `** Diskussion'; gaps are
visible; the word Claude and the provided translations never appear."
  (tibetan-handout-test--with-corpus
    (tibetan-paper-test--add-comparison (tibetan-sentence--filepath 654 folder src))
    (let ((s (tibetan-paper-test--render src folder)))
      (should (string-prefix-p tibetan-discussion-paper-generated-marker s))
      (should (string-match-p "a5paper" s))
      (should (string-match-p (regexp-quote "#+TITLE: Rgyan — §220") s))
      (should-not (string-match-p "^\\* Text$" s))
      (should (string-match-p "^\\* Satz 654 (Seg\\. 1970–1971)$" s))
      (should (string-match-p "བློས" s))
      (should (string-match-p "blos mthong ba dang" s))
      (should (string-match-p "Durch den Verstand gesehen" s))
      (should (string-match-p "^\\*\\* Begriffe$" s))
      (should (string-match-p "\\*kun rdzob\\* — note" s))
      (should (string-match-p "^\\*\\* Sanskrit-Vergleich$" s))
      (should (string-match-p "Der Vers entspricht Sanskrit-Satz 1" s))
      (should (string-match-p "^- Tibetisch :: blos mthong ba$" s))
      (should (string-match-p "^- Bedeutung :: VERGLEICH-BEDEUTUNG\\.$" s))
      (should (string-match-p "Eng am Sanskrit" s))
      (should (string-match-p "^\\*\\* Diskussion$" s))
      (should (string-match-p "^- Problem :: " s))
      (should (string-match-p "^- Lopez 2006 :: “by the mind”" s))
      ;; Reihenfolge: Begriffe → Sanskrit-Vergleich → Diskussion
      (should (< (string-match "^\\*\\* Begriffe$" s)
                 (string-match "^\\*\\* Sanskrit-Vergleich$" s)
                 (string-match "^\\*\\* Diskussion$" s)))
      ;; Satz 655 ohne Datei: Lücke
      (should (string-match-p "^\\* Satz 655" s))
      (should (string-match-p "luecke(Analysedatei fehlt" s))
      ;; Nie: Claude, Referenzvolltext, DM, Working Translation, Vokabular
      (should-not (string-match-p "Claude" s))
      (should-not (string-match-p "LOPEZ POISON TEXT" s))
      (should-not (string-match-p "DM POISON TEXT" s))
      (should-not (string-match-p "WT POISON TEXT" s))
      (should-not (string-match-p "Grammatik" s))
      ;; Vokabular: unter dem Text, vor der Übersetzung (Carsten 26.10.),
      ;; kompakt je Segment, ★ Wortliste zuerst, Kontextglosse klein.
      (should (< (string-match "{{{rubrik(Vokabular)}}}" s)
                 (string-match "{{{rubrik(Übersetzung)}}}" s)))
      (should (string-match-p "{{{n(1970)}}} \\*blos\\* ★ Verstand // mind / awareness {{{ctx(durch den Verstand)}}}" s))
      (should (string-match-p "{{{dim(ergative/instrumental; Skt\\. buddhi)}}}" s))
      (should (string-match-p "{{{n(1971)}}} \\*kun rdzob\\* ★ scheinbar" s))
      (should-not (string-match-p "POISON-NA" s)))))

(ert-deftest tibetan-discussion-paper-header-tibetan-fallback-font ()
  "The paper shares the handout's font setup: Tibetan fallback on the
main font (discussion titles carry Uchen)."
  (tibetan-handout-test--with-corpus
    (let ((s (tibetan-paper-test--render src folder '(654))))
      (should (string-match-p "luaotfload.add_fallback(\"tibfallback\"" s))
      (should (string-match-p "setmainfont{Libertinus Serif}\\[RawFeature={fallback=tibfallback}\\]" s)))))

(ert-deftest tibetan-discussion-paper-metre-pauses ()
  "§5.65: the reMarkable paper shows a Sanskrit verse with its pauses
and the metre line (Carsten reads from it in class)."
  (tibetan-handout-test--with-sa-verse
    (let ((s (tibetan-discussion-paper-render
              (list :title "T" :scope "Satz 1002" :source-name "Verse-sa.org"
                    :date "2026-10-21"
                    :sentences (list (tibetan-discussion-paper--sentence-data 1002 vsrc))))))
      (should (string-match-p "Śloka (Anuṣṭubh) · keine feste Binnenpause" s))
      (should (string-match-p "evāsmi | śrīmatkalyāṇavāridheḥ" s)))))

(ert-deftest tibetan-discussion-paper-render-gaps ()
  "Without Concept Notes / discussion / comparison the sections say
so instead of vanishing silently — except the comparison, which is
only shown when the source is bilingual (a comparison exists)."
  (tibetan-handout-test--with-corpus
    (let ((file (tibetan-sentence--filepath 654 folder src)))
      (with-temp-file file
        (insert (replace-regexp-in-string
                 "\\*\\* Discussion\\(?:.\\|\n\\)*?\\(\\* Working Translation\\)" "\\1"
                 (replace-regexp-in-string "- \\*\\*kun rdzob\\*\\* — note\\.\n" ""
                                           (tibetan-paper-test--read file))))))
    (let ((s (tibetan-paper-test--render src folder '(654))))
      (should (string-match-p "^\\*\\* Begriffe$" s))
      (should (string-match-p "luecke(noch keine Begriffe" s))
      (should (string-match-p "rubrik(Vokabular)" s))
      (should (string-match-p "^\\*\\* Diskussion$" s))
      (should (string-match-p "luecke(noch keine Diskussion" s))
      (should-not (string-match-p "Sanskrit-Vergleich" s)))))

(ert-deftest tibetan-discussion-paper-render-parses-as-org ()
  (tibetan-handout-test--with-corpus
    (tibetan-paper-test--add-comparison (tibetan-sentence--filepath 654 folder src))
    (let ((s (tibetan-paper-test--render src folder)))
      (with-temp-buffer
        (insert s)
        (org-mode)
        (let ((titles (org-element-map (org-element-parse-buffer) 'headline
                        (lambda (h) (org-element-property :raw-value h)))))
          (should (member "Satz 654 (Seg. 1970–1971)" titles))
          (should (member "Begriffe" titles))
          (should (member "Sanskrit-Vergleich" titles))
          (should (member "Diskussion" titles)))))))

;; ----------------------------------------------------------------------------
;; File, upload, command
;; ----------------------------------------------------------------------------

(ert-deftest tibetan-discussion-paper-output-names ()
  (tibetan-handout-test--with-corpus
    (should (string-suffix-p "handouts/rgyan-disk-par-220.org"
                             (tibetan-discussion-paper--output-file
                              (list :source src :sent-nums '(654 655) :lopez 220))))
    (should (string-suffix-p "handouts/rgyan-disk-sent-654.org"
                             (tibetan-discussion-paper--output-file
                              (list :source src :sent-nums '(654)))))))

(ert-deftest tibetan-discussion-paper-build-guarded ()
  "Build writes the generated .org; a hand-owned file at the target
is never overwritten."
  (tibetan-handout-test--with-corpus
    (let* ((scope (list :source src :sent-nums '(654) :label "Satz 654"))
           (out (tibetan-discussion-paper-build scope "2026-10-20")))
      (should (file-exists-p out))
      (should (string-prefix-p tibetan-discussion-paper-generated-marker
                               (tibetan-paper-test--read out)))
      (with-temp-file out (insert "# meine Datei\n"))
      (should-error (tibetan-discussion-paper-build scope) :type 'user-error))))


(ert-deftest tibetan-discussion-paper-doc-name-ascii ()
  "Carsten (20.10.): the folder `bDen pa gnyis la 'jug pa
(Satyadvayāvatāra)' could not be opened on the reMarkable (the desktop
app shows it) — names on the device carry no diacritics, apostrophes,
brackets, umlauts or dashes beyond ASCII."
  (should (equal "bDen pa gnyis la jug pa Satyadvayavatara"
                 (tibetan-discussion-paper--doc-name
                  "bDen pa gnyis la 'jug pa (Satyadvayāvatāra)")))
  (should (equal "Saetze 2-3" (tibetan-discussion-paper--doc-name "Sätze 2–3")))
  (should (equal "Satz 1" (tibetan-discussion-paper--doc-name "Satz 1")))
  (should (equal "Par. 220" (tibetan-discussion-paper--doc-name "§220")))
  (should (equal "Tantraloka, Kapitel 1 vijnanabhid"
                 (tibetan-discussion-paper--doc-name "Tantrāloka, Kapitel 1 (vijñānabhid)")))
  (should (equal "Chos mngon pa kun las btus pa Abhidharmasamuccaya mit Bhasya, Teil zu Heft 1"
                 (tibetan-discussion-paper--doc-name
                  "Chos mngon pa kun las btus pa (Abhidharmasamuccaya) mit Bhāṣya, Teil zu Heft ⑴"))))

(ert-deftest tibetan-discussion-paper-upload ()
  "Upload (Carsten 20.10.: Ordner je Werk, der Satz als Dateiname): ONE
rmapi session with mkdir base, mkdir `/Reading/<Werk>', put
`<Satz>.pdf' there; when the name is taken a second session puts a
time-stamped copy — notes on the earlier document are never
overwritten; any other failure is reported; without rmapi a message,
no error, nil."
  (let ((sessions '()) (reply "uploading: [x]...\nOK\n")
        (pdf (make-temp-file "paper-" nil ".pdf")))
    (unwind-protect
        (cl-letf (((symbol-function 'tibetan-discussion-paper--rmapi-session)
                   (lambda (commands cb)
                     (push (mapcar (lambda (c)
                                     (if (equal (car c) "put")
                                         (list "put" (file-name-nondirectory (cadr c)) (caddr c))
                                       c))
                                   commands)
                           sessions)
                     (funcall cb 0 reply) t))
                  ((symbol-function 'executable-find)
                   (lambda (name &rest _) (and (equal name "rmapi") "/usr/bin/rmapi"))))
          (let ((tibetan-discussion-paper-remarkable-folder "Reading"))
            (should (tibetan-discussion-paper--upload pdf "Klu sgrub dgongs rgyan" "Satz 654"))
            (should (equal '((("mkdir" "/Reading")
                              ("mkdir" "/Reading/Klu sgrub dgongs rgyan")
                              ("put" "Satz 654.pdf" "/Reading/Klu sgrub dgongs rgyan")))
                           sessions))
            ;; Name taken → second session with time stamp.
            (setq sessions '() reply "ERROR: entry already exists\n")
            (should (tibetan-discussion-paper--upload pdf "Rgyan" "Sätze 24–28"))
            (should (= 2 (length sessions)))
            (should (equal "Saetze 24-28.pdf" (cadr (caddr (cadr sessions)))))
            (should (string-match-p "\\`Saetze 24-28 20[0-9-]\\{8\\} [0-9]\\{4\\}\\.pdf\\'"
                                    (cadr (caddr (car sessions)))))
            ;; 429 → reported, no retry loop.
            (setq sessions '() reply "uploading: [x]...ERROR: request failed with status 429\n")
            (should (tibetan-discussion-paper--upload pdf "Rgyan" "Satz 1"))
            (should (= 1 (length sessions)))))
      (cl-letf (((symbol-function 'executable-find) (lambda (&rest _) nil)))
        (should-not (tibetan-discussion-paper--upload pdf "Rgyan" "Satz 1")))
      (delete-file pdf))))

(ert-deftest tibetan-discussion-paper-upload-many-one-session ()
  "Bulk upload: ONE session — base folder, each work folder once, then
one put per paper (one token for fifteen papers)."
  (let ((sessions '()) (a (make-temp-file "a-" nil ".pdf")) (b (make-temp-file "b-" nil ".pdf")))
    (unwind-protect
        (cl-letf (((symbol-function 'tibetan-discussion-paper--rmapi-session)
                   (lambda (commands cb) (push commands sessions) (funcall cb 0 "uploading: [a]...\nOK\nuploading: [b]...\nOK\nuploading: [c]...\nOK\n") t))
                  ((symbol-function 'executable-find)
                   (lambda (name &rest _) (and (equal name "rmapi") "/usr/bin/rmapi"))))
          (let* ((tibetan-discussion-paper-remarkable-folder "Reading")
                 (cmds (tibetan-discussion-paper-upload-many
                        (list (list a "Werk A" "Satz 1") (list a "Werk A" "Satz 2")
                              (list b "Werk B (ā)" "Satz 1")))))
            (should (= 1 (length sessions)))
            (should (equal '(("mkdir" "/Reading") ("mkdir" "/Reading/Werk A")
                             ("mkdir" "/Reading/Werk B a"))
                           (cl-subseq cmds 0 3)))
            (should (equal '("Satz 1.pdf" "Satz 2.pdf" "Satz 1.pdf")
                           (mapcar (lambda (c) (file-name-nondirectory (cadr c)))
                                   (cl-subseq cmds 3))))
            (should (equal "/Reading/Werk B a" (caddr (nth 5 cmds))))))
      (delete-file a) (delete-file b))))

(ert-deftest tibetan-discussion-paper-rmapi-script-and-quoting ()
  "The session script quotes arguments with spaces or brackets and ends
with exit."
  (should (equal "mkdir /Reading\nmkdir \"/Reading/Werk A\"\nput \"/tmp/Satz 1.pdf\" \"/Reading/Werk A\"\nexit\n"
                 (tibetan-discussion-paper--rmapi-script
                  '(("mkdir" "/Reading") ("mkdir" "/Reading/Werk A")
                    ("put" "/tmp/Satz 1.pdf" "/Reading/Werk A"))))))

(ert-deftest tibetan-discussion-paper-rmapi-serial ()
  "Bug (20.10.2026, erster echter Upload): parallel rmapi processes hit
the token rate limit (429) — sessions run ONE at a time through a
queue: the next starts when the previous process has finished."
  (let ((started '()) (finish nil))
    (cl-letf (((symbol-function 'tibetan-discussion-paper--rmapi-session-start)
               (lambda (commands done) (push commands started) (setq finish done) t)))
      (setq tibetan-discussion-paper--rmapi-queue nil
            tibetan-discussion-paper--rmapi-running nil)
      (let ((results '()))
        (dolist (c '((("mkdir" "/A")) (("mkdir" "/B")) (("put" "x" "/A"))))
          (tibetan-discussion-paper--rmapi-session c (lambda (code _out) (push (cons c code) results))))
        (should (equal '((("mkdir" "/A"))) started))
        (funcall finish 0 "")                  ; first done → second starts
        (should (equal '((("mkdir" "/B")) (("mkdir" "/A"))) started))
        (should (equal '(((("mkdir" "/A")) . 0)) results))
        (funcall finish 1 "429")
        (funcall finish 0 "")
        (should (= 3 (length started)))
        (should (equal 1 (cdr (assoc '(("mkdir" "/B")) results))))
        (should-not tibetan-discussion-paper--rmapi-running)))))

(ert-deftest tibetan-discussion-paper-command ()
  "C-c u D: in the source, C-u → only the .org (opened, no export,
no upload); without prefix → build, export, open the PDF, upload.
IN a generated paper the edited file is exported and uploaded as it
is."
  (tibetan-handout-test--with-corpus
    (let ((exported nil) (opened nil) (uploaded nil) (where nil)
          (tibetan-discussion-paper-upload t)
          (org-file (expand-file-name "handouts/rgyan-disk-par-220.org" dir)))
      (cl-letf (((symbol-function 'tibetan-handout-export-pdf)
                 (lambda (org) (setq exported org)
                   (concat (file-name-sans-extension org) ".pdf")))
                ((symbol-function 'tibetan-discussion-paper--upload)
                 (lambda (pdf title label)
                   (setq uploaded pdf where (cons title label)) t))
                ((symbol-function 'org-open-file)
                 (lambda (f &rest _) (setq opened f))))
        (tibetan-handout-test--in-source
          (goto-char (point-min))
          (re-search-forward "^\\*\\* Section §220")
          (save-window-excursion
            (tibetan-discussion-paper '(4))
            (should (file-exists-p org-file))
            (should-not exported)
            (should-not uploaded)
            (should (equal (file-truename org-file)
                           (file-truename (buffer-file-name))))
            (goto-char (point-max))
            (insert "\nMEINE KÜRZUNG\n")
            (tibetan-discussion-paper nil)
            (should (equal (file-truename org-file) (file-truename exported)))
            (should (string-suffix-p "rgyan-disk-par-220.pdf" uploaded))
            (should (string-match-p "MEINE KÜRZUNG" (tibetan-paper-test--read org-file)))
            (set-buffer-modified-p nil)
            (kill-buffer))
          (setq exported nil opened nil uploaded nil)
          (tibetan-discussion-paper nil)
          (should (string-suffix-p "rgyan-disk-par-220.org" exported))
          (should (string-suffix-p "rgyan-disk-par-220.pdf" opened))
          (should (string-suffix-p "rgyan-disk-par-220.pdf" uploaded))
          ;; Ordner = Werktitel, Dokument = Scope-Label
          (should (equal '("Klu sgrub dgongs rgyan" . "§220") where)))))))


;; ----------------------------------------------------------------------------
;; Auto paper after C-c u G (§5.64, Carsten 20.10.: „C-c u D kann gleich an
;; C-c u G mit angehängt werden")
;; ----------------------------------------------------------------------------

(ert-deftest tibetan-discussion-paper-complete-p ()
  "A sentence is paper-ready when its translation is there, the
discussion is there or no reference translation asks for one, and
the Sanskrit comparison is there or no parallel source asks for one."
  (tibetan-handout-test--with-corpus
    (let ((file (tibetan-sentence--filepath 654 folder src)))
      (should (tibetan-discussion-paper--complete-p file))
      ;; Discussion removed, Lopez reference still there → waiting.
      (let ((orig (tibetan-paper-test--read file)))
        (with-temp-file file
          (insert (replace-regexp-in-string
                   "\\*\\* Discussion\\(?:.\\|\n\\)*?\\(\\* Working Translation\\)" "\\1" orig)))
        (should-not (tibetan-discussion-paper--complete-p file))
        ;; … and without the Lopez reference → complete again.
        (with-temp-file file
          (insert (replace-regexp-in-string
                   "\\*\\* Lopez 2006\nLOPEZ POISON TEXT\n\n" ""
                   (tibetan-paper-test--read file))))
        (should (tibetan-discussion-paper--complete-p file)))
      ;; No analysis file / no translation → not complete.
      (should-not (tibetan-discussion-paper--complete-p
                   (tibetan-sentence--filepath 655 folder src))))))

(defmacro tibetan-paper-test--with-auto-stubs (&rest body)
  "Stub export, upload and open; bind BUILT (labels of exported
papers), UPLOADED, OPENED."
  (declare (indent 0))
  `(let ((built '()) (uploaded '()) (opened '())
         (tibetan-discussion-paper-upload t))
     (cl-letf (((symbol-function 'tibetan-handout-export-pdf)
                (lambda (org)
                  (push (cdr (tibetan-discussion-paper--title-and-label org)) built)
                  (concat (file-name-sans-extension org) ".pdf")))
               ((symbol-function 'tibetan-discussion-paper--upload)
                (lambda (pdf _title label) (push label uploaded) t))
               ((symbol-function 'org-open-file)
                (lambda (f &rest _) (push f opened))))
       (ignore built uploaded opened)
       ,@body)))

(ert-deftest tibetan-discussion-paper-auto-after-fetch ()
  "C-c u G ends with the paper: sentences of the scope that needed no
request get their one-sentence paper (\"Satz N\") at once — built,
uploaded, NOT opened; sentences with a request in flight are
remembered and get it when their last expected part lands; the
hand-fired fetch never builds when `tibetan-discussion-paper-auto'
is off."
  (tibetan-handout-test--with-corpus
    (tibetan-paper-test--with-auto-stubs
      (clrhash tibetan-discussion-paper--pending)
      (let ((tibetan-discussion-paper-auto t)
            ;; The corpus lives in a temp dir — the fixture guard
            ;; (`--temp-source-p') would veto; pretend it is elsewhere.
            (temporary-file-directory "/var/empty/")
            (file (tibetan-sentence--filepath 654 folder src)))
        ;; 654 complete, 655 has no file: fetch creates 655's skeleton
        ;; and fires it (stub) → 654 paper now, 655 pending.
        (cl-letf (((symbol-function 'tibetan-cascade--fire-sentence)
                   (lambda (s &rest _) (if (= 655 (plist-get s :sent-num)) 'fired nil))))
          (tibetan-cascade-fetch (list :source src :sent-nums '(654 655) :label "§220")))
        (should (equal '("Satz 654") built))
        (should (equal '("Satz 654") uploaded))
        (should-not opened)
        (should (gethash (file-truename (tibetan-sentence--filepath 655 folder src))
                         tibetan-discussion-paper--pending))
        (should-not (gethash (file-truename file) tibetan-discussion-paper--pending))
        ;; A landing in 655 while it is incomplete: nothing.
        (let ((f655 (tibetan-sentence--filepath 655 folder src)))
          (tibetan-discussion-paper--maybe-build f655)
          (should (equal '("Satz 654") built))
          ;; Make 655 complete (translation; no refs, no parallel) → the
          ;; landing hook builds it once and forgets it.
          (with-temp-file f655
            (insert (replace-regexp-in-string
                     "\\[Requesting translation\\.\\.\\.\\]" "Satz 655 übersetzt."
                     (tibetan-paper-test--read f655))))
          (tibetan-discussion-paper--maybe-build f655)
          (should (equal '("Satz 655" "Satz 654") built))
          (tibetan-discussion-paper--maybe-build f655)
          (should (equal '("Satz 655" "Satz 654") built))))
      ;; Switched off: nothing.
      (setq built '() uploaded '())
      (let ((tibetan-discussion-paper-auto nil))
        (cl-letf (((symbol-function 'tibetan-cascade--fire-sentence)
                   (lambda (&rest _) nil)))
          (tibetan-cascade-fetch (list :source src :sent-nums '(654) :label "Satz 654")))
        (should-not built)))))

(ert-deftest tibetan-discussion-paper-auto-skips-temp-sources ()
  "26.10.: a test fixture (title `D', temp dir) ran C-c u G in an
interactive Emacs — the auto paper uploaded `Reading/D/Satz 4' to the
device.  Sources under `temporary-file-directory' never get an auto
paper, whatever the switch says."
  (tibetan-handout-test--with-corpus
    (tibetan-paper-test--with-auto-stubs
      (clrhash tibetan-discussion-paper--pending)
      (let ((tibetan-discussion-paper-auto t))
        (cl-letf (((symbol-function 'tibetan-cascade--fire-sentence)
                   (lambda (&rest _) nil)))
          (tibetan-cascade-fetch (list :source src :sent-nums '(654) :label "Satz 654")))
        (should-not built)
        (should-not uploaded)
        (should (= 0 (hash-table-count tibetan-discussion-paper--pending)))
        ;; The explicit command still works on such a source.
        (should (tibetan-discussion-paper-auto-build
                 (tibetan-discussion-paper--sentence-scope src 654)))))))

(ert-deftest tibetan-discussion-paper-auto-skips-unchanged ()
  "A paper whose content did not change since the last build is not
uploaded again (the date line alone does not count); a change in the
sentence file does."
  (tibetan-handout-test--with-corpus
    (tibetan-paper-test--with-auto-stubs
      (let ((scope (list :source src :sent-nums '(654) :label "Satz 654"))
            (file (tibetan-sentence--filepath 654 folder src)))
        (should (tibetan-discussion-paper-auto-build scope "2026-10-20"))
        (should-not (tibetan-discussion-paper-auto-build scope "2026-10-21"))
        (should (equal '("Satz 654") uploaded))
        (with-temp-file file
          (insert (replace-regexp-in-string "Durch den Verstand gesehen," "Durch den Verstand erkannt,"
                                            (tibetan-paper-test--read file))))
        (should (tibetan-discussion-paper-auto-build scope "2026-10-21"))
        (should (equal '("Satz 654" "Satz 654") uploaded))))))

(provide 'tibetan-discussion-paper-test)
;;; tibetan-discussion-paper-test.el ends here
