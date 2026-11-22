;;; bcat-survey-test.el --- Tests for the survey of the sources on a passage  -*- lexical-binding: t; -*-

;;; Commentary:
;; Carsten 22.11.2026: „Wenn es mehrere Quellen gibt wäre so ein
;; Übersichtsdokument noch gut.  Was verschiebt sich z.B. im bhasya, bei
;; Sthiramati usw.  Welche unterschiedlichen Gesichtspunkte betrachten
;; die Übersetzungen“ — „im Tantraloka z.B. auch Sanderson betont mehr
;; das, Dyzkowki das“ — „das Tibetische könnte, im Gegensatz zum
;; Sanskrit, die Übersetzung/Interpretation zu lassen“.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((root (expand-file-name ".." (file-name-directory
                                    (or load-file-name buffer-file-name)))))
  (dolist (d '("persist" "core" "analysis" "philology" "data" "config" "doc-prep" "test"))
    (add-to-list 'load-path (expand-file-name d root))))

(require 'org)
(require 'bcat-survey)
(require 'bcat-export)
(require 'bcat-sentence-test)
(require 'bcat-commentary-test)

(defconst bcat-survey-test--answer
  (concat
   "{\"gist\": \"Zwei Vergleiche: wie der Mond im zerbrochenen Gefäß, so der Buddha in den Wesen.\", "
   "\"layers\": ["
   "{\"name\": \"Vers\", \"place\": \"Skt. 92–93\", \"key\": \"zwei Vergleiche\", \"shift_key\": \"—\", "
   "\"says\": \"Der Vers stellt das Bild hin.\", \"shift\": \"—\"}, "
   "{\"name\": \"Bhāṣya\", \"place\": \"Satz 65\", \"key\": \"Beweis durch Beispiel\", \"shift_key\": \"Logik kommt herein\", "
   "\"says\": \"Das Bhāṣya nennt den Vers einen Beweis durch ein Beispiel (/dṛṣṭānta/).\", "
   "\"shift\": \"Aus dem Bild wird ein Argument.\"}], "
   "\"layers_point\": \"Der Vers vergleicht, das Bhāṣya beweist.\", "
   "\"wording\": [\"/na dṛśyate/ → /mi snang/: das Tibetische lässt „erscheint nicht“ zu.\"], "
   "\"voices\": [{\"name\": \"Thurman 2004\", \"from\": \"Sanskrit\", \"key\": \"„cannot be seen“\", "
   "\"text\": \"Betont das Nicht-sehen-Können.\"}], "
   "\"voices_line\": \"Nur eine Stimme.\", "
   "\"questions\": [\"Ist das Gefäß der Geist oder das Wesen?\"]}")
  "A survey as Claude answers it.")

(defun bcat-survey-test--answers (kind file)
  "The fixture's answers, the survey's among them."
  (if (equal kind "bcat-survey")
      bcat-survey-test--answer
    (bcat-commentary-test--bhasya-answer kind file)))

(defmacro bcat-survey-test--with-verse (&rest body)
  "BODY in the MSA trio with the bhāṣya named as a commentary, verse
IX.16 fetched (verse pair, bhāṣya, Sthiramati); FILE its Sanskrit
sentence file."
  (declare (indent 0))
  `(bcat-commentary-test--with-bhasya
     (bcat-commentary-test--with-file-stubs #'bcat-commentary-test--bhasya-answer
       (bcat-sentence-test--fetch-n src 36))
     (let ((file (tibetan-sentence--filepath 36 folder src)))
       ,@body)))

(ert-deftest bcat-survey-material ()
  "What the survey is drawn from — only what is analysed already: the
passage in both languages with the class's translation, every layer as
the reading page has it (the bhāṣya and the commentator sentence by
sentence with their translations), the published translations word for
word.  No machine translation, no links."
  (bcat-survey-test--with-verse
    (let* ((material (bcat-survey-material file))
           (layers (plist-get material :layers)))
      (should (equal "MSA IX.16" (plist-get material :label)))
      (should (equal '("Vers" "Bhāṣya" "Sthiramati") (mapcar #'car layers)))
      ;; the passage: Sanskrit and Tibetan
      (should (string-match-p "yathodabhājane bhinne candrabimbaṃ na dṛśyate /" (nth 1 (assoc "Vers" layers))))
      (should (string-match-p "ji ltar chu yi snod chag na/ /" (nth 1 (assoc "Vers" layers))))
      ;; the layers sentence by sentence, with what they translate to
      (should (string-match-p "\\[65\\]\\(?:.*\n\\)*?.*By this fifth verse he proves by an example"
                              (nth 1 (assoc "Bhāṣya" layers))))
      (should (string-match-p "\\[258\\]" (nth 1 (assoc "Sthiramati" layers))))
      (should (string-match-p "To show the example" (nth 1 (assoc "Sthiramati" layers))))
      ;; the published translation, as it stands in the file
      (should (equal '("Thurman 2004") (mapcar #'car (plist-get material :references))))
      (should (string-match-p "Just as the reflection of the moon cannot be seen in a broken water pot"
                              (cdr (car (plist-get material :references)))))
      (should (equal '("Vers" "Bhāṣya" "Sthiramati" "Thurman 2004") (bcat-survey-sources material)))
      ;; where it all stands, for the last part of the survey
      (should (equal '("MSA IX.16" "MSA IX.16 Bhāṣya" "MSA IX.16–17 Sthiramati") (plist-get material :documents)))
      (let ((user (bcat-survey-user-prompt material)))
        (should (string-match-p "\\`Passage: MSA IX\\.16\n" user))
        (should (string-match-p "^== Bhāṣya ==$" user))
        (should (string-match-p "^== Published translations ==\n\\*Thurman 2004:\\* " user))
        (should-not (string-match-p "DharmaMitra\\|\\[\\[file:" user))
        ;; the transliteration is enough for a request: no line twice,
        ;; once in the original script
        (should-not (string-match-p "[\u0900-\u097F\u0F00-\u0FFF]" user))
        ;; … and the mark of a line that takes up the verse goes over to it
        (should (string-match-p "^\\*ji ltar chu yi snod chag na/ /\\*$" user))))))

(ert-deftest bcat-survey-render ()
  "The survey as an org file: the passage in one sentence; the layers —
a table of key words, then what each says and what shifts; Sanskrit and
Tibetan; the voices — table, then each; the questions; where it all
stands.  A part without content is left out.  The file says that it is
a draft and carries the tool's stamp."
  (let* ((material (list :label "MSA IX.16" :lead-name "sent-036-msa.org"
                         :documents '("MSA IX.16" "MSA IX.16 Bhāṣya")))
         (text (bcat-survey-render (bcat-survey--read bcat-survey-test--answer) material)))
    (should (string-match-p "\\`#\\+TITLE: MSA IX\\.16 — Überblick\n" text))
    (should (string-match-p (regexp-quote "#+CAT_SURVEY_OF: [[file:sent-036-msa.org][MSA IX.16]]") text))
    (should (bcat-survey-untouched-p text))
    ;; for its PDF: the title, but neither author nor date; its tables
    ;; are plain ones — no row of them is a gloss
    (should (string-match-p "^#\\+OPTIONS: toc:nil num:nil author:nil date:nil$" text))
    (should (= 2 (with-temp-buffer (insert text)
                                   (how-many "^#\\+ATTR_LATEX: :font \\\\bcatplain\n| " (point-min) (point-max)))))
    (should (string-match-p "^\\* Die Stelle in einem Satz\nZwei Vergleiche: wie der Mond" text))
    ;; (the tables are aligned)
    (should (string-match-p "^| Vers +| zwei Vergleiche +| — +|\n| Bhāṣya +| Beweis durch Beispiel +| Logik kommt herein +|$" text))
    (should (string-match-p (regexp-quote "** Bhāṣya (Satz 65)\nDas Bhāṣya nennt den Vers einen Beweis durch ein Beispiel (/dṛṣṭānta/).\n\n*Verschiebt sich:* Aus dem Bild wird ein Argument.\n") text))
    (should (string-match-p "^\\*Worauf es ankommt:\\* Der Vers vergleicht, das Bhāṣya beweist\\.$" text))
    (should (string-match-p "^\\* Sanskrit und Tibetisch — was der Wortlaut zulässt\n- /na dṛśyate/ → /mi snang/" text))
    (should (string-match-p "^| Thurman 2004 +| Sanskrit +| „cannot be seen“ +|$" text))
    (should (string-match-p "^- \\*Thurman 2004:\\* Betont das Nicht-sehen-Können\\.$" text))
    (should (string-match-p "^\\*Die Linie:\\* Nur eine Stimme\\.$" text))
    (should (string-match-p "^\\* Fragen fürs Reading\n1\\. Ist das Gefäß der Geist oder das Wesen\\?$" text))
    (should (string-match-p "^\\* Wo es steht\n- „MSA IX\\.16“\n- „MSA IX\\.16 Bhāṣya“$" text))
    ;; a hand's change: no longer the tool's draft
    (should-not (bcat-survey-untouched-p (replace-regexp-in-string "Zwei Vergleiche" "Zwei Bilder" text))))
  ;; one language, no voices: those parts are not there
  (let ((text (bcat-survey-render
               (bcat-survey--read "{\"gist\": \"G.\", \"layers\": [{\"name\": \"Vers\", \"place\": \"1\", \"key\": \"k\", \"shift_key\": \"—\", \"says\": \"S.\", \"shift\": \"—\"}], \"layers_point\": \"\", \"wording\": [], \"voices\": [], \"voices_line\": \"\", \"questions\": []}")
               (list :label "Satz 9" :lead-name "sent-009-x.org" :documents '("Satz 9")))))
    (should (string-match-p "^\\* Die Schichten" text))
    (should-not (string-match-p "Sanskrit und Tibetisch\\|Stimmen\\|Worauf es ankommt\\|Fragen fürs Reading\\|Verschiebt sich" text))))

(ert-deftest bcat-survey-fetch ()
  "C-c u Q on a verse: ONE request that reads what is analysed — its
answer becomes the survey beside the sentence file (survey-036-msa.org)
and is kept, so that the survey is set anew without a request as long
as the material is the same.  A survey someone worked on is not
overwritten: the new draft goes beside it."
  (bcat-survey-test--with-verse
    (let ((survey (expand-file-name "survey-036-msa.org" folder)))
      (should (equal survey (bcat-survey-file file)))
      (should (bcat-survey-pending-p file))
      (bcat-commentary-test--with-file-stubs #'bcat-survey-test--answers
        (should (= 1 (bcat-survey-fetch file)))
        (should (equal '("bcat-survey") kinds))
        (should (string-match-p "compare the sources" (car (car prompts))))
        (should (string-match-p "\\`Passage: MSA IX\\.16\n" (cdr (car prompts)))))
      (let ((text (bcat-sentence-test--text-of survey)))
        (should (string-match-p "^#\\+TITLE: MSA IX\\.16 — Überblick$" text))
        (should (string-match-p "^\\*\\* Bhāṣya (Satz 65)$" text))
        (should (string-match-p "^- „MSA IX\\.16–17 Sthiramati“$" text))
        (should (bcat-survey-untouched-p text))
        ;; the answer is kept: set anew without a request
        (should-not (bcat-survey-pending-p file))
        (delete-file survey)
        (bcat-commentary-test--with-file-stubs #'bcat-survey-test--answers
          (should (= 0 (bcat-survey-fetch file)))
          (should-not kinds))
        (should (equal text (bcat-sentence-test--text-of survey)))
        ;; asked for anew: a request
        (should (bcat-survey-pending-p file t))
        ;; worked on by hand: the new draft goes beside it
        (with-temp-file survey (insert (replace-regexp-in-string "Zwei Vergleiche" "Zwei Bilder" text t t)))
        (bcat-commentary-test--with-file-stubs #'bcat-survey-test--answers
          (should (= 1 (bcat-survey-fetch file t))))
        (should (string-match-p "Zwei Bilder" (bcat-sentence-test--text-of survey)))
        (should (string-match-p "Zwei Vergleiche"
                                (bcat-sentence-test--text-of (expand-file-name "survey-036-msa-Entwurf.org" folder))))))))

(ert-deftest bcat-survey-needs-sources ()
  "A passage with nothing beside it — no commentary, no published
translation, no lecture — has no survey: nothing is asked.  One other
source is enough (the text and one published translation)."
  (bcat-sentence-test--with-source
    (bcat-sentence-test--create src)
    (let ((file (tibetan-sentence--filepath 9 folder src)))
      (bcat-sentence-test--with-stubs bcat-survey-test--answer
        ;; the fixture has a published translation beside the text
        (should (= 2 (length (bcat-survey-sources (bcat-survey-material file)))))
        (should (= 1 (bcat-survey-fetch file)))
        (should (= 1 submits))
        ;; the text alone
        (cl-letf (((symbol-function 'bcat-survey-material)
                   (lambda (_file) (list :label "Satz 9" :lead file :layers '(("Text" "brjod du med pa/"))))))
          (should-error (bcat-survey-fetch file t) :type 'user-error)
          (should (= 1 submits)))))))

(ert-deftest bcat-survey-export ()
  "C-c u H of the verse puts the survey on the reMarkable too, as a PDF
of its own beside the others: „MSA IX.16 Überblick“
\(msa-IX.16-ueberblick.pdf) — only when there is one."
  (bcat-survey-test--with-verse
    (bcat-commentary-test--with-export-stubs
      (bcat-export-file file)
      (should-not (cl-find-if (lambda (u) (string-match-p "ueberblick" (car u))) uploaded)))
    (bcat-commentary-test--with-file-stubs #'bcat-survey-test--answers
      (bcat-survey-fetch file))
    (bcat-commentary-test--with-export-stubs
      (bcat-export-file file)
      (should (member '("msa-IX.16-ueberblick.pdf" . "MSA IX.16 Überblick") uploaded))
      (let ((pdf-text (cdr (cl-find-if (lambda (e) (string-match-p "ueberblick" (car e))) exported))))
        (should (string-match-p "^\\* Die Schichten" pdf-text))
        (should-not (string-match-p "CAT_SURVEY_WRITTEN" pdf-text))))))

(ert-deftest bcat-survey-key ()
  "C-c u Q: the survey of the sources („Quellen im Vergleich“)."
  (require 'tibetan-keybindings)
  (should (eq (key-binding (kbd "C-c u Q")) 'bcat-survey)))

(provide 'bcat-survey-test)
;;; bcat-survey-test.el ends here
