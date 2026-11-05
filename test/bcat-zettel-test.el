;;; bcat-zettel-test.el --- Tests for the concept notes and the zettels  -*- lexical-binding: t; -*-

;;; Commentary:
;; Carsten 5.11.2026: „concept Notes sollten doch eigentlich immer auch in
;; Zetteln repräsentiert werden“ — the terms of the concept notes link
;; to their zettels, and the passage goes into the zettel as a marked
;; Beleg (automatically: „ja, bitte“).  Index and zettels are fixtures
;; in a temp directory; the real Zettelkasten is never touched.

;;; Code:

(require 'ert)
(require 'cl-lib)

(let ((root (expand-file-name ".." (file-name-directory
                                    (or load-file-name buffer-file-name)))))
  (dolist (d '("persist" "core" "analysis" "philology" "data" "config" "doc-prep" "test"))
    (add-to-list 'load-path (expand-file-name d root))))

(require 'org)
(require 'bcat-zettel)
(require 'bcat-sentence-test)

(defconst bcat-zettel-test--kala
  (concat ":PROPERTIES:\n:ID: 20261027T135714\n:SANSKRIT: kalā\n:END:\n"
          "#+title: kalā\n\n/kalā/ · Teil, Kraft\n\n"
          "* Konzept\nKraft.\n\n"
          "* Belegstellen in der Literatur\n"
          "- [cite:@Baeumer2008 p. 215] „kalā: die Energie …“\n"
          "- TĀ 1.78: /kalātattva…/ — von Hand\n\n"
          "* Abgrenzungen\n- kañcuka\n\n* Verweise\n"))

(defconst bcat-zettel-test--visarga
  (concat ":PROPERTIES:\n:ID: 20261016T140000\n:END:\n"
          "#+title: visarga\n\n* Konzept\nEmission.\n\n* Verweise\n# /term\n"))

(defmacro bcat-zettel-test--with-kasten (&rest body)
  "Run BODY with a fixture Zettelkasten (KASTEN, kalā, visarga, a stub
for niyantraṇa) and its index loaded."
  (declare (indent 0))
  `(let* ((kasten (make-temp-file "bcat-kasten-" t))
          (index (expand-file-name "index.json" kasten)))
     (unwind-protect
         (progn
           (with-temp-file (expand-file-name "20261027T135714--kala.org" kasten)
             (insert bcat-zettel-test--kala))
           (with-temp-file (expand-file-name "20261016T140000--visarga.org" kasten)
             (insert bcat-zettel-test--visarga))
           (with-temp-file index
             (insert (json-encode
                      `((generated . "x") (zk_dir . ,kasten) (count . 3)
                        (entries . [((id . "20261027T135714") (file . "20261027T135714--kala.org")
                                     (title . "kalā") (kind . "concept") (stub . :json-false)
                                     (keys . ["kala" "vimalakala"]))
                                    ((id . "20261016T140000") (file . "20261016T140000--visarga.org")
                                     (title . "visarga") (kind . "concept") (stub . :json-false)
                                     (keys . ["visarga"]))
                                    ((id . "20261001T000000") (file . "20261001T000000--niyantrana.org")
                                     (title . "niyantraṇa") (kind . "concept") (stub . t)
                                     (keys . ["niyantrana"]))])))))
           (let ((tibetan-zettel-highlight-index-file index)
                 (tibetan-zettel-highlight--index nil)
                 (tibetan-zettel-highlight--entries nil)
                 (tibetan-zettel-highlight--index-mtime nil)
                 (tibetan-zettel-highlight--zk-dir nil)
                 (bcat-zettel-link-concepts t))
             ,@body))
       (delete-directory kasten t))))

(defun bcat-zettel-test--read (file)
  (with-temp-buffer (insert-file-contents file) (buffer-string)))

(ert-deftest bcat-zettel-link ()
  "A term of a concept note (in bold) with a zettel links to it —
denote:ID, the Kasten's own links, RET leads to the Research instance;
case and diacritics do not matter; a term without zettel stays as it
is; linking again changes nothing."
  (bcat-zettel-test--with-kasten
    (let* ((text "*kalā* — power; ideation (*vimarśa*); *Visarga* the emission.")
           (linked (bcat-zettel-link text)))
      (should (equal (concat "*[[denote:20261027T135714][kalā]]* — power; ideation (*vimarśa*); "
                             "*[[denote:20261016T140000][Visarga]]* the emission.")
                     linked))
      (should (equal linked (bcat-zettel-link linked)))
      (should (equal '("20261027T135714" "20261016T140000") (bcat-zettel-ids linked))))))

(ert-deftest bcat-zettel-link-without-index ()
  "No index (or linking switched off): the notes stay as they are."
  (let ((tibetan-zettel-highlight-index-file "/nonexistent/bcat-zettel-index.json")
        (tibetan-zettel-highlight--index nil)
        (bcat-zettel-link-concepts t))
    (should (equal "*kalā* — power." (bcat-zettel-link "*kalā* — power."))))
  (bcat-zettel-test--with-kasten
    (let ((bcat-zettel-link-concepts nil))
      (should (equal "*kalā* — power." (bcat-zettel-link "*kalā* — power."))))))

(ert-deftest bcat-zettel-fetch-links-concepts ()
  "C-c u G: the concept notes land linked in the sentence file; the PDF
shows the term without the link (the reMarkable cannot open the Kasten)."
  (bcat-zettel-test--with-kasten
    (bcat-sentence-test--with-sa-source
      (bcat-sentence-test--create-n src 1007)
      (bcat-sentence-test--with-stubs (bcat-sentence-test--sa-answer)
        (bcat-sentence-test--fetch-n src 1007))
      (let ((file (tibetan-sentence--filepath 1007 folder src)))
        (should (string-match-p (regexp-quote "⟦685⟧ *[[denote:20261001T000000][niyantraṇa]]*")
                                (bcat-sentence-test--text-of file)))
        (with-temp-buffer
          (insert "⟦685⟧ *[[denote:20261001T000000][niyantraṇa]]* — control.\n")
          (bcat-export--plain-zettel-links)
          (should (equal "⟦685⟧ *niyantraṇa* — control.\n" (buffer-string))))))))

(provide 'bcat-zettel-test)
;;; bcat-zettel-test.el ends here
