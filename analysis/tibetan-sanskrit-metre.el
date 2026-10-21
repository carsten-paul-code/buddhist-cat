;;; tibetan-sanskrit-metre.el --- Sanskrit scansion, metre, recitation pauses -*- lexical-binding: t -*-

;;; Commentary:
;; §5.65 (2026-10-21).  Carsten (Tantrāloka-Reading, nach Satz 1001,
;; Jayarathas erstem Einleitungsvers): „Es geht mit einer Rezitation
;; los, da bin ich mit der Metrik nicht klar gekommen.  Ginge es, dass
;; wir eine Kennzeichnung der Pausen beim Rezitieren reinbekommen?"
;;
;; Pure module over IAST:
;;
;;   SCANSION   `tibetan-metre-syllables' — syllables with weight.
;;              Heavy (G, guru) = long vowel (ā ī ū ṝ ḹ e ai o au), or
;;              anusvāra/visarga, or two+ consonants before the next
;;              vowel (across word boundaries); else light (L, laghu).
;;              Aspirates (kh, bh …) are one consonant; avagraha makes
;;              no syllable; daṇḍas and numbers are no syllables.
;;   METRE      `tibetan-metre-identify' — over the lines (segments) of
;;              ONE verse: the sama metres of the table below, upajāti
;;              mixtures, two ardhasama metres, the śloka, the āryā
;;              family (by morae).  Guard against prose: pāda
;;              boundaries must fall on line boundaries or inside lines
;;              — a line never ends inside a pāda.
;;   PAUSES     `tibetan-metre-mark-segments' — the lines with ` ¦ '
;;              at the caesura (yati) and ` | ' at a pāda end inside a
;;              line.  Inside a word the mark sits at the syllable
;;              boundary, the first consonant of a cluster closing the
;;              syllable (rahas¦yā).
;;   SECTION    `tibetan-metre-section' — the `** Metre' block of a
;;              cascade sentence file: name, pattern, pause rule, the
;;              marked verse, a syllable table (one per verse, columns
;;              shared by all pādas).
;;
;; Patterns and caesurae of the sama metres: Macdonell, A Sanskrit
;; Grammar for Students, App. II (Śālinī 4, Mālinī 8, Śikhariṇī 6,
;; Hariṇī 6/10, Mandākrāntā 4/10, Śārdūlavikrīḍita 12, Sragdharā
;; 7/14; Vasantatilakā, Upajāti, Vaṃśastha … without fixed caesura);
;; Pṛthvī 8, Praharṣiṇī 3, Mattamayūra 4 after the Vṛttaratnākara's
;; definitions.  Nardaṭaka (TĀ 1.1) has no fixed caesura (with yati
;; 7/13 the same pattern is called Kokilaka).  Āryā: caesura after the
;; third foot (12 morae) in each half.

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'ucs-normalize)

;; ============================================================================
;; Scansion
;; ============================================================================

(defconst tibetan-metre--long-vowels '("ai" "au" "ā" "ī" "ū" "ṝ" "ḹ" "e" "o")
  "Long vowels (diphthongs first: longest match).")

(defconst tibetan-metre--short-vowels '("a" "i" "u" "ṛ" "ḷ")
  "Short vowels.")

(defconst tibetan-metre--consonants
  '("kh" "gh" "ch" "jh" "ṭh" "ḍh" "th" "dh" "ph" "bh"
    "k" "g" "ṅ" "c" "j" "ñ" "ṭ" "ḍ" "ṇ" "t" "d" "n" "p" "b" "m"
    "y" "r" "l" "v" "ś" "ṣ" "s" "h")
  "Consonants (aspirates first: ONE consonant each).")

(defconst tibetan-metre--codas '("ṃ" "ṁ" "ḥ")
  "Anusvāra and visarga: they close (and weigh) the syllable.")

(defun tibetan-metre--units (text)
  "TEXT as a list of units (TYPE STRING BEG END); TYPE is `long',
`short', `cons', `coda' or `space'.  Everything else (daṇḍas,
digits, avagraha, punctuation) is skipped."
  (let ((i 0) (n (length text)) units)
    (while (< i n)
      (let* ((two (and (< (1+ i) n) (substring text i (+ i 2))))
             (one (substring text i (1+ i)))
             (hit
              (cond
               ((and two (member two tibetan-metre--long-vowels)) (list 'long two))
               ((and two (member two tibetan-metre--consonants)) (list 'cons two))
               ((member one tibetan-metre--long-vowels) (list 'long one))
               ((member one tibetan-metre--short-vowels) (list 'short one))
               ((member one tibetan-metre--consonants) (list 'cons one))
               ((member one tibetan-metre--codas) (list 'coda one))
               ((string-match-p "[ \t\n-]" one) (list 'space one)))))
        (if hit
            (let ((len (length (cadr hit))))
              (push (list (car hit) (cadr hit) i (+ i len)) units)
              (setq i (+ i len)))
          (setq i (1+ i)))))
    (nreverse units)))

(defun tibetan-metre--nucleus-p (u)
  (memq (car u) '(long short)))

(defun tibetan-metre-syllables (text)
  "The syllables of the IAST TEXT, in order: plists (:text :weight
:beg :end :bnd :space), :weight `G' or `L'; :beg/:end delimit the
syllable in TEXT; :bnd is the position where a pause mark after this
syllable goes, :space non-nil when that position is a space."
  (let* ((text (downcase (ucs-normalize-NFC-string (or text ""))))
         (units (vconcat (tibetan-metre--units text)))
         (nuclei (cl-loop for k from 0 below (length units)
                          when (tibetan-metre--nucleus-p (aref units k))
                          collect k))
         (start (cl-loop for k from 0 below (length units)
                         unless (eq (car (aref units k)) 'space)
                         return (nth 2 (aref units k))))
         out)
    (cl-loop
     for (k next) on nuclei
     do (let* ((between (cl-loop for j from (1+ k)
                                 below (or next (length units))
                                 collect (aref units j)))
               (cons (cl-remove-if-not (lambda (u) (eq (car u) 'cons)) between))
               (spaces (cl-remove-if-not (lambda (u) (eq (car u) 'space)) between))
               (heavy (or (eq (car (aref units k)) 'long)
                          (cl-some (lambda (u) (eq (car u) 'coda)) between)
                          (>= (length cons) 2)
                          ;; a final consonant closes the last syllable
                          (and (null next) cons)))
               (bnd (cond
                     ((null next)
                      ;; last syllable: ends after its consonants/coda
                      (let ((tail (cl-remove-if (lambda (u) (eq (car u) 'space))
                                                between)))
                        (if tail (nth 3 (car (last tail)))
                          (nth 3 (aref units k)))))
                     (spaces (nth 2 (car (last spaces))))
                     ((null cons) (nth 2 (aref units next)))
                     ((= 1 (length cons)) (nth 2 (car cons)))
                     (t (nth 2 (cadr cons)))))
               (next-start (cond ((null next) bnd)
                                 (spaces (nth 3 (car (last spaces))))
                                 (t bnd)))
               (syl-text (replace-regexp-in-string
                          "[^[:alpha:]ṃṁḥ]" "" (substring text start bnd))))
          (push (list :text syl-text :weight (if heavy 'G 'L)
                      :beg start :end bnd :bnd bnd :space (and spaces t))
                out)
          (setq start next-start)))
    (nreverse out)))

(defun tibetan-metre-scheme (text)
  "TEXT's weights as a string of G and L."
  (mapconcat (lambda (s) (symbol-name (plist-get s :weight)))
             (tibetan-metre-syllables text) ""))

;; ============================================================================
;; Metres
;; ============================================================================

(defconst tibetan-metre--sama
  '(("Indravajrā"        "GGLGGLLGLGG"           "ta ta ja ga ga"        nil      upajati-11)
    ("Upendravajrā"      "LGLGGLLGLGG"           "ja ta ja ga ga"        nil      upajati-11)
    ("Śālinī"            "GGGGGLGGLGG"           "ma ta ta ga ga"        (4)      nil)
    ("Rathoddhatā"       "GLGLLLGLGLG"           "ra na ra la ga"        nil      nil)
    ("Svāgatā"           "GLGLLLGLLGG"           "ra na bha ga ga"       nil      nil)
    ("Vaṃśastha"         "LGLGGLLGLGLG"          "ja ta ja ra"           nil      upajati-12)
    ("Indravaṃśā"        "GGLGGLLGLGLG"          "ta ta ja ra"           nil      upajati-12)
    ("Drutavilambita"    "LLLGLLGLLGLG"          "na bha bha ra"         nil      nil)
    ("Bhujaṅgaprayāta"   "LGGLGGLGGLGG"          "ya ya ya ya"           nil      nil)
    ("Toṭaka"            "LLGLLGLLGLLG"          "sa sa sa sa"           nil      nil)
    ("Praharṣiṇī"        "GGGLLLLGLGLGG"         "ma na ja ra ga"        (3)      nil)
    ("Mattamayūra"       "GGGGGLLGGLLGG"         "ma ta ya sa ga"        (4)      nil)
    ("Vasantatilakā"     "GGLGLLLGLLGLGG"        "ta bha ja ja ga ga"    nil      nil)
    ("Mālinī"            "LLLLLLGGGLGGLGG"       "na na ma ya ya"        (8)      nil)
    ("Pṛthvī"            "LGLLLGLGLLLGLGGLG"     "ja sa ja sa ya la ga"  (8)      nil)
    ("Śikhariṇī"         "LGGGGGLLLLLGGLLLG"     "ya ma na sa bha la ga" (6)      nil)
    ("Hariṇī"            "LLLLLGGGGGLGLLGLG"     "na sa ma ra sa la ga"  (6 10)   nil)
    ("Mandākrāntā"       "GGGGLLLLLGGLGGLGG"     "ma bha na ta ta ga ga" (4 10)   nil)
    ("Nardaṭaka"         "LLLLGLGLLLGLLGLLG"     "na ja bha ja ja la ga" nil      nil)
    ("Śārdūlavikrīḍita"  "GGGLLGLGLLLGGGLGGLG"   "ma sa ja sa ta ta ga"  (12)     nil)
    ("Sragdharā"         "GGGGLGGLLLLLLGGLGGLGG" "ma ra bha na ya ya ya" (7 14)   nil))
  "Sama metres: (NAME PATTERN GAṆAS YATI UPAJĀTI-FAMILY).  The last
syllable of every pāda is anceps (not compared).")

(defconst tibetan-metre--ardhasama
  '(("Puṣpitāgrā" "LLLLLLGLGLGG" "LLLLGLLGLGLGG" "na na ra ya / na ja ja ra ga"))
  "Ardhasama metres: (NAME ODD-PĀDA EVEN-PĀDA GAṆAS).")

(defconst tibetan-metre--mora-ardhasama
  '(("Vaitālīya"       6 8 "GLGLG"  "ungerade Pādas 6, gerade 8 Moren + ra la ga")
    ("Aupacchandasika" 6 8 "GLGLGG" "ungerade Pādas 6, gerade 8 Moren + ra ya"))
  "Mora-defined ardhasama metres: (NAME ODD-MORAE EVEN-MORAE TAIL
DESCRIPTION) — a pāda is a stretch of exactly ODD/EVEN-MORAE morae
followed by the fixed TAIL (last syllable anceps).  Viyoginī is a
fixed form of the Vaitālīya; TĀ 1.13 and 1.333 are Aupacchandasika.")

(defconst tibetan-metre--arya-family
  '(((30 . 27) . "Āryā") ((30 . 30) . "Gīti")
    ((27 . 27) . "Upagīti") ((27 . 30) . "Udgīti")
    ((32 . 32) . "Āryāgīti"))
  "Morae of the two half-verses → name.")

(defun tibetan-metre--weights (syls)
  (mapconcat (lambda (s) (symbol-name (plist-get s :weight))) syls ""))

(defun tibetan-metre--match-p (pattern syls)
  "Non-nil when SYLS fit PATTERN, the last syllable anceps."
  (and (= (length pattern) (length syls))
       (string= (substring pattern 0 -1)
                (substring (tibetan-metre--weights syls) 0 -1))))

(defun tibetan-metre--split (syls lengths)
  "SYLS cut into consecutive pieces of LENGTHS, or nil."
  (when (= (apply #'+ lengths) (length syls))
    (let ((rest syls) out)
      (dolist (n lengths (nreverse out))
        (push (cl-subseq rest 0 n) out)
        (setq rest (nthcdr n rest))))))

(defun tibetan-metre--aligned-p (line-ends pada-lengths)
  "Non-nil when every line end (cumulative syllable count) is a pāda
boundary — a line never ends inside a pāda (the prose guard)."
  (let ((bounds (let ((acc 0)) (mapcar (lambda (n) (cl-incf acc n))
                                       pada-lengths))))
    (cl-every (lambda (e) (memq e bounds)) line-ends)))

(defun tibetan-metre--morae (syls)
  "Morae of a half-verse SYLS; its last syllable counts as heavy."
  (+ (cl-loop for s in (butlast syls)
              sum (if (eq (plist-get s :weight) 'G) 2 1))
     (if syls 2 0)))

(defun tibetan-metre--mora-cut (syls n)
  "Number of syllables of SYLS making exactly N morae, or nil."
  (let ((acc 0) (k 0))
    (cl-loop for s in syls
             do (cl-incf acc (if (eq (plist-get s :weight) 'G) 2 1))
             (cl-incf k)
             when (= acc n) return k
             when (> acc n) return nil)))

(defun tibetan-metre--mora-pada-p (syls morae tail)
  "Non-nil when SYLS are a stretch of MORAE morae followed by TAIL
\(a G/L string, its last syllable anceps)."
  (let ((n (length tail)))
    (and (> (length syls) n)
         (let ((head (butlast syls n))
               (end (last syls n)))
           (and (= morae (cl-loop for s in head
                                  sum (if (eq (plist-get s :weight) 'G) 2 1)))
                (tibetan-metre--match-p tail end))))))

(defun tibetan-metre--mora-ardhasama (syls ends)
  "The mora-defined ardhasama metre of SYLS (line ends ENDS), or nil."
  (cl-loop
   for (name odd even tail desc) in tibetan-metre--mora-ardhasama
   for found =
   (catch 'found
     (let ((n (length syls)) (tl (length tail)))
       (cl-loop
        for a from (1+ tl) to (+ tl odd) do
        (cl-loop
         for b from (1+ tl) to (+ tl even) do
         (cl-loop
          for c from (1+ tl) to (+ tl odd)
          for d = (- n a b c)
          when (> d tl) do
          (let* ((lens (list a b c d))
                 (padas (and (tibetan-metre--aligned-p ends lens)
                             (tibetan-metre--split syls lens))))
            (when (and padas
                       (cl-every #'identity
                                 (cl-mapcar (lambda (m p) (tibetan-metre--mora-pada-p p m tail))
                                            (list odd even odd even) padas)))
              (throw 'found padas))))))
       nil))
   when found
   return (list :name name :gana desc :padas found :yati nil :kind 'mora)))

(defun tibetan-metre--verse-shaped-p (segments)
  "The prose guard: a verse has two or more lines, or one line closed
by a double daṇḍa."
  (or (cdr segments)
      (string-match-p "\\(//\\|॥\\)" (or (car segments) ""))))

(defun tibetan-metre--verse (segments)
  "Syllables of the joined SEGMENTS and the cumulative syllable count
at every line end: (SYLS . LINE-ENDS)."
  (let* ((joined (mapconcat #'identity segments "\n"))
         (syls (tibetan-metre-syllables joined))
         (ends '()) (off 0))
    (dolist (seg segments)
      (setq off (+ off (length seg)))
      (push (cl-count-if (lambda (s) (<= (plist-get s :end) off)) syls) ends)
      (setq off (1+ off)))
    (cons syls (nreverse ends))))

(defun tibetan-metre-identify (segments)
  "The metre of the verse whose lines are SEGMENTS (IAST strings), or
nil (prose, or no known metre).  Plist: :name :gana :padas (lists of
syllable plists) :yati (syllable counts within the pāda) :caesura
\(syllable counts within each half, āryā family) :note."
  (let* ((v (tibetan-metre--verse segments))
         (syls (car v)) (ends (cdr v)) (n (length syls)))
    (and
     (tibetan-metre--verse-shaped-p segments)
     (or
     ;; sama metres
     (when (and (> n 0) (zerop (% n 4)))
       (let* ((k (/ n 4))
              (padas (tibetan-metre--split syls (list k k k k)))
              (fits (and (tibetan-metre--aligned-p ends (list k k k k))
                         (mapcar (lambda (p)
                                   (cl-remove-if-not
                                    (lambda (m) (tibetan-metre--match-p (nth 1 m) p))
                                    tibetan-metre--sama))
                                 padas))))
         (when (and fits (cl-every #'identity fits))
           (let ((common (cl-reduce (lambda (a b) (cl-intersection a b :test #'equal))
                                    fits)))
             (if common
                 (let ((m (car common)))
                   (list :name (nth 0 m) :gana (nth 2 m) :padas padas
                         :yati (nth 3 m)))
               ;; upajāti: every pāda in the same family
               (let ((fams (mapcar (lambda (f) (delq nil (mapcar (lambda (m) (nth 4 m)) f)))
                                   fits)))
                 (when-let ((fam (car (cl-reduce #'cl-intersection fams))))
                   (list :name (if (eq fam 'upajati-11)
                                   "Upajāti (Indravajrā/Upendravajrā)"
                                 "Upajāti (Vaṃśastha/Indravaṃśā)")
                         :gana (mapconcat (lambda (f) (nth 0 (car f))) fits " · ")
                         :padas padas :yati nil))))))))
     ;; ardhasama
     (cl-loop for (name odd even gana) in tibetan-metre--ardhasama
              for lens = (list (length odd) (length even) (length odd) (length even))
              for padas = (and (tibetan-metre--aligned-p ends lens)
                               (tibetan-metre--split syls lens))
              when (and padas
                        (cl-every #'identity
                                  (cl-mapcar (lambda (pat p) (tibetan-metre--match-p pat p))
                                             (list odd even odd even) padas)))
              return (list :name name :gana gana :padas padas :yati nil))
     (tibetan-metre--mora-ardhasama syls ends)
     ;; śloka: the even pādas end ⏑ – ⏑ ⏓ — required of at least half
     ;; of them (the prose guard's second line of defence)
     (when (and (memq n '(16 32 48)) (tibetan-metre--aligned-p ends (make-list (/ n 8) 8)))
       (let* ((padas (tibetan-metre--split syls (make-list (/ n 8) 8)))
              (even (cl-loop for p in padas for i from 1 when (cl-evenp i) collect (cons i p)))
              (bad (cl-loop for (i . p) in even
                            unless (string= (substring (tibetan-metre--weights p) 4 7) "LGL")
                            collect (string (+ ?a (1- i))))))
         (when (<= (* 2 (length bad)) (length even))
           (list :name "Śloka (Anuṣṭubh)" :gana nil :padas padas :yati nil
                 :note (when bad
                         (format "Pāda %s: Silbe 5–7 nicht ⏑ – ⏑ (Vipulā oder Lesefehler?)"
                                 (string-join bad ", ")))))))
     ;; āryā family: two half-verses (2 lines, or 4 lines = 2 per half)
     (let* ((halves (pcase (length ends)
                      (2 (tibetan-metre--split syls (list (car ends) (- n (car ends)))))
                      (4 (tibetan-metre--split syls (list (nth 1 ends) (- n (nth 1 ends)))))))
            (morae (and halves (cons (tibetan-metre--morae (car halves))
                                     (tibetan-metre--morae (cadr halves)))))
            (name (and morae (cdr (assoc morae tibetan-metre--arya-family))))
            (cuts (and name (mapcar (lambda (h) (tibetan-metre--mora-cut h 12)) halves))))
       (when (and name (cl-every #'identity cuts))
         (list :name name :padas halves :caesura cuts
               :gana (format "%d + %d Moren" (car morae) (cdr morae)))))))))

;; ============================================================================
;; Pause marks
;; ============================================================================

(defun tibetan-metre--marks (m)
  "Mark requests of the identified metre M: ((SYL-INDEX . MARK) …),
the mark going AFTER the syllable with global index SYL-INDEX."
  (let ((idx 0) out)
    (if (plist-get m :caesura)
        (cl-loop for h in (plist-get m :padas) for cut in (plist-get m :caesura)
                 do (push (cons (+ idx cut -1) "¦") out)
                 (cl-incf idx (length h)))
      (dolist (p (plist-get m :padas))
        (dolist (y (plist-get m :yati))
          (push (cons (+ idx y -1) "¦") out))
        (cl-incf idx (length p))
        (push (cons (1- idx) "|") out)))
    out))

(defun tibetan-metre-mark-segments (segments)
  "SEGMENTS with pause marks — ` ¦ ' at the caesura, ` | ' at a pāda
end inside a line — or nil when no metre is identified.  Marks at a
line end are dropped (the line break is the pause)."
  (when-let ((m (tibetan-metre-identify segments)))
    (let* ((joined (mapconcat #'identity segments "\n"))
           (syls (vconcat (tibetan-metre-syllables joined)))
           (inserts '()))
      (dolist (mk (tibetan-metre--marks m))
        (let* ((s (aref syls (car mk)))
               (pos (plist-get s :bnd)))
          (unless (or (>= (1+ (car mk)) (length syls))
                      (and (< pos (length joined))
                           (eq (aref joined pos) ?\n)))
            (push (if (plist-get s :space)
                      (cons (1+ pos) (concat (cdr mk) " "))
                    (cons pos (cdr mk)))
                  inserts))))
      (dolist (ins (sort inserts (lambda (a b) (> (car a) (car b)))))
        (setq joined (concat (substring joined 0 (car ins)) (cdr ins)
                             (substring joined (car ins)))))
      (split-string joined "\n"))))

;; ============================================================================
;; Section
;; ============================================================================

(defun tibetan-metre--verse-p (segments)
  "Non-nil when SEGMENTS end like a verse (double daṇḍa) and no line is
longer than a half-verse of the longest metre (Sragdharā, 42) —
Viveka prose also ends in `//'."
  (let ((last (car (last segments))))
    (and last
         (string-match-p "\\(//\\|॥\\)[ \t]*[0-9]*[a-z]*[ \t]*/?[ \t]*\\'" last)
         (cl-every (lambda (seg) (<= (length (tibetan-metre-syllables seg)) 44))
                   segments))))

(defun tibetan-metre--pause-rule (m)
  (cond
   ((plist-get m :caesura) "Zäsur nach dem dritten Fuß (12 Moren) in jedem Halbvers")
   ((plist-get m :yati)
    (let ((ys (mapcar (lambda (y) (format "%d." y)) (plist-get m :yati))))
      (format "Pause (yati) nach der %s Silbe"
              (if (cdr ys)
                  (concat (string-join (butlast ys) ", ") " und " (car (last ys)))
                (car ys)))))
   (t "keine feste Binnenpause — Pause am Pāda-Ende")))

(defun tibetan-metre--description (m)
  (let ((padas (plist-get m :padas)))
    (cond
     ((plist-get m :caesura)
      (format "Āryā-Familie, %s je Halbvers (Füße zu vier Moren)"
              (plist-get m :gana)))
     ((string-prefix-p "Śloka" (plist-get m :name))
      (format "%s Pādas zu 8 Silben; in den geraden Pādas Silbe 5–7 ⏑ – ⏑"
              (if (= 2 (length padas)) "Halbvers, zwei" "vier")))
     ((eq (plist-get m :kind) 'mora)
      (format "Ardhasama nach Moren: %s" (plist-get m :gana)))
     (t (format "%s Pādas zu %s Silben (%s)"
                (if (= 4 (length padas)) "vier" (number-to-string (length padas)))
                (mapconcat (lambda (n) (number-to-string n))
                           (delete-dups (mapcar #'length padas)) "/")
                (plist-get m :gana))))))

(defun tibetan-metre--table (m)
  "One org table for the verse: per pāda a syllable row and a weight
row; a `¦' column at the caesura; columns shared by all pādas."
  (let* ((padas (plist-get m :padas))
         (cut-of (lambda (i)
                   (cond ((plist-get m :caesura) (list (nth i (plist-get m :caesura))))
                         (t (plist-get m :yati)))))
         (rows
          (cl-loop
           for p in padas for i from 0
           for cuts = (funcall cut-of i)
           for label = (string (+ ?a i))
           append
           (let (srow wrow (k 0))
             (dolist (s p)
               (cl-incf k)
               (push (plist-get s :text) srow)
               (push (if (eq (plist-get s :weight) 'G) "–" "⏑") wrow)
               (when (memq k cuts)
                 (push "¦" srow) (push "¦" wrow)))
             (list (cons label (nreverse srow)) (cons "" (nreverse wrow))))))
         (ncol (apply #'max (mapcar #'length rows)))
         (rows (mapcar (lambda (r) (append r (make-list (- ncol (length r)) "")))
                       rows))
         (widths (cl-loop for c from 0 below ncol
                          collect (apply #'max (mapcar (lambda (r) (string-width (nth c r)))
                                                       rows)))))
    (mapconcat
     (lambda (r)
       (concat "|"
               (cl-loop for cell in r for w in widths
                        concat (concat " " cell
                                       (make-string (- w (string-width cell)) ?\s)
                                       " |"))))
     rows "\n")))

(defun tibetan-metre-section (segments)
  "The `** Metre' org block for the verse SEGMENTS, or nil for prose.
An unrecognised verse (double daṇḍa at the end) gets a visible note
with the bare scansion per line — never silence."
  (let ((m (tibetan-metre-identify segments)))
    (cond
     (m
      (concat
       "** Metre\n"
       (format "%s — %s; %s.\n" (plist-get m :name)
               (tibetan-metre--description m) (tibetan-metre--pause-rule m))
       (if (plist-get m :note) (concat (plist-get m :note) "\n") "")
       "Zeichen: ¦ Pause (yati) · | Pāda-Ende in der Zeile · – schwer (guru) · ⏑ leicht (laghu)\n\n"
       "#+begin_verse\n"
       (mapconcat #'identity (tibetan-metre-mark-segments segments) "\n")
       "\n#+end_verse\n\n"
       (tibetan-metre--table m) "\n\n"))
     ((tibetan-metre--verse-p segments)
      (concat
       "** Metre\n"
       "Metrum nicht erkannt — Silbenfolge je Zeile (Lesefehler oder seltenes Metrum?):\n\n"
       "#+begin_example\n"
       (mapconcat (lambda (seg)
                    (let ((sc (tibetan-metre-scheme seg)))
                      (format "%2d  %s"
                              (length sc)
                              (mapconcat (lambda (c) (if (eq c ?G) "–" "⏑"))
                                         (string-to-list sc) " "))))
                  segments "\n")
       "\n#+end_example\n\n")))))

(defun tibetan-metre-label (segments)
  "One printable line for the verse SEGMENTS — name · pause rule —
or nil (prose, unknown metre).  Used by the handout and the
discussion paper."
  (when-let ((m (tibetan-metre-identify segments)))
    (format "%s · %s" (plist-get m :name) (tibetan-metre--pause-rule m))))

(defun tibetan-metre-table (segments)
  "The syllable table (org) of the verse SEGMENTS, or nil."
  (when-let ((m (tibetan-metre-identify segments)))
    (tibetan-metre--table m)))

(provide 'tibetan-sanskrit-metre)
;;; tibetan-sanskrit-metre.el ends here
