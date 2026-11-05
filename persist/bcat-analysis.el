;;; bcat-analysis.el --- The C-c u G analysis for template-built files -*- lexical-binding: t -*-

;; This file is not part of GNU Emacs.

;;; Commentary:
;; §5.74 step 3 (Carsten 28.10.2026): one Claude request per sentence
;; yields everything the template shows — per segment the words
;; (gloss + function, so the tables lose their "?"), the segment's
;; share of the translation and of every reference translation (with
;; meaning-shifting passages marked), structure, an optional note and
;; concepts; for the sentence the grammar overview, the translation,
;; the marked references and a comparison.  "First the segments, then
;; the whole sentence/verse."  This module is pure: prompts, parser and
;; template fields — the C-c u G flow lives in bcat-sentence.el.
;;
;; The answer is one JSON object (robust to parse; Claude writes org
;; markup inside the strings: *bold* = meaning shift, /italic/ = Tibetan
;; words, =case= particles, ~converbs~).

;;; Code:

(require 'cl-lib)
(require 'subr-x)
(require 'json)
(require 'seq)
(require 'ol)
(require 'tibetan-wylie nil t)

(declare-function tibetan-gloss-table--cell "tibetan-gloss-table" (s))
(declare-function tibetan-gloss-table--truncate-cell "tibetan-gloss-table" (gloss))
(declare-function tibetan-gloss-table--format-rows "tibetan-gloss-table" (rows))

;; ----------------------------------------------------------------------------
;; Prompts
;; ----------------------------------------------------------------------------

(defun bcat-analysis--language (target)
  "The language name for the target code TARGET (\"de\" → German)."
  (if (equal target "de") "German" "English"))

(defun bcat-analysis--words-rule (lang)
  "The rule for the words of a Tibetan segment (glosses in LANG) — one
wording for the Tibetan analysis and the Tibetan request of a bilingual
sentence."
  (format "- words: the segment's words in order, together covering every syllable of its Wylie exactly once. A lexical compound or fixed verb phrase is one word (\"brjod du med pa\"); case and converb particles are words of their own. wylie: exactly as in the given Wylie. gloss: the meaning in THIS context, at most 25 characters, in %s. function: part of speech and syntactic function, compact, at most 25 characters — \"N · agent\", \"ADJ · predicate\", \"V pf. ('pong ba) · main verb\", \"DEM · object\", \"NEG + V pf. (byed pa)\"; a case or converb particle gets only its label: ERG, GEN, ABS, TERM, LOC, ABL, COM, DAT, or \"CONV · cond.\" etc.; a verb's dictionary form in parentheses only when the word is another stem than it shows (spangs: \"V pf. ('pong ba)\"), not when it repeats the stem (brgyud pa'i: \"V pf.\"); any further explanation (what it modifies, the kind of clause) belongs in structure."
          lang))

(defun bcat-analysis--segment-entry (num text &optional wylie translation)
  "One segment of a user prompt: [NUM] and TEXT — Tibetan with its
WYLIE, or (WYLIE nil) a line as transmitted —, then TRANSLATION when
given."
  (concat (if wylie
              (format "[%d] Tibetan: %s\n     Wylie: %s" num (string-trim text) wylie)
            (format "[%d] %s" num (string-trim text)))
          (if translation (format "\n     Translation: %s" translation) "")))

(defun bcat-analysis--tibetan-segment-rules (lang)
  "The rules for the segment fields of a Tibetan analysis, words to
concepts (glosses in LANG) — one wording for the Tibetan analysis, the
Tibetan request of a bilingual pair and the commentary."
  (concat (bcat-analysis--words-rule lang) "\n"
          "- translation (segment): this segment's part of your translation; the parts read in order form the sentence translation. Give key terms with the Tibetan and the established Sanskrit in parentheses, e.g. \"uncompounded ('dus ma byas: asaṃskṛta)\".
- references (segment): for every reference translation given, the part that corresponds to this segment, quoted verbatim (keep its brackets and Sanskrit; not its notes); name: the full name as given under Reference translations, never a siglum. Mark with org bold *…* ONLY passages that shift the meaning against the Tibetan or against your reading; stylistic variation is not marked.
- shift: one line on what the marked passages shift, beginning with the translator name(s): \"Lindtner 1981: active, with the yogin as agent\"; \"\" when nothing is marked.
- structure: the segment's syntax in one line, org markup — case particles =yis=, converbs ~na~, Tibetan words /italic/: \"rnal 'byor pa =yis= [agent, ERG] · de [object, ABS] · rtogs [V] ~na~ [conditional] → protasis of 41.\"
- note: only for a grammatical peculiarity or a real ambiguity, one or two sentences; otherwise \"\".
- concepts: Buddhist technical concepts occurring in THIS segment — the term in *bold*, its Sanskrit, a short encyclopedic note of two or three sentences; \"\" when there is none."))

(defun bcat-analysis--second-language (target)
  "The second language of an analysis in TARGET: (KEY . NAME) — the
field `translation_de' and German for an English one, `translation_en'
and English for a German one."
  (if (equal target "de") '("translation_en" . "English") '("translation_de" . "German")))

(defun bcat-analysis--second-request (target)
  "The paragraph of a user prompt asking for the sentence translation in
the second language of TARGET — only for a sentence with the option
\(Carsten 1.11.: „nicht als default …, sondern nur als Option
\(wahrscheinlich sogar auf Satzebene)“; `:CAT_SECOND_LANGUAGE:' in the
sentence's drawer or #+CAT_SECOND_LANGUAGE in the source).  In the user
prompt, not the system prompt: that stays the same per document (the
cache)."
  (let ((second (bcat-analysis--second-language target)))
    (format "\n\nAlso add to the JSON object the field \"%s\": the sentence translation once more in %s — close to the original, the key terms with the original in parentheses; the class thinks the concepts through in this language too.  In the comparison units, the finding also says what each reading would be in %s (in quotation marks)."
            (car second) (cdr second) (cdr second))))

(defcustom bcat-analysis-comparison-language nil
  "Language code of the reasoning in the comparison of the translations
\(core, finding, weight, the class conclusions, decisions, corrections)
and of its words in the file; nil: the document's target language.
Carsten 2.11.: first „Sprache der Begründungen Deutsch, Übersetzungen
Englisch (wie im Muster)“, the same day „bitte im Handout auch immer
alles in der Zielsprache“ — so nil; \"de\" or \"en\" fixes one."
  :type '(choice (const :tag "Zielsprache des Dokuments" nil)
                 (const :tag "Deutsch" "de") (const :tag "English" "en"))
  :group 'buddhist-cat)

(defconst bcat-analysis--comparison-schema
  "\"units\": [{\"passage\": \"…\", \"rating\": \"green|yellow|red\", \"core\": \"…\", \"readings\": [{\"sigla\": \"…\", \"text\": \"…\"}], \"finding\": \"…\", \"weight\": \"…\", \"grammatical\": \"all|one\", \"grammar_note\": \"…\", \"certainty\": \"certain|likely|open\", \"impact\": \"low|medium|high\", \"impact_note\": \"…\", \"doubts\": [{\"who\": \"…\", \"verdict\": \"doubtful|wrong\", \"reason\": \"…\", \"better\": \"…\"}]}], \"proposal\": [{\"label\": \"…\", \"text\": \"…\"}], \"decisions\": \"…\", \"corrections\": [\"…\"]"
  "The comparison's part of the JSON contract of every sentence analysis
\(`bcat-analysis--sentence-rules').")

(defun bcat-analysis--sentence-rules (lang)
  "The rules for the sentence translation and the comparison of the
translations (renderings in LANG) — one wording for every analysis.
Carsten 2.11. (Muster vergleich-ta1-1.org, „das Muster passt“; at
every C-c u G): instead of a short paragraph, unit by unit — rated
green/yellow/red, argued from the original, „es gibt da keine
Autorität, auf die man sich verlassen kann“ —, a proposal that decides
the red units, the decisions, the errors of the own analysis.  The
translation in the second language and the readings of the comparison
in it are an option of the sentence (`bcat-analysis--second-request')."
  (format "- translation (sentence): your translation of the whole sentence (the segment parts joined).
- units: the translations compared unit by unit. Divide the sentence into its units of meaning — a word, a compound or a phrase; about 4 to 12 for a verse — and set side by side what each given translation makes of each: yours (siglum C) and every reference translation, by the sigla the request gives. One entry per unit in the order of the text, a last one for the sentence as a whole only where it matters (a double sense, śleṣa, that a translation gives only in part); [] when no reference translation is given.
  - passage: the unit, words of the original in /italic/: \"/vimalakalāśrayā/\", \"/jananī/, /janakaḥ/\".
  - rating: \"green\": no difference in meaning, only in wording; \"yellow\": a possible other interpretation, both defensible from the text; \"red\": different meanings, not both right at once — red only when the grammar or the lexicon excludes one reading, and red as soon as one reading is wrong (a doubt with the verdict \"wrong\"); when all readings are grammatically possible, yellow at most, however strongly the context favours one.
  - core: the point of the unit for the overview table, at most 80 characters.
  - readings: each translation's wording of the unit, verbatim in „…“ (shortened with … where long); translations reading alike in one entry (\"sigla\": \"D, DM, C\"); a second sense after a semicolon; [] for a green unit whose finding names the common reading.
  - finding: why the readings differ and what the original supports — argued from the original alone: grammar, sandhi, lexicon, the context in the work and its commentary; never from a translator's standing (there is no authority to rely on). An argument from the given lectures is cited with siglum, lecture and time mark: \"(S, I 0:15:24)\". Words of the original in /italic/. For a green unit one sentence.
  - weight: the reading the finding favours, as a rendering in „…“, with a short note where needed; \"\" for a green unit.
  - grammatical: \"all\" when every reading is grammatically possible (the choice then rests on context, commentary, doctrine or style), \"one\" when the grammar or the lexicon excludes the others; grammar_note: one sentence why, words of the original in /italic/. Carsten: \"the question is simply whether both readings are grammatically possible\".
  - certainty: how certain the decision of the proposal for this unit is — \"certain\" (the grammar decides), \"likely\" (context or commentary decides), \"open\" (both remain defensible).
  - impact: what the choice changes — \"low\" (the same sense or doctrine, only the construction differs), \"medium\", \"high\" (another claim); impact_note: one sentence on the consequence for the meaning of the sentence. For a green unit grammatical, certainty and impact may be left out.
  - doubts: the conclusion for the class, said aloud as „D renders it so, I am not sure that is right, because …“ — draw the conclusion wherever the text allows one. For every yellow and red unit one entry per rendering the finding questions (translations reading alike in one entry; yours too, when it is questioned); [] for a green unit. who: the sigla with their wording, „D „the hidden (protected) light“, DM „concealed within the five faces““. verdict: \"wrong\": the grammar or the lexicon excludes it; \"doubtful\": grammatically possible, but the context or the commentary speaks against it. reason: why, in one or two sentences, argued from the original as in the finding, a lecture with its time mark — a subordinate clause without the conjunction (\"because\" is set before it; in German verb last: \"gupta (√gup) zuerst ‚bewahrt‘ heißt\"). better: the rendering to prefer in „…“ with the sigla of the translations that have it, \"\" when none is better.
- proposal: your translation of the sentence weighed by the units — the red units decided, for the yellow ones the chosen reading (the other belongs in a note); where the text has a double sense one entry per sense, each with its label (\"First sense (theological)\"), else one entry with the label \"\"; [] when units is []. Each text a finished translation to be read aloud in class: fluent and idiomatic, every statement once, the word order free; square brackets only for words the original leaves implicit — never to show a construction or to repeat a clause (that belongs in decisions).
- decisions: the decisions of the proposal, by unit number, in one to three sentences; \"\" when units is [].
- corrections: errors of your own analysis above that the comparison brings to light (a form misidentified, a gloss that does not fit), one sentence each; [] when there are none.
- Language of the comparison: core, finding, the notes in weight, reason, decisions and corrections in %s, the labels of the proposal, grammar_note and impact_note too; readings verbatim; the renderings in weight and proposal in %s."
          (if bcat-analysis-comparison-language
              (bcat-analysis--language bcat-analysis-comparison-language)
            lang)
          lang))

(defun bcat-analysis--tibetan-rules (lang)
  "The rules for the fields of a Tibetan sentence analysis, from words
to comparison (glosses in LANG) — one wording for the Tibetan analysis
and the Tibetan request of a bilingual pair."
  (concat (bcat-analysis--tibetan-segment-rules lang) "\n"
          "- grammar: an overview of the whole sentence — verb backbone, clause linkage, agents, elisions.\n"
          (bcat-analysis--sentence-rules lang)))

(defun bcat-analysis--references-block (refs)
  "The reference translations REFS ((NAME . TEXT) …) of a user prompt."
  (concat "\n\nReference translations:\n"
          (if refs
              (mapconcat (lambda (r) (format "=== %s ===\n%s" (car r) (string-trim (cdr r))))
                         refs "\n")
            "none")))

(defun bcat-analysis-sigla (names)
  "The sigla of the comparison for the reference NAMES: ((NAME . SIGLUM)
…) — the initial of the first word, its capitals when it has several
\(DharmaMitra: DM); C is the own translation; on a collision the next
letters (Sanderson 2025 after Sanderson 2022: Sa).  Muster 2.11.: D =
Dyczkowski 2023, W = Wallis 2015, S = Sanderson 2022, DM, C."
  (let ((case-fold-search nil) (taken (list "C")) (out '()))
    (dolist (name names (nreverse out))
      (let* ((word (or (car (split-string name "[ /,]+" t)) "X"))
             (caps (replace-regexp-in-string "[^[:upper:]]" "" word))
             (sig (if (> (length caps) 1) caps (upcase (substring word 0 1))))
             (i 2))
        (while (member sig taken)
          (setq sig (if (<= i (length word))
                        (concat (upcase (substring word 0 1)) (substring word 1 i))
                      (format "%s%d" sig i))
                i (1+ i)))
        (push sig taken)
        (push (cons name sig) out)))))

(defun bcat-analysis--lecture-text (body)
  "The lecture BODY for a prompt: a time-mark link (YouTube) as its time
mark in brackets, any other link as its text."
  (replace-regexp-in-string
   "\\[\\[[^]]*\\]\\[\\([^]]*\\)\\]\\]" "\\1"
   (replace-regexp-in-string "\\[\\[[^]]*\\]\\[\\([0-9]+:[0-9:]+\\)\\]\\]" "[\\1]" body)))

(defun bcat-analysis--comparison-block (refs lectures)
  "The paragraphs of a user prompt for the comparison: the sigla of REFS
\((NAME . TEXT) …) and the LECTURES on the sentence ((NAME . BODY) …) —
Carsten 2.11.: Sanderson's lectures „nur beim Vergleich“ (before: never
in a prompt)."
  (concat
   "\n\nSigla for the comparison: C = your translation"
   (mapconcat (lambda (s) (format ", %s = %s" (cdr s) (car s)))
              (bcat-analysis-sigla (mapcar #'car refs)) "")
   " — only in the units (their readings); everywhere else, references and shift included, the full names."
   (if lectures
       (concat "\n\nLectures on this sentence — only for the comparison (the findings), cite with the time mark:\n"
               (mapconcat (lambda (l) (format "=== %s ===\n%s" (car l)
                                              (string-trim (bcat-analysis--lecture-text (cdr l)))))
                          lectures "\n"))
     "")))

(defun bcat-analysis-system-prompt (target)
  "The system prompt of the sentence analysis for the target language
code TARGET — constant per document, so the prompt cache stays warm."
  (let ((lang (bcat-analysis--language target)))
    (format "You are a philologist preparing one sentence or verse of a Classical Tibetan Buddhist text for a university reading class. Work segment by segment, then on the whole sentence. Answer with ONE JSON object and nothing else (no prose, no code fence):

{\"segments\": [{\"num\": <segment number>, \"words\": [{\"wylie\": \"…\", \"gloss\": \"…\", \"function\": \"…\"}], \"translation\": \"…\", \"references\": [{\"name\": \"…\", \"text\": \"…\"}], \"shift\": \"…\", \"structure\": \"…\", \"note\": \"…\", \"concepts\": \"…\"}], \"grammar\": \"…\", \"translation\": \"…\", %s}

Rules:
- segments: one entry per given segment, same numbers, same order.
%s
- Write glosses, notes and explanations in %s. Never mention yourself."
            bcat-analysis--comparison-schema (bcat-analysis--tibetan-rules lang) lang)))

(defun bcat-analysis-user-prompt (sent-num verse segs refs &optional context second lectures)
  "The user prompt for sentence SENT-NUM (VERSE its :VERSE: or nil):
SEGS ((NUM TIBETAN WYLIE) …), REFS ((NAME . TEXT) …) the reference
translations (DharmaMitra among them), CONTEXT the document's
#+TIBETAN_CLAUDE_CONTEXT lines; SECOND, the target language code of a
sentence with the option, asks for the translation in the second
language (`bcat-analysis--second-request'); the sigla of the comparison
and LECTURES ((NAME . BODY) …) on the sentence for it
\(`bcat-analysis--comparison-block')."
  (concat
   (if context (concat (mapconcat #'identity context "\n") "\n\n") "")
   (format "Sentence %d%s — segments:\n" sent-num
           (if verse (format " (%s)" verse) ""))
   (mapconcat (lambda (s) (bcat-analysis--segment-entry (nth 0 s) (nth 1 s) (nth 2 s)))
              segs "\n")
   (bcat-analysis--references-block refs)
   (bcat-analysis--comparison-block refs lectures)
   (if second (bcat-analysis--second-request second) "")
   "\n\nReturn the JSON object now."))

(defun bcat-analysis-align-system-prompt (target &optional source)
  "System prompt of the small request that places reference
translations added by hand (§5.74 step 3c) — target language TARGET,
SOURCE the language of the text (\"Classical Tibetan\" by default,
\"Sanskrit\")."
  (let ((lang (bcat-analysis--language target)))
    (format "You align additional reference translations with the segments of a %s sentence that has already been analysed for a university reading class. Answer with ONE JSON object and nothing else:

{\"segments\": [{\"num\": <segment number>, \"references\": [{\"name\": \"…\", \"text\": \"…\"}], \"shift\": \"…\"}], \"comparison\": \"…\"}

Rules:
- segments: one entry per given segment; references: for every given reference translation the part that corresponds to this segment, quoted verbatim (not its notes). Mark with org bold *…* ONLY passages that shift the meaning against the original or against the given translation of the segment; stylistic variation is not marked.
- shift: one line on what the marked passages shift, beginning with the translator name; \"\" when nothing is marked.
- comparison: one or two sentences on the meaning-shifting differences of the new translation(s); \"\" when there are none.
- Write in %s. Never mention yourself."
            (or source "Classical Tibetan") lang)))

(defun bcat-analysis-align-user-prompt (sent-num verse segs refs &optional context second)
  "User prompt of the alignment request: SEGS ((NUM TEXT WYLIE
TRANSLATION) …) with the translation already in the file — WYLIE nil
for Sanskrit, whose TEXT is shown as transmitted —, REFS the new
reference translations ((NAME . TEXT) …); SECOND, the target language
code of a sentence with the option, asks for the readings of the
comparison in the second language too."
  (concat
   (if context (concat (mapconcat #'identity context "\n") "\n\n") "")
   (format "Sentence %d%s — segments:\n" sent-num (if verse (format " (%s)" verse) ""))
   (mapconcat (lambda (s)
                (bcat-analysis--segment-entry (nth 0 s) (nth 1 s) (nth 2 s) (or (nth 3 s) "")))
              segs "\n")
   "\n\nReference translations to align:\n"
   (mapconcat (lambda (r) (format "=== %s ===\n%s" (car r) (string-trim (cdr r)))) refs "\n")
   (if second
       (format "\n\nIn the comparison, for each difference also say what each reading would be in %s (in quotation marks)."
               (cdr (bcat-analysis--second-language second)))
     "")
   "\n\nReturn the JSON object now."))

(defun bcat-analysis-sanskrit-system-prompt (target &optional bhasya recitation)
  "The system prompt of the Sanskrit sentence analysis (§5.74, Carsten
29.10.: his Sanskrit is rudimentary, the class has Sanskrit
professionals and goes deep into the grammar — roots, gaṇa, stems;
terminology of the Devavāṇīpraveśikā) for the target language code
TARGET — constant per document, so the prompt cache stays warm.
BHASYA: the document is read verse by verse with the bhāṣya below the
verse (MSA, 30.10.) — the answer has `bhasya_hint' too.  RECITATION:
the file has a Recitation (the Sanskrit template) — for prose the
answer gives `recitation', the text in sense units (31.10., TĀ 1008)."
  (let ((lang (bcat-analysis--language target)))
    (format "You are a Sanskritist preparing one sentence or verse of a Sanskrit text for a university reading class of experienced Sanskritists; one participant reads Sanskrit only haltingly, so every form is spelled out. Work segment by segment, then on the whole sentence. Use the grammatical terminology of Goldman & Sutherland Goldman, Devavāṇīpraveśikā: Sanskrit terms, with the English equivalent where it helps. Answer with ONE JSON object and nothing else (no prose, no code fence):

{\"segments\": [{\"num\": <segment number>, \"words\": [{\"word\": \"…\", \"lemma\": \"…\", \"formation\": \"…\", \"parts\": \"…\", \"gloss\": \"…\", \"form\": \"…\"}], \"translation\": \"…\", \"references\": [{\"name\": \"…\", \"text\": \"…\"}], \"shift\": [\"…\"], \"sandhi\": \"…\", \"forms\": [{\"word\": \"…\", \"analysis\": \"…\"}], \"compounds\": [{\"word\": \"…\", \"analysis\": \"…\"}], \"structure\": \"…\", \"note\": \"…\", \"concepts\": \"…\"}], \"grammar\": \"…\", \"translation\": \"…\", %s%s}

Rules:
- segments: one entry per given segment, same numbers, same order.
- words: the segment's padapāṭha — every word with all sandhi undone, in pausa form, in order, together giving the whole segment; the members of a compound joined by hyphens (\"mad-vāk\", \"śrīmat-mahā-ānanda-vijṛmbhitam\"). Check the resolution: a privative a- or an initial vowel can vanish in sandhi (madvāgaśaktāpi = mad-vāk aśaktā api).
- lemma: the dictionary form — a nominal's stem (\"aśakta\", \"niyantraṇa\", \"vāc\"), a verb's prefixes and √root (\"ud-√las\"), a compound's members as stems joined by hyphens (\"mad-vāc\", \"śrīmat-mahā-ānanda-vijṛmbhita\"); an indeclinable as it stands.
- formation: how the word is built — prefixes, √root, suffix: \"a-√śak-ta\", \"ni-√yantr-ana\", \"ud-√las\"; for a compound its members separated by \" · \", each with its formation (\"śrī-mat · mahā · ā-√nand-a · vi-√jṛmbh-ita\"); \"—\" for indeclinables and pronouns.
- parts: the meaning of each part of the formation in one or two words, in the same order, separated by \" · \", a root with its gaṇa number: \"a- not · √śak (5) be able · -ta ppp\"; \"\" when formation is \"—\".
- gloss: the meaning in THIS context, at most 30 characters, no comments or cross-references (those belong in structure or note).
- form: a compact label — \"f. nom. sg.\", \"ppp. f. nom. sg.\", \"opt. 3 sg. P\", \"pron. n. nom. sg.\", \"adj. n. nom. sg.\", \"ind.\".
- translation (segment): this segment's part of your translation; the parts read in order form the sentence translation. Give key terms with the Sanskrit in parentheses, e.g. \"incapable (aśakta)\".
- references (segment): for every reference translation given, the part that corresponds to this segment, quoted verbatim (keep its brackets; not its notes); name: the full name as given under Reference translations, never a siglum. Mark with org bold *…* ONLY passages that shift the meaning against the Sanskrit or against your reading; stylistic variation is not marked.
- shift: one line for each translator whose part has a marked passage, beginning with the translator's name: \"Dyczkowski 2023: niyantraṇa as a restriction on the reader's understanding\"; [] when nothing is marked — never a line for a translator without marks.
- sandhi: every junction of the segment resolved as it stands in the transmitted text (yanniyantraṇam = yat niyantraṇam: t before a nasal → n; no junction or visarga that the text does not have), separated by \" · \": \"madvāg aśaktā: final -k voiced before a vowel → -g · aśaktā + api → aśaktāpi (ā + a → ā)\"; internal sandhi of a prefix where it changes the form (\"ud + laset → ullaset (d + l → ll)\").
- forms: one entry per declined or conjugated word (none for indeclinables), word as in words. Nominals: stem with gender and stem class, vibhakti and vacana with the English (\"prathamā vibhakti ekavacana (nom. sg.)\"), function. Verbs: √root with gaṇa number, gaṇa name, pada and the 3rd sg. present (\"√las (1, bhvādi, P: lasati)\"), lakāra with the English, pada, puruṣa and vacana with the English (\"prathamapuruṣa ekavacana (3rd sg.)\" — prathama puruṣa is the 3rd person), the present stem. Participles and other kṛdanta: their name and suffix (\"bhūte kṛdanta (ppp., kta)\", \"tvānta/lyabanta (gerund)\", \"tumanta (infinitive)\"). Taddhita derivatives: base and suffix, with vṛddhi or guṇa where it occurs.
- compounds: one entry per compound — the samāsa type and the vigraha with its meaning: \"ṣaṣṭhī tatpuruṣa: mama vāk „my speech“\"; for longer compounds the inner structure; [] when there is none.
- structure: the segment's syntax in one line, org markup — Sanskrit words /italic/, functions in brackets: \"/yat niyantraṇam/ [subject] /ullaset/ [V opt.] → relative clause, taken up by /tat/ in 686\".
- note: only for a grammatical peculiarity or a real ambiguity, one or two sentences; otherwise \"\".
- concepts: technical concepts (Buddhist, Śaiva, philosophical) occurring in THIS segment — the term in *bold*, a short encyclopedic note of two or three sentences; \"\" when there is none.
- grammar: an overview of the whole sentence — the clauses and how they are linked, the verbs, subjects, elisions (an understood copula). Name a metre only as given under Metre in the request; none when none is given.
%s
%s- JSON escaping: only \\\" and \\\\ — never a backslash before an apostrophe.
- Write glosses, notes and explanations in %s. Never mention yourself."
            bcat-analysis--comparison-schema
            (concat (if bhasya ", \"bhasya_hint\": \"…\"" "")
                    (if recitation ", \"recitation\": [\"…\"]" ""))
            (bcat-analysis--sentence-rules lang)
            (concat
             (if bhasya
                 "- bhasya_hint: the bhāṣya on this verse is given as context; one to three sentences on what in it matters for reading the verse — how it takes a word or the verse's function, a reading it presupposes, where its Tibetan says something else; Sanskrit and Tibetan words in /italic/; \"\" when nothing.\n"
               "")
             (if recitation
                 "- recitation: for prose only (for a verse []): the transmitted text of the whole sentence exactly as given — same spelling and sandhi, nothing added or left out —, divided for reading aloud into lines, one array entry per line, each a sense unit of a few words ending at a clause or phrase boundary (break only where the text has a space); inside a line mark a short breath pause with ¦.\n"
               ""))
            lang)))

(defun bcat-analysis-sanskrit-user-prompt (sent-num verse segs refs &optional context bhasya second lectures metre)
  "The user prompt for the Sanskrit sentence SENT-NUM (VERSE its :VERSE:
or nil): SEGS ((NUM TEXT) …) the segments as transmitted, REFS ((NAME .
TEXT) …) the reference translations, CONTEXT the document's
#+TIBETAN_CLAUDE_CONTEXT lines, BHASYA (SANSKRIT . TIBETAN) the bhāṣya
on the verse — lines as transmitted, the Tibetan in Wylie; SECOND and
LECTURES as in `bcat-analysis-user-prompt'; METRE the name of the metre
the tool scans (`bcat-sentence--metre-name') — 5.11., TĀ 1.1: without
it Claude named Śālinī for a Nardaṭaka."
  (concat
   (if context (concat (mapconcat #'identity context "\n") "\n\n") "")
   (format "Sentence %d%s — segments:\n" sent-num (if verse (format " (%s)" verse) ""))
   (mapconcat (lambda (s) (bcat-analysis--segment-entry (nth 0 s) (nth 1 s))) segs "\n")
   (if metre
       (format "\n\nMetre (scanned by the tool from the syllable quantities): %s" metre)
     "")
   (if bhasya
       (concat "\n\nBhāṣya on this verse (context):"
               (if (car bhasya) (concat "\nSanskrit: " (string-join (car bhasya) " ")) "")
               (if (cdr bhasya) (concat "\nTibetan: " (string-join (cdr bhasya) " ")) ""))
     "")
   (bcat-analysis--references-block refs)
   (bcat-analysis--comparison-block refs lectures)
   (if second (bcat-analysis--second-request second) "")
   "\n\nReturn the JSON object now."))

;; Bilingual (§5.74, Carsten 30.10.): the Sanskrit request above, then
;; one for the Tibetan translation.

(defun bcat-analysis-tibetan-system-prompt (target)
  "System prompt of the Tibetan request of a bilingual sentence (§5.74,
Carsten 30.10.: the class reads and translates the Sanskrit, then reads
the Tibetan aloud and discusses where it differs; the Tibetan file of
the pair gets „das volle Programm“) for the target language code
TARGET: the whole Tibetan analysis (the rules of the Tibetan contract)
plus, per Tibetan segment, the Sanskrit segment it translates, the
differences per Sanskrit segment — none where there is nothing („wenn
nichts da ist bitte weg lassen“) — and the rendering of the sentence.
Constant per document."
  (let ((lang (bcat-analysis--language target)))
    (format "You prepare the Tibetan translation of one sentence of a Sanskrit Buddhist text for a university reading class that reads and translates the Sanskrit first, then reads the Tibetan aloud, analyses it and discusses where it differs from the Sanskrit. Work segment by segment, then on the whole sentence. Answer with ONE JSON object and nothing else (no prose, no code fence):

{\"segments\": [{\"num\": <Tibetan segment number>, \"sanskrit\": <Sanskrit segment number>, \"words\": [{\"wylie\": \"…\", \"gloss\": \"…\", \"function\": \"…\"}], \"translation\": \"…\", \"references\": [{\"name\": \"…\", \"text\": \"…\"}], \"shift\": \"…\", \"structure\": \"…\", \"note\": \"…\", \"concepts\": \"…\"}], \"differences\": [{\"sanskrit\": <Sanskrit segment number>, \"text\": \"…\"}], \"grammar\": \"…\", \"translation\": \"…\", %s, \"rendering\": \"…\"}

Rules:
- segments: one entry per given Tibetan segment, same numbers, same order.
- sanskrit: the number of the Sanskrit segment this Tibetan segment translates. The Tibetan follows the Sanskrit, so the numbers never decrease; a Tibetan segment translating parts of two Sanskrit segments goes with the first.
%s
- differences: one entry for each Sanskrit segment whose Tibetan says or reads something different — another sense or term (udbhāvanatā \"bringing to light\" rendered brjod pa \"stating\"), an addition or omission, another construction, a different reading of the Sanskrit; one to three sentences, Sanskrit and Tibetan words in /italic/. No entry where the Tibetan renders the Sanskrit faithfully: word order, particles, a plural marker or the question formula are no difference.
- rendering: the Tibetan translation of the whole sentence in two to five sentences — how it reproduces the structure, how it renders the key terms, where it shifts the sense; Sanskrit and Tibetan words in /italic/.
- JSON escaping: only \\\" and \\\\ — never a backslash before an apostrophe.
- Write glosses, notes and explanations in %s. Never mention yourself."
            bcat-analysis--comparison-schema (bcat-analysis--tibetan-rules lang) lang)))

(defun bcat-analysis-tibetan-user-prompt (sent-num verse sanskrit tibetan refs &optional context second lectures)
  "User prompt of the Tibetan request for sentence SENT-NUM (VERSE its
:VERSE: or nil): SANSKRIT ((NUM TEXT TRANSLATION) …) the Sanskrit
segments as transmitted with the translation of the Sanskrit answer,
TIBETAN ((NUM TIBETAN WYLIE) …), REFS ((NAME . TEXT) …) the reference
translations of the Tibetan, CONTEXT the #+TIBETAN_CLAUDE_CONTEXT lines
\(the Tibetan source's), SECOND and LECTURES as in
`bcat-analysis-user-prompt'."
  (concat
   (if context (concat (mapconcat #'identity context "\n") "\n\n") "")
   (format "Sentence %d%s — Sanskrit segments:\n" sent-num (if verse (format " (%s)" verse) ""))
   (mapconcat (lambda (s) (bcat-analysis--segment-entry (nth 0 s) (nth 1 s) nil (or (nth 2 s) "")))
              sanskrit "\n")
   "\n\nTibetan segments:\n"
   (mapconcat (lambda (s) (bcat-analysis--segment-entry (nth 0 s) (nth 1 s) (nth 2 s)))
              tibetan "\n")
   (bcat-analysis--references-block refs)
   (bcat-analysis--comparison-block refs lectures)
   (if second (bcat-analysis--second-request second) "")
   "\n\nReturn the JSON object now."))

;; ----------------------------------------------------------------------------
;; Parsing the answer
;; ----------------------------------------------------------------------------

(defcustom bcat-analysis-response-directory
  (expand-file-name "bcat-responses" temporary-file-directory)
  "Where an answer C-c u G could not read is kept, one file per answer
\(sentence file name + time) — outside the corpus, for diagnosis."
  :type 'directory
  :group 'buddhist-cat)

(defun bcat-analysis--json-problem (err start)
  "A short description of the JSON error ERR; positions count from the
start of the answer (the object began at START)."
  (let ((pos (car (last (seq-filter #'integerp (cdr err))))))
    (if pos
        (format "JSON-Fehler bei Zeichen %d" (+ start pos))
      "JSON-Fehler")))

(defun bcat-analysis--repair-json (text)
  "TEXT with the defects seen in Claude's JSON mended: a raw control
character inside a string is escaped; so is a quote inside a string
that cannot end it where the string stands — a key ends before `:', a
value in an object before `}' or `,' plus the start of a key, an
element of an array before `]' or `,' plus the start of a value
\(Eimer's \"Um die Hemmnisse, nämlich die \"Befleckungen\", …\" in
Lindtner's note; 30.10., bDen 16: [TERM + auxiliary, \\\"come to
realize\"] in a structure line — a quote before `]' in an object's
value); a string where a key belongs, followed by `}' or `,' plus a
key, is a value without key and gets \"text\" (30.10., MSA IX.60: a
DharmaMitra reference {\"name\": \"DharmaMitra\", \"*The first …*\"} —
the only bare text in our answers is a reference's); a backslash
before a character JSON does not escape is dropped (29.10.: a backslash
before the apostrophe of the genitive particle in a structure line).
Valid JSON comes back unchanged."
  (with-temp-buffer
    (insert text)
    (goto-char (point-min))
    (let ((in-string nil) (key nil) (expect-key nil) (stack '()) (start nil)
          (value "[ \t\n\r]*\\(?:[\"{[]\\|-?[0-9]\\|true\\_>\\|false\\_>\\|null\\_>\\)"))
      (while (not (eobp))
        (let ((c (char-after)))
          (cond
           ((not in-string)
            (pcase c
              (?\" (setq in-string t
                         key (and expect-key (eq (car stack) ?{))
                         start (point-marker)))
              ((or ?{ ?\[) (push c stack) (setq expect-key (eq c ?{)))
              ((or ?} ?\]) (pop stack) (setq expect-key nil))
              (?, (setq expect-key (eq (car stack) ?{)))
              (?: (setq expect-key nil)))
            (forward-char 1))
           ((eq c ?\\)
            (if (memq (char-after (1+ (point))) '(?\" ?\\ ?/ ?b ?f ?n ?r ?t ?u))
                (forward-char (min 2 (- (point-max) (point))))
              ;; No JSON escape (Claude put one before the apostrophe
              ;; of the genitive 'i): drop the backslash, keep the char.
              (delete-char 1)))
           ((eq c ?\")
            (let* ((after (lambda (as-key)
                            (forward-char 1)
                            (skip-chars-forward " \t\n\r")
                            (cond ((eobp))
                                  (as-key (eq (char-after) ?:))
                                  ((eq (char-after) ?,)
                                   (forward-char 1)
                                   (looking-at (if (eq (car stack) ?{) "[ \t\n\r]*\"" value)))
                                  ((eq (car stack) ?{) (eq (char-after) ?}))
                                  ((eq (car stack) ?\[) (eq (char-after) ?\])))))
                   (close (save-excursion (funcall after key)))
                   ;; A value where a key belongs: as if it were one.
                   (orphan (and key (not close)
                                (save-excursion (funcall after nil)))))
              (cond (orphan
                     (save-excursion (goto-char start) (insert "\"text\": "))
                     (setq in-string nil expect-key nil))
                    (close (setq in-string nil expect-key nil))
                    (t (insert "\\"))))
            (forward-char 1))
           ((< c 32)
            (delete-char 1)
            (insert (pcase c
                      (?\n "\\n")
                      (?\t "\\t")
                      (?\r "\\r")
                      (_ (format "\\u%04x" c)))))
           (t (forward-char 1))))))
    (buffer-string)))

(defun bcat-analysis--unterminated-p (text)
  "Non-nil when the JSON in TEXT (from its first brace) never closes — an
answer cut off at the output limit (30.10., AS sentence 2: it ended
after 41 283 characters inside „grammar“)."
  (let ((depth 0) (in-string nil) (escape nil))
    (dolist (c (string-to-list text))
      (cond (escape (setq escape nil))
            ((and in-string (eq c ?\\)) (setq escape t))
            ((eq c ?\") (setq in-string (not in-string)))
            (in-string)
            ((memq c '(?{ ?\[)) (cl-incf depth))
            ((memq c '(?} ?\])) (cl-decf depth))))
    (or in-string (> depth 0))))

(defun bcat-analysis-no-links (text)
  "TEXT with every org link turned into plain brackets — „[[topic]]“ →
„[topic]“, „[[x][y]]“ → „[x][y]“: text from outside (Claude,
DharmaMitra) never forms a link in a file.  Carsten 30.10.: „broken
links sollten eigentlich nicht entstehen können“ (MSA IX.60: „ni
[[topic]]“ aborted the export of the commentary).  Only what org reads
as a link (`org-link-bracket-re'): the bracketing of compounds,
„[[[pañca-ākāra] ātmavastu] udbhāvanatā]“, stays as it is."
  (replace-regexp-in-string org-link-bracket-re
                            (lambda (link) (concat "[" (substring link 2 -2) "]"))
                            text t t))

(defun bcat-analysis--no-links-in (object)
  "OBJECT (a parsed answer) with `bcat-analysis-no-links' on every string."
  (cond ((stringp object) (bcat-analysis-no-links object))
        ((proper-list-p object) (mapcar #'bcat-analysis--no-links-in object))
        ((consp object) (cons (bcat-analysis--no-links-in (car object))
                              (bcat-analysis--no-links-in (cdr object))))
        (t object)))

(defun bcat-analysis--decode (json)
  "JSON (a string) as an alist with symbol keys and lists for arrays, its
strings unable to form org links (`bcat-analysis-no-links')."
  (bcat-analysis--no-links-in
   (json-parse-string json :object-type 'alist :array-type 'list
                      :null-object nil :false-object nil)))

(defun bcat-analysis-read (response)
  "(OBJECT . PROBLEM) for Claude's RESPONSE: the JSON object as an alist
\(keys as symbols, arrays as lists) and nil, or nil and a short
description why there is none — an answer cut off at the output limit
says so.  Tolerates a code fence or prose around the object, and mends
what `bcat-analysis--repair-json' mends."
  (if (not (and (stringp response) (string-match "{" response)))
      (cons nil "kein JSON-Objekt")
    (let* ((start (match-beginning 0))
           (end (string-match "}[^}]*\\'" response)))
      (if (not (and end (> end start)))
          (cons nil "JSON-Objekt unvollständig")
        (let* ((json (substring response start (1+ end)))
               (strict (condition-case err
                           (cons (bcat-analysis--decode json) nil)
                         (error (cons nil err))))
               (obj (or (car strict)
                        (and (cdr strict)
                             (ignore-errors
                               (bcat-analysis--decode (bcat-analysis--repair-json json)))))))
          (cond
           ((and (consp obj) (assq 'segments obj)) (cons obj nil))
           ((or obj (null (cdr strict))) (cons nil "JSON ohne segments"))
           ((bcat-analysis--unterminated-p (substring response start))
            (cons nil "abgeschnitten — Grenze `tibetan-claude-max-tokens' erreicht"))
           (t (cons nil (bcat-analysis--json-problem (cdr strict) start)))))))))

(defun bcat-analysis-parse (response)
  "The JSON object in Claude's RESPONSE as an alist, or nil when there
is none (see `bcat-analysis-read')."
  (car (bcat-analysis-read response)))

(defun bcat-analysis-keep-response (file response)
  "Save RESPONSE, an answer for the sentence FILE that could not be
read, under `bcat-analysis-response-directory'; return its path."
  (make-directory bcat-analysis-response-directory t)
  (let ((path (expand-file-name
               (format "%s-%s.txt" (file-name-base file)
                       (format-time-string "%Y%m%d-%H%M%S"))
               bcat-analysis-response-directory)))
    (with-temp-file path (insert (or response "")))
    path))

;; ----------------------------------------------------------------------------
;; Template fields
;; ----------------------------------------------------------------------------

(defconst bcat-analysis--particle-labels
  '("ERG" "GEN" "ABS" "TERM" "LOC" "ABL" "COM" "DAT" "ALL" "INST" "CONV")
  "Function labels of case and converb particles — such a word gets no
dictionary link in the table.")

(defun bcat-analysis--particle-p (word)
  "Non-nil when WORD (an alist with wylie and function) is a particle: a
single syllable whose function begins with a case or converb label.
29.10.: Claude gave the word „shes bya'i“ the function „GEN · second
member of compound“ — several syllables, so a word all the same."
  (let ((function (bcat-analysis--field word 'function)))
    (and (not (string-match-p " " (bcat-analysis--field word 'wylie)))
         (string-match "\\`[ \t]*\\([A-Z]+\\)" function)
         (member (match-string 1 function) bcat-analysis--particle-labels))))

(defun bcat-analysis--field (alist key)
  "String value of KEY in ALIST, trimmed; \"\" when absent."
  (let ((v (alist-get key alist)))
    (if (stringp v) (string-trim v) "")))

(define-obsolete-variable-alias 'bcat-analysis-tibetan-table-width
  'bcat-analysis-table-width "5.11.2026")

(defcustom bcat-analysis-table-width 158
  "Widest table for the words of a segment in the sentence file — every
language (5.11.; before, Tibetan only, Sanskrit had the PDF's 120) —,
in characters as it is seen (each column's widest line, links by their
text, plus 3 for bar and spaces); wider segments are split into several
tables.  Carsten 4.11. (bDen 16, Segment 70, six columns of 119
characters broken in a window of 162: „müsste eigentlich nicht
umbrechen, oder?“): the file fills the MacBook window (162 less the
org-indent prefix); the PDF chunks anew in the export, by its own
measure (`bcat-export-tibetan-table-width').  An exception per language:
`bcat-analysis-layout-overrides'."
  :type 'integer
  :group 'buddhist-cat)

(defcustom bcat-analysis-cell-width 45
  "Narrowest a wide cell of a segment's table is broken to, every
language: between the members of a compound, a member still wider at
its „ · “ (`bcat-analysis--wrap-cell') — nothing is cut.  A segment's
cells are broken only as far as that saves lines
\(`bcat-analysis--best-chunks').  5.11., TĀ 1.1: one parts cell of 89
characters made its column the widest; broken, segment 1 has two
tables instead of five — but AS 3, two columns that fit side by side,
stays unbroken.  An exception per language:
`bcat-analysis-layout-overrides'."
  :type 'integer
  :group 'buddhist-cat)

(defcustom bcat-analysis-layout-overrides nil
  "Exceptions to the generic table layout per source language (the
#+SOURCE_LANG code, \"bo\", \"sa\"): ((LANG . ((KEY . VALUE) …)) …),
KEY one of `table-width', `cell-width', `function-width' — e.g.
\((\"sa\" (cell-width . 60))).  Carsten 5.11.: „alles erstmal generisch
und dann durch Ausnahmen überschrieben“."
  :type '(alist :key-type string :value-type (alist :key-type symbol :value-type integer))
  :group 'buddhist-cat)

(defun bcat-analysis--uchen-syllables (uchen)
  "The syllables of the Tibetan UCHEN — tsheg, shad and spaces dropped."
  (split-string (replace-regexp-in-string "[།༎༏༑༔ \t\n]+" "་" (or uchen "")) "་" t))

(defun bcat-analysis--wylie-key (wylie)
  "WYLIE as compared with the Wylie of Uchen syllables: no spaces or
shad, lower case."
  (downcase (replace-regexp-in-string "[ /]+" "" (or wylie ""))))

(defun bcat-analysis-uchen-words (uchen wylies)
  "The Uchen of each of WYLIES — the words of a segment in order, as in
its table — from the segment's UCHEN; nil for a word whose syllables do
not match (the next words still get theirs).  Carsten 31.10. abends,
Tibetisch V: the Uchen above the Wylie in the tables.  The tool has no
Wylie → Uchen conversion, but Uchen → Wylie per syllable: the words
take the syllables whose Wylie they spell (bDen 9–20: 216 of 216)."
  (let* ((syls (bcat-analysis--uchen-syllables uchen))
         (keys (and (fboundp 'tibetan-to-wylie-fixed)
                    (mapcar (lambda (syl) (bcat-analysis--wylie-key (tibetan-to-wylie-fixed syl))) syls)))
         (i 0) (out '())
         (spell (lambda (target from)
                  ;; The end of the syllables from FROM that spell
                  ;; TARGET, or nil.
                  (let ((acc "") (j from))
                    (while (and (< j (length keys)) (< (length acc) (length target)))
                      (setq acc (concat acc (nth j keys)) j (1+ j)))
                    (and (not (string-empty-p target)) (equal acc target) j)))))
    (dolist (w wylies (nreverse out))
      (let* ((target (bcat-analysis--wylie-key w))
             ;; Here, or — after a word spelled otherwise — a few
             ;; syllables on; the skipped ones are that word's.
             (start (cl-loop for j from i to (min (length keys) (+ i 4))
                             when (funcall spell target j) return j))
             (end (and start (funcall spell target start))))
        (if (null end)
            (push nil out)
          (when (and (> start i) out (null (car out)))
            (setcar out (string-join (cl-subseq syls i start) "་")))
          (push (string-join (cl-subseq syls start end) "་") out)
          (setq i end))))))

(defun bcat-analysis--uchen-row (uchen wylies)
  "The Uchen row for the words WYLIES of the segment UCHEN — \"\" where
a word is not found —, or nil when none is (then no row at all)."
  (let ((row (bcat-analysis-uchen-words uchen wylies)))
    (when (cl-some #'identity row)
      (mapcar (lambda (u) (or u "")) row))))

(defun bcat-analysis-plain-cell (cell)
  "CELL of a gloss table as plain text: links reduced to their text,
`\\vert' back to a bar."
  (replace-regexp-in-string
   "\\\\vert\\(?:{}\\)?" "|"
   (replace-regexp-in-string "\\[\\[[^]]*\\]\\[\\([^]]*\\)\\]\\]" "\\1" (or cell "") t)
   t t))

(defun bcat-analysis--wrap-pieces (pieces sep mark max)
  "PIECES joined by SEP into lines of at most MAX characters, greedily;
every line but the last ends in MARK (room kept for it).  A piece
longer than MAX stays whole."
  (let ((lines '()) (line nil) (room (- max (string-width mark))))
    (dolist (piece pieces)
      (let ((cand (if line (concat line sep piece) piece)))
        (if (and line (> (string-width cand) room))
            (progn (push (concat line mark) lines) (setq line piece))
          (setq line cand))))
    (nreverse (cons line lines))))

(defun bcat-analysis--wrap-cell (text max)
  "TEXT as lines of at most MAX characters (any table, 5.11. generic):
broken between the members of a compound („;“ — whole members kept on
a line) and a member still wider at its „ · “ (31.10., TĀ 1008: 160
characters in one cell); the separator stays at the end of the line —
`bcat-analysis--table-columns' joins the lines again by it.  A piece
longer than MAX stays whole; a cell with a link is never broken."
  (if (or (<= (string-width (bcat-analysis-plain-cell text)) max)
          (string-match-p "\\[\\[" text))
      (list text)
    (let* ((members (split-string text "; "))
           (n (length members))
           (lines '()) (line nil))
      (cl-loop for m in members
               for i from 1
               for end = (if (< i n) ";" "")
               do (cond
                   ((> (string-width m) max)
                    ;; A member wider than MAX: lines of its own.
                    (when line (push (concat line ";") lines) (setq line nil))
                    (let ((parts (bcat-analysis--wrap-pieces (split-string m " · ") " · " " ·" max)))
                      (dolist (l (butlast parts)) (push l lines))
                      (push (concat (car (last parts)) end) lines)))
                   ((and line (> (string-width (concat line "; " m)) (1- max)))
                    (push (concat line ";") lines)
                    (setq line m))
                   (t (setq line (if line (concat line "; " m) m)))))
      (when line (push line lines))
      (nreverse lines))))

(defun bcat-analysis--continued-p (cell)
  "Non-nil when CELL, a line of a broken cell, goes on in the row
below: it ends in a separator (`bcat-analysis--wrap-cell')."
  (string-match-p "\\(?: ·\\|;\\)\\'" cell))

(defun bcat-analysis--join-continuations (rows)
  "ROWS (lists of cells) with the lines of broken cells joined: a row
whose filled cells all stand under a cell that goes on
\(`bcat-analysis--continued-p') belongs to the row above."
  (let ((out '()))
    (dolist (row rows (nreverse out))
      (let ((prev (car out)))
        (if (and prev
                 (cl-some (lambda (c) (not (string-empty-p c))) row)
                 (cl-every (lambda (j)
                             (or (string-empty-p (or (nth j row) ""))
                                 (bcat-analysis--continued-p (or (nth j prev) ""))))
                           (number-sequence 0 (1- (length row)))))
            (setcar out (cl-loop for j below (max (length prev) (length row))
                                 collect (let ((a (or (nth j prev) "")) (b (or (nth j row) "")))
                                           (if (string-empty-p b) a (concat a " " b)))))
          (push row out))))))

(defun bcat-analysis-wrap-rows (rows max)
  "ROWS (lists of cells) with every cell wider than MAX broken at its „·“
\(`bcat-analysis--wrap-cell') into further rows — the other cells of
such a row empty there.  The tables of every language
\(`bcat-analysis--format-chunks', 5.11.) and the export of tables
written before (31.10.)."
  (cl-loop for row in rows
           append (let ((cells (mapcar (lambda (c) (bcat-analysis--wrap-cell c max)) row)))
                    (cl-loop for i below (apply #'max (mapcar #'length cells))
                             collect (mapcar (lambda (c) (or (nth i c) "")) cells)))))

(defun bcat-analysis--table-columns (table)
  "The columns of the org TABLE — each a list of its cells, top to
bottom — in reading order: bands (rows between hlines, the older form of
a wide table) one after the other, their empty padding dropped; the
lines of a broken cell joined again (`bcat-analysis--join-continuations',
5.11.)."
  (let ((bands '()) (band '()))
    (dolist (line (append (split-string table "\n" t) '("|-")))
      (if (string-match-p "\\`[ \t]*|-" line)
          (progn (when band (push (bcat-analysis--join-continuations (nreverse band)) bands))
                 (setq band nil))
        (push (mapcar #'string-trim (butlast (cdr (split-string line "|")))) band)))
    (cl-loop for b in (nreverse bands)
             append (cl-loop for j below (length (car b))
                             for col = (mapcar (lambda (row) (or (nth j row) "")) b)
                             unless (cl-every #'string-empty-p col) collect col))))

(defun bcat-analysis-uchen-table (tables uchen)
  "TABLES — the org tables of a Tibetan segment, blank lines between —
with the Uchen of UCHEN as their first row, when they have none yet
\(tables written before 31.10. abends; C-c u L).  Each table is set
anew from its columns (an older one in bands as well); tables already
in Uchen stay as they are."
  (let ((parts (split-string (or tables "") "\n[ \t]*\n" t)))
    (if (or (null parts) (null uchen)
            (string-match-p "[\u0F00-\u0FFF]" (car (split-string (car parts) "\n"))))
        tables
      (let* ((columns (mapcar #'bcat-analysis--table-columns parts))
             (uchens (bcat-analysis--uchen-row
                      uchen (mapcar (lambda (col) (bcat-analysis-plain-cell (car col)))
                                    (apply #'append columns)))))
        (if (null uchens)
            tables
          (mapconcat (lambda (cols)
                       (let ((cols (cl-mapcar #'cons uchens cols)))
                         (setq uchens (nthcdr (length cols) uchens))
                         (string-trim-right
                          (tibetan-gloss-table--format-rows
                           (cl-loop for r below (length (car cols))
                                    collect (mapcar (lambda (col) (nth r col)) cols))))))
                     columns "\n\n"))))))

(defcustom bcat-analysis-function-width 30
  "Widest function cell in a Tibetan table: a longer function is cut at
its „·“ and the rest goes to the segment's Structure
\(`bcat-analysis--function-split').  Carsten 4.11. (bDen 16 on the
MacBook, „ziemlich viel verschenkter Platz“): one function of 64
characters widened its column, the table broke and left a word alone."
  :type 'integer
  :group 'buddhist-cat)

(defun bcat-analysis--layout (key &optional lang)
  "The value of the layout KEY (`table-width', `cell-width',
`function-width') for the source language LANG: its exception in
`bcat-analysis-layout-overrides', else the generic option."
  (or (alist-get key (cdr (assoc lang bcat-analysis-layout-overrides)))
      (pcase key
        ('table-width bcat-analysis-table-width)
        ('cell-width bcat-analysis-cell-width)
        ('function-width bcat-analysis-function-width))))

(defun bcat-analysis--cut-label (text max)
  "(SHORT . PIECES) for a label TEXT wider than MAX: without its remarks
in parentheses when that fits, else that cut at its last fitting comma,
else at a space with „…“; PIECES what SHORT leaves out, in the order of
TEXT (the remarks without their parentheses).  TĀ 1.1 (5.11.): „āśiṣi
\(precative/benedictive) 3 sg. P, prathamapuruṣa ekavacana (3rd sg.)“
→ „āśiṣi 3 sg. P“."
  (let* ((pieces '()) (pos 0))
    (while (string-match "(\\([^()]*\\))" text pos)
      (push (cons (match-beginning 0) (string-trim (match-string 1 text))) pieces)
      (setq pos (match-end 0)))
    (let* ((bare (string-trim (replace-regexp-in-string
                               "  +" " " (replace-regexp-in-string "[ \t]*([^()]*)" "" text))))
           (bare (if (string-empty-p bare) text bare))
           (cut-at (lambda (str end)
                     ;; (SHORT . REMAINDER) of STR cut before END.
                     (cons (string-trim (substring str 0 end))
                           (string-trim (substring str end) "[ \t,]+")))))
      (if (and pieces (<= (string-width bare) max))
          (cons bare (mapcar #'cdr (nreverse pieces)))
        (let* ((comma (and (string-match-p ", " bare)
                           (cl-loop for i downfrom (min (length bare) (1+ max)) to 1
                                    when (and (< (1+ i) (length bare))
                                              (eq (aref bare i) ?,) (eq (aref bare (1+ i)) ?\s))
                                    return i)))
               (split (if comma
                          (funcall cut-at bare comma)
                        (let ((sp (or (cl-position ?\s bare :end (- max 2) :from-end t) (- max 2))))
                          (cons (concat (string-trim (substring bare 0 sp)) " …")
                                (string-trim (substring bare sp))))))
               (rest (cdr split))
               (at (or (and (not (string-empty-p rest)) (string-search rest text)) (length text))))
          (cons (car split)
                (mapcar #'cdr
                        (sort (append (and (not (string-empty-p rest)) (list (cons at rest)))
                                      (nreverse pieces))
                              (lambda (a b) (< (car a) (car b)))))))))))

(defun bcat-analysis--function-split (fn &optional lang)
  "(SHORT . REST) of the last cell of a word's column — the function of a
Tibetan word, the form of a Sanskrit one (5.11., generic): FN itself
and nil when it is no wider than `bcat-analysis-function-width'; else
the leading „·“ parts that fit and the rest; a first part wider than
that cut by `bcat-analysis--cut-label' (remarks in parentheses out, else
at a comma, else at a space with „…“), what it leaves out in REST.
LANG: the source language, for its exception (`bcat-analysis--layout')."
  (let ((fn (string-trim (or fn "")))
        (max (bcat-analysis--layout 'function-width lang)))
    (if (<= (string-width fn) max)
        (list fn)
      (let* ((parts (split-string fn "[ \t]*·[ \t]*" t))
             (short (car parts))
             (n 1))
        (while (and (< n (length parts))
                    (<= (string-width (concat short " · " (nth n parts))) max))
          (setq short (concat short " · " (nth n parts)) n (1+ n)))
        (if (> (string-width short) max)
            (let ((cut (bcat-analysis--cut-label short max)))
              (cons (car cut)
                    (string-join (delq nil (list (and (cdr cut) (string-join (cdr cut) "; "))
                                                 (and (cdr parts) (string-join (cdr parts) " · "))))
                                 " · ")))
          (cons short (string-join (nthcdr n parts) " · ")))))))

(defun bcat-analysis--trim-lemma (fn wylie)
  "FN, the function of the word WYLIE, without a verb's dictionary form
in parentheses that only repeats the stem the word shows — „V pf.
\(brgyud pa)“ under brgyud pa'i becomes „V pf.“; „V pf. ('pong ba)“
under spangs stays.  Carsten 4.11.: „ist die Wiederholung des Verbs
nicht redundant?“ (54 of 99 verbs in the files)."
  (replace-regexp-in-string
   "[ \t]*(\\([^()]+\\) [pb]a)"
   (lambda (m)
     (let ((stem (save-match-data
                   (string-match "(\\([^()]+\\) [pb]a)" m)
                   (match-string 1 m))))
       (if (string-match-p (concat "\\(?:\\`\\| \\)" (regexp-quote stem) "\\(?:'[a-z]+\\)?\\(?: \\|\\'\\)")
                           (or wylie ""))
           ""
         m)))
   (or fn "") t t))

(defun bcat-analysis--word-name (w)
  "The word W as its table's word row names it: a Tibetan word's Wylie,
a Sanskrit word as in the padapāṭha."
  (let ((wylie (bcat-analysis--field w 'wylie)))
    (if (string-empty-p wylie) (bcat-analysis--field w 'word) wylie)))

(defun bcat-analysis--word-function (w)
  "The last cell of the word W as the table shows it, before cutting —
a Tibetan word's function without a repeated dictionary form
\(`bcat-analysis--trim-lemma'), a Sanskrit word's form."
  (let ((fn (bcat-analysis--field w 'function)))
    (bcat-analysis--trim-lemma (if (string-empty-p fn) (bcat-analysis--field w 'form) fn)
                               (bcat-analysis--word-name w))))

(defun bcat-analysis--function-overflow (words &optional lang)
  "The lines for Structure of the WORDS whose last cell was cut — a
Tibetan function, since 5.11. a Sanskrit form too: „/word/: rest“
\(`bcat-analysis--function-split'; LANG for its exception)."
  (delq nil (mapcar (lambda (w)
                      (let ((rest (cdr (bcat-analysis--function-split (bcat-analysis--word-function w) lang))))
                        (and rest (format "/%s/: %s" (bcat-analysis--word-name w) rest))))
                    words)))

(defun bcat-analysis--with-overflow (structure lines)
  "STRUCTURE with LINES (`bcat-analysis--function-overflow') each on a
line of its own (org line breaks)."
  (if (null lines)
      structure
    (string-join (append (and structure (not (string-blank-p structure)) (list structure)) lines)
                 " \\\\\n")))

(defun bcat-analysis--chunk-columns (columns &optional width charge cell-width)
  "COLUMNS (each a list of its cells, top to bottom) in chunks no wider
than WIDTH (`bcat-analysis-table-width') — each column charged with its
widest line (links by their text; a cell wider than CELL-WIDTH,
`bcat-analysis-cell-width', by the lines it breaks into) plus CHARGE (3:
bar and spaces, the table as seen; the PDF charges 6)."
  (let ((chunks '()) (chunk '()) (used 1)
        (width (or width bcat-analysis-table-width))
        (charge (or charge 3))
        (cell-width (or cell-width bcat-analysis-cell-width)))
    (dolist (col columns)
      (let ((w (+ charge (apply #'max 0 (mapcar (lambda (line) (string-width (bcat-analysis-plain-cell line)))
                                                (cl-mapcan (lambda (c) (bcat-analysis--wrap-cell c cell-width))
                                                           (copy-sequence col)))))))
        (when (and chunk (> (+ used w) width))
          (push (nreverse chunk) chunks)
          (setq chunk nil used 1))
        (push col chunk)
        (setq used (+ used w))))
    (when chunk (push (nreverse chunk) chunks))
    (nreverse chunks)))

(defun bcat-analysis--format-chunks (chunks &optional cell-width)
  "CHUNKS (lists of columns) as org tables, blank lines between — a cell
wider than CELL-WIDTH (`bcat-analysis-cell-width') broken into further
lines (`bcat-analysis-wrap-rows')."
  (mapconcat (lambda (ch)
               (string-trim-right
                (tibetan-gloss-table--format-rows
                 (bcat-analysis-wrap-rows
                  (cl-loop for r below (length (car ch)) collect (mapcar (lambda (col) (nth r col)) ch))
                  (or cell-width bcat-analysis-cell-width)))))
             chunks "\n\n"))

(defun bcat-analysis--chunk-lines (chunks cell-width)
  "The lines CHUNKS take as org tables with cells broken at CELL-WIDTH:
their rows plus a blank line between two tables."
  (+ (max 0 (1- (length chunks)))
     (cl-loop for ch in chunks
              sum (cl-loop for r below (length (car ch))
                           sum (apply #'max 1 (mapcar (lambda (col)
                                                        (length (bcat-analysis--wrap-cell (nth r col) cell-width)))
                                                      ch))))))

(defun bcat-analysis--best-chunks (columns width charge cell-min)
  "(CHUNKS . CELL-WIDTH) for the COLUMNS of a segment at the table WIDTH
\(CHARGE per column): wide cells broken only as far as that saves lines
— of the widths from unbroken down to CELL-MIN (step 5) the one giving
the fewest lines, the widest of those.  5.11.: broken at a fixed 45,
AS 3's two columns, side by side at 120, grew from 5 lines to 8."
  (let ((best nil))
    (dolist (cw (append (number-sequence (- width charge 1) (1+ cell-min) -5) (list cell-min)))
      (let* ((chunks (bcat-analysis--chunk-columns columns width charge cw))
             (lines (bcat-analysis--chunk-lines chunks cw)))
        (when (or (null best) (< lines (car best)))
          (setq best (list lines chunks cw)))))
    (cons (nth 1 best) (nth 2 best))))

(defun bcat-analysis--layout-tables (columns lang)
  "COLUMNS of a segment's table as org tables in the layout of the file
for the source language LANG (`bcat-analysis--layout'): chunked to the
table width, wide cells broken as far as that saves lines
\(`bcat-analysis--best-chunks')."
  (let ((best (bcat-analysis--best-chunks columns (bcat-analysis--layout 'table-width lang) 3
                                          (bcat-analysis--layout 'cell-width lang))))
    (bcat-analysis--format-chunks (car best) (cdr best))))

(defun bcat-analysis-compact-tables (tables &optional lang)
  "(TABLES . OVERFLOW) for the tables of a segment as a file written
before holds them — every language (5.11.; 4.11. Tibetan only): every
last cell (function, form) without a repeated dictionary form
\(`bcat-analysis--trim-lemma') and, wider than
`bcat-analysis-function-width', cut — the rest as Structure lines in
OVERFLOW —, the columns chunked anew and wide cells broken, in the
layout of the file (a word left alone joins the table before when it
fits).  LANG: the source language (for its exception); by default
\"bo\" for tables in Tibetan script, else \"sa\".  TABLES itself and nil
when nothing changes."
  (let* ((parts (split-string (or tables "") "\n[ \t]*\n" t))
         (columns (apply #'append (mapcar #'bcat-analysis--table-columns parts)))
         (lang (or lang (if (and columns (string-match-p "[\u0F00-\u0FFF]" (caar columns))) "bo" "sa")))
         (overflow '()))
    (setq columns
          (mapcar (lambda (col)
                    (let* ((word (bcat-analysis-plain-cell
                                  (nth (if (string-match-p "[\u0F00-\u0FFF]" (car col)) 1 0) col)))
                           (fn (bcat-analysis-plain-cell (car (last col))))
                           (sp (bcat-analysis--function-split (bcat-analysis--trim-lemma fn word) lang)))
                      (when (cdr sp)
                        (push (format "/%s/: %s" word (cdr sp)) overflow))
                      (if (equal (car sp) fn)
                          col
                        (append (butlast col) (list (tibetan-gloss-table--cell (car sp)))))))
                  columns))
    ;; Chunked anew — the width of the file changed on 4.11., the
    ;; breaking of wide cells on 5.11.; the same chunks broken into the
    ;; same lines leave the text as it is.
    (let* ((best (bcat-analysis--best-chunks columns (bcat-analysis--layout 'table-width lang) 3
                                             (bcat-analysis--layout 'cell-width lang)))
           (chunks (car best))
           (out (bcat-analysis--format-chunks chunks (cdr best)))
           (lines (lambda (tbs) (mapcar (lambda (tb) (length (split-string tb "\n" t))) tbs))))
      (if (or (null columns)
              (and (equal chunks (mapcar #'bcat-analysis--table-columns parts))
                   (equal (funcall lines parts) (funcall lines (split-string out "\n\n" t)))))
          (cons tables nil)
        (cons out (nreverse overflow))))))

(defun bcat-analysis-gloss-table (words &optional uchen)
  "The table of WORDS (Claude's word list): row 1 Wylie (with a
Steinert link unless a particle), row 2 gloss, row 3 function — split
into separate tables in the layout of the file
\(`bcat-analysis--layout-tables', `bcat-analysis-table-width').  With
UCHEN, the segment's Tibetan, the word in Uchen above its Wylie
\(Carsten 31.10. abends, Tibetisch V: Uchen and Wylie black, the
function blue).  \"\" for no words."
  (if (null words)
      ""
    (let* ((uchens (and uchen (bcat-analysis--uchen-row
                               uchen (mapcar (lambda (w) (bcat-analysis--field w 'wylie)) words))))
           (row (and uchens t))
           (columns
            (cl-loop
             for w in words
             for u = (pop uchens)
             collect (let ((wy (bcat-analysis--field w 'wylie)))
                       (append (and row (list (or u "")))
                               (list (if (bcat-analysis--particle-p w)
                                         (tibetan-gloss-table--cell wy)
                                       (format "[[steinert:%s][%s]]" wy (tibetan-gloss-table--cell wy)))
                                     (tibetan-gloss-table--cell
                                      (or (tibetan-gloss-table--truncate-cell (bcat-analysis--field w 'gloss)) ""))
                                     (tibetan-gloss-table--cell
                                      (car (bcat-analysis--function-split (bcat-analysis--word-function w) "bo")))))))))
      (bcat-analysis--layout-tables columns "bo"))))

(defun bcat-analysis--bracketed (word gloss)
  "WORD followed by GLOSS in brackets.  A gloss Claude brackets itself
loses its brackets, brackets inside become parentheses — „ni [[topic]]“
is an org link (30.10., MSA IX.60: the commentary's export aborted)."
  (let ((gl (string-trim gloss)))
    (when (string-match "\\`\\[\\([^][]*\\)\\]\\'" gl)
      (setq gl (match-string 1 gl)))
    (format "%s [%s]" word
            (replace-regexp-in-string "\\]" ")" (replace-regexp-in-string "\\[" "(" gl)))))

(defun bcat-analysis-glossed-line (wylie words)
  "WYLIE (one verse line, its shad at the end) with each of WORDS
\(alists with wylie, gloss, function) followed by its gloss in
brackets — a particle by its function label, an unknown (\"?\") or
empty gloss by nothing — and the shad after the last.  Carsten 29.10.:
„die Vokabeln in Klammern beim Text“, so a segment can be translated
from the verse without turning to its table.  WYLIE unchanged when
WORDS do not cover its syllables exactly: the text is never altered."
  (let* ((tail (if (string-match "\\(?:[ \t]*/\\)+[ \t]*\\'" wylie)
                   (match-string 0 wylie)
                 ""))
         (body (substring wylie 0 (- (length wylie) (length tail)))))
    (if (or (null words)
            (not (equal (split-string body)
                        (split-string (mapconcat (lambda (w) (bcat-analysis--field w 'wylie))
                                                 words " ")))))
        wylie
      (concat
       (mapconcat (lambda (w)
                    (let* ((wy (bcat-analysis--field w 'wylie))
                           (fn (bcat-analysis--field w 'function))
                           (gl (bcat-analysis--field w 'gloss))
                           (show (cond ((bcat-analysis--particle-p w) fn)
                                       ((member gl '("" "?")) "")
                                       (t gl))))
                      (if (string-empty-p show) wy (bcat-analysis--bracketed wy show))))
                  words " ")
       (if (string-empty-p tail) "" (concat " " (string-trim tail)))))))

(defun bcat-analysis-glosses-p (segments)
  "t when a verse line of SEGMENTS (template fields) carries glosses —
the template then shows the glossed verse below the plain one."
  (and (cl-some (lambda (s)
                  (let ((g (alist-get 'glossed s)))
                    (and g (not (equal g (alist-get 'wylie s))))))
                segments)
       t))

(defun bcat-analysis--reference-lines (refs)
  "\"*NAME:* TEXT\" for each of REFS (alists with name and text)."
  (mapcar (lambda (r) (format "*%s:* %s" (bcat-analysis--field r 'name)
                              (bcat-analysis--field r 'text)))
          (cl-remove-if (lambda (r) (string-empty-p (bcat-analysis--field r 'text)))
                        refs)))

(defun bcat-analysis-strip-marks (text)
  "TEXT without org bold marks (the meaning-shift marks of a fetch)."
  (replace-regexp-in-string "\\*\\([^*\n]+\\)\\*" "\\1" text t))

(defun bcat-analysis--loose-regexp (text)
  "A regexp for TEXT that lets any run of whitespace stand for its spaces."
  (mapconcat #'regexp-quote (split-string text) "[ \t\n]+"))

(defun bcat-analysis--reference-parts (segments name)
  "The parts of the reference NAME in Claude's SEGMENTS, in segment
order — each with its passages in org bold."
  (delq nil
        (mapcar (lambda (seg)
                  (let ((r (cl-find name (alist-get 'references seg)
                                    :key (lambda (r) (bcat-analysis--field r 'name))
                                    :test #'equal)))
                    (and r (bcat-analysis--field r 'text))))
                (sort (copy-sequence segments)
                      (lambda (a b)
                        (< (string-to-number (format "%s" (alist-get 'num a)))
                           (string-to-number (format "%s" (alist-get 'num b)))))))))

(defun bcat-analysis-mark-reference (text parts)
  "TEXT, a reference translation as given (notes included), with the
meaning-shift marks of PARTS in org bold.  PARTS are Claude's parts of
that reference, segment by segment, their passages in org bold: each
part is looked for after the previous one and its passages are marked
where it stands — never at an earlier occurrence of the same words.
Whitespace may differ; a part or passage not found changes nothing.
The text itself is never taken from the answer (29.10.: Claude's copy
of Lindtner's verse 9 dropped his note)."
  (let ((case-fold-search nil) (cursor 0) (spans '()))
    (dolist (part parts)
      (let* ((plain (string-trim (bcat-analysis-strip-marks part)))
             (passages (let ((ps '()) (start 0))
                         (while (string-match "\\*\\([^*\n]+\\)\\*" part start)
                           (push (string-trim (match-string 1 part)) ps)
                           (setq start (match-end 0)))
                         (nreverse ps)))
             (region (and (not (string-empty-p plain))
                          (string-match (bcat-analysis--loose-regexp plain) text cursor)
                          (cons (match-beginning 0) (match-end 0))))
             (pos (if region (car region) cursor)))
        (dolist (p passages)
          (when (and (not (string-empty-p p))
                     (string-match (bcat-analysis--loose-regexp p) text pos)
                     (or (null region) (<= (match-end 0) (cdr region))))
            (push (cons (match-beginning 0) (match-end 0)) spans)
            (setq pos (match-end 0))))
        (setq cursor (if region (cdr region) pos))))
    ;; SPANS run from the last to the first: inserting from the end
    ;; keeps the earlier positions valid.
    (dolist (sp spans text)
      (setq text (concat (substring text 0 (car sp)) "*"
                         (substring text (car sp) (cdr sp)) "*"
                         (substring text (cdr sp)))))))

(defun bcat-analysis-shift-lines (seg)
  "The „→“ lines of SEG (Claude's segment object): its shift, one line,
or — the Sanskrit contract — a list with one line per translator.  A
line with nothing after the translator's name is dropped (29.10., TĀ
1007: „Dyczkowski 2023: []“)."
  (let ((sh (alist-get 'shift seg)))
    (mapcar (lambda (l) (concat "→ " (string-trim l)))
            (cl-remove-if (lambda (l)
                            (or (not (stringp l))
                                (string-blank-p l)
                                (string-match-p "\\`[^:\n]*:[ \t]*\\(?:\\[\\]\\|\"\"\\)?[ \t]*\\'" l)))
                          (if (listp sh) sh (list sh))))))

(defun bcat-analysis-translations-block (num seg)
  "The left-aligned translation block of segment NUM from SEG (Claude's
segment object): ⟦NUM⟧ own translation (labelled like the sentence's —
29.10.: unlabelled it did not read as a translation next to the named
references), the reference parts, the shift line — one line each, org
line breaks between."
  (let ((lines (append
                (let ((tr (bcat-analysis--field seg 'translation)))
                  (unless (string-empty-p tr) (list (format "⟦%d⟧ *Translation:* %s" num tr))))
                (bcat-analysis--reference-lines (alist-get 'references seg))
                (bcat-analysis-shift-lines seg))))
    (mapconcat #'identity lines " \\\\\n")))

(defun bcat-analysis-footnotes-last (text)
  "TEXT — paragraphs between blank lines — with its footnote definitions
\(paragraphs beginning „[fn:“) at the end, in their order; TEXT itself
when it has none.  Carsten 1.11., SDA 10: „Dharma Mitra und Apple [sind]
mit in die Fußnote gewandert“ — Lindtner's notes come with his
reference as org footnotes, and a definition runs on to the next
heading, definition or two blank lines: it swallowed every paragraph
after it.  At the end of Translations compared the next heading ends
it.  Only the text before TEXT's first heading counts: the units of the
comparison below it (*** Übersicht …, 2.11.) stay where they are."
  (let* ((text (or text ""))
         (cut (and (string-match "^\\*+ " text) (match-beginning 0)))
         (head (if cut (substring text 0 cut) text))
         (paras (split-string head "\n[ \t]*\n" t "[ \t\n]+")))
    (if (not (cl-some (lambda (p) (string-prefix-p "[fn:" p)) paras))
        text
      (concat (string-join (append (cl-remove-if (lambda (p) (string-prefix-p "[fn:" p)) paras)
                                   (cl-remove-if-not (lambda (p) (string-prefix-p "[fn:" p)) paras))
                           "\n\n")
              (if cut (concat "\n\n" (substring text cut)) "")))))

(defconst bcat-analysis--comparison-labels
  '(("de" (overview . "Übersicht") (passages . "Die Stellen im Einzelnen")
     (proposal . "Übersetzungsvorschlag") (corrections . "Berichtigungen zur Analyse")
     (columns "Nr." "Stelle" "Einstufung" "Kern")
     (finding . "Befund") (weight . "Gewicht") (decisions . "Entscheidungen")
     (sigla . "Sigel") (own . "Translation")
     (class . "Für die Klasse") (wrong . "abzulehnen") (doubtful . "fraglich")
     (because . "weil") (better . "Besser")
     (grammar . "Grammatik") (grammar-all . "alle Lesarten möglich") (grammar-one . "nur eine Lesart möglich")
     (decision . "Entscheidung") (certain . "sicher") (likely . "wahrscheinlich") (open . "offen")
     (impact . "Auswirkung") (low . "gering") (medium . "mittel") (high . "groß")
     (green . "grün") (yellow . "gelb") (red . "rot"))
    ("en" (overview . "Overview") (passages . "The passages")
     (proposal . "Proposed translation") (corrections . "Corrections to the analysis")
     (columns "No." "Passage" "Rating" "Point")
     (finding . "Finding") (weight . "Weight") (decisions . "Decisions")
     (sigla . "Sigla") (own . "Translation")
     (class . "For the class") (wrong . "wrong") (doubtful . "doubtful")
     (because . "because") (better . "Better")
     (grammar . "Grammar") (grammar-all . "all readings possible") (grammar-one . "only one reading possible")
     (decision . "Decision") (certain . "certain") (likely . "likely") (open . "open")
     (impact . "Impact") (low . "low") (medium . "medium") (high . "high")
     (green . "green") (yellow . "yellow") (red . "red")))
  "The words of the comparison in the file, by
`bcat-analysis-comparison-language' — the Muster's (2.11.) for German.")

(defconst bcat-analysis--rating-colours
  '((green . "ok") (yellow . "mid") (red . "bad"))
  "The cell colour of each rating — the colours ok, mid, bad of the
Muster's xcolor definitions (`bcat-export--font-lines').")

(defconst bcat-analysis--comparison-table-attr
  "#+ATTR_LATEX: :align rp{4.3cm}p{1.6cm}p{8cm} :font \\small"
  "The overview table's fixed column widths — without them it runs over
the page (the Muster's).")

(defvar bcat-analysis--comparison-lang "en"
  "Language code of the comparison being rendered — bound by
`bcat-analysis-comparison-units' (`bcat-analysis-comparison-language' or
the document's target language).")

(defun bcat-analysis--comparison-labels-for ()
  "The words of `bcat-analysis--comparison-lang' (English when that
language has none)."
  (cdr (or (assoc bcat-analysis--comparison-lang bcat-analysis--comparison-labels)
           (assoc "en" bcat-analysis--comparison-labels))))

(defun bcat-analysis--comparison-label (key)
  "The word for KEY in the language of the comparison being rendered."
  (alist-get key (bcat-analysis--comparison-labels-for)))

(defun bcat-analysis--rating (rating)
  "The RATING of a unit as `green', `yellow' or `red' — also given as
grün/gelb/rot or ok/mid/bad —, or nil."
  (pcase (downcase (string-trim (or rating "")))
    ((or "green" "grün" "ok") 'green)
    ((or "yellow" "gelb" "mid") 'yellow)
    ((or "red" "rot" "bad") 'red)))

(defun bcat-analysis--rating-word (rating)
  "RATING (Claude's) as the word of the file — the raw text when unknown."
  (let ((r (bcat-analysis--rating rating)))
    (if r (bcat-analysis--comparison-label r) (string-trim (or rating "")))))

(defun bcat-analysis--table-cell (text)
  "TEXT as one cell of an org table: one line, a bar as \\vert{}."
  (replace-regexp-in-string
   "|" "\\vert{}"
   (replace-regexp-in-string "[ \t]*\n[ \t]*" " " (string-trim (or text ""))) t t))

(defun bcat-analysis--aligned-table (header rows)
  "HEADER and ROWS (lists of cells) as an org table aligned in the raw
file, an hline under the header; the first column (numbers) to the right."
  (let* ((all (cons header rows))
         (widths (cl-loop for i below (length header)
                          collect (cl-loop for r in all maximize (string-width (or (nth i r) "")))))
         (line (lambda (r)
                 (concat "| "
                         (mapconcat
                          (lambda (i)
                            (let* ((c (or (nth i r) "")) (pad (make-string (- (nth i widths) (string-width c)) ?\s)))
                              (if (= i 0) (concat pad c) (concat c pad))))
                          (number-sequence 0 (1- (length header))) " | ")
                         " |"))))
    (concat (funcall line header) "\n"
            "|" (mapconcat (lambda (w) (make-string (+ w 2) ?-)) widths "+") "|\n"
            (mapconcat line rows "\n"))))

(defun bcat-analysis--class-conclusions (units)
  "*** Für die Klasse from UNITS (Carsten 2.11.: „x übersetzt so und so,
ich bin mir nicht sicher, ob das richtig ist weil … Dafür müsste ich es
übersichtlich sehen“): per unit with doubts — red first, then by number —
„- *N. /stelle/ — rot*“ and a line per doubt: who and how —
*abzulehnen*/*fraglich*, weil …  Besser: …; nil without doubts."
  (let* ((label #'bcat-analysis--comparison-label)
         (one (lambda (s) (replace-regexp-in-string "[ \t]*\n[ \t]*" " " s)))
         (word (lambda (key value)
                 ;; the label of a closed value (certain, low …), or the value
                 (let ((sym (intern-soft (downcase (string-trim (or value ""))))))
                   (or (and sym (memq sym (if (eq key 'certainty) '(certain likely open) '(low medium high)))
                            (funcall label sym))
                       (string-trim (or value ""))))))
         (items
          (cl-loop for u in units for i from 1
                   for rating = (bcat-analysis--rating (bcat-analysis--field u 'rating))
                   for doubts = (cl-remove-if (lambda (d) (string-empty-p (bcat-analysis--field d 'who)))
                                              (alist-get 'doubts u))
                   for grammar = (bcat-analysis--field u 'grammatical)
                   for decision = (or (not (string-empty-p (bcat-analysis--field u 'certainty)))
                                      (not (string-empty-p (bcat-analysis--field u 'impact))))
                   when (and (memq rating '(yellow red))
                             (or doubts (not (string-empty-p grammar)) decision))
                   collect (list (if (eq rating 'red) 0 1) i u doubts))))
    (when items
      (concat
       "*** " (funcall label 'class) "\n"
       (mapconcat
        (lambda (item)
          (pcase-let ((`(,_ ,i ,u ,doubts) item))
            (concat
             (format "- *%d. %s — %s*" i (funcall one (bcat-analysis--field u 'passage))
                     (bcat-analysis--rating-word (bcat-analysis--field u 'rating)))
             (let ((g (downcase (bcat-analysis--field u 'grammatical)))
                   (note (string-remove-suffix "." (bcat-analysis--field u 'grammar_note))))
               (if (not (member g '("all" "one")))
                   ""
                 (concat "\n  - " (funcall label 'grammar) ": "
                         (funcall label (if (equal g "all") 'grammar-all 'grammar-one))
                         (if (string-empty-p note) "" (concat " — " (funcall one note)))
                         ".")))
             (mapconcat
              (lambda (d)
                (let* ((verdict (if (equal (downcase (bcat-analysis--field d 'verdict)) "wrong")
                                    'wrong 'doubtful))
                       ;; Claude may begin with the conjunction the line
                       ;; sets itself (4.11., bDen 15: „because because“).
                       (reason (string-remove-suffix
                                "." (replace-regexp-in-string
                                     "\\`\\(?:[Bb]ecause\\|[Ss]ince\\|[Aa]s\\|[Ww]eil\\|[Dd]a\\)[ \t]+" ""
                                     (bcat-analysis--field d 'reason))))
                       (better (bcat-analysis--field d 'better)))
                  (concat "\n  - " (funcall one (bcat-analysis--field d 'who))
                          " — *" (funcall label verdict) "*"
                          (if (string-empty-p reason) ""
                            (concat ", " (funcall label 'because) " " (funcall one reason)))
                          "."
                          (if (string-empty-p better) ""
                            (format " %s: %s" (funcall label 'better)
                                    (string-remove-suffix "." (funcall one better))))
                          (if (string-empty-p better) "" "."))))
              doubts "")
             (let ((c (bcat-analysis--field u 'certainty))
                   (im (bcat-analysis--field u 'impact))
                   (note (string-remove-suffix "." (bcat-analysis--field u 'impact_note))))
               (if (and (string-empty-p c) (string-empty-p im))
                   ""
                 (concat "\n  - "
                         (string-join
                          (delq nil (list (and (not (string-empty-p c))
                                               (format "%s: *%s*" (funcall label 'decision)
                                                       (funcall word 'certainty c)))
                                          (and (not (string-empty-p im))
                                               (format "%s: *%s*" (funcall label 'impact)
                                                       (funcall word 'impact im)))))
                          " · ")
                         (if (string-empty-p note) "" (concat " — " (funcall one note)))
                         "."))))))
        (sort items (lambda (a b) (if (= (car a) (car b)) (< (nth 1 a) (nth 1 b)) (< (car a) (car b)))))
        "\n")))))

(defun bcat-analysis-comparison-proposal (parsed &optional target)
  "The proposal of PARSED for the place under the segment tables — per
sense „*Proposed translation — LABEL:*“ (without a label „*Proposed
translation:*“) and its quote, then „*Decisions:* …“ — in
`bcat-analysis-comparison-language', else in the target language
TARGET; \"\" without a proposal.  Carsten 4.11.: „Darunter könnte ja
dann die Grammar stehen und danach die proposed translation und die
decisions. Die ursprüngliche (Claude) translation dann nur in der
Comparison.“"
  (let* ((bcat-analysis--comparison-lang (or bcat-analysis-comparison-language target "en"))
         (word (bcat-analysis--comparison-label 'proposal))
         (proposal (cl-remove-if (lambda (p) (string-empty-p (bcat-analysis--field p 'text)))
                                 (alist-get 'proposal parsed)))
         (decisions (bcat-analysis--field parsed 'decisions)))
    (if (null proposal)
        ""
      (string-join
       (append
        (mapcar (lambda (p)
                  (let ((l (bcat-analysis--field p 'label)))
                    (format "*%s:*\n#+begin_quote\n%s\n#+end_quote"
                            (if (string-empty-p l) word (concat word " — " l))
                            (bcat-analysis--field p 'text))))
                proposal)
        (unless (string-empty-p decisions)
          (list (format "*%s:* %s" (bcat-analysis--comparison-label 'decisions) decisions))))
       "\n\n"))))

(defun bcat-analysis-comparison-units (parsed &optional refs target)
  "The comparison of the translations unit by unit from PARSED (Carsten
2.11., Muster vergleich-ta1-1.org): *** Übersicht — the sigla of REFS
and the table Nr. | Stelle | Einstufung | Kern, each rating with its
cell colour —, *** Die Stellen im Einzelnen (per unit the readings, the
finding, the weight), *** Berichtigungen zur Analyse; \"\" without
units.  The proposal stands under the tables since 4.11.
\(`bcat-analysis-comparison-proposal').  In
`bcat-analysis-comparison-language', else in the target language TARGET
\(Carsten 2.11.: „im Handout auch immer alles in der Zielsprache“)."
  (let ((bcat-analysis--comparison-lang (or bcat-analysis-comparison-language target "en"))
        (units (alist-get 'units parsed))
        (corrections (cl-remove-if-not (lambda (c) (and (stringp c) (not (string-blank-p c))))
                                       (alist-get 'corrections parsed)))
        (label #'bcat-analysis--comparison-label))
    (if (null units)
        ""
      (string-join
       (delq nil
             (list
              (concat "*** " (funcall label 'overview) "\n"
                      (funcall label 'sigla) ": C = " (funcall label 'own)
                      (mapconcat (lambda (s) (format ", %s = %s" (cdr s) (car s)))
                                 (bcat-analysis-sigla (mapcar #'car refs)) "")
                      "\n\n" bcat-analysis--comparison-table-attr "\n"
                      (bcat-analysis--aligned-table
                       (alist-get 'columns (bcat-analysis--comparison-labels-for))
                       (cl-loop for u in units for i from 1
                                for r = (bcat-analysis--rating (bcat-analysis--field u 'rating))
                                collect (list (number-to-string i)
                                              (bcat-analysis--table-cell (bcat-analysis--field u 'passage))
                                              (concat (if r (format "@@latex:\\cellcolor{%s}@@"
                                                                    (alist-get r bcat-analysis--rating-colours))
                                                        "")
                                                      (bcat-analysis--table-cell
                                                       (bcat-analysis--rating-word (bcat-analysis--field u 'rating))))
                                              (bcat-analysis--table-cell (bcat-analysis--field u 'core))))))
              (bcat-analysis--class-conclusions units)
              (concat "*** " (funcall label 'passages) "\n"
                      (mapconcat
                       (lambda (iu)
                         (let* ((u (cdr iu))
                                (one (lambda (s) (replace-regexp-in-string "[ \t]*\n[ \t]*" " " s)))
                                (finding (bcat-analysis--field u 'finding))
                                (weight (bcat-analysis--field u 'weight)))
                           (string-join
                            (append
                             (list (format "**** %d. %s — %s" (car iu)
                                           (funcall one (bcat-analysis--field u 'passage))
                                           (bcat-analysis--rating-word (bcat-analysis--field u 'rating))))
                             (delq nil (mapcar (lambda (r)
                                                 (let ((text (bcat-analysis--field r 'text)))
                                                   (unless (string-empty-p text)
                                                     (format "- %s :: %s" (bcat-analysis--field r 'sigla)
                                                             (funcall one text)))))
                                               (alist-get 'readings u)))
                             (unless (string-empty-p finding)
                               (list (format "*%s:* %s" (funcall label 'finding) (funcall one finding))))
                             (unless (string-empty-p weight)
                               (list (format "*%s:* %s" (funcall label 'weight) (funcall one weight)))))
                            "\n")))
                       (cl-loop for u in units for i from 1 collect (cons i u))
                       "\n\n"))
              (when corrections
                (concat "*** " (funcall label 'corrections) "\n"
                        (mapconcat (lambda (c) (concat "- " (string-trim c))) corrections "\n")))))
       "\n\n"))))

(defun bcat-analysis--full-names (parsed refs)
  "PARSED with the sigla of REFS (`bcat-analysis-sigla') named again
where Claude used them outside the units: the name of a segment's
reference part, the translators at the start of a shift line
\(„L, DM: …“ → „Lindtner 1981, DharmaMitra: …“).  4.11., bDen 16:
since the prompt names the sigla, the parts came as „L“ — and the marks
of Translations compared, looked up by „Lindtner 1981“, were lost."
  (let ((names (mapcar (lambda (s) (cons (cdr s) (car s))) (bcat-analysis-sigla (mapcar #'car refs)))))
    (if (null names)
        parsed
      (let ((shift-line
             (lambda (l)
               (if (and (stringp l)
                        (string-match "\\`\\(\\*?\\)\\([^:„\"\n]+?\\)\\([ \t]*[:„\"]\\)" l))
                   (let* ((star (match-string 1 l)) (rest (substring l (match-beginning 3)))
                          (tokens (split-string (match-string 2 l) "[ \t]*,[ \t]*" t)))
                     (if (cl-every (lambda (tk) (assoc tk names)) tokens)
                         (concat star (mapconcat (lambda (tk) (cdr (assoc tk names))) tokens ", ") rest)
                       l))
                 l))))
        (cons (cons 'segments
                    (mapcar
                     (lambda (seg)
                       (let ((sh (alist-get 'shift seg)))
                         (append
                          `((references . ,(mapcar (lambda (r)
                                                     (let ((full (cdr (assoc (bcat-analysis--field r 'name) names))))
                                                       (if full (cons (cons 'name full) r) r)))
                                                   (alist-get 'references seg)))
                            (shift . ,(if (listp sh) (mapcar shift-line sh) (funcall shift-line sh))))
                          seg)))
                     (alist-get 'segments parsed)))
              parsed)))))

(defun bcat-analysis-comparison (parsed &optional refs target)
  "Translations compared of the sentence: the own translation from
PARSED, each of REFS ((NAME . TEXT) …, the texts sent with the request)
with the marks of its segment parts, the comparison paragraph of an
older answer, then the units of the comparison in the target language
TARGET (`bcat-analysis-comparison-units')."
  (let ((segments (alist-get 'segments parsed))
        (units (bcat-analysis-comparison-units parsed refs target)))
    (concat
     (bcat-analysis-footnotes-last
     (string-join
     (delq nil
           (append
            (let ((tr (bcat-analysis--field parsed 'translation)))
              (unless (string-empty-p tr) (list (concat "*Translation:* " tr))))
            (mapcar (lambda (r)
                      (let ((text (string-trim (or (cdr r) ""))))
                        (unless (string-empty-p text)
                          (format "*%s:* %s" (car r)
                                  (bcat-analysis-mark-reference
                                   text (bcat-analysis--reference-parts segments (car r)))))))
                    refs)
            (let ((c (bcat-analysis--field parsed 'comparison)))
              (unless (string-empty-p c) (list c)))))
     "\n\n"))
     (if (string-empty-p units) "" (concat "\n\n" units)))))

(defun bcat-analysis--second-translation (parsed)
  "The fields of the translation in the second language PARSED gives —
`second-translation' and its label `second-label' (*Deutsch:*,
*English:*) — or nil."
  (cl-loop for (key . label) in '((translation_de . "Deutsch") (translation_en . "English"))
           for text = (bcat-analysis--field parsed key)
           unless (string-empty-p text)
           return `((second-translation . ,text) (second-label . ,label))))

(defun bcat-analysis--glossed (seg c)
  "The glossed Wylie line of the Tibetan segment SEG (rump fields) with
the words of C, Claude's object for it — whatever the rump had when the
words do not cover the line."
  (let* ((wy (alist-get 'wylie seg))
         (g (and wy (bcat-analysis-glossed-line wy (alist-get 'words c)))))
    (if (or (null g) (equal g wy))
        (or (alist-get 'glossed seg) wy)
      g)))

(defun bcat-analysis-context (base parsed &optional refs)
  "Template fields of a fetched sentence: BASE (the rump fields —
header, segments with num and wylie, user sections) completed from
PARSED (Claude's answer) and REFS, the reference translations sent
with the request ((NAME . TEXT) …).  Segments are matched by number; a
segment Claude skipped keeps its rump fields.  Sigla used as names are
named again (`bcat-analysis--full-names')."
  (let* ((parsed (bcat-analysis--full-names parsed refs))
         (by-num (mapcar (lambda (s) (cons (alist-get 'num s) s))
                         (alist-get 'segments parsed)))
         (segments
          (mapcar
           (lambda (seg)
             (let* ((num (alist-get 'num seg))
                    (c (cdr (assoc (if (stringp num) (string-to-number num) num)
                                   by-num))))
               (if (null c)
                   seg
                 (append
                  `((glossed . ,(bcat-analysis--glossed seg c))
                    (gloss-table . ,(bcat-analysis-gloss-table (alist-get 'words c)
                                                               (alist-get 'uchen seg)))
                    (translations . ,(bcat-analysis-translations-block
                                      (if (stringp num) (string-to-number num) num) c))
                    (segment-translation . ,(bcat-analysis--field c 'translation))
                    (structure . ,(bcat-analysis--with-overflow
                                   (bcat-analysis--field c 'structure)
                                   (bcat-analysis--function-overflow (alist-get 'words c) "bo")))
                    (note . ,(bcat-analysis--field c 'note))
                    (concepts . ,(bcat-analysis--field c 'concepts)))
                  seg))))
           (alist-get 'segments base))))
    (append
     (bcat-analysis--second-translation parsed)
     `((segments . ,segments)
       (glosses . ,(bcat-analysis-glosses-p segments))
       (translation . ,(bcat-analysis--field parsed 'translation))
       (grammar . ,(bcat-analysis--field parsed 'grammar))
       (comparison . ,(bcat-analysis-comparison parsed refs (alist-get 'target-lang base)))
       (proposal . ,(bcat-analysis-comparison-proposal parsed (alist-get 'target-lang base))))
     base)))

;; ----------------------------------------------------------------------------
;; Sanskrit (§5.74, Carsten 29.10.)
;; ----------------------------------------------------------------------------

(defcustom bcat-analysis-sanskrit-table-width 120
  "Widest Sanskrit table in the PDF (the export breaks a wider cell,
`bcat-export--wrap-wide-tables').  Since 5.11. the sentence file has the
width of every language (`bcat-analysis-table-width').  120 (30.10.,
was 95): in the PDF of AS sentence 3 a table of 115 characters fits the
page, one of 134 runs into the margin; Carsten wanted compound and
upādāya side by side as in the preview."
  :type 'integer
  :group 'buddhist-cat)

(defun bcat-analysis-sanskrit-tables (words)
  "The tables of a Sanskrit segment's WORDS (Claude's padapāṭha: word,
formation, parts, gloss, form — five rows) in the layout of every
language (5.11., Carsten: „generisch machen und mit anwenden“;
`bcat-analysis--layout-tables'): split into tables of the file's width,
a wide cell broken at its members into further lines (31.10., TĀ 1008:
a one-word table cannot get narrower by columns), the form cut like a
Tibetan function — its rest goes to Structure
\(`bcat-analysis--function-overflow').  \"\" for no words."
  (if (null words)
      ""
    (bcat-analysis--layout-tables
     (mapcar (lambda (w)
               (append (mapcar (lambda (k) (tibetan-gloss-table--cell (bcat-analysis--field w k)))
                               '(word formation parts gloss))
                       (list (tibetan-gloss-table--cell
                              (car (bcat-analysis--function-split (bcat-analysis--word-function w) "sa"))))))
             words)
     "sa")))

(defun bcat-analysis-padapatha-line (samhita words &optional plain)
  "The padapāṭha line of a segment for * Text: WORDS (Claude's
padapāṭha) each with its gloss in brackets, then the daṇḍas closing the
transmitted line SAMHITA.  PLAIN: the words alone and the whole close
of SAMHITA, daṇḍas and verse number (Carsten 29.10.: the Text in pure
IAST with the sandhis resolved).  The AS edition's daṇḍa, a full stop,
closes the line set close (30.10.).  nil without words."
  (when words
    (let ((close (and (string-match "\\(\\([/.]+\\(?:[ \t]*/+\\)*\\)[ \t]*[0-9]*[a-z]*\\)[ \t]*\\'" samhita)
                      (string-trim (match-string (if plain 1 2) samhita)))))
      (concat (mapconcat (lambda (w)
                           (let ((wd (bcat-analysis--field w 'word))
                                 (gl (bcat-analysis--field w 'gloss)))
                             (if (or plain (string-empty-p gl)) wd (bcat-analysis--bracketed wd gl))))
                         words " ")
              (cond ((null close) "")
                    ((string-prefix-p "." close) close)
                    (t (concat " " close)))))))

(defun bcat-analysis--word-entries (entries)
  "ENTRIES (alists with word and analysis) as \"/word/ — analysis\"."
  (mapcar (lambda (e) (format "/%s/ — %s" (bcat-analysis--field e 'word)
                              (bcat-analysis--field e 'analysis)))
          (cl-remove-if (lambda (e) (string-empty-p (bcat-analysis--field e 'analysis)))
                        entries)))

(defun bcat-analysis-word-analysis (segments)
  "The `** Word Analysis' body for Claude's Sanskrit SEGMENTS, in the
format its readers parse (zettel highlighting, discussion, padapāṭha
PDF): per segment `*** Segment N', the padapāṭha, then `- WORD —
LEMMA; FORM' — \"\" when no segment has words."
  (mapconcat
   (lambda (seg)
     (let ((words (alist-get 'words seg)))
       (concat (format "*** Segment %s\n" (alist-get 'num seg))
               (mapconcat (lambda (w) (bcat-analysis--field w 'word)) words " ")
               "\n\n"
               (mapconcat (lambda (w)
                            (format "- %s — %s; %s" (bcat-analysis--field w 'word)
                                    (bcat-analysis--field w 'lemma)
                                    (bcat-analysis--field w 'form)))
                          words "\n"))))
   (cl-remove-if-not (lambda (seg) (alist-get 'words seg)) segments)
   "\n\n"))

(defun bcat-analysis--prose-recitation (base parsed)
  "The recitation fields of a prose sentence from PARSED's `recitation'
\(lines of sense units, ¦ a breath pause): each line in Devanāgarī over
IAST, under a short header — when BASE has no scansion (no verse) and
the lines are the transmitted text, broken at its spaces; else nil.
31.10., TĀ 1008: prose had only the text as one long line."
  (let* ((lines (cl-remove-if-not #'stringp (alist-get 'recitation parsed)))
         (norm (lambda (s) (string-trim (replace-regexp-in-string
                                         "[ \t\n]+" " " (replace-regexp-in-string "¦" " " s)))))
         (text (mapconcat (lambda (seg) (or (alist-get 'samhita seg) ""))
                          (alist-get 'segments base) " ")))
    (when (and lines (not (alist-get 'scansion base))
               (equal (funcall norm (string-join lines " ")) (funcall norm text)))
      `((recitation . ,(mapconcat (lambda (line)
                                    (let ((l (string-trim (replace-regexp-in-string
                                                           "[ \t]+" " "
                                                           (replace-regexp-in-string "[ \t]*¦[ \t]*" " ¦ " line)))))
                                      (concat (tibetan-sanskrit-script-iast-to-devanagari l) "\n" l)))
                                  lines "\n"))
        (metre . "Prosa — zum lauten Lesen in Sinnabschnitten · ¦ kurze Atempause")))))

(defun bcat-analysis-sanskrit-context (base parsed &optional refs)
  "Template fields of a fetched Sanskrit sentence: BASE (the rump —
header, recitation, segments with num and samhita, user slots)
completed from PARSED (Claude's answer, Sanskrit contract) and REFS,
the reference translations sent ((NAME . TEXT) …).  A segment Claude
skipped keeps its rump fields; sigla used as names are named again
\(`bcat-analysis--full-names')."
  (let* ((parsed (bcat-analysis--full-names parsed refs))
         (by-num (mapcar (lambda (s) (cons (alist-get 'num s) s))
                         (alist-get 'segments parsed)))
         (segments
          (mapcar
           (lambda (seg)
             (let* ((num (alist-get 'num seg))
                    (n (if (stringp num) (string-to-number num) num))
                    (c (cdr (assoc n by-num)))
                    (words (alist-get 'words c)))
               (if (null c)
                   seg
                 (append
                  `((padapatha . ,(or (bcat-analysis-padapatha-line
                                       (or (alist-get 'samhita seg) "") words t)
                                      (alist-get 'padapatha seg)))
                    (glossed . ,(bcat-analysis-padapatha-line
                                 (or (alist-get 'samhita seg) "") words))
                    (gloss-table . ,(bcat-analysis-sanskrit-tables words))
                    (translations . ,(bcat-analysis-translations-block n c))
                    (segment-translation . ,(bcat-analysis--field c 'translation))
                    (sandhi . ,(bcat-analysis--field c 'sandhi))
                    (forms . ,(mapconcat (lambda (l) (concat "- " l))
                                         (bcat-analysis--word-entries (alist-get 'forms c))
                                         "\n"))
                    (compounds . ,(string-join
                                   (bcat-analysis--word-entries (alist-get 'compounds c))
                                   " · "))
                    (structure . ,(bcat-analysis--with-overflow
                                   (bcat-analysis--field c 'structure)
                                   (bcat-analysis--function-overflow words "sa")))
                    (note . ,(bcat-analysis--field c 'note))
                    (concepts . ,(bcat-analysis--field c 'concepts)))
                  seg))))
           (alist-get 'segments base))))
    (append
     (bcat-analysis--prose-recitation base parsed)
     (bcat-analysis--second-translation parsed)
     `((segments . ,segments)
       (glosses . ,(and (cl-some (lambda (s) (alist-get 'glossed s)) segments) t))
       (translation . ,(bcat-analysis--field parsed 'translation))
       (grammar . ,(bcat-analysis--field parsed 'grammar))
       (comparison . ,(bcat-analysis-comparison parsed refs (alist-get 'target-lang base)))
       (proposal . ,(bcat-analysis-comparison-proposal parsed (alist-get 'target-lang base)))
       (word-analysis . ,(bcat-analysis-word-analysis (alist-get 'segments parsed)))
       (bhasya-hint . ,(bcat-analysis--field parsed 'bhasya_hint)))
     base)))

;; ----------------------------------------------------------------------------
;; Bilingual: Sanskrit with the Tibetan translation (§5.74, Carsten 30.10.)
;; ----------------------------------------------------------------------------

(defun bcat-analysis--num (alist &optional key)
  "The number under KEY (default `num') of ALIST as an integer — Claude
writes numbers, now and then as strings — or nil."
  (let ((v (alist-get (or key 'num) alist)))
    (cond ((integerp v) v)
          ((and (stringp v) (string-match-p "\\`[ \t]*[0-9]+[ \t]*\\'" v))
           (string-to-number v)))))

(defun bcat-analysis--num-ranges (nums)
  "NUMS (ascending) for a heading: runs as ranges, the rest by comma —
\"40–44\", \"77, 79–80\"; \"\" for none."
  (let ((runs '()))
    (dolist (n nums)
      (if (and runs (= n (1+ (cdar runs))))
          (setcdr (car runs) n)
        (push (cons n n) runs)))
    (mapconcat (lambda (r) (if (= (car r) (cdr r))
                               (number-to-string (car r))
                             (format "%d–%d" (car r) (cdr r))))
               (nreverse runs) ", ")))

(defun bcat-analysis--assignment (tibetan parsed sa-nums)
  "Alist (TIB-NUM . SA-NUM) for the Tibetan segments TIBETAN (alists
with num) from the `sanskrit' fields of PARSED (the answer of the
Tibetan request).  A Tibetan segment without a valid assignment goes
with the one before it, the first with the first of SA-NUMS."
  (let ((by-num (mapcar (lambda (c) (cons (bcat-analysis--num c) c))
                        (alist-get 'segments parsed)))
        (prev (car sa-nums)))
    (mapcar (lambda (ts)
              (let* ((c (cdr (assoc (bcat-analysis--num ts) by-num)))
                     (sa (and c (bcat-analysis--num c 'sanskrit))))
                (when (memq sa sa-nums) (setq prev sa))
                (cons (bcat-analysis--num ts) prev)))
            tibetan)))

(defun bcat-analysis--differences (parsed n)
  "The differences PARSED gives for the Sanskrit segment N, joined; \"\"
for none."
  (string-join
   (delq nil (mapcar (lambda (d)
                       (let ((text (bcat-analysis--field d 'text)))
                         (and (eql n (bcat-analysis--num d 'sanskrit))
                              (not (string-empty-p text))
                              text)))
                     (alist-get 'differences parsed)))
   " "))

(defun bcat-analysis-pair-lists (sanskrit tibetan assignment differences)
  "The two parts of a bilingual pair (Carsten 1.11.: „Erst Sanskrit,
dann Tibetisch … so handeln, wie bei den einsprachigen, nur dass zu den
Tibetisch Übersetzungen noch die Differenz zum Sanskrit ergänzt wird“;
„Sprungmarken … sollten wir … einfügen“): SANSKRIT and TIBETAN — the
segments, alists with num, gloss-table, segment-translation … —, the
ASSIGNMENT ((TIB . SKT) …) and DIFFERENCES ((SKT . TEXT) …).  Returns
\(SANSKRIT-ITEMS TIBETAN-ITEMS UCHEN-LINES): each Sanskrit segment with
its `mark' — a link to its first Tibetan segment, „ · Tib. 46–63“ —,
each Tibetan one with its mark back („ · Skt. 19“) and, the last of
its group, the group's `differences'; the Uchen of the Tibetan one line
per Sanskrit segment (AS 2: 7 lines, not 41), or — before the
assignment — one per segment, its Wylie under it when it has no table."
  (let* ((tibs-of (lambda (skt) (cl-loop for a in assignment when (eql (cdr a) skt) collect (car a))))
         ;; The mark stands whenever the segment shows something — so
         ;; differences after a segment without a table still have it.
         (shown (lambda (item)
                  (and (cl-some (lambda (k) (let ((v (alist-get k item)))
                                              (and (stringp v) (not (string-blank-p v)))))
                                '(gloss-table segment-translation differences))
                       t)))
         (uchen-of (lambda (n) (string-trim (or (alist-get 'uchen (cl-find n tibetan :key #'bcat-analysis--num)) ""))))
         (s-items (mapcar (lambda (s)
                            (let ((tibs (funcall tibs-of (bcat-analysis--num s))))
                              (append `((mark . ,(if tibs
                                                     (format " · [[tib-%d][Tib. %s]]" (car tibs)
                                                             (bcat-analysis--num-ranges tibs))
                                                   ""))
                                        (shown . ,(funcall shown s)))
                                      s)))
                          sanskrit))
         (t-items (mapcar (lambda (ts)
                            (let* ((cell (assq (bcat-analysis--num ts) assignment))
                                   (skt (cdr cell))
                                   ;; The last Tibetan segment that renders
                                   ;; SKT — the Tibetan may reorder (MSA
                                   ;; IX.60: 459 → 287, 460–461 → 286,
                                   ;; 462 → 287).
                                   (last (and skt (not (cl-find skt (cdr (memq cell assignment))
                                                                :key #'cdr)))))
                              (let ((d (or (and last (cdr (assq skt differences))) "")))
                                (append `((mark . ,(if skt (format " · [[skt-%d][Skt. %d]]" skt skt) ""))
                                          (differences . ,d)
                                          (shown . ,(funcall shown (cons (cons 'differences d) ts))))
                                        ts))))
                          tibetan))
         (lines (if assignment
                    (delq nil (mapcar (lambda (s)
                                        (let ((tibs (funcall tibs-of (bcat-analysis--num s))))
                                          (and tibs `((line . ,(mapconcat uchen-of tibs " ")) (wylie . "")))))
                                      sanskrit))
                  (mapcar (lambda (ts)
                            `((line . ,(funcall uchen-of (bcat-analysis--num ts)))
                              (wylie . ,(if (string-blank-p (or (alist-get 'gloss-table ts) ""))
                                            (or (alist-get 'wylie ts) "")
                                          ""))))
                          tibetan))))
    (list s-items t-items lines)))

(defun bcat-analysis--tibetan-answered (tibetan parsed)
  "The TIBETAN segments (num uchen wylie …) with the tables (Uchen above
the Wylie) and translations of PARSED, the answer of the Tibetan
request."
  (let ((by-num (mapcar (lambda (c) (cons (bcat-analysis--num c) c))
                        (alist-get 'segments parsed))))
    (mapcar (lambda (ts)
              (let ((c (cdr (assoc (bcat-analysis--num ts) by-num))))
                (if (null c)
                    ts
                  (append `((gloss-table . ,(bcat-analysis-gloss-table (alist-get 'words c)
                                                                      (alist-get 'uchen ts)))
                            (segment-translation . ,(bcat-analysis--field c 'translation)))
                          ts))))
            tibetan)))

(defun bcat-analysis-bilingual-context (base parsed)
  "Template fields of a bilingual sentence: BASE (the fields after the
Sanskrit answer, with `tibetan', the Tibetan segments: num uchen wylie)
completed from PARSED, the answer of the Tibetan request — the two
parts (`bcat-analysis-pair-lists'): the Sanskrit segments with their
marks, `tibetan-segments' (tables, translations, marks, differences),
`tibetan-lines' (the Uchen) and `tibetan-translation'; per Sanskrit
segment `parallel' for its heading (\" · Tib. 40–44\") and its
`differences'; the `rendering'.  AS sentence 2 has 7 Sanskrit and 41
Tibetan segments (Carsten 30.10.: Claude assigns them); for segments
without a valid assignment see `bcat-analysis--assignment'."
  (let* ((assign (bcat-analysis--assignment (alist-get 'tibetan base) parsed
                                            (mapcar #'bcat-analysis--num (alist-get 'segments base))))
         (tibetan (bcat-analysis--tibetan-answered (alist-get 'tibetan base) parsed))
         (diffs (mapcar (lambda (seg)
                          (let ((n (bcat-analysis--num seg)))
                            (cons n (bcat-analysis--differences parsed n))))
                        (alist-get 'segments base)))
         (segments
          (mapcar
           (lambda (seg)
             (let* ((n (bcat-analysis--num seg))
                    (mine (cl-loop for a in assign when (eql (cdr a) n) collect (car a))))
               (append
                `((parallel . ,(if mine (concat " · Tib. " (bcat-analysis--num-ranges mine)) ""))
                  (differences . ,(cdr (assq n diffs))))
                seg)))
           (alist-get 'segments base)))
         (lists (bcat-analysis-pair-lists segments tibetan assign diffs)))
    (append
     `((segments . ,(nth 0 lists))
       (tibetan-segments . ,(nth 1 lists))
       (tibetan-lines . ,(nth 2 lists))
       (tibetan-translation . ,(bcat-analysis--field parsed 'translation))
       (tibetan . ,tibetan)
       (rendering . ,(bcat-analysis--field parsed 'rendering))
       (assignment . ,assign))
     base)))

(defun bcat-analysis-partner-context (base parsed sanskrit &optional sanskrit-translation)
  "Template fields of the Tibetan file of a bilingual pair (Carsten
30.10.: „ein gegenseitiges Verlinken“, „das volle Programm“; 1.11.:
mirrored, Tibetan first): BASE (the Tibetan analysis,
`bcat-analysis-context') completed from PARSED, the answer of the
Tibetan request, and SANSKRIT, the Sanskrit segments after the Sanskrit
answer (num devanagari samhita gloss-table segment-translation …): the
two parts (`bcat-analysis-pair-lists') — the Tibetan segments with
their marks and, the last of a group, its differences;
`sanskrit-segments' with theirs; SANSKRIT-TRANSLATION, the sentence's —
per Tibetan segment `parallel' (\" · Skt. 17\") for its heading; the
`rendering'."
  (let* ((assign (bcat-analysis--assignment (alist-get 'segments base) parsed
                                            (mapcar #'bcat-analysis--num sanskrit)))
         (diffs (mapcar (lambda (s)
                          (let ((n (bcat-analysis--num s)))
                            (cons n (bcat-analysis--differences parsed n))))
                        sanskrit))
         (segments
          (mapcar (lambda (seg)
                    (let ((sa (cdr (assq (bcat-analysis--num seg) assign))))
                      (cons (cons 'parallel (if sa (format " · Skt. %d" sa) "")) seg)))
                  (alist-get 'segments base)))
         (lists (bcat-analysis-pair-lists sanskrit segments assign diffs)))
    (append
     `((segments . ,(nth 1 lists))
       (sanskrit-segments . ,(nth 0 lists))
       (sanskrit-translation . ,(or sanskrit-translation ""))
       (sanskrit . ,sanskrit)
       (sanskrit-p . ,(and sanskrit t))
       (rendering . ,(bcat-analysis--field parsed 'rendering))
       (assignment . ,assign))
     base)))

(provide 'bcat-analysis)
;;; bcat-analysis.el ends here
