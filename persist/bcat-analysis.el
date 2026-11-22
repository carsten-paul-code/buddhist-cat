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
(require 'ucs-normalize)
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

(defconst bcat-analysis--label-limit 25
  "Longest last cell of a word the prompts ask for — a Tibetan word's
function, a Sanskrit word's form (5.11., one rule for both); the table
cuts a longer one at `bcat-analysis-function-width'.")

(defun bcat-analysis--words-rule (lang)
  "The rule for the words of a Tibetan segment (glosses in LANG) — one
wording for the Tibetan analysis and the Tibetan request of a bilingual
sentence."
  (format "- words: the segment's words in order, together covering every syllable of its Wylie exactly once. A lexical compound or fixed verb phrase is one word (\"brjod du med pa\"); case and converb particles are words of their own. wylie: exactly as in the given Wylie. gloss: the meaning in THIS context, at most 25 characters, in %s. function: part of speech and syntactic function, compact, at most %d characters — \"N · agent\", \"ADJ · predicate\", \"V pf. ('pong ba) · main verb\", \"DEM · object\", \"NEG + V pf. (byed pa)\"; a case or converb particle gets only its label: ERG, GEN, ABS, TERM, LOC, ABL, COM, DAT, or \"CONV · cond.\" etc.; a verb's dictionary form in parentheses only when the word is another stem than it shows (spangs: \"V pf. ('pong ba)\"), not when it repeats the stem (brgyud pa'i: \"V pf.\"); any further explanation (what it modifies, the kind of clause) belongs in structure."
          lang bcat-analysis--label-limit))

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

(defconst bcat-analysis-machine-references '("DharmaMitra")
  "Reference translations that are a machine's.  Carsten 18.11.: „Dharma
Mitra ist inzwischen für mich keine Referenz mehr und sollte nur der
Vollständigkeit halber mit aufgeführt sein“ — such a translation stands
last under Translations compared, under a heading that says what it is,
and its renderings alone make no point for the discussion.")

(defconst bcat-analysis--comparison-schema
  "\"units\": [{\"passage\": \"…\", \"segment\": 0, \"readings\": [{\"sigla\": \"…\", \"text\": \"…\"}], \"finding\": \"…\", \"grammatical\": \"all|one\", \"grammar_note\": \"…\", \"certainty\": \"certain|likely|open\", \"impact\": \"medium|high\", \"impact_note\": \"…\", \"doubts\": [{\"who\": \"…\", \"verdict\": \"doubtful|wrong\", \"reason\": \"…\", \"better\": \"…\"}]}], \"minor\": \"…\", \"proposal\": [{\"label\": \"…\", \"text\": \"…\"}], \"decisions\": \"…\", \"corrections\": [\"…\"]"
  "The comparison's part of the JSON contract of every sentence analysis
\(`bcat-analysis--sentence-rules').  18.11.: the points for the
discussion only — no rating, core or weight any more (2.11.–18.11.).")

(defun bcat-analysis--sentence-rules (lang)
  "The rules for the sentence translation and the comparison of the
translations (renderings in LANG) — one wording for every analysis.
Carsten 2.11. (Muster vergleich-ta1-1.org): unit by unit, argued from the
original — „es gibt da keine Autorität, auf die man sich verlassen
kann“ —, a proposal, the decisions, the errors of the own analysis.
18.11.: „auf dem Punkt … wirklich bloß enthalten was diskussionswürdig
ist“, „das grün/gelb/rot hilft mir dabei nicht wirklich weiter“, „Dharma
Mitra ist inzwischen für mich keine Referenz mehr“: only the points for
the discussion, each with its segment (it is marked there), no rating;
a machine translation no reason for a point; the rest in one sentence
\(`minor').  The translation in the second language and the readings of
the comparison in it are an option of the sentence
\(`bcat-analysis--second-request')."
  (format "- translation (sentence): your translation of the whole sentence (the segment parts joined).
- units: only the points for the discussion in class — the passages where the given translations differ in meaning, not in wording, and the difference matters for the sentence. Compare yours (siglum C) and every reference translation by the sigla the request gives. A machine translation (DharmaMitra, siglum DM) is no reference: give its reading beside the others where a point is made, but never make a point because of it and never question it. One entry per point in the order of the text — usually none to four for a sentence; [] when nothing is worth discussing or no reference translation other than a machine's is given. A last entry for the sentence as a whole only where it matters (a double sense, śleṣa, that a translation gives only in part).
  - passage: the point, words of the original in /italic/: \"/vimalakalāśrayā/\", \"/jananī/, /janakaḥ/\".
  - segment: the number of the segment the passage stands in (the first, when it runs over several); it is marked there.
  - readings: each translation's wording of the passage, verbatim in „…“ (shortened with … where long); translations reading alike in one entry (\"sigla\": \"D, C\"); a second sense after a semicolon.
  - finding: why the readings differ and what the original supports, in two or three sentences — argued from the original alone: grammar, sandhi, lexicon, the context in the work and its commentary; never from a translator's standing (there is no authority to rely on). An argument from the given lectures is cited with siglum, lecture and time mark: \"(S, I 0:15:24)\". Words of the original in /italic/.
  - grammatical: \"all\" when every reading is grammatically possible (the choice then rests on context, commentary, doctrine or style), \"one\" when the grammar or the lexicon excludes the others; grammar_note: one sentence why, words of the original in /italic/. Carsten: \"the question is simply whether both readings are grammatically possible\".
  - certainty: how certain the decision of the proposal for this point is — \"certain\" (the grammar decides), \"likely\" (context or commentary decides), \"open\" (both remain defensible).
  - impact: \"medium\" or \"high\" (another claim) — a difference of low impact (the same sense or doctrine, only the construction differs) is no point; impact_note: one sentence on what depends on it for the meaning of the sentence.
  - doubts: the conclusion for the class, said aloud as „D renders it so, I am not sure that is right, because …“ — draw the conclusion wherever the text allows one. One entry per rendering the finding questions (translations reading alike in one entry; yours too, when it is questioned; never the machine translation's); [] when the point stays open and both readings are defensible. who: the sigla with their wording, „D „the hidden (protected) light“, S „the guarded light““. verdict: \"wrong\": the grammar or the lexicon excludes it; \"doubtful\": grammatically possible, but the context or the commentary speaks against it. reason: why, in one or two sentences, argued from the original as in the finding, a lecture with its time mark — a subordinate clause without the conjunction (\"because\" is set before it; in German verb last: \"gupta (√gup) zuerst ‚bewahrt‘ heißt\"). better: the rendering to prefer in „…“ with the sigla of the translations that have it, \"\" when none is better.
- minor: one sentence on what differs in wording only or matters little (a misprint, a nuance) — worth a glance, not a discussion; nothing of the machine translation's; \"\" when there is nothing.
- proposal: your translation of the sentence weighed by the comparison — every point decided (the other reading belongs in a note); where the text has a double sense one entry per sense, each with its label (\"First sense (theological)\"), else one entry with the label \"\"; [] when no reference translation other than a machine's is given. Each text a finished translation to be read aloud in class: fluent and idiomatic, every statement once, the word order free; square brackets only for words the original leaves implicit — never to show a construction or to repeat a clause (that belongs in decisions).
- decisions: the decisions of the proposal, by point number, in one to three sentences; \"\" when units is [].
- corrections: errors of your own analysis above that the comparison brings to light (a form misidentified, a gloss that does not fit), one sentence each; [] when there are none.
- Language of the comparison: finding, impact_note, minor, reason, decisions and corrections in %s, the labels of the proposal and grammar_note too; readings verbatim; the renderings in better and proposal in %s."
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
   (mapconcat (lambda (s) (format ", %s = %s%s" (cdr s) (car s)
                                  ;; 18.11.: no reference, for completeness only
                                  (if (member (car s) bcat-analysis-machine-references)
                                      " (a machine translation: no reference, for completeness only)"
                                    "")))
              (bcat-analysis-sigla (mapcar #'car refs)) "")
   " — only in the units (their readings); everywhere else, references and shift included, the full names."
   (if lectures
       (concat "\n\nLectures on this sentence — only for the comparison (the findings), cite with the time mark:\n"
               (mapconcat (lambda (l) (format "=== %s ===\n%s" (car l)
                                              (string-trim (bcat-analysis--lecture-text (cdr l)))))
                          lectures "\n"))
     "")))

(defconst bcat-analysis--overview-schema
  "\"overview\": {\"context\": \"…\", \"subject\": \"…\", \"steps\": [\"…\"], \"terms\": [\"…\"], \"move\": \"…\", \"reading\": [{\"segment\": 0, \"text\": \"…\", \"unit\": 0}], \"questions\": [\"…\"]}"
  "The part of the JSON contract for „Worum es geht“
\(`bcat-analysis--overview-rule') — the last field of an answer: it
draws on everything before it.")

(defun bcat-analysis--overview-rule ()
  "The rule for `overview', the section „Worum es geht“ of a sentence
file — one wording for every analysis (Tibetan, Sanskrit, commentary).
Carsten 16.11. (über die Tantrāloka-Session, Muster TĀ 1009–1040 von
Hand): he follows the translations but cannot join the discussion in
class; the concepts give lexicon knowledge, „For the class“ only
compares translations.  What is missing: what the sentence explains,
what the commentator does, one or two questions to join in.
19.11. (Carsten: „Worum es geht hätte ich gerne ausführlicher. Das
würde ich dann in der Vorbereitung als erstes lesen und dann an den
Text gehen“; prototype on TĀ 1012: „passt, super so“): half a page
instead of 8 to 14 lines — where the sentence stands (context), each
step of the train of thought in two or three sentences with the words
of the original, the terms one must know (terms), the places where a
reader of the original stumbles (reading; `unit' ties a place to a
point of the comparison, `bcat-analysis-overview' names the point).
The first fetches (TĀ 1001, bden gnyis 28, 19.11.) came out at 66 and
53 lines of 120 characters, not the 25 to 35 the rule names — Carsten:
„gefällt mir so“; the wording stays as it is, do not tighten it.
German whatever the language of the rest; only from the own analysis,
the context lines and the neighbouring sentences
\(`bcat-sentence--neighbours-block'); nothing invented, no
doxographical label, the open said to be open.  The labels are the
tool's (`bcat-analysis-overview')."
  "- overview: the orientation a participant reads first when preparing the sentence, before going to the text — someone who follows the translations but finds it hard to join the discussion in class. Always in German, whatever the language of the rest; plain words, short sentences, about 25 to 35 lines in all (half a page); words of the original in /italic/, renderings in „…“. Draw only on your own analysis above (the words and their parts, the grammar, the concepts and, where there is one, the comparison of the translations with its decisions), on the context lines of the document and on the neighbouring sentences where the request gives them (a second round on the same word, a reference back or ahead: „zum zweiten Mal nach 1011“). Invent nothing: no source of a quotation, no parallel, no doctrinal position; assign the text to no school and no doxographical category; where the analysis leaves something open, say that it is open.
  - context: two or three sentences on where we are — the work and the passage as the context lines of the document name them, what the sentence before did, what this one takes up, what the next one brings; only from those lines and the neighbouring sentences, \"\" when the request gives neither.
  - subject: one sentence on what is explained here — in a commentary the word or member of the root text it glosses („das Kompositum /anuttarāmṛtakulam/ aus Vers 1.1“), for a quotation that it is no gloss but a proof, and of what („kein Versglied, sondern ein Beleg zu 1012“); in a root text its topic.
  - steps: the train of thought in the order of the text, three to five steps — one entry per step when the text proceeds member by member, each beginning with the word it explains and its rendering („/anuttara/ „das Unübertreffliche“: …“); one entry when it reads as a paragraph. Each step two or three sentences: what the text says there and why, with the words of the original that carry it in /italic/ beside their rendering. No numbers of your own.
  - terms: three to five terms a participant must know to follow this sentence — each one line, „/term/: …“, what it means here, taken from your concepts; where your analysis leaves a point open, say so. [] when the sentence needs none.
  - move: one or two sentences on what the author does as a method — an etymology, a wordplay, a quotation as proof, a compound resolved into relative clauses, a second round of glossing, a reference back; \"\" when nothing stands out.
  - reading: two or three places where a reader of the original stumbles — a long compound and the order in which to resolve it, where a quotation begins and ends, what a pronoun or a relative clause refers to, a sandhi that hides a word. Each with segment, the number of its segment; text, one or two sentences on how to read the place; unit, the number of the entry in units (counted from 1) when the place is one of the points there, else 0. [] when nothing stands in the way.
  - questions: two questions for the discussion, a third only for a real question of text or reading — each about a concrete place in the text, followed by a half sentence on where the answer lies or why it is open (the translations diverge: name them by their sigla; the analysis is uncertain).")

(defun bcat-analysis-system-prompt (target)
  "The system prompt of the sentence analysis for the target language
code TARGET — constant per document, so the prompt cache stays warm."
  (let ((lang (bcat-analysis--language target)))
    (format "You are a philologist preparing one sentence or verse of a Classical Tibetan Buddhist text for a university reading class. Work segment by segment, then on the whole sentence. Answer with ONE JSON object and nothing else (no prose, no code fence):

{\"segments\": [{\"num\": <segment number>, \"words\": [{\"wylie\": \"…\", \"gloss\": \"…\", \"function\": \"…\"}], \"translation\": \"…\", \"references\": [{\"name\": \"…\", \"text\": \"…\"}], \"shift\": \"…\", \"structure\": \"…\", \"note\": \"…\", \"concepts\": \"…\"}], \"grammar\": \"…\", \"translation\": \"…\", %s, %s}

Rules:
- segments: one entry per given segment, same numbers, same order.
%s
%s
- Write glosses, notes and explanations in %s; the overview in German. Never mention yourself."
            bcat-analysis--comparison-schema bcat-analysis--overview-schema
            (bcat-analysis--tibetan-rules lang) (bcat-analysis--overview-rule) lang)))

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

(defun bcat-analysis--checklist-block (checklist)
  "CHECKLIST — the text of a checklist as its file has it (org: a
heading per group, numbered rules) — as the block of a system prompt,
or \"\" when there is none: a sentence that says what it is, the
headings as lines with a colon, the rules word for word (20.11.)."
  (if (or (null checklist) (string-blank-p checklist))
      ""
    (concat "Checklist — each rule stands for a mistake made in earlier analyses of this kind and corrected by hand. Check your answer against every one of them before you give it:\n"
            (string-trim (replace-regexp-in-string "^\\*+[ \t]+\\(.*?\\)[ \t]*$" "\\1:" checklist))
            "\n")))

(defun bcat-analysis-sanskrit-system-prompt (target &optional bhasya recitation checklist)
  "The system prompt of the Sanskrit sentence analysis (§5.74, Carsten
29.10.: his Sanskrit is rudimentary, the class has Sanskrit
professionals and goes deep into the grammar — roots, gaṇa, stems;
terminology of the Devavāṇīpraveśikā) for the target language code
TARGET — constant per document, so the prompt cache stays warm.
BHASYA: the document is read verse by verse with the bhāṣya below the
verse (MSA, 30.10.) — the answer has `bhasya_hint' too.  RECITATION:
the file has a Recitation (the Sanskrit template) — for prose the
answer gives `recitation', the text in sense units (31.10., TĀ 1008).
CHECKLIST (20.11., Carsten über die Tantrāloka-Sitzung): the text of
the checklist for the Sanskrit analysis
\(Templates/analysis/checklist-sanskrit.org, `bcat-sentence--sanskrit-checklist')
— the mistakes the fetch made more than once and that were corrected by
hand; after the rules, before the closing lines
\(`bcat-analysis--checklist-block')."
  (let ((lang (bcat-analysis--language target)))
    (format "You are a Sanskritist preparing one sentence or verse of a Sanskrit text for a university reading class of experienced Sanskritists; one participant reads Sanskrit only haltingly, so every form is spelled out. Work segment by segment, then on the whole sentence. Use the grammatical terminology of Goldman & Sutherland Goldman, Devavāṇīpraveśikā: Sanskrit terms, with the English equivalent where it helps. Answer with ONE JSON object and nothing else (no prose, no code fence):

{\"segments\": [{\"num\": <segment number>, \"words\": [{\"word\": \"…\", \"lemma\": \"…\", \"formation\": \"…\", \"parts\": \"…\", \"members\": \"…\", \"layers\": [{\"part\": \"…\", \"role\": \"…\", \"phrase\": \"…\"}], \"gloss\": \"…\", \"form\": \"…\"}], \"translation\": \"…\", \"references\": [{\"name\": \"…\", \"text\": \"…\"}], \"shift\": [\"…\"], \"sandhi\": \"…\", \"forms\": [{\"word\": \"…\", \"analysis\": \"…\"}], \"compounds\": [{\"word\": \"…\", \"analysis\": \"…\"}], \"structure\": \"…\", \"note\": \"…\", \"concepts\": \"…\"}], \"grammar\": \"…\", \"translation\": \"…\", %s%s, %s}

Rules:
- segments: one entry per given segment, same numbers, same order.
- words: the segment's padapāṭha — the transmitted segment complete and in the same order, every word with all sandhi undone, in pausa form; the members of a compound joined by hyphens (\"mad-vāk\", \"śrīmat-mahā-ānanda-vijṛmbhitam\"). Undo only the sandhi: never change, add or leave out a word, and keep every ending as the text has it (abhinavāyām stays abhinavāyām, vyatiriktatvena stays vyatiriktatvena). Do not join independent words into a compound (yat vakṣyati, not yat-vakṣyati) and do not split a word (yasyāḥ, not yasya āḥ). Resolve an avagraha or a vowel sandhi into the right words, pronouns too (yāsāv = yā asau, not yā sā). Check the resolution: a privative a- or an initial vowel can vanish in sandhi (madvāgaśaktāpi = mad-vāk aśaktā api).
- lemma: the dictionary form — a nominal's stem (\"aśakta\", \"niyantraṇa\", \"vāc\"), a verb's prefixes and √root (\"ud-√las\"), a compound's members as stems joined by hyphens (\"mad-vāc\", \"śrīmat-mahā-ānanda-vijṛmbhita\"); an indeclinable as it stands.
- formation: how the word is built — prefixes, √root, suffix: \"a-√śak-ta\", \"ni-√yantr-ana\", \"ud-√las\"; for a compound its members separated by \" · \", each with its formation (\"śrī-mat · mahā · ā-√nand-a · vi-√jṛmbh-ita\"); \"—\" for indeclinables and pronouns.
- parts: the meaning of each part of the formation in one or two words, in the same order, separated by \" · \", a root with its gaṇa number: \"a- not · √śak (5) be able · -ta ppp\"; \"\" when formation is \"—\".
- members: only for a compound — the meaning of each member as your translation uses it, in the order of the members, separated by \" · \", exactly one entry per member of word: for \"para-vimarśa-eka-svabhāva-kartṛtā-lakṣaṇā\" \"supreme · reflective awareness · sole · nature · agency · mark\". Leave the field out for a word of one member.
- layers: only for a compound of three or more members — how it is resolved, from its last member backwards, one entry per step. part: the member or members taken in this step as word writes them, the rest of the compound as \"…\" (\"…-lakṣaṇā\", then \"…-kartṛtā-\", \"…-eka-svabhāva-\", last \"para-vimarśa-\"); role: the relation in a few words — the samāsa type and what it refers to (\"bahuvrīhi, of kalā\", \"head\", \"bahuvrīhi, of kartṛtā\", \"karmadhāraya\"); phrase: the words this step has in your translation (\"whose mark is\", \"agency,\", \"the sole nature of which is\", \"supreme reflective awareness\") — the phrases read in order are the translation of the compound. Leave the field out otherwise.
- gloss: the meaning in THIS context, at most 30 characters, no comments or cross-references (those belong in structure or note).
- form: a compact label, at most %d characters — \"f. nom. sg.\", \"ppp. f. nom. sg.\", \"opt. 3 sg. P\", \"pron. n. nom. sg.\", \"adj. n. nom. sg.\", \"ind.\"; no remarks in parentheses — the compound type, what the word agrees with and any further explanation belong in compounds or structure.
- translation (segment): this segment's part of your translation; the parts read in order form the sentence translation. Give key terms with the Sanskrit in parentheses, e.g. \"incapable (aśakta)\".
- references (segment): for every reference translation given, the part that corresponds to this segment, quoted verbatim (keep its brackets; not its notes); name: the full name as given under Reference translations, never a siglum. Mark with org bold *…* ONLY passages that shift the meaning against the Sanskrit or against your reading; stylistic variation is not marked.
- shift: one line for each translator whose part has a marked passage, beginning with the translator's name: \"Dyczkowski 2023: niyantraṇa as a restriction on the reader's understanding\"; [] when nothing is marked — never a line for a translator without marks.
- sandhi: every junction of the segment resolved as it stands in the transmitted text (yanniyantraṇam = yat niyantraṇam: t before a nasal → n; no junction or visarga that the text does not have), separated by \" · \": \"madvāg aśaktā: final -k voiced before a vowel → -g · aśaktā + api → aśaktāpi (ā + a → ā)\"; internal sandhi of a prefix where it changes the form (\"ud + laset → ullaset (d + l → ll)\").
- forms: one entry per declined or conjugated word (none for indeclinables), word as in words. Nominals: stem with gender and stem class, vibhakti and vacana with the English (\"prathamā vibhakti ekavacana (nom. sg.)\"), function. Verbs: √root with gaṇa number, gaṇa name, pada and the 3rd sg. present (\"√las (1, bhvādi, P: lasati)\"), lakāra with the English, pada, puruṣa and vacana with the English (\"prathamapuruṣa ekavacana (3rd sg.)\" — prathama puruṣa is the 3rd person), the present stem. Participles and other kṛdanta: their name and suffix (\"bhūte kṛdanta (ppp., kta)\", \"tvānta/lyabanta (gerund)\", \"tumanta (infinitive)\"). Taddhita derivatives: base and suffix, with vṛddhi or guṇa where it occurs.
- imperative in -tāt: forms like sphuratāt, vikasatāt, saṃsphuratāt, jīvatāt, bhavatāt are imperative (loṭ) 2nd/3rd sg. parasmaipada in -tāt, which replaces -tu/-hi in a benediction (āśiṣi, Pāṇini 7.1.35 tuhyos tātaṅ āśiṣy anyatarasyām); function a wish, „may it …“. In forms, formation and parts decompose it as present stem + -tāt: sphura- + -tāt (\"√sphur-a-tāt\"), vi-kasa- + -tāt (\"vi-√kas-a-tāt\"); form \"ipv. 3 sg. P\" (or 2 sg.). It is never a participle, an ablative ending, a \"Vedic optative\" or the āśīrliṅ (precative, -yāt).
- compounds: one entry per compound — the samāsa type and the vigraha with its meaning: \"ṣaṣṭhī tatpuruṣa: mama vāk „my speech“\"; for longer compounds the inner structure; [] when there is none.
- structure: the segment's syntax in one line, org markup — Sanskrit words /italic/, functions in brackets: \"/yat niyantraṇam/ [subject] /ullaset/ [V opt.] → relative clause, taken up by /tat/ in 686\".
- note: only for a grammatical peculiarity or a real ambiguity, one or two sentences; otherwise \"\".
- concepts: technical concepts (Buddhist, Śaiva, philosophical) occurring in THIS segment — the term in *bold*, a short encyclopedic note of two or three sentences; \"\" when there is none.
- grammar: an overview of the whole sentence — the clauses and how they are linked, the verbs, subjects, elisions (an understood copula). Name a metre only as given under Metre in the request; none when none is given.
%s
%s- JSON escaping: only \\\" and \\\\ — never a backslash before an apostrophe.
- Write glosses, notes and explanations in %s; the overview in German. Never mention yourself."
            bcat-analysis--comparison-schema
            (concat (if bhasya ", \"bhasya_translation\": \"…\", \"bhasya_hint\": \"…\"" "")
                    (if recitation ", \"recitation\": [\"…\"]" ""))
            bcat-analysis--overview-schema
            bcat-analysis--label-limit
            (bcat-analysis--sentence-rules lang)
            (concat
             (if bhasya
                 (concat "- bhasya_translation: the bhāṣya on this verse translated whole — close to the Sanskrit as given, the key terms with the original in parentheses; the Tibetan only where it reads otherwise.\n"
                 "- bhasya_hint: the bhāṣya on this verse is given as context; one to three sentences on what in it matters for reading the verse — how it takes a word or the verse's function, a reading it presupposes, where its Tibetan says something else; Sanskrit and Tibetan words in /italic/; \"\" when nothing.\n")
               "")
             (if recitation
                 "- recitation: for prose only (for a verse []): the transmitted text of the whole sentence exactly as given — same spelling and sandhi, nothing added or left out —, divided for reading aloud into lines, one array entry per line, each a sense unit of a few words ending at a clause or phrase boundary (break only where the text has a space); inside a line mark a short breath pause with ¦.\n"
               "")
             ;; 16.11.: „Worum es geht“, after all it draws on
             (bcat-analysis--overview-rule) "\n"
             ;; 20.11.: the checklist of mistakes made before
             (bcat-analysis--checklist-block checklist))
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
before the apostrophe of the genitive particle in a structure line); a
closer that does not match the open bracket but the one below it gets
the missing closer set before it (22.11., MSA XI.17: an entry of a list
of compounds without its brace — {\"word\": …, \"analysis\": \"…\" ],
\"structure\": … — cost the whole answer of the verse).
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
              ((or ?} ?\])
               ;; 22.11., MSA XI.17: a closer for the bracket BELOW the
               ;; open one — the open one's own closer is missing
               ;; ({"word": …, "analysis": "…" ], …); set it first.
               (let ((want (if (eq c ?}) ?{ ?\[)))
                 (when (and (cdr stack) (not (eq (car stack) want)) (eq (cadr stack) want))
                   (insert (if (eq (car stack) ?{) "}" "]"))
                   (pop stack)))
               (pop stack) (setq expect-key nil))
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
            (let* (;; After the closer at point (of the bracket BELOW the
                   ;; open one — the open one's own is missing): does what
                   ;; follows go on in OUTER, the bracket around both?  A
                   ;; bracket in running text does not („[…, \"come to
                   ;; realize\"] → main clause“, bDen 16).
                   (beyond (lambda (outer)
                             (forward-char 1)
                             (skip-chars-forward " \t\n\r")
                             (cond ((eobp) (null outer))
                                   ((memq (char-after) '(?} ?\]))
                                    (eq (char-after) (if (eq outer ?{) ?} ?\])))
                                   ((eq (char-after) ?,)
                                    (forward-char 1)
                                    (looking-at (if (eq outer ?{)
                                                    "[ \t\n\r]*\"[^\"\n]*\"[ \t\n\r]*:"
                                                  value))))))
                   (after (lambda (as-key)
                            (forward-char 1)
                            (skip-chars-forward " \t\n\r")
                            (cond ((eobp))
                                  (as-key (eq (char-after) ?:))
                                  ((eq (char-after) ?,)
                                   (forward-char 1)
                                   (looking-at (if (eq (car stack) ?{) "[ \t\n\r]*\"" value)))
                                  ((eq (car stack) ?{)
                                   (or (eq (char-after) ?})
                                       ;; the object's brace missing before
                                       ;; the `]' of the array it stands in
                                       (and (eq (cadr stack) ?\[) (eq (char-after) ?\])
                                            (funcall beyond (nth 2 stack)))))
                                  ((eq (car stack) ?\[)
                                   (or (eq (char-after) ?\])
                                       (and (eq (cadr stack) ?{) (eq (char-after) ?})
                                            (funcall beyond (nth 2 stack))))))))
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

(defun bcat-analysis--join-two (a b)
  "The segment entries A and B (same number) as one: lists one after the
other — a reference's parts under its one name —, texts joined (sandhi
with „ · “, else a space), anything else A's."
  (mapcar
   (lambda (key)
     (let ((x (alist-get key a)) (y (alist-get key b)))
       (cons key
             (cond
              ((eq key 'num) x)
              ((eq key 'references)
               (let ((out (copy-tree x)))
                 (dolist (r y out)
                   (let ((same (cl-find (alist-get 'name r) out
                                        :key (lambda (o) (alist-get 'name o)) :test #'equal)))
                     (if same
                         (setf (alist-get 'text same)
                               (string-join (delete "" (list (or (alist-get 'text same) "")
                                                             (or (alist-get 'text r) "")))
                                            " "))
                       (setq out (append out (list r))))))))
              ((and (listp x) (listp y)) (append x y))
              ((and (stringp x) (stringp y))
               (string-join (delete "" (list x y)) (if (eq key 'sandhi) " · " " ")))
              ((null x) y)
              (t x)))))
   (delete-dups (append (mapcar #'car a) (mapcar #'car b)))))

(defun bcat-analysis--join-split-segments (segments)
  "SEGMENTS with entries of the same number joined into one, in order
\(`bcat-analysis--join-two') — 16.11., TĀ 1016: Claude split segment 704
at a comma into two entries, and the lookup by number took only the
first (* Text lost the second half, the Word Analysis showed both)."
  (let ((out '()) (split '()))
    (dolist (s segments)
      (let* ((n (bcat-analysis--num s))
             (prev (and n (cl-member n out :key #'bcat-analysis--num))))
        (if (not prev)
            (push s out)
          (setcar prev (bcat-analysis--join-two (car prev) s))
          (cl-pushnew n split))))
    (when split
      (message "Claude: Segment %s in Teilen beantwortet — zusammengeführt"
               (mapconcat #'number-to-string (sort split #'<) ", ")))
    (nreverse out)))

(defun bcat-analysis--close-quotes (value)
  "VALUE, a part of a decoded answer, with every quotation that „ opens
and the straight \" closes closed with “ — in each string of VALUE,
down its lists and alists (changed in place).  19.11. (TĀ 1001, the
first fetch with the longer „Worum es geht“): the rules show „…“, the
answer writes „…\" — in JSON the straight quote is the escaped one —,
22 lines of one file.  Within a line only; a quotation closed already,
straight quotes of their own and a „ nothing closes stay."
  (cond
   ((stringp value)
    (if (string-search "„" value)
        (replace-regexp-in-string "„\\([^„“”\"\n]*\\)\"" "„\\1“" value t)
      value))
   ((consp value)
    (let ((tail value))
      (while (consp tail)
        (setcar tail (bcat-analysis--close-quotes (car tail)))
        (when (and (cdr tail) (not (consp (cdr tail))))
          (setcdr tail (bcat-analysis--close-quotes (cdr tail))))
        (setq tail (cdr tail))))
    value)
   (t value)))

(defun bcat-analysis-mixed-script-words (parsed)
  "The words in the texts of the answer PARSED that mix Latin letters
with letters of a look-alike script, Cyrillic or Greek — each once, in
the order found.  20.11. (Tantrāloka-Sitzung): TĀ 1012 came back with
„kalayет“, Cyrillic е and т in kalayet; nothing shows it on the page.
A word wholly in another script (a Russian title, Greek, Japanese,
Devanāgarī, Tibetan) is none."
  (let ((found '())
        (case-fold-search nil))
    (cl-labels ((walk (value)
                  (cond
                   ((stringp value)
                    (let ((pos 0))
                      (while (string-match "[[:alpha:]\u0300-\u036f]+" value pos)
                        (let ((word (match-string 0 value)))
                          (setq pos (match-end 0))
                          (when (and (string-match-p "[a-zA-Z\u00c0-\u024f\u1e00-\u1eff]" word)
                                     (string-match-p "[\u0370-\u03ff\u0400-\u052f]" word)
                                     (not (member word found)))
                            (push word found))))))
                   ((consp value)
                    (let ((tail value))
                      (while (consp tail)
                        (walk (car tail))
                        (when (and (cdr tail) (not (consp (cdr tail))))
                          (walk (cdr tail)))
                        (setq tail (cdr tail))))))))
      (walk parsed))
    (nreverse found)))

(defun bcat-analysis-read (response)
  "(OBJECT . PROBLEM) for Claude's RESPONSE: the JSON object as an alist
\(keys as symbols, arrays as lists) and nil, or nil and a short
description why there is none — an answer cut off at the output limit
says so (one whose brackets stay open and that does not end with the
brace of its object; a complete one with a bracket amiss does not).  Tolerates a code fence or prose around the object, and mends
what `bcat-analysis--repair-json' mends; its quotations are closed as
they were opened (`bcat-analysis--close-quotes')."
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
           ((and (consp obj) (assq 'segments obj))
            (setf (alist-get 'segments obj)
                  (bcat-analysis--join-split-segments (alist-get 'segments obj)))
            ;; 19.11.: „…\" → „…“
            (cons (bcat-analysis--close-quotes obj) nil))
           ((or obj (null (cdr strict))) (cons nil "JSON ohne segments"))
           ;; cut off: its brackets stay open AND it does not end with
           ;; the brace of its object (22.11.: a complete answer with
           ;; one bracket amiss was called cut off)
           ((and (bcat-analysis--unterminated-p (substring response start))
                 (not (string-match-p "}[ \t\n\r`]*\\'" response)))
            (cons nil "abgeschnitten — Grenze `tibetan-claude-max-tokens' erreicht"))
           ((bcat-analysis--unterminated-p (substring response start))
            (cons nil (concat (bcat-analysis--json-problem (cdr strict) start)
                              " — die Antwort ist vollständig, aber ihre Klammern gehen nicht auf")))
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
\(`bcat-analysis--continued-p') belongs to the row above.  Never below
the rows the words have — four with the Uchen in the first row, three
without: a gloss from the dictionary may end in „;“ like the line of a
broken cell („understand; opportunity;“, Kadam 2067 — its function row
„?“ was joined to it, 18.11.)."
  (let* ((out '())
         (least (if (and rows (cl-some (lambda (c) (string-match-p "[\u0F00-\u0FFF]" c)) (car rows))) 4 3))
         (left (length rows)))
    (dolist (row rows (nreverse out))
      (let ((prev (car out)))
        (if (and prev
                 (> left least)
                 (cl-some (lambda (c) (not (string-empty-p c))) row)
                 (cl-every (lambda (j)
                             (or (string-empty-p (or (nth j row) ""))
                                 (bcat-analysis--continued-p (or (nth j prev) ""))))
                           (number-sequence 0 (1- (length row)))))
            (progn
              (setq left (1- left))
              (setcar out (cl-loop for j below (max (length prev) (length row))
                                   collect (let ((a (or (nth j prev) "")) (b (or (nth j row) "")))
                                             (if (string-empty-p b) a (concat a " " b))))))
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
     (green . "grün") (yellow . "gelb") (red . "rot")
     ;; 18.11.: the comparison on the point
     (points . "Diskussionspunkte") (none . "Keine: die Übersetzungen stimmen in der Sache überein.")
     (discuss . "Zu besprechen") (here . "hier") (point . "Punkt") (depends . "Was daran hängt")
     (not-at-issue . "Unstrittig") (agree . "%d Stelle(n), an denen alle übereinstimmen")
     (machine-only . "nur %s weicht ab") (minor . "geringfügig")
     (machine . "%s (maschinelle Übersetzung, der Vollständigkeit halber)"))
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
     (green . "green") (yellow . "yellow") (red . "red")
     (points . "Discussion points") (none . "None: the translations agree in substance.")
     (discuss . "Discuss") (here . "here") (point . "point") (depends . "What depends on it")
     (not-at-issue . "Not at issue") (agree . "%d passage(s) on which all agree")
     (machine-only . "only %s differs") (minor . "minor")
     (machine . "%s (machine translation, for completeness)")))
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

;; ----------------------------------------------------------------------------
;; The comparison on the point (18.11.2026)
;; ----------------------------------------------------------------------------

(defun bcat-analysis--machine-sigla (refs)
  "The sigla of the machine translations among REFS ((NAME . TEXT) …) —
\"DM\" when REFS names none."
  (or (delq nil (mapcar (lambda (s) (and (member (car s) bcat-analysis-machine-references) (cdr s)))
                        (bcat-analysis-sigla (mapcar #'car refs))))
      '("DM")))

(defun bcat-analysis--known-sigla (refs)
  "The sigla of a comparison with REFS: C, the own translation, and
those of REFS."
  (cons "C" (mapcar #'cdr (bcat-analysis-sigla (mapcar #'car refs)))))

(defun bcat-analysis--who-sigla (who known)
  "The sigla WHO names — a doubt's `who', „D „the hidden light“, DM
„concealed““: the wordings in quotes do not count — among KNOWN; when
KNOWN has only C (no references at hand) every short word in capitals."
  (let ((bare (replace-regexp-in-string
               "„[^“”\"\n]*[“”\"]\\|\"[^\"\n]*\"\\|‚[^‘'\n]*[‘']" " " (or who "")))
        (case-fold-search nil))
    (cl-remove-if-not (lambda (tk)
                        (if (cdr known)
                            (member tk known)
                          (string-match-p "\\`[A-Z][A-Za-z0-9]\\{0,3\\}\\'" tk)))
                      (split-string bare "[^[:alnum:]]+" t))))

(defun bcat-analysis--reading-sigla (reading)
  "The sigla of READING, an entry of a unit's readings."
  (split-string (bcat-analysis--field reading 'sigla) "[^[:alnum:]]+" t))

(defun bcat-analysis--unit-reading (unit siglum)
  "The wording SIGLUM has for UNIT, or nil."
  (cl-loop for r in (alist-get 'readings unit)
           when (member siglum (bcat-analysis--reading-sigla r))
           return (let ((text (replace-regexp-in-string "[ \t]*\n[ \t]*" " " (bcat-analysis--field r 'text))))
                    (and (not (string-empty-p text)) text))))

(defun bcat-analysis--scholar-doubts (unit machine known)
  "The doubts of UNIT that question a rendering other than a machine's:
\((SIGLA . DOUBT) …), SIGLA those the doubt names without MACHINE's."
  (delq nil
        (mapcar (lambda (d)
                  (let ((sigla (cl-remove-if (lambda (s) (member s machine))
                                             (bcat-analysis--who-sigla (bcat-analysis--field d 'who) known))))
                    (and sigla (cons sigla d))))
                (alist-get 'doubts unit))))

(defun bcat-analysis--point-p (unit machine known)
  "Non-nil when UNIT of the comparison is a point for the discussion
\(Carsten 18.11.: „wirklich bloß … was diskussionswürdig ist“): not
agreed (rated green — the answers until 18.11.), its impact not low, and
the rendering of a translator other than a machine in question — or,
where no rendering has a verdict, two such renderings standing apart."
  (and (not (eq 'green (bcat-analysis--rating (bcat-analysis--field unit 'rating))))
       (not (member (downcase (string-trim (bcat-analysis--field unit 'impact))) '("low" "gering")))
       (if (cl-some (lambda (d) (not (string-empty-p (bcat-analysis--field d 'who)))) (alist-get 'doubts unit))
           (and (bcat-analysis--scholar-doubts unit machine known) t)
         (> (cl-count-if (lambda (r)
                           (let ((sigla (bcat-analysis--reading-sigla r)))
                             (and sigla (not (cl-every (lambda (s) (member s machine)) sigla)))))
                         (alist-get 'readings unit))
            1))))

(defun bcat-analysis--letters (text)
  "TEXT reduced to its letters, in small — to find a passage in a text
whatever the hyphens, spaces and marks."
  (downcase (replace-regexp-in-string "[^[:alpha:]]" "" (or text ""))))

(defun bcat-analysis--segment-keys (base parsed)
  "((NUM KEY …) …) for the segments of BASE (the rump): the texts a
passage of the comparison is looked for in — as transmitted (saṃhitā,
Wylie) and as PARSED divides it (its words joined), letters only."
  (let ((by-num (mapcar (lambda (s) (cons (bcat-analysis--num s) s)) (alist-get 'segments parsed))))
    (mapcar (lambda (seg)
              (let* ((num (bcat-analysis--num seg))
                     (c (cdr (assoc num by-num))))
                (cons num
                      (cl-remove-if
                       #'string-empty-p
                       (list (bcat-analysis--letters (or (alist-get 'samhita seg) (alist-get 'wylie seg)))
                             (bcat-analysis--letters
                              (mapconcat (lambda (w)
                                           (let ((word (bcat-analysis--field w 'word)))
                                             (if (string-empty-p word) (bcat-analysis--field w 'wylie) word)))
                                         (alist-get 'words c) "")))))))
            (alist-get 'segments base))))

(defun bcat-analysis--unit-segment (unit keys)
  "The number of the segment UNIT belongs to, or nil (a point on the
whole verse): its own `segment' (the answers since 18.11.); else the
number its passage names („(segment 694)“, „(seg. 47)“, „⟦47⟧“) when it
is one of KEYS (`bcat-analysis--segment-keys'); the only segment of a
sentence of one; the first of KEYS in whose text the passage stands —
its first words in /italic/ up to an ellipsis —, else its first word in
any final sandhi (the last letter dropped: „sopādānāḥ“ in „sopādānā“)."
  (let ((own (alist-get 'segment unit))
        (passage (bcat-analysis--field unit 'passage))
        (nums (mapcar #'car keys)))
    (cond
     ((integerp own) own)
     ((and (stringp own) (string-match-p "\\`[0-9]+\\'" (string-trim own))) (string-to-number own))
     ((string-match "\\(?:\\_<seg\\(?:ment\\)?\\.?\\|⟦\\)[ \t]*\\([0-9]+\\)" passage)
      (car (memql (string-to-number (match-string 1 passage)) nums)))
     ((and nums (null (cdr nums))) (car nums))
     (t
      (let* ((piece (if (string-match "/\\([^/\n]+\\)/" passage) (match-string 1 passage) passage))
             (key (bcat-analysis--letters (car (split-string piece "…"))))
             (word (bcat-analysis--letters (car (split-string piece "[ \t…]+" t))))
             (stem (and (> (length word) 4) (substring word 0 -1)))
             (find (lambda (k) (and k (not (string-empty-p k))
                                    (car (cl-find-if (lambda (e) (cl-some (lambda (text) (string-search k text)) (cdr e)))
                                                     keys))))))
        (or (funcall find key) (funcall find stem)))))))

(defun bcat-analysis-discussion-points (parsed &optional refs base)
  "The points for the discussion among the units of the comparison in
PARSED (`bcat-analysis--point-p'), in the order of the text: plists
\(:k NUMBER-OF-THE-POINT :n NUMBER-OF-THE-UNIT :unit UNIT :segment NUM).
REFS, the references sent with the request, give the sigla; BASE, the
rump, the segments (`bcat-analysis--unit-segment')."
  (let ((machine (bcat-analysis--machine-sigla refs))
        (known (bcat-analysis--known-sigla refs))
        (keys (bcat-analysis--segment-keys base parsed))
        (k 0))
    (cl-loop for u in (alist-get 'units parsed) for n from 1
             when (bcat-analysis--point-p u machine known)
             collect (list :k (cl-incf k) :n n :unit u :segment (bcat-analysis--unit-segment u keys)))))

(defun bcat-analysis--point-renderings (unit machine known)
  "((SIGLUM . WORDING) …) of UNIT in question: those of the translators
its doubts name; without a verdict every one but the own and a
machine's."
  (let* ((doubted (delete-dups (apply #'append (mapcar #'car (bcat-analysis--scholar-doubts unit machine known)))))
         (sigla (or (remove "C" doubted)
                    (cl-remove-if (lambda (s) (or (equal s "C") (member s machine)))
                                  (delete-dups (apply #'append (mapcar #'bcat-analysis--reading-sigla
                                                                       (alist-get 'readings unit))))))))
    (delq nil (mapcar (lambda (s) (let ((text (bcat-analysis--unit-reading unit s))) (and text (cons s text))))
                      sigla))))

(defun bcat-analysis-discuss-lines (parsed &optional refs base)
  "The lines that mark the points for the discussion at their segments
\(Carsten 18.11.: „Schon bei den Segmenten hätte ich gerne schon
*wesentliche* Unterschiede bzw. Diskussionspunkte markiert“): ((NUM .
TEXT) …) — per point „*Discuss:* PASSAGE — D: „…“; here: „…“ (→ point
K)“, the link to the point under Translations compared; several points
of a segment a paragraph each.  In the language of the comparison."
  (let* ((bcat-analysis--comparison-lang (or bcat-analysis-comparison-language (alist-get 'target-lang base) "en"))
         (label #'bcat-analysis--comparison-label)
         (machine (bcat-analysis--machine-sigla refs))
         (known (bcat-analysis--known-sigla refs))
         (out '()))
    (dolist (p (bcat-analysis-discussion-points parsed refs base))
      (let* ((u (plist-get p :unit)) (seg (plist-get p :segment)) (k (plist-get p :k))
             (own (bcat-analysis--unit-reading u "C"))
             (others (bcat-analysis--point-renderings u machine known))
             (line (concat "*" (funcall label 'discuss) ":* "
                           (replace-regexp-in-string "[ \t]*\n[ \t]*" " " (bcat-analysis--field u 'passage))
                           (if others
                               (concat " — " (mapconcat (lambda (r) (format "%s: %s" (car r) (cdr r))) others " · "))
                             "")
                           (if own (format "; %s: %s" (funcall label 'here) own) "")
                           (format " (→ [[point-%d][%s %d]])" k (funcall label 'point) k))))
        (when seg
          (let ((cell (assoc seg out)))
            (if cell (setcdr cell (concat (cdr cell) "\n\n" line)) (push (cons seg line) out))))))
    (nreverse out)))

(defun bcat-analysis--with-discuss (segments parsed refs base)
  "SEGMENTS (template fields) with `discuss' — the line(s) of
`bcat-analysis-discuss-lines' — on those that have a point."
  (let ((lines (bcat-analysis-discuss-lines parsed refs base)))
    (if (null lines)
        segments
      (mapcar (lambda (seg)
                (let ((line (cdr (assoc (bcat-analysis--num seg) lines))))
                  (if line (cons (cons 'discuss line) seg) seg)))
              segments))))

(defun bcat-analysis--point-block (point machine known)
  "The paragraphs of POINT (`bcat-analysis-discussion-points') under
Discussion points: its heading with the jump target and its segment,
the renderings but a machine's, per rendering in question the verdict —
why, and what is better —, without a verdict the finding; then what
depends on it."
  (let* ((label #'bcat-analysis--comparison-label)
         (one (lambda (s) (replace-regexp-in-string "[ \t]*\n[ \t]*" " " (string-trim (or s "")))))
         (u (plist-get point :unit)) (k (plist-get point :k)) (seg (plist-get point :segment))
         (doubts (bcat-analysis--scholar-doubts u machine known))
         (note (funcall one (bcat-analysis--field u 'impact_note))))
    (string-join
     (delq nil
           (append
            (list
             (string-join
              (cons (format "**** <<point-%d>>%d. %s%s" k k (funcall one (bcat-analysis--field u 'passage))
                            (if seg (format " (⟦%s⟧)" seg) ""))
                    (delq nil (mapcar (lambda (r)
                                        (let ((sigla (bcat-analysis--reading-sigla r))
                                              (text (funcall one (bcat-analysis--field r 'text))))
                                          (unless (or (string-empty-p text)
                                                      (and sigla (cl-every (lambda (s) (member s machine)) sigla)))
                                            (format "- %s :: %s" (bcat-analysis--field r 'sigla) text))))
                                      (alist-get 'readings u))))
              "\n"))
            (if doubts
                (mapcar (lambda (sd)
                          (let* ((d (cdr sd))
                                 (verdict (if (equal (downcase (bcat-analysis--field d 'verdict)) "wrong")
                                              'wrong 'doubtful))
                                 (reason (string-remove-suffix
                                          "." (replace-regexp-in-string
                                               "\\`\\(?:[Bb]ecause\\|[Ss]ince\\|[Aa]s\\|[Ww]eil\\|[Dd]a\\)[ \t]+" ""
                                               (funcall one (bcat-analysis--field d 'reason)))))
                                 (better (string-remove-suffix "." (funcall one (bcat-analysis--field d 'better)))))
                            (concat (format "*%s — %s:*" (string-join (car sd) ", ") (funcall label verdict))
                                    (if (string-empty-p reason) "" (concat " " reason "."))
                                    (if (string-empty-p better) ""
                                      (format " %s: %s." (funcall label 'better) better)))))
                        doubts)
              (let ((finding (funcall one (bcat-analysis--field u 'finding))))
                (unless (string-empty-p finding)
                  (list (format "*%s:* %s" (funcall label 'finding) finding)))))
            (unless (string-empty-p note)
              (list (format "*%s:* %s" (funcall label 'depends) note)))))
     "\n\n")))

(defun bcat-analysis--not-at-issue (parsed points machine known)
  "The line for what the comparison found but is no point: how many
passages all agree on, where only the machine differs, what is minor —
from the units of PARSED that are not among POINTS (the answers until
18.11. rated every passage) — and what an answer says itself (`minor');
nil when there is nothing."
  (let* ((label #'bcat-analysis--comparison-label)
         (one (lambda (u) (replace-regexp-in-string "[ \t]*\n[ \t]*" " " (bcat-analysis--field u 'passage))))
         (taken (mapcar (lambda (p) (plist-get p :unit)) points))
         (rest (cl-remove-if (lambda (u) (memq u taken)) (alist-get 'units parsed)))
         (agree (cl-remove-if-not (lambda (u) (eq 'green (bcat-analysis--rating (bcat-analysis--field u 'rating)))) rest))
         (others (cl-remove-if (lambda (u) (memq u agree)) rest))
         (machine-only (cl-remove-if-not
                        (lambda (u) (and (alist-get 'doubts u)
                                         (not (bcat-analysis--scholar-doubts u machine known))))
                        others))
         (minor (cl-remove-if (lambda (u) (memq u machine-only)) others))
         (said (string-trim (bcat-analysis--field parsed 'minor)))
         (parts (delq nil
                      (list (and agree (format (funcall label 'agree) (length agree)))
                            (and machine-only
                                 (format "%s: %s"
                                         (format (funcall label 'machine-only)
                                                 (string-join bcat-analysis-machine-references ", "))
                                         (mapconcat one machine-only ", ")))
                            (and minor (format "%s: %s" (funcall label 'minor) (mapconcat one minor ", ")))))))
    (when (or parts (not (string-empty-p said)))
      (concat "*" (funcall label 'not-at-issue) ":* "
              (if parts (concat (string-join parts " · ") ".") "")
              (if (and parts (not (string-empty-p said))) " " "")
              said))))

(defun bcat-analysis-split-proposal (proposal)
  "(PROPOSAL . DECISIONS) of PROPOSAL: the proposed translation(s), read
first, and from *Decisions:* (*Entscheidungen:*) on what justifies them
— both trimmed, \"\" when missing."
  (let* ((proposal (or proposal ""))
         (cut (and (string-match "^\\*\\(?:Decisions\\|Entscheidungen\\)\\(?: — [^*\n]*\\)?:\\*" proposal)
                   (match-beginning 0))))
    (cons (string-trim (if cut (substring proposal 0 cut) proposal))
          (if cut (string-trim (substring proposal cut)) ""))))

(defun bcat-analysis-reading-fields (proposal translation)
  "The fields that part the translation of the sentence between * Text
and * Philology (18.11., Carsten: „die proposed Translation sollte als
letzter Teil im Text Abschnitt kommen“): `reading-translation' — what
closes * Text: the proposed translation(s) of PROPOSAL with their
labels, without the decisions; without a proposal the own TRANSLATION as
„*Translation:* …“; \"\" for a sentence not analysed — and `decisions',
what justifies the proposal, for * Philology.  As the reading page of
the PDF has it since 5.11. (`bcat-analysis-split-proposal')."
  (let ((split (bcat-analysis-split-proposal proposal)))
    `((reading-translation
       . ,(cond ((not (string-empty-p (car split))) (car split))
                ((and (stringp translation) (not (string-blank-p translation)))
                 (concat "*Translation:* " (string-trim translation)))
                (t "")))
      (decisions . ,(cdr split)))))

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

(defun bcat-analysis-comparison-units (parsed &optional refs target base)
  "What follows the translations under Translations compared, from PARSED
\(Carsten 18.11.: „auf dem Punkt … wirklich bloß enthalten was
diskussionswürdig ist“; no green/yellow/red, no table): *** Discussion
points — per point its paragraphs (`bcat-analysis--point-block'), or
that there is none —, the line for the rest
\(`bcat-analysis--not-at-issue'), *** Corrections to the analysis;
\"\" when the answer has neither.  The points are named only when the
answer compared at all (it has `units') and there is a translator to
compare with — a machine translation alone is none.  REFS give the
sigla, BASE (the rump) the segments of the points.  In
`bcat-analysis-comparison-language', else in the target language
TARGET.  Until 18.11.: `bcat-analysis-comparison-units-rated'."
  (let* ((bcat-analysis--comparison-lang (or bcat-analysis-comparison-language target "en"))
         (label #'bcat-analysis--comparison-label)
         (machine (bcat-analysis--machine-sigla refs))
         (known (bcat-analysis--known-sigla refs))
         (points (bcat-analysis-discussion-points parsed refs base))
         (scholars (cl-remove-if (lambda (r) (member (car r) bcat-analysis-machine-references)) refs))
         (compared (and (assq 'units parsed) (or points scholars)))
         (corrections (cl-remove-if-not (lambda (c) (and (stringp c) (not (string-blank-p c))))
                                        (alist-get 'corrections parsed))))
    (string-join
     (delq nil
           (list
            (and compared
                 (concat "*** " (funcall label 'points) "\n"
                         (if points
                             (mapconcat (lambda (p) (bcat-analysis--point-block p machine known)) points "\n\n")
                           (funcall label 'none))))
            (and compared (bcat-analysis--not-at-issue parsed points machine known))
            (and corrections
                 (concat "*** " (funcall label 'corrections) "\n"
                         (mapconcat (lambda (c) (concat "- " (string-trim c))) corrections "\n")))))
     "\n\n")))

(defun bcat-analysis-comparison-units-rated (parsed &optional refs target)
  "The comparison as it was written from 2.11. to 18.11. — every unit
rated green, yellow or red; kept to know such a part in a file again
\(C-c u L sets it anew from the answer only when nobody changed it).
The comparison of the translations unit by unit from PARSED (Carsten
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

(defun bcat-analysis-comparison (parsed &optional refs target base)
  "Translations compared of the sentence: the own translation from
PARSED, each of REFS ((NAME . TEXT) …, the texts sent with the request)
with the marks of its segment parts, the comparison paragraph of an
older answer, then the points for the discussion in the target language
TARGET (`bcat-analysis-comparison-units'; BASE, the rump, for their
segments).  18.11.: a machine translation among REFS
\(`bcat-analysis-machine-references') stands last, under a heading of
its own that says what it is."
  (let* ((segments (alist-get 'segments parsed))
         (units (bcat-analysis-comparison-units parsed refs target base))
         (paragraph (lambda (r)
                      (let ((text (string-trim (or (cdr r) ""))))
                        (unless (string-empty-p text)
                          (format "*%s:* %s" (car r)
                                  (bcat-analysis-mark-reference
                                   text (bcat-analysis--reference-parts segments (car r))))))))
         (machine-p (lambda (r) (member (car r) bcat-analysis-machine-references)))
         (machines (let ((bcat-analysis--comparison-lang (or bcat-analysis-comparison-language target "en")))
                     (delq nil
                           (mapcar (lambda (r)
                                     (let ((p (funcall paragraph r)))
                                       (and p (concat "*** " (format (bcat-analysis--comparison-label 'machine) (car r))
                                                      "\n" p))))
                                   (cl-remove-if-not machine-p refs))))))
    (string-join
     (delq nil
           (append
            (list
             (let ((top (bcat-analysis-footnotes-last
                         (string-join
                          (delq nil
                                (append
                                 (let ((tr (bcat-analysis--field parsed 'translation)))
                                   (unless (string-empty-p tr) (list (concat "*Translation:* " tr))))
                                 (mapcar paragraph (cl-remove-if machine-p refs))
                                 (let ((c (bcat-analysis--field parsed 'comparison)))
                                   (unless (string-empty-p c) (list c)))))
                          "\n\n"))))
               (and top (not (string-empty-p top)) top))
             (and (not (string-empty-p units)) units))
            machines))
     "\n\n")))

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
     (bcat-analysis-overview-fields base parsed refs)
     (bcat-analysis--second-translation parsed)
     ;; 18.11.: the points for the discussion marked at their segments
     `((segments . ,(bcat-analysis--with-discuss segments parsed refs base))
       (glosses . ,(bcat-analysis-glosses-p segments))
       (translation . ,(bcat-analysis--field parsed 'translation))
       (grammar . ,(bcat-analysis--field parsed 'grammar))
       (comparison . ,(bcat-analysis-comparison parsed refs (alist-get 'target-lang base) base))
       (proposal . ,(bcat-analysis-comparison-proposal parsed (alist-get 'target-lang base))))
     ;; 18.11.: the translation closes * Text, the decisions stay
     (bcat-analysis-reading-fields (bcat-analysis-comparison-proposal parsed (alist-get 'target-lang base))
                                   (bcat-analysis--field parsed 'translation))
     base)))

;; ----------------------------------------------------------------------------
;; „Worum es geht“ (16.11.2026)
;; ----------------------------------------------------------------------------

(defun bcat-analysis--overview-list (value)
  "VALUE — a list of strings, or one string — as the list of its
non-blank strings, trimmed."
  (delq nil (mapcar (lambda (s) (and (stringp s) (not (string-blank-p s)) (string-trim s)))
                    (if (listp value) value (list value)))))

(defun bcat-analysis--overview-number (value)
  "VALUE — a number, or a string of digits — as a positive number, else nil."
  (let ((n (cond ((integerp value) value)
                 ((and (stringp value) (string-match-p "\\`[0-9]+\\'" (string-trim value)))
                  (string-to-number value)))))
    (and n (> n 0) n)))

(defun bcat-analysis--overview-place (entry points)
  "One place of „Beim Lesen beachten“ as a line, or nil: ENTRY of the
overview's `reading' — segment, text, unit — as „N: text“, N the number
of its segment; when its unit is one of POINTS
\(`bcat-analysis-discussion-points'), „(→ Diskussionspunkt K)“ after it,
K the number the point has under Translations compared.  An entry given
as plain text is taken as it stands."
  (cond
   ((stringp entry)
    (let ((text (replace-regexp-in-string "\\`[-–•][ \t]+" "" (string-trim entry))))
      (unless (string-empty-p text) text)))
   ((consp entry)
    (let* ((text (bcat-analysis--field entry 'text))
           (segment (bcat-analysis--overview-number (alist-get 'segment entry)))
           (unit (bcat-analysis--overview-number (alist-get 'unit entry)))
           (point (and unit (cl-find unit points :key (lambda (p) (plist-get p :n))))))
      (unless (string-empty-p text)
        (concat (if segment (format "%d: " segment) "")
                text
                (if point (format " (→ Diskussionspunkt %d)" (plist-get point :k)) "")))))))

(defun bcat-analysis--overview-items (items)
  "ITEMS as the lines of a list, a dash of their own dropped."
  (mapconcat (lambda (i) (concat "- " (replace-regexp-in-string "\\`[-–•][ \t]+" "" i))) items "\n"))

(defun bcat-analysis-overview (parsed &optional refs base)
  "The text of „Worum es geht“ from the `overview' of the answer PARSED
\(`bcat-analysis--overview-rule'), since 19.11. in seven parts (Carsten:
„ausführlicher“, read first in the preparation; prototype on TĀ 1012):
*Wo wir sind:* the context; *Erklärt wird:* the subject; *Der
Gedankengang:* the steps — several numbered, one after the label;
*Begriffe, die man kennen muss:* the terms as a list; *Der Kniff:* the
move; *Beim Lesen beachten:* the places of the original
\(`bcat-analysis--overview-place' — REFS, the references sent with the
request, and BASE, the rump, find the points of the comparison they
name); *Fragen für die Diskussion:* as a list.  The labels are German
like the text, whatever the language of the file.  A part left empty
is left out (an answer of 16.–18.11. has four of them); an overview
given as plain text is taken as it stands; nil when there is nothing."
  (let ((o (alist-get 'overview parsed)))
    (cond
     ((stringp o) (let ((text (string-trim o))) (unless (string-empty-p text) text)))
     ((consp o)
      (let* ((context (bcat-analysis--field o 'context))
             (subject (bcat-analysis--field o 'subject))
             (steps (mapcar (lambda (s) (replace-regexp-in-string "\\`[0-9]+[.)][ \t]+" "" s))
                            (bcat-analysis--overview-list (alist-get 'steps o))))
             (terms (bcat-analysis--overview-list (alist-get 'terms o)))
             (move (bcat-analysis--field o 'move))
             (reading (let ((value (alist-get 'reading o)))
                        (and value
                             (let ((points (bcat-analysis-discussion-points parsed refs base)))
                               (delq nil (mapcar (lambda (e) (bcat-analysis--overview-place e points))
                                                 (if (stringp value) (list value) value)))))))
             (questions (bcat-analysis--overview-list (alist-get 'questions o)))
             (parts (delq nil
                          (list (unless (string-empty-p context) (concat "*Wo wir sind:* " context))
                                (unless (string-empty-p subject) (concat "*Erklärt wird:* " subject))
                                (cond ((cdr steps)
                                       (concat "*Der Gedankengang:*\n"
                                               (string-join (cl-loop for s in steps for i from 1
                                                                     collect (format "%d. %s" i s))
                                                            "\n")))
                                      (steps (concat "*Der Gedankengang:* " (car steps))))
                                (and terms
                                     (concat "*Begriffe, die man kennen muss:*\n"
                                             (bcat-analysis--overview-items terms)))
                                (unless (string-empty-p move) (concat "*Der Kniff:* " move))
                                (and reading
                                     (concat "*Beim Lesen beachten:*\n"
                                             (bcat-analysis--overview-items reading)))
                                (and questions
                                     (concat "*Fragen für die Diskussion:*\n"
                                             (bcat-analysis--overview-items questions)))))))
        (and parts (string-join parts "\n\n")))))))

(defun bcat-analysis-overview-hash (text)
  "The mark of a generated „Worum es geht“ with the text TEXT: the md5 of
its trimmed text — in the file :GENERATED_HASH: under the heading
\(`bcat-sentence--overview').  A section whose text no longer has this
hash was changed by hand."
  (md5 (string-trim text) nil nil 'utf-8))

(defun bcat-analysis-overview-fields (base parsed &optional refs)
  "The fields `overview' and `overview-hash' that go before BASE (the
rump with what the file has: `overview', its text, and `overview-hash',
its mark) for the answer PARSED — or nil when BASE's stay; REFS, the
references sent with the request, for the points of the comparison
the overview names (`bcat-analysis-overview').  Carsten
16.11. (über die Tantrāloka-Session, 32 sections written by hand: „Am
Inhalt meiner Handtexte bitte nichts ändern“): a section written by
hand (no mark) or changed by hand (the mark no longer fits its text)
stays whatever the answer says — a stale mark goes; only where there is
none, or a generated one nobody touched, the answer's overview is set,
with its mark."
  (let* ((kept (let ((k (alist-get 'overview base)))
                 (and (stringp k) (not (string-blank-p k)) k)))
         (mark (alist-get 'overview-hash base))
         (by-hand (and kept (not (equal mark (bcat-analysis-overview-hash kept)))))
         (new (bcat-analysis-overview parsed refs base)))
    (cond (by-hand (and mark (list (list 'overview-hash))))
          (new `((overview . ,new) (overview-hash . ,(bcat-analysis-overview-hash new)))))))

;; ----------------------------------------------------------------------------
;; Sanskrit (§5.74, Carsten 29.10.)
;; ----------------------------------------------------------------------------

(defun bcat-analysis-sanskrit-tables (words)
  "The tables of a Sanskrit segment's WORDS (Claude's padapāṭha): word,
gloss and form — in the layout of every language (5.11., Carsten:
„generisch machen und mit anwenden“), the form cut like a Tibetan
function, its rest goes to Structure
\(`bcat-analysis--function-overflow') —, and since 18.11. under a rule
what explains each word, in its column (`bcat-analysis--sub-cells',
`bcat-analysis--band-tables'; Carsten, TĀ 1016: „dass die Word
Formation näher am Text ist, also als Subtable im Table“).  5.11. to
18.11. the parts had tables of their own under the segment's
translation.  \"\" for no words."
  (bcat-analysis--band-tables
   (mapcar (lambda (w)
             (cons (list (tibetan-gloss-table--cell (bcat-analysis--field w 'word))
                         (tibetan-gloss-table--cell (bcat-analysis--field w 'gloss))
                         (tibetan-gloss-table--cell
                          (car (bcat-analysis--function-split (bcat-analysis--word-function w) "sa"))))
                   (bcat-analysis--sub-cells w)))
           words)
   "sa"))

;; The parts of a word in tables of their own (Carsten 5.11., TĀ 1.1:
;; „Können wir beim Sanskrit irgendwie mit Untertabellen arbeiten und die
;; Redundanzen rausnehmen?“ — of three drafts „B … einzeilig, kursiv“).

(defun bcat-analysis--part-norm (s)
  "S as a part's form is compared with the word: lower case, without √,
hyphens, spaces, digits, brackets and diacritics — tantra is a piece of
tantrālokaḥ, sva of svātantrya."
  (replace-regexp-in-string
   "[\u0300-\u036F]" ""
   (ucs-normalize-NFD-string (downcase (replace-regexp-in-string "[√·+()0-9 \t-]" "" (or s ""))))))

(defun bcat-analysis--part-item (text)
  "(FORM . REST) of one part TEXT of a word: its first word and the rest
— „√mal (1) be impure“ → („√mal“ . „(1) be impure“)."
  (let ((text (string-trim text)))
    (if (string-match "\\`\\(\\S-+\\)[ \t]+\\(.+\\)\\'" text)
        (cons (match-string 1 text) (match-string 2 text))
      (cons text ""))))

(defun bcat-analysis--part-form-p (form keys)
  "Non-nil when FORM, the first word of a part, is Sanskrit — a root
\(√), a hyphenated form (vi-, -ta, vi-bhu), a word with diacritics, a
piece of one of KEYS (the members of the word, its formation) or with
its first three letters in one (mahas, the stem of mahāḥ) —, not the
start of a meaning: TĀ 1007 gives the parts of mad-vāk as „my ·
speech“."
  (let ((n (bcat-analysis--part-norm form)))
    (or (string-match-p "√" form)
        (string-search "-" form)
        (string-match-p "[^[:ascii:]]" form)
        (and (>= (length n) 2)
             (cl-some (lambda (k)
                        (let ((nk (bcat-analysis--part-norm k)))
                          (or (string-search n nk)
                              (and (>= (length n) 3) (string-search (substring n 0 3) nk)))))
                      keys)
             t))))

(defun bcat-analysis--part-groups (items keys)
  "ITEMS ((FORM . REST) …), the parts of a word in order, by its members
KEYS (what each member is written as): a list with the items of each
member, in order.  Most answers separate every part by „ · “, so the
members cannot be read from the parts (5.11.: of 395 words in the files
6 have „;“ between the members, 37 match neither the formation's pieces
nor its members).  The assignment in order that finds the most of the
forms in their members wins (10 a letter); a member left empty costs
20 — every member has parts —, a suffix (-a) starting a member or a
member starting after a prefix (vi-) 5: Claude writes members with a
hyphen too (śrīmat-mahā-…: „mahā- great“), a bare meaning before the
forms of its member 1; on a tie a suffix stays with the part before it,
anything else goes with the part after it."
  (let* ((n (length items)) (m (length keys))
         (nkeys (vconcat (mapcar #'bcat-analysis--part-norm keys)))
         (forms (vconcat (mapcar (lambda (i) (bcat-analysis--part-norm (car i))) items)))
         (suffix (vconcat (mapcar (lambda (i) (string-prefix-p "-" (car i))) items)))
         (prefix (vconcat (mapcar (lambda (i) (string-suffix-p "-" (car i))) items)))
         (meaning (vconcat (mapcar (lambda (i) (not (bcat-analysis--part-form-p (car i) keys))) items)))
         (score (lambda (k j)
                  (let ((f (aref forms k)))
                    (if (and (> (length f) 0) (string-search f (aref nkeys j))) (* 10 (length f)) 0))))
         (best (make-vector (* n m) 0))
         (from (make-vector (* n m) -1)))
    (if (or (zerop n) (< m 2))
        (list items)
      (dotimes (j m)
        (aset best j (+ (funcall score 0 j) 20)))
      (cl-loop
       for k from 1 below n do
       (let ((cost (+ (if (aref suffix k) 5 0) (if (aref prefix (1- k)) 5 0))))
         (dotimes (j m)
           (let ((same (- (aref best (+ (* (1- k) m) j))
                          ;; a bare meaning before the forms that name
                          ;; the member (AS 3: „merit“ before abhi-)
                          (if (and (aref meaning (1- k)) (> (funcall score k j) 0)) 1 0)))
                 (prev j))
             ;; A new member from an earlier one.  On a tie the part
             ;; before decides: a suffix stays with what precedes it
             ;; (a-dva-ya · maya), anything else goes with what follows
             ;; (AS 3, „merit · not · merit“ for dharma-adharma: „not“
             ;; opens adharma).
             (dotimes (j2 j)
               (let ((adv (- (+ (aref best (+ (* (1- k) m) j2)) 20) cost)))
                 (when (or (> adv same) (and (= adv same) (aref suffix (1- k))))
                   (setq same adv prev j2))))
             (aset best (+ (* k m) j) (+ (funcall score k j) same))
             (aset from (+ (* k m) j) prev)))))
      (let* ((j (cl-loop with bj = 0 for j from 1 below m
                         when (>= (aref best (+ (* (1- n) m) j)) (aref best (+ (* (1- n) m) bj)))
                         do (setq bj j)
                         finally return bj))
             (groups (make-vector m nil)))
        (cl-loop for k downfrom (1- n) to 0 do
                 (push (nth k items) (aref groups j))
                 (setq j (aref from (+ (* k m) j))))
        (append groups nil)))))

(defun bcat-analysis--word-parts (w)
  "The parts of the Sanskrit word W by its members, (HEADS . GROUPS): the
members as the word writes them (split at its hyphens) and for each its
parts, (FORM . REST) …; nil without parts.  „;“ between the parts
separates the members when there are as many as the word has (TĀ 1.1),
else the parts go to the members by their forms
\(`bcat-analysis--part-groups'; the formation helps when it has as many
members)."
  (let* ((parts (bcat-analysis--field w 'parts))
         (heads (split-string (bcat-analysis--field w 'word) "-" t "[ \t]*"))
         (formation (bcat-analysis--field w 'formation))
         (fms (and (not (member formation '("" "—" "-")))
                   (split-string formation "[ \t]*·[ \t]*" t)))
         (keys (cl-loop for h in heads for i from 0
                        collect (if (= (length fms) (length heads)) (concat h " " (nth i fms)) h)))
         (members (split-string parts "[ \t]*;[ \t]+" t))
         (items (lambda (text sep) (mapcar #'bcat-analysis--part-item (split-string text sep t)))))
    (unless (or (string-empty-p parts) (null heads))
      (cons heads
            (cond ((= 1 (length heads)) (list (funcall items parts "[ \t]*·[ \t]*")))
                  ((= (length members) (length heads))
                   (mapcar (lambda (mem) (funcall items mem "[ \t]*·[ \t]*")) members))
                  (t (bcat-analysis--part-groups (funcall items parts "[ \t]*[·;][ \t]*") keys)))))))

(defun bcat-analysis--part-cell (item keys)
  "The part ITEM (FORM . REST) as it stands in the table: the Sanskrit
form in italics before its meaning (Carsten 5.11.: „kursiv“); a part
that only gives a meaning, or a form with a slash, as it is.  KEYS:
the word's members (`bcat-analysis--part-form-p')."
  (let ((form (car item)) (rest (cdr item)))
    (string-trim
     (if (and (bcat-analysis--part-form-p form keys) (not (string-search "/" form)))
         (concat "/" form "/ " rest)
       (concat form " " rest)))))

(defun bcat-analysis--bare-part-p (items head keys)
  "Non-nil when ITEMS, the parts of the member HEAD, are only the member
itself — one part whose form is HEAD (kalā: „kalā part, power“), not a
root (cit: „√cit (1) consciousness“ stays), or only a meaning (kāya:
„body“): then its meaning stands alone."
  (and (= 1 (length items))
       (or (not (bcat-analysis--part-form-p (caar items) keys))
           (and (not (string-search "√" (caar items)))
                (equal (bcat-analysis--part-norm (caar items)) (bcat-analysis--part-norm head))))))

(defun bcat-analysis--header-rule (line)
  "The hline under the table row LINE: dashes as wide as each cell."
  (concat "|" (mapconcat (lambda (cell) (make-string (string-width cell) ?-))
                         (butlast (cdr (split-string line "|")))
                         "+")
          "|"))

(defun bcat-analysis--header-table (rows)
  "ROWS (lists of cells) as an org table whose first row is its header —
an hline under it."
  (let ((lines (split-string (string-trim-right (tibetan-gloss-table--format-rows rows)) "\n")))
    (string-join (cons (car lines) (cons (bcat-analysis--header-rule (car lines)) (cdr lines))) "\n")))

(defun bcat-analysis--column-table (head cells)
  "A table of one column: HEAD as its header row, CELLS below, one a
row — the parts of a word of one member (Carsten 9.11., TĀ 1009: under
the translation only tables, „der Formatwechsel (Tabelle/Fließtext)
verwirrt mich“)."
  (bcat-analysis--header-table (cons (list head) (mapcar #'list cells))))

(defun bcat-analysis--member-meanings (w heads)
  "The meanings of the members HEADS of the compound W as its answer
gives them (`members', 18.11.: „ · “ between them) — a list as long as
HEADS, or nil: no compound, none given, or not one for each member."
  (let ((meanings (split-string (bcat-analysis--field w 'members) "[ \t]*·[ \t]*" t "[ \t]+")))
    (and (cdr heads) (= (length meanings) (length heads)) meanings)))

;; ----------------------------------------------------------------------------
;; The parts under the words, in the table of the segment (18.11.)
;; ----------------------------------------------------------------------------
;; Carsten 18.11., after the Tantrāloka reading (TĀ 1016): „können wir mal
;; versuchen, dass die Word Formation näher am Text ist, also als Subtable
;; im Table?“ — of two prototypes: „B, bau es so ein“; „mir geht es mehr
;; um's .org. Das nehme ich im Reading.“  The table of a Sanskrit segment
;; has two bands: word, gloss, form — a rule — and under each word what
;; explains it (`bcat-analysis--sub-cells').

(defcustom bcat-analysis-emphasis-hidden t
  "Non-nil: the columns of a table with the parts under its words are
set for hidden emphasis markers (`org-hide-emphasis-markers') — the
slashes of „/vi-/ away“ take no room on the screen, the bars stand in
line there and are off by two in the raw text.  Carsten reads with the
markers hidden (18.11.).  An option of the tool, not read from the
init: a batch run sets the same file."
  :type 'boolean
  :group 'buddhist-cat)

(defun bcat-analysis--strip-emphasis (s)
  "S without the slashes of its italics (/x/ → x) — what stands on the
screen when the emphasis markers are hidden."
  (let ((re "\\(\\`\\|[ \t('\"{-]\\)/\\([^/ \t\n]\\(?:[^/\n]*[^/ \t\n]\\)?\\)/\\(\\'\\|[ \t.,:;!?')}\"-]\\)")
        (s (or s "")))
    ;; twice: a match takes the character after it with it
    (dotimes (_ 2)
      (setq s (replace-regexp-in-string re "\\1\\2\\3" s)))
    s))

(defun bcat-analysis--cell-width (cell)
  "The width of the table CELL as it stands in the window: a link by its
text, italics without their slashes when `bcat-analysis-emphasis-hidden'."
  (tibetan-gloss-table--visible-width
   (if bcat-analysis-emphasis-hidden (bcat-analysis--strip-emphasis cell) cell)))

(defun bcat-analysis--resolution-lines (w)
  "The steps in which the compound W is resolved (`layers' of its answer),
each a line „MEMBERS [RELATION] WORDS“, from the last member backwards
— the words read down are the translation of the compound.  A step
without its members or its words is left out; nil for fewer than two."
  (let* ((layers (alist-get 'layers w))
         (lines (delq nil
                      (mapcar (lambda (l)
                                (when (consp l)
                                  (let ((part (bcat-analysis--field l 'part))
                                        (role (string-trim (bcat-analysis--field l 'role) "\\[" "\\]"))
                                        (phrase (bcat-analysis--field l 'phrase)))
                                    (unless (or (string-empty-p part) (string-empty-p phrase))
                                      (format "%s [%s] %s" part (if (string-empty-p role) "—" role) phrase)))))
                              (and (listp layers) layers)))))
    (and (cdr lines) lines)))

(defun bcat-analysis--sub-cells (w)
  "What stands under the Sanskrit word W in the table of its segment, a
list of cells, or nil when its parts only repeat the word.  A word of
one member: its parts, one a cell, the Sanskrit form in italics.  A
compound: a line per member — „MEMBER: MEANING ← PARTS“ with the
meanings of its answer (`bcat-analysis--member-meanings'), „MEMBER:
PARTS“ without; a member that is one bare stem has only its meaning —,
then the steps of its resolution (`bcat-analysis--resolution-lines')."
  (let ((wp (bcat-analysis--word-parts w)))
    (when wp
      (let* ((heads (car wp)) (groups (cdr wp))
             (word (bcat-analysis--field w 'word))
             (keys (cons (bcat-analysis--field w 'formation) heads))
             (meanings (bcat-analysis--member-meanings w heads))
             (cell (lambda (item) (bcat-analysis--part-cell item keys))))
        (if (= 1 (length heads))
            (unless (bcat-analysis--bare-part-p (car groups) word keys)
              (mapcar (lambda (item) (tibetan-gloss-table--cell (funcall cell item))) (car groups)))
          (let ((lines
                 (cl-mapcar
                  (lambda (head items meaning)
                    (let* ((bare (bcat-analysis--bare-part-p items head keys))
                           (parts (cond ((not bare) (mapconcat cell items " · "))
                                        ;; the member itself: its meaning
                                        ((bcat-analysis--part-form-p (caar items) keys) (cdar items))
                                        ;; only a meaning (mad-vāk: „my“)
                                        (t (mapconcat cell items " · ")))))
                      (cond ((and meaning (not bare)) (format "%s: %s ← %s" head meaning parts))
                            (meaning (format "%s: %s" head meaning))
                            ((string-empty-p (string-trim parts)) nil)
                            (t (format "%s: %s" head parts)))))
                  heads groups (or meanings (make-list (length heads) nil)))))
            (when (cl-some #'identity lines)
              (mapcar #'tibetan-gloss-table--cell
                      (append (delq nil lines) (bcat-analysis--resolution-lines w))))))))))

(defun bcat-analysis--band-table (chunk cell-width)
  "CHUNK — columns, each (OVER . SUB): the cells of the overview and
those under them — as ONE org table: the overview (a cell wider than
CELL-WIDTH broken into further rows, `bcat-analysis-wrap-rows'), a rule,
the rows under it; no rule when no column has any.  The columns as wide
as their widest cell on the screen (`bcat-analysis--cell-width')."
  (let* ((height (apply #'max 0 (mapcar (lambda (c) (length (car c))) chunk)))
         (over (bcat-analysis-wrap-rows
                (cl-loop for r below height
                         collect (mapcar (lambda (c) (or (nth r (car c)) "")) chunk))
                cell-width))
         (depth (apply #'max 0 (mapcar (lambda (c) (length (cdr c))) chunk)))
         (sub (cl-loop for r below depth
                       collect (mapcar (lambda (c) (or (nth r (cdr c)) "")) chunk)))
         (widths (cl-loop for j below (length chunk)
                          collect (apply #'max 1 (mapcar (lambda (row) (bcat-analysis--cell-width (nth j row)))
                                                         (append over sub)))))
         (line (lambda (row)
                 (concat "| "
                         (string-join
                          (cl-mapcar (lambda (cell w)
                                       (concat cell (make-string (max 0 (- w (bcat-analysis--cell-width cell))) ?\s)))
                                     row widths)
                          " | ")
                         " |")))
         (rule (concat "|" (mapconcat (lambda (w) (make-string (+ w 2) ?-)) widths "+") "|")))
    (string-join (append (mapcar line over)
                         (and sub (cons rule (mapcar line sub))))
                 "\n")))

(defun bcat-analysis--band-chunks (columns width charge cell-min)
  "(CHUNKS . CELL-WIDTH) for COLUMNS ((OVER . SUB) …) at the table WIDTH,
as `bcat-analysis--best-chunks' gives it for the overview — the widest
cell under a word counted in as one that is never broken."
  (let* ((pseudo (mapcar (lambda (c)
                           (append (car c)
                                   (list (make-string
                                          (apply #'max 0 (mapcar #'bcat-analysis--cell-width (cdr c)))
                                          ?x))))
                         columns))
         (best (bcat-analysis--best-chunks pseudo width charge cell-min))
         (rest columns))
    (cons (mapcar (lambda (ch) (prog1 (seq-take rest (length ch))
                                 (setq rest (seq-drop rest (length ch)))))
                  (car best))
          (cdr best))))

(defun bcat-analysis--band-tables (columns &optional lang)
  "COLUMNS ((OVER . SUB) …) of a segment as org tables in the layout of
the file for the source language LANG (`bcat-analysis--layout'), blank
lines between (`bcat-analysis--band-chunks', `bcat-analysis--band-table');
\"\" without columns."
  (if (null columns)
      ""
    (let ((best (bcat-analysis--band-chunks columns (bcat-analysis--layout 'table-width (or lang "sa")) 3
                                            (bcat-analysis--layout 'cell-width (or lang "sa")))))
      (mapconcat (lambda (chunk) (bcat-analysis--band-table chunk (cdr best))) (car best) "\n\n"))))

(defun bcat-analysis-band-columns (tables)
  "The columns of TABLES, the tables of a Sanskrit segment (blank lines
between), each (OVER . SUB): OVER the cells of the overview — word,
gloss, form, the lines of a broken cell joined — and SUB what stands
under the rule in its column, as it stands (no joining there: such a
line may end in „;“).  A table whose first band has not these three
rows (five, before 5.11.) is read as `bcat-analysis--table-columns'
reads it, its columns without SUB."
  (cl-loop
   for tb in (split-string (or tables "") "\n[ \t]*\n" t)
   append
   (let* ((lines (split-string tb "\n" t))
          (rule-p (lambda (l) (string-match-p "\\`[ \t]*|-" l)))
          (cells (lambda (l) (mapcar #'string-trim (butlast (cdr (split-string l "|"))))))
          (rule (cl-position-if rule-p lines))
          (over (bcat-analysis--join-continuations
                 (mapcar cells (if rule (seq-take lines rule) lines)))))
     (if (/= 3 (length over))
         (mapcar #'list (bcat-analysis--table-columns tb))
       (let ((sub (mapcar cells (cl-remove-if rule-p (and rule (seq-drop lines (1+ rule)))))))
         (cl-loop for j below (length (car over))
                  for o = (mapcar (lambda (row) (or (nth j row) "")) over)
                  unless (cl-every #'string-empty-p o)
                  collect (cons o (cl-remove-if #'string-empty-p
                                                (mapcar (lambda (row) (or (nth j row) "")) sub)))))))))

(defun bcat-analysis--header-table-rows (table)
  "(HEADS . ROWS) of TABLE when it is a table with a header row — its
second line a rule, no other —, else nil."
  (let* ((lines (split-string table "\n" t))
         (rule-p (lambda (l) (string-match-p "\\`[ \t]*|-" l)))
         (cells (lambda (l) (mapcar #'string-trim (butlast (cdr (split-string l "|")))))))
    (when (and (cddr lines) (funcall rule-p (nth 1 lines))
               (not (funcall rule-p (car lines)))
               (not (cl-some rule-p (cddr lines))))
      (cons (funcall cells (car lines)) (mapcar cells (cddr lines))))))

(defun bcat-analysis--resolution-rows (table)
  "The rows of TABLE when it is the table of a compound's resolution as
files held it on 18.11. — no rule, three cells a row, the relation in
brackets in the middle —, else nil."
  (let ((rows (mapcar (lambda (l) (mapcar #'string-trim (butlast (cdr (split-string l "|")))))
                      (split-string table "\n" t))))
    (and rows
         (cl-every (lambda (r) (and (= 3 (length r)) (string-match-p "\\`\\[.*\\]\\'" (nth 1 r)))) rows)
         rows)))

(defun bcat-analysis--member-line (head cells meanings-p)
  "The line of the member HEAD with CELLS, the cells under it in a table
of a compound's parts; MEANINGS-P: the first is its meaning."
  (let* ((cells (cl-remove-if #'string-empty-p cells))
         (meaning (and meanings-p (car cells)))
         (parts (string-join (if meanings-p (cdr cells) cells) " · ")))
    (cond ((and meaning (not (string-empty-p parts))) (format "%s: %s ← %s" head meaning parts))
          (meaning (format "%s: %s" head meaning))
          ((string-empty-p parts) nil)
          (t (format "%s: %s" head parts)))))

(defun bcat-analysis--word-key (word)
  "WORD as it is compared when a table under Word Formation names it
differently than the overview: without the hyphens of the padapāṭha, a
final s or r a visarga (the stem „mahas“ for „mahaḥ“)."
  (replace-regexp-in-string "[sr]\\'" "ḥ" (replace-regexp-in-string "-" "" (or word ""))))

(defun bcat-analysis--block-word (name words free-p)
  "Index of the word of WORDS that a table under Word Formation calls
NAME, among those FREE-P (a function of the index) lets through, or nil:
the same word; else the same after `bcat-analysis--word-key'; else — a
name of eight letters and more — the only word at most two letters from
it (the word in its sandhi: „tattadbhāva…“ for „tat-tat-bhāva-…“; as
near to two words it is no one's)."
  (let* ((key (bcat-analysis--word-key name))
         (idx (cl-loop for i below (length words) when (funcall free-p i) collect i)))
    (or (cl-find-if (lambda (i) (equal (nth i words) name)) idx)
        (cl-find-if (lambda (i) (equal (bcat-analysis--word-key (nth i words)) key)) idx)
        (and (>= (length key) 8)
             (let ((near (cl-remove-if-not
                          (lambda (i) (<= (string-distance key (bcat-analysis--word-key (nth i words))) 2))
                          idx)))
               (and near (null (cdr near)) (car near)))))))

(defun bcat-analysis--block-compound (name words free-p)
  "Index of the only compound of WORDS, among those FREE-P lets through,
that has NAME among its members (the same, or the same after
`bcat-analysis--word-key'), or nil — a member of two compounds is no
one's."
  (let* ((key (bcat-analysis--word-key name))
         (hits (cl-loop for i below (length words)
                        when (and (funcall free-p i)
                                  (let ((members (split-string (nth i words) "-" t)))
                                    (and (cdr members)
                                         (cl-some (lambda (m) (or (equal m name)
                                                                  (equal (bcat-analysis--word-key m) key)))
                                                  members))))
                        collect i)))
    (and hits (null (cdr hits)) (car hits))))

(defun bcat-analysis--block-sub-cells (formation words)
  "(CELLS . LEFTOVER) for FORMATION, the tables under Word Formation of
a Sanskrit segment as files held them until 18.11. (blank lines
between), and WORDS, the words of its overview: CELLS a list as long as
WORDS — for each word what the block says of it, as it goes under the
word (`bcat-analysis--sub-cells') — and LEFTOVER the tables that belong
to no word, as they are (nil for none).  A word of one member: its
table of one column or its column of the common table.  A compound: its
table (several, when it was split for the width) — a first row without
a Sanskrit form in italics is the row of its members' meanings —, then
the table of its resolution.  A table may name its word differently than
the overview (`bcat-analysis--block-word': the files of the first weeks
— the stem, the word in its sandhi); a table of one member of a compound
is a line of that compound (`bcat-analysis--block-compound')."
  (let* ((n (length words))
         (words (mapcar #'bcat-analysis-plain-cell words))
         (cells (make-vector n nil))
         (taken (make-vector n nil))
         (free (lambda (pred) (cl-loop for i below n
                                       when (and (not (aref taken i)) (funcall pred (nth i words)))
                                       return i)))
         (pending '()) (pending-tables '()) (last nil) (leftover '()))
    (dolist (tb (split-string (or formation "") "\n[ \t]*\n" t))
      (let ((header (bcat-analysis--header-table-rows tb))
            (resolution (bcat-analysis--resolution-rows tb)))
        (cond
         ((and resolution last)
          (aset cells last (append (aref cells last)
                                   (mapcar (lambda (r) (string-join r " ")) resolution))))
         ((null header) (push tb leftover))
         (t
          (let* ((heads (mapcar #'bcat-analysis-plain-cell (car header)))
                 (rows (cdr header))
                 (meanings-p (and rows
                                  (cl-some (lambda (c) (not (string-empty-p c))) (car rows))
                                  (not (cl-some (lambda (c) (string-match-p "/[^/ \n][^/\n]*/" c)) (car rows)))))
                 (columns (cl-loop for j below (length heads)
                                   collect (list (nth j heads)
                                                 (mapcar (lambda (r) (or (nth j r) "")) rows)
                                                 meanings-p)))
                 (members (append pending columns))
                 (joined (mapconcat #'car members "-"))
                 (untaken (lambda (i) (not (aref taken i))))
                 (whole (funcall free (lambda (w) (equal w joined))))
                 (prefix (funcall free (lambda (w) (string-prefix-p (concat joined "-") w))))
                 (compound (lambda (i)
                             (aset taken i t)
                             (aset cells i (append (aref cells i)
                                                   (delq nil (mapcar (lambda (m) (apply #'bcat-analysis--member-line m))
                                                                     members))))
                             (setq last i pending nil pending-tables nil)))
                 (near nil) (of nil))
            (cond
             ;; the members of a compound, complete
             ((and whole (cdr members)) (funcall compound whole))
             ;; words of one member, each a column
             ((and (null pending)
                   (let ((idx '()) (ok t))
                     (dolist (h heads)
                       (let ((i (and ok (bcat-analysis--block-word
                                         h words (lambda (i) (and (not (aref taken i)) (not (memq i idx))))))))
                         (if i (push i idx) (setq ok nil))))
                     (when ok
                       (cl-loop for i in (nreverse idx) for col in columns
                                do (aset taken i t)
                                (aset cells i (append (aref cells i) (cl-remove-if #'string-empty-p (nth 1 col)))))
                       (setq last nil)
                       t))))
             ;; a compound split for the width: more of its members follow
             (prefix
              (setq pending members)
              (push tb pending-tables))
             ;; the members of a compound that the overview splits differently
             ((and (cdr members) (setq near (bcat-analysis--block-word joined words untaken)))
              (funcall compound near))
             ;; one member of a compound: a line of that compound
             ((and (null pending) (null (cdr columns))
                   (setq of (bcat-analysis--block-compound (car heads) words untaken)))
              (let ((line (apply #'bcat-analysis--member-line (car columns))))
                (when line (aset cells of (append (aref cells of) (list line)))))
              (setq last nil))
             (t (push tb leftover))))))))
    ;; members that found no end are no word's
    (setq leftover (append pending-tables leftover))
    (cons (append cells nil) (nreverse leftover))))

(defun bcat-analysis-sanskrit-relayout-tables (tables formation)
  "(TABLES LEFTOVER OVERFLOW) for a Sanskrit segment as a file holds it:
TABLES its tables, FORMATION what stands under Word Formation in a file
written before 18.11. midday (or nil).  The tables are set anew in the
layout of the file with, under each word, what the tables show there —
that wins — or else what FORMATION says of the word
\(`bcat-analysis--block-sub-cells'); a form wider than
`bcat-analysis-function-width' is cut, its rest a line for Structure in
OVERFLOW.  LEFTOVER: the tables of FORMATION that went under no word,
as they are — they stay under Word Formation.  Tables in another form
\(five rows) come back as they are.  C-c u L."
  (let ((columns (bcat-analysis-band-columns tables)))
    (cond
     ((null columns) (list tables (and formation (not (string-blank-p formation)) formation) nil))
     ((cl-some (lambda (c) (/= 3 (length (car c)))) columns)
      (list tables (and formation (not (string-blank-p formation)) formation) nil))
     (t
      (let* ((block (bcat-analysis--block-sub-cells formation (mapcar #'caar columns)))
             (overflow '())
             (kept '())
             (columns
              (cl-mapcar
               (lambda (col from-block)
                 (let* ((over (car col))
                        (word (bcat-analysis-plain-cell (nth 0 over)))
                        (fn (bcat-analysis-plain-cell (nth 2 over)))
                        (sp (bcat-analysis--function-split (bcat-analysis--trim-lemma fn word) "sa")))
                   (when (cdr sp)
                     (push (format "/%s/: %s" word (cdr sp)) overflow))
                   ;; what the table shows under the word wins; then the
                   ;; block's table of the word is no word's any more
                   (when (and (cdr col) from-block) (push word kept))
                   (cons (if (equal (car sp) fn)
                             over
                           (list (nth 0 over) (nth 1 over) (tibetan-gloss-table--cell (car sp))))
                         (or (cdr col) from-block))))
               columns (car block)))
             (leftover (cdr block)))
        ;; a block table of a word whose column is filled already stays
        (when kept
          (dolist (tb (split-string (or formation "") "\n[ \t]*\n" t))
            (let ((header (bcat-analysis--header-table-rows tb)))
              (when (and header (null (cdar header))
                         (member (bcat-analysis-plain-cell (caar header)) kept)
                         (not (member tb leftover)))
                (setq leftover (append leftover (list tb)))))))
        (list (bcat-analysis--band-tables columns "sa")
              (and leftover (string-join leftover "\n\n"))
              (nreverse overflow)))))))

(defun bcat-analysis-parts-line-table (line)
  "The table of one column for LINE, the line of a word of one member as
the files of 5.11. (draft B) held it — „/hṛdayam/: /hṛd/ heart · /-aya/
suffix“ —: the word above its parts as the line has them (a hand
correction stays; `bcat-analysis--column-table').  nil for anything
else.  C-c u L (Carsten 9.11., TĀ 1009)."
  (let ((line (string-trim (or line ""))))
    (when (string-match "\\`/\\([^/\n]+\\)/: \\([^\n]+\\)\\'" line)
      (bcat-analysis--column-table (match-string 1 line)
                                   (split-string (match-string 2 line) " · " t "[ \t]+")))))

(defun bcat-analysis-split-word-tables (tables)
  "(TABLES nil OVERFLOW) for TABLES, the tables of a Sanskrit segment as
files held them before 5.11. — five rows: word, formation, parts,
gloss, form —: read back into words and set as the tables of today, the
parts under the words (`bcat-analysis-sanskrit-tables'; until 18.11.
tables of their own for Word Formation, the second element), with the
lines for Structure of a form cell cut
\(`bcat-analysis--function-overflow').  nil for other tables — Tibetan,
an overview already.  C-c u L."
  (let ((columns (apply #'append (mapcar #'bcat-analysis--table-columns
                                         (split-string (or tables "") "\n[ \t]*\n" t)))))
    (when (and columns
               (cl-every (lambda (col) (= 5 (length col))) columns)
               (not (string-match-p "[\u0F00-\u0FFF]" (caar columns))))
      (let ((words (mapcar (lambda (col)
                             (cl-mapcar (lambda (key cell) (cons key (bcat-analysis-plain-cell cell)))
                                        '(word formation parts gloss form) col))
                           columns)))
        ;; 18.11.: the parts go under the words in the tables themselves
        (list (bcat-analysis-sanskrit-tables words)
              nil
              (bcat-analysis--function-overflow words "sa"))))))

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

(defconst bcat-analysis--skeleton-fold
  '((?ā . ?a) (?ī . ?i) (?ū . ?u) (?ṛ . ?r) (?ṝ . ?r) (?ḷ . ?l) (?ṅ . ?n) (?ñ . ?n)
    (?ṇ . ?n) (?ṭ . ?t) (?ḍ . ?d) (?ś . ?s) (?ṣ . ?s) (?ṃ . ?m) (?ṁ . ?m) (?ḥ . ?h))
  "IAST letters without their diacritics, for `bcat-analysis--skeleton'.")

(defconst bcat-analysis--skeleton-class
  '((?k . ?K) (?g . ?K) (?c . ?C) (?j . ?C) (?t . ?T) (?d . ?T) (?p . ?P) (?b . ?P)
    (?m . ?M) (?n . ?M) (?s . ?S) (?h . ?S) (?r . ?S) (?v . ?V) (?w . ?V))
  "The classes sandhi moves within: voiced and voiceless, the nasals,
visarga with s and r — for `bcat-analysis--skeleton'.")

(defun bcat-analysis--skeleton (s)
  "The skeleton of the IAST string S, (CLASSES . POSITIONS) with S as
compared (NFC, lower case): letters only, no diacritics, an aspirate's
h dropped, vowels one class and a run of them one (a + i = e), the
consonants in their sandhi classes (`bcat-analysis--skeleton-class');
POSITIONS the index in S of each class."
  (let ((s (downcase (ucs-normalize-NFC-string s))) (out '()) (pos '()) (prev nil))
    (dotimes (i (length s))
      (let ((f (or (cdr (assq (aref s i) bcat-analysis--skeleton-fold)) (aref s i))))
        (when (and (>= f ?a) (<= f ?z))
          (cond
           ((and (eq f ?h) (memq prev '(?k ?g ?c ?j ?t ?d ?p ?b))))
           ((memq f '(?a ?i ?u ?e ?o))
            (unless (eq (car out) ?A) (push ?A out) (push i pos)))
           (t (push (or (cdr (assq f bcat-analysis--skeleton-class)) f) out) (push i pos))))
        (setq prev f)))
    (list (concat (nreverse out)) (vconcat (nreverse pos)) s)))

(defconst bcat-analysis--anchor-length 6
  "Text and padapāṭha are aligned where their skeletons share a stretch
of at least this many classes (`bcat-analysis--skeleton-diff').  Shorter
agreements are chance between unrelated lines — the classes are few —;
calibrated on the kept answers of TĀ 1, AS and MSA IX (16.11.).")

(defun bcat-analysis--lcs-pairs (a b &optional ok)
  "The pairs (I . J) of a longest common subsequence of the strings A
and B, in order; OK, a function of I and J, says which positions may
be paired (default: equal characters)."
  (let* ((n (length a)) (m (length b))
         (ok (or ok (lambda (i j) (eq (aref a i) (aref b j)))))
         (l (make-vector (1+ n) nil)))
    (dotimes (i (1+ n)) (aset l i (make-vector (1+ m) 0)))
    (cl-loop for i from (1- n) downto 0
             do (let ((row (aref l i)) (below (aref l (1+ i))))
                  (cl-loop for j from (1- m) downto 0
                           do (aset row j (if (funcall ok i j)
                                              (1+ (aref below (1+ j)))
                                            (max (aref below j) (aref row (1+ j))))))))
    (let ((i 0) (j 0) (pairs '()))
      (while (and (< i n) (< j m))
        (cond ((and (funcall ok i j)
                    (= (aref (aref l i) j) (1+ (aref (aref l (1+ i)) (1+ j)))))
               (push (cons i j) pairs) (cl-incf i) (cl-incf j))
              ((>= (aref (aref l (1+ i)) j) (aref (aref l i) (1+ j))) (cl-incf i))
              (t (cl-incf j))))
      (nreverse pairs))))

(defun bcat-analysis--unmatched-runs (len matched)
  "The runs (START . END), inclusive, of the positions below LEN that are
not in MATCHED (ascending)."
  (let ((runs '()) (start nil))
    (dotimes (x len)
      (if (and matched (= x (car matched)))
          (progn (when start (push (cons start (1- x)) runs) (setq start nil))
                 (setq matched (cdr matched)))
        (unless start (setq start x))))
    (when start (push (cons start (1- len)) runs))
    (nreverse runs)))

(defun bcat-analysis--skeleton-diff (a b)
  "Where the skeletons A (the text) and B (the padapāṭha) differ beyond
sandhi: (A-RUNS . B-RUNS), each run (START . END) inclusive; nil when
the strings are too long to compare.  16.11., TĀ 1001: a plain common
subsequence aligns unrelated lines too.  So the two are aligned first
at their common stretches of `bcat-analysis--anchor-length' classes or
more; of each stretch between such anchors: when it is large and unlike
\(four classes or more without a partner, less than three quarters in
common) each side with three or more left over counts whole; else its
runs of three classes or more without a partner count.  What sandhi
does between short words — a class here and there — stays below that."
  (let ((n (length a)) (m (length b)))
    (when (<= (* (1+ n) (1+ m)) 1000000)
      ;; span[i][j]: the length of the common stretch through (i, j)
      (let ((span (make-vector (1+ n) nil)))
        (dotimes (i (1+ n)) (aset span i (make-vector (1+ m) 0)))
        (dotimes (i n)
          (dotimes (j m)
            (when (eq (aref a i) (aref b j))
              (aset (aref span (1+ i)) (1+ j) (1+ (aref (aref span i) j))))))
        (let ((next (make-vector (1+ m) 0)))
          (cl-loop for i from (1- n) downto 0
                   do (let ((cur (make-vector (1+ m) 0)))
                        (cl-loop for j from (1- m) downto 0
                                 do (if (eq (aref a i) (aref b j))
                                        (let ((run (1+ (aref next (1+ j)))))
                                          (aset cur j run)
                                          (aset (aref span (1+ i)) (1+ j)
                                                (+ (aref (aref span (1+ i)) (1+ j)) run -1)))
                                      (aset (aref span (1+ i)) (1+ j) 0)))
                        (setq next cur))))
        (let ((anchors (append (bcat-analysis--lcs-pairs
                                a b (lambda (i j) (>= (aref (aref span (1+ i)) (1+ j))
                                                      bcat-analysis--anchor-length)))
                               (list (cons n m))))
              (pi 0) (pj 0) (ra '()) (rb '()))
          (dolist (anchor anchors)
            (let* ((i (car anchor)) (j (cdr anchor))
                   (la (- i pi)) (lb (- j pj)))
              (when (or (> la 0) (> lb 0))
                (let* ((inner (bcat-analysis--lcs-pairs (substring a pi i) (substring b pj j)))
                       (l (length inner)))
                  (if (and (>= (- (max la lb) l) 4)
                           (< (* 4 l) (* 3 (max la lb))))
                      (progn (when (>= (- la l) 3) (push (cons pi (1- i)) ra))
                             (when (>= (- lb l) 3) (push (cons pj (1- j)) rb)))
                    (dolist (r (bcat-analysis--unmatched-runs la (mapcar #'car inner)))
                      (when (>= (- (cdr r) (car r)) 2)
                        (push (cons (+ pi (car r)) (+ pi (cdr r))) ra)))
                    (dolist (r (bcat-analysis--unmatched-runs lb (mapcar #'cdr inner)))
                      (when (>= (- (cdr r) (car r)) 2)
                        (push (cons (+ pj (car r)) (+ pj (cdr r))) rb))))))
              (setq pi (1+ i) pj (1+ j))))
          (cons (nreverse ra) (nreverse rb)))))))

(defun bcat-analysis--gap-text (gap)
  "The text GAP of a skeleton run as shown: trimmed, without a bare vowel
of a neighbouring word at either end („yām ā“ → „yām“) — vowels are one
class, so a run may take one in."
  (let ((parts (split-string (string-trim gap) "[ \t]+" t))
        (vowel-only (lambda (w) (string-match-p "\\`[aāiīuūṛṝeoḥṃ]+\\'" w))))
    (when (and (cdr parts) (funcall vowel-only (car (last parts))))
      (setq parts (butlast parts)))
    (when (and (cdr parts) (funcall vowel-only (car parts)))
      (setq parts (cdr parts)))
    (string-join parts " ")))

(defun bcat-analysis-padapatha-check (samhita padapatha)
  "What the padapāṭha PADAPATHA (Claude's words, in IAST) leaves out of
the transmitted segment SAMHITA or adds to it, beyond what sandhi
explains — a sentence for the file, or nil.  Carsten 16.11. (TĀ
1013–1020: abhinavā for abhinavāyām, vyatiriktena for vyatiriktatvena,
half a segment missing; TĀ 1001: the halves of two segments swapped):
compared are the skeletons (`bcat-analysis--skeleton',
`bcat-analysis--skeleton-diff'), shown is the text itself.  Editorial
brackets in SAMHITA ([cchilaṣṭaḥ]) do not count.  Words merely split or
joined wrongly (yasya āḥ, yā sā, yat-vakṣyati) change too few letters
to show."
  (when (and (stringp samhita) (stringp padapatha)
             (not (string-blank-p samhita)) (not (string-blank-p padapatha)))
    (let* ((sa (bcat-analysis--skeleton (replace-regexp-in-string "\\[[^]\n]*\\]" "" samhita)))
           (pa (bcat-analysis--skeleton padapatha))
           (diff (bcat-analysis--skeleton-diff (car sa) (car pa)))
           (text (lambda (sk runs)
                   (delete "„“"
                           (mapcar (lambda (r)
                                     (format "„%s“"
                                             (bcat-analysis--gap-text
                                              (substring (nth 2 sk) (aref (nth 1 sk) (car r))
                                                         (1+ (aref (nth 1 sk) (cdr r)))))))
                                   runs))))
           (missing (and diff (funcall text sa (car diff))))
           (extra (and diff (funcall text pa (cdr diff)))))
      (when (or missing extra)
        (concat "Padapāṭha weicht vom Text ab — "
                (string-join
                 (delq nil (list (and missing (concat "im Text, nicht im Padapāṭha: " (string-join missing ", ")))
                                 (and extra (concat "nur im Padapāṭha: " (string-join extra ", ")))))
                 " · "))))))

(defun bcat-analysis-with-padapatha-checks (context)
  "CONTEXT with each Sanskrit segment that has a padapāṭha and its text
\(`segments', `sanskrit-segments') checked: `padapatha-check' set to
what `bcat-analysis-padapatha-check' finds, or removed — the line under
⟦N⟧ comes and goes with the bold line (16.11.)."
  (mapcar (lambda (entry)
            (if (and (memq (car-safe entry) '(segments sanskrit-segments)) (listp (cdr entry)))
                (cons (car entry)
                      (mapcar (lambda (seg)
                                (if (and (consp seg) (consp (car seg))
                                         (alist-get 'padapatha seg) (alist-get 'samhita seg))
                                    (let ((c (bcat-analysis-padapatha-check (alist-get 'samhita seg)
                                                                            (alist-get 'padapatha seg)))
                                          (rest (assq-delete-all 'padapatha-check (copy-alist seg))))
                                      (if c (cons (cons 'padapatha-check c) rest) rest))
                                  seg))
                              (cdr entry)))
              entry))
          context))

(defun bcat-analysis-table-padapatha (samhita tables)
  "The padapāṭha line of a Sanskrit segment read back from its overview
TABLES — the words of their first row, then the daṇḍas closing the
transmitted line SAMHITA (`bcat-analysis-padapatha-line', PLAIN) —, for
the original in bold under ⟦N⟧ (Carsten 9.11., TĀ 1009) when C-c u L
sets a file that does not show it yet.  nil without tables or for
tables in Tibetan script."
  ;; 18.11.: only the words — not what stands under them in the table
  (let ((columns (mapcar #'car (bcat-analysis-band-columns tables))))
    (when (and columns (not (string-match-p "[\u0F00-\u0FFF]" (caar columns))))
      (bcat-analysis-padapatha-line
       (or samhita "")
       (mapcar (lambda (col) (list (cons 'word (bcat-analysis-plain-cell (car col))))) columns)
       t))))

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

(defun bcat-analysis-quote-lines (lines quotes)
  "LINES (IAST, the lines of a recitation) with the verses QUOTES
\((:lines ORIGINAL :verse METRICAL) …, `bcat-sentence--quoted-verses')
set in their metrical lines, with the pāda marks — wherever consecutive
LINES are exactly a quote's text (spaces, ¦ and | aside: the pāda mark
may stand inside a sandhi word, kiṃcin|na); the other lines stay.
16.11., TĀ 1012: a śloka quoted in Jayaratha's prose."
  (let ((bare (lambda (s) (replace-regexp-in-string "[ \t\n¦|]+" "" s))))
    (dolist (q quotes lines)
      (let ((target (funcall bare (string-join (plist-get q :lines) " ")))
            (n (length lines))
            (hit nil))
        (cl-loop for i from 0 below n until hit
                 do (cl-loop for j from i below n
                             for joined = (funcall bare (string-join (cl-subseq lines i (1+ j)) " "))
                             until (or hit (> (length joined) (length target)))
                             when (equal joined target) do (setq hit (cons i j))))
        (when hit
          (setq lines (append (cl-subseq lines 0 (car hit))
                              (plist-get q :verse)
                              (nthcdr (1+ (cdr hit)) lines))))))))

(defun bcat-analysis--prose-recitation (base parsed)
  "The recitation fields of a prose sentence from PARSED's `recitation'
\(lines of sense units, ¦ a breath pause): each line in Devanāgarī over
IAST, under a short header — when BASE is no verse and the lines are
the transmitted text, broken at its spaces; else nil.  A verse quoted
in the prose gets its metrical lines (`bcat-analysis-quote-lines',
16.11.).  31.10., TĀ 1008: prose had only the text as one long line."
  (let* ((lines (cl-remove-if-not #'stringp (alist-get 'recitation parsed)))
         (norm (lambda (s) (string-trim (replace-regexp-in-string
                                         "[ \t\n]+" " " (replace-regexp-in-string "¦" " " s)))))
         (text (mapconcat (lambda (seg) (or (alist-get 'samhita seg) ""))
                          (alist-get 'segments base) " ")))
    (when (and lines (not (alist-get 'verse base))
               (equal (funcall norm (string-join lines " ")) (funcall norm text)))
      (setq lines (bcat-analysis-quote-lines lines (alist-get 'quotes base)))
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
                    ;; 18.11.: the parts of the words under them, in the table
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
     (bcat-analysis-overview-fields base parsed refs)
     (bcat-analysis--prose-recitation base parsed)
     (bcat-analysis--second-translation parsed)
     ;; 18.11.: the points for the discussion marked at their segments
     `((segments . ,(bcat-analysis--with-discuss segments parsed refs base))
       (glosses . ,(and (cl-some (lambda (s) (alist-get 'glossed s)) segments) t))
       (translation . ,(bcat-analysis--field parsed 'translation))
       (grammar . ,(bcat-analysis--field parsed 'grammar))
       (comparison . ,(bcat-analysis-comparison parsed refs (alist-get 'target-lang base) base))
       (proposal . ,(bcat-analysis-comparison-proposal parsed (alist-get 'target-lang base)))
       (word-analysis . ,(bcat-analysis-word-analysis (alist-get 'segments parsed)))
       (bhasya-hint . ,(bcat-analysis--field parsed 'bhasya_hint))
       (bhasya-translation . ,(bcat-analysis--field parsed 'bhasya_translation)))
     ;; 18.11.: the translation closes * Text, the decisions stay
     (bcat-analysis-reading-fields (bcat-analysis-comparison-proposal parsed (alist-get 'target-lang base))
                                   (bcat-analysis--field parsed 'translation))
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
assignment — one per segment; under each its Wylie (18.11., Carsten:
„Originaltext in Script und Wylie“; 31.10. to 18.11. only where a
segment had no table yet)."
  (let* ((tibs-of (lambda (skt) (cl-loop for a in assignment when (eql (cdr a) skt) collect (car a))))
         ;; The mark stands whenever the segment shows something — so
         ;; differences after a segment without a table still have it.
         (shown (lambda (item)
                  (and (cl-some (lambda (k) (let ((v (alist-get k item)))
                                              (and (stringp v) (not (string-blank-p v)))))
                                '(gloss-table segment-translation differences))
                       t)))
         (uchen-of (lambda (n) (string-trim (or (alist-get 'uchen (cl-find n tibetan :key #'bcat-analysis--num)) ""))))
         (wylie-of (lambda (n) (string-trim (or (alist-get 'wylie (cl-find n tibetan :key #'bcat-analysis--num)) ""))))
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
                                          ;; 18.11.: the original in script and
                                          ;; transliteration — the Wylie always
                                          (and tibs `((line . ,(mapconcat uchen-of tibs " "))
                                                      (wylie . ,(string-trim
                                                                 (mapconcat wylie-of tibs " ")))))))
                                      sanskrit))
                  (mapcar (lambda (ts)
                            `((line . ,(funcall uchen-of (bcat-analysis--num ts)))
                              (wylie . ,(funcall wylie-of (bcat-analysis--num ts)))))
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
