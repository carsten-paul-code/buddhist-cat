# CLAUDE.md — Agent handoff for Buddhist CAT (buddhist-cat, formerly tibetan-cat.el)

This file briefs Claude Code (or any other Claude surface) picking up
work on **Buddhist CAT** (`~/buddhist-cat`, GitHub
`carsten-paul-code/buddhist-cat`; until 28.10.2026 *tibetan-cat.el* in
`~/tibetan-cat.el`, which is now a symlink), Carsten Paul's Emacs-Lisp
Computer-Assisted Translation (CAT) system for Classical Tibetan and
Sanskrit. Read it in full before editing. Symbols keep their
`tibetan-' prefix until their module is rebuilt; new and rebuilt
modules use `bcat-' (§5.73). Last updated 2026-10-28 (§5.74: SATZDATEIEN
NACH VORLAGE — Reading-Klassen, Tibetisch: `#+CAT_TEMPLATE: tibetan' im
Quellkopf, Gliederung * Text / * Segments / * Sentence, C-c u A = Rumpf
ohne KI, C-c u G = ein JSON-Abruf je Satz (+ DharmaMitra, + kleiner
Zuordnungs-Abruf für ergänzte Vergleichsübersetzungen), endet mit C-c u H
= die Satzdatei als PDF aufs reMarkable; Sanskrit seit 29.10. (Rezitation zuerst); * Text Tibetisch Uchen über Wylie, Sanskrit Rezitation in Devanāgarī + Text als IAST-Padapāṭha; ERT 2659.
Previous: §5.73: UMBENENNUNG IN
BUDDHIST CAT — erster Schritt: die Hülle (Hauptdatei/Feature
buddhist-cat, alter Name lädt weiter, Customize-Gruppe, Menü, Repo,
Verzeichnis, Carstens Configs); ERT 2597.
Previous: §5.72: PDF-EXPORT DER SATZDATEIEN —
steinert:-Links schützen `#'/`%' in \href (tabularx las sie als
Kommentar, C-c C-e l p brach ab); ⟦N⟧ via STIX Two Math in Carstens
Export-Init; ERT 2593.
Previous: §5.71: BEDEUTUNGSZEILE GEKAPPT — die
§5.67-Zeile sprengte bei langen Komposita die Tabellenbreite; ERT 2591.
Previous: §5.70: HÄNGER NACH C-c u G BEHOBEN —
Emacs stand bei 100 % CPU in `org-indent-initialize-agent', nicht
abbrechbar: die Landung schrieb die Satzdateien mit
`inhibit-modification-hooks' t, der org-element-Cache sah nichts, org
9.7 parst in `org-at-item-p' je Zeile; jetzt landen beide Writer mit
laufenden Hooks; Diagnoseweg (sample, Timer-Protokoll) dokumentiert;
ERT 2587.
Previous: §5.69: VOKABULAR IM DISKUSSIONSPAPIER — Rubrik „Vokabular"
zwischen Text und Übersetzung (★ zuerst), rmapi-Upload-Limit je
Sitzung, kein Auto-Papier für Test-Fixtures; ERT 2586.
Previous: §5.68: SYNOPSE-VORSPANN IM HANDOUT —
nach dem Vorbereitungsblatt der Doktoranden: `* Siglen', je Section
`* Synopse' im Querformat mit Stellenzeile aus :FOLIO:, Skt|Tib-
Tabelle (Vers fett) und Kommentar (Sthiramati) mit Versworten fett /
Bhāṣya-Worten unterstrichen (Silbenheuristik, Goldstandard = das
Blatt); Keywords #+TIBETAN_SYNOPSIS/COMMENTARY/SIGLA/SIGLUM; kein
Abruf; ERT 2585.
Previous: §5.67: BEDEUTUNGEN ALS 3. ZEILE —
unter der Wortbildung stehen die Bedeutungen aller Teile (vi- apart ·
√muc free · -ta past participle), drittes Feld derselben
Begleitantwort, Zielsprache des Dokuments; ERT 2579.
Previous: §5.66: WORTBILDUNG ALS 2. ZEILE —
Sanskrit-Glossentabellen tragen Präfix + √Wurzel + Suffix je Wort
(Claude-Begleitabruf zu C-c u G, Zettel-Wortbildung als Vorgabe,
`** Word Formation'); Bugfix Formlabel am ersten Semikolon; ERT 2579.
Previous: §5.65: METRUM + REZITATIONSPAUSEN —
Sanskrit-Skansion, Metrum-Erkennung (21 Sama-Metren, Upajāti,
Puṣpitāgrā, Vaitālīya/Aupacchandasika, Śloka, Āryā-Familie) und
Pausen ` ¦ ' / ` | ' als `** Metre' vorn in * Reading, im Handout und
im reMarkable-Papier; TĀ 1: 344 von 348 Versen erkannt; ERT 2569.
Previous: §5.63: ZETTEL-HERVORHEBUNG — jedes
Wort mit Zettel in Quelltext und Analyse-Dateien unterstrichen und
verlinkt (RET/Klick → Research-Instanz); Index `skos_index.py`, keine
Stamm-Raterei: Analyse über die Word Analysis, Quelltext als Projektion
des Satz-Padapāṭha; `C-c u z h`/`H`; ERT 2521.  Previous:
§5.62: ÜBERSETZUNGS-DISKUSSION — je
Kaskaden-Satz `** Discussion' am Ende von * Provided Translations:
Einschätzung der Referenzübersetzung(en), Referenz mit vom Tool selbst
markierten Passagen (⟨N⟩), nummerierte Seminar-Punkte gegen Claude +
DharmaMitra; automatisch nach C-c u G, auf Abruf C-c u c; im Handout
je Satz; ERT 2505.  Previous: §5.61: SANSKRIT-ANALYSE — sa-Satzdateien
tragen `* Sanskrit Analysis', ausführliche Grammatik je Segment (Sandhi /
Nominal- / Verbalformen / Komposita / Syntax), eigenes Antwortlimit
16000 statt gptels 4096 (wahrscheinliche Ursache der sent-654-
Abschneidung), Regenerate erhält #+CREATED; ERT 2489.  Previous:
§5.60: KI NUR AUF ABRUF — C-c u A
legt an/öffnet überall ohne Fire (Auto-Fire-Schalter Default nil, auch
Zwei-Datei-Öffner), C-c u G holt Claude + DharmaMitra; Vers-Referenzen
#+TIBETAN_SENTENCE_REFS (Dyczkowski für das Tantrāloka) im Rumpf +
Prompt; Tantrāloka: 332 Rümpfe; ERT 2483.  Previous: §5.59: C-c u A/R AUF JEDER EBENE
der Kaskaden-Quelle (Section/§/`* Tibetan Text' analysieren bzw.
rendern den org-Subtree — vorher Absatz-Fehlweg) + LESE-HANDOUT
C-c u H (Wylie + Uchen, Übersetzung, Vokabular ★-zuerst, Grammatik
gefiltert → org → LuaLaTeX-PDF); Bugfix org-Wortliste verschluckte
die erste Tabellenzeile (`bden pa gnyis'); ERT 2470 / BDD 251.
Previous: §5.58: READING-CLASS-STRUKTUR —
Kaskaden-Satzdateien umgebaut für die 4 Reading-Klassen WS26:
* Translation auf L1, Reading = nur Gloss Tables (⟦N⟧-Zeile unter
der Segment-Tabelle, steinert:-Links in Zeile 1, Converb-Funktion
in Zeile 3), * Tibetan Analysis flach, * Provided Translations L1
mit DM + materialisierten Lopez/W&M; 6 Bugfixes vorneweg (keep-l2-
fboundp B0, sa-Grammar-Verlust, Gate-Falle nach Chunk-Landung,
Truncation-Stub, དེ/པས-Homographen, Steinert-downcase); Migration =
Regenerate (migrate-structure-v2, render-only); ERT 2440 / BDD 251.
Previous: §5.57: SANSKRIT im Kaskaden-
Layout — `#+SOURCE_LANG: sa`, Importer `C-c o k`, Padapāṭha-
Kontrakt (`** Word Analysis`), Regenerate-after-Land (auch bo),
Devanagari→IAST, DM iast/german; 4 Bugfixes vorneweg (u.a. C-c s R
Kaskaden-Zerstörung, B0-Klasse); ERT 2401 / BDD 251.  Previous:
§5.56: §15-Review — Ein-Raster-
Glossentabellen: hline-Bänder auf gemeinsamem Spaltenraster statt
Leerzeilen-Chunks, Zeile-2-Zellen-Budget 25 (…), Escape-Strip;
ERT 2345 / BDD 250.  §5.55: §185-Review — Sentence
Structure tabellarisch je Einheit, Segment-Headings + Breiten-
Chunking der Glossentabellen, par: Tabelle vor Interlinear /
Reference Translations unten / DM-Fire; Batch-Wedge-Bugfix;
ERT 2339 / BDD 250.  §5.54: Masterarbeit-Drei-Sichten —
`** Gloss Tables` als erstes Reading-Kind mit Edit-Schutz
(generiert-bis-angefasst, :GENERATED_HASH:), Übersetzungs-Stitcher +
§-Ansicht-Generator in `persist/tibetan-translation-doc.el`
(Copyright-Lock testverriegelt), Navigation C-c u j/n/p; ERT 2322 /
BDD 250.  Previous same day: §5.53: batch-vs-interactive
sense-selection drift root-caused — steinert now above raw RY in
`tibetan-dictionary-priority`, bundled RY store authoritative over
the init-defined combined store.  §5.52: Phonetics retired, the
§184-handout `** Gloss Table` ships in every analysis file, the
Claude-gloss tier now beats dictionary first-senses across the
two-file, paragraph AND cascade render paths, B0 cascade-guard
postmortem.  ERT 2287 / BDD 249.  Corpus landing per the §5.52
recipe is the next operational step.  Previous: §5.46 C0 = Part B
Phase 0+1; §5.26 BUG fix — regenerate-
auto preserves Claude content under `:missing-only' too;
`(not re-request-claude)' was treating `:missing-only' as
truthy → wipe.  Caused 274 / 287 Milarepa segs to lose Claude
content on a 2026-05-24 batch run.  Fix:  preserve unless
`re-request-claude' is the symbol `t'.  267 files restored
from git blob `bc5cbe9' (the buddhist-studies/ folder is git-
tracked).  Regression test locked in.  Previous: §5.25 added:
thesaurus zettel
cross-link in Concept Notes — Claude cites `[[id:ZID][zettel ↗]]'
inline for any term that has a curated zettel in
`~/Documents/tibetan-thesaurus/';  writer post-processes the
response as a safety net.  §5.24 added: `** Concept
Notes' replaces `*** Claude Context' — class-reading aid that
surfaces Buddhist + Sanskrit technical concepts with encyclopedia
notes instead of the old narrative-arc commentary.  Heading
promoted from L3 to L2 (segment + sentence).  Migration
automatic on first regenerate.  Previous:  §5.23 added: per-source
filename suffix `seg-NNN-SHORT.org' for ALL new analysis files;
fixes silent multi-source overwrite in MA Reading folder.
Migration helper `tibetan-analysis-migrate-suffix-in-folder'
renames existing files based on each file's `#+SOURCE:' link.
Ran on Yogācārabhūmi (96) + MA Reading (39); created + Claude-
filled 23 previously-overwritten MA Reading segs.  Previous:
§5.22 final: sentence-analysis
files unconditionally render the in-class compressed layout —
Vocabulary + Translation + Grammar (with the converb-rich
`*** Particles' section) + Provided Translations only;  7
reference sections stripped (Wylie / Phonetics / Interlinear /
DharmaMitra / Sentence Structure / Verb Classification /
Detailed Dictionary).  The opt-in machinery from §5.22 initial
(`#+TIBETAN_SENTENCE_COMPRESSED:' header + `C-c u z C' toggle +
dynamic var + metadata-reader key) is RETIRED.  Per-segment
seg-NNN.org keeps the full §5.21 layout — clean split of class-
reading vs deep-analysis surfaces.  Also: `#+OPTIONS: toc:nil
num:nil' on all analysis-file scaffolds.  Previous: §5.21 Mid-
class Cleanup (CAT Gloss retired, Claude Vocabulary promoted
to level-2, Particle Map + Particles merged with Bialek 2022
refs, Detailed Dictionary moved to bottom, Provided
Translations user-content); §5.20 DM-Tibetan nested; §5.19
Phonetics; §5.18 Sanskrit-first layout revision; §5.17 two-
language-parallel architecture; §5.10–§5.13 dictionary polish,
grammar unification, thesaurus + target-language pipeline,
three-level dispatch).

Companion files worth reading alongside this one:
- `~/.claude/projects/-Users-cp/memory/working_discipline.md` — the
  project-agnostic baseline extract of §2.1–§2.5 (applies to every
  Carsten repo).  This file refines but does not override them.
- `~/.claude/projects/-Users-cp/memory/project_tibetan_cat.md` — the
  one-page pointer from auto-memory; older / less detailed than this
  doc.  If the two disagree, believe this one.

---

## 1. Who you're working with

- **Carsten**, researcher in Buddhist studies. Works primarily on
  Classical Tibetan sources (Bhutan-Kagyu Madhyamaka context, Milarepa
  material, verse philology).
- He is the tool's author and only current user; the system is being
  prepared for a workshop presentation.
- Project order he is following: **refactor → extend → document →
  presentation.** We are currently in the "extend" phase with a few
  targeted items still open. Documentation and presentation come
  **after** the code is where he wants it. Do not start on docs or
  slides unless he explicitly asks.

## 2. How Carsten likes to work

These are hard preferences. Treat them as rules, not suggestions.

1. **Bug fixing always first.** If a bug surfaces during any session
   — live review of an analysis file, a regression spotted mid-
   feature, a failing ERT test, a warning Claude notices in passing —
   fix the bug BEFORE continuing the current feature work. Don't
   defer with a "I'll flag it for later"; the correct default is to
   pause the feature, fix the bug (tests + commit), then resume.
   Only defer when (a) the bug is clearly out of scope for this
   session AND (b) you explicitly ask Carsten and he says OK.
2. **Test first — always.** The workflow has three flavours, and
   tests come BEFORE the code in each:

   - **BDD for specifications.** When Carsten asks for a new
     behaviour, write a failing ERT test that encodes the acceptance
     criteria (inputs → expected output / state change) BEFORE any
     implementation.  The test is the spec; the code is what makes
     the spec pass.  Prefer one test per observable behaviour, named
     after the behaviour not the function.

   - **TDD for functions.** New helper / new branch / new edge case
     → failing test first, minimal implementation to pass, refactor
     with tests green.

   - **Regression-test-first for bugs.** When a bug surfaces:
     1. Reproduce it — isolate the minimum input that triggers it.
     2. Write a failing test (or several) that nails it down: the
        exact misbehaviour, plus adjacent cases the fix must not
        break.  Check the tests fail on the current code for the
        reason you expect — if they pass or fail in a different way,
        your understanding of the bug is wrong and you need to go
        back to step 1.
     3. Fix the code.  All your new tests go green; the suite stays
        green.
     4. Only now do manual verification.  The tests guarantee the
        specific bug class can't come back silently — manual testing
        just confirms UX polish.

     Do NOT fix a bug by writing the patch first and a test
     "afterwards to lock it in"; that path produces tests that pass
     by construction and don't actually guard against the bug
     re-appearing via a different code path.

   Stub external dependencies (SQLite, network, glossary files) so
   tests run in batch without side effects.  Keep the suite green
   after every commit — `make test` is the baseline gate.
3. **Verify failure mode before implementing.** After writing a
   regression test (rule 2), RUN it and confirm it fails for the
   reason you expect — the specific bug you're targeting, not a
   typo, missing import, wrong assertion, or unrelated environment
   issue.  If the test passes or fails in an unexpected way, your
   understanding of the bug is wrong; go back to reproduce.

   Before moving to the fix, include the failing test output in
   your report to Carsten.  He reads it to confirm you're fixing
   the right thing.  This is the discipline that turns "tests that
   pass by construction" into "tests that cannot pass without the
   bug being gone".
4. **One logical change per commit.** Each commit is the minimum
   reviewable unit.  If a session adds a feature AND fixes an
   adjacent bug AND refactors unrelated code, that is three
   commits, not one.  Default to splitting; only combine if
   Carsten explicitly asks.  The rationale is both `git bisect`
   hygiene (a future bisect lands on a single change) and review
   cost (a reviewer can accept/reject one piece without rejecting
   the whole).

   Corollary: never squash a green interim commit into a
   follow-up refinement after the fact — the interim state is
   itself a useful reference point.
5. **Commit messages document the WHY with concrete examples.**
   Subject line: imperative mood, ~60 chars, names the change
   (`thesaurus: add gap report for German prep`).  Body structure:

   - Problem / observation — what went wrong or what user need
     surfaced; include the concrete reproducer or a quote from
     a live review when available.
   - Root cause — the INSIGHT, not just the symptom.  Why did
     this bug exist / this feature matter?
   - Fix — what the code now does differently, at the level of
     mechanism (not just "added function X").
   - Tests — names of new / modified tests and what they guard
     against.  Report the suite totals after the change.

   Reference earlier commits by short hash when relevant (e.g.
   "extends commit 259d00b's Hill-DB guard pattern").  A future
   session reading `git log` should understand not only WHAT
   changed but the REASONING — the project has several polish
   bugs whose fix is one line but whose explanation is three
   paragraphs.  Preserve the explanation.
6. **Preserve user content.** Any operation that regenerates analysis
   files must preserve `* My Notes`, `* Working Translation`,
   `* Footnotes`, and an existing `*** Claude` translation. If you
   touch the regenerate / batch-reanalyze path, add a test that
   asserts preservation.
7. **Persistent analysis is the canonical workflow.** He uses `C-c u A`
   (persistent analysis file per segment) — not `C-c u i` (classroom /
   scratch view). Design features around the persistent-file flow
   first; the classroom view is secondary.
8. **Don't silently change behaviour.** If a fix alters what users see
   in their analysis files, say so and ask before rewriting files he
   has invested hours annotating.
9. **German and English side by side.** Many glosses are bilingual
   (`DE // EN`). Don't collapse this — the `tibetan-analysis--format-bilingual-gloss`
   helper exists for a reason. Resources/Custom entries are hand-written
   and must never be truncated or rewritten.  Pass 5c adds a per-
   document `#+TIBETAN_TARGET_LANG:` header that determines which
   half the Interlinear / CAT Gloss / Claude Translation show;
   the bilingual pair stays in the Detailed Dictionary regardless.
10. **Aim close to perfect.** He is not looking for "ship it" output;
    he wants careful, correct code. If a solution is 80% there, say so
    and propose the remaining work rather than declaring it done.
11. **Explain what you're about to do before doing it** for anything
    that touches more than one module, then run the tests after.
12. **New reading source files follow the Section → Sentence → Segment
    layout.** Every new `.org` source document for the CAT pipeline
    (MA Readings Tibetan, Tibetisch IV, any future reading corpus)
    uses this nested structure:

    ```
    * Tibetan Text
    ** Section N. <label>        ← optional; only for editorial
                                    divisions (e.g. the three-persons
                                    [I]/[II]/[III] in a lam-rim;
                                    stanza groups; opening/body/colophon)
    *** Sentence N               ← one per prose paragraph or verse
                                    stanza; global numbering from 1
    **** Segment M               ← shad-bounded clause; global
                                    numbering from 1
    :PROPERTIES:
    :FOLIO:    <ref>             ← only when folio refs matter
    :END:

    <Tibetan Unicode>

    **** Working Translation     ← sibling of Segment; empty
                                    placeholder at creation time
    ```

    Reference implementation:
    `/Users/cp/buddhist-studies/SS26/Tibetisch IV/work in progress/Milarepa-prepared.org`
    (the corpus is a plain local tree since the 2026-07-28
    iCloud→local migration; git/GitHub is the sync + backup channel).
    The sentence-aware commands (`C-c u B` auto-analyze-document,
    `C-c s N` sentence-create-all, `C-c s Z` resegment) expect this
    hierarchy — flat `** Section → *** Segment` layouts (older Gal
    chen nyi shu style) miss the Sentence level and don't feed
    `sent-*.org` creation cleanly.  Do NOT regenerate older flat
    files unless Carsten explicitly asks.

## 3. Repository shape

```
buddhist-cat/      (formerly tibetan-cat.el/; ~/tibetan-cat.el is a symlink)
├── core/         primitives: wylie, vocabulary, verb-classifier,
│                 steinert (SQLite), doc-display, etc.
├── analysis/     segment / compound / clause / round1+round2 analysis
├── persist/      tibetan-analysis-persist.el — the persistent
│                 analysis-file machinery (generate, regenerate, batch)
├── workspace/    sentence-workspace, verse-workspace buffers
├── philology/    madhyamaka-terms, verse-philology, particles
├── doc-prep/     OCR runner, doc-format, ocr-correct
├── config/       tibetan-keybindings.el, tibetan-menu.el
├── test/         ERT test suite, run-all-tests.el is the entry point
├── data/         dictionaries/steinert.db (SQLite, built via make)
├── Resources/    per-document vocabulary lists (user-curated)
├── Makefile      `make test` = ERT (test-quick) + BDD (test-bdd)
├── buddhist-cat.el top-level loader (feature buddhist-cat + tibetan-cat)
└── tibetan-cat.el  old name: loads buddhist-cat.el from its own directory
```

Key invariants:

- All source files start with `;;; foo.el --- … -*- lexical-binding: t -*-`.
- Every module provides itself: `(provide 'foo)` at the bottom.
- Soft-requires use `(require 'x nil t)` so an optional module missing
  does not break loading.
- Test files live in `test/` and get added by `(condition-case nil
  (require 'foo-test) (error nil))` in `run-all-tests.el`.

## 4. Running the tests

The project has TWO suites:

- **ERT** — the unit / regression tests in `test/`.  This is the
  primary gate.
  ```sh
  cd test
  emacs -batch -l run-all-tests.el -f ert-run-tests-batch-and-exit 2>&1 | tail -25
  ```
  Or `make test-quick` from the project root.
- **BDD** — the behaviour specs in `spec/`.  `make test-bdd`, or
  `./run-specs.sh`.

`make test` runs **both** (`test-quick` then `test-bdd`) and stops on
the first failure — this is the full batch suite.  (Before 2026-06-01
`make test` ran only the BDD specs and its `-f tibetan-bdd-run-all-specs`
flag named a non-existent function that never executed; the spec suite
also made a live dharmamitra.org request on every run.  Both fixed.)

Current state (2026-06-11, post-§5.42 span highlighting):  **ERT
2206 tests, 2205 expected, 0 unexpected, 1 intentional skip
(compound-analysis-callable); BDD 244 / 244.**  Full `make compile` is
clean (zero warnings).  Carsten runs `make test` after every change and
expects it to stay green.

When adding a test, always wire it into `test/run-all-tests.el` via
`condition-case`. Otherwise the suite silently doesn't pick it up and
you'll think your tests passed when they didn't run.

## 5. What was done in the last block of work

This is the recent history. Read it so you don't re-do finished work.

### 5.1 Round-2 clause analysis (done, tested)
- **New module** `analysis/tibetan-clause-segmenter.el` with three
  composable public entry points:
  - `tibetan-clause-segment (words verbs &optional _mwu)` —
    position-indexed clause segmentation on converb / final-particle /
    verb-end boundaries.
  - `tibetan-np-chunk (words &optional clauses mwu)` — NP chunking
    with case-tagging (ERG/GEN/DAT/LOC/TERM/ABL/COM).
  - `tibetan-build-argument-structure (clauses nps)` — assigns subject
    / agent / object / oblique roles using verb case-frames from the
    Hill 2010 classifier.
  - `tibetan-analyze-round2` — convenience wrapper that calls all three.
- Tests: `test/tibetan-round2-clause-segmenter-test.el` (11 tests,
  including an end-to-end seg-012 integration test).
- **Not yet wired** into `tibetan-analysis-generate-content` as a
  `** Clause Structure` section in the analysis file — see §6.

### 5.2 Batch reanalysis (done, tested)
- Added to `persist/tibetan-analysis-persist.el`:
  - `tibetan-analysis-reanalyze-file (filepath &key source-file
    re-request-claude dry-run)` — regenerates one analysis file while
    preserving all user sections AND an existing non-placeholder
    `*** Claude` translation.
  - `tibetan-analysis-batch-reanalyze (&key folder source-file
    re-request-claude dry-run)` — iterates every `seg-NNN*.org` in a
    folder, in seg-id order.
  - Helpers: `--read-section-body`, `--read-claude-translation`,
    `--restore-claude-translation`, `--seg-id-from-filename`,
    `--folder-analysis-files`.
- Keybinding: `C-c u r` (lowercase r — distinct from `C-c u R` which
  is single-segment reanalyze and `C-c u B` which is "analyze all
  segments, create missing files").
- Menu entry: `Tibetan → Persistent Analysis → Batch Re-analyze Folder`.
- Tests: `test/tibetan-batch-reanalyze-test.el` (6 tests covering
  notes preservation, Claude preservation, placeholder-not-persisted,
  dry-run, missing seg-id, folder iteration).

### 5.3 Detailed Dictionary multi-source rendering (done, eyeball-verified on seg-4)
- **Problem observed:** verbose polysemous words like ཡུལ produced a
  huge block from whichever single source `tibetan-vocab-lookup-detailed`
  picked. Also, Sanskrit was being synthesised from Sanskrit-indexed
  tables even when the entry itself didn't carry it.
- **New rule** agreed with Carsten ("rule A"):
  1. Provided vocabulary list (Resources) entry appears first when present.
  2. Steinert aggregated block (top hits, deduplicated internally).
  3. Rangjung Yeshe standalone.
  4. Bundled / DharmaMitra *only* if they add a genuinely different gloss.
  5. Sanskrit surfaced **only** when the source entry natively carries it.
- **Implementation:**
  - `core/tibetan-vocabulary-detailed.el` gained
    `tibetan-vocab--dedup-fingerprint` (normalised gloss fingerprint)
    and `tibetan-vocab-multisource-entries (word)` which returns the
    ordered list of per-source plists.
  - `persist/tibetan-analysis-persist.el` — the `** Detailed Dictionary`
    renderer (around line 1834) now walks that multi-source list.
  - Tests: `test/tibetan-vocab-multisource-test.el` (8 tests with fully
    stubbed sources).
- **Verified on seg-4 (2026-04-15)**: cap tuned from 8 → 5 via the
  `tibetan-vocab--steinert-cap' defconst.  Knobs for further tuning:
  the cap defconst itself, and the 80-char fingerprint length in
  `tibetan-vocab--dedup-fingerprint'.

### 5.4 Display consistency + Bialek terminology (done, verified)
Two cross-cutting cleanups:
- **`SCRIPT [wylie]` everywhere** except the top-level `* Tibetan
  Text` section.  One helper `tibetan-analysis--format-word-with-wylie'
  in `persist/tibetan-analysis-persist.el' is the single source of
  truth; six callers (Word/Particle List, Grammatical Markers,
  Sentence Structure, Verb Classification lemma + STEMS, Detailed
  Dictionary head) route through it.  Particle Map stays wylie-only
  by design.
- **Bialek terminology unified**.  Both the Word/Particle List and
  the Grammatical Markers section now route through
  `tibetan-analyze-grammar-bialek' via the `tibetan-analysis--bialek-type'
  helper.  The legacy hardcoded role table in
  `tibetan-analysis--get-grammatical-role' was deleted; the verb
  suffix labeller (`tibetan-analysis--detect-verb-suffix') now
  delegates to the same helper.  The legacy Sentence Structure
  argument analyser (`tibetan-analyze-arguments' in
  `analysis/tibetan-enhanced-display.el') was also routed through
  the segmenter's case-of helper so single-char ས/ལ/ར/ན don't get
  mistaken for case particles.

### 5.5 Round-2 wired into analysis output, with chunker fixes (done)
- `** Clause Structure' section (gated by
  `tibetan-analysis-show-clause-structure', default `t').  Renderer
  `tibetan-analysis--render-clause-structure' uses `tibetan-analyze-round2'.
- `tibetan-clause-segmenter' is now hard-required from
  `tibetan-cat.el' (and soft-required from the persist module) so
  the renderer always sees `tibetan-analyze-round2' at runtime.
- Three Round-2 chunker bugs fixed:
  1. **Single-char case particles standalone-only** (ས/ལ/ར/ན as
     final consonants no longer mis-stripped).  New helper
     `tibetan-clause-seg--multichar-suffix-p' applied to
     `--classify-word', `--case-of', `--case-strip'.  Also added
     `("ལས" . ABL)' to `--case-particles' (was missing — already
     present in bialek).
  2. **Content-word gluing** for compound proper nouns.  Plus a
     compound-`Xར' terminative rule in the NP chunker that mirrors
     bialek's seg-4 fallback.
  3. **Nominalised verbs filtered out of clause segmentation**.  New
     `tibetan-clause-seg--nominaliser-suffixes' defconst.  When a
     verb is followed by a bare nominaliser (`པ' / `པའི' / etc., but
     NOT `པས'/`བས' which are causal converbs), the verb does not
     form a clause; the last surviving clause's end is extended to
     cover any uncovered tail.  Falls back to the unfiltered list
     if filtering would produce zero clauses.
- MWU spans truncate at verb positions (Resources-supplied MWU like
  `ཡུལ་འཐོན' no longer absorbs the verb into the NP head).

### 5.9 Tokenization, section reorder, robust fallback (done, tested, 2026-04-21)

Three related fixes in response to user review of a live Milarepa
segment (seg-026 `བདག་ལ་སྟོད་ནས་འོངས།`) and a Yogācārabhūmi
class-prep QA sweep:

1. **Particle-tail / particle-head MWU rejection** (`core/tibetan-vocabulary.el`
   + `core/tibetan-vocabulary-detailed.el`, commit `29da547`):
   The two greedy 4/3/2-syllable compound loops that feed the
   Word/Particle List, Interlinear Gloss, and Detailed Dictionary
   were picking up real dictionary idioms like `བདག་ལ` (Skt. `naḥ`
   "to me") and `སྟོད་ནས` (Skt. `uparimāt` "from above") as single
   lexical units, hiding the grammatical `stem + PARTICLE` split
   that Particle Map / Clause Structure / Grammatical Markers
   otherwise render correctly.

   Fix: reject any compound whose head OR tail syllable is in
   `tibetan-extract-vocab--particle-tails` (the same list the MWU
   parser already used via `tibetan-enhanced-parser--case-particle-
   tail-p`).  Nominaliser tails (`པ` / `བ` / `མ`) are explicitly
   EXCLUDED so `བསྐལ་པ` (kalpa) etc. remain intact.  +3 ERT tests.

2. **Section reorder: Claude Translation + Grammar before Verb
   Classification** (`persist/tibetan-analysis-persist.el`, commit
   `ad7dd6c`).  New priority order in
   `tibetan-analysis--priority-section-order`:

       ** Wylie Transliteration       — sounded reading
       ** Particle Map
       ** Interlinear Gloss           — word-for-word trot
       ** Claude Translation          ← was position 5, now 4
       ** Claude Grammar              ← was position 6, now 5
       ** Verb Classification (Hill 2010)  ← was 4, now 6
       (… Word/Particle List, Clause Structure, Detailed Dictionary …)

   Reader flow: sound → particle-annotated skeleton → word-for-word
   trot → fluent translation + grammar explanation → parser-side
   reference tables.  Applies uniformly to seg- and sent-*.org via
   the shared renderer + reorder post-pass.  +1 ERT test on the
   Claude<VerbClass ordering invariant; existing priority-order
   test updated.

3. **Richer error fallback** (`persist/tibetan-analysis-persist.el`,
   commit `aa60337`).  When `tibetan-analysis-generate-content`'s
   outer condition-case fires, the fallback template used to emit
   only three `[Error]` stubs, wiping a file that had working
   Wylie + Claude content.  New fallback still emits:
     - `** Wylie Transliteration` with real Wylie (safe path)
     - `** Claude Translation [Requesting translation...]` placeholder
     - `** Claude Grammar` empty but present
     - `** [Analysis error — partial file only]` with diagnostic
   So a parser failure on ONE section no longer destroys the whole
   file — student sees Tibetan + Wylie + full Claude content with
   a visible marker flagging the degraded parser output.

   Used to recover YBh seg-030..034 on 2026-04-21: a parser corner-
   case on `gyur pa'o` (nominaliser + declarative particle) made
   `cl-remove-if-not` receive word-strings where verb-alists were
   expected.  Underlying bug not yet root-caused — see §6 P5.

Full suite at end of 2026-04-21: **1337 / 1334 expected / 0
unexpected / 3 intentional skips.**

### 5.10 Condensed Detailed Dictionary + ★-Resources marker (done, 2026-04-21/22)

Four commits refining the Detailed Dictionary block that ships under
every STEM in the analysis file.  Motivation: the multi-source block
from §5.3 was correct but visually dense, and it was hard to tell at
a glance which entries came from Carsten's hand-curated Resources
list versus auto-sourced dictionaries.

1. **Slimmer Detailed Dictionary — Pass B** (`e56dd7d`).  Added a
   per-source cap, a Sanskrit filter (synthesised Skt from reverse-
   indexed tables no longer surfaces when the source entry doesn't
   carry it), and a particle exemption so case-particle entries
   don't get truncated like content words.
2. **Condensed dictionary foundation — Pass 5a** (`8f3d40b`).
   New ranker with anchored source ordering (Resources-first,
   Steinert, then the long tail) + first-sense bracket-aware
   extraction that keeps `(1) [accusative, …]` intact instead of
   chopping at the first comma.
3. **Detailed Dictionary renders under the STEM — Pass 5a.1**
   (`1a4e48e`).  The DD block now appears nested under the
   Word/Particle List STEM heading instead of as a sibling —
   reader's eye stays on the stem while scanning senses.
4. **★ marks Resources / Custom / Thesaurus entries in Interlinear**
   (`4d2792d`).  Curated sources get a ★ prefix in the word-for-word
   gloss so at a glance Carsten can see "this is my own gloss, not
   an auto-lookup".  Same commit removed the now-redundant
   `** Word/Particle List` top-level section (the info lives inside
   Interlinear and Detailed Dictionary).

### 5.11 Grammar section unification (done, 2026-04-22)

The analysis file used to carry four overlapping grammar-ish
sections: `** Particle Map`, `** Claude Grammar`, `** Particle
Overview`, `** Grammatical Markers`.  Carsten called this "a lot of
redundancy" on live review.  Result is a single `** Grammar`
section with a Particle Map subsection and per-particle Portfolio-
cross-referenced entries.

1. **Pass 6b — merge into `** Grammar`** (`0de3e31`).  Single
   top-level section replaces the four.  Particle Map is now a
   nested `*** Particle Map` rendering the stem+particle skeleton;
   the bialek/Claude per-particle analysis follows as numbered
   entries.
2. **Pass 6c — per-particle function IDs + self-contained Portfolio
   snippets** (`8b30377`).  Each particle entry carries an id like
   `[[id:XYZ][Portfolio §1.6 ↗]]` linking into Carsten's Bialek
   Portfolio zettel, AND inlines the relevant Portfolio paragraph
   in the analysis file so a reader without access to the Portfolio
   can still follow the grammar.  Snippet body indented via
   `(replace-regexp-in-string "\n" "\n    " raw)` so the multi-line
   body doesn't leak as a sibling bullet.
3. **Dynamic Portfolio section-number injection into Claude prompt**
   (`0636665`).  Claude's `*** Claude Particles` output references
   Portfolio section numbers (`§1.6 Terminative`).  Previously
   Claude used its own guessed numbering (e.g. §1.5), which drifted
   from Carsten's actual Portfolio (§1.6).  Fix: the system prompt
   now includes `tibetan-interlinear-portfolio-reference-block` —
   a dynamic list of the real section numbers parsed from the
   Portfolio zettel.
4. **Particle-map presentation polish** (four commits):
   - Apply face via font-lock bypassing org emphasis word-boundary
     rule (`fa671e2`) — `'i` now renders magenta even when flanked
     by `=` delimiters where org won't parse emphasis.
   - Hide `=` / `~` delimiters, keep only the particle visible
     (`743535a`) — invisible text property + namespaced
     `tibetan-analysis-particle-marker` in buffer-invisibility-spec.
   - Lowercase particle tokens inside markup (`94652dc`).
   - Multi-tuple matching — one bialek entry, multiple Claude tuples
     per occurrence, dedup by sub-id, annotate with context word
     (`4b122ba`).  `r` ↔ `ra` Wylie normalisation via
     `--particle-wylie-normalise` so bare `ར` matches Claude's `r`.
5. **Auto-regen Grammar when Claude Particles arrives** (`ca2e6c5`).
   `insert-claude-sections` now calls `reanalyze-file` with
   `:re-request-claude nil` when Particles are present, so the
   Grammar section rebuilds with Claude data without re-firing
   the Claude translation request.

### 5.12 Thesaurus + target-language pipeline (done, 2026-04-22)

User-editable multilingual glossary layered on top of the dictionary
stack, plus per-document target-language selection and a gap-report
designed to prep a source file for translation (currently the German
Milarepa workflow).  This is the biggest single feature since the
Round-2 clause segmenter.

**New module** `core/tibetan-thesaurus.el`:

- File-per-term org zettels in `~/Documents/tibetan-thesaurus/` with
  `:ID:` property + `:wylie:` + `:script:` + bilingual
  `:primary:` / `:detailed:` fields.  Each zettel carries back-links
  to the analysis segments that use the term.
- Rank-1 injection into `tibetan-vocab-multisource-entries` (above
  Resources).  Overrides `:primary`/`:detailed` with the full
  bilingual `DE // EN` string so the parser's first-sense extraction
  doesn't truncate at the bilingual separator.
- Sliding-window tokenised Wylie matching via
  `tibetan-thesaurus--tokenise-wylie-text` +
  `tibetan-thesaurus--count-mentions` (replaced a regex-with-
  boundary-consumption approach that miscounted consecutive
  occurrences — `bdag bdag bdag` scored 2 instead of 3).

**Commands** under `C-c u z` prefix:

| binding    | command                                   |
|------------|-------------------------------------------|
| `C-c u z e`| `tibetan-thesaurus-edit-at-point`         |
| `C-c u z n`| `tibetan-thesaurus-new-entry-interactively` |
| `C-c u z r`| `tibetan-thesaurus-reload`                |
| `C-c u z i`| `tibetan-thesaurus-initialize-from-kramer`|
| `C-c u z a`| `tibetan-thesaurus-audit-folder-display`  |
| `C-c u z R`| `tibetan-thesaurus-rerun-affected-by-zettel` |
| `C-c u z L`| `tibetan-analysis-set-source-target-lang` |
| `C-c u z g`| `tibetan-thesaurus-translation-gaps-display` |

**Pass 5b** (`7dc80d6`) — user-editable glossary at rank 1; new zettels
take precedence over all dictionary sources in the renderer.

**Pass 5b.2** (`f6ddf48`) — create/edit entries from analysis buffer.
`--wylie-at-point` recognises the word under cursor in `** Detailed
Dictionary`; the new-entry command pre-fills script + wylie from the
analysis context.  `case-fold-search` must be `nil` inside the wylie
detector — default case-insensitive matching caused `[A-Z]+` to
match lowercase Wylie and filter real words out.

**Pass 5c** (`ec5b1a9`) — per-document DE/EN target-language picker.
`#+TIBETAN_TARGET_LANG:` header added to source files via
`tibetan-analysis-set-source-target-lang` (`C-c u z L`).  Value is
threaded through `tibetan-analysis--target-lang` dynamic var into
`generate-content`, `--cat-english-gloss`, and the Claude system
prompt (`target-lang-block` conditional — adds a German directive
when de-selected).  The bilingual DE // EN pair always remains in
the Detailed Dictionary; only Interlinear / CAT Gloss / Claude
Translation narrow to the selected half.

**Pass 5d** (`8476eef`) — cross-document consistency audit +
targeted rerun.  `audit-folder` walks all seg-*.org in a folder and
flags segments whose Interlinear gloss for a given term doesn't
match the thesaurus.  `rerun-affected-by-zettel` re-analyses only
the segments touched by a specific zettel, avoiding a full
`batch-reanalyze` when one gloss changes.

**Translation-gap report** (`6e78c22`) — `C-c u z g` lists every
segment where the thesaurus zettel has `[to be researched]` or an
empty target-lang side, so Carsten can walk the list and fill
entries before translating.  Designed for the German Milarepa prep
workflow: `C-c u z i` → `C-c u z L de` → `C-c u B` → `C-c u z g` →
walk the list.

**Tests**: `test/tibetan-thesaurus-test.el` (37 tests),
`test/tibetan-analysis-claude-prompt-test.el` (target-lang +
set-source tests), `tibetan-wylie-test.el` (`'adra`/`'dra`
regression from `d0e9f8a`), `tibetan-particles-bialek-test.el`
(single-`s` ergative tests from `259d00b`).

### 5.13 Three-level analysis dispatch (done, 2026-04-23)

`C-c u A` and `C-c u R` used to always target the segment the cursor
was on.  New behaviour: **detect context and route** — segment if
cursor is under a `**** Segment`, sentence if under `*** Sentence`,
paragraph if under `** §N` or similar.  Added via three commits:

1. **Generic §-level paragraph analysis** (`19b19ff`) — new
   `tibetan-paragraph-*` machinery for `** §N` (paragraph) context,
   bound to `C-c p A` / `C-c p R` as dedicated paragraph commands.
2. **Paragraph dispatch from `C-c u A` / `C-c u R`** (`03ff719`) —
   cursor-context detection hook added.
3. **Three-level unification** (`2b82b5f`) — dispatcher routes
   between segment/sentence/paragraph handlers based on the
   nearest enclosing heading.

Also in this window: `19a95f6` (specs/tigress + document-prep
updated to the current reading-file layout from §2.12).

### 5.14 Sanskrit-parallel reading workflow (done, tested, 2026-04-27)

Per-document opt-in parallel-Sanskrit reading mode for the
Yogācārabhūmi class.  Sanskrit is treated as the primary source;
the Tibetan canonical translation is the secondary.  The
analysis pipeline emits a `** Sanskrit Source` section above
Wylie, the Claude prompt becomes Sanskrit-primary with optional
`### Tibetan Divergence` notes, and a toggle command manages the
opt-in header.

Six phases shipped (1 commit each):

1. `cf8b635` — Walker + source-mode predicate
   (`core/tibetan-sanskrit-parallel.el`).  `**** Sanskrit`
   sibling lookup at-cursor and by-id; `--source-mode-parallel-p`
   reads `#+SOURCE_MODE: parallel-sanskrit`.
2. `4e27bdf` — `** Sanskrit Source` rendering, prepended to
   `tibetan-analysis--priority-section-order`.  Emitted only
   when the dynamic var `--sanskrit-text-for-render` is bound
   (caller-controlled gate).
3. `d68f37d` — Claude prompt directive: when `:source-mode` is
   `parallel-sanskrit`, system prompt gains a Sanskrit-primary
   block; user prompt prepends the IAST + Devanagari above the
   Tibetan passage.  Cache-warm: system block constant per doc.
4. `6b84d17` — Markdown `### Tibetan Divergence` →
   `(parent-level + 1)`-star org sub-heading via
   `--claude-body-md-h3-to-org` in
   `--replace-claude-section-body`.  Foldable, navigable.
5. `75eb837` — `tibetan-cat-toggle-source-mode-parallel`
   (`C-c u z P`) + menu.
6. `b33e864` — End-to-end wiring: dynamic var bound from the
   walker on the four real call paths
   (`tibetan-auto-analyze-document`,
   `--open-segment-analysis-impl`,
   `--reanalyze-segment-impl`,
   `tibetan-analysis-reanalyze-file`).

Tests: 61 ERT specs across
`test/tibetan-sanskrit-parallel-test.el` and the existing
prompt / sections test files.

Source files using the workflow: `gotrapatala.org` (97 segments
of Bodhisattvabhūmi) currently.  Other YBh chapters or the
Hausarbeit Tibetisch IV Milarepa work could opt in via
`#+SOURCE_MODE: parallel-sanskrit`.

Design doc: `docs/feature-sanskrit-parallel.org` (not yet
written; see this section for the summary).

### 5.15 DharmaMitra-driven Sanskrit re-alignment (done, tested, 2026-04-30)

Five-phase feature that uses DharmaMitra's public search +
chat-translate APIs to correct the rough daṇḍa-split alignment
of `**** Sanskrit` siblings in parallel-mode documents.
Workflow per segment:  translate Tibetan → English (DM) →
search Sanskrit corpus (DM) → ask Claude to disambiguate
candidates → build proposal (current vs proposed + status) →
preview / apply.

Phases:

1. `307183f` — DharmaMitra HTTP client
   (`core/tibetan-dharmamitra-api.el`): SSE parser, request
   builders, sha256-keyed in-memory cache, error handling.
2. `88385cd` — Per-segment candidates orchestrator
   (`core/tibetan-sanskrit-parallel-dharmamitra.el`):
   `--read-source-metadata` extended with
   `:dm-sanskrit-source` / `:dm-tibetan-source` plist keys
   from `#+DM_SANSKRIT_SOURCE:` / `#+DM_TIBETAN_SOURCE:`
   headers.
3. `4e881d1` — Claude disambiguation: prompt builder, response
   parser (`## Choice` + `## Reason` schema), synchronous
   gptel wrapper outside `tibetan-claude-queue`, top-hit
   fallback with stamped `:reason`.
4. `4b54f7b` — Proposals + walker + preview renderer.
   `tibetan-sanskrit-parallel-dm-realign-document-proposals`
   walks every `**** Segment N` heading; preview buffer
   summarises change / unchanged / no-candidates counts and
   shows per-segment current vs proposed Sanskrit + Claude
   reason.
5. `41689ac` — Writer + apply + interactive commands (initial).
   Source-side writer
   `tibetan-sanskrit-parallel-write-sanskrit-for-segment-id`
   that replaced segment's `**** Sanskrit` sibling body.
   `C-c u z d` segment, `C-c u z D` document; both default to
   preview, `C-u` applies.

6. `31ad7dd` — First design doc + CLAUDE.md + first live test
   on three gotrapatala segments.

7. `9c12060` — **Architectural correction.**  The source-side
   writer was retired.  Live test on segment 5 showed that DM
   segment granularity is coarser than Tibetan segment
   granularity (a 1-line uddāna verse line received a 10-line
   full-chapter-opening replacement).  More fundamentally,
   modifying the user's source from an analysis command
   violates CLAUDE.md §6.  New behaviour: apply mode writes to
   the per-segment analysis file (seg-NNN.org) as a top-level
   `* Sanskrit (DharmaMitra)` section.  Source file is NEVER
   touched (regression-tested).  The new section carries
   DM_SEGMENTNR / DM_RANK / CLAUDE_REASON / LAST_REALIGN in
   its property drawer + the proposed Sanskrit in its body.
   Top-level placement means it survives reanalysis naturally
   without preserve-list changes.

8. `(this commit)` — **Clause extraction polish.**  Phase 7
   wrote the FULL multi-line DM candidate text to the analysis
   file, addressing the source-write violation but leaving the
   granularity mismatch unfixed.  Phase 8 closes the gap:
   Claude's pick prompt schema gains a `## Clause' section;
   parser captures `:chosen-clause' alongside `:chosen-rank' /
   `:reason';  pick output plist carries `:chosen-clause'
   (full `:chosen-text' retained for provenance);
   `--build-proposal' uses `:chosen-clause' for
   `:proposed-sanskrit' when present + non-empty, falls back
   to full text otherwise.  Result: the analysis file's
   `* Sanskrit (DharmaMitra)' body contains just the matching
   line(s), not the full corpus chunk.

Tests: 89 ERT specs across
`test/tibetan-dharmamitra-api-test.el` (19) and
`test/tibetan-sanskrit-parallel-dharmamitra-test.el` (53).

Strict test-first discipline applied throughout: empty stubs
before implementation, run-against-stub to confirm each test
fails with a `should` assertion (not `void-function`),
implement, run-against-implementation to confirm pass.

Design doc: `docs/feature-dharmamitra-realign.org` — quickstart
+ architecture + design decisions + DM API surface findings.

DM endpoint base URL `https://dharmamitra.org/api-search/`,
public bearer `sthiramati` (default — overridable via
`tibetan-dharmamitra-api-token`).  Full OpenAPI spec at
`/api-search/openapi.json` (no auth).

### 5.16 Multi-translator parallel reading (done, tested, 2026-04-30)

Major feature pivot from §5.15's realign work: DharmaMitra is
added as a *translator* alongside Claude in the analysis files,
and Claude in parallel-Sanskrit mode now produces THREE
translation sections (Tibetan / Sanskrit / Combined synthesis)
plus an optional flagged-divergence note.  Per-segment analysis
files in parallel mode now carry up to seven translation-related
artefacts:

| Section                                | Engine | Source   |
|----------------------------------------|--------|----------|
| `* DharmaMitra Translation (Tibetan)`  | DM     | Tibetan  |
| `* DharmaMitra Translation (Sanskrit)` | DM     | Sanskrit |
| `** Sanskrit Source` (raw IAST + Devan)| —      | source   |
| `** Claude Translation`                | Claude | Tibetan  |
| `** Claude Translation (Sanskrit)`     | Claude | Sanskrit |
| `** Claude Translation (Combined)`     | Claude | Both     |
| `** Claude Divergence` (when flagged)  | Claude | Both     |

Non-parallel documents render identically to before — only the
Tibetan-side artefacts appear.  The two top-level DM sections sit
OUTSIDE `* Auto-Analysis` so they survive reanalysis without
preserve-list machinery.

Phases (6 commits, all green):

1. `09cba65` — Phase A.1: DM as Tibetan translator.  New module
   `core/tibetan-dharmamitra-translation.el` with synchronous
   `chat-translate` calls + section writer.  Fires on file
   create via `tibetan-auto-fire-dm-on-create` defcustom
   (default `t`).
2. `a90e6e4` — Phase A.1.5: existing-file open path also fires
   DM when section is missing (predicate
   `--needs-request-p` avoids redoing good translations).
3. `b5d5b43` — Phase A.2: Sanskrit DM fire when source-mode
   parallel-Sanskrit AND segment has a non-placeholder
   `**** Sanskrit` sibling.  Umbrella `fire-for-segment`
   dispatches both languages.
4. `59b2f5e` — Phase A.3: batch DM fire on `C-c u B` and
   `C-u C-c u r`.  Stagger via `tibetan-auto-dm-request-delay`
   (default 0.5s).
5. `c6e4d59` — Phase B: parallel-mode prompt directive +
   `## Translation (Sanskrit)` schema.  Parser, section-order,
   reader, scaffolder.
6. `de1fba0` — Phase C: `## Divergence` opt-in commentary,
   only when Claude flags serious differences.
7. `77e23b4` — Phase D: `## Translation (Combined)` synthesis.
   Reading order: Tibetan → Sanskrit → Combined → Divergence.

Live test on gotrapatala.org seg-005 surfaced two follow-up
concerns:

- **Source alignment was structurally invalid.**  The 2026-04-27
  daṇḍa-split positionally aligned chapter-1 Sanskrit clauses
  with Tibetan segments, but the Tibetan canon's chapter 1 opens
  with translator's homages (segs 1-2) and a 10-dharma framework
  intro + uddāna verse (segs 3-8) that has no counterpart in the
  Dutt-edition Sanskrit (which begins directly with `ṣaḍ
  imāni...`).  The 10 dharmas are the canonical
  *mahāyānasaṃgraha* framework (ādhāra/lakṣaṇa/pakṣa/adhyāśaya/
  vihāra/upapatti/parigraha/bhūmi/caryā/pratiṣṭhā = gzhi/rtags/
  phyogs/lhag-bsam/gnas-pa/skye/yongs-'dzin/sa/spyod/rab-gnas) —
  faithful to a Sanskrit framework, just not one that appears
  in the Dutt etext.

  Fix: `0b19f34` generalized placeholder-marker recognition
  (new `tibetan-sanskrit-parallel--placeholder-text-p`
  predicate; recognises `[Sanskrit alignment pending`, `[Sanskrit
  alignment exhausted`, `[No Sanskrit counterpart`).  Source
  file's all 97 `**** Sanskrit` bodies bulk-marked with
  `[Sanskrit alignment pending …]`.  `52454ad` added
  `M-x tibetan-sanskrit-parallel-reset-sanskrit-sections-in-
  folder` so the wrongly-translated sections in existing
  analysis files can be cleared idempotently.

- **Verb-detect false positives in uddāna verses.**  `སྡོམ` and
  `རྣམས` were classified as verbs in seg-005's Sentence
  Structure block, contradicting Verb Classification's
  `[No Hill-DB verbs detected]`.  Root cause: vocab-fallback
  fired on Bialek glosses starting with `to X` (`to fetter |
  bind...`, `to choke | plural marker`) without checking if
  the rest of the gloss labels the word a particle / marker.

  Fix: `897235c` tightened `--vocab-says-verb-p` to AND its
  verbal-pattern check with a NOT-match against
  `\b(particle|marker|postposition|nominali[sz]er|enclitic)\b`,
  AND tagged minimal entries with `(source . closed-set)` vs
  `(source . vocab-fallback)`.  The clause-structure renderer
  now filters vocab-fallback entries before clause segmentation,
  restoring symmetry with Verb Classification.  Curated closed-
  set verbs (`གསོལ`, `མཛད`, `བྱུང`, …) still drive clauses.

Tests: 51 new ERT specs across the work series (Phase A.1: 14;
A.2: 8; A.3: 6; B: 10; C: 9; D: 10; placeholder: 8; reset: 6;
verb-detect: 6).

Suite progression today (all green): 1734 → 1744 (B) → 1753 (C)
→ 1764 (D) → 1772 (placeholder) → 1778 (reset) → **1784**
(verb-detect).

Design doc: `docs/feature-multi-translator-parallel-reading.org` —
phase walkthrough + reader-flow ordering + alignment workflow +
why automated realign couldn't fix gotrapatala + verb-detect
fix root-cause + commit trail.

**SUPERSEDED 2026-04-30 by §5.17.**  The Phase B–D suffixed
schema (`** Claude Translation (Sanskrit)' / `(Combined)' /
`** Claude Divergence' INSIDE `* Auto-Analysis') is retired.
Sanskrit and Combined output now live in dedicated top-level
sections via three independent Claude calls.  See §5.17 for
the new architecture.

### 5.17 Two-language parallel analysis (done, tested, 2026-04-30 — superseded by §5.18)

**SUPERSEDED 2026-05-04 by §5.18.**  The per-segment file layout
described below was reorganised for the Sanskrit-first reading
workflow:  parent heading rename `* Auto-Analysis' → `* Tibetan
Analysis'; level-2 `** Claude Translation' → `** Translation' on
both Tibetan and Sanskrit sides; section reorder Sanskrit-first;
`** Divergence' (opt-in) → `** Sanskrit-Tibetan Comparison'
(always-emitted with `[Faithful — …]' marker for faithful
renderings).  Architecture (three Claude calls, walker, queue,
dispatcher) unchanged.  See §5.18 for the layout-revision details.

**Architectural elevation** of yesterday's §5.16 work.  After
§5.16's seg-005 live test surfaced a Combined-translation
duplication bug (the parallel-mode prompt asked Claude for a
Combined section even when only Tibetan input was present),
Carsten asked for Sanskrit to become a first-class peer of
the Tibetan analysis with a dedicated synthesis layer above
both — instead of a sibling section inside the Tibetan
analysis file.

Per parallel-Sanskrit segment, three independent top-level
analysis sections in one `seg-NNN.org` file:

| Section                   | Producer                       | Pre-existing? |
|---------------------------|--------------------------------|---------------|
| `* Auto-Analysis`         | Claude (Tibetan call)          | yes (slimmed) |
| `* Sanskrit Text`         | walker (raw IAST + Devanagari) | NEW (Phase 3) |
| `* Sanskrit Analysis`     | Claude (Sanskrit call)         | NEW (Phase 2) |
| `* Combined Analysis`     | Claude (Combined call)         | NEW (Phase 4) |

Plus the three pre-existing top-level DM sections (`*
DharmaMitra Translation (Tibetan)' / `(Sanskrit)' / `*
Sanskrit (DharmaMitra)').

**Phases (two commits):**

1. `a5caeac` — Phases 1+2+3+4 bundled.
   - Phase 1: retired the §5.16 parallel-mode block
     instructions from the Tibetan prompt + the suffixed
     parser keys / scaffolders / section-order entries.
     Tibetan call now byte-identical for parallel and non-
     parallel docs.
   - Phase 2: NEW `persist/tibetan-analysis-sanskrit.el` —
     Sanskrit Claude pipeline (5-section response: Translation
     / Devanagari / Sandhi / Word List / Grammar).
   - Phase 3: `--render-sanskrit-source' rewritten to emit
     `* Sanskrit Text' top-level (was `** Sanskrit Source'
     under `* Auto-Analysis').
   - Phase 4: NEW `persist/tibetan-analysis-combined.el' —
     Combined synthesis pipeline (2-section response:
     Translation / Divergence; both gated on all four inputs
     being present).

2. `9802833` — Phase 5: dispatcher + reanalyse preservation.
   - New `tibetan-analysis--fire-parallel-mode-claude-calls'
     orchestrates Sanskrit + (chained) Combined.  Wired into
     `reanalyze-file' + auto-analyze batch + auto-fire-on-
     create batch.
   - `--get-user-sections' / `--regenerate-auto' extended to
     preserve the six new top-level sections.  Closes a
     latent regression where DM sections were silently
     destroyed by every reanalyse-without-refire.

**Three Claude calls in parallel mode:**

| Call     | Module                                      | Schema |
|----------|---------------------------------------------|--------|
| Tibetan  | `tibetan-analysis-claude.el` (slimmed)      | `## Translation` / `## Vocabulary` / `## Grammar` / `## Particles` |
| Sanskrit | `tibetan-analysis-sanskrit.el` (NEW)        | `## Translation` / `## Devanagari` / `## Sandhi` / `## Word List` / `## Grammar` |
| Combined | `tibetan-analysis-combined.el` (NEW)        | `## Translation` / `## Divergence` (the latter only when serious differences) |

Each call's system prompt is constant per document.  Anthropic
prompt cache stays warm for each call type independently.
Tibetan + Sanskrit fire concurrently; Combined chains off
Sanskrit's callback when both translations are present.

**Section independence:** each top-level analysis section is
owned by one call type — Tibetan reanalyse touches only `*
Auto-Analysis`, Sanskrit reanalyse only `* Sanskrit Analysis',
etc.  Same pattern as today's DM top-level sections.

**Reanalyse preservation (regression fix):**  before this
work, `--get-user-sections' didn't list the DM sections
(shipped Phase A.1–A.2 earlier today), so reanalyse-without-
refire silently destroyed them.  Phase A.3 masked the bug
because batch reanalyse typically runs with refire on.  Phase
5 extends the section list with all six new top-level names
\(Sanskrit Text, Sanskrit Analysis, Combined Analysis, three
DM variants).

**Migration of existing analysis files:** soft.  Existing
files keep their current layout until next reanalyse — at that
point the Tibetan side regenerates without the now-retired
suffixed sections, the Sanskrit/Combined sections appear on
re-fire, and the prior file's top-level sections are
preserved.  Phase 6 (a migration helper to drop the now-inert
`** Claude Translation (Sanskrit)' / `(Combined)' / `**
Claude Divergence' subsections lingering inside `* Auto-
Analysis' from yesterday's §5.16 work) is **deferred** —
pairs with the analysis-files refactoring Carsten plans.

**Suite trajectory:** 1792 (post-§5.16 + verb-detect work) →
1755 (Phase 1 retire) → 1771 (Phase 2 add) → 1769 (Phase 3
update) → 1783 (Phase 4 add) → **1787** (Phase 5 add).  Net
−5 because Phase 1 retired more tests than the new modules
added.  For a refactor that retires schema, lower-but-tighter
is the right outcome.

Design doc: `docs/feature-two-language-parallel.org` — full
phase walkthrough + per-segment file layout diagram + the
three Claude system-prompt drafts + dispatcher mechanics +
preservation invariants + verification recipe + commit trail.

### 5.18 Per-segment layout revision (Sanskrit-first reading, done, tested, 2026-05-04)

**Layout-only revision of §5.17.**  Architecture is unchanged
(three Claude calls per parallel-Sanskrit segment; walker +
queue + dispatcher all preserved).  This work reorganises the
on-disk shape of the per-segment analysis file to match the
class workflow:  Yogācārabhūmi reading reads the Sanskrit FIRST,
translates it, then checks the Tibetan against it.  Three
specific user-facing pain points:

1. Tibetan was first in the file order, with Sanskrit
   interleaved — wrong reading order.
2. Both `* Auto-Analysis' and `* Sanskrit Analysis' carried
   level-2 `** Claude Translation' — students couldn't tell at
   a glance which language a translation was in.
3. `** Divergence' was opt-in; faithful renderings produced
   no comparison section, so the reader couldn't tell whether
   comparison was performed.

#### Target on-disk layout

```
* My Notes                                    user-edited
* Working Translation                         user-edited

* Sanskrit Text                               raw IAST + Devanagari
* Sanskrit Analysis
  ** Devanagari                               (conditional)
  ** Sandhi Decomposition                     (conditional)
  ** Word List
  ** Translation                              ← was ** Claude Translation
  ** Grammar

* Tibetan Text                                raw Tibetan
* Tibetan Analysis                            ← was * Auto-Analysis
  ** Wylie Transliteration
  ** Interlinear Gloss
  ** Translation                              ← was ** Claude Translation
  ** Grammar
    *** Particle Map
    *** Claude Grammar                        (level-3, kept)
    *** Particles in This Segment
  ** Sentence Structure / Verb Classification / Detailed Dictionary
  ** Provided Translations
    *** Claude Vocabulary                     (level-3, kept)
    *** Claude Particles                      (level-3, kept)

* Combined Analysis
  ** Combined Translation
  ** Sanskrit-Tibetan Comparison              ← always emitted
                                                 ([Faithful — …] marker
                                                 when faithful)

* Footnotes                                   user-edited

* DharmaMitra Translation (Sanskrit)          ← Sanskrit-DM first now
* DharmaMitra Translation (Tibetan)
* Sanskrit (DharmaMitra)                      legacy realign output
```

#### Heading rename table

| Old (§5.17)              | New (§5.18)                      |
|--------------------------|----------------------------------|
| `* Auto-Analysis`        | `* Tibetan Analysis`             |
| `** Claude Translation`  | `** Translation` (both sides)    |
| `** Divergence`          | `** Sanskrit-Tibetan Comparison` |

Level-3 sub-headings under `** Grammar' / `** Provided
Translations' KEEP their `Claude' prefix:  `*** Claude
Vocabulary', `*** Claude Particles', `*** Claude Grammar'.
The prefix still disambiguates among multiple level-3 cousins
(DharmaMitra / Roehrich / Reference Translations).

#### Phases (13 commits, +30 tests)

| Phase | Commits | Tests | Description |
|-------|---------|-------|-------------|
| 1.1   | 1       | +2    | `--get-user-sections' lists Tibetan Analysis |
| 1.2   | 1       | +2    | Writers emit `* Tibetan Analysis` (paragraph carve-out kept) |
| 1.3   | 1       | +3    | Tibetan-side `** Claude Translation` → `** Translation` |
| 1.4   | 1       | +3    | Sanskrit-side same rename |
| 1.5   | 1       | +2    | Dispatcher reads `** Translation` for chained Combined call |
| 2.1   | 1       | +5    | Sanskrit-first section reorder in `regenerate-auto` + `create-file` |
| 3.1+2 | 1       | +8    | Combined parser/writer rename + on-disk migration |
| 3.3+4 | 1       | +5    | Combined system prompt always-emit + faithful marker + end-to-end |
| 4.1–3 | 3       | +0    | CLAUDE.md §5.18 + cross-doc sweep |

#### Migration story

**Dual-name parsers + new-name-only writers.**  Every reader
accepts both old and new heading names (preferring new); every
writer emits only the new name.  On first reanalyse of an OLD
file, the preserved-content path picks up bodies from old
headings, the regenerator emits using new headings — natural
one-pass migration, no manual sweep, no scripted batch.

Every rename has an in-place migration helper called from the
appropriate insert/restore entry point:
- `tibetan-analysis--migrate-legacy-claude-headings' renames
  level-2 `** Claude Translation' → `** Translation' (Tibetan
  side, segment layout only).
- `tibetan-analysis-sanskrit--migrate-legacy-translation-heading'
  same for Sanskrit-side.
- `tibetan-analysis-combined--migrate-legacy-divergence-heading'
  renames `** Divergence' → `** Sanskrit-Tibetan Comparison'.
- `regenerate-auto' rewrites `* Auto-Analysis' → `* Tibetan
  Analysis' on the next reanalyse pass.

All migrations are idempotent.  `--get-user-sections' lists both
old and new section names so the regenerator's alist consumer
sees the new heading on already-migrated files.

#### Carve-outs (deliberately untouched)

- **Sentence files (`sent-NNN*.org`):** ALIGNED with segment
  shape as of 2026-05-18 (the original §5.18 carve-out is retired
  — see "Sentence-file alignment" subsection below).  Sentence
  files now use `* Tibetan Analysis' parent, `** Translation' at
  level-2, nested `** Provided Translations' with sentence-only
  `*** Roehrich' / `*** Class Translation' / `*** Claude Context'
  alongside `*** Claude Vocabulary' / `*** Claude Particles'.
- **Paragraph files (`par-NNN.org`):** keep `* Auto-Analysis'.
  Different flow; not parallel-Sanskrit.
- **Compound analysis (`tibetan-compound-analysis.el`):** keeps
  `* Auto-Analysis'.  Multi-verb verse workflow, separate.

#### Sentence-file alignment (2026-05-18 follow-up)

The original §5.18 work carved sentence files out because their
discourse-level Claude pipeline (3 sections: Translation /
Grammar / Context) is distinct from the segment-level word-by-
word pipeline (4 sections: Translation / Vocabulary / Grammar /
Particles).  A subsequent user request — "I want sentence files
in the equal form like segment files" — drove a follow-up
alignment.

What aligned:
- Parent rename `* Auto-Analysis' → `* Tibetan Analysis'.
- User sections (My Notes, Working Translation) moved from
  BOTTOM to TOP (matching segment §5.18 ordering).
- Top-level `* Provided Translations' DROPPED;  the level-3
  entries (Roehrich, Class Translation, Claude Context) moved
  INSIDE a nested `** Provided Translations' under `* Tibetan
  Analysis'.
- Sentence-level Claude Translation promoted from level-3
  (`*** Claude Translation') to level-2 (`** Translation') —
  matches segment's primary slot.
- Footnotes stays at the BOTTOM.

Mechanics:
- `tibetan-sentence--segment-claude-sections' strip-list emptied;
  the segment-renderer's `** Translation' and `** Provided
  Translations' subtrees now flow through to the sentence file's
  `* Tibetan Analysis' body unchanged.
- `tibetan-sentence--inject-sentence-l3-entries' appends `***
  Roehrich' / `*** Class Translation' / `*** Claude Context' to
  the nested `** Provided Translations' block.
- `tibetan-sentence--regenerate' rebuilt to use a PRESERVE →
  REBUILD-via-scaffold → RESTORE pattern.  Reads bodies from
  legacy positions, scaffolds fresh, restores into new positions.
- `tibetan-analysis--claude-segment-layout-p' detector updated
  to recognise the new aligned sentence layout (presence of
  `* Tibetan Analysis' parent) as segment-shape for migration
  purposes.  Legacy sentence files (with `* Auto-Analysis' or
  top-level `* Provided Translations') still classify as sentence-
  shape so the Claude writer uses the level-3 layout expected
  by the pre-migration files.
- `tibetan-analysis--claude-section-order' extended with
  `(:context "Claude Context" 3)' so the writer can place the
  sentence-only Context body.  Segment Claude responses don't
  produce Context — the writer just skips it.

Migration:  on first `--regenerate' the old layout (Auto-Analysis
+ top-level Provided Translations + bottom user sections) is
rewritten into the new layout, preserving user-edited bodies
verbatim.  Idempotent — re-running on an already-migrated file
produces identical output.

#### Cache impact

Tibetan + Sanskrit Claude system prompts byte-identical pre/post
refactor — Anthropic prompt cache stays warm.  Only Combined
prompt changes in Phase 3.3 (one-time cache miss on the first
Combined call after the commit lands; subsequent calls re-cache
at the usual 10% input cost — bounded to one segment per active
Combined batch).

#### Suite trajectory

1809 (post-§5.17 + DSBC alignment + cleanup work) → 1811 (Phase
1.1) → 1813 (1.2) → 1816 (1.3) → 1819 (1.4) → 1821 (1.5) → 1826
(2.1) → 1834 (3.1+3.2) → **1839** (3.3+3.4).  Monotonic growth;
zero unexpected failures at any phase boundary.

#### Verification recipe

Run after migration ships:

1. `cd /Users/cp/tibetan-cat.el/test && emacs -batch -l
   run-all-tests.el -f ert-run-tests-batch-and-exit` → 1839
   pass / 0 unexpected / 1 intentional skip.

2. **Live migration**.  On a copy of the gotrapaṭala
   `analysis/` folder (~97 seg-NNN.org files):
   - `M-x tibetan-analysis-batch-reanalyze` with
     `:re-request-claude nil`.
   - Spot-check three random files:  confirm new top-level
     order (My Notes / Working Translation at top, Sanskrit
     pair before Tibetan pair, DM Sanskrit before DM Tibetan),
     `* Tibetan Analysis' parent heading, `** Translation'
     (both sides), `** Sanskrit-Tibetan Comparison' present.
   - `git diff` on the analysis folder:  heading renames +
     section reorder + `** Divergence' → `**
     Sanskrit-Tibetan Comparison', no lost user content.

3. **Live re-fire**.  Pick one segment, re-fire with
   `:re-request-claude t` to confirm the Combined Claude call's
   new prompt produces a non-empty `** Sanskrit-Tibetan
   Comparison' body on a previously faithful-rendering segment
   (the `[Faithful — …]' marker should appear).

4. **Idempotency**.  Re-run batch reanalyse a second time; diff
   should be empty modulo whitespace + LAST_ANALYZED stamps.

5. **Carve-out**.  Reanalyse a sentence file; confirm `*
   Auto-Analysis' stays unchanged.

#### Files changed

| Path | Role |
|------|------|
| `persist/tibetan-analysis-persist.el` | `regenerate-auto` reorder + parent rename, `create-file` reorder, `--get-user-sections` extension, `--fire-parallel-claude-with-plist` translation-readback chain |
| `persist/tibetan-analysis-claude.el` | `--claude-section-order` (segment) `Translation` rename, `--migrate-legacy-claude-headings` extension, `--insert-claude-translation-heading` rewrite + new fallback, `--ensure-claude-headings` dual-name accept, `--write-comparison-section` regex, `--write-claude-failure-stub` layout-aware dispatch, `--read-claude-sections` chain |
| `persist/tibetan-analysis-sanskrit.el` | `--section-order` `Translation` rename, `--migrate-legacy-translation-heading` helper, `--read-sections` fallback |
| `persist/tibetan-analysis-combined.el` | `:divergence` → `:comparison` parser key, `--section-order` `Sanskrit-Tibetan Comparison` rename, `--migrate-legacy-divergence-heading` helper, `--read-sections` fallback, `--system-prompt-base` rewrite (always-emit + `[Faithful — …]` marker) |
| `persist/tibetan-analysis-combine.el` | `--read-auto-sections` regex multi-name accept |
| `persist/tibetan-sentence-persist.el` | strip-list extended with `** Translation` for segment embeds |

### 5.8 Claude integration hardening + Anthropic prompt caching (done, 2026-04-20)

Five cumulative improvements to the Claude request path, all
motivated by running real batch fills against the 97-segment YBh
corpus and the 85-sentence Milarepa corpus:

1. **Batch-create fires Claude automatically** (commits `bac120b`
   + `bd3ef40`).  `tibetan-auto-analyze-document' (C-c u B) and
   `tibetan-sentence-create-all' (C-c s N) previously left every
   newly-created file with a `[Requesting translation...]'
   placeholder, requiring a separate
   `tibetan-auto-request-claude-translations' pass to fill them —
   parity broken with the single-file `C-c u A' / `C-c s A' path
   that always fired Claude on create.  Both batch commands now
   queue staggered async Claude requests for every newly-created
   file.  Gated by `tibetan-auto-fire-claude-on-create' (default
   `t`).  +6 ERT tests.  The placeholder-detector regex was also
   relaxed from `^\\*\\*\\* Claude\\$' (three stars, no suffix) to
   `^\\*+ Claude\\(?: Translation\\)?[ \\t]*\\$' so both the Auto-
   Analysis two-star and Provided-Translations three-star layouts
   register as "needs Claude".  +4 ERT tests.

2. **Drawer-aware segment-text extractor** (commit `ebac416`).
   `tibetan-org-get-segment-text' used to return everything between
   the `*** Segment' heading line and the subtree end — which on YBh
   segments carrying a `:FOLIO: Dxxxx' property drawer meant the
   drawer body (`:PROPERTIES: :FOLIO: D3a3 :END:') was concatenated
   into the "Tibetan text" handed to the structural analyser,
   sometimes tripping `(wrong-type-argument stringp nil)` downstream.
   Fixed to skip a leading `:PROPERTIES: … :END:' drawer and stop
   at the first child heading (any depth) so `**** Working
   Translation' siblings also don't leak.  +2 ERT tests.

   Known regression: the drawer-skip fix meant newly-created
   analysis files no longer carry the `:FOLIO:' property on their
   own `* Tibetan Text' heading (the drawer is stripped during
   read).  Low priority — the folio is still visible in the source
   seg's drawer.  See §6 P6.

3. **Skip-check path matches create-file's write path** (commit
   `28deda7`).  `tibetan-auto-analyze-document's skip-check called
   `(tibetan-analysis-get-filepath seg-num source-file)' which
   returns a SUFFIXED path like `seg-NNN-<shortname>.org', but
   `tibetan-analysis-create-file' internally re-computes the path
   WITHOUT source-file and writes to the UNSUFFIXED `seg-NNN.org'.
   The mismatch meant every existing seg-NNN.org was silently
   OVERWRITTEN on every re-run of `C-c u B' — wiping Claude
   translations.  Fix: drop the source-file arg from the skip-check
   so both sides use the unsuffixed path.  Lost ~3 hours of API
   spend rediscovering this; regression test in place (`auto-analyze-
   skip-check-matches-create-path').

4. **Anthropic prompt caching** (commit `cc582ff`).
   Every Claude request from the tool uses the SAME system prompt
   across all segments of a given document (it's `tibetan-analysis--
   claude-system-prompt' + the source file's `#+TIBETAN_CLAUDE_CONTEXT'
   block).  Wrapping each `gptel-request' call in
   `(let ((gptel-cache '(system))) ...)' tells gptel to attach
   `cache_control: {"type": "ephemeral"}' to the system prompt —
   Anthropic then serves it from cache at 10% of normal input cost
   on requests 2..N within the 5-min TTL.  Verified working via
   direct API probe (`cache_read_input_tokens' jumps to non-zero
   after first request).  Net savings: ~50% of input cost on warm
   cache.

5. **Operational outcomes** delivered 2026-04-20:
   - **Milarepa Tibetisch IV**: 85/85 `sent-*.org' filled via
     headless batch (12-min wallclock), preserved across
     re-segmentation disaster.
   - **Yogācārabhūmi Gotrapaṭala**: 97/97 `seg-*.org' filled via
     three-pass headless batch (first pass 39/92 ok, OTPM
     rate-limited; second pass with concurrency=1 delay=15s got
     the rest after user bought Anthropic API credits).
   - **Credit-balance vs Max-plan-subscription** distinction
     documented — the tool uses a separate `~/.authinfo' API key,
     NOT the Claude Code Max subscription.

### 5.7 Sentence re-segmentation workflow + bare-seg migrator (done, tested, 2026-04-20)

Two related workflows added/fixed for the Milarepa re-segmentation
and Josefine layout-harmonisation passes:

1. **Re-segmentation orchestrator** (`persist/tibetan-sentence-persist.el`):
   - `tibetan-sentence-resegment` (`C-c s Z`) — orchestrator chaining
     archive → reset → re-segment → create-all behind a single yes/no.
   - Sub-commands: `…-archive-analysis-folder` (`C-c s X`),
     `…-reset-structure` (`C-c s U`, actually lives in
     `doc-prep/tibetan-sentence-structure.el`), `…-create-all`
     (`C-c s N`).
   - Tests: 18 ERT (8 reset, 4 archive, 4 create-all, 2 resegment).
2. **Segment-structure demotion fix** (`doc-prep/tibetan-sentence-structure.el`):
   `tibetan-add-sentence-structure' previously only demoted the first
   segment of each sentence, leaving continuation segments orphaned
   at `*** Segment' sibling of `*** Sentence'.  Fixed; all segments
   now demote to `**** Segment'.  +3 ERT tests.
3. **WT-leak fix in `create-all`**: body-end detector now stops at
   any heading (`^\\*+ `) instead of only `**** Segment', so sibling
   `**** Working Translation' doesn't leak into the sentence's
   concatenated Tibetan text.  +1 ERT test.
4. **Bare-seg migrator** (`doc-prep/tibetan-segment-migrate.el`):
   `tibetan-migrate-bare-segments-to-headings` — converts
   `〔seg:N〕…〔/seg〕' markers (no `〔trans:N〕' pairs — the
   Thar-rgyan / Josefine pipeline) to `*** Segment M' headings.
   Pipeline: split+flatten Tibetan-title headings to `** Section',
   hoist `#+KEYWORD:' metadata to document header, renumber from 1,
   drop segments that become empty after hoisting.  +10 ERT tests.
5. **Yogācārabhūmi Wylie preparation** (`doc-prep/tibetan-ybh-prep.py`):
   Python script (pyewts dependency) that prepares a Wylie Derge-style
   śāstra source into the Milarepa segment layout.  Used 2026-04-20
   to prep `gotrapatala.org' (97 segments × 97 sentences, 97 :FOLIO:
   properties inheritance-filled).  Install pyewts with
   `pip3 install --break-system-packages --user --no-build-isolation pyewts'.

### 5.6 Bug fixes shipped this round
- **`པ' / `པའི' tagged as verbs (P1 from §6).** Added
  `tibetan-verb-detect--nominalizer-set' in
  `analysis/tibetan-enhanced-display.el'; `--lookup' rejects bare
  nominalisers regardless of which dictionary path matches.
- **Steinert cap drift fix** (the original 7 multisource tests were
  red — fingerprint regex bracket-class typo + lexical-binding
  `set'/`symbol-value' pattern; both rewritten).
- **`ལ' detailed dictionary primary chopped at `(1) [accusative'.**
  `tibetan-vocab--parse-entry' now uses a bracket-aware
  `--first-sense-bracket-aware' scanner so commas inside `[…]' or
  `(…)' don't terminate the first-sense extraction.
- **Word/Particle List vs Detailed Dictionary out of sync** (entries
  showed Steinert/IvesWaldo gloss while DD showed Resources).  Word
  list now routes through `tibetan-vocab-multisource-entries' too,
  taking the first entry — Resources-first across the board.
- **Dictionary reload on every reanalysis**.
  `data/tibetan-glossary-loader.el's `load-all-glossaries' was
  unguarded (always `clrhash' + reload).  Added the
  `tibetan-glossaries-loaded' flag and an idempotent guard;
  `tibetan-analysis--ensure-vocabulary' short-circuits on the flag.

### 5.19 Phonetics section below Wylie (THL, Lhasa, 2026-05-19)

Class-reading aid.  Wylie alone is unforgiving when the
student just needs to vocalise a line;  THL Simplified
Phonetic Transcription (Germano & Tournadre 2003)
approximates Lhasa pronunciation in Wylie-friendly Roman
orthography (sh / ch / zh / ng).  Sits at level-2 directly
under `** Wylie Transliteration' in every per-segment AND
per-sentence analysis file (segment renderer flows through
the shared `tibetan-analysis-generate-content' pipeline).

#### Layout

```
* Tibetan Analysis
  ** Wylie Transliteration
  bkra shis bde legs phun sum tshogs

  ** Phonetics                              ← NEW (auto-generated)
  tra shi de lek phün sum tshok

  ** Interlinear Gloss
  …
```

#### Module

`core/tibetan-phonetics.el' (~370 lines + 46 ERT specs).
Public entry points:

  - `tibetan-to-phonetics (TEXT)` — Tibetan Unicode OR Wylie
    input;  returns space-separated THL string, shad-preserving,
    nil-safe.
  - `tibetan-wylie-syllable-to-phonetic (WYLIE-SYL)` — single-
    syllable converter, exposed for thesaurus UI + tests.
  - `tibetan-phonetics--curated-override (WYLIE)` — looks up
    a `:phonetics:' field on a thesaurus zettel;  when present,
    takes precedence over the rule engine.

#### Algorithm

Per syllable:
1. Parse into prefix / superscript / root / subscript / vowel /
   suffix / post-suffix using validated combo tables
   (`--valid-prefix-combos', `--valid-super-combos') to
   disambiguate cases like `by-' (root b + y-subscript, NOT
   prefix b + root y).
2. Map initial cluster (root + subscript) to THL initial via
   `--cluster-overrides' table (palatalisations like `by → j',
   retroflexes like `kr → tr', etc.).  Prefix + superscript
   silently drop.
3. Apply vowel umlaut when suffix ∈ {i, e, n, l, d, s}.
   Post-suffix is NEVER an umlaut trigger (so `tshogs' with
   suffix `g' + post-suffix `s' stays unumlauted → `tshok',
   while `chos' with suffix `s' alone umlauts → `chö').
4. Apply final consonant rules:
   - `-ng' / `-m' kept
   - `-g' (+ silent `-s') → `-k'
   - `-b' → `-p' (final devoicing)
   - `-n' / `-l' kept (visual cue post-umlaut)
   - `-s' / `-d' / `-'` / `-r' silent

#### Tone marking

THL formal documents distinguish high/low tone via diacritics
(`á' vs `à').  This implementation OMITS tone — consistent
with simplified renderings in most published THL texts.  Tone
can be inferred from the prefix/superscript when needed;
omitting it keeps the phonetic line readable.

#### Curated overrides

When the rule engine produces wrong output for a specific
word (Madhyamaka terms, Sanskrit loanwords, monastic-tradition
readings), the fix is to populate the per-word thesaurus
zettel's `:phonetics:' field — not to complicate the rules
table.  The override path checks for this field and uses it
verbatim when present.

#### Wiring

- `tibetan-analysis--priority-section-order' (in
  `persist/tibetan-analysis-persist.el', line 2948+) extended
  with `"** Phonetics"' at position 2 (right after Wylie).
- `tibetan-analysis-generate-content' (~line 3308) inserts
  the heading + body inline with the Wylie pass.  Soft-required
  — emits `[Phonetics not available]' when the converter
  module isn't loaded.
- Sentence files inherit automatically via
  `tibetan-sentence--render-auto-analysis' (sentence renderer
  re-uses the segment generator).
- Top-level `(require 'tibetan-phonetics)' in `tibetan-cat.el'.

#### Suite trajectory

1882 → 1929 / 1928 expected / 0 unexpected / 1 skipped.  46
phonetics specs + 1 integration spec.  BDD 244 / 244.  Lint
clean.

#### Live spot-check (seg-049, Tibetisch IV)

Wylie:  `khyed rang sgor shog dang  mdang mkha' 'gro chos
skyong rnams ngo so zla ba bzhin phyar nas/'

Phonetics:  `khye rang go shok dang dang kha dro chö kyong
nam ngo so da ba zhin cha ne/'

Readable, faithful to Lhasa pronunciation;  `'gro' →  `dro'
(retroflex r-subscript), `chos' → `chö' (umlaut), `mdang' →
`dang' (m-prefix silent, homophone with `dang').

### 5.20 DharmaMitra Tibetan next to Claude Translation (done, 2026-05-19)

Layout-only revision:  the top-level `* DharmaMitra
Translation (Tibetan)' section moved INSIDE `* Tibetan
Analysis' as a level-2 sibling of `** Translation' (Claude).
Reader gets both AI translations side-by-side under one parent
heading instead of one at the top and one at the bottom of the
file.  Sanskrit-side DM stays at top-level (parallel-mode
only).

Mechanics:  new `tibetan-dharmamitra-translation--write-
nested-tibetan-section' writer locates the nested slot and
overwrites the body atomically;  `--read-dharmamitra-tibetan-
nested-body' reader pulls the body from the new location;
`regenerate-auto' reads BEFORE buffer erase, writes BACK after
save.  Same dual-name preserve pattern as §5.18:  legacy
top-level body still preserved on first regenerate, then
migrated to the new nested location.

### 5.21 Per-segment layout revision — Mid-class Cleanup (done, 2026-05-20)

Class-use feedback after §5.18 / §5.19 / §5.20 layout-revisions:
the per-segment file flows better than before but five concrete
frictions remained.  Each was small in isolation;  together
they pushed the file into the shape Carsten wants for the
Tibetisch IV Milarepa class reading.

#### Target on-disk layout (post-§5.21)

```
* My Notes                            user-edited (preserved)
* Working Translation                 user-edited (preserved)
* Tibetan Text                        raw Tibetan
* Tibetan Analysis
  ** Wylie Transliteration
  ** Phonetics
  ** Interlinear Gloss
  ** Claude Vocabulary                ← MOVED from Provided Translations
  ** Translation                      Claude
  ** DharmaMitra Translation          peer of Translation
  ** Grammar
    *** Particles                     ← MERGED (Particle Map + Particles
                                        in This Segment) with Bialek 2022
                                        textbook refs in the bullet bracket
    *** Claude Grammar                Claude prose, kept
  ** Sentence Structure
  ** Verb Classification (Hill 2010)
  ** Provided Translations            ← USER-CONTENT; preserved across
                                        regenerate (Carsten pastes
                                        Roehrich / Lopez here)
  ** Detailed Dictionary              ← MOVED to bottom (reference, not
                                        flow)
* Footnotes                           user-edited (preserved)
* DharmaMitra Translation (Sanskrit)  parallel-mode only
* Sanskrit (DharmaMitra)              parallel-mode only
```

#### Commits (7 commits)

| Commit | Subject                                                | Tests |
|--------|--------------------------------------------------------|-------|
| 1/7    | `*** CAT Gloss' + 248-line composer retired            | +2    |
| 2/7    | `** Claude Vocabulary' promoted to level 2 below Interlinear | +3 |
| 3/7    | `*** Particle Map' + `*** Particles in This Segment' merged into `*** Particles' | +2 (updated) |
| 4/7    | Bialek 2022 published-textbook refs added to particle bullets | +4 |
| 5/7    | `** Detailed Dictionary' moved to bottom of `* Tibetan Analysis' | +2 |
| 6/7    | `** Provided Translations' becomes user-content (preserved across reanalyze) | +3 |
| 7/7    | CLAUDE.md §5.21 + final verification                   | docs  |

Each commit is a single logical change per CLAUDE.md §2.4 and
follows the §5.18 / §5.19 / §5.20 layout-revision mechanics
(dual-name parsers + new-name writers + migration helpers,
signed off three times before this batch).

#### Particle bullet shape (Commits 3+4)

```
*** Particles
=CASE= (magenta) · ~CONVERB~ (orange) · Ø zero-marked

khyed rang sgor shog =dang=  mdang mkha' 'gro ... =gyi= ... ~nas~

- དང [dang] · COMITATIVE (COM)
    [Bialek 2022 §1.7 (Comitative); Portfolio §1.9]
    §1.7.1 Accompaniment — the comitative links two nouns that
    co-participate in an event or state.

    - byams pa dang snying rjer ldan te
      "endowed with kindness and compassion"  (LBRG)

- ནས [nas] · CONVERBIAL: ABLATIVE CONVERB
    [Bialek 2022 §2.11 (V+nas); Portfolio §2.11]
    §2.4 approx-sequential
```

Visual skeleton at the top of the merged section, per-particle
bullets below.  Each bullet's bracket carries BOTH `Bialek 2022
§X.Y (Title)' (published textbook, canonical numbering, via the
new `philology/tibetan-bialek-textbook-refs.el' alist) AND
`Portfolio §X.Y' (Carsten's hand-written zettel, may differ
from the textbook numbering).  Either side may be absent →
bracket falls back to whichever is available.  Portfolio sub-
function snippet inlined below the bullet (existing Pass 6c
behaviour preserved).

#### Provided Translations preservation (Commit 6)

Renderer emits `** Provided Translations\n\n\n' as a pristine
empty placeholder.  `tibetan-analysis--read-provided-
translations-nested-body' reads the existing body BEFORE the
buffer is erased;  `tibetan-analysis--write-nested-provided-
translations-body' restores it AFTER save.  Same pattern as
the §5.20 DharmaMitra Tibetan nested body.

`*** Reference Translations' auto-fill (which used to pull
external PDF text from Resources) retired — Carsten now pastes
reference translations manually, and the new preservation
machinery keeps them verbatim across regenerate.  Paragraph-
mode files (`par-NNN.org') still get top-level `* Reference
Translations' from
`tibetan-analysis-create-paragraph-file' (unchanged).

#### Deferred (per AskUserQuestion during plan mode)

Two specific improvements were explicitly deferred to keep the
batch layout-only:

- **Zero-marker (Ø) function improvements in the particle
  skeleton.**  The current `Ø zero-marked' rendering correctly
  marks unmarked clausal positions but the heuristic for which
  positions to mark is hand-tuned and occasionally surfaces a
  spurious `Ø' on transitive subjects.  Layout is correct;
  body needs work in a follow-up.
- **Detailed Dictionary per-entry rendering polish.**  The
  multi-source block from §5.3 is correct but visually dense.
  Improvement deferred — moving the section to the bottom
  (Commit 5) is the layout-level win;  per-entry tuning is a
  separate polish pass.

#### Suite trajectory

1929 (post-§5.20) → 1938 (Commit 1, +2 -1 retire) → 1938
(Commit 2, +3 -3 retire) → 1938 (Commit 3, +2 updated) →
1942 (Commit 4, +4) → 1944 (Commit 5, +2) → **1947** (Commit 6,
+3) / 1946 expected / 0 unexpected / 1 skipped.  Plus BDD
specs 244 / 244 unchanged.

#### Verification recipe

After Commit 7 lands:

1. `cd /Users/cp/tibetan-cat.el && make test` (or the batch
   command from §4) → 1947 / 1946 expected / 0 unexpected /
   1 skipped.  `make compile` clean (no new warnings).

2. **Live regenerate.**  On the Tibetisch IV / Milarepa
   analysis folder:
   - `M-x tibetan-analysis-batch-reanalyze` with
     `:re-request-claude nil` (preserve Claude content;  the
     new layout takes effect, Claude content lands in the new
     slots).
   - Spot-check seg-049:
     - `*** CAT Gloss' heading absent.
     - `** Claude Vocabulary' heading present, level 2, AFTER
       `** Interlinear Gloss', BEFORE `** Translation'.
     - `*** Particles' heading present;  `*** Particle Map'
       and `*** Particles in This Segment' absent.
     - Particle bullet for `nas' shows `[Bialek 2022 §2.11
       (V+nas); Portfolio §2.11]'.
     - `** Provided Translations' heading present but EMPTY
       body (or carrying just Carsten's hand-paste, no auto-
       children).
     - `** Detailed Dictionary' is the LAST `** ' heading
       inside `* Tibetan Analysis'.

3. **User-content preservation test.**  Paste a reference
   translation into `** Provided Translations' on a seg file,
   re-run `C-c u R' on that segment, confirm paste survives.

4. **Export safety.**  `M-x org-html-export-to-html' on a
   regen'd seg file succeeds (the §5.18 dangling-term-link
   strip + the §5.21 layout integration both fire).

5. **Idempotency.**  Re-run `batch-reanalyze' a second time;
   diff should be empty modulo `:LAST_ANALYZED:' timestamps.

Yogācārabhūmi explicitly deferred per Carsten's "for the
moment only Milarepa is urgent" comment.

#### Files changed (across all 7 commits)

| Path | Role |
|------|------|
| `persist/tibetan-analysis-persist.el` | Renderer, regenerate-auto, scaffold, priority section order, particle skeleton + bullets + grammar section, detailed dictionary placement, provided translations nested read/write |
| `persist/tibetan-analysis-claude.el` | Claude section-order (Vocabulary → level 2), `--migrate-legacy-claude-headings' Step 4, `--insert-claude-vocabulary-heading' helper, `--merge-claude-vocabulary' retired, `--place-claude-grammar-heading' updated for merged `*** Particles' |
| `philology/tibetan-bialek-textbook-refs.el` (NEW) | Defconst alist + `tibetan-bialek-textbook-ref' lookup helper |
| `test/tibetan-analysis-persist-test.el` | +14 ERT specs across the batch |
| `test/tibetan-analysis-claude-sections-test.el` | Claude tests updated for level-2 Vocabulary + retired merge |
| `test/tibetan-analysis-claude-prompt-test.el` | CAT-Gloss prompt test retired |
| `tibetan-cat.el` | `(require 'tibetan-bialek-textbook-refs nil t)' |
| `CLAUDE.md` | §5.21 added |

### 5.22 Compressed sentence-analysis layout for class reading (done, 2026-05-21)

Class-use feedback the day after §5.21 shipped:  the per-sentence
analysis files (`sent-NNN.org') inherit the full per-segment
renderer output (~11 level-2 sections inside `* Tibetan
Analysis').  In class, the reference sections (Detailed
Dictionary, Verb Classification, Sentence Structure) are prep-
time tools, not in-class tools.

Carsten:  *"the full layout isn't necessary for the sentence,
only for the segments… max 2 pages A4 per sentence with the
really important information for the sentence esp. the
structure (with converbs)."*

Solution evolved over the day:

1. **Initial (Commits 1-5):**  opt-in compressed layout via per-
   source `#+TIBETAN_SENTENCE_COMPRESSED: t' header + `C-c u z C'
   toggle command + dynamic var + metadata-reader key.  Matched
   the §5.12 / §5.14 per-source-header pattern.
2. **Drop DD always (Commit 6):**  even without the header, sentence
   files balloon when DD lists 20+ entries for a multi-segment
   sentence.  DD became always-stripped from sentence files.
3. **Final (Commit 7):**  retire the opt-in entirely — sentence
   files are unconditionally compressed.  The toggle + header +
   dynamic var + metadata key are removed.  Per-segment seg-NNN.org
   files keep the full §5.21 layout.

Net result:  sentence files render the in-class compressed shape
by default;  segment files keep the full prep-time shape.  Clean
split of class-reading vs deep-analysis surfaces.

#### Target on-disk layout (sent-NNN.org)

```
* My Notes                            user-edited (preserved)
* Working Translation                 user-edited (preserved)
* Tibetan Text                        raw Tibetan (full sentence)
* Tibetan Analysis
  ** Claude Vocabulary                ← KEPT (Claude per-word)
  ** Translation                      ← KEPT (Claude)
  ** Grammar                          ← KEPT (prose + Particles)
    *** Particles                     legend + skeleton + bullets
                                        with [Bialek 2022 §X.Y;
                                        Portfolio §A.B] refs
                                        (the converb-rich
                                        structure section)
    *** Claude Grammar                Claude's prose reading
  ** Provided Translations            ← KEPT (user-content slot)
    *** Roehrich                      hand-paste of published English
    *** Class Translation             working class translation
    *** Claude Context                discourse-level Claude reading
* Footnotes                           user-edited (preserved)
```

Compare to FULL §5.21 sent-NNN.org:  11 level-2 sections inside
`* Tibetan Analysis' → 4 always.

DROPPED in sentence files (unconditional):  `** Wylie
Transliteration', `** Phonetics', `** Interlinear Gloss', `**
DharmaMitra Translation', `** Sentence Structure', `** Verb
Classification (Hill 2010)', `** Detailed Dictionary'.

Per-segment `seg-NNN.org' files are UNAFFECTED — segment scaffold
keeps the full §5.21 layout.

#### Activation

None needed.  Sentence files are unconditionally compressed.
The `C-c u z' prefix loses one command (`C-c u z C' freed up
for future use);  per-source `#+TIBETAN_SENTENCE_COMPRESSED:'
header is ignored if present (backwards-tolerant).

#### Commits (7)

| Commit | Subject                                                                  | Tests |
|--------|--------------------------------------------------------------------------|-------|
| 1      | `--read-source-metadata' extension:  `:sentence-compressed' plist key    | +1    |
| 2      | Dynamic strip-list:  defconst → defun accessor + new compressed-strip-list| +3   |
| 3      | Wire flag through `--scaffold' (used by both `--create-file' + `--regenerate')| +2 |
| 4      | `tibetan-sentence-toggle-source-compressed' + `C-c u z C' + menu         | +1    |
| 5      | CLAUDE.md §5.22 initial + verification                                   | docs  |
| 6      | Always-strip Detailed Dictionary from sentence files (full + compressed) | +1, ±3 |
| 7      | Retire opt-in machinery;  compressed becomes unconditional default       | −3, ±4|

Commits 1-5 set up the opt-in;  Commit 6 broadened the strip;
Commit 7 retired the opt-in and made the strip unconditional.
A pragmatic but slightly long path — kept here in the §5.22
entry rather than fragmented across §5.22 + §5.23 because the
whole arc is one logical batch (one day's iteration on class-
reading sentence files).

#### Mechanism (post-Commit 7)

The §5.18 sentence alignment already had a stripper hook
(`tibetan-sentence--strip-segment-claude-sections') reading from
a `defconst' filter list (empty since §5.18 alignment landed).
§5.22 final keeps that mechanism, simplified:

  · `tibetan-sentence--strip-list' (defconst, 7 entries):
    Wylie / Phonetics / Interlinear / DharmaMitra Translation /
    Sentence Structure / Verb Classification (Hill 2010) /
    Detailed Dictionary.
  · `--segment-claude-sections' accessor returns the list
    unconditionally.

No dynamic var, no metadata-reader key, no header, no toggle.

Sentence-only L3 extras (Roehrich / Class Translation / Claude
Context) survive the strip:  the filter operates on level-2
headings, and `** Provided Translations' is NOT in the strip
list, so `--inject-sentence-l3-entries' continues to attach
them under that parent.

#### Verification recipe

1. **ERT**:  `make test-quick' → 1955 / 1954 expected / 0
   unexpected / 1 skipped.  `make test' (BDD) stays 244 / 244.
   `make compile' clean.

2. **Live spot-check** on Milarepa source:
   - `M-x tibetan-sentence-batch-reanalyze' with
     `:re-request-claude nil' on the Tibetisch IV analysis
     folder.
   - Open any sent-NNN.org:  inside `* Tibetan Analysis' only
     `** Claude Vocabulary', `** Translation', `** Grammar', `**
     Provided Translations' — the 7 reference sections absent.
     Roehrich / Class Translation / Claude Context still under
     Provided Translations.

3. **Segment-file regression** (no behavioural change expected):
   pick a seg-NNN.org file, confirm the full §5.21 layout
   (Wylie / Phonetics / Interlinear / Vocabulary / Translation /
   DM Translation / Grammar / Sentence Structure / Verb
   Classification / Provided Translations / Detailed
   Dictionary) is intact.

4. **Idempotency**:  re-run batch-reanalyse a second time;  diff
   should be empty modulo `:LAST_ANALYZED:' timestamps.

#### Files changed (across the 7 commits)

| Path | Role |
|------|------|
| `persist/tibetan-analysis-claude.el` | `:sentence-compressed' plist key (added Commit 1, retired Commit 7) |
| `persist/tibetan-sentence-persist.el` | `tibetan-sentence--strip-list' defconst (Commit 7 consolidation of the intermediate accessor + strip-list pair from Commits 2+6);  `--scaffold' uses the unconditional strip |
| `config/tibetan-keybindings.el` | `C-c u z C' binding (added Commit 4, retired Commit 7) |
| `config/tibetan-menu.el` | "Toggle Compressed Sentence Layout" entry (added Commit 4, retired Commit 7) |
| `test/tibetan-analysis-claude-prompt-test.el` | Metadata-reader spec (added Commit 1, flipped to absence guard in Commit 7) |
| `test/tibetan-sentence-persist-test.el` | ERT specs across the arc:  +6 in Commits 2-4, ±4 / -3 in Commit 7 |
| `CLAUDE.md` | §5.22 added Commit 5, reframed Commit 7 |

#### TOC + section-numbering disabled on export (follow-up, 2026-05-21)

Independent of the sentence-layout work:  all three analysis-
file scaffolds (`tibetan-analysis-create-file' for segment files,
`tibetan-analysis-create-paragraph-file' for paragraph files,
`tibetan-sentence--scaffold' for sentence files) now emit
`#+OPTIONS: toc:nil num:nil' right after `#+STARTUP: showall'.
HTML / PDF / LaTeX export of any analysis file no longer
includes an auto-generated table of contents or numbered
section prefixes (`1. My Notes' / `1.1. …').

### 5.23 Per-source filename suffix for analysis files (done, 2026-05-22)

Class-use feedback (2026-05-22):  the MA Reading folder shares
one `analysis/` between four source documents (`gal-chen-nyi-
shu.org', `lam-rim-thun-cig.org', `title-colophon.org', `reading-
companion.org').  Each source numbers segments from 1 — so
`Segment 6' exists in BOTH gal-chen AND lam-rim AND title-
colophon → all three wanted to write `seg-006.org' in the same
folder → second writer silently overwrote first.

Visible damage:  on inspection 2026-05-22, gal-chen had 22 seg
analysis files (segs 0-21), lam-rim had 17 seg files (numbered
22-38 — its TRUE segs 22-38), and lam-rim's segs 1-21 +
title-colophon's segs 1-2 were entirely MISSING (silent
overwrite by gal-chen, which was created first).

#### Root cause

`tibetan-analysis-make-short-name' (line 549, infrastructure
existed) derives a short-name suffix from any source file
\(`gal-chen-nyi-shu.org' → `gal',  `Milarepa-prepared.org' →
`milarepa', etc.).  `tibetan-analysis-get-filepath' emits
`seg-NNN-SHORT.org' when given a source-file argument, plain
`seg-NNN.org' when not.

`tibetan-analysis-create-file' (line 659) called `(tibetan-
analysis-get-filepath seg-id)' WITHOUT source-file → wrote to
the UNSUFFIXED path.  Per §5.8.3 (2026-04-20), this was a
deliberate fix for the inverse bug (skip-check used suffixed,
create wrote unsuffixed → every C-c u B overwrote existing
seg-NNN.org silently).  The §5.8.3 fix locked both paths to the
UNSUFFIXED form, which worked for single-source folders
(Milarepa) but broke multi-source folders.

Sentence files (`sent-NNN.org') already used the suffix in some
paths — saw `sent-001-lam.org' alongside `sent-001.org' in the
MA Reading folder — confirming the infrastructure works.

#### Fix — pass source-file in BOTH create-path AND skip-check

Restoring §5.8.3's symmetry but in the OTHER direction:  both
sides now produce / look at the SUFFIXED path.  Three call-sites
updated:

  · `tibetan-analysis-create-file' (line 659)         — the writer.
  · `tibetan-auto-analyze-document' (auto-analysis    — the batch
    line 403)                                            skip-check.
  · `tibetan-open-segment-analysis' (line 4040)       — interactive
                                                         `C-c u A'.
  · `tibetan--reanalyze-segment-impl' (line 4219)     — interactive
                                                         `C-c u R'.
                                                         Backwards-
                                                         compat:
                                                         falls back
                                                         to UNSUFFIXED
                                                         when
                                                         SUFFIXED
                                                         absent.

Going forward, ALL new analysis files carry the per-source
suffix.  Multi-source folders are collision-free.

#### Migration helper

`tibetan-analysis-migrate-suffix-in-folder' (autoloaded) walks
FOLDER, for each unsuffixed `seg-NNN.org' / `sent-NNN.org' reads
the `#+SOURCE:' header to derive the source-doc path, computes
short-name, and renames in place.  Idempotent:  already-suffixed
files filtered by directory regex;  orphan files without
`#+SOURCE:' skipped;  target-already-exists conflict detected
and skipped rather than clobbered.

Returns plist summary `(:folder F :total N :renamed N-ren
:skipped N-skip :failed N-bad :results RESULTS)'.  Interactive
callers get a final `message'.

#### Migrations run 2026-05-22

  · Yogācārabhūmi (`gotrapatala.org', single source):  96
    files renamed to `seg-NNN-gotrapat.org' /
    `sent-NNN-gotrapat.org'.  3 skipped (Emacs `.org~' backup
    files, correctly excluded by regex).
  · MA Readings (multi-source, 4 sources):  39 files renamed.
    1 skipped (an orphan `sent-001.org' without `#+SOURCE:'
    link — predates the auto-creator; user can hand-rename or
    delete).
  · Milarepa (Tibetisch IV):  NOT migrated (single-source,
    no collision urgency).  User can run the migration command
    when convenient;  `--reanalyze-segment-impl' backwards-compat
    fallback handles UNSUFFIXED files until then.

After MA Reading migration:  ran `tibetan-auto-analyze-document'
on `lam-rim-thun-cig.org' + `title-colophon.org' which created
the previously-overwritten 23 missing analysis files (21 lam-rim
segs 1-21 + 2 title-colophon segs 1-2).  Then fired Claude+DM
for the 23 newly-created files via `:re-request-claude
:missing-only' (yesterday's §5.22-follow-up policy) — only the
empty files got API calls;  the 58 already-populated files
were correctly skipped.  All 23 now carry real Claude
Vocabulary / Translation + DM Translation.

#### Commits (3)

| Commit | Subject |
|---|---|
| 1c8332d | Code fix:  always suffix seg/sent filenames |
| 1c99d90 | Migration helper for unsuffixed analysis filenames |
| (this)  | CLAUDE.md §5.23 entry + final tally |

#### Tests (+2)

  · `create-file-emits-suffixed-filename' — asserts the
    SUFFIXED filename is written;  legacy UNSUFFIXED file is NOT.
  · `migrate-suffix-in-folder-renames' — temp-folder fixture
    with two unsuffixed files (different sources), one
    already-suffixed, one orphan;  asserts:  2 renamed, 1 orphan
    skipped, already-suffixed untouched, new names exist, old
    names gone.

Suite total:  1958 → 1960 / 1959 expected / 0 unexpected / 1
skipped.  `make compile' clean.

#### Files changed

| Path | Role |
|------|------|
| `persist/tibetan-analysis-persist.el` | `create-file' threads source-file;  `open-segment-analysis' + `reanalyze-segment-impl' wrappers; `migrate-suffix-in-folder' command |
| `persist/tibetan-auto-analysis.el` | Skip-check threads source-file (preserves §5.8.3 symmetry) |
| `test/tibetan-analysis-persist-test.el` | +2 ERT specs |
| `CLAUDE.md` | §5.23 added |

### 5.24 `** Concept Notes' replaces `*** Claude Context' (done, 2026-05-24)

Class-use feedback after the MA Reading class (2026-05-24).
Carsten:  *"Claude Context isn't that useful at the moment.  In
the class I got the idea, that we should reuse this for
additional information about the segment/sentence if there is a
Buddhist concept in the text that I get more information about
it."*

The pre-§5.24 `*** Claude Context' surfaced narrative-arc /
discourse-position information — useful occasionally but not
the high-value class artefact.  Higher-value:  Buddhist +
Sanskrit technical concepts (doctrine, doxography, lineage and
person and place names, category-lists like the five aggregates).
Claude-provided encyclopedia notes that keep students in the
text rather than breaking flow to look terms up.

#### What changed

| Aspect | Before | After |
|---|---|---|
| **Heading** (segment) | `*** Claude Context' (L3 inside Provided Translations) | `** Concept Notes' (L2, after DharmaMitra Translation, before Grammar) |
| **Heading** (sentence) | `*** Claude Context' (L3 inside nested Provided Translations) | `** Concept Notes' (L2, segment-renderer-emitted, kept in §5.22 compressed strip) |
| **Claude prompt section** | `## Context` — discourse-position narration | `## Concept Notes` — 0-3 technical-concept encyclopedia notes |
| **Sub-concept handling** | n/a | 1-sentence parenthetical gloss inline (e.g. "one of the twelve dhutaguṇas (the canonical list of ascetic disciplines)") — one level of recursion |
| **Empty-passage behaviour** | Claude wrote something even when narratively thin | Explicit `[No notable concepts in this passage]` placeholder |
| **Elisp key** | `:context` | `:concepts` |
| **Heading name flexibility** | Buddhist-only ("Claude Context") | Neutral — accommodates non-Buddhist terms (clans, places, lineages) |

#### Migration

Existing files migrate naturally on first regenerate post-§5.24:

  · `--migrate-legacy-claude-headings' Step 5 (segment layout) /
    Step 6 (sentence layout) renames `*** Claude Context' →
    `** Concept Notes' (segment) or `*** Concept Notes' (sentence),
    preserving body verbatim.
  · Reader's fallback chain checks `** Concept Notes' L2 → `***
    Concept Notes' L3 → legacy `*** Claude Context' L3.  First
    non-nil wins, so an already-migrated file's L2 body takes
    precedence over any stale L3 remnant.
  · Parser `## Concept Notes` and legacy `## Context' both route
    to the `:concepts' plist slot.

Preserved bodies carry the OLD narrative-arc content from the
pre-§5.24 prompt;  the user re-fires Claude (`C-c u R' or
`:re-request-claude :missing-only' batch) to get fresh
concept-notes content in place.

#### Sentence renderer changes

  · `tibetan-sentence--scaffold-sentence-only-l3-entries' drops
    `Claude Context' from the L3 inject list — the renamed
    `** Concept Notes' lives at L2 in both segment and sentence
    files (single source of truth, no duplication).
  · Sentence regenerate's preserved-body reader extended for
    `** Concept Notes' L2 + `*** Concept Notes' L3 + legacy
    `*** Claude Context' L3.  Writer uses the L2 path
    (`--set-l2-body-in-buffer').

#### Commits (2)

| Commit  | Subject |
|---------|---|
| f6b3f67 | Code + tests:  heading rename, prompt rewrite, migration, parser/reader/writer all aligned |
| (this)  | CLAUDE.md §5.24 entry |

#### Verification

  · `make test`        → 244 / 244 BDD specs pass.
  · `make test-quick'  → 1965 / 1964 expected / 0 unexpected /
                         1 intentional skip.
  · `make compile`     → clean (no NEW warnings).

#### Pending follow-ups

User raised two extensions during the class-reading review;
the first ships in §5.25 (below);  the second has no concrete
proposal yet:

1. **Thesaurus zettel cross-link** — shipped as §5.25 (next
   subsection).
2. (none open)

### 5.25 Thesaurus zettel cross-link in Concept Notes (done, 2026-05-24)

Carsten's follow-up to §5.24's `** Concept Notes':  *"should
also always check if there is a Zettel for this concept
available."*  Where the user has a curated thesaurus zettel for
a term that Claude discusses, the Concept Notes body should
link to it directly — let the reader jump from the in-passage
explanation to the zettel's full content.

#### Architecture choice

User picked option **(C) Both — prompt-cite + post-process
safety net** from a three-way AskUserQuestion.  The two paths
work together:

  1. **Pre-load (in-prompt)**:  before each Claude call, walk
     the passage's tokenised words, look each up in the
     thesaurus, inject the matches into the user prompt with
     instructions to cite as `[[id:ZID][zettel ↗]]'.  Claude
     does the semantic matching — it knows which zettel
     applies to the term *as used* in this context.

  2. **Post-process (writer-side safety net)**:  after Claude
     returns the Concept Notes body, scan bolded Tibetan terms
     and auto-append `[[id:ZID][zettel ↗]]' to any line that
     matches a zettel but didn't get cited in Claude's output.
     Lines that already carry a link are skipped (idempotent).

The two paths give robustness through overlap:  Claude usually
cites correctly in-prompt;  the post-process catches the
occasional miss.

#### Three new helpers (`persist/tibetan-analysis-claude.el')

| Helper | Returns |
|---|---|
| `--collect-zettel-references (tibetan-text)' | Deduplicated list of zettel plists matching tokens in `tibetan-text` (via `tibetan-thesaurus-lookup', Wylie key) |
| `--format-zettel-references-block (zettels)' | User-prompt-injectable text block — header instructing Claude how to cite + one bullet per zettel (Wylie / ZID / Skt. / EN-or-DE primary) |
| `--cross-link-zettels-in-body (body)' | Post-processed copy of `body` with `[[id:ZID][zettel ↗]]' appended to bolded-term lines that have a zettel but no link yet.  Idempotent. |

All three are no-ops when the thesaurus module isn't loaded —
silent fall-through preserves the no-zettel UX.

#### Wiring

  · `--build-claude-prompts' (segment) — injects the zettel
    block right before "Produce the five sections now."
  · `tibetan-sentence--build-claude-prompts' — same injection,
    guarded by `fboundp' so the sentence module loads cleanly
    without the analysis-claude module.
  · `--insert-claude-sections' — pre-write step post-processes
    the `:concepts' plist slot through `--cross-link-zettels-in-
    body'.

End-to-end flow:
  1. Prompt builder collects relevant zettels.
  2. User prompt carries the zettel block.
  3. Claude emits Concept Notes with inline `[[id:ZID][zettel ↗]]'
     citations.
  4. Writer's post-process catches any zettel Claude missed,
     auto-appends the link.
  5. On-disk `** Concept Notes' carries clickable cross-links
     to every relevant zettel.

#### Verification

  · `make test'        → 244 / 244 BDD specs pass.
  · `make test-quick'  → 1971 / 1970 expected / 0 unexpected /
                         1 intentional skip (compound-
                         analysis-callable).
  · `make compile'     → clean (pre-existing warnings only).

#### Pending (truly open now)

  · (none for the Concept Notes thread)

### 5.26 BUG: `:missing-only` wiped Claude content + restore (done, 2026-05-26)

Class-day discovery — Carsten ran `M-x tibetan-analysis-batch-
reanalyze' on the Milarepa folder on 2026-05-24 with the default
`:missing-only' answer (from the §5.22-follow-up three-way
prompt), expecting populated files to be SKIPPED.  Result on
inspection 2026-05-26 (today, just before class):  **274 of 287
seg-NNN.org files lost their Claude content**.  Pure silent
data loss.

#### Root cause

`tibetan-analysis-reanalyze-file' had the gate:

    (existing-sections
     (and (not re-request-claude)
          (tibetan-analysis--read-claude-sections filepath)))

When `re-request-claude' is `:missing-only', `(not :missing-only)'
is nil → `existing-sections' is nil → the file's populated Claude
content is NEVER READ INTO MEMORY before the regenerate-auto
write.  regenerate-auto overwrites the file with the fresh
scaffold → content gone.  `:missing-only' then sees an empty
file and queues a Claude refire.  Async queue (concurrency 3
over 284 files) + writer hooks / rate limits / network +
user-quits-Emacs all conspire to lose most responses.

The `--should-fire-claude-p' policy added in §5.22-follow-up
gated the API CALL correctly, but didn't gate the WIPE.

#### Fix

One-line gate change:

    (and (not (eq re-request-claude t))
         (tibetan-analysis--read-claude-sections filepath))

Now `nil' AND `:missing-only' both preserve;  only literal `t'
(explicit force-refresh) wipes.  Matches the user's stated
expectation:  *"if Claude/DharmaMitra are in the analyses it
should stay there except a renew is explicit requested."*

Also extended `has-any-section' to look at `:concepts' (the
§5.24 rename of `:context') and `:vocabulary' — both are
populated-Claude indicators that should trigger the restore
path.  Previously a file with only Vocabulary (no Translation
yet) would have been treated as "no Claude content" and wiped
even with the gate fixed.

Regression test (`tibetan-batch-reanalyze-missing-only-
preserves-claude') verified to FAIL without the gate fix, PASS
with it.  Locked in so this can't silently regress again.

#### Disaster recovery — restored from git

Carsten's `~/.../buddhist-studies/' folder is a git repo.  The
pre-wipe versions of all 284 Milarepa seg-NNN.org files were
preserved in commit `bc5cbe9' (the original `SS26: bring
semester coursework into git' commit).  Built a one-shot Elisp
script (`/tmp/restore-milarepa-claude.el', not checked in;  it's
a one-time tool):

  1. For each unsuffixed seg-NNN.org in the Milarepa analysis/
     folder, check if it has the `[Awaiting Claude…]' / `[Requesting
     translation...]' placeholder.
  2. If yes, fetch the git-blob version from `bc5cbe9'.
  3. Parse the git version's Claude bodies via `--read-claude-
     sections' (which handles legacy `*** Claude Translation' L2
     / `*** Claude Vocabulary' L3 / `** Claude Grammar' L2 / etc.
     thanks to its fallback chain).
  4. If the parsed plist has any populated slot, call
     `--restore-claude-sections' to write the bodies back into
     the current (new-§5.24-layout) file.

Result:  **267 files restored** from git blobs (zero API
spend);  12 were already populated (skipped — restore is
idempotent);  3 had no Claude content even in the git version
(true bottom of the well);  2 not in git (recently-created files
that postdated the `bc5cbe9' commit).

Post-restore tally:  **280 / 287 seg files have populated
Claude Vocabulary + Translation**.  The 7 still empty are split
between the 3 that never had content + the 2 not-in-git + 2
edge cases (seg-IDs from non-Milarepa source debris in the
folder).  Effectively complete recovery for class.

#### Commits

| Commit  | Subject |
|---------|---|
| a66b8a7 | BUG: regenerate-auto preserves content under :missing-only too (gate fix + regression test) |
| (this)  | CLAUDE.md §5.26 entry |

#### Verification

  · `make test'       → 244 / 244 BDD specs pass.
  · `make test-quick' → 1972 / 1971 expected / 0 unexpected /
                        1 intentional skip.  +1 regression test.
  · `make compile'    → clean.

#### Lessons captured

1. **`(not X)' is treacherous when X is a symbol that's not just
   `nil' / `t'.**  `:missing-only' is non-nil — `(not :missing-
   only)' returns nil, NOT t.  When the legacy code path was
   "preserve unless force-refresh", that should have been
   `(not (eq X t))', not `(not X)'.

2. **Policy gates should be CONSISTENT across the pipeline.**
   §5.22-follow-up added `:missing-only' semantics at the
   API-call site but left the wipe-decision-site with the old
   `(not re-request-claude)' check.  Two policy gates with
   different semantics ate the content.

3. **Per-file restore from git is FAR cheaper than re-firing
   Claude.**  267 API calls saved by checking
   `git show bc5cbe9:analysis/seg-NNN.org' before queueing.

### 5.27 Unified document-preparation wizard (done, 2026-05-27)

**Problem.** Preparing a new Tibetan document for class analysis
required six+ separate commands across two tooling surfaces (CLI
+ Emacs):  decide input route (OCR / Wylie / Tibetan-script
paste), shell out to `doc-prep/tibetan-ybh-prep.py` if Wylie, mv
the result into `work in progress/`, hand-edit the
`#+TIBETAN_TARGET_LANG:` / `#+TIBETAN_TEXT_TYPE:` /
`#+TIBETAN_CLAUDE_CONTEXT:` headers, run
`tibetan-add-sentence-structure`, run `C-c u B`, run
`tibetan-sentence-create-all` if reading-class.  No single
entry-point, no genre help, no Claude pre-fill of metadata.

**Decision.** One guided wizard exposing every decision as a
prompt, with the Phase 1-5 building blocks composing under the
hood.  `M-x tibetan-document-prep-wizard` (bound to `C-c o d`,
menu *Tibetan > Document Preparation > Document Wizard
(unified)...*).

**Architecture.** Seven phases, all shipped 2026-05-27:

  · **Phase 1** — Header plumbing.
    `tibetan-analysis--read-source-metadata' (in
    `persist/tibetan-analysis-claude.el') gains four new keys —
    `:text-type', `:class-mode', `:sentence-detail', `:author' —
    parsing the matching `#+TIBETAN_*:' single-line headers.
    `:author' falls back to the PROPERTIES drawer's `:AUTHOR:'
    entry for backwards compatibility.  See commit `5710e64'.

  · **Phase 2** — Wylie ingest wrapper.
    `doc-prep/tibetan-wylie-ingest.el' shells out to
    `tibetan-ybh-prep.py' via `call-process'.  Pre-flight gates
    check `python3' + `pyewts' + script presence;  validation
    helper (`--validate-input', interactive variant
    `-interactive') reports paragraph + segment counts and flags
    suspicious characters (embedded Tibetan Unicode, smart
    quotes / em-dashes) before conversion.  See commits
    `9e03d5f' (wrapper) and `fcd8379' (post-ingest relocation
    into `../work in progress/' — defcustom
    `tibetan-doc-prep-work-in-progress-folder', default
    `"work in progress"').

  · **Phase 3** — Genre taxonomy + selectbox.
    `doc-prep/tibetan-document-genres.el' carries a single
    defconst with 14 entries — 12 traditional Tibetan genres
    (rNam thar, Lam rim, mGur, mDo, rGyud, bsTan bcos, sNyan
    ngag, gTam rgyud, 'Grel pa, Ṭīkā, gTer ma, gDams ngag) + 2
    legacy keys (`classical' / `madhyamaka-verse').  Each entry
    carries `:tibetan' (Wylie name), `:label' (display),
    `:description', and `:claude-hint' (genre-tailored Claude
    system-prompt guidance).  Reader
    `tibetan-document-genres-read &optional default prompt'
    runs `completing-read';  vertico / marginalia honour the
    alist-mapped completion table.  See commit `682f093'.

  · **Phase 4** — Async Claude metadata pre-fill.
    `doc-prep/tibetan-document-prep-claude.el' fires a small
    Claude call early in the wizard, asking for `{genre,
    author, context}' as a JSON object.  Submitted via
    `tibetan-claude-queue-submit' so throttle / retry / rate-
    limit handling matches the rest of the pipeline.  Parser
    tolerates code-fence wrapping;  unknown genre keys collapse
    to `:genre nil';  `"unknown"' author (case-insensitive)
    collapses to `:author nil'.  Result is cached as a buffer-
    local plist `tibetan-document-prep--claude-suggestions';
    interactive applier
    `tibetan-document-prep-apply-claude-suggestions' (bound
    `C-c o D') writes the headers and reverts the buffer.  See
    commit `d0431d6'.

  · **Phase 5** — Sentence-detail switch.  When the source
    carries `#+TIBETAN_SENTENCE_DETAIL: detailed', the §5.22
    7-entry strip-list is suppressed and `sent-NNN.org' files
    carry the FULL §5.21 segment layout (all 11 L2 sections).
    Wired via the dynamic var
    `tibetan-sentence--detail-for-render', bound inside
    `tibetan-sentence--scaffold' from the source-file's
    metadata so both create-file AND regenerate paths inherit
    the preference.  See commit `b2347da'.

  · **Phase 6** — Wizard module.
    `doc-prep/tibetan-document-prep-wizard.el' — pure
    orchestration walking twelve steps:  input route → (Wylie)
    validate + convert + relocate → target language → fire async
    Claude → genre selectbox (Claude-suggested default) →
    author + context (Claude-suggested defaults) → class mode →
    (reading) sentence detail → optional Resources/vocabulary.org
    scaffold → optional `tibetan-add-sentence-structure' →
    optional `tibetan-auto-analyze-document' + (reading)
    `tibetan-sentence-create-all' → final summary buffer.
    Resume support is built in:  every step reads existing
    header as completing-read default, so re-running on a
    partially-prepared source just re-confirms each value.  See
    commit `15fee53'.

  · **Phase 7** — Menu + keybindings + docs.  Menu entries
    *Document Wizard (unified)...* + *Apply Claude Metadata
    Suggestions* land at the top of *Tibetan > Document
    Preparation*;  the legacy OCR wizard is renamed *OCR /
    Format Wizard...* one slot below.  Keymap `C-c o' gains
    `d' (unified wizard), `D' (apply Claude suggestions),
    `y' (Wylie ingest), `Y' (Wylie validate-only);  the legacy
    `C-c o o' (OCR wizard) stays for backwards compatibility.

**Verification.** ERT suite 1976 → 2081 specs (+105) all green;
single pre-existing skip unchanged.  Compile clean.  Sanskrit-
included documents are deliberately out of scope — users enable
the parallel-Sanskrit path via the existing `C-c u z P' toggle.

**User-facing recipe** (a Wylie source for the SS26 Tibetisch
IV class):

```
~/buddhist-studies/SS26/Tibetisch IV/raw/foo.org   ← Wylie input
M-x tibetan-document-prep-wizard      (or C-c o d)
  Input route:    Wylie file (.org with Wylie body) ...
  Wylie source:   ~/.../raw/foo.org
  → validates, converts via pyewts, relocates to
    ~/buddhist-studies/SS26/Tibetisch IV/work in progress/foo.org
  Target language: de
  → async Claude metadata call fires
  Genre:           (Claude-suggested default or `classical')
  Author:          (Claude-suggested or freeform)
  Context line:    (Claude-suggested or freeform, RET to finish)
  Class mode:      grammar | reading
  Sentence detail: compressed | detailed   (reading mode only)
  Create Resources/vocabulary.org? y
  Run sentence-structure demotion? y
  Run full Claude analysis now? y/n
  → final summary buffer
```

When the async Claude call returns post-wizard:
`M-x tibetan-document-prep-apply-claude-suggestions` (`C-c o D`)
opens the source file and writes the cached suggestions into
the metadata headers.

### 5.28 Opus 4.8 full-codebase audit + fixes (done, 2026-06-01)

A comprehensive audit of the whole tree (correctness / security /
design / dead code / test gaps), fanned out over parallel reviewers,
followed by a fix pass.  Every fix was regression-test-first and is its
own commit.  Suite 2107 → **2119** ERT (BDD 244/244); full `make
compile` is now clean (zero warnings).

**Two CRITICALs:**
- **No TLS verification on outbound HTTPS** (`tibetan-dharmamitra-api`,
  `tibetan-mitra-translation`).  `url-retrieve-synchronously` ran with
  Emacs' default `gnutls-verify-error' = nil, so a forged/MITM cert did
  not abort — the bearer token was sent and the response trusted over an
  unverified channel.  Fixed by binding `gnutls-verify-error' t +
  `network-security-level' 'high.  **Follow-up bug (also fixed):** the
  bindings were LEXICAL under byte-compilation (the special vars weren't
  declared), so the enforcement was silently inert in `.elc` —
  `(require 'gnutls)`/`(require 'nsm)` makes them genuinely special.
  Caught by `make compile`'s unused-lexical warning; now tested against
  compiled code.
- **Sentence regenerate wiped parallel-mode top-level sections**
  (`* Sanskrit Analysis' / `* Combined Analysis' / DM-Sanskrit /
  realign).  The §5.26 data-loss class on the un-patched sentence path —
  latent (no current parallel-Sanskrit corpus uses sentence files) but
  real.  `tibetan-sentence--regenerate' now preserves + re-appends them
  (idempotent), mirroring the segment path.

**HIGH:** dead `C-c u D`/`C-c u W` bindings (void-function) removed;
`make test' fixed to run BOTH ERT + BDD (its `-f' named a non-existent
function and only worked by accident) and made network-free (the BDD
run hit dharmamitra.org live); `tibetan-auto--claude-needs-request-p'
taught the post-§5.18 `** Translation' heading + `[Awaiting' placeholder
(it was failing closed-as-populated, skipping real refires);
§5.23 suffix threaded through the reorg-rename and DM-realign-apply
paths (both wrote bare seg-NNN.org → multi-source collision); DM /
realign responses sanitised before org insertion (line-leading `*' →
heading injection; `:reason' newlines → forged property-drawer
entries); leftover `DEBUG' messages stripped from `C-c u I'.

**MEDIUM:** ནས/ལས ablative no longer over-matches lexemes like གནས;
`tibetan-clause-segment' tolerates malformed VERBS (the §5.9/P5
`wrong-type-argument listp' crash class); zero-marker analysis stops
excluding root-final-ས content words (སེམས/ལུས); `tibetan-verb-lookup'
trims a trailing tsheg; Steinert SQLite handle released on
`kill-emacs-hook'; SSE parser tolerates `data:{…}' (no space); workspace
defvars no longer wipe ox-latex's class defaults; wizard seeds class
mode from the existing header on non-Wylie resume (was silently flipping
grammar→reading); `#+TIBETAN_CLAUDE_CONTEXT:' append now dedups.

**Two audit findings DISPROVEN on inspection** (recorded so they aren't
re-investigated): the "German leaks into the English gloss slot"
case-fold claim — `tibetan-extract-english-from-bilingual' /
`-format-bilingual-meaning' are output-invariant, the predicates are
dead but harmless; and the reading-mode adjacent-`||' "bug" — it is an
intentional, test-locked design choice (`||' no-space = segment
boundary even in reading mode; the spaced `| |' is the within-paragraph
double-shad).

**Cleanup:** removed grep-confirmed dead code (`--get-word-info',
`tibetan-detect-verb-with-suffix' + its defconsts — which also resolved
a `tibetan-converb-particles' name collision with the live
clause-analysis copy —, a dead `(and … nil)' clause, redundant
string-suffix guards, an empty banner, the `test-tibetan-functions.el'
scratch file); added a run-all-tests.el visibility guard that warns
about any `*-test.el' that fails to load (the `(condition-case nil …
(error nil))' wiring silently swallows such failures); fixed a
byte-compile forward-ref + a Python invalid-escape warning.

**Deliberately NOT done** (Carsten's call): the M12 shared
section-body-reader refactor; renaming `tibetan-analysis-combine.el'
\(corpus-stitch) to disambiguate from `combined.el'; wiring/relocating
the orphaned `setup/` modules.  Also left as enhancements, not bugs:
auth-source lookup for a private DharmaMitra token (the default token is
public), and the benign gptel sync-timeout reason-string race.

### 5.29 BUG: DharmaMitra re-fired populated sections on open (done, 2026-06-02)

User report: "a lot of DharmaMitra requests" — opening an already-
translated segment re-hit the DM API every time and overwrote the
existing translation.  Two compounding bugs (split into two commits):

- `tibetan-dharmamitra-translation-needs-request-p' searched the
  OBSOLETE top-level `* DharmaMitra Translation (Tibetan)' heading, but
  §5.20 moved the Tibetan section to the NESTED `** DharmaMitra
  Translation' that the writer fills.  Read != write location → always
  "needs request".  Fixed to read the nested heading (legacy top-level
  fallback; `[Awaiting…' / `[Requesting…' count as still-needed).
- `tibetan-dharmamitra-translation-fire-for-segment' fired the Tibetan
  path UNCONDITIONALLY.  Added `&optional force'; each language fires
  only when FORCE or its needs-request-p.  C-c u R passes FORCE=t;
  batch passes (eq re-request-claude t).  Implements the policy: a
  populated Claude/DM section is renewed only on explicit request.
  (Claude's on-open fire was already gated by --claude-needs-request-p.)

### 5.30 Opus 4.8 audit round 2 + fixes (done, 2026-06-02)

A second whole-tree audit (the §5.28 fixes were re-verified present).
Fixed, each regression-test-first, one commit each:

- **H2** wizard metadata loss: header writes went to disk via
  write-region while --fire-async-claude kept a stale visiting buffer;
  the kickoff save-buffer then clobbered the headers.  New
  `--edit-source' routes source mutations through the visiting buffer.
- **M1** `tibetan-reorg--collect-document-segments' matched only heading
  levels 2-3 → found ZERO segments in the canonical §2.12 nested layout
  (Sentence@3 / Segment@4); now level-agnostic.
- **M4** DharmaMitra dictionary lookup now negative-caches misses (was
  re-hitting the network on every miss).
- **M5** Steinert URL JSON built with json-encode (was a raw format
  string → malformed JSON for quote/backslash terms).
- **M6** Madhyamaka term match requires whole tsheg/shad-delimited units
  (was raw substring match → e.g. `ལམ' inside `ལམས'); compound-internal
  matches (`ལམ' in `ལམ་རིམ') remain an inherent-segmentation limitation.
- **M7** removed contradictory duplicate `བཙོང' verb-DB entry (the `ཚོང'
  alias pointed at the dead one).
- **M8** moved reorg's load-time global-set-key (C-c u O / C-c u U) into
  config/tibetan-keybindings.el; same for the duplicate C-c u P /
  C-c s w / C-c s p / C-c e p module bindings.
- **DEBUG** removed leftover message spam (workspace C-c s w; wylie
  safe-substring + a provably-dead BUG message).
- **Dead code** removed: module `analysis/tibetan-particles.el' (a
  Schwieger duplicate of -bialek), `tibetan-steinert-format-matches',
  `tibetan-extract-particle-text'.
- **ocr-validate** the audit's "zero coverage" was a FALSE POSITIVE
  (10 tests live in tibetan-doc-prep-test.el); added the two untested
  paths (subscript-start, suspicious) + tidied a redundant format.

**Deferred (need design / too risky for a cleanup pass — proposed as
follow-ups):**
- **H3** parallel-Sanskrit unscoped `** Translation' read: scoping the
  reader alone broke a test because the WRITER + migrate/ensure helpers
  also operate buffer-wide and round-trip the Sanskrit body.  A correct
  fix needs reader+writer+migrate scoped together (or the Sanskrit
  pipeline to use distinct heading names).  Latent — parallel-Sanskrit
  docs only (gotrapaṭala), not the normal workflow.  Reverted the
  partial change.
- **M2** `tibetan-verb-lookup' blind `ས$' strip: purely latent (the
  gethash guard prevents current misfires); a real fix needs noun-
  awareness.  No reproducible bug → left.
- **M9** `tibetan-auto--create-sentence-file' emits a legacy/orphan
  layout: reconciling it with the canonical sentence creator is a
  design task (the auto path doesn't collect per-sentence seg-nums);
  legacy files migrate on regenerate anyway.

### 5.31 Grammar-section improvements + sentence-structure feature (done, 2026-06-02)

Class-reading improvements to the per-segment + per-sentence analysis
files (planned in `~/.claude/plans/synthetic-stargazing-anchor.md`).
Five items, each test-first, one commit each:

- **A. Zero-marker fix** (`tibetan-analysis--render-particle-skeleton`):
  the persistent-file `Ø` is now stamped before a verb ONLY when the
  immediately-preceding token is a bare content word — suppressed when
  it is already markup (`=case=` / `~converb~` / `*final*`).  Fixes the
  §5.21-deferred "spurious Ø on an overt ergative subject".
- **B. Reading-optimized section order** (`--priority-section-order`):
  Sentence Structure elevated ABOVE Grammar; Concept Notes demoted to a
  reference block just above Detailed Dictionary.  Existing reorder
  post-pass migrates files on regenerate.
- **C. Hill verb stems** (`tibetan-verb-classifier`): a normalization
  pass auto-indexes every entry's present/past/future/imperative stems
  as lookup keys (8 were missing — e.g. སྨྲོས, ཐོབས, མཐོངས); new
  `tibetan-verb-matched-stem' tags WHICH stem appeared, surfaced in Verb
  Classification as `ATTESTED: <form> — perfect (past) stem'.
- **D. Subjects/objects + full sentence structure** (the new feature):
  `--render-clause-structure' now prints explicit SUBJECT / DIRECT
  OBJECT / oblique labels (new `--role-order' / `--role-labels' /
  `--role-label') with the full NP, ordered subject→object→oblique.
  Sentence files: `** Sentence Structure' DROPPED from
  `tibetan-sentence--strip-list' so the full per-clause structure
  survives; the main-clause-only summary (`--render-main-clause' /
  `--append-main-clause' / `--role-display' / `--main-clause-role-order')
  retired/removed.
- **E. Role-based highlighting**: new `tibetan-analysis-verb-face'
  (blue) + `-structure-role-face' (green-bold); the Sentence Structure
  role labels + clause verbs are font-locked (verified).  A Tibetan-
  script base layer (`--tibetan-script-matcher', a FUNCTION matcher —
  a `[ༀ-࿿]' char-range as a STRING in font-lock-keywords is silently
  dropped when other keywords are present) colours Tibetan runs;  under
  batch `font-lock-ensure' this base layer is inconsistent (jit-lock
  artifact) — **eyeball + tune colours live**.

NOTE: §5.21 layout (and now these changes on regenerate) apply to
Milarepa; Yogācārabhūmi is still pre-§5.21 — propagating is a regenerate
run (Carsten's call, and §5.29 governs Claude/DM re-firing).  The
verb-extractor not recognising some past stems in running text
(e.g. བལྟམས) is a separate data-coverage matter, not part of this batch.

### 5.32 seg-039 analysis-quality fixes (done, 2026-06-03)

Live-review of a regenerated Milarepa segment
(`རྔོག་གི་དྲུང་དུ་ཕྱིན་ནས།', "Segment 39") surfaced four GENERAL quality
bugs.  Each test-first, one commit (plan
`~/.claude/plans/synthetic-stargazing-anchor.md`).  Verified read-only
on the real segment — see file-naming NOTE below.

- **A. Wylie ra-mgo + nga stack** (`d24e1fa`,
  `core/tibetan-wylie.el`).  `རྔ` was absent from the consonant-stacks
  table → `རྔོག` (rNgog) rendered "raog", which cascaded to a junk
  `term-raog` anchor, a wrong dictionary key, and bad Phonetics
  ("rao").  Added `("རྔ" . "rng")` (2-char) + `("རྔྱ" . "rngy")`
  (3-char).  `རྔོག` → "rngog"; Phonetics auto-corrects → "ngok".

- **B. `ཕྱིན` = past of `འགྲོ`** (`2485344`,
  `analysis/tibetan-verb-classifier.el`).  `འགྲོ` ("to go") had
  `past_stem` "སོང", so the literary past `ཕྱིན` ("went") was
  unindexed → `Verb Classification: [No Hill-DB verbs detected]` and
  the §5.31-elevated `** Sentence Structure` was EMPTY.  Set
  `past_stem` → "ཕྱིན" (kept `imperative_stem` "སོང" + its alias).  The
  §5.31 stem-normalization pass auto-indexes ཕྱིན;  detection +
  "ATTESTED: ཕྱིན — perfect (past) stem" + a populated clause all work.

- **C. MWU verb-tail guard, BOTH loops** (`207d034` +
  `e05bd4a`).  The greedy MWU matchers accepted a Rangjung-Yeshe
  *phrasal* entry `དྲུང་དུ་ཕྱིན' that ENDS in a verb — gluing it into
  one unit, swallowing `ཕྱིན', and surfacing the RY entry's Tibetan
  EXAMPLE sentence (`bcom ldan 'das kyi drung du') as the gloss.  Two
  loops needed fixing: `tibetan-find-multiword-units`
  (`analysis/tibetan-enhanced-parser.el`, the clause/structure path)
  AND `tibetan-extract-vocabulary` (`core/tibetan-vocabulary.el`, the
  §5.9 Interlinear path).  New predicates
  `tibetan-enhanced-parser--verb-tail-p` /
  `tibetan-extract-vocab--tail-is-verb-p` (both reuse
  `tibetan-verb-lookup`);  guard rejects a ≥3-syll (parser) / ≥2-syll
  (interlinear) candidate whose tail is a Hill verb — UNLESS it is a
  user-curated Resources / Custom MWU (e.g. `ཆུང་མ་བྱེད' "to take a
  wife" stays a unit).  Interlinear now: `drung [beside] / du [TERM] /
  phyin [pf. of 'gro; to go]`.

- **D (deferred — dictionary-quality, not quick fixes).**  Two
  residuals visible on this segment are inherent lookup limitations,
  flagged not fixed: (1) `rngog [mane]` — rNgog is the proper name of
  Mila's teacher (rNgog ston), but RY glosses the common noun "mane /
  dewlap";  needs proper-noun awareness.  (2) `nas [ABL/CONV:nas]` —
  a particle shows its grammatical label (correct — no lexical sense)
  but the format is terse.  The general example-sentence-as-gloss
  filter (skip a leading Tibetan example in
  `tibetan-vocab--parse-entry`) is also still open;  C fixed the
  verb-tail case, but a non-verb-tail compound with an example-only
  `:primary` could still surface one.

**NOTE — file-naming mess in the Milarepa analysis folder.**  The
on-disk `seg-039.org` does NOT contain "Segment 39"
(`རྔོག་གི་དྲུང་དུ་ཕྱིན`) — that text lives in `seg-110.org` (and a
duplicate `seg-110-milarepa.org`).  The folder mixes bare `seg-NNN.org`
with §5.23-suffixed `seg-NNN-milarepa.org' duplicates AND iCloud
conflict copies (`seg-10492 2.org' etc.), so the seg-id ↔ segment
mapping is scrambled.  Cleaning this (de-dup + canonical naming) should
precede any corpus-wide re-regenerate so the fixes land on the right
files.  Verification above was done on copies in /tmp via
`tibetan-analysis-reanalyze-file … :re-request-claude nil` (preserve
mode, zero network) — the live corpus was NOT mutated.

Suite 2119 → **2125** / 2124 expected / 0 unexpected / 1 skipped.
`make compile` clean.  REFERENCE.org regenerated (`56a68d9`).

### 5.33 Milarepa folder cleanup + item D + corpus regenerate (done, 2026-06-03)

Follow-up to §5.32.  Two user decisions (AskUserQuestion): clean up the
analysis-folder file-naming FIRST, then tackle item D INCLUDING the
proper-noun gloss.  Then re-regenerate the corpus so all fixes land.

#### Folder cleanup — the seg-id↔segment scramble

The Milarepa `analysis/` folder had grown to **728 files**.  Root cause:
the §5.8.3 / §5.23 filename history left TWO numbering schemes coexisting
— the original bare `seg-NNN.org` (created 2026-04-14, correctly mapped
to the current 282-segment source) AND 123 `seg-NNN-milarepa.org`
duplicates from a 2026-06-02 regen whose `#+SOURCE'/`#+TITLE' labels were
written WRONG (e.g. `seg-110-milarepa.org' carried the rNgog text — real
Segment 110 — but was labelled "Segment 39").  Plus ~330 junk files
(`.org~' backups, `.bak*', garbage `seg-10491/10492.*', iCloud
conflict copies `seg-N N.org').

Audit method (all read-only): the source `Milarepa-prepared.org' has a
clean 282 segments (1..282, no dups).  Verified the bare files match the
CURRENT source by content (Segment 39 = `kho'i rtsar…', Segment 110 =
`rngog… phyin nas') — bare filename-num == source segment for all of
them.  Proved every one of the 123 `-milarepa' duplicates has a FULL
bare counterpart at the same filename-number → dropping them loses zero
content.

Action: moved **332 files** to a sibling `analysis-quarantine-2026-06-03/`
(with `MANIFEST.txt`; NOTHING deleted — fully reversible).  Folder now
holds exactly 282 bare `seg-001..282.org` + 85 `sent-001..085.org` +
legit `.tex/.pdf` exports + the pre-existing `archive/`.  Segments
**140, 141, 142** have no Claude content anywhere (need a refire).

#### Item D — both halves done (the §5.32 "D deferred" note is now closed)

- **D2 proper-noun gloss** (`1e830cc`).  The Claude-vocab override
  (`tibetan-analysis--apply-claude-vocab-override') only fired on Steinert
  `<person>'/`<place>' tags.  New `tibetan-analysis--claude-vocab-proper-
  noun-p' reads Claude's part-of-speech field (text before the quoted
  gloss, so a comma in the gloss can't trip it); when it says "proper",
  an UNTAGGED common-noun dict gloss is overridden with Claude's name.
  `རྔོག' → "mane" (Rangjung-Yeshe) now renders `[rNgog]' because the
  file's own Claude Vocabulary classifies it `proper noun, "rNgog"'.
  Fires on the regen path (preserved Claude vocab is bound into
  `--claude-vocabulary-for-render'); first-time generate is a no-op.
- **D1 example-sentence filter** (`17eacee`).  New `tibetan-vocab--mostly-
  tibetan-p' (Tibetan-block chars, zero Latin = a usage example, since
  real glosses are English/German).  `tibetan-vocab--parse-entry' skips
  such a first sense and falls back to the first `;'-separated sense with
  a Latin gloss.  General defence behind §5.32-C for the example-sentence-
  as-gloss class.

#### Corpus regenerate (preserve mode, zero network)

`tibetan-analysis-batch-reanalyze :re-request-claude nil` → **282/282
ok, 0 failed, all `:claude-preserved t'**.  `tibetan-sentence-batch-
reanalyze` → **85/85 ok**.  All §5.32 A/B/C + §5.33 D fixes are render-
time (D2 uses the preserved Claude vocab), so preserve mode lands them
without re-firing Claude.  Verified live on seg-110: `rngog gi drung du
phyin nas' Wylie, `[[term-rngog][rngog]] [rNgog] gi [GEN] drung [beside]
du [TERM] phyin [pf. of 'gro; to go] nas', ATTESTED past-stem ཕྱིན.
Backup of the clean pre-regen folder at
`/tmp/milarepa-analysis-backup-*.tar.gz'.

STILL OPEN: segments 140, 141, 142 need a Claude/DM fire (e.g.
`:re-request-claude :missing-only' or interactive `C-c u R' on each) —
they carry no content to preserve.

Suite 2125 → **2129** / 2128 expected / 0 unexpected / 1 skip.  Compile
clean.  REFERENCE.org regenerated (`0341768`).

### 5.34 phru rlog hallucination → grounding fix + Resources-load regression (done, 2026-06-03)

Live review of a (mislabeled) "Segment 37" file: Claude's Vocabulary
glossed `ཕྲུ་རློག' as "hand-mill turning" — a hallucination.  The
dictionary is unambiguous: Resources curated "Feldarbeit // farm work",
Steinert/Dan-Martin `sa zhing gsar pa rmos pa' (ploughing), Negi-Skt
`= rmo rko' (agriculture).  Diagnosis uncovered THREE things:

1. **File was mislabeled (already fixed by §5.33).**  The pasted file
   titled "Segment 37" actually held source **Segment 97**'s text — a
   pre-cleanup corrupted copy.  After §5.33 the content is correctly in
   `seg-097.org'; `seg-037.org' is the real Segment 37.

2. **Claude vocabulary prompt had NO dictionary grounding** (`4fbc7d3`).
   `--format-segment-vocabulary' read the §5.10-RETIRED `** Word /
   Particle List' section → nil for every current file → Claude guessed
   meanings for rare words.  Fix: new
   `tibetan-analysis--read-interlinear-glosses' extracts `wylie = gloss'
   pairs from `** Interlinear Gloss' (the layered lookup incl. ★
   Resources); `--format-segment-vocabulary' falls back to it and the
   instruction now says base the Vocabulary on these attested glosses,
   do NOT invent meanings for listed words.  Takes effect on the next
   Claude re-fire.

3. **Resources-load regression — corpus-wide silent wipe** (`747030a`).
   While verifying (2), found that the §5.33 preserve-mode batch
   regenerate had DROPPED every curated Resources gloss (0 files with
   `[Resources (provided)]', down from 8).  Root cause:
   `tibetan-find-resources-folder' resolves `Resources/' only via
   `buffer-file-name'; in a headless `emacs -batch' run
   `generate-content' executes in `*scratch*' (nil buffer-file-name) →
   finder returns nil → Resources never load → Interlinear/DD fall
   through to generic glosses.  Interactive `C-c u R' was unaffected
   (runs from the analysis buffer).  Fix: the finder falls back to
   `default-directory', and `reanalyze-file' binds `default-directory'
   to the analysis file's folder around `generate-content'.  A fresh
   preserve-mode batch then RESTORED Resources to **161 files** (★ +
   `[Resources (provided)]') — more than ever, since the
   buffer-file-name dependency had been quietly losing them in many
   prior generations too.  `seg-097' `phru rlog' → "farm work"; bonus
   `mar ★ [Mar pa]' (D2 proper-noun + Resources both live).

LESSON: any code reachable from a headless batch must not depend on
`buffer-file-name' for locating per-document assets — derive from the
file path / `default-directory' instead.

Corpus re-regenerated twice in preserve mode (282 seg + 85 sent, 0
failed, content preserved).  Segments 140-142 still need a Claude fire.
Suite 2129 → **2132** / 2131 expected / 0 unexpected / 1 skip.  Compile
clean.  REFERENCE.org regenerated.

### 5.35 Claude Vocabulary term highlighting (done, 2026-06-03)

Class-use request: the headwords in `** Claude Vocabulary' were hard to
recognise.  Each entry is `WYLIE-TERM, part-of-speech, "gloss",
commentary' — and because the term is Wylie (Latin script) the §5.31
Tibetan-script font-lock colourizer never touched it, so the headword
blended into the POS / gloss / commentary.

Fix: new face `tibetan-analysis-vocabulary-term-face' (bold teal) + a
section-bounded font-lock matcher `tibetan-analysis--vocab-term-matcher'
(gated by `tibetan-analysis--in-claude-vocab-section-p') that bolds the
run before the first comma, ONLY inside `** Claude Vocabulary'.
Identically shaped `term, …' lines in `*** Claude Particles' are left
alone.  Registered in `tibetan-analysis-setup-faces'; applies on the
next open/refresh of an analysis buffer (font-lock only — no file
rewrite, so the corpus needs no regenerate).  Suite 2132 → **2134**.

Matcher tested directly (font-lock-ensure in batch is unreliable —
§5.31); verified live on seg-097 that the `bla ma' term carries the
face.

### 5.36 Shad-boundary markers in word lists (done, 2026-06-03)

Class-use request (MA Reading): the section is read whole, but analysed
shad-by-shad.  An MA Reading `segment' often spans several shad (།)-
delimited units (34 of 88 files), and the Interlinear Gloss + Claude
Vocabulary listed every word with no indication of where each shad-unit
ended.  ("Later we should make sentence/verse/paragraph the main unit —
but for now just a marker.")

Render-time marker `──────── ། ────────' at each INTERNAL shad boundary
(a shad with content after it), in the Interlinear Gloss AND Claude
Vocabulary.  Placement is by the boundary word — the last syllable
before the shad:
- `tibetan-analysis--shad-boundary-words' — Wylie boundary words from the
  file's Tibetan Text.
- `--mark-shads-in-vocab' — marker after the entry whose term ends in the
  boundary word.
- `--mark-shads-in-interlinear' — marker spliced after that token's
  `[gloss]' in the flowing line.
- `--apply-shad-markers' (via `--mark-l2-section') rewrites both sections;
  wired into `reanalyze-file' after regenerate + Claude restore.

Idempotent (stripped + re-placed each pass), no-op on single-shad
segments (Milarepa untouched), render-time only (no Claude re-fire).
Approach was an AskUserQuestion pick: scope = Vocabulary + Interlinear,
glyph = rule-with-shad.  Suite 2134 → 2138.

**MA Reading application — markers-only, NOT a full regenerate.**  A
full preserve-mode batch over MA Reading WIPED 48 files' baked-in
`[Resources (provided)]' glosses, because the per-document Resources
`wordlist' is now an EMPTY §5.27 scaffold (`work in progress/Resources/
vocabulary.org' = template only) — regenerate re-derives Resources from
the current (empty) wordlist, so the old glosses (from a wordlist that
no longer exists) are not reproducible.  Recovery: restored the folder
from the pre-regen backup, then ran `--apply-shad-markers' on each file
directly (markers splice into the EXISTING Interlinear/Vocab text;  no
regenerate, Resources untouched).  Result: 31 marker files + 48 ★
Resources files, both intact.

CAVEAT for a future session:  **regenerating MA Reading will lose those
48 Resources glosses** until the wordlist is repopulated (or the glosses
are moved to thesaurus zettels).  Unlike §5.34 (a buffer-file-name bug),
this is missing source data, not a code bug — the §5.34 default-directory
fix is working;  there is simply no wordlist to load.

### 5.37 BUG: -milarepa duplicate files + filename↔label scramble (done, 2026-06-03)

Class-prep review surfaced three symptoms on the Milarepa folder: (a) a
lost `* Working Translation' "stopped here" marker, (b) Claude/DM
sections looking empty "so requests fire at every segment change", and
(c) `seg-112' showing "Segment 39" as its title.  Root-caused to ONE
code bug plus a found-not-lost note:

- **(a) NOT lost** — the "stopped here" marker is in the SOURCE
  `Milarepa-prepared.org' after Segment 79's `**** Working Translation'
  (the user reads it there, not in the analysis files).

- **(b)+(c) the §5.23 suffix-vs-unmigrated-Milarepa bug.**
  `tibetan-analysis-get-filepath' with a source-file ALWAYS returned the
  suffixed `seg-NNN-milarepa.org'.  Milarepa was deliberately left
  un-migrated (bare `seg-NNN.org'), so `C-c u B' / segment creation
  computed the suffixed name, did NOT find it, CREATED it as a duplicate
  of the bare file, and re-fired Claude + DM.  Result: 17 mislabeled
  twins (seg-097..113-milarepa.org, titles "Segment 37/38/39") + the
  spurious-request behaviour.  §5.23 had added a bare fallback to the
  REANALYZE path only; create / get-filepath never got it.

#### Fix (`2fe4715`)

`get-filepath' → `tibetan-analysis--resolve-filepath': prefer an
existing suffixed file, else an existing bare `seg-NNN.org' that belongs
to the SAME source (`--file-belongs-to-source-p', a `#+SOURCE' basename
check), else a new suffixed path.  The source-match guard keeps
multi-source folders collision-safe (a bare file from source A is never
reused for source B).  So Milarepa operations now target the bare
canonical files — no duplicates, no spurious refires — while MA Reading
stays fully suffixed.  +2 tests (2138 → 2140).

#### Cleanup (data, not repo)

- The user's `phru rlog … hallucination?' My-Notes line lived only in
  `seg-097-milarepa.org' → copied into bare `seg-097.org' first.
- 17 `-milarepa' duplicates moved to `analysis-quarantine-2026-06-03/'
  (content-verified identical to their bare twins beforehand).
- Final: 282 bare seg + 85 sent, 0 duplicates, 0 label mismatches
  (filename == #+TITLE == #+SOURCE for all 282), 0 empty Claude/DM.

NOTE: the duplicates were created by a real `C-c u B' run hitting the
bug (mtimes clustered today), not iCloud.  With the fix, re-running
auto-analyze on Milarepa no longer spawns twins.  If `-milarepa' files
ever reappear they are iCloud re-syncing the quarantine from another
device — delete them; the bare `seg-NNN.org' is canonical.

### 5.38 Fable-5 targeted audit + fix-all (done, 2026-06-10)

First audit on a new model generation (Fable 5), scoped to the
§5.31–§5.37 diff + a fresh-eyes persist/ data-safety pass.  Three
parallel reviewers; every finding probe-verified before fixing; all
fixes regression-test-first, one commit each.  Suite 2140 → **2159**.

**CRITICAL C1** — section-body bounds `^\*{1,N}[^*\n]` also matched
markdown-bold `**Clause 1:**` / org-emphasis `*sic*` body lines,
truncating every preserve-side reader at the first such line →
preserve-mode reanalyze destroyed the section tail (§5.26 class; LIVE
trigger: MA Reading sent-001.org's Claude Grammar, 110 of 2159 chars
read).  Fixed: all 8 bounds → stars-then-SPACE (`^\*{1,N} `); plus
Claude bodies now sanitised against line-leading `*` at insert (the
§5.28 DM sanitizer mirrored).

**HIGH** — H1: the §5.32 verb-tail guard fired at ≥2 syllables in the
Interlinear (ཡེ་ཤེས/རྣམ་ཤེས/ཉན་ཐོས split into junk) while parser
guarded ≥3 and DD not at all → three surfaces disagreed (seg-049
anchor class).  Now ≥3 in ALL THREE loops.  H2: regenerate-auto's
whitelist deleted unknown top-level sections (45 live sent-*-lam files
with legacy `* Translation`/`* Grammar Notes`) → preserve-by-default
via `--get-unknown-top-sections`.  H3: §5.35 vocab-term font-lock
matcher infinite-looped (Emacs freeze) on a final line without
trailing newline → mandatory-progress guard.  H4: both regenerate
paths discarded UNSAVED visiting-buffer edits → modified visit saved
before the preserve reads.

**MEDIUM** — M1: `--file-belongs-to-source-p` substring → exact
basename via `--file-source-basename`; resolver rejects a
positively-foreign suffixed file (short-name collisions).  M2:
proper-noun override exempt for curated/`<term>` glosses + exact key
only (bare མར no longer inherits "Mar pa").  M3+L6: shad markers —
boundaries now `(wylie . occurrence)` conses, per-boundary independent
matching, ༼…༽ stripped.  M4: role-nil NPs (GEN) listed again in
Sentence Structure.  M5: D1 fallback skips junk senses (page refs)
with a bracket-aware sense splitter.  M6: sentence regenerate
round-trips L2 `** Claude Vocabulary`.  M7: force-refresh (`t` and
C-c u R) restores existing Claude content until the async response
lands — the §5.26 amplifier closed on the explicit path too.

**LOW batch** — སོང labels as past (`past_stem_alt`); zettel-linked
tokens included in Claude grounding; trailing-whitespace headings
found by the preserve pass; vocab-term matcher tightened (entry shape
required, L1 headings close the section).  Deferred deliberately: the
zero-marker clause-initial case (linguistics display call — eyeball
live per §5.31).

**Corpus**: Milarepa re-rendered preserve-mode (282 seg + 85 sent, 0
failed) — ye shes back to one unit, rngog GEN listed, Resources
intact (162 ★ files).  MA Reading NOT regenerated (§5.36 caveat —
the 48 ★ Resources files would lose their glosses; the C1/H2 fixes
make a future regenerate safe for *structure*, but the missing
wordlist data still stands).

### 5.39 Verb-first (head-driven) sentence-structure redesign (done, 2026-06-10)

Carsten: *"my mistake at the beginning was concentrate on segments, not
on sentences resp. verbs.  The final verb have to be the main focus."*
Approved plan (AskUserQuestion): sentence-first computation, verb-headed
tree rendering, elided slots shown.  6 phases, 7 commits, all
test-first.  Suite 2159 → **2175**.

**Architecture** (new module `analysis/tibetan-sentence-tree.el` on top
of the kept clause-segmenter Parts A+B):
- `tibetan-frame--slot-specs`: the 8 Hill frames → ordered (ROLE CASES
  REQUIRED) slots; `tibetan-frame-fill-slots`: saturation (each slot
  once), agent = leftmost ERG, patient = bare NP nearest the verb,
  leftovers → adjuncts, unfilled required slots → elided.  GEN NPs
  attach as `:possessors`; dang-coordination merges.
- `tibetan-analyze-sentence`: root = FINAL clause's verb; V+TERM
  pattern (`byed du` before `bcug`) → nested :complements; other
  dependents → :converbs with Bialek labels; matrix-ERG-claiming rule
  fills the matrix agent from a complement's span.
- Renderer `tibetan-analysis--render-sentence-tree` replaces the flat
  `Clause N [main]` blocks: MAIN VERB root with frame, indented slots,
  `— [elided]` lines, `ADJUNCT — CASE`, nested POSSESSOR, `⤷ converb`
  chains.  Old Part C (`tibetan-build-argument-structure` + flat
  case→role map — the seg-37 `bla ma` = DIRECT OBJECT source) RETIRED.
- Sentence-first: `tibetan-sentence--sentence-for-segment` (inverse
  walker) + `--sentence-tree-for-segment` (per-child parse with
  offset-shifted concatenation — parsing joined text merges tokens
  across the shad at child borders) + `--render-sentence-slice`: head
  segment gets the full tree under a `(Sentence N — segments …)`
  header; dependent segments get their clause subtrees + `⤷ chains to
  MAIN VERB: X (Segment M)`.  Flat layouts / single-segment sentences
  fall back to segment-local trees.
- Prerequisite bug (Phase 2): standalone པས/བས after a NOUN is now
  nominaliser+ERG (`མར་པས` = Mar pa ERG), not a causal converb; the
  greedy compound-`Xར` TERM rule is gated off before པས/བས.  ནི is an
  NP boundary (copular sentences).

**Corpus**: Milarepa preserve-mode re-render (282 seg + 85 sent, 0
failed, 162 ★ Resources intact); seg-097 renders the textbook causative
tree (AGENT མར་པ, CAUSEE elided, COMPLEMENT VERB བྱེད); 60 segment
files carry chain notes.  MA Reading excluded (§5.36 caveat).

**Coverage batch (same day)**: scanned pre-shad verb positions across
Milarepa + KBGP and added **21 framed Hill entries** (འབྱུང འབྱོན འབུལ
ཟེར གནང གསོལ དགོས བགྱིད རྫོང བཤམ ལེན འབེབས ཁྱེར སྐུར ཡལ ཐིམ འཁྱམས
འཕྲོག ཁྲོ ཕྱར ཆེ དགའ).  Re-rendered Milarepa (282+85) and the KBGP
seg files (32, per-file — gal/lam/title untouched, 48 ★ intact).
Effect: seg-110's slice collapsed from 17 junk adjuncts to the clean
minimal tree; sent-021 now roots at འབྱུང with a chained འབྱོན clause.
`tibetan-round1-minimal-entry-tags-source` repointed (གསོལ promoted
to Hill; ཤོར is the closed-set exemplar now).

Residual noise on very long sentences remains coverage-driven —
further verbs can be added the same way (scan: pre-shad positions,
`tibetan-verb-lookup` misses, frequency-ranked).  Still deferred:
recursive complements, `འགྲོ` → "Abs-Term" upgrade, sentence-parse
cache.

### 5.40 Sentence-level Claude + DM calls, split per segment (done, 2026-06-10)

Carsten: sending only the segment wastes context — send the COMPLETE
SENTENCE to Claude/DM, split the response back per segment.  Approved:
Claude SELF-SEGMENTS (`### Segment N` subsections — deterministic
parse, no heuristics); DM gets the sentence and every child shows the
whole translation under a `(Sentence N — segments A–B)` label;
Milarepa re-fired afterwards.  6 phases, 6 commits.  Suite 2175 →
**2197**.

**New module `persist/tibetan-sentence-claude.el`**: splitter
(`--parse-response` → :translation-whole/:grammar-preamble/:concepts +
:per-segment + :missing), synthesizers feeding the unmodified
`--insert-claude-sections` (heading migration / C1 sanitizer / zettel
linking for free), sentence-first system addendum (whole-sentence
Translation preamble + per-segment subsections in Vocab/Grammar/
Particles, Grammar cross-clause preamble, sentence-level Concept
Notes), prompt builder (per-child enumeration + per-child Interlinear
grounding + children parser grounding + zettels; no ±2 block — the
sentence IS the context), in-flight dedup registry
(`clear-inflight` for crash recovery), dispatcher
`tibetan-analysis--fire-sentence-level` (nil = fall back per-segment:
flat layouts, single-seg sentences, parallel-Sanskrit, missing
children).  Landing gates per M7 (non-FORCE skips populated children);
missing subsections → visible `[Claude sentence response missing
Segment N…]` stub that counts as needs-request.

**Shared static system blocks**: `--claude-static-system-blocks`
extracted from the segment builder — both system prompts are
per-document constants by construction (two coexisting Anthropic
cache prefixes).

**DM**: `fire-tibetan-sentence` (one call, multi-write via the nested
writer, per-file §5.29 gating, label inside the body); scheduled via
`tibetan-dharmamitra-sentence-request-delay` (6.5s — 10/min limit);
the sent file's DM body now ROUND-TRIPS through sentence regenerate
(read-before/write-after — the §5.22 strip-list omission would
otherwise eat it, §5.26 class; regression test failed pre-fix).

**Wiring**: reanalyze-file, C-c u R, auto-analysis create loop, and
the sentence create/reanalyze sites all try the dispatcher first; a
claim suppresses BOTH per-segment fires; multi-seg sentences retire
the sent file's separate Claude call (one call feeds both file kinds).

**Milarepa re-fire (2026-06-10, complete)**: 57 multi-seg sentences
(17 + 40 resumed) + 28 single-seg segments + 18 per-segment fills for
sub-translations Claude skipped in the longest responses — all queue
jobs succeeded, 0 failed.  Final corpus: 0 placeholder translations
(3 apparent ones are REAL translations opening with editorial
brackets, e.g. "[He] spoke…"), 254 children with sentence-context
labels, 0 missing-segment stubs, 162 ★ Resources intact.  seg-097's
"hand-mill turning" hallucination is GONE (sentence context →
"farm work, work in the fields").  Pre-fire backups: buddhist-studies
git snapshot + `analysis-backup-pre-sentence-first/` (keep until
class-verified).

**Live-run lesson (fixed mid-operation)**: the response fan-out
originally ran INSIDE gptel's curl process sentinel — landing N
children + the auto-regen cascade there corrupted gptel's
process-buffer parsing ("Search failed: <boundary>") and the batch
emacs died mid-queue (exit 255; no content lost — M7 restore-first).
Fix: the callback only schedules the fan-out via `run-at-time 0`.
Corollary for drivers: drain loops must use `sit-for` (runs timers),
not `sleep-for` — the deferred fan-out and the DM stagger live on the
timer queue.  The resume driver keyed off the `(Sentence N —
segments…)' label to fire only not-yet-landed sentences.

Deferred: ±1 surrounding-sentence context; parallel-Sanskrit
sentence-first; response-truncation mitigation (visible as missing
stubs); the multi-child auto-regen cascade.

### 5.41 Class view — reference sections fold on open (done, 2026-06-10)

Class-use feedback: too much scrolling, lost overview (esp. MA
Readings — the worst file's Detailed Dictionary is 67% of 2153
lines).  Display-only fix (no regenerate — §5.36 blocks slimming
gal/lam structurally): analysis buffers now OPEN with the reference
sections folded to their heading lines (`tibetan-analysis-class-
folded-sections', default Detailed Dictionary + Verb Classification;
Sentence Structure deliberately stays visible).  `C-c u f' toggles
class view ↔ full view; TAB unfolds a single folded section; the
on-open behaviour is the `tibetan-analysis-class-view-on-open'
defcustom (default t).

Bonus bug fixed: the mode hook matched only bare `seg-NNN.org', so
suffixed MA files and sent files never got the §5.31/§5.35 faces.
New `--analysis-buffer-name-p' covers seg/sent with suffixes.

Deferred (per plan): `#+TIBETAN_SEGMENT_DETAIL: compressed' (blocked
by the §5.36 wordlist gap); combined-handout polish.

### 5.42 Whole-sentence Claude Translation + own-span highlight (done, 2026-06-11)

Class feedback on §5.40: the whole-sentence DM section is "much more
helpful" — same wanted for Claude, with the current segment
highlighted.  Segment files' `** Translation` now lands as:

    (Sentence 39 — segments 103–114)
    The guru told him… ⟪went to rNgog's place⟫ …and requested it.

    This segment: Having gone to rNgog's place,

Mechanism: the sentence-first addendum instructs Claude to wrap each
segment's span in the whole-sentence translation with `⟦N⟧…⟦/N⟧`
(deterministic — Claude does the alignment).  Fan-out renders per
child via `--render-marked-whole`: own pair → `⟪…⟫` (split per line —
font-lock is single-line), all other tokens stripped; ANY marker
irregularity degrades to the stripped plain whole (`⟦` can never
land).  Sent file gets the stripped whole.  New
`tibetan-analysis-segment-span-face` (background tint + bold) +
3-group font-lock keywords hide the ⟪⟫ delimiters via the namespaced
invisibility spec (Particle-Map pattern).  A `(Sentence …)`-opening
Translation counts as POPULATED (placeholder filters are
`[`-anchored; test-locked).  DM unchanged (free text — no reliable
alignment; whole-sentence display kept).

NOTE for future resume drivers: every child Translation now contains
`(Sentence` — the new-layout discriminator is `This segment:` inside
the `** Translation` body.

Milarepa re-fired with the new schema: 57/57 sentences succeeded,
0 failed, ZERO stragglers (the sentinel fix held) — 254/254 children
carry the new layout, 251 with a highlighted span (3 degraded
gracefully to the plain whole on marker irregularities), 0 raw `⟦`
anywhere, 162 ★ intact.

### 5.43 BUG: sentence analysis files weren't §5.23-suffixed (done, 2026-06-17)

Surfaced while preparing a NEW MA-Reading source (`Khu-dbon-rnam-thar`,
a rNam-thar excerpt, re-segmented verse-aware into 111 sentences / 790
segments) into the SHARED `analysis/' folder it co-inhabits with
`gal-chen' / `lam-rim' / `kbgp' / `title-colophon'.

**Root cause.** §5.23/§5.37 gave the SEGMENT path a per-source filename
suffix (`seg-NNN-SHORT.org') + a resolver that prefers an existing
suffixed/bare-same-source file, so multi-source folders don't collide.
The SENTENCE path never got it:  `tibetan-sentence--filepath' built a
bare `sent-NNN.org' unconditionally, so `tibetan-sentence-create-all'
(and 3 sibling callers) wrote `sent-NNN.org' — which would overwrite
another source's same-numbered sentence in the shared folder (the exact
§5.23 silent-overwrite class, just for sentences).  Lam-rim's
`sent-NNN-lam.org' files only exist because the §5.23 migration helper
renamed them AFTER the fact.

**Fix.** Generalized `tibetan-analysis--resolve-filepath' with an
optional PREFIX (default `"seg"'; existing seg callers unchanged), and
routed `tibetan-sentence--filepath' (now `(sent-num &optional folder
source-file)') through it with PREFIX `"sent"' when a SOURCE-FILE is
given — else the legacy bare path (backward compatible).  Threaded
source-file through all 4 sentence call sites (create-file, create-all,
open-analysis, reanalyze) — each already had it in scope.

**Tests.** +2 (`resolve-filepath-honors-prefix',
`sentence-filepath-suffixes-with-source'), both confirmed RED first
(`wrong-number-of-arguments').  11 existing sentence-persist tests
encoded the old bare contract and were updated to the suffixed
expectation (3 had a one-arg `--filepath' stub widened; 8 had path
lookups recomputed via the source-aware `--filepath').  Suite
2206 → **2208 ERT** (0 unexpected, 1 skip); BDD 244/244; compile clean.

**Live use.** Generated 790 `seg-NNN-khu.org' + 111 `sent-NNN-khu.org'
(suffixed, zero collision with the other four sources; structural only —
Claude/DM placeholders await the user's API).

**FOLLOW-UP — §5.40 dispatcher had the SAME bare-path bug (FIXED §5.44).**
`tibetan-analysis--fire-sentence-level'
(`tibetan-sentence-claude.el' ~605-611) builds child `seg-%03d.org' and
`sent-%03d.org' by hand, so in a SUFFIXED folder the
`(cl-every #'file-exists-p child-files)' gate fails → the sentence path
returns nil and the caller falls back to PER-SEGMENT Claude (no
whole-sentence fan-out, no §5.42 span highlight).  Works on Milarepa
(bare, single-source); broken for MA-Reading (suffixed).  Fix = route
child-files through `tibetan-analysis--resolve-filepath' + sent-file
through `tibetan-sentence--filepath' (both now suffix-aware).  Until
then, fill MA-Reading sentence files via the per-sentence
`tibetan-sentence--request-claude' (writes the whole-sentence Claude
translation straight to the suffixed `sent-NNN-SHORT.org' — class
artifact intact; only the seg-level fan-out is missing).

### 5.44 §5.40 dispatcher: suffix-aware child/sent paths (done, 2026-06-17)

The §5.43 follow-up.  `tibetan-analysis--fire-sentence-level'
(`tibetan-sentence-claude.el') built its child seg / sent paths by hand
with bare `seg-%03d.org' / `sent-%03d.org'.  In a §5.23-suffixed
multi-source folder those don't exist, so `(cl-every #'file-exists-p
child-files)' failed and the dispatcher returned nil — the caller then
ran PER-SEGMENT Claude (no whole-sentence fan-out, no §5.42 own-span
highlight).  Worked on Milarepa (bare, single-source) only.

**Fix.** Route child seg files through the now-prefix-generalized
`tibetan-analysis--resolve-filepath' (short-name from source-file) and
the sent file through the suffix-aware `tibetan-sentence--filepath'
(both §5.43).  Single-source bare folders are unaffected — the resolver
prefers an existing bare-same-source file, so Milarepa keeps bare names.

**Tests.** +1 (`tibetan-sentence-claude-fire-resolves-suffixed-children'
in `tibetan-sentence-claude-test.el'):  a 2-segment sentence with
SUFFIXED-only children must reach `'fired' (claim/request/schedule-dm
stubbed).  Confirmed RED first (returns nil — bare children absent).
`dispatch-dedup-and-fallbacks' updated: its scaffolded children now
carry a `#+SOURCE' link so the resolver reuses the bare names (a bare
file with no parseable `#+SOURCE' is treated as not-ours by the M1b
guard).  Suite 2208 → **2209 ERT** (0 unexpected, 1 skip); BDD 244/244;
compile clean.

**Net.** Multi-source reading folders (MA-Reading) now get the full
§5.40/§5.42 sentence-level fire — Claude fanned out to each
`seg-NNN-SHORT.org' + the sent file with the highlighted own-span, and
one staggered DM per sentence.  See `…/MA Readings Tibetan/Resources/
khu-fire-claude.el' for the per-source scoped driver.

### 5.45 Wylie: subjoined-ha (ལྷ lha) was dropped (done, 2026-06-18)

Live on Milarepa seg-167 (`ལྷོ་ཕུ་` rendered `lao phu` instead of `lho
phu`).  The Wylie converter (`tibetan-to-wylie-fixed' /
`core/tibetan-wylie.el') had no entry for subjoined ha (U+0FB7):  the
`subjoined' fallback map held only ya/ra/la/wa, and `ལྷ' was not in the
`consonant-stacks' table.  So `ལ' matched as a bare root, got a spurious
implicit-`a', and the ha was dropped → `ལྷོ' = "lao", `ལྷ' = "la", `ལྷག'
= "lag".

Fix:  added the ha-btags as STACKS (the proven mechanism — ya-btags
`ཀྱ' and la-btags `གླ' already live there;  the subjoined-map route
mis-places the implicit-`a', giving "laho").  `("ལྷ" . "lh")' plus the
Sanskrit aspirates `གྷ ཛྷ ཌྷ དྷ བྷ' (mantras/loanwords).  `ལྷོ' → "lho",
`ལྷ' → "lha", `དྷ' → "dha".  §5.32-A class (a missing consonant stack,
like `རྔ'→"raog").

Test:  `tibetan-wylie-subscript-ha' (`test/tibetan-wylie-test.el'),
confirmed RED first (`"la"' ≠ `"lha"').  Suite 2209 → **2210** (0
unexpected, 1 skip).  Existing analysis files keep the old "lao" until
regenerated;  any re-fire / reanalyze picks up the fix.

### 5.46 C0 = Part B Phase 0+1: always-sentence-first + prerequisite bug fixes (done, 2026-07-28)

First implementation slice of the approved CASCADE v2 plan
(`~/.claude/plans/idempotent-pondering-wozniak.md`) — the C0 stage
that unblocks BOTH the Portfolio P5 freeze+fire (Aug 9–10) and the
Khu-dbon sent-file backfill (Part B Phase 4).  Six commits, each
test-first with RED-verified failure modes:

- **B-0.1 strict glob** (`0a56056`).  One regexp builder
  `tibetan-analysis--analysis-file-re' (PREFIX-NNN[-SUFFIX].org —
  no spaces, no extra dots, fully anchored) + wrapper
  `--folder-analysis-files-strict' in tibetan-analysis-persist.el;
  all 7 folder-glob sites route through it, so iCloud conflict
  copies (`seg-005-lam 4.org') no longer trip batches.  The
  ±2-neighbor resolver's UNANCHORED `-' branch (which could feed a
  stale conflict copy into the Claude prompt) is anchored inline in
  tibetan-analysis-claude.el (that module sits BELOW persist in the
  require graph).
- **B-0.2 DM target-lang** (`64d4ac2`).  New
  `tibetan-dharmamitra-translation--target-lang' resolves
  `#+TIBETAN_TARGET_LANG:' (de→"german", else "english"; enum
  verified against the live DM OpenAPI spec) via the same
  source-resolution order as `--defer-mt-p'; threaded into
  fire-tibetan + fire-tibetan-sentence (the two leaves all callers
  delegate to).  NOTE for the Portfolio: after the freeze, DM output
  is now GERMAN for target-lang-de documents — the old "DM=EN,
  Claude=DE" division of labor needs an explicit header flip if the
  EN comparator is still wanted.
- **B-1.1 fire gate includes sent file** (`a44d7f7`).  Third gate
  clause: an EXISTING placeholder sent file opens the sentence-level
  fire; landing stays per-file gated, so a backfill fire fills ONLY
  the sent file and leaves populated children byte-identical.  A
  missing sent file deliberately does NOT open the gate.
- **B-1.2 singletons fire sentence-level** (`ba2ab80`).  The
  `>1'-segment dispatcher guard is gone (non-empty seg-nums
  suffices); `--segment-label' is singular-aware (`segment 136', not
  `segments 136–136') and the DM sentence fire delegates its label
  to it (fboundp-guarded); SENTENCE-FIRST addendum says "one or more
  numbered segments" (one-time sentence-level cache-prefix
  invalidation, schema unchanged).
- **B-1.3 call sites route singletons** (`f0b8ef3`).  The two
  `>1' conjuncts in tibetan-sentence-persist.el (open-analysis
  create branch, C-u reanalyze branch) dropped — the legacy
  `tibetan-sentence--request-claude' is now reachable ONLY as the
  dispatcher-nil fallback (flat layout / missing children).
- **BDD end-to-end** (`47b4a14`).  New suite
  spec/suites/sentence-first-spec.el (3 specs): full fan-out with
  ⟪span⟫ landing; the Khu-dbon backfill state (populated children
  byte-identical, sent file absorbs); gate-closed no-fire.  Only the
  process edges are stubbed (queue thunk synchronous, canned gptel
  :callback, immediate run-at-time).

PITFALL captured: `features' is NOT a special variable — in a
lexical-binding file, let-binding it silently creates a lexical
shadow that `featurep' never consults.  Provide/withdraw the feature
under unwind-protect instead.  (Cost one debugging round: a
dynamic-scope repro script masked the failure.)

Suite 2215 → **2226 ERT** (0 unexpected, 1 skip); BDD 244 → **247**;
`make compile' clean; REFERENCE.org regenerated per def-touching
commit.

Next per the plan's sequencing: Khu-dbon backfill (Phase 4,
operational — needs a git snapshot of buddhist-studies first) and the
Portfolio P5 freeze+fire are UNBLOCKED; cascade core C1–C4 follows.
The MA-Reading §5.36 caveat still stands: do NOT batch-regenerate MA
Reading until its wordlist is repopulated.

### 5.47 CASCADE core C1–C3 + dictionary D1 (done, 2026-07-28)

The cascade architecture is now LIVE end-to-end for opt-in documents
(`#+TIBETAN_LAYOUT: cascade`), alongside the untouched two-file model.
Eleven commits, all RED-first.  New module persist/tibetan-cascade.el.

- **C1** (`eed52d4`, `7f9aba2`): `:layout` metadata key +
  `tibetan-analysis--cascade-p` (mirrors --defer-mt-p; explicit
  header, never sniffed); pure `tibetan-cascade-split-shad-units`
  (concat-of-units == input, byte-for-byte; ། ༎ །། ། ། handled;
  shad-less text = ONE unit).
- **C2** (`b705a3d`, `d3a4071`, `5666381`): scaffold + create-file
  (sent-NNN-SHORT.org name KEPT; `* Subsegments` keyed by GLOBAL
  segment numbers, :SUBSEG: ordinal display-only; per-unit
  Rendering/Wylie/Phonetics/Interlinear/Particles from one
  segment-renderer pass per unit); subtree I/O primitives (byte-
  identity-locked single-section writes; needs-request gates on
  machine prefixes only — `[He] spoke…` is a real span, §5.40
  lesson); regenerate with preservation (user slots, populated
  Claude/DM bodies, populated Renderings, UNKNOWN L1 sections
  verbatim — §5.38-H2; idempotent modulo LAST_ANALYZED).
- **C3** (`83f5dc2`, `ab3bd6f`, `aeeb5a2`): ⟦N⟧ span extraction
  (malformed → nil → visible stub); landing = sentence-level
  sections via --insert-claude-sections (Vocab/Grammar/Particles
  keep their ### Segment subsections; sub-TRANSLATIONS DISCARDED —
  the rendering is the extracted span of the whole, so subsegment ⊂
  sentence BY CONSTRUCTION; schema unchanged → same prompts, stable
  cache prefixes); dispatcher branches on cascade-p into
  `tibetan-cascade--fire-sentence` (gate: sentence needs OR any
  Rendering placeholder; claim/queue/DM reused — DM targets the
  cascade file's own nested slot); prompt grounding from the cascade
  file's OWN subsegment Interlinear sections; BDD end-to-end spec
  (one artifact, no seg files, spans landed, second fire declines).
  Two-file paths byte-identical: extracted unchanged into
  --handle-response-1 / --fire-sentence-level-2.
- **D1** (`e3e509f`, `f7ffed0`, `a1c66e8`): dead COMPOUND-AWARE
  subtree removed (190 lines, zero callers);
  `tibetan-vocab-lookup-detailed` now delegates to the ranked
  multisource assembler (ONE ranking authority — the old chain's
  Steinert preference resolved in HITS ORDER, and Bundled outranked
  Steinert; verb detection/tokenizer now see the same gloss as the
  Interlinear); the two NON-delegated paths carry contract notes
  (--mwu-exists-p = strict existence probe, seg-049 guard;
  tibetan-dictionary-priority = bilingual walk-all + the only Hill
  verb-DB consumer; D2 will feed both from dictionary-sources.json).

**C4 dispatch/UX (same day, `c7a1d81` `15e46f4` `3195d05`)**: all
user-facing entry points branch on the layout — C-c u B / C-c s N →
`tibetan-cascade-create-all` (one file per sentence, auto-fire with
the queue throttling; `--fire-sentence` carries its own P1 defer-MT
guard as a leaf fire point); C-c u A at-Segment AND at-Sentence →
`tibetan-cascade-open-for-segment` (creates when missing, point on
the `** Segment N` subtree; composite seg labels from the nested-
layout detector tolerated); C-c u R → `tibetan-cascade-reanalyze-
for-segment`; and the BATCH SAFETY guard: `tibetan-sentence-
reanalyze-file` routes files that THEMSELVES carry `#+TIBETAN_LAYOUT:
cascade` (`tibetan-cascade-file-p`) to the cascade regenerate —
RED-verified that the two-file sentence regenerate would otherwise
DESTROY the * Subsegments tree on a C-c u r folder batch.  Legacy
bodies extracted verbatim (`-two-file` suffix) at every branch.

**C5 recognition layer (same day, `a70cc11` `37630c6` `33e31c6`
`b9c1876` `8281a1c`)**: Phase 2b — a shad is a HARD token boundary
at all five splitter sites (the bialek sites were DELETING shads,
actively gluing the flanking syllables — the seg-137 'khulgnyis
class); `tibetan-shad-split-segments` one-time source transform
(refuses on two-file corpora — renumbering is the §5.26 hazard);
Phase 2.1+2.2 — Section genre detection ([verse]/[prose] tag or
:GENRE_PART:) + the pure verse grouper, GOLD-standard-tested against
the hand-grouped Khu-dbon Sentences 10–11: verse needs its OWN
closer set (the -o family + ཞེས/ཅེས/ཟེར + mid-line དེ་སྐད་ཟེར; the
prose final-particle list would break at ཡིན/ཡོད/གྱུར/བཞུགས/གསུང
five times across the gold span), frame OPENERS do not close, and
the 8-line cap (defcustom) does the heavy lifting; Phase 2.3+2.4 —
genre-aware `tibetan-add-sentence-structure` with a HARD sentence
break at every Section line (the cross-section converb leak is
RED-verified fixed) and re-run ergonomics (offers reset instead of
the silent zero-segments no-op); Phase 3 — Hill-DB weak→strong
promotion, corpus-evaluated BEFORE committing (Milarepa 16/58,
Khu-dbon 5/150 promoted, ~2.4%, no over-segmentation) with a
tunable stop-list (ཆེ ཆེས འདྲ དགའ — the last protects proper names
like ཐོས་པ་དགའ; ཚར is a Hill coverage gap, addable per §5.39).

**C7 Rgyan §-layer (2026-07-29, `cc8778f` `7f6a759`)**:
`tibetan-cascade-import-comparative` turns the GENERATOR-OWNED
comparative document into a hand-owned cascade CAT source (Lopez
anchors carried; copyrighted Lopez/W&M and the Wylie NEVER copied —
locked by test; refuses to overwrite);  `#+TIBETAN_SECTION_REFS:` +
`tibetan-cascade--section-refs-block` inject the enclosing Lopez §'s
reference translations into the USER prompt at request time (system
prompt stays byte-constant — cache prefix untouched, locked by
test).

**Operational runs (2026-07-29, corpus commit `e228b62`)** — the
user moved Portfolio drafting to AFTER the cascade, so the Portfolio
became BORN-CASCADE instead of the planned post-Aug-24 C8 migration:
32 wizard-era analysis files (verified ZERO user content) quarantined
to `analysis-pre-cascade-2026-07-29/`; source got the cascade header;
shad-split 8 → 54 units; 8 cascade sent files created (54
subsegments, all 53 ★ wordlist glosses in the per-unit Interlinears,
ZERO API calls — the defer-MT guard held).  Rgyan:
`Rgyan_167-186-cat.org` imported (20 §§) + shad-split → 173 units;
sentence grouping (C-c s S) and the first C-c u B are the USER's
next moves there.

Suite: **ERT 2250** (0 unexpected, 1 skip) + **BDD 248**; compile
clean.  REMAINING from the cascade plan: C6 migration tool —
deferred until an actually-drafted two-file corpus needs converting
(the Portfolio no longer does; Milarepa/Khu-dbon/MA Reading stay
two-file by decision); Part B Phase 5 stretch — deferred; Khu-dbon
sent-file backfill — unblocked since C0, operational, needs the
user's go for API spend.  Also 2026-07-28: the buddhist-studies
corpus is LOCAL now (`/Users/cp/buddhist-studies`, git/GitHub sync —
iCloud had been planting conflict copies inside .git itself).

### 5.48 Chunk integration — section-sized AI calls (done, 2026-07-29)

Carsten's insight: AI translations improve with more text; send big
chunks, split afterwards.  The sentence-first pipeline IS that idea
at sentence scale; this scales it to SECTIONS.  Three commits
(`f9cb2d0` `4565fe5` + docs), all RED-first.

- **CH1 DM probe findings** (live, recorded in the plan): DM has NO
  context-only channel — a preceding chat message is TRANSLATED too
  (two-message ≈ concatenation).  But the chunk effect is REAL: the
  isolated sentence detached ("who could there be…"); the passage
  version resolved anaphora + kept terminology consistent.  2s per §;
  `explain-grammar` (4s) / `deep-research` (17s) work, optional.
  Backend is vertex-gemini regardless of the model field.
- **CH2 Claude chunk-fire**: --collect-section-chunks (groups by
  Section; implicit chunk without one; splits past
  tibetan-cascade-chunk-max-segments = 40 — §5.40 showed output
  LENGTH is the reliability ceiling); CHUNK MODE system addendum
  (third coexisting cache prefix) demands ONLY `## Translation` with
  every segment's ⟦N⟧ span — small output by design;
  --land-chunk-response: per subsegment the extracted span (missing →
  stub → the C3 sentence dispatcher is the AUTOMATIC fallback), per
  sentence the marker-bounded SLICE of the whole (keeps connective
  English between own segments), labeled `(Sentence N — §… chunk)`;
  --fire-section (defer-MT-guarded leaf) + `chunk' request flavor in
  --request/--handle-response (claim key = (chunk . FIRST-SENT)
  cons); UX `tibetan-cascade-fire-sections` (prefix = FORCE).
  Vocabulary/Grammar/Particles stay per-sentence.
- **CH3 DM section fire**: ONE chat-translate per section, whole
  translation into every member file's nested DM slot under
  `(Section §N)` — per-file §5.29 gating, target-lang aware,
  defer-MT guarded, ~10× friendlier to the 10/min limit; scheduled
  via the §5.40 stagger from --fire-section.  Two-file corpora keep
  sentence-level DM.

**LIVE EVALUATION + FLIP (2026-07-29, `d43b954` `5fd759b` `0e2f697`,
corpus `14f1daf`)**: Rgyan auto-grouped (20 sections → 56 sentences /
173 units; manual override remains the instrument), 56 cascade files
created; §167 chunk-fired with real API: **9/9 spans, 0 stubs, 3/3
sentence Translations** — coherent German honoring target-lang and
the square-bracket conventions.  The evaluation surfaced a REAL BUG:
DM's backend (vertex-gemini) streams SSE that url.el cannot consume
\(200 OK + empty body; `stream:false` unimplemented server-side) —
EVERY DharmaMitra call had been silently failing.  Fix: `--http-post`
transports via curl (body over stdin; TLS verified by curl defaults —
the §5.28 guarantee now test-locked on BOTH transports; url.el stays
as fallback).  After the fix the §167 DM section fire landed the
whole-§ German translation in all member files.  **Cascade auto-fire
is now CHUNKED** (create-all fires one section call per chunk; gates
skip populated members; missing spans fall back to the C3 sentence
dispatcher automatically).  NOTE (one dirty commit): `d43b954` went
in with one test red (the §5.28 TLS test, transport-shape-dependent);
fixed in the immediate follow-up `5fd759b` — recorded per the
suite-green-per-commit rule.

Suite: **ERT 2257** (0 unexpected, 1 skip) + **BDD 248**; compile
clean.

### 5.49 Defer-MT visibility + two batch data-hazards fixed (2026-08-10)

Commits `1fd2aef` `08a39f6` `ca20688` `d89cc47` (corpus `612abfe`
`e51f461`).  Trigger: Carsten did `C-c u A` on the Portfolio's first
sentence and read the generic `[Requesting translation...]` /
`[Awaiting DharmaMitra…]` placeholders as a FAILED request — but
`#+TIBETAN_DEFER_MT` was working exactly as designed (his own P1
protocol).  The tool never said WHY the slots were empty.

**Defer-MT visibility (1fd2aef + 08a39f6)**: new
`tibetan-analysis-defer-mt-placeholder` — emitted instead of the
generic placeholders by all three scaffold builders (cascade
scaffold, two-file segment `tibetan-analysis-create-file`, two-file
sentence `tibetan-sentence--scaffold`) via the shared
`tibetan-analysis--defer-mt-rewrite-placeholders` when
`--defer-mt-p`.  KEY DESIGN: the text keeps the `[Awaiting` prefix,
which every placeholder recognizer already treats as
needs-request — zero recognizer surgery; an ERT recognizer-lock
test pins the prefix contract.  Plus messages at the two
interactive moments: opening a defer-MT segment (`C-c u A`) and the
`C-c u R` decline (the guarded fire returns `deferred` silently —
it looked like a no-op failure).

**★-gloss loss on headless regenerate (ca20688)**: the V3 refresh
of the 8 deb files dropped ALL curated ★ wordlist glosses —
`tibetan-cascade--scaffold` ran its renderer calls with the
CALLER's `default-directory`, and `tibetan-find-resources-folder`'s
headless fallback IS `default-directory` (the 2026-06-03
corpus-wipe lesson).  Fix: `--scaffold` pins `default-directory` to
the source's directory.  Refresh re-run: ★ back at snapshot parity
(35), user slots byte-identical, defer text 8/8.

**Batch-deadlock root cause FOUND + FIXED (d89cc47)**: both Rgyan
hangs (the 29 h big-bang AND the sequential driver at §171) wedged
on the interactive supersession prompt (`…has changed since
visited or saved.  Save anyway?`) — blocking on stdin in batch.
Mechanism: mixed writer families on the same sent file — direct
disk writers (cascade span writer via `write-region`) crossing
buffer writers (DM + Claude landings via `find-file-noselect` +
`save-buffer`); the `(or (find-buffer-visiting …)
(find-file-noselect …))` pattern returns the stale buffer with no
modtime handling.  Fix: `tibetan-fresh-file-buffer`
(core/tibetan-utils.el) — visiting-buffer-first (find-file-noselect
has its own "Reread from disk?" prompt), silent revert when stale
AND unmodified, never touches unsaved edits; applied at the five
landing write sites (DM nested/toplevel, `--insert-claude-sections`,
`--restore-claude-sections`, failure stub).  The sequential
per-section driver remains the operational pattern for corpus-wide
fires; a big-bang batch may be re-validated now that the cause is
fixed, but do it on a small corpus first.

### 5.50 Wordlist authority — curated-first tokenization (2026-08-11)

Commits `828e803` `24b1055` `2985e67` `e7b0428` (corpus `9d03c15`).
Trigger: Carsten asked whether the analysis respects the provided
vocabulary list.  Audit answer: only 42 of 66 in-passage wordlist
terms surfaced, and ★ glosses showed English-only on a de-target
document.  Root causes and fixes, all RED-first:

- **W1 curated-first MWU grouping (`828e803`)**: the wordlist
  parsers store WYLIE keys (the Tibetan-key store branch is dead —
  `tibetan-wylie-to-tibetan` is not fbound), so the exact-Tibetan-key
  MWU probe could never license Resources-only compounds; greedy-
  longest grouping let generic compounds shadow curated terms
  (sprod pa "supply" over the class's sprod); the particle-tail
  guard rejected curated idioms (kog gis) before probing.  New
  strict `tibetan-vocab--curated-exact-entry` (Resources+Custom,
  exact Tibetan + exact Wylie key, NO stripping — seg-049 contract)
  + CURATED-FIRST pass (longest curated 4→1 wins at each position,
  guard-exempt) + no-swallow guard on the generic pass.  Empty-hash
  short-circuit ⇒ zero change for uncurated documents.
- **W2 cascade binds target-lang (`24b1055`)**: Pass-5c's DE//EN
  half selection reads dynamic `tibetan-analysis--target-lang`,
  which the cascade temp-buffer scaffold never bound (same
  missing-context class as the ★-loss cwd bug) — bound in
  `--scaffold` from source metadata; declared special in
  tibetan-cascade.el (the `features`-shadow lesson).
- **W3 particle homograph alternatives (`2985e67`)**: particle
  tokens whose Wylie is an exact curated key render
  `gyis [ERG ‖ ★ Imp. von bgyid…]` — the class's homograph reading
  was invisible behind the Bialek tag.
- **W4b splitter syllable-boundary + curated-whole guards
  (`e7b0428`)**: `--split-word-particle` split MID-SYLLABLE
  (བསྒྲུབས → བསྒྲུ + བས [CONV:pas]); syllabic particles now require a
  preceding tsheg (new `tibetan-interlinear--merged-clitics` འི འིས
  འང ར are the only letter-wise attachers), and exact curated keys
  (ltar = "als ob") are never split.

Portfolio regen result: **66/66** in-passage wordlist terms honored
(was 42), ★ 53 → 88, German halves shown, user slots byte-identical.
NOTE: Milarepa / Khu-dbon / MA Reading also carry Resources
wordlists — their tokenization follows the same curated-first rules
on NEXT regenerate (MA Reading batch regen stays prohibited, §5.36).

### 5.51 READING VIEW — cascade layout redesign (done, 2026-08-12)

Commits `126791c`…`1dff40f` + cleanup (corpus `8627b91`).  Carsten's
verdict on the per-segment sections: fragmenting.  Approved design
(AskUserQuestion): per-LAYER Reading view, ⟦N⟧ renderings inside it,
POS marking = verbs + main verb, Phonetics/per-segment sections
dropped.  Cascade files now read:

    * Tibetan Text (shads intact) → * Reading (** Wylie decorated,
    one line per shad unit / ** Interlinear / ** Renderings
    `- ⟦N⟧ …') → * Tibetan Analysis (Sentence Structure PER UNIT)

Key pieces: R1 `tibetan-segment-text' shads→tsheg (the missed
Phase-2b site — fused tokens + hallucinated main verbs in every
multi-clause tree); R2 `!x!' verb (blue) + `*x*' main-verb (red)
font-lock, the `*x*' matcher SECTION-GATED to ** Wylie under
* Reading (org-bold collision); R3 `analysis/tibetan-reading.el' —
token-wise decorated lines (curated-first streams; bare `ར' clitic
NEVER split — graphically ambiguous vs root-final ར, only འ-initial
clitics attach letter-wise); R5 dual-format rendering I/O (⟦N⟧ line
first, legacy `** Segment N' subtree fallback — the dual format IS
the migration); R6/R7 landing + grounding + regenerate on the dual
primitives; R8 scaffold flip with "Subsegments" kept in
--known-l1-sections as OWNED-LEGACY (dropped on regen, never
re-appended); R9 open-for-segment → the ⟦N⟧ line; R10 per-unit
Sentence Structure.  LESSON: the Reading gloss layer is
`** Interlinear' (not "Interlinear Gloss") — the Claude-machinery
anchors (ensure-claude-headings) key on bare L2 heading names
file-wide, and a colliding name pulled ** Claude Vocabulary into
* Reading.

MIGRATION (live, 2026-08-12): preserve-mode reanalyze of all 64
files (8 deb + 56 rgyan) after a scratchpad dry-run — 173/173
landed renderings verbatim on their ⟦N⟧ lines, user slots
byte-identical, defer texts intact, idempotent second pass, deb ★
88 → 175 (new layers carry their own stars).  Legacy READ
primitives kept (quarantines hold old-layout files);
--subsegment-block/--extract-particles-body retired.

Known display nuances (data, not machinery): Hill-DB gaps leave
some verbs unmarked (sgrigs → nas shows =nas= not ~nas~); wordlist
key coverage decides MWU grouping ('bre ston / mar pa render
per-syllable until curated).  Extending the class wordlist tunes
both.

**R12 follow-up (same day, `c41d1fc`, corpus `1a578e3`)**: Carsten
combined the two layers — ONE `** Interlinear' layer now carries
decorated Wylie + Steinert links + ★ + target-lang glosses per
token (particles keep their [LABEL] text — the POS signal that
survives export).  The `*x*' matcher gates on `** Interlinear';
`EN (DE: …)' lookup shapes yield their German half on de-target
docs; the grounding's `\\=`\\[' placeholder filter admits lines
beginning with org links.  All 64 files regenerated: 173/173
renderings intact.

### 5.52 Phonetics retired · Gloss Table · Claude-gloss tier (+ B0 postmortem) (done, 2026-09-15)

Carsten's three requests (2026-09-15): (1) *"phonetic brauche ich
nicht mehr.  Kann überall raus"*; (2) the analysis should ALSO carry
the §184-handout three-row table form (Wylie | Glosse |
Grammatik-Label); (3) *"das interlinear glossary fällt gegen das
Claude vocabulary deutlich ab — ist da was machbar?"*  Plan:
`~/.claude/plans/idempotent-pondering-wozniak.md` (superseding the
CASCADE v2 content there).  Executed bug-first, every commit
RED-verified.

#### B0 postmortem — silent cascade destruction (fixed FIRST)

The 2026-09-12 regeneration (buddhist-studies `d7ff7ec`) DESTROYED
`sent-047/048-rgyan.org`'s cascade structure: `tibetan-sentence-
reanalyze-file`'s dispatch guarded on `(fboundp cascade-handlers)` —
in a batch where tibetan-cascade wasn't loaded, the guard silently
took the DESTRUCTIVE two-file branch.  Fix (`3389277`): textual
detector `tibetan-sentence--cascade-layout-p` (scans the first 4096
bytes for `#+TIBETAN_LAYOUT: cascade`), then `(require
'tibetan-cascade nil t)`, else REFUSE with `:error` — never silently
destructive.  Data repaired from `58c88a6` + 12.09 bodies + a
corrected-source cascade regenerate (buddhist-studies `5aaa5ea`,
idempotent, user slots byte-identical).  LESSON: an fboundp guard is
a LOAD-STATE probe, not a FILE-KIND probe — never let it choose
between destructive and preserving branches.

#### Interlinear-quality bugs (Claude-gloss tier, part 1 — two-file)

- `797143b` sense-number strip also handles `(1) ` (regex gap: only
  `1.` / `1)` / `1:` were stripped).
- `2421809` Claude auto-regen fires on VOCABULARY-only arrival too
  (was :particles-gated — vocab-only responses never re-rendered).
- `a40e4c6` C1b star-sanitizer spares the section's own
  already-converted org sub-headings (`*** Tibetan Divergence` was
  space-prefixed → destroyed on every rewrite; REAL latent bug the
  RED suite surfaced).
- `6582b3c` paragraph reanalyze binds BOTH render vars (par files
  had NO Claude override at all) + preserves Claude sections (the
  interactive par command WIPED them every run); shared helper
  `tibetan-analysis--claude-render-vars`.
- `4956b3c` headless `tibetan-analysis-reanalyze-paragraph-file`
  (par-NNN filename + #+SOURCE rescan) + auto-regen routes par-*
  files there.
- `2f0d7cf` override gate WIDENED (the C-core): precedence is now
  kuratiert★/Custom/`<term>` > Claude context gloss > dictionary
  first-sense — for ALL tokens, not just proper nouns.  Generic path
  EXACT key only (M2's `mar`/`mar pas`).  Accepted grounding cycle:
  Claude glosses fed back as grounding are its own earlier context
  readings.

#### Phonetics retirement (request 1)

`72d1638` layout unplug (emitter block deleted, priority-order +
sentence strip-list entries out, ~8 test retargets — the §5.19
feature inverted the same way F1 killed the Detailed Dictionary);
`0145715` module + 54 tests deleted; `f08ae19` thesaurus template
line dropped.  Legacy files lose the section on next reanalyze.
MA Readings keep theirs until the §5.36 wordlist repair (NO batch
regenerate there — caveat still stands).

#### Gloss Table (request 2)

NEW module `analysis/tibetan-gloss-table.el` (+18 tests, wired):
- `c662519` pure LABEL RESOLVER on `tibetan-reading--unit-tokens`
  plists: native first (particle label verbatim; bare nominaliser
  after verb → NMLZ; verb → V / V.HON on "(hon.)"), then Claude-POS
  exact key (PN/PRON/N/ADJ/ADV/CONJ/NUM/V, `.HON` suffix), fallback
  `?`; merged-clitic labels dot-append (bya-ba'i → NMLZ.GEN).
- `b0f1cda` pure renderer: per shad unit ONE org table, 3 rows ×
  N token columns, plain cells (`|`→`\vert`, no markup/links),
  column-width padded so it aligns as RAW TEXT.
- `96cc13a` layout: `** Gloss Table` between Interlinear and Claude
  Vocabulary (marker trick: both insert at the interlinear marker,
  LAST writer lands FIRST); in priority-order; ADDED to the sentence
  strip-list (4→5 — prep-time material); par files inherit; cascade
  files untouched (`* Reading` carries the per-unit view).  +1 BDD.

#### Claude-gloss tier, part 2 — Reading/cascade (request 3)

- `42629c0` `tibetan-reading--gloss`: NON-curated tokens prefer the
  exact-key Claude gloss from `--claude-vocabulary-for-render`
  (curated 60-char budget); ★ still wins.  Feeds cascade Reading
  lines AND Gloss Table row 2.
- `eb9b573` BUG found while wiring: cascade regenerate DROPPED any
  preserved L2 body whose heading the fresh scaffold didn't emit
  (renderer-error fallback path; §5.26 class).  Restore now CREATES
  the missing heading above `* Footnotes`.
- `e3fa4d3` `tibetan-cascade--regenerate` binds the render var from
  the file's own preserved Claude Vocabulary (shared helper) around
  the scaffold — the Rgyan corpus's context glosses reach the layer
  Carsten reads.

#### Suite

2257 → **2287 ERT** (net: +84 new − 54 deleted phonetics), 0
unexpected, 1 skip; BDD 248 → **249**; `make compile` clean
throughout; REFERENCE.org regenerated per def-touching commit.

#### Corpus landing (recipe, NOT yet run at write time)

Per-corpus preserve-mode recipe: ★-parity, user slots
byte-identical, second run idempotent, NO `** Phonetics`,
`** Gloss Table` after Interlinear, 5 spot-checks Claude-gloss-
over-dictionary, no `(1) ` residue.  Order: 1. canary
Milarepa/Tibetisch IV; 2. Portfolio (defer-MT, render-only safe);
3. par files individually via the new headless command.  NOT
regenerated: MA Readings (§5.36), MA-Rgyan cascade beyond the two
B0 files, Khu-dbon.

### 5.53 Sense-selection drift batch vs interactive — root-caused, fixed (2026-09-15)

Commits `e2d16b5` `50d6dfc`.  Symptom (found during the §5.52 corpus
landing): `emacs -batch` regeneration of cascade files picked
DIFFERENT dictionary senses for NON-curated tokens than the runs
that produced the committed corpus — raw Rangjung Yeshe first lines
in `* Reading ** Interlinear`: Wylie example text (`gzhung [rgyun de
nyid skye srid nas]`), cross-references, biographical dates.
★-curated glosses were stable; only the ranked fallback drifted.
Reproducer: `tibetan-cascade-reanalyze-file` on a scratch copy of
Portfolio `analysis/sent-004-deb.org`, diff `** Interlinear` vs
buddhist-studies HEAD.

**Root cause — environmental, no commit is guilty.**  Batch output
is byte-identical at `c41d1fc` (the commit the corpus was generated
at) and at pre-fix HEAD, so W6 `eb6cd9e` is exonerated (the earlier
worktree bisect was itself skewed: `steinert.db` is gitignored, so
every worktree ran with Steinert silently disabled).  The
Reading/Gloss-Table cell gloss comes from `tibetan-lookup-word`'s
first-hit-wins walk over `tibetan-dictionary-priority`, which ranked
`rangjung-yeshe` above `steinert`; and the RY branch consulted
`lookup-combined-dictionary-first` — defined only in Carsten's
`init-tibetan-legacy.el`, i.e. only in interactive sessions — before
the bundled store.  Which store answered therefore depended on the
ENVIRONMENT.  The committed corpus is byte-for-byte what the walk
produces with steinert first (verified: all 8 deb files regenerate
byte-identical modulo `#+CREATED`/`#+LAST_ANALYZED`, idempotent
second run) — the same relative order the ranked assembler
`tibetan-vocab-multisource-entries` has always used (§5.3 rule A).
Smoking gun: the SAME seg file from the 15.09 Milarepa landing shows
the clean Hopkins sense in `** Interlinear Gloss` (multisource path)
and RY junk in `** Gloss Table` row 2 (lookup-word path).

**Fixes** (RED-first, one per commit):
- `e2d16b5` default `tibetan-dictionary-priority` now `(resources
  custom verbs steinert rangjung-yeshe local-glossary dharmamitra)`.
- `50d6dfc` `tibetan-lookup-word-in-rangjung-yeshe` consults the
  bundled lazy-loaded store FIRST; the ambient combined store is a
  fallback for bundled-store misses.  Same lesson as B0
  (`3389277`): an fboundp probe is a LOAD-STATE probe and must
  never select between divergent behaviours.

Known residual (data, not machinery): sent-005's `dkon bartsegs`
groups as an RY-only MWU with a real gloss in batch where the
committed file has per-syllable tokens + `[(look up)]` — the
corpus-producing environment's RY store was evidently empty, so the
MWU-existence probe denied the compound.  Predates the fix; the
grouped form is arguably better.  NOTE: the 15.09 Milarepa landing
(buddhist-studies `0b022dd`) was generated PRE-fix, so its
`** Gloss Table` row 2 / seg-level glosses carry raw-RY senses for
non-curated tokens — a re-landing decision is Carsten's.

Suite: ERT 2287 → **2291** (+4: 2 ranked-selection, 2 RY-store
authority), 0 unexpected, 1 skip; BDD 249; `make compile` clean;
REFERENCE.org regenerated per commit.

### 5.54 Masterarbeit: Drei Sichten auf den Rgyan-Korpus (done, 2026-09-15)

Carstens Workflow-Design für die MA-Übersetzung (§§167–186), Plan
`~/.claude/plans/idempotent-pondering-wozniak.md`, alle Commits
RED-first.  Drei Sichten, Arbeit in EINEM Fenster (dem
Analyse-Fenster); Original daneben in der Reading Class auf dem
reMarkable.

**Sicht 1 — Original** (`Rgyan_167-186-cat.org`): bereits
übersetzungsfrei (Lopez/W&M leben NUR in den Comparative-Docs +
par-Dateien).  Datenfix buddhist-studies `785c881`: die 20
`:B2_SEG_START/END:`-Drawer trugen die prä-25.08.-Ankerung (bis 27
Segmente daneben) → aus `b2-seg-mapping-2026-08-25.csv` gezogen
(34 Zeilen; Drawer sind rein informativ — sent-Dateien keyen auf
LOKALE Segmentnummern).

**Sicht 2 — Kaskaden-Analyse mit Glossentabellen** (Commits
`6e3f851` `6db5c44` `c051b95`):
- `tibetan-gloss-table-render-captioned` (pur): Kaskaden-Form
  ((GLOBAL-NUM . TEXT)…), je Einheit Beschriftungszeile `Unit K —
  Segment N` (KEIN Heading — §5.51-Lektion) + Drei-Zeilen-Tabelle.
- `** Gloss Tables` = ERSTES Reading-Kind (Carstens Platzierung:
  VOR dem Interlinear), emittiert vom Scaffold mit
  `:GENERATED_HASH:`-Drawer (sha1 des Bodys); nichts renderbar →
  Sektion entfällt.  Null Leser-Chirurgie (alle Reading-Leser
  L2-begrenzt; Grounding liest nur Interlinear-Zeilen → Tabellen
  können nie in Prompts lecken).
- **EDIT-SCHUTZ** (Carstens Bedingung „geschützt, wenn ich editiert
  habe"): `--gloss-tables-edited-p` = Body ≠ gespeicherter Hash
  (oder Hash fehlt) → `--regenerate` preserved den Block verbatim
  (alter Hash bleibt → dauerhaft geschützt; fehlt die Sektion im
  frischen Scaffold, wird sie als erstes Reading-Kind wieder
  eingesetzt — eb9b573-Muster).  RESET = Sektion von Hand löschen →
  nächster Regenerate emittiert frisch.

**Sicht 3 — Übersetzungsdokument + §-Ansicht** (NEUES Modul
`persist/tibetan-translation-doc.el`, Commits `9076d72` `edb82c6`
`3b56ed5` `7c8c79c`): Carstens Deutsch entsteht satzweise in
`* Working Translation` der sent-Dateien, Fußnoten als benannte
org-Fußnoten (`[fn:name]`-Anker im Text, Definitionen unter
`* Footnotes` derselben Datei — Handout-Konvention).
- `tibetan-translation-doc-build (source out &optional from to)` →
  GENERIERTES §-gruppiertes Dokument (`uebersetzung[-FROM-TO].org`):
  Fließtext unter `* §NNN`, Fußnoten je Satz genamespaced
  (`s%03d-`, EIN Regex für Anker/Definition/Inline; anonymes
  `[fn::…]` unangetastet), EIN `* Footnotes` am Ende.  Sichtbare
  Lücken (`[Satz N — noch keine Übersetzung]` / `Analysedatei
  fehlt`); Überschreib-Guard (Ziel ohne GENERATED-Marker →
  user-error); §-Bereich für Anhang A.1 (182–186).  Interaktiv
  `tibetan-translation-doc` (Menü „Translation"); Export `C-c e p`.
- `tibetan-translation-doc-section-view` → `par-NNN-ansicht.org`
  (kollidiert bewusst NICHT mit hand-eigenen par-NNN-handout):
  je Satz Tibetisch / Glossentabellen VERBATIM (editierte Tabellen
  fließen mit) / Vorschlag Claude + DM (Maschinen-Platzhalter
  prefix-gefiltert, §5.40-Lektion) / Übersetzung CP.  Interaktiv
  `tibetan-translation-doc-section` mit §-Completion.
- **COPYRIGHT-LOCK, testverriegelt**: beide Generatoren lesen NUR
  Working Translation / Footnotes / Gloss Tables / Claude- und
  DM-Sektionen — Poison-String-Tests stellen sicher, dass
  Renderings/Provided Translations (der Lopez/W&M-Slot) nie in
  generierte Dokumente gelangen.

**Navigation** (`workspace/tibetan-analysis-nav.el`, `a2e22b3`
`92c80cc`): `C-c u j` Rücksprung Analyse→Quelle (#+SOURCE-Link,
Window-Reuse), `C-c u n`/`C-c u p` Nachbar-Satzdateien
(suffix-aware; die seit je auskommentierten Slots sind jetzt echt).

**Sanskrit ab Oktober** (Reading Class): nichts gebaut; `* Reading`
bleibt erweiterbar (alle Leser L2-begrenzt) — ein künftiges
`** Sanskrit`-Kind braucht keine Leser-Chirurgie.

Suite 2291 → **2322 ERT** (0 unexpected, 1 skip); BDD 249 → **250**
(+ translation-doc-Szenario); compile clean durchgehend;
REFERENCE.org je def-Commit.  Korpus-Landung (56 Rgyan-Dateien
regenerieren, Demo-Stitch, §184-Ansicht) siehe Korpus-Commits.

### 5.55 §185-Review: Struktur-Tabellen · Segment-Headings · par-Politur (done, 2026-09-16)

Carstens Review der regenerierten par-185: vier Punkte, alle
RED-first umgesetzt (Commits `1ff0d34`…`1d23e1a`).

- **Glossentabellen**: überbreite Einheiten brechen in gestapelte
  3-Zeilen-Blöcke (`tibetan-gloss-table-max-width`, Default 100 —
  der ExPex-Umbruch des Handouts auf Textebene); Beschriftung ist
  jetzt ein faltbares `*** Segment N`-HEADING (Kaskade Level 3;
  par-Dateien nummerieren ihre Einheiten aus dem Quell-§-Drawer
  `:B2_SEG_START:` bzw. dem `:seg_A_to_B:`-Tag via neuer Dynamik
  `tibetan-analysis--gloss-table-seg-start`); im Two-File-Layout
  sitzt die Tabelle VOR dem Interlinear (Lesereihenfolge wie die
  Kaskade).  Die §-Ansicht demotet die kopierten Headings auf L4.
- **Sentence Structure tabellarisch** („Beides"):
  `tibetan-analysis--render-structure-tables` — EINE Übersicht
  (Seg | Satzphrasen | Verb | Anschluss, Konverb-Kette in der
  Anschluss-Spalte, HAUPTVERB auf der letzten Einheit) + je
  Einheit `*** Segment N` mit Detailtabelle (Phrase | Kasus |
  Funktion; deutsche Rollenlabels, [elidiert] sichtbar,
  Konverb-/Komplement-Klausen textuell geflattet mit →-Zeilen).
  JE SHAD-EINHEIT gerechnet — der par-Pfad lief zuvor über den
  ganzen § (Adjunkt-Müllhalde).  Kaskade delegiert
  (`--sentence-structure-body` → Tabellen, Baum-Fallback);
  seg/sent-Dateien behalten den flachen Baum (Milarepa-Lock,
  test-gepinnt).
- **par-Layout**: `* Reference Translations` ans Ende (nach
  Apparatus, vor Footnotes; Migration beim nächsten Regenerate).
- **DM-Fire für par** (`--maybe-fire-dm-for-par`, beide
  par-Kommandos): Gates needs-request-p (§5.29) + defer-MT.
- **BUG en route** (`e7ec847`): Batch-par-Reanalyze wedgte auf
  dem Supersession-Prompt — vier persist-Stellen (u.a.
  regenerate-auto!) nutzten rohes `find-file-noselect`; jetzt
  `tibetan-fresh-file-buffer` (§5.49-Klasse).  RED via
  yes-or-no-p-Signal-Stub statt Hänger.
- **Test-Harness**: der No-Network-Stub deckte den curl-Transport
  (§5.48) nicht — der neue DM-Fire traf LIVE dharmamitra.org aus
  den Fixtures.  run-all-tests.el gated `--http-post` hinter
  `tibetan-test-allow-dm-transport`; die zwei Transport-
  Selbsttests binden die Var (offline über eigene Primitiv-Stubs).

Suite 2324 → **2339 ERT** (0 unexpected, 1 skip); BDD 250;
compile clean; REFERENCE.org je def-Commit.

### 5.56 §15-Review: Ein-Raster-Glossentabellen (done, 2026-09-17)

Carstens Befund an der frisch erzeugten par-015.org: „die
Tabellendarstellung ist nicht spaltenweise ausgerichtet" — der
§5.55-Breiten-Chunker zerlegte §15 in 37 leerzeilengetrennte
Mini-Tabellen à 1–5 Spalten mit je PRIVATEN Spaltenbreiten
(innerhalb jedes Blocks org-align-identisch, verifiziert; das
Segment als Ganzes ohne gemeinsames Raster, org-Tabellenbefehle
nur je Block).  Treiber: Bedeutungszellen mit dem Interlinear-
Budget (60/30) drückten die Chunks auf 2–3 Spalten.  Formwahl
Carstens (AskUserQuestion): „Ein Raster + kompakte Zellen".
Vier Commits (`8851881`…`b96d8b5`), alle RED-first:

- `--format-rows` rendert je Einheit EINE org-Tabelle: Umbruch in
  3-Zeilen-Bänder, getrennt durch org-hlines, alle Bänder teilen
  EIN Spaltenraster (`--band-width`/`--columns-per-band`: größte
  Spaltenzahl je Band im Budget, Position k = Max über die k-ten
  Spalten aller Bänder, letztes Band mit Leerzellen gefüllt).
  Schmale Einheiten byte-identisch; `--column-chunks` entfernt.
  Ausrichtungs-Lock-Test: identische Pipe-Positionen über ALLE
  Zeilen.
- Zeile-2-Zellen-Budget: defcustom
  `tibetan-gloss-table-cell-gloss-width` (25) + `--truncate-cell`
  (Wortgrenze, hängende Interpunktion gestrippt, IMMER `…` als
  Verweis auf die Vollglosse in Claude Vocabulary/Interlinear).
  Reading-Zeilen-Budgets unberührt.
- Literale `\n`/`\t`-Serialisierungsreste: geteilter
  `--strip-escapes`, läuft VOR der Kappung (der Schnitt durch die
  2-Zeichen-Sequenz hinterließ sonst `contemplate\…`) und als
  Netz in `--cell`.

Landung: par-015/184/185 regeneriert (`:claude-preserved t`,
DM/RT/Slots intakt; par-015 erstmals committet — sein DM-Fire
hatte beim Anlegen automatisch gegriffen).  Kaskaden-sent-Dateien
migrieren beim nächsten Regenerate (editierte `** Gloss Tables`
bleiben per §5.54-Schutz stehen).  Suite 2339 → **2345 ERT**
(0 unexpected, 1 skip); BDD 250; compile clean; REFERENCE.org je
def-Commit.

### 5.74 Satzdateien nach Vorlage: Text / Segmente / Satz (done für Tibetisch, 2026-10-28)

Carsten (28.10.), Umbau der Satzdateien der Reading-Klassen:
Partikelkarte, Claude Particles, Claude Vocabulary und die Sentence
Structure je Segment fallen weg; die Glossentabellen tragen nach C-c u G
keine „?" mehr; Grammatik nur noch als Überblick über den ganzen Satz,
kurze Note + Struktur + Begriffe unter dem jeweiligen Segment;
Vergleichsübersetzungen je Segment und je Satz, sinnverschiebende
Unterschiede hervorgehoben (kein Issue/Assessment/Question); das Handout
ist keine eigene Funktion mehr, sondern genau diese Datei als PDF aufs
reMarkable, **ein PDF je Satz**.  „Template basierend arbeiten, gerne
auch kaskadierend"; Gliederung „ganzes Original (bei Sanskrit + Metrik)
/ Segments / Sentence".  Reihenfolge: **Tibetisch zuerst**, dann
Sanskrit, dann zweisprachig.  Rückfragen entschieden: Änderungen nur bei
C-c u G auf eine Datei (keine Rück-Aktualisierung); C-c u A nutzt die
Vorlage schon, ohne KI; C-c u G baut eine Datei im alten Layout komplett
neu; C-c u G endet mit C-c u H; Vergleichsübersetzungen im persönlichen
PDF erlaubt; Kopfzeile „Ordner · Werk · Satz · Vers"; **nur
Reading-Klassen** (die Masterarbeit behält das alte Layout).

#### Zielform (Golden: `Templates/analysis/samples/Tibetan.org', bDen Satz 9)

```
* Text                   ganzes Wylie des Satzes (verse-Block)
* Segments
** Segment N             Glossentabelle (3 Zeilen, steinert:-Links,
                         Partikel ohne Link), darunter
                         ⟦N⟧ Claude \  *DharmaMitra:* … \  *Lopez:* …
                         → Lopez: <Sinnverschiebung>
                         *Structure:* … / *Note:* … / *Concepts:* …
* Sentence
** Grammar               Überblick über den ganzen Satz
** Translations compared *Translation:* (Claude) + Referenzen + Fazit
* Working Translation / * My Notes / * Footnotes   (Nutzer-Slots)
```

#### Bausteine (neue Module, Präfix `bcat-')

- **`persist/bcat-template.el'** — Mustache-artige Vorlagen: `{{name}}',
  `{{#liste}}…{{/liste}}', `{{^x}}', `{{> teil}}'; eine Zeile, deren
  Platzhalter alle leer sind, fällt samt Beschriftung weg (doppelte
  Leerzeile → eine); `#+begin_comment'-Blöcke werden entfernt;
  Abschnittsmarken müssen allein stehen (sonst user-error).  **Kaskade:**
  `<Quellordner>/Templates/analysis/' → `<Quellordner>/Resources/
  Templates/analysis/' → `bcat-template-directory' (Werkzeug).
- **`Templates/analysis/tibetan.org'** — das Gerüst; Kopf mit
  `#+OPTIONS: … title:nil author:nil date:nil' und `#+CAT_TEMPLATE:
  tibetan'.
- **`persist/bcat-sentence.el'** — Rumpf (C-c u A, ohne KI:
  Wylie, Wörterbuchtabelle, Referenzen aus § und Satz), Erkennen der
  Vorlage, Ablauf von C-c u G.  **Opt-in pro Quelle:** `#+CAT_TEMPLATE:
  tibetan' im Quellkopf (`bcat-sentence-template-name' liest nur
  Kaskaden-Quellen; `bcat-sentence-file-p' prüft nur `sent-NNN'-Dateien
  — sonst zählte die Quelle selbst als Satzdatei, `7393d1f').  Schalter
  `bcat-sentence-templates' (t).
- **`persist/bcat-analysis.el'** — EIN Claude-Abruf je Satz, Antwort
  JSON: je Segment `words' (wylie/gloss/function), `translation',
  `references', `shift', `structure', `note', `concepts'; für den Satz
  `grammar', `translation', `references', `comparison'.  Partikel-Labels
  (ERG GEN ABS TERM LOC ABL COM DAT ALL INST CONV) bekommen keinen
  steinert:-Link.  Dazu der kleine Zuordnungs-Abruf (3c).
- **`persist/bcat-export.el'** — C-c u H: org-latex mit
  `org-latex-tables-centered' nil, ohne tabularx, LuaLaTeX ×2,
  Kopfzeile per `org-export-before-processing-functions' (fancyhdr,
  `Ordner · \textit{Werk} · Satz N · Vers'), Upload über
  `tibetan-discussion-paper--upload' (Ordner = Werk, Dokument „Satz N",
  vorhandener Name bekommt einen Zeitstempel — nie überschreiben).
  Schalter `bcat-export-auto' / `bcat-export-upload' (t); in
  `bcat-sentence-after-fetch-functions' eingehängt = C-c u G endet mit
  C-c u H.

#### Ablauf C-c u G auf Vorlagendateien (`bcat-sentence-fetch')

1. defer-mt-Prüfung; `--prepare': fehlt die Datei → Rumpf; altes Layout
   → komplett neu bauen, dabei Nutzer-Slots, `#+CREATED' und von Hand
   eingetragene Referenzen erhalten.
2. Zustand je Datei (`bcat-sentence-fetch-state'): `full' (noch keine
   Analyse) / `align' (Analyse da, aber eine ergänzte
   Vergleichsübersetzung ohne Zuordnung) / nil (fertig).  Über
   `tibetan-cascade-subtree-confirm-threshold' → Rückfrage.
3. `full': DharmaMitra zuerst (gestaffelt), dann Claude über
   `tibetan-discussion--submit' („bcat-analysis"); `--land' rendert die
   Vorlage neu, fehlende Referenzen kommen wieder dazu, `LAST_ANALYZED'
   heute, dann die Hook-Funktionen (Export + Upload).
4. `align' (3c): nur die neue Übersetzung wird per kleinem Abruf
   („bcat-align") den Segmenten zugeordnet und chirurgisch
   eingesetzt (`--align-merge', rein funktional) — keine
   Vollanalyse.  **Ergänzte Vergleichsübersetzungen funktionieren also
   automatisch mit C-c u G.**
5. Geschrieben wird über den besuchenden Puffer (`--write'; bei
   veraltetem Puffer `set-visited-file-modtime', sonst „Cannot resolve
   conflict in batch mode").

#### Weichen im alten Code

`tibetan-cascade--create-file' / `--regenerate' / `tibetan-cascade-fetch'
verzweigen auf bcat, wenn die Quelle eine Vorlage hat (die alten Pfade
heißen `…-legacy'); `--fire-sentence-1' liefert für Vorlagendateien
`new-layout'; der Materialisierer überspringt sie.  Zettel-Hervorhebung:
`** Segment N' zählt als Glossentabellen-Abschnitt (`8b3e36b').  C-c u D
leitet in Vorlagenquellen auf C-c u H um (`1e7d5ad').  Nebenbefund
„Segment 38 beginnt mit barjod": Präfix བ vor ར/ལ, la-mgo-Stapel ལྐ ལྒ ལྔ ལྤ
ལྦ (`ae21baa').

#### Korpus

`#+CAT_TEMPLATE: tibetan' nach `#+TIBETAN_LAYOUT: cascade' in
`WS26-27/Tibetisch V/bden-gnyis-cat.org',
`Mahayanasutralamkara/Tibetisch/mdo-sde-rgyan-1-cat.org',
`…/Tibetisch-9/mdo-sde-rgyan-9-cat.org', `…/Sthiramati-9/savbh-9-cat.org',
`Abhidharmasamuccaya/Tibetisch/kunbtus-1-cat.org' und in der
Reading-Class-Vorlage `.claude/skills/reading-class/vorlagen/kopf-bo.org'
(Korpus `254b1de5').  Vorhandene Satzdateien bleiben, bis C-c u G sie
neu baut.

#### Tests und Commits

Neu: `test/bcat-template-test.el', `test/bcat-sentence-test.el',
`test/bcat-analysis-test.el', `test/bcat-export-test.el' (inkl.
LuaLaTeX-Rauchtest per `skip-unless'), Fixtures
`test/fixtures/bcat-template-sent-009.el' und
`bcat-analysis-sent-009.json'.  Die Test-Runner (`test/run-all-tests.el',
`spec/run-specs.el') setzen `bcat-sentence-templates',
`bcat-export-auto' und `bcat-export-upload' auf nil.  Commits
`b584942' (Zielform), `4dd83ab' (Schritt 1 Vorlagen), `8ac9974'
(2 Rumpf), `3f0e9b5' / `bb9924a' / `7f50a3b' (3a–c Abruf), `9ab36bb' (4
Export), `7393d1f' (Opt-in), `8b3e36b' / `1e7d5ad' (5 Leser), `99d6919'
(Bugfix: der Export-Haken überspringt Fixtures unter
`temporary-file-directory' wie der alte Auto-Papier-Pfad seit §5.69 —
sonst lädt ein ERT-Lauf im laufenden Emacs Test-Dateien aufs Gerät).
ERT 2629 (0 unexpected, 1 Skip), BDD 251, compile ohne Warnung.

**Erster Live-Lauf (28.10., bDen Satz 9):** Queue „ok“, dann „Antwort
… nicht lesbar“ — die Datei war schon zum Rumpf umgebaut, die Antwort
verloren.  `957ea88': `bcat-analysis-read' liefert (OBJEKT . PROBLEM),
eine unlesbare Antwort (Analyse wie Zuordnung) wird unter
`bcat-analysis-response-directory' (<temp>/bcat-responses/) als
`<satzdatei>-<zeit>.txt' aufgehoben, die Meldung nennt Grund
(„JSON-Fehler bei Zeichen N“, 1-basiert wie `goto-char') und Pfad.
`6206254': scheitert das strenge Lesen, repariert
`bcat-analysis--repair-json' rohe Steuerzeichen und innere
Anführungszeichen in Strings — genau die Defekte, die die verlangte
wörtliche Kopie von Lindtners Anmerkung (Absatz, Eimer-Zitat mit
"…"Befleckungen"…") erzeugt; Ursache für Satz 9 nicht belegt.
ERT 2631.

**Nachbesserung 29.10.** (Carsten nach dem ersten echten Satz 9):
- `91eec29' Satz-Referenzen bleiben UNSER Text: Prompt (Analyse wie
  Zuordnung) verlangt keine Vollkopie mehr („not its notes“ für die
  Ausschnitte); `bcat-analysis-mark-reference' überträgt die fetten
  Marken der Segment-Ausschnitte der Reihe nach auf den gesendeten
  Text (`bcat-analysis-context base parsed REFS').  Behebt den
  Verlust von Lindtners Anmerkung (Claude hatte sie beim Abschreiben
  weggelassen) und die JSON-Brüchigkeit.
- `b409720' Segment-Übersetzung beschriftet: `⟦N⟧ *Translation:* …'
  (unbeschriftet las sie sich nicht als Übersetzung); ⟦N⟧ bleibt vorn
  (Fetch-Zustand), `--own-translations' liest beide Formen.
- `bcda730' Vers unter `* Text' mit den Vokabeln in Klammern
  („Wenn ich einen Satz segmentweise übersetze muss ich … blättern“):
  `rnal 'byor pa [yogin] yis [ERG] … na [CONV · cond.] / /';
  `bcat-analysis-glossed-line' (Claudes Wörter; im Rumpf die
  Wörterbuch-Einheiten der Tabelle), deckt die Wortliste die Silben
  nicht exakt ab → blankes Wylie.  Vorlagenfeld `{{glossed}}'
  (`{{wylie}}' = blanker Vers).
- `ad22135' PDF: glossierte Verszeilen aus `\mbox'-Einheiten, Shad
  mit `~' gebunden, Vers flatterrechts — Umbruch nur zwischen
  Wort+Glosse (vorher „[ob-struction]“, „/“ allein, Überlauf).
ERT 2637 (2. Skip = pyewts fehlt dem python3 der Shell).
- `93f1e4a' Bugfix: mehrsilbiges Wort mit Kasus-Funktion („shes bya'i“,
  „GEN · second member of compound“) ist keine Partikel — Partikel nur
  bei EINER Silbe (Link in der Tabelle, Glosse im Vers).
- `2964907' * Text nach Carstens Leseprobe („trial and error“): blanker
  Vers, darunter (nur wenn eine Zeile Glossen hat, Feld `glosses') der
  Vers mit Vokabeln, dann `*Translation:*' des ganzen Satzes (Feld
  `translation', bleibt auch unter Translations compared).  ERT 2639.
- `9efcfc9' dritter Satz-9-Abruf „nicht lesbar“ — dank 957ea88 lag die
  Antwort vor: Claude hatte in der Strukturzeile `=\'i=' geschrieben
  (\' ist kein JSON-Escape).  Die Reparatur lässt einen Backslash vor
  einem Nicht-Escape-Zeichen weg.  Die aufgehobene Antwort wurde per
  `bcat-sentence--land' ohne neuen Abruf eingesetzt (DharmaMitra-Text
  aus dem Git-Stand, da die Datei ihn nicht mehr hatte).  Lücke: bei
  einer unlesbaren Antwort geht auch der DharmaMitra-Text des Laufs
  verloren — spricht für das Aufheben von Antwort + Referenzen.
  ERT 2640.
#### Sanskrit (29.10.)

Carsten: Sanskrit nach demselben Muster; sein Sanskrit ist rudimentär,
in den Readings sitzen Profis (Sadananda Das, Jowita Kramer,
Doktoranden), es geht tief in die Grammatik — gaṇa, Stamm usw. dazu,
Referenz die Devavāṇīpraveśikā (Goldman & Sutherland Goldman 1999, als
Text unter ~/Documents/privat/Devavanipravesika.txt).  Antworten: unter
Text saṃhitā + Padapāṭha mit Vokabeln „reicht so“, Grammatiktiefe
„erstmal so“, keine Devanāgarī, Rezitation ZUERST, Pāda-Kennungen a–d
weg.

- `909c1ac' Zielform `samples/Sanskrit.org' (TĀ 1007, Inhalt von Hand,
  nicht über C-c u G; korrigiert madvāgaśaktāpi = mad-vāk aśaktā api).
- `c82202c' Vorlagen-Bugfix: übersprungener Abschnitt fasst Leerzeilen
  zusammen.
- `1600a59' Gerüst `sanskrit.org': {{#metre}} * Recitation (metre,
  {{#recitation}} Vers, scansion) · * Text (samhita, {{#glosses}}
  glossed, *Translation:*) · je Segment Tabellen, Übersetzungsblock,
  *Sandhi:*, {{#forms}} *Forms:*, *Compounds:*, Structure/Note/Concepts
  · {{#word-analysis}} ** Word Analysis am Ende von * Segments · * Sentence.
- `b96bb8a' Rumpf ohne KI: `tibetan-metre-recitation' (Teile; alter
  `** Metre' byte-gleich daraus), saṃhitā, leere Segmente.
- `33812c2' Prompt/Antwort (words: word lemma formation parts gloss form;
  sandhi, forms, compounds in Devavāṇīpraveśikā-Begriffen, shift als
  Liste), `bcat-analysis-sanskrit-tables' (eigene Tabellen ≤
  `bcat-analysis-sanskrit-table-width' 95, nichts gekappt),
  `-padapatha-line', `-sanskrit-context'.
- `35a27ca' Abruf/Landung/Zuordnung verzweigen über
  `bcat-sentence--sanskrit-p'.
- `5f03fe1' `** Word Analysis' (- Wort — Lemma; Form) in den neuen
  Dateien für die sechs Leser (Hervorhebung inkl. Projektion in die
  Quelle, Diskussion, Papier, Handout, Wortbildung, padapatha-pdf.py);
  am Ende von * Segments (unter * Sentence wäre ein eingefügter
  Vergleichsabsatz darunter gerutscht); C-c u H lässt ihn im PDF weg.
- Korpus `61bdf659': #+CAT_TEMPLATE: sanskrit in tantraloka-1.org,
  msa-1-cat.org, msa-9-cat.org, asam-1-cat.org, kopf-sa.org; an einer
  Kopie von tantraloka-1.org geprüft (Satz 1 Nardaṭaka, 1007, 1008
  Prosa).  Sessions Tantrāloka/Reading-Class/AS benachrichtigt.
Originalschrift (29.10. nachm., Carsten: „immer mehr von Roman Script zu
Uchen und Devanagari“): `e3c62ad' Umschrift IAST → Devanāgarī
(`tibetan-sanskrit-script-iast-to-devanagari', aus den Tabellen der
Gegenrichtung); `832ed74' * Text je Segment {{uchen}} über {{wylie}}
bzw. {{devanagari}} über {{samhita}}; `daf5b7c' PDF: eigene
Schriftfamilien \bcattib/\bcatdeva (`bcat-export-tibetan-font' Noto
Serif Tibetan, `bcat-export-devanagari-font' Noto Sans Devanagari —
Arial Unicode MS als Ersatzschrift formt keine Ligaturen), \large,
\\[1ex] nach jedem Paar.  Tabellen bleiben in Umschrift.
Sanskrit danach umgestellt (`e38ab13', Carsten: „das Devanagari im
Recitationsverse (also de facto im Original) und den Text dann im
reinen IAST mit sandhi Auflösung“): * Recitation = Original in
Devanāgarī (Vers mit Pausen; Prosa ohne Metrum/Skansion, aber mit
Rezitation), * Text = Padapāṭha in IAST ({{padapatha}}, Rumpf: die
überlieferte Zeile), darunter mit Vokabeln.  Erster echter Abruf (TĀ
1007, 16:44): Padapāṭha richtig; `6cce69e' →-Zeile „Name: []“ fällt
weg; `93d1c7d' Prompt: Glosse ≤ 30 Zeichen ohne Kommentar, →-Zeilen
nur mit Markierung, Sandhi „as it stands in the transmitted text“.
Modell in Carstens gptel: claude-opus-4-8 (Sandhi-Zeile fehlerhaft).
`56c7846' C-u C-c u G analysiert fertige Sätze neu (immer mit Frage,
Nutzer-Slots bleiben); ohne Präfix sagt C-c u G „vollständig — nichts
abgerufen (neu abrufen: C-u C-c u G)“ — Carsten hatte nach dem
Neustart C-c u G auf den fertigen Satz 1007 gedrückt, nichts änderte
sich.  `48c15a2' Statusanzeige: persist/bcat-status.el, Puffer
*Buddhist CAT* (unten, q), Block je Lauf, Zeile je Satz (angelegt/neu
gebaut · DharmaMitra · Claude/nicht lesbar mit Grund · Zuordnung · PDF
· reMarkable mit Dokumentname); Upload hat dafür ein optionales DONE.
ERT 2666.  Offen: maschinelle Sandhi-Gegenprobe des Padapāṭha (wie
padapatha-pdf.py) fehlt noch; MSA-Synopse (§5.68) hängt am alten
Handout — in Vorlagenquellen ist C-c u H jetzt der Satz-Export.

**Offen:** C-c u G lässt eine vollständige Datei liegen — eine
geänderte Vorlage erreicht fertige Sätze nur per neuem Abruf.
Vorschlag an Carsten: Claudes geparste Antwort neben der Datei
aufheben, dann rendert C-c u G fertige Sätze ohne Abruf neu.

LEHRE: `make compile' VOR dem Commit separat prüfen — in 3c erschienen
Warnungen erst nach der Commit-Kette (es war ein veraltetes .elc).

#### Offen

- Sanskrit-Vorlage (mit Metrum im `* Text'), dann zweisprachig.
- Erst danach fliegen altes Handout, Synopse, Diskussionspapier und
  der alte Satzdatei-Pfad raus — Sanskrit-Quellen brauchen sie noch.
  `tibetan-discussion--submit' und der rmapi-Upload ziehen dann in
  bcat-Module.
- §5.73-Restschritt: `#+CAT_*' neben `#+TIBETAN_*' lesen.

### 5.73 Umbenennung in Buddhist CAT — erster Schritt: die Hülle (done, 2026-10-28)

Carsten (28.10.): „Ich will jetzt sowieso das CAT-Tool umbauen
(lassen). Der erste Schritt wäre jetzt schon mal es von
Tibetan-CAT-Tool in Buddhist-CAT-Tool umzubenennen." Umfang per
Rückfrage entschieden:

- **Hülle jetzt, Präfix beim Umbau.**  Hauptdatei `git mv
  tibetan-cat.el buddhist-cat.el', Feature `buddhist-cat'; die 1772
  `tibetan-'-Symbole bleiben, bis ihr Modul umgebaut wird.  Neue und
  umgebaute Module: Präfix **`bcat-'**.  Wirklich tibetische Module
  (Wylie, Steinert, Bialek, Hill-Verben, tibetisches Vokabular) bleiben
  `tibetan-'.  NICHT `buddhist-': Carstens `~/.emacs.d' hat 36 eigene
  `buddhist-*'-Funktionen.
- **Alter Name lädt weiter:** `tibetan-cat.el' ist ein kleiner Lader, der
  `buddhist-cat' aus SEINEM Verzeichnis lädt (Carstens frühere Init lud
  per Pfad ohne `load-path'); `buddhist-cat.el' meldet BEIDE Features an
  (`36012de' — sonst liefen `with-eval-after-load 'tibetan-cat' und
  `featurep'-Prüfungen ins Leere, wenn die Init direkt buddhist-cat.el
  lädt).
- **Customize-Gruppe `buddhist-cat'** mit `defgroup' — die alte Gruppe
  `tibetan-cat' wurde in 57 defcustoms benutzt, aber nie definiert.
  Modulgruppen (`tibetan-thesaurus' usw.) bleiben bis zum Umbau.
- **Menü und Kontextmenü „Buddhist CAT"**; Makefile, CI, install.sh,
  Setup-Assistent (schreibt buddhist-cat.el in die Init, erkennt beide
  Namen), Doku-Generatoren, laufende Doku.  `test/tibetan-ocr-correct-
  test.el' las den Lader `../tibetan-cat.el' — jetzt `../buddhist-cat.el'.
- **GitHub** `tibetan-cat.el' → `buddhist-cat' (`gh repo rename', die
  alte URL leitet weiter), Remote umgestellt.
- **Verzeichnis** `~/tibetan-cat.el' → `~/buddhist-cat'; unter dem alten
  Namen ein **Symlink**, damit laufende Emacs-Instanzen, andere Sessions,
  Claude-Code-Einstellungen und noch nicht angepasste Pfade weiterlaufen.
  Entfernen erst, wenn nichts mehr darauf zeigt.
- **Carstens Configs:** `~/.emacs.d' (Lader, Menü-/Tasten-/Glossarpfade,
  Präsentationsordner, Treemacs-Standardprojekt — der laufende
  Arbeitsbereich wurde live umgestellt, sonst stünden nach dem Neustart
  beide Projekte da), `~/emacs-zettelkasten' (Kommentar, CLAUDE.md) +
  Gitlink im Korpus.

**Offen (eigener Schritt, entschieden):** Kopfzeilen `#+CAT_*' — neue
Dateien bekommen sie, das Werkzeug liest `#+TIBETAN_*' und `#+CAT_*'
\(211 Stellen in 27 Dateien, 324 in Tests), der Korpus wird NICHT
umgeschrieben (7621 Dateien, MA-Readings gesperrt).

Tests: `test/buddhist-cat-test.el' (Feature + alter Name; Laden per
Pfad in frischem `emacs -Q' über beide Dateinamen; Gruppe; Menütitel),
vorher rot.  ERT 2597 (0 unexpected, 1 Skip), BDD grün, compile ohne
Warnung.  Commits `4c2ca4e', `36012de'.

### 5.72 PDF-Export der Satzdateien: steinert:-Links in tabularx (done, 2026-10-28)

Carsten (28.10.): `C-c C-e l p' auf Tibetisch V `sent-009-bden.org'
„ging schief" — LuaLaTeX: „File ended while scanning use of
\TX@get@body", kein PDF.

- **Ursache:** Zeile 1 jeder Glossentabelle trägt `steinert:'-Links;
  `tibetan-steinert-link-export' schrieb die Web-URL roh in `\href' —
  mit `#' und vielen `%' (`#%7B%22activeTerm…').  Roh ist das nur gut,
  solange `\href' nicht im Argument eines anderen Befehls steht.
  Carstens Init (`~/.emacs.d/config/init-org-stable.el',
  `org-latex-auto-tabularx') macht Tabellen ab drei Spalten zu
  `tabularx', und tabularx liest seinen Inhalt vorab als Argument:
  `%' wird Kommentar, frisst die schließende Klammer, LaTeX läuft bis
  zum Dateiende.  Segment 38 (zwei Spalten → `tabular') ging durch,
  Segment 39 nicht.
- **Fix** (`3ddc042'): `%' und `#' als `\%'/`\#' — wie org es für die
  eigenen `\href'-Pfade tut (`org-latex--protect-text'); hyperref
  schreibt die echte URL ins PDF (geprüft: 11 URIs ohne Backslash).
  Tests `tibetan-steinert-link-export-latex-escapes-url' und
  `…-latex-compiles-in-tabularx' (echter lualatex-Lauf, skip ohne);
  der alte Export-Test hatte die rohe Form festgeschrieben.
- **⟦N⟧ fehlten im PDF:** Times New Roman und die bisherigen
  Ersatzschriften haben U+27E6/27E7 nicht.  In Carstens Export-Init
  steht jetzt `STIX Two Math' (macOS-System, zu Times gezeichnet) als
  letzte Stufe der luaotfload-Ersatzkette (emacs-config `37e60f5';
  ebenda die liegengebliebenen Änderungen vom 09./19.10. als eigene
  Commits).  Export von sent-009 mit voller Init: 8 S., ohne Warnung.
- REFERENCE.org bei einem fremd-verschmutzten Baum: aus `git archive
  HEAD' + eigener Änderung erzeugen, Pfade zurücksetzen, nur das
  übernehmen.

### 5.71 Bedeutungszeile der Wortbildung gekappt (done, 2026-10-28)

Carsten (28.10.): „bei asam-1-cat.org ist die Tabelle bei der Analyse von
Sentence 4, Segment 35 im Analyse File irgendwie gebrochen". Ursache: die
§5.67-Zeile (Bedeutungen von Präfix/Wurzel/Suffix) war als einzige
Zeile ungekappt; das Kompositum ṣaḍ-ākāra-atīta-vartamāna-upabhoga-
dhāraṇatām brachte 270 Zeichen in eine Zelle → Spaltenbreite 282 →
ein Wort je Band, jede Zeile im Fenster umgebrochen.
- `tibetan-gloss-table--formation-meaning-cell': kappt auf
  max(`tibetan-gloss-table-cell-formation-width' (60), Breite der
  Wortbildungszelle) mit „…" (Hilfsfunktion
  `tibetan-gloss-table--truncate-to', aus `--truncate-cell'
  herausgezogen). Volle Liste bleibt in `** Word Formation'.
- Test `tibetan-gloss-table-sa-formation-meaning-capped'; ERT 2591.
- Korpus: sent-004-asam neu gerendert (Zeilen ≤ 166). Einziger
  weiterer Fall: Tantraloka/analysis/sent-1000-tantralo.org (nicht
  angefasst).

### 5.70 Hänger nach C-c u G: Landung ohne Änderungs-Hooks (done, 2026-10-26)

Carsten (26.10.): „Irgendwie stürzt Emacs jetzt nach C-c u G hin und
wieder ab" — später „nach erstem C-c u G wieder nicht bedienbar".
Kein Absturz: Emacs bei 100 % CPU, Speicher +30 MB/s, weder `C-g'
noch `emacsclient' kamen durch, kein Crash-Report.

**Diagnoseweg** (ohne Lisp-Backtrace, lldb-Attach ist auf macOS
verboten):
- `sample PID 5` (C-Stack): `command_loop → … → sit_for →
  wait_reading_process_output → timer_check → Ffuncall → exec_byte_code'
  = eine TIMER-Funktion, die nicht zurückkehrt; Blätter `plist-get',
  `gethash', `buffer-base-buffer', `cl-type-of', `make-vector',
  `skip_chars' = org-element.  Timer laufen in jeder Eingabe-Wartephase
  weiter, deshalb kein `C-g'; `kill -INT' ×3 erzwingt zwar den Quit
  (`signal_or_quit → call_debugger'), aber der Debugger wartet wieder
  auf Eingabe, und der Timer läuft weiter.
- Timer-Protokoll: `(advice-add 'timer-event-handler :around …)' mit
  START/END je Timer nach `~/timer-trace.log' (Datei
  `scratchpad/timer-trace.el' der Session; per `emacsclient -e (load
  …)' in den laufenden Emacs geladen — NICHT per `C-x C-e', das wertet
  nur einen Ausdruck aus und ließ die Advice ins Leere laufen).  Die
  letzte START-Zeile ohne END: **`org-indent-initialize-agent'**.
- org 9.7.11: `org-at-item-p' ruft `org-element-at-point' — der
  org-indent-Agent tut das je Zeile.  `org-element--cache-active-p'
  vergleicht `org-element--cache-change-tic' mit
  `buffer-chars-modified-tick'; Schreiben mit
  `inhibit-modification-hooks' t lässt den Zähler zurück → Cache mit
  verschobenen Positionen.  Batch-Nachspiel an sent-754: „Invalid
  search bound (wrong side of point) … Resetting"; im interaktiven
  Emacs (Folding, jit-lock, Scrollen) kehrt der Agent nicht zurück.

**Ursache im Tool (Hälfte 1, `e89c7e6'):**
`tibetan-analysis--insert-claude-sections' und
`--restore-claude-sections' schrieben die Satzdatei unter
`(let ((inhibit-modification-hooks t)) …)' — seit 2026-05-21 gegen
„Match data clobbered" vor einem `replace-match', das es längst nicht
mehr gibt (`--replace-claude-section-body' löscht und fügt nach
Position ein; die Überschriften-Helfer nutzen nach `insert' keine
Match-Daten).  **Fix:** die Bindung weg, Hooks laufen.  RED-Test
`tibetan-claude-sections-landing-keeps-org-element-cache-in-sync'
\(beide Writer; danach Zähler gleich, `org-element-at-point' am
Dateiende antwortet) — vorher scheiterte er mit genau der org-Warnung
„Unregistered buffer modifications detected".

**Hälfte 2 — warum es danach WEITER hing (der Lisp-Backtrace):** mit
dem Fix geladen hing Emacs schon nach C-c u A auf `sent-1005-tantralo'
\(16 KB).  Lisp-Backtrace (s. u., wie er zustande kam):
`org-indent-initialize-agent → org-indent-initialize-buffer →
org-indent-add-properties → org-at-item-p → org-element-at-point →
org-element--parse-to(1223) → org-element--cache-put' eines
**nullbreiten `node-property'-Elements bei 537** — mitten in der
Überschrift `** Metre' — Runde um Runde (daher +30 MB/s).  Der Cache
des Puffers war mit Positionen aus einer ANDEREN Fassung der Datei
gefüllt: `org-element-cache-persistent' t (Carstens
`init-org-stable.el') + org-persist (`~/.cache/org-persist/',
517 Cache-Einträge, 465 davon Satzdateien) — in früheren Sitzungen
mit der Hälfte-1-Macke verdorbene Caches wurden unter dem md5 des
Endinhalts persistiert und bei jedem Besuch wiederhergestellt
\(org-persist prüft nur den Hash des Inhalts, nicht die Konsistenz).
Im Batch nie reproduzierbar: dort ist `org-persist-directory' ein
Temp-Verzeichnis.  **Gegenmittel:** live `(org-persist-unregister
'org-element--cache 'all :remove-related t)' + `org-persist-gc'
\(517 → 1 Einträge), `org-element-cache-reset' im Puffer;
dauerhaft: `tibetan-analysis-mode-hook' setzt in Analyse-Puffern
`org-element-cache-persistent' lokal nil (RED-Test
`tibetan-analysis-mode-hook-no-persistent-org-element-cache').
Empfehlung an Carsten: global nil (die Dateien sind klein und werden
ständig neu geschrieben).

**Wie der Backtrace zustande kam** (`scripts/timer-trace.el', per
`(load …)' aus der Init oder `emacsclient -e'): (1) Advice um
`timer-event-handler' protokolliert START/END je Timer mit Puffer
→ `~/timer-trace.log' (Täter = START ohne END); (2) Timer laufen mit
`inhibit-quit' t (timer.el) — Advice `my/with-quit' hebt das für den
Agenten auf; (3) `org-element--parse-to' verschluckt C-g fünfmal
\(Meldung „Suppressed C-g … Press C-g N more times") —
`debug-on-message' auf genau diese Meldung öffnet den Debugger beim
ERSTEN C-g mitten in der Schleife; (4) Advice um
`debugger-setup-buffer' schreibt `~/backtrace-names.txt'
\(`mapbacktrace', nur Namen, kann nicht hängen) und `~/backtrace.txt'
\(gekürzt gedruckt).  Im Debugger ist Emacs per `emacsclient'
ansprechbar (Cache-Inhalt via `avl-tree-flatten org-element--cache').
NICHT: `kill -INT' an ein GUI-Emacs ohne Terminal — das ist kein C-g,
sondern setzt `quit-flag' auf `kill-emacs'; `kill -USR2' wird in einer
Timer-Schleife nie verarbeitet; `lldb -p' ist auf macOS verboten;
`sample PID' liefert nur den C-Stack (immerhin: `timer_check →
Ffuncall' = Timer, `signal_or_quit → call_debugger' = Debugger).
`C-x C-e' wertet EINEN Ausdruck aus — eine Advice ohne ihre `defun'
machte Emacs unbenutzbar.

**Regeln:** nie `inhibit-modification-hooks'/`with-silent-modifications'
um TEXT-Änderungen in org-Puffern (Text-Properties sind ok — so
arbeitet die Zettel-Hervorhebung); Tool-Dateien nie persistent
cachen.  Suite 2586 → **2588 ERT** (0 unexpected, 1 Skip), BDD grün.

### 5.69 Vokabular im Diskussionspapier (done, 2026-10-26)

Carsten (26.10.): „in den Handouts unter dem Text, vor der Übersetzung
noch das Vocabulary einfügen" — gemeint ist das Diskussionspapier
(C-c u D / Auto-Papier nach C-c u G), das am 20.10. bewusst ohne
Vokabular und Grammatik angelegt worden war.

- `--sentence-data` trägt jetzt `:vocab` (`tibetan-handout--vocab`:
  ★ Klassenwortliste zuerst, Kontextglosse nur wo sie abweicht, sa
  ohne ★ mit Lemma/Morph aus Word Analysis).  Die Wortliste bindet
  `--render-scope` einmal je Papier; `--sentence-data` lädt sie selbst,
  wenn der Aufrufer sie nicht gebunden hat (Tests, Einzelaufrufe).
- Renderer `--vocab-block`: Rubrik „Vokabular" zwischen Textblock und
  Übersetzung, je Segment eine Zeile (Segmentziffer vorn bei mehreren),
  Einträge mit · getrennt: `*term* [★ ]Glosse {{{ctx(Kontext)}}}
  {{{dim(Wortart; Skt.)}}}`; neues Makro `dim`/`\dimtext`.  Grammatik
  bleibt draußen (steht im CAT-Tool).
- Alle 22 Papiere neu gebaut und in EINER rmapi-Sitzung auf dem Gerät
  ersetzt (rm + put je Name; danach `find` auf Dubletten geprüft).

Tests: sentence-data (★/Glosse/Kontext), render (Reihenfolge, Einträge,
kein POISON-NA), render-gaps (Rubrik auch ohne Vokabular).

**Nachträge 26.10.** (`4678c62`): (1) rmapi-Lektion Nr. 4 — auch
INNERHALB einer Sitzung deckelt der Upload-Endpunkt: von 22 puts
scheiterten drei mit 429 (TĀ Satz 3, 4, 1001; nach 12 min einzeln
nachgeladen).  Sammel-Sitzungen auf ≈15 puts begrenzen.  (2) Auf dem
Gerät lagen `Reading/D/Satz 4, 6` = Kaskaden-Test-Fixture: ein ERT-Lauf
im interaktiven Emacs hat über den Auto-Haken hochgeladen (der Schutz
in run-all-tests.el gilt nur im Batch).  `--temp-source-p`: Quellen
unter `temporary-file-directory` bekommen nie ein Auto-Papier.
Suite 2586 (0 unexpected, 1 Skip).

### 5.68 Synopse-Vorspann im Handout: Siglen, Stellen, Parallele, Kommentar (done, 2026-10-21)

Carsten (21.10.) legte das Vorbereitungsblatt der Doktoranden zum
MSA-Reading vor (Zhoulin Li, `Preparations/MSA 10.21.pdf', Vers IX.60
mit Sthiramati): „Finde ich eigentlich besser als mein Handout, bzw.
könnte das am Anfang vom Handout stehen."  Befund: zwei Gattungen —
Vortragsblatt (Siglen, Stellen L/N/D/Q/T, Synopse Skt·Tib·Chin, Apparat,
Sthiramati lemmaweise mit Versworten **fett** und Bhāṣya-Worten
_unterstrichen_) gegen Lernblatt (Vokabular, Grammatik, Schreibrand).
Alles außer Chinesisch, Peking- und Lévi/Nagao-Apparat ist aus den
Kaskaden ableitbar (gleiche `** Section'-Titel, `:FOLIO:', `:VERSE:').
Entscheidung: als Vorspann vors Handout, ohne Thurman (die
Referenzsperre bleibt), ohne KI-Abruf.

- **Keywords** im Kopf der Quelle (Pfade relativ): `#+TIBETAN_SYNOPSIS:
  <Kaskade>' (wiederholbar — weitere Spalte der Tabelle),
  `#+TIBETAN_COMMENTARY: <Kaskade>' (wiederholbar — Kommentar unter der
  Tabelle), `#+TIBETAN_SIGLA: <org>' (Body wörtlich als `* Siglen',
  `#'-Zeilen weg), `#+TIBETAN_SIGLUM: L' (Spaltenkopf; Fallback
  Kurzname).  Metadaten-Leser: `:synopsis :commentary :sigla :siglum'.
  Bewusst NICHT `#+TIBETAN_PARALLEL_SOURCE' (§5.64 — das lässt C-c u G
  einen Sanskrit-Vergleich holen; die Synopse feuert nie).
- **Daten**: `tibetan-cascade--collect-sentences' liefert zusätzlich
  `:verse' (die :VERSE:-Property des Satzes) und `:folios' ((N .
  "D159a5") …) — additiv, kein Konsument bricht.  Handout:
  `--section-sentences' (Datei, Section-Titel), `--synopsis-column'
  (:siglum :lang :locator :sentences; Locator = Folio-Spanne erstes–
  letztes Segment ohne Editionsbuchstaben, sonst Vers-Id, sonst
  Section-Id; sa OHNE Rezitationspausen — `--seg-list' PLAIN),
  `--synopsis-commentary' (Läufe je Segment + Übersetzung des Werkzeugs
  je Satz, nie Referenzen — Poison-Test), `--synopsis-data' (:id :lead
  :parallels :commentaries; Kommentar-Zitate gegen die erste Spalte
  seiner Sprache), `--scope-sections' (Sections der Sätze im Umfang, je
  einmal), `--sigla-text'.  Build setzt `:sigla'/`:synopsis' nur, wenn
  die Quelle SYNOPSIS oder COMMENTARY nennt; alles andere unverändert.
- **Hervorhebung** `tibetan-handout--mark-quotes' (rein; Wylie-Silben,
  Shad als eigenes Token; über den GANZEN Abschnitt, weil Lemma und
  Marker oft durch Shad und Satzgrenze getrennt sind: „… yin te/ /"
  „zhes bya ba la/"): (1) LEMMA vor `zhes/ces/shes bya' bzw. `… smos'
  (zurück bis zum vorigen Shad, max. 16; nicht die Schlussformeln
  `… ba'i don to' / `… ste'): ≥ 2/3 der Inhaltssilben im Vers → `verse'
  (fett), sonst im Bhāṣya → `bhasya' (unterstrichen), Gleichstand →
  Vers.  (2) LÄUFE ≥ 2 Inhaltssilben, Inhaltssilbe zu Inhaltssilbe:
  im Bhāṣya → bhasya (Gleichstand → Bhāṣya: Begriffsechos), nur im
  Vers → verse.  Funktionssilben = `--particle-syllables' +
  Nominalisierer/Genitive/Kopula/Finalpartikel/Marker.  Ausgabe je
  Segment ((TEXT . KIND) …), Zwischenstücke wörtlich.  Goldstandard-
  Test aus dem Blatt (IX.60: Uddeśa-Lemma unterstrichen, „rang bzhin
  dang ni" / „rdzogs longs spyod" / „sprul pa'i sku yang gzhan yin te"
  fett, „chos kyi sku" / „sems can" / „longs spyod" unterstrichen,
  „pa ni"/„yin no" nie).  Bekannte Grenzen: Sthiramatis eigene
  Versübersetzung („thog ma … gnas" für „dang po … rten") wird nicht
  erkannt; Begriffsechos („rang bzhin", „sangs rgyas") werden auch dort
  unterstrichen, wo das Blatt sie frei lässt.
- **Renderer**: `* Siglen' (+ Konventionszeile), je Section `* Synopse
  <id>' in `\begin{landscape}' (pdflscape; der Umgebungsbeginn steht
  VOR der org-Überschrift, sonst bleibt sie allein auf der Hochformat-
  seite zurück): Stellenzeile „L MSA IX.60 · D4026 159a5–159a7 · D4034
  135b5–136a6", longtable mit n Spalten (0.98/n `\linewidth'), Vers
  `\textbf', Segment-Hochzahlen (`\segsup'), dann je Kommentar
  Siglum+Stelle und die markierten Sätze (`\textbf' / `\uline' aus
  ulem[normalem]), Übersetzung kursiv, wo gelandet; dann `* Text' und
  `* Satz N' wie bisher.  `--md-to-latex' für Claude-Prosa in
  Rohblöcken.  CJK-Fallback `tibetan-handout-cjk-font' (Hiragino
  Mincho ProN) in der luaotfload-Kette — Nagaos Titel in den Siglen
  zeigte Kästchen.
- **Korpus** (buddhist-studies): Keywords in `Sanskrit-9/msa-9-cat.org'
  (L; SYNOPSIS Tibetisch-9, COMMENTARY Sthiramati-9), `Tibetisch-9/
  mdo-sde-rgyan-9-cat.org' (D4026; symmetrisch), `Sthiramati-9/
  savbh-9-cat.org' (D4034; SYNOPSIS beide), neu `Resources/
  siglen-msa.org'; Probe `Sanskrit-9/handouts/msa-par-60.{org,pdf}'
  (5 S.).  Vorher die Sthiramati-Grenzen IX.60–62 von Hand gesetzt
  (`c28d40b2', `3c3529b6'): Beginn beim Uddeśa-Lemma (Satz 754, D135b5;
  das Skript hatte Pāda c, Satz 757), IX.61 ab 768, IX.62 ab 780 — 61/62
  fehlten ganz, IX.60 lief über 253 Sätze; die 26 Satzdateien neu
  materialisiert (Slots löschen, dann `materialize-provided-
  translations' — EINMALIG-Gate).
- **Parallel-Session** (21.10.): „Mahāyānasūtrālaṃkāra Reading Materials"
  materialisierte gleichzeitig Makransky/Radich im selben Ordner;
  Abstimmung per SendMessage, nur eigene Dateien gestaged, Commits
  sequenziert.

Suite 2579 → **2585 ERT** (0 unexpected, 1 Skip), BDD grün; `make
compile' warnungsfrei.

**Offen:** Chinesisch (T1604) und Peking-Apparat bleiben Handarbeit;
Sthiramatis restliche 24 unerkannte Section-Grenzen (Liste im
Dateikopf) nach demselben Muster — Lemma statt D4020-Wortlaut.

### 5.67 Bedeutungen von Präfix, Wurzel und Suffix als 3. Zeile (done, 2026-10-21)

Carsten (21.10., zu §5.66): „schön wäre es, wenn ich auch noch die
Bedeutung der Root hätte" — auf Nachfrage: „Zeile 3, dann kann gleich
auch die Bedeutung der Prä- und Suffixe mit rein".  Commit `c67b3dc'.

- **Ein Feld mehr, kein neuer Abruf**: die Begleitantwort aus §5.66
  trägt je Wort `- WORT — BILDUNG — BEDEUTUNGEN'; die Bedeutungen
  folgen der Reihenfolge der Bildung, je Teil ein bis drei Wörter,
  Präfix = Grundsinn, Wurzel = Grundbedeutung, Suffix = Funktion,
  Stamm = Bedeutung (`vi- apart · √muc free · -ta past participle').
  Pronomen/Partikeln: `- yasya — —'.  Sprache = `#+TIBETAN_TARGET_LANG'
  des Quelldokuments (de → German, sonst English);
  `tibetan-word-formation--system-prompt' ist jetzt eine Funktion der
  Zielsprache, je Sprache konstant (Cache-Präfix); `--inputs' trägt
  :target.
- **Parser**: Einträge sind Plists `(WORT :formation F :meaning M)';
  zweiteilige Zeilen bleiben gültig (keine Bedeutung → keine Zeile 3),
  leeres oder `—'-Feld = nil.  Token :formation-meaning.
- **Tabelle**: sa mit Bildung UND Bedeutungen → FÜNF Zeilen (Wort /
  Bildung / Bedeutungen / Glosse / Label); nur Bildung → vier; sonst
  drei; Tibetisch unverändert.

**Stand Korpus:** wie §5.66 — Zeilen 2 und 3 erscheinen mit dem
nächsten C-c u G.  Suite **2579 ERT** (0 unexpected, 1 Skip; Tests
angepasst, keine neuen); compile sauber; REFERENCE.org regeneriert.

### 5.66 Wortbildung (Präfix + √Wurzel) als 2. Zeile der Sanskrit-Glossentabellen (done, 2026-10-21)

Carsten (21.10.): „Mein Sanskrit ist noch sehr, sehr basic.  Ich möchte
die Reading-Klassen auch dafür nutzen, das zu ändern.  Für die Zettel
haben wir eine Funktion, die Präfix und Wurzel zerlegt.  Das hätte ich
gerne in der Tabelle als zusätzliche Zeile (als 2.)."  Commits
`e8e74a6' (Bugfix vorab) `194c225' `5fbb57d'; Korpus `c14700fd'.

- **Bugfix vorab** (`e8e74a6'): das Formlabel der Word Analysis wurde
  hinter dem LETZTEN Semikolon gesucht; Kommentar in Klammern mit
  eigenem Semikolon („(compound; subject …)") landete in Zeile 3.
  Jetzt: erstes Semikolon bis zur Klammer.  TĀ: 2 Sätze neu gerendert.
- **Die Zettel-„Funktion" ist keine Rechenregel**: :ETYMOLOGY: schreibt
  Claude beim Anlegen (SKOS-DESIGN.org).  Die parallele Sitzung (`d86e170',
  §5.63-Nachtrag) zeigt sie im Tooltip und über C-c u z w.  Für die
  Tabelle darum dasselbe Prinzip: NEUES Modul
  `persist/tibetan-word-formation.el' — kleiner Claude-Abruf je
  Sanskrit-Satz über die Wörter der Word Analysis (Form, Lemma,
  Label), Antwort `### Segment N' / `- WORT — BILDUNG' in kompakter
  Notation (vi-√muc-ta · √iṣ-ana · √vid · √kṛ-yā · sva-tantra-ya · —;
  keine Glossen, keine Endungen).  Lemmata mit Zettel-Wortbildung
  gehen als CURATED-Vorgabe mit (`--curated' über
  `tibetan-zettel-highlight--etymology', DRY `5fbb57d').  Landung
  `** Word Formation' in * Sanskrit Analysis nach der Word Analysis,
  dann reines Re-Render.  Auslösung: Haken nach der Claude-Landung
  (`tibetan-word-formation-auto', Default t) und C-c u G für schon
  analysierte Sätze (meldet KEINEN Begleiter — das Diskussionspapier
  zeigt die Tabellen nicht und darf nicht darauf warten);
  `tibetan-word-formation-dwim' (Menü) erzwingt neu.  Abruf über
  `tibetan-discussion--submit'.
- **Lesemodul/Tabelle/Regenerate**: Parser `tibetan-sanskrit-reading-
  parse-word-formation', textkeyed Dynamik `--word-formation', Token
  :formation; `tibetan-gloss-table--unit-rows' liefert bei sa mit
  Bildung VIER Zeilen (Wort / Bildung / Glosse / Label), sonst drei;
  der Kaskaden-Regenerate liest, bindet und setzt `** Word Formation'
  wieder ein (wie Word Analysis); hartes require des Lesemoduls.
  Bearbeitete Glossentabellen (§5.54-Schutz) bekommen die Zeile nicht.

**Stand Korpus:** noch kein Satz hat eine Wortbildung — sie kommt mit
dem nächsten C-c u G (für TĀ 1–4, 1001/1002 nur der kleine Abruf).
**Offen:** Wortbildung im Handout-Vokabular (nur auf Zuruf).

Suite ERT 2569 → **2579** (0 unexpected, 1 Skip); BDD grün; compile
sauber; REFERENCE.org regeneriert.

### 5.65 Metrum und Rezitationspausen für Sanskrit-Verse (done, 2026-10-21)

Carsten (21.10., Tantrāloka, nach Satz 1001 = Jayarathas erstem
Einleitungsvers): „Es geht mit einer Rezitation los, da bin ich mit
der Metrik nicht klar gekommen.  Ginge es, dass wir eine Kennzeichnung
der Pausen beim Rezitieren reinbekommen?"  1001 ist Śārdūlavikrīḍita
\(Pause nach der 12. Silbe), die Einleitungsverse wechseln das Metrum,
TĀ 1.1 ist Nardaṭaka.  Commits `ac176ef' `022efe1' `fe40e90'; Korpus
`0c5d2a53'.

- **Modul** `analysis/tibetan-sanskrit-metre.el' (rein, IAST):
  Skansion (`tibetan-metre-syllables'/`-scheme': schwer = langer Vokal,
  Anusvāra/Visarga, 2+ Konsonanten bis zum nächsten Vokal über
  Wortgrenzen, Schlusskonsonant; Aspiraten = 1 Konsonant; Avagraha,
  Daṇḍas, Ziffern keine Silben).  Erkennung `tibetan-metre-identify'
  über die Zeilen EINES Verses: 21 Sama-Metren mit Yati (Macdonell
  App. II; Pṛthvī/Praharṣiṇī/Mattamayūra nach Vṛttaratnākara),
  Upajāti-Mischungen, Puṣpitāgrā, Vaitālīya/Aupacchandasika nach Moren
  \(6/8 Moren + Schluss), Śloka (Mehrheit der geraden Pādas ⏑ – ⏑ in
  5–7), Āryā/Gīti/Upagīti/Udgīti/Āryāgīti nach Moren (Zäsur nach 12).
  PROSA-SPERRE: Zeilenenden müssen Pāda-Grenzen sein; eine Zeile gilt
  nur mit `//' als Vers (die Durchsicht fand Viveka-Prosa 1081/1099/
  1106 mit zufällig 32/48 Silben als „Śloka").  Pausen
  `tibetan-metre-mark-segments': ` ¦ ' Zäsur, ` | ' Pāda-Ende in der
  Zeile, im Wort an der Silbengrenze (erster Konsonant einer Gruppe
  schließt: rahas¦yā).  `tibetan-metre-section' (der Block),
  `-label', `-table'.  Nicht erkannter Vers (`//', Zeilen ≤ 44
  Silben) → sichtbarer Hinweis mit Skansion je Zeile.
- **Kaskade**: `tibetan-cascade--reading-section' setzt bei sa
  `** Metre' als ERSTES Kind von * Reading (vor die Glossentabellen;
  enthält weder `*** Segment' noch ⟦N⟧, der Edit-Schutz-Restore
  ersetzt in place).  Deterministisch, Regenerate baut ihn frisch.
- **Ausdrucke**: `tibetan-handout--seg-list' (geteilt) setzt die
  markierten Zeilen ein; `--metre' liefert Label + Tabelle; Handout:
  Metrum-Zeile über jedem Vers im Text-Teil + `** Metrum' mit
  Silbentabelle je Satz (auch ohne Analysedatei); Papier: Metrum-Zeile
  + Pausen.  ⏑ → \newunicodechar \smile (Libertinus hat es nicht).
- **Durchsicht TĀ 1** (Verse + Viveka): Śloka 318, Gīti 4,
  Vasantatilakā 4, Udgīti 3, Śārdūla 2, Upajāti 2, Upagīti 2, Āryā 2,
  Aupacchandasika 2 (1.13, 1.333), Nardaṭaka (1.1), Āryāgīti (1.21),
  Pṛthvī, Sragdharā, Mandākrāntā.  Nicht erkannt: 1.48 (33 Silben —
  GRETIL *saṃbhaveta*, metrisch *saṃbhavet*), 1.98 (2. Halbvers 29
  Moren), 1.104 (drei Halbverse), 1.201 (Śloka-Halbzeile + zitierte
  Rathoddhatā in einem Satz).
- **Korpus**: alle 444 Satzdateien render-only neu (migrate-structure-
  v2; Trockenlauf auf Kopie, Diff nur Metre + LAST_ANALYZED + eine
  verschobene Leerzeile in PT bei 6 Dateien; Zweitlauf idempotent).
  Die schon hochgeladenen reMarkable-Papiere (TĀ 1–4, 1000–1002)
  tragen die Pausen erst nach erneutem C-c u D.

**Offen / Ausbau:** Kokilaka (Nardaṭaka mit Yati 7/13) nicht getrennt;
gemischte Sätze (1.201) je Zeilengruppe erkennen; Vipulā-Formen des
Śloka benennen statt nur zu vermerken.  MSA/AS-Korpora tragen den
Abschnitt erst nach ihrem nächsten Regenerate.

Suite ERT 2553 → **2569** (0 unexpected, 1 Skip); BDD grün; compile
sauber; REFERENCE.org regeneriert.

### 5.64 Diskussionspapier fürs reMarkable (C-c u D) + Sanskrit–Tibetisch-Vergleich (done, 2026-10-20)

Carsten (20.10., Reading-Klassen WS26): „In der Klasse habe ich den
Text im CAT-Tool offen.  Das Handout würde ich gerne in ein
Diskussionspapier umwandeln — Concept Notes und Diskussion im
wesentlichen — immer parallel auf dem reMarkable … bei zweisprachigen
Texten ein Vergleich des Tibetischen mit dem Sanskrit … wenn es auf
eine Seite passt."  Entscheidungen (AskUserQuestion): reMarkable-
Format; Text + Übersetzung + Concept Notes + Diskussion; Vergleich als
eigener Abschnitt der Satzdatei, mit C-c u G geholt; PDF + rmapi.
Nachtrag: auf dem Gerät Ordner je Werk, der Satz als Dateiname.
Commits `6d53470` … `e292b60`.

- **Refactors vorneweg** (`6d53470`, `102004a`): Diskussion →
  `--section-body` / `--parse-points` / `--write-pt-child` /
  `--submit` (KIND, Prompt-Thunk, Landungsfunktion; In-flight-Schlüssel
  "KIND:truename"); Handout → `--seg-list`, `--latex-commands`,
  `--output-file` mit KIND.  Beide ohne Verhaltenswechsel.
- **Sanskrit-Vergleich** `persist/tibetan-sanskrit-comparison.el`
  (`8de8ee5`): tibetische Kaskade trägt `#+TIBETAN_PARALLEL_SOURCE:
  ../Sanskrit/msa-1-cat.org`; gleiche `** Section`-Titel verbinden die
  Verse.  Claude bekommt den tibetischen Satz (Uchen + Wylie) und ALLE
  Sanskrit-Sätze der gleichnamigen Section (Satz-zu-Satz-Zuordnung
  unbekannt) und antwortet `## Correspondence` / `## Divergences`
  (`### N. <Wylie>`, Zeilen Tibetan / Sanskrit / Divergence /
  Significance) / `## Assessment`.  Landung `** Sanskrit Comparison`
  unter * Provided Translations (Labels in der Dokumentsprache,
  Einschätzung als letztes `***`-Kind; überlebt den Regenerate); die
  Diskussion schließt den Abschnitt aus ihren Referenzen aus
  (`tibetan-discussion--comparison-heading` lebt im Diskussionsmodul,
  Zyklusvermeidung).  Abruf: `tibetan-cascade-after-fetch-functions`
  am Ende von `tibetan-cascade-fetch` (C-c u G; je Satz des Scopes,
  auch vollständige; nie nach abgelehntem Abruf; `:compared` +
  Meldung); Gate (Section mit Gegenstück, noch kein Vergleich) im
  Haken UND in `--request` — RED zeigte beim Stubben den Titel-Satz
  mitgezählt.  `tibetan-sanskrit-comparison-dwim` (Menü) erzwingt neu.
  Korpus: `Mahayanasutralamkara/Tibetisch/mdo-sde-rgyan-1-cat.org`
  trägt das Keyword (buddhist-studies).
- **Diskussionspapier** `persist/tibetan-discussion-paper.el`
  (`d9d1808`), C-c u D: Scope wie C-c u H.  DATA `--sentence-data`
  (Segmente, Übersetzung, Concept Notes als Klartext, Diskussion,
  Vergleich — kein Vokabular/Grammatik); RENDERER A5 (= reMarkable-2-
  Schirm 157,8 × 210,4 mm auf einen Zentimeter), 7-mm-Ränder, 9 pt,
  kein `* Text`; je Satz Uchen/Wylie-Zeile, Rubrik Übersetzung,
  `** Begriffe`, `** Sanskrit-Vergleich` (nur wenn vorhanden),
  `** Diskussion` (geteilter Handout-Block); Lücken sichtbar; nie
  „Claude", nie Referenz-Volltext.  Datei `handouts/<short>-disk-…org`,
  Export über `tibetan-handout-export-pdf`; C-u nur die .org; in der
  erzeugten .org exportiert + lädt C-c u D die bearbeitete Fassung.
  **Upload** `--upload PDF TITLE LABEL`: rmapi `mkdir /Reading`,
  `mkdir /Reading/<Werktitel>`, `put <Label>.pdf` („Satz 28", „Sätze
  24–28", „§220"; Titel/Label aus der #+TITLE-Zeile); schlägt put fehl
  (Name vergeben) einmal mit Zeitstempel — Notizen auf dem früheren
  Dokument bleiben; ohne rmapi Meldung.  Asynchron (`--rmapi` via
  make-process, stubbar).  `tibetan-discussion-paper-remarkable-folder`
  ("Reading"), `tibetan-discussion-paper-upload` (t).
- **Bugfix unterwegs** (`e292b60`): Diskussions-Punkttitel mit
  Markdown-Kursiv (`*mahā jananī*`) wurden ungewandelt fett gerahmt →
  rohe Sterne (Handout UND Papier, seit §5.62); Titel/Text laufen jetzt
  durch `--discussion-text`.

- **Nachträge nach dem ersten echten Upload** (`965a560`, `02b2ccd`):
  gleichnummerierter Sanskrit-Satz = Gegenstück (:counterpart; AS-
  Kaskaden tragen dieselben Nummern, MSA nicht); rmapi-Aufrufe laufen
  NACHEINANDER (`--rmapi-queue`/`--rmapi-next`) — drei Papiere parallel
  = neun Prozesse = 429 für alle, und die parallelen `mkdir /Reading`
  legten DREI gleichnamige Ordner an (von Hand bereinigt; rmapi löst
  Pfade mit Dubletten unzuverlässig auf — nie wieder parallel).  Live:
  Tibetisch V Satz 1 / Sätze 2–3 / Sätze 4–9 liegen in
  `/Reading/bDen pa gnyis la 'jug pa (Satyadvayāvatāra)/`.

- **Nachträge 20.10. nachmittags** (`2978467`, `047be3d`): ASCII-Namen
  auf dem Gerät (`--doc-name`: der Ordner mit Apostroph/Klammern/ā ließ
  sich auf dem reMarkable nicht öffnen; jetzt `/Reading/bDen pa gnyis la
  jug pa Satyadvayavatara/Satz N`).  **Auto-Papier**: C-c u G endet mit
  dem Ein-Satz-Papier je Satz des Scopes (`tibetan-cascade-fetch-done-
  functions` → sofort, wenn nichts anzufragen war, sonst `--pending` bis
  die Landungs-Hooks — Claude, DM, `tibetan-discussion-after-land-
  functions`, `tibetan-sanskrit-comparison-after-land-functions` —
  `--complete-p` melden); kein PDF-Öffnen, unverändertes Papier wird
  nicht erneut hochgeladen; Schalter `tibetan-discussion-paper-auto`.
  C-c u G fordert jetzt auch die fehlende DISKUSSION an
  (`tibetan-discussion-fetch` am after-fetch-Haken — Referenz nach der
  Analyse hinzugefügt, AS/Rahula); Begleit-Haken geben Dateilisten
  zurück, `:companion`.  **Suite-Schutz**: run-all-tests.el setzt Auto +
  Upload aus — ein Kaskaden-Fetch-Test hatte echte rmapi-Aufrufe
  gemacht (Ordner „D" auf dem Gerät).  Headless-Rezept für Nachzieh-
  Abrufe mit Carstens gptel: `(package-initialize) (require 'gptel)
  (setq gptel-model "claude-opus-4-8" gptel-use-curl t)` vor
  tibetan-cat.el, dann `tibetan-cascade-fetch` + Warteschleife
  (`accept-process-output nil 1`) bis Queue, `--pending` und rmapi-Queue
  leer sind (Skript im Sitzungs-Scratchpad `update-asam.el`).

- **rmapi-Lektion Nr. 2** (`b1006a1`): auch seriell 429 — jeder
  rmapi-AUFRUF holt ein Token, der Token-Endpunkt deckelt pro
  Zeitfenster (und blockiert dann wohl auch den Geräte-Sync desselben
  Kontos: Carsten sah die Werkordner ohne `/Reading` und konnte sie
  nicht öffnen).  Darum `--rmapi-session`: EIN rmapi-Prozess im
  Shell-Modus, Befehle über stdin (`--rmapi-script`, Quoting), ein
  Token je Upload; `tibetan-discussion-paper-upload-many` = viele
  Papiere in EINER Sitzung.  Nach einem 429 mindestens 10 Minuten
  warten, nie „noch einmal probieren".

- **Schrift** (`35eefca`): Tibetisch außerhalb der Uchen-Zeile zeigte
  Kästchen (Libertinus) — gemeinsame Preamble `tibetan-handout--latex-
  fonts` mit Noto Serif Tibetan als luaotfload-Fallback des Hauptfonts.
- **rmapi-Lektion Nr. 3**: `rm <pfad>` löscht nur den ERSTEN Treffer;
  beim Austausch (rm + put gleicher Name) kann ein Duplikat
  zurückbleiben, das Gerät zeigt dann das alte.  Nach einem Austausch
  `find` lesen und doppelte Namen erneut löschen.  Live-Stand 20.10.
  abends: `/Reading/` mit bDen (Satz 1–9), AS Sanskrit (1–3, 1001–1002),
  AS tib. (1), Tantrāloka (1–4, 1000–1002) — Carsten: „Jetzt passt
  alles."

**Probelauf** Tantrāloka V. 1–2: 5 A5-Seiten; Vers 1 mit fünf
Diskussionspunkten knapp zwei Seiten — inhaltsbedingt.  **Offen:**
Sanskrit-Vergleich noch an keinem echten Satz gefeuert (API nur auf
Zuruf); erster C-c u G auf MSA I.1 zeigt, ob die Section-weite
Sanskrit-Eingabe (Vers + Bhāṣya) im Antwortlimit bleibt.

Suite 2521 → **2553 ERT** (0 unexpected, 1 Skip); compile clean;
REFERENCE.org regeneriert.

### 5.63 Zettel-Hervorhebung: Wörter mit Zettel unterstrichen + verlinkt (done, 2026-10-18)

Carsten (16.10., Tantrāloka als erstes Testprojekt des Research-
Workflows): „ideal wäre es, wenn im CAT alles, was Zettel hat,
hervorgehoben und verlinkt wäre."  Entscheidungen (AskUserQuestion):
Quelltext UND Analyse-Dateien; RET/Klick öffnet in der Research-
Instanz (shared-route), Fallback lokal.  Plan
`~/.claude/plans/try-again-greedy-kay.md`; RED-first.

- **Index** = EINE JSON-Datei (`~/.cache/buddhist-studies/zettel-
  index.json`) aus `skos_index.py` im Tools-Repo (Schlüssel wie
  skos_context.search normalisiert: klein, Diakritika weg, Tibetisch
  ohne Tsheg; Homonyme behalten beide IDs; Titel-Schlüssel nur für
  Begriffs-Typen).  Der Wylie-Index `tibetan-zettel.el` bleibt — er
  speist den Glossen-Tier; dieses Modul hebt nur hervor.
- **Modul** `core/tibetan-zettel-highlight.el`, buffer-lokaler Minor-
  Mode, Font-Lock-Funktionsmatcher (§5.35-Muster, §5.38-H3-Guard),
  Facespec als ausgewertete Form: jeder Treffer trägt
  `tibetan-zettel-id`, Keymap (RET/mouse-1/mouse-2), help-echo,
  rear-nonsticky — alle in `font-lock-extra-managed-props`, damit das
  Ausschalten keine klickbaren Reste lässt.  Auch
  `org-open-at-point-functions` (C-c C-o).
- **Kein Stamm-Raten** (Flexion, nicht Sandhi, ist das Problem;
  `jñāna` in `vijñāna`, Konsonantstämme).  Analyse-Dateien: Lemma-
  Hälfte der Word-Analysis-Bullets exakt; Gloss-Tables Zeile 1 und
  Vocabulary-Headwords über die Lemma-Map derselben Datei
  (Parser `tibetan-sanskrit-reading-parse-word-analysis`); Komposita
  gliedweise.  Quelltext: je `*** Sentence N` PROJEKTION der Word
  Analysis der Satzdatei (suffix-aware über `tibetan-sentence--
  filepath`): jedes Oberflächen-Glied mit Zettel wird über seine
  Mitte (erstes/letztes Zeichen weg) im sandhi-verschmolzenen Segment
  gefunden; Cache je Satz, invalidiert durch
  `tibetan-cascade-after-land-functions` → ein Vers leuchtet, sobald
  er per C-c u G analysiert ist.  Quelltext-Erkennung: kein Analyse-
  Name (die Analyse-Dateien tragen DIESELBEN Kopfzeilen) UND CAT-
  Kopfzeile.
- **Tasten/Menü**: `C-c u z h` Toggle, `C-c u z H` Index neu bauen;
  `C-c u Z` öffnet zuerst das markierte Wort (Homonyme →
  completing-read über Titel).
- **Lektion**: `string-trim` ist regex-basiert und überschreibt die
  Match-Daten — Positionen direkt nach `string-match` sichern.

Phase 2 (offen): Tibetisch-Schrift-Quellen (Tsheg-Längstmatch);
opt-in Rate-Modus mit eigenem Face für unanalysierte Verse.
Suite ERT 2505 → **2521** (0 unexpected, 1 Skip); BDD grün; compile
sauber; REFERENCE.org regeneriert.

**Nachtrag 2026-10-21 — Wortbildung im CAT.**  Carsten (Tantrāloka-
Reading, 20.10.): bei Sanskrit-Begriffen eine Bedeutungszerlegung
(englisch: Präfix → Bedeutung, Wurzel → Bedeutung), „kann als Funktion
dann auch im CAT-Tool genutzt werden".  Die Zerlegung lebt im Zettel
(`:ETYMOLOGY:` + Zeile `Wortbildung: …` unter der Kopfzeile; Notation
in `~/emacs-zettelkasten/SKOS-DESIGN.org`, Lint `missing-etymology`),
`skos_index.py` liefert sie als Feld `etymology`, der Index-Plist
trägt `:etymology`.  Der Tooltip zeigt sie als zweite Zeile unter
„title — de / en"; neues Kommando `tibetan-zettel-highlight-etymology`
(`C-c u z w`, Menü „Wortbildung at Point") gibt `lemma — de / en —
Zerlegung` im Echo-Bereich aus (`C-u` kopiert; ohne Zettel: Hinweis
auf /term; ohne Eintrag: „keine Wortbildung im Zettel").  Nicht in v1:
Zerlegung ad hoc per Claude für Wörter ohne Zettel; Einspeisung in den
`C-c u G`-Prompt (der Index trägt sie dann schon).  Nach `C-c u z H`
(Index neu) stehen die Zerlegungen der Chargen vom 21.10. bereit.
Lektion: `make compile` hinterlässt `.elc` in `core/`; ein Test, der
nach einer Quelländerung „unerklärlich" rot bleibt, lädt die alte
`.elc` — erst `make compile`, dann testen.  ERT 2521 → **2571**
(mit zwei neuen Tests hier; die übrigen kamen aus §5.64/§5.65).

**Nachtrag 2026-10-27 — Kompositionsglieder.**  Carsten (TĀ-Reading):
„Mantra fehlt" — der Zettel existierte, aber /mantra/ leuchtete im
Kompositum nicht, weil die Word Analysis Komposita als EIN fusioniertes
Lemma gibt (`- mantra-vīryam — mantravīrya;`): der Lemma-Schlüssel
`mantravirya` hat keinen Zettel.  Jetzt (1) markiert die Word-Analysis-
Zeile bei Bindestrich-Oberfläche zusätzlich die Oberflächenglieder
einzeln (`--analysis-line-hits`), und (2) löst `--member-hits` das
flektierte Endglied über `--stem-in-lemma-ids` auf: Kandidaten sind die
Präfixe des Glieds und die Schwänze des fusionierten Lemmas; ein Stamm
zählt, wenn er einen Zettel hat, im Lemma vorkommt und das Glied mit ihm
beginnt (Endvokal darf vom Suffix geschluckt sein: hṛdaya → hṛdayena).
Kein Raten über Sandhi hinweg (`kulam` in `parakalā` → nichts).  Gilt
für Analyse-Zeilen, Gloss-Tables-Zeile 1, Vocabulary und die
Quelltext-Projektion.  Zwei Tests; ERT (mit den Suiten anderer Sessions seit dem 21.10.) **2590**, 0 unexpected, 1 Skip; BDD grün.

### 5.62 Übersetzungs-Diskussion für das Seminar (done, 2026-10-12)

Carsten (12.10., Tantrāloka): „Was es perfekt machen würde, wäre eine
Einschätzung der Dyczkowski-Übersetzung, Hervorhebung der Teile, die
diskussionswürdig sind.  Claude und DharmaMitra übersetzen in der
Regel anders als die zur Verfügung gestellten Übersetzungen, und das
diskutieren wir im Seminar.  Als generelles Modul wäre das super."
Entscheidungen (AskUserQuestion): automatisch mit C-c u G (schaltbar);
Sprache = Zielsprache des Dokuments; verglichen werden
Referenzübersetzungen + Claude + DharmaMitra (NICHT die Working
Translation); ins Handout je Satz.  Commits `56608f8' … `1e496c1'.

- **Vorarbeiten**: `56608f8' geteilter Slot-Text-Reiniger
  `tibetan-cascade--clean-slot-text' (Maschinen-Platzhalter, Drawer,
  „(Sentence …)"-Label — Handout und Diskussion lesen Slots gleich);
  `b9fed88' Hooks `tibetan-cascade-after-land-functions' (Ende von
  `--land-response') und `tibetan-dharmamitra-translation-after-
  write-functions' (Ende des Nested-Writers), je in condition-case.
- **Bugfix unterwegs** (`a379f17'): der `* Translation Comparison'-
  Abschnitt der par-Dateien empfahl „Refresh via C-c u T" — C-c u T
  ist translate-region, der Vergleich liegt seit langem auf C-c u c.
- **Modul** `persist/tibetan-translation-discussion.el' (`288bd89'):
  Eingaben = PT-Kinder außer DM / Claude Particles / Discussion /
  „Übersetzung…" (Referenzen), Claude aus `* Translation', DM aus dem
  Slot (beide über den Reiniger), Quelltext + Wylie (bo) bzw.
  Padapāṭha aus `** Word Analysis' (sa).  System-Prompt konstant je
  Zielsprache (eigener Cache-Präfix); Antwortschema `## Assessment' /
  `## Discussion Points' mit `### N. <Quellwort>' und Zeilen
  Text / <Referenz> / Claude / DharmaMitra / Issue / Assessment /
  Question — Zitate WÖRTLICH.  Landung: `** Discussion' (:GENERATED:)
  als letztes PT-Kind; `*** <Referenz> — marked|markiert' = Referenz,
  in der das TOOL die Zitate sucht (whitespace-tolerant) und fett +
  ⟨N⟩ markiert — nicht gefunden = unmarkiert, eine falsche Stelle kann
  nie markiert werden; der Marker steht IN der Fettung (`*x⟨1⟩*' —
  nach schließendem `*' verlangt org Leerraum/Satzzeichen, `*x*⟨1⟩'
  wäre nicht fett; Test mit org-element als Schiedsrichter); gekürzte
  Referenzlabels („Dyczkowski:") werden zugeordnet.  Punkte als
  Beschreibungsliste, Labels in der Dokumentsprache (de: Problem /
  Einschätzung / Frage, „— markiert").  Überlebt den Regenerate
  (unbekanntes L2 im PT-Body).
- **Auslösung**: `tibetan-discussion--maybe-fire' an beiden Hooks —
  wer von Claude/DM als ZWEITER landet, löst aus; Gates: Schalter
  `tibetan-discussion-auto' (Default t), Kaskaden-Datei, kein
  DEFER_MT, noch keine Diskussion, Referenz + Claude + DM vorhanden.
  Ein Request je Datei gleichzeitig (Inflight-Tabelle), Landung per
  run-at-time außerhalb des gptel-Sentinels (§5.40), gptel-max-tokens
  wie §5.61.  Abhängigkeiten HART geladen (defer-MT-Prädikat per
  fboundp = B0-Falle).
- **C-c u c** = `tibetan-translation-compare-dwim': in Kaskaden-
  Quelle/sent-Datei Diskussion für den Satz bzw. alle Sätze unter der
  Überschrift (erzwungen neu; > 10 mit Rückfrage; Sätze ohne
  Claude/DM/Referenz werden gezählt, erst C-c u G); in par-NNN.org
  unverändert die Ähnlichkeitsmatrix.
- **Handout** (`1e496c1'): `tibetan-discussion-read' liest die
  geschriebene Form zurück (kanonische Schlüssel, NIE der markierte
  Volltext — zitieren ja, reproduzieren nie); `** Diskussion' nach der
  Grammatik, deutsche Labels; Claudes Übersetzung heißt dort wie ihr
  Abschnitt: /Übersetzung/ (Label und Fließtext — das Handout sagt
  nie „Claude").  Die Handout-Fixture trägt Lopez-Poison im
  markierten Block → der Build-Test verriegelt das Nicht-Durchsickern.

**Kosten**: ein zusätzlicher Claude-Call je Satz mit Referenz
\(Tantrāloka: 288 Verse mit Dyczkowski; Rgyan: Lopez/W&M) — also drei
Calls je Vers mit C-c u G (Claude, DM, Diskussion).  Bereits geholte
Verse (TĀ 1.1) bekommen die Diskussion NICHT automatisch (die Hooks
feuern nur bei einer Landung) — dort C-c u c.

Suite ERT 2489 → **2505** (0 unexpected, 1 Skip); BDD grün; compile
sauber; REFERENCE.org je def-Commit.

### 5.61 Sanskrit-Analyse: Überschrift, ausführliche Grammatik, Antwortlimit (done, 2026-10-12)

Carsten (12.10., Tantrāloka): „Tibetan Analysis als Überschrift ist
hier falsch" und „da mein Sanskrit sehr schwach ist, wäre eine
ausführlichere Grammatik-Erklärung (Sandhis, Geschlecht, Stamm,
Deklination, Verbform …) sehr willkommen".  Commits `d3e54ae` …
`1014a6c`.

- **Antwortlimit** (`d3e54ae`, Bug): das Tool setzte nie
  `max_tokens'; gptel-anthropic schickt dann 4096.  Neu
  `tibetan-claude-max-tokens' (16000, tibetan-claude-queue.el) als
  `gptel-max-tokens' an allen vier Request-Stellen gebunden.
  Wahrscheinliche Ursache der abgeschnittenen sent-654-Antwort (B4).
- **`* Sanskrit Analysis'** (`0698593`): Scaffold nach Quellsprache;
  `tibetan-analysis--cascade-analysis-heading-re' (beide Namen) NUR in
  Kaskaden-v2-Pfaden — Zwei-Datei-Parallel-Dateien tragen ein ANDERES
  `* Sanskrit Analysis' (§5.17) über ihrem `* Tibetan Analysis';
  `--claude-segment-layout-p' erkennt v2 zusätzlich an `* Translation';
  `Sanskrit Analysis' in `--known-l1-sections' → Regenerate migriert.
- **Ausführliche sa-Grammatik** (`93726b0`): `## Grammar' des
  sa-Kontrakts didaktisch („reader with WEAK Sanskrit"), je Segment
  *Sandhi:* / *Nominal forms:* / *Verbal forms:* / *Compounds:* /
  *Syntax:*.  Das Handout zeigt bei sa alle (Fallback des Filters).
  Achtung: Grammar ist NICHT Teil des Fire-Gates — bereits gelandete
  Verse (TĀ 1.1) bekommen die neue Grammatik nur per C-u C-c u R.
- **#+CREATED** (`1014a6c`, Bug): Regenerate stempelte das
  Anlagedatum mit heute (Rgyan sent-001: 08-11 → … → 10-09);
  jetzt erhalten.
Korpus `56ae20d': Tantrāloka 332 + MAv-sa 1 neu gerendert (nur
Überschrift).

### 5.60 KI nur auf Abruf (C-c u G) · Vers-Referenzen · Tantrāloka-Rümpfe (done, 2026-10-12)

Carsten (12.10., Tantrāloka-Reading, Sanskrit → Englisch): „Ich will
nicht gleich alle Verse komplett analysieren … alle Analysefiles
erstmal als Rumpf mit der/den Übersetzung/en, alles ohne AI; ein
extra Kommando holt bei Claude und DharmaMitra ab."  Entscheidungen
(AskUserQuestion): ÜBERALL so (auch Rgyan); Dyczkowski auch als
Claude-Kontext.  Commits `3348fed` … `cb46405`.

- **A1 C-c u A öffnet nur** (`3348fed`): `tibetan-auto-fire-claude-
  on-create' / `-dm-on-create' Default nil (gelten fürs Anlegen UND
  Öffnen; t = altes Automatikverhalten).  RED zeigte UNGESCHALTETE
  Fires in den Zwei-Datei-Öffnern (Segment: Claude/DM/Parallel beim
  Anlegen UND Wiederöffnen; Satz; Absatz) — jetzt an den Schaltern.
  `tibetan-cascade-analyze-subtree' = Rumpf (`--ensure-files') +
  öffnen.  C-c u B / C-c s N = Rumpf des Dokuments.  C-c u R bleibt.
- **A2 C-c u G** (`31d673e`): `tibetan-fetch' → `tibetan-cascade-
  fetch' über den geteilten `tibetan-cascade--scope-at-point'
  (`7a77203', aus dem Handout umgezogen): Rumpf sicherstellen, Gate
  `--sentence-needs-fire-p', FORCE nil, unabhängig von den
  Schaltern; > `tibetan-cascade-subtree-confirm-threshold' (10)
  y-or-n-p.  Zwei-Datei-Quellen: Hinweis auf C-c u R.
- **B Vers-Referenzen** (`a277807`): `#+TIBETAN_SENTENCE_REFS:' →
  Datei mit `** Sentence N' / `*** <Name>'; generischer
  `--ref-bodies' (§ und Satz), `--refs-block' für Satz- UND Chunk-
  Prompt (Kontext, nicht kopieren), Materialisierung §+Vers,
  `--create-file' materialisiert sofort (Rumpf trägt die Übersetzung;
  `cb46405': ohne `~'-Backup — Fixtures unter temporary-file-directory
  sichern nie, der Test schaltet `backup-enable-predicate' frei).
- Kleinere Fixes vorab: sa-Tokenizer überspringt GRETIL-Versnummern
  (`83d12d4'), erklärende Meldung statt „Not in a segment or
  paragraph" (`b83d075').  `65b4e47' räumt zwei durchgerutschte
  Compile-Warnungen aus `7a77203' auf.

**Korpus** (buddhist-studies `4146e15'): `WS26-27/Tantraloka/
tantraloka-1.org' ist Sanskrit-Kaskade (Vers = `*** Sentence N' mit
:VERSE: TĀ 1.N, Halbvers = Segment, `** Section <Dyczkowski>',
Zielsprache en).  `Resources/dyczkowski-ta1.org' = Dyczkowski 2023 je
Vers aus dem OCR-Textlayer (Skript im Sitzungs-Scratchpad: Halbverse
gegen 1–3-Zeilen-Fenster gematcht — er setzt einen Pāda pro Zeile;
Übersetzung bis zur Marke „(N…) (KSTS…)", Fußnoten/Seitenköpfe
übersprungen; 288 auto / 43 prüfen (nur Kommentar, nicht
materialisiert) / 1 fehlt).  332 Rümpfe in analysis/, 288 mit
Dyczkowski.  Freischalten eines prüfen-Verses: `*** Dyczkowski 2023'
in der Referenzdatei anlegen, dann `tibetan-cascade-materialize-
provided-translations' auf analysis/.

**Offen:** prüfen-Fälle (Carsten); Bereichsangaben können bei direkt
aufeinanderfolgenden Versen zu weit greifen (1.250); Handout zeigt
„Satz N" statt „TĀ 1.N" (= gleiche Nummer).

### 5.59 C-c u A/R auf jeder Ebene · Lese-Handout C-c u H (done, 2026-10-09)

Carsten (09.10.): für die Reading-Klassen ein Druckblatt neben dem
Klassentext (er arbeitet parallel analog) — Wylie, Übersetzung,
Vokabular („ohne Claude in der Überschrift; gibt es eine
Vokabelliste, diese Übersetzungen bevorzugt"), Grammatik.  Nach
einem Probedruck §220 entschied er: § UND Satz; satzweise;
★-Bedeutung zuerst, Kontextglosse klein dahinter nur wenn abweichend;
Wiederholungen ok; Grammatik nur „Verb backbone" + „Notable
constructions"; Uchen-Zeile; sa analog; generierte .org → PDF.
Dazu: „C-c u A sollte auf jeder Hierarchieebene funktionieren und
kaskadierend analysieren."  Plan `~/.claude/plans/idempotent-
pondering-wozniak.md`; alle Commits RED-first (`de29096`…`0b901cd`).

**Bug A1/A2 (zuerst):** in einer KASKADEN-Quelle lief C-c u A auf
`** Section §220' in den Two-File-Absatzpfad (`tibetan-org-at-
paragraph-p' matcht jedes „§" → „has no `*** Tibetisch' child"),
auf Sections ohne „§" (sa) und `* Tibetan Text' in den Segment-Impl
(„Not in a segment or paragraph"); C-c u R ebenso.  Jetzt: Zweig
NACH Segment/Satz, VOR Absatz (textuelle Layout-Probe + hartes
require, B0-Lektion) → `tibetan-cascade-analyze-subtree' (org-
Subtree an Punkt = Umfang; fehlende Dateien anlegen, Fire nur wo das
geteilte Gate `--sentence-needs-fire-p' offen ist; über
`tibetan-cascade-subtree-confirm-threshold' (10) y-or-n-p mit Anzahl;
danach öffnet der erste Satz, nach Ablehnung ohne Open-Fire) bzw.
`tibetan-cascade-reanalyze-subtree' (Satz-Semantik von C-c u R:
existierende Dateien preserve-rendern, kein Fire; C-u = erzwungenes
Neu-Feuern, IMMER bestätigt).  A0 legte `--fire-plist' (A3-Vertrag
:children an EINER Stelle) und das Gate als Helfer frei.

**Bugfix unterwegs (f760ddc):** `tibetan-parse-wordlist-org' —
die Zellen-Regex lief über das Zeilenende, las die org-Trennlinie
`|---+---|' mit dem Pipe der Folgezeile als Eintrag und VERSCHLUCKTE
die erste Datenzeile jeder org-Tabellen-Wortliste.  In
Wortliste-rgyan.org ist das `bden pa gnyis' — der in §5.58 als
Datenfrage notierte Fall.  Bestehende Dateien zeigen den ★ erst nach
dem nächsten Regenerate.

**Handout (persist/tibetan-handout.el, drei Schichten):**
- Daten `--sentence-data': Segmente aus der Quelle (Wylie + Uchen;
  sa: IAST); Übersetzung = ⟦N⟧-Zeilen wenn alle gelandet (nummeriert),
  sonst Translation-Slot ohne „(Sentence …)"-Label, Platzhalter/
  Stub = Lücke; Vokabular je Segment (Splitter
  `tibetan-analysis--split-body-by-segment', B0) mit ★ über
  `tibetan-vocab--curated-exact-entry': exakt → Partikel-SILBE am Ende
  (Liste aus `tibetan-extract-vocab--particle-tails', ra/sa/'i
  ausgenommen) → Klitikum NUR wenn Claudes Wortart es deckt
  (`tibetan-handout--clitic-pos-rules': blos ergative → blo, rgyas
  Verb ≠ rgya); sa NIE ★ (Poison, per Negativ-Probe tragend), Info
  „Lemma; MORPH" (Word-Analysis-Parser liefert jetzt `:lemma');
  Grammatik nur `tibetan-handout-grammar-labels', Fallback alle
  Bullets (nie leer).  Wortliste aus dem Resources-Ordner der QUELLE
  unter `let' (kein globales Leck).  Lopez/W&M/DM/Working
  Translation nie (Poison-Lock-Test).
- Renderer `tibetan-handout-render' (rein): GENERATED-Kopf,
  LuaLaTeX, rechts 6 cm Notizrand, Libertinus + Noto Serif Tibetan
  (HarfBuzz; `tibetan-handout-tibetan-font'), `* Text' (Uchen über
  Wylie, Rand-Nummern, Export-Block, eigene Seiten), je Satz
  Übersetzung/Vokabular (longtable je Segment)/Grammatik
  (Beschreibungsliste); Markdown → org nur wo org die Hervorhebung
  erkennt, sonst Snippet; nirgends „Claude".
- Kommando `tibetan-handout' (C-c u H): Umfang wie A1 (+ sent-Buffer);
  `handouts/<kurz>-par-NNN.org' bzw. `-sent-N[-M].org' neben der
  Quelle, Überschreibschutz per Marker; PDF mit LOKAL gebundenem
  `org-latex-pdf-process' (2× lualatex); C-u nur .org; IN einer
  Handout-.org exportiert C-c u H die bearbeitete Fassung (Kürzungen
  gehen nie durch Neu-Generieren verloren).

Korpus (buddhist-studies `2c79883`): `Translations/handouts/
rgyan-par-220.{org,pdf}' (5 S.; Satz 656 noch Lücken bis C-c u A
auf §220) und `Madhyamakavatara-sa/handouts/mav-sent-001.{org,pdf}'.

**Offen / Ausbau:** Uchen-Zeilen für sa (kein IAST→Devanagari);
Partikel-Funktion je Vokabel ins Handout (nur auf Zuruf);
`wordlist.txt' (Tibetisch-IV-Kopie) trägt PDF-Umlautverluste
(„ku nstlich") — Carsten: Vorsemester, kein Scope.  Testlauf-Hinweis:
`make test' lädt veraltete .elc — vor dem Test `make compile' (oder
`load-prefer-newer'), sonst testet man alten Code.

Suite 2440 → **2470 ERT** (0 unexpected, 1 Skip; der echte
LuaLaTeX-Smoke-Test läuft nur mit lualatex + Font); BDD 251;
compile clean durchgehend; REFERENCE.org je def-Commit.

**Nachtrag 12.10. (Tantrāloka-Reading WS26/27):** Carstens Lesedatei
`WS26-27/Tantraloka/tantraloka-1.org' (Verse `*** TĀ 1.N' mit
#+begin_verse) war kein CAT-Dokument → C-c u A meldete nur „Not in a
segment or paragraph" („passiert nichts").  Tool: die Meldung erklärt
jetzt Gliederung + C-c o k (`b83d075', user-error); der sa-Tokenizer
überspringt GRETIL-Versnummern `// 1' (`83d12d4').  Daten (Korpus
`5ed3a0f'): Datei in place auf die Kaskade umgestellt — Vers =
`*** Sentence N' (N = Versnummer, :VERSE: TĀ 1.N), Halbvers-Zeile =
`**** Segment M' (1–668), Dyczkowski-Abschnitte `** Section …',
#+SOURCE_LANG: sa; Übersetzung/Notizen in analysis/sent-NNN-tantralo.
org.  Suite **ERT 2472**.

### 5.58 READING-CLASS-STRUKTUR der Kaskaden-Satzdateien (done, 2026-10-09)

Carstens Struktur-Skizze (09.10.) für die 4 Reading-Klassen WS26
(bo / sa / 2× beides; beim Drankommen zählt der schnelle Blick).
Entscheidungen (AskUserQuestion): Interlinear ENTFÄLLT; Lopez/W&M
EINMALIG materialisieren (editierbar); die 56 in Korpus-Commit
`8627b91` verlorenen Abschnitts-Übersetzungen aus `3d5ebf0`
wiederherstellen; ALLE Kaskaden-Korpora migrieren.  Plan
`~/.claude/plans/idempotent-pondering-wozniak.md`; alle Commits
RED-first (`87f47f9`…`b5890dd`).

**Bugfixes vorneweg (B1–B6b, je ein Commit):** B1 keep-l2 hing an
`(fboundp 'tibetan-sentence--read-l2-body)` — stiller Totalverlust
aller L2-Bodies im modullosen Batch (B0-Klasse; jetzt hartes
require).  B2 sa-Dateien verloren `*** Claude Grammar` bei JEDER
Landung (L3-Restore ohne Anlege-Fallback; sent-001-mav.org: null
Grammar-Headings).  B3 GATE-FALLE: nach Chunk-Landung (nur
Translation) war needs-request-p (Translation UND Vocab leer)
dauerhaft zu — Vocabulary/Grammar/Particles/Concept Notes kamen NIE
mehr an (der Zustand von ~736/737 Rgyan-Dateien); jetzt per-Slot-
Gates (`--claude-sections-incomplete-p` + Slot-Landung, M7 bleibt:
Gelandetes wird ohne force nie überschrieben).  B4 TRUNCATION:
sent-654s Antwort riss mitten im Particles-Tupel ab, Concept Notes
blieb stummer Platzhalter; jetzt `--response-truncated-p`
(fehlender ## Concept-Notes-Pflichtschluss) + persistenter Stub
"[Antwort abgeschnitten …]" (ÜBERLEBT den Regenerate-after-Land —
darum KEIN [Awaiting-Präfix; als leer zählt er via
`--truncation-stub-p`, damit er Gate offen hält und seine eigene
Ersetzung nicht blockiert); kein Auto-Refire (Schleifengefahr).
B5 Homographen im Reading-Tokenizer kontextsensitiv: standalone
དེ ohne Vorgänger-Verb = Demonstrativum (kein CONV:ste mehr),
standalone པས/བས ohne Verb = NMLZ.ERG (§5.39; `snang de'
postverbal bleibt CONV:ste, test-gepinnt).  B6b
`tibetan-steinert-url` downcaste (zerstörte Wylie-Caps = Sanskrit)
+ Docstring log über Klitika-Strip; B6a (DB-Entkopplung der
Reading-Links) bewusst entfallen — die Fläche starb mit S2, die
Entkopplung lebt in T2.

**Struktur (bo UND sa):** `* Translation` (L1) · `* Reading` →
nur `** Gloss Tables` (je Einheit `*** Segment N` + Tabelle +
`- ⟦N⟧ …` DARUNTER; T4) · `* Tibetan Analysis` flach (Sentence
Structure / Claude Vocabulary / Claude Grammar L2 / Buddhist Terms
L2 / Partikelkarte L2 (die Bialek-Karte) / Claude Particles L2 /
Concept Notes / [Word Analysis sa]) · `* Provided Translations`
(L1: DM + Lopez/W&M) · Working Translation · My Notes · Footnotes.
`* Tibetan Text` und `** Interlinear`/`** Renderings` ENTFALLEN.

**Mechanik:** T4 Hash-Kanonik (`--gloss-tables-canonical` strippt
die ⟦N⟧-Maschinenzeilen beidseitig — Landung ≠ Edit, Zellen-Edit
schützt weiter; Alt-Hashes bleiben gültig); Renderings-Region =
ganze * Reading-Sektion (EIN Leser/Writer-Pfad alt+neu;
Insert-if-missing hinter dem Segment-Heading); S1 Dual-Reads im
geteilten Sections-Leser (Translation L1→L2→Legacy, Particles
L2→L3) — Leser VOR Schreiber, sonst Doppel-Fire-Klasse; S2
Regenerate = slot-bewusste MIGRATION (PT-Body komponiert: DM-Slot
+ PT-Rest ohne DM-/Particles-Duplikate; Anlege-Anker Ende
* Tibetan Analysis statt vor * Footnotes); Landung: cascade-v2-
Zweig in effective-section-order/ensure-claude-headings (textuelle
Probe `^* Translation$`; Alt-Dateien landen im Segment-Zweig und
migrieren beim Regenerate-after-Land — self-migrating);
Prompt-Grounding aus den Token-Strömen
(`--generated-reading-lines` über shad-gesplittete Units, gleiche
Render-Var-Bindungen wie der Scaffold, Paritätstest §5.53);
§-Ansicht liest Tibetisch aus den Quell-Segmenten.  T1
org-Linktyp `steinert:` (:follow baut URL zur Klickzeit, :export
echte URLs; Registrierung bei ol-Load).  T2 Tabellen-Zeile 1
verlinkt Wörter+Verben (`[[steinert:STAMM][pa'i]]`), Breiten nach
SICHTBAREM Text (`--visible-width`); sa linkfrei; Two-File-Tabellen
erben beim nächsten expliziten Regenerate.  T3 Converb-Funktion in
Zeile 3 (`CONV:nas (seq.)`): textkeyed Dynamik
`tibetan-gloss-table--claude-particles` (Regenerate bindet aus dem
preservierten Body; segment-gruppierter Parser
`--parse-claude-particles-by-segment`; Vorkommens-Cursor je
Partikel+Einheit, NIE first-match), Fallback = EIN Mapping der
Clause-Segmenter-Klassen.  V1 Struktur-Übersicht ohne
—-Strich-Zeilen (Zählzeile "n Einheit(en) ohne erkanntes
Hill-Verb").  P1/P2 `tibetan-cascade-materialize-provided-
translations` (einmalig, editierbar, keep-geschützt; Tibetisch/
Wylie nie; §5.54-Poison-Locks um die neue L1-Form erweitert).  M1
`tibetan-cascade-migrate-structure-v2` (render-only — WICHTIG:
nach B3 gelten fast alle Rgyan-Dateien als incomplete, ein
feuernder Batch wäre ~736 API-Calls; der Wrapper feuert NIE).

**Ausbauliste / Folgearbeiten:** V2 Phrasengrenzen der
Struktur-Übersicht (`blos · ba dang rnam gzhag…`, Split mitten in
mthong ba dang — steckt in tibetan-sentence-tree.el, harte
Scope-Grenze); Partikelkarte-Platzierung = mein Vorschlag (eigenes
L2, deterministisch regeneriert) — Carstens Veto trivial
umsetzbar; Nach-Fire der fehlenden Sektionen nur auf Zuruf
(API-Spend), sequenzieller Treiber.

Suite 2406 → **2440 ERT** (0 unexpected, 1 Skip); BDD 251;
compile clean durchgehend; REFERENCE.org je def-Commit.

### 5.57 SANSKRIT im Kaskaden-Layout (done, 2026-09-24)

Die Masterarbeit braucht Sanskrit-Belegstellen (Candrakīrti: PP ad
MMK 24.8/24.10, MAv VI.28 — Prosa UND Vers) im Drei-Sichten-
Workflow.  Carstens Entscheidungen (AskUserQuestion): Kaskade als
EINE Arbeitsfläche; Glossen zunächst Claude+DM (Monier-Williams =
Ausbau); Eingabe gemischt IAST/Devanagari → intern IAST.  Plan
`~/.claude/plans/idempotent-pondering-wozniak.md`; alle Commits
RED-first (`4f112ca`…).

**Kernproblem/-lösung**: Tibetische Tokens existieren VOR Claude
(tsheg); Sanskrit-Wörter entstehen erst durch die Sandhi-Auflösung
— also aus Claudes Antwort.  Dreistufig: (1) Scaffold rendert
degradiertes Reading (Oberflächen-IAST, Labels `?`); (2) die
Satz-Antwort trägt je Segment einen **Padapāṭha** + Morphologie in
der neuen, preservierten Sektion `** Word Analysis` (Kontrakt:
Zeile 1 = sandhi-aufgelöste Wortfolge OHNE Label; Bullets
`- wort — lemma; MORPH` mit kompakten Labels N.GEN.PL/V.PRS.3SG/
IND) und das Vokabular im unveränderten Komma-Format (Schlüssel =
Padapāṭha-Schreibung); (3) **Regenerate-after-Land** (pur,
fire-frei — NIE über reanalyze-file: dessen Fire-Logik + Stubs =
Schleife; Reihenfolge Sektionen→Regen→Spans, sonst degradiert der
sichtbare Missing-Stub) materialisiert Tabellen/Interlinear.  Der
Re-Render läuft für ALLE Kaskaden-Dateien — schließt zugleich die
bo-Lücke (gelandetes Vokabular erreichte das Reading erst beim
manuellen Regenerate; der Auto-Regen-Router kennt nur seg-/par-).

**Bugfixes vorneweg (Teil A)**: A1 `C-c s R`/`C-c s A` umgingen
die Kaskaden-Weiche (B0-Klasse — Two-File-Regenerate hätte
* Reading zerstört; textuelle Probe + Refusal); A2 tote
fboundp-Guard (nicht existentes Symbol) ließ Parallel-Sanskrit-
Docs in den Sentence-First-Pfad; A3 Kaskaden-Re-Fire ohne
`:children` → leere Prompt-Enumeration; A4 DM-Sanskrit ohne
target-lang.  Dry-Run-Nachzügler: C6a Vocab-Parser strippt
Bullet-Zeilen (Modell-Drift traf den Glossen-Tier), C6b
Padapāṭha-Label-Strip + Morph-Kompaktierung (Parser UND Prompt).

**Bausteine**: `#+SOURCE_LANG: sa` (+ Resolver
`--resolve-source-lang`, Dynamik `tibetan-analysis--source-lang` —
Bindung NUR aus Quell-Metadaten, §5.53-Parität; NICHT SOURCE_MODE,
der gehört dem Two-File-Parallelmodus) · NEU
core/tibetan-sanskrit-script.el (Devanagari→IAST, pur,
idempotent; Daṇḍas bleiben als Split-Anker) · NEU
analysis/tibetan-sanskrit-reading.el (Token-Provider: Oberfläche/
Padapāṭha, textkeyed Dynamik `--word-analysis`; NIE Wylie-Lexika —
na/ca/ma/sa träfen tibetische Einträge, Poison-Locks) ·
reading/gloss-table-Dispatch (HARTES require, kein
fboundp-Fallback; `:morph` = neuer Tier 0 in Zeile 3) ·
sa-System-Base `tibetan-sentence-claude--system-prompt-sanskrit`
(vierter Cache-Präfix; kein Wylie, kein Zettel-/Dict-Grounding) +
Chunk-Pendant · DM `--input-encoding` (sa→"iast"; bo explizit
"auto" = Default, Bodies byte-identisch) · Importer
`tibetan-cascade-import-sanskrit` (`C-c o k`, Menü „Translation"):
`# LABEL`-Zeilen = Sections (`:LOPEZ_SECTION:` = Sequenz-Int, das
Label trägt die echte Referenz), Vers = Zeile-je-Pāda (Block =
Strophe = Satz), Prosa = Daṇḍa-Unit = EIGENER Satz (Singletons
feuern seit B-1.2); Konkatenations-Kontrakt; Überschreib-Refusal.
Stitcher/§-Ansicht laufen unverändert (Poison-Lock mitgetestet).

**Empfehlung Betrieb**: EIN Belegstellen-Dokument je Vorhaben,
EIGENER Ordner je Quelle (Stitcher-Ausgabenamen pro analysis/ fix;
make-short-name kollabiert `MMK-*` auf denselben Suffix).
Referenz-ÜBERSETZUNGEN (Siderits/Katsura, MacDonald …) nur in
`** Provided Translations` — der §5.54-Poison-Lock hält sie aus
den generierten Dokumenten.

**Dry-Run (echte API, MMK 24.18)**: Import→create-all→Fire→
Landung end-to-end; deutsche Übersetzung, Word Analysis, Tabellen
mit Morph-Zeile, DM iast/german — die vier Kontrakt-Abweichungen
des ersten Calls sind als C6a/C6b verriegelt.  BDD-Suite
sanskrit-cascade-spec (gecannte Antwort, voller Durchlauf).

**NICHT im Scope (Ausbaustufen)**: Sanskrit-Tibetisch-Parallel IN
der Kaskade (bleibt Two-File); Monier-Williams-Tier;
IAST→Devanagari; Umbenennung `* Tibetan Analysis` (kosmetisch —
Writer keyen auf den Literal); Two-File-Sanskrit-Writer-Härtung
über A2/A4 hinaus (§5.30-H3); IAST-Diakritika im Headword-Bolder;
Wizard-Route des Importers.

Suite 2346 → **2401 ERT** (0 unexpected, 1 skip); BDD 250 →
**251**; compile clean durchgehend; REFERENCE.org je def-Commit.

## 6. Open work (prioritised)

### P0 — Verify Detailed Dictionary on a real segment ✓ DONE 2026-04-15
Cap tuned 8 → 5 after seg-4 review.  Knob is the
`tibetan-vocab--steinert-cap' defconst at `core/tibetan-vocabulary-detailed.el'.

### P1 — Fix `པ` / `པའི` being tagged as verbs ✓ DONE 2026-04-15
Fixed in `analysis/tibetan-enhanced-display.el' via the
`tibetan-verb-detect--nominalizer-set' guard.  Tests in
`test/tibetan-round1-verb-extraction-test.el'.

### P2 — Wire Round-2 into analysis file output ✓ DONE 2026-04-15
Section is gated by `tibetan-analysis-show-clause-structure'
(default `t').  Renderer + 5 tests in `tibetan-analysis-persist-test.el',
plus 9 tests for the chunker fixes in `tibetan-round2-clause-segmenter-test.el'.

### P3 — Documentation phase (explicitly deferred)
Only start when Carsten says so. When that time comes:
- Update `README.md`, `FEATURES.md`, `ARCHITECTURE.md`,
  `VERSE-ANALYSIS-GUIDE.md` to reflect the batch-reanalyze command,
  multi-source Detailed Dictionary, and Round-2 clause analysis.
- Also `CHANGELOG.md` — follow the existing format.

### P4 — Workshop presentation (explicitly deferred)
A `.pptx` / walkthrough for the workshop. Don't start without
explicit instruction.

### P5 — Root-cause the `gyur pa'o' parser crash ✓ DONE 2026-04-30
Bisect via instrumented checkpoints found the signal site: the
Section 2 vocab-pairs loop's first `short-meaning' binding.

Root cause: `tibetan-vocab-multisource-entries' returned a
detailed-entry whose `:primary' field was an empty string `""'.
`raw-meaning' became `""' (truthy in elisp), `(when raw-meaning
…)' entered the body, `(car (split-string "" ";" t))' returned
nil, and `(string-trim nil)' raised `Wrong type argument:
stringp, nil'.

The outer condition-case caught it and emitted the
`[Analysis error — partial file only]' fallback for these
segments (note: the original CLAUDE.md report cited
`(wrong-type-argument listp "གྱུར")' from `cl-remove' — likely
a different earlier symptom; the current crash is the
stringp-on-nil one above).

Fix: empty-string guard at the top of the FIRST short-meaning
let — short-circuit empty raw-meaning to nil so the binding
becomes nil and the downstream chain follows the same path as
a missing dictionary entry.  Patch in
`persist/tibetan-analysis-persist.el` Section 2 vocab-loop.

Tests: `tibetan-analysis-generate-content-gyur-pao-no-fallback'
asserts (a) full Section 1..8 layout is generated, (b)
fallback template's `[Analysis error]' marker is absent, (c)
all 8 user-facing section headings are present.

Suite total at fix: 1791 → 1792, 0 unexpected, 1 skipped.

### P6 — Preserve `:FOLIO:' drawer on analysis-file `* Tibetan Text' (low priority, 2026-04-21)
The drawer-aware extractor (§5.8.2) correctly strips the
`:PROPERTIES: :FOLIO: Dxxxx :END:' drawer from the source
segment's body before feeding the analyser — but a side-effect is
that newly-created analysis files no longer carry `:FOLIO:' on
their own `* Tibetan Text' heading.  Fix: have
`tibetan-analysis-create-file' accept an optional folio argument
and write the drawer explicitly, AND have
`tibetan-org-get-segment-text' (or a companion helper) return the
folio alongside the text so the caller can thread it through.

## 7. Pitfalls / things that have bitten us

- **Trailing tsheg after case-stripping**: `"བདག་གིས"` minus `"གིས"`
  leaves `"བདག་"`, not `"བདག"`. The fix in `tibetan-clause-seg--case-strip`
  does a final `(replace-regexp-in-string "་+$" "" stem)`. If you write
  similar code elsewhere, apply the same trim.
- **`cl-defun` vs `defun`**: if a helper uses `&key`, it must be
  `cl-defun`. We wasted a test cycle on this once.
- **Test discovery silently fails**: new test files do NOT run unless
  added to `test/run-all-tests.el`. The overall count will look fine.
  Always sanity-check the total test count moved up after adding tests.
- **Rangjung Yeshe lazy load**: the 162k-entry RY dictionary is
  lazy-loaded. Tests that touch it should stub
  `tibetan-lookup-word-in-rangjung-yeshe` so they don't depend on
  load state.
- **Steinert DB may be absent**: tests that need it should check
  `(tibetan-steinert-available-p)` and `skip-unless`. Several already
  do this.
- **Worktrees have no steinert.db**: the DB is gitignored (built via
  `make build-steinert`), so a `git worktree` checkout silently runs
  with the Steinert source DISABLED — sense selection changes and a
  bisect there chases ghosts (§5.53).  Symlink the main checkout's
  `data/dictionaries/steinert.db` into any worktree before comparing
  dictionary-dependent output.
- **Ambient init functions**: repo code must never let an
  `fboundp` probe on a function defined in Carsten's init (e.g.
  `lookup-combined-dictionary-first`) decide which data source
  answers — that makes interactive and batch runs diverge (§5.53;
  same lesson class as the B0 fboundp guard, §5.52).
- **Placeholder translations**: `[Requesting translation...]`,
  `[Claude unavailable`, `[Translation not available` — these are NOT
  real translations. Any "preserve existing Claude translation" logic
  must skip these placeholder patterns.

## 8. Conventions for Claude Code in this repo

- Use the ERT loader, not a custom test harness.
- Stub external deps in tests (SQLite, HTTP, file system outside
  tempdirs) — tests must run in batch without side effects.
- When renaming or moving a function, keep the old name as an alias
  or update every caller and test — Carsten runs into the difference
  immediately.
- Byte-compile cleanly: `make compile` should not introduce new
  warnings.
- Keep commit messages factual: what changed, which tests were added,
  what behaviour users will see.
- Don't commit without Carsten asking.

## 9. First thing to do in a new session

1. `make test` (or the batch command in §4). Confirm baseline green
   (expect 1787 / 1786 expected / 0 unexpected / 1 skipped at 2026-04-30).
2. Skim `MEMORY.md` (auto-memory) — `working_discipline.md` is the
   baseline rule set; this file refines it for tibetan-cat.el.
3. Skim `git log --oneline -20` for anything newer than §5.13 (this
   file may be stale; the log is authoritative).
4. Ask Carsten which P-level from §6 he wants to tackle — don't guess.
5. Write tests before code. Run tests after each edit.
6. Report back with: what changed, which files, which tests now
   cover it, and the updated test count.

### Active workflow: preparing a new document for German translation

Carsten's current priority is preparing new reading sources for
translation into German.  The end-to-end pipeline for a fresh
source file:

1. Create the source file in the §2.12 reading-file layout
   (Section → Sentence → Segment, with `**** Working Translation`
   siblings).
2. `C-c u z i` — initialise thesaurus from Kramer glossary (one-
   time; skip if already done).
3. `C-c u z L de` — set this source's target language to German.
   Writes `#+TIBETAN_TARGET_LANG: de` into the source header.
4. `C-c u B` — auto-analyse the whole document, creating all
   `seg-NNN*.org` analysis files with the German-side gloss in
   Interlinear + CAT Gloss + Claude Translation; bilingual DE // EN
   stays in Detailed Dictionary.
5. `C-c u z g` — run the translation-gap report.  It lists every
   segment whose thesaurus zettel has `[to be researched]` or an
   empty DE side.  Walk the list with `C-c u z e` (edit-at-point)
   to fill entries.
6. `C-c u z R <zettel>` — after editing a zettel, re-run just the
   affected segments instead of a full `C-c u r` batch.

This workflow is the reason §5.12 exists; when in doubt treat it
as the driving use case for that subsystem.

---

*If this file gets out of date, update it as part of the same change
that made it stale. A wrong handoff doc is worse than no doc.*

## 10. REFERENCE.org maintenance (added 2026-05-24)

This repo ships a `REFERENCE.org` at the project root listing every
top-level Emacs Lisp definition (1100+ forms across 64 modules) with
docstring, signature, interactive flag, and autoload marker.
Auto-generated by `~/emacs-zettelkasten/generate-function-reference.py`.

**Hard requirement:** when you add, remove, or rename a `defun` /
`cl-defun` / `defcustom` / `defvar` / `defconst` in any `.el` file
under this repo, re-run the generator BEFORE the next commit:

```bash
python3 ~/emacs-zettelkasten/generate-function-reference.py \
    ~/tibetan-cat.el \
    ~/tibetan-cat.el/REFERENCE.org
```

Commit `REFERENCE.org` in the SAME commit as the `.el` change (or as
the very next commit).  The reference should never lag the source
by more than one commit.

Skip when:
- The change is in `test/` or `data/`.
- The change is a docstring tweak only (the reference picks it up
  on the next regular re-run anyway).
- The change doesn't touch any `def…` form.

A future enhancement is to add a `make reference` target and a
pre-commit hook so this becomes automatic.  Until then it's manual
discipline plus this section.
