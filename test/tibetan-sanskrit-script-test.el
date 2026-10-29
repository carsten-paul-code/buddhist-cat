;;; tibetan-sanskrit-script-test.el --- Tests for Devanagari→IAST -*- lexical-binding: t -*-

;;; Commentary:
;; Sanskrit-Kaskade B2 (2026-09-24).  Pure tests — strings only.

;;; Code:

(require 'ert)

(let ((base-dir (file-name-directory (or load-file-name buffer-file-name))))
  (add-to-list 'load-path (expand-file-name "../core" base-dir)))

(require 'tibetan-sanskrit-script)

(ert-deftest tibetan-sanskrit-script-consonant-virama ()
  "Consonant + virāma suppresses the inherent a; conjuncts chain."
  ;; धर्म = dha r(virāma) ma → dharma
  (should (equal "dharma"
                 (tibetan-sanskrit-script-devanagari-to-iast "धर्म")))
  ;; सत्त्व = sa t(v) t(v) va → sattva
  (should (equal "sattva"
                 (tibetan-sanskrit-script-devanagari-to-iast "सत्त्व")))
  ;; Word-final virāma: तत् → tat
  (should (equal "tat"
                 (tibetan-sanskrit-script-devanagari-to-iast "तत्"))))

(ert-deftest tibetan-sanskrit-script-inherent-a ()
  "A bare consonant carries the inherent a."
  (should (equal "na ca"
                 (tibetan-sanskrit-script-devanagari-to-iast "न च")))
  (should (equal "bhagavat"
                 (tibetan-sanskrit-script-devanagari-to-iast "भगवत्"))))

(ert-deftest tibetan-sanskrit-script-vowel-signs ()
  "Dependent vowel signs replace the inherent a; independent vowels
stand alone."
  ;; बुद्ध → buddha; धी → dhī; गुरु → guru
  (should (equal "buddha"
                 (tibetan-sanskrit-script-devanagari-to-iast "बुद्ध")))
  (should (equal "dhī"
                 (tibetan-sanskrit-script-devanagari-to-iast "धी")))
  (should (equal "guru"
                 (tibetan-sanskrit-script-devanagari-to-iast "गुरु")))
  ;; Vocalic ṛ as sign and independent: कृत → kṛta; ऋषि → ṛṣi
  (should (equal "kṛta"
                 (tibetan-sanskrit-script-devanagari-to-iast "कृत")))
  (should (equal "ṛṣi"
                 (tibetan-sanskrit-script-devanagari-to-iast "ऋषि")))
  ;; Diphthongs: गौतम → gautama; एक → eka
  (should (equal "gautama"
                 (tibetan-sanskrit-script-devanagari-to-iast "गौतम")))
  (should (equal "eka"
                 (tibetan-sanskrit-script-devanagari-to-iast "एक"))))

(ert-deftest tibetan-sanskrit-script-anusvara-visarga ()
  "Anusvāra → ṃ, visarga → ḥ, candrabindu → m̐."
  ;; संस्कृत → saṃskṛta
  (should (equal "saṃskṛta"
                 (tibetan-sanskrit-script-devanagari-to-iast "संस्कृत")))
  ;; दुःख → duḥkha
  (should (equal "duḥkha"
                 (tibetan-sanskrit-script-devanagari-to-iast "दुःख")))
  ;; नमः → namaḥ
  (should (equal "namaḥ"
                 (tibetan-sanskrit-script-devanagari-to-iast "नमः"))))

(ert-deftest tibetan-sanskrit-script-avagraha-danda ()
  "Avagraha → apostrophe; daṇḍas pass through (the importer splits
on them — script-neutral punctuation)."
  ;; सो ऽपि → so 'pi
  (should (equal "so 'pi"
                 (tibetan-sanskrit-script-devanagari-to-iast "सो ऽपि")))
  (should (equal "gataḥ। punaḥ॥"
                 (tibetan-sanskrit-script-devanagari-to-iast
                  "गतः। पुनः॥"))))

(ert-deftest tibetan-sanskrit-script-digits ()
  "Devanagari digits map to ASCII."
  (should (equal "24.18"
                 (tibetan-sanskrit-script-devanagari-to-iast "२४.१८"))))

(ert-deftest tibetan-sanskrit-script-normalize-idempotent-on-iast ()
  "Pure IAST passes through byte-identically (idempotence — the
importer may be run on already-normalized text)."
  (let ((iast "svabhāvo hi prakṛtir akṛtrimā | dharmāṇāṃ śūnyatā ||"))
    (should (equal iast (tibetan-sanskrit-script-normalize iast)))
    (should (equal (tibetan-sanskrit-script-normalize iast)
                   (tibetan-sanskrit-script-normalize
                    (tibetan-sanskrit-script-normalize iast))))))

(ert-deftest tibetan-sanskrit-script-normalize-mixed-input ()
  "Mixed Devanagari/IAST input normalizes to one IAST form; line
structure (pāda breaks) survives untouched."
  (should (equal "na svato nāpi parato\ndharmāṇāṃ śūnyatā"
                 (tibetan-sanskrit-script-normalize
                  "न स्वतो नापि परतो\ndharmāṇāṃ śūnyatā")))
  ;; Same line in both scripts → same normalization.
  (should (equal (tibetan-sanskrit-script-normalize "धर्माणां शून्यता")
                 (tibetan-sanskrit-script-normalize
                  "dharmāṇāṃ śūnyatā"))))

(ert-deftest tibetan-sanskrit-script-nil-safe ()
  "nil and empty input degrade quietly."
  (should-not (tibetan-sanskrit-script-devanagari-to-iast nil))
  (should-not (tibetan-sanskrit-script-normalize nil))
  (should (equal "" (tibetan-sanskrit-script-devanagari-to-iast "")))
  (should (equal "" (tibetan-sanskrit-script-normalize ""))))

;; ----------------------------------------------------------------------------
;; IAST → Devanāgarī (§5.74, Carsten 29.10.: from Roman script to Uchen
;; and Devanāgarī — the sentence file shows the Devanāgarī above the IAST)
;; ----------------------------------------------------------------------------

(ert-deftest tibetan-sanskrit-script-iast-to-devanagari-verse ()
  "TĀ sentence 1007 (Jayaratha, introductory verse 7): conjuncts by
virāma, final consonants with virāma, anusvāra, the daṇḍas / and // as
। and ॥, the verse number in Devanāgarī digits."
  (should (equal "अत्र मद्वागशक्तापि यन्नियन्त्रणमुल्लसेत् ।"
                 (tibetan-sanskrit-script-iast-to-devanagari
                  "atra madvāgaśaktāpi yanniyantraṇamullaset /")))
  (should (equal "तत्पारमेश्वरं श्रीमन्महानन्दविजृम्भितम् ॥ ७"
                 (tibetan-sanskrit-script-iast-to-devanagari
                  "tatpārameśvaraṃ śrīmanmahānandavijṛmbhitam // 7"))))

(ert-deftest tibetan-sanskrit-script-iast-to-devanagari-letters ()
  "Aspirates and the two-letter vowels before their first letter
\(kh, ai, au), initial vowels, ṛ, visarga, avagraha, candrabindu, ṁ as
anusvāra, capitals; anything else passes through."
  (dolist (c '(("saṃdigdhāḥ ke'pi" . "संदिग्धाः केऽपि")
               ("aiśvaryam auṣadham" . "ऐश्वर्यम् औषधम्")
               ("kṛṣṇaḥ ṛṣiḥ" . "कृष्णः ऋषिः")
               ("khaḍga bhīṣma" . "खड्ग भीष्म")
               ("haṁsa" . "हंस")
               ("Śiva" . "शिव")
               ("ūrdhvam īśvara" . "ऊर्ध्वम् ईश्वर")
               ("a, b?" . "अ, ब्?")))
    (should (equal (cdr c) (tibetan-sanskrit-script-iast-to-devanagari (car c)))))
  (should-not (tibetan-sanskrit-script-iast-to-devanagari nil)))

(ert-deftest tibetan-sanskrit-script-round-trip ()
  "IAST → Devanāgarī → IAST gives the text back (the daṇḍas come back
as ।/॥, which the importer keeps)."
  (dolist (s '("atra madvāgaśaktāpi yanniyantraṇamullaset"
               "vimalakalāśrayābhinavasṛṣṭimahā jananī"
               "saṃdigdhāḥ ke'pi kiṃ brūyāṃ śrotāro yadanāgatāḥ"))
    (should (equal s (tibetan-sanskrit-script-devanagari-to-iast
                      (tibetan-sanskrit-script-iast-to-devanagari s))))))

(provide 'tibetan-sanskrit-script-test)

;;; tibetan-sanskrit-script-test.el ends here
