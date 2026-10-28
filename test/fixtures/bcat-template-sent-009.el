;;; bcat-template-sent-009.el --- Daten von bDen Satz 9 für den Vorlagentest -*- lexical-binding: t -*-
;; Einmalig aus Templates/analysis/samples/Tibetan.org gezogen (28.10.2026).
;; Gerüst tibetan.org + diese Daten muss die Musterdatei ergeben.
(defconst bcat-template-test--sent-009
  `((sent-num . "9")
    (source-link . "[[file:../bden-gnyis-cat.org::*Sentence 9][bden-gnyis-cat.org / Sentence 9]]")
    (segment-list . "38, 39, 40, 41")
    (hash . "0fb9a1413fd15e6cec234b8abb1e02b9")
    (created . "2026-10-14")
    (last-analyzed . "2026-10-28")
    (segments
     ((num . "38")
      (wylie . "brjod du med pa bltar med pa/ /")
      (gloss-table . "| [[steinert:brjod du med pa][brjod du med pa]] | [[steinert:bltar med pa][bltar med pa]]    |
| indescribable   | invisible       |
| ADJ · predicate | ADJ · predicate |")
      (translations . "⟦38⟧ Indescribable (brjod du med pa: avācya) and invisible (bltar med pa: adraṣṭavya), \\\\
*Lindtner 1981:* [It is also] inexpressible (avācya) and invisible (adraṣṭavya), \\\\
*DharmaMitra:* Inexpressible and invisible,")
      (structure . "no finite verb — 38–39 are a nominal predicate chain, the object of /rtogs/ in 40.")
      (note . "/brjod du med pa/ = verb stem + terminative /du/ + /med pa/, the negated potential: \"there is no expressing [it]\"; likewise /blta-r med pa/ \"cannot be seen\".")
      (concepts . "*brjod du med pa / bltar med pa* (Skt. avācya / adraṣṭavya) — standard apophatic epithets of the ultimate truth (paramārtha-satya) in Madhyamaka: beyond conventional expression (brjod pa) and beyond ordinary perception (blta ba), since it transcends the subject–object duality of saṃvṛti cognition; recurrent e.g. in the Laṅkāvatārasūtra."))
     ((num . "39")
      (wylie . "'gyur ba med pa 'dus ma byas/ /")
      (gloss-table . "| [[steinert:'gyur ba med pa]['gyur ba med pa]] | [[steinert:'dus]['dus]]                  | [[steinert:ma byas][ma byas]]               |
| immutable       | assembled, compounded | not made              |
| ADJ · predicate | V pf. ('du ba)        | NEG + V pf. (byed pa) |")
      (translations . "⟦39⟧ immutable ('gyur ba med pa: avikāra) and uncompounded ('dus ma byas: asaṃskṛta) \\\\
*Lindtner 1981:* unchangeable (avikāra) and uncompounded (asaṃskṛta). \\\\
*DharmaMitra:* Unchanging and unconditioned—")
      (structure . "continues the predicate chain; /'dus ma byas/ = /ma/ + past stem /byas/ with /'dus/ as complement (negated past participle).")
      (note . "")
      (concepts . "*'dus ma byas* (Skt. asaṃskṛta) — \"uncompounded/unconditioned\": in Abhidharma the phenomena not produced by causes and conditions (saṃskāra); here, in Madhyamaka usage, the ultimate nature (dharmatā/śūnyatā) as free from causal construction, against the saṃskṛta dharmas."))
     ((num . "40")
      (wylie . "rnal 'byor pa yis de rtogs na/ /")
      (gloss-table . "| [[steinert:rnal 'byor pa][rnal 'byor pa]] | yis | [[steinert:de][de]]           | [[steinert:rtogs][rtogs]]    | na           |
| yogin         | by  | that         | realizes | if           |
| N · agent     | ERG | DEM · object | V tr.    | CONV · cond. |")
      (translations . "⟦40⟧ if a yogin (rnal 'byor pa: yogin) realizes that, \\\\
*Lindtner 1981:* *When* a yogi understands it \\\\
*DharmaMitra:* *When* a yogi realizes this, \\\\
→ both: temporal \"when\" for the conditional \"if\"")
      (structure . "rnal 'byor pa =yis= [agent, ERG] · de [object, ABS — picks up 38–39] · rtogs [V] ~na~ [conditional] → protasis of 41.")
      (note . "/na/ = \"if\" (realization as condition); \"when\" is grammatically possible as well (both §2.8).")
      (concepts . ""))
     ((num . "41")
      (wylie . "nyon mongs shes bya'i sgrib pa spangs/ /")
      (gloss-table . "| [[steinert:nyon mongs][nyon mongs]] | [[steinert:shes bya'i][shes bya'i]]      | [[steinert:sgrib pa][sgrib pa]]    | [[steinert:spangs][spangs]]                       |
| affliction | of the knowable | obstruction | abandoned                    |
| N          | N + GEN         | N · object  | V pf. ('pong ba) · main verb |")
      (translations . "⟦41⟧ the obstructions of affliction (nyon mongs: kleśa) and of the knowable (śes bya'i sgrib pa: jñeyāvaraṇa) are abandoned. \\\\
*Lindtner 1981:* *he abandons* the obstructions of defilement and cognizable *(kleśajñeyāvaraṇa)*. \\\\
→ active, with the yogin as agent; and one compound, not two obstructions \\\\
*DharmaMitra:* The obscurations of afflictions and the knowable are abandoned.")
      (structure . "[nyon mongs · shes bya'i sgrib pa] [ABS] · spangs [main verb]; agent (the yogin) implicit from 40, copula elided (verse).")
      (note . "/nyon mongs shes bya'i sgrib pa/ — the two obstructions (kleśāvaraṇa + jñeyāvaraṇa) or a single compound? Doctrinally the pair; the Tibetan is one nominal string.  /spangs/ active (\"he abandons\") or resultative passive — ambiguous.")
      (concepts . "*nyon mongs kyi sgrib pa / shes bya'i sgrib pa* (Skt. kleśāvaraṇa / jñeyāvaraṇa) — the two obstructions: the afflictive one obstructs liberation (mokṣa), the cognitive one omniscience (sarvajñatā). Only a buddha has removed both; śrāvakas and pratyekabuddhas only the first. That realizing the nature of 38–39 removes both is a characteristically Mahāyāna claim.")))
    (grammar . "This four-pāda verse (verse 9 in the treatise) consists of two predicate-attribute pādas (38–39) describing an unnamed referent (the paramārtha/dharmatā established in prior verses) followed by a conditional converb clause (40: rtogs + na) and its apodosis (41: spangs). The subject of the conditional (rnal 'byor pa) is marked ergative (yis) as agent of the transitive verb rtogs; the apodosis (spangs) has an implicit agent (the same yogin) and an implicit copula, both licensed by verse-register compression. The demonstrative de in pāda 40 anaphorically picks up the entire description of segments 38–39.")
    (comparison . "*Translation:* Indescribable (brjod du med pa: avācya) and invisible (bltar med pa: adraṣṭavya), immutable ('gyur ba med pa: avikāra) and uncompounded ('dus ma byas: asaṃskṛta) — if a yogin (rnal 'byor pa: yogin) realizes that, the obstructions of affliction (nyon mongs: kleśa) and of the knowable (śes bya'i sgrib pa: jñeyāvaraṇa) are abandoned.

*Lindtner 1981:* [It is also] inexpressible (avācya) and invisible (adraṣṭavya), unchangeable (avikāra) and uncompounded (asaṃskṛta). *When* a yogi understands it *he abandons* the obstructions of defilement and cognizable *(kleśajñeyāvaraṇa)*.

*DharmaMitra:* Inexpressible and invisible, / Unchanging and unconditioned— / *When* a yogi realizes this, / The obscurations of afflictions and the knowable are abandoned.

Both read the realization temporally (\"when\"). Lindtner makes the yogin the agent of the abandoning and gives the two obstructions as one compound; DharmaMitra and the translation here read two obstructions and a passive result.")
    (working-translation . "") (my-notes . "") (footnotes . ""))
  "Felder von bDen Satz 9 (Tibetisch V) in der Form der Vorlage.")

(provide 'bcat-template-sent-009)
;;; bcat-template-sent-009.el ends here
