# Vitis gap analysis, 2026 taxonomy update: where things stand (11 Oct 2026)

Companion files: `teamStatus_20261011.xlsx` (one row per gap taxon, current and published values side by side, what could change),
`taxonEffects_20261011.csv` (the same, compact), `data/datasetsForPublication/gapAnalysisSummary_combined_20261011.csv` (the combined
results table), and the current occurrence dataset `data/processed_occurrence/model_data20261002.csv`.

## The 40 gap taxa in four groups

| Group | Taxa | What it means |
|---|---:|---|
| Unchanged | 15 | No taxonomic record change. Published values stand exactly. |
| Updated | 8 | Stable on the review input already received: every record reassigned between concepts carries Anne's decision, and at most 10 records under dropped or added synonyms are unreviewed. Four needed a sheet edit and were re-run on 2 Oct (acerifolia, baileyana, lincecumii, shuttleworthii). Four needed no edit, so the 16 Sept draft run stands (aestivalis var. aestivalis, berlandieri, cinerea var. cinerea, mustangensis). |
| Provisional | 14 | Modelled on 16 Sept with the corrected taxonomy. Their records are byte-identical to the current dataset (fingerprint check), but some of their record changes have no reviewer decision yet. Reported as the current best numbers, with that caveat, until the review closes. |
| Needs input | 3 | popenoei, rufotomentosa, cinerea var. tomentosa. See below. |

The provisional group is: aestivalis, aestivalis var. bicolor, cinerea, labrusca, martineziana, munsoniana, riparia, rotundifolia, simpsonii,
tiliifolia, vulpina, x champinii, x doaniana, x novae-angliae.

Why draft-run results rather than a re-run: a re-run today would read exactly the same records with the same seed and the same method, so it
would reproduce the September numbers. Compute goes only to taxa whose records change after the review.

## What changes against the published table

Two taxa change their combined priority category, and both are provisional because the change rests on something still unreviewed:

| Taxon | Records | FCS combined | Category | Rests on |
|---|---|---|---|---|
| V. rotundifolia | 27,551 to 27,309 | 24.7 to 28.1 | UP to HP | 242 V. munsoniana records moved out. The split has not been signed off (241 of 243 unreviewed). |
| V. x champinii | 126 to 192 | 52.6 to 46.1 | MP to HP (ex situ LP to MP) | 66 added records carry only the genus name and were assigned from the verbatim hybrid formula. Unreviewed. |

One updated taxon changes a component category: V. berlandieri's ex situ class falls from HP to UP (46.7 to 23.1) while its in situ score rises
(25.8 to 39.1), so the combined class holds at HP (36.3 to 31.1). That follows from the 627 records moved in from V. cinerea, all agreed.

Largest record shifts, all expected from the taxonomy correction: berlandieri 736 to 1,357; munsoniana 22 to 265 (22.1 to 20.0, UP);
lincecumii 168 to 332 (21.3 to 19.5, UP); baileyana 531 to 654; simpsonii 913 to 1,008; cinerea 5,341 to 5,070 (24.5 to 22.7, UP);
tiliifolia 3,912 to 3,996 (28.4 to 30.7, HP).

One reading note: the run folders pool the varieties into V. aestivalis and V. cinerea (the published model did the same), but the
published table printed species-only record counts. So the record deltas for those two are not like for like; the scores are.

## What could still change

For the 14 provisional taxa, the records still open are the record changes that carry no reviewer decision in
`taxonChangeRawData_20260924.xlsx` (removed rows attach to the taxon that lost the record, added rows to the taxon that gained it).

| Taxon | Open records | Share | What they are |
|---|---:|---:|---|
| munsoniana / rotundifolia | 241 / 242 | 91% / 0.9% | the munsoniana split out of rotundifolia; 2 of 243 agreed so far |
| vulpina | 187 | 2.4% | V. cordifolia Lam. additions |
| aestivalis | 162 | 1.0% | dropped old names: V. bicolor (3 spellings), rufotomentosa, smalliana, araneosa |
| cinerea | 122 | 2.4% | the 85 simpsonii split records plus dropped names (berlandieri var. tomentosa, helleri, sola, ...) |
| labrusca | 112 | 2.1% | dropped V. x labruscana and other old names |
| tiliifolia | 87 | 2.2% | dropped V. caribaea; a further 311 records differ for non-taxonomic reasons (country exemption), outside the review |
| simpsonii | 85 | 8.4% | the split out of cinerea; 10 of 95 agreed so far |
| x champinii / x novae-angliae / x doaniana | 66 / 23 / 12 | 34% / 7.7% / 10% | genus-only records assigned from verbatim fields |
| riparia | 21 | 0.1% | dropped V. incisa, V. odoratissima; 3 added V. vulpina subsp. riparia |
| aestivalis var. bicolor | 9 | 0.6% | V. argentifolia; Anne suggested moving them to the species. Our decision pending. |
| martineziana | 2 | 14% | Anne asked how these were determined; they are non-GBIF (pers. comm. and a paper), answered |

Only two of these can move a priority category: the munsoniana split (rotundifolia UP to HP) and the x champinii genus-only records.
Everything else is within a few percent of the taxon's records.

## The three taxa that need input

- **V. popenoei**: 22 Muscadinia popenoei records (11% of the taxon) would join if that combination is added to the include list. Re-run after the decision.
- **V. rufotomentosa**: the concept needs the review. 42 records, 20 with coordinates. Its SDM fails the robustness rule, so the 50 km buffer method applies. Needs the review and an interpretation call on how to report it.
- **V. cinerea var. tomentosa**: zero records under the corrected taxonomy; 11 V. berlandieri var. tomentosa records would return if that synonym is added. Cannot be modelled either way. Needs a decision on how to report it.

## Asks

1. Confirm the framing: 8 updated on the review input already in hand, 14 provisional with a per-taxon caveat, so the corrigendum draft can proceed.
2. The two unreviewed items that move a category: the munsoniana split and the x champinii genus-only records. If either can be settled by the team, two more taxa become final.
3. Decisions on the three taxa above, which are the only ones blocking a complete table.

## An additional task while we wait

An independent check of the open names against World Flora Online and NatureServe is done (`work2026/backboneAgreement/`). It is not used
in the classification above. If the review stays stalled, it can be offered as supporting evidence for the open names where both backbones
place a name the same way the project sheet does (the munsoniana and simpsonii splits among them).
