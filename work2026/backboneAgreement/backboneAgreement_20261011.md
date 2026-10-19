# Do the taxonomic splits hold up against WFO and NatureServe?

Prepared 2026-10-11 for the core project team. Companion workbook: `backboneAgreement_20261011.xlsx`
(tabs Flows, Dropped, New records, Concepts, Names, plus the raw WFO and NatureServe pulls). Script:
`backboneAgreement.R`. Raw API pulls are cached in `cache/` so the run repeats offline.

## Question

The 2026 update moves GBIF records between taxa because the GBIF backbone had been undoing the project's
taxonomy (folding elevated segregates back into their parents). Each move needs sign-off from the
taxonomic review, which is slow. This check asks a narrower question for each move: do two independent
backbones, World Flora Online (WFO) and NatureServe Explorer, place the name where the project places it?
Where all three agree, the move is uncontroversial and can run now. Where they do not, it waits for the review.

## Sources

| Source | What it is | Version |
|---|---|---|
| Project taxonomy | "New World Vitis" sheet, 40 concepts with include and exclude name lists | snapshot 2026-10-05 |
| WFO | World Flora Online Plant List, the consensus nomenclatural backbone | release 2026-06, read via ChecklistBank dataset 2004 (WFO's own servers have a broken certificate chain) |
| NatureServe | Explorer API, one record per Vitis concept with synonyms and Element Code | pulled 2026-10-11 |
| Changed names | every name whose records moved between the published and round-2 datasets | `taxonChanges_published_vs_round2_detail_20260919.csv` |
| Review decisions | Anne's per-record decisions, rolled up per name | `taxonChangeRawData_20260924.xlsx` |

NatureServe is the concept authority the sheet already follows (its Element Codes are in the sheet), so it
answers "which concept". WFO answers "what is this name a synonym of". They are complementary.

## The six species-to-species moves

| From | To | Records | WFO | NatureServe | Anne | Verdict |
|---|---|---:|---|---|---|---|
| V. cinerea | V. berlandieri | 627 | accepted species | accepted concept | Agree 627 | **Run** |
| V. cinerea | V. baileyana | 123 | accepted as V. cinerea var. baileyana | accepted concept, same synonym | Agree 123 | **Run** (same circumscription, rank differs) |
| V. cinerea | V. simpsonii | 95 | accepted as V. cinerea var. floridana | accepted concept, same synonym | Agree 10, rest unreviewed | **Run** (same circumscription, rank differs) |
| V. rotundifolia | V. munsoniana | 243 | accepted as V. rotundifolia var. munsoniana | accepted concept, under the same varietal name | Agree 4, rest unreviewed | **Run** 194 (three V. munsoniana author spellings); 49 Muscadinia munsoniana records are WFO-only because NatureServe lacks that combination |
| V. aestivalis | V. lincecumii | 164 | not recognised at any rank; V. linsecomii is a synonym of V. aestivalis | accepted concept | Agree 164 | **Hold on backbone evidence**, but already approved by the review |
| V. aestivalis | V. rufotomentosa | 26 | synonym of V. aestivalis | accepted concept | Agree 1, rest unreviewed | **Hold**: WFO lumps it |

Three of the six destination concepts (baileyana, simpsonii, munsoniana) are recognised by both backbones but as
varieties rather than species. The circumscription is identical, so the record flow is the same whichever
rank is used. The split is agreed in substance; only the rank is a project decision.

## Concept-level picture (40 project concepts)

| Agreement | Concepts |
|---|---:|
| Both backbones accept the concept as named | 19 |
| Both recognise it, under another name or rank (baileyana, simpsonii, munsoniana) | 3 |
| NatureServe accepts, WFO lumps into V. aestivalis (lincecumii, rufotomentosa) | 2 |
| NatureServe accepts the variety, WFO folds it into V. aestivalis (var. aestivalis, var. bicolor) | 2 |
| WFO accepts, NatureServe has no concept (extralimital Mexican and Central American taxa, plus cinerea var. cinerea and var. tomentosa) | 12 |
| Unknown to both (V. martineziana, V. rubriflora, recent Wen species) | 2 |

WFO treats V. aestivalis, V. cinerea and V. rotundifolia more broadly than the project. A WFO pointer to one of
those three species is therefore weaker evidence and is flagged "broad WFO concept" in the Names tab.

## Records the pipeline dropped (name matched no project concept)

| Group | Records | What to do |
|---|---:|---|
| Named hybrids with their own NatureServe concept (V. × labruscana 98, V. × slavinii 5) | 103 | Exclude, as the pipeline already does |
| WFO maps the name to a project concept, NatureServe has no entry | ~305 | Proposed include-list edits, listed in the Names tab (column sheetEditSuggested). Evidence from one backbone only, so these go to the review, pre-filled |
| Same, but the WFO target is a broadly defined species (aestivalis, cinerea) | 129 | Weaker evidence; review |
| No backbone opinion | 25 | V. araneosa J.Le Conte, V. bourquiniana, V. pullaria: unplaced in WFO, unknown to NatureServe |

Largest single-backbone items: V. caribaea to V. tiliifolia (84), V. coriacea to V. shuttleworthii (36, Anne
already said include), Muscadinia popenoei to V. popenoei (22), V. helleri and V. cordifolia var. helleri to
V. berlandieri (14), V. incisa and V. odoratissima to V. riparia (16). Some names were removed from both a
species and its variety and count twice in the workbook.

## Records new in round 2

| Group | Records | Note |
|---|---:|---|
| V. cordifolia Lam. and V. cordifolia, placed in V. vulpina | 181 | **No opinion.** WFO holds three homonyms (Michx. = V. vulpina, Roth = V. heyneana, (L.) Morales = Cissus). The GBIF authorship "Lam." matches none. Michx. is the only North American one. Review item |
| Genus-only records ("Vitis L.") that the pipeline assigned from verbatim names, mostly to hybrids | 125 | Outside what a backbone can check |
| V. vulpina subsp. riparia to V. riparia, V. candicans var. diversa to V. mustangensis, V. cordifolia var. sempervirens to V. vulpina | 5 | WFO agrees, NatureServe silent |

## Proposed decision

1. Run the berlandieri, baileyana and simpsonii splits and the 194 V. munsoniana records now. Project, WFO and
   NatureServe agree on the circumscription, and the review has agreed where it has looked.
2. Treat lincecumii as approved on the strength of the review (Anne agreed all 164), with the WFO disagreement
   noted in the corrigendum. Hold rufotomentosa for the review.
3. Send the single-backbone list (Names tab, rows marked ONE BACKBONE) to the review as pre-filled proposals.
4. Keep the hybrids excluded.
5. Ask the review specifically about V. cordifolia Lam. (181 records) and the three unplaced Le Conte and Munson names.

Backbones are evidence, not the review. Nothing here is applied to the sheet automatically.
