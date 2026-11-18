# Vitis gap analysis, 2026 taxonomy update: what changed for each taxon (6 Nov 2026)

This replaces the note of 20 Oct 2026. One thing is new since then: V. tiliifolia was re-run on 22 Oct after Anne asked for its
California and Florida records to be removed. Everything else is as reported on 20 Oct, and the figure and SI data packages of
29 Oct 2026 already carry this state.

Companion files: `teamStatus_20261106.xlsx` (one row per gap taxon, current and published values side by side, the reviewer's
decisions, why it was or was not re-run), `taxonEffects_20261106.csv` (the same, compact),
`data/datasetsForPublication/gapAnalysisSummary_combined_20261029.csv` (the combined results table), the aggregate run folder
`data/Vitis/runAggregate2026_1k/` (every taxon's result files from its source run, for the all-species summaries), and the final
occurrence dataset `data/processed_occurrence/model_data20261022.csv` (113,396 records).

## The review is closed and the runs are done

Anne returned the workbook (`taxonChangeRawData_20260924v2.xlsx`) on 19 Oct with a decision on every one of its 3,674 rows.
Her 108 rows marked "Unclear" or "Review" were settled by project decisions the same day (`reviewDecisions_20261019.md`):
V. bicolor to var. bicolor (87), V. argentifolia to species level (9), V. odoratissima stays out (6), the two martineziana
records answered, and six riparia and rufotomentosa rows. The sheet edits were made on 19 Oct, the dataset rebuilt, and the
models re-run overnight. On 22 Oct Anne asked, by email, for the V. tiliifolia records in California and Florida to be deleted
because the taxon does not occur in the contiguous US. The 26 records (all herbarium, none germplasm) were removed as a
documented preprocessing step (`preprocessing/functions/reviewRemovals.R`, removed rows in
`data/processed_occurrence/removedByReview_20261022.csv`) and the taxon was re-run the same day. Every SDM passes the
robustness rule; no taxon falls back to the 50 km buffer.

## The 40 gap taxa in two groups

| Group | Taxa | What it means |
|---|---:|---|
| Unchanged | 15 | No taxonomic record change. Published values stand exactly. |
| Updated | 25 | Records changed with the review. The result comes from the run named in `sourceRun`. |

Updated taxa by source run, following the rule that compute goes only where it changes something (a taxon is re-run only when
its record set changed or a state range list was added; otherwise the earlier run whose records are byte-identical to the
final dataset stands, checked by fingerprint):

| Source run | Taxa | Why |
|---|---:|---|
| run10222026_1k (22 Oct) | 1 | Reviewer-requested record removal after the review closed: tiliifolia. |
| run10192026_1k (19-20 Oct) | 11 | Records changed with the final sheet edits: aestivalis, aestivalis var. bicolor, berlandieri, cinerea, cinerea var. cinerea, cinerea var. tomentosa, labrusca, popenoei, riparia, rotundifolia, vulpina. |
| run10192026_1k (19-20 Oct) | 5 | State range list added (see below): baileyana, lincecumii, munsoniana, rufotomentosa, simpsonii. |
| run10022026_1k (2 Oct) | 2 | Re-run after the first sheet edits, records unchanged since: acerifolia, shuttleworthii. |
| run09162026_1k (16 Sept draft) | 6 | Records identical to the final dataset: aestivalis var. aestivalis, martineziana, mustangensis, x champinii, x doaniana, x novae-angliae. |

## What changes against the published table

Two taxa change their combined priority category, one changes a component category, and one taxon that the published table
could not score is scored for the first time. None of these is new since 20 Oct, but V. rufotomentosa was missing from the
category-change table of the earlier notes because the comparison skipped a taxon with blank published values.

| Taxon | Records | FCS combined | Category | Why |
|---|---|---|---|---|
| V. rotundifolia | 27,551 to 27,308 | 24.7 to 28.1 | UP to HP | Comes through the model, not the counts. The 243 records that left for V. munsoniana (0.9%) change the fitted SDM: the predicted range shrinks from 761,860 to 651,963 km2 and from 18 to 17 ecoregions, and the same nine germplasm accessions now cover 8 of 17 ecoregions instead of 6 of 18. ERSex rises from 33 to 47, FCSex from 12.0 to 17.0. The 16 Sept draft gave the same numbers to three decimals (the driver seeds each taxon), so this is a stable consequence of the corrected concept, not run-to-run noise. |
| V. x champinii | 126 to 192 | 52.6 to 46.1 | MP to HP (ex situ LP to MP) | 66 genebank accessions republished on GBIF under the hybrid name in the verbatim field; Anne agreed all 66. |
| V. berlandieri | 736 to 1,371 | 36.3 to 31.1 | HP, unchanged (ex situ HP to UP, 46.7 to 23.0) | 627 records moved in from V. cinerea, all agreed; the in situ score rises (25.8 to 39.1). |
| V. rufotomentosa | 2 to 42 | not scored to 19.0 | not scored to UP (ex situ UP, in situ HP) | The published table had 2 germplasm records and no coordinates, so no model ran and every score was blank. The review brought 40 records in from V. aestivalis (39 agreed, 1 settled by project decision); with the Weakley state list applied (FL, GA, SC, NC; 4 points removed) 14 modelled points remain and the SDM passes the robustness rule. First-time scores: SRSex 5.0, GRSex 0, ERSex 0, FCSex 1.7; FCSin 36.3; FCSc 19.0. |

### V. tiliifolia, the one result that moved since 20 Oct

| | Published (run08282025_1k) | 20 Oct (run10192026_1k) | Now (run10222026_1k) |
|---|---|---|---|
| Records / with coordinates | 3,912 / 2,389 | 4,207 / 2,622 | 4,181 / 2,615 |
| SRSex / GRSex / ERSex | 0.5 / 0.0 / 11.3 | 0.8 / 4.5 / 19.7 | 0.8 / 4.6 / 20.0 |
| FCSex / FCSin / FCSc mean | 3.9 / 52.8 / 28.4 | 8.3 / 53.0 / 30.6 | 8.4 / 53.6 / 31.0 |
| Categories (ex / in / combined) | UP / MP / HP | UP / MP / HP | UP / MP / HP |

The record gain against the published table (269 records) is the taxonomy correction from the review; the removal of the
26 California and Florida records then trims the modelled area at its northern and western margin (the natural area no
longer reaches either state), which is why the in situ score and the combined score rise slightly. All three categories
stay where they were. The all-taxa layers (species richness, gap richness, ex situ ERS richness) were rebuilt on 29 Oct with
the new surface and are in the figure package of that date.

Other record shifts, all expected from the taxonomy correction and none changing a category: munsoniana 22 to 265 (22.1 to
20.0), lincecumii 168 to 332 (21.3 to 20.8), baileyana 531 to 657 (16.6 to 17.1), simpsonii 913 to 1,013 (19.7 to 21.4),
cinerea 5,341 to 5,091 (24.5 to 22.7), popenoei 227 to 225 (30.0 to 29.9, HP), cinerea var. tomentosa 11 to 11 (22.0 to
18.0), aestivalis var. bicolor 1,412 to 1,496 (17.3 to 17.4), acerifolia 855 to 855 (32.2 to 35.1, HP). V. acerifolia
deserves a word: Anne kept its three V. novomexicana records, so its final record set is identical to the published one, but
its scores come from the 2 Oct re-run made after that sheet edit rather than from the published run. Under the rule that
published values stand where records did not change, the team may prefer to move it to the unchanged group (published
values 32.2, HP; category unaffected either way).

One reading note: the run folders pool the varieties into V. aestivalis and V. cinerea (the published model did the same), but the
published table printed species-only record counts. So the record deltas for those two are not like for like; the scores are.

## The new state range lists

The published run filtered US, Mexican and Canadian points against the state list in FNA, which only exists for taxa FNA treats as
species. The five elevated taxa now have lists from other sources, recorded in the data file
(`data/source_data/FNA_stateClassification.csv`, columns `source` and `dateAdded`; tracked copy
`work2026/rerun/stateListsAdded_20261019.csv`). Points removed from the models against each taxon's previous run:

| Taxon | List | Source | Points removed |
|---|---|---|---|
| rufotomentosa | FL, GA, SC, NC | Weakley, via Anne | 4 (3 Maryland, 1 Alabama) |
| lincecumii | AR, OK, LA, TX | Weakley, via Anne | 10 (2 Kansas, 1 New Mexico, 2 Missouri, 1 Florida, 4 USDA accessions in Coahuila and Nuevo León) |
| munsoniana | AL, FL, GA | FNA, var. munsoniana entry | 2 (the Davis vineyard vouchers, 1 Texas 1890) |
| simpsonii | AL, AR, FL, GA, MS, NC, SC, VA | NatureServe; Weakley check asked | 19 (Maryland 7, Louisiana 6, Texas 3, the Davis vouchers, DC, Missouri) |
| baileyana | AL, AR, GA, IN, KY, NC, OH, PA, SC, TN, VA, WV | NatureServe; Weakley check asked | 6 (Mississippi 4, Maryland 2) |

V. berlandieri keeps no list, as published, so its Mexican range stays in the model. V. tiliifolia has no list either; its
out-of-range records were handled by the explicit removal above rather than a state filter. The methods text needs a sentence
naming the additional sources and the removal step (`corrigendumNotes.md`).

## What could still change

Nothing in the table waits on a decision. Three questions are with Anne, none of which blocks the table:

- Whether the three "Vitis obovata Raf." genebank records should return to the V. labrusca record count. None has coordinates;
  no score moves either way.
- Whether the NatureServe state lists for simpsonii and baileyana agree with Weakley. A different list would mean one more
  re-run for that taxon (about 10 minutes each).
- Whether the four remaining US records of V. tiliifolia (Illinois 1, Louisiana 1, two with no state) should also go. Removing
  them would mean one more short re-run; no category change is expected.

## Asks

1. Confirm the table can go into the corrigendum as the final values: 15 unchanged, 25 updated, two combined-category
   changes (rotundifolia, x champinii), one component change (berlandieri ex situ) and one taxon scored for the first time
   (rufotomentosa).
2. Send the message to Anne so the three open questions close.
3. The methods notes for the corrigendum are in `work2026/corrigendumNotes.md` (sheet-based concepts, the hybrid parser fix,
   the non-FNA state lists, the aestivalis variety names, the re-run policy and the rotundifolia change, plus the
   germplasm-dataset limitation left for a future methods pass); the tiliifolia removal step still needs its sentence there.
