# Vitis gap analysis, 2026 taxonomy update: what changed for each taxon (15 Nov 2026)

This replaces the note of 6 Nov 2026. Two things are new, both answers to co-author queries on the updated results
(Colin's review of the score changes, 13 Nov; Wes and Nan on V. rufotomentosa in Maryland, 13 Nov): V. berlandieri was
re-run on 13 Nov without two far-outlying points, and V. rufotomentosa on 15 Nov with Maryland added to its state range
list. The figure and SI data packages of 15 Nov 2026 (`data/publicationUpdate_20261115.zip`,
`data/datasetsForPublication_20261115.zip`) carry this state, and the website documents for both taxa and the two
all-taxa pages are updated in the website repository (not yet pushed).

Companion files: `teamStatus_20261115.xlsx` (one row per gap taxon, current and published values side by side, the
reviewer's decisions, why it was or was not re-run), `taxonEffects_20261115.csv` (the same, compact),
`data/datasetsForPublication/gapAnalysisSummary_combined_20261115.csv` (the combined results table), the aggregate run
folder `data/Vitis/runAggregate2026_1k/`, and the final occurrence dataset
`data/processed_occurrence/model_data20261113.csv` (113,392 records).

## The two re-runs

**V. berlandieri (run11132026_1k).** Colin asked why the geographic filter had left a point near St. Louis and one in
Baja California. Both carried the name "Vitis berlandieri Planch." and sat under V. cinerea in the published dataset,
where cinerea's US-only FNA state list kept every Mexican cinerea record, and so these, out of the model. The review
moved them to V. berlandieri, which has no state list (a US-only list would drop its Mexican range), so nothing screened
them. The St. Louis record is a 1906 MO specimen whose locality is "Missouri Botanical Garden", a cultivated plant; the
Baja California record is a 1953 UC collection from the Santo Domingo River that also enters twice without coordinates.
All four records were removed as the second rule of the documented removal step (`reviewRemovals.R`;
`removedByReview_20261113.csv`) and the taxon re-run. The two points had each brought a whole ecoregion into the natural
area (Ozarks; California South Coast).

| | Published | 19 Oct run | 13 Nov run |
|---|---|---|---|
| Records / with coordinates | 736 / 359 | 1,371 / 628 | 1,367 / 626 |
| Natural area ecoregions | 8 | 21 | 19 |
| Predicted range (km2) | 137,908 | 352,132 | 251,893 |
| FCSex (category) | 46.7 (HP) | 23.0 (UP) | 26.2 (HP) |
| FCSin (category) | 25.8 (HP) | 39.1 (HP) | 35.9 (HP) |
| FCSc mean (category) | 36.3 (HP) | 31.1 (HP) | 31.0 (HP) |

The ex situ category returns to HP, as published, so the "ex situ HP to UP" change in the earlier notes no longer holds.
Still open: about 97 other Mexican records that were V. cinerea in the published dataset (Jalisco 29, Veracruz 14,
Michoacán 5, plus Chihuahua, Chiapas and others) reach the berlandieri model unscreened, because the sheet gives its
Mexican range only as "MEXICO". A Mexican state list, from Anne or Jun, would mean one more re-run of this taxon.

**V. rufotomentosa (run11152026_1k).** Wes asked whether the taxon is in Maryland, since the map showed none and a
Maryland botanist's collections had prompted the correction. The records are in the dataset: six 2023 Eastern Shore
sheets by C. T. Frye (NCU, TAWES, MARY; Idylwild Wildlife Management Area, Chesapeake State Forest, Route 50 west of
Salisbury), all labelled V. rufotomentosa and recovered by the verbatim-name fix. The state range list adopted on 19 Oct
(Weakley: FL, GA, SC, NC) removed them before modelling; Anne had noted the taxon is "perhaps more widespread" and the
19 Oct message asked whether Maryland was plausible, without an answer. Maryland is now on the list and the taxon was
re-run on the same records: 17 modelled points (Florida 12, North Carolina 2, Maryland 3). The SDM on those points
fails the published robustness rule (STAUC 0.22 against the 0.15 limit; the 14-point model of 19 Oct had 0.10), so the
result comes from the 50 km buffer method the methods describe for such cases. It is the only taxon of the update on
the buffer method; every model of the published run passed the rule.

| | Published | 19 Oct run (SDM) | 15 Nov run (buffer) |
|---|---|---|---|
| Records / with coordinates / modelled | 2 / 0 / none | 42 / 20 / 14 | 42 / 20 / 17 |
| Area (km2) | not scored | 17,942 (predicted range) | 59,479 (buffer) |
| FCSex (category) | not scored | 1.7 (UP) | 1.7 (UP) |
| FCSin (category) | not scored | 36.3 (HP) | 43.5 (HP) |
| FCSc mean (category) | not scored | 19.0 (UP) | 22.6 (UP) |

## The 40 gap taxa in two groups

| Group | Taxa | What it means |
|---|---:|---|
| Unchanged | 15 | No taxonomic record change. Published values stand exactly. |
| Updated | 25 | Records changed with the review, or a state list was added. The result comes from the run named in `sourceRun`. |

| Source run | Taxa | Why |
|---|---:|---|
| run11152026_1k (15 Nov) | 1 | Maryland added to the state range list: rufotomentosa (buffer method). |
| run11132026_1k (13 Nov) | 1 | Two far-outlying points removed after a co-author query: berlandieri. |
| run10222026_1k (22 Oct) | 1 | Reviewer-requested record removal after the review closed: tiliifolia. |
| run10192026_1k (19-20 Oct) | 10 | Records changed with the final sheet edits: aestivalis, aestivalis var. bicolor, cinerea, cinerea var. cinerea, cinerea var. tomentosa, labrusca, popenoei, riparia, rotundifolia, vulpina. |
| run10192026_1k (19-20 Oct) | 4 | State range list added: baileyana, lincecumii, munsoniana, simpsonii. |
| run10022026_1k (2 Oct) | 2 | Re-run after the first sheet edits, records unchanged since: acerifolia, shuttleworthii. |
| run09162026_1k (16 Sept draft) | 6 | Records identical to the final dataset: aestivalis var. aestivalis, martineziana, mustangensis, x champinii, x doaniana, x novae-angliae. |

## What changes against the published table

| Taxon | Records | FCS combined | Category | Why |
|---|---|---|---|---|
| V. rotundifolia | 27,551 to 27,308 | 24.7 to 28.1 | UP to HP | Through the re-fitted model, not the counts (see the 6 Nov note); stable against the Sept draft. |
| V. x champinii | 126 to 192 | 52.6 to 46.1 | MP to HP (ex situ LP to MP) | 66 genebank accessions republished on GBIF under the hybrid name; all agreed by Anne. |
| V. rufotomentosa | 2 to 42 | not scored to 22.6 | not scored to UP (ex situ UP, in situ HP) | Scored for the first time; buffer method (above). |
| V. berlandieri | 736 to 1,367 | 36.3 to 31.0 | HP, unchanged in every category | 627 records moved in from V. cinerea, all agreed; the in situ score rises (25.8 to 35.9), the ex situ score falls (46.7 to 26.2) but stays HP. |

## Two team decisions, raised by Colin's review

**V. cinerea var. tomentosa and V. acerifolia: let the published values stand?** Both have a final record set identical
to the published one (tomentosa: the same 11 GBIF sheets, 9 unique coordinates; acerifolia: Anne kept its 3
novomexicana records). Both were re-run only because their records moved during the drafts, and both moved only because
a re-fit gives a different model (the published run was unseeded). Tomentosa's SRSin fell from 20 to 0 because one
protected point, at Arteaga, Coahuila, now sits at median suitability 0.577 against a threshold of 0.584 and so falls
just outside the predicted range; same points, same protected-area layer. Under the project's own rule, published values
stand where records did not change: tomentosa would report 22.0 (UP) and acerifolia 32.2 (HP), their published values.
No category changes either way. The tables of this note still carry the re-run values pending that decision.

**V. berlandieri: a Mexican state list?** See above. If one is adopted, the taxon is re-run once more and the all-taxa
products rebuilt once more.

## Other open items (none blocks the table)

With the reviewer: whether V. obovata Raf. belongs in labrusca (no coordinates, no score effect); whether the
NatureServe state lists for simpsonii and baileyana agree with Weakley; whether the four remaining US records of
V. tiliifolia (IL, LA, two with no state) should also go. Methods notes for the corrigendum are in
`work2026/corrigendumNotes.md` (entries of 13 and 15 Nov added; the 22 Oct tiliifolia removal still needs its entry).
