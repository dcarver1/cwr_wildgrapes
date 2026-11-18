# Corrigendum notes: methods changes in the 2026 taxonomy update

One dated entry per change to how a result is produced. Each says what changed, which taxa it touches, and what in the
published methods it affects. Appended as work proceeds. Record-level taxonomy decisions themselves live in
`work2026/taxaReviewAnne/` (the reviewer's workbook and `reviewDecisions_20261019.md`); this file tracks only what the
paper's methods text has to say differently.

## 2026-09-16: concept definitions moved to the project taxonomy sheet

- **What changed.** Each gap taxon's concept is now defined by the "Names to include" and "Names to exclude" columns of the
  New World Vitis sheet, pulled at the start of preprocessing, with GBIF names parsed from the verbatim and interpreted
  name fields together (`nameSource = "combined"`). The published build matched GBIF's interpreted name only.
- **Taxa touched.** Every taxon whose records moved between concepts: cinerea to berlandieri, baileyana, simpsonii;
  rotundifolia to munsoniana; aestivalis to lincecumii, rufotomentosa; plus synonym additions and removals across 20 taxa.
  The reviewer (Anne) signed off every concept split on 2026-10-19.
- **Methods text affected.** The taxonomic framework and data cleaning paragraph: how names were assigned to concepts, and
  that the elevated taxa (berlandieri, baileyana, simpsonii, munsoniana, lincecumii, rufotomentosa) are treated as species
  following the project sheet rather than FNA's varietal ranks.

## 2026-10-19: hybrid formulas with authorship or a cultivar before the "x"

- **What changed.** The name parser treats a standalone " x " anywhere after the binomial as a hybrid formula
  ("Vitis aestivalis Michx. x V. vulpina L.", "Muscadinia rotundifolia cv. Yuga x Carlos"); such records go to no concept.
  Previously only a bare "Vitis riparia x Vitis rupestris" was caught. 98 GBIF rows change, all crosses.
- **Taxa touched.** aestivalis (6 records fewer), labrusca (2), rotundifolia (1).
- **Methods text affected.** Data cleaning: the statement that hybrid formulas were excluded from parental taxa now holds
  for all spellings.

## 2026-10-19: state-level range lists for the six elevated taxa, from sources other than FNA

- **What changed.** The published run filtered US, Mexican and Canadian points against the state list in FNA for taxa FNA
  treats as species. The six elevated taxa have no FNA list, so under the corrected taxonomy their out-of-range points
  (notably UC Davis vineyard vouchers for simpsonii and munsoniana) would reach the model unfiltered. Lists were added to
  `data/source_data/FNA_stateClassification.csv` (new `source` and `dateAdded` columns record where each list came from):
  rufotomentosa and lincecumii from Weakley's Flora of the Southeastern US as stated by the reviewer; munsoniana from FNA's
  own row for var. munsoniana; simpsonii and baileyana from the NatureServe distribution on the project sheet (reviewer
  asked to confirm against Weakley). Berlandieri keeps no list, as in the published run, because a US-only list would drop
  its Mexican range. The filter code itself is unchanged.
- **Taxa touched.** rufotomentosa, lincecumii, munsoniana, simpsonii, baileyana.
- **Accepted** by the project lead on 2026-10-19 after the first run (run10192026_1k). Points removed against the
  previous run: simpsonii 19 (Maryland 7, Louisiana 6, Texas 3, California, DC, Missouri; the 31 Davis vouchers share one
  coordinate), lincecumii 10 (including 4 in Coahuila and Nuevo León, since neither Weakley nor NatureServe gives the taxon
  a Mexican range), baileyana 6, rufotomentosa 4, munsoniana 2. Every SDM passed the robustness rule; rufotomentosa, which
  failed it on the September draft, passes with the filtered points.
- **Methods text affected.** The sentence describing the FNA state filter needs to name the additional sources
  (Weakley; NatureServe; FNA at varietal rank) for these five taxa.

## 2026-10-19: variety-level names in V. aestivalis

- **What changed.** Following the reviewer: "Vitis bicolor" (87 records, three spellings) is included in
  V. aestivalis var. bicolor; the 9 records named "Vitis argentifolia" (species rank) are removed from the variety and kept
  at species level, where FNA places them. The 400 records named "Vitis aestivalis var. argentifolia" stay with var.
  bicolor, as in the published run (an earlier draft of this entry had them leaving too; corrected 2026-10-19 after the
  first build showed the variety losing 412 records).
- **Taxa touched.** aestivalis var. bicolor (1,412 published, 1,496 now); aestivalis (pooled model only).
- **Methods text affected.** None beyond the sheet-based concept definition above; the supplementary synonym table.

## 2026-10-19: known limitation left in place, germplasm accessions republished on GBIF typed as herbarium records

- **What it is.** GBIF dataset 07044577 (the global crop wild relative occurrence database) republishes genebank
  accessions (Bioversity, USDA NPGS) as occurrences with basis of record "OCCURRENCE". The pipeline types every GBIF
  record that is not a living specimen as H, so these count as herbarium records, while the same accessions also enter as
  G through the direct GRIN, Genesys and WIEWS pulls. 41,499 Vitis records in the download, 90 percent without
  coordinates. The published build did the same.
- **Taxa touched.** All; the ex situ sampling score (SRSex) of every taxon with such records. Most visible for x champinii,
  where 135 of 192 records are these.
- **Decision.** Not changed in this update (no method revision on main). Recorded here for the next methods pass, and as a
  limitation statement for the corrigendum.

## 2026-10-20: which taxa were re-run, and a priority change that comes through the model

- **What changed.** With the review closed, 17 taxa were re-run on 19-20 Oct 2026 (run10192026_1k, input
  model_data20261019_2303.csv): the 12 whose record set changed with the final sheet edits and the 5 elevated taxa that
  received a state range list. The other 8 updated taxa keep an earlier run whose records are byte-identical to the final
  dataset (fingerprint check: acerifolia and shuttleworthii from 2 Oct; aestivalis var. aestivalis, martineziana,
  mustangensis, x champinii, x doaniana, x novae-angliae from the 16 Sept draft). The 15 unchanged taxa keep the published
  values. Every SDM in the update passes the robustness rule; no taxon uses the buffer fallback. Source run per taxon:
  `taxonomyUpdateStatus` in R2/summarize/aggregateRuns.R; the combined table is
  data/datasetsForPublication/gapAnalysisSummary_combined_20261020.csv.
- **Priority changes against the published table.** Two taxa change combined category: V. rotundifolia UP to HP (24.7 to
  28.1) and V. x champinii MP to HP (52.6 to 46.1). V. berlandieri changes its ex situ category (HP to UP, 46.7 to 23.0)
  while the combined class holds at HP. For rotundifolia the change comes through the model rather than the counts: the
  243 records that left for V. munsoniana (0.9 percent of the taxon) change the fitted SDM, whose range shrinks from
  761,860 to 651,963 km2 and from 18 to 17 ecoregions; the same nine germplasm accessions then cover 8 of 17 ecoregions
  instead of 6 of 18, so ERSex rises from 33 to 47 and FCSex from 12.0 to 17.0. The 16 Sept draft gave the same result to
  three decimals, so this is a stable consequence of the corrected concept under the published method, not run-to-run
  noise.
- **Methods text affected.** The results paragraph that lists priority categories, and a sentence in the corrigendum
  stating that re-fitted models can move a score through the predicted range even when the record change is small.

## 2026-11-13: two far-outlying V. berlandieri records removed at a co-author's query

- **What changed.** Reviewing the updated results, a co-author asked why the geographic filter had not removed a point
  near St. Louis and one in Baja California from V. berlandieri. Both records carried the name "Vitis berlandieri
  Planch." and sat under V. cinerea in the published dataset, where the US-only FNA state list of V. cinerea kept every
  Mexican cinerea record, and so these, out of the model; the review moved them to V. berlandieri, which has no state
  list (entry of 2026-10-19 above), so they reached a model for the first time. The St. Louis record is a 1906 MO
  specimen whose locality is "Missouri Botanical Garden", a cultivated plant; the Baja California record is a 1953 UC
  collection from the Santo Domingo River, which also enters twice without coordinates (UCJEPS). All four records are
  removed as the second rule of the documented preprocessing step (`preprocessing/functions/reviewRemovals.R`; removed
  rows in `data/processed_occurrence/removedByReview_20261113.csv`), giving `model_data20261113.csv` (113,392 records;
  every other taxon is record-for-record identical to the 22 Oct file). The taxon was re-run the same day as
  run11132026_1k (SDM passes the robustness rule; no buffer fallback).
- **Effect.** Records 1,371 to 1,367 (626 with coordinates). The natural area loses the two ecoregions only those points
  reached (Ozarks; California South Coast): 21 to 19 ecoregions. Predicted range 352,132 to 251,893 km2. FCSex 23.0 to
  26.2, FCSin 39.1 to 35.9, FCSc mean 31.1 to 31.0; combined category HP throughout. The ex situ category returns to HP,
  as published (46.7), so the "HP to UP" ex situ change reported in the entry of 2026-10-20 no longer holds.
- **Still open.** The same mechanism admitted about 97 other formerly-cinerea Mexican records to V. berlandieri without
  any geographic screen (Jalisco 29, Veracruz 14, Michoacán 5, plus Chihuahua, Chiapas and others); the sheet gives the
  Mexican range only as "MEXICO". A Mexican state list for the taxon is a question for the reviewer; if one is adopted,
  it goes into `data/source_data/FNA_stateClassification.csv` with its source and the taxon is re-run again.
- **Methods text affected.** Data cleaning: the sentence on reviewer-requested removals (with the tiliifolia removal of
  2026-10-22) should name this one; the results paragraph on priority categories, where berlandieri's ex situ class is
  now unchanged from the published table.

## 2026-11-15: Maryland added to the V. rufotomentosa range list; the taxon moves to the buffer method

- **What changed.** A co-author asked why V. rufotomentosa's map showed no Maryland points when a Maryland botanist's
  collections were what prompted the correction. The records are in the dataset: six 2023 Eastern Shore sheets by
  C. T. Frye (NCU, TAWES, MARY; Idylwild Wildlife Management Area, Chesapeake State Forest, Route 50 west of Salisbury),
  all labelled V. rufotomentosa and recovered by the verbatim-name fix. The state range list adopted on 2026-10-19
  (Weakley: FL, GA, SC, NC) removed them before modelling; the reviewer had noted the taxon is "perhaps more widespread"
  and the 19 Oct message asked whether Maryland was plausible, without an answer. Maryland is now on the list
  (`data/source_data/FNA_stateClassification.csv`, source and date recorded) and the taxon was re-run on the same input
  (model_data20261113.csv) as run11152026_1k. Records unchanged (42; 20 with coordinates); modelled points 14 to 17
  (Florida 12, North Carolina 2, Maryland 3 after the 1 km thinning); the natural area gains the Chesapeake Bay Lowlands.
- **Effect.** The SDM on 17 points fails the published robustness rule (STAUC 0.22 against the 0.15 limit; ATAUC 0.78),
  so the result comes from the 50 km buffer method the methods text describes for such cases. It is the only taxon in
  the update on the buffer method; every model of the published run passed the rule, and so did the 14-point model of
  19 Oct (STAUC 0.10). Scores: FCSex 1.7 (unchanged; SRSex 5, no germplasm with coordinates), FCSin 36.3 to 43.5
  (SRSin 33 to 24, GRSin 8.8 to 7.0, ERSin 67 to 100 over 5 ecoregions), FCSc mean 19.0 to 22.6. Categories unchanged:
  UP / HP / UP. Area 17,942 to 59,479 km2 (buffer, not predicted range).
- **Methods text affected.** The state-list sentence (entry of 2026-10-19 above) gains Maryland for rufotomentosa with
  its source; the sentence on the robustness rule and buffer fallback now has one taxon to name; the results paragraph
  that says every model passed must be qualified.

## 2026-11-16: evaluation of the geographic filter and of reproducibility (no method change)

- **What was checked.** Why tomentosa differs from the published value with identical records (the published run was
  unseeded; the update's driver repeats exactly, verified by a scratch re-run), why the state filter never applied to
  berlandieri (FNA has no row for it, treating it as V. cinerea var. helleri), which 17 gap taxa run with no filter,
  where NatureServe is a hard filter (simpsonii and baileyana only, since 19 Oct), and what NatureServe would remove
  if applied to every taxon (86 points across 10 taxa, mostly states FNA lists and NatureServe does not). Full note
  with tables and the check script: `work2026/geographyFilter/geographyFilterEvaluation_20261116.md`.
- **Methods text affected.** Add to the methods: the published models were fitted without a fixed random seed, so a
  re-fit of a taxon with unchanged input gives slightly different variable selections and surfaces; the update fixes
  seeds and worker random streams, and its results repeat exactly. The range-filter sentence should say the lists are
  FNA's for the taxa FNA treats and, for the five elevated taxa, Weakley, FNA at varietal rank or NatureServe as
  recorded per taxon in the list file. Berlandieri's sentence depends on decision 2 of the note.
- **Decisions still open.** Published values for tomentosa and acerifolia; a range list for berlandieri (FNA var.
  helleri is the ready-made option); Weakley check of the simpsonii and baileyana lists.

## 2026-11-18: all-taxa products leave out the two species assessed through their varieties

- **What changed.** Every product summed or selected over taxa (the predicted richness map, the GA50 and ERS gap
  richness maps, the points- and richness-per-protected-area tables, the optimal protected-area selection) now stacks
  one layer per taxon and leaves out V. aestivalis and V. cinerea, whose records are the pooled records of their
  varieties (`genusLevelTaxa()` in R2/summarize/speciesRichnessMap.R, applied in generateRunSummaries() and in the
  dataset compile script). The published run stacked the pooled species beside their varieties: its richness map had
  38 layers (the caption said 36), and cells where a species and one of its varieties both predicted presence were
  counted twice (3.1 million of 7.9 million cells with any richness; the maximum of 10 came only from such cells, 9
  without them). The ERS gap richness maps were already free of the pooled species, by a file-matching accident the
  code now states explicitly; the species lists written beside them named the pooled species anyway. Per-taxon results
  are untouched. Decision by the core team on the Correction draft (co-author comment, 2026-11-18).
- **Taxa touched.** None individually. All-taxa layers and tables: 37 taxa (36 SDMs, V. jaegeriana, V. rubriflora and
  V. rufotomentosa by the 50 km buffer); V. novogranatensis has no georeferenced record and is in no map.
- **Methods text affected.** The description of the richness and gap-richness maps and of the protected-area summaries
  should state that species with assessed varieties are represented by the varieties only. Figure 1 caption: 37 taxa,
  not 36 models. The published Figures 1 and 4 and SI pages 9 to 11 therefore change for this reason as well as for
  the taxonomy.
- **Size of the effect on SI page 10** (maximum predicted richness per WDPA polygon, 66,535 rows; script
  `work2026/figureData/sheet10Decomposition.R`, output `sheet10Decomposition_20261118.txt`). Published to published
  with the pooled layers removed: 18,718 polygons change, all downward, maximum 10 to 9. Published to the 15 Nov table
  (taxonomy only): 9,539 change, 5,419 up and 4,120 down, maximum stays 10. Published to the 18 Nov table (both):
  18,601 change, 1,390 up and 17,211 down, maximum 9 (19 polygons). Between the two genus-level tables (taxonomy
  alone, correctly counted): 9,711 change, 5,098 up and 4,613 down. The points-per-polygon table loses 172 polygons
  whose only records were the pooled species' own (2,887 to 2,715 polygons with points). The optimal protected-area
  selection (greedy) changes almost entirely with either input change, 6 areas in common with the published 17.
- **Protected-area layer.** WDPA polygons coded fully marine (MARINE = 2) are excluded and terrestrial (0) and
  coastal (1) polygons kept, in the rasterized layer behind the in situ scores and in the polygon tables alike
  (published preprocessing, unchanged). Coastal polygons are 2,165 of the 58,299 in the layer; 242 of them have a
  predicted richness above 0 (maximum 9). The compact 7,900-row table shipped as "sheet 9" in packages before 18 Nov
  came from one of the three WDPA download parts only (`wdpa_1.gpkg`) and is not a published page; the published
  pages 9 and 10 are the all-polygon tables.
