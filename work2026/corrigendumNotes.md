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
