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
- **Methods text affected.** The sentence describing the FNA state filter needs to name the additional sources
  (Weakley; NatureServe; FNA at varietal rank) for these five taxa.

## 2026-10-19: variety-level names in V. aestivalis

- **What changed.** Following the reviewer: "Vitis bicolor" (87 records, three spellings) is included in
  V. aestivalis var. bicolor; "Vitis argentifolia" and "Vitis aestivalis var. argentifolia" (9 records) are removed from the
  variety and kept at species level, where FNA places them.
- **Taxa touched.** aestivalis var. bicolor; aestivalis (pooled model only).
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
