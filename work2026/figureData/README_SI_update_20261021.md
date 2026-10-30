# Supplementary information, 2026 taxonomy update: what changed on each page (21 Oct 2026)

> Superseded on 29 Oct 2026 by `data/SI_update_20261029/` (README_SI_update_20261029.md). Built by the script now named `packagePublicationUpdate.R`.

Folder `data/SI_update_20261021/`: one workbook per supplementary page, a single sheet each, ready to drop into the
Google Sheet that holds the SI. The values come from the rebuilt publication dataset of 20 Oct 2026
(`data/datasetsForPublication_20261020/`, built from the aggregate run `runAggregate2026_1k`); this folder only
re-cuts it by page. Built by `work2026/figureData/packagePublicationUpdate_20261021.R`.

Why anything changed at all: the taxonomic review of the occurrence records (Anne Frances, closed 19 Oct 2026) moved
records between names, hybrid names are now parsed, and five taxa elevated to species received a state range list.
Of the 40 gap taxa, 15 keep their published values exactly and 25 carry a result from a re-run (see
`gapAnalysisSummary-sources.csv` in this folder for the run behind each taxon). Two taxa change their combined
priority category (V. rotundifolia UP to HP, V. x champinii MP to HP) and one changes a component category
(V. berlandieri ex situ HP to UP). V. rufotomentosa has a modelled result for the first time (42 records; the
published table had 2 and no score).

| Page | File | Updated? | Why, and what to expect |
|---|---|---|---|
| 1 | sheet01_allSpeciesOccurrences.xlsx | Yes | The final occurrence dataset (113,422 records) after the review: records re-assigned between names, the hybrid-name records added, synonyms the reviewer asked to keep restored. Same columns as published except the hand-added `recordID`. |
| 2 | sheet02_modelVariables.xlsx | No | Predictor list; the models used the same variables. Supplied so the folder is complete. |
| 3 | sheet03_modelData.xlsx | Yes | Presence points with predictor values for every modelled taxon (61,552 rows, 39 taxa). Rows change for the 25 updated taxa; rufotomentosa appears for the first time. |
| 4 | sheet04_variableSelections.xlsx | Yes | Variable importance and selection per taxon (37 taxa, jaegeriana left out as published). Re-fitted models select slightly different variable sets. |
| 5 | sheet05_modelEvaluations.xlsx | Yes | Per-fold AUC and threshold statistics for the re-fitted models. Every model still passes the robustness rule (`SDM_testConditions.csv` in this folder). |
| 6 | sheet06_gapAnalysisSummary.xlsx | Yes | The results table. Two extra columns, `status` (unchanged / updated) and `sourceRun`, say where each row comes from; unchanged rows are the published values verbatim. This is the page the corrigendum table is read from. |
| 7 | sheet07_exsituCollectionSummary.xlsx | Yes | Germplasm records by database and institution: 3,198 G records (published 3,168), the hybrid-name genebank accessions being the main addition. |
| 8 | sheet08_pointsInProtectedAreas.xlsx | Yes, and larger | Occurrence points per protected area, now 2,877 areas and 14,681 points. The published page (734 areas, 4,806 points) was an incomplete join to one of the three WDPA chunks; the per-taxon files behind it already held about 14,700 points. This page therefore changes more than the records do. |
| 9 | sheet09_predictedInProtectedAreas.xlsx | Yes, slightly | Maximum predicted richness per protected area (7,900 areas); 31 areas change value. |
| 10 | sheet10_optimalProtectedAreas.xlsx | Yes, replaced | Greedy minimum set of protected areas covering every taxon with a record. The number of areas (16) is the stable result; the specific list is one of many equally good answers and changes with any input change (only 7 of the published areas recur). Read it as a new table, not an edit of the old one. |
| 11 | sheet11_multipleBufferSizes.xlsx | No | Buffer-size method comparison; not re-run. Supplied so the folder is complete. |

Helper files in the folder: `gapAnalysisSummary-sources.csv` (status and source run per taxon),
`SDM_testConditions.csv` (robustness rule per model).

Reading notes: the record counts on page 6 pool the varieties into V. aestivalis and V. cinerea, as the models do,
while the published page printed species-only counts; the scores are comparable, those two record counts are not.
Methods notes for the corrigendum text are in `work2026/corrigendumNotes.md`.
