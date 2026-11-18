# Supplementary information, 2026 taxonomy update: pages as of 18 Nov 2026

Folder `data/SI_update_20261118/`: one workbook per supplementary page, a single sheet each, ready to drop into
the SI workbook. It replaces `data/SI_update_20261115/`. The values are the same build as 15 Nov 2026
(`data/datasetsForPublication_20261115/`, README there); nothing was re-run. What changed is the cut: the pages now
follow the numbering of the published SI (12 pages), after a co-author found that the 15 Nov package had no page for
the second model-evaluation table and numbered the later pages one short. Built by
`work2026/figureData/packagePublicationUpdate.R 20261118 20261115`.

Three corrections against the 15 Nov package:

1. **Page 6 added**: the robustness-rule table (ATAUC, STAUC, ASD15, Valid, Reason per modelled taxon), which the
   15 Nov package carried only as the helper file `SDM_testConditions.csv`. Pages 6 to 11 of the 15 Nov package are
   therefore pages 7 to 12 here.
2. **Pages 9 and 10 now hold the all-polygon tables** (66,535 WDPA polygons each) that the published SI holds, the
   tables the published data script built last ("I used the most recent results"). The 15 Nov package had cut these
   pages from the compact per-run tables instead (2,879 areas with points; 7,900 areas with a predicted-richness
   maximum, which only reaches 7). The compact tables stay in the folder as helper files. The published page 10
   reaches a richness of 10; so does this one.
3. **Page 12, buffer sizes**: unchanged and still three distances (10, 50, 100 km). The co-author recalls five in the
   published SI; the project holds results for three only (`data/Vitis/varBuffer/`, the file the published script
   copied), so the two others must be identified before they can be reproduced.

| Page | File | Changed against the published SI? | What to expect |
|---|---|---|---|
| 1 | sheet01_allSpeciesOccurrences.xlsx | Yes | 113,392 records (published 112,983 under the old concepts). Same columns. |
| 2 | sheet02_modelVariables.xlsx | No | |
| 3 | sheet03_modelData.xlsx | Yes | Presence rows with predictor values for the 39 modelled taxa, 61,549 rows. |
| 4 | sheet04_variableSelections.xlsx | Yes | Variable importance per modelled taxon. |
| 5 | sheet05_modelEvaluations.xlsx | Yes | Per-fold AUC. |
| 6 | sheet06_modelEvaluations2.xlsx | Yes | Robustness rule per model: 37 taxa (published 36). One model fails, V. rufotomentosa (STAUC 0.22 against the 0.15 limit), and that taxon is scored by the 50 km buffer method, as the methods describe. Every other model passes. |
| 7 | sheet07_gapAnalysisSummary.xlsx | Yes | One row per gap taxon, 40 rows, with `status` (15 unchanged, 25 updated) and `sourceRun`. The corrigendum table is read from this page. |
| 8 | sheet08_exsituCollectionSummary.xlsx | Yes | 3,198 G records (published 3,168). |
| 9 | sheet09_pointsInProtectedAreas.xlsx | Yes | 66,535 polygons: 2,887 hold occurrence points (published 2,884), 14,766 points in all (14,669); point count changes in 482 polygons; richest polygon holds 7 taxa, as published. |
| 10 | sheet10_predictedInProtectedAreas.xlsx | Yes, widely | 66,535 polygons; the maximum predicted richness changes in 37,307 of them (13,837 up, 23,470 down) because every all-taxa raster was rebuilt from the 25 re-fitted models. Maximum stays 10 (19 polygons reach it, published 33); polygons with any predicted taxon 34,411 (published 34,237). |
| 11 | sheet11_optimalProtectedAreas.xlsx | Yes, replaced | Greedy selection of 16 areas per seed, 20 seeds, 38 rows. Read as a new table. |
| 12 | sheet12_multipleBufferSizes.xlsx | No | Three distances, 38 taxa, 114 rows: the published file. |

Helper files in the folder: `gapAnalysisSummary-sources.csv` (status and source run per taxon),
`pointsInProtectedAreas-sheet8.csv` and `predictedInProtectedAreas-sheet9.csv` (the compact protected-area tables
the run folders write, named as in the dataset build). The page-by-page description of the 21 Oct package
(`README_SI_update_20261021.md`) still applies to the content of pages 1 to 5, 7, 8, 11 and 12 under their new numbers.
