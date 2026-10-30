# Supplementary information, 2026 taxonomy update: pages as of 29 Oct 2026

Folder `data/SI_update_20261029/`: one workbook per supplementary page, a single sheet each, ready to drop into
the Google Sheet that holds the SI. It replaces `data/SI_update_20261021/`. The values come from the publication
dataset build of 29 Oct 2026 (`data/datasetsForPublication_20261029/`, README there); this folder only re-cuts it
by page. Built by `work2026/figureData/packagePublicationUpdate.R`.

What is new since the 21 Oct package: on the reviewer's request the Vitis tiliifolia records in California and
Florida (26 records) were removed and the taxon re-run; its scores move slightly (FCSc mean 30.6 to 31.0) and its
categories do not change (UP / MP / HP). Everything summed over all taxa was rebuilt. In addition, pages 8 and 10 of
the 21 Oct package had been computed from pre-final point files for 11 of the re-run taxa; both are now computed from
the final points (details in the dataset README).

| Page | File | Changed since 21 Oct? | What to expect |
|---|---|---|---|
| 1 | sheet01_allSpeciesOccurrences.xlsx | Yes | 113,396 records: 26 V. tiliifolia records (CA, FL) removed. |
| 2 | sheet02_modelVariables.xlsx | No | |
| 3 | sheet03_modelData.xlsx | Yes | V. tiliifolia presence rows from the new model (61,548 rows in all). |
| 4 | sheet04_variableSelections.xlsx | Yes | V. tiliifolia rows. |
| 5 | sheet05_modelEvaluations.xlsx | Yes | V. tiliifolia rows; every model still passes the robustness rule (`SDM_testConditions.csv`). |
| 6 | sheet06_gapAnalysisSummary.xlsx | Yes, one row | V. tiliifolia: `sourceRun` is now run10222026_1k; counts 4,181 / 2,615; FCSex 8.45, FCSin 53.61, FCSc mean 31.03; categories unchanged. This remains the page the corrigendum table is read from. |
| 7 | sheet07_exsituCollectionSummary.xlsx | No | 3,198 G records as before. |
| 8 | sheet08_pointsInProtectedAreas.xlsx | Yes | 2,879 areas, 14,766 points, from the final point files of every taxon (21 Oct: 2,877 / 14,681 with pre-final points for 11 taxa). |
| 9 | sheet09_predictedInProtectedAreas.xlsx | Yes, 3 areas | Maximum predicted richness per protected area. |
| 10 | sheet10_optimalProtectedAreas.xlsx | Yes, replaced | Greedy selection re-run on the corrected points; 16 areas per seed as before, 6 of the 21 Oct areas recur. Read it as a new table. |
| 11 | sheet11_multipleBufferSizes.xlsx | No | |

Helper files in the folder: `gapAnalysisSummary-sources.csv` (status and source run per taxon),
`SDM_testConditions.csv` (robustness rule per model). The page-by-page description of the 21 Oct package
(`README_SI_update_20261021.md`) still applies to the content of each page.
