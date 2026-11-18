# Supplementary information, 2026 taxonomy update: pages as of 15 Nov 2026

Folder `data/SI_update_20261115/`: one workbook per supplementary page, a single sheet each, ready to drop into
the Google Sheet that holds the SI. It replaces `data/SI_update_20261029/`. The values come from the publication
dataset build of 15 Nov 2026 (`data/datasetsForPublication_20261115/`, README there); this folder only re-cuts it
by page. Built by `work2026/figureData/packagePublicationUpdate.R`.

What is new since the 29 Oct package: two co-author queries led to one re-run each. V. berlandieri lost two
far-outlying points (a cultivated Missouri Botanical Garden specimen and a Baja California collection, 4 records);
its ex situ category returns to HP as published (FCSex 23.0 to 26.2) and its combined score is unchanged (31.0, HP).
V. rufotomentosa gained Maryland in its state range list, so the three Maryland points of the botanist whose
collections prompted the correction now enter the analysis; the SDM then fails the robustness rule and the taxon is
scored by the 50 km buffer method (FCSc mean 19.0 to 22.6; categories unchanged, UP / HP / UP). Everything summed over
all taxa was rebuilt.

| Page | File | Changed since 29 Oct? | What to expect |
|---|---|---|---|
| 1 | sheet01_allSpeciesOccurrences.xlsx | Yes | 113,392 records: 4 V. berlandieri records removed. |
| 2 | sheet02_modelVariables.xlsx | No | |
| 3 | sheet03_modelData.xlsx | Yes | V. berlandieri and V. rufotomentosa presence rows from the new models (61,549 rows in all). |
| 4 | sheet04_variableSelections.xlsx | Yes | Rows of the two taxa. |
| 5 | sheet05_modelEvaluations.xlsx | Yes | Rows of the two taxa; V. rufotomentosa's model fails the robustness rule and is replaced by the buffer method (`SDM_testConditions.csv`). |
| 6 | sheet06_gapAnalysisSummary.xlsx | Yes, two rows | V. berlandieri: `sourceRun` run11132026_1k; 1,367 / 626; FCSex 26.16 (HP), FCSin 35.92, FCSc mean 31.04 (HP). V. rufotomentosa: `sourceRun` run11152026_1k; 42 / 20; FCSex 1.67, FCSin 43.52, FCSc mean 22.59 (UP). This remains the page the corrigendum table is read from. |
| 7 | sheet07_exsituCollectionSummary.xlsx | No | 3,198 G records as before. |
| 8 | sheet08_pointsInProtectedAreas.xlsx | No | 2,879 areas, 14,766 points, as on 29 Oct. |
| 9 | sheet09_predictedInProtectedAreas.xlsx | Yes, 7 areas | Maximum predicted richness per protected area. |
| 10 | sheet10_optimalProtectedAreas.xlsx | Yes, counts only | The same 16 areas as on 29 Oct; their predicted-richness counts are updated. |
| 11 | sheet11_multipleBufferSizes.xlsx | No | |

Helper files in the folder: `gapAnalysisSummary-sources.csv` (status and source run per taxon),
`SDM_testConditions.csv` (robustness rule per model). The page-by-page description of the 21 Oct package
(`README_SI_update_20261021.md`) still applies to the content of each page.
