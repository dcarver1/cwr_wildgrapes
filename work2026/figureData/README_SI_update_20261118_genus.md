# Supplementary information, 2026 taxonomy update: pages as of 18 Nov 2026 (genus-level summaries)

Folder `data/SI_update_20261118_genus/`: one workbook per supplementary page, a single sheet each, numbered as in the
published SI workbook (12 pages). It replaces the packages of 15 and 18 Nov 2026. The values come from the publication
dataset build `data/datasetsForPublication_20261118_genus/` (README there). Built by
`work2026/figureData/packagePublicationUpdate.R`.

What is new: no taxon was re-run; the per-taxon pages are those of 15 Nov. The all-taxa products now count one layer
per taxon, leaving out V. aestivalis and V. cinerea, which are represented by their varieties (37 taxa; the published
map stacked 38 layers and its maximum of 10 came only from a species counted on top of its own variety). Page 12 is
now the five-size buffer comparison that the published SI holds, rebuilt on the current results; earlier packages
carried an older three-size table in that slot.

| Page | File | Changed since the published SI? | What to expect |
|---|---|---|---|
| 1 | sheet01_allSpeciesOccurrences.xlsx | Yes | 113,392 records (published 112,983). As on 15 Nov. |
| 2 | sheet02_modelVariables.xlsx | No | |
| 3 | sheet03_modelData.xlsx | Yes | Presence rows of the 25 re-fitted models (61,549 rows). As on 15 Nov. |
| 4 | sheet04_variableSelections.xlsx | Yes | Rows of the re-fitted models. As on 15 Nov. |
| 5 | sheet05_modelEvaluations.xlsx | Yes | Rows of the re-fitted models. As on 15 Nov. |
| 6 | sheet06_modelEvaluations2.xlsx | Yes | Robustness rule per model (ATAUC, STAUC, ASD15, Valid): 37 models, one fails (V. rufotomentosa, buffer method). As on 15 Nov. |
| 7 | sheet07_gapAnalysisSummary.xlsx | Yes | The corrected Table 1 values with status and source run per taxon. As on 15 Nov. |
| 8 | sheet08_exsituCollectionSummary.xlsx | Yes | As on 15 Nov. |
| 9 | sheet09_pointsInProtectedAreas.xlsx | Yes | 66,535 WDPA rows; 2,715 with occurrence points (published 2,884), 13,616 points (published 14,669), maximum 6 taxa per polygon (published 7). 172 polygons drop out because their only records were the pooled species' own. |
| 10 | sheet10_predictedInProtectedAreas.xlsx | Yes | 66,535 WDPA rows; maximum predicted richness 9 in 19 polygons (published 10 in 33; the published data also peak at 9 once counted one layer per taxon). 18,601 rows change against the published page (1,390 up, 17,211 down); of these, the one-layer-per-taxon rule alone moves 18,718 rows downward and the taxonomy update alone 9,711 rows in both directions (`sheet10Decomposition_20261118.txt`). |
| 11 | sheet11_optimalProtectedAreas.xlsx | Yes | Greedy selection on the 37-taxon inputs: 16 areas (published 17), 6 in common with the published set. Changes almost entirely with any input change. |
| 12 | sheet12_multipleBufferSizes.xlsx | Yes | Five buffer sizes (1, 5, 20, 50, 100 km) x two projections, 39 taxa, 390 rows (published 38 taxa, 380 rows; V. rufotomentosa added). The 14 unchanged taxa reproduce the published rows exactly; largest FCSc-mean differences V. berlandieri (8.3) and V. x champinii (6.8). |

Helper files: `gapAnalysisSummary-sources.csv` (status and source run per taxon), `pointsInProtectedAreas-sheet8.csv`
(compact occurrence-points-per-area table from the run folder, shipped as page 8 in packages before 18 Nov),
`multipleBufferSizes-sheet11.csv` (the older three-size table shipped as the buffer page before 18 Nov). The compact
7,900-row "predicted richness per area" table of earlier packages is not included: it covers only one of the three
WDPA download parts and is not a published page.
