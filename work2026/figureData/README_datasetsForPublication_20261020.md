# Datasets for the figures, 2026 taxonomy update (built 20 Oct 2026)

Folder: `data/datasetsForPublication_20261020/` (zip of the same name). Every file keeps the name it had in the
published dataset (`data/datasetsForPublication/`, built from run `run08282025_1k`) so the figure scripts can be
pointed at this folder in place of the old one. Built by `work2026/figureData/compileFigureData_20261020.R` and
`work2026/figureData/protectedAreaRichness_20261020.R`.

## What changed underneath

The taxonomic review closed on 19 Oct 2026 and the models were re-run where records changed. Of the 40 gap taxa,
15 are unchanged (published values stand) and 25 are updated; the result of each updated taxon comes from one of
three runs (19-20 Oct 2026, 2 Oct 2026, or the 16 Sept 2026 draft whose records are identical to the final
dataset). `gapAnalysisSummary-sources.csv` and `aggregateSources.csv` say which run each taxon's row and files come
from. All-taxa layers (richness, gap maps, protected-area tables) were rebuilt from the aggregate folder
`data/Vitis/<taxon>/runAggregate2026_1k`, which holds each taxon's files from its source run.

Against the published table: V. rotundifolia moves UP to HP (24.7 to 28.1), V. x champinii MP to HP (52.6 to
46.1), V. berlandieri's ex situ class HP to UP (combined stays HP). V. rufotomentosa now has a modelled result
(42 records, FCSc 19.0, UP) where the published table had none. Details: `work2026/teamStatus/teamStatus_20261020.md`;
methods notes for the corrigendum: `work2026/corrigendumNotes.md`.

## Files

| File | Published counterpart | Built from | Notes |
|---|---|---|---|
| allSpeciesOccurrences-sheet1.csv | sheet 1 | `data/processed_occurrence/model_data20261019_2303.csv` | The final occurrence dataset, every taxon, 113,422 records. Same columns as published except the `recordID` column, which the published file added by hand. |
| modelVaraible-sheet2.csv | sheet 2 | `variableNames_072025.csv` | Unchanged. |
| modelData-sheet3.csv | sheet 3 | `<taxon>/runAggregate2026_1k/occurances/allmodelData.csv` | Presence rows with predictor values; 61,552 rows, 39 taxa (novogranatensis has no model). |
| variableSelections-sheet4.csv | sheet 4 | `occurances/topVariablesData.csv` | 37 taxa; V. jaegeriana left out as in the published sheet (buffer method). |
| modelEvaluations-sheet5.csv | sheet 5 | `results/evaluationTable.csv` | Per-fold AUC, 37 modelled taxa. |
| SDM_testConditions.csv | same | `results/aucMetrics.csv` | Robustness rule (ATAUC, STAUC, ASD15, Valid); every model passes, no buffer fallback. |
| gapAnalysisSummary-sheet6.csv | sheet 6 | `aggregateRuns()` combined table | One row per gap taxon with two extra columns, `status` and `sourceRun`. For the 15 unchanged taxa the published values are used verbatim (two of them, blancoi and girdiana, differ slightly from their run folder because parts of the published folder were re-run after publication). |
| gapAnalysisSummary-sources.csv | new | `aggregateRuns()` | Status and source run per taxon. |
| summaryTable_runAggregate2026_1k.csv | `summaryTable_run08282025_1k.csv` | `summaryTable()` on the aggregate folder | Same table read straight from the run folders, without the published-value override; differs from sheet 6 only for blancoi and girdiana. |
| exsituCollectionSummary-sheet7.csv | sheet 7 | sheet 1, type G | Germplasm records by database source and institution: 3,198 G records (published 3,168). |
| pointsInProtectedAreas-sheet8.csv | sheet 8 | `runAggregate2026_1k/protectedAreaSpeciesPointRichness.csv` | Occurrence points per protected area, 2,877 areas. The published sheet 8 had 734 rows and 4,806 points although its own per-taxon files sum to about the same 14,600 points as here; the original compile script flagged that mismatch and added the "Update" table below. |
| predictedInProtectedAreas-sheet9.csv | sheet 9 | `runAggregate2026_1k/protectedAreaSpeciesRasterRichness_1km.csv` | Max predicted richness per protected area (wdpa_1 set, 7,900 areas); 31 areas change value against the published file. |
| optimalProtectedAreas-sheet10.csv | sheet 10 | `calculate_optimal_protected_areas()` | Greedy minimum set of protected areas covering the predicted taxa (20 seeds, 16 areas selected each). The selection is sensitive to its inputs: 38 rows, only 7 WDPA IDs shared with the published 37, so treat it as a new table rather than an edit of the old one. |
| multipleBufferSizes-sheet11.csv | sheet 11 | published folder | Unchanged (method comparison, not re-run). |
| pointsInProtectedAreasUpdate.csv | same | aggregate points x WDPA vector | Second half of the published compile script, re-run on the aggregate points: every WDPA polygon of the range countries with point count, number of taxa and taxa list (66,535 rows as published; 2,887 areas hold points, 14,766 points, vs 2,884 / 14,669 published). The published script tested each polygon against all points in a loop; here the test is one sparse sf::st_intersects() join, which gives the same per-polygon result in minutes. Rows: the script joins the result back on WDPAID, which is not unique in the layer (58,299 polygons, 57,461 ids), so 838 polygons appear more than once, exactly as in the published table. |
| rastInProtectedAreasUpdate.csv | same | aggregate richness x WDPA vector | Max predicted richness per WDPA polygon, same polygon set and join (64,857 rows with raster cells, identical to published; 34,362 rows with a maximum above 0 vs 34,237 published; maximum 10 in both). |
| protectedAreaSpeciesRichness.csv | run folder file | `protectedAreaRichness_20261020.R` | Max predicted richness for all 66,449 WDPA polygons in the range countries (published run: 63,315). Feeds the protected-areas table of the summary document and the optimal-areas selection. |
| speciesrichness_1km.tif, speciesUsed_speciesrichness.csv | same | sum of `prj_threshold.tif` | Predicted taxon richness, 39 taxa (published 38: rufotomentosa now modelled). Max 10 in both. |
| ga50_speciesrichness_1km.tif, ga50speciesUsed_speciesrichness.csv | same | sum of `ga50_masked.tif` | Richness of taxa inside 50 km of a germplasm accession, 21 taxa (as published). |
| ersexRichness.tif, ersex_speciesUsed_Richness.csv | same | `ers_ex_gaps.tif` | Ex situ ecological gap richness, 39 taxa. |
| ersinRichness.tif, ersin_speciesUsed_Richness.tif | same | `ers_in_gaps_5.tif` | In situ ecological gap richness (5 km aggregate), 39 taxa. The species list is a csv despite the `.tif` name, as published. |
| grsEX_map.tif, grsIn_map.tif | same | written by the run summary document | Geographic ex situ and in situ gap richness maps (richness minus the ga50 / protected-area masked richness), as in the published figures. |
| runAggregate2026_1k_Summary.html | `run08282025_1k_Summary.html` | `runSummary_editsSGCK.Rmd` | The all-taxa summary document: results table, richness and gap maps, protected-areas table. Reference rendering of the figures. |
| runAggregate2026_1k_boxPlotSummary.html | `run08282025_1k_boxPlotSummary.html` | `boxplotSummaries_editsSGCK.Rmd` | Predictor box plots per taxon from environementalDataForBoxPlots.csv. |
| environementalDataForBoxPlots.csv | same | `allmodelData.csv` presence rows | Same content as sheet 3 with the geometry column kept and taxon last, the layout the box plot script reads. |
| aggregateSources.csv | new | `buildAggregateRun()` | Per taxon: status, source run, newest source file, copy time. |

## Checks made

- Every taxon's records in the aggregate folder match its named source run (fingerprint check, 40 of 40).
- Sheet 6 equals the team status table of 20 Oct (same combined table).
- Per-taxon protected-area point files agree with the published run for unchanged taxa to within a few areas
  (the published folder was partly re-run after publication); differences elsewhere follow the record changes.
- Richness map: 39 taxa, maximum 10 overlapping taxa (published 38 and 10); predicted cells with at least one taxon
  7.56 million against 7.89 million published, the net effect of the re-fitted ranges (rotundifolia smaller).
- The two published rasters that the summary document overwrites in place (`grsEX_map.tif`, `grsIn_map.tif`) were
  restored in `data/datasetsForPublication/` after the render; the new versions are only in this folder.
