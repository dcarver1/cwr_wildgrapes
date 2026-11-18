# Datasets for the figures, 2026 taxonomy update: build of 15 Nov 2026

Folder: `data/datasetsForPublication_20261115/` (zip of the same name). It replaces the build of 29 Oct 2026
(`data/datasetsForPublication_20261029/`, README kept beside this one) and keeps the published file names, so the
figure scripts can be pointed at this folder. Built by `work2026/figureData/rebuildAggregateSummaries.R` (the
all-taxa products of the aggregate run), `compileFigureData.R` (this folder) and `packagePublicationUpdate.R`
(the SI workbooks and figure material folders).

## Why a new build

Two co-author queries on the updated results, both answered by a single-taxon re-run:

**V. berlandieri (13 Nov 2026, run11132026_1k).** A co-author asked why the geographic filter had left a point near
St. Louis and one in Baja California. Both were V. cinerea records in the published dataset, where V. cinerea's
US-only state list kept them out of every model; the review moved them to V. berlandieri, which has no state list,
so they reached a model for the first time. The St. Louis record is a 1906 specimen from the Missouri Botanical
Garden itself (cultivated); the Baja California record is a 1953 UC collection that also enters twice without
coordinates. The four records were removed as the second rule of the documented preprocessing step
(`preprocessing/functions/reviewRemovals.R`; `data/processed_occurrence/removedByReview_20261113.csv`), giving the
final occurrence dataset `model_data20261113.csv` (113,392 records; every other taxon is record-for-record identical
to the 22 Oct file).

**V. rufotomentosa (15 Nov 2026, run11152026_1k).** A co-author asked why the map showed no Maryland points when a
Maryland botanist's collections had prompted the correction. The six 2023 Eastern Shore sheets (C. T. Frye; NCU,
TAWES, MARY) are in the dataset under V. rufotomentosa, recovered by the verbatim-name fix, but the state range list
adopted on 19 Oct (Weakley: FL, GA, SC, NC) removed them before modelling. Maryland was added to the list
(`data/source_data/FNA_stateClassification.csv`, source and date recorded) and the taxon re-run on the same records.
With the three Maryland points (17 modelled points) the SDM fails the published robustness rule (STAUC 0.22 against
the 0.15 limit), so the result comes from the 50 km buffer method the methods describe for such cases. It is the only
taxon of the update on the buffer method (`SDM_testConditions.csv`, Valid = FALSE).

| | 29 Oct build | this build |
|---|---|---|
| V. berlandieri records / with coordinates | 1,371 / 628 | 1,367 / 626 |
| V. berlandieri FCSex / FCSin / FCSc mean | 23.02 / 39.15 / 31.08 | 26.16 / 35.92 / 31.04 |
| V. berlandieri categories (ex / in / combined) | UP / HP / HP | HP / HP / HP |
| V. rufotomentosa records / with coordinates | 42 / 20 (14 modelled) | 42 / 20 (17 modelled) |
| V. rufotomentosa FCSex / FCSin / FCSc mean | 1.67 / 36.28 / 18.97 | 1.67 / 43.52 / 22.59 |
| V. rufotomentosa categories (ex / in / combined) | UP / HP / UP | UP / HP / UP |

Against the published table, V. berlandieri's ex situ category is now HP as published (the 29 Oct build had it at UP),
so the component changes reported earlier reduce to the combined changes of V. rotundifolia (UP to HP) and
V. x champinii (MP to HP), plus V. rufotomentosa scored for the first time. The status of the other 38 gap taxa is as
on 29 Oct: 15 unchanged (published values verbatim), 23 updated from the runs of 16 Sept, 2 Oct, 19-20 Oct and 22 Oct.

## What changed against the 29 Oct build

| File | Change |
|---|---|
| allSpeciesOccurrences-sheet1.csv | 113,392 records (4 fewer: the V. berlandieri removals). Source `model_data20261113.csv`. |
| modelData-sheet3.csv, environementalDataForBoxPlots.csv | 61,549 rows (V. berlandieri 515 presence rows, was 517; V. rufotomentosa 17, was 14). |
| variableSelections-sheet4.csv, modelEvaluations-sheet5.csv, SDM_testConditions.csv | Rows of the two taxa from the new models; 36 of 37 models pass the robustness rule, V. rufotomentosa does not (buffer method). |
| gapAnalysisSummary-sheet6.csv, gapAnalysisSummary-sources.csv, summaryTable_runAggregate2026_1k.csv | V. berlandieri row from `run11132026_1k`, V. rufotomentosa row from `run11152026_1k` (values above); `sourceRun` names them. |
| exsituCollectionSummary-sheet7.csv | Unchanged (no germplasm record was removed). |
| pointsInProtectedAreas-sheet8.csv | 2,879 areas, 14,766 points: unchanged (none of the removed berlandieri points lay in a protected area, and the Maryland rufotomentosa points fall in no WDPA cell). |
| predictedInProtectedAreas-sheet9.csv | 7,900 areas; 7 change value. |
| optimalProtectedAreas-sheet10.csv | Greedy selection re-run: the same 16 WDPA areas as on 29 Oct, with their predicted-richness counts updated. |
| multipleBufferSizes-sheet11.csv, modelVaraible-sheet2.csv | Unchanged. |
| speciesrichness_1km.tif | 39 taxa, max 10; 315,447 cells change value (the berlandieri range shrinks, the rufotomentosa buffer is larger than its former predicted range). |
| ga50_speciesrichness_1km.tif | 21 taxa; 11,844 cells change. |
| ersexRichness.tif | 39 taxa; 159,536 cells change. |
| ersinRichness.tif | 39 taxa; 149 cells change. |
| grsEX_map.tif, grsIn_map.tif | 236,745 and 229,715 cells change. Written by the summary document; kept in the aggregate run folder, the published copies in `data/datasetsForPublication/` verified by checksum after the renders. |
| pointsInProtectedAreasUpdate.csv | Identical to 29 Oct. |
| rastInProtectedAreasUpdate.csv | 34,411 polygons with a maximum above 0 (34,375 on 29 Oct); 554 change value. |
| runAggregate2026_1k_Summary.html, runAggregate2026_1k_boxPlotSummary.html | Re-rendered on this build. |
| aggregateSources.csv | V. berlandieri copied from `run11132026_1k` and V. rufotomentosa from `run11152026_1k` on 15 Nov 2026; every other taxon as on 29 Oct. |

## Checks made

- Aggregate folder: only the two taxa were re-copied; both fingerprints name `model_data20261113.csv`.
- Sheet 6 has 40 rows: 15 unchanged, 25 updated (6 + 2 + 14 + 1 + 1 + 1 by source run).
- Sheet 8 total equals the "Update" table total (14,766 points).
- The published gap maps in `data/datasetsForPublication/` match their backup checksums after the renders.

The figure material folder (`data/figureMaterial_20261115/`) holds the all-taxa layers and tables of this package with a copy
of this README; the update zip extracts into one folder, `publicationUpdate_20261115/`, holding it and the SI folder.

For the file-by-file description of the package see `README_datasetsForPublication_20261020.md`; the content of each
file is the same as described there.
