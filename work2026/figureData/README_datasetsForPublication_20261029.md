# Datasets for the figures, 2026 taxonomy update: build of 29 Oct 2026

Folder: `data/datasetsForPublication_20261029/` (zip of the same name). It replaces the build of 20 Oct 2026
(`data/datasetsForPublication_20261020/`, README kept beside this one) and keeps the published file names, so the
figure scripts can be pointed at this folder. Built by `work2026/figureData/rebuildAggregateSummaries.R` (the
all-taxa products of the aggregate run), `compileFigureData.R` (this folder) and `packagePublicationUpdate.R`
(the SI workbooks and figure material folders).

## Why a new build

On 22 Oct 2026 the reviewer (Anne Frances) asked for the Vitis tiliifolia records in California and Florida to be
removed: the taxon does not occur in the contiguous US. The 26 records were removed as a documented preprocessing
step (`preprocessing/functions/reviewRemovals.R`; removed rows in
`data/processed_occurrence/removedByReview_20261022.csv`), giving the final occurrence dataset
`model_data20261022.csv` (113,396 records; every other taxon is record-for-record identical to the 19 Oct file).
V. tiliifolia was re-run on 22 Oct as `run10222026_1k` (model valid, no buffer fallback). Its result:

| | 20 Oct build (run10192026_1k) | this build (run10222026_1k) |
|---|---|---|
| Records / with coordinates | 4,207 / 2,622 | 4,181 / 2,615 |
| FCSex / FCSin / FCSc mean | 8.32 / 52.97 / 30.65 | 8.45 / 53.61 / 31.03 |
| Categories (ex / in / combined) | UP / MP / HP | UP / MP / HP |

The modelled area lost its northern and western margin (the natural area no longer reaches California or Florida;
extent now 110.3 W to 41.8 W, 6.1 S to 34.2 N against 122.9 W and 40.7 N before). Predicted cells changed by
0.4 percent, ex situ ecological gap cells by 1.4 percent, and the re-fitted surface differs cell by cell, so every
all-taxa layer and table was rebuilt rather than patched.

The status of the other 39 gap taxa is as on 20 Oct: 15 unchanged (published values verbatim), 24 updated from the
runs of 16 Sept, 2 Oct and 19-20 Oct 2026. Against the published table nothing new changes category; the category
changes reported on 20 Oct stand (V. rotundifolia UP to HP, V. x champinii MP to HP, V. berlandieri ex situ HP to UP,
V. rufotomentosa modelled for the first time).

## A correction to the 20 Oct build: sheets 8 and 10

The per-taxon "points in protected areas" files that feed sheet 8 (points per protected area) and sheet 10 (optimal
protected areas) are kept in the run folder (`proPoints/`) and the function that makes them keeps any file already
there. The aggregate folder had inherited files dated 5 Oct 2026, before the final runs, so in the 20 Oct build
those two sheets used pre-final points for 11 of the re-run taxa (largest effect: V. berlandieri, 20 points in
protected areas then, 90 now; V. cinerea 262 then, 236 now). The folder is now emptied before the files are
rebuilt (`rebuildAggregateSummaries.R`), and this build's sheets 8 and 10 are computed from the final points of every
taxon. Sheet 8 now agrees with the independent "Update" table (14,766 points in both). The other sheets and layers of
the 20 Oct build were not affected.

## What changed against the 20 Oct build

| File | Change |
|---|---|
| allSpeciesOccurrences-sheet1.csv | 113,396 records (26 fewer: the V. tiliifolia removals). Source `model_data20261022.csv`. |
| modelData-sheet3.csv, environementalDataForBoxPlots.csv | 61,548 rows (V. tiliifolia 1,724 presence rows, was 1,728). |
| variableSelections-sheet4.csv, modelEvaluations-sheet5.csv, SDM_testConditions.csv | V. tiliifolia rows from the new model; all 37 models still pass the robustness rule. |
| gapAnalysisSummary-sheet6.csv, gapAnalysisSummary-sources.csv, summaryTable_runAggregate2026_1k.csv | V. tiliifolia row from `run10222026_1k` (values above); `sourceRun` names it. |
| exsituCollectionSummary-sheet7.csv | Unchanged (3,198 G records; no germplasm record was removed). |
| pointsInProtectedAreas-sheet8.csv | 2,879 areas, 14,766 points (20 Oct: 2,877 and 14,681, see the correction above). |
| predictedInProtectedAreas-sheet9.csv | 7,900 areas; 3 change value. |
| optimalProtectedAreas-sheet10.csv | Greedy selection re-run on the corrected points: 16 areas per seed as before, 38 rows; 6 WDPA IDs in common with the 20 Oct list. As before, read it as a new table, not an edit. |
| multipleBufferSizes-sheet11.csv, modelVaraible-sheet2.csv | Unchanged. |
| speciesrichness_1km.tif | 39 taxa, max 10; 7.553 million predicted cells (7.556 million on 20 Oct); 113,076 cells change value. |
| ga50_speciesrichness_1km.tif | 21 taxa; 1,434 cells change. |
| ersexRichness.tif | 39 taxa; 84,328 cells change. |
| ersinRichness.tif | Identical to 20 Oct (the 5 km aggregate of the in situ gaps did not move). |
| ersin_speciesUsed_Richness.csv | The in situ ERS species list (39 taxa). Earlier packages, and the published dataset, carried this csv under the name `ersin_speciesUsed_Richness.tif`; it is delivered under its right name from this build on and the misnamed copy is dropped. |
| grsEX_map.tif, grsIn_map.tif | 13,385 and 12,268 cells change. Written by the summary document; now kept in the aggregate run folder so the published copies in `data/datasetsForPublication/` are never left overwritten (verified by checksum after this build). |
| pointsInProtectedAreasUpdate.csv | Identical to 20 Oct: none of the removed V. tiliifolia points lay in a protected area. |
| rastInProtectedAreasUpdate.csv | 34,375 polygons with a maximum above 0 (34,362 on 20 Oct). |
| runAggregate2026_1k_Summary.html, runAggregate2026_1k_boxPlotSummary.html | Re-rendered on this build. |
| aggregateSources.csv | V. tiliifolia copied from `run10222026_1k` on 29 Oct 2026; every other taxon as on 20 Oct. |

## Checks made

- Aggregate folder: only V. tiliifolia was re-copied; its fingerprint names `model_data20261022.csv`. The V. tiliifolia
  run holds no US record; the four US records left in the dataset (Illinois, Louisiana, two without a state) have no
  coordinates and are with the reviewer.
- Sheet 6 has 40 rows: 15 unchanged, 25 updated (6 + 2 + 16 + 1 by source run).
- Sheet 8 total equals the "Update" table total (14,766 points).
- The published gap maps in `data/datasetsForPublication/` match their backup checksums after the renders.

The figure material folder (`data/figureMaterial_20261029/`) holds the all-taxa layers and tables of this package with a copy
of this README; the update zip extracts into one folder, `publicationUpdate_20261029/`, holding it and the SI folder.

For the file-by-file description of the package see `README_datasetsForPublication_20261020.md`; the content of each
file is the same as described there.
