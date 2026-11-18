# Datasets for the figures, 2026 taxonomy update: build of 18 Nov 2026 (genus-level summaries)

Folder: `data/datasetsForPublication_20261118_genus/` (zip of the same name). It replaces the build of 15 Nov 2026
(`data/datasetsForPublication_20261115/`, README kept beside this one) and keeps the published file names, so the
figure scripts can be pointed at this folder. Built by `work2026/figureData/rebuildAggregateSummaries.R` (the
all-taxa products of the aggregate run), `variableBufferAnalysis_2026.R` (the buffer-size comparison),
`compileFigureData.R` (this folder) and `packagePublicationUpdate.R` (the SI workbooks and figure material folders).

## Why a new build

No taxon was re-run. Every per-taxon result is the one of 15 Nov 2026. Two things changed in how the all-taxa
products are made, both prompted by a co-author's reading of the Correction draft and of the 15 Nov SI package.

**One layer per taxon in the all-taxa products.** V. aestivalis and V. cinerea are assessed as species and also
through their varieties (var. aestivalis, var. bicolor; var. cinerea, var. tomentosa), the species row pooling the
varieties' records. The published richness map, gap-richness maps and protected-area tables stacked the two species
beside their varieties, so any cell where a species and one of its varieties were both predicted was counted twice.
Every product summed over taxa now leaves the two pooled species out (`genusLevelTaxa()` in
`R2/summarize/speciesRichnessMap.R`): 37 taxa, being 36 species distribution models plus the 50 km buffered ranges of
V. jaegeriana, V. rubriflora and V. rufotomentosa. V. novogranatensis has no georeferenced record and is in no map.
The published map had 38 layers (its caption said 36) and a maximum of 10; counted one layer per taxon the published
data also peak at 9. The ERS gap-richness maps turn out to have left the pooled species out already, through a
file-matching accident in the old code; they are unchanged, and their species lists now say what was summed.

**The buffer-size comparison is the five-size table.** The published SI page on buffer sizes is the table of
`published/variableBufferAnalysis.R` (March 2026, written for the reviewer response): scores at 1, 5, 20, 50 and
100 km under WGS84 and an equal-area projection. Packages before this one carried an older three-size table
(10, 50, 100 km). The five-size analysis is rebuilt on the aggregate run by `variableBufferAnalysis_2026.R`; for the
14 unchanged taxa it reproduces the published rows to machine precision, so every difference in it is the taxonomy
update.

## What changed against the 15 Nov build

| File | Change |
|---|---|
| allSpeciesOccurrences-sheet1.csv, modelVaraible-sheet2.csv, modelData-sheet3.csv, variableSelections-sheet4.csv, modelEvaluations-sheet5.csv, SDM_testConditions.csv, gapAnalysisSummary-sheet6.csv, gapAnalysisSummary-sources.csv, exsituCollectionSummary-sheet7.csv, environementalDataForBoxPlots.csv, summaryTable_runAggregate2026_1k.csv, aggregateSources.csv | Identical to 15 Nov (per-taxon results unchanged). |
| variable_buffer_raw_results.csv | New file: the five-size comparison, 39 taxa x 2 projections x 5 sizes = 390 rows (published: 38 taxa, 380 rows; V. rufotomentosa is now scored). Unchanged taxa identical to the published rows; largest FCSc-mean differences V. berlandieri (8.3) and V. x champinii (6.8); 9 of 240 updated-taxon rows change category. |
| multipleBufferSizes-sheet11.csv | The older three-size table, unchanged, kept for reference. |
| speciesUsed_speciesrichness.csv, ersex_speciesUsed_Richness.csv, ersin_speciesUsed_Richness.csv | 37 taxa (were 39: V. aestivalis and V. cinerea removed). |
| ga50speciesUsed_speciesrichness.csv | 19 taxa (were 21). |
| speciesrichness_1km.tif | Maximum 10 to 9; 3,171,382 cells change value (all downward). |
| ga50_speciesrichness_1km.tif | Maximum 5 to 4; 73,618 cells change. |
| ersexRichness.tif, ersinRichness.tif | Unchanged (see above). |
| grsEX_map.tif, grsIn_map.tif | Maximum 10 to 9; 2,986,062 and 2,925,401 cells change. |
| pointsInProtectedAreasUpdate.csv | 2,715 rows with points (were 2,887): 172 polygons held records of the pooled species only. 13,616 points (were 14,766). Maximum taxa per polygon 6 (was 7). |
| rastInProtectedAreasUpdate.csv | Maximum 10 to 9 (19 polygons at 9); 19,201 of 66,535 rows change, all downward. Against the published table: 18,601 change (1,390 up, 17,211 down); see `work2026/figureData/sheet10Decomposition_20261118.txt` for the split between this change and the taxonomy update. |
| pointsInProtectedAreas-sheet8.csv | Compact per-run table, 2,710 areas (were 2,879). |
| predictedInProtectedAreas-sheet9.csv | Compact per-run table, 7,900 areas, 19 change value, maximum 7 as before. This table covers only one of the three WDPA download parts and is not a published page; kept in the build for completeness, no longer shipped in the SI package. |
| optimalProtectedAreas-sheet10.csv | Greedy selection re-run on the 37-taxon inputs: 16 areas, 6 in common with the 15 Nov set. |
| runAggregate2026_1k_Summary.html | Re-rendered; states "combining 37 potential distributions". boxPlotSummary unchanged. |

## Protected-area layer

WDPA polygons coded fully marine (MARINE = 2) are excluded; terrestrial (0) and coastal (1) polygons are kept, in the
rasterized layer behind the in situ scores and in the polygon tables alike (published preprocessing, unchanged).
Coastal polygons are 2,165 of the 58,299 in the layer; 242 of them have a predicted richness above 0.

## Checks made

- Species lists of all four richness maps hold 37 taxa and name neither pooled species; no `points_Vitis aestivalis`
  or `points_Vitis cinerea` file in the protected-area points folder.
- Richness raster maximum 9; the published raster minus the two pooled layers also peaks at 9.
- Five-size buffer table: 140 rows of the 14 unchanged taxa differ from the published rows by at most 7e-15.
- Sheet 1 has 113,392 records, sheet 6 40 rows (15 unchanged, 25 updated), as on 15 Nov.

The figure material folder (`data/figureMaterial_20261118_genus/`) holds the all-taxa layers and tables of this package with
a copy of this README; the update zip extracts into one folder, `publicationUpdate_20261118_genus/`, holding it and
the SI folder. For the file-by-file description of the package see `README_datasetsForPublication_20261020.md`.
