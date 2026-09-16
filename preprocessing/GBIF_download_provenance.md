# GBIF occurrence downloads used in this project

Written 2026-09-16 from inspection of the files. No script in this repository
performs a GBIF download; both files were requested through the GBIF website
and stored on the project Google Drive. The download keys and DOIs were not
recorded at the time. **Action item:** recover them from the GBIF user account
(gbif.org > user > downloads) and add them below; the DOI is the required
citation for the manuscript and is the only way to re-request an identical
file.

Both files are GBIF "simple" (CSV/TSV) format: 50 columns, tab-separated,
`.csv` extension. The simple format does **not** include `taxonomicStatus` or
`acceptedScientificName`; the backbone-vs-recorded name relationship is visible
only through `scientificName` vs `species` and the `issue` column. Both files
have `genus == "Vitis"` on every row, so the query was keyed to the genus in the
GBIF backbone: names the backbone files under another genus (e.g. *Vitis
novogranatensis* Moldenke -> *Cissus*) are absent by construction.

| | `gbif.csv` | `vitisGBIFDownload_20250721.csv` |
|---|---|---|
| Local path | `data/source_data/gbif.csv` | `data/source_data/vitisGBIFDownload_20250721.csv` |
| Used by | published run (`process_gbif.R`, `compileInputDatasets.R`) | 2025-07 and 2026-08-20 preprocessing |
| File date | 2023-05-23 | 2025-07-21 |
| `lastInterpreted` range | 2023-01-24 to 2023-03-10 | 2025-07-04 to 2025-07-21 |
| Rows | 286,084 | 332,914 |
| Rows with coordinates | 170,289 | 206,836 |
| Distinct datasets | 1,189 | 1,706 |
| Fossil specimens (removed by parser) | (not counted) | 975 |
| Google Drive name | (unknown) | `vitisGBIFDownload_20250721.csv` |
| Pulled to disk by | manual | `pullGBIFFromDrive()` in `preprocessing/functions/preprocessing07_2025Functions.R` |
| Download key / DOI | **unknown** | **unknown** |

Notes on the 2025 file: 1,699 rows are `occurrenceStatus == ABSENT` and are
not filtered anywhere in the pipeline; top countries are US (99,974), PT
(64,163), FR (37,416), blank (24,528). The pipeline keeps only USA, CAN, MEX and
blank ISO3 in `checksOnLatLong()`.

## Re-requesting

Until the keys are recovered, the closest reproducible request is an
`rgbif::occ_download()` on the *Vitis* genus backbone key with
`format = "SIMPLE_CSV"` and no other predicates, then filtering fossils in the
parser as now. Doing it by genus key reproduces the known gap for names filed
under other genera; a name-based request (`scientificName` predicate per
project name and synonym) would close that gap but is a methodological change
and needs a co-author decision (plan phase 2).
