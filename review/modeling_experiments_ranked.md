# Candidate modelling changes, ranked for the round-2 experiments

Written 2026-09-17 from: `review/WORKFLOW_EVALUATION.md` (sections 2 to 5),
`review/modeling_future_improvements.md`, `work2026/variableBufferAnalysis.R`
and sheet 11 of the publication (10 / 50 / 100 km buffers, WGS84 and
equal-area), `compare_ers_run_all.R` and the `ERSex.R` / `ERSin.R` variants,
`R2/modeling/numberBackground.R`, `generateModelData.R`, `fnaFilter.R`, the
2025-2026 commit history, and the round-2 test runs. Ranked by expected
effect on the published scores, then by cost. Each experiment runs in the
experiment worktree under its own runVersion and is compared to
run09162026_1k with `work2026/compareRunVersions_20260916.R` on the four
reference species (rufotomentosa, x doaniana, x champinii, popenoei), then on
the large species where an effect is expected.

| Rank | Change | What it touches | Expected effect | Cost | Notes |
|---|---|---|---|---|---|
| 1 | **Restore spatial thinning** (5 km, H points > 50) | `generateModelData.R`: thinned set is computed then overwritten | SDM, threshold, every in situ score and GRS/ERS ex situ. Largest for riparia, rotundifolia, aestivalis, vulpina (thousands of clustered herbarium points); nil for the four small reference species | one line | Already suggested. Also removes a random draw that currently affects nothing |
| 2 | **Act on the model-validity flag**: fall to the 50 km buffer method when ATAUC < 0.7, STAUC >= 0.15 or ASD15 > 10% | driver, after `calc_sdm_metrics()` | Only species that fail; rufotomentosa is the first. Makes the code do what the methods text says | small | Partner decision pending (item 1b); build it behind a switch |
| 3 | **ERS counts distinct ecoregions, not polygon parts** | `ers_insitu.R`, `ers_exsitu.R`, or dissolve in `nat_area_shp()` | ERS ex/in for every species whose natural area has multi-part ecoregions; direction depends on fragmentation. `compare_ers_run_all.R` already has the fixed version and the numbers | small, drafted | Separate from `limitByPoints`; run the 2x2 so the two effects are not conflated |
| 4 | **FNA filter: Mexican states / restrict test to USA-CAN** | `fnaFilter.R` + table | Natural area, SDM and all scores for cross-border species: monticola (17 pts), mustangensis (15), acerifolia, rupestris, x champinii, x doaniana, riparia. Nil for Mexican endemics and eastern species | small code, curation of state lists | Partner decision; the arizonica row shows the intended form |
| 5 | **Fixed background sample (10,000)** instead of min(area km2, 10,000) | `numberBackground.R` | Suitability scaling and threshold for every narrow-range species (< 10,000 km2 natural area): biformis, nesbittiana, blancoi, jaegeriana, bloodworthiana, peninsularis, x doaniana, rufotomentosa... | one line | The in-code comment ("10 x presences") describes a rule never implemented; choose one and state it |
| 6 | **Protected areas: STATUS-filtered WDPA raster** (Designated / Inscribed / Established; drop MAB) | `global.R` loads `wdpa_1km_all_.tif`; `generateWDPARaster.R` output exists | SRSin, GRSin, ERSin down for every species with proposed / not-reported areas in range | swap one path | Layer already generated (2026-07-13); check its extent matches the template |
| 7 | **Cross-source de-duplication actually applied** (`df2_a` fed downstream) | preprocessing driver | Counts, SRSex denominators, and points for species with GBIF / Midwest Herbaria / GRIN / Genesys overlap | small | Also retires the novogranatensis re-injection. Changes the input file, so it is a preprocessing experiment, not a run-version experiment |
| 8 | **Bounding-box enforcement + sign-flip rule** (the `enforceBoundingBox` switch) | preprocessing driver | 206 records, mostly vulpina / aestivalis eastern-hemisphere coordinates; FNA already removes them before modelling for filtered species, so scores move only for unfiltered ones | switch exists | Round 2 runs with it off; input-file experiment |
| 9 | **Buffer distance sensitivity** (10 / 50 / 100 km, equal-area projection) | `variableBufferAnalysis.R`, sheet 11 | GRS ex/in and ERS ex for every species; already reported to reviewers at three distances | exists | A reporting choice rather than a fix; rerun on round-2 outputs for consistency |
| 10 | **Spatial-block cross-validation** (`blockCV`) and leading with cAUC | `runMaxnet.R` (random k-fold) | AUC and validity flags, thresholds; the cAUC correction already mitigates | medium | Method change that needs methods text; try after 1 and 2 |
| 11 | **Correlation pruning over all selected predictors** (not only the top 5) and index by name | `variableSelection.R` | Predictor sets for most species; effect on scores usually small | medium | Variable selection currently keeps 24-25 of 25, so the practical change is which one or two drop |
| 12 | **Threshold edge case** (median exactly 0 becomes NA) | `generateThresholdModel.R` | Cells at the edge of the projection; tiny | one line | Fold into 3 |
| 13 | **Consistent record sets for SRSex and the in situ metrics** (pre- vs post-FNA / dedup) | driver | Ex situ vs in situ halves of FCSc computed over different denominators | small | Definition decision; state it in methods |
| 14 | **Confirmed, not a change: GBIF LIVING_SPECIMEN records are germplasm (G)** | `process_gbif_082026.R`, unchanged since the published run | none | none | The only open question is whether to narrow it (e.g. exclude display / arboretum publishers); the current rule stands unless the partners want that (2026-09-17) |

Suggested order of experiments: 1, 3 and 5 are one-line, deterministic and
independent, so run them as separate run versions on the four reference
species plus riparia, rotundifolia and biformis; then 2 (behind a switch);
then 4 and 6 after the partner decisions; 7 and 8 as input-file experiments
through the preprocessing driver.


## Status after the 2026-09-17 review with the lead author

Classification: **M** = modelling method (changes the SDM or which model is
accepted); **G** = gap-analysis score computation; **P** = preprocessing /
input records (affects counts and points before either).

| Rank | Class | Decision |
|---|---|---|
| 1 thinning | M | Running as `run09172026_exp_thin` in the experiments worktree (rufotomentosa, x doaniana, x champinii, popenoei, monticola) |
| 2 validity -> buffer | M | Built into the driver behind `bufferOnInvalidModel` (default FALSE); not enabled |
| 3 ERS distinct ecoregions | G | Confirmed as a defect: both functions count zonal-table rows = polygon parts; to be fixed and tested (see "How ERS counts" below) |
| 4 FNA Mexico | P/M | Future enhancement, not for the paper edits |
| 5 background points | M | Kept: the rule is min(natural-area km2, 10,000); every published species has a natural area above 9,400 km2, so all get 9,400-10,000 points and the rule is effectively constant. The lead author's intent (do not over-sample small ranges) stands; nothing to change |
| 6 WDPA STATUS | G | Moot for the study area: the raster in use (`wdpa_1.gpkg` -> `wdpa_1km_all_.tif`, 89,259 polygons after the marine filter) has 611 Proposed / 8 Adopted / 3 Not Reported polygons worldwide and **none in USA, CAN or MEX**. North America holds only Designated (6,177), Inscribed (20) and Established (1). The only status-type question left is the 50 Biosphere Reserves in North America (MAB), which some CWR studies exclude |
| 7 cross-source dedup | P | To test through the preprocessing driver |
| 8 bounding box | P | Preprocessing stage (record filtering before any modelling). Off for the paper edits; candidate for future preprocessing |
| 9 buffer distances | G | Not regenerated |
| 10 spatial-block CV | M | New method; not for the paper edits |
| 11 correlation pruning | M | To test |
| 12 threshold edge case | M | On the methods-testing list (see "Threshold edge case" below) |
| 13 record sets ex vs in situ | G | Explanation below; definition decision |
| 14 living specimens = G | G | Existing behaviour; no change |

### How ERS counts ecoregions (item 3)

`nat_area_shp()` selects, from the TNC ecoregion layer, every polygon whose
`ECO_ID_U` is intersected by a species point, **without dissolving**; the TNC
layer stores several polygons per ecoregion ID (islands, disjunct parts).
`ers_insitu()` and `ers_exsitu()` then run `terra::zonal(thres, natArea,
"sum")`, which returns **one row per polygon**, attach `ECO_ID_U` to each row,
filter `value > 0`, and take `nrow()`. So an ecoregion split into n parts that
all intersect the model counts n times in the denominator, and n times in the
numerator only if every part also holds a protected cell (in situ) or a G
buffer cell (ex situ). Fix: `dplyr::distinct(ECO_ID_U)` before `nrow()` in
both functions (or dissolve in `nat_area_shp()`); `compare_ers_run_all.R`
already carries the fixed in situ version and its numbers. `nat_area_shp()`
itself is not wrong for the natural area (the union of parts is the same); the
error is only in the counting.

### Threshold edge case (item 12)

`generateThresholdModel()` classifies the median prediction with
`classify(rcl = matrix(c(0, thr, 0, thr, 1, 1)), right = TRUE)`, i.e. the
intervals (0, thr] -> 0 and (thr, 1] -> 1. A cell whose median is exactly 0
falls in neither interval and becomes NA instead of 0. Such cells are rare
(all folds must predict exactly zero) and NA is treated like "outside the
model" almost everywhere, so the effect is at the margin of the natural area.
Fix: `ifel(median > thr, 1, 0)`.

### Record sets for the ex situ and in situ halves (item 13)

SRSex is computed from `generateCounts(sd1)`: every record of the taxon in
the model data, with or without coordinates, before the FNA filter and before
duplicate-location removal. Every in situ metric (and GRS/ERS ex situ) is
computed from `sp1`: georeferenced, FNA-filtered, one point per location.
Example: monticola's 17 Mexican records count in NH for SRSex but are removed
from the in situ set; x champinii's 136 herbarium records include 98 without
coordinates that only SRSex sees. This is consistent with the published
definition ("SRSex uses all compiled records irrespective of coordinates"),
so it is a definition to state, not a defect, unless the partners want the
same filtered set used throughout.
