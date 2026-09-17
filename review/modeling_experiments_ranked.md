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
| 14 | **Living specimens as germplasm** (GBIF LIVING_SPECIMEN -> G) | `process_gbif_082026.R` | SRSex up for garden-popular taxa; decision, not a fix | one line | Partner decision (brief section 6) |

Suggested order of experiments: 1, 3 and 5 are one-line, deterministic and
independent, so run them as separate run versions on the four reference
species plus riparia, rotundifolia and biformis; then 2 (behind a switch);
then 4 and 6 after the partner decisions; 7 and 8 as input-file experiments
through the preprocessing driver.
