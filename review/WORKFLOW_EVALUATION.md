# cwr_wildgrapes — workflow evaluation

Branch: `workflow-evaluation` · Reviewed at `67cf6fa` · 2026-09-06

Scope: (1) the GBIF download/parse path and the taxonomic error in it, (2) logical flaws
across preprocessing and modeling, (3) alignment of the gap-analysis scores with the
`GapAnalysis` R package (source pulled from the CRAN archive, v1.0.2, and compared
function by function).

Findings are ordered by how much they can move a published number.

---

## 1. GBIF taxonomy — the confirmed functional error

> **Superseded 2026-09-14.** This section was written from live backbone lookups
> (`review/verify_gbif_backbone.R`), not from the occurrence download. Several
> claims are wrong or out of date: the file has no `taxonomicStatus` /
> `acceptedScientificName` columns; the "12 of 40" figure is not what happened to
> records in the file; *novogranatensis* and *rubriflora* have no GBIF records at
> all; the corrected parser in commit `15e72a1` does recover *lincecumii* and the
> hyphenated hybrids. The corrected, download-based account with per-taxon counts
> is `review/GBIF_backbone_coauthor_brief.md` (section 8 there lists each change).
> Sections 2 to 6 of this document stand.

### 1.1 What GBIF actually returns

In a GBIF interpreted occurrence download the taxonomy columns describe **two different
taxa** whenever the record's identification is a backbone synonym:

| column | describes |
|---|---|
| `verbatimScientificName` | what the collector/publisher recorded — **never read anywhere in this repo** |
| `scientificName` | the *matched* backbone name (may be the synonym, may be silently replaced) |
| `taxonRank` | the rank of the **matched** name |
| `taxonomicStatus` | `ACCEPTED` / `SYNONYM` / `DOUBTFUL` — **never read anywhere in this repo** |
| `species` | the species-rank name of the **accepted** taxon |
| `infraspecificEpithet` | epithet of the **accepted** taxon |

Live example (GBIF API, `occurrence/search?scientificName=Vitis berlandieri`):

```
scientificName         : Vitis berlandieri Planch.
taxonomicStatus        : SYNONYM
taxonRank              : SPECIES          <- rank of the MATCHED name
acceptedScientificName : Vitis cinerea var. helleri (L.H.Bailey) M.O.Moore
species                : Vitis cinerea    <- from the ACCEPTED taxon
```

`preprocessing/functions/process_gbif.R:47-55` builds the working taxon as:

```r
taxon = case_when(
  taxonRank == "SPECIES"    ~ species,
  taxonRank == "GENUS"      ~ genus,
  taxonRank == "VARIETY"    ~ paste0(species, " var. ",   infraspecificEpithet),
  taxonRank == "SUBSPECIES" ~ paste0(species, " subsp. ", infraspecificEpithet),
  TRUE ~ species)
```

It switches on the rank of the *matched* name and then reads fields off the *accepted*
name. For any synonym those are different taxa, so the composed string is not the
recorded identification — and for the `VARIETY`/`SUBSPECIES` branches it is a
**paste of an accepted-taxon species with an epithet that may belong to another
parent**, i.e. a name combination that need not exist.

### 1.2 Measured blast radius

I re-ran the old rule against the GBIF backbone (`species/match`) for all 40 taxa in
`work2026/gbif_taxonomic_impact_summary.csv`. **12 of 40 project taxa cannot survive it**:

| project taxon | GBIF status | rank | `species` field | old rule produces |
|---|---|---|---|---|
| Vitis baileyana | SYNONYM | SPECIES | Vitis cinerea | **Vitis cinerea** |
| Vitis berlandieri | SYNONYM | SPECIES | Vitis cinerea | **Vitis cinerea** |
| Vitis lincecumii | SYNONYM | SPECIES | Vitis aestivalis | **Vitis aestivalis** |
| Vitis simpsonii | SYNONYM | SPECIES | Vitis cinerea | **Vitis cinerea** |
| Vitis munsoniana | SYNONYM | SPECIES | Vitis rotundifolia | **Vitis rotundifolia** |
| Vitis novogranatensis | SYNONYM | SPECIES | Cissus novogranatensis | **Cissus novogranatensis** |
| Vitis rubriflora | SYNONYM | SPECIES | Parthenocissus semicordata | **Parthenocissus semicordata** |
| Vitis martineziana | — | GENUS | NULL | **Vitis** |
| Vitis rufotomentosa | — | GENUS | NULL | **Vitis** |
| Vitis x champinii | ACCEPTED | SPECIES | Vitis champinii | Vitis champinii (× lost) |
| Vitis x doaniana | ACCEPTED | SPECIES | Vitis doaniana | Vitis doaniana (× lost) |
| Vitis x novae-angliae | ACCEPTED | SPECIES | Vitis novae-angliae | Vitis novae-angliae (× lost) |

This explains the manual patches that have accumulated around it: the special-case
re-injection of `novogranatensis` in both preprocessing drivers, the "martineziana
present / rubriflora - 4" tracking comments through `preprocessingUpdates2025_07.R`, and
`Vitis munsoniana` showing 131 raw → 131 `lumped_lost` in the impact summary.

Two taxa (`martineziana`, `rufotomentosa`) collapse to the bare string `"Vitis"`, which
then flows into `speciesCheck()` and matches nothing.

### 1.3 The 082026 parser fixes some of this, but not the mechanism

`process_gbif_082026.R` parses `originalTaxon` (= `scientificName`). That recovers the
cases where GBIF *retained* the synonym string (berlandieri, baileyana, the GENUS-rank
pair), but it cannot recover the cases where GBIF **replaced** the name:

- `Vitis lincecumii` records come back with `scientificName = "Vitis aestivalis Michx."` → still lumped.
- `Vitis rubriflora` → `"Parthenocissus semicordata ..."` → no regex match → falls through to `species` → still lost.
- `Vitis novogranatensis` → `"Cissus novogranatensis ..."` → still lost.

Only `verbatimScientificName` carries the recorded identification. Nothing in the repo
reads it.

Two independent bugs in the new regex (verified against the literal pattern):

```
"Vitis novae-angliae Fernald"        -> "Vitis novae"        # [a-z]+ stops at the hyphen
"Vitis aestivalis var. Bicolor Deam" -> "Vitis aestivalis"   # capitalised epithet, variety silently dropped
"Vitis riparia Michx. x Vitis rupestris Scheele" -> "Vitis riparia"
```

`Vitis x novae-angliae` is in the project taxon list, so this one is live.

### 1.4 The diagnostic script over- and under-states the impact

`work2026/taxonomic_impact_analysis.R:73` strips authorship with a hardcoded list of ten
author abbreviations (`Small|Buckley|Munson|House|Planch.|Michx.|L.|Simpson…`). Every
other author survives, so `clean_raw_name` stays unmatchable and `raw_accepted` becomes
`NA`. That is why the summary reports `Vitis californica` with `original_raw_count = 0`
and `gained_absorbed = 3173`, and `Vitis rupestris` with 6 raw vs 2254 gained. Those are
artefacts of the author regex, not of GBIF lumping. Treat the numbers in
`gbif_taxonomic_impact_summary.csv` as unusable until the name cleaning is replaced with
a real parser (`rgbif::name_parse()` / `taxize`, or GBIF's own `canonicalName`).

The `countryCode != "JP"` filter in the same script also silently drops any record with
a missing country code alongside Japan.

### 1.5 Recommended parse

```r
d2 <- d1 |> mutate(
  # recorded identification, canonicalised — authority-free, hybrid marker preserved
  recordedName = rgbif::name_parse(verbatimScientificName)$canonicalnamewithmarker,
  # what the backbone did with it, kept for audit
  backboneName = scientificName,
  backboneStatus = taxonomicStatus,
  taxon = coalesce(recordedName, scientificName)
)
```

Then resolve `taxon` against `New World Vitis.csv` (your concept table) — not against the
GBIF backbone. Keep `backboneName`/`backboneStatus` in the output so any future
disagreement is auditable rather than invisible. The three fields you need
(`verbatimScientificName`, `taxonomicStatus`, `acceptedScientificName`) are already in
the download; no new GBIF pull is required.

Also add `Vitis rotundifolia var. munsoniana` to the `Vitis munsoniana` synonym list —
that is the name GBIF interprets those records to, and it is currently absent from both
the `munsoniana` and `rotundifolia` concepts, so those records fall out entirely.

---

## 2. Preprocessing — logical flaws

### 2.1 Cross-dataset de-duplication never reaches the model data (both drivers)

`preprocessingUpdates2026_08_20.R:158-169`:

```r
df2_a <- uniqueTaxon |> map(.f = removeDups, data = df2) |> bind_rows()
df2_a <- bind_rows(df2_a, novogranatensis)
...
d3 <- checksOnLatLong(df2)     # <- df2, not df2_a
```

`df2_a` is computed and then never used again. Everything downstream derives from `df2`.
Same in `preprocessingUpdates2025_07.R` (there `df2_a` is at least written to
`allEvaluated_data_removedDups_072025.csv`, but `model_data*.csv` still comes from
`df2`). So `removeDups()` — the GBIF↔Midwest-Herbaria, GRIN↔Davis, Genesys↔WIEWS overlap
logic — has never affected a published dataset. The later `duplicated(t2)` full-row check
only catches records that are identical in *every* column, which cross-source duplicates
are not.

### 2.2 Records that fail the bounding-box QC are re-added to the model data

`checksOnLatLong()` returns `countycheck = bind_rows(export2, export3)` where `export2` is
`validLatLon == FALSE` (real coordinates outside the Americas) and `export3` is
`is.na(validLatLon)` (no coordinates). The driver then does:

```r
d3_g <- d3$countycheck |> ...        # comment says "grab the G records with no lat lon"
d6  <- valLatLon |> bind_rows(d3_g)
```

so the bounding-box exclusions come straight back in, with their coordinates intact, and
the `validLatLon` flag is dropped a few lines later at `t2`. The file
`excludedOnLatLonBoundingBox.csv` is written but has no effect. This is almost certainly
the source of the ad-hoc patches: the two hardcoded coordinate removals
(`latitude == 82.233333`, `longitude == -177.2805`), the `Vitis shuttleworthii`
`longitude != -80.001483` filter in `run_all05082026.R`, and the `JP` filter in the
diagnostic script. Fix the bind and those patches become unnecessary.

Related: `export3` is *all* records without coordinates, not just `type == "G"`, despite
the comment. H records with no coordinates re-enter as well.

Also, `validLat` is now unconditionally `TRUE` for any non-`NA` latitude (the `>= 14`
test is commented out), so the only live geographic test is `longitude <= -30`. A record
at 60°N/-40°W passes.

### 2.3 `speciesCheck()` injects phantom NA rows

`preprocessing/functions/speciesStandardization.R:19,28`:

```r
df2 <- data[data$taxon == taxon, ]
df3 <- data[data$taxon == j, ]
```

Base-`R` `[` with a logical index containing `NA` returns a row of all-`NA` values, once
per `NA` in `data$taxon`. `taxon` *is* `NA` for GBIF records the backbone did not match
(the `TRUE ~ species` fallback), so every taxon subset gets one all-`NA` row per
unmatched record. They are removed later by `filter(!is.na(taxon))` at `d8`, but they
inflate the intermediate `df2` and `uniqueTaxon`, and they mean `removeDups()` runs over
padded frames. Use `dplyr::filter(taxon == !!taxon)` or `which(...)`.

Matching is also exact-string against a `str_to_sentence()`-normalised `taxon`, so any
whitespace or case drift in `New World Vitis.csv` silently drops a whole concept with no
warning. A post-condition check (`setdiff(synonymList$taxon, unique(includedData$taxon))`)
would catch that.

### 2.4 The exclusion list and the model-species flag are loaded but never applied

Both drivers select `"Names to exclude from this concept"` and
`modelSpecies = "Include in gap analysis?"` into `vitis2`, and `speciesCheck()` uses
neither. Names you have explicitly curated *out* of a concept are not being excluded.

### 2.5 `type` assignment over-counts germplasm and drops records

`process_gbif.R:59-62`: `sampleCategory != "LIVING_SPECIMEN" ~ "H"` classifies
`HUMAN_OBSERVATION`, `MACHINE_OBSERVATION`, `MATERIAL_SAMPLE` and `OCCURRENCE` as
herbarium records, and every `LIVING_SPECIMEN` as `"G"`. GBIF `LIVING_SPECIMEN` is
predominantly botanic-garden living collections, which are not genebank accessions.
Since `SRSex = NG/NH × 100`, this pushes ex-situ scores up for exactly the taxa that are
popular in gardens. Worth deciding explicitly and documenting.

Separately, `removeDuplicates()` (`data_processing_functions.R:64-72`) keeps only
`type == "G"` and `type == "H"` — any record with `type` `NA` is silently discarded from
the spatial object while still counting in `generateCounts()$totalRecords`. That makes
`NTOTAL` in the SRS tables inconsistent with what was actually modelled.

`filter(sampleCategory != "FOSSIL_SPECIMEN")` also drops rows where `basisOfRecord` is
`NA`; `!(sampleCategory %in% "FOSSIL_SPECIMEN")` is the safe form.

### 2.6 `applyFNA()` truncates the non-US range

`R2/dataProcessing/fnaFilter.R:23-27` drops any record with `is.na(state)`, and the
`naStates` layer only covers USA/CAN/MEX — so Central American, Caribbean and South
American records (relevant for *tiliifolia*, *bourgaeana*, *popenoei*, *biformis*)
are dropped before the FNA test.

Then for a species present in the FNA table:

```r
nonNA    <- filter(!iso3 %in% c("USA","MEX","CAN"))   # kept unconditionally
pointsNA <- filter(state %in% states_to_filter)        # FNA state list
bindData <- bind_rows(pointsNA, nonNA)
```

Flora of North America does not cover Mexico, so `states_to_filter` contains no Mexican
states — and Mexican records are excluded from `nonNA` by construction. **Every Mexican
record of an FNA-listed species is dropped.** For a US+Mexico *Vitis* assessment that is
a large systematic loss. Please verify against `FNA_stateClassification.csv` and, if
confirmed, restrict the FNA state test to `iso3 == "USA" | iso3 == "CAN"`.

---

## 3. Modeling

### 3.1 Spatial thinning is computed and then discarded

`R2/modeling/generateModelData.R:19-38`:

```r
if (nrow(sh) > 50) {
  thin1   <- spThin::thin(..., thin.par = 5, reps = 5, ...)[[1]] |> row.names()
  shThin  <- sh[as.numeric(thin1), ]
  sp1     <- bind_rows(sg, shThin)     # thinned result
} else {
  sp1     <- speciesPoints
}

sp1 <- speciesPoints |> mutate("presence" = 1) |> dplyr::select(presence, type)   # <- overwrites
```

The 5 km thinning is run (at full cost, 5 reps) and then unconditionally overwritten by
the unthinned points. **No model in this workflow has been fit to thinned occurrences.**
For taxa like *V. riparia* (20k records) and *V. rotundifolia* (28k records) that is a
large sampling-bias effect on the response curves, on AUC, and — through
`threshold_train` — on the binary threshold that every gap-analysis score is computed
from. This is the single highest-impact bug in the modeling half.

### 3.2 Background sample size scales with range area

`numberBackground()` returns `min(area_km², 10000)`. A taxon with a 3,000 km² native area
gets 3,000 background points; a widespread one gets 10,000. Because maxnet's logistic
output depends on the presence:background ratio, the same species-environment
relationship yields differently-scaled suitability surfaces for narrow vs wide taxa, and
those feed thresholds and therefore GRS/ERS. The in-code comment ("10 times the number of
presence") describes a rule that was never implemented. Fix to a constant (10,000 is the
maxent convention) or to a density-based rule, and record which was used.

### 3.3 `output_rasters/` is shared across species

`rasterResults()` writes `mean.tif` / `median.tif` / `stdev.tif` into a single
`out_dir = "output_rasters"` for every species, and returns disk-backed `SpatRaster`
handles to those files. Sequentially it happens to work because each is consumed before
the next species runs — but it makes the workflow unsafe under `furrr` (which `global.R`
loads), and any cached `.RDS` holding those handles points at whatever species ran last.
Write into `allPaths$results`.

### 3.4 Random k-fold on spatially autocorrelated data

`modelr::crossv_kfold` splits at random, which inflates test AUC for clustered
occurrences. The `cAUC` null-geographic-model correction is present and does mitigate
this, which is good — but the summary tables and `gatherAUCMetrics.R` should lead with
`cAUC`, not `AUCtest`. Spatial-block CV (`blockCV`) is the stronger fix.

### 3.5 Correlation pruning is order-dependent and partial

`variableSelection.R:70-95` only tests the top 5 VSURF-ranked predictors for |r| > 0.7.
Variables ranked 6+ can remain mutually correlated at any level, and a variable already
flagged for removal still contributes to later comparisons. Also:

- `bio_no_na[, c(2:26)]` and `absence[, 2:27]` in `run_all05082026.R` are hardcoded
  column indices — they break silently if the bioclim stack changes.
- `test2 <- complete.cases(varOnly)` is computed and never used.
- `varSelect[, c(inputPredictors)]` relies on VSURF's indices lining up positionally with
  a differently-constructed frame. It happens to hold; index by name instead.

### 3.6 `generateThresholdModel()` edge case

`classify(rcl = matrix(c(0, thr, 0, thr, 1, 1)), right = TRUE)` uses half-open intervals
`(0, thr]` and `(thr, 1]`, so a cell whose median suitability is exactly `0` falls in no
interval and becomes `NA` rather than `0`. Add a row starting below zero, or use
`ifel(median > thr, 1, 0)`.

---

## 4. Gap-analysis scores vs. the `GapAnalysis` package

Compared against `GapAnalysis` 1.0.2 (CRAN archive) function by function.

### 4.1 Where you already agree — keep it

| metric | verdict |
|---|---|
| `srs_exsitu` | Verbatim port of `SRSex()`, including its dead first `if`. Identical results. |
| `srs_insitu` | Matches `SRSin()`, **including** the package's deliberate choice of `totalNum <- dim(inSDM)[1]` (points inside the SDM, not all points — the alternative is commented out in the package source). Your restriction of the numerator via `p1 <- protectedArea * mask1` is algebraically equivalent to the package's `extract(Pro_areas1, inSDM)`. |
| `grs_exsitu` | Matches `GRSex()`. `cropG_Buffer()` supplies the buffer∩SDM mask that the package builds inline. Your exact `sum(cellSize())` is **more accurate** than the package's `length(cells) × median(cellArea)` approximation. |
| `grs_insitu` | Same formula as `GRSin()`. |
| `fcs_insitu` | **More correct than the package.** `GapAnalysis::FCSin()` has a real typo at `rowMeans(FCSin_df[, c("SRSin","GRSin","GRSin")])` — it averages GRSin twice and never uses ERSin. Do **not** "align" to that. |
| `generateCounts` | Also more correct: the package's `OccurrenceCounts()` computes `hasLatLong <- hasLat == hasLong`, which is `TRUE` when *both* are `FALSE`. Yours uses `hasLat & hasLong`. |

### 4.2 Where you diverge and it changes numbers

**(a) ERS counts polygon parts, not ecoregions — the discrepancy your compare script is chasing.**

`nat_area_shp()` returns `ecoregions[ecoregions$ECO_ID_U %in% ids, ]` **undissolved**;
the TNC layer holds multiple features per `ECO_ID_U`. Both `ers_insitu()` and
`ers_exsitu()` then count `nrow()` of the zonal table:

```r
nEco <- v1 |> filter(value > 0) |> nrow()          # polygon features
```

The package counts distinct IDs — `unique(ecoVals$ECO_ID_U)` in both `ERSin()` and
`ERSex()`. So an ecoregion split into *n* polygon parts is counted up to *n* times in
your numerator *and* denominator, and the ratio is biased in whichever direction the
fragmentation is asymmetric. This is a real defect, not just a stylistic difference.
Your `ERS_In_2026_Fixed` block in `compare_ers_run_all.R` is the correct fix — promote it
into `R2/gapAnalysis/ers_insitu.R` and `ers_exsitu.R`:

```r
nEco <- v1 |> filter(value > 0) |> distinct(ECO_ID_U) |> nrow()
```

or dissolve in `nat_area_shp()`: `group_by(ECO_ID_U) |> summarise(.groups = "drop")`.

**(b) `limitByPoints = TRUE` is not the published methodology.**

`compare_ers_run_all.R` calls both `ERSex()` and `ERSin()` with `limitByPoints = TRUE`,
which restricts the ecoregion set to those containing occurrence points. That shrinks the
denominator and mechanically raises ERS relative to both your 2024 functions and the
package (Khoury et al. 2019 defines the denominator as *all* ecoregions the distribution
model occupies). If you adopt it, it is a methodological change that needs stating in the
paper — not a bug fix. As written the comparison table conflates the unique-ID fix with
the `limitByPoints` change, so the two effects can't be separated. Run the 2×2.

**(c) Priority class labels are on different scales.**

| FCS range | this repo | `GapAnalysis` |
|---|---|---|
| < 25 | `UP` | `HP` |
| 25–50 | `HP` | `MP` |
| 50–75 | `MP` | `LP` |
| ≥ 75 | `LP` | `SC` |

Your labels follow Khoury et al. (Urgent/High/Medium/Low Priority); the package's are
shifted one step and top out at "Sufficiently Conserved". A repo `HP` is a package `MP`.
Any side-by-side table of the two must relabel, or it will read as a one-class
disagreement on every taxon.

**(d) `FCSc_mean` halves a one-sided score.**

`fcs_combined.R:16`: `sum(c(FCSex, FCSin), na.rm = TRUE) / 2`. If either is `NA` you get
half the surviving score. The package uses `mean(..., na.rm = TRUE)`. Same pattern in
`fcs_insitu`/`fcs_exsitu`: `sum(c(SRS, GRS, ERS), na.rm = TRUE) / 3` in the `noModel`
branch is deliberate (treating a missing component as 0), which is defensible and more
conservative than the package — but it should be stated, because a reader comparing to
`GapAnalysis` output will not expect it.

**(e) `grs_insitu()` does not align the protected-areas raster.**

`srs_insitu()` does `terra::resample(protectedArea, mask1, method = "near")`;
`grs_insitu()` and `ers_insitu()` only `crop()` and then multiply. The package resamples
in all three whenever `res()` differs. Currently the WDPA raster and the bioclim
template share a 1 km grid so it works, but any resolution change silently produces
misaligned products or a `terra` error. Make it consistent.

**(f) Protected areas: no `STATUS` filter in the raster that is actually loaded.**

`global.R` loads `wdpa_1km_all_.tif`, built by
`preprocessing/functions/produceProtectedAreaRast.R`, which filters only
`MARINE %in% c("0","1")`. The newer `preprocessing/generateWDPARaster.R` correctly adds
`STATUS %in% c("Designated","Inscribed","Established")` — but its output
(`wdpa_1km_final_center.tif`) is not the file being loaded. So current in-situ scores
include *Proposed* and *Not Reported* protected areas, which inflates SRSin/GRSin/ERSin.
Neither script excludes UNESCO-MAB Biosphere Reserves, which the WDPA manual and standard
CWR practice both recommend dropping. Switch `global.R` to the filtered raster and
document the WDPA version.

**(g) In the `< 8 records` branch the G buffer is not masked to the distribution.**

`run_all05082026.R`: the modelled branch uses `cropG_Buffer(g_buffer, thres)`, but the
buffer branch uses `g_bufferCrop <- g_buffer |> terra::mask(natAreaV)` — masked to the
native area, not to `buffer_rs`. GRSex/ERSex for those taxa are computed on a larger
buffer than the denominator assumes, so they are inflated relative to the modelled taxa.

**(h) SRSex and SRSin use different record sets.**

`srs_exsitu(sp_counts = c1)` where `c1 = generateCounts(sd1)` — pre-FNA, pre-dedup, all
records. Every in-situ metric uses `sp1` — post-FNA, post-dedup, coordinates only. The
ex-situ and in-situ halves of `FCSc_mean` are therefore computed over different
denominators for the same taxon.

---

## 5. Reproducibility and correctness of the drivers

These will bite on the next fresh run.

1. **`run_all05082026.R` silently skips the FNA filter.** `sp1` is written with
   `overwrite = TRUE`, then the `applyFNA` call passes `overwrite = overwrite` (`FALSE`).
   `write_GPKG` sees the file it just wrote, returns `read_sf(path)`, and never evaluates
   `applyFNA()`. Every output is named `*_Summary_fnaFilter.html` regardless.

2. **`runVersion <- "run08282025_1k"` with `overwrite <- FALSE`.** Because `write_*()`
   arguments are lazy promises, an existing file means the function body never runs. New
   occurrence data therefore gets scored against **cached 2025 SDMs, thresholds, natural
   areas and buffers**, while `sp1` is force-refreshed. Mixed-vintage results with no
   warning. Add a manifest (input hash + date) per run version, or bump `runVersion`.

3. **Undefined variables.** `dontRun` (used at `run_all05082026.R:52`, defined only in
   `run_all072025.R:229`) and `gPoints` (passed to `fcs_exsitu` in the `< 8` branch,
   defined nowhere). Both error on a clean session.

4. **`R2/primaryWorkflow.R` is stale.** It calls `rasterResults(sdm_result)` — the object
   is `sdm_results` — and calls `fcs_insitu`/`fcs_exsitu` without the required `noModel`
   (and `gPoints`) arguments. It is dead relative to `run_all05082026.R`; either delete it
   or bring it back in sync, because `sourceFiles()` sources everything under `R2/`.

5. **`global.R` cannot run on a clean clone.** It sources `temp/clearNewErrors.R`
   (`/temp` is gitignored) and reads `variableNames_072025.csv` while the repo tracks
   `variableNames.csv`.

6. **`filter(index != d11a$index)`** in both preprocessing drivers assumes the hardcoded
   coordinate filters match exactly one row each. Zero matches → `logical(0)` → dplyr
   error; two matches → recycling. Fixing §2.2 removes the need for these entirely.

7. **`filter(taxon != "NA")`** in `run_all05082026.R:47` tests the *string* `"NA"`, not
   `NA`. Actual `NA` taxa pass through.

8. **`set.seed(1234)` in `global.R` does not cover the parallel paths.** `VSURF(parallel =
   TRUE)` and any `furrr` use need `future.seed` / `RNGkind("L'Ecuyer-CMRG")` for the runs
   to be reproducible. The `while (is.null(sdm_results) && attempt <= 10)` retry loop in
   `runMaxnet()` also re-randomises the folds on each attempt, so a species that needs a
   retry is not reproducible from the seed alone. Log the successful attempt number.

9. **`Vitis cinerea` / `Vitis aestivalis` are re-lumped inside the run loop.** The
   variety-to-species collapse is hardcoded in `run_all05082026.R:66-88` rather than
   living in `New World Vitis.csv`. Since `var. cinerea` and `var. bicolor` are *also*
   modelled as separate taxa, their records contribute to two taxa in the run summary —
   fine if intended, but it needs to be visible in the taxonomy table, not in the runner.

10. **Committed artefacts.** `.RData`, `.Rhistory` and `.Rproj.user/` are tracked;
    `.RData` in particular will silently restore a stale workspace on RStudio open.

---

## 6. Suggested order of work

**Must fix before the next published run**

1. §3.1 — restore spatial thinning (one deleted line; changes every model).
2. §2.2 — stop re-binding bounding-box failures into the model data.
3. §2.1 — point `checksOnLatLong()` at `df2_a`.
4. §5.1 — make the FNA filter actually run.
5. §4.2(a) — count unique `ECO_ID_U` in both ERS functions.
6. §4.2(f) — load the `STATUS`-filtered WDPA raster.

**Taxonomy (the original question)**

7. §1.5 — parse `verbatimScientificName`; carry `taxonomicStatus` through as an audit column.
8. §1.3 — fix the hyphen and capitalisation cases in the 082026 regex, or drop the regex in favour of `rgbif::name_parse()`.
9. §2.4 — apply the exclusion list and `modelSpecies` flag.
10. §2.3 — replace the base-`R` `NA`-unsafe subsetting in `speciesCheck()` and add a "no concept lost" assertion.
11. §1.4 — rebuild the impact analysis on a real name parser before citing its numbers.

**Methodological decisions to make explicit (not bugs, but they need a sentence in the methods)**

12. §4.2(b) — `limitByPoints` on or off; run the 2×2 so its effect is separable from the unique-ID fix.
13. §4.2(c) — relabel before any side-by-side with `GapAnalysis` output.
14. §4.2(d) — the `/3` and `/2` conventions when a component is missing.
15. §2.5 — whether GBIF `LIVING_SPECIMEN` counts as a genebank accession for SRSex.
16. §3.2 — fix the background-point rule.
17. §4.1 — record that you deliberately do *not* reproduce `FCSin()`'s `GRSin`-twice typo.

**Engineering**

18. §3.3 species-specific `out_dir`; §5.2 run manifest; §5.3–5.5 undefined vars, stale
    `primaryWorkflow.R`, clean-clone `global.R`; §5.10 untrack `.RData`/`.Rhistory`.
