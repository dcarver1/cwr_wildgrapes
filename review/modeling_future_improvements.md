# Modelling workflow: items deferred from round 2

Written 2026-09-17. Round 2 of the paper reproduces the published method
exactly (driver `run_round2_20260916.R`, validated 2026-09-16/17 against
`run08282025_1k` on nesbittiana, biformis and monticola). The items below
were identified during that work and are deliberately NOT applied in round 2.
Each is a method change that alters results and needs a decision and a
methods-text sentence before it is used. Ordered by how much they can move a
score.

## 1. FNA state filter and the Mexican part of ranges

`R2/dataProcessing/fnaFilter.R` keeps, for a species with an FNA state list,
(a) points whose overlaid state is on the list and (b) points outside USA,
Mexico and Canada. Mexican points fall in neither group unless a Mexican state
is written into the list. The table (`data/source_data/FNA_stateClassification.csv`)
has Mexican states for only two species (arizonica: Chihuahua, Coahuila,
Nuevo Leon, Sinaloa, Sonora; girdiana: Baja California). Every other
cross-border species (monticola, mustangensis, acerifolia, rupestris,
x champinii, x doaniana, riparia ...) loses all Mexican records. Flora of
North America stops at the border, so the list cannot represent Mexican range.

The published monticola SDM (19 Sep 2025) was fit before the Mexico exclusion
entered the code (24 Sep 2025) and includes 17 Chihuahua/Coahuila/Sonora
points; the stored spatial file (17 Dec 2025) has 405 Texas points. Same for
mustangensis (15 points) and martineziana. See
`work2026/publishedRun_modelData_vs_spatial_20260917.csv`.

Considerations before changing it:
* The filter is doing two jobs: removing mis-georeferenced points (its
  stated purpose) and defining the study extent. Separating them would make
  each decision explicit.
* Mexican states could come from the sheet's native-range column or a
  Mexican flora, as arizonica's row already does.
* Restricting the state test to USA/CAN points and letting Mexico fall under
  group (b) is the smallest code change, but it removes the outlier check for
  Mexico entirely.
* Thirteen species have no list and are unfiltered; the Mexican endemics
  are among them, so they are unaffected either way.

## 2. Bounding-box enforcement and longitude sign errors

`checksOnLatLong()` flags eastern-hemisphere records under USA/CAN/MEX or
blank country, but every driver re-bound them into the model data; the FNA
filter's `longitude < 0` test removed them again before the SDM for filtered
species, and nothing removed them for the others. 215 records in the December
data. Nine WIEWS *V. riparia* accessions are Montana with the sign dropped.
The 2026-08-20 driver has `enforceBoundingBox` (default TRUE; round 2 uses
FALSE) and a WIEWS-only sign flip. Decide: enforce; which sources to sign-flip
(Mexico Huerta-Acosta record); whether the latitude test (commented out, so
only longitude is live) should return.

## 3. Cross-source de-duplication never reaches the model data

`removeDups()` output (`df2_a`) is computed and unused in every driver; only a
full-row duplicate check runs. The `novogranatensis` re-injection exists to
work around this step. Evaluation section 2.1.

## 4. Spatial thinning is computed and discarded

`generateModelData()` runs spThin at 5 km and then overwrites the result with
the unthinned points. No published model used thinned occurrences.
Evaluation section 3.1. Largest effect on riparia and rotundifolia.

## 5. Protected-areas raster without a STATUS filter

`global.R` loads `wdpa_1km_all_.tif` (MARINE filter only). The newer
`generateWDPARaster.R` adds STATUS Designated/Inscribed/Established but its
output is not the file loaded. Evaluation section 4.2(f).

## 6. ERS counts polygon parts, not ecoregions

`nat_area_shp()` returns undissolved TNC polygons; both ERS functions count
rows. Evaluation section 4.2(a); the fix is already drafted in
`compare_ers_run_all.R`.

## 7. Variety and subspecies autonyms

Only the forma autonym maps to its species in code. "Vitis riparia subsp.
riparia" (417 raw GBIF records) and similar stay unmatched pending a sheet
decision (notes section 5).

## 8. Second GBIF pull by name

The download is genus-keyed; names the backbone files under other genera
(*V. novogranatensis* -> *Cissus*, 4 records) are absent by construction.

## Reproducibility items already applied in round 2 (not method changes)

* per-species `set.seed(1234)` and `RNGkind("L'Ecuyer-CMRG")`; VSURF on a
  FORK cluster with 8 cores (`varaibleSelection()` defaults) so a rerun
  repeats exactly for a given seed and core count
* FNA step forced (`overwrite = TRUE`) as the published run had it
* `modelDataSummary.csv` written; `RSTUDIO_PANDOC` set for rendering
