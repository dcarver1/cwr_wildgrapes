# Geographic filter and reproducibility: evaluation of 16 Nov 2026

Answers to the project lead's questions after Colin's review of the updated results (13 Nov 2026). Nothing here changes
a method or a result; it records what was checked, how, and what is still a team decision. Companion files in this
folder: `natureServeCheck_20261116.R` and its outputs `natureServe_outside_byTaxon_20261116.csv` (one row per gap
taxon) and `natureServe_outside_points_20261116.csv` (every flagged record with its source ID).

## 1. Why V. cinerea var. tomentosa differs from the published value with identical records

- The published driver (`published/run_all072025.R`, August 2025) has its `set.seed` line commented out and runs VSURF
  on a default socket cluster, whose workers are never seeded. The published variable selection was therefore a one-off
  draw that no re-fit can repeat.
- The reproducibility fixes landed on 16-17 Sept 2026 (`run_round2_20260916.R`: L'Ecuyer-CMRG streams, FORK cluster
  pinned to 8 cores, `set.seed(1234)` at the top of every taxon). Every result in the update comes from that driver.
- Tomentosa had zero records in the 16 Sept taxonomy-only input (the verbatim-name fix came later), so 19 Oct
  (`run10192026_1k`) is its only seeded run. Against the published fit the 24 predictors rank differently and the kept
  set differs (bio_19, bio_14 and bio_15 are in now and were not before). Points inside the thresholded range fall from
  5 to 3 and the one protected point drops out: SRSin 20 to 0. The 11 records and the protected-area layer are
  identical.
- Verified 16 Nov 2026: a scratch re-run of tomentosa on `model_data20261019.csv` matched `run10192026_1k` byte for
  byte in every csv and tif (variable importance, AUC metrics, threshold raster, all gap tables); the RDS and
  GeoPackage files differ only in container metadata and compare as equal in R. Across the Sept and Oct runs the six
  taxa whose modelled points did not change (cinerea, cinerea var. cinerea, labrusca, riparia, rotundifolia, vulpina)
  gave identical variable selections, AUC metrics and SRSin. Lincecumii and baileyana matched between 16 Sept and
  2 Oct and changed only on 19 Oct when their state lists were added. The scratch run was deleted.
- Sentence for the methods: the published models were fitted without a fixed random seed, so a re-fit of a taxon with
  unchanged input gives slightly different variable selections and surfaces; the update fixes seeds and worker random
  streams, and its results repeat exactly.

## 2. V. berlandieri: why the state filter never applied

- The filter (`R2/dataProcessing/fnaFilter.R`) keeps a point only if its state is on the taxon's list; US, Mexican and
  Canadian points are treated alike, and the FNA lists do carry Mexican states where FNA gives them (arizonica:
  Chihuahua, Coahuila, Nuevo León, Sinaloa, Sonora; girdiana: Baja California). "FNA has no Mexican information" is
  not the reason.
- FNA has no row for V. berlandieri because it treats the taxon as V. cinerea var. helleri. The published list gave
  berlandieri an NA entry, the filter was skipped, and the update kept that (19 Oct reasoning: a US-only list would
  drop the Mexican range). In the published run this was harmless because most Mexican berlandieri-type material was
  labelled V. cinerea and went through cinerea's US-only list. The review moved those records to berlandieri, the one
  taxon with no screen: modelled Mexican points 13 (published) to 130 (now).
- Pre-established ranges for the concept: NatureServe on the sheet ("NM, TX, MEXICO", no Mexican states; check-only
  on 19 Oct, flagged the Missouri and Oklahoma points) and FNA for V. cinerea var. helleri ("Texas; Mexico: Coahuila,
  Nuevo León, Tamaulipas"; canyons, limestone slopes, floodplains, 100-700 m; FNA itself notes the variety may be
  better recognised at species rank as V. berlandieri). The sheet's "Relationship to FNA" column records berlandieri
  as equal to var. helleri, so the FNA list is for the same concept.
- Effect of the FNA var. helleri list on the current model (`run11132026_1k`, 515 points): 440 inside (Texas 383,
  Tamaulipas 25, Nuevo León 18, Coahuila 14), 75 outside (Jalisco 29, Veracruz 14, San Luis Potosí 7, Michoacán 5,
  Durango 3, Puebla 3, Querétaro 3, Chihuahua 2, Colima 2, Hidalgo 2, Oklahoma 2, one each Chiapas, Nayarit,
  Zacatecas). Both hand-removed points of 13 Nov fall outside it.
- Sentence for the Correction if no list is adopted: Vitis berlandieri has no range list in Flora of North America,
  which treats it as V. cinerea var. helleri, so the state filter was not applied to it in the published analysis or
  here; under the corrected taxonomy this admitted records formerly held under V. cinerea, including two far-outlying
  specimens (a cultivated 1906 Missouri Botanical Garden sheet and a 1953 Baja California collection) that were
  removed by hand.

## 3. Gap taxa with no state filter (17 of 40)

| Group | Taxa | Range source held | Exposure |
|---|---|---|---|
| Outside FNA's scope (13) | biformis, blancoi, bloodworthiana, bourgaeana, jaegeriana, martineziana, nesbittiana, novogranatensis, peninsularis, popenoei, rubriflora, tiliifolia, cinerea var. tomentosa | NatureServe country line only | All points in the named countries. One tiliifolia record carries iso3 HRV with coordinates in Costa Rica (Puntarenas): a country-code error, not a stray point. Unfiltered in the published run too. |
| Varieties with an FNA list only at species level (3) | aestivalis var. aestivalis, aestivalis var. bicolor, cinerea var. cinerea | FNA gives each variety its own range; not transcribed | Against the species list: 1 DC point each for the aestivalis varieties; 2 Coahuila points (PRC 2005, 2008) for cinerea var. cinerea. Unfiltered in the published run too. |
| Binational taxon with an FNA list for the exact concept (1) | berlandieri | see section 2 | 75 points |

Code quirk found on the way: the state list writes "D.C" and the state layer writes "District of Columbia", so DC
points are dropped from every filtered taxon. Small fix; not applied.

## 4. Is NatureServe used as a hard filter anywhere?

No code reads NatureServe. The only hard filter is the state list file. In the published run every row came from FNA.
On 19 Oct five rows were added for taxa FNA does not treat as species; two of them come from the NatureServe
Distribution column of the sheet and are therefore NatureServe ranges applied as hard filters: V. simpsonii (AL, AR,
FL, GA, MS, NC, SC, VA) and V. baileyana (AL, AR, GA, IN, KY, NC, OH, PA, SC, TN, VA, WV). Both rows note that a
Weakley check was requested from the reviewer on 19 Oct; it has not come back. The other three rows are Weakley
(rufotomentosa, lincecumii) and FNA at varietal rank (munsoniana).

## 5. NatureServe applied as a hard filter to every current model point

Script: `natureServeCheck_20261116.R` (parses the sheet's Distribution column: US/Canadian state codes, whole
countries, Mexican states in parentheses; "Central America ... Caribbean" read as every country south of Mexico).
Result: 11 taxa with points outside, 1,175 points, of which 1,089 are V. tiliifolia's Mexican points, outside only
because the sheet's wording ("Central America to South America ... and in the Caribbean") never names Mexico. The
remaining 86 points:

| Taxon | FNA filter today | Points | Outside NatureServe | Where |
|---|---|---|---|---|
| V. cinerea | yes | 1,266 | 33 | NC 17, VA 11, PA 5 |
| V. aestivalis var. bicolor | no | 467 | 19 | NC 10, IA 3, one each AR, DC, KS, MO, OK, TX |
| V. x novae-angliae | yes | 69 | 12 | PA |
| V. riparia | yes | 11,853 | 7 | WA |
| V. aestivalis | yes | 4,824 | 3 | NE |
| V. aestivalis var. aestivalis | no | 339 | 3 | WI 2, ME 1 |
| V. labrusca | yes | 1,755 | 3 | WI |
| V. rufotomentosa | yes | 17 | 3 | MD (added deliberately on 15 Nov) |
| V. berlandieri | no | 515 | 2 | OK |
| V. martineziana | no | 13 | 1 | Jalisco, against "Mexico (Chiapas)" |

Reading: for the filtered taxa these are states FNA lists and NatureServe does not (a source disagreement, not stray
points). For berlandieri NatureServe changes almost nothing: it lists Mexico whole, so the Baja California point would
have passed and the 75 central-Mexican points pass; only Oklahoma and the already-removed Missouri specimen fall
outside. Cinerea var. cinerea and var. tomentosa have no NatureServe range on the sheet and were not tested.

## 6. The Weakley reference behind the V. rufotomentosa list

Weakley, A.S. and Southeastern Flora Team. 2024. Flora of the Southeastern United States. University of North Carolina
Herbarium, North Carolina Botanical Garden, Chapel Hill (citation form used by the Maryland Natural Heritage Program,
2026). Web edition at fsus.ncbg.unc.edu: "2026 Edition, version 126.131 (November 04, 2026)"; taxon page 66810, Rusty
Grape. Entry: endemic; maritime thickets and maritime forests; "Florida to se. North Carolina, and perhaps more
widespread". The reviewer quoted that line in the 19 Oct email. Corroboration: NatureServe FL (SNR), NC (SNR); GRIN
lists Delaware and Maryland under the aestivalis synonymy (1967, 1991 sources); the Maryland rare plant list of March
2026 (C. Frye, ed.) has no rufotomentosa or aestivalis entry, only V. rupestris; no 2023-2025 publication reports
Maryland as a new state record. The Maryland addition therefore rests on "perhaps more widespread" plus Frye's 2023
sheets. The full citation is now in the source field of `data/source_data/FNA_stateClassification.csv`. The flora
site refuses automated fetches; the entry wording came from the search index and should be read in a browser before
it is quoted in the Correction.

## 7. Decisions this leaves with the team

1. Tomentosa and acerifolia: let the published values stand (records unchanged; the difference is seed noise)?
2. Berlandieri: adopt the FNA var. helleri list (TX, Coahuila, Nuevo León, Tamaulipas; drops 75 points; one re-run
   and one rebuild), wait for Jun or Anne on the central-Mexican states, or keep no list with the sentence above?
3. Simpsonii and baileyana: confirm the NatureServe lists against Weakley (asked 19 Oct).
4. Optional: transcribe FNA's varietal ranges for the aestivalis varieties and cinerea var. cinerea (one to two points
   each today); fix the "D.C" label.
