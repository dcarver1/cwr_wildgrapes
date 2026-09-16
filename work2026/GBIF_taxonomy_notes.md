# GBIF taxonomic backbone vs. project taxonomy: the *Vitis rufotomentosa* problem

Written 2026-09-13. Numbers come from `work2026/gbif_name_source_comparison.R`
run against `data/source_data/vitisGBIFDownload_20250721.csv`
(331,939 non-fossil records). All counts are taken **after** the project synonym
step (`speciesCheck()`) and **before** cross-source de-duplication and the
coordinate checks, so they are directly comparable between methods but are not
final model counts. Final counts require running
`preprocessing/preprocessingUpdates2026_08_20.R`.

## 1. The problem

The published pipeline (`preprocessing/functions/process_gbif.R`) built `taxon`
from GBIF's backbone columns: `species` (the backbone's *accepted* species),
`taxonRank` and `infraspecificEpithet`. Any name that GBIF treats as a synonym
is therefore moved to a different species **before** our own synonym list is
applied, and the recorded name is lost. *Vitis rufotomentosa* Small is a synonym
of *V. aestivalis* in the backbone, so all 32 GBIF records named
*V. rufotomentosa* were assigned to *V. aestivalis* and *V. rufotomentosa*
received none.

`preprocessing/functions/process_gbif_082026.R` fixes this by parsing the taxon
from the name string and using the backbone only as a fallback.

## 2. How many species does it affect, and by how much

Records whose **recorded name** belongs to a project concept but which the
backbone sent somewhere else (scientificName parse as reference):

| Project taxon | Records the backbone took away | Backbone put them in |
|---|---:|---|
| Vitis berlandieri | 740 | V. cinerea |
| Vitis munsoniana | 280 | V. rotundifolia |
| Vitis lincecumii | 173 | V. aestivalis |
| Vitis baileyana | 132 | V. cinerea |
| Vitis simpsonii | 95 | V. cinerea |
| Vitis rufotomentosa | 32 | V. aestivalis |
| Vitis palmata | 11 | V. labrusca (6), V. riparia (5) |
| Vitis aestivalis var. bicolor | 10 | V. aestivalis (species only) |
| Vitis californica | 4 | V. arizonica |
| Vitis vulpina | 8 | V. labrusca (7), dropped (1) |
| Vitis riparia | 3 | dropped |
| Vitis mustangensis | 1 | dropped |

Six species are materially affected (berlandieri, munsoniana, lincecumii,
baileyana, simpsonii, rufotomentosa): 1,452 records. The mirror image is that
*V. cinerea* was inflated by 967 records, *V. rotundifolia* by 280 and
*V. aestivalis* by 205. These are exactly the segregates that the taxonomy sheet
elevates from FNA varieties (see the "Taxonomic notes" column), so the backbone
was systematically undoing the project's taxonomic decisions for the
*cinerea*, *aestivalis* and *rotundifolia* complexes.

A further 35 records go the other way: the backbone was right and name parsing
is wrong, because of **homonyms**: *Vitis labrusca* Thunb. (21, = *V. coignetiae*),
*V. cordifolia* Roth ex Roem. & Schult. (13, = *V. heyneana*) and *V. labrusca*
Scop. (1, = *V. vinifera*). See section 6 for how to drop them.

### Net effect per taxon (GBIF records, pre-dedup)

`backbone` = published pipeline, `combined` = recommended new parser (section 4).
Current GBIF rows in `data/datasetsForPublication/allSpeciesOccurrences.csv`
are given for scale.

| Taxon | backbone | combined | change | currently in model file |
|---|---:|---:|---:|---:|
| Vitis cinerea | 5568 | 4564 | -1004 | 5060 |
| Vitis berlandieri | 693 | 1427 | +734 | 564 |
| Vitis aestivalis | 13078 | 12728 | -350 | 12746 |
| Vitis rotundifolia | 27820 | 27540 | -280 | 27472 |
| Vitis munsoniana | 22 | 302 | +280 | 22 |
| Vitis lincecumii | 188 | 361 | +173 | 165 |
| Vitis tiliifolia | 4124 | 3968 | -156 | 3876 |
| Vitis labrusca | 6021 | 5888 | -133 | 4890 |
| Vitis baileyana | 530 | 662 | +132 | 528 |
| Vitis simpsonii | 912 | 1007 | +95 | 904 |
| Vitis x champinii | 92 | 166 | +74 | 73 |
| Vitis rufotomentosa | 0 | 46 | +46 | 0 (2 records total) |
| Vitis shuttleworthii | 980 | 944 | -36 | 965 |
| Vitis rupestris | 2252 | 2227 | -25 | 1086 |
| Vitis riparia | 19632 | 19609 | -23 | 17123 |
| Vitis x novae-angliae | 280 | 303 | +23 | 275 |
| Vitis popenoei | 220 | 198 | -22 | 219 |
| Vitis x doaniana | 62 | 78 | +16 | 54 |
| Vitis vulpina | 8446 | 8434 | -12 | 7493 |
| Vitis cinerea var. tomentosa | 11 | 0 | -11 | 11 |
| Vitis palmata | 1123 | 1134 | +11 | 1052 |
| Vitis aestivalis var. bicolor | 1404 | 1414 | +10 | 1392 |
| Vitis cinerea var. cinerea | 636 | 628 | -8 | 616 |
| Vitis arizonica | 3991 | 3987 | -4 | 3969 |
| Vitis californica | 3166 | 3170 | +4 | 3139 |
| Vitis acerifolia | 991 | 988 | -3 | 678 |
| Vitis martineziana | 0 | 2 | +2 | 12 (other sources) |

All other modeled taxa are unchanged. Full table with all four methods:
`work2026/gbif_name_source_comparison_by_taxon.csv`.

Modeling consequences: *V. rufotomentosa* moves from a "no spatial records"
species (special summary document) to one with enough records for an SDM;
*V. munsoniana* goes from 22 to roughly 300 records; *V. berlandieri* roughly
doubles; *V. cinerea* loses about a fifth of its records. Every downstream
product (SDMs, buffers, ERS/GRS/SRS/FCS, richness rasters, protected-area
tables, publication sheets) is keyed to a run version, so a new occurrence file
means a full re-run under a new `runVersion`.

Negative changes in the table are of two kinds: (a) segregates moved to their
own concept (*cinerea*, *aestivalis*, *rotundifolia*), and (b) names the
backbone silently treated as synonyms that our include lists do not contain
(section 5). Case (b) is not a parser error: those records are now visible in
`excludedOnTaxonomy_<date>.csv` and can be recovered by editing the sheet.

## 3. Issues found in the 2026-08-20 code and what was changed

| Issue | Records | Fix |
|---|---:|---|
| Regex had no hyphen support: "Vitis × novae-angliae" parsed as "Vitis x novae" and all records were dropped | 280 | `parseVitisName()` allows hyphenated epithets |
| Hybrid formulas ("Vitis riparia x Vitis rupestris") matched on the first binomial and were credited to the first parent | 197 (144 to riparia, 41 to berlandieri, 12 other) | parser returns NA for hybrid formulas; named hybrids ("Vitis x doaniana") are kept |
| *V. vulpina* synonym cell uses semicolons and "V." ("Vitis cordifolia; V. cordifolia var. sempervirens"); `speciesCheck()` split on ", " so nothing matched | 607 (593 of them were in vulpina under the backbone) | `splitNames()` splits on "," or ";" and expands "V. " |
| *V. munsoniana* lists itself as a synonym; `speciesCheck()` appended every record twice | 231 records -> 462 rows | self-synonyms skipped; a record enters a concept once (`!duplicated(index)`) |
| "Names to exclude from this concept" was loaded but never applied | see section 6 | applied inside `speciesCheck()`; removed records returned as `excludedByConcept` and written to `excludedOnTaxonomy_<date>.csv` / `excludedByConcept_<date>.csv` |
| `speciesCheck()` is defined twice (`preprocessing07_2025Functions.R` and `speciesStandardization.R`); the second wins only because it is sourced later | - | the copy in `preprocessing07_2025Functions.R` should be deleted |

The munsoniana duplication never reached the published data because the
backbone renamed those 231 records to *V. rotundifolia* first; the 22 munsoniana
rows in the publication file are distinct. With name parsing the duplication
would have appeared, so the fix matters now.

## 4. `scientificName` vs `verbatimScientificName`

GBIF does **not** rewrite `verbatimScientificName`; it is the publisher's string
untouched. `scientificName` is the backbone name usage that GBIF matched the
record to, with authorship. It is kept at the rank the publisher gave (a synonym
stays a synonym; it is *not* translated to the accepted name, that is what
`species` / `acceptedScientificName` are). Comparison of the two after parsing:

| Relationship | Records | Notes |
|---|---:|---|
| identical | 291,226 | |
| same species, infraspecific rank differs | 15,566 | publisher supplied the epithet in a separate Darwin Core field; GBIF assembled the full name. Verbatim alone loses it: "Vitis cinerea" -> "Vitis cinerea var. floridana" (184, our *V. simpsonii*), "Vitis aestivalis" -> var. aestivalis (238) |
| neither parses as a Vitis binomial | 15,468 | |
| scientificName not parseable, verbatim is | 7,274 | GBIF could not match the verbatim name and fell back to "Vitis L.". Mostly cultivar / "sp." strings, but includes 138 project records: x champinii 74, x novae-angliae 23, x doaniana 16, rufotomentosa 14, aestivalis 6, labrusca 2, martineziana 2, rotundifolia 1 |
| spelling normalised | 1,538 | |
| verbatim not parseable, scientificName is | 838 | |
| matched to a different name | 29 | fuzzy-match errors, almost all Asian taxa |

Full pairs: `work2026/gbif_verbatim_vs_scientificName.csv`.

Conclusion: neither string alone is best. `scientificName` keeps infraspecific
information the publisher supplied in atomised fields; `verbatimScientificName`
keeps names the backbone could not match (hybrids, *rufotomentosa*).
`processGBIF(nameSource = "combined")` parses `scientificName`, then
`verbatimScientificName`, then falls back to the backbone. It is recommended
over the current default (`"scientificName"`); the difference is +138 records
in the taxa listed above and nothing else.

## 5. Names that surface under name parsing but are on no list

None of these appear in any "Names to exclude" cell, so none is a deliberate
removal: they are naming conventions the include lists do not cover. The
backbone used to fold them in silently. Curators should decide each one.

| Name in GBIF | Records | Backbone put them in | Suggested concept |
|---|---:|---|---|
| Vitis riparia subsp. riparia | 417 | dropped (both methods) | V. riparia (autonym; FNA recognises no subspecies) |
| Vitis rotundifolia var. munsoniana | 412 | V. rotundifolia | V. munsoniana (its taxonomic note says the concept *is* var. munsoniana) |
| Vitis caribaea | 202 | V. tiliifolia | V. tiliifolia |
| Vitis x labruscana | 106 | V. labrusca | probably exclude (cultivated labrusca x vinifera) |
| Vitis bicolor | 87 | V. aestivalis | V. aestivalis var. bicolor |
| Vitis coriacea | 36 | V. shuttleworthii | V. shuttleworthii (cell has "Vitis candicans var. coriacea" only) |
| Vitis riparia subsp. longii | 28 | dropped | curator call (V. longii is an acerifolia synonym) |
| Vitis rupestris f. rupestris | 25 | V. rupestris | V. rupestris |
| Muscadinia popenoei | 22 | V. popenoei | V. popenoei |
| Vitis berlandieri var. tomentosa | 11 | V. cinerea var. tomentosa | V. cinerea var. tomentosa |
| Vitis cordifolia var. helleri | 6 | V. berlandieri | V. berlandieri |
| Muscadinia rotundifolia var. munsoniana | 4 | dropped | V. munsoniana |

Full list with counts: `work2026/gbif_unmatched_names.csv`.

## 6. The "Names to exclude" step

Written basis: the "Names to exclude from this concept" and "Taxonomic notes"
columns of `data/New World Vitis.csv` (synced from the Google Sheet by
`preprocessing/grabSheetsFromDrive.R`). There is no other document in the repo.
The column was read by both preprocessing scripts but never used.

It is now applied inside `speciesCheck()`: a record is removed from a concept
when its **original source name** (`originalTaxon`, authorship stripped) is on
that concept's exclude list. A record can also be excluded by listing the full
name **with authorship**, which is the intended way to handle homonyms.

Effect measured on the full compiled dataset (fresh GBIF plus the 2025-07
processed files for the other eight sources, 122,190 records):

* with name parsing: **0 records removed**. Names on an exclude list never land
  in the excluding concept because they are parsed to their own concept first.
* with the old backbone assignment it would have removed **1,105** GBIF records:
  910 from *V. cinerea* (berlandieri 740, baileyana 132, simpsonii 95 ...) and
  195 from *V. aestivalis* (lincecumii 102, linsecomii 71, rufotomentosa 32 ...).

So the exclude lists encode exactly the rufotomentosa-type problem, and would
have caught most of it, but only by dropping the records rather than moving
them to the right concept. Under the new parser the step is a safety net.
Suggested additions to make it do real work:

* *V. labrusca*, exclude: "Vitis labrusca Thunb.", "Vitis labrusca Scop." (22 homonym records)
* *V. vulpina*, exclude: "Vitis cordifolia Roth ex Roem. & Schult." (13 homonym records)

## 7. Sheet edits to request

* *V. vulpina*, include cell: replace the semicolon with a comma and spell out "Vitis" (code now tolerates it, but the sheet should be clean).
* *V. munsoniana*, include cell: remove the self reference "Vitis munsoniana".
* Add the names in section 5 that curators accept.
* Add the homonym exclusions in section 6.

## 8. Pipeline state and next steps

1. `preprocessing/preprocessingUpdates2026_08_20.R` has not been executed; none of its outputs exist. It now writes the exclusion files as well.
2. Switch `nameSource` to `"combined"` in that script once agreed.
3. Run it and diff per-taxon counts against `data/processed_occurrence/model_data20251216.csv`, as the 2025-07 script did (`temp/changeInCounts12_16.csv`).
4. `run_all05082026.R` reads `data/datasetsForPublication/allSpeciesOccurrences.csv` and refers to a `prep_species_data.R` that does not exist. That file has 1,525 more rows than `model_data20251216.csv` and an extra `recordID` column; the step producing it needs to be scripted before the next run is reproducible.
5. Re-run the models under a new `runVersion`.

## 9. Changes made 2026-09-16 on `workflow-evaluation`

Main was frozen at tag `main-frozen-2026-09-16` and merged into this branch.
Then, from `review/WORKFLOW_EVALUATION.md`:

* driver switched to `nameSource = "combined"` (section 4 above)
* driver no longer re-binds records that failed the Americas bounding box; only
  records with no coordinates are added back (evaluation section 2.2)
* hard-coded outlier removals use `!index %in%` so a zero match cannot error
* `speciesCheck()` subsets with `which()`; NA-taxon records no longer inject an
  all-NA row into every concept (evaluation section 2.3)
* fossil filter is `!(sampleCategory %in% "FOSSIL_SPECIMEN")` so a blank
  basisOfRecord is kept (evaluation section 2.5)
* GBIF `LIVING_SPECIMEN` -> type "G" is unchanged and marked as a decision point
  in `process_gbif_082026.R` (evaluation section 2.5)

Still open and deliberately untouched so the co-author diff isolates the
taxonomy fix: `df2_a` (cross-source de-duplication) is still not fed downstream;
the duplicate `speciesCheck()` in `preprocessing07_2025Functions.R`; the
`novogranatensis` re-injection.

## 10. First run of the 2026-08-20 pipeline (2026-09-16)

`preprocessing/preprocessingUpdates2026_08_20.R` ran end to end on the first
attempt (`nameSource = "combined"`, exclusions applied, bounding-box failures no
longer re-bound). Output: `data/processed_occurrence/model_data20260820.csv`.
Comparison against `model_data20251216.csv` by
`work2026/compareCounts_20260916.R` ->
`work2026/changeInCounts_20260916.csv` (per taxon) and
`work2026/changeInCounts_bySource_20260916.csv` (per taxon x source).

Totals: 111,458 -> 111,092 rows (-366); with coordinates 70,695 -> 70,373;
40 -> 39 taxa (*V. cinerea* var. *tomentosa* now has 0 records, see below).

### Final counts, taxa that changed (model-data rows, after all filters)

| Taxon | Dec 2025 | Sep 2026 | change | with coords Dec | Sep | change |
|---|---:|---:|---:|---:|---:|---:|
| Vitis cinerea | 5341 | 4445 | -896 | 2172 | 1772 | -400 |
| Vitis berlandieri | 736 | 1357 | +621 | 359 | 626 | +267 |
| Vitis aestivalis | 12877 | 12494 | -383 | 5618 | 5496 | -122 |
| Vitis rotundifolia | 27551 | 27297 | -254 | 23084 | 22980 | -104 |
| Vitis munsoniana | 22 | 265 | +243 | 18 | 110 | +92 |
| Vitis lincecumii | 168 | 332 | +164 | 71 | 88 | +17 |
| Vitis baileyana | 531 | 653 | +122 | 105 | 152 | +47 |
| Vitis labrusca | 5346 | 5236 | -110 | 2223 | 2147 | -76 |
| Vitis vulpina | 7674 | 7777 | +103 | 2546 | 2476 | -70 |
| Vitis simpsonii | 913 | 1008 | +95 | 392 | 448 | +56 |
| Vitis tiliifolia | 2294 | 2207 | -87 | 1375 | 1352 | -23 |
| Vitis x champinii | 126 | 192 | +66 | 33 | 38 | +5 |
| Vitis riparia | 17551 | 17519 | -32 | 13200 | 13188 | -12 |
| Vitis shuttleworthii | 978 | 937 | -41 | 666 | 661 | -5 |
| Vitis rufotomentosa | 2 | 42 | +40 | 0 | 20 | +20 |
| Vitis x novae-angliae | 277 | 300 | +23 | 81 | 86 | +5 |
| Vitis popenoei | 216 | 194 | -22 | 115 | 112 | -3 |
| Vitis rupestris | 1254 | 1233 | -21 | 260 | 259 | -1 |
| Vitis x doaniana | 106 | 118 | +12 | 50 | 53 | +3 |
| Vitis cinerea var. tomentosa | 11 | 0 | -11 | 11 | 0 | -11 |
| Vitis aestivalis var. bicolor | 1412 | 1421 | +9 | 555 | 557 | +2 |
| Vitis cinerea var. cinerea | 619 | 611 | -8 | 181 | 181 | 0 |
| Vitis arizonica | 4307 | 4303 | -4 | 2762 | 2761 | -1 |
| Vitis palmata | 1097 | 1101 | +4 | 245 | 238 | -7 |
| Vitis acerifolia | 855 | 852 | -3 | 391 | 389 | -2 |
| Vitis californica | 3220 | 3223 | +3 | 2154 | 2154 | 0 |
| Vitis martineziana | 12 | 14 | +2 | 12 | 14 | +2 |
| Vitis aestivalis var. aestivalis | 1569 | 1568 | -1 | 392 | 392 | 0 |

Eleven taxa are unchanged. Every change is in GBIF rows except the three
items below.

### Taxonomy-only run for the co-author table

Because the bounding-box enforcement is a separate correction, the driver has a
switch `enforceBoundingBox` (default TRUE). Run with it FALSE
(`Rscript -e 'enforceBoundingBox <- FALSE; source("preprocessing/preprocessingUpdates2026_08_20.R")'`)
it reproduces the December handling of coordinate failures and skips the WIEWS
sign flip, writing `model_data20260820_taxonomyOnly.csv`. That file differs from
December by the taxonomy changes alone: 111,458 -> 111,298 rows, and the only
non-GBIF movement is the 19 *V. cordifolia* accessions entering *V. vulpina*.
`work2026/compareCounts_20260916.R` run with `taxonomyOnly <- TRUE` writes
`changeInCounts_20260916_taxonomyOnly.csv` and the share copy
`temp/speciesCounts_dec2025_vs_sep2026.csv`; the full-pipeline comparison is
kept as `temp/speciesCounts_dec2025_vs_sep2026_allChanges.csv`.

### Changes not caused by the parser (separate them in the co-author note)

1. **Bounding box now enforced: -206 records.** The December file held 215
   records with longitude > -30 (eastern hemisphere) under a USA/CAN/MEX or
   blank country; the old driver re-bound them after the check flagged them.
   Nine of them (WIEWS *V. riparia* accessions at 46 N / +106.7 E, Montana
   with the minus sign dropped) are now recovered by a documented sign flip in
   the driver (decision 2026-09-16; written to
   `coordinateSignFlipped_<date>.csv`). The remaining 206 are GBIF (198),
   Genesys (1, corrupt values), GRIN (1) and WIEWS (1) at 42.8 N / +46 E which
   no sign flip makes plausible, and Huerta-Acosta (1, Mexico with the sign
   dropped, not flipped because the rule is WIEWS-only for now). Largest:
   *vulpina* 88, *aestivalis* 60, *cinerea* 13, *rotundifolia* 12. This is why
   *vulpina* gains 103 rows overall but loses 70 with coordinates.
2. **Synonym-cell split fix: +19 non-GBIF records.** "V. cordifolia" now
   matches, so Genesys (+17) and WIEWS (+2) accessions enter *V. vulpina*.
3. ***V. cinerea* var. *tomentosa* has 0 records.** All 11 of its December rows
   were recorded as "Vitis berlandieri var. tomentosa Planch." and reached the
   concept only through the backbone. That name is on no include list, so the
   records now sit in `excludedOnTaxonomy_08202026.csv`. Adding the name to the
   concept's include cell (section 5) restores them. Until the sheet decides,
   the taxon cannot be modelled.

### *V. rufotomentosa* in the model data

42 rows: 37 GBIF US (20 with coordinates), 3 GBIF with blank country, 1
Genesys, 1 WIEWS. The 6 Japanese records never reach the model data because
`checksOnLatLong()` keeps only USA/CAN/MEX/blank ISO3, so that curator question
affects the raw counts only. 20 georeferenced records clears the 8-record SDM
threshold.

`excludedByConcept_08202026.csv` is empty, as predicted in section 6.

## Files

* `work2026/compareCounts_20260916.R`: section 10 tables; also writes the share copy `temp/speciesCounts_dec2025_vs_sep2026.csv`
* `work2026/compareSRSex_20260916.R`, `srsEx_comparison_20260916.csv`: SRSex and priority class, Dec 2025 vs Sep 2026 (share copy `temp/srsEx_dec2025_vs_sep2026.csv`)
* `work2026/changeInCounts_20260916.csv`, `changeInCounts_bySource_20260916.csv`: section 10 outputs
* `work2026/gbif_name_source_comparison.R`: sections 2 to 6
* `work2026/gbif_name_source_comparison_by_taxon.csv`: counts per taxon, four methods
* `work2026/gbif_backbone_reassignments.csv`: every (recorded name concept, backbone concept, scientificName) flow
* `work2026/gbif_unmatched_names.csv`: parsed names matching no concept, with backbone destination and list membership
* `work2026/gbif_verbatim_vs_scientificName.csv`: verbatim / interpreted name pairs that differ
* `work2026/exclusion_step_effect.csv`: exclude-step removals on the compiled dataset (empty under name parsing)
* `work2026/gbif_name_source_comparison.log`: console output of the last run
* `work2026/taxonomic_impact_analysis.R` and `gbif_taxonomic_impact_summary.csv` (2026-08-20): earlier diagnostic; its "original_raw_count" relies on stripping a handful of author strings and undercounts several species, so prefer the tables above.
