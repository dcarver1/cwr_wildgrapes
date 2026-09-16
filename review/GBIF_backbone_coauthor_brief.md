# GBIF backbone vs. project taxonomy: brief for the co-author discussion

Revised 2026-09-14. Base of knowledge is the table shared with the project
partner, `temp/gbif_backbone_records_taken_away_by_taxon.csv`, which was cut
from `work2026/gbif_name_source_comparison.R` (commit `15e72a1` on `main`)
run against `data/source_data/vitisGBIFDownload_20250721.csv` (331,939
non-fossil records). Every figure below was re-checked against that raw file.
Counts are taken after the project synonym step and before de-duplication
and coordinate checks, so they are comparable to each other but are not final
model counts.

This replaces the earlier brief drafted from the `workflow-evaluation` branch.
Section 9 lists what changed and why; section 8 has the results of the first run.

## 1. One-sentence version

GBIF re-labels every record to its own taxonomic backbone, and our parser read
the species name from that re-labelled version instead of from the name the
collector recorded, so any record whose recorded name GBIF treats as a synonym
was silently moved to GBIF's accepted species before our own synonym list ever
saw it.

## 2. The mechanism, plainly

- Every GBIF download carries two names per record: `verbatimScientificName`
  (what the publisher wrote) and `scientificName` (the backbone name GBIF
  matched it to, with authorship). Alongside those sit the backbone's
  *accepted* fields: `species`, `taxonRank`, `infraspecificEpithet`.
- The published parser (`preprocessing/functions/process_gbif.R`) switched on
  `taxonRank` and built the taxon from `species`. `species` is the backbone's
  accepted species, so a synonym match is rewritten to the accepted name.
- Nothing in the published pipeline read `verbatimScientificName`.
- The download is GBIF's "simple" format (50 columns). It does **not** contain
  `taxonomicStatus` or `acceptedScientificName`. The synonym relationship is
  still visible in the file: `scientificName` and `species` name different
  taxa, and the `issue` column carries `TAXON_MATCH_HIGHERRANK` or
  `TAXON_MATCH_FUZZY` when the match was weak.
- The download was keyed to the genus *Vitis*: all 332,914 rows have
  `genus == "Vitis"`. Anything the backbone files under another genus was
  never downloaded (section 4).

## 3. *Vitis rufotomentosa* as the illustration

The July 2025 file holds 46 records whose recorded name is *Vitis
rufotomentosa*, and the backbone handled the same string two different ways:

| Recorded name | GBIF matched it to | Backbone `species` | Records | What our parser produced |
|---|---|---|---:|---|
| Vitis rufotomentosa / V. rufotomentosa Small | Vitis rufotomentosa Small (SPECIES, synonym) | Vitis aestivalis | 32 | *V. aestivalis* |
| Vitis rufotomentosa | Vitis L. (GENUS, no match) | blank | 14 | the bare string "Vitis", matched nothing |

So 32 records were moved to *V. aestivalis* and 14 were dropped. Either way
*V. rufotomentosa* received zero GBIF records and became a "no spatial
records" species, with conservation scores hand-coded in
`R2/summarize/compileConservationData.R` and
`R2/summarize/summaryDocForNoRecords.Rmd` rather than modelled. The current
model file (`model_data20251216.csv`) has 2 *rufotomentosa* rows, both
germplasm entries without coordinates (Genesys, WIEWS).

**Reconciling 26, 32 and 46.** All three are correct under different
definitions and should be stated that way in the meeting:

| Figure | Definition |
|---:|---|
| 32 | records GBIF matched to the synonym *V. rufotomentosa* Small (the shared CSV) |
| 26 | the same 32 minus 6 whose `countryCode` is JP (the older impact script's Japan filter) |
| 46 | the 32 plus 14 that GBIF could not match at all; recoverable only from `verbatimScientificName` |

Geography and coordinates of the 46:

| Country | With coordinates | Without |
|---|---:|---:|
| US | 20 | 17 |
| JP | 0 | 6 |
| blank | 0 | 3 |

The 6 Japanese records are named *V. rufotomentosa* Small but almost certainly
belong to *V. flexuosa* var. *rufotomentosa* Makino, which has 145 records of
its own in the file under its full name and never collides with our concept.
Only these 6 need a curator decision; a country filter is not needed for the
rest.

A further 15 records are recorded as *Vitis aestivalis* subsp.
*rufotomentosa* (Small) D.J.Rogers (14 with no country, 1 US). GBIF rewrites
that string to "Vitis aestivalis Michx.", and because the recommended parser
reads `scientificName` before `verbatimScientificName`, they stay in
*V. aestivalis*. They move to *rufotomentosa* only if that name is added to
the concept's include list, which is currently empty.

Modelling: `run_all072025.R` attempts an SDM at 8 or more records. With 20
georeferenced US records before de-duplication, *rufotomentosa* should clear
that bar, so it likely moves from the summary-only document to a modelled
species.

## 4. Scope beyond *rufotomentosa*

The earlier brief's "12 of 40 taxa" came from asking today's backbone what it
thinks of each of the 40 project names. That is a different question from
"which records in our file were moved", and it gave two wrong answers
(*novogranatensis*, *rubriflora*) and one misleading one (the hybrids). The
table below is what the download shows.

### 4.1 Records moved to another project concept (the real damage)

| Project taxon | Records taken | Backbone put them in |
|---|---:|---|
| Vitis berlandieri | 740 | V. cinerea |
| Vitis munsoniana | 280 | V. rotundifolia |
| Vitis lincecumii | 173 | V. aestivalis |
| Vitis baileyana | 132 | V. cinerea |
| Vitis simpsonii | 95 | V. cinerea |
| Vitis rufotomentosa | 32 (+14 dropped) | V. aestivalis |
| Vitis palmata | 11 | V. labrusca (6), V. riparia (5) |
| Vitis aestivalis var. bicolor | 10 | V. aestivalis |
| Vitis californica | 4 | V. arizonica |

Six species are materially affected: 1,452 records. The recipients were
inflated by the same amount: *V. cinerea* +967, *V. rotundifolia* +280,
*V. aestivalis* +205. These are exactly the segregates the project elevates
from FNA varieties, so the backbone was systematically undoing the project's
taxonomic decisions for the *cinerea*, *aestivalis* and *rotundifolia*
complexes.

### 4.2 Records dropped because GBIF could not match the recorded name

GBIF fell back to "Vitis L." and the parser produced the bare genus:

| Project taxon | Records | Recorded as |
|---|---:|---|
| Vitis x champinii | 74 | "Vitis xchampinii" (no space after the marker) |
| Vitis x novae-angliae | 23 | "Vitis xnovae-angliae" |
| Vitis x doaniana | 16 | "Vitis xdoaniana" |
| Vitis rufotomentosa | 14 | "Vitis rufotomentosa" |
| Vitis martineziana | 2 | "Vitis martineziana" (Mexico) |

The hybrid marker itself was not the problem: the 92, 280 and 62 records GBIF
did match were kept under the published rule, because the synonym step maps
"Vitis champinii" back to "Vitis x champinii". The loss was the unmatched
spelling variants, and they come back only from `verbatimScientificName`.

### 4.3 Records where the backbone was right and name parsing is wrong

Homonyms: *Vitis labrusca* Thunb. (21 records, = *V. coignetiae*),
*V. cordifolia* Roth ex Roem. & Schult. (13, = *V. heyneana*), *V. labrusca*
Scop. (1, = *V. vinifera*). A name-based parser puts these into *labrusca* and
*vulpina*. They should be listed, with authorship, in the concept's "Names to
exclude" cell.

### 4.4 Not a download problem: *novogranatensis* and *rubriflora*

- Neither name has a single record in the file under any method. The 1
  *novogranatensis* and 4 *rubriflora* rows in the current model data come
  from Jun Wen's personal-communication spreadsheet, not GBIF.
- The backbone files *Vitis novogranatensis* Moldenke under *Cissus
  novogranatensis*. GBIF today holds 4 records recorded as "Vitis
  novogranatensis"; they sit under *Cissus* and a genus-keyed *Vitis* download
  never sees them. Whether to chase 4 records is a curator call.
- The *Parthenocissus semicordata* result for *rubriflora* is a fuzzy match to
  *Vitis rubrifolia*; a strict match returns nothing, and GBIF holds zero
  records recorded as "Vitis rubriflora". Nothing to recover.
- The *novogranatensis* re-injection in both preprocessing drivers is a
  work-around for the cross-dataset de-duplication step, not for GBIF, and the
  de-duplicated table (`df2_a`) is never used downstream anyway. It is a
  separate defect and should not be presented as a symptom of the backbone
  error.

### 4.5 Names surfacing under name parsing that match no concept

These were folded in silently by the backbone and now need a sheet decision.
The large ones: *Vitis rotundifolia* var. *munsoniana* (412; the sheet's own
note says the *munsoniana* concept *is* var. *munsoniana*), *V. riparia*
subsp. *riparia* (417), *V. caribaea* (202, = *tiliifolia*), *V.* x
*labruscana* (106, probably exclude), *V. bicolor* (87), *V. coriacea* (36),
*Muscadinia popenoei* (22). Full list: `work2026/gbif_unmatched_names.csv`
on `main`.

## 5. Other defects found on the way (all fixed in `15e72a1`)

| Issue | Records | Fix |
|---|---:|---|
| Hybrid formulas ("Vitis riparia x Vitis rupestris") were credited to the first parent | 197 | parser returns NA for formulas; named hybrids kept |
| Regex had no hyphen support, so "Vitis x novae-angliae" parsed as "Vitis x novae" | 280 | hyphenated epithets allowed |
| *V. vulpina* synonym cell uses semicolons and "V."; `speciesCheck()` split only on ", " | 607 | `splitNames()` handles both |
| *V. munsoniana* lists itself as a synonym; every record was appended twice | 231 -> 462 rows | self-synonyms skipped |
| "Names to exclude from this concept" was loaded but never applied | 1,105 would have been removed under the old rule | applied inside `speciesCheck()`, removals written to `excludedByConcept_<date>.csv` |

Still open in the parser: capitalised variety epithets ("var. Bicolor") fall
out of the optional group and the variety is silently dropped; homonyms need
the exclude list.

## 6. Decisions to get from the co-author

1. The project concept table (`data/New World Vitis.csv`, synced from the
   Google Sheet) is the taxonomic authority. The backbone never overrides it.
   Disagreements are carried as audit columns, not acted on.
2. Record-keeping policy, three cases:
   - recorded name is a project name or synonym, GBIF disagrees: keep under
     our concept;
   - recorded name is genus-only or unparseable: drop, as now;
   - GBIF rewrote the rank (e.g. *V. aestivalis* subsp. *rufotomentosa* ->
     "Vitis aestivalis"): decide whether the recorded string or GBIF's wins.
     The recommended parser currently lets GBIF win because its string keeps
     infraspecific epithets that publishers supplied in separate fields.
3. *V. rufotomentosa* specifically: what to do with the 6 Japanese records
   named *V. rufotomentosa* Small, and whether *V. aestivalis* subsp.
   *rufotomentosa* (15 records) belongs in the concept.
4. Sheet edits (these go to the sheet, never to the code):
   - *munsoniana* include: add "Vitis rotundifolia var. munsoniana" and
     "Muscadinia rotundifolia var. munsoniana", remove the self reference;
   - *vulpina* include: replace the semicolon, spell out "Vitis";
   - *labrusca* exclude: "Vitis labrusca Thunb.", "Vitis labrusca Scop.";
   - *vulpina* exclude: "Vitis cordifolia Roth ex Roem. & Schult.";
   - accept or reject each name in section 4.5.
5. Whether to run a second, name-based GBIF download to pick up the 4
   *novogranatensis* records filed under *Cissus*.
6. Manuscript stage. `process_gbif.R` is annotated as the version used in the
   publication, and the path it names is the May 2023 download
   (`data/source_data/gbif.csv`), which has the same columns. The mechanism
   applies to the published run; the counts above are from the July 2025
   file. Pre-submission fix or erratum changes how much has to be re-run.

## 7. What the fix looks like and the sequence

The fix is already on `main` in `preprocessing/functions/process_gbif_082026.R`
(commit `15e72a1`): `parseVitisName()` reads the name string and uses the
backbone only when the string does not parse. `nameSource = "combined"`
(parse `scientificName`, then `verbatimScientificName`, then backbone) is
the recommended setting; the driver
`preprocessing/preprocessingUpdates2026_08_20.R` still calls the default
(`scientificName`), which recovers everything in 4.1 but nothing in 4.2.
No new GBIF download is needed for any of this.

1. Switch the driver to `nameSource = "combined"`, apply the agreed sheet
   edits, and run the 2026-08-20 preprocessing for the first time. Diff
   per-taxon counts against `model_data20251216.csv`.
2. Add audit columns to the GBIF output: recorded name, GBIF matched name,
   backbone accepted species, and the `issue` flags, so a future disagreement
   is visible instead of silent.
3. Add a post-processing check that every "Y" taxon in the concept table
   appears in the processed data.
4. Remove the compensating patches: the hard-coded *rufotomentosa* and
   *novogranatensis* scores in `compileConservationData.R` and
   `summaryDocForNoRecords.Rmd`, and the `novogranatensis` re-injection once
   the de-duplication step is fixed to actually feed downstream.
5. Re-run the models under a new `runVersion`. Every downstream product is
   keyed to the occurrence file, so a full re-run is cleaner than re-running
   the twelve affected taxa plus their recipients. Before that,
   `run_all05082026.R` still refers to a `prep_species_data.R` that does not
   exist and reads a publication file that is 1,525 rows longer than the
   model data with no script producing it.
6. Decide together which published tables and figures change and how to
   communicate it.

## 8. Update 2026-09-16: the pipeline has now been run

The 2026-08-20 preprocessing was executed for the first time with the
recommended parser setting. Final model-data counts (after de-duplication,
country and coordinate checks) for the taxa in section 4:

| Taxon | Dec 2025 rows | Sep 2026 rows | with coordinates Dec -> Sep |
|---|---:|---:|---|
| Vitis berlandieri | 736 | 1357 | 359 -> 626 |
| Vitis munsoniana | 22 | 265 | 18 -> 110 |
| Vitis lincecumii | 168 | 332 | 71 -> 88 |
| Vitis baileyana | 531 | 653 | 105 -> 152 |
| Vitis simpsonii | 913 | 1008 | 392 -> 448 |
| Vitis rufotomentosa | 2 | 42 | 0 -> 20 |
| Vitis x champinii | 126 | 192 | 33 -> 38 |
| Vitis cinerea | 5341 | 4445 | 2172 -> 1772 |
| Vitis aestivalis | 12877 | 12496 | 5618 -> 5497 |
| Vitis rotundifolia | 27551 | 27297 | 23084 -> 22980 |

Full table for all 40 taxa: `work2026/changeInCounts_20260916.csv`; by source:
`work2026/changeInCounts_bySource_20260916.csv`; discussion in
`work2026/GBIF_taxonomy_notes.md` section 10.

Two changes in the same run are **not** the taxonomy fix and should be
reported separately: (a) 206 records with eastern-hemisphere longitudes that
the old driver re-added after flagging them are now removed (mostly *vulpina*
88, *aestivalis* 60); nine WIEWS *V. riparia* accessions whose Montana
longitude lacked the minus sign are kept via a documented sign flip; (b) *V. cinerea* var. *tomentosa* drops to 0 records
because its 11 records were all named "Vitis berlandieri var. tomentosa",
which is on no include list. Item (b) needs a sheet decision (section 6, point
4). Decision 3 on the six Japanese records is moot for the model data: they are
removed by the country filter either way.

SRSex re-computed on both files (`work2026/srsEx_comparison_20260916.csv`):
24 of 40 taxa change score, four change priority class. *V. rufotomentosa*
100 -> 5.0 (LP -> UP; the published 100 was the no-herbarium-records default),
*V.* x *champinii* 80.0 -> 41.2 (LP -> HP), *V. berlandieri* 28.5 -> 18.0
(HP -> UP), *V. cinerea* var. *tomentosa* has no score. All other scores move
by under 13 points and stay in class.

Homonyms (section 4.3): decision 2026-09-16 is to keep the backbone
assignment for authored homonyms in code rather than maintain exclude-cell
entries. The sheet edits for *labrusca* and *vulpina* exclusions in section 6
are therefore withdrawn.

Autonyms: decision 2026-09-16 is to match the forma autonym ("Vitis
rupestris f. rupestris") to its species in code, so *rupestris* keeps its 20
accessions. Variety and subspecies autonyms ("Vitis riparia subsp. riparia",
417 records) stay on the decision list in section 4.5.

## 9. What changed from the earlier brief, and why

| Earlier claim | Status | Evidence |
|---|---|---|
| Raw download not on this machine; 26 vs 32 cannot be reconciled | Wrong | `data/source_data/vitisGBIFDownload_20250721.csv` is present; 32 = synonym-matched, 26 = same minus 6 JP, 46 = plus 14 genus-fallback |
| *rufotomentosa* collapsed to bare genus | Partly | 32 went to *V. aestivalis* as a synonym; 14 collapsed to genus. The bare-genus outcome is what today's backbone returns for the name, not what happened to most records in the file |
| *novogranatensis* and *rubriflora* lost to *Cissus* / *Parthenocissus* | Wrong | Zero records in the file under either name; the *Parthenocissus* result is a fuzzy match to *V. rubrifolia*; current model rows come from Jun Wen's data |
| *novogranatensis* re-injection compensates for the backbone bug | Wrong | It works around the de-duplication step, whose output is never used |
| Hybrids "probably recovered through the synonym list" | Wrong | The matched records were never lost; the 113 unmatched spelling variants are lost under both the old rule and the `scientificName` parse and come back only via `verbatimScientificName` |
| `taxonomicStatus` and `acceptedScientificName` are already in the file | Wrong | Simple-format download, 50 columns; neither is present. `verbatimScientificName` and `issue` are |
| The 082026 parser does not recover *lincecumii* | Out of date | True of the branch's regex; `15e72a1` recovers all 173 |
| 12 of 40 taxa affected, present as categories not counts | Superseded | The download gives defensible counts per taxon (section 4) |
| Sequence step 1: implement recorded-name parse and rerun the impact table | Done | Section 4 is that table |

Branch state: `main` (`15e72a1`) holds the parser fix, the diagnostic script
and its outputs; `workflow-evaluation` (`2c152b2`) holds the evaluation and
the backbone check script. The branches have diverged from `67cf6fa` and need
merging. The three files showing as modified on `workflow-evaluation` are
mode changes only.
