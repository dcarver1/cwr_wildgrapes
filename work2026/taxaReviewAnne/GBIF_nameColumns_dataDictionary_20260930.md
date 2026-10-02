# Data dictionary: taxon name columns (GBIF download and project pipeline)

For the taxonomy review with Anne (2026-09-30). Covers the name-related columns in
`taxonChangeRawData_20260924.xlsx` (records tab) and where each comes from.

Source download: `data/source_data/vitisGBIFDownload_20250721.csv` (GBIF "simple" format, genus-keyed on *Vitis*).
In the review workbook every column copied from that download carries the prefix `gbif_`.
The simple format has no `taxonomicStatus` or `acceptedScientificName` column; whether a name is
a synonym has to be looked up by `taxonKey` (see `gbifNameUsage` tab of the raw-data workbook).

## How a name travels through GBIF and the pipeline

```
publisher writes a name  ->  verbatimScientificName        (untouched)
GBIF matches it to its backbone  ->  scientificName, taxonKey, taxonRank   (the name usage it matched; may be a synonym)
GBIF resolves that usage to an accepted species  ->  species, speciesKey   (where GBIF "folds" the record)
project pipeline stores scientificName as  ->  originalTaxon  (= projectRecordedName in the review workbook)
project pipeline parses a name string to assign the concept  ->  taxon  (= species column in the review workbook)
```

The published (Dec 2025) dataset assigned records by GBIF's `species` (the accepted species), which is how
synonyms were lumped (e.g. *V. berlandieri* records sat under *V. cinerea*). Round 2 assigns by parsing the
name string instead (`nameSource = "combined"` in `preprocessing/functions/process_gbif_082026.R`).

## GBIF columns (prefix `gbif_` in the review workbook)

| Column | What it holds | Set by | Notes for review |
|---|---|---|---|
| `verbatimScientificName` | The name exactly as the data publisher supplied it, before any GBIF interpretation. May lack authorship, contain typos, hybrid formulas, odd spacing. | Publisher | The most direct evidence of what the collector/herbarium called the specimen. Round 2 uses it only when `scientificName` can't be parsed (see "Vitis L." below). |
| `verbatimScientificNameAuthorship` | Authorship as supplied by the publisher, when given separately. | Publisher | Often blank; authorship is frequently inside `verbatimScientificName` instead. |
| `scientificName` | GBIF's interpreted name: the backbone name usage the verbatim name was matched to, with standardised authorship. Can be a synonym usage. | GBIF matching | Normalises spelling and authorship (e.g. "Vitis coriacea Shuttleworth ex Planchon" -> "Vitis coriacea Shuttlew. ex Planch."). If GBIF cannot match below genus, this becomes **"Vitis L."**. |
| `taxonKey` | Backbone ID of the `scientificName` usage. | GBIF matching | Look up this key to see whether the usage is ACCEPTED or SYNONYM, and what it is a synonym of. |
| `taxonRank` | Rank of the matched usage: SPECIES, VARIETY, SUBSPECIES, FORM, GENUS. | GBIF matching | GENUS = GBIF could not match the name to anything finer. |
| `species` | The backbone's **accepted** species that `taxonKey` resolves to. For a synonym, this is the species GBIF folds the record into. | GBIF backbone | This is the column behind the lumping problem: *V. berlandieri* -> *V. cinerea*, *V. caribaea* -> *V. tiliifolia*. Blank when rank is GENUS. |
| `speciesKey` | Backbone ID of that accepted species. | GBIF backbone | |
| `infraspecificEpithet` | Infraspecific epithet of the matched usage, if any. | GBIF matching | |
| `genus` (and `kingdom`...`family`) | Higher classification of the matched usage. | GBIF backbone | |
| `issue` | GBIF interpretation flags. `TAXON_MATCH_HIGHERRANK` = only matched at a higher rank (usually genus); `TAXON_MATCH_FUZZY` = matched after correcting spelling; `TAXON_MATCH_NONE` = no match. | GBIF matching | Useful to spot records whose name GBIF altered or gave up on. |

## Project columns (review workbook)

| Column | What it holds | Pipeline name |
|---|---|---|
| `projectRecordedName` | The name the project stored for the record. For GBIF records this **is** GBIF's `scientificName` (renamed on import); identical in every review row where both are filled. For non-GBIF sources it is that source's own name field. | `originalTaxon` |
| `species` | The project concept the record was in (Removed rows) or is now in (Added rows). | `taxon` |
| `counterpart` | Removed rows: the concept the record now sits in, or "no project concept (excluded)". Added rows: the concept it sat in before, or "not in the published dataset". | derived |

## Worked examples from the review records

| verbatimScientificName | scientificName (= projectRecordedName) | species (GBIF accepted) | rank | What happened |
|---|---|---|---|---|
| Vitis berlandieri | Vitis berlandieri Planch. | Vitis cinerea | SPECIES | Published: folded into *V. cinerea*. Round 2 parses "Vitis berlandieri" -> moved to *V. berlandieri* (627 records across all verbatim spellings). |
| Vitis caribaea | Vitis caribaea DC. | Vitis tiliifolia | SPECIES | Published: in *V. tiliifolia*. Round 2: "V. caribaea" is on no include list -> dropped. |
| Vitis coriacea Shuttleworth ex Planchon | Vitis coriacea Shuttlew. ex Planch. | Vitis shuttleworthii | SPECIES | Same pattern; Anne: include as *V. shuttleworthii* synonym. |
| Vitis argentifolia Munson ex L. H. Bailey | Vitis argentifolia Munson ex L.H.Bailey | Vitis aestivalis | SPECIES | Authorship spacing normalised by GBIF; argentifolia is on the var. *bicolor* include list. |
| Vitis xchampinii | **Vitis L.** | (blank) | GENUS | GBIF could not match the hybrid name (no space after "x"), so fell back to genus. Round 2 parses the verbatim name instead -> *V. × champinii* (66 records). |
| Vitis rufotomentosa | **Vitis L.** | (blank) | GENUS | Same fallback; recovered from the verbatim name (13 records). |

## Reading "Vitis L."

`scientificName = "Vitis L."` with `taxonRank = GENUS` means GBIF matched only to genus. The review question for
these rows is about the **verbatim** name, not "Vitis L.". In the review records 124 of 3,641 joined GBIF rows are
GENUS rank, and 292 carry `TAXON_MATCH_HIGHERRANK`.

## Evaluation: are the three names the same for the affected records?

Scope: records whose change is attributed to the taxonomy handling (`reason = gbifTaxonomy`): 3,358 review rows,
of which 3,338 are GBIF rows joined to the download = **2,040 unique GBIF records** (a moved record appears twice,
once Removed and once Added). 19 rows are non-GBIF sources and 1 GBIF row could not be joined.
Script: `nameColumnComparison_20260930.R`; per-record and per-name results in `nameColumnComparison_20260930.xlsx`.

Names are compared as exact text and as the bare name (genus + epithet + rank/infraspecific epithet, authorship
dropped) using the pipeline's own parser, `parseVitisName()`.

**originalTaxon vs scientificName: always the same.** Every joined GBIF row matches. (For 24 rows joined by the
fallback match the review workbook left `gbif_scientificName` blank, because those rows were matched *on* that
name; they are the same by construction.) The pipeline copies `scientificName` into `originalTaxon` unchanged.

**scientificName vs verbatimScientificName (2,040 unique GBIF records):**

| Result | Records | Meaning |
|---|---|---|
| Same name, authorship/capitalisation differs | 1,441 | Same plant name; GBIF added or standardised the author ("Vitis berlandieri" -> "Vitis berlandieri Planch.") |
| Same (identical text) | 374 | Publisher's string already matched the backbone form |
| Different: GBIF matched genus only ("Vitis L.") | 124 | GBIF could not match the name (e.g. "Vitis xchampinii", "Vitis rufotomentosa"); round 2 takes the name from the verbatim string |
| Same name, hybrid marker/spacing differs | 67 | Mostly "Vitis labruscana" / "Vitis X labruscana" vs "Vitis × labruscana" |
| Verbatim name blank in the download | 23 | Nothing to compare; the pipeline used `scientificName` |
| Same binomial, rank/infraspecific part differs | 8 | e.g. verbatim "Vitis munsoniana f. pygmaea ..." matched to species; "Vitis vulpina L. ssp. riparia ..." |
| Different: spelling corrected by GBIF (fuzzy match) | 3 | "Vitis berandieri" -> berlandieri; "Vitis australis" -> austrina; "Muscadinia munsonia" -> munsoniana |

**Bottom line:** for 1,882 of 2,040 affected records (92%) all three columns carry the same plant name, differing at
most in authorship, capitalisation or hybrid-marker formatting. The name did not change between publisher and GBIF;
what moved these records is how the name was assigned to a project concept (GBIF's accepted `species` in the
published run vs the name itself in round 2). The exceptions worth a look are the 124 genus-only matches
(assignment rests entirely on the verbatim string) and the 3 fuzzy corrections (GBIF's spelling fix was accepted
as the name). The "Vitis australis" -> *V. austrina* correction in particular is a guess by GBIF's matcher.

## Sources

- Column meanings: GBIF simple-download fields (Darwin Core terms plus GBIF interpretation), as summarised in the
  `fieldNotes` tab of `temp/taxonChangeRawData_20260924.xlsx` (written 2026-09-24).
- Pipeline behaviour: `preprocessing/functions/process_gbif_082026.R` (`processGBIF()`, `parseVitisName()`).
- Examples and counts: `temp/taxonChangeRawData_20260924_records.csv`.
