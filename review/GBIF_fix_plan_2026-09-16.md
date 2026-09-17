# Plan: GBIF backbone fix through to the trimmed repo

Agreed 2026-09-16. Working branch is `workflow-evaluation`; `main` is frozen at
tag `main-frozen-2026-09-16` (15e72a1). A `restructure` branch exists for the
later trim. Background: `review/GBIF_backbone_coauthor_brief.md` and
`work2026/GBIF_taxonomy_notes.md`.

Rationale for the order: the co-author deliverable is a per-taxon count change
that must be attributable to the parser fix alone, so the fix is run in the
existing repo before anything is moved. The re-run also produces the list of
scripts that are actually load-bearing, which is the keep-list for the trim.
The history that links published numbers to code is kept by tagging, not by
starting a new repo.

## Phase 1: co-author deliverable (this branch)

1. Run `preprocessing/preprocessingUpdates2026_08_20.R` end to end (first
   execution ever). Fix only what blocks the run.
2. Diff per-taxon record counts against
   `data/processed_occurrence/model_data20251216.csv`, split by source and by
   coordinate presence. Output: `work2026/changeInCounts_<date>.csv`.
3. Write a download provenance note next to the raw GBIF file: query, format,
   date, Drive location, download DOI if recoverable from the GBIF account.

## Phase 2: co-author decisions

4. Sheet edits (never code): munsoniana include names, vulpina cell cleanup,
   homonym exclusions for labrusca and vulpina, accept/reject each unmatched
   name in notes section 5.
5. Policy calls: the 6 Japanese rufotomentosa records; whether
   V. aestivalis subsp. rufotomentosa belongs in the concept; whether GBIF
   LIVING_SPECIMEN counts as germplasm for SRSex.
6. Manuscript stage: pre-submission fix or erratum; what is re-run and
   re-reported.
6b. Models that run but fail the stated robustness rule (rufotomentosa, STAUC
   0.158): keep, buffer-method fallback in code, or hand decision. See
   `review/modeling_future_improvements.md` item 1b.

## Phase 3: re-run

7. Re-sync the taxonomy sheet, re-run preprocessing under a new date suffix,
   re-run models under a new `runVersion`. Remove hand-coded rufotomentosa
   scores from `compileConservationData.R` and `summaryDocForNoRecords.Rmd`
   once it has a real model.
8. Merge to `main`, tag the state that produced the corrected numbers.

## Phase 4: trim (on `restructure`)

9. Cut from the phase-3 tag. Keep-list = files sourced by the re-run. Delete
   everything else first, restore on demand.
10. Carry the GBIF documents, the provenance note and a rewritten README into
    the trimmed structure.

## Deliberately left open until phase 3 or 4

Full list with considerations: `review/modeling_future_improvements.md`.

* `df2_a` (cross-source de-duplication) is computed and never fed downstream.
* Duplicate `speciesCheck()` in `preprocessing07_2025Functions.R`.
* The `novogranatensis` re-injection.
* `run_all05082026.R` refers to a missing `prep_species_data.R` and reads a
  publication file 1,525 rows longer than the model data with no script
  producing it.
