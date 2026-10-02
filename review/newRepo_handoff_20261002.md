# Handoff brief: new CWR modelling repository

Written 2026-10-02 from the `cwr_wildgrapes` repo (branch `workflow-evaluation`).

## Goal

Build a new repository that systematically improves the reproducibility,
methods, functionality and organisation of the crop wild relative (CWR)
modelling and gap-analysis process. It is NOT tied to the Vitis paper: the
paper's corrections are being finished in `cwr_wildgrapes` with the published
method, unchanged. The new repo is where method improvements and a cleaner
structure go, and it should work for taxa beyond Vitis.

## What the current pipeline does (the behaviour to carry over)

Source repo: `/home/dune/trueNAS/work/cwr_wildgrapes`. R, base scripts, no package.

1. **Preprocessing** (`preprocessing/preprocessingUpdates2026_08_20.R` +
   `preprocessing/functions/`): one script per source (GBIF, GRIN, Genesys,
   WIEWS, SEINet, Midwest Herbaria, UC Davis, BONAP, NatureServe, botanical
   gardens, PNAS 2020), standardised to one schema, names assigned to project
   taxon concepts from the taxonomy sheet (`data/New World Vitis.csv`:
   include names / exclude names per concept), coordinate checks, output one
   model-data CSV. Records are typed G (germplasm) or H (herbarium/reference).
2. **Model driver** (`run_round2_20260916.R`, ~800 lines, loop over taxa;
   functions in `R2/`). Per taxon:
   - points + FNA state filter (`R2/dataProcessing/fnaFilter.R`)
   - natural area = TNC ecoregions containing a point; background points
   - variable selection (VSURF on a FORK cluster, then correlation pruning)
   - maxnet SDM, k-fold, median projection, threshold -> binary map
   - validity rule: mean test AUC >= 0.7, SD of AUC < 0.15, ASD15 <= 10%
   - fewer than 8 points -> 50 km buffer method instead of an SDM
   - gap metrics (`R2/gapAnalysis/`): SRS/GRS/ERS ex situ and in situ, FCS ex,
     FCS in, FCS combined, priority class (UP/HP/MP/LP)
   - per-taxon HTML summary (Rmd)
3. **Outputs**: `data/Vitis/<taxon>/<runVersion>/{occurances,results,gap_analysis}`
   plus compile scripts in `R2/summarize/` that gather per-taxon CSVs.

## Structural changes recommended (agreed in principle 2026-10-02)

- **Taxonomy as data**: one versioned name -> concept table with decision,
  decider and date. The Google Sheet is the editing surface; a run reads a
  dated snapshot so it is tied to a sheet state.
- **Records flagged, never dropped**: every record carries a fate column
  (kept, or excluded with a reason code). All of September's diff reports
  would have been a `count()`.
- **One config per run** (run version, input file, method switches), copied
  into the output folder. Replaces copied driver scripts.
- **`targets` with per-taxon branching**: a synonym change reruns only the
  affected taxa; one taxon's failure does not stop the rest.
- **One long metrics table per run** (taxon, metric, value, class). Comparing
  two runs becomes a join.
- **Separate geographic QC from study extent**: the FNA filter currently does
  both jobs.
- **`terra` throughout**; no `raster::predict` (it wrote ~7 GB of fold
  projections to the system temp dir and filled the root disk).
- **Functions take arguments, not globals**; packaged or at least testable.
- **Regression test**: a few small taxa whose outputs must reproduce exactly.
- **Data manifest** with checksums and download provenance (GBIF DOIs).
- Parameterised report templates.

## Defects and traps found in the current code (do not copy)

- GBIF backbone renames records away from the project's concept; parse the
  verbatim/authored name (`parseVitisName()`, `nameSource = "combined"`).
- FNA filter matches on exact taxon name, so concepts with a different name
  in the FNA table are silently unfiltered (munsoniana keeps Texas and
  California points; lincecumii has no row). Mexican states are missing for
  most cross-border taxa.
- Cross-source de-duplication is computed and never used downstream.
- Bounding-box failures were re-bound into the model data.
- Spatial thinning was computed then overwritten (never applied in any
  published model); correlation pruning only tests the top 5 predictors.
- Validity flag is computed but nothing acts on it (rufotomentosa fails).
- A hand filter `longitude != x` dropped all NA-longitude rows for one taxon.
- Mixed-vintage output folders: a published SDM fit on points the stored
  spatial file no longer contains. Outputs must be written atomically per run.
- SRSex uses all records; in situ metrics use georeferenced, filtered points.
  Legitimate by definition, but must be stated.
- Hand-coded scores in `compileConservationData.R`; a driver referencing a
  missing script; species folder names differing only by case.
- Reproducibility depends on `RNGkind("L'Ecuyer-CMRG")`, a per-taxon seed and
  a fixed worker count; runs then repeat exactly.

Detail: `review/WORKFLOW_EVALUATION.md`, `review/modeling_future_improvements.md`,
`review/modeling_experiments_ranked.md`.

## Method-test evidence (2026-09-17 and 2026-10-02)

Switches in the current driver, default off: `thinOccurrences` (5 km thinning
of H points when > 50) and `fullCorrPruning`. Results:
`temp/methodTests_20261002/methodTestSummary.csv`, maps in `maps/`, scripts
`work2026/methodTestSummary_20261002.R`, `work2026/methodTestMaps_20261002.R`.

- 10 taxa tested with baseline / thinning / pruning / both. Every model
  passes the validity rule.
- Thinning removes 10-60% of model points and expands modelled area by up to
  ~50%; test AUC drops slightly. Pruning shifts area by -10% to +34%.
- Priority class changes under the combined method in 2 of 10 taxa
  (monticola via thinning, x champinii via pruning); rupestris changes under
  thinning alone. All three have substantial ex situ scores; the 7 that did
  not change have little or no germplasm.
- Not tested: the 6 remaining medium taxa and all 11 large taxa (1,250 to
  22,000 points). spThin builds an all-pairs distance matrix, so the largest
  taxa may be slow or memory-heavy.
- Decision for the paper: published method only. In the new repo these
  methods can be on by default, applied to all taxa.
- Resolved as non-issues for this data: ERS polygon-part counting, WDPA
  status filter (no proposed areas in USA/CAN/MEX), background-point rule,
  threshold edge case (fixed, zero effect).
- Still untested: buffer fallback on invalid model, cross-source dedup,
  bounding-box enforcement, FNA Mexico handling, spatial-block CV.

## Performance profile (24 cores, 117 GB RAM)

- Most of a taxon's run is single-threaded: maxnet fitting across folds is
  serial and is the longest step. Only variable selection uses its 8 workers.
- Memory is the constraint: ~13 GB machine-wide per concurrent taxon during
  variable selection for a medium taxon; the largest taxa peak at 15-23 GB in
  the main process alone. Staggering starts by ~100 s kept three concurrent
  arms at 24-33 GB.
- Obvious gains: parallel folds, taxon-level parallelism with a memory cap,
  4 workers instead of 8 for variable selection (identical scores in test).
- Use `/mnt/scratch/tmp` (400 GB) as R's temp dir, not `/tmp`.

## Regression baseline to reproduce first

Run version `run09162026_1k` in the old repo (all 40 taxa, published method,
seeded). Small, fast, exactly repeatable taxa: nesbittiana, biformis,
x doaniana, x champinii. A new pipeline with all switches off should
reproduce their gap metrics before any method is changed.

## Open design questions for the new thread

1. Package, `targets` project, or both?
2. Sheet read live from Drive vs dated snapshot committed per run.
3. Where large data lives (not in git) and how it is manifested.
4. Scope: Vitis first then generalise, or genus-agnostic from the start?
5. Align score functions with the `GapAnalysis` package or keep own versions
   (evaluation section 4 lists where they diverge).
6. Which method changes are on by default, and in what order they are
   validated against the baseline.
