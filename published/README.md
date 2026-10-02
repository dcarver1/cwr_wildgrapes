# published/

Scripts and tables that produced the published analysis (run version
`run08282025_1k`) and are not used by the current pipeline. They are kept as a
record. Paths inside them refer to their original locations (repository root
and `preprocessing/`), so they will not run from this folder without edits.

* `run_all05082026.R`, `run_all072025.R`: model drivers of the published run
* `preprocessing/preprocessingUpdates2025_07.R` and `preprocessing/functions/`:
  the published preprocessing, the original GBIF parser (`process_gbif.R`) and
  the per-source functions whose saved outputs the current preprocessing reads
* `preprocessing/` (other files): how the rasters, protected-area layer and
  source tables were built
* `variable_buffer_*.csv`, `variableBufferAnalysis.R`: buffer-distance
  sensitivity table of the supplement
