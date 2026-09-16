# work2026/compareRunVersions_20260916.R
# Compare per-species outputs of two run versions (default: the published
# run08282025_1k vs the round-2 test run09162026_1k) for the species that
# exist under the new version. Deterministic outputs (counts, SRSex, spatial
# points, natural area, buffer) must match exactly; SDM-dependent ones
# (threshold, GRS/ERS, FCS, AUC) are expected to be close but not identical.
suppressPackageStartupMessages({library(dplyr); library(readr); library(sf); library(terra); library(purrr); library(tidyr)})
options(width = 220)
old <- if (exists("oldRun")) oldRun else "run08282025_1k"
new <- if (exists("newRun")) newRun else "run09162026_1k"
species <- basename(dirname(list.dirs("data/Vitis", recursive = FALSE) |> (\(d) file.path(d, new))() |> Filter(f = dir.exists)))
cat("species with", new, ":", paste(species, collapse = ", "), "\n\n")

readOne <- function(sp, run) {
  base <- file.path("data/Vitis", sp, run)
  g <- function(p) file.path(base, p)
  rc <- function(p, ...) if (file.exists(g(p))) read_csv(g(p), show_col_types = FALSE) else NULL
  cnt <- rc("occurances/counts.csv"); srs <- rc("gap_analysis/srs_ex.csv"); fce <- rc("gap_analysis/fcs_ex.csv"); fci <- rc("gap_analysis/fcs_in.csv")
  grse <- rc("gap_analysis/grs_ex.csv"); erse <- rc("gap_analysis/ers_ex.csv"); srsi <- rc("gap_analysis/srs_in.csv"); grsi <- rc("gap_analysis/grs_in.csv"); ersi <- rc("gap_analysis/ers_in.csv"); fcc <- rc("gap_analysis/fcs_combined.csv")
  ev <- rc("results/evaluationTable.csv"); mds <- rc("occurances/modelDataSummary.csv")
  pts <- if (file.exists(g("occurances/spatialData.gpkg"))) st_read(g("occurances/spatialData.gpkg"), quiet = TRUE) else NULL
  nat <- if (file.exists(g("results/naturalArea.gpkg"))) st_read(g("results/naturalArea.gpkg"), quiet = TRUE) else NULL
  thr <- if (file.exists(g("results/prj_threshold.tif"))) rast(g("results/prj_threshold.tif")) else NULL
  vs <- if (file.exists(g("occurances/topVariablesData.csv"))) read_csv(g("occurances/topVariablesData.csv"), show_col_types = FALSE) else NULL
  tibble(taxon = sp, run = run,
    NTOTAL = cnt$totalRecords, NCOORDS = cnt$totalUseful, NG = cnt$totalGRecords, NH = cnt$totalHRecords,
    spatialPts = if (!is.null(pts)) nrow(pts) else NA, spatialG = if (!is.null(pts)) sum(pts$type == "G") else NA,
    statesInPts = if (!is.null(pts)) n_distinct(pts$state) else NA,
    natAreaFeatures = if (!is.null(nat)) nrow(nat) else NA, natAreaEco = if (!is.null(nat) && "ECO_ID_U" %in% names(nat)) n_distinct(nat$ECO_ID_U) else NA,
    presenceRows = mds$presenceRecords, backgroundRows = mds$backgroudRecords,
    nVarsSelected = if (!is.null(vs)) nrow(vs) else NA, varsSelected = if (!is.null(vs)) paste(sort(vs[[1]]), collapse = "|") else NA,
    thresholdCells = if (!is.null(thr)) as.numeric(global(thr, "sum", na.rm = TRUE)[1, 1]) else NA,
    AUCtest = if (!is.null(ev)) mean(ev$AUCtest, na.rm = TRUE) else NA, threshold = if (!is.null(ev)) mean(ev$threshold_train, na.rm = TRUE) else NA,
    SRSex = srs$SRS, GRSex = grse$GRS, ERSex = erse$ERS, FCSex = fce$FCSex,
    SRSin = srsi$SRS, GRSin = grsi$GRS, ERSin = ersi$ERS, FCSin = fci$FCSin,
    FCSc_mean = fcc$FCSc_mean, FCSc_mean_class = fcc$FCSc_mean_class)
}
both <- map_dfr(species, function(sp) bind_rows(readOne(sp, old), readOne(sp, new)))
long <- both |> mutate(across(-c(taxon, run), as.character)) |> pivot_longer(-c(taxon, run), names_to = "metric") |>
  pivot_wider(names_from = run, values_from = value) |>
  mutate(match = coalesce(.data[[old]] == .data[[new]], is.na(.data[[old]]) & is.na(.data[[new]])),
         kind = if_else(metric %in% c("NTOTAL","NCOORDS","NG","NH","spatialPts","spatialG","statesInPts","natAreaFeatures","natAreaEco","presenceRows","SRSex"), "deterministic", "sdm-dependent"))
write_csv(long, paste0("work2026/runComparison_", old, "_vs_", new, ".csv"))
for (sp in species) { cat("=====", sp, "=====\n"); print(as.data.frame(long |> filter(taxon == sp) |> select(kind, metric, all_of(c(old, new)), match)), row.names = FALSE) }
cat("\nDeterministic mismatches:\n"); print(as.data.frame(long |> filter(kind == "deterministic", !match)))
