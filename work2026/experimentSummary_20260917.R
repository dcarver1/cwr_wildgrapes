# work2026/experimentSummary_20260917.R
# Compare each experiment run version with production (run09162026_1k) on the
# species both have: runtime per species (timing logs), peak R memory, sampled
# process-tree RSS peak (from the orchestrator's sampler), the three priority
# classes and the main metrics. Output: work2026/experimentSummary_20260917.csv
# and a per-experiment class-change count.
suppressPackageStartupMessages({library(dplyr); library(readr); library(purrr); library(tidyr); library(stringr)})
options(width = 240)
prod <- "run09162026_1k"
exps <- if (exists("expRuns")) expRuns else grep("^timing_run09172026_exp", list.files("data/Vitis"), value = TRUE) |> str_remove("^timing_") |> str_remove("\\.csv$")
readTiming <- function(run) { f <- paste0("data/Vitis/timing_", run, ".csv"); if (!file.exists(f)) return(NULL); read_csv(f, show_col_types = FALSE) |> group_by(taxon, runVersion) |> slice_tail(n = 8) |> ungroup() }
readScores <- function(sp, run) { b <- file.path("data/Vitis", sp, run); rc <- function(p) if (file.exists(file.path(b, p))) read_csv(file.path(b, p), show_col_types = FALSE) else NULL
  fce <- rc("gap_analysis/fcs_ex.csv"); fci <- rc("gap_analysis/fcs_in.csv"); fcc <- rc("gap_analysis/fcs_combined.csv"); auc <- rc("results/aucMetrics.csv"); mds <- rc("occurances/modelDataSummary.csv"); vs <- rc("occurances/topVariablesData.csv")
  if (is.null(fcc)) return(NULL)
  tibble(taxon = sp, run = run, presence = if (!is.null(mds)) mds$presenceRecords else NA, nVars = if (!is.null(vs)) nrow(vs) else NA,
         ATAUC = if (!is.null(auc)) auc$ATAUC else NA, valid = if (!is.null(auc)) auc$Valid else NA,
         GRSex = fce$GRS, ERSex = fce$ERS, FCSex = fce$FCS, FCSex_class = fce$FCS_Score, SRSin = fci$SRS, GRSin = fci$GRS, ERSin = fci$ERS, FCSin = fci$FCS, FCSin_class = fci$FCS_Score,
         FCSc = fcc$FCSc_mean, FCSc_class = fcc$FCSc_mean_class) }
# sampled RSS (MB) per run version from the orchestrator sampler: columns ts, run, rssMB
samp <- if (file.exists("work2026/experiment_rss_samples.csv")) read_csv("work2026/experiment_rss_samples.csv", show_col_types = FALSE) else NULL
# two sampler lines are garbled (overlapping writes); parse ts explicitly and drop what does not parse
if (!is.null(samp)) samp <- samp |> mutate(ts = as.POSIXct(as.character(ts), format = "%Y-%m-%dT%H:%M:%S", tz = "UTC")) |> filter(!is.na(ts))
out <- map_dfr(exps, function(e) {
  te <- readTiming(e); if (is.null(te)) return(NULL)
  sps <- unique(te$taxon)
  tp <- readTiming(prod) |> filter(taxon %in% sps)
  tot <- function(t) t |> filter(step == "total") |> group_by(taxon) |> summarise(seconds = last(seconds), peakMB = last(peakMB), finished = last(finished), .groups = "drop")
  tt <- full_join(tot(tp) |> rename(sec_prod = seconds, peakMB_prod = peakMB, fin_prod = finished), tot(te) |> rename(sec_exp = seconds, peakMB_exp = peakMB, fin_exp = finished), by = "taxon")
  vsel <- function(t) t |> filter(step == "variableSelection") |> group_by(taxon) |> summarise(vs = last(seconds), .groups = "drop")
  tt <- tt |> left_join(vsel(tp) |> rename(vsel_prod = vs), by = "taxon") |> left_join(vsel(te) |> rename(vsel_exp = vs), by = "taxon")
  sc <- map_dfr(sps, function(s) bind_rows(readScores(s, prod), readScores(s, e))) |> mutate(run = if_else(run == prod, "prod", "exp")) |>
    pivot_wider(names_from = run, values_from = -c(taxon, run))
  rssPeak <- function(fin, sec, run) { if (is.null(samp) || is.na(fin)) return(NA); w <- samp |> filter(run == !!run, ts <= as.POSIXct(fin, tz = "UTC") + 60, ts >= as.POSIXct(fin, tz = "UTC") - sec - 60); if (nrow(w)) max(w$rssMB) else NA }
  tt |> left_join(sc, by = "taxon") |> mutate(experiment = e,
    rss_exp = map2_dbl(fin_exp, sec_exp, ~rssPeak(.x, .y, e)),
    classChanged = coalesce(FCSex_class_prod != FCSex_class_exp | FCSin_class_prod != FCSin_class_exp | FCSc_class_prod != FCSc_class_exp, FALSE),
    dFCSc = round(FCSc_exp - FCSc_prod, 1), dFCSex = round(FCSex_exp - FCSex_prod, 1), dFCSin = round(FCSin_exp - FCSin_prod, 1),
    secRatio = round(sec_exp / sec_prod, 2), vselRatio = round(vsel_exp / vsel_prod, 2))
})
write_csv(out, "work2026/experimentSummary_20260917.csv"); write_csv(out, "temp/experimentSummary_20260917.csv")
cat("=== per experiment: species, class changes, runtime ratio (exp/prod), variable-selection ratio ===\n")
print(as.data.frame(out |> group_by(experiment) |> summarise(species = n(), classChanges = sum(classChanged), medSecRatio = median(secRatio, na.rm = TRUE), medVselRatio = median(vselRatio, na.rm = TRUE), maxAbs_dFCSc = max(abs(dFCSc), na.rm = TRUE), peakRSS_MB = max(rss_exp, na.rm = TRUE))), row.names = FALSE)
cat("\n=== per species ===\n")
print(as.data.frame(out |> select(experiment, taxon, presence_prod, presence_exp, nVars_prod, nVars_exp, sec_prod, sec_exp, secRatio, vsel_prod, vsel_exp, rss_exp, FCSc_prod, FCSc_exp, dFCSc, FCSc_class_prod, FCSc_class_exp, classChanged)), row.names = FALSE)
