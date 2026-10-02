# work2026/classificationReport_round2.R
# For every species with a run09162026_1k folder: published (run08282025_1k)
# vs round-2 priority classes at three levels (FCS ex situ, FCS in situ,
# FCSc mean), the SRSex class, counts and coordinate changes, model validity,
# and a caveat column. Writes work2026/classificationReport_round2.csv (+ temp/
# share copy) and prints the species whose class changed at any level.
suppressPackageStartupMessages({library(dplyr); library(readr); library(purrr); library(tidyr)})
options(width = 230)
pub <- "run08282025_1k"; new <- if (exists("newRun")) newRun else "run09162026_1k"
cls <- function(x) case_when(is.na(x) ~ NA_character_, x >= 75 ~ "LP", x >= 50 ~ "MP", x >= 25 ~ "HP", TRUE ~ "UP")
readRun <- function(sp, run) {
  b <- file.path("data/Vitis", sp, run); rc <- function(p) if (file.exists(file.path(b, p))) read_csv(file.path(b, p), show_col_types = FALSE) else NULL
  cnt <- rc("occurances/counts.csv"); fce <- rc("gap_analysis/fcs_ex.csv"); fci <- rc("gap_analysis/fcs_in.csv"); fcc <- rc("gap_analysis/fcs_combined.csv"); auc <- rc("results/aucMetrics.csv"); srs <- rc("gap_analysis/srs_ex.csv")
  if (is.null(cnt)) return(NULL)
  tibble(taxon = sp, run = run, records = cnt$totalRecords, coords = cnt$totalUseful, G = cnt$totalGRecords, H = cnt$totalHRecords,
         SRSex = srs$SRS, SRSex_class = cls(srs$SRS),
         FCSex = fce$FCS, FCSex_class = fce$FCS_Score, FCSin = fci$FCS, FCSin_class = fci$FCS_Score,
         FCSc_mean = fcc$FCSc_mean, FCSc_class = fcc$FCSc_mean_class,
         modelValid = if (!is.null(auc)) auc$Valid[1] else NA, modelReason = if (!is.null(auc)) auc$Reason[1] else "no model (buffer or no records)")
}
species <- basename(dirname(list.dirs("data/Vitis", recursive = FALSE) |> (\(d) file.path(d, new))() |> Filter(f = dir.exists)))
species <- species[grepl("^Vitis", species)]
both <- map_dfr(species, function(s) bind_rows(readRun(s, pub), readRun(s, new)))
wide <- both |> mutate(run = if_else(run == pub, "published", "round2")) |>
  pivot_wider(names_from = run, values_from = -c(taxon, run))
rep <- wide |> mutate(
  records_change = records_round2 - records_published, coords_change = coords_round2 - coords_published,
  SRSex_change = round(SRSex_round2 - SRSex_published, 1), FCSex_change = round(FCSex_round2 - FCSex_published, 1),
  FCSin_change = round(FCSin_round2 - FCSin_published, 1), FCSc_change = round(FCSc_mean_round2 - FCSc_mean_published, 1),
  SRSex_classChanged = coalesce(SRSex_class_published != SRSex_class_round2, TRUE),
  FCSex_classChanged = coalesce(FCSex_class_published != FCSex_class_round2, TRUE),
  FCSin_classChanged = coalesce(FCSin_class_published != FCSin_class_round2, TRUE),
  FCSc_classChanged  = coalesce(FCSc_class_published != FCSc_class_round2, TRUE),
  anyClassChanged = SRSex_classChanged | FCSex_classChanged | FCSin_classChanged | FCSc_classChanged,
  caveat = case_when(
    is.na(records_published) ~ "not modelled in the published run (hand-coded)",
    records_round2 == 0 ~ "0 records until the sheet decision on its include name",
    modelValid_round2 %in% FALSE ~ paste0("round-2 model fails robustness rule (", modelReason_round2, "); in situ scores from that model"),
    taxon %in% c("Vitis munsoniana", "Vitis tiliifolia", "Vitis shuttleworthii", "Vitis popenoei", "Vitis aestivalis", "Vitis cinerea") ~ "counts may move again with pending sheet include names",
    TRUE ~ ""))
rep <- rep |> select(taxon, anyClassChanged, records_published, records_round2, records_change, coords_change,
                     SRSex_published, SRSex_round2, SRSex_class_published, SRSex_class_round2,
                     FCSex_published, FCSex_round2, FCSex_class_published, FCSex_class_round2,
                     FCSin_published, FCSin_round2, FCSin_class_published, FCSin_class_round2,
                     FCSc_mean_published, FCSc_mean_round2, FCSc_class_published, FCSc_class_round2,
                     modelValid_published, modelValid_round2, caveat) |>
  mutate(across(where(is.numeric), ~round(.x, 1))) |> arrange(desc(anyClassChanged), taxon)
write_csv(rep, "work2026/classificationReport_round2.csv"); write_csv(rep, "temp/classificationReport_round2.csv")
cat("species with round-2 folders:", nrow(rep), "\n")
cat("\n=== class changed at any level ===\n")
print(as.data.frame(rep |> filter(anyClassChanged) |> select(taxon, records_change, coords_change, SRSex_class_published, SRSex_class_round2, FCSex_class_published, FCSex_class_round2, FCSin_class_published, FCSin_class_round2, FCSc_class_published, FCSc_class_round2, caveat)), row.names = FALSE)
cat("\n=== no class change ===\n")
print(as.data.frame(rep |> filter(!anyClassChanged) |> select(taxon, records_change, coords_change, FCSc_mean_published, FCSc_mean_round2, FCSc_class_round2, caveat)), row.names = FALSE)
