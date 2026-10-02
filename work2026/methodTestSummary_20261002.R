###
# Per-taxon summary of the method tests (thinning, full correlation pruning,
# both) against the baseline run: points in the model, the model validity
# flag (ATAUC >= 0.7, STAUC < 0.15, ASD15 <= 10; R2 calc_sdm_metrics), scores,
# classes and change from baseline. Output: temp/methodTests_20261002/methodTestSummary.csv
#   Rscript work2026/methodTestSummary_20261002.R
###
suppressMessages({library(dplyr); library(readr); library(purrr)})
arms <- c(thin = "run10022026_thin", fullcorr = "run10022026_fullcorr", both = "run10022026_both")
one <- function(taxon, arm, run){
  d <- file.path("data/Vitis", taxon, run)
  f <- function(x) file.path(d, x)
  if (!file.exists(f("gap_analysis/fcs_combined.csv"))) return(NULL)
  rd <- function(x) read_csv(f(x), show_col_types = FALSE)
  auc <- if (file.exists(f("results/aucMetrics.csv"))) rd("results/aucMetrics.csv") else tibble(ATAUC = NA, STAUC = NA, ASD15 = NA, Valid = NA, Reason = "no SDM")
  fin <- rd("gap_analysis/fcs_in.csv"); fex <- rd("gap_analysis/fcs_ex.csv"); fc <- rd("gap_analysis/fcs_combined.csv")
  tibble(taxon = taxon, arm = arm, runVersion = run,
         points_in_model = rd("occurances/modelDataSummary.csv")$presenceRecords[1],
         ATAUC = round(auc$ATAUC[1], 3), STAUC = round(auc$STAUC[1], 3), ASD15 = round(auc$ASD15[1], 2),
         modelValid = as.logical(auc$Valid[1]), validReason = auc$Reason[1],
         model_km2 = round(rd("gap_analysis/grs_in.csv")$SPP_AREA_km2[1]),
         SRSin = fin$SRS, GRSin = fin$GRS, ERSin = fin$ERS, FCSin = fin$FCS, FCSin_class = fin$FCS_Score,
         GRSex = fex$GRS, ERSex = fex$ERS, FCSex = fex$FCS, FCSex_class = fex$FCS_Score,
         FCSc = fc$FCSc_mean, FCSc_class = fc$FCSc_mean_class)
}
taxa <- basename(dirname(Sys.glob("data/Vitis/*/run10022026_thin")))
taxa <- taxa[grepl("^Vitis ", taxa)]
res <- map_dfr(taxa, function(t){
  # a fresh same-day baseline is used where one exists (shuttleworthii)
  baseRun <- if (dir.exists(file.path("data/Vitis", t, "run10022026_base"))) "run10022026_base" else "run09162026_1k"
  b <- one(t, "baseline", baseRun)
  a <- imap_dfr(arms, ~ one(t, .y, .x))
  bind_rows(b, a) |>
    mutate(d_points_pct = round(100 * (points_in_model / b$points_in_model - 1), 1),
           d_area_pct = round(100 * (model_km2 / b$model_km2 - 1), 1),
           dFCSin = round(FCSin - b$FCSin, 1), dFCSex = round(FCSex - b$FCSex, 1), dFCSc = round(FCSc - b$FCSc, 1),
           classChanged = FCSin_class != b$FCSin_class | FCSex_class != b$FCSex_class | FCSc_class != b$FCSc_class)
}) |> mutate(across(c(SRSin:FCSin, GRSex:FCSex, FCSc), ~ round(.x, 1)))
write_csv(res, "temp/methodTests_20261002/methodTestSummary.csv")
print(as.data.frame(select(res, taxon, arm, points_in_model, ATAUC, modelValid, d_area_pct, FCSin, FCSex, FCSc, dFCSc, FCSc_class, classChanged)), row.names = FALSE)
