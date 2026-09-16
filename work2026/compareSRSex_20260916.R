# work2026/compareSRSex_20260916.R
# Re-run generateCounts() + srs_exsitu() (R2/) on the December 2025 and the
# September 2026 model data, exactly as run_all05082026.R builds the species
# subset (cinerea / aestivalis varieties folded in, shuttleworthii outlier
# dropped), and classify SRSex with the FCS priority thresholds used in the
# summary documents (UP < 25, HP < 50, MP < 75, LP >= 75).
# Also pulls the SRSex stored by the last model run (run08282025_1k, or the
# newest run folder present) to confirm the December reproduction.
# Output: temp/srsEx_dec2025_vs_sep2026.csv and work2026/srsEx_comparison_20260916.csv
suppressPackageStartupMessages({library(dplyr); library(readr); library(purrr); library(tidyr)})
source("R2/dataProcessing/generateCounts.R")
source("R2/gapAnalysis/srs_exsitu.R")

classify <- function(x) case_when(is.na(x) ~ NA_character_, x >= 75 ~ "LP", x >= 50 ~ "MP", x >= 25 ~ "HP", TRUE ~ "UP")

speciesSubset <- function(speciesData, j) {
  sd1 <- speciesData |> filter(taxon == j)
  if (j == "Vitis cinerea") sd1 <- speciesData |> filter(taxon %in% c("Vitis cinerea", "Vitis cinerea var. cinerea", "Vitis cinerea var. tomentosa")) |> mutate(taxon = "Vitis cinerea")
  if (j == "Vitis aestivalis") sd1 <- speciesData |> filter(taxon %in% c("Vitis aestivalis", "Vitis aestivalis var. aestivalis", "Vitis aestivalis var. bicolor")) |> mutate(taxon = "Vitis aestivalis")
  if (j == "Vitis shuttleworthii") sd1 <- sd1 |> filter(is.na(longitude) | longitude != "-80.001483")
  sd1
}

runSRS <- function(path, label) {
  d <- read_csv(path, col_types = cols(.default = "c"), progress = FALSE)
  taxa <- sort(unique(d$taxon))
  map_dfr(taxa, function(j) {
    sd1 <- speciesSubset(d, j)
    if (nrow(sd1) == 0) return(tibble(ID = j, NTOTAL = 0, NG = 0, NH = 0, SRS = NA_real_))
    c1 <- suppressMessages(generateCounts(sd1))
    srs_exsitu(c1) |> select(ID, NTOTAL, NG, NH, SRS)
  }) |> mutate(run = label)
}

old <- runSRS("data/processed_occurrence/model_data20251216.csv", "dec2025")
new <- runSRS("data/processed_occurrence/model_data20260820.csv", "sep2026")

# stored values from the last model run, for validation of the December reproduction
stored <- map_dfr(list.dirs("data/Vitis", recursive = FALSE), function(sp) {
  runs <- list.dirs(sp, recursive = FALSE); runs <- runs[grepl("run\\d+_1k$", runs)]
  if (!length(runs)) return(NULL)
  f <- file.path(sort(runs, decreasing = TRUE)[1], "gap_analysis", "srs_ex.csv")
  if (!file.exists(f)) return(NULL)
  read_csv(f, show_col_types = FALSE) |>
    # stale folders exist ("Vitis Bloodworthiana" holds a rufotomentosa file,
    # "Vitis rotundifolia var. munsoniana" holds rotundifolia): keep only files
    # whose ID matches the folder they sit in
    filter(ID == basename(sp)) |>
    transmute(ID, storedRun = basename(dirname(dirname(f))), NG_stored = NG, NH_stored = NH, SRS_stored = SRS)
})

cmp <- full_join(
  old |> select(ID, NTOTAL_dec2025 = NTOTAL, NG_dec2025 = NG, NH_dec2025 = NH, SRSex_dec2025 = SRS),
  new |> select(ID, NTOTAL_sep2026 = NTOTAL, NG_sep2026 = NG, NH_sep2026 = NH, SRSex_sep2026 = SRS),
  by = "ID") |>
  left_join(stored, by = "ID") |>
  mutate(across(c(SRSex_dec2025, SRSex_sep2026, SRS_stored), ~ round(.x, 2)),
         SRSex_change = round(SRSex_sep2026 - SRSex_dec2025, 2),
         class_dec2025 = classify(SRSex_dec2025),
         class_sep2026 = classify(SRSex_sep2026),
         scoreChanged = coalesce(SRSex_change != 0, TRUE),
         classChanged = coalesce(class_dec2025 != class_sep2026, TRUE),
         dec2025_matches_storedRun = coalesce(abs(SRSex_dec2025 - SRS_stored) < 0.01, FALSE)) |>
  rename(taxon = ID) |>
  select(taxon, NG_dec2025, NH_dec2025, SRSex_dec2025, class_dec2025,
         NG_sep2026, NH_sep2026, SRSex_sep2026, class_sep2026,
         SRSex_change, scoreChanged, classChanged,
         storedRun, SRS_stored, dec2025_matches_storedRun, NTOTAL_dec2025, NTOTAL_sep2026) |>
  arrange(desc(classChanged), desc(abs(SRSex_change)), taxon)

write_csv(cmp, "temp/srsEx_dec2025_vs_sep2026.csv")
write_csv(cmp, "work2026/srsEx_comparison_20260916.csv")
options(width = 220)
cat("=== SRSex comparison (scores that changed) ===\n")
print(as.data.frame(cmp |> filter(scoreChanged) |> select(taxon, NG_dec2025, NH_dec2025, SRSex_dec2025, class_dec2025, NG_sep2026, NH_sep2026, SRSex_sep2026, class_sep2026, SRSex_change, classChanged)))
cat("\n=== class changes ===\n")
print(as.data.frame(cmp |> filter(classChanged) |> select(taxon, SRSex_dec2025, class_dec2025, SRSex_sep2026, class_sep2026)))
cat("\n=== December reproduction vs stored run ===\n")
print(as.data.frame(cmp |> select(taxon, storedRun, SRS_stored, SRSex_dec2025, dec2025_matches_storedRun) |> filter(!dec2025_matches_storedRun)))
cat("\nscores changed:", sum(cmp$scoreChanged), " classes changed:", sum(cmp$classChanged), " of", nrow(cmp), "\n")
