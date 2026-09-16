# work2026/compareSRSex_20260916.R
# Re-run generateCounts() + srs_exsitu() (R2/) on the December 2025 and the
# September 2026 model data, exactly as run_all05082026.R builds the species
# subset (cinerea / aestivalis varieties folded in, shuttleworthii outlier
# dropped), and classify SRSex with the FCS priority thresholds used in the
# summary documents (UP < 25, HP < 50, MP < 75, LP >= 75).
# Also pulls the SRSex stored by the last model run (run08282025_1k, or the
# newest run folder present) to confirm the December reproduction.
# taxonomyOnly <- TRUE compares against model_data20260820_taxonomyOnly.csv
# (driver run with enforceBoundingBox = FALSE) and writes the share copies
# temp/srsEx_dec2025_vs_sep2026.csv and temp/rerunCandidates_dec2025_vs_sep2026.csv;
# the default (full pipeline) writes the same names with "_allChanges".
# The rerun-candidate table flags a taxon when SRSex changed or the number of
# georeferenced germplasm (G) records changed; total coordinate pairs are shown
# alongside because the SDM uses all of them.
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
    if (nrow(sd1) == 0) return(tibble(ID = j, NTOTAL = 0, NTOTAL_COORDS = 0, NG = 0, NG_COORDS = 0, NH = 0, NH_COORDS = 0, SRS = NA_real_))
    c1 <- suppressMessages(generateCounts(sd1))
    srs_exsitu(c1) |> select(ID, NTOTAL, NTOTAL_COORDS, NG, NG_COORDS, NH, NH_COORDS, SRS)
  }) |> mutate(run = label)
}

taxonomyOnly <- exists("taxonomyOnly") && taxonomyOnly
newFile <- if (taxonomyOnly) "data/processed_occurrence/model_data20260820_taxonomyOnly.csv" else "data/processed_occurrence/model_data20260820.csv"
tag <- if (taxonomyOnly) "" else "_allChanges"
cat("comparing against:", newFile, "\n")
old <- runSRS("data/processed_occurrence/model_data20251216.csv", "dec2025")
new <- runSRS(newFile, "sep2026")

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
  old |> select(ID, NTOTAL_dec2025 = NTOTAL, coords_dec2025 = NTOTAL_COORDS, NG_dec2025 = NG, NGcoords_dec2025 = NG_COORDS, NH_dec2025 = NH, NHcoords_dec2025 = NH_COORDS, SRSex_dec2025 = SRS),
  new |> select(ID, NTOTAL_sep2026 = NTOTAL, coords_sep2026 = NTOTAL_COORDS, NG_sep2026 = NG, NGcoords_sep2026 = NG_COORDS, NH_sep2026 = NH, NHcoords_sep2026 = NH_COORDS, SRSex_sep2026 = SRS),
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
  mutate(NG_change = NG_sep2026 - NG_dec2025,
         NGcoords_change = NGcoords_sep2026 - NGcoords_dec2025,
         NHcoords_change = NHcoords_sep2026 - NHcoords_dec2025,
         coords_change = coords_sep2026 - coords_dec2025) |>
  select(taxon, NG_dec2025, NH_dec2025, SRSex_dec2025, class_dec2025,
         NG_sep2026, NH_sep2026, SRSex_sep2026, class_sep2026,
         SRSex_change, scoreChanged, classChanged,
         NG_change, NGcoords_dec2025, NGcoords_sep2026, NGcoords_change,
         NHcoords_dec2025, NHcoords_sep2026, NHcoords_change,
         coords_dec2025, coords_sep2026, coords_change,
         storedRun, SRS_stored, dec2025_matches_storedRun, NTOTAL_dec2025, NTOTAL_sep2026) |>
  arrange(desc(classChanged), desc(abs(SRSex_change)), taxon)

write_csv(cmp, paste0("temp/srsEx_dec2025_vs_sep2026", tag, ".csv"))
write_csv(cmp, paste0("work2026/srsEx_comparison_20260916", if (taxonomyOnly) "_taxonomyOnly" else "", ".csv"))

# rerun candidates: SRSex changed, or georeferenced G records changed
rerun <- cmp |>
  mutate(SRSexChanged = scoreChanged,
         gCoordsChanged = coalesce(NGcoords_change != 0, TRUE),
         rerunCandidate = SRSexChanged | gCoordsChanged,
         reason = case_when(
           is.na(SRSex_sep2026) ~ "no records in Sep 2026",
           SRSexChanged & gCoordsChanged ~ "SRSex and G coordinate pairs",
           SRSexChanged ~ "SRSex only",
           gCoordsChanged ~ "G coordinate pairs only",
           TRUE ~ "no change")) |>
  select(taxon, rerunCandidate, reason,
         SRSex_dec2025, SRSex_sep2026, SRSex_change, class_dec2025, class_sep2026, classChanged,
         NG_dec2025, NG_sep2026, NG_change,
         NGcoords_dec2025, NGcoords_sep2026, NGcoords_change,
         NHcoords_dec2025, NHcoords_sep2026, NHcoords_change,
         coords_dec2025, coords_sep2026, coords_change) |>
  arrange(desc(rerunCandidate), desc(classChanged), desc(abs(NGcoords_change)), desc(abs(SRSex_change)), taxon)
write_csv(rerun, paste0("temp/rerunCandidates_dec2025_vs_sep2026", tag, ".csv"))
write_csv(rerun, paste0("work2026/rerunCandidates_20260916", if (taxonomyOnly) "_taxonomyOnly" else "", ".csv"))
cat("\n=== rerun candidates ===\n")
print(as.data.frame(rerun |> filter(rerunCandidate) |> select(taxon, reason, SRSex_dec2025, SRSex_sep2026, class_dec2025, class_sep2026, NG_change, NGcoords_dec2025, NGcoords_sep2026, NGcoords_change, coords_change)))
cat("\nrerun candidates:", sum(rerun$rerunCandidate), " of", nrow(rerun), "\n")
options(width = 220)
cat("=== SRSex comparison (scores that changed) ===\n")
print(as.data.frame(cmp |> filter(scoreChanged) |> select(taxon, NG_dec2025, NH_dec2025, SRSex_dec2025, class_dec2025, NG_sep2026, NH_sep2026, SRSex_sep2026, class_sep2026, SRSex_change, classChanged)))
cat("\n=== class changes ===\n")
print(as.data.frame(cmp |> filter(classChanged) |> select(taxon, SRSex_dec2025, class_dec2025, SRSex_sep2026, class_sep2026)))
cat("\n=== December reproduction vs stored run ===\n")
print(as.data.frame(cmp |> select(taxon, storedRun, SRS_stored, SRSex_dec2025, dec2025_matches_storedRun) |> filter(!dec2025_matches_storedRun)))
cat("\nscores changed:", sum(cmp$scoreChanged), " classes changed:", sum(cmp$classChanged), " of", nrow(cmp), "\n")
