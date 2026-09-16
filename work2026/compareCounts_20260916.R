# work2026/compareCounts_20260916.R
# Phase 1 step 2 (review/GBIF_fix_plan_2026-09-16.md): per-taxon record counts,
# first run of preprocessingUpdates2026_08_20.R (nameSource = "combined")
# against the December 2025 model data. Outputs work2026/changeInCounts_20260916.csv
# and work2026/changeInCounts_bySource_20260916.csv.
suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr)})
old <- read_csv("data/processed_occurrence/model_data20251216.csv", col_types = cols(.default = "c"), progress = FALSE) |>
  mutate(run = "dec2025")
# Two September files: the full pipeline output, and the "_taxonomyOnly" run
# (enforceBoundingBox = FALSE in the driver) whose only differences from
# December are the taxonomy changes. The share copy in temp/ is built from the
# taxonomy-only file (decision 2026-09-16); work2026/ keeps both.
newFile <- if (exists("taxonomyOnly") && taxonomyOnly) "data/processed_occurrence/model_data20260820_taxonomyOnly.csv" else "data/processed_occurrence/model_data20260820.csv"
tag <- if (exists("taxonomyOnly") && taxonomyOnly) "_taxonomyOnly" else ""
cat("comparing against:", newFile, "\n")
new <- read_csv(newFile, col_types = cols(.default = "c"), progress = FALSE) |>
  mutate(run = "sep2026")
cat("rows old:", nrow(old), " new:", nrow(new), "\n")
cat("columns only in old:", setdiff(names(old), names(new)), "\n")
cat("columns only in new:", setdiff(names(new), names(old)), "\n")

both <- bind_rows(old, new) |>
  mutate(hasCoords = !is.na(latitude) & !is.na(longitude),
         isGBIF = databaseSource == "GBIF")

# per taxon: total, with coordinates, GBIF, GBIF with coordinates, G, H
per_taxon <- both |>
  group_by(taxon, run) |>
  summarise(total = n(),
            withCoords = sum(hasCoords),
            gbif = sum(isGBIF, na.rm = TRUE),
            gbifCoords = sum(isGBIF & hasCoords, na.rm = TRUE),
            G = sum(type == "G", na.rm = TRUE),
            H = sum(type == "H", na.rm = TRUE),
            .groups = "drop") |>
  pivot_wider(names_from = run, values_from = c(total, withCoords, gbif, gbifCoords, G, H),
              values_fill = 0) |>
  mutate(total_change = total_sep2026 - total_dec2025,
         withCoords_change = withCoords_sep2026 - withCoords_dec2025,
         gbif_change = gbif_sep2026 - gbif_dec2025,
         gbifCoords_change = gbifCoords_sep2026 - gbifCoords_dec2025) |>
  select(taxon, starts_with("total"), starts_with("withCoords"), starts_with("gbif_"), starts_with("gbifCoords"),
         G_dec2025, G_sep2026, H_dec2025, H_sep2026) |>
  arrange(desc(abs(total_change)))
write_csv(per_taxon, paste0("work2026/changeInCounts_20260916", tag, ".csv"))

# share copy for the co-author (temp/ is gitignored): totals, GBIF-only,
# other-source and coordinate columns; gbifWithCoords dropped on request.
# Taxonomy-only run -> speciesCounts_dec2025_vs_sep2026.csv (the file to share);
# full run -> speciesCounts_dec2025_vs_sep2026_allChanges.csv
share <- per_taxon |> transmute(
  taxon,
  total_dec2025, total_sep2026, total_change,
  gbif_dec2025, gbif_sep2026, gbif_change,
  otherSources_dec2025 = total_dec2025 - gbif_dec2025,
  otherSources_sep2026 = total_sep2026 - gbif_sep2026,
  otherSources_change = otherSources_sep2026 - otherSources_dec2025,
  withCoords_dec2025, withCoords_sep2026, withCoords_change,
  germplasm_dec2025 = G_dec2025, germplasm_sep2026 = G_sep2026,
  germplasm_change = G_sep2026 - G_dec2025,
  herbarium_dec2025 = H_dec2025, herbarium_sep2026 = H_sep2026
) |> arrange(desc(abs(total_change)), taxon)
write_csv(share, if (tag == "") "temp/speciesCounts_dec2025_vs_sep2026_allChanges.csv" else "temp/speciesCounts_dec2025_vs_sep2026.csv")

# per taxon x source
by_source <- both |>
  count(taxon, databaseSource, run) |>
  pivot_wider(names_from = run, values_from = n, values_fill = 0) |>
  mutate(change = sep2026 - dec2025) |>
  filter(change != 0) |>
  arrange(taxon, desc(abs(change)))
write_csv(by_source, paste0("work2026/changeInCounts_bySource_20260916", tag, ".csv"))

options(width = 200)
cat("\n=== taxa with any change (total / with coords / GBIF) ===\n")
print(as.data.frame(per_taxon |> filter(total_change != 0) |>
  select(taxon, total_dec2025, total_sep2026, total_change, withCoords_dec2025, withCoords_sep2026, withCoords_change, gbif_dec2025, gbif_sep2026, gbif_change)))
cat("\n=== taxa only in one run ===\n")
print(as.data.frame(per_taxon |> filter(total_dec2025 == 0 | total_sep2026 == 0) |> select(taxon, total_dec2025, total_sep2026)))
cat("\n=== non-GBIF sources that changed (should be ~none) ===\n")
print(as.data.frame(by_source |> filter(databaseSource != "GBIF" | is.na(databaseSource)) |> group_by(databaseSource) |> summarise(taxa = n(), net = sum(change))))
cat("\n=== totals ===\n")
print(as.data.frame(both |> group_by(run) |> summarise(n = n(), withCoords = sum(hasCoords), gbif = sum(isGBIF, na.rm = TRUE), taxa = n_distinct(taxon))))
