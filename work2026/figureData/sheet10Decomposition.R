# Why supplementary page 10 (maximum predicted richness per WDPA polygon, 66,535 rows) changes
# between the published SI and the 18 Nov 2026 package: separates the method change (the pooled
# species V. aestivalis and V. cinerea left out of the richness map) from the taxonomy update (25
# re-fitted models). Four tables, same polygons:
#   A published            data/datasetsForPublication/rastInProtectedAreasUpdate.csv
#   B published, genus-level: published richness map minus the two pooled layers (computed here)
#   C 15 Nov 2026          taxonomy update, pooled species still in (datasetsForPublication_20261115)
#   D 18 Nov 2026          taxonomy update + genus-level (datasetsForPublication_<build>)
#   Rscript work2026/figureData/sheet10Decomposition.R <build> [outDir]
suppressPackageStartupMessages({library(dplyr); library(readr); library(terra); library(sf)})
args <- commandArgs(trailingOnly = TRUE); build <- args[1]
outDir <- if (length(args) >= 2) args[2] else "data"   # the derived table is data, not code
wdpa <- st_read("data/geospatial_datasets/protectedLands/vect_Vitis_run08282025_1k.gpkg", quiet = TRUE)
bFile <- file.path(outDir, "rastInProtectedAreas_publishedGenusLevel.csv")
if (!file.exists(bFile)) {
  rich <- rast("data/Vitis/run08282025_1k/speciesrichness_1km.tif")
  for (sp in c("Vitis aestivalis", "Vitis cinerea")) {
    x <- resample(rast(sprintf("data/Vitis/%s/run08282025_1k/results/prj_threshold.tif", sp)), rich, method = "near") |> classify(cbind(NA, 0))
    rich <- rich - x
  }
  cat("published richness without pooled species: max", minmax(rich)[2], "\n")
  mx <- terra::extract(rich, vect(wdpa), fun = max, na.rm = TRUE, ID = FALSE)[, 1]
  mx[is.infinite(mx) | is.nan(mx)] <- NA
  st_drop_geometry(wdpa) |> left_join(data.frame(id = wdpa$WDPAID, has_raster_data = !is.na(mx), maxValue = mx), by = c("WDPAID" = "id"), relationship = "many-to-many") |>
    write_csv(bFile)
}
rd <- function(f) read_csv(f, show_col_types = FALSE, progress = FALSE) |> mutate(row = row_number()) |> select(row, WDPAID, maxValue)
tabs <- list(A_published = rd("data/datasetsForPublication/rastInProtectedAreasUpdate.csv"),
             B_publishedGenusLevel = rd(bFile),
             C_update20261115 = rd("data/datasetsForPublication_20261115/rastInProtectedAreasUpdate.csv"))
if (!is.na(build) && file.exists(sprintf("data/datasetsForPublication_%s/rastInProtectedAreasUpdate.csv", build)))
  tabs$D_updateGenusLevel <- rd(sprintf("data/datasetsForPublication_%s/rastInProtectedAreasUpdate.csv", build))
stopifnot(length(unique(sapply(tabs, nrow))) == 1)
summ <- function(t) c(rows = nrow(t), withValue = sum(!is.na(t$maxValue) & t$maxValue > 0), max = max(t$maxValue, na.rm = TRUE),
                      atMax = sum(t$maxValue == max(t$maxValue, na.rm = TRUE), na.rm = TRUE), at8plus = sum(t$maxValue >= 8, na.rm = TRUE))
cat("\nper table:\n"); print(t(sapply(tabs, summ)))
cmp <- function(a, b) { d <- b$maxValue - a$maxValue; d[is.na(d)] <- 0
  c(changed = sum(d != 0), up = sum(d > 0), down = sum(d < 0), meanChangeWhereChanged = round(mean(d[d != 0]), 2)) }
cat("\nstep by step (polygons whose maximum changes):\n")
steps <- list("A -> B  method only (published map, pooled species removed)" = c("A_published", "B_publishedGenusLevel"),
              "A -> C  taxonomy only (15 Nov package)" = c("A_published", "C_update20261115"),
              "C -> D  method on top of taxonomy" = c("C_update20261115", "D_updateGenusLevel"),
              "A -> D  both (what the new sheet 10 shows against the published)" = c("A_published", "D_updateGenusLevel"),
              "B -> D  taxonomy only, both genus-level" = c("B_publishedGenusLevel", "D_updateGenusLevel"))
for (n in names(steps)) if (all(steps[[n]] %in% names(tabs))) { cat(n, "\n"); print(cmp(tabs[[steps[[n]][1]]], tabs[[steps[[n]][2]]])) }
