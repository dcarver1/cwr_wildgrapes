# Max predicted taxon richness per WDPA polygon for the aggregate run: writes
# data/Vitis/<run>/protectedAreaSpeciesRichness.csv, the table the run summary document
# (protected areas table) and calculate_optimal_protected_areas() read. The published run
# made it with protectedAreaRichness() (R2/summarize/richnessPerProtectedArea.R) when that
# function still wrote its result; the current version returns nothing, so the same steps
# are repeated here without changing R2/.
#   Rscript work2026/figureData/protectedAreaRichness.R [runVersion]
suppressPackageStartupMessages({library(dplyr); library(sf); library(terra); library(readr)})
run <- commandArgs(trailingOnly = TRUE)[1]; if (is.na(run)) run <- "runAggregate2026_1k"
r1 <- rast(file.path("data/Vitis", run, "speciesrichness_1km.tif"))
c1 <- vect("data/geospatial_datasets/countries/ne_10m_admin_0_countries.gpkg")
ex1 <- terra::extract(r1, c1, mean, na.rm = TRUE) |> filter(!is.na(Threshold), Threshold != 0)
id1 <- unique(c1$ISO_A3[ex1$ID]); id1 <- id1[id1 != "-99"]
cat("countries with predicted richness:", paste(id1, collapse = " "), "\n")
files <- list.files("data/geospatial_datasets/protectedLands/WDPA_Mar2023_Public_shp", pattern = "\\.gpkg$", full.names = TRUE, recursive = TRUE)
maxSpecies <- bind_rows(lapply(files, function(f) {
  s1 <- st_read(f, quiet = TRUE) |> filter(MARINE %in% c("0", "1"), ISO3 %in% id1)
  cat(basename(dirname(f)), ":", nrow(s1), "polygons\n")
  ex2 <- terra::extract(r1, vect(s1), max, na.rm = TRUE)
  s1 |> st_drop_geometry() |> select(WDPAID, NAME, ORIG_NAME, DESIG_ENG, ISO3) |> mutate(totalSpecies = ex2$Threshold)
}))
maxSpecies$totalSpecies[is.infinite(maxSpecies$totalSpecies)] <- NA
out <- file.path("data/Vitis", run, "protectedAreaSpeciesRichness.csv")
write_csv(maxSpecies, out)
cat("wrote", out, ":", nrow(maxSpecies), "rows;", sum(maxSpecies$totalSpecies > 0, na.rm = TRUE), "with predicted taxa\n")
