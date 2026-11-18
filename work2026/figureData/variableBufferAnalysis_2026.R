# Gap analysis at five buffer sizes (1, 5, 20, 50, 100 km) under WGS84 and an equal-area
# projection, for the taxa of the 2026 taxonomy update: the table behind supplementary page 12
# ("multiple buffer sizes"). Re-does published/variableBufferAnalysis.R (March 2026, written for
# the reviewer response; its output published/variable_buffer_raw_results.csv, 38 taxa x 2
# projections x 5 sizes = 380 rows, is the published page) on the aggregate run, with the same
# gap-analysis functions and the same steps. Differences from the published script: the taxon
# list is read from the project sheet (V. rufotomentosa is now scored, so it is included;
# V. novogranatensis has no coordinates and is skipped as before); per-taxon results are written
# to <run>/varBuffer5/ instead of into each taxon's results folder; the unfinished summary
# functions at the end of the published script (never part of the SI) are left out.
#
#   Rscript work2026/figureData/variableBufferAnalysis_2026.R [taxon ...]
#     no argument: every gap taxon (skips taxa whose per-taxon file exists); with taxon names:
#     only those, recomputed. Combined table: data/Vitis/<run>/variable_buffer_raw_results.csv
suppressPackageStartupMessages({library(dplyr); library(sf); library(terra); library(purrr); library(readr); library(stringr); library(tidyr)})
source("R2/helperFunctions.R")
sourceFiles(gapAnalysisOnly = TRUE)
source("R2/modeling/cropG_buffer.R")
terraOptions(progress = 0)

ea_proj <- "+proj=cea +lon_0=0 +lat_ts=30 +datum=WGS84 +units=m +no_defs"
runVersion <- "runAggregate2026_1k"
buffer_sizes_km <- c(1, 5, 20, 50, 100)
projections <- c("WGS84", "EqualArea")
outDir <- file.path("data/Vitis", runVersion, "varBuffer5"); dir.create(outDir, showWarnings = FALSE)

args <- commandArgs(trailingOnly = TRUE)
gapSpecies <- read_csv("data/New World Vitis.csv", col_types = cols(.default = "c"), show_col_types = FALSE) |>
  filter(`Include in gap analysis?` == "Y") |> pull(`Scientific Name`) |> sort()
species_list <- if (length(args)) args else gapSpecies
stopifnot(all(species_list %in% gapSpecies))
overwrite <- length(args) > 0

protectedAreas <- terra::rast("data/geospatial_datasets/protectedLands/wdpa_1km_all_.tif")

runTaxon <- function(species) {
  p1 <- file.path("data/Vitis", species, runVersion)
  results_path <- file.path(outDir, paste0(species, ".csv"))
  if (!overwrite && file.exists(results_path)) { message(species, ": exists, kept"); return(read_csv(results_path, show_col_types = FALSE)) }
  if (!file.exists(file.path(p1, "occurances/spatialData.gpkg"))) { message(species, ": no georeferenced records, skipped"); return(NULL) }
  t0 <- Sys.time(); message("\n", species)
  sp1 <- sf::st_read(file.path(p1, "occurances/spatialData.gpkg"), quiet = TRUE)
  counts <- read_csv(file.path(p1, "occurances/counts.csv"), show_col_types = FALSE)
  natArea <- sf::st_read(file.path(p1, "results/naturalArea.gpkg"), quiet = TRUE)
  thres <- terra::rast(file.path(p1, "results/prj_threshold.tif"))
  templateRast <- thres; templateRast[!is.na(templateRast)] <- 1
  rows <- list()
  for (proj in projections) {
    message("  projection ", proj)
    if (proj == "EqualArea") {
      sp1_proj <- sf::st_transform(sp1, crs = ea_proj)
      natArea_proj <- sf::st_transform(natArea, crs = ea_proj)
      thres_proj <- terra::project(thres, ea_proj, method = "near")
      protectedAreas_proj <- terra::crop(protectedAreas, natArea) |> terra::project(thres_proj, method = "near")
      templateRast_proj <- thres_proj; templateRast_proj[!is.na(templateRast_proj)] <- 1
    } else {
      sp1_proj <- sp1; natArea_proj <- natArea; thres_proj <- thres
      protectedAreas_proj <- protectedAreas; templateRast_proj <- templateRast
    }
    # in situ: projection-specific, buffer-independent
    srsin <- srs_insitu(occuranceData = sp1_proj, thres = thres_proj, protectedArea = protectedAreas_proj)
    ersin <- ers_insitu(occuranceData = sp1_proj, nativeArea = natArea_proj, protectedArea = protectedAreas_proj, thres = thres_proj)
    grsin <- grs_insitu(occuranceData = sp1_proj, protectedArea = protectedAreas_proj, thres = thres_proj)
    fcsin <- fcs_insitu(srsin = srsin, grsin = grsin, ersin = ersin, noModel = FALSE)
    srsex <- srs_exsitu(sp_counts = counts)
    for (b_km in buffer_sizes_km) {
      p1_G <- sp1_proj |> dplyr::filter(type == "G")
      if (nrow(p1_G) == 0) {
        g_bufferCrop <- "there are no g points for this species"
      } else {
        g_buff_vect <- terra::vect(p1_G) |> terra::buffer(width = b_km * 1000)
        r1 <- templateRast_proj |> terra::crop(terra::vect(natArea_proj)) |> terra::mask(terra::vect(natArea_proj))
        g_buff_rs <- terra::rasterize(g_buff_vect, r1) |> terra::crop(terra::vect(natArea_proj)) |> terra::mask(terra::vect(natArea_proj))
        g_buff_rs <- terra::resample(g_buff_rs, thres_proj, method = "near") |> terra::crop(thres_proj)
        g_bufferCrop <- cropG_Buffer(ga50 = g_buff_rs, thres = thres_proj)
      }
      grsex <- grs_exsitu(speciesData = sp1_proj, ga50 = g_bufferCrop, thres = thres_proj)
      ersex <- ers_exsitu(speciesData = sp1_proj, thres = thres_proj, natArea = natArea_proj, ga50 = g_bufferCrop)
      fcsex <- fcs_exsitu(srsex = srsex, grsex = grsex, ersex = ersex, noModel = FALSE, gPoints = nrow(p1_G))
      fcsCombined <- fcs_combine(fcsin = fcsin, fcsex = fcsex)
      rows[[length(rows) + 1]] <- data.frame(
        species = species, projection = proj, buffer_km = b_km,
        SRS_ex = srsex$SRS, GRS_ex = grsex$GRS, ERS_ex = ersex$ERS, FCS_ex = fcsex$FCS, FCS_ex_class = fcsex$FCS_Score,
        SRS_in = srsin$SRS, GRS_in = grsin$GRS, ERS_in = ersin$ERS, FCS_in = fcsin$FCS, FCS_in_class = fcsin$FCS_Score,
        FCS_min = fcsCombined$FCSc_min, FCS_min_class = fcsCombined$FCSc_min_class,
        FCS_max = fcsCombined$FCSc_max, FCS_max_class = fcsCombined$FCSc_max_class,
        FCS_mean = fcsCombined$FCSc_mean, FCS_mean_class = fcsCombined$FCSc_mean_class)
    }
  }
  df <- bind_rows(rows)
  # SD and CV over the five buffer sizes, per projection, joined to every row (as published)
  stats <- df |> group_by(species, projection) |>
    summarise(across(c(GRS_ex, ERS_ex, FCS_ex, GRS_in, ERS_in, FCS_in, FCS_min, FCS_max, FCS_mean),
                     list(SD = ~ sd(.x, na.rm = TRUE),
                          CV = ~ ifelse(mean(.x, na.rm = TRUE) == 0, 0, (sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE)) * 100)),
                     .names = "{.col}_{.fn}"), .groups = "drop")
  df <- left_join(df, stats, by = c("species", "projection"))
  write_csv(df, results_path)
  message(sprintf("  %d rows, %.1f min", nrow(df), as.numeric(difftime(Sys.time(), t0, units = "mins"))))
  df
}

all <- map(species_list, runTaxon) |> list_rbind()
if (!length(args)) {
  write_csv(all, file.path("data/Vitis", runVersion, "variable_buffer_raw_results.csv"))
  cat("combined table:", nrow(all), "rows,", n_distinct(all$species), "taxa\n")
}
