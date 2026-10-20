# Rebuild the publication datasets from the aggregate run of the 2026 taxonomy update, so the
# paper's figures can be reproduced on the corrected results.
#
# Mirrors the three scripts that built data/datasetsForPublication/ for the published run
# (complineDataForPublication .R, gatherAllModelPredictorResults.R, exsituInsitutions.R), which
# are hard-coded to run08282025_1k. Every taxon's files are read from
# data/Vitis/<taxon>/runAggregate2026_1k, the folder buildAggregateRun() fills from the run
# each taxon's result comes from (taxonomyUpdateStatus, R2/summarize/aggregateRuns.R).
# Output: data/datasetsForPublication_20261020/ with the published file names.
#
#   Rscript work2026/figureData/compileFigureData_20261020.R tables
#       sheets 1-7, SDM test conditions, box-plot data, sheet 11 (method table, unchanged)
#   Rscript work2026/figureData/compileFigureData_20261020.R summaries
#       sheets 8-10, the protected-area "Update" tables, the richness and gap rasters, the
#       summary documents. Needs generateRunSummaries() to have run on runAggregate2026_1k.
#   Rscript work2026/figureData/compileFigureData_20261020.R update
#       the two protected-area "Update" tables (points and raster maximum per WDPA polygon)
suppressPackageStartupMessages({library(dplyr); library(readr); library(purrr); library(terra)})
part <- commandArgs(trailingOnly = TRUE)[1]
run <- "runAggregate2026_1k"
out <- "data/datasetsForPublication_20261020"
pubDir <- "data/datasetsForPublication"
modelData <- "data/processed_occurrence/model_data20261019_2303.csv"
dir.create(out, showWarnings = FALSE)
gapSpecies <- read_csv("data/New World Vitis.csv", col_types = cols(.default = "c"), show_col_types = FALSE) |>
  filter(`Include in gap analysis?` == "Y") |> pull(`Scientific Name`) |> sort()
rd <- function(p) read_csv(p, show_col_types = FALSE, progress = FALSE)
cp <- function(from, to) { stopifnot(file.exists(from)); file.copy(from, file.path(out, to), overwrite = TRUE, copy.date = TRUE); cat("  ", to, "<-", from, "\n") }

if (part == "tables") {
  cat("sheet 1: occurrence dataset\n")
  cp(modelData, "allSpeciesOccurrences-sheet1.csv")
  cat("sheet 2: predictor variable names\n")
  cp("data/geospatial_datasets/bioclim_layers/variableNames_072025.csv", "modelVaraible-sheet2.csv")

  cat("sheets 3-5 and SDM test conditions: per-taxon model files\n")
  gather <- function(sp) {
    p1 <- file.path("data/Vitis", sp, run)
    rdIf <- function(f) if (file.exists(f)) rd(f) |> mutate(taxon = sp) |> select(taxon, everything()) else NULL
    md <- rdIf(file.path(p1, "occurances/allmodelData.csv"))
    list(auc = rdIf(file.path(p1, "results/evaluationTable.csv")),
         varSelection = rdIf(file.path(p1, "occurances/topVariablesData.csv")),
         modelData = if (is.null(md)) NULL else md |> filter(presence == 1) |> select(-any_of("geometry")),
         boxPlot = if (is.null(md)) NULL else md |> filter(presence == 1) |> select(-taxon, everything(), taxon),
         aucMetrics = rdIf(file.path(p1, "results/aucMetrics.csv")))
  }
  res <- map(gapSpecies, gather)
  bind <- function(el) map(res, el) |> list_rbind()
  write_csv(bind("modelData"), file.path(out, "modelData-sheet3.csv"))
  # the published sheet 4 left out V. jaegeriana (buffer method, no variable selection)
  write_csv(bind("varSelection") |> filter(taxon != "Vitis jaegeriana"), file.path(out, "variableSelections-sheet4.csv"))
  write_csv(bind("auc"), file.path(out, "modelEvaluations-sheet5.csv"))
  write_csv(bind("aucMetrics"), file.path(out, "SDM_testConditions.csv"))
  # the driver's box-plot input: presence rows of allmodelData.csv with the geometry column, taxon last
  write_csv(bind("boxPlot"), file.path(out, "environementalDataForBoxPlots.csv"))
  for (f in c("modelData-sheet3.csv", "variableSelections-sheet4.csv", "modelEvaluations-sheet5.csv", "SDM_testConditions.csv", "environementalDataForBoxPlots.csv"))
    cat("  ", f, ":", nrow(rd(file.path(out, f))), "rows,", n_distinct(rd(file.path(out, f))$taxon), "taxa\n")

  cat("sheet 6: gap analysis summary (combined table from aggregateRuns(), with status and source run)\n")
  cp(file.path(pubDir, "gapAnalysisSummary_combined_20261020.csv"), "gapAnalysisSummary-sheet6.csv")
  cp(file.path(pubDir, "gapAnalysisSummary_sources_20261020.csv"), "gapAnalysisSummary-sources.csv")

  cat("sheet 7: ex situ collections by database source and institution\n")
  g <- read_csv(modelData, col_types = cols(.default = "c"), show_col_types = FALSE, progress = FALSE) |> filter(type == "G")
  g |> group_by(databaseSource, institutionCode) |>
    summarise(unique_taxa = paste(unique(taxon), collapse = ", "), total_records = n(),
              n_unique_species = n_distinct(species), .groups = "drop") |>
    write_csv(file.path(out, "exsituCollectionSummary-sheet7.csv"))

  cat("sheet 11: variable buffer size method (unchanged, copied from the published folder)\n")
  cp(file.path(pubDir, "multipleBufferSizes-sheet11.csv"), "multipleBufferSizes-sheet11.csv")
}

if (part == "summaries") {
  agg <- file.path("data/Vitis", run)
  cat("sheets 8-10: protected areas\n")
  cp(file.path(agg, "protectedAreaSpeciesPointRichness.csv"), "pointsInProtectedAreas-sheet8.csv")
  cp(file.path(agg, "protectedAreaSpeciesRasterRichness_1km.csv"), "predictedInProtectedAreas-sheet9.csv")
  cp(file.path(agg, "topProArea.csv"), "optimalProtectedAreas-sheet10.csv")
  cat("richness and gap layers\n")
  for (f in c("speciesrichness_1km.tif", "speciesUsed_speciesrichness.csv", "ga50_speciesrichness_1km.tif",
              "ga50speciesUsed_speciesrichness.csv", "ersexRichness.tif", "ersex_speciesUsed_Richness.csv",
              "ersinRichness.tif", "ersin_speciesUsed_Richness.tif", "aggregateSources.csv")) cp(file.path(agg, f), f)
  cp(paste0("data/Vitis/summaryTable_", run, ".csv"), paste0("summaryTable_", run, ".csv"))
  cp(paste0("data/Vitis/", run, "_Summary.html"), paste0(run, "_Summary.html"))
  bp <- paste0("data/Vitis/", run, "_boxPlotSummary.html"); if (file.exists(bp)) cp(bp, basename(bp))

}

if (part == "update") {
  # Protected-area "Update" tables (second half of complineDataForPublication .R, on the
  # aggregate run). The published script tested every polygon against every point; done as
  # one dense relate() matrix (64,078 x 58,299) that took > 50 GB and did not finish, so the
  # point test is a sparse sf::st_intersects() join (an index lookup, a few minutes) and the
  # raster maximum one terra::extract() over all polygons. The published tables join the
  # per-polygon result back on WDPAID, which is not unique in the layer (58,299 polygons,
  # 57,461 ids), so they hold 66,535 rows; the same join is kept here so the shape matches.
  suppressPackageStartupMessages(library(sf))
  cat("protected-area 'Update' tables\n")
  pts <- list.files("data/Vitis", pattern = "spatialData.gpkg", full.names = TRUE, recursive = TRUE)
  pts <- pts[grepl(paste0(run, "/occurances/spatialData.gpkg"), pts, fixed = TRUE)]
  pts <- pts[basename(dirname(dirname(dirname(pts)))) %in% gapSpecies]
  vitis <- do.call(rbind, lapply(pts, function(p) st_read(p, quiet = TRUE)[, "taxon"]))
  wdpa <- st_read("data/geospatial_datasets/protectedLands/vect_Vitis_run08282025_1k.gpkg", quiet = TRUE)
  st_crs(vitis) <- st_crs(wdpa)
  cat("  ", nrow(vitis), "points,", nrow(wdpa), "protected areas,", n_distinct(wdpa$WDPAID), "distinct WDPAID\n")
  sf_use_s2(FALSE)
  hit <- st_intersects(wdpa, vitis)                      # list: polygon -> point indices
  pointsDF <- map_dfr(seq_along(hit), function(i) {
    k <- hit[[i]]; sp <- unique(vitis$taxon[k])
    data.frame(id = wdpa$WDPAID[i], any_present = length(k) > 0, totalNumber = length(k), ntaxa = length(sp),
               taxa = if (length(k)) paste(sort(sp), collapse = "; ") else NA)
  })
  st_drop_geometry(wdpa) |> left_join(pointsDF, by = c("WDPAID" = "id"), relationship = "many-to-many") |>
    write_csv(file.path(out, "pointsInProtectedAreasUpdate.csv"))
  cat("   pointsInProtectedAreasUpdate.csv:", sum(pointsDF$any_present), "polygons with points\n")
  r <- rast(file.path("data/Vitis", run, "speciesrichness_1km.tif"))
  mx <- terra::extract(r, vect(wdpa), fun = max, na.rm = TRUE, ID = FALSE)[, 1]
  mx[is.infinite(mx) | is.nan(mx)] <- NA
  data.frame(id = wdpa$WDPAID, has_raster_data = !is.na(mx), maxValue = mx) -> rastDF
  st_drop_geometry(wdpa) |> left_join(rastDF, by = c("WDPAID" = "id"), relationship = "many-to-many") |>
    write_csv(file.path(out, "rastInProtectedAreasUpdate.csv"))
  cat("   rastInProtectedAreasUpdate.csv:", sum(rastDF$has_raster_data), "polygons with raster cells\n")
}
cat("done:", part, "\n")
