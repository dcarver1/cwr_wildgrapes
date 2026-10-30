# Rebuild the all-taxa products of the aggregate run after one or more taxa were re-run.
#
# The aggregate run (data/Vitis/<taxon>/runAggregate2026_1k) holds each gap taxon's files from
# the run its result comes from (taxonomyUpdateStatus, R2/summarize/aggregateRuns.R). When a
# taxon is re-run and its entry there changes, everything summed or selected over all taxa has
# to be rebuilt. These are the steps the 20 Oct 2026 build did by hand, in the order they
# depend on each other:
#
#   aggregate  buildAggregateRun(): refresh the aggregate folder of any taxon whose source run
#              changed; aggregateRuns(): combined gap-analysis table (published values for the
#              unchanged taxa) -> data/datasetsForPublication/gapAnalysisSummary_combined_<stamp>.csv;
#              summaryTable() on the aggregate folder -> data/Vitis/summaryTable_<run>.csv
#   rasters    generateRunSummaries(overwrite = TRUE): richness, ga50, ERS ex / in richness
#              rasters, points and max richness per protected area, and a first render of the
#              summary document (with the previous protected-area richness table, replaced below).
#              protectedAreaPoints() keeps any per-taxon file already in <run>/proPoints/, so
#              that folder is emptied first: the 20 Oct build inherited files dated 5 Oct 2026
#              there and its sheets 8 and 10 were made from them.
#   points     the same per-taxon point files and the points-per-protected-area table only
#              (<run>/proPoints/ emptied, then generateRunSummaries(overwrite = FALSE), which
#              rebuilds what is missing and renders the summary document)
#   proAreas   protectedAreaRichness.R: max predicted richness for every WDPA polygon of the
#              range countries, the table the summary document and the optimal-areas selection
#              read; generateRunSummaries() does not write it any more
#   render     generateRunSummaries(overwrite = FALSE): render the summary document again with
#              the fresh table; keep the two geographic gap maps it writes
#   optimal    calculate_optimal_protected_areas(): topProArea.csv (greedy selection, 20 seeds)
#   boxplots   predictor box plots per taxon from the presence rows of allmodelData.csv
#
# The summary document writes grsEX_map.tif / grsIn_map.tif into data/datasetsForPublication/
# (hard-coded in runSummary_editsSGCK.Rmd). The published files are backed up before any render
# and restored afterwards; the new maps are kept beside the other layers in data/Vitis/<run>/.
#
#   Rscript work2026/figureData/rebuildAggregateSummaries.R [stamp YYYYMMDD] [steps, comma separated]
#   e.g. Rscript work2026/figureData/rebuildAggregateSummaries.R 20261029
#        Rscript work2026/figureData/rebuildAggregateSummaries.R 20261029 points,optimal
args <- commandArgs(trailingOnly = TRUE)
stamp <- if (length(args) >= 1 && nzchar(args[1])) args[1] else format(Sys.Date(), "%Y%m%d")
allSteps <- c("aggregate", "rasters", "points", "proAreas", "render", "optimal", "boxplots")
steps <- if (length(args) >= 2) strsplit(args[2], ",")[[1]] else allSteps
stopifnot(all(steps %in% allSteps))
run <- "runAggregate2026_1k"; dir1 <- "data/Vitis"; genus <- "Vitis"
wdpaVectPath <- "data/geospatial_datasets/protectedLands/vect_Vitis_run08282025_1k.gpkg"
pubDir <- "data/datasetsForPublication"
say <- function(...) cat(format(Sys.time(), "%H:%M:%S"), ..., "\n")

source("global.R")
gapSpecies <- readr::read_csv("data/New World Vitis.csv", col_types = readr::cols(.default = "c"), show_col_types = FALSE) |>
  dplyr::filter(`Include in gap analysis?` == "Y") |> dplyr::pull(`Scientific Name`) |> sort()

# The two rasters the summary document overwrites in the published folder. A copy of the
# published files is kept beside them (made once, never overwritten); every render is wrapped
# so the published files are put back even when the render fails. (on.exit() is not usable
# here: this is a script, not a function.)
grsMaps <- c("grsEX_map.tif", "grsIn_map.tif")
keepDir <- file.path(pubDir, ".published_grsMaps_backup")
withPublishedProtected <- function(expr) {
  if (!dir.exists(keepDir)) { dir.create(keepDir); file.copy(file.path(pubDir, grsMaps), keepDir, copy.date = TRUE) }
  stopifnot(all(file.exists(file.path(keepDir, grsMaps))))
  tryCatch(expr, finally = {
    # the fresh maps go to the aggregate folder, the published ones back where they were
    file.copy(file.path(pubDir, grsMaps), file.path(dir1, run, grsMaps), overwrite = TRUE, copy.date = TRUE)
    file.copy(file.path(keepDir, grsMaps), file.path(pubDir, grsMaps), overwrite = TRUE, copy.date = TRUE)
    say("new gap maps kept in", file.path(dir1, run), "; published ones restored from", keepDir)
  })
}
proPoints <- file.path(dir1, run, "proPoints")
pointsTable <- file.path(dir1, run, "protectedAreaSpeciesPointRichness.csv")

if ("aggregate" %in% steps) {
  say("aggregate: buildAggregateRun()")
  buildAggregateRun(aggregateRun = run)
  say("aggregate: aggregateRuns()")
  aggregateRuns(outDir = pubDir)
  say("aggregate: summaryTable()")
  summaryCSV <- summaryTable(species = gapSpecies, runVersion = run)
  readr::write_csv(summaryCSV, file.path(dir1, paste0("summaryTable_", run, ".csv")))
}

if ("rasters" %in% steps) {
  say("rasters: generateRunSummaries(overwrite = TRUE); emptying", proPoints)
  unlink(proPoints, recursive = TRUE); unlink(pointsTable)
  withPublishedProtected(
    generateRunSummaries(dir1 = dir1, runVersion = run, species = gapSpecies, genus = genus,
                         protectedAreas = protectedAreas, overwrite = TRUE, wdpaVectPath = wdpaVectPath))
}

if ("points" %in% steps && !"rasters" %in% steps) {
  say("points: per-taxon protected-area point files; emptying", proPoints)
  unlink(proPoints, recursive = TRUE); unlink(pointsTable)
  withPublishedProtected(
    generateRunSummaries(dir1 = dir1, runVersion = run, species = gapSpecies, genus = genus,
                         protectedAreas = protectedAreas, overwrite = FALSE, wdpaVectPath = wdpaVectPath))
}

if ("proAreas" %in% steps) {
  say("proAreas: protectedAreaRichness.R")
  st <- system2("Rscript", c("work2026/figureData/protectedAreaRichness.R", run))
  if (st != 0) stop("protectedAreaRichness.R failed")
}

if ("render" %in% steps) {
  say("render: generateRunSummaries(overwrite = FALSE)")
  withPublishedProtected(
    generateRunSummaries(dir1 = dir1, runVersion = run, species = gapSpecies, genus = genus,
                         protectedAreas = protectedAreas, overwrite = FALSE, wdpaVectPath = wdpaVectPath))
}

if ("optimal" %in% steps) {
  say("optimal: calculate_optimal_protected_areas()")
  calculate_optimal_protected_areas(runVersion = run, dir1 = dir1)
}

if ("boxplots" %in% steps) {
  say("boxplots: boxplotSummaries_editsSGCK.Rmd")
  df4 <- purrr::map_dfr(gapSpecies, function(sp) {
    f <- file.path(dir1, sp, run, "occurances/allmodelData.csv")
    if (!file.exists(f)) return(NULL)
    read.csv(f) |> dplyr::filter(presence == 1) |> dplyr::mutate(taxon = sp)
  })
  inputData <- list(data = df4, species = gapSpecies, names = bioNames)
  rmarkdown::render(input = "R2/summarize/boxplotSummaries_editsSGCK.Rmd", output_format = "html_document",
                    output_dir = dir1, output_file = paste0(run, "_boxPlotSummary.html"),
                    params = list(inputData = inputData), envir = new.env(parent = globalenv()))
}
say("done:", paste(steps, collapse = ", "), "stamp", stamp)
