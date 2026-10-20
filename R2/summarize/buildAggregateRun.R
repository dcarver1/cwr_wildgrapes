###
# Gather one complete run folder from more than one run version.
#
# The all-species summaries (richness maps, protected-area tables, run summary
# document) read a single run version. The taxonomy update holds its results
# in two: the published run for taxa that are unchanged or still under review,
# the update run for taxa that were re-run. buildAggregateRun() copies each
# taxon's folder from the run its row comes from (taxonomyUpdateStatus,
# R2/summarize/aggregateRuns.R) into one aggregate run version, so those
# summaries can be pointed at it like any other run.
#
# Nothing is modelled here and the source runs are not touched. Every copied
# taxon folder holds aggregateSource.csv (status, source run, newest file of
# the source, when it was copied) and file dates are kept from the source. A
# taxon is copied again only when its source run or the source files changed,
# so the function is re-run after each batch of taxa is moved to `updated`.
#
#   source("global.R"); buildAggregateRun()
# (global.R sources every file under R2/, so this file must only define functions)
###
buildAggregateRun <- function(aggregateRun = "runAggregate2026_1k",
                              publishedRun = "run08282025_1k",
                              status = taxonomyUpdateStatus,
                              dir1 = "data/Vitis") {
  gapSpecies <- readr::read_csv("data/New World Vitis.csv", col_types = readr::cols(.default = "c"), show_col_types = FALSE) |>
    dplyr::filter(`Include in gap analysis?` == "Y") |> dplyr::pull(`Scientific Name`) |> sort()
  newest <- function(d) {
    f <- list.files(d, recursive = TRUE, full.names = TRUE)
    if (length(f) == 0) NA_character_ else format(max(file.mtime(f)), "%Y-%m-%dT%H:%M:%S")
  }
  # per-taxon summary document: the published run and the round-2 driver name it differently
  htmlOf <- function(j, run) {
    f <- file.path(dir1, "speciesSummaryHTML", run, paste0(j, c("_Summary_fnaFilter.html", paste0("_Summary_", run, ".html"))))
    f[file.exists(f)][1]
  }
  htmlDir <- file.path(dir1, "speciesSummaryHTML", aggregateRun)
  dir.create(file.path(dir1, aggregateRun), recursive = TRUE, showWarnings = FALSE)
  dir.create(htmlDir, recursive = TRUE, showWarnings = FALSE)

  out <- purrr::map_dfr(gapSpecies, function(j) {
    st <- dplyr::case_when(j %in% status$updated ~ "updated", j %in% status$unchanged ~ "unchanged", TRUE ~ "pending")
    run <- if (st == "updated") unname(status$updatedRun[[j]]) else publishedRun
    src <- file.path(dir1, j, run); dst <- file.path(dir1, j, aggregateRun)
    if (!dir.exists(src)) stop(j, ": no folder for its source run ", run)
    marker <- file.path(dst, "aggregateSource.csv")
    row <- data.frame(taxon = j, status = st, sourceRun = run, sourceNewestFile = newest(src))
    was <- if (file.exists(marker)) readr::read_csv(marker, col_types = readr::cols(.default = "c"), show_col_types = FALSE) else NULL
    current <- !is.null(was) && was$sourceRun == run && identical(was$sourceNewestFile, row$sourceNewestFile)
    if (!current) {
      # only a folder this function made (it holds the marker) is ever replaced
      if (dir.exists(dst) && is.null(was)) stop(dst, " exists and was not made by buildAggregateRun()")
      unlink(dst, recursive = TRUE)
      dir.create(dst)
      ok <- file.copy(list.files(src, full.names = TRUE), dst, recursive = TRUE, copy.date = TRUE)
      if (!all(ok)) stop(j, ": copy from ", src, " failed")
      row$copied <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S")
      readr::write_csv(row, marker)
      message(j, ": copied from ", run, " (", st, ")")
    } else {
      row$copied <- was$copied
    }
    # the summary document, also when it turned up after the folder was copied
    h <- htmlOf(j, run); hOut <- file.path(htmlDir, paste0(j, "_Summary_", aggregateRun, ".html"))
    if (!is.na(h) && (!current || !file.exists(hOut))) file.copy(h, hOut, overwrite = TRUE, copy.date = TRUE)
    row$hasScores <- file.exists(file.path(dst, "gap_analysis/fcs_combined.csv"))
    row$hasSummaryDoc <- file.exists(file.path(htmlDir, paste0(j, "_Summary_", aggregateRun, ".html")))
    row
  })
  readr::write_csv(out, file.path(dir1, aggregateRun, "aggregateSources.csv"))
  print(dplyr::count(out, status, sourceRun, hasScores))
  invisible(out)
}
