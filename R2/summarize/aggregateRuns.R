###
# Build one gap-analysis summary table from more than one run version.
#
# The taxonomy update only re-runs taxa whose records changed AND whose name
# changes have been approved. Every other taxon keeps its published result.
# This script records, per taxon, which run its row comes from and writes the
# combined table to the publication folder.
#
#   unchanged : no record changes from the taxonomy review -> published run
#   updated   : taxonomy changes approved and re-run        -> update run
#   pending   : taxonomy changes still under review         -> published run,
#               flagged so the row is not mistaken for a final value
#
# Move a taxon from pending to updated by adding it to `updatedTaxa` once its
# review is complete and it has been run under `updateRun`.
#
#   source("global.R"); aggregateRuns()
# (global.R sources every file under R2/, so this file must only define functions)
###
aggregateRuns <- function(publishedRun = "run08282025_1k",
                          updateRun = "run10022026_1k",
                          outDir = "data/datasetsForPublication",
                          # same list as dontRun in run_round2_20260916.R
                          unchangedTaxa = c(
                            "Vitis arizonica", "Vitis biformis", "Vitis blancoi", "Vitis bloodworthiana",
                            "Vitis bourgaeana", "Vitis californica", "Vitis jaegeriana", "Vitis monticola",
                            "Vitis nesbittiana", "Vitis palmata", "Vitis peninsularis", "Vitis rubriflora",
                            "Vitis rupestris"),
                          # review complete as of 2026-10-02 (work2026/taxaReviewAnne/)
                          updatedTaxa = c("Vitis acerifolia", "Vitis baileyana", "Vitis lincecumii", "Vitis shuttleworthii")) {
  gapSpecies <- readr::read_csv("data/New World Vitis.csv", col_types = readr::cols(.default = "c"), show_col_types = FALSE) |>
    dplyr::filter(`Include in gap analysis?` == "Y") |> dplyr::pull(`Scientific Name`) |> sort()

  sources <- dplyr::tibble(taxon = gapSpecies) |>
    dplyr::mutate(status = dplyr::case_when(taxon %in% updatedTaxa ~ "updated",
                                            taxon %in% unchangedTaxa ~ "unchanged",
                                            TRUE ~ "pending"),
                  sourceRun = ifelse(status == "updated", updateRun, publishedRun),
                  hasRun = file.exists(file.path("data/Vitis", taxon, sourceRun, "gap_analysis/fcs_combined.csv")))

  missingUpdate <- dplyr::filter(sources, status == "updated", !hasRun)
  if (nrow(missingUpdate) > 0) stop("Listed as updated but not run under ", updateRun, ": ", paste(missingUpdate$taxon, collapse = ", "))

  combined <- purrr::pmap_dfr(sources, function(taxon, status, sourceRun, hasRun) {
    row <- if (hasRun) suppressMessages(summaryTable(species = taxon, runVersion = sourceRun)) else dplyr::tibble(ID = taxon)
    dplyr::mutate(row, dplyr::across(dplyr::everything(), as.character), status = status, sourceRun = ifelse(hasRun, sourceRun, NA))
  }) |>
    dplyr::mutate(dplyr::across(-c(ID, status, sourceRun, dplyr::ends_with("category")), ~ suppressWarnings(as.numeric(.x)))) |>
    dplyr::relocate(status, sourceRun, .after = ID)

  stamp <- format(Sys.Date(), "%Y%m%d")
  readr::write_csv(combined, file.path(outDir, paste0("gapAnalysisSummary_combined_", stamp, ".csv")))
  readr::write_csv(sources, file.path(outDir, paste0("gapAnalysisSummary_sources_", stamp, ".csv")))
  print(dplyr::count(sources, status, sourceRun, hasRun))
  invisible(combined)
}
