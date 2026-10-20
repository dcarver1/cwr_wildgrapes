###
# Build one gap-analysis summary table from more than one run version.
#
# The taxonomy update only re-runs taxa whose records changed AND whose name
# changes have been approved. Every other taxon keeps its published result.
# This script records, per taxon, which run its row comes from and writes the
# combined table to the publication folder.
#
#   unchanged : no record changes from the taxonomy review -> published run
#   updated   : taxonomy changes approved and run           -> the run named for
#               the taxon in taxonomyUpdateStatus$updatedRun
#   pending   : taxonomy changes still under review         -> published run,
#               flagged so the row is not mistaken for a final value
#
# Move a taxon from pending to updated by adding it to the run list in
# taxonomyUpdateStatus once its review is complete and it has been run there.
#
#   source("global.R"); aggregateRuns()
# (global.R sources every file under R2/, so this file must only define functions)
###
# Taxonomy-update status of each taxon; the single list used by the model
# driver (dontRun), aggregateRuns(), buildAggregateRun() and taxaNeedingRerun().
#   unchanged : no record changes from the taxonomy review (no rows in the
#               reviewer's workbook, work2026/taxaReviewAnne/
#               taxonChangeRawData_20260924v2.xlsx), or only changes that are
#               not taxonomic. Published results stand.
#   updated   : records changed with the review and the result comes from the
#               run named in updatedRun. The review closed on 2026-10-19 (every
#               row of the v2 workbook carries a decision; the rows marked
#               Unclear/Review were settled by project decisions the same day,
#               reviewDecisions_20261019.md). A taxon was re-run only when its
#               record fingerprint changed or a state range list was added
#               (rerunGroups(), work2026/rerun/rerunGroups_20261019.csv);
#               otherwise the earlier run whose records are fingerprint-identical
#               to the current dataset stands.
#   every other gap taxon is pending (none as of 2026-10-20)
#   The WFO / NatureServe check (work2026/backboneAgreement/) is evidence for
#   the review, not a route into any class.
taxonomyUpdateStatus <- local({
  updatedByRun <- list(
    # re-run 2026-10-19/20 on model_data20261019_2303.csv: 12 taxa whose records
    # changed with the final sheet edits (group A) and the 5 elevated taxa that
    # received a state range list (group B: baileyana, lincecumii, munsoniana,
    # rufotomentosa, simpsonii)
    run10192026_1k = c(
      "Vitis aestivalis", "Vitis aestivalis var. bicolor", "Vitis baileyana", "Vitis berlandieri",
      "Vitis cinerea", "Vitis cinerea var. cinerea", "Vitis cinerea var. tomentosa", "Vitis labrusca",
      "Vitis lincecumii", "Vitis munsoniana", "Vitis popenoei", "Vitis riparia", "Vitis rotundifolia",
      "Vitis rufotomentosa", "Vitis simpsonii", "Vitis tiliifolia", "Vitis vulpina"),
    # re-run 2026-10-02 after the first sheet edits; records unchanged since
    run10022026_1k = c("Vitis acerifolia", "Vitis shuttleworthii"),
    # round-2 draft run (2026-09-16) stands: records fingerprint-identical to
    # the current dataset, every record change reviewed
    run09162026_1k = c(
      "Vitis aestivalis var. aestivalis", "Vitis martineziana", "Vitis mustangensis",
      "Vitis x champinii", "Vitis x doaniana", "Vitis x novae-angliae"))
  updatedRun <- unlist(lapply(names(updatedByRun), function(r) stats::setNames(rep(r, length(updatedByRun[[r]])), updatedByRun[[r]])))
  list(
    unchanged = c(
      "Vitis arizonica", "Vitis biformis", "Vitis blancoi", "Vitis bloodworthiana",
      "Vitis bourgaeana", "Vitis californica", "Vitis jaegeriana", "Vitis monticola",
      "Vitis nesbittiana", "Vitis palmata", "Vitis peninsularis", "Vitis rubriflora",
      "Vitis rupestris",
      # decision 2026-10-05: record differences that are not taxonomic do not
      # change a published result. Both taxa appear only on the 'Not taxonomy'
      # tab of nameApprovalChecklist_20261002.xlsx (one driver one-off record each).
      "Vitis girdiana", "Vitis novogranatensis"),
    updated = names(updatedRun),
    # named character vector: taxon -> run its result comes from
    updatedRun = updatedRun)
})

aggregateRuns <- function(publishedRun = "run08282025_1k",
                          outDir = "data/datasetsForPublication",
                          # the published table of record (sheet 6 of the publication dataset)
                          publishedTable = "data/datasetsForPublication/gapAnalysisSummary-sheet6.csv",
                          unchangedTaxa = taxonomyUpdateStatus$unchanged,
                          updatedRun = taxonomyUpdateStatus$updatedRun) {
  gapSpecies <- readr::read_csv("data/New World Vitis.csv", col_types = readr::cols(.default = "c"), show_col_types = FALSE) |>
    dplyr::filter(`Include in gap analysis?` == "Y") |> dplyr::pull(`Scientific Name`) |> sort()

  sources <- dplyr::tibble(taxon = gapSpecies) |>
    dplyr::mutate(status = dplyr::case_when(taxon %in% names(updatedRun) ~ "updated",
                                            taxon %in% unchangedTaxa ~ "unchanged",
                                            TRUE ~ "pending"),
                  sourceRun = ifelse(status == "updated", unname(updatedRun[taxon]), publishedRun),
                  hasRun = file.exists(file.path("data/Vitis", taxon, sourceRun, "gap_analysis/fcs_combined.csv")),
                  # the published run holds counts but no scores for the two taxa
                  # without usable coordinates (novogranatensis, rufotomentosa)
                  hasCounts = file.exists(file.path("data/Vitis", taxon, sourceRun, "occurances/counts.csv")))

  missingUpdate <- dplyr::filter(sources, status == "updated", !hasRun)
  if (nrow(missingUpdate) > 0) stop("Listed as updated but not run under its source run: ", paste(missingUpdate$taxon, collapse = ", "))

  # summaryTable() returns the counts with empty scores when a run has no score
  # files, which is how those taxa appear in the published table (sheet 6)
  combined <- purrr::pmap_dfr(sources, function(taxon, status, sourceRun, hasRun, hasCounts) {
    row <- if (hasCounts) suppressMessages(summaryTable(species = taxon, runVersion = sourceRun)) else dplyr::tibble(ID = taxon)
    dplyr::mutate(row, dplyr::across(dplyr::everything(), as.character), status = status, sourceRun = ifelse(hasCounts, sourceRun, NA))
  }) |>
    dplyr::mutate(dplyr::across(-c(ID, status, sourceRun, dplyr::ends_with("category")), ~ suppressWarnings(as.numeric(.x)))) |>
    dplyr::relocate(status, sourceRun, .after = ID)

  # An unchanged taxon is reported exactly as published. Parts of the published
  # run folder were re-run after publication, so its files are not always the
  # published values (blancoi SRSex, girdiana record count): the published
  # table wins, and every cell it replaces is listed.
  pub <- readr::read_csv(publishedTable, show_col_types = FALSE)
  cols <- setdiff(intersect(names(combined), names(pub)), "ID")
  for (j in combined$ID[combined$status == "unchanged" & combined$ID %in% pub$ID]) {
    i <- which(combined$ID == j); k <- which(pub$ID == j)
    now <- unlist(lapply(combined[i, cols], as.character)); was <- unlist(lapply(pub[k, cols], as.character))
    num <- suppressWarnings(as.numeric(now)); numWas <- suppressWarnings(as.numeric(was))
    differs <- ifelse(!is.na(num) & !is.na(numWas), abs(num - numWas) > 1e-6, !(is.na(now) & is.na(was)) & (is.na(now) | is.na(was) | now != was))
    if (any(differs)) {
      message(j, ": run folder differs from the published table, published values used: ",
              paste0(cols[differs], " ", now[differs], " -> ", was[differs], collapse = "; "))
      combined[i, cols] <- pub[k, cols]
    }
  }

  stamp <- format(Sys.Date(), "%Y%m%d")
  readr::write_csv(combined, file.path(outDir, paste0("gapAnalysisSummary_combined_", stamp, ".csv")))
  readr::write_csv(sources, file.path(outDir, paste0("gapAnalysisSummary_sources_", stamp, ".csv")))
  print(dplyr::count(sources, status, sourceRun, hasRun))
  invisible(combined)
}
