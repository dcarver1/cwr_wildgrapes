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
# Move a taxon from pending to updated by adding it to taxonomyUpdateStatus$updated once its
# review is complete and it has been run under `updateRun`.
#
#   source("global.R"); aggregateRuns()
# (global.R sources every file under R2/, so this file must only define functions)
###
# Taxonomy-update status of each taxon; the single list used by the model
# driver (dontRun), aggregateRuns() and taxaNeedingRerun().
#   unchanged   : no record changes from the taxonomy review (no rows in
#                 work2026/taxaReviewAnne/taxonChangeRawData_20260924.xlsx), or
#                 only changes that are not taxonomic. Published results stand.
#   updated     : stable on the review input already received (Anne,
#                 taxonChangeRawData_20260924.xlsx): every record reassigned
#                 between project concepts carries her decision, and the only
#                 unreviewed rows are a handful (<= 10) of records under dropped
#                 or added synonyms. Either re-run under the update run after a
#                 sheet edit (updated) or, when no edit was needed, the round-2
#                 draft run stands (updatedFromDraft; its records are
#                 fingerprint-identical to the current dataset).
#   provisional : records identical to the round-2 draft run (fingerprint check,
#                 taxaNeedingRerun()) but some of its record changes are still
#                 unreviewed; the draft-run result is reported with that caveat
#                 until the review closes (decision 2026-10-11)
#   every other gap taxon is pending
#   The WFO / NatureServe check (work2026/backboneAgreement/) is evidence for
#   the review, not a route into any class.
taxonomyUpdateStatus <- list(
  unchanged = c(
    "Vitis arizonica", "Vitis biformis", "Vitis blancoi", "Vitis bloodworthiana",
    "Vitis bourgaeana", "Vitis californica", "Vitis jaegeriana", "Vitis monticola",
    "Vitis nesbittiana", "Vitis palmata", "Vitis peninsularis", "Vitis rubriflora",
    "Vitis rupestris",
    # decision 2026-10-05: record differences that are not taxonomic do not
    # change a published result. Both taxa appear only on the 'Not taxonomy'
    # tab of nameApprovalChecklist_20261002.xlsx (one driver one-off record each).
    "Vitis girdiana", "Vitis novogranatensis"),
  # review complete as of 2026-10-02 (work2026/taxaReviewAnne/)
  updated = c("Vitis acerifolia", "Vitis baileyana", "Vitis lincecumii", "Vitis shuttleworthii"),
  # decision 2026-10-11: review input already covers every reassignment and the
  # unreviewed residue is <= 10 records, so the draft run stands as updated.
  # berlandieri: 627 of 627 records from V. cinerea agreed, 6 V. cordifolia var.
  # helleri records dropped unreviewed; var. aestivalis: 1 dropped unreviewed;
  # var. cinerea: 8 dropped unreviewed; mustangensis: 1 added unreviewed.
  updatedFromDraft = c(
    "Vitis aestivalis var. aestivalis", "Vitis berlandieri", "Vitis cinerea var. cinerea",
    "Vitis mustangensis"),
  # draft-run results stand provisionally (2026-10-11); pending = popenoei,
  # rufotomentosa, cinerea var. tomentosa
  provisional = c(
    "Vitis aestivalis", "Vitis aestivalis var. bicolor", "Vitis cinerea", "Vitis labrusca",
    "Vitis martineziana", "Vitis munsoniana", "Vitis riparia", "Vitis rotundifolia",
    "Vitis simpsonii", "Vitis tiliifolia", "Vitis vulpina", "Vitis x champinii",
    "Vitis x doaniana", "Vitis x novae-angliae"))

aggregateRuns <- function(publishedRun = "run08282025_1k",
                          updateRun = "run10022026_1k",
                          draftRun = "run09162026_1k",
                          outDir = "data/datasetsForPublication",
                          # the published table of record (sheet 6 of the publication dataset)
                          publishedTable = "data/datasetsForPublication/gapAnalysisSummary-sheet6.csv",
                          unchangedTaxa = taxonomyUpdateStatus$unchanged,
                          updatedTaxa = taxonomyUpdateStatus$updated,
                          draftUpdatedTaxa = taxonomyUpdateStatus$updatedFromDraft,
                          provisionalTaxa = taxonomyUpdateStatus$provisional) {
  gapSpecies <- readr::read_csv("data/New World Vitis.csv", col_types = readr::cols(.default = "c"), show_col_types = FALSE) |>
    dplyr::filter(`Include in gap analysis?` == "Y") |> dplyr::pull(`Scientific Name`) |> sort()

  sources <- dplyr::tibble(taxon = gapSpecies) |>
    dplyr::mutate(status = dplyr::case_when(taxon %in% c(updatedTaxa, draftUpdatedTaxa) ~ "updated",
                                            taxon %in% unchangedTaxa ~ "unchanged",
                                            taxon %in% provisionalTaxa ~ "provisional",
                                            TRUE ~ "pending"),
                  sourceRun = dplyr::case_when(taxon %in% updatedTaxa ~ updateRun,
                                               taxon %in% draftUpdatedTaxa | status == "provisional" ~ draftRun,
                                               TRUE ~ publishedRun),
                  hasRun = file.exists(file.path("data/Vitis", taxon, sourceRun, "gap_analysis/fcs_combined.csv")),
                  # the published run holds counts but no scores for the two taxa
                  # without usable coordinates (novogranatensis, rufotomentosa)
                  hasCounts = file.exists(file.path("data/Vitis", taxon, sourceRun, "occurances/counts.csv")))

  missingUpdate <- dplyr::filter(sources, status %in% c("updated", "provisional"), !hasRun, !taxon %in% c("Vitis cinerea var. tomentosa"))
  if (nrow(missingUpdate) > 0) stop("Listed as updated/provisional but not run under its source run: ", paste(missingUpdate$taxon, collapse = ", "))

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
