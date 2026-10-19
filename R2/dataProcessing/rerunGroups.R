# R2/dataProcessing/rerunGroups.R
# Which gap taxa need a new model run after the 2026-10 review, and why. Two triggers:
#   1. records changed: taxaNeedingRerun() compares the fingerprint of the taxon's current
#      records with the run it carries (R2/dataProcessing/taxonRecords.R);
#   2. a state list newly added to data/source_data/FNA_stateClassification.csv (dateAdded on
#      or after sinceDate). The fingerprint is taken before applyFNA() runs, so it cannot see
#      this; without the second trigger munsoniana, rufotomentosa and lincecumii would be
#      skipped although the new list removes points from their models.
# Groups (decision 2026-10-19): A = taxonomy only, re-run as soon as the sheet edits land;
# B = geography, a state list was added (records may have changed as well); untouched = keep
# the current run. A taxon with no state list is modelled unfiltered, as in the published run.
#   source("R2/dataProcessing/taxonRecords.R"); source("R2/dataProcessing/rerunGroups.R")
#   rerunGroups(outPath = "work2026/rerun/rerunGroups_20261019.csv")
rerunGroups <- function(modelDataPath = latestModelData(),
                        fnaPath = "data/source_data/FNA_stateClassification.csv",
                        sinceDate = as.Date("2026-10-19"),
                        outPath = NULL, ...) {
  need <- taxaNeedingRerun(modelDataPath = modelDataPath, ...)
  fna <- readr::read_csv(fnaPath, col_types = readr::cols(.default = "c"), show_col_types = FALSE) |>
    dplyr::transmute(taxon = `Taxon Name`,
                     hasStateList = nchar(`States from FNA`) > 3,
                     stateListNew = hasStateList & !is.na(dateAdded) & as.Date(dateAdded) >= sinceDate,
                     stateListSource = dplyr::if_else(hasStateList, source, NA_character_))
  out <- need |>
    dplyr::left_join(fna, by = "taxon") |>
    dplyr::mutate(hasStateList = dplyr::coalesce(hasStateList, FALSE),
                  stateListNew = dplyr::coalesce(stateListNew, FALSE),
                  group = dplyr::case_when(
                    stateListNew ~ "B: geography",
                    recordsChanged %in% TRUE ~ "A: taxonomy only",
                    is.na(recordsChanged) ~ "no fingerprint for the compared run",
                    TRUE ~ "untouched"),
                  rerun = stateListNew | recordsChanged %in% TRUE,
                  reason = dplyr::case_when(
                    stateListNew & recordsChanged %in% TRUE ~ "state list added and records changed",
                    stateListNew ~ "state list added (records unchanged)",
                    recordsChanged %in% TRUE & hasStateList ~ "records changed; existing state list applies",
                    recordsChanged %in% TRUE ~ "records changed; no state list, modelled unfiltered as published",
                    is.na(recordsChanged) ~ "no fingerprint to compare",
                    TRUE ~ "records identical to the current run")) |>
    dplyr::arrange(group, taxon)
  if (!is.null(outPath)) readr::write_csv(out, outPath)
  cat("\n== rerun groups\n"); print(dplyr::count(out, group, rerun))
  invisible(out)
}
