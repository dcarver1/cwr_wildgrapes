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
#   Rscript R2/summarize/aggregateRuns.R
###
suppressMessages({library(dplyr); library(readr); library(purrr)})
source("R2/summarize/summaryTable.R")

publishedRun <- "run08282025_1k"
updateRun    <- "run10022026_1k"
outDir       <- "data/datasetsForPublication"

# same list as dontRun in run_round2_20260916.R
unchangedTaxa <- c(
  "Vitis arizonica", "Vitis biformis", "Vitis blancoi", "Vitis bloodworthiana",
  "Vitis bourgaeana", "Vitis californica", "Vitis jaegeriana", "Vitis monticola",
  "Vitis nesbittiana", "Vitis palmata", "Vitis peninsularis", "Vitis rubriflora",
  "Vitis rupestris")
# review complete as of 2026-10-02 (work2026/taxaReviewAnne/)
updatedTaxa <- c("Vitis acerifolia", "Vitis baileyana", "Vitis lincecumii", "Vitis shuttleworthii")

gapSpecies <- read_csv("data/New World Vitis.csv", col_types = cols(.default = "c"), show_col_types = FALSE) |>
  filter(`Include in gap analysis?` == "Y") |> pull(`Scientific Name`) |> sort()

sources <- tibble(taxon = gapSpecies) |>
  mutate(status = case_when(taxon %in% updatedTaxa ~ "updated",
                            taxon %in% unchangedTaxa ~ "unchanged",
                            TRUE ~ "pending"),
         sourceRun = ifelse(status == "updated", updateRun, publishedRun),
         hasRun = file.exists(file.path("data/Vitis", taxon, sourceRun, "occurances/counts.csv")))

missingUpdate <- sources |> filter(status == "updated", !hasRun)
if (nrow(missingUpdate) > 0) stop("Listed as updated but not run under ", updateRun, ": ", paste(missingUpdate$taxon, collapse = ", "))

combined <- pmap_dfr(sources, function(taxon, status, sourceRun, hasRun) {
  row <- if (hasRun) suppressMessages(summaryTable(species = taxon, runVersion = sourceRun)) else tibble(ID = taxon)
  mutate(row, across(everything(), as.character), status = status, sourceRun = ifelse(hasRun, sourceRun, NA))
}) |>
  mutate(across(-c(ID, status, sourceRun, ends_with("category")), ~ suppressWarnings(as.numeric(.x)))) |>
  relocate(status, sourceRun, .after = ID)

stamp <- format(Sys.Date(), "%Y%m%d")
write_csv(combined, file.path(outDir, paste0("gapAnalysisSummary_combined_", stamp, ".csv")))
write_csv(sources, file.path(outDir, paste0("gapAnalysisSummary_sources_", stamp, ".csv")))
print(count(sources, status, sourceRun, hasRun))
