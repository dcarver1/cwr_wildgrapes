###
# One-off (2026-10-02): store input fingerprints in run folders made before
# the driver recorded them, so taxaNeedingRerun() has something to compare.
#   run09162026_1k : every taxon, input model_data20260820_taxonomyOnly.csv
#   run10022026_1k : the four updated taxa, input model_data20261002.csv
#   run08282025_1k : the 13 unchanged taxa. The published run read the
#     publication file, which is not kept as one input; for these taxa its
#     records equal the round-2 input (change = 0 in
#     work2026/publication_vs_round2_byTaxon.csv, checked below), so the
#     round-2 input stands in for it.
#   Rscript work2026/backfillFingerprints_20261002.R
###
source("global.R")
rd <- function(f) readr::read_csv(f, show_col_types = FALSE)
r2 <- "data/processed_occurrence/model_data20260820_taxonomyOnly.csv"
up <- "data/processed_occurrence/model_data20261002.csv"
d2 <- readModelDataText(r2); dU <- readModelDataText(up)
pub <- rd("work2026/publication_vs_round2_byTaxon.csv")
# Unchanged by decision, not by record identity (2026-10-05): the only
# differences from the publication file are not taxonomic, so the published
# result stands and the current input is the baseline for later checks.
nonTaxonomic <- c("Vitis girdiana" = "one record removed (dec16Driver)",
                  "Vitis novogranatensis" = "one record removed and one added (dec16Driver)")
bad <- pub |> dplyr::filter(taxon %in% setdiff(taxonomyUpdateStatus$unchanged, names(nonTaxonomic)), change != 0)
if (nrow(bad) > 0) stop("publication and round-2 records differ for: ", paste(bad$taxon, collapse = ", "))
gap <- readr::read_csv("data/New World Vitis.csv", col_types = readr::cols(.default = "c"), show_col_types = FALSE) |>
  dplyr::filter(`Include in gap analysis?` == "Y") |> dplyr::pull(`Scientific Name`)
for (j in gap) {
  f <- file.path("data/Vitis", j, "run09162026_1k")
  if (dir.exists(f)) writeFingerprint(taxonFingerprint(taxonRecords(d2, j), j, r2), f, "backfilled 2026-10-02")
}
for (j in taxonomyUpdateStatus$updated) {
  writeFingerprint(taxonFingerprint(taxonRecords(dU, j), j, up), file.path("data/Vitis", j, "run10022026_1k"), "backfilled 2026-10-02")
}
for (j in setdiff(taxonomyUpdateStatus$unchanged, names(nonTaxonomic))) {
  writeFingerprint(taxonFingerprint(taxonRecords(d2, j), j, r2), file.path("data/Vitis", j, "run08282025_1k"),
                   "backfilled 2026-10-02 from the round-2 input: publication records identical for this taxon")
}
for (j in names(nonTaxonomic)) {
  writeFingerprint(taxonFingerprint(taxonRecords(dU, j), j, up), file.path("data/Vitis", j, "run08282025_1k"),
                   paste0("baseline 2026-10-05 from the current input, not the records of the published run: ",
                          nonTaxonomic[[j]], "; published result kept by decision"))
}
