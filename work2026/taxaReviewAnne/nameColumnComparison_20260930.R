# work2026/taxaReviewAnne/nameColumnComparison_20260930.R
# For every record affected by the taxonomy change (reason == "gbifTaxonomy"), compare the three name columns:
#   originalTaxon (projectRecordedName) vs GBIF scientificName vs GBIF verbatimScientificName.
# Names are compared exactly and after parsing to the bare name (genus, epithet, rank + infraspecific epithet,
# authorship dropped) with parseVitisName(), the same parser the pipeline uses.
# Output: nameColumnComparison_20260930.xlsx (Summary, By name, Records) in this folder.
suppressPackageStartupMessages({library(dplyr); library(readr); library(stringr); library(openxlsx)})
source("preprocessing/functions/process_gbif_082026.R")  # parseVitisName()

dir <- "work2026/taxaReviewAnne"
rec <- read_csv("temp/taxonChangeRawData_20260924_records.csv", col_types = cols(.default = "c"), show_col_types = FALSE) |>
  filter(reason == "gbifTaxonomy")

sq <- function(x) str_squish(coalesce(x, ""))
cap <- function(x) sub("^([vm])", "\\U\\1", str_squish(x), perl = TRUE)   # "vitis berlandieri planch." -> "Vitis ..."
noHyb <- function(x) str_to_lower(str_remove_all(coalesce(x, ""), "\\bx\\s+|[- ]"))            # drop hybrid marker, spaces, hyphens
cmp <- rec |> mutate(
  originalTaxon = projectRecordedName,
  fallbackJoin = str_starts(coalesce(gbifRowJoin, ""), "fallback"),
  # rows joined by the fallback were matched ON scientificName = projectRecordedName, so the value was never copied over
  gbif_scientificName = if_else(fallbackJoin & is.na(gbif_scientificName), projectRecordedName, gbif_scientificName),
  bareScientific = parseVitisName(gbif_scientificName),
  bareVerbatim = parseVitisName(cap(gbif_verbatimScientificName)),
  fuzzy = str_detect(coalesce(gbif_issue, ""), "TAXON_MATCH_FUZZY"),
  `originalTaxon vs scientificName` = case_when(
    databaseSource != "GBIF" ~ "n/a (not a GBIF record)",
    is.na(gbif_gbifID) ~ "n/a (GBIF row not joined)",
    fallbackJoin ~ "Same (row matched on this name)",
    sq(originalTaxon) == sq(gbif_scientificName) ~ "Same",
    TRUE ~ "Different"),
  `scientificName vs verbatimScientificName` = case_when(
    databaseSource != "GBIF" ~ "n/a (not a GBIF record)",
    is.na(gbif_gbifID) ~ "n/a (GBIF row not joined)",
    sq(gbif_verbatimScientificName) == "" ~ "Verbatim name blank in the download",
    sq(gbif_scientificName) == sq(gbif_verbatimScientificName) ~ "Same (identical text)",
    gbif_taxonRank == "GENUS" ~ "Different: GBIF matched genus only (Vitis L.)",
    is.na(bareVerbatim) & str_detect(sq(gbif_verbatimScientificName), "\\s[xX\u00d7]\\s") ~ "Different: verbatim is a hybrid formula",
    !is.na(bareVerbatim) & bareVerbatim == bareScientific ~ "Same name, authorship/capitalisation differs",
    !is.na(bareVerbatim) & noHyb(bareVerbatim) == noHyb(bareScientific) ~ "Same name, hybrid marker/spacing differs",
    fuzzy ~ "Different: spelling corrected by GBIF (fuzzy match)",
    noHyb(word(cap(gbif_verbatimScientificName), 1, 2)) == noHyb(word(gbif_scientificName, 1, 2)) ~ "Same binomial, rank/infraspecific part differs",
    TRUE ~ "Different name"),
  `all three` = case_when(
    str_starts(`originalTaxon vs scientificName`, "n/a") ~ `originalTaxon vs scientificName`,
    str_starts(`originalTaxon vs scientificName`, "Same") & str_starts(`scientificName vs verbatimScientificName`, "Same") ~ "Same name in all three",
    str_starts(`scientificName vs verbatimScientificName`, "Verbatim") ~ "originalTaxon = scientificName; verbatim blank",
    TRUE ~ "Verbatim name differs"))

# --- summaries ---------------------------------------------------------------------------------------
summ <- cmp |> count(`originalTaxon vs scientificName`, `scientificName vs verbatimScientificName`, name = "Records") |>
  arrange(desc(Records))
byName <- cmp |> filter(databaseSource == "GBIF", !is.na(gbif_gbifID)) |>
  count(Species = species, Change = change, originalTaxon, `GBIF scientificName` = gbif_scientificName,
        `GBIF verbatimScientificName` = gbif_verbatimScientificName, `GBIF accepted species` = gbif_species,
        `scientificName vs verbatimScientificName`, name = "Records") |>
  arrange(Species, Change, originalTaxon, desc(Records))
recOut <- cmp |> select(species, change, counterpart, databaseSource, sourceUniqueID, gbif_gbifID, originalTaxon,
                        gbif_scientificName, gbif_verbatimScientificName, gbif_species, gbif_taxonRank, gbif_issue,
                        `originalTaxon vs scientificName`, `scientificName vs verbatimScientificName`, `all three`)

wb <- createWorkbook(); hs <- createStyle(textDecoration = "bold", fgFill = "#DDE6F0", wrapText = TRUE, valign = "top")
for (t in list(list("Summary", summ, c(30, 50, 10)), list("By name", byName, c(24, 9, 36, 36, 36, 22, 40, 9)), list("Records", recOut, 18))) {
  addWorksheet(wb, t[[1]]); writeData(wb, t[[1]], t[[2]], headerStyle = hs); freezePane(wb, t[[1]], firstRow = TRUE)
  setColWidths(wb, t[[1]], seq_along(t[[2]]), rep_len(t[[3]], ncol(t[[2]])))
}
saveWorkbook(wb, file.path(dir, "nameColumnComparison_20260930.xlsx"), overwrite = TRUE)

options(width = 200)
cat("Records affected by the taxonomy change:", nrow(cmp), "\n")
print(as.data.frame(summ), row.names = FALSE)
print(as.data.frame(count(cmp, `all three`)), row.names = FALSE)
cat("\nBy change (GBIF, joined):\n")
print(as.data.frame(cmp |> filter(databaseSource == "GBIF", !is.na(gbif_gbifID)) |> count(change, `scientificName vs verbatimScientificName`)), row.names = FALSE)
cat("\nExamples of 'Different name':\n")
print(as.data.frame(byName |> filter(str_starts(`scientificName vs verbatimScientificName`, "Different"), !str_detect(`scientificName vs verbatimScientificName`, "genus only")) |> head(25)), row.names = FALSE)
