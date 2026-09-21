# work2026/taxonChangeSpreadsheet_20260919.R
# Partner-facing workbook: for every species whose record set changed between
# the published input dataset and the round-2 (taxonomy-only) model data, list
# which recorded names were removed and which were added, where each block of
# records went to / came from, and what decision (if any) the sheet needs.
# Inputs: work2026/publication_vs_round2_records.csv (from compareToPublication_20260917.R),
#         the two model-data files, excludedOnTaxonomy_08202026.csv, raw GBIF download.
# Output: temp/taxonChanges_published_vs_round2_bySpecies.xlsx (+ detail CSV in work2026/)
suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(stringr); library(openxlsx)})
key <- function(d) paste(d$databaseSource, coalesce(d$sourceUniqueID, paste(d$originalTaxon, d$latitude, d$longitude, d$localityInformation, d$yearRecorded, d$institutionCode, d$observerName, sep = "~")), sep = "|")
rd <- function(p) { d <- read_csv(p, col_types = cols(.default = "c"), show_col_types = FALSE, progress = FALSE); d$key <- key(d); d }
pub <- rd("data/datasetsForPublication/allSpeciesOccurrences.csv")
r2  <- rd("data/processed_occurrence/model_data20260820_taxonomyOnly.csv")
ex  <- read_csv("data/processed_occurrence/excludedOnTaxonomy_08202026.csv", col_types = cols(.default = "c"), show_col_types = FALSE, progress = FALSE)
exKey <- paste(ex$databaseSource, coalesce(ex$sourceUniqueID, ""), sep = "|")
flows <- read_csv("work2026/taxonChangeFlows_20260916.csv", show_col_types = FALSE)
stem <- function(x) str_remove(str_squish(coalesce(x, "")), "\\s+[A-Z(].*$")
flowStems <- unique(c(str_squish(flows$recordedName), stem(flows$recordedName)))
gbif <- read_tsv("data/source_data/vitisGBIFDownload_20250721.csv", col_types = cols(.default = "c"), show_col_types = FALSE, progress = FALSE, quote = "") |>
  filter(!is.na(occurrenceID), occurrenceID != "") |> select(occurrenceID, verbatimScientificName) |> distinct(occurrenceID, .keep_all = TRUE)
nam <- c("USA", "CAN", "MEX")
cne <- function(d) { lat <- suppressWarnings(as.numeric(d$latitude)); lon <- suppressWarnings(as.numeric(d$longitude))
  (d$taxon == "Vitis cinerea" & lat == 0) | (d$taxon == "Vitis palmata" & (lat == 0 | round(lat, 4) == 26.7333)) |
  (d$taxon == "Vitis riparia" & lat == 0) | (d$taxon == "Vitis shuttleworthii" & lon > -28) | (d$taxon == "Vitis vulpina" & lon == 0) |
  (d$taxon == "Vitis labrusca" & d$sourceUniqueID %in% "UNA00061932") | (d$taxon == "Vitis peninsularis" & d$sourceUniqueID %in% "UCJEPS:UC:UC1191193") }
dec16 <- function(d) { lat <- suppressWarnings(as.numeric(d$latitude)); lon <- suppressWarnings(as.numeric(d$longitude))
  (round(lat, 6) == 82.233333) | (round(lon, 4) == -177.2805) | d$taxon == "Vitis novogranatensis" }

# --- rebuild the record-level table with the pieces the partner needs -------
pubOnly <- pub |> filter(!paste(key, taxon) %in% paste(r2$key, r2$taxon)) |> mutate(direction = "Removed")
r2Only  <- r2  |> filter(!paste(key, taxon) %in% paste(pub$key, pub$taxon)) |> mutate(direction = "Added")
r2Concept  <- r2  |> distinct(key, .keep_all = TRUE) |> select(key, nowIn = taxon)
pubConcept <- pub |> distinct(key, .keep_all = TRUE) |> select(key, wasIn = taxon)
all <- bind_rows(pubOnly, r2Only)
all <- all |>
  left_join(r2Concept, by = "key") |> left_join(pubConcept, by = "key") |>
  left_join(gbif, by = c("sourceUniqueID" = "occurrenceID")) |>
  mutate(movedConcept = (direction == "Removed" & !is.na(nowIn) & nowIn != taxon) | (direction == "Added" & !is.na(wasIn) & wasIn != taxon),
         nowExcluded = direction == "Removed" & key %in% exKey,
         # flows were traced against the Dec-2025 file, where tiliifolia/popenoei lacked their Central American records;
         # against the publication file a tiliifolia/popenoei record under its own name is a swap artefact, not a taxonomy change
         ownName = taxon %in% c("Vitis tiliifolia", "Vitis popenoei") & str_detect(coalesce(originalTaxon, ""), "tili*folia|popenoei"),
         nameInFlows = !ownName & (str_squish(coalesce(originalTaxon, "")) %in% flowStems | stem(originalTaxon) %in% flowStems | stem(verbatimScientificName) %in% flowStems),
         isExempt = taxon %in% c("Vitis tiliifolia", "Vitis popenoei") & !(iso3 %in% nam) & !is.na(iso3),
         reason = case_when(coalesce(cne(all), FALSE) ~ "clearNewErrors", coalesce(dec16(all), FALSE) ~ "dec16Driver", isExempt ~ "countryExempt",
                            movedConcept | nowExcluded | nameInFlows ~ "gbifTaxonomy", TRUE ~ "other"),
         hasCoords = !is.na(suppressWarnings(as.numeric(latitude))) & !is.na(suppressWarnings(as.numeric(longitude))),
         isG = coalesce(type, "") == "G",
         recordedName = str_squish(coalesce(originalTaxon, "")),
         verbatimName = if_else(databaseSource == "GBIF", str_squish(coalesce(verbatimScientificName, "")), NA_character_),
         verbatimName = if_else(!is.na(verbatimName) & verbatimName != "" & verbatimName != recordedName, verbatimName, NA_character_),
         counterpart = case_when(direction == "Removed" & !is.na(nowIn) & nowIn != taxon ~ nowIn,
                                 direction == "Removed" ~ "no project concept (excluded)",
                                 direction == "Added" & !is.na(wasIn) & wasIn != taxon ~ wasIn,
                                 direction == "Added" ~ "not in the published dataset"))
stopifnot(sum(all$direction == "Removed") == nrow(pubOnly), sum(all$direction == "Added") == nrow(r2Only))

# --- curator notes from the 2026-09-16 per-taxon review (taxonChangeExplanations_20260916.md) ---
notes <- tribble(
  ~pattern, ~note,
  "^Vitis bicolor", "Synonym of V. aestivalis var. bicolor; suggest adding 'Vitis bicolor' to the var. bicolor include cell.",
  "^Vitis caribaea", "Synonym of V. tiliifolia; suggest adding to the tiliifolia include cell.",
  "^Vitis coriacea", "The include cell has only 'Vitis candicans var. coriacea'; suggest adding 'Vitis coriacea'.",
  "^Muscadinia popenoei", "Suggest adding 'Muscadinia popenoei' to the popenoei include cell.",
  "^Vitis berlandieri var\\. tomentosa", "Reached V. cinerea var. tomentosa only via the backbone; adding the name to that include cell restores the taxon (now 0 records).",
  "^Vitis aestivalis var\\. cinerea", "Suggest adding to the V. cinerea var. cinerea include cell.",
  "^Vitis helleri", "Suggest adding to the V. berlandieri include cell.",
  "^Vitis cordifolia var\\. helleri", "Consider adding to the V. berlandieri include cell.",
  "^Vitis (x |× )?labruscana", "Cultivated V. labrusca x V. vinifera hybrid; recommend leaving excluded.",
  "^Vitis bourquiniana", "Cultivated hybrid; probably exclude.",
  "^Vitis (x |× )?slavinii", "Hybrid; probably exclude.",
  "^Vitis catawba", "Cultivar name; probably exclude.",
  "^Vitis novomexicana", "Curator call: an acerifolia synonym in most treatments.",
  "^Vitis rotundifolia var\\. munsoniana", "Sheet note says the munsoniana concept is var. munsoniana; add the name to move these from rotundifolia.",
  "^Vitis (x |× )?champinii|^Vitis xchampinii", "Verbatim name 'Vitis xchampinii' (no space after the hybrid marker) was unmatched by GBIF; now recovered from the verbatim name.",
  "^Vitis cordifolia", "The V. vulpina include cell uses a semicolon and 'V.'; it never matched before. Data-entry fix, records enter for the first time.",
  "^Vitis argentifolia", "A var. bicolor synonym already on the include list; the backbone had folded it into V. aestivalis.",
  "^Vitis linsecomii|^Vitis lincecumii", "Backbone treats V. lincecumii as a synonym of V. aestivalis; the project keeps it as its own concept.",
  "^Vitis berlandieri Planch", "Backbone: synonym of V. cinerea var. helleri; the project keeps V. berlandieri as its own concept.",
  "^Vitis baileyana|^Vitis simpsonii", "Backbone folds this segregate into V. cinerea; the project keeps it as its own concept.",
  "^Vitis munsoniana|^Muscadinia munsoniana", "Backbone folds munsoniana into V. rotundifolia; the project keeps it as its own concept.",
  "^Vitis rufotomentosa", "Backbone: synonym of V. aestivalis; the project keeps V. rufotomentosa as its own concept.",
  "^Vitis riparia subsp\\. riparia", "Subspecies autonym; only the forma autonym is mapped in code. Sheet decision.",
  "^Vitis vulpina subsp\\. riparia", "On the riparia include list; matched by the new parser.",
  " x V\\. | x Vitis | cv\\. .* x ", "Verbatim name is a hybrid formula; the parser kept the first parent's name. The publication team may prefer to exclude these."
)
noteFor <- function(nm) { out <- rep(NA_character_, length(nm)); for (i in seq_len(nrow(notes))) { hit <- is.na(out) & str_detect(nm, notes$pattern[i]); out[hit] <- notes$note[i] }; out }

mech <- function(direction, counterpart, recordedName, reason, taxon) case_when(
  reason == "countryExempt" ~ "Not taxonomy: Central American records the publication kept by hand and round 2 keeps by rule (country-filter exemption)",
  reason == "dec16Driver"   ~ "Not taxonomy: hard-coded outlier removals / source relabel added to the driver on 2025-12-16, after the publication file",
  reason == "clearNewErrors"~ "Not taxonomy: one-off coordinate removal from the summary-map review",
  reason == "other" & taxon %in% c("Vitis tiliifolia", "Vitis popenoei") ~ "Not taxonomy: artefact of the hand swap of tiliifolia/popenoei records from the July 2025 file into the publication dataset",
  reason == "other"         ~ "Not taxonomy: unexplained difference",
  direction == "Removed" & !str_starts(counterpart, "no project concept") ~ "A. Segregate recovered: records recorded under this name now go to their own project concept",
  direction == "Removed" ~ "D. Name on no include list: the backbone had folded this name into the species; name-based matching keeps the recorded name, which matches no project concept",
  direction == "Added" & counterpart != "not in the published dataset" ~ "A. Segregate recovered: records recorded under this name return from the concept the backbone had lumped them into",
  direction == "Added" & recordedName == "Vitis L." ~ "B. Verbatim recovery: GBIF could not match the publisher's name (interpreted as 'Vitis L.'); the verbatim name is now parsed",
  direction == "Added" & str_detect(recordedName, "^Vitis cordifolia") ~ "C. Synonym-cell fix: the include cell entry now matches",
  direction == "Added" ~ "F. Name newly matched: a name on the include list that the old parser did not match")

vb <- all |> filter(!is.na(verbatimName)) |> count(taxon, direction, recordedName, databaseSource, counterpart, reason, verbatimName) |>
  arrange(desc(n)) |> group_by(species = taxon, change = direction, recordedName, dataSource = databaseSource, counterpart, reason) |>
  summarise(verbatimNames = paste0(verbatimName, " (", n, ")", collapse = "; "), .groups = "drop")
detail <- all |>
  group_by(species = taxon, change = direction, recordedName, dataSource = databaseSource, counterpart, reason) |>
  summarise(records = n(), withCoordinates = sum(hasCoords), germplasm = sum(isG), .groups = "drop") |>
  left_join(vb, by = c("species", "change", "recordedName", "dataSource", "counterpart", "reason")) |>
  mutate(mechanism = mech(change, counterpart, recordedName, reason, species),
         taxonomyChange = if_else(reason == "gbifTaxonomy", "Yes", "No"),
         decisionNeeded = if_else(reason == "gbifTaxonomy" & change == "Removed" & str_starts(counterpart, "no project concept"), "Yes", "No"),
         note = coalesce(noteFor(recordedName), noteFor(coalesce(verbatimNames, "")))) |>
  arrange(species, desc(change == "Removed"), desc(records))

# --- summary by species ------------------------------------------------------
lab <- function(d) d |> group_by(species, change) |>
  summarise(txt = paste0(recordedName, " (", records,
                         if_else(change == "Removed", " -> ", " <- "), counterpart, ")", collapse = "; "), .groups = "drop")
tx <- detail |> filter(taxonomyChange == "Yes")
counts <- full_join(pub |> count(species = taxon, name = "publicationRecords"), r2 |> count(species = taxon, name = "round2Records"), by = "species") |>
  mutate(across(-species, ~coalesce(.x, 0L)), netChange = round2Records - publicationRecords)
summary <- counts |>
  left_join(tx |> group_by(species) |> summarise(removedTaxonomy = sum(records[change == "Removed"]), addedTaxonomy = sum(records[change == "Added"]),
                                                 coordsRemoved = sum(withCoordinates[change == "Removed"]), coordsAdded = sum(withCoordinates[change == "Added"]),
                                                 germplasmRemoved = sum(germplasm[change == "Removed"]), germplasmAdded = sum(germplasm[change == "Added"]), .groups = "drop"), by = "species") |>
  left_join(detail |> filter(taxonomyChange == "No") |> group_by(species) |> summarise(otherRemoved = sum(records[change == "Removed"]), otherAdded = sum(records[change == "Added"]), .groups = "drop"), by = "species") |>
  left_join(tx |> filter(change == "Removed") |> group_by(species) |> summarise(namesRemoved = paste(unique(recordedName), collapse = "; "), .groups = "drop"), by = "species") |>
  left_join(tx |> filter(change == "Added") |> group_by(species) |> summarise(namesAdded = paste(unique(coalesce(if_else(recordedName == "Vitis L.", NA_character_, recordedName), str_remove_all(verbatimNames, " \\(\\d+\\)"), recordedName)), collapse = "; "), .groups = "drop"), by = "species") |>
  left_join(lab(tx) |> filter(change == "Removed") |> select(species, removedNames = txt), by = "species") |>
  left_join(lab(tx) |> filter(change == "Added") |> select(species, addedNames = txt), by = "species") |>
  left_join(tx |> filter(decisionNeeded == "Yes") |> group_by(species) |> summarise(namesNeedingDecision = paste0(recordedName, " (", records, ")", collapse = "; "), .groups = "drop"), by = "species") |>
  mutate(across(where(is.integer), ~coalesce(.x, 0L))) |>
  filter(netChange != 0 | removedTaxonomy > 0 | addedTaxonomy > 0 | otherRemoved > 0 | otherAdded > 0) |>
  arrange(desc(abs(netChange)), species)
unchanged <- setdiff(counts$species, summary$species) |> sort()

# --- write workbook ------------------------------------------------------------
readme <- c(
  "Record changes per species: published input dataset vs round-2 (taxonomy-only) model data",
  paste0("Prepared ", format(Sys.Date(), "%Y-%m-%d"), " from work2026/taxonChangeSpreadsheet_20260919.R."),
  "",
  "Baseline: data/datasetsForPublication/allSpeciesOccurrences.csv (the input dataset behind the published analysis).",
  "Comparison: data/processed_occurrence/model_data20260820_taxonomyOnly.csv (same pipeline with the GBIF name handling revised; no other method changes).",
  "Records are matched between the two files by data source + source record ID. Counts are model-data rows after all filters.",
  "",
  "What changed in the GBIF name handling: the old parser took the GBIF backbone's accepted species for every record, so records the backbone treats as synonyms",
  "(e.g. 'Vitis berlandieri' -> V. cinerea, 'Vitis munsoniana' -> V. rotundifolia) never reached the project's own concept. The revised parser uses the name",
  "the record was published under (GBIF's interpreted name first, the publisher's verbatim name if GBIF could not match it) and matches it against the",
  "project's taxonomy sheet. A recorded name that is on no include list now leaves the model data instead of being silently folded into a species.",
  "",
  "namesRemoved / namesAdded: plain lists of the recorded names only. For names GBIF could not match ('Vitis L.') the publisher's verbatim name is listed instead.",
  "Summary text format: 'recorded name (records -> where they went)' for removed names, 'recorded name (records <- where they came from)' for added names.",
  "",
  "Sheets:",
  "  Summary by species  - one row per species whose record set differs; counts, the removed/added names, and the names that need a sheet decision.",
  "  Detail              - one row per species x removed/added x recorded name x data source, with where the records went to or came from.",
  "  Unchanged species   - species with an identical record set in both files.",
  "",
  "Column notes (Detail):",
  "  recordedName     the name as GBIF interpreted it (or the name in the non-GBIF source).",
  "  verbatimNames    the publisher's own name strings with record counts, shown only where they differ from recordedName (GBIF records only).",
  "  counterpart      Removed: the project concept the records now sit in, or 'no project concept (excluded)' (record is in excludedOnTaxonomy_08202026.csv).",
  "                   Added: the concept the records sat in before, or 'not in the published dataset'.",
  "  taxonomyChange   Yes = caused by the revised name handling. No = a difference with another cause (kept here so per-species totals reconcile).",
  "  decisionNeeded   Yes = records left the model data because the name is on no include list; adding the name to an include cell restores them.",
  "  note             suggestion from the 2026-09-16 review; these are proposals for the publication team to confirm, not decisions.",
  "",
  "Mechanism codes: A segregate recovered (fix working as intended); B verbatim recovery (fix working as intended); C synonym-cell fix;",
  "D name on no include list (needs a curator decision); F name newly matched. Homonyms (same binomial, different author, different species) keep the",
  "backbone assignment by rule and do not appear here because they did not change.",
  "",
  "Not included: the 412 records named 'Vitis rotundifolia var. munsoniana' (still in V. rotundifolia under both parsers) and the 417 named",
  "'Vitis riparia subsp. riparia' (excluded under both parsers). Both are pending sheet decisions but are not differences between the two files.")

wb <- createWorkbook()
hs <- createStyle(textDecoration = "bold", fgFill = "#DDEBF7", border = "bottom", wrapText = TRUE, valign = "top")
wrap <- createStyle(wrapText = TRUE, valign = "top")
addWorksheet(wb, "Read me"); writeData(wb, "Read me", data.frame(readme), colNames = FALSE); setColWidths(wb, "Read me", 1, 160)
addWorksheet(wb, "Summary by species")
writeData(wb, "Summary by species", summary, headerStyle = hs); freezePane(wb, "Summary by species", firstRow = TRUE, firstCol = TRUE)
setColWidths(wb, "Summary by species", 1:ncol(summary), c(30, 12, 12, 10, 12, 12, 12, 12, 12, 12, 12, 12, 60, 60, 70, 70, 50))
addStyle(wb, "Summary by species", wrap, rows = 2:(nrow(summary) + 1), cols = 1:ncol(summary), gridExpand = TRUE)
addWorksheet(wb, "Detail")
writeData(wb, "Detail", detail |> select(species, change, recordedName, verbatimNames, dataSource, records, withCoordinates, germplasm, counterpart, mechanism, taxonomyChange, decisionNeeded, note), headerStyle = hs)
freezePane(wb, "Detail", firstRow = TRUE, firstCol = TRUE); addFilter(wb, "Detail", 1, 1:13)
setColWidths(wb, "Detail", 1:13, c(30, 10, 38, 30, 20, 9, 12, 10, 40, 60, 10, 10, 60))
addStyle(wb, "Detail", wrap, rows = 2:(nrow(detail) + 1), cols = 1:13, gridExpand = TRUE)
addWorksheet(wb, "Unchanged species"); writeData(wb, "Unchanged species", data.frame(species = unchanged), headerStyle = hs); setColWidths(wb, "Unchanged species", 1, 32)
saveWorkbook(wb, "temp/taxonChanges_published_vs_round2_bySpecies.xlsx", overwrite = TRUE)
write_csv(detail, "work2026/taxonChanges_published_vs_round2_detail_20260919.csv")
write_csv(summary, "work2026/taxonChanges_published_vs_round2_summary_20260919.csv")

# --- checks ---------------------------------------------------------------------
byTaxon <- read_csv("work2026/publication_vs_round2_byTaxon.csv", show_col_types = FALSE)
chk <- summary |> select(species, netChange) |> inner_join(byTaxon |> select(species = taxon, change), by = "species")
stopifnot(all(chk$netChange == chk$change))
cat("species changed:", nrow(summary), " unchanged:", length(unchanged), " detail rows:", nrow(detail), "\n")
cat("records removed:", sum(all$direction == "Removed"), " added:", sum(all$direction == "Added"), "\n")
print(as.data.frame(summary |> select(species, publicationRecords, round2Records, netChange, removedTaxonomy, addedTaxonomy, otherRemoved, otherAdded, namesNeedingDecision)), row.names = FALSE)
cat("\n'other' rows:\n"); print(as.data.frame(detail |> filter(reason == "other") |> select(species, change, recordedName, dataSource, records)), row.names = FALSE)
