# work2026/taxonChangeRawData_20260924.R  (temporary, for the taxonomy reviewers)
# Raw data behind the per-species additions/subtractions in
# temp/taxonChanges_published_vs_round2_bySpecies.xlsx:
#   1. records   - every record removed from / added to a species between the published input
#                  dataset and the round-2 (taxonomy-only) model data, with ALL columns of the raw
#                  GBIF download row (no reformatting) plus the project's attribution columns.
#   2. gbifNameUsage - how the GBIF backbone treats every distinct name the records carry
#                  (one row per taxonKey seen in the records, straight from api.gbif.org/v1/species/{key}).
#   3. sheetNamesGbifMatch - the names on the project sheet's include/exclude cells for the affected
#                  species, matched against the GBIF backbone (api.gbif.org/v1/species/match).
#   4. sheetCells - the include/exclude cells themselves, verbatim from the sheet export.
#   5. fieldNotes - GBIF's own definitions of the name columns in the download.
# Output: temp/taxonChangeRawData_20260924.xlsx and temp/taxonChangeRawData_20260924_<sheet>.csv
suppressPackageStartupMessages({library(dplyr); library(readr); library(stringr); library(tidyr); library(httr); library(jsonlite); library(openxlsx)})

# --- record set: identical logic to taxonChangeSpreadsheet_20260919.R ----------------------------
key <- function(d) paste(d$databaseSource, coalesce(d$sourceUniqueID, paste(d$originalTaxon, d$latitude, d$longitude, d$localityInformation, d$yearRecorded, d$institutionCode, d$observerName, sep = "~")), sep = "|")
rd <- function(p) { d <- read_csv(p, col_types = cols(.default = "c"), show_col_types = FALSE, progress = FALSE); d$key <- key(d); d }
pub <- rd("data/datasetsForPublication/allSpeciesOccurrences.csv")
r2  <- rd("data/processed_occurrence/model_data20260820_taxonomyOnly.csv")
ex  <- read_csv("data/processed_occurrence/excludedOnTaxonomy_08202026.csv", col_types = cols(.default = "c"), show_col_types = FALSE, progress = FALSE)
exKey <- paste(ex$databaseSource, coalesce(ex$sourceUniqueID, ""), sep = "|")
flows <- read_csv("work2026/taxonChangeFlows_20260916.csv", show_col_types = FALSE)
stem <- function(x) str_remove(str_squish(coalesce(x, "")), "\\s+[A-Z(].*$")
flowStems <- unique(c(str_squish(flows$recordedName), stem(flows$recordedName)))
gbifAll <- read_tsv("data/source_data/vitisGBIFDownload_20250721.csv", col_types = cols(.default = "c"), show_col_types = FALSE, progress = FALSE, quote = "")
gbifRaw <- gbifAll |> filter(!is.na(occurrenceID), occurrenceID != "") |> distinct(occurrenceID, .keep_all = TRUE)
# download rows with no occurrenceID cannot be joined by ID; matched below on institution + year + coordinates + interpreted name when that is unique
gbifNoID <- gbifAll |> filter(is.na(occurrenceID) | occurrenceID == "") |>
  mutate(fb_lat = round(as.numeric(decimalLatitude), 4), fb_lon = round(as.numeric(decimalLongitude), 4)) |>
  group_by(institutionCode, year, fb_lat, fb_lon, scientificName) |> filter(n() == 1) |> ungroup()
nam <- c("USA", "CAN", "MEX")
cne <- function(d) { lat <- suppressWarnings(as.numeric(d$latitude)); lon <- suppressWarnings(as.numeric(d$longitude))
  (d$taxon == "Vitis cinerea" & lat == 0) | (d$taxon == "Vitis palmata" & (lat == 0 | round(lat, 4) == 26.7333)) |
  (d$taxon == "Vitis riparia" & lat == 0) | (d$taxon == "Vitis shuttleworthii" & lon > -28) | (d$taxon == "Vitis vulpina" & lon == 0) |
  (d$taxon == "Vitis labrusca" & d$sourceUniqueID %in% "UNA00061932") | (d$taxon == "Vitis peninsularis" & d$sourceUniqueID %in% "UCJEPS:UC:UC1191193") }
dec16 <- function(d) { lat <- suppressWarnings(as.numeric(d$latitude)); lon <- suppressWarnings(as.numeric(d$longitude))
  (round(lat, 6) == 82.233333) | (round(lon, 4) == -177.2805) | d$taxon == "Vitis novogranatensis" }
pubOnly <- pub |> filter(!paste(key, taxon) %in% paste(r2$key, r2$taxon)) |> mutate(change = "Removed")
r2Only  <- r2  |> filter(!paste(key, taxon) %in% paste(pub$key, pub$taxon)) |> mutate(change = "Added")
r2Concept  <- r2  |> distinct(key, .keep_all = TRUE) |> select(key, nowIn = taxon)
pubConcept <- pub |> distinct(key, .keep_all = TRUE) |> select(key, wasIn = taxon)
all <- bind_rows(pubOnly, r2Only) |> left_join(r2Concept, by = "key") |> left_join(pubConcept, by = "key")
all <- all |> mutate(
  movedConcept = (change == "Removed" & !is.na(nowIn) & nowIn != taxon) | (change == "Added" & !is.na(wasIn) & wasIn != taxon),
  nowExcluded = change == "Removed" & key %in% exKey,
  ownName = taxon %in% c("Vitis tiliifolia", "Vitis popenoei") & str_detect(coalesce(originalTaxon, ""), "tili*folia|popenoei"),
  nameInFlows = !ownName & (str_squish(coalesce(originalTaxon, "")) %in% flowStems | stem(originalTaxon) %in% flowStems),
  isExempt = taxon %in% c("Vitis tiliifolia", "Vitis popenoei") & !(iso3 %in% nam) & !is.na(iso3),
  reason = case_when(coalesce(cne(all), FALSE) ~ "clearNewErrors", coalesce(dec16(all), FALSE) ~ "dec16Driver", isExempt ~ "countryExempt",
                     movedConcept | nowExcluded | nameInFlows ~ "gbifTaxonomy", TRUE ~ "other"),
  counterpart = case_when(change == "Removed" & !is.na(nowIn) & nowIn != taxon ~ nowIn,
                          change == "Removed" ~ "no project concept (excluded)",
                          change == "Added" & !is.na(wasIn) & wasIn != taxon ~ wasIn,
                          change == "Added" ~ "not in the published dataset"))
# the workbook also caught a few rows via the verbatim name; recompute that flag after the GBIF join below
records <- all |>
  transmute(species = taxon, change, counterpart, reason, projectRecordedName = originalTaxon,
            databaseSource, sourceUniqueID, type, latitude, longitude, iso3, yearRecorded, institutionCode) |>
  left_join(gbifRaw |> rename_with(~paste0("gbif_", .x)), by = c("sourceUniqueID" = "gbif_occurrenceID")) |>
  mutate(fb_lat = round(as.numeric(latitude), 4), fb_lon = round(as.numeric(longitude), 4))
fb <- records |> filter(databaseSource == "GBIF", is.na(sourceUniqueID)) |> select(species, change, projectRecordedName, institutionCode, yearRecorded, fb_lat, fb_lon) |>
  inner_join(gbifNoID |> select(-occurrenceID) |> rename_with(~paste0("gbif_", .x), -c(institutionCode, year, fb_lat, fb_lon, scientificName)),
             by = c("institutionCode", "yearRecorded" = "year", "fb_lat", "fb_lon", "projectRecordedName" = "scientificName")) |>
  group_by(species, change, projectRecordedName, institutionCode, yearRecorded, fb_lat, fb_lon) |> filter(n() == 1) |> ungroup()
records <- records |> rows_update(fb, by = c("species", "change", "projectRecordedName", "institutionCode", "yearRecorded", "fb_lat", "fb_lon"), unmatched = "ignore") |>
  mutate(gbifRowJoin = case_when(databaseSource != "GBIF" ~ "not a GBIF record", !is.na(sourceUniqueID) & !is.na(gbif_gbifID) ~ "by occurrenceID",
                                 !is.na(gbif_gbifID) ~ "fallback: institution + year + coordinates + interpreted name (download row has no occurrenceID)",
                                 TRUE ~ "not joined (download row has no occurrenceID and the fallback match was not unique)")) |>
  select(-fb_lat, -fb_lon) |> relocate(gbifRowJoin, .after = institutionCode) |>
  mutate(reason = if_else(reason == "other" & change %in% c("Removed", "Added") & databaseSource == "GBIF" &
                            stem(gbif_verbatimScientificName) %in% flowStems & !(species %in% c("Vitis tiliifolia", "Vitis popenoei")), "gbifTaxonomy", reason)) |>
  arrange(species, desc(change == "Removed"), projectRecordedName, sourceUniqueID)

# --- GBIF backbone usage for every taxonKey in the records ----------------------------------------
getJSON <- function(url) { r <- RETRY("GET", url, times = 3, pause_base = 2, quiet = TRUE); if (status_code(r) != 200) return(NULL)
  fromJSON(content(r, as = "text", encoding = "UTF-8"), simplifyVector = FALSE) }
flat <- function(x) { if (is.null(x)) return(tibble()); x <- x[!vapply(x, is.list, logical(1)) | vapply(x, length, integer(1)) == 0]
  x <- x[vapply(x, length, integer(1)) > 0]; as_tibble(lapply(x, function(v) as.character(v))) }
usage <- function(k) { u <- getJSON(paste0("https://api.gbif.org/v1/species/", k)); if (is.null(u)) return(tibble(key = as.character(k), note = "lookup failed"))
  out <- flat(u)
  if (!is.null(u$acceptedKey)) { a <- getJSON(paste0("https://api.gbif.org/v1/species/", u$acceptedKey))
    if (!is.null(a)) out <- bind_cols(out, flat(a) |> select(any_of(c("scientificName", "rank", "taxonomicStatus", "parent", "parentKey"))) |> rename_with(~paste0("accepted_", .x))) }
  out }
keys <- records |> filter(!is.na(gbif_taxonKey)) |> distinct(gbif_taxonKey) |> pull()
cat("looking up", length(keys), "taxonKeys from the GBIF API\n")
gbifNameUsage <- bind_rows(lapply(keys, usage)) |> mutate(across(everything(), as.character))
nRec <- records |> filter(!is.na(gbif_taxonKey)) |> count(gbif_taxonKey, name = "recordsInThisTable")
gbifNameUsage <- gbifNameUsage |> left_join(nRec, by = c("key" = "gbif_taxonKey")) |>
  select(key, scientificName, canonicalName, authorship, rank, taxonomicStatus, any_of(c("accepted", "acceptedKey", "accepted_scientificName", "accepted_rank", "accepted_taxonomicStatus", "accepted_parent", "accepted_parentKey", "parent", "parentKey", "species", "speciesKey", "basionym", "basionymKey", "publishedIn", "remarks", "nameType", "origin")), recordsInThisTable, everything()) |>
  arrange(desc(recordsInThisTable))

# --- sheet cells for the affected species, and how GBIF matches each listed name ------------------
sheet <- read_csv("data/New World Vitis.csv", col_types = cols(.default = "c"), show_col_types = FALSE, progress = FALSE)
affected <- sort(unique(c(records$species, records$counterpart[!str_detect(records$counterpart, "^no project|^not in")])))
sheetCells <- sheet |> filter(`Scientific Name` %in% affected) |>
  select(`Include in gap analysis?`, `Scientific Name`, `Scientific name with authors`, `Names to include in this concept (Homotypic synonyms)`, `Names to exclude from this concept`, `Taxonomic notes`)
splitCell <- function(x) str_squish(unlist(str_split(coalesce(x, ""), "[,;]")))
sheetNames <- bind_rows(
  sheetCells |> transmute(sheetSpecies = `Scientific Name`, cell = "include", name = lapply(`Names to include in this concept (Homotypic synonyms)`, splitCell)) |> unnest(name),
  sheetCells |> transmute(sheetSpecies = `Scientific Name`, cell = "exclude", name = lapply(`Names to exclude from this concept`, splitCell)) |> unnest(name),
  sheetCells |> transmute(sheetSpecies = `Scientific Name`, cell = "concept name", name = `Scientific Name`)) |>
  filter(name != "") |> distinct()
matchName <- function(n) { m <- getJSON(paste0("https://api.gbif.org/v1/species/match?kingdom=Plantae&name=", URLencode(n, reserved = TRUE)))
  if (is.null(m)) return(tibble(note = "lookup failed")); out <- flat(m) |> rename_with(~paste0("match_", .x))
  if (!is.null(m$acceptedUsageKey)) { a <- getJSON(paste0("https://api.gbif.org/v1/species/", m$acceptedUsageKey))
    if (!is.null(a)) out <- bind_cols(out, flat(a) |> select(any_of(c("scientificName", "rank", "taxonomicStatus"))) |> rename_with(~paste0("accepted_", .x))) }
  out }
cat("matching", n_distinct(sheetNames$name), "sheet names against the GBIF backbone\n")
lookups <- lapply(unique(sheetNames$name), matchName); names(lookups) <- unique(sheetNames$name)
sheetNamesGbifMatch <- sheetNames |> mutate(res = lookups[name]) |> unnest(res) |>
  select(sheetSpecies, cell, name, any_of(c("match_matchType", "match_confidence", "match_usageKey", "match_scientificName", "match_rank", "match_status", "match_acceptedUsageKey", "accepted_scientificName", "accepted_rank", "accepted_taxonomicStatus", "match_species", "match_speciesKey", "note")), everything())

fieldNotes <- tribble(
  ~column, ~meaning,
  "gbif_scientificName", "GBIF's interpreted name: the backbone taxon the publisher's name was matched to (with authorship). This is the name the project's old parser used, after reducing it to the backbone's ACCEPTED species.",
  "gbif_verbatimScientificName", "The name exactly as the data publisher supplied it, before GBIF interpretation.",
  "gbif_verbatimScientificNameAuthorship", "Authorship exactly as the publisher supplied it.",
  "gbif_taxonKey", "GBIF backbone key of the interpreted name (may be a synonym usage). Look this key up in the gbifNameUsage sheet.",
  "gbif_speciesKey / gbif_species", "GBIF backbone ACCEPTED species that the taxonKey resolves to. For a synonym, this is the species GBIF folds the record into.",
  "gbif_taxonRank", "Rank of the interpreted name (SPECIES, VARIETY, GENUS ...).",
  "gbif_infraspecificEpithet", "Infraspecific epithet of the interpreted name, if any.",
  "gbif_issue", "GBIF interpretation flags; TAXON_MATCH_HIGHERRANK / TAXON_MATCH_FUZZY / TAXON_MATCH_NONE describe how well the name matched.",
  "projectRecordedName", "The name the project pipeline recorded for the record (round-2 parser: GBIF's interpreted name, or the verbatim name when GBIF could only match to genus).",
  "species", "The project concept the record was in (Removed) or is now in (Added).",
  "counterpart", "Removed: the project concept the record now sits in, or 'no project concept (excluded)'. Added: the concept it sat in before, or 'not in the published dataset'.",
  "reason", "gbifTaxonomy = caused by the revised name handling; other values are non-taxonomy differences kept so totals reconcile with the earlier workbook.",
  "gbifNameUsage.taxonomicStatus", "ACCEPTED / SYNONYM / DOUBTFUL etc. in the GBIF backbone for that taxonKey.",
  "gbifNameUsage.accepted_* ", "The backbone taxon a SYNONYM points to (name, rank, status, parent).",
  "sheetNamesGbifMatch.match_*", "Result of matching the sheet's name string to the backbone; status SYNONYM plus accepted_* shows where GBIF would place it.")

# --- write ----------------------------------------------------------------------------------------
tabs <- list(records = records, gbifNameUsage = gbifNameUsage, sheetNamesGbifMatch = sheetNamesGbifMatch, sheetCells = sheetCells, fieldNotes = fieldNotes)
wb <- createWorkbook(); for (n in names(tabs)) { addWorksheet(wb, n); writeData(wb, n, tabs[[n]]); freezePane(wb, n, firstRow = TRUE) }
saveWorkbook(wb, "temp/taxonChangeRawData_20260924.xlsx", overwrite = TRUE)
for (n in names(tabs)) write_csv(tabs[[n]], paste0("temp/taxonChangeRawData_20260924_", n, ".csv"), na = "")
cat("records:", nrow(records), " removed:", sum(records$change == "Removed"), " added:", sum(records$change == "Added"), "\n")
print(records |> count(databaseSource, gbifRowJoin))
print(as.data.frame(records |> filter(species == "Vitis cinerea") |> count(change, projectRecordedName, gbif_scientificName, gbif_species, counterpart)), row.names = FALSE)
