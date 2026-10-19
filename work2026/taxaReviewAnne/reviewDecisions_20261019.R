# work2026/taxaReviewAnne/reviewDecisions_20261019.R
# Anne's completed review of the taxonomy record changes (taxonChangeRawData_20260924v2.xlsx,
# received 2026-10-19; every one of the 3,674 rows now carries a decision in AF.decision /
# AF.Comment). This script turns the decisions into pipeline actions and checks the geography
# of every record that is added or returns, as the review asks.
#
#   Rscript work2026/taxaReviewAnne/reviewDecisions_20261019.R
#
# Inputs
#   work2026/taxaReviewAnne/taxonChangeRawData_20260924v2.xlsx   the review (records tab)
#   data/processed_occurrence/excludedOnTaxonomy_10022026.csv     records the current build dropped
#                                                                 because their name matched no concept
#   data/processed_occurrence/model_data20261002.csv              current model input
#   data/New World Vitis.csv                                      current synonym sheet (local copy)
#   data/source_data/FNA_stateClassification.csv                  the state lists the model driver
#                                                                 filters on (R2/dataProcessing/fnaFilter.R)
#   data/geospatial_datasets/states/ne_10m_admin_1_states_provinces.gpkg
# Output
#   work2026/taxaReviewAnne/reviewDecisions_20261019.xlsx and one csv per tab
#
# Tabs
#   decisionsByTaxon  counts of each decision per taxon and direction
#   sheetEdits        names to add to the sheet's include lists, with the record counts behind them
#   sheetEditsEffect  what those edits do to each concept, from a dry run of speciesCheck() on the
#                     current build's inputs with the edited synonym list (sheet itself untouched)
#   parserFix         records the review rejects as hybrid formulas; handled in process_gbif_082026.R
#   geographyCheck    one row per added / returning record: state from the coordinates (or the
#                     record's state field), tested against the FNA list the driver filters on, the
#                     NatureServe list on the sheet, and Weakley's range for the two taxa the review
#                     named (rufotomentosa, lincecumii)
#   geographySummary  the same per taxon
#   openDecisions     rows the review left to us
#   notesForAnne      answers to the questions in her comments
suppressPackageStartupMessages({library(dplyr); library(readr); library(stringr); library(tidyr); library(openxlsx); library(terra)})
options(width = 220)
outStem <- "work2026/taxaReviewAnne/reviewDecisions_20261019"

# --- the review ------------------------------------------------------------------------------------
rv <- read.xlsx("work2026/taxaReviewAnne/taxonChangeRawData_20260924v2.xlsx", sheet = "records") |>
  mutate(decision = str_squish(coalesce(AF.decision, "")), comment = str_squish(coalesce(AF.Comment, "")),
         decisionClass = case_when(
           decision == "n/a" ~ "n/a (paired: see the addition)",
           str_detect(decision, "^Agree but") ~ "Agree with concerns",
           str_detect(decision, "^Agree") ~ "Agree",
           str_detect(decision, "^Disagree") ~ "Disagree",
           str_detect(decision, "^Unclear") ~ "Unclear",
           str_detect(decision, "^Review") ~ "Review",
           TRUE ~ "none"),
         name = coalesce(gbif_scientificName, projectRecordedName),
         nameStem = str_squish(str_remove(name, "\\s+[A-Z(].*$")),
         # what the decision means for the record, given its direction
         effect = case_when(
           decisionClass == "n/a (paired: see the addition)" ~ "record moved; decided on the addition row",
           change == "Removed" & decisionClass == "Agree" & str_detect(comment, "^Counterpart is") ~ "stays out of this concept; add to the counterpart named in the comment",
           change == "Removed" & decisionClass == "Agree" ~ "stays out",
           change == "Removed" & decisionClass == "Disagree" ~ "should return",
           change == "Added" & decisionClass %in% c("Agree", "Agree with concerns") ~ "stays in",
           change == "Added" & decisionClass == "Disagree" ~ "should leave",
           TRUE ~ "open"))
stopifnot(all(rv$decisionClass != "none"))

decisionsByTaxon <- rv |> count(species, change, decisionClass, effect, name = "records") |>
  arrange(species, desc(change), decisionClass)

# --- 1. sheet edits: names the review wants inside a concept -------------------------------------
# One row per (concept, name). The name is the parsed form the pipeline matches on
# (preprocessing/functions/process_gbif_082026.R: "Genus epithet [var.|subsp.|f. epithet]").
# `reviewRecords` counts the records behind the decision; `recordsNowExcluded` counts every
# record under that name in the current build's excludedOnTaxonomy file, which is what a sheet
# edit would bring in (the review saw only the records the published dataset had held).
sheetEdits <- tribble(
  ~concept, ~name, ~decisionSource, ~basis,
  # Disagree with the removal: the name is a synonym of the concept
  "Vitis aestivalis", "Vitis smalliana", "Removed / Disagree", "Treated as a synonym by FNA and not part of the varieties that were elevated",
  "Vitis aestivalis", "Vitis araneosa", "Removed / Disagree", "V. araneosa is a synonym in FNA",
  "Vitis aestivalis", "Vitis intermedia", "Removed / Disagree", "Not a synonym of V. aestivalis in FNA but it is in World Flora Online",
  "Vitis aestivalis", "Vitis labrusca var. aestivalis", "Removed / Disagree", "Listed as synonym of V. aestivalis in FNA",
  "Vitis berlandieri", "Vitis cordifolia var. helleri", "Removed / Disagree", "This is a synonym of V. berlandieri",
  "Vitis cinerea var. tomentosa", "Vitis berlandieri var. tomentosa", "Removed / Disagree", "Homotypic synonym of V. cinerea var. tomentosa per POWO; counterpart if specimens are within geographic range",
  "Vitis labrusca", "Vitis catawba", "Removed / Disagree", "FNA does not include it as a synonym, most other sources do. Important to check distribution",
  "Vitis labrusca", "Vitis blanda", "Removed / Disagree", "FNA does not include it as a synonym, most other sources do. Important to check distribution",
  "Vitis labrusca", "Vitis obovata", "Removed / Disagree", "FNA does not include it as a synonym, most other sources do. Important to check distribution",
  "Vitis labrusca", "Vitis labrusca f. alba", "Removed / Disagree", "The form may not be recognized but at the species level this is okay to include. Important to check distribution",
  "Vitis popenoei", "Muscadinia popenoei", "Removed / Disagree", "Muscadinia popenoei is a synonym of V. popenoei",
  "Vitis riparia", "Vitis vulpina var. praecox", "Removed / Disagree", "Accepted synonym of V. riparia",
  "Vitis tiliifolia", "Vitis caribaea", "Removed / Disagree", "Accepted (heterotypic) synonym of V. tiliifolia per POWO",
  "Vitis tiliifolia", "Vitis arachnoidea", "Removed / Disagree", "Accepted (heterotypic) synonym of V. tiliifolia per POWO",
  "Vitis tiliifolia", "Vitis acuminata", "Removed / Disagree", "Accepted (heterotypic) synonym of V. tiliifolia per POWO",
  "Vitis vulpina", "Vitis pullaria", "Removed / Disagree", "V. pullaria is a good synonym of V. vulpina per FNA",
  # Agree with the removal from V. cinerea, but the record belongs to the counterpart named
  "Vitis berlandieri", "Vitis helleri", "Removed from V. cinerea / Agree, counterpart", "Counterpart is V. berlandieri per FNA",
  "Vitis baileyana", "Vitis virginiana", "Removed from V. cinerea / Agree, counterpart", "Counterpart is V. baileyana",
  "Vitis simpsonii", "Vitis austrina", "Removed from V. cinerea / Agree, counterpart", "Counterpart is V. simpsonii",
  "Vitis simpsonii", "Vitis sola", "Removed from V. cinerea / Agree, counterpart", "Counterpart is V. simpsonii",
  "Vitis cinerea", "Vitis aestivalis var. cinerea", "Removed from V. cinerea and var. cinerea / Agree, counterpart", "Counterpart is V. cinerea (both rows)",
  "Vitis cinerea var. cinerea", "Vitis canescens", "Removed from V. cinerea / Agree, counterpart", "Counterpart is V. cinerea var. cinerea",
  # already on the sheet (edited 2026-10-05, re-run under run10022026_1k)
  "Vitis acerifolia", "Vitis novomexicana", "Removed / Disagree (applied 2026-10-05)", "Included as synonym of V. acerifolia per FNA",
  "Vitis shuttleworthii", "Vitis coriacea", "Removed / Disagree, include record (applied 2026-10-05)", "Per FNA, V. coriacea is a synonym of V. shuttleworthii") |>
  mutate(column = "Names to include in this concept (Homotypic synonyms)")

sheet <- read_csv("data/New World Vitis.csv", col_types = cols(.default = "c"), show_col_types = FALSE)
splitCell <- function(x) { if (is.na(x)) return(character(0)); str_squish(str_replace(unlist(str_split(x, "\\s*[,;]\\s*")), "^V\\.\\s*", "Vitis ")) }
incl <- sheet |> transmute(concept = `Scientific Name`, inc = `Names to include in this concept (Homotypic synonyms)`)
ex <- read_csv("data/processed_occurrence/excludedOnTaxonomy_10022026.csv", col_types = cols(.default = "c"), show_col_types = FALSE)
exCount <- ex |> count(taxon, name = "recordsNowExcluded")
exXY <- ex |> group_by(taxon) |> summarise(nowExcludedWithCoordinates = sum(!is.na(latitude) & !is.na(longitude)), .groups = "drop")
# distinct records: a record held by two concepts (V. cinerea and a variety) has two removal rows
reviewRows <- rv |> filter(change == "Removed", effect %in% c("should return", "stays out of this concept; add to the counterpart named in the comment")) |>
  distinct(nameStem, gbif_gbifID, sourceUniqueID) |> count(nameStem, name = "reviewRecords")
sheetEdits <- sheetEdits |>
  left_join(incl, by = "concept") |>
  mutate(alreadyOnSheet = mapply(function(n, cell) n %in% splitCell(cell), name, inc)) |>
  select(-inc) |>
  left_join(reviewRows, by = c("name" = "nameStem")) |>
  left_join(exCount, by = c("name" = "taxon")) |> left_join(exXY, by = c("name" = "taxon")) |>
  mutate(across(c(reviewRecords, recordsNowExcluded, nowExcludedWithCoordinates), ~ coalesce(.x, 0L)),
         status = if_else(alreadyOnSheet, "on the sheet already", "add to the sheet")) |>
  relocate(column, .after = concept) |> relocate(status, .after = name)

# --- 2. dry run of the edits: speciesCheck() on the current build's inputs ------------------------
# Rebuilds the bound input exactly as preprocessingUpdates2026_08_20.R does (its readAndBind step,
# on the 10022026 files), then assigns concepts with the current synonym list and with the edited
# one. Counts are before de-duplication and the coordinate checks, so they bound the change.
source("preprocessing/functions/standardizeNames.R")
source("preprocessing/functions/speciesStandardization.R")
rd <- function(f) read_csv(f, col_types = cols(.default = "c"), show_col_types = FALSE, progress = FALSE)
suffix <- "10022026"
bound <- bind_rows(
  rd(paste0("data/processed_occurrence/gbif_", suffix, ".csv")), rd("data/processed_occurrence/grin.csv"),
  rd("data/processed_occurrence/wiews.csv"), rd(paste0("data/processed_occurrence/genesys_", suffix, ".csv")),
  rd(paste0("data/processed_occurrence/botanicalGardenSurvey_", suffix, ".csv")), rd("data/processed_occurrence/pnas2020.csv"),
  rd("data/processed_occurrence/UCDavis.csv"), rd(paste0("data/processed_occurrence/jun_", suffix, ".csv")),
  rd(paste0("data/processed_occurrence/mexicoRecords_", suffix, ".csv"))) |>
  mutate(index = row_number()) |> select(index, everything()) |>
  filter(!databaseSource %in% c("FAO 2019 (WIEWS)", "GBIF 2019", "Global Crop Diversity Trust 2019a (Genesys)",
                                "Global Crop Diversity Trust 2019b  (Cwr Occ)", "USDA ARS NPGS 2019a", "Midwest Herbaria 2019", "BGCI 2019 (PlantSearch)"))
if ("Collecting number" %in% names(bound)) bound <- select(bound, -`Collecting number`)
bound <- standardizeNames(bound)
synNow <- sheet |> select(taxon = `Scientific Name`, acceptedSynonym = `Names to include in this concept (Homotypic synonyms)`,
                          excludeNames = `Names to exclude from this concept`, modelSpecies = `Include in gap analysis?`)
toAdd <- sheetEdits |> filter(status == "add to the sheet") |> group_by(concept) |> summarise(add = paste(name, collapse = ", "), .groups = "drop")
synEdited <- synNow |> left_join(toAdd, by = c("taxon" = "concept")) |>
  mutate(acceptedSynonym = case_when(is.na(add) ~ acceptedSynonym,
                                     is.na(acceptedSynonym) | acceptedSynonym %in% c("", "NA", "n/a") ~ add,
                                     TRUE ~ paste(acceptedSynonym, add, sep = ", "))) |> select(-add)
cat("dry run: speciesCheck() with the current sheet ...\n"); nowRun <- speciesCheck(bound, synNow)
cat("dry run: speciesCheck() with the edited sheet ...\n"); editRun <- speciesCheck(bound, synEdited)
conceptCount <- function(r, nm) r$includedData |> count(taxon, name = nm)
sheetEditsEffect <- full_join(conceptCount(nowRun, "recordsCurrentSheet"), conceptCount(editRun, "recordsEditedSheet"), by = "taxon") |>
  mutate(across(where(is.integer), ~ coalesce(.x, 0L)), change = recordsEditedSheet - recordsCurrentSheet) |>
  filter(change != 0) |>
  left_join(editRun$includedData |> anti_join(nowRun$includedData, by = c("index", "taxon")) |>
              count(taxon, originalTaxon) |> group_by(taxon) |> summarise(namesEntering = paste0(originalTaxon, " ", n, collapse = "; "), .groups = "drop"), by = "taxon") |>
  mutate(note = case_when(
    taxon %in% c("Vitis cinerea var. cinerea", "Vitis cinerea var. tomentosa") ~ "the V. cinerea model pools the varieties (taxonRecords), so these also reach the V. cinerea model",
    taxon == "Vitis tiliifolia" ~ "counts are before the country step; tiliifolia keeps non-North-American records (exempt from the USA/CAN/MEX filter)",
    TRUE ~ "")) |>
  arrange(desc(abs(change)))
cat("records entering concepts after the edits (before dedup / coordinate checks):\n"); print(as.data.frame(sheetEditsEffect |> select(-namesEntering)), row.names = FALSE)

# --- 3. parser fix: additions the review rejects as hybrid formulas -------------------------------
parserFix <- rv |> filter(change == "Added", decisionClass == "Disagree") |>
  transmute(species, decision, comment, projectRecordedName, gbif_scientificName, gbif_verbatimScientificName, databaseSource, sourceUniqueID, gbif_gbifID,
            handledBy = "process_gbif_082026.R: parseVitisName() now treats a standalone ' x ' anywhere after the binomial as a hybrid formula (record assigned to no concept) [edit 2026-10-19]")

# --- 4. geography check -----------------------------------------------------------------------
states <- vect("data/geospatial_datasets/states/ne_10m_admin_1_states_provinces.gpkg")
stateTab <- as.data.frame(states)[, c("name", "adm0_a3", "postal")]
# NatureServe codes and bare two-letter state strings are US postal codes; the world table reuses
# the same letters for other countries' provinces, so decode those against the US rows only
usTab <- stateTab[stateTab$adm0_a3 == "USA", ]
fna <- read_csv("data/source_data/FNA_stateClassification.csv", col_types = cols(.default = "c"), show_col_types = FALSE) |>
  filter(nchar(`States from FNA`) > 3) |>
  transmute(taxon = `Taxon Name`, fnaStates = lapply(str_split(`States from FNA`, ",\\s*"), function(s) { s <- str_squish(s); s[s != ""] |> str_replace("^D\\.C$", "District of Columbia") }))
# the driver filters a taxon only when its concept name is in the file; FNA's variety-level row for
# munsoniana ("Vitis rotundifolia var. munsoniana") is therefore not applied to V. munsoniana today
fnaFor <- function(tx) { i <- match(tx, fna$taxon); if (is.na(i)) return(NULL); fna$fnaStates[[i]] }
nsFor <- function(tx) { d <- sheet$Distribution[match(tx, sheet$`Scientific Name`)]; if (is.na(d)) return(NULL)
  d <- str_replace_all(d, "\\([^)]*\\)", ""); unique(str_extract_all(d, "\\b[A-Z]{2}\\b")[[1]]) }
# the review's guidance for taxa FNA does not treat as distinct (Weakley, Flora of the Southeastern US)
weakleyRange <- list("Vitis rufotomentosa" = c("FL", "GA", "SC", "NC"), "Vitis lincecumii" = c("AR", "OK", "LA", "TX"))
lincecumiiRange <- c("AR", "OK", "LA", "TX")

# records to check: additions the review keeps (or left open), and records that return via the edits
added <- rv |> filter(change == "Added", decisionClass != "Disagree") |>
  transmute(taxon = species, source = "addition in the review", decisionClass, comment, name, databaseSource, sourceUniqueID, gbif_gbifID,
            latitude, longitude, iso3, stateText = gbif_stateProvince, yearRecorded, basisOfRecord = gbif_basisOfRecord, institutionCode)
returning <- editRun$includedData |> anti_join(nowRun$includedData, by = c("index", "taxon")) |>
  transmute(taxon, source = "returns via a sheet edit", decisionClass = "Disagree with removal / counterpart", comment = "", name = originalTaxon, databaseSource, sourceUniqueID,
            gbif_gbifID = NA_character_, latitude, longitude, iso3, stateText = state, yearRecorded, basisOfRecord = NA_character_, institutionCode)
geo <- bind_rows(added, returning) |>
  mutate(lat = suppressWarnings(as.numeric(latitude)), lon = suppressWarnings(as.numeric(longitude)), hasXY = !is.na(lat) & !is.na(lon))
pts <- geo |> filter(hasXY)
hit <- terra::extract(states, vect(pts, geom = c("lon", "lat"), crs = "EPSG:4326"))
pts$stateFromXY <- hit$name; pts$countryFromXY <- hit$adm0_a3; pts$postalFromXY <- hit$postal
geo <- geo |> left_join(pts |> select(taxon, source, name, databaseSource, sourceUniqueID, lat, lon, stateFromXY, countryFromXY, postalFromXY) |> distinct(),
                        by = c("taxon", "source", "name", "databaseSource", "sourceUniqueID", "lat", "lon"))
# state from text when there are no coordinates: match the record's state string to a state name or postal code
textState <- function(s) { s <- str_squish(coalesce(s, "")); if (s == "") return(NA_character_)
  i <- match(tolower(s), tolower(stateTab$name)); if (!is.na(i)) return(stateTab$name[i])
  i <- match(toupper(s), usTab$postal); if (!is.na(i)) return(usTab$name[i]); NA_character_ }
geo <- geo |> mutate(
  stateUsed = if_else(!is.na(stateFromXY), stateFromXY, vapply(stateText, textState, character(1))),
  stateSource = case_when(!is.na(stateFromXY) ~ "coordinates", !is.na(stateUsed) ~ "record's state field", TRUE ~ "none"),
  postalUsed = stateTab$postal[match(stateUsed, stateTab$name)],
  countryUsed = coalesce(countryFromXY, stateTab$adm0_a3[match(stateUsed, stateTab$name)], iso3))
checkOne <- function(tx, st, po, co) {
  f <- fnaFor(tx); n <- nsFor(tx); w <- weakleyRange[[tx]]
  inF <- if (is.null(f)) NA else st %in% f
  inN <- if (is.null(n)) NA else po %in% n
  inW <- if (is.null(w)) NA else po %in% w
  c(fna = if (is.null(f)) "no FNA list (driver applies no filter)" else if (is.na(st)) "no state" else if (inF) "in" else "out",
    natureServe = if (is.null(n)) "no list on the sheet" else if (is.na(po)) "no state" else if (inN) "in" else "out",
    weakley = if (is.null(w)) "" else if (is.na(po)) "no state" else if (inW) "in" else "out")
}
chk <- t(mapply(checkOne, geo$taxon, geo$stateUsed, geo$postalUsed, geo$countryUsed))
geo <- bind_cols(geo, as_tibble(chk)) |>
  mutate(verdict = case_when(
    !(countryUsed %in% c("USA", "CAN")) & !is.na(countryUsed) ~ paste0("outside USA/CAN (", countryUsed, "): state lists do not apply"),
    is.na(stateUsed) ~ "no state: cannot check (no coordinates in the model anyway)",
    weakley == "out" ~ "OUT of Weakley's range",
    weakley == "in" ~ "in Weakley's range",
    fna == "out" ~ "OUT of the FNA list",
    fna == "in" ~ "in the FNA list",
    natureServe == "out" ~ "OUT of the NatureServe list",
    natureServe == "in" ~ "in the NatureServe list",
    TRUE ~ "no range list to check against"),
    # what an out-of-range verdict means for the model: only points reach the SDM, and the driver's
    # FNA filter removes out-of-list points for taxa with an FNA row
    modelEffect = case_when(
      !str_detect(verdict, "^OUT") ~ "",
      stateSource != "coordinates" ~ "no coordinates: counted in the record total, never in the model",
      fna == "out" ~ "the driver's FNA filter drops this point before modelling",
      TRUE ~ "reaches the model: no FNA filter for this taxon"),
    flag = case_when(taxon == "Vitis rufotomentosa" & postalUsed %in% lincecumiiRange ~ "rufotomentosa record inside the lincecumii range",
                     taxon == "Vitis lincecumii" & postalUsed %in% weakleyRange[["Vitis rufotomentosa"]] ~ "lincecumii record inside the rufotomentosa range",
                     TRUE ~ "")) |>
  select(taxon, source, decisionClass, name, databaseSource, sourceUniqueID, gbif_gbifID, institutionCode, yearRecorded, basisOfRecord,
         latitude, longitude, iso3, stateText, stateUsed, stateSource, countryUsed, fna, natureServe, weakley, verdict, modelEffect, flag, comment) |>
  arrange(taxon, desc(str_detect(verdict, "^OUT")), source, name)
geographySummary <- geo |> group_by(taxon) |>
  summarise(records = n(), withCoordinates = sum(stateSource == "coordinates"), stateFromText = sum(stateSource == "record's state field"), noState = sum(stateSource == "none"),
            outsideUSACAN = sum(str_detect(verdict, "^outside")), inRange = sum(str_detect(verdict, "^in ")), outOfRange = sum(str_detect(verdict, "^OUT")),
            outOfRange_points = sum(str_detect(verdict, "^OUT") & stateSource == "coordinates"),
            outOfRange_pointsReachingModel = sum(modelEffect == "reaches the model: no FNA filter for this taxon"),
            outOfRange_textStateOnly = sum(str_detect(verdict, "^OUT") & stateSource != "coordinates"),
            outOfRangeStates_points = paste(sort(unique(stateUsed[str_detect(verdict, "^OUT") & stateSource == "coordinates"])), collapse = ", "),
            outOfRangeStates_text = paste(sort(unique(stateUsed[str_detect(verdict, "^OUT") & stateSource != "coordinates"])), collapse = ", "),
            flagged = sum(flag != ""), fnaFilterApplies = if_else(is.null(fnaFor(first(taxon))), "no", "yes"),
            rangeUsed = case_when(!is.null(weakleyRange[[first(taxon)]]) ~ "Weakley (review guidance)", !is.null(fnaFor(first(taxon))) ~ "FNA list (driver filter)",
                                  !is.null(nsFor(first(taxon))) ~ "NatureServe list on the sheet", TRUE ~ "none"), .groups = "drop") |>
  arrange(desc(outOfRange_pointsReachingModel), desc(outOfRange_points), desc(records))

# --- 5. open decisions and answers ----------------------------------------------------------------
openDecisions <- tribble(
  ~item, ~records, ~review, ~proposal,
  "V. bicolor records removed from V. aestivalis (87; 3 spellings of the name)", 87L,
  "Unclear: agree to remove from V. aestivalis species level, but perhaps treat as counterpart to var. bicolor. 'If we are recognizing var. bicolor as distinct, why not move these records to var. bicolor?'",
  "Add 'Vitis bicolor' to the include list of V. aestivalis var. bicolor (the V. aestivalis model pools the varieties, so the records reach the species model through the variety). 13 of the 87 have coordinates.",
  "V. argentifolia additions to V. aestivalis var. bicolor (9)", 9L,
  "Unclear: remove from var. bicolor and perhaps add to V. aestivalis at the species level to be safe; otherwise each specimen would need checking.",
  "Remove 'Vitis argentifolia' and 'Vitis aestivalis var. argentifolia' from the var. bicolor include list; they stay on the V. aestivalis include list (already there). Note the tension with the item above: var. bicolor gains V. bicolor but loses V. argentifolia (FNA lists var. argentifolia under V. aestivalis).",
  "V. odoratissima records removed from V. riparia (6)", 6L,
  "Unclear: potentially okay to include but without spatial data it doesn't seem to matter.",
  "Leave out. None has coordinates, so they would change the record count only.",
  "V. rufotomentosa addition with no data (1, DEU456)", 1L, "Unclear: not enough data.",
  "Keep as it stands (no coordinates; no effect on the model).",
  "V. tiliifolia removal, UC Davis record named 'tiliifolia' (1)", 1L, "Unclear: not enough data here to assess.",
  "Leave out. Non-taxonomic difference (reason = other).",
  "V. rufotomentosa iNaturalist additions (9)", 9L, "Agree but with concerns that an iNaturalist observation can be correctly identified to rufotomentosa. Very important to check distribution.",
  "See geographyCheck: the Alabama and inland North Carolina observations fall outside Weakley's range; the Florida ones are inside. Decide whether to keep observations at all for this taxon.",
  "V. martineziana additions (2)", 2L, "Review: unclear how the species was determined to be martineziana from GBIF.",
  "Answered: not GBIF records (Jun Wen pers. comm. and the Huerta-Acosta paper). Keep.",
  "V. novogranatensis (1 removed, 1 added)", 2L, "Unclear: no data to evaluate.",
  "Hand-coded published rows; a driver one-off, not taxonomy. Unchanged.")

notesForAnne <- tribble(
  ~question, ~answer,
  "V. girdiana: 'all names are V. girdiana then why removed?'",
  "Not a taxonomy change. The record (gbifID 2573168105, VT, 2001) sits at longitude -177.28, a coordinate error the published run removed by hand; the current driver removes it the same way. Reason code dec16Driver.",
  "V. riparia: 'Name seems to be riparia throughout so not sure why removed'",
  "Same: the record (gbifID 4072319870, MUHW, 1992) sits at latitude 82.23, the other hand-removed coordinate error. Reason code dec16Driver.",
  "V. aestivalis var. aestivalis: 'This is a counterpart to same record under aestivalis species, yes?'",
  "Yes. The one V. labrusca var. aestivalis record was held by both the species and the variety; it returns to V. aestivalis (species) only.",
  "V. tiliifolia: 'I'm confused as to why these weren't in the published dataset' (245 additions)",
  "Two causes. 235 are named V. tiliifolia but the GBIF backbone had folded them elsewhere or the old parser could not read the name; 10 carry the name only in the publisher's verbatim field. The country exemption for tiliifolia also keeps Central American and Caribbean records the published build dropped.",
  "V. x champinii / x doaniana / x novae-angliae additions (101)",
  "These carry only 'Vitis L.' as GBIF's interpreted name; the hybrid name is in the publisher's verbatim field, which the current parser reads. Agreed in the review.",
  "V. x champinii / x doaniana / x novae-angliae: where the additions come from",
  "All but 5 are records of the global crop wild relative occurrence database (Castaneda-Alvarez et al., GBIF dataset 07044577), which republishes germplasm accessions (Bioversity, USDA NPGS) as occurrences; the x champinii ones are Olmo's UC Davis Armstrong Vineyard accessions. The pipeline types every non-living GBIF record as H, as the published build did for the same dataset, so they count as herbarium records without coordinates. Not a taxonomy question, but it is what moves x champinii from MP to HP.",
  "UC Davis (DAV) 1958 Olmo vouchers",
  "Herbarium vouchers of vineyard plants at Davis, georeferenced to the vineyard (38.54, -121.79). Under V. cinerea and V. rotundifolia the FNA state filter removed them; under V. simpsonii (31 points) and V. munsoniana (22) nothing does, because those taxa have no FNA row. See rangeStatesProposed.",
  "Geography filter",
  "The model driver filters each taxon's points to the FNA state list (R2/dataProcessing/fnaFilter.R). Taxa FNA does not treat as distinct have no list and so no filter: rufotomentosa, lincecumii, munsoniana, baileyana, berlandieri, simpsonii. The geographyCheck tab tests them against Weakley (rufotomentosa, lincecumii, as you set out) and the NatureServe list on the sheet.")

# --- 6. proposed range lists for the taxa the driver cannot filter today ---------------------------
# The driver's filter (applyFNA) acts only on taxa named in FNA_stateClassification.csv. The taxa
# elevated from varieties have no row, so their out-of-range points reach the SDM unfiltered. These
# are the lists the review's guidance points to (Weakley where given; FNA's variety row; otherwise
# the NatureServe list on the sheet, pending a Weakley check, which this machine cannot fetch).
nsNames <- function(tx) { p <- nsFor(tx); usTab$name[match(p, usTab$postal)] |> na.omit() |> as.character() }
rangeStatesProposed <- tribble(
  ~taxon, ~states, ~source, ~note,
  "Vitis rufotomentosa", "Florida, Georgia, South Carolina, North Carolina", "Weakley (review email, 2026-10-19): 'Florida to se. North Carolina, and perhaps more widespread'",
    "Keeps the range disjoint from V. lincecumii as the review asks. The Maryland (NCU/TAWES 2023) and Alabama (iNaturalist) points fall outside.",
  "Vitis lincecumii", "Arkansas, Oklahoma, Louisiana, Texas", "Weakley (review email): 'Arkansas and Oklahoma south to w. Louisiana and e. and c. Texas'",
    "Two Kansas points fall outside.",
  "Vitis munsoniana", "Alabama, Florida, Georgia", "FNA row 'Vitis rotundifolia var. munsoniana' (already in FNA_stateClassification.csv under the variety name)",
    "22 UC Davis vineyard vouchers (California) and 3 Texas points would be dropped.",
  "Vitis simpsonii", paste(nsNames("Vitis simpsonii"), collapse = ", "), "NatureServe list on the sheet; Weakley not yet checked",
    "31 UC Davis vineyard vouchers (California), 1 Louisiana and 1 District of Columbia point would be dropped. Under V. cinerea these points were filtered; under V. simpsonii nothing filters them.",
  "Vitis baileyana", paste(nsNames("Vitis baileyana"), collapse = ", "), "NatureServe list on the sheet; Weakley not yet checked",
    "6 points (Maryland, Mississippi) would be dropped.",
  "Vitis berlandieri", "", "none proposed",
    "NatureServe gives NM, TX plus Mexico. applyFNA keeps only points whose state is on the list, so a US-only list would drop the Mexican points (about half the taxon). Leave unfiltered or add the Mexican states.")

# --- write -----------------------------------------------------------------------------------------
tabs <- list(decisionsByTaxon = decisionsByTaxon, sheetEdits = sheetEdits, sheetEditsEffect = sheetEditsEffect, parserFix = parserFix,
             geographySummary = geographySummary, geographyCheck = geo, rangeStatesProposed = rangeStatesProposed, openDecisions = openDecisions, notesForAnne = notesForAnne)
wb <- createWorkbook()
for (n in names(tabs)) { addWorksheet(wb, n); writeData(wb, n, tabs[[n]], headerStyle = createStyle(textDecoration = "bold")); freezePane(wb, n, firstRow = TRUE); setColWidths(wb, n, 1:ncol(tabs[[n]]), "auto") }
saveWorkbook(wb, paste0(outStem, ".xlsx"), overwrite = TRUE)
for (n in names(tabs)) write_csv(tabs[[n]], paste0(outStem, "_", n, ".csv"), na = "")

cat("\n== decisions\n"); print(rv |> count(change, decisionClass) |> as.data.frame(), row.names = FALSE)
cat("\n== sheet edits\n"); print(sheetEdits |> select(concept, name, status, reviewRecords, recordsNowExcluded, nowExcludedWithCoordinates) |> as.data.frame(), row.names = FALSE, right = FALSE)
cat("\n== geography\n"); print(as.data.frame(geographySummary), row.names = FALSE, right = FALSE)
cat("\n== out-of-range points\n"); print(geo |> filter(str_detect(verdict, "^OUT"), stateSource == "coordinates") |> select(taxon, source, name, databaseSource, institutionCode, yearRecorded, stateUsed, verdict, modelEffect, flag) |> as.data.frame(), row.names = FALSE, right = FALSE)
