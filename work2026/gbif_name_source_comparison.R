# work2026/gbif_name_source_comparison.R
# Quantify how the GBIF taxonomic backbone reassigns records away from the
# project's taxon concepts (the "V. rufotomentosa problem") and compare three
# ways of deriving a taxon from a GBIF record:
#   backbone   : original processGBIF() - GBIF accepted `species` + rank columns
#   sciName    : parseVitisName(scientificName)         (GBIF interpreted name)
#   verbatim   : parseVitisName(verbatimScientificName) (publisher's name)
#   combined   : scientificName parse, else verbatim parse, else backbone
# Each is pushed through the project synonym list with speciesCheck().
# Outputs (work2026/):
#   gbif_name_source_comparison_by_taxon.csv
#   gbif_backbone_reassignments.csv
#   gbif_unmatched_names.csv
#   gbif_verbatim_vs_scientificName.csv
#   exclusion_step_effect.csv
pacman::p_load(dplyr, readr, stringr, purrr, tidyr, countrycode)
options(width = 220)
source("preprocessing/functions/process_gbif_082026.R")
source("preprocessing/functions/speciesStandardization.R")
source("preprocessing/functions/standardizeNames.R")
source("preprocessing/functions/helperFunctions.R")

vitis2 <- read_csv("data/New World Vitis.csv", col_types = cols(.default = "c")) |>
  dplyr::select(
    "taxon" = "Scientific Name",
    "acceptedSynonym" = "Names to include in this concept (Homotypic synonyms)",
    "excludeNames" = "Names to exclude from this concept",
    "modelSpecies" = "Include in gap analysis?"
  ) |> filter(!is.na(taxon))

raw <- read_tsv("data/source_data/vitisGBIFDownload_20250721.csv", col_types = cols(.default = "c"), progress = FALSE) |>
  filter(is.na(basisOfRecord) | basisOfRecord != "FOSSIL_SPECIMEN") |>
  mutate(index = row_number())
cat("GBIF records (non-fossil):", nrow(raw), "\n")

# ---- three taxon assignments ------------------------------------------------
backbone <- with(raw, case_when(
  taxonRank == "SPECIES" ~ species,
  taxonRank == "GENUS" ~ genus,
  taxonRank == "VARIETY" ~ paste0(species, " var. ", infraspecificEpithet),
  taxonRank == "SUBSPECIES" ~ paste0(species, " subsp. ", infraspecificEpithet),
  TRUE ~ species))
p_sci <- parseVitisName(raw$scientificName)
p_ver <- parseVitisName(raw$verbatimScientificName)
methods <- list(
  backbone = backbone,
  sciName  = ifelse(is.na(p_sci), backbone, p_sci),
  verbatim = ifelse(is.na(p_ver), backbone, p_ver),
  combined = coalesce(p_sci, p_ver, backbone)
)
cat("parse failures -> backbone fallback: scientificName", sum(is.na(p_sci)), " verbatim", sum(is.na(p_ver)), "\n")

# run each through standardizeNames + speciesCheck
# backbone = the pipeline as published (exclude lists were not applied then)
runCheck <- function(taxonVec, applyExclusions = TRUE) {
  d <- tibble(index = raw$index, taxon = taxonVec, originalTaxon = raw$scientificName,
              species = str_remove(taxonVec, "^\\S+\\s+"), genus = str_extract(taxonVec, "^\\S+"))
  d <- standardizeNames(d)
  speciesCheck(d, vitis2, applyExclusions = applyExclusions)
}
res <- imap(methods, ~ runCheck(.x, applyExclusions = (.y != "backbone")))

counts <- imap(res, ~ .x$includedData |> count(taxon, name = .y)) |>
  reduce(full_join, by = "taxon") |>
  mutate(across(-taxon, ~ replace_na(., 0L)))
byTaxon <- vitis2 |> select(taxon, modelSpecies) |>
  left_join(counts, by = "taxon") |>
  mutate(across(c(backbone, sciName, verbatim, combined), ~ replace_na(., 0L)),
         sciName_minus_backbone = sciName - backbone,
         verbatim_minus_backbone = verbatim - backbone,
         combined_minus_backbone = combined - backbone,
         combined_minus_sciName = combined - sciName)
write_csv(byTaxon, "work2026/gbif_name_source_comparison_by_taxon.csv")
cat("\n== records per concept (after synonym + exclude step; before dedup / coordinate checks) ==\n")
print(byTaxon |> arrange(desc(abs(sciName_minus_backbone))), n = 50)
cat("\nexcluded (no concept match): ", paste(imap_chr(res, ~ paste0(.y, "=", nrow(.x$excludedData))), collapse = "  "), "\n")
cat("removed by exclude lists   : ", paste(imap_chr(res, ~ paste0(.y, "=", nrow(.x$excludedByConcept))), collapse = "  "), "\n")
cat("\n== exclude-list removals by concept and source name (scientificName shown), per method ==\n")
exclDetail <- imap(res, ~ .x$excludedByConcept |> mutate(method = .y)) |> bind_rows()
if (nrow(exclDetail)) print(exclDetail |> count(method, excludedFromConcept, originalTaxon, sort = TRUE) |> head(30), n = 30)

cat("\n== records GBIF could not match (scientificName is genus-only) but verbatim name parses ==\n")
genusOnly <- tibble(index = raw$index, sciName = raw$scientificName, p_ver, p_sci) |> filter(is.na(p_sci), !is.na(p_ver))
gConcept <- res$verbatim$includedData |> filter(index %in% genusOnly$index) |> count(taxon, name = "records_recovered_via_verbatim", sort = TRUE)
print(genusOnly |> count(sciName, p_ver, sort = TRUE) |> head(15), n = 15)
print(gConcept, n = 40)

# ---- the rufotomentosa problem: backbone moves a record to another concept ---
conceptOf <- function(r) r$includedData |> group_by(index) |> summarise(concept = paste(sort(unique(taxon)), collapse = " | "), .groups = "drop")
flow <- full_join(conceptOf(res$backbone) |> rename(backbone = concept),
                  conceptOf(res$sciName)  |> rename(sciName = concept), by = "index") |>
  left_join(conceptOf(res$verbatim) |> rename(verbatim = concept), by = "index") |>
  mutate(across(c(backbone, sciName, verbatim), ~ replace_na(., "<no concept>"))) |>
  left_join(raw |> select(index, scientificName, verbatimScientificName, gbif_species = species), by = "index")

reassign <- flow |> filter(backbone != sciName) |>
  count(name_concept = sciName, backbone_concept = backbone, scientificName, sort = TRUE)
write_csv(reassign, "work2026/gbif_backbone_reassignments.csv")
cat("\n== backbone reassignments: concept by recorded name vs concept by GBIF backbone (top 40) ==\n")
print(reassign |> count(name_concept, backbone_concept, wt = n, sort = TRUE) |> head(40), n = 40)

cat("\n-- why does the backbone drop records whose scientificName is a project taxon? --\n")
print(flow |> filter(backbone == "<no concept>", sciName != "<no concept>") |>
  left_join(raw |> select(index, taxonRank, backbone_species = species), by = "index") |>
  count(sciName, scientificName, taxonRank, backbone_species, sort = TRUE) |> head(12), n = 12)

# per-taxon summary of the rufotomentosa-type loss / gain
perTaxon <- bind_rows(
  flow |> filter(backbone != sciName, sciName != "<no concept>") |> separate_rows(sciName, sep = " \\| ") |>
    count(taxon = sciName, name = "recorded_as_taxon_but_backbone_moved_elsewhere"),
  flow |> filter(backbone != sciName, backbone != "<no concept>") |> separate_rows(backbone, sep = " \\| ") |>
    count(taxon = backbone, name = "backbone_added_from_other_names")
) |> group_by(taxon) |> summarise(across(everything(), ~ sum(., na.rm = TRUE)), .groups = "drop")
cat("\n== per taxon: records the backbone took away / added (scientificName parse as reference) ==\n")
print(perTaxon |> arrange(desc(recorded_as_taxon_but_backbone_moved_elsewhere)), n = 50)

# ---- names surfacing that match no concept, checked against include / exclude lists ----
allInclude <- normalizeName(unlist(map(vitis2$acceptedSynonym, splitNames)))
allExclude <- unlist(map(seq_len(nrow(vitis2)), \(i) { e <- splitNames(vitis2$excludeNames[i]); if (length(e)) paste0(vitis2$taxon[i], " excludes ", e) else character(0) }))
exclNorm <- normalizeName(str_remove(allExclude, "^.* excludes "))
unmatched <- res$sciName$excludedData |> count(taxon, sort = TRUE) |>
  filter(str_detect(taxon, "^(Vitis|Muscadinia) ")) |>
  left_join(flow |> select(index, backbone) |> inner_join(res$sciName$excludedData |> select(index, taxon), by = "index") |>
              count(taxon, backbone) |> group_by(taxon) |> slice_max(n, n = 1, with_ties = FALSE) |> select(taxon, backbone_concept = backbone), by = "taxon") |>
  mutate(in_any_include_list = normalizeName(taxon) %in% allInclude,
         in_exclude_list_of = map_chr(normalizeName(taxon), \(x) paste(str_remove(allExclude[exclNorm == x], " excludes.*"), collapse = "; ")))
write_csv(unmatched, "work2026/gbif_unmatched_names.csv")
cat("\n== names with no concept match (scientificName parse), with old backbone destination (top 30) ==\n")
print(unmatched |> head(30), n = 30)

# ---- verbatim vs scientificName ---------------------------------------------
pairDist <- tibble(p_ver, p_sci) |> distinct() |> filter(!is.na(p_ver), !is.na(p_sci)) |>
  mutate(dist = mapply(\(a, b) drop(adist(a, b)), p_ver, p_sci))
vs <- tibble(index = raw$index, verbatim = raw$verbatimScientificName, sciName = raw$scientificName, p_ver, p_sci) |>
  left_join(pairDist, by = c("p_ver", "p_sci")) |>
  mutate(kind = case_when(
    is.na(p_ver) & is.na(p_sci) ~ "neither parses",
    is.na(p_ver) ~ "verbatim not parseable",
    is.na(p_sci) ~ "scientificName not parseable",
    p_ver == p_sci ~ "same",
    str_remove(p_ver, "\\s+(var\\.|subsp\\.|f\\.).*") == str_remove(p_sci, "\\s+(var\\.|subsp\\.|f\\.).*") ~ "same species, rank/infraspecific differs",
    dist <= 2 ~ "spelling normalised",
    TRUE ~ "different name"))
cat("\n== verbatimScientificName vs scientificName ==\n")
print(vs |> count(kind, sort = TRUE))
vsTab <- vs |> filter(!kind %in% c("same", "neither parses")) |> count(kind, p_ver, p_sci, sort = TRUE)
write_csv(vsTab, "work2026/gbif_verbatim_vs_scientificName.csv")
cat("\n-- 'different name' cases (GBIF matched the verbatim string to a different name), top 25 --\n")
print(vsTab |> filter(kind == "different name") |> head(25), n = 25)
cat("\n-- concept-level disagreements verbatim vs sciName --\n")
print(flow |> filter(verbatim != sciName) |> count(verbatim_concept = verbatim, sciName_concept = sciName, sort = TRUE) |> head(20), n = 20)

# ---- specific counts asked for ----------------------------------------------
cat("\n== V. vulpina synonym cell (semicolon) ==\n")
cord <- tibble(p = methods$sciName, b = flow$backbone[match(raw$index, flow$index)]) |>
  filter(str_to_sentence(p) %in% c("Vitis cordifolia", "Vitis cordifolia var. sempervirens"))
print(cord |> count(p, b))
cat("\n== V. munsoniana self-synonym double count ==\n")
mun <- sum(str_to_sentence(methods$sciName) == "Vitis munsoniana", na.rm = TRUE)
cat("records named Vitis munsoniana:", mun, " -> old speciesCheck produced", 2 * mun, "rows\n")
pub <- read_csv("data/datasetsForPublication/allSpeciesOccurrences.csv", col_types = cols(.default = "c"), progress = FALSE)
pm <- pub |> filter(taxon == "Vitis munsoniana", databaseSource == "GBIF")
cat("publication file munsoniana GBIF rows:", nrow(pm), " distinct sourceUniqueID:", n_distinct(pm$sourceUniqueID),
    " duplicated rows:", sum(duplicated(pm |> select(-any_of(c("index", "recordID"))))), "\n")

# ---- exclusion step on the full compiled dataset ----------------------------
cat("\n== exclude-list step on the compiled dataset (fresh GBIF + 072025 files for other sources) ==\n")
standardColumnNames <- c("taxon","originalTaxon","genus","species","latitude","longitude","databaseSource","institutionCode","type",
  "sourceUniqueID","sampleCategory","country","iso3","localityInformation","biologicalStatus","collectionSource","finalOriginStat",
  "yearRecorded","county","countyFIPS","state","stateFIPS","coordinateUncertainty","observerName","recordID")
gbif <- processGBIF("data/source_data/vitisGBIFDownload_20250721.csv", nameSource = "scientificName") |>
  orderNames(names = standardColumnNames) |> removeDuplicatesID()
rd <- function(f) read_csv(paste0("data/processed_occurrence/", f), col_types = cols(.default = "c"), progress = FALSE)
df <- bind_rows(gbif, rd("grin.csv"), rd("wiews.csv"), rd("genesys_072025.csv"), rd("botanicalGardenSurvey_072025.csv"),
                rd("pnas2020.csv"), rd("UCDavis.csv"), rd("jun_072025.csv"), rd("mexicoRecords_082025.csv")) |>
  mutate(index = row_number()) |>
  filter(!databaseSource %in% c("FAO 2019 (WIEWS)","GBIF 2019","Global Crop Diversity Trust 2019a (Genesys)",
    "Global Crop Diversity Trust 2019b  (Cwr Occ)","USDA ARS NPGS 2019a","Midwest Herbaria 2019","BGCI 2019 (PlantSearch)"))
full <- speciesCheck(standardizeNames(df), vitis2)
excl <- full$excludedByConcept |> count(excludedFromConcept, databaseSource, originalTaxon, sort = TRUE)
# what the exclude lists WOULD have removed under the old backbone assignment on the same compiled data
gbifB <- gbif |> mutate(taxon = str_to_sentence(backbone[match(sourceUniqueID, raw$occurrenceID)]))
dfB <- bind_rows(gbifB, df |> filter(databaseSource != "GBIF")) |> mutate(index = row_number())
fullB <- speciesCheck(standardizeNames(dfB), vitis2)
cat("compiled data, backbone assignment: removed by exclude lists =", nrow(fullB$excludedByConcept), "\n")
print(fullB$excludedByConcept |> count(excludedFromConcept, databaseSource, sort = TRUE) |> head(15), n = 15)
write_csv(excl, "work2026/exclusion_step_effect.csv")
cat("records removed by exclude lists:", nrow(full$excludedByConcept), " of ", nrow(full$includedData) + nrow(full$excludedByConcept), "\n")
print(excl, n = 30)
cat("\ndone\n")
