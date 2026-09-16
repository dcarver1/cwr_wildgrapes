# preprocessing/functions/process_gbif_082026.R
# Custom GBIF processor to preserve the recorded taxonomy and prevent lumping by
# the GBIF taxonomic backbone.
#
# Background (see work2026/GBIF_taxonomy_notes.md):
#   The original processGBIF() built `taxon` from the GBIF backbone columns
#   (`species`, `infraspecificEpithet`, `taxonRank`). `species` is the backbone's
#   ACCEPTED species, so any name GBIF treats as a synonym is silently moved to a
#   different species before our own synonym list is ever applied. Example:
#   "Vitis rufotomentosa Small" is a synonym of V. aestivalis in the backbone, so
#   every record was assigned to V. aestivalis and V. rufotomentosa received none.
#
# This version parses the name string instead and only falls back to the backbone
# when the string cannot be parsed (e.g. "Vitis L.").
#
# nameSource controls which string is parsed:
#   "scientificName"          GBIF's interpreted name usage (backbone-matched name,
#                             with authorship). This can differ from what the
#                             publisher wrote (e.g. rank changes, spelling fixes).
#   "verbatimScientificName"  the name exactly as supplied by the publisher.
#   "combined"                parse scientificName; if that fails (GBIF could not
#                             match the name and fell back to "Vitis L."), parse
#                             verbatimScientificName; only then use the backbone.

# Parse "Genus [x] epithet [var.|subsp.|f. epithet]" from a name string.
# Hyphenated epithets (novae-angliae, izu-insularis) are supported.
# Returns NA when the string does not start with a Vitis/Muscadinia binomial, or
# when it is a hybrid formula ("Vitis riparia x Vitis rupestris",
# "Vitis labrusca x vinifera"): such records belong to neither parent.
# Named hybrids ("Vitis x doaniana") are kept.
# Homonyms (same binomial, different author, different plant: "Vitis labrusca
# Thunb." is V. coignetiae, "Vitis vulpina Bartram" is V. labrusca) are handled
# inside processGBIF(): see the homonym block there. Decision 2026-09-16: keep
# the original (backbone) assignment for those records rather than maintain
# exclude-cell entries on the sheet.
parseVitisName <- function(x) {
  clean <- stringr::str_replace_all(x, "×\\s*|\\s+[xX]\\s+", " x ")
  clean <- stringr::str_replace_all(clean, "\\s+", " ")
  clean <- stringr::str_trim(clean)
  m <- stringr::str_match(
    clean,
    "^((?:Vitis|Muscadinia)\\s+(?:x\\s+)?[a-z]+(?:-[a-z]+)?(?:\\s+(?:var\\.|subsp\\.|f\\.)\\s+[a-z]+(?:-[a-z]+)?)?)(.*)$"
  )
  parsed <- m[, 2]
  remainder <- m[, 3]
  hybridFormula <- !is.na(remainder) & stringr::str_detect(remainder, "^\\s+x\\s+")
  parsed[hybridFormula] <- NA_character_
  parsed
}

processGBIF <- function(path, nameSource = c("scientificName", "verbatimScientificName", "combined"),
                        homonymLog = NULL) {
  nameSource <- match.arg(nameSource)
  
  d1a <- read_tsv(file = path)
  
  # 1. Grab and rename raw features from GBIF
  d1 <- d1a |> 
    dplyr::select(
      originalTaxon = "scientificName",
      verbatimScientificName,
      sourceUniqueID = "occurrenceID",
      genus = "genus",
      species,
      infraspecificEpithet, 
      locality,
      latitude = "decimalLatitude",
      longitude = "decimalLongitude",
      yearRecorded = "year",
      institutionCode = "institutionCode", 
      finalOriginStat = "establishmentMeans",
      sampleCategory = "basisOfRecord",
      countryCode,
      state = "stateProvince", 
      taxonRank,
      coordinateUncertainty = "coordinateUncertaintyInMeters",
      observerName = "identifiedBy"
    ) |>
    dplyr::mutate(
      databaseSource = "GBIF",
      collectionSource = NA,
      biologicalStatus = NA
    )|>
    # Remove fossil records. `%in%` keeps rows whose basisOfRecord is NA;
    # `!= "FOSSIL_SPECIMEN"` silently dropped them.
    dplyr::filter(!(sampleCategory %in% "FOSSIL_SPECIMEN"))|>
    # Standardize locality information
    dplyr::mutate(localityInformation = paste0(state, " -- ", locality ))
  
  # 2. Extract the unlumped taxon from the chosen name string
  extracted_taxon <- switch(nameSource,
    scientificName = parseVitisName(d1$originalTaxon),
    verbatimScientificName = parseVitisName(d1$verbatimScientificName),
    combined = dplyr::coalesce(parseVitisName(d1$originalTaxon), parseVitisName(d1$verbatimScientificName))
  )
  
  # 2b. Homonyms: the parser strips authorship, so "Vitis vulpina Bartram"
  #     (a V. labrusca synonym) would land in V. vulpina next to "Vitis vulpina
  #     L.". Rule: within one parsed binomial, if the authored name strings map
  #     to more than one backbone accepted species, the records whose authored
  #     name maps to a species OTHER than the majority one keep the backbone
  #     assignment (as in the published workflow). A binomial whose every
  #     authored form maps to the same accepted species (e.g. all
  #     "Vitis berlandieri Planch." -> V. cinerea) is NOT a homonym and is
  #     still recovered. Names treated this way are written to `homonymLog`.
  canon <- parseVitisName(d1$originalTaxon)
  homonyms <- dplyr::tibble(canon = canon, sci = d1$originalTaxon, acc = d1$species) |>
    dplyr::filter(!is.na(canon), !is.na(acc)) |>
    dplyr::count(canon, sci, acc, name = "records") |>
    dplyr::group_by(canon) |>
    dplyr::mutate(majorityAccepted = acc[which.max(records)]) |>
    dplyr::ungroup() |>
    dplyr::filter(acc != majorityAccepted) |>
    dplyr::rename(scientificName = sci, backboneAccepted = acc)
  isHomonym <- d1$originalTaxon %in% homonyms$scientificName
  extracted_taxon[isHomonym] <- NA_character_
  message(sum(isHomonym), " records with ", nrow(homonyms), " homonym name strings keep the backbone assignment")
  if (!is.null(homonymLog)) readr::write_csv(homonyms, homonymLog)
  
  # 3. Assign the parsed taxon; fall back to the GBIF backbone ONLY when the
  #    string cannot be parsed (e.g. "Vitis L.", non-Vitis verbatim names)
  #    or the name is a homonym (2b)
  d2 <- d1 |> 
    dplyr::mutate(
      taxon = dplyr::case_when(
        !is.na(extracted_taxon) ~ extracted_taxon,
        taxonRank == "SPECIES" ~ d1$species,
        taxonRank == "GENUS" ~ d1$genus,
        taxonRank == "VARIETY" ~ paste0(d1$species, " var. ", d1$infraspecificEpithet),
        taxonRank == "SUBSPECIES" ~ paste0(d1$species, " subsp. ", d1$infraspecificEpithet),
        TRUE ~ d1$species
      )
    ) |>
    # 4. Re-calculate genus and species epithets to match the parsed taxon
    dplyr::mutate(
      genus = stringr::str_split_fixed(taxon, " ", 2)[, 1],
      species_temp = stringr::str_split_fixed(taxon, " ", 2)[, 2]
    ) |>
    dplyr::mutate(
      species = stringr::str_remove(species_temp, "^[xX]\\s+") |>
        stringr::str_remove("\\s+(var\\.|subsp\\.|f\\.).*")
    ) |>
    dplyr::select(-species_temp, -verbatimScientificName)
  
  # 5. Define the specimen type (H = Herbarium, G = Germplasm/Living)
  # DECISION POINT (unchanged from the published run): every GBIF
  # LIVING_SPECIMEN is typed "G" and counts as a germplasm accession in SRSex.
  # Most GBIF living specimens are botanic-garden collections, not genebank
  # accessions, so this inflates ex-situ scores for garden-popular taxa
  # (review/WORKFLOW_EVALUATION.md section 2.5). Kept as-is so the co-author
  # diff isolates the taxonomy fix; revisit with the co-author.
  d3 <- d2 |>
    dplyr::mutate(type = case_when(
      sampleCategory != "LIVING_SPECIMEN" ~ "H",
      sampleCategory == "LIVING_SPECIMEN" ~ "G"
    ))
  
  # Convert country names and ISO3
  d3$country <- countrycode::countrycode(
    sourcevar = d3$countryCode, 
    origin = "iso2c", 
    destination = "country.name.en"
  )
  
  d3$iso3 <- countrycode::countrycode(
    sourcevar = d3$countryCode,
    origin = "iso2c",
    destination = "iso3c"
  )
  
  # 6. Format and select output columns to maintain full pipeline compatibility
  d4 <- d3 |> 
    dplyr::mutate(
      county = NA,
      countyFIPS = NA,
      stateFIPS = NA,
      recordID = paste0(databaseSource, "_", sourceUniqueID)
    ) |> 
    dplyr::select(
      taxon,
      originalTaxon,
      genus,
      species,
      latitude,
      longitude,
      databaseSource,
      institutionCode,
      type,
      sourceUniqueID,
      sampleCategory,
      country,
      iso3,
      localityInformation,
      biologicalStatus, 
      collectionSource,
      finalOriginStat,
      yearRecorded,
      county,
      countyFIPS,
      state,
      stateFIPS,
      coordinateUncertainty,
      observerName,
      recordID
    )
  
  return(d4)
}
