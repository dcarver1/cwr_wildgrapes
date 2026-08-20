# preprocessing/functions/process_gbif_082026.R
# Custom GBIF processor to preserve original/recorded taxonomy and prevent taxonomic lumping

processGBIF <- function(path){
  
  d1a <- read_tsv(file = path)
  
  # 1. Grab and rename raw features from GBIF
  d1 <- d1a |> 
    dplyr::select(
      originalTaxon = "scientificName",
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
    # Remove fossil records 
    dplyr::filter(sampleCategory != "FOSSIL_SPECIMEN")|>
    # Standardize locality information
    dplyr::mutate(localityInformation = paste0(state, " -- ", locality ))
  
  # 2. Extract unlumped, clean botanical taxonomy from raw scientificName
  # Standardize hybrid symbols (×) and normalize multiple spaces
  clean_raw <- stringr::str_replace_all(d1$originalTaxon, "×\\s*|\\s+[xX]\\s+", " x ")
  clean_raw <- stringr::str_replace_all(clean_raw, "\\s+", " ")
  
  # Extract clean Genus + optional hybrid marker (x) + specificEpithet + optional variety/subspecies
  extracted_taxon <- stringr::str_match(
    clean_raw, 
    "^((?:Vitis|Muscadinia)\\s+(?:x\\s+)?[a-z]+(?:\\s+(?:var\\.|subsp\\.|f\\.)\\s+[a-z]+)?)"
  )[, 2]
  
  # 3. Assign the custom parsed taxon to bypass the GBIF backbone lumping
  # Fall back to standard GBIF backbone assignment ONLY if the raw name can't be parsed (e.g. "Vitis L.")
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
    # 4. Re-calculate clean genus and species epithets to match our unlumped taxonomy
    dplyr::mutate(
      genus = stringr::str_split_fixed(taxon, " ", 2)[, 1],
      species_temp = stringr::str_split_fixed(taxon, " ", 2)[, 2]
    ) |>
    dplyr::mutate(
      species = stringr::str_remove(species_temp, "^[xX]\\s+") |>
        stringr::str_remove("\\s+(var\\.|subsp\\.|f\\.).*")
    ) |>
    dplyr::select(-species_temp)
  
  # 5. Define the specimen type (H = Herbarium, G = Germplasm/Living)
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
