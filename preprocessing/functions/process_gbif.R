#path <- "E:/mbg/wild grapes/data/gbif.csv"

processGBIF <- function(path){
  
  d1a <- vroom(file = path)
  
  # grab and rename all features from gbif
  d1 <- d1a %>% 
    dplyr::select(
      sourceUniqueID = "occurrenceID",
      genus = "genus",
      species,
      infraspecificEpithet, 
      stateProvince,
      locality,
      latitude = "decimalLatitude",
      longitude = "decimalLongitude",
      year = "year",
      institutionCode = "institutionCode", # need to determine the 
      finalOriginStat =  "establishmentMeans",
      sampleCategory = "basisOfRecord",
      countryCode,
      stateProvince, 
      taxonRank
    )%>%
    dplyr::mutate(databaseSource = "GBIF",
                  collectionSource = NA,
                  biologicalStatus = NA)%>%
    # remove fossil records 
    filter(sampleCategory != "FOSSIL_SPECIMEN")%>%
    # assign location value  and drop original 
    dplyr::mutate(localityInformation = paste0(stateProvince, " -- ",locality ))%>%
    dplyr::filter(is.na(latitude) | latitude > 10,
                  is.na(longitude) | longitude < -50)
  
  # filter data based on institution code --- need to regenerate this list 
  
  
  # mutate the taxon 
  d1 <- d1 %>% mutate( taxon =  case_when(
      taxonRank == "SPECIES" ~ d1$species,
      taxonRank == "GENUS" ~ d1$genus,
      taxonRank == "VARIETY" ~ paste0(d1$species, " var. ", d1$infraspecificEpithet),
      taxonRank == "SUBSPECIES" ~ paste0(d1$species, " subsp. ", d1$infraspecificEpithet),
      TRUE ~ d1$species,
    )
  )

  
  # define the type
  ## GBIF has a perserved specimen class as well, could be good   
  d1 <- d1 %>%
    mutate(type = case_when(
      sampleCategory != "LIVING_SPECIMEN" ~ "H",
      sampleCategory == "LIVING_SPECIMEN" ~ "G"
    ))
  
  # county
  d1$country <- countrycode(sourcevar = d1$countryCode, 
                            origin = "iso2c", 
                            destination = "country.name.en")

  # iso3 
  d1$iso3 <- countrycode(sourcevar = d1$countryCode,
                         origin = "iso2c",
                         destination = "iso3c")
  
  d1 <- d1 %>% dplyr::select(taxon,
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
    finalOriginStat
  )
  
  
}