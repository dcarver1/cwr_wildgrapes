

processGBIF <- function(path){
  d1a <- vroom(file = path)
  
  d1 <- d1a %>% 
    dplyr::select(
      sourceUniqueID = "occurrenceID",
      genus = "genus",
      species,
      infraspecificEpithet, 
      localityInformation = "locality",
      latitude = "decimalLatitude",
      longitude = "decimalLongitude",
      year = "year",
      collectionSource = "institutionCode",
      finalOriginStat =  "establishmentMeans",
      basisOfRecord,
      countryCode
    )%>%
    dplyr::mutate(databaseSource = "GBIF")
  
  # mutate the taxon 
  d1$taxon <- paste0(d1$species, " ", d1$infraspecificEpithet) %>%
    stringr::str_remove_all(pattern = "NA")
  
  # define the type
  
  # biologicalStatus 
  
  # county
  d1$country <- countrycode(sourcevar = d1$countryCode, 
                            origin = "iso2c", 
                            destination = "country.name.en")

  # iso3 
  df$iso3 <- countrycode(sourcevar = d1$countryCode,
                         origin = "iso2c",
                         destination = "iso3c")
  
}