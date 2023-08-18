
# path <- "data/source_data/ucDavis.csv"

processDavis <- function(path){
  d1 <- read_csv(path)
  
  d2 <- d1 %>% 
    dplyr::select(
      originalTaxon = species,
      latitude = lat,
      longitude = lon,
      sourceUniqueID = id)%>%
    dplyr::mutate(    
      taxon = str_to_sentence(originalTaxon), 
      genus = "Vitis",
      species = str_remove(taxon, pattern = "Vitis "),
      institutionCode = NA,
      databaseSource = "UC Davis Grape Breeding Collection",
      sampleCategory = "Plant",
      country = NA,
      yearRecorded = NA,
      iso3 = NA,
      localityInformation = "Living specimen at UC Davis",
      collectionSource = "experiement site",
      biologicalStatus = NA,
      type = "G",
      state = NA,
      county = NA,
      finalOriginStat = NA,
      countyFIPS = NA,
      stateFIPS = NA,
      coordinateUncertainty = NA
    )
  
  # adding condition to apply 
  
  
  return(d2)
}
