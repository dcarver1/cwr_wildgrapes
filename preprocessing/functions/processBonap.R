
# path <- "data/source_data/bonap.csv"

processBonap <- function(path){
  ## drop the NA value that is being read in. 
  d1 <- read_csv(path)%>% 
    filter(!is.na(`Scientific Name`))
  
  ## we will want the false report and questionable columns for later, I'm 
  ## excluding for now because we want to keep in the standard datastructure. 
  ### currently the questionalbe and incorrect column are not being transfered 
  ### well from the google sheet. Address when needed. 
  
  d2 <- d1 %>%
    dplyr::select(
      originalTaxon = `Scientific Name`,
      countyFIPS = FIPS,
      county = County,
      state = Stateabb)%>%
    dplyr::mutate(    
      sourceUniqueID = paste0(1:nrow(.), "_bonap"),
      taxon = str_to_sentence(originalTaxon),
      genus = "Vitis",
      species = str_remove(taxon, pattern = "Vitis "),
      institutionCode = NA,
      databaseSource = "BONAP",
      sampleCategory = "Observation",
      country = "USA",
      yearRecorded = NA,
      iso3 = NA,
      latitude = NA,
      longitude = NA,
      localityInformation = NA,
      collectionSource = NA,
      biologicalStatus = NA,
      type = "H",
      finalOriginStat = NA,
      stateFIPS = substr(countyFIPS, start = 1, stop = 2),
      coordinateUncertainty = NA
    )
  
  return(d2)
}
