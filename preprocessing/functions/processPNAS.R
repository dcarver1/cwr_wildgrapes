
# path <- "data/source_data/pnas2020.csv"

processPNAS <- function(path){
  d1 <- read_csv(path)
  
  d2 <- d1 %>%
    select(-c("...1","V1"))
  
  d3 <- d2 %>%     
    dplyr::select(
      taxon, 
      genus ,
      species,
      latitude ,
      longitude ,
      databaseSource,
      institutionCode,
      sampleCategory ,
      type = type,
      country = country,
      iso3,
      localityInformation,
      biologicalStatus,
      collectionSource,   
      finalOriginStat,    
      sourceUniqueID = uniqueID)%>%
    dplyr::mutate(    
      originalTaxon = taxon,
      yearRecorded = NA,
      state = NA,
      county = NA,
      countyFIPS = NA,
      stateFIPS = NA,
      coordinateUncertainty = NA,
      recordID = paste0(databaseSource, "_",sourceUniqueID),
      observerName =NA
    )
  return(d3)
}

