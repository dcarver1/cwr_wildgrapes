
# path <- "data/source_data/wiews.csv"

processWIEWS <- function(path){
  d1 <- read_csv(path)
  
  d2 <- d1 %>% 
    dplyr::select(
      taxon = Taxon, 
      originalTaxon = Taxon,
      genus = Genus,
      species = Species,
      latitude =`Latitude of collecting site (decimal degrees format)`,
      longitude = `Longitude of collecting site (decimal degrees format)`,
      institutionCode = `Holding institute code`,
      sourceUniqueID = `Accession number`,
      sampleCategory = `Type of germplasm storage`,
      country = `Country of origin`,
      yearRecorded = Year,
      iso3 = `Country of origin (ISO3)`,
      localityInformation = `Collecting/acquisition source`,
      collectionSource = `Data owner details`,
      biologicalStatus = `Biological status`)%>%
    dplyr::mutate(      
      databaseSource = "wiews",
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
