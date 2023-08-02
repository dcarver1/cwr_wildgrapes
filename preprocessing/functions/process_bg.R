
# path <- "data/source_data/bg_survey.csv"
### something with the lat long column is preventing it from being read in? 
processBG <- function(path){
  
  d1 <- read_csv(path)%>%
    select(
      originalTaxon = `Taxon Full Name`,
      genus = `Generic Epithet`,
      species = `Specific Epithet`,
      latitude = `Latitude of Wild-Collection Site`,  
      longitude = `Longitude of Wild-Collection Site`,
      sourceUniqueID = `Accession Number`,
      country = `Country of Wild-Collection Site`,
      sampleCategory = `Germplasm Type Received`,
      localityInformation = `Locality of Wild-Collection Site`,
      biologicalStatus= `Provenance Type`,
      collectionSource = `Original Source`,
      yearRecorded = `Collection Year`,
      county = `County of Wild-Collection Site`,
      state = `State/Province of Wild-Collection Site`
    )%>%
    mutate(
      taxon = paste0(genus," ",species)%>%
        stringr::str_replace(pattern = " NA", replacement = ""), 
      databaseSource = "bg_survey", 
      type = "G",
      institutionCode = NA,
      iso3 = NA,
      finalOriginStat = NA,
      countyFIPS = NA,
      stateFIPS = NA ,
      coordinateUncertainty = NA
    )
  return(d1)     
  
}
