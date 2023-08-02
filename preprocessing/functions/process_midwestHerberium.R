# https://github.com/dcarver1/CWR-of-the-USA-Gap-Analysis/blob/master/dataPrep/dataBaseTransform/midwestHerbTransform.R
# use this is a reference for the column headers 

# path <- "data/source_data/midwestherberium.csv"

processMidwestHerberium <- function(path){
  d1 <- read_csv(path)
  
  d2 <- d1 %>% 
    mutate(
      originalTaxon = paste0(scientificName, " ", scientificNameAuthorship)
    )%>%
    dplyr::select(
      taxon = scientificName,
      originalTaxon,
      genus = genus,
      latitude =decimalLatitude,
      longitude = decimalLongitude,
      institutionCode = institutionCode,
      sourceUniqueID = id,
      sampleCategory = basisOfRecord,
      country = country,
      yearRecorded = year,
      state = stateProvince,
      county = county,
      localityInformation = locality,
      collectionSource = references,
      coordinateUncertainty = coordinateUncertaintyInMeters)%>%
    dplyr::mutate(
      species = NA, 
      databaseSource = "midwestHerbarium",
      type = "H",
      iso3 = NA,
      finalOriginStat = NA,
      countyFIPS = NA,
      biologicalStatus = NA, 
      stateFIPS = NA,
      coordinateUncertainty = NA
    )
  
  # assign genus species 
  d4 <- str_split_fixed(d2$taxon, " ", 2)
  d2$species <- d4[,2]
  
  return(d2)
}

