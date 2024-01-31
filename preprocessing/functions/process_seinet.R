# https://github.com/dcarver1/CWR-of-the-USA-Gap-Analysis/blob/master/dataPrep/dataBaseTransform/midwestHerbTransform.R
# use this is a reference for the column headers 

# path <- "data/source_data/seinet.csv"

processSEINET <- function(path){
  d1 <- read_csv(path)
  
  d2 <- d1 |> 
    mutate(
      originalTaxon = paste0(scientificName, " ", scientificNameAuthorship)
    ) |>
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
      coordinateUncertainty = coordinateUncertaintyInMeters,
      observerName = identifiedBy
      )|>
    dplyr::mutate(
      species = NA, 
      databaseSource = "seinet",
      type = "H",
      iso3 = NA,
      finalOriginStat = NA,
      countyFIPS = NA,
      biologicalStatus = NA, 
      stateFIPS = NA,
      recordID = paste0(databaseSource,"_",sourceUniqueID)
    )
  
  # Assign types based on sample category
  d3 <- d2 |>
    filter(!sampleCategory %in% c("Observation", "HumanObservation"))
    
  
  # # reassign datatype on lat lon column 
  # d2$latitude <- d2$latitude
  # 
  # assign genus species 
  d4 <- stringr::str_split_fixed(d3$taxon, " ", 2)
  d3$species <- d4[,2]
  
  return(d3)
}

