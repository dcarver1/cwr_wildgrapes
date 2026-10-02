###
# this data is a bit unique, it was provided by two different poeple with alot of expected overlap
# Need to merge them together priotizing source 2 as it contain more attribute data.  
### 
 # path <- "data/source_data/ucDavis.csv" # Luis
 # path2 <- "data/source_data/ucDavis2.csv" # Claire H

processDavis <- function(path,path2){
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
  
  # second source 
  f1 <- read_csv(path2)
  
  f2 <- f1 %>% 
    dplyr::select(
      originalTaxon = `Species for diss. ch 2`,
      latitude = `Lat (DD)`,
      longitude = `Long (DD)`,
      sourceUniqueID = Accession,
      localityInformation = `location notes`)%>%
    dplyr::mutate(     
      genus = "Vitis",
      species = str_remove(originalTaxon, pattern = "V. "),
      institutionCode = NA,
      databaseSource = "UC Davis Grape Breeding Collection",
      sampleCategory = "Plant",
      country = NA,
      yearRecorded = NA,
      iso3 = NA,
      collectionSource = "experiement site",
      biologicalStatus = NA,
      type = "G",
      state = NA,
      county = NA,
      finalOriginStat = NA,
      countyFIPS = NA,
      stateFIPS = NA,
      coordinateUncertainty = NA
    )%>%
    dplyr::mutate(
      species = str_remove(species, pattern = "V."),
      taxon = paste0(genus," ", species))
  # there appears to be multiple plantings of the same feature Run some test for duplicaiton 
  f2 <- f2[!duplicated(f2[,2:7]),]
  

  # test between two inputs -------------------------------------------------
  f3 <- bind_rows(f2, d2) %>%
    mutate(sourceUniqueID = str_replace_all(string = sourceUniqueID, pattern = "FEMALE", replacement = "female"))
  f3 <- f3[!duplicated(f3[,2:4]),]
  
  f4 <- f3 |>
    dplyr::mutate(
      recordID = paste0(databaseSource, "_",sourceUniqueID),
      observerName =NA
      
    )
  
  return(f4)
}
