
processGRIN <- function(path){
# define and generate specific features  
  d1 <- read.csv(path)
  
  d2 <- d1 %>% 
    dplyr::select(
      taxon = taxon, 
      latitude =latitude,
      longitude = longitude,
      institutionCode = site_short_name,
      sourceUniqueID = accession_number,
      sampleCategory = status_code,
      biologicalStatus = improvement_status_code, 
      localityInformation = collector_verbatim_locality)%>%
    dplyr::mutate(
      genus = NA,
      species = NA, 
      databaseSource = "USDA_NPGS_GRINGlobal",
      type = NA,
      country = NA,
      iso3 = NA,
      collectionSource = NA,
      finalOriginStat = NA,
      yearRecorded = NA,
      county = NA,
      countyFIPS = NA,
      state = NA,
      stateFIPS = NA,
      coordinateUncertainty = NA
    )
  
  # filter based on active sampleCategory and biological status
  d3 <- d2 %>% 
    dplyr::filter(biologicalStatus == "WILD")%>%
    dplyr::mutate(type = case_when(
      sampleCategory == "INACTIVE" ~ "H",
      TRUE ~ "G"
    ))
  # assign genus species 
  d4 <- str_split_fixed(d3$taxon, " ", 2)
  d3$genus <- d4[,1]
  d3$species <- d4[,2]

  return(d3)
}
