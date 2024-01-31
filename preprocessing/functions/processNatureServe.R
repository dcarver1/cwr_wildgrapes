
# path <- "data/source_data/iuncData.gdb"


processIUNC <- function(path){
  # look at layer names 
  layers <- st_layers(path)
  # pull layer of interest
  t1 <- st_read(path,layer = layers$name[2])
  # filter to vitis features 
  t2 <- t1 %>% 
    filter(grepl("Vitis ", x = gname))
 
  d1 <- t2 %>%
    select(
      originalTaxon = gname,
      taxon = gname, 
      sourceUniqueID = nsx_link,
      yearRecorded = max_obs_year,
      county = county_name ,
      state = state_cd,
      countyFIPS = fips_cd,
    )%>%
    mutate(
      species = str_remove(string = taxon, pattern = "Vitis "),
      latitude = NA,  
      longitude = NA,
      country = "United States of America",
      sampleCategory = "Herberium",
      collectionSource = "Nature Serve",
      genus = "Vitis",
      databaseSource = "Nature Serve", 
      type = "H",
      institutionCode = NA,
      iso3 = NA,
      finalOriginStat = NA,
      localityInformation = NA,
      biologicalStatus= NA,
      stateFIPS = NA ,
      coordinateUncertainty = NA,
      recordID = paste0(databaseSource, "_",sourceUniqueID),
      observerName =NA
    )
  return(d1)   
}
