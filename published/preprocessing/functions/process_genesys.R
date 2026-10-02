# path <- "data/source_data/genesys.csv"

processGenesys <- function(path){
  d1 <- read_csv(path)
  
  d2 <- d1 %>% 
    dplyr::mutate(
      type = case_when(
        historic == TRUE ~ "H",
        TRUE ~ "G"
      ),
      species = paste0(species," ",subtaxa) %>%
        stringr::str_replace(pattern = " NA", replacement = "")
    )%>%
    # SAMPSTATRemove non wild- exclude 300 and above (but include 999)
    dplyr::filter(
      sampStat %in% c(999,100,110,120, 130, 200, 999, NA)
    )%>%
    # COLLSRC- exclude 30, 40, 50
    dplyr::filter(
      !collSrc %in% c(30, 40, 50)
    )%>%
    # exclude USDA collection codes 
    dplyr::filter(
      !instCode  %in%  c("USA003" ,"USA004", "USA005" ,"USA016" ,"USA020",
                                "USA022", "USA026", "USA028", "USA029", "USA042" ,"USA047", "USA049",
                                "USA074", "USA108", "USA133", "USA148", "USA151", "USA167", "USA176",
                                "USA390", "USA955", "USA956", "USA970", "USA971", "USA995")
    )
    
  
  d3 <- d2 %>% 
    dplyr::mutate(
      taxon = paste0(genus," ",species)
      )%>%
    dplyr::select(
      taxon,
      originalTaxon = fullTaxa,
      genus = genus,
      latitude =latitude,
      longitude = longitude,
      institutionCode = instCode,
      sourceUniqueID = uuid,
      sampleCategory = sampStat,
      localityInformation = collSite,
      collectionSource = collSrc,
      country = origCty,
      coordinateUncertainty = uncertainty,
      type,
      species)%>%
    dplyr::mutate(
      yearRecorded = NA,
      state = NA,
      county = NA,
      databaseSource = "genesys",
      iso3 = NA,
      finalOriginStat = NA,
      countyFIPS = NA,
      biologicalStatus = NA, 
      stateFIPS = NA,
      observerName = NA,
      recordID = paste0(databaseSource,"_",sourceUniqueID)
    )
  
  
  return(d3)
}

