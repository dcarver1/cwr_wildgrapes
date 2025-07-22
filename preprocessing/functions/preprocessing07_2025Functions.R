

# process Dataset from june Wen  ------------------------------------------
processJun <- function(){
  files <- list.files("data/jWen_data2025",
             full.names = TRUE) 
  d1 <- read_csv(files[1]) |> 
    dplyr::select(
      collectionSource = Collection,
      state = State,
      latitude = Latitude, 
      longitude = Longitude,
      localityInformation = `Terra-i`
    )|>
    dplyr::mutate(
      taxon = "Vitis martineziana",
      originalTaxon = "Vitis martineziana",
      genus = "Vitis",
      species = "martineziana",
      observerName = "Jun Wen",
      iso3 = "MEX",
      country = "Mexico",
      type = "H",
      databaseSource = "Personal Communication with Jun Wen"
    )
  d2 <- read_csv(files[2])  |> 
    dplyr::select(
      collectionSource = Collection,
      state = State,
      latitude = Latitude, 
      longitude = Longitude,
      localityInformation = `Terri-i`
    )|>
    dplyr::mutate(
      taxon = "Vitis rubriflora",
      originalTaxon = "Vitis rubriflora",
      genus = "Vitis",
      species = "rubriflora",
      observerName = "Jun Wen",
      iso3 = "MEX",
      country = "Mexico",
      type = "H",
      databaseSource = "Personal Communication with Jun Wen"
  )
  output <- data.frame(matrix(nrow = 0, ncol = 25))
  names <- c(
    "taxon","originalTaxon","genus","species","latitude", "longitude","databaseSource",
    "institutionCode","type","sourceUniqueID", "sampleCategory","country","iso3",
    "localityInformation","biologicalStatus","collectionSource","finalOriginStat",
    "yearRecorded","county","countyFIPS","state","stateFIPS","coordinateUncertainty",
    "observerName","recordID"  
  )
  output <- data.frame(setNames(lapply(names, function(x) character()), names), stringsAsFactors = FALSE)
  output$latitude <- as.numeric(output$latitude)
  output$longitude <- as.numeric(output$longitude)
  
  # bind data together 
  output <- output |>
    bind_rows(d1, d2)
  return(output)
}



# download GBIF data from google drive  -----------------------------------

pullGBIFFromDrive<-function(run = FALSE){
  if(run == TRUE){
    drive_auth()
    
    drive_download(
      file = "vitisGBIFDownload_20250721.csv",      # The name of the file on Google Drive
      path = "data/source_data/vitisGBIFDownload_20250721.csv", # The desired name for the downloaded file
      overwrite = TRUE                 # Set to TRUE to replace a local file if it exists
    )
  }
}

print("loaded pull pullGBIFFromDrive")


# read in and bind datasets -----------------------------------------------

readAndBind <- function(run = FALSE){
  # gbif
  gbif <- read_csv("data/processed_occurrence/gbif_072025.csv",col_types = cols(.default = "c"))
  
  ## grin
  grin <- read_csv("data/processed_occurrence/grin.csv",col_types = cols(.default = "c"))
  
  ## seinet
  seinet <- read_csv("data/processed_occurrence/seinet.csv",col_types = cols(.default = "c"))
  
  ## WEIWS
  wiews <- read_csv("data/processed_occurrence/wiews.csv",col_types = cols(.default = "c"))
  
  ## GENESYS
  genesys <- read_csv("data/processed_occurrence/genesys.csv",col_types = cols(.default = "c"))
  
  ## botanical garden Survey
  bgSurvey <- read_csv("data/processed_occurrence/bgSurvey.csv",col_types = cols(.default = "c"))
  
  ## UC Davis datasets
  ucdavis <- read_csv("data/processed_occurrence/UCDavis.csv",col_types = cols(.default = "c"))
  
  # ## natural heritage county level data
  # iunc <- read_csv("data/processed_occurrence/natureServe.csv",col_types = cols(.default = "c"))
  
  ## BONAP
  bonap <- read_csv("data/processed_occurrence/bonap.csv",col_types = cols(.default = "c"))
  
  ## Data from the PNAS paper
  pnas2020 <- read_csv("data/processed_occurrence/pnas2020.csv",col_types = cols(.default = "c"))
  
  # jun wen mexico 
  jun  <- read_csv("data/processed_occurrence/jun_072025.csv",col_types = cols(.default = "c"))
  
  
  d2 <-  bind_rows(gbif, grin, seinet, wiews, genesys, bgSurvey, pnas2020, ucdavis, jun) 
  d2 <- d2 |> 
    dplyr::mutate( index = dplyr::row_number())
  # reorder
  d2 <- d2 |>
    dplyr::select(index, everything())
  return(d2)
}


# synonym integration  ----------------------------------------------------
speciesCheck <- function(data, synonymList){
  # check for each species on Taxon  
  nSpecies <- 1:length(synonymList$taxon)
  # map over nSpecies... 
  mapSynonyms <- function(nSpecies, synonymList, data){
    i <- nSpecies
    taxon <- synonymList$taxon[i]
    synonyms <- synonymList$acceptedSynonym[i]
    # grab all original taxon name 
    df2 <- data[data$taxon == taxon, ]
    
    # grab all species with species name 
    if(!is.na(synonyms)){
      syn1 <- synonyms |>
        stringr::str_split(pattern = ", ")|>
        unlist()
      for(j in syn1){
        print(j)
        df3 <- data[data$taxon == j, ]
        df3$taxon <- taxon
        df2 <- bind_rows(df2, df3)
      }
    }
    # set the taxon name to be consistent 
    df2$taxon <- taxon
    return(df2)
  }
  
  # gather data included
  ## there can be repeated records for taxon that are being modeled directly and also included as a synonym "Vitis aestivalis var. aestivalis"
  includedData <- nSpecies |>
    purrr::map(mapSynonyms, synonymList =synonymList, data = data)|>
    bind_rows()
  
  
  # define excluded data 
  excludedData <- data[!data$index %in% includedData$index, ]
  
  return(list(
    excludedData = excludedData,
    includedData = includedData
  ))
}


