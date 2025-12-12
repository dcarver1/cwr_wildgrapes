# process the mexican taxon data  -----------------------------------------

processMex <- function() {
  file <- read_csv("data/source_data/mexicoRecords_ck.csv")
  #
  d1 <- file |>
    dplyr::select(
      taxon = "Taxon",
      originalTaxon = "Possible Species ID",
      # genus = "Vitis",
      "latitude" = Latitude,
      "longitude" = Longitude,
      sourceUniqueID = Genotype,
      type = Type,
      state = State,
      localityInformation = Locality,
      collectionSource = "Holding Institute",
      observerName = Source,
      "Collecting number"
    ) |>
    tidyr::separate(
      col = taxon,
      into = c("genus", "species"),
      sep = " ",
      remove = FALSE
    ) |>
    dplyr::mutate(
      databaseSource = "Huerta-Acosta publication",
      institutionCode = "HAP"
    )
  # ID correction
  d1[d1$taxon == "Vitis novogranatensis", "sourceUniqueID"] <- d1[
    d1$taxon == "Vitis novogranatensis",
    "Collecting number"
  ]
  d1$latitude <- as.numeric(d1$latitude)
  d1$longitude <- as.numeric(d1$longitude)

  # format data
  output <- data.frame(matrix(nrow = 0, ncol = 25))
  names <- c(
    "taxon",
    "originalTaxon",
    "genus",
    "species",
    "latitude",
    "longitude",
    "databaseSource",
    "institutionCode",
    "type",
    "sourceUniqueID",
    "sampleCategory",
    "country",
    "iso3",
    "localityInformation",
    "biologicalStatus",
    "collectionSource",
    "finalOriginStat",
    "yearRecorded",
    "county",
    "countyFIPS",
    "state",
    "stateFIPS",
    "coordinateUncertainty",
    "observerName",
    "recordID"
  )
  output <- data.frame(
    setNames(lapply(names, function(x) character()), names),
    stringsAsFactors = FALSE
  )
  output$latitude <- as.numeric(output$latitude)
  output$longitude <- as.numeric(output$longitude)

  # bind data together
  output <- output |>
    bind_rows(d1)

  return(output)
}


# process Dataset from june Wen  ------------------------------------------
processJun <- function() {
  files <- list.files("data/jWen_data2025", full.names = TRUE)
  d1 <- read_csv(files[1]) |>
    dplyr::select(
      collectionSource = Collection,
      state = State,
      latitude = Latitude,
      longitude = Longitude,
      localityInformation = `Terra-i`
    ) |>
    dplyr::mutate(
      taxon = "Vitis martineziana",
      originalTaxon = "Vitis martineziana",
      genus = "Vitis",
      species = "martineziana",
      observerName = "Jun Wen",
      iso3 = "MEX",
      country = "Mexico",
      type = "H",
      databaseSource = "Personal Communication with Jun Wen",
      sourceUniqueID = paste0("sourceID_", 1:12),
    )
  d2 <- read_csv(files[2]) |>
    dplyr::select(
      collectionSource = Collection,
      state = State,
      latitude = Latitude,
      longitude = Longitude,
      localityInformation = `Terri-i`
    ) |>
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
    "taxon",
    "originalTaxon",
    "genus",
    "species",
    "latitude",
    "longitude",
    "databaseSource",
    "institutionCode",
    "type",
    "sourceUniqueID",
    "sampleCategory",
    "country",
    "iso3",
    "localityInformation",
    "biologicalStatus",
    "collectionSource",
    "finalOriginStat",
    "yearRecorded",
    "county",
    "countyFIPS",
    "state",
    "stateFIPS",
    "coordinateUncertainty",
    "observerName",
    "recordID"
  )
  output <- data.frame(
    setNames(lapply(names, function(x) character()), names),
    stringsAsFactors = FALSE
  )
  output$latitude <- as.numeric(output$latitude)
  output$longitude <- as.numeric(output$longitude)

  # bind data together
  output <- output |>
    bind_rows(d1, d2)
  return(output)
}


# download GBIF data from google drive  -----------------------------------

pullGBIFFromDrive <- function(run = FALSE) {
  if (run == TRUE) {
    drive_auth()

    drive_download(
      file = "vitisGBIFDownload_20250721.csv", # The name of the file on Google Drive
      path = "data/source_data/vitisGBIFDownload_20250721.csv", # The desired name for the downloaded file
      overwrite = TRUE # Set to TRUE to replace a local file if it exists
    )
  }
}

print("loaded pull pullGBIFFromDrive")


# process genesys  --------------------------------------------------------

processGenesysUpdate <- function(path) {
  d1 <- read_csv(path)

  d2 <- d1 %>%
    dplyr::mutate(
      type = case_when(
        HISTORIC == TRUE ~ "H",
        TRUE ~ "G"
      ),
      species = paste0(SPECIES, " ", SUBTAXA) %>%
        stringr::str_replace(pattern = " NA", replacement = "")
    ) %>%
    # SAMPSTATRemove non wild- exclude 300 and above (but include 999)
    dplyr::filter(
      SAMPSTAT %in% c(999, 100, 110, 120, 130, 200, 999, NA)
    ) %>%
    # # COLLSRC- exclude 30, 40, 50
    # dplyr::filter(
    #   !collSrc %in% c(30, 40, 50)
    # )%>%
    # exclude USDA collection codes
    dplyr::filter(
      !INSTCODE %in%
        c(
          "USA003",
          "USA004",
          "USA005",
          "USA016",
          "USA020",
          "USA022",
          "USA026",
          "USA028",
          "USA029",
          "USA042",
          "USA047",
          "USA049",
          "USA074",
          "USA108",
          "USA133",
          "USA148",
          "USA151",
          "USA167",
          "USA176",
          "USA390",
          "USA955",
          "USA956",
          "USA970",
          "USA971",
          "USA995"
        )
    )

  d3 <- d2 %>%
    dplyr::mutate(
      taxon = paste0(GENUS, " ", SPECIES)
    ) %>%
    dplyr::select(
      taxon,
      genus = GENUS,
      latitude = DECLATITUDE,
      longitude = DECLONGITUDE,
      institutionCode = INSTCODE,
      sourceUniqueID = UUID,
      sampleCategory = SAMPSTAT,
      country = ORIGCTY,
      type,
      species
    ) %>%
    dplyr::mutate(
      originalTaxon = taxon,
      yearRecorded = NA,
      state = NA,
      county = NA,
      localityInformation = NA,
      collectionSource = NA,
      databaseSource = "genesys",
      iso3 = NA,
      finalOriginStat = NA,
      countyFIPS = NA,
      biologicalStatus = NA,
      stateFIPS = NA,
      observerName = NA,
      coordinateUncertainty = NA,
      recordID = paste0(databaseSource, "_", sourceUniqueID)
    )

  return(d3)
}


# read in and bind datasets -----------------------------------------------

readAndBind <- function(run = FALSE) {
  # gbif
  gbif <- read_csv(
    "data/processed_occurrence/gbif_072025.csv",
    col_types = cols(.default = "c")
  )

  ## grin
  grin <- read_csv(
    "data/processed_occurrence/grin.csv",
    col_types = cols(.default = "c")
  )

  ## seinet
  ### 20250828 -- droping scient as they report to gbif
  # seinet <- read_csv("data/processed_occurrence/seinet.csv",col_types = cols(.default = "c"))s

  ## WEIWS
  wiews <- read_csv(
    "data/processed_occurrence/wiews.csv",
    col_types = cols(.default = "c")
  )

  ## GENESYS
  genesys <- read_csv(
    "data/processed_occurrence/genesys_072025.csv",
    col_types = cols(.default = "c")
  )

  ## botanical garden Survey
  bgSurvey <- read_csv(
    "data/processed_occurrence/botanicalGardenSurvey_072025.csv",
    col_types = cols(.default = "c")
  )

  ## UC Davis datasets
  ucdavis <- read_csv(
    "data/processed_occurrence/UCDavis.csv",
    col_types = cols(.default = "c")
  )

  # ## natural heritage county level data
  # iunc <- read_csv("data/processed_occurrence/natureServe.csv",col_types = cols(.default = "c"))

  ## BONAP
  bonap <- read_csv(
    "data/processed_occurrence/bonap.csv",
    col_types = cols(.default = "c")
  )

  ## Data from the PNAS paper
  pnas2020 <- read_csv(
    "data/processed_occurrence/pnas2020.csv",
    col_types = cols(.default = "c")
  )

  # jun wen mexico
  jun <- read_csv(
    "data/processed_occurrence/jun_072025.csv",
    col_types = cols(.default = "c")
  )

  # more mexico records
  mex <- read_csv(
    "data/processed_occurrence/mexicoRecords_082025.csv",
    col_types = cols(.default = "c")
  )

  d2 <- bind_rows(
    gbif,
    grin,
    wiews,
    genesys,
    bgSurvey,
    pnas2020,
    ucdavis,
    jun,
    mex
  ) #seinet,
  d2 <- d2 |>
    dplyr::mutate(index = dplyr::row_number())
  # reorder
  d2 <- d2 |>
    dplyr::select(index, everything())
  # exclude some of the older observational data

  return(d2)
}


# synonym integration  ----------------------------------------------------
speciesCheck <- function(data, synonymList) {
  # check for each species on Taxon
  nSpecies <- 1:length(synonymList$taxon)
  # map over nSpecies...
  mapSynonyms <- function(nSpecies, synonymList, data) {
    i <- nSpecies
    taxon <- synonymList$taxon[i]
    synonyms <- synonymList$acceptedSynonym[i]
    # grab all original taxon name
    df2 <- data[data$taxon == taxon, ]

    # grab all species with species name
    if (!is.na(synonyms)) {
      syn1 <- synonyms |>
        stringr::str_split(pattern = ", ") |>
        unlist()
      for (j in syn1) {
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
    purrr::map(mapSynonyms, synonymList = synonymList, data = data) |>
    bind_rows()

  # define excluded data
  excludedData <- data[!data$index %in% includedData$index, ]

  return(list(
    excludedData = excludedData,
    includedData = includedData
  ))
}
