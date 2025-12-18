pacman::p_load(
  dplyr,
  readr,
  sf,
  terra,
  rgbif,
  googledrive,
  countrycode,
  stringr
)


# new vitis data
vitis2 <- read_csv("data/New World Vitis.csv") |>
  dplyr::select(
    "taxon" = "Scientific Name",
    "acceptedSynonym" = "Names to include in this concept (Homotypic synonyms)",
    "Names to exclude from this concept" = "Names to exclude from this concept",
    "modelSpecies" = "Include in gap analysis?"
  )


# General changes to taxonomy
# include data for missing species
# 1. New pull of data from GBIF
# 2. Update the filtering and synonym methods to account for changes in taxonomic relationships
# 3. rerender more at 1km resolutions for all species

# replicated what was generate in the compileInputDatasets.R script here just to
# have a clean start

# call in all new functions
source("preprocessing/functions/preprocessing07_2025Functions.R")
# call in specific require functions that were not altered
source("preprocessing/functions/process_gbif.R")
source("preprocessing/functions/helperFunctions.R")
source("preprocessing/functions/process_bg.R")

# global variables  -------------------------------------------------------
standardColumnNames <- c(
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

# Download gbif data from google drive ------------------------------------
pullGBIFFromDrive(run = FALSE)


# process the GBIF data  ---------------------------------------
# gbif
gbif <- processGBIF(path = "data/source_data/vitisGBIFDownload_20250721.csv") |>
  orderNames(names = standardColumnNames) |>
  removeDuplicatesID()


# write out data
write_csv(x = gbif, file = "data/processed_occurrence/gbif_072025.csv")


# process data from Jun  --------------------------------------------------
jun <- processJun() |>
  orderNames(names = standardColumnNames) |>
  removeDuplicatesID()

# add single record
single <- data.frame(matrix(nrow = 1, ncol = length(standardColumnNames)))
names(single) <- standardColumnNames
single$taxon <- "Vitis bloodworthiana"
single$observerName <- "Facultad de Estudios Superiores Iztacala, UNAM, (FESI-UNAM), Mexico"
single$originalTaxon <- "Vitis bloodworthiana"
single$genus <- "Vitis"
single$species <- "bloodworthiana"
single$type <- "G"
single$latitude <- "18.00" # "18.876888776"
single$longitude <- "-100.00" # "-100.306249885"
single$localityInformation <- "Generalized lat lon per curator's request"
single$databaseSource <- "MBG"
single$institutionCode <- "MBG"
jun <- bind_rows(jun, single)
# write out data
write_csv(x = jun, file = "data/processed_occurrence/jun_072025.csv")
# Vitis rubriflora - 4 

# processing Mexico accessions  -------------------------------------------
mex <- processMex()
# write out data
write_csv(x = mex, file = "data/processed_occurrence/mexicoRecords_082025.csv")



# update the botanical garden dataset
bg1 <- processBG(path = "data/source_data/bg_survey.csv") |>
  orderNames(names = standardColumnNames) |>
  removeDuplicatesID()
write_csv(
  x = bg1,
  file = "data/processed_occurrence/botanicalGardenSurvey_072025.csv"
)

# update genesys data
gen <- processGenesysUpdate(
  path = "data/source_data/GenesysPGR_Vitis_subset.csv"
) |>
  orderNames(names = standardColumnNames) |>
  removeDuplicatesID()
# write out data
write_csv(x = gen, file = "data/processed_occurrence/genesys_072025.csv")

# read in all other datasets ----------------------------------------------
df <- readAndBind(run = TRUE) |>
  dplyr::select(-`Collecting number`)
# martineziana present
# Vitis rubriflora - 4 



# drop specific datasets based on source  ---------------------------------
df <- df |>
  dplyr::filter(
    !databaseSource %in%
      c(
        "FAO 2019 (WIEWS)",
        "GBIF 2019",
        "Global Crop Diversity Trust 2019a (Genesys)",
        "Global Crop Diversity Trust 2019b  (Cwr Occ)",
        "USDA ARS NPGS 2019a",
        "Midwest Herbaria 2019",
        "BGCI 2019 (PlantSearch)"
      )
  )

View(df)
# martineziana present
# rubriflora - 4


# Standardize names ( genus, species)  ------------------------------------
source("preprocessing/functions/standardizeNames.R")
## does not filter out data
## preforms text check steps. Structure, capilatization, etc
df1 <- standardizeNames(df)
# martineziana present
# rubriflora - 4 

# species filter and synonym check  ---------------------------------------
source("preprocessing/functions/speciesStandardization.R")

## exclude any species not used in either the county or sdm appoach
datasets <- speciesCheck(data = df1, synonymList = vitis2)
df2 <- datasets$includedData
# martineziana present
# rubriflora - 4 

novogranatensis <- df2[df2$taxon == "Vitis novogranatensis", ]
# export the dataset to be used in the SRSex
# Remove duplicated data --------------------------------------------------
uniqueTaxon <- unique(df2$taxon)
# 40 taxon here
source("preprocessing/functions/removeDupsAcrossDatasets.R")
df2_a <- uniqueTaxon |>
  purrr::map(.f = removeDups, data = df2) |>
  bind_rows()
# add back in novo
df2_a <- bind_rows(df2_a, novogranatensis)

write_csv(
  x = df2,
  file = "data/processed_occurrence/allEvaluated_data072025.csv"
)
# rubriflora - 4 
write_csv(
  x = df2_a,
  file = "data/processed_occurrence/allEvaluated_data_removedDups_072025.csv"
)
# rubriflora - 4 


# Checks on the lat lon  --------------------------------------------------

# Lat long based quality checks  ------------------------------------------
source("preprocessing/functions/checksOnLatLong.R")
d3 <- checksOnLatLong(df2)


## grab the G records with no lat lon values
d3_g <- d3$countycheck |>
  dplyr::mutate(
    county = stringr::str_remove_all(string = county, pattern = " .Co"),
    county = stringr::str_remove_all(string = county, pattern = " Co."),
    county = case_when(
      grepl("County", county) ~ county,
      is.na(county) ~ NA,
      TRUE ~ paste0(county, " County")
    )
  )


### has lat lon so can be used in both county and modeling products
valLatLon <- d3$validLatLon

# add the G records with no lat lon back to the modeling data---------------------------------------
d6 <- valLatLon |> bind_rows(d3_g)
# rubriflora -4 

# pull out single and re add
## don't know what this is maybe the single object from above
# s1 <- d6[d6$index == 510393, ]
single$latitude <- as.numeric(single$latitude)
single$longitude <- as.numeric(single$longitude)


# export data 2023-10-24 --- All g points included and duplicates between sources are removed.

#more duplicate checks - can
## many datasets do not have the sources UniqueID 
s2 <- d6[!is.na(d6$sourceUniqueID), ]
s3 <- d6[is.na(d6$sourceUniqueID), ]
d <- s2[!duplicated(s2[, c("taxon", "databaseSource", "sourceUniqueID")]), ]
# add back in the features with no sourceUniqueID 
d7 <- bind_rows(d, s3)

# remove NA taxon
d8 <- d7 |>
  dplyr::filter(!is.na(taxon))


t1 <- read_csv("data/processed_occurrence/model_data122025.csv")
# get counts per taxon and compare to see what needs alterations
prevCounts <- t1 |>
  dplyr::group_by(taxon) |>
  dplyr::count(sort = TRUE) |>
  dplyr::select(previousCount = n) |>
  dplyr::filter(!is.na(taxon))

newCounts <- d8 |>
  dplyr::group_by(taxon) |>
  dplyr::count(sort = TRUE) |>
  dplyr::select(newCount = n)


joined <- dplyr::left_join(newCounts, prevCounts, by = "taxon") |>
  dplyr::mutate(
    changeInNumber = newCount - previousCount
  )

write_csv(joined, "temp/changeInCounts12_16.csv")

# testing 
t1 <- d8 |>
  dplyr::filter(taxon == "Vitis x doaniana")
# drop some columns and test for duplicated matches 
t2 <- d8|>
  dplyr::select(
    -c(index,recordID,validLat,validLon,validLatLon)
  ) 
all_duplicates <- duplicated(t2) | duplicated(t2, fromLast = TRUE)
t3 <- t2[!all_duplicates, ]


newCounts <- t3 |>
  dplyr::group_by(taxon) |>
  dplyr::count(sort = TRUE) |>
  dplyr::select(newCount = n)


joined <- dplyr::left_join(newCounts, prevCounts, by = "taxon") |>
  dplyr::mutate(
    changeInNumber = newCount - previousCount
  )

View(joined)

# testing duplicaiton between huerto and david 
d9 <- t3 |>
  dplyr::filter(databaseSource %in% c("Huerta-Acosta publication", "UC Davis Grape Breeding Collection"))|>
  dplyr::distinct(taxon, sourceUniqueID, .keep_all = TRUE)

d10 <- t3 |>
  dplyr::filter(!databaseSource %in% c("Huerta-Acosta publication", "UC Davis Grape Breeding Collection"))

d11 <- bind_rows(d9, d10) |>
  dplyr::arrange(taxon)|>
  dplyr::mutate(
    index = row_number()
  )

# remove a rotundifolia occurrence 
d11a <- d11 |>
  dplyr::filter(latitude == 82.233333) |>
  select(index)

d11 <- d11 |>
  filter(index != d11a$index)
write_csv(x = d11, file = "data/processed_occurrence/model_data20251216.csv")

d11 |>
  dplyr::filter(taxon == "Vitis rupestris")|>
  dplyr::group_by(type)|>
  count()


newCounts <- d11 |>
  dplyr::group_by(taxon) |>
  dplyr::count(sort = TRUE) |>
  dplyr::select(newCount = n)


joined <- dplyr::left_join(newCounts, prevCounts, by = "taxon") |>
  dplyr::mutate(
    changeInNumber = newCount - previousCount
  )

View(joined)



