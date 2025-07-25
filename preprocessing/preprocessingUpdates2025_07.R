pacman::p_load(dplyr, readr, sf, terra, rgbif, googledrive, countrycode,
               stringr)


# old taxonomy 
# synonymList <- read_csv("data/source_data/taxonomy20231212.csv") |>
#   dplyr::filter(modelSpecies == "Y") 
# 
# 
# 
# speciesOld <- unique(synonymList$taxon)

# # Species with model results currently 
# modelDirs <- list.dirs("data/Vitis",
#                           recursive = FALSE)
# modeledOld <- c()
# missingOld <- c()
# for(i in speciesOld){
#   val <- grepl(pattern = i, modelDirs)
#   if(TRUE %in% val){
#     modeledOld <- c(modeledOld, i)
#   }else{
#     missingOld <- c(missingOld, i)
#   }
# }

# new vitis data 
vitis2 <- read_csv("data/newWorldVitis.csv")|> 
  dplyr::select(
    "taxon" = "Scientific Name", 
    "acceptedSynonym" = "Names to include in this concept (Homotypic synonyms)",
    "Names to exclude from this concept" = "Names to exclude from this concept" , 
    "modelSpecies"    =  "Include in gap analysis?"
  )



# speciesNew <- vitis2$`Taxon Name`
# modeled <- c()
# missing <- c()
# for(i in speciesNew){
#   val <- grepl(pattern = i, modelDirs)
#   if(TRUE %in% val){
#     modeled <- c(modeled, i)
#   }else{
#     missing <- c(missing, i)
#   }
# }

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


# global variables  -------------------------------------------------------
standardColumnNames <- c(
  "taxon","originalTaxon","genus","species","latitude","longitude","databaseSource",
  "institutionCode","type","sourceUniqueID","sampleCategory","country","iso3",
  "localityInformation","biologicalStatus","collectionSource","finalOriginStat",
  "yearRecorded","county","countyFIPS","state","stateFIPS","coordinateUncertainty",
  "observerName","recordID"
)

# Download gbif data from google drive ------------------------------------
pullGBIFFromDrive(run=FALSE)


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
single$observerName <-  "Facultad de Estudios Superiores Iztacala, UNAM, (FESI-UNAM), Mexico"
single$originalTaxon <- "Vitis bloodworthiana"
single$genus <- "Vitis"
single$taxon <- "bloodworthiana"
single$type <- "G"
single$latitude <- "18.876888776" 
single$longitude <- "-100.306249885"
single$localityInformation <- "Generalized lat lon per curator's request"

jun <- bind_rows(jun, single)

# write out data
write_csv(x = jun, file = "data/processed_occurrence/jun_072025.csv")

# update genesys data 
gen <- processGenesysUpdate(path = "data/source_data/GenesysPGR_Vitis_subset.csv") |>
  orderNames(names = standardColumnNames) |>
  removeDuplicatesID()
# write out data
write_csv(x = gen, file = "data/processed_occurrence/genesys_072025.csv")

# read in all other datasets ----------------------------------------------
df <- readAndBind(run = TRUE)

# Standardize names ( genus, species)  ------------------------------------
source("preprocessing/functions/standardizeNames.R")
## does not filter out data
## preforms text check steps. Structure, capilatization, etc 
df1 <- standardizeNames(df)

# species filter and synonym check  ---------------------------------------
source("preprocessing/functions/speciesStandardization.R")

## exclude any species not used in either the county or sdm appoach
datasets <- speciesCheck(data = df1, synonymList = vitis2)
df2 <- datasets$includedData


# Checks on the lat lon  --------------------------------------------------

# Lat long based quality checks  ------------------------------------------
source("preprocessing/functions/checksOnLatLong.R")
d3 <- checksOnLatLong(df2)

## grab the G records with no lat lon values
d3_g <- d3$countycheck |>
  dplyr::mutate(
    county = stringr::str_remove_all(string = county,pattern = " .Co"),
    county = stringr::str_remove_all(string = county,pattern = " Co."),
    county = case_when(
      grepl("County", county) ~ county, 
      is.na(county) ~ NA,
      TRUE ~ paste0(county," County")
    )
  )


### has lat lon so can be used in both county and modeling products
valLatLon <- d3$validLatLon

# Spatial base data checks ------------------------------------------------
countries <- st_read("data/geospatial_datasets/countries/ne_10m_admin_0_countries.gpkg")
states <- st_read("data/geospatial_datasets/states/ne_10m_admin_1_states_provinces.gpkg")
counties <- st_read("data/geospatial_datasets/counties/ne_10m_admin_2_counties.gpkg") ## US only

source("preprocessing/functions/spatialChecks.R")
d4 <- spatialChecks(
  data = valLatLon,
  countries = countries,
  states = states,
  counties = counties
)

valLatLon2 <- d4$validLatLon


# assign FIPS codes -------------------------------------------------------
source("preprocessing/functions/assignFIPS.R")
## all the state and coutry 
d5 <- assignFIPS(valLatLon2)


# add the G records with no lat lon back to the modeling data---------------------------------------
d6 <- d5 |> bind_rows(d3_g)



# Remove duplicated data --------------------------------------------------
uniqueTaxon <- unique(d6$taxon)
source("preprocessing/functions/removeDupsAcrossDatasets.R")
d7 <- uniqueTaxon |> purrr::map(.f = removeDups, data = d6) |> bind_rows()

# export data 2023-10-24 --- All g points included and duplicates between sources are removed.
write_csv(x = d7, file = "data/processed_occurrence/model_data072025.csv")




# set up environment  -----------------------------------------------------

# primary loop ------------------------------------------------------------
genera <- unique(speciesData$genus)
# species <- rerunTaxon
species <- sort(unique(speciesData$taxon))


# #testing
i <- genera[1]
j <- species[23]

erroredSpecies <- list(noLatLon = c(),
                       lessThenEight = c(),
                       noSDM = c(),
                       noHTML = c())

# plan(strategy = "multicore", workers =4)







