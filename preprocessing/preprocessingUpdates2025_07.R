pacman::p_load(dplyr, readr, sf, terra, rgbif, googledrive, countrycode)


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
# write out data
write_csv(x = jun, file = "data/processed_occurrence/jun_072025.csv")



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
d6 <- checksOnLatLong(d5a)
d6_sum <- summarizeBySource(d6$validLatLon)
## grab the G records with no lat lon values
d6_g <- d6$countycheck |>
  dplyr::mutate(
    county = stringr::str_remove_all(string = county,pattern = " .Co"),
    county = stringr::str_remove_all(string = county,pattern = " Co."),
    county = case_when(
      grepl("County", county) ~ county, 
      is.na(county) ~ NA,
      TRUE ~ paste0(county," County")
    )
  )










