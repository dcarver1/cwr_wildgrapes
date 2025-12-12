pacman::p_load(dplyr, readr, sf, terra, rgbif, googledrive, countrycode,
               stringr)


# new vitis data 
vitis2 <- read_csv("data/New World Vitis.csv")|> 
  dplyr::select(
    "taxon" = "Scientific Name", 
    "acceptedSynonym" = "Names to include in this concept (Homotypic synonyms)",
    "Names to exclude from this concept" = "Names to exclude from this concept" , 
    "modelSpecies"    =  "Include in gap analysis?"
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
single$species <- "bloodworthiana"
single$type <- "G"
single$latitude <-  "18.00"# "18.876888776" 
single$longitude <- "-100.00"  # "-100.306249885"
single$localityInformation <- "Generalized lat lon per curator's request"
single$databaseSource <- "MBG"
single$institutionCode <- "MBG"
jun <- bind_rows(jun, single)
# write out data
write_csv(x = jun, file = "data/processed_occurrence/jun_072025.csv")


# processing Mexico accessions  -------------------------------------------
mex <- processMex()
# write out data
write_csv(x = mex, file = "data/processed_occurrence/mexicoRecords_082025.csv")



# update genesys data 
gen <- processGenesysUpdate(path = "data/source_data/GenesysPGR_Vitis_subset.csv") |>
  orderNames(names = standardColumnNames) |>
  removeDuplicatesID()
# write out data
write_csv(x = gen, file = "data/processed_occurrence/genesys_072025.csv")

# read in all other datasets ----------------------------------------------
df <- readAndBind(run = TRUE) |>
  dplyr::select(-`Collecting number`)
# martineziana present 


# drop specific datasets based on source  ---------------------------------
df <- df |> 
  dplyr::filter(
    !databaseSource %in% c("FAO 2019 (WIEWS)",
                           "GBIF 2019",
                           "Global Crop Diversity Trust 2019a (Genesys)",
                           "Global Crop Diversity Trust 2019b  (Cwr Occ)",
                           "USDA ARS NPGS 2019a",
                           "Midwest Herbaria 2019")
  )

View(df)
# martineziana present 


# Standardize names ( genus, species)  ------------------------------------
source("preprocessing/functions/standardizeNames.R")
## does not filter out data
## preforms text check steps. Structure, capilatization, etc 
df1 <- standardizeNames(df)
# martineziana present 

# species filter and synonym check  ---------------------------------------
source("preprocessing/functions/speciesStandardization.R")

## exclude any species not used in either the county or sdm appoach
datasets <- speciesCheck(data = df1, synonymList = vitis2)
df2 <- datasets$includedData
# martineziana present 


novogranatensis <- df2[df2$taxon == "Vitis novogranatensis" ,]
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

write_csv(x = df2, file = "data/processed_occurrence/allEvaluated_data072025.csv")
write_csv(x = df2_a, file = "data/processed_occurrence/allEvaluated_data_removedDups_072025.csv")

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
    ))


### has lat lon so can be used in both county and modeling products
valLatLon <- d3$validLatLon

# add the G records with no lat lon back to the modeling data---------------------------------------
d6 <- valLatLon |> bind_rows(d3_g)

# pull out single and re add 
## don't know what this is maybe the single object from above
# s1 <- d6[d6$index == 510393, ]
single$latitude <- as.numeric(single$latitude)
single$longitude <- as.numeric(single$longitude)


# export data 2023-10-24 --- All g points included and duplicates between sources are removed.

#more duplicate checks 
d <- d6[!duplicated(d6[,c("taxon", "databaseSource" , "sourceUniqueID")]), ]
View(d)

# # conflict between gbif and sienet 
# d1 <- d |>
#   dplyr::filter(databaseSource  %in% c("GBIF")) |>
#   tidyr::separate(
#       col = localityInformation,               # The column to split
#       into = c("temp", "gbifLoc"),    # The names for the new columns
#       sep = " -- " ,
#       remove = FALSE # The separator to split on
#     )
#   
# d2 <- d |>
#   dplyr::filter(!databaseSource  %in% c("GBIF")) 

# row by row exclusion 
# for(row in 1:nrow(d2)){
#   print(row)
#   
#   feat <- d2[row,]
#   taxon <- feat$taxon
#   lat <- feat$latitude
#   lon <- feat$longitude
#   loc <- feat$localityInformation
#   year <- feat$yearRecorded
#   
#   # test for presence in gbif 
#   test <- d1 |>
#     dplyr::filter(
#       taxon == taxon,
#       latitude == lat,
#       longitude == lon,
#       gbifLoc == loc,
#       yearRecorded == year
#     )
#   if(nrow(test) >0 ){
#     print(paste0("removed", row))
#     d1 <- d1[!d1$index == test$index, ]
#   }
# }

# bind the data back in 
# d7 <- bind_rows(d1,d2)

# remove NA taxon 
d8 <- d |>
  dplyr::filter(!is.na(taxon))
  


write_csv(x = d8, file = "data/processed_occurrence/model_data122025.csv")


t1<- read_csv("temp/allVitisData082025.csv")

# get counts per taxon and compare to see what needs alterations 
prevCounts <- t1 |>
  dplyr::group_by(taxon)|>
  dplyr::count( sort =TRUE) |>
  dplyr::select(previousCount = n) |>
  dplyr::filter(!is.na(taxon))

newCounts <- d8 |>
  dplyr::group_by(taxon)|>
  dplyr::count( sort =TRUE)|>
  dplyr::select(newCount = n)


joined <- dplyr::left_join(newCounts, prevCounts, by = "taxon") |>
  dplyr::mutate(
    changeInNumber = newCount - previousCount
  )

write_csv(joined, "temp/changeInCounts.csv")
