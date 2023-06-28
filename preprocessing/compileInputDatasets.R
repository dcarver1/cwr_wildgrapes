###
# Code for processing the input datasets to match the model data requirements
# carverd@colostate.edu
# 20230608
### 

pacman::p_load("vroom", "tidyr", "dplyr", "countrycode")


lapply(X = list.files("preprocessing/functions", pattern = ".R", full.names = TRUE),
       FUN = source)

# processing check --- performed across all sources 
## standardize species names (var subsp " " or  _ )

## synonym test 
### reassign names based on accecpted sysn

## filter on iso3 for (USA,CAN,MEX)
## valid lat long 
### remove all lat long outside of North America 
## assign state level classification
### probably lat long


# render input datasets ---------------------------------------------------
## gbif 
gbif <- processGBIF(path = "data/source_data/gbif.csv")
write_csv(x = gbif, file = "data/processed_occurance/tempOccurances_gbifOnly.csv")
## grin

## midwest herberium


# compile into single dataset ---------------------------------------------


