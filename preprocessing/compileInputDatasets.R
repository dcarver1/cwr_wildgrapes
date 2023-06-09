###
# Code for processing the input datasets to match the model data requirements
# carverd@colostate.edu
# 20230608
### 

pacman::p_load("vroom", "tidyr", "dplyr", "countrycode")


lapply(X = list.files("preprocessing/functions", pattern = ".R", full.names = TRUE),
       FUN = source)

# processing check 
## synonym test 
## valid lat long 
## filter on iso3 for (USA,CAN,MEX)

# data sources 
gbif <- processGBIF(path = "E:/mbg/wild grapes/data/gbif.csv")
