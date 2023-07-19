###
# Code for processing the input datasets to match the model data requirements
# carverd@colostate.edu
# 20230608
### 

pacman::p_load("vroom", "tidyr", "dplyr", "countrycode", "stringr")


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

standardColumnNames <- c(
  "taxon","genus","species","latitude","longitude","databaseSource",       
  "institutionCode","type","sourceUniqueID","sampleCategory","country","iso3",                 
  "localityInformation","biologicalStatus","collectionSource","finalOriginStat","yearRecorded","county",               
  "countyFIPS","state","stateFIPS","coordinateUncertainty"
)


# render input datasets ---------------------------------------------------
## gbif 
gbif <- processGBIF(path = "data/source_data/gbif.csv")%>%
  dplyr::select(all_of(standardColumnNames))%>%
  mutate(across(everything(), as.character))

write_csv(x = gbif, file = "data/processed_occurance/tempOccurances_gbifOnly.csv")

## grin
grin <- processGRIN(path = "data/source_data/grin.csv")%>%
  dplyr::select(all_of(standardColumnNames))%>%
  mutate(across(everything(), as.character))
write_csv(grin, file = "data/processed_occurance/grin.csv")

## Midwest herberium
mwh <- processMidwestHerberium(path = "data/source_data/midwestherberium.csv")%>%
  dplyr::select(all_of(standardColumnNames))%>%
  mutate(across(everything(), as.character))
write_csv(mwh, file = "data/processed_occurance/midwestHerberium.csv")

## WEIWS 
wiews <- processWIEWS(path = "data/source_data/wiews.csv")%>%
  dplyr::select(all_of(standardColumnNames))%>%
  mutate(across(everything(), as.character))

## GENESYS
genesys <- processGenesys(path = "data/source_data/genesys.csv")%>%
  dplyr::select(all_of(standardColumnNames))%>%
  mutate(across(everything(), as.character))

# compile into single dataset ---------------------------------------------
d2 <- bind_rows(gbif, grin)

d3 <- bind_rows(d2, mwh)
d4 <- bind_rows(d3, wiews)
d5 <- bind_rows(d4, genesys)

View(d5)
