###
# Code for processing the input datasets to match the model data requirements
# carverd@colostate.edu
# 20230608
### 

pacman::p_load("vroom", "tidyr", "dplyr", "countrycode", "stringr", "tigris",
               "sf","readr")


lapply(X = list.files("preprocessing/functions", pattern = ".R", full.names = TRUE),
       FUN = source)


# helper funcitons --------------------------------------------------------
is.integer0 <- function(x){
  is.integer(x) && length(x) == 0L
}

orderNames <- function(data, names){
  d1 <- data %>%
    dplyr::select(all_of(names))%>%
    mutate(across(everything(), as.character))
  return(d1)
}

summarizeBySource <- function(data){
  d1 <- data %>% 
    group_by(databaseSource)%>%
    summarise(count = n())
  View(d1)
  return(d1)
}

# Spatail reference files  ------------------------------------------------
countries <- st_read("data/geospatial_datasets/countries/ne_10m_admin_0_countries.gpkg")
states <- st_read("data/geospatial_datasets/states/ne_10m_admin_1_states_provinces.gpkg")
counties <-st_read("data/geospatial_datasets/counties/ne_10m_admin_2_counties.gpkg") ## US only 

 

synonymList <- read_csv("data/vitis/synonymList.csv")


standardColumnNames <- c(
  "taxon","originalTaxon","genus","species","latitude","longitude","databaseSource",       
  "institutionCode","type","sourceUniqueID","sampleCategory","country","iso3",                 
  "localityInformation","biologicalStatus","collectionSource","finalOriginStat","yearRecorded","county",               
  "countyFIPS","state","stateFIPS","coordinateUncertainty"
)
  

# render input datasets ---------------------------------------------------
## gbif 
gbif <- processGBIF(path = "data/source_data/gbif.csv")%>%
  orderNames(names = standardColumnNames)

# write_csv(x = gbif, file = "data/processed_occurance/gbif.csv")

## grin
grin <- processGRIN(path = "data/source_data/grin.csv")%>%
  orderNames(standardColumnNames)
  
# write_csv(grin, file = "data/processed_occurance/grin.csv")

## Midwest herberium
mwh <- processMidwestHerberium(path = "data/source_data/midwestherberium.csv")%>%
  orderNames(names = standardColumnNames)

# write_csv(mwh, file = "data/processed_occurance/midwestHerberium.csv")

## WEIWS 
wiews <- processWIEWS(path = "data/source_data/wiews.csv")%>%
  orderNames(names = standardColumnNames)

# write_csv(wiews, file = "data/processed_occurance/wiews.csv")

## GENESYS
genesys <- processGenesys(path = "data/source_data/genesys.csv")%>%
  orderNames(names = standardColumnNames)

# write_csv(genesys, file = "data/processed_occurance/genesys.csv")

## botanical garden Survey 
bgSurvey <- processBG(path = "data/source_data/bg_survey.csv")%>%
  orderNames(names = standardColumnNames)

# write_csv(bgSurvey, file = "data/processed_occurance/bgSurvey.csv")

## UC Davis datasets 
ucdavis <- processDavis(path = "data/source_data/ucDavis.csv",
                        path2 = "data/source_data/ucDavis2.csv")%>%
  orderNames(names = standardColumnNames)
# write_csv(ucdavis, file = "data/processed_occurance/UCDavis.csv" )

## natural heritage county level data 
### potential to get lat lon, but be selective 
iunc <- processIUNC(path  = "data/source_data/iuncData.gdb")%>%
  orderNames(names = standardColumnNames)
# write_csv(iunc, file = "data/processed_occurance/natureServe.csv")

## BONAP
bonap <- processBonap(path = "data/source_data/bonap.csv")%>%
  orderNames(names = standardColumnNames)
# write_csv(bonap, file = "data/processed_occurance/bonap.csv")


## Data from the PNAS paper 
pnas2020 <- processPNAS(path = "data/source_data/pnas2020.csv")%>%
  orderNames(names = standardColumnNames)

# compile web sourced data into single dataset ---------------------------------------------
d2 <- bind_rows(gbif, grin,mwh, wiews,genesys,bgSurvey,pnas2020, ucdavis)
  
d2_sum <- summarizeBySource(d2)

# Summary of counts on taxon
d5_summary <- d2 %>% 
  group_by(taxon,type)%>%
  summarise(count = n())

# write_csv(d5_summary, file = "data/processed_occurance/unfilterDataSummary.csv")


# Standardize names ( genus, species)  ------------------------------------
## does not filter out data 
d5 <- standardizeNames(d2)

# species filter and synonym check  ---------------------------------------
datasets <- speciesCheck(data = d5, synonymList = synonymList)
View(datasets$excludedData)
# write_csv(x = datasets$excludedData, file = "data/processed_occurance/excludedOnTaxonomy.csv")

d5a <- datasets$includedData
d5a_sum <- summarizeBySource(d5a)


# Lat long based quality checks  ------------------------------------------
d6 <- checksOnLatLong(d5a)
d6_sum <- summarizeBySource(d6$validLatLon)

### has lat lon so can be used in both county and modeling products 
valLatLon <- d6$validLatLon

### need to evaluate if these records can be used in the county level maps.  
countyEval <- d6$countycheck

# Spatial base data checks ------------------------------------------------
### Davis data is being dropped here. Probably due to to matching state/county attribute data
d7 <- spatialChecks(data = valLatLon, 
                    countries = countries, 
                    states = states, 
                    counties = counties)


d7_sum <- summarizeBySource(d7$validLatLon)

valLatLon2 <- d7$validLatLon



# adding to the IUNC datasets that need to be evaluated as a county level input
temp1 <- countyEval %>% select(names(iunc)) %>% sapply(class) 
colToChange <- temp1[temp1 == "numeric"]

iunc2 <- iunc %>%
  mutate(
    latitude = is.numeric(latitude),
    longitude = is.numeric(longitude),
    yearRecorded = is.numeric(yearRecorded),
    coordinateUncertainty = is.numeric(coordinateUncertainty)
  )

bonap2 <- bonap %>%
  mutate(
    latitude = is.numeric(latitude),
    longitude = is.numeric(longitude),
    yearRecorded = is.numeric(yearRecorded),
    coordinateUncertainty = is.numeric(coordinateUncertainty)
  )

countyEval <- bind_rows(countyEval, d7$countycheck, iunc2, bonap)


# write_csv(x = countyEval,  file = "data/processed_occurance/checkForIncludingInCountyMaps.csv")


# evaluate the county level maps ------------------------------------------
c1 <- read_csv(file = "data/processed_occurance/checkForIncludingInCountyMaps.csv")



# assign FIPS codes -------------------------------------------------------
d8 <- assignFIPS(valLatLon2)


# Remove duplicated data --------------------------------------------------
### need to write the function for this yet. 


write_csv(x = d8, file = "data/processed_occurance/draft_model_data.csv")

### Temp feature for testing new county maps 
temp1 <- read_csv("data/processed_occurance/draft_model_data.csv")%>%
  bind_rows(iunc2, bonap2)
write_csv(temp1, file = "data/processed_occurance/testFor0905CountyMaps.csv")

d8a <- d8 %>%
  st_drop_geometry()%>%
  group_by(taxon, type)%>%
  summarize(count = n())
write_csv(d8a, file = "data/processed_occurance/filteredDataSummary.csv")



