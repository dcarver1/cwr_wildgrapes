###
# Code for processing the input datasets to match the model data requirements
# carverd@colostate.edu
# 20230608
### 

pacman::p_load("vroom", "tidyr", "dplyr", "countrycode", "stringr", "tigris")


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

# Spatail reference files  ------------------------------------------------
countries <- st_read("data/geospatial_datasets/countries/ne_10m_admin_0_countries.gpkg")
states <- st_read("data/geospatial_datasets/states/ne_10m_admin_1_states_provinces.gpkg")
counties <-st_read("data/geospatial_datasets/counties/ne_10m_admin_2_counties.gpkg") ## US only 

 



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
ucdavis <- processDavis(path = "data/source_data/ucDavis.csv")%>%
  orderNames(names = standardColumnNames)
write_csv(ucdavis, file = "data/processed_occurance/UCDavis.csv" )

## IUNC county level data 
iunc <- processIUNC(path <- "data/source_data/iuncData.gdb")%>%
  orderNames(names = standardColumnNames)
### I'm going to keep this 

# write_csv(iunc, file = "data/processed_occurance/iunc.csv")

# compile into single dataset ---------------------------------------------
d2 <- bind_rows(gbif, grin)
d3 <- bind_rows(d2, mwh)
d4 <- bind_rows(d3, wiews)
d4a <- bind_rows(d4, genesys)
d5 <- bind_rows(d4a, bgSurvey)%>%
  bind_rows(ucdavis)


# Summary 
d5_summary <- d5 %>% 
  group_by(taxon,type)%>%
  summarise(count = n())

# write_csv(d5_summary, file = "data/processed_occurance/unfilterDataSummary.csv")


# Standardize names ( genus, species)  ------------------------------------
d5 <- standardizeNames(d5)

# species filter and synonym check  ---------------------------------------
synonymList <- read_csv("data/vitis/synonymList.csv")

datasets <- speciesCheck(data = d5, synonymList = synonymList)
write_csv(x = datasets$excludedData, file = "data/processed_occurance/excludedOnTaxonomy.csv")

d5a <- datasets$includedData



# Lat long based quality checks  ------------------------------------------
d6 <- checksOnLatLong(d5a)
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

valLatLon2 <- d7$validLatLon
# adding to the datasets that need to be evaluated
temp1 <- countyEval %>% select(names(iunc)) %>% sapply(class) 
colToChange <- temp1[temp1 == "numeric"]

iunc2 <- iunc %>%
  mutate(
    latitude = is.numeric(latitude),
    longitude = is.numeric(longitude),
    yearRecorded = is.numeric(yearRecorded),
    coordinateUncertainty = is.numeric(coordinateUncertainty)
  )

countyEval <- bind_rows(countyEval, d7$countycheck, iunc2)
write_csv(x = countyEval,  file = "data/processed_occurance/checkForIncludingInCountyMaps.csv")

# assign FIPS codes -------------------------------------------------------
d8 <- assignFIPS(valLatLon2)


# Remove duplicated data --------------------------------------------------
### need to write the function for this yet. 


write_csv(x = d8, file = "data/processed_occurance/draft_model_data.csv")


d8a <- d8 %>%
  st_drop_geometry()%>%
  group_by(taxon, type)%>%
  summarize(count = n())
write_csv(d8a, file = "data/processed_occurance/filteredDataSummary.csv")



# evaluate the county level maps ------------------------------------------



