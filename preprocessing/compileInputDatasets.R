###
# Code for processing the input datasets to match the model data requirements
# carverd@colostate.edu
# 20230608
### 

pacman::p_load("vroom", "tidyr", "dplyr", "countrycode", "stringr", "tigris")


lapply(X = list.files("preprocessing/functions", pattern = ".R", full.names = TRUE),
       FUN = source)
is.integer0 <- function(x){
  is.integer(x) && length(x) == 0L
}

# Spatail reference files  ------------------------------------------------
countries <- st_read("data/geospatial_datasets/countries/ne_10m_admin_0_countries.gpkg")
states <- st_read("data/geospatial_datasets/states/ne_10m_admin_1_states_provinces.gpkg")
counties <-st_read("data/geospatial_datasets/counties/ne_10m_admin_2_counties.gpkg") ## US only 

 



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
write_csv(wiews, file = "data/processed_occurance/wiews.csv")


## GENESYS
genesys <- processGenesys(path = "data/source_data/genesys.csv")%>%
  dplyr::select(all_of(standardColumnNames))%>%
  mutate(across(everything(), as.character))
write_csv(genesys, file = "data/processed_occurance/genesys.csv")

# compile into single dataset ---------------------------------------------
d2 <- bind_rows(gbif, grin)

d3 <- bind_rows(d2, mwh)
d4 <- bind_rows(d3, wiews)
d5 <- bind_rows(d4, genesys)

View(d5)

# Summary 
d5 %>% 
  group_by(taxon,type)%>%
  summarise(count = n()) %>%
  View()



# Standardize names ( genus, species)  ------------------------------------
d5 <- standardizeNames(d5)

# species filter and synonym check  ---------------------------------------
synonymList <- read_csv("data/vitis/synonymList.csv")
datasets <- speciesCheck(data = d5, synonymList = synonymList)
write_csv(x = datasets$excludedData, file = "data/processed_occurance/excludedOnTaxonomy.csv")

d5a <- datasets$includedData



# Lat long based quality checks  ------------------------------------------
d6 <- checksOnLatLong(d5a)


# Spatial base data checks ------------------------------------------------

d7 <- spatialChecks(d6, 
                    countries = countries, 
                    states = states, 
                    counties = counties)


# assign FIPS codes -------------------------------------------------------
d8 <- assignFIPS(d7)

