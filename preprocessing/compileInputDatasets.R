###
# Code for processing the input datasets to match the model data requirements
# carverd@colostate.edu
# 20230608
###

pacman::p_load("vroom",
               "tidyr",
               "dplyr",
               "countrycode",
               "stringr",
               "tigris",
               "sf",
               "readr")

# helps with the intersection processing time
sf_use_s2(FALSE)

lapply(
  X = list.files(
    "preprocessing/functions",
    pattern = ".R",
    full.names = TRUE
  ),
  FUN = source
)


# Spatail reference files  ------------------------------------------------
countries <- st_read("data/geospatial_datasets/countries/ne_10m_admin_0_countries.gpkg")
states <- st_read("data/geospatial_datasets/states/ne_10m_admin_1_states_provinces.gpkg")
counties <- st_read("data/geospatial_datasets/counties/ne_10m_admin_2_counties.gpkg") ## US only

synonymList <- read_csv("data/source_data/taxonomy20231212.csv") |>
  dplyr::filter(modelSpecies == "Y")


uniqueTaxon <- unique(synonymList$taxon)


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
  "coordinateUncertainty"
)


# render input datasets ---------------------------------------------------
overwrite <-FALSE


## gbif
if(overwrite == TRUE){
  gbif <- processGBIF(path = "data/source_data/gbif.csv") |>
    orderNames(names = standardColumnNames) |>
    removeDuplicatesID()
  # write out data
  write_csv(x = gbif, file = "data/processed_occurrence/gbif.csv")
}else{
  gbif <- read_csv("data/processed_occurrence/gbif.csv",col_types = cols(.default = "c"))
}

## grin
if(overwrite == TRUE){
grin <- processGRIN(path = "data/source_data/grin.csv") |>
  orderNames(standardColumnNames) |>
  removeDuplicatesID()
  # write out data
  write_csv(grin, file = "data/processed_occurrence/grin.csv")
}else{
  grin <- read_csv("data/processed_occurrence/grin.csv",col_types = cols(.default = "c"))
}

## seinet
if(overwrite == TRUE){
seinet <- processSEINET(path = "data/source_data/midwestherberium.csv")|>
  orderNames(names = standardColumnNames) |>
  removeDuplicatesID()
  
# write out data
  write_csv(seinet, file = "data/processed_occurrence/seinet.csv")
}else{
  seinet <- read_csv("data/processed_occurrence/seinet.csv",col_types = cols(.default = "c"))
}

## WEIWS
if(overwrite == TRUE){
wiews <- processWIEWS(path = "data/source_data/wiews.csv") |>
  orderNames(names = standardColumnNames) |>
  removeDuplicatesID()
  # write out data
  write_csv(wiews, file = "data/processed_occurrence/wiews.csv")
}else{
  wiews <- read_csv("data/processed_occurrence/wiews.csv",col_types = cols(.default = "c"))
}

## GENESYS
if(overwrite == TRUE){
genesys <- processGenesys(path = "data/source_data/genesys.csv") |>
  orderNames(names = standardColumnNames) |>
  removeDuplicatesID()
  # write out data  
  write_csv(genesys, file = "data/processed_occurrence/genesys.csv")
}else{
  genesys <- read_csv("data/processed_occurrence/genesys.csv",col_types = cols(.default = "c"))
}

## botanical garden Survey
if(overwrite == TRUE){
bgSurvey <- processBG(path = "data/source_data/bg_survey.csv") |>
  orderNames(names = standardColumnNames) |>
  removeDuplicatesID()
  # write out data
  write_csv(bgSurvey, file = "data/processed_occurrence/bgSurvey.csv")
}else{
  bgSurvey <- read_csv("data/processed_occurrence/bgSurvey.csv",col_types = cols(.default = "c"))
}

## UC Davis datasets
if(overwrite == TRUE){
ucdavis <- processDavis(path = "data/source_data/ucDavis.csv",
                        path2 = "data/source_data/ucDavis2.csv") |>
  orderNames(names = standardColumnNames) |>
  removeDuplicatesID()
  # write out data
  write_csv(ucdavis, file = "data/processed_occurrence/UCDavis.csv" )
}else{
  ucdavis <- read_csv("data/processed_occurrence/UCDavis.csv",col_types = cols(.default = "c"))
}

## natural heritage county level data
### potential to get lat lon, but be selective
if(overwrite == TRUE){
iunc <- processIUNC(path  = "data/source_data/iuncData.gdb") |>
  orderNames(names = standardColumnNames)|>
  removeDuplicatesID()
  # write out data
  write_csv(iunc, file = "data/processed_occurrence/natureServe.csv")
}else{
  iunc <- read_csv("data/processed_occurrence/natureServe.csv",col_types = cols(.default = "c"))
}

## BONAP
if(overwrite == TRUE){
bonap <- processBonap(path = "data/source_data/bonap.csv") |>
  orderNames(names = standardColumnNames) |>
  removeDuplicatesID()
  # write out data
  write_csv(bonap, file = "data/processed_occurrence/bonap.csv")
}else{
  bonap <- read_csv("data/processed_occurrence/bonap.csv",col_types = cols(.default = "c"))
}

## Data from the PNAS paper
if(overwrite == TRUE){
  pnas2020 <- processPNAS(path = "data/source_data/pnas2020.csv") |>
  orderNames(names = standardColumnNames) |>
  removeDuplicatesID()
  
  # write out data
  write_csv(pnas2020, file = "data/processed_occurrence/pnas2020.csv")
}else{
  pnas2020 <- read_csv("data/processed_occurrence/pnas2020.csv",col_types = cols(.default = "c"))
}
  
  
  
  
# compile web sourced data into single dataset ---------------------------------------------
d2 <-  bind_rows(gbif, grin, seinet, wiews, genesys, bgSurvey, pnas2020, ucdavis) 
# add an index column, basically row name 
d2$index <- 1:nrow(d2)

d2_sum <- summarizeBySource(d2)

# Summary of counts on taxon
d5_summary <- d2 |>
  group_by(taxon, type) |>
  summarise(count = n())

write_csv(d5_summary, file = "data/processed_occurrence/unfilterDataSummary.csv")


# Standardize names ( genus, species)  ------------------------------------
## does not filter out data
## preforms text check steps. Structure, capilatization, etc 
d5 <- standardizeNames(d2)

# species filter and synonym check  ---------------------------------------
## exclude any species not used in either the county or sdm appoach
datasets <- speciesCheck(data = d5, synonymList = synonymList)
# write_csv(x = datasets$excludedData, file = "data/processed_occurrence/excludedOnTaxonomy.csv")

d5a <- datasets$includedData
d5a_sum <- summarizeBySource(d5a)


# Lat long based quality checks  ------------------------------------------
d6 <- checksOnLatLong(d5a)
d6_sum <- summarizeBySource(d6$validLatLon)
## grab the G records with no lat lon values
d6_g <- d6$countycheck |>
  filter(type == "G")


### has lat lon so can be used in both county and modeling products
valLatLon <- d6$validLatLon

### need to evaluate if these records can be used in the county level maps.
countyEval <- d6$countycheck

# Spatial base data checks ------------------------------------------------
### Davis data is being dropped here. Probably due to to matching state/county attribute data
### some filter condition not accounting the NA values
d7 <- spatialChecks(
  data = valLatLon,
  countries = countries,
  states = states,
  counties = counties
)


d7_sum <- summarizeBySource(d7$validLatLon)

valLatLon2 <- d7$validLatLon


# adding to the IUNC datasets that need to be evaluated as a county level input
temp1 <- countyEval |> select(names(iunc)) |> sapply(class)
colToChange <- temp1[temp1 == "numeric"]

iunc2 <- iunc |>
  mutate(
    latitude = 0,
    longitude = 0,
    yearRecorded = is.numeric(yearRecorded),
    coordinateUncertainty = is.numeric(coordinateUncertainty)
  ) |>
  mutate(latitude = case_when(latitude == 0 ~ NA),
         longitude = case_when(longitude == 0 ~ NA))

bonap2 <- bonap |>
  mutate(
    latitude = 0,
    longitude = 0,
    yearRecorded = is.numeric(yearRecorded),
    coordinateUncertainty = is.numeric(coordinateUncertainty)
  ) |>
  mutate(latitude = case_when(latitude == 0 ~ NA),
         longitude = case_when(longitude == 0 ~ NA))

countyEval <- bind_rows(countyEval, d7$countycheck, iunc2, bonap2)


write_csv(x = countyEval,  file = "data/processed_occurrence/checkForIncludingInCountyMaps.csv")



# assign FIPS codes -------------------------------------------------------
d8 <- assignFIPS(valLatLon2)


# add the G records with no lat lon back to the modeling data---------------------------------------
d8a <- d8 |> bind_rows(d6_g)


# Remove duplicated data --------------------------------------------------
d9 <- uniqueTaxon |> purrr::map(.f = removeDups, data = d8a) |> bind_rows()

# export data 2023-10-24 --- All g points included and duplicates between sources are removed.
write_csv(x = d9, file = "data/processed_occurrence/draft_model_data.csv")



# evaluate the county level maps ------------------------------------------
c1 <-
  read_csv(file = "data/processed_occurrence/checkForIncludingInCountyMaps.csv")

c2 <-
  checkCounties(countyCheckData = c1,
                states = states,
                counties = counties)

write_csv(x = c2$exclude, file = "data/processed_occurrence/countyCheck_Exclude.csv")
write_csv(x = c2$include, file = "data/processed_occurrence/countyCheck_Include.csv")

# duplicate check for county data
### keep en eye out for duplicated within the county only feautes. I currently don't have a check put in place but it also
### seems like it;s not the big of an issue.
c3 <- bind_rows(c2$include, d9) |>
  st_drop_geometry() |>
  select(-validLat,-validLon, -validLatLon,-index) |>
  assignFIPS()

write_csv(x = c3, file = "data/processed_occurrence/tempDataForCountyMaps_20231025.csv")



