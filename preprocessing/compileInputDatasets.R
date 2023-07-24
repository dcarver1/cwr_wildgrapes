###
# Code for processing the input datasets to match the model data requirements
# carverd@colostate.edu
# 20230608
### 

pacman::p_load("vroom", "tidyr", "dplyr", "countrycode", "stringr")


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

# Summary 
d5 %>% 
  group_by(taxon,type)%>%
  summarise(count = n()) %>%
  View()



# Standardize names ( genus, species)  ------------------------------------


d5 <- standardizeNames(d5)



# species filter and synonym check  ---------------------------------------




# processing check --- performed across all sources 
## reassign column types as needed 
d6 <- d5 %>% 
  dplyr::mutate(
    latitude = as.numeric(latitude),
    longitude = as.numeric(longitude),
    yearRecorded = as.numeric(yearRecorded),
    coordinateUncertainty = as.numeric(coordinateUncertainty)
  )


## standardize species names (var subsp " " or  _ )

## synonym test 
### reassign names based on accecpted sysn

## assign iso3 based on country 
## filter on iso3 for (USA,CAN,MEX)
d7 <- d6 %>% 
  dplyr::mutate(
    temp = countrycode(sourcevar = country, origin = "country.name", destination = "iso3c" ),
    iso3 = case_when(
    is.na(iso3) ~ temp,
    TRUE ~ iso3
  ))%>%
  dplyr::filter(iso3 %in% c("USA","CAN","MEX",NA))%>%
  select(-temp)

## valid lat long 
### remove all lat long outside of North America 
d8 <- d7 %>% 
  dplyr::mutate(
    validLatLong = case_when(
      longitude <= -50 & longitude >= -180 & latitude >= 14 ~ TRUE,
      is.na(longitude) | is.na(latitude) ~ NA,
      longitude > -50 & latitude < 14 ~ FALSE
    )
  )


# Spilting valid lat long and not points  ---------------------------------
noLatLong <- d8[d8$validLatLong != TRUE, ]
excludedObservations <- noLatLong %>% 
  mutate(excludedBecause = "Invalid Lat Long Pair")


d9 <- d8[d8$validLatLong == TRUE & !is.na(d8$validLatLong), ]
sp1 <- st_as_sf(x = d9, coords = c("longitude", "latitude"), crs = 4326,remove = FALSE)

### spatial reference tests 
### county 
selectCountries <- countries[countries$ISO_A3 %in% c("USA","CAN", "MEX"),]%>%
  dplyr::select(ISO_A3)%>%
  st_union()%>%
  st_buffer(dist = 0.01 )# buffer by ~1km to capture the coast line issue
sp1$countryCheck <- sf::st_intersects(x = sp1, y = selectCountries)

# this is pretty suspect... st_intersect returns a interger(0) value for no match.harder to filter on directly
pointsOutsideOfCountries <- sp1[!sp1$countryCheck %in% c(1),] %>%
  mutate(excludedBecause = "1km outside of country boundary")%>%
  select(-countryCheck)%>%
  st_drop_geometry()

excludedObservations <- bind_rows(excludedObservations, pointsOutsideOfCountries)

pointsOnLand <- sp1 %>% 
  filter(countryCheck %in% c(1))%>%
  select(-countryCheck)
  

## assign state level classification
counter <- 1 

for(i in c("USA","CAN", "MEX")){
  
  sp2 <- pointsOnLand 
  
  ## test for mismatch between noted country and interested feature 
  # Country select 
  country1 <- countries %>% 
    dplyr::filter(ISO_A3 == i)
  
  sp2$countryInter <- sp2 %>%
    st_intersects(country1)%>%
    lapply(is.integer0)%>%
    unlist()
  
  ## outside of country and ISO3 matches 
  nonCountryPoints <- sp2 %>%
    filter(countryInter == TRUE,
           iso3 == i)%>%
    mutate(excludedBecause = "Outside of country boundary and matched ISO3")%>%
    select(-countryInter)%>%
    st_drop_geometry()
    
  
  excludedObservations <- bind_rows(excludedObservations, nonCountryPoints)
  
  # dataset for state level eval 
  sp3 <- sp2 %>%
    filter(countryInter == FALSE)%>%
    select(-countryInter)
  
  # state select  
  state1 <- states %>% 
    dplyr::filter(adm0_a3 == i)
  
  sp3$stateInter <- sp3 %>%
    st_intersects(state1)%>%
    unlist()
  
  names1 <- state1$name
  index <- names1[sp3$stateInter]
  sp3 <- sp3 %>%
    mutate(georefState = index,
           stateTest = case_when(
             georefState == state & !is.na(state) ~ TRUE,
             is.na(state) ~ NA,
             georefState != state & !is.na(state) ~ FALSE
           )
          )
  # states do no match 
  statesDontMatch <- sp3 %>%
    filter(stateTest ==FALSE)%>%
    mutate(excludedBecause = "Recorded State and georeferenced state don't match")%>%
    select(-georefState,-stateTest)%>%
    st_drop_geometry()
  
  excludedObservations <- bind_rows(excludedObservations, statesDontMatch)
  
  sp4 <- sp3 %>%
    filter(stateTest != FALSE)%>%
    mutate(state = case_when(
      is.na(stateTest) ~ georefState,
      TRUE ~ state
    ))%>%
    select(-georefState, - stateTest)
  
  if(i == "USA"){
    ## assign county level classification
    county1 <- counties %>% 
      filter(GU_A3 == i)
    
    sp4$countyInter <- sp4 %>%
      st_intersects(county1)%>%
      unlist()
    
    names2 <- county1$NAME
    index2 <- names2[sp4$countyInter]
    sp4 <- sp4 %>%
      mutate(georefCounty = index2,
             countyTest = case_when(
               georefCounty == county & !is.na(county) ~ TRUE,
               is.na(county) ~ NA,
               georefCounty != county & !is.na(county) ~ FALSE
             )
      )
    
    # county do no match 
    countyDontMatch <- sp4 %>%
      filter(countyTest ==FALSE)%>%
      mutate(excludedBecause = "Recorded County and georeferenced County don't match")%>%
      select(-georefCounty,-countyTest,-countyInter,-stateInter)%>%
      st_drop_geometry()
    
    excludedObservations <- bind_rows(excludedObservations, countyDontMatch)
    
    sp5 <- sp4 %>%
      filter(countyTest != FALSE)%>%
      mutate(county = case_when(
        is.na(countyTest) ~ georefCounty,
        TRUE ~ county
      ))%>%
      select(-georefCounty,-countyTest,-countyInter,-stateInter)
    
    validSpatialData <- sp5
    
  }else{
    # no count data for can/mex
    validSpatialData <- bind_rows(validSpatialData, sp4)
  }
}

write_csv(validSpatialData, file = "data/processed_occurance/validSpatialData.csv")
write_csv(excludedObservations, file = "data/processed_occurance/excludedObservations.csv")

summary1 <- validSpatialData %>% 
  st_drop_geometry()%>%
  group_by(taxon, type)%>%
  summarize(count = n())


View(summary1)

