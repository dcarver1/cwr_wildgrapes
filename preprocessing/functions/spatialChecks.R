
# data <- d6

spatialChecks <- function(data, countries,states,counties){
  # create a spatial object 
  sp1 <- st_as_sf(x = data, coords = c("longitude", "latitude"), crs = 4326,remove = FALSE)%>%
    select(-validLat,-validLon,-validLatLon)
  
  # buffer selected counties by 1km for three countries of interest
  selectCountries <- countries[countries$ISO_A3 %in% c("USA","CAN", "MEX"),]%>%
    dplyr::select(ISO_A3)%>%
    st_union()%>%
    st_crop(xmin = -180, ymin = 5, xmax = -50, ymax = 90)%>%
    st_buffer(dist = 0.01 )# buffer by ~1km to capture the coast line issue
  # intersect the features to exclude objects not present based on current lat long
  sp1$countryCheck <- sf::st_intersects(x = sp1, y = selectCountries)
  
  exclude1 <-  sp1[!sp1$countryCheck %in% c(1),] %>%
    select(-countryCheck)%>%
    as.data.frame()
  write_csv(x = exclude1, file = "data/processed_occurance/excludeOutsideOfCountries.csv")
  
  sp2 <- sp1[!sp1$index %in% exclude1$index, ] %>%
    select(-countryCheck)
  
# just a temp object for condition statement later on. 
excludedObservations <- exclude1[0,] 

# test for agreement between country, states and counties -----------------
  for(i in c("USA","CAN", "MEX")){
    sp3 <- sp2
    
    ## test for mismatch between noted country and interested feature 
    # Country select 
    country1 <- countries %>% 
      dplyr::filter(ISO_A3 == i)
    
    sp3$countryInter <- sp3 %>%
      st_intersects(country1)%>%
      lapply(is.integer0)%>%
      unlist()
    
    ## outside of country and ISO3 matches 
    nonCountryPoints <- sp3 %>%
      filter(countryInter == TRUE,
             iso3 == i)%>%
      mutate(excludedBecause = "Outside of country boundary and matched ISO3")%>%
      select(-countryInter)
    
    if(nrow(excludedObservations) == 0){
      excludedObservations <- nonCountryPoints
    }else{
      excludedObservations <- bind_rows(excludedObservations, nonCountryPoints)
    }
    
    # dataset for state level eval 
    sp4 <- sp3 %>%
      filter(countryInter == FALSE | is.na(countryInter))%>%
      select(-countryInter)
    
    # state select  
    state1 <- states %>% 
      dplyr::filter(adm0_a3 == i)
    
    sp4$stateInter <- sp4 %>%
      st_intersects(state1)%>%
      unlist()
    # grab names of all states 
    names1 <- state1$name
    # bind names to dataset and test for match against 
    sp4 <- sp4 %>%
      mutate(georefState = names1[sp4$stateInter],
             stateTest = case_when(
               georefState == state & !is.na(state) ~ TRUE,
               is.na(state) ~ NA,
               georefState != state & !is.na(state) ~ FALSE
             )
      )
    # states do no match 
    statesDontMatch <- sp4 %>%
      filter(stateTest ==FALSE)%>%
      mutate(excludedBecause = "Recorded State and georeferenced state don't match")%>%
      select(-georefState,-stateTest, -stateInter)
    
    excludedObservations <- bind_rows(excludedObservations, statesDontMatch)
    
    sp5 <- sp4 %>%
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
      sf::sf_use_s2(FALSE)
      sp5$countyInter <- sp5 %>%
        st_intersects(county1)%>%
        unlist()
      
      names2 <- county1$NAME
      index2 <- names2[sp5$countyInter]
      sp5 <- sp5 %>%
        mutate(georefCounty = index2,
               countyTest = case_when(
                 georefCounty == county & !is.na(county) ~ TRUE,
                 is.na(county) ~ NA,
                 georefCounty != county & !is.na(county) ~ FALSE
               )
        )
      
      # county do no match 
      countyDontMatch <- sp5 %>%
        filter(countyTest ==FALSE)%>%
        mutate(excludedBecause = "Recorded County and georeferenced County don't match")%>%
        select(-georefCounty,-countyTest,-countyInter,-stateInter)%>%
        st_drop_geometry()
      
      excludedObservations <- bind_rows(excludedObservations, countyDontMatch)
      
      sp6 <- sp5 %>%
        filter(countyTest != FALSE)%>%
        mutate(county = case_when(
          is.na(countyTest) ~ georefCounty,
          TRUE ~ county
        ))%>%
        select(-georefCounty,-countyTest,-countyInter,-stateInter)
      
      validSpatialData <- sp6
      
    }else{
      # no count data for can/mex
      validSpatialData <- bind_rows(validSpatialData, sp5)
    }
  }
}