
# data <- d6

spatialChecks <- function(data, countries, states, counties){
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
  
  # recorded that have a lat long value outside of a 1km buffer of the country boundary, primarily coastal point that might be able to be utilized
  write_csv(x = exclude1, file = "data/processed_occurance/excludeOutsideOfCountries.csv")
  
  ## grab the data that is within the countries of interest
  sp2 <- sp1[!sp1$index %in% exclude1$index, ] %>%
    select(-countryCheck)

  # deal with NA in ISO3 coulumn
  sp2a <- sp2 %>% 
    filter(is.na(sp2$iso3))%>%
    st_intersection(y = countries)%>%
    mutate(iso3 = ISO_A3,
           country = ADMIN)%>%
    select(names(sp2))
  
  # generate new dataset for the evaluation process 
  sp3 <- sp2 %>% 
    filter(!index %in% sp2a$index)%>%
    bind_rows(sp2a)%>%
    #standardize admin names 
    mutate(country = case_when(
      country =="United States" ~ "United States of America",
      country =="USA" ~ "United States of America",
      country =="UNITED STATES" ~ "United States of America",
      country =="U.S.A." ~ "United States of America",
      country == "MEXICO" ~ "Mexico",
      TRUE ~ country
    ))%>%
    mutate(iso3 = case_when(
        country == "CAN" ~ "CAN",
        country == "Canada" ~ "CAN",
        country == "Mexico" ~ "MEX",
        country == "México" ~ "MEX",
        country == "MEX" ~ "MEX",
        country == "U.S.A." ~ "USA",
        country == "United States" ~ "USA",
        country == "UNITED STATES" ~ "USA",
        country == "United States of America" ~ "USA",
        country == "usa" ~ "USA",
        country == "USA" ~ "USA",
        TRUE ~ iso3
      )
    )
  
  # direct intesect with country -- no buffer 
  spCounty <-st_intersection(sp3, states)%>%
    select(names(sp2), name, adm0_a3 )
  
  countriesNames <- c("USA","CAN", "MEX")
  # just a temp object for condition statement later on. 
  excludedObservations <- exclude1[0,] 
  
  # test for the interestion between country, state, county
  for(i in seq_along(countriesNames)){
    if(i == 1){
      df <- spCounty[0,]%>%
        dplyr::mutate( countryMatch = NA,
                       stateMatch = NA,
                       countyMatch = NA)
    }
    
    iso3 <- countriesNames[i]
    
    c1 <- spCounty[spCounty$adm0_a3 == iso3, ]   %>%
      dplyr::mutate(
        countryMatch = iso3 == adm0_a3,
        stateMatch = state == name,
        countyMatch = NA
      )
   
    
    if(iso3 == "USA"){
      c2 <- st_intersection(c1, counties)%>%
        dplyr::mutate(
          countyMatch = county == NAME
        )
    
      c2$county[is.na(c2$county)] <- c2$NAME[is.na(c2$county)]
      c2 <- c2 %>%
        dplyr::select(names(c1))
      
      df <- bind_rows(df,c2)
    }else{
      df <- bind_rows(df,c1)  
    }
  }
  
  ### assign NA values to State, and County when Possible. 
  # state
  df$state[is.na(df$state)] <- df$name[is.na(df$state)]
  
  
  
  
  
  ## unfiltered spatial data 
  write_csv(x = df, file = "data/processed_occurance/reference_AllLatLonValues.csv")
  
  ### organized missed matched data for review 
  noCountryMatch <- df %>% filter(countryMatch == FALSE)
  noStateMatch <- df %>% filter(stateMatch == FALSE)
  noCountyMatch <- df %>% filter(iso3 == "USA" & countyMatch ==FALSE)
  
  write_csv(x = noCountryMatch, file = "data/processed_occurance/evaluateNocountryMatch.csv")
  write_csv(x = noStateMatch, file = "data/processed_occurance/evaluatenoStateMatch.csv")
  write_csv(x = noCountyMatch, file = "data/processed_occurance/evaluatenoCountyMatch.csv")
  
  ### return matched data 
  df2 <- df%>% filter(
    countryMatch == TRUE,
    stateMatch != FALSE,
    countryMatch != FALSE
  )%>%
    select(names(sp2))
    return(list(validLatLon = df2,
                countycheck = bind_rows(exclude1, noCountryMatch, noStateMatch,noCountyMatch)) )
}

