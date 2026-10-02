# path <- "data/processed_occurance/checkForIncludingInCountyMaps.csv"


evaluateForCounty <-function(path){
  # conditions that have been check for. 
  ## validLatLon
  ## country check 
  ## state check 
  ## county check 
  
  c1 <- read_csv(path)
  # ensure that iso3 values are matched for can, mex, usa
  c1 <- c1 %>%
    mutate(
      iso3 = case_when(
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
  # expected bad 
  ## no lat long or county name information
  bad1 <- c1 %>% 
    filter(is.na(latitude) | is.na(longitude) & is.na(county))
    
  c2 <- c1[!c1$index %in% bad1$index,]
  # the points are lat long present but not state data. State was reassigned if NA
  # so missing values means it is spatially not overlapped with any feautres 
  bad2 <- c2 %>% 
    filter(is.na(state))
  
  c3 <- c2[!c2$index %in% bad2$index, ]
  View(c3)
  
  #### happy with the exclusions up to this point, now I need to differentiate when to 
  #### 1. bring data back into the modeling methodology (ex. mismatched name between states but county matches)
  #### 2. what can be used for county level data onlyu. 
  
  
}