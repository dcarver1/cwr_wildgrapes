

checksOnLatLong <- function(data){
  # convert datatypes. 
  df1 <- data  %>% 
    dplyr::mutate(
      latitude = as.numeric(latitude),
      longitude = as.numeric(longitude),
      yearRecorded = as.numeric(yearRecorded),
      coordinateUncertainty = as.numeric(coordinateUncertainty)
    ) %>% 
    # assign the countycode 
    dplyr::mutate(
      temp = countrycode(sourcevar = country, origin = "country.name", destination = "iso3c" ),
      iso3 = case_when(
        is.na(iso3) ~ temp,
        TRUE ~ iso3
      ))%>% 
    select(-temp)
  
  # Export the localities that are not in the countries of interest 
  df2 <- df1 %>%
    dplyr::filter(iso3 %in% c("USA","CAN","MEX",NA))
  export1 <- df1[!df1$index %in% df2$index, ]
  
  write_csv(x = export1, file = "data/processed_occurance/excludeOnIso3.csv")
  
  df3 <-df2 %>% 
    # Test the lat long values based on generalized bounding box. 
    dplyr::mutate(
      validLat = case_when(
        latitude >= 14 ~ TRUE,
        is.na(latitude) ~ NA, 
        latitude < 14 ~ FALSE
        ),
      validLon = case_when(
        longitude <= -50 ~ TRUE,
        is.na(longitude) ~ NA, 
        TRUE ~ FALSE
      ),
      validLatLon = case_when(
        validLat == TRUE & validLon == TRUE ~ TRUE,
        is.na(validLat) ~ NA,
        is.na(validLon) ~ NA,
        TRUE ~ FALSE
      )
    )
    
  export2 <- df3 %>% 
    dplyr::filter(validLatLon == FALSE)
  write_csv(x = export2, file = "data/processed_occurance/excludedOnLatLonBoundingBox.csv")
  
  export3 <- df3 %>% 
    filter(is.na(validLatLon))
  write_csv(x = export3, file = "data/processed_occurance/considerNoLatLonProvided.csv")
  
  df4 <- df3 %>% 
    filter(validLatLon == TRUE)
  return(df4)
}
