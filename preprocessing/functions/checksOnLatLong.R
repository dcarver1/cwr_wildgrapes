

# exemptTaxa: taxa whose records are kept regardless of country. The published
# dataset kept Vitis tiliifolia and V. popenoei records from Central America and
# the Caribbean (run_all072025.R swapped in a pre-filter July 2025 version of
# those two species; 1,624 + 9 records). Passing them here reproduces that
# intent from the current data instead of from an old file.
checksOnLatLong <- function(data, exemptTaxa = NULL){
  # convert datatypes. 
  df1 <- data  |> 
    dplyr::mutate(
      latitude = as.numeric(latitude),
      longitude = as.numeric(longitude),
      yearRecorded = as.numeric(yearRecorded),
      coordinateUncertainty = as.numeric(coordinateUncertainty)
    ) |> 
    # assign the countycode 
    dplyr::mutate(
      temp = countrycode(sourcevar = country, origin = "country.name", destination = "iso3c" ),
      iso3 = case_when(
        is.na(iso3) ~ temp,
        TRUE ~ iso3
      ))|> 
    select(-temp)
  
  # Export the localities that are not in the countries of interest 
  df2 <- df1 |>
    dplyr::filter(iso3 %in% c("USA","CAN","MEX",NA) | taxon %in% exemptTaxa)
  
  export1 <- df1[!df1$index %in% df2$index, ]
  
  # write_csv(x = export1, file = "data/processed_occurrence/excludeOnIso3.csv")
  
  # reassign longitude to negitive value based on country ISO3 
  ## this is turning up some weird results avoiding for now. 
  # df2a <- df2 |>
  #   filter(iso3 %in% c("USA","CAN","MEX"))|>
  #   mutate(longitude =  -1 * longitude)
  # 
  # df2[df2$index %in% df2a$index, "longitude"] <- df2a$longitude
  # 
  
  # Start gathering county specific datasets  -------------------------------
  df3 <- df2 |> 
    # Test the lat long values based on generalized bounding box. 
    dplyr::mutate(
      ## altering the lat lon filters to include of of the americas 
      validLat = case_when(
      #   latitude >= 14 ~ TRUE,
      #   latitude < 14 ~ FALSE
        is.na(latitude) ~ NA,
        TRUE ~ TRUE
        ),
      validLon = case_when(
        longitude <= -30 ~ TRUE,
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
    
  export2 <- df3 |> 
    dplyr::filter(validLatLon == FALSE)
  
  write_csv(x = export2, file = "data/processed_occurrence/excludedOnLatLonBoundingBox.csv")
  
  export3 <- df3 |> 
    filter(is.na(validLatLon))
  
  write_csv(x = export3, file = "data/processed_occurrence/considerNoLatLonProvided.csv")
  
  df4 <- df3 |> 
    filter(validLatLon == TRUE)
  
  return(list(validLatLon = df4,
              countycheck = bind_rows(export2, export3)))
}

