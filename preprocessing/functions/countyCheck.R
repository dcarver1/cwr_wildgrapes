# countyCheckData <- c1

checkCounties <- function(countyCheckData, states, counties){
  sf_use_s2(FALSE)
  #names No geom
  names2 <- names(countyCheckData)[1:27]

  # filter to locations of interest
  states1 <- states |> 
    filter(adm0_a3 %in% c("USA","CAN", "MEX"))|>
    st_make_valid()
  # grab state names from state files 
  stateNames <- states1 |>
    st_drop_geometry() |>
    filter(adm0_a3 == "USA") |>
    select(name) |>
    pull()
  
  # various test to see if addition county level observations can be derived 
  
  
  # organize the county column 
  newCounty <- stringr::str_remove_all(string = countyCheckData$county,pattern = " .Co") 
  newCounty <- stringr::str_remove_all(string = newCounty,pattern = " Co.") 
    
  countyCheckData$county <- newCounty
  countyCheckData1 <- countyCheckData|>
    dplyr::mutate(county = case_when(
      grepl("County", county) ~ county, 
      is.na(county) ~ NA,
      TRUE ~ paste0(county," County")
    ))
  
  ## without lat lon values 
  noLatLon <- countyCheckData1 |>
    filter(is.na(latitude) | is.na(longitude)) 
  ### lat is NA or lon is NA and county is NA : exclude 
  exclude1 <- noLatLon |> 
    filter(is.na(county)) |>
    select(names2) |>
    st_drop_geometry()
  
  ### non excluded features 
  noLatLon2 <- noLatLon |>
    filter(!index %in% exclude1$index)
  # remove features without a state reference 
  ## county reference but no lat lon or state reference 
  exclude2 <- noLatLon2 |> 
    filter(is.na(state))|>
    select(names2) |>
    st_drop_geometry()
  
  
  # county and State reference included
  addToCounty1 <- noLatLon2 |>
    filter(!is.na(state) & !is.na(county)) |>
    select(names2)
  
  ## exclude any localities that are not from US states
  exclude3 <- addToCounty1 |>
    filter(!state %in% stateNames)|>
    select(names2) |>
    st_drop_geometry()
  
  # with Lat Lon Values 
  withLatLon <- countyCheckData1 |>
    filter(!is.na(latitude) & !is.na(longitude)) %>%
    st_as_sf(coords = c("longitude","latitude"),crs = st_crs(4326),remove = FALSE)
  ### extract county
  withLatLon2 <- st_intersection(withLatLon, states) |>
    select(names(withLatLon), name, adm0_a3 ) 
  
  # exclude with lat lon is outside of USA
  exclude4 <- withLatLon2 |> 
    filter(adm0_a3 != "USA")|>
    select(names2) |>
    st_drop_geometry()
  
  # Next filter of lat lon values 
  ## none of these listed counties match with the county grab from lat long. 
  ## perform some string edits to remove special characters and 
  withLatLon3 <- withLatLon2 |> 
    filter(!index %in% exclude4$index)%>%
    select(-name, -adm0_a3)
  
  countyNames <- withLatLon3$county %>%
    str_remove_all(pattern = " County")%>%
    str_remove_all(pattern = " Co.") %>%
    str_to_title() %>%
    str_replace_all(pattern = "  ", replacement = " ") |>
    str_replace_all(pattern = "St.", replacement = "Saint")
  
  withLatLon3$county <- countyNames
  
  ## interest with county layer and recheck the relationship 
  withLatLon4 <- st_intersection(withLatLon3, counties) |>
    select(names(withLatLon3), NAME) %>%
    mutate(countyMatch = case_when(
      county == NAME ~ TRUE,
      county != NAME ~ FALSE
    ))
  # if the counties match and the iso3 is USA we will keep, else exclude. 
  exclude5 <- withLatLon4 |> 
    select(-NAME) |> 
    filter(countyMatch == FALSE | iso3 != "USA") |>
    select(names2) |>
    st_drop_geometry()
  # county match 
  addToCounty2 <- withLatLon4 |>
    filter(!index %in% exclude5$index) |>
    st_drop_geometry() |> 
    select(names2)
  
  # all excluded data 
  exclude <- bind_rows(exclude1, exclude2, exclude3, exclude4, exclude5)
  
  # all additional reference data
  include <- bind_rows(addToCounty1, addToCounty2) |>
    dplyr::mutate(county = str_replace_all(county, pattern = "nty ", replacement = " "))|>
    dplyr::mutate(county = str_to_title(county))
  ### the nrow of these two features are > then the Nrow of the input feature, 
  ### so something is going duplicated. I'm rolling with it for now. 
  
  # return features 
  return(list(exclude = exclude,
              include = include))
}









