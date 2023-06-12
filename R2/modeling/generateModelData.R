#' Generate modeling dataset
#'
#' @param speciesPoints 
#' @param natArea 
#' @param bioVars 
#'
#' @return
#' @export
#'
#' @examples
generateModelData <- function(speciesPoints,natArea,bioVars,b_Number){
  
  # generate background points 
  ## format species data
  sp1 <- speciesPoints %>%
    mutate("presence" = 1)%>%
    dplyr::select(presence,geometry)
  
  
  ## this will need to be adjust if species if only present in a small area 
  bg1 <- sf::st_sample(x = natArea, size = b_Number)%>%
    sf::st_as_sf()%>%
    mutate("presence" = 0)%>%
    dplyr::select(presence,"geometry" = x)%>%
    filter(!geometry %in% sp1$geometry) # test for same coordinated between presence and background data
  # bind datasets 
  d1 <- bind_rows(sp1, bg1)
  
  # extract values from rasters 
  ## I don't know if I need this a spatial object at this point, I don't thin so
  ## Also this are no reference values for what points aligns with which input location. 
  d2 <- terra::extract(x = bioVars, y = vect(d1), bind= TRUE)%>%
    st_as_sf()
  
  # convert to sf and drop NA values
  drop <- st_drop_geometry(d2)%>%
    complete.cases()
  ### might be some issues here with droping observations, but this step is a requirement. 
  d3 <- d2[drop, ]
  return(d3) 
}
