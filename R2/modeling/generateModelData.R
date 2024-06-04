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
  sp1 <- speciesPoints |>
    mutate("presence" = 1)|>
    dplyr::select(presence, type)
    # dplyr::select(presence,geometry)
  
  
  ## this will need to be adjust if species if only present in a small area 
  bg1 <- sf::st_sample(x = natArea, size = b_Number)|>
    sf::st_as_sf()|>
    mutate("presence" = 0)|>
    dplyr::select(presence,"geometry" = x)|>
    filter(!geometry %in% sp1$geometry) # test for same coordinated between presence and background data
  
  # extract values from rasters 
  ## I don't know if I need this a spatial object at this point, I don't thin so
  ## Also this are no reference values for what points aligns with which input location. 
  d2 <- terra::extract(x = bioVars,
                       y = vect(bg1),
                       bind= TRUE)|>
    st_as_sf()
  drop2 <- st_drop_geometry(d2)|>
    complete.cases()
  d2 <-d2[drop2, ]
  
  
  d3 <- terra::extract(x = bioVars,
                       y = vect(sp1),
                       bind= TRUE)|>
    st_as_sf()
  drop3 <- st_drop_geometry(d3)|>
    complete.cases()
  d3 <-d3[drop3, ]
  # bind datasets 
  d1 <- bind_rows(d3, d2)
  
    return(d1) 
}
