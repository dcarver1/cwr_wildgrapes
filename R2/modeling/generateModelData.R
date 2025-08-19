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
  
  # split into categories 
  sg <- speciesPoints[speciesPoints$type == "G", ]
  sh <- speciesPoints[speciesPoints$type == "H", ]
  
  ## thin data if needed 
  if(nrow(sh) > 50){
    # thin to 5km 
    thin1 <- spThin::thin(loc.data = sh,
                          lat.col = "latitude",
                          long.col = "longitude",
                          spec.col = "taxon",
                          thin.par = 5,
                          reps = 5,
                          locs.thinned.list.return = TRUE, 
                          write.files = FALSE, 
                          write.log.file = FALSE)[[1]] |> row.names()
    shThin <- sh[as.numeric(thin1), ]
    # generate combined features 
    sp1 <- bind_rows(sg, shThin)
  }else{
    # recombined data. Will run with any total data at this point. 
    sp1 <- speciesPoints
  }
  
  ## format species data
  sp1 <- speciesPoints |>
    mutate("presence" = 1)|>
    dplyr::select(presence, type)
  # dplyr::select(presence,geometry
  
  
  
  
  
  
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
  # drop any na values 
  drop2 <- st_drop_geometry(d2)|>
    complete.cases()
  d2 <-d2[drop2, ]
  
  # extract values to the point data 
  d3 <- terra::extract(x = bioVars,
                       y = vect(sp1),
                       bind= TRUE)|>
    st_as_sf()
  # 
  drop3 <- st_drop_geometry(d3)|>
    complete.cases()
  d3 <-d3[drop3, ]
  # bind datasets 
  d1 <- bind_rows(d3, d2)
  
  return(d1) 
}
