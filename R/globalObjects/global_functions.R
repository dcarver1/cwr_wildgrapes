###
# functions used in the declaration of global objects
# carverd@colostate.edu
# 20230308
###



#' processBioClim
#'
#' @param rasters path to a rds file of bioclim predictor variables 
#' @param names path to a csv file of bioclim predictor names 
#'
#' @return
#' 
processBioClim <-function(rasters,names){
  # names 
  r1 <- readRDS(rasters) %>%
    terra::unwrap()
  names(r1) <- names$shortName
  return(wrap(r1))
}