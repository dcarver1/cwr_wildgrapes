#' Crop Rasters 
#'
#' @param natArea 
#' @param bioVars 
#' @param selectVars 
#'
#' @return : a Clip and mask subset of raster that are used in the modeling 
#' process. Limited to the variables of interest. 
cropRasters <- function(natArea,bioVars,selectVars){
  vars <- selectVars$rankPredictors %>% 
    filter(includeInFinal == TRUE)%>%
    dplyr::select(varNames)%>%
    pull()
  # generate crop raster list 
  r1 <- bioVars[[names(bioVars) %in% vars]] %>%
    terra::crop(y = natArea)%>%
    mask(mask = natArea)
  
  return(r1)
} 
