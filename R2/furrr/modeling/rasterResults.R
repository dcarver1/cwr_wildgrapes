#' Generate Results Rasters
#'
#' @param sdm_result 
#'
#' @return
#' @export
#'
#' @examples
#' 
rasterResults <- function(sdm_result){
  #Convert back to Terra objects. 
  prj_stk <- sdm_results %>% 
    dplyr::select(do.projections) %>%
    unlist() %>% 
    raster::stack()%>%
    rast()
  
  # list of modeled outputs 
  rasters <- list(
    all = raster(prj_stk),
    mean = raster(terra::mean(prj_stk)),
    median = raster(median(prj_stk)),
    stdev = raster(terra::stdev(prj_stk))
  )
  return(rasters)
}
