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
    all = prj_stk,
    mean = mean(prj_stk),
    median = median(prj_stk),
    stdev = terra::stdev(prj_stk)
  )
  return(rasters)
}
