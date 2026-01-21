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
  prj_stk <- sdm_result %>% 
    dplyr::select(do.projections) %>%
    unlist()   %>% 
    raster::stack()  %>%
    rast()
  
  # list of modeled outputs 
  rasters <- list(
    all = prj_stk,
    mean = terra::mean(prj_stk),
    median = terra::median(prj_stk),
    stdev = terra::stdev(prj_stk)
  )
  
  # wrapping the raster to be able to store in RDS 
  wrapped_list <- lapply(rasters, terra::wrap)
  return(wrapped_list)
}
