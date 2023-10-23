 
#' Geographic Representation Score Exsitu
#'
#' @param speciesData 
#' @param ga50 
#' @param thres 
#'
#' @return
#' @export
#'
#' @examples
grs_exsitu <- function(speciesData, ga50, thres) {
  ## all the areas of the cells 
  r1 <- cellSize(thres,unit="km")
  ## mutliple by origin. values of 1 will retain area measures
  r2 <- r1 * thres
  totalArea <- sum(values(r2), na.rm = TRUE)
  
  # clause to see if any g points exist
  if(class(ga50)[[1]] != "SpatRaster"){ # not sure if this is the best condition 
    grs <- 0
    gArea <- 0
  }else{
    ## repeat the process for the ga50 raster 
    c2 <- r1 * ga50
    gArea <- sum(values(c2), na.rm = TRUE)
    
    # clause to determine if any of the buffered area falls within predicted area
    if(gArea == 0){
      grs <- 0
      gArea <- 0
    }else{
      #calculate GRS
      grs <- min(c(100, gArea/totalArea*100))
    }
  }

  #create data.frame with output
  out_df <- data.frame(ID=speciesData$taxon[1],
                       SPP_AREA_km2=totalArea, 
                       G_AREA_km2=gArea,
                       GRS=grs)
  return(out_df)
}
