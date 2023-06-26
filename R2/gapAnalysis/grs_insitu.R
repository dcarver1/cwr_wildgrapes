


#' Geographic Representativeness Score insitu 
#'
#' @param occuranceData 
#' @param protectedArea 
#' @param thres 
#'
#' @return dataframe with GRS data
grs_insitu <- function(occuranceData, protectedArea, thres){
  # mask protected areas layer 
  thres[thres == 0] <- NA
  # crop protected areas raster 
  p1 <- terra::crop(x = protectedArea, y = thres)
  # multiple to create mask 
  p1 <- p1 * thres
  
  protectArea <- terra::cellSize(p1,mask = TRUE,unit = "km" )
  protectAreaSum <- sum(values(protectArea), na.rm = TRUE)
  
  thresholdArea <- terra::cellSize(thres, mask = TRUE, unit = "km")
  thresholdAreaSum <- sum(values(thresholdArea), na.rm = TRUE)
  
  # calcualte the total area
  if(protectAreaSum == 0){
    protectAreaSum <- 0
    grs <- 0
  }else{
    grs <- min(c(100, protectAreaSum/thresholdAreaSum*100))
  }
  # return objects 
  df <- data.frame(ID = occuranceData$taxon[1],
                   SPP_AREA_km2 = thresholdAreaSum,
                   SPP_WITHIN_PA_AREA_km2 = protectAreaSum,
                   GRS = grs)
  return(df)
}
  