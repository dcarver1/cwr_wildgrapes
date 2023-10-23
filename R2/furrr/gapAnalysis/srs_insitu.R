

#' Species representativeness score insitu 
#'
#' @param occuranceData 
#' @param thres 
#' @param protectedArea 
#'
#' @return dataframe of results 
srs_insitu <- function(occuranceData, thres,protectedArea){
  ## modeled points 
  ## threshold model 
  
  totalObservations <- nrow(occuranceData)
  ## if there is occurrance data but now modele
  if(class(thres) != "SpatRaster"){
    t1 <- terra::extract(x = protectedArea,y = vect(occuranceData))
  }else{
    thres[thres == 0] <- NA
    # crop protected areas raster 
    p1 <- terra::crop(x = protectedArea, y = thres)
    # multiple to create mask 
    p1 <- p1 * thres
    # extract values from crop 
    t1 <- terra::extract(x = p1,y = vect(occuranceData))
  }
  # extracted values are 1 or NA so sum all the values to get the total. 
  totalInProtectArea <- sum(t1$layer, na.rm = TRUE)
  
  #define SRS
  if(totalInProtectArea >= 0 ){
    srsInsitu <- 100 *(totalInProtectArea/totalObservations)
  }else{
    srsInsitu <- 0
  }
  
  #create data.frame with output
  out_df <- data.frame(ID=occuranceData$taxon[1],
                       NTOTAL=totalObservations,
                       ProTotal = totalInProtectArea,
                       SRS=srsInsitu)
  return(out_df)
  
}
