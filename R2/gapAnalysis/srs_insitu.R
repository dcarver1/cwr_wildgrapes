

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
  
 ## if there is occurrence data but now model
  if(class(thres) != "SpatRaster"){
    totalObservations <- nrow(occuranceData)
    t1 <- terra::extract(x = protectedArea,y = vect(occuranceData))
  }else{
    mask1 <- ifel(test = thres == 1, yes = 1, no = NA)
    # determine the number of observations within the threshold
    to <- terra::extract(x = mask1, y = vect(occuranceData))
    totalObservations <- sum(to$Threshold, na.rm = TRUE)
    
    # crop protected areas raster 
    p1 <- terra::crop(x = protectedArea, y = mask1)
    # multiple to create mask -- protected areas within the threshold of the model 
    p1 <- p1 * mask1
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
