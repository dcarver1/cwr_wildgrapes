#' Generate the threshold binary raster 
#'
#' @param evalTable 
#' @param rasterResults 
#'
#' @return
generateThresholdModel <- function(evalTable, rasterResults){
  #calculate threshold values form median model results 
  threshold <-mean(evalTable$threshold_train, na.rm = TRUE)
  
  #create classification matrix 
  m <- c(0, threshold, 0,
         threshold, 1, 1)
  rclmat <- matrix(m, ncol=3, byrow=TRUE)
  
  # generate binary raster based on threshold value 
  ## The right side value of the matrix is include. all values > threshold are 1.  
  rast1 <- terra::classify(rasterResults$median, rcl = rclmat, right = TRUE)
  names(rast1) <- "Threshold"
  
  return(rast1)
}