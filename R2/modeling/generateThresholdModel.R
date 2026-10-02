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
  # a median of exactly 0 falls outside both (0, thr] and (thr, 1] above and
  # becomes NA; this form gives it 0. Tested 2026-09-17 on six species: no
  # score changes (work2026/experimentSummary_20260917.csv, exp_thresh).
  rast1 <- terra::ifel(rasterResults$median > threshold, 1, 0)
  names(rast1) <- "Threshold"
  
  return(rast1)
}
