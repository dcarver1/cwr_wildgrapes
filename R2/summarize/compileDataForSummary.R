#' Grab Data for the summary html documents 
#'
#' @return RDS file with a nested list of all required file inputs 
grabData <- function(ersex, fscCombined, fcsex, fcsin,evalTable,g_bufferCrop,thres, 
                     projectsResults, v_data, g_buffer,natArea,protectedAreas,
                     occuranceData, countsData){
  # crop protected areas 
  # mask protected areas layer 
  p2 <- thres
  p2[p2 == 0] <- NA
  # crop protected areas raster 
  p1 <- terra::crop(x = protectedAreas, y = p2)
  # multiple to create mask 
  p1 <- p1 * p2
  
  
  
  
  
  reportData <- list(
    occuranceData = occuranceData,
    naturalArea = natArea,
    model_Occurances = v_data,
    protectedArea = p1,
    g_buffer = g_buffer,
    g_bufferCrop = g_bufferCrop,
    projectedResults = projectsResults,
    binaryMap = thres,
    modelEvaluation = evalTable,
    ersex = ersex,
    fcsCombined = fcsCombined,
    fcsex= fcsex,
    fcsin = fcsin,
    countsData = countsData
  ) 
  return(reportData)
}
