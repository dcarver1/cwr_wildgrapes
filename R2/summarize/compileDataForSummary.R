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
  
  
  # some reprjecting issues are causing some visualization problems in the htmls
  # project all the raster objects to EPSG:3857
  # do not project the vector objects. 
  p1 <- terra::project(x = p1, y = "epsg:3857", method = "near")
  g_buffer <- terra::project(x = g_buffer, y = "epsg:3857", method = "near")
  g_bufferCrop <- terra::project(x = g_bufferCrop, y = "epsg:3857", method = "near")
  projectsResults <- projectsResults |> map(raster::projectRaster, crs = "epsg:3857", method = "ngb" )
  thres <- terra::project(x = thres, y = "epsg:3857", method = "near")
  
  # bind to export object 
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
