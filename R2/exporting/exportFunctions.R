writeData <- function(overwrite, 
                      dirs, 
                      c1, 
                      sp1,
                      srsex,
                      natArea,
                      g_buffer,
                      rasterResults,
                      evalTable,
                      thres,
                      g_bufferMask,
                      grsex,
                      ersex,
                      fcsex){
  ## counts data
  countsPath <- paste0(dirs[2], "/counts.csv")
  if(!file.exists(countsPath) | isTRUE(overwrite)){
    write_csv(x = c1, file = countsPath)
  }
  
  ### sp object 
  spPath <- paste0(dirs[2], "/spatialData.csv")
  if(!file.exists(spPath)| isTRUE(overwrite)){
    write_csv(x = sp1, file = spPath)
  }
  
  ### srsex 
  srsExPath <- paste0(dirs[1], "/srs_ex.csv")
  if(!file.exists(srsExPath)| isTRUE(overwrite)){
    write_csv(x = srsex, file = srsExPath)
  }
  
  ### natural area 
  natAreaPath <- paste0(dirs[3], "/naturalArea.gpkg")
  if(!file.exists(natAreaPath)| isTRUE(overwrite)){
    sf::write_sf(obj = natArea, dsn = natAreaPath)
  }
  
  ### ga50 buffer object  
  ga50Path <- paste0(dirs[3], "/ga50.tif")
  if(!file.exists(ga50Path)| isTRUE(overwrite) & class(g_buffer) == "SpatRaster" ){
    terra::writeRaster(x = g_buffer, 
                       file = ga50Path, 
                       overwrite = TRUE)
  }
  
  ## export model rasters 
  if(class(rasterResults$all)[1] == "SpatRaster" ){
    names <- c("individualRuns.tif", "prj_mean.tif", "prj_median.tif",
               "prj_stdev.tif")
    for(feat in seq_along(names)){
      writeRaster(x = rasterResults[feat][[1]],
                  filename = paste0(dirs[3], "/", names[feat]),
                  overwrite = TRUE)
    }
  }
  
  ## export evaluation table 
  evalTablePath <- paste0(dirs[3],"/evaluationTable.csv")
  if(!file.exists(evalTablePath) | isTRUE(overwrite)){
    write_csv(x = evalTable, file = evalTablePath)
  }
  ## export threshold model 
  thresPath <- paste0(dirs[3],"/prj_threshold.tif")
  if(!file.exists(evalTablePath) | isTRUE(overwrite) & class(thres)=="SpatRaster"){
    writeRaster(x = thres, 
                filename = paste0(dirs[3],"/prj_threshold.tif"),
                overwrite = TRUE)
  }
  
  ## g50_buffer 
  g50_bufferPath <- paste0(dirs[3], "/ga50_masked.tif")
  if(!file.exists(g50_bufferPath)| isTRUE(overwrite) & class(g_bufferMask) == "SpatRaster" ){
    terra::writeRaster(x = g_bufferMask, 
                       file = g50_bufferPath,
                       overwrite = TRUE)
  }
  ## GRSex 
  grsexPath <- paste0(dirs[3],"/grs_ex.csv")
  if(!file.exists(grsexPath) | isTRUE(overwrite)){
    write_csv(x = grsex, file = grsexPath)
  }
  
  
  ## ERSex 
  ersexPath <- paste0(dirs[3],"/ers_ex.csv")
  if(!file.exists(ersexPath) | isTRUE(overwrite)){
    write_csv(x = ersex, file = ersexPath)
  }
  
  ## FCSex
  fcsexPath <- paste0(dirs[3],"/fcs_ex.csv")
  if(!file.exists(fcsexPath) | isTRUE(overwrite)){
    write_csv(x = fcsex, file = fcsexPath)
  }
  
}
