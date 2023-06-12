writeData <- function(overwrite, 
                      dirs, 
                      c1, 
                      sp1,
                      srsex,
                      natArea,
                      g_buffer,
                      rasterResults,
                      evalTable,
                      thres){
  countsPath <- paste0(dirs[2], "/counts.csv")
  if(!file.exists(countsPath)){
    write_csv(x = c1, file = countsPath)
  }
  ### sp object 
  spPath <- paste0(dirs[2], "/spatialData.csv")
  if(!file.exists(spPath)){
    write_csv(x = sp1, file = spPath)
  }
  ### srsex 
  srsExPath <- paste0(dirs[1], "/srs_ex.csv")
  if(!file.exists(srsExPath)){
    write_csv(x = srsex, file = srsExPath)
  }
  ### natural area 
  natAreaPath <- paste0(dirs[3], "/naturalArea.gpkg")
  if(!file.exists(natAreaPath)){
    sf::write_sf(obj = natArea, dsn = natAreaPath)
  }
  
  ### natural area 
  ga50Path <- paste0(dirs[3], "/ga50.tif")
  if(!file.exists(ga50Path) & class(g_buffer) == "SpatRaster" ){
    terra::writeRaster(x = g_buffer, file = ga50Path)
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
  if(class(evalTable)=="data.frame"){
    write_csv(x = evalTable, file = paste0(dirs[3],"/evaluationTable.csv"))
  }
  ## export threshold model 
  if(class(thres)=="SpatRaster"){
    writeRaster(x = thres, 
                filename = paste0(dirs[3],"/prj_threshold.tif"),
                overwrite = TRUE)
  }
  
  
}
