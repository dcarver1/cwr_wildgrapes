#speciesList <- species
#directory <- "daucus"




#' Rescale a raster based on a template feature 
#'
#' @return
#' @export
#'
#' @examples
resampleRast <- function(rast, resampleFactor = 5, fun = "mean"){
  thresAgg <- matrix(c(
    -Inf, 0.5, 0,  # All values from -Infinity up to (but not including) 0.5 become 0
    0.5,  Inf, 1   # All values from 0.5 (including 0.5) up to +Infinity become 1
  ), ncol = 3, byrow = TRUE)
  
  
  r1 <- rast |> 
    terra::aggregate(fact = resampleFactor, fun = fun) |>
    terra::classify(thresAgg, include.lowest = TRUE)
  return(r1)
}



#' replace NAN 
#' @description Helper funtion to replace all nan values within a raster. 
#' @param raster 
#'
#' @return
#' @export
#'
#' @examples
replaceNAN <- function(raster){
  raster[is.nan(raster)] <- 0 
  return(raster)
}

#' Generate species richness map
#'
#' @param directory : the top level directy from which you want to look for files
#' @param runVersion : the specific run version that should be used for the analysis
#'
#' @return : a 
generateSpeciesRichnessMap <- function(directory, runVersion, rasterFileName){
  # plan(strategy = "multisession", workers = 12) ### have to be carefull with this because it copies all the session inform
  ### which is a lot of data to move around if this is part of the modeling environment. 
  # grab all potential options
    allFiles <- list.files( path = directory,
                pattern =  rasterFileName,
                full.names = TRUE,
                recursive = TRUE)
    # subset 
    runFiles <- allFiles[grepl(pattern = runVersion, x = allFiles)]
    
  # combined all the raster layers 
  print("reading in rasters")
  r1 <- map(.x = runFiles, .f = terra::rast)
  
  # produce an extent raster template 
  print("generating maximum extent")
  r2 <- Reduce(terra::extend, r1)
  
  # extend all the raster in the stack
  print("extenting all the objects. ")
  r3 <- map(.x = r1, .f = terra::resample, y = r2, method = 'near') ## still nan values being applied  
  
  # replace the NaN values 
  r4 <- map(.x = r3, .f = replaceNAN)
  
  
  # sum all the features 
  print("bind to single object")
  r5 <- Reduce("+", r4)
  
  output <- list(
    richnessTif = r5,
    speciesUsed = runFiles
  )
  return(output)
}

generateERSRichnessMap <- function(directory, runVersion, ersMap, species, thresholdMap){
  # pull all the ERS gap maps 
  ersFiles <- list.files( path = directory,
                          pattern =  ersMap,
                          full.names = TRUE,
                          recursive = TRUE)
  # subset 
  ersFiles <- ersFiles[grepl(pattern = runVersion, x = ersFiles)]
  
  # pull all the threshold files 
  thresholdFiles <- list.files( path = directory,
                          pattern =  thresholdMap,
                          full.names = TRUE,
                          recursive = TRUE)
  thresholdFiles <- thresholdFiles[grepl(pattern = runVersion, x = thresholdFiles)]
  
  # loop over species and mask the ERSlayer to the distribution 
  maskedFeatures <- c()
  for(i in species){
    f1 <- ersFiles[grepl(pattern = i, x = ersFiles)]
    if(length(f1)==1){
      t1 <-  terra::rast(f1)
      t2 <- NA 
      try(t2 <-  terra::rast(thresholdFiles[grepl(pattern = i, x = thresholdFiles)]))
      if(class(t2)== "SpatRaster"){
        t3 <- t1 * t2 
        maskedFeatures <- c(maskedFeatures, t3)
      }
    }
  }
  # 
  # 
  # # combined all the raster layers 
  # print("reading in rasters")
  # r1 <- map(.x = runFiles, .f = rast)
  
  # produce an extent raster template 
  print("generating maximum extent")
  r2 <- Reduce(extend, maskedFeatures)
  
  # extend all the raster in the stack
  print("extenting all the objects. ")
  r3 <- map(.x = maskedFeatures, .f = terra::resample, y = r2, method = 'near') ## still nan values being applied  

  # replace the NaN values 
  r4 <- map(.x = r3, .f = replaceNAN)
  
  
  # sum all the features 
  print("bind to single object")
  r5 <- Reduce("+", r4)
  
  output <- list(
    richnessTif = r5,
    speciesUsed = ersFiles
  )
  return(output)
}


