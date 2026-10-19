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
  
  # extend, replace NaN and sum, one raster at a time
  r5 <- sumRastersOnCommonGrid(r1)
  
  output <- list(
    richnessTif = r5,
    speciesUsed = runFiles
  )
  return(output)
}

#' Sum rasters of different extents on their common grid
#' @description Same result as extending every raster to the combined extent,
#' replacing NaN with 0 and adding them up, but one raster is processed at a
#' time. Holding every extended raster in memory at once (about 40 grids of
#' the full study area, twice) used tens of GB and took the VM down on
#' 2026-10-05.
#' @param rasters : list of SpatRaster objects
#' @return SpatRaster of the cell-wise sum over the combined extent
sumRastersOnCommonGrid <- function(rasters){
  # the combined extent, on the grid of the first raster (geometry only)
  print("generating maximum extent")
  template <- terra::rast(rasters[[1]])
  for(r in rasters[-1]){
    template <- terra::extend(template, r)
  }
  print("extending and summing the objects")
  total <- NULL
  for(r in rasters){
    x <- terra::resample(r, template, method = 'near') |>
      terra::classify(cbind(NA, 0))
    total <- if(is.null(total)) x else total + x
    # R does not see the size of the rasters terra holds, so it does not free
    # the ones replaced above on its own; without this they pile up
    rm(x); invisible(gc())
  }
  return(total)
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
  
  # extend, replace NaN and sum, one raster at a time
  r5 <- sumRastersOnCommonGrid(maskedFeatures)
  
  output <- list(
    richnessTif = r5,
    speciesUsed = ersFiles
  )
  return(output)
}


