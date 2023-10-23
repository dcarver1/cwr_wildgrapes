#speciesList <- species
#directory <- "daucus"



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
generateSpeciesRichnessMap <- function(directory, runVersion){
  # plan(strategy = "multisession", workers = 12) ### have to be carefull with this because it copies all the session inform
  ### which is a lot of data to move around if this is part of the modeling environment. 
  # grab all potential options
    allFiles <- list.files( path = directory,
                pattern =  "prj_threshold.tif",
                full.names = TRUE,
                recursive = TRUE)
    # subset 
    runFiles <- allFiles[grepl(pattern = runVersion, x = allFiles)]
    
  # combined all the raster layers 
  print("reading in rasters")
  r1 <- map(.x = runFiles, .f = rast)
  
  # produce an extent raster template 
  print("generating maximum extent")
  r2 <- Reduce(extend, r1)
  
  # extend all the raster in the stack
  print("extenting all the objects. ")
  r3 <- map(.x = r1, .f = terra::extend, y = r2, fill = 0) ## still nan values being applied  
  
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
