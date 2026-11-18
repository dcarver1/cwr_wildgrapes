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

#' Taxa that enter the genus-level (all-taxa) summaries
#' @description A species that is also assessed through its varieties (V. aestivalis and
#' V. cinerea: the species row pools the records of its varieties) is left out of every
#' product summed over taxa: the richness and gap-richness maps, the protected-area tables
#' built from them and the optimal protected-area selection. The varieties stand for it, so
#' no area is counted twice. The per-taxon result of the species itself is unchanged.
#' Adopted 2026-11-18 (team decision on the Correction); the published run stacked the
#' pooled species next to their varieties (38 layers in the richness map, not 36).
#' @param taxa : character, taxon names as the data/<genus>/<taxon> folders are named
#' @return the taxa whose name is also the stem of a "<name> var. ..." taxon in the set
speciesWithInfraspecifics <- function(taxa){
  taxa <- unique(taxa)
  taxa[vapply(taxa, function(x) any(startsWith(setdiff(taxa, x), paste0(x, " var."))), logical(1))]
}

#' @describeIn speciesWithInfraspecifics the set without those species
genusLevelTaxa <- function(taxa){
  setdiff(unique(taxa), speciesWithInfraspecifics(taxa))
}

#' Taxon folder of a run file
#' @param files : paths of the form <directory>/<taxon>/<runVersion>/...
#' @param directory : the top level directory the taxon folders sit in
taxonFromPath <- function(files, directory){
  rel <- sub(paste0("^", directory, "/+"), "", files)
  vapply(strsplit(rel, "/", fixed = TRUE), `[`, character(1), 1)
}

#' Generate species richness map
#'
#' @param directory : the top level directy from which you want to look for files
#' @param runVersion : the specific run version that should be used for the analysis
#' @param rasterFileName : the per-taxon raster to stack (prj_threshold.tif, ga50_masked.tif)
#' @param species : optional, restrict to these taxa; the species with infraspecifics are
#' dropped in any case (see speciesWithInfraspecifics)
#'
#' @return : list(richnessTif = cell-wise sum, speciesUsed = the files stacked)
generateSpeciesRichnessMap <- function(directory, runVersion, rasterFileName, species = NULL){
  # plan(strategy = "multisession", workers = 12) ### have to be carefull with this because it copies all the session inform
  ### which is a lot of data to move around if this is part of the modeling environment. 
  # grab all potential options
    allFiles <- list.files( path = directory,
                pattern =  rasterFileName,
                full.names = TRUE,
                recursive = TRUE)
    # subset 
    runFiles <- allFiles[grepl(pattern = runVersion, x = allFiles)]
    # genus-level product: one layer per taxon, the pooled species left out
    taxa <- taxonFromPath(runFiles, directory)
    keep <- taxa %in% genusLevelTaxa(if (is.null(species)) taxa else species)
    if (any(!keep)) print(paste("left out of the genus-level sum:", paste(taxa[!keep], collapse = ", ")))
    runFiles <- runFiles[keep]
    
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

#' Generate an ERS gap richness map
#' @description Sum over taxa of (ERS gap map x predicted distribution). Genus-level product:
#' the species with infraspecifics are left out (see speciesWithInfraspecifics). Until
#' 2026-11-18 the files were matched by grepl() on the taxon name, which found three files for
#' "Vitis aestivalis" or "Vitis cinerea" and so skipped them silently; the species list that was
#' written beside the map named them anyway. They are now left out explicitly and the list
#' holds the files actually summed.
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
  ersTaxa <- taxonFromPath(ersFiles, directory)
  thresholdTaxa <- taxonFromPath(thresholdFiles, directory)
  
  # loop over the genus-level taxa and mask the ERS layer to the distribution 
  maskedFeatures <- c()
  used <- c()
  for(i in genusLevelTaxa(species)){
    f1 <- ersFiles[ersTaxa == i]
    f2 <- thresholdFiles[thresholdTaxa == i]
    if(length(f1) == 1 && length(f2) == 1){
      t3 <- terra::rast(f1) * terra::rast(f2)
      maskedFeatures <- c(maskedFeatures, t3)
      used <- c(used, f1)
    }
  }
  print(paste(length(used), "taxa in the", ersMap, "richness map"))
  
  # extend, replace NaN and sum, one raster at a time
  r5 <- sumRastersOnCommonGrid(maskedFeatures)
  
  output <- list(
    richnessTif = r5,
    speciesUsed = used
  )
  return(output)
}


