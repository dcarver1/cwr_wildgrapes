#' definePaths
#'
#' @param dir1 : genus level foundation 
#' @param j : species name that define the root feature for additional feautres
#'
#' @return named list of all paths for individual outputs 
definePaths <- function(dir1, j, runVersion){
  
  path <- list(
  # root level folder -------------------------------------------------------
    # root direct 
    rootDir = paste0(dir1,"/",j)
  )
  
  # Second level folders ----------------------------------------------------
  paths <- c(path, 
    # gap analysis 
    gap_analysis = paste0(path$rootDir,"/", runVersion ,"/gap_analysis"),
    # occurances
    occurances = paste0(path$rootDir,"/", runVersion ,"/occurances"),
    # results
    results = paste0(path$rootDir,"/", runVersion ,"/results")
  )

  # three lever -- Results data ---------------------------------------------
  path2 <- c(paths, 
  # natural area data  
  natAreaPath=paste0(paths$results, "/naturalArea.gpkg"),
  # ga50 Path
  ga50Path=paste0(paths$results, "/ga50.tif"),
  # cropped rasters 
  prepRasters = paste0(paths$results, "/croppedRasters.tif"),
  # all SDM results 
  sdmResults = paste0(paths$results, "/sdm_results.RDS"),
  # projected Model Rasters 
  modeledRasters =  paste0(paths$results, "/modeledRasters.RDS"),
  # individual Run 
  individualRuns = paste0(paths$results, "/individualRuns.tif"),
  # mean run 
  meanRun.tif = paste0(paths$results, "/prj_mean.tif"),
  # median run
  medianRun = paste0(paths$results, "/prj_median.tif"),
  # standard devation run
  stdevRun = paste0(paths$results, "/prj_stdev.tif"),
  # evaluation table 
  evalTablePath=paste0(paths$results,"/evaluationTable.csv"),
  # threshold raster
  thresPath=paste0(paths$results,"/prj_threshold.tif"),
  # ga50 raster
  g50_bufferPath=paste0(paths$results, "/ga50_masked.tif"),
  # data for summary data 
  summaryDataPath = paste0(paths$results, "/",j,"_Summary.RDS"), 
  # summary html path
  summaryHTMLPath = paste0(paths$results, "/",j,"_Summary.html"),
  # third level -- occurrences -----------------------------------------------
  # counts data 
  countsPaths = paste0(paths$occurances, "/counts.csv"),
  # spatial data 
  spatialDataPath = paste0(paths$occurances, "/spatialData.gpkg"),
  # all model data 
  allDataPath=paste0(paths$occurances, "/allmodelData.csv"),
  # varabile selection data 
  variablbeSelectPath=paste0(paths$occurances, "/variableSelectionData.csv"),
  # third level -- gap_analysis --------------------------------------------
  #srsex  
  srsExPath = paste0(paths$gap_analysis, "/srs_ex.csv"),
  #grsex
  grsexPath = paste0(paths$gap_analysis,"/grs_ex.csv"),
  # ersex
  ersexPath = paste0(paths$gap_analysis,"/ers_ex.csv"),
  # fcsex
  fcsexPath = paste0(paths$gap_analysis,"/fcs_ex.csv"),
  #srsin  
  srsinPath = paste0(paths$gap_analysis, "/srs_in.csv"),
  #grsin
  grsinPath = paste0(paths$gap_analysis,"/grs_in.csv"),
  # ersin
  ersinPath = paste0(paths$gap_analysis,"/ers_in.csv"),
  # fcsin
  fcsinPath = paste0(paths$gap_analysis,"/fcs_in.csv"),
  #fcsCombined 
  fcsCombinedPath = paste0(paths$gap_analysis,"/fcs_combined.csv")
  )
  return(path2)
}

  