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
  
  # run level folder -------
  path <- c(path, 
             runLevel = paste0(path$rootDir, "/", runVersion)
  )
  
  # Second level folders ----------------------------------------------------
  path <- c(path, 
    # gap analysis 
    gap_analysis = paste0(path$runLevel,"/gap_analysis"),
    # occurances
    occurances = paste0(path$runLevel,"/occurances"),
    # results
    results = paste0(path$runLevel,"/results")
  )

  # three lever -- Results data ---------------------------------------------
  path <- c(path, 
  # natural area data  
  natAreaPath=paste0(path$results, "/naturalArea.gpkg"),
  # ga50 Path
  ga50Path=paste0(path$results, "/ga50.tif"),
  # cropped rasters 
  prepRasters = paste0(path$results, "/croppedRasters.tif"),
  # all SDM results 
  sdmResults = paste0(path$results, "/sdm_results.RDS"),
  # projected Model Rasters 
  modeledRasters =  paste0(path$results, "/modeledRasters.RDS"),
  # individual Run 
  individualRuns = paste0(path$results, "/individualRuns.tif"),
  # mean run 
  meanRun.tif = paste0(path$results, "/prj_mean.tif"),
  # median run
  medianRun = paste0(path$results, "/prj_median.tif"),
  # standard devation run
  stdevRun = paste0(path$results, "/prj_stdev.tif"),
  # additional AUC metrics 
  aucMetrics = paste0(path$results, "/aucMetrics.csv"),
  # evaluation table 
  evalTablePath=paste0(path$results,"/evaluationTable.csv"),
  # threshold raster
  thresPath=paste0(path$results,"/prj_threshold.tif"),
  # ga50 raster
  g50_bufferPath=paste0(path$results, "/ga50_masked.tif"),
  # data for summary data 
  summaryDataPath = paste0(path$results, "/",j,"_Summary.RDS"), 

  # third level -- occurrences -----------------------------------------------
  # counts data 
  countsPaths = paste0(path$occurances, "/counts.csv"),
  # spatial data 
  spatialDataPath = paste0(path$occurances, "/spatialData.gpkg"),
  # all model data 
  allDataPath=paste0(path$occurances, "/allmodelData.csv"),
  # varabile selection data 
  variablbeSelectPath=paste0(path$occurances, "/variableSelectionData.csv"),
  # third level -- gap_analysis --------------------------------------------
  #srsex  
  srsExPath = paste0(path$gap_analysis, "/srs_ex.csv"),
  #grsex
  grsexPath = paste0(path$gap_analysis,"/grs_ex.csv"),
  # ersex
  ersexPath = paste0(path$gap_analysis,"/ers_ex.csv"),
  ersexRast = paste0(path$gap_analysis,"/ers_ex_gaps.tif"),
  # fcsex
  fcsexPath = paste0(path$gap_analysis,"/fcs_ex.csv"),
  #srsin  
  srsinPath = paste0(path$gap_analysis, "/srs_in.csv"),
  #grsin
  grsinPath = paste0(path$gap_analysis,"/grs_in.csv"),
  # ersin
  ersinPath = paste0(path$gap_analysis,"/ers_in.csv"),
  ersinRast = paste0(path$gap_analysis,"/ers_in_gaps.tif"),
  # fcsin
  fcsinPath = paste0(path$gap_analysis,"/fcs_in.csv"),
  #fcsCombined 
  fcsCombinedPath = paste0(path$gap_analysis,"/fcs_combined.csv"),
  # summary html path
  summaryHTMLPath = paste0("data/Vitis/speciesSummaryHTML/",j,"_Summary_fnaFilter.html")
  )
  return(path)
}

  