# bioNames <- "data/geospatial_datasets/bioclim_layers/variableNames.csv"
# bioVars <- "data/geospatial_datasets/bioclim_layers/bioclim_2.5arcsec_terra.RDS"

#' prepRasterData
#'
#' @param bioNames : path to variable names csv 
#' @param bioVars : path to bioclim raster layers 
#'
#' @return : list with template raster and names bioclim variables. 
prepRasterData <- function(bioNames, bioVars){
  bioNames <- read_csv(bioNames)
  bioVars <- readRDS(bioVars)
  names(bioVars) <- bioNames$shortName
  templateRast <- bioVars[[1]]
  return(list(bioVars = bioVars,
              templateRast = templateRast))
}