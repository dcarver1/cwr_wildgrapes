###
# Stage 5 - run the model and generate prediction layers
# 
###

#define libraries used 
pacman::p_load(future,furrr, sf, dplyr, terra)

# source Global parameters 
source("~/Documents/cwr_wildgrapes/distributiveModel/globalParameters.R")

# source specific functions for this workflow
source("~/Documents/cwr_wildgrapes/R2/modeling/runMaxnet.R")
source("~/Documents/cwr_wildgrapes/R2/modeling/rasterResults.R")
source("~/Documents/cwr_wildgrapes/R2/modeling/evaluateTable.R")
source("~/Documents/cwr_wildgrapes/R2/modeling/generateThresholdModel.R")
source("~/Documents/cwr_wildgrapes/R2/modeling/cropG_Buffer.R")

# multicore -- shared environment distributed processes -- so far slightly slower then sequential?  
# sequential  -- linear process
# multisession -- not very good do to the set up of multiple environments 
plan(multicore, workers = round(cores * 2/5)) # using multiplication to account for the variable cpu qualities 


# generate function that containerizes the specific calls 
## these function should allways start with the taxon variable that that is what is being mapped over.
stage4 <- function(taxon, dir1, runVersion,overwrite,
                   bioVars){
  
  # define all paths object
  allPaths <- definePaths(dir1 = dir1,
                          j = taxon,
                          runVersion = runVersion)
  
  ## perform maxent model 
  ### tabular data 
  sdm_results <- write_RDS(path = allPaths$sdmResults,
                           overwrite = overwrite, 
                           function1 = runMaxnet(selectVars = v_data,
                                                 rasterData = rasterInputs))
  
  ## condition to test if model was suscessfull produced.
  if(!is.null(sdm_results)){
    ## raster objects 
    projectsResults <- write_RDS(path = allPaths$modeledRasters,
                                 overwrite = overwrite,
                                 function1 = rasterResults(sdm_result))
    
    ## generate evaluationTable 
    evalTable <- write_CSV(path = allPaths$evalTablePath,
                           overwrite = overwrite, 
                           function1 = evaluateTable(sdm_result = sdm_results))
    
    ## generate threshold rasters 
    thres <- write_Rast(path =  allPaths$thresPath,
                        overwrite = overwrite,
                        function1 = generateThresholdModel(evalTable = evalTable,
                                                           rasterResults = projectsResults))
    
    ## generate a mess map 
    ## generate a kernal density map 
    
    ## crop GA50 to threshold area 
    g_bufferCrop <- write_Rast(path = allPaths$g50_bufferPath, 
                               overwrite = overwrite,
                               function1 = cropG_Buffer(ga50 = g_buffer,
                                                        thres = thres))
      
    
  }
}

# read in input files 
bioVars <- read_rds("~/Documents/cwr_wildgrapes/distributiveModel/data/bioClim.rds")|>
  terra::unwrap()
print("read in data")

tictoc::tic()
# all the function in parallel 
furrr::future_map(.x = speciesList, .f = stage4,
                  dir1 = dir1, 
                  runVersion = runVersion,
                  overwrite = overwrite, 
                  bioVars = bioVars,
                  .progress = TRUE, 
                  .options = furrr_options(seed = seed))
tictoc::toc()
# clean environment 
rm(list=ls())
