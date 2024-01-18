###
# Stage 5 - run the model and generate prediction layers
# memory usage  -- ~20G 
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
source("~/Documents/cwr_wildgrapes/R2/modeling/cropG_buffer.R")

# multicore -- shared environment distributed processes -- so far slightly slower then sequential?  
# sequential  -- linear process
# multisession -- not very good do to the set up of multiple environments 
plan(multicore, workers = 5) # using multiplication to account for the variable cpu qualities 


# generate function that containerizes the specific calls 
## these function should always start with the taxon variable that that is what is being mapped over.
## only add parameters that are input datasets used by all features. otherwise, data calls are done inside the function. 
stage5 <- function(taxon, dir1, runVersion,overwrite){
  
  # define all paths object
  allPaths <- definePaths(dir1 = dir1,
                          j = taxon,
                          runVersion = runVersion)
  print(taxon)
  ## perform maxent model
  v_data <- readRDS(allPaths$variablbeSelectPath)
  rasterInputs <- terra::rast(allPaths$prepRasters)
  
  ### tabular data 
  ### wrapping in a try to attempt to push through all species... will want this condition inside the function. 
  sdm_results <- try(
    write_RDS(path = allPaths$sdmResults,
                           overwrite = overwrite, 
                           function1 = runMaxnet(selectVars = v_data,
                                                 rasterData = rasterInputs))
    )
  print(sdm_results)
  
  ## condition to test if model was suscessfull produced.
  if(class(sdm_results)[1]=="tbl_df"){
    ## raster objects
    projectsResults <- write_RDS(path = allPaths$modeledRasters,
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
    if(file.exists(allPaths$ga50Path)){
      g_buffer <- terra::rast(allPaths$ga50Path)
      g_bufferCrop <- write_Rast(path = allPaths$g50_bufferPath,
                                 overwrite = overwrite,
                                 function1 = cropG_Buffer(ga50 = g_buffer,
                                                          thres = thres))
    }
 }
}

# read in input files 

tictoc::tic()
# all the function in parallel 
speciesList1 <- speciesList[1]
furrr::future_map(.x = speciesList1 , .f = stage5,
                  dir1 = dir1, 
                  runVersion = runVersion,
                  overwrite = overwrite, 
                  .progress = TRUE, 
                  .options = furrr_options(seed = seed))
tictoc::toc()
# clean environment 
rm(list=ls())
gc()
