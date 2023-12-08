###
# Stage 2 - Genererate Observational Datasets
# 
###

#define libraries used 
pacman::p_load(future,furrr, sf, dplyr)

# source Global parameters 
source("~/Documents/cwr_wildgrapes/distributiveModel/globalParameters.R")

# source specific functions for this workflow
source("~/Documents/cwr_wildgrapes/R2/dataProcessing/data_processing_functions.R")
source("~/Documents/cwr_wildgrapes/R2/gapAnalysis/srs_exsitu.R")

# multicore -- shared environment distributed processesd 
# sequential  -- linear process
# multisession -- not very good do to the set up of multiple environments 
plan(multicore, workers = round(cores * 3/4)) # using multiplication to account for the variable cpu qualities 

# generate function that containerizes the specific calls 
## these function should allways start with the taxon variable that that is what is being mapped over.
stage2 <- function(taxon, dir1, runVersion,overwrite, 
                   speciesData){
  # define all paths object
  allPaths <- definePaths(dir1 = dir1,
                          j = taxon,
                          runVersion = runVersion)
  
  # subset species 
  sd1 <- subsetSpecies(occuranceData =speciesData, species = taxon)
  
  # # generate counts 
  c1 <- write_CSV(path = allPaths$countsPaths,
                  overwrite = overwrite,
                  function1 = generateCounts(speciesData = sd1))
  # ## spatial object
  sp1 <- write_GPKG(path = allPaths$spatialDataPath,
                     overwrite = overwrite,
                     function1 = createSF_Objects(speciesData = sd1))
  # #srsex
  srsex <- write_CSV(path = allPaths$srsExPath,
                     overwrite = overwrite,
                     function1 = srs_exsitu(sp_counts = c1))

}

# read in input files 
speciesData <- read_rds("~/Documents/cwr_wildgrapes/distributiveModel/data/speciesData.rds")

tictoc::tic()
# all the function in parally 
furrr::future_map(.x = speciesList, .f = stage2,
                  dir1 = dir1, 
                  runVersion = runVersion,
                  overwrite = overwrite, 
                  speciesData = speciesData,
                  .progress = TRUE, 
                  .options = furrr_options(seed = seed))
tictoc::toc()
# clean environment 
rm(list=ls())
