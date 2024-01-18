###
# Stage 1 - Set allPaths and File Folder for unique Taxon
# 
###

#define libraries used 
pacman::p_load(future,furrr)

# source Global parameters 
source("~/Documents/cwr_wildgrapes/distributiveModel/globalParameters.R")

# source specific functions for this workflow
source("~/Documents/cwr_wildgrapes/R2/dataProcessing/generateFolders.R")

# set future plan 
plan(multisession, workers = round(cores * 7/8))

# generate function that containerizes the specific calls 
## these function should allways start with the taxon variable that that is what is being mapped over.
stage1 <- function(taxon, dir1, runVersion){
  # define all paths object
  allPaths <- definePaths(dir1 = dir1,
                          j = taxon,
                          runVersion = runVersion)
  # create folders as needed 
  generateFolders(allPaths)
  
}
tictoc::tic()
# all the function in parally 
furrr::future_map(.x = speciesList, .f = stage1,
                  dir1 = dir1, 
                  runVersion = runVersion,
                  .progress = TRUE, 
                  .options = furrr_options(seed = seed))
tictoc::toc()
# clean environment 
rm(list=ls())
