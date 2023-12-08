###
# Stage 3 - Define native area features
# 
###

#define libraries used 
pacman::p_load(future,furrr, sf, dplyr, terra)

# source Global parameters 
source("~/Documents/cwr_wildgrapes/distributiveModel/globalParameters.R")

# source specific functions for this workflow
source("~/Documents/cwr_wildgrapes/R2/modeling/generateModelData.R")
source("~/Documents/cwr_wildgrapes/R2/modeling/numberBackground.R")
source("~/Documents/cwr_wildgrapes/R2/modeling/variableSelection.R")
source("~/Documents/cwr_wildgrapes/R2/modeling/cropRasters.R")

# multicore -- shared environment distributed processes -- so far slightly slower then sequential?  
# sequential  -- linear process
# multisession -- not very good do to the set up of multiple environments 
plan(multicore, workers = round(cores * 4/5)) # using multiplication to account for the variable cpu qualities 

overwrite <- TRUE
# generate function that containerizes the specific calls 
## these function should allways start with the taxon variable that that is what is being mapped over.
stage4 <- function(taxon, dir1, runVersion,overwrite,
                   bioVars){
  
  # define all paths object
  allPaths <- definePaths(dir1 = dir1,
                          j = taxon,
                          runVersion = runVersion)

  
  # read in generated Objects 
  if(file.exists(allPaths$spatialDataPath)){
    # read in point object 
    sp1 <- st_read(allPaths$spatialDataPath)
    # read in ecoregion feature
    natArea <- st_read(allPaths$natAreaPath)
        
    # condition for at least 5 observations
    if(nrow(sp1) >=5 ){
    
      ## define number of background points 
      b_Number <- numberBackground(natArea = natArea)
    
      # associate observations with bioclim data
      m_data <- write_CSV(path = allPaths$allDataPath,
                        overwrite = overwrite,
                        generateModelData(speciesPoints = sp1,
                                          natArea = natArea,
                                          bioVars = bioVars,
                                          b_Number = b_Number))
      ## perform variable selection
      v_data <- write_RDS(path = allPaths$variablbeSelectPath,
                        overwrite = overwrite,
                        function1 = varaibleSelection(modelData = m_data))

      ## prepare data for maxent model
      # rasterInputs <- write_Rast(path = allPaths$prepRasters,
      #                          overwrite = overwrite,
      #                          function1 = cropRasters(natArea = natArea,
      #                                                  bioVars = bioVars,
      #                                                  selectVars = v_data))
      # 
    }
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
