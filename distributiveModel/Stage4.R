###
# Stage 4 - Prep data for modeing
# Memory use is ~ 32gb -- so keep the cores number of process to 3 
# sequential : 4421.243 sec elapsed
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

plan(sequential , workers = 3) # using multiplication to account for the variable cpu qualities 

#testing
overwrite <- TRUE

# generate function that containerizes the specific calls 
## these function should allways start with the taxon variable that that is what is being mapped over.
stage4 <- function(taxon, dir1, runVersion,overwrite,
                   bioVars){
  
  # define all paths object
  allPaths <- definePaths(dir1 = dir1,
                          j = taxon,
                          runVersion = runVersion)

  print(taxon)
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
    
      # unwrap the biovars features 
      bioVars <- bioVars |> terra::unwrap()
      # associate observations with bioclim data
      m_data <- write_CSV(path = allPaths$allDataPath,
                        overwrite = TRUE,
                        generateModelData(speciesPoints = sp1,
                                          natArea = natArea,
                                          bioVars = bioVars,
                                          b_Number = b_Number))
      ## perform variable selection
      v_data <- write_RDS(path = allPaths$variablbeSelectPath,
                        overwrite = TRUE,
                        function1 = varaibleSelection(modelData = m_data,
                                                      parallel = FALSE))
      # 
      ## prepare data for maxent model
      print("preping Rasters")
      rasterInputs <- write_Rast(path = allPaths$prepRasters,
                               overwrite = overwrite,
                               function1 = cropRasters(natArea = natArea,
                                                       bioVars = bioVars,
                                                       selectVars = v_data))

    }
  }
}

# read in input files --- might want to keeep this wrap until we pass into the function
## - maybe this matters for multisesson vs multiprocessor
bioVars <- read_rds("~/Documents/cwr_wildgrapes/distributiveModel/data/bioClim.rds")
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
