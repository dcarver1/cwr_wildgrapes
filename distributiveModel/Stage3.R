###
# Stage 3 - Define native area features
# 
###

#define libraries used 
pacman::p_load(future,furrr, sf, dplyr)

# source Global parameters 
source("~/Documents/cwr_wildgrapes/distributiveModel/globalParameters.R")

# source specific functions for this workflow
source("~/Documents/cwr_wildgrapes/R2/dataProcessing/data_processing_functions.R")
source("~/Documents/cwr_wildgrapes/R2/modeling/create_buffers.R")

# multicore -- shared environment distributed processes -- so far slightly slower then sequential?  
# sequential  -- linear process
# multisession -- not very good do to the set up of multiple environments 
plan(sequential, workers = round(cores * 3/4)) # using multiplication to account for the variable cpu qualities 

### only want to define overwrite in a globalparameters section 
# overwrite <- TRUE

# generate function that containerizes the specific calls 
## these function should allways start with the taxon variable that that is what is being mapped over.
stage3 <- function(taxon, dir1, runVersion,overwrite,
                   ecoregions,
                   templateRast){
  templateRast <- terra::unwrap(templateRast)
  
  # define all paths object
  allPaths <- definePaths(dir1 = dir1,
                          j = taxon,
                          runVersion = runVersion)
  
  # read in generated Objects 
  if(file.exists(allPaths$spatialDataPath)){
    # read in point object 
    sp1 <- st_read(allPaths$spatialDataPath)
    
    ## define natural area based on ecoregions
    natArea <- write_GPKG(path = allPaths$natAreaPath,
                          overwrite = overwrite,
                          function1 = nat_area_shp(speciesPoints = sp1,
                                                   ecoregions = ecoregions))  
    
    # condition for at least 5 observations
    if(nrow(sp1) >=5 )
      
      ## generate GA50 objects
      g_buffer <- write_Rast(path = allPaths$ga50Path,
                             overwrite = TRUE,
                             function1 = create_buffers(speciesPoints = sp1,
                                                        natArea = natArea,
                                                        bufferDist = bufferDist,
                                                        templateRast = templateRast))
    print(taxon)
  }
}

# read in input files 
ecoregions <- read_rds("~/Documents/cwr_wildgrapes/distributiveModel/data/ecoregion.rds")
templateRast <- read_rds("~/Documents/cwr_wildgrapes/distributiveModel/data/templateRast.rds")

tictoc::tic()
# all the function in parally 
furrr::future_map(.x = speciesList, .f = stage3,
                  dir1 = dir1, 
                  runVersion = runVersion,
                  overwrite = overwrite, 
                  ecoregions = ecoregions,
                  templateRast = templateRast,
                  .progress = TRUE, 
                  .options = furrr_options(seed = seed))
tictoc::toc()
# clean environment overwrite
rm(list=ls())
