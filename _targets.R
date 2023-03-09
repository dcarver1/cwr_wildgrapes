###
# 2023 CWR modeling workflow target structure 
# 20230303
# carverd@colostate.edu 
###


# Load packages required to define the pipeline:
pacman::p_load(targets,tarchetypes)

# Set target options:
tar_option_set(
  packages = c("terra", "dplyr", "sf", "purrr","tmap","randomForest","VSURF",
               "modelr","maxnet","pROC","DT", "readr"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_future() configuration (okay to leave alone):
future::plan(future.callr::callr)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed. # nolint





# Replace the target list below with your own:
list(
  # input data processing ---------------------------------------------------
  # global objects 
  c(globalTargets)

  # environment setup -------------------------------------------------------
  # generate file directories for species 
  ## this will be limited as we're using targets for intemediate steps  

  
  # individual species 
  ## filter raw data to species of interest 
  ## generate counts dataset for summary stats 
    ### used as a filtering process for what records are used in the model/SRS assessment 
  ## create spatial point object 
  ## County filter -- observations only found in specific counties of interest 
  ## duplication filter -- points with exact lat long

  # running the model  ------------------------------------------------------
  ## subsample based on geography 
  ## generates a native area extent file 
  ## generate g50 buffer object 
  ## associate observations with bioclim data
  ## perform variable selection 
  ## perform maxent model 
  ## evaluate outputs of the modeling process
  ## generate a mess map 
  ## generate a kernal density map 
  

  # Applying the gap analysis method ----------------------------------------
  ## SRSin assessment
  ## GRSin assessment 
  ## ERSin assessment 
  ## FCSin assessment
  
  ## SRSex assessment 
  ## GRSex assessment
  ## ERSex assessment
  ## FCSex assessment 
  
  ## FCS_combined assessment

  # generate summary documentation ------------------------------------------
  ## summary documentation 
  
  
  # error catching functions 
  ## unknown? 
   
)
