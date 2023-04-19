###
# 2023 CWR modeling workflow target structure 
# 20230303
# carverd@colostate.edu 
###


# Load packages required to define the pipeline:
pacman::p_load(targets,tarchetypes, dplyr)

# Set target options:
tar_option_set(
  packages = c("terra", "dplyr", "sf", "purrr","tmap","randomForest","VSURF",
               "modelr","maxnet","pROC","DT", "readr", "vroom", "readr"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_future() configuration (okay to leave alone):
future::plan(future.callr::callr)

# Run the R scripts in the R/ folder with your custom functions:
lapply(
  # need to ensure functions are grabbed first
  # this sources some object twice but oh well
  c(list.files(
    path = "R/functions",
    pattern = ".R",
    full.names = TRUE,
    recursive = TRUE
    ), list.files(
    path = "R/",
    pattern = ".R",
    full.names = TRUE,
    recursive = TRUE
    )),
  targets::tar_source
)

# Replace the target list below with your own:
list(
  # input data processing ---------------------------------------------------
  # global objects 
  c(globalTargets),
  
  ## define input occurance data 
  ### outside of the globalTargets features because this is unique to the model run. 
  ### input datasets needs to have the following structure
  
  ##! better to have some preprocessing script to structure the input datasets 
  ##! with specific names / column types etc
  tar_file_read(name = occuranceData,
                "data/raw_occurances/daucusData_BioClimatic_2.5arc_modified.csv",
                read = read_csv(!!.x)
                ),
  

  # environment setup -------------------------------------------------------
  # generate file directories for species 
  ## this will be limited as we're using targets for intermediate steps  
  c(environmentalSetup),
  

  # data processing ---------------------------------------------------------
  c(dataProcessing),
  ## County filter -- observations only found in specific counties of interest 

  ### outcome --- I want to filter out any species that do not have valid lat long
  ### values at this point. Might require redefining the speciesList object for
  ### the next step. Really I'm just not so sure how targets is going to work with 
  ### empty object or elements that are difference data types??? 
  
  
  # running the model  ------------------------------------------------------
  c(runModels) 
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
