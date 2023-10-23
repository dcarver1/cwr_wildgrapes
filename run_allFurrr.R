###
# Developing a furrr implementation of the processing code
# carverd@colostate.edu
# 20231023
### 


# local testing 
pacman::p_load("terra", "dplyr", "sf", "purrr","randomForest","VSURF",
               "modelr","maxnet","pROC","DT", "readr", "vroom", "readr", "dismo",
               "leaflet", "tidyterra", "rmarkdown", "furrr","tmap", "stringr")
tmap_mode("view")

#source functions
source("R2/helperFunctions.R")
sourceFiles(furrr = TRUE)

# source global objects 
numPoint <- 2000
bufferDist <- 0.45
set.seed(1234)
runVersion <- "test1"

## overwrite Parameter 
overwrite <- FALSE

# define species data 
speciesData <- read_csv("data/processed_occurrence/draft_model_data.csv")
species <- unique(speciesData$taxon)




