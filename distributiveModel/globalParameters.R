# source global objects 
library(readr)

## maximun number of points used in model (use in subSampleCountry.R)
numPoint <- 2000
## used to define buffer distance in g buffer process in degrees 
# 50k(1 degree/111km) = 0.45
### terra implementation uses measure in meters
bufferDist <- 50000
# set random seed. Important for reproducability
seed <- 1234
set.seed(seed)
# set run version 
runVersion <- "run20231227"

## overwrite Parameter 
### used to determine if you want to write over existing content. 
overwrite <- TRUE

# read in species Data 
## vitis 
# speciesData <- read_rds("~/Documents/cwr_wildgrapes/distributiveModel/data/vitis_speciesData.rds")
## daucus 
speciesData <- read_rds("~/Documents/cwr_wildgrapes/distributiveModel/data/daucus_speciesData.rds")
genus <- unique(speciesData$genus)[1]
speciesList <- unique(speciesData$taxon)

## for testing 
# speciesList <- speciesList[c(5,8,9)]


# set data directory 
dir1 <- paste0("data/",genus) 

# source the all paths function as it is required in all steps 
## define paths function
source("~/Documents/cwr_wildgrapes/R2/dataProcessing/definePaths.R")
## export features function
source("~/Documents/cwr_wildgrapes/R2/dataProcessing/exportFeatures.R")
## source memory testing function
source("~/Documents/cwr_wildgrapes/distributiveModel/memoryAvailable.R")


# # provide some estimated core requirements base on processes --------------
# source("~/Documents/cwr_wildgrapes/distributiveModel/memoryAvailable.R")
# totalMemory <- getFreeMemoryKB()

# set future plan 
cores <- future::availableCores()
## set based on the computational requirements of the procesess and the limitation of the computer maybe? 
## based on memory allocation more then anything 
## so could read in the initial objects, round up to the MB and divide by the total memory allocation to get something more 
## automated
## file moving : 7/8
## creating spatial features : 3/4 
## moderate spatial operations : 1/2 
## heavy spatial operations : 2/5 

totalMemory <- getFreeMemoryGB()




