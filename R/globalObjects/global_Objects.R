###
# Set global parameters for the modeling process
# 20230308
# carverd@colostate.edu 
###


# adjustable parameters ---------------------------------------------------
## maximun number of points used in model (use in subSampleCountry.R)
numPoint <- 2000
## used to define buffer distance in g buffer process
bufferDist <- 50000
# set random seed. Important for reproducability 
set.seed(1234)

