###
# Generate a set of targets to render the census tract data
# 20221109
# carverd@colostate.edu
###

globalTargets <- list(
  # name csv for bioclim layers 
  tar_file_read(bioNames,
                "data/geospatial_datasets/bioclim_layers/variableNames.csv",
                read = read_csv(!!.x)
                ),
  # predictor variables 
  ## set path to the object
  tar_target(rasterRDS,
             "data/geospatial_datasets/bioclim_layers/bioclim_2.5arcsec_terra.RDS",
             format = "file"),
  ## process data 
  tar_target(name = biovars, command = processBioClim(rasters = rasterRDS,
                                                      names = bioNames))
  
)
  