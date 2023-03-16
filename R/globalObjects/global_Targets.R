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
                                                      names = bioNames)),
  # read in additional spatial data objects
  # countries
  tar_file_read(
    countries, 
    "data/geospatial_datasets/countries/ne_10m_admin_0_countries.gpkg",
    read = sf::st_read(!!.x)
  ),
  #ecoregions
  tar_file_read(
    ecoregions, 
    "data/geospatial_datasets/ecoregions/tnc_terr_ecoregions.gpkg",
    read = sf::st_read(!!.x)
  ),
  # states 
  tar_file_read(
    states, 
    "data/geospatial_datasets/states/ne_10m_admin_1_states_provinces.gpkg",
    read = sf::st_read(!!.x)
  ),
  #counties
  tar_file_read(
    counties, 
    "data/geospatial_datasets/counties/ne_10m_admin_2_counties.gpkg",
    read = sf::st_read(!!.x)
  )
)
  