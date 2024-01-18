# prep input parameters
pacman::p_load("terra", "sf", "readr","dplyr") 

# species data --------------------------------------------------------
# vitis
read_csv("data/processed_occurrence/draft_model_data.csv")|>
  dplyr::select(-c("geometry","index", "validLat","validLon","validLatLon"))|>
  write_rds(file = "~/Documents/cwr_wildgrapes/distributiveModel/data/vitis_speciesData.rds")
# daucus
read_csv("data/raw_occurances/daucusData_BioClimatic_2.5arc_modified.csv")|>
  dplyr::rename(institutionCode = institute)|>
  write_rds(file = "~/Documents/cwr_wildgrapes/distributiveModel/data/daucus_speciesData.rds")

# bioclim Layers --------------------------------------------------------
bioNames <- read_csv("data/geospatial_datasets/bioclim_layers/variableNames.csv")
bioVars <- readRDS("data/geospatial_datasets/bioclim_layers/bioclim_2.5arcsec_terra.RDS")
### assign names 
names(bioVars) <- bioNames$shortName
### export 
write_rds(x = bioVars |> terra::wrap(),
          file = "~/Documents/cwr_wildgrapes/distributiveModel/data/bioClim.rds")
### generate template element 
bioVars[[1]] |>
  terra::wrap()|>
  write_rds(file = "~/Documents/cwr_wildgrapes/distributiveModel/data/templateRast.rds")
## clear environment of datasets 
rm(bioNames,bioVars)


# ecoregions --------------------------------------------------------
sf::st_read("data/geospatial_datasets/ecoregions/tnc_terr_ecoregions.gpkg")|>
  write_rds(file = "~/Documents/cwr_wildgrapes/distributiveModel/data/ecoregion.rds")

# protected lands --------------------------------------------------------
terra::rast("data/geospatial_datasets/protectedLands/wdpa_rasterized_all.tif")|>
  terra::wrap()|>
  write_rds(file = "~/Documents/cwr_wildgrapes/distributiveModel/data/protectedAreas.rds")

