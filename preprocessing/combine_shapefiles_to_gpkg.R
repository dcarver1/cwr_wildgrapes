library(sf)

combine_shapefiles_to_gpkg <- function(shp_paths, output_gpkg, layer_name = "combined_data") {
  # Verify all provided file paths exist before starting
  if (!all(file.exists(shp_paths))) {
    stop("One or more shapefile paths do not exist. Please check your file paths.")
  }
  
  for (i in seq_along(shp_paths)) {
    message(sprintf("Processing file %d of %d: %s", i, length(shp_paths), shp_paths[i]))
    
    # Read the shapefile
    current_sf <- st_read(shp_paths[i], quiet = TRUE)
    
    if (i == 1) {
      # For the first file, create the GeoPackage (overwriting if it already exists)
      st_write(current_sf, 
               dsn = output_gpkg, 
               layer = layer_name,
               driver = "GPKG", 
               append = FALSE, 
               delete_dsn = TRUE,
               quiet = TRUE)
      message("  -> GeoPackage created and first file written.")
    } else {
      # For subsequent files, append to the existing GeoPackage layer
      st_write(current_sf, 
               dsn = output_gpkg, 
               layer = layer_name,
               driver = "GPKG", 
               append = TRUE,
               quiet = TRUE)
      message("  -> Data appended to GeoPackage.")
    }
    
    # Clear the object from memory and force garbage collection
    rm(current_sf)
    gc()
  }
  
  message(sprintf("Success! All shapefiles have been combined into %s", output_gpkg))
}

# combine the WDPA layres 
wdpas <- list.files(path = "data/wdpa_072026/", pattern = "polygons.shp",recursive = TRUE,full.names = TRUE)

combine_shapefiles_to_gpkg(shp_paths = wdpas,
                           output_gpkg = "data/wdpa_072026/combined_wdpa_072026.gpkg")
