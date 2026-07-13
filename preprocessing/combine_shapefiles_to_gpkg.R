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
    
    # ---------------------------------------------------------
    # FILTERING STEP
    # ---------------------------------------------------------
    # Filter for Terrestrial/Coastal realms and specific statuses
    # We use grepl to catch mixed realms like "Terrestrial, Marine" which represent coastal reserves
    current_sf <- subset(
      current_sf, 
      grepl("Terrestrial", REALM, ignore.case = TRUE) & 
        STATUS %in% c("Designated", "Inscribed", "Established")
    )
    
    # Skip appending if the filtering results in an empty dataset for this chunk
    if (nrow(current_sf) == 0) {
      message("  -> No matching records after filtering. Skipping.")
      rm(current_sf)
      gc()
      next
    }
    
    if (i == 1 || !file.exists(output_gpkg)) {
      # For the first file (or first non-empty file), create the GeoPackage
      st_write(current_sf, 
               dsn = output_gpkg, 
               layer = layer_name,
               driver = "GPKG", 
               append = FALSE, 
               delete_dsn = TRUE,
               quiet = TRUE)
      message("  -> GeoPackage created and first filtered file written.")
    } else {
      # For subsequent files, append to the existing GeoPackage layer
      st_write(current_sf, 
               dsn = output_gpkg, 
               layer = layer_name,
               driver = "GPKG", 
               append = TRUE,
               quiet = TRUE)
      message("  -> Filtered data appended to GeoPackage.")
    }
    
    # Clear the object from memory and force garbage collection
    rm(current_sf)
    gc()
  }
  
  message(sprintf("Success! Filtered shapefiles have been combined into %s", output_gpkg))
}

# Combine the WDPA layers 
wdpas <- list.files(path = "data/wdpa_072026/", pattern = "polygons.shp", recursive = TRUE, full.names = TRUE)

combine_shapefiles_to_gpkg(shp_paths = wdpas,
                           output_gpkg = "data/wdpa_072026/combined_wdpa_072026.gpkg")