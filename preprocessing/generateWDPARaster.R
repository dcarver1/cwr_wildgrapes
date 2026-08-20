pacman::p_load(terra, sf)

# ---------------------------------------------------------
# USER CONFIGURATION
# ---------------------------------------------------------
# Set this to "cover", "touches", or "center" based on your test evaluation
CHOSEN_METHOD <- "center" 

# ---------------------------------------------------------
# SETUP & TEMPLATE PREPARATION
# ---------------------------------------------------------
bio1 <- readRDS("data/geospatial_datasets/bioclim_layers/bioVar_1km.RDS")
template1 <- bio1$bio_1

wdpa_dir <- "data/wdpa_072026/"
wdpa_files <- list.files(path = wdpa_dir, pattern = "polygons.shp", recursive = TRUE, full.names = TRUE)

temp_raster_dir <- "data/wdpa_072026/temp_rasters/"
if (!dir.exists(temp_raster_dir)) {
  dir.create(temp_raster_dir, recursive = TRUE)
}

# ---------------------------------------------------------
# CHUNKED PROCESSING LOOP
# ---------------------------------------------------------
message(sprintf("Starting full dataset processing using method: %s", CHOSEN_METHOD))

for (i in seq_along(wdpa_files)) {
  message(sprintf("Processing file %d of %d: %s", i, length(wdpa_files), wdpa_files[i]))
  
  current_sf <- st_read(wdpa_files[i], quiet = TRUE)
  
  # Apply filters using %in% for exact matching
  current_sf <- subset(
    current_sf, 
    REALM %in% c("Terrestrial", "Coastal") & 
      STATUS %in% c("Designated", "Inscribed", "Established")
  )
  
  if (nrow(current_sf) == 0) {
    message("  -> No matching records after filtering. Skipping.")
    rm(current_sf)
    gc()
    next
  }
  
  current_vect <- terra::vect(current_sf)
  current_vect$pa_presence <- 1
  
  message("  -> Rasterizing...")
  
  # Apply chosen method
  if (CHOSEN_METHOD == "cover") {
    chunk_raster <- terra::rasterize(current_vect, template1, cover = TRUE)
  } else if (CHOSEN_METHOD == "touches") {
    chunk_raster <- terra::rasterize(current_vect, template1, field = "pa_presence", touches = TRUE, background = 0)
  } else {
    chunk_raster <- terra::rasterize(current_vect, template1, field = "pa_presence", background = 0)
  }
  
  out_file <- file.path(temp_raster_dir, sprintf("wdpa_chunk_%03d.tif", i))
  terra::writeRaster(chunk_raster, out_file, overwrite = TRUE)
  message(sprintf("  -> Saved intermediate raster to %s", out_file))
  
  rm(current_sf, current_vect, chunk_raster)
  gc()
}

# ---------------------------------------------------------
# AGGREGATE FINAL RASTER
# ---------------------------------------------------------
message("Aggregating intermediate rasters into final dataset...")

temp_tifs <- list.files(temp_raster_dir, pattern = "\\.tif$", full.names = TRUE)
raster_collection <- terra::sprc(temp_tifs)

# If using fractional cover, 'sum' might be appropriate for boundaries depending on your gap analysis needs.
# For binary methods, 'max' is standard.
mosaic_fun <- ifelse(CHOSEN_METHOD == "cover", "max", "max") 

final_wdpa_raster <- terra::mosaic(raster_collection, fun = mosaic_fun)
# reclass the 0 to NA 
final_wdpa_raster <- terra::subst(x = final_wdpa_raster, from = 0, to = NA)


final_output_path <- sprintf("data/wdpa_072026/wdpa_1km_final_%s.tif", CHOSEN_METHOD)
terra::writeRaster(final_wdpa_raster, final_output_path, overwrite = TRUE)

message(sprintf("Success! Final dataset saved to %s", final_output_path))
