pacman::p_load(terra, sf)

# ---------------------------------------------------------
# SETUP & TEMPLATE PREPARATION
# ---------------------------------------------------------
bio1 <- readRDS("data/geospatial_datasets/bioclim_layers/bioVar_1km.RDS")
template1 <- bio1$bio_1
cell_areas <- cellSize(template1, unit = "km")

wdpa_dir <- "data/wdpa_072026/"
wdpa_files <- list.files(path = wdpa_dir, pattern = "polygons.shp", recursive = TRUE, full.names = TRUE)


# Create a dedicated temp folder in your project directory
terra_temp <- "data/wdpa_072026/terra_temp/"
if (!dir.exists(terra_temp)) {
  dir.create(terra_temp, recursive = TRUE)
}

# Instruct terra to use this directory for all backend processing
terraOptions(tempdir = terra_temp)


# Helper function
calculate_pa_area <- function(pa_raster, area_raster, is_fraction = FALSE) {
  if (is_fraction) {
    fractional_area <- pa_raster * area_raster
    total <- global(fractional_area, "sum", na.rm = TRUE)
  } else {
    masked_area <- mask(area_raster, pa_raster, maskvalues = 0)
    total <- global(masked_area, "sum", na.rm = TRUE)
  }
  return(total$sum)
}

# ---------------------------------------------------------
# ISOLATE FIRST VALID CHUNK, FILTER & SUBSAMPLE
# ---------------------------------------------------------
message("Searching for the first valid shapefile chunk for testing...")

valid_vect <- NULL

for (i in seq_along(wdpa_files)) {
  current_sf <- st_read(wdpa_files[i], quiet = TRUE)
  
  # Apply filters
  # Apply filters using %in% for exact matching
  current_sf <- subset(
    current_sf, 
    REALM %in% c("Terrestrial", "Coastal") & 
      STATUS %in% c("Designated", "Inscribed", "Established")
  )
  if (nrow(current_sf) > 0) {
    message(sprintf("Found valid data in file %d: %s", i, wdpa_files[i]))
    message(sprintf("  -> Initial valid features: %d", nrow(current_sf)))
    
    # Calculate 10% of the rows (ensure at least 1 feature is selected)
    sample_size <- max(1, floor(0.10 * nrow(current_sf)))
    
    # Randomly sample the rows (seed set for reproducible testing)
    set.seed(42) 
    sampled_indices <- sample(seq_len(nrow(current_sf)), size = sample_size)
    current_sf <- current_sf[sampled_indices, ]
    
    message(sprintf("  -> Subsampled to 10%%: %d features for rapid testing.", nrow(current_sf)))
    
    valid_vect <- terra::vect(current_sf)
    break # Stop searching once we have our subsampled chunk
  }
}

if (is.null(valid_vect)) {
  stop("No valid terrestrial features found in any of the provided shapefiles.")
}

# ---------------------------------------------------------
# CRS ALIGNMENT
# ---------------------------------------------------------
# Ensure the vector dataset explicitly matches the raster template
if (crs(valid_vect) != crs(template1)) {
  message("  -> CRS mismatch detected. Reprojecting vector to match template...")
  valid_vect <- terra::project(valid_vect, crs(template1))
} else {
  message("  -> CRS alignment verified.")
}

# Assign presence field for binary methods
valid_vect$pa_presence <- 1

# ---------------------------------------------------------
# RUN EVALUATION
# ---------------------------------------------------------
message("Running standard rasterization methods...")

r_center <- terra::rasterize(valid_vect, template1, field = "pa_presence", background = 0)
r_touches <- terra::rasterize(valid_vect, template1, field = "pa_presence", touches = TRUE, background = 0)

message("Aggregating overlapping geometries for Fractional Cover evaluation...")
# Flatten the polygons to dissolve internal boundaries and overlapping designations
valid_vect_agg <- terra::aggregate(valid_vect)

# creating a finer resolution template to replace the cover function.  
# 1. Create a finer resolution template (e.g., 10x finer)
# disagg splits the 1km cells into smaller cells
template_fine <- terra::disagg(template1, fact = 10)



message("Running Fractional Cover method...")
r_cover <- terra::rasterize(valid_vect_agg, template1, cover = TRUE)

# Calculate areas
area_center <- calculate_pa_area(r_center, cell_areas)
area_touches <- calculate_pa_area(r_touches, cell_areas)
# 2. Rasterize using the fast default (center) method on the fine grid
r_fine_center <- terra::rasterize(valid_vect_agg, template_fine, field = "pa_presence", background = 0)

# 3. Aggregate back to 1km, calculating the mean of the binary values
r_fractional_approx <- terra::aggregate(r_fine_center, fact = 10, fun = "mean")

results <- data.frame(
  Method = c("Default (Cell Center)", "Touches (Any Overlap)", "Cover (Fractional, Aggregated)"),
  Total_Area_km2 = c(area_center, area_touches, area_cover)
)

message("\n--- METHOD EVALUATION RESULTS (10% SAMPLE) ---")
print(results)
message("----------------------------------------------\n")

# Clean up memory
unlink(terra_temp, recursive = TRUE)
rm(valid_vect, valid_vect_agg, r_center, r_touches, r_cover, current_sf)
gc()