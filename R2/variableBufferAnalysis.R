# Script to run gap analysis (SRS, GRS, ERS) at variable buffer sizes (1, 5, 20, 50, 100 km)
# under both WGS84 (geodetic) and an Equal Areas projection.
# Includes a statistical measure (coefficient of variation) of the changes across buffer sizes.

pacman::p_load("dplyr", "sf", "terra", "purrr", "readr", "stringr")

# Source standard functions
source("R2/helperFunctions.R")
sourceFiles(gapAnalysisOnly = FALSE)

# Define an Equal Area Projection (Cylindrical Equal Area)
ea_proj <- "+proj=cea +lon_0=0 +lat_ts=30 +datum=WGS84 +units=m +no_defs"

#' Run Variable Buffer Gap Analysis
#' 
#' @param species The taxon name (e.g., "Vitis rupestris")
#' @param runVersion The model run version directory
#' @param buffer_sizes_km Numeric vector of buffer sizes in kilometers
#' @return A list containing the raw results table and a statistics table.
run_variable_buffer_analysis <- function(species, 
                                         runVersion = "run08282025_1k",
                                         buffer_sizes_km = c(1, 5, 20, 50, 100)) {
  
  # Define path
  p1 <- paste0("data/Vitis/", species, "/", runVersion)
  if(!dir.exists(p1)) {
    message("No data found for ", species)
    return(NULL)
  }
  
  message("Processing species: ", species)
  
  # Load data
  sp1 <- sf::st_read(paste0(p1, "/occurances/spatialData.gpkg"), quiet = TRUE)
  counts <- read_csv(paste0(p1, "/occurances/counts.csv"), show_col_types = FALSE)
  natArea <- sf::st_read(paste0(p1, "/results/naturalArea.gpkg"), quiet = TRUE)
  
  # Rasters
  thres <- terra::rast(paste0(p1, "/results/prj_threshold.tif"))
  protectedAreas <- terra::rast("data/geospatial_datasets/protectedLands/wdpa_1km_all_.tif")
  
  # Base template raster for rasterizing (using thres for extent and resolution)
  templateRast <- thres
  templateRast[!is.na(templateRast)] <- 1
  
  projections <- c("WGS84", "EqualArea")
  results_list <- list()
  
  for(proj in projections) {
    message("  Projection: ", proj)
    
    if(proj == "EqualArea") {
      # Project datasets to Equal Area
      sp1_proj <- sf::st_transform(sp1, crs = ea_proj)
      natArea_proj <- sf::st_transform(natArea, crs = ea_proj)
      thres_proj <- terra::project(thres, ea_proj, method = "near")
      protectedAreas_proj <- terra::crop(protectedAreas, natArea) |> 
        terra::project(ea_proj, method = "near")
      templateRast_proj <- terra::project(templateRast, ea_proj, method = "near")
    } else {
      # Use original WGS84
      sp1_proj <- sp1
      natArea_proj <- natArea
      thres_proj <- thres
      protectedAreas_proj <- protectedAreas
      templateRast_proj <- templateRast
    }
    
    # Calculate In Situ Gap Analysis (independent of ex situ buffer size, but affected by projection)
    # The functions are designed to handle SpatRasters and SF objects
    srsin <- srs_insitu(occuranceData = sp1_proj, thres = thres_proj, protectedArea = protectedAreas_proj)
    ersin <- ers_insitu(occuranceData = sp1_proj, nativeArea = natArea_proj, protectedArea = protectedAreas_proj, thres = thres_proj)
    grsin <- grs_insitu(occuranceData = sp1_proj, protectedArea = protectedAreas_proj, thres = thres_proj)
    fcsin <- fcs_insitu(srsin = srsin, grsin = grsin, ersin = ersin,noModel = FALSE)
    
    # Ex Situ SRS is strictly based on counts, independent of buffer
    srsex <- srs_exsitu(sp_counts = counts)
    
    for(b_km in buffer_sizes_km) {
      message("    Buffer: ", b_km, " km")
      b_m <- b_km * 1000
      
      # create_buffers logic adapted for projected/unprojected
      p1_G <- sp1_proj |> dplyr::filter(type == "G")
      
      if(nrow(p1_G) == 0) {
        g_bufferCrop <- "there are no g points for this species"
      } else {
        # Buffer points (Geodetic if WGS84, Cartesian if Equal Area)
        g_buff_vect <- terra::vect(p1_G) |> terra::buffer(width = b_m)
        
        # Crop and mask template to natArea
        r1 <- templateRast_proj |> 
          terra::crop(terra::vect(natArea_proj)) |> 
          terra::mask(terra::vect(natArea_proj))
        
        # Rasterize buffered vector
        g_buff_rs <- terra::rasterize(g_buff_vect, r1) |> 
          terra::crop(terra::vect(natArea_proj)) |> 
          terra::mask(terra::vect(natArea_proj))
        
        # Crop to threshold
        g_bufferCrop <- cropG_Buffer(ga50 = g_buff_rs, thres = thres_proj)
      }
      
      # Calculate Ex Situ Gap Analysis
      # speciesData should be a dataframe with at least $taxon to identify the species. We use counts.
      grsex <- grs_exsitu(speciesData = sp1_proj, ga50 = g_bufferCrop, thres = thres_proj)
      ersex <- ers_exsitu(speciesData = sp1_proj, thres = thres_proj, natArea = natArea_proj, ga50 = g_bufferCrop)
      
      fcsex <- fcs_exsitu(srsex = srsex, grsex = grsex, ersex = ersex, noModel = FALSE, gPoints = nrow(p1_G))
      
      # Combined FCS
      fcsCombined <- fcs_combine(fcsin = fcsin, fcsex = fcsex)
      
      # Store result
      res <- data.frame(
        species = species,
        projection = proj,
        buffer_km = b_km,
        SRS_ex = srsex$SRS,
        GRS_ex = grsex$GRS,
        ERS_ex = ersex$ERS,
        FCS_ex = fcsex$FCS,
        SRS_in = srsin$SRS,
        GRS_in = grsin$GRS,
        ERS_in = ersin$ERS,
        FCS_in = fcsin$FCS,
        FCS_min = fcsCombined$FCSc_min,
        FCS_max = fcsCombined$FCSc_max,
        FCS_mean = fcsCombined$FCSc_mean
      )
      
      results_list[[length(results_list) + 1]] <- res
    }
  }
  
  final_df <- dplyr::bind_rows(results_list)
  
  # Calculate Statistical Measure: Coefficient of Variation (CV) & Standard Deviation for Ex Situ Metrics across buffer sizes
  # CV = (Standard Deviation / Mean) * 100
  cv_stats <- final_df |>
    dplyr::group_by(species, projection) |>
    dplyr::summarise(
      GRS_ex_SD = sd(GRS_ex, na.rm = TRUE),
      GRS_ex_CV = ifelse(mean(GRS_ex, na.rm=TRUE) == 0, 0, (sd(GRS_ex, na.rm = TRUE) / mean(GRS_ex, na.rm = TRUE)) * 100),
      ERS_ex_SD = sd(ERS_ex, na.rm = TRUE),
      ERS_ex_CV = ifelse(mean(ERS_ex, na.rm=TRUE) == 0, 0, (sd(ERS_ex, na.rm = TRUE) / mean(ERS_ex, na.rm = TRUE)) * 100),
      FCS_ex_SD = sd(FCS_ex, na.rm = TRUE),
      FCS_ex_CV = ifelse(mean(FCS_ex, na.rm=TRUE) == 0, 0, (sd(FCS_ex, na.rm = TRUE) / mean(FCS_ex, na.rm = TRUE)) * 100),
      .groups = "drop"
    )
  
  return(list(
    results = final_df,
    statistics = cv_stats
  ))
}

# -----------------------------------------------------------------------------------------
# Example Execution:
# Uncomment the code below to run for specific species, e.g., "Vitis rupestris"
# -----------------------------------------------------------------------------------------
# species_list <- c("Vitis rupestris")
# 
# all_raw_results <- list()
# all_stats <- list()
# 
# for(spp in species_list) {
#   out <- run_variable_buffer_analysis(spp)
#   if(!is.null(out)) {
#     all_raw_results[[spp]] <- out$results
#     all_stats[[spp]] <- out$statistics
#   }
# }
# 
# # Combine all species results into single dataframes
# final_raw_results_df <- dplyr::bind_rows(all_raw_results)
# final_stats_df <- dplyr::bind_rows(all_stats)
# 
# # Write to CSV
# write_csv(final_raw_results_df, "variable_buffer_raw_results.csv")
# write_csv(final_stats_df, "variable_buffer_statistics.csv")
# 
# print("Variable buffer analysis complete. Results saved to CSV.")
