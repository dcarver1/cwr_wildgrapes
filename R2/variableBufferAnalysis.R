# Script to run gap analysis (SRS, GRS, ERS) at variable buffer sizes (1, 5, 20, 50, 100 km)
# under both WGS84 (geodetic) and an Equal Areas projection.

pacman::p_load("dplyr", "sf", "terra", "purrr", "readr", "stringr", "tidyr")

# Source standard functions
source("R2/helperFunctions.R")
sourceFiles(gapAnalysisOnly = TRUE)
source("R2/modeling/cropG_buffer.R")

# -----------------------------------------------------------------------------------------
# Configuration & Setup
# -----------------------------------------------------------------------------------------
ea_proj <- "+proj=cea +lon_0=0 +lat_ts=30 +datum=WGS84 +units=m +no_defs"
runVersion <- "run08282025_1k"
buffer_sizes_km <- c(1, 5, 20, 50, 100)
projections <- c("WGS84", "EqualArea")
overwrite <- FALSE

species_list <- read_csv("data/processed_occurrence/model_data20251216.csv") |>
  dplyr::select("taxon") |>
  dplyr::pull() |>
  unique()
# removing novo
species_list <- species_list[!species_list %in% c("Vitis novogranatensis",
"Vitis rufotomentosa")]

# Global storage for post-loop summaries
all_raw_results <- list()
all_stats <- list()

# -----------------------------------------------------------------------------------------
# Main Processing Loop
# -----------------------------------------------------------------------------------------
for (species in species_list) {
  message("\nProcessing species: ", species)

  # Setup paths
  p1 <- file.path("data/Vitis", species, runVersion)
  if (!dir.exists(p1)) {
    message("  -> No data found for ", species, ". Skipping.")
    next
  }

  out_dir <- file.path(p1, "results")
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  results_path <- file.path(out_dir, "variable_buffer_raw_results.csv")
  stats_path <- file.path(out_dir, "variable_buffer_statistics.csv")

  # Resume/Skip logic
  if (!overwrite && file.exists(results_path) && file.exists(stats_path)) {
    message("  -> Skipping already processed species (outputs exist).")
    all_raw_results[[species]] <- readr::read_csv(
      results_path,
      show_col_types = FALSE
    )
    all_stats[[species]] <- readr::read_csv(stats_path, show_col_types = FALSE)
    next
  }

  # Load species inputs
  sp1 <- sf::st_read(file.path(p1, "occurances/spatialData.gpkg"), quiet = TRUE)
  counts <- readr::read_csv(
    file.path(p1, "occurances/counts.csv"),
    show_col_types = FALSE
  )
  natArea <- sf::st_read(
    file.path(p1, "results/naturalArea.gpkg"),
    quiet = TRUE
  )
  thres <- terra::rast(file.path(p1, "results/prj_threshold.tif"))
  protectedAreas <- terra::rast(
    "data/geospatial_datasets/protectedLands/wdpa_1km_all_.tif"
  )

  templateRast <- thres
  templateRast[!is.na(templateRast)] <- 1

  species_results_list <- list()

  for (proj in projections) {
    message("  -> Projection: ", proj)

    # Projection transformations
    # Projection transformations
    if (proj == "EqualArea") {
      sp1_proj <- sf::st_transform(sp1, crs = ea_proj)
      natArea_proj <- sf::st_transform(natArea, crs = ea_proj)

      # 1. Project the threshold raster first (this becomes the master grid)
      thres_proj <- terra::project(thres, ea_proj, method = "near")

      # 2. Project protectedAreas USING thres_proj as the template to guarantee grid alignment
      protectedAreas_proj <- terra::crop(protectedAreas, natArea) |>
        terra::project(thres_proj, method = "near")

      # 3. Create templateRast directly from thres_proj to save compute and guarantee alignment
      templateRast_proj <- thres_proj
      templateRast_proj[!is.na(templateRast_proj)] <- 1
    } else {
      sp1_proj <- sp1
      natArea_proj <- natArea
      thres_proj <- thres
      protectedAreas_proj <- protectedAreas
      templateRast_proj <- templateRast
    }

    # In situ (projection-specific, buffer-independent)
    srsin <- srs_insitu(
      occuranceData = sp1_proj,
      thres = thres_proj,
      protectedArea = protectedAreas_proj
    )
    ersin <- ers_insitu(
      occuranceData = sp1_proj,
      nativeArea = natArea_proj,
      protectedArea = protectedAreas_proj,
      thres = thres_proj
    )
    grsin <- grs_insitu(
      occuranceData = sp1_proj,
      protectedArea = protectedAreas_proj,
      thres = thres_proj
    )
    fcsin <- fcs_insitu(
      srsin = srsin,
      grsin = grsin,
      ersin = ersin,
      noModel = FALSE
    )

    # Ex situ SRS from counts only
    srsex <- srs_exsitu(sp_counts = counts)

    for (b_km in buffer_sizes_km) {
      message("    -> Buffer: ", b_km, " km")
      b_m <- b_km * 1000
      p1_G <- sp1_proj |> dplyr::filter(type == "G")

      if (nrow(p1_G) == 0) {
        g_bufferCrop <- "there are no g points for this species"
      } else {
        g_buff_vect <- terra::vect(p1_G) |> terra::buffer(width = b_m)
        r1 <- templateRast_proj |>
          terra::crop(terra::vect(natArea_proj)) |>
          terra::mask(terra::vect(natArea_proj))

        g_buff_rs <- terra::rasterize(g_buff_vect, r1) |>
          terra::crop(terra::vect(natArea_proj)) |>
          terra::mask(terra::vect(natArea_proj))

        g_buff_rs <- terra::resample(g_buff_rs, thres_proj, method = "near") |>
          terra::crop(thres_proj)

        g_bufferCrop <- cropG_Buffer(ga50 = g_buff_rs, thres = thres_proj)
      }

      grsex <- grs_exsitu(
        speciesData = sp1_proj,
        ga50 = g_bufferCrop,
        thres = thres_proj
      )
      ersex <- ers_exsitu(
        speciesData = sp1_proj,
        thres = thres_proj,
        natArea = natArea_proj,
        ga50 = g_bufferCrop
      )
      fcsex <- fcs_exsitu(
        srsex = srsex,
        grsex = grsex,
        ersex = ersex,
        noModel = FALSE,
        gPoints = nrow(p1_G)
      )
      fcsCombined <- fcs_combine(fcsin = fcsin, fcsex = fcsex)

      species_results_list[[length(species_results_list) + 1]] <- data.frame(
        species = species,
        projection = proj,
        buffer_km = b_km,
        SRS_ex = srsex$SRS,
        GRS_ex = grsex$GRS,
        ERS_ex = ersex$ERS,
        FCS_ex = fcsex$FCS,
        FCS_ex_class = fcsex$FCS_Score,
        SRS_in = srsin$SRS,
        GRS_in = grsin$GRS,
        ERS_in = ersin$ERS,
        FCS_in = fcsin$FCS,
        FCS_in_class = fcsin$FCS_Score,
        FCS_min = fcsCombined$FCSc_min,
        FCS_min_class = fcsCombined$FCSc_min_class,
        FCS_max = fcsCombined$FCSc_max,
        FCS_max_class = fcsCombined$FCSc_max_class,
        FCS_mean = fcsCombined$FCSc_mean,
        FCS_mean_class = fcsCombined$FCSc_mean_class
      )
    }
  }

  # Compile single species results
  species_final_df <- dplyr::bind_rows(species_results_list)

  # Calculate Statistical Measure: CV & SD per species
  cv_stats <- species_final_df |>
    dplyr::group_by(species, projection) |>
    dplyr::summarise(
      dplyr::across(
        c(
          "GRS_ex",
          "ERS_ex",
          "FCS_ex",
          "GRS_in",
          "ERS_in",
          "FCS_in",
          "FCS_min",
          "FCS_max",
          "FCS_mean"
        ),
        list(
          SD = ~ sd(.x, na.rm = TRUE),
          CV = ~ ifelse(
            mean(.x, na.rm = TRUE) == 0,
            0,
            (sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE)) * 100
          )
        ),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    )

  # ... (previous code inside the species loop: running the buffers) ...

  # Compile single species results (Raw Data)
  species_final_df <- dplyr::bind_rows(species_results_list)

  # Calculate Statistical Measure: CV & SD per species
  cv_stats <- species_final_df |>
    dplyr::group_by(species, projection) |>
    dplyr::summarise(
      dplyr::across(
        c(
          "GRS_ex",
          "ERS_ex",
          "FCS_ex",
          "GRS_in",
          "ERS_in",
          "FCS_in",
          "FCS_min",
          "FCS_max",
          "FCS_mean"
        ),
        list(
          SD = ~ sd(.x, na.rm = TRUE),
          CV = ~ ifelse(
            mean(.x, na.rm = TRUE) == 0,
            0,
            (sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE)) * 100
          )
        ),
        .names = "{.col}_{.fn}"
      ),
      .groups = "drop"
    )

  # NEW STEP: Join the CV and SD statistics directly into the raw results dataframe
  species_final_df <- species_final_df |>
    dplyr::left_join(cv_stats, by = c("species", "projection"))

  # Export to local species directory
  # Now, variable_buffer_raw_results.csv will contain both the raw scores AND the CV/SD columns!
  readr::write_csv(species_final_df, results_path)

  # (Optional) You can still save the standalone stats file if you want a condensed view,
  # or you can safely delete this line if you only want one file.
  readr::write_csv(cv_stats, stats_path)

  # Store for global aggregation
  all_raw_results[[species]] <- species_final_df
  all_stats[[species]] <- cv_stats
}

# -----------------------------------------------------------------------------------------
# Global Outputs & Summaries
# -----------------------------------------------------------------------------------------
message("\nAll species processed. Generating global exports and summaries...")

# Bind all lists into final dataframes
final_raw_results_df <- dplyr::bind_rows(all_raw_results)
final_stats_df <- dplyr::bind_rows(all_stats)

# Save global CSVs (final_raw_results_df now contains all the CV/SD columns)
readr::write_csv(
  final_raw_results_df,
  "variable_buffer_raw_results_with_stats.csv"
)
readr::write_csv(final_stats_df, "variable_buffer_statistics.csv")


# -----------------------------------------------------------------------------------------
# Global Outputs & Summaries
# -----------------------------------------------------------------------------------------
message("\nAll species processed. Generating global exports and summaries...")

# Bind all lists into final dataframes
final_raw_results_df <- dplyr::bind_rows(all_raw_results)
final_stats_df <- dplyr::bind_rows(all_stats)

# Save global CSVs
readr::write_csv(final_raw_results_df, "variable_buffer_raw_results.csv")
readr::write_csv(final_stats_df, "variable_buffer_statistics.csv")

# Summary Functions
summarize_projection_consistency <- function(results_df, target_buffer = 50) {
  metric_cols <- c(
    "SRS_ex",
    "GRS_ex",
    "ERS_ex",
    "FCS_ex",
    "SRS_in",
    "GRS_in",
    "ERS_in",
    "FCS_in",
    "FCS_min",
    "FCS_max",
    "FCS_mean"
  )

  # 1. Use target_buffer to filter correctly and avoid variable shadowing
  df_wide <- results_df |>
    dplyr::filter(buffer_km == target_buffer) |>
    dplyr::select(species, projection, dplyr::all_of(metric_cols)) |>
    tidyr::pivot_wider(
      names_from = projection,
      values_from = dplyr::all_of(metric_cols)
    )

  # 2. Calculate deltas explicitly to bypass dplyr versioning errors
  for (col in metric_cols) {
    wgs_name <- paste0(col, "_WGS84")
    ea_name <- paste0(col, "_EqualArea")
    delta_name <- paste0("delta_", col)

    # Calculate difference: EqualArea - WGS84
    if (wgs_name %in% names(df_wide) && ea_name %in% names(df_wide)) {
      df_wide[[delta_name]] <- df_wide[[ea_name]] - df_wide[[wgs_name]]
    }
  }

  return(df_wide)
}

summarize_class_stability <- function(results_df, baseline_km = 50) {
  class_cols <- c(
    "FCS_ex_class",
    "FCS_in_class",
    "FCS_min_class",
    "FCS_max_class",
    "FCS_mean_class"
  )

  # 1. Isolate the baseline data (e.g., buffer = 50km)
  baseline <- results_df |>
    dplyr::filter(buffer_km == baseline_km) |>
    dplyr::select(species, projection, dplyr::all_of(class_cols)) |>
    dplyr::rename_with(~ paste0(.x, "_baseline"), dplyr::all_of(class_cols))

  # 2. Join the baseline data to the full dataset
  joined_df <- results_df |>
    dplyr::left_join(baseline, by = c("species", "projection"))

  # 3. Calculate the True/False change flag explicitly to avoid cur_column() errors
  for (col in class_cols) {
    base_col <- paste0(col, "_baseline")
    change_col <- paste0(col, "_changed")
    # TRUE if the class is different from the baseline, FALSE if it matches
    joined_df[[change_col]] <- joined_df[[col]] != joined_df[[base_col]]
  }

  # 4. Summarize the unique counts and the mean of our new change flags
  joined_df |>
    dplyr::group_by(species, projection) |>
    dplyr::summarise(
      # Count unique classes
      dplyr::across(
        dplyr::all_of(class_cols),
        ~ dplyr::n_distinct(.x, na.rm = TRUE),
        .names = "{.col}_unique_classes"
      ),
      # Calculate the change rate (% of time it changed from baseline)
      dplyr::across(
        dplyr::ends_with("_changed"),
        ~ mean(.x, na.rm = TRUE),
        .names = "{.col}_rate"
      ),
      .groups = "drop"
    ) |>
    # Clean up names so they match your original expected output (e.g., FCS_ex_class_change_rate)
    dplyr::rename_with(
      ~ sub("_changed_rate$", "_change_rate", .x),
      dplyr::ends_with("_changed_rate")
    )
}
# Execute summaries on the full dataset
proj_consistency <- summarize_projection_consistency(
  final_raw_results_df,
  buffer_km = 50
)
buffer_sensitivity <- summarize_buffer_sensitivity(final_raw_results_df)
class_stability <- summarize_class_stability(
  final_raw_results_df,
  baseline_km = 50
)

message("Run complete.")
