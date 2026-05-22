#' Generate Results Rasters (Memory Optimized with Safety Check)
#'
#' @param sdm_result Dataframe or list containing the SDM output.
#' @param out_dir Character. Path to directory to save rasters.
#'
#' @return A list of terra SpatRaster objects, or NULL if projections are missing
#' @export
rasterResults <- function(sdm_result, out_dir = "output_rasters") {
  # SAFETY CHECK: Did the SDM actually produce projections for this species?
  if (
    is.null(sdm_result$do.projections) || length(sdm_result$do.projections) == 0
  ) {
    message(
      "No projections found in sdm_result for this species. Skipping raster generation."
    )
    return(NULL)
  }

  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE)
  }

  # 1. Safely extract the list of raster::RasterLayer objects
  proj_list <- sdm_result$do.projections

  # 2. Convert each RasterLayer to a SpatRaster individually
  spat_list <- lapply(proj_list, terra::rast)

  # 3. Stack the SpatRasters together into a multi-layer terra object
  prj_stk <- terra::rast(spat_list)

  # 4. Set terra to process conservatively
  terra::terraOptions(memfrac = 0.5)

  # 5. Calculate and write the heavy summary rasters directly to disk
  mean_rst <- terra::mean(
    prj_stk,
    filename = file.path(out_dir, "mean.tif"),
    overwrite = TRUE
  )
  median_rst <- terra::median(
    prj_stk,
    filename = file.path(out_dir, "median.tif"),
    overwrite = TRUE
  )
  stdev_rst <- terra::stdev(
    prj_stk,
    filename = file.path(out_dir, "stdev.tif"),
    overwrite = TRUE
  )

  # 6. Return the objects (mean, median, stdev are now backed by your hard drive)
  rasters <- list(
    all = prj_stk,
    mean = mean_rst,
    median = median_rst,
    stdev = stdev_rst
  )

  return(rasters)
}
