# rerun
pacman::p_load(
  terra,
  dplyr,
  readr,
  leaflet,
  remotes,
  furrr,
  future
)

# Set your parallel plan.
plan(multisession, workers = 12)
# -------------------------

# sourcing directly for the file storage for the most to date gapR functions
list.files("~/trueNAS/work/GapAnalysis/R", full.names = TRUE) |>
  lapply(FUN = source)

## ecoregions
ecoregions_path <- "data/geospatial_datasets/ecoregions/tnc_terr_ecoregions.gpkg"
## protect lands
protectedAreas_path <- "data/geospatial_datasets/protectedLands/wdpa_1km_all_.tif"

# gather data
runVersion <- "run08282025_1k"
files <- list.files(path = "data/Vitis", full.names = TRUE, recursive = TRUE)
r1 <- files[grepl(pattern = runVersion, x = files)]

# load in all rasters
r2 <- r1[grepl(pattern = "prj_threshold.tif", x = r1)]
# load in data for species
s2 <- r1[grepl(pattern = "spatialData.gpkg", x = r1)]

speciesData <- read_csv("temp/allVitisData082025.csv")
species <- sort(unique(speciesData$taxon))

# org for gap r
occurrenceData1 <- speciesData |>
  dplyr::select(
    species = taxon,
    latitude,
    longitude,
    type
  )

# processing function for parallalzation
process_species_gaps <- function(
  i,
  bs,
  r2_files,
  s2_files,
  occurrenceData1,
  ecoregions_path,
  protectedAreas_path
) {
  # --- Load large spatial objects *inside* the worker ---
  # This is the standard, robust way to handle this in parallel
  # important note and a standard to work toward going forward
  ecoregions <- terra::vect(ecoregions_path)
  protectedAreas <- terra::rast(protectedAreas_path)
  # ---

  export <- paste0("data/Vitis/varBuffer/", i, "_", bs, "includePoints.csv")

  if (!file.exists(export)) {
    # print(i) # Printing in parallel can be messy, use with caution

    rs <- r2_files[grepl(pattern = paste0(i, "/"), x = r2_files)]

    # If no raster, return NULL. future_map_dfr will skip it.
    if (length(rs) == 0) {
      return(NULL)
    }

    sdm <- rast(rs)
    sdm[sdm == 0, ] <- NA
    p1 <- sf::st_read(s2_files[grepl(pattern = paste0(i, "/"), x = s2_files)])
    occurrenceData <- p1 |>
      as.data.frame() |>
      dplyr::select(
        species = taxon,
        latitude,
        longitude,
        type
      )

    srs_exsitu1 <- SRSex(
      taxon = i,
      occurrence_Data = occurrenceData1
    )

    gBuffer <- generateGBuffers(
      taxon = i,
      occurrenceData = occurrenceData,
      bufferDistM = bs
    )

    grs_exsitu1 <- GRSex(taxon = i, sdm = sdm, gBuffer = gBuffer)

    ers_exsitu1 <- ERSex(
      taxon = i,
      sdm = sdm,
      occurrenceData = occurrenceData,
      gBuffer = gBuffer,
      ecoregions = ecoregions,
      idColumn = "ECO_ID_U",
      limitByPoints = TRUE
    )

    fcs_exsitu1 <- FCSex(
      taxon = i,
      srsex = srs_exsitu1,
      grsex = grs_exsitu1,
      ersex = ers_exsitu1
    )

    srs_insitu1 <- SRSin(
      taxon = i,
      sdm = sdm,
      occurrenceData = occurrenceData,
      protectedAreas = protectedAreas
    )

    grs_insitu1 <- GRSin(
      taxon = i,
      sdm = sdm,
      protectedAreas = protectedAreas
    )

    ers_insitu1 <- ERSin(
      taxon = i,
      sdm = sdm,
      occurrenceData = occurrenceData,
      protectedAreas = protectedAreas,
      ecoregions = ecoregions,
      idColumn = "ECO_CODE",
      limitByPoints = TRUE
    )

    fcs_insitu1 <- FCSin(
      taxon = i,
      srsin = srs_insitu1,
      grsin = grs_insitu1,
      ersin = ers_insitu1
    )

    fcs_combine1 <- FCSc_mean(
      taxon = i,
      fcsin = fcs_insitu1,
      fcsex = fcs_exsitu1
    )

    write_csv(x = fcs_combine1, file = export)
  } else {
    # If file exists, just read it
    fcs_combine1 <- read_csv(export)
  }

  # Return the data frame for this species
  return(fcs_combine1)
}


# ---------------------------------------------------------------------
# --- Main outer loop (still serial) ---
# ---------------------------------------------------------------------

for (bs in c(10000, 50000, 100000)) {
  # --- REPLACED: The inner for-loop is now future_map_dfr ---
  #
  # future_map_dfr runs the function on each element of 'species'
  # (in parallel) and combines all the resulting data frames
  # using dplyr::bind_rows() at the end.

  output <- furrr::future_map_dfr(
    .x = species,
    .f = process_species_gaps,
    # --- Pass all other arguments needed by the function ---
    bs = bs,
    r2_files = r2,
    s2_files = s2,
    occurrenceData1 = occurrenceData1,
    ecoregions_path = ecoregions_path,
    protectedAreas_path = protectedAreas_path,
    # --- furrr options ---
    .options = furrr_options(seed = TRUE) # Ensures reproducibility
  )

  write_csv(
    x = output,
    file = paste0("data/Vitis/varBuffer/summaryTable_", bs, "inlcudePoints.csv")
  )
}

# compile exported summary data  -----------------------------------------
gatherSummaries <- function(path) {
  # pull numberic value
  numbers_str <- stringr::str_match(path, "_([0-9e\\+\\.-]+)inlcudePoints")[, 2]
  # read in data and assign buffDist
  r1 <- readr::read_csv(path) |>
    dplyr::mutate(buffer_Distance = as.numeric(numbers_str))
  return(r1)
}
# list file paths
summaryFiles <- list.files(
  "data/Vitis/varBuffer",
  pattern = "summaryTable",
  full.names = TRUE
)

# apply funciton
dfs <- purrr::map(.x = summaryFiles, .f = gatherSummaries) |>
  dplyr::bind_rows()
# export
readr::write_csv(
  x = dfs,
  file = "data/Vitis/varBuffer/allDistanceVitisBuffer.csv"
)
