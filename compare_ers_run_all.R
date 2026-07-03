###
# compare_ers_run_all.R
# Adapted ERS Comparative Script for Vitis Species
#
# This script loads previously generated publication outputs (cached via the write_* helpers
# when overwrite = FALSE) and calculates multiple ERS metrics side-by-side:
#  - ERS Ex-Situ 2024 (unmasked, polygon-based)
#  - ERS Ex-Situ 2026 (masked to SDM, grouped by unique ID, limitByPoints = FALSE)
#  - ERS Ex-Situ 2026 (masked to SDM, grouped by unique ID, limitByPoints = TRUE)
#  - ERS In-Situ 2024 (uncropped ecoregions, polygon-based)
#  - ERS In-Situ 2026 (cropped ecoregions, polygon-based, limitByPoints = FALSE)
#  - ERS In-Situ 2026 (cropped ecoregions, polygon-based, limitByPoints = TRUE)
#  - ERS In-Situ 2026 (Fixed unique ID-grouped method)
###

# 1. Load global environment and assets from your existing workflow
source("global.R")

# 2. Source comparison ERS functions
# Load 2024 functions (from gapAnalysis_vitis directory)
source("R2/gapAnalysis/ers_exsitu.R")
source("R2/gapAnalysis/ers_insitu.R")
# soruce version with the point limitations 
source("ERSex.R")
source("ERSin.R")


# Load 2026 GAP-R package functions
# If the package is installed in your library, use library(GapAnalysis)
# Otherwise, we source the files from the GapAnalysis_library directory
# if (requireNamespace("GapAnalysis", quietly = TRUE)) {
#   library(GapAnalysis)
# } else {
#   source("GapAnalysis_library/ERSex.R")
#   source("GapAnalysis_library/ERSin.R")
# }

# Convert standard ecoregions sf object to terra SpatVector for 2026 functions
ecoregions_vect <- terra::vect(ecoregions)

# 3. Parameters (match your run Version)
runVersion <- "run08282025_1k"
overwrite <- FALSE  # Essential: FALSE ensures we load cached data without rebuilding SDMs
dir1 <- "data/Vitis"

# 4. Load Occurrence Database for 2026 functions
allDataPath <- "data/datasetsForPublication/allSpeciesOccurrences.csv"
if (!file.exists(allDataPath)) {
  stop("Clean dataset not found. Please verify the allSpeciesOccurrences.csv path.")
}
speciesData <- read_csv(allDataPath, show_col_types = FALSE)
species <- sort(unique(speciesData$taxon))

# 5. Define target species to test
# You can change r3 to check specific species (e.g. r3 <- c("Vitis shuttleworthii"))
# Or run on all modeled species with: r3 <- species
r3 <- c("Vitis arizonica", "Vitis monticola", "Vitis rupestris") 

# Dataframe to accumulate comparative results
comparison_report <- data.frame()

# 6. Main Comparison Loop
for (j in r3) {
  print(paste("--------------------------------------------------"))
  print(paste("Comparing ERS metrics for:", j))
  
  allPaths <- definePaths(dir1 = dir1, j = j, runVersion = runVersion)
  
  # Check if threshold model exists (i.e. model was successfully completed)
  if (!file.exists(allPaths$thresPath)) {
    warning(paste("No threshold model found for", j, "- Skipping comparison."))
    next
  }
  
  # A. LOAD CACHED 2024 OUTPUTS DIRECTLY FROM THE WORKSPACE
  # (These will match your established publication data exactly)
  # Loading as sf objects (using sf::read_sf) to match the expected classes of 2024 functions
  sp1 <- sf::read_sf(allPaths$spatialDataPath)
  natArea <- sf::read_sf(allPaths$natAreaPath)
  thres <- terra::rast(allPaths$thresPath)
  
  if (file.exists(allPaths$g50_bufferPath)) {
    g_bufferCrop <- terra::rast(allPaths$g50_bufferPath)
  } else {
    g_bufferCrop <- NULL
  }
  
  # Species-specific occurrence subset for 2026 functions (with taxonomic alignments)
  # To ensure exact comparison with 2024 results, we use the points from sp1
  sd1 <- sp1 |>
    sf::st_drop_geometry()
  
  # Add coordinates ensuring we don't duplicate existing latitude/longitude columns
  coords <- sf::st_coordinates(sp1) |> as.data.frame()
  sd1$longitude <- coords$X
  sd1$latitude <- coords$Y
  
  # B. COMPUTE EX-SITU METRICS
  ers_ex_24 <- NA
  ers_ex_26 <- NA
  
  if (!is.null(g_bufferCrop)) {
    # 2024 Ex-Situ
    ers_ex_24_res <- ers_exsitu(
      speciesData = sp1,
      thres = thres,
      natArea = natArea,
      ga50 = g_bufferCrop,
      rasterPath = NULL
    )
    ers_ex_24 <- ers_ex_24_res$ERS[1]
    
    # Prepare 2026 G-buffer vector wrapper by polygonizing cached cropped G-buffer raster
    gBuffer_2026 <- list(data = terra::as.polygons(g_bufferCrop))
    
    # 2026 Ex-Situ (CRAN version without limitByPoints)
    ers_ex_26_res <- ERSex(
      taxon = j,
      sdm = thres,
      occurrenceData = sd1,
      gBuffer = gBuffer_2026,
      ecoregions = ecoregions_vect,
      idColumn = "ECO_ID_U",
      limitByPoints = TRUE
    )
    ers_ex_26 <- ers_ex_26_res$results$`ERS exsitu`[1]
  }
  
  # C. COMPUTE IN-SITU METRICS
  # 2024 In-Situ
  ers_in_24_res <- ers_insitu(
    occuranceData = sp1,
    nativeArea = natArea,
    protectedArea = protectedAreas,
    thres = thres,
    rasterPath = NULL
  )
  ers_in_24 <- ers_in_24_res$ERS[1]
  
  # 2026 In-Situ (CRAN version without limitByPoints)
  ers_in_26_res <- ERSin(
    taxon = j,
    sdm = thres,
    occurrenceData = sd1,
    protectedAreas = protectedAreas,
    ecoregions = ecoregions_vect,
    idColumn = "ECO_ID_U",
    limitByPoints = TRUE
  )
  ers_in_26 <- ers_in_26_res$results$`ERS insitu`[1]
  
  # D. COMPUTE FIXED 2026 IN-SITU METRIC
  # (Our proposed fix that groups cropped polygons by unique ecoregion ID before calculation)
  ers_in_26_fixed <- NA
  tryCatch({
    pro <- terra::crop(protectedAreas, thres)
    proMask <- pro * thres
    eco_cropped <- terra::crop(ecoregions_vect, thres)
    
    # Calculate predicted presence zonal sum
    eco_cropped$totEco <- terra::zonal(x = thres, z = eco_cropped, fun = "sum", na.rm = TRUE) |> dplyr::pull()
    selectedEcos <- eco_cropped[eco_cropped$totEco > 0, ]
    
    # Calculate protected presence zonal sum
    eco_cropped$totPro <- terra::zonal(x = proMask, z = eco_cropped, fun = "sum", na.rm = TRUE) |> dplyr::pull()
    protectedEcos <- eco_cropped[eco_cropped$totPro > 0, ]
    
    # Unique ecoregion ID counts (eliminating polygon-segment duplicates)
    unique_ecos_in_sdm <- length(unique(as.data.frame(selectedEcos)$ECO_ID_U))
    unique_ecos_protected <- length(unique(as.data.frame(protectedEcos)$ECO_ID_U))
    
    if (unique_ecos_in_sdm > 0) {
      ers_in_26_fixed <- (unique_ecos_protected / unique_ecos_in_sdm) * 100
    } else {
      ers_in_26_fixed <- 0
    }
  }, error = function(e) {
    ers_in_26_fixed <- NA
  })
  
  # E. PRINT COMPARATIVE SUMMARY FOR CURRENT SPECIES
  print(paste("EX-SITU RESULTS:"))
  print(paste("  - 2024 Ex-Situ:                  ", round(ers_ex_24, 2), "%"))
  print(paste("  - 2026 Ex-Situ:                  ", round(ers_ex_26, 2), "%"))
  print(paste("IN-SITU RESULTS:"))
  print(paste("  - 2024 In-Situ:                  ", round(ers_in_24, 2), "%"))
  print(paste("  - 2026 In-Situ:                  ", round(ers_in_26, 2), "%"))
  print(paste("  - 2026 In-Situ (Proposed Fix):   ", round(ers_in_26_fixed, 2), "%"))
  
  # F. APPEND TO ACCUMULATED DATA
  species_report <- data.frame(
    Taxon = j,
    ERS_Ex_2024 = ers_ex_24,
    ERS_Ex_2026 = ers_ex_26,
    ERS_In_2024 = ers_in_24,
    ERS_In_2026 = ers_in_26,
    ERS_In_2026_Fixed = ers_in_26_fixed
  )
  comparison_report <- rbind(comparison_report, species_report)
}

# Write final comparison file
output_csv_path <- file.path(dir1, paste0("ERS_methods_comparison_", runVersion, ".csv"))
write_csv(comparison_report, output_csv_path)
print(paste("=================================================="))
print(paste("Comparison complete! Detailed CSV written to:", output_csv_path))
