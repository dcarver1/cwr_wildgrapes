###
# global.R
# Environment setup, library loading, and static asset loading
###

# 1. Package Management
pacman::p_load(
  "dplyr",
  "sf",
  "terra",
  "purrr",
  "randomForest",
  "VSURF",
  "modelr",
  "maxnet",
  "pROC",
  "DT",
  "readr",
  "vroom",
  "dismo",
  "leaflet",
  "tidyterra",
  "rmarkdown",
  "furrr",
  "stringr",
  "spThin",
  "tictoc",
  "tigris",
  "tmap",
  "ggplot2",
  "plotly",
  "factoextra",
  "tidyr",
  "rnaturalearth"
)

# 2. Environment Settings
tmap::tmap_mode("view")
set.seed(1234)
bufferDist <- 50000

# 3. Source Helper Functions
source("R2/helperFunctions.R")
sourceFiles(gapAnalysisOnly = FALSE)
source("temp/clearNewErrors.R")

# 4. Load Heavy Static Geospatial Assets
message("Loading static geospatial assets...")

# Download the states and provinces for North America
naStates <- rnaturalearth::ne_states(
  country = c("mexico", "canada", "united states of america"),
  returnclass = "sf"
) |>
  dplyr::select(name, adm0_a3)

# Read in bioclim layers
bioVars <- readRDS("data/geospatial_datasets/bioclim_layers/bioVar_1km.RDS")
templateRast <- bioVars[[1]]
bioNames <- read_csv(
  "data/geospatial_datasets/bioclim_layers/variableNames_072025.csv"
)

# Ecoregions
ecoregions <- sf::st_read(
  "data/geospatial_datasets/ecoregions/tnc_terr_ecoregions.gpkg",
  quiet = TRUE
)

# Protected lands
protectedAreas <- terra::rast(
  "data/geospatial_datasets/protectedLands/wdpa_1km_all_.tif"
)

# FNA Data (used across scripts for filtering)
fnaData <- read_csv("data/source_data/FNA_stateClassification.csv")
