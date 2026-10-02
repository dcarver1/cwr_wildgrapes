# preprocessing/preprocessingUpdates2026_08_20.R
# Altered workflow using process_gbif_082026.R, saving outputs under today's date (08202026)

pacman::p_load(
  dplyr,
  readr,
  sf,
  terra,
  rgbif,
  googledrive,
  countrycode,
  stringr,
  purrr,
  tidyr
)

# -------------------------------------------------------------------------
# Step 1: Load Project Taxonomy Reference & Supporting Functions
# -------------------------------------------------------------------------
# Pull the live taxonomy sheet first: edits made there replace the local copy
# (previous copy backed up, snapshot and change log written) and so reach the
# model input built below. Preset syncTaxonomy <- FALSE to build from the
# local copy as it stands.
source("preprocessing/functions/syncTaxonomySheet.R")
if (!exists("syncTaxonomy") || isTRUE(syncTaxonomy)) syncTaxonomySheet(update = TRUE)
vitis2 <- read_csv("data/New World Vitis.csv") |>
  dplyr::select(
    "taxon" = "Scientific Name",
    "acceptedSynonym" = "Names to include in this concept (Homotypic synonyms)",
    "excludeNames" = "Names to exclude from this concept",
    "modelSpecies" = "Include in gap analysis?"
  )

# Sourcing original functions
source("preprocessing/functions/preprocessing07_2025Functions.R")
source("preprocessing/functions/helperFunctions.R")
source("preprocessing/functions/process_bg.R")

# CRITICAL CHANGE: Source the custom taxonomic-preservation parser
source("preprocessing/functions/process_gbif_082026.R")

# -------------------------------------------------------------------------
# Step 2: Global Configuration & Variables
# -------------------------------------------------------------------------
# Suffix = date of the taxonomy sheet pull the run is built from
# (data/New World Vitis.csv). 20261002: V. novomexicana added to acerifolia
# and V. coriacea to shuttleworthii. The previous build is
# model_data20260820_taxonomyOnly.csv.
# Coordinate failures are handled as in the published dataset: records that
# fail the Americas bounding box stay in the model data.
date_suffix <- "10022026"
final_date_suffix <- "20261002"

standardColumnNames <- c(
  "taxon", "originalTaxon", "genus", "species", "latitude", "longitude",
  "databaseSource", "institutionCode", "type", "sourceUniqueID",
  "sampleCategory", "country", "iso3", "localityInformation",
  "biologicalStatus", "collectionSource", "finalOriginStat", "yearRecorded",
  "county", "countyFIPS", "state", "stateFIPS", "coordinateUncertainty",
  "observerName", "recordID"
)

# -------------------------------------------------------------------------
# Step 3: Run Parsers with New Date-Suffixed Outputs
# -------------------------------------------------------------------------

# 1. Process GBIF (Using the new unlumped processGBIF from process_gbif_082026.R)
# nameSource = "combined": parse GBIF's interpreted scientificName first; if that
# cannot be parsed (GBIF fell back to "Vitis L."), parse the publisher's
# verbatimScientificName; only then use the backbone columns. Recommended in
# work2026/GBIF_taxonomy_notes.md section 4 (+138 records over "scientificName").
# homonymLog: authored name strings that keep the backbone assignment (see
# process_gbif_082026.R step 2b)
gbif <- processGBIF(path = "data/source_data/vitisGBIFDownload_20250721.csv",
                    nameSource = "combined",
                    homonymLog = paste0("data/processed_occurrence/gbifHomonymNames_", date_suffix, ".csv")) |>
  orderNames(names = standardColumnNames) |>
  removeDuplicatesID()
write_csv(x = gbif, file = paste0("data/processed_occurrence/gbif_", date_suffix, ".csv"))

# 2. Process Jun Wen's Mexico dataset
jun <- processJun() |>
  orderNames(names = standardColumnNames) |>
  removeDuplicatesID()

# Add single special Vitis bloodworthiana record
single <- data.frame(matrix(nrow = 1, ncol = length(standardColumnNames)))
names(single) <- standardColumnNames
single$taxon <- "Vitis bloodworthiana"
single$observerName <- "Facultad de Estudios Superiores Iztacala, UNAM, (FESI-UNAM), Mexico"
single$originalTaxon <- "Vitis bloodworthiana"
single$genus <- "Vitis"
single$species <- "bloodworthiana"
single$type <- "G"
single$latitude <- "18.00"
single$longitude <- "-100.00"
single$localityInformation <- "Generalized lat lon per curator's request"
single$databaseSource <- "MBG"
single$institutionCode <- "MBG"
jun <- bind_rows(jun, single)
write_csv(x = jun, file = paste0("data/processed_occurrence/jun_", date_suffix, ".csv"))

# 3. Process Mexico Records
mex <- processMex()
write_csv(x = mex, file = paste0("data/processed_occurrence/mexicoRecords_", date_suffix, ".csv"))

# 4. Process Botanical Garden dataset
bg1 <- processBG(path = "data/source_data/bg_survey.csv") |>
  orderNames(names = standardColumnNames) |>
  removeDuplicatesID()
write_csv(x = bg1, file = paste0("data/processed_occurrence/botanicalGardenSurvey_", date_suffix, ".csv"))

# 5. Process Genesys dataset
gen <- processGenesysUpdate(path = "data/source_data/GenesysPGR_Vitis_subset.csv") |>
  orderNames(names = standardColumnNames) |>
  removeDuplicatesID()
write_csv(x = gen, file = paste0("data/processed_occurrence/genesys_", date_suffix, ".csv"))


# -------------------------------------------------------------------------
# Step 4: Parameterized readAndBind for Date-Suffixed Files
# -------------------------------------------------------------------------
readAndBind_08202026 <- function() {
  # Reads newly-generated date-suffixed files
  gbif <- read_csv(paste0("data/processed_occurrence/gbif_", date_suffix, ".csv"), col_types = cols(.default = "c"))
  grin <- read_csv("data/processed_occurrence/grin.csv", col_types = cols(.default = "c"))
  wiews <- read_csv("data/processed_occurrence/wiews.csv", col_types = cols(.default = "c"))
  genesys <- read_csv(paste0("data/processed_occurrence/genesys_", date_suffix, ".csv"), col_types = cols(.default = "c"))
  bgSurvey <- read_csv(paste0("data/processed_occurrence/botanicalGardenSurvey_", date_suffix, ".csv"), col_types = cols(.default = "c"))
  ucdavis <- read_csv("data/processed_occurrence/UCDavis.csv", col_types = cols(.default = "c"))
  bonap <- read_csv("data/processed_occurrence/bonap.csv", col_types = cols(.default = "c"))
  pnas2020 <- read_csv("data/processed_occurrence/pnas2020.csv", col_types = cols(.default = "c"))
  jun <- read_csv(paste0("data/processed_occurrence/jun_", date_suffix, ".csv"), col_types = cols(.default = "c"))
  mex <- read_csv(paste0("data/processed_occurrence/mexicoRecords_", date_suffix, ".csv"), col_types = cols(.default = "c"))

  d2 <- bind_rows(
    gbif, grin, wiews, genesys, bgSurvey, pnas2020, ucdavis, jun, mex
  ) |>
    dplyr::mutate(index = dplyr::row_number()) |>
    dplyr::select(index, everything())
  
  return(d2)
}

# Bind all active sources
df <- readAndBind_08202026() |>
  dplyr::select(-`Collecting number`)

# Drop obsolete 2019 versions of databases to prevent historical duplication
df <- df |>
  dplyr::filter(
    !databaseSource %in%
      c(
        "FAO 2019 (WIEWS)",
        "GBIF 2019",
        "Global Crop Diversity Trust 2019a (Genesys)",
        "Global Crop Diversity Trust 2019b  (Cwr Occ)",
        "USDA ARS NPGS 2019a",
        "Midwest Herbaria 2019",
        "BGCI 2019 (PlantSearch)"
      )
  )

# -------------------------------------------------------------------------
# Step 5: Standardization, Synonyms, and Geochecks
# -------------------------------------------------------------------------
source("preprocessing/functions/standardizeNames.R")
df1 <- standardizeNames(df)

source("preprocessing/functions/speciesStandardization.R")
datasets <- speciesCheck(data = df1, synonymList = vitis2)
df2 <- datasets$includedData
# records dropped because their source name is on a concept's "Names to exclude" list
write_csv(datasets$excludedByConcept,
          paste0("data/processed_occurrence/excludedByConcept_", date_suffix, ".csv"))
# records whose name matched no concept or synonym
write_csv(datasets$excludedData,
          paste0("data/processed_occurrence/excludedOnTaxonomy_", date_suffix, ".csv"))

# Keep Vitis novogranatensis as a special-case manually added/passed taxon
novogranatensis <- df2[df2$taxon == "Vitis novogranatensis", ]

# Remove duplicates across overlapping databases
uniqueTaxon <- unique(df2$taxon)
source("preprocessing/functions/removeDupsAcrossDatasets.R")
df2_a <- uniqueTaxon |>
  purrr::map(.f = removeDups, data = df2) |>
  bind_rows()

# Re-inject the novogranatensis records back in
df2_a <- bind_rows(df2_a, novogranatensis)

# Quality Checks on Spatial Coordinates (Lat/Long Bounding Box)
source("preprocessing/functions/checksOnLatLong.R")
# tiliifolia / popenoei keep their Central American and Caribbean records, as
# the published dataset did (see checksOnLatLong.R); every other taxon is
# limited to USA / CAN / MEX / blank as before.
d3 <- checksOnLatLong(df2, exemptTaxa = c("Vitis tiliifolia", "Vitis popenoei"))
valLatLon <- d3$validLatLon

# Records with no coordinates and records that failed the Americas bounding
# box both stay in the model data, as in the published dataset.
d3_g <- d3$countycheck |>
  dplyr::mutate(
    county = stringr::str_remove_all(string = county, pattern = " .Co"),
    county = stringr::str_remove_all(string = county, pattern = " Co."),
    county = case_when(
      grepl("County", county) ~ county,
      is.na(county) ~ NA,
      TRUE ~ paste0(county, " County")
    )
  )

# Recombine valid spatial coordinates and G-record county check records
d6 <- valLatLon |> bind_rows(d3_g)

# Perform final duplicate verification on matching database sources & IDs
s2 <- d6[!is.na(d6$sourceUniqueID), ]
s3 <- d6[is.na(d6$sourceUniqueID), ]
d <- s2[!duplicated(s2[, c("taxon", "databaseSource", "sourceUniqueID")]), ]
d7 <- bind_rows(d, s3)

d8 <- d7 |>
  dplyr::filter(!is.na(taxon))

# Drop temporary columns and perform full row-wise duplication checks
t2 <- d8 |>
  dplyr::select(-c(index, recordID, validLat, validLon, validLatLon)) 
all_duplicates <- duplicated(t2) | duplicated(t2, fromLast = TRUE)
t3 <- t2[!all_duplicates, ]

# Filter and clean up distinct/overlapping databases (e.g. UC Davis and Huerta-Acosta)
d9 <- t3 |>
  dplyr::filter(databaseSource %in% c("Huerta-Acosta publication", "UC Davis Grape Breeding Collection")) |>
  dplyr::distinct(taxon, sourceUniqueID, .keep_all = TRUE)

d10 <- t3 |>
  dplyr::filter(!databaseSource %in% c("Huerta-Acosta publication", "UC Davis Grape Breeding Collection"))

d11 <- bind_rows(d9, d10) |>
  dplyr::arrange(taxon) |>
  dplyr::mutate(index = row_number())

# Quality filtering: remove extreme lat/lon outlier records
d11a <- d11 |> dplyr::filter(latitude == 82.233333) |> select(index)
d11b <- d11 |> dplyr::filter(longitude == -177.2805) |> select(index)

# `!index %in%` is safe when a filter matches zero or several rows
# (`index != d11a$index` errors on zero matches and recycles on several)
d11 <- d11 |>
  dplyr::filter(!index %in% d11a$index) |>
  dplyr::filter(!index %in% d11b$index)

# Update database source tag for Vitis novogranatensis
d11[d11$taxon == "Vitis novogranatensis", "databaseSource"] <- "Personal Communication with Jun Wen"

# One-off removals from the summary-map review that fed the published run
# (run_all072025.R applied them from temp/clearNewErrors.R; the function now
# lives in preprocessing/functions/ so the model data carries them). Needs the
# `index` column assigned above. Removes: cinerea lat 0 (13), palmata lat 0 or
# 26.7333 (8), riparia lat 0 (6), shuttleworthii lon > -28 (5), vulpina lon 0
# (86), one labrusca record; flips the latitude sign of one peninsularis record.
source("preprocessing/functions/clearNewErrors.R")
nBefore <- nrow(d11)
d11 <- clearNewErrors(data = d11)
message(nBefore - nrow(d11), " records removed by clearNewErrors()")

# -------------------------------------------------------------------------
# Step 6: Export Final Clean Dataset
# -------------------------------------------------------------------------
write_csv(x = d11, file = paste0("data/processed_occurrence/model_data", final_date_suffix, ".csv"))

message(paste0("Preprocessing successfully drafted and compiled! Final dataset: data/processed_occurrence/model_data", final_date_suffix, ".csv"))
