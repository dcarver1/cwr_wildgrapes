#' Grab Data for the summary html documents
#'
#' @return RDS file with a nested list of all required file inputs
grabData <- function(
    ersex,
    ersin,
    fscCombined,
    fcsex,
    fcsin,
    evalTable,
    aucMetrics,
    g_bufferCrop,
    thres,
    projectsResults,
    v_data,
    g_buffer,
    natArea,
    protectedAreas,
    occuranceData,
    countsData,
    variableImportance,
    NoModel,
    modelDataCounts
) {
  
  # Sanitize missingEcos strings (which often arrive as semicolon-separated strings) 
  # into lists so the Rmd's unlist() calls can properly parse them into vectors.
  # This prevents 0-row ecoregion tables that crash leaflet with NULL attributes.
  clean_ecos <- function(df) {
    if (!is.null(df) && "missingEcos" %in% names(df)) {
      if (is.character(df$missingEcos) && length(df$missingEcos) == 1) {
        df$missingEcos <- list(strsplit(as.character(df$missingEcos), ";")[[1]])
      }
    }
    return(df)
  }
  ersex <- clean_ecos(ersex)
  ersin <- clean_ecos(ersin)
  
  # crop protected areas
  # mask protected areas layer
  if (inherits(thres, "SpatRaster")) {
    p2 <- thres
    p2[p2 == 0] <- NA
    # crop protected areas raster
    p1 <- terra::crop(x = protectedAreas, y = p2)
    # multiple to create mask
    p1 <- p1 * p2
    
    # -------------------------------------------------------------------------
    # Dynamic Resampling Logic:
    # Evaluate number of active cells in the 1km thres raster to prevent 
    # memory overflow and visual rendering crashes.
    # -------------------------------------------------------------------------
    total_active_cells <- as.numeric(terra::global(thres, "sum", na.rm = TRUE))
    if (total_active_cells > 500000) {
      resampleFactor <- 5
      thres <- terra::aggregate(thres, fact = resampleFactor, fun = "max")
      p1 <- terra::aggregate(p1, fact = resampleFactor, fun = "max")
      
      if (!is.character(g_buffer)) {
        g_buffer <- terra::aggregate(g_buffer, fact = resampleFactor, fun = "max")
        g_bufferCrop <- terra::aggregate(g_bufferCrop, fact = resampleFactor, fun = "max")
      }
      if (length(projectsResults) > 1) {
        projectsResults <- purrr::map(projectsResults, terra::aggregate, fact = resampleFactor, fun = "mean")
      }
    }
    # -------------------------------------------------------------------------
    
    # some reprjecting issues are causing some visualization problems in the htmls
    # project all the raster objects to EPSG:3857
    # do not project the vector objects.
    p1 <- terra::project(x = p1, y = "epsg:3857", method = "max")
    if (!is.character(g_buffer)) {
      g_buffer <- terra::project(x = g_buffer, y = "epsg:3857", method = "near")
      g_bufferCrop <- terra::project(
        x = g_bufferCrop,
        y = "epsg:3857",
        method = "near"
      )
    }
    if (length(projectsResults) > 1) {
      projectsResults <- projectsResults |>
        purrr::map(terra::project, y = "epsg:3857", method = "near")
    }
    thres <- terra::project(x = thres, y = "epsg:3857", method = "near")
    
    # add variable importance data
    if (!is.na(variableImportance)) {
      var1 <- readRDS(variableImportance) #$rankPredictors
      # Added show_col_types = FALSE to silence the stdout column spec messages
      names <- readr::read_csv(
        "data/geospatial_datasets/bioclim_layers/variableNames_072025.csv",
        show_col_types = FALSE 
      )
      variableImportance <- var1$rankPredictors |>
        dplyr::left_join(y = names, by = c("varNames" = "vitisModelNames"))
    }
    
    # bind to export object
    reportData <- list(
      occuranceData = occuranceData,
      naturalArea = natArea,
      model_Occurances = v_data,
      protectedArea = p1,
      g_buffer = g_buffer,
      g_bufferCrop = g_bufferCrop,
      projectedResults = projectsResults,
      binaryMap = thres,
      modelEvaluation = evalTable,
      aucMetrics = aucMetrics,
      ersex = ersex,
      ersin = ersin,
      fcsCombined = fscCombined,
      fcsex = fcsex,
      fcsin = fcsin,
      countsData = countsData,
      variableImportance = variableImportance,
      NoModel = NoModel,
      modelDataCounts = modelDataCounts
    )
  } else {
    reportData <- list(
      occuranceData = occuranceData,
      naturalArea = natArea,
      model_Occurances = NA,
      protectedArea = NA,
      g_buffer = NA,
      g_bufferCrop = NA,
      projectedResults = NA,
      binaryMap = NA,
      modelEvaluation = NA,
      aucMetrics = NA,
      ersex = ersex,
      ersin = ersin,
      fcsCombined = fscCombined,
      fcsex = fcsex,
      fcsin = fcsin,
      countsData = countsData,
      variableImportance = NA,
      NoModel = NoModel,
      modelDataCounts = NA
    )
  }
  
  return(reportData)
}