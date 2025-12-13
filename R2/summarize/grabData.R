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
  # crop protected areas
  # mask protected areas layer
  if (class(thres) == "SpatRaster") {
    p2 <- thres
    p2[p2 == 0] <- NA
    # crop protected areas raster
    p1 <- terra::crop(x = protectedAreas, y = p2)
    # multiple to create mask
    p1 <- p1 * p2

    # some reprjecting issues are causing some visualization problems in the htmls
    # project all the raster objects to EPSG:3857
    # do not project the vector objects.
    p1 <- terra::project(x = p1, y = "epsg:3857", method = "max")
    if (class(g_buffer) != "character") {
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
      names <- read_csv(
        "data/geospatial_datasets/bioclim_layers/variableNames_072025.csv"
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
      ersex = ersex,
      ersin = ersin,
      fcsCombined = fcsCombined,
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
      ersex = NA,
      ersin = NA,
      fcsCombined = fcsCombined,
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
