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
  # Load required libraries
  suppressPackageStartupMessages({
    library(dplyr)
    library(terra)
    library(sf)
    library(stringr)
    library(readr)
    library(purrr)
    library(RColorBrewer)
  })

  # Helper for sigfig rounding
  sigfig <- function(vec, n = 3) {
    formatC(signif(vec, digits = n), digits = n, format = "fg", flag = "#")
  }

  # 1. Data QA/QC & Cleaning
  species <- countsData$species[1]
  # Handle hardcoded Vitis bloodworthiana longitude filter
  if (!is.null(species) && species == "Vitis bloodworthiana") {
    occuranceData <- occuranceData[occuranceData$longitude != -100.00000, ]
  }

  # Prep Occurence Data for Leaflet (add popups and styling)
  occData <- occuranceData |>
    dplyr::mutate(
      popup = paste0(
        "<br/><b>Taxon:</b> ",
        taxon,
        "<br/><b>Source:</b> ",
        databaseSource,
        "<br/><b>Collector Code:</b> ",
        institutionCode,
        "<br/><b>Collection Type:</b> ",
        type
      ),
      color = dplyr::case_when(
        type == "H" ~ "#1184D4",
        type == "G" ~ "#6300F0",
        TRUE ~ "#CCCCCC"
      )
    )

  # 2. Table Joins and Categorical Logic

  # Unified Counts Data for Table 1
  c1_a <- countsData |>
    dplyr::select(
      Taxon = species,
      'Occurrences' = totalRecords,
      'Occurrences with Lat/Long' = totalUseful,
      'Germplasm Records(G)' = totalGRecords,
      'Germplasm Records(G) with Lat/Long' = totalGUseful,
      'Reference Records(H)' = totalHRecords,
      'Reference Records(H) with Lat/Long' = totalHUseful,
      'Unique Data Sources' = numberOfUniqueSources
    )

  if (!is.null(modelDataCounts) && !all(is.na(modelDataCounts))) {
    c2 <- modelDataCounts |>
      dplyr::select(
        Taxon = species,
        'Presence Occurrences' = presenceRecords,
        'Background Occurrences' = backgroudRecords,
        'Total Occurrences Used in Model' = totalRecords
      )
    unifiedCounts <- dplyr::left_join(c1_a, c2, by = "Taxon") |>
      dplyr::mutate(
        Taxon = stringr::str_replace_all(
          Taxon,
          pattern = "_",
          replacement = " "
        )
      )
  } else {
    unifiedCounts <- c1_a |>
      dplyr::mutate(
        Taxon = stringr::str_replace_all(
          Taxon,
          pattern = "_",
          replacement = " "
        )
      )
  }

  # Ex situ Scores and Categories
  exScores <- fcsex |>
    dplyr::mutate(
      "SRS ex situ" = as.numeric(sigfig(SRS)),
      "GRS ex situ" = as.numeric(sigfig(GRS)),
      "ERS ex situ" = as.numeric(sigfig(ERS)),
      "FCS ex situ" = as.numeric(sigfig(FCS))
    ) |>
    dplyr::mutate(
      "FCS ex situ priority category" = dplyr::case_when(
        `FCS ex situ` >= 75 ~ "LP",
        `FCS ex situ` >= 50 ~ "MP",
        `FCS ex situ` >= 25 ~ "HP",
        TRUE ~ "UP"
      )
    )

  # In situ Scores and Categories
  inScores <- fcsin |>
    dplyr::mutate(
      "SRS in situ" = as.numeric(sigfig(SRS)),
      "GRS in situ" = as.numeric(sigfig(GRS)),
      "ERS in situ" = as.numeric(sigfig(ERS)),
      "FCS in situ" = as.numeric(sigfig(FCS))
    ) |>
    dplyr::mutate(
      "FCS in situ priority category" = dplyr::case_when(
        `FCS in situ` >= 75 ~ "LP",
        `FCS in situ` >= 50 ~ "MP",
        `FCS in situ` >= 25 ~ "HP",
        TRUE ~ "UP"
      )
    )

  # Combined Conservation Score Calculation
  meanVal <- mean(
    c(exScores$`FCS ex situ`, inScores$`FCS in situ`),
    na.rm = TRUE
  )
  finalConservationTable <- fscCombined |>
    dplyr::mutate(
      Taxon = stringr::str_replace_all(ID, pattern = "_", replacement = " ")
    ) |>
    dplyr::mutate(across(where(is.numeric), \(x) round(x, 2))) |>
    dplyr::mutate(
      `Final Conservation Score Mean` = round(meanVal, 2),
      `Combined Conservation Priority` = dplyr::case_when(
        meanVal >= 75 ~ "LP",
        meanVal >= 50 ~ "MP",
        meanVal >= 25 ~ "HP",
        TRUE ~ "UP"
      )
    )

  # 3. Dynamic Resolution & Master Masking (The 5km Fix)
  resampleFactor <- 1
  if (inherits(thres, "SpatRaster") && NoModel == FALSE) {
    # Evaluate number of active cells in the 1km thres raster
    total_active_cells <- as.numeric(terra::global(thres, "sum", na.rm = TRUE))

    if (total_active_cells > 500000) {
      resampleFactor <- 5
      # Aggregate threshold to 5km using fun = "max" to act as Master Mask
      thres <- terra::aggregate(thres, fact = resampleFactor, fun = "max")
    }

    # Define Masks
    habitatMask <- terra::ifel(thres == 1, 1, NA)
    masterMask <- terra::ifel(thres >= 0, 1, NA) # Native area + habitat

    # Project to EPSG:3857 for Leaflet
    thres <- terra::project(thres, "epsg:3857", method = "near")
    masterMask <- terra::project(masterMask, "epsg:3857", method = "near")
    habitatMask <- terra::project(habitatMask, "epsg:3857", method = "near")

    # Resample and Mask all other necessary rasters to prevent bleeding
    p1 <- terra::project(protectedAreas, "epsg:3857", method = "max") |>
      terra::crop(thres)
    p1 <- terra::resample(p1, thres, method = "max") |> terra::mask(habitatMask)

    if (inherits(g_buffer, "SpatRaster")) {
      g_buffer <- terra::project(g_buffer, "epsg:3857", method = "near")
      g_buffer <- terra::resample(g_buffer, thres, method = "near") |>
        terra::mask(habitatMask)
    }
    if (inherits(g_bufferCrop, "SpatRaster")) {
      g_bufferCrop <- terra::project(g_bufferCrop, "epsg:3857", method = "near")
      g_bufferCrop <- terra::resample(g_bufferCrop, thres, method = "near") |>
        terra::mask(habitatMask)
    }

    if (length(projectsResults) > 1) {
      projectsResults <- purrr::map(projectsResults, function(r) {
        rp <- terra::project(r, "epsg:3857", method = "near")
        terra::resample(rp, thres, method = "near") |> terra::mask(masterMask)
      })
    }

    # 4. Heavy Spatial Operations
    webMecOccData <- sf::st_as_sf(
      occuranceData,
      coords = c("longitude", "latitude"),
      crs = 4326
    ) |>
      sf::st_transform(3857) |>
      terra::vect()

    # Pre-calculate counts in habitat and protected areas
    inThres <- terra::extract(habitatMask, webMecOccData)
    totalObsinThres <- sum(inThres[, 2] == 1, na.rm = TRUE)
    inPro <- terra::extract(p1, webMecOccData)
    totalObsinPro <- totalObsinThres -
      sum(
        inThres[, 2] == 1 & (is.na(inPro[, 2]) | inPro[, 2] == 0),
        na.rm = TRUE
      )

    # SRS in situ raster (rasterized observations)
    srs1 <- terra::rasterize(
      webMecOccData,
      thres,
      fun = "sum",
      background = 0
    ) |>
      terra::mask(habitatMask)

    # GRS in situ raster math
    proReclass <- terra::ifel(is.na(p1), 0, 1)
    grs1 <- thres + proReclass

    # ERS in situ zonal stats and ecoregion gaps
    allEcos <- natArea |> dplyr::select("ECO_ID_U", "ECO_NAME")
    # Leaflet needs 4326 for polygons
    leafletAllEcos <- sf::st_transform(allEcos, 4326)
    # Web mercator for rasterization
    webMecAllEcos <- sf::st_transform(allEcos, 3857)

    # Identify ecoregions with habitat
    ## we have the missing ecoregion list in the so use this to parse out the captured and missing

    v1 <- terra::zonal(
      habitatMask,
      terra::vect(webMecAllEcos),
      fun = "sum",
      na.rm = TRUE
    )
    if (ncol(v1) > 0) {
      names(v1)[ncol(v1)] <- "value"
    }
    v1$ECO_ID_U <- webMecAllEcos$ECO_ID_U
    ecosInThres <- v1 |> dplyr::filter(value > 0) |> dplyr::pull(ECO_ID_U)

    # Identify ecoregions with protection
    ers1 <- terra::zonal(
      proReclass,
      terra::vect(webMecAllEcos),
      fun = "sum",
      na.rm = TRUE
    )
    if (ncol(ers1) > 0) {
      names(ers1)[ncol(ers1)] <- "value"
    }
    ers1$ECO_ID_U <- webMecAllEcos$ECO_ID_U
    ers1$ECO_NAME <- webMecAllEcos$ECO_NAME

    # Missing ecoregions (habitat > 0 AND protection == 0)
    ers2 <- ers1 |>
      dplyr::filter(ECO_ID_U %in% ecosInThres) |>
      dplyr::filter(value == 0)

    if (nrow(ers2) > 0) {
      ers2 <- ers2 |> arrange(desc(ECO_NAME))
      ecoPal2 <- rep(
        RColorBrewer::brewer.pal(n = 11, name = "Spectral"),
        ceiling(nrow(ers2) / 11)
      )
      ers2$color <- ecoPal2[1:nrow(ers2)]
      ers2$ECO_ID_U <- factor(
        ers2$ECO_ID_U,
        ordered = TRUE,
        levels = ers2$ECO_ID_U
      )

      ersinMap <- webMecAllEcos |>
        dplyr::filter(ECO_ID_U %in% ers2$ECO_ID_U) |>
        terra::rasterize(y = thres, field = "ECO_ID_U") |>
        terra::mask(habitatMask)
    } else {
      ersinMap <- NULL
    }

    # GRS ex situ raster math
    if (inherits(g_bufferCrop, "SpatRaster")) {
      m_grs <- matrix(c(NA, 0), ncol = 2)
      gbuf2 <- terra::classify(g_bufferCrop, m_grs)
      grsexMap <- thres + gbuf2
    } else {
      grsexMap <- NULL
    }

    # ERS ex situ ecoregion gaps
    # Handle both list (from direct function call) and string (from CSV)
    if (is.list(ersex$missingEcos)) {
      missingEcos_ex <- ersex$missingEcos |> unlist()
    } else {
      missingEcos_ex <- strsplit(as.character(ersex$missingEcos), ";")[[1]]
    }
    ecoReg <- natArea |> dplyr::filter(ECO_ID_U %in% missingEcos_ex)
    if (nrow(ecoReg) > 0) {
      ecoReg <- ecoReg |> arrange(desc(ECO_NAME))
      ecoPal <- rep(
        RColorBrewer::brewer.pal(n = 11, name = "Spectral"),
        ceiling(nrow(ecoReg) / 11)
      )
      ecoReg$color <- ecoPal[1:nrow(ecoReg)]
      ecoReg$ECO_ID_U <- factor(
        ecoReg$ECO_ID_U,
        ordered = TRUE,
        levels = ecoReg$ECO_ID_U
      )

      # For Leaflet polygons
      leafletEcoReg <- sf::st_transform(ecoReg, 4326)

      webMecEcoReg <- sf::st_transform(ecoReg, 3857)
      ecoRast <- terra::rasterize(webMecEcoReg, thres, field = "ECO_ID_U") |>
        terra::mask(habitatMask)
    } else {
      ecoRast <- NULL
      leafletEcoReg <- NULL
    }

    # Pre-format Evaluation Metrics for DT
    evalData <- if (!is.null(evalTable) && !all(is.na(evalTable))) {
      evalTable |>
        dplyr::select(
          "AUC" = "AUCtest",
          "Normalized AUC" = "nAUC",
          "Calibrated AUC" = "cAUC",
          "Threshold Value" = "threshold_test",
          "Sensitivity" = "sensi_test",
          "Specificity" = "speci_test",
          "Mathews Correlation" = "matthews.cor_test",
          "Cohen's kappa" = "kappa_index_test"
        ) |>
        dplyr::mutate(across(everything(), sigfig))
    } else {
      NA
    }

    aucData <- if (!is.null(aucMetrics) && !all(is.na(aucMetrics))) {
      aucMetrics |>
        dplyr::mutate(across(where(is.numeric), sigfig)) |>
        dplyr::select(
          "Mean AUC" = "ATAUC",
          "SDAUC" = "STAUC",
          "AUC15" = "ASD15",
          "Passed Validity Tests" = "Valid"
        )
    } else {
      NA
    }

    # Variable Importance Formatting
    variableImportanceData <- if (!is.na(variableImportance)) {
      var1 <- readRDS(variableImportance)$rankPredictors
      vn <- readr::read_csv(
        "data/geospatial_datasets/bioclim_layers/variableNames_072025.csv"
      )
      var1 |>
        dplyr::left_join(y = vn, by = c("varNames" = "vitisModelNames")) |>
        dplyr::mutate(across(c('importance'), \(x) round(x, 3))) |>
        dplyr::select(
          "Predictor variable name" = `Current title`,
          "Relative importance to model" = importance,
          "Included in the modeling process" = includeInFinal,
          "Abbrevaition" = varNames
        ) |>
        dplyr::arrange(desc(`Relative importance to model`))
    } else {
      NA
    }

    # Final report data structure
    reportData <- list(
      occData = occData,
      allEcos = leafletAllEcos,
      protectedArea = p1,
      g_buffer = g_buffer,
      g_bufferCrop = g_bufferCrop,
      projectedResults = projectsResults,
      binaryMap = thres,
      habitatMask = habitatMask,
      masterMask = masterMask,
      resampleFactor = resampleFactor,
      unifiedCounts = unifiedCounts,
      fcsex = fcsex,
      fcsin = fcsin,
      exScores = exScores,
      inScores = inScores,
      finalConservationTable = finalConservationTable,
      totalObsinThres = totalObsinThres,
      totalObsinPro = totalObsinPro,
      srs1 = srs1,
      grs1 = grs1,
      ersinMap = ersinMap,
      ers2 = ers2,
      ecoRast = ecoRast,
      ecoReg = leafletEcoReg,
      missingEcos = missingEcos_ex,
      grsexMap = grsexMap,
      modelEvaluation = evalData,
      aucMetrics = aucData,
      variableImportance = variableImportanceData,
      NoModel = NoModel
    )
  } else {
    reportData <- list(
      occData = occData,
      unifiedCounts = unifiedCounts,
      fcsex = fcsex,
      fcsin = fcsin,
      exScores = exScores,
      inScores = inScores,
      finalConservationTable = finalConservationTable,
      NoModel = TRUE
    )
  }

  return(reportData)
}
