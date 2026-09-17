###
# run_round2_20260916.R
# Round-2 model driver. A copy of run_all05082026.R with ONLY these changes
# (decisions 2026-09-16, see review/GBIF_fix_plan_2026-09-16.md):
#   * runVersion "run09162026_1k" (fresh folder, nothing cached)
#   * input = data/processed_occurrence/model_data20260820_taxonomyOnly.csv
#   * `dontRun` defined (it was undefined and errored before the loop)
#   * hard-coded "run08282025_1k" path for topVariablesData.csv -> runVersion
#   * set.seed(1234) at the top of every species iteration so a species'
#     result does not depend on which species ran before it.
#   * species vector `speciesToRun` set explicitly (test: 3 unchanged species)
#   * post-run summaries (all-species) switched off for the test
#   * modelDataSummary.csv is written (the copy built it but never saved it,
#     and grabData() reads it from disk)
#   * projection rasters written to the species results folder, not the shared
#     output_rasters/ (allows concurrent runs; 2026-09-17)
#   * species universe = sheet concepts flagged "Y" for the gap analysis, so a
#     concept with zero records is still processed: zero counts row, gap files
#     from the noModel conventions, no-records summary document (2026-09-17)
#   * FNA step forced (overwrite = TRUE): the copy skipped it; see note at the call
#   * RNGkind("L'Ecuyer-CMRG") + varaibleSelection() on a FORK cluster with 8
#     cores (R2/modeling/variableSelection.R defaults, 2026-09-17): variable
#     selection, and so everything downstream, now repeats exactly for a given
#     seed and core count
###

# 1. Load global environment and assets
source("global.R")
# Reproducible random streams for forked workers (varaibleSelection() runs
# VSURF on a FORK cluster with 8 cores). Must be set before any set.seed().
RNGkind("L'Ecuyer-CMRG")

# 2. Run Parameters
runVersion <- "run09162026_1k"
# overwrite / speciesToRun can be preset before source()-ing this file, e.g.
#   Rscript -e 'speciesToRun <- "Vitis monticola"; overwrite <- TRUE; source("run_round2_20260916.R")'
if (!exists("overwrite")) overwrite <- FALSE
# bufferOnInvalidModel = TRUE sends a species whose SDM fails the published
# robustness rule (ATAUC > 0.7, STAUC < 0.15, ASD15 < 10, from
# calc_sdm_metrics()) to the 50 km buffer method, as the methods text
# describes. Built 2026-09-17, NOT enabled: partner decision pending
# (review/modeling_future_improvements.md item 1b). Default FALSE reproduces
# the published behaviour (the model is used regardless of the flag).
if (!exists("bufferOnInvalidModel")) bufferOnInvalidModel <- FALSE
dontRun <- c()
# (default speciesToRun = gapSpecies, set after the sheet is read below)

# 3. Load Clean Data
allDataPath <- "data/processed_occurrence/model_data20260820_taxonomyOnly.csv"
if (!file.exists(allDataPath)) {
  stop("Model data not found: run preprocessing/preprocessingUpdates2026_08_20.R with enforceBoundingBox <- FALSE first.")
}
speciesData <- read_csv(allDataPath)
# Species universe = every concept flagged for the gap analysis in the taxonomy
# sheet, not just the taxa present in the data: a concept with zero records
# (e.g. Vitis cinerea var. tomentosa after the taxonomy fix) still gets a
# counts table, zero-score gap files and the no-records summary document.
gapSpecies <- read_csv("data/New World Vitis.csv", col_types = cols(.default = "c"), show_col_types = FALSE) |>
  dplyr::filter(`Include in gap analysis?` == "Y") |>
  dplyr::pull(`Scientific Name`) |>
  sort()
species <- sort(unique(c(speciesData$taxon, gapSpecies)))
if (!exists("speciesToRun")) speciesToRun <- gapSpecies

# 4. Directory Setup
dir1 <- "data/Vitis"
if (!dir.exists(dir1)) {
  dir.create(dir1)
}

dir2 <- paste0(dir1, "/", runVersion)
if (!dir.exists(dir2)) {
  dir.create(dir2)
}

# Error tracking lists
erroredSpecies <- list(
  noLatLon = c(),
  lessThenEight = c(),
  noSDM = c(),
  noHTML = c()
)

# 5. Determine which species to run
s2 <- speciesData |>
  dplyr::group_by(taxon) |>
  dplyr::summarise(count = n()) |>
  dplyr::arrange(count) |>
  dplyr::filter(taxon != "NA")


updateSpecies <- c()



r2 <- s2$taxon[!s2$taxon %in% dontRun]
r3 <- speciesToRun
# adding some text for git 
#
# 6. Main Modeling Loop
for (j in r3) {
  set.seed(1234)
  modelInvalid <- FALSE # set TRUE only by the validity gate when bufferOnInvalidModel is on
  print(paste("Processing:", j))

  p1 <- paste0("data/Vitis/speciesSummaryHTML/", runVersion)
  if (!dir.exists(p1)) {
    dir.create(p1)
  }

  allPaths <- definePaths(dir1 = dir1, j = j, runVersion = runVersion)
  generateFolders(allPaths)

  # Species specific data subset
  sd1 <- speciesData |> dplyr::filter(taxon == j)

  if (j == "Vitis cinerea") {
    sd1 <- speciesData |>
      dplyr::filter(
        taxon %in%
          c(
            "Vitis cinerea",
            "Vitis cinerea var. cinerea",
            "Vitis cinerea var. tomentosa"
          )
      ) |>
      dplyr::mutate(taxon = "Vitis cinerea")
  }
  if (j == "Vitis aestivalis") {
    sd1 <- speciesData |>
      dplyr::filter(
        taxon %in%
          c(
            "Vitis aestivalis",
            "Vitis aestivalis var. aestivalis",
            "Vitis aestivalis var. bicolor"
          )
      ) |>
      dplyr::mutate(taxon = "Vitis aestivalis")
  }
  
  if (j == "Vitis shuttleworthii"){
    sd1 <- sd1 |> 
      dplyr::filter(
        longitude != -80.001483
      )
  }
  
  
  c1 <- write_CSV(
    path = allPaths$countsPaths,
    overwrite = overwrite,
    function1 = if (nrow(sd1) > 0) generateCounts(speciesData = sd1) else
      data.frame(species = j, totalRecords = 0L, hasLat = 0L, hasLong = 0L, totalUseful = 0L,
                 totalGRecords = 0L, totalGUseful = 0L, totalHRecords = 0L, totalHUseful = 0L,
                 numberOfUniqueSources = 0L)
  )

  srsex <- write_CSV(
    path = allPaths$srsExPath,
    overwrite = overwrite,
    function1 = srs_exsitu(sp_counts = c1)
  )
  
  if(c1$totalUseful > 0 ){
    # --- 1. Generate the spatial object FIRST ---
    sp1 <- write_GPKG(
      path = allPaths$spatialDataPath,
      overwrite = TRUE,
      function1 = createSF_Objects(speciesData = sd1) |> removeDuplicates()
    )
  }else{
    sp1 <- write_GPKG(
      path = allPaths$spatialDataPath,
      overwrite = TRUE,
      function1 = createSF_Objects(speciesData = sd1) )
  }


  # Only apply FNA if sp1 is actually a spatial object (not the character error string)
  # overwrite must be TRUE here: the file was just written above, so with FALSE
  # write_GPKG() returns it unchanged and applyFNA() never runs (run_all072025.R
  # carries the same note). Confirmed on the 2026-09-16 test: monticola kept 22
  # out-of-state points and ERSex halved. Published run applied the filter.
  if (!inherits(sp1, "character")) {
    sp1 <-  write_GPKG(
      path = allPaths$spatialDataPath,
      overwrite = TRUE,
      function1 = applyFNA(
        speciesPoints = sp1,
        fnaData = fnaData,
        states = naStates
      )
    )
  }

  # --- 2. Check for empty, missing, or invalid coordinates ---
  # This catches species with 0 records OR species whose records were removed by FNA
  if (
    inherits(sp1, "character") ||
      is.null(sp1) ||
      nrow(sp1) == 0 ||
      c1$totalUseful == 0
  ) {
    erroredSpecies$noLatLon <- c(erroredSpecies$noLatLon, j)

    # Read counts safely
    countsPath <- paste0(
      "data/Vitis/",
      j,
      "/",
      runVersion,
      "/occurances/counts.csv"
    )
    if (file.exists(countsPath)) {
      counts <- read_csv(countsPath, show_col_types = FALSE)
    } else {
      counts <- c1 # fallback to the one we just generated
    }

    # Gap-analysis files for a taxon with no usable coordinates, using the
    # existing functions' noModel conventions (GRS/ERS unavailable, FCS = SRS/3,
    # in situ scores 0). This replaces the hand-coded rows for rufotomentosa and
    # novogranatensis in compileConservationData.R / summaryDocForNoRecords.Rmd
    # and lets summaryTable() pick the taxon up like any other.
    srsexNR <- write_CSV(path = allPaths$srsExPath, overwrite = TRUE, function1 = srs_exsitu(sp_counts = counts))
    fcsexNR <- write_CSV(path = allPaths$fcsexPath, overwrite = TRUE,
                         function1 = fcs_exsitu(srsex = srsexNR, grsex = NULL, ersex = NULL, noModel = TRUE, gPoints = counts$totalGUseful))
    srsinNR <- data.frame(ID = j, SRS = 0)
    fcsinNR <- write_CSV(path = allPaths$fcsinPath, overwrite = TRUE,
                         function1 = fcs_insitu(srsin = srsinNR, grsin = NULL, ersin = NULL, noModel = TRUE))
    fcscNR  <- write_CSV(path = allPaths$fcsCombinedPath, overwrite = TRUE, function1 = fcs_combine(fcsin = fcsinNR, fcsex = fcsexNR))
    conservationNR <- data.frame(
      Taxon = j,
      `Ex situ Sampling Representativeness Score` = round(fcsexNR$SRS, 1),
      `Ex situ Geographic Representativeness Score` = fcsexNR$GRS,
      `Ex situ Ecological Representativeness Score` = fcsexNR$ERS,
      `Ex situ Final Conservation Score` = round(fcsexNR$FCS, 1),
      `Ex situ Conservation Priority` = fcsexNR$FCS_Score,
      `In situ Sampling Representativeness Score` = fcsinNR$SRS,
      `In situ Geographic Representativeness Score` = fcsinNR$GRS,
      `In situ Ecological Representativeness Score` = fcsinNR$ERS,
      `In situ Final Conservation Score` = round(fcsinNR$FCS, 1),
      `In situ Conservation Priority` = fcsinNR$FCS_Score,
      `Final Conservation Score Mean` = round(fcscNR$FCSc_mean, 1),
      `Combined Conservation Priority` = fcscNR$FCSc_mean_class,
      check.names = FALSE)

    htmlExport <- paste0(
      "data/Vitis/speciesSummaryHTML/",
      runVersion,
      "/",
      j,
      "_Summary_fnaFilter.html"
    )

    render_result_nr <- try(
      rmarkdown::render(
        input = "R2/summarize/summaryDocForNoRecords.Rmd",
        output_format = "html_document",
        output_dir = paste0("data/Vitis/speciesSummaryHTML/", runVersion, "/"),
        output_file = paste0(j, "_Summary_fnaFilter"),
        params = list(counts = counts, conservation = conservationNR),
        envir = new.env(parent = globalenv())
      )
    )
    if (inherits(render_result_nr, "try-error")) {
      erroredSpecies$noHTML <- c(erroredSpecies$noHTML, j)
      message("Failed to render no-records summary for ", j)
    }
    next # SKIP TO THE NEXT SPECIES
  }

  # --- 3. Proceed with standard analysis ---
  natArea <- write_GPKG(
    path = allPaths$natAreaPath,
    overwrite = overwrite,
    function1 = nat_area_shp(speciesPoints = sp1, ecoregions = ecoregions)
  )

  b_Number <- numberBackground(natArea = natArea)

  m_data1 <- write_CSV(
    path = allPaths$allDataPath,
    overwrite = overwrite,
    generateModelData(
      speciesPoints = sp1,
      natArea = natArea,
      bioVars = bioVars,
      b_Number = b_Number
    )
  )

  if (nrow(sp1) >= 8) {
    print("Modeling")

    g_buffer <- write_Rast(
      path = allPaths$ga50Path,
      overwrite = overwrite,
      function1 = create_buffers(
        speciesPoints = sp1,
        natArea = natArea,
        bufferDist = bufferDist,
        templateRast = templateRast
      )
    )

    m_data <- m_data1
    presence <- m_data[m_data$presence == 1, ]
    absence <- m_data[m_data$presence != 1, ]
    dubs <- duplicated(absence[, 2:27])
    absence <- absence[!dubs, ]
    m_data <- bind_rows(presence, absence)

    modelDataSummary <- data.frame(
      species = j,
      presenceRecords = nrow(m_data[m_data$presence == 1, ]),
      backgroudRecords = nrow(m_data[m_data$presence == 0, ]),
      totalRecords = nrow(m_data)
    )
    # run_all05082026.R built this table but never wrote it; grabData() reads it
    # from disk (the 0828 run has the file). Blocking fix, no method change.
    write_csv(modelDataSummary, file = paste0(allPaths$occurances, "/modelDataSummary.csv"))

    v_data <- write_RDS(
      path = allPaths$variablbeSelectPath,
      overwrite = overwrite,
      function1 = varaibleSelection(modelData = m_data, parallel = TRUE)
    )

    message(paste0("exporting model data with variables for ", j))

    write_csv(
      x = v_data$rankPredictors,
      file = paste0(
        "data/Vitis/",
        j,
        "/", runVersion, "/occurances/topVariablesData.csv"
      )
    )

    rasterInputs <- write_Rast(
      path = allPaths$prepRasters,
      overwrite = overwrite,
      function1 = cropRasters(
        natArea = natArea,
        bioVars = bioVars,
        selectVars = v_data
      )
    )

    sdm_result <- write_RDS(
      path = allPaths$sdmResults,
      overwrite = overwrite,
      function1 = runMaxnet(selectVars = v_data, rasterData = rasterInputs)
    )

    if (!is.null(sdm_result)) {
      print("conservation metrics")

      projectsResults <- write_RDS(
        path = allPaths$modeledRasters,
        overwrite = TRUE,
        # write the mean/median/stdev rasters into this species' results folder
        # instead of the shared output_rasters/ (which two concurrent runs would
        # overwrite for each other). Path change only; values are unchanged.
        function1 = rasterResults(sdm_result, out_dir = allPaths$results)
      ) |>
        lapply(terra::unwrap)

      aucMetrics <- write_CSV(
        path = allPaths$aucMetrics,
        overwrite = overwrite,
        function1 = calc_sdm_metrics(
          sd_raster = projectsResults$stdev,
          auc_scores = sdm_result$AUC
        )
      )

      evalTable <- write_CSV(
        path = allPaths$evalTablePath,
        overwrite = overwrite,
        function1 = evaluateTable(sdm_result = sdm_result)
      )

      # validity gate (see bufferOnInvalidModel above)
      modelInvalid <- isTRUE(bufferOnInvalidModel) && isFALSE(aucMetrics$Valid[1])
      if (modelInvalid) {
        message(j, ": SDM fails the robustness rule (", aucMetrics$Reason[1],
                "); bufferOnInvalidModel = TRUE, using the 50 km buffer method")
        erroredSpecies$noSDM <- c(erroredSpecies$noSDM, j)
      }
    }

    if (!is.null(sdm_result) && !modelInvalid) {

      thres <- write_Rast(
        path = allPaths$thresPath,
        overwrite = overwrite,
        function1 = generateThresholdModel(
          evalTable = evalTable,
          rasterResults = projectsResults
        )
      )

      g_bufferCrop <- write_Rast(
        path = allPaths$g50_bufferPath,
        overwrite = overwrite,
        function1 = cropG_Buffer(ga50 = g_buffer, thres = thres)
      )

      srsin <- write_CSV(
        path = allPaths$srsinPath,
        overwrite = overwrite,
        function1 = srs_insitu(
          occuranceData = sp1,
          thres = thres,
          protectedArea = protectedAreas
        )
      )

      ersin <- write_CSV(
        path = allPaths$ersinPath,
        overwrite = overwrite,
        function1 = ers_insitu(
          occuranceData = sp1,
          nativeArea = natArea,
          protectedArea = protectedAreas,
          thres = thres,
          rasterPath = allPaths$ersinRast
        )
      )
      

      grsin <- write_CSV(
        path = allPaths$grsinPath,
        overwrite = overwrite,
        function1 = grs_insitu(
          occuranceData = sp1,
          protectedArea = protectedAreas,
          thres = thres
        )
      )

      fcsin <- write_CSV(
        path = allPaths$fcsinPath,
        overwrite = overwrite,
        function1 = fcs_insitu(
          srsin = srsin,
          grsin = grsin,
          ersin = ersin,
          noModel = FALSE
        )
      )

      ersex <- write_CSV(
        path = allPaths$ersexPath,
        overwrite = overwrite,
        function1 = ers_exsitu(
          speciesData = sp1,
          thres = thres,
          natArea = natArea,
          ga50 = g_bufferCrop,
          rasterPath = allPaths$ersexRast
        )
      )

      grsex <- write_CSV(
        path = allPaths$grsexPath,
        overwrite = overwrite,
        function1 = grs_exsitu(
          speciesData = sp1,
          ga50 = g_bufferCrop,
          thres = thres
        )
      )

      fcsex <- write_CSV(
        path = allPaths$fcsexPath,
        overwrite = overwrite,
        function1 = fcs_exsitu(
          srsex = srsex,
          grsex = grsex,
          ersex = ersex,
          noModel = FALSE
        )
      )

      fcsCombined <- write_CSV(
        path = allPaths$fcsCombinedPath,
        overwrite = overwrite,
        function1 = fcs_combine(fcsin = fcsin, fcsex = fcsex)
      )

      reportData <- write_RDS(
        path = allPaths$summaryDataPath,
        overwrite = TRUE,
        function1 = grabData(
          fscCombined = fcsCombined,
          ersex = ersex,
          ersin = ersin,
          fcsex = fcsex,
          fcsin = fcsin,
          evalTable = evalTable,
          aucMetrics = aucMetrics,
          g_bufferCrop = g_bufferCrop,
          thres = thres,
          projectsResults = projectsResults,
          occuranceData = sp1,
          v_data = v_data,
          g_buffer = g_buffer,
          natArea = natArea,
          protectedAreas = protectedAreas,
          countsData = c1,
          variableImportance = allPaths$variablbeSelectPath,
          NoModel = FALSE,
          modelDataCounts = read_csv(paste0(
            allPaths$occurances,
            "/modelDataSummary.csv"
          ))
        )
      )

      export1 <- paste0(j, "_Summary_fnaFilter")
      # if (!file.exists(export1)) {
        render_result <- try(
          rmarkdown::render(
            input = "R2/summarize/singleSpeciesSummary_1k_editsSGCK.Rmd",
            output_format = "html_document",
            output_dir = p1,
            output_file = export1,
            params = list(reportData = reportData),
            envir = new.env(parent = globalenv())
          )
        )
        if (inherits(render_result, "try-error")) {
          erroredSpecies$noHTML <- c(erroredSpecies$noHTML, j)
          message("Failed to render 1km summary for ", j)
        }
      # }
    }
  }

  # Buffer method: fewer than eight points (published rule) OR, when the
  # switch is on, a model that failed the robustness rule.
  if (nrow(sp1) < 8 || modelInvalid) {
    if (nrow(sp1) < 8) erroredSpecies$lessThenEight <- c(erroredSpecies$lessThenEight, j)
    # gPoints was never defined in the published scripts (fcs_exsitu() needs it
    # in this branch): number of georeferenced G records
    gPoints <- c1$totalGUseful

    natAreaV <- terra::vect(natArea)
    buffer <- sp1 |>
      terra::vect() |>
      terra::buffer(width = bufferDist) |>
      terra::crop(natAreaV) |>
      terra::mask(natAreaV)

    rastBuff <- terra::crop(templateRast, buffer)
    buffer_rs <- terra::rasterize(buffer, rastBuff)
    names(buffer_rs) <- "Threshold"

    # overwrite when the buffer replaces a failed model, so the saved threshold
    # raster is the buffer and not the SDM's
    write_Rast(buffer_rs, path = allPaths$thresPath, overwrite = overwrite || modelInvalid)

    g_buffer <- write_Rast(
      path = allPaths$ga50Path,
      overwrite = overwrite,
      function1 = create_buffers(
        speciesPoints = sp1,
        natArea = natArea,
        bufferDist = bufferDist,
        templateRast = templateRast
      )
    )

    if (class(g_buffer) == "character") {
      g_bufferCrop <- g_buffer
    } else {
      g_bufferCrop <- g_buffer |> terra::mask(natAreaV)
      write_Rast(
        g_buffer,
        path = allPaths$g50_bufferPath,
        overwrite = overwrite
      )
    }

    srsin <- write_CSV(
      path = allPaths$srsinPath,
      overwrite = overwrite,
      function1 = srs_insitu(
        occuranceData = sp1,
        thres = buffer_rs,
        protectedArea = protectedAreas
      )
    )

    ersin <- write_CSV(
      path = allPaths$ersinPath,
      overwrite = overwrite,
      function1 = ers_insitu(
        occuranceData = sp1,
        nativeArea = natArea,
        protectedArea = protectedAreas,
        thres = buffer_rs,
        rasterPath = allPaths$ersinRast
      )
    )

    grsin <- write_CSV(
      path = allPaths$grsinPath,
      overwrite = overwrite,
      function1 = grs_insitu(
        occuranceData = sp1,
        protectedArea = protectedAreas,
        thres = buffer_rs
      )
    )

    fcsin <- write_CSV(
      path = allPaths$fcsinPath,
      overwrite = overwrite,
      function1 = fcs_insitu(
        srsin = srsin,
        grsin = grsin,
        ersin = ersin,
        noModel = FALSE
      )
    )

    ersex <- write_CSV(
      path = allPaths$ersexPath,
      overwrite = TRUE,
      function1 = ers_exsitu(
        speciesData = sd1,
        thres = buffer_rs,
        natArea = natArea,
        ga50 = g_bufferCrop,
        rasterPath = allPaths$ersexRast
      )
    )

    grsex <- write_CSV(
      path = allPaths$grsexPath,
      overwrite = TRUE,
      function1 = grs_exsitu(
        speciesData = sd1,
        ga50 = g_bufferCrop,
        thres = buffer_rs
      )
    )

    fcsex <- write_CSV(
      path = allPaths$fcsexPath,
      overwrite = TRUE,
      function1 = fcs_exsitu(
        srsex = srsex,
        grsex = grsex,
        ersex = ersex,
        noModel = FALSE,
        gPoints = gPoints
      )
    )

    fcsCombined <- write_CSV(
      path = allPaths$fcsCombinedPath,
      overwrite = overwrite,
      function1 = fcs_combine(fcsin = fcsin, fcsex = fcsex)
    )

    reportData <- write_RDS(
      path = allPaths$summaryDataPath,
      overwrite = TRUE,
      function1 = grabData(
        fscCombined = fcsCombined,
        ersex = ersex,
        fcsex = fcsex,
        fcsin = fcsin,
        ersin = ersin,
        evalTable = NA,
        aucMetrics = NA,
        g_bufferCrop = g_bufferCrop,
        thres = buffer_rs,
        projectsResults = NA,
        occuranceData = sp1,
        v_data = NA,
        g_buffer = g_buffer,
        natArea = natArea,
        protectedAreas = protectedAreas,
        countsData = c1,
        variableImportance = NA,
        NoModel = FALSE,
        modelDataCounts = NA
      )
    )

    export_buf <- paste0(j, "_Summary_fnaFilter")
    # if (!file.exists(paste0(p1, "/", export_buf, ".html"))) {
      render_result_buf <- try(
        rmarkdown::render(
          input = "R2/summarize/singleSpeciesSummaryBuffer_1k.Rmd",
          output_format = "html_document",
          output_dir = p1,
          output_file = export_buf,
          params = list(reportData = reportData),
          envir = new.env(parent = globalenv())
        )
      )
      if (inherits(render_result_buf, "try-error")) {
        erroredSpecies$noHTML <- c(erroredSpecies$noHTML, j)
        message("Failed to render 1km summary (buffer version) for ", j)
      }
    # }
  }
}

# 7. Post-Run Summaries
runSummaries <- FALSE # test run: all-species summaries need every taxon under this runVersion
if (runSummaries == TRUE) {
  generateRunSummaries(
    dir1 = dir1,
    runVersion = runVersion,
    species = s2$taxon,
    genus = "Vitis",
    protectedAreas = protectedAreas,
    overwrite = FALSE
  )
}

renderBoxPlots <- FALSE
if (renderBoxPlots == TRUE) {
  amd <- list.files(
    dir1,
    pattern = "allmodelData.csv",
    full.names = TRUE,
    recursive = TRUE
  )
  amd2 <- amd[grepl(pattern = runVersion, x = amd)]
  df4 <- data.frame()
  for (p in seq_along(species)) {
    p1 <- amd2[grepl(pattern = paste0(species[p], "/"), x = amd2)]
    if (length(p1) == 1) {
      p2 <- p1 |>
        read.csv() |>
        dplyr::filter(presence == 1) |>
        dplyr::mutate(taxon = species[p])
      df4 <- bind_rows(p2, df4)
    }
  }
  inputData <- list(data = df4, species = sort(species), names = bioNames)

  rmarkdown::render(
    input = "R2/summarize/boxplotSummaries_editsSGCK.Rmd",
    output_format = "html_document",
    output_dir = file.path(dir1),
    output_file = paste0(runVersion, "_boxPlotSummary.html"),
    params = list(inputData = inputData),
    envir = new.env(parent = globalenv())
  )
}

if (runSummaries) {
  source("R2/summarize/summaryTable.R")
  summaryCSV <- summaryTable(species = species, runVersion = runVersion)
  write_csv(x = summaryCSV, file = paste0("data/Vitis/summaryTable_", runVersion, ".csv"))
}
saveRDS(erroredSpecies, paste0("data/Vitis/erroredSpecies_", runVersion, ".RDS"))
print(erroredSpecies)
