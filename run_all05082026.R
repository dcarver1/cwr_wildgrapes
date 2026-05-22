###
# run_all.R
# Main execution script for modeling and generating summaries
###

# 1. Load global environment and assets
source("global.R")

# 2. Run Parameters
runVersion <- "run08282025_1k"
overwrite <- FALSE

# 3. Load Clean Data
allDataPath <- "data/datasetsForPublication/allSpeciesOccurrences.csv"
if (!file.exists(allDataPath)) {
  stop("Clean dataset not found. Please run prep_species_data.R first.")
}
speciesData <- read_csv(allDataPath)
species <- sort(unique(speciesData$taxon))

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
r3 <- c("Vitis shuttleworthii") # specific target for this run
# adding some text for git 
#
# 6. Main Modeling Loop
for (j in r3) {
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
    function1 = generateCounts(speciesData = sd1)
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
  if (!inherits(sp1, "character")) {
    sp1 <-  write_GPKG(
      path = allPaths$spatialDataPath,
      overwrite = overwrite,
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

    htmlExport <- paste0(
      "data/Vitis/speciesSummaryHTML/",
      runVersion,
      "/",
      j,
      "_Summary_fnaFilter.html"
    )

    # if (!file.exists(htmlExport)) {
      rmarkdown::render(
        input = "R2/summarize/summaryDocForNoRecords.Rmd",
        output_format = "html_document",
        output_dir = paste0("data/Vitis/speciesSummaryHTML/", runVersion, "/"),
        output_file = paste0(j, "_Summary_fnaFilter"),
        params = list(counts = counts),
        envir = new.env(parent = globalenv())
      )
    # }
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
        "/run08282025_1k/occurances/topVariablesData.csv"
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
        function1 = rasterResults(sdm_result)
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
  } else {
    erroredSpecies$lessThenEight <- c(erroredSpecies$lessThenEight, j)

    natAreaV <- terra::vect(natArea)
    buffer <- sp1 |>
      terra::vect() |>
      terra::buffer(width = bufferDist) |>
      terra::crop(natAreaV) |>
      terra::mask(natAreaV)

    rastBuff <- terra::crop(templateRast, buffer)
    buffer_rs <- terra::rasterize(buffer, rastBuff)
    names(buffer_rs) <- "Threshold"

    write_Rast(buffer_rs, path = allPaths$thresPath, overwrite = overwrite)

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
runSummaries <- TRUE
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

renderBoxPlots <- TRUE
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

source("R2/summarize/summaryTable.R")
summaryCSV <- summaryTable(species = species, runVersion = runVersion)
write_csv(
  x = summaryCSV,
  file = paste0("data/Vitis/summaryTable_", runVersion, ".csv")
)
