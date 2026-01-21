# dir1 = dir1
# runVersion = runVersion
# genus = i

# this script sources a lot of functions from speciesRichnessMap.R

generateRunSummaries <- function(
  dir1,
  runVersion,
  species,
  genus,
  protectedAreas,
  overwrite
) {
  # storing summaries data in run folders
  dir2 <- paste0(dir1, "/", runVersion)
  # generate summary of all the models --------------------------------------
  path1 <- paste0(dir2, "/speciesrichness_1km.tif")
  path2 <- paste0(dir2, "/speciesUsed_speciesrichness.csv")
  path3 <- paste0(dir2, "/ga50_speciesrichness_1km.tif")
  path4 <- paste0(dir2, "/ga50speciesUsed_speciesrichness.csv")
  path5 <- paste0(dir2, "/protectedAreaSpeciesRichness.csv")
  path6 <- paste0(dir2, "/ersexRichness.tif")
  path7 <- paste0(dir2, "/ersex_speciesUsed_Richness.csv")
  path8 <- paste0(dir2, "/ersinRichness.tif")
  path9 <- paste0(dir2, "/ersin_speciesUsed_Richness.tif")
  path10 <- paste0(dir2, "/protectedAreaSpeciesPointRichness.csv")
  path11 <- paste0(dir2, "/protectedAreaSpeciesRasterRichness_1km.csv")

  # Testing the presense of species in the summary maps
  t1 <- read_csv(path10)

  # Rescale all the imagery for the 1km runs  -------------------------------
  ##   "prj_threshold.tif", "ga50_masked.tif","ers_ex_gaps.tif","ers_in_gaps.tif"
  for (i in c(
    "prj_threshold.tif",
    "ga50_masked.tif",
    "ers_ex_gaps.tif",
    "ers_in_gaps.tif"
  )) {
    # grap all files
    allFiles <- list.files(
      path = dir1,
      pattern = i,
      full.names = TRUE,
      recursive = TRUE
    )
    # filter to current run version
    files2 <- allFiles[grepl(pattern = runVersion, x = allFiles)]
    for (file in files2) {
      # generate export name
      export <- sub("\\.tif$", "_5.tif", file)
      # resample if needed
      if (!file.exists(export)) {
        print(file)
        r2 <- resampleRast(rast = terra::rast(file))
        terra::writeRaster(x = r2, filename = export, overwrite = TRUE)
      }
    }
  }

  # this takes a while to run so be careful
  ## generate the species richness file
  if (!file.exists(path1) | isTRUE(overwrite)) {
    # generate specific richness map
    richness <- generateSpeciesRichnessMap(
      directory = dir1,
      runVersion = runVersion,
      rasterFileName = "prj_threshold.tif"
    )
    terra::writeRaster(
      x = richness$richnessTif,
      filename = path1,
      overwrite = TRUE
    )
    # need to convert to a df before writing
    df <- data.frame(speciesUsed = richness$speciesUsed)
    write_csv(x = df, file = path2)
  }

  # generate grsex richness map
  if (!file.exists(path3) | isTRUE(overwrite)) {
    # generate specific richness map
    ga50Richness <- generateSpeciesRichnessMap(
      directory = dir1,
      runVersion = runVersion,
      rasterFileName = "ga50_masked.tif"
    )

    # need to extend this file to matcht he extent of the richness image
    t1 <- terra::rast(path1)
    extended_raster <- extend(ga50Richness$richnessTif, t1, fill = 0)

    terra::writeRaster(x = extended_raster, filename = path3, overwrite = TRUE)
    # export the
    df <- data.frame(speciesUsed = ga50Richness$speciesUsed)
    write_csv(x = df, file = path4)
  }

  # generate ersex richness map
  if (!file.exists(path6) | isTRUE(overwrite)) {
    # # generate specific richness map
    ersExRichness <- generateERSRichnessMap(
      directory = dir1,
      runVersion = runVersion,
      ersMap = "ers_ex_gaps.tif",
      species = species,
      thresholdMap = "prj_threshold.tif"
    )
    terra::writeRaster(
      x = ersExRichness$richnessTif,
      filename = path6,
      overwrite = TRUE
    )
    # # export the
    df <- data.frame(speciesUsed = ersExRichness$speciesUsed)
    write_csv(x = df, file = path7)
  }

  # generate ersin richness map
  if (!file.exists(path8) | isTRUE(overwrite)) {
    # # generate specific richness map
    ersInRichness <- generateERSRichnessMap(
      directory = dir1,
      runVersion = runVersion,
      ersMap = "ers_in_gaps_5.tif",
      species = species,
      thresholdMap = "prj_threshold_5.tif"
    )
    terra::writeRaster(
      x = ersInRichness$richnessTif,
      filename = path8,
      overwrite = TRUE
    )
    # # export the
    df <- data.frame(speciesUsed = ersInRichness$speciesUsed)
    write_csv(x = df, file = path9)
  }

  # points in protected areas Richness  -------------------------------------
  wdpaVectExport <- paste0(
    "data/geospatial_datasets/protectedLands/vect_",
    genus,
    "_",
    runVersion,
    ".gpkg"
  )
  if (!file.exists(wdpaVectExport)) {
    # this function generate the gpkg of all the wdpa files
    wdpaVect1 <- wdpaVect(species = species, runVersion = runVersion)
    terra::writeVector(wdpaVect, wdpaVectExport)
  } else {
    wdpaVect1 <- terra::vect(wdpaVectExport)
  }
  # not 100% what's going on here so going to read in the files directly
  wdpa1 <- terra::vect("data/geospatial_datasets/protectedLands/wdpa_1.gpkg")

  if (overwrite == TRUE) {
    protectedAreaPoints(
      species = s2$taxon,
      runVersion = runVersion,
      genus = "Vitis",
      wdpaVect = wdpaVect
    )

    # read in all files
    proP <- list.files(
      paste0("data/", genus, "/", runVersion, "/proPoints"),
      pattern = ".csv",
      full.names = TRUE
    ) |>
      read_csv() |>
      as_tibble() |>
      group_by(name) |>
      summarise(
        total_observation = sum(count, na.rm = TRUE),
        total_unique_taxa = length(unique(taxon)),
        taxa_list = list(unique(taxon))
      ) |>
      dplyr::select(
        WDPAID = name,
        total_observation,
        total_unique_taxa,
        taxa_list
      )
    # select names and id from pro area
    wdpaDF <- as_tibble(wdpaVect) |>
      dplyr::filter(WDPAID %in% proP$WDPAID) |>
      dplyr::left_join(y = proP, by = "WDPAID")

    write_csv(wdpaDF, path10)
  }

  # generate species richness within protected areas ------------------------
  ## richness
  speciesRichness <- terra::rast(path1)
  # crop to species richness
  pro1 <- wdpa1 |>
    terra::crop(speciesRichness)

  calculate_max_pixel_values <- function(speciesRichness, wdpaVect) {
    #mask the wdpa file to the raster as it is much larger
    pro1 <- wdpaVect |>
      terra::crop(speciesRichness)
    # Extract values from the raster for each polygon in the vector
    raster_values <- terra::extract(speciesRichness, pro1)

    # Calculate the maximum pixel value for each polygon
    max_values <- aggregate(
      raster_values[, -1, drop = FALSE],
      by = list(raster_values[, 1]),
      FUN = max,
      na.rm = TRUE
    )

    # Rename the columns
    colnames(max_values) <- c("ID", "max_pixel_value")

    # Merge the maximum values back to the vector object
    wdpaVect$max_pixel_value <- max_values$max_pixel_value[match(
      wdpaVect$WDPAID,
      max_values$ID
    )]
    # structure for the output
    output <- wdpaVect |>
      as.data.frame() |>
      dplyr::select(
        `WDPA ID` = WDPAID,
        `Protected area name` = NAME,
        `Protected area name orig` = ORIG_NAME,
        `Protected area type` = DESIG_ENG,
        ISO3,
        `Number of taxa based on predicted distributions` = max_pixel_value
      )

    return(output)
  }
  
  if(!file.exists(path11) | isTRUE(overwrite)){
    projectProCounts <- calculate_max_pixel_values(
      speciesRichness = speciesRichness,
      wdpaVect = pro1
    )
    # export
    write_csv(projectProCounts, path11)
  }else{
    projectProCounts <- read_csv(path11)
  }


  # generate conservation summary figures
  conservationSummary <- compileConservationData(
    directory = dir1,
    runVersion = runVersion,
    genus = genus
  )
  # add additional object to the list to be passed to the rmd
  conservationSummary$map <- rast(path1)
  conservationSummary$proAreas <- protectedAreas
  conservationSummary$ga50Map <- rast(path3)
  conservationSummary$protectAreasRichness <- read_csv(path5)
  conservationSummary$ersExRichness <- terra::rast(path6)
  conservationSummary$ersInRichness <- terra::rast(path8)
  conservationSummary$genus <- genus
  
  # run summary html
  try(
    rmarkdown::render(
      input = "R2/summarize/runSummary_editsSGCK.Rmd",
      output_format = "html_document",
      output_dir = paste0(dir1, "/"),
      output_file = paste0(runVersion, "_Summary.html"),
      params = list(
        reportData = conservationSummary
      ),
      envir = new.env(parent = globalenv())
      # clean = F,
      # encoding = "utf-8"
    )
  )
}
