ers_insitu <- function(
  occuranceData,
  nativeArea,
  protectedArea,
  thres,
  rasterPath = NULL
) {
  # total e

  # mask protected areas layer
  mask1 <- ifel(test = thres == 1, yes = 1, no = NA)
  # crop protected areas raster
  p1 <- terra::crop(x = protectedArea, y = thres)
  # multiple to create mask
  p1 <- p1 * mask1

  # convert native area to a vect object
  if (class(nativeArea)[1] != "SpatVector") {
    nativeArea <- vect(nativeArea)
  }
  # total number of eco regions within the SDM
  ## ecoregions with predicted presence within the boundaries
  totEco <- terra::zonal(
    x = thres,
    z = nativeArea,
    fun = "sum",
    na.rm = TRUE
  ) |>
    dplyr::mutate(ECO_ID_U = nativeArea$ECO_ID_U) |>
    dplyr::filter(Threshold > 0)

  # reduce to number of rows for simple math
  totalEcoregions <- nrow(totEco)

  # total number of eco regions within the SDM with protect areas.
  totProEco <- terra::zonal(
    x = p1,
    z = nativeArea,
    fun = "sum",
    na.rm = TRUE
  ) |>
    dplyr::mutate(ECO_ID_U = nativeArea$ECO_ID_U) |>
    dplyr::filter(layer > 0)

  totalProtectedEcoregions <- nrow(totProEco)
  # calculate the ERS function
  if (totalProtectedEcoregions == 0) {
    ers <- 0
  } else {
    ers <- (totalProtectedEcoregions / totalEcoregions) * 100
  }

  # generate a gap map for the ERSin
  mEcos <- totEco[!totEco$ECO_ID_U %in% totProEco$ECO_ID_U, ]

  # vector of missing ecos
  if (nrow(mEcos) == 0) {
    missEcoIDs <- NA
  } else {
    missEcoIDs <- mEcos$ECO_ID_U
  }
  
  # mask the thres features
  thres[thres!=1, ] <- NA
  
  missingEcos <- nativeArea[nativeArea$ECO_ID_U %in% mEcos$ECO_ID_U, ]
  
  maskThres <- terra::mask(thres, missingEcos)
  # export 
  if (!is.null(rasterPath)) {
    print("writing ")
    terra::writeRaster(x = maskThres, filename = rasterPath, overwrite = TRUE)
  }

  df <- data.frame(
    ID = occuranceData$taxon[1],
    SPP_N_ECO = totalEcoregions,
    SPP_WITHIN_PA_N_ECO = totalProtectedEcoregions,
    ERS = ers
  )
  df$missingEcos <- list(missEcoIDs)
  return(df)
}
