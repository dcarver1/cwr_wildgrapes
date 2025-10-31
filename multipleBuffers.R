# rerun
pacman::p_load(terra, dplyr, readr)
devtools::install_git("https://github.com/CIAT-DAPA/GapAnalysis")
library(GapAnalysis)
# load in ecoregion and protect areas data
## ecoregions
ecoregions <- sf::st_read(
  "data/geospatial_datasets/ecoregions/tnc_terr_ecoregions.gpkg"
) |>
  terra::vect()
## protect lands
protectedAreas <- terra::rast(
  "data/geospatial_datasets/protectedLands/wdpa_1km_all_.tif"
)

# gather data
runVersion <- "run08282025_1k"
files <- list.files(path = "data/Vitis", full.names = TRUE, recursive = TRUE)
r1 <- files[grepl(pattern = runVersion, x = files)]


# load in all rasters
r2 <- r1[grepl(pattern = "prj_threshold.tif", x = r1)]
# load in data for species
s2 <- r1[grepl(pattern = "spatialData.gpkg", x = r1)]
speciesData <- read_csv("temp/allVitisData082025.csv")
species <- sort(unique(speciesData$taxon))

# org for gap r
occurrenceData1 <- speciesData |>
  dplyr::select(
    species = taxon,
    latitude,
    longitude,
    type
  )


buffSize <- 50000

# test some options
# i <- "Vitis arizonica"
# i <- "Vitis californica"

n <- 1
for (i in species) {
  export <- paste0("data/Vitis/", i, "_", buffSize, ".csv")

  if (!file.exists(export)) {
    print(i)
    rs <- r2[grepl(pattern = paste0(i, "/"), x = r2)]
    if (length(rs) == 0) {
      next
    }
    sdm <- rast(rs)
    sdm[sdm == 0, ] <- NA
    p1 <- vect(s2[grepl(pattern = paste0(i, "/"), x = s2)])
    # limit ecoregions to the areas with known occurrences
    eco1 <- ecoregions[p1, ]
    # occurrenceData is used for srsex
    srs_exsitu <- GapAnalysis::SRSex(
      taxon = i,
      occurrence_Data = occurrenceData1
    )

    # need to use the spatial data as this is watch was evaluated within the model
    spatialData <- as.data.frame(p1) |>
      dplyr::select(
        species = taxon,
        latitude,
        longitude,
        type
      )
    # testing
    ## Generate buffer objects
    gBuffer <- generateGBuffers(
      taxon = i,
      occurrenceData = spatialData,
      bufferDistM = buffSize
    )

    ## geographic representativeness score  exsitu
    grs_exsitu <- GRSex(taxon = i, sdm = sdm, gBuffer = gBuffer)
    ## map is not clipping the buffer objects to the distribution

    ## Ecological representativeness score exsitu
    ers_exsitu <- ERSex(
      taxon = i,
      sdm = sdm,
      occurrenceData = spatialData,
      gBuffer = gBuffer,
      ecoregions = eco1,
      idColumn = "ECO_CODE"
    )

    # Running final conservation score ecoregionsexsitu
    fcs_exsitu <- FCSex(
      taxon = i,
      srsex = srs_exsitu,
      grsex = grs_exsitu,
      ersex = ers_exsitu
    )

    # generate insitu conservation summaries
    ## sample representativeness score insitu
    srs_insitu <- SRSin(
      taxon = i,
      sdm = sdm,
      occurrenceData = spatialData,
      protectedAreas = protectedAreas
    )

    ## Geographic representativeness score insitu
    grs_insitu <- GRSin(taxon = i, sdm = sdm, protectedAreas = protectedAreas)

    ## ecological representativeness score insitu
    ers_insitu <- ERSin(
      taxon = i,
      sdm = sdm,
      occurrenceData = occurrenceData,
      protectedAreas = protectedAreas,
      ecoregions = ecoregions,
      idColumn = "ECO_CODE"
    )

    ## final representativeness score insitu
    fcs_insitu <- FCSin(
      taxon = i,
      srsin = srs_insitu,
      grsin = grs_insitu,
      ersin = ers_insitu
    )
    ## combine conservation score
    fcs_combine <- FCSc_mean(taxon = i, fcsin = fcs_insitu, fcsex = fcs_exsitu)
    write_csv(x = fcs_combine, file = export)
  } else {
    fcs_combine <- read_csv(export)
  }

  if (n == 1) {
    output <- fcs_combine
  } else {
    output <- bind_rows(output, fcs_combine)
  }
  n = n + 1
}

# testing -----
# taxon <- i
# occurrenceData <- spatialData
# bufferDistM <- bufferDist
generateGBuffers <- function(taxon, occurrenceData, bufferDistM) {
  d1 <- terra::vect(
    dplyr::filter(occurrenceData, species == taxon & type == "G"),
    geom = c("longitude", "latitude")
  )
  terra::crs(d1) <- "epsg:4326"
  if (nrow(d1) > 0) {
    d2 <- terra::buffer(d1, width = bufferDistM)
    map_title <- "<h3 style='text-align:center; background-color:rgba(255,255,255,0.7); padding:2px;'>Buffered G Occurrences</h3>"
    map <- leaflet::addControl(
      leaflet::addCircleMarkers(
        leaflet::addPolygons(
          leaflet::addTiles(leaflet::leaflet(d2)),
          color = "#444444",
          weight = 1,
          opacity = 1,
          fillOpacity = 0.5,
          fillColor = "#6300f0"
        ),
        data = d1,
        color = "#000",
        radius = 2,
        opacity = 1
      ),
      html = map_title,
      position = "bottomleft"
    )
    return(list(data = d2, map = map))
  } else {
    d2 <- "No G points present"
    return(list(data = d2, map = NA))
  }
}

# taxon <- i
# ecoregions <- eco1

# # ers ex
# d1 <- terra::vect(dplyr::filter(occurrence_Data, occurrence_Data$species ==
#         taxon), geom = c("longitude", "latitude"), crs = "+proj=longlat +datum=WGS84")
# d1$color <- ifelse(d1$type == "H", yes = "#1184d4", no = "#6300f0")
# #
# ecoregions$id_column <- as.data.frame(ecoregions)[[idColumn]]

# ecoregions <- terra::aggregate(x = ecoregions, by = "id_column")
# ecoregions <- terra::crop(ecoregions, sdm)
# ecoregions$sdmSum <- terra::zonal(x = sdm, z = ecoregions,fun = "sum", na.rm = TRUE)

# # remove all eco with no area in the distribution
# ecoSelect <- ecoregions[ecoregions$sdmSum > 0, ]
# # get a count of cells in each area
# eco2 <- dplyr::select(terra::as.data.frame(ecoSelect), ecoID = id_column,
#         count = sdmSum)

# if (is.character(gBuffer$data)) {
#     ers <- 0
#     gEco <- NA
#     missingEcos <- eco2$ecoID
# }else{
#     # buffer area converted to a raster object and clipped to the sdm
#     b1 <- terra::mask(terra::rasterize(x = gBuffer$data,
#         y = sdm), sdm)
#     # a Count of buffer area within each ecoregion
#     eco2$bufferEcos <- unlist(terra::zonal(x = b1, z = ecoSelect,
#         fun = "sum", na.rm = TRUE))
#     # inprove the naming of the dataframe object
#     ecoGrouped <- dplyr::summarise(dplyr::group_by(dplyr::mutate(eco2,
#         bufferEcos = dplyr::case_when(is.na(bufferEcos) ~
#             0, is.nan(bufferEcos) ~ 0, TRUE ~ bufferEcos)),
#         ecoID), inDistribution = sum(count, na.rm = TRUE),
#         inGBuffer = sum(bufferEcos, na.rm = TRUE))
#   #
#     totalEcosCount <- nrow(ecoGrouped)
#     gEcoIds <- pull(ecoGrouped[ecoGrouped$inGBuffer > 0,
#         "ecoID"])
#     gEcoCounts <- length(gEcoIds)
#     missingEcos <- ecoSelect[!ecoSelect$id_column %in% gEcoIds,
#         ]
# }
#     ers <- min(c(100, (gEcoCounts/totalEcosCount) * 100))
#     out_df = dplyr::tibble(Taxon = taxon, `Ecoregions with records` = totalEcosCount,
#         `Ecoregions within G buffer` = gEcoCounts, `ERS exsitu` = ers)

# ### ersec from the vitis work
# # convert natural area object in a vect feature
# natArea <- sf::st_read("/home/dune/trueNAS/work/cwr_wildgrapes/data/Vitis/Vitis arizonica/run08282025_1k/results/naturalArea.gpkg")
# thres <- terra::rast("/home/dune/trueNAS/work/cwr_wildgrapes/data/Vitis/Vitis arizonica/run08282025_1k/results/prj_threshold.tif")
# ga50 <- terra::rast("data/Vitis/Vitis arizonica/run08282025_1k/results/ga50.tif")
# ga50m <- terra::rast("data/Vitis/Vitis arizonica/run08282025_1k/results/ga50_masked.tif")

# ga50m <- terra::mask(terra::rasterize(x = gBuffer$data,
#         y = sdm), sdm)

# n1 <- natArea %>%
#     dplyr::select(ECO_ID_U)%>%
#     vect()

#   v1 <- terra::zonal(x = thres,z = n1,fun="sum",na.rm=TRUE)
#   v1$ECO_ID_U <- n1$ECO_ID_U

#   # reset the number of ecoregions considered
#   v2 <- v1 %>%
#     filter(Threshold > 0)

#   # Number of ecoregions considered.
#   nEco <- v2 %>%
#     nrow()
#   # use these ids to remove some eco regions
#   n2 <- n1[n1$ECO_ID_U %in% v2$ECO_ID_U, ]

#   # these methods are matching and the denomonator is the same for both features

#   if(class(ga50)[[1]] != "SpatRaster"){
#     ers <- 0
#     gEco <- NA
#     missingEcos <- v1$ECO_ID_U
#   }else{

#     # determine ecoregions in ga50 area
#     v2 <- terra::zonal(x = ga50m,z = n2,fun="sum",na.rm=TRUE)
#     v2$ECO_ID_U <- n1$ECO_ID_U

#     # determine the ecoregions that are not being considered
#     areasWithGBuffer <- v2 |>
#       filter(layer >0)  |>
#       filter(!is.nan(layer))
#     # get the total number number of eco regions with a g buffer area
#     gEco <- areasWithGBuffer |>
#       nrow()
#     # generate a list of the ecoregions ID that are inside the threshold but have no g buffer
#     missingEcos <- v1 |>
#       dplyr::filter(Threshold >0) |>
#       dplyr::filter(!ECO_ID_U %in% areasWithGBuffer$ECO_ID_U)|>
#       dplyr::select(ECO_ID_U)|>
#       pull()

#     # ERs calculation
#     ers <- min(c(100, (gEco/nEco)*100))

#   }
#   if(!is.null(rasterPath)){
#   # produce threshold map excluding collected eco regions.
#   n2 <- n1 |>
#     dplyr::filter(ECO_ID_U %in% missingEcos)|>
#     terra::rasterize(y = thres)
#     terra::writeRaster(x = n2, filename = rasterPath,overwrite=TRUE)
#   }

#   # generate filter

#   out_df = data.frame(ID=speciesData$taxon[1],
#                   SPP_N_ECO=nEco,
#                   G_N_ECO=gEco,
#                   ERS=ers)
#   out_df$missingEcos <- list(missingEcos)

#   # generate dataframe
#   return(out_df)

# # ersin
# taxon <- i

# d1 <- terra::vect(dplyr::filter(occurrence_Data, occurrence_Data$species ==
#         taxon), geom = c("longitude", "latitude"), crs = "+proj=longlat +datum=WGS84")
# d1$color <- ifelse(d1$type == "H", yes = "#1184d4", no = "#6300f0")

# ecoregions$id_column <- as.data.frame(ecoregions)[[idColumn]]

# ecoregions <- terra::aggregate(x = ecoregions, by = "id_column")

# ecoregions <- terra::crop(ecoregions, sdm)
# # count the pixels from the distribution within each ecoregion
# ecoregions$sdmSum <- terra::zonal(x = sdm, z = ecoregions,
#         fun = "sum", na.rm = TRUE)
# # select all ecos within the distributin
# ecoSelect <- ecoregions[ecoregions$sdmSum > 0, ]

# eco2 <- dplyr::select(terra::as.data.frame(ecoSelect), ecoID = id_column,
#         count = sdmSum)
#     if (is.character(gBuffer$data)) {
#         ers <- 0
#         gEco <- NA
#         missingEcos <- eco2$ecoID
#     }
#     else {
#         b1 <- terra::mask(terra::rasterize(x = gBuffer$data,
#             y = sdm), sdm)
#         eco2$bufferEcos <- unlist(terra::zonal(x = b1, z = ecoSelect,
#             fun = "sum", na.rm = TRUE))
#         ecoGrouped <- dplyr::summarise(dplyr::group_by(dplyr::mutate(eco2,
#             bufferEcos = dplyr::case_when(is.na(bufferEcos) ~
#                 0, is.nan(bufferEcos) ~ 0, TRUE ~ bufferEcos)),
#             ecoID), inDistribution = sum(count, na.rm = TRUE),
#             inGBuffer = sum(bufferEcos, na.rm = TRUE))
#         totalEcosCount <- nrow(ecoGrouped)
#         gEcoIds <- pull(ecoGrouped[ecoGrouped$inGBuffer > 0,
#             "ecoID"])
#         gEcoCounts <- length(gEcoIds)
#         missingEcos <- ecoSelect[!ecoSelect$id_column %in% gEcoIds,
#             ]
#     }
#     ers <- min(c(100, (gEcoCounts/totalEcosCount) * 100))
#     out_df = dplyr::tibble(Taxon = taxon, `Ecoregions with records` = totalEcosCount,
#         `Ecoregions within G buffer` = gEcoCounts, `ERS exsitu` = ers)
