# rerun
pacman::p_load(terra, dplyr, readr, leaflet)
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


# custom ERSex function to account for the native area features -----------
# gap analysis functions 
ERSex <- function (taxon, sdm, occurrenceData, gBuffer, ecoregions, idColumn){
  # grab the point features from the occurrenceData 
  d1 <- terra::vect(dplyr::filter(occurrenceData, occurrenceData$species == taxon),
                    geom = c("longitude", "latitude"), crs = "+proj=longlat +datum=WGS84")
  d1$color <- ifelse(d1$type == "H", yes = "#1184d4", no = "#6300f0")
  
  # filter ecoregion to locations with point observations 
  eco1 <- ecoregions1[d1,]
  # select the ecoregions that are in the 
  eco1$id_column <- as.data.frame(eco1)[[idColumn]]
  # eco1 <- terra::aggregate(x = eco1, by = "id_column")
  eco1 <- terra::crop(eco1, sdm)
  # this is the first measure, how much SDM in all the ecoregions 
  eco1$sdmSum <- terra::zonal(x = sdm, z = eco1, fun = "sum", na.rm = TRUE)
  # filter to feaetures with values 
  ecoSelect <- eco1[eco1$sdmSum > 0, ]
  
  # 15 features with distribution inside the ecoregions
  eco2 <- dplyr::select(terra::as.data.frame(ecoSelect), ecoID = id_column, 
                        count = sdmSum)
  if (is.character(gBuffer$data)) {
    ers <- 0
    gEco <- NA
    gEcoCounts <- 0
    totalEcosCount <- nrow(ecoSelect)
    missingEcos <- eco2$ecoID
  } else {
    b1 <- terra::mask(terra::rasterize(x = gBuffer$data, 
                                       y = sdm), sdm)
    # three features with buffered ecos 
    # returning 9 
    eco2$bufferEcos <- unlist(terra::zonal(x = b1, z = ecoSelect, 
                                           fun = "sum", na.rm = TRUE))
    ecoGrouped <- dplyr::summarise(
      dplyr::group_by(dplyr::mutate(eco2,
                                    bufferEcos = dplyr::case_when(
                                      is.na(bufferEcos) ~                                            0, is.nan(bufferEcos) ~ 0, TRUE ~ bufferEcos)), 
                                                   ecoID), inDistribution = sum(count, na.rm = TRUE), 
                                   inGBuffer = sum(bufferEcos, na.rm = TRUE))
    totalEcosCount <- nrow(ecoGrouped)
    # 
    gEcoIds <- pull(ecoGrouped[ecoGrouped$inGBuffer > 0, 
                               "ecoID"])
    gEcoCounts <- length(gEcoIds)
    missingEcos <- ecoSelect[!ecoSelect$id_column %in% gEcoIds, 
    ]
    ers <- min(c(100, (gEcoCounts/totalEcosCount) * 100))
  }
  out_df = dplyr::tibble(Taxon = taxon, `Ecoregions with records` = totalEcosCount, 
                         `Ecoregions within G buffer` = gEcoCounts, `ERS exsitu` = ers)
  map_title <- "<h3 style='text-align:center; background-color:rgba(255,255,255,0.7); padding:2px;'>Ecoregions outside of the G Buffer areas</h3>"
  map <- leaflet::addControl(leaflet::addLegend(leaflet::addPolygons(leaflet::addTiles(leaflet()), 
                                                                     data = ecoSelect, color = "#444444", weight = 1, opacity = 1, 
                                                                     fillOpacity = 0.1, popup = ~ECO_NAME, fillColor = NA), 
                                                position = "topright", title = "ERS ex situ", colors = c("#47ae24", 
                                                                                                         "#746fae", "#f0a01f", "#44444440"), labels = c("Distribution", 
                                                                                                                                                        "G buffer", "Eco gaps", "All Ecos"), opacity = 1), 
                             html = map_title, position = "bottomleft")
  if (ers > 0) {
    map <- leaflet::addCircleMarkers(leaflet::addRasterImage(leaflet::addRasterImage(leaflet::addPolygons(map, 
                                                                                                          data = missingEcos, color = "#444444", weight = 1, 
                                                                                                          opacity = 1, popup = ~ECO_NAME, fillOpacity = 0.5, 
                                                                                                          fillColor = "#f0a01f"), x = sdm, colors = "#47ae24"), 
                                                             x = b1, colors = "#746fae"), data = d1, color = ~color, 
                                     radius = 2, opacity = 1)
  }
  else {
    map <- leaflet::addCircleMarkers(map, data = d1, color = ~color, 
                                     radius = 2, opacity = 1)
  }
  output <- list(results = out_df, ecoGaps = missingEcos, map = map)
  return(output)
}










n <- 1
for (i in species) {
  export <- paste0("data/Vitis/varBuffer/", i, "_", buffSize, ".csv")

  if (!file.exists(export)) {
    print(i)
    rs <- r2[grepl(pattern = paste0(i, "/"), x = r2)]
    if (length(rs) == 0) {
      next
    }
    sdm <- rast(rs)
    sdm[sdm == 0, ] <- NA
    p1 <- sf::st_read(s2[grepl(pattern ="Vitis arizonica/", x = s2) ])
    occurrenceData <- p1 |>
      as.data.frame() |>
      dplyr::select(
        species= taxon,  latitude, longitude, type
      )
    
    
    # p1 <- vect(s2[grepl(pattern = paste0(i, "/"), x = s2)])
    # limit ecoregions to the areas with known occurrences
    # eco1 <- ecoregions[p1, ]
    
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
      occurrenceData = occurrenceData,
      bufferDistM = buffSize
    )

    ## geographic representativeness score  exsitu
    grs_exsitu <- GRSex(taxon = i,
                        sdm = sdm,
                        gBuffer = gBuffer)
    ## map is not clipping the buffer objects to the distribution

    ## Ecological representativeness score exsitu
    ers_exsitu <- ERSex(
      taxon = i,
      sdm = sdm,
      occurrenceData = occurrenceData,
      gBuffer = gBuffer,
      ecoregions = ecoregions,
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
write_csv(x = output, file = paste0("data/Vitis/varBuffer/summaryTable_",buffSize,".csv" ))

# 
# # Do a direct comparison of the ERSex functions between the source --------
# 
# # use arizona as the examples 
# species <- "Vitis arizonica"
# taxon <-  "Vitis arizonica"
# 
# thres <- terra::rast(r2[grepl(pattern = paste0(i, "/"), x = r2)])
# sdm <- terra::rast(r2[grepl(pattern = paste0(i, "/"), x = r2)])
# 
# speciesData <- occurrenceData1[occurrenceData1$species == "Vitis arizonica", ]
# occurrenceData <- sf::st_read(s2[grepl(pattern ="Vitis arizonica/", x = s2) ]) |>
#   as.data.frame() |>
#   dplyr::select(
#     species= taxon,  latitude, longitude, type
#   )
# 
# 
# # only used by vitis 
# ga50 <- rast("data/Vitis/Vitis arizonica/run08282025_1k/results/ga50.tif"  )
# natArea <- sf::st_read("data/Vitis/Vitis arizonica/run08282025_1k/results/naturalArea.gpkg" )
# # ecoregions object is used by vitis 
# ecoregions1 <- ecoregions
# 
# bufferDistM <- 50000
# buffDist <- 50000 
# 
# idColumn <- "ECO_CODE"
# ## grapes 
# 
# ### altering input to be equal between the two 
# ers_exsitu <- function(speciesData,thres,natArea,ga50, rasterPath = NULL) {
#   
#   # convert natural area object in a vect feature
#   n1 <- natArea %>% 
#     dplyr::select(ECO_ID_U)%>% 
#     vect()
#   
#   # calculate the total number of cells within each eco region  -- check 1 
#   ## returns 2 zeros - 15 features within the distribution 
#   v1 <- terra::zonal(x = thres,z = n1,fun="sum",na.rm=TRUE)
#   # 
#   v1$ECO_ID_U <- n1$ECO_ID_U
#   
# 
#   # Number of ecoregions considered. 
#   eSelect <- v1 |>
#     filter(Threshold > 0)
#   
#   nEco <- eSelect |>
#     nrow()
#   # remove the potential ecoregions witn no distribution 
#   n1 <- n1[n1$ECO_ID_U %in% eSelect$ECO_ID_U,]
#   
#   
#   if(class(ga50)[[1]] != "SpatRaster"){
#     ers <- 0
#     gEco <- NA
#     missingEcos <- v1$ECO_ID_U
#   }else{
#     
#     
#     # determine ecoregions in ga50 area  --- this object needs to be
#     ga50masked <- ga50 * thres
#     ## currently returns : 5 NAN 
#     v2 <- terra::zonal(x = ga50masked,z = n1,fun="sum",na.rm=TRUE)
#     v2$ECO_ID_U <- n1$ECO_ID_U
#     
#     # determine the ecoregions that are not being considered 
#     areasWithGBuffer <- v2 |> 
#       filter(layer >0) |>
#       filter(!is.nan(layer)) 
#     # get the total number number of eco regions with a g buffer area
#     # 12 total 
#     gEco <- areasWithGBuffer |> 
#       nrow()
#     # generate a list of the ecoregions ID that are inside the threshold but have no g buffer 
#     missingEcos <- v1 |> 
#       dplyr::filter(Threshold >0) |>
#       dplyr::filter(!ECO_ID_U %in% areasWithGBuffer$ECO_ID_U)|>
#       dplyr::select(ECO_ID_U)|>
#       pull()
#     
#     # ERs calculation 
#     ers <- min(c(100, (gEco/nEco)*100))
#     
#   }
#   # if(!is.null(rasterPath)){
#   #   # produce threshold map excluding collected eco regions. 
#   #   n2 <- n1 |>
#   #     dplyr::filter(ECO_ID_U %in% missingEcos)|>
#   #     terra::rasterize(y = thres)
#   #   terra::writeRaster(x = n2, filename = rasterPath,overwrite=TRUE)
#   # }
#   
#   # generate filter 
#   out_df = data.frame(ID=speciesData$taxon[1],
#                       SPP_N_ECO=nEco,
#                       G_N_ECO=gEco, 
#                       ERS=ers)
#   out_df$missingEcos <- list(missingEcos)
#   
#   # generate dataframe
#   return(out_df)
# }
# 
# 
# 
# 
# gBuffer <- function (taxon, occurrenceData, bufferDistM){
#   d1 <- terra::vect(dplyr::filter(occurrenceData,
#                                   species == taxon & type == "G"),
#                     geom = c("longitude", "latitude"))
#   terra::crs(d1) <- "epsg:4326"
#   if (nrow(d1) > 0) {
#     d2 <- terra::buffer(d1, width = bufferDistM)
#     map_title <- "<h3 style='text-align:center; background-color:rgba(255,255,255,0.7); padding:2px;'>Buffered G Occurrences</h3>"
#     map <- leaflet::addControl(leaflet::addCircleMarkers(leaflet::addPolygons(leaflet::addTiles(leaflet::leaflet(d2)), 
#                                                                               color = "#444444", weight = 1, opacity = 1, fillOpacity = 0.5, 
#                                                                               fillColor = "#6300f0"), data = d1, color = "#000", 
#                                                          radius = 2, opacity = 1), html = map_title, position = "bottomleft")
#   }
#   else {
#     d2 <- "No G points present"
#     map <- leaflet::leaflet(d2)
#   }
#   return(list(data = d2, map = map))
# }
# 
# gBuffer <- generateGBuffers(taxon = taxon, 
#                             occurrenceData = occurrenceData,
#                             bufferDistM = buffDist)
# 
# # gap analysis functions 
# ERSex <- function (taxon, sdm, occurrenceData, gBuffer, ecoregions, idColumn){
#   # grab the point features from the occurrenceData 
#   d1 <- terra::vect(dplyr::filter(occurrenceData, occurrenceData$species == taxon),
#                     geom = c("longitude", "latitude"), crs = "+proj=longlat +datum=WGS84")
#   d1$color <- ifelse(d1$type == "H", yes = "#1184d4", no = "#6300f0")
#   
#   # filter ecoregion to locations with point observations 
#   eco1 <- ecoregions1[d1,]
#   # select the ecoregions that are in the 
#   eco1$id_column <- as.data.frame(eco1)[[idColumn]]
#   # eco1 <- terra::aggregate(x = eco1, by = "id_column")
#   eco1 <- terra::crop(eco1, sdm)
#   # this is the first measure, how much SDM in all the ecoregions 
#   eco1$sdmSum <- terra::zonal(x = sdm, z = eco1, fun = "sum", na.rm = TRUE)
#   # filter to feaetures with values 
#   ecoSelect <- eco1[eco1$sdmSum > 0, ]
#   
#   # 15 features with distribution inside the ecoregions
#   eco2 <- dplyr::select(terra::as.data.frame(ecoSelect), ecoID = id_column, 
#                         count = sdmSum)
#   if (is.character(gBuffer$data)) {
#     ers <- 0
#     gEco <- NA
#     gEcoCounts <- 0
#     totalEcosCount <- nrow(ecoSelect)
#     missingEcos <- eco2$ecoID
#   } else {
#     b1 <- terra::mask(terra::rasterize(x = gBuffer$data, 
#                                        y = sdm), sdm)
#     # three features with buffered ecos 
#     eco2$bufferEcos <- unlist(terra::zonal(x = b1, z = ecoSelect, 
#                                            fun = "sum", na.rm = TRUE))
#     ecoGrouped <- dplyr::summarise(dplyr::group_by(dplyr::mutate(eco2, 
#                                                                  bufferEcos = dplyr::case_when(is.na(bufferEcos) ~ 
#                                                                                                  0, is.nan(bufferEcos) ~ 0, TRUE ~ bufferEcos)), 
#                                                    ecoID), inDistribution = sum(count, na.rm = TRUE), 
#                                    inGBuffer = sum(bufferEcos, na.rm = TRUE))
#     totalEcosCount <- nrow(ecoGrouped)
#     # 
#     gEcoIds <- pull(ecoGrouped[ecoGrouped$inGBuffer > 0, 
#                                "ecoID"])
#     gEcoCounts <- length(gEcoIds)
#     missingEcos <- ecoSelect[!ecoSelect$id_column %in% gEcoIds, 
#     ]
#     ers <- min(c(100, (gEcoCounts/totalEcosCount) * 100))
#   }
#   out_df = dplyr::tibble(Taxon = taxon, `Ecoregions with records` = totalEcosCount, 
#                          `Ecoregions within G buffer` = gEcoCounts, `ERS exsitu` = ers)
#   map_title <- "<h3 style='text-align:center; background-color:rgba(255,255,255,0.7); padding:2px;'>Ecoregions outside of the G Buffer areas</h3>"
#   map <- leaflet::addControl(leaflet::addLegend(leaflet::addPolygons(leaflet::addTiles(leaflet()), 
#                                                                      data = ecoSelect, color = "#444444", weight = 1, opacity = 1, 
#                                                                      fillOpacity = 0.1, popup = ~ECO_NAME, fillColor = NA), 
#                                                 position = "topright", title = "ERS ex situ", colors = c("#47ae24", 
#                                                                                                          "#746fae", "#f0a01f", "#44444440"), labels = c("Distribution", 
#                                                                                                                                                         "G buffer", "Eco gaps", "All Ecos"), opacity = 1), 
#                              html = map_title, position = "bottomleft")
#   if (ers > 0) {
#     map <- leaflet::addCircleMarkers(leaflet::addRasterImage(leaflet::addRasterImage(leaflet::addPolygons(map, 
#                                                                                                           data = missingEcos, color = "#444444", weight = 1, 
#                                                                                                           opacity = 1, popup = ~ECO_NAME, fillOpacity = 0.5, 
#                                                                                                           fillColor = "#f0a01f"), x = sdm, colors = "#47ae24"), 
#                                                              x = b1, colors = "#746fae"), data = d1, color = ~color, 
#                                      radius = 2, opacity = 1)
#   }
#   else {
#     map <- leaflet::addCircleMarkers(map, data = d1, color = ~color, 
#                                      radius = 2, opacity = 1)
#   }
#   output <- list(results = out_df, ecoGaps = missingEcos, map = map)
#   return(output)
# }
# 
# 
# 
# 
# # test the ERSin functions  -----------------------------------------------
# ERSin <- function (taxon, sdm, occurrenceData, protectedAreas, ecoregions, 
#                    idColumn) 
# {
#   pro <- terra::crop(protectedAreas, sdm)
#   proMask <- pro * sdm
#   ecoregions$id_column <- as.data.frame(ecoregions)[, idColumn]
#   ecoregions <- terra::aggregate(x = ecoregions, by = "id_column")
#   eco <- terra::crop(ecoregions, sdm)
#   eco$totEco <- dplyr::pull(terra::zonal(x = sdm, z = eco, 
#                                          fun = "sum", na.rm = TRUE))
#   selectedEcos <- eco[eco$totEco > 0, ]
#   nEcoModel <- nrow(selectedEcos)
#   eco$totPro <- dplyr::pull(terra::zonal(x = proMask, z = eco, 
#                                          fun = "sum", na.rm = TRUE))
#   protectedEcos <- eco[eco$totPro > 0, ]
#   nProModel <- nrow(protectedEcos)
#   missingEcos <- selectedEcos[!selectedEcos$id_column %in% 
#                                 protectedEcos$id_column, ]
#   if (nProModel == 0) {
#     ers <- 0
#   }
#   else {
#     ers <- (nProModel/nEcoModel) * 100
#   }
#   df <- dplyr::tibble(Taxon = taxon, `Ecoregions within model` = nEcoModel, 
#                       `Ecoregions with protected areas` = nProModel, `ERS insitu` = ers)
#   map_title <- "<h3 style='text-align:center; background-color:rgba(255,255,255,0.7); padding:2px;'>Ecoregions within the SDM without Protected Area</h3>"
#   map <- leaflet::addControl(leaflet::addLegend(leaflet::addRasterImage(leaflet::addRasterImage(leaflet::addPolygons(leaflet::addPolygons(leaflet::addTiles(leaflet::leaflet()), 
#                                                                                                                                           data = selectedEcos, color = "#444444", weight = 1, opacity = 1, 
#                                                                                                                                           popup = ~ECO_NAME, fillOpacity = 0.5, fillColor = "#44444420"), 
#                                                                                                                      data = missingEcos, color = "#444444", weight = 1, opacity = 1, 
#                                                                                                                      popup = ~ECO_NAME, fillOpacity = 0.5, fillColor = "#f0a01f"), 
#                                                                                                 x = sdm, colors = "#47ae24"), x = proMask, colors = "#746fae"), 
#                                                 position = "topright", title = "ERS in situ", colors = c("#47ae24", 
#                                                                                                          "#746fae", "#f0a01f", "#44444440"), labels = c("Distribution", 
#                                                                                                                                                         "Protected Areas", "Eco gaps", "All Ecos"), opacity = 1), 
#                              html = map_title, position = "bottomleft")
#   output = list(results = df, missingEcos = missingEcos, map = map)
#   return(output)
# }
# 
# 
# 
# ers_insitu <- function(occuranceData,nativeArea, protectedArea, thres,rasterPath = NULL){
#   # total e
#   
#   # mask protected areas layer 
#   mask1 <- ifel(test = thres == 1, yes = 1, no = NA)
#   # crop protected areas raster 
#   p1 <- terra::crop(x = protectedArea, y = thres)
#   # multiple to create mask 
#   p1 <- p1 * mask1
#   
#   ## point object of the protected area 
#   # protectedPoints <- terra::as.points(x = p1)
#   # 
#   # thresPoints <- terra::as.points(x = mask1)
#   
#   # convert native area to a vect object
#   if(class(nativeArea)[1] != "SpatVector"){
#     nativeArea <- vect(nativeArea)
#   }
#   # total number of eco regions within the SDM
#   ## ecoregions with predicted presence within the boundaries
#   totEco <- terra::zonal(x = thres ,z = nativeArea, fun = "sum",na.rm=TRUE)|>
#     dplyr::mutate(ECO_ID_U = nativeArea$ECO_ID_U)|> 
#     dplyr::filter(Threshold > 0) 
#   
#   # reduce to number of rows for simple math
#   totalEcoregions <- nrow(totEco) 
#   
#   
#   # totalEcoregions <- terra::extract(x = nativeArea, y = thresPoints) |>
#   #   dplyr::distinct(ECO_ID_U) %>%
#   #   tidyterra::drop_na()%>%
#   #   tidyterra::pull()%>%
#   #   length()
#   # total number of eco regions within the SDM with protect areas. 
#   totProEco <- terra::zonal(x = p1 ,z = nativeArea, fun = "sum",na.rm=TRUE) |>
#     dplyr::mutate(ECO_ID_U = nativeArea$ECO_ID_U) |> 
#     dplyr::filter(layer > 0)
#   
#   totalProtectedEcoregions <- nrow(totProEco) 
#   
#   # totalProtectedEcoregions <- terra::extract(x = nativeArea, y = protectedPoints)%>%
#   #   tidyterra::distinct(ECO_ID_U)%>%
#   #   tidyterra::drop_na()%>%
#   #   tidyterra::pull()%>%
#   #   length()
#   
#   if(totalProtectedEcoregions == 0){
#     ers <- 0 
#   }else{
#     ers <- (totalProtectedEcoregions/totalEcoregions)*100
#   }
#   
#   # generate a gap map for the ERSin 
#   mEcos <- totEco[!totEco$ECO_ID_U %in% totProEco$ECO_ID_U, ]
#   
#   missingEcos <- nativeArea |> 
#     st_as_sf()|>
#     dplyr::filter(ECO_ID_U %in% mEcos$ECO_ID_U) |> # needed to add the $ for filter to work
#     terra::vect()|>
#     terra::rasterize(y = thres)
#   # export for the full 
#   if(!is.null(rasterPath)){
#     terra::writeRaster(x = missingEcos, filename = rasterPath,overwrite=TRUE)
#   }
#   
#   
#   df <- data.frame(ID=occuranceData$taxon[1],
#                    SPP_N_ECO = totalEcoregions,
#                    SPP_WITHIN_PA_N_ECO = totalProtectedEcoregions,
#                    ERS = ers)
#   return(df)
# }














