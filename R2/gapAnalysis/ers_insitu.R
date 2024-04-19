


ers_insitu <- function(occuranceData,nativeArea, protectedArea, thres,rasterPath = NULL){
  # total e
  
  # mask protected areas layer 
  mask1 <- ifel(test = thres == 1, yes = 1, no = NA)
  # crop protected areas raster 
  p1 <- terra::crop(x = protectedArea, y = thres)
  # multiple to create mask 
  p1 <- p1 * mask1
  
  ## point object of the protected area 
  # protectedPoints <- terra::as.points(x = p1)
  # 
  # thresPoints <- terra::as.points(x = mask1)
  
  # convert native area to a vect object
  if(class(nativeArea)[1] != "SpatVector"){
    nativeArea <- vect(nativeArea)
  }
  # total number of eco regions within the SDM
  ## ecoregions with predicted presence within the boundaries
  totEco <- terra::zonal(x = thres ,z = nativeArea, fun = "sum",na.rm=TRUE)|>
    dplyr::mutate(ECO_ID_U = nativeArea$ECO_ID_U)|> 
    dplyr::filter(Threshold > 0) 
  
  # reduce to number of rows for simple math
  totalEcoregions <- nrow(totEco) 
  
  
  # totalEcoregions <- terra::extract(x = nativeArea, y = thresPoints) |>
  #   dplyr::distinct(ECO_ID_U) %>%
  #   tidyterra::drop_na()%>%
  #   tidyterra::pull()%>%
  #   length()
  # total number of eco regions within the SDM with protect areas. 
  totProEco <- terra::zonal(x = p1 ,z = nativeArea, fun = "sum",na.rm=TRUE) |>
    dplyr::mutate(ECO_ID_U = nativeArea$ECO_ID_U) |> 
    dplyr::filter(layer > 0)
  
  totalProtectedEcoregions <- nrow(totProEco) 
  
  # totalProtectedEcoregions <- terra::extract(x = nativeArea, y = protectedPoints)%>%
  #   tidyterra::distinct(ECO_ID_U)%>%
  #   tidyterra::drop_na()%>%
  #   tidyterra::pull()%>%
  #   length()
  
  if(totalProtectedEcoregions == 0){
    ers <- 0 
  }else{
    ers <- (totalProtectedEcoregions/totalEcoregions)*100
  }
  
  # generate a gap map for the ERSin 
  mEcos <- totEco[!totEco$ECO_ID_U %in% totProEco$ECO_ID_U, ]
  
  missingEcos <- nativeArea |> 
    st_as_sf()|>
    dplyr::filter(ECO_ID_U %in% mEcos$ECO_ID_U) |> # needed to add the $ for filter to work
    terra::vect()|>
    terra::rasterize(y = thres)
  # export for the full 
  if(!is.null(rasterPath)){
    terra::writeRaster(x = missingEcos, filename = rasterPath,overwrite=TRUE)
  }
  
  
  df <- data.frame(ID=occuranceData$taxon[1],
                   SPP_N_ECO = totalEcoregions,
                   SPP_WITHIN_PA_N_ECO = totalProtectedEcoregions,
                   ERS = ers)
  return(df)
}
