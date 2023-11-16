


ers_insitu <- function(occuranceData,nativeArea, protectedArea, thres){
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
  nativeArea <- vect(nativeArea)
  # total number of eco regions within the SDM 
  totalEcoregions <- terra::zonal(x = thres ,z = nativeArea, fun = "sum",na.rm=TRUE)|>
    dplyr::mutate(ECO_ID_U = nativeArea$ECO_ID_U)|> 
    dplyr::filter(Threshold > 0) |>
    nrow()
  
  
  # totalEcoregions <- terra::extract(x = nativeArea, y = thresPoints) |>
  #   dplyr::distinct(ECO_ID_U) %>%
  #   tidyterra::drop_na()%>%
  #   tidyterra::pull()%>%
  #   length()
  # total number of eco regions within the SDM with protect areas. 
  totalProtectedEcoregions <- terra::zonal(x = p1 ,z = nativeArea, fun = "sum",na.rm=TRUE)|>
    dplyr::mutate(ECO_ID_U = nativeArea$ECO_ID_U) |> 
    dplyr::filter(layer > 0) |>
    nrow()
  
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
  
  
  df <- data.frame(ID=occuranceData$taxon[1],
                   SPP_N_ECO = totalEcoregions,
                   SPP_WITHIN_PA_N_ECO = totalProtectedEcoregions,
                   ERS = ers)
  return(df)
}
