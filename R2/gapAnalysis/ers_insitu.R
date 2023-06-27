


ers_insitu <- function(occuranceData,nativeArea, protectedArea, thres){
  # total e
  
  # mask protected areas layer 
  thres[thres == 0] <- NA
  # crop protected areas raster 
  p1 <- terra::crop(x = protectedArea, y = thres)
  # multiple to create mask 
  p1 <- p1 * thres
  
  ## point object of the protected area 
  protectedPoints <- terra::as.points(x = p1)
  
  thresPoints <- terra::as.points(x = thres)
  
  # convert native area to a vect object
  nativeArea <- vect(nativeArea) %>% 
    tidyterra::select("ECO_ID_U")
  
  # total number of eco regions within the SDM 
  totalEcoregions <- terra::extract(x = nativeArea, y = thresPoints) %>%
    tidyterra::distinct(ECO_ID_U)%>%
    tidyterra::drop_na()%>%
    tidyterra::pull()%>%
    length()
  # total number of eco regions within the SDM with protect areas. 
  totalProtectedEcoregions <- terra::extract(x = nativeArea, y = protectedPoints)%>%
    tidyterra::distinct(ECO_ID_U)%>%
    tidyterra::drop_na()%>%
    tidyterra::pull()%>%
    length()
  
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
