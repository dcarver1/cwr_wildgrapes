
# calculates the total ecoregions within modeled area where G occurrences have
# been collected
# carverd@colostate.edu 
# 20230613
###

#' Ecological Representatiativeness Score Exsitu collections 
#'
#' @param speciesData 
#' @param thres 
#' @param natArea 
#' @param ga50 
#'
#' @return Data frames with ERS score details. 
ers_exsitu <- function(speciesData,thres,natArea,ga50, rasterPath) {

  # convert natural area object in a vect feature
  n1 <- natArea %>% 
    dplyr::select(ECO_ID_U)%>% 
    vect()
  
  
  v1 <- terra::zonal(x = thres,z = n1,fun="sum",na.rm=TRUE)
  v1$ECO_ID_U <- n1$ECO_ID_U
  
  # Number of ecoregions considered. 
  nEco <- v1 %>% 
    filter(Threshold > 0)%>%
    nrow()
  
  
  if(class(ga50)[[1]] != "SpatRaster"){
    ers <- 0
    gEco <- NA
    missingEcos <- v1$ECO_ID_U
  }else{

    
    # determine ecoregions in ga50 area 
    v2 <- terra::zonal(x = ga50,z = n1,fun="sum",na.rm=TRUE)
    v2$ECO_ID_U <- n1$ECO_ID_U
    
    # determine the ecoregions that are not being considered 
    areasWithGBuffer <- v2 |> 
      filter(layer >0) |>
      filter(!is.nan(layer)) 
    # get the total number number of eco regions with a g buffer area
    gEco <- areasWithGBuffer |> 
      nrow()
    # generate a list of the ecoregions ID that are inside the threshold but have no g buffer 
    missingEcos <- v1 |> 
      dplyr::filter(Threshold >0) |>
      dplyr::filter(!ECO_ID_U %in% areasWithGBuffer$ECO_ID_U)|>
      dplyr::select(ECO_ID_U)|>
      pull()
    
    # ERs calculation 
    ers <- min(c(100, (gEco/nEco)*100))

  }
  
  # produce threshold map excluding collected eco regions. 
  n2 <- n1 |>
    dplyr::filter(ECO_ID_U %in% missingEcos)|>
    terra::rasterize(y = thres)
  terra::writeRaster(x = n2, filename = rasterPath,overwrite=TRUE)
  
  # generate filter 
  
  out_df = data.frame(ID=speciesData$taxon[1],
                  SPP_N_ECO=nEco,
                  G_N_ECO=gEco, 
                  ERS=ers)
  out_df$missingEcos <- list(missingEcos)

  # generate dataframe
  return(out_df)
}
  