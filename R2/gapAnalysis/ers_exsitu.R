
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
ers_exsitu <- function(speciesData,thres,natArea,ga50) {

  if(class(ga50)[[1]] != "SpatRaster"){
    ers <- 0
    gEco <- NA
  }else{
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
    
    # determine ecoregions in ga50 area 
    v2 <- terra::zonal(x = ga50,z = n1,fun="sum",na.rm=TRUE)
    gEco <- v2 %>% 
      filter(layer >0)%>%
      nrow()
    
    # ERs calculation 
    ers <- min(c(100, (gEco/nEco)*100))
  }
  # generate dataframe
  out_df <- data.frame(ID=speciesData$taxon[1],
                       SPP_N_ECO=nEco,
                       G_N_ECO=gEco, 
                       ERS=ers)
  return(out_df)
}
  