
createNaturalAreas <- function(sp_object, species, ecoRegions){
  sp_object <- sp_object[sp_object$taxon == species, ]
  
  ids <- sp_object %>%
    sf::st_intersection(ecoRegions)%>%
    sf::st_drop_geometry()%>%
    dplyr::select("ECO_ID_U")%>%
    dplyr::distinct()%>%
    pull()
  
  natArea <- ecoRegions[ecoRegions$ECO_ID_U %in% ids, ]
  
}


ga50_buffers <- function(sp_object, species, nativeArea, templateRast, buffDist){
  sf::sf_use_s2(FALSE)
  # filter to species 
  sp_object <- sp_object[sp_object$taxon == species, ]
  # fitler to g points 
  sp_g <- sp_object[sp_object$type == "G", ]
  
  # buffer point object. 
  sp_b <- sf::st_buffer(x = sp_g, dist = buffDist)
  
  #conver to terra object
  
  t_1 <- terra::rasterize(x = vect(sp_b), y = templateRast)%>%
    terra::crop(y = nativeArea)
  
  
}
  