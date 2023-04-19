
createNaturalAreas <- function(sp_object, species, ecoRegions){
  sf::sf_use_s2(FALSE)
  sp_object <- sp_object[sp_object$taxon == species, ]
  
  ids <- sp_object %>%
    sf::st_intersection(ecoRegions)%>%
    sf::st_drop_geometry()%>%
    dplyr::select("ECO_ID_U")%>%
    dplyr::distinct()%>%
    pull()
  
  natArea <- ecoRegions[ecoRegions$ECO_ID_U %in% ids, ]
  
}
