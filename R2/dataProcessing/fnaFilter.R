
# speciesPoints <- sp1
# fnaData <- fnaData
applyFNA <- function(speciesPoints, fnaData) {
  
  # grab species name
  species <- speciesPoints$taxon[1]
  
  # filter fna to exclude states with no measures 
  ## nchar because it's not a true NA value 
  fna<- fnaData |> 
    dplyr::filter(nchar(`States from FNA`) > 3)
  # test for present in FNA 
  if(species %in% fna$`Taxon Name`){
    # pull accepted states 
    states_to_filter <- fna |> 
      dplyr::filter(`Taxon Name` == species ) |>
      dplyr::select(`States from FNA` )|>
      dplyr::pull() |>
      stringr::str_split(pattern = ", ") |> 
      unlist()|>
      stringr::str_remove_all(",")
    
    # filter points based on name in states 
    nonUS <- sp1 |> dplyr::filter(iso3 != "USA")
    us <- sp1 |> 
      dplyr::filter(iso3 == "USA") |>
      dplyr::filter(state %in% states_to_filter)

    # combine together 
    bindData <- dplyr::bind_rows(us, nonUS)
    return(bindData)  
  }else{
    return(speciesPoints)
  }
  
  
  if(!crs(speciesPoints) == crs(ecoregions)){
    speciesPoints <- sf::st_transform(x = speciesPoints, crs = crs(ecoregions))
  }
  
  
  ids <- speciesPoints |>
    sf::st_intersection(ecoregions)|>
    sf::st_drop_geometry()|>
    dplyr::select("ECO_ID_U")|>
    dplyr::distinct()|>
    pull()
  natArea <- ecoregions[ecoregions$ECO_ID_U %in% ids, ]|>
    sf::st_make_valid()
  
  return(natArea)
}
