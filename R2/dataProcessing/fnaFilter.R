
# speciesPoints <- sp1
# fnaData <- fnaData
applyFNA <- function(speciesPoints, fnaData, states) {
  
  # grab species name
  species <- speciesPoints$taxon[1]
  
  # spatial check for features that does
  noState <- speciesPoints[is.na(speciesPoints$state),]
  state <- speciesPoints[!is.na(speciesPoints$state),]
  # extract data to states 
  s1 <- terra::extract( terra::vect(states),vect(noState))
  noState$state <- s1$name
  noState$iso3 <- s1$adm0_a3 
  # joining 
  speciesPoints <- bind_rows(state, noState)
  
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
    nonUS <- speciesPoints |> dplyr::filter(iso3 != "USA")
    us <- speciesPoints |> 
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
  
}
