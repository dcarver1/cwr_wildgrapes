
# speciesPoints <- sp1
# fnaData <- fnaData
applyFNA <- function(speciesPoints, fnaData, states) {
  
  # grab species name
  species <- speciesPoints$taxon[1]
  
  # spatial check for features and assign county/iso3 
  s1 <- terra::extract( terra::vect(states),vect(speciesPoints))
  
  # remove NA values 
  speciesPoints$stateTest <- s1$name
  speciesPoints$iso3Test <- s1$adm0_a3 
  # reassign the state based on the lat lon when possible 
  speciesPoints <- speciesPoints |>
    dplyr::mutate(iso3 = case_when(
      is.na(iso3Test) ~ iso3,
      !is.na(iso3Test) ~ iso3Test
    ),
    state = case_when(
      is.na(stateTest) ~ state,
      !is.na(stateTest) ~ stateTest
    )
  )|>
    dplyr::filter(
      !is.na(iso3) & !is.na(state) , 
      longitude < 0 ,
      latitude < 70
    )
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
    nonNA <- speciesPoints |>
      dplyr::filter(!iso3 %in% c("USA", "MEX","CAN"))
    pointsNA <- speciesPoints |> 
      dplyr::filter(state %in% states_to_filter)

    if(nrow(nonNA) > 0){
      # combine together 
      bindData <- dplyr::bind_rows(pointsNA, nonNA)
    }else{
      bindData <- pointsNA
    }
    return(bindData)  
  }else{
    return(speciesPoints)
  }
}
