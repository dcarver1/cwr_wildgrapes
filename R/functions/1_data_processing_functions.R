#' subsetSpecies
#'
#' @param speciesList : list of species being considered within the projecet
#'
#' @return
subsetSpecies <- function(occuranceData, species){
  d1 <- occuranceData %>% 
    dplyr::filter(Taxon == species)
  return(d1)
}


