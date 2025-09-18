#' subsetSpecies
#'
#' @param speciesList : list of species being considered within the projecet
#'
#' @return dataframe of all unfiltered input datasets
#' 
#' #' 
#' subsetSpecies <- function(occuranceData, species){
#'   occuranceData[occuranceData$taxon == species, ]
#' }




#' createSF_Objects
#'
#' @param speciesData : all species occurance data
#' @param species : species list with full taxon name
#'
#' @return : dataframe of species data with valid lat long values  
createSF_Objects <- function(speciesData){

  latLong <- speciesData |>
    dplyr::filter(!is.na(latitude))|> 
    dplyr::filter(!is.na(longitude))|>
    mutate(latitude = as.numeric(as.character(latitude)),
           longitude = as.numeric(as.character(longitude))) |>
    dplyr::filter(!(latitude == 0 & longitude == 0))
  
  
  
  if(nrow(latLong)>0){
    coord <- latLong |> 
      sf::st_as_sf(coords = c("longitude","latitude"), crs = 4326,remove = FALSE )|>
      removeDuplicates()
    
  }else{
    print("there are no coodinate pairs for this species")
    coord <- "no data available"
  }
  return(coord)
}



countryCheck <- function(sf_points, speciesList, countryLists){
  # I don't have a great list of contries for dacaus so this is an optional element now 
  # makes me think we might 
}

#' removeDuplicates
#'
#' @param sf_points : point objects for individual species
#' @param species : speciesList object
#'
#' @return : a thinned version on the observations data were any feature with 
#' type == h and the same coordinates as another obervation has been removed. 
removeDuplicates <- function(sf_points){
  #subset out all g points 
  g_p <- sf_points |> 
    filter(type=="G")|>
    filter(!duplicated(geometry))
  h_p <- sf_points |> 
    filter(type=="H")|>
    filter(!duplicated(geometry))
  sf_p <- bind_rows(g_p, h_p)
  
  return(sf_p)
}




#' Native Area shp
#'
#' @param species 
#'
#' @return
#' @export
#'
#' @examples
nat_area_shp <- function(speciesPoints, ecoregions) {
  sf::sf_use_s2(FALSE)
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



