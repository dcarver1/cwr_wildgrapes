#' subsetSpecies
#'
#' @param speciesList : list of species being considered within the projecet
#'
#' @return dataframe of all unfiltered input datasets
#' 
#' 
subsetSpecies <- function(occuranceData, species){
  occuranceData[occuranceData$taxon == species, ]
}


#' generateCounts
#'
#' @param speciesData : raw data of an individual species
#'
#' @return : dataframe summarizing input data from a given species
#'
generateCounts <- function(speciesData){
  # define presence of usable lat long values
  dataThin <- speciesData %>%
    dplyr::select(c("taxon", "latitude", "longitude", "type","databaseSource")) %>%
    mutate(hasLat = !is.na(latitude) & latitude != "\\N" & latitude != "" & !is.null(latitude) & latitude != "NULL") %>%
    mutate(hasLong = !is.na(longitude) & longitude != "\\N"& longitude != "" & !is.null(longitude)& longitude != "NULL") %>%
    mutate(hasLatLong = hasLat & hasLong)
  
  # set column names for counts df
  colNames <- c("species","totalRecords",	"hasLat", "hasLong","totalUseful", 	"totalGRecords",
                "totalGUseful","totalHRecords",	"totalHUseful","numberOfUniqueSources")
  # summarize data
  tbl <- dataThin %>%
    dplyr::group_by(type, hasLatLong )%>%
    dplyr::summarize(total = n())
  
  # generate counts df
  countsData <- data.frame(matrix(NA, nrow = 1, ncol = 10))
  colnames(countsData) <- colNames
  #assign values to counts df
  countsData$species <- unique(dataThin$taxon)
  countsData$totalRecords <- nrow(dataThin)
  countsData$totalUseful <- sum((subset(tbl, hasLatLong == TRUE))$total)
  countsData$totalGRecords <- sum((subset(tbl, type == "G"))$total)
  countsData$totalGUseful <- sum((subset(tbl, type == "G" & hasLatLong == TRUE))$total)
  countsData$totalHRecords <- sum((subset(tbl, type == "H"))$total)
  countsData$totalHUseful <- sum((subset(tbl, type == "H" & hasLatLong == TRUE))$total)
  countsData$hasLat <- sum(dataThin$hasLat)
  countsData$hasLong <- sum(dataThin$hasLong)
  countsData$numberOfUniqueSources <- n_distinct(speciesData$databaseSource)
  return(countsData)
}


#' createSF_Objects
#'
#' @param speciesData : all species occurance data
#' @param species : species list with full taxon name
#'
#' @return : dataframe of species data with valid lat long values  
createSF_Objects <- function(speciesData){

  latLong <- speciesData %>%
    mutate(latitude = as.numeric(as.character(latitude)),
           longitude = as.numeric(as.character(longitude)))%>%
    dplyr::filter(!is.na(latitude) | !is.na(longitude))
  
  if(nrow(latLong)>0){
    coord <- latLong %>% 
      sf::st_as_sf(coords = c("longitude","latitude"), crs = 4326)
    
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
  g_p <- sf_points %>% 
    filter(type=="G")%>%
    filter(!duplicated(geometry))
  h_p <- sf_points %>% 
    filter(type=="H")%>%
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
  
  
  ids <- speciesPoints %>%
    sf::st_intersection(ecoregions)%>%
    sf::st_drop_geometry()%>%
    dplyr::select("ECO_ID_U")%>%
    dplyr::distinct()%>%
    pull()
  
  natArea <- ecoregions[ecoregions$ECO_ID_U %in% ids, ]
  return(natArea)
}



