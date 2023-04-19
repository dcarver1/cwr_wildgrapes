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
createSF_Objects <- function(speciesData, species){
  # select all  rows with valid lat long
  speciesData <- speciesData[speciesData$taxon == species, ]
  
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
removeDuplicates <- function(sf_points, species){
  sf_points <- sf_points[sf_points$taxon == species,]
  
  sf_points <- sf_points %>%
    filter(!duplicated(geometry) & type =="H")
  return(sf_points)
}



