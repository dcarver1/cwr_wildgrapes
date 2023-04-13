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


