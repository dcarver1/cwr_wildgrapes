# highlight two things 
# 1. the counts of all records 
# 2. the ranking of individuals collectors/germplasm sources 

databaseSourcesSummary <- function(data){
  # all data 
  databaseSource <- data |>
    dplyr::group_by(databaseSource)|>
    dplyr::count(sort = TRUE) 
  
  institutionCode <- data |>
    dplyr::group_by(institutionCode)|>
    dplyr::count(sort = TRUE) 
  
  observerName <- data |>
    dplyr::group_by(observerName)|>
    dplyr::count(sort = TRUE) 
  # store values 
  allData <- list(
    databaseSource = databaseSource,
    institutionCode = institutionCode,
    observerName = observerName
  )
  
  
  # limit to G data with coordinates 
  gData <- data|>
    dplyr::filter(type == "G" & !is.na(longitude))
  
  databaseSourceG <- gData |>
    dplyr::group_by(databaseSource)|>
    dplyr::count(sort = TRUE) 
  
  institutionCodeG <- gData |>
    dplyr::group_by(institutionCode)|>
    dplyr::count(sort = TRUE) 
  
  observerNameG <- gData |>
    dplyr::group_by(observerName)|>
    dplyr::count(sort = TRUE) 
  # store values 
  gData <- list(
    databaseSource = databaseSourceG,
    institutionCode = institutionCodeG,
    observerName = observerNameG
  )
  output <- list(
    allData = allData,
    gData = gData
  ) 
  return(output)
}
