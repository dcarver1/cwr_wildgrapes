

#' speciesCheck
#'
#' @param data : dataframe of unfilter occurance data
#' @param synonymList : reference data from for define what synonyms are expected 
#'
#' @return data for reassigned species names based on accepted synonyms 
#' 
speciesCheck <- function(data, synonymList){
  # check for each species on Taxon  
  nSpecies <- 1:length(synonymList$taxon)
  # map over nSpecies... 
  mapSynonyms <- function(nSpecies, synonymList, data){
    i <- nSpecies
    taxon <- synonymList$taxon[i]
    synonyms <- synonymList$acceptedSynonym[i]
    # grab all original taxon name 
    df2 <- data[data$taxon == taxon, ]
    
    # grab all species with species name 
    if(!is.na(synonyms)){
      syn1 <- synonyms |>
        stringr::str_split(pattern = ", ")|>
        unlist()
      for(j in syn1){
        print(j)
        df3 <- data[data$taxon == j, ]
        df3$taxon <- taxon
        df2 <- bind_rows(df2, df3)
      }
    }
    return(df2)
  }
  
  # gather data included
  ## there can be repeated records for taxon that are being modeled directly and also included as a synonym "Vitis aestivalis var. aestivalis"
  includedData <- nSpecies |>
    purrr::map(mapSynonyms, synonymList =synonymList, data = data)|>
    bind_rows()
  
  
  # define excluded data 
  excludedData <- data[!data$index %in% includedData$index, ]
  
  return(list(
    excludedData = excludedData,
    includedData = includedData
  ))
}
