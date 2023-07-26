

speciesCheck <- function(data, synonymList){
  # create empty df to hold matched datasets 
  data$index <- 1:nrow(data)
  df1 <- data[0,]
  # check for each species on Taxon  
  ## pull exact matched names 
  ## pull synonyms and rename 
  for(i in seq_along(synonymList$`Taxon Name`)){
    species <- synonymList$`Taxon Name`[i]
    # taxon match 
    df2 <- data[data$taxon == species, ]
    # redefine data to remove any element that was added to new df 
    data <- data[!data$index %in% df2$index, ]
    # synonym match 
    if(!is.na(synonymList$Synonyms[i])){
      synonyms <- synonymList$Synonyms[i] %>%
        stringr::str_split(pattern = ";")%>%
        unlist()
      df3 <- data[data$taxon %in% synonyms, ] %>%
        dplyr::mutate(taxon = synonymList$`Taxon Name`[i])
      
      # redefine data to remove any element that was added to new df 
      data <- data[!data$index %in% df3$index, ]
      
      df1 <- df1 %>%
        bind_rows(df2)%>%
        bind_rows(df3)
    }else{
      df1 <- df1 %>%
        bind_rows(df2)
    }
  rm(df2, df3)

  }
  return(list(
    excludedData = data,
    includedData = df1
  ))
}
