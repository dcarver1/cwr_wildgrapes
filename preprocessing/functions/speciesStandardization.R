

synonymList <- read_csv("data/vitis/synonymList.csv")

View(synonymList)

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
    
    # synonym match 
    if(!is.na(synonymList$Synonyms[i])){
      synonyms <- synonymList$Synonyms[i] %>%
        stringr::str_split(pattern = ";")%>%
        unlist()
      df3 <- data[data$taxon %in% synonyms, ] %>%
        dplyr::mutate(taxon = synonymList$`Taxon Name`[i])
      
      df1 <- df1 %>%
        bind_rows(df2)%>%
        bind_rows(df3)
    }else{
      df1 <- df1 %>%
        bind_rows(df2)
    }
  rm(df2, df3)
  
  

  }
  #### 20230724: something not lining up here. Either values area being pulled across species, 
  ### I think it has to do with specuf 
  # exclude the data that has been assigned 
  df1a <- data[!data$index %in% df1$index, ]
  
  df1a <- anti_join(data,df1)
  
  df1b <- data[data$sourceUniqueID %in% df1$sourceUniqueID, ]
  
  # check for each species on Taxon  
  ## pull exact matched names 
  ## pull synonyms and rename 
  
}