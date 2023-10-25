
# helper funcitons --------------------------------------------------------
is.integer0 <- function(x){
  is.integer(x) && length(x) == 0L
}

orderNames <- function(data, names){
  d1 <- data %>%
    dplyr::select(all_of(names))%>%
    mutate(across(everything(), as.character))
  return(d1)
}

removeDuplicatesID <- function(data){
  d1 <- data[!duplicated(data$sourceUniqueID), ]
  return(d1)
}

summarizeBySource <- function(data){
  d1 <- data %>% 
    group_by(databaseSource)%>%
    summarise(count = n())
  return(d1)
}
