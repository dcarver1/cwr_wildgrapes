
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
  # split out NA values in sourceUniqueID column 
  d1 <- data[is.na(data$sourceUniqueID), ]
  # grap all described values
  d2 <- data[!is.na(data$sourceUniqueID), ]
  # remove duplicates 
  d2 <- d2[!duplicated(d2$sourceUniqueID), ]
  # combine back with original.
  d3 <- bind_rows(d2,d1)
  return(d3)
}

summarizeBySource <- function(data){
  d1 <- data %>% 
    group_by(databaseSource)%>%
    summarise(count = n())
  return(d1)
}
