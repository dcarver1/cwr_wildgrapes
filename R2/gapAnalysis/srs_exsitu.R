#' SRS Exsitu 
#'
#' @param sp_counts 
#'
#' @return Dataframe that summarizes the srs exist content. 
srs_exsitu <- function(sp_counts){
  
  # caluse for no g points
  if(sp_counts$totalGRecords >= 1 & sp_counts$totalHRecords == 0){
    srs <-100
  }
  
  #clause for no data
  if (sp_counts$totalGRecords == 0 & sp_counts$totalHRecords ==0) {
    srs <- 0
  } else {
    # clause for species with data
    srs <- min(c(100,sp_counts$totalGRecords/sp_counts$totalHRecords*100))
  }
  
  
  #create data.frame with output
  out_df <- data.frame(ID=sp_counts$species,
                       NTOTAL=sp_counts$totalRecords,
                       NTOTAL_COORDS=sp_counts$totalUseful,
                       NG= sp_counts$totalGRecords,
                       NG_COORDS=sp_counts$totalGUseful,
                       NH=sp_counts$totalHRecords,
                       NH_COORDS=sp_counts$totalHUseful,
                       SRS=srs)
  return(out_df)
}