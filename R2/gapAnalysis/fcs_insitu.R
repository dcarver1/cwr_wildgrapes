#' Final Conservation Score insitu 
#'
#' @param srsin 
#' @param grsin 
#' @param ersin 
#'
#' @return
fcs_insitu <- function(srsin, grsin, ersin, noModel){
  
  if(noModel == TRUE){
    out_df <- data.frame(ID=srsin$ID, 
                         SRS=srsin$SRS, 
                         GRS=NA,
                         ERS=NA, 
                         FCS=srsin$SRS, # this might issues when zero... 
                         FCS_Score = NA)
  }else{
    # calculate the mean across the three measures 
    sp_fcs <- sum(c(srsin$SRS,grsin$GRS,ersin$ERS), na.rm=T) /3

    out_df <- data.frame(ID=srsin$ID, 
                         SRS=srsin$SRS, 
                         GRS=grsin$GRS,
                         ERS=ersin$ERS, 
                         FCS=sp_fcs, 
                         FCS_Score = NA)
    
  }
  sp_fcs <- out_df$FCS
  #assign classes (min)
  if (sp_fcs < 25) {
    score <- "UP"
  } else if (sp_fcs >= 25 & sp_fcs < 50) {
    score <- "HP"
  } else if (sp_fcs >= 50 & sp_fcs < 75) {
    score <- "MP"
  } else {
    score <- "LP"
  }
  
  out_df$FCS_Score <- score
  return(out_df)
}
