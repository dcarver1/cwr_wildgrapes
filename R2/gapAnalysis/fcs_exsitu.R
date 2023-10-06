#' Final Conservation Score Exsitu 
#'
#' @param srsex 
#' @param grsex 
#' @param ersex 
#'
#' @return
fcs_exsitu <- function(srsex, grsex, ersex, noModel) {
    # 
    if(noModel==TRUE){
      out_df <- data.frame(ID=srsex$ID,
                           SRS=srsex$SRS,
                           GRS=NA,
                           ERS=NA,
                           FCS=srsex$SRS/3,
                           FCS_Score = NA)
    }else{
      # calculate the mean across the three measures 
      sp_fcs <- mean(c(srsex$SRS,grsex$GRS,ersex$ERS), na.rm=T)

      out_df <- data.frame(ID=srsex$ID,
                           SRS=srsex$SRS,
                           GRS=grsex$GRS,
                           ERS=ersex$ERS,
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
