#' Final Conservation Score Exsitu 
#'
#' @param srsex 
#' @param grsex 
#' @param ersex 
#'
#' @return
fcs_exsitu <- function(srsex, grsex, ersex, noModel, gPoints) {
    # 

    if(noModel==TRUE){
      if(gPoints > 0){
        # points are present but no modle so can't generate GRS, ERS 
        out_df <- data.frame(ID=srsex$ID,
                             SRS=srsex$SRS,
                             GRS= NA,
                             ERS= NA,
                             FCS= srsex$SRS,
                             FCS_Score = NA)
      }else{
        # no go points so values are assigned as zero 
        out_df <- data.frame(ID=srsex$ID,
                             SRS=srsex$SRS,
                             GRS=0,
                             ERS=0,
                             FCS=srsex$SRS/3,
                             FCS_Score = NA)
      }
   
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
