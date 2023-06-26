#' Final Conservation Score Combined 
#'
#' @param fcsin dataframe of fcsin values
#' @param fcsex dataframe of fcsexvalues 
#'
#' @return dataframe of combined summayr values 
fcs_combine <- function(fcsin, fcsex) {
  
  #compute FCSc_min and FCSc_max
  data_comb <- data.frame(ID=fcsin$ID,
                          FCSex=fcsex$FCS, 
                          FCSin=fcsin$FCS)
  
  
  data_comb$FCSc_min <- min(c(data_comb$FCSex,data_comb$FCSin),na.rm=T)
  data_comb$FCSc_max <- max(c(data_comb$FCSex,data_comb$FCSin),na.rm=T)
  data_comb$FCSc_mean <- mean(c(data_comb$FCSex,data_comb$FCSin),na.rm=T)
  
  #assign classes (min)
  if (data_comb$FCSc_min < 25) {
    data_comb$FCSc_min_class <- "UP"
  } else if (data_comb$FCSc_min >= 25 & data_comb$FCSc_min < 50) {
    data_comb$FCSc_min_class <- "HP"
  } else if (data_comb$FCSc_min >= 50 & data_comb$FCSc_min < 75) {
    data_comb$FCSc_min_class <- "MP"
  } else {
    data_comb$FCSc_min_class <- "LP"
  }
  
  #assign classes (max)
  if (data_comb$FCSc_max < 25) {
    data_comb$FCSc_max_class <- "UP"
  } else if (data_comb$FCSc_max >= 25 & data_comb$FCSc_max < 50) {
    data_comb$FCSc_max_class <- "HP"
  } else if (data_comb$FCSc_max >= 50 & data_comb$FCSc_max < 75) {
    data_comb$FCSc_max_class <- "MP"
  } else {
    data_comb$FCSc_max_class <- "LP"
  }
  
  #assign classes (mean)
  if (data_comb$FCSc_mean < 25) {
    data_comb$FCSc_mean_class <- "UP"
  } else if (data_comb$FCSc_mean >= 25 & data_comb$FCSc_mean < 50) {
    data_comb$FCSc_mean_class <- "HP"
  } else if (data_comb$FCSc_mean >= 50 & data_comb$FCSc_mean < 75) {
    data_comb$FCSc_mean_class <- "MP"
  } else {
    data_comb$FCSc_mean_class <- "LP"
  }

  return(data_comb)
}  
  