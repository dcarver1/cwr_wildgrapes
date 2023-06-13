#' crop G buffer. 
#'
#' @param ga50 
#' @param thres 
#'
#' @return Trim gbuffer object that will be used within the 
cropG_Buffer<-function(ga50, thres){
  thres[thres==0] <- NA
  ga50_mask <- ga50 * thres
  return(ga50_mask)
}
