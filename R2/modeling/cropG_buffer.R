#' crop G buffer. 
#'
#' @param ga50 
#' @param thres 
#'
#' @return Trim gbuffer object that will be used within the 
cropG_Buffer<-function(ga50, thres){
  if(class(ga50) != "character"){
    thres[thres==0] <- NA

    ga50_mask <- ga50 * thres
  }else{
    ga50_mask <-  "there are no g points for this species"
  }
  
  return(ga50_mask)
  
}
