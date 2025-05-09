#' Number of background points 
#'
#' @param natArea 
#'
#' @return
#' @export
#'
#' @examples
numberBackground <- function(natArea){
  ### need to determine what the logic behind this was. 
  ### n right now is total area in meters square, convert sq km
  n <- as.numeric(sum(sf::st_area(natArea))) * 0.000001
  
  ### 10 times the number of presence 
  
  if( n >= 10000){
    n <- 10000
  }else{
    n <- n
  }
  return(n)
}
