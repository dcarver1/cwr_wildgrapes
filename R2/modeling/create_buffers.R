#' create buffers 
#'
#' @param speciesPoints 
#' @param natArea 
#' @param bufferDist 
#' @param templateRast 
#'
#' @return
#' @export
#'
#' @examples
create_buffers <- function(speciesPoints, natArea, bufferDist, templateRast){
  ## select all g points from point object
  p1 <- speciesPoints %>% filter(type == "G")
    
  #clause to test for G occurrences
  if(nrow(p1)== 0){
    print("there are no g points for this species")
  }else{
    ##buffering
    buffer <- sf::st_buffer(x = p1, dist = bufferDist )

    # set extent equal to native area
    r1 <- templateRast %>%
      terra::crop(natArea) %>%
      terra::mask(natArea)
    
    ##rasterizing and matching cells to predictor layers
    buffer_rs <- terra::rasterize(vect(buffer), r1)%>%
      terra::crop(natArea)%>%
      terra::mask(natArea)
    
    return(buffer_rs)
  }
}
























