


#' Assign model prioity 
#'
#' @param data 
#'
#' @return
#' @export
#'
#' @examples
assignPriority <- function(data){
  # G points == 2 
  # I nat == 3 
  # else == 2 
  
  data <- data |>
    dplyr::mutate(
      modelPriority = case_when(
        type == "G" ~ 1,
        grepl(pattern = "https://www.inaturalist.org",sourceUniqueID) ~ 3,
        .default = 2
      )
    )
  return(data)
}