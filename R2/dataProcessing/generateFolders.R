#' Generate Folders 
#'
#' @param paths : all the list object 
#' @description : generate all the required file directories 
generateFolders <- function(paths){
  
  selectedPaths <- paths[c(1:5)] |> unlist()
  
  for(i in selectedPaths){
    if(!dir.exists(i)){
      dir.create(i)
    }
  }
  
}
