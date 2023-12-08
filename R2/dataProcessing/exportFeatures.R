
#' writeCSV
#'
#' @param path : path to the object
#' @param overwrite : Binary true/false value 
#' @param fuction1 : the function that is being applied 
#'
#' @return Dataframe of sort. 
write_CSV <-function(path, overwrite, function1){
  if(!file.exists(path) | isTRUE(overwrite)){
    result <- function1
    write_csv(x = result, file = path)
    return(result)
    }else{
    return(read_csv(path))
  }
}



#' writeGPKG
#'
#' @param path : path to file location  
#' @param overwrite :TRUE/FALSE values to determine if function shoudld be ran
#' @param function1 : function to generate output
#'
#' @return : write and read the products of the function 
write_GPKG <- function(path, overwrite, function1){
  if(!file.exists(path) | isTRUE(overwrite)){
    result <- function1
    if(class(result)[1]=="sf"){
    sf::write_sf(obj = result, dsn = path)
    }
    return(result)
  }else{
    return(sf::read_sf(path))
  }
}

#' writeRaster 
#'
#' @param path 
#' @param overwrite 
#' @param function1 
#'
#' @return
write_Rast <- function(path, overwrite, function1){
  if(!file.exists(path)| isTRUE(overwrite)){
    result <- function1
    if(class(result)=="SpatRaster"){
      terra::writeRaster(x = result, 
                         file = path, 
                         overwrite = TRUE)
    }
    return(result)
  }else{
    return(terra::rast(path))
  }
}

### the terra objects inside of the list are not being contained well. 
### need to figure out a way to wrap things before exporting (look to targets workflow)
write_RDS <- function(path, overwrite, function1){
  if(!file.exists(path) | isTRUE(overwrite)){
    result <- function1
    write_rds(x = result, file = path)
    return(result)
  }else{
    return(readRDS(path))
  }
}



