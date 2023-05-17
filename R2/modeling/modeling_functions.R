
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
  ### n right now is total area in meters square
  n <- sum(sf::st_area(natArea)) 

    if( n >= 5000){
    n <- 5000}else{
      n <- n
    }
  return(n)
}


#' Generate modeling dataset
#'
#' @param speciesPoints 
#' @param natArea 
#' @param bioVars 
#'
#' @return
#' @export
#'
#' @examples
generateModelData <- function(speciesPoints,natArea,bioVars){
  
  # generate background points 
  ## format species data
  sp1 <- speciesPoints %>%
    mutate("presence" = 1)%>%
    select(presence,geometry)
  
  
  ## this will need to be adjust if species if only present in a small area 
  bg1 <- sf::st_sample(x = natArea, size = 5000)%>%
    sf::st_as_sf()%>%
    mutate("presence" = 0)%>%
    select(presence,"geometry" = x)%>%
    filter(!geometry %in% sp1$geometry) # test for same coordinated between presence and background data
  # bind datasets 
  d1 <- bind_rows(sp1, bg1)
  
  # extract values from rasters 
  ## I don't know if I need this a spatial object at this point, I don't thin so
  ## Also this are no reference values for what points aligns with which input location. 
  d2 <- terra::extract(x = bioVars, y = vect(d1), bind= TRUE)

  return(st_as_sf(d2)) 
}





varaibleSelection <- function(modelData){
  # subset predictor data and presence column
  varOnly <- modelData %>% st_drop_geometry() %>% select(-presence)
  # remove all na from dataframe
  test2 <-complete.cases(varOnly)
  # drop all column from bioValues set as well so the same data is used for maxnet modeling.
  bioValues <- modelData[test2,] %>% st_drop_geometry()
  # redefine var select to in
  varSelect <- bioValues %>% st_drop_geometry() %>% select(-presence)
  # Maximum modelled data
  #write.csv(x = bioValues, file = paste0(sp_dir, "/modeling/maxent/bioValuesForPresencePoints.csv"))
  
  
  # # #vsurf
  ### Considered altering the number of trees, 100 is somewhat low for the
  # number of predictors used. It was a time concern more then anything.
  # vsurfThres <- VSURF_thres(x=bioValues[,1:26] , y=as.factor(bioValues$presence) ,
  #                           ntree = 100 )
  ### change for 30 arc second run 
  vsurfThres <- VSURF_thres(x=bioValues[,2:27] , y=as.factor(bioValues$presence) ,
                            ntree.thres = 100, parallel = TRUE )
  ###
  #correlation matrix
  ###
  
  # define predictor list based on Run
  inputPredictors <- vsurfThres$varselect.thres
  
  # ordered predictors from our variable selection
  predictors <- varSelect[,c(inputPredictors)]
  # Calculate correlation coefficient matrix
  correlation <-cor(predictors, method="pearson")
  #change self correlation value
  
  # #define the list of top 15 predictors
  varNames <- colnames(correlation)
  # empty list containing the variables tested
  varsTested <- c()
  #loop through the top 5 predictors to remove correlated varables.
  for( i in 1:5){
    print(varNames[i])
    if(varNames[i] %in% varNames){
      # add variable to the test list
      varsTested <- c(varsTested, varNames[i])
      # Test for correlations with predictors
      vars <- correlation[(i+1):nrow(correlation),i] > 0.7 | correlation[(i+1):nrow(correlation),i] < -0.7
      # Select correlated values names
      corVar <- names(which(vars == TRUE))
      #test is any correlated variables exist
      if(length(corVar) >0 ){
        # loop through the list of correlated variables
        varNames <- varNames[!varNames  %in% corVar]
        print(paste0("the variable ", corVar, " was removed"))
      }
    }else{
      print("this variable has been removed already")
    }
  }
  
  # include all variables that were tested.
  for(p in varsTested){
    if(p %in% varNames){
    }else{
      varNames <- c(varNames, p)
    }
  }# It's a little bit confusing why variable are being dropped after they area tested. Correlation
  # should be the same in both directs. This is just a test to make sure it works.
  
  
  #create a dataframe of the top predictors and
  rankPredictors <- data.frame(matrix(nrow = length(colnames(correlation)),ncol = 3))
  rankPredictors$varNames <- colnames(correlation)
  rankPredictors$importance <- vsurfThres$imp.varselect.thres
  rankPredictors$includeInFinal <- colnames(correlation) %in% varNames
  rankPredictors <- rankPredictors[,4:6]
  # write.csv(x = rankPredictors, file = paste0(sp_dir, "/modeling/maxent/predictorImportance.csv"))
  
  variblesToModel <- varSelect[,varNames]
  
  return(list(
    allModelData = bioValues, 
    rankPredictors = rankPredictors,
    variblesToModel = variblesToModel
  ))
}
