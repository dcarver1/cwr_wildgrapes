#' Run Variable Selection
#'
#' @param modelData
#'
#' @return A csv of occurance data that has been thinned to include on primary variables.
varaibleSelection <- function(modelData, parallel) {
  # subset predictor data and presence column
  varOnly <- modelData |>
    st_drop_geometry() |>
    dplyr::select(-presence)
  # remove all na from dataframe
  test2 <- complete.cases(varOnly)
  # drop all column from bioValues set as well so the same data is used for maxnet modeling.
  bioValues <- modelData |>
    st_drop_geometry()
  
  # redefine var select to in
  varSelect <- bioValues |>
    dplyr::select(-presence, -type)
  
  # # #vsurf
  ### Considered altering the number of trees, 100 is somewhat low for the
  # number of predictors used. It was a time concern more then anything.
  # vsurfThres <- VSURF_thres(x=bioValues[,1:26] , y=as.factor(bioValues$presence) ,
  #                           ntree = 100 )
  ### change for 30 arc second run
  bio_no_na <- bioValues |>
    dplyr::select(-type) |>
    tidyr::drop_na()
  
  vsurfThres <- VSURF_thres(
    x = bio_no_na[, c(2:26)],
    y = as.factor(bio_no_na$presence),
    parallel = parallel
  )
  
  ###
  #correlation matrix
  ###
  
  # define predictor list based on Run
  inputPredictors <- vsurfThres$varselect.thres
  
  # ordered predictors from our variable selection
  predictors <- varSelect[, c(inputPredictors)]
  
  # Calculate correlation coefficient matrix
  correlation <- predictors |>
    dplyr::select(where(is.numeric)) |>
    cor(method = "pearson")
  
  # define the list of predictors in order of importance
  varNames <- colnames(correlation)
  
  # Initialize an empty vector to store correlated variables flagged for removal
  varsToRemove <- c()
  
  # loop through the top 5 predictors to identify correlated variables
  for (i in 1:min(5, length(varNames))) {
    currentVar <- varNames[i]
    
    # Only test correlations if the current variable hasn't already been flagged for removal
    if (!(currentVar %in% varsToRemove)) {
      
      # Ensure we do not index out of bounds if i equals the number of rows
      if (i < nrow(correlation)) {
        # Test for correlations greater than 0.7 or less than -0.7
        vars <- correlation[(i + 1):nrow(correlation), i] > 0.7 |
          correlation[(i + 1):nrow(correlation), i] < -0.7
        
        # Select names of highly correlated variables
        corVar <- names(which(vars == TRUE))
        
        if (length(corVar) > 0) {
          # Add to the removal list, ensuring no duplicates
          varsToRemove <- unique(c(varsToRemove, corVar))
          print(paste0("Variables flagged for removal due to correlation with ", currentVar, ": ", paste(corVar, collapse = ", ")))
        }
      }
    } else {
      print(paste0("Variable ", currentVar, " was previously flagged for removal. Skipping correlation test."))
    }
  }
  
  # Filter the main variable list to exclude the highly correlated variables
  varNames <- varNames[!varNames %in% varsToRemove]
  
  #create a dataframe of the top predictors and
  rankPredictors <- data.frame(matrix(
    nrow = length(colnames(correlation)),
    ncol = 3
  ))
  rankPredictors$varNames <- colnames(correlation)
  rankPredictors$importance <- vsurfThres$imp.varselect.thres
  rankPredictors$includeInFinal <- colnames(correlation) %in% varNames
  rankPredictors <- rankPredictors[, 4:6]
  
  # filter the input sf object based on rank order of selected variables.
  variblesToModel <- modelData[, c("presence", varNames, "geometry")]
  
  return(list(
    rankPredictors = rankPredictors,
    variblesToModel = variblesToModel
  ))
}