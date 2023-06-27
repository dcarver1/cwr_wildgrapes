#' evaluateTable
#'
#' @param sdm_result 
#'
#' @return : a data.table with the summarized results of the modeling process 
evaluateTable <- function(sdm_result){
  if(class(sdm_results$do.projections[[1]])!="RasterLayer"){
    cat(paste(species," not modelled yet"),"\n")
    evaluationTable <- paste(species," not modelled yet")
  }else{ 
    evaluationTable <- data.frame(
      AUCtrain = do.call(rbind, sdm_results$AUC_train),
      AUCtest = do.call(rbind,sdm_results$AUC),
      nAUC = do.call(rbind,sdm_results$nAUC),
      cAUC = do.call(rbind,sdm_results$cAUC)
    )
    atrain <- do.call(rbind,sdm_results$evaluation_train)
    colnames(atrain) <- paste0(colnames(atrain),"_","train")
    
    atest <- do.call(rbind,sdm_results$evaluation_test)
    colnames(atest) <- paste0(colnames(atest),"_","test")
    
    evaluationTable <- evaluationTable %>% 
      bind_cols(atrain, atest)
  }
  return(evaluationTable)
}