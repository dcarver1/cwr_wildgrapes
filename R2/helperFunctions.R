sourceFiles <- function(gapAnalysisOnly, furrr=FALSE){
  if(furrr == TRUE){
    f1 <- list.files(
      path = "R2",
      pattern = "\\.R$", # \\ensure file extension. $ensures ends with. Avoids .Rmd
      full.names = TRUE,
      recursive = TRUE )
    f2 <- f1[grepl(pattern = "/furrr/", x = f1)]
    for(i in f2){
      cat(i)
      source(i)
    }
  }else{
    f1 <- list.files(
      path = "R2",
      pattern = "\\.R$", # \\ensure file extension. $ensures ends with. Avoids .Rmd
      full.names = TRUE,
      recursive = TRUE )
    
    # drop the county files that are causing errors 
    f2 <- f1[!grepl(pattern = "/furrr/", x = f1)]
    f2 <- f2[!grepl(pattern =  "R2/summarize/generateCountyMapSummaryTable.R", f2)]
    f2 <- f2[!grepl(pattern =  "R2/summarize/renderCountySummaries.R", f2)]
    
    for(f123 in 1:length(f2)){
      print(f123)
      
      source(f2[f123])
    }
  }
  if(gapAnalysisOnly == TRUE){
    f1 <- list.files(
      path = "R2/gapAnalysis",
      pattern = "\\.R$", # \\ensure file extension. $ensures ends with. Avoids .Rmd
      full.names = TRUE,
      recursive = TRUE )
    for(i in f2){
      print(i)
      source(i)
    }
  }

}


# find all successful runs 

