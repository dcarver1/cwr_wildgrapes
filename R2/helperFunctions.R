sourceFiles <- function(furrr=FALSE){
  if(furrr == TRUE){
    f1 <- list.files(
      path = "R2",
      pattern = "\\.R$", # \\ensure file extension. $ensures ends with. Avoids .Rmd
      full.names = TRUE,
      recursive = TRUE )
    f2 <- f1[grepl(pattern = "/furrr/", x = f1)]
    for(i in f2){
      print(i)
      source(i)
    }
  }else{
    f1 <- list.files(
      path = "R2",
      pattern = "\\.R$", # \\ensure file extension. $ensures ends with. Avoids .Rmd
      full.names = TRUE,
      recursive = TRUE )
    f2 <- f1[!grepl(pattern = "/furrr/", x = f1)]
    for(i in f2){
      print(i)
      source(i)
    }
  }
  

}


# find all successful runs 

