sourceFiles <- function(){
  for(i in list.files(
    path = "R2",
    pattern = "\\.R$", # \\ensure file extension. $ensures ends with. Avoids .Rmd
    full.names = TRUE,
    recursive = TRUE,
    
  )){
    print(i)
    source(i)
  }
}


# find all successful runs 

