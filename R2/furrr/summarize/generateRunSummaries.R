# dir1 = dir1
# runVersion = runVersion
# genus = i


generateRunSummaries <- function(dir1,runVersion, genus, overwrite){
  
  # generate summary of all the models --------------------------------------
  path1 <- paste0("data/",genus,"/speciesrichness",Sys.Date(),".tif")
  path2 <- paste0("data/",genus,"/speciesUsed_speciesrichness",Sys.Date(),".csv")
  if(!file.exists(path1) | isTRUE(overwrite)){
    # generate specific richness map 
    richness <- generateSpeciesRichnessMap(directory = dir1,
                                           runVersion = runVersion)
    terra::writeRaster(x = richness$richnessTif,
                       filename = path1,
                       overwrite  = TRUE)
    # need to convert to a df before writing
    df <- data.frame(speciesUsed = richness$speciesUsed)
    write_csv(x = df,
              file = path2)
  }

    # generate conservation summary figures 
  conservationSummary <- compileConservationData(directory = dir1, 
                                                 runVersion = runVersion,
                                                 genus = genus, 
                                                 figure = TRUE)
  conservationSummary$map <- rast(path1)
  
  # run summary html 
  try(
    rmarkdown::render(input = "R2/summarize/runSummary.Rmd",
                      output_format = "html_document",
                      output_dir = paste0("data/",genus,"/"),
                      output_file = paste0(runVersion,"_Summary.html"),
                      params = list(
                        reportData = conservationSummary),
                      envir = new.env(parent = globalenv())
                      # clean = F,
                      # encoding = "utf-8"
    )
  )
  
}
