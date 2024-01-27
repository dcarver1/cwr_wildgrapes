# dir1 = dir1
# runVersion = runVersion
# genus = i


generateRunSummaries <- function(dir1,runVersion, genus, protectedAreas, overwrite){
  
  # generate summary of all the models --------------------------------------
  path1 <- paste0(dir1,"/speciesrichness.tif")
  path2 <- paste0(dir1,"/speciesUsed_speciesrichness.csv")
  path3 <- paste0(dir1,"/ga50_speciesrichness.tif")
  path4 <- paste0(dir1, "/ga50speciesUsed_speciesrichness.csv")
  path5 <- paste0(dir1,"/protectedAreaSpeciesRichness.csv")
  
  # this takes a while to run so be careful 
  ## generate the species richness file 
  if(!file.exists(path1) | isTRUE(overwrite)){
    # generate specific richness map 
    richness <- generateSpeciesRichnessMap(directory = dir1,
                                           runVersion = runVersion,
                                           rasterFileName = "prj_threshold.tif")
    terra::writeRaster(x = richness$richnessTif,
                       filename = path1,
                       overwrite  = TRUE)
    # need to convert to a df before writing
    df <- data.frame(speciesUsed = richness$speciesUsed)
    write_csv(x = df,
              file = path2)
  }
  # generate grsex richness map
  if(!file.exists(path3) | isTRUE(overwrite)){
    # generate specific richness map 
    ga50Richness <- generateSpeciesRichnessMap(directory = dir1,
                                           runVersion = runVersion,
                                           rasterFileName = "ga50_masked.tif")
    terra::writeRaster(x = ga50Richness$richnessTif,
                       filename = path3,
                       overwrite  = TRUE)
    # export the 
    df <- data.frame(speciesUsed = ga50Richness$speciesUsed)
    write_csv(x = df,
              file = path4)
  }
  
  # generate ersex richness map
  if(!file.exists(path3) | isTRUE(overwrite)){
    # generate specific richness map 
    ga50Richness <- generateSpeciesRichnessMap(directory = dir1,
                                               runVersion = runVersion,
                                               rasterFileName = "ga50_masked.tif")
    terra::writeRaster(x = ga50Richness$richnessTif,
                       filename = path3,
                       overwrite  = TRUE)
    # export the 
    df <- data.frame(speciesUsed = ga50Richness$speciesUsed)
    write_csv(x = df,
              file = path4)
  }
  
  
  
  # generate species richness within protected areas ------------------------
  if(overwrite == TRUE){
    protectedAreaRichness(speciesRichness = path1,
                          pathToProGPKG =  "data/geospatial_datasets/protectedLands/WDPA_Mar2023_Public_shp",
                          countries = "data/geospatial_datasets/countries/ne_10m_admin_0_countries.gpkg",
                          exportPath =dir1 )
  }

  


  # generate conservation summary figures 
  conservationSummary <- compileConservationData(directory = dir1, 
                                                 runVersion = runVersion,
                                                 genus = genus)
  # add additional object to the list to be passed to the rmd 
  conservationSummary$map <- raster(path1)
  conservationSummary$proAreas <- protectedAreas
  conservationSummary$ga50Map <- rast(path3)
  conservationSummary$protectAreasRichness <- read_csv(path5)
  
  
  
  
  # run summary html 
  try(
    rmarkdown::render(input = "R2/summarize/runSummary.Rmd",
                      output_format = "html_document",
                      output_dir = paste0(dir1,"/"),
                      output_file = paste0(runVersion,"_Summary.html"),
                      params = list(
                        reportData = conservationSummary),
                      envir = new.env(parent = globalenv())
                      # clean = F,
                      # encoding = "utf-8"
    )
  )
  
}
