# dir1 = dir1
# runVersion = runVersion
# genus = i


generateRunSummaries <- function(dir1,runVersion, species,
                                 genus, protectedAreas, overwrite){
  # storing summaries data in run folders 
  dir2 <- paste0(dir1, "/", runVersion)
  # generate summary of all the models --------------------------------------
  path1 <- paste0(dir2,"/speciesrichness.tif")
  path2 <- paste0(dir2,"/speciesUsed_speciesrichness.csv")
  path3 <- paste0(dir2,"/ga50_speciesrichness.tif")
  path4 <- paste0(dir2, "/ga50speciesUsed_speciesrichness.csv")
  path5 <- paste0(dir2,"/protectedAreaSpeciesRichness.csv")
  path6 <- paste0(dir2,"/ersexRichness.tif")
  path7 <- paste0(dir2,"/ersex_speciesUsed_Richness.csv")
  path8 <- paste0(dir2,"/ersinRichness.tif")
  path9 <- paste0(dir2,"/ersin_speciesUsed_Richness.tif")
  path10 <- paste0(dir2,"/protectedAreaSpeciesPointRichness.csv")


  # Rescale all the imagery for the 1km runs  -------------------------------
  ##   "prj_threshold.tif", "ga50_masked.tif","ers_ex_gaps.tif","ers_in_gaps.tif"
  for(i in c("prj_threshold.tif", "ga50_masked.tif","ers_ex_gaps.tif","ers_in_gaps.tif")){
    # grap all files 
    allFiles <- list.files( path = dir1,
                            pattern =  i,
                            full.names = TRUE,
                            recursive = TRUE)
    # filter to current run version 
    files2 <- allFiles[grepl(pattern = runVersion, x = allFiles)]
    for(file in files2){
      # generate export name 
      export <-sub("\\.tif$", "_5.tif", file)
      # resample if needed 
      if(!file.exists(export)){
        print(file)
        r2 <- resampleRast(rast = terra::rast(file))
        terra::writeRaster(x = r2, filename = export)
      }
    }
  }
  
    
    
  # this takes a while to run so be careful 
  ## generate the species richness file 
  if(!file.exists(path1) | isTRUE(overwrite)){
    # generate specific richness map 
    richness <- generateSpeciesRichnessMap(directory = dir1,
                                           runVersion = runVersion,
                                           rasterFileName = "prj_threshold_5.tif")
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
                                           rasterFileName = "ga50_masked_5.tif")
    
    # need to extend this file to matcht he extent of the richness image 
    t1 <- terra::rast(path1)
    extended_raster <- extend(ga50Richness$richnessTif, t1, fill = 0) 
    
    terra::writeRaster(x = extended_raster,
                       filename = path3,
                       overwrite  = TRUE)
    # export the 
    df <- data.frame(speciesUsed = ga50Richness$speciesUsed)
    write_csv(x = df,
              file = path4)
  }
  
  ##
  
  
  # generate ersex richness map
  if(!file.exists(path6) | isTRUE(overwrite)){
    # # generate specific richness map 
    ersExRichness <- generateERSRichnessMap(directory = dir1,
                                               runVersion = runVersion,
                                               ersMap = "ers_ex_gaps_5.tif",
                                               species = species,
                                               thresholdMap = "prj_threshold_5.tif")
    terra::writeRaster(x = ersExRichness$richnessTif,
                       filename = path6,
                       overwrite  = TRUE)
    # # export the 
    df <- data.frame(speciesUsed = ersExRichness$speciesUsed)
    write_csv(x = df,
              file = path7)
  }
  
  # generate ersin richness map
  if(!file.exists(path8) | isTRUE(overwrite)){
    # # generate specific richness map 
    ersInRichness <- generateERSRichnessMap(directory = dir1,
                                            runVersion = runVersion,
                                            ersMap = "ers_in_gaps_5.tif",
                                            species = species,
                                            thresholdMap = "prj_threshold_5.tif")
    terra::writeRaster(x = ersInRichness$richnessTif,
                       filename = path8,
                       overwrite  = TRUE)
    # # export the 
    df <- data.frame(speciesUsed = ersInRichness$speciesUsed)
    write_csv(x = df,
              file = path9)
  }
  
  
  # generate species richness within protected areas ------------------------
  if(overwrite == TRUE){
    protectedAreaRichness(speciesRichness = path1,
                          pathToProGPKG =  "data/geospatial_datasets/protectedLands/WDPA_Mar2023_Public_shp",
                          countries = "data/geospatial_datasets/countries/ne_10m_admin_0_countries.gpkg",
                          exportPath =dir2 )
  }
  

  # points in protected areas Richness  -------------------------------------
  wdpaVectExport <-paste0("data/geospatial_datasets/protectedLands/vect_",genus,"_",runVersion,".gpkg")
  if(!file.exists(wdpaVectExport)){
    wdpaVect <- wdpaVect(species = species, runVersion = runVersion)
    terra::writeVector(wdpaVect, wdpaVectExport)
  }else{
    wdpaVect <- terra::vect(wdpaVectExport)
  }
  
  
  if(overwrite == TRUE){
    
    proPoints <- protectedAreaPoints(species = s2$taxon,
                                     runVersion = runVersion,
                                     genus = "Vitis",
                                     wdpaVect = wdpaVect
                                     )
    write_csv(proPoints, path10)
  }
  
  

  


  # generate conservation summary figures 
  conservationSummary <- compileConservationData(directory = dir1, 
                                                 runVersion = runVersion,
                                                 genus = genus)
  # add additional object to the list to be passed to the rmd 
  conservationSummary$map <- rast(path1)
  conservationSummary$proAreas <- protectedAreas
  conservationSummary$ga50Map <- rast(path3)
  conservationSummary$protectAreasRichness <- read_csv(path5)
  conservationSummary$ersExRichness <- terra::rast(path6)
  conservationSummary$ersInRichness <- terra::rast(path8)
  conservationSummary$genus <- genus
  
  
  
  
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
