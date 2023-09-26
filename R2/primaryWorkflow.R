
primaryWorkflow <- function(species, dir1){
  #generate paths for exporting data 
  allPaths <- definePaths(dir1 = dir1,
                          j = species,
                          runVersion = runVersion) 
  
  # process data 
  ## species specific data
  sd1 <- subsetSpecies(occuranceData =speciesData, species = species)
  
  ## counts data
  c1 <- write_CSV(path = allPaths$countsPaths,
                  overwrite = overwrite,
                  function1 = generateCounts(speciesData = sd1))
  
  ## spatial object
  sp1 <- write_GPKG (path = allPaths$spatialDataPath,
                     overwrite = overwrite, 
                     function1 = createSF_Objects(speciesData = sd1) %>%
                       removeDuplicates()
  )
  
  
  #srsex
  srsex <- write_CSV(path = allPaths$srsExPath,
                     overwrite = overwrite,
                     function1 = srs_exsitu(sp_counts = c1))
  
  ## define natural area based on ecoregions
  natArea <- write_GPKG(path = allPaths$natAreaPath,
                        overwrite = overwrite,
                        function1 = nat_area_shp(speciesPoints = sp1,
                                                 ecoregions = ecoregions))
  
  
  ## at some point we're going to need to filter this out. I don't know if this
  ## is the right time or not. 
  
  if(nrow(sp1) >=5 ){
    ## define number of background points 
    b_Number <- numberBackground(natArea = natArea)
    
    ## generate GA50 objects
    
    g_buffer <- write_Rast(path = allPaths$ga50Path, 
                           overwrite = overwrite,
                           function1 = create_buffers(speciesPoints = sp1,
                                                      natArea = natArea,
                                                      bufferDist = bufferDist,
                                                      templateRast = templateRast))
    
    ## associate observations with bioclim data
    m_data <- write_CSV(path = allPaths$allDataPath, 
                        overwrite = overwrite,
                        generateModelData(speciesPoints = sp1,
                                          natArea = natArea,
                                          bioVars = bioVars,
                                          b_Number = b_Number))
    ## perform variable selection
    v_data <- write_RDS(path = allPaths$variablbeSelectPath, 
                        overwrite = overwrite,
                        function1 = varaibleSelection(modelData = m_data))
    
    ## prepare data for maxent model 
    rasterInputs <- write_Rast(path = allPaths$prepRasters,
                               overwrite = overwrite,
                               function1 = cropRasters(natArea = natArea,
                                                       bioVars = bioVars,
                                                       selectVars = v_data))
    
    ## perform maxent model 
    ### tabular data 
    sdm_results <- write_RDS(path = allPaths$sdmResults,
                             overwrite = overwrite, 
                             function1 = runMaxnet(selectVars = v_data,
                                                   rasterData = rasterInputs))
    
    ## condition to test if model was suscessfull produced.
    if(!is.null(sdm_results)){
      ## raster objects 
      projectsResults <- write_RDS(path = allPaths$modeledRasters,
                                   overwrite = overwrite,
                                   function1 = rasterResults(sdm_result))
      
      ## generate evaluationTable 
      evalTable <- write_CSV(path = allPaths$evalTablePath,
                             overwrite = overwrite, 
                             function1 = evaluateTable(sdm_result = sdm_results))
      
      ## generate threshold rasters 
      thres <- write_Rast(path =  allPaths$thresPath,
                          overwrite = overwrite,
                          function1 = generateThresholdModel(evalTable = evalTable,
                                                             rasterResults = projectsResults))
      
      ## generate a mess map 
      ## generate a kernal density map 
      
      ## crop GA50 to threshold area 
      g_bufferCrop <- write_Rast(path = allPaths$g50_bufferPath, 
                                 overwrite = overwrite,
                                 function1 = cropG_Buffer(ga50 = g_buffer,
                                                          thres = thres))
      
      # Gap Analysis Methods  ---------------------------------------------------
      # insitu 
      ## srsin
      srsin <- write_CSV(path = allPaths$srsinPath,
                         overwrite = overwrite,
                         function1 = srs_insitu(occuranceData = sp1, 
                                                thres = thres,
                                                protectedArea =protectedAreas ))
      ## ersin 
      ### very slow at the moment. Lots of check against individual points. 
      ### Maybe test ecoregions individually from nat area-- extract values from sdm. 
      ersin <- write_CSV(path = allPaths$ersinPath,
                         overwrite = overwrite,
                         function1 = ers_insitu(occuranceData = sp1,
                                                nativeArea = natArea,
                                                protectedArea = protectedAreas,
                                                thres = thres)) 
      ## grsin 
      grsin <-  write_CSV(path = allPaths$grsinPath,
                          overwrite = overwrite,
                          function1 = grs_insitu(occuranceData = sp1,
                                                 protectedArea = protectedAreas,
                                                 thres = thres))
      ## fcsin 
      fcsin <- write_CSV(path = allPaths$fcsinPath,
                         overwrite = overwrite,
                         function1 = fcs_insitu(srsin = srsin,
                                                grsin = grsin,
                                                ersin = ersin))
      
      
      #exsitu 
      ##ersex  
      ersex <- write_CSV(path = allPaths$ersexPath,
                         overwrite = overwrite,
                         function1 = ers_exsitu(speciesData = sd1,
                                                thres = thres,
                                                natArea = natArea,
                                                ga50 = g_bufferCrop))
      ##grsex 
      grsex <- write_CSV(path = allPaths$grsexPath,
                         overwrite = overwrite,
                         function1 = grs_exsitu(speciesData = sd1,
                                                ga50 = g_bufferCrop,
                                                thres = thres))
      ##fcsex
      fcsex <- write_CSV(path = allPaths$fcsexPath,
                         overwrite = overwrite,
                         function1 = fcs_exsitu(srsex = srsex,
                                                grsex = grsex,
                                                ersex = ersex))
      
      #combined measure 
      fcsCombined <- write_CSV(path = allPaths$fcsCombinedPath,
                               overwrite = overwrite,
                               function1 = fcs_combine(fcsin = fcsin,
                                                       fcsex = fcsex))
      
      #gather features for RMD 
      ## just a helper function to reduce the number of input for the RMD
      reportData <- write_RDS(path = allPaths$summaryDataPath,
                              overwrite = TRUE,
                              function1 = grabData(fscCombined = fcsCombined,
                                                   fcsex = fcsex,
                                                   fcsin = fcsin,
                                                   evalTable = evalTable,
                                                   g_bufferCrop = g_bufferCrop,
                                                   thres = thres,
                                                   projectsResults = projectsResults,
                                                   occuranceData = sp1,
                                                   v_data = v_data,
                                                   g_buffer = g_buffer,
                                                   natArea = natArea,
                                                   protectedAreas = protectedAreas,
                                                   countsData = c1))
    }else{ # no sdm results 
      if(!file.exists(allPaths$sdmResults)){
        # erroredSpecies$noSDM <- c(erroredSpecies$noSDM, j)  
      }
    }
    
    
    ## basic summary maps --- this is being phased out
    # basicMap(thres = thres, occurances = sp1)
    
    ## need some work on this 
    ### there is need for conditional statements to determine if specific values
    ### should be used or not. 
    # if(!file.exists(allPaths$summaryHTMLPath)| isTRUE(overwrite)){
    try(
      rmarkdown::render(input = "R2/summarize/singleSpeciesSummary.Rmd",
                        output_format = "html_document",
                        output_dir = file.path(allPaths$result),
                        output_file = paste0(j,"_Summary.html"),
                        params = list(
                          reportData = reportData),
                        envir = new.env(parent = globalenv())
                        # clean = F,
                        # encoding = "utf-8"
      )
    )
    # }else{
    #   if(!file.exists(allPaths$summaryHTMLPath)){
    #     erroredSpecies$noHTML <- c(erroredSpecies$noHTML, j)
    #     
    #   }
    # }
    # block here for testing. I want variable in local environment and don't want them written out. 
    # stop()
    
    # remove all reused variables ---------------------------------------------
    rm(c1,sp1,srsex,natArea,g_buffer, projectsResults,evalTable,thres)
    
  }else{# sp1 >= 5 condition 
    # erroredSpecies$lessThenFive <- c(erroredSpecies$lessThenFive, j)
  }
}
