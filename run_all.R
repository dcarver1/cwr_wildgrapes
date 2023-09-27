###
# hitting some odd errors with the targets workflow, going to start a none target
# implimentation to get everything running as expected and move from there. 
# carverd@colostate.edu
# 20230621
### 


# local testing 
pacman::p_load("terra", "dplyr", "sf", "purrr","randomForest","VSURF",
               "modelr","maxnet","pROC","DT", "readr", "vroom", "readr", "dismo",
               "leaflet", "tidyterra", "rmarkdown", "furrr")

#source functions
source("R2/helperFunctions.R")
## using the helper function to help with edits. Save changes then run sourceFiles to console
sourceFiles()

# source global objects 
## maximun number of points used in model (use in subSampleCountry.R)
numPoint <- 2000
## used to define buffer distance in g buffer process in degrees 
# 50k(1 degree/111km) = 0.45
bufferDist <- 0.45
# set random seed. Important for reproducability 
set.seed(1234)
# set run version 
runVersion <- "test1"

## overwrite Parameter 
### used to determine if you want to write over existing content. 
overwrite <- TRUE

# input datasets ----------------------------------------------------------
## species observations 
speciesData <- read_csv("data/raw_occurances/daucusData_BioClimatic_2.5arc_modified.csv")
## bioclim layers 
bioNames <- read_csv("data/geospatial_datasets/bioclim_layers/variableNames.csv")
bioVars <- readRDS("data/geospatial_datasets/bioclim_layers/bioclim_2.5arcsec_terra.RDS")
names(bioVars) <- bioNames$shortName
templateRast <- bioVars[[1]]
## countries
country <- sf::st_read("data/geospatial_datasets/counties/ne_10m_admin_2_counties.gpkg")
## counties
counties <- sf::st_read("data/geospatial_datasets/countries/ne_10m_admin_0_countries.gpkg")
## ecoregions
ecoregions <- sf::st_read("data/geospatial_datasets/ecoregions/tnc_terr_ecoregions.gpkg")
## north America
northAmerica <- sf::st_read("data/geospatial_datasets/northAmerica/northAmericaArea.gpkg")
## protect lands 
protectedAreas <- terra::rast("data/geospatial_datasets/protectedLands/wdpa_rasterized_all.tif")
## states 
states <- sf::st_read("data/geospatial_datasets/states/ne_10m_admin_1_states_provinces.gpkg")


# set up environment  -----------------------------------------------------



# primary loop ------------------------------------------------------------
genera <- unique(speciesData$genus)
species <- sort(unique(speciesData$taxon))
# species subset
species <- species[c(30:length(species))]
# test species
# D. syrticus,
# D. sahariensis,
# D. carota susbp. gummifer, 
# D. carota subsp. capillifolius 
# D. carota subsp. fontanesii




#issues 
## [1] "Daucus_carota_subsp._fontanesii" : `k` must be a single integer.Error in h(simpleError(msg, call)) : 
## [1] "Daucus_carota_subsp._rupestris" :  `k` must be a single integer.Error in h(simpleError(msg, call)) : 


#testing
# i <- genera[1]
# j <- species[1]

erroredSpecies <- list(lessThenFive = c(),
                       noSDM = c(),
                       noHTML = c())
# 
plan(strategy = "multisession", workers =4)


# Daucus_aureus is species[1] is a reasonable one for troubleshooting
for(i in genera){
  print(i)
  #create folder
  dir1 <- paste0("data/",genera) 
  if(!dir.exists(dir1)){dir.create(dir1)}
  
  
  

  # furrr itoration ---------------------------------------------------------
  
#  furrr::future_map(species, primaryWorkflow, dir1 = dir1)
  
  
  # loop over species  ------------------------------------------------------
  ### this is probably the placee for a Furrr map function. Just the species being altered
  ### need to think about how to structure the code based from this part to best organize the process.
  for(j in species){
    
  #generate paths for exporting data 
  allPaths <- definePaths(dir1 = dir1,
                          j = j,
                          runVersion = runVersion) 

  # process data 
  ## species specific data
  sd1 <- subsetSpecies(occuranceData =speciesData, species = j)
  
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
        erroredSpecies$noSDM <- c(erroredSpecies$noSDM, j)  
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
    erroredSpecies$lessThenFive <- c(erroredSpecies$lessThenFive, j)
    }
  }# end of species loop 

  # generate summary of all the models --------------------------------------
  richnessTif <- generateSpeciesRichnessMap(directory = dir1,
                                            runVersion = runVersion)
  terra::writeRaster(x = richnessTif$richnessTif,
                     filename =paste0("data/daucus/speciesrichness",Sys.Date(),".tif"))
  # need to convert to a df before writing
  # write_csv(x = richnessTif$speciesUsed, 
  #           file = paste0("data/daucus/speciesUsedInRichnessMap",Sys.Date(),".csv"))
  
  
  
}






