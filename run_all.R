###
# hitting some odd errors with the targets workflow, going to start a none target
# implimentation to get everything running as expected and move from there. 
# carverd@colostate.edu
# 20230621
### 

# local testing 
pacman::p_load("terra", "dplyr", "sf", "purrr","randomForest","VSURF",
               "modelr","maxnet","pROC","DT", "readr", "vroom", "readr", "dismo",
               "leaflet", "tidyterra", "rmarkdown", "furrr","tmap", "stringr",
               "tictoc","tigris")
tmap_mode("view")

#source functions
source("R2/helperFunctions.R")
## using the helper function to help with edits. Save changes then run sourceFiles to console
sourceFiles(furrr = FALSE)


# input datasets ----------------------------------------------------------
## species observations 
### Daucus 
# speciesData <- read_csv("data/raw_occurances/daucusData_BioClimatic_2.5arc_modified_20240117.csv")
### sepecies with less then 8 D. biseriatus, D. carota subsp. annuus, D. carota subsp. fontanesii, D. carota subsp. parviflorus, D. carota subsp. rupestris , D. carota subsp. tenuissimus , D. della-cellae, D. edulis, D. gracilis, D. jordanicus, D. mauritii, D. microscias, D. mirabilis ,D. reboudii ,D. virgatus
### Vitis
## filtering the extra values coming from the data prep process 
# speciesData <- read_csv("data/processed_occurrence/draft_model_data.csv") |>
#   dplyr::select(-c("geometry","index", "validLat","validLon","validLatLon"))

### Quercus 
# speciesData <- read_csv("data/Quercus/QUAC_coord_ind.csv")
speciesData <- read_csv("data/Quercus/Quercus_lobata.csv")

### unique species data 
# speciesData <- read_csv("data/imlsGenCorSpeciesData.csv")



## bioclim layers 
## commiting out for summary runs 
bioNames <- read_csv("data/geospatial_datasets/bioclim_layers/variableNames.csv")
bioVars <- readRDS("data/geospatial_datasets/bioclim_layers/bioclim_2.5arcsec_terra.RDS")
names(bioVars) <- bioNames$shortName
templateRast <- bioVars[[1]]
## ecoregions
ecoregions <- sf::st_read("data/geospatial_datasets/ecoregions/tnc_terr_ecoregions.gpkg")
## protect lands 
protectedAreas <- terra::rast("data/geospatial_datasets/protectedLands/wdpa_rasterized_all.tif")
## buffer distance 
bufferDist <- 50000


# run version 
## daucus 
# runVersion <- "run20240104"
#vitis run 
# runVersion <- "run20231227"
# Quercus and other IMLS species 
runVersion <- "run1"

# overwrite 
overwrite <- FALSE

  # set up environment  -----------------------------------------------------

# primary loop ------------------------------------------------------------
genera <- unique(speciesData$genus)
species <- sort(unique(speciesData$taxon))
# species <- species[!grepl(pattern = "Daucus_glochidiatus", x = species)]
# species <- species[!grepl(pattern = "Daucus_carota_subsp._azoricus", x = species)] # points in ocean
# species <- species[!grepl(pattern = "Daucus_carota_subsp._fontanesii", x = species)] # no model
# species <- species[!grepl(pattern = "Daucus_carota_subsp._rupestris", x = species)] # no model 
# species <- species[!grepl(pattern = "Daucus_insularis", x = species)] # no model 
# 
# # ## subset species for testings 
# species <- species[30:length(species)]


# #testing
i <- genera[1]
j <- species[3]

erroredSpecies <- list(noLatLon = c(),
                       lessThenFive = c(),
                       noSDM = c(),
                       noHTML = c())

plan(strategy = "multisession", workers =8)

 

# testing 
# species <- species[27:length(species)] # error on Vitis rufotomentosa, Vitis x champinii, "Vitis x doaniana related to no coordinates
# # vitis subset 
# species <- c("Vitis arizonica",
#              "Vitis californica",
#              "Vitis rupestris",
#              "Vitis aestivalis",
#              "Vitis shuttleworthii",
#              "Vitis palmata",
#              "Vitis vulpina"                        )


# Daucus_aureus is species[1] is a reasonable one for troubleshooting
for(i in genera){
  print(i)
  #create folder
  dir1 <- paste0("data/",i) 
  if(!dir.exists(dir1)){dir.create(dir1)}
  
  
  # loop over species  ------------------------------------------------------
  ### this is probably the placee for a Furrr map function. Just the species being altered
  ### need to think about how to structure the code based from this part to best organize the process.
  for(j in species){
    print(j)
  #generate paths for exporting data 
  allPaths <- definePaths(dir1 = dir1,
                          j = j,
                          runVersion = runVersion) 
  # create directories if needed 
  generateFolders(allPaths)
  
  # process data 
  ## species specific data
  sd1 <- subsetSpecies(occuranceData =speciesData, species = j)
  
  ## counts data
  c1 <- write_CSV(path = allPaths$countsPaths,
                 overwrite = overwrite,
                 function1 = generateCounts(speciesData = sd1))
  # check for no lat lon data
  if(c1$totalUseful == 0){
    erroredSpecies$noLatLon <- c(erroredSpecies$noLatLon, j)
    next
    print("next")
  }
  
  ## spatial object
  sp1 <- write_GPKG(path = allPaths$spatialDataPath,
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
  # condition for at least 5 observations 
  if(nrow(sp1) >=8){
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
    m_data1 <- write_CSV(path = allPaths$allDataPath, 
                       overwrite = TRUE,
                       generateModelData(speciesPoints = sp1,
                                natArea = natArea,
                                bioVars = bioVars,
                                b_Number = b_Number))
    # exporting with type column now removing for consistenty 
    m_data <- m_data1 |>
      dplyr::select(-type)
    
    ## perform variable selection
    v_data <- write_RDS(path = allPaths$variablbeSelectPath, 
                       overwrite = overwrite,
                       function1 = varaibleSelection(modelData = m_data,
                                                     parallel = TRUE))
    
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
      # ## raster objects 
      # projectsResults <- write_RDS(path = allPaths$modeledRasters,
      #                             overwrite = overwrite,
      #                             function1 = rasterResults(sdm_result))
      # 
      # ## generate evaluationTable 
      # evalTable <- write_CSV(path = allPaths$evalTablePath,
      #                       overwrite = overwrite, 
      #                       function1 = evaluateTable(sdm_result = sdm_results))
      # 
      # ## generate threshold rasters 
      # thres <- write_Rast(path =  allPaths$thresPath,
      #                    overwrite = overwrite,
      #                    function1 = generateThresholdModel(evalTable = evalTable,
      #                                                       rasterResults = projectsResults))
      # 
      # ## generate a mess map 
      # ## generate a kernal density map 
      # 
      # ## crop GA50 to threshold area 
      # g_bufferCrop <- write_Rast(path = allPaths$g50_bufferPath, 
      #                           overwrite = overwrite,
      #                           function1 = cropG_Buffer(ga50 = g_buffer,
      #                                                    thres = thres))
      # 
      # # Gap Analysis Methods  ---------------------------------------------------
      # # insitu 
      # ## srsin
      # srsin <- write_CSV(path = allPaths$srsinPath,
      #                   overwrite = overwrite,
      #                   function1 = srs_insitu(occuranceData = sp1, 
      #                                          thres = thres,
      #                                          protectedArea =protectedAreas ))
      # ## ersin 
      # ersin <- write_CSV(path = allPaths$ersinPath,
      #                      overwrite = TRUE,
      #                      function1 = ers_insitu(occuranceData = sp1,
      #                                             nativeArea = natArea,
      #                                             protectedArea = protectedAreas,
      #                                             thres = thres,
      #                                             rasterPath = allPaths$ersinRast))
      # 
      # ## grsin 
      # grsin <-  write_CSV(path = allPaths$grsinPath,
      #                    overwrite = overwrite ,
      #                    function1 = grs_insitu(occuranceData = sp1,
      #                                           protectedArea = protectedAreas,
      #                                           thres = thres))
      # ## fcsin 
      # fcsin <- write_CSV(path = allPaths$fcsinPath,
      #                   overwrite = TRUE ,
      #                   function1 = fcs_insitu(srsin = srsin,
      #                                          grsin = grsin,
      #                                          ersin = ersin,
      #                                          noModel = FALSE
      #                                          ))
      # 
      # 
      # #exsitu 
      # ##ersex  
      # ersex <- write_CSV(path = allPaths$ersexPath,
      #                   overwrite = TRUE,
      #                   function1 = ers_exsitu(speciesData = sd1,
      #                                          thres = thres,
      #                                          natArea = natArea,
      #                                          ga50 = g_bufferCrop,
      #                                          rasterPath = allPaths$ersexRast))
      # ##grsex 
      # grsex <- write_CSV(path = allPaths$grsexPath,
      #                   overwrite = overwrite,
      #                   function1 = grs_exsitu(speciesData = sd1,
      #                                          ga50 = g_bufferCrop,
      #                                          thres = thres))
      # ##fcsex
      # fcsex <- write_CSV(path = allPaths$fcsexPath,
      #                   overwrite = TRUE,
      #                   function1 = fcs_exsitu(srsex = srsex,
      #                                          grsex = grsex,
      #                                          ersex = ersex,
      #                                          noModel = FALSE))
      # 
      # #combined measure 
      # fcsCombined <- write_CSV(path = allPaths$fcsCombinedPath,
      #                         overwrite = TRUE,
      #                         function1 = fcs_combine(fcsin = fcsin,
      #                                                 fcsex = fcsex))
      # 
      # #gather features for RMD 
      # ## just a helper function to reduce the number of input for the RMD
      # reportData <- write_RDS(path = allPaths$summaryDataPath,
      #                        overwrite = TRUE,
      #                        function1 = grabData(fscCombined = fcsCombined,
      #                                             ersex = ersex,
      #                                             fcsex = fcsex,
      #                                             fcsin = fcsin,
      #                                             evalTable = evalTable,
      #                                             g_bufferCrop = g_bufferCrop,
      #                                             thres = thres,
      #                                             projectsResults = projectsResults,
      #                                             occuranceData = sp1,
      #                                             v_data = v_data,
      #                                             g_buffer = g_buffer,
      #                                             natArea = natArea,
      #                                             protectedAreas = protectedAreas,
      #                                             countsData = c1,
      #                                             variableImportance = allPaths$variablbeSelectPath))
    }else{ # no sdm results 
      if(!file.exists(allPaths$sdmResults)){
        erroredSpecies$noSDM <- c(erroredSpecies$noSDM, j)
        
      #Complete conservation assessments without models 
        ## srsin
        srsin <- write_CSV(path = allPaths$srsinPath,
                           overwrite = overwrite,
                           function1 = srs_insitu(occuranceData = sp1, 
                                                  thres = NA,
                                                  protectedArea =protectedAreas ))
        ## fcsin 
        fcsin <- write_CSV(path = allPaths$fcsinPath,
                           overwrite = overwrite,
                           function1 = fcs_insitu(srsin = srsin,
                                                  grsin = grsin,
                                                  ersin = ersin,
                                                  noModel = TRUE))
        
        
        ##fcsex
        fcsex <- write_CSV(path = allPaths$fcsexPath,
                           overwrite = overwrite,
                           function1 = fcs_exsitu(srsex = srsex,
                                                  grsex = grsex,
                                                  ersex = ersex,
                                                  noModel = TRUE))
        
        #combined measure 
        fcsCombined <- write_CSV(path = allPaths$fcsCombinedPath,
                                 overwrite = overwrite,
                                 function1 = fcs_combine(fcsin = fcsin,
                                                         fcsex = fcsex))

      }
    }
   
    # # temp leafletmap for QUAC
    # thres2 <- thres |>
    #   project("epsg:3857",method="near")
    # 
    # map1 <- leaflet() |>
    #   addTiles() |>
    #   leaflet::addRasterImage(x = raster(thres2),
    #                           colors = c("#FFFFFF80", "#00FF00"))|>
    #   leaflet::addCircleMarkers(data = sp1,
    #                             color = "#7532a8",
    #                             opacity = 1,
    #                             radius = 1,
    #                             group = "Occurrences",
    #                             stroke = 1)

    
    # generate summary html  
    if(!file.exists(allPaths$summaryHTMLPath)| isTRUE(overwrite)){
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
    }else{
      if(!file.exists(allPaths$summaryHTMLPath)){
        # erroredSpecies$noHTML <- c(erroredSpecies$noHTML, j)
      }
    # }
    # block here for testing. I want variable in local environment and don't want them written out.
    # stop()
    
    # remove all reused variables ---------------------------------------------
    rm(c1,sp1,srsex,natArea,g_buffer, projectsResults,evalTable,thres)
    
  }else{
    erroredSpecies$lessThenFive <- c(erroredSpecies$lessThenFive, j)
    }
  }# end of species loop 
  errorDF <- erroredSpecies |> 
    bind_cols()
  
  write_csv(x = errorDF,
            file = paste0(dir1,"/","errorredSpecies_",runVersion,".csv"))

  
  # produce Run level Summaries ---------------------------------------------
  ### big processing step... 
  generateRunSummaries(dir1 = dir1,
                       runVersion = runVersion,
                       genus = i,
                       protectedAreas = protectedAreas,
                       overwrite = FALSE)


  # produce boxplot summaries -----------------------------------------------
  # renderBoxPlots  <- FALSE
  # if(renderBoxPlots == TRUE){
  #   # compile all modeling data
  #   amd <- list.files(dir1, pattern = "allmodelData.csv", full.names = TRUE, recursive = TRUE)
  #   amd2 <- amd[grepl(pattern = runVersion, x = amd)]
  #   #empty df for storing data from the loop
  #   df4 <- data.frame()
  #   # loop over species
  #   for(p in seq_along(species)){
  #     p1 <- amd2[grepl(pattern = species[p],x = amd2)]
  #     if(length(p1)==1){
  #       p2 <- p1 |>
  #         read.csv() |>
  #         dplyr::filter(presence == 1)|>
  #         dplyr::mutate(taxon = species[p])
  #       df4 <- bind_rows(p2,df4)
  #     }
  #   }
  #   # generate input data set
  #   inputData <- list(
  #     data = df4,
  #     species = species,
  #     names = bioNames
  #   )
  #   # produce the document
  #   rmarkdown::render(input = "R2/summarize/boxplotSummaries.Rmd",
  #                     output_format = "html_document",
  #                     output_dir = file.path(dir1),
  #                     output_file = paste0(runVersion,"_boxPlotSummary.html"),
  #                     params = list(
  #                       inputData = inputData),
  #                     envir = new.env(parent = globalenv())
  #                     # clean = F,
  #                     # encoding = "utf-8"
  #   )
  #
  }
  
}







