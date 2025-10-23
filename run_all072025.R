###
# Developing a furrr implementation of the processing code
# carverd@colostate.edu
# 20231023
### 


# local testing 
pacman::p_load("dplyr", "sf","terra",  "purrr","randomForest","VSURF",
               "modelr","maxnet","pROC","DT", "readr", "vroom", "readr", "dismo",
               "leaflet", "tidyterra", "rmarkdown", "furrr", "stringr", "spThin",
               "tictoc","tigris", "tmap","ggplot2", "plotly",
               "factoextra", "tidyr","rnaturalearth")
# library( "googlesheets4") # not always needed and generates a 
tmap::tmap_mode("view")


# Download the states and provinces for North America
naStates <- ne_states(country =  c("mexico", "canada", "united states of america"), returnclass = "sf")|>
  dplyr::select(name,adm0_a3)


#source functions
source("R2/helperFunctions.R")
## using the helper function to help with edits. Save changes then run sourceFiles to console
sourceFiles(gapAnalysisOnly = FALSE)


# # source global objects 
# numPoint <- 2000
# bufferDist <- 0.45
set.seed(1234)
#
# # define species data 
# speciesData <- read_csv("data/processed_occurrence/draft_model_data.csv")
# species <- unique(speciesData$taxon)
# 

# funciton to show counts per species 
cPerSpec <- function(data){
  data |> 
    dplyr::group_by(taxon, type)|>
    count()%>%
    pivot_wider(
      names_from = type,   # The new column names will come from the 'type' column
      values_from = n,     # The values for those new columns will come from the 'n' column
      values_fill = 0      # If a species is missing a G or H, fill it with 0
    )
}

# Vitis
# # filtering the extra values coming from the data prep process
# speciesData <- read_csv("data/processed_occurrence/allEvaluated_data_removedDups_072025.csv")
# s1 <- cPerSpec(speciesData)
# names(s1) <- c("taxon", "g1","h1")
# |>
#   dplyr::select(-c("index", "validLat","validLon","validLatLon")) # "geometry",
# # using the data from the county maps for an reference run
# speciesData1 <- read_csv("data/processed_occurrence/DataForCountyMaps_20230320.csv")|>
#   dplyr::filter(!is.na(taxon),
#                 taxon %in% speciesData$taxon,
#                 genus == "Vitis")|>
#   dplyr::select(-c(geometry))
# speciesData <- speciesData1
# fnaData
fnaData <- read_csv("data/source_data/FNA_stateClassification.csv")
# read in the data from the spreadsheet 
# alteredData <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1_BfJawocOnA-1m9_gl5qZvufXHBCCOacMZX69cQz2LY/edit?gid=139317771#gid=139317771")
# a2 <- alteredData |>
#   dplyr::filter(nchar(alteredData$`Record ID for point`) > 2)|>
#   dplyr::filter(!is.na(Taxon)) 
# 
# # exclude from the input datasets 
# speciesData3 <- speciesData[!c(speciesData$recordID %in% a2$`Record ID for point`), ]
# speciesData <- speciesData3
# export for gap r testing 
# write.csv(speciesData, file = "temp/allVitisData082025.csv")
## doubled check and this data seems to have less duplication of G values 
speciesData <- read_csv("temp/allVitisData082025.csv")
# s2 <- cPerSpec(speciesData)


# adding back in Vitis tiliifolia records  --------------------------------
vt <- read_csv("data/processed_occurrence/allEvaluated_data_removedDups_072025.csv") |>
  dplyr::filter(taxon %in% c("Vitis tiliifolia", "Vitis popenoei"))

sd2 <- speciesData[!speciesData$taxon %in% c("Vitis tiliifolia", "Vitis popenoei"), ]
speciesData <- bind_rows(sd2, vt)
# one off removals based on summayr map reviews 
source("temp/clearNewErrors.R")
speciesData<- clearNewErrors(speciesData)


# join features
datasourceSummary <- databaseSourcesSummary(speciesData)

# order but n observations 
nOrder <- speciesData |>
  dplyr::group_by(taxon)|>
  dplyr::count() |>
  dplyr::arrange(n)



# read in bioclim layers  -------------------------------------------------
bioVars <- readRDS("data/geospatial_datasets/bioclim_layers/bioVar_1km.RDS")
templateRast <- bioVars[[1]]
## ecoregions
ecoregions <- sf::st_read("data/geospatial_datasets/ecoregions/tnc_terr_ecoregions.gpkg")

## protect lands 
protectedAreas <- terra::rast("data/geospatial_datasets/protectedLands/wdpa_1km_all_.tif")
## buffer distance 
bufferDist <- 50000
bioNames <- read_csv("data/geospatial_datasets/bioclim_layers/variableNames_072025.csv")


#vitis run 
runVersion <- "run08282025_1k"
# overwrite 
overwrite <- FALSE

# Vitis popenoei

# create folder structure 
#create folder
dir1 <- paste0("data/Vitis") 
if(!dir.exists(dir1)){dir.create(dir1)}

dir2 <- paste0(dir1, "/", runVersion)
if(!dir.exists(dir2)){dir.create(dir2)}

species <- sort(unique(speciesData$taxon))
# generate folder paths
erroredSpecies <- list(noLatLon = c(),
                       lessThenEight = c(),
                       noSDM = c(),
                       noHTML = c())

# species to test for FNA filter - maybe need to rerun if filter was not applied 
fnaSpecies <- fnaData$`Taxon Name`[fnaData$`States from FNA`!= "NA,"]

# 

# summary table of species 
s2 <- speciesData |>
  dplyr::group_by(taxon)|>
  dplyr::summarise(count = n())|>
  dplyr::arrange(count) |>
  dplyr::filter(taxon != "NA")


#testing 
j <- "Vitis cinerea"
# species to regenerate nat area and SRSex measures 
rerun<- c(
"Vitis martineziana"
,"Vitis rubriflora"
# ,"Vitis rufotomentosa"
,"Vitis cinerea var. tomentosa"
,"Vitis munsoniana"
,"Vitis jaegeriana"
,"Vitis biformis"
,"Vitis nesbittiana"
,"Vitis peninsularis"
,"Vitis lincecumii"
,"Vitis blancoi"
,"Vitis popenoei"
,"Vitis x novae-angliae"
,"Vitis baileyana"
,"Vitis cinerea var. cinerea"
,"Vitis bourgaeana"
,"Vitis simpsonii"
,"Vitis shuttleworthii"
,"Vitis palmata"
,"Vitis aestivalis var. bicolor"
,"Vitis aestivalis var. aestivalis"
,"Vitis tiliifolia")
# start of for loop -------------------------------------------------------
for(j in rerun[21]){ # species 
  # create unique path for summary HTML docs 
  p1 <- paste0("data/Vitis/speciesSummaryHTML/",runVersion)
  if(!dir.exists(p1)){
    dir.create(p1)
  }
  print(j)
  #generate paths for exporting data 
  allPaths <- definePaths(dir1 = dir1,
                          j = j,
                          runVersion = runVersion) 
  # create directories if needed 
  generateFolders(allPaths)
    
  # process data 
  ## species specific data
  sd1 <- speciesData |>
    dplyr::filter(taxon == j )
  # write_csv(sd1, "temp/doania.csv" )
  ## counts data
  c1 <- write_CSV(path = allPaths$countsPaths,
                  overwrite = TRUE,
                  function1 = generateCounts(speciesData = sd1))
  
  #srsex
  srsex <- write_CSV(path = allPaths$srsExPath,
                     overwrite = TRUE,
                     function1 = srs_exsitu(sp_counts = c1))
    
  
  
  # check for no lat lon data
  if(c1$totalUseful == 0){
    erroredSpecies$noLatLon <- c(erroredSpecies$noLatLon, j)
    next
    print("next")
  }


  ## create the inital spatial object
  sp1 <- write_GPKG(path = allPaths$spatialDataPath,
                    overwrite = TRUE,
                    function1 = createSF_Objects(speciesData = sd1) %>%
                      removeDuplicates()
  )


  # apply FNA filter if possible.
  sp1 <- write_GPKG(path = allPaths$spatialDataPath,
                    overwrite = TRUE, # this needs to stay true otherwise the call above will be used.
                    function1 = applyFNA(speciesPoints = sp1,
                                         fnaData = fnaData,
                                         states = naStates))


  ## define natural area based on ecoregions
  natArea <- write_GPKG(path = allPaths$natAreaPath,
                        overwrite = TRUE,
                        function1 = nat_area_shp(speciesPoints = sp1,
                                                 ecoregions = ecoregions))

  # condition for at least 8 observations
  ## attempt to model the data
  if(nrow(sp1) >=8){
    print("Modeling")
    ## define number of background points
    b_Number <- numberBackground(natArea = natArea)

    ## generate GA50 objects
    g_buffer <- write_Rast(path = allPaths$ga50Path,
                           overwrite = overwrite,
                           function1 = create_buffers(speciesPoints = sp1,
                                                      natArea = natArea,
                                                      bufferDist = bufferDist,
                                                      templateRast = templateRast))

    ## associate observations with bioclim data and spatial thin
    m_data1 <- write_CSV(path = allPaths$allDataPath,
                         overwrite = overwrite,
                         generateModelData(speciesPoints = sp1,
                                           natArea = natArea,
                                           bioVars = bioVars,
                                           b_Number = b_Number))
    # exporting with type column now removing for consistenty
    m_data <- m_data1 |>
      dplyr::select(-type)


    # remove duplicated background data
    presence <- m_data[m_data$presence == 1,]
    absence <- m_data[m_data$presence != 1,]
    dubs <- duplicated(absence[,2:27])
    absence <- absence[!dubs, ]
    m_data <- bind_rows(presence, absence)

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
      print("conservation metrics")
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
                         overwrite  = overwrite,
                         function1 = srs_insitu(occuranceData = sp1,
                                                thres = thres,
                                                protectedArea =protectedAreas ))
      ## ersin
      ersin <- write_CSV(path = allPaths$ersinPath,
                         overwrite  = overwrite,
                         function1 = ers_insitu(occuranceData = sp1,
                                                nativeArea = natArea,
                                                protectedArea = protectedAreas,
                                                thres = thres,
                                                rasterPath = allPaths$ersinRast))

      ## grsin
      grsin <-  write_CSV(path = allPaths$grsinPath,
                          overwrite  = overwrite ,
                          function1 = grs_insitu(occuranceData = sp1,
                                                 protectedArea = protectedAreas,
                                                 thres = thres))
      ## fcsin
      fcsin <- write_CSV(path = allPaths$fcsinPath,
                         overwrite  = overwrite ,
                         function1 = fcs_insitu(srsin = srsin,
                                                grsin = grsin,
                                                ersin = ersin,
                                                noModel = FALSE
                         ))


      #exsitu
      ##ersex
      ersex <- write_CSV(path = allPaths$ersexPath,
                         overwrite  = TRUE,
                         function1 = ers_exsitu(speciesData = sd1,
                                                thres = thres,
                                                natArea = natArea,
                                                ga50 = g_bufferCrop,
                                                rasterPath = allPaths$ersexRast))
      ##grsex
      grsex <- write_CSV(path = allPaths$grsexPath,
                         overwrite  = overwrite,
                         function1 = grs_exsitu(speciesData = sd1,
                                                ga50 = g_bufferCrop,
                                                thres = thres))
      ##fcsex
      fcsex <- write_CSV(path = allPaths$fcsexPath,
                         overwrite  = TRUE,
                         function1 = fcs_exsitu(srsex = srsex,
                                                grsex = grsex,
                                                ersex = ersex,
                                                noModel = FALSE))

      #combined measure
      fcsCombined <- write_CSV(path = allPaths$fcsCombinedPath,
                               overwrite  = TRUE,
                               function1 = fcs_combine(fcsin = fcsin,
                                                       fcsex = fcsex))

      # crop everything with the riparia to the native area for riparia 
      # na <- terra::vect(natArea)
      # g_bufferCrop <- g_bufferCrop |> terra::crop(na) |> terra::mask(na)
      # thres <-  thres |> terra::crop(na) |> terra::mask(na)
      # 
      # projectsResults$all <-  terra::rast(projectsResults$all) |> terra::crop(na) |> terra::mask(na) |> raster()
      # projectsResults$mean <-  terra::rast(projectsResults$mean) |> terra::crop(na) |> terra::mask(na)|> raster()
      # projectsResults$median <-  terra::rast(projectsResults$median) |> terra::crop(na) |> terra::mask(na)|> raster()
      # projectsResults$stdev <-  terra::rast(projectsResults$stdev) |> terra::crop(na) |> terra::mask(na)|> raster()
      # g_buffer <- g_buffer|> terra::crop(na) |> terra::mask(na)
      
      
      #gather features for RMD
      ## just a helper function to reduce the number of input for the RMD
      reportData <- write_RDS(path = allPaths$summaryDataPath,
                              overwrite = TRUE,
                              function1 = grabData(fscCombined = fcsCombined,
                                                   ersex = ersex,
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
                                                   countsData = c1,
                                                   variableImportance = allPaths$variablbeSelectPath,
                                                   NoModel = FALSE))
    }else{ # no sdm results
    #   erroredSpecies$noSDM <- c(erroredSpecies$noSDM, j)
    #   print("conservation Metrics")
    #   #Complete conservation assessments without models
    #   ## srsin
    #   srsin <- write_CSV(path = allPaths$srsinPath,
    #                      overwrite = overwrite,
    #                      function1 = srs_insitu(occuranceData = sp1,
    #                                             thres = NA,
    #                                             protectedArea =protectedAreas ))
    #   ersin <- write_CSV(path = allPaths$ersinPath,
    #                      overwrite = overwrite,
    #                      function1 = ers_insitu(occuranceData = sp1,
    #                                             nativeArea = natArea,
    #                                             protectedArea = protectedAreas,
    #                                             thres = thres,
    #                                             rasterPath = allPaths$ersinRast))
    #   
    #   ## grsin
    #   grsin <-  write_CSV(path = allPaths$grsinPath,
    #                       overwrite = overwrite ,
    #                       function1 = grs_insitu(occuranceData = sp1,
    #                                              protectedArea = protectedAreas,
    #                                              thres = thres))
    #   
    #   
    #   ## fcsin
    #   fcsin <- write_CSV(path = allPaths$fcsinPath,
    #                      overwrite = overwrite,
    #                      function1 = fcs_insitu(srsin = srsin,
    #                                             grsin = grsin,
    #                                             ersin = ersin,
    #                                             noModel = TRUE))
    # 
    # 
    #   ##fcsex
    #   fcsex <- write_CSV(path = allPaths$fcsexPath,
    #                      overwrite = overwrite,
    #                      function1 = fcs_exsitu(srsex = srsex,
    #                                             grsex = grsex,
    #                                             ersex = ersex,
    #                                             noModel = TRUE))
    # 
    #   #combined measure
    #   fcsCombined <- write_CSV(path = allPaths$fcsCombinedPath,
    #                            overwrite = overwrite,
    #                            function1 = fcs_combine(fcsin = fcsin,
    #                                                    fcsex = fcsex))
    #   # generate report data for species
    #   reportData <- write_RDS(path = allPaths$summaryDataPath,
    #                           overwrite = overwrite,
    #                           function1 = grabData(fscCombined = fcsCombined,
    #                                                ersex = NA,
    #                                                fcsex = fcsex,
    #                                                fcsin = fcsin,
    #                                                evalTable = NA,
    #                                                g_bufferCrop = NA,
    #                                                thres = NA,
    #                                                 = NA,
    #                                                occuranceData = sp1,
    #                                                v_data = NA,
    #                                                g_buffer = NA,
    #                                                natArea = natArea,
    #                                                protectedAreas = protectedAreas,
    #                                                countsData = c1,
    #                                                variableImportance = allPaths$variablbeSelectPath,
    #                                                NoModel = TRUE))
    # 
     }
    }else{ # end of attempt to model
      erroredSpecies$lessThenEight <- c(erroredSpecies$lessThenEight, j)
      ### need the FCS summary data for the full run summary
      ## srsin can be calculated for all species with at least one lat lon value
      ## if no model is present (GRSin and ERSin are NA)
      ## if no G points are present (srsex, grsex, and ersex are 0)
      ## if g Points present but no model (srsex is calculated, grsex, and ersex are NA )
      
      # generating a buffer object to represent the threshold model 
      natAreaV <- terra::vect(natArea)
      buffer <- sp1 |> 
        terra::vect()|>
        terra::buffer(width = bufferDist) |>
        terra::crop(natAreaV) |>
        terra::mask(natAreaV)
      # convert to a rast 
      rastBuff <- terra::crop(templateRast, buffer)
      buffer_rs <- terra::rasterize(buffer, rastBuff)
      names(buffer_rs) <- "Threshold"
      # try to generate g buffer
      g_buffer <- write_Rast(path = allPaths$ga50Path,
                             overwrite = TRUE,
                             function1 = create_buffers(speciesPoints = sp1,
                                                        natArea = natArea,
                                                        bufferDist = bufferDist,
                                                        templateRast = templateRast))
      if(class(g_buffer) == "character"){
        g_bufferCrop <- g_buffer
      }else{
        g_bufferCrop <- g_buffer |> terra::mask(natAreaV)
      }
        
      # pull n G points from
      gPoints <- c1$totalGUseful
      #
      srsin <- write_CSV(path = allPaths$srsinPath,
                         overwrite = TRUE,
                         function1 = srs_insitu(occuranceData = sp1,
                                                thres = buffer_rs,
                                                protectedArea =protectedAreas))
      ersin <- write_CSV(path = allPaths$ersinPath,
                         overwrite = TRUE,
                         function1 = ers_insitu(occuranceData = sp1,
                                                nativeArea = natArea,
                                                protectedArea = protectedAreas,
                                                thres = buffer_rs,
                                                rasterPath = allPaths$ersinRast))
      
      ## grsin
      grsin <-  write_CSV(path = allPaths$grsinPath,
                          overwrite = TRUE ,
                          function1 = grs_insitu(occuranceData = sp1,
                                                 protectedArea = protectedAreas,
                                                 thres = buffer_rs))

      fcsin <- write_CSV(path = allPaths$fcsinPath,
                         overwrite = TRUE,
                         function1 = fcs_insitu(srsin = srsin,
                                                grsin = grsin,
                                                ersin = ersin,
                                                noModel = FALSE))
      ##ersex
      ersex <- write_CSV(path = allPaths$ersexPath,
                         overwrite = TRUE,
                         function1 = ers_exsitu(speciesData = sd1,
                                                thres = buffer_rs,
                                                natArea = natArea,
                                                ga50 = g_bufferCrop,
                                                rasterPath = allPaths$ersexRast))
      ##grsex
      grsex <- write_CSV(path = allPaths$grsexPath,
                         overwrite = TRUE,
                         function1 = grs_exsitu(speciesData = sd1,
                                                ga50 = g_bufferCrop,
                                                thres = buffer_rs))
      
      

      ##fcsex
      fcsex <- write_CSV(path = allPaths$fcsexPath,
                         overwrite = TRUE,
                         function1 = fcs_exsitu(srsex = srsex,
                                                grsex = grsex,
                                                ersex = ersex,
                                                noModel = FALSE,
                                                gPoints = gPoints))

      #combined measure
      fcsCombined <- write_CSV(path = allPaths$fcsCombinedPath,
                               overwrite = TRUE,
                               function1 = fcs_combine(fcsin = fcsin,
                                                       fcsex = fcsex))


      
      reportData <- write_RDS(path = allPaths$summaryDataPath,
                              overwrite = TRUE,
                              function1 = grabData(fscCombined = fcsCombined,
                                                   ersex = ersex,
                                                   fcsex = fcsex,
                                                   fcsin = fcsin,
                                                   evalTable = NA,
                                                   g_bufferCrop = g_bufferCrop,
                                                   thres = buffer_rs,
                                                   projectsResults = NA,
                                                   occuranceData = sp1,
                                                   v_data = NA,
                                                   g_buffer = g_buffer,
                                                   natArea = natArea,
                                                   protectedAreas = protectedAreas,
                                                   countsData = c1,
                                                   variableImportance = NA,
                                                   NoModel = FALSE))
      
      

      # generate the report with 
      rmarkdown::render(input = "R2/summarize/singleSpeciesSummaryBuffer_1k.Rmd",
                        output_format = "html_document",
                        output_dir =  p1,  # file.path(allPaths$result),
                        output_file = paste0(j,"_Summary_fnaFilter"),
                        params = list(
                          reportData = reportData),
                        envir = new.env(parent = globalenv())
                        # clean = F,
                        # encoding = "utf-8"
      )

    }

    # generate summary html
    # this is not working with the 1k data do to size fo the rasters... need to reevaluate
  
  


    # rmd with Model ----------------------------------------------------------
    # if(!file.exists(p1)| isTRUE(overwrite)){
    try(
      # print(j),
      # if(!j %in% erroredSpecies$lessThenEight){
        rmarkdown::render(input = "R2/summarize/singleSpeciesSummary_1k.Rmd",
                          output_format = "html_document",
                          output_dir =  p1,  # file.path(allPaths$result),
                          output_file = paste0(j,"_Summary_fnaFilter"),
                          params = list(
                            reportData = reportData),
                          envir = new.env(parent = globalenv())
                          # clean = F,
                          # encoding = "utf-8"
        )
      # }
    )
    # }else{
    #   if(!file.exists(allPaths$summaryHTMLPath)){
    #     # erroredSpecies$noHTML <- c(erroredSpecies$noHTML, j)
    #   }
    # }
    # # block here for testing. I want variable in local environment and don't want them written out.
    # # stop()
    # 
    # # remove all reused variables ---------------------------------------------
    # rm(c1,sp1,srsex,natArea,g_buffer, ,evalTable,thres)
  }# end of species loop















# errorDF <- erroredSpecies |> 
#   bind_cols()
# 
# write_csv(x = errorDF,
#           file = paste0(dir1,"/","errorredSpecies_",runVersion,".csv"))
# 
# 
# # produce Run level Summaries ---------------------------------------------
# need to set overwrite to true to produce most of the layers
### big processing step...
## might need to revisit how these are being generated... 
generateRunSummaries(dir1 = dir1,
                     runVersion = runVersion,
                     species = s2$taxon, 
                     genus = "Vitis",
                     protectedAreas = protectedAreas,
                     overwrite = FALSE)

# # produce boxplot summaries -----------------------------------------------
renderBoxPlots  <- TRUE
if(renderBoxPlots == TRUE){
  # compile all modeling data
  amd <- list.files(dir1, pattern = "allmodelData.csv", full.names = TRUE, recursive = TRUE)
  amd2 <- amd[grepl(pattern = runVersion, x = amd)]
  #empty df for storing data from the loop
  df4 <- data.frame()
  # loop over species
  for(p in seq_along(species)){
    p1 <- amd2[grepl(pattern = species[p],x = amd2)]
    if(length(p1)==1){
      p2 <- p1 |>
        read.csv() |>
        dplyr::filter(presence == 1)|>
        dplyr::mutate(taxon = species[p])
      df4 <- bind_rows(p2,df4)
    }
  }
  # generate input data set
  inputData <- list(
    data = df4,
    species = species,
    names = bioNames
  )
  # produce the document
  rmarkdown::render(input = "R2/summarize/boxplotSummaries.Rmd",
                    output_format = "html_document",
                    output_dir = file.path(dir1),
                    output_file = paste0(runVersion,"_boxPlotSummary.html"),
                    params = list(
                      inputData = inputData),
                    envir = new.env(parent = globalenv())
                    # clean = F,
                    # encoding = "utf-8"
  )
}
# 
# # 
# # # generate a summary CSV for vitis 
source("R2/summarize/summaryTable.R")
summaryCSV <- summaryTable(species = species, runVersion = runVersion)
write_csv(x = summaryCSV, file = paste0("data/Vitis/summaryTable_",runVersion,".csv"))
