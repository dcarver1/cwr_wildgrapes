###
# hitting some odd errors with the targets workflow, going to start a none target
# implimentation to get everything running as expected and move from there. 
# carverd@colostate.edu
# 20230621
### 


# local testing 
pacman::p_load("terra", "dplyr", "sf", "purrr","tmap","randomForest","VSURF",
               "modelr","maxnet","pROC","DT", "readr", "vroom", "readr", "dismo",
               "leaflet")

#source functions
for(i in list.files(
  path = "R2",
  pattern = ".R",
  full.names = TRUE,
  recursive = TRUE
)){
  print(i)
  source(i)
}


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
overwrite <- FALSE

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
# species <- species[c(14,8)]
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
i <- genera[1]
j <- species[1]
# Daucus_aureus is species[1] is a reasonable one for troubleshooting
for(i in genera){
  print(i)
  #create folder
  dir1 <- paste0("data/",genera) 
  if(!dir.exists(dir1)){dir.create(dir1)}
  
  # loop over species  ------------------------------------------------------
  for(j in species){
    print(j)
    dir2 <- paste0(dir1,"/",j)
    if(!dir.exists(dir2)){dir.create(dir2)}
    dirs <- paste0(
      dir2,"/", runVersion ,"/", c("gap_analysis", "occurances", "results")
    )
    for(k in dirs){
      if(!dir.exists(k))
        {dir.create(k,recursive = T)}
    }
  
  # process data 
  ## species specific data
  sd1 <- subsetSpecies(occuranceData =speciesData, species = j )
  
  ## counts data
  c1 <- generateCounts(speciesData = sd1)

  ## spatial object
  sp1 <- createSF_Objects(speciesData = sd1) %>%
    removeDuplicates()
  
  #srsex
  srsex <- srs_exsitu(sp_counts = c1)
  
  ## define natural area based on ecoregions
  natArea <-nat_area_shp(speciesPoints = sp1, ecoregions = ecoregions)
  
  
  ## at some point we're going to need to filter this out. I don't know if this
  ## is the right time or not. 
  
  if(nrow(sp1) >=5 ){
    ## define number of background points 
    b_Number <- numberBackground(natArea = natArea)
    
    ## generate GA50 objects
    g_buffer <- create_buffers(
      speciesPoints = sp1,
      natArea = natArea,
      bufferDist = bufferDist,
      templateRast = templateRast
    )
    
    ## associate observations with bioclim data
    m_data <- generateModelData(speciesPoints = sp1,
                                natArea = natArea,
                                bioVars = bioVars,
                                b_Number = b_Number)
    ## perform variable selection
    v_data <- varaibleSelection(modelData = m_data)
    
    ## prepare data for maxent model 
    rasterInputs <- cropRasters(
      natArea = natArea,
      bioVars = bioVars,
      selectVars = v_data
    )
    
    ## perform maxent model 
    sdm_results <- runMaxnet(selectVars = v_data,
                             rasterData = rasterInputs)
    
    ## generatre summary raster images  
    projectsResults <- rasterResults(sdm_result)
    
    ## generate evaluationTable 
    evalTable <- evaluateTable(sdm_result = sdm_results)
    
    ## generate threshold rasters 
    thres <- generateThresholdModel(evalTable = evalTable,
                                    rasterResults = projectsResults)
    ## generate a mess map 
    ## generate a kernal density map 
    
    ## crop GA50 to threshold area 
    g_bufferCrop <- cropG_Buffer(ga50 = g_buffer, thres = thres)
    
    # Gap Analysis Methods  ---------------------------------------------------
    # insitu 
    ## srsin
    srsin <- srs_insitu(occuranceData = sp1, 
                        thres = thres,
                        protectedArea =protectedAreas )
    ## ersin 
    ersin <- ers_insitu(occuranceData = sp1,
                        nativeArea = natArea,
                        protectedArea = protectedAreas,
                        thres = thres)
    ## grsin 
    grsin <- grs_insitu(occuranceData = sp1,
                        protectedArea = protectedAreas,
                        thres = thres)
    ## fcsin 
    fcsin <- fcs_insitu(srsin = srsin,
                        grsin = grsin,
                        ersin = ersin)
    
    
    #exsitu 
    ##ersex  
    ersex <- ers_exsitu(speciesData = sd1, thres = thres, natArea = natArea,
                        ga50 = g_bufferCrop)
    ##grsex 
    grsex <- grs_exsitu(speciesData = sd1, ga50 = g_bufferCrop, thres = thres)
    ##fcsex
    fcsex <- fcs_exsitu(srsex = srsex,grsex = grsex,ersex = ersex)
    
    #combined measure 
    fcsCombined <- fcs_combine(fcsin = fcsin,fcsex = fcsex)
    
    
    ## basic summary maps 
    basicMap(thres = thres, occurances = sp1)
    
    # block here for testing. I want variable in local enviroment and don't want them written out. 
    stop()
    
    # Export the data ---------------------------------------------------------
    ## add to the export 
    ## - grsin, ersin, srsin, fcsin, fcscombined 
    writeData(overwrite = overwrite,
              dirs = dirs,
              c1 = c1,
              sp1 = sp1,
              srsex = srsex,
              natArea = natArea,
              g_buffer = g_buffer,
              rasterResults = projectsResults,
              evalTable = evalTable,
              thres = thres,
              g_bufferMask = g_bufferCrop,
              grsex = grsex,
              ersex = ersex,
              fcsex = fcsex)
    
    # remove all reused variables ---------------------------------------------
    rm(c1,sp1,srsex,natArea,g_buffer, projectsResults,evalTable,thres)
    
  }
  
  
  
  }# end of species loop 
}


