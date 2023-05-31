###
# hitting some odd errors with the targets workflow, going to start a none target implimentation to get everything running as expected and move from there. 
# carverd@colostate.edu
# 
### 


# local testing 
pacman::p_load("terra", "dplyr", "sf", "purrr","tmap","randomForest","VSURF",
               "modelr","maxnet","pROC","DT", "readr", "vroom", "readr")

#source functions
lapply(
  # need to ensure functions are grabbed first
  list.files(
    path = "R2",
    pattern = ".R",
    full.names = TRUE,
    recursive = TRUE
  ),
  source
)

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
### come back to this one requires some more processing 
## states 
states <- sf::st_read("data/geospatial_datasets/states/ne_10m_admin_1_states_provinces.gpkg")


# set up environment  -----------------------------------------------------



# primary loop ------------------------------------------------------------
genera <- unique(speciesData$genus)
species <- unique(speciesData$taxon)

#testing
i <- genera[1]
j <- species[1]
for(i in genera){
  #create folder
  dir1 <- paste0("data/",genera) 
  if(!dir.exists(dir1)){dir.create(dir1)}
  
  # loop over species  ------------------------------------------------------
  for(j in species){
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
  sdm_results <- runMaxnet(selectVars = v_data, rasterData = rasterInputs)
  
  ## export raster images 
  writeProjections(sdm_result = sdm_results,
                   directory = dirs[3])
  
  ## evaluate outputs of the modeling process
  
  ## generate a mess map 
  ## generate a kernal density map 
  
  
  # Writing out data  -------------------------------------------------------
  ## rather then write out inside of functions I'll try to run everything at the end 
  ### raw species data 
  ### counts data 
  countsPath <- paste0(dirs[2], "/counts.csv")
  if(!file.exists(countsPath)){
    write_csv(x = c1, file = countsPath)
  }
  ### sp object 
  spPath <- paste0(dirs[2], "/spatialData.csv")
  if(!file.exists(spPath)){
    write_csv(x = sp1, file = spPath)
  }
  ### srsex 
  srsExPath <- paste0(dirs[1], "/srs_ex.csv")
  if(!file.exists(srsExPath)){
    write_csv(x = srsex, file = srsExPath)
  }
  ### natural area 
  natAreaPath <- paste0(dirs[3], "/naturalArea.gpkg")
  if(!file.exists(natAreaPath)){
    sf::write_sf(obj = natArea, dsn = natAreaPath)
  }
  
  ### natural area 
  ga50Path <- paste0(dirs[3], "/ga50.tif")
  if(!file.exists(ga50Path) & class(g_buffer) == "SpatRaster" ){
    terra::writeRaster(x = g_buffer, file = ga50Path)
  }
  
  

  }# end of species loop 
}


