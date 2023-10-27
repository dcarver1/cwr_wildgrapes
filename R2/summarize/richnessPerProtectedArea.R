# speciesRichness <- "data/daucus/speciesrichness.tif"
# pathToProGPKG <- "data/geospatial_datasets/protectedLands/WDPA_Mar2023_Public_shp"
# countries <- "data/geospatial_datasets/countries/ne_10m_admin_0_countries.gpkg"
# exportPath <- "data/daucus"

protectedAreaRichness <- function(speciesRichness, pathToProGPKG, countries, exportPath){
  # read in richness layer
  r1 <- rast(speciesRichness)
  # read in countyies 
  c1 <- vect(countries)
  # extact values to countries and exclude any location where there are no species present
  ex1 <- terra::extract(r1, c1, mean,na.rm=TRUE) |> 
    filter(Threshold != 0)
  # use the id value to index out the counties of interest 
  id1 <- c1[ex1$ID, ] |> 
    dplyr::select(ISO_A3) |>
    pull() |>
    unique()
  #drop the -99 value 
  id1 <- id1[id1 != "-99"]
  
  # grab all the spatial files of interest
  files <- list.files(path = pathToProGPKG,
                      pattern = ".gpkg",
                      full.names = TRUE,
                      recursive = TRUE)
  
  for(i in seq_along(files)){
    # read in data and filter 
   s1 <- st_read(files[i]) |>
      dplyr::filter(MARINE %in% c("0","1"), 
                    ISO3 %in% id1) 
    # run the extraction 
    ex2 <- terra::extract(r1, s1, max,na.rm=TRUE)
    # select elements of interest 
    s2 <- s1 |>
      st_drop_geometry() |> 
      dplyr::select("WDPAID",  "NAME","ORIG_NAME", "DESIG_ENG","ISO3") |>
      dplyr::mutate(totalSpecies = ex2$Threshold)
    # bind into single object 
    if(i == 1){
      maxSpecies <- s2
    }else{
      maxSpecies <- bind_rows(maxSpecies, s2)
    }
  } 
  
  write_csv(x = maxSpecies, file = paste0(exportPath,"/protectedAreaSpeciesRichness.csv"))
  
  
  # # furrr implemetation 
  # not working :: Error in .External(list(name = "CppMethod__invoke_notvoid", address = <pointer: (nil)>,  : 
  # NULL value passed as symbol address
  # plan(strategy = "multisession", workers =3)
  # extractValues <- function(file, r1){
  #   # read in data and filter 
  #   s1 <- st_read(file) |>
  #     dplyr::filter(MARINE %in% c("0","1"), 
  #                   ISO3 %in% id1) 
  #   # run the extraction 
  #   ex2 <- terra::extract(r1, s1, max,na.rm=TRUE)
  #   # select elements of interest 
  #   s2 <- s1 |>
  #     st_drop_geometry() |> 
  #     dplyr::select("WDPAID",  "NAME","ORIG_NAME", "DESIG_ENG","ISO3") |>
  #     dplyr::mutate(totalSpecies = ex2$Threshold)
  #   return(s2 )
  # }
  # 
  # vals <- files |> furrr::future_map(.f = extractValues, r1 = r1) 
  
}
