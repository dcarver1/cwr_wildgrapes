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

intersectTest <- function(site, p2, s1){
  # select area of interest
  area <- p2[p2$WDPAID == site, ]
  # intersect
  vals <- terra::crop(s1, area) 
  if(nrow(vals) > 0){
    vals <- terra::mask(vals, area)
  }
  return(data.frame(name = site, count = length(vals)))
}



areaPerSpecies <- function(iter, species, s1, pro1){
  ss1 <- s1[[iter]]
  if(!is.null(ss1)){
    # filter to species species 
    sp1 <- terra::vect(ss1)#[ss1$taxon == species, ]
    # crop pro area to species extent 
    p2 <- terra::crop(pro1, sp1)
    if(nrow(p2) > 0){
      sites <- unique(p2$WDPAID)
      intersect <- purrr::map(.x = sites,.f = intersectTest,  p2 = p2, s1 = sp1)
      i1 <- bind_rows(intersect) |>
        dplyr::filter(count > 0) |>
        dplyr::mutate(taxon = sp1$taxon[1])
      return(i1)
    }
  }
}


protectedAreaPoints <- function(species, runVersion, genus, wdpaVect){

  # contrust species path 
  # generate spatial data objects for each species 
  sp <- purrr::map(.x = species, .f = pathRead, runVersion = runVersion) |>
    bind_rows()
  pro1 <- wdpaVect
  # path1 
  path1 <-  paste0("data/",genus,"/",runVersion,"/proPoints")
  if(!dir.exists(path1)){dir.create(path1)}
  for(i in species){
    print(i)
    export <- paste0(path1,"/points_", i,".csv")
    if(!file.exists(export)){
      d1 <- pathRead(i, runVersion = runVersion) 
      if(is.null(d1)){
        next 
      }
      iso <- d1$iso3
      d1 <- terra::vect(d1)
      # filter wdpa to iso3 
      w1 <- wdpaVect[wdpaVect$ISO3 %in% iso, ]
      
      # select protected areas that intersect 
      w2 <- w1[d1, ]
      for(j in 1:nrow(w2)){
        pro <- w2[j, ]
        # crop pro area to species extent 
        p2 <- terra::crop(pro, d1)
        if(nrow(p2) > 0){
          site <- unique(p2$WDPAID)
          d2 <- d1 |>
            terra::crop(p2)|>
            terra::mask(p2)
          df <- data.frame(
            name = site,
            count = nrow(d2),
            taxon = d1$taxon[1]
          )
          if(j == 1){
            results <- df
          }else{
            results <- bind_rows(results,df)
          }
        }
      }
      if(nrow(results) > 0){
        write_csv(x = results, file = export)
      }
    }
    gc()
  }
}

# loop over all species and read in spatial points object 
pathRead <- function(species, runVersion){
  path <- paste0("data/Vitis/",species,"/",runVersion,"/occurances/spatialData.gpkg")
  if(file.exists(path)){
    s1 <- st_read(path)
    return(s1)
  }
}

wdpaVect <- function(species, runVersion){
  
  iso <-as.data.frame(sp)|>
    dplyr::select(iso3)|>
    dplyr::distinct() |>
    pull()
  rm(sp)
  # generate a single protected areas vector 
  # grab all the spatial files of interest
  files <- list.files(path = "data/geospatial_datasets/protectedLands/WDPA_Mar2023_Public_shp",
                      pattern = ".gpkg",
                      full.names = TRUE,
                      recursive = TRUE)
  # process individual shapefiles  into a single feature 
  for(i in seq_along(files)){
    print(i)
    # read in data and filter 
    s1 <- st_read(files[i])|>
      dplyr::filter(MARINE %in% c("0","1"))|>
      dplyr::filter(ISO3 %in% iso)|>
      terra::vect()
    if(i == 1){
      pro <- s1
    }else{
      pro <- c(pro,s1)
    }
  }
  pro1 <- vect(pro)[,c("WDPAID", "NAME","ORIG_NAME", "DESIG_ENG", "DESIG_TYPE","ISO3")]
  rm(s1, pro)
  gc()
  return(pro1)
}
