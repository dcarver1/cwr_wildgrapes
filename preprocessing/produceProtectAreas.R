###
# Convert the gpkp files to raster and generate a combined layer.
# carverd@colostate.edu
# 20230626 
###

# !note: the filter step might be developed as we drop about more features. I don't know the specific at this time though. 



pacman::p_load("terra", "tmap", "dplyr", "tictoc", "tidyterra","rmapshaper")

# bioVars <- readRDS("data/geospatial_datasets/bioclim_layers/bioclim_2.5arcsec_terra.RDS")%>%
#   unwrap()

# temp1 <- bioVars[[1]]
# 
# rm(bioVars)


f1 <- list.files(path = "data/geospatial_datasets/protectedLands/WDPA_Mar2023_Public_shp", pattern = ".gpkg",
           recursive = TRUE, full.names = TRUE)
for(file in seq_along(f1)){
  export <- paste0("data/geospatial_datasets/protectedLands/wdpa_",file,".gpkg")
  if(!file.exists(export)){
    p1 <- vect(f1[file])
    # remove all marine areas before dim(91055,30) -- after dim(89259    30)p1 <- p1[p1$MARINE %in% c("0","1"),] 
    p2 <- p1 %>% 
      tidyterra::filter(MARINE %in% c("0","1")) |>#0 = terrestrial, 1 = Coastal 
      terra::makeValid()
    # simplfy 
    p3 <- terra::simplifyGeom(p2, tolerance = 0.001, makeValid = TRUE)
    # export 
    terra::writeVector(x = p3, 
                       filename = export,
                       overwrite = TRUE)
    rm(p2)
    rm(p3)
    gc()
  }
}


for(file in seq_along(f1)){
  p1 <- vect(f1[file])
  # remove all marine areas before dim(91055,30) -- after dim(89259    30)p1 <- p1[p1$MARINE %in% c("0","1"),] 
  p2 <- p1 %>% 
    tidyterra::filter(MARINE %in% c("0","1")) #0 = terrestrial, 1 = Coastal 
  rm(p1)
  # rasterize the feature
  r1 <- rasterize(x = p2, y = temp1)
  # expect raster
  terra::writeRaster(x = r1, 
                     filename = paste0("data/geospatial_datasets/protectedLands/wdpa_rasterized_", file, ".tif"),
                     overwrite = TRUE)
 
  if(file == 1){
    r2 <- r1 
  }
  if(file == 2){
    r3 <- r1 
  }
  if(file == 3){
    s <- sprc(r2,r3,r1)
    m <- merge(s)
    terra::writeRaster(x = m, 
                       filename = "data/geospatial_datasets/protectedLands/wdpa_rasterized_all.tif",
                       overwrite = TRUE)
  }
}
