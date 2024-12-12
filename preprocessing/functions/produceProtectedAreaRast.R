
# generic type function for the processing the the WDPA dataset to a specific raster resolution 
# takes file paths to objects 
f1 <- list.files("data/geospatial_datasets/protectedLands/WDPA_Mar2023_Public_shp",
                 pattern = ".gpkg",
                full.names = TRUE,
                recursive = TRUE)
r1 <- readRDS(file = "data/geospatial_datasets/bioclim_layers/bioVar_1km.RDS")                  
raster <- r1[[1]]
rm(r1)

wdpa <- f1[1]

library(terra)

genProArea <- function(wdpa, raster){
  p1 <- vect(wdpa)
  # remove all marine areas before dim(91055,30) -- after dim(89259    30)p1 <- p1[p1$MARINE %in% c("0","1"),] 
  p2 <- p1[p1$MARINE %in% c("0","1"),] #0 = terrestrial, 1 = Coastal 
  rm(p1)
  # rasterize the feature
  r1 <- rasterize(x = p2, y = raster)
  # export raster
  return(r1)
}

for(i in seq_along(f1)){
  print(i)
  r2 <- genProArea(wdpa = f1[i], raster = raster)
  writeRaster(x = r2, filename = paste0("data/geospatial_datasets/protectedLands/wdpa_1km_", i, "_.tif"))
}


rasts <- list.files(path = "data/geospatial_datasets/protectedLands",
                    pattern = "wdpa_1km_", 
                    full.names = TRUE)

wdpa <- rasts

# takes a list of rast objects and merges 
combineProAreas <- function(wdpa){
  # combine and merge 
  s <- terra::sprc(wdpa) |>
    terra::merge()
  return(s)
}

allPro <- combineProAreas(wdpa = rasts)
writeRaster(x = allPro, filename = paste0("data/geospatial_datasets/protectedLands/wdpa_1km_all_.tif"))
