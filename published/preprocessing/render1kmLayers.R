

pacman::p_load(terra)

# all files 
files <- list.files(path = "data/geospatial_datasets/bioclim_layers/wc2.1_30s",
                    recursive = TRUE,
                    pattern = ".tif",
                    full.names = TRUE)


# srad
s1 <- files[grepl(pattern = "_srad", x = files)]
s2 <- rast(s1) |> 
  terra::app(fun = mean, cores = 12, na.rm = TRUE)
writeRaster(x = s2, filename = "data/geospatial_datasets/bioclim_layers/wc2.1_30s/meanSrad.tif")
try(rm(s2))

# wind
w1 <- files[grepl(pattern = "_wind", x = files)]
w2 <- rast(w1) |> 
  terra::app(fun = mean, cores = 12, na.rm = TRUE)
writeRaster(x = w2, filename = "data/geospatial_datasets/bioclim_layers/wc2.1_30s/meanWind.tif")
try(rm(w2))
# vapr 
v1 <- files[grepl(pattern = "_vapr", x = files)]
v2 <- rast(v1) |> 
  terra::app(fun = mean, cores = 12, na.rm = TRUE)
writeRaster(x = v2, filename = "data/geospatial_datasets/bioclim_layers/wc2.1_30s/meanVapr.tif")
try(rm(v2))
# elevation 
## slope and aspect 
t1 <- files[grepl(pattern = "elev", x = files)]
t2 <- rast(t1) |>
  terra::terrain(v = c("slope", "aspect"))
writeRaster(x = t2, filename = "data/geospatial_datasets/bioclim_layers/wc2.1_30s/slope_aspect.tif")
try(rm(t2))

### bind all the layers together 
files <- list.files(path = "data/geospatial_datasets/bioclim_layers/wc2.1_30s",
                    recursive = TRUE,
                    pattern = ".tif",
                    full.names = TRUE)

# bio layers 
bio <- files[grepl(pattern = "_bio", x = files)]
## manually order 
bioOrder <- bio[c(1,12:19,2:11)] |> rast()
# add content in order 
## srad, vapr, wind, elevation, aspect, slope 
srad <- terra::rast("data/geospatial_datasets/bioclim_layers/wc2.1_30s/meanSrad.tif" )
vapr <- terra::rast("data/geospatial_datasets/bioclim_layers/wc2.1_30s/meanVapr.tif")
wind <- terra::rast("data/geospatial_datasets/bioclim_layers/wc2.1_30s/meanWind.tif")
elevation <- terra::rast("data/geospatial_datasets/bioclim_layers/wc2.1_30s/wc2.1_30s_elev.tif")
e2 <- terra::rast("data/geospatial_datasets/bioclim_layers/wc2.1_30s/slope_aspect.tif")
aspect <- e2$aspect
slope <- e2$slope

bioVars <- c(bioOrder, srad, vapr, wind, elevation, aspect, slope)
# rename 
names(bioVars) <- c(paste0("bio_",seq(1,19)), "srad", "vapr", "wind", "elevation", "aspect","slope")
# export 
writeRaster(x = bioVars, filename = ("data/geospatial_datasets/bioclim_layers/bioVar_1km.tif"))
#rds file 
wrapped <- terra::wrap(bioVars)
saveRDS(object = wrapped, file = "data/geospatial_datasets/bioclim_layers/bioVar_1km.RDS")

d1 <- readRDS("data/geospatial_datasets/bioclim_layers/bioVar_1km.RDS")
b2 <- terra::unwrap(d1)
b2
