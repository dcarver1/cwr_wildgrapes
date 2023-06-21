

files <- list.files(path = "data/source_data/usda_plants",
                    pattern = ".csv",
                    full.names = TRUE)

plantsData <- files[10]


pacman::p_load("readr", "dplyr", "leaflet", "sf","tmap","stringr","htmltools")
tmap::tmap_mode("view")





generateMap  <- function(plantsData){
    d1 <- readr::read_csv(plantsData)
    # read in states data 
    states <- st_read("data\geospatial_datasets\counties\ne_10m_admin_2_counties.gpkg")%>%
      st_transform(crs = 4326)
    #read in county data
    counties <- st_read("E:/genericSpatialData/US/counties/tl_2017_us_county.shp")%>%
      st_transform(crs=4326)
}



usdaGeographies <- function(counties, states, usda){
  # grab unique geoids 
  c1 <- usda %>%
    mutate(geoid = stringr::str_remove(geoid,pattern = "US"))%>%
    dplyr::select(geoid)%>%
    pull()
  # select counties 
  c2 <- counties %>% 
    filter(GEOID %in% c1)
  # select states 
  s2 <- states %>%
    filter(NAME %in% unique(usda$state))
  return(list(
    selectC = c2,
    selectS = s2))
}


### render function 
geographies <- usdaGeographies(counties = counties, states = states,
                               usda = usda)
gbif2 <- gbifFilter(gbif = gbif,speciesName = "Vitis rupestris")
write_csv(gbif2, 
          file = paste0("outputs/vitisRupestris_gbif", Sys.Date(),".csv"))
View(gbif2)

gbif_sf <- st_as_sf(x = gbif2, coords = c("decimalLongitude", "decimalLatitude") )

# add color based on not Inaturalist 
gbif_sf <- gbif_sf %>%
  mutate(color = case_when(
    institutionCode == "iNaturalist" ~ "red",
    TRUE ~ "black"
  ))

mwh2 <- mwhFilter(mwh = mwh, speciesName = "Vitis rupestris")
write_csv(mwh2, 
          file = paste0("outputs/vitisRupestris_mwf", Sys.Date(),".csv"))
View(mwh2)

mwh_sf <- st_as_sf(x = mwh2, coords = c("decimalLongitude", "decimalLatitude") )
# add color based on not Inaturalist 
mwh_sf <- mwh_sf %>%
  mutate(color = case_when(
    institutionCode == "iNaturalist" ~ "red",
    TRUE ~ "blue"
  ))
## generate label 


### render map 


m1 <- leaflet()%>%
  addTiles()%>%
  addMapPane("states", zIndex = 410) %>%
  addMapPane("counties", zIndex = 415) %>%
  addMapPane("points", zIndex = 420) %>%
  addLayersControl(
    overlayGroups = c("States", "Counties","GBIF","Midwest Herberium"),
    options = layersControlOptions(collapsed = FALSE)
  )%>%
  addPolygons(data = geographies[[2]], 
              color= "grey", 
              opacity = 0.5,
              group = "States",
              options = pathOptions(pane = "states"))%>%
  addPolygons(data = geographies[[1]], 
              color = "green",
              group = "Counties",
              label = ~geographies[[1]]$NAME,
              options = pathOptions(pane = "counties"))%>%
  addCircleMarkers(data = gbif_sf,
                   color = ~color,
                   group = "GBIF",
                   radius = 2,
                   stroke = TRUE,
                   label = ~htmlEscape(
                     paste0('GBIF Code:', institutionCode)
                   ),
                   options = pathOptions(pane = "points"))%>%
  addMarkers(data = mwh_sf, 
             group = "Midwest Herberium",
             clusterOptions = markerClusterOptions(),
             label = ~htmlEscape(
               paste0('MWH Code:', institutionCode)
             ),
             options = pathOptions(pane = "points"))
m1

