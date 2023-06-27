pacman::p_load("readr", "dplyr", "leaflet", "sf","tmap","stringr","htmltools")
tmap::tmap_mode("view")
library(htmlwidgets)

countySHP <- read_sf("data/geospatial_datasets/counties/ne_10m_admin_2_counties.gpkg")
stateSHP <- read_sf("data/geospatial_datasets/states/ne_10m_admin_1_states_provinces.gpkg")
usdaPlants <- read_csv("data/source_data/usda_plants/vitis_vulpina.csv")
occurranceData <- vroom("data/source_data/gbif.csv")

speciesOnly <-c("aestivalis",
                "palmata",
                "riparia",
                "rotundifolia",
                "rupestris",
                "shuttleworthii",
                "tiliifolia",
                "vinifera", 
                "vulpina")
speciesGbif <- paste0("Vitis ", speciesOnly)

usdaPlants <- list.files("data/source_data/usda_plants",pattern = ".csv",
                         full.names = TRUE)
for(i in seq_along(speciesOnly)){
  plantsData <- usdaPlants[grepl(pattern = speciesOnly[i], x = usdaPlants)]%>%
    read_csv()
  try(getCountyMap(usdaPlants = plantsData,
               countySHP = countySHP,
               stateSHP = stateSHP,
               occurranceData = occurranceData,
               speciesName = speciesGbif[i]))
}


#' Gerneate county maps 
#'
#' @return exports a html document of the county level accession data. 
#' 
getCountyMap <- function(usdaPlants, countySHP, stateSHP, occurranceData, speciesName){
  # generate the full fips for counties 
  usdaPlants$fullFIPS <- paste0(usdaPlants$`State FIP`, usdaPlants$`County FIP`)
  # filter states based on usda plants data 
  states <- stateSHP %>% 
    filter(adm0_a3 == "USA") %>% 
    filter(name %in% unique(usdaPlants$State))
  # filter counties based on usda plants data 
  counties <- countySHP %>% 
    filter(CODE_LOCAL %in% usdaPlants$fullFIPS)
  
  occData <- occurranceData %>% 
    dplyr::filter(species == speciesName,
                  !is.na(decimalLatitude),
                  !is.na(decimalLongitude),
                  countryCode =="US")%>%
    dplyr::select("gbifID","species","scientificName",
                  "countryCode","locality","year",
                  "basisOfRecord","institutionCode",
                  "decimalLatitude","decimalLongitude")
  # generate spatial object from points 
  gbif_sf <- st_as_sf(x = occData, coords = c("decimalLongitude", "decimalLatitude") )
  # add color based on not Inaturalist 
  gbif_sf <- gbif_sf %>%
    mutate(color = case_when(
      institutionCode == "iNaturalist" ~ "red",
      TRUE ~ "black"
    ))%>%
    mutate(popup = paste0("<b>Source:</b> GBIF",
                          "<br/><b>Collector Code:</b> ", institutionCode,
                          "<br/><b>Year:</b> ", year,
                          "<br/><b>Scientific Name:</b> ", scientificName,
                          "<br/><b>Collection Type:</b> ", basisOfRecord))
  
  ### render map 
  m1 <- leaflet()%>%
    addTiles()%>%
    addMapPane("states", zIndex = 410) %>%
    addMapPane("counties", zIndex = 415) %>%
    addMapPane("points", zIndex = 420) %>%
    addLayersControl(
      overlayGroups = c("States", "Counties","Occurances"),
      options = layersControlOptions(collapsed = FALSE)
    )%>%
    addPolygons(data = states, 
                color= "grey", 
                opacity = 0.5,
                group = "States",
                options = pathOptions(pane = "states"))%>%
    addPolygons(data = counties, 
                color = "green",
                group = "Counties",
                label = ~counties$NAME,
                options = pathOptions(pane = "counties"))%>%
    addCircleMarkers(data = gbif_sf,
                     color = ~color,
                     group = "GBIF",
                     radius = 2,
                     stroke = TRUE,
                     popup = ~popup,
                     # label = 
                     options = pathOptions(pane = "points"))%>%
    addControl(speciesName, position = "bottomleft")
  
  saveWidget(m1, 
             file=paste0("data/countyMaps/",speciesName,"_",Sys.Date(),".html"),
             selfcontained = TRUE)
  
}



