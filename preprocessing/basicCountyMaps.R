###
# generate county maps for all species of interest 
# 20230628
# carverd@colostate.edu 
###


# libraries ---------------------------------------------------------------
pacman::p_load("readr", "dplyr", "leaflet", "sf","tmap","stringr")
tmap::tmap_mode("view")


# reading input datasets --------------------------------------------------
plantsData <- read_csv(file = "data/source_data/usda_plants/completeVitis.csv")
namedFeatures <- read_csv(file = "data/source_data/nameList.csv")
speciesNames <- read_csv(file = "data/source_data/taxonomy20230628.csv")
# we will want this normalized across all input sources... Just using gbif for now 
observationData <- read_csv("data/processed_occurance/tempOccurances_gbifOnly.csv")
#spatial data 
countySHP <- read_sf("data/geospatial_datasets/counties/ne_10m_admin_2_counties.gpkg")
stateSHP <- read_sf("data/geospatial_datasets/states/ne_10m_admin_1_states_provinces.gpkg")%>%
  dplyr::filter(adm0_a3 == "USA")


speciesList <- dplyr::left_join(x = plantsData, y = namedFeatures, by = c("plant_symbol" =  "Accepted Symbol"))%>%
  dplyr::select( "plant_symbol","Scientific Name")%>%
  distinct()


for(i in seq_along(speciesList$plant_symbol)){
  print(i)
  species <- speciesList$`Scientific Name`[i]
  #select input data per species 
  observationsFiltered <- observationData[observationData$taxon == species,]
  observationsFiltered <- observationsFiltered %>%
    dplyr::filter(iso3 == "USA",
                  !is.na(latitude),
                  !is.na(longitude))
  # generate points 
  points <- st_as_sf(x = observationsFiltered, 
                       coords = c("longitude", "latitude"),
                       remove = FALSE)%>%
    dplyr::mutate("fromINaturalist" = case_when(
      institutionCode == "iNaturalist" ~ "triangle",
      institutionCode != "iNaturalist" ~ "circle"
    ),
    popup =paste0( "<b>Data Source: </b>", databaseSource,
    "<br/> <b> Record ID: </b>", sourceUniqueID,
    "<br/> <b> Type: </b>", type,
    "<br/> <b> Year: </b>", yearRecorded,
    "<br/> <b> Location Description: </b>", localityInformation))
  # generate color palette for points 
  pointsPalette <- colorBin(palette = "YlGnBu", domain = points$yearRecorded,
                            bins = 5, reverse = TRUE,
                                na.color = "transparent")
  #icon attempt one -- pretty bad 
  # icons <- awesomeIcons(icon = points$fromINaturalist,
  #                       markerColor = "lightgray",
  #                       iconColor = pointsPalette(points$yearRecorded),
  #                       library = "glyphicon",
  #                       squareMarker = TRUE)
  
  # icon attempt two 
  my_icons2 <- iconList(
    circle = makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/black-circle-icon-23.png",
                       iconWidth = 12, iconHeight = 12),
    triangle = makeIcon(iconUrl = "https://www.freeiconspng.com/uploads/triangle-png-28.png",
                       iconWidth = 6, iconHeight = 6)
  )
  
  icon2 <- ifelse(test = points$fromINaturalist == "circle", 
         yes = my_icons2[1] ,
         no = my_icons2[2])
  
  
  # select plants county data per species 
  plantsFiltered <- plantsData[plantsData$plant_symbol == speciesList$plant_symbol[i], ]
  
  # filter layers based on plants dataset 
  countyFiltered <- countySHP[countySHP$FIPS %in% plantsFiltered$geoid, ]
  stateFiltered <- stateSHP[stateSHP$name %in% plantsFiltered$state, ]
  
  
  #html legend for Icons
  html_legend <- "<img src='https://www.freeiconspng.com/uploads/black-circle-icon-23.png'style='width:15px;height:15px;'>Collection<br/>
<img src='https://www.freeiconspng.com/uploads/triangle-png-28.png' style='width:15px;height:15px;''>Inaturalist"
  
  ### render map 
  m1 <- leaflet()%>%
    addTiles()%>%
    addMapPane("states", zIndex = 410) %>%
    addMapPane("counties", zIndex = 415) %>%
    addMapPane("points", zIndex = 420)%>%
    addMapPane("icons", zIndex = 425)%>%
    addLayersControl(
      overlayGroups = c("States", "Counties","Observations"),
      options = layersControlOptions(collapsed = FALSE)
    )
  
  if(nrow(stateFiltered)>0){
    m1 <- m1 %>%
      addPolygons(data = stateFiltered, 
                  color= "grey", 
                  opacity = 0.5,
                  group = "States",
                  options = pathOptions(pane = "states"))
  }
  if(nrow(countyFiltered)>0){
    m1 <- m1 %>%
      addPolygons(data = countyFiltered, 
                  color = "white",
                  group = "Counties",
                  label = ~countyFiltered$NAME,
                  options = pathOptions(pane = "counties"))
    }
  if(nrow(points)>0){
    m1 <- m1 %>%
      addCircleMarkers(data = points, 
                       color = ~pointsPalette(yearRecorded),
                       group = "Observations",
                       opacity = 0.8,
                       radius = 6,
                       fillOpacity = 0.8,
                       popup = ~popup,
                       options = pathOptions(pane = "points"))%>% 
      # specific icon shape according to the 'Group' column
      addMarkers(data = points, 
                 icon =  ~my_icons2[as.factor(fromINaturalist)],
                 group = "Observations",
                 popup = ~popup,
                 options = pathOptions(pane = "icons"))%>%
      addLegend(position = "topright",
                pal = pointsPalette,
                group = "Observations",
                values = points$yearRecorded,
                title = "Year of Observation")%>%
      addControl(position = "bottomleft",
                 html = points$taxon[1])%>%
      addControl(position = "bottomleft",
                 html = html_legend)
  }
  
                
  saveWidget(m1, 
             file=paste0("data/countyMaps/",species,".html"),
             selfcontained = TRUE)
}

