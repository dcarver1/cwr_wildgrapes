

basicMap <- function(thres,occurances ){
  
  speciesName <- occurances$taxon[1]
  
  # Assign color to occurrence data 
  occurances <- occurances %>%
    mutate(color = case_when(
      type  == "H" ~ "#4ca258",
      type == "G" ~ "#54278f"
    ),
    popup = paste0(
      "<b>Data Source:</b>", databaseSource,
      "<br/> <b> Record ID:</b>", record_identifyer,
      "<br/> <b> Type:</b>", type,
      "<br/> <b> Location Description:</b>", locality
    ))
  # threshold map palette
  pal <- colorNumeric(palette = c('#ffffb2','#fecc5c','#fd8d3c','#f03b20','#bd0026'),
                      domain = values(thres),
                      na.color = "transparent")
  
  m1 <- leaflet()%>%
    addTiles()%>%
    addMapPane("dsitribution", zIndex = 410) %>%
    addMapPane("points", zIndex = 420) %>%
    addLayersControl(
      overlayGroups = c("Distribution", "Occurances"),
      options = layersControlOptions(collapsed = FALSE)
    )%>%
    addRasterImage(raster(thres),
                   colors = pal,
                   opacity = 0.8,
                   group = "Distribution") %>%
    leaflet::addCircles(data = occurances,
                     color = ~color,
                     group = "Occurances",
                     popup = ~popup,
                     opacity = 1
                     )%>%
    addLegend(pal = pal, values = values(thres),
              title = "Probability of suitable habitat")%>%
    addControl(speciesName, position = "bottomleft")
  
  saveWidget(m1, 
             file=paste0("data/basicMaps/",speciesName,"_",Sys.Date(),".html"),
             selfcontained = TRUE)
}
