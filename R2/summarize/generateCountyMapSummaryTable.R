pacman::p_load(dplyr, sf, readr)

# For each species show a table break out counties in to species data sources 
# total counties 
## count and % counties per herbarium and germ plasm, usda plants, bonap, inat 

# datasets 
files <- list.files("data/Vitis/countyMapTables",
                    full.names = TRUE) 
sigfig <- function(vec, n=4){ 
  ### function to round values to N significant digits
  # input:   vec       vector of numeric
  #          n         integer is the required sigfig  
  # output:  outvec    vector of numeric rounded to N sigfig
  
  formatC(signif(vec,digits=n), digits=n,format="fg", flag="#") 
  
} 

for(i in 1:length(files)){
  # read in 
  d1 <- read_csv(files[i])
  # select species 
  taxa <- d1$taxon[1]
  # total Counties 
  d2 <- d1 |> 
    dplyr::filter(anyRecord > 0)
  # total Counties 
  c1 <- nrow(d2)
  # total H and G 
  hg <- d2 |> 
    dplyr::filter(!is.na(H) | !is.na(G)) |>
    nrow()
  # total i nat 
  n1 <- d2 |>    
    dplyr::filter(!is.na(O)) |>
    nrow()
  # total bonap
  b1 <- d2 |>    
    dplyr::filter(!is.na(BONAP )) |>
    nrow()
  # total plants 
  p1 <- d2 |>    
    dplyr::filter(!is.na(`USDA Plants`)) |>
    nrow()
  # construct df 
  df <- tibble(
    taxon = taxa, 
    totalCounties = c1,
    `Counties with H or G` = hg,
    `Percent Coverage H or G` = (hg/c1)*100,
    `Counties with BONAP` = hg,
    `Percent Coverage BONAP` = (hg/c1)*100,
    `Counties with USDA Plants` = p1,
    `Percent Coverage USDA Plants` = (p1/c1)*100,
    `Counties with INaturalist` = n1,
    `Percent Coverage INaturalist` = (n1/c1)*100
  ) %>%
    mutate(across(where(is.numeric), sigfig))
  if(i ==1 ){
    output = df
  }else{
    output = bind_rows(output, df)
  }
}
write_csv(output, file = "data/countyMaps/countyCountsSummaryTable.csv")




# Generate the species richies map at the county level --------------------
countySHP <- sf::st_read("data/geospatial_datasets/counties/ne_10m_admin_2_counties.gpkg") |> 
  dplyr::select(FIPS, NAME, NAME_ALT, 
                fips2 = CODE_LOCAL)
stateSHP <- read_sf("data/geospatial_datasets/states/ne_10m_admin_1_states_provinces.gpkg")|>
  dplyr::filter(adm0_a3 == "USA")

# group data 
data <- read_csv(file = files) |> 
  dplyr::filter(anyRecord != 0) |>
  dplyr::mutate(presence = 1)

# group by county geoid and get a count 
data2 <- data |>
  dplyr::group_by(fips2)|>
  dplyr::summarise(
    count = n(),
    taxa = list(unique(taxon))
  )

# join to county data 
c2 <- dplyr::left_join(countySHP, y = data2, by = "fips2" )

library(leaflet)
library(RColorBrewer)

palette_colors <- colorNumeric(
  palette = brewer.pal(n = 12 , name = "BuGn"), # Add more colors if you have more categories
  domain = c2$count # The column containing the categories
)

m <- leaflet(c2) %>%
  addTiles() %>% # Add default OpenStreetMap tiles
  addPolygons(
    fillColor = ~palette_colors(count), # Symbolize by 'category' using the defined palette
    color = "black", # Border color
    weight = 1, # Border weight
    opacity = 1,
    fillOpacity = 0.7,
    popup = ~paste(
      "<b>", NAME_ALT, "</b> <br>",
      "<b>Taxa:</b> ", taxa
    ), # Popup on click
    highlightOptions = highlightOptions(
      color = "white", weight = 2,
      bringToFront = TRUE
    )
  ) %>%
  addLegend(
    pal = palette_colors,
    values = ~count,
    title = "Polygon Categories",
    position = "bottomright"
  )
  