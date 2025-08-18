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

# append the bonap data 
bon <- read_csv("data/countyMaps/vitis_plants_bonap0730.csv") |>
  dplyr::select(
    "taxon","countyFIPS",
    Bonap2 = "BONAP",           
    USDA = "USDA Plants"
  )|>
  dplyr::mutate(
    countyFIPS = as.character(countyFIPS)
  )


for(i in 1:length(files)){
  # read in 
  d1 <- read_csv(files[i])|>
    dplyr::mutate(
      fips2 = as.character(fips2)
    )
  # select species 
  taxa <- d1$taxon[1]
  # filter bonnap/plants 
  bon2 <- bon |>
    dplyr::filter(taxon == taxa)
  
  # total Counties 
  if(nrow(bon2) > 0){
    d2 <- d1 |> 
      dplyr::left_join(y = bon2, by = c("fips2" = "countyFIPS"))|>
      dplyr::rowwise()|>
      dplyr::mutate(
        any = sum(c(anyRecord, USDA, Bonap2),na.rm = TRUE)
      ) |>
    dplyr::filter(any > 0)
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
      dplyr::filter(!is.na(Bonap2 )) |>
      nrow()
    # total plants 
    p1 <- d2 |>    
      dplyr::filter(!is.na(USDA)) |>
      nrow()
    # construct df 
    df <- tibble(
      taxon = taxa, 
      totalCounties = c1,
      `Counties with H or G` = hg,
      `Percent Coverage H or G` = (hg/c1)*100,
      `Counties with BONAP` = b1,
      `Percent Coverage BONAP` = (b1/c1)*100,
      `Counties with USDA Plants` = p1,
      `Percent Coverage USDA Plants` = (p1/c1)*100,
      `Counties with INaturalist` = n1,
      `Percent Coverage INaturalist` = (n1/c1)*100
    ) %>%
      mutate(across(where(is.numeric), sigfig))
    
    
  }else{
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
      `Counties with BONAP` = b1,
      `Percent Coverage BONAP` = (b1/c1)*100,
      `Counties with USDA Plants` = p1,
      `Percent Coverage USDA Plants` = (p1/c1)*100,
      `Counties with INaturalist` = n1,
      `Percent Coverage INaturalist` = (n1/c1)*100
    ) %>%
      mutate(across(where(is.numeric), sigfig))
    
  }

 
  if(i ==1 ){
    output = df
  }else{
    output = bind_rows(output, df)
  }
}
write_csv(output, file = "data/countyMaps/countyCountsSummaryTable.csv")



View(output)


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

pacman::p_load(leaflet, RColorBrewer, tidyr, tigris)

fips <- tigris::fips_codes |> 
  dplyr::mutate(
    county = gsub(" County", x = county, replacement = ""),
    countyFIPS = paste0(state_code, county_code)
  )


# read in eddMAPS data  ---------------------------------------------------

s1 <- read_csv("data/countyMaps/mappings.csv")|>
  # drop most the stuff 
  dplyr::select(objectid, Location)|>
  # pull out punctiation 
  dplyr::mutate(Location = gsub('"', "", Location)) |>
  # split out location in county state country 
  tidyr::separate(
    Location,
    into = c("county","state", "country"),
    sep = ","
  )|>
  dplyr::mutate(
    state = stringr::str_trim(state , side = "both"),
    county = stringr::str_trim(county , side = "both")
  )|>
  # drop some stuff
  dplyr::select(-country)
# join data
s2 <- dplyr::left_join(x = s1, y = fips, by = c("state" = "state_name", "county" = "county")) |>
  group_by(countyFIPS)|>
  count()

# select unique fips 
slf <- unique(s2$countyFIPS)
# index county layers 
slfCounties <- countySHP |>
  dplyr::left_join(s2, by = c("fips2"= "countyFIPS"))|>
  dplyr::filter(!is.na(n))

# palette for the species counts 
palette_colors <- colorNumeric(
  palette = brewer.pal(n = 9 , name = "Oranges"), # Add more colors if you have more categories
  domain = c2$count, # The column containing the categories
  na.color = "#14A1D920" # A common gray for NA values, or "transparent" if you want them invisible
)

# update the popup 
library(stringr)
c2$taxa2 <- stringr::str_remove_all(string = c2$taxa, pattern = "c\\")
c2$taxa2 <-  stringr::str_remove_all(string = c2$taxa, "^c\\(|\\)$")
c2$taxa2 <-  stringr::str_remove_all(string = c2$taxa,  '"')

# The str_replace_all() function may leave a leading or trailing space
# on some strings. Use str_trim() to remove them.
c2$taxaShort <- stringr::str_replace_all(string = c2$taxa, pattern = "Vitis", replacement = "V.")


c2$popup <- NA
for(i in 1:nrow(c2)){
  text <- str_remove_all(c2$taxaShort[i], "[\\\\\"]")|>
    stringr::str_remove_all( "^c\\(|\\)$")
  
  # convert to vector for lenght '
  vect <- stringr::str_split(string = text, pattern = ",") |>
     unlist()
  v2 <- 
  if(length(text)>1){
    text2 <- vect
  }else{
    text2 <- text
  }
  
  
  name <- c2$NAME_ALT[i]
  if(!is.na(text)){
    c2$popup[i] <- paste0(
      "<b>", name, "</b> <br>",
      paste("Total Vitis Species:", length(vect)),
      "<br>" ,
      text
      )
  }
}

m <- leaflet(c2) %>%
  addTiles() %>% # Add default OpenStreetMap tiles
  #state outline 
  addPolygons(
    data = stateSHP, 
    fillColor = "#FFFFFF", # Symbolize by 'category' using the defined palette
    color = "black", # Border color
    weight = 1, # Border weight
    opacity = 1,
    fillOpacity = 0.8
    ) |> 
  addPolygons(
    data = c2,
    fillColor = ~palette_colors(count), # Symbolize by 'category' using the defined palette
    color = "black", # Border color
    weight = 1, # Border weight
    opacity = 1,
    fillOpacity = 0.7,
    group = "Vitis",
    popup = ~popup, # Popup on click
    highlightOptions = highlightOptions(
      color = "white", weight = 2,
      bringToFront = TRUE
    )
  ) |>
  addPolygons(
    data = slfCounties,
    fillColor = "#18F5B4",       # Set the fill color to red
    fillOpacity = 0,       # Set fill opacity to 30% (transparent red)
    color = "#18F5B4",           # Set the outline color to solid red
    weight = 3,              # Set the outline thickness (e.g., 3 pixels)
    opacity = 1,   
    group = "SLF",
    popup = ~paste(
      "<b>",NAME , "</b> <br>",
      "<b>EDDMaps Count :</b> ", n
    ), 
  # Optional: add a label on hover
    highlightOptions = highlightOptions(
      weight = 5,
      color = "darkred",
      fillOpacity = 0.5,
      bringToFront = TRUE
    )
  )|> 
  addLegend(
    pal = palette_colors,
    values = ~count,
    title = "Vitis Species Present",
    position = "bottomright",
    group = "Vitis"
  ) |>
  addLegend(
    colors =  "#18F5B4",
    labels = "Spotted Lanter Fly",
    # opacity = my_polygon_fill_opacity, # Use the fill opacity for the legend swatch
    title = "Legend", # Optional legend title
    position = "bottomleft",
    group = "SLF"
  )|>
  addLayersControl(
    overlayGroups = c("SLF", "Vitis"), # These can be toggled on/off (checkboxes)
    position = "topleft", # Position of the control box
    options = layersControlOptions(collapsed = FALSE) # TRUE collapses it to an icon, FALSE keeps it open
  )
m



# Summary stats for slf ---------------------------------------------------
library(tidyr)
sum1 <- slfCounties |>
  mutate(
    stateFIPS = str_sub(FIPS, 1, 4)
  )

statesSel <- stateSHP[stateSHP$code_local %in% sum1$stateFIPS, ]
uniqueStates <- unique(statesSel$name)
# counties with SLF 
countySel <- unique(sum1$FIPS)
# counties with vitis 
vitisC <- c2 |>
  mutate(
    stateFIPS = str_sub(FIPS, 1, 4)
  )|>
  dplyr::filter(!is.na(count))
countyVit <- vitisC |> 
  dplyr::filter(FIPS %in% countySel)

dim(vitisC)

# join the SLF and vitis county records into single dataframe 
slf1 <- as.data.frame(slfCounties)|>
  dplyr::mutate(type = "SLF")|>
  dplyr::select(
    "FIPS",
    type,
    "NAME",
    "NAME_ALT", 
    "totalSLF_obs" = "n",
    -geom
  )
v1 <- vitisC |> 
  dplyr::mutate(type = "vitis") |>
  dplyr::select(
    FIPS,
    "totalVitis_obs" = "count",
    "taxa", 
    "stateFIPS",
    -geom
  )
slfV <- slf1 |> 
  dplyr::full_join(y = v1, by = "FIPS")|>
  dplyr::select(
    -geom
  )
# export full summary data 
write_csv(slfV, file = "temp/slf/slf_vitis_countyJoin.csv")


# test for presence of VITIS in states with SLF 
d1 <- slfV |> 
  dplyr::group_by(stateFIPS)|>
  dplyr::summarise(
    slfCount = sum(totalSLF_obs, na.rm = TRUE),
    vitisCount = sum(totalVitis_obs, na.rm = TRUE)
  ) |> 
  dplyr::filter(slfCount > 0)
# write_csv(x = d1 ,file = "temp/slf/slf_vitis_stateOverlap.csv")

# which species overlap the most with SLF 
d2 <- slfV |>
  dplyr::filter(totalSLF_obs  > 0)
# list of unique taxa 
taxa <- output$taxon
# storage dataframe 
out1 <- data.frame(taxon = taxa, slfCounties = NA, totalCounties = output$totalCounties)

for(i in 1:length(taxa)){
  # grepl select from join 
  v1 <- d2[grepl(pattern = taxa[i], x = d2$taxa), ]
  # assign value 
  out1$slfCounties[i] <- nrow(v1)
}
# export 
write_csv(x = out1, file = "temp/slf/slfCountiesPerTaxa.csv")
