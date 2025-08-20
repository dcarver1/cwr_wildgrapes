pacman::p_load(dplyr,sf, tmap, readr)

#helpers 
sigfig <- function(vec, n=3){
  ### function to round values to N significant digits
  # input:   vec       vector of numeric
  #          n         integer is the required sigfig
  # output:  outvec    vector of numeric rounded to N sigfig
  
  formatC(signif(vec,digits=n), digits=n,format="fg", flag="#")
  
}


# generate a table that shows per species
# the counts and percent area of herbrium, germplasma, inaturlist, bonap, and plants counties 

# generate a table that shows per species ;
# - % of counties lacking H
# - % of counties lacking G
# - % of counties where plants and bonap agree 
# - % of counties with plants no boanp 
# - % of counties with bonap no plants 
# - % of counties with Inat only 

# table showing the number of vitis species present in each county 


pacman::p_load(dplyr, sf, readr)

# For each species show a table break out counties in to species data sources
# total counties
## count and % counties per herbarium and germ plasm, usda plants, bonap, inat

# datasets
files <- list.files("data/Vitis/countyMapTables",
                    full.names = TRUE)

county <- st_read("data/geospatial_datasets/counties/ne_10m_admin_2_counties.gpkg") |>
  st_drop_geometry()|>
  dplyr::select(
    "County Name" = NAME,
    State = REGION,
    fips = CODE_LOCAL 
  )
# append the bonap data

bon <- read_csv("data/countyMaps/vitis_plants_bonap0730.csv")|>
  dplyr::mutate(
    fips2 = as.character(countyFIPS),
    fips = case_when(
      nchar(fips2) == 4 ~ paste0("0",fips2),
      nchar(fips2) == 5 ~ fips2
     )
  ) |> 
  dplyr::select("taxon","fips",
                bonap = "BONAP",
                usda = "USDA Plants")
# reformat 
bonap <- bon[,c("taxon","fips", "bonap")]
plants <- bon[,c("taxon","fips", "usda")]


for(i in 1:length(files)){
  # read in
  d1 <- read_csv(files[i])|>
    dplyr::mutate(
      fips = as.character(fips2)
    )|>
    dplyr::select(-geom)
  # select species
  taxa <- d1$taxon[1]
  
  b2 <- bonap |> 
    dplyr::filter(taxon == taxa)|>
    dplyr::select(fips,bonap) |>
    dplyr::filter(!is.na(bonap))
  
  p2 <- plants |>
    dplyr::filter(taxon == taxa)|>
    dplyr::select(fips,usda) |>
    dplyr::filter(!is.na(usda))
  
  # reconstruct dataframe 
  d2 <- d1 |>
    dplyr::select(
      taxon, NAME, fips,O,H,G
    ) |> 
    dplyr::left_join(
      y = b2, by = "fips"
    )|> 
    dplyr::left_join(
      y = p2, by = "fips"
    )|>
    dplyr::rowwise()|>
    dplyr::mutate(
      sum = sum( c(O, H, G, bonap, usda), na.rm = TRUE)
      )|> 
    dplyr::filter(sum > 0)
  # export this features 
  write_csv(x = d2, file = paste0("countyMapReporting/data/",taxa,"_countySummary.csv"))
  
  
  # lacking h and g 
  d3h <- d2 |> 
    dplyr::filter(
      is.na(H)
    )
  d3g <- d2 |> 
    dplyr::filter(
      is.na(G)
    )
  # plants/bonap agree 
  d4 <- d2 |> 
    dplyr::filter(
      !is.na(bonap) & !is.na(usda)
    )
  # usda no bonap 
  d5 <- d2 |> 
    dplyr::filter(
      is.na(bonap) & !is.na(usda)
    )
  # bonap no plants
  d6 <- d2 |> 
    dplyr::filter(
      !is.na(bonap) & is.na(usda)
    )
  # inat only 
  d7 <- d2 |>
    dplyr::filter(
      !is.na(O) & is.na(H) & is.na(G)& is.na(bonap)& is.na(usda)
    )
  # - % of counties lacking H
  # - % of counties lacking G
  # - % of counties where plants and bonap agree 
  # - % of counties with plants no boanp 
  # - % of counties with bonap no plants 
  # - % of counties with Inat only 
  # construct a data frame 
  df <- data.frame(
    taxon = taxa, 
    totalCounties = nrow(d2),
    herbarium = sum(!is.na(d2$H)),
    germplasm = sum(!is.na(d2$G)),
    inaturalist = sum(!is.na(d2$O)),
    bonap = sum(!is.na(d2$bonap)),
    plants = sum(!is.na(d2$usda)),
    no_H = nrow(d3h),
    no_G = nrow(d3g),
    bonap_plants_agree = nrow(d4),
    plants_noBonap = nrow(d5),
    bonap_noPlants = nrow(d6),
    inat_only = nrow(d7)
  )
  # construct a percentable table 
  cols_to_process <- c(
    "herbarium", "germplasm", "inaturalist", "bonap", "plants", 
    "no_H", "no_G", "bonap_plants_agree", "plants_noBonap"
  )
  
  # Use across() to apply the percentage calculation
  df_percent <- df %>%
    mutate(
      across(
        .cols = all_of(cols_to_process),
        .fns = ~ sigfig((.x / totalCounties) * 100),
        .names = "{.col}_percent"
      )
    )
 # export 
  if(i  == 1){
    output = df_percent 
  }else{
    output = bind_rows(output, df_percent)
  }
}

write_csv(x = output, file = "countyMapReporting/data/allSpeciesSummary.csv")




# count of specific species per county  -----------------------------------
f1 <- list.files(
  path = "countyMapReporting/data",
  pattern = "_countySummary.csv",
  full.names = TRUE)|>
  read_csv()

# summary data 
f2 <- f1 |>
  dplyr::group_by(fips)|>
  dplyr::summarise(
    total_taxa = length(unique(taxon)),
    unique_taxa_list = paste(unique(taxon), collapse = ", "),
    totalObservations = sum(sum, na.rm = TRUE)
  ) |> dplyr::left_join(
    y = county,
    by = "fips"
  )
# export 
write_csv(x = f2, file = "countyMapReporting/data/countsPerCounty.csv")









