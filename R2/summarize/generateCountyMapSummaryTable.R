pacman::p_load(dplyr, sf, readr, leaflet, RColorBrewer, tidyr, tigris, stringr)

#' Generate County Map Summary Table and Richness Maps
#'
#' @param bon BONAP data object
#' @param files List of county map table files
generate_county_map_summary_table <- function(bon, files) {

  sigfig <- function(vec, n=4){
    ### function to round values to N significant digits
    # input:   vec       vector of numeric
    #          n         integer is the required sigfig
    # output:  outvec    vector of numeric rounded to N sigfig
    formatC(signif(vec,digits=n), digits=n,format="fg", flag="#")
  }

  # append the bonap data
  bonPlant <- bon |>
    dplyr::mutate(
      fips = case_when(
        nchar(countyFIPS) ==  4 ~ paste0("0", countyFIPS),
        TRUE ~ countyFIPS
      ))
  # select the sources
  bon1 <- bonPlant |> 
    dplyr::filter(Bonap2 == 1) |>
    dplyr::select(fips, taxon, bonap = Bonap2)
  u1 <- bonPlant |> 
    dplyr::filter(USDA == 1) |>
    dplyr::select(fips, taxon, USDA)
  # join back 
  bonPlant <- dplyr::full_join(bon1, u1)

  output <- data.frame()
  
  # summarized by species  --------------------------------------------------
  for(i in 1:length(files)){
    # read in
    d1 <- read_csv(files[i])|>
      dplyr::mutate(
        fips2 = as.character(fips2)
      )
    # select species
    taxa <- d1$taxon[1]
    # filter bonnap/plants
    bon2 <- bonPlant |>
      dplyr::filter(taxon == taxa)

    # total Counties
    if(nrow(bon2) > 0){
      d2 <- d1 |>
        dplyr::select(fips = fips2, NAME, REGION, O,     H,     G, pre1970 )|>
        dplyr::left_join(y = bon2, by = "fips") |>
        dplyr::rowwise()|>
        dplyr::mutate(
          any = sum(c(O, H, G, bonap, USDA),na.rm = TRUE)
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
        dplyr::filter(!is.na(bonap )) |>
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
        dplyr::filter(!is.na(BONAP )) |> # Note: Case difference in original script
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
    
    if(i == 1){
      output <- df
    }else{
      output <- bind_rows(output, df)
    }
  }
  
  write_csv(output, file = "data/countyMaps/countyCountsSummaryTable.csv")

  # Summarized by county  ---------------------------------------------------
  ## prep all county data 
  counties <- sf::st_read("data/geospatial_datasets/counties/ne_10m_admin_2_counties.gpkg") |>
    dplyr::select(name = NAME_ALT,
                  state = REGION ,
                  fips = CODE_LOCAL)|>
    dplyr::mutate(fips = as.character(fips))
  
  ## prep the bonap/usda plants data (re-run logic for consistency)
  bonPlant <- bon |>
    dplyr::mutate(
      fips = case_when(
        nchar(countyFIPS) ==  4 ~ paste0("0", countyFIPS),
        TRUE ~ countyFIPS
      ))
  bon1 <- bonPlant |> 
    dplyr::filter(Bonap2 == 1) |>
    dplyr::select(fips, taxon, bonap = Bonap2)
  u1 <- bonPlant |> 
    dplyr::filter(USDA == 1) |>
    dplyr::select(fips, taxon, USDA)
  bonPlant <- dplyr::full_join(bon1, u1)

  results <- data.frame()
  for(i in 1:length(files)){
    # read in
    d1 <- read_csv(files[i])|>
      dplyr::mutate(
        fips2 = as.character(fips2)
      ) |> 
      dplyr::filter(
        anyRecord > 1
      ) |> 
      dplyr::select(taxon, fips = fips2,O,H,G)
    # select species
    taxa <- d1$taxon[1]
    # filter bonnap/plants
    bon2 <- bonPlant |>
      dplyr::filter(taxon == taxa) |>
      dplyr::select(-taxon)
    # join to d1 
    d2 <- dplyr::left_join(d1, bon2, by = "fips")
    if(i == 1){
      results <- d2
    }else{
      results <- bind_rows(results, d2)
    }
  }
  # aggregate values by species 
  r2 <- results |>
    group_by(fips) |>
    summarise(across(
      .cols = -taxon,  # Selects all columns except for 'taxon' (fixed original bug '.cols = !taxon')
      .fns = ~sum(., na.rm = TRUE) # Applies the sum function, ignoring NA values
    ))
  # join this to the county data 
  allCounts <- dplyr::left_join(counties, r2, by = "fips")  |>
    rowwise() |>
    mutate(total_sum = sum(c_across(O:USDA), na.rm = TRUE)) |>
    ungroup()

  # export 
  write_csv(allCounts, "data/countyMaps/vitisSummarizedByCounty.csv")

  # Generate the species richness map at the county level --------------------
  countySHP <- sf::st_read("data/geospatial_datasets/counties/ne_10m_admin_2_counties.gpkg") |>
    dplyr::select(FIPS, NAME, NAME_ALT,
                  fips2 = CODE_LOCAL)
  stateSHP <- read_sf("data/geospatial_datasets/states/ne_10m_admin_1_states_provinces.gpkg")|>
    dplyr::filter(adm0_a3 == "USA")

  # group data
  data_rich <- read_csv(file = files) |>
    dplyr::filter(anyRecord != 0) |>
    dplyr::mutate(presence = 1)

  # group by county geoid and get a count
  data2 <- data_rich |>
    dplyr::group_by(fips2)|>
    dplyr::summarise(
      count = n(),
      taxa = list(unique(taxon))
    )

  # join to county data
  c2 <- dplyr::left_join(countySHP, y = data2, by = "fips2" )

  fips_codes_data <- tigris::fips_codes |>
    dplyr::mutate(
      county_clean = gsub(" County", x = county, replacement = ""),
      countyFIPS = paste0(state_code, county_code)
    )
  
  # read in eddMAPS data (example path, might need adjustment)
  if(file.exists("data/countyMaps/mappings.csv")){
    s1 <- read_csv("data/countyMaps/mappings.csv")|>
      dplyr::select(objectid, Location)|>
      dplyr::mutate(Location = gsub('"', "", Location)) |>
      tidyr::separate(
        Location,
        into = c("county_nm","state_nm", "country_nm"),
        sep = ","
      )|>
      dplyr::mutate(
        state_nm = stringr::str_trim(state_nm , side = "both"),
        county_nm = stringr::str_trim(county_nm , side = "both")
      )|>
      dplyr::select(-country_nm)
    
    s2 <- dplyr::left_join(x = s1, y = fips_codes_data, by = c("state_nm" = "state_name", "county_nm" = "county_clean")) |>
      group_by(countyFIPS)|>
      count()

    slfCounties <- countySHP |>
      dplyr::left_join(s2, by = c("fips2"= "countyFIPS"))|>
      dplyr::filter(!is.na(n))
  } else {
    slfCounties <- NULL
  }

  # palette for the species counts
  palette_colors <- colorNumeric(
    palette = brewer.pal(n = 9 , name = "Oranges"),
    domain = c2$count,
    na.color = "#14A1D920"
  )

  # update the popup
  c2$taxaShort <- stringr::str_replace_all(string = as.character(c2$taxa), pattern = "Vitis", replacement = "V.")

  c2$popup <- NA
  for(i in 1:nrow(c2)){
    text <- str_remove_all(as.character(c2$taxaShort[i]), "[\\\\\"]")|>
      stringr::str_remove_all( "^c\\(|\\)$")

    vect <- stringr::str_split(string = text, pattern = ",") |>
       unlist()
    
    name <- c2$NAME_ALT[i]
    if(!is.na(text) && text != "NULL"){
      c2$popup[i] <- paste0(
        "<b>", name, "</b> <br>",
        paste("Total Vitis Species:", length(vect)),
        "<br>" ,
        text
        )
    }
  }

  # Return objects as a list if needed, or just print map
  return(list(summaryTable = output, countyCounts = allCounts, mapData = c2, slfData = slfCounties))
}
