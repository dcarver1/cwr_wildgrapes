speciesData <- read_csv("temp/allVitisData082025.csv")

species <- sort(c(unique(speciesData$taxon), "Vitis novogranatensis"))


for(i in seq_along(species)){
  t <- species[i]
  # read in metrics data 
  metrics <- paste0("data/Vitis/",t,"/run08282025_1k/results/aucMetrics.csv")
  if(file.exists(metrics)){
    r1 <- read_csv(metrics) |>
      dplyr::mutate(
        taxon = t
      )|>
      dplyr::select(
        taxon, everything()
      )
    if(i == 1){
      df <- r1
    }else{
      df <- bind_rows(df, r1)
    }
  }else{
    r2 <- data.frame(
      taxon = t, 
      ATAUC = NA,
      STAUC = NA,
      ASD15 = NA,
      Valid = NA,
      Reason =  "No Model"
    ) 
    df <- bind_rows(df, r2)
  }
  write_csv(df, "data/Vitis/run08282025_1k/aucMetrics.csv")
}
