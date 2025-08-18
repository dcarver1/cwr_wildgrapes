# data = read_csv("data/processed_occurrence/draft_model_data_withG.csv")
# taxon1 = "Vitis palmata"
# 
# t1 <- removeDups(data = data, taxon1 = taxon1)

removeDups <- function(taxon1,data){
  d1 <- data
  # for each species 
  d2 <- d1 |> dplyr::filter(taxon == taxon1)
  # export temp
  write_csv(x = d2, "temp/vArizonicaAllSources.csv")
  
  # compare datasets where there is expect overlap 
  grin <- c("USDA_NPGS_GRINGlobal","USDA ARS NPGS 2019a") 
  grinDavis <-  c("USDA_NPGS_GRINGlobal","UC Davis Grape Breeding Collection")
  genesys <- c( "genesys",  "Global Crop Diversity Trust 2019a (Genesys)")
  gbif <- c("GBIF","GBIF 2019") 
  mwh <- c( "seinet","Midwest Herbaria 2019")
  wiews <- c("wiews","FAO 2019 (WIEWS)")
  gbif_mdh <-c("GBIF","GBIF 2019", "midwestHerbarium","Midwest Herbaria 2019") 
  
  
  # function for filtering the source ID 
  sourceID_Check <- function(sources,data){
    d1 <- data |>
      dplyr::filter(databaseSource %in% sources ) |> # subset to features of interest
      arrange(match(databaseSource, sources))|> # order so the dulicate test will work 
      dplyr::filter(!duplicated(`sourceUniqueID`))
    return(d1)
  }
  
  #grin 
  d2a <- sourceID_Check(grin, d2)
  # grin Davis 
  d2b <- sourceID_Check(grinDavis, d2)
  # genesys
  d2c <- sourceID_Check(genesys, d2)
  # gbif 
  d2d <- sourceID_Check(gbif, d2)
  # midwest herberium 
  d2e <- sourceID_Check(mwh, d2)
  # wiews
  d2f <- sourceID_Check(wiews, d2)
  
  # combined back to single df 
  df <-bind_rows(list(d2a,d2b,d2c,d2d,d2e,d2f))
  
  # gbif_mdh 
  ## exclude NA from any of the filtering columns 
  dfna <- df |>
    dplyr::filter(is.na(latitude) |
                  is.na(longitude)|
                  is.na(institutionCode))
  # grab all records with no NA values 
  df_eval <- df |>
    filter(!index %in% dfna$index)
  # arrange so that source order is kept
  df2 <- df_eval |>
    dplyr::filter(databaseSource %in% gbif_mdh)|> # subset to features of interest
    arrange(match(databaseSource, gbif_mdh)) 
  # remove duplicated features
  df2a <- df2[!duplicated(df2[,c("latitude","longitude","institutionCode")]),]
  # bind back to na data 
  df3 <- bind_rows(dfna, df2a)
  write_csv(df3, file = "temp/vArizonica_afterDupRemoved.csv")
  return(df3)  
}
