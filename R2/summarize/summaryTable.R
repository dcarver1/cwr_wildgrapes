
# compiles data to reproduce a summary table modeled after the PNAS SI table 4
summaryTable <- function(species, runVersion){
  # get species links 
  for(i in seq_along(species)){
    taxon <- species[i]
    # run path 
    p1 <- paste0("data/Vitis/",taxon,"/", runVersion)
    # counts data 
    c1 <- read_csv(file = paste0(p1, "/occurances/counts.csv"))|>
      dplyr::select(ID = "species",
                    "Total Records" = totalRecords,
                    "Records with latitude" = hasLat,
                    "Records with longitude" = hasLong,
                    "Records with coordinates" = totalUseful,
                    "Total G records" = totalGRecords,
                    "Total G records with coordinates" = totalGUseful,
                    "Total H Records" = totalHRecords,
                    "Total H with coordinates" = numberOfUniqueSources
                    )
    # model statistics summary
    # m1 <- read_csv(file = paste0(p1, "/results/evaluationTable.csv"))
    
    # conservation assessments
    finalConScore <- paste0(p1, "/gap_analysis/fcs_combined.csv")
    if(file.exists(finalConScore)){
    fex <- read_csv(file =paste0(p1, "/gap_analysis/fcs_ex.csv") ) |>
      dplyr::select(ID,
                    "SRS exsitu" = SRS,
                    "GRS exsitu" = GRS,
                    "ERS exsitu" = ERS, 
                    "FCS exsitu score" = FCS, 
                    "FCSex priority category" = FCS_Score)
    fin <- read_csv(file = paste0(p1, "/gap_analysis/fcs_in.csv"))|>
      dplyr::select(ID,
                    "SRS insitu" = SRS,
                    "GRS insitu" = GRS,
                    "ERS insitu" = ERS, 
                    "FCS insitu score" = FCS, 
                    "FCSin priority category" = FCS_Score)
    fcs <- read_csv(file = paste0(p1, "/gap_analysis/fcs_combined.csv"))|>
      dplyr::select(ID,
                    "FCS minimum" = FCSc_min,
                    "FCS maximum" = FCSc_max,
                    "FCSc mean" = FCSc_mean, 
                    "FCS minimum priority category" = FCSc_min_class ,
                    "FCS maximum priority category" = FCSc_max_class ,
                    "FCS mean priority category" = FCSc_mean_class)
    # bind rows 
    ca <- c1 |>
      dplyr::left_join(fex, by = "ID") |>
      dplyr::left_join(fin, by = "ID") |>
      dplyr::left_join(fcs, by = "ID") 
    }else{
      df <- data.frame(matrix(nrow =1, ncol = 16))
      names(df) <- c("SRS exsitu",
                    "GRS exsitu",
                    "ERS exsitu", 
                    "FCS exsitu score", 
                    "FCSex priority category",
                    "SRS insitu",
                    "GRS insitu",
                    "ERS insitu", 
                    "FCS insitu score", 
                    "FCSin priority category",
                    "FCS minimum",
                    "FCS maximum",
                    "FCSc mean", 
                    "FCS minimum priority category",
                    "FCS maximum priority category",
                    "FCS mean priority category")
      # bind rows 
      ca <- bind_cols(c1, df)
        
    }
    
    if(i == 1){
      data <- ca
    }else{
      data <- bind_rows(data,ca)
    }
  }
  return(data)
}
