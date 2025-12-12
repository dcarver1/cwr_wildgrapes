

# Function to calculate ASD15 from a stack of model predictions (folds)
calc_sdm_metrics <- function(sd_raster, auc_scores) {
  # convert to vector of auc values 
  aucs <- auc_scores |> unlist(use.names = FALSE)
  # convert to terra vect 
  sdRast <-sd_raster
  # 1. ATAUC and STAUC (Scalar calculations)
  atauc <- mean(aucs, na.rm = TRUE)
  stauc <- sd(aucs, na.rm = TRUE)
  global(sdRast, "max", na.rm = TRUE)
  
  # 2. ASD15 using the standard deviation raster 
  # Calculate proportion of cells where SD > 0.15
  asd15 <- global(sdRast > 0.15, fun = "mean", na.rm = TRUE)[[1]]
  
  # store as a dataframe and test for pass 
  df <- data.frame (
    ATAUC = atauc,
    STAUC = stauc,
    ASD15 = asd15
  ) |>
    dplyr::mutate(
      # 1. The Boolean Check (TRUE/FALSE)
      Valid = ATAUC > 0.7 & STAUC < 0.15 & ASD15 < 10,
      # 2. Build the failure string
      # We paste together text for any condition that fails
      Reason = paste0(
        ifelse(ATAUC <= 0.7, "Low ATAUC; ", ""),
        ifelse(STAUC >= 0.15, "High STAUC; ", ""),
        ifelse(ASD15 >= 10,  "High ASD15; ", "")
      )
    ) |>
    # 3. Clean up the string formatting
    mutate(
      # Remove the trailing "; " from the end of the string
      Reason = sub("; $", "", Reason),
      # If Valid is TRUE, make Reason "NA" (or "Pass" if you prefer)
      Reason = ifelse(Valid, "All Conditions Meet", Reason)
    )
  
  
  return(df)
}
