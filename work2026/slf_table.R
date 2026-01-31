library(dplyr)
library(readr)
library(stringr)
library(tidyr)

# 1. Load the Datasets ----------------------------------------------------
# Update paths if necessary to match your working directory
taxa_stats <- read_csv("~/trueNAS/work/cwr_wildgrapes/temp/slf/slfCountiesPerTaxa.csv")

# 2. Define Range Estimates Manually --------------------------------------
# Since the county join file is missing taxa names, we assign ranges 
# based on the known distribution of these specific Vitis taxa.

taxa_ranges <- taxa_stats %>%
  mutate(
    Range_Estimate = case_when(
      # Western Species (Pacific/Southwest)
      str_detect(taxon, "californica|arizonica|girdiana") ~ "Western",
      
      # Northern/Eastern Species (Cool Climate/New England)
      str_detect(taxon, "novae-angliae|labrusca|aestivalis var. bicolor") ~ "Northern/Eastern",
      
      # Southern Species (Texas, Florida, Southeast)
      str_detect(taxon, "rotundifolia|mustangensis|shuttleworthii|berlandieri|monticola|acerifolia|lincecumii|rufotomentosa|rupestris|champinii|doaniana|baileyana") ~ "Southern",
      
      # Widespread Species (Broad Eastern US Distribution)
      str_detect(taxon, "aestivalis|riparia|vulpina|cinerea") ~ "Widespread",
      
      # Fallback for any others
      TRUE ~ "Widespread"
    )
  )

# 3. Create Final Summary Table -------------------------------------------
final_table <- taxa_ranges %>%
  mutate(
    # Calculate Percent Overlap
    # Ensure totalCounties is not zero to avoid division by zero
    Percent_Overlap = if_else(totalCounties > 0, (slfCounties / totalCounties) * 100, 0),
    
    # Round to 2 decimal places
    Percent_Overlap = round(Percent_Overlap, 2)
  ) %>%
  # Rename columns to match desired output
  select(
    Taxon = taxon,
    `Total Counties` = totalCounties,
    `Counties with SLF` = slfCounties,
    `Percent Overlap (%)` = Percent_Overlap,
    `Estimated Range` = Range_Estimate
  ) %>%
  # Sort by highest overlap first
  arrange(desc(`Percent Overlap (%)`))

# 4. View and Export ------------------------------------------------------
print(final_table)

# Export to CSV
write_csv(final_table, "~/trueNAS/work/cwr_wildgrapes/temp/slf/SLF_Species_Impact_Summary.csv")
