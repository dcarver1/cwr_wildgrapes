library(dplyr)
library(readr)

# 1. Load data
v_county <- read_csv("data/countyMaps/vitisSummarizedByCounty.csv")
t_summary <- read_csv("data/countyMaps/countyCountsSummaryTable.csvv")

# 2. Top density counties
v_county <- v_county %>%
  mutate(across(c(O, H, G, bonap, USDA), ~replace_na(., 0)),
         Total_Obs = O + H + G)

# Highest records
top_total <- v_county %>% filter(Total_Obs == max(Total_Obs))
top_herb <- v_county %>% filter(H == max(H))
top_germ <- v_county %>% filter(G == max(G))

# 3. Unverified counties (Checklist presence but no H or G)
unverified <- v_county %>%
  filter((bonap > 0 | USDA > 0) & H == 0 & G == 0)

# Unverified with no observation data (No O/iNaturalist)
priority_unverified <- unverified %>% filter(O == 0)

# 4. Range summaries
top_species_richness_usda <- v_county %>% filter(USDA == max(USDA))

# Output stats
print(paste("Top Total Observations:", top_total$name, top_total$Total_Obs))
print(paste("Counties with checklist presence but no vouchers:", nrow(unverified)))
print(paste("Priority counties (no vouchers and no iNat):", nrow(priority_unverified)))