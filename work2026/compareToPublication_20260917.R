# work2026/compareToPublication_20260917.R
# Per-species comparison of the published input dataset
# (data/datasetsForPublication/allSpeciesOccurrences.csv) with the round-2
# model data (model_data20260820_taxonomyOnly.csv), and attribution of every
# record-level difference to one of:
#   gbifTaxonomy   : record moved concept / entered / left because of the parser
#                    and synonym fixes (present in work2026/taxonChangeFlows or
#                    in excludedOnTaxonomy_08202026.csv)
#   countryExempt  : tiliifolia / popenoei record outside USA/CAN/MEX (the
#                    published file took these from a July 2025 pre-filter file;
#                    round 2 takes them from the current parse)
#   clearNewErrors : one-off coordinate removal from the summary-map review
#   dec16Driver    : the two hard-coded outlier removals and the novogranatensis
#                    source relabel added to the driver on 2025-12-16 (the
#                    publication file predates them)
#   other          : not explained by the above (look at these)
# Output: work2026/publication_vs_round2_byTaxon.csv, ..._records.csv
suppressPackageStartupMessages({library(dplyr); library(readr); library(tidyr); library(stringr)})
options(width = 220)
key <- function(d) paste(d$databaseSource, coalesce(d$sourceUniqueID, paste(d$originalTaxon, d$latitude, d$longitude, d$localityInformation, d$yearRecorded, d$institutionCode, d$observerName, sep = "~")), sep = "|")
rd <- function(p) { d <- read_csv(p, col_types = cols(.default = "c"), show_col_types = FALSE, progress = FALSE); d$key <- key(d); d }
pub <- rd("data/datasetsForPublication/allSpeciesOccurrences.csv")
r2  <- rd("data/processed_occurrence/model_data20260820_taxonomyOnly.csv")
ex  <- read_csv("data/processed_occurrence/excludedOnTaxonomy_08202026.csv", col_types = cols(.default = "c"), show_col_types = FALSE, progress = FALSE)
exKey <- paste(ex$databaseSource, coalesce(ex$sourceUniqueID, ""), sep = "|")
flows <- read_csv("work2026/taxonChangeFlows_20260916.csv", show_col_types = FALSE)
flowNames <- unique(str_squish(flows$recordedName))
nam <- c("USA", "CAN", "MEX")
cne <- function(d) { lat <- suppressWarnings(as.numeric(d$latitude)); lon <- suppressWarnings(as.numeric(d$longitude))
  (d$taxon == "Vitis cinerea" & lat == 0) | (d$taxon == "Vitis palmata" & (lat == 0 | round(lat, 4) == 26.7333)) |
  (d$taxon == "Vitis riparia" & lat == 0) | (d$taxon == "Vitis shuttleworthii" & lon > -28) | (d$taxon == "Vitis vulpina" & lon == 0) |
  (d$taxon == "Vitis labrusca" & d$sourceUniqueID %in% "UNA00061932") | (d$taxon == "Vitis peninsularis" & d$sourceUniqueID %in% "UCJEPS:UC:UC1191193") }
dec16 <- function(d) { lat <- suppressWarnings(as.numeric(d$latitude)); lon <- suppressWarnings(as.numeric(d$longitude))
  (round(lat, 6) == 82.233333) | (round(lon, 4) == -177.2805) | d$taxon == "Vitis novogranatensis" }
kp <- paste(pub$key, pub$taxon); kr <- paste(r2$key, r2$taxon)
pubOnly <- pub[!kp %in% kr, ] |> mutate(direction = "inPublicationNotRound2")
r2Only  <- r2[!kr %in% kp, ]  |> mutate(direction = "inRound2NotPublication")
attrib <- function(d) d |> mutate(
  inOtherConceptNow = key %in% r2$key & !(paste(key, taxon) %in% kr),
  inExcludedTaxonomy = key %in% exKey,
  nameInFlows = str_squish(coalesce(originalTaxon, "")) %in% flowNames | str_remove(str_squish(coalesce(originalTaxon, "")), "\\s+[A-Z(].*$") %in% flowNames,
  isExempt = taxon %in% c("Vitis tiliifolia", "Vitis popenoei") & !(iso3 %in% nam) & !is.na(iso3),
  reason = case_when(
    coalesce(cne(d), FALSE) ~ "clearNewErrors",
    coalesce(dec16(d), FALSE) ~ "dec16Driver",
    isExempt ~ "countryExempt",
    inOtherConceptNow | inExcludedTaxonomy | nameInFlows ~ "gbifTaxonomy",
    TRUE ~ "other"))
recs <- bind_rows(attrib(pubOnly), attrib(r2Only)) |>
  select(direction, taxon, reason, databaseSource, originalTaxon, iso3, latitude, longitude, sourceUniqueID, inOtherConceptNow, inExcludedTaxonomy)
write_csv(recs, "work2026/publication_vs_round2_records.csv")
byTaxon <- full_join(pub |> count(taxon, name = "publication"), r2 |> count(taxon, name = "round2"), by = "taxon") |>
  mutate(across(-taxon, ~coalesce(.x, 0L)), change = round2 - publication) |>
  left_join(recs |> count(taxon, direction, reason) |> mutate(col = paste0(if_else(direction == "inRound2NotPublication", "gained_", "lost_"), reason)) |>
              select(taxon, col, n) |> pivot_wider(names_from = col, values_from = n, values_fill = 0), by = "taxon") |>
  mutate(across(-taxon, ~coalesce(.x, 0L))) |> arrange(desc(abs(change)), taxon)
write_csv(byTaxon, "work2026/publication_vs_round2_byTaxon.csv")
write_csv(byTaxon, "temp/publication_vs_round2_byTaxon.csv")
cat("rows: publication", nrow(pub), " round2", nrow(r2), "\n")
cat("records only in publication:", nrow(pubOnly), " only in round 2:", nrow(r2Only), "\n\n")
print(as.data.frame(recs |> count(direction, reason)), row.names = FALSE)
cat("\n=== per taxon (taxa that differ) ===\n")
print(as.data.frame(byTaxon |> filter(change != 0 | rowSums(across(starts_with(c("gained_", "lost_")))) > 0)), row.names = FALSE)
cat("\n=== 'other' records, by taxon and source ===\n")
print(as.data.frame(recs |> filter(reason == "other") |> count(direction, taxon, databaseSource, sort = TRUE) |> head(30)), row.names = FALSE)
