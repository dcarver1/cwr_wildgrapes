# Unreviewed record changes per gap taxon, from the reviewer's workbook
# (work2026/taxaReviewAnne/taxonChangeRawData_20260924.xlsx, records tab,
# decisions in AF.decision). Output feeds teamStatus_<date>.R.
#
# A row is resolved when it carries a decision, or when it is a removal whose
# paired addition (same gbif_gbifID, species == counterpart) carries one.
# Decisions that ask for more ("Unclear", "Review") stay open. Only rows with
# reason == gbifTaxonomy count as open for the review; other reasons (country
# exemption, driver one-offs) are reported separately as nonTaxonomicOpen.
suppressPackageStartupMessages({library(openxlsx); library(dplyr); library(readr)})
args <- commandArgs(trailingOnly = TRUE); d <- if (length(args) >= 1) args[1] else format(Sys.Date(), "%Y%m%d")

r <- read.xlsx("work2026/taxaReviewAnne/taxonChangeRawData_20260924.xlsx", sheet = "records") |>
  mutate(dec = ifelse(is.na(AF.decision) | AF.decision == "", "not reviewed", trimws(AF.decision)))
add <- r |> filter(change == "Added", dec != "not reviewed") |> transmute(gbif_gbifID, species, addDec = dec)
r <- r |> left_join(add, by = c("gbif_gbifID" = "gbif_gbifID", "counterpart" = "species")) |>
  mutate(effDec = ifelse(dec != "not reviewed", dec, ifelse(!is.na(addDec), paste0("via pair: ", addDec), "not reviewed")),
         openRow = effDec == "not reviewed" | grepl("^(Unclear|Review)", effDec),
         nm = coalesce(gbif_scientificName, projectRecordedName),
         nmShort = sub("\\s[A-Z(].*$", "", nm),
         dir = ifelse(change == "Removed", "could return", "could leave"))

open <- r |> filter(openRow, reason == "gbifTaxonomy") |> group_by(ID = species) |>
  summarise(anneOpen = n(),
            anneOpenNames = { t <- sort(table(paste0(nmShort, " (", dir, ")")), decreasing = TRUE); paste0(names(t), " ", t, collapse = "; ") },
            .groups = "drop")
nonTax <- r |> filter(openRow, reason != "gbifTaxonomy") |> group_by(ID = species) |> summarise(nonTaxonomicOpen = n(), .groups = "drop")
res <- full_join(open, nonTax, by = "ID") |> mutate(across(where(is.numeric), ~ coalesce(.x, 0L))) |> arrange(desc(anneOpen))
write_csv(res, sprintf("work2026/teamStatus/anneOpenRecords_%s.csv", d))
print(as.data.frame(res |> select(-anneOpenNames)), row.names = FALSE)
