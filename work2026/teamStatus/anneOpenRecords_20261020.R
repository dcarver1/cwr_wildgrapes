# Reviewer decisions per gap taxon, from the completed workbook
# (work2026/taxaReviewAnne/taxonChangeRawData_20260924v2.xlsx, records tab,
# decisions in AF.decision). Output feeds teamStatus_<date>.R.
#
# The review closed on 2026-10-19: every row carries a decision. Rows whose
# decision asks for more ("Unclear ...", "Review ...") were settled by project
# decisions the same day (work2026/taxaReviewAnne/reviewDecisions_20261019.md)
# and are counted as settledByProject, with the names involved. Only rows with
# reason == gbifTaxonomy belong to the review; other reasons (country exemption,
# driver one-offs) are counted separately as nonTaxonomic.
suppressPackageStartupMessages({library(openxlsx); library(dplyr); library(readr)})
args <- commandArgs(trailingOnly = TRUE); d <- if (length(args) >= 1) args[1] else format(Sys.Date(), "%Y%m%d")

r <- read.xlsx("work2026/taxaReviewAnne/taxonChangeRawData_20260924v2.xlsx", sheet = "records") |>
  mutate(dec = ifelse(is.na(AF.decision) | AF.decision == "", "not reviewed", trimws(AF.decision)),
         kind = case_when(dec == "not reviewed" ~ "open",
                          grepl("^(Unclear|Review)", dec) ~ "settledByProject",
                          grepl("^Agree", dec) ~ "agree",
                          grepl("^Disagree", dec) ~ "disagree",
                          TRUE ~ "other"),
         nm = coalesce(gbif_scientificName, projectRecordedName),
         nmShort = sub("\\s[A-Z(].*$", "", nm),
         dir = ifelse(change == "Removed", "removed", "added"))

tax <- r |> filter(reason == "gbifTaxonomy") |> group_by(ID = species) |>
  summarise(reviewRows = n(), agree = sum(kind == "agree"), disagree = sum(kind == "disagree"),
            settledByProject = sum(kind == "settledByProject"), open = sum(kind == "open"),
            settledNames = { k <- kind == "settledByProject"; if (!any(k)) NA_character_ else { t <- sort(table(paste0(nmShort[k], " (", dir[k], ")")), decreasing = TRUE); paste0(names(t), " ", t, collapse = "; ") } },
            .groups = "drop")
nonTax <- r |> filter(reason != "gbifTaxonomy") |> group_by(ID = species) |> summarise(nonTaxonomic = n(), .groups = "drop")
res <- full_join(tax, nonTax, by = "ID") |> mutate(across(where(is.numeric), ~ coalesce(.x, 0L))) |> arrange(desc(reviewRows))
write_csv(res, sprintf("work2026/teamStatus/anneOpenRecords_%s.csv", d))
cat("decision kinds over all rows:\n"); print(table(r$kind))
print(as.data.frame(res |> select(-settledNames)), row.names = FALSE)
