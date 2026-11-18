# Compact per-taxon effects summary, one row per taxon, grouped by status class.
# Derived from teamStatus_<date>.csv (built by teamStatus_<date>.R).
#   Rscript work2026/teamStatus/taxonEffects_20261115.R [date YYYYMMDD]
suppressPackageStartupMessages({library(dplyr); library(readr)})
args <- commandArgs(trailingOnly = TRUE); d <- if (length(args) >= 1) args[1] else format(Sys.Date(), "%Y%m%d")
st <- read_csv(sprintf("work2026/teamStatus/teamStatus_%s.csv", d), show_col_types = FALSE)
eff <- st |>
  mutate(class = ifelse(status == "updated", "Updated", "Unchanged"),
         recordsDelta = records - pub_records,
         FCSc_published = round(pub_FCSc, 1), FCSc_now = round(FCSc, 1), FCSc_delta = round(FCSc - pub_FCSc, 1) + 0,
         FCSex_now = round(FCSex, 1), FCSex_delta = round(FCSex - pub_FCSex, 1) + 0,
         FCSin_now = round(FCSin, 1), FCSin_delta = round(FCSin - pub_FCSin, 1) + 0,
         categoryChange = case_when(categoryChanged %in% TRUE ~ paste(coalesce(pub_FCSc_cat, "not scored"), "to", FCSc_cat), TRUE ~ "none"),
         note = ifelse(is.na(pooledNote), whatCouldChange, paste(whatCouldChange, pooledNote))) |>
  arrange(desc(status == "updated"), ID) |>
  select(taxon = ID, class, sourceRun, rerunGroup, recordsPublished = pub_records, recordsNow = records, recordsDelta,
         FCSex_now, FCSex_delta, FCSin_now, FCSin_delta, FCSc_published, FCSc_now, FCSc_delta,
         category_published = pub_FCSc_cat, category_now = FCSc_cat, categoryChange,
         reviewRows, agree, disagree, settledByProject, removedByReviewer, settledNames, note)
write_csv(eff, sprintf("work2026/teamStatus/taxonEffects_%s.csv", d), na = "")
cat("rows:", nrow(eff), "\n"); print(table(eff$class))
