# Compact per-taxon effects summary, one row per taxon, grouped by status class.
# Derived from teamStatus_20261011.csv (built by teamStatus_20261011.R).
suppressPackageStartupMessages({library(dplyr); library(readr); library(stringr)})

st <- read_csv("teamStatus_20261011.csv", show_col_types = FALSE)

classOrder <- c(unchanged = 1, updated = 2, provisional = 3, pending = 4)
classLabel <- c(unchanged = "Unchanged", updated = "Updated", provisional = "Provisional", pending = "Needs input")

pooled <- c("Vitis aestivalis", "Vitis cinerea")

eff <- st %>%
  mutate(
    class = classLabel[status],
    recordsNow      = records,
    recordsPublished = pub_records,
    recordsDelta    = records - pub_records,
    FCSc_published  = round(pub_FCSc, 1),
    FCSc_now        = round(FCSc, 1),
    FCSc_delta      = round(FCSc - pub_FCSc, 1) + 0,
    category_published = pub_FCSc_cat,
    category_now       = FCSc_cat,
    categoryChange = case_when(
      status == "pending" ~ "held",
      categoryChanged ~ paste(pub_FCSc_cat, "to", FCSc_cat),
      TRUE ~ "none"),
    FCSex_now = round(FCSex, 1), FCSin_now = round(FCSin, 1),
    FCSex_delta = round(FCSex - pub_FCSex, 1) + 0, FCSin_delta = round(FCSin - pub_FCSin, 1) + 0,
    recordsOpen = recordsInPlay,
    pctOpen = ifelse(is.na(pctInPlay), NA, round(pctInPlay, 1)),
    note = whatCouldChange,
    note = ifelse(ID %in% pooled,
                  paste0(note, " Record counts here pool the varieties (as the model does); the published table printed species-only counts, so the record delta is not like for like. Scores are comparable."),
                  note)
  ) %>%
  arrange(classOrder[status], ID) %>%
  select(taxon = ID, class, sourceRun,
         recordsPublished, recordsNow, recordsDelta,
         FCSex_now, FCSex_delta, FCSin_now, FCSin_delta,
         FCSc_published, FCSc_now, FCSc_delta,
         category_published, category_now, categoryChange,
         recordsOpen, pctOpen, nonTaxonomicOpen, openNames, note)

write_csv(eff, "taxonEffects_20261011.csv", na = "")
cat("rows:", nrow(eff), "\n")
print(table(eff$class))
