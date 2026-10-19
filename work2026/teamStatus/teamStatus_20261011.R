# work2026/teamStatus/teamStatus_20261011.R
# State of the 2026 taxonomy update for the project team: which result each gap taxon currently carries,
# where it comes from, how it compares with the published table, and what could still change.
#   Rscript work2026/teamStatus/teamStatus_20261011.R [date YYYYMMDD]
# Inputs
#   data/datasetsForPublication/gapAnalysisSummary_combined_<date>.csv  (aggregateRuns(), one row per gap taxon)
#   data/datasetsForPublication/gapAnalysisSummary-sheet6.csv           (published table of record)
#   work2026/teamStatus/anneOpenRecords_<date>.csv                       (unreviewed record changes per taxon)
# Outputs: work2026/teamStatus/teamStatus_<date>.xlsx and .csv
suppressPackageStartupMessages({library(dplyr); library(readr); library(stringr); library(tidyr); library(openxlsx)})
args <- commandArgs(trailingOnly = TRUE); d <- if (length(args) >= 1) args[1] else format(Sys.Date(), "%Y%m%d")
comb <- read_csv(sprintf("data/datasetsForPublication/gapAnalysisSummary_combined_%s.csv", d), show_col_types = FALSE)
pub  <- read_csv("data/datasetsForPublication/gapAnalysisSummary-sheet6.csv", show_col_types = FALSE)
# open for review: record changes from the taxonomy correction that carry no decision from the reviewer yet
# (or a decision that asks for more), from taxonChangeRawData_20260924.xlsx; built by work2026/teamStatus/anneOpenRecords.
# Removed rows attach to the published species (record could return), added rows to the round-2 species (could leave).
inPlay <- read_csv("work2026/teamStatus/anneOpenRecords_20261011.csv", show_col_types = FALSE) |>
  transmute(ID, recordsInPlay = anneOpen, openNames = anneOpenNames, nonTaxonomicOpen)

pubS <- pub |> transmute(ID, pub_records = `Total Records`, pub_coords = `Records with coordinates`,
                         pub_FCSex = `FCS exsitu score`, pub_FCSex_cat = `FCSex priority category`,
                         pub_FCSin = `FCS insitu score`, pub_FCSin_cat = `FCSin priority category`,
                         pub_FCSc = `FCSc mean`, pub_FCSc_cat = `FCS mean priority category`)
status <- comb |> transmute(ID, status, sourceRun, records = `Total Records`, coords = `Records with coordinates`, G = `Total G records`, H = `Total H Records`,
                            SRSex = `SRS exsitu`, GRSex = `GRS exsitu`, ERSex = `ERS exsitu`, FCSex = `FCS exsitu score`, FCSex_cat = `FCSex priority category`,
                            SRSin = `SRS insitu`, GRSin = `GRS insitu`, ERSin = `ERS insitu`, FCSin = `FCS insitu score`, FCSin_cat = `FCSin priority category`,
                            FCSc = `FCSc mean`, FCSc_cat = `FCS mean priority category`) |>
  left_join(pubS, by = "ID") |> left_join(inPlay, by = "ID") |>
  mutate(recordsInPlay = coalesce(recordsInPlay, 0), nonTaxonomicOpen = coalesce(nonTaxonomicOpen, 0), pctInPlay = if_else(status == "pending", NA_real_, round(100 * recordsInPlay / pmax(records, 1), 1)),
         records_vs_published = records - pub_records, FCSex_vs_published = round(FCSex - pub_FCSex, 2), FCSin_vs_published = round(FCSin - pub_FCSin, 2),
         FCSc_vs_published = round(FCSc - pub_FCSc, 2),
         categoryChanged = case_when(is.na(pub_FCSc_cat) | is.na(FCSc_cat) ~ NA, TRUE ~ FCSc_cat != pub_FCSc_cat),
         whatCouldChange = case_when(
           status == "unchanged" ~ "Nothing. No taxonomic record change; published values stand.",
           status == "updated" & sourceRun == "run10022026_1k" ~ "Nothing expected. Review complete, re-run under the update run.",
           status == "updated" ~ paste0("Nothing expected. Every record reassigned between concepts carries the reviewer's decision; ", recordsInPlay, " record(s) under a dropped or added synonym unreviewed. Draft-run result stands."),
           status == "provisional" & ID == "Vitis x champinii" ~ "66 genus-only records assigned from the verbatim hybrid formula are unreviewed, and the MP to HP change rests on them.",
           status == "provisional" & ID == "Vitis rotundifolia" ~ "242 records left for V. munsoniana; the reviewer has not yet signed off that split, and the UP to HP change rests on it.",
           status == "provisional" & ID == "Vitis aestivalis var. bicolor" ~ "9 V. argentifolia records; the reviewer suggested removing them from the variety and perhaps adding them to V. aestivalis. Decision pending.",
           status == "provisional" & ID == "Vitis tiliifolia" ~ paste0(recordsInPlay, " records (", pctInPlay, "%) under old synonyms are unreviewed; not expected to alter the priority category. A further 311 records differ for non-taxonomic reasons (country exemption), outside the review."),
           status == "provisional" & pctInPlay >= 5 ~ paste0(recordsInPlay, " records (", pctInPlay, "%) are unreviewed; the result depends on the review."),
           status == "provisional" ~ paste0(recordsInPlay, " records (", pctInPlay, "%) are unreviewed and could move; a change of that size is not expected to alter the priority category."),
           ID == "Vitis popenoei" ~ "22 Muscadinia popenoei records (11%) would join if that combination is added to the include list. Re-run after the decision.",
           ID == "Vitis rufotomentosa" ~ "Concept needs the review. 42 records, 20 with coordinates; the SDM fails the robustness rule, so the 50 km buffer method applies. Needs the review and an interpretation call.",
           ID == "Vitis cinerea var. tomentosa" ~ "Zero records under the corrected taxonomy; 11 V. berlandieri var. tomentosa records would return if that synonym is added. Cannot be modelled either way; needs a decision on how to report it.",
           TRUE ~ "Pending review.")) |>
  arrange(factor(status, levels = c("updated", "provisional", "pending", "unchanged")), ID)

needsInput <- status |> filter(status == "pending") |> select(ID, records, coords, recordsInPlay, openNames, whatCouldChange)
readme <- tibble(item = c("Purpose", "status = unchanged", "status = updated", "status = provisional", "status = pending", "Published comparison", "Records in play", "Sources", "Caveat"),
  text = c("One row per gap-analysis taxon: which result it currently carries, the run it comes from, the published value beside it, and what could still change.",
           "No taxonomic record change from the review. Published values stand (run08282025_1k, with the published table taking precedence).",
           "Stable on the review input already received (Anne, taxonChangeRawData_20260924.xlsx): every record reassigned between concepts carries her decision and at most 10 records under dropped or added synonyms are unreviewed. sourceRun = run10022026_1k where a sheet edit required a re-run; run09162026_1k where the draft run stands (its records are fingerprint-identical to model_data20261002.csv).",
           "Records identical to the round-2 draft run (run09162026_1k, fingerprint-checked against model_data20261002.csv) but some of its record changes are still unreviewed. Reported as the current best numbers, with that caveat, until the review closes.",
           "Needs input before a result can be reported: V. popenoei, V. rufotomentosa, V. cinerea var. tomentosa.",
           "pub_* columns are the published table (gapAnalysisSummary-sheet6.csv); *_vs_published are current minus published.",
           "Records whose move (into or out of the taxon) carries no reviewer decision yet. They could join or leave the taxon after the review. nonTaxonomicOpen counts record differences outside the review (country exemption, driver one-offs).",
           paste0("Combined table gapAnalysisSummary_combined_", d, ".csv from aggregateRuns(); review workbook taxonChangeRawData_20260924.xlsx; name checklist nameApprovalChecklist_20261002.xlsx."),
           "Provisional results are the project's current best numbers, not final. The corrigendum reports them as provisional until the review signs off."))
wb <- createWorkbook()
addTab <- function(n, x) { addWorksheet(wb, n); writeData(wb, n, x, headerStyle = createStyle(textDecoration = "bold")); freezePane(wb, n, firstRow = TRUE, firstCol = TRUE); setColWidths(wb, n, 1:ncol(x), "auto") }
addTab("Read me", readme); addTab("Status", status); addTab("Needs input", needsInput)
saveWorkbook(wb, sprintf("work2026/teamStatus/teamStatus_%s.xlsx", d), overwrite = TRUE)
write_csv(status, sprintf("work2026/teamStatus/teamStatus_%s.csv", d))
options(width = 230)
cat("\n== status counts\n"); print(count(status, status, sourceRun))
cat("\n== per taxon\n"); print(status |> select(ID, status, records, pub_records, FCSc, pub_FCSc, FCSc_cat, pub_FCSc_cat, categoryChanged, recordsInPlay, pctInPlay) |> as.data.frame(), right = FALSE, row.names = FALSE)
cat("\n== category changes\n"); print(status |> filter(categoryChanged %in% TRUE) |> select(ID, status, FCSex, pub_FCSex, FCSex_cat, pub_FCSex_cat, FCSin, pub_FCSin, FCSin_cat, pub_FCSin_cat, FCSc, pub_FCSc, FCSc_cat, pub_FCSc_cat) |> as.data.frame(), right = FALSE, row.names = FALSE)
