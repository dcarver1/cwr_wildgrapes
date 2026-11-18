# work2026/teamStatus/teamStatus_20261106.R
# State of the 2026 taxonomy update for the project team, version of 6 Nov 2026: the 20 Oct 2026 state plus the
# V. tiliifolia re-run of 22 Oct 2026 (California and Florida records removed at the reviewer's request).
# One row per gap taxon: which result it carries, where it comes from, how it compares with the published table,
# and what (if anything) could still change.
#   Rscript work2026/teamStatus/teamStatus_20261106.R [date YYYYMMDD] [combined-table date YYYYMMDD]
# Inputs
#   data/datasetsForPublication/gapAnalysisSummary_combined_<tableDate>.csv  (aggregateRuns(), one row per gap taxon)
#   data/datasetsForPublication/gapAnalysisSummary-sheet6.csv                (published table of record)
#   work2026/teamStatus/anneOpenRecords_<date>.csv                            (reviewer decisions per taxon)
#   work2026/rerun/rerunGroups_20261019.csv                                   (why each taxon was or was not re-run)
#   data/processed_occurrence/removedByReview_20261022.csv                    (reviewer-requested removals)
# Outputs: work2026/teamStatus/teamStatus_<date>.xlsx and .csv
suppressPackageStartupMessages({library(dplyr); library(readr); library(stringr); library(tidyr); library(openxlsx)})
args <- commandArgs(trailingOnly = TRUE)
d  <- if (length(args) >= 1) args[1] else format(Sys.Date(), "%Y%m%d")
td <- if (length(args) >= 2) args[2] else "20261029"
comb <- read_csv(sprintf("data/datasetsForPublication/gapAnalysisSummary_combined_%s.csv", td), show_col_types = FALSE)
pub  <- read_csv("data/datasetsForPublication/gapAnalysisSummary-sheet6.csv", show_col_types = FALSE)
rev  <- read_csv(sprintf("work2026/teamStatus/anneOpenRecords_%s.csv", d), show_col_types = FALSE) |>
  transmute(ID, reviewRows, agree, disagree, settledByProject, open, settledNames, nonTaxonomic)
grp  <- read_csv("work2026/rerun/rerunGroups_20261019.csv", show_col_types = FALSE) |>
  transmute(ID = taxon, rerunGroup = group, rerunReason = reason, stateListSource)
rem  <- read_csv("data/processed_occurrence/removedByReview_20261022.csv", show_col_types = FALSE) |>
  count(ID = taxon, name = "removedByReviewer")

pubS <- pub |> transmute(ID, pub_records = `Total Records`, pub_coords = `Records with coordinates`,
                         pub_FCSex = `FCS exsitu score`, pub_FCSex_cat = `FCSex priority category`,
                         pub_FCSin = `FCS insitu score`, pub_FCSin_cat = `FCSin priority category`,
                         pub_FCSc = `FCSc mean`, pub_FCSc_cat = `FCS mean priority category`)
status <- comb |> transmute(ID, status, sourceRun, records = `Total Records`, coords = `Records with coordinates`, G = `Total G records`, H = `Total H Records`,
                            SRSex = `SRS exsitu`, GRSex = `GRS exsitu`, ERSex = `ERS exsitu`, FCSex = `FCS exsitu score`, FCSex_cat = `FCSex priority category`,
                            SRSin = `SRS insitu`, GRSin = `GRS insitu`, ERSin = `ERS insitu`, FCSin = `FCS insitu score`, FCSin_cat = `FCSin priority category`,
                            FCSc = `FCSc mean`, FCSc_cat = `FCS mean priority category`) |>
  left_join(pubS, by = "ID") |> left_join(rev, by = "ID") |> left_join(grp, by = "ID") |> left_join(rem, by = "ID") |>
  mutate(across(c(reviewRows, agree, disagree, settledByProject, open, nonTaxonomic, removedByReviewer), ~ coalesce(.x, 0)),
         rerunGroup = ifelse(sourceRun == "run10222026_1k", "C: reviewer removal", rerunGroup),
         rerunReason = ifelse(sourceRun == "run10222026_1k", "records removed at the reviewer's request (2026-10-22); re-run", rerunReason),
         records_vs_published = records - pub_records, FCSex_vs_published = round(FCSex - pub_FCSex, 2), FCSin_vs_published = round(FCSin - pub_FCSin, 2),
         FCSc_vs_published = round(FCSc - pub_FCSc, 2),
         # a taxon the published table could not score (no coordinates, no model) counts as a change when it is scored now
         categoryChanged = case_when(is.na(FCSc_cat) ~ NA, is.na(pub_FCSc_cat) ~ TRUE, TRUE ~ FCSc_cat != pub_FCSc_cat),
         componentChanged = (!is.na(FCSex_cat) & coalesce(FCSex_cat != pub_FCSex_cat, TRUE)) | (!is.na(FCSin_cat) & coalesce(FCSin_cat != pub_FCSin_cat, TRUE)),
         resultFrom = case_when(
           status == "unchanged" ~ "Published run; published table values.",
           sourceRun == "run10222026_1k" ~ "Re-run 22 Oct 2026 after the reviewer asked for the California and Florida records to be removed (26 records); records otherwise as in the 19 Oct dataset.",
           sourceRun == "run10192026_1k" & rerunGroup == "B: geography" ~ "Re-run 19-20 Oct 2026 on the final records with a state range list added.",
           sourceRun == "run10192026_1k" ~ "Re-run 19-20 Oct 2026 on the final records.",
           sourceRun == "run10022026_1k" ~ "Re-run 2 Oct 2026 after the first sheet edits; records unchanged since (fingerprint check).",
           sourceRun == "run09162026_1k" ~ "Draft run of 16 Sept 2026 stands; records identical to the final dataset (fingerprint check)."),
         whatCouldChange = case_when(
           status == "unchanged" ~ "Nothing. No taxonomic record change; published values stand.",
           ID == "Vitis rotundifolia" ~ "The UP to HP change comes through the model, not the counts. The 243 records that left for V. munsoniana (0.9%) change the fitted SDM: the range shrinks from 761,860 to 651,963 km2 and from 18 to 17 ecoregions, and the same nine germplasm accessions now cover 8 of 17 ecoregions instead of 6 of 18, so ERSex rises from 33 to 47 and FCSex from 12.0 to 17.0. The draft run of 16 Sept gave the same result to three decimals. Reported as is, under the published method.",
           ID == "Vitis tiliifolia" ~ "Four more US records (Illinois 1, Louisiana 1, two with no state) are with the reviewer; removing them would mean one more re-run (about 10 minutes) and no category change is expected. The 22 Oct re-run moved FCSc from 30.6 to 31.0 and left all three categories as they were (UP / MP / HP).",
           ID %in% c("Vitis simpsonii", "Vitis baileyana") ~ "State range list taken from NatureServe; the reviewer was asked to confirm it against Weakley. A different list would mean one more re-run.",
           ID == "Vitis lincecumii" ~ "Nothing expected. State list from Weakley (via the reviewer) removed 10 points, 4 of them in Coahuila and Nuevo Leon.",
           ID == "Vitis labrusca" ~ "Nothing expected. Four 'Vitis obovata' records (none with coordinates) are excluded; the reviewer was asked whether V. obovata Raf. should return. No effect on scores either way.",
           ID == "Vitis cinerea var. tomentosa" ~ "Nothing expected. 11 records under the final concept (9 with coordinates); the SDM passes the robustness rule.",
           ID == "Vitis rufotomentosa" ~ "Nothing expected. With the state list applied the SDM passes the robustness rule (it failed on the draft run), so no buffer fallback.",
           TRUE ~ "Nothing expected. Review complete; every record change carries a decision."),
         pooledNote = ifelse(ID %in% c("Vitis aestivalis", "Vitis cinerea"),
                             "Record counts pool the varieties (as the model does); the published table printed species-only counts, so the record delta is not like for like. Scores are comparable.", NA)) |>
  arrange(factor(status, levels = c("updated", "unchanged")),
          factor(sourceRun, levels = c("run10222026_1k", "run10192026_1k", "run10022026_1k", "run09162026_1k", "run08282025_1k")), ID)

readme <- tibble(item = c("Purpose", "status = unchanged", "status = updated", "Source runs", "Published comparison", "Category changes tab", "Review columns", "Rerun columns", "Sources", "Open items"),
  text = c("One row per gap-analysis taxon: which result it carries, the run it comes from, the published value beside it, and what could still change. Version of 6 Nov 2026: the 20 Oct 2026 state plus the V. tiliifolia re-run of 22 Oct 2026.",
           "No taxonomic record change from the review. Published values stand (run08282025_1k, with the published table taking precedence).",
           "Records changed with the review (closed 2026-10-19, every row decided) and the result comes from the run in sourceRun. A taxon was re-run only when its record fingerprint changed or a state range list was added (rerunGroups_20261019.csv); otherwise the earlier run whose records are identical to the final dataset stands. V. tiliifolia was re-run once more on 22 Oct 2026 after the reviewer asked for its California and Florida records to be removed.",
           "run10222026_1k: 22 Oct 2026 on model_data20261022.csv (tiliifolia only). run10192026_1k: 19-20 Oct 2026 on model_data20261019_2303.csv (16 taxa). run10022026_1k: 2 Oct 2026 (acerifolia, shuttleworthii). run09162026_1k: 16 Sept draft (6 taxa with identical records). run08282025_1k: published.",
           "pub_* columns are the published table (gapAnalysisSummary-sheet6.csv); *_vs_published are current minus published.",
           "Every taxon whose combined or component category differs from the published table, including a taxon the published table could not score (blank pub_* columns: V. rufotomentosa had 2 records and no coordinates, so no model ran) that is scored now.",
           "reviewRows / agree / disagree / settledByProject / open: the reviewer's decisions on this taxon's record changes (taxonChangeRawData_20260924v2.xlsx). settledByProject = rows she marked Unclear or Review, settled by project decisions on 2026-10-19 (names in settledNames). nonTaxonomic = record differences outside the review. removedByReviewer = records deleted at the reviewer's later request (removedByReview_20261022.csv).",
           "rerunGroup A: records changed; B: state range list added (stateListSource says where the list came from); C: records removed at the reviewer's request after the review closed; untouched: fingerprint identical to the source run.",
           paste0("Combined table gapAnalysisSummary_combined_", td, ".csv from aggregateRuns(); review workbook taxonChangeRawData_20260924v2.xlsx; decisions reviewDecisions_20261019.md; removals removedByReview_20261022.csv; methods notes corrigendumNotes.md."),
           "Three questions are with the reviewer: whether V. obovata Raf. belongs in labrusca (no coordinates, no score effect); whether the NatureServe state lists for simpsonii and baileyana agree with Weakley; and whether the four remaining US records of V. tiliifolia (IL, LA, two with no state) should also go. None blocks the table."))
wb <- createWorkbook()
addTab <- function(n, x) { addWorksheet(wb, n); writeData(wb, n, x, headerStyle = createStyle(textDecoration = "bold")); freezePane(wb, n, firstRow = TRUE, firstCol = TRUE); setColWidths(wb, n, 1:ncol(x), "auto") }
addTab("Read me", readme); addTab("Status", status)
addTab("Category changes", status |> filter(categoryChanged %in% TRUE | componentChanged) |>
         select(ID, sourceRun, FCSex, pub_FCSex, FCSex_cat, pub_FCSex_cat, FCSin, pub_FCSin, FCSin_cat, pub_FCSin_cat, FCSc, pub_FCSc, FCSc_cat, pub_FCSc_cat, whatCouldChange))
saveWorkbook(wb, sprintf("work2026/teamStatus/teamStatus_%s.xlsx", d), overwrite = TRUE)
write_csv(status, sprintf("work2026/teamStatus/teamStatus_%s.csv", d), na = "")
options(width = 230)
cat("\n== status counts\n"); print(count(status, status, sourceRun, rerunGroup))
cat("\n== per taxon\n"); print(status |> select(ID, sourceRun, records, pub_records, records_vs_published, FCSc, pub_FCSc, FCSc_vs_published, FCSc_cat, pub_FCSc_cat, categoryChanged) |> as.data.frame(), right = FALSE, row.names = FALSE)
cat("\n== any category change (combined or component)\n"); print(status |> filter(categoryChanged %in% TRUE | componentChanged) |>
  select(ID, FCSex, pub_FCSex, FCSex_cat, pub_FCSex_cat, FCSin, pub_FCSin, FCSin_cat, pub_FCSin_cat, FCSc, pub_FCSc, FCSc_cat, pub_FCSc_cat) |> as.data.frame(), right = FALSE, row.names = FALSE)
