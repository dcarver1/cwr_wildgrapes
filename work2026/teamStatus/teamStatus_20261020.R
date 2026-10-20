# work2026/teamStatus/teamStatus_20261020.R
# State of the 2026 taxonomy update for the project team after the review closed and the final runs completed:
# which result each gap taxon carries, where it comes from, how it compares with the published table, and what
# (if anything) could still change.
#   Rscript work2026/teamStatus/teamStatus_20261020.R [date YYYYMMDD]
# Inputs
#   data/datasetsForPublication/gapAnalysisSummary_combined_<date>.csv  (aggregateRuns(), one row per gap taxon)
#   data/datasetsForPublication/gapAnalysisSummary-sheet6.csv           (published table of record)
#   work2026/teamStatus/anneOpenRecords_<date>.csv                       (reviewer decisions per taxon)
#   work2026/rerun/rerunGroups_20261019.csv                              (why each taxon was or was not re-run)
# Outputs: work2026/teamStatus/teamStatus_<date>.xlsx and .csv
suppressPackageStartupMessages({library(dplyr); library(readr); library(stringr); library(tidyr); library(openxlsx)})
args <- commandArgs(trailingOnly = TRUE); d <- if (length(args) >= 1) args[1] else format(Sys.Date(), "%Y%m%d")
comb <- read_csv(sprintf("data/datasetsForPublication/gapAnalysisSummary_combined_%s.csv", d), show_col_types = FALSE)
pub  <- read_csv("data/datasetsForPublication/gapAnalysisSummary-sheet6.csv", show_col_types = FALSE)
rev  <- read_csv(sprintf("work2026/teamStatus/anneOpenRecords_%s.csv", d), show_col_types = FALSE) |>
  transmute(ID, reviewRows, agree, disagree, settledByProject, open, settledNames, nonTaxonomic)
grp  <- read_csv("work2026/rerun/rerunGroups_20261019.csv", show_col_types = FALSE) |>
  transmute(ID = taxon, rerunGroup = group, rerunReason = reason, stateListSource)

pubS <- pub |> transmute(ID, pub_records = `Total Records`, pub_coords = `Records with coordinates`,
                         pub_FCSex = `FCS exsitu score`, pub_FCSex_cat = `FCSex priority category`,
                         pub_FCSin = `FCS insitu score`, pub_FCSin_cat = `FCSin priority category`,
                         pub_FCSc = `FCSc mean`, pub_FCSc_cat = `FCS mean priority category`)
status <- comb |> transmute(ID, status, sourceRun, records = `Total Records`, coords = `Records with coordinates`, G = `Total G records`, H = `Total H Records`,
                            SRSex = `SRS exsitu`, GRSex = `GRS exsitu`, ERSex = `ERS exsitu`, FCSex = `FCS exsitu score`, FCSex_cat = `FCSex priority category`,
                            SRSin = `SRS insitu`, GRSin = `GRS insitu`, ERSin = `ERS insitu`, FCSin = `FCS insitu score`, FCSin_cat = `FCSin priority category`,
                            FCSc = `FCSc mean`, FCSc_cat = `FCS mean priority category`) |>
  left_join(pubS, by = "ID") |> left_join(rev, by = "ID") |> left_join(grp, by = "ID") |>
  mutate(across(c(reviewRows, agree, disagree, settledByProject, open, nonTaxonomic), ~ coalesce(.x, 0)),
         records_vs_published = records - pub_records, FCSex_vs_published = round(FCSex - pub_FCSex, 2), FCSin_vs_published = round(FCSin - pub_FCSin, 2),
         FCSc_vs_published = round(FCSc - pub_FCSc, 2),
         categoryChanged = case_when(is.na(pub_FCSc_cat) | is.na(FCSc_cat) ~ NA, TRUE ~ FCSc_cat != pub_FCSc_cat),
         resultFrom = case_when(
           status == "unchanged" ~ "Published run; published table values.",
           sourceRun == "run10192026_1k" & rerunGroup == "B: geography" ~ "Re-run 19-20 Oct 2026 on the final records with a state range list added.",
           sourceRun == "run10192026_1k" ~ "Re-run 19-20 Oct 2026 on the final records.",
           sourceRun == "run10022026_1k" ~ "Re-run 2 Oct 2026 after the first sheet edits; records unchanged since (fingerprint check).",
           sourceRun == "run09162026_1k" ~ "Draft run of 16 Sept 2026 stands; records identical to the final dataset (fingerprint check)."),
         whatCouldChange = case_when(
           status == "unchanged" ~ "Nothing. No taxonomic record change; published values stand.",
           ID == "Vitis rotundifolia" ~ "The UP to HP change comes through the model, not the counts. The 243 records that left for V. munsoniana (0.9%) change the fitted SDM: the range shrinks from 761,860 to 651,963 km2 and from 18 to 17 ecoregions, and the same nine germplasm accessions now cover 8 of 17 ecoregions instead of 6 of 18, so ERSex rises from 33 to 47 and FCSex from 12.0 to 17.0. The draft run of 16 Sept gave the same result to three decimals. Reported as is, under the published method.",
           ID %in% c("Vitis simpsonii", "Vitis baileyana") ~ "State range list taken from NatureServe; the reviewer was asked to confirm it against Weakley. A different list would mean one more re-run.",
           ID == "Vitis lincecumii" ~ "Nothing expected. State list from Weakley (via the reviewer) removed 10 points, 4 of them in Coahuila and Nuevo Leon.",
           ID == "Vitis labrusca" ~ "Nothing expected. Four 'Vitis obovata' records (none with coordinates) are excluded; the reviewer was asked whether V. obovata Raf. should return. No effect on scores either way.",
           ID == "Vitis cinerea var. tomentosa" ~ "Nothing expected. 11 records under the final concept (9 with coordinates); the SDM passes the robustness rule.",
           ID == "Vitis rufotomentosa" ~ "Nothing expected. With the state list applied the SDM passes the robustness rule (it failed on the draft run), so no buffer fallback.",
           TRUE ~ "Nothing expected. Review complete; every record change carries a decision."),
         pooledNote = ifelse(ID %in% c("Vitis aestivalis", "Vitis cinerea"),
                             "Record counts pool the varieties (as the model does); the published table printed species-only counts, so the record delta is not like for like. Scores are comparable.", NA)) |>
  arrange(factor(status, levels = c("updated", "unchanged")), factor(sourceRun, levels = c("run10192026_1k", "run10022026_1k", "run09162026_1k", "run08282025_1k")), ID)

readme <- tibble(item = c("Purpose", "status = unchanged", "status = updated", "Source runs", "Published comparison", "Review columns", "Rerun columns", "Sources", "Open items"),
  text = c("One row per gap-analysis taxon: which result it carries, the run it comes from, the published value beside it, and what could still change.",
           "No taxonomic record change from the review. Published values stand (run08282025_1k, with the published table taking precedence).",
           "Records changed with the review (closed 2026-10-19, every row decided) and the result comes from the run in sourceRun. A taxon was re-run only when its record fingerprint changed or a state range list was added (rerunGroups_20261019.csv); otherwise the earlier run whose records are identical to the final dataset stands.",
           "run10192026_1k: 19-20 Oct 2026 on model_data20261019_2303.csv (17 taxa). run10022026_1k: 2 Oct 2026 (acerifolia, shuttleworthii). run09162026_1k: 16 Sept draft (6 taxa with identical records). run08282025_1k: published.",
           "pub_* columns are the published table (gapAnalysisSummary-sheet6.csv); *_vs_published are current minus published.",
           "reviewRows / agree / disagree / settledByProject / open: the reviewer's decisions on this taxon's record changes (taxonChangeRawData_20260924v2.xlsx). settledByProject = rows she marked Unclear or Review, settled by project decisions on 2026-10-19 (names in settledNames). nonTaxonomic = record differences outside the review.",
           "rerunGroup A: records changed; B: state range list added (stateListSource says where the list came from); untouched: fingerprint identical to the source run.",
           paste0("Combined table gapAnalysisSummary_combined_", d, ".csv from aggregateRuns(); review workbook taxonChangeRawData_20260924v2.xlsx; decisions reviewDecisions_20261019.md; methods notes corrigendumNotes.md."),
           "Two questions are with the reviewer (forAnne_message_20261019.md): whether V. obovata Raf. belongs in labrusca (no coordinates, no score effect) and whether the NatureServe state lists for simpsonii and baileyana agree with Weakley. Neither blocks the table."))
wb <- createWorkbook()
addTab <- function(n, x) { addWorksheet(wb, n); writeData(wb, n, x, headerStyle = createStyle(textDecoration = "bold")); freezePane(wb, n, firstRow = TRUE, firstCol = TRUE); setColWidths(wb, n, 1:ncol(x), "auto") }
addTab("Read me", readme); addTab("Status", status)
addTab("Category changes", status |> filter(categoryChanged %in% TRUE | FCSex_cat != pub_FCSex_cat | FCSin_cat != pub_FCSin_cat) |>
         select(ID, sourceRun, FCSex, pub_FCSex, FCSex_cat, pub_FCSex_cat, FCSin, pub_FCSin, FCSin_cat, pub_FCSin_cat, FCSc, pub_FCSc, FCSc_cat, pub_FCSc_cat, whatCouldChange))
saveWorkbook(wb, sprintf("work2026/teamStatus/teamStatus_%s.xlsx", d), overwrite = TRUE)
write_csv(status, sprintf("work2026/teamStatus/teamStatus_%s.csv", d), na = "")
options(width = 230)
cat("\n== status counts\n"); print(count(status, status, sourceRun, rerunGroup))
cat("\n== per taxon\n"); print(status |> select(ID, sourceRun, records, pub_records, records_vs_published, FCSc, pub_FCSc, FCSc_vs_published, FCSc_cat, pub_FCSc_cat, categoryChanged) |> as.data.frame(), right = FALSE, row.names = FALSE)
cat("\n== any category change (combined or component)\n"); print(status |> filter(categoryChanged %in% TRUE | FCSex_cat != pub_FCSex_cat | FCSin_cat != pub_FCSin_cat) |>
  select(ID, FCSex, pub_FCSex, FCSex_cat, pub_FCSex_cat, FCSin, pub_FCSin, FCSin_cat, pub_FCSin_cat, FCSc, pub_FCSc, FCSc_cat, pub_FCSc_cat) |> as.data.frame(), right = FALSE, row.names = FALSE)
