# Repackage the rebuilt publication datasets (data/datasetsForPublication_20261020/, built by
# compileFigureData_20261020.R) into the two things the co-authors need:
#
#   data/SI_update_20261021/        one xlsx per supplementary page (sheets 1-11), single sheet each, so a
#                                   page can be dropped into the Google Sheet that holds the SI; plus a
#                                   README listing, per page, whether it changed and why.
#   data/figureMaterial_20261021/   the all-taxa layers and tables the figures are drawn from (richness and
#                                   gap rasters with their species lists, protected-area tables, box-plot
#                                   data, the two rendered summary documents).
#
#   Rscript work2026/figureData/packagePublicationUpdate_20261021.R
suppressPackageStartupMessages({library(dplyr); library(readr); library(openxlsx)})
src <- "data/datasetsForPublication_20261020"
si <- "data/SI_update_20261021"; fig <- "data/figureMaterial_20261021"
dir.create(si, showWarnings = FALSE); dir.create(fig, showWarnings = FALSE)

# published page -> source csv in the rebuilt folder
pages <- tribble(
  ~page, ~sheetName,                ~file,
  1, "allSpeciesOccurrences",      "allSpeciesOccurrences-sheet1.csv",
  2, "modelVariables",             "modelVaraible-sheet2.csv",
  3, "modelData",                  "modelData-sheet3.csv",
  4, "variableSelections",         "variableSelections-sheet4.csv",
  5, "modelEvaluations",           "modelEvaluations-sheet5.csv",
  6, "gapAnalysisSummary",         "gapAnalysisSummary-sheet6.csv",
  7, "exsituCollectionSummary",    "exsituCollectionSummary-sheet7.csv",
  8, "pointsInProtectedAreas",     "pointsInProtectedAreas-sheet8.csv",
  9, "predictedInProtectedAreas",  "predictedInProtectedAreas-sheet9.csv",
  10, "optimalProtectedAreas",     "optimalProtectedAreas-sheet10.csv",
  11, "multipleBufferSizes",       "multipleBufferSizes-sheet11.csv")
for (i in seq_len(nrow(pages))) {
  d <- read_csv(file.path(src, pages$file[i]), col_types = cols(.default = "c"), show_col_types = FALSE, progress = FALSE)
  # a few occurrence strings carry bytes that are not UTF-8 (source databases); the xlsx writer refuses them
  d <- d |> mutate(across(everything(), ~ stringi::stri_enc_toutf8(.x, validate = TRUE)))
  # numbers back to numbers so the sheet sorts and plots; text stays text
  d <- d |> mutate(across(everything(), ~ if (all(is.na(.x) | !is.na(suppressWarnings(as.numeric(.x))))) suppressWarnings(as.numeric(.x)) else .x))
  out <- file.path(si, sprintf("sheet%02d_%s.xlsx", pages$page[i], pages$sheetName[i]))
  wb <- createWorkbook(); addWorksheet(wb, pages$sheetName[i]); writeData(wb, 1, d); freezePane(wb, 1, firstRow = TRUE)
  saveWorkbook(wb, out, overwrite = TRUE)
  cat(sprintf("  sheet %2d  %-26s %7d rows x %2d cols -> %s\n", pages$page[i], pages$sheetName[i], nrow(d), ncol(d), basename(out)))
}
# the two helper tables that explain sheet 6
for (f in c("gapAnalysisSummary-sources.csv", "SDM_testConditions.csv")) file.copy(file.path(src, f), si, overwrite = TRUE)

# figure material
figFiles <- c("speciesrichness_1km.tif", "speciesUsed_speciesrichness.csv",
              "ga50_speciesrichness_1km.tif", "ga50speciesUsed_speciesrichness.csv",
              "ersexRichness.tif", "ersex_speciesUsed_Richness.csv",
              "ersinRichness.tif", "ersin_speciesUsed_Richness.tif",
              "grsEX_map.tif", "grsIn_map.tif",
              "pointsInProtectedAreasUpdate.csv", "rastInProtectedAreasUpdate.csv",
              "environementalDataForBoxPlots.csv", "aggregateSources.csv",
              "runAggregate2026_1k_Summary.html", "runAggregate2026_1k_boxPlotSummary.html")
for (f in figFiles) { stopifnot(file.exists(file.path(src, f))); file.copy(file.path(src, f), fig, overwrite = TRUE, copy.date = TRUE) }
# the species list with the .tif name is a csv (published quirk): give the copy the right extension as well
file.copy(file.path(src, "ersin_speciesUsed_Richness.tif"), file.path(fig, "ersin_speciesUsed_Richness.csv"), overwrite = TRUE)
cat("figure material:", length(list.files(fig)), "files\n")
