# Repackage the rebuilt publication datasets (data/datasetsForPublication_<build>/, built by
# compileFigureData.R) into the two things the co-authors need:
#
#   data/SI_update_<stamp>/        one xlsx per supplementary page (sheets 1-12), single sheet each, so a
#                                  page can be dropped into the Google Sheet that holds the SI; plus a
#                                  README listing, per page, whether it changed and why (written by hand,
#                                  tracked copy in work2026/figureData/).
#   data/figureMaterial_<stamp>/   the all-taxa layers and tables the figures are drawn from (richness and
#                                  gap rasters with their species lists, protected-area tables, box-plot
#                                  data, the two rendered summary documents).
#
#   Rscript work2026/figureData/packagePublicationUpdate.R [stamp] [build]
#     stamp: date of this package (default today); build: date of the datasetsForPublication folder it
#     re-cuts (default stamp). Packages so far: 20261021 from build 20261020; 20261029 from build 20261029;
#     20261115 from build 20261115; 20261118 from build 20261115 (12-page numbering).
suppressPackageStartupMessages({library(dplyr); library(readr); library(openxlsx)})
args <- commandArgs(trailingOnly = TRUE)
stamp <- if (length(args) >= 1 && nzchar(args[1])) args[1] else format(Sys.Date(), "%Y%m%d")
build <- if (length(args) >= 2 && nzchar(args[2])) args[2] else stamp
src <- paste0("data/datasetsForPublication_", build)
si <- paste0("data/SI_update_", stamp); fig <- paste0("data/figureMaterial_", stamp)
cat("package", stamp, "from", src, "->", si, "and", fig, "\n")
dir.create(si, showWarnings = FALSE); dir.create(fig, showWarnings = FALSE)

# published page -> source csv in the rebuilt folder
# Numbering follows the SI workbook the co-authors hold (12 pages), inferred from the page numbers a
# co-author quoted on 2026-11-18: page 6 is the robustness-rule table (ATAUC, STAUC, ASD15) and the buffer-size comparison is
# page 12. Pages 9 and 10 are the all-polygon "Update" tables the published script built last
# (66,535 WDPA polygons; the published page 10 reaches a richness of 10, which only that table does).
# Packages before 18 Nov 2026 used an 11-page numbering with the compact 2,879- and 7,900-row tables as
# pages 8 and 9; those tables are now kept as helper files. Page 12 is the five-size (1, 5, 20, 50,
# 100 km) x two-projection table of published/variableBufferAnalysis.R, rebuilt on the aggregate run
# (variableBufferAnalysis_2026.R); packages before 18 Nov shipped the older three-size table
# (multipleBufferSizes-sheet11.csv), which is kept as a helper file.
pages <- tribble(
  ~page, ~sheetName,                ~file,
  1, "allSpeciesOccurrences",      "allSpeciesOccurrences-sheet1.csv",
  2, "modelVariables",             "modelVaraible-sheet2.csv",
  3, "modelData",                  "modelData-sheet3.csv",
  4, "variableSelections",         "variableSelections-sheet4.csv",
  5, "modelEvaluations",           "modelEvaluations-sheet5.csv",
  6, "modelEvaluations2",          "SDM_testConditions.csv",
  7, "gapAnalysisSummary",         "gapAnalysisSummary-sheet6.csv",
  8, "exsituCollectionSummary",    "exsituCollectionSummary-sheet7.csv",
  9, "pointsInProtectedAreas",     "pointsInProtectedAreasUpdate.csv",
  10, "predictedInProtectedAreas", "rastInProtectedAreasUpdate.csv",
  11, "optimalProtectedAreas",     "optimalProtectedAreas-sheet10.csv",
  12, "multipleBufferSizes",       "variable_buffer_raw_results.csv")
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
# helper tables: status and source run per taxon (explains page 7), the compact occurrence-points-per-area
# table the run folder writes (shipped as page 8 before 18 Nov 2026), and the older three-size buffer table.
# The compact "predicted richness per area" table (7,900 rows) is not shipped any more: it covers only one of
# the three WDPA download parts (wdpa_1.gpkg) and was mistaken for page 10 once; it stays in the build folder.
for (f in c("gapAnalysisSummary-sources.csv", "pointsInProtectedAreas-sheet8.csv", "multipleBufferSizes-sheet11.csv")) file.copy(file.path(src, f), si, overwrite = TRUE)

# figure material
figFiles <- c("speciesrichness_1km.tif", "speciesUsed_speciesrichness.csv",
              "ga50_speciesrichness_1km.tif", "ga50speciesUsed_speciesrichness.csv",
              "ersexRichness.tif", "ersex_speciesUsed_Richness.csv",
              "ersinRichness.tif", "ersin_speciesUsed_Richness.csv",
              "grsEX_map.tif", "grsIn_map.tif",
              "pointsInProtectedAreasUpdate.csv", "rastInProtectedAreasUpdate.csv",
              "environementalDataForBoxPlots.csv", "aggregateSources.csv",
              "runAggregate2026_1k_Summary.html", "runAggregate2026_1k_boxPlotSummary.html")
for (f in figFiles) { stopifnot(file.exists(file.path(src, f))); file.copy(file.path(src, f), fig, overwrite = TRUE, copy.date = TRUE) }
unlink(file.path(fig, "ersin_speciesUsed_Richness.tif"))   # misnamed copy of the csv in packages before 30 Oct 2026
# the build README describes every file of the figure material as well
file.copy(file.path(src, "README.md"), file.path(fig, "README.md"), overwrite = TRUE)
cat("figure material:", length(list.files(fig)), "files\n")
