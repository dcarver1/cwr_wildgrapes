# work2026/taxaReviewAnne/buildNameApprovalChecklist.R
# Reusable version of nameApprovalChecklist_20260930.R: turns a review workbook from Anne into the
# name-level approval checklist plus the list of edits to make in the Google Sheet crosswalk.
#   Rscript work2026/taxaReviewAnne/buildNameApprovalChecklist.R [Anne's workbook] [output date YYYYMMDD]
# Inputs
#   * work2026/taxonChanges_published_vs_round2_detail_20260919.csv: published vs round-2 changes, one row per
#     species x change x recorded name. Fixed baseline, so the checklist stays a cumulative record.
#   * Anne's workbook (default: newest taxonChangeRawData_*.xlsx here), "records" tab, columns "AF decision"
#     and "AF Comment". Decisions are rolled up per name and pre-fill the Approve columns only where every
#     record under that name got the same decision.
#   * newest crosswalk snapshot in taxonomy/snapshots (preprocessing/pullTaxonomySheet.R)
# Tabs
#   Checklist    : Section 1, every name removed from a species, with the species it moved to (if any);
#                  Section 2, names added to a species that did not come from another species' removals.
#   Sheet edits  : one row per name to add to an include or exclude cell, with the cell's current value and
#                  whether the edit is already in the sheet (re-run after editing the sheet to confirm).
#   Code changes : decisions the crosswalk cannot express (hybrid formulas kept by the name parser).
#   Not taxonomy : country filters, driver one-offs; handled by the processing workflow, listed for completeness.
# Output: work2026/taxaReviewAnne/nameApprovalChecklist_<date>.xlsx
suppressPackageStartupMessages({library(dplyr); library(readr); library(readxl); library(stringr); library(openxlsx)})

dir <- "work2026/taxaReviewAnne"
args <- commandArgs(trailingOnly = TRUE)
afFile  <- if (length(args) >= 1) args[1] else tail(sort(list.files(dir, "^taxonChangeRawData_.*\\.xlsx$", full.names = TRUE)), 1)
outDate <- if (length(args) >= 2) args[2] else format(Sys.Date(), "%Y%m%d")
snapFile <- tail(sort(list.files("taxonomy/snapshots", "^NewWorldVitis_\\d{8}\\.csv$", full.names = TRUE)), 1)
if (length(snapFile) == 0) stop("No crosswalk snapshot: run preprocessing/pullTaxonomySheet.R first.")
cat("Review workbook:", afFile, "\nCrosswalk snapshot:", snapFile, "\n")

# a sheet edit normally goes to the concept the name was removed from; names that belong to a different
# concept are redirected here (name without authorship = concept)
targetOverride <- c("Vitis helleri" = "Vitis berlandieri", "Vitis cordifolia var. helleri" = "Vitis berlandieri")
acceptedHybrids <- c("Vitis x champinii", "Vitis x doaniana", "Vitis x novae-angliae")

det <- read_csv("work2026/taxonChanges_published_vs_round2_detail_20260919.csv", show_col_types = FALSE)
af  <- read_excel(afFile, "records")

# --- intake checks on Anne's workbook ---------------------------------------------------------------
need <- c("species", "AF decision", "AF Comment", "change", "counterpart", "reason", "projectRecordedName")
if (length(setdiff(need, names(af))) > 0) stop("Review workbook is missing columns: ", paste(setdiff(need, names(af)), collapse = ", "))
bad <- unique(af$`AF decision`[!is.na(af$`AF decision`) & !str_detect(af$`AF decision`, "^(Agree|Disagree|Unclear|Review)")])
if (length(bad) > 0) stop("Decisions must start with Agree, Disagree, Unclear or Review. Found: ", paste(bad, collapse = " | "))
noMatch <- anti_join(distinct(af, species, change, projectRecordedName),
                     distinct(det, species, change, projectRecordedName = recordedName), by = c("species", "change", "projectRecordedName"))
if (nrow(noMatch) > 0) warning(nrow(noMatch), " species/change/name combinations in the review workbook are not in the baseline detail file.")

# --- Anne's decisions rolled up per name ------------------------------------------------------------
lbl <- function(x) case_when(is.na(x) ~ "not reviewed", str_detect(x, "^Agree") ~ "Agree", str_detect(x, "^Disagree") ~ "Disagree",
                             str_detect(x, "^Unclear") ~ "Unclear", str_detect(x, "^Review") ~ "Review", TRUE ~ x)
afName <- af |> mutate(dec = lbl(`AF decision`)) |>
  group_by(species, change, reason, name = projectRecordedName, dec) |>
  summarise(n = n(), comment = paste(unique(na.omit(`AF Comment`)), collapse = " | "), .groups = "drop") |>
  group_by(species, change, reason, name) |>
  summarise(afSummary = paste0(dec, " ", n, collapse = "; "),
            afApprove = if (n() == 1) c(Agree = "Y", Disagree = "N")[dec[1]] else NA_character_,
            afComment = paste(unique(comment[comment != ""]), collapse = " | "), .groups = "drop") |>
  mutate(afApprove = unname(afApprove))

# the detail file splits a name by data source; one row per name here
tax <- det |> filter(taxonomyChange == "Yes") |>
  group_by(species, change, recordedName, counterpart, reason) |>
  summarise(records = sum(records), verbatimNames = paste(unique(na.omit(verbatimNames)), collapse = "; "),
            note = paste(unique(na.omit(note)), collapse = " "), .groups = "drop") |>
  mutate(across(c(verbatimNames, note), ~na_if(.x, ""))) |>
  left_join(afName, by = c("species", "change", "reason", "recordedName" = "name"))
moved <- function(cp) !str_detect(cp, "^no project concept|^not in the published")

# --- Section 1: removed names -----------------------------------------------------------------------
adds <- tax |> filter(change == "Added") |>
  select(addedTo = species, recordedName, fromSpecies = counterpart, addAnne = afSummary, addApprove = afApprove, addComment = afComment)
s1 <- tax |> filter(change == "Removed") |>
  left_join(adds, by = c("counterpart" = "addedTo", "species" = "fromSpecies", "recordedName")) |>
  mutate(isMoved = moved(counterpart),
         addedTo = if_else(isMoved, counterpart, "none (record dropped)"),
         # an agreed addition resolves its paired removal
         removeApprove = coalesce(afApprove, if_else(isMoved & addApprove %in% "Y", "Y", NA_character_)),
         removeAnne = if_else(is.na(afSummary) | (afSummary == paste("not reviewed", records) & isMoved & addApprove %in% "Y"),
                              coalesce(if_else(isMoved & addApprove %in% "Y", "resolved by agreed addition", afSummary), "not reviewed"), afSummary),
         addAnne = if_else(isMoved, addAnne, "n/a"), addApprove = if_else(isMoved, addApprove, "n/a"),
         notes = str_squish(paste(coalesce(note, ""), if_else(coalesce(afComment, "") != "", paste("Anne:", afComment), ""),
                                  if_else(coalesce(addComment, "") != "", paste("Anne (addition):", addComment), "")))) |>
  group_by(species) |> mutate(tot = sum(records)) |> ungroup() |>
  arrange(desc(tot), species, desc(records)) |>
  transmute(`Original species` = species, `Name removed` = recordedName, Records = records,
            `Verbatim names in records` = verbatimNames, `Anne (records)` = removeAnne, `Approve removal (Y/N)` = removeApprove,
            `Added to` = addedTo, `Anne (records) ` = addAnne, `Approve addition (Y/N)` = addApprove, Notes = notes)

# --- Section 2: added names not from the removals ---------------------------------------------------
# additions with no matching removal row (e.g. argentifolia -> var. bicolor names aestivalis, which has no such removal) go here too
paired <- tax |> filter(change == "Removed", moved(counterpart)) |> select(species = counterpart, recordedName, counterpart = species)
s2 <- tax |> filter(change == "Added") |> anti_join(paired, by = c("species", "recordedName", "counterpart")) |>
  mutate(note = if_else(moved(counterpart), paste0("Listed as coming from ", counterpart, " but that species has no matching removal: check the join. ", coalesce(note, "")), note)) |>
  group_by(species) |> mutate(tot = sum(records)) |> ungroup() |> arrange(desc(tot), species, desc(records)) |>
  transmute(`Species` = species, `Name added` = recordedName, Records = records, `Verbatim names in records` = verbatimNames,
            `Anne (records)` = coalesce(afSummary, "not reviewed"), `Approve addition (Y/N)` = afApprove,
            Notes = str_squish(paste(coalesce(note, ""), if_else(coalesce(afComment, "") != "", paste("Anne:", afComment), ""))))

# every paired addition must have been picked up by a removal row
stopifnot(sum(tax$records[tax$change == "Added"]) == sum(s1$Records[s1$`Added to` != "none (record dropped)"]) + sum(s2$Records))

other <- det |> filter(taxonomyChange != "Yes") |>
  transmute(Species = species, Change = change, `Recorded name` = recordedName, Reason = reason, Records = records, Note = note)

# --- workbook ---------------------------------------------------------------------------------------
wb <- createWorkbook()
hdr <- createStyle(textDecoration = "bold", fgFill = "#DDE6F0", wrapText = TRUE, valign = "top", border = "bottom")
sec <- createStyle(textDecoration = "bold", fontSize = 13)
wrap <- createStyle(wrapText = TRUE, valign = "top")
fillY <- createStyle(fgFill = "#E2F0D9"); fillN <- createStyle(fgFill = "#F8D7DA"); fillBlank <- createStyle(fgFill = "#FFF2CC")

addWorksheet(wb, "Checklist")
writeData(wb, "Checklist", "Section 1. Names removed from a species", startRow = 1); addStyle(wb, "Checklist", sec, 1, 1)
writeData(wb, "Checklist", s1, startRow = 2, headerStyle = hdr)
r2 <- nrow(s1) + 5
writeData(wb, "Checklist", "Section 2. Names added that did not come from another species' removals", startRow = r2 - 1)
addStyle(wb, "Checklist", sec, r2 - 1, 1)
writeData(wb, "Checklist", s2, startRow = r2, headerStyle = hdr)
addStyle(wb, "Checklist", wrap, rows = 3:(r2 + nrow(s2)), cols = 1:10, gridExpand = TRUE, stack = TRUE)
shade <- function(rows, col, vals) {
  for (i in seq_along(vals)) addStyle(wb, "Checklist", switch(coalesce(vals[i], "blank"), Y = fillY, N = fillN, `n/a` = wrap, fillBlank),
                                      rows[i], col, stack = TRUE)
}
shade(2 + seq_len(nrow(s1)), 6, s1$`Approve removal (Y/N)`)
shade(2 + seq_len(nrow(s1)), 9, s1$`Approve addition (Y/N)`)
shade(r2 + seq_len(nrow(s2)), 6, s2$`Approve addition (Y/N)`)
setColWidths(wb, "Checklist", 1:10, c(24, 34, 9, 34, 22, 12, 24, 22, 12, 70))
freezePane(wb, "Checklist", firstActiveRow = 3)

# --- Sheet edits and code changes -------------------------------------------------------------------
incCol <- "Names to include in this concept (Homotypic synonyms)"; excCol <- "Names to exclude from this concept"
snap <- read_csv(snapFile, col_types = cols(.default = "c"), show_col_types = FALSE)
# same cell splitting as preprocessing/functions/speciesStandardization.R (splitNames)
splitCell <- function(x) {
  if (is.na(x) || str_trim(x) %in% c("", "NA", "n/a", "N/A")) return(character(0))
  out <- str_squish(str_replace(str_trim(unlist(str_split(x, "\\s*[,;]\\s*"))), "^V\\.\\s*", "Vitis "))
  out[out != ""]
}
# name as the sheet needs it: authorship removed, so author variants of one name collapse to one entry
sheetName <- function(x) str_match(str_squish(str_replace_all(x, "[×✕]\\s*", "x ")),
  "^((?:Vitis|Muscadinia)\\s+(?:x\\s+)?[a-z]+(?:-[a-z]+)?(?:\\s+(?:var\\.|subsp\\.|f\\.)\\s+[a-z]+(?:-[a-z]+)?)?)")[, 2]
isHybrid <- function(recorded, verbatim) {
  nm <- sheetName(recorded)
  formula <- str_detect(coalesce(verbatim, ""), "\\s[xX×✕]\\s") & !str_detect(coalesce(verbatim, ""), "champinii|doaniana|novae-angliae")
  (str_detect(recorded, "[×✕]|\\sx\\s") & !nm %in% acceptedHybrids) | (recorded == "Vitis L." & formula)
}

# names dropped from a concept (matched no concept): Anne disagreeing with the removal = add to the include cell
drop <- s1 |> filter(`Added to` == "none (record dropped)") |>
  transmute(concept = `Original species`, cell = "include", recorded = `Name removed`, records = as.numeric(Records),
            verbatim = `Verbatim names in records`, anne = `Anne (records)`,
            apply = c(Y = "N", N = "Y")[`Approve removal (Y/N)`])
# names newly entering a concept: Anne disagreeing with the addition = add to the exclude cell
# (hybrid formulas are listed whatever their review status: hybrids outside the accepted three are never included)
add <- s2 |> filter(`Approve addition (Y/N)` %in% "N" | isHybrid(`Name added`, `Verbatim names in records`)) |>
  transmute(concept = Species, cell = "exclude", recorded = `Name added`, records = as.numeric(Records),
            verbatim = `Verbatim names in records`, anne = `Anne (records)`, apply = "Y")
cand <- bind_rows(drop, add) |> mutate(apply = unname(apply), name = sheetName(recorded), hybrid = isHybrid(recorded, verbatim))

code <- cand |> filter(hybrid) |>
  transmute(Concept = concept, `Recorded name` = recorded, `Verbatim names in records` = verbatim, Records = records, `Anne (records)` = anne,
            Action = if_else(cell == "exclude", "Name parser keeps the first parent of a hybrid formula: reject hybrid formulas in parseVitisName().",
                             "No change: hybrid outside the three accepted hybrids, stays out of the gap analysis."))

edits <- cand |> filter(!hybrid, !is.na(name)) |>
  mutate(redirected = name %in% names(targetOverride), concept = coalesce(unname(targetOverride[name]), concept)) |>
  # a name dropped from both a species and its variety is entered once, under the variety
  group_by(name, cell) |> filter(!sapply(concept, function(p) any(str_starts(concept, fixed(paste0(p, " var. ")))))) |>
  group_by(concept, cell, name) |>
  summarise(records = sum(records), sourceNames = paste(unique(recorded), collapse = "; "),
            anne = paste(unique(anne), collapse = "; "),
            apply = if (n_distinct(apply, na.rm = FALSE) == 1) apply[1] else NA_character_,
            mixed = n_distinct(apply, na.rm = FALSE) > 1, redirected = any(redirected), .groups = "drop") |>
  left_join(select(snap, concept = `Scientific Name`, inc = all_of(incCol), exc = all_of(excCol)), by = "concept") |>
  mutate(current = if_else(cell == "include", inc, exc),
         inSheet = mapply(function(n, cur, con) n %in% c(splitCell(cur), if (identical(cell, "include")) con), name, current, concept),
         note = str_squish(paste(if_else(mixed, "Author variants of this name got different decisions: needs one name-level call.", ""),
                                 if_else(redirected, "Target concept set in targetOverride (the name was removed from a different concept).", "")))) |>
  arrange(desc(coalesce(apply, "") == "Y"), is.na(apply), concept, desc(records)) |>
  transmute(`Concept (Scientific Name row)` = concept, Cell = cell, `Name to add` = name, Records = records,
            `Apply (Y/N)` = apply, `Already in sheet` = if_else(inSheet, "Yes", "No"), `Anne (records)` = anne,
            `Current cell value` = coalesce(current, ""), `Recorded names covered` = sourceNames, Notes = note)

addWorksheet(wb, "Sheet edits")
writeData(wb, "Sheet edits", edits, headerStyle = hdr)
setColWidths(wb, "Sheet edits", 1:10, c(30, 9, 34, 9, 10, 10, 26, 60, 50, 50))
if (nrow(edits) > 0) {
  addStyle(wb, "Sheet edits", wrap, 2:(nrow(edits) + 1), 1:10, gridExpand = TRUE)
  for (i in seq_len(nrow(edits))) addStyle(wb, "Sheet edits", switch(coalesce(edits$`Apply (Y/N)`[i], "blank"), Y = fillY, N = fillN, fillBlank), i + 1, 5, stack = TRUE)
}
freezePane(wb, "Sheet edits", firstActiveRow = 2)
addWorksheet(wb, "Code changes")
writeData(wb, "Code changes", code, headerStyle = hdr)
setColWidths(wb, "Code changes", 1:6, c(24, 30, 50, 9, 22, 80))
if (nrow(code) > 0) addStyle(wb, "Code changes", wrap, 2:(nrow(code) + 1), 1:6, gridExpand = TRUE)

addWorksheet(wb, "Not taxonomy")
writeData(wb, "Not taxonomy", other, headerStyle = hdr)
setColWidths(wb, "Not taxonomy", 1:6, c(24, 10, 34, 14, 9, 70)); addStyle(wb, "Not taxonomy", wrap, 2:(nrow(other) + 1), 1:6, gridExpand = TRUE)

addWorksheet(wb, "Read me")
writeData(wb, "Read me", data.frame(Notes = c(
  "One row per recorded name, not per record. Source: work2026/taxonChanges_published_vs_round2_detail_20260919.csv (taxonomy changes only).",
  "Section 1: each name removed from a species. 'Added to' is the species the same records now belong to, or 'none (record dropped)' when the name matches no project concept.",
  "Section 2: names added to a species that were not in the published dataset under any species.",
  paste0("Anne (records): Anne's decisions from ", basename(afFile), " rolled up per name (decision and record count)."),
  paste0("'Sheet edits' tab: the edits to make in the Google Sheet crosswalk ('New World Vitis' tab), one row per name without authorship. ",
         "Apply = Y where Anne's decision is the same for every record of the name, N where she agreed the name stays out, blank where still open. ",
         "'Already in sheet' is checked against ", basename(snapFile), "; pull a new snapshot and rebuild after editing to confirm."),
  "A variety's synonym is entered on the variety row only; the model driver folds the varieties into the species.",
  "'Code changes' tab: hybrid names outside the three accepted hybrids. They stay out of the gap analysis; the ones the name parser let in need a parser fix, not a sheet edit.",
  "Approve columns are pre-filled only where every record under the name got the same decision (Agree = Y, Disagree = N). Yellow = still open.",
  "A removal whose records moved to another species is marked Y when Anne agreed with every record of the addition ('resolved by agreed addition').",
  "'Vitis L.' is a genus-only interpreted name; the question is really about the verbatim names shown next to it.",
  "'Not taxonomy' tab: changes caused by country filters, driver one-offs or other non-name reasons; listed for completeness, no synonymy decision needed.")))
setColWidths(wb, "Read me", 1, 140)

out <- file.path(dir, paste0("nameApprovalChecklist_", outDate, ".xlsx"))
cat("Sheet edits:", nrow(edits), "names;", sum(edits$`Apply (Y/N)` %in% "Y"), "approved,", sum(edits$`Apply (Y/N)` %in% "Y" & edits$`Already in sheet` == "No"), "of those not yet in the sheet\n")
saveWorkbook(wb, out, overwrite = TRUE)
cat("Section 1:", nrow(s1), "names;", "Section 2:", nrow(s2), "names; not taxonomy:", nrow(other), "\n")
print(count(s1, `Approve removal (Y/N)`, `Approve addition (Y/N)`)); print(count(s2, `Approve addition (Y/N)`))
cat("Written:", out, "\n")
