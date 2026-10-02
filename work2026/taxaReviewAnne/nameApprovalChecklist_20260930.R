# work2026/taxaReviewAnne/nameApprovalChecklist_20260930.R
# Name-level approval checklist for the taxonomy review with Anne, built from
# work2026/taxonChanges_published_vs_round2_detail_20260919.csv (one row per species x change x recorded name).
# Anne's record-level decisions (taxonChangeRawData_20260924.xlsx, records tab) are rolled up per name and
# pre-fill the Approve columns only where every record under that name got the same decision.
#   Section 1: every name removed from a species, with the species it moved to (if any).
#   Section 2: names added to a species that did not come from another species' removals.
# Non-taxonomy changes (countryExempt, dec16Driver, other) are listed on their own tab.
# Output: work2026/taxaReviewAnne/nameApprovalChecklist_20260930.xlsx
suppressPackageStartupMessages({library(dplyr); library(readr); library(readxl); library(stringr); library(openxlsx)})

dir <- "work2026/taxaReviewAnne"
det <- read_csv("work2026/taxonChanges_published_vs_round2_detail_20260919.csv", show_col_types = FALSE)
af  <- read_excel(file.path(dir, "taxonChangeRawData_20260924.xlsx"), "records")

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

addWorksheet(wb, "Not taxonomy")
writeData(wb, "Not taxonomy", other, headerStyle = hdr)
setColWidths(wb, "Not taxonomy", 1:6, c(24, 10, 34, 14, 9, 70)); addStyle(wb, "Not taxonomy", wrap, 2:(nrow(other) + 1), 1:6, gridExpand = TRUE)

addWorksheet(wb, "Read me")
writeData(wb, "Read me", data.frame(Notes = c(
  "One row per recorded name, not per record. Source: work2026/taxonChanges_published_vs_round2_detail_20260919.csv (taxonomy changes only).",
  "Section 1: each name removed from a species. 'Added to' is the species the same records now belong to, or 'none (record dropped)' when the name matches no project concept.",
  "Section 2: names added to a species that were not in the published dataset under any species.",
  "Anne (records): Anne's record-by-record decisions from taxonChangeRawData_20260924.xlsx rolled up per name (decision and record count).",
  "Approve columns are pre-filled only where every record under the name got the same decision (Agree = Y, Disagree = N). Yellow = still open.",
  "A removal whose records moved to another species is marked Y when Anne agreed with every record of the addition ('resolved by agreed addition').",
  "'Vitis L.' is a genus-only interpreted name; the question is really about the verbatim names shown next to it.",
  "'Not taxonomy' tab: changes caused by country filters, driver one-offs or other non-name reasons; listed for completeness, no synonymy decision needed.")))
setColWidths(wb, "Read me", 1, 140)

out <- file.path(dir, "nameApprovalChecklist_20260930.xlsx")
saveWorkbook(wb, out, overwrite = TRUE)
cat("Section 1:", nrow(s1), "names;", "Section 2:", nrow(s2), "names; not taxonomy:", nrow(other), "\n")
print(count(s1, `Approve removal (Y/N)`, `Approve addition (Y/N)`)); print(count(s2, `Approve addition (Y/N)`))
cat("Written:", out, "\n")
