# preprocessing/pullTaxonomySheet.R
# Pull the taxonomy crosswalk ("New World Vitis" tab) from Google Drive and write a dated snapshot.
# The Google Sheet is the single source for taxon concepts and synonyms; a run reads a snapshot, never
# the live sheet, so a run can always be traced to the exact crosswalk it used.
#   Sheet   : "Conservation gap analysis for wild grapevines (Vitis L.) of the Americas - taxon sheet"
#   Output  : taxonomy/snapshots/NewWorldVitis_<YYYYMMDD>.csv (tracked in git; data/ is ignored)
#             taxonomy/snapshots/NewWorldVitis_<YYYYMMDD>_checks.csv (validation findings, if any)
# A snapshot is only written when the sheet differs from the latest snapshot.
# This script does not touch data/New World Vitis.csv.
suppressPackageStartupMessages({library(googledrive); library(googlesheets4); library(readr); library(dplyr); library(stringr)})

sheetID  <- "15j4wOIhl60Vcz1mCMlKBIQYwY-5oJ2Q21xwp6lNYrm4"
sheetTab <- "New World Vitis"
snapDir  <- "taxonomy/snapshots"
incCol <- "Names to include in this concept (Homotypic synonyms)"
excCol <- "Names to exclude from this concept"

# --- auth -------------------------------------------------------------------------------------------
# The sheet is not link-shared, so gs4_deauth() cannot read it. Interactive sessions use the normal
# login; Rscript cannot pick a cached token by email, so it loads the newest token file directly.
if (interactive()) {
  drive_auth(scopes = "https://www.googleapis.com/auth/drive.readonly")
} else {
  tk <- list.files("~/.cache/gargle", full.names = TRUE)
  if (length(tk) == 0) stop("No cached Google token: run googledrive::drive_auth() in an interactive session first.")
  drive_auth(token = readRDS(tk[which.max(file.mtime(tk))]))
}
gs4_auth(token = drive_token())

# --- pull -------------------------------------------------------------------------------------------
meta <- drive_get(as_id(sheetID))$drive_resource[[1]]
new <- read_sheet(as_id(sheetID), sheet = sheetTab, col_types = "c")
new <- new[rowSums(!is.na(new)) > 0, ]
cat("Sheet last modified", meta$modifiedTime, "by", meta$lastModifyingUser$displayName, "-", nrow(new), "rows\n")

# --- checks -----------------------------------------------------------------------------------------
# same cell splitting as speciesStandardization.R (splitNames)
splitCell <- function(x) {
  if (is.na(x) || str_trim(x) %in% c("", "NA", "n/a", "N/A")) return(character(0))
  out <- str_squish(str_replace(str_trim(unlist(str_split(x, "\\s*[,;]\\s*"))), "^V\\.\\s*", "Vitis "))
  out[out != ""]
}
issue <- function(taxon, check, detail) {
  if (length(taxon) == 0) return(NULL)
  data.frame(taxon = coalesce(taxon, "(blank)"), check = check, detail = detail)
}
checks <- list()
sci <- new$`Scientific Name`
checks[[1]] <- issue(sci[is.na(sci) | duplicated(sci)], "Scientific Name blank or duplicated", "the pipeline keys on this column")
d <- which(is.na(new$`Taxon Name`) | new$`Taxon Name` != sci)
checks[[2]] <- issue(sci[d], "Taxon Name differs from Scientific Name", coalesce(new$`Taxon Name`[d], "(blank)"))
for (cn in c(incCol, excCol)) {
  d <- which(str_detect(new[[cn]], ";|(^|[,;]\\s*)V\\.\\s"))
  checks[[length(checks) + 1]] <- issue(sci[d], "semicolon or abbreviated 'V.' in a name cell", new[[cn]][d])
}
inc <- lapply(new[[incCol]], splitCell); exc <- lapply(new[[excCol]], splitCell)
self <- which(mapply(function(n, s) s %in% n, inc, sci))
checks[[length(checks) + 1]] <- issue(sci[self], "concept lists itself as an include name", sci[self])
both <- which(mapply(function(a, b) length(intersect(a, b)) > 0, inc, exc))
checks[[length(checks) + 1]] <- issue(sci[both], "name in both the include and exclude cell", new[[incCol]][both])
# a name included by two concepts is expected only for a species and its own variety
long <- data.frame(taxon = rep(sci, lengths(inc)), name = unlist(inc))
dup <- long |> group_by(name) |> filter(n_distinct(taxon) > 1) |> summarise(taxa = paste(taxon, collapse = " + "), .groups = "drop")
checks[[length(checks) + 1]] <- issue(dup$name, "name on the include list of more than one concept", dup$taxa)
checks <- bind_rows(checks)

# --- write ------------------------------------------------------------------------------------------
dir.create(snapDir, recursive = TRUE, showWarnings = FALSE)
old <- sort(list.files(snapDir, pattern = "^NewWorldVitis_\\d{8}\\.csv$", full.names = TRUE))
same <- length(old) > 0 && isTRUE(all.equal(as.data.frame(read_csv(tail(old, 1), col_types = cols(.default = "c"), na = character(), show_col_types = FALSE)),
                                            as.data.frame(mutate(new, across(everything(), ~coalesce(.x, ""))))))
stamp <- format(Sys.Date(), "%Y%m%d")
if (same) {
  cat("No change since", basename(tail(old, 1)), "- no new snapshot written.\n")
} else {
  out <- file.path(snapDir, paste0("NewWorldVitis_", stamp, ".csv"))
  write_csv(new, out, na = "")
  cat("Snapshot written:", out, "\n")
}
write_csv(checks, file.path(snapDir, paste0("NewWorldVitis_", stamp, "_checks.csv")))
cat(nrow(checks), "check findings:\n"); print(count(checks, check))
