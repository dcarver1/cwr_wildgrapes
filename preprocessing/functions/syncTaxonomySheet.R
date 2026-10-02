# Keep the local taxonomy crosswalk in step with the Google Sheet.
#
# The "New World Vitis" tab of the taxon sheet is the single source for taxon
# concepts and synonyms; it is edited over time. The pipeline reads the local
# copy (data/New World Vitis.csv). syncTaxonomySheet() pulls the live tab,
# reports every cell that differs from the local copy and, when update = TRUE,
# replaces the local copy (the previous one is kept as a dated backup), writes
# a tracked snapshot under taxonomy/snapshots/ and appends the cell changes to
# taxonomy/snapshots/changeLog.csv.
#
#   update = TRUE  : preprocessing (the sheet edits reach the model input)
#   update = FALSE : check only (the model driver uses this to warn when the
#                    sheet has moved on since the model input was built)
#
# If the sheet cannot be reached (no token, no network, API quota) the local
# copy is used and a warning says so; set required = TRUE to stop instead.
# The sheet is not link-shared: run googledrive::drive_auth() once in an
# interactive session so a token is cached.
taxonomySheetID  <- "15j4wOIhl60Vcz1mCMlKBIQYwY-5oJ2Q21xwp6lNYrm4"
taxonomySheetTab <- "New World Vitis"

readLiveTaxonomySheet <- function() {
  if (interactive()) {
    googledrive::drive_auth(scopes = "https://www.googleapis.com/auth/drive.readonly")
  } else {
    tk <- list.files("~/.cache/gargle", full.names = TRUE)
    if (length(tk) == 0) stop("No cached Google token: run googledrive::drive_auth() in an interactive session first.")
    googledrive::drive_auth(token = readRDS(tk[which.max(file.mtime(tk))]))
  }
  googlesheets4::gs4_auth(token = googledrive::drive_token())
  new <- googlesheets4::read_sheet(googlesheets4::as_sheets_id(taxonomySheetID), sheet = taxonomySheetTab, col_types = "c")
  new[rowSums(!is.na(new)) > 0, ]
}

# cell-level differences between two versions of the sheet, keyed on Scientific Name
taxonomySheetChanges <- function(old, new) {
  key <- "Scientific Name"
  # an empty cell and a cell holding the text "NA" are the same thing to the pipeline
  blank <- function(x) { x[is.na(x) | x == "NA"] <- ""; x }
  out <- list()
  for (t in setdiff(new[[key]], old[[key]])) out[[length(out) + 1]] <- data.frame(taxon = t, column = "(row)", old = "", new = "row added")
  for (t in setdiff(old[[key]], new[[key]])) out[[length(out) + 1]] <- data.frame(taxon = t, column = "(row)", old = "row removed", new = "")
  for (cn in setdiff(names(new), names(old))) out[[length(out) + 1]] <- data.frame(taxon = "(all)", column = cn, old = "", new = "column added")
  for (cn in setdiff(names(old), names(new))) out[[length(out) + 1]] <- data.frame(taxon = "(all)", column = cn, old = "column removed", new = "")
  both <- intersect(old[[key]], new[[key]])
  o <- old[match(both, old[[key]]), ]; n <- new[match(both, new[[key]]), ]
  for (cn in setdiff(intersect(names(old), names(new)), key)) {
    d <- which(blank(o[[cn]]) != blank(n[[cn]]))
    if (length(d)) out[[length(out) + 1]] <- data.frame(taxon = both[d], column = cn, old = blank(o[[cn]])[d], new = blank(n[[cn]])[d])
  }
  if (length(out)) do.call(rbind, out) else data.frame(taxon = character(), column = character(), old = character(), new = character())
}

syncTaxonomySheet <- function(localPath = "data/New World Vitis.csv",
                              snapDir = "taxonomy/snapshots",
                              update = TRUE, required = FALSE) {
  new <- tryCatch(readLiveTaxonomySheet(), error = function(e) e)
  if (inherits(new, "error")) {
    msg <- paste0("Taxonomy sheet not reachable (", conditionMessage(new), "); using the local copy ", localPath)
    if (required) stop(msg) else warning(msg, call. = FALSE)
    return(invisible(list(reached = FALSE, changed = NA, changes = NULL)))
  }
  old <- readr::read_csv(localPath, col_types = readr::cols(.default = "c"), na = "", show_col_types = FALSE)
  changes <- taxonomySheetChanges(old, new)
  if (nrow(changes) == 0) {
    message("Taxonomy sheet: local copy is current (", nrow(new), " taxa).")
    return(invisible(list(reached = TRUE, changed = FALSE, changes = changes)))
  }
  message("Taxonomy sheet: ", nrow(changes), " cell(s) differ from the local copy:")
  for (i in seq_len(nrow(changes))) message("  ", changes$taxon[i], " | ", changes$column[i], " | '", changes$old[i], "' -> '", changes$new[i], "'")
  if (!update) return(invisible(list(reached = TRUE, changed = TRUE, changes = changes)))

  stamp <- format(Sys.time(), "%Y%m%d_%H%M")
  file.copy(localPath, sub("\\.csv$", paste0("_backup_", stamp, ".csv"), localPath))
  readr::write_csv(new, localPath, na = "")
  dir.create(snapDir, recursive = TRUE, showWarnings = FALSE)
  snap <- file.path(snapDir, paste0("NewWorldVitis_", stamp, ".csv"))
  readr::write_csv(new, snap, na = "")
  logPath <- file.path(snapDir, "changeLog.csv")
  readr::write_csv(cbind(pulled = stamp, changes), logPath, append = file.exists(logPath))
  message("Local copy updated; snapshot ", snap, "; changes appended to ", logPath)
  invisible(list(reached = TRUE, changed = TRUE, changes = changes))
}
