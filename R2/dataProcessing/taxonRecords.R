# Records behind each taxon's model, and a fingerprint of them.
#
# taxonRecords()     : the records the model driver uses for taxon j (the
#                      species-level concepts of cinerea and aestivalis pool
#                      their varieties; one known bad shuttleworthii point is
#                      dropped). The driver and the rerun check both call this,
#                      so they always see the same record set.
# taxonFingerprint() : record count, georeferenced count, G count and an md5
#                      of the sorted records. Two inputs give the same
#                      fingerprint for a taxon only if its records are identical.
# writeFingerprint() : stores the fingerprint in a run folder
#                      (occurances/inputFingerprint.csv); the driver calls it
#                      when a taxon finishes.
# taxaNeedingRerun() : compares the fingerprint stored with each taxon's
#                      current run against the current model input and lists
#                      the taxa whose records no longer match.

taxonRecords <- function(speciesData, j) {
  pooled <- list(
    "Vitis cinerea" = c("Vitis cinerea", "Vitis cinerea var. cinerea", "Vitis cinerea var. tomentosa"),
    "Vitis aestivalis" = c("Vitis aestivalis", "Vitis aestivalis var. aestivalis", "Vitis aestivalis var. bicolor"))
  if (j %in% names(pooled)) {
    sd1 <- speciesData |>
      dplyr::filter(taxon %in% pooled[[j]]) |>
      dplyr::mutate(taxon = j)
  } else {
    sd1 <- speciesData |> dplyr::filter(taxon == j)
  }
  if (j == "Vitis shuttleworthii") {
    # keep records without coordinates: a bare != drops NA rows
    lon <- suppressWarnings(as.numeric(sd1$longitude))
    sd1 <- sd1[is.na(lon) | lon != -80.001483, ]
  }
  sd1
}

taxonFingerprint <- function(sd1, j, inputFile) {
  # row numbers (index, recordID) change whenever any other taxon's records
  # change, so they are left out; every other column counts
  rec <- sd1 |>
    dplyr::select(-dplyr::any_of(c("index", "recordID", "...1"))) |>
    dplyr::mutate(dplyr::across(dplyr::everything(), as.character))
  key <- if (nrow(rec) == 0) "" else sort(do.call(paste, c(rec, sep = "|")))
  tmp <- tempfile(); writeLines(key, tmp)
  md5 <- unname(tools::md5sum(tmp)); unlink(tmp)
  lat <- suppressWarnings(as.numeric(sd1$latitude)); lon <- suppressWarnings(as.numeric(sd1$longitude))
  data.frame(taxon = j, inputFile = basename(inputFile),
             records = nrow(sd1), withCoordinates = sum(!is.na(lat) & !is.na(lon)),
             G = sum(sd1$type == "G", na.rm = TRUE), md5 = md5)
}

writeFingerprint <- function(fp, runFolder, note = NA) {
  dir.create(file.path(runFolder, "occurances"), recursive = TRUE, showWarnings = FALSE)
  fp$written <- format(Sys.time(), "%Y-%m-%dT%H:%M:%S"); fp$note <- note
  readr::write_csv(fp, file.path(runFolder, "occurances", "inputFingerprint.csv"))
}

readFingerprint <- function(runFolder) {
  f <- file.path(runFolder, "occurances", "inputFingerprint.csv")
  if (!file.exists(f)) return(NULL)
  readr::read_csv(f, col_types = readr::cols(.default = "c"), show_col_types = FALSE)
}

# The fingerprint is taken from the input read as text: read_csv's type
# guessing can differ between two builds (and turns values it cannot parse
# into NA), which would change a fingerprint without any record changing.
readModelDataText <- function(path) {
  readr::read_csv(path, col_types = readr::cols(.default = "c"), show_col_types = FALSE)
}

# latest dated model input: data/processed_occurrence/model_dataYYYYMMDD[_HHMM].csv
latestModelData <- function(dir = "data/processed_occurrence") {
  f <- list.files(dir, pattern = "^model_data\\d{8}(_\\d{4})?\\.csv$", full.names = TRUE)
  if (length(f) == 0) stop("No dated model input in ", dir, ": run preprocessing first.")
  # order on the date and time in the name, not on the file name: in most
  # locales "model_data20261002.csv" sorts after "model_data20261002_1530.csv"
  stamp <- sub("^model_data(\\d{8})_?(\\d{4})?\\.csv$", "\\1\\2", basename(f))
  stamp <- ifelse(nchar(stamp) == 8, paste0(stamp, "0000"), stamp)
  f[order(stamp, decreasing = TRUE)][1]
}

# Compare each taxon's current run with the current model input.
# The run a taxon is compared with follows its taxonomy-update status
# (taxonomyUpdateStatus, R2/summarize/aggregateRuns.R):
#   updated   -> the update run
#   unchanged -> the published run
#   pending   -> the round-2 draft run (run09162026_1k), which every taxon has
taxaNeedingRerun <- function(modelDataPath = latestModelData(),
                             publishedRun = "run08282025_1k",
                             updateRun = "run10022026_1k",
                             draftRun = "run09162026_1k",
                             status = taxonomyUpdateStatus,
                             outPath = NULL) {
  speciesData <- readModelDataText(modelDataPath)
  gapSpecies <- readr::read_csv("data/New World Vitis.csv", col_types = readr::cols(.default = "c"), show_col_types = FALSE) |>
    dplyr::filter(`Include in gap analysis?` == "Y") |> dplyr::pull(`Scientific Name`) |> sort()
  out <- purrr::map_dfr(gapSpecies, function(j) {
    st <- dplyr::case_when(j %in% status$updated ~ "updated", j %in% status$unchanged ~ "unchanged", TRUE ~ "pending")
    run <- switch(st, updated = updateRun, unchanged = publishedRun, pending = draftRun)
    now <- taxonFingerprint(taxonRecords(speciesData, j), j, modelDataPath)
    was <- readFingerprint(file.path("data/Vitis", j, run))
    if (is.null(was)) was <- data.frame(inputFile = NA, records = NA, withCoordinates = NA, G = NA, md5 = NA)
    data.frame(taxon = j, status = st, comparedRun = run, runInput = was$inputFile,
               records_run = as.integer(was$records), records_now = now$records,
               coords_run = as.integer(was$withCoordinates), coords_now = now$withCoordinates,
               G_run = as.integer(was$G), G_now = now$G,
               recordsChanged = ifelse(is.na(was$md5), NA, was$md5 != now$md5),
               currentInput = basename(modelDataPath))
  }) |>
    dplyr::mutate(action = dplyr::case_when(
      is.na(recordsChanged) ~ "no fingerprint for the compared run",
      recordsChanged & status == "pending" ~ "records changed; rerun once review is complete",
      recordsChanged ~ "records changed: rerun",
      TRUE ~ "no change"))
  if (!is.null(outPath)) readr::write_csv(out, outPath)
  out
}
