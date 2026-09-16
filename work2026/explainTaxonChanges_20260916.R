# work2026/explainTaxonChanges_20260916.R
# For every taxon whose record count changed between model_data20251216.csv and
# model_data20260820_taxonomyOnly.csv, trace each record (databaseSource +
# sourceUniqueID) and tabulate the flows: which concept it sat in before, which
# it sits in now (or "not in file"), and the recorded name that drove the move.
# Outputs work2026/taxonChangeFlows_20260916.csv (one row per
# taxon-direction-fromTo-recordedName) and prints a per-taxon summary.
suppressPackageStartupMessages({library(dplyr); library(readr); library(stringr); library(tidyr)})
options(width = 220)
# record key: source + source ID when the ID exists; otherwise a composite of
# the fields that identify a record without an ID (name, coordinates, locality,
# year, institution). Row position is NOT used: it differs between runs.
rd <- function(p, run) read_csv(p, col_types = cols(.default = "c"), progress = FALSE) |>
  mutate(run = run,
         key = paste(databaseSource,
                     coalesce(sourceUniqueID,
                              paste(originalTaxon, latitude, longitude, localityInformation, yearRecorded, institutionCode, observerName, sep = "~")),
                     sep = "|"),
         hasCoords = !is.na(latitude) & !is.na(longitude),
         name = str_squish(originalTaxon))
old <- rd("data/processed_occurrence/model_data20251216.csv", "dec")
new <- rd("data/processed_occurrence/model_data20260820_taxonomyOnly.csv", "sep")

# one row per record key per run (a key can legitimately sit in two concepts,
# e.g. a variety modeled directly and folded into its species; keep taxon in key)
o <- old |> distinct(key, taxon, .keep_all = TRUE) |> select(key, taxon_dec = taxon, name_dec = name, coords_dec = hasCoords, type_dec = type, src = databaseSource)
n <- new |> distinct(key, taxon, .keep_all = TRUE) |> select(key, taxon_sep = taxon, name_sep = name, coords_sep = hasCoords, type_sep = type, src2 = databaseSource)

# records lost by a taxon: in dec under taxon X, not in sep under taxon X -> where now?
lost <- o |> anti_join(n, by = c("key", "taxon_dec" = "taxon_sep")) |>
  left_join(n |> select(key, taxon_now = taxon_sep, name_sep), by = "key", multiple = "first") |>
  mutate(taxon_now = coalesce(taxon_now, "not in file"),
         direction = "lost", taxon = taxon_dec, from = taxon_dec, to = taxon_now,
         recordedName = coalesce(name_dec, name_sep), coords = coords_dec, type = type_dec)
# records gained by a taxon: in sep under taxon Y, not in dec under Y -> where before?
gained <- n |> anti_join(o, by = c("key", "taxon_sep" = "taxon_dec")) |>
  left_join(o |> select(key, taxon_before = taxon_dec, name_dec), by = "key", multiple = "first") |>
  mutate(taxon_before = coalesce(taxon_before, "not in file"),
         direction = "gained", taxon = taxon_sep, from = taxon_before, to = taxon_sep,
         recordedName = coalesce(name_sep, name_dec), coords = coords_sep, type = type_sep, src = src2)

flows <- bind_rows(lost, gained) |>
  mutate(recordedName = str_remove(recordedName, "\\s+(L\\.|Michx\\.|Planch\\.|Small|Buckley|Munson|Engelm\\.|Bailey|Roth|Thunb\\.|Scop\\.|Simpson|Rogers|Fernald|Regel|Moore|Berl\\.|Ashe|Rydb\\.|Deam|Rehder|Comeaux)\\b.*$")) |>
  count(taxon, direction, from, to, recordedName, src, name = "records") |>
  left_join(bind_rows(lost, gained) |> group_by(taxon, direction, from, to, src) |> summarise(withCoords = sum(coords, na.rm = TRUE), G = sum(type == "G", na.rm = TRUE), .groups = "drop"),
            by = c("taxon", "direction", "from", "to", "src")) |>
  arrange(taxon, direction, desc(records))
write_csv(flows, "work2026/taxonChangeFlows_20260916.csv")

# per-taxon summary
sumTab <- flows |> group_by(taxon, direction, from, to) |>
  summarise(records = sum(records), names = paste0(head(unique(recordedName), 4), collapse = "; "), .groups = "drop") |>
  arrange(taxon, direction, desc(records))
write_csv(sumTab, "work2026/taxonChangeSummary_20260916.csv")
# taxa whose net count changed
net <- bind_rows(old |> count(taxon, name = "dec"), new |> count(taxon, name = "sep")) |>
  group_by(taxon) |> summarise(dec = sum(dec, na.rm = TRUE), sep = sum(sep, na.rm = TRUE)) |> mutate(net = sep - dec)
cat("taxa with net change:", sum(net$net != 0), "\n")
cat("flows in taxa with NO net change (should be 0 rows):\n")
print(as.data.frame(sumTab |> semi_join(net |> filter(net == 0), by = "taxon")))
cat("\n=== per-taxon flows (taxa with net change) ===\n")
print(as.data.frame(sumTab |> semi_join(net |> filter(net != 0), by = "taxon") |> mutate(names = substr(names, 1, 110))))
