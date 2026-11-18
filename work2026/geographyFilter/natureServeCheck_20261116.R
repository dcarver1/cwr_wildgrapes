###
# natureServeCheck_20261116.R
# Tests every current model point (runAggregate2026_1k spatialData.gpkg) against
# the NatureServe Distribution column of the project taxonomy sheet, as if that
# range were applied as a hard filter. Check only: nothing in the pipeline uses
# this. Written 2026-11-16 for the geography-filter evaluation
# (work2026/geographyFilter/geographyFilterEvaluation_20261116.md).
#   Rscript work2026/geographyFilter/natureServeCheck_20261116.R
###
suppressMessages({library(sf); library(dplyr); library(readr); library(stringr)})
sheet <- read_csv("data/New World Vitis.csv", col_types = cols(.default = "c"), show_col_types = FALSE) |>
  filter(`Include in gap analysis?` == "Y")
fna <- read_csv("data/source_data/FNA_stateClassification.csv", col_types = cols(.default = "c"), show_col_types = FALSE)
filtered <- fna$`Taxon Name`[nchar(fna$`States from FNA`) > 3]
abb <- c(setNames(state.abb, state.name), "District of Columbia" = "DC",
         "Ontario"="ON","Quebec"="QC","Québec"="QC","New Brunswick"="NB","Nova Scotia"="NS","Manitoba"="MB",
         "Saskatchewan"="SK","British Columbia"="BC","Prince Edward Island"="PE","Alberta"="AB","Newfoundland and Labrador"="NL")
ctry <- c(Belize="BLZ", Guatemala="GTM", Colombia="COL", Ecuador="ECU", Venezuela="VEN")
rows <- list(); pts <- list()
for (i in seq_len(nrow(sheet))) {
  t <- sheet$`Scientific Name`[i]; dist <- sheet$Distribution[i]
  f <- file.path("data/Vitis", t, "runAggregate2026_1k/occurances/spatialData.gpkg")
  if (!file.exists(f)) { rows[[t]] <- tibble(taxon=t, filtered=t %in% filtered, nsRange=dist, points=0L, outside=0L, detail="no points"); next }
  d <- st_drop_geometry(st_read(f, quiet = TRUE))
  if (is.na(dist)) { rows[[t]] <- tibble(taxon=t, filtered=t %in% filtered, nsRange=NA, points=nrow(d), outside=NA, detail="NatureServe gives no range"); next }
  codes <- str_match_all(dist, "\\b([A-Z]{2}) \\(S[^)]*\\)")[[1]][,2]
  mexWhole <- str_detect(dist, regex("mexico(?!\\s*\\()", ignore_case = TRUE)) && !str_detect(dist, "Mexico \\(")
  mexStates <- str_match_all(dist, "Mexico \\(([^)]*)\\)")[[1]][,2] |> str_split(",\\s*") |> unlist()
  countries <- ctry[names(ctry)[str_detect(dist, names(ctry))]]
  broad <- str_detect(dist, "Central America|Caribbean")  # tiliifolia: everything south of Mexico
  inRange <- with(d, case_when(
    iso3 %in% c("USA","CAN") ~ unname(abb[state]) %in% codes,
    iso3 == "MEX" ~ mexWhole | state %in% mexStates,
    TRUE ~ broad | iso3 %in% countries))
  out <- d[!inRange, ]
  tb <- sort(table(paste(out$iso3, out$state)), decreasing = TRUE)
  rows[[t]] <- tibble(taxon=t, filtered=t %in% filtered, nsRange=dist, points=nrow(d), outside=nrow(out),
                      detail=paste(names(tb), tb, sep="=", collapse="; "))
  if (nrow(out) > 0) pts[[t]] <- out |> mutate(taxon = t) |> select(any_of(c("taxon","iso3","state","county","yearRecorded","databaseSource","institutionCode","sourceUniqueID","type","latitude","longitude")))
}
res <- bind_rows(rows) |> arrange(desc(outside))
write_csv(res, "work2026/geographyFilter/natureServe_outside_byTaxon_20261116.csv"); write_csv(bind_rows(pts), "work2026/geographyFilter/natureServe_outside_points_20261116.csv")
cat("taxa:", nrow(res), " with points outside NatureServe range:", sum(res$outside > 0, na.rm=TRUE), " total points outside:", sum(res$outside, na.rm=TRUE), "\n\n")
for (i in seq_len(nrow(res))) if (is.na(res$outside[i]) || res$outside[i] > 0) cat(sprintf("%-32s filter=%-5s n=%5d outside=%4s  %s\n", res$taxon[i], res$filtered[i], res$points[i], res$outside[i], substr(res$detail[i], 1, 170)))
