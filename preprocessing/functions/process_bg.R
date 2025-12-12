#
# path <- "data/source_data/bg_survey.csv"
# t1 <- read_csv(path)
# write_csv(t1, file = "temp/bg_surveyCounts.csv")
# #species object is pull from run_all072025
# summarizeCount <- t1|>
#   dplyr::filter(`Taxon Full Name` %in% species)|>
#   dplyr::group_by(`Ex situ Collection Name`)|>
#   dplyr::count(sort = TRUE)
# View(summarizeCount)
# # read in the species list and filter then summarize

# write_csv(summarizeCount, file = "temp/bg_surveyCounts.csv")
### something with the lat long column is preventing it from being read in?
processBG <- function(path) {
  d1 <- read_csv(path) %>%
    select(
      originalTaxon = `Taxon Full Name`,
      genus = `Generic Epithet`,
      species = `Specific Epithet`,
      latitude = `Latitude of Wild-Collection Site`,
      longitude = `Longitude of Wild-Collection Site`,
      sourceUniqueID = `Accession Number`,
      country = `Country of Wild-Collection Site`,
      sampleCategory = `Germplasm Type Received`,
      localityInformation = `Locality of Wild-Collection Site`,
      biologicalStatus = `Provenance Type`,
      collectionSource = `Original Source`,
      yearRecorded = `Collection Year`,
      county = `County of Wild-Collection Site`,
      state = `State/Province of Wild-Collection Site`,
      observerName = `Collector Name`,
      institutionCode = `Ex situ Collection Name`
    ) %>%
    mutate(
      taxon = paste0(genus, " ", species) %>%
        stringr::str_replace(pattern = " NA", replacement = ""),
      databaseSource = "bg_survey",
      type = "G",
      iso3 = NA,
      finalOriginStat = NA,
      countyFIPS = NA,
      stateFIPS = NA,
      coordinateUncertainty = NA,
      recordID = paste0(databaseSource, "_", sourceUniqueID)
    )
  return(d1)
}
