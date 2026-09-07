###
# review/verify_gbif_backbone.R
#
# Reproduces section 1.2 of review/WORKFLOW_EVALUATION.md.
#
# For every taxon in "New World Vitis.csv" this asks the GBIF backbone what it
# would return, then applies the taxon rule from preprocessing/functions/process_gbif.R
# and reports which project taxa the rule cannot recover.
#
# Needs only network access + rgbif; does not touch the occurrence download.
###

pacman::p_load(dplyr, readr, purrr, rgbif, stringr)

vitis <- read_csv("data/New World Vitis.csv", col_types = cols(.default = "c")) |>
  dplyr::select(taxon = "Scientific Name") |>
  dplyr::filter(!is.na(taxon))

# what the backbone returns for each project name
backbone <- purrr::map_dfr(vitis$taxon, function(nm) {
  m <- rgbif::name_backbone(name = nm)
  tibble::tibble(
    projectTaxon   = nm,
    status         = m$status         %||% NA_character_,
    gbifRank       = m$rank           %||% NA_character_,
    gbifScientific = m$scientificName %||% NA_character_,
    gbifSpecies    = m$species        %||% NA_character_,
    gbifGenus      = m$genus          %||% NA_character_
  )
})

# infraspecific epithet as GBIF would report it for the ACCEPTED taxon
backbone <- backbone |>
  dplyr::mutate(
    canonical = stringr::str_remove(gbifScientific, "\\s+\\(.*$"),
    infraEpithet = dplyr::if_else(
      gbifRank %in% c("VARIETY", "SUBSPECIES"),
      stringr::word(stringr::str_remove(canonical, "\\s+(var\\.|subsp\\.)\\s+"), -1),
      NA_character_
    )
  )

# the rule from process_gbif.R, applied verbatim
backbone <- backbone |>
  dplyr::mutate(
    pipelineTaxon = dplyr::case_when(
      gbifRank == "SPECIES"    ~ gbifSpecies,
      gbifRank == "GENUS"      ~ gbifGenus,
      gbifRank == "VARIETY"    ~ paste0(gbifSpecies, " var. ",   infraEpithet),
      gbifRank == "SUBSPECIES" ~ paste0(gbifSpecies, " subsp. ", infraEpithet),
      TRUE ~ gbifSpecies
    ),
    recovered = pipelineTaxon == projectTaxon
  )

readr::write_csv(backbone, "review/gbif_backbone_check.csv")

message(sum(!backbone$recovered, na.rm = TRUE), "/", nrow(backbone),
        " project taxa are NOT recovered by the taxonRank/species rule:")
print(as.data.frame(
  backbone |>
    dplyr::filter(!recovered | is.na(recovered)) |>
    dplyr::select(projectTaxon, status, gbifRank, gbifSpecies, pipelineTaxon)
))
