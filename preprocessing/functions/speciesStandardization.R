# preprocessing/functions/speciesStandardization.R
# Assign records to the project taxon concepts defined in data/New World Vitis.csv
#
# Changes 2026-09 (see work2026/GBIF_taxonomy_notes.md):
#   * synonym cells are split on commas OR semicolons and "V. " is expanded to
#     "Vitis " (the V. vulpina cell is "Vitis cordifolia; V. cordifolia var.
#     sempervirens", which the old ", " split never matched)
#   * a taxon listed as its own synonym (V. munsoniana) no longer duplicates
#     every record; a record can enter a given concept only once
#   * the "Names to exclude from this concept" column is now applied: a record
#     whose original (source) name is on the concept's exclude list is removed
#     from that concept and returned in `excludedByConcept`
#   * subsetting uses which(): a record with an NA taxon no longer injects an
#     all-NA row into every concept (review/WORKFLOW_EVALUATION.md section 2.3)
#   * autonyms match their species (decision 2026-09-16): a record named
#     "Vitis rupestris f. rupestris" or "Vitis riparia subsp. riparia" is the
#     typical form of the species and enters the species concept without a
#     sheet entry. Autonyms that are project taxa themselves (V. aestivalis
#     var. aestivalis, V. cinerea var. cinerea) still match their own concept
#     directly; a record may feed both, as before.

# split a synonym / exclusion cell into clean names
splitNames <- function(x) {
  if (is.null(x) || is.na(x)) return(character(0))
  x <- stringr::str_trim(x)
  if (x %in% c("", "NA", "n/a", "N/A")) return(character(0))
  out <- x |>
    stringr::str_split(pattern = "\\s*[,;]\\s*") |>
    unlist() |>
    stringr::str_trim() |>
    stringr::str_replace("^V\\.\\s*", "Vitis ") |>
    stringr::str_squish()
  out[out != ""]
}

# the autonym forms of a species-level concept name ("Vitis riparia" ->
# "Vitis riparia var. riparia", "... subsp. riparia", "... f. riparia")
autonymsOf <- function(taxon) {
  m <- stringr::str_match(taxon, "^((?:Vitis|Muscadinia)\\s+(?:x\\s+)?([a-z]+(?:-[a-z]+)?))$")
  if (is.na(m[1, 1])) return(character(0))
  paste0(m[1, 2], c(" var. ", " subsp. ", " f. "), m[1, 3])
}

# reduce a name to a comparable form: "Genus [x] epithet [rank epithet]", lower case,
# authorship removed. Falls back to the squished lower-case string when the
# name does not parse (e.g. "V. arizonica/cinerea").
normalizeName <- function(x) {
  x <- stringr::str_replace(stringr::str_trim(x), "^V\\.\\s*", "Vitis ")
  # lower-case everything first: str_to_sentence() would keep a capital after
  # "subsp. " (e.g. "subsp. Arizonica") and the epithet would then be dropped
  x <- tolower(x)
  parsed <- stringr::str_match(
    stringr::str_replace_all(x, "×\\s*|\\s+x\\s+", " x ") |> stringr::str_squish(),
    "^((?:vitis|muscadinia)\\s+(?:x\\s+)?[a-z]+(?:-[a-z]+)?(?:\\s+(?:var\\.|subsp\\.|f\\.)\\s+[a-z]+(?:-[a-z]+)?)?)"
  )[, 2]
  ifelse(is.na(parsed), stringr::str_squish(x), parsed)
}

#' speciesCheck
#'
#' @param data : dataframe of unfiltered occurrence data (needs `index`, `taxon`, `originalTaxon`)
#' @param synonymList : reference data frame with columns `taxon`, `acceptedSynonym`
#'   and optionally `excludeNames` (or "Names to exclude from this concept")
#' @param applyExclusions : set FALSE to reproduce the pre-2026 behaviour (exclude lists ignored)
#'
#' @return list(includedData, excludedData, excludedByConcept)
#'   includedData      : records reassigned to the project concept name
#'   excludedData      : records whose name matched no concept or synonym
#'   excludedByConcept : records dropped from a concept because their original
#'                       name is on that concept's exclude list
speciesCheck <- function(data, synonymList, applyExclusions = TRUE){
  exclCol <- intersect(c("excludeNames", "Names to exclude from this concept"), names(synonymList))[1]
  if (!applyExclusions) exclCol <- NA
  nSpecies <- 1:length(synonymList$taxon)
  
  mapSynonyms <- function(i, synonymList, data){
    taxon <- synonymList$taxon[i]
    # synonyms, dropping a self reference (the concept name is matched already),
    # plus the concept's autonym forms
    syn1 <- union(setdiff(splitNames(synonymList$acceptedSynonym[i]), taxon), autonymsOf(taxon))
    
    # which() drops NA comparisons: `data[data$taxon == taxon, ]` returns one
    # all-NA row for every record whose taxon is NA (genus-only GBIF records)
    df2 <- data[which(data$taxon == taxon), ]
    for(j in syn1){
      df3 <- data[which(data$taxon == j), ]
      df2 <- bind_rows(df2, df3)
    }
    # a record may enter this concept only once
    df2 <- df2[!duplicated(df2$index), ]
    df2$taxon <- taxon
    
    # apply the concept's exclude list against the original (source) name
    removed <- df2[0, ]
    if (!is.na(exclCol)) {
      exclRaw <- splitNames(synonymList[[exclCol]][i])
      excl <- normalizeName(exclRaw)
      if (length(excl) > 0 && nrow(df2) > 0) {
        # match on the normalised name (authorship stripped) OR on the full
        # string with authorship, so homonyms such as "Vitis labrusca Thunb."
        # can be listed explicitly
        hit <- normalizeName(df2$originalTaxon) %in% excl |
          tolower(stringr::str_squish(df2$originalTaxon)) %in% tolower(stringr::str_squish(exclRaw))
        removed <- df2[hit, ]
        df2 <- df2[!hit, ]
      }
    }
    removed$excludedFromConcept <- rep(taxon, nrow(removed))
    return(list(kept = df2, removed = removed))
  }
  
  ## a record can legitimately feed two concepts when a taxon is modeled directly
  ## and is also a synonym of a broader concept (e.g. "Vitis aestivalis var. aestivalis")
  results <- purrr::map(nSpecies, mapSynonyms, synonymList = synonymList, data = data)
  includedData <- purrr::map(results, "kept") |> bind_rows()
  excludedByConcept <- purrr::map(results, "removed") |> bind_rows()
  
  # records that matched no concept at all
  excludedData <- data[!data$index %in% c(includedData$index, excludedByConcept$index), ]
  
  return(list(
    excludedData = excludedData,
    includedData = includedData,
    excludedByConcept = excludedByConcept
  ))
}
