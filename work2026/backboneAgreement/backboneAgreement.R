# work2026/backboneAgreement/backboneAgreement.R
# Does the project taxonomy agree with two independent backbones?
#   WFO         : World Flora Online Plant List, read from ChecklistBank (dataset 2004 = WFO 2026-06).
#                 WFO's own hosts serve an incomplete certificate chain, ChecklistBank is the same release.
#   NatureServe : NatureServe Explorer API, every Vitis concept with its synonym list.
# Purpose: let the core project team run the splits where the project concept, WFO and NatureServe
# agree, and hold the rest for the taxonomic review.
#   Rscript work2026/backboneAgreement/backboneAgreement.R [output date YYYYMMDD]
# Inputs
#   * newest crosswalk snapshot in taxonomy/snapshots (the Google Sheet, pulled by preprocessing/pullTaxonomySheet.R)
#   * work2026/taxonChanges_published_vs_round2_detail_20260919.csv: every name whose records moved
#     between the published and round-2 datasets (fixed baseline shared with the review checklist)
#   * newest taxonChangeRawData_*.xlsx in work2026/taxaReviewAnne: Anne's per-record decisions, rolled up per name
# Outputs (work2026/backboneAgreement/)
#   cache/                      raw API pulls, dated, so the run can be repeated offline
#   backboneAgreement_<date>.xlsx  tabs: Concepts, Names, Flows, WFO Vitis, NatureServe Vitis, Read me
#   backboneAgreement_<date>.md    summary for the project team
suppressPackageStartupMessages({library(dplyr); library(readr); library(readxl); library(stringr); library(purrr); library(tidyr)
  library(httr2); library(jsonlite); library(openxlsx)})
options(width = 200)
dir <- "work2026/backboneAgreement"; cache <- file.path(dir, "cache")
args <- commandArgs(trailingOnly = TRUE)
outDate <- if (length(args) >= 1) args[1] else format(Sys.Date(), "%Y%m%d")
WFO_DATASET <- 2004   # ChecklistBank key for WFO Plant List 2026-06

# ---- helpers ---------------------------------------------------------------------------------------
# canonical name: genus + epithet (+ rank marker + infraspecific epithet), hybrid sign dropped, authors dropped
# orthographic variants WFO spells differently from the sheet (sheet spelling -> WFO spelling); used only for WFO lookups
spellingVariants <- c(lincecumii = "linsecomii", tiliaefolia = "tiliifolia")
dropHybridSign <- function(x) str_squish(str_replace_all(x, c("[×]" = " ", "(?<=\\s)x(?=\\s)" = " ", "(?<=\\s)x(?=doaniana|champinii|novae|labruscana|slavinii|bourquina)" = " ")))
rankRx <- "\\s(var\\.|subsp\\.|ssp\\.|f\\.|forma|subvar\\.)\\s+([a-z-]+)"
# canonical name: genus + epithet (+ rank marker + infraspecific epithet); hybrid sign and authors dropped wherever they sit
canon <- function(x) {
  x <- dropHybridSign(x)
  ge <- str_match(x, "^([A-Z][a-z-]+)\\s+([a-z-]+)")
  inf <- str_match(x, rankRx)
  rank <- recode(inf[, 2], ssp. = "subsp.", forma = "f.")
  out <- ifelse(is.na(ge[, 1]), NA_character_, ifelse(is.na(inf[, 1]), paste(ge[, 2], ge[, 3]), paste(ge[, 2], ge[, 3], rank, inf[, 3])))
  sel <- !is.na(x) & is.na(out) & str_detect(x, "^[A-Z][a-z-]+(\\s+[A-Z].*)?$")       # genus-only names such as "Vitis L."
  out[sel] <- str_extract(x[sel], "^[A-Z][a-z-]+")
  out
}
authorOf <- function(x) {
  x <- dropHybridSign(x)
  a <- str_remove(x, "^[A-Z][a-z-]+\\s+[a-z-]+") |> str_remove(rankRx) |> str_squish()
  a[is.na(canon(x)) | !str_detect(x, "^[A-Z][a-z-]+\\s+[a-z-]+")] <- NA_character_
  na_if(a, "")
}
chr <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else as.character(x[[1]])
normAuth <- function(a) str_to_lower(str_remove_all(coalesce(a, ""), "[\\s\\.]"))
isHybridName <- function(x) str_detect(x, "×|(^|\\s)x\\s")

# ---- 1. project taxonomy (the sheet) --------------------------------------------------------------
snapFile <- tail(sort(list.files("taxonomy/snapshots", "^NewWorldVitis_\\d{8}(_\\d{4})?\\.csv$", full.names = TRUE)), 1)
sheet <- read_csv(snapFile, col_types = cols(.default = "c")) |>
  transmute(concept = `Scientific Name`, conceptWithAuthor = `Scientific name with authors`,
            include = `Names to include in this concept (Homotypic synonyms)`, exclude = `Names to exclude from this concept`,
            elementCode = `Element Code`, fna = `Relationship to FNA`, nsStatusSheet = `Classification Status NatureServe`,
            extralimital = `Extralimital to US/Canada (native range)`, inGapAnalysis = `Include in gap analysis?`) |>
  filter(!is.na(concept))
cat("Sheet snapshot:", snapFile, "-", nrow(sheet), "concepts\n")
splitNames <- function(s) str_squish(str_split(coalesce(s, ""), ",")[[1]]) |> discard(~.x == "")
conceptNames <- sheet |> rowwise() |>
  reframe(concept, name = c(concept, splitNames(include)), role = c("concept", rep("include", length(splitNames(include))))) |>
  mutate(canonical = canon(name))
excludeNames <- sheet |> rowwise() |> reframe(concept, name = splitNames(exclude)) |> mutate(canonical = canon(name))
# canonical -> project concept (NA if on no list, "ambiguous" if on several)
toConcept <- function(canonical) {
  m <- conceptNames |> filter(canonical %in% !!canonical) |> distinct(canonical, concept, role) |>
    group_by(canonical) |> filter(!any(role == "concept") | role == "concept") |> ungroup() |> distinct(canonical, concept) |>
    group_by(canonical) |> summarise(concept = if (n() == 1) concept else paste("ambiguous:", paste(concept, collapse = " / ")), .groups = "drop")
  m$concept[match(canonical, m$canonical)]
}

# ---- 2. WFO via ChecklistBank ---------------------------------------------------------------------
wfoFile <- file.path(cache, sprintf("wfo_checklistbank%d_%s.json", WFO_DATASET, outDate))
if (!file.exists(wfoFile)) {
  pull <- function(q, offset) request("https://api.checklistbank.org") |>
    req_url_path("dataset", WFO_DATASET, "nameusage", "search") |>
    req_url_query(q = q, content = "SCIENTIFIC_NAME", limit = 1000, offset = offset) |>
    req_timeout(120) |> req_perform() |> resp_body_string() |> fromJSON(simplifyVector = FALSE)
  res <- list()
  for (q in c("Vitis", "Muscadinia")) { off <- 0; repeat { p <- pull(q, off); res <- c(res, p$result); off <- off + 1000; if (off >= p$total) break } }
  meta <- request("https://api.checklistbank.org") |> req_url_path("dataset", WFO_DATASET) |> req_perform() |> resp_body_string() |> fromJSON()
  write_json(list(dataset = meta[c("key", "title", "alias", "version", "issued")], pulled = format(Sys.time(), "%Y-%m-%d %H:%M %Z"), result = res), wfoFile, auto_unbox = TRUE)
}
wfoRaw <- fromJSON(wfoFile, simplifyVector = FALSE)
wfoVersion <- paste0(wfoRaw$dataset$title, " (ChecklistBank dataset ", wfoRaw$dataset$key, ")")
wfo <- map_dfr(wfoRaw$result, function(r) { u <- r$usage; n <- u$name
  tibble(wfoId = str_remove(chr(u$id), "-\\d{4}-\\d{2}$"), wfoName = chr(n$scientificName), wfoAuthor = chr(n$authorship), rank = chr(n$rank),
         genus = chr(n$genus), status = chr(u$status),
         acceptedId = chr(u$accepted$id %||% u$id), acceptedName = chr(u$accepted$name$scientificName %||% n$scientificName),
         acceptedAuthor = chr(u$accepted$name$authorship %||% n$authorship), acceptedRank = chr(u$accepted$name$rank %||% n$rank))}) |>
  filter(genus %in% c("Vitis", "Muscadinia") | wfoName %in% c("Vitis", "Muscadinia")) |>
  mutate(canonical = canon(wfoName), acceptedCanonical = canon(acceptedName)) |>
  distinct(wfoName, wfoAuthor, status, acceptedName, .keep_all = TRUE)        # the search returns some usages twice
cat("WFO:", wfoVersion, "-", nrow(wfo), "Vitis/Muscadinia names;", sum(wfo$status == "accepted"), "accepted,",
    sum(wfo$status == "synonym"), "synonyms,", sum(!wfo$status %in% c("accepted", "synonym")), "unplaced (bare names)\n")
# match one name to WFO: canonical first, author to split homonyms; placed names (accepted/synonym) beat bare names
wfoEmpty <- tibble(wfoMatch = NA_character_, wfoId = NA_character_, wfoName = NA_character_, wfoAuthor = NA_character_, wfoStatus = NA_character_,
                   wfoAccepted = NA_character_, wfoAcceptedAuthor = NA_character_, wfoAcceptedRank = NA_character_)
wfoMatch <- function(name) {
  cn <- str_replace_all(canon(name), setNames(unname(spellingVariants), paste0("\\b", names(spellingVariants), "\\b"))); au <- normAuth(authorOf(name))
  map2_dfr(cn, au, function(c1, a1) {
    cand <- wfo |> filter(!is.na(c1), canonical == c1)
    if (nrow(cand) == 0) return(mutate(wfoEmpty, wfoMatch = "no match"))
    how <- "exact"
    if (nrow(cand) > 1) {
      hit <- if (a1 == "") cand[0, ] else cand |> filter(normAuth(wfoAuthor) == a1)
      if (nrow(hit) == 0 && a1 != "") hit <- cand |> filter(normAuth(wfoAuthor) != "", pmin(nchar(a1), nchar(normAuth(wfoAuthor))) >= 3,
                                                           str_starts(normAuth(wfoAuthor), fixed(a1)) | str_starts(a1, fixed(normAuth(wfoAuthor))))
      if (nrow(hit) > 1 && any(hit$status != "bare name")) hit <- filter(hit, status != "bare name")
      if (nrow(hit) > 1 && n_distinct(hit$acceptedName) == 1) hit <- hit[1, ]
      if (nrow(hit) == 0 && nrow(filter(cand, status != "bare name")) == 1) { hit <- filter(cand, status != "bare name"); how <- "canonical (author differs)" }
      placed <- filter(cand, status != "bare name")
      if (nrow(hit) == 0 && nrow(placed) > 1 && n_distinct(placed$acceptedName) == 1) { hit <- placed[1, ]; how <- "author unresolved, but every homonym has the same accepted name" }
      if (nrow(hit) == 1) { cand <- hit; if (how == "exact") how <- "homonym resolved by author" }
      else return(mutate(wfoEmpty, wfoMatch = paste0("homonym, author needed: ", paste(paste(cand$wfoName, cand$wfoAuthor, "=", cand$acceptedName, cand$acceptedAuthor), collapse = " | ")), wfoName = c1))
    } else how <- if (a1 == "") "canonical (no author given)" else if (normAuth(cand$wfoAuthor) == a1) "exact" else "canonical (author differs)"
    tibble(wfoMatch = how, wfoId = cand$wfoId, wfoName = cand$wfoName, wfoAuthor = cand$wfoAuthor, wfoStatus = cand$status,
           wfoAccepted = cand$acceptedName, wfoAcceptedAuthor = cand$acceptedAuthor, wfoAcceptedRank = cand$acceptedRank)
  })
}

# ---- 3. NatureServe Explorer ---------------------------------------------------------------------
nsFile <- file.path(cache, sprintf("natureserve_vitis_%s.json", outDate))
if (!file.exists(nsFile)) {
  nsSearch <- function(token) request("https://explorer.natureserve.org/api/data/speciesSearch") |>
    req_body_json(list(criteriaType = "species",
                       textCriteria = list(list(paramType = "textSearch", searchToken = token, matchAgainst = "allScientificNames", operator = "startsWith")),
                       pagingOptions = list(page = 0, recordsPerPage = 100))) |>
    req_timeout(60) |> req_perform() |> resp_body_string() |> fromJSON(simplifyVector = FALSE)
  hits <- c(nsSearch("Vitis")$results, nsSearch("Muscadinia")$results)
  ids <- unique(map_chr(hits, "uniqueId"))
  details <- map(ids, ~request("https://explorer.natureserve.org/api/data/taxon") |> req_url_path_append(.x) |>
                   req_timeout(60) |> req_perform() |> resp_body_string() |> fromJSON(simplifyVector = FALSE))
  write_json(list(pulled = format(Sys.time(), "%Y-%m-%d %H:%M %Z"), taxa = details), nsFile, auto_unbox = TRUE)
}
nsRaw <- fromJSON(nsFile, simplifyVector = FALSE)
ns <- map_dfr(nsRaw$taxa, function(t) tibble(
  nsName = chr(t$scientificName), nsAuthor = chr(t$scientificNameAuthor), elementCode = chr(t$elcode), nsId = chr(t$uniqueId),
  nsStatus = chr(t$classificationStatus$classificationStatusDescEn), grank = chr(t$grank),
  nsSynonyms = paste(unlist(t$speciesGlobal$synonyms), collapse = "; "),
  conceptRef = str_remove_all(chr(t$conceptRefFullCitation), "<[^>]+>"),
  taxonomicComments = str_remove_all(chr(t$taxonomicComments), "<[^>]+>"),
  lastModified = chr(t$lastModified))) |>
  filter(str_detect(nsName, "^Vitis|^Muscadinia")) |> mutate(canonical = canon(nsName))
nsNames <- ns |> rowwise() |> reframe(nsName, elementCode, name = c(nsName, splitNames(str_replace_all(nsSynonyms, ";", ",")))) |>
  mutate(canonical = canon(name), role = if_else(name == nsName, "concept", "synonym"))
cat("NatureServe:", nrow(ns), "Vitis concepts, pulled", nsRaw$pulled, "\n")
lbl <- function(x) if_else(x == "OUT", "not a project concept", x)
nsEmpty <- tibble(nsMatch = "no match", nsConcept = NA_character_, nsElementCode = NA_character_)
nsMatch <- function(name) {
  cn <- canon(name)
  map_dfr(cn, function(c1) {
    h <- nsNames |> filter(!is.na(c1), canonical == c1) |> group_by(nsName, elementCode) |> summarise(role = if (any(role == "concept")) "concept" else "synonym", .groups = "drop")
    if (nrow(h) == 0) nsEmpty
    else if (nrow(h) > 1) tibble(nsMatch = "name on several concepts", nsConcept = paste(h$nsName, collapse = " / "), nsElementCode = paste(h$elementCode, collapse = " / "))
    else tibble(nsMatch = if (h$role == "concept") "accepted concept" else "synonym", nsConcept = h$nsName, nsElementCode = h$elementCode)
  })
}
# a backbone's accepted name, expressed as a project concept: via the sheet's own names first, then via the
# NatureServe concept the sheet points at with its Element Code. "OUT" = placed outside every project concept.
toConceptAny <- function(canonical) {
  viaSheet <- toConcept(canonical)
  nsCode <- nsNames$elementCode[match(canonical, nsNames$canonical)]
  viaNs <- sheet$concept[match(nsCode, sheet$elementCode, incomparables = NA)]
  out <- coalesce(viaSheet, viaNs)
  if_else(is.na(out) & !is.na(canonical), "OUT", out)
}

# ---- 4. Concepts tab: one row per project concept ------------------------------------------------
# WFO lookup for a concept: the concept's own name with author; if WFO does not know it, the include-list names
conceptWfo <- map_dfr(seq_len(nrow(sheet)), function(i) {
  tries <- c(sheet$conceptWithAuthor[i], sheet$concept[i], splitNames(sheet$include[i]))
  for (t in tries) { m <- wfoMatch(t); if (!m$wfoMatch %in% "no match" && !str_starts(m$wfoMatch, "homonym, author")) return(mutate(m, wfoLookedUp = t)) }
  mutate(wfoMatch(sheet$conceptWithAuthor[i]), wfoLookedUp = sheet$conceptWithAuthor[i])
})
concepts <- sheet |> bind_cols(conceptWfo) |>
  mutate(nsRow = match(elementCode, ns$elementCode),
         nsConcept = ns$nsName[nsRow], nsElementCode = ns$elementCode[nsRow], nsStatus = ns$nsStatus[nsRow], nsSynonyms = ns$nsSynonyms[nsRow],
         conceptRef = ns$conceptRef[nsRow], nsLastModified = ns$lastModified[nsRow],
         nsByName = nsMatch(concept)$nsConcept,       # what a name search would give, to catch a wrong Element Code
         wfoConcept = case_when(is.na(wfoStatus) | wfoStatus == "bare name" ~ NA_character_, TRUE ~ toConceptAny(canon(wfoAccepted))),
         wfoView = case_when(wfoMatch == "no match" ~ "name unknown to WFO",
                             str_starts(wfoMatch, "homonym, author") ~ "homonym, author needed",
                             wfoStatus == "bare name" ~ "name known to WFO but unplaced",
                             wfoStatus == "accepted" & canon(wfoAccepted) == canon(concept) ~ "accepted as named",
                             wfoStatus == "accepted" ~ paste0("accepted as ", wfoAccepted),
                             TRUE ~ paste0("synonym of ", wfoAccepted)),
         nsView = case_when(is.na(elementCode) ~ "no concept (no Element Code in sheet)",
                            is.na(nsRow) ~ "Element Code not found in Explorer",
                            canon(nsConcept) == canon(concept) ~ "accepted concept as named",
                            TRUE ~ paste0("accepted concept, as ", nsConcept)),
         wfoSame = !is.na(wfoConcept) & wfoConcept == concept,
         wfoParent = !is.na(wfoConcept) & wfoConcept != concept & str_starts(concept, paste0(wfoConcept, " ")),
         agreement = case_when(
           wfoSame & !is.na(nsRow) & wfoView == "accepted as named" & nsView == "accepted concept as named" ~ "AGREED: WFO and NatureServe both accept the concept as named",
           wfoSame & !is.na(nsRow) ~ "AGREED CIRCUMSCRIPTION: both recognise it, but under another name or rank",
           wfoSame & is.na(nsRow) & extralimital %in% "Y" ~ "WFO accepts; NatureServe has no concept (extralimital)",
           wfoSame & is.na(nsRow) ~ "WFO accepts; NatureServe has no concept (no Element Code in sheet)",
           wfoParent & !is.na(nsRow) ~ paste0("VARIETY DISPUTED: NatureServe accepts the variety, WFO folds it into ", wfoConcept),
           wfoParent ~ paste0("WFO folds the variety into ", wfoConcept, "; NatureServe has no concept"),
           !is.na(wfoConcept) & wfoConcept != "OUT" & !is.na(nsRow) ~ paste0("SPLIT DISPUTED: NatureServe accepts, WFO lumps into ", wfoConcept),
           !is.na(wfoConcept) & wfoConcept != "OUT" ~ paste0("WFO lumps into ", wfoConcept, "; NatureServe has no concept"),
           wfoConcept %in% "OUT" ~ paste0("WFO places it outside the project concepts (", wfoAccepted, ")"),
           !is.na(nsRow) ~ "NatureServe accepts; WFO has no placement for the name",
           TRUE ~ "NO BACKBONE OPINION"),
         elementCodeCheck = case_when(is.na(elementCode) ~ NA_character_, is.na(nsByName) ~ "code found; name not in Explorer", nsByName == nsConcept ~ "ok", TRUE ~ paste0("name search gives ", nsByName))) |>
  select(concept, conceptWithAuthor, inGapAnalysis, extralimital, fna, include, exclude, agreement,
         wfoView, wfoConcept, wfoLookedUp, -wfoSame, -wfoParent, wfoMatch, wfoId, wfoName, wfoAuthor, wfoStatus, wfoAccepted, wfoAcceptedAuthor,
         nsView, nsConcept, elementCode, elementCodeCheck, nsStatus, nsSynonyms, conceptRef, nsLastModified)

wfoVarConcept <- toConceptAny(wfo$acceptedCanonical)
wfoVarSpecies <- str_remove(wfo$acceptedName, " (var|subsp|f)\\. .*")
broadWfo <- unique(c(concepts$wfoConcept[str_detect(concepts$agreement, "DISPUTED")],
                     wfoVarSpecies[wfo$status == "accepted" & wfo$rank != "species" & !is.na(wfoVarConcept) & wfoVarConcept != "OUT" & wfoVarConcept != wfoVarSpecies]))
broadWfo <- broadWfo[!is.na(broadWfo) & broadWfo != "OUT"]
cat("WFO treats these species more broadly than the project:", paste(broadWfo, collapse = ", "), "\n")

# ---- 5. Names tab: every name whose records moved -----------------------------------------------
det <- read_csv("work2026/taxonChanges_published_vs_round2_detail_20260919.csv", show_col_types = FALSE)
afFile <- tail(sort(list.files("work2026/taxaReviewAnne", "^taxonChangeRawData_.*\\.xlsx$", full.names = TRUE)), 1)
anne <- read_excel(afFile, "records") |>
  mutate(dec = case_when(is.na(`AF decision`) ~ "not reviewed", TRUE ~ str_extract(`AF decision`, "^(Agree|Disagree|Unclear|Review)"))) |>
  group_by(name = projectRecordedName) |>
  summarise(anneDecision = paste0(names(table(dec)), " ", as.integer(table(dec)), collapse = "; "),
            anneComment = paste(unique(na.omit(`AF Comment`)), collapse = " | "), .groups = "drop")
names_ <- det |> filter(taxonomyChange == "Yes") |>
  group_by(name = recordedName, change, species, counterpart, reason) |>
  summarise(records = sum(records), verbatimNames = paste(unique(na.omit(verbatimNames)), collapse = "; "), note = paste(unique(na.omit(note)), collapse = " "), .groups = "drop") |>
  mutate(publishedSpecies = case_when(change == "Removed" ~ species, str_detect(counterpart, "^not in the published") ~ "not in published dataset", TRUE ~ counterpart),
         round2Destination = case_when(change == "Added" ~ species, str_detect(counterpart, "^no project concept") ~ "dropped (no project concept)", TRUE ~ counterpart)) |>
  select(-species, -counterpart) |> left_join(anne, by = "name")
nm <- names_ |> bind_cols(wfoMatch(names_$name)) |> bind_cols(nsMatch(names_$name)) |>
  mutate(canonical = canon(name), isGenusOnly = !str_detect(canonical, " "),
         hybrid = isHybridName(name) | isHybridName(coalesce(wfoAccepted, "")) | isHybridName(coalesce(nsConcept, "")) | str_detect(canonical, "labruscana|slavinii|champinii|doaniana|novae-angliae|bourquina"),
         onSheetList = toConcept(canonical), onExcludeList = excludeNames$concept[match(canonical, excludeNames$canonical)],
         wfoConcept = case_when(is.na(wfoStatus) | wfoStatus == "bare name" ~ NA_character_, TRUE ~ toConceptAny(canon(wfoAccepted))),
         nsConceptMapped = case_when(nsMatch == "no match" | nsMatch == "name on several concepts" ~ NA_character_,
                                     TRUE ~ coalesce(sheet$concept[match(nsElementCode, sheet$elementCode, incomparables = NA)], "OUT")),
         wfoView = case_when(wfoMatch == "no match" ~ "unknown to WFO", str_starts(wfoMatch, "homonym, author") ~ wfoMatch,
                             wfoStatus == "bare name" ~ "known to WFO but unplaced",
                             wfoStatus == "accepted" ~ paste0("accepted (", wfoAcceptedRank, ")"), TRUE ~ paste0("synonym of ", wfoAccepted, " ", coalesce(wfoAcceptedAuthor, ""))),
         nsView = case_when(nsMatch == "no match" ~ "unknown to NatureServe", nsMatch == "accepted concept" ~ "accepted concept",
                            nsMatch == "name on several concepts" ~ paste0("on several concepts: ", nsConcept), TRUE ~ paste0("synonym of ", nsConcept)),
         nested = !is.na(wfoConcept) & !is.na(nsConceptMapped) & wfoConcept != nsConceptMapped &
                  (str_starts(nsConceptMapped, paste0(wfoConcept, " ")) | str_starts(wfoConcept, paste0(nsConceptMapped, " "))),
         backboneConcept = case_when(
           isGenusOnly ~ "genus-level name",
           !is.na(wfoConcept) & !is.na(nsConceptMapped) & wfoConcept == nsConceptMapped ~ lbl(wfoConcept),
           nested ~ paste0(if_else(nchar(wfoConcept) > nchar(nsConceptMapped), wfoConcept, nsConceptMapped), " (nested: the other backbone stops at the species)"),
           !is.na(wfoConcept) & !is.na(nsConceptMapped) ~ paste0("CONFLICT: WFO -> ", lbl(wfoConcept), "; NatureServe -> ", lbl(nsConceptMapped)),
           !is.na(wfoConcept) ~ paste0(lbl(wfoConcept), " (WFO only)"),
           !is.na(nsConceptMapped) ~ paste0(lbl(nsConceptMapped), " (NatureServe only)"),
           TRUE ~ "no backbone opinion"),
         nSources = (!is.na(wfoConcept)) + (!is.na(nsConceptMapped)),
         target = case_when(nested ~ if_else(nchar(wfoConcept) > nchar(nsConceptMapped), wfoConcept, nsConceptMapped),
                            !is.na(wfoConcept) & !is.na(nsConceptMapped) & wfoConcept != nsConceptMapped ~ NA_character_,
                            TRUE ~ coalesce(wfoConcept, nsConceptMapped)),
         agreement = case_when(
           isGenusOnly ~ "n/a: genus-only record",
           str_starts(backboneConcept, "CONFLICT") ~ "CONFLICT between backbones: hold for review",
           nSources == 0 ~ "NO OPINION: hold for review",
           target == "OUT" & nSources == 2 & round2Destination == "dropped (no project concept)" ~ "AGREED DROP: both backbones place it outside the project concepts",
           target == "OUT" & nSources == 2 ~ "AGREED BACKBONES, PIPELINE DIFFERS: both place it outside the project concepts, pipeline kept it",
           target == "OUT" & hybrid ~ "ONE BACKBONE: outside the project concepts (named hybrid)",
           target == "OUT" ~ "ONE BACKBONE: outside the project concepts",
           nSources == 2 & target == round2Destination ~ "AGREED: both backbones match the round-2 destination",
           nSources == 2 ~ paste0("AGREED BACKBONES, PIPELINE DIFFERS: both say ", target),
           target == round2Destination ~ "ONE BACKBONE: matches the round-2 destination",
           is.na(nsConceptMapped) & target %in% broadWfo ~ paste0("ONE BACKBONE (broad WFO concept): says ", target, ", pipeline differs"),
           TRUE ~ paste0("ONE BACKBONE: says ", target, ", pipeline differs")),
         sheetEditSuggested = case_when(
           str_detect(agreement, "PIPELINE DIFFERS: both say") | str_detect(agreement, "^ONE BACKBONE( \\(broad WFO concept\\))?: says") ~ paste0("add '", canonical, "' to the include list of ", target),
           str_starts(agreement, "AGREED DROP") & hybrid & !publishedSpecies %in% "not in published dataset" ~ paste0("hybrid: add '", canonical, "' to the exclude list of ", publishedSpecies),
           TRUE ~ NA_character_)) |>
  select(name, canonical, change, records, publishedSpecies, round2Destination, reason, agreement, backboneConcept, sheetEditSuggested,
         anneDecision, anneComment, wfoView, wfoConcept, wfoMatch, wfoId, nsView, nsConceptMapped, nsElementCode,
         onSheetList, onExcludeList, hybrid, verbatimNames, note, -nested) |>
  mutate(wfoConcept = if_else(wfoConcept == "OUT", "not a project concept", wfoConcept), nsConceptMapped = if_else(nsConceptMapped == "OUT", "not a project concept", nsConceptMapped)) |>
  arrange(factor(str_extract(agreement, "^[A-Za-z/ ,]+?(?=:)"), levels = c("AGREED", "AGREED DROP", "AGREED BACKBONES, PIPELINE DIFFERS", "ONE BACKBONE", "CONFLICT between backbones", "NO OPINION", "n/a")), desc(records))
stopifnot(!anyNA(nm$agreement), !anyNA(nm$records))

# ---- 6. Flows tab: the species-to-species reassignments, summarised ------------------------------
isAgreed <- function(a) str_starts(a, "AGREED:")
flows <- nm |> filter(change == "Removed", !str_starts(round2Destination, "dropped")) |>
  group_by(from = publishedSpecies, to = round2Destination) |>
  summarise(names = n(),
            agreedRecords = sum(records[isAgreed(agreement)]), heldRecords = sum(records[!isAgreed(agreement)]),
            namesHeld = paste(unique(name[!isAgreed(agreement)]), collapse = "; "),
            anne = paste(unique(anneDecision), collapse = " / "), records = sum(records), .groups = "drop") |>
  relocate(records, .after = to) |>
  left_join(concepts |> select(to = concept, toConceptAgreement = agreement, toWfo = wfoView, toNatureServe = nsView), by = "to") |>
  mutate(verdict = case_when(!str_starts(toConceptAgreement, "AGREED") ~ "HOLD: the destination concept itself is disputed",
                             heldRecords == 0 ~ "RUN: destination concept and every name agreed",
                             TRUE ~ "RUN the agreed names; hold the names listed")) |>
  arrange(desc(records))
dropped <- nm |> filter(change == "Removed", str_starts(round2Destination, "dropped")) |>
  group_by(from = publishedSpecies) |> summarise(names = n(),
    agreedDrop = sum(records[str_starts(agreement, "AGREED DROP")]),
    backbonesSayKeep = sum(records[str_detect(agreement, "PIPELINE DIFFERS: both say")]),
    oneBackboneSaysKeep = sum(records[str_starts(agreement, "ONE BACKBONE: says")]),
    oneBackboneBroadSaysKeep = sum(records[str_starts(agreement, fixed("ONE BACKBONE (broad"))]),
    oneBackboneSaysDrop = sum(records[str_starts(agreement, "ONE BACKBONE: outside")]),
    held = sum(records[str_starts(agreement, "NO OPINION") | str_starts(agreement, "CONFLICT")]), records = sum(records), .groups = "drop") |>
  relocate(records, .after = from) |> arrange(desc(records))
newRecords <- nm |> filter(change == "Added", publishedSpecies == "not in published dataset") |>
  group_by(to = round2Destination) |> summarise(names = n(),
    agreed = sum(records[isAgreed(agreement)]), oneBackboneAgrees = sum(records[str_starts(agreement, "ONE BACKBONE: matches")]),
    genusOnly = sum(records[str_starts(agreement, "n/a")]), records = sum(records), other = records - agreed - oneBackboneAgrees - genusOnly, .groups = "drop") |>
  relocate(records, .after = to) |> arrange(desc(records))

# ---- 7. write ----------------------------------------------------------------------------------------
readme <- tibble(item = c("Purpose", "Project taxonomy", "WFO", "NatureServe", "Changed names", "Anne's decisions",
                          "Concepts tab", "Names tab", "Flows tab", "Dropped tab", "New records tab", "Agreement rule", "Caveat"),
  text = c("Compare the project taxon concepts and every name whose records moved in round 2 against two independent backbones.",
           paste("Sheet snapshot", basename(snapFile)),
           paste0(wfoVersion, ", release issued ", wfoRaw$dataset$issued, ", read from api.checklistbank.org on ", wfoRaw$pulled, ". WFO's own servers were unreachable (certificate chain), ChecklistBank mirrors the same release."),
           paste("NatureServe Explorer API, taxon record for every Vitis and Muscadinia concept, pulled", nsRaw$pulled),
           "work2026/taxonChanges_published_vs_round2_detail_20260919.csv (taxonomyChange = Yes)",
           basename(afFile),
           "One row per project concept: does each backbone recognise it? AGREED = both accept it as named; AGREED CIRCUMSCRIPTION = both recognise the same thing under another name or rank (e.g. as a variety).",
           "One row per recorded name and published species: where each backbone would place the name, in project-concept terms, versus where the round-2 pipeline put it.",
           "Species-to-species reassignments with records split into agreed and held, and a verdict.",
           "Records the round-2 pipeline dropped because the name matched no project concept, by published species.",
           "Records that are new in round 2 (not in the published dataset), by destination.",
           "Run a split when the destination concept is AGREED and the names carrying its records are AGREED. Hold anything marked CONFLICT or NO OPINION, and anything a backbone places outside the project concepts unless it is a hybrid. ONE BACKBONE rows are evidence for the review, not agreement. 'broad WFO concept' marks a WFO pointer to a species WFO defines more widely than the project (it lumps other project concepts into it), which is weaker evidence.",
           "WFO and NatureServe are evidence, not the taxonomic review. Names they resolve are proposed for sign-off, not applied automatically."))
wb <- createWorkbook()
addTab <- function(nm_, d) { addWorksheet(wb, nm_); writeData(wb, nm_, d, headerStyle = createStyle(textDecoration = "bold")); freezePane(wb, nm_, firstRow = TRUE); setColWidths(wb, nm_, 1:ncol(d), "auto") }
addTab("Read me", readme); addTab("Flows", flows); addTab("Dropped", dropped); addTab("New records", newRecords); addTab("Concepts", concepts); addTab("Names", nm)
addTab("WFO Vitis", wfo |> select(-canonical, -acceptedCanonical) |> arrange(acceptedName, status)); addTab("NatureServe Vitis", ns |> select(-canonical))
xlsx <- file.path(dir, sprintf("backboneAgreement_%s.xlsx", outDate)); saveWorkbook(wb, xlsx, overwrite = TRUE)
write_csv(concepts, file.path(dir, sprintf("concepts_%s.csv", outDate))); write_csv(nm, file.path(dir, sprintf("names_%s.csv", outDate)))
write_csv(flows, file.path(dir, sprintf("flows_%s.csv", outDate)))
cat("\nWrote", xlsx, "\n\n")
cat("== Concepts\n"); print(count(concepts, agreement), n = 20)
cat("\n== Names (records)\n"); print(nm |> count(agreement, wt = records, name = "records") |> arrange(desc(records)), n = 30)
cat("\n== Flows\n"); print(flows |> select(from, to, records, agreedRecords, heldRecords, verdict), n = 30)
cat("\n== Dropped\n"); print(dropped, n = 30)
cat("\n== New records\n"); print(newRecords, n = 30)
