# Record removals requested in the 2026 taxonomic review.
#
# Each entry is one reviewer request: the taxon, the rule that picks the
# records, and the source of the request. The rules act on the assembled
# model data (after clearNewErrors(), so the row indices of every other
# taxon are untouched) and the removed records are written next to the
# model input as removedByReview_<suffix>.csv.
#
# 1. Vitis tiliifolia, United States records in California and Florida.
#    Anne Frances, email of 22 Oct 2026: "Please delete records in CA and
#    FL - species does not occur in contiguous US." The georeferenced
#    records are the UC Davis Hopkins Vineyard vouchers (DAV 1958, one
#    coordinate) and three 1898 Jacksonville sheets (CONN, MNHN x2); they
#    were the two US points on the tiliifolia map. The records without
#    coordinates are the same vineyard material (Bioversity, USDA Grape
#    Repository, DAV) and Florida sheets (US, CAS, VT), plus the Kew
#    duplicate of the Jacksonville collection whose state field is empty.
#    Other US records of the taxon (Illinois, Louisiana, two with no state)
#    are left in place pending the reviewer's answer.
#
# 2. Vitis berlandieri, the two far-outlying points queried by a co-author
#    during the team review of the updated results (email of 13 Nov 2026;
#    decision by the project lead the same day). Both were V. cinerea records
#    in the published dataset, where the US-only FNA state list of V. cinerea
#    kept them out of every model; the review moved them to V. berlandieri,
#    which has no state list, so they reached the model for the first time.
#    (a) MO 1906, Tropicos 1395342: "Missouri -- Missouri Botanical Garden",
#        a specimen from the garden itself (cultivated), 38.61 N 90.26 W.
#    (b) UC 1953, "Cliff, Santo Domingo River above Hamilton Ranch", Baja
#        California, 30.77 N 115.93 W. The same collection enters twice more
#        without coordinates (UCJEPS UC1015881, UC1053351); all three go, as
#        the tiliifolia rule also removes the record set, not just the point.
#    Every other Mexican record of V. berlandieri is left in place; a Mexican
#    state range list is a separate decision for the reviewer.

reviewRemovalRules <- list(
  list(
    taxon  = "Vitis tiliifolia",
    reason = "Reviewer (A. Frances, 2026-10-22): does not occur in the contiguous US; delete CA and FL records",
    pick   = function(d) {
      d$iso3 %in% "USA" &
        (d$state %in% c("California", "Florida") |
           grepl("Jacksonville", d$localityInformation, ignore.case = TRUE))
    }
  ),
  list(
    taxon  = "Vitis berlandieri",
    reason = "Co-author query (2026-11-13), project decision: far-outlying points removed; Missouri Botanical Garden specimen (cultivated) and the Baja California collection",
    pick   = function(d) {
      d$sourceUniqueID %in% "urn:catalog:MO:Tropicos:1395342" |
        (d$iso3 %in% "MEX" & d$state %in% c("Baja California", "Baja california"))
    }
  )
)

applyReviewRemovals <- function(data, rules = reviewRemovalRules) {
  removed <- list()
  for (r in rules) {
    inTaxon <- data$taxon %in% r$taxon
    hit <- inTaxon & r$pick(data)
    hit[is.na(hit)] <- FALSE
    removed[[length(removed) + 1]] <- data[hit, ] |> dplyr::mutate(removalReason = r$reason)
    data <- data[!hit, ]
  }
  list(data = data, removed = dplyr::bind_rows(removed))
}
