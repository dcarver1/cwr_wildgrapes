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

reviewRemovalRules <- list(
  list(
    taxon  = "Vitis tiliifolia",
    reason = "Reviewer (A. Frances, 2026-10-22): does not occur in the contiguous US; delete CA and FL records",
    pick   = function(d) {
      d$iso3 %in% "USA" &
        (d$state %in% c("California", "Florida") |
           grepl("Jacksonville", d$localityInformation, ignore.case = TRUE))
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
