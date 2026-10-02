# Taxon change review: summary for Anne meeting (2026-09-30)

Source: `~/Documents/taxonChangeRawData_20260924.xlsx` (records tab, 3674 rows, Anne's decisions in cols B–C)

## Key finding

Anne's assumption is correct: add/remove pairs share the same `gbif_gbifID`, and the
`counterpart` column is consistent in all 1278 pairs. An accepted addition can
mechanically resolve its paired removal.

## Row populations

| Population | Rows | Meaning |
|---|---|---|
| Reassignment pairs | 1278 added + 1278 removed | Same record moved parent -> elevated species |
| Removed, no counterpart ("no project concept") | 551 | Record dropped entirely |
| Added, no counterpart ("not in the published dataset") | 556 | New record, nothing to pair |
| Double removals | 20 records (40 rows) | Under both species and variety, dropped from both |

## Reassignment flows (corrigendum skeleton)

| From | To | Records |
|---|---|---|
| V. cinerea | V. berlandieri | 627 |
| V. cinerea | V. baileyana | 123 |
| V. cinerea | V. simpsonii | 95 |
| V. rotundifolia | V. munsoniana | 243 |
| V. aestivalis | V. lincecumii | 164 |
| V. aestivalis | V. rufotomentosa | 26 |

## Review status

- 944 additions have a decision: 927 Agree, 6 Disagree, 9 Unclear, 2 Review.
- All 927 Agree additions are paired -> 927 removals need no review.
- 351 paired removals remain; they resolve when their additions are reviewed.
- 551 unpaired removals are the real burden (39 done). They cluster by GBIF name:
  V. caribaea 145, V. x labruscana 98, V. bicolor (3 spellings) 87, V. berlandieri var. tomentosa 22,
  Muscadinia popenoei 22, V. smalliana 19, V. aestivalis var. cinerea 16. Review per name, not per row.

## Non-Agree decisions -> pipeline changes, not spreadsheet edits

- **Synonymy fixes** (Anne: "Disagree, include record"): V. novomexicana -> V. acerifolia (3 rows),
  V. coriacea -> V. shuttleworthii (36 rows). Add to synonym table and rerun.
- **Exclude**: 6 V. aestivalis additions that are aestivalis x vulpina hybrid names.
- **My decision needed**: 9 V. aestivalis var. bicolor additions; Anne suggests adding at species level.
- **Answer directly**: 2 V. martineziana rows are not GBIF (Jun Wen pers. comm., Huerta-Acosta paper).

## Anomaly to check

The 9 var. bicolor additions list V. aestivalis as counterpart but have no matching removal row.
Either never under aestivalis in the published set, or the join missed them.

## Next steps

1. Build annotated copy of workbook (new file): review-status column marking the 927
   counterpart-resolved removals + carrying over the addition's decision; review-group column
   (= GBIF name) for unpaired removals; per-name summary tab.
2. Implement decisions in the pipeline (synonym table, exclusions) and rerun.
3. Decide on var. bicolor; reply on martineziana.
4. Draft corrigendum around the 6 flows + 551 exclusions + 556 new records, citing reason codes
   (gbifTaxonomy, countryExempt).
