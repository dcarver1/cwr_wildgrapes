# Anne's completed review: what it asks of the pipeline (19 Oct 2026)

Source: `taxonChangeRawData_20260924v2.xlsx` (records tab; every one of the 3,674 rows now carries a decision).
Built by `reviewDecisions_20261019.R`; tables in `reviewDecisions_20261019.xlsx` (one csv per tab).

## Decisions

| Direction | Agree | Agree with concerns | Disagree | Unclear / Review | n/a (paired) |
|---|---:|---:|---:|---:|---:|
| Added (1,845) | 1,815 | 9 | 8 | 13 | |
| Removed (1,829) | 179 | | 277 | 95 | 1,278 |

The 1,278 n/a removals are the reassignment pairs (cinerea to berlandieri / baileyana / simpsonii, rotundifolia to
munsoniana, aestivalis to lincecumii / rufotomentosa); all of their additions are agreed. So every concept split is signed off.

## Actions

1. **Sheet edits** (tab sheetEdits): 22 names to add to include lists on the Google sheet, 24 concept-name pairs in all
   (acerifolia / novomexicana and shuttleworthii / coriacea are already on). A dry run of speciesCheck() with the edited list
   (tab sheetEditsEffect) brings back: tiliifolia +211 (caribaea, arachnoidea, acuminata; 204 of them V. caribaea, the review
   saw 152 because the rest were never in the published dataset), aestivalis +38, popenoei +22, berlandieri +14,
   cinerea var. tomentosa +11 (from zero), cinerea +8, labrusca +8, simpsonii +5, vulpina +5, baileyana +3, var. cinerea +2,
   riparia +2. Counts are before de-duplication and the coordinate checks.
2. **Parser fix, done** (`preprocessing/functions/process_gbif_082026.R`): a hybrid formula with authorship or a cultivar
   before the "x" ("Vitis aestivalis Michx. x V. vulpina L.", "Muscadinia rotundifolia cv. Yuga x Carlos") is now assigned
   to no concept. Removes the 8 additions the review rejected (aestivalis 6, labrusca 1, rotundifolia 1). Over the whole
   GBIF download 98 rows change, all hybrid or cultivar crosses; no other name is affected.
3. **Geography** (tabs geographyCheck, geographySummary, rangeStatesProposed): the driver's state filter (applyFNA) acts
   only on taxa named in `FNA_stateClassification.csv`. The elevated taxa have no row, so their out-of-range points reach
   the SDM unfiltered:

   | Taxon | Out-of-range points reaching the model | Where |
   |---|---:|---|
   | simpsonii | 33 | 31 UC Davis vineyard vouchers (Olmo 1958, Davis CA), LA, DC |
   | munsoniana | 25 | 22 Davis vouchers, 3 Texas |
   | baileyana | 6 | MD, MS |
   | rufotomentosa | 6 | 4 Maryland (NCU/TAWES 2023), 1 Maryland, 1 Alabama iNaturalist; plus TX/LA specimens without coordinates |
   | berlandieri | 2 | MO, OK |
   | lincecumii | 2 | KS |

   Under V. cinerea and V. rotundifolia the Davis vouchers were filtered out; under the new concepts nothing filters them.
   Proposed lists: Weakley for rufotomentosa (FL, GA, SC, NC) and lincecumii (AR, OK, LA, TX) as Anne set out; FNA's
   var. munsoniana row for munsoniana; NatureServe (sheet) for simpsonii and baileyana pending a Weakley check
   (fsus.ncbg.unc.edu sits behind a browser challenge, so it was not fetched); none for berlandieri, because a US-only list
   would drop its Mexican points.
4. **Open decisions** (tab openDecisions): the 87 V. bicolor records (Anne: move to var. bicolor), the 9 V. argentifolia
   additions to var. bicolor (Anne: species level instead), 6 V. odoratissima (no coordinates; leave out), whether to keep
   iNaturalist observations for rufotomentosa.
5. **Answers for Anne** (tab notesForAnne): the girdiana and riparia removals she queried are the two hand-removed
   coordinate errors (lon -177.28, lat 82.23), not taxonomy. The 101 hybrid additions (x champinii etc.) come from the
   global CWR occurrence database on GBIF (dataset 07044577), which republishes germplasm accessions as occurrences;
   the pipeline types them H, as the published build did. That, not taxonomy, is what moves x champinii from MP to HP.

## Next

Apply the sheet edits on Google Drive, decide the open items, decide on the range lists, then re-run preprocessing and
the affected taxa; then revise `taxonomyUpdateStatus` and the team status.
