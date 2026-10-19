# Google sheet edits for the review decisions (19 Oct 2026)

Edit the "New World Vitis" Google sheet. Preprocessing pulls it at the start and logs each changed cell to
`taxonomy/snapshots/changeLog.csv`, so nothing needs editing locally. Names are comma-separated within a cell, exactly as
the existing entries are. Tick each line as done.

## Column: Names to include in this concept (Homotypic synonyms)

| Concept row | Add these names | Why (reviewer) |
|---|---|---|
| Vitis aestivalis | Vitis smalliana, Vitis araneosa, Vitis intermedia, Vitis labrusca var. aestivalis | synonyms per FNA / WFO |
| Vitis aestivalis var. bicolor | Vitis bicolor | "why not move these records to var. bicolor" |
| Vitis baileyana | Vitis virginiana | counterpart of a V. cinerea removal |
| Vitis berlandieri | Vitis cordifolia var. helleri, Vitis helleri | synonym; counterpart per FNA |
| Vitis cinerea | Vitis aestivalis var. cinerea | counterpart (both cinerea rows were removals) |
| Vitis cinerea var. cinerea | Vitis canescens | counterpart |
| Vitis cinerea var. tomentosa | Vitis berlandieri var. tomentosa | homotypic synonym per POWO |
| Vitis labrusca | Vitis catawba, Vitis blanda, Vitis obovata, Vitis labrusca f. alba | synonyms in most sources; "check distribution" |
| Vitis popenoei | Muscadinia popenoei | synonym of V. popenoei |
| Vitis riparia | Vitis vulpina var. praecox | accepted synonym |
| Vitis simpsonii | Vitis austrina, Vitis sola | counterpart |
| Vitis tiliifolia | Vitis caribaea, Vitis arachnoidea, Vitis acuminata | heterotypic synonyms per POWO |
| Vitis vulpina | Vitis pullaria | synonym per FNA |

Already on the sheet (applied 2026-10-05, no action): Vitis novomexicana under V. acerifolia; Vitis coriacea under
V. shuttleworthii.

## Column: Names to include, removals

| Concept row | Remove these names | Why |
|---|---|---|
| Vitis aestivalis var. bicolor | Vitis aestivalis var. argentifolia, Vitis argentifolia | reviewer: keep argentifolia at species level; both strings stay on the V. aestivalis row |

## Column: Names to exclude from this concept

| Concept row | Add this string | Why |
|---|---|---|
| Vitis labrusca | Vitis obovata Baker | later homonym described from Brazil (1 GBIF record, no locality); the exclude list matches the authored string, so "Vitis obovata" Raf. records still come in through the include list |

## Notes

- The V. aestivalis row already lists "Vitis aestivalis var. bicolor" and the two argentifolia strings, so the species
  concept keeps every argentifolia record. "Vitis bicolor" goes only on the variety row, per the decision of 2026-10-19.
- "Vitis caribaea" brings 204 records, of which 53 carry the author string "Hook. & Arn." (a later homonym GBIF cannot file
  as a synonym; same plants, same geography). Noted for the reviewer, no sheet action.
- Expected effect once preprocessing runs (before de-duplication): tiliifolia +211, aestivalis +38, popenoei +22,
  berlandieri +14, cinerea var. tomentosa +11, cinerea +8, labrusca +8, simpsonii +5, vulpina +5, baileyana +3,
  var. cinerea +2, riparia +2; var. bicolor +87 and -9.
