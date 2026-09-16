# Why each taxon's record count changed: taxonomy-only run vs December 2025

Written 2026-09-16 from `work2026/explainTaxonChanges_20260916.R`, which traces
every record (source + source ID) between `model_data20251216.csv` and
`model_data20260820_taxonomyOnly.csv`. Full flows with names:
`work2026/taxonChangeFlows_20260916.csv`. Counts are model-data rows after all
filters; "coords" = rows with coordinates. Taxa not listed did not change.

Five mechanisms account for everything:

* **A. Segregate recovered.** Records recorded under a project name that the
  GBIF backbone treats as a synonym of another project species. The old parser
  read the backbone's accepted species, so the records sat in the wrong concept.
* **B. Verbatim recovery.** Records GBIF could not match at all (interpreted
  name "Vitis L."). The publisher's verbatim name is now parsed.
* **C. Synonym-cell fix.** The *V. vulpina* include cell uses a semicolon and
  "V."; it never matched before.
* **D. Name on no include list.** The backbone silently folded these names into
  a project species. Name parsing keeps the recorded name, which matches no
  concept, so the records are now in `excludedOnTaxonomy_08202026.csv`. Each
  needs a sheet decision (notes section 5); adding the name to an include cell
  restores the records.
* **E. Homonym: backbone kept.** Same binomial, different author, different
  species. Decision 2026-09-16: `processGBIF()` keeps the backbone assignment
  for an authored name that maps to a different accepted species than the
  majority form of its binomial (465 records, 23 name strings, see
  `gbifHomonymNames_08202026.csv`). No sheet entries needed. The taxa below
  reflect that rule.

## Taxa that gained records

(25 taxa change in total after the homonym and autonym rules; 15 unchanged.)

**Vitis berlandieri** +621 (coords +267, G +44). A: 627 records recorded as
"Vitis berlandieri Planch." return from *V. cinerea* (backbone: synonym of
*V. cinerea* var. *helleri*). D: 6 "Vitis cordifolia var. helleri" now sit on
no list. Action: consider adding "Vitis cordifolia var. helleri" to the include
cell.

**Vitis munsoniana** +243 (coords +92). A: 243 records recorded as
"Vitis munsoniana" or "Muscadinia munsoniana" return from *V. rotundifolia*.
D (not in this file, see notes section 5): 412 more records named "Vitis
rotundifolia var. munsoniana" are still in *rotundifolia*; the sheet's own note
says the concept is var. munsoniana, so they belong here once the name is added.

**Vitis vulpina** +177 (coords +18, G +25). C: 182 records recorded as
"Vitis cordifolia" (GBIF "Vitis cordifolia Lam." and "Michx.", plus 19 Genesys
and WIEWS accessions, 1 var. sempervirens) enter for the first time. E: the 13
"Vitis cordifolia Roth ex Roem. & Schult." (= *V. heyneana*) stay out and the
7 "Vitis vulpina Bartram" stay in *labrusca*, both by the homonym rule. D: 5
"Vitis pullaria" lost.

**Vitis lincecumii** +164 (coords +17, G +7). A: 93 "Vitis lincecumii" and 71
"Vitis linsecomii" return from *V. aestivalis*.

**Vitis baileyana** +122 (coords +48, G +2). A: 123 "Vitis baileyana" return
from *V. cinerea*; 1 lost to no list.

**Vitis simpsonii** +95 (coords +56). A: 95 "Vitis simpsonii" return from
*V. cinerea*.

**Vitis x champinii** +66 (coords +5). B: 66 records whose verbatim name was
"Vitis xchampinii" (no space after the marker); GBIF could not match it.

**Vitis rufotomentosa** +40 (coords +20). A: 26 "Vitis rufotomentosa Small"
return from *V. aestivalis*. B: 14 records GBIF filed as "Vitis L." whose
verbatim name is "Vitis rufotomentosa" (9 with coordinates, iNaturalist). The
6 Japanese records never reach the model data (country filter).

**Vitis x novae-angliae** +23 (coords +5). B: verbatim "Vitis xnovae-angliae".

**Vitis x doaniana** +12 (coords +3). B: verbatim "Vitis xdoaniana".

**Vitis aestivalis var. bicolor** +9 (coords +2). A: 9 "Vitis argentifolia"
(a bicolor synonym on the include list) return from *V. aestivalis*.

**Vitis martineziana** +2 (coords +2). B: verbatim "Vitis martineziana",
Mexico, GBIF filed as "Vitis L.".

**Vitis mustangensis** +1. "Vitis candicans var. diversa" now matched.

**Vitis riparia** -15 (coords -1). D: 18 to no list: incisa 10, odoratissima
6, vulpina var. praecox 2. E: 5 "Vitis rubra Desf." stay in riparia by the
homonym rule. +3 "Vitis vulpina subsp. riparia" (on the include list). Not
in this file under either parser: 417 raw records named "Vitis riparia subsp.
riparia", a subspecies autonym left for the sheet decision (only the forma
autonym is mapped in code).

## Taxa that lost records

**Vitis cinerea** -882 (coords -386, G -46). A: 627 to *berlandieri*, 123 to
*baileyana*, 95 to *simpsonii* (see above). D: 37 to no list: "Vitis
berlandieri var. tomentosa" 11, "Vitis aestivalis var. cinerea" 8, "Vitis
helleri" 8, sola 3, virginiana 3, austrina 2, canescens 2.
Action: berlandieri var. tomentosa -> var. tomentosa include; aestivalis var.
cinerea -> var. cinerea include; helleri -> berlandieri include.

**Vitis aestivalis** -323 (coords -62, G -9). A: 164 to *lincecumii*, 26 to
*rufotomentosa*, 9 to var. *bicolor*. D: 139 to no list: "Vitis bicolor" 87
(three author variants; should go to var. bicolor), smalliana 19, araneosa 11,
bourquiniana 9 (a cultivated hybrid; probably exclude), x slavinii 5 (hybrid),
intermedia 2, lecontiana 2. B: +6 from verbatim.

**Vitis rotundifolia** -242 (coords -92). A: 243 to *munsoniana*. +1 verbatim.

**Vitis labrusca** -113 (coords -72, G -3). D: 114 to no list, "Vitis x
labruscana" 98 (cultivated labrusca x vinifera; recommend exclude), latifolia
8, f. labrusca 3, catawba 2 (cultivar). E: "Vitis labrusca Thunb." (21, =
*V. coignetiae*) and "Scop." (1, = *V. vinifera*) stay out; "Vitis vulpina
Bartram" (7) and "Vitis palmata Leconte" (6) stay in, all by the homonym rule.

**Vitis tiliifolia** -87 (coords -23). D: "Vitis caribaea" 84 (a tiliifolia
synonym; add to include cell), arachnoidea 2, acuminata 1.

**Vitis shuttleworthii** -36. D: 36 "Vitis coriacea Shuttlew. ex Planch." The
include cell has only "Vitis candicans var. coriacea"; add "Vitis coriacea".
No coordinates lost.

**Vitis popenoei** -22 (coords -3). D: 22 "Muscadinia popenoei". Add to
include cell.

**Vitis cinerea var. tomentosa** -11 (coords -11), now 0 records. D: all 11
were "Vitis berlandieri var. tomentosa Planch.", reached the concept only via
the backbone. Add the name to the include cell to restore the taxon.

**Vitis cinerea var. cinerea** -8. D: 8 "Vitis aestivalis var. cinerea
Engelm." Add to include cell.

**Vitis acerifolia** -3 (coords -2). D: 3 "Vitis novomexicana". Curator call
(it is an acerifolia synonym in most treatments).

**Vitis aestivalis var. aestivalis** -1. D: 1 "Vitis labrusca var. aestivalis".

## What this means for the sheet

Mechanisms A and B are the fix working as intended and need no sheet change.
Mechanism C is a data-entry fix already tolerated by the code. Mechanism E is
handled in code. Forma autonyms are handled in
code (*rupestris* is now unchanged); variety and subspecies autonyms are not. Mechanism D is the
residue: about a dozen names to add to include cells (largest: riparia subsp. riparia 417 (dropped under
both parsers), bicolor 87, caribaea 84, coriacea 36, popenoei 22, and the
412 rotundifolia var. munsoniana still sitting in rotundifolia).
*palmata*, *californica*, *arizonica* and *rupestris* no longer change.
