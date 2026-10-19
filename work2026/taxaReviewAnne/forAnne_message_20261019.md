# Draft message to Anne (19 Oct 2026)

Attachment: `forAnne_outOfRangeRecords_20261019.csv` (one row per record outside the range used for its taxon, with
institution, year, state and whether it reaches the model).

---

Anne,

Thank you for the completed review. We have gone through every decision in columns B and C and are applying them as follows.
Nothing below is waiting on you; we will re-run with these settings and adjust any taxon where your answer changes a record.

**1. Synonyms you asked us to keep.** All 22 names go back on the include lists, including the "counterpart" placements
(helleri to V. berlandieri; virginiana to V. baileyana; austrina and sola to V. simpsonii; V. aestivalis var. cinerea to
V. cinerea; canescens to V. cinerea var. cinerea). V. bicolor goes to var. bicolor and V. argentifolia moves to species
level, as you suggested. V. odoratissima stays out (no coordinates). Two details you may want to know:

- *Vitis caribaea* brings 204 records to V. tiliifolia. 53 of them carry the author string "Hook. & Arn." rather than
  "DC." (a later homonym GBIF cannot file as a synonym). They are 1840 to 1941 herbarium sheets from Costa Rica, Colombia,
  Mexico, Guatemala and the Caribbean, the same geography as the DC. material, so we are treating them as the same plant.
  Say if you disagree.
- *Vitis obovata*: one GBIF record is "Vitis obovata Baker", described from Brazil (Flora Brasiliensis, 1871), with no
  locality. We are excluding that one as a different plant. The other three, "Vitis obovata Raf.", are genebank entries
  (Bioversity) with origin Afghanistan, India and Yunnan, and no coordinates. They cannot be labrusca in the wild. Our
  proposal, as an example of how we handle this case: keep them on the V. labrusca record count (they affect no map), and
  drop them only if you tell us the name is misapplied. Which do you prefer?

**2. Geography filter on added and elevated taxa.** The model filters each taxon's US, Mexican and Canadian points to a
state list. The published run took those lists from FNA, so the taxa FNA does not treat as species had no list. We have
added lists as follows and would like your check on the last two:

| Taxon | States | Source |
|---|---|---|
| V. rufotomentosa | FL, GA, SC, NC | your Weakley summary ("Florida to se. North Carolina") |
| V. lincecumii | AR, OK, LA, TX | your Weakley summary |
| V. munsoniana | AL, FL, GA | FNA's entry for V. rotundifolia var. munsoniana |
| V. simpsonii | AL, AR, FL, GA, MS, NC, SC, VA | NatureServe. **Please check against Weakley.** |
| V. baileyana | AL, AR, GA, IN, KY, NC, OH, PA, SC, TN, VA, WV | NatureServe. **Please check against Weakley.** |

V. berlandieri keeps no list, as in the published analysis, because its range is mostly Mexican and a US-only list would
drop those points.

**3. Records the filter removes (attached csv).** The ones that matter are the points that would otherwise enter a model:

- *V. rufotomentosa*: five 2023 specimens from Maryland (NCU, TAWES) and one 2024 iNaturalist observation from Alabama.
  All fall outside Florida to North Carolina and will be dropped. You wrote "perhaps more widespread": if Maryland is
  plausible, tell us and we will widen the list. Three Texas and one Louisiana specimen (BRIT 1934, 1960; GA; COLO) carry
  no coordinates, so they only count toward the record total; they sit inside the lincecumii range, which you asked us to
  keep separate. Should they stay on the count?
- *V. lincecumii*: two Kansas points (UAM 1994, MNHN 1922) are dropped. Missouri and Illinois specimens (US, APCR) have no
  coordinates.
- *V. berlandieri*: one Oklahoma point (NY 1935) and one Missouri point (MO 1906) stay in the model, since this taxon has
  no list. Say if either should go.
- *V. baileyana*: one Maryland point (MARY 2015) and five Mississippi points (MISS, MMNS, USMS, 1969 to 2013) are dropped
  under the NatureServe list. If Weakley gives Mississippi for baileyana, the list changes.
- *V. simpsonii* and *V. munsoniana*: 31 and 22 UC Davis vouchers of vineyard plants (Olmo, 1958, georeferenced to the
  Davis vineyard) are dropped, plus three 1890 Texas points (NY) for munsoniana, one Louisiana (LSU 2010) and one 2024
  District of Columbia point (US) for simpsonii.

**4. Your questions on the removed rows.**

- V. girdiana and V. riparia: the two records you saw removed with the name unchanged are coordinate errors (longitude
  -177 and latitude 82) that the published run also removed by hand. Not taxonomy.
- V. aestivalis var. aestivalis: yes, the one V. labrusca var. aestivalis record was the same record under the species.
- V. martineziana: the two additions are not GBIF records; one is from Jun Wen and one from the Huerta-Acosta paper.
- V. tiliifolia: the 245 additions were in GBIF under the right name but the published name-matching could not read them
  (235), or carried the name only in the publisher's verbatim field (10).
- The hybrid additions (x champinii, x doaniana, x novae-angliae) are genebank accessions republished on GBIF by the global
  crop wild relative database; the name was in the verbatim field. We are keeping them as you agreed, and noting the data
  source in the paper.

Dan
