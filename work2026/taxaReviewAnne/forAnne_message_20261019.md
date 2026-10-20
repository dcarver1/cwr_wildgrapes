# Draft message to Anne (drafted 19 Oct 2026, updated 20 Oct after the final runs)

Attachment: `forAnne_outOfRangeRecords_20261019.csv` (one row per record outside the range used for its taxon, with
institution, year, state and whether it reaches the model).

---

Anne,

Thank you for the completed review. We have gone through every decision in columns B and C and are applying them as follows.
Nothing below is waiting on you: the models were re-run with these settings on 19 and 20 October, and we will adjust any
taxon where your answer changes a record.

**1. Synonyms you asked us to keep.** All 22 names go back on the include lists, including the "counterpart" placements
(helleri to V. berlandieri; virginiana to V. baileyana; austrina and sola to V. simpsonii; V. aestivalis var. cinerea to
V. cinerea; canescens to V. cinerea var. cinerea). V. bicolor goes to var. bicolor; the 9 records named V. argentifolia
move to species level, as you suggested, while the 400 named V. aestivalis var. argentifolia stay with var. bicolor as in
the published analysis. V. odoratissima stays out (no coordinates). Two details you may want to know:

- *Vitis caribaea* brings 204 records to V. tiliifolia. 53 of them carry the author string "Hook. & Arn." rather than
  "DC." (a later homonym GBIF cannot file as a synonym). They are 1840 to 1941 herbarium sheets from Costa Rica, Colombia,
  Mexico, Guatemala and the Caribbean, the same geography as the DC. material, so we are treating them as the same plant.
  Say if you disagree.
- *Vitis obovata*: all four GBIF records under this name are currently excluded from V. labrusca. One is "Vitis obovata
  Baker", described from Brazil (Flora Brasiliensis, 1871) with no locality, a different plant. The other three, "Vitis
  obovata Raf.", are genebank entries (Bioversity) with origin Afghanistan, India and Yunnan, and no coordinates, so they
  cannot be labrusca in the wild. None of the four touches a map or a score. As an example of the two ways we can handle
  this: (a) leave all four out, as now; or (b) return the three Raf. records to the V. labrusca record count because the
  name is a labrusca synonym, and drop only the Baker one. Which do you prefer?

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

**3. Points the filter removed from the models (attached csv lists every record outside its taxon's range).** Measured
against the previous run of each taxon:

- *V. rufotomentosa* (18 points to 14): three 2023 specimens from Maryland (NCU) and one 2024 iNaturalist observation from
  Alabama, all outside Florida to North Carolina. You wrote "perhaps more widespread": if Maryland is plausible, tell us
  and we will widen the list. Texas and Louisiana specimens (BRIT 1934, 1960; GA; COLO) carry no coordinates, so they only
  count toward the record total; they sit inside the lincecumii range, which you asked us to keep separate. Should they
  stay on the count?
- *V. lincecumii* (85 to 75): two Kansas points (UAM 1994, MNHN 1922), one New Mexico (UNM 1988), and seven USDA genebank
  accessions georeferenced to their collection site: two in Missouri, one in Florida, and four in Mexico (Coahuila 2,
  Nuevo León 2). Neither Weakley nor NatureServe gives the taxon a Mexican range, so those four are out. Say if you think
  lincecumii does reach Coahuila and Nuevo León.
- *V. berlandieri*: one Oklahoma point (NY 1935) and one Missouri point (MO 1906) stay in the model, since this taxon has
  no list. Say if either should go.
- *V. baileyana* (139 to 133): two Maryland points (MARY 1965, 2015) and four Mississippi points (MISS 1969, MMNS 2008 and
  2013, USMS 2003) are dropped under the NatureServe list. If Weakley gives Mississippi for baileyana, the list changes.
- *V. simpsonii* (356 to 339): the UC Davis vineyard vouchers (Olmo, 1958; 31 records at one coordinate), one 2024
  District of Columbia point (US), six Louisiana points (LSU 2010, UAM and US 2014), seven Maryland points (NCU 2013, US
  1949 and 1951, two Bioversity accessions), one Missouri (US 2016) and three Texas (IND 1903, US 2014, one USDA
  accession).
- *V. munsoniana* (70 to 68): the Davis vineyard vouchers (22 records at one coordinate) and one 1890 Texas point (NY).

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
