# cwr_wildgrapes

Conservation gap analysis for the wild grapevines (*Vitis* L.) of the Americas: occurrence data preparation, species distribution models and ex situ / in situ conservation scores. An update of the 2020 PNAS crop wild relatives of the USA workflow.

The `main` branch holds the method used for the published analysis, with the corrected taxonomy.

## Running the workflow

1. **Taxonomy**: taxon concepts and synonyms are maintained in the "New World Vitis" tab of the project taxon sheet on Google Drive. Edit names there, not in the code.
2. **Preprocessing**: `preprocessing/preprocessingUpdates2026_08_20.R` pulls the current sheet and builds the occurrence dataset used for modelling.
3. **Modelling**: `run_round2_20260916.R` runs the models and gap analysis for each taxon.

To run a single taxon:

```
Rscript -e 'speciesToRun <- "Vitis nesbittiana"; overwrite <- TRUE; source("run_round2_20260916.R")'
```

## Folder structure

**data**: inputs and outputs, including the results for each taxon and run. Not stored in git.

**preprocessing**: scripts and functions that prepare occurrence records from the source datasets.

**R2**: functions for modelling, gap analysis and the per-taxon summary documents.

**taxonomy**: dated snapshots of the taxonomy sheet and a log of changes to it.

**published**: scripts and tables from the published analysis, kept as a record.

**work2026**: analysis supporting the 2026 taxonomy update, including the taxonomic review.

**utilities**: helper scripts for moving outputs.

## Branches

`main` is the accepted workflow. `workflow-evaluation` holds exploratory method testing and is not part of the published analysis.
