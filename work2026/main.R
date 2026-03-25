# code specifically developed for addressing reviewers requests

pacman::p_load(dplyr, sf, readr)


# the mutliple buffer comparison -- update dataset to include th CV for all observations

# reproduce the ecogeographic predictions by binding all records from the varitel species
## gather the model dataset
## data/Vitis/Vitis arizonica/run08282025_1k/occurances/allmodelData.csv
## data/Vitis/Vitis arizonica/run08282025_1k/occurances/allmodelData.csv
runVersion <- "run08282025_1k"
speciesData <- read_csv("data/processed_occurrence/model_data20251216.csv")
spList <- unique(speciesData$taxon)

#storage df
df <- data.frame()

for (i in spList) {
  # construct path to data
  files <- list.files(
    paste0("data/Vitis/", i, "/run08282025_1k/"),
    recursive = TRUE,
    full.names = TRUE
  )
  #
  path <- files[grepl("occurances/allmodelData.csv", x = files)]
  if (length(path) > 0) {
    # read in data
    t1 <- read_csv(path) |>
      dplyr::filter(presence == 1) |>
      dplyr::mutate(taxon = i)
    # bind to previous data
    df <- dplyr::bind_rows(df, t1)
  }
}

# select out all variate data
vars <- df |>
  dplyr::filter(grepl("var.", x = df$taxon)) |>
  dplyr::mutate(
    taxon = case_when(
      taxon == "Vitis aestivalis var. aestivalis" ~ "Vitis aestivalis",
      taxon == "Vitis aestivalis var. bicolor" ~ "Vitis aestivalis",
      taxon == "Vitis cinerea var. cinerea" ~ "Vitis cinerea",
      taxon == "Vitis cinerea var. tomentosa" ~ "Vitis cinerea",
    )
  )
# add this back into the previous data
df2 <- dplyr::bind_rows(df, vars)
df2 |> dplyr::group_by(taxon) |> count()
write_csv(df2, "work2026/data/var_collapse_modelData.csv")

# regenerate the material for species with variatals
species <- c("Vitis cinerea", "Vitis aestivalis")
for (i in species) {
  # read in the specific file
  files <- list.files(
    paste0("data/Vitis/", i, "/run08282025_1k/"),
    recursive = TRUE,
    full.names = TRUE
  )
  path <- files[grepl("occurances/allmodelData.csv", x = files)]
  if (length(path) > 0) {
    # read in data - for all background records
    t1 <- read_csv(path) |>
      dplyr::filter(presence == 0)
    # bind to previous data
    df <- dplyr::bind_rows(df, t1)
  }
}
