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
df_update <- read_csv("work2026/data/var_collapse_modelData.csv") |>
  dplyr::filter(grepl("Vitis cinerea|Vitis aestivalis", x = taxon))
species <- c("Vitis cinerea", "Vitis aestivalis")
for (i in species) {
  # read in the specific file for species and variatals

  path <- files[grepl("occurances/allmodelData.csv", x = files)]
  if (length(path) > 0) {
    # read in data - for all background records
    t1 <- read_csv(path)
    t2 <- t1 |>
      dplyr::filter(presence == 0)
    # extract the presence records for the specific species.
    df_u <- df_update |>
      dplyr::filter(taxon == i)
    # combind the background and presence records
    combined <- dplyr::bind_rows(df_u, t2)
    # test if the new data has more records than the original data
    if (nrow(combined) > nrow(t1)) {
      print(paste0(
        "The new data for ",
        i,
        " has more records than the original data. Updating the file."
      ))
    } else {
      "Something wrong"
    }
    # export results
    write_csv(
      combined,
      paste0(
        "data/Vitis/",
        i,
        "/run08282025_1k/occurances/allmodelData_update.csv"
      )
    )
  }
}

# regenerate the spatial data object for the species
species <- c("Vitis cinerea", "Vitis aestivalis")
for (i in species) {
  spList2 <- spList[grepl(i, x = spList)]
  varSp <- spList2[grepl("var.", x = spList2)]
  # read in file for the primary species
  primary <- st_read(paste0(
    "data/Vitis/",
    i,
    "/run08282025_1k/occurances/spatialData.gpkg"
  ))
  # read in the variate data for the species
  for (j in varSp) {
    # read in an object
    p2 <- st_read(paste0(
      "data/Vitis/",
      j,
      "/run08282025_1k/occurances/spatialData.gpkg"
    ))
    dim(p2)
    # rename the taxon column
    p2$taxon <- i
    # bind it to the primary
    primary <- bind_rows(primary, p2)
  }
  # write over the original species file
  st_write(
    primary,
    paste0(
      "data/Vitis/",
      i,
      "/run08282025_1k/occurances/spatialData.gpkg"
    ),
    delete_layer = TRUE
  )
}

# from here we return to run_all072025.R and rerun the information for these specific species

# adding new stuff does it show up in git
