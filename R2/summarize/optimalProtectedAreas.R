## trying some AI formation for documentation and more consistent in object names

# 0. Setup ------------------------------------------------------------------
# Load required packages using pacman
pacman::p_load(dplyr, readr, tidyr, purrr, stringr, tibble)


# 1. Functions --------------------------------------------------------------

#' Structure long species data into a wide summary format
#'
#' This function takes a long data frame of species observations (one row per
#' species per protected area) and transforms it into a wide data frame (one
#' row per protected area) with summary columns.
#'
#' @param df A data frame with columns `name`, `taxon`, and `count`.
#' @return A tibble where each row is a unique `name`, with columns for
#'         `Unique_Species_Count`, `Species_List` (comma-separated string),
#'         `Total_Count`, and one column for each unique `taxon`.
structureData <- function(df) {
  df %>%
    # Group by protected area 'name' to create summaries
    group_by(name) %>%
    mutate(
      # Count unique species for this 'name'
      Unique_Species_Count = n_distinct(taxon),
      # Create a single string of all unique species
      Species_List = toString(unique(taxon))
    ) %>%
    ungroup() %>%

    # Group again to handle cases where one 'name' might have multiple
    # rows for the *same* 'taxon'. We sum the counts.
    group_by(name, taxon, Unique_Species_Count, Species_List) %>%
    summarise(count = sum(count, na.rm = TRUE), .groups = 'drop') %>%

    # Pivot the data from long to wide
    pivot_wider(
      id_cols = c(name, Unique_Species_Count, Species_List),
      names_from = taxon,
      values_from = count,
      values_fill = 0
    ) %>%

    # Create a grand total of all counts for each 'name'
    mutate(
      Total_Count = rowSums(across(
        -c(name, Unique_Species_Count, Species_List)
      ))
    ) %>%
    relocate(Total_Count, .after = last_col())
}


# 2. Data Loading and Preparation -------------------------------------------

# Load metadata for protected areas (e.g., richness, names)
pa_richness_summary <- read_csv(
  "data/Vitis/run08282025_1k/protectedAreaSpeciesRichness.csv"
)

# Load and combine all species point data from the 'proPoints' directory.
# read_csv() can accept a vector of file paths and will row-bind them.
all_pa_species_long <- list.files(
  "data/Vitis/run08282025_1k/proPoints",
  full.names = TRUE
) |>
  read_csv() |>
  dplyr::filter(count > 0) # Keep only records with at least one observation

# Get a complete list of all unique species found across all areas
master_species_list <- unique(all_pa_species_long$taxon)

# Process the long data into the wide summary format (one row per PA)
all_pa_species_wide <- all_pa_species_long |>
  structureData()

# Export this intermediate summary file
write_csv(
  all_pa_species_wide,
  "data/Vitis/run08282025_1k/pointsInProSummary.csv"
)


# 3. Optimal Protected Area Selection (Greedy Algorithm) --------------------

# This loop implements a "greedy" heuristic algorithm to find a minimal
# set of protected areas that "cover" all species. It iteratively
# selects the protected area that adds the *most* new, uncovered species.

# Note: The loop is set to run once (i in 1).
# To run multiple iterations (e.g., to test stability due to random
# tie-breaking), change '1' to '1:100' or your desired number.
for (i in 1:20) {
  set.seed(i)

  # Initialize/reset the list of species that still need to be "covered"
  uncovered_species_list <- master_species_list

  # This list will store the final set of selected PAs and their species
  selected_pa_species <- list()

  # Create a working copy of the PA data for this loop iteration
  working_pa_data <- all_pa_species_wide

  while (length(uncovered_species_list) > 0) {
    # 1. Select the PA with the most *remaining* uncovered species
    best_pa_row <- working_pa_data |>
      dplyr::filter(Unique_Species_Count == max(Unique_Species_Count))

    # 2. Handle ties: if multiple PAs have the same max count, pick one
    if (nrow(best_pa_row) > 1) {
      best_pa_row <- dplyr::slice_sample(best_pa_row, n = 1)
    }

    # 3. Get the list of species covered by this selected PA
    #    (These are species that were, until now, "uncovered")
    newly_covered_species <- stringr::str_split(
      best_pa_row$Species_List,
      pattern = ", "
    ) |>
      unlist()

    # 4. Store this PA's name and the species it "contributed"
    selected_pa_species[[as.character(
      best_pa_row$name
    )]] <- newly_covered_species

    # 5. Update the master list, removing the species we just covered
    uncovered_species_list <- uncovered_species_list[
      !uncovered_species_list %in% newly_covered_species
    ]

    # 6. Update the working data for the next loop iteration
    working_pa_data <- working_pa_data |>
      mutate(
        # Remove the newly covered species from all PAs' Species_List
        Species_List = map_chr(
          Species_List,
          ~ .x |>
            stringr::str_split_1(", ") |>
            setdiff(newly_covered_species) |>
            toString()
        ),
        # Recalculate the count of *remaining* unique species
        Unique_Species_Count = ifelse(
          Species_List == "",
          0,
          str_count(Species_List, ", ") + 1
        )
      ) |>
      # Remove PAs that no longer contain any uncovered species
      dplyr::filter(Unique_Species_Count > 0)
  }

  print(paste0(
    "Seed ",
    i,
    " finished. Selected ",
    length(selected_pa_species),
    " areas."
  ))
  # To track lengths over multiple runs, re-add:
  # run_lengths <- c(run_lengths, length(selected_pa_species))
}


# 4. Post-Processing and Report Generation ----------------------------------

# Convert the named list of results into a tibble
# 'ID' column = PA name, 'Species_List' column = list-column of species
selected_pa_species_df <- enframe(
  selected_pa_species,
  name = "ID",
  value = "Species_List"
)

# Join the results with the original PA metadata
final_optimal_pa_report <- selected_pa_species_df |>
  # Join with the PA metadata, ensuring ID is character for the join
  left_join(
    pa_richness_summary |> mutate(ID = as.character(WDPAID)),
    by = "ID"
  ) |>
  # Calculate the number of *observed* species this PA contributed
  mutate(
    totalObservedSpecies = lengths(Species_List)
  ) |>
  # Expand the list-column to one row per species
  unnest(Species_List) |>
  # Select, rename, and reorder final columns for the report
  select(
    "WDPAID",
    "NAME",
    "DESIG_ENG",
    "ISO3",
    totalPredictedSpecies = "totalSpecies",
    observedSpecies = "Species_List",
    totalOservedSpecies = "totalObservedSpecies"
  )


# 5. Export Final Report ----------------------------------------------------

write_csv(
  final_optimal_pa_report,
  file = "data/Vitis/run08282025_1k/topProArea.csv"
)
