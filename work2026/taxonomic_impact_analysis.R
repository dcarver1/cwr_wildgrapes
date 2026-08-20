# work2026/taxonomic_impact_analysis.R
# Diagnostic script to evaluate the impact of GBIF's taxonomic backbone
# and synonymy processing across all species in the New World Vitis project.

# Load required libraries
pacman::p_load(dplyr, readr, stringr, purrr, tidyr)

message("Starting GBIF taxonomic impact analysis...")

# -------------------------------------------------------------------------
# Step 1: Load Project Taxonomy Reference
# -------------------------------------------------------------------------
vitis_tax <- read_csv("data/New World Vitis.csv", col_types = cols(.default = "c")) |>
  dplyr::select(
    accepted_name = "Scientific Name",
    synonyms_raw = "Names to include in this concept (Homotypic synonyms)"
  ) |>
  dplyr::filter(!is.na(accepted_name))

# Clean and parse synonyms list
# Create a mapping from synonym name -> accepted name
synonym_map <- list()
for (i in 1:nrow(vitis_tax)) {
  acc <- vitis_tax$accepted_name[i]
  synonym_map[[acc]] <- acc # accepted name maps to itself
  
  syns_str <- vitis_tax$synonyms_raw[i]
  if (!is.na(syns_str) && syns_str != "" && syns_str != "NA") {
    syns_list <- str_split(syns_str, ",\\s*")[[1]]
    for (s in syns_list) {
      if (s != "" && s != "NA") {
        synonym_map[[s]] <- acc
      }
    }
  }
}

# -------------------------------------------------------------------------
# Step 2: Load and Process Raw GBIF Download
# -------------------------------------------------------------------------
gbif_path <- "data/source_data/vitisGBIFDownload_20250721.csv"
message("Reading raw GBIF download (this may take a few seconds)...")
raw_gbif <- read_tsv(gbif_path, col_types = cols(.default = "c"))

message("Analyzing raw GBIF records...")

# Replicate processGBIF() classification logic:
# 1. Map to raw genus, species, infraspecific, rank, and original name
processed_gbif <- raw_gbif |>
  dplyr::select(
    raw_scientificName = "scientificName",
    gbif_species = "species",
    gbif_genus = "genus",
    gbif_infraspecific = "infraspecificEpithet",
    gbif_rank = "taxonRank",
    countryCode = "countryCode"
  ) |>
  # Exclude JPN (Japan) records to avoid confounding with Vitis flexuosa var. rufotomentosa
  dplyr::filter(countryCode != "JP" | is.na(countryCode)) |>
  # Apply the exact taxonomic assignment logic from process_gbif.R
  dplyr::mutate(
    pipeline_taxon = case_when(
      gbif_rank == "SPECIES" ~ gbif_species,
      gbif_rank == "GENUS" ~ gbif_genus,
      gbif_rank == "VARIETY" ~ paste0(gbif_species, " var. ", gbif_infraspecific),
      gbif_rank == "SUBSPECIES" ~ paste0(gbif_species, " subsp. ", gbif_infraspecific),
      TRUE ~ gbif_species
    )
  )

# Clean up raw names to match standard genus + species form
processed_gbif <- processed_gbif |>
  dplyr::mutate(
    # Strip authorship from raw scientific name to extract species name
    clean_raw_name = str_remove(raw_scientificName, "\\s+Small|\\s+Buckley|\\s+Munson|\\s+House|\\s+Planch\\.|\\s+ex\\s+Planch\\.|\\s+Michx\\.|\\s+L\\.|\\s+Simpson.*") |>
      str_trim()
  )

# -------------------------------------------------------------------------
# Step 3: Analyze Preservation, Lumping, Dropping, and Inflation
# -------------------------------------------------------------------------
# Define a helper function to determine what accepted species a name belongs to (using synonym_map)
get_accepted_species <- function(name) {
  if (is.null(name) || is.na(name) || name == "") return(NA_character_)
  # Try exact match
  if (name %in% names(synonym_map)) {
    return(synonym_map[[name]])
  }
  # Try fuzzy/partial match without author
  cleaned <- str_remove(name, "\\s+var\\..*|\\s+subsp\\..*") |> str_trim()
  if (cleaned %in% names(synonym_map)) {
    return(synonym_map[[cleaned]])
  }
  return(NA_character_)
}

processed_gbif <- processed_gbif |>
  dplyr::mutate(
    raw_accepted = sapply(clean_raw_name, get_accepted_species),
    pipeline_accepted = sapply(pipeline_taxon, get_accepted_species)
  )

# -------------------------------------------------------------------------
# Step 4: Summarize Results for Each Species
# -------------------------------------------------------------------------
summary_table <- vitis_tax |>
  dplyr::mutate(
    # 1. Total records matching this concept originally (based on raw name)
    original_raw_count = sapply(accepted_name, function(sp) {
      sum(processed_gbif$raw_accepted == sp, na.rm = TRUE)
    }),
    
    # 2. Preserved: Raw belongs to Sp AND Pipeline belongs to Sp
    preserved_count = sapply(accepted_name, function(sp) {
      sum(processed_gbif$raw_accepted == sp & !is.na(processed_gbif$pipeline_accepted) & processed_gbif$pipeline_accepted == sp, na.rm = TRUE)
    }),
    
    # 3. Lumped/Lost: Raw belongs to Sp but Pipeline belongs to a DIFFERENT accepted species
    lumped_lost_count = sapply(accepted_name, function(sp) {
      sum(processed_gbif$raw_accepted == sp & !is.na(processed_gbif$pipeline_accepted) & processed_gbif$pipeline_accepted != sp, na.rm = TRUE)
    }),
    
    # 4. Gained/Absorbed: Raw belongs to a different species, but Pipeline lumped it into Sp
    gained_absorbed_count = sapply(accepted_name, function(sp) {
      sum((is.na(processed_gbif$raw_accepted) | processed_gbif$raw_accepted != sp) & !is.na(processed_gbif$pipeline_accepted) & processed_gbif$pipeline_accepted == sp, na.rm = TRUE)
    }),
    
    # 5. Dropped entirely: Raw belongs to Sp, but pipeline taxon is NA_accepted (not in our taxonomy list)
    dropped_entirely_count = sapply(accepted_name, function(sp) {
      sum(processed_gbif$raw_accepted == sp & is.na(processed_gbif$pipeline_accepted), na.rm = TRUE)
    }),
    
    # 6. Final pipeline count before deduplication
    final_pipeline_count = preserved_count + gained_absorbed_count,
    
    # 7. Net Change
    net_change = final_pipeline_count - original_raw_count
  )

# Output summary table to console
options(width = 150)
print(as.data.frame(summary_table %>% 
                      dplyr::select(accepted_name, original_raw_count, preserved_count, lumped_lost_count, gained_absorbed_count, dropped_entirely_count, final_pipeline_count, net_change) %>%
                      dplyr::filter(original_raw_count > 0 | final_pipeline_count > 0) %>%
                      dplyr::arrange(desc(original_raw_count))))

# Write full summary table to CSV
write_csv(summary_table, "work2026/gbif_taxonomic_impact_summary.csv")
message("\nFull summary saved to work2026/gbif_taxonomic_impact_summary.csv")
