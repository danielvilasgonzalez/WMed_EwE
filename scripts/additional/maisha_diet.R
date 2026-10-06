# ============================================================
# MAISHA -> WESTERN MEDITERRANEAN EwE DIET MATRIX
#
# Input:
#   P:/EwE Western Med 2026/data/FG_WMed_2026.csv
#   P:/EwE Western Med 2026/data/diet/MAISHA/MAISHA_DwC-A/
#
# Primary diet measure:
#   Gravimetric percentage
#
# Matching hierarchy:
#   species -> genus -> family -> order -> class -> phylum
#
# If one prey taxon matches multiple FGs:
#   original diet proportion is divided equally among those FGs.
#
# Output:
#   species-level audit
#   unmatched predators
#   unmatched prey
#   FG-level long matrix
#   FG-level wide matrix
#   checks
# ============================================================


# ============================================================
# 1. PACKAGES
# ============================================================

library(tidyverse)
library(readr)
library(janitor)
library(stringr)


# ============================================================
# 2. PATHS
# ============================================================

fg_file <- "P:/EwE Western Med 2026/data/FG_WMed_2026.csv"

maisha_dir <- "P:/EwE Western Med 2026/data/diet/MAISHA/MAISHA_DwC-A"

output_dir <- "P:/EwE Western Med 2026/output/diet/MAISHA"

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# 3. READ FG LIST
# ============================================================

fg <- read_delim(
  fg_file,
  delim = NULL,
  show_col_types = FALSE
) |>
  clean_names() |>
  mutate(
    fg_number = as.integer(fg_number),
    
    species = str_to_lower(str_squish(species)),
    genus   = str_to_lower(str_squish(genus)),
    family  = str_to_lower(str_squish(family)),
    order   = str_to_lower(str_squish(order)),
    class   = str_to_lower(str_squish(class)),
    phylum  = str_to_lower(str_squish(phylum))
  )


# ============================================================
# 4. READ MAISHA
# ============================================================

occurrence <- read_delim(
  file.path(maisha_dir, "occurrence.txt"),
  delim = "\t",
  show_col_types = FALSE
) |>
  clean_names()

emof <- read_delim(
  file.path(maisha_dir, "extendedmeasurementorfact.txt"),
  delim = "\t",
  show_col_types = FALSE
) |>
  clean_names()


# ============================================================
# 5. IDENTIFY PREDATOR RECORDS
# ============================================================

predators <- occurrence |>
  filter(occurrence_remarks == "Predator") |>
  transmute(
    predator_id = id,
    predator_species = str_to_lower(
      str_squish(scientific_name)
    ),
    predator_species_original = scientific_name,
    study_reference = bibliographic_citation,
    reference = references,
    study_year = event_date,
    latitude = decimal_latitude,
    longitude = decimal_longitude,
    associated_occurrences
  )


cat("\nPredator records:", nrow(predators), "\n")


# ============================================================
# 6. RECONSTRUCT PREDATOR -> PREY LINKS
#
# associated_occurrences contains strings such as:
#
# Stomach content: OCC482, OCC483, OCC484...
# ============================================================

predator_prey <- predators |>
  mutate(
    prey_id = str_extract_all(
      associated_occurrences,
      "OCC[0-9]+"
    )
  ) |>
  unnest(prey_id) |>
  distinct(
    predator_id,
    prey_id,
    .keep_all = TRUE
  )


cat("Predator-prey links:", nrow(predator_prey), "\n")


# ============================================================
# 7. JOIN PREY OCCURRENCE RECORDS
# ============================================================

prey_occurrence <- occurrence |>
  transmute(
    prey_id = id,
    
    prey_taxon = scientific_name,
    
    prey_taxon_original = scientific_name,
    
    prey_verbatim = verbatim_identification,
    
    prey_species_id = scientific_name_id,
    
    prey_occurrence_remarks = occurrence_remarks
  )


predator_prey <- predator_prey |>
  left_join(
    prey_occurrence,
    by = "prey_id"
  )


# ============================================================
# 8. LINK MAISHA DIET MEASUREMENTS TO OCCURRENCE RECORDS
# ============================================================

# Identify the occurrence-link column in EMOF
link_candidates <- c(
  "coreid",
  "occurrence_id",
  "core_id",
  "id"
)

link_col <- link_candidates[
  link_candidates %in% names(emof)
][1]

if (is.na(link_col)) {
  stop(
    "Could not identify the occurrence linking column in ",
    "extendedmeasurementorfact.txt.\nAvailable columns are:\n",
    paste(names(emof), collapse = ", ")
  )
}

cat(
  "\nEMOF occurrence linking column:",
  link_col,
  "\n"
)


# Keep only gravimetric diet measurements
emof_diet <- emof |>
  filter(
    measurement_type == "Gravimetric percentage"
  ) |>
  mutate(
    prey_id = .data[[link_col]],
    diet_proportion = as.numeric(measurement_value)
  ) |>
  transmute(
    prey_id,
    diet_proportion,
    measurement_type,
    measurement_unit
  ) |>
  filter(
    !is.na(prey_id),
    !is.na(diet_proportion)
  )


cat(
  "Gravimetric diet records:",
  nrow(emof_diet),
  "\n"
)

cat(
  "Diet proportion range:",
  min(emof_diet$diet_proportion, na.rm = TRUE),
  "to",
  max(emof_diet$diet_proportion, na.rm = TRUE),
  "\n"
)

# ============================================================
# 9. JOIN DIET TO PREDATOR-PREY LINKS
# ============================================================

diet_species <- predator_prey |>
  left_join(
    emof_diet,
    by = "prey_id"
  ) |>
  filter(
    !is.na(diet_proportion),
    !is.na(prey_taxon)
  ) |>
  mutate(
    predator_species = str_to_lower(
      str_squish(predator_species)
    ),
    
    prey_taxon = str_to_lower(
      str_squish(prey_taxon)
    )
  )


cat(
  "\nPredator-prey observations with gravimetric diet:",
  nrow(diet_species),
  "\n"
)


# ============================================================
# 10. MATCH PREDATORS TO EwE FGs
# ============================================================

predator_fg <- fg |>
  filter(!is.na(species), species != "") |>
  select(
    fg_number,
    fg_name,
    species
  ) |>
  distinct()


diet_species <- diet_species |>
  left_join(
    predator_fg,
    by = c(
      "predator_species" = "species"
    )
  ) |>
  rename(
    predator_fg = fg_number,
    predator_fg_name = fg_name
  )


# ============================================================
# 11. SAVE UNMATCHED PREDATORS
# ============================================================

unmatched_predators <- diet_species |>
  filter(is.na(predator_fg)) |>
  distinct(
    predator_species,
    predator_species_original
  ) |>
  arrange(predator_species)


write_csv(
  unmatched_predators,
  file.path(
    output_dir,
    "MAISHA_unmatched_predators.csv"
  )
)


# ============================================================
# 12. KEEP ONLY MATCHED PREDATORS
# ============================================================

diet_species_matched <- diet_species |>
  filter(!is.na(predator_fg))


# ============================================================
# 13. TAXONOMIC MATCHING FUNCTION
# ============================================================

match_prey_to_fg <- function(
    prey_name,
    fg_table
) {
  
  prey_name <- str_to_lower(
    str_squish(prey_name)
  )
  
  # ----------------------------------------------------------
  # Species
  # ----------------------------------------------------------
  
  x <- fg_table |>
    filter(
      !is.na(species),
      species != "",
      species == prey_name
    )
  
  if (nrow(x) > 0) {
    return(
      x |>
        transmute(
          prey_fg = fg_number,
          prey_fg_name = fg_name,
          match_rank = "species"
        ) |>
        distinct()
    )
  }
  
  
  # ----------------------------------------------------------
  # Extract genus from binomial prey name
  # ----------------------------------------------------------
  
  prey_genus <- str_split(
    prey_name,
    "\\s+"
  )[[1]][1]
  
  
  # ----------------------------------------------------------
  # Genus
  # ----------------------------------------------------------
  
  x <- fg_table |>
    filter(
      !is.na(genus),
      genus != "",
      genus == prey_genus
    )
  
  if (nrow(x) > 0) {
    return(
      x |>
        transmute(
          prey_fg = fg_number,
          prey_fg_name = fg_name,
          match_rank = "genus"
        ) |>
        distinct()
    )
  }
  
  
  # ----------------------------------------------------------
  # Higher taxonomy
  #
  # Because MAISHA prey can be identified only to higher
  # taxonomic levels, match the literal taxon against the
  # corresponding FG taxonomic fields.
  # ----------------------------------------------------------
  
  for (rank in c(
    "family",
    "order",
    "class",
    "phylum"
  )) {
    
    x <- fg_table |>
      filter(
        !is.na(.data[[rank]]),
        .data[[rank]] != "",
        .data[[rank]] == prey_name
      )
    
    if (nrow(x) > 0) {
      
      return(
        x |>
          transmute(
            prey_fg = fg_number,
            prey_fg_name = fg_name,
            match_rank = rank
          ) |>
          distinct()
      )
    }
  }
  
  
  # ----------------------------------------------------------
  # No match
  # ----------------------------------------------------------
  
  tibble(
    prey_fg = NA_integer_,
    prey_fg_name = NA_character_,
    match_rank = "unmatched"
  )
}


# ============================================================
# 14. CREATE PREY TAXONOMY LOOKUP
# ============================================================

fg_taxonomy <- fg |>
  select(
    fg_number,
    fg_name,
    species,
    genus,
    family,
    order,
    class,
    phylum
  ) |>
  distinct()


# ============================================================
# 15. MATCH EACH PREY TAXON
# ============================================================

prey_taxa <- diet_species_matched |>
  distinct(
    prey_taxon
  ) |>
  filter(
    !is.na(prey_taxon),
    prey_taxon != ""
  )


prey_matches <- map_dfr(
  prey_taxa$prey_taxon,
  function(x) {
    
    match_prey_to_fg(
      prey_name = x,
      fg_table = fg_taxonomy
    ) |>
      mutate(
        prey_taxon = x,
        .before = 1
      )
  }
)


# ============================================================
# 16. JOIN PREY FG MATCHES
# ============================================================

diet_allocated <- diet_species_matched |>
  left_join(
    prey_matches,
    by = "prey_taxon"
  )


# ============================================================
# 17. ALLOCATE DIET AMONG MULTIPLE FGs
#
# Example:
#
# Decapoda = 0.30
# matches FG 20, FG 21, FG 22
#
# each receives:
#
# 0.30 / 3 = 0.10
#
# Total remains 0.30.
# ============================================================

diet_allocated <- diet_allocated |>
  group_by(
    predator_id,
    prey_id
  ) |>
  mutate(
    n_matching_fg = sum(
      !is.na(prey_fg)
    ),
    
    allocation = case_when(
      !is.na(prey_fg) &
        n_matching_fg > 0 ~
        diet_proportion / n_matching_fg,
      
      TRUE ~ NA_real_
    ),
    
    allocation_method = case_when(
      
      is.na(prey_fg) ~
        "unmatched",
      
      n_matching_fg == 1 &
        match_rank == "species" ~
        "exact_species",
      
      n_matching_fg == 1 &
        match_rank == "genus" ~
        "exact_genus",
      
      n_matching_fg == 1 &
        match_rank == "family" ~
        "exact_family",
      
      n_matching_fg == 1 &
        match_rank == "order" ~
        "exact_order",
      
      n_matching_fg == 1 &
        match_rank == "class" ~
        "exact_class",
      
      n_matching_fg == 1 &
        match_rank == "phylum" ~
        "exact_phylum",
      
      n_matching_fg > 1 ~
        "multi_FG_equal",
      
      TRUE ~
        "unmatched"
    )
  ) |>
  ungroup()


# ============================================================
# 18. SAVE SPECIES-LEVEL AUDIT TABLE
# ============================================================

diet_species_audit <- diet_allocated |>
  select(
    predator_id,
    predator_species_original,
    predator_species,
    predator_fg,
    predator_fg_name,
    
    prey_id,
    prey_taxon_original,
    prey_taxon,
    prey_verbatim,
    
    prey_fg,
    prey_fg_name,
    match_rank,
    
    diet_proportion,
    n_matching_fg,
    allocation,
    allocation_method,
    
    study_year,
    study_reference,
    reference
  )


write_csv(
  diet_species_audit,
  file.path(
    output_dir,
    "MAISHA_species_level_audit.csv"
  )
)


# ============================================================
# 19. UNMATCHED PREY
# ============================================================

unmatched_prey <- diet_species_audit |>
  filter(
    is.na(prey_fg)
  ) |>
  group_by(
    prey_taxon,
    prey_verbatim
  ) |>
  summarise(
    n_records = n(),
    total_diet = sum(
      diet_proportion,
      na.rm = TRUE
    ),
    .groups = "drop"
  ) |>
  arrange(
    desc(total_diet)
  )


write_csv(
  unmatched_prey,
  file.path(
    output_dir,
    "MAISHA_unmatched_prey.csv"
  )
)


# ============================================================
# 20. FG-LEVEL AGGREGATION
#
# First average species/study observations within each
# predator FG.
#
# This avoids a predator FG with many MAISHA observations
# dominating solely because it has more records.
# ============================================================

fg_species_diet <- diet_allocated |>
  filter(
    !is.na(prey_fg),
    !is.na(allocation)
  ) |>
  group_by(
    predator_fg,
    predator_fg_name,
    predator_species,
    prey_fg,
    prey_fg_name
  ) |>
  summarise(
    diet = mean(
      allocation,
      na.rm = TRUE
    ),
    n_observations = n(),
    .groups = "drop"
  )


# ============================================================
# 21. AGGREGATE SPECIES DIETS TO PREDATOR FG
#
# Equal weighting among predator species.
# ============================================================

diet_fg_raw <- fg_species_diet |>
  group_by(
    predator_fg,
    predator_fg_name,
    prey_fg,
    prey_fg_name
  ) |>
  summarise(
    diet_raw = mean(
      diet,
      na.rm = TRUE
    ),
    
    n_predator_species = n_distinct(
      predator_species
    ),
    
    n_observations = sum(
      n_observations
    ),
    
    .groups = "drop"
  )


# ============================================================
# 22. NORMALIZE WITHIN EACH PREDATOR FG
#
# This is important because unmatched prey and different
# study coverage can otherwise make row sums < 1.
# ============================================================

diet_fg <- diet_fg_raw |>
  group_by(
    predator_fg
  ) |>
  mutate(
    Diet = diet_raw / sum(
      diet_raw,
      na.rm = TRUE
    )
  ) |>
  ungroup() |>
  select(
    Predator_FG = predator_fg,
    Predator_FG_name = predator_fg_name,
    Prey_FG = prey_fg,
    Prey_FG_name = prey_fg_name,
    Diet,
    n_predator_species,
    n_observations
  ) |>
  arrange(
    Predator_FG,
    Prey_FG
  )


# ============================================================
# 23. FINAL EwE LONG MATRIX
# ============================================================

diet_matrix_long <- diet_fg |>
  select(
    Predator_FG,
    Prey_FG,
    Diet
  )


write_csv(
  diet_matrix_long,
  file.path(
    output_dir,
    "EwE_diet_matrix_MAISHA.csv"
  )
)


# ============================================================
# 24. WIDE MATRIX
# ============================================================

diet_matrix_wide <- diet_matrix_long |>
  pivot_wider(
    names_from = Prey_FG,
    values_from = Diet,
    values_fill = 0
  ) |>
  arrange(Predator_FG)


write_csv(
  diet_matrix_wide,
  file.path(
    output_dir,
    "EwE_diet_matrix_MAISHA_wide.csv"
  )
)


# ============================================================
# 25. MATRIX CHECKS
# ============================================================

row_checks <- diet_matrix_long |>
  group_by(
    Predator_FG
  ) |>
  summarise(
    row_sum = sum(
      Diet,
      na.rm = TRUE
    ),
    
    n_prey_fg = n(),
    
    .groups = "drop"
  )


write_csv(
  row_checks,
  file.path(
    output_dir,
    "EwE_diet_matrix_MAISHA_checks.csv"
  )
)


# ============================================================
# 26. CHECK OUTPUT
# ============================================================

cat("\n")
cat("============================================\n")
cat("MAISHA EwE DIET MATRIX COMPLETE\n")
cat("============================================\n\n")

cat(
  "Predator FG x Prey FG combinations:",
  nrow(diet_matrix_long),
  "\n"
)

cat(
  "Predator FGs:",
  n_distinct(diet_matrix_long$Predator_FG),
  "\n"
)

cat(
  "Prey FGs:",
  n_distinct(diet_matrix_long$Prey_FG),
  "\n\n"
)

cat("Row-sum check:\n")

print(
  row_checks,
  n = Inf
)


# ============================================================
# 27. SHOW MATRIX
# ============================================================

cat("\n")
cat("============================================\n")
cat("FINAL LONG-FORM MATRIX\n")
cat("============================================\n\n")

print(
  diet_matrix_long,
  n = 100
)

