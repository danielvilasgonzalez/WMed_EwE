# ============================================================
# FISHBASE DIET MATRIX FOR WESTERN MEDITERRANEAN EwE
#
# Multi-FG prey allocation:
# one prey item can match multiple FGs.
#
# Matching hierarchy:
#   1. species
#   2. genus
#   3. family
#   4. order
#   5. class
#   6. phylum
#
# If multiple FGs are matched, diet is divided equally.
# The original diet contribution is always conserved.
# ============================================================


# ============================================================
# 0. PACKAGES
# ============================================================

packages <- c(
  "tidyverse",
  "readr",
  "janitor",
  "rfishbase"
)

new_packages <- packages[
  !packages %in% rownames(installed.packages())
]

if (length(new_packages) > 0) {
  install.packages(new_packages)
}

library(tidyverse)
library(readr)
library(janitor)
library(rfishbase)


# ============================================================
# 1. PATHS
# ============================================================

fg_file <- "P:/EwE Western Med 2026/data/FG_WMed_2026.csv"

output_dir <- "P:/EwE Western Med 2026/output/diet/FishBase"

dir.create(
  output_dir,
  recursive = TRUE,
  showWarnings = FALSE
)


# ============================================================
# 2. READ FG LIST
# ============================================================

# Automatically detect delimiter
first_line <- readLines(
  fg_file,
  n = 1,
  encoding = "UTF-8"
)

delimiter <- if (grepl("\t", first_line)) {
  "\t"
} else if (grepl(";", first_line)) {
  ";"
} else {
  ","
}

fg_raw <- read_delim(
  fg_file,
  delim = delimiter,
  show_col_types = FALSE,
  locale = locale(encoding = "UTF-8")
) |>
  clean_names()


# Check columns
required_cols <- c(
  "fg_number",
  "fg_name",
  "species",
  "genus",
  "family",
  "order",
  "class",
  "phylum"
)

missing_cols <- setdiff(
  required_cols,
  names(fg_raw)
)

if (length(missing_cols) > 0) {
  stop(
    "Missing columns in FG file: ",
    paste(missing_cols, collapse = ", ")
  )
}


# ============================================================
# 3. STANDARDIZE TAXONOMY
# ============================================================

tax_cols <- c(
  "species",
  "genus",
  "family",
  "order",
  "class",
  "phylum"
)

fg_tax <- fg_raw |>
  mutate(
    across(
      all_of(tax_cols),
      ~ str_squish(
        str_to_lower(
          as.character(.x)
        )
      )
    )
  ) |>
  mutate(
    across(
      all_of(tax_cols),
      ~ na_if(.x, "")
    )
  )


# One row per species/FG
fg_species <- fg_tax |>
  filter(
    !is.na(species),
    !is.na(fg_number)
  ) |>
  distinct(
    fg_number,
    fg_name,
    species,
    genus,
    family,
    order,
    class,
    phylum
  )


cat(
  "FGs:",
  n_distinct(fg_species$fg_number),
  "\n"
)

cat(
  "Species:",
  n_distinct(fg_species$species),
  "\n"
)


# ============================================================
# 4. FISHBASE DIET EXTRACTION
# ============================================================

predators <- sort(
  unique(fg_species$species)
)

cat(
  "Getting FishBase diet data for",
  length(predators),
  "species...\n"
)


get_fishbase_diet <- function(sp) {
  
  out <- tryCatch(
    {
      
      x <- rfishbase::ecology(sp)
      
      if (is.null(x) || nrow(x) == 0) {
        return(NULL)
      }
      
      x <- as_tibble(x)
      
      x$predator_species <- sp
      
      x
      
    },
    error = function(e) {
      NULL
    }
  )
  
  out
}


fishbase_raw_list <- lapply(
  predators,
  get_fishbase_diet
)

fishbase_raw <- bind_rows(
  fishbase_raw_list
)


if (nrow(fishbase_raw) == 0) {
  
  stop(
    "No FishBase ecology/diet records were returned."
  )
  
}


# Save raw data before processing
write_csv(
  fishbase_raw,
  file.path(
    output_dir,
    "FishBase_ecology_raw.csv"
  )
)


# ============================================================
# 5. IDENTIFY PREY AND DIET COLUMNS
# ============================================================

fishbase_names <- names(fishbase_raw)


prey_candidates <- fishbase_names[
  grepl(
    "food|prey|item",
    fishbase_names,
    ignore.case = TRUE
  )
]

diet_candidates <- fishbase_names[
  grepl(
    "percent|diet|weight|volume|proportion|fraction",
    fishbase_names,
    ignore.case = TRUE
  )
]


cat("\nPossible prey columns:\n")
print(prey_candidates)

cat("\nPossible diet columns:\n")
print(diet_candidates)


# The first candidate is used.
# The raw table is saved above so this can be checked.
if (length(prey_candidates) == 0) {
  stop("Could not identify a FishBase prey column.")
}

if (length(diet_candidates) == 0) {
  stop("Could not identify a FishBase diet contribution column.")
}

prey_col <- prey_candidates[1]
diet_col <- diet_candidates[1]

cat(
  "\nUsing prey column:",
  prey_col,
  "\n"
)

cat(
  "Using diet column:",
  diet_col,
  "\n"
)


# ============================================================
# 6. STANDARDIZE FISHBASE DIET DATA
# ============================================================

diet_species <- fishbase_raw |>
  
  transmute(
    
    predator_species,
    
    prey_original = as.character(
      .data[[prey_col]]
    ),
    
    diet_raw = suppressWarnings(
      as.numeric(
        .data[[diet_col]]
      )
    )
    
  ) |>
  
  mutate(
    
    predator_species = str_squish(
      str_to_lower(predator_species)
    ),
    
    prey_original = str_squish(
      str_to_lower(prey_original)
    )
    
  ) |>
  
  filter(
    
    !is.na(predator_species),
    !is.na(prey_original),
    prey_original != "",
    !is.na(diet_raw),
    diet_raw > 0
    
  )


# ============================================================
# 7. CONVERT DIET VALUES TO PROPORTIONS
# ============================================================

# FishBase records may be percentages or proportions.
#
# If the maximum is > 1, treat values as percentages.
# Otherwise treat them as proportions.

diet_species <- diet_species |>
  
  group_by(predator_species) |>
  
  mutate(
    
    diet_proportion_raw = case_when(
      
      max(diet_raw, na.rm = TRUE) > 1 ~
        diet_raw / 100,
      
      TRUE ~
        diet_raw
      
    )
    
  ) |>
  
  ungroup()


# Normalize within predator species
diet_species <- diet_species |>
  
  group_by(predator_species) |>
  
  mutate(
    
    diet_proportion =
      diet_proportion_raw /
      sum(
        diet_proportion_raw,
        na.rm = TRUE
      )
    
  ) |>
  
  ungroup()


# ============================================================
# 8. ADD PREDATOR FG
# ============================================================

diet_species <- diet_species |>
  
  left_join(
    
    fg_species |>
      select(
        fg_number,
        fg_name,
        species
      ) |>
      distinct(),
    
    by = c(
      "predator_species" = "species"
    )
    
  ) |>
  
  rename(
    predator_fg = fg_number,
    predator_fg_name = fg_name
  )


# Save unmatched predators
unmatched_predators <- diet_species |>
  filter(
    is.na(predator_fg)
  ) |>
  distinct(
    predator_species
  )


write_csv(
  unmatched_predators,
  file.path(
    output_dir,
    "unmatched_predators.csv"
  )
)


diet_species <- diet_species |>
  filter(
    !is.na(predator_fg)
  )


# ============================================================
# 9. MATCH PREY TO MULTIPLE FGs
# ============================================================

# Extract taxonomic words from the prey description.
#
# For exact species matching, the full prey string is used.
# For broader groups, we subsequently use genus/family/etc.
#
# First try species.
# Then genus.
# Then family.
# Then order.
# Then class.
# Then phylum.

match_prey_to_fg <- function(prey_name) {
  
  prey_name <- str_squish(
    str_to_lower(prey_name)
  )
  
  # ----------------------------------------------------------
  # SPECIES
  # ----------------------------------------------------------
  
  m <- fg_species |>
    filter(
      species == prey_name
    ) |>
    distinct(
      fg_number,
      fg_name
    )
  
  if (nrow(m) > 0) {
    
    return(
      m |>
        mutate(
          match_rank = "species"
        )
    )
    
  }
  
  
  # ----------------------------------------------------------
  # GENUS
  # ----------------------------------------------------------
  
  m <- fg_species |>
    filter(
      genus == prey_name
    ) |>
    distinct(
      fg_number,
      fg_name
    )
  
  if (nrow(m) > 0) {
    
    return(
      m |>
        mutate(
          match_rank = "genus"
        )
    )
    
  }
  
  
  # ----------------------------------------------------------
  # FAMILY
  # ----------------------------------------------------------
  
  m <- fg_species |>
    filter(
      family == prey_name
    ) |>
    distinct(
      fg_number,
      fg_name
    )
  
  if (nrow(m) > 0) {
    
    return(
      m |>
        mutate(
          match_rank = "family"
        )
    )
    
  }
  
  
  # ----------------------------------------------------------
  # ORDER
  # ----------------------------------------------------------
  
  m <- fg_species |>
    filter(
      order == prey_name
    ) |>
    distinct(
      fg_number,
      fg_name
    )
  
  if (nrow(m) > 0) {
    
    return(
      m |>
        mutate(
          match_rank = "order"
        )
    )
    
  }
  
  
  # ----------------------------------------------------------
  # CLASS
  # ----------------------------------------------------------
  
  m <- fg_species |>
    filter(
      class == prey_name
    ) |>
    distinct(
      fg_number,
      fg_name
    )
  
  if (nrow(m) > 0) {
    
    return(
      m |>
        mutate(
          match_rank = "class"
        )
    )
    
  }
  
  
  # ----------------------------------------------------------
  # PHYLUM
  # ----------------------------------------------------------
  
  m <- fg_species |>
    filter(
      phylum == prey_name
    ) |>
    distinct(
      fg_number,
      fg_name
    )
  
  if (nrow(m) > 0) {
    
    return(
      m |>
        mutate(
          match_rank = "phylum"
        )
    )
    
  }
  
  
  # ----------------------------------------------------------
  # NO MATCH
  # ----------------------------------------------------------
  
  tibble()
  
}


# ============================================================
# 10. APPLY MULTI-FG MATCHING
# ============================================================

prey_matches <- lapply(
  
  seq_len(nrow(diet_species)),
  
  function(i) {
    
    m <- match_prey_to_fg(
      diet_species$prey_original[i]
    )
    
    if (nrow(m) == 0) {
      
      return(
        tibble(
          row_id = i,
          fg_number = NA_real_,
          fg_name = NA_character_,
          match_rank = NA_character_
        )
      )
      
    }
    
    m |>
      mutate(
        row_id = i
      ) |>
      select(
        row_id,
        fg_number,
        fg_name,
        match_rank
      )
    
  }
  
) |>
  bind_rows()


# ============================================================
# 11. ALLOCATE DIET AMONG MATCHING FGs
# ============================================================

diet_allocated <- prey_matches |>
  
  left_join(
    
    diet_species |>
      mutate(
        row_id = row_number()
      ) |>
      select(
        row_id,
        predator_species,
        predator_fg,
        predator_fg_name,
        prey_original,
        diet_proportion
      ),
    
    by = "row_id"
    
  ) |>
  
  group_by(row_id) |>
  
  mutate(
    
    n_matching_fgs =
      sum(!is.na(fg_number)),
    
    allocation =
      if_else(
        !is.na(fg_number),
        1 / n_matching_fgs,
        NA_real_
      ),
    
    diet_allocated =
      diet_proportion * allocation
    
  ) |>
  
  ungroup()


# ============================================================
# 12. UNMATCHED PREY
# ============================================================

unmatched_prey <- diet_allocated |>
  
  filter(
    is.na(fg_number)
  ) |>
  
  select(
    predator_species,
    predator_fg,
    predator_fg_name,
    prey_original,
    diet_proportion,
    match_rank
  ) |>
  
  distinct()


write_csv(
  unmatched_prey,
  file.path(
    output_dir,
    "unmatched_prey.csv"
  )
)


# ============================================================
# 13. SPECIES-LEVEL AUDIT TABLE
# ============================================================

write_csv(
  diet_allocated,
  file.path(
    output_dir,
    "diet_species_FishBase_allocated.csv"
  )
)


# ============================================================
# 14. AGGREGATE TO EwE FUNCTIONAL GROUPS
# ============================================================

diet_fg <- diet_allocated |>
  
  filter(
    !is.na(predator_fg),
    !is.na(fg_number),
    !is.na(diet_allocated)
  ) |>
  
  group_by(
    
    predator_fg,
    predator_fg_name,
    prey_fg = fg_number,
    prey_fg_name = fg_name
    
  ) |>
  
  summarise(
    
    diet = sum(
      diet_allocated,
      na.rm = TRUE
    ),
    
    n_predator_species =
      n_distinct(
        predator_species
      ),
    
    n_prey_items =
      n_distinct(
        prey_original
      ),
    
    .groups = "drop"
    
  )


# ============================================================
# 15. NORMALIZE FINAL FG DIETS
# ============================================================

diet_fg <- diet_fg |>
  
  group_by(
    predator_fg
  ) |>
  
  mutate(
    
    diet =
      diet /
      sum(
        diet,
        na.rm = TRUE
      )
    
  ) |>
  
  ungroup()


# ============================================================
# 16. LONG EwE MATRIX
# ============================================================

diet_matrix <- diet_fg |>
  
  transmute(
    
    Predator_FG = predator_fg,
    
    Predator_FG_name =
      predator_fg_name,
    
    Prey_FG = prey_fg,
    
    Prey_FG_name =
      prey_fg_name,
    
    Diet = diet
    
  ) |>
  
  arrange(
    Predator_FG,
    Prey_FG
  )


write_csv(
  diet_matrix,
  file.path(
    output_dir,
    "EwE_diet_matrix_FishBase.csv"
  )
)


# ============================================================
# 17. WIDE MATRIX
# ============================================================

diet_matrix_wide <- diet_matrix |>
  
  select(
    Predator_FG,
    Prey_FG,
    Diet
  ) |>
  
  pivot_wider(
    
    names_from = Prey_FG,
    
    values_from = Diet,
    
    values_fill = 0
    
  ) |>
  
  arrange(
    Predator_FG
  )


write_csv(
  diet_matrix_wide,
  file.path(
    output_dir,
    "EwE_diet_matrix_FishBase_wide.csv"
  )
)


# ============================================================
# 18. CHECKS
# ============================================================

diet_checks <- diet_matrix |>
  
  group_by(
    Predator_FG,
    Predator_FG_name
  ) |>
  
  summarise(
    
    total_diet =
      sum(
        Diet,
        na.rm = TRUE
      ),
    
    n_prey_FGs =
      n_distinct(
        Prey_FG
      ),
    
    .groups = "drop"
    
  )


write_csv(
  diet_checks,
  file.path(
    output_dir,
    "EwE_diet_matrix_FishBase_checks.csv"
  )
  
  
  cat("\n============================================\n")
  cat("FISHBASE DIET MATRIX COMPLETE\n")
  cat("============================================\n")
  
  cat(
    "Predator FGs:",
    n_distinct(diet_matrix$Predator_FG),
    "\n"
  )
  
  cat(
    "Prey FGs:",
    n_distinct(diet_matrix$Prey_FG),
    "\n"
  )
  
  cat(
    "Predator-prey links:",
    nrow(diet_matrix),
    "\n"
  )
  
  cat(
    "Maximum deviation from 1:",
    max(
      abs(
        diet_checks$total_diet - 1
      ),
      na.rm = TRUE
    ),
    "\n"
  )
  
  
  
  # ============================================================
  # MAISHA DIET MATRIX FOR WESTERN MEDITERRANEAN EwE
  #
  # Multi-FG prey allocation:
  # one prey item can match multiple FGs.
  #
  # MAISHA contains diet data standardized using WoRMS.
  # Diet contributions have been normalized in the dataset.
  #
  # Matching hierarchy:
  #   1. species
  #   2. genus
  #   3. family
  #   4. order
  #   5. class
  #   6. phylum
  #
  # Multiple matching FGs receive equal shares of the original
  # diet contribution.
  # ============================================================
  
  
  # ============================================================
  # 0. PACKAGES
  # ============================================================
  
  packages <- c(
    "tidyverse",
    "readr",
    "janitor",
    "readxl"
  )
  
  new_packages <- packages[
    !packages %in% rownames(installed.packages())
  ]
  
  if (length(new_packages) > 0) {
    install.packages(new_packages)
  }
  
  library(tidyverse)
  library(readr)
  library(janitor)
  library(readxl)
  
  
  # ============================================================
  # 1. PATHS
  # ============================================================
  
  fg_file <- "P:/EwE Western Med 2026/data/FG_WMed_2026.csv"
  
  maisha_file <-
    "P:/EwE Western Med 2026/data/diet/MAISHA.xlsx"
  
  output_dir <-
    "P:/EwE Western Med 2026/output/diet/MAISHA"
  
  dir.create(
    output_dir,
    recursive = TRUE,
    showWarnings = FALSE
  )
  
  
  # ============================================================
  # 2. READ FG LIST
  # ============================================================
  
  first_line <- readLines(
    fg_file,
    n = 1,
    encoding = "UTF-8"
  )
  
  delimiter <- if (grepl("\t", first_line)) {
    "\t"
  } else if (grepl(";", first_line)) {
    ";"
  } else {
    ","
  }
  
  fg_raw <- read_delim(
    fg_file,
    delim = delimiter,
    show_col_types = FALSE,
    locale = locale(encoding = "UTF-8")
  ) |>
    clean_names()
  
  
  required_cols <- c(
    "fg_number",
    "fg_name",
    "species",
    "genus",
    "family",
    "order",
    "class",
    "phylum"
  )
  
  missing_cols <- setdiff(
    required_cols,
    names(fg_raw)
  )
  
  if (length(missing_cols) > 0) {
    stop(
      "Missing columns in FG file: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  
  # ============================================================
  # 3. STANDARDIZE FG TAXONOMY
  # ============================================================
  
  tax_cols <- c(
    "species",
    "genus",
    "family",
    "order",
    "class",
    "phylum"
  )
  
  fg_tax <- fg_raw |>
    
    mutate(
      
      across(
        all_of(tax_cols),
        ~ str_squish(
          str_to_lower(
            as.character(.x)
          )
        )
      )
      
    ) |>
    
    mutate(
      
      across(
        all_of(tax_cols),
        ~ na_if(.x, "")
      )
      
    )
  
  
  fg_species <- fg_tax |>
    
    filter(
      !is.na(species),
      !is.na(fg_number)
    ) |>
    
    distinct(
      fg_number,
      fg_name,
      species,
      genus,
      family,
      order,
      class,
      phylum
    )
  
  
  # ============================================================
  # 4. READ ALL MAISHA SHEETS
  # ============================================================
  
  sheet_names <- excel_sheets(
    maisha_file
  )
  
  cat("\nMAISHA sheets:\n")
  print(sheet_names)
  
  
  maisha_sheets <- lapply(
    
    sheet_names,
    
    function(s) {
      
      x <- read_excel(
        maisha_file,
        sheet = s
      ) |>
        clean_names()
      
      x$source_sheet <- s
      
      x
      
    }
    
  )
  
  names(maisha_sheets) <- sheet_names
  
  
  # ============================================================
  # 5. IDENTIFY LIKELY DIET TABLE
  # ============================================================
  
  score_sheet <- function(x) {
    
    nm <- names(x)
    
    predator_score <- sum(
      grepl(
        "predator|consumer|species|scientific_name|scientificname",
        nm,
        ignore.case = TRUE
      )
    )
    
    prey_score <- sum(
      grepl(
        "prey|food|item",
        nm,
        ignore.case = TRUE
      )
    )
    
    diet_score <- sum(
      grepl(
        "diet|contribution|percentage|percent|proportion|weight|volume|frequency",
        nm,
        ignore.case = TRUE
      )
    )
    
    predator_score +
      prey_score +
      diet_score
    
  }
  
  
  sheet_scores <- tibble(
    
    sheet = names(maisha_sheets),
    
    score = sapply(
      maisha_sheets,
      score_sheet
    )
    
  ) |>
    
    arrange(
      desc(score)
    )
  
  
  cat("\nMAISHA sheet scores:\n")
  print(sheet_scores)
  
  
  best_sheet <- sheet_scores$sheet[1]
  
  maisha_raw <- maisha_sheets[[best_sheet]]
  
  
  cat(
    "\nUsing MAISHA sheet:",
    best_sheet,
    "\n"
  )
  
  cat(
    "\nMAISHA columns:\n"
  )
  
  print(
    names(maisha_raw)
  )
  
  
  write_csv(
    maisha_raw,
    file.path(
      output_dir,
      "MAISHA_raw_selected_sheet.csv"
    )
  )
  
  
  # ============================================================
  # 6. IDENTIFY PREDATOR / PREY / DIET COLUMNS
  # ============================================================
  
  nm <- names(maisha_raw)
  
  
  predator_candidates <- nm[
    grepl(
      "predator|consumer|species|scientific_name|scientificname",
      nm,
      ignore.case = TRUE
    )
  ]
  
  
  prey_candidates <- nm[
    grepl(
      "prey|food|item",
      nm,
      ignore.case = TRUE
    )
  ]
  
  
  diet_candidates <- nm[
    grepl(
      "diet|contribution|percentage|percent|proportion|fraction|weight|volume",
      nm,
      ignore.case = TRUE
    )
  ]
  
  
  cat("\nPredator candidates:\n")
  print(predator_candidates)
  
  cat("\nPrey candidates:\n")
  print(prey_candidates)
  
  cat("\nDiet candidates:\n")
  print(diet_candidates)
  
  
  if (length(predator_candidates) == 0) {
    stop(
      "Could not identify the MAISHA predator column."
    )
  }
  
  if (length(prey_candidates) == 0) {
    stop(
      "Could not identify the MAISHA prey column."
    )
  }
  
  if (length(diet_candidates) == 0) {
    stop(
      "Could not identify the MAISHA diet contribution column."
    )
  }
  
  
  predator_col <-
    predator_candidates[1]
  
  prey_col <-
    prey_candidates[1]
  
  diet_col <-
    diet_candidates[1]
  
  
  cat(
    "\nUsing predator column:",
    predator_col,
    "\n"
  )
  
  cat(
    "Using prey column:",
    prey_col,
    "\n"
  )
  
  cat(
    "Using diet column:",
    diet_col,
    "\n"
  )
  
  
  # ============================================================
  # 7. CREATE STANDARDIZED MAISHA DIET TABLE
  # ============================================================
  
  diet_species <- maisha_raw |>
    
    transmute(
      
      predator_species =
        as.character(
          .data[[predator_col]]
        ),
      
      prey_original =
        as.character(
          .data[[prey_col]]
        ),
      
      diet_raw =
        suppressWarnings(
          as.numeric(
            .data[[diet_col]]
          )
        ),
      
      source_sheet
      
    ) |>
    
    mutate(
      
      predator_species =
        str_squish(
          str_to_lower(
            predator_species
          )
        ),
      
      prey_original =
        str_squish(
          str_to_lower(
            prey_original
          )
        )
      
    ) |>
    
    filter(
      
      !is.na(predator_species),
      
      predator_species != "",
      
      !is.na(prey_original),
      
      prey_original != "",
      
      !is.na(diet_raw),
      
      diet_raw > 0
      
    )
  
  
  # ============================================================
  # 8. STANDARDIZE DIET CONTRIBUTIONS
  # ============================================================
  
  # MAISHA states that diet contributions have been normalized.
  #
  # Nevertheless, normalize within predator species after
  # extraction so that the resulting diet used by EwE sums to 1.
  
  diet_species <- diet_species |>
    
    group_by(
      predator_species
    ) |>
    
    mutate(
      
      diet_proportion_raw =
        
        if_else(
          
          max(
            diet_raw,
            na.rm = TRUE
          ) > 1,
          
          diet_raw / 100,
          
          diet_raw
          
        )
      
    ) |>
    
    mutate(
      
      diet_proportion =
        
        diet_proportion_raw /
        sum(
          diet_proportion_raw,
          na.rm = TRUE
        )
      
    ) |>
    
    ungroup()
  
  
  # ============================================================
  # 9. MATCH PREDATOR TO EwE FG
  # ============================================================
  
  diet_species <- diet_species |>
    
    left_join(
      
      fg_species |>
        
        select(
          fg_number,
          fg_name,
          species
        ) |>
        
        distinct(),
      
      by = c(
        "predator_species" = "species"
      )
      
    ) |>
    
    rename(
      
      predator_fg =
        fg_number,
      
      predator_fg_name =
        fg_name
      
    )
  
  
  # ============================================================
  # 10. UNMATCHED PREDATORS
  # ============================================================
  
  unmatched_predators <- diet_species |>
    
    filter(
      is.na(predator_fg)
    ) |>
    
    distinct(
      predator_species
    )
  
  
  write_csv(
    unmatched_predators,
    file.path(
      output_dir,
      "unmatched_predators.csv"
    )
  )
  
  
  diet_species <- diet_species |>
    
    filter(
      !is.na(predator_fg)
    )
  
  
  # ============================================================
  # 11. MULTI-FG PREY MATCHING FUNCTION
  # ============================================================
  
  match_prey_to_fg <- function(prey_name) {
    
    prey_name <- str_squish(
      str_to_lower(prey_name)
    )
    
    
    # ----------------------------------------------------------
    # SPECIES
    # ----------------------------------------------------------
    
    m <- fg_species |>
      
      filter(
        species == prey_name
      ) |>
      
      distinct(
        fg_number,
        fg_name
      )
    
    if (nrow(m) > 0) {
      
      return(
        m |>
          mutate(
            match_rank = "species"
          )
      )
      
    }
    
    
    # ----------------------------------------------------------
    # GENUS
    # ----------------------------------------------------------
    
    m <- fg_species |>
      
      filter(
        genus == prey_name
      ) |>
      
      distinct(
        fg_number,
        fg_name
      )
    
    if (nrow(m) > 0) {
      
      return(
        m |>
          mutate(
            match_rank = "genus"
          )
      )
      
    }
    
    
    # ----------------------------------------------------------
    # FAMILY
    # ----------------------------------------------------------
    
    m <- fg_species |>
      
      filter(
        family == prey_name
      ) |>
      
      distinct(
        fg_number,
        fg_name
      )
    
    if (nrow(m) > 0) {
      
      return(
        m |>
          mutate(
            match_rank = "family"
          )
      )
      
    }
    
    
    # ----------------------------------------------------------
    # ORDER
    # ----------------------------------------------------------
    
    m <- fg_species |>
      
      filter(
        order == prey_name
      ) |>
      
      distinct(
        fg_number,
        fg_name
      )
    
    if (nrow(m) > 0) {
      
      return(
        m |>
          mutate(
            match_rank = "order"
          )
      )
      
    }
    
    
    # ----------------------------------------------------------
    # CLASS
    # ----------------------------------------------------------
    
    m <- fg_species |>
      
      filter(
        class == prey_name
      ) |>
      
      distinct(
        fg_number,
        fg_name
      )
    
    if (nrow(m) > 0) {
      
      return(
        m |>
          mutate(
            match_rank = "class"
          )
      )
      
    }
    
    
    # ----------------------------------------------------------
    # PHYLUM
    # ----------------------------------------------------------
    
    m <- fg_species |>
      
      filter(
        phylum == prey_name
      ) |>
      
      distinct(
        fg_number,
        fg_name
      )
    
    if (nrow(m) > 0) {
      
      return(
        m |>
          mutate(
            match_rank = "phylum"
          )
      )
      
    }
    
    
    tibble()
    
  }
  
  
  # ============================================================
  # 12. APPLY MULTI-FG MATCHING
  # ============================================================
  
  prey_matches <- lapply(
    
    seq_len(
      nrow(diet_species)
    ),
    
    function(i) {
      
      m <- match_prey_to_fg(
        diet_species$prey_original[i]
      )
      
      if (nrow(m) == 0) {
        
        return(
          
          tibble(
            
            row_id = i,
            
            fg_number =
              NA_real_,
            
            fg_name =
              NA_character_,
            
            match_rank =
              NA_character_
            
          )
          
        )
        
      }
      
      m |>
        
        mutate(
          row_id = i
        ) |>
        
        select(
          row_id,
          fg_number,
          fg_name,
          match_rank
        )
      
    }
    
  ) |>
    
    bind_rows()
  
  
  # ============================================================
  # 13. ALLOCATE PREY AMONG MULTIPLE FGs
  # ============================================================
  
  diet_allocated <- prey_matches |>
    
    left_join(
      
      diet_species |>
        
        mutate(
          row_id = row_number()
        ) |>
        
        select(
          
          row_id,
          
          predator_species,
          
          predator_fg,
          
          predator_fg_name,
          
          prey_original,
          
          diet_proportion,
          
          source_sheet
          
        ),
      
      by = "row_id"
      
    ) |>
    
    group_by(
      row_id
    ) |>
    
    mutate(
      
      n_matching_fgs =
        sum(
          !is.na(fg_number)
        ),
      
      allocation =
        if_else(
          
          !is.na(fg_number),
          
          1 / n_matching_fgs,
          
          NA_real_
          
        ),
      
      diet_allocated =
        diet_proportion *
        allocation
      
    ) |>
    
    ungroup()
  
  
  # ============================================================
  # 14. IDENTIFY MATCH QUALITY
  # ============================================================
  
  diet_allocated <- diet_allocated |>
    
    mutate(
      
      allocation_method =
        case_when(
          
          is.na(fg_number) ~
            "unmatched",
          
          match_rank == "species" &
            n_matching_fgs == 1 ~
            "exact_species",
          
          match_rank == "genus" &
            n_matching_fgs == 1 ~
            "exact_genus",
          
          match_rank == "family" &
            n_matching_fgs == 1 ~
            "exact_family",
          
          match_rank == "order" &
            n_matching_fgs == 1 ~
            "exact_order",
          
          match_rank == "class" &
            n_matching_fgs == 1 ~
            "exact_class",
          
          match_rank == "phylum" &
            n_matching_fgs == 1 ~
            "exact_phylum",
          
          n_matching_fgs > 1 ~
            "multi_FG_equal",
          
          TRUE ~
            "other"
          
        )
      
    )
  
  
  # ============================================================
  # 15. UNMATCHED PREY
  # ============================================================
  
  unmatched_prey <- diet_allocated |>
    
    filter(
      is.na(fg_number)
    ) |>
    
    select(
      
      predator_species,
      
      predator_fg,
      
      predator_fg_name,
      
      prey_original,
      
      diet_proportion,
      
      match_rank,
      
      allocation_method
      
    ) |>
    
    distinct()
  
  
  write_csv(
    unmatched_prey,
    file.path(
      output_dir,
      "unmatched_prey.csv"
    )
  )
  
  
  # ============================================================
  # 16. SPECIES-LEVEL AUDIT
  # ============================================================
  
  write_csv(
    diet_allocated,
    file.path(
      output_dir,
      "diet_species_MAISHA_allocated.csv"
    )
  )
  
  
  # ============================================================
  # 17. AGGREGATE TO EwE FGs
  # ============================================================
  
  diet_fg <- diet_allocated |>
    
    filter(
      
      !is.na(predator_fg),
      
      !is.na(fg_number),
      
      !is.na(diet_allocated)
      
    ) |>
    
    group_by(
      
      predator_fg,
      
      predator_fg_name,
      
      prey_fg =
        fg_number,
      
      prey_fg_name =
        fg_name
      
    ) |>
    
    summarise(
      
      diet =
        sum(
          diet_allocated,
          na.rm = TRUE
        ),
      
      n_predator_species =
        n_distinct(
          predator_species
        ),
      
      n_prey_items =
        n_distinct(
          prey_original
        ),
      
      .groups = "drop"
      
    )
  
  
  # ============================================================
  # 18. NORMALIZE FINAL FG DIET
  # ============================================================
  
  diet_fg <- diet_fg |>
    
    group_by(
      predator_fg
    ) |>
    
    mutate(
      
      diet =
        diet /
        sum(
          diet,
          na.rm = TRUE
        )
      
    ) |>
    
    ungroup()
  
  
  # ============================================================
  # 19. FINAL EwE LONG MATRIX
  # ============================================================
  
  diet_matrix <- diet_fg |>
    
    transmute(
      
      Predator_FG =
        predator_fg,
      
      Predator_FG_name =
        predator_fg_name,
      
      Prey_FG =
        prey_fg,
      
      Prey_FG_name =
        prey_fg_name,
      
      Diet =
        diet
      
    ) |>
    
    arrange(
      
      Predator_FG,
      
      Prey_FG
      
    )
  
  
  write_csv(
    
    diet_matrix,
    
    file.path(
      output_dir,
      "EwE_diet_matrix_MAISHA.csv"
    )
    
  )
  
  
  # ============================================================
  # 20. WIDE MATRIX
  # ============================================================
  
  diet_matrix_wide <- diet_matrix |>
    
    select(
      Predator_FG,
      Prey_FG,
      Diet
    ) |>
    
    pivot_wider(
      
      names_from =
        Prey_FG,
      
      values_from =
        Diet,
      
      values_fill =
        0
      
    ) |>
    
    arrange(
      Predator_FG
    )
  
  
  write_csv(
    
    diet_matrix_wide,
    
    file.path(
      output_dir,
      "EwE_diet_matrix_MAISHA_wide.csv"
    )
    
  )
  
  
  # ============================================================
  # 21. CHECKS
  # ============================================================
  
  diet_checks <- diet_matrix |>
    
    group_by(
      
      Predator_FG,
      
      Predator_FG_name
      
    ) |>
    
    summarise(
      
      total_diet =
        sum(
          Diet,
          na.rm = TRUE
        ),
      
      n_prey_FGs =
        n_distinct(
          Prey_FG
        ),
      
      .groups = "drop"
      
    )
  
  
  write_csv(
    
    diet_checks,
    
    file.path(
      output_dir,
      "EwE_diet_matrix_MAISHA_checks.csv"
    )
    
  )
  
  
  # ============================================================
  # 22. MULTI-FG ALLOCATION SUMMARY
  # ============================================================
  
  allocation_summary <-
    diet_allocated |>
    
    count(
      allocation_method,
      sort = TRUE
    )
  
  
  write_csv(
    
    allocation_summary,
    
    file.path(
      output_dir,
      "MAISHA_allocation_summary.csv"
    )
    
  )
  
  
  # ============================================================
  # 23. FINAL REPORT
  # ============================================================
  
  cat("\n")
  cat("============================================\n")
  cat("MAISHA DIET MATRIX COMPLETE\n")
  cat("============================================\n")
  
  cat(
    "Predator FGs:",
    n_distinct(
      diet_matrix$Predator_FG
    ),
    "\n"
  )
  
  cat(
    "Prey FGs:",
    n_distinct(
      diet_matrix$Prey_FG
    ),
    "\n"
  )
  
  cat(
    "Predator-prey links:",
    nrow(
      diet_matrix
    ),
    "\n"
  )
  
  cat(
    "Unmatched prey:",
    nrow(
      unmatched_prey
    ),
    "\n"
  )
  
  cat(
    "Multi-FG allocations:",
    sum(
      diet_allocated$allocation_method ==
        "multi_FG_equal",
      na.rm = TRUE
    ),
    "\n"
  )
  
  cat(
    "Maximum deviation from diet = 1:",
    max(
      abs(
        diet_checks$total_diet - 1
      ),
      na.rm = TRUE
    ),
    "\n"
  )
  
  cat("\nOutput directory:\n")
  cat(output_dir)
  cat("\n")
  