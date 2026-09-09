#!/usr/bin/env Rscript
# ==========================================================================
# diet_to_ewe.R
# ==========================================================================
#
# Build an EwE (Ecopath with Ecosim) diet-composition matrix for the West
# Mediterranean model from a local stomach-content database (MS Access),
# blended with diet proportions imported from other sources, and written
# out in the numbered-row/column, decimal-comma CSV layout EwE itself uses
# for diet composition import/export (see example_output_format_CoArc1985.csv
# for the target shape this script reproduces).
#
# --------------------------------------------------------------------------
# WORKFLOW
# --------------------------------------------------------------------------
# 1. Read raw stomach-content records from an Access database (.accdb/.mdb)
#    via RODBC (Windows) or odbc/DBI (cross-platform), or from an exported
#    CSV/XLSX as a fallback. Each record is one prey item found in one
#    stomach sample: predator_species, prey_species, %weight, stomach_id...
#
# 2. Map predator and prey species onto your West Med EwE functional groups
#    (fg) using a *weighted* lookup table (species_to_fg_weight.csv):
#    columns `species, fg_name, proportion` -- a species can map to a
#    single group at proportion 1, or be split across more than one group
#    (e.g. a species whose population your model represents as two
#    size-based stanza groups, "Hake juv" / "Hake adult") at whatever
#    proportion of that species belongs to each group. Every stomach
#    record is then distributed across all (predator_group, prey_group)
#    combinations implied by the predator's and prey's group splits,
#    scaled by both proportions.
#
# 3. Aggregate stomach records into mean diet proportions per
#    (predator_group, prey_group), tracking sample size (n stomachs).
#
# 4. Optionally load one or more "external" diet-proportion tables (CSV/XLSX,
#    already in predator_group/prey_group/proportion form -- e.g. the
#    output of fetch_external_diet_fishbase.R, or a manually digitised
#    source) and blend them in.
#
# 5. Pivot into the full EwE diet matrix using your master group list
#    (ewe_group_table.csv: group_number, group_name, is_predator) for row
#    and column order -- prey rows cover every group (in Ecopath group-
#    number order), predator columns cover only the subset flagged
#    is_predator (also in group-number order, matching how EwE itself
#    lays diet composition out). Renormalise every predator column to sum
#    to 1, then write it out with the same layout as EwE's own diet
#    composition CSV: row number + prey name down the side, predator group
#    numbers across the top, decimal-comma values, blank cells for zero,
#    plus the standard "Import" / "Sum" / "(1 - Sum)" check rows at the
#    bottom.
#
# A companion QA report lists: species found in the DB that are NOT in your
# mapping table, species whose fg proportions don't sum to ~1, predators
# with fewer than `min_n` stomachs, and any predator column that doesn't
# sum to 1 before normalisation.
#
# --------------------------------------------------------------------------
# REQUIREMENTS
# --------------------------------------------------------------------------
#   install.packages(c("dplyr", "tidyr", "readr", "optparse"))
# (readxl is only needed if you actually pass a .xlsx/.xls file -- install
# it then, if/when you need it: install.packages("readxl"))
#
# For Access itself, ONE of:
#   - Windows: install.packages("RODBC")
#   - Cross-platform: install.packages(c("odbc", "DBI")) + an Access ODBC
#     driver.
# Or skip ODBC entirely and use --stomach_csv with a CSV/XLSX export of the
# stomach-content table instead of --access_db.
#
# --------------------------------------------------------------------------
# CONFIGURE ME
# --------------------------------------------------------------------------
# ==========================================================================

suppressMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
})
# readxl is only needed if you actually pass a .xlsx/.xls file somewhere --
# loaded lazily (see read_table_auto() below) rather than at startup, since
# a broken readxl install (a known libiconv issue on some R-for-macOS
# arm64 setups) would otherwise stop the whole script even for CSV-only use.
read_table_auto <- function(path) {
  if (grepl("\\.xlsx?$", path, ignore.case = TRUE)) {
    if (!requireNamespace("readxl", quietly = TRUE)) {
      stop("Reading '", path, "' requires the readxl package (install.packages(\"readxl\")), ",
           "or export it to CSV instead and point the script at that.")
    }
    readxl::read_excel(path)
  } else {
    readr::read_csv(path, show_col_types = FALSE)
  }
}

# ==========================================================================
# CONFIG -- edit this section to match your database / files
# ==========================================================================

# Map the columns in *your* Access stomach-content table onto the standard
# names this script uses internally. Change the right-hand side (values)
# only.
stomach_columns <- c(
  predator_species  = "PredatorSpecies",
  prey_species      = "PreySpecies",
  proportion        = "PercentWeight",
  stomach_id        = "StomachID",
  predator_length   = "PredatorLength_cm",  # optional; NA if you don't have it
  region            = "Region"              # optional; NA if you don't have it
)

stomach_table <- "StomachContents"

# Minimum number of stomachs required before local data is trusted on its
# own for a given predator group; below this threshold external sources are
# blended in (or used exclusively if no local data at all).
min_n_stomachs <- 10

# Blend weight given to *local* stomach data relative to each external
# source when both exist for the same predator/prey pair (0-1).
local_blend_weight <- 0.7


# ==========================================================================
# GROUP LIST + SPECIES-TO-GROUP WEIGHTS
# ==========================================================================

#' Load the master EwE functional-group table: which groups exist, their
#' Ecopath group number (this fixes row/column order in the output, exactly
#' as it appears in your model's Basic Estimates), and whether each group
#' is a predator (i.e. should get its own column in the diet matrix) --
#' typically everything except primary producers and detritus.
#'
#' Expected columns: group_number, group_name, is_predator (TRUE/FALSE)
load_group_table <- function(csv_path) {
  g <- read_csv(csv_path, show_col_types = FALSE)
  required <- c("group_number", "group_name", "is_predator")
  if (!all(required %in% names(g))) {
    stop("Group table must have columns ", paste(required, collapse = ", "),
         ", got ", paste(names(g), collapse = ", "))
  }
  g$is_predator <- as.logical(g$is_predator)
  g <- g[order(g$group_number), , drop = FALSE]
  g
}

#' Load the species -> functional-group weight table.
#'
#' Expected columns: species, fg_name, proportion
#' A species may appear on more than one row if it's split across groups
#' (e.g. by size/stanza) -- proportion is how much of that species belongs
#' to each fg_name, and should sum to ~1 across a species' rows. Species
#' names are matched case-insensitively against the stomach-content table's
#' predator_species/prey_species columns.
load_species_fg_weights <- function(csv_path) {
  w <- read_csv(csv_path, show_col_types = FALSE, comment = "#")
  required <- c("species", "fg_name", "proportion")
  if (!all(required %in% names(w))) {
    stop("Species-to-fg weight file must have columns ", paste(required, collapse = ", "),
         ", got ", paste(names(w), collapse = ", "))
  }
  w$species <- tolower(trimws(w$species))
  w$proportion <- as.numeric(w$proportion)
  w
}

#' Check that each species' proportions sum to ~1 (informational -- doesn't
#' stop the run, just flags it in the QA report, since a partial split may
#' be intentional, e.g. you only modelled part of a species' population).
check_species_weight_sums <- function(weights) {
  totals <- weights %>% group_by(species) %>% summarise(total = sum(proportion), .groups = "drop")
  off <- totals %>% filter(abs(total - 1) > 0.01)
  data.frame(species = off$species, total = off$total)
}


# ==========================================================================
# DATA LOADING (stomach contents, external sources)
# ==========================================================================

load_stomach_contents_access <- function(db_path, table_or_query = stomach_table) {
  query <- if (grepl("^\\s*select", table_or_query, ignore.case = TRUE)) {
    table_or_query
  } else {
    sprintf("SELECT * FROM [%s]", table_or_query)
  }
  
  df <- NULL
  if (requireNamespace("RODBC", quietly = TRUE)) {
    conn_str <- sprintf("DRIVER={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=%s;", db_path)
    ch <- RODBC::odbcDriverConnect(conn_str)
    on.exit(RODBC::odbcClose(ch), add = TRUE)
    df <- RODBC::sqlQuery(ch, query, stringsAsFactors = FALSE)
  } else if (requireNamespace("odbc", quietly = TRUE) && requireNamespace("DBI", quietly = TRUE)) {
    conn_str <- sprintf("Driver={Microsoft Access Driver (*.mdb, *.accdb)};DBQ=%s;", db_path)
    con <- DBI::dbConnect(odbc::odbc(), .connection_string = conn_str)
    on.exit(DBI::dbDisconnect(con), add = TRUE)
    df <- DBI::dbGetQuery(con, query)
  } else {
    stop(
      "Neither RODBC nor odbc/DBI is installed. Install one of them, or export the ",
      "table to CSV/XLSX and use --stomach_csv instead."
    )
  }
  
  standardise_stomach_columns(df)
}

load_stomach_contents_csv <- function(path) {
  df <- read_table_auto(path)
  standardise_stomach_columns(as.data.frame(df))
}

standardise_stomach_columns <- function(df) {
  present <- stomach_columns[!is.na(stomach_columns) & stomach_columns %in% names(df)]
  df <- df %>% rename(!!!setNames(present, names(present)))
  
  required <- c("predator_species", "prey_species", "proportion")
  missing <- setdiff(required, names(df))
  if (length(missing) > 0) {
    stop("Stomach-content table is missing required column(s) [", paste(missing, collapse = ", "),
         "] after applying stomach_columns mapping.")
  }
  
  keep <- intersect(c("predator_species", "prey_species", "proportion", "stomach_id",
                      "predator_length", "region"), names(df))
  df[, keep, drop = FALSE]
}

#' Load a pre-aggregated external diet-proportion table (predator_group,
#' prey_group, proportion -- already in your EwE group names, e.g. the
#' output of fetch_external_diet_fishbase.R or a manually digitised source).
load_external_diet_source <- function(path, source_name, weight = 1.0) {
  df <- read_table_auto(path)
  df <- as.data.frame(df)
  required <- c("predator_group", "prey_group", "proportion")
  if (!all(required %in% names(df))) {
    stop("External diet source '", path, "' must have columns ", paste(required, collapse = ", "))
  }
  df <- df[, required, drop = FALSE]
  df$source <- source_name
  df$weight <- weight
  df
}


# ==========================================================================
# MAPPING (species -> weighted fg splits) + AGGREGATION
# ==========================================================================

new_qa_report <- function() {
  list(
    unmapped_predator_species = character(0),
    unmapped_prey_species = character(0),
    species_weight_not_summing_to_1 = data.frame(species = character(0), total = double(0)),
    low_sample_predators = data.frame(group = character(0), n = integer(0)),
    unnormalised_columns = data.frame(group = character(0), total = double(0))
  )
}

print_qa_report <- function(qa) {
  cat("\n===== QA REPORT =====\n")
  if (length(qa$unmapped_predator_species) > 0) {
    cat(sprintf("\nPredator species NOT in species_to_fg_weight.csv (%d):\n", length(qa$unmapped_predator_species)))
    for (s in sort(qa$unmapped_predator_species)) cat("  -", s, "\n")
  }
  if (length(qa$unmapped_prey_species) > 0) {
    cat(sprintf("\nPrey species NOT in species_to_fg_weight.csv (%d):\n", length(qa$unmapped_prey_species)))
    for (s in sort(qa$unmapped_prey_species)) cat("  -", s, "\n")
  }
  if (nrow(qa$species_weight_not_summing_to_1) > 0) {
    cat("\nSpecies whose fg proportions don't sum to ~1 (check if intentional):\n")
    for (i in seq_len(nrow(qa$species_weight_not_summing_to_1))) {
      r <- qa$species_weight_not_summing_to_1[i, ]
      cat(sprintf("  - %s: sum=%.4f\n", r$species, r$total))
    }
  }
  if (nrow(qa$low_sample_predators) > 0) {
    cat(sprintf("\nPredator groups with < %d stomachs (external sources will be blended/used):\n", min_n_stomachs))
    for (i in seq_len(nrow(qa$low_sample_predators))) {
      r <- qa$low_sample_predators[i, ]
      cat(sprintf("  - %s: n=%d\n", r$group, r$n))
    }
  }
  if (nrow(qa$unnormalised_columns) > 0) {
    cat("\nPredator columns that did not sum to 1 before normalisation:\n")
    for (i in seq_len(nrow(qa$unnormalised_columns))) {
      r <- qa$unnormalised_columns[i, ]
      cat(sprintf("  - %s: sum=%.4f\n", r$group, r$total))
    }
  }
  if (length(qa$unmapped_predator_species) == 0 && length(qa$unmapped_prey_species) == 0 &&
      nrow(qa$species_weight_not_summing_to_1) == 0 && nrow(qa$low_sample_predators) == 0) {
    cat("No issues found.\n")
  }
  cat("======================\n\n")
}

#' Expand each stomach record across every (predator_group, prey_group)
#' combination implied by the predator's and prey's species-to-fg splits,
#' scaling the original proportion by predator_weight * prey_weight.
#' Records whose predator or prey species has no entry in the weight table
#' are dropped (and reported in the qa report).
map_to_ewe_groups <- function(stomachs, weights, qa) {
  w <- weights %>% transmute(key = species, group = fg_name, w = proportion)
  
  stomachs <- stomachs %>% mutate(.pred_key = tolower(trimws(predator_species)),
                                  .prey_key = tolower(trimws(prey_species)),
                                  .row_id = row_number())
  
  qa$unmapped_predator_species <- sort(unique(
    stomachs$predator_species[!(stomachs$.pred_key %in% w$key)]
  ))
  qa$unmapped_prey_species <- sort(unique(
    stomachs$prey_species[!(stomachs$.prey_key %in% w$key)]
  ))
  
  # many-to-many is expected here (a species may split across several fg's) --
  # silence dplyr's warning about it explicitly rather than suppressing all warnings.
  pred_split <- stomachs %>%
    inner_join(w %>% rename(predator_group = group, predator_weight = w),
               by = c(".pred_key" = "key"), relationship = "many-to-many")
  
  both_split <- pred_split %>%
    inner_join(w %>% rename(prey_group = group, prey_weight = w),
               by = c(".prey_key" = "key"), relationship = "many-to-many")
  
  before <- length(unique(stomachs$.row_id))
  after <- length(unique(both_split$.row_id))
  dropped <- before - after
  if (dropped > 0) {
    cat(sprintf(
      "[map_to_ewe_groups] Dropped %d stomach record(s) with unmapped predator or prey species -- add them to species_to_fg_weight.csv.\n",
      dropped
    ))
  }
  
  both_split$proportion <- both_split$proportion * both_split$predator_weight * both_split$prey_weight
  
  list(stomachs = both_split %>% select(-.pred_key, -.prey_key, -.row_id, -predator_weight, -prey_weight),
       qa = qa)
}

#' Aggregate mapped (and fg-split) stomach records into mean diet
#' proportions per (predator_group, prey_group) -- the "mean %W" method --
#' then renormalise each predator group to sum to 1.
aggregate_local_diet <- function(stomachs, qa) {
  n_stomachs <- if ("stomach_id" %in% names(stomachs)) {
    stomachs %>% group_by(predator_group) %>% summarise(n = n_distinct(stomach_id), .groups = "drop")
  } else {
    stomachs %>% group_by(predator_group) %>% summarise(n = n(), .groups = "drop")
  }
  
  agg <- stomachs %>%
    group_by(predator_group, prey_group) %>%
    summarise(proportion = mean(proportion), .groups = "drop")
  
  totals <- agg %>% group_by(predator_group) %>% summarise(total = sum(proportion), .groups = "drop")
  off <- totals %>% filter(!(total >= 0.99 & total <= 1.01) & !(total >= 99 & total <= 101))
  qa$unnormalised_columns <- data.frame(group = off$predator_group, total = off$total)
  
  agg <- agg %>%
    left_join(totals, by = "predator_group") %>%
    mutate(proportion = proportion / total) %>%
    select(-total) %>%
    left_join(n_stomachs, by = "predator_group") %>%
    rename(n_stomachs = n)
  
  low <- n_stomachs %>% filter(n < min_n_stomachs) %>% arrange(predator_group)
  qa$low_sample_predators <- data.frame(group = low$predator_group, n = low$n)
  
  list(diet = agg, qa = qa)
}


# ==========================================================================
# BLENDING LOCAL + EXTERNAL SOURCES
# ==========================================================================

blend_diet_sources <- function(local_diet, external_sources = list(),
                               min_n = min_n_stomachs, local_weight = local_blend_weight) {
  local_diet$source <- "local_stomachs"
  
  if (length(external_sources) == 0) {
    return(local_diet %>% select(predator_group, prey_group, proportion, source))
  }
  
  external <- bind_rows(external_sources)
  external_avg <- external %>%
    group_by(predator_group, prey_group) %>%
    summarise(proportion = sum(proportion * weight) / sum(weight), .groups = "drop") %>%
    mutate(source = "external")
  
  trusted_predators <- unique(local_diet$predator_group[local_diet$n_stomachs >= min_n])
  all_predators <- union(trusted_predators, union(unique(external_avg$predator_group), unique(local_diet$predator_group)))
  
  blended_rows <- lapply(all_predators, function(pred) {
    loc <- local_diet %>% filter(predator_group == pred)
    ext <- external_avg %>% filter(predator_group == pred)
    
    if (pred %in% trusted_predators && nrow(ext) > 0) {
      merged <- full_join(
        loc %>% select(prey_group, proportion), ext %>% select(prey_group, proportion),
        by = "prey_group", suffix = c("_local", "_ext")
      )
      merged[is.na(merged)] <- 0
      merged$proportion <- local_weight * merged$proportion_local + (1 - local_weight) * merged$proportion_ext
      merged$predator_group <- pred
      merged$source <- "blended"
      merged %>% select(predator_group, prey_group, proportion, source)
    } else if (pred %in% trusted_predators) {
      loc %>% select(predator_group, prey_group, proportion, source)
    } else if (nrow(ext) > 0) {
      ext %>% mutate(predator_group = pred) %>% select(predator_group, prey_group, proportion, source)
    } else {
      loc %>% mutate(source = "local_sparse") %>% select(predator_group, prey_group, proportion, source)
    }
  })
  
  result <- bind_rows(blended_rows)
  totals <- result %>% group_by(predator_group) %>% summarise(total = sum(proportion), .groups = "drop")
  result %>%
    left_join(totals, by = "predator_group") %>%
    mutate(proportion = ifelse(total > 0, proportion / total, 0)) %>%
    select(-total)
}


# ==========================================================================
# BUILD FINAL MATRIX + EXPORT IN EwE's OWN CSV LAYOUT
# ==========================================================================

#' Pivot the long (predator_group, prey_group, proportion) table into a
#' prey-by-predator matrix, using group_table for row order (all groups,
#' by group_number) and column order (is_predator groups only, by
#' group_number), and renormalise each predator column to sum to 1.
build_ewe_matrix <- function(diet_long, group_table) {
  row_names <- group_table$group_name
  col_names <- group_table$group_name[group_table$is_predator]
  
  wide <- diet_long %>%
    group_by(prey_group, predator_group) %>%
    summarise(proportion = sum(proportion), .groups = "drop") %>%
    pivot_wider(names_from = predator_group, values_from = proportion, values_fill = 0)
  
  mat <- matrix(0, nrow = length(row_names), ncol = length(col_names),
                dimnames = list(row_names, col_names))
  present_rows <- intersect(wide$prey_group, row_names)
  present_cols <- intersect(names(wide), col_names)
  if (length(present_rows) > 0 && length(present_cols) > 0) {
    src <- as.data.frame(wide)
    rownames(src) <- src$prey_group
    mat[present_rows, present_cols] <- as.matrix(src[present_rows, present_cols, drop = FALSE])
  }
  
  col_sums <- colSums(mat)
  nonzero <- col_sums > 0
  if (any(nonzero)) mat[, nonzero] <- sweep(mat[, nonzero, drop = FALSE], 2, col_sums[nonzero], "/")
  
  mat
}

#' Format a number the way the target EwE CSV does: decimal comma, trimmed
#' to as few decimals as needed (min 3) without misleading rounding, blank
#' for exactly zero.
format_ewe_number <- function(x, blank_zero = TRUE) {
  vapply(x, function(v) {
    if (is.na(v)) return("")
    if (blank_zero && v == 0) return("")
    s <- formatC(v, format = "f", digits = 6)
    s <- sub("0+$", "", s)
    s <- sub("\\.$", ".000", s)
    parts <- strsplit(s, "\\.", fixed = FALSE)[[1]]
    if (length(parts) == 1) parts <- c(parts, "000")
    while (nchar(parts[2]) < 3) parts[2] <- paste0(parts[2], "0")
    sub("\\.", ",", paste(parts, collapse = "."))
  }, character(1))
}

#' Write the diet matrix out in the same layout as EwE's own diet
#' composition CSV: blank,"Prey \ predator",<predator group numbers...>
#' header row; one row per prey group (group_number, group_name, values);
#' then Import / Sum / (1 - Sum) rows with blank first column, matching
#' example_output_format_CoArc1985.csv. `import_row` is an optional named
#' numeric vector (predator group_name -> import proportion), defaulting to
#' 0 for every predator.
export_ewe_matrix_csv <- function(mat, group_table, out_path, import_row = NULL) {
  col_names <- colnames(mat)
  col_numbers <- group_table$group_number[match(col_names, group_table$group_name)]
  row_numbers <- group_table$group_number[match(rownames(mat), group_table$group_name)]
  
  if (is.null(import_row)) import_row <- setNames(rep(0, length(col_names)), col_names)
  import_row <- import_row[col_names]
  import_row[is.na(import_row)] <- 0
  
  sum_row <- colSums(mat) + import_row
  resid_row <- 1 - sum_row
  
  quote_if_needed <- function(x) {
    ifelse(grepl(",", x, fixed = TRUE) | grepl('"', x, fixed = TRUE),
           paste0('"', gsub('"', '""', x), '"'), x)
  }
  
  header <- c("", '"Prey \\ predator"', as.character(col_numbers))
  
  data_lines <- vapply(seq_len(nrow(mat)), function(i) {
    vals <- format_ewe_number(mat[i, ])
    vals <- ifelse(vals == "", "", quote_if_needed(vals))
    paste(c(row_numbers[i], quote_if_needed(rownames(mat)[i]), vals), collapse = ",")
  }, character(1))
  
  import_line <- paste(c("", '"Import"', quote_if_needed(format_ewe_number(import_row, blank_zero = FALSE))), collapse = ",")
  sum_line <- paste(c("", '"Sum"', quote_if_needed(format_ewe_number(sum_row, blank_zero = FALSE))), collapse = ",")
  resid_line <- paste(c("", '"(1 - Sum)"', quote_if_needed(format_ewe_number(resid_row, blank_zero = FALSE))), collapse = ",")
  
  writeLines(c(paste(header, collapse = ","), data_lines, import_line, sum_line, resid_line), out_path)
  cat(sprintf("Wrote EwE diet composition matrix (%d prey groups x %d predator groups) -> %s\n",
              nrow(mat), ncol(mat), out_path))
  
  off <- which(abs(resid_row) > 0.01)
  if (length(off) > 0) {
    cat("NOTE: these predator columns did not sum to 1 (no diet data mapped to them) -- ",
        "check the QA report / your species mapping for:\n", sep = "")
    for (j in off) cat("  -", col_names[j], sprintf("(sum=%.3f)\n", sum_row[j]))
  }
}


# ==========================================================================
# CLI
# ==========================================================================

run_pipeline <- function(access_db = NULL, stomach_csv = NULL, table = stomach_table,
                         weights_path, groups_path, external_specs = character(0),
                         out_path = "ewe_diet_composition.csv",
                         min_n = min_n_stomachs, local_weight = local_blend_weight) {
  qa <- new_qa_report()
  
  stomachs <- if (!is.null(access_db)) load_stomach_contents_access(access_db, table) else load_stomach_contents_csv(stomach_csv)
  weights <- load_species_fg_weights(weights_path)
  group_table <- load_group_table(groups_path)
  
  qa$species_weight_not_summing_to_1 <- check_species_weight_sums(weights)
  
  mapped <- map_to_ewe_groups(stomachs, weights, qa)
  agg <- aggregate_local_diet(mapped$stomachs, mapped$qa)
  
  external_sources <- lapply(external_specs, function(spec) {
    parts <- strsplit(spec, ":")[[1]]
    path <- parts[1]
    name <- if (length(parts) > 1) parts[2] else tools::file_path_sans_ext(basename(path))
    weight <- if (length(parts) > 2) as.numeric(parts[3]) else 1.0
    load_external_diet_source(path, name, weight)
  })
  
  blended <- blend_diet_sources(agg$diet, external_sources, min_n = min_n, local_weight = local_weight)
  mat <- build_ewe_matrix(blended, group_table)
  export_ewe_matrix_csv(mat, group_table, out_path)
  
  print_qa_report(agg$qa)
  invisible(mat)
}

# Only try to parse command-line flags when actually invoked as
# `Rscript diet_to_ewe.R --flags...` from a terminal (interactive() is
# FALSE there). When the file is source()'d or pasted into an interactive
# R/RStudio console -- which looks identical to Rscript from inside the
# script otherwise -- this block is skipped entirely: all the functions
# above are still defined, ready to call run_pipeline(...) directly, and
# nothing errors out just because there were no --flags to parse.
if (!interactive() && identical(environment(), globalenv()) && sys.nframe() == 0L) {
  suppressMessages(library(optparse))
  
  option_list <- list(
    make_option("--access_db", type = "character", default = NULL),
    make_option("--stomach_csv", type = "character", default = NULL),
    make_option("--table", type = "character", default = stomach_table),
    make_option("--weights", type = "character", default = NULL,
                help = "CSV mapping species -> fg_name, proportion"),
    make_option("--groups", type = "character", default = NULL,
                help = "CSV of group_number, group_name, is_predator (your model's group list)"),
    make_option("--external", type = "character", action = "append", default = list(),
                help = "External diet source as PATH:SOURCE_NAME:WEIGHT (repeatable)"),
    make_option("--out", type = "character", default = "ewe_diet_composition.csv"),
    make_option("--min_n", type = "integer", default = min_n_stomachs),
    make_option("--local_weight", type = "double", default = local_blend_weight)
  )
  opt <- parse_args(OptionParser(option_list = option_list))
  
  if (is.null(opt$access_db) && is.null(opt$stomach_csv)) stop("Provide either --access_db or --stomach_csv")
  if (is.null(opt$weights)) stop("--weights is required (species,fg_name,proportion CSV)")
  if (is.null(opt$groups)) stop("--groups is required (group_number,group_name,is_predator CSV)")
  
  run_pipeline(
    access_db = opt$access_db, stomach_csv = opt$stomach_csv, table = opt$table,
    weights_path = opt$weights, groups_path = opt$groups, external_specs = unlist(opt$external),
    out_path = opt$out, min_n = opt$min_n, local_weight = opt$local_weight
  )
}