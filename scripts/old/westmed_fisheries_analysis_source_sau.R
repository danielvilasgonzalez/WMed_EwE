#!/usr/bin/env Rscript
#
# westmed_fisheries_analysis_source_sau.R
# ============================================================================
# SAU (Sea Around Us) data-source module for the western Mediterranean
# fisheries analysis.
#
# This file only LOADS and RESHAPES data -- no plotting, no file export.
# westmed_fisheries_analysis.R (or the .Rmd) sources this file and calls
# these functions, then does the plotting/export itself. That separation is
# the point: if SAU changes its column names tomorrow, only this file needs
# a fix; if you want a different plot style, only the main script changes.
#
# Requires: sau_raw_combined_west_med.csv already produced by
# sau_west_med_full.R (this module does not call the SAU API itself -- that
# extraction is a separate, longer-running step you run once).

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
})

#' Read and clean the SAU raw combined extract.
#'
#' Resolves the gear/species/sector/reporting-status/tonnes columns
#' defensively (SAU's dimension-aggregation export and its raw CSV export
#' use slightly different column names), renames them to a fixed internal
#' schema (.gear, .sci, .com, .tonnes, .sector, .report), and filters to the
#' requested year range. Which optional columns were actually found is
#' recorded as attributes so downstream functions can skip gracefully.
sau_load_raw <- function(csv_path, year_min = 1994, year_max = 2019) {
  if (!file.exists(csv_path)) {
    stop("Can't find ", csv_path, " -- run sau_west_med_full.R first to produce it.")
  }
  read_fn <- if (requireNamespace("readr", quietly = TRUE)) {
    function(p) readr::read_csv(p, show_col_types = FALSE, progress = FALSE)
  } else {
    function(p) utils::read.csv(p, stringsAsFactors = FALSE, check.names = FALSE)
  }
  raw <- read_fn(csv_path)

  candidates <- list(
    gear     = c("gear_type", "gear"),
    sci_name = c("scientific_name"),
    com_name = c("common_name"),
    tonnes   = c("tonnes", "catch_sum", "value"),
    sector   = c("fishing_sector", "sector"),
    report   = c("reporting_status")
  )
  resolved <- list()
  for (field in names(candidates)) {
    for (opt in candidates[[field]]) {
      if (opt %in% names(raw)) { resolved[[field]] <- opt; break }
    }
  }
  missing_cols <- setdiff(c("gear", "sci_name", "tonnes"), names(resolved))
  if (length(missing_cols) > 0) {
    stop("SAU raw extract is missing required column(s): ", paste(missing_cols, collapse = ", "))
  }
  if (is.null(resolved$com_name)) resolved$com_name <- resolved$sci_name

  have_sector_col <- !is.null(resolved$sector)
  have_report_col <- !is.null(resolved$report)
  if (!have_sector_col) message("[SAU] No sector column found -- sector-level outputs will be skipped.")
  if (!have_report_col) message("[SAU] No reporting-status column found -- the reported-only series will be skipped.")

  raw <- raw %>%
    rename(.gear = !!resolved$gear, .sci = !!resolved$sci_name,
           .com = !!resolved$com_name, .tonnes = !!resolved$tonnes) %>%
    filter(!is.na(year), year >= year_min, year <= year_max)
  if (have_sector_col) raw <- raw %>% rename(.sector = !!resolved$sector)
  if (have_report_col) raw <- raw %>% rename(.report = !!resolved$report)

  attr(raw, "have_sector_col") <- have_sector_col
  attr(raw, "have_report_col") <- have_report_col
  message(sprintf("[SAU] Loaded %d rows, %d-%d.", nrow(raw), year_min, year_max))
  raw
}

sau_has_sector <- function(raw) isTRUE(attr(raw, "have_sector_col"))
sau_has_report <- function(raw) isTRUE(attr(raw, "have_report_col"))

#' Top-N species by total tonnage across the whole extract.
sau_top_species <- function(raw, n_top = 10) {
  raw %>%
    group_by(scientific_name = .sci, common_name = .com) %>%
    summarise(total_tonnes = sum(.tonnes, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total_tonnes)) %>%
    slice_head(n = n_top) %>%
    mutate(rank = row_number()) %>%
    dplyr::select(rank, scientific_name, common_name, total_tonnes)
}

#' Gear x top-species x country breakdown, plus the plot-ready long table
#' (top gears kept individually, the rest folded into "Other gear").
sau_gear_species_country <- function(raw, top_species, n_top_gears = 8) {
  raw_top <- raw %>% filter(.sci %in% top_species$scientific_name)

  gear_species_country_year <- raw_top %>%
    group_by(country, year, gear = .gear, scientific_name = .sci, common_name = .com) %>%
    summarise(tonnes = sum(.tonnes, na.rm = TRUE), .groups = "drop") %>%
    left_join(top_species %>% dplyr::select(scientific_name, rank), by = "scientific_name") %>%
    arrange(rank, country, year, desc(tonnes))

  gear_species_country_allyears <- gear_species_country_year %>%
    group_by(country, gear, scientific_name, common_name, rank) %>%
    summarise(tonnes = sum(tonnes, na.rm = TRUE), .groups = "drop") %>%
    arrange(rank, country, desc(tonnes))

  top_gears <- gear_species_country_allyears %>%
    group_by(gear) %>% summarise(g_tot = sum(tonnes, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(g_tot)) %>% slice_head(n = n_top_gears) %>% pull(gear)

  plot_df <- gear_species_country_allyears %>%
    mutate(gear_grp = ifelse(gear %in% top_gears, gear, "Other gear")) %>%
    group_by(country, gear_grp, scientific_name, common_name, rank) %>%
    summarise(tonnes = sum(tonnes, na.rm = TRUE), .groups = "drop") %>%
    mutate(species_label = sprintf("%02d. %s", rank, common_name))

  list(raw_top10 = raw_top, gear_species_country_year = gear_species_country_year,
       gear_species_country_allyears = gear_species_country_allyears, plot_df = plot_df)
}

#' Catch over time by sector, by country x year. Empty tibble if there's no
#' sector column in this extract.
sau_sector_timeseries <- function(raw) {
  if (!sau_has_sector(raw)) return(tibble())
  raw %>% group_by(country, year, sector = .sector) %>% summarise(tonnes = sum(.tonnes, na.rm = TRUE), .groups = "drop")
}

#' Catch over time by gear (top-N + "Other gear"), by country x year.
sau_gear_timeseries <- function(raw, n_top_gears = 8) {
  top <- raw %>% group_by(gear = .gear) %>% summarise(g_tot = sum(.tonnes, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(g_tot)) %>% slice_head(n = n_top_gears) %>% pull(gear)
  raw %>% mutate(gear_grp = ifelse(.gear %in% top, .gear, "Other gear")) %>%
    group_by(country, year, gear_grp) %>% summarise(tonnes = sum(.tonnes, na.rm = TRUE), .groups = "drop")
}

#' Every sector's tonnage and % share of each country's total catch (no
#' top-N truncation). Empty tibble if there's no sector column.
sau_sector_summary_full <- function(raw) {
  if (!sau_has_sector(raw)) return(tibble())
  country_totals <- raw %>% group_by(country) %>% summarise(country_total_tonnes = sum(.tonnes, na.rm = TRUE), .groups = "drop")
  raw %>% group_by(country, sector = .sector) %>% summarise(tonnes = sum(.tonnes, na.rm = TRUE), .groups = "drop") %>%
    left_join(country_totals, by = "country") %>%
    mutate(pct_of_country_catch = 100 * tonnes / country_total_tonnes) %>%
    dplyr::select(-country_total_tonnes) %>% arrange(country, desc(tonnes))
}

#' Every gear's tonnage and % share of each country's total catch (no
#' top-N truncation).
sau_gear_summary_full <- function(raw) {
  country_totals <- raw %>% group_by(country) %>% summarise(country_total_tonnes = sum(.tonnes, na.rm = TRUE), .groups = "drop")
  raw %>% group_by(country, gear = .gear) %>% summarise(tonnes = sum(.tonnes, na.rm = TRUE), .groups = "drop") %>%
    left_join(country_totals, by = "country") %>%
    mutate(pct_of_country_catch = 100 * tonnes / country_total_tonnes) %>%
    dplyr::select(-country_total_tonnes) %>% arrange(country, desc(tonnes))
}

#' Each top species' tonnage and % share of its country's total catch.
sau_species_share <- function(raw, top_species) {
  country_totals <- raw %>% group_by(country) %>% summarise(country_total_tonnes = sum(.tonnes, na.rm = TRUE), .groups = "drop")
  raw %>% filter(.sci %in% top_species$scientific_name) %>%
    group_by(country, scientific_name = .sci, common_name = .com) %>%
    summarise(tonnes = sum(.tonnes, na.rm = TRUE), .groups = "drop") %>%
    left_join(country_totals, by = "country") %>%
    mutate(pct_of_country_catch = 100 * tonnes / country_total_tonnes) %>%
    dplyr::select(-country_total_tonnes) %>%
    left_join(top_species %>% dplyr::select(scientific_name, rank), by = "scientific_name") %>%
    arrange(country, rank)
}

#' SAU TOTAL catch (every row: reported + unreported + discarded + IUU),
#' by country x year -- one row per source/approach, ready to bind_rows()
#' with the other sources for the master comparison.
sau_total_by_country_year <- function(raw) {
  raw %>% group_by(country, year) %>% summarise(tonnes = sum(.tonnes, na.rm = TRUE), .groups = "drop") %>%
    mutate(source = "SAU total (reconstructed)")
}

#' SAU REPORTED-ONLY catch (reporting_status flagged "Reported"), by
#' country x year. Empty tibble if there's no reporting-status column.
sau_reported_by_country_year <- function(raw) {
  if (!sau_has_report(raw)) return(tibble())
  raw %>% group_by(country, year) %>%
    summarise(tonnes = sum(.tonnes[grepl("^report", .report, ignore.case = TRUE) &
                                    !grepl("unreport", .report, ignore.case = TRUE)], na.rm = TRUE),
              .groups = "drop") %>%
    mutate(source = "SAU reported-only")
}

#' Catch by gear, country x year (full, no top-N truncation) -- feeds the
#' gear-share comparison in the main script.
sau_gear_by_country_year <- function(raw) {
  raw %>% group_by(country, year, gear = .gear) %>% summarise(tonnes = sum(.tonnes, na.rm = TRUE), .groups = "drop")
}

#' % of each country's catch (tonnes) by gear, top-N + "Other gear".
sau_gear_share_by_country <- function(gear_country_year, n_top_gears = 8) {
  top_gears <- gear_country_year %>% group_by(gear) %>% summarise(g = sum(tonnes, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(g)) %>% slice_head(n = n_top_gears) %>% pull(gear)
  gear_country_year %>%
    mutate(gear_grp = ifelse(gear %in% top_gears, gear, "Other gear")) %>%
    group_by(country, gear_grp) %>% summarise(tonnes = sum(tonnes, na.rm = TRUE), .groups = "drop") %>%
    group_by(country) %>% mutate(pct = 100 * tonnes / sum(tonnes)) %>% ungroup()
}
