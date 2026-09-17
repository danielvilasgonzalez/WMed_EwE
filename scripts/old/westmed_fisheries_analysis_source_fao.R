#!/usr/bin/env Rscript
#
# westmed_fisheries_analysis_source_fao.R
# ============================================================================
# FAO FishStat data-source module for the western Mediterranean fisheries
# analysis. Wraps the `fishstat` R package (CRAN; mirrors
# github.com/sofia-taf/fishstat) -- official capture statistics, used as
# the "official reporting" reference point against SAU's reconstruction and
# FishMIP's calibration catch.
#
# This file only LOADS and RESHAPES data -- no plotting, no file export.
# Every function is best-effort: if 'fishstat' isn't installed, or nothing
# matches, they return an empty tibble (or throw, for fao_load_tables()
# itself) rather than silently guessing -- the main script decides what to
# do about a missing source.

suppressPackageStartupMessages({
  library(dplyr)
})

#' TRUE if the 'fishstat' package is available.
fao_available <- function() requireNamespace("fishstat", quietly = TRUE)

#' Load fishstat's tables. Errors if the package isn't installed -- callers
#' should check fao_available() first if they want to degrade gracefully
#' instead.
fao_load_tables <- function() {
  if (!fao_available()) stop("Package 'fishstat' isn't installed -- install.packages('fishstat').")
  suppressPackageStartupMessages(library(fishstat))
  list(capture = capture, country = country, area = area, measure = measure, species = species)
}

#' Official FAO catch, by country x year, for a given FAO major area
#' (default: anything with "mediterranean" in its name, i.e. Area 37) and a
#' set of target ISO3 country codes. Returns columns (country, year,
#' tonnes, source) -- the `source` column is fixed text so this bind_rows()s
#' straight into the multi-source comparison alongside the SAU/FishMIP
#' tables. Empty tibble (with a message) if 'fishstat' isn't installed.
fao_country_year <- function(target_iso3, year_min = 1994, year_max = 2019, area_pattern = "mediterranean") {
  if (!fao_available()) {
    message("[FAO] Package 'fishstat' isn't installed -- FAO series skipped (install.packages('fishstat')).")
    return(tibble())
  }
  tabs <- fao_load_tables()
  med_area_ids <- (tabs$area %>% filter(grepl(area_pattern, area_name, ignore.case = TRUE)))$area
  target_country_codes <- (tabs$country %>% filter(iso3 %in% target_iso3))$country
  tonnes_measure_codes <- (tabs$measure %>%
                              filter(grepl("ton", measure_name, ignore.case = TRUE) | unit %in% c("t", "tonnes")))$measure

  out <- tabs$capture %>%
    filter(area %in% med_area_ids, country %in% target_country_codes, measure %in% tonnes_measure_codes,
           year >= year_min, year <= year_max) %>%
    left_join(tabs$country %>% dplyr::select(country, country_name), by = "country") %>%
    group_by(country = country_name, year) %>%
    summarise(tonnes = sum(value, na.rm = TRUE), .groups = "drop") %>%
    mutate(source = "FAO (Area 37, official)")

  message(sprintf("[FAO] %d country-year rows.", nrow(out)))
  out
}

#' Match a set of scientific names (e.g. SAU's top-10 species) to FAO's own
#' species table, by normalized (trimmed, lowercased) scientific name.
#' Returns the input table with `species` (FAO's code, NA if unmatched) and
#' `fao_species_name` columns added.
fao_match_species <- function(species_df, scientific_name_col = "scientific_name") {
  if (!fao_available()) {
    message("[FAO] Package 'fishstat' isn't installed -- species matching skipped.")
    species_df$species <- NA
    species_df$fao_species_name <- NA
    return(species_df)
  }
  tabs <- fao_load_tables()
  norm <- function(x) trimws(tolower(x))
  species_lookup <- tabs$species %>% filter(!is.na(scientific)) %>% mutate(.sci_norm = norm(scientific))

  species_df %>%
    mutate(.sci_norm = norm(.data[[scientific_name_col]])) %>%
    left_join(species_lookup %>% dplyr::select(species, .sci_norm, fao_species_name = species_name), by = ".sci_norm") %>%
    dplyr::select(-.sci_norm)
}

#' Species-level FAO catch, by country x scientific_name x year, for a set
#' of already-FAO-matched species (i.e. the output of fao_match_species()
#' filtered to rows with a non-NA `species` code) and a set of target ISO3
#' codes. Returns (country, scientific_name, common_name, year, fao_tonnes).
fao_species_country_year <- function(matched_species_df, target_iso3, year_min = 1994, year_max = 2019,
                                      area_pattern = "mediterranean") {
  if (!fao_available()) return(tibble())
  tabs <- fao_load_tables()
  med_area_ids <- (tabs$area %>% filter(grepl(area_pattern, area_name, ignore.case = TRUE)))$area
  target_country_codes <- (tabs$country %>% filter(iso3 %in% target_iso3))$country
  tonnes_measure_codes <- (tabs$measure %>%
                              filter(grepl("ton", measure_name, ignore.case = TRUE) | unit %in% c("t", "tonnes")))$measure

  tabs$capture %>%
    filter(area %in% med_area_ids, country %in% target_country_codes, species %in% matched_species_df$species,
           measure %in% tonnes_measure_codes, year >= year_min, year <= year_max) %>%
    left_join(tabs$country %>% dplyr::select(country, country_name), by = "country") %>%
    left_join(matched_species_df %>% dplyr::select(species, scientific_name, common_name), by = "species") %>%
    group_by(country = country_name, scientific_name, common_name, year) %>%
    summarise(fao_tonnes = sum(value, na.rm = TRUE), .groups = "drop")
}
