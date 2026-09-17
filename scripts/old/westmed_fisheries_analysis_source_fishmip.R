#!/usr/bin/env Rscript
#
# westmed_fisheries_analysis_source_fishmip.R
# ============================================================================
# FishMIP/ISIMIP3a data-source module for the western Mediterranean
# fisheries analysis. Reads the pre-clipped regional parquet extracts from
# the FishMIP Input Explorer (fishmip.global-ecosystem-model.cloud.edu.au)
# for the "Western Mediterranean Sea" regional model: nominal fishing
# EFFORT (kW x days at sea -- a variable SAU's own API doesn't expose at
# all) and FishMIP's own SAU-derived calibration CATCH (reported/IUU/
# discards split).
#
# This file only LOADS and RESHAPES data -- no plotting, no file export.
#
# Confirmed directly against the actual files (not guessed):
#   - region is constant "Western Mediterranean Sea" for every row -- this
#     download is already clipped to ONE FishMIP regional polygon, NOT
#     split further by GSA.
#   - saup is a standard ISO 3166-1 numeric country code (Algeria=12,
#     France=250, Italy=380, Morocco=504, Spain=724, Tunisia=788).
#   - sector is spelled differently between the two files ("Artisanal"/
#     "Industrial" in effort, lower-case in catch) -- both are normalized
#     to Title Case here so they line up.
#   - catch's total_catch = reported + iuu + discards, verified exactly.
#   - catch is itself built FROM Sea Around Us data for FishMIP's own model
#     calibration -- it is NOT an independent catch source from SAU's own
#     extract, just aggregated to FishMIP's coarser functional groups
#     instead of species. Effort, by contrast, genuinely is independent --
#     SAU doesn't have an effort variable at all.

suppressPackageStartupMessages({
  library(dplyr)
})

FISHMIP_TARGET_SAUP <- c(Algeria = 12, France = 250, Italy = 380, Morocco = 504, Spain = 724, Tunisia = 788)
fishmip_saup_to_country <- function(x) names(FISHMIP_TARGET_SAUP)[match(x, FISHMIP_TARGET_SAUP)]

#' TRUE if the 'arrow' package (needed to read .parquet files) is available.
fishmip_available <- function() requireNamespace("arrow", quietly = TRUE)

#' Read and clean the FishMIP effort parquet: maps saup -> country name,
#' normalizes sector casing, and filters to the target countries/years.
fishmip_load_effort <- function(effort_parquet, year_min = 1994, year_max = 2019) {
  if (!fishmip_available()) stop("Package 'arrow' isn't installed -- install.packages('arrow').")
  if (!file.exists(effort_parquet)) stop("Can't find ", effort_parquet)
  effort <- arrow::read_parquet(effort_parquet) %>%
    mutate(year = as.integer(year), sector = tools::toTitleCase(tolower(sector)),
           country = fishmip_saup_to_country(saup)) %>%
    filter(!is.na(country), year >= year_min, year <= year_max)
  if (nrow(effort) == 0) stop("No effort rows matched the target countries/year range.")
  message(sprintf("[FishMIP] Effort: %d rows, %d-%d.", nrow(effort), year_min, year_max))
  effort
}

#' Read and clean the FishMIP calibration-catch parquet, same treatment.
fishmip_load_catch <- function(catch_parquet, year_min = 1994, year_max = 2019) {
  if (!fishmip_available()) stop("Package 'arrow' isn't installed -- install.packages('arrow').")
  if (!file.exists(catch_parquet)) stop("Can't find ", catch_parquet)
  catch <- arrow::read_parquet(catch_parquet) %>%
    mutate(year = as.integer(year), sector = tools::toTitleCase(tolower(sector)),
           country = fishmip_saup_to_country(saup)) %>%
    filter(!is.na(country), year >= year_min, year <= year_max)
  if (nrow(catch) == 0) message("[FishMIP] No catch rows matched the target countries/year range.")
  else message(sprintf("[FishMIP] Catch: %d rows, %d-%d.", nrow(catch), year_min, year_max))
  catch
}

#' Effort by country x year x sector x gear x functional-group (full detail).
fishmip_effort_by_country_year_sector_gear <- function(effort) {
  effort %>% group_by(country, year, sector, gear, f_group) %>%
    summarise(nom_active = sum(nom_active, na.rm = TRUE), .groups = "drop")
}

#' Effort by sector, with each sector's % share of its country's total.
fishmip_effort_sector_summary <- function(effort) {
  effort %>% group_by(country, sector) %>% summarise(nom_active = sum(nom_active, na.rm = TRUE), .groups = "drop") %>%
    group_by(country) %>% mutate(pct_of_country = 100 * nom_active / sum(nom_active)) %>% ungroup() %>%
    arrange(country, desc(nom_active))
}

#' Effort by gear (full, no top-N truncation), with % share of country total.
fishmip_effort_gear_summary <- function(effort) {
  effort %>% group_by(country, gear) %>% summarise(nom_active = sum(nom_active, na.rm = TRUE), .groups = "drop") %>%
    group_by(country) %>% mutate(pct_of_country = 100 * nom_active / sum(nom_active)) %>% ungroup() %>%
    arrange(country, desc(nom_active))
}

#' Effort over time by sector, country x year (for time-series plots).
fishmip_effort_sector_timeseries <- function(effort) {
  effort %>% group_by(country, year, sector) %>% summarise(nom_active = sum(nom_active, na.rm = TRUE), .groups = "drop")
}

#' Effort over time by gear (top-N + "Other gear"), country x year.
fishmip_effort_gear_timeseries <- function(effort, n_top_gears = 8) {
  top_gears <- effort %>% group_by(gear) %>% summarise(g_tot = sum(nom_active, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(g_tot)) %>% slice_head(n = n_top_gears) %>% pull(gear)
  effort %>% mutate(gear_grp = ifelse(gear %in% top_gears, gear, "Other gear")) %>%
    group_by(country, year, gear_grp) %>% summarise(nom_active = sum(nom_active, na.rm = TRUE), .groups = "drop")
}

#' Effort by gear, country x year (full detail, feeds the gear-share
#' comparison against SAU's catch-by-gear).
fishmip_effort_gear_by_country_year <- function(effort) {
  effort %>% group_by(country, year, gear) %>% summarise(nom_active = sum(nom_active, na.rm = TRUE), .groups = "drop")
}

#' % of each country's effort (kW-days) by gear, top-N + "Other gear".
fishmip_effort_gear_share <- function(gear_country_year, n_top_gears = 8) {
  top_gears <- gear_country_year %>% group_by(gear) %>% summarise(g = sum(nom_active, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(g)) %>% slice_head(n = n_top_gears) %>% pull(gear)
  gear_country_year %>%
    mutate(gear_grp = ifelse(gear %in% top_gears, gear, "Other gear")) %>%
    group_by(country, gear_grp) %>% summarise(nom_active = sum(nom_active, na.rm = TRUE), .groups = "drop") %>%
    group_by(country) %>% mutate(pct = 100 * nom_active / sum(nom_active)) %>% ungroup()
}

#' Catch by country x year x sector x functional-group (full detail).
fishmip_catch_by_country_year_sector_fgroup <- function(catch) {
  if (nrow(catch) == 0) return(tibble())
  catch %>% group_by(country, year, sector, f_group) %>%
    summarise(reported = sum(reported, na.rm = TRUE), iuu = sum(iuu, na.rm = TRUE),
              discards = sum(discards, na.rm = TRUE), total_catch = sum(total_catch, na.rm = TRUE), .groups = "drop")
}

#' Catch composition by country: reported/IUU/discards/total, plus each's
#' % of the total.
fishmip_catch_country_summary <- function(catch_summary) {
  if (nrow(catch_summary) == 0) return(tibble())
  catch_summary %>% group_by(country) %>%
    summarise(reported = sum(reported, na.rm = TRUE), iuu = sum(iuu, na.rm = TRUE),
              discards = sum(discards, na.rm = TRUE), total_catch = sum(total_catch, na.rm = TRUE), .groups = "drop") %>%
    mutate(pct_iuu_of_total = 100 * iuu / total_catch, pct_discards_of_total = 100 * discards / total_catch) %>%
    arrange(desc(total_catch))
}

#' Catch over time, country x year: reported/IUU/discards (for time-series
#' plots showing the composition of FishMIP's catch reconstruction).
fishmip_catch_timeseries <- function(catch) {
  if (nrow(catch) == 0) return(tibble())
  catch %>% group_by(country, year) %>%
    summarise(reported = sum(reported, na.rm = TRUE), iuu = sum(iuu, na.rm = TRUE),
              discards = sum(discards, na.rm = TRUE), .groups = "drop")
}

#' FishMIP's "reported" catch, by country x year -- one row per
#' source/approach, ready to bind_rows() into the multi-source comparison.
fishmip_reported_by_country_year <- function(catch_summary) {
  if (nrow(catch_summary) == 0) return(tibble())
  catch_summary %>% group_by(country, year) %>% summarise(tonnes = sum(reported, na.rm = TRUE), .groups = "drop") %>%
    mutate(source = "FishMIP reported")
}

#' FishMIP's "total" catch (reported + IUU + discards), by country x year.
fishmip_total_by_country_year <- function(catch_summary) {
  if (nrow(catch_summary) == 0) return(tibble())
  catch_summary %>% group_by(country, year) %>% summarise(tonnes = sum(total_catch, na.rm = TRUE), .groups = "drop") %>%
    mutate(source = "FishMIP total (+IUU+discards)")
}
