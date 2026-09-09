#!/usr/bin/env Rscript
#
# Full gear x species x sector x region analysis, + SAU vs FAO comparison
# ============================================================================
#
# Standalone-script counterpart to sau_west_med_analysis.Rmd -- same
# analysis, same outputs, but as PNG files (rather than inline plots in a
# knitted report) so it can run headless via `Rscript`. If you're working
# interactively, the .Rmd is usually nicer (plots render inline); this
# script is for automation / re-running from the command line.
#
# Builds on the outputs of sau_west_med_full.R:
#   sau_raw_combined_west_med.csv   (required)
#   sau_west_med_summary.xlsx       (needed for the country-level FAO
#                                     comparison and the reported-only
#                                     time series; species-level FAO
#                                     comparison doesn't need it)
#
# PART A (SAU-only, always runs):
#   - top 10 species by total tonnes
#   - gear x top-10-species x country (table + heatmap + bar charts)
#   - catch over time by sector and by gear, by country (line plots)
#   - FULL sector and gear summaries by country (every sector/gear, not
#     just a top-N sample)
#   - each top species' share of its country's catch
#
# PART B (best-effort, needs 'fishstat'; never blocks Part A):
#   - country-level (region) SAU vs FAO comparison, raw + standardized
#   - SAU total vs SAU reported-only vs FAO, by region (time series)
#   - species-level SAU vs FAO comparison for ALL matched top-10 species
#
# PART C (executive summary):
#   - one table per country tying sector, gear, species, and the FAO
#     comparison together, exported as its own workbook
#
# Usage
# -----
#   install.packages(c("dplyr", "tidyr", "openxlsx"))
#   install.packages("ggplot2")     # optional, nicer faceted plots
#   install.packages("fishstat")    # optional, for Part B
#   Rscript sau_gear_species_region_analysis.R
#
# Output (written next to this script)
# -------------------------------------
#   sau_gear_species_region.xlsx
#     Top10Species, GearBySpeciesByCountry, GearBySpeciesByCountry_AllYears,
#     SpeciesByCountry_Share, SectorByCountry_Full, GearByCountry_Full
#   sau_vs_fao_species_comparison.xlsx   (only if Part B succeeds)
#     Comparison, SpeciesSummary, Unmatched_species, Notes
#   sau_full_analysis_summary.xlsx
#     ExecutiveSummary, Top10Species, SectorByCountry, GearByCountry,
#     SpeciesByCountry, CountryVsFAO, SpeciesVsFAO  (FAO sheets only if available)
#   PNGs: sau_top10_species.png, sau_gear_species_heatmap.png,
#     sau_gear_species_bars.png, sau_sector_ts.png, sau_gear_ts.png,
#     sau_sector_share.png, sau_vs_fao_region_raw.png,
#     sau_vs_fao_region_index.png, sau_total_vs_reported_vs_fao.png,
#     sau_vs_fao_species.png
#
# Citation: Sea Around Us (seaaroundus.org/citation-policy) and
# FAO FishStat via the fishstat R package (github.com/sofia-taf/fishstat).

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(openxlsx)
})

OUT_DIR <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(), value = TRUE)[1],
                                     fixed = TRUE), mustWork = FALSE))
if (is.na(OUT_DIR) || OUT_DIR == "" || OUT_DIR == ".") OUT_DIR <- getwd()

RAW_CSV      <- file.path(OUT_DIR, "sau_raw_combined_west_med.csv")
SUMMARY_XLSX <- file.path(OUT_DIR, "sau_west_med_summary.xlsx")

N_TOP_SPECIES <- 10
N_TOP_GEARS   <- 8
YEAR_MIN <- 1994
YEAR_MAX <- 2019

TARGET_ISO3 <- c(Spain = "ESP", France = "FRA", Italy = "ITA",
                 Tunisia = "TUN", Algeria = "DZA", Morocco = "MAR")

readr_or_base_read_csv <- function(path) {
  if (requireNamespace("readr", quietly = TRUE)) {
    readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
  } else {
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  }
}

out_path <- function(name) file.path(OUT_DIR, name)

# =============================================================================
# PART A: SAU-only analysis
# =============================================================================

run_part_a <- function() {
  if (!file.exists(RAW_CSV)) {
    stop("Can't find ", RAW_CSV, " -- run sau_west_med_full.R first (same folder as this script).")
  }
  message("Reading ", RAW_CSV, " ...")
  raw <- readr_or_base_read_csv(RAW_CSV)
  
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
  missing <- setdiff(c("gear", "sci_name", "tonnes"), names(resolved))
  if (length(missing) > 0) {
    stop("Raw extract is missing required column(s): ", paste(missing, collapse = ", "),
         " -- check names(raw) and update `candidates` above.")
  }
  if (is.null(resolved$com_name)) resolved$com_name <- resolved$sci_name
  
  have_sector_col <- !is.null(resolved$sector)
  have_report_col <- !is.null(resolved$report)
  if (!have_sector_col) message("No sector column found -- sector breakdowns will be skipped.")
  if (!have_report_col) message("No reporting-status column found -- the reported-only comparison will be skipped.")
  
  raw <- raw %>%
    rename(.gear = !!resolved$gear, .sci = !!resolved$sci_name,
           .com = !!resolved$com_name, .tonnes = !!resolved$tonnes) %>%
    filter(!is.na(year), year >= YEAR_MIN, year <= YEAR_MAX)
  if (have_sector_col) raw <- raw %>% rename(.sector = !!resolved$sector)
  if (have_report_col) raw <- raw %>% rename(.report = !!resolved$report)
  
  message(sprintf("Loaded %d rows, %d-%d.", nrow(raw), YEAR_MIN, YEAR_MAX))
  
  have_ggplot2 <- requireNamespace("ggplot2", quietly = TRUE)
  if (have_ggplot2) suppressPackageStartupMessages(library(ggplot2))
  
  # ---- Top 10 species -------------------------------------------------------
  species_totals <- raw %>%
    group_by(scientific_name = .sci, common_name = .com) %>%
    summarise(total_tonnes = sum(.tonnes, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(total_tonnes))
  
  top10 <- species_totals %>% slice_head(n = N_TOP_SPECIES) %>%
    mutate(rank = row_number()) %>%
    dplyr::select(rank, scientific_name, common_name, total_tonnes)
  
  message("\nTop 10 species by total tonnes:")
  print(top10)
  
  png(out_path("sau_top10_species.png"), width = 900, height = 600, res = 110)
  barplot(rev(top10$total_tonnes), names.arg = rev(top10$common_name),
          horiz = TRUE, las = 1, col = "steelblue",
          xlab = sprintf("Total tonnes (%d-%d)", YEAR_MIN, YEAR_MAX), cex.names = 0.8,
          main = "Top 10 species by total catch")
  dev.off()
  
  # ---- Gear x top-10-species x country x year --------------------------------
  raw_top10 <- raw %>% filter(.sci %in% top10$scientific_name)
  
  gear_species_country_year <- raw_top10 %>%
    group_by(country, year, gear = .gear, scientific_name = .sci, common_name = .com) %>%
    summarise(tonnes = sum(.tonnes, na.rm = TRUE), .groups = "drop") %>%
    left_join(top10 %>% dplyr::select(scientific_name, rank), by = "scientific_name") %>%
    arrange(rank, country, year, desc(tonnes))
  
  gear_species_country_allyears <- gear_species_country_year %>%
    group_by(country, gear, scientific_name, common_name, rank) %>%
    summarise(tonnes = sum(tonnes, na.rm = TRUE), .groups = "drop") %>%
    arrange(rank, country, desc(tonnes))
  
  top_gears <- gear_species_country_allyears %>%
    group_by(gear) %>% summarise(g_tot = sum(tonnes, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(g_tot)) %>% slice_head(n = N_TOP_GEARS) %>% pull(gear)
  
  plot_df <- gear_species_country_allyears %>%
    mutate(gear_grp = ifelse(gear %in% top_gears, gear, "Other gear")) %>%
    group_by(country, gear_grp, scientific_name, common_name, rank) %>%
    summarise(tonnes = sum(tonnes, na.rm = TRUE), .groups = "drop") %>%
    mutate(species_label = sprintf("%02d. %s", rank, common_name))
  
  if (have_ggplot2) {
    p1 <- ggplot(plot_df, aes(x = gear_grp, y = reorder(species_label, -rank), fill = tonnes)) +
      geom_tile() + facet_wrap(~country) +
      scale_fill_viridis_c(name = "Tonnes", trans = "sqrt", option = "magma") +
      labs(title = "Gear x top-10-species tonnage, by country (region)", x = "Gear", y = NULL) +
      theme_minimal(base_size = 10) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), strip.text = element_text(face = "bold"))
    ggsave(out_path("sau_gear_species_heatmap.png"), p1, width = 11, height = 9, dpi = 110)
    
    p2 <- ggplot(plot_df, aes(x = reorder(species_label, rank), y = tonnes, fill = gear_grp)) +
      geom_col(position = "fill") + coord_flip() + facet_wrap(~country) +
      scale_y_continuous(labels = scales::percent_format()) +
      scale_fill_viridis_d(name = "Gear", option = "turbo") +
      labs(title = "Gear composition of each top-10 species, by country (region)",
           x = NULL, y = "Share of that species' catch") +
      theme_minimal(base_size = 10) + theme(strip.text = element_text(face = "bold"), legend.position = "bottom")
    ggsave(out_path("sau_gear_species_bars.png"), p2, width = 11, height = 9, dpi = 110)
  } else {
    message("Package 'ggplot2' not installed -- skipping the gear x species heatmap/bar PNGs ",
            "(install.packages('ggplot2') to get them).")
  }
  
  # ---- Catch over time by sector and by gear, by country ---------------------
  if (have_sector_col) {
    sector_ts <- raw %>%
      group_by(country, year, sector = .sector) %>%
      summarise(tonnes = sum(.tonnes, na.rm = TRUE), .groups = "drop")
    
    if (have_ggplot2) {
      p3 <- ggplot(sector_ts, aes(x = year, y = tonnes, color = sector)) +
        geom_line(linewidth = 0.9) + facet_wrap(~country, scales = "free_y") +
        scale_color_viridis_d(name = "Sector", option = "turbo") +
        labs(title = "SAU catch by sector over time, by country (region)", x = "Year", y = "Tonnes") +
        theme_minimal(base_size = 10) + theme(strip.text = element_text(face = "bold"), legend.position = "bottom")
      ggsave(out_path("sau_sector_ts.png"), p3, width = 11, height = 9, dpi = 110)
    } else {
      countries_here <- sort(unique(sector_ts$country))
      ncol_grid <- ceiling(sqrt(length(countries_here))); nrow_grid <- ceiling(length(countries_here) / ncol_grid)
      sector_levels <- sort(unique(sector_ts$sector))
      cols <- hcl.colors(length(sector_levels), "Dark 3")
      png(out_path("sau_sector_ts.png"), width = 350 * ncol_grid, height = 300 * nrow_grid, res = 100)
      par(mfrow = c(nrow_grid, ncol_grid), mar = c(4, 4, 3, 1))
      for (cty in countries_here) {
        d <- sector_ts %>% filter(country == cty)
        plot(range(d$year), range(d$tonnes, na.rm = TRUE), type = "n", xlab = "Year", ylab = "Tonnes", main = cty)
        for (sec in sector_levels) {
          dd <- d %>% filter(sector == sec) %>% arrange(year)
          lines(dd$year, dd$tonnes, col = cols[match(sec, sector_levels)], lwd = 2)
        }
      }
      plot.new(); legend("center", legend = sector_levels, col = cols, lwd = 2, bty = "n", cex = 0.8, title = "Sector")
      dev.off()
    }
  } else {
    sector_ts <- tibble()
  }
  
  gear_ts_top <- raw %>% group_by(gear = .gear) %>% summarise(g_tot = sum(.tonnes, na.rm = TRUE), .groups = "drop") %>%
    arrange(desc(g_tot)) %>% slice_head(n = N_TOP_GEARS) %>% pull(gear)
  gear_ts <- raw %>%
    mutate(gear_grp = ifelse(.gear %in% gear_ts_top, .gear, "Other gear")) %>%
    group_by(country, year, gear_grp) %>%
    summarise(tonnes = sum(.tonnes, na.rm = TRUE), .groups = "drop")
  
  if (have_ggplot2) {
    p4 <- ggplot(gear_ts, aes(x = year, y = tonnes, color = gear_grp)) +
      geom_line(linewidth = 0.9) + facet_wrap(~country, scales = "free_y") +
      scale_color_viridis_d(name = "Gear", option = "turbo") +
      labs(title = "SAU catch by gear over time, by country (region)", x = "Year", y = "Tonnes") +
      theme_minimal(base_size = 10) + theme(strip.text = element_text(face = "bold"), legend.position = "bottom")
    ggsave(out_path("sau_gear_ts.png"), p4, width = 11, height = 9, dpi = 110)
  } else {
    countries_here <- sort(unique(gear_ts$country))
    ncol_grid <- ceiling(sqrt(length(countries_here))); nrow_grid <- ceiling(length(countries_here) / ncol_grid)
    gear_levels <- sort(unique(gear_ts$gear_grp))
    cols <- hcl.colors(length(gear_levels), "Dark 3")
    png(out_path("sau_gear_ts.png"), width = 350 * ncol_grid, height = 300 * nrow_grid, res = 100)
    par(mfrow = c(nrow_grid, ncol_grid), mar = c(4, 4, 3, 1))
    for (cty in countries_here) {
      d <- gear_ts %>% filter(country == cty)
      plot(range(d$year), range(d$tonnes, na.rm = TRUE), type = "n", xlab = "Year", ylab = "Tonnes", main = cty)
      for (g in gear_levels) {
        dd <- d %>% filter(gear_grp == g) %>% arrange(year)
        lines(dd$year, dd$tonnes, col = cols[match(g, gear_levels)], lwd = 2)
      }
    }
    plot.new(); legend("center", legend = gear_levels, col = cols, lwd = 2, bty = "n", cex = 0.8, title = "Gear")
    dev.off()
  }
  
  # ---- Full sector and gear summaries, by country (no truncation) -----------
  country_totals <- raw %>% group_by(country) %>% summarise(country_total_tonnes = sum(.tonnes, na.rm = TRUE), .groups = "drop")
  
  sector_summary <- if (have_sector_col) {
    raw %>% group_by(country, sector = .sector) %>% summarise(tonnes = sum(.tonnes, na.rm = TRUE), .groups = "drop") %>%
      left_join(country_totals, by = "country") %>%
      mutate(pct_of_country_catch = 100 * tonnes / country_total_tonnes) %>%
      dplyr::select(-country_total_tonnes) %>%
      arrange(country, desc(tonnes))
  } else tibble()
  
  if (have_sector_col) {
    if (have_ggplot2) {
      p5 <- ggplot(sector_summary, aes(x = country, y = pct_of_country_catch, fill = sector)) +
        geom_col(position = "stack") + scale_fill_viridis_d(name = "Sector", option = "turbo") +
        labs(title = "Sector share of total catch, by country (region)", x = NULL, y = "% of country's total catch") +
        theme_minimal(base_size = 10) + theme(legend.position = "bottom")
      ggsave(out_path("sau_sector_share.png"), p5, width = 9, height = 5, dpi = 110)
    } else {
      wide <- sector_summary %>% tidyr::pivot_wider(id_cols = country, names_from = sector,
                                                    values_from = pct_of_country_catch, values_fill = 0)
      mat <- t(as.matrix(wide[,-1])); colnames(mat) <- wide$country
      png(out_path("sau_sector_share.png"), width = 900, height = 600, res = 110)
      barplot(mat, col = hcl.colors(nrow(mat), "Dark 3"), legend.text = rownames(mat),
              args.legend = list(x = "topright", cex = 0.7, bty = "n"),
              ylab = "% of country's total catch", main = "Sector share of total catch, by country")
      dev.off()
    }
  }
  
  gear_summary_full <- raw %>% group_by(country, gear = .gear) %>% summarise(tonnes = sum(.tonnes, na.rm = TRUE), .groups = "drop") %>%
    left_join(country_totals, by = "country") %>%
    mutate(pct_of_country_catch = 100 * tonnes / country_total_tonnes) %>%
    dplyr::select(-country_total_tonnes) %>%
    arrange(country, desc(tonnes))
  
  # ---- Each top species' share of its country's catch ------------------------
  species_country_share <- raw_top10 %>%
    group_by(country, scientific_name = .sci, common_name = .com) %>%
    summarise(species_tonnes = sum(.tonnes, na.rm = TRUE), .groups = "drop") %>%
    left_join(country_totals, by = "country") %>%
    mutate(pct_of_country_catch = 100 * species_tonnes / country_total_tonnes) %>%
    left_join(top10 %>% dplyr::select(scientific_name, rank), by = "scientific_name") %>%
    arrange(rank, desc(pct_of_country_catch))
  
  # ---- Export -----------------------------------------------------------------
  xlsx_path <- out_path("sau_gear_species_region.xlsx")
  wb <- createWorkbook()
  addWorksheet(wb, "Top10Species"); writeData(wb, "Top10Species", top10)
  addWorksheet(wb, "GearBySpeciesByCountry"); writeData(wb, "GearBySpeciesByCountry", gear_species_country_year)
  addWorksheet(wb, "GearBySpeciesByCountry_AllYears"); writeData(wb, "GearBySpeciesByCountry_AllYears", gear_species_country_allyears)
  addWorksheet(wb, "SpeciesByCountry_Share"); writeData(wb, "SpeciesByCountry_Share", species_country_share)
  if (have_sector_col) { addWorksheet(wb, "SectorByCountry_Full"); writeData(wb, "SectorByCountry_Full", sector_summary) }
  addWorksheet(wb, "GearByCountry_Full"); writeData(wb, "GearByCountry_Full", gear_summary_full)
  saveWorkbook(wb, xlsx_path, overwrite = TRUE)
  message(sprintf("\nWrote %s", xlsx_path))
  
  list(raw = raw, top10 = top10, raw_top10 = raw_top10,
       sector_summary = sector_summary, gear_summary_full = gear_summary_full,
       species_country_share = species_country_share,
       have_sector_col = have_sector_col, have_report_col = have_report_col,
       have_ggplot2 = have_ggplot2)
}

# =============================================================================
# PART B: SAU vs FAO comparison (best-effort)
# =============================================================================

run_part_b_region <- function(raw, have_report_col) {
  if (!requireNamespace("fishstat", quietly = TRUE)) {
    message("\n[Region-level FAO comparison skipped] Package 'fishstat' isn't installed.")
    return(NULL)
  }
  if (!file.exists(SUMMARY_XLSX)) {
    message("\n[Region-level FAO comparison skipped] Can't find ", SUMMARY_XLSX,
            " -- run sau_west_med_full.R first.")
    return(NULL)
  }
  suppressPackageStartupMessages(library(fishstat))
  
  sau_country_fao <- tryCatch(
    openxlsx::read.xlsx(SUMMARY_XLSX, sheet = "ByCountryYear_FAOarea37") %>%
      rename(sau_tonnes = tonnage) %>% mutate(year = as.integer(year)),
    error = function(e) { message("Couldn't read ByCountryYear_FAOarea37: ", conditionMessage(e)); NULL }
  )
  if (is.null(sau_country_fao) || nrow(sau_country_fao) == 0) {
    message("[Region-level FAO comparison skipped] ByCountryYear_FAOarea37 sheet is empty or missing.")
    return(NULL)
  }
  
  med_area_ids <- (area %>% filter(grepl("mediterranean", area_name, ignore.case = TRUE)))$area
  target_country_codes <- (country %>% filter(iso3 %in% TARGET_ISO3))$country
  tonnes_measure_codes <- (measure %>% filter(grepl("ton", measure_name, ignore.case = TRUE) |
                                                unit %in% c("t", "tonnes")))$measure
  
  fao_country <- capture %>%
    filter(area %in% med_area_ids, country %in% target_country_codes, measure %in% tonnes_measure_codes,
           year >= YEAR_MIN, year <= YEAR_MAX) %>%
    left_join(country %>% dplyr::select(country, country_name), by = "country") %>%
    group_by(country = country_name, year) %>%
    summarise(fao_tonnes = sum(value, na.rm = TRUE), .groups = "drop")
  
  region_comparison <- full_join(fao_country, sau_country_fao, by = c("country", "year")) %>%
    arrange(country, year) %>%
    group_by(country) %>%
    mutate(
      diff = sau_tonnes - fao_tonnes,
      pct_diff = ifelse(!is.na(fao_tonnes) & fao_tonnes != 0, 100 * diff / fao_tonnes, NA),
      first_common_year = suppressWarnings(min(year[!is.na(fao_tonnes) & !is.na(sau_tonnes)])),
      fao_base = fao_tonnes[year == first_common_year][1],
      sau_base = sau_tonnes[year == first_common_year][1],
      fao_index = 100 * fao_tonnes / fao_base,
      sau_index = 100 * sau_tonnes / sau_base
    ) %>%
    ungroup()
  
  message("\nSAU vs FAO by country/region (FAO Area 37):")
  print(region_comparison %>% group_by(country) %>%
          summarise(mean_fao = mean(fao_tonnes, na.rm = TRUE), mean_sau = mean(sau_tonnes, na.rm = TRUE),
                    mean_pct_diff = mean(pct_diff, na.rm = TRUE), .groups = "drop"))
  
  countries_here <- sort(unique(region_comparison$country))
  ncol_grid <- ceiling(sqrt(length(countries_here))); nrow_grid <- ceiling(length(countries_here) / ncol_grid)
  
  png(out_path("sau_vs_fao_region_raw.png"), width = 350 * ncol_grid, height = 300 * nrow_grid, res = 100)
  par(mfrow = c(nrow_grid, ncol_grid), mar = c(4, 4, 3, 1))
  for (cty in countries_here) {
    d <- region_comparison %>% filter(country == cty) %>% arrange(year)
    yrange <- suppressWarnings(range(c(d$fao_tonnes, d$sau_tonnes), na.rm = TRUE))
    plot(d$year, d$fao_tonnes, type = "l", col = "steelblue", lwd = 2, ylim = yrange,
         xlab = "Year", ylab = "Tonnes", main = cty)
    lines(d$year, d$sau_tonnes, col = "firebrick", lwd = 2, lty = 2)
    legend("topleft", legend = c("FAO (Area 37)", "SAU (Area 37)"), col = c("steelblue", "firebrick"),
           lty = c(1, 2), lwd = 2, bty = "n", cex = 0.7)
  }
  dev.off()
  message("Wrote ", out_path("sau_vs_fao_region_raw.png"))
  
  png(out_path("sau_vs_fao_region_index.png"), width = 350 * ncol_grid, height = 300 * nrow_grid, res = 100)
  par(mfrow = c(nrow_grid, ncol_grid), mar = c(4, 4, 3, 1))
  for (cty in countries_here) {
    d <- region_comparison %>% filter(country == cty) %>% arrange(year)
    yrange <- suppressWarnings(range(c(d$fao_index, d$sau_index), na.rm = TRUE))
    plot(d$year, d$fao_index, type = "l", col = "steelblue", lwd = 2, ylim = yrange,
         xlab = "Year", ylab = sprintf("Index (100 = %d)", d$first_common_year[1]), main = cty)
    lines(d$year, d$sau_index, col = "firebrick", lwd = 2, lty = 2)
    abline(h = 100, col = "grey70", lty = 3)
    legend("topleft", legend = c("FAO", "SAU"), col = c("steelblue", "firebrick"),
           lty = c(1, 2), lwd = 2, bty = "n", cex = 0.7)
  }
  dev.off()
  message("Wrote ", out_path("sau_vs_fao_region_index.png"))
  
  # ---- SAU total vs SAU reported-only vs FAO ---------------------------------
  if (have_report_col) {
    sau_reported_ts <- raw %>%
      group_by(country, year) %>%
      summarise(
        sau_total_tonnes = sum(.tonnes, na.rm = TRUE),
        sau_reported_tonnes = sum(.tonnes[grepl("^report", .report, ignore.case = TRUE) &
                                            !grepl("unreport", .report, ignore.case = TRUE)], na.rm = TRUE),
        .groups = "drop"
      )
    reported_comparison <- sau_reported_ts %>% left_join(fao_country, by = c("country", "year")) %>% arrange(country, year)
    
    png(out_path("sau_total_vs_reported_vs_fao.png"), width = 350 * ncol_grid, height = 300 * nrow_grid, res = 100)
    par(mfrow = c(nrow_grid, ncol_grid), mar = c(4, 4, 3, 1))
    for (cty in countries_here) {
      d <- reported_comparison %>% filter(country == cty) %>% arrange(year)
      yrange <- suppressWarnings(range(c(d$sau_total_tonnes, d$sau_reported_tonnes, d$fao_tonnes), na.rm = TRUE))
      plot(d$year, d$sau_total_tonnes, type = "l", col = "firebrick", lwd = 2, ylim = yrange,
           xlab = "Year", ylab = "Tonnes", main = cty)
      lines(d$year, d$sau_reported_tonnes, col = "darkorange", lwd = 2, lty = 2)
      lines(d$year, d$fao_tonnes, col = "steelblue", lwd = 2, lty = 3)
      legend("topleft", legend = c("SAU total (EEZ-based)", "SAU reported-only (EEZ-based)", "FAO (Area 37)"),
             col = c("firebrick", "darkorange", "steelblue"), lty = c(1, 2, 3), lwd = 2, bty = "n", cex = 0.65)
    }
    dev.off()
    message("Wrote ", out_path("sau_total_vs_reported_vs_fao.png"))
  } else {
    message("[SAU total vs reported-only comparison skipped] No reporting-status column in the raw extract.")
  }
  
  region_comparison
}

run_part_b_species <- function(top10, raw_top10) {
  if (!requireNamespace("fishstat", quietly = TRUE)) {
    message("\n[Species-level FAO comparison skipped] Package 'fishstat' isn't installed.")
    return(NULL)
  }
  suppressPackageStartupMessages(library(fishstat))
  
  med_area <- area %>% filter(grepl("mediterranean", area_name, ignore.case = TRUE))
  if (nrow(med_area) == 0) {
    message("[Species-level FAO comparison skipped] Couldn't find a Mediterranean area in fishstat::area.")
    return(NULL)
  }
  tonnes_measures <- measure %>% filter(grepl("ton", measure_name, ignore.case = TRUE) | unit %in% c("t", "tonnes"))
  target_countries <- country %>% filter(iso3 %in% TARGET_ISO3)
  
  norm <- function(x) trimws(tolower(x))
  species_lookup <- fishstat::species %>% filter(!is.na(scientific)) %>% mutate(.sci_norm = norm(scientific))
  
  top10_matched <- top10 %>%
    mutate(.sci_norm = norm(scientific_name)) %>%
    left_join(species_lookup %>% dplyr::select(species, .sci_norm, fao_species_name = species_name), by = ".sci_norm")
  
  unmatched <- top10_matched %>% filter(is.na(species)) %>%
    dplyr::select(rank, scientific_name, common_name, total_tonnes)
  matched <- top10_matched %>% filter(!is.na(species))
  
  if (nrow(unmatched) > 0) {
    message("\n! ", nrow(unmatched), " of the top ", N_TOP_SPECIES,
            " species couldn't be matched to an FAO scientific name (excluded from this comparison, ",
            "still fully present in Part A):")
    print(unmatched)
  }
  if (nrow(matched) == 0) {
    message("[Species-level FAO comparison skipped] None of the top species matched.")
    return(list(unmatched = unmatched))
  }
  
  fao_species <- capture %>%
    filter(area %in% med_area$area, country %in% target_countries$country, species %in% matched$species,
           measure %in% tonnes_measures$measure, year >= YEAR_MIN, year <= YEAR_MAX) %>%
    left_join(target_countries %>% dplyr::select(country, country_name), by = "country") %>%
    left_join(matched %>% dplyr::select(species, scientific_name, common_name), by = "species") %>%
    group_by(country = country_name, scientific_name, common_name, year) %>%
    summarise(fao_tonnes = sum(value, na.rm = TRUE), .groups = "drop")
  
  sau_species <- raw_top10 %>%
    group_by(country, scientific_name = .sci, year) %>%
    summarise(sau_tonnes = sum(.tonnes, na.rm = TRUE), .groups = "drop")
  
  comparison <- full_join(fao_species, sau_species, by = c("country", "scientific_name", "year")) %>%
    arrange(scientific_name, country, year) %>%
    mutate(diff = sau_tonnes - fao_tonnes,
           pct_diff = ifelse(!is.na(fao_tonnes) & fao_tonnes != 0, 100 * diff / fao_tonnes, NA))
  
  species_summary <- comparison %>%
    filter(!is.na(fao_tonnes), !is.na(sau_tonnes)) %>%
    group_by(scientific_name, common_name, country) %>%
    summarise(n_years = n(), mean_fao_tonnes = mean(fao_tonnes, na.rm = TRUE),
              mean_sau_tonnes = mean(sau_tonnes, na.rm = TRUE), mean_pct_diff = mean(pct_diff, na.rm = TRUE),
              .groups = "drop") %>%
    arrange(scientific_name, country)
  
  message("\nSpecies x country summary (mean_pct_diff = SAU vs FAO, positive = SAU higher):")
  print(species_summary, n = 100)
  
  # ---- Plot ALL matched top-10 species (not a sample) ------------------------
  species_for_plot <- unique(comparison$scientific_name)
  if (length(species_for_plot) > 0) {
    ncol_grid <- ceiling(sqrt(length(species_for_plot))); nrow_grid <- ceiling(length(species_for_plot) / ncol_grid)
    png(out_path("sau_vs_fao_species.png"), width = 350 * ncol_grid, height = 300 * nrow_grid, res = 100)
    par(mfrow = c(nrow_grid, ncol_grid), mar = c(4, 4, 3, 1))
    for (sp in species_for_plot) {
      d <- comparison %>% filter(scientific_name == sp) %>% group_by(year) %>%
        summarise(fao_tonnes = sum(fao_tonnes, na.rm = TRUE), sau_tonnes = sum(sau_tonnes, na.rm = TRUE), .groups = "drop") %>%
        arrange(year)
      common_lbl <- unique(comparison$common_name[comparison$scientific_name == sp])[1]
      yrange <- suppressWarnings(range(c(d$fao_tonnes, d$sau_tonnes), na.rm = TRUE))
      plot(d$year, d$fao_tonnes, type = "l", col = "steelblue", lwd = 2, ylim = yrange,
           xlab = "Year", ylab = "Tonnes", main = ifelse(is.na(common_lbl), sp, common_lbl), cex.main = 0.9)
      lines(d$year, d$sau_tonnes, col = "firebrick", lwd = 2, lty = 2)
      legend("topleft", legend = c("FAO", "SAU"), col = c("steelblue", "firebrick"), lty = c(1, 2), lwd = 2, bty = "n", cex = 0.7)
    }
    dev.off()
    message("Wrote ", out_path("sau_vs_fao_species.png"))
  }
  
  notes <- c(
    "FAO figures: fishstat R package, Capture Quantity table, FAO Major Fishing Area 37,",
    "tonnage-based measures only, matched species, 1994-2019.",
    "SAU figures: per-EEZ raw extracts (sau_west_med_full.R), aggregated to country x species x year.",
    "Species matched by scientific name (case-insensitive, trimmed) -- see Unmatched_species.",
    "FAO does not report catch by gear -- this comparison stops at species x country x year.",
    "No true Western-Mediterranean subdivision exists in FAO's own classification --",
    "Area 37 is the finest available and includes Italy's Adriatic and Ionian coasts.",
    "Data vintage: fishstat runs through 2024; SAU stops at 2019. Capped at 2019 here."
  )
  xlsx_path <- out_path("sau_vs_fao_species_comparison.xlsx")
  wb <- createWorkbook()
  addWorksheet(wb, "Comparison"); writeData(wb, "Comparison", comparison)
  addWorksheet(wb, "SpeciesSummary"); writeData(wb, "SpeciesSummary", species_summary)
  addWorksheet(wb, "Unmatched_species"); writeData(wb, "Unmatched_species", unmatched)
  addWorksheet(wb, "Notes"); writeData(wb, "Notes", data.frame(note = notes), colNames = FALSE)
  saveWorkbook(wb, xlsx_path, overwrite = TRUE)
  message(sprintf("Wrote %s", xlsx_path))
  
  list(comparison = comparison, species_summary = species_summary, unmatched = unmatched)
}

# =============================================================================
# PART C: Executive summary across all dimensions
# =============================================================================

run_part_c <- function(part_a, region_comparison, part_b_species) {
  total_by_country <- part_a$raw %>% group_by(country) %>%
    summarise(total_tonnes = sum(.tonnes, na.rm = TRUE), .groups = "drop")
  
  top_sector_by_country <- if (part_a$have_sector_col && nrow(part_a$sector_summary) > 0) {
    part_a$sector_summary %>% group_by(country) %>% slice_max(tonnes, n = 1, with_ties = FALSE) %>%
      transmute(country, top_sector = sector, top_sector_pct = round(pct_of_country_catch, 1))
  } else {
    tibble(country = total_by_country$country, top_sector = NA_character_, top_sector_pct = NA_real_)
  }
  
  top_gear_by_country <- part_a$gear_summary_full %>% group_by(country) %>% slice_max(tonnes, n = 1, with_ties = FALSE) %>%
    transmute(country, top_gear = gear, top_gear_pct = round(pct_of_country_catch, 1))
  
  top_species_by_country <- part_a$species_country_share %>% group_by(country) %>%
    slice_max(species_tonnes, n = 1, with_ties = FALSE) %>%
    transmute(country, top_species = common_name, top_species_pct = round(pct_of_country_catch, 1))
  
  fao_summary_by_country <- if (!is.null(region_comparison) && nrow(region_comparison) > 0) {
    region_comparison %>% filter(!is.na(fao_tonnes), !is.na(sau_tonnes)) %>%
      group_by(country) %>%
      summarise(mean_fao_tonnes = round(mean(fao_tonnes, na.rm = TRUE), 0),
                mean_sau_tonnes_area37 = round(mean(sau_tonnes, na.rm = TRUE), 0),
                mean_pct_diff_sau_vs_fao = round(mean(pct_diff, na.rm = TRUE), 1), .groups = "drop")
  } else {
    message("[Executive summary] SAU-vs-FAO country comparison not available -- those columns will be NA.")
    tibble(country = total_by_country$country, mean_fao_tonnes = NA_real_,
           mean_sau_tonnes_area37 = NA_real_, mean_pct_diff_sau_vs_fao = NA_real_)
  }
  
  master_summary <- total_by_country %>%
    left_join(top_sector_by_country, by = "country") %>%
    left_join(top_gear_by_country, by = "country") %>%
    left_join(top_species_by_country, by = "country") %>%
    left_join(fao_summary_by_country, by = "country") %>%
    arrange(desc(total_tonnes))
  
  message("\nExecutive summary by country (region):")
  print(master_summary)
  
  xlsx_path <- out_path("sau_full_analysis_summary.xlsx")
  wb <- createWorkbook()
  addWorksheet(wb, "ExecutiveSummary"); writeData(wb, "ExecutiveSummary", master_summary)
  addWorksheet(wb, "Top10Species"); writeData(wb, "Top10Species", part_a$top10)
  if (part_a$have_sector_col) { addWorksheet(wb, "SectorByCountry"); writeData(wb, "SectorByCountry", part_a$sector_summary) }
  addWorksheet(wb, "GearByCountry"); writeData(wb, "GearByCountry", part_a$gear_summary_full)
  addWorksheet(wb, "SpeciesByCountry"); writeData(wb, "SpeciesByCountry", part_a$species_country_share)
  if (!is.null(region_comparison) && nrow(region_comparison) > 0) {
    addWorksheet(wb, "CountryVsFAO"); writeData(wb, "CountryVsFAO", region_comparison)
  }
  if (!is.null(part_b_species) && !is.null(part_b_species$species_summary) && nrow(part_b_species$species_summary) > 0) {
    addWorksheet(wb, "SpeciesVsFAO"); writeData(wb, "SpeciesVsFAO", part_b_species$species_summary)
  }
  saveWorkbook(wb, xlsx_path, overwrite = TRUE)
  message(sprintf("Wrote %s -- one workbook with every dimension (sector, gear, species, country, FAO) together.", xlsx_path))
  
  master_summary
}

# =============================================================================
# Main
# =============================================================================

main <- function() {
  part_a <- run_part_a()
  
  region_comparison <- tryCatch(
    run_part_b_region(part_a$raw, part_a$have_report_col),
    error = function(e) { message("\n[Region-level FAO comparison skipped due to an error] ", conditionMessage(e)); NULL }
  )
  
  part_b_species <- tryCatch(
    run_part_b_species(part_a$top10, part_a$raw_top10),
    error = function(e) { message("\n[Species-level FAO comparison skipped due to an error] ", conditionMessage(e)); NULL }
  )
  
  tryCatch(
    run_part_c(part_a, region_comparison, part_b_species),
    error = function(e) message("\n[Executive summary skipped due to an error] ", conditionMessage(e))
  )
  
  message("\nAll done. Remember to cite Sea Around Us: https://www.seaaroundus.org/citation-policy/")
}

main()