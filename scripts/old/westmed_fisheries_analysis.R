#!/usr/bin/env Rscript
#
# westmed_fisheries_analysis.R -- MAIN script
# ============================================================================
#
# Western Mediterranean fisheries: gear x species x region analysis, plus a
# 4-source, 5-approach comparison of catch/effort trends -- SAU (with and
# without unreported catch), FAO, and FishMIP.
#
# This is the orchestrator: it sources the three data-source modules below
# (one per source, each responsible only for loading/reshaping its own
# data -- no plotting, no export) and does all the analysis, plotting, and
# exporting itself. If you need to change how a source's data is loaded,
# edit that source's module; if you want different plots or exports, this
# file is the only one that needs to change.
#
#   westmed_fisheries_analysis_source_sau.R      -- Sea Around Us
#   westmed_fisheries_analysis_source_fao.R      -- FAO FishStat
#   westmed_fisheries_analysis_source_fishmip.R  -- FishMIP/ISIMIP3a
#
# Put all three of those files in the same folder as this script.
#
# SOURCES / APPROACHES COVERED
# -------------------------------
#   - SAU total         = the full SAU reconstruction (reported +
#                          unreported + discarded + IUU)
#   - SAU reported-only  = the SAU subset flagged reporting_status ==
#                          "Reported"
#   - FAO                = official capture statistics (fishstat package),
#                          FAO Major Area 37
#   - FishMIP reported   = the "reported" component of FishMIP's own
#                          SAU-derived calibration catch
#   - FishMIP total      = FishMIP's reported + IUU + discards
#   - Gear               = SAU catch-by-gear (tonnes) vs FishMIP
#                          effort-by-gear (kW-days) -- shown side by side,
#                          never merged (different units and gear
#                          taxonomies -- see Part D below)
#
# Prerequisites
# ---------------
#   1. sau_west_med_full.R must have been run already (hits the live SAU
#      API -- a separate, longer step), producing sau_raw_combined_west_med.csv
#      in this same folder.
#   2. The FishMIP parquet files, downloaded from the FishMIP Input
#      Explorer, in this same folder:
#        effort_histsoc_1841_2017_western-mediterranean-sea.parquet
#        calibration_catch_histsoc_1850_2017_western-mediterranean-sea.parquet
#      (FishMIP is optional -- if these aren't there, Parts A-B-D still run
#      on SAU + FAO alone, and Part C / the FishMIP columns of Part D are
#      skipped with a clear message.)
#   3. install.packages(c("dplyr", "tidyr", "openxlsx"))
#      install.packages("ggplot2")   # optional, nicer faceted plots
#      install.packages("fishstat")  # optional, for the FAO side
#      install.packages("arrow")     # optional, for the FishMIP side
#
# Usage
# -----
#   Rscript westmed_fisheries_analysis.R
#
# Output (written next to this script)
# ---------------------------------------
#   westmed_fisheries_analysis.xlsx -- ONE workbook, every sheet:
#     Top10Species, GearSpeciesByCountry, SectorByCountry, GearByCountry,
#     SpeciesShareByCountry, SAUvsFAO_ByCountry, SAUvsFAO_BySpecies,
#     FishMIP_EffortFullDetail, FishMIP_EffortSectorSummary,
#     FishMIP_EffortGearSummary, FishMIP_CatchFullDetail,
#     FishMIP_CatchCountrySummary, MasterComparison_ByCountryYear,
#     MasterComparison_Summary, GearShare_SAU_Catch, GearShare_FishMIP_Effort
#   PNGs: westmed_top10_species.png, westmed_gear_species_heatmap.png,
#     westmed_gear_species_bars.png, westmed_sector_ts.png, westmed_gear_ts.png,
#     westmed_sector_share.png, westmed_sau_vs_fao_country.png,
#     westmed_sau_vs_fao_country_index.png, westmed_sau_vs_fao_species.png,
#     westmed_fishmip_effort_by_sector_ts.png, westmed_fishmip_effort_by_gear_ts.png,
#     westmed_fishmip_catch_composition_ts.png,
#     westmed_master_comparison_raw.png, westmed_master_comparison_index.png,
#     westmed_gear_share_sau_vs_fishmip.png
#
# Citation: Sea Around Us (seaaroundus.org/citation-policy), FAO FishStat
# (github.com/sofia-taf/fishstat), FishMIP/ISIMIP3a (Novaglio, Rousseau,
# Watson & Blanchard 2024, doi.org/10.48364/ISIMIP.240282).

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(openxlsx)
})

OUT_DIR <- dirname(normalizePath(sub("--file=", "", grep("--file=", commandArgs(), value = TRUE)[1],
                                     fixed = TRUE), mustWork = FALSE))
if (is.na(OUT_DIR) || OUT_DIR == "" || OUT_DIR == ".") OUT_DIR <- getwd()
out_path <- function(name) file.path(OUT_DIR, name)

# ---- Source the three data-source modules -----------------------------
for (mod in c("westmed_fisheries_analysis_source_sau.R",
              "westmed_fisheries_analysis_source_fao.R",
              "westmed_fisheries_analysis_source_fishmip.R")) {
  mod_path <- out_path(mod)
  if (!file.exists(mod_path)) stop("Can't find ", mod_path, " -- put it in the same folder as this script.")
  source(mod_path)
}

# ---- Config -------------------------------------------------------------
RAW_CSV        <- out_path("sau_raw_combined_west_med.csv")
EFFORT_PARQUET <- out_path("effort_histsoc_1841_2017_western-mediterranean-sea.parquet")
CATCH_PARQUET  <- out_path("calibration_catch_histsoc_1850_2017_western-mediterranean-sea.parquet")

N_TOP_SPECIES <- 10
N_TOP_GEARS   <- 8
YEAR_MIN <- 1994
YEAR_MAX <- 2019
TARGET_ISO3 <- c(Spain = "ESP", France = "FRA", Italy = "ITA", Tunisia = "TUN", Algeria = "DZA", Morocco = "MAR")

# ---- Shared plot theme & palette --------------------------------------------
# White chart background + a validated, colorblind-safe categorical palette
# (8 hues, ordered so adjacent pairs clear CVD-safety checks -- see the
# accompanying data-viz reference). One fixed set used everywhere below.
have_ggplot2 <- requireNamespace("ggplot2", quietly = TRUE)
if (have_ggplot2) suppressPackageStartupMessages(library(ggplot2))

WESTMED_PALETTE <- c("#2a78d6", "#eb6834", "#1baf7a", "#eda100",
                     "#e87ba4", "#008300", "#4a3aa7", "#e34948")
westmed_colors <- function(n) {
  if (n <= length(WESTMED_PALETTE)) WESTMED_PALETTE[seq_len(n)] else grDevices::colorRampPalette(WESTMED_PALETTE)(n)
}
westmed_png <- function(path, width, height, res = 100) {
  grDevices::png(path, width = width, height = height, res = res, bg = "white")
}
theme_westmed <- function(base_size = 11) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      plot.background   = ggplot2::element_rect(fill = "white", color = NA),
      panel.background  = ggplot2::element_rect(fill = "white", color = NA),
      legend.background = ggplot2::element_rect(fill = "white", color = NA),
      legend.key        = ggplot2::element_rect(fill = "white", color = NA),
      strip.background  = ggplot2::element_rect(fill = "#f2f1ee", color = NA),
      strip.text        = ggplot2::element_text(face = "bold", color = "#0b0b0b"),
      panel.grid.major  = ggplot2::element_line(color = "#e1e0d9", linewidth = 0.3),
      panel.grid.minor  = ggplot2::element_blank(),
      axis.line         = ggplot2::element_line(color = "#c3c2b7", linewidth = 0.3),
      axis.text         = ggplot2::element_text(color = "#52514e"),
      axis.title        = ggplot2::element_text(color = "#0b0b0b"),
      plot.title        = ggplot2::element_text(face = "bold", color = "#0b0b0b"),
      plot.subtitle     = ggplot2::element_text(color = "#52514e"),
      legend.position   = "bottom",
      legend.title      = ggplot2::element_text(color = "#0b0b0b", face = "bold"),
      legend.text       = ggplot2::element_text(color = "#52514e")
    )
}

# a small helper: faceted-by-country line plot, ggplot2 if available, a
# base-R multi-panel fallback otherwise. Used by several sections below so
# the fallback logic isn't repeated five times.
plot_country_lines <- function(df, y_col, group_col, png_path, title, subtitle = NULL, ylab = "Tonnes",
                               width = 11, height = 9) {
  if (have_ggplot2) {
    p <- ggplot(df, aes(x = year, y = .data[[y_col]], color = .data[[group_col]], linetype = .data[[group_col]])) +
      geom_line(linewidth = 0.9) + facet_wrap(~country, scales = "free_y") +
      scale_color_manual(name = tools::toTitleCase(gsub("_", " ", group_col)), values = westmed_colors(dplyr::n_distinct(df[[group_col]]))) +
      labs(title = title, subtitle = subtitle, x = "Year", y = ylab) +
      theme_westmed(base_size = 10) + guides(color = guide_legend(nrow = 3))
    ggsave(png_path, p, width = width, height = height, dpi = 110, bg = "white")
  } else {
    countries_here <- sort(unique(df$country))
    ncol_grid <- ceiling(sqrt(length(countries_here))); nrow_grid <- ceiling(length(countries_here) / ncol_grid)
    groups_here <- unique(df[[group_col]])
    cols <- westmed_colors(length(groups_here))
    westmed_png(png_path, width = 350 * ncol_grid, height = 300 * nrow_grid, res = 100)
    par(mfrow = c(nrow_grid, ncol_grid), mar = c(4, 4, 3, 1))
    for (cty in countries_here) {
      d <- df %>% filter(country == cty)
      plot(range(d$year), suppressWarnings(range(d[[y_col]], na.rm = TRUE)), type = "n", xlab = "Year", ylab = ylab, main = cty)
      for (g in groups_here) {
        dd <- d %>% filter(.data[[group_col]] == g) %>% arrange(year)
        if (nrow(dd) > 0) lines(dd$year, dd[[y_col]], col = cols[match(g, groups_here)], lwd = 2)
      }
    }
    plot.new(); legend("center", legend = groups_here, col = cols, lwd = 2, bty = "n", cex = 0.7)
    dev.off()
  }
}

wb <- createWorkbook()
add_sheet <- function(name, data) { addWorksheet(wb, name); writeData(wb, name, data) }

cat("\n============================================================\n")
cat("PART A -- SAU: top species, gear x species x region\n")
cat("============================================================\n")

raw <- sau_load_raw(RAW_CSV, YEAR_MIN, YEAR_MAX)
top10 <- sau_top_species(raw, N_TOP_SPECIES)
gsc <- sau_gear_species_country(raw, top10, N_TOP_GEARS)
sector_ts <- sau_sector_timeseries(raw)
gear_ts <- sau_gear_timeseries(raw, N_TOP_GEARS)
sector_summary_full <- sau_sector_summary_full(raw)
gear_summary_full <- sau_gear_summary_full(raw)
species_share <- sau_species_share(raw, top10)

cat("\nTop 10 species by total tonnes:\n"); print(top10)
add_sheet("Top10Species", top10)
add_sheet("GearSpeciesByCountry", gsc$gear_species_country_allyears)
if (nrow(sector_summary_full) > 0) add_sheet("SectorByCountry", sector_summary_full)
add_sheet("GearByCountry", gear_summary_full)
add_sheet("SpeciesShareByCountry", species_share)

if (have_ggplot2) {
  westmed_png(out_path("westmed_top10_species.png"), width = 900, height = 600, res = 110)
} else {
  westmed_png(out_path("westmed_top10_species.png"), width = 900, height = 600, res = 110)
}
barplot(rev(top10$total_tonnes), names.arg = rev(top10$common_name), horiz = TRUE, las = 1,
        col = WESTMED_PALETTE[1], xlab = sprintf("Total tonnes (%d-%d)", YEAR_MIN, YEAR_MAX),
        cex.names = 0.8, main = "Top 10 species by total catch")
dev.off()

if (have_ggplot2) {
  p_heat <- ggplot(gsc$plot_df, aes(x = gear_grp, y = reorder(species_label, -rank), fill = tonnes)) +
    geom_tile() + facet_wrap(~country) +
    scale_fill_gradient(name = "Tonnes", trans = "sqrt", low = "#cde2fb", high = "#0d366b") +
    labs(title = "Gear x top-10-species tonnage, by country", x = "Gear", y = NULL) +
    theme_westmed(base_size = 10) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
  ggsave(out_path("westmed_gear_species_heatmap.png"), p_heat, width = 11, height = 9, dpi = 110, bg = "white")
  
  p_bars <- ggplot(gsc$plot_df, aes(x = reorder(species_label, rank), y = tonnes, fill = gear_grp)) +
    geom_col(position = "fill") + coord_flip() + facet_wrap(~country) +
    scale_y_continuous(labels = scales::percent_format()) +
    scale_fill_manual(name = "Gear", values = westmed_colors(dplyr::n_distinct(gsc$plot_df$gear_grp))) +
    labs(title = "Gear composition of each top-10 species, by country", x = NULL, y = "Share of that species' catch") +
    theme_westmed(base_size = 10)
  ggsave(out_path("westmed_gear_species_bars.png"), p_bars, width = 11, height = 9, dpi = 110, bg = "white")
}

if (nrow(sector_ts) > 0) {
  plot_country_lines(sector_ts, "tonnes", "sector", out_path("westmed_sector_ts.png"),
                     "SAU catch by sector over time, by country")
}
plot_country_lines(gear_ts, "tonnes", "gear_grp", out_path("westmed_gear_ts.png"),
                   "SAU catch by gear over time, by country")

if (nrow(sector_summary_full) > 0 && have_ggplot2) {
  p_sector_share <- ggplot(sector_summary_full, aes(x = country, y = pct_of_country_catch, fill = sector)) +
    geom_col(position = "stack") +
    scale_fill_manual(name = "Sector", values = westmed_colors(dplyr::n_distinct(sector_summary_full$sector))) +
    labs(title = "Sector share of total catch, by country", x = NULL, y = "% of country's total catch") +
    theme_westmed(base_size = 10)
  ggsave(out_path("westmed_sector_share.png"), p_sector_share, width = 9, height = 5, dpi = 110, bg = "white")
}

cat("\n============================================================\n")
cat("PART B -- SAU vs FAO (best-effort)\n")
cat("============================================================\n")

fao_cy <- fao_country_year(TARGET_ISO3, YEAR_MIN, YEAR_MAX)
have_fao <- nrow(fao_cy) > 0

if (have_fao) {
  sau_total_cy <- sau_total_by_country_year(raw) %>% rename(sau_tonnes = tonnes)
  region_comparison <- fao_cy %>% rename(fao_tonnes = tonnes) %>% dplyr::select(-source) %>%
    full_join(sau_total_cy %>% dplyr::select(-source), by = c("country", "year")) %>%
    arrange(country, year) %>%
    group_by(country) %>%
    mutate(diff = sau_tonnes - fao_tonnes,
           pct_diff = ifelse(!is.na(fao_tonnes) & fao_tonnes != 0, 100 * diff / fao_tonnes, NA),
           first_common_year = suppressWarnings(min(year[!is.na(fao_tonnes) & !is.na(sau_tonnes)])),
           fao_base = fao_tonnes[year == first_common_year][1],
           sau_base = sau_tonnes[year == first_common_year][1],
           fao_index = 100 * fao_tonnes / fao_base,
           sau_index = 100 * sau_tonnes / sau_base) %>%
    ungroup()
  
  add_sheet("SAUvsFAO_ByCountry", region_comparison)
  
  if (have_ggplot2) {
    p_raw <- ggplot(region_comparison, aes(x = year)) +
      geom_line(aes(y = fao_tonnes, color = "FAO"), linewidth = 0.9) +
      geom_line(aes(y = sau_tonnes, color = "SAU"), linewidth = 0.9) +
      facet_wrap(~country, scales = "free_y") +
      scale_color_manual(name = NULL, values = c(FAO = WESTMED_PALETTE[1], SAU = WESTMED_PALETTE[2])) +
      labs(title = "SAU vs FAO catch by country", x = "Year", y = "Tonnes") + theme_westmed(base_size = 10)
    ggsave(out_path("westmed_sau_vs_fao_country.png"), p_raw, width = 10, height = 7, dpi = 110, bg = "white")
    
    p_idx <- ggplot(region_comparison, aes(x = year)) +
      geom_line(aes(y = fao_index, color = "FAO"), linewidth = 0.9) +
      geom_line(aes(y = sau_index, color = "SAU"), linewidth = 0.9) +
      geom_hline(yintercept = 100, color = "grey70", linetype = 3) +
      facet_wrap(~country) +
      scale_color_manual(name = NULL, values = c(FAO = WESTMED_PALETTE[1], SAU = WESTMED_PALETTE[2])) +
      labs(title = "SAU vs FAO, standardized (100 = first common year)", x = "Year", y = "Index") +
      theme_westmed(base_size = 10)
    ggsave(out_path("westmed_sau_vs_fao_country_index.png"), p_idx, width = 10, height = 7, dpi = 110, bg = "white")
  }
  
  # Species-level: match SAU's top-10 to FAO species codes, compare where matched.
  top10_matched <- fao_match_species(top10, "scientific_name")
  matched <- top10_matched %>% filter(!is.na(species))
  n_unmatched <- sum(is.na(top10_matched$species))
  if (n_unmatched > 0) message(sprintf("[FAO] %d of the top %d species couldn't be matched to an FAO scientific name.",
                                       n_unmatched, N_TOP_SPECIES))
  
  if (nrow(matched) > 0) {
    fao_species <- fao_species_country_year(matched, TARGET_ISO3, YEAR_MIN, YEAR_MAX)
    sau_species <- gsc$raw_top10 %>% group_by(country, scientific_name = .sci, year) %>%
      summarise(sau_tonnes = sum(.tonnes, na.rm = TRUE), .groups = "drop")
    
    species_comparison <- full_join(fao_species, sau_species, by = c("country", "scientific_name", "year")) %>%
      arrange(scientific_name, country, year) %>%
      mutate(diff = sau_tonnes - fao_tonnes,
             pct_diff = ifelse(!is.na(fao_tonnes) & fao_tonnes != 0, 100 * diff / fao_tonnes, NA))
    
    species_summary <- species_comparison %>%
      filter(!is.na(fao_tonnes), !is.na(sau_tonnes)) %>%
      group_by(scientific_name, common_name, country) %>%
      summarise(n_years = n(), mean_fao_tonnes = mean(fao_tonnes, na.rm = TRUE),
                mean_sau_tonnes = mean(sau_tonnes, na.rm = TRUE), mean_pct_diff = mean(pct_diff, na.rm = TRUE),
                .groups = "drop") %>%
      arrange(scientific_name, country)
    
    add_sheet("SAUvsFAO_BySpecies", species_summary)
    
    if (have_ggplot2) {
      p_species <- ggplot(species_comparison, aes(x = year)) +
        geom_line(aes(y = fao_tonnes, color = "FAO"), linewidth = 0.8) +
        geom_line(aes(y = sau_tonnes, color = "SAU"), linewidth = 0.8) +
        facet_wrap(~scientific_name, scales = "free_y") +
        scale_color_manual(name = NULL, values = c(FAO = WESTMED_PALETTE[1], SAU = WESTMED_PALETTE[2])) +
        labs(title = "SAU vs FAO by species (summed across countries)", x = "Year", y = "Tonnes") +
        theme_westmed(base_size = 9)
      ggsave(out_path("westmed_sau_vs_fao_species.png"), p_species, width = 11, height = 10, dpi = 110, bg = "white")
    }
  }
} else {
  region_comparison <- tibble()
}

cat("\n============================================================\n")
cat("PART C -- FishMIP: effort and catch\n")
cat("============================================================\n")

fishmip_ok <- fishmip_available() && file.exists(EFFORT_PARQUET) && file.exists(CATCH_PARQUET)
fishmip_effort_gear_cy <- tibble()
fishmip_reported_cy <- tibble()
fishmip_total_cy <- tibble()

if (!fishmip_ok) {
  if (!fishmip_available()) message("[FishMIP] Package 'arrow' isn't installed -- skipping Part C.")
  else message("[FishMIP] Parquet files not found in this folder -- skipping Part C (download them from the ",
               "FishMIP Input Explorer). Part D below will still run on SAU + FAO alone.")
} else {
  effort <- fishmip_load_effort(EFFORT_PARQUET, YEAR_MIN, YEAR_MAX)
  catch  <- fishmip_load_catch(CATCH_PARQUET, YEAR_MIN, YEAR_MAX)
  
  effort_full <- fishmip_effort_by_country_year_sector_gear(effort)
  effort_sector_summary <- fishmip_effort_sector_summary(effort)
  effort_gear_summary <- fishmip_effort_gear_summary(effort)
  fishmip_effort_gear_cy <- fishmip_effort_gear_by_country_year(effort)
  
  add_sheet("FishMIP_EffortFullDetail", effort_full)
  add_sheet("FishMIP_EffortSectorSummary", effort_sector_summary)
  add_sheet("FishMIP_EffortGearSummary", effort_gear_summary)
  
  plot_country_lines(fishmip_effort_sector_timeseries(effort), "nom_active", "sector",
                     out_path("westmed_fishmip_effort_by_sector_ts.png"),
                     "FishMIP nominal fishing effort by sector over time, by country",
                     subtitle = "Effort = kW x days at sea", ylab = "Nominal effort (kW-days)")
  plot_country_lines(fishmip_effort_gear_timeseries(effort, N_TOP_GEARS), "nom_active", "gear_grp",
                     out_path("westmed_fishmip_effort_by_gear_ts.png"),
                     "FishMIP nominal fishing effort by gear over time, by country", ylab = "Nominal effort (kW-days)")
  
  catch_full <- fishmip_catch_by_country_year_sector_fgroup(catch)
  catch_country_summary <- fishmip_catch_country_summary(catch_full)
  if (nrow(catch_full) > 0) {
    add_sheet("FishMIP_CatchFullDetail", catch_full)
    add_sheet("FishMIP_CatchCountrySummary", catch_country_summary)
    
    catch_ts <- fishmip_catch_timeseries(catch)
    countries_here <- sort(unique(catch_ts$country))
    ncol_grid <- ceiling(sqrt(length(countries_here))); nrow_grid <- ceiling(length(countries_here) / ncol_grid)
    westmed_png(out_path("westmed_fishmip_catch_composition_ts.png"), width = 350 * ncol_grid, height = 300 * nrow_grid, res = 100)
    par(mfrow = c(nrow_grid, ncol_grid), mar = c(4, 4, 3, 1))
    for (cty in countries_here) {
      d <- catch_ts %>% filter(country == cty) %>% arrange(year)
      yrange <- suppressWarnings(range(c(d$reported, d$iuu, d$discards), na.rm = TRUE))
      plot(d$year, d$reported, type = "l", col = WESTMED_PALETTE[1], lwd = 2, ylim = yrange, xlab = "Year", ylab = "Tonnes", main = cty)
      lines(d$year, d$iuu, col = WESTMED_PALETTE[8], lwd = 2, lty = 2)
      lines(d$year, d$discards, col = WESTMED_PALETTE[2], lwd = 2, lty = 3)
      legend("topleft", legend = c("Reported", "IUU", "Discards"), col = WESTMED_PALETTE[c(1, 8, 2)],
             lty = c(1, 2, 3), lwd = 2, bty = "n", cex = 0.7)
    }
    dev.off()
    
    fishmip_reported_cy <- fishmip_reported_by_country_year(catch_full)
    fishmip_total_cy <- fishmip_total_by_country_year(catch_full)
  }
}

cat("\n============================================================\n")
cat("PART D -- Master comparison: SAU total, SAU reported-only, FAO, FishMIP\n")
cat("============================================================\n")

sau_total_master <- sau_total_by_country_year(raw)
sau_reported_master <- sau_reported_by_country_year(raw)

master_catch_long <- bind_rows(sau_total_master, sau_reported_master, fao_cy,
                               fishmip_reported_cy, fishmip_total_cy) %>%
  arrange(country, source, year)

cat(sprintf("Combined master table: %d rows across %d source/approach combinations: %s\n",
            nrow(master_catch_long), n_distinct(master_catch_long$source),
            paste(unique(master_catch_long$source), collapse = "; ")))

catch_summary_by_country <- master_catch_long %>%
  group_by(country, source) %>% summarise(mean_tonnes = mean(tonnes, na.rm = TRUE), n_years = n(), .groups = "drop") %>%
  arrange(country, desc(mean_tonnes))
add_sheet("MasterComparison_ByCountryYear", master_catch_long)
add_sheet("MasterComparison_Summary", catch_summary_by_country)

plot_country_lines(master_catch_long, "tonnes", "source", out_path("westmed_master_comparison_raw.png"),
                   "Catch by country: SAU (total vs reported-only), FAO, FishMIP (reported vs total)", width = 12, height = 9)

master_catch_indexed <- master_catch_long %>%
  arrange(country, source, year) %>% group_by(country, source) %>%
  mutate(base_year = suppressWarnings(min(year[!is.na(tonnes) & tonnes != 0])),
         base_value = tonnes[year == base_year][1], index = 100 * tonnes / base_value) %>%
  ungroup()
plot_country_lines(master_catch_indexed, "index", "source", out_path("westmed_master_comparison_index.png"),
                   "Standardized catch trend by country (each series indexed to 100 at its own first year)",
                   ylab = "Index (100 = first available year)", width = 12, height = 9)

# ---- Gear: SAU catch-by-gear share vs FishMIP effort-by-gear share --------
# NOT merged into one chart -- SAU's gear breakdown is CATCH (tonnes),
# FishMIP's is EFFORT (kW-days), a different metric entirely, and FishMIP's
# catch file has no gear dimension at all. Two side-by-side % share charts,
# useful for comparing relative gear importance within each source's own
# terms -- never for reading one number against the other.
sau_gear_cy <- sau_gear_by_country_year(raw)
gear_share_sau <- sau_gear_share_by_country(sau_gear_cy, N_TOP_GEARS)
add_sheet("GearShare_SAU_Catch", gear_share_sau)

gear_share_fishmip <- tibble()
if (nrow(fishmip_effort_gear_cy) > 0) {
  gear_share_fishmip <- fishmip_effort_gear_share(fishmip_effort_gear_cy, N_TOP_GEARS)
  add_sheet("GearShare_FishMIP_Effort", gear_share_fishmip)
}

if (have_ggplot2) {
  p_sau_gear <- ggplot(gear_share_sau, aes(x = country, y = pct, fill = gear_grp)) +
    geom_col(position = "stack") +
    scale_fill_manual(name = "Gear", values = westmed_colors(dplyr::n_distinct(gear_share_sau$gear_grp))) +
    labs(title = "SAU: % of catch (tonnes) by gear", x = NULL, y = "% of country's catch") + theme_westmed(base_size = 10)
  if (nrow(gear_share_fishmip) > 0) {
    p_fm_gear <- ggplot(gear_share_fishmip, aes(x = country, y = pct, fill = gear_grp)) +
      geom_col(position = "stack") +
      scale_fill_manual(name = "Gear", values = westmed_colors(dplyr::n_distinct(gear_share_fishmip$gear_grp))) +
      labs(title = "FishMIP: % of effort (kW-days) by gear", x = NULL, y = "% of country's effort") + theme_westmed(base_size = 10)
    if (requireNamespace("patchwork", quietly = TRUE)) {
      suppressPackageStartupMessages(library(patchwork))
      ggsave(out_path("westmed_gear_share_sau_vs_fishmip.png"), p_sau_gear / p_fm_gear, width = 9, height = 10, dpi = 110, bg = "white")
    } else {
      ggsave(out_path("westmed_gear_share_sau.png"), p_sau_gear, width = 9, height = 5, dpi = 110, bg = "white")
      ggsave(out_path("westmed_gear_share_fishmip.png"), p_fm_gear, width = 9, height = 5, dpi = 110, bg = "white")
    }
  } else {
    ggsave(out_path("westmed_gear_share_sau.png"), p_sau_gear, width = 9, height = 5, dpi = 110, bg = "white")
  }
}

cat("\n============================================================\n")
cat("Export\n")
cat("============================================================\n")
xlsx_path <- out_path("westmed_fisheries_analysis.xlsx")
saveWorkbook(wb, xlsx_path, overwrite = TRUE)
cat(sprintf("Wrote %s -- one workbook, every dimension and every source together.\n", xlsx_path))

cat("\nAll done. Sources included: ", paste(unique(master_catch_long$source), collapse = "; "), "\n", sep = "")
if (!fishmip_ok) cat("(FishMIP not included this run -- see the Part C message above for what's missing.)\n")
if (!have_fao) cat("(FAO not included this run -- install.packages('fishstat') for the official-statistics comparison.)\n")
cat("\nStill not included: STECF Fisheries Dependent Information (landings+discards, GSA-level, EU countries),",
    " STECF Fishing Effort (officially reported days-at-sea, EU countries), and GFCM DCRF",
    " (GSA-level, all 6 countries, access currently restricted). These need real sample files/access to",
    " build against, the same way the FishMIP parquet structure had to be confirmed directly rather than guessed.\n")
cat("\nCitations: Sea Around Us (seaaroundus.org/citation-policy), FAO FishStat (github.com/sofia-taf/fishstat),",
    " FishMIP/ISIMIP3a (Novaglio et al. 2024, doi.org/10.48364/ISIMIP.240282).\n")