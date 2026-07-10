## ---------------------------------------------------------------
## MEDBS Survey Analysis: Demersal (trawl) & Acoustic time series
## Faceted by GSA, coloured by country, per species
## ---------------------------------------------------------------

## --- 0. Packages -----------------------------------------------------------

pkgs <- c("readr", "dplyr", "tidyr", "ggplot2", "stringr", "forcats")
new_pkgs <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new_pkgs) > 0) install.packages(new_pkgs)
invisible(lapply(pkgs, library, character.only = TRUE))

## --- 1. Config ---------------------------------------------------------

base_dir <- "MEDBSsurvey_2024"
out_dir  <- "plots"
if (!dir.exists(out_dir)) dir.create(out_dir)

# Keep facets readable: NULL = use everything, or set e.g. c("ITA","ESP","FRA")
FILTER_COUNTRIES <- NULL
# Western Mediterranean (broader definition, matching the map): GSA 1-12
# (Alboran Sea, Balearic Islands, N. Spain, Gulf of Lion, Corsica, Ligurian/
# Tyrrhenian, Sardinia, plus GSA 12 Northern Tunisia)
FILTER_AREAS     <- 1:12
TOP_N_AREAS      <- 6             # used only if FILTER_AREAS is NULL
TOP_N_SPECIES    <- 6             # per dataset, ranked by total volume

## --- 2. Load & clean DEMERSAL catch (TB.csv) -------------------------------

tb <- read_csv(file.path(base_dir, "Demersal", "TB.csv"), show_col_types = FALSE)

demersal <- tb %>%
  mutate(
    species_code = paste0(genus, species),
    ptot  = ifelse(ptot  < 0, NA, ptot),   # -1 = not sampled -> NA
    nbtot = ifelse(nbtot < 0, NA, nbtot),
    gsa   = as.numeric(area)
  ) %>%
  group_by(country, gsa, year, species_code) %>%
  summarise(
    total_weight_kg = sum(ptot, na.rm = TRUE) / 1000,
    total_n         = sum(nbtot, na.rm = TRUE),
    n_hauls         = n_distinct(haul_number),
    .groups = "drop"
  )

## --- 3. Load & clean ACOUSTIC (abundance.csv, biomass.csv) -----------------

# sums the lengthclass0 ... lengthclass100_plus columns for one row,
# treating -1 as NA; returns NA (not 0) if the whole row is unsampled
sum_lengthclasses <- function(df) {
  lc_cols <- grep("^lengthclass", names(df), value = TRUE)
  mat <- as.matrix(df[lc_cols])
  mat[mat < 0] <- NA
  totals <- rowSums(mat, na.rm = TRUE)
  totals[rowSums(!is.na(mat)) == 0] <- NA
  totals
}

load_acoustic <- function(file, value_name) {
  df <- read_csv(file.path(base_dir, "Acoustic", file), show_col_types = FALSE)
  df$total_value <- sum_lengthclasses(df)
  df %>%
    mutate(gsa = as.numeric(str_extract(area, "\\d+"))) %>%
    # prefer combined sex ("C") to avoid double-counting with M/F rows
    { if (any(.$sex == "C")) filter(., sex == "C") else . } %>%
    group_by(country, gsa, year, species) %>%
    summarise(!!value_name := sum(total_value, na.rm = TRUE), .groups = "drop")
}

acoustic_abund <- load_acoustic("abundance.csv", "total_abundance")
acoustic_biom  <- load_acoustic("biomass.csv",   "total_biomass")

acoustic <- full_join(acoustic_abund, acoustic_biom,
                      by = c("country", "gsa", "year", "species"))

## --- 4. Optional filters ----------------------------------------------------

apply_filters <- function(df, area_col = "gsa", species_col, value_col) {
  if (!is.null(FILTER_COUNTRIES)) df <- filter(df, country %in% FILTER_COUNTRIES)
  
  if (!is.null(FILTER_AREAS)) {
    df <- filter(df, .data[[area_col]] %in% FILTER_AREAS)
  } else {
    top_areas <- df %>%
      group_by(.data[[area_col]]) %>%
      summarise(tot = sum(.data[[value_col]], na.rm = TRUE), .groups = "drop") %>%
      slice_max(tot, n = TOP_N_AREAS) %>%
      pull(.data[[area_col]])
    df <- filter(df, .data[[area_col]] %in% top_areas)
  }
  
  top_species <- df %>%
    group_by(.data[[species_col]]) %>%
    summarise(tot = sum(.data[[value_col]], na.rm = TRUE), .groups = "drop") %>%
    slice_max(tot, n = TOP_N_SPECIES) %>%
    pull(.data[[species_col]])
  filter(df, .data[[species_col]] %in% top_species)
}

## --- 5. Reusable faceted time-series plot -----------------------------------

plot_timeseries <- function(df, y, species_col, title, y_lab) {
  ggplot(df, aes(x = year, y = .data[[y]], color = country)) +
    geom_line(linewidth = 0.6) +
    geom_point(size = 1) +
    facet_grid(rows = vars(.data[[species_col]]),
               cols = vars(gsa),
               scales = "free_y") +
    labs(title = title, x = "Year", y = y_lab, color = "Country") +
    theme_minimal(base_size = 11) +
    theme(strip.text.y = element_text(angle = 0),
          legend.position = "bottom")
}

## --- 6. Build & save plots ---------------------------------------------------

# Demersal - biomass
d_biom <- apply_filters(demersal, species_col = "species_code", value_col = "total_weight_kg")
p1 <- plot_timeseries(d_biom, "total_weight_kg", "species_code",
                      "Demersal trawl survey - Biomass by GSA & country",
                      "Total catch weight (kg)")
ggsave(file.path(out_dir, "demersal_biomass_timeseries.png"), p1, width = 14, height = 8, dpi = 150)

# Demersal - abundance
d_abund <- apply_filters(demersal, species_col = "species_code", value_col = "total_n")
p2 <- plot_timeseries(d_abund, "total_n", "species_code",
                      "Demersal trawl survey - Abundance by GSA & country",
                      "Total individuals caught")
ggsave(file.path(out_dir, "demersal_abundance_timeseries.png"), p2, width = 14, height = 8, dpi = 150)

# Acoustic - biomass
a_biom <- apply_filters(acoustic %>% filter(!is.na(total_biomass)),
                        species_col = "species", value_col = "total_biomass")
p3 <- plot_timeseries(a_biom, "total_biomass", "species",
                      "Acoustic survey - Biomass by GSA & country",
                      "Total biomass")
ggsave(file.path(out_dir, "acoustic_biomass_timeseries.png"), p3, width = 14, height = 8, dpi = 150)

# Acoustic - abundance
a_abund <- apply_filters(acoustic %>% filter(!is.na(total_abundance)),
                         species_col = "species", value_col = "total_abundance")
p4 <- plot_timeseries(a_abund, "total_abundance", "species",
                      "Acoustic survey - Abundance by GSA & country",
                      "Total abundance")
ggsave(file.path(out_dir, "acoustic_abundance_timeseries.png"), p4, width = 14, height = 8, dpi = 150)

## --- 7. Export summary tables -------------------------------------------------

write_csv(demersal, file.path(out_dir, "demersal_summary.csv"))
write_csv(acoustic,  file.path(out_dir, "acoustic_summary.csv"))

message("Done. Plots and summary CSVs written to '", out_dir, "/'")
print(list.files(out_dir))