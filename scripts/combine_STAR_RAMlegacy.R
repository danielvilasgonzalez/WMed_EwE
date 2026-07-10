## =================================================================
## Combine STAR (2019-2025) and RAM Legacy (mostly 1945-2015)
## into one continuous Western Mediterranean time series
## =================================================================

pkgs <- c("readr", "dplyr", "ggplot2", "stringr")
new_pkgs <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new_pkgs) > 0) install.packages(new_pkgs)
invisible(lapply(pkgs, library, character.only = TRUE))

if (!dir.exists("plots")) dir.create("plots")

## Explicit column types - readr's type-guessing samples only the first
## ~1000 rows, and RAM Legacy's 'gsa' column is mostly single numbers
## ("6", "9") with fewer compound ones ("1,5,6,7"), so it can get silently
## guessed as double and then fail/NA on the first compound value it hits.
tidy_col_types <- cols(
  source = col_character(),
  stock_key = col_character(),
  species = col_character(),
  common_name = col_character(),
  gsa = col_character(),
  subregion = col_character(),
  year = col_integer(),
  biomass = col_double(),
  biomass_unit = col_character(),
  b_ratio = col_double(),
  landings = col_double(),
  landings_unit = col_character(),
  catches = col_double(),
  catches_unit = col_character()
)

star_tidy <- read_csv("star_data_tidy.csv", col_types = tidy_col_types)
ram_tidy  <- read_csv("ramlegacy_data_tidy.csv", col_types = tidy_col_types)

## --- 1. Combine ------------------------------------------------------------

combined <- bind_rows(star_tidy, ram_tidy)

message("Combined rows: ", nrow(combined), " (STAR: ", nrow(star_tidy), ", RAM Legacy: ", nrow(ram_tidy), ")")
message("Year range: ", paste(range(combined$year, na.rm = TRUE), collapse = " - "))

## --- 2. Check for genuine overlap between the two sources, PER METRIC ------
## (a species/gsa/year combo might have biomass in one source but only
## landings in the other for that same year - that's not really an
## "overlap" for biomass specifically, so check each metric separately)

check_overlap <- function(data, metric_col) {
  data %>%
    filter(!is.na(species), !is.na(gsa), !is.na(year), !is.na(.data[[metric_col]])) %>%
    distinct(species, gsa, year, source) %>%
    count(species, gsa, year) %>%
    filter(n > 1)
}

for (metric in c("biomass", "b_ratio", "landings", "catches")) {
  ov <- check_overlap(combined, metric)
  message("\n[", metric, "] species/gsa/year combos present in BOTH sources: ", nrow(ov))
  if (nrow(ov) > 0) print(ov)
}

message("\nThese are kept as separate rows (see 'source' column) rather than merged -",
        " compare them directly if you want to check agreement between methods.")

## --- 3. Gap check: is there a real gap between RAM Legacy ending and ------
## STAR beginning, per stock?

gap_check <- combined %>%
  filter(!is.na(year)) %>%
  group_by(species, gsa, source) %>%
  summarise(first_year = min(year), last_year = max(year), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = source, values_from = c(first_year, last_year))

message("\nPer-stock coverage from each source (NA = that source has no data for this stock):")
print(gap_check, n = 30)

write_csv(combined, "combined_medbs_star_ramlegacy.csv")
message("\nSaved combined dataset: combined_medbs_star_ramlegacy.csv")

## --- Helper: single unit string for the y-axis title -----------------------

get_unit_label <- function(data, unit_col) {
  units <- unique(na.omit(data[[unit_col]]))
  if (length(units) == 0) return("unit unknown")
  if (length(units) == 1) return(units)
  "mixed units"
}

## --- 4. Plot: full combined history, Western Med, faceted by species -----
## (facet by species so each panel gets its own sensible axis scale via
## free - color by GSA within each species panel, shape distinguishes source)

TOP_N_SPECIES <- 8
TOP_N_GSA <- 8
MIN_OBS <- 4

westmed_combined <- combined %>%
  filter(str_detect(subregion, "Western Mediterranean"), !is.na(biomass))

top_species <- westmed_combined %>%
  count(common_name, sort = TRUE) %>%
  slice_head(n = TOP_N_SPECIES) %>%
  pull(common_name)

top_gsa <- westmed_combined %>%
  count(gsa, sort = TRUE) %>%
  slice_head(n = TOP_N_GSA) %>%
  pull(gsa)

plot_data <- westmed_combined %>%
  filter(common_name %in% top_species, gsa %in% top_gsa) %>%
  group_by(common_name) %>%
  filter(n() >= MIN_OBS) %>%
  ungroup() %>%
  mutate(species_label = paste0(common_name, "\n(", species, ")"))

p_combined <- ggplot(plot_data, aes(x = year, y = biomass, color = gsa, shape = source)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_line(aes(group = interaction(gsa, source)), alpha = 0.4, linewidth = 0.4) +
  facet_wrap(~ species_label, scales = "free") +
  labs(title = "Western Mediterranean biomass - combined STAR + RAM Legacy",
       x = "Year", y = paste0("Biomass (", get_unit_label(plot_data, "biomass_unit"), ")"),
       color = "GSA", shape = "Source") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", legend.text = element_text(size = 8))

ggsave("plots/combined_westmed_biomass.png", p_combined, width = 15, height = 10, dpi = 150)

message("\nSaved plot: plots/combined_westmed_biomass.png")

## --- 5. Same treatment for landings ------------------------------------------

westmed_landings <- combined %>%
  filter(str_detect(subregion, "Western Mediterranean"), !is.na(landings))

top_species_l <- westmed_landings %>%
  count(common_name, sort = TRUE) %>%
  slice_head(n = TOP_N_SPECIES) %>%
  pull(common_name)

top_gsa_l <- westmed_landings %>%
  count(gsa, sort = TRUE) %>%
  slice_head(n = TOP_N_GSA) %>%
  pull(gsa)

plot_data_l <- westmed_landings %>%
  filter(common_name %in% top_species_l, gsa %in% top_gsa_l) %>%
  group_by(common_name) %>%
  filter(n() >= MIN_OBS) %>%
  ungroup() %>%
  mutate(species_label = paste0(common_name, "\n(", species, ")"))

p_combined_landings <- ggplot(plot_data_l, aes(x = year, y = landings, color = gsa, shape = source)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_line(aes(group = interaction(gsa, source)), alpha = 0.4, linewidth = 0.4) +
  facet_wrap(~ species_label, scales = "free") +
  labs(title = "Western Mediterranean total landings - combined STAR + RAM Legacy",
       x = "Year", y = paste0("Total landings (", get_unit_label(plot_data_l, "landings_unit"), ")"),
       color = "GSA", shape = "Source") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", legend.text = element_text(size = 8))

ggsave("plots/combined_westmed_landings.png", p_combined_landings, width = 15, height = 10, dpi = 150)

message("Saved plot: plots/combined_westmed_landings.png")

## --- 6. Same treatment for total catch (kept separate from landings) -------

westmed_catches <- combined %>%
  filter(str_detect(subregion, "Western Mediterranean"), !is.na(catches))

top_species_c <- westmed_catches %>%
  count(common_name, sort = TRUE) %>%
  slice_head(n = TOP_N_SPECIES) %>%
  pull(common_name)

top_gsa_c <- westmed_catches %>%
  count(gsa, sort = TRUE) %>%
  slice_head(n = TOP_N_GSA) %>%
  pull(gsa)

plot_data_c <- westmed_catches %>%
  filter(common_name %in% top_species_c, gsa %in% top_gsa_c) %>%
  group_by(common_name) %>%
  filter(n() >= MIN_OBS) %>%
  ungroup() %>%
  mutate(species_label = paste0(common_name, "\n(", species, ")"))

p_combined_catches <- ggplot(plot_data_c, aes(x = year, y = catches, color = gsa, shape = source)) +
  geom_point(size = 2, alpha = 0.8) +
  geom_line(aes(group = interaction(gsa, source)), alpha = 0.4, linewidth = 0.4) +
  facet_wrap(~ species_label, scales = "free") +
  labs(title = "Western Mediterranean total catch - combined STAR + RAM Legacy",
       x = "Year", y = paste0("Total catch (", get_unit_label(plot_data_c, "catches_unit"), ")"),
       color = "GSA", shape = "Source") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", legend.text = element_text(size = 8))

ggsave("plots/combined_westmed_catches.png", p_combined_catches, width = 15, height = 10, dpi = 150)

message("Saved plot: plots/combined_westmed_catches.png")