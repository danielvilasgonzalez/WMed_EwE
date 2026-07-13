## ---------------------------------------------------------------
## Configuration
## ---------------------------------------------------------------

setwd("/Users/daniel/Work/iMARES/")

BASE_DIR <- "./WMed EwE Model"
START_YEAR <- 1995

## Available datasets:
##   "FAO_2020"     = original Excel dataset
##   "GFCM_2025"    = latest GFCM regional database
##   "FAO_2026"     = global FAO capture database

DATASET_VERSION <- "GFCM_2025"

WESTMED_DIVISIONS <- c(
  "37.1.1",
  "37.1.2",
  "37.1.3"
)

TOP_N_SPECIES <- 20

OUT_DIR <- file.path(BASE_DIR, "data", "processed")
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

## ---------------------------------------------------------------
## Dataset-specific paths
## ---------------------------------------------------------------

if (DATASET_VERSION == "FAO_2020") {
  
  catch_file <- file.path(
    BASE_DIR,
    "data/raw/FAO-GFCM-CapturepProduction-1970_2020.xlsx"
  )
  
  fg_file <- file.path(
    BASE_DIR,
    "data/FG_WMed.xlsx"
  )
  
}

if (DATASET_VERSION == "GFCM_2025") {
  
  data_dir <- file.path(
    BASE_DIR,
    "data/FI_Regional_2025.1.0"
  )
  
  capture_file <- file.path(
    data_dir,
    "GFCM_Capture_Quantity.csv"
  )
  
  species_file <- file.path(
    data_dir,
    "CL_FI_SPECIES_GROUPS.csv"
  )
  
  countries_file <- file.path(
    data_dir,
    "CL_FI_COUNTRY_GROUPS.csv"
  )
  
  divisions_file <- file.path(
    data_dir,
    "CL_FI_WATERAREA_DIVISION.csv"
  )
  
  fg_file <- file.path(
    BASE_DIR,
    "data/FG_WMed.xlsx"
  )
  
}

if (DATASET_VERSION == "FAO_2026") {
  
  data_dir <- file.path(
    BASE_DIR,
    "data/Capture_2026.1.0"
  )
  
  capture_file <- file.path(
    data_dir,
    "Capture_Quantity.csv"
  )
  
  species_file <- file.path(
    data_dir,
    "CL_FI_SPECIES_GROUPS.csv"
  )
  
  countries_file <- file.path(
    data_dir,
    "CL_FI_COUNTRY_GROUPS.csv"
  )
  
  areas_file <- file.path(
    data_dir,
    "CL_FI_WATERAREA_GROUPS.csv"
  )
  
  fg_file <- file.path(
    BASE_DIR,
    "data/raw/FG_WMed.xlsx"
  )
  
}

## ---------------------------------------------------------------
## Load selected dataset
## ---------------------------------------------------------------

if (DATASET_VERSION == "FAO_2020") {
  
  catch_data <- readxl::read_excel(catch_file)
  
}

if (DATASET_VERSION %in% c("GFCM_2025", "FAO_2026")) {
  
  capture   <- data.table::fread(capture_file)
  species   <- data.table::fread(species_file)
  countries <- data.table::fread(countries_file)
  
}

if (DATASET_VERSION == "GFCM_2025") {
  divisions <- data.table::fread(divisions_file)
}

if (DATASET_VERSION == "FAO_2026") {
  areas <- data.table::fread(areas_file)
}

fg <- readxl::read_excel(
  fg_file,
  sheet = 4
)

message("Dataset loaded: ", DATASET_VERSION)

## ---------------------------------------------------------------
## Process Data
## ---------------------------------------------------------------

if (DATASET_VERSION == "FAO_2020") {
  
  westmed <- catch_data %>%
    filter(
      `Area (FAO subarea)` == "Western Med (37.1)",
      Year >= START_YEAR
    )
  
  country_ts <- westmed %>%
    group_by(
      Year,
      Country,
      `Area (FAO division)`
    ) %>%
    summarise(
      Catch = sum(Quantity, na.rm = TRUE),
      .groups = "drop"
    )
  
  species_ts <- westmed %>%
    group_by(
      Year,
      `Species (scientific name)`,
      `Area (FAO division)`
    ) %>%
    summarise(
      Catch = sum(Quantity, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(
      Species = `Species (scientific name)`,
      Division = `Area (FAO division)`
    )
  
}


if (DATASET_VERSION %in% c("GFCM_2025", "FAO_2026")) {
  
  species <- species[
    ,
    .(
      SPECIES.ALPHA_3_CODE = `3A_Code`,
      Species = Name_En
    )
  ]
  
  countries <- countries[
    ,
    .(
      COUNTRY.UN_CODE = UN_Code,
      Country = Name_En
    )
  ]
  
  capture <- merge(
    capture,
    species,
    by = "SPECIES.ALPHA_3_CODE",
    all.x = TRUE
  )
  
  capture <- merge(
    capture,
    countries,
    by = "COUNTRY.UN_CODE",
    all.x = TRUE
  )
  
}


if (DATASET_VERSION == "GFCM_2025") {
  
  divisions <- divisions[
    ,
    .(
      DIVISION.CODE = Code,
      Division = Name_En
    )
  ]
  
  capture <- merge(
    capture,
    divisions,
    by = "DIVISION.CODE",
    all.x = TRUE
  )
  capture$PERIOD<-as.numeric(capture$PERIOD)
  
  westmed <- capture[
    DIVISION.CODE %in% WESTMED_DIVISIONS &
      MEASURE == "Q_tlw" &
      PERIOD >= START_YEAR
  ]
  
  country_ts <- westmed[
    ,
    .(
      Catch = sum(VALUE, na.rm = TRUE)
    ),
    by = .(
      PERIOD,
      Country,
      DIVISION.CODE
    )
  ]
  
  species_ts <- westmed[
    ,
    .(
      Catch = sum(VALUE, na.rm = TRUE)
    ),
    by = .(
      PERIOD,
      Species,
      DIVISION.CODE
    )
  ]
  
  setnames(
    species_ts,
    "DIVISION.CODE",
    "Division"
  )
  
}


## ---------------------------------------------------------------
## Functional Group Matching
## ---------------------------------------------------------------

if (DATASET_VERSION == "FAO_2020") {
  
  unmatched_species <- catch_data %>%
    anti_join(
      fg,
      by = c(
        "Species (scientific name)" = "ESPECIE"
      )
    ) %>%
    group_by(`Species (scientific name)`) %>%
    summarise(
      Catch = sum(Quantity, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    rename(
      Species = `Species (scientific name)`
    ) %>%
    arrange(desc(Catch)) %>%
    mutate(
      FG_num = NA,
      FG_name = NA
    )
  
}


if (DATASET_VERSION == "GFCM_2025") {
  
  unmatched_species <- westmed[
    !Species %in% fg$ESPECIE,
    .(
      Catch = sum(VALUE, na.rm = TRUE)
    ),
    by = Species
  ][order(-Catch)]
  
  unmatched_species[
    ,
    `:=`(
      FG_num = NA,
      FG_name = NA
    )
  ]
  
}


## ---------------------------------------------------------------
## Summary Statistics
## ---------------------------------------------------------------

if (DATASET_VERSION == "FAO_2020") {
  
  top_species <- species_ts %>%
    group_by(Species) %>%
    summarise(
      TotalCatch = sum(Catch),
      .groups = "drop"
    ) %>%
    arrange(desc(TotalCatch)) %>%
    slice_head(n = TOP_N_SPECIES) %>%
    pull(Species)
  
  species_plot <- species_ts %>%
    filter(Species %in% top_species)
  
}


if (DATASET_VERSION == "GFCM_2025") {
  
  top_species <- westmed[
    ,
    .(
      TotalCatch = sum(VALUE, na.rm = TRUE)
    ),
    by = Species
  ][order(-TotalCatch)][
    1:TOP_N_SPECIES,
    Species
  ]
  
  species_plot <- species_ts[
    Species %in% top_species
  ]
  
}


## ---------------------------------------------------------------
## Plots
## ---------------------------------------------------------------

if (DATASET_VERSION == "FAO_2020") {
  
  p_country <- ggplot(
    country_ts,
    aes(
      x = Year,
      y = Catch,
      colour = Country,
      group = Country
    )
  ) +
    geom_line() +
    facet_wrap(
      ~ `Area (FAO division)`,
      scales = "free_y",
      ncol = 1
    ) +
    theme_bw() +
    labs(
      x = "Year",
      y = "Catch (t)"
    )
  
}


if (DATASET_VERSION == "GFCM_2025") {
  
  p_country <- ggplot(
    country_ts,
    aes(
      x = PERIOD,
      y = Catch,
      colour = Country
    )
  ) +
    geom_line() +
    facet_wrap(
      ~ DIVISION.CODE,
      scales = "free_y"
    ) +
    theme_bw() +
    labs(
      x = "Year",
      y = "Catch (t)"
    )
  
}


species_plot <- species_plot %>%
  mutate(
    Year = if ("PERIOD" %in% names(.)) PERIOD else Year
  )

p_species <- ggplot(
  species_plot,
  aes(
    x = Year,
    y = Catch,
    colour = Division,
    group = Division
  )
) +
  geom_line(linewidth = 0.8) +
  facet_wrap(
    ~ Species,
    scales = "free_y",
    ncol = 4
  ) +
  theme_bw() +
  labs(
    x = "Year",
    y = "Catch (t)",
    colour = NULL
  ) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 8)
  )


print(p_country)
print(p_species)

## ---------------------------------------------------------------
## Export Results
## ---------------------------------------------------------------

write.csv(
  unmatched_species,
  file.path(
    OUT_DIR,
    "unmatched_species_FG_assignment.csv"
  ),
  row.names = FALSE
)

if (DATASET_VERSION == "FAO_2020") {
  
  write.csv(
    species_ts,
    file.path(
      OUT_DIR,
      "westmed_species_timeseries.csv"
    ),
    row.names = FALSE
  )
  
}

if (DATASET_VERSION == "GFCM_2025") {
  
  fwrite(
    species_ts,
    file.path(
      OUT_DIR,
      "westmed_species_timeseries.csv"
    )
  )
  
}

ggsave(
  file.path(
    OUT_DIR,
    "country_catch_timeseries.png"
  ),
  p_country,
  width = 12,
  height = 8,
  dpi = 300
)

ggsave(
  file.path(
    OUT_DIR,
    "species_catch_timeseries.png"
  ),
  p_species,
  width = 14,
  height = 10,
  dpi = 300
)

message("Analysis completed.")

