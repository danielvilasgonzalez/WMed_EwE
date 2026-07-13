# Load package
library(readxl)
library(dplyr)
library(ggplot2)

# catch data from FAO from previous model #####

#wd
if (Sys.info()[['machine']]=='arm64') {
  setwd('/Users/daniel/Work/iMARES/')
  file <- "./MPA4FISH/data/raw/FAO-GFCM-CapturepProduction-1970_2020.xlsx"
} else {
  setwd('C:/Documents and Settings/danie/Desktop/iMARES/')
  file <- "./WMed EwE Model/FutureMares/New fitting/gfcm catch data/FAO-GFCM-CapturepProduction-1970_2020/FAO-GFCM-CapturepProduction-1970_2020.xlsx"
}

#catch File
excel_sheets(file)
df1 <- read_excel(file, sheet = 1)
head(df1)
colnames(df1)
#unique(df$`Area (FAO subarea)`)

#west med
west_med <- df1 %>%
  filter(`Area (FAO subarea)` == "Western Med (37.1)" & Year >= 1995)
unique(west_med$Country)

#by year
west_med_year <- west_med %>%
  group_by(Year, Country,`Area (FAO division)`) %>%
  summarise(Catch = sum(Quantity, na.rm = TRUE),
            .groups = "drop")

#plot
ggplot(west_med_year,
       aes(x = Year,
           y = Catch,
           color = Country,
           group = Country)) +
  geom_line() +
  facet_wrap(~ `Area (FAO division)`,
             ncol = 1,
             scales = "free_y") +
  theme_minimal() +
  labs(y = "Catch (t)")

#fg file code
file <- "./WMed EwE Model/FutureMares/New fitting/FG_WMed.xlsx"
df2 <- read_excel(file, sheet = 4)
head(df2)


#check unmatching specise
unmatched_species <- df1 %>%
  anti_join(
    df2,
    by = c("Species (scientific name)" = "ESPECIE")
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
  ) %>%
  select(Species, Catch, FG_num, FG_name)

#save file
write.csv(
  unmatched_species,
  paste0(gitdir,"data/processed/unmatched_species_FG_assignment.csv"),
  row.names = FALSE
)

# catch data from GFCM-FAO last update #####

## ---------------------------------------------------------------
## Read GFCM Capture Production dataset
## Download manually from:
## https://www.fao.org/fishery/en/collection/gfcm_capture
## Save the downloaded ZIP as:
##   data/raw/GFCM_Capture.zip
## ---------------------------------------------------------------
## ---------------------------------------------------------------
## Download & inspect GFCM Capture Production dataset
## ---------------------------------------------------------------

url      <- "https://www.fao.org/fishery/collection/gfcm_capture/en#:~:text=Regional%20capture%20fisheries%20(CSV%20raw%20data)"
zip_file <- "./WMed EwE Model/data/FI_Regional_2025.1.0.zip"
out_dir  <- "./WMed EwE Model/data/FI_Regional_2025.1.0"

## --- 1. Download (skip if already present) ----------------------

if (!file.exists(zip_file)) {
  message("Downloading GFCM Capture Production dataset...")
  download.file(
    url,
    destfile = zip_file,
    mode = "wb",
    method = "libcurl"
  )
} else {
  message("Zip already exists locally, skipping download.")
}

## --- 2. Unzip ---------------------------------------------------

if (!dir.exists(out_dir)) dir.create(out_dir)

unzip(zip_file, exdir = out_dir)

files <- list.files(
  out_dir,
  recursive = TRUE,
  full.names = TRUE
)

message("Extracted ", length(files), " file(s):")
print(files)

## --- Preview CSVs -----------------------------------------------

csv_files <- files[grepl("\\.csv$", files, ignore.case = TRUE)]

for (f in csv_files) {
  
  cat("\n============================\n")
  cat(basename(f), "\n")
  cat("============================\n")
  
  first_line <- readLines(f, n = 1, warn = FALSE)
  sep <- if (grepl(";", first_line)) ";" else ","
  
  df <- tryCatch(
    read.csv(
      f,
      sep = sep,
      nrows = 5,
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    error = function(e) NULL
  )
  
  if (!is.null(df)) {
    cat("Columns:\n")
    print(names(df))
    cat("\nFirst rows:\n")
    print(df)
  }
}


westmed_divisions <- c(
  "37.1.1", # Western Mediterranean
  "37.1.2", # Gulf of Lions
  "37.1.3"  # Sardinia / Tyrrhenian
)

library(data.table)
library(ggplot2)

## ---------------------------------------------------------------
## Read GFCM capture data and lookup tables
## ---------------------------------------------------------------

capture <- fread(
  file.path(out_dir, "GFCM_Capture_Quantity.csv")
)

species <- fread(
  file.path(out_dir, "CL_FI_SPECIES_GROUPS.csv")
)

countries <- fread(
  file.path(out_dir, "CL_FI_COUNTRY_GROUPS.csv")
)

divisions <- fread(
  file.path(out_dir, "CL_FI_WATERAREA_DIVISION.csv")
)

## ---------------------------------------------------------------
## Keep only columns needed for joins
## ---------------------------------------------------------------

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

divisions <- divisions[
  ,
  .(
    DIVISION.CODE = Code,
    Division = Name_En
  )
]

## ---------------------------------------------------------------
## Join lookup tables
## ---------------------------------------------------------------

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

capture <- merge(
  capture,
  divisions,
  by = "DIVISION.CODE",
  all.x = TRUE
)

## ---------------------------------------------------------------
## Filter Western Mediterranean divisions
## ---------------------------------------------------------------

westmed_divisions <- c(
  "37.1.1",  # Western Mediterranean
  "37.1.2",  # Gulf of Lions
  "37.1.3"   # Sardinia / Tyrrhenian
)

westmed <- capture[
  DIVISION.CODE %in% westmed_divisions &
    MEASURE == "Q_tlw" &
    PERIOD >= 1995
]

## ---------------------------------------------------------------
## Select top species by total catch
## ---------------------------------------------------------------

top_species <- x[
  ,
  .(TotalCatch = sum(VALUE, na.rm = TRUE)),
  by = Species
][order(-TotalCatch)][1:20, Species]

## ---------------------------------------------------------------
## Aggregate catches by year, species and division
## ---------------------------------------------------------------

species_ts <- westmed[
  Species %in% top_species,
  .(
    Catch = sum(VALUE, na.rm = TRUE)
  ),
  by = .(
    PERIOD,
    Species,
    DIVISION.CODE
  )
]

## ---------------------------------------------------------------
## Pretty division labels
## ---------------------------------------------------------------

species_ts[
  ,
  Division := factor(
    DIVISION.CODE,
    levels = c(
      "37.1.1",
      "37.1.2",
      "37.1.3"
    ),
    labels = c(
      "Western Med",
      "Gulf of Lions",
      "Sardinia-Tyrrhenian"
    )
  )
]

## ---------------------------------------------------------------
## Plot
## ---------------------------------------------------------------

ggplot(
  species_ts,
  aes(
    x = PERIOD,
    y = Catch,
    colour = Division
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
    strip.text = element_text(size = 8),
    legend.position = "bottom"
  )
