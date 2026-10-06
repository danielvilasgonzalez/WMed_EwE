# ============================================================
# ORMEF species records in Western Mediterranean GFCM GSAs
# Western Mediterranean = GSA 1-11
# GSA 12 and GSA 16 are excluded
# ============================================================

library(sf)
library(dplyr)
library(stringr)
library(tibble)

# ------------------------------------------------------------
# Species to check
# ------------------------------------------------------------

target_species <- c(
  "Fistularia commersonii",
  "Pterois miles",
  "Lagocephalus lagocephalus lagocephalus",
  "Lagocephalus sceleratus",
  "Siganus luridus",
  "Siganus rivulatus",
  "Sparisoma cretense"
)

# ------------------------------------------------------------
# Extract observation year from ORMEF eventDate
# ------------------------------------------------------------

occ <- occ %>%
  mutate(
    observation_year = suppressWarnings(
      as.integer(str_extract(eventDate, "^\\d{4}"))
    )
  )

# ------------------------------------------------------------
# Read GFCM GSA shapefile
# ------------------------------------------------------------

gsa <- st_read(
  "/Users/andreaobradors/Documents/daniel/WMed_EwE/GFCM_GSA_shp/GFCM_GSA/GFCM_GSA.shp",
  quiet = TRUE
)

# Inspect the GSA dataset
names(gsa)
print(gsa)

# ------------------------------------------------------------
# Create spatial ORMEF records
# ------------------------------------------------------------

occ_sf <- occ %>%
  filter(
    scientificName %in% target_species,
    !is.na(observation_year),
    observation_year >= 1994,
    !is.na(decimalLongitude),
    !is.na(decimalLatitude)
  ) %>%
  st_as_sf(
    coords = c("decimalLongitude", "decimalLatitude"),
    crs = 4326,
    remove = FALSE
  )

# Match coordinate reference systems
gsa <- st_transform(
  gsa,
  st_crs(occ_sf)
)

# ------------------------------------------------------------
# IMPORTANT:
# Run the two lines below first to see the GSA field.
# ------------------------------------------------------------

names(gsa)

# If the field is called "GSA", inspect its values with:
unique(gsa$GSA)

# ------------------------------------------------------------
# After confirming the field is GSA, keep GSA 1-11
# ------------------------------------------------------------

gsa_wmed <- gsa %>%
  filter(
    GSA %in% c(
      1, 2, 3, 4, 5, 6,
      7, 8, 9, 10, 11,
      11.1, 11.2
    )
  )

# Check selected GSAs
unique(gsa_wmed$GSA)

# ------------------------------------------------------------
# Spatially assign ORMEF records to GFCM GSAs
# ------------------------------------------------------------

occ_wmed <- st_join(
  occ_sf,
  gsa_wmed %>% select(GSA),
  join = st_within,
  left = FALSE
)

# ------------------------------------------------------------
# Records during the 1994-1996 Ecopath baseline
# ------------------------------------------------------------

occ_1994_1996 <- occ_wmed %>%
  filter(
    observation_year >= 1994,
    observation_year <= 1996
  ) %>%
  st_drop_geometry() %>%
  select(
    scientificName,
    observation_year,
    eventDate,
    GSA,
    country,
    locality,
    decimalLatitude,
    decimalLongitude,
    minimumDepthInMeters,
    maximumDepthInMeters,
    occurrenceStatus,
    occurrenceRemarks,
    associatedReferences
  ) %>%
  arrange(observation_year, scientificName)

# ------------------------------------------------------------
# First documented Western Mediterranean record
# ------------------------------------------------------------

first_record <- occ_wmed %>%
  group_by(scientificName) %>%
  slice_min(
    observation_year,
    n = 1,
    with_ties = TRUE
  ) %>%
  ungroup() %>%
  st_drop_geometry() %>%
  select(
    scientificName,
    observation_year,
    eventDate,
    GSA,
    country,
    locality,
    decimalLatitude,
    decimalLongitude,
    associatedReferences
  ) %>%
  arrange(observation_year, scientificName)

# ------------------------------------------------------------
# Summary
# ------------------------------------------------------------

summary_species <- tibble(
  scientificName = target_species
) %>%
  left_join(
    occ_wmed %>%
      st_drop_geometry() %>%
      group_by(scientificName) %>%
      summarise(
        first_record_year = min(observation_year),
        n_records = n(),
        GSAs = paste(
          sort(unique(GSA)),
          collapse = ", "
        ),
        .groups = "drop"
      ),
    by = "scientificName"
  ) %>%
  mutate(
    status = case_when(
      is.na(first_record_year) ~
        "No record in GSA 1-11 from 1994 onward",
      
      first_record_year <= 1996 ~
        "Documented in GSA 1-11 during 1994-1996",
      
      TRUE ~
        "First documented after 1996"
    )
  )

# ------------------------------------------------------------
# Results
# ------------------------------------------------------------

summary_species
occ_1994_1996
first_record