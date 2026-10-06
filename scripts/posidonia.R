# ============================================================
# POSIDONIA OCEANICA BIOMASS
# WESTERN MEDITERRANEAN
#
# Spatial data:
#   EUSeaMap 2025
#   EMODnet Seabed Habitats
#
#   GFCM Geographical Sub-Areas (GSAs)
#   GSA 1-11 define the intended Western Mediterranean domain.
#
# Habitat classes:
#   EUNIS2019C = MB252
#       Biocenosis of Posidonia oceanica
#
#   EUNIS2019C = MB2522
#       Ecomorphosis of "barrier-reef" Posidonia oceanica meadows
#
# Excluded:
#   MB2523 = Facies of dead "mattes" of Posidonia oceanica
#
# Biomass:
#   Aboveground = 501 g DW/m2
#
# Source:
#   Boudouresque et al. (2006), citing Duarte & Chiscano (1999)
#
#   Boudouresque, C.F., Mayot, N. & Pergent, G. (2006).
#   The outstanding traits of the functioning of the Posidonia
#   oceanica seagrass ecosystem.
#   Biologia Marina Mediterranea 13(4): 109-113.
#
#   Duarte, C.M. & Chiscano, C.L. (1999).
#   Seagrass biomass and production: A reassessment.
#   Aquatic Botany 65: 159-174.
#   DOI: 10.1016/S0304-3770(99)00038-8
#
# Biomass component:
#   ABOVEGROUND ONLY
#   Belowground biomass (1,611 g DW/m2) is intentionally excluded.
#
# Dry-weight to wet-weight conversion:
#   DW:FW = 0.24
#   Therefore:
#       FW = DW / 0.24
#       FW = DW * 4.166667
#
# Source:
#   Apostolaki et al. (2024).
#   Patterns of Carbon and Nitrogen Accumulation in Seagrass
#   (Posidonia oceanica) Meadows of the Eastern Mediterranean Sea.
#   Journal of Geophysical Research: Biogeosciences.
#   DOI: 10.1029/2024JG008163
#
# Important:
#   The 0.24 ratio was measured for fresh P. oceanica leaves
#   and is therefore being used here as an aboveground
#   leaf biomass DW -> FW conversion.
#
# Unit conversion:
#   1 g DW/m2 = 1 t DW/km2
#   because:
#       1 g = 1e-6 t
#       1 m2 = 1e-6 km2
#
# Therefore:
#   501 g DW/m2 = 501 t DW/km2 habitat
#
# Wet-weight biomass density:
#   501 / 0.24 = 2,087.5 t FW/km2 habitat
#
# Ecopath:
#   Habitat biomass density =
#       biomass within Posidonia habitat
#
#   Domain-average biomass =
#       total Posidonia biomass / total model area
#
# ============================================================


library(sf)
library(dplyr)
library(readr)


# ------------------------------------------------------------
# 1. INPUTS
# ------------------------------------------------------------

# EUSeaMap 2025 geodatabase
#
# Source:
# EMODnet Seabed Habitats
# EUSeaMap 2025 Broad-scale Predictive Habitat Map for Europe
#
# EUSeaMap is a predictive broad-scale habitat map based on
# environmental and seabed descriptors.
#
# EUSeaMap 2025 covers the Mediterranean and other European seas.
#
# Reference:
# https://emodnet.ec.europa.eu/en/seabed-habitats

gdb <- "/Users/andreaobradors/Downloads/EUSeaMap_2025/EUSeaMap_2025.gdb"


# GFCM GSA boundaries
#
# Source:
# European Commission / GFCM Geographical Sub-Areas
#
# GSA 1-11 correspond to:
#   1  Northern Alboran Sea
#   2  Alboran Island
#   3  Southern Alboran Sea
#   4  Algeria
#   5  Balearic Islands
#   6  Northern Spain
#   7  Gulf of Lions
#   8  Corsica
#   9  Ligurian and Northern Tyrrhenian Sea
#   10 Southern and Central Tyrrhenian Sea
#   11 Sardinia
#
# Reference:
# European Commission DCF GFCM-GSAs
# https://dcf.ec.europa.eu/data-calls/definitions-and-terminology/g/gfcm-gsas_en

gsa_shp <- "/Users/andreaobradors/Documents/daniel/WMed_EwE/output/shapefiles/GFCM_GSA_shp/GFCM_GSA/gfcm_gsa.shp"


# ------------------------------------------------------------
# 2. READ EUSEAMAP 2025
# ------------------------------------------------------------

euseamap <- st_read(
  gdb,
  layer = "EUSeaMap_2025",
  quiet = TRUE
)

cat(
  "\nEUSeaMap loaded:",
  nrow(euseamap),
  "features\n"
)


# ------------------------------------------------------------
# 3. SELECT LIVING POSIDONIA
# ------------------------------------------------------------
#
# IMPORTANT:
# The Posidonia habitat code is stored in EUNIS2019C.
#
# MB252:
#   Biocenosis of Posidonia oceanica
#
# MB2522:
#   Ecomorphosis of "barrier-reef" Posidonia oceanica meadows
#
# MB2523:
#   Dead matte
#
# MB2523 is excluded because this calculation represents
# living aboveground Posidonia biomass.
#
# EUSeaMap 2025 uses EUNIS habitat classifications.
#
# ------------------------------------------------------------

posidonia_raw <- euseamap |>
  filter(
    EUNIS2019C %in% c(
      "MB252",
      "MB2522"
    )
  )

cat(
  "\nLiving Posidonia features:",
  nrow(posidonia_raw),
  "\n"
)

cat("\nPosidonia EUNIS2019C:\n")

print(
  table(
    posidonia_raw$EUNIS2019C,
    useNA = "ifany"
  )
)


# ------------------------------------------------------------
# 4. READ GFCM GSA BOUNDARIES
# ------------------------------------------------------------

gsa <- st_read(
  gsa_shp,
  quiet = TRUE
)

cat("\nGSA fields:\n")

print(
  names(gsa)
)


# ------------------------------------------------------------
# 5. SELECT GSA 1-11
# ------------------------------------------------------------
#
# The intended model domain is GFCM GSA 1-11.
#
# IMPORTANT:
# In the current shapefile, SMU_CODE = 11 is apparently not
# present as a single value. The observed Sardinia-related
# codes should be checked before claiming that GSA 11 is
# included.
#
# Therefore this filter reproduces the requested GSA 1-11
# selection but the resulting selected codes are printed below.
#
# ------------------------------------------------------------

westmed_gsa <- gsa |>
  filter(
    SMU_CODE %in% 1:11
  ) |>
  st_make_valid()

cat(
  "\nNumber of GSA polygons:",
  nrow(westmed_gsa),
  "\n"
)

cat("\nSelected GSA codes:\n")

print(
  sort(
    unique(
      westmed_gsa$SMU_CODE
    )
  )
)


# ------------------------------------------------------------
# 6. EQUAL-AREA CRS
# ------------------------------------------------------------
#
# Albers Equal Area projection centered on the Western
# Mediterranean.
#
# This projection is used for area calculations so that
# polygon areas are calculated in metres and then converted
# to km2.
#
# Projection:
#   lat_1 = 35
#   lat_2 = 45
#   lat_0 = 40
#   lon_0 = 5
#
# Units:
#   metres
#
# ------------------------------------------------------------

equal_area <- st_crs(
  "+proj=aea +lat_1=35 +lat_2=45 +lat_0=40 +lon_0=5 +datum=WGS84 +units=m +no_defs"
)


# ------------------------------------------------------------
# 7. CLEAN AND PREPARE POSIDONIA GEOMETRIES
# ------------------------------------------------------------
#
# EUSeaMap contains complex MULTIPOLYGON geometries.
#
# Some polygons are geometrically invalid.
# st_make_valid() repairs these before spatial operations.
#
# S2 is disabled because the spatial processing below uses
# planar geometry followed by an equal-area projection.
#
# ------------------------------------------------------------

sf::sf_use_s2(FALSE)

posidonia <- posidonia_raw[
  !st_is_empty(posidonia_raw),
]

posidonia <- st_make_valid(
  posidonia
)

cat(
  "\nInvalid Posidonia geometries after repair:",
  sum(
    !st_is_valid(posidonia)
  ),
  "\n"
)


# ------------------------------------------------------------
# 8. WESTERN MEDITERRANEAN PRE-CLIP
# ------------------------------------------------------------
#
# EUSeaMap 2025 covers a much larger European area.
#
# A preliminary geographic clip reduces processing to:
#
#   longitude: -6 to 16 E
#   latitude:   35 to 45 N
#
# The final model domain is still determined by the GFCM
# GSA polygons, not this rectangular bounding box.
#
# ------------------------------------------------------------

posidonia <- st_crop(
  posidonia,
  xmin = -6,
  xmax = 16,
  ymin = 35,
  ymax = 45
)

cat(
  "\nPosidonia features after Western Mediterranean clip:",
  nrow(posidonia),
  "\n"
)


# ------------------------------------------------------------
# 9. TRANSFORM BOTH DATASETS TO EQUAL-AREA CRS
# ------------------------------------------------------------

posidonia <- st_transform(
  posidonia,
  equal_area
)

westmed_gsa <- st_transform(
  westmed_gsa,
  equal_area
)


# ------------------------------------------------------------
# 10. CREATE WESTERN MEDITERRANEAN MODEL DOMAIN
# ------------------------------------------------------------

westmed_geometry <- st_union(
  st_geometry(westmed_gsa)
)

westmed <- st_sf(
  geometry = westmed_geometry,
  crs = st_crs(westmed_gsa)
)


# ------------------------------------------------------------
# 11. WESTERN MEDITERRANEAN MODEL AREA
# ------------------------------------------------------------

westmed_area_km2 <- as.numeric(
  st_area(westmed)
) / 1e6

cat(
  "\nWestern Mediterranean model area:",
  round(
    westmed_area_km2,
    2
  ),
  "km2\n"
)


# ------------------------------------------------------------
# 12. CLIP POSIDONIA TO GFCM MODEL DOMAIN
# ------------------------------------------------------------

posidonia_wm <- st_intersection(
  posidonia,
  westmed
)

cat(
  "\nPosidonia features after GSA-domain clipping:",
  nrow(posidonia_wm),
  "\n"
)


# ------------------------------------------------------------
# 13. DISSOLVE POSIDONIA
# ------------------------------------------------------------
#
# EUSeaMap contains many adjacent polygons.
#
# Dissolving them before calculating area prevents overlapping
# polygons from being counted more than once.
#
# ------------------------------------------------------------

posidonia_wm_union <- st_union(
  st_geometry(posidonia_wm)
)


# ------------------------------------------------------------
# 14. TOTAL MAPPED POSIDONIA AREA
# ------------------------------------------------------------

posidonia_area_km2 <- as.numeric(
  st_area(posidonia_wm_union)
) / 1e6

cat(
  "\n============================================\n"
)

cat(
  "TOTAL MAPPED POSIDONIA AREA\n"
)

cat(
  "============================================\n"
)

cat(
  round(
    posidonia_area_km2,
    2
  ),
  "km2\n"
)


# ------------------------------------------------------------
# 15. POSIDONIA AREA AS FRACTION OF MODEL DOMAIN
# ------------------------------------------------------------

posidonia_fraction <-
  posidonia_area_km2 /
  westmed_area_km2

cat(
  "\nPosidonia fraction of model area:",
  round(
    posidonia_fraction,
    6
  ),
  "\n"
)

cat(
  "Posidonia percentage of model area:",
  round(
    posidonia_fraction * 100,
    3
  ),
  "%\n"
)


# ------------------------------------------------------------
# 16. POSIDONIA BIOMASS DENSITY
# ------------------------------------------------------------
#
# SOURCE:
#
# Boudouresque et al. (2006) report:
#
#   Aboveground biomass = 501 g DW/m2
#   Belowground biomass = 1,611 g DW/m2
#
# These values are attributed to:
#
# Duarte & Chiscano (1999)
#
# The present Ecopath estimate intentionally uses ONLY the
# aboveground component.
#
# Belowground biomass is excluded because the current model
# parameter represents the living aboveground producer biomass.
#
# ------------------------------------------------------------

posidonia_aboveground_g_m2 <- 501


# ------------------------------------------------------------
# 17. UNIT CONVERSION:
# g DW/m2 -> t DW/km2
# ------------------------------------------------------------
#
# Exact conversion:
#
#   1 g = 1e-6 t
#   1 km2 = 1e6 m2
#
# Therefore:
#
#   1 g/m2
#   = 1 g × 1e6 m2/km2
#   = 1e6 g/km2
#   = 1 t/km2
#
# Thus:
#
#   501 g DW/m2
#   = 501 t DW/km2 habitat
#
# ------------------------------------------------------------

posidonia_aboveground_t_dw_km2 <-
  posidonia_aboveground_g_m2


# ------------------------------------------------------------
# 18. DRY WEIGHT -> WET WEIGHT
# ------------------------------------------------------------
#
# The 2024 study by Apostolaki et al. measured fresh
# Posidonia oceanica leaves and reported:
#
#   DW:FW = 0.24
#
# Therefore:
#
#   DW / FW = 0.24
#
# and:
#
#   FW = DW / 0.24
#
# Conversion factor:
#
#   FW/DW = 1 / 0.24
#         = 4.166667
#
# Applying this to the aboveground biomass:
#
#   501 t DW/km2 / 0.24
#   = 2,087.5 t FW/km2
#
# IMPORTANT:
# This is a provisional conversion for aboveground/leaf biomass.
# The DW:FW ratio is from fresh leaves and may vary with tissue,
# season and site.
#
# ------------------------------------------------------------

posidonia_dw_fw_ratio <- 0.24

posidonia_dw_to_fw_factor <-
  1 / posidonia_dw_fw_ratio

posidonia_biomass_t_km2 <-
  posidonia_aboveground_t_dw_km2 *
  posidonia_dw_to_fw_factor

cat(
  "\nPosidonia aboveground biomass:",
  posidonia_aboveground_t_dw_km2,
  "t DW/km2 habitat\n"
)

cat(
  "DW:FW ratio:",
  posidonia_dw_fw_ratio,
  "\n"
)

cat(
  "DW -> FW conversion factor:",
  round(
    posidonia_dw_to_fw_factor,
    6
  ),
  "\n"
)

cat(
  "Posidonia aboveground biomass:",
  posidonia_biomass_t_km2,
  "t FW/km2 habitat\n"
)


# ------------------------------------------------------------
# 19. TOTAL POSIDONIA BIOMASS
# ------------------------------------------------------------
#
# Total biomass =
#
#   mapped Posidonia area
#   × biomass density within Posidonia habitat
#
# ------------------------------------------------------------

total_posidonia_biomass_t <-
  posidonia_area_km2 *
  posidonia_biomass_t_km2

cat(
  "\nTotal Posidonia aboveground biomass:",
  round(
    total_posidonia_biomass_t,
    2
  ),
  "t FW\n"
)


# ------------------------------------------------------------
# 20. ECOPATH DOMAIN-AVERAGE BIOMASS
# ------------------------------------------------------------
#
# This is the biomass averaged across the ENTIRE model domain.
#
# It is NOT the biomass density inside Posidonia habitat.
#
# Formula:
#
#   Ecopath B =
#       total Posidonia biomass
#       /
#       total model area
#
# Equivalent to:
#
#   habitat biomass density × habitat proportion
#
# ------------------------------------------------------------

ecopath_posidonia_B <-
  total_posidonia_biomass_t /
  westmed_area_km2

cat(
  "\nEcopath Posidonia B:",
  round(
    ecopath_posidonia_B,
    4
  ),
  "t FW/km2 model area\n"
)


# ------------------------------------------------------------
# 21. INTERSECT POSIDONIA WITH EACH GSA
# ------------------------------------------------------------

posidonia_gsa <- st_intersection(
  posidonia,
  westmed_gsa
)

cat(
  "\nPosidonia-GSA intersection features:",
  nrow(posidonia_gsa),
  "\n"
)


# ------------------------------------------------------------
# 22. CALCULATE UNIQUE POSIDONIA AREA BY GSA
# ------------------------------------------------------------
#
# Each GSA is dissolved independently.
#
# This prevents overlapping/adjacent EUSeaMap polygons from
# causing double counting within each GSA.
#
# ------------------------------------------------------------

gsa_codes <- sort(
  unique(
    westmed_gsa$SMU_CODE
  )
)

posidonia_gsa_list <- lapply(
  gsa_codes,
  function(gsa_code) {
    
    gsa_poly <- westmed_gsa[
      westmed_gsa$SMU_CODE == gsa_code,
    ]
    
    pos_poly <- posidonia_gsa[
      posidonia_gsa$SMU_CODE == gsa_code,
    ]
    
    # GSA area
    gsa_union <- st_union(
      st_geometry(gsa_poly)
    )
    
    gsa_area_km2 <- as.numeric(
      st_area(gsa_union)
    ) / 1e6
    
    # Posidonia area
    if (nrow(pos_poly) == 0) {
      
      pos_area_km2 <- 0
      
    } else {
      
      pos_union <- st_union(
        st_geometry(pos_poly)
      )
      
      pos_area_km2 <- as.numeric(
        st_area(pos_union)
      ) / 1e6
    }
    
    data.frame(
      SMU_CODE = gsa_code,
      gsa_area_km2 = gsa_area_km2,
      posidonia_area_km2 = pos_area_km2
    )
  }
)

posidonia_gsa_table <- bind_rows(
  posidonia_gsa_list
)


# ------------------------------------------------------------
# 23. ADD BIOMASS AND HABITAT COVERAGE BY GSA
# ------------------------------------------------------------

posidonia_gsa_table <- posidonia_gsa_table |>
  mutate(
    
    # Biomass density within Posidonia habitat
    biomass_t_km2 =
      posidonia_biomass_t_km2,
    
    # Fraction of each GSA covered by Posidonia
    posidonia_proportion =
      posidonia_area_km2 /
      gsa_area_km2,
    
    # Percentage of each GSA covered by Posidonia
    posidonia_percent =
      100 *
      posidonia_proportion,
    
    # Total Posidonia biomass within the GSA
    posidonia_biomass_t =
      posidonia_area_km2 *
      biomass_t_km2,
    
    # Domain-average biomass contribution
    # if interpreted relative to that individual GSA
    ecopath_B_t_km2 =
      posidonia_biomass_t /
      gsa_area_km2
    
  ) |>
  arrange(
    SMU_CODE
  )


# ------------------------------------------------------------
# 24. CHECK GSA TOTAL
# ------------------------------------------------------------

gsa_total_area_km2 <-
  sum(
    posidonia_gsa_table$posidonia_area_km2,
    na.rm = TRUE
  )

gsa_total_biomass_t <-
  sum(
    posidonia_gsa_table$posidonia_biomass_t,
    na.rm = TRUE
  )

gsa_total_model_area_km2 <-
  sum(
    posidonia_gsa_table$gsa_area_km2,
    na.rm = TRUE
  )


cat(
  "\n============================================\n"
)

cat(
  "GSA TOTAL CHECK\n"
)

cat(
  "============================================\n"
)

cat(
  "GSA-summed model area:",
  round(
    gsa_total_model_area_km2,
    2
  ),
  "km2\n"
)

cat(
  "Western Mediterranean model area:",
  round(
    westmed_area_km2,
    2
  ),
  "km2\n"
)

cat(
  "GSA model-area difference:",
  round(
    gsa_total_model_area_km2 -
      westmed_area_km2,
    4
  ),
  "km2\n"
)

cat(
  "\nGSA-summed Posidonia area:",
  round(
    gsa_total_area_km2,
    2
  ),
  "km2\n"
)

cat(
  "Western Mediterranean Posidonia area:",
  round(
    posidonia_area_km2,
    2
  ),
  "km2\n"
)

cat(
  "Posidonia-area difference:",
  round(
    gsa_total_area_km2 -
      posidonia_area_km2,
    4
  ),
  "km2\n"
)

cat(
  "\nGSA-summed biomass:",
  round(
    gsa_total_biomass_t,
    2
  ),
  "t FW\n"
)

cat(
  "Western Mediterranean biomass:",
  round(
    total_posidonia_biomass_t,
    2
  ),
  "t FW\n"
)


# ------------------------------------------------------------
# 25. AREA-WEIGHTED BIOMASS ACROSS ALL GSAs
# ------------------------------------------------------------
#
# This is the biomass density averaged over the entire
# GFCM-GSA model domain.
#
# Formula:
#
#   weighted B =
#
#       sum(Posidonia biomass in each GSA)
#       /
#       sum(GSA area)
#
# Equivalent to:
#
#       Posidonia biomass density
#       ×
#       total Posidonia proportion
#
# Units:
#       t FW/km2 model area
#
# ------------------------------------------------------------

weighted_B <-
  sum(
    posidonia_gsa_table$posidonia_biomass_t,
    na.rm = TRUE
  ) /
  sum(
    posidonia_gsa_table$gsa_area_km2,
    na.rm = TRUE
  )

cat(
  "\nArea-weighted Posidonia biomass:",
  round(
    weighted_B,
    4
  ),
  "t FW/km2 model area\n"
)


# ------------------------------------------------------------
# 26. FINAL SUMMARY
# ------------------------------------------------------------

posidonia_summary <- tibble(
  
  metric = c(
    
    "Western Mediterranean model area",
    
    "Mapped living Posidonia area",
    
    "Posidonia fraction of model area",
    
    "Posidonia aboveground biomass density DW",
    
    "DW:FW ratio",
    
    "DW to FW conversion factor",
    
    "Posidonia aboveground biomass density FW",
    
    "Total Posidonia aboveground biomass",
    
    "Ecopath Posidonia biomass",
    
    "Area-weighted Posidonia biomass"
    
  ),
  
  value = c(
    
    westmed_area_km2,
    
    posidonia_area_km2,
    
    posidonia_fraction,
    
    posidonia_aboveground_t_dw_km2,
    
    posidonia_dw_fw_ratio,
    
    posidonia_dw_to_fw_factor,
    
    posidonia_biomass_t_km2,
    
    total_posidonia_biomass_t,
    
    ecopath_posidonia_B,
    
    weighted_B
    
  ),
  
  unit = c(
    
    "km2",
    
    "km2",
    
    "proportion",
    
    "t DW/km2 habitat",
    
    "DW:FW",
    
    "FW/DW",
    
    "t FW/km2 habitat",
    
    "t FW",
    
    "t FW/km2 model area",
    
    "t FW/km2 model area"
    
  )
)


# ------------------------------------------------------------
# 27. PRINT FINAL RESULTS
# ------------------------------------------------------------

cat(
  "\n============================================\n"
)

cat(
  "FINAL POSIDONIA SUMMARY\n"
)

cat(
  "============================================\n\n"
)

print(
  posidonia_summary
)


cat(
  "\n============================================\n"
)

cat(
  "POSIDONIA BIOMASS BY GSA\n"
)

cat(
  "============================================\n\n"
)

print(
  posidonia_gsa_table
)


# ------------------------------------------------------------
# 28. OPTIONAL CSV OUTPUTS
# ------------------------------------------------------------

# write_csv(
#   posidonia_gsa_table,
#   "data/posidonia_biomass_by_GSA.csv"
# )

# write_csv(
#   posidonia_summary,
#   "data/posidonia_biomass_summary.csv"
# )


# ============================================================
# REFERENCES
# ============================================================
#
# 1. EUSeaMap 2025
#
# EMODnet Seabed Habitats.
# EUSeaMap 2025 Broad-scale Predictive Habitat Map for Europe.
#
# https://emodnet.ec.europa.eu/en/seabed-habitats
#
# EUSeaMap 2025 is a predictive broad-scale habitat product.
# The map includes the Mediterranean Sea and uses EUNIS
# classifications among its habitat classification systems.
#
#
# 2. GFCM Geographical Sub-Areas
#
# European Commission.
# GFCM Geographical Sub-Areas (GSAs).
#
# https://dcf.ec.europa.eu/data-calls/definitions-and-terminology/g/gfcm-gsas_en
#
# GFCM GSAs 1-11 define the western Mediterranean region used
# here, with GSA 11 corresponding to Sardinia.
#
#
# 3. Posidonia biomass
#
# Duarte, C.M. & Chiscano, C.L. (1999).
# Seagrass biomass and production: A reassessment.
# Aquatic Botany 65: 159-174.
#
# DOI:
# 10.1016/S0304-3770(99)00038-8
#
# Boudouresque, C.F., Mayot, N. & Pergent, G. (2006).
# The outstanding traits of the functioning of the
# Posidonia oceanica seagrass ecosystem.
# Biologia Marina Mediterranea 13(4): 109-113.
#
# The latter reports the P. oceanica average biomass as:
#   501 g DW/m2 aboveground
#   1,611 g DW/m2 belowground
#
# Only the aboveground 501 g DW/m2 value is used here.
#
#
# 4. Dry-weight / wet-weight conversion
#
# Apostolaki, E.T. et al. (2024).
# Patterns of Carbon and Nitrogen Accumulation in Seagrass
# (Posidonia oceanica) Meadows of the Eastern Mediterranean Sea.
# Journal of Geophysical Research: Biogeosciences.
#
# DOI:
# 10.1029/2024JG008163
#
# Reported DW:FW ratio for fresh P. oceanica leaves:
#   DW:FW = 0.24
#
# Therefore:
#   FW = DW / 0.24
#      = DW * 4.166667
#
#
# 5. Spatial area conversion
#
# Equal-area Albers projection:
#
#   +proj=aea
#   +lat_1=35
#   +lat_2=45
#   +lat_0=40
#   +lon_0=5
#   +datum=WGS84
#   +units=m
#
# Areas are calculated in m2 and converted to km2:
#
#   area_km2 = area_m2 / 1,000,000
#
#
# 6. Biomass unit conversion
#
#   1 g/m2 = 1 t/km2
#
# Therefore:
#
#   501 g DW/m2
#       =
#   501 t DW/km2
#
# After DW -> FW conversion:
#
#   501 / 0.24
#       =
#   2,087.5 t FW/km2 habitat
#
# ============================================================
# END
# ============================================================