# ============================================================
# FG70 BIOMASS ESTIMATE
# Western Mediterranean
#
# Approach:
#
# 1. Use Grinyó densities for 40–360 m.
# 2. Use Linares et al. shallow NW Mediterranean densities
#    for 0–40 m.
# 3. Use Grinyó mean colony heights where available.
# 4. Use the E. singularis biomass allometry as the generic
#    gorgonian biomass scaler where species-specific
#    allometry is unavailable:
#
#       DW (g/colony) = 0.070 * H(cm)^2.0209
#
# 5. P. clavata uses the same generic scaler here, but its
#    mean height is independently available from Linares et al.
# 6. For A. hirsuta and B. mollis, where no mean height is
#    available, use the E. singularis mean height as a generic
#    fallback.
#
# This is therefore a regional biomass estimate, not a
# species-specific biomass census.
# ============================================================

# ============================================================
# GSA × DEPTH CORALLIGENOUS AREA
# ============================================================

# Load/rebuild GSA layer if not already in memory

gsa_shp <- paste0(
  "/Users/andreaobradors/Documents/daniel/",
  "WMed_EwE/output/shapefiles/GFCM_GSA_shp/",
  "GFCM_GSA/gfcm_gsa.shp"
)

gsa <- st_read(
  gsa_shp,
  quiet = TRUE
) |>
  st_make_valid()

# Current Western Med definition used in this analysis:
# GSAs 1–10 because the shapefile currently has no SMU_CODE 11

westmed_gsa <- gsa |>
  filter(
    SMU_CODE %in% 1:11
  )


# ============================================================
# TRANSFORM GSA TO SAME EQUAL-AREA CRS
# ============================================================

westmed_gsa_ea <- westmed_gsa |>
  st_transform(equal_area)


# ============================================================
# INTERSECT CORALLIGENOUS HABITAT WITH GSAs
# ============================================================

coral_gsa_depth <- st_intersection(
  coral_ea,
  westmed_gsa_ea |>
    select(SMU_CODE)
)


# ============================================================
# CALCULATE INTERSECTION AREA
# ============================================================

coral_gsa_depth <- coral_gsa_depth |>
  mutate(
    area_km2 =
      as.numeric(
        st_area(geometry)
      ) / 1e6
  )


# ============================================================
# SUM BY GSA × DEPTH
# ============================================================

fg70_area_depth <- coral_gsa_depth |>
  
  st_drop_geometry() |>
  
  group_by(
    SMU_CODE,
    depth_class
  ) |>
  
  summarise(
    coralligenous_area_km2 =
      sum(
        area_km2,
        na.rm = TRUE
      ),
    .groups = "drop"
  )


# ============================================================
# ADD ZEROES FOR MISSING GSA × DEPTH COMBINATIONS
# ============================================================

fg70_area_depth <- fg70_area_depth |>
  
  complete(
    SMU_CODE =
      sort(
        unique(
          westmed_gsa$SMU_CODE
        )
      ),
    
    depth_class =
      factor(
        depth_levels,
        levels = depth_levels
      ),
    
    fill = list(
      coralligenous_area_km2 = 0
    )
  ) |>
  
  mutate(
    depth_class =
      factor(
        depth_class,
        levels = depth_levels
      )
  ) |>
  
  arrange(
    SMU_CODE,
    depth_class
  )


# ============================================================
# CHECK
# ============================================================

cat(
  "\n============================================================\n",
  "CORALLIGENOUS AREA BY GSA × DEPTH\n",
  "============================================================\n"
)

print(
  fg70_area_depth,
  n = Inf
)


# ============================================================
# 1. GENERIC GORGONIAN BIOMASS SCALER
# ============================================================

biomass_a <- 0.070
biomass_b <- 2.0209


# ============================================================
# 2. MEAN COLONY HEIGHTS
#
# Heights from the Grinyó dataset where available.
#
# P. clavata:
# Linares et al. mean = 24.2 cm.
#
# A. hirsuta and B. mollis:
# no suitable mean height available, so use the
# E. singularis mean height as generic fallback.
# ============================================================

generic_height_E_singularis <- 21.84

colony_size <- tribble(
  
  ~species,                    ~mean_H_cm, ~height_source,
  
  "Eunicella singularis",
  21.84,
  "Grinyó",
  
  "Paramuricea clavata",
  24.20,
  "Linares et al. 2008",
  
  "Paramuricea macrospina M1",
  8.92,
  "Grinyó",
  
  "Paramuricea macrospina M2",
  9.46,
  "Grinyó",
  
  "Paramuricea macrospina M3",
  17.53,
  "Grinyó",
  
  "Eunicella cavolini",
  13.03,
  "Grinyó",
  
  "Viminella flagellum",
  41.19,
  "Grinyó",
  
  "Acanthogorgia hirsuta",
  generic_height_E_singularis,
  "Generic E. singularis fallback",
  
  "Callogorgia verticillata",
  26.43,
  "Grinyó",
  
  "Swiftia pallida",
  5.92,
  "Grinyó",
  
  "Bebryce mollis",
  generic_height_E_singularis,
  "Generic E. singularis fallback"
)


# ============================================================
# 3. CALCULATE BIOMASS PER COLONY
#
# Same scaler applied to all species because no suitable
# species-specific allometry is available for most species.
# ============================================================

colony_size <- colony_size |>
  mutate(
    
    biomass_g_colony =
      biomass_a *
      mean_H_cm^biomass_b,
    
    biomass_kg_colony =
      biomass_g_colony / 1000
  )


# ============================================================
# 4. DEEP DENSITY DATA
#
# Grinyó et al. 2016
# Menorca Channel, Western Mediterranean.
# ============================================================

density_deep <- density_species_depth


# ============================================================
# 5. SHALLOW 0–40 m DENSITY
#
# Linares et al. 2008:
#
# E. singularis:
# mean density = 20 colonies/m2
#
# P. clavata:
# mean density = 33 colonies/m2
#
# These are regional NW Mediterranean populations,
# mainly sampled at 15–35 m.
# ============================================================

density_shallow <- tribble(
  
  ~depth_class, ~species, ~density_col_m2, ~density_sd,
  
  "0–10 m",
  "Eunicella singularis",
  20,
  18,
  
  "0–10 m",
  "Paramuricea clavata",
  33,
  14,
  
  "10–40 m",
  "Eunicella singularis",
  20,
  18,
  
  "10–40 m",
  "Paramuricea clavata",
  33,
  14
)


# ============================================================
# 6. COMBINE SHALLOW + DEEP DENSITIES
# ============================================================

density_all_depths <- bind_rows(
  density_shallow,
  density_deep
)


# ============================================================
# 7. JOIN COLONY BIOMASS
# ============================================================

biomass_species_depth <- density_all_depths |>
  
  left_join(
    colony_size,
    by = "species"
  ) |>
  
  mutate(
    
    # t/km2 habitat
    #
    # density = colonies/m2
    # biomass = kg/colony
    #
    # colonies/m2 × kg/colony
    # = kg/m2
    # = t/km2
    biomass_t_km2_habitat =
      density_col_m2 *
      biomass_kg_colony
  )


# ============================================================
# 8. SPECIES-LEVEL RESULTS
# ============================================================

cat(
  "\n============================================================\n",
  "SPECIES × DEPTH BIOMASS\n",
  "============================================================\n"
)

print(
  biomass_species_depth |>
    select(
      depth_class,
      species,
      density_col_m2,
      mean_H_cm,
      biomass_g_colony,
      biomass_t_km2_habitat,
      height_source
    ),
  n = Inf
)


# ============================================================
# 9. SUM SPECIES WITHIN EACH DEPTH CLASS
# ============================================================

biomass_by_depth <- biomass_species_depth |>
  
  group_by(
    depth_class
  ) |>
  
  summarise(
    
    fg70_biomass_t_km2_habitat =
      sum(
        biomass_t_km2_habitat,
        na.rm = TRUE
      ),
    
    n_species =
      n(),
    
    .groups = "drop"
  )


# ============================================================
# 10. ADD DEPTHS WITH NO DENSITY DATA
#
# In your current dataset there is no independent density
# observation for 180–360 m? There is one, so it remains.
# ============================================================

biomass_by_depth <- tibble(
  depth_class =
    factor(
      depth_levels,
      levels = depth_levels
    )
) |>
  
  left_join(
    biomass_by_depth,
    by = "depth_class"
  )


# ============================================================
# 11. JOIN TO CORALLIGENOUS HABITAT AREA
# ============================================================

fg70_biomass_depth <- coral_depth_area |>
  
  left_join(
    biomass_by_depth,
    by = "depth_class"
  ) |>
  
  mutate(
    
    biomass_t =
      coralligenous_area_km2 *
      fg70_biomass_t_km2_habitat
  )


# ============================================================
# 12. PRINT FINAL DEPTH RESULTS
# ============================================================

cat(
  "\n============================================================\n",
  "FG70 BIOMASS BY DEPTH\n",
  "============================================================\n"
)

print(
  fg70_biomass_depth |>
    select(
      depth_class,
      coralligenous_area_km2,
      percent,
      fg70_biomass_t_km2_habitat,
      biomass_t
    )
)


# ============================================================
# 13. TOTAL WESTERN MEDITERRANEAN FG70 BIOMASS
# ============================================================

fg70_total_biomass_t <-
  sum(
    fg70_biomass_depth$biomass_t,
    na.rm = TRUE
  )


cat(
  "\nTotal estimated FG70 biomass: ",
  round(
    fg70_total_biomass_t,
    1
  ),
  " tonnes DW\n",
  sep = ""
)


# ============================================================
# 14. BIOMASS-WEIGHTED MEAN WITHIN CORALLIGENOUS HABITAT
# ============================================================

fg70_mean_biomass_coralligenous <-
  fg70_total_biomass_t /
  total_coralligenous_area


cat(
  "Mean FG70 biomass within mapped coralligenous habitat: ",
  round(
    fg70_mean_biomass_coralligenous,
    3
  ),
  " t/km2 habitat\n",
  sep = ""
)


# ============================================================
# 15. GSA-LEVEL FG70 BIOMASS
#
# Requires fg70_area_depth from the GSA intersection.
# ============================================================

fg70_gsa_biomass <- fg70_area_depth |>
  
  left_join(
    biomass_by_depth,
    by = "depth_class"
  ) |>
  
  mutate(
    
    biomass_t =
      coralligenous_area_km2 *
      fg70_biomass_t_km2_habitat
  ) |>
  
  group_by(
    SMU_CODE
  ) |>
  
  summarise(
    
    coralligenous_area_km2 =
      sum(
        coralligenous_area_km2,
        na.rm = TRUE
      ),
    
    fg70_biomass_t =
      sum(
        biomass_t,
        na.rm = TRUE
      ),
    
    .groups = "drop"
  )


# ============================================================
# 16. GSA BIOMASS DENSITY
# ============================================================

fg70_gsa_biomass <- fg70_gsa_biomass |>
  
  left_join(
    gsa_area,
    by = "SMU_CODE"
  ) |>
  
  mutate(
    
    fg70_biomass_t_km2_model =
      fg70_biomass_t /
      gsa_area_km2
  )


# ============================================================
# 17. PRINT GSA RESULTS
# ============================================================

cat(
  "\n============================================================\n",
  "FG70 BIOMASS BY GSA\n",
  "============================================================\n"
)

print(
  fg70_gsa_biomass,
  n = Inf
)


# ============================================================
# 18. SAVE RESULTS
# ============================================================

write.csv(
  biomass_species_depth,
  file.path(
    dirname(bathymetry_file),
    "FG70_species_depth_biomass_WMed.csv"
  ),
  row.names = FALSE
)

write.csv(
  biomass_by_depth,
  file.path(
    dirname(bathymetry_file),
    "FG70_biomass_by_depth_WMed.csv"
  ),
  row.names = FALSE
)

write.csv(
  fg70_biomass_depth,
  file.path(
    dirname(bathymetry_file),
    "FG70_biomass_depth_area_WMed.csv"
  ),
  row.names = FALSE
)

write.csv(
  fg70_gsa_biomass,
  file.path(
    dirname(bathymetry_file),
    "FG70_biomass_GSA_WMed.csv"
  ),
  row.names = FALSE
)

# ============================================================
# 16.1 WESTERN MEDITERRANEAN FG70 BIOMASS DENSITY
# ============================================================

fg70_wmed_biomass <- fg70_gsa_biomass |>
  
  summarise(
    
    total_coralligenous_area_km2 =
      sum(
        coralligenous_area_km2,
        na.rm = TRUE
      ),
    
    total_fg70_biomass_t =
      sum(
        fg70_biomass_t,
        na.rm = TRUE
      ),
    
    total_wmed_area_km2 =
      sum(
        gsa_area_km2,
        na.rm = TRUE
      ),
    
    fg70_biomass_t_km2 =
      total_fg70_biomass_t /
      total_wmed_area_km2
  )

print(fg70_wmed_biomass)
