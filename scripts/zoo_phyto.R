# ============================================================
# WESTERN MEDITERRANEAN PLANKTON BIOMASS
# 1995
#
# Functional groups:
#   1. Small phytoplankton
#   2. Large phytoplankton
#   3. Meso + microzooplankton
#   4. Macrozooplankton
#
# Sources:
#   Phytoplankton:
#   Copernicus GLOBAL_MULTIYEAR_BGC_001_029
#   PISCES
#   1995 monthly data
#
#   Zooplankton:
#   Copernicus GLOBAL_MULTIYEAR_BGC_001_033
#   SEAPODYM LMTL
#   1998-2000 used as temporary proxy for 1995
#
# Units of final biomass:
#   t C km-2
#
# IMPORTANT:
#   The size fractions at the end are provisional.
# ============================================================


# ------------------------------------------------------------
# 0. PACKAGES
# ------------------------------------------------------------

required_packages <- c(
  "ncdf4",
  "terra",
  "dplyr",
  "readr"
)

for (p in required_packages) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}

library(ncdf4)
library(terra)
library(dplyr)
library(readr)


# ------------------------------------------------------------
# 1. PATHS
# ------------------------------------------------------------

outdir <- "/Users/daniel/Work/iMARES/data/plankton_1995_copernicus"

dir.create(
  outdir,
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------
# 2. WESTERN MEDITERRANEAN DOMAIN
# ------------------------------------------------------------

lon_min <- -6
lon_max <- 16

lat_min <- 35
lat_max <- 45


# ------------------------------------------------------------
# 3. HELPER FUNCTIONS
# ------------------------------------------------------------

# ------------------------------------------------------------
# Area-weighted mean
#
# Only valid ocean cells are included.
# ------------------------------------------------------------

area_weighted_mean <- function(
    raster,
    lon_min,
    lon_max,
    lat_min,
    lat_max
) {
  
  r <- crop(
    raster,
    ext(
      lon_min,
      lon_max,
      lat_min,
      lat_max
    )
  )
  
  values <- values(r)
  
  cell_area <- cellSize(
    r,
    unit = "km"
  )
  
  weights <- values(cell_area)
  
  ok <- is.finite(values) &
    is.finite(weights) &
    weights > 0
  
  if (!any(ok)) {
    return(NA_real_)
  }
  
  sum(
    values[ok] * weights[ok],
    na.rm = TRUE
  ) /
    sum(
      weights[ok],
      na.rm = TRUE
    )
}


# ------------------------------------------------------------
# Read NetCDF time
# ------------------------------------------------------------

decode_nc_time <- function(
    nc,
    time_name
) {
  
  time_values <- ncvar_get(
    nc,
    time_name
  )
  
  time_att <- ncatt_get(
    nc,
    time_name,
    "units"
  )$value
  
  calendar_att <- ncatt_get(
    nc,
    time_name,
    "calendar"
  )$value
  
  if (is.null(calendar_att) ||
      is.na(calendar_att) ||
      calendar_att == "") {
    
    calendar_att <- "standard"
  }
  
  origin_string <- sub(
    "^[^ ]+ since ",
    "",
    time_att
  )
  
  unit_string <- sub(
    " since.*$",
    "",
    time_att
  )
  
  origin <- as.POSIXct(
    origin_string,
    tz = "UTC"
  )
  
  if (grepl(
    "seconds",
    unit_string,
    ignore.case = TRUE
  )) {
    
    dates <- origin +
      time_values
    
  } else if (grepl(
    "hours",
    unit_string,
    ignore.case = TRUE
  )) {
    
    dates <- origin +
      time_values * 3600
    
  } else if (grepl(
    "days",
    unit_string,
    ignore.case = TRUE
  )) {
    
    dates <- origin +
      time_values * 86400
    
  } else {
    
    stop(
      "Unknown NetCDF time unit: ",
      unit_string
    )
  }
  
  as.Date(dates)
}


# ------------------------------------------------------------
# Calculate layer thickness from depth centers
#
# Example:
# 0.5, 1.5, 2.5 m
#
# creates approximately:
# 1, 1, 1 m layers
# ------------------------------------------------------------

depth_thickness <- function(depth) {
  
  depth <- as.numeric(depth)
  
  if (length(depth) == 1) {
    return(1)
  }
  
  dz <- numeric(
    length(depth)
  )
  
  dz[1] <- (
    depth[2] -
      depth[1]
  ) / 2
  
  dz[length(depth)] <- (
    depth[length(depth)] -
      depth[length(depth) - 1]
  ) / 2
  
  if (length(depth) > 2) {
    
    dz[2:(length(depth) - 1)] <-
      (
        depth[3:length(depth)] -
          depth[1:(length(depth) - 2)]
      ) / 2
  }
  
  dz
}


# ------------------------------------------------------------
# Find dimension position
# ------------------------------------------------------------

find_dim <- function(
    dims,
    patterns
) {
  
  hit <- which(
    sapply(
      dims,
      function(x) {
        any(
          grepl(
            patterns,
            x,
            ignore.case = TRUE
          )
        )
      }
    )
  )
  
  if (length(hit) == 0) {
    return(NA_integer_)
  }
  
  hit[1]
}


# ============================================================
# PART A
# PHYTOPLANKTON
# ============================================================

cat(
  "\n",
  "============================================================\n",
  "PART A: PHYTOPLANKTON\n",
  "============================================================\n"
)


# ------------------------------------------------------------
# Find phytoplankton NetCDF
# ------------------------------------------------------------

nc_files <- list.files(
  outdir,
  pattern = "\\.nc$",
  full.names = TRUE
)

phyto_candidates <- character(0)

for (f in nc_files) {
  
  test_nc <- try(
    nc_open(f),
    silent = TRUE
  )
  
  if (inherits(
    test_nc,
    "try-error"
  )) {
    next
  }
  
  vars <- names(
    test_nc$var
  )
  
  nc_close(
    test_nc
  )
  
  if ("phyc" %in% vars) {
    
    phyto_candidates <- c(
      phyto_candidates,
      f
    )
  }
}


if (length(phyto_candidates) == 0) {
  
  stop(
    "Could not find a NetCDF containing variable 'phyc'."
  )
}

if (length(phyto_candidates) > 1) {
  
  cat(
    "\nMultiple phytoplankton files found:\n"
  )
  
  print(
    phyto_candidates
  )
  
  stop(
    "More than one file contains 'phyc'."
  )
}


phyto_file <- phyto_candidates[1]


cat(
  "\nPhytoplankton file:\n",
  phyto_file,
  "\n"
)


# ------------------------------------------------------------
# Open phytoplankton file
# ------------------------------------------------------------

nc_phyto <- nc_open(
  phyto_file
)

on.exit(
  try(nc_close(nc_phyto), silent = TRUE),
  add = TRUE
)


cat(
  "\nVariables:\n"
)

print(
  names(
    nc_phyto$var
  )
)


# ------------------------------------------------------------
# phyc information
# ------------------------------------------------------------

phyc_var <- nc_phyto$var[["phyc"]]

phyc_dims <- sapply(
  phyc_var$dim,
  function(x) x$name
)

cat(
  "\nphyc dimensions:\n"
)

print(
  phyc_dims
)


phyc_units <- ncatt_get(
  nc_phyto,
  "phyc",
  "units"
)$value

cat(
  "\nphyc units:\n",
  phyc_units,
  "\n"
)


# ------------------------------------------------------------
# Identify dimensions
# ------------------------------------------------------------

lon_i <- find_dim(
  phyc_dims,
  "longitude|lon"
)

lat_i <- find_dim(
  phyc_dims,
  "latitude|lat"
)

depth_i <- find_dim(
  phyc_dims,
  "depth|lev"
)

time_i <- find_dim(
  phyc_dims,
  "time"
)


if (any(
  is.na(
    c(
      lon_i,
      lat_i,
      depth_i,
      time_i
    )
  )
)) {
  
  stop(
    "Could not identify all phyc dimensions."
  )
}


# ------------------------------------------------------------
# Coordinates
# ------------------------------------------------------------

lon <- ncvar_get(
  nc_phyto,
  phyc_dims[lon_i]
)

lat <- ncvar_get(
  nc_phyto,
  phyc_dims[lat_i]
)

depth <- ncvar_get(
  nc_phyto,
  phyc_dims[depth_i]
)


# ------------------------------------------------------------
# Time
# ------------------------------------------------------------

dates_phyto <- decode_nc_time(
  nc_phyto,
  phyc_dims[time_i]
)

cat(
  "\nPhytoplankton dates:\n"
)

print(
  range(
    dates_phyto
  )
)


# ------------------------------------------------------------
# Read phyc
# ------------------------------------------------------------

phyc <- ncvar_get(
  nc_phyto,
  "phyc"
)


# ------------------------------------------------------------
# Reorder to:
#
# longitude x latitude x depth x time
# ------------------------------------------------------------

phyc <- aperm(
  phyc,
  c(
    lon_i,
    lat_i,
    depth_i,
    time_i
  )
)


cat(
  "\nReordered phyc dimensions:\n"
)

print(
  dim(phyc)
)


# ------------------------------------------------------------
# Select 1995
# ------------------------------------------------------------

idx_1995 <- which(
  format(
    dates_phyto,
    "%Y"
  ) == "1995"
)


if (length(idx_1995) == 0) {
  
  stop(
    "No 1995 phytoplankton data found."
  )
}


phyc_1995 <- phyc[
  , ,
  ,
  idx_1995,
  drop = FALSE
]


# ------------------------------------------------------------
# Annual mean
#
# Important:
# mean(..., na.rm=TRUE) is used here because some
# bottom/deep cells can be NA.
# ------------------------------------------------------------

phyc_annual <- apply(
  phyc_1995,
  c(1, 2, 3),
  mean,
  na.rm = TRUE
)


# ------------------------------------------------------------
# Check units
# ------------------------------------------------------------

cat(
  "\nRaw phyc summary:\n"
)

print(
  summary(
    as.vector(
      phyc_annual
    )
  )
)


# ------------------------------------------------------------
# Depth layer thickness
# ------------------------------------------------------------

dz <- depth_thickness(
  depth
)

cat(
  "\nDepth range:\n"
)

print(
  range(depth)
)

cat(
  "\nTotal derived water-column thickness:\n",
  sum(dz),
  "m\n"
)


# ------------------------------------------------------------
# Convert phyc to carbon inventory
#
# phyc:
#   mmol C m-3
#
# Multiply by dz:
#   mmol C m-2
#
# Multiply by 0.012:
#   g C m-2
#
# Numerical equivalence:
#   1 g C m-2 = 1 t C km-2
# ------------------------------------------------------------

phyc_inventory <- array(
  0,
  dim = dim(
    phyc_annual
  )
)


valid_depth_count <- array(
  0,
  dim = dim(
    phyc_annual
  )[1:2]
)


for (k in seq_along(depth)) {
  
  layer <- phyc_annual[
    , ,
    k
  ]
  
  valid <- is.finite(
    layer
  )
  
  valid_depth_count[
    valid
  ] <-
    valid_depth_count[
      valid
    ] + 1
  
  layer[!valid] <- 0
  
  phyc_inventory[
    , ,
    k
  ] <-
    layer *
    dz[k]
}


# ------------------------------------------------------------
# Sum through the water column
# ------------------------------------------------------------

phyc_integrated <- apply(
  phyc_inventory,
  c(1, 2),
  sum,
  na.rm = TRUE
)


# ------------------------------------------------------------
# Cells with zero valid depths are land/no-data
# ------------------------------------------------------------

phyc_integrated[
  valid_depth_count == 0
] <- NA


# ------------------------------------------------------------
# Convert mmol C/m2 to g C/m2
# ------------------------------------------------------------

phyc_integrated_tC_km2 <-
  phyc_integrated *
  0.012


# ------------------------------------------------------------
# Diagnostic
# ------------------------------------------------------------

cat(
  "\nIntegrated phytoplankton summary:\n"
)

print(
  summary(
    as.vector(
      phyc_integrated_tC_km2
    )
  )
)


# ------------------------------------------------------------
# Raster
# ------------------------------------------------------------

r_phyto <- rast(
  nrows = length(lat),
  ncols = length(lon),
  xmin = min(lon),
  xmax = max(lon),
  ymin = min(lat),
  ymax = max(lat),
  crs = "EPSG:4326"
)

values(
  r_phyto
) <- as.vector(
  phyc_integrated_tC_km2
)


# ------------------------------------------------------------
# Crop Western Mediterranean
# ------------------------------------------------------------

r_phyto_wmed <- crop(
  r_phyto,
  ext(
    lon_min,
    lon_max,
    lat_min,
    lat_max
  )
)


# ------------------------------------------------------------
# Save raster
# ------------------------------------------------------------

writeRaster(
  r_phyto_wmed,
  file.path(
    outdir,
    "total_phytoplankton_1995_tC_km2.tif"
  ),
  overwrite = TRUE
)


# ------------------------------------------------------------
# Area-weighted Western Mediterranean biomass
# ------------------------------------------------------------

phyto_total_1995 <-
  area_weighted_mean(
    r_phyto_wmed,
    lon_min,
    lon_max,
    lat_min,
    lat_max
  )


cat(
  "\nTotal phytoplankton biomass:\n",
  round(
    phyto_total_1995,
    4
  ),
  "t C km-2\n"
)


# ============================================================
# PART B
# ZOOPLANKTON
# ============================================================

cat(
  "\n",
  "============================================================\n",
  "PART B: ZOOPLANKTON\n",
  "============================================================\n"
)


# ------------------------------------------------------------
# Exact file identified from your diagnostic
# ------------------------------------------------------------

zoo_file <- file.path(
  outdir,
  "zooplankton_1998_2000_WMed.nc"
)


if (!file.exists(zoo_file)) {
  
  stop(
    "Zooplankton file not found:\n",
    zoo_file
  )
}


cat(
  "\nZooplankton file:\n",
  zoo_file,
  "\n"
)


# ------------------------------------------------------------
# Open
# ------------------------------------------------------------

nc_zoo <- nc_open(
  zoo_file
)


# ------------------------------------------------------------
# Check variable
# ------------------------------------------------------------

if (!"zooc" %in% names(nc_zoo$var)) {
  
  nc_close(
    nc_zoo
  )
  
  stop(
    "Variable 'zooc' not found."
  )
}


zooc_var <- nc_zoo$var[["zooc"]]

zooc_dims <- sapply(
  zooc_var$dim,
  function(x) x$name
)


cat(
  "\nzooc dimensions:\n"
)

print(
  zooc_dims
)


zooc_units <- ncatt_get(
  nc_zoo,
  "zooc",
  "units"
)$value


cat(
  "\nzooc units:\n",
  zooc_units,
  "\n"
)


# ------------------------------------------------------------
# Coordinates
# ------------------------------------------------------------

lon_zoo <- ncvar_get(
  nc_zoo,
  "longitude"
)

lat_zoo <- ncvar_get(
  nc_zoo,
  "latitude"
)


# ------------------------------------------------------------
# Time
# ------------------------------------------------------------

time_name_zoo <- zooc_dims[
  grepl(
    "time",
    zooc_dims,
    ignore.case = TRUE
  )
][1]


dates_zoo <- decode_nc_time(
  nc_zoo,
  time_name_zoo
)


cat(
  "\nZooplankton date range:\n"
)

print(
  range(
    dates_zoo
  )
)


# ------------------------------------------------------------
# Read zooc
#
# Expected:
# longitude x latitude x time
# ------------------------------------------------------------

zooc <- ncvar_get(
  nc_zoo,
  "zooc"
)


cat(
  "\nRaw zooc dimensions:\n"
)

print(
  dim(zooc)
)


# ------------------------------------------------------------
# Check raw values
# ------------------------------------------------------------

cat(
  "\nRaw zooc summary:\n"
)

print(
  summary(
    as.vector(
      zooc
    )
  )
)


# ------------------------------------------------------------
# Select 1998-2000
#
# This is a temporary historical proxy because the
# LMTL product starts in 1998.
# ------------------------------------------------------------

idx_zoo <- which(
  dates_zoo >=
    as.Date("1998-01-01") &
    dates_zoo <=
    as.Date("2000-12-31")
)


if (length(idx_zoo) == 0) {
  
  nc_close(
    nc_zoo
  )
  
  stop(
    "No 1998-2000 zooplankton data found."
  )
}


zooc_1998_2000 <- zooc[
  , ,
  idx_zoo,
  drop = FALSE
]


# ------------------------------------------------------------
# Temporal mean
# ------------------------------------------------------------

zooc_proxy <- apply(
  zooc_1998_2000,
  c(1, 2),
  mean,
  na.rm = TRUE
)


# ------------------------------------------------------------
# Check proxy
# ------------------------------------------------------------

cat(
  "\n1998-2000 zooc proxy summary:\n"
)

print(
  summary(
    as.vector(
      zooc_proxy
    )
  )
)


# ------------------------------------------------------------
# IMPORTANT UNIT CONVERSION
#
# zooc = g C m-2
#
# 1 g C m-2 =
# 1 tonne C km-2
#
# Therefore NO numerical conversion is required.
# ------------------------------------------------------------

zooc_proxy_tC_km2 <-
  zooc_proxy


# ------------------------------------------------------------
# Raster
# ------------------------------------------------------------

r_zoo <- rast(
  nrows = length(lat_zoo),
  ncols = length(lon_zoo),
  xmin = min(lon_zoo),
  xmax = max(lon_zoo),
  ymin = min(lat_zoo),
  ymax = max(lat_zoo),
  crs = "EPSG:4326"
)

values(
  r_zoo
) <- as.vector(
  zooc_proxy_tC_km2
)


# ------------------------------------------------------------
# Crop Western Mediterranean
# ------------------------------------------------------------

r_zoo_wmed <- crop(
  r_zoo,
  ext(
    lon_min,
    lon_max,
    lat_min,
    lat_max
  )
)


# ------------------------------------------------------------
# Save raster
# ------------------------------------------------------------

writeRaster(
  r_zoo_wmed,
  file.path(
    outdir,
    "total_zooplankton_1998_2000_proxy_tC_km2.tif"
  ),
  overwrite = TRUE
)


# ------------------------------------------------------------
# Area-weighted mean
# ------------------------------------------------------------

zoo_total_proxy <-
  area_weighted_mean(
    r_zoo_wmed,
    lon_min,
    lon_max,
    lat_min,
    lat_max
  )


cat(
  "\nTotal zooplankton biomass proxy:\n",
  round(
    zoo_total_proxy,
    4
  ),
  "t C km-2\n"
)


# ------------------------------------------------------------
# Close NetCDF
# ------------------------------------------------------------

nc_close(
  nc_zoo
)


# ============================================================
# PART C
# LITERATURE-BASED SIZE-FRACTION ALLOCATION
# AND CARBON -> WET-WEIGHT CONVERSION
# ============================================================

cat(
  "\n",
  "============================================================\n",
  "PART C: LITERATURE-BASED PLANKTON ALLOCATION\n",
  "AND CARBON -> WET WEIGHT CONVERSION\n",
  "============================================================\n"
)


# ============================================================
# IMPORTANT UNIT RELATIONSHIP
# ============================================================
#
# The Copernicus biomass values are:
#
#     t C km-2
#
# Ecopath biomass will be:
#
#     t WW km-2
#
# Because:
#
#     1 g C m-2 = 1 t C km-2
#
# and:
#
#     1 g WW m-2 = 1 t WW km-2
#
# the numerical conversion is simply based on the
# carbon / wet-weight ratio.
#
#
# If:
#
#     C/WW = 0.16
#
# then:
#
#     1 g C = 1 / 0.16 = 6.25 g WW
#
# ============================================================


# ============================================================
# 1. CARBON / WET-WEIGHT CONVERSION FACTORS
# ============================================================

# ------------------------------------------------------------
# Phytoplankton
#
# C / WW = 0.16
#
# Therefore:
#
# 1 g C = 6.25 g WW
#
# Reference:
# Yacobi & Zohary (2010) and references therein,
# as summarized in Scheffold et al. carbon conversion table.
# ------------------------------------------------------------

phyto_C_per_WW <- 0.16

phyto_C_to_WW <-
  1 / phyto_C_per_WW


# ------------------------------------------------------------
# Microzooplankton / protozooplankton
#
# C / WW = 0.07
#
# Therefore:
#
# 1 g C = 14.29 g WW
#
# Reference:
# Fenchel & Finlay (1983)
# ------------------------------------------------------------

microzoo_C_per_WW <- 0.07

microzoo_C_to_WW <-
  1 / microzoo_C_per_WW


# ------------------------------------------------------------
# Mesozooplankton
#
# C / WW = 0.09
#
# Therefore:
#
# 1 g C = 11.11 g WW
#
# Reference:
# Kiørboe (2013) and references therein.
# ------------------------------------------------------------

mesozoo_C_per_WW <- 0.09

mesozoo_C_to_WW <-
  1 / mesozoo_C_per_WW


# ------------------------------------------------------------
# Macrozooplankton
#
# We use the general non-gelatinous zooplankton value:
#
# C / WW = 0.09
#
# Therefore:
#
# 1 g C = 11.11 g WW
#
# Reference:
# Kiørboe (2013).
#
# IMPORTANT:
# This is a generalized value because the macrozooplankton
# FG may contain several taxonomic groups with different
# body composition.
# ------------------------------------------------------------

macrozoo_C_per_WW <- 0.09

macrozoo_C_to_WW <-
  1 / macrozoo_C_per_WW


# ============================================================
# 2. PHYTOPLANKTON SIZE FRACTION
# ============================================================
#
# Copernicus PISCES provides total phytoplankton carbon
# biomass (phyc).
#
# We need to map this to the two EwE groups:
#
#   Small phytoplankton
#   Large phytoplankton
#
# Current allocation:
#
#   Small = 60%
#   Large = 40%
#
# This remains PROVISIONAL because the total phyc variable
# does not directly provide this exact EwE split.
# ============================================================

small_phyto_fraction <- 0.60

large_phyto_fraction <- 0.40


if (
  abs(
    small_phyto_fraction +
    large_phyto_fraction -
    1
  ) > 1e-10
) {
  stop(
    "Phytoplankton fractions do not sum to 1."
  )
}


small_phyto_1995_C <-
  phyto_total_1995 *
  small_phyto_fraction


large_phyto_1995_C <-
  phyto_total_1995 *
  large_phyto_fraction


# ============================================================
# 3. TOTAL ZOOPLANKTON SIZE ALLOCATION
# ============================================================
#
# The Copernicus LMTL product provides:
#
#   zooc = total zooplankton carbon biomass
#
# It does not directly provide the four EwE size groups.
#
# We therefore allocate:
#
#   Meso + micro = 70%
#   Macro        = 30%
#
# This is a literature-constrained PROVISIONAL allocation.
#
# Western Mediterranean observations show substantial
# micrometazooplankton biomass and mesozooplankton dominance.
#
# Fernández de Puelles et al. (2014):
#
#   micrometazooplankton = 29-41% of total zooplankton
#   biomass in Balearic Sea observations.
#
# Therefore the allocation is not simply an arbitrary
# 80/20 split, but the exact 70/30 split remains an assumption.
# ============================================================

meso_micro_fraction <- 0.70

macro_zoo_fraction <- 0.30


if (
  abs(
    meso_micro_fraction +
    macro_zoo_fraction -
    1
  ) > 1e-10
) {
  stop(
    "Zooplankton fractions do not sum to 1."
  )
}


meso_micro_1995_C <-
  zoo_total_proxy *
  meso_micro_fraction


macro_zoo_1995_C <-
  zoo_total_proxy *
  macro_zoo_fraction


# ============================================================
# 4. INTERNAL SPLIT OF MESO + MICROZOOPLANKTON
# ============================================================
#
# The combined EwE group contains:
#
#   Microzooplankton
#   Mesozooplankton
#
# We use:
#
#   Micro = 35%
#   Meso  = 65%
#
# The 35% value is approximately the midpoint of the
# 29-41% micrometazooplankton biomass contribution reported
# from the Balearic Sea.
#
# Reference:
#
# Fernández de Puelles, M.L. et al. (2014).
# Journal of Marine Systems 138: 82-94.
#
# IMPORTANT:
# Their micrometazooplankton definition is not identical to
# our Ecopath microzooplankton FG. Therefore this is a
# PROVISIONAL mapping, not a direct measurement.
# ============================================================

micro_fraction_within_meso_micro <- 0.35

meso_fraction_within_meso_micro <- 0.65


if (
  abs(
    micro_fraction_within_meso_micro +
    meso_fraction_within_meso_micro -
    1
  ) > 1e-10
) {
  stop(
    "Micro/meso fractions do not sum to 1."
  )
}


microzoo_1995_C <-
  meso_micro_1995_C *
  micro_fraction_within_meso_micro


mesozoo_1995_C <-
  meso_micro_1995_C *
  meso_fraction_within_meso_micro


# ============================================================
# 5. CONVERT EACH COMPONENT TO WET WEIGHT
# ============================================================

# ------------------------------------------------------------
# Small phytoplankton
# ------------------------------------------------------------

small_phyto_1995_WW <-
  small_phyto_1995_C *
  phyto_C_to_WW


# ------------------------------------------------------------
# Large phytoplankton
# ------------------------------------------------------------

large_phyto_1995_WW <-
  large_phyto_1995_C *
  phyto_C_to_WW


# ------------------------------------------------------------
# Microzooplankton
# ------------------------------------------------------------

microzoo_1995_WW <-
  microzoo_1995_C *
  microzoo_C_to_WW


# ------------------------------------------------------------
# Mesozooplankton
# ------------------------------------------------------------

mesozoo_1995_WW <-
  mesozoo_1995_C *
  mesozoo_C_to_WW


# ------------------------------------------------------------
# Combined meso + microzooplankton
# ------------------------------------------------------------

meso_micro_1995_WW <-
  microzoo_1995_WW +
  mesozoo_1995_WW


# ------------------------------------------------------------
# Macrozooplankton
# ------------------------------------------------------------

macro_zoo_1995_WW <-
  macro_zoo_1995_C *
  macrozoo_C_to_WW


# ============================================================
# PART D
# FINAL ECOPATH TABLE
# ============================================================

plankton_1995_WW <- tibble(
  
  year = 1995,
  
  functional_group = c(
    "Small phytoplankton",
    "Large phytoplankton",
    "Meso + microzooplankton",
    "Macrozooplankton"
  ),
  
  biomass_t_km2 = c(
    small_phyto_1995_WW,
    large_phyto_1995_WW,
    meso_micro_1995_WW,
    macro_zoo_1995_WW
  ),
  
  biomass_tC_km2 = c(
    small_phyto_1995_C,
    large_phyto_1995_C,
    meso_micro_1995_C,
    macro_zoo_1995_C
  ),
  
  carbon_to_wet_weight = c(
    phyto_C_to_WW,
    phyto_C_to_WW,
    NA,
    macrozoo_C_to_WW
  ),
  
  carbon_per_wet_weight = c(
    phyto_C_per_WW,
    phyto_C_per_WW,
    NA,
    macrozoo_C_per_WW
  ),
  
  biomass_unit = "t WW km-2",
  
  source = c(
    "Copernicus GLOBAL_MULTIYEAR_BGC_001_029 PISCES, 1995",
    "Copernicus GLOBAL_MULTIYEAR_BGC_001_029 PISCES, 1995",
    "Copernicus GLOBAL_MULTIYEAR_BGC_001_033 LMTL, 1998-2000 proxy",
    "Copernicus GLOBAL_MULTIYEAR_BGC_001_033 LMTL, 1998-2000 proxy"
  ),
  
  allocation_fraction = c(
    small_phyto_fraction,
    large_phyto_fraction,
    meso_micro_fraction,
    macro_zoo_fraction
  ),
  
  allocation_basis = c(
    "Provisional PISCES -> EwE size allocation",
    "Provisional PISCES -> EwE size allocation",
    "70% of total zooplankton; Balearic observations constrain size structure",
    "30% of total zooplankton; provisional allocation"
  ),
  
  conversion_reference = c(
    "Yacobi & Zohary (2010); Scheffold et al. synthesis",
    "Yacobi & Zohary (2010); Scheffold et al. synthesis",
    "Fenchel & Finlay (1983); Kiørboe (2013); Fernández de Puelles et al. (2014)",
    "Kiørboe (2013)"
  ),
  
  allocation_status = c(
    "PROVISIONAL",
    "PROVISIONAL",
    "PROVISIONAL",
    "PROVISIONAL"
  )
)


# ============================================================
# PART E
# ADD FULL INTERNAL ZOOPLANKTON INFORMATION
# ============================================================
#
# This table is useful for documenting how the combined
# meso + micro group was constructed.
# ============================================================

zooplankton_internal <- tibble(
  
  year = 1995,
  
  source_total_group =
    "Copernicus GLOBAL_MULTIYEAR_BGC_001_033",
  
  source_period =
    "1998-2000 proxy for 1995",
  
  total_zooplankton_tC_km2 =
    zoo_total_proxy,
  
  meso_micro_fraction =
    meso_micro_fraction,
  
  macro_fraction =
    macro_zoo_fraction,
  
  meso_micro_tC_km2 =
    meso_micro_1995_C,
  
  macro_tC_km2 =
    macro_zoo_1995_C,
  
  micro_fraction_within_meso_micro =
    micro_fraction_within_meso_micro,
  
  meso_fraction_within_meso_micro =
    meso_fraction_within_meso_micro,
  
  micro_tC_km2 =
    microzoo_1995_C,
  
  meso_tC_km2 =
    mesozoo_1995_C,
  
  micro_C_to_WW =
    microzoo_C_to_WW,
  
  meso_C_to_WW =
    mesozoo_C_to_WW,
  
  macro_C_to_WW =
    macrozoo_C_to_WW,
  
  allocation_reference =
    "Fernández de Puelles et al. (2014), Balearic Sea",
  
  allocation_status =
    "PROVISIONAL"
)


# ============================================================
# PART F
# PRINT RESULTS
# ============================================================

cat(
  "\n",
  "============================================================\n",
  "1995 WESTERN MEDITERRANEAN PLANKTON BIOMASS\n",
  "============================================================\n\n"
)


print(
  plankton_1995_WW
)


cat(
  "\n\nInternal zooplankton allocation:\n\n"
)

print(
  zooplankton_internal
)


# ============================================================
# PART G
# SAVE OUTPUTS
# ============================================================

write_csv(
  plankton_1995_WW,
  file.path(
    outdir,
    "WMed_plankton_biomass_1995_WW_literature_based.csv"
  )
)


write_csv(
  zooplankton_internal,
  file.path(
    outdir,
    "WMed_zooplankton_allocation_1995.csv"
  )
)


# ============================================================
# PART H
# SUMMARY
# ============================================================

total_phyto_WW <-
  small_phyto_1995_WW +
  large_phyto_1995_WW

total_zoo_WW <-
  meso_micro_1995_WW +
  macro_zoo_1995_WW

total_plankton_WW <-
  total_phyto_WW +
  total_zoo_WW


cat(
  "\n",
  "============================================================\n",
  "SUMMARY\n",
  "============================================================\n\n"
)


cat(
  "Total phytoplankton:\n",
  round(
    phyto_total_1995,
    4
  ),
  "t C km-2\n"
)

cat(
  "Total phytoplankton:\n",
  round(
    total_phyto_WW,
    4
  ),
  "t WW km-2\n\n"
)


cat(
  "Total zooplankton proxy:\n",
  round(
    zoo_total_proxy,
    4
  ),
  "t C km-2\n"
)

cat(
  "Total zooplankton proxy:\n",
  round(
    total_zoo_WW,
    4
  ),
  "t WW km-2\n\n"
)


cat(
  "Small phytoplankton:\n",
  round(
    small_phyto_1995_WW,
    4
  ),
  "t WW km-2\n"
)

cat(
  "Large phytoplankton:\n",
  round(
    large_phyto_1995_WW,
    4
  ),
  "t WW km-2\n"
)

cat(
  "Meso + microzooplankton:\n",
  round(
    meso_micro_1995_WW,
    4
  ),
  "t WW km-2\n"
)

cat(
  "Macrozooplankton:\n",
  round(
    macro_zoo_1995_WW,
    4
  ),
  "t WW km-2\n\n"
)


cat(
  "Total four plankton groups:\n",
  round(
    total_plankton_WW,
    4
  ),
  "t WW km-2\n\n"
)


cat(
  "Output directory:\n",
  outdir,
  "\n\n"
)


cat(
  "Files created:\n",
  "- total_phytoplankton_1995_tC_km2.tif\n",
  "- total_zooplankton_1998_2000_proxy_tC_km2.tif\n",
  "- WMed_plankton_biomass_1995_WW_literature_based.csv\n",
  "- WMed_zooplankton_allocation_1995.csv\n"
)


# ============================================================
# END
# ============================================================