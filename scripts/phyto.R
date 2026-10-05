# ============================================================
# PHYTOPLANKTON BIOMASS
# WESTERN MEDITERRANEAN
# 1995 ECOpath BASELINE
#
# Functional groups:
#   Small phytoplankton  = <20 µm
#   Large phytoplankton  = >20 µm
#
# FINAL UNITS:
#   t wet weight km^-2
#
# ------------------------------------------------------------
# DATA SOURCE
# ------------------------------------------------------------
#
# Copernicus Marine Service:
#   GLOBAL_MULTIYEAR_BGC_001_029
#   Global Ocean Biogeochemistry Hindcast
#
# Dataset:
#   cmems_mod_glo_bgc_my_0.25deg_P1M-m
#
# Variables:
#   phyc = phytoplankton carbon concentration
#          (mmol C m^-3)
#
#   chl  = chlorophyll-a concentration
#          (mg Chl-a m^-3)
#
# The Copernicus product is a global PISCES biogeochemical
# hindcast produced by Mercator Ocean International.
#
# Product:
#   GLOBAL_MULTIYEAR_BGC_001_029
#
# DOI:
#   https://doi.org/10.48670/moi-00019
#
# Copernicus product documentation:
#   https://data.marine.copernicus.eu/product/
#   GLOBAL_MULTIYEAR_BGC_001_029/description
#
# Reference:
#   Aumont et al. / PISCES model documentation
#   Product uses PISCES within NEMO.
#
# Copernicus describes this product as:
#   - 0.25 degree horizontal resolution
#   - 75 vertical levels
#   - monthly 3-D phytoplankton carbon fields
#   - simulation beginning in 1993
#   - no data assimilation
#
# ------------------------------------------------------------
# WESTERN MEDITERRANEAN SPATIAL DOMAIN
# ------------------------------------------------------------
#
# GFCM Western Mediterranean GSAs included:
#   GSA 1   Northern Alboran Sea
#   GSA 2   Alboran Island
#   GSA 3   Southern Alboran Sea
#   GSA 4   Algeria
#   GSA 5   Balearic Islands
#   GSA 6   Northern Spain
#   GSA 7   Gulf of Lions
#   GSA 8   Corsica
#   GSA 9   Ligurian / Northern Tyrrhenian
#   GSA 10  Southern / Central Tyrrhenian
#   GSA 11.1 Western Sardinia
#   GSA 11.2 Eastern Sardinia
#
# GSA 12 and GSA 16 are NOT included.
#
# GFCM / FAO reference:
#   GFCM geographical subareas
#   https://www.fao.org/gfcm/data/maps/gsas
#
# ------------------------------------------------------------
# LITERATURE SIZE FRACTION
# ------------------------------------------------------------
#
# Delgado, Latasa & Estrada (1992)
#
# "Variability in the size-fractionated distribution of the
# phytoplankton across the Catalan front of the north-west
# Mediterranean"
#
# Journal of Plankton Research 14:753-771
#
# DOI:
#   10.1093/plankt/14.5.753
#
# Western Mediterranean / Catalan Sea observations:
#
#   February 1990:
#       >20 µm phytoplankton = 55% of autotrophic carbon
#
# Therefore:
#
#       <20 µm fraction = 45%
#       >20 µm fraction = 55%
#
# This is used here as the literature-based size partition
# for the Ecopath phytoplankton groups.
#
# Importantly, the 45/55 split is applied AFTER calculating
# total phytoplankton carbon from the PISCES field.
#
# ------------------------------------------------------------
# CARBON -> WET WEIGHT
# ------------------------------------------------------------
#
# Conversion:
#
#       C/WW = 0.16
#
# therefore:
#
#       WW = C / 0.16
#
# where:
#       C  = g C m^-2
#       WW = g wet weight m^-2
#
# Because:
#
#       1 g m^-2 = 1 t km^-2
#
# the numerical value is unchanged when converting from
# g C m^-2 to t C km^-2.
#
# Phytoplankton C/WW reference:
#
# Yacobi & Zohary (2010) and references therein.
#
# A recent synthesis table reports:
#
#       phytoplankton C/WW = 0.16 +/- 0.03
#
# We use 0.16 as the central estimate.
#
# ------------------------------------------------------------
# SURFACE CHLOROPHYLL -> EUPHOTIC DEPTH
# ------------------------------------------------------------
#
# Reference:
#
# Morel, A. & Berthon, J.-F. (1989)
#
# "Surface pigments, algal biomass profiles, and potential
# production of the euphotic layer: Relationships reinvestigated
# in view of remote-sensing applications"
#
# Limnology and Oceanography 34:1545-1562
#
# DOI:
#   10.4319/lo.1989.34.8.1545
#
# The Morel & Berthon relationships were developed from
# approximately 4,000 oceanic Case-I pigment profiles.
#
# First estimate integrated pigment:
#
#   Ctot = 38.0 * Chl_surface^0.425
#          when surface Chl <= 1 mg m^-3
#
#   Ctot = 40.2 * Chl_surface^0.507
#          when surface Chl > 1 mg m^-3
#
# where:
#   Ctot = integrated pigment in mg m^-2
#
# Euphotic depth is then estimated from Ctot:
#
#   Zeu = 568.2 * Ctot^-0.746
#         for the shallow-euphotic branch
#
#   Zeu = 200.0 * Ctot^-0.293
#         for the deep-euphotic branch
#
# These are the empirical approximations given by
# Morel & Berthon (1989).
#
# ------------------------------------------------------------
# PHYC UNIT CONVERSION
# ------------------------------------------------------------
#
# PISCES phyc:
#
#       mmol C m^-3
#
# Atomic carbon mass:
#
#       1 mol C = 12.011 g C
#
# Therefore:
#
#       1 mmol C = 0.012011 g C
#
# We use:
#
#       0.012011 g C mmol^-1
#
# giving:
#
#       g C m^-3 = phyc * 0.012011
#
# ------------------------------------------------------------
# INTEGRATION THROUGH THE EUPHOTIC ZONE
# ------------------------------------------------------------
#
# PISCES provides phytoplankton carbon at discrete depth
# centres.
#
# Layer thickness is calculated from midpoints between
# adjacent depth centres.
#
# Integrated phytoplankton carbon:
#
#       C_areal = SUM(C_i * dz_i)
#
# where:
#
#       C_i  = phytoplankton carbon concentration
#              (g C m^-3)
#
#       dz_i = thickness represented by layer i (m)
#
# giving:
#
#       g C m^-2
#
# which is numerically equivalent to:
#
#       t C km^-2
#
# ------------------------------------------------------------
# AREA WEIGHTING
# ------------------------------------------------------------
#
# PISCES is on a regular latitude-longitude grid.
#
# Grid-cell area varies with latitude.
#
# For an equal longitude-latitude grid, the relative area
# weighting is proportional to:
#
#       cos(latitude)
#
# This is therefore used for the spatial mean.
#
# The actual Western Mediterranean spatial mask is based on
# GFCM GSAs 1-11.2.
#
# ============================================================


# ------------------------------------------------------------
# PACKAGES
# ------------------------------------------------------------

library(ncdf4)
library(sf)


# ------------------------------------------------------------
# PATHS
# ------------------------------------------------------------

outdir <- file.path(
  path.expand("~"),
  "Work",
  "iMARES",
  "data",
  "plankton_1995_copernicus"
)

dir.create(
  outdir,
  recursive = TRUE,
  showWarnings = FALSE
)


# ------------------------------------------------------------
# FIND NETCDF FILE
# ------------------------------------------------------------

nc_files <- list.files(
  outdir,
  pattern = "\\.nc$",
  full.names = TRUE
)

if (length(nc_files) == 0) {
  stop("No Copernicus NetCDF file found in: ", outdir)
}

ncfile <- nc_files[1]

message("Using NetCDF file:")
message(ncfile)


# ------------------------------------------------------------
# OPEN NETCDF
# ------------------------------------------------------------

nc <- nc_open(ncfile)

lon   <- ncvar_get(nc, "longitude")
lat   <- ncvar_get(nc, "latitude")
depth <- ncvar_get(nc, "depth")

phyc <- ncvar_get(nc, "phyc")
chl  <- ncvar_get(nc, "chl")

time <- ncvar_get(nc, "time")

phyc_units <- ncatt_get(nc, "phyc", "units")$value
chl_units  <- ncatt_get(nc, "chl",  "units")$value

nc_close(nc)


# ------------------------------------------------------------
# CHECK VARIABLES
# ------------------------------------------------------------

message("phyc units: ", phyc_units)
message("chl units:  ", chl_units)

if (!grepl("mmol", phyc_units, ignore.case = TRUE)) {
  stop(
    "Unexpected phyc units: ",
    phyc_units,
    "\nExpected mmol C m-3."
  )
}

if (!grepl("mg", chl_units, ignore.case = TRUE)) {
  stop(
    "Unexpected chl units: ",
    chl_units,
    "\nExpected mg Chl-a m-3."
  )
}


# ------------------------------------------------------------
# PHYC:
# mmol C m^-3 -> g C m^-3
#
# 1 mmol C = 0.012011 g C
# based on atomic carbon mass = 12.011 g mol^-1.
# ------------------------------------------------------------

phyc_gC_m3 <- phyc * 0.012011


# ------------------------------------------------------------
# 1995 ANNUAL MEAN
#
# The dataset contains 12 monthly fields for 1995.
#
# Arithmetic annual mean:
#
#       X_1995 = mean(X_Jan ... X_Dec)
#
# This is appropriate for constructing an annual-average
# Ecopath baseline from monthly model fields.
# ------------------------------------------------------------

phyc_1995 <- apply(
  phyc_gC_m3,
  c(1, 2, 3),
  mean,
  na.rm = TRUE
)

chl_1995 <- apply(
  chl,
  c(1, 2, 3),
  mean,
  na.rm = TRUE
)


# ------------------------------------------------------------
# SURFACE CHLOROPHYLL
#
# PISCES first depth level is approximately 0.5 m.
#
# This is used as the surface chlorophyll concentration.
# ------------------------------------------------------------

chl_surface <- chl_1995[, , 1]


# ------------------------------------------------------------
# MOREL & BERTHON (1989)
#
# Surface Chl -> integrated euphotic pigment
#
# Ctot in mg Chl-a m^-2
# ------------------------------------------------------------

chl_integrated <- matrix(
  NA_real_,
  nrow = length(lon),
  ncol = length(lat)
)

low_chl <- is.finite(chl_surface) &
  chl_surface <= 1

high_chl <- is.finite(chl_surface) &
  chl_surface > 1

chl_integrated[low_chl] <-
  38.0 * chl_surface[low_chl]^0.425

chl_integrated[high_chl] <-
  40.2 * chl_surface[high_chl]^0.507


# ------------------------------------------------------------
# MOREL & BERTHON (1989)
#
# Integrated pigment -> euphotic depth
#
# Original empirical approximation:
#
#   Zeu = 568.2 * Ctot^-0.746
#         for Zeu < approximately 100 m
#
#   Zeu = 200.0 * Ctot^-0.293
#         for Zeu > approximately 100 m
#
# The corresponding Ctot transition is approximately
# 10-11 mg m^-2.
#
# We use 10 mg m^-2 as the operational threshold, matching
# the two-branch implementation commonly used with these
# equations.
# ------------------------------------------------------------

Zeu <- matrix(
  NA_real_,
  nrow = length(lon),
  ncol = length(lat)
)

low_C <- is.finite(chl_integrated) &
  chl_integrated <= 10

high_C <- is.finite(chl_integrated) &
  chl_integrated > 10

Zeu[low_C] <-
  200.0 * chl_integrated[low_C]^(-0.293)

Zeu[high_C] <-
  568.2 * chl_integrated[high_C]^(-0.746)


# ------------------------------------------------------------
# LIMIT Zeu TO AVAILABLE PISCES DEPTH
#
# PISCES data downloaded to 600 m.
#
# This prevents integration beyond the available water-column
# profile.
# ------------------------------------------------------------

Zeu <- pmin(
  Zeu,
  max(depth, na.rm = TRUE)
)


# ------------------------------------------------------------
# PISCES DEPTH LAYER BOUNDARIES
#
# Depths are depth centres.
#
# Boundaries are defined halfway between adjacent centres.
# The first layer begins at 0 m.
# The final layer is extended by the last observed interval.
# ------------------------------------------------------------

depth_upper <- numeric(length(depth))
depth_lower <- numeric(length(depth))

depth_upper[1] <- 0

if (length(depth) > 1) {
  
  depth_upper[2:length(depth)] <-
    (depth[1:(length(depth) - 1)] +
       depth[2:length(depth)]) / 2
}

depth_lower[1:(length(depth) - 1)] <-
  depth_upper[2:length(depth)]

depth_lower[length(depth)] <-
  depth[length(depth)] +
  (depth[length(depth)] -
     depth[length(depth) - 1])

layer_thickness <- depth_lower - depth_upper


# ------------------------------------------------------------
# INTEGRATE PHYC THROUGH EUPHOTIC ZONE
#
# For each grid cell:
#
#       C_euphotic =
#       SUM(C_i * dz_i)
#
# where only the portion of a layer inside Zeu is included.
#
# Output:
#
#       g C m^-2
#
# equivalent numerically to:
#
#       t C km^-2
# ------------------------------------------------------------

phyc_euphotic <- matrix(
  NA_real_,
  nrow = length(lon),
  ncol = length(lat)
)

for (i in seq_along(lon)) {
  
  for (j in seq_along(lat)) {
    
    z <- Zeu[i, j]
    
    if (!is.finite(z)) {
      next
    }
    
    dz_euphotic <- pmax(
      0,
      pmin(depth_lower, z) -
        depth_upper
    )
    
    phyc_profile <- phyc_1995[i, j, ]
    
    ok <- is.finite(phyc_profile) &
      dz_euphotic > 0
    
    if (any(ok)) {
      
      phyc_euphotic[i, j] <-
        sum(
          phyc_profile[ok] *
            dz_euphotic[ok],
          na.rm = TRUE
        )
    }
  }
}


# ------------------------------------------------------------
# CONVERT:
#
# g C m^-2
#
# -> t C km^-2
#
# Identity:
#
# 1 g m^-2 =
# 1,000 g / 1,000,000 m^2
# =
# 1 tonne / 1 km^2
#
# Therefore the numerical value is identical.
# ------------------------------------------------------------

phyc_tC_km2 <- phyc_euphotic


# ------------------------------------------------------------
# GFCM GSA 1-11 MASK
#
# GFCM Western Mediterranean includes:
# GSA 1, 2, 3, 4, 5, 6, 7, 8, 9, 10,
# 11.1 and 11.2.
#
# GSA 12 and GSA 16 are excluded.
#
# Source:
# FAO / GFCM geographical subareas.
# ------------------------------------------------------------

gsa_file <- file.path(
  "/Users/andreaobradors/Documents/daniel/WMed_EwE",
  "GFCM_GSA_shp",
  "GFCM_GSA",
  "gfcm_gsa.shp"
)

gsa <- st_read(
  gsa_file,
  quiet = TRUE
)


# ------------------------------------------------------------
# INSPECT GSA CODE FIELD
# ------------------------------------------------------------

gsa_names <- names(gsa)

print(gsa_names)


# ------------------------------------------------------------
# IMPORTANT:
# Change ONLY this field name if your shapefile uses another
# GSA identifier column.
#
# The code below attempts to identify the GSA field
# automatically.
# ------------------------------------------------------------

gsa_field_candidates <- c(
  "GSA_CODE",
  "GSA",
  "GSA_CODE_",
  "GSA_ID",
  "SMU_CODE"
)

gsa_field <- gsa_field_candidates[
  gsa_field_candidates %in% names(gsa)
][1]

if (is.na(gsa_field)) {
  stop(
    "Could not identify the GSA code field.\n",
    "Available fields:\n",
    paste(names(gsa), collapse = ", ")
  )
}


# ------------------------------------------------------------
# WESTERN MEDITERRANEAN GSA CODES
# ------------------------------------------------------------

western_gsa <- c(
  "1",
  "2",
  "3",
  "4",
  "5",
  "6",
  "7",
  "8",
  "9",
  "10",
  "11",
  "11.1",
  "11.2"
)


# ------------------------------------------------------------
# EXTRACT WESTERN MEDITERRANEAN GSAs
# ------------------------------------------------------------

gsa_code <- as.character(gsa[[gsa_field]])

gsa_wmed <- gsa[
  gsa_code %in% western_gsa,
]


# ------------------------------------------------------------
# UNION WESTERN MEDITERRANEAN GSAs
# ------------------------------------------------------------

gsa_wmed_union <- st_union(gsa_wmed)


# ------------------------------------------------------------
# CREATE PISCES GRID POINTS
# ------------------------------------------------------------

grid <- expand.grid(
  lon = lon,
  lat = lat
)

grid_sf <- st_as_sf(
  grid,
  coords = c("lon", "lat"),
  crs = 4326,
  remove = FALSE
)


# ------------------------------------------------------------
# KEEP PISCES GRID CELLS WHOSE CENTRE FALLS INSIDE
# THE WESTERN MEDITERRANEAN GSA 1-11 MASK
# ------------------------------------------------------------

inside_wmed <- st_within(
  grid_sf,
  gsa_wmed_union,
  sparse = FALSE
)[, 1]

inside_wmed <- matrix(
  inside_wmed,
  nrow = length(lon),
  ncol = length(lat)
)


# ------------------------------------------------------------
# AREA WEIGHTING
#
# For regular longitude-latitude cells:
#
#       area ∝ cos(latitude)
#
# Therefore:
#
#       weighted mean =
#       SUM(value * cos(lat)) /
#       SUM(cos(lat))
#
# only over valid ocean cells inside GSA 1-11.
# ------------------------------------------------------------

area_weight <- matrix(
  rep(
    cos(lat * pi / 180),
    each = length(lon)
  ),
  nrow = length(lon),
  ncol = length(lat)
)


# ------------------------------------------------------------
# TOTAL 1995 PHYTOPLANKTON CARBON
# ------------------------------------------------------------

valid <- is.finite(phyc_tC_km2) &
  inside_wmed &
  is.finite(area_weight)

total_phyto_tC_km2 <-
  sum(
    phyc_tC_km2[valid] *
      area_weight[valid],
    na.rm = TRUE
  ) /
  sum(
    area_weight[valid],
    na.rm = TRUE
  )


# ------------------------------------------------------------
# LITERATURE SIZE FRACTION
#
# Delgado et al. (1992), Catalan Sea:
#
# February 1990:
#
#   >20 µm = 55% of autotrophic carbon
#
# Therefore:
#
#   <20 µm = 45%
#   >20 µm = 55%
#
# This is the literature-derived size split used for the
# Ecopath groups.
# ------------------------------------------------------------

small_fraction <- 0.45
large_fraction <- 0.55

stopifnot(
  abs(
    small_fraction +
      large_fraction - 1
  ) < 1e-12
)


# ------------------------------------------------------------
# SIZE-FRACTIONATED CARBON BIOMASS
# ------------------------------------------------------------

small_phyto_tC_km2 <-
  total_phyto_tC_km2 *
  small_fraction

large_phyto_tC_km2 <-
  total_phyto_tC_km2 *
  large_fraction


# ------------------------------------------------------------
# CARBON -> WET WEIGHT
#
# Central conversion:
#
#       C/WW = 0.16
#
# Therefore:
#
#       WW = C / 0.16
#
# Reference:
# Yacobi & Zohary (2010) and references therein.
#
# Reported uncertainty:
#
#       C/WW = 0.16 +/- 0.03
#
# We retain 0.16 as the central value.
# ------------------------------------------------------------

C_to_WW <- 0.16

small_phyto_tWW_km2 <-
  small_phyto_tC_km2 /
  C_to_WW

large_phyto_tWW_km2 <-
  large_phyto_tC_km2 /
  C_to_WW

total_phyto_tWW_km2 <-
  total_phyto_tC_km2 /
  C_to_WW


# ------------------------------------------------------------
# UNCERTAINTY FROM C/WW CONVERSION
#
# C/WW = 0.16 +/- 0.03
#
# Lower C/WW = 0.13 -> higher wet biomass
# Upper C/WW = 0.19 -> lower wet biomass
# ------------------------------------------------------------

C_to_WW_low <- 0.13
C_to_WW_high <- 0.19

total_phyto_tWW_low <-
  total_phyto_tC_km2 /
  C_to_WW_high

total_phyto_tWW_high <-
  total_phyto_tC_km2 /
  C_to_WW_low


# ------------------------------------------------------------
# FINAL ECOPATH VALUES
# ------------------------------------------------------------

Ecopath_phyto <- data.frame(
  
  FG = c(
    "Small phytoplankton",
    "Large phytoplankton",
    "Total phytoplankton"
  ),
  
  Size_class = c(
    "<20 um",
    ">20 um",
    "Total"
  ),
  
  Fraction = c(
    small_fraction,
    large_fraction,
    1
  ),
  
  Biomass_tC_km2 = c(
    small_phyto_tC_km2,
    large_phyto_tC_km2,
    total_phyto_tC_km2
  ),
  
  C_to_WW = C_to_WW,
  
  Biomass_tWW_km2 = c(
    small_phyto_tWW_km2,
    large_phyto_tWW_km2,
    total_phyto_tWW_km2
  )
)


# ------------------------------------------------------------
# RESULTS
# ------------------------------------------------------------

cat("\n")
cat("============================================================\n")
cat("WESTERN MEDITERRANEAN PHYTOPLANKTON BIOMASS - 1995\n")
cat("GFCM GSAs 1-11.2\n")
cat("============================================================\n\n")

cat(
  "Mean euphotic depth: ",
  round(
    mean(
      Zeu[inside_wmed & is.finite(Zeu)],
      na.rm = TRUE
    ),
    2
  ),
  " m\n",
  sep = ""
)

cat(
  "Mean integrated phytoplankton carbon: ",
  round(total_phyto_tC_km2, 3),
  " t C km-2\n",
  sep = ""
)

cat(
  "C/WW conversion: ",
  C_to_WW,
  "\n",
  sep = ""
)

cat(
  "Total phytoplankton biomass: ",
  round(total_phyto_tWW_km2, 3),
  " t WW km-2\n",
  sep = ""
)

cat(
  "Small phytoplankton (<20 um): ",
  round(small_phyto_tWW_km2, 3),
  " t WW km-2\n",
  sep = ""
)

cat(
  "Large phytoplankton (>20 um): ",
  round(large_phyto_tWW_km2, 3),
  " t WW km-2\n",
  sep = ""
)

cat("\n")
cat("C/WW conversion range:\n")

cat(
  "  Lower biomass: ",
  round(total_phyto_tWW_low, 3),
  " t WW km-2\n",
  sep = ""
)

cat(
  "  Central:        ",
  round(total_phyto_tWW_km2, 3),
  " t WW km-2\n",
  sep = ""
)

cat(
  "  Upper biomass: ",
  round(total_phyto_tWW_high, 3),
  " t WW km-2\n",
  sep = ""
)

cat("\n")
print(
  Ecopath_phyto,
  row.names = FALSE
)


# ============================================================
# MONTHLY DIAGNOSTIC
#
# This is retained to show whether the annual mean is being
# driven by a particular month.
#
# The same euphotic-depth and depth-integration procedure is
# applied independently to every 1995 monthly field.
# ============================================================

n_months <- dim(phyc_gC_m3)[4]

monthly_total_tC <- numeric(n_months)

for (m in seq_len(n_months)) {
  
  chl_surface_m <- chl[, , 1, m]
  
  chl_integrated_m <- matrix(
    NA_real_,
    nrow = length(lon),
    ncol = length(lat)
  )
  
  low_chl_m <- is.finite(chl_surface_m) &
    chl_surface_m <= 1
  
  high_chl_m <- is.finite(chl_surface_m) &
    chl_surface_m > 1
  
  chl_integrated_m[low_chl_m] <-
    38.0 * chl_surface_m[low_chl_m]^0.425
  
  chl_integrated_m[high_chl_m] <-
    40.2 * chl_surface_m[high_chl_m]^0.507
  
  Zeu_m <- matrix(
    NA_real_,
    nrow = length(lon),
    ncol = length(lat)
  )
  
  low_C_m <- is.finite(chl_integrated_m) &
    chl_integrated_m <= 10
  
  high_C_m <- is.finite(chl_integrated_m) &
    chl_integrated_m > 10
  
  Zeu_m[low_C_m] <-
    200.0 * chl_integrated_m[low_C_m]^(-0.293)
  
  Zeu_m[high_C_m] <-
    568.2 * chl_integrated_m[high_C_m]^(-0.746)
  
  Zeu_m <- pmin(
    Zeu_m,
    max(depth, na.rm = TRUE)
  )
  
  phyto_m <- matrix(
    NA_real_,
    nrow = length(lon),
    ncol = length(lat)
  )
  
  for (i in seq_along(lon)) {
    
    for (j in seq_along(lat)) {
      
      z <- Zeu_m[i, j]
      
      if (!is.finite(z)) {
        next
      }
      
      dz_euphotic <- pmax(
        0,
        pmin(depth_lower, z) -
          depth_upper
      )
      
      profile <- phyc_gC_m3[i, j, , m]
      
      ok <- is.finite(profile) &
        dz_euphotic > 0
      
      if (any(ok)) {
        
        phyto_m[i, j] <-
          sum(
            profile[ok] *
              dz_euphotic[ok],
            na.rm = TRUE
          )
      }
    }
  }
  
  valid_m <- is.finite(phyto_m) &
    inside_wmed &
    is.finite(area_weight)
  
  monthly_total_tC[m] <-
    sum(
      phyto_m[valid_m] *
        area_weight[valid_m],
      na.rm = TRUE
    ) /
    sum(
      area_weight[valid_m],
      na.rm = TRUE
    )
}


# ------------------------------------------------------------
# MONTHLY TABLE
# ------------------------------------------------------------

monthly_phyto <- data.frame(
  Month = month.name[seq_len(n_months)],
  Total_tC_km2 = monthly_total_tC,
  Total_tWW_km2 = monthly_total_tC / C_to_WW,
  Small_tWW_km2 =
    monthly_total_tC *
    small_fraction /
    C_to_WW,
  Large_tWW_km2 =
    monthly_total_tC *
    large_fraction /
    C_to_WW
)

cat("\n")
cat("============================================================\n")
cat("MONTHLY 1995 PHYTOPLANKTON BIOMASS\n")
cat("============================================================\n\n")

print(
  monthly_phyto,
  row.names = FALSE
)


# ============================================================
# REFERENCES
# ============================================================

cat("\n")
cat("============================================================\n")
cat("REFERENCES\n")
cat("============================================================\n\n")

cat(
  "1. Copernicus Marine Service.
",
  "   Global Ocean Biogeochemistry Hindcast.
",
  "   Product: GLOBAL_MULTIYEAR_BGC_001_029.
",
  "   DOI: 10.48670/moi-00019.
",
  "   Mercator Ocean International.
\n\n",
  sep = ""
)

cat(
  "2. Morel, A. & Berthon, J.-F. (1989).
",
  "   Surface pigments, algal biomass profiles, and potential
",
  "   production of the euphotic layer: Relationships
",
  "   reinvestigated in view of remote-sensing applications.
",
  "   Limnology and Oceanography 34:1545-1562.
",
  "   DOI: 10.4319/lo.1989.34.8.1545.
\n\n",
  sep = ""
)

cat(
  "3. Delgado, M., Latasa, M. & Estrada, M. (1992).
",
  "   Variability in the size-fractionated distribution of the
",
  "   phytoplankton across the Catalan front of the
",
  "   north-west Mediterranean.
",
  "   Journal of Plankton Research 14:753-771.
",
  "   DOI: 10.1093/plankt/14.5.753.
\n\n",
  sep = ""
)

cat(
  "4. Phytoplankton C/WW conversion:
",
  "   central value = 0.16.
",
  "   Reported as 0.16 +/- 0.03 in a compilation based on
",
  "   Yacobi & Zohary (2010) and references therein.
\n\n",
  sep = ""
)

cat(
  "5. GFCM geographical subareas:
",
  "   FAO / General Fisheries Commission for the Mediterranean.
",
  "   Western Mediterranean GSAs include GSAs 1-11.2;
",
  "   GSA 12 and GSA 16 are outside the Western Mediterranean
",
  "   subarea used here.
",
  sep = ""
)