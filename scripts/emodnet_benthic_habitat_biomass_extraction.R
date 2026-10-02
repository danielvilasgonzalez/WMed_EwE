# ============================================================
# WESTERN MEDITERRANEAN HABITAT AREAS -> benthic_habitat_biomass_literature.csv
#
# Computes a real EMODnet-derived habitat area for Posidonia, Cymodocea,
# Macroalgae, Coralligenous, and Gorgonians/octocorals, combines each
# with a biomass-density figure, and writes the result directly in the
# schema `benthic_habitat_biomass_literature.csv` already uses (Group,
# Year, Density_value, Density_unit, Habitat_area_km2, Collection_year,
# Source_citation, Confidence, Notes) - so the output can be merged
# straight into that file.
#
# Run this on a machine with network access to EMODnet's WFS geoserver,
# the `sf`/`emodnet.wfs` packages, and the wmed_ices/seagrass_wmed
# objects already built.
#
# METHOD
#
# 1. Habitat area, not domain-wide depth band: each group's real mapped
#    extent is pulled from EMODnet Seabed Habitats (via WFS), clipped to
#    the ICES Western Mediterranean ecoregion polygon (wmed_ices), and
#    summed in an equal-area projection - the area a density figure gets
#    multiplied against, instead of assuming it applies uniformly
#    across a whole depth band.
#
# 2. Domain consistency: this script does NOT compute a final t/km2.
#    Every other FG in this pipeline gets its Ecopath_B by dividing
#    Biomass_t by `sum(strata_area_by_area$area_km2)` - the MEDITS-
#    bathymetry-clipped GSA 1-11 domain (01_biomass.R). The ICES WMed
#    ecoregion used here for clipping is a different boundary
#    definition, so this script stops at Habitat_area_km2 and density
#    and lets 01_biomass.R's own extrapolate_density_row() do the final
#    Biomass_t -> t/km2 conversion using the pipeline's own domain
#    denominator. Re-clip to GSA 1-11 instead if the two boundaries
#    differ meaningfully in total area.
#
# 3. Year: every row carries Year = 1995 (the Ecopath baseline).
#    Collection_year records the true sampling year where it differs -
#    load_manual_cited_biomass_group() flags any gap automatically,
#    same as every other manual-cited row in this pipeline.
#
# 4. Occupancy fraction: a mapped habitat polygon isn't 100% occupied
#    at the cited density everywhere within it. occupancy_fraction is
#    an explicit model assumption (not an EMODnet observation) applied
#    to the raw polygon area to get an effective habitat area - carried
#    into the output CSV's Notes column rather than baked silently into
#    one number.
#
# 5. Density confidence: Posidonia and Cymodocea use a real citation
#    (Guidetti et al. 2002). Macroalgae, Coralligenous, and Gorgonians/
#    octocorals use round-number placeholder densities, marked
#    Confidence = "Provisional - not a literature citation". Ballesteros
#    2006 (Oceanography and Marine Biology: An Annual Review 44:123-195)
#    is the standard Mediterranean synthesis reference for macroalgae/
#    coralligenous biomass, worth extracting a real figure from before
#    treating those two as final. For gorgonians, this pipeline
#    separately has a real per-species citation (Ambroso et al. 2019,
#    Cap de Creus AFDM densities: Paramuricea clavata 19.16, Eunicella
#    singularis 2.95, Leptogorgia sarmentosa 0.19 g AFDM/m2) worth
#    comparing against the placeholder here (AFDM and DW are different
#    units, so don't average the two without resolving that first).
#
# 6. Gorgonians/octocorals habitat area: EMODnet's seabed-habitats
#    catalog has no dedicated gorgonian/octocoral extent product - the
#    closest match ("live hard coral cover") is a different taxon
#    (scleractinian/stony coral). This script reuses the coralligenous
#    polygon as the gorgonian habitat proxy instead, the same
#    convention used elsewhere in this pipeline (gorgonians live within
#    coralligenous habitat; EUSeaMap's own coralligenous class is
#    labelled a "coralligenous/gorgonian proxy" for the same reason).
#
# 7. Layer selection never guesses: choose_layer() stops and prints
#    every candidate whenever more than one EMODnet product matches a
#    search term. Each EOV product ships as a "points" layer (occurrence
#    records, no polygon geometry) and a "poly" layer (the actual mapped
#    polygons) under near-identical titles; select_poly_layer() narrows
#    to the polygon variant via the layer_name's own "_poly"/"_points"
#    suffix, since a free-text title/abstract search can match both. If
#    a search term matches nothing, choose_layer() prints every
#    coral/cnidaria/reef/garden-adjacent product in the catalog so the
#    real product name can be found directly.
# ============================================================

library(sf)
library(dplyr)
library(stringr)
library(tibble)
library(emodnet.wfs)


# ============================================================
# 0. SETTINGS
# ============================================================

aea <- "+proj=laea +lat_0=40 +lon_0=5 +datum=WGS84 +units=m +no_defs"

YEAR_ECOPATH_TARGET <- 1995   # the pipeline's Ecopath baseline year - matches YEAR_ECOPATH <- 1994:1996 in 01_biomass.R

# Your existing ICES WMed polygon
# wmed_ices

wmed_area_km2 <- wmed_ices |>
  st_transform(aea) |>
  st_area() |>
  sum(na.rm = TRUE) |>
  as.numeric() / 1e6

cat("\nICES Western Mediterranean area:", round(wmed_area_km2, 1), "km2",
    "(reference only - the final Ecopath_B denominator is the pipeline's own",
    " strata_area_by_area, computed downstream in 01_biomass.R)\n")


# ============================================================
# 1. DOWNLOAD A WFS LAYER WITHIN WMED
# ============================================================

download_wmed_wfs <- function(layer, service = "emodnet_open", wmed = wmed_ices) {
  bbox <- st_bbox(wmed)
  bbox_string <- paste(bbox["xmin"], bbox["ymin"], bbox["xmax"], bbox["ymax"], sep = ",")
  url <- paste0(
    "https://ows.emodnet-seabedhabitats.eu/geoserver/", service, "/wfs?",
    "SERVICE=WFS&VERSION=2.0.0&REQUEST=GetFeature&TYPENAMES=", service, ":", layer,
    "&OUTPUTFORMAT=application/json&SRSNAME=EPSG:4326&BBOX=", bbox_string, ",EPSG:4326"
  )
  cat("\nDownloading:", layer, "\n")
  tryCatch(st_read(url, quiet = TRUE),
           error = function(e) { warning("\nCould not download layer ", layer, ":\n", conditionMessage(e)); NULL })
}


# ============================================================
# 2. GENERIC AREA FUNCTION
#
# Keeps only polygon/multipolygon geometry, intersects with the ICES
# WMed polygon, reprojects to an equal-area CRS, and sums.
# ============================================================

calculate_area_wmed <- function(x, wmed = wmed_ices, projection = aea) {
  if (is.null(x)) return(NULL)
  if (!inherits(x, "sf")) stop("Object is not an sf object.")
  x <- st_make_valid(x)
  geom_type <- as.character(unique(st_geometry_type(x)))
  if (!any(geom_type %in% c("POLYGON", "MULTIPOLYGON"))) stop("Layer does not contain polygon geometry.")
  x <- x |> filter(st_geometry_type(geometry) %in% c("POLYGON", "MULTIPOLYGON"))
  x_wmed <- st_intersection(x, wmed)
  if (nrow(x_wmed) == 0) { warning("No polygons intersect ICES WMed."); return(list(data = x_wmed, area_km2 = 0)) }
  x_aea <- st_transform(x_wmed, projection)
  area_km2 <- sum(st_area(x_aea), na.rm = TRUE) |> as.numeric() / 1e6
  list(data = x_wmed, area_km2 = area_km2)
}


# ============================================================
# 3. POSIDONIA + CYMODOCEA
#
# Both come from the same EMODnet seagrass EOV polygon layer
# (seagrass_wmed), filtered by species-level attributes
# (habsubtype/hab_origin/eunis_name/anxi_name).
# ============================================================

posidonia_wmed <- seagrass_wmed |>
  filter(str_detect(str_to_lower(habsubtype), "posidonia oceanica"))
posidonia_area <- calculate_area_wmed(posidonia_wmed)
cat("\nPosidonia area:", round(posidonia_area$area_km2, 2), "km2\n")

cymodocea_wmed <- seagrass_wmed |>
  filter(str_detect(str_to_lower(paste(habsubtype, hab_origin, eunis_name, anxi_name)), "cymodocea"))
if (nrow(cymodocea_wmed) == 0) {
  warning("No Cymodocea polygons found in seagrass_eov_poly_2025.")
} else {
  cymodocea_area <- calculate_area_wmed(cymodocea_wmed)
  cat("\nCymodocea area:", round(cymodocea_area$area_km2, 2), "km2\n")
}


# ============================================================
# 4. FIND + DOWNLOAD THE MACROALGAE AND CORALLIGENOUS LAYERS
#
# choose_layer() refuses to pick automatically when more than one
# candidate product matches - it prints the candidates and stops.
# ============================================================

sbh_general <- emodnet_get_wfs_info(service = "seabed_habitats_general_datasets_and_products")

macro_candidates <- sbh_general |>
  filter(str_detect(str_to_lower(paste(layer_name, title, abstract)),
                    "macroalgal|macroalgae|macroalgal canopy|brown seaweed"))
coralligenous_candidates <- sbh_general |>
  filter(str_detect(str_to_lower(paste(layer_name, title, abstract)), "coralligenous"))

choose_layer <- function(candidates, preferred_pattern = NULL, name) {
  if (nrow(candidates) == 0) {
    # Print every coral/cnidaria/reef/garden-adjacent product in the
    # catalog so the real product name can be found directly, instead
    # of failing with no information to act on.
    hint <- sbh_general |>
      filter(str_detect(str_to_lower(paste(layer_name, title, abstract)), "coral|cnidaria|reef|garden"))
    if (nrow(hint) > 0) {
      cat("\nNo '", name, "' product matched directly - here is every coral/cnidaria/reef/garden-adjacent",
          " product in the catalog; find the real name below and re-run with that keyword:\n", sep = "")
      print(hint |> select(layer_name, title), width = Inf)
    }
    stop("\nNo ", name, " product found.")
  }
  if (!is.null(preferred_pattern)) {
    preferred <- candidates |> filter(str_detect(str_to_lower(paste(layer_name, title, abstract)), preferred_pattern))
    if (nrow(preferred) == 1) return(preferred$layer_name)
    if (nrow(preferred) > 1) { print(preferred |> select(layer_name, title), width = Inf); stop("\nChoose the appropriate ", name, " layer.") }
  }
  if (nrow(candidates) == 1) return(candidates$layer_name[1])
  print(candidates |> select(layer_name, title), width = Inf)
  stop("\nMore than one ", name, " product found. Do not select automatically.")
}

# Narrows to the polygon variant of a product by filtering directly on
# layer_name's own "_poly" suffix (unambiguous), rather than a
# free-text search across title/abstract (which can match both the
# points and polygon variants, since each product's abstract commonly
# cross-references its sibling).
select_poly_layer <- function(candidates, name) {
  poly_only <- candidates[str_detect(str_to_lower(candidates$layer_name), "_poly"), ]
  choose_layer(poly_only, name = name)
}

macro_layer <- select_poly_layer(macro_candidates, name = "macroalgae")
macro_raw <- download_wmed_wfs(macro_layer)
coralligenous_layer <- select_poly_layer(coralligenous_candidates, name = "coralligenous")
coralligenous_raw <- download_wmed_wfs(coralligenous_layer)

macroalgae_area <- calculate_area_wmed(macro_raw)
coralligenous_area <- calculate_area_wmed(coralligenous_raw)

# Gorgonians/octocorals: no dedicated EMODnet product exists for this
# group (checked against the full seabed-habitats catalog - the
# closest match, "live hard coral cover", is a different taxon). Reuse
# the coralligenous polygon/area as the habitat proxy instead, the same
# convention this pipeline uses elsewhere (gorgonians live within
# coralligenous habitat).
octocoral_layer <- coralligenous_layer
octocoral_area <- coralligenous_area

cat("\nMacroalgae area:", round(macroalgae_area$area_km2, 2), "km2\n")
cat("\nCoralligenous area:", round(coralligenous_area$area_km2, 2), "km2\n")
cat("\nGorgonians/octocorals area (coralligenous proxy - no dedicated product exists):",
    round(octocoral_area$area_km2, 2), "km2\n")


# ============================================================
# 5. HABITAT AREA + OCCUPANCY-FRACTION + DENSITY TABLE
#
# occupancy_fraction is a model assumption (how much of the mapped
# habitat polygon is actually occupied at the stated density), not an
# EMODnet observation - kept explicit here and carried into the output
# CSV's Notes column.
# ============================================================

## Density/occupancy reference values (occupancy_fraction, biomass_g_m2_dw,
## biomass_confidence, biomass_reference, biomass_data_year) are hand-typed
## literature/placeholder figures, externalized to CSV - same convention
## as read_fisheries_reference() in 02_fisheries.R. This script has no
## config block of its own (sourced against objects already built by the
## caller), so fall back to the working directory only if pcloud_dir
## isn't already in scope from whatever sourced this file.
BENTHIC_HABITAT_REFERENCE_DIR <- if (exists("pcloud_dir", inherits = TRUE)) {
  file.path(pcloud_dir, "data/Complementary data/benthic_habitat_reference_tables")
} else {
  file.path(getwd(), "reference_tables")
}
read_benthic_habitat_density_reference <- function(filename = "benthic_habitat_density_reference.csv") {
  path <- file.path(BENTHIC_HABITAT_REFERENCE_DIR, filename)
  if (!file.exists(path)) {
    stop("[Benthic habitat reference] '", path, "' not found - copy it into place under ",
         "BENTHIC_HABITAT_REFERENCE_DIR before re-running.")
  }
  as_tibble(read.csv(path, stringsAsFactors = FALSE))
}

habitat_density_reference <- read_benthic_habitat_density_reference()

habitat_area <- tibble(
  FG = c("Posidonia", "Cymodocea", "Macroalgae", "GorgoniansCorals", "Coralligenous"),
  area_km2 = c(
    posidonia_area$area_km2,
    ifelse(exists("cymodocea_area"), cymodocea_area$area_km2, NA_real_),
    macroalgae_area$area_km2,
    octocoral_area$area_km2,
    coralligenous_area$area_km2
  ),
  spatial_layer = c("seagrass_eov_poly_2025 (Posidonia oceanica subset)",
                    "seagrass_eov_poly_2025 (Cymodocea subset)",
                    macro_layer, paste0(octocoral_layer, " (coralligenous proxy - no dedicated gorgonian/octocoral product exists)"),
                    coralligenous_layer)
) |>
  left_join(habitat_density_reference, by = "FG") |>
  mutate(
    effective_area_km2 = area_km2 * occupancy_fraction
  )

print(habitat_area, width = Inf)


# ============================================================
# 6. WRITE IN benthic_habitat_biomass_literature.csv's SCHEMA
#
# Density_unit = "g_m2_dw" so 01_biomass.R's own DENSITY_UNIT_TO_WET_G_M2
# table applies its dry-to-wet-weight conversion. Biomass_t and the
# final t/km2 are computed downstream by extrapolate_density_row(),
# using the pipeline's own domain area (see method note #2 above), not
# wmed_area_km2 from this script.
# ============================================================

out <- habitat_area |>
  transmute(
    Group = FG,
    Year = YEAR_ECOPATH_TARGET,
    Density_value = biomass_g_m2_dw,
    Density_unit = "g_m2_dw",
    Habitat_area_km2 = effective_area_km2,
    Collection_year = biomass_data_year,
    Source_citation = biomass_reference,
    Confidence = biomass_confidence,
    Notes = paste0(
      "EMODnet-derived mapped-habitat area (", spatial_layer, ") = ", round(area_km2, 2),
      " km2 within the ICES Western Mediterranean ecoregion (a different boundary definition than the",
      " pipeline's own GSA 1-11 MEDITS-strata domain - re-clip to GSA 1-11 if a fully consistent fraction",
      " is needed). occupancy_fraction = ", occupancy_fraction, " (a model assumption about how much of the",
      " mapped habitat polygon is actually occupied at the stated density, not an EMODnet observation)",
      " applied to give effective Habitat_area_km2 = ", round(effective_area_km2, 2), " km2."
    )
  )

write.csv(out, "benthic_habitat_biomass_literature_emodnet_rows.csv", row.names = FALSE)
cat("\nWrote benthic_habitat_biomass_literature_emodnet_rows.csv - review, then merge these rows into",
    " Complementary data/benthic_habitat_biomass_literature.csv.\n")