## =================================================================
## medits_example_custom_region.R
##
## Same underlying MEDITS survey data and pipeline as
## medits_example_using_functions.R, but demonstrates the OTHER way to
## define a study area: a custom boundary (a shapefile for a specific
## EwE model's own footprint, or a simple bounding box) instead of the
## GFCM GSA polygons. Useful when the same survey data needs to support
## a DIFFERENT model with a boundary that doesn't line up with GSA
## lines at all - a specific bay, a sub-region spanning parts of
## several GSAs, or any other custom footprint.
##
## Structurally this is almost identical to the GSA-based example - the
## data loading and FG-matching stages (Steps 1-4) are exactly the
## same MEDITS logic, since it's the same raw data either way. What's
## genuinely different is the AREA definition itself (Step 1b) and,
## downstream, how strata areas get computed (Step 6) - both of which
## can no longer assume a set of named, pre-existing GSA polygons.
## =================================================================

pkgs <- c("readr", "dplyr", "tidyr", "ggplot2", "stringr", "data.table",
          "marmap", "raster", "terra", "sf", "openxlsx", "rnaturalearth", "rnaturalearthdata")
new_pkgs <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new_pkgs) > 0) install.packages(new_pkgs)
invisible(lapply(pkgs, library, character.only = TRUE))

source("/Users/daniel/Documents/GitHub/WMed_EwE/scripts/survey_fg_density_functions.R")
source("/Users/daniel/Documents/GitHub/WMed_EwE/scripts/worms_taxonomy_lookup.R")

## =================================================================
## STEP 1: Configuration
## =================================================================

if (tolower(Sys.info()[["user"]]) == "daniel") {
  setwd("/Users/daniel/Documents/Github/")
  out_dir <- "/Users/daniel/Work/iMARES/WMed EwE Model/output/custom_region/"
} else {
  ## Falls back to an interactive directory picker in RStudio, rather
  ## than just stopping with "set it manually" - so this script works
  ## for anyone, not just the one hardcoded username above.
  if (!requireNamespace("rstudioapi", quietly = TRUE) ||
      !rstudioapi::isAvailable()) {
    stop(
      "This script requires RStudio. Please select the output directory manually."
    )
  }
  rstudioapi::showQuestion(
    title = "Select Output Directory",
    message = paste(
      "Please select the directory where output files",
      "and intermediate results will be saved."
    )
  )
  out_dir <- rstudioapi::selectDirectory()
  if (is.null(out_dir) || out_dir == "" || !dir.exists(out_dir)) {
    stop("No valid output directory selected.")
  }
}
## plot_dir is INSIDE out_dir (out_dir/plots), not two directories up -
## this also naturally avoids a real collision the earlier version had:
## with plot_dir = dirname(dirname(out_dir))/plots, this script's
## out_dir ("data/processed_custom_region/") and the GSA example's
## out_dir ("data/processed/") shared the same grandparent, so both
## resolved to the SAME plot_dir and would silently overwrite each
## other's plot files. Since each example's own out_dir is already
## distinct, plot_dir nested inside each is distinct too, with no
## manual workaround needed.
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
plot_dir <- file.path(out_dir, "plots")
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

## Loud and explicit on purpose - if out_dir here ever ends up matching
## medits_example_using_functions.R's own out_dir (e.g. both edited by
## hand to the same value after this script was first set up), both
## scripts would silently write to the exact same plot files, and
## whichever ran most recently would overwrite the other's output with
## no error at all - this is exactly the "both maps show the same
## thing" symptom that looks like a plotting bug but is actually an
## output-path collision. Printed clearly here so it's directly
## checkable by eye, and the existing-file check below catches it even
## without reading the console output carefully.
message("This script (custom-region example) will write its outputs to:\n  out_dir  = ", out_dir, "\n  plot_dir = ", plot_dir,
        "\nConfirm this does NOT match medits_example_using_functions.R's own out_dir before proceeding.")
existing_plot_files <- list.files(plot_dir, pattern = "\\.png$")
if (length(existing_plot_files) > 0) {
  message("NOTE: plot_dir already contains ", length(existing_plot_files), " .png file(s) from a previous run",
          " (either this same script, or - if out_dir is accidentally shared - the GSA example).",
          " They'll be overwritten below. If any of these came from a DIFFERENT example script,",
          " out_dir needs to be changed to something distinct for this one.")
}

in_dir   <- "/Users/daniel/Documents/GitHub/WMed_EwE/data/raw/2024_MEDBSsurvey/"

fg_file          <- "/Users/daniel/Documents/GitHub/WMed_EwE/data/raw/FG_WMed.xlsx"
tm_list_file     <- "/Users/daniel/Documents/GitHub/WMed_EwE/data/raw/2024_MEDBSsurvey/TM_list_(April_2019).xlsx"

STRATA        <- TRUE
YEAR_ECOPATH  <- 1994:1996
TS_YEARS      <- NULL
DROP_OUTLIERS <- TRUE

MEDITS_STRATA <- data.table(
  stratum_num = 1:5,
  depth_min = c(10, 50, 100, 200, 500),
  depth_max = c(49.9999, 99.9999, 199.9999, 499.9999, 799.9999)
)

## =================================================================
## STEP 1b: Custom area definition - the part that's genuinely
## different from the GSA-based example
## =================================================================
## Pick ONE of the two options below by setting CUSTOM_AREA_TYPE.
##
## Either way, the result needs to be a single-row (or single-feature)
## sf polygon with a numeric/character ID column, matching the same
## "area_shp" shape the shared functions expect elsewhere - just with
## exactly one area instead of GFCM's ~30. AREA_ID_COL is set to "1"
## for everything since there's no sub-division within this custom
## region unless you build one yourself (e.g. by cutting the boundary
## into your own sub-zones and giving each a distinct ID).

CUSTOM_AREA_TYPE <- "bbox"   # "bbox" or "shapefile"

if (CUSTOM_AREA_TYPE == "bbox") {
  ## Option A - a simple rectangular boundary. Replace with your own
  ## region's actual coordinates (decimal degrees).
  CUSTOM_BBOX <- c(xmin = 2, xmax = 8, ymin = 38, ymax = 42)
  ## unname() is essential - CUSTOM_BBOX["xmin"] carries the name
  ## "xmin" with it, so c(xmin = CUSTOM_BBOX["xmin"], ...) would
  ## concatenate into "xmin.xmin" instead of "xmin", which st_bbox()
  ## can't parse correctly (same bug class as the earlier land-basemap
  ## fix in plot_sample_map() - confirmed directly, not hypothetical).
  custom_poly <- st_as_sfc(st_bbox(c(
    xmin = unname(CUSTOM_BBOX["xmin"]), ymin = unname(CUSTOM_BBOX["ymin"]),
    xmax = unname(CUSTOM_BBOX["xmax"]), ymax = unname(CUSTOM_BBOX["ymax"])
  ), crs = 4326))
  area_shp <- st_sf(area_id = 1, geometry = custom_poly)
} else if (CUSTOM_AREA_TYPE == "shapefile") {
  ## Option B - a real shapefile for a specific model's own boundary.
  ## Update this to the actual file location.
  CUSTOM_SHAPEFILE_PATH <- "/path/to/your_model_boundary.shp"
  area_shp <- st_read(CUSTOM_SHAPEFILE_PATH, quiet = TRUE)
  if (is.na(st_crs(area_shp))) {
    stop("CUSTOM_SHAPEFILE_PATH has no CRS defined - set st_crs(area_shp) <- <correct CRS>",
         " before proceeding (check the shapefile's own .prj file or documentation).")
  }
  area_shp <- st_transform(area_shp, 4326)
  ## if the shapefile has multiple features/sub-zones, keep its own ID
  ## column; if it's a single boundary, give it one explicitly
  if (!"area_id" %in% names(area_shp)) area_shp$area_id <- seq_len(nrow(area_shp))
} else {
  stop("CUSTOM_AREA_TYPE must be 'bbox' or 'shapefile'.")
}

AREA_ID_COL <- "area_id"
FILTER_AREAS <- unique(area_shp[[AREA_ID_COL]])   # everything in area_shp, since it's already the custom boundary

## =================================================================
## STEP 2: Input data - MEDITS-specific loading, reshaped into the
## standardized dataframe1/dataframe2 format
## =================================================================
## Identical to the GSA-based example - same raw files, same swept-area
## calculation, same DDMM.mmm/Alboran-sign fixes. The one difference:
## AreaID below is NOT taken from MEDITS' own "area" (GSA) column,
## since the custom boundary doesn't correspond to any GSA code at all
## - it's derived spatially instead, via filter_samples_by_area(), once
## Lat/Lon are available.

fg_raw <- as.data.table(readxl::read_excel(fg_file, sheet = 4))
dataframe2 <- unique(fg_raw[, .(ScientificName = ESPECIE, FG_num = GF, FG_name)])

ta <- read_csv(file.path(in_dir, "Demersal", "TA.csv"), show_col_types = FALSE)

WING_OPENING_UNIT <- "decimetres"

convert_ddmm_to_decimal <- function(x) {
  deg <- floor(x / 100)
  minutes <- x %% 100
  deg + minutes / 60
}
## Alboran-sign fix retained for completeness even though this example's
## default bbox doesn't reach that far west - remove if your own custom
## region is nowhere near GSA 1-3, or keep it since it's a no-op
## elsewhere (only triggers for GSA area codes 1/2/3 specifically).
ALBORAN_GSAS <- c(1, 2, 3)
fix_alboran_longitude_sign <- function(longitude, gsa) {
  ifelse(gsa %in% ALBORAN_GSAS & longitude > 0, -longitude, longitude)
}

ta_swept <- ta %>%
  mutate(
    wing_opening_m = if (WING_OPENING_UNIT == "decimetres") wing_opening / 10 else wing_opening,
    swept_area_km2 = (distance * wing_opening_m) / 1e6,
    mean_depth = (shooting_depth + hauling_depth) / 2,
    SampleID = paste(country, area, vessel, year, haul_number, month, day, sep = "_"),
    shooting_latitude = convert_ddmm_to_decimal(shooting_latitude),
    shooting_longitude = fix_alboran_longitude_sign(convert_ddmm_to_decimal(shooting_longitude), area)
  ) %>%
  dplyr::select(SampleID, swept_area_km2, mean_depth, shooting_latitude, shooting_longitude)

tb <- read_csv(file.path(in_dir, "Demersal", "TB.csv"), show_col_types = FALSE)
tb <- tb %>%
  mutate(
    species_code = paste0(genus, species),
    ptot = ifelse(ptot < 0, NA, ptot),
    SampleID = paste(country, area, vessel, year, haul_number, month, day, sep = "_")
  ) %>%
  as.data.table()

tm_list <- as.data.table(readxl::read_excel(tm_list_file, sheet = 1, skip = 2))
code_col <- grep("MEDITS", names(tm_list), ignore.case = TRUE, value = TRUE)[1]
name_col <- grep("Scientific", names(tm_list), ignore.case = TRUE, value = TRUE)[1]
tm_lookup <- unique(tm_list[, .(species_code = get(code_col), ScientificName = get(name_col))])

survey_observations <- merge(tb, tm_lookup, by = "species_code", all.x = TRUE)
survey_observations <- merge(survey_observations, as.data.table(ta_swept), by = "SampleID", all.x = TRUE)

## reshape into the standardized dataframe1 columns - AreaID left NA
## here (unlike the GSA example) since it's derived spatially just
## below, not taken from MEDITS' own area code
dataframe1 <- survey_observations[, .(
  species_code = species_code,
  ScientificName = ScientificName,
  Biomass = ptot / 1e6,
  Year = year,
  Depth = mean_depth,
  SampleID = SampleID,
  Effort = swept_area_km2,
  Lat = shooting_latitude,
  Lon = shooting_longitude
)]

## AreaID = 1 for every sample, set here (BEFORE the map call and
## BEFORE filtering) rather than after filter_samples_by_area() runs -
## it's just a constant label, not dependent on filtering at all, since
## this whole example only ever has one custom area. plot_sample_map()
## below needs this column to exist for its own internal n_samples
## grouping - calling it on the full, pre-filter dataframe1 without
## AreaID set yet fails with "object 'AreaID' not found", which is
## exactly what setting it here, before that call, avoids.
dataframe1[, AreaID := 1]

message("dataframe1 built (pre-filter): ", nrow(dataframe1), " observations from ",
        uniqueN(dataframe1$SampleID), " samples.")

## Map plotted on the FULL, unfiltered data - BEFORE filter_samples_by_area()
## runs below - so it shows the complete MEDITS distribution across the
## whole survey region, with the custom area's contour highlighted
## within that full context (via selected_areas), rather than zooming
## tightly into just the custom box itself and losing all surrounding
## context. land_on_top=TRUE because CUSTOM_AREA_TYPE="bbox" is a plain
## rectangle that can genuinely overlap land (unlike the GFCM GSA
## polygons, which are official marine-zone boundaries that don't) -
## with land drawn on top, any part of the box that's actually land is
## visibly masked, so only marine area reads as part of the study
## region.
p_sample_map <- plot_sample_map(dataframe1, area_shp, area_id_col = AREA_ID_COL,
                                title = "MEDITS sample coverage - custom region in Mediterranean context",
                                selected_areas = FILTER_AREAS,
                                land_on_top = (CUSTOM_AREA_TYPE == "bbox"))
ggsave(file.path(plot_dir, "survey_sample_coverage_map.png"), p_sample_map, width = 10, height = 8, dpi = 150, bg = "white")

## restrict to the custom region BEFORE validation/downstream steps -
## this is the actual point of this example. Every sample outside the
## custom boundary is dropped here, once, rather than carried through
## the whole pipeline and filtered out later.
dataframe1 <- filter_samples_by_area(
  dataframe1, area_filter = if (CUSTOM_AREA_TYPE == "bbox") CUSTOM_BBOX else area_shp,
  filter_type = CUSTOM_AREA_TYPE
)

validate_survey_data(dataframe1, dataframe2, strata = STRATA)

## =================================================================
## STEP 3: Scientific name -> FG (direct match)
## =================================================================
## Identical to the GSA-based example from here through Step 4 - the
## FG matching logic doesn't depend on how the study area was defined,
## since it's operating species-by-species, not area-by-area.

fg_lookup_safe <- prepare_fg_lookup(dataframe2)
dt <- match_species_to_fg(dataframe1, fg_lookup_safe)
dt <- match_nominate_subspecies_fg(dt, fg_lookup_safe)

MANUAL_OVERRIDES <- data.table(
  species_code = c("ARGRACU", "FMBONEL", "GASTRDA", "ILLESPP", "BUCCSPP", "ASCDCEA", "PTEDGRI", 'SOLEAEG'),
  taxon_name   = c("Argyropelecus aculeatus", "Bonellidae", "Gastropoda", "Illex",
                   "Buccinum", "Ascidiacea", "Pteroeides griseum", 'Solea aegyptiaca'),
  taxon_rank   = c("species", "family", "class", "genus", "genus", "class", "species", 'species')
)

fg_taxonomy <- fetch_taxonomy(fg_lookup_safe$ScientificName, taxonomy_source = "worms")
fg_lookup_safe <- merge(fg_lookup_safe, fg_taxonomy, by = "ScientificName", all.x = TRUE)

resolve_override <- function(taxon_name, rank) {
  rank_col <- switch(rank, species = "ScientificName", genus = "Genus", family = "Family", class = "Class")
  matches <- unique(fg_lookup_safe[get(rank_col) == taxon_name, .(FG_num, FG_name)])
  if (nrow(matches) != 1) return(data.table(FG_num = NA_real_, FG_name = NA_character_))
  matches
}
override_results <- MANUAL_OVERRIDES[, cbind(resolve_override(taxon_name, taxon_rank), taxon_name), by = species_code]
setnames(override_results, "taxon_name", "override_ScientificName")

dt <- merge(dt, override_results[!is.na(FG_num)], by = "species_code", all.x = TRUE, suffixes = c("", "_override"))
dt[!is.na(FG_num_override), `:=`(FG_num = FG_num_override, FG_name = FG_name_override,
                                 ScientificName = fcoalesce(ScientificName, override_ScientificName))]
dt[, c("FG_num_override", "FG_name_override", "override_ScientificName") := NULL]

message("After manual overrides: ", dt[!is.na(FG_num), uniqueN(ScientificName)], " species matched.")

## =================================================================
## STEP 4: Maximize FG assignment via taxonomy fallback
## =================================================================

non_taxon <- str_detect(dt$ScientificName, "^NO\\b") | str_detect(dt$ScientificName, regex("eggs?", ignore_case = TRUE))
dt[non_taxon, ScientificName := NA_character_]
message(sum(non_taxon), " non-taxon row(s) excluded from the taxonomy fallback attempt.")

dt <- fallback_match_fg_by_taxonomy(dt, fg_lookup_safe, taxonomy_source = "worms")

SEED_RULES <- data.table(
  rank = c("Class", "Class", "Class", "Class", "Class", "Class",
           "Order", "Order", "Order", "Order", "Order", "Order",
           "Family", "Family", "Phylum", "Phylum", "Phylum"),
  rank_value = c("Holothuroidea", "Asteroidea", "Echinoidea", "Ophiuroidea", "Bivalvia", "Gastropoda",
                 "Decapoda", "Stomatopoda", "Amphipoda", "Cumacea", "Tanaidacea", "Mysida",
                 "Scorpaenidae", "Sepiolidae", "Bryozoa", "Cnidaria", "Porifera"),
  fg_name_target = c("Sea cucumbers", "Other macro-benthos", "Other sea urchins", "Other macro-benthos",
                     "Bivalves", "Gastropods", "Non-commercial decapods", "Non-commercial decapods",
                     "Suprabenthos", "Suprabenthos", "Suprabenthos", "Suprabenthos",
                     "Scorpaenidae+", "Other benthic cephalopods",
                     "Other macro-benthos", "Other macro-benthos", "Other macro-benthos")
)
SPECIES_EXCEPTIONS <- data.table(ScientificName = "Squilla mantis", fg_name_target = "Other commercial decapods")
dt <- apply_seed_fg_rules(dt, dataframe2, SEED_RULES, species_exceptions = SPECIES_EXCEPTIONS)

taxonomy_context <- attr(dt, "still_unresolved_taxonomy")
still_unmatched <- summarize_unresolved_species(dt, taxonomy = taxonomy_context)
fwrite(still_unmatched, file.path(out_dir, "survey_unmatched_for_manual_review.csv"))

message("\nFinal FG match rate: ", dt[!is.na(FG_num), uniqueN(ScientificName)], " of ",
        dt[, uniqueN(ScientificName)], " distinct species matched.")

species_taxonomy <- unique(rbindlist(list(
  fg_lookup_safe[, .(ScientificName, Genus, Family, Order, Class, Phylum)],
  attr(dt, "fetched_taxonomy")
), fill = TRUE), by = "ScientificName")
species_actually_observed <- unique(dt[!is.na(ScientificName), ScientificName])
still_missing_taxonomy <- setdiff(species_actually_observed, species_taxonomy$ScientificName)
if (length(still_missing_taxonomy) > 0) {
  gap_taxonomy <- fetch_taxonomy(still_missing_taxonomy, taxonomy_source = "worms")
  species_taxonomy <- unique(rbindlist(list(species_taxonomy, gap_taxonomy), fill = TRUE), by = "ScientificName")
}

## =================================================================
## STEP 5: densities (biomass/effort), mean/sum across species within
## FG, by year/strata/area
## =================================================================
## match_samples_to_area() is NOT a no-op here, unlike the GSA example
## - dt already has AreaID=1 for everything (set in Step 2), so this
## call will just confirm that and skip the spatial join, but it's
## worth keeping in the pipeline for consistency/robustness rather than
## assuming.

dt <- match_samples_to_area(dt, area_shp, AREA_ID_COL)
dt <- assign_depth_stratum(dt, MEDITS_STRATA)
dt <- compute_sample_densities(dt)

## catchability correction - same template as the GSA example, same
## caveats about the CSV path being an unverified guess
CATCHABILITY_CSV_PATH <- "/Users/daniel/Documents/GitHub/WMed_EwE/data/raw/Elena_EDelta_MEDI.csv"
EXEMPT_FG_NAMES <- c(
  # e.g. "Small pelagic fish", "Gelatinous plankton", "Seagrass" - REPLACE with your real FG_name values
)
if (file.exists(CATCHABILITY_CSV_PATH)) {
  catchability_raw <- fread(CATCHABILITY_CSV_PATH)
  CATCHABILITY_TABLE <- catchability_raw[, .(ScientificName = species, q = q_FACTOR)]
  dt <- apply_catchability_correction(dt, CATCHABILITY_TABLE, default_q = 1, species_taxonomy = species_taxonomy,
                                      exempt_fg_names = EXEMPT_FG_NAMES)
} else {
  message("CATCHABILITY_CSV_PATH not found - skipping catchability correction.")
}

dt <- remove_sample_outliers(dt, threshold = 70, min_samples = 5, drop_outliers = DROP_OUTLIERS)
flagged_outliers <- attr(dt, "flagged_outliers")
if (!is.null(flagged_outliers) && nrow(flagged_outliers) > 0) {
  fwrite(flagged_outliers, file.path(out_dir, "survey_outliers_flagged.csv"))
}

per_group_fg <- compute_fg_densities_by_stratum(dt, strata = STRATA)
n_samples_by_stratum <- dt[!is.na(Stratum), .(n_samples = uniqueN(SampleID)), by = .(AreaID, Year, Stratum)]
n_samples_by_area <- dt[, .(n_samples = uniqueN(SampleID)), by = .(AreaID, Year)]

## =================================================================
## STEP 6: weight densities per stratum -> FG, year, area
## =================================================================
## This is the step that's genuinely different from the GSA example -
## strata areas are computed for the single custom polygon (AreaID=1),
## not a set of ~30 named GSAs. compute_strata_area_by_area() itself
## doesn't need to know or care that this is a custom boundary rather
## than an official GSA - it just clips bathymetry to whatever polygon
## it's given and sums cell areas by depth stratum, so this works
## identically either way.

strata_area_by_area <- compute_strata_area_by_area(
  area_ids = 1, area_shp = area_shp, area_id_col = AREA_ID_COL, strata_def = MEDITS_STRATA,
  cache_path = file.path(out_dir, "strata_area_by_area.csv")
)

fg_index <- if (STRATA) {
  weight_by_strata(per_group_fg, n_samples_by_stratum, strata_area_by_area)
} else {
  simple_area_density(per_group_fg, n_samples_by_area)
}

## =================================================================
## STEP 7: weight densities per area -> FG, year (region-wide)
## =================================================================
## With only one "area" in this example, this step is a formality -
## weight_by_area() still runs the same area-weighted formula, it just
## has a single area's worth of (stratum, area_km2) cells to sum over,
## so the result is mathematically identical to the per-area fg_index
## above. Kept in the pipeline anyway so this script mirrors the same
## Excel export structure as the GSA example - if you later split this
## custom region into your own sub-zones (multiple AreaID values),
## this step becomes meaningful again without any other code changing.

fg_index_regional <- weight_by_area(fg_index)

per_group_sp <- compute_species_densities_by_stratum(dt, strata = STRATA)
species_density_regional <- weight_species_by_area(per_group_sp, n_samples_by_stratum, strata_area_by_area)

## =================================================================
## STEP 8: plots
## =================================================================

p_by_area <- plot_fg_timeseries_by_area(
  fg_index, title = "MEDITS trawl survey by FG, custom region", y_lab = "Density (t/km^2)")
ggsave(file.path(plot_dir, "survey_fg_density_timeseries.png"), p_by_area, width = 14, height = 10, dpi = 150, bg = "white")

p_regional <- plot_fg_timeseries_regional(fg_index_regional,
                                          title = "MEDITS trawl survey by FG, custom region (area-weighted)", y_lab = "Area-weighted density (t/km^2)")
ggsave(file.path(plot_dir, "survey_fg_density_timeseries_regional.png"), p_regional, width = 14, height = 10, dpi = 150, bg = "white")

if (STRATA) {
  p_profile <- plot_strata_profile(fg_index, MEDITS_STRATA)
  ggsave(file.path(plot_dir, "survey_fg_depth_strata_profile.png"), p_profile, width = 16, height = 12, dpi = 150, bg = "white")
}

## =================================================================
## STEP 9: Excel export
## =================================================================

fwrite(fg_index, file.path(out_dir, "survey_fg_annual_index.csv"))
fwrite(fg_index_regional, file.path(out_dir, "survey_fg_annual_index_regional.csv"))

export_ecopath_ecosim_excel(
  fg_index_regional = fg_index_regional,
  species_density_regional = species_density_regional,
  n_samples_by_area_year = n_samples_by_stratum[, .(n_samples = sum(n_samples)), by = .(AreaID, Year)],
  dataframe2 = dataframe2,
  year_ecopath = YEAR_ECOPATH,
  ts_years = TS_YEARS,
  out_path = file.path(out_dir, "ecopath_ecosim_inputs.xlsx"),
  species_taxonomy = species_taxonomy
)

message("\nDone. Outputs in ", out_dir, " and ", plot_dir)