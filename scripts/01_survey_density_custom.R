## =================================================================
## PIPELINE STEP 1 of 4 (custom / non-GSA region version)
## Alternative to 01_survey_density_westmed.R for a custom bounding-box
## or shapefile study area - run ONE OR THE OTHER, not both, as your
## Step 1. Same downstream role: 02_fao_catches.R and 04_pbqb_calc.R
## both depend on this step's output.
## =================================================================

## =================================================================
## 01_survey_density_custom.R
##
## Same underlying MEDITS survey data and pipeline as
## 01_survey_density_westmed.R, but demonstrates the OTHER way to
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
          "marmap", "raster", "terra", "sf", "openxlsx", "readxl", "maps", "scales",
          "rnaturalearth", "rnaturalearthdata")
new_pkgs <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new_pkgs) > 0) install.packages(new_pkgs)
invisible(lapply(pkgs, library, character.only = TRUE))

## =================================================================
## AREA_NAME - identifies THIS custom study area. Used both in the
## output folder name and appended to every file this script produces
## (instead of the generic word "custom"), so outputs stay self-
## describing even if copied out of their folder, and so a SECOND
## custom-region run (a different area) can't overwrite this one's
## results - each AREA_NAME gets its own subfolder.
## =================================================================
if (tolower(Sys.info()[["user"]]) == "daniel") {
  AREA_NAME <- "custom_region"   # <-- EDIT to name your actual study area, e.g. "cap_de_creus_mpa"
} else {
  if (!requireNamespace("rstudioapi", quietly = TRUE) || !rstudioapi::isAvailable()) {
    stop("This script requires RStudio. Please set AREA_NAME manually above.")
  }
  AREA_NAME <- rstudioapi::showPrompt(
    title = "Custom Study Area Name",
    message = paste("Enter a short name for this custom study area",
                    "(used in the output folder name and every file",
                    "this script produces) - e.g. 'cap_de_creus_mpa':"),
    default = "custom_region"
  )
  if (is.null(AREA_NAME) || AREA_NAME == "") {
    stop("No AREA_NAME entered.")
  }
}

## =================================================================
## STEP 1: Configuration
## Byte-identical to 01_survey_density_westmed.R's own STEP 1 below,
## with ONE necessary deviation: out_dir points at an AREA_NAME
## subfolder rather than the exact same path westmed uses. Making
## out_dir literally identical between the two would reintroduce the
## output-collision bug this script was fixed for earlier - both
## scripts would silently overwrite each other's plots/CSVs/workbook
## sheets. Everything else - pcloud_dir, git_dir, the picker fallback
## wording, the fallback order - matches exactly.
## =================================================================
if (tolower(Sys.info()[["user"]]) == "daniel" && .Platform$OS.type == "unix") {
  out_dir <- file.path("/Users/daniel/Work/iMARES/WMed EwE Model/output", AREA_NAME)
  pcloud_dir   <- "/Users/daniel/pCloud Drive/EwE Western Med 2026/"
  git_dir <-"/Users/daniel/Documents/GitHub/WMed_EwE/"
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
      "Please select the PARENT directory where output files",
      "and intermediate results will be saved - a subfolder named",
      "after AREA_NAME will be created inside it."
    )
  )
  out_dir_parent <- rstudioapi::selectDirectory()
  if (is.null(out_dir_parent) || out_dir_parent == "" || !dir.exists(out_dir_parent)) {
    stop("No valid output directory selected.")
  }
  out_dir <- file.path(out_dir_parent, AREA_NAME)
  
  if (!requireNamespace("rstudioapi", quietly = TRUE) ||
      !rstudioapi::isAvailable()) {
    stop(
      "This script requires RStudio. Please select the pCloud Drive/EwE Western Med 2026 folder."
    )
  }
  rstudioapi::showQuestion(
    title = "Select pCloud EwE West Med Directory",
    message = paste(
      "Please select the location of the the pCloud Drive/EwE Western Med 2026 folder."
    )
  )
  
  pcloud_dir <- rstudioapi::selectDirectory()
  if (is.null(out_dir) || out_dir == "" || !dir.exists(out_dir)) {
    stop("No valid pcloud directory selected.")
  }
  
  if (!requireNamespace("rstudioapi", quietly = TRUE) ||
      !rstudioapi::isAvailable()) {
    stop(
      "This script requires RStudio. Please select the github directory manually."
    )
  }
  rstudioapi::showQuestion(
    title = "Select Github WMed_EwE Directory",
    message = paste(
      "Please select the directory where you cloned the WMed_EwE repository."
    )
  )
  git_dir <- rstudioapi::selectDirectory()
  if (is.null(out_dir) || out_dir == "" || !dir.exists(out_dir)) {
    stop("No valid Github directory selected.")
  }
}

## =================================================================
## (Run-log/sink()-to-file mechanism removed - same reasoning as
## 01_survey_density_westmed.R: it caused real trouble tied to
## sink()/split=TRUE combined with source()-ing a large file and its
## package loads. Plain console output only from here.)
## =================================================================

source(paste0(git_dir, "/scripts/lib_survey_fg_density_functions.R"))
source(paste0(git_dir, "./scripts/lib_worms_taxonomy_lookup.R"))

## Same pcloud_dir-based convention as 01_survey_density_westmed.R -
## previously this script read fg_file/tm_list_file/in_dir from a
## stale "data/raw/..." location under git_dir instead, which is a
## DIFFERENT (and older) location than where the GSA example actually
## reads the same underlying files from now.
fg_file      <- paste0(pcloud_dir, "/data/FG_WMed.xlsx")
tm_list_file <- paste0(pcloud_dir, "/data/Medits_Medias_JRC2026/2024_MEDBSsurvey/TM_list_(April_2019).xlsx")
in_dir       <- paste0(pcloud_dir, "/data/Medits_Medias_JRC2026/2024_MEDBSsurvey/")

## plot_dir is INSIDE out_dir (out_dir/plots), not two directories up -
## this also naturally avoids a real collision an earlier version had:
## with plot_dir = dirname(dirname(out_dir))/plots and a hardcoded
## "custom_region" folder name, a SECOND custom-region run (a
## different AREA_NAME) could still resolve to a plot_dir shared with
## the FIRST one. Now that out_dir itself is AREA_NAME-specific
## (output/<AREA_NAME>/) and plot_dir nests inside it, each area gets
## its own fully distinct plot_dir with no manual workaround needed.
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
plot_dir <- file.path(out_dir, "plots")
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

## af()/pf(): build an output file path with AREA_NAME inserted right
## before the extension - e.g. af("survey_fg_annual_index.csv") ->
## out_dir/survey_fg_annual_index_<AREA_NAME>.csv. Used for every
## file this script writes, so each one is self-describing even if
## copied out of its out_dir/plot_dir folder later.
##
## EXCEPTION - three files stay at their plain, unsuffixed names on
## purpose: species_density_regional_combined.csv, ecopath_ecosim_
## inputs.xlsx, and strata_area_by_area.csv. These are the CONTRACT
## files 02_fao_catches.R and 04_pbqb_calc.R read by that exact fixed
## name from out_dir, regardless of which Step 1 script produced
## them - area-suffixing them here would break that handoff unless
## those two scripts also learned about AREA_NAME, which is out of
## scope for this change. out_dir itself already being an AREA_NAME-
## specific folder is what keeps these three from colliding with
## 01_survey_density_westmed.R's own copies.
area_suffix_name <- function(fname) sub("(\\.[^.]+)$", paste0("_", AREA_NAME, "\\1"), fname)
af <- function(fname) file.path(out_dir, area_suffix_name(fname))
pf <- function(fname) file.path(plot_dir, area_suffix_name(fname))

## Loud and explicit on purpose - if out_dir here ever ends up matching
## 01_survey_density_westmed.R's own out_dir (e.g. both edited by
## hand to the same value after this script was first set up), both
## scripts would silently write to the exact same plot files, and
## whichever ran most recently would overwrite the other's output with
## no error at all - this is exactly the "both maps show the same
## thing" symptom that looks like a plotting bug but is actually an
## output-path collision. Printed clearly here so it's directly
## checkable by eye, and the existing-file check below catches it even
## without reading the console output carefully.
message("This script (custom-region example) will write its outputs to:\n  out_dir  = ", out_dir, "\n  plot_dir = ", plot_dir,
        "\nConfirm this does NOT match 01_survey_density_westmed.R's own out_dir before proceeding.")
existing_plot_files <- list.files(plot_dir, pattern = "\\.png$")
if (length(existing_plot_files) > 0) {
  message("NOTE: plot_dir already contains ", length(existing_plot_files), " .png file(s) from a previous run",
          " (either this same script, or - if out_dir is accidentally shared - the GSA example).",
          " They'll be overwritten below. If any of these came from a DIFFERENT example script,",
          " out_dir needs to be changed to something distinct for this one.")
}

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
ggsave(pf("survey_sample_coverage_map.png"), p_sample_map, width = 10, height = 8, dpi = 150, bg = "white")

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
fwrite(still_unmatched, af("survey_unmatched_for_manual_review.csv"))

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
## caveats about the CSV path being an unverified guess. Aligned to
## 01_survey_density_westmed.R's own path/filename (pcloud_dir-based) -
## this was previously pointing at a different file under git_dir
## entirely, which looks like drift rather than an intentional
## difference between the two examples.
CATCHABILITY_CSV_PATH <- paste0(pcloud_dir, "/data/catchability_factors_ecotrans_medits_2021_spp.csv")
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
  fwrite(flagged_outliers, af("survey_outliers_flagged.csv"))
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

## Written out here as its own file, same as 01_survey_density_westmed.R's
## Step 9g - 04_pbqb_calc.R's lib_build_species_df_from_survey.R needs
## this exact filename (Species/FG/Biomass input for PB/QB estimation)
## regardless of which Step 1 script produced it. This is a single-
## survey table here (no MEDIAS combination in this custom-region
## example, unlike the GSA one), but the output filename and schema
## match exactly so 04_pbqb_calc.R works transparently either way.
fwrite(species_density_regional, file.path(out_dir, "species_density_regional_combined.csv"))
message("Saved species_density_regional_combined.csv (", nrow(species_density_regional),
        " rows) - MEDITS species-level density, all years. This is the file",
        " lib_build_species_df_from_survey.R reads to build 04_pbqb_calc.R's species_df input.")

## =================================================================
## STEP 8: plots
## =================================================================

p_by_area <- plot_fg_timeseries_by_area(
  fg_index, title = "MEDITS trawl survey by FG, custom region", y_lab = "Density (t/km^2)")
ggsave(pf("survey_fg_density_timeseries.png"), p_by_area, width = 14, height = 10, dpi = 150, bg = "white")

p_regional <- plot_fg_timeseries_regional(fg_index_regional,
                                          title = "MEDITS trawl survey by FG, custom region (area-weighted)", y_lab = "Area-weighted density (t/km^2)")
ggsave(pf("survey_fg_density_timeseries_regional.png"), p_regional, width = 14, height = 10, dpi = 150, bg = "white")

if (STRATA) {
  p_profile <- plot_strata_profile(fg_index, MEDITS_STRATA)
  ggsave(pf("survey_fg_depth_strata_profile.png"), p_profile, width = 16, height = 12, dpi = 150, bg = "white")
}

## =================================================================
## STEP 9: Excel export
## =================================================================

fwrite(fg_index, af("survey_fg_annual_index.csv"))
fwrite(fg_index_regional, af("survey_fg_annual_index_regional.csv"))

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

## =================================================================
## STEP 10: Complete FG_spp_Ecopath / FG_lookup sheets.
##
## FG_spp_Ecopath includes EVERY species from the UNION of:
##   (a) dataframe2 (the literal reference catalog, fg_wmed_95), and
##   (b) species_density_regional (species actually observed and
##       matched to an FG this run - including taxa resolved via
##       taxonomy fallback/seed rules in Steps 3-4 that were never
##       individually listed by name in dataframe2 to begin with).
## Taking the union (not dataframe2 alone) matters: an earlier version
## of this step used dataframe2 alone and ended up with FEWER species
## than FG_spp_Ecosim, because fallback-matched taxa are only in (b),
## not (a). Using (a) alone would silently drop them again.
##
## Species with NO observed density in the YEAR_ECOPATH base years
## (either never observed at all, or observed only in other years)
## get Density = 0 and prop_sp_fg = 0 - by request, not NA and not
## dropped, so every FG-master species has a numeric value Ecopath can
## read directly.
## =================================================================
all_species_fg <- unique(species_density_regional[, .(FG_num, FG_name, Species = ScientificName)])
fg_master_species <- unique(dataframe2[, .(FG_num, FG_name, Species = ScientificName)])
full_species_fg <- unique(rbindlist(list(all_species_fg, fg_master_species)), by = "Species")

## --- base-year Density + within-FG proportion, zero-filled -------------
density_in_base_years <- species_density_regional[
  Year %in% YEAR_ECOPATH, .(Density = mean(mean_density, na.rm = TRUE)),
  by = .(FG_num, FG_name, Species = ScientificName)]
FG_spp_Ecopath <- merge(full_species_fg, density_in_base_years,
                        by = c("FG_num", "FG_name", "Species"), all.x = TRUE)

n_zero_density <- FG_spp_Ecopath[is.na(Density), .N]
if (n_zero_density > 0) {
  message(n_zero_density, " of ", nrow(FG_spp_Ecopath), " species have no observed density in the ",
          min(YEAR_ECOPATH), "-", max(YEAR_ECOPATH), " Ecopath base years (never observed, or ",
          "observed only in other years) - Density and prop_sp_fg set to 0 for these.")
}
FG_spp_Ecopath[is.na(Density), Density := 0]

FG_spp_Ecopath[, fg_total_density := sum(Density, na.rm = TRUE), by = FG_num]
FG_spp_Ecopath[, prop_sp_fg := ifelse(fg_total_density > 0, Density / fg_total_density, 0)]
FG_spp_Ecopath[, fg_total_density := NULL]

## --- taxonomy columns (Genus/Family/Order/Class/Phylum) ----------------
if (!is.null(species_taxonomy)) {
  n_before_tax <- nrow(FG_spp_Ecopath)
  FG_spp_Ecopath <- merge(FG_spp_Ecopath, species_taxonomy, by.x = "Species", by.y = "ScientificName", all.x = TRUE)
  n_missing_tax <- FG_spp_Ecopath[is.na(Genus) & is.na(Family) & is.na(Class), .N]
  if (n_missing_tax > 0) {
    message(n_missing_tax, " of ", n_before_tax, " species in FG_spp_Ecopath have no taxonomy match",
            " in species_taxonomy - taxonomy columns left blank for these:")
    print(FG_spp_Ecopath[is.na(Genus) & is.na(Family) & is.na(Class), .(Species)])
  }
} else {
  message("species_taxonomy not available - FG_spp_Ecopath will have no taxonomy columns.")
}

## --- defensive check: every FG_num/FG_name from dataframe2 present? ----
## Should already be guaranteed since full_species_fg is built from
## dataframe2 itself (every FG has at least one catalog species/
## pseudo-species row, e.g. "Detritus"/"Discards") - kept as an
## explicit check rather than a silent assumption.
full_fg_list <- unique(dataframe2[, .(FG_num, FG_name)])
fg_missing <- full_fg_list[!FG_num %in% unique(FG_spp_Ecopath$FG_num)]
if (nrow(fg_missing) > 0) {
  warning(nrow(fg_missing), " FG(s) from dataframe2 have NO species at all (not even a catalog",
          " entry) and are missing from FG_spp_Ecopath - added as a placeholder row: ",
          paste0(fg_missing$FG_num, ": ", fg_missing$FG_name, collapse = "; "))
  FG_spp_Ecopath <- rbindlist(list(FG_spp_Ecopath, fg_missing), fill = TRUE)
  FG_spp_Ecopath[is.na(Density), Density := 0]
  FG_spp_Ecopath[is.na(prop_sp_fg), prop_sp_fg := 0]
}

## --- enforce column order: FG_num, FG_name, Species first -------------
## The taxonomy merge above joins on "Species" (by.x/by.y), which
## data.table puts first in the result - pushing FG_num/FG_name behind
## it. Reset explicitly rather than relying on merge()'s column-order
## side effect, since that's implementation detail, not a guarantee.
setcolorder(FG_spp_Ecopath, c("FG_num", "FG_name", "Species",
                              setdiff(names(FG_spp_Ecopath), c("FG_num", "FG_name", "Species"))))

setorder(FG_spp_Ecopath, FG_num, -prop_sp_fg)

FG_lookup <- unique(dataframe2[, .(FG_num, FG_name)])
setorder(FG_lookup, FG_num)

message("FG_spp_Ecopath: ", nrow(FG_spp_Ecopath), " species total (", n_zero_density,
        " with Density/prop_sp_fg = 0). FG_lookup: ", nrow(FG_lookup), " unique FGs.")

upsert_workbook_sheets(
  sheets = list(
    FG_spp_Ecopath = FG_spp_Ecopath,
    FG_lookup      = FG_lookup
  ),
  out_path = file.path(out_dir, "ecopath_ecosim_inputs.xlsx")
)

## =================================================================
## STEP 11: traits_ewe sheet - species-level life-history/ecology
## traits (Organism, Ecology, Occurrence status, Biomass/Catch
## contribution, IUCN status, Exploitation status, Vulnerability
## index, Mean/Max length, Mean weight, Mean life span), read from
## fg_file's own "traits_ewe" sheet and reconciled against dataframe2
## before being written into ecopath_ecosim_inputs.xlsx.
##
## fg_file's traits_ewe sheet alternates: a "N: Group Name" header row
## (blank index column) followed by one data row per species in that
## FG - the index column on species rows is a running species ID, NOT
## the FG number; the FG number only exists in the header row's text
## above it. This reads that structure directly and fills the FG
## number/name DOWN from each header row onto the species rows below
## it (the same thing a human does visually reading the merged-
## looking layout in Excel), stopping at the next header row.
## =================================================================
fill_down <- function(x) {
  idx <- which(!is.na(x))
  if (length(idx) == 0) return(x)
  rep_idx <- findInterval(seq_along(x), idx)
  out <- x[idx][pmax(rep_idx, 1)]
  out[rep_idx == 0] <- NA
  out
}

traits_raw <- as.data.table(readxl::read_excel(fg_file, sheet = "traits_ewe", col_names = TRUE))
setnames(traits_raw, 1, "row_index")
setnames(traits_raw, "Species", "Species_col")

is_header_row <- is.na(traits_raw$row_index) &
  str_detect(traits_raw$Species_col, "^\\d+:\\s*")
if (sum(is_header_row) == 0) {
  stop("No FG header rows (pattern 'N: Group name') found in traits_ewe - the sheet ",
       "layout may have changed. Check fg_file's traits_ewe sheet by eye before proceeding.")
}

header_num  <- rep(NA_real_, nrow(traits_raw))
header_name <- rep(NA_character_, nrow(traits_raw))
header_num[is_header_row]  <- as.numeric(str_match(traits_raw$Species_col[is_header_row], "^(\\d+):")[, 2])
header_name[is_header_row] <- trimws(sub("^\\d+:\\s*", "", traits_raw$Species_col[is_header_row]))

traits_raw[, FG_num_traits_sheet  := fill_down(header_num)]
traits_raw[, FG_name_traits_sheet := fill_down(header_name)]

## column names cleaned up for downstream use - original header text
## (with its "(?)" unit uncertainty markers) kept in a comment here
## rather than silently asserting units that weren't confirmed:
##   Organism, Ecology, "Occurrence status", "Biomass contribution",
##   "Catch contribution", "IUCN conservation status",
##   "Exploitation status", "Vulnerability index (?)",
##   "Mean length (?)", "Max length (?)", "Mean weight (?)",
##   "Mean life span (year)"
old_trait_names <- c("Organism", "Ecology", "Occurrence status", "Biomass contribution",
                     "Catch contribution", "IUCN conservation status", "Exploitation status",
                     "Vulnerability index (?)", "Mean length (?)", "Max length (?)",
                     "Mean weight (?)", "Mean life span (year)")
new_trait_names <- c("Organism", "Ecology", "Occurrence_status", "Biomass_contribution",
                     "Catch_contribution", "IUCN_conservation_status", "Exploitation_status",
                     "Vulnerability_index", "Mean_length", "Max_length",
                     "Mean_weight", "Mean_lifespan_years")
missing_trait_cols <- setdiff(old_trait_names, names(traits_raw))
if (length(missing_trait_cols) > 0) {
  stop("traits_ewe is missing expected trait column(s): ", paste(missing_trait_cols, collapse = ", "),
       " - the sheet layout may have changed since this step was written.")
}
setnames(traits_raw, old_trait_names, new_trait_names)

species_traits <- traits_raw[!is.na(row_index)]
species_traits[, ScientificName := trimws(Species_col)]
species_traits <- species_traits[, c("ScientificName", "FG_num_traits_sheet", "FG_name_traits_sheet",
                                     new_trait_names), with = FALSE]

dupe_traits_species <- species_traits[, .N, by = ScientificName][N > 1, ScientificName]
if (length(dupe_traits_species) > 0) {
  warning(length(dupe_traits_species), " species appear MORE THAN ONCE in traits_ewe - ",
          "keeping the first occurrence of each, review the sheet for duplicates: ",
          paste(dupe_traits_species, collapse = ", "))
  species_traits <- unique(species_traits, by = "ScientificName")
}

## --- reconcile against dataframe2 (authoritative species/FG master) ----
traits_reconciled <- merge(dataframe2, species_traits, by = "ScientificName", all = TRUE)

not_in_master <- traits_reconciled[is.na(FG_num), ScientificName]
if (length(not_in_master) > 0) {
  warning(length(not_in_master), " species have a traits_ewe row but are NOT in dataframe2/",
          "fg_wmed_95 (FG master) - likely a naming variant (e.g. 'Bivalvia' vs 'Bivalvia sp.') ",
          "rather than a genuinely new species. Kept in the output, flagged in_fg_master = FALSE, ",
          "NOT auto-matched to a master name since guessing wrong here would silently mix two ",
          "different species' trait rows:\n  ", paste(not_in_master, collapse = ", "))
}

missing_traits <- traits_reconciled[!is.na(FG_num) & is.na(FG_num_traits_sheet), ScientificName]
non_living_fgs <- c("Detritus", "Discards")
missing_traits_living <- setdiff(missing_traits, non_living_fgs)
if (length(missing_traits_living) > 0) {
  warning(length(missing_traits_living), " species are in the FG master but have NO traits_ewe ",
          "row (missing_traits = TRUE in the output, not silently dropped): ",
          paste(missing_traits_living, collapse = ", "))
}
if (length(intersect(missing_traits, non_living_fgs)) > 0) {
  message(length(intersect(missing_traits, non_living_fgs)), " non-living FG placeholder(s) (",
          paste(intersect(missing_traits, non_living_fgs), collapse = ", "),
          ") have no traits_ewe row, as expected.")
}

fg_num_mismatch <- traits_reconciled[
  !is.na(FG_num) & !is.na(FG_num_traits_sheet) & FG_num != FG_num_traits_sheet,
  .(ScientificName, FG_num, FG_name, FG_num_traits_sheet, FG_name_traits_sheet)
]
if (nrow(fg_num_mismatch) > 0) {
  warning(nrow(fg_num_mismatch), " species have a DIFFERENT FG_num in traits_ewe than in the FG ",
          "master (dataframe2's FG_num is used in the output) - worth reconciling by hand:")
  print(fg_num_mismatch)
}

traits_reconciled[, in_fg_master := !is.na(FG_num)]
traits_reconciled[, missing_traits := is.na(FG_num_traits_sheet)]
traits_reconciled[, c("FG_num_traits_sheet", "FG_name_traits_sheet") := NULL]
setorder(traits_reconciled, FG_num, ScientificName, na.last = TRUE)

message("traits_ewe reconciled: ", nrow(traits_reconciled), " species total (",
        sum(!traits_reconciled$missing_traits), " with traits, ",
        sum(traits_reconciled$missing_traits), " missing traits).")

upsert_workbook_sheets(
  sheets = list(traits_ewe = traits_reconciled),
  out_path = file.path(out_dir, "ecopath_ecosim_inputs.xlsx")
)

## =================================================================
## STEP 12: fix sheet names/order in ecopath_ecosim_inputs.xlsx.
##
## Safe to run from EVERY script that touches this workbook, in any
## order - finalize_workbook_sheet_order() skips target sheets that
## don't exist yet (e.g. Catches_Ecopath/Catches_Ecosim from
## 02_fao_catches.R, PB_QB from 04_pbqb_calc.R, Ecobase/References from
## wherever those come from, if they haven't run yet) and appends any
## sheet it doesn't recognize rather than dropping it - so whichever
## script runs LAST naturally leaves the workbook in the right order,
## same order-independent design as upsert_workbook_sheets() itself.
## Add this same block to 02_fao_catches.R and 04_pbqb_calc.R too so
## the order stays correct no matter which one actually runs last.
## =================================================================
finalize_workbook_sheet_order(
  out_path = file.path(out_dir, "ecopath_ecosim_inputs.xlsx"),
  rename_map = c(
    FG_lookup  = "FG",
    References = "PB_QB_References_"
  ),
  target_order = c(
    "FG", "Ecopath", "Catches_Ecopath", "FG_spp_Ecopath", "PB_QB", "PB_QB_spp",
    "Ecobase", "PB_QB_References_", "traits_ewe", "Ecosim", "FG_spp_Ecosim",
    "Catches_Ecosim"
  )
)

message("\nDone. Outputs in ", out_dir, " and ", plot_dir)