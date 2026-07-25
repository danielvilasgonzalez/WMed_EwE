## =================================================================
## medits_example_using_functions.R
##
## MEDITS-specific example calling into survey_fg_density_functions.R.
## This script's job is narrow: (1) read MEDITS' own raw file formats
## (TA.csv/TB.csv), (2) compute MEDITS-specific things the shared
## library can't know about (swept area from distance x wing opening,
## MEDITS species-code -> scientific-name lookup, MEDITS' own manual FG
## overrides), (3) reshape into the standardized dataframe1/dataframe2
## format documented at the top of survey_fg_density_functions.R, then
## (4) call the shared functions for everything genuinely generic
## (FG fallback matching, strata weighting, area weighting, plots,
## Excel export).
##
## Produces the same outputs as medbs_pipeline_current.R - this is a
## refactor of that script's logic into reusable form, not a new
## calculation.
## =================================================================

pkgs <- c("readr", "dplyr", "tidyr", "ggplot2", "stringr", "data.table",
          "marmap", "raster", "terra", "sf", "openxlsx")
new_pkgs <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new_pkgs) > 0) install.packages(new_pkgs)
invisible(lapply(pkgs, library, character.only = TRUE))

source("/Users/andreaobradors/Documents/GitHub/WMed_EwE/scripts/survey_fg_density_functions.R")
source("/Users/andreaobradors/Documents/GitHub/WMed_EwE/scripts/worms_taxonomy_lookup.R")

## =================================================================
## STEP 1: Configuration
## =================================================================

if (tolower(Sys.info()[["user"]]) == "daniel") {
  setwd("/Users/daniel/Documents/Github/")
} else {
  stop("Set working directory manually, or add your username to the branch above.")
}

in_dir   <- "/Users/andreaobradors/Documents/GitHub/WMed_EwE/data/raw/2024_MEDBSsurvey/"
out_dir  <- "/Users/andreaobradors/Documents/daniel/WMed_EwE/output/"
plot_dir <- paste0(out_dir, '/plots')
if (!dir.exists(plot_dir)) dir.create(plot_dir)

fg_file          <- "/Users/andreaobradors/Documents/GitHub/WMed_EwE/data/raw/FG_WMed.xlsx"
tm_list_file     <- "/Users/andreaobradors/Documents/GitHub/WMed_EwE/data/raw/2024_MEDBSsurvey/TM_list_(April_2019).xlsx"

FILTER_AREAS  <- 1:11
STRATA        <- TRUE                       # function attribute: strata
YEAR_ECOPATH  <- 1994:1996                   # function attribute: year_ecopath
TS_YEARS      <- NULL                        # function attribute: ts_years (NULL -> min:max of data)
DROP_OUTLIERS <- TRUE                        # function attribute: whether remove_sample_outliers() actually
# removes flagged observations (TRUE) or only reports them (FALSE)

## MEDITS' 5 standard bathymetric strata - passed as strata_def to the
## shared functions, not hardcoded inside them
MEDITS_STRATA <- data.table(
  stratum_num = 1:5,
  depth_min = c(10, 50, 100, 200, 500),
  depth_max = c(49.9999, 99.9999, 199.9999, 499.9999, 799.9999)
)

## GFCM GSA shapefile - this is the "area_shp" function attribute
gsa_zip_url  <- "https://gfcmsitestorage.blob.core.windows.net/website/5.Data/ArcGIS/GFCM_GSA.zip"
gsa_zip_file <- paste0(dirname(out_dir), "/GFCM_GSA.zip")
gsa_shp_dir  <- paste0(dirname(out_dir), "/GFCM_GSA_shp")
if (!dir.exists(gsa_shp_dir)) {
  if (!file.exists(gsa_zip_file)) {
    dir.create(dirname(gsa_zip_file), recursive = TRUE, showWarnings = FALSE)
    download.file(gsa_zip_url, destfile = gsa_zip_file, mode = "wb", method = "libcurl")
  }
  unzip(gsa_zip_file, exdir = gsa_shp_dir)
}
gsa_shp_path <- list.files(gsa_shp_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)[1]
area_shp <- st_read(gsa_shp_path, quiet = TRUE)
area_shp$gsa_num <- as.numeric(area_shp$SMU_CODE)
area_shp$gsa_num[area_shp$gsa_num %in% c(111, 112)] <- 11   # W/E Sardinia fix, same as the map script
AREA_ID_COL <- "gsa_num"

## =================================================================
## STEP 2: Input data - MEDITS-specific loading, reshaped into the
## standardized dataframe1/dataframe2 format
## =================================================================

## --- dataframe2: species -> FG reference ------------------------------------
fg_raw <- as.data.table(readxl::read_excel(fg_file, sheet = 4))
dataframe2 <- unique(fg_raw[, .(ScientificName = ESPECIE, FG_num = GF, FG_name)])

## --- dataframe1: MEDITS' TA.csv (samples/hauls) + TB.csv (catch) -----------
ta <- read_csv(file.path(in_dir, "Demersal", "TA.csv"), show_col_types = FALSE)

## swept area (the "Effort" column) - MEDITS-specific: distance towed x
## wing opening, unit confirmed via cross-check against vertical_opening
## (see original pipeline for the full unit-verification reasoning)
WING_OPENING_UNIT <- "decimetres"

## shooting_latitude/shooting_longitude are in DDMM.mmm format (degrees
## and decimal minutes concatenated, e.g. 4021 means 40 deg 21.0 min =
## 40.35 decimal degrees), NOT plain decimal degrees - confirmed
## directly: converting the actual reported min/median/max via this
## formula landed every value inside area_shp's own Mediterranean
## bounding box (lat 30.07-47.28, lon -5.35-41.78), which is exactly
## what should happen if this is right. Using raw DDMM.mmm values
## directly (as an earlier version of this script did) is what caused
## 99.6% of points to plot outside every GSA polygon - not a CRS/
## projection issue, this was a units problem the whole time.
convert_ddmm_to_decimal <- function(x) {
  deg <- floor(x / 100)
  minutes <- x %% 100
  deg + minutes / 60
}

## Longitude sign fix for GSAs 1-3 (Alboran Sea) - confirmed via
## area_shp's own data: GSA 1/2/3 (Northern Alboran Sea, Alboran
## Island, Southern Alboran Sea) have CENTER_X of -2.97/-3.00/-3.83
## respectively - all clearly negative/western - while
## convert_ddmm_to_decimal() always returns a positive value regardless
## of true hemisphere (this was the flagged-but-unresolved
## shooting_quadrant caveat from before: that field likely encodes
## hemisphere separately from the magnitude, and wasn't being used).
## For these 3 GSAs specifically, a positive converted longitude is
## used as ground-truth evidence that the sign needs flipping, rather
## than trying to fully decode the quadrant field's exact convention
## (uncertain - values up to 7 were seen, more than a standard 4-way
## N/S-E/W code would need). GSAs 4+ are all confirmed positive-
## longitude in area_shp and are left untouched.
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
  dplyr::select(SampleID, swept_area_km2, mean_depth,
                shooting_latitude, shooting_longitude)

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

## reshape into the standardized dataframe1 columns
## species_code is carried through as an extra column (the generalized
## functions only use the columns they need, extra ones are harmless) -
## needed below for the manual-override merge, since ScientificName is
## exactly the column that's NA for override-needed rows (that's WHY
## they need a manual override - the MEDITS list lookup already failed
## for them), so it can't be the merge key for applying overrides.
dataframe1 <- survey_observations[, .(
  species_code = species_code,
  ScientificName = ScientificName,
  Biomass = ptot / 1e6,             # ptot native unit = grams -> tonnes
  Year = year,
  AreaID = as.numeric(area),        # MEDITS already has GSA directly - Lat/Lon not needed for the spatial join,
  # only carried through for plot_sample_map() below
  Depth = mean_depth,
  SampleID = SampleID,
  Effort = swept_area_km2,
  Lat = shooting_latitude,
  Lon = shooting_longitude
)]

message("dataframe1 built: ", nrow(dataframe1), " observations from ",
        uniqueN(dataframe1$SampleID), " samples.")

validate_survey_data(dataframe1, dataframe2, strata = STRATA)

## Sample coverage diagnostic - worth looking at before the rest of the
## pipeline, e.g. to spot a gap in survey coverage or samples plotting
## outside their expected area boundary.
p_sample_map <- plot_sample_map(dataframe1, area_shp, area_id_col = AREA_ID_COL,
                                title = "MEDITS sample coverage by GSA")
ggsave(file.path(plot_dir, "survey_sample_coverage_map.png"), p_sample_map, width = 10, height = 8, dpi = 150)

## same coverage map, faceted by year - useful for spotting a specific
## year with a coverage gap that the all-years-combined map above would
## mask (a gap in one year can look fine once every other year's
## samples are overlaid on top of it)
p_sample_map_byyear <- plot_sample_map(dataframe1, area_shp, area_id_col = AREA_ID_COL,
                                       title = "MEDITS sample coverage by GSA and year", by_year = TRUE)
ggsave(file.path(plot_dir, "survey_sample_coverage_map_by_year.png"), p_sample_map_byyear,
       width = 16, height = 12, dpi = 150)

## =================================================================
## STEP 3: Scientific name -> FG (direct match)
## =================================================================

fg_lookup_safe <- prepare_fg_lookup(dataframe2)
dt <- match_species_to_fg(dataframe1, fg_lookup_safe)
dt <- match_nominate_subspecies_fg(dt, fg_lookup_safe)

## --- MEDITS-specific manual overrides ---------------------------------------
## Species codes not in the MEDITS taxonomic list at all - this table
## is genuinely specific to this survey's code list and this FG
## reference scheme, so it stays here rather than in the shared library.
MANUAL_OVERRIDES <- data.table(
  species_code = c("ARGRACU", "FMBONEL", "GASTRDA", "ILLESPP", "BUCCSPP", "ASCDCEA", "PTEDGRI",'SOLEAEG'),
  taxon_name   = c("Argyropelecus aculeatus", "Bonellidae", "Gastropoda", "Illex",
                   "Buccinum", "Ascidiacea", "Pteroeides griseum", 'Solea aegyptiaca'),
  taxon_rank   = c("species", "family", "class", "genus", "genus", "class", "species",'species')
)

## needs taxonomy on fg_lookup_safe to resolve genus/family/class-rank overrides
fg_taxonomy <- fetch_taxonomy(fg_lookup_safe$ScientificName, taxonomy_source = "worms")
fg_lookup_safe <- merge(fg_lookup_safe, fg_taxonomy, by = "ScientificName", all.x = TRUE)

resolve_override <- function(taxon_name, rank) {
  rank_col <- switch(rank, species = "ScientificName", genus = "Genus", family = "Family", class = "Class")
  matches <- unique(fg_lookup_safe[get(rank_col) == taxon_name, .(FG_num, FG_name)])
  if (nrow(matches) != 1) return(data.table(FG_num = NA_real_, FG_name = NA_character_))
  matches
}
## keep taxon_name alongside the resolved FG - needed to fill in
## ScientificName on dt for these rows, since dt's own ScientificName
## is NA for exactly these species_codes
override_results <- MANUAL_OVERRIDES[, cbind(resolve_override(taxon_name, taxon_rank), taxon_name), by = species_code]
setnames(override_results, "taxon_name", "override_ScientificName")

## merge on species_code - the reliable key that's present regardless
## of whether ScientificName resolved. Merging on ScientificName here
## (as an earlier version of this script did) matches every NA row in
## dt against every NA row in the override table simultaneously,
## producing a cartesian-product explosion - this is what caused the
## "Join results in 931710 rows" error.
dt <- merge(dt, override_results[!is.na(FG_num)], by = "species_code", all.x = TRUE, suffixes = c("", "_override"))
dt[!is.na(FG_num_override), `:=`(FG_num = FG_num_override, FG_name = FG_name_override,
                                 ScientificName = fcoalesce(ScientificName, override_ScientificName))]
dt[, c("FG_num_override", "FG_name_override", "override_ScientificName") := NULL]

message("After manual overrides: ", dt[!is.na(FG_num), uniqueN(ScientificName)], " species matched.")



## =================================================================
## STEP 4: Maximize FG assignment via taxonomy fallback
## =================================================================
## fallback_match_fg_by_taxonomy() now walks Genus -> Family -> Order
## -> Class -> Phylum automatically, using whichever level is most
## specific and still maps exclusively to one FG among fg_lookup_safe's
## own species - this replaces the old hardcoded CLASS_RULES/
## ORDER_RULES/PHYLUM_RULES/FAMILY_RULES tables entirely. Those rules
## were manually re-deriving exactly this "exclusive mapping" logic by
## hand for broader ranks; now it's the same data-driven rule at every
## rank, so a change to the FG scheme doesn't need a matching manual
## update here - it's picked up automatically on the next run.

## exclude non-taxon entries before the fallback attempt - "NO ..."
## (an upstream column-concatenation artifact) and egg-capsule entries
## aren't real taxa and would just waste a WoRMS query for nothing;
## setting ScientificName to NA here only affects the fallback match
## attempt, not the underlying observation rows themselves
non_taxon <- str_detect(dt$ScientificName, "^NO\\b") | str_detect(dt$ScientificName, regex("eggs?", ignore_case = TRUE))
dt[non_taxon, ScientificName := NA_character_]
message(sum(non_taxon), " non-taxon row(s) (NO-prefixed or egg-capsule entries) excluded",
        " from the taxonomy fallback attempt.")

dt <- fallback_match_fg_by_taxonomy(dt, fg_lookup_safe, taxonomy_source = "worms")

## --- Seed FG rules -----------------------------------------------------
## For FGs that genuinely have ZERO species pre-listed in dataframe2 -
## there's nothing for the automatic, data-driven fallback above to
## learn an exclusive mapping from, since it can only discover a
## Class/Order/Phylum -> FG relationship from species that are ALREADY
## assigned to that FG. Confirmed this is the actual reason before
## adding entries here, not just assumed - check with:
##   dataframe2[FG_name %in% c("Sea cucumbers", "Other macro-benthos",
##                              "Non-commercial decapods"), .N, by = FG_name]
## If any of these come back with a nonzero species count, the
## automatic fallback SHOULD already be resolving that one - worth
## checking for an ambiguous rank elsewhere in the reference instead of
## assuming a seed rule is needed.
##
## Deliberately small - only the FGs actually confirmed empty, not a
## full re-statement of the old CLASS_RULES/ORDER_RULES/PHYLUM_RULES
## tables. Resolved by FG_name text, so this survives FG_num changing.
SEED_RULES <- data.table(
  rank = c(
    "Class", "Class", "Class", "Class",
    "Class", "Class",
    "Order", "Order", "Order", "Order", "Order", "Order",
    "Family", "Family",
    "Phylum", "Phylum", "Phylum"
  ),
  rank_value = c(
    "Holothuroidea",
    "Asteroidea",
    "Echinoidea",
    "Ophiuroidea",
    "Bivalvia",
    "Gastropoda",
    "Decapoda",
    "Stomatopoda",
    "Amphipoda",
    "Cumacea",
    "Tanaidacea",
    "Mysida",
    "Scorpaenidae",
    "Sepiolidae",
    "Bryozoa",
    "Cnidaria",
    "Porifera"
  ),
  fg_name_target = c(
    "Sea cucumbers",
    "Other macro-benthos",
    "Other sea urchins",
    "Other macro-benthos",
    "Bivalves",
    "Gastropods",
    "Non-commercial decapods",
    "Non-commercial decapods",
    "Suprabenthos",
    "Suprabenthos",
    "Suprabenthos",
    "Suprabenthos",
    "Scorpaenidae+",
    "Other benthic cephalopods",
    "Other macro-benthos",
    "Other macro-benthos",
    "Other macro-benthos"
  )
)

## Species-level exceptions - applied BEFORE the rank rules above, so a
## named species is never caught by a broader rule that's wrong for it
## specifically. Squilla mantis is commercially exploited even though
## the rest of Stomatopoda isn't, so the "Order Stomatopoda ->
## Non-commercial decapods" rule above would misclassify it without
## this override.
SPECIES_EXCEPTIONS <- data.table(
  ScientificName = "Squilla mantis",
  fg_name_target = "Other commercial decapods"
)

dt <- apply_seed_fg_rules(dt, dataframe2, SEED_RULES, species_exceptions = SPECIES_EXCEPTIONS)

## still_unresolved_taxonomy (set inside fallback_match_fg_by_taxonomy(),
## before the seed rules above ran) has Genus/Family/Order/Class/
## Phylum already attached - passed to summarize_unresolved_species()
## for context in the manual-review export. summarize_unresolved_species()
## itself computes mean biomass and appearance count per species (using
## dt's own, up-to-date FG_num, so species resolved by the seed rules
## just above correctly don't show up as "still needs review") and
## prints the result sorted by appearance count, most first - so a
## species showing up in hundreds of samples with real biomass is
## immediately visible ahead of a one-off trace appearance.
taxonomy_context <- attr(dt, "still_unresolved_taxonomy")
still_unmatched <- summarize_unresolved_species(dt, taxonomy = taxonomy_context)
fwrite(still_unmatched, file.path(out_dir, "survey_unmatched_for_manual_review.csv"))
message("Saved to survey_unmatched_for_manual_review.csv for review.")

message("\nFinal FG match rate: ", dt[!is.na(FG_num), uniqueN(ScientificName)], " of ",
        dt[, uniqueN(ScientificName)], " distinct species matched.")


## =================================================================
## STEP 5: densities (biomass/effort), mean/sum across species within
## FG, by year/strata/area
## =================================================================

dt <- match_samples_to_area(dt, area_shp, AREA_ID_COL)   # no-op here, MEDITS already has AreaID
dt <- assign_depth_stratum(dt, MEDITS_STRATA)
dt <- compute_sample_densities(dt)
dt <- dt[AreaID %in% FILTER_AREAS]

## Removes (not just flags) individual sample-level observations that
## are extreme outliers compared to that SAME species' own history in
## that SAME area - e.g. a single haul with an implausible spike for
## one species. Runs here, before any aggregation, specifically so a
## single bad sample can't inflate an entire year's FG-level density -
## only that one species' one observation is affected, not the whole
## FG or the whole year for other species sharing it. Uses a high
## (threshold=5) bar since this is an automatic exclusion, not just a
## flag for review - only fires with real confidence.
dt <- remove_sample_outliers(dt, threshold = 70, min_samples = 5, drop_outliers = DROP_OUTLIERS)

## save right here, before dt moves on to compute_fg_densities_by_stratum()
## etc. below - those functions weren't designed to know about or
## preserve the flagged_outliers attribute, so it needs to be pulled
## off now rather than later.
flagged_outliers <- attr(dt, "flagged_outliers")
if (!is.null(flagged_outliers) && nrow(flagged_outliers) > 0) {
  fwrite(flagged_outliers, file.path(out_dir, "survey_outliers_flagged.csv"))
  message("Saved ", nrow(flagged_outliers), " flagged outlier(s) to survey_outliers_flagged.csv",
          " (includes a 'was_dropped' column - TRUE if actually removed, FALSE if only reported).")
}

per_group_fg <- compute_fg_densities_by_stratum(dt, strata = STRATA)

## total sample count, computed from the FULL dt (every species/FG
## together) - NOT from a species/FG-filtered subset, which would only
## count samples where that specific group had non-zero catch and
## understate the true sampling effort. n_samples_by_stratum is used
## by the STRATA=TRUE path; n_samples_by_area (same idea, no Stratum
## dimension) is used by the STRATA=FALSE path. Unaffected by
## remove_sample_outliers() above - that only nulls out Density/Biomass
## for specific species' observations, the sample/haul itself still
## happened and still counts toward total sampling effort.
n_samples_by_stratum <- dt[
  AreaID %in% FILTER_AREAS & !is.na(Stratum),
  .(n_samples = uniqueN(SampleID)), by = .(AreaID, Year, Stratum)]
n_samples_by_area <- dt[
  AreaID %in% FILTER_AREAS,
  .(n_samples = uniqueN(SampleID)), by = .(AreaID, Year)]

## =================================================================
## STEP 6: weight densities per stratum -> FG, year, area
## =================================================================

strata_area_by_area <- compute_strata_area_by_area(
  area_ids = sort(unique(dt$AreaID[dt$AreaID %in% FILTER_AREAS])),
  area_shp = area_shp, area_id_col = AREA_ID_COL, strata_def = MEDITS_STRATA,
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

fg_index_regional <- weight_by_area(fg_index)

## species-level regional density (for the FG_spp Excel sheet)
per_group_sp <- compute_species_densities_by_stratum(dt, strata = STRATA)
species_density_regional <- weight_species_by_area(per_group_sp, n_samples_by_stratum, strata_area_by_area)

## =================================================================
## STEP 8: plots
## =================================================================

p_by_area <- plot_fg_timeseries_by_area(
  fg_index, title = "MEDITS trawl survey by FG and area (strata-weighted)", y_lab = "Density (t/km^2)")
ggsave(file.path(plot_dir, "survey_fg_density_timeseries.png"), p_by_area, width = 14, height = 10, dpi = 150)

p_regional <- plot_fg_timeseries_regional(fg_index_regional,
                                          title = "MEDITS trawl survey by FG, Western Med (area-weighted)", y_lab = "Area-weighted density (t/km^2)")
ggsave(file.path(plot_dir, "survey_fg_density_timeseries_regional.png"), p_regional, width = 14, height = 10, dpi = 150)

if (STRATA) {
  p_profile <- plot_strata_profile(fg_index, MEDITS_STRATA)
  ggsave(file.path(plot_dir, "survey_fg_depth_strata_profile.png"), p_profile, width = 16, height = 12, dpi = 150)
}

## =================================================================
## STEP 9: Excel export
## =================================================================

fwrite(fg_index, file.path(out_dir, "survey_fg_annual_index.csv"))
fwrite(fg_index_regional, file.path(out_dir, "survey_fg_annual_index_regional.csv"))

## species_taxonomy for the FG_spp sheet - starts from fg_lookup_safe's
## own taxonomy (reference species) plus whatever fallback_match_fg_by_
## taxonomy() fetched (species needing genus/family/etc. fallback), then
## explicitly checks for and fetches ANY remaining gap. This matters
## because manually-overridden species (MANUAL_OVERRIDES, Step 3) never
## go through fallback_match_fg_by_taxonomy() at all - they're already
## matched by the time that function runs, so their taxonomy was never
## fetched via that path, and neither fg_lookup_safe nor
## fetched_taxonomy would have them. Explicitly closing that gap here
## rather than leaving it to be silently blank.
species_taxonomy <- unique(rbindlist(list(
  fg_lookup_safe[, .(ScientificName, Genus, Family, Order, Class, Phylum)],
  attr(dt, "fetched_taxonomy")
), fill = TRUE), by = "ScientificName")

species_actually_used <- unique(species_density_regional$ScientificName)
still_missing_taxonomy <- setdiff(species_actually_used, species_taxonomy$ScientificName)
if (length(still_missing_taxonomy) > 0) {
  message("\n", length(still_missing_taxonomy), " species in species_density_regional have no",
          " taxonomy yet (likely matched via MANUAL_OVERRIDES, which doesn't fetch taxonomy) -",
          " fetching directly for these:")
  gap_taxonomy <- fetch_taxonomy(still_missing_taxonomy, taxonomy_source = "worms")
  species_taxonomy <- rbindlist(list(species_taxonomy, gap_taxonomy), fill = TRUE)
  species_taxonomy <- unique(species_taxonomy, by = "ScientificName")
}

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

