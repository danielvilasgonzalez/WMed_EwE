## =================================================================
## PIPELINE STEP 1 of 4 (Western Med GSA version)
## Run FIRST - no dependencies on the other numbered scripts.
## Produces: species_density_regional_combined.csv, strata_area_by_area.csv,
## and the Biomass sheets (FG_spp_Ecopath/Ecopath/Ecosim/FG_spp_Ecosim)
## in output/ecopath_ecosim_inputs.xlsx.
## 02_fao_catches.R and 04_pbqb_calc.R both REQUIRE this to have run
## first (strata_area_by_area.csv and species_density_regional_combined.csv
## respectively don't exist until this does).
## Use 01_survey_density_custom.R INSTEAD of this one for a custom
## (non-GSA) study area - run one or the other, not both.
## =================================================================

## =================================================================
## 01_survey_density_westmed.R
##
## MEDITS-specific example calling into lib_survey_fg_density_functions.R.
## This script's job is narrow: (1) read MEDITS' own raw file formats
## (TA.csv/TB.csv), (2) compute MEDITS-specific things the shared
## library can't know about (swept area from distance x wing opening,
## MEDITS species-code -> scientific-name lookup, MEDITS' own manual FG
## overrides), (3) reshape into the standardized dataframe1/dataframe2
## format documented at the top of lib_survey_fg_density_functions.R, then
## (4) call the shared functions for everything genuinely generic
## (FG fallback matching, strata weighting, area weighting, plots,
## Excel export). Also runs the MEDIAS acoustic survey (Step 9) as an
## independent analysis over the same GSAs/FG scheme, then COMBINES
## both surveys' FG-level and species-level density into the SAME
## Ecopath/Ecosim/FG_spp sheets (Step 10) - not separate MEDIAS sheets.
##
## Produces the same outputs as medbs_pipeline_current.R - this is a
## refactor of that script's logic into reusable form, not a new
## calculation.
## =================================================================

pkgs <- c("readr", "dplyr", "tidyr", "ggplot2", "stringr", "data.table",
          "marmap", "raster", "terra", "sf", "openxlsx", "readxl", "maps", "scales",
          "rnaturalearth", "rnaturalearthdata")
new_pkgs <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new_pkgs) > 0) install.packages(new_pkgs)
invisible(lapply(pkgs, library, character.only = TRUE))

## =================================================================
## STEP 1: Configuration
## =================================================================
if (tolower(Sys.info()[["user"]]) == "daniel" && .Platform$OS.type == "unix") {
  out_dir <- "/Users/daniel/Work/iMARES/WMed EwE Model/output/"
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
      "Please select the directory where output files",
      "and intermediate results will be saved."
    )
  )
  out_dir <- rstudioapi::selectDirectory()
  if (is.null(out_dir) || out_dir == "" || !dir.exists(out_dir)) {
    stop("No valid output directory selected.")
  }
  
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
## (Run-log/sink()-to-file mechanism removed - it caused two separate
## rounds of real trouble: first sink(type="message") silently dropping
## messages AND errors from the console, then a suspected hang tied to
## sink(type="output", split=TRUE) combined with source()-ing this
## script's large shared-library file and its package loads. A saved
## log file isn't worth that fragility - plain console output only
## from here. If you want a log later, RStudio's own console history
## or a manual `Rscript this_file.R > log.txt 2>&1` from the command
## line are simpler, better-tested ways to get one.)
## =================================================================

#call functions
source(paste0(git_dir,"/scripts/lib_survey_fg_density_functions.R"))
source(paste0(git_dir,"/scripts/lib_worms_taxonomy_lookup.R"))

#files in pcloud
#fg_file should be correctly reference the species scientific name with the FG_name and FG_num
fg_file          <- paste0(pcloud_dir,"/data/FG_WMed.xlsx")
#taxonomy list of MEDITS and MEDIAS species and code species
#downloaded from MEDITS website
tm_list_file     <- paste0(pcloud_dir,"/data/Medits_Medias_JRC2026/2024_MEDBSsurvey/TM_list_(April_2019).xlsx")

## plot_dir is INSIDE out_dir (out_dir/plots), not two directories up
## from it - both are created (recursive=TRUE, in case the full parent
## path doesn't exist yet) rather than assuming either already exists.
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
plot_dir <- file.path(out_dir, "plots")
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

## Loud and explicit on purpose - if this ever matches the OTHER
## example script's own out_dir (01_survey_density_custom.R), both
## scripts would silently write their own survey_sample_coverage_map.png
## etc. to the exact same files, and whichever script ran most recently
## would overwrite the other's plots with no error or warning at all -
## exactly the "both maps show the same thing" symptom that's easy to
## mistake for a plotting bug when it's actually an output-path
## collision. Printed clearly here so it's directly checkable by eye,
## and the existing-file check below catches it even if you don't
## read the console output carefully.
message("This script will write its outputs to:\n  out_dir  = ", out_dir, "\n  plot_dir = ", plot_dir)

FILTER_AREAS  <- 1:11
STRATA        <- TRUE                       # function attribute: strata
YEAR_ECOPATH  <- 1994:1996                   # function attribute: year_ecopath
TS_YEARS      <- 1995:2023                        # function attribute: ts_years (NULL -> min:max of data)
DROP_OUTLIERS <- FALSE                        # function attribute: whether remove_sample_outliers() actually
NORMALIZE_TS  <- TRUE   # TRUE = Ecosim series rescaled to reference index (first value = 1); FALSE = raw density
# removes flagged observations (TRUE) or only reports them (FALSE)

message(
  "This script will run for GSA: ", paste(FILTER_AREAS, collapse = ", "),
  "\nfor Ecopath years: ", paste(range(YEAR_ECOPATH), collapse = "-"),
  "\nfor Ecosim years: ", paste(range(TS_YEARS), collapse = "-")
)

## MEDITS' 5 standard bathymetric strata - passed as strata_def to the
## shared functions, not hardcoded inside them
MEDITS_STRATA <- data.table(
  stratum_num = 1:5,
  depth_min = c(10, 50, 100, 200, 500),
  depth_max = c(49.9999, 99.9999, 199.9999, 499.9999, 799.9999))

## GFCM GSA shapefile - this is the "area_shp" function attribute
gsa_zip_url  <- "https://gfcmsitestorage.blob.core.windows.net/website/5.Data/ArcGIS/GFCM_GSA.zip"
gsa_zip_file <- file.path(out_dir, "GFCM_GSA.zip")
gsa_shp_dir  <- file.path(out_dir, "GFCM_GSA_shp")
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
ta <- read_csv(file.path(pcloud_dir,"data/Medits_Medias_JRC2026/2024_MEDBSsurvey/Demersal/TA.csv"), show_col_types = FALSE)

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

tb <- read_csv(file.path(pcloud_dir, "data/Medits_Medias_JRC2026/2024_MEDBSsurvey/Demersal/TB.csv"), show_col_types = FALSE)
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
## outside their expected area boundary. selected_areas highlights the
## GSAs actually in FILTER_AREAS (this analysis's scope) with a thicker
## blue border, distinguishing them from other GSAs shown on the map
## but outside the analysis.
p_sample_map <- plot_sample_map(dataframe1, area_shp, area_id_col = AREA_ID_COL,
                                title = "MEDITS sample coverage by GSA", selected_areas = FILTER_AREAS)
ggsave(file.path(plot_dir, "survey_sample_coverage_map.png"), p_sample_map, width = 10, height = 8, dpi = 150, bg = "white")

## same coverage map, faceted by year - useful for spotting a specific
## year with a coverage gap that the all-years-combined map above would
## mask (a gap in one year can look fine once every other year's
## samples are overlaid on top of it)
p_sample_map_byyear <- plot_sample_map(dataframe1, area_shp, area_id_col = AREA_ID_COL,
                                       title = "MEDITS sample coverage by GSA and year", by_year = TRUE,
                                       selected_areas = FILTER_AREAS)
ggsave(file.path(plot_dir, "survey_sample_coverage_map_by_year.png"), p_sample_map_byyear,
       width = 16, height = 12, dpi = 150, bg = "white")

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
  taxon_rank   = c("species", "family", "class", "genus", "genus", "class", "species",'species'))

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
  ))

## Species-level exceptions - applied BEFORE the rank rules above, so a
## named species is never caught by a broader rule that's wrong for it
## specifically. Squilla mantis is commercially exploited even though
## the rest of Stomatopoda isn't, so the "Order Stomatopoda ->
## Non-commercial decapods" rule above would misclassify it without
## this override.
SPECIES_EXCEPTIONS <- data.table(
  ScientificName = "Squilla mantis",
  fg_name_target = "Other commercial decapods")

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

## species_taxonomy - built here (moved up from the Excel-export step)
## since apply_catchability_correction() below needs it too, not just
## the export. Starts from fg_lookup_safe's own taxonomy (reference
## species) plus whatever fallback_match_fg_by_taxonomy() fetched
## (species needing genus/family/etc. fallback), then explicitly
## checks for and fetches ANY remaining gap. This matters because
## manually-overridden species (MANUAL_OVERRIDES, Step 3) never go
## through fallback_match_fg_by_taxonomy() at all - they're already
## matched by the time that function runs, so their taxonomy was never
## fetched via that path, and neither fg_lookup_safe nor
## fetched_taxonomy would have them. Using dt's own ScientificName
## values here (not species_density_regional, which doesn't exist yet
## at this point in the pipeline) - this is the same, or a superset of,
## the species that'll end up in species_density_regional later, since
## no new species get introduced between here and there.
species_taxonomy <- unique(rbindlist(list(
  fg_lookup_safe[, .(ScientificName, Genus, Family, Order, Class, Phylum)],
  attr(dt, "fetched_taxonomy")), fill = TRUE), by = "ScientificName")

species_actually_observed <- unique(dt[!is.na(ScientificName), ScientificName])
still_missing_taxonomy <- setdiff(species_actually_observed, species_taxonomy$ScientificName)
if (length(still_missing_taxonomy) > 0) {
  message("\n", length(still_missing_taxonomy), " observed species have no taxonomy yet",
          " (likely matched via MANUAL_OVERRIDES, which doesn't fetch taxonomy) -",
          " fetching directly for these:")
  gap_taxonomy <- fetch_taxonomy(still_missing_taxonomy, taxonomy_source = "worms")
  species_taxonomy <- rbindlist(list(species_taxonomy, gap_taxonomy), fill = TRUE)
  species_taxonomy <- unique(species_taxonomy, by = "ScientificName")
}

## =================================================================
## STEP 5: densities (biomass/effort), mean/sum across species within
## FG, by year/strata/area
## =================================================================
## OPTIONAL - custom sub-region filter, independent of GSA/AreaID.
## Useful if the SAME survey data needs to support a DIFFERENT EwE
## model with its own boundary (a specific bay, a custom polygon that
## cuts across GSA lines, a simple bounding box, etc.) rather than
## being limited to GSA boundaries. Disabled by default here since this
## example still uses GSA-based filtering (FILTER_AREAS/AREA_ID_COL)
## further down - enable one of these if you need a genuinely different
## model boundary instead of/in addition to that.
USE_CUSTOM_AREA_FILTER <- FALSE
if (USE_CUSTOM_AREA_FILTER) {
  ## Option A - a bounding box (simplest, no shapefile needed):
  # CUSTOM_BBOX <- c(xmin = 2, xmax = 8, ymin = 38, ymax = 42)
  # dt <- filter_samples_by_area(dt, CUSTOM_BBOX, filter_type = "bbox")
  
  ## Option B - a custom shapefile (a different model's own boundary,
  ## which may not line up with GSA lines at all):
  # CUSTOM_MODEL_SHAPEFILE <- "/path/to/other_model_boundary.shp"
  # dt <- filter_samples_by_area(dt, CUSTOM_MODEL_SHAPEFILE, filter_type = "shapefile")
}

dt <- match_samples_to_area(dt, area_shp, AREA_ID_COL)   # no-op here, MEDITS already has AreaID
dt <- assign_depth_stratum(dt, MEDITS_STRATA)
dt <- compute_sample_densities(dt)
dt <- dt[AreaID %in% FILTER_AREAS]

## --- Catchability correction -------------------------------------------
## Reads your actual catchability CSV and reshapes it to the
## ScientificName/q format apply_catchability_correction() expects -
## FG/FG_name columns dropped (confirmed not needed), species/q_FACTOR
## renamed. Filename extension (.csv) is still a guess - confirm this
## matches the actual file once it's placed in data/raw.
CATCHABILITY_CSV_PATH <- paste0(pcloud_dir,"/data/catchability_factors_ecotrans_medits_2021_spp.csv")

## PLACEHOLDER - not filled in with your real FG names. Pelagic/
## planktonic/seagrass/algae FGs get q=1 (no catchability correction)
## regardless of anything catchability_table or the taxonomic-proximity
## fallback would otherwise resolve - a bottom trawl survey isn't
## designed to sample these representatively at all, so "catchability"
## as a concept doesn't apply the same way. I don't know your FG
## scheme's exact names for these groups - replace with the real
## FG_name values from your dataframe2 (check unique(dataframe2$FG_name)
## for the actual list to pick from).
EXEMPT_FG_NAMES <- c(
  # e.g. "Small pelagic fish", "Large pelagic fish", "Gelatinous plankton",
  #      "Suprabenthos", "Seagrass", "Macroalgae" - REPLACE with your real FG_name values
)

if (file.exists(CATCHABILITY_CSV_PATH)) {
  catchability_raw <- fread(CATCHABILITY_CSV_PATH)
  CATCHABILITY_TABLE <- catchability_raw[, .(ScientificName = species, q = q_FACTOR)]
  message("Loaded catchability table: ", nrow(CATCHABILITY_TABLE), " entries from ", CATCHABILITY_CSV_PATH)
  ## species_taxonomy (built above, right after Step 4) is what makes
  ## the genus/order/class/phylum-level entries in this table (e.g.
  ## "Isopoda", "Cirolana spp", "Hydrozoa", "Cnidaria") actually
  ## resolve, not just the true species-level ones, and also enables
  ## the taxonomic-proximity fallback (borrowing q from a close
  ## relative already in the table) for anything still unmatched after
  ## that - see apply_catchability_correction()'s own header comment
  ## for the full explanation of the matching order.
  dt <- apply_catchability_correction(dt, CATCHABILITY_TABLE, default_q = 1, species_taxonomy = species_taxonomy,
                                      exempt_fg_names = EXEMPT_FG_NAMES)
} else {
  message("CATCHABILITY_CSV_PATH not found at '", CATCHABILITY_CSV_PATH, "' - skipping catchability",
          " correction entirely. Update the path above to your actual file location.")
}

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
  cache_path = file.path(out_dir, "strata_area_by_area.csv"))

fg_index <- if (STRATA) {
  weight_by_strata(per_group_fg, n_samples_by_stratum, strata_area_by_area)
} else {
  simple_area_density(per_group_fg, n_samples_by_area)
}

## --- MEDITS per-haul density (the replicate unit for CV.log) --------------
## Mirrors the per_sample_fg step inside compute_fg_densities_by_stratum()
## but kept un-aggregated here specifically to measure within-year spread.
medits_replicate_dt <- dt[!is.na(Stratum)][
  !is.na(FG_num) & !is.na(Density),
  .(fg_density = sum(Density, na.rm = TRUE)),
  by = .(SampleID, AreaID, Year, FG_num, FG_name)
]
fg_cv_log_medits <- compute_cv_log_by_fg(medits_replicate_dt, "fg_density")
fg_cv_log_medits[, Survey := "MEDITS"]

## =================================================================
## STEP 7: weight densities per area -> FG, year (region-wide)
## =================================================================
fg_index_regional <- weight_by_area(fg_index)

## species-level regional density (for the FG_spp Excel sheet)
per_group_sp <- compute_species_densities_by_stratum(dt, strata = STRATA)
species_density_regional <- weight_species_by_area(per_group_sp, n_samples_by_stratum, strata_area_by_area)

## =================================================================
## STEP 8: plots (MEDITS)
## =================================================================
p_by_area <- plot_fg_timeseries_by_area(
  fg_index, title = "MEDITS trawl survey by FG and area (strata-weighted)", y_lab = "Density (t/km^2)")
ggsave(file.path(plot_dir, "survey_fg_density_timeseries.png"), p_by_area, width = 14, height = 10, dpi = 150, bg = "white")

p_regional <- plot_fg_timeseries_regional(fg_index_regional,
                                          title = "MEDITS trawl survey by FG, Western Med (area-weighted)", y_lab = "Area-weighted density (t/km^2)")
ggsave(file.path(plot_dir, "survey_fg_density_timeseries_regional.png"), p_regional, width = 14, height = 10, dpi = 150, bg = "white")

if (STRATA) {
  p_profile <- plot_strata_profile(fg_index, MEDITS_STRATA)
  ggsave(file.path(plot_dir, "survey_fg_depth_strata_profile.png"), p_profile, width = 16, height = 12, dpi = 150, bg = "white")
}

## =================================================================
## STEP 9: MEDIAS acoustic survey - FG-level AND species-level DENSITY
## (biomass & abundance per unit area), same FILTER_AREAS/
## fg_lookup_safe/area_shp as MEDITS. NOT strata-weighted (no per-haul
## depth here) and NO catchability correction applied to this data.
## Ends (9f/9g) by combining with MEDITS into single FG-level and
## species-level tables, fed into ONE set of Ecopath/Ecosim/FG_spp
## sheets in Step 10 - NOT written as separate MEDIAS sheets.
##
## Per the MEDIAS Handbook (April 2025, medias-project.eu):
##  - official reporting convention is BIOMASS IN TONS and DENSITY IN
##    t/nm^2 (not kg/km^2) - "Biomass estimation results in tons by GSA
##    and graphs in terms of biomass density (time series of average
##    t/nm2)"
##  - acoustic sampling covers a 10-200m depth band (10m isobath
##    minimum, 200m max echo-sounding depth)
##  - Table 1 gives each institute's own reported survey area size
##    (NM^2) by country/geographic-area name - used here as the primary
##    area denominator, with a bathymetry-derived 10-200m fallback
##    (reusing compute_strata_area_by_area() from the shared library)
##    for any GSA/country combination Table 1 doesn't cover.
## =================================================================
medias_dir  <- paste0(pcloud_dir, "/data/Medits_Medias_JRC2026/2024_MEDBSsurvey/Acoustic/")
## ASFIS species list, downloaded from https://www.fao.org/fishery/collection/asfis/en
asfis_file  <- paste0(pcloud_dir, "/data/ASFIS_sp_2026.1.csv")

## --- 9a. Load + collapse to country/gsa/year/species -----------------------
sum_lengthclasses <- function(df) {
  lc_cols <- grep("^lengthclass", names(df), value = TRUE)
  mat <- as.matrix(df[lc_cols]); mat[mat < 0] <- NA
  totals <- rowSums(mat, na.rm = TRUE)
  totals[rowSums(!is.na(mat)) == 0] <- NA
  totals
}

check_raw_replication <- function(file) {
  df <- read_csv(file.path(medias_dir, file), show_col_types = FALSE) %>%
    mutate(gsa = as.numeric(str_extract(area, "\\d+")))
  if (any(df$sex == "C")) df <- filter(df, sex == "C")
  df %>% dplyr::count(country, gsa, year, species)
}
abund_reps <- check_raw_replication("abundance.csv")
biom_reps  <- check_raw_replication("biomass.csv")
AGG_FUN_MEDIAS <- if (max(abund_reps$n, biom_reps$n) > 1) "mean" else "sum"
message("MEDIAS: using ", AGG_FUN_MEDIAS, "() to collapse raw records",
        " (based on replication check).")

load_acoustic <- function(file, value_name, agg_fun = AGG_FUN_MEDIAS) {
  df <- read_csv(file.path(medias_dir, file), show_col_types = FALSE)
  df$total_value <- sum_lengthclasses(df)
  result <- df %>%
    dplyr::mutate(gsa = as.numeric(str_extract(area, "\\d+"))) %>%
    { if (any(.$sex == "C")) dplyr::filter(., sex == "C") else . } %>%
    dplyr::group_by(country, gsa, year, species) %>%
    dplyr::summarise(value = if (agg_fun == "mean") mean(total_value, na.rm = TRUE)
                     else sum(total_value, na.rm = TRUE), .groups = "drop")
  setnames(as.data.table(result), "value", value_name)
}
acoustic_abund <- load_acoustic("abundance.csv", "total_abundance")
acoustic_biom  <- load_acoustic("biomass.csv",   "total_biomass")
acoustic <- merge(acoustic_biom, acoustic_abund, by = c("country","gsa","year","species"), all = TRUE)

## --- 9b. species code -> ScientificName -> FG (reuses fg_lookup_safe) -----
## ASFIS 2026.1 structure: ISSCAAP_Group, Taxonomic_Code, Alpha3_Code,
## Scientific_Name, English_name, ... - species code column is Alpha3_Code.
fao_species <- fread(asfis_file, encoding = "UTF-8")
fao_code_lookup <- unique(fao_species[, .(species = Alpha3_Code, ScientificName = Scientific_Name)])
acoustic <- merge(acoustic, fao_code_lookup, by = "species", all.x = TRUE)

n_no_sci <- acoustic[is.na(ScientificName), uniqueN(species)]
if (n_no_sci > 0) {
  message("MEDIAS: ", n_no_sci, " species code(s) with no ASFIS scientific-name match - excluded:")
  print(unique(acoustic[is.na(ScientificName), .(species)]))
}

acoustic_matched <- match_species_to_fg(acoustic[!is.na(ScientificName)], fg_lookup_safe)
acoustic_matched <- match_nominate_subspecies_fg(acoustic_matched, fg_lookup_safe)
acoustic_matched <- fallback_match_fg_by_taxonomy(acoustic_matched, fg_lookup_safe, taxonomy_source = "worms")
message("MEDIAS: ", acoustic_matched[!is.na(FG_num), uniqueN(ScientificName)], " of ",
        acoustic_matched[, uniqueN(ScientificName)], " species matched to an FG.")

## --- 9c. Area denominator: MEDIAS Handbook Table 1 (per country, NM^2) ----
## Restricted here to entries plausibly overlapping FILTER_AREAS (GSA 1-11,
## Western Med) - Adriatic/Black Sea entries from Table 1 omitted since
## they're out of scope for this pipeline's region.
MEDIAS_HANDBOOK_AREAS <- data.table(
  country        = c("Spain", "France", "Italy",           "Italy",                      "Italy",   "Malta"),
  geographic_area = c("Iberian coast", "Gulf of Lion", "Sardinia (east)", "Tyrrhenian and Ligurian Sea", "Sicily Channel", "Malta (east)"),
  area_nm2       = c(8829,   3300,      3207,              6644,                          4300,      1868)
)

## Area-name -> GSA number crosswalk. NOT stated explicitly in the
## handbook (Table 1 only names geographic areas) - this is my best-guess
## mapping based on standard GFCM GSA geography and should be CONFIRMED
## against your own institute's MEDIAS survey reports before trusting the
## resulting density values, particularly for Spain (Iberian coast could
## plausibly span more/fewer GSAs than listed here depending on which
## years/vessels covered which stretch).
GSA_AREA_CROSSWALK <- data.table(
  country = c("Spain", "Spain", "Spain", "France", "Italy", "Italy", "Italy", "Italy", "Malta"),
  gsa     = c(1,        5,       6,       7,        9,       10,      11,      16,      15),
  geographic_area = c("Iberian coast", "Iberian coast", "Iberian coast", "Gulf of Lion",
                      "Tyrrhenian and Ligurian Sea", "Tyrrhenian and Ligurian Sea",
                      "Sardinia (east)", "Sicily Channel", "Malta (east)")
)
GSA_AREA_CROSSWALK <- merge(GSA_AREA_CROSSWALK, MEDIAS_HANDBOOK_AREAS,
                            by = c("country", "geographic_area"))
message("GSA <-> handbook area crosswalk (VERIFY this against your own institute's",
        " reports before trusting density values downstream):")
print(GSA_AREA_CROSSWALK[order(country, gsa)])

## --- 9c(i). Per-country/GSA biomass totals - no country-averaging here,
## country coverage areas are real, additive quantities -------------------
acoustic_fg_by_country_gsa <- acoustic_matched[
  !is.na(FG_num),
  .(total_biomass_t = sum(total_biomass, na.rm = TRUE),   # ASSUMED already tons - verify against summary() below
    total_abundance = sum(total_abundance, na.rm = TRUE)),
  by = .(country, AreaID = gsa, Year = year, FG_num, FG_name)
][AreaID %in% FILTER_AREAS]

message("Biomass summary (verify these look like tons, not kg, per the handbook's own",
        " 'biomass estimation results in tons' convention):")
print(summary(acoustic_matched[!is.na(FG_num), total_biomass]))

## --- 9c(ii). Attach area: handbook value where the crosswalk covers it,
## bathymetric 10-200m fallback (compute_strata_area_by_area(), reused
## from lib_survey_fg_density_functions.R) otherwise ---------------------------
acoustic_fg_by_country_gsa <- merge(
  acoustic_fg_by_country_gsa, GSA_AREA_CROSSWALK[, .(country, AreaID = gsa, area_nm2)],
  by = c("country", "AreaID"), all.x = TRUE)

missing_area <- unique(acoustic_fg_by_country_gsa[is.na(area_nm2), .(country, AreaID)])
if (nrow(missing_area) > 0) {
  message(nrow(missing_area), " country/GSA combination(s) not in the handbook crosswalk -",
          " falling back to a bathymetry-derived 10-200m area estimate for these:")
  print(missing_area)
  
  MEDIAS_DEPTH_RANGE <- data.table(stratum_num = 1, depth_min = 10, depth_max = 200)
  fallback_area_km2 <- compute_strata_area_by_area(
    area_ids = missing_area$AreaID, area_shp = area_shp, area_id_col = AREA_ID_COL,
    strata_def = MEDIAS_DEPTH_RANGE,
    cache_path = file.path(out_dir, "medias_area_10_200m_fallback.csv")
  )[, .(AreaID, area_nm2_fallback = area_km2 / 1.852^2)]
  
  acoustic_fg_by_country_gsa <- merge(acoustic_fg_by_country_gsa, fallback_area_km2,
                                      by = "AreaID", all.x = TRUE)
  acoustic_fg_by_country_gsa[is.na(area_nm2), area_nm2 := area_nm2_fallback]
  acoustic_fg_by_country_gsa[, area_nm2_fallback := NULL]
}
acoustic_fg_by_country_gsa <- acoustic_fg_by_country_gsa[!is.na(area_nm2)]

## --- 9c(iii). Combine countries within a GSA (e.g. a GSA covered by more
## than one institute) via a proper area-weighted sum, then compute density
KM2_PER_NM2 <- 1.852^2
acoustic_fg_by_gsa <- acoustic_fg_by_country_gsa[
  , .(total_biomass_t = sum(total_biomass_t, na.rm = TRUE),
      total_abundance = sum(total_abundance, na.rm = TRUE),
      area_nm2        = sum(unique(area_nm2))),   # unique() - country-level area shouldn't be summed per FG row, just once per country/GSA
  by = .(AreaID, Year, FG_num, FG_name)
]
acoustic_fg_by_gsa[, `:=`(
  density_biomass_t_nm2   = total_biomass_t / area_nm2,          # official MEDIAS convention
  density_biomass_t_km2   = total_biomass_t / (area_nm2 * KM2_PER_NM2),  # for MEDITS comparison
  density_abundance_n_nm2 = total_abundance / area_nm2
)]

## --- MEDIAS per-country/GSA density (the replicate unit for CV.log) ------
## No individual hauls in MEDIAS' aggregated data - country/GSA totals
## within a year are the finest replicate unit available.
acoustic_fg_by_country_gsa[, density_t_km2 := total_biomass_t / (area_nm2 * KM2_PER_NM2)]
fg_cv_log_medias <- compute_cv_log_by_fg(acoustic_fg_by_country_gsa, "density_t_km2")
fg_cv_log_medias[, Survey := "MEDIAS"]

## --- 9d. Region-wide FG index - area-weighted, same pattern as MEDITS'
## weight_by_area(): D_region = sum(D_gsa * A_gsa) / sum(A_gsa)
medias_fg_index_regional <- acoustic_fg_by_gsa[
  , .(mean_density_biomass_t_nm2   = sum(density_biomass_t_nm2 * area_nm2, na.rm = TRUE) / sum(area_nm2, na.rm = TRUE),
      mean_density_biomass_t_km2   = sum(total_biomass_t, na.rm = TRUE) / sum(area_nm2 * KM2_PER_NM2, na.rm = TRUE),
      mean_density_abundance_n_nm2 = sum(density_abundance_n_nm2 * area_nm2, na.rm = TRUE) / sum(area_nm2, na.rm = TRUE),
      n_gsa_contributing = uniqueN(AreaID)),
  by = .(Year, FG_num, FG_name)
]
message("MEDIAS region-wide FG annual DENSITY index built: ", nrow(medias_fg_index_regional),
        " rows. Density in t/nm^2 (handbook convention) and t/km^2 (MEDITS comparison).",
        " NO catchability correction applied.")

fwrite(acoustic_fg_by_gsa, file.path(out_dir, "medias_fg_annual_density_by_area.csv"))
fwrite(medias_fg_index_regional, file.path(out_dir, "medias_fg_annual_density_regional.csv"))

## --- 9e. Plots (MEDIAS) -----------------------------------------------------
p_medias_biom <- plot_fg_timeseries_regional(
  medias_fg_index_regional[, .(Year, FG_num, FG_name, mean_density = mean_density_biomass_t_nm2)],
  title = "MEDIAS acoustic survey - biomass density by FG, Western Med", y_lab = "Density (t/nm^2)")
ggsave(file.path(plot_dir, "medias_fg_biomass_density_timeseries_regional.png"), p_medias_biom,
       width = 14, height = 10, dpi = 150, bg = "white")

p_medias_abund <- plot_fg_timeseries_regional(
  medias_fg_index_regional[, .(Year, FG_num, FG_name, mean_density = mean_density_abundance_n_nm2)],
  title = "MEDIAS acoustic survey - abundance density by FG, Western Med", y_lab = "Density (n/nm^2)")
ggsave(file.path(plot_dir, "medias_fg_abundance_density_timeseries_regional.png"), p_medias_abund,
       width = 14, height = 10, dpi = 150, bg = "white")

## --- 9f. Species-level MEDIAS density (mirrors species_density_regional
## from MEDITS) - needed to feed FG_spp_Ecopath/FG_spp_Ecosim, not just
## the FG-level Ecopath/Ecosim sheets. Reuses the SAME per-country/GSA
## area (area_nm2) already resolved in 9c(ii)/9c(iii) for the FG-level
## index - area doesn't depend on species, so pulled from
## acoustic_fg_by_country_gsa rather than recomputed.
area_by_country_gsa <- unique(acoustic_fg_by_country_gsa[, .(country, AreaID, area_nm2)])

acoustic_sp_by_country_gsa <- acoustic_matched[
  !is.na(FG_num),
  .(species_biomass_t = sum(total_biomass, na.rm = TRUE)),
  by = .(country, AreaID = gsa, Year = year, FG_num, FG_name, ScientificName)
][AreaID %in% FILTER_AREAS]

acoustic_sp_by_country_gsa <- merge(acoustic_sp_by_country_gsa, area_by_country_gsa,
                                    by = c("country", "AreaID"), all.x = TRUE)
acoustic_sp_by_country_gsa <- acoustic_sp_by_country_gsa[!is.na(area_nm2)]

acoustic_sp_by_gsa <- acoustic_sp_by_country_gsa[
  , .(species_biomass_t = sum(species_biomass_t, na.rm = TRUE),
      area_nm2 = sum(unique(area_nm2))),
  by = .(AreaID, Year, FG_num, FG_name, ScientificName)
]
acoustic_sp_by_gsa[, density_t_km2 := species_biomass_t / (area_nm2 * KM2_PER_NM2)]

species_density_regional_medias <- acoustic_sp_by_gsa[
  , .(mean_density = sum(density_t_km2 * (area_nm2 * KM2_PER_NM2), na.rm = TRUE) /
        sum(area_nm2 * KM2_PER_NM2, na.rm = TRUE)),
  by = .(Year, FG_num, FG_name, ScientificName)
]
message("MEDIAS region-wide species-level density built: ", nrow(species_density_regional_medias), " rows.")

## --- 9g. Combine MEDITS + MEDIAS into single FG-level and species-level
## tables (t/km^2 throughout) - these, not separate MEDIAS sheets, are
## what feed export_ecopath_ecosim_excel() in Step 10.
fg_index_combined_raw <- rbindlist(list(
  fg_index_regional[, .(Year, FG_num, FG_name, mean_density, Survey = "MEDITS")],
  medias_fg_index_regional[, .(Year, FG_num, FG_name, mean_density = mean_density_biomass_t_km2, Survey = "MEDIAS")]
), fill = TRUE)

fg_overlap <- fg_index_combined_raw[, .N, by = .(Year, FG_num, FG_name)][N > 1]
if (nrow(fg_overlap) > 0) {
  message("NOTE: ", nrow(fg_overlap), " FG/Year combination(s) have density from BOTH MEDITS",
          " and MEDIAS - these are AVERAGED in the combined Ecopath/Ecosim index below.",
          " Review whether blending a demersal-trawl density with an acoustic density is",
          " appropriate for these specific FG/years before trusting them:")
  print(merge(fg_overlap[, .(Year, FG_num, FG_name)], fg_index_combined_raw,
              by = c("Year", "FG_num", "FG_name"))[order(FG_num, Year)])
}
fg_index_regional_combined <- fg_index_combined_raw[
  , .(mean_density = mean(mean_density, na.rm = TRUE)), by = .(Year, FG_num, FG_name)]

sp_index_combined_raw <- rbindlist(list(
  species_density_regional[, .(Year, FG_num, FG_name, ScientificName, mean_density, Survey = "MEDITS")],
  species_density_regional_medias[, .(Year, FG_num, FG_name, ScientificName, mean_density, Survey = "MEDIAS")]
), fill = TRUE)

sp_overlap <- sp_index_combined_raw[, .N, by = .(Year, ScientificName)][N > 1]
if (nrow(sp_overlap) > 0) {
  message("NOTE: ", nrow(sp_overlap), " species/Year combination(s) appear in BOTH surveys -",
          " averaged in the combined species-level index. Likely genuine overlap species",
          " (e.g. a small pelagic also taken in demersal trawl catches) - worth a look:")
  print(merge(sp_overlap[, .(Year, ScientificName)], sp_index_combined_raw,
              by = c("Year", "ScientificName"))[order(ScientificName, Year)])
}
species_density_regional_combined <- sp_index_combined_raw[
  , .(mean_density = mean(mean_density, na.rm = TRUE)), by = .(Year, FG_num, FG_name, ScientificName)]

## Written out here as its own file - up to now this table only ever
## existed in-memory, passed straight into export_ecopath_ecosim_excel()
## for the FG_spp sheets. 04_pbqb_calc.R (the PB/QB estimation pipeline)
## needs this same species x FG x density data as its species_df input
## (Species/FG/Biomass), so it's saved as a plain CSV here rather than
## making 04_pbqb_calc.R parse the Ecopath-formatted Excel sheet.
fwrite(species_density_regional_combined, file.path(out_dir, "species_density_regional_combined.csv"))
message("Saved species_density_regional_combined.csv (", nrow(species_density_regional_combined),
        " rows) - MEDITS+MEDIAS combined species-level density, all years. This is the",
        " file lib_build_species_df_from_survey.R reads to build 04_pbqb_calc.R's species_df input.")

## --- Combine CV.log across surveys - average where both have a value,
## use whichever exists otherwise. Prints overlaps for the same reason
## the density combination does: blending two different sampling designs'
## variability estimates is a modeling choice worth a second look, not
## something to average away silently.
cv_log_combined_raw <- rbindlist(list(fg_cv_log_medits, fg_cv_log_medias), fill = TRUE)
cv_overlap <- cv_log_combined_raw[, .N, by = FG_num][N > 1]
if (nrow(cv_overlap) > 0) {
  message("NOTE: ", nrow(cv_overlap), " FG(s) have a CV.log from BOTH surveys - AVERAGED below:")
  print(merge(cv_overlap[, .(FG_num)], cv_log_combined_raw, by = "FG_num")[order(FG_num)])
}
fg_cv_log_combined <- cv_log_combined_raw[, .(cv_log = mean(cv_log, na.rm = TRUE)), by = FG_num]

## =================================================================
## STEP 10: Excel export - MEDITS + MEDIAS COMBINED into the SAME
## FG_spp_Ecopath/Ecopath/Ecosim/FG_spp_Ecosim sheets (no separate
## MEDIAS sheets). species_taxonomy was already built earlier (right
## after Step 4, FG matching) since apply_catchability_correction()
## needed it too - reused as-is here, not rebuilt.
## =================================================================
fwrite(fg_index, file.path(out_dir, "survey_fg_annual_index.csv"))
fwrite(fg_index_regional_combined, file.path(out_dir, "survey_fg_annual_index_regional_combined.csv"))

export_ecopath_ecosim_excel(
  fg_index_regional = fg_index_regional_combined,
  species_density_regional = species_density_regional_combined,
  n_samples_by_area_year = n_samples_by_stratum[, .(n_samples = sum(n_samples)), by = .(AreaID, Year)],
  dataframe2 = dataframe2,
  year_ecopath = YEAR_ECOPATH,
  ts_years = TS_YEARS,
  out_path = file.path(out_dir, "ecopath_ecosim_inputs.xlsx"),
  species_taxonomy = species_taxonomy,
  fg_cv_log = fg_cv_log_combined,
  normalize_ts = NORMALIZE_TS
)

## =================================================================
## STEP 11: Complete FG_spp_Ecopath / FG_lookup sheets.
##
## FG_spp_Ecopath includes EVERY species from the UNION of:
##   (a) dataframe2 (the literal reference catalog, fg_wmed_95), and
##   (b) species_density_regional_combined (species actually observed
##       and matched to an FG this run - including taxa resolved via
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
all_species_fg <- unique(species_density_regional_combined[, .(FG_num, FG_name, Species = ScientificName)])
fg_master_species <- unique(dataframe2[, .(FG_num, FG_name, Species = ScientificName)])
full_species_fg <- unique(rbindlist(list(all_species_fg, fg_master_species)), by = "Species")

## --- base-year Density + within-FG proportion, zero-filled -------------
density_in_base_years <- species_density_regional_combined[
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
## STEP 12: traits_ewe sheet - species-level life-history/ecology
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
## STEP 13: fix sheet names/order in ecopath_ecosim_inputs.xlsx.
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
    "Catches_Ecosim", "Fleet_Structure", "Catches_by_Fleet", "Catches_by_Fleet_AllYears",
    "Fishing_Effort_by_Fleet", "DataSources_Catch"
  )
)

message("\nDone. Outputs in ", out_dir, " and ", plot_dir)
message("Run finished: ", Sys.time())

