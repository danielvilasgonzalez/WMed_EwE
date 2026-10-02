## =================================================================
## Created by: Daniel Vilas
## PIPELINE STEP 1 of 4 - single script for EVERY region.
## Run FIRST - no dependencies on the other numbered scripts.
## Produces: species_density_regional_combined.csv, strata_area_by_area.csv,
## and the Biomass sheets (FG_spp_Ecopath/Ecopath/Ecosim/FG_spp_Ecosim)
## in output/ecopath_ecosim_inputs.xlsx.
## 02_fisheries.R and 03_pbqb-traits.R both REQUIRE this to have run
## first (strata_area_by_area.csv and species_density_regional_combined.csv
## respectively don't exist until this does).
##
## AREA_MODE (see STEP 1 below) picks the region: "westmed" (default) -
## the Western Med, GSA 1-11, with MEDIAS acoustic survey + stock-
## assessment priority layered on top of MEDITS - or "custom" - any
## GSA subset, bounding box, or arbitrary shapefile, MEDITS-only.
## Merged from what used to be two separate scripts (01_biomass.R for
## Western Med, 01_survey_density_custom.R for a custom region) into
## this one file, so there's a single script to maintain and a single
## set of fixes/improvements that both regions benefit from - see
## AREA_MODE's own comment for what's genuinely region-specific vs. shared.
## =================================================================

## =================================================================
## 01_biomass.R
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
## Excel export).
##
## AREA_MODE == "westmed" (default) additionally runs the MEDIAS
## acoustic survey (Step 9) as an independent analysis over the same
## GSAs/FG scheme, then COMBINES both surveys' FG-level and species-
## level density into the SAME Ecopath/Ecosim/FG_spp sheets (Step 10) -
## not separate MEDIAS sheets - and layers a GFCM STAR/RAM Legacy
## stock-assessment priority on top of that. AREA_MODE == "custom" is
## MEDITS-only - no MEDIAS, no stock-assessment layer - since neither
## has a defined meaning outside the named GSAs they're built around.
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
##
## SOURCE-ABLE SCRIPT: every variable below is only set when it isn't
## already defined in the calling environment - if a driver script
## (e.g. run_pipeline_demo.R) sets out_dir/pcloud_dir/git_dir,
## FILTER_AREAS, YEAR_ECOPATH, TS_YEARS, etc. BEFORE calling
## source("01_biomass.R"), those values are used as-is
## and none of the interactive prompts/hardcoded defaults below fire.
## Running this script standalone (nothing pre-set) reproduces the
## exact original Western Med (GSA 1-11) behavior - this change is
## purely additive.
## =================================================================

## AREA_MODE picks the region this run covers - "westmed" (default) for
## the named Western Med GSAs (1-11), or "custom" for any GSA subset,
## bounding box, or arbitrary shapefile that doesn't line up with GSA
## lines at all. Set BEFORE source()-ing this script, same pattern as
## every other knob here. Everything below that's genuinely specific to
## AREA_MODE == "custom" (AREA_NAME, CUSTOM_AREA_TYPE/CUSTOM_GSA_IDS/
## CUSTOM_BBOX/CUSTOM_SHAPEFILE_PATH) is only READ when AREA_MODE ==
## "custom" - defining them has no effect otherwise.
if (!exists("AREA_MODE", envir = .GlobalEnv, inherits = FALSE)) AREA_MODE <- "westmed"   # "westmed" or "custom"
if (!AREA_MODE %in% c("westmed", "custom")) {
  stop("AREA_MODE must be \"westmed\" or \"custom\" - got \"", AREA_MODE, "\".")
}

if (AREA_MODE == "custom") {
  ## AREA_NAME - a short label for this custom study area, used to build
  ## a DEDICATED output subfolder (out_dir/AREA_NAME/ by default, when
  ## out_dir itself isn't pre-set - see out_dir's own resolution below)
  ## so two different custom-region runs never collide or overwrite each
  ## other's outputs. Typed interactively when unset and running as
  ## "daniel", hardcoded otherwise - EDIT this to your actual study area
  ## name (e.g. "cap_de_creus_mpa") rather than leaving the placeholder.
  if (!exists("AREA_NAME", envir = .GlobalEnv, inherits = FALSE)) {
    if (tolower(Sys.info()[["user"]]) == "daniel") {
      AREA_NAME <- "custom_region"
    } else if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      AREA_NAME <- rstudioapi::showPrompt(
        title = "Custom study area name",
        message = "Short label for this custom study area (used in output folder/file names):",
        default = "custom_region"
      )
      if (is.null(AREA_NAME) || AREA_NAME == "") stop("No AREA_NAME entered.")
    } else {
      AREA_NAME <- "custom_region"
    }
  }
  
  ## CUSTOM_AREA_TYPE picks how the custom boundary itself is defined -
  ## see STEP 1's area-definition block below for what each does:
  ##  "bbox" (default) - a rectangle, CUSTOM_BBOX <- c(xmin, xmax, ymin, ymax)
  ##  "shapefile" - an arbitrary polygon, CUSTOM_SHAPEFILE_PATH
  ##  "gsa" - one or more GFCM GSAs, CUSTOM_GSA_IDS <- c(...) - the same
  ##      GSA shapefile AREA_MODE == "westmed" downloads, just subset to
  ##      a different (or single) GSA grouping than the fixed 1:11 range.
  if (!exists("CUSTOM_AREA_TYPE", envir = .GlobalEnv, inherits = FALSE)) CUSTOM_AREA_TYPE <- "bbox"
  if (!CUSTOM_AREA_TYPE %in% c("bbox", "shapefile", "gsa")) {
    stop("CUSTOM_AREA_TYPE must be \"bbox\", \"shapefile\", or \"gsa\" - got \"", CUSTOM_AREA_TYPE, "\".")
  }
  if (!exists("CUSTOM_GSA_IDS",        envir = .GlobalEnv, inherits = FALSE)) CUSTOM_GSA_IDS        <- c(11)
  if (!exists("CUSTOM_BBOX",           envir = .GlobalEnv, inherits = FALSE)) CUSTOM_BBOX           <- c(xmin = 2, xmax = 8, ymin = 38, ymax = 42)
  if (!exists("CUSTOM_SHAPEFILE_PATH", envir = .GlobalEnv, inherits = FALSE)) CUSTOM_SHAPEFILE_PATH <- "/path/to/your_model_boundary.shp"
}

if (exists("out_dir", envir = .GlobalEnv, inherits = FALSE) &&
    exists("pcloud_dir", envir = .GlobalEnv, inherits = FALSE) &&
    exists("git_dir", envir = .GlobalEnv, inherits = FALSE)) {
  message("[01_survey_density] Using pre-set out_dir/pcloud_dir/git_dir from calling environment:\n  out_dir  = ", out_dir, "\n  pcloud_dir = ", pcloud_dir, "\n  git_dir  = ", git_dir)
  ## A pre-set out_dir that doesn't exist on this machine would otherwise
  ## fail silently (dir.create(..., recursive=TRUE) further down just
  ## returns FALSE with a warning when it lacks permission to create it,
  ## e.g. someone else's home directory) or crash much later inside
  ## ggsave() with a cryptic error. run_pipeline_demo.R checks this itself
  ## before sourcing anything, but this guard stays here too for any other driver.
  if (!dir.exists(out_dir)) {
    stop("[01_biomass.R] out_dir was pre-set by the calling script but doesn't exist on this machine: \"",
         out_dir, "\". Fix it in the driver script (e.g. run_pipeline_demo.R) before sourcing this file.")
  }
} else if (tolower(Sys.info()[["user"]]) == "daniel" && .Platform$OS.type == "unix") {
  ## AREA_MODE == "custom" nests the default out_dir under AREA_NAME, so
  ## different custom-region runs never collide or overwrite each
  ## other's outputs - AREA_MODE == "westmed" keeps the plain path
  ## exactly as before.
  out_dir <- if (AREA_MODE == "custom") {
    file.path("/Users/daniel/Work/iMARES/WMed EwE Model/output", AREA_NAME)
  } else {
    "/Users/daniel/Work/iMARES/WMed EwE Model/output/"
  }
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
  if (is.null(pcloud_dir) || pcloud_dir == "" || !dir.exists(pcloud_dir)) {
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
  if (is.null(git_dir) || git_dir == "" || !dir.exists(git_dir)) {
    stop("No valid Github directory selected.")
  }
}

## --- Run log (plain text, for sharing/debugging) ------------------------
## Captures this run's console output (cat/print/message/warning) into a
## timestamped .txt file under out_dir, in addition to the normal console,
## so a run can be reviewed or shared without copying the console by hand.
.run_log_path <- file.path(out_dir, paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_01_biomass_log.txt"))
.run_log_con  <- file(.run_log_path, open = "wt")
sink(.run_log_con, split = TRUE)
sink(.run_log_con, split = TRUE, type = "message")
message("[Log] This run's console output is also being written to: ", .run_log_path)

## MEDITS_REFERENCE_DIR: hand-typed reference/lookup tables for this
## script, externalized to CSV so they're editable without touching code.
## Lives under pcloud_dir, same data/code separation as fg_species_file
## etc. above (reference/raw data on pCloud, scripts on GitHub).
MEDITS_REFERENCE_DIR <- file.path(pcloud_dir, "data/Complementary data/medits_reference_tables")
read_medits_reference <- function(filename, required_cols = NULL) {
  path <- file.path(MEDITS_REFERENCE_DIR, filename)
  if (!file.exists(path)) {
    stop("[read_medits_reference] Reference table not found: \"", path, "\".")
  }
  dt <- fread(path, encoding = "UTF-8")
  if (!is.null(required_cols) && !all(required_cols %in% names(dt))) {
    stop("[read_medits_reference] \"", filename, "\" is missing required column(s): ",
         paste(setdiff(required_cols, names(dt)), collapse = ", "))
  }
  dt
}

#call functions
source(paste0(git_dir,"/scripts/lib_survey_fg_density_functions.R"))
source(paste0(git_dir,"/scripts/lib_worms_taxonomy_lookup.R"))

#files in pcloud
## fg_species_file: the species -> FG reference (ScientificName/FG_num/FG_name +
## taxonomy) used to build dataframe2. As of the 2026 review this is a CSV
## (FG_WMed_2026.csv - the columns are species/FG_number/FG_name/Genus/Family/
## Order/Class/Phylum/source/status - the same shape build_full_fg_species_
## catalog() in the shared library writes, since that's what this file was
## reviewed from), not the old FG_WMed.xlsx sheet 4. Point this at wherever the
## current reviewed CSV lives; the code below (STEP 2) reads it with fread(),
## not readxl::read_excel().
fg_species_file  <- resolve_pcloud_file(paste0(pcloud_dir,"/data/FG_WMed_2026.csv"), pcloud_dir)
#taxonomy list of MEDITS and MEDIAS species and code species
#downloaded from MEDITS website
tm_list_file     <- resolve_pcloud_file(paste0(pcloud_dir,"/data/Medits_Medias_JRC2026/2024_MEDBSsurvey/TM_list_(April_2019).xlsx"), pcloud_dir)

## plot_dir is INSIDE out_dir (out_dir/plots/biomass), not two directories
## up from it - both are created (recursive=TRUE, in case the full parent
## path doesn't exist yet) rather than assuming either already exists.
## Each script's REGULAR (non-validation) plots live in their own named
## subfolder under out_dir/plots/ - biomass's own figures go here,
## fisheries' in out_dir/plots/fisheries (02_fisheries.R), pbqb-traits'
## in out_dir/plots/pbqb-traits (03_pbqb-traits.R). The shared
## cross-script Ecopath-input validation checks (PB/QB, F, P/Q ratio,
## diet matrix) land in out_dir/plots/validation (see 03_pbqb-traits.R
## and 04_diets.R) regardless of which script produced them.
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
plot_dir <- file.path(out_dir, "plots", "biomass")
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

## This block's own native/intermediate CSV outputs (everything below
## EXCEPT the shared ecopath_ecosim_inputs.xlsx workbook, which stays at
## the top-level out_dir since 02/03/04 all read/write it too) go into
## their own "biomass" subfolder. Other blocks that read this block's
## CSVs (species_density_regional_combined.csv, strata_area_by_area.csv,
## FG_lookup.csv, Ecosim.csv) point at this same subfolder explicitly -
## see 02_fisheries.R/03_pbqb-traits.R/04_diets.R.
csv_out_dir <- file.path(out_dir, "biomass")
if (!dir.exists(csv_out_dir)) dir.create(csv_out_dir, recursive = TRUE)

## Loud and explicit on purpose - if two different runs of this same
## script (e.g. AREA_MODE == "westmed" and a AREA_MODE == "custom" run
## with the same AREA_NAME, or two custom runs that reuse an AREA_NAME)
## ever resolve to the same out_dir, both would silently write their own
## survey_sample_coverage_map.png etc. to the exact same files, and
## whichever run happened most recently would overwrite the other's
## plots with no error or warning at all - exactly the "both maps show
## the same thing" symptom that's easy to mistake for a plotting bug
## when it's actually an output-path collision. Printed clearly here so
## it's directly checkable by eye, and the existing-file check below
## catches it even if you don't read the console output carefully.
message("This script will write its outputs to:\n  out_dir  = ", out_dir, "\n  plot_dir = ", plot_dir)

## Region/year/QC knobs - each only takes its default (Western Med,
## GSA 1-11) if not already set by a calling driver script. Set any
## subset of these in the global environment before source()-ing this
## script to run a custom region/year range; anything not pre-set
## keeps the West Med default.
## FILTER_AREAS' AREA_MODE == "custom" default is NULL, not a literal -
## it's derived from area_shp once the custom boundary itself is built
## below (STEP 1's area-definition block), since a custom bbox/shapefile
## doesn't have a fixed, known-in-advance set of area codes the way the
## Western Med's GSA 1-11 does.
if (!exists("FILTER_AREAS",  envir = .GlobalEnv, inherits = FALSE)) FILTER_AREAS  <- if (AREA_MODE == "custom") NULL else 1:11        # GSA
if (!exists("STRATA",        envir = .GlobalEnv, inherits = FALSE)) STRATA        <- TRUE         # function attribute: strata
if (!exists("YEAR_ECOPATH",  envir = .GlobalEnv, inherits = FALSE)) YEAR_ECOPATH  <- 1994:1996     # function attribute: year_ecopath
## TS_YEARS' default last year used to be the hardcoded literal 2023,
## which silently went stale as soon as a newer MEDITS year was added to
## TA.csv. Captured here (before the default below fires) so the block
## right after `ta` is read (~line 527) knows whether to leave a pre-set
## TS_YEARS alone or recompute its default end year from the real data.
TS_YEARS_WAS_PRESET <- exists("TS_YEARS", envir = .GlobalEnv, inherits = FALSE)
if (!exists("TS_YEARS",      envir = .GlobalEnv, inherits = FALSE)) TS_YEARS      <- 1995:2023     # function attribute: ts_years (NULL -> min:max of data); last year corrected below to the real MEDITS data once TA.csv is read, if not pre-set
if (!exists("DROP_OUTLIERS", envir = .GlobalEnv, inherits = FALSE)) DROP_OUTLIERS <- TRUE         # function attribute: whether remove_sample_outliers() actually
if (!exists("NORMALIZE_TS",  envir = .GlobalEnv, inherits = FALSE)) NORMALIZE_TS  <- TRUE         # TRUE = Ecosim series rescaled to reference index (first value = 1); FALSE = raw density
# removes flagged observations (TRUE) or only reports them (FALSE)
## Per project decision, EcoBase (existing published Ecopath models) is
## the preferred source for biomass on FGs no survey here samples -
## macro-/meso-/microzooplankton, large/small phytoplankton, Posidonia/
## seagrass, macroalgae, gorgonians and corals - over hand-copying a
## number out of a paper's supplementary tables. Same knob pattern as
## 03_pbqb-traits.R's ENABLE_ECOBASE_QUERY/ECOBASE_FORCE_REFRESH.
if (!exists("ENABLE_ECOBASE_BIOMASS_QUERY", envir = .GlobalEnv, inherits = FALSE)) ENABLE_ECOBASE_BIOMASS_QUERY <- TRUE
if (!exists("ECOBASE_BIOMASS_FORCE_REFRESH", envir = .GlobalEnv, inherits = FALSE)) ECOBASE_BIOMASS_FORCE_REFRESH <- FALSE
source(file.path(git_dir, "scripts/03b_ecobase.R"))
if (!exists("SAVE_SHINY_CACHE_PATH", envir = .GlobalEnv, inherits = FALSE)) SAVE_SHINY_CACHE_PATH <- NULL
## Opt-in, default off (NULL = don't save). Set to a file path (e.g.
## file.path(out_dir, "shiny_cache", "survey_density_raw.rds")) to save a
## snapshot of the final per-sample MEDITS table - FG-matched, area/stratum-
## assigned, catchability-corrected, outliers already resolved - plus
## area_shp/MEDITS_STRATA/species_taxonomy, for pipeline_documentation_shiny.qmd's
## reactive "Try it" figures to re-filter/re-aggregate by year/area/strata
## without re-running Steps 1-6 on every input change. For that cache to
## support exploring an arbitrary year/region choice (not just the one this
## run happened to be scoped to), set FILTER_AREAS/YEAR_ECOPATH/TS_YEARS to
## their full available extent for this one caching run before setting this.
if (!exists("STOP_AFTER_SHINY_CACHE", envir = .GlobalEnv, inherits = FALSE)) STOP_AFTER_SHINY_CACHE <- FALSE
## Opt-in, default off, only meaningful together with SAVE_SHINY_CACHE_PATH.
## When TRUE, the script exits cleanly right after saving that cache (below,
## after outlier removal - Steps 1-5) instead of continuing through Steps
## 6-13 (MEDIAS, stock assessment, Excel export, plot files) that
## pipeline_documentation_shiny.qmd's cache doesn't need at all. Lets that
## document `source()` this script as an automatic first-run cache build
## (see its own setup chunk) without waiting for - or writing the side
## effects of - a full pipeline run just to get the one object it actually
## reads. The exit itself is a custom condition (class "shinyCacheStop"),
## not a plain stop() - caught cleanly by pipeline_documentation_shiny.qmd's
## tryCatch, not reported as an error, and harmless (raises a normal,
## visible error instead) if this script is ever source()'d some other way
## without that handler in place.
message("[01_survey_density] Region/year config in effect: AREA_MODE = ", AREA_MODE,
        " | FILTER_AREAS = ", if (is.null(FILTER_AREAS)) "(derived from the custom area below)" else paste(FILTER_AREAS, collapse=","),
        " | YEAR_ECOPATH = ", paste(range(YEAR_ECOPATH), collapse="-"),
        " | TS_YEARS = ", paste(range(TS_YEARS), collapse="-"))

## OUTLIER_METHOD: which sample-level outlier removal rule actually
## runs (see remove_sample_outliers_multi()'s own header comment for
## the full description of each method):
##  "medits" (default) - Tukey/boxplot IQR fences (1.5xIQR beyond
##      Q1/Q3), applied per (AreaID, ScientificName) on log1p(Density).
##      This automates the same box-plot convention RoME (the MEDITS-
##      community QC package) draws in its own check_abundance() check
##      for screening abundance/density indices - RoME's docs don't
##      publish that as a numeric auto-removal rule (it's meant for a
##      person to eyeball the plot), so this turns that same visual
##      convention into an automatic rule rather than reproducing a
##      published MEDITS threshold that doesn't exist for density data.
##  "mad" - this pipeline's original single robust-z method (median/MAD,
##      threshold=70) - see remove_sample_outliers()'s own header
##      comment for the calibration story. Not a MEDITS/RoME method.
##  "percentile" - the 5th-95th percentile-band method RoME's
##      check_weight() uses for its own official QC check - but that
##      check is defined for mean individual weight per haul, not
##      Density, so this is an adaptation of the method to density, not
##      a literal reproduction of RoME's own published numeric bands
##      (those come from an external 2012-2022 reference dataset baked
##      into that package, not exposed as a reusable table from R).
##  "multi" - runs more than one method at once and combines them via
##      OUTLIER_CONSENSUS ("all"/"any"/"majority"); set
##      OUTLIER_MULTI_METHODS below to whichever combination you want.
if (!exists("OUTLIER_METHOD",        envir = .GlobalEnv, inherits = FALSE)) OUTLIER_METHOD        <- "medits"            # "medits" (=boxplot/Tukey), "mad", "percentile", or "multi"
if (!exists("OUTLIER_CONSENSUS",     envir = .GlobalEnv, inherits = FALSE)) OUTLIER_CONSENSUS     <- "all"               # only used when OUTLIER_METHOD == "multi": "all"/"any"/"majority"
if (!exists("OUTLIER_MULTI_METHODS", envir = .GlobalEnv, inherits = FALSE)) OUTLIER_MULTI_METHODS <- c("mad", "percentile", "boxplot")  # only used when OUTLIER_METHOD == "multi"

## Species presence check - a character VECTOR of ScientificName(s) to
## print, per species, how many distinct hauls caught it in each year
## of the full 1994-2023 MEDITS record (not just TS_YEARS), or NULL to
## skip entirely. Example below checks two Lessepsian/invasive species
## whose presence-by-year (and how many hauls, not just which years) is
## itself often the question, not just their density where present.
if (!exists("SPECIES_PRESENCE_CHECK", envir = .GlobalEnv, inherits = FALSE)) SPECIES_PRESENCE_CHECK <- c("Pterois miles", "Siganus luridus")    # NULL to skip

## Persistent on-disk cache for FishBase/SeaLifeBase taxonomy lookups
## (see fetch_taxonomy_fishbase()'s own header comment in
## lib_survey_fg_density_functions.R for the full rationale) - species
## already resolved on a PREVIOUS run of this script are read from this
## file instead of re-queried, which is what actually fixes the
## taxonomy-fallback step "taking forever" on every re-run during normal
## development/debugging. Set to NULL to disable caching entirely
## (always query fresh, the old behavior). Delete this file if you
## suspect it's stale and want a clean re-query.
## Switched from WoRMS to FishBase/SeaLifeBase (2026-09) so this
## script's species taxonomy matches 02_fisheries.R and
## 03_pbqb-traits.R's own FishBase-based taxonomy instead of a third,
## independent authority - taxonomy_source = "worms" is still supported
## below (lib_worms_taxonomy_lookup.R is still sourced above) for a
## species FishBase/SeaLifeBase genuinely lacks.
FISHBASE_TAXONOMY_CACHE_PATH <- file.path(out_dir, "fishbase_taxonomy_cache.rds")

## taxonomy_source = "both": FishBase/SeaLifeBase is fish/aquatic-
## organism-focused and doesn't carry every algae/sponge/bryozoan/
## echinoderm/crustacean name in the survey data (e.g. Osmundaria
## volubilis, Anamathia rissoana, Ergasticus clouei, Lissa chiragra came
## back with zero taxonomy from it alone). WoRMS covers the full marine
## taxonomic tree, including bare higher-rank names (e.g. "Porifera" is
## itself a valid WoRMS phylum-rank record), so it's a SECOND-PASS
## fallback for whatever FishBase/SeaLifeBase left with no rank filled
## in; FishBase/SeaLifeBase stays the first authority wherever it resolves a name.
WORMS_TAXONOMY_CACHE_PATH <- file.path(out_dir, "worms_taxonomy_cache.rds")

## OPT-IN, default FALSE - see lib_aquamaps_depth_extension.R's own header
## for the full rationale/assumption/caveats. MEDITS_STRATA below only
## covers 10-800m; this adds two DERIVED pseudo-strata (0-10m, 800-6000m)
## using AquaMaps depth-envelope ratios scaled against each species' own
## real MEDITS-measured density, folded into the SAME weight_by_strata()/
## weight_species_by_area() calculation as the 5 real strata. Left FALSE,
## behavior is byte-identical to before this existed. Turning it on
## changes fg_index/fg_index_regional/species_density_regional - i.e. the
## actual Biomass numbers written to the workbook - so review the
## AquaMaps_Depth_Adjustment audit sheet (written when this is TRUE)
## before trusting the adjusted figures for anything downstream. Requires
## the `aquamapsdata` package's local database (~2GB/~10GB unpacked,
## downloaded once, cached after that - see ensure_aquamaps_db()).
if (!exists("APPLY_AQUAMAPS_DEPTH_ADJUSTMENT", envir = .GlobalEnv, inherits = FALSE)) APPLY_AQUAMAPS_DEPTH_ADJUSTMENT <- TRUE

message(
  "This script will run for area(s): ", if (is.null(FILTER_AREAS)) "(derived from the custom area below)" else paste(FILTER_AREAS, collapse = ", "),
  "\nfor Ecopath years: ", paste(range(YEAR_ECOPATH), collapse = "-"),
  "\nfor Ecosim years: ", paste(range(TS_YEARS), collapse = "-")
)

## MEDITS' 5 standard bathymetric strata - passed as strata_def to the
## shared functions, not hardcoded inside them. Bounds: 10-50, 51-100,
## 101-200, 201-500, 501-800m. assign_depth_stratum() calls findInterval()
## on depth_min only, so depth_min is set one unit ABOVE each printed
## lower bound (51, 101, 201, 501) to push a boundary depth (e.g. Depth
## == 50) into the shallower stratum, matching the spec's wording exactly.
MEDITS_STRATA <- read_medits_reference("medits_strata.csv",
                                       required_cols = c("stratum_num", "depth_min", "depth_max"))

## Area definition - branches on AREA_MODE. Both branches end with the
## same three things defined: area_shp (an sf polygon set), AREA_ID_COL
## (the column in area_shp identifying each polygon), and FILTER_AREAS
## (which values of that column are actually in scope) - everything
## downstream of here reads only those three, never AREA_MODE itself.
download_gfcm_gsa_shapefile <- function() {
  ## GFCM GSA shapefile - shared by AREA_MODE == "westmed" and by
  ## AREA_MODE == "custom" + CUSTOM_AREA_TYPE == "gsa" (a custom GSA
  ## grouping still starts from these same official polygons).
  ## The shapefile lives under out_dir's own "shapefiles" subfolder
  ## (out_dir/shapefiles/GFCM_GSA_shp/GFCM_GSA/gfcm_gsa.shp) - that exact
  ## file is read whenever it's already there, so a shapefile placed or
  ## kept at that path is always the one used; it's only downloaded and
  ## unzipped into that same "shapefiles" subfolder when missing.
  gsa_zip_url  <- "https://gfcmsitestorage.blob.core.windows.net/website/5.Data/ArcGIS/GFCM_GSA.zip"
  gsa_shp_dir  <- file.path(out_dir, "shapefiles", "GFCM_GSA_shp")
  gsa_zip_file <- file.path(out_dir, "shapefiles", "GFCM_GSA.zip")
  if (!dir.exists(gsa_shp_dir) || length(list.files(gsa_shp_dir, pattern = "\\.shp$", recursive = TRUE)) == 0) {
    if (!file.exists(gsa_zip_file)) {
      dir.create(dirname(gsa_zip_file), recursive = TRUE, showWarnings = FALSE)
      download.file(gsa_zip_url, destfile = gsa_zip_file, mode = "wb", method = "libcurl")
    }
    unzip(gsa_zip_file, exdir = gsa_shp_dir)
  }
  gsa_shp_path <- list.files(gsa_shp_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)[1]
  shp <- st_read(gsa_shp_path, quiet = TRUE)
  shp$gsa_num <- as.numeric(shp$SMU_CODE)
  shp$gsa_num[shp$gsa_num %in% c(111, 112)] <- 11   # W/E Sardinia fix, same as the map script
  shp
}

if (AREA_MODE == "westmed") {
  area_shp <- download_gfcm_gsa_shapefile()
  AREA_ID_COL <- "gsa_num"
} else {
  ## AREA_MODE == "custom" - see CUSTOM_AREA_TYPE's own comment above
  ## for what each of these three does.
  if (CUSTOM_AREA_TYPE == "gsa") {
    area_shp <- download_gfcm_gsa_shapefile()
    area_shp <- area_shp[area_shp$gsa_num %in% CUSTOM_GSA_IDS, ]
    area_shp$area_id <- area_shp$gsa_num
  } else if (CUSTOM_AREA_TYPE == "bbox") {
    custom_poly <- sf::st_as_sfc(sf::st_bbox(
      c(xmin = CUSTOM_BBOX["xmin"], xmax = CUSTOM_BBOX["xmax"],
        ymin = CUSTOM_BBOX["ymin"], ymax = CUSTOM_BBOX["ymax"]),
      crs = 4326
    ))
    area_shp <- sf::st_sf(area_id = 1, geometry = custom_poly)
  } else {
    ## "shapefile"
    if (!file.exists(CUSTOM_SHAPEFILE_PATH)) {
      stop("CUSTOM_SHAPEFILE_PATH ('", CUSTOM_SHAPEFILE_PATH, "') does not exist - set it to your actual boundary shapefile.")
    }
    area_shp <- sf::st_read(CUSTOM_SHAPEFILE_PATH, quiet = TRUE)
    if (is.na(sf::st_crs(area_shp))) {
      message("CUSTOM_SHAPEFILE_PATH has no CRS set - assuming WGS84 (EPSG:4326).")
      sf::st_crs(area_shp) <- 4326
    } else if (sf::st_crs(area_shp) != sf::st_crs(4326)) {
      area_shp <- sf::st_transform(area_shp, 4326)
    }
    if (!"area_id" %in% names(area_shp)) area_shp$area_id <- seq_len(nrow(area_shp))
  }
  AREA_ID_COL <- "area_id"
  ## Everything in area_shp, since it's already the custom boundary -
  ## unless a driver script pre-set FILTER_AREAS to a subset of that.
  if (is.null(FILTER_AREAS)) FILTER_AREAS <- sort(unique(area_shp[[AREA_ID_COL]]))
}

## =================================================================
## STEP 2: Input data - MEDITS-specific loading, reshaped into the
## standardized dataframe1/dataframe2 format
## =================================================================
## --- dataframe2: species -> FG reference ------------------------------------
## fread(), not readxl::read_excel() - fg_species_file is the 2026 reviewed
## CSV (species/FG_number/FG_name/taxonomy/source/status), not the old
## FG_WMed.xlsx sheet 4 (ESPECIE/GF/FG_name). Column names differ accordingly:
## species -> ScientificName, FG_number -> FG_num, same FG_name either way.
fg_raw <- fread(fg_species_file)
dataframe2 <- unique(fg_raw[, .(ScientificName = species, FG_num = FG_number, FG_name)])

## --- dataframe1: MEDITS' TA.csv (samples/hauls) + TB.csv (catch) -----------
ta <- read_csv(file.path(pcloud_dir,"data/Medits_Medias_JRC2026/2024_MEDBSsurvey/Demersal/TA.csv"), show_col_types = FALSE)

## TS_YEARS' default end year is the actual last year present in TA.csv,
## not a hardcoded literal - only when TS_YEARS wasn't already pre-set
## by the calling environment. min(TS_YEARS) (1995, the Ecosim-output
## start) is left as-is - only the end year is derived from the data.
if (!TS_YEARS_WAS_PRESET) {
  medits_last_year <- max(ta$year, na.rm = TRUE)
  TS_YEARS <- min(TS_YEARS):medits_last_year
  message("[TS_YEARS] Not pre-set - defaulted to ", min(TS_YEARS), ":", medits_last_year,
          " (end year = the actual last year present in TA.csv).")
}

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
  AreaID = as.numeric(area),        # MEDITS already has GSA directly - overridden below
  # for a genuinely custom (non-GSA) boundary; otherwise also carried
  # through for plot_sample_map() below
  Depth = mean_depth,
  SampleID = SampleID,
  Effort = swept_area_km2,
  Lat = shooting_latitude,
  Lon = shooting_longitude
)]

if (AREA_MODE == "custom" && CUSTOM_AREA_TYPE %in% c("bbox", "shapefile")) {
  ## No GSA code applies to an arbitrary bbox/shapefile boundary (unlike
  ## CUSTOM_AREA_TYPE == "gsa", which is still a subset of real, named
  ## GSAs and keeps MEDITS' own area code above) - every sample starts
  ## at AreaID = 1, and filter_samples_by_area() right below (a spatial
  ## join on Lat/Lon, since there's no GSA code to filter on) keeps only
  ## the ones actually inside the custom boundary.
  dataframe1[, AreaID := 1]
  dataframe1 <- filter_samples_by_area(
    dataframe1,
    area_filter = if (CUSTOM_AREA_TYPE == "bbox") CUSTOM_BBOX else area_shp,
    filter_type = CUSTOM_AREA_TYPE
  )
}

message("dataframe1 built: ", nrow(dataframe1), " observations from ",
        uniqueN(dataframe1$SampleID), " samples.")

validate_survey_data(dataframe1, dataframe2, strata = STRATA)

## Sample coverage diagnostic - worth looking at before the rest of the
## pipeline, e.g. to spot a gap in survey coverage or samples plotting
## outside their expected area boundary. selected_areas highlights the
## GSAs actually in FILTER_AREAS (this analysis's scope) with a thicker
## blue border, distinguishing them from other GSAs shown on the map
## but outside the analysis.
## land_on_top: TRUE only for a plain bbox boundary (CUSTOM_AREA_TYPE ==
## "bbox"), where area_shp is a rectangle with no real coastline of its
## own - drawing land last keeps the coast readable over the bbox fill
## instead of the bbox rectangle hiding it. A GSA polygon (westmed, or
## custom + "gsa") and a real custom shapefile already follow the coast,
## so land draws first there (the default, FALSE) as usual.
map_land_on_top <- (AREA_MODE == "custom" && CUSTOM_AREA_TYPE == "bbox")

p_sample_map <- plot_sample_map(dataframe1, area_shp, area_id_col = AREA_ID_COL,
                                title = "MEDITS sample coverage by GSA", selected_areas = FILTER_AREAS,
                                land_on_top = map_land_on_top)
ggsave(file.path(plot_dir, "survey_sample_coverage_map.png"), p_sample_map, width = 10, height = 8, dpi = 150, bg = "white")

## same coverage map, faceted by year - useful for spotting a specific
## year with a coverage gap that the all-years-combined map above would
## mask (a gap in one year can look fine once every other year's
## samples are overlaid on top of it)
p_sample_map_byyear <- plot_sample_map(dataframe1, area_shp, area_id_col = AREA_ID_COL,
                                       title = "MEDITS sample coverage by GSA and year", by_year = TRUE,
                                       selected_areas = FILTER_AREAS, land_on_top = map_land_on_top)
ggsave(file.path(plot_dir, "survey_sample_coverage_map_by_year.png"), p_sample_map_byyear,
       width = 16, height = 12, dpi = 150, bg = "white")

## =================================================================
## STEP 3: Scientific name -> FG (direct match)
## =================================================================
fg_lookup_safe <- prepare_fg_lookup(dataframe2)
dt <- match_species_to_fg(dataframe1, fg_lookup_safe)
dt <- match_nominate_subspecies_fg(dt, fg_lookup_safe)

## --- MEDITS-specific manual overrides ---------------------------------------
## Species codes not in the MEDITS taxonomic list at all - specific to
## this survey's code list and this FG reference scheme, so it stays
## here rather than in the shared library.
MANUAL_OVERRIDES <- read_medits_reference("medits_manual_overrides.csv",
                                          required_cols = c("species_code", "taxon_name", "taxon_rank"))

## needs taxonomy on fg_lookup_safe to resolve genus/family/class-rank overrides
fg_taxonomy <- fetch_taxonomy(fg_lookup_safe$ScientificName, taxonomy_source = "both", cache_path = FISHBASE_TAXONOMY_CACHE_PATH, worms_cache_path = WORMS_TAXONOMY_CACHE_PATH)
fg_lookup_safe <- merge(fg_lookup_safe, fg_taxonomy, by = "ScientificName", all.x = TRUE)

resolve_override <- function(taxon_name, rank) {
  rank_col <- switch(rank, species = "ScientificName", genus = "Genus", family = "Family", class = "Class")
  matches <- unique(fg_lookup_safe[get(rank_col) == taxon_name, .(FG_num, FG_name)])
  if (nrow(matches) != 1) return(data.table(FG_num = NA_real_, FG_name = NA_character_))
  ## FG_num is as.numeric()'d here because fg_lookup_safe$FG_num can come
  ## through as integer (e.g. from fread()'s type-guessing on
  ## FG_WMed_2026.csv), while the no-match branch above returns NA_real_
  ## (double). MANUAL_OVERRIDES[, cbind(resolve_override(...)), by =
  ## species_code] requires every group's result to have the same column
  ## type, so a mix of integer/double across groups would throw "Column 1
  ## of result for group N is type 'integer' but expecting type 'double'."
  matches[, FG_num := as.numeric(FG_num)]
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
## (an upstream column-concatenation artifact), egg-capsule entries, and
## three more confirmed non-taxon entries flagged after inspecting real
## fallback output: "Sea ball of Posidonia oceanica" (a detritus/plant-
## debris category, not an animal), "shell debris", and "Leaves of
## Posidonia oceanica" - none of these are real taxa and would just
## waste a WoRMS query for nothing (or, worse, get force-matched to an
## unrelated FG by the bare-word/genus fallback below); setting
## ScientificName to NA here only affects the fallback match attempt,
## not the underlying observation rows themselves.
## str_detect() on an already-NA ScientificName returns NA (not FALSE),
## so non_taxon itself legitimately contains NAs wherever ScientificName
## was already NA - data.table's `dt[non_taxon, ...]` already treats
## those NA entries as "not selected" (same as FALSE), so the exclusion
## logic here was always correct; only sum(non_taxon) below needs na.rm
## (NA propagates through sum() without it, printing "NA" instead of a
## real count).
NON_TAXON_LITERAL_NAMES <- c("Sea ball of Posidonia oceanica", "shell debris", "Leaves of Posidonia oceanica")

## A "NO Genus species" ScientificName is NOT a non-taxon artifact to
## exclude - "NO" is MEDITS' own code meaning "not identified to species",
## prefixed onto the genus-level name that WAS identified (e.g.
## "NO Mullus" -> genus Mullus, species unidentified). The "NO " prefix
## is stripped here and the real genus-level name underneath is kept and
## sent through fallback_match_fg_by_taxonomy() like any other row,
## rather than being excluded.
no_prefixed <- str_detect(dt$ScientificName, "^NO\\b")
dt[no_prefixed & !is.na(ScientificName), ScientificName := str_trim(str_remove(ScientificName, "^NO\\b"))]
message(sum(no_prefixed, na.rm = TRUE), " 'NO '-prefixed row(s) had the prefix stripped and the underlying",
        " genus-level name kept for the taxonomy fallback attempt.")

non_taxon <- str_detect(dt$ScientificName, regex("eggs?", ignore_case = TRUE)) |
  (dt$ScientificName %in% NON_TAXON_LITERAL_NAMES)
dt[non_taxon, ScientificName := NA_character_]
message(sum(non_taxon, na.rm = TRUE), " non-taxon row(s) (egg-capsule, or a literal non-taxon",
        " name - Sea ball/Leaves of Posidonia oceanica, shell debris) excluded from the taxonomy fallback attempt.")

dt <- fallback_match_fg_by_taxonomy(dt, fg_lookup_safe, taxonomy_source = "both", cache_path = FISHBASE_TAXONOMY_CACHE_PATH, worms_cache_path = WORMS_TAXONOMY_CACHE_PATH)

## --- Seed FG rules: REMOVED ENTIRELY ------------------------
## The 4 rules that survived the previous re-audit (Holothuroidea ->
## Sea cucumbers, Bivalvia -> Bivalves, Gastropoda -> Gastropods,
## Echinoidea -> Other sea urchins) were kept on the reasoning that
## those target FGs have zero species pre-listed in FG_WMed_2026.csv,
## so the automatic taxonomy fallback has nothing to learn an exclusive
## mapping from. That's true, but it means those 4 rules were still
## inventing a Class -> FG assignment with NO support anywhere in
## FG_WMed_2026.csv - exactly the kind of code-side manual override
## this pipeline has been moving away from (FG_WMed_2026.csv as the
## single source of truth for species -> FG, not a rule baked into the
## R script). Removed rather than kept "just in case": a species that
## would have been seeded this way now surfaces honestly as unresolved
## (survey_unmatched_for_manual_review.csv below), which is the correct
## signal that FG_WMed_2026.csv itself is missing an example species for
## that FG - the fix belongs in the CSV, not in another code-side rule.
##
## SPECIES_EXCEPTIONS (Squilla mantis -> "Other commercial decapods") is
## ALSO removed: it's already dead code as of the current
## FG_WMed_2026.csv - Squilla mantis is listed there BY NAME under
## "Other commercial decapods" (FG 65), so it resolves via the ordinary
## direct scientific-name match at STEP 3 above, before this code ever
## ran. It only existed to guard against the old "Order Stomatopoda ->
## Non-commercial decapods" seed rule, which is also gone now (Non-
## commercial decapods already has 200 real species to learn from). No
## manual override is needed for it any more.
##
## Net effect: dt keeps exactly what fallback_match_fg_by_taxonomy()
## resolved above (direct match, then genus/family/order/class taxonomy
## inference, all learned from FG_WMed_2026.csv's own real rows) - no
## further code-side assignment happens here.

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
still_unmatched[, Survey := "MEDITS"]
fwrite(still_unmatched, file.path(csv_out_dir, "survey_unmatched_for_manual_review.csv"))
message("Saved to survey_unmatched_for_manual_review.csv for review.")

## Audit trail for every species assigned by the taxonomy fallback
## (fallback_match_fg_by_taxonomy()) - both the "exclusive" ones (every
## already-assigned relative at that rank agrees on one FG) and the
## "majority" ones (that rank was ambiguous - assigned to whichever FG
## holds the most already-assigned relatives, per the instruction
## that an unmatched species can be placed by its closest relative even
## when the genus/family itself isn't exclusive to one FG). vote_share
## (e.g. "4/5") shows exactly how strong that majority was, so a review
## can spot a thin 2/3 majority vs. an overwhelming 9/10 one.
fallback_detail <- attr(dt, "fallback_match_detail")
if (!is.null(fallback_detail) && nrow(fallback_detail) > 0) {
  fallback_detail[, Survey := "MEDITS"]
  fwrite(fallback_detail, file.path(csv_out_dir, "taxonomy_fallback_matches_for_review.csv"))
  message("Saved to taxonomy_fallback_matches_for_review.csv (", nrow(fallback_detail), " species assigned via",
          " taxonomy fallback - ", fallback_detail[match_type == "majority", .N], " of those by majority vote,",
          " ", fallback_detail[match_type == "exclusive", .N], " by unanimous agreement among relatives).")
}

message("\nFinal FG match rate: ", dt[!is.na(FG_num), uniqueN(ScientificName)], " of ",
        dt[, uniqueN(ScientificName)], " distinct species matched.")

## Species presence check (SPECIES_PRESENCE_CHECK, set above) - for
## each species, how many DISTINCT HAULS (SampleID) actually caught it
## in each year, checked against TS_YEARS (whose own end year is derived
## from the real MEDITS data when not pre-set, see the TS_YEARS default
## fix above). Deliberately BEFORE outlier removal/catchability correction below,
## since this is about whether/how often the species was ever caught at
## all, not about its corrected density. Checked on Biomass (dt's own
## raw-catch column at this point in the pipeline - Density doesn't
## exist yet, it's only computed later on the separate FG_spp_Ecopath
## table), and > 0 (not just !is.na) specifically - some survey formats
## carry an explicit zero-catch row per haul per species checked for,
## which would otherwise inflate "presence" to mean "was checked for"
## rather than "was caught".
if (!is.null(SPECIES_PRESENCE_CHECK)) {
  ## Uses TS_YEARS instead of a hardcoded literal - TS_YEARS' own end
  ## year is derived from the real MEDITS data too (see the TS_YEARS
  ## default fix right after `ta` is read, above), so this stays in sync
  ## with it automatically.
  full_survey_years <- TS_YEARS
  for (sp in SPECIES_PRESENCE_CHECK) {
    hauls_by_year <- dt[ScientificName == sp & !is.na(Biomass) & Biomass > 0,
                        .(n_hauls_present = uniqueN(SampleID)), by = Year][order(Year)]
    yrs_missing <- setdiff(full_survey_years, hauls_by_year$Year)
    if (nrow(hauls_by_year) == 0) {
      message("\n[Species check] '", sp, "' NOT found in dt at all across ",
              min(full_survey_years), "-", max(full_survey_years), ".")
    } else {
      message("\n[Species check] '", sp, "' present in ", nrow(hauls_by_year), " of ",
              length(full_survey_years), " year(s), ", sum(hauls_by_year$n_hauls_present),
              " haul(s) total across the time series. Hauls with presence, by year:")
      print(hauls_by_year)
      message("Absent (zero hauls) in: ",
              if (length(yrs_missing) == 0) "none" else paste(yrs_missing, collapse = ", "), ".")
    }
  }
}

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
  gap_taxonomy <- fetch_taxonomy(still_missing_taxonomy, taxonomy_source = "both", cache_path = FISHBASE_TAXONOMY_CACHE_PATH, worms_cache_path = WORMS_TAXONOMY_CACHE_PATH)
  species_taxonomy <- rbindlist(list(species_taxonomy, gap_taxonomy), fill = TRUE)
  species_taxonomy <- unique(species_taxonomy, by = "ScientificName")
}

## Full-extent MEDITS species snapshot for build_full_fg_species_catalog()
## (called once near the end of this script) - taken HERE, deliberately
## before Step 5's `dt <- dt[AreaID %in% FILTER_AREAS]` a few lines down,
## so it covers every species anywhere in the raw MEDITS file(s) this run
## loaded, not just whatever GSAs FILTER_AREAS happens to be set to.
snapshot_full_extent_species(dt, species_taxonomy, "MEDITS", csv_out_dir)

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
CATCHABILITY_CSV_PATH <- resolve_pcloud_file(paste0(pcloud_dir,"/data/catchability_factors_ecotrans_medits_2021_spp.csv"), pcloud_dir, required = FALSE)

## Pelagic/planktonic/seagrass/algae FGs get q=1 (no catchability
## correction) regardless of anything catchability_table or the
## taxonomic-proximity fallback would otherwise resolve - a bottom trawl
## survey isn't designed to sample these representatively at all, so
## "catchability" as a concept doesn't apply the same way.
## Per project decision, zooplankton and suprabenthos can't use survey
## biomass and need other sources. EXEMPT_FG_NAMES is DERIVED from
## FG_WMed_2026.csv itself (fg_raw, read at STEP 2 above) if that file
## carries a "survey_exempt" column (Y/N per species row, same place
## Genus/Family/.../status already live), rather than hand-typed here -
## add/remove the flag there, not in this script. A FG counts as exempt
## only if EVERY one of its listed species is flagged (so a mixed FG
## never gets silently exempted by one stray flag).
if ("survey_exempt" %in% names(fg_raw)) {
  fg_raw_flag <- toupper(trimws(as.character(fg_raw$survey_exempt))) %chin% c("Y", "YES", "TRUE", "1")
  fg_exempt_share <- data.table(FG_name = fg_raw$FG_name, flag = fg_raw_flag)[, .(share = mean(flag, na.rm = TRUE)), by = FG_name]
  EXEMPT_FG_NAMES <- fg_exempt_share[share == 1, FG_name]
  message("EXEMPT_FG_NAMES derived from FG_WMed_2026.csv's 'survey_exempt' column: ",
          length(EXEMPT_FG_NAMES), " FG(s) - ", paste(EXEMPT_FG_NAMES, collapse = ", "))
} else {
  ## Fallback only: FG_WMed_2026.csv doesn't have the survey_exempt column
  ## yet, so fall back to the previously reviewed name list below rather
  ## than silently exempting nothing - but flag this loudly, since this
  ## branch is meant to be temporary (add the column to the CSV to retire
  ## it for good).
  message("[EXEMPT_FG_NAMES] FG_WMed_2026.csv has no 'survey_exempt' column yet - falling back to",
          " the previously reviewed hardcoded FG-name list. Add a Y/N 'survey_exempt' column to",
          " FG_WMed_2026.csv (one value per species row) to make this fully data-driven and remove",
          " this fallback for good.")
  EXEMPT_FG_NAMES <- c(
    "Cymodocea", "Posidonia", "Macroalgae", "Corals and gorgonians",
    "Macro zooplankton", "Meso and micro zooplankton", "Suprabenthos"
  )
}

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
## dt_before_outliers kept specifically for plot_outlier_diagnostic()
## below, which needs the REAL Density values for flagged rows (already
## NA'd out of dt itself once removed).
dt_before_outliers <- copy(dt)

dt <- if (identical(OUTLIER_METHOD, "medits")) {
  ## boxplot/Tukey IQR only - see OUTLIER_METHOD's own comment above for
  ## why this (not "mad") is the default: it's the method RoME's own
  ## check_abundance() convention uses for density/abundance screening.
  remove_sample_outliers_multi(dt, methods = "boxplot", drop_outliers = DROP_OUTLIERS)
} else if (identical(OUTLIER_METHOD, "percentile")) {
  remove_sample_outliers_multi(dt, methods = "percentile", drop_outliers = DROP_OUTLIERS)
} else if (identical(OUTLIER_METHOD, "multi")) {
  remove_sample_outliers_multi(dt, methods = OUTLIER_MULTI_METHODS,
                               consensus = OUTLIER_CONSENSUS, drop_outliers = DROP_OUTLIERS)
} else {
  remove_sample_outliers(dt, threshold = 70, min_samples = 5, drop_outliers = DROP_OUTLIERS)
}

## save right here, before dt moves on to compute_fg_densities_by_stratum()
## etc. below - those functions weren't designed to know about or
## preserve the flagged_outliers attribute, so it needs to be pulled
## off now rather than later.
flagged_outliers <- attr(dt, "flagged_outliers")
if (!is.null(flagged_outliers) && nrow(flagged_outliers) > 0) {
  fwrite(flagged_outliers, file.path(csv_out_dir, "survey_outliers_flagged.csv"))
  message("Saved ", nrow(flagged_outliers), " flagged outlier(s) to survey_outliers_flagged.csv",
          " (includes a 'was_dropped' column - TRUE if actually removed, FALSE if only reported).")
  
  ## diagnostic plot - box-plot per flagged species with the flagged
  ## points overlaid, the same visual convention RoME's check_abundance()
  ## uses for MEDITS QC screening (see plot_outlier_diagnostic()'s own
  ## header comment).
  p_outliers <- plot_outlier_diagnostic(dt_before_outliers, flagged_outliers)
  ggsave(file.path(plot_dir, "survey_outlier_diagnostic.png"), p_outliers,
         width = 10, height = 7, dpi = 150, bg = "white")
}
rm(dt_before_outliers)

## Opt-in Shiny data cache (see SAVE_SHINY_CACHE_PATH's own comment near the
## top of this Configuration section) - snapshotted right here, since dt is
## now in its final per-sample form (FG-matched, area/stratum-assigned,
## catchability-corrected, outliers resolved) but hasn't yet been collapsed
## by compute_fg_densities_by_stratum()/weight_by_strata()/weight_by_area()
## below - exactly the shape pipeline_documentation_shiny.qmd's reactive
## code re-runs that same 3-step aggregation cascade against, per whatever
## year/area/strata the user picks interactively.
if (!is.null(SAVE_SHINY_CACHE_PATH)) {
  if (!dir.exists(dirname(SAVE_SHINY_CACHE_PATH))) {
    dir.create(dirname(SAVE_SHINY_CACHE_PATH), recursive = TRUE, showWarnings = FALSE)
  }
  saveRDS(
    list(
      dt              = dt,
      area_shp        = area_shp,
      area_id_col     = AREA_ID_COL,
      strata_def      = MEDITS_STRATA,
      species_taxonomy = if (exists("species_taxonomy", inherits = FALSE)) species_taxonomy else NULL,
      cached_at       = Sys.time(),
      filter_areas_at_cache_time = FILTER_AREAS,
      year_range_at_cache_time   = range(dt$Year, na.rm = TRUE)
    ),
    SAVE_SHINY_CACHE_PATH
  )
  message("Saved Shiny data cache to '", SAVE_SHINY_CACHE_PATH, "' (", nrow(dt), " MEDITS samples, ",
          "years ", paste(range(dt$Year, na.rm = TRUE), collapse = "-"), ", areas ",
          paste(sort(unique(dt$AreaID)), collapse = ","), ").")
  
  if (isTRUE(STOP_AFTER_SHINY_CACHE)) {
    message("STOP_AFTER_SHINY_CACHE is TRUE - stopping here now that the cache is saved, skipping",
            " Steps 6-13 (MEDIAS, stock assessment, Excel export, plot files) since",
            " pipeline_documentation_shiny.qmd's cache doesn't need any of them.")
    stop(structure(
      class = c("shinyCacheStop", "error", "condition"),
      list(message = "01_biomass.R stopped intentionally after saving the Shiny cache (STOP_AFTER_SHINY_CACHE = TRUE).",
           call = NULL)
    ))
  }
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
  cache_path = file.path(csv_out_dir, "strata_area_by_area.csv"),
  ## AreaID IS the real GSA number in "westmed" mode (AREA_ID_COL ==
  ## "gsa_num") and in "custom" + gsa mode - label the CSV's area column
  ## accordingly; a bbox/shapefile custom area has no real GSA, so it
  ## stays "AreaID" there.
  area_id_label = if (AREA_MODE == "westmed" || (AREA_MODE == "custom" && CUSTOM_AREA_TYPE == "gsa")) "GSA" else "AreaID")

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
## STEP 7b: AquaMaps shallow/deep depth adjustment (OPT-IN - see
## APPLY_AQUAMAPS_DEPTH_ADJUSTMENT above and lib_aquamaps_depth_
## extension.R's own header for the full rationale). Re-derives
## fg_index/fg_index_regional/species_density_regional from an
## EXTENDED strata set (the 5 real MEDITS strata + 3 AquaMaps-derived
## pseudo-strata: 0-10m shallow, plus the deep zone split into 800-1000m
## and 1000-2850m rather than one 800-2850m lump) using the SAME
## weight_by_strata()/weight_species_by_area() functions as above - not
## a separate formula.
## =================================================================
aquamaps_depth_adjustment_audit <- NULL
## Default for STEP 8's strata-profile plot - overridden below to the
## AquaMaps-extended 8-stratum definition when the adjustment is applied.
strata_def_for_plotting <- MEDITS_STRATA
## Default for the area-weight donut (plot_area_weight_donut()) -
## overridden below to aq$strata_area (the AquaMaps-extended 8-stratum
## area table, including the deep 800-1000m/1000-2850m pseudo-strata)
## when the adjustment is applied, same pairing as strata_def_for_plotting.
strata_area_by_area_for_plotting <- strata_area_by_area
if (isTRUE(APPLY_AQUAMAPS_DEPTH_ADJUSTMENT)) {
  if (!isTRUE(STRATA)) {
    stop("APPLY_AQUAMAPS_DEPTH_ADJUSTMENT requires STRATA <- TRUE - the pseudo-strata mechanism",
         " extends the strata-weighted path specifically; there's no equivalent for the",
         " unstratified simple_area_density() path.")
  }
  message("\n[AquaMaps] APPLY_AQUAMAPS_DEPTH_ADJUSTMENT is TRUE - extending fg_index/",
          "fg_index_regional/species_density_regional with AquaMaps-derived 0-10m/800-6000m",
          " pseudo-strata on top of the real MEDITS strata above...")
  source(file.path(git_dir, "scripts/lib_aquamaps_depth_extension.R"))
  aq <- apply_aquamaps_depth_adjustment(
    dt = dt, per_group_fg = per_group_fg, per_group_sp = per_group_sp,
    n_samples_by_stratum = n_samples_by_stratum,
    medits_strata_def = MEDITS_STRATA,
    area_ids = sort(unique(dt$AreaID[dt$AreaID %in% FILTER_AREAS])),
    area_shp = area_shp, area_id_col = AREA_ID_COL,
    cache_path = file.path(csv_out_dir, "strata_area_by_area_aquamaps_extended.csv"))
  
  ## Kept specifically to compute net_density_multiplier below - the
  ## REAL before/after effect, as opposed to depth_extrapolation_
  ## multiplier (lib_aquamaps_depth_extension.R's own column), which is
  ## only the per-species pseudo-strata add-on BEFORE area-reweighting.
  ## These two numbers can differ, sometimes a lot: folding the two
  ## pseudo-strata's AREA into the total also shrinks every REAL
  ## stratum's own area-proportion (prop = area_km2 / sum(area_km2),
  ## now summed over 7 strata instead of 5) - so a species can show a
  ## depth_extrapolation_multiplier > 1 (its pseudo-strata density IS
  ## higher than its reference density) while its net_density_multiplier
  ## still comes out < 1, if the real strata's own diluted contribution
  ## drops by more than the pseudo-strata add - i.e. a genuine "1.2" or
  ## "0.3"-type outcome depends on both effects together, not just the
  ## depth-envelope ratio in isolation.
  species_density_regional_unadjusted <- copy(species_density_regional)
  
  fg_index <- weight_by_strata(aq$per_group_fg, aq$n_samples_by_stratum, aq$strata_area)
  fg_index_regional <- weight_by_area(fg_index)
  species_density_regional <- weight_species_by_area(aq$per_group_sp, aq$n_samples_by_stratum, aq$strata_area)
  aquamaps_depth_adjustment_audit <- aq$audit
  
  ## fg_index's own per_stratum attribute now spans all 8 strata (the
  ## 3 AquaMaps pseudo-strata - shallow 0-10m, deep 800-1000m, deep
  ## 1000-2850m - plus the 5 real MEDITS ones) - the STEP 8 strata-
  ## profile plot needs THIS extended definition, not the plain 5-row
  ## MEDITS_STRATA, or the 3 pseudo-strata bars would merge to
  ## "unknown depth range" instead of a real m label.
  strata_def_for_plotting <- aq$strata_def
  strata_area_by_area_for_plotting <- aq$strata_area
  
  ## net_density_multiplier: species_density_regional's own mean_density
  ## (t/km^2 - same unit as every other Biomass/density figure in this
  ## pipeline), AFTER divided by BEFORE, averaged over YEAR_ECOPATH
  ## (this pipeline's baseline period for Biomass figures) - this is
  ## the actual "multiplied by 1.2" or "multiplied by 0.3" number.
  before_after <- merge(
    species_density_regional_unadjusted[Year %in% YEAR_ECOPATH, .(ScientificName, Year, density_before = mean_density)],
    species_density_regional[Year %in% YEAR_ECOPATH, .(ScientificName, Year, density_after = mean_density)],
    by = c("ScientificName", "Year"), all = TRUE
  )
  net_multiplier_by_species <- before_after[
    , .(density_before = mean(density_before, na.rm = TRUE), density_after = mean(density_after, na.rm = TRUE)),
    by = ScientificName
  ]
  net_multiplier_by_species[, net_density_multiplier := ifelse(
    !is.na(density_before) & density_before > 0, density_after / density_before, NA_real_)]
  aquamaps_depth_adjustment_audit <- merge(
    aquamaps_depth_adjustment_audit, net_multiplier_by_species, by = "ScientificName", all.x = TRUE)
  
  message("[AquaMaps] Done - ", aquamaps_depth_adjustment_audit[note == "adjusted", .N], " of ",
          nrow(aquamaps_depth_adjustment_audit), " species actually got a shallow/deep adjustment",
          " (see the 'note' column in AquaMaps_Depth_Adjustment for why the rest didn't; the",
          " 'net_density_multiplier' column is each species' own YEAR_ECOPATH-averaged density,",
          " AFTER divided by BEFORE this adjustment - the actual real-world 'x1.2'/'x0.3' figure,",
          " which can differ from depth_extrapolation_multiplier - see that column's own comment).")
}

## =================================================================
## STEP 8: plots (MEDITS)
## =================================================================
p_by_area <- plot_fg_timeseries_by_area(
  fg_index, title = "MEDITS trawl survey by FG and area (strata-weighted)", y_lab = "Density (t/km^2)")
ggsave(file.path(plot_dir, "survey_fg_density_timeseries.png"), p_by_area, width = 14, height = 10, dpi = 150, bg = "white")

regional_title_area <- if (AREA_MODE == "westmed") "Western Med" else AREA_NAME
p_regional <- plot_fg_timeseries_regional(fg_index_regional,
                                          title = paste0("MEDITS trawl survey by FG, ", regional_title_area, " (area-weighted)"), y_lab = "Area-weighted density (t/km^2)")
ggsave(file.path(plot_dir, "survey_fg_density_timeseries_regional.png"), p_regional, width = 14, height = 10, dpi = 150, bg = "white")

## Same data as above, normalized so each FG's own series starts at 1
## (relative to its first non-zero value) - makes relative CHANGE over
## time comparable across FGs regardless of how different their
## absolute densities are.
p_regional_normalized <- plot_fg_timeseries_regional(
  fg_index_regional, title = paste0("MEDITS trawl survey by FG, ", regional_title_area, " (normalized)"),
  y_lab = "Area-weighted density (t/km^2)", normalize = TRUE)
ggsave(file.path(plot_dir, "survey_fg_density_timeseries_regional_normalized.png"), p_regional_normalized,
       width = 14, height = 10, dpi = 150, bg = "white")

fg_density_by_stratum <- NULL  # only built below when STRATA - see the Excel-write step further down
if (STRATA) {
  ## strata_def_for_plotting is MEDITS_STRATA (5 real strata) normally,
  ## or the AquaMaps-extended 8-stratum definition (3 pseudo-strata +
  ## 5 real) when APPLY_AQUAMAPS_DEPTH_ADJUSTMENT is TRUE - see STEP 7b.
  ## fg_index's own per_stratum attribute already reflects whichever set
  ## was actually used to build it, so this just keeps the plot's stratum
  ## labels in sync with it instead of merging against the wrong table.
  p_profile <- plot_strata_profile(fg_index, strata_def_for_plotting)
  ggsave(file.path(plot_dir, "survey_fg_depth_strata_profile.png"), p_profile, width = 16, height = 12, dpi = 150, bg = "white")
  
  ## Same "density by depth stratum" information as the plot above, but
  ## as an Excel sheet: one row per FG, one column per stratum (region-
  ## wide, area-weighted, YEAR_ECOPATH-averaged), plus Total_Density -
  ## which reproduces fg_index_regional's own mean_density exactly (see
  ## build_fg_density_by_stratum_sheet()'s own comment on why the columns
  ## are additive). Covers whichever strata are actually in play - the
  ## plain 5 real MEDITS strata, or all 8 once the AquaMaps pseudo-strata
  ## are folded in - same strata_def_for_plotting as the plot above.
  fg_density_by_stratum <- build_fg_density_by_stratum_sheet(
    fg_index, strata_def_for_plotting, year_filter = YEAR_ECOPATH)
  
  ## Area-weight composition (Stage 5's "% of area" weighting, made
  ## visible per area rather than buried in strata_area_by_area's own
  ## numbers) - one donut per area, sliced by depth stratum's prop.
  ## Capped to a readable number of areas via facet_wrap's own default;
  ## pass area_ids = <a few AreaIDs> below if FILTER_AREAS covers many
  ## areas and the full facet grid gets too small to read.
  ## Reads strata_area_by_area_for_plotting directly (the actual
  ## area-only table, AquaMaps-extended when that adjustment is active -
  ## see its own comment above) rather than deriving the area
  ## proportions from fg_index's catch-conditioned per_stratum attribute,
  ## which could drop the deepest pseudo-strata whenever nothing
  ## happened to be caught there for a given area/year; facets are
  ## labeled "GSA: <n>" when AreaID really is one.
  p_area_weights <- plot_area_weight_donut(
    strata_area_by_area_for_plotting, strata_def_for_plotting,
    area_id_label = if (AREA_MODE == "westmed" || (AREA_MODE == "custom" && CUSTOM_AREA_TYPE == "gsa")) "GSA" else "AreaID")
  ggsave(file.path(plot_dir, "survey_area_weight_donut.png"), p_area_weights,
         width = 14, height = 10, dpi = 150, bg = "white")
}

## =================================================================
## STEP 9: MEDIAS acoustic survey + GFCM stock-assessment biomass +
## the MEDITS/MEDIAS/stock-assessment priority combine. Every data
## source in this whole step is scoped to real, named GSAs (MEDIAS
## Handbook Table 1 areas, GFCM STAR/RAM Legacy species-by-GSA
## assessments, the "Western Mediterranean" subregion filter below) -
## none of it has a meaning for a custom bbox/shapefile boundary that
## doesn't follow GSA lines, so AREA_MODE == "custom" with
## CUSTOM_AREA_TYPE other than "gsa" still skips straight to the
## MEDITS-only else{} branch at the bottom of this step.
## AREA_MODE == "custom" + CUSTOM_AREA_TYPE == "gsa" IS a real-GSA
## boundary (just a subset of the full GSA 1-11, see CUSTOM_GSA_IDS), so
## this step also runs for that case, filtered to FILTER_AREAS exactly
## like MEDITS already is (see acoustic_fg_by_country_gsa's own
## `[AreaID %in% FILTER_AREAS]` below). The *_combined variables Step 10
## onward reads are aliased either way, so nothing downstream needs to
## know which branch actually ran.
## =================================================================
if (AREA_MODE == "westmed" || (AREA_MODE == "custom" && CUSTOM_AREA_TYPE == "gsa")) {
  
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
  asfis_file  <- resolve_pcloud_file(paste0(pcloud_dir, "/data/ASFIS_sp_2026.1.csv"), pcloud_dir)
  
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
  acoustic_matched <- fallback_match_fg_by_taxonomy(acoustic_matched, fg_lookup_safe, taxonomy_source = "both", cache_path = FISHBASE_TAXONOMY_CACHE_PATH, worms_cache_path = WORMS_TAXONOMY_CACHE_PATH)
  
  ## Same manual-review export MEDITS gets above - apply_seed_fg_rules()
  ## itself is gone (see the "Seed FG rules: REMOVED ENTIRELY" comment
  ## above dt's own matching block): acoustic_matched keeps exactly what
  ## fallback_match_fg_by_taxonomy() resolved just above, no further
  ## code-side assignment. summarize_unresolved_species()
  ## expects a `Biomass` column (MEDITS' own convention) - acoustic_matched
  ## carries the same value under `total_biomass`, aliased here rather than
  ## renamed so nothing downstream that expects `total_biomass` breaks.
  acoustic_taxonomy_context <- attr(acoustic_matched, "still_unresolved_taxonomy")
  acoustic_matched[, Biomass := total_biomass]
  medias_still_unmatched <- summarize_unresolved_species(acoustic_matched, taxonomy = acoustic_taxonomy_context)
  acoustic_matched[, Biomass := NULL]
  medias_still_unmatched[, Survey := "MEDIAS"]
  ## Consolidated with the MEDITS version of this same review file (same
  ## shape, same summarize_unresolved_species() output, just a different
  ## survey) into ONE survey_unmatched_for_manual_review.csv,
  ## distinguished by the Survey column above.
  fwrite(medias_still_unmatched, file.path(csv_out_dir, "survey_unmatched_for_manual_review.csv"), append = TRUE)
  message("Appended MEDIAS rows to survey_unmatched_for_manual_review.csv for review.")
  
  ## Same audit trail as the MEDITS side above (see its comment for the
  ## full rationale) - species assigned via the taxonomy fallback for the
  ## MEDIAS/acoustic data, split by match_type ("exclusive" vs "majority").
  acoustic_fallback_detail <- attr(acoustic_matched, "fallback_match_detail")
  if (!is.null(acoustic_fallback_detail) && nrow(acoustic_fallback_detail) > 0) {
    acoustic_fallback_detail[, Survey := "MEDIAS"]
    ## Consolidated with the MEDITS version (same shape) into ONE
    ## taxonomy_fallback_matches_for_review.csv, distinguished by Survey.
    fwrite(acoustic_fallback_detail, file.path(csv_out_dir, "taxonomy_fallback_matches_for_review.csv"), append = TRUE)
    message("Appended MEDIAS rows to taxonomy_fallback_matches_for_review.csv (", nrow(acoustic_fallback_detail),
            " species assigned via taxonomy fallback - ", acoustic_fallback_detail[match_type == "majority", .N],
            " by majority vote, ", acoustic_fallback_detail[match_type == "exclusive", .N],
            " by unanimous agreement among relatives).")
  }
  
  message("MEDIAS: ", acoustic_matched[!is.na(FG_num), uniqueN(ScientificName)], " of ",
          acoustic_matched[, uniqueN(ScientificName)], " species matched to an FG.")
  
  ## Full-extent MEDIAS species snapshot for build_full_fg_species_catalog() -
  ## acoustic_matched itself is never restricted by FILTER_AREAS in place
  ## (only the country/GSA summary tables derived from it below are), so this
  ## already covers every species in the raw acoustic abundance/biomass files,
  ## independent of FILTER_AREAS/YEAR_ECOPATH/TS_YEARS.
  snapshot_full_extent_species(acoustic_matched, species_taxonomy, "MEDIAS", csv_out_dir)
  
  ## --- 9c. Area denominator: MEDIAS Handbook Table 1 (per country, NM^2) ----
  ## Restricted here to entries plausibly overlapping FILTER_AREAS (GSA 1-11,
  ## Western Med) - Adriatic/Black Sea entries from Table 1 omitted since
  ## they're out of scope for this pipeline's region.
  MEDIAS_HANDBOOK_AREAS <- read_medits_reference("medias_handbook_areas.csv",
                                                 required_cols = c("country", "geographic_area", "area_nm2"))
  
  ## Area-name -> GSA number crosswalk. NOT stated explicitly in the
  ## handbook (Table 1 only names geographic areas) - this is a best-guess
  ## mapping based on standard GFCM GSA geography and should be CONFIRMED
  ## against your own institute's MEDIAS survey reports before trusting the
  ## resulting density values, particularly for Spain (Iberian coast could
  ## plausibly span more/fewer GSAs depending on which years/vessels
  ## covered which stretch).
  GSA_AREA_CROSSWALK <- read_medits_reference("gsa_area_crosswalk.csv",
                                              required_cols = c("country", "gsa", "geographic_area"))
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
      cache_path = file.path(csv_out_dir, "medias_area_10_200m_fallback.csv"),
      area_id_label = "GSA"  # this branch only runs under AREA_MODE == "westmed" (MEDIAS), where AreaID IS the GSA number
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
  
  ## Label the area column as GSA (MEDIAS only ever runs under AREA_MODE
  ## == "westmed", where AreaID IS the real GSA number) - a renamed COPY
  ## only, acoustic_fg_by_gsa itself keeps "AreaID" so
  ## medias_fg_index_regional and everything else below still works.
  acoustic_fg_by_gsa_for_csv <- copy(acoustic_fg_by_gsa)
  setnames(acoustic_fg_by_gsa_for_csv, "AreaID", "GSA")
  setcolorder(acoustic_fg_by_gsa_for_csv, c("GSA", setdiff(names(acoustic_fg_by_gsa_for_csv), "GSA")))
  fwrite(acoustic_fg_by_gsa_for_csv, file.path(csv_out_dir, "medias_fg_annual_density_by_area.csv"))
  fwrite(medias_fg_index_regional, file.path(csv_out_dir, "medias_fg_annual_density_regional.csv"))
  
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
  
  ## The two plots above are region-wide (every GSA already pooled into
  ## one line per FG) - this is the complementary per-GSA view, faceted
  ## by GSA instead of by FG,
  ## built from acoustic_fg_by_gsa (still has its own AreaID/GSA
  ## dimension, unlike medias_fg_index_regional above which already
  ## summed across it). Limited to the top N FGs by total biomass
  ## density (same "top_n_fg" idea plot_fg_timeseries_regional uses) so
  ## the legend/color scale stays readable instead of ~90 FG colors.
  p_medias_biom_by_gsa <- tryCatch({
    top_fg_medias <- acoustic_fg_by_gsa[, .(tot = sum(density_biomass_t_nm2, na.rm = TRUE)), by = FG_name][
      order(-tot)][seq_len(min(15, .N)), FG_name]
    ggplot(acoustic_fg_by_gsa[FG_name %in% top_fg_medias],
           aes(x = Year, y = density_biomass_t_nm2, colour = FG_name)) +
      geom_line(linewidth = 0.6, alpha = 0.85) +
      facet_wrap(vars(AreaID), labeller = labeller(AreaID = function(x) paste0("GSA ", x))) +
      labs(title = "MEDIAS acoustic survey - biomass density by FG, faceted by GSA",
           subtitle = paste0("Top ", length(top_fg_medias), " FG(s) by total biomass density - see",
                             " medias_fg_biomass_density_timeseries_regional.png for the region-wide (all-GSA) view"),
           x = "Year", y = "Density (t/nm^2)", colour = "FG") +
      theme_minimal(base_size = 10) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom")
  }, error = function(e) { message("[MEDIAS by-GSA plot] skipped - ", conditionMessage(e)); NULL })
  if (!is.null(p_medias_biom_by_gsa)) {
    ggsave(file.path(plot_dir, "medias_fg_biomass_density_timeseries_by_gsa.png"), p_medias_biom_by_gsa,
           width = 14, height = 10, dpi = 150, bg = "white")
  }
  
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
  
  ## =================================================================
  ## Stock-assessment (GFCM STAR/RAM Legacy + ICCAT) biomass AND biomass
  ## for FGs no survey or stock assessment can reach (marine megafauna,
  ## and the pelagic/benthic groups a bottom-trawl survey isn't designed
  ## to sample) - ALL consolidated into 01b_biomass_unsurveyed.R, which
  ## builds stock_assessment_fg_year itself from scratch as its first
  ## step. 01_biomass.R itself covers only what MEDITS/MEDIAS actually
  ## measured. Sourced with local = TRUE so it runs in this exact
  ## environment - see that file's own header for the full input/
  ## output contract.
  ## =================================================================
  source(file.path(git_dir, "scripts/01b_biomass_unsurveyed.R"), local = TRUE)
  
  
  ## =================================================================
  ## FG biomass-source priority. REPLACES the earlier demersal/small-
  ## pelagic-KEYWORD-guessing version, which is removed entirely. The
  ## rule, verbatim: "the priority of FG estimates MEDIAS>MEDITS, then if FG single species
  ## and have stock assessment then stock assessment, if a species is
  ## FG stanza then stock assessment. the rest MEDITS." Translated to
  ## one cascade per FG/Year, most specific first:
  ##   1. FG is single-species (n_species_in_fg == 1) AND has a real
  ##      stock assessment for that FG/Year -> stock assessment.
  ##      (A stanza-split FG is STRUCTURALLY single-species-exclusive by
  ##      construction in prepare_fg_lookup() - see that function's own
  ##      header comment - so "single species" and "species is a FG
  ##      stanza" are the SAME condition here, not two separate checks.)
  ##   2. Otherwise, MEDIAS if this FG/Year has a MEDIAS value.
  ##   3. Otherwise (the rest), MEDITS if this FG/Year has a MEDITS value.
  ## No keyword-based FG-name guessing (ex-DEMERSAL_FG_KEYWORDS/
  ## SMALL_PELAGIC_FG_KEYWORDS) is involved anywhere in this anymore.
  ## =================================================================
  ## n_species_in_fg re-derived directly from fg_lookup_safe as it
  ## actually exists right now, NOT trusted as a surviving column from
  ## prepare_fg_lookup() - that function computes its own internal
  ## n_species_in_fg for its dedup logic but does not return it (see its
  ## final `unique(fg_deduped[, .(ScientificName, FG_num, FG_name)])`
  ## line), which is what broke this before: "object 'n_species_in_fg'
  ## not found".
  fg_species_counts <- unique(fg_lookup_safe[, .(ScientificName, FG_num)])[, .(n_species_in_fg = uniqueN(ScientificName)), by = FG_num]
  fg_ecology_lookup <- merge(unique(fg_lookup_safe[, .(FG_num, FG_name)]), fg_species_counts, by = "FG_num", all.x = TRUE)
  
  fg_stock_assessed_nums <- if (nrow(stock_assessment_fg_year) > 0) unique(stock_assessment_fg_year$FG_num) else integer(0)
  ## "survey_exempt" is a THIRD ecology type (per project decision),
  ## checked FIRST (takes priority over single_species_assessed/mixed)
  ## for every FG in EXEMPT_FG_NAMES above - MEDITS/MEDIAS are bottom-
  ## trawl/acoustic surveys, not designed to sample zooplankton,
  ## suprabenthos, macroalgae, seagrass or coralligenous fauna
  ## representatively at all, so even a NONZERO survey density for one
  ## of these FGs is incidental bycatch/noise, not a real measurement -
  ## it must never be used, not even as a last-resort fallback. See the
  ## fcase() priority rule and the avg_density backfill guard just below
  ## for where this classification actually changes behavior.
  fg_ecology_lookup[, FG_ECOLOGY_TYPE := fcase(
    FG_name %in% EXEMPT_FG_NAMES, "survey_exempt",
    n_species_in_fg == 1 & FG_num %in% fg_stock_assessed_nums, "single_species_assessed",
    default = "mixed"
  )]
  fwrite(fg_ecology_lookup, file.path(csv_out_dir, "fg_ecology_classification.csv"))
  n_by_ecology <- fg_ecology_lookup[, .N, by = FG_ECOLOGY_TYPE]
  message("[FG biomass-source priority] ", paste0(n_by_ecology$FG_ECOLOGY_TYPE, "=", n_by_ecology$N, collapse = ", "),
          " - written to fg_ecology_classification.csv.")
  
  ## --- 9g. Combine MEDITS + MEDIAS into single FG-level and species-level
  ## tables (t/km^2 throughout) - these, not separate MEDIAS sheets, are
  ## what feed export_ecopath_ecosim_excel() in Step 10. FG-level
  ## combination now applies the priority cascade above: a single-
  ## species/stanza FG with an assessed stock prefers the GFCM STAR/RAM
  ## Legacy stock-assessment density; every other FG prefers MEDIAS over
  ## MEDITS, falling back to whichever ONE of MEDITS/MEDIAS actually has
  ## a value for that FG/Year (the flat average is now only a defensive
  ## fallback for the - in practice unreachable, since avg_density
  ## itself requires at least one of the two to be non-NA - case where
  ## neither individual figure is available).
  fg_index_combined_raw <- rbindlist(list(
    fg_index_regional[, .(Year, FG_num, FG_name, mean_density, Survey = "MEDITS")],
    medias_fg_index_regional[, .(Year, FG_num, FG_name, mean_density = mean_density_biomass_t_km2, Survey = "MEDIAS")]
  ), fill = TRUE)
  
  fg_overlap <- fg_index_combined_raw[, .N, by = .(Year, FG_num, FG_name)][N > 1]
  if (nrow(fg_overlap) > 0) {
    message("NOTE: ", nrow(fg_overlap), " FG/Year combination(s) have density from BOTH MEDITS",
            " and MEDIAS - see biomass_source below for which of these is actually used",
            " (MEDIAS preferred over MEDITS whenever both exist, per the priority rule).",
            " Review whether blending a demersal-trawl density with an acoustic density is",
            " appropriate for the FG/years still averaged:")
    print(merge(fg_overlap[, .(Year, FG_num, FG_name)], fg_index_combined_raw,
                by = c("Year", "FG_num", "FG_name"))[order(FG_num, Year)])
  }
  
  ## FG_name dropped from the group-by here - it's always
  ## 1:1 with FG_num anyway (relied on already by fg_ecology_lookup's own
  ## unique(FG_num, FG_name)), and dropping it lets fg_index_avg/
  ## fg_index_medits_only/fg_index_medias_only key purely on (Year,
  ## FG_num) below, which is what makes the FULL FG grid fix underneath
  ## possible - FG_name gets re-attached exactly once, from
  ## fg_ecology_lookup (which covers EVERY FG, not just survey-caught
  ## ones), after that full grid exists.
  fg_index_avg <- fg_index_combined_raw[
    , .(avg_density = mean(mean_density, na.rm = TRUE)), by = .(Year, FG_num)]
  fg_index_medits_only <- fg_index_combined_raw[Survey == "MEDITS", .(Year, FG_num, medits_density = mean_density)]
  fg_index_medias_only <- fg_index_combined_raw[Survey == "MEDIAS", .(Year, FG_num, medias_density = mean_density)]
  
  ## Builds the FULL (FG_num, Year) row set FIRST - every FG the SURVEY
  ## caught (fg_index_avg) UNIONED with every FG/Year the stock-
  ## assessment/megafauna table covers (stock_assessment_fg_year, which
  ## by this point already has ICCAT bluefin tuna/swordfish/albacore AND
  ## the manual-cited megafauna biomass folded into it - see
  ## merge_manual_cited_biomass() above). Without this union, a merge
  ## all.x=TRUE onto fg_index_avg alone would give an FG the survey has
  ## ZERO catch for (bluefin tuna, swordfish, every megafauna FG - none
  ## of these are ever actually landed in a MEDITS bottom trawl or a
  ## MEDIAS acoustic transect) no row to attach its stock-assessment/
  ## megafauna override density to, so it would silently never appear in
  ## fg_index_regional_combined - missing from Ecopath_B itself, not just
  ## missing PB/QB downstream (03_pbqb-traits.R).
  fg_year_survey <- unique(fg_index_avg[, .(FG_num, Year)])
  fg_year_assessed <- unique(stock_assessment_fg_year[, .(FG_num, Year)])
  fg_year_full <- unique(rbind(fg_year_survey, fg_year_assessed))
  fg_year_assessment_only <- fsetdiff(fg_year_full, fg_year_survey)
  if (nrow(fg_year_assessment_only) > 0) {
    message("[FG biomass-source priority] ", nrow(fg_year_assessment_only), " (FG_num, Year) row(s) added",
            " that the survey never sampled at all (bluefin tuna/swordfish/megafauna, etc.) - these get",
            " mean_density ENTIRELY from stock-assessment/megafauna biomass below (medits_density/",
            " medias_density/avg_density all genuinely NA for these rows - there was nothing for either",
            " survey to have measured):")
    print(merge(fg_year_assessment_only, unique(fg_lookup_safe[, .(FG_num, FG_name)]),
                by = "FG_num")[order(FG_num, Year)])
  }
  
  fg_index_regional_combined <- merge(fg_year_full, fg_index_avg, by = c("FG_num", "Year"), all.x = TRUE)
  fg_index_regional_combined <- merge(fg_index_regional_combined, fg_index_medits_only,
                                      by = c("Year", "FG_num"), all.x = TRUE)
  fg_index_regional_combined <- merge(fg_index_regional_combined, fg_index_medias_only,
                                      by = c("Year", "FG_num"), all.x = TRUE)
  fg_index_regional_combined <- merge(fg_index_regional_combined,
                                      fg_ecology_lookup[, .(FG_num, FG_name, FG_ECOLOGY_TYPE)],
                                      by = "FG_num", all.x = TRUE)   # re-attaches FG_name for every row, including the assessment-only ones added above
  fg_index_regional_combined <- merge(fg_index_regional_combined,
                                      stock_assessment_fg_year[, .(FG_num, Year, stock_assessment_density_t_km2, stock_assessment_source)],
                                      by = c("FG_num", "Year"), all.x = TRUE)
  
  ## fcase()'s own `default=` must be a single scalar value, not a
  ## row-varying vector like avg_density - passing avg_density there (as
  ## an earlier version of this did) throws "Length of 'default' must
  ## be 1" the moment this actually runs against real (>1-row) data.
  ## Built in two steps instead: fcase() first with NO default (so any
  ## row matching none of the four named conditions comes back NA),
  ## then an explicit is.na() backfill to avg_density for exactly those
  ## rows - a true row-wise fallback, which `default=` cannot express.
  fg_index_regional_combined[, `:=`(
    mean_density = fcase(
      ## "survey_exempt" checked FIRST: prefer the manual/EcoBase-cited
      ## stock_assessment_density_t_km2 exactly like single_species_assessed
      ## does, but if THAT doesn't exist either, stop here with NA - do
      ## NOT fall through to medias_density/medits_density/avg_density
      ## below. A trawl/acoustic survey catching a stray zooplankton/
      ## suprabenthos/macroalgae/seagrass/coralligenous individual is
      ## bycatch noise, not a real density measurement for that FG - using
      ## it (e.g. Macro zooplankton in stock_assessment_biomass_crosscheck.csv
      ## getting a nonsense 2.9e-05 t/km2 from MEDITS instead of the real
      ## ~120 t/km2 EcoBase literature figure) is actively worse than
      ## leaving the cell genuinely missing (which at least shows up in
      ## fg_missing_ecopath_B_REVIEW.csv for review).
      FG_ECOLOGY_TYPE == "survey_exempt" & !is.na(stock_assessment_density_t_km2), stock_assessment_density_t_km2,
      FG_ECOLOGY_TYPE == "survey_exempt", NA_real_,
      FG_ECOLOGY_TYPE == "single_species_assessed" & !is.na(stock_assessment_density_t_km2), stock_assessment_density_t_km2,
      !is.na(medias_density), medias_density,
      !is.na(medits_density), medits_density,
      ## Fallback for a "mixed" (multi-species) stock-assessment/megafauna
      ## FG - e.g. a cetacean or seabird FG that lumps several species.
      ## FG_ECOLOGY_TYPE only equals "single_species_assessed" for an
      ## EXCLUSIVELY single-species FG, so without this a mixed FG's own
      ## group-level megafauna density would be discarded even when it's
      ## the ONLY density available (medias/medits/avg_density all NA for
      ## an FG the survey never samples). This is exactly the case
      ## load_manual_cited_biomass_group() exists for - its output IS
      ## already the right group-level total for that whole FG.
      !is.na(stock_assessment_density_t_km2), stock_assessment_density_t_km2
    ),
    biomass_source = fcase(
      FG_ECOLOGY_TYPE == "survey_exempt" & !is.na(stock_assessment_density_t_km2),
      paste0(stock_assessment_source, " - survey-exempt FG (MEDITS/MEDIAS don't sample this representatively)"),
      FG_ECOLOGY_TYPE == "survey_exempt", NA_character_,  # left genuinely missing on purpose - see mean_density comment above
      FG_ECOLOGY_TYPE == "single_species_assessed" & !is.na(stock_assessment_density_t_km2),
      paste0(stock_assessment_source, " - single species/stanza FG"),  # stock_assessment_source is now a per-row label ("stock assessment (ICCAT)" or "stock assessment (STAR/RAM)") rather than a hardcoded string - see the ICCAT biomass block above
      !is.na(medias_density), "MEDIAS (preferred over MEDITS)",
      !is.na(medits_density), "MEDITS (MEDIAS unavailable this Year)",
      !is.na(stock_assessment_density_t_km2),
      paste0(stock_assessment_source, " - mixed/multi-species FG, survey has no catch of it at all")
    )
  )]
  ## Guard: this backfill must NEVER touch a
  ## survey_exempt FG - avg_density is itself just a MEDITS/MEDIAS blend,
  ## the exact survey noise this whole fix exists to keep out. Without
  ## this guard, a survey_exempt row deliberately left NA just above
  ## would get silently refilled with that same bycatch-noise average
  ## right here, undoing the fix.
  fg_index_regional_combined[is.na(mean_density) & FG_ECOLOGY_TYPE != "survey_exempt", mean_density := avg_density]
  fg_index_regional_combined[is.na(biomass_source) & FG_ECOLOGY_TYPE != "survey_exempt",
                             biomass_source := "MEDITS+MEDIAS average (neither MEDIAS nor MEDITS alone had a value this Year)"]
  fg_index_regional_combined[FG_ECOLOGY_TYPE == "survey_exempt" & is.na(biomass_source),
                             biomass_source := "MISSING - survey-exempt FG with no stock-assessment/EcoBase/literature source either (see EXEMPT_FG_NAMES) - genuinely no usable biomass yet, not filled with survey noise"]
  fg_index_regional_combined <- fg_index_regional_combined[, .(Year, FG_num, FG_name, mean_density, biomass_source, FG_ECOLOGY_TYPE)]
  
  n_by_source <- fg_index_regional_combined[, .N, by = biomass_source]
  message("[Biomass source priority] fg_index_regional_combined built with the stock-assessment/MEDIAS/MEDITS priority rule - ",
          paste0(n_by_source$biomass_source, " = ", n_by_source$N, collapse = "; "), ".")
  
  ## =================================================================
  ## STEP 9h: Multistanza (juvenile/adult) biomass split. Per project
  ## decision - fix for code-review finding #3: "European hake adult"
  ## was structurally impossible to ever receive its own survey/stock-
  ## assessment density, because prepare_fg_lookup()'s stanza tie-break
  ## (see that function's own header comment in
  ## lib_survey_fg_density_functions.R) keeps only ONE row per stanza
  ## species - by lowest FG_num, i.e. the juvenile FG here - so every
  ## MEDITS/MEDIAS observation and every STAR/RAM stock-assessment
  ## figure for Merluccius merluccius landed entirely on "European hake
  ## juv." (FG26); the adult FG got nothing from any real source, which
  ## is exactly what used to make it fall through to STEP 15b's EcoBase
  ## fallback (Port Cros MPA - see the code review). Now that fish FGs
  ## are barred from that fallback entirely (STEP 15b below), the adult
  ## FG needs a real number from somewhere else.
  ##
  ## Per the project's explicit choice, split whatever total density
  ## the retained stanza ended up with just above - from survey OR
  ## stock assessment, whichever the priority cascade used - using the
  ## SAME real juvenile:adult proportion 02_fisheries.R already derives
  ## from STECF FDI's age-resolved Discards/Landings files for the catch
  ## side (Spain/France/Italy, 2014+ - the one age-resolved source in
  ## this pipeline; see detect_multistanza_fg_pairs()/
  ## compute_multistanza_age_proportion() in
  ## lib_survey_fg_density_functions.R for the shared logic). This is a
  ## PROXY, not a measurement of standing-biomass age structure (catch
  ## selectivity at age != biomass age structure) - explicitly flagged
  ## as such below and in multistanza_biomass_split_REVIEW.csv. Runs
  ## generically over whatever detect_multistanza_fg_pairs() finds in
  ## dataframe2 (currently just European hake), not hardcoded to it.
  ## =================================================================
  multistanza_fg_pairs <- detect_multistanza_fg_pairs(dataframe2)
  if (nrow(multistanza_fg_pairs) == 0) {
    message("\n[Multistanza biomass split] No multistanza (juvenile/adult) FG pair found in FG_WMed_2026.csv - skipped.")
  } else {
    message("\n[Multistanza biomass split] ", nrow(multistanza_fg_pairs), " species with a clean juvenile/adult FG pair: ",
            paste0(multistanza_fg_pairs$ScientificName, " (FG", multistanza_fg_pairs$FG_num_juv, " juv / FG",
                   multistanza_fg_pairs$FG_num_adult, " adult)", collapse = ", "), ".")
    
    ## Prefer MEDITS TC.csv's own maturity staging (matsub) when
    ## available - a direct survey measurement of the standing population's
    ## age structure, rather than the STECF FDI catch-at-age proxy below
    ## (catch selectivity at age != biomass age structure). TC_SURVEY_FILE
    ## follows the same resolve_pcloud_file() pattern as ta/tb above; point
    ## it at wherever your real TC.csv lives (same folder as TA.csv/TB.csv).
    ## See compute_multistanza_age_proportion_from_medits_tc()'s own header
    ## comment in lib_survey_fg_density_functions.R for the juvenile_stage_
    ## cutoff assumption (default: matsub stage 1 = juvenile, 2A+ = adult)
    ## and for code_overrides if a species' genus/species MEDITS code isn't
    ## the standard 4+3-letter truncation.
    TC_SURVEY_FILE <- resolve_pcloud_file(paste0(pcloud_dir, "/data/Medits_Medias_JRC2026/2024_MEDBSsurvey/Demersal/TC.csv"), pcloud_dir)
    multistanza_age_proportion <- if (file.exists(TC_SURVEY_FILE)) {
      compute_multistanza_age_proportion_from_medits_tc(multistanza_fg_pairs, TC_SURVEY_FILE)
    } else {
      message("[Multistanza biomass split] TC.csv not found at '", TC_SURVEY_FILE, "' - falling back to the",
              " STECF FDI catch-at-age proxy.")
      data.table()
    }
    
    STECF_FDI_PARENT_DIR_BIOMASS <- file.path(pcloud_dir, "data/fisheries/FDI")
    stecf_fdi_dir_biomass <- resolve_versioned_data_subdir(STECF_FDI_PARENT_DIR_BIOMASS, "Biological")
    if (is.na(stecf_fdi_dir_biomass)) stecf_fdi_dir_biomass <- STECF_FDI_PARENT_DIR_BIOMASS
    stecf_bio_dir_biomass <- file.path(stecf_fdi_dir_biomass, "Biological")
    
    multistanza_biomass_split_applied <- data.table()
    multistanza_source_label <- "MEDITS TC.csv maturity staging (matsub)"
    if (nrow(multistanza_age_proportion) == 0) {
      multistanza_source_label <- "STECF FDI Biological Age data's juvenile:adult catch proportion"
      fao_species_biomass <- tryCatch(resolve_fao_species_reference(pcloud_dir, csv_out_dir), error = function(e) {
        message("[Multistanza biomass split] Could not load the FAO species reference (", conditionMessage(e),
                ") - the multistanza biomass split is skipped this run, FG(s) stay whatever STEP 9g resolved.")
        NULL
      })
      if (!is.null(fao_species_biomass)) {
        multistanza_age_proportion <- compute_multistanza_age_proportion(
          multistanza_fg_pairs, stecf_bio_dir_biomass, fao_species_biomass)
      }
    }
    {
      if (nrow(multistanza_age_proportion) == 0) {
        message("[Multistanza biomass split] No usable age-resolved proportion (see messages above) - FG(s) stay",
                " whatever STEP 9g resolved (i.e. the whole species' density on whichever stanza",
                " prepare_fg_lookup() retained, nothing on the other).")
      } else {
        prop_by_species <- multistanza_age_proportion[, .(prop_juvenile = mean(prop_juvenile, na.rm = TRUE)),
                                                      by = ScientificName]  # landings+discards averaged - biomass has no landed/discarded split of its own
        for (i in seq_len(nrow(multistanza_fg_pairs))) {
          sci_name <- multistanza_fg_pairs$ScientificName[i]
          fg_juv   <- multistanza_fg_pairs$FG_num_juv[i]
          fg_adult <- multistanza_fg_pairs$FG_num_adult[i]
          retained_fg <- fg_lookup_safe[ScientificName == sci_name, FG_num]
          if (length(retained_fg) == 0) {
            message("[Multistanza biomass split] '", sci_name, "' not found in fg_lookup_safe (unexpected) - skipped.")
            next
          }
          retained_fg <- retained_fg[1]
          missing_fg <- setdiff(c(fg_juv, fg_adult), retained_fg)
          if (length(missing_fg) != 1) {
            message("[Multistanza biomass split] '", sci_name, "': couldn't identify a single missing stanza FG",
                    " (retained=", retained_fg, ") - skipped.")
            next
          }
          p_juv <- prop_by_species[ScientificName == sci_name, prop_juvenile]
          if (length(p_juv) == 0 || is.na(p_juv)) {
            message("[Multistanza biomass split] '", sci_name, "': no usable juvenile proportion - FG",
                    missing_fg, " stays at whatever it already had (likely NA/0).")
            next
          }
          retained_share <- if (missing_fg == fg_juv) (1 - p_juv) else p_juv
          missing_share  <- if (missing_fg == fg_juv) p_juv else (1 - p_juv)
          retained_rows <- copy(fg_index_regional_combined[FG_num == retained_fg])
          missing_name <- if (missing_fg == fg_juv) multistanza_fg_pairs$FG_name_juv[i] else multistanza_fg_pairs$FG_name_adult[i]
          new_missing_rows <- copy(retained_rows)
          new_missing_rows[, `:=`(
            FG_num = missing_fg,
            FG_name = missing_name,
            mean_density = mean_density * missing_share,
            biomass_source = paste0(biomass_source, " - stanza split (", round(100 * missing_share, 1),
                                    "% of '", sci_name, "' combined total, via ", multistanza_source_label,
                                    " - see multistanza_biomass_split_REVIEW.csv)")
          )]
          fg_index_regional_combined[FG_num == retained_fg, `:=`(
            mean_density = mean_density * retained_share,
            biomass_source = paste0(biomass_source, " - stanza split (", round(100 * retained_share, 1),
                                    "% of '", sci_name, "' combined total, same source as above)")
          )]
          fg_index_regional_combined <- fg_index_regional_combined[FG_num != missing_fg]  # drop the old all-NA placeholder row(s) for the missing stanza
          fg_index_regional_combined <- rbindlist(list(fg_index_regional_combined, new_missing_rows), use.names = TRUE, fill = TRUE)
          multistanza_biomass_split_applied <- rbindlist(list(multistanza_biomass_split_applied, data.table(
            ScientificName = sci_name, FG_num_retained = retained_fg, FG_num_split = missing_fg,
            prop_juvenile_used = p_juv, retained_share = retained_share, missing_share = missing_share
          )), fill = TRUE)
          message("[Multistanza biomass split] '", sci_name, "': FG", retained_fg, " kept ", round(100 * retained_share, 1),
                  "% of the combined MEDITS/MEDIAS/stock-assessment density; FG", missing_fg, " (\"", missing_name,
                  "\") now carries the remaining ", round(100 * missing_share, 1), "%, split via ", multistanza_source_label,
                  " (", round(100 * p_juv, 1), "% juvenile).")
        }
        setorder(fg_index_regional_combined, FG_num, Year)
        if (nrow(multistanza_biomass_split_applied) > 0) {
          fwrite(multistanza_biomass_split_applied, file.path(csv_out_dir, "multistanza_biomass_split_REVIEW.csv"))
          proxy_caveat <- if (grepl("^STECF", multistanza_source_label)) {
            " This is a PROXY (catch-age selectivity, not a direct biomass-age measurement) - review before trusting."
          } else {
            " Source: direct MEDITS survey maturity staging (matsub) - review the juvenile_stage_cutoff assumption"
          }
          message("[Multistanza biomass split] Applied to ", nrow(multistanza_biomass_split_applied),
                  " of ", nrow(multistanza_fg_pairs), " multistanza species pair(s) - see",
                  " multistanza_biomass_split_REVIEW.csv.", proxy_caveat)
        }
      }
    }
  }
  
  ## The single most useful "at a glance" biomass validation
  ## plot - every FG's Ecopath-baseline-year density, ordered by
  ## magnitude, colored by WHICH source actually supplied it (survey vs.
  ## stock assessment vs. manual-cited literature vs. genuinely missing).
  ## p_by_area/p_regional above already show the MEDITS/MEDIAS survey
  ## index over the full time series - this is the complementary view:
  ## the actual final per-FG value that feeds Ecopath_B, tagged with its
  ## provenance, so a reviewer can see immediately which FGs are resting
  ## on a real stock assessment/literature figure vs. raw survey signal
  ## vs. nothing at all (survey_exempt + missing).
  p_biomass_by_fg_source <- tryCatch({
    d <- fg_index_regional_combined[Year %in% YEAR_ECOPATH, .(mean_density = mean(mean_density, na.rm = TRUE),
                                                              biomass_source = biomass_source[1]), by = .(FG_num, FG_name)]
    d[, Tier := fcase(
      is.na(biomass_source), "Missing",
      grepl("^MISSING", biomass_source), "Missing",
      grepl("^MEDITS|^MEDIAS", biomass_source), "Survey (MEDITS/MEDIAS)",
      grepl("stock assessment", biomass_source), "Stock assessment (ICCAT/STAR-RAM)",
      grepl("EcoBase", biomass_source, ignore.case = TRUE), "EcoBase (literature model)",
      grepl("marine megafauna|primary producer|manual", biomass_source, ignore.case = TRUE), "Manual-cited literature",
      default = "Other"
    )]
    d[, mean_density := fifelse(is.na(mean_density), 0, mean_density)]
    d <- d[order(-mean_density)]
    d[, FG_label := factor(paste0(FG_num, " - ", FG_name), levels = paste0(FG_num, " - ", FG_name))]
    ## A handful of FGs (typically Detritus/plankton/megafauna-adjacent
    ## groups) sit 1-2+ orders of magnitude above everything else, so on a
    ## LINEAR x-axis every other bar gets compressed down to a sliver next
    ## to them. pseudo_log_trans behaves like log10 once values get large
    ## but stays linear (and defined) near zero, so FGs with a real but
    ## small density are still visible AND a genuine 0 (missing) still
    ## plots at 0 instead of erroring the way a true log scale would.
    ##
    ## A "Missing" FG's grey bar is easy to miss at a glance (it's the
    ## same visual weight as a genuinely tiny-but-real value), so the
    ## FG's own NAME on the y-axis is ALSO colored red for exactly the
    ## FGs flagged "Missing", on top of (not instead of) the existing
    ## grey bar. element_text(colour=...) takes one color per tick IN
    ## AXIS ORDER, so axis_tick_colors below is built from the same
    ## ascending-by-mean_density order reorder(FG_label, mean_density)
    ## uses for the axis itself - order must match exactly or labels get
    ## the wrong color.
    y_axis_order <- levels(reorder(d$FG_label, d$mean_density))
    axis_tick_colors <- ifelse(d$Tier[match(y_axis_order, d$FG_label)] == "Missing", "firebrick", "grey20")
    ggplot(d, aes(x = mean_density, y = reorder(FG_label, mean_density), fill = Tier)) +
      geom_col() +
      scale_x_continuous(trans = scales::pseudo_log_trans(base = 10)) +
      scale_fill_manual(values = c("Survey (MEDITS/MEDIAS)" = "#4C6FE7", "Stock assessment (ICCAT/STAR-RAM)" = "#2E7D32",
                                   "Manual-cited literature" = "#F9A825", "EcoBase (literature model)" = "#8E24AA",
                                   "Missing" = "grey70", "Other" = "grey40")) +
      labs(title = "Biomass by functional group at the Ecopath baseline, by data source",
           subtitle = paste0("Mean density, Year ", paste(range(YEAR_ECOPATH), collapse = "-"), " - color = which source actually supplied the value.",
                             " x-axis is log-like (pseudo-log) so small-density FGs stay visible next to a few very large ones.",
                             " FG name in RED = Missing (no source at all)."),
           x = "t/km2 (pseudo-log scale)", y = NULL, fill = "Source") +
      theme_minimal(base_size = 7) + theme(legend.position = "bottom",
                                           axis.text.y = element_text(colour = axis_tick_colors))
  }, error = function(e) { message("[Biomass-by-FG-source plot] skipped - ", conditionMessage(e)); NULL })
  if (!is.null(p_biomass_by_fg_source)) {
    n_fg_plotted <- length(unique(fg_index_regional_combined[Year %in% YEAR_ECOPATH]$FG_num))
    ggsave(file.path(plot_dir, "biomass_by_fg_source.png"), p_biomass_by_fg_source,
           width = 10, height = max(8, 0.16 * n_fg_plotted), dpi = 150, bg = "white", limitsize = FALSE)
  }
  
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
  ## for the FG_spp sheets. 03_pbqb-traits.R (the PB/QB estimation pipeline)
  ## needs this same species x FG x density data as its species_df input
  ## (Species/FG/Biomass), so it's saved as a plain CSV here rather than
  ## making 03_pbqb-traits.R parse the Ecopath-formatted Excel sheet.
  fwrite(species_density_regional_combined, file.path(csv_out_dir, "species_density_regional_combined.csv"))
  message("Saved species_density_regional_combined.csv (", nrow(species_density_regional_combined),
          " rows) - MEDITS+MEDIAS combined species-level density, all years. This is the",
          " file 03_pbqb-traits.R reads directly (SPECIES_DF_SOURCE == \"survey\") to build",
          " its species_df input.")
  
  ## Species inventory, for review - every species that appears ANYWHERE
  ## in the combined time series (not just the Ecopath base years), one
  ## row per species, with its FG_num/FG_name assignment and full
  ## taxonomy (Genus/Family/Order/Class/Phylum) attached, "to see the
  ## species i got" . A species matched to more than one FG_num
  ## across different rows (shouldn't normally happen post-fallback, but
  ## checked rather than assumed) gets one row per FG_num it was actually
  ## assigned to, flagged, rather than silently picking one.
  species_fg_map <- unique(species_density_regional_combined[, .(FG_num, FG_name, ScientificName)])
  dupe_species_fg <- species_fg_map[, .N, by = ScientificName][N > 1, ScientificName]
  if (length(dupe_species_fg) > 0) {
    message("NOTE: ", length(dupe_species_fg), " species are assigned to MORE THAN ONE FG_num across the",
            " combined time series (kept as separate rows in species_inventory_with_taxonomy.csv,",
            " flagged rather than collapsed) - worth checking: ", paste(dupe_species_fg, collapse = ", "))
  }
  species_inventory <- merge(species_fg_map, species_taxonomy, by = "ScientificName", all.x = TRUE)
  setcolorder(species_inventory, c("ScientificName", "FG_num", "FG_name",
                                   setdiff(names(species_inventory), c("ScientificName", "FG_num", "FG_name"))))
  setorder(species_inventory, FG_num, ScientificName)
  fwrite(species_inventory, file.path(csv_out_dir, "species_inventory_with_taxonomy.csv"))
  message("Saved species_inventory_with_taxonomy.csv (", nrow(species_inventory), " species x FG row(s),",
          " ", uniqueN(species_inventory$ScientificName), " distinct species) - every species in the",
          " combined MEDITS+MEDIAS time series, with its FG_name and full taxonomy, for review.")
  
  ## =================================================================
  ## Stock-assessment vs. survey comparison - now that fg_index_regional_
  ## combined exists (built with ecology/quality priority above, which
  ## may already use stock_assessment_fg_year as the PRIMARY source for
  ## single-species FGs - see the "9g" block above), this just attaches
  ## the survey-only figure alongside it for every assessed FG x Year, so
  ## the override (where it happened) is visibly checked against what the
  ## survey alone would have said, and every other assessed FG (the
  ## "mixed"-ecology ones, where stock assessment is validation-only)
  ## gets its comparison too.
  ## =================================================================
  ## stock_assessment_biomass_crosscheck.csv isn't needed - keep only
  ## iccat_biomass_by_fg.csv. Removed entirely (not just the fwrite):
  ## nothing downstream reads this table, it was only ever computed to
  ## be written to this CSV.
  
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
  
} else {
  
  ## AREA_MODE == "custom" - MEDITS-only. MEDIAS acoustic survey and
  ## GFCM stock-assessment biomass are both West-Med-subregion-scoped
  ## data sources (see the header comment above STEP 9) that don't
  ## apply to a custom, non-GSA-aligned boundary, so this branch just
  ## aliases the plain MEDITS results already computed above (Steps
  ## 5-7: species_density_regional, fg_index_regional, fg_cv_log_medits)
  ## under the *_combined names Step 10 onward expects - same shape,
  ## same file names, so nothing downstream branches on AREA_MODE.
  message("\n[AREA_MODE = custom] Skipping MEDIAS acoustic survey and GFCM stock-assessment",
          " biomass - both are West-Med-subregion-scoped data sources that don't apply to a",
          " custom, non-GSA-aligned region. Biomass for this run is MEDITS-only.")
  
  species_density_regional_combined <- copy(species_density_regional)
  fwrite(species_density_regional_combined, file.path(csv_out_dir, "species_density_regional_combined.csv"))
  message("Saved species_density_regional_combined.csv (", nrow(species_density_regional_combined),
          " rows) - MEDITS-only species-level density (AREA_MODE = custom), all years. This is the",
          " file 03_pbqb-traits.R reads directly (SPECIES_DF_SOURCE == \"survey\") to build",
          " its species_df input.")
  
  species_fg_map <- unique(species_density_regional_combined[, .(FG_num, FG_name, ScientificName)])
  dupe_species_fg <- species_fg_map[, .N, by = ScientificName][N > 1, ScientificName]
  if (length(dupe_species_fg) > 0) {
    message("NOTE: ", length(dupe_species_fg), " species are assigned to MORE THAN ONE FG_num -",
            " kept as separate rows in species_inventory_with_taxonomy.csv, flagged rather than",
            " collapsed) - worth checking: ", paste(dupe_species_fg, collapse = ", "))
  }
  species_inventory <- merge(species_fg_map, species_taxonomy, by = "ScientificName", all.x = TRUE)
  setcolorder(species_inventory, c("ScientificName", "FG_num", "FG_name",
                                   setdiff(names(species_inventory), c("ScientificName", "FG_num", "FG_name"))))
  setorder(species_inventory, FG_num, ScientificName)
  fwrite(species_inventory, file.path(csv_out_dir, "species_inventory_with_taxonomy.csv"))
  message("Saved species_inventory_with_taxonomy.csv (", nrow(species_inventory), " species x FG row(s),",
          " ", uniqueN(species_inventory$ScientificName), " distinct species) - every species in the",
          " MEDITS-only time series, with its FG_name and full taxonomy, for review.")
  
  fg_index_regional_combined <- copy(fg_index_regional)[, .(Year, FG_num, FG_name, mean_density)]
  fg_index_regional_combined[, `:=`(
    biomass_source = "MEDITS (AREA_MODE = custom - no MEDIAS or stock assessment available)",
    FG_ECOLOGY_TYPE = "mixed"
  )]
  
  fg_cv_log_combined <- copy(fg_cv_log_medits)[, .(FG_num, cv_log)]
}

## =================================================================
## STEP 10: Excel export - fg_index_regional_combined/species_density_
## regional_combined/fg_cv_log_combined feed the SAME FG_spp_Ecopath/
## Ecopath/Ecosim/FG_spp_Ecosim sheets either way. AREA_MODE ==
## "westmed": MEDITS + MEDIAS COMBINED (no separate MEDIAS sheets).
## AREA_MODE == "custom": MEDITS-only (see STEP 9's else{} branch).
## species_taxonomy was already built earlier (right after Step 4, FG
## matching) since apply_catchability_correction() needed it too -
## reused as-is here, not rebuilt.
## =================================================================
## Label fg_index's AreaID column as GSA
## when it really is one (AREA_MODE == "westmed", or "custom" + gsa) - a
## renamed COPY only, fg_index itself keeps "AreaID" since nothing past
## this point still needs it either way, but better safe than reused later.
fg_index_for_csv <- copy(fg_index)
if (AREA_MODE == "westmed" || (AREA_MODE == "custom" && CUSTOM_AREA_TYPE == "gsa")) {
  setnames(fg_index_for_csv, "AreaID", "GSA")
  setcolorder(fg_index_for_csv, c("GSA", setdiff(names(fg_index_for_csv), "GSA")))
}
fwrite(fg_index_for_csv, file.path(csv_out_dir, "survey_fg_annual_index.csv"))
fwrite(fg_index_regional_combined, file.path(csv_out_dir, "survey_fg_annual_index_regional_combined.csv"))

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
  normalize_ts = NORMALIZE_TS,
  csv_out_dir = csv_out_dir
)

## Read back Ecopath_B's per-FG Biomass (just written above) so STEP 11
## below can reconcile FG_spp_Ecopath's species-level proportions
## against the SAME FG-level Biomass Ecopath_B actually uses - see that
## step's own comment for why this matters (the two used to be built
## from different sources and could disagree).
ecopath_b_by_fg <- as.data.table(openxlsx::read.xlsx(file.path(out_dir, "ecopath_ecosim_inputs.xlsx"), sheet = "Ecopath_B"))
ecopath_b_by_fg[, FG_num := as.integer(FG_num)]

## =================================================================
## STEP 10b: "Ecobase" workbook sheet. Per project decision: "add a
## section where it adds a Ecobase sheet on the excel input file
## produced with data from the model biomass pbqb reference year etc".
## build_ecobase_sheet_dt() (03b_ecobase.R) reads back whichever of the
## EcoBase queries above already ran this pipeline (biomass here in
## 01_biomass.R, PB/QB later in 03_pbqb-traits.R - both write into the
## SAME shared raw cache, ecobase_all_inputs_with_meta.csv, so whichever
## ran first is enough) and writes a side-by-side comparison table -
## other Western Med Ecopath models' Biomass/PB/QB per group, next to
## their own model name/ecosystem/country/reference year/authors - into
## an "Ecobase" sheet in ecopath_ecosim_inputs.xlsx, sorted so whichever
## model's reference year is closest to THIS model's own YEAR_ECOPATH
## sits at the top. "Ecobase" is now in trim_workbook_to_final_sheets()'s
## target_order (lib_survey_fg_density_functions.R), so it survives every
## script's end-of-run trim, not just this one.
## Silently does nothing if the raw EcoBase cache doesn't exist yet
## (e.g. ENABLE_ECOBASE_BIOMASS_QUERY was FALSE and 03_pbqb-traits.R
## hasn't run yet either) - never blocks the rest of the pipeline.
## =================================================================
ecobase_sheet_dt <- build_ecobase_sheet_dt(csv_out_dir, target_year = round(mean(YEAR_ECOPATH)))
if (!is.null(ecobase_sheet_dt) && nrow(ecobase_sheet_dt) > 0) {
  upsert_workbook_sheets(list(Ecobase = ecobase_sheet_dt), file.path(out_dir, "ecopath_ecosim_inputs.xlsx"))
}

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
## Dedup key is (FG_num, Species), NOT Species alone. Several FGs that
## the survey never samples (marine megafauna in particular - cetaceans,
## seabirds, sea turtles, pinnipeds) can carry a blank/NA Species in the
## FG_WMed_2026.csv catalog, or share a generic placeholder text, since
## there's no individual scientific-name-level catalog entry for them.
## Deduping by Species alone treated every one of those blank/shared
## values as "the same row" across DIFFERENT FGs, so only the first
## such FG survived unique() and every other one silently vanished from
## full_species_fg (and therefore from FG_spp_Ecopath entirely - no
## Density, no prop_sp_fg row at all) - this is what was happening to
## cetaceans and seabirds. Keying on FG_num too means two rows only
## collapse when they really are the same species in the same FG.
full_species_fg <- unique(rbindlist(list(all_species_fg, fg_master_species)), by = c("FG_num", "Species"))

## --- base-year Density + within-FG proportion, zero-filled -------------
## Per project decision, a species genuinely present in the West Med
## (fish, cephalopod, benthos, coral, invertebrate - i.e. anything
## MEDITS/MEDIAS actually samples between 10-800m) can show a real
## zero/no-data density in the 1994-1996 Ecopath baseline years purely
## from trawl catchability/rarity (patchy distribution, low encounter
## probability), not because it was actually absent then. Where a
## species has a zero/no-data baseline but a real (>0) density in some
## OTHER survey year, borrow the nearest such year's value instead of
## reporting a false zero - flagged, never silently blended into an
## average. Skipped entirely for any FG whose name contains "Expanding"
## (a genuinely range-expanding/colonizing group, where a real baseline
## zero IS the correct signal and must not be papered over).
species_baseline_fallback <- resolve_baseline_with_nearest_year_fallback(
  species_density_regional_combined[, .(Year, FG_num, FG_name, Species = ScientificName, mean_density)],
  id_cols = c("FG_num", "FG_name", "Species"), value_col = "mean_density",
  baseline_years = YEAR_ECOPATH)
density_in_base_years <- species_baseline_fallback[, .(FG_num, FG_name, Species, Density = final_value)]
FG_spp_Ecopath <- merge(full_species_fg, density_in_base_years,
                        by = c("FG_num", "FG_name", "Species"), all.x = TRUE)

n_borrowed <- sum(species_baseline_fallback$borrowed, na.rm = TRUE)
if (n_borrowed > 0) {
  fwrite(species_baseline_fallback[borrowed == TRUE],
         file.path(csv_out_dir, "species_baseline_year_borrowed_REVIEW.csv"))
  message(n_borrowed, " species had a zero/no-data density in the ", min(YEAR_ECOPATH), "-", max(YEAR_ECOPATH),
          " Ecopath base years but a real nonzero density in another survey year - borrowed the",
          " nearest such year's value instead of reporting a false zero (skipped for any FG whose",
          " name contains \"Expanding\"). See species_baseline_year_borrowed_REVIEW.csv.")
}

n_zero_density <- FG_spp_Ecopath[is.na(Density), .N]
if (n_zero_density > 0) {
  message(n_zero_density, " of ", nrow(FG_spp_Ecopath), " species still have no usable density for the ",
          min(YEAR_ECOPATH), "-", max(YEAR_ECOPATH), " Ecopath base years (never observed anywhere in the",
          " time series, or only ever observed in a FG_name containing \"Expanding\", which is deliberately",
          " excluded from the borrow-fallback above) - Density and prop_sp_fg set to 0 for these.")
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

## --- prop_sp_fg fallback for FGs with no usable species density --------
## The Density-ratio arithmetic above (Density / fg_total_density) only
## produces a real answer when at least one species in the FG has a
## nonzero survey density. Two cases where that's NOT true, both common
## for FGs the survey never samples (megafauna especially):
##   - a SINGLE-species FG: its one species IS the FG, 100% by
##     definition, whatever its density happens to be (was already
##     forced to 1 here; folded into the general rule below instead).
##   - a MULTI-species FG where EVERY species has Density = 0 (e.g.
##     "Other dolphins" = Delphinus delphis + Stenella coeruleoalba,
##     neither ever caught by a bottom-trawl/acoustic survey) - the
##     Density-ratio formula leaves prop_sp_fg = 0 for ALL of them, which
##     silently throws away the FG's real (literature/stock-assessment)
##     biomass at species level: Biomass_t_km2 below would come out 0
##     for every species even though Ecopath_B has a real nonzero total
##     for that FG. The project owner, from exactly this case ("Other
##     dolphins" showing Density=0/prop_sp_fg=0 for both species): "this
##     biomass is resulted from the sum of species and this should be
##     noted on the FG_spp".
## Fixed by falling back to an EVEN split (1/n species) whenever the
## whole FG's survey density is zero - so Biomass_t_km2 always sums back
## to Ecopath_B's real total, never silently to zero - and by adding a
## prop_sp_fg_basis column that says in plain language whether a row's
## prop_sp_fg is a real density-weighted split or an even-split
## assumption (i.e. exactly the note it was asked for), so an even split
## is never mistaken for an actual per-species measurement.
FG_spp_Ecopath[, n_species_in_fg := .N, by = FG_num]
FG_spp_Ecopath[, fg_total_density := sum(Density, na.rm = TRUE), by = FG_num]
FG_spp_Ecopath[fg_total_density == 0, prop_sp_fg := 1 / n_species_in_fg]
FG_spp_Ecopath[, prop_sp_fg_basis := fifelse(
  n_species_in_fg == 1L,
  "single species in FG (100% by definition)",
  fifelse(fg_total_density > 0,
          "density-weighted (real MEDITS/MEDIAS survey density)",
          "even split across FG's species (no survey density available for this FG - not a real per-species measurement)"))]
FG_spp_Ecopath[, `:=`(n_species_in_fg = NULL, fg_total_density = NULL)]

## --- species-level split from the SAME literature figures that built --
## --- this FG's Ecopath_B total (species_lit_biomass, from -------------
## --- load_manual_cited_biomass_group() above) --------------------------
## Per project decision: "the proportion of species within FG should
## include all values inputted in B Ecopath, including the literature
## ones. Thats because it will be used for the pbqb for example." Both
## the even-split fallback just above and the raw survey-density split
## before it are blind to WHERE Ecopath_B's own total actually came from
## - for any FG whose manual-cited biomass CSV (marine_megafauna_
## biomass.csv / primary_producer_plankton_biomass.csv) supplies a
## Species column alongside its Biomass_t (e.g. "Other dolphins":
## separate Stenella/Delphinus rows; "Pelagic/Offshore seabirds":
## separate Calonectris/Puffinus yelkouan/Hydrobates rows), that IS the
## real species-level composition behind the FG's own Ecopath_B figure -
## using it here means prop_sp_fg, and therefore Biomass_t_km2/PB_QB_spp
## downstream in 03_pbqb-traits.R, is built from the exact same numbers
## as Ecopath_B, not a second, independently-maintained approximation
## that could drift out of step with it (species_weight_within_fg.csv/
## MEGAFAUNA_FG_SPECIES_WEIGHTS below still exists for FGs that have NO
## per-species literature row at all - e.g. "Deep sea-cetacean feeders" -
## where only a relative-abundance-based approximation is available, no
## direct species-level Biomass_t).
if (exists("species_lit_biomass") && nrow(species_lit_biomass) > 0) {
  species_lit_biomass_by_fg <- species_lit_biomass[, .(species_biomass_t = sum(species_biomass_t, na.rm = TRUE)),
                                                   by = .(FG_num, Species)]
  species_lit_biomass_by_fg[, fg_lit_total := sum(species_biomass_t), by = FG_num]
  FG_spp_Ecopath <- merge(FG_spp_Ecopath, species_lit_biomass_by_fg[, .(FG_num, Species, species_biomass_t, fg_lit_total)],
                          by = c("FG_num", "Species"), all.x = TRUE)
  lit_fg_nums <- unique(species_lit_biomass_by_fg$FG_num)
  in_lit_fg <- FG_spp_Ecopath$FG_num %in% lit_fg_nums
  ## Within an FG that has literature species rows, a species named in
  ## that CSV gets its real share; a species in the SAME FG with no row
  ## gets 0 (same "unweighted species get 0" convention used everywhere
  ## else in this file - not silently dropped, and still flagged via
  ## prop_sp_fg_basis so a 0 here is never mistaken for a real-zero
  ## biomass claim).
  FG_spp_Ecopath[in_lit_fg, prop_sp_fg := fifelse(!is.na(species_biomass_t), species_biomass_t / fg_lit_total, 0)]
  FG_spp_Ecopath[in_lit_fg, prop_sp_fg_basis := fifelse(
    !is.na(species_biomass_t),
    "literature species-level biomass (from the same manual-cited CSV that built this FG's own Ecopath_B total - see Source_citation)",
    "0 - literature species-level biomass exists for other species in this FG, but not for this one (not a claim this species' real biomass is zero)")]
  n_lit_species <- FG_spp_Ecopath[in_lit_fg & !is.na(species_biomass_t), .N]
  message("[FG_spp] ", n_lit_species, " species across ", length(lit_fg_nums), " FG(s) had prop_sp_fg set directly",
          " from literature species-level biomass (species_lit_biomass) - takes priority over both survey density",
          " and the MEGAFAUNA_FG_SPECIES_WEIGHTS/species_weight_within_fg.csv mechanism below for these FG(s).")
  FG_spp_Ecopath[, `:=`(species_biomass_t = NULL, fg_lit_total = NULL)]
} else {
  lit_fg_nums <- integer(0)
}

## --- documented species-level WEIGHTED override for megafauna FGs ------
## The even-split fallback just above is only correct when there's
## genuinely no basis to prefer one species over another. Two megafauna
## FGs now have a real basis instead - one an exact single-species
## target, the other a relative-abundance-based weighting - rather than
## the flat even split correctly flagged as implausible ("is this
## actually the same exact proportion among species? i think that this
## isnt accurate" / "this needs review").
##
## "Other dolphins" (Delphinus delphis + Stenella coeruleoalba): its
## ACCOBAMS Survey Initiative figure is documented (per the project
## owner - see the MEGAFAUNA_TAXON_KEYWORDS comment above) as a STRIPED
## DOLPHIN (Stenella coeruleoalba) proxy specifically, not a combined
## measurement - weight 1 for Stenella, 0 for Delphinus.
##
## "Deep sea-cetacean feeders" (Globicephala melas + Grampus griseus +
## Ziphius cavirostris): weighted by relative BIOMASS, not a straight
## species count, using the ACCOBAMS Survey Initiative's own basin-wide
## abundance estimates (Table 4, Lauriano et al./ACCOBAMS 2023 synoptic
## assessment - https://www.frontiersin.org/journals/marine-science/articles/10.3389/fmars.2023.1270513/full,
## cross-referenced against the ASI-Med-Report at accobams.org) -
## individuals: long-finned pilot whale 5,540; Risso's dolphin 26,006;
## Cuvier's beaked whale 2,929 - multiplied by each species' typical
## adult body mass (long-finned pilot whale ~1,800 kg; Risso's dolphin
## ~400 kg; Cuvier's beaked whale ~1,200 kg - commonly cited species
## averages, e.g. via Wikipedia/marine-mammal reference summaries, NOT
## independently re-verified against a primary weight study this
## session) to convert individual counts into a relative BIOMASS share.
## CAVEATS, explicit rather than hidden (same "flagged, not guessed"
## convention as every other literature fallback in this pipeline):
## these abundance figures are WHOLE-MEDITERRANEAN, not Western-Med-
## specific (ACCOBAMS ASI reports its design-based strata results as
## larger merged sub-areas, and this session's search didn't surface a
## Western-Med-only breakdown for these three species), and the body-
## mass figures are typical-adult approximations, not this study's own
## measurements - REVIEW BEFORE FULLY TRUSTING, but this ratio (pilot
## whale ~42%, Risso's dolphin ~44%, Cuvier's beaked whale ~15% of the
## FG's biomass) is a considerably better approximation than an even
## 33/33/33 split, which implies equal biomass despite Risso's dolphin
## and Cuvier's beaked whale differing by roughly 3x in body mass alone.
##
## Per project decision (pasted a detailed literature reconstruction):
## Zotier, Thibault & Guyot 1992 (Mediterranean-wide breeding-pair census,
## close to this pipeline's 1994-1996 baseline) gives real, documented
## breeding populations for the three species that make up "Pelagic/
## Offshore seabirds" (FG7) with a real population figure: Calonectris
## diomedea 57,000-76,000 pairs, Puffinus yelkouan 18,000 pairs "known",
## Hydrobates pelagicus 8,500-15,000 pairs "known" (Puffinus mauretanicus
## is the fourth FG7 species but the project's own notes give no population
## figure for it - left out of this weighting, not guessed). Converted to
## a relative biomass weight via breeding pairs x 2 (breeding adults only
## - non-breeders/juveniles are NOT included, a real, flagged
## underestimate of true standing biomass, but irrelevant to a RELATIVE
## weight as long as the undercount is roughly proportional across the
## three species) x typical adult body mass - species-typical mass is
## NOT from the project's literature (she didn't supply one) - Calonectris
## diomedea ~650g, Puffinus yelkouan ~400g, Hydrobates pelagicus ~28g,
## standard seabird-biology figures, flagged here as this session's own
## addition, not independently verified against a citation.
## CAVEATS, explicit rather than hidden: (1) Zotier et al. 1992 is
## Mediterranean-WIDE, not Western-Med-specific, same limitation as the
## ACCOBAMS-based cetacean weights below; (2) Puffinus yelkouan's 18,000
## pairs is documented as a floor ("known"), not a full census - Bourgeois
## & Vidal warn some at-sea count-based estimates for this species have
## been overestimated by 5-10x, so the lower/"known" figure is used
## deliberately, not the high end; (3) this is a RELATIVE weight for
## splitting FG7's own biomass across species - it does NOT set FG7's
## total biomass (that still comes from Ecopath_B/the megafauna manual
## CSV). KNOWN SIDE EFFECT: because Puffinus mauretanicus has no entry
## here, the override mechanism below (by construction, same as the
## existing Delphinus delphis = 0 case) assigns it a literal 0% share of
## FG7's biomass - that is a mechanical consequence of "unweighted
## species get 0", NOT a claim that Balearic shearwater biomass is
## actually zero. Add a real population figure for it (the project's notes
## flag Balearic-only breeding monitoring exists but gave no number here)
## to fix this properly.
## CORRECTION: an earlier note here claimed "Coastal/inshore
## seabirds" (FG8) has NO species assigned to it at all in the project's
## taxonomy - WRONG, confirmed by the project's own FG_spp screenshot: FG8
## has 11 real species (Larus audouinii/genei/melanocephalus/
## michahellis/ridibundus, 2x Phalacrocorax, Sterna albifrons/hirundo/
## nilotica/sandvicensis), all currently on the even 1/11 split
## (prop_sp_fg_basis = "even split"). A real per-species weighting for
## FG8 was researched again this session (WebSearch) specifically
## looking for a single comparable 1990-96 Mediterranean-wide breeding-
## pair count across all 11 species - found nothing usable: the best
## candidate (Zotier, Bretagnolle & Thibault 1999, J. Biogeography
## 26(2):297-313) could not be confirmed to tabulate gull/tern pair
## counts (paywalled); what WAS found for individual species is a mix
## of national counts, current-day (not 1990s) counts, and different
## reference years - not a coherent, comparable set, so assembling
## "weights" from it would be guessing with extra steps, not a real
## weighting. Left on the even split, still explicitly flagged via
## prop_sp_fg_basis - a genuinely real fix needs either that 1999 paper's
## actual table or Isenmann & Goutner 1993 (cited elsewhere as a
## Mediterranean gull/tern breeding-status source, not yet obtained).
## HISTORY (kept short - see the project doc
## `prop_sp_fg_species_weight_code_only_2026-09-29.md` for the full
## back-and-forth): per project decision ("the species weight within fg
## should be calculated inside the code, not created manually"), this is
## now a plain computed-in-R list rather than a CSV - same raw-components
## transparency (explicit Abundance x Body_mass_kg multiplication, not an
## opaque pre-multiplied number), just no longer a file to maintain,
## since these are DERIVED relative weights, not their own literature
## observation (see the REVERSAL note directly below for the reasoning
## on where that line sits).
##
## CAVEATS, still true of the values below: (1) Zotier et al. 1992
## (pelagic seabird pair counts) is Mediterranean-WIDE, not Western-Med-
## specific, same limitation as the ACCOBAMS-based cetacean weights;
## (2) Puffinus yelkouan's 18,000 pairs is documented as a floor
## ("known"), not a full census - Bourgeois & Vidal warn some at-sea
## count-based estimates for this species have been overestimated by
## 5-10x, so the lower/"known" figure is used deliberately, not the high
## end; (3) these are RELATIVE weights for splitting an FG's own biomass
## across species - they do NOT set the FG's total biomass (that still
## comes from Ecopath_B/the megafauna manual CSV). KNOWN SIDE EFFECT: any
## species with no entry here (e.g. Puffinus mauretanicus in FG7, or
## FG8's 11 coastal seabird species, for which no comparable 1990-96
## Mediterranean-wide breeding-pair count was found despite two research
## passes - see FG8 note above) mechanically gets a literal 0% or
## even-split share - NOT a claim that species' real biomass is zero.
## REVERSAL, per project decision ("the species weight within fg should be",
## " calculated inside the code, not created manually"): the CSV-loading
## mechanism above (SPECIES_FG_WEIGHT_PATH/load_species_fg_weights()) is
## REMOVED. The project's distinction, worked out over this session's
## back-and-forth: a manually-cited CSV
## belongs to a number that IS an actual observation from the literature
## (marine_megafauna_biomass.csv's Biomass_t rows, which also set
## Ecopath_B - see the "species-level split from the SAME literature
## figures..." block above) - that kind of number should live in an
## editable CSV so the project owner can add/correct it without a code change. A
## RELATIVE weight with no FG-total significance of its own (this list)
## isn't that - it is a derived quantity computed FROM literature
## abundance/body-mass figures that are already fixed citations, so it
## belongs in code as a transparent calculation, not in a second file to
## maintain. Every entry below is still computed from its own raw
## Abundance x Body_mass_kg components (kept as explicit multiplication,
## not a pre-multiplied opaque number - the same transparency the project owner
## asked for when this was still a CSV), just written directly in R.
##
## In practice, "Other dolphins" and "Pelagic/Offshore seabirds" below
## are no longer actually used - species_lit_biomass now covers both of
## them with a direct literature species-level split, and the loop below
## skips any FG species_lit_biomass already covers. They're kept here
## only as a safety-net fallback for the (currently hypothetical) case
## where marine_megafauna_biomass.csv doesn't exist on a given machine at
## all, so this loop still has a documented value to fall back to rather
## than silently reverting to an even split. "Deep sea-cetacean feeders"
## (Globicephala melas/Grampus griseus/Ziphius cavirostris) is the one FG
## this mechanism actually drives today - no FG-total entry for it exists
## in marine_megafauna_biomass.csv at all, so there's nothing for
## species_lit_biomass to cover it with.
MEGAFAUNA_FG_SPECIES_WEIGHTS <- list(
  ## Per project decision ("missing input biomass for delphinus
  ## delphis"): Stenella coeruleoalba basin-wide 1991 survey (Forcada et
  ## al. 1994, 117,880 individuals x 120 kg, ~593,660 km2, excl.
  ## Tyrrhenian) vs. Delphinus delphis's much smaller, Alboran-Sea-ONLY
  ## 1991 figure (Forcada & Hammond 1998, 14,736 individuals x 150 kg,
  ## ~90,670 km2) - the project's own worked calculation ("B_Delphinus_WMed <-
  ## 14736 * 0.150/1000/model_area_km2"), expressed as a RELATIVE weight.
  ## Fallback only - species_lit_biomass (from marine_megafauna_biomass_
  ## 1995_literature_additions.csv's own Species column) drives this FG
  ## directly in normal operation.
  "Other dolphins" = c(
    "Stenella coeruleoalba" = 117880 * 120,   # Forcada et al. 1994, basin-wide (excl. Tyrrhenian) 1991 survey, kg
    "Delphinus delphis"     = 14736 * 150     # Forcada & Hammond 1998, Alboran Sea ONLY 1991 survey, kg
  ),
  ## The one FG this mechanism actually applies to today (see header
  ## comment above) - ACCOBAMS Survey Initiative basin-wide abundance
  ## estimates (Table 4, Lauriano et al./ACCOBAMS 2023 synoptic
  ## assessment) x typical adult body mass (commonly cited species
  ## averages, not Western-Med-specific measurements).
  "Deep sea-cetacean feeders" = c(
    "Globicephala melas"  = 5540 * 1800,  # ACCOBAMS ASI individuals x typical adult mass (kg)
    "Grampus griseus"     = 26006 * 400,
    "Ziphius cavirostris" = 2929 * 1200
  ),
  ## Fallback only (see header comment) - species_lit_biomass drives this
  ## FG directly in normal operation. Zotier/Thibault/Guyot 1992
  ## Mediterranean-wide breeding-pair counts x 2 (breeding adults only)
  ## x typical adult body mass; Puffinus yelkouan deliberately uses the
  ## lower "known" floor, not a higher at-sea-count-based estimate
  ## (Bourgeois & Vidal warn some such estimates are overestimated 5-10x).
  "Pelagic/Offshore seabirds" = c(
    "Calonectris diomedea" = 66500 * 2 * 0.650,  # Zotier/Thibault/Guyot 1992 midpoint pairs x 2 x typical adult mass (kg)
    "Puffinus yelkouan"    = 18000 * 2 * 0.400,  # "known" floor, deliberately not the high end (Bourgeois & Vidal overestimate warning)
    "Hydrobates pelagicus" = 11750 * 2 * 0.028   # Zotier/Thibault/Guyot 1992 midpoint pairs x 2 x typical adult mass (kg)
  )
)
for (fg_nm in names(MEGAFAUNA_FG_SPECIES_WEIGHTS)) {
  weights <- MEGAFAUNA_FG_SPECIES_WEIGHTS[[fg_nm]]
  fg_rows <- FG_spp_Ecopath$FG_name == fg_nm
  if (!any(fg_rows)) {
    message("[FG_spp] MEGAFAUNA_FG_SPECIES_WEIGHTS names FG '", fg_nm,
            "' but it wasn't found in FG_spp_Ecopath - override skipped.")
    next
  }
  ## Skip any FG that already got a direct literature
  ## species-level split just above (species_lit_biomass) - that's the
  ## real per-species figure behind this FG's own Ecopath_B total, and
  ## should never be overwritten by this relative-abundance-only
  ## approximation. See the "species-level split from the SAME literature
  ## figures..." block above for which FGs that covers.
  if (unique(FG_spp_Ecopath[fg_rows]$FG_num) %in% lit_fg_nums) {
    message("[FG_spp] MEGAFAUNA_FG_SPECIES_WEIGHTS names FG '", fg_nm, "' but it already has a direct",
            " literature species-level biomass split (species_lit_biomass) - skipping this weaker",
            " relative-abundance-only override so the stronger, already-applied split isn't overwritten.")
    next
  }
  present_species <- FG_spp_Ecopath[fg_rows]$Species
  matched_species <- intersect(names(weights), present_species)
  if (length(matched_species) == 0) {
    message("[FG_spp] MEGAFAUNA_FG_SPECIES_WEIGHTS names FG '", fg_nm,
            "' but none of its named species (", paste(names(weights), collapse = ", "),
            ") were found among this FG's actual species (", paste(present_species, collapse = ", "),
            ") - override skipped. Check FG_WMed_2026.csv naming hasn't drifted.")
    next
  }
  total_weight <- sum(weights[matched_species])
  is_single_target <- length(matched_species) > 0 && all(weights[matched_species] %in% c(0, 1)) && total_weight == 1
  basis_label <- if (is_single_target) {
    "literature-targeted species (this FG's cited biomass figure is documented as specifically measuring this species - not a combined-species average)"
  } else {
    "relative-abundance-weighted (basin-wide ACCOBAMS Survey Initiative abundance x typical body mass - a documented approximation, not a Western-Med-specific measurement; see code comment for citation/caveats)"
  }
  for (sp in present_species) {
    w <- if (sp %in% matched_species) weights[[sp]] / total_weight else 0
    FG_spp_Ecopath[fg_rows & Species == sp, prop_sp_fg := w]
    FG_spp_Ecopath[fg_rows & Species == sp, prop_sp_fg_basis :=
                     if (w > 0) basis_label else paste0("0 - ", basis_label, " (this species' computed share rounds to zero)")]
  }
  message("[FG_spp] Species-level weighted override applied for FG '", fg_nm, "': ",
          paste(sprintf("%s = %.3f", matched_species, weights[matched_species] / total_weight), collapse = ", "),
          " (", if (is_single_target) "documented single-species target" else "relative-abundance weighting", ").")
}

## --- reconcile against Ecopath_B: species-level Biomass_t_km2 ---------
## Density/prop_sp_fg above are the SURVEY's own species-level density
## and each species' share of the survey total for its FG - correct as
## a proportion of what the survey itself saw, but Ecopath_B's own
## FG-level Biomass can come from a completely different source (stock
## assessment, EcoBase, marine-megafauna/primary-producer literature)
## whenever the survey doesn't sample that FG representatively (see the
## biomass_source priority cascade above) - so summing the raw Density
## numbers within an FG does NOT actually add up to Ecopath_B's total
## for those FGs. The project owner: "the proportion of biomass in the
## FG_spp doesn't correspond to Ecopath_B". Fixed by adding
## Biomass_t_km2 = prop_sp_fg * that FG's ACTUAL Ecopath_B Biomass, so
## summing Biomass_t_km2 within any FG always reconciles exactly to
## Ecopath_B, whichever source fed that FG's biomass. Density/prop_sp_fg
## are left unchanged alongside it - they're still the real survey
## observations, not overwritten, just no longer the only biomass-like
## number in this sheet.
FG_spp_Ecopath <- merge(FG_spp_Ecopath, ecopath_b_by_fg[, .(FG_num, Ecopath_B_Biomass = Biomass)],
                        by = "FG_num", all.x = TRUE)
FG_spp_Ecopath[, Biomass_t_km2 := prop_sp_fg * Ecopath_B_Biomass]
n_no_ecopath_b <- FG_spp_Ecopath[is.na(Ecopath_B_Biomass), uniqueN(FG_num)]
if (n_no_ecopath_b > 0) {
  message(n_no_ecopath_b, " FG(s) in FG_spp_Ecopath have no matching row in Ecopath_B at all",
          " (unexpected - Ecopath_B should cover every FG in dataframe2) - Biomass_t_km2 left NA",
          " for their species; check ecopath_b_by_fg/Ecopath_B directly.")
}
n_zero_biomass_fg <- FG_spp_Ecopath[!is.na(Ecopath_B_Biomass) & Ecopath_B_Biomass == 0 & prop_sp_fg > 0, uniqueN(FG_num)]
if (n_zero_biomass_fg > 0) {
  message(n_zero_biomass_fg, " FG(s) have a real survey-based prop_sp_fg split among species but Ecopath_B",
          " itself is 0 for that FG this baseline period - Biomass_t_km2 correctly comes out 0 for every",
          " species in these FG(s) too (there's no FG-level biomass to distribute).")
}

## --- Per project decision: "the density in the FG_spp should be in ---
## --- line with the Ecopath_B and it isnt right now" --------------------
## Until now FG_spp kept the RAW MEDITS/MEDIAS survey `Density` sitting
## right next to `Biomass_t_km2`/`Ecopath_B_Biomass`, disagreeing for
## every FG whose real Ecopath_B comes from somewhere other than the
## survey (megafauna/primary-producer literature, stock assessment,
## EcoBase) - e.g. a cetacean species showing Density = 0 alongside a
## real nonzero Ecopath_B_Biomass for the same row. Explained once
## already this session as "expected" (Density is a genuinely different
## measurement), but this is right that having two disagreeing
## biomass-like numbers side by side in the same sheet, under a column
## literally named "Density", reads as broken even when it isn't - and
## it's an easy source of real confusion for anyone opening FG_spp cold.
## Fixed by keeping the raw survey number (still needed - it's what
## computed prop_sp_fg via the density-ratio method above) but under an
## unambiguous name, `Density_survey_raw`, and making the sheet's own
## `Density` column BE the Ecopath_B-consistent value - identical to
## `Biomass_t_km2` (same t/km2 unit, same number), so `Density` always
## sums to that FG's real `Ecopath_B` total by construction, whatever
## sourced it. `Biomass_t_km2` is left in place too (03_pbqb-traits.R
## already reads it by that exact name from biomass_proportion_by_
## species_fg.csv - removing it would break that read) - it and the new
## `Density` are now simply the same number under two names, one for
## backward compatibility, one for what a reader opening FG_spp expects
## "Density" to mean.
setnames(FG_spp_Ecopath, "Density", "Density_survey_raw")
FG_spp_Ecopath[, Density := Biomass_t_km2]

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
        " with no observed survey density/prop_sp_fg = 0). FG_lookup: ", nrow(FG_lookup), " unique FGs.")

## Plain CSV of each species' share of its own FG's biomass - same
## prop_sp_fg column FG_spp_Ecopath carries (and that 04_diets.R reads
## straight out of the workbook for its diet-blending weights), just
## exported on its own for review/matching purposes without needing to
## open the workbook.
fwrite(FG_spp_Ecopath[, .(FG_num, FG_name, Species, Density, Density_survey_raw, prop_sp_fg, prop_sp_fg_basis, Ecopath_B_Biomass, Biomass_t_km2)],
       file.path(csv_out_dir, "biomass_proportion_by_species_fg.csv"))
message("Saved to biomass_proportion_by_species_fg.csv (Species x FG, prop_sp_fg = that species' share of its FG's total biomass",
        " (see prop_sp_fg_basis for whether that's a real density-weighted split or an even-split assumption);",
        " Density = Biomass_t_km2 = prop_sp_fg x that FG's own Ecopath_B Biomass, so it always sums to Ecopath_B exactly",
        " within each FG, whatever sourced that FG's total; Density_survey_raw is the original MEDITS/MEDIAS-observed",
        " density, kept for audit - it's what prop_sp_fg was computed from, but is NOT what 'Density' means in this sheet any more.)")

## --- review list: FGs whose species-level split is NOT a real
## measurement (i.e. NOT MEDITS/MEDIAS density-weighted and not a
## documented single-species/literature-weighted override like the
## cetacean fixes above) - just an equal split across the FG's own
## species, because no per-species density/abundance source exists for
## it at all. The project owner: "we need to break down the biomass
## of FG that were not extracted from medits medias or stock
## assessments" - this makes that list explicit (FG-level, sorted by
## how much total biomass is riding on the assumption) instead of it
## being visible only by reading prop_sp_fg_basis row by row. Any FG
## that shows up here is a candidate for the same treatment already
## applied to "Other dolphins"/"Deep sea-cetacean feeders" - i.e. find a
## real per-species literature/survey source and add it to
## MEGAFAUNA_FG_SPECIES_WEIGHTS (or an equivalently-named mechanism for
## non-megafauna groups).
needs_review <- FG_spp_Ecopath[grepl("^even split", prop_sp_fg_basis)]
if (nrow(needs_review) > 0) {
  fg_needs_review_summary <- needs_review[, .(
    n_species = .N,
    species_list = paste(sort(unique(Species)), collapse = "; "),
    FG_total_Biomass_t_km2 = sum(Biomass_t_km2, na.rm = TRUE)
  ), by = .(FG_num, FG_name)]
  setorder(fg_needs_review_summary, -FG_total_Biomass_t_km2)
  fwrite(fg_needs_review_summary, file.path(csv_out_dir, "fg_species_biomass_needs_review.csv"))
  message("Saved to fg_species_biomass_needs_review.csv - ", nrow(fg_needs_review_summary),
          " FG(s) (", sum(fg_needs_review_summary$n_species), " species total) whose per-species",
          " biomass split is an EVEN SPLIT with no real per-species source behind it (not MEDITS/",
          "MEDIAS density, not a documented literature override), sorted by how much total FG",
          " biomass rides on that assumption. Top of list = highest priority for a real source:")
  print(fg_needs_review_summary[1:min(10, .N)])
} else {
  message("fg_species_biomass_needs_review.csv: no FGs left on the even-split fallback - every",
          " multi-species FG now has either real survey density or a documented literature/",
          " relative-abundance override.")
}

## FG_spp_Ecopath and FG_lookup are native/intermediate
## reference tables, not final target sheets - written as CSV only.
## FG_lookup.csv in particular is read back by read_full_fg_reference()
## (used by 03/04's PB_QB and diet FG-expansion steps), so this must run
## before those.
write_native_sheets_csv(
  sheets  = list(
    FG_spp_Ecopath = FG_spp_Ecopath,
    FG_lookup      = FG_lookup
  ),
  out_dir = csv_out_dir
)

## FG_spp is one of the final workbook sheets (FG_num, FG_name, Species
## and taxonomy - the full species list for every FG, including
## reference-catalog species with zero observed density and the
## placeholder rows for FGs with none at all). Written directly to
## ecopath_ecosim_inputs.xlsx here since it's a final target sheet, not
## a native/intermediate table - unlike FG_spp_Ecopath/FG_lookup above,
## which stay CSV-only. Density/prop_sp_fg are kept alongside the
## taxonomy columns since they're already computed and useful context,
## not because the final spec requires them.
upsert_workbook_sheets(
  list(FG_spp = FG_spp_Ecopath),
  file.path(out_dir, "ecopath_ecosim_inputs.xlsx")
)

## =================================================================
## STEP 12: traits_ewe sheet - MOVED to 03_pbqb-traits.R.
## This script no longer reads FG_WMed.xlsx at all - the traits_ewe
## sheet is now built in 03_pbqb-traits.R, where it's reconciled
## against the correct FG_WMed_2026.csv numbering (via that script's
## own fg_ref_unique/species_df) instead of the stale FG numbers baked
## into the traits sheet's own header rows. See that script for the
## logic that used to live here.
## =================================================================

## =================================================================
## STEP 12b: AquaMaps_Depth_Adjustment audit sheet - only written when
## APPLY_AQUAMAPS_DEPTH_ADJUSTMENT was TRUE this run (see STEP 7b
## above). Per-species DepthMin/DepthPrefMin/DepthPrefMax/DepthMax,
## the shallow/deep probability masses and resulting multipliers, and
## WHY a species wasn't adjusted where that applies (note column) - so
## the adjustment behind fg_index/species_density_regional's numbers
## is reviewable per species, not a black box.
## =================================================================
if (!is.null(aquamaps_depth_adjustment_audit)) {
  ## Audit sheet, not a final target sheet - CSV only.
  write_native_sheets_csv(
    sheets  = list(AquaMaps_Depth_Adjustment = aquamaps_depth_adjustment_audit),
    out_dir = csv_out_dir
  )
}

## =================================================================
## STEP 12c: FG_Density_by_Stratum sheet - built whenever STRATA is
## TRUE (Step 8), independent of APPLY_AQUAMAPS_DEPTH_ADJUSTMENT: one
## row per FG, one column per depth stratum actually in play (the 5
## real MEDITS strata alone, or those plus the 3 AquaMaps pseudo-strata
## when the adjustment is on), plus Total_Density - which reproduces
## fg_index_regional's own YEAR_ECOPATH-averaged mean_density exactly
## (see build_fg_density_by_stratum_sheet()'s own comment on why the
## per-stratum columns are additive, not independently-normalized).
## =================================================================
if (!is.null(fg_density_by_stratum)) {
  ## Reference/audit sheet, not a final target sheet - CSV only.
  write_native_sheets_csv(
    sheets  = list(FG_Density_by_Stratum = fg_density_by_stratum),
    out_dir = csv_out_dir
  )
}

## =================================================================
## STEP 12b: FG_References + per-sheet Reference column - rebuilt HERE
## TOO, not only from 04_diets.R.
##
## Per project decision: "i dont see the references for each
## estimates of B, L, Di, PBQB, traits" - root cause: build_fg_
## references_sheet()/append_reference_columns_to_final_sheets() were
## ONLY ever called from 04_diets.R, which this pipeline's own docs
## describe as OPTIONAL ("04_diets.R is optional (diet composition,
## not every run needs it)" - see run_pipeline_demo.R's header
## comment). Anyone running just 01 -> 02 -> 03 (a very normal thing
## to do - diet composition is a separate concern from biomass/
## landings/PBQB/traits) got a workbook with NO Reference column on
## ANY sheet at all, and no FG_References sheet either, since the one
## place that built them never ran.
##
## Fixed by calling both functions here too (and in 02_fisheries.R,
## 03_pbqb-traits.R, right before each script's own call to
## trim_workbook_to_final_sheets() below) - same "safe to run any
## time, skips what's not ready yet" convention build_fg_references_
## sheet() already documents in its own header (each source CSV it
## reads either exists by now or doesn't, and it messages + leaves
## that column blank rather than erroring either way). Whichever
## script runs LAST simply has the most complete picture to build
## from - running it here too means B_ref (and Ecopath_B's own
## Reference column) is already populated and correct even if 02/03/04
## never run at all.
build_fg_references_sheet(file.path(out_dir, "ecopath_ecosim_inputs.xlsx"),
                          biomass_csv_dir   = csv_out_dir,
                          fisheries_csv_dir = file.path(out_dir, "fisheries"),
                          pbqb_csv_dir      = file.path(out_dir, "pbqb-traits"),
                          diet_csv_dir      = file.path(out_dir, "diet"))
append_reference_columns_to_final_sheets(file.path(out_dir, "ecopath_ecosim_inputs.xlsx"))

## =================================================================
## STEP 13: trim ecopath_ecosim_inputs.xlsx down to EXACTLY the 9 final
## target sheets (info, FG_spp, Ecopath_B, Ecopath_L, Ecopath_Di,
## Ecopath_PBQB, Ecopath_traits, Ecopath_diet, Ecosim_ts) - the excel
## ecopath_ecosim file must have exactly the intended sheets, trimmed
## script by script. Safe to run from EVERY script that touches this
## workbook, in any order - trim_workbook_to_final_sheets() (via
## finalize_workbook_sheet_order(..., drop_extras = TRUE)) skips target
## sheets that don't exist yet and DROPS anything else, so whichever
## script runs LAST naturally leaves the workbook holding only whichever
## of the 9 final sheets exist so far - never any native/intermediate
## sheet, since those are all CSV-only now. Add this same call to
## 02_fisheries.R, 03_pbqb-traits.R, and 04_diets.R too.
## =================================================================
trim_workbook_to_final_sheets(file.path(out_dir, "ecopath_ecosim_inputs.xlsx"))

## =================================================================
## STEP 14: full FG species catalog (matched + needs-review), combining
## MEDITS + MEDIAS + stock assessment - reads back the all_species_*_
## full_extent.csv snapshots taken during Steps 3-4/9/stock-assessment
## above, each already covering that source's WHOLE available extent
## regardless of THIS run's FILTER_AREAS/YEAR_ECOPATH/TS_YEARS (see
## build_full_fg_species_catalog()'s own header comment in
## lib_survey_fg_density_functions.R for the full rationale). Use this
## as the starting point for a genuinely complete, next-generation
## FG_WMed.xlsx sheet 4.
## =================================================================
build_full_fg_species_catalog(csv_out_dir)

## =================================================================
## STEP 15: completeness check - which FGs have NO Ecopath_B biomass
## at all (per project decision: "print the FGs at the end of the
## scripts that dont have data on Ecopath B, or L or Di"). Checked
## against fg_index_regional_combined (the exact table that feeds
## Ecopath_B via export_ecopath_ecosim_excel() in STEP 10), restricted
## to the Ecopath base year(s) (YEAR_ECOPATH) - an FG with no row at
## all there, or a row whose mean_density is NA or exactly 0, has no
## real biomass estimate behind it from ANY source (survey, stock
## assessment, ICCAT, marine megafauna, or primary producer/plankton),
## regardless of which fallback ultimately would have applied.
## =================================================================
## Mirrors the same nearest-year borrow fallback applied to
## Ecopath_B itself (export_ecopath_ecosim_excel()) so this check
## doesn't wrongly flag an FG that the fallback already fixed - an FG
## still counted "missing" here has no usable biomass even AFTER
## borrowing from another survey year (or is a real "Expanding" group,
## deliberately excluded from the fallback).
fg_biomass_base_year_fallback <- resolve_baseline_with_nearest_year_fallback(
  fg_index_regional_combined[, .(Year, FG_num, FG_name, mean_density)],
  id_cols = c("FG_num", "FG_name"), value_col = "mean_density", baseline_years = YEAR_ECOPATH)
fg_biomass_base_year <- fg_biomass_base_year_fallback[, .(FG_num, FG_name, mean_density = final_value)]
fg_missing_biomass <- merge(full_fg_list, fg_biomass_base_year, by = c("FG_num", "FG_name"), all.x = TRUE)
fg_missing_biomass <- fg_missing_biomass[is.na(mean_density) | mean_density == 0]

## =================================================================
## STEP 15b: EcoBase last-resort fallback for whatever's still missing
## (per project decision: "try to get biomass from other models
## [EcoBase] for the ones without estimates" - bluefin tuna is the
## motivating case, deliberately left with no density earlier in this
## script rather than a guessed spatial split - see the ICCAT block's
## own comment). Unlike the EARLIER, narrowly-scoped EcoBase call above
## (fixed list: zooplankton/macroalgae/seagrass/gorgonians+corals),
## this one runs against WHATEVER is still in fg_missing_biomass at
## this point, regardless of which FG it is - keywords are derived
## automatically from the FG's own name (split into words) plus its
## ScientificName where it's a single-species FG, so no hardcoded
## species list needs maintaining here. Same "closest available, not a
## real measurement, flagged not guessed" principle as every other
## EcoBase use in this pipeline - every filled value is written to its
## own REVIEW csv naming the source model, and this only patches the
## Ecopath_B baseline-year snapshot (fg_biomass_base_year) - it does
## NOT backfill the full Ecosim_ts time series for these FG(s), which
## will still show NA/0 outside YEAR_ECOPATH unless a real time series
## is sourced separately (known, flagged scope limit, not an oversight).
## =================================================================
## Per project decision - two explicit exclusions from the
## EcoBase/literature fallback below, decided directly rather than left
## to a plausibility check:
##   (1) "dont use any literature or ecobase values for fish species
##       because there are survey data for fish biomass FG" - MEDITS/
##       MEDIAS survey data is the intended source for every fish FG, so
##       no fish FG is ever backfilled from an unrelated EcoBase model
##       here, even if its own survey value is genuinely 0/NA.
##   (2) "for expanding FGs, dont use biomass from literature, keep it
##       like MEDITS" - kept as its OWN explicit check (not just relied
##       on as a special case of (1)) even though every current
##       "Expanding ... fish" FG is already covered by the fish check,
##       so the exclusion still holds if a future non-fish "Expanding"
##       FG is ever added to FG_WMed_2026.csv.
## Also excluded, for a different reason: Detritus/Discards are
## non-living Ecopath compartments, not animal/plant FGs - an EcoBase
## model's own "detritus"-named group is never a real analogue for THIS
## study area's detrital pool. This is exactly the mechanism that
## produced the Bay of Biscay (Expanding omnivore/herbivore fish) and
## Port Cros (European hake adult) mismatches flagged in the code review.
##
## Fish classification is read from dataframe2 - the UNDEDUPED
## species->FG catalog straight from FG_WMed_2026.csv - not
## fg_lookup_safe, because prepare_fg_lookup()'s stanza dedup (see that
## function's own header comment in lib_survey_fg_density_functions.R)
## would otherwise make a stanza FG like "European hake adult" invisible
## to this check (zero species survive the dedup for whichever stanza
## has the higher FG_num). FISH_CLASSES mirrors 03_pbqb-traits.R's own
## constant of the same name, so "fish" means the same thing in both
## scripts. A FG_name containing "fish" is also treated as fish even
## with zero matched species (covers a genuinely species-empty
## "Expanding ... fish" FG).
FISH_CLASSES <- c("Teleostei", "Elasmobranchii", "Chondrichthyes", "Actinopteri",
                  "Actinopterygii", "Myxini", "Petromyzonti", "Holocephali")
NON_LIVING_FG_NAMES <- c("Detritus", "Discards")

fg_species_class <- merge(unique(dataframe2[, .(FG_num, ScientificName)]),
                          species_taxonomy[, .(ScientificName, Class)],
                          by = "ScientificName", all.x = TRUE)
fg_is_fish_lookup <- fg_species_class[, .(fg_has_fish_species = any(Class %in% FISH_CLASSES, na.rm = TRUE)), by = FG_num]

fg_missing_biomass[, is_fish_fg := (FG_num %in% fg_is_fish_lookup[fg_has_fish_species == TRUE, FG_num]) |
                     grepl("fish", FG_name, ignore.case = TRUE)]
fg_missing_biomass[, is_expanding_fg := grepl("Expanding", FG_name)]
fg_missing_biomass[, is_non_living_fg := FG_name %in% NON_LIVING_FG_NAMES]

fg_missing_biomass_excluded <- fg_missing_biomass[is_fish_fg | is_expanding_fg | is_non_living_fg]
fg_missing_biomass_eligible <- fg_missing_biomass[!(is_fish_fg | is_expanding_fg | is_non_living_fg)]

if (nrow(fg_missing_biomass_excluded) > 0) {
  message("\n[Completeness check] ", nrow(fg_missing_biomass_excluded), " of ", nrow(fg_missing_biomass),
          " still-missing FG(s) are EXCLUDED from the EcoBase/literature fallback below, per project decision:",
          " fish FGs (MEDITS/MEDIAS survey data is their intended source, no literature/",
          " EcoBase substitute), \"Expanding\" FGs (a real baseline zero is the correct signal), and",
          " Detritus/Discards (non-living compartments, not a real analogue for any EcoBase model's own",
          " detritus group). These stay genuinely missing (0/NA) here rather than getting a biologically-",
          " unrelated stand-in value:")
  print(fg_missing_biomass_excluded[, .(FG_num, FG_name, is_fish_fg, is_expanding_fg, is_non_living_fg)])
}

if (nrow(fg_missing_biomass_eligible) > 0 &&
    (!exists("ENABLE_ECOBASE_BIOMASS_QUERY", envir = .GlobalEnv, inherits = FALSE) || ENABLE_ECOBASE_BIOMASS_QUERY)) {
  missing_fg_keywords <- lapply(seq_len(nrow(fg_missing_biomass_eligible)), function(i) {
    fg_num_i <- fg_missing_biomass_eligible$FG_num[i]
    words <- unique(tolower(strsplit(fg_missing_biomass_eligible$FG_name[i], "[^A-Za-z]+")[[1]]))
    words <- words[nchar(words) >= 4]  # drop tiny fragments ("and", "of", "sp", ...) that would over-match
    sci_words <- if (exists("fg_lookup_safe")) tolower(unique(fg_lookup_safe[FG_num == fg_num_i, ScientificName])) else character(0)
    unique(c(words, sci_words))
  })
  names(missing_fg_keywords) <- as.character(fg_missing_biomass_eligible$FG_num)
  missing_fg_keywords <- missing_fg_keywords[lengths(missing_fg_keywords) > 0]
  
  if (length(missing_fg_keywords) > 0) {
    ecobase_missing_fill <- fetch_ecobase_literature_biomass(
      out_dir = csv_out_dir, force_refresh = FALSE,
      target_fg_keywords = missing_fg_keywords, target_year = round(mean(YEAR_ECOPATH))
    )
    ## Per project decision ("ecobase biomass should be evaluated but it
    ## shouldnt be incorporated in the Ecopath_B"): EcoBase stays an
    ## EVALUATION step only - still queried, still reported, but never
    ## allowed to fill Ecopath_B itself (no patch to
    ## fg_biomass_base_year$mean_density or to
    ## survey_fg_annual_index_regional_combined.csv's biomass_source).
    ## fg_missing_biomass_eligible FGs with no other source stay
    ## genuinely missing (0/NA) in Ecopath_B, same as fish/Expanding/
    ## Detritus-Discards FGs already do above - EcoBase's candidate is
    ## still written out below, purely as a reference for a human to look
    ## at and decide whether to add a REAL literature/survey/stock-
    ## assessment row by hand (same path as every other manual-cited
    ## biomass source in this script), not something the pipeline applies
    ## on its own.
    ##
    ## The plausibility check (implausibly-large candidate vs. this
    ## model's own known densities) is kept as a column on the evaluation
    ## output rather than a gate on incorporation, since nothing is being
    ## incorporated any more - it's still useful context for whoever
    ## reviews the candidate by hand.
    if (!is.null(ecobase_missing_fill) && nrow(ecobase_missing_fill) > 0) {
      ecobase_missing_fill[, FG_num := as.integer(TargetGroup)]
      ecobase_missing_fill[, Biomass_t := Biomass_t_km2 * sum(strata_area_by_area$area_km2, na.rm = TRUE)]
      
      if (!exists("PLAUSIBILITY_MAX_DENSITY_MULTIPLE", envir = .GlobalEnv, inherits = FALSE)) PLAUSIBILITY_MAX_DENSITY_MULTIPLE <- 8
      max_known_density <- suppressWarnings(max(fg_biomass_base_year[!is.na(mean_density) & mean_density > 0, mean_density], na.rm = TRUE))
      if (is.finite(max_known_density) && max_known_density > 0) {
        implausible_threshold <- max_known_density * PLAUSIBILITY_MAX_DENSITY_MULTIPLE
        ecobase_missing_fill[, plausible := Biomass_t_km2 <= implausible_threshold]
      } else {
        ecobase_missing_fill[, plausible := NA]
      }
      
      fwrite(ecobase_missing_fill[, .(FG_num, Biomass_t_km2, Biomass_t, EwE_model, ecosystem_name, year, Source_citation, plausible)],
             file.path(csv_out_dir, "ecobase_biomass_evaluation_NOT_INCORPORATED.csv"))
      message("\n[Completeness check] EcoBase evaluation (other published Mediterranean models, keyword-matched",
              " on each missing FG's own name/scientific name) found a candidate for ", nrow(ecobase_missing_fill),
              " of the ", nrow(fg_missing_biomass_eligible), " non-fish/non-Expanding/non-Detritus-Discards FG(s)",
              " that had no biomass from any other source - see ecobase_biomass_evaluation_NOT_INCORPORATED.csv.",
              " Per the project owner, these are REFERENCE ONLY and are NOT applied to Ecopath_B - these FG(s)",
              " remain in fg_missing_ecopath_B_REVIEW.csv below until a real literature/survey/stock-assessment",
              " value is added by hand.")
    } else {
      message("\n[Completeness check] EcoBase evaluation found no matching group for any of the ",
              nrow(fg_missing_biomass_eligible), " still-missing, eligible FG(s) - see",
              " ecobase_literature_biomass_full.csv to check the keyword match by hand (auto-derived",
              " keywords from the FG's own name can miss EcoBase's actual group naming convention).")
    }
  }
}

if (nrow(fg_missing_biomass) > 0) {
  fwrite(fg_missing_biomass[, .(FG_num, FG_name)], file.path(csv_out_dir, "fg_missing_ecopath_B_REVIEW.csv"))
  message("\n[Completeness check] ", nrow(fg_missing_biomass), " of ", nrow(full_fg_list),
          " FG(s) have NO Ecopath_B biomass at all for ", paste(range(YEAR_ECOPATH), collapse = "-"),
          " (no survey/stock-assessment/ICCAT/megafauna/primary-producer source supplied a value) -",
          " written to fg_missing_ecopath_B_REVIEW.csv:")
  print(fg_missing_biomass[, .(FG_num, FG_name)])
} else {
  message("\n[Completeness check] Every one of ", nrow(full_fg_list),
          " FG(s) has a nonzero Ecopath_B biomass value for ", paste(range(YEAR_ECOPATH), collapse = "-"), ".")
}

message("\nDone. Outputs in ", out_dir, " and ", plot_dir)
message("Run finished: ", Sys.time())

## --- Close run log --------------------------------------------------------
sink(type = "message")
sink()
close(.run_log_con)