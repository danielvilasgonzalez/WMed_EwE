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
## 2026-09-19: merged from what used to be two separate scripts
## (01_biomass.R for Western Med, 01_survey_density_custom.R for a
## custom region) into this one file, so there's a single script to
## maintain and a single set of fixes/improvements that both regions
## benefit from - see AREA_MODE's own comment for what's genuinely
## region-specific vs. shared.
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
fg_file          <- resolve_pcloud_file(paste0(pcloud_dir,"/data/FG_WMed.xlsx"), pcloud_dir)
#taxonomy list of MEDITS and MEDIAS species and code species
#downloaded from MEDITS website
tm_list_file     <- resolve_pcloud_file(paste0(pcloud_dir,"/data/Medits_Medias_JRC2026/2024_MEDBSsurvey/TM_list_(April_2019).xlsx"), pcloud_dir)

## plot_dir is INSIDE out_dir (out_dir/plots), not two directories up
## from it - both are created (recursive=TRUE, in case the full parent
## path doesn't exist yet) rather than assuming either already exists.
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
plot_dir <- file.path(out_dir, "plots")
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

## 2026-09-17 update: this block's own native/intermediate CSV outputs
## (everything below EXCEPT the shared ecopath_ecosim_inputs.xlsx
## workbook, which stays at the top-level out_dir since 02/03/04 all
## read/write it too) now go into their own "biomass" subfolder, so a
## flat out_dir isn't a mix of all four blocks' files. Other blocks
## that read this block's CSVs (species_density_regional_combined.csv,
## strata_area_by_area.csv, FG_lookup.csv, Ecosim.csv) point at this
## same subfolder explicitly - see 02_fisheries.R/03_pbqb-traits.R/
## 04_diets.R.
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
if (!exists("TS_YEARS",      envir = .GlobalEnv, inherits = FALSE)) TS_YEARS      <- 1995:2023     # function attribute: ts_years (NULL -> min:max of data)
if (!exists("DROP_OUTLIERS", envir = .GlobalEnv, inherits = FALSE)) DROP_OUTLIERS <- TRUE         # function attribute: whether remove_sample_outliers() actually
if (!exists("NORMALIZE_TS",  envir = .GlobalEnv, inherits = FALSE)) NORMALIZE_TS  <- TRUE         # TRUE = Ecosim series rescaled to reference index (first value = 1); FALSE = raw density
# removes flagged observations (TRUE) or only reports them (FALSE)
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

## taxonomy_source = "both" (2026-09-17): FishBase/SeaLifeBase is
## fish/aquatic-organism-focused and genuinely doesn't carry every
## algae/sponge/bryozoan/echinoderm/crustacean name in the survey data
## (e.g. Osmundaria volubilis, Anamathia rissoana, Ergasticus clouei,
## Lissa chiragra came back with zero taxonomy from FishBase/SeaLifeBase
## alone). WoRMS (World Register of Marine Species) covers the full
## marine taxonomic tree - algae, invertebrates of every phylum, and
## bare higher-rank names themselves (e.g. "Porifera" is itself a valid
## WoRMS phylum-rank record) - so it's used as a SECOND-PASS fallback,
## only for whatever FishBase/SeaLifeBase left with no rank filled in at
## all. FishBase/SeaLifeBase stays the first authority wherever it does
## resolve a name, so nothing already working changes.
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
## shared functions, not hardcoded inside them.
## Bounds written per spec: 10-50, 51-100, 101-200, 201-500,
## 501-800m - i.e. each stratum's own upper bound (50/100/200/500/800)
## belongs to THAT stratum, not the next one down. assign_depth_stratum()
## only calls findInterval() on depth_min (see its own comment on why
## depth_max isn't used for the interval logic itself, only for the final
## out-of-range exclusion check) - so depth_min is set one unit ABOVE each
## printed lower bound (51, 101, 201, 501) to push the boundary depth
## itself (e.g. Depth == 50) into the SHALLOWER stratum, matching the
## "10 to 50" / "51 to 100" wording exactly. Previously this used
## depth_min = c(10,50,100,200,500) with depth_max = *.99, which instead
## put Depth == 50 into the 50-100 stratum - functionally almost
## identical for continuous depth data, but not what was asked for.
MEDITS_STRATA <- data.table(
  stratum_num = 1:5,
  depth_min = c(10, 51, 101, 201, 501),
  depth_max = c(50, 100, 200, 500, 800))

## Area definition - branches on AREA_MODE. Both branches end with the
## same three things defined: area_shp (an sf polygon set), AREA_ID_COL
## (the column in area_shp identifying each polygon), and FILTER_AREAS
## (which values of that column are actually in scope) - everything
## downstream of here reads only those three, never AREA_MODE itself.
download_gfcm_gsa_shapefile <- function() {
  ## GFCM GSA shapefile - shared by AREA_MODE == "westmed" and by
  ## AREA_MODE == "custom" + CUSTOM_AREA_TYPE == "gsa" (a custom GSA
  ## grouping still starts from these same official polygons).
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
## Species codes not in the MEDITS taxonomic list at all - this table
## is genuinely specific to this survey's code list and this FG
## reference scheme, so it stays here rather than in the shared library.
MANUAL_OVERRIDES <- data.table(
  species_code = c("ARGRACU", "FMBONEL", "GASTRDA", "ILLESPP", "BUCCSPP", "ASCDCEA", "PTEDGRI",'SOLEAEG'),
  taxon_name   = c("Argyropelecus aculeatus", "Bonellidae", "Gastropoda", "Illex",
                   "Buccinum", "Ascidiacea", "Pteroeides griseum", 'Solea aegyptiaca'),
  taxon_rank   = c("species", "family", "class", "genus", "genus", "class", "species",'species'))

## needs taxonomy on fg_lookup_safe to resolve genus/family/class-rank overrides
fg_taxonomy <- fetch_taxonomy(fg_lookup_safe$ScientificName, taxonomy_source = "both", cache_path = FISHBASE_TAXONOMY_CACHE_PATH, worms_cache_path = WORMS_TAXONOMY_CACHE_PATH)
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
## (an upstream column-concatenation artifact), egg-capsule entries,
## and (2026-09-17 review addition) three more confirmed non-taxon
## entries flagged after inspecting real fallback output: "Sea ball of
## Posidonia oceanica" (a detritus/plant-debris category, not an
## animal), "shell debris", and "Leaves of Posidonia oceanica" - none
## of these are real taxa and would just waste a WoRMS query for
## nothing (or, worse, get force-matched to an unrelated FG by the
## bare-word/genus fallback below); setting ScientificName to NA here
## only affects the fallback match attempt, not the underlying
## observation rows themselves.
## str_detect() on an already-NA ScientificName returns NA (not FALSE),
## so non_taxon itself legitimately contains NAs wherever ScientificName
## was already NA - data.table's `dt[non_taxon, ...]` already treats
## those NA entries as "not selected" (same as FALSE), so the actual
## exclusion logic here was always correct; only sum(non_taxon) below
## was wrong (NA propagates through sum() without na.rm, printing "NA"
## instead of a real count, which is what made this look broken/stuck).
NON_TAXON_LITERAL_NAMES <- c("Sea ball of Posidonia oceanica", "shell debris", "Leaves of Posidonia oceanica")
non_taxon <- str_detect(dt$ScientificName, "^NO\\b") | str_detect(dt$ScientificName, regex("eggs?", ignore_case = TRUE)) |
  (dt$ScientificName %in% NON_TAXON_LITERAL_NAMES)
dt[non_taxon, ScientificName := NA_character_]
message(sum(non_taxon, na.rm = TRUE), " non-taxon row(s) (NO-prefixed, egg-capsule, or a literal non-taxon",
        " name - Sea ball/Leaves of Posidonia oceanica, shell debris) excluded from the taxonomy fallback attempt.")

dt <- fallback_match_fg_by_taxonomy(dt, fg_lookup_safe, taxonomy_source = "both", cache_path = FISHBASE_TAXONOMY_CACHE_PATH, worms_cache_path = WORMS_TAXONOMY_CACHE_PATH)

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
## 2026-09-17 review addition: three more Phylum-level entries for
## unmatched taxa flagged after inspecting real fallback output
## - "Rhodophyta"/"Chlorophyta" (red/green algae) -> "Seaweeds", and
## "Ectoprocta"/"Brachiopoda"/"Scaphopoda" -> "Other macro-benthos"
## (Ectoprocta is the modern name for the SAME phylum "Bryozoa"
## already seeded below - added as its own explicit row rather than
## assumed synonymous, since which name a given taxonomy source
## returns - FishBase vs WoRMS - isn't guaranteed consistent). VERIFY
## the "Seaweeds" FG_name spelling against the actual FG reference
## file (fg_wmed_95) before relying on this - if that FG is named
## something else (e.g. "Macroalgae"), this row silently no-ops
## (apply_seed_fg_rules() only assigns rows whose fg_name_target
## exists as a real FG_name in dataframe2) rather than erroring, so a
## typo here won't crash the run but also won't seed anything.
SEED_RULES <- data.table(
  rank = c(
    "Class", "Class", "Class", "Class",
    "Class", "Class",
    "Order", "Order", "Order", "Order", "Order", "Order",
    "Family", "Family",
    "Phylum", "Phylum", "Phylum",
    "Phylum", "Phylum", "Phylum", "Phylum", "Phylum"
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
    "Porifera",
    "Rhodophyta",
    "Chlorophyta",
    "Ectoprocta",
    "Brachiopoda",
    "Scaphopoda"
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
    "Other macro-benthos",
    "Seaweeds",
    "Seaweeds",
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
  fwrite(fallback_detail, file.path(csv_out_dir, "taxonomy_fallback_matches_for_review.csv"))
  message("Saved to taxonomy_fallback_matches_for_review.csv (", nrow(fallback_detail), " species assigned via",
          " taxonomy fallback - ", fallback_detail[match_type == "majority", .N], " of those by majority vote,",
          " ", fallback_detail[match_type == "exclusive", .N], " by unanimous agreement among relatives).")
}

message("\nFinal FG match rate: ", dt[!is.na(FG_num), uniqueN(ScientificName)], " of ",
        dt[, uniqueN(ScientificName)], " distinct species matched.")

## Species presence check (SPECIES_PRESENCE_CHECK, set above) - for
## each species, how many DISTINCT HAULS (SampleID) actually caught it
## in each year, checked against the FULL 1994-2023 MEDITS time series
## (not just TS_YEARS, which is the shorter Ecosim-output window and
## may start later - this check is about the raw survey record).
## Deliberately BEFORE outlier removal/catchability correction below,
## since this is about whether/how often the species was ever caught at
## all, not about its corrected density. Checked on Biomass (dt's own
## raw-catch column at this point in the pipeline - Density doesn't
## exist yet, it's only computed later on the separate FG_spp_Ecopath
## table), and > 0 (not just !is.na) specifically - some survey formats
## carry an explicit zero-catch row per haul per species checked for,
## which would otherwise inflate "presence" to mean "was checked for"
## rather than "was caught".
if (!is.null(SPECIES_PRESENCE_CHECK)) {
  full_survey_years <- 1994:2023
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
  cache_path = file.path(csv_out_dir, "strata_area_by_area.csv"))

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
  p_area_weights <- plot_area_weight_donut(fg_index, strata_def_for_plotting)
  ggsave(file.path(plot_dir, "survey_area_weight_donut.png"), p_area_weights,
         width = 14, height = 10, dpi = 150, bg = "white")
}

## =================================================================
## STEP 9: MEDIAS acoustic survey + GFCM stock-assessment biomass +
## the MEDITS/MEDIAS/stock-assessment priority combine - AREA_MODE ==
## "westmed" ONLY. Every data source in this whole step is scoped to
## real, named GSAs (MEDIAS Handbook Table 1 areas, GFCM STAR/RAM
## Legacy species-by-GSA assessments, the "Western Mediterranean"
## subregion filter below) - none of it has a meaning for a custom
## bbox/shapefile boundary that doesn't follow GSA lines. AREA_MODE ==
## "custom" skips straight to the MEDITS-only else{} branch at the
## bottom of this step, aliasing the *_combined variables Step 10
## onward reads either way, so nothing downstream needs to know which
## branch actually ran.
## =================================================================
if (AREA_MODE == "westmed") {
  
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
  
  ## Same manual-rules tier and manual-review export MEDITS gets above
  ## (SEED_RULES/SPECIES_EXCEPTIONS - defined once above, reused verbatim
  ## here since it's the same FG scheme) - previously skipped for MEDIAS,
  ## which meant a species matchable only via a manual rule (e.g. a
  ## zero-reference FG) silently stayed unmatched here even though the
  ## identical species would have resolved for MEDITS, and nothing
  ## unmatched ever got written out for review. summarize_unresolved_species()
  ## expects a `Biomass` column (MEDITS' own convention) - acoustic_matched
  ## carries the same value under `total_biomass`, aliased here rather than
  ## renamed so nothing downstream that expects `total_biomass` breaks.
  acoustic_matched <- apply_seed_fg_rules(acoustic_matched, dataframe2, SEED_RULES, species_exceptions = SPECIES_EXCEPTIONS)
  acoustic_taxonomy_context <- attr(acoustic_matched, "still_unresolved_taxonomy")
  acoustic_matched[, Biomass := total_biomass]
  medias_still_unmatched <- summarize_unresolved_species(acoustic_matched, taxonomy = acoustic_taxonomy_context)
  acoustic_matched[, Biomass := NULL]
  fwrite(medias_still_unmatched, file.path(csv_out_dir, "medias_unmatched_for_manual_review.csv"))
  message("Saved to medias_unmatched_for_manual_review.csv for review.")
  
  ## Same audit trail as the MEDITS side above (see its comment for the
  ## full rationale) - species assigned via the taxonomy fallback for the
  ## MEDIAS/acoustic data, split by match_type ("exclusive" vs "majority").
  acoustic_fallback_detail <- attr(acoustic_matched, "fallback_match_detail")
  if (!is.null(acoustic_fallback_detail) && nrow(acoustic_fallback_detail) > 0) {
    fwrite(acoustic_fallback_detail, file.path(csv_out_dir, "medias_taxonomy_fallback_matches_for_review.csv"))
    message("Saved to medias_taxonomy_fallback_matches_for_review.csv (", nrow(acoustic_fallback_detail), " species assigned via",
            " taxonomy fallback - ", acoustic_fallback_detail[match_type == "majority", .N], " by majority vote,",
            " ", acoustic_fallback_detail[match_type == "exclusive", .N], " by unanimous agreement among relatives).")
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
      cache_path = file.path(csv_out_dir, "medias_area_10_200m_fallback.csv")
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
  
  fwrite(acoustic_fg_by_gsa, file.path(csv_out_dir, "medias_fg_annual_density_by_area.csv"))
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
  ## Stock-assessment (GFCM STAR / RAM Legacy) biomass, matched to FG x
  ## Year - built HERE, BEFORE the MEDITS+MEDIAS combine step below, so
  ## it can feed the priority rule that combine step applies
  ## (single-species / stanza FGs use it as PRIMARY; every other FG
  ## keeps it validation-only, "the
  ## priority of FG estimates MEDIAS>MEDITS, then if FG single species
  ## and have stock assessment then stock assessment, if a species is
  ## FG stanza then stock assessment. the rest MEDITS" - see the "FG
  ## biomass-source priority" block below for the actual rule, which
  ## replaced an earlier demersal/small-pelagic-keyword-based version).
  ## Expects the combine_STAR_RAMlegacy.R output,
  ## combined_medbs_star_ramlegacy.csv (source/stock_key/species/
  ## common_name/gsa/subregion/year/biomass/catches/landings/
  ## landings_flag/... - see that script's own header), placed under
  ## STAR_RAMLEGACY_DIR. Optional - message + skip if not found, same
  ## convention as every other optional source in this pipeline.
  ## =================================================================
  extract_genus <- function(sci_name) str_extract(sci_name, "^[A-Za-z]+")  # pull the genus (first word) from a scientific name
  
  STAR_RAMLEGACY_DIR <- file.path(pcloud_dir, "data/fisheries/STAR_RAMLegacy")  # folder for combine_STAR_RAMlegacy.R's own output CSV
  star_ram_path <- file.path(STAR_RAMLEGACY_DIR, "combined_medbs_star_ramlegacy.csv")
  star_ram_combined <- if (file.exists(star_ram_path)) {
    fread(star_ram_path, encoding = "UTF-8")
  } else {
    message("\n[Stock-assessment biomass] '", star_ram_path, "' not found - skipping (run",
            " combine_STAR_RAMlegacy.R first and place its output under STAR_RAMLEGACY_DIR",
            " if you want stock-assessment biomass available for the priority rule below).")
    data.table()
  }
  
  ## Empty but with the right COLUMNS, not a bare data.table() - a
  ## zero-column empty data.table crashes any later `dt[, .(FG_num, ...)]`
  ## column select with "object 'FG_num' not found" the moment
  ## combine_STAR_RAMlegacy.R's output genuinely isn't there (the normal,
  ## documented "optional, skip if not found" case above, not an error
  ## condition) - this is what was hit during testing.
  stock_assessment_fg_year <- data.table(FG_num = integer(0), FG_name = character(0), Year = integer(0),
                                         star_biomass_t = numeric(0), star_n_stocks = integer(0),
                                         star_sources = character(0), stock_assessment_density_t_km2 = numeric(0))
  if (nrow(star_ram_combined) > 0) {
    ## Restrict to the West Med subregion, same convention as the
    ## fisheries-side STAR/RAM catch cross-check (02_fisheries.R).
    star_wm <- star_ram_combined[grepl("Western Mediterranean", subregion, fixed = TRUE)]
    star_wm[, genus := extract_genus(species)]
    
    ## Species -> FG_num, exact scientific-name match first, genus
    ## fallback second - identical cascade to the fisheries-side match,
    ## reusing fg_lookup_safe's own Genus column (from fetch_taxonomy())
    ## rather than re-deriving genus for every FG species.
    star_direct <- merge(star_wm, fg_lookup_safe[, .(ScientificName, FG_num, FG_name)],
                         by.x = "species", by.y = "ScientificName")
    star_remaining <- fsetdiff(star_wm[, .(source, stock_key, species, gsa, year)],
                               star_direct[, .(source, stock_key, species, gsa, year)])
    star_remaining <- merge(star_remaining, star_wm, by = c("source", "stock_key", "species", "gsa", "year"))
    star_genus <- merge(star_remaining[!is.na(genus)],
                        unique(fg_lookup_safe[!is.na(Genus), .(genus = Genus, FG_num, FG_name)]),
                        by = "genus", allow.cartesian = TRUE)
    star_matched <- rbindlist(list(star_direct, star_genus), use.names = TRUE, fill = TRUE)
    
    n_star_unmatched <- uniqueN(star_wm$species) - uniqueN(star_matched$species)
    message("\n[Stock-assessment biomass] ", uniqueN(star_matched$species), " of ", uniqueN(star_wm$species),
            " assessed species matched to a FG (exact name or genus fallback); ", max(n_star_unmatched, 0),
            " species could not be matched and are excluded.")
    
    ## Full-extent stock-assessment species snapshot for
    ## build_full_fg_species_catalog() - star_wm is already the whole West
    ## Med subregion (not restricted by THIS run's FILTER_AREAS), so this is
    ## every species combine_STAR_RAMlegacy.R's output contains for the
    ## region, matched (a real FG_num/FG_name) or not (NA, excluded above).
    star_unmatched_species <- setdiff(unique(star_wm$species), unique(star_matched$species))
    star_all_species <- rbindlist(list(
      unique(star_matched[, .(species, FG_num, FG_name)]),
      if (length(star_unmatched_species) > 0) {
        ## Guard against a zero-length `species` recycling a scalar NA into
        ## a single phantom NA row (a genuine data.table/data.frame gotcha)
        ## when every assessed species matched - i.e. nothing to add here.
        data.table(species = star_unmatched_species, FG_num = NA_real_, FG_name = NA_character_)
      } else {
        NULL
      }
    ), fill = TRUE)
    snapshot_full_extent_species(star_all_species, species_taxonomy, "stock_assessment", csv_out_dir,
                                 sci_name_col = "species")
    
    ## Sum assessed biomass (absolute tons) across every West Med GSA in
    ## scope - stock assessments report an absolute stock total, not a
    ## density, so summing across GSAs gives the whole-domain total
    ## directly (same principle as the fisheries-side STAR/RAM catch
    ## cross-check).
    star_biomass_by_fg <- star_matched[, .(
      star_biomass_t = sum(biomass, na.rm = TRUE),
      star_n_stocks  = uniqueN(stock_key),
      star_sources   = paste(sort(unique(source)), collapse = "+")
    ), by = .(FG_num, FG_name, Year = year)]
    
    ## Convert to the SAME t/km^2 density unit as the MEDITS/MEDIAS
    ## survey indices, using the SAME total study area already resolved
    ## for survey density (strata_area_by_area, built earlier in this
    ## script) - so a stock-assessment figure and a survey figure for
    ## the same FG/year are directly comparable and, where the priority
    ## rule below picks it, directly interchangeable in
    ## fg_index_regional_combined.
    total_area_km2_biomass <- sum(strata_area_by_area$area_km2, na.rm = TRUE)
    star_biomass_by_fg[, stock_assessment_density_t_km2 := star_biomass_t / total_area_km2_biomass]
    
    stock_assessment_fg_year <- star_biomass_by_fg[star_biomass_t > 0]
    fwrite(stock_assessment_fg_year, file.path(csv_out_dir, "stock_assessment_biomass_by_fg.csv"))
    message("[Stock-assessment biomass] stock_assessment_fg_year: ", nrow(stock_assessment_fg_year),
            " FG x Year row(s) (", uniqueN(stock_assessment_fg_year$FG_num), " FG(s)) - written to",
            " stock_assessment_biomass_by_fg.csv. Area used for the density conversion: ",
            round(total_area_km2_biomass, 1), " km^2.")
  }
  
  ## =================================================================
  ## FG biomass-source priority (rewritten 2026-09-16 -
  ## REPLACES the earlier demersal/small-pelagic-KEYWORD-guessing version,
  ## which is removed entirely). The rule, verbatim: "the
  ## priority of FG estimates MEDIAS>MEDITS, then if FG single species
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
  fg_ecology_lookup[, FG_ECOLOGY_TYPE := fifelse(
    n_species_in_fg == 1 & FG_num %in% fg_stock_assessed_nums, "single_species_assessed", "mixed"
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
  
  fg_index_avg <- fg_index_combined_raw[
    , .(avg_density = mean(mean_density, na.rm = TRUE)), by = .(Year, FG_num, FG_name)]
  fg_index_medits_only <- fg_index_combined_raw[Survey == "MEDITS", .(Year, FG_num, FG_name, medits_density = mean_density)]
  fg_index_medias_only <- fg_index_combined_raw[Survey == "MEDIAS", .(Year, FG_num, FG_name, medias_density = mean_density)]
  
  fg_index_regional_combined <- fg_index_avg
  fg_index_regional_combined <- merge(fg_index_regional_combined, fg_index_medits_only,
                                      by = c("Year", "FG_num", "FG_name"), all.x = TRUE)
  fg_index_regional_combined <- merge(fg_index_regional_combined, fg_index_medias_only,
                                      by = c("Year", "FG_num", "FG_name"), all.x = TRUE)
  fg_index_regional_combined <- merge(fg_index_regional_combined,
                                      fg_ecology_lookup[, .(FG_num, FG_ECOLOGY_TYPE)],
                                      by = "FG_num", all.x = TRUE)
  fg_index_regional_combined <- merge(fg_index_regional_combined,
                                      stock_assessment_fg_year[, .(FG_num, Year, stock_assessment_density_t_km2)],
                                      by = c("FG_num", "Year"), all.x = TRUE)
  
  ## fcase()'s own `default=` must be a single scalar value, not a
  ## row-varying vector like avg_density - passing avg_density there (as
  ## an earlier version of this did) throws "Length of 'default' must
  ## be 1" the moment this actually runs against real (>1-row) data.
  ## Built in two steps instead: fcase() first with NO default (so any
  ## row matching none of the three named conditions comes back NA),
  ## then an explicit is.na() backfill to avg_density for exactly those
  ## rows - a true row-wise fallback, which `default=` cannot express.
  fg_index_regional_combined[, `:=`(
    mean_density = fcase(
      FG_ECOLOGY_TYPE == "single_species_assessed" & !is.na(stock_assessment_density_t_km2), stock_assessment_density_t_km2,
      !is.na(medias_density), medias_density,
      !is.na(medits_density), medits_density
    ),
    biomass_source = fcase(
      FG_ECOLOGY_TYPE == "single_species_assessed" & !is.na(stock_assessment_density_t_km2), "stock assessment (STAR/RAM) - single species/stanza FG",
      !is.na(medias_density), "MEDIAS (preferred over MEDITS)",
      !is.na(medits_density), "MEDITS (MEDIAS unavailable this Year)"
    )
  )]
  fg_index_regional_combined[is.na(mean_density), mean_density := avg_density]
  fg_index_regional_combined[is.na(biomass_source), biomass_source := "MEDITS+MEDIAS average (neither MEDIAS nor MEDITS alone had a value this Year)"]
  fg_index_regional_combined <- fg_index_regional_combined[, .(Year, FG_num, FG_name, mean_density, biomass_source, FG_ECOLOGY_TYPE)]
  
  n_by_source <- fg_index_regional_combined[, .N, by = biomass_source]
  message("[Biomass source priority] fg_index_regional_combined built with the stock-assessment/MEDIAS/MEDITS priority rule - ",
          paste0(n_by_source$biomass_source, " = ", n_by_source$N, collapse = "; "), ".")
  
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
          " file lib_build_species_df_from_survey.R reads to build 03_pbqb-traits.R's species_df input.")
  
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
  ## "mixed"-ecology ones, where stock assessment is validation-only per
  ## the original 2026-09 instruction) gets its comparison too.
  ## =================================================================
  stock_assessment_biomass_crosscheck <- data.table()
  if (nrow(stock_assessment_fg_year) > 0) {
    stock_assessment_biomass_crosscheck <- merge(
      stock_assessment_fg_year,
      fg_index_regional_combined[, .(FG_num, Year, combined_density_t_km2 = mean_density, biomass_source)],
      by = c("FG_num", "Year"), all.x = TRUE
    )
    ## survey-only figure, from the raw per-survey table, ignoring
    ## whatever priority rule fed into biomass_source - this is ALWAYS
    ## "what would MEDITS/MEDIAS alone have said", even for FGs where the
    ## final combined figure used stock assessment instead.
    survey_only <- fg_index_combined_raw[, .(survey_density_t_km2 = mean(mean_density, na.rm = TRUE)), by = .(FG_num, Year)]
    stock_assessment_biomass_crosscheck <- merge(stock_assessment_biomass_crosscheck, survey_only,
                                                 by = c("FG_num", "Year"), all.x = TRUE)
    stock_assessment_biomass_crosscheck[, pct_diff_vs_survey := fifelse(
      !is.na(survey_density_t_km2) & survey_density_t_km2 > 0,
      round(100 * (stock_assessment_density_t_km2 - survey_density_t_km2) / survey_density_t_km2, 1), NA_real_
    )]  # positive = stock assessment reports MORE than the survey-only figure for this FG/year
    setorder(stock_assessment_biomass_crosscheck, FG_num, Year)
    
    fwrite(stock_assessment_biomass_crosscheck, file.path(csv_out_dir, "stock_assessment_biomass_crosscheck.csv"))
    n_big_diff <- stock_assessment_biomass_crosscheck[!is.na(pct_diff_vs_survey) & abs(pct_diff_vs_survey) > 50, .N]
    n_used_primary <- stock_assessment_biomass_crosscheck[biomass_source %like% "stock assessment", .N]
    message("[Stock-assessment biomass] stock_assessment_biomass_crosscheck: ", nrow(stock_assessment_biomass_crosscheck),
            " FG x Year row(s) (", uniqueN(stock_assessment_biomass_crosscheck$FG_num), " assessed FG(s)) - written to",
            " stock_assessment_biomass_crosscheck.csv. ", n_big_diff, " row(s) disagree with the survey-only figure by",
            " more than 50%. ", n_used_primary, " row(s) actually used stock assessment as the PRIMARY biomass source",
            " (single-species FGs, see FG_ECOLOGY_TYPE/biomass_source) - every other row is validation-only, exactly",
            " as before: fg_index_regional_combined stays MEDITS/MEDIAS-built for every mixed-ecology FG.")
    
    ## 2026-09-17 update: stock_assessment_biomass_crosscheck.csv (written
    ## just above via fwrite()) already IS this table as a CSV - no need to
    ## also add it to the workbook now that native/intermediate tables are
    ## CSV-only, so the upsert_workbook_sheets() call that used to add a
    ## StockAssessment_Biomass_CrossCheck workbook sheet has been removed.
  }
  
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
          " file lib_build_species_df_from_survey.R reads to build 03_pbqb-traits.R's species_df input.")
  
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
  
  stock_assessment_biomass_crosscheck <- data.table()
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
fwrite(fg_index, file.path(csv_out_dir, "survey_fg_annual_index.csv"))
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

## Plain CSV of each species' share of its own FG's biomass - same
## prop_sp_fg column FG_spp_Ecopath carries (and that 04_diets.R reads
## straight out of the workbook for its diet-blending weights), just
## exported on its own for review/matching purposes without needing to
## open the workbook.
fwrite(FG_spp_Ecopath[, .(FG_num, FG_name, Species, Density, prop_sp_fg)],
       file.path(csv_out_dir, "biomass_proportion_by_species_fg.csv"))
message("Saved to biomass_proportion_by_species_fg.csv (Species x FG, prop_sp_fg = that species' share of its FG's total biomass).")

## 2026-09-17 update: FG_spp_Ecopath and FG_lookup are native/intermediate
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

## 2026-09-17 update, revised: Ecopath_traits is the FG_name/species-level
## traits table "as it was saved" - i.e. this species-level
## traits_reconciled table, not the FG-level biomass-weighted rollup
## 03_pbqb-traits.R computes from it. Written directly to the workbook
## as the final Ecopath_traits sheet (still also kept as a
## traits_ewe.csv native table below, for anything reading it by that
## name). 03_pbqb-traits.R's own FG-level rollup is written as an
## audit-only CSV instead of a workbook sheet, so the two scripts don't
## both try to own the Ecopath_traits sheet name.
write_native_sheets_csv(
  sheets  = list(traits_ewe = traits_reconciled),
  out_dir = csv_out_dir
)
upsert_workbook_sheets(
  list(Ecopath_traits = traits_reconciled),
  file.path(out_dir, "ecopath_ecosim_inputs.xlsx")
)

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
  ## 2026-09-17 update: audit sheet, not a final target sheet - CSV only.
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
  ## 2026-09-17 update: reference/audit sheet, not a final target sheet - CSV only.
  write_native_sheets_csv(
    sheets  = list(FG_Density_by_Stratum = fg_density_by_stratum),
    out_dir = csv_out_dir
  )
}

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

message("\nDone. Outputs in ", out_dir, " and ", plot_dir)
message("Run finished: ", Sys.time())