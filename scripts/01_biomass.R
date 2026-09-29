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
  ## 2026-09-27: a pre-set out_dir that doesn't exist on this machine used
  ## to fail silently (dir.create(..., recursive=TRUE) further down just
  ## returns FALSE with a warning when it lacks permission to create it,
  ## e.g. someone else's home directory) or crash much later inside
  ## ggsave() with a cryptic error, instead of here where the bad path was
  ## actually accepted. run_pipeline_demo.R now checks this itself before
  ## sourcing anything, but this guard stays here too for any other driver.
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
## 2026-09-27: every script's REGULAR (non-validation) plots
## now live in their own named subfolder under out_dir/plots/ - biomass's
## own figures go here, fisheries' own go in out_dir/plots/fisheries
## (02_fisheries.R), pbqb-traits' own go in out_dir/plots/pbqb-traits
## (03_pbqb-traits.R) - so out_dir/plots/ no longer mixes figures from
## different scripts together. The shared cross-script Ecopath-input
## validation checks (PB/QB, F, P/Q ratio, diet matrix) still all land in
## one place regardless of which script produced them, but that's now
## out_dir/plots/validation (nested under plots/, see 03_pbqb-traits.R
## and 04_diets.R), not a separate top-level out_dir/validation folder.
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
plot_dir <- file.path(out_dir, "plots", "biomass")
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
## 2026-09-24, per Andrea: EcoBase (existing published Ecopath models) is
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
## fread(), not readxl::read_excel() - fg_species_file is the 2026 reviewed
## CSV (species/FG_number/FG_name/taxonomy/source/status), not the old
## FG_WMed.xlsx sheet 4 (ESPECIE/GF/FG_name). Column names differ accordingly:
## species -> ScientificName, FG_number -> FG_num, same FG_name either way.
fg_raw <- fread(fg_species_file)
dataframe2 <- unique(fg_raw[, .(ScientificName = species, FG_num = FG_number, FG_name)])

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
  ## FG_num as.numeric()'d here (2026-09-22 fix): fg_lookup_safe$FG_num can
  ## come through as integer (e.g. straight from fread()'s type-guessing
  ## on FG_WMed_2026.csv), while the no-match branch above returns
  ## NA_real_ (double). MANUAL_OVERRIDES[, cbind(resolve_override(...)),
  ## by = species_code] requires every group's result to have the SAME
  ## column type, not just the same column name - a mix of integer (this
  ## branch, when unconverted) and double (the NA branch) across
  ## different species_code groups throws "Column 1 of result for group
  ## N is type 'integer' but expecting type 'double'." Forcing double
  ## here, unconditionally, makes every group's FG_num the same type
  ## regardless of which branch fired for it.
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

## --- Seed FG rules: REMOVED ENTIRELY (2026-09-22) ------------------------
## The 4 rules that survived the previous re-audit (Holothuroidea ->
## Sea cucumbers, Bivalvia -> Bivalves, Gastropoda -> Gastropods,
## Echinoidea -> Other sea urchins) were kept on the reasoning that
## those target FGs have zero species pre-listed in FG_WMed_2026.csv,
## so the automatic taxonomy fallback has nothing to learn an exclusive
## mapping from. That's true, but it means those 4 rules were still
## inventing a Class -> FG assignment that has NO support anywhere in
## FG_WMed_2026.csv - exactly the kind of code-side manual override
## this pipeline has been moving away from all session (FG_WMed_2026.csv
## as the single source of truth for species -> FG, not a rule baked
## into the R script). Removed rather than kept "just in case": a
## species that would have been seeded this way now surfaces honestly
## as unresolved (survey_unmatched_for_manual_review.csv below), which
## is the correct signal that FG_WMed_2026.csv itself is missing an
## example species for that FG - the fix belongs in the CSV (add at
## least one real Holothuroidea/Bivalvia/Gastropoda/Echinoidea species
## under its intended FG), not in another code-side rule.
##
## SPECIES_EXCEPTIONS (Squilla mantis -> "Other commercial decapods")
## is ALSO removed, for a different but related reason: it's already
## dead code as of the current FG_WMed_2026.csv - Squilla mantis is
## listed there BY NAME under "Other commercial decapods" (FG 65), so
## it resolves via the ordinary direct scientific-name match at STEP 3
## above, before this code ever ran. It only existed to guard against
## the old "Order Stomatopoda -> Non-commercial decapods" seed rule,
## which is also gone now (removed in the earlier re-audit, since
## Non-commercial decapods already has 200 real species to learn from).
## No manual override is needed for it any more.
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
## 2026-09-24, per Andrea ("zooplankton and suprabenthos cannnot use
## biomass from survey... need to be removed and use other sources"):
## filled in with the real FG_name values confirmed against this
## pipeline's own output (stock_assessment_biomass_crosscheck.csv,
## fg_missing_ecopath_B_REVIEW.csv) - this list was a literal unfilled
## placeholder before now, so NEITHER the catchability exemption NOR
## (see FG_ECOLOGY_TYPE == "survey_exempt" below, the new, separate fix
## for the SAME root problem in the biomass-source PRIORITY rule) was
## ever actually applying to any FG. "Suprabenthos" specifically is
## Andrea's own wording, not yet confirmed against the exact FG_name
## spelling in FG_WMed_2026.csv - fix that one entry if it doesn't
## match (a name here that matches nothing in your FG table is a silent
## no-op, not an error, so a typo here won't be obvious otherwise).
EXEMPT_FG_NAMES <- c(
  "Cymodocea", "Posidonia", "Macroalgae", "Corals and gorgonians",
  "Macro zooplankton", "Meso and micro zooplankton", "Suprabenthos"
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
  
  ## Same manual-review export MEDITS gets above - apply_seed_fg_rules()
  ## itself is gone now (2026-09-22, see the "Seed FG rules: REMOVED
  ## ENTIRELY" comment above dt's own matching block): acoustic_matched
  ## keeps exactly what fallback_match_fg_by_taxonomy() resolved just
  ## above, no further code-side assignment. summarize_unresolved_species()
  ## expects a `Biomass` column (MEDITS' own convention) - acoustic_matched
  ## carries the same value under `total_biomass`, aliased here rather than
  ## renamed so nothing downstream that expects `total_biomass` breaks.
  acoustic_taxonomy_context <- attr(acoustic_matched, "still_unresolved_taxonomy")
  acoustic_matched[, Biomass := total_biomass]
  medias_still_unmatched <- summarize_unresolved_species(acoustic_matched, taxonomy = acoustic_taxonomy_context)
  acoustic_matched[, Biomass := NULL]
  medias_still_unmatched[, Survey := "MEDIAS"]
  ## 2026-09-26: consolidated with the MEDITS version of this same review
  ## file (same shape, same summarize_unresolved_species() output, just a
  ## different survey) into ONE survey_unmatched_for_manual_review.csv,
  ## distinguished by the Survey column above - reduces two near-identical
  ## review CSVs to one, nothing lost (both surveys' rows are still there).
  fwrite(medias_still_unmatched, file.path(csv_out_dir, "survey_unmatched_for_manual_review.csv"), append = TRUE)
  message("Appended MEDIAS rows to survey_unmatched_for_manual_review.csv for review (was written",
          " separately as medias_unmatched_for_manual_review.csv before 2026-09-26).")
  
  ## Same audit trail as the MEDITS side above (see its comment for the
  ## full rationale) - species assigned via the taxonomy fallback for the
  ## MEDIAS/acoustic data, split by match_type ("exclusive" vs "majority").
  acoustic_fallback_detail <- attr(acoustic_matched, "fallback_match_detail")
  if (!is.null(acoustic_fallback_detail) && nrow(acoustic_fallback_detail) > 0) {
    acoustic_fallback_detail[, Survey := "MEDIAS"]
    ## 2026-09-26: consolidated with the MEDITS version (same shape) into
    ## ONE taxonomy_fallback_matches_for_review.csv, distinguished by Survey.
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
                                         star_sources = character(0), stock_assessment_density_t_km2 = numeric(0),
                                         stock_assessment_source = character(0))  # per-row source label - "STAR/RAM" or "ICCAT", see the ICCAT block below
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
    ## 2026-09-29, per Andrea: star_sources (just above) already carries
    ## the real per-row provenance from star_ram_combined's own "source"
    ## column (e.g. which GFCM/STAR/RAM Legacy assessment) - embed it
    ## instead of a generic "stock assessment (STAR/RAM)" label with no
    ## way to trace which specific assessment it came from.
    stock_assessment_fg_year[, stock_assessment_source := paste0("stock assessment (STAR/RAM Legacy Database): ", star_sources)]
    fwrite(stock_assessment_fg_year, file.path(csv_out_dir, "stock_assessment_biomass_by_fg.csv"))
    message("[Stock-assessment biomass] stock_assessment_fg_year: ", nrow(stock_assessment_fg_year),
            " FG x Year row(s) (", uniqueN(stock_assessment_fg_year$FG_num), " FG(s)) - written to",
            " stock_assessment_biomass_by_fg.csv. Area used for the density conversion: ",
            round(total_area_km2_biomass, 1), " km^2.")
  }
  
  ## =================================================================
  ## ICCAT stock-assessment BIOMASS (SSB) - Atlantic bluefin tuna,
  ## Mediterranean swordfish, Mediterranean albacore (2026-09-23, added
  ## at the user's explicit request as the biomass-side counterpart to
  ## the ICCAT catch addition already made to 02_fisheries.R). Same 3
  ## ICCAT-managed highly-migratory stocks, same reasoning: ICCAT is the
  ## RFMO that actually assesses them directly (GFCM STAR/RAM Legacy's
  ## own bluefin/swordfish coverage, on the rare years it has any, is
  ## typically a re-publication of ICCAT's own assessment one step
  ## removed).
  ##
  ## Folded into the SAME stock_assessment_fg_year table STAR/RAM built
  ## above, rather than a parallel table with its own copy of the
  ## priority-rule logic below - ICCAT simply wins wherever it and
  ## STAR/RAM both cover a FG x Year cell.
  ##
  ## IMPORTANT (checked directly against the current FG_WMed_2026.csv,
  ## 2026-09-23): Bluefin tuna (FG 12, Thunnus thynnus) and Swordfish
  ## (FG 13, Xiphias gladius) are already their own single-species FGs,
  ## so their ICCAT biomass rows attach automatically below. Albacore
  ## (Thunnus alalunga) is NOT currently in FG_WMed_2026.csv at all - no
  ## FG represents it yet - so its ICCAT biomass will load and
  ## cross-check fine but has nowhere to attach until Thunnus alalunga
  ## is added to FG_WMed_2026.csv as its own FG (same convention as
  ## Bluefin tuna/Swordfish - see 01_biomass.R's own "Seed FG rules:
  ## REMOVED ENTIRELY" comment elsewhere in this file for why a
  ## code-side FG assignment isn't the right fix here either).
  ##
  ## Expects a small per-stock CSV at iccat_biomass_path with a year
  ## column, a biomass/SSB-in-tonnes column, and EITHER a species-code
  ## OR a species-name/stock column (several plausible header spellings
  ## accepted for each - see ICCAT_BIOMASS_COL_ALIASES). ICCAT doesn't
  ## publish SSB across every stock assessment via one uniform bulk
  ## export the way its Task I catch database works (see the ICCAT
  ## catch block in 02_fisheries.R) - this file is expected to be built
  ## by hand from the relevant stock assessment's own SSB table (one
  ## row per stock x year is all this needs). Optional - message + skip
  ## if not found, same convention as every other optional source in
  ## this pipeline.
  ##
  ## ICCAT_DIR is the same path 02_fisheries.R's own ICCAT catch block
  ## computes, redefined here the same cheap/harmless way
  ## STAR_RAMLEGACY_DIR already is in both scripts - this script is a
  ## standalone SOURCE-ABLE SCRIPT with no dependency on 02_fisheries.R
  ## having run first.
  ## =================================================================
  ICCAT_DIR <- file.path(pcloud_dir, "data/fisheries/ICCAT")
  iccat_biomass_path <- file.path(ICCAT_DIR, "iccat_ssb_biomass.csv")
  
  ICCAT_SPECIES <- data.table(
    iccat_code     = c("BFT", "SWO", "ALB"),
    ScientificName = c("Thunnus thynnus", "Xiphias gladius", "Thunnus alalunga")
  )
  ICCAT_BIOMASS_COL_ALIASES <- list(
    year         = c("Yearc", "YearC", "Year", "year"),
    species      = c("Species", "SpeciesCode", "sp_code"),
    species_name = c("SpeciesName", "SpName", "CommonName", "Stock"),
    biomass_t    = c("SSB_t", "SSB", "Biomass_t", "Biomass", "TotalBiomass_t"),
    ## 2026-09-29, per Andrea ("i would like real reference to be cited
    ## in the FG_ref"): iccat_ssb_biomass.csv already carries a real
    ## per-row Source_citation (see the GBYP/JABBA citations in the
    ## comment block above, for BFT/SWO) - previously read in, then
    ## discarded in favor of a hardcoded "ICCAT stock assessment (SSB)"
    ## label a few dozen lines below. Optional (NA if not found), same
    ## tolerant lookup as load_manual_cited_biomass_group()'s col_src.
    source       = c("Source_citation", "Source", "Citation", "Reference")
  )
  resolve_iccat_col <- function(dt_names, aliases) {
    hit <- intersect(aliases, dt_names)
    if (length(hit) == 0) NA_character_ else hit[1]
  }
  
  iccat_biomass_fg_year <- data.table(FG_num = integer(0), FG_name = character(0), Year = integer(0),
                                      iccat_biomass_t = numeric(0), iccat_sources = character(0),
                                      stock_assessment_density_t_km2 = numeric(0))
  if (!file.exists(iccat_biomass_path)) {
    message("\n[ICCAT biomass] '", iccat_biomass_path, "' not found - skipping (build a small per-stock SSB",
            " table from the relevant ICCAT stock assessment - bluefin tuna/swordfish/albacore - and place it",
            " at iccat_biomass_path if you want ICCAT's own assessed biomass available for the priority rule",
            " below; it takes priority over GFCM STAR/RAM Legacy wherever both cover the same FG x Year cell).")
  } else {
    iccat_bio_raw <- fread(iccat_biomass_path, encoding = "UTF-8")
    col_year <- resolve_iccat_col(names(iccat_bio_raw), ICCAT_BIOMASS_COL_ALIASES$year)
    col_bio  <- resolve_iccat_col(names(iccat_bio_raw), ICCAT_BIOMASS_COL_ALIASES$biomass_t)
    col_sp   <- resolve_iccat_col(names(iccat_bio_raw), ICCAT_BIOMASS_COL_ALIASES$species)
    col_spn  <- resolve_iccat_col(names(iccat_bio_raw), ICCAT_BIOMASS_COL_ALIASES$species_name)
    if (is.na(col_year) || is.na(col_bio) || (is.na(col_sp) && is.na(col_spn))) {
      stop("[ICCAT biomass] '", iccat_biomass_path, "' is missing required column(s) - found columns: ",
           paste(names(iccat_bio_raw), collapse = ", "), ". Needed: a year column (tried ",
           paste(ICCAT_BIOMASS_COL_ALIASES$year, collapse = "/"), "), a biomass column (tried ",
           paste(ICCAT_BIOMASS_COL_ALIASES$biomass_t, collapse = "/"), "), and EITHER a species-code column",
           " (tried ", paste(ICCAT_BIOMASS_COL_ALIASES$species, collapse = "/"), ") OR a species-name/stock",
           " column (tried ", paste(ICCAT_BIOMASS_COL_ALIASES$species_name, collapse = "/"), "). Rename the",
           " real column(s) to match one of these, or add the actual header spelling to",
           " ICCAT_BIOMASS_COL_ALIASES above.")
    }
    setnames(iccat_bio_raw, col_year, "Year")
    setnames(iccat_bio_raw, col_bio, "iccat_biomass_t")
    iccat_bio_raw[, `:=`(Year = as.integer(Year), iccat_biomass_t = as.numeric(iccat_biomass_t))]
    
    col_src <- resolve_iccat_col(names(iccat_bio_raw), ICCAT_BIOMASS_COL_ALIASES$source)
    if (!is.na(col_src)) {
      setnames(iccat_bio_raw, col_src, "Source_citation")
    } else {
      iccat_bio_raw[, Source_citation := NA_character_]
      message("[ICCAT biomass] '", iccat_biomass_path, "' has no Source_citation/Source/Citation/Reference",
              " column - FG_References' B_ref will fall back to the generic 'ICCAT stock assessment (SSB)'",
              " label for these FG(s) rather than the real per-stock citation. Add that column (see the",
              " GBYP aerial-survey/JABBA citations in the code comments above for what it should contain)",
              " to get the real reference through.")
    }
    
    if (!is.na(col_sp)) {
      setnames(iccat_bio_raw, col_sp, "iccat_code")
      iccat_bio_raw <- merge(iccat_bio_raw, ICCAT_SPECIES, by = "iccat_code")
    } else {
      setnames(iccat_bio_raw, col_spn, "iccat_species_name")
      iccat_bio_name_match <- data.table(
        iccat_species_name = c("Bluefin tuna", "Atlantic bluefin tuna", "BFT", "Swordfish", "SWO",
                               "Albacore", "Albacore tuna", "Albacore, N Atl.", "ALB"),
        ScientificName      = c("Thunnus thynnus", "Thunnus thynnus", "Thunnus thynnus", "Xiphias gladius", "Xiphias gladius",
                                "Thunnus alalunga", "Thunnus alalunga", "Thunnus alalunga", "Thunnus alalunga")
      )
      iccat_bio_raw <- merge(iccat_bio_raw, iccat_bio_name_match, by = "iccat_species_name")
    }
    
    iccat_bio_matched <- merge(unique(iccat_bio_raw[, .(ScientificName)]),
                               fg_lookup_safe[, .(ScientificName, FG_num, FG_name)], by = "ScientificName")
    iccat_bio_raw <- merge(iccat_bio_raw, iccat_bio_matched, by = "ScientificName")
    
    ## ---------------------------------------------------------------
    ## Spatial allocation (2026-09-24, Andrea's explicit correction to
    ## the naive version below): ICCAT does NOT assess a "Western
    ## Mediterranean" stock for any of these three species - it assesses
    ## Bluefin tuna as ONE Eastern Atlantic + Mediterranean stock, and
    ## Swordfish/Albacore as their own whole-Mediterranean stocks. The
    ## OLD code divided the biomass_t straight into
    ## sum(strata_area_by_area$area_km2) (the West Med survey-strata area
    ## ALONE) - for a stock whose real range is much bigger than the West
    ## Med, that silently inflates density by whatever factor the real
    ## range exceeds the West Med by. Fixed by dividing by the STOCK'S
    ## OWN assessed range area instead (an explicit "uniform density
    ## across the whole assessed range" assumption - itself an
    ## approximation, but a documented and bounded one, not a silent
    ## multiplier error) - wherever that assumption is defensible.
    ##
    ## Swordfish/Albacore: Mediterranean-only stock, so "uniform density
    ## across the whole Mediterranean" is the least-bad assumption
    ## available with no finer-grained spatial data in hand. Mediterranean
    ## Sea total surface area ~2,510,000 km^2 (standard oceanographic
    ## figure, e.g. Bethoux 1979-style Mediterranean physical geography
    ## references) is used as the stock's range.
    ##
    ## Swordfish (2026-09-27, filled in): ICCAT's own SS assessment
    ## reports SSB only as a B/Bmsy ratio, not a tonnage - but the 2020
    ## Mediterranean swordfish assessment's JABBA (surplus-production)
    ## model DOES give an absolute Bmsy: joint posterior median 71,319 t
    ## (67,509-73,928 t across model variants), with B2018/Bmsy = 0.72 for
    ## the terminal year -> B2018 = 0.72 x 71,319 = 51,350 t (whole
    ## Mediterranean stock, one year only - no public year-by-year SSB
    ## table exists, so this is used as the single closest-available point
    ## to the 1994-1996 baseline, same "closest available, flagged not
    ## guessed" convention as every other manual-cited source in this
    ## pipeline). See iccat_ssb_biomass.csv's SWO row for the full
    ## citation. Albacore (ALB) has no equivalent figure sourced yet - the
    ## mechanism is ready, the number isn't found.
    ##
    ## Bluefin tuna (2026-09-24, updated per Andrea: "do what you think is
    ## best - area weight or aerial"): area-ratio allocation across the
    ## whole Eastern Atlantic+Mediterranean stock range is NOT used - as
    ## explained above, the fish aren't spread evenly over that huge
    ## range, so any area ratio would be invented, not sourced. Using
    ## GBYP's own aerial-survey biomass density instead - a REAL, DIRECT,
    ## regionally-specific measurement of the Balearic Sea spawning
    ## aggregation ("A-core" survey block), not a back-calculation from
    ## the whole-stock SSB at all:
    ##   2017: 130.54 kg/km2, 2018: 217.84 kg/km2, 2019: 188.38 kg/km2,
    ##   2021: 76.27 kg/km2 (CREEM's own statistical analysis of ICCAT's
    ##   GBYP Phase 11 aerial survey, A-core area, Balearic Sea -
    ##   https://iccat.int/GBYP/DOCS/Aerial_Survey_Phase_11_CREEM_2021_Data_Analysis.pdf)
    ##   -> 4-year average 153.26 kg/km2 = 0.15326 t/km2, used below as
    ##   iccat_ssb_biomass.csv's BFT row (see that file's Source_citation).
    ## Caveats, same "closest available, flagged" spirit as every other
    ## site-specific figure in this pipeline (matches how Posidonia/
    ## gorgonian density is sourced from a single site and applied
    ## domain-wide - see claude/benthic_habitat_megafauna_biomass_sourcing_guide.md):
    ##   (a) this is a SPAWNING-SEASON snapshot (survey flown during the
    ##       June spawning aggregation), not a year-round average - likely
    ##       overstates the annual mean if the fish disperse to the
    ##       Atlantic for much of the rest of the year;
    ##   (b) it's the density WITHIN the core aggregation block itself,
    ##       applied here as the FG's domain-wide West Med average - the
    ##       same simplification already used for gorgonians/Posidonia;
    ##   (c) 2017-2021 data used as a stand-in for 1995 (no aerial survey
    ##       existed then - GBYP itself only started ~2010).
    ## Because this is already a density (not a whole-stock tonnage/area
    ## division like Swordfish/Albacore below), the mechanism is reused by
    ## setting stock_area_km2 = 1 for BFT - iccat_ssb_biomass.csv's BFT
    ## Biomass_t column is therefore expected to already BE the density in
    ## t/km2 (dividing by 1 is a no-op), not a real tonnage figure - flagged
    ## here so this isn't misread as an actual whole-stock biomass number.
    ICCAT_STOCK_AREA_KM2 <- data.table(
      iccat_code           = c("BFT", "SWO", "ALB"),
      stock_area_km2       = c(1, 2510000, 2510000),
      uniform_density_valid = c(TRUE, TRUE, TRUE),
      area_note = c(
        "GBYP aerial-survey direct density (Balearic Sea A-core spawning aggregation, 2017-2021 average) - Biomass_t IS the density already (t/km2), stock_area_km2=1 is a pass-through, not a real area",
        "Mediterranean-only stock - approximated as uniform density across the whole Mediterranean (~2,510,000 km^2) - a flagged approximation, not a real spatial distribution model",
        "Mediterranean-only stock - same uniform-density approximation as Swordfish"
      )
    )
    if ("iccat_code" %in% names(iccat_bio_raw)) iccat_bio_raw[, iccat_code := NULL]  # re-derive fresh below regardless of which branch (col_sp vs col_spn) ran above
    iccat_bio_raw <- merge(iccat_bio_raw, ICCAT_SPECIES, by = "ScientificName")
    iccat_bio_raw <- merge(iccat_bio_raw, ICCAT_STOCK_AREA_KM2, by = "iccat_code", all.x = TRUE)
    iccat_bio_raw[, row_density_t_km2 := fifelse(uniform_density_valid, iccat_biomass_t / stock_area_km2, NA_real_)]
    for (note_code in unique(iccat_bio_raw[uniform_density_valid == FALSE, iccat_code])) {
      message("[ICCAT biomass] '", note_code, "': ", ICCAT_STOCK_AREA_KM2[iccat_code == note_code, area_note],
              " - biomass recorded, density left NA (excluded from the FG priority rule until fixed).")
    }
    
    n_iccat_bio_unmatched <- uniqueN(ICCAT_SPECIES$ScientificName) - uniqueN(iccat_bio_matched$ScientificName)
    if (n_iccat_bio_unmatched > 0) {
      unmatched_sp <- setdiff(ICCAT_SPECIES$ScientificName, iccat_bio_matched$ScientificName)
      message("[ICCAT biomass] ", n_iccat_bio_unmatched, " of the ", uniqueN(ICCAT_SPECIES$ScientificName),
              " requested ICCAT species have no matching FG in the current FG_WMed_2026.csv, so their ICCAT",
              " biomass rows loaded but couldn't attach anywhere: ", paste(unmatched_sp, collapse = ", "),
              ". Add the species to FG_WMed_2026.csv as its own FG (same convention as Bluefin tuna/FG12,",
              " Swordfish/FG13) if you want it picked up here - this loader deliberately does not invent an",
              " FG for it.")
    }
    
    iccat_biomass_fg_year <- iccat_bio_raw[, .(
      iccat_biomass_t = sum(iccat_biomass_t, na.rm = TRUE),
      ## same stock -> same stock_area_km2/uniform_density_valid for every row being summed here (one
      ## species per FG), so summing row_density_t_km2 * iccat_biomass_t and dividing back out is
      ## equivalent to biomass_t / stock_area_km2 even if a stock ever had >1 row per FG x Year
      stock_assessment_density_t_km2 = fifelse(all(uniform_density_valid), sum(iccat_biomass_t, na.rm = TRUE) / stock_area_km2[1], NA_real_),
      ## Real per-stock citation where iccat_ssb_biomass.csv has one
      ## (Source_citation, resolved above), falling back to the old
      ## generic label only for a row that genuinely has none.
      iccat_sources   = {
        real_cites <- sort(unique(na.omit(Source_citation)))
        if (length(real_cites) > 0) paste0("ICCAT stock assessment (SSB): ", paste(real_cites, collapse = " | "))
        else "ICCAT stock assessment (SSB) - no Source_citation on file for this row"
      }
    ), by = .(FG_num, FG_name, Year, iccat_code)]
    iccat_biomass_fg_year <- iccat_biomass_fg_year[iccat_biomass_t > 0]
    
    ## --- temporal-baseline mismatch flag (2026-09-29, per Andrea:
    ## "for FG with biomass from present days (2010-2020) used for
    ## ecopath (1995), this should be adjusted to reflect if there was
    ## lower or higher biomass in the past using references in
    ## literature"). BFT's density comes from a 2017-2021 GBYP aerial
    ## survey (see the code comment above this ICCAT block) and SWO's
    ## from a 2018 JABBA model estimate - both real measurements, but
    ## for the WRONG year, applied here to a 1994-1996 baseline as-is.
    ##
    ## Researched this session (WebSearch/WebFetch against the actual
    ## ICCAT SCRS reports): the DIRECTION of the bias is documented for
    ## both stocks, but no source found gives a citable 1994-1996 vs.
    ## modern-year NUMERIC ratio (ICCAT's own SSB-by-year series is
    ## graphical/appendix-only in the reports checked, not a table this
    ## session could extract) - so this does NOT apply a numeric
    ## correction (that would be guessing a number, which this pipeline
    ## deliberately never does - see "the mechanism is ready, the
    ## number isn't found" convention used elsewhere, e.g. Albacore).
    ## It DOES flag the direction and likely bias prominently in
    ## stock_assessment_source/biomass_source (so it reaches B_ref) and
    ## in a dedicated review CSV, so this is visible rather than a
    ## silent temporal mismatch:
    ##   - BFT: ICCAT SCRS 2022 Eastern Atlantic & Mediterranean
    ##     bluefin tuna assessment (ASAP model, Collect. Vol. Sci. Pap.
    ##     ICCAT 79(3), SCRS/2022/013) - SSB declined sharply from the
    ##     1970s to a trough around 2007, then rose steadily 2010-2020
    ##     to the highest level since the 1960s (also stated by ICCAT
    ##     2023a via the OSPAR Atlantic bluefin tuna assessment). 1995
    ##     falls in the low/declining phase -> the 2017-2021 GBYP
    ##     density used here is very likely an OVERESTIMATE of 1995
    ##     biomass.
    ##   - SWO: ICCAT SCRS 2020 Mediterranean swordfish JABBA assessment
    ##     (Report of the 2020 ICCAT Mediterranean Swordfish Stock
    ##     Assessment Meeting) - states the stock underwent a sharp
    ##     decline from 1950-1970 to "an overfished status" ALREADY BY
    ##     THE MID-1990s, then only modest further decline through
    ##     ~2010 before an accelerating decline into 2018. 1995 biomass
    ##     was therefore likely somewhat HIGHER than the 2018 JABBA
    ##     estimate used here -> the value used here is very likely an
    ##     UNDERESTIMATE of 1995 biomass, though probably by a smaller
    ##     margin than BFT's overestimate.
    ICCAT_TEMPORAL_BASELINE_NOTE <- list(
      BFT = paste0(
        "TEMPORAL MISMATCH: this density is from a 2017-2021 GBYP aerial survey, applied as-is to the ",
        "1994-1996 baseline. ICCAT SCRS 2022 BFT-E assessment (ASAP model, Collect. Vol. Sci. Pap. ICCAT ",
        "79(3), SCRS/2022/013): SSB declined from the 1970s to a trough ~2007, then rose steadily 2010-2020 ",
        "to the highest level since the 1960s - so 1995 SSB was LOWER than 2017-2021. No citable numeric ",
        "ratio found in the SCRS report tables checked (SSB-by-year is graphical/appendix-only there) - NOT ",
        "numerically adjusted. Likely OVERESTIMATES 1995 biomass - review before trusting as a baseline value."
      ),
      SWO = paste0(
        "TEMPORAL MISMATCH: this density is from a 2018 JABBA model estimate, applied as-is to the ",
        "1994-1996 baseline. ICCAT SCRS 2020 SWO-MED assessment (JABBA Bayesian state-space model): stock ",
        "was already overfished by the mid-1990s (sharp 1950-1970 decline), with only modest further decline ",
        "1996-2010 before accelerating decline into 2018 - so 1995 biomass was likely HIGHER than the 2018 ",
        "estimate used here. No citable numeric ratio found - NOT numerically adjusted. Likely UNDERESTIMATES ",
        "1995 biomass, probably by a smaller margin than BFT's overestimate - review before trusting."
      )
    )
    for (code in names(ICCAT_TEMPORAL_BASELINE_NOTE)) {
      flagged_rows <- iccat_biomass_fg_year$iccat_code == code
      if (any(flagged_rows)) {
        iccat_biomass_fg_year[flagged_rows, iccat_sources := paste0(iccat_sources, " -- ", ICCAT_TEMPORAL_BASELINE_NOTE[[code]])]
      }
    }
    temporal_mismatch_review <- iccat_biomass_fg_year[iccat_code %in% names(ICCAT_TEMPORAL_BASELINE_NOTE),
                                                      .(FG_num, FG_name, iccat_code, Year, stock_assessment_density_t_km2,
                                                        ## `[` on a LIST returns a sub-list, which becomes a
                                                        ## list-column here (fwrite() mishandles those) -
                                                        ## unlist() to a plain character vector instead.
                                                        temporal_note = unlist(ICCAT_TEMPORAL_BASELINE_NOTE[iccat_code], use.names = FALSE))]
    if (nrow(temporal_mismatch_review) > 0) {
      fwrite(temporal_mismatch_review, file.path(csv_out_dir, "stock_assessment_temporal_mismatch_REVIEW.csv"))
      message("[ICCAT biomass] ", uniqueN(temporal_mismatch_review$FG_num), " FG(s) flagged in",
              " stock_assessment_temporal_mismatch_REVIEW.csv - their ICCAT biomass source year doesn't match",
              " the 1994-1996 Ecopath baseline; direction of likely bias is documented (see the CSV/",
              " biomass_source), but not numerically corrected (no citable ratio found this session).")
    }
    
    fwrite(iccat_biomass_fg_year, file.path(csv_out_dir, "iccat_biomass_by_fg.csv"))
    message("[ICCAT biomass] iccat_biomass_fg_year: ", nrow(iccat_biomass_fg_year), " FG x Year row(s) (",
            uniqueN(iccat_biomass_fg_year$FG_num), " FG(s)) - written to iccat_biomass_by_fg.csv.")
  }
  
  ## Merge ICCAT into stock_assessment_fg_year, ICCAT winning on overlap
  ## (an anti-join drops any STAR/RAM row for a FG x Year cell ICCAT
  ## also covers, before appending ICCAT's own rows for every cell it
  ## covers, overlapping or not).
  if (nrow(iccat_biomass_fg_year) > 0) {
    overlap_cells <- fintersect(stock_assessment_fg_year[, .(FG_num, Year)], iccat_biomass_fg_year[, .(FG_num, Year)])
    if (nrow(overlap_cells) > 0) {
      message("[Stock-assessment biomass] ICCAT overrides STAR/RAM Legacy for ", nrow(overlap_cells),
              " FG x Year cell(s) both sources cover for the same FG (ICCAT takes priority).")
      stock_assessment_fg_year <- stock_assessment_fg_year[!overlap_cells, on = c("FG_num", "Year")]
    }
    stock_assessment_fg_year <- rbindlist(list(
      stock_assessment_fg_year,
      iccat_biomass_fg_year[, .(FG_num, FG_name, Year, stock_assessment_density_t_km2,
                                star_biomass_t = iccat_biomass_t, star_n_stocks = NA_integer_,
                                star_sources = iccat_sources, stock_assessment_source = iccat_sources)]
    ), use.names = TRUE, fill = TRUE)
    message("[Stock-assessment biomass] stock_assessment_fg_year now combines STAR/RAM Legacy and ICCAT: ",
            nrow(stock_assessment_fg_year), " FG x Year row(s) total (", uniqueN(stock_assessment_fg_year$FG_num),
            " FG(s)) - ICCAT wins on any FG x Year overlap.")
  }
  
  ## =================================================================
  ## Marine megafauna BIOMASS - cetaceans, seabirds, sea turtles
  ## (2026-09-24, per Andrea's explicit request: "we need to get marine
  ## mammals, seabirds and seaturtles biomass time series"). UNLIKE
  ## bluefin tuna/swordfish above, there is NO RFMO-style bulk catch/
  ## stock-assessment database for any of these three groups - checked
  ## directly (2026-09-24):
  ##   - Cetaceans: the ACCOBAMS Survey Initiative (ASI) is the one real,
  ##     peer-reviewed, Mediterranean-wide density/abundance estimate
  ##     that exists, by species and sub-region - but it is essentially
  ##     ONE synoptic snapshot (aerial+ship surveys run in 2018, a
  ##     second ASI round has since followed) published as a PDF report
  ##     (accobams.org), not an annual bulk-downloadable time series the
  ##     way GFCM/FDI catch data is. Two data points, decades apart, is
  ##     the realistic ceiling here, not a real 1994-2024 series.
  ##   - Sea turtles: no Mediterranean-wide biomass series either, but
  ##     several individual nesting beaches (Zakynthos/Kyparissia in
  ##     Greece, Dalyan in Turkey, etc.) DO have genuine multi-decade
  ##     annual NESTING COUNT series - the closest thing to a real long
  ##     time series among these three groups, but it measures nesting
  ##     females/nests, not total population biomass, and needs a
  ##     documented nests-to-population conversion (e.g. Casale et al.)
  ##     to become a biomass figure - not attempted automatically here.
  ##   - Seabirds: no Mediterranean-wide population database found at
  ##     all - coverage is scattered, species-specific literature (e.g.
  ##     the Balearic shearwater population-trend papers), each with its
  ##     own methodology and reporting units.
  ## None of this is a "wrong URL" situation the way the earlier ICCAT
  ## biomass placeholder's problem was a real bulk download waiting to
  ## be found - there genuinely isn't a bulk source, so this stays a
  ## manual, per-record, CITED entry table, same convention as
  ## BYCATCH_RATE_MANUAL/RECREATIONAL_CATCH_MANUAL/iccat_ssb_biomass.csv
  ## above. This block is the CODE PLUMBING so that whatever real
  ## figures Andrea/Daniel pull from ACCOBAMS ASI, a nesting-count
  ## series (converted to population), or a seabird paper can be
  ## dropped straight into one small CSV and flow into the model with
  ## the priority rule below, instead of being pasted into the workbook
  ## by hand.
  ##
  ## Expected schema at MEGAFAUNA_BIOMASS_PATH - one row per group/
  ## species x year: Group (or Species/FG_name - several header
  ## spellings accepted), Year, Biomass_t (total, whole West Med study
  ## area - not a density; this script converts to density itself using
  ## the SAME Total_Area_km2 every other biomass figure here uses), and
  ## Source_citation (REQUIRED in spirit, not enforced in code - every
  ## row here is a literature/survey figure, never a measurement this
  ## pipeline made itself, so it must be traceable to where it came
  ## from). Group is matched to FG by KEYWORD against FG_name (same
  ## "closest FG" technique BELHABIB_TAXON_KEYWORDS uses in
  ## 02_fisheries.R for broad taxon-group catch) - a Group matching zero
  ## or more than one FG is written to
  ## marine_megafauna_group_to_fg_REVIEW.csv and excluded, never guessed.
  ## =================================================================
  ## 2026-09-24, per Andrea: these groups don't need a real time series -
  ## the minimum is a single baseline biomass figure for the Ecopath base
  ## year (1994-1996). load_manual_cited_biomass_group() below accepts
  ## that as-is: a CSV with just one Year value (or one row per year of
  ## 1994/1995/1996) works fine, since everything downstream already
  ## groups by FG x Year - it never assumed a full annual series.
  ##
  ## Generic loader, used twice below (once for marine megafauna, once
  ## for the lower-trophic groups that MEDITS/MEDIAS also can't sample -
  ## see EXEMPT_FG_NAMES/catchability-correction comment near the top of
  ## this script: a bottom-trawl survey isn't designed to represent
  ## phytoplankton/zooplankton/macroalgae/seagrass at all). Both are the
  ## SAME situation - no bulk API, no survey coverage, manual cited entry
  ## is the only honest option - so one function, two csv files, two
  ## keyword sets.
  ## =================================================================
  ## 2026-09-26, per Andrea: two extensions to the manual-cited-CSV
  ## mechanism above, both driven by the SAME underlying idea - a
  ## literature-sourced FG total should be built from its real
  ## taxonomic composition, not one lumped guess:
  ##
  ##  (a) SUM OF SUBGROUPS - already free. load_manual_cited_biomass_
  ##      group() below sums group_biomass_t across every CSV row that
  ##      keyword-matches the same FG_num x Year (see the `out <-
  ##      matched[, .(group_biomass_t = sum(...))]` aggregation). So
  ##      "Benthic mollusc" = Bivalvia + Gastropoda + Scaphopoda +
  ##      Polyplacophora, or "Other macro-benthos" = sum of its 18
  ##      classes, works TODAY as soon as each subgroup gets its own
  ##      keyword category (pointing at the same FG) and its own CSV
  ##      row/citation - no code change needed for this half.
  ##
  ##  (b) HABITAT-AREA EXTRAPOLATION - genuinely new. Previously every
  ##      manual-cited row had to already BE a whole-study-area total
  ##      (Biomass_t), converted to density by dividing by the FULL
  ##      strata_area_by_area sum - i.e. a literature density figure
  ##      for, say, bivalves would get smeared uniformly across the
  ##      ENTIRE West Med domain, including depth ranges bivalves don't
  ##      even occupy. The three functions below let a CSV row instead
  ##      supply a DENSITY (Density_value + Density_unit, from the
  ##      literature site) plus that taxon's real habitat depth range
  ##      (Depth_min_m/Depth_max_m if the paper states it directly, or
  ##      Habitat_species - a ";"-separated species list resolved via
  ##      AquaMaps' preferred-depth envelope) - and load_manual_cited_
  ##      biomass_group() converts density x REAL habitat area (not the
  ##      full domain) -> Biomass_t itself, with the full calculation
  ##      audit-trailed into Source_citation so nothing is a silent
  ##      guess. A row that already supplies Biomass_t directly (the
  ##      original schema) is untouched - this is purely additive.
  ## =================================================================
  
  ## approximate multiplier to convert a literature density figure to
  ## g wet-weight / m2 - which is numerically identical to t/km2 (1
  ## t/km2 = 1e6 g / 1e6 m2 = 1 g/m2), so once a density is in this
  ## unit it can multiply directly against a km2 habitat area to get
  ## tonnes. The "_dw" (dry-weight) entries carry a generic wet:dry
  ## ratio of 4.5 (a commonly-cited macrobenthos DW->WW default, e.g.
  ## Ricciardi & Bourget 1998-style conversions) - ONLY used when a
  ## row's Density_unit is explicitly a dry-weight unit, and always
  ## flagged as an assumption in that row's audit trail (see
  ## extrapolate_density_row() below), never silently applied.
  DENSITY_UNIT_TO_WET_G_M2 <- c(
    "g_m2" = 1, "g_per_m2" = 1,
    "kg_m2" = 1000,
    "mg_m2" = 0.001,
    "t_km2" = 1,
    "g_m2_dw" = 4.5, "g_m2_dry" = 4.5, "gdw_m2" = 4.5
  )
  
  ## Pro-rate each MEDITS stratum's REAL bathymetry-derived area
  ## (strata_area_by_area, computed once above from actual seafloor
  ## depth, not a flat guess) by how much of that stratum's depth band
  ## overlaps the target group's [depth_min_m, depth_max_m] range, then
  ## sums across strata (and across every AreaID already in scope,
  ## since strata_area_by_area is built only for FILTER_AREAS). This is
  ## the "study inhabitat by group" piece of the request - a taxon
  ## confined to 10-100m gets only the 10-100m slice of the domain, not
  ## the whole 10-800m study area.
  estimate_habitat_area_km2 <- function(depth_min_m, depth_max_m, strata_area_by_area, strata_def = MEDITS_STRATA) {
    if (is.na(depth_min_m) || is.na(depth_max_m) || depth_max_m <= depth_min_m) return(NA_real_)
    overlap_by_stratum <- rbindlist(lapply(seq_len(nrow(strata_def)), function(i) {
      s_min <- strata_def$depth_min[i]; s_max <- strata_def$depth_max[i]
      overlap <- max(0, min(depth_max_m, s_max) - max(depth_min_m, s_min))
      frac <- if ((s_max - s_min) > 0) overlap / (s_max - s_min) else 0
      data.table(Stratum = strata_def$stratum_num[i], frac = frac)
    }))
    merged <- merge(strata_area_by_area, overlap_by_stratum, by = "Stratum", all.x = TRUE)
    merged[is.na(frac), frac := 0]
    sum(merged$area_km2 * merged$frac, na.rm = TRUE)
  }
  
  ## Resolves a taxonomic subgroup's own depth range from AquaMaps
  ## (lib_aquamaps_depth_extension.R's resolve_aquamaps_species_ids() +
  ## fetch_aquamaps_depth_envelope()), independent of whether the
  ## APPLY_AQUAMAPS_DEPTH_ADJUSTMENT opt-in flag is TRUE this run - that
  ## flag only gates a DIFFERENT, unrelated use of AquaMaps (per-species
  ## shallow/deep density adjustment earlier in this script); this
  ## manual-CSV path needs the same library defensively source()'d on
  ## its own. Uses the 10th/90th percentile of matched species'
  ## PREFERRED depth envelope (DepthPrefMin/DepthPrefMax), not the bare
  ## min/max, so one outlier deep- or shallow-water species in the list
  ## doesn't blow the habitat range out past where the bulk of the
  ## group actually lives.
  resolve_group_depth_range_aquamaps <- function(species_names, label = "") {
    species_names <- unique(species_names[!is.na(species_names) & species_names != ""])
    out <- list(depth_min_m = NA_real_, depth_max_m = NA_real_, n_matched = 0L, n_total = length(species_names))
    if (length(species_names) == 0) return(out)
    if (!exists("resolve_aquamaps_species_ids", mode = "function")) {
      aquamaps_lib_path <- file.path(git_dir, "scripts/lib_aquamaps_depth_extension.R")
      if (file.exists(aquamaps_lib_path)) {
        source(aquamaps_lib_path)
      } else {
        message("[", label, "] AquaMaps depth-extension library not found at ", aquamaps_lib_path,
                " - cannot resolve a habitat depth range for ", paste(species_names, collapse = ", "),
                "; falling back to the full study-domain depth range for this row.")
        return(out)
      }
    }
    id_lookup <- tryCatch(resolve_aquamaps_species_ids(species_names),
                          error = function(e) { message("[", label, "] AquaMaps species lookup failed: ", conditionMessage(e)); NULL })
    if (is.null(id_lookup) || nrow(id_lookup) == 0) return(out)
    envelope <- tryCatch(fetch_aquamaps_depth_envelope(id_lookup),
                         error = function(e) { message("[", label, "] AquaMaps depth-envelope fetch failed: ", conditionMessage(e)); NULL })
    if (is.null(envelope)) return(out)
    ok <- envelope[!is.na(DepthPrefMin) & !is.na(DepthPrefMax)]
    out$n_matched <- if (nrow(ok) > 0) uniqueN(ok$ScientificName) else 0L
    if (nrow(ok) == 0) return(out)
    out$depth_min_m <- as.numeric(quantile(ok$DepthPrefMin, 0.10, na.rm = TRUE))
    out$depth_max_m <- as.numeric(quantile(ok$DepthPrefMax, 0.90, na.rm = TRUE))
    out
  }
  
  ## One row's density -> Biomass_t conversion, with a full audit trail
  ## string returned alongside so the caller can append it to
  ## Source_citation - never a silent number. Depth-range priority:
  ## CSV Depth_min_m/Depth_max_m (the literature site's own stated
  ## range) > AquaMaps via Habitat_species > full study-domain range
  ## (the old uniform-smear behavior, kept as a last-resort fallback,
  ## always flagged as such).
  extrapolate_density_row <- function(density_value, density_unit, depth_min_m, depth_max_m, habitat_species,
                                      strata_area_by_area, strata_def = MEDITS_STRATA, label = "",
                                      habitat_area_km2_override = NA_real_) {
    ## 2026-09-28: a depth-band area is a poor stand-in for a genuinely
    ## PATCHY habitat - Posidonia/macroalgae/coralligenous fauna don't
    ## carpet their entire depth range, they occupy a much smaller real
    ## footprint within it (e.g. Posidonia's real West Med meadow extent
    ## is ~10,511 km^2, per a dedicated EUSeaMap habitat shapefile -
    ## nowhere near the full area of the 0-40m band summed across GSA
    ## 1-11). Multiplying an in-habitat density measurement by the whole
    ## depth-band area silently assumes the habitat is contiguous and
    ## complete across that band, overstating total biomass by
    ## potentially an order of magnitude for anything patchy. When the
    ## CSV row supplies a real, independently-measured Habitat_area_km2
    ## (e.g. from a habitat-extent shapefile clipped to the study
    ## domain), that number is used directly instead of the depth-band
    ## estimate - depth range/Habitat_species are then not needed for
    ## this row at all.
    if (!is.na(habitat_area_km2_override)) {
      habitat_area_km2 <- habitat_area_km2_override
      depth_source <- paste0("REAL HABITAT-EXTENT OVERRIDE (Habitat_area_km2 = ", round(habitat_area_km2, 2),
                             " km2, supplied directly in the CSV row - not derived from a depth band; this is",
                             " the correct method for a patchy habitat like seagrass/macroalgae/coralligenous,",
                             " where the depth band it occupies is far larger than its actual footprint)")
      if (is.na(density_value)) {
        return(list(biomass_t = NA_real_,
                    audit_note = "EXTRAPOLATION FAILED (missing density value) - left as NA, not guessed."))
      }
    } else {
      depth_source <- "CSV Depth_min_m/Depth_max_m (literature site's own stated depth range)"
      if (is.na(depth_min_m) || is.na(depth_max_m)) {
        if (!is.na(habitat_species) && nzchar(habitat_species)) {
          sp_list_hab <- trimws(strsplit(habitat_species, ";")[[1]])
          am <- resolve_group_depth_range_aquamaps(sp_list_hab, label = label)
          depth_min_m <- am$depth_min_m; depth_max_m <- am$depth_max_m
          depth_source <- paste0("AquaMaps preferred-depth envelope (10th/90th pct across ", am$n_matched, "/",
                                 am$n_total, " matched species: ", habitat_species, ")")
        }
      }
      if (is.na(depth_min_m) || is.na(depth_max_m)) {
        depth_min_m <- min(strata_def$depth_min); depth_max_m <- max(strata_def$depth_max)
        depth_source <- "FULL STUDY-DOMAIN DEPTH RANGE (no Depth_min_m/Depth_max_m or resolvable Habitat_species given - uniform extrapolation across the whole domain, same as the old whole-area approach; add a depth range or Habitat_species to narrow this)"
      }
      habitat_area_km2 <- estimate_habitat_area_km2(depth_min_m, depth_max_m, strata_area_by_area, strata_def)
      if (is.na(habitat_area_km2) || is.na(density_value)) {
        return(list(biomass_t = NA_real_,
                    audit_note = "EXTRAPOLATION FAILED (missing habitat area or density value) - left as NA, not guessed."))
      }
    }
    unit_key <- gsub("/", "_", tolower(gsub("[[:space:]]+", "_", trimws(as.character(density_unit)))))
    mult <- DENSITY_UNIT_TO_WET_G_M2[unit_key]
    if (is.na(mult) || length(mult) == 0) {
      message("[", label, "] Density_unit '", density_unit, "' not recognized (known: ",
              paste(names(DENSITY_UNIT_TO_WET_G_M2), collapse = ", "),
              ") - assuming it's already g/m2 wet-weight (== t/km2). Add it to DENSITY_UNIT_TO_WET_G_M2 if that's wrong.")
      mult <- 1
    }
    density_t_km2 <- density_value * mult
    biomass_t <- density_t_km2 * habitat_area_km2
    depth_note <- if (is.na(depth_min_m) || is.na(depth_max_m)) "" else paste0(" (depth ", round(depth_min_m), "-", round(depth_max_m), " m)")
    audit_note <- paste0("EXTRAPOLATED from density ", density_value, " ", density_unit, " (x", mult, " -> ",
                         round(density_t_km2, 4), " t/km2) over habitat area ", round(habitat_area_km2, 1),
                         " km2", depth_note, "; ", depth_source,
                         " => ", round(biomass_t, 3), " t")
    list(biomass_t = biomass_t, audit_note = audit_note)
  }
  
  load_manual_cited_biomass_group <- function(csv_path, taxon_keywords, label, review_csv_name, output_csv_name,
                                              fallback_message) {
    col_aliases <- list(
      year      = c("Year", "year", "Yearc"),
      group     = c("Group", "Species", "FG_name", "Taxon", "CommonName"),
      biomass_t = c("Biomass_t", "Biomass", "Population_t", "Total_t"),
      source    = c("Source_citation", "Source", "Citation", "Reference"),
      ## 2026-09-29, per Andrea: "for FG with biomass from present days
      ## (2010-2020) used for ecopath (1995), this should be adjusted
      ## to reflect if there was lower or higher biomass in the past
      ## using references in literature". `Year` above is the TARGET
      ## baseline year this row is being applied to (usually forced to
      ## 1994-1996 so it actually lands in the Ecopath baseline window
      ## via the FG_num/Year merge below) - it is NOT necessarily when
      ## the underlying measurement/survey was actually taken. This
      ## OPTIONAL column lets a source CSV say when the real
      ## measurement is from, distinct from the target Year, so a
      ## genuine temporal mismatch (a 2018 ACCOBAMS survey applied to
      ## 1995, say) can be flagged automatically instead of only being
      ## caught by hand (as done for the ICCAT BFT/SWO case above).
      ## Optional - every row without it is treated as already
      ## contemporaneous with Year, same as before this fix.
      collection_year = c("Collection_year", "Survey_year", "Data_year", "Observed_year", "Measurement_year")
    )
    out <- data.table(FG_num = integer(0), FG_name = character(0), Year = integer(0),
                      group_biomass_t = numeric(0), group_sources = character(0),
                      stock_assessment_density_t_km2 = numeric(0))
    if (!file.exists(csv_path)) {
      message("\n[", label, "] '", csv_path, "' not found - skipping. ", fallback_message)
      return(out)
    }
    raw <- fread(csv_path, encoding = "UTF-8")
    col_year <- resolve_iccat_col(names(raw), col_aliases$year)
    col_bio  <- resolve_iccat_col(names(raw), col_aliases$biomass_t)
    col_grp  <- resolve_iccat_col(names(raw), col_aliases$group)
    col_src  <- resolve_iccat_col(names(raw), col_aliases$source)
    if (any(is.na(c(col_year, col_bio, col_grp)))) {
      message("\n[", label, "] '", csv_path, "' is missing a required column - found: ",
              paste(names(raw), collapse = ", "), ". Needed a year column (tried ",
              paste(col_aliases$year, collapse = "/"), "), a biomass column (tried ",
              paste(col_aliases$biomass_t, collapse = "/"), "), and a group/species column (tried ",
              paste(col_aliases$group, collapse = "/"), ") - skipping rather than guessing.")
      return(out)
    }
    setnames(raw, col_year, "Year")
    setnames(raw, col_bio, "group_biomass_t")
    setnames(raw, col_grp, "Group")
    if (!is.na(col_src)) setnames(raw, col_src, "Source_citation") else raw[, Source_citation := NA_character_]
    raw[, `:=`(Year = as.integer(Year), group_biomass_t = as.numeric(group_biomass_t))]
    
    ## 2026-09-29, per Andrea ("the proportion of species within FG should
    ## include all values inputted in B Ecopath, including the literature
    ## ones... it will be used for the pbqb"): an OPTIONAL Species column,
    ## distinct from Group (Group can still be an FG-level keyword like
    ## "OtherDolphins" or "PelagicSeabirds" - Species, when present, names
    ## the actual species that row's Biomass_t is FOR). Species-level rows
    ## already exist in the megafauna CSV (Stenella vs. Delphinus within
    ## "OtherDolphins"; Calonectris/Puffinus yelkouan/Hydrobates within
    ## "PelagicSeabirds") but were previously only ever summed to one FG
    ## total here, discarding exactly the species-level detail
    ## FG_spp_Ecopath's prop_sp_fg needs. Capturing it here (rather than
    ## needing a SEPARATE species-weight CSV, which risks drifting out of
    ## sync with these numbers) means the same literature figure that
    ## builds the FG's Ecopath_B total is, by construction, also what
    ## splits it across species - one source of truth, not two. See the
    ## species_lit_biomass merge in the FG_spp_Ecopath build below for
    ## where this actually gets applied to prop_sp_fg.
    col_sp <- resolve_iccat_col(names(raw), c("Species", "Species_name", "ScientificName", "Scientific_name"))
    if (!is.na(col_sp)) setnames(raw, col_sp, "Species") else raw[, Species := NA_character_]
    
    ## --- temporal-baseline mismatch flag (optional Collection_year
    ## column - see col_aliases$collection_year comment above). Flags,
    ## never numerically corrects - this pipeline doesn't guess a
    ## correction factor without a literature-sourced ratio (same
    ## "closest available, flagged not guessed" rule as everywhere
    ## else here). A row with no Collection_year column, or one equal
    ## to Year, is left completely untouched.
    col_coll_year <- resolve_iccat_col(names(raw), col_aliases$collection_year)
    if (!is.na(col_coll_year)) {
      setnames(raw, col_coll_year, "Collection_year")
      raw[, Collection_year := as.integer(Collection_year)]
      year_ecopath_mid <- if (exists("YEAR_ECOPATH", envir = .GlobalEnv, inherits = FALSE)) round(mean(YEAR_ECOPATH)) else NA_integer_
      mismatched <- !is.na(raw$Collection_year) & raw$Collection_year != raw$Year
      if (any(mismatched) && !is.na(year_ecopath_mid)) {
        gap <- raw$Collection_year[mismatched] - year_ecopath_mid
        temporal_note <- paste0(
          "TEMPORAL MISMATCH: measured in ", raw$Collection_year[mismatched], ", applied here to the ",
          year_ecopath_mid, " Ecopath baseline (gap ~", gap, " year(s)) - no literature-sourced numeric",
          " trend correction applied (direction/magnitude not verified this run); review whether this",
          " FG's biomass is known to have been higher or lower ~", gap, " year(s) earlier."
        )
        raw[mismatched, Source_citation := fifelse(is.na(Source_citation), temporal_note,
                                                   paste0(Source_citation, " -- ", temporal_note))]
        review_rows <- unique(raw[mismatched, .(Group, Year, Collection_year, gap_years = Collection_year - year_ecopath_mid)])
        fwrite(review_rows, file.path(csv_out_dir, paste0("temporal_mismatch_REVIEW_", label, ".csv")))
        message("[", label, "] ", nrow(review_rows), " Group(s) flagged for a temporal mismatch (Collection_year",
                " != the ", year_ecopath_mid, " Ecopath baseline) - see temporal_mismatch_REVIEW_", label,
                ".csv and Source_citation. NOT numerically corrected - only flagged.")
      }
    }
    
    ## --- optional density -> habitat-area extrapolation (2026-09-26) -------
    ## A row can supply Density_value/Density_unit instead of a direct
    ## Biomass_t, plus either Depth_min_m/Depth_max_m or Habitat_species
    ## (";"-separated scientific names resolved via AquaMaps) to size the
    ## REAL habitat area that density applies over, rather than the whole
    ## study domain. Purely additive - a row with Biomass_t already filled
    ## in is left completely alone.
    col_dens_val  <- resolve_iccat_col(names(raw), c("Density_value", "Density", "Density_t_km2"))
    col_dens_unit <- resolve_iccat_col(names(raw), c("Density_unit", "DensityUnit", "Unit"))
    col_depth_min <- resolve_iccat_col(names(raw), c("Depth_min_m", "DepthMin_m", "Depth_min"))
    col_depth_max <- resolve_iccat_col(names(raw), c("Depth_max_m", "DepthMax_m", "Depth_max"))
    col_hab_sp    <- resolve_iccat_col(names(raw), c("Habitat_species", "HabitatSpecies", "Taxa_list"))
    ## 2026-09-28: optional REAL habitat-extent override (e.g. a habitat
    ## shapefile clipped to the study domain and summed) - see
    ## extrapolate_density_row()'s own header comment for why this beats
    ## the depth-band estimate for a patchy habitat (seagrass, macroalgae,
    ## coralligenous fauna). When given, Depth_min_m/Depth_max_m/
    ## Habitat_species are ignored for that row - this area is used directly.
    col_hab_area  <- resolve_iccat_col(names(raw), c("Habitat_area_km2", "HabitatArea_km2", "Habitat_area", "Real_habitat_area_km2"))
    if (!is.na(col_dens_val)) {
      setnames(raw, col_dens_val, "Density_value")
      if (!is.na(col_dens_unit)) setnames(raw, col_dens_unit, "Density_unit") else raw[, Density_unit := NA_character_]
      if (!is.na(col_depth_min)) setnames(raw, col_depth_min, "Depth_min_m") else raw[, Depth_min_m := NA_real_]
      if (!is.na(col_depth_max)) setnames(raw, col_depth_max, "Depth_max_m") else raw[, Depth_max_m := NA_real_]
      if (!is.na(col_hab_sp)) setnames(raw, col_hab_sp, "Habitat_species") else raw[, Habitat_species := NA_character_]
      if (!is.na(col_hab_area)) setnames(raw, col_hab_area, "Habitat_area_km2") else raw[, Habitat_area_km2 := NA_real_]
      raw[, `:=`(Density_value = as.numeric(Density_value), Depth_min_m = as.numeric(Depth_min_m),
                 Depth_max_m = as.numeric(Depth_max_m), Habitat_area_km2 = as.numeric(Habitat_area_km2))]
      needs_row <- which(!is.na(raw$Density_value) & (is.na(raw$group_biomass_t) | raw$group_biomass_t == 0))
      if (length(needs_row) > 0) {
        n_with_override <- sum(!is.na(raw$Habitat_area_km2[needs_row]))
        message("[", label, "] ", length(needs_row), " row(s) supply Density_value instead of Biomass_t - ",
                "extrapolating via real habitat area (", n_with_override, " using an explicit Habitat_area_km2",
                " override, ", length(needs_row) - n_with_override, " estimated from depth range instead;",
                " see Source_citation for the per-row audit trail).")
        for (i in needs_row) {
          row_result <- extrapolate_density_row(
            density_value = raw$Density_value[i], density_unit = raw$Density_unit[i],
            depth_min_m = raw$Depth_min_m[i], depth_max_m = raw$Depth_max_m[i],
            habitat_species = raw$Habitat_species[i], strata_area_by_area = strata_area_by_area,
            strata_def = MEDITS_STRATA, label = label,
            habitat_area_km2_override = raw$Habitat_area_km2[i])
          set(raw, i, "group_biomass_t", row_result$biomass_t)
          set(raw, i, "Source_citation",
              paste0(if (is.na(raw$Source_citation[i])) "" else paste0(raw$Source_citation[i], " | "),
                     row_result$audit_note))
        }
      }
    }
    
    ## Full FG catalog isn't built until later in this script (full_fg_list,
    ## from dataframe2) - fg_lookup_safe is already in scope this far up
    ## (built at line ~637) and carries the same FG_num/FG_name universe.
    full_fg_catalog <- unique(fg_lookup_safe[, .(FG_num, FG_name)])
    group_word_sets <- lapply(taxon_keywords, tolower)
    fg_word_hits <- rbindlist(lapply(names(group_word_sets), function(g) {
      hits <- full_fg_catalog[sapply(tolower(FG_name), function(nm) any(sapply(group_word_sets[[g]], function(kw) grepl(kw, nm, fixed = TRUE))))]
      if (nrow(hits) == 0) return(NULL)
      data.table(Group = g, FG_num = hits$FG_num, FG_name = hits$FG_name)
    }))
    if (is.null(fg_word_hits) || nrow(fg_word_hits) == 0) fg_word_hits <- data.table(Group = character(), FG_num = integer(), FG_name = character())
    
    unmatched_groups_dt <- unique(raw[!Group %in% unique(fg_word_hits$Group), .(Group)])
    if (nrow(unmatched_groups_dt) > 0) {
      fwrite(unmatched_groups_dt, file.path(csv_out_dir, review_csv_name))
      message("[", label, "] ", nrow(unmatched_groups_dt), " Group value(s) in ", csv_path,
              " matched ZERO FG by keyword (checked against FG_WMed_2026.csv's real FG_name text - add a",
              " keyword to the taxon-keyword list above if the FG really exists under a different name): ",
              paste(unmatched_groups_dt$Group, collapse = ", "), " - written to ", review_csv_name, ", excluded below.")
    }
    ambiguous_groups <- fg_word_hits[, .N, by = Group][N > 1, Group]
    if (length(ambiguous_groups) > 0) {
      message("[", label, "] ", length(ambiguous_groups), " Group value(s) matched MORE than one",
              " FG by keyword - kept (biomass is apportioned across all matching FGs, same 'split it' rule the",
              " Belhabib taxon-keyword match in 02_fisheries.R uses): ", paste(ambiguous_groups, collapse = ", "))
    }
    matched <- merge(raw, fg_word_hits, by = "Group", allow.cartesian = TRUE)
    species_detail <- data.table(FG_num = integer(0), FG_name = character(0), Year = integer(0),
                                 Species = character(0), species_biomass_t = numeric(0),
                                 species_source = character(0))
    if (nrow(matched) > 0) {
      matched[, n_fg_for_group := uniqueN(FG_num), by = .(Group, Year)]
      matched[, group_biomass_t := group_biomass_t / n_fg_for_group]  # split evenly across ambiguous FGs - never guessed at full weight onto each
      ## Capture the per-species detail BEFORE it's summed away into one
      ## FG total below - this is what lets prop_sp_fg reuse the exact
      ## same literature figures that built Ecopath_B, instead of a
      ## second, separately-maintained relative-weight source.
      if (any(!is.na(matched$Species))) {
        species_detail <- matched[!is.na(Species), .(
          FG_num, FG_name, Year, Species, species_biomass_t = group_biomass_t,
          species_source = fifelse(is.na(Source_citation), paste0(label, " (source_citation not filled in)"), Source_citation)
        )]
        message("[", label, "] ", uniqueN(species_detail$Species), " row(s) also carry a Species-level",
                " biomass figure across ", uniqueN(species_detail$FG_num), " FG(s) - these will directly drive",
                " prop_sp_fg for those species in FG_spp_Ecopath (see species_lit_biomass below), taking",
                " priority over both raw survey density and any separate relative-weighting CSV.")
      }
      out <- matched[, .(
        group_biomass_t = sum(group_biomass_t, na.rm = TRUE),
        group_sources = paste(unique(na.omit(Source_citation)), collapse = "; ")
      ), by = .(FG_num, FG_name, Year)]
      out[group_sources == "", group_sources := paste0(label, " (source_citation not filled in)")]
      out[, stock_assessment_density_t_km2 := group_biomass_t / sum(strata_area_by_area$area_km2, na.rm = TRUE)]
      out <- out[group_biomass_t > 0]
      fwrite(out, file.path(csv_out_dir, output_csv_name))
      message("[", label, "] ", nrow(out), " FG x Year row(s) (", uniqueN(out$FG_num), " FG(s)) - written to ", output_csv_name, ".")
    }
    attr(out, "species_detail") <- species_detail
    out
  }
  
  ## Merge one manual-cited group's output into stock_assessment_fg_year -
  ## it wins on any FG x Year overlap (it's always the single most direct
  ## source available for these FGs; no MEDITS/MEDIAS trawl survey samples
  ## megafauna or plankton/primary-producer groups at all).
  merge_manual_cited_biomass <- function(stock_assessment_fg_year, group_fg_year, source_label) {
    if (nrow(group_fg_year) == 0) return(stock_assessment_fg_year)
    overlap_cells <- fintersect(stock_assessment_fg_year[, .(FG_num, Year)], group_fg_year[, .(FG_num, Year)])
    if (nrow(overlap_cells) > 0) {
      message("[Stock-assessment biomass] ", source_label, " overrides an existing source for ",
              nrow(overlap_cells), " FG x Year cell(s).")
      stock_assessment_fg_year <- stock_assessment_fg_year[!overlap_cells, on = c("FG_num", "Year")]
    }
    ## stock_assessment_source now carries the REAL literature citation
    ## (group_sources - read straight from the input CSV's own
    ## Source_citation column, see load_manual_cited_biomass_group()
    ## above), prefixed with source_label, instead of the generic
    ## "literature/survey estimate (... - manual, cited entry)"
    ## placeholder phrase this used to write no matter what the row's
    ## actual citation was. The source_label prefix ("marine megafauna",
    ## "primary producer/plankton") is kept so the Tier classification in
    ## the biomass-by-source QA plot below (which matches on those exact
    ## words) still recognizes these rows - only the "manual, cited
    ## entry" filler is replaced with content. group_sources already
    ## reads "<label> (source_citation not filled in)" for a row whose
    ## input CSV genuinely left Source_citation blank, so that case still
    ## surfaces as a visible gap rather than a silent fake citation.
    stock_assessment_fg_year <- rbindlist(list(
      stock_assessment_fg_year,
      group_fg_year[, .(FG_num, FG_name, Year, stock_assessment_density_t_km2,
                        star_biomass_t = group_biomass_t, star_n_stocks = NA_integer_,
                        star_sources = group_sources,
                        stock_assessment_source = paste0(source_label, ": ", group_sources))]
    ), use.names = TRUE, fill = TRUE)
    message("[Stock-assessment biomass] stock_assessment_fg_year now also includes ", source_label, ": ",
            nrow(stock_assessment_fg_year), " FG x Year row(s) total.")
    stock_assessment_fg_year
  }
  
  ## --- Marine megafauna (cetaceans, seabirds, sea turtles) ---------------
  ## Real sources for a 1994-1996 baseline figure: ACCOBAMS Survey
  ## Initiative (accobams.org - cetacean density/abundance by species and
  ## sub-region; note its own survey rounds are ~2018+, so treat it as the
  ## closest available proxy, not a real 1994-1996 measurement, unless a
  ## published back-cast exists); a long-running nesting-beach count series
  ## (e.g. Zakynthos/Kyparissia, Greece; Dalyan, Turkey) converted to a
  ## population estimate via a published nests-to-population factor (e.g.
  ## Casale et al.) for sea turtles - these series DO reach back to the
  ## 1990s, so this is the one megafauna group with a real shot at an
  ## actual base-year figure; species-specific published population papers
  ## for seabirds (no single Mediterranean-wide source found). Whole-
  ## Mediterranean EwE models covering this exact period also exist and
  ## are worth pulling a cited baseline from directly - see
  ## Piroddi et al. 2015 (Mar Ecol Prog Ser 533:47-65, "Modelling the
  ## Mediterranean marine ecosystem as a whole" - two baseline periods,
  ## "1950s" and "2000s", 4 Mediterranean sub-regions including a Western
  ## Mediterranean one, tables S2/S3 in the supplementary material carry
  ## the actual B t/km2 by functional group INCLUDING cetaceans, seabirds,
  ## sea turtles) and, as a smaller-scale cross-check, the GOLEM Gulf of
  ## Lion Ecopath model (Ecosyst. modelling in the NW Med Sea, 2010-2014
  ## baseline - reports dolphins+seabirds combined at <0.01% of total
  ## system biomass, a useful order-of-magnitude sanity check even though
  ## its reference period is 20 years later than ours).
  ## 2026-09-25, per Andrea: moved from data/ to data/Complementary data/,
  ## alongside the other manual/cited reference files that live there
  ## (matching where westmed_posidonia_coralligenous.shp and similar
  ## hand-curated inputs already live).
  MEGAFAUNA_BIOMASS_PATH <- file.path(pcloud_dir, "data/Complementary data/marine_megafauna_biomass.csv")
  ## 2026-09-24, per Andrea (Ecopath_B still showing no cetaceans/
  ## seabirds/sea turtles even after the fg_year_full fix): "Pinnipeds"
  ## added as its OWN keyword group - Monk seals (FG6) matched NEITHER
  ## Cetaceans/Seabirds/SeaTurtles before this (no "seal" keyword
  ## anywhere), so a manual CSV would have loaded fine but Monk seals
  ## specifically could never have received a value regardless of what
  ## the CSV said - a real gap, not just a missing CSV.
  ## 2026-09-25, per Andrea: real, species-specific ACCOBAMS Survey
  ## Initiative density figures are now available for bottlenose dolphins,
  ## "other dolphins" (striped dolphin proxy), and fin whale (see
  ## marine_megafauna_biomass.csv below) - but the broad "Cetaceans"
  ## bucket above matches ALL FIVE cetacean FGs by keyword (every one of
  ## them contains "dolphin" or "whale"), so a Group="Cetaceans" row would
  ## get its biomass SPLIT EVENLY across Bottlenose dolphins/Other
  ## dolphins/Fin whale/Deep sea-cetacean feeders/Sperm whale regardless
  ## of their real relative abundance - throwing away exactly the
  ## species-specificity these new figures provide. Added five
  ## FG-specific categories below so a CSV row can target exactly one
  ## FG; each keyword is checked to match ONLY its intended FG_name
  ## and no other (verified against the real FG_WMed_2026.csv text -
  ## "bottlenose" only appears in "Bottlenose dolphins", "other dolphin"
  ## only in "Other dolphins", etc.). "Cetaceans" is kept too, as a
  ## fallback bucket for a future figure that's genuinely only available
  ## at the whole-cetacean-guild level (e.g. a total abundance survey that
  ## doesn't break out species) - it still splits evenly across all 5 FGs,
  ## which is the correct behavior for a genuinely undifferentiated figure.
  ## 2026-09-25, per Andrea ("i am still missing two FGs of seabirds..."):
  ## same bug as the original single "Cetaceans" bucket - the broad
  ## "Seabirds" keyword list below (still kept, as a genuinely-
  ## undifferentiated-figure fallback) matches BOTH "Pelagic/Offshore
  ## seabirds" (FG7) and "Coastal/inshore seabirds" (FG8) by keyword
  ## (both FG_names literally contain the substring "seabirds"), so a
  ## Group="Seabirds" row would still split evenly across both FGs
  ## regardless of which one a real count actually describes. Added
  ## PelagicSeabirds/CoastalSeabirds as their own keyword categories -
  ## each keyword checked to match ONLY its intended FG_name text
  ## ("pelagic/offshore seabird" only appears in FG7, "coastal/inshore
  ## seabird" only in FG8).
  MEGAFAUNA_TAXON_KEYWORDS <- list(
    Cetaceans              = c("cetacean", "dolphin", "whale", "porpoise"),
    BottlenoseDolphins     = c("bottlenose"),
    OtherDolphins          = c("other dolphin"),
    FinWhale               = c("fin whale"),
    SpermWhale             = c("sperm whale"),
    DeepSeaCetaceanFeeders = c("deep sea-cetacean", "deep sea cetacean"),
    Pinnipeds  = c("seal", "monk seal"),
    Seabirds   = c("seabird", "shearwater", "gull", "petrel", "tern", "auk", "cormorant"),
    PelagicSeabirds = c("pelagic/offshore seabird", "pelagic seabird"),
    CoastalSeabirds = c("coastal/inshore seabird", "coastal seabird"),
    SeaTurtles = c("turtle")
  )
  megafauna_biomass_fg_year <- load_manual_cited_biomass_group(
    csv_path = MEGAFAUNA_BIOMASS_PATH, taxon_keywords = MEGAFAUNA_TAXON_KEYWORDS,
    label = "Marine megafauna biomass", review_csv_name = "marine_megafauna_group_to_fg_REVIEW.csv",
    output_csv_name = "marine_megafauna_biomass_by_fg.csv",
    fallback_message = paste0("A single 1994-1996 baseline figure per group is enough - no time series needed.",
                              " Preferred real sources, per Andrea (2026-09-24): AERIAL SURVEY density/abundance for cetaceans -",
                              " the ACCOBAMS Survey Initiative (ASI-Med-Report, accobams.org) and the dedicated Central/Western",
                              " Mediterranean aerial survey (Panigada et al., ScienceDirect S0967064517301418) both report real",
                              " aerial-survey density/abundance by species and sub-region; CENSUS data for seabirds - UNEPMAP's",
                              " Mediterranean Quality Status Report 'Common Indicator 4: Population abundance - Seabirds'",
                              " (medqsr.org) is the closest thing to a single Mediterranean-wide seabird census, with the World",
                              " Seabird Union's database directory (worldseabirdunion.org) as a second place to check for a",
                              " colony-count series; nesting-beach count series x a published nests-to-population factor, e.g.",
                              " Casale et al. (sea turtles - the one group with count series reaching back to the 1990s). A cited",
                              " B t/km2 straight out of Piroddi et al. 2015 (Mar Ecol Prog Ser 533:47-65, supplementary tables",
                              " S2/S3, Western Mediterranean sub-region) remains a fallback/cross-check if a real aerial-survey or",
                              " census figure isn't available for a given species/year. Expected columns: Year, Group (Cetaceans/Seabirds/SeaTurtles, or a",
                              " specific FG_name), Biomass_t (total for the whole West Med study area, not a density), Source_citation.")
  )
  megafauna_species_lit_biomass <- attr(megafauna_biomass_fg_year, "species_detail")
  
  ## --- Lower trophic levels (zooplankton, phytoplankton, macroalgae, ------
  ## seagrass/Posidonia, gorgonians/corals) - same situation as megafauna:
  ## MEDITS/MEDIAS are bottom-trawl surveys and were never designed to
  ## sample these groups representatively (see EXEMPT_FG_NAMES near the
  ## top of this script, which already excludes them from the trawl
  ## catchability correction for exactly this reason) - so there's no
  ## survey density figure to fall back on here either.
  ##
  ## 2026-09-24, per Andrea: pull this from EcoBase (existing published
  ## Ecopath models covering the Mediterranean) rather than hand-copying
  ## numbers out of a paper - EcoBase already has these exact groups as
  ## Biomass inputs in OTHER models' own Ecopath parameterizations, and is
  ## therefore the fastest, most directly comparable (same B t/km2 EwE
  ## unit) source, on top of whatever satellite/other-source estimates are
  ## closest to 1994-1996.
  ##
  ## fetch_ecobase_literature_biomass() (03b_ecobase.R) queries EcoBase,
  ## keyword-matches every returned group_name against the 7 target
  ## groups below, and for each one picks whichever candidate model's
  ## year is CLOSEST to 1995 (midpoint of YEAR_ECOPATH) - written to
  ## ecobase_literature_biomass_best_by_group.csv with the source model/
  ## year/authors and how far that year actually is from 1995, so "closest
  ## available, not a real 1994-1996 measurement" stays explicit. If
  ## PRIMARY_PRODUCER_BIOMASS_PATH doesn't exist yet, that EcoBase result
  ## is used to AUTO-DRAFT it (clearly tagged as an EcoBase draft, not a
  ## final reviewed figure) - so load_manual_cited_biomass_group() below
  ## always has something to read, without needing Andrea/Daniel to
  ## manually type numbers in first. Once a real, reviewed CSV is placed
  ## at that path, this auto-draft step is skipped entirely (existing file
  ## always wins - never overwritten by this block).
  ##
  ## A target group EcoBase has no match for at all (reported by
  ## fetch_ecobase_literature_biomass() itself) still needs a non-EcoBase
  ## source - candidates from this research: satellite chlorophyll-a ->
  ## phytoplankton biomass conversions (ocean-colour record starts
  ## ~1997/1998 (SeaWiFS), so also a "closest available year" proxy, not
  ## a real 1994-1996 measurement); Posidonia standing biomass per m2 from
  ## the seagrass-ecology literature (e.g. Pergent et al.) x the actual
  ## meadow area within the study area's strata, if known.
  PRIMARY_PRODUCER_BIOMASS_PATH <- file.path(pcloud_dir, "data/primary_producer_plankton_biomass.csv")
  ## Full keyword catalog - used below to match the manual/auto-drafted
  ## CSV's Group column to an FG_name, REGARDLESS of which source (EcoBase
  ## or satellite) actually supplied each group's number.
  ## 2026-09-25, per Andrea ("suprabenthos... salps and gelatinous
  ## zooplankton and jellyfish... and cymodocea for biomass"): these four
  ## groups were NEVER in this keyword list before - meaning the EcoBase
  ## auto-draft block above (fetch_ecobase_literature_biomass()) has never
  ## actually searched for them, even though EcoBase model group_names
  ## commonly use exactly these labels (per Andrea, re: Suprabenthos
  ## specifically: "usually a lot of models include this FG with that
  ## name") - a real, silent gap, not just missing manual data. Added as
  ## their own categories so a future EcoBase query (or a manual/cited CSV
  ## row) can target each one specifically. NOTE: because
  ## PRIMARY_PRODUCER_BIOMASS_PATH already exists (Andrea/Daniel's real,
  ## reviewed data/primary_producer_plankton_biomass.csv), the auto-draft
  ## block above is SKIPPED entirely this run ("existing file always wins
  ## - never overwritten") - these new categories only take effect once
  ## EITHER (a) that file is deleted/renamed so the EcoBase auto-draft
  ## runs fresh and can populate them, or (b) real cited rows for
  ## Suprabenthos/Cymodocea/GelatinousZooplankton are added to that CSV by
  ## hand, same as the megafauna file above. Web search this session
  ## (2026-09-25) could NOT find a citable Western-Med-specific standing-
  ## biomass figure for any of these three (Suprabenthos: Corrales et al.
  ## 2015/South Catalan Sea Ecopath papers exist but are paywalled;
  ## Cymodocea nodosa: literature has LEAF/RHIZOME PRODUCTION rates, e.g.
  ## Pérez & Camp 1986 Mar Menor lagoon 160-427 g DW/m2/year, but no
  ## standing-biomass figure; gelatinous zooplankton/salps: Mediterranean-
  ## specific trawl-survey biomass papers found by title but not
  ## accessible full-text) - genuinely still open, not filled with a
  ## guess. "Jellyfish" (FG68) and "Other macro-benthos" (FG67) are NOT
  ## added here - both already receive a small non-NA MEDITS-survey-
  ## derived value in the current real output (confirmed against Daniel's
  ## actual ecopath_ecosim_inputs.xlsx, Ecopath_B sheet: Jellyfish =
  ## 0.000196 t/km2, Other macro-benthos = 0.003135 t/km2) - genuinely
  ## missing is Suprabenthos/Cymodocea/"Salps and other gelatinous
  ## zooplankton" (all = NA in that same real output), which is what these
  ## three new categories target. The tiny Jellyfish/macro-benthos MEDITS
  ## values are likely a real undersample (bottom trawls are known to
  ## catch gelatinous fauna poorly) rather than a bug - worth a literature
  ## cross-check later, but that is a "is this number too low" question,
  ## not a "this cell is empty" one, so left untouched here.
  PRIMARY_PRODUCER_TAXON_KEYWORDS <- list(
    MacroZooplankton     = c("macrozooplankton", "macro-zooplankton", "macro zooplankton"),
    MesoMicroZooplankton = c("mesozooplankton", "meso-zooplankton", "meso zooplankton",
                             "microzooplankton", "micro-zooplankton", "micro zooplankton"),
    LargePhytoplankton   = c("large phytoplankton", "diatom"),
    SmallPhytoplankton   = c("small phytoplankton", "picophytoplankton", "nanophytoplankton"),
    Posidonia            = c("posidonia", "seagrass"),
    Macroalgae           = c("macroalga", "macro-alga"),
    GorgoniansCorals     = c("gorgonian", "coral"),
    Suprabenthos         = c("suprabenthos", "supra-benthos", "supra benthos"),
    Cymodocea            = c("cymodocea"),
    GelatinousZooplankton = c("gelatinous zooplankton", "salp", "salpidae", "thaliacea", "jellyfish"),
    
    ## 2026-09-26, per Andrea: "Benthic mollusc" and "Other macro-benthos"
    ## are NOT survey-exempt (MEDITS does sample them - see the real
    ## Ecopath_B values referenced near EXEMPT_FG_NAMES above, ~0.00314
    ## and ~0.000196 t/km2), but a bottom-trawl survey badly under-samples
    ## small/soft-bodied/burrowing benthos, so per Andrea these two FGs
    ## should ALSO be built from literature, one keyword category PER REAL
    ## TAXONOMIC SUBGROUP (each pointing at the SAME FG) so load_manual_
    ## cited_biomass_group()'s existing sum-by-FG_num/Year aggregation adds
    ## them back up into one FG total - e.g. "Benthic mollusc" = Bivalvia +
    ## Gastropoda + Scaphopoda + Polyplacophora. Composition confirmed
    ## against Daniel's real FG_WMed_2026.csv (2026-09-26): Benthic
    ## mollusc = 96 Bivalvia spp, 129 Gastropoda spp, 4 Scaphopoda spp, 2
    ## Polyplacophora spp; Other macro-benthos = 18 classes (Echinoidea,
    ## Holothuroidea, Hexacorallia, Demospongiae, Gymnolaemata, Thecostraca,
    ## Ophiuroidea, Polychaeta, Asteroidea, Crinoidea, Ascidiacea, Hydrozoa,
    ## Stenolaemata, Octocorallia, Articulata, Clitellata, Palaeonemertea).
    ## Each key below is the exact "Group" value a CSV row must use - the
    ## keyword itself (the FG_name substring to match) is the SAME for
    ## every subgroup of one FG, on purpose, since it's the FG they all
    ## belong to, not a per-class FG_name difference.
    ##
    ## NO REAL BIOMASS/DENSITY NUMBERS EXIST YET for any of these rows -
    ## this only wires the matching so a real CSV row (Biomass_t, or a
    ## Density_value + Depth_min_m/Depth_max_m/Habitat_species per the
    ## 2026-09-26 extrapolation extension above) drops straight in. Until
    ## then these categories simply find zero raw rows and do nothing -
    ## the current MEDITS-survey figures for both FGs are untouched.
    BenthicMollusc_Bivalvia       = c("benthic mollusc"),
    BenthicMollusc_Gastropoda     = c("benthic mollusc"),
    BenthicMollusc_Scaphopoda     = c("benthic mollusc"),
    BenthicMollusc_Polyplacophora = c("benthic mollusc"),
    
    Macrobenthos_Echinoidea      = c("other macro-benthos"),
    Macrobenthos_Holothuroidea   = c("other macro-benthos"),
    Macrobenthos_Hexacorallia    = c("other macro-benthos"),
    Macrobenthos_Demospongiae    = c("other macro-benthos"),
    Macrobenthos_Gymnolaemata    = c("other macro-benthos"),
    Macrobenthos_Thecostraca     = c("other macro-benthos"),
    Macrobenthos_Ophiuroidea     = c("other macro-benthos"),
    Macrobenthos_Polychaeta      = c("other macro-benthos"),
    Macrobenthos_Asteroidea      = c("other macro-benthos"),
    Macrobenthos_Crinoidea       = c("other macro-benthos"),
    Macrobenthos_Ascidiacea      = c("other macro-benthos"),
    Macrobenthos_Hydrozoa        = c("other macro-benthos"),
    Macrobenthos_Stenolaemata    = c("other macro-benthos"),
    Macrobenthos_Octocorallia    = c("other macro-benthos"),
    Macrobenthos_Articulata      = c("other macro-benthos"),
    Macrobenthos_Clitellata      = c("other macro-benthos"),
    Macrobenthos_Palaeonemertea  = c("other macro-benthos")
  )
  ## 2026-09-24, per Andrea (refining the 2026-09-24-earlier satellite
  ## approach): phytoplankton should come from a Mediterranean
  ## BIOGEOCHEMICAL MODEL (Copernicus Marine's Med BGC Reanalysis,
  ## MedBFM/OGSTM-BFM - reports phytoplankton biomass directly as carbon,
  ## not a Chl-a proxy) - see lib_cmems_phytoplankton_biomass.R. Satellite
  ## chlorophyll-a (lib_satellite_phytoplankton_biomass.R, kept as-is) is
  ## now the FALLBACK if CMEMS access isn't set up (it needs a free
  ## Copernicus Marine account + the copernicusmarine CLI - see that
  ## function's own header) or the query fails for any reason.
  ##
  ## Zooplankton (Macro-/MesoMicroZooplankton) - the SAME reanalysis has
  ## NO public zooplankton biomass variable at all (confirmed from its
  ## own product documentation - only phytoplankton/chlorophyll/
  ## nutrients/carbon system are released, even though the underlying
  ## BFM model simulates zooplankton internally). Zooplankton therefore
  ## still goes through EcoBase below, same as macroalgae/seagrass/
  ## gorgonians+corals - flagged explicitly so this isn't mistaken for
  ## an oversight; ask CMCC/OGS directly for their model's internal
  ## zooplankton output if you want the true biogeochemical-model value.
  ECOBASE_LOWTROPHIC_KEYWORDS <- PRIMARY_PRODUCER_TAXON_KEYWORDS[
    !names(PRIMARY_PRODUCER_TAXON_KEYWORDS) %in% c("LargePhytoplankton", "SmallPhytoplankton")]
  
  if (!file.exists(PRIMARY_PRODUCER_BIOMASS_PATH) && ENABLE_ECOBASE_BIOMASS_QUERY) {
    ecobase_best_lowtrophic <- fetch_ecobase_literature_biomass(
      out_dir = csv_out_dir, force_refresh = ECOBASE_BIOMASS_FORCE_REFRESH,
      target_fg_keywords = ECOBASE_LOWTROPHIC_KEYWORDS, target_year = round(mean(YEAR_ECOPATH))
    )
    if (!is.null(ecobase_best_lowtrophic) && nrow(ecobase_best_lowtrophic) > 0) {
      ## Biomass_t_km2 (EcoBase's native Ecopath unit) -> a total tonnage,
      ## same convention load_manual_cited_biomass_group() expects
      ## (Biomass_t for the whole West Med study area) - scaled by the
      ## SAME Total_Area_km2 every other biomass figure in this script
      ## uses, via strata_area_by_area (already in scope this far down).
      ecobase_draft <- ecobase_best_lowtrophic[, .(
        Year = round(mean(YEAR_ECOPATH)),
        Group = TargetGroup,
        Biomass_t = Biomass_t_km2 * sum(strata_area_by_area$area_km2, na.rm = TRUE),
        Source_citation = paste0("AUTO-DRAFT FROM ECOBASE (2026-09-24) - REVIEW BEFORE TRUSTING: ", Source_citation)
      )]
      fwrite(ecobase_draft, PRIMARY_PRODUCER_BIOMASS_PATH)
      message("\n[Primary producer/plankton biomass] No reviewed CSV existed yet at '", PRIMARY_PRODUCER_BIOMASS_PATH,
              "' - auto-drafted ", nrow(ecobase_draft), " row(s) from EcoBase's closest-to-", round(mean(YEAR_ECOPATH)),
              " model: ", paste(ecobase_draft$Group, collapse = ", "), ". REVIEW this file - it's a starting point,",
              " not a final figure - then re-run. Delete it and this step will re-draft next run; replace it with",
              " your own reviewed numbers and it's used as-is (never overwritten once it exists).")
    }
  }
  
  ## 2026-09-29, per Andrea ("I see there is no phytoplankton biomass that",
  ## " should be extracted from the biogeochemical model"): root cause -
  ## the CMEMS/satellite phytoplankton fetch used to be nested INSIDE the
  ## `if (!file.exists(PRIMARY_PRODUCER_BIOMASS_PATH))` block above, i.e.
  ## an all-or-nothing gate on the WHOLE draft mechanism. The moment
  ## Andrea/Daniel's own reviewed primary_producer_plankton_biomass.csv
  ## existed on disk (even with zero Phytoplankton rows in it, same as
  ## the already-documented Suprabenthos/Cymodocea/GelatinousZooplankton
  ## gap above), this entire block - including the phytoplankton fetch -
  ## was skipped ENTIRELY ("existing file always wins"), so
  ## fetch_cmems_phytoplankton_biomass()/fetch_satellite_phytoplankton_
  ## biomass() would never even be attempted, silently, no matter how
  ## many times the pipeline was re-run. Fixed by pulling the
  ## phytoplankton fetch out of that gate entirely: it now runs whenever
  ## Large/SmallPhytoplankton rows are missing from whatever CSV exists
  ## right now (freshly EcoBase-drafted above, or Andrea's own existing
  ## file), and APPENDS the fetched row(s) to that file rather than
  ## requiring the whole file to not exist - so it can never again be
  ## silently blocked by the rest of the file already being reviewed and
  ## real. Existing Phytoplankton rows (if Andrea has already reviewed
  ## and added her own) still always win - this only fills a genuine gap,
  ## never overwrites.
  existing_primary_producer_csv <- if (file.exists(PRIMARY_PRODUCER_BIOMASS_PATH)) {
    tryCatch(fread(PRIMARY_PRODUCER_BIOMASS_PATH, encoding = "UTF-8"), error = function(e) NULL)
  } else NULL
  has_phyto_row <- FALSE
  if (!is.null(existing_primary_producer_csv)) {
    grp_col <- resolve_iccat_col(names(existing_primary_producer_csv), c("Group", "Species", "FG_name", "Taxon", "CommonName"))
    if (!is.na(grp_col)) {
      has_phyto_row <- any(tolower(trimws(existing_primary_producer_csv[[grp_col]])) %in%
                             c("largephytoplankton", "smallphytoplankton"))
    }
  }
  if (!has_phyto_row) {
    if (!exists("ENABLE_CMEMS_PHYTOPLANKTON", envir = .GlobalEnv, inherits = FALSE)) ENABLE_CMEMS_PHYTOPLANKTON <- TRUE
    if (!exists("ENABLE_SATELLITE_PHYTOPLANKTON", envir = .GlobalEnv, inherits = FALSE)) ENABLE_SATELLITE_PHYTOPLANKTON <- TRUE
    phyto_draft <- NULL
    phyto_source_label <- NA_character_
    ## Both lib files below are sourced DEFENSIVELY - a missing file (e.g.
    ## these two new scripts haven't been copied into your local
    ## scripts/ folder yet alongside 01_biomass.R) degrades to a message
    ## and moves on to the next fallback, exactly like a failed network
    ## query does, rather than crashing the whole 01_biomass.R run over
    ## one optional lower-trophic biomass source.
    cmems_lib_path <- file.path(git_dir, "scripts/lib_cmems_phytoplankton_biomass.R")
    satellite_lib_path <- file.path(git_dir, "scripts/lib_satellite_phytoplankton_biomass.R")
    
    if (ENABLE_CMEMS_PHYTOPLANKTON) {
      if (!file.exists(cmems_lib_path)) {
        message("[Primary producer/plankton biomass] '", cmems_lib_path, "' not found - copy",
                " lib_cmems_phytoplankton_biomass.R into your scripts/ folder to enable this source.",
                " Skipping straight to the satellite fallback for this run.")
      } else {
        source(cmems_lib_path)
        cmems_phyto <- fetch_cmems_phytoplankton_biomass(out_dir = csv_out_dir)
        if (!is.null(cmems_phyto) && nrow(cmems_phyto) > 0) {
          phyto_draft <- cmems_phyto
          phyto_source_label <- "CMEMS MED BGC REANALYSIS (biogeochemical model, phytoplankton carbon)"
        }
      }
    }
    if (is.null(phyto_draft) && ENABLE_SATELLITE_PHYTOPLANKTON) {
      message("[Primary producer/plankton biomass] CMEMS phytoplankton unavailable this run",
              " (ENABLE_CMEMS_PHYTOPLANKTON = FALSE, the lib file wasn't found, or the query above",
              " failed/wasn't set up) - falling back to satellite chlorophyll-a.")
      if (!file.exists(satellite_lib_path)) {
        message("[Primary producer/plankton biomass] '", satellite_lib_path, "' ALSO not found - copy",
                " lib_satellite_phytoplankton_biomass.R into your scripts/ folder to enable this fallback.",
                " Large/SmallPhytoplankton will remain missing this run - both the CMEMS biogeochemical-model",
                " source and its satellite fallback are unavailable in this environment.")
      } else {
        source(satellite_lib_path)
        satellite_phyto <- fetch_satellite_phytoplankton_biomass(out_dir = csv_out_dir)
        if (!is.null(satellite_phyto) && nrow(satellite_phyto) > 0) {
          phyto_draft <- satellite_phyto
          phyto_source_label <- "SATELLITE CHLOROPHYLL-A (fallback - CMEMS biogeochemical model was unavailable)"
        }
      }
    }
    if (!is.null(phyto_draft)) {
      phyto_rows <- phyto_draft[, .(
        Year = round(mean(YEAR_ECOPATH)),
        Group = TargetGroup,
        Biomass_t = Biomass_t_km2 * sum(strata_area_by_area$area_km2, na.rm = TRUE),
        Source_citation = paste0("AUTO-DRAFT FROM ", phyto_source_label, " (2026-09-29) - REVIEW BEFORE TRUSTING: ", Source_citation)
      )]
      if (file.exists(PRIMARY_PRODUCER_BIOMASS_PATH)) {
        fwrite(phyto_rows, PRIMARY_PRODUCER_BIOMASS_PATH, append = TRUE)
        message("\n[Primary producer/plankton biomass] Appended ", nrow(phyto_rows), " Phytoplankton row(s) (",
                paste(phyto_rows$Group, collapse = ", "), ") from ", phyto_source_label, " to the EXISTING '",
                PRIMARY_PRODUCER_BIOMASS_PATH, "' - every other row in that file is untouched. REVIEW the",
                " appended row(s) before trusting them.")
      } else {
        fwrite(phyto_rows, PRIMARY_PRODUCER_BIOMASS_PATH)
        message("\n[Primary producer/plankton biomass] Created '", PRIMARY_PRODUCER_BIOMASS_PATH, "' with ",
                nrow(phyto_rows), " Phytoplankton row(s) from ", phyto_source_label, ". REVIEW before trusting.")
      }
    } else {
      message("[Primary producer/plankton biomass] No Phytoplankton row exists yet and BOTH the CMEMS",
              " biogeochemical-model source and the satellite chlorophyll-a fallback were unavailable this run",
              " (lib file(s) not found in '", file.path(git_dir, "scripts"), "', or the query failed) - Large/",
              "SmallPhytoplankton will have no Ecopath_B this run. If lib_cmems_phytoplankton_biomass.R exists in",
              " your real scripts/ folder, check it's actually being found at that exact path, and that a",
              " Copernicus Marine account/copernicusmarine CLI is set up for it to query.")
    }
  }
  
  primary_producer_biomass_fg_year <- load_manual_cited_biomass_group(
    csv_path = PRIMARY_PRODUCER_BIOMASS_PATH, taxon_keywords = PRIMARY_PRODUCER_TAXON_KEYWORDS,
    label = "Primary producer/plankton biomass", review_csv_name = "primary_producer_group_to_fg_REVIEW.csv",
    output_csv_name = "primary_producer_plankton_biomass_by_fg.csv",
    fallback_message = paste0("A single 1994-1996 baseline figure per group is enough - no time series needed.",
                              " MEDITS/MEDIAS are bottom-trawl surveys and don't sample these groups at all (same reason they're in",
                              " EXEMPT_FG_NAMES above). With ENABLE_ECOBASE_BIOMASS_QUERY/ENABLE_CMEMS_PHYTOPLANKTON/",
                              " ENABLE_SATELLITE_PHYTOPLANKTON = TRUE (all default) this file should have been auto-drafted just",
                              " above - if you're seeing this message, every query failed (check the [EcoBase biomass]/",
                              " fetch_cmems_phytoplankton_biomass()/fetch_satellite_phytoplankton_biomass() messages above) or",
                              " matched nothing. Other real sources: Posidonia standing biomass per m2 (seagrass-ecology",
                              " literature, e.g. Pergent et al., or satellite/Sentinel-2-derived meadow extent x that per-m2",
                              " figure) x actual meadow area. Expected columns: Year, Group (a target group name above, or a",
                              " specific FG_name), Biomass_t (total for the whole West Med study area, not a density),",
                              " Source_citation.")
  )
  primary_producer_species_lit_biomass <- attr(primary_producer_biomass_fg_year, "species_detail")
  
  ## 2026-09-29, per Andrea ("the proportion of species within FG should
  ## include all values inputted in B Ecopath, including the literature
  ## ones... it will be used for the pbqb"): combine both manual-cited
  ## sources' per-species detail (where a Species column was supplied -
  ## see load_manual_cited_biomass_group()'s Species handling above) into
  ## one table used below, when FG_spp_Ecopath is built, to set prop_sp_fg
  ## DIRECTLY from whichever literature figures actually built that FG's
  ## Ecopath_B total - instead of a separately-maintained relative-weight
  ## CSV that could in principle drift out of step with the real numbers.
  species_lit_biomass <- rbindlist(list(megafauna_species_lit_biomass, primary_producer_species_lit_biomass),
                                   use.names = TRUE, fill = TRUE)
  if (nrow(species_lit_biomass) > 0) {
    message("[Species-level literature biomass] ", uniqueN(species_lit_biomass$Species), " species across ",
            uniqueN(species_lit_biomass$FG_num), " FG(s) have a direct literature biomass figure (not just an",
            " FG-level total) - these will set prop_sp_fg directly in FG_spp_Ecopath below.")
  }
  
  ## 2026-09-29, per Andrea (pasted two real sources: Telesca et al. 2015 on
  ## Posidonia decline, Linares et al. 2021 on gorgonian collapse from
  ## marine heatwaves): same "flag the real direction, never fabricate a
  ## ratio" pattern already used for ICCAT BFT/SWO (ICCAT_TEMPORAL_BASELINE_
  ## NOTE above) and the generic Collection_year mechanism inside
  ## load_manual_cited_biomass_group() - but those two only flag a generic
  ## "review whether this was higher/lower" message. These two groups now
  ## get the actual documented direction and a real citation, same as BFT/
  ## SWO got by hand. Matched on FG_name (the aggregated group_fg_year
  ## output collapses away the original "Group" column), using the same
  ## keyword text as PRIMARY_PRODUCER_TAXON_KEYWORDS's Posidonia/
  ## GorgoniansCorals entries above.
  PRIMARY_PRODUCER_TEMPORAL_BASELINE_NOTE <- list(
    Posidonia = paste0(
      "TEMPORAL TREND (Mediterranean-wide, real citation): Telesca et al. 2015 (Scientific Reports 5:12505,",
      " 'Seagrass meadows (Posidonia oceanica) distribution and trajectories of change') report a 34%",
      " regression in Posidonia oceanica meadow AREA over the last ~50 years across sites with historical",
      " data (Spain, France/Monaco, Italy, Albania, Tunisia, Egypt, Turkey) - a basin-wide DECLINING",
      " trend, not a Western-Med-specific or 1994-1996-anchored figure. If this FG's standing-biomass",
      " density comes from a present-day/2010s+ field measurement (e.g. Bernardeau-Esteller et al. 2023,",
      " Cadiz), the 1994-1996 Ecopath baseline biomass was very likely HIGHER than that measurement",
      " implies. No numeric correction factor applied - the 34%-over-50-years figure is basin-wide areal",
      " extent, not a per-site biomass-density trend, so it is not safely convertible into a multiplier",
      " for one site's density figure."
    ),
    GorgoniansCorals = paste0(
      "TEMPORAL TREND (NW Mediterranean, real citation): Linares et al. 2021 (Proceedings of the Royal",
      " Society B 288:20212384, 'Population collapse of habitat-forming species in the Mediterranean: a",
      " long-term study of gorgonian populations affected by recurrent marine heatwaves') document",
      " gorgonian population COLLAPSE driven by recurrent marine-heatwave mass-mortality events",
      " (documented from the early 2000s onward). If this FG's AFDM-density figure comes from a",
      " post-2010s field survey (e.g. Ambroso et al. 2019, Cap de Creus), the 1994-1996 Ecopath baseline",
      " biomass was very likely HIGHER than that measurement implies (i.e. before most of the documented",
      " heatwave mortality). No numeric correction factor applied - the paper reports population/cover",
      " trend at specific monitored sites, not a basin-wide biomass-density multiplier."
    )
  )
  if (nrow(primary_producer_biomass_fg_year) > 0) {
    temporal_trend_hits <- list(
      Posidonia        = grepl("posidonia|seagrass", primary_producer_biomass_fg_year$FG_name, ignore.case = TRUE),
      GorgoniansCorals = grepl("gorgonian|coral", primary_producer_biomass_fg_year$FG_name, ignore.case = TRUE)
    )
    for (grp_nm in names(PRIMARY_PRODUCER_TEMPORAL_BASELINE_NOTE)) {
      hit_rows <- temporal_trend_hits[[grp_nm]]
      if (any(hit_rows)) {
        primary_producer_biomass_fg_year[hit_rows, group_sources :=
                                           paste0(group_sources, " -- ", PRIMARY_PRODUCER_TEMPORAL_BASELINE_NOTE[[grp_nm]])]
        message("[Primary producer/plankton biomass] Temporal-trend note appended for '", grp_nm, "' (",
                sum(hit_rows), " FG x Year row(s)) - see group_sources / B_ref.")
      }
    }
    fwrite(primary_producer_biomass_fg_year[temporal_trend_hits$Posidonia | temporal_trend_hits$GorgoniansCorals,
                                            .(FG_num, FG_name, Year, group_biomass_t, group_sources)],
           file.path(csv_out_dir, "primary_producer_temporal_trend_REVIEW.csv"))
  }
  
  stock_assessment_fg_year <- merge_manual_cited_biomass(stock_assessment_fg_year, megafauna_biomass_fg_year, "marine megafauna")
  stock_assessment_fg_year <- merge_manual_cited_biomass(stock_assessment_fg_year, primary_producer_biomass_fg_year, "primary producer/plankton")
  
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
  ## "survey_exempt" (2026-09-24, per Andrea): a NEW, THIRD ecology type,
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
  
  ## FG_name dropped from the group-by here (2026-09-24) - it's always
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
  
  ## 2026-09-24 fix, per Andrea: build the FULL (FG_num, Year) row set
  ## FIRST - every FG the SURVEY caught (fg_index_avg) UNIONED with every
  ## FG/Year the stock-assessment/megafauna table covers
  ## (stock_assessment_fg_year, which by this point already has ICCAT
  ## bluefin tuna/swordfish/albacore AND the manual-cited megafauna
  ## biomass folded into it - see merge_manual_cited_biomass() above).
  ## BEFORE this fix, every merge below was all.x=TRUE onto fg_index_avg
  ## ALONE, so an FG the survey has ZERO catch for (bluefin tuna,
  ## swordfish, and every megafauna FG - none of these are ever actually
  ## landed in a MEDITS bottom trawl or a MEDIAS acoustic transect) had
  ## no row to attach its stock-assessment/megafauna override density
  ## to, and silently never appeared in fg_index_regional_combined AT
  ## ALL - not just missing PB/QB downstream (03_pbqb-traits.R), missing
  ## from Ecopath_B itself, despite this section's own comment above
  ## claiming these FGs' biomass "attaches automatically". Caught by
  ## Andrea noticing bluefin tuna/swordfish/cetaceans/seabirds had no
  ## PB/QB at all and asking why.
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
      ## "survey_exempt" checked FIRST (2026-09-24, per Andrea): prefer
      ## the manual/EcoBase-cited stock_assessment_density_t_km2 exactly
      ## like single_species_assessed does, but if THAT doesn't exist
      ## either, stop here with NA - do NOT fall through to
      ## medias_density/medits_density/avg_density below. A trawl/
      ## acoustic survey catching a stray zooplankton/suprabenthos/
      ## macroalgae/seagrass/coralligenous individual is bycatch noise,
      ## not a real density measurement for that FG - using it (as
      ## happened before this fix - see Macro zooplankton in
      ## stock_assessment_biomass_crosscheck.csv getting a nonsense
      ## 2.9e-05 t/km2 from MEDITS instead of the real ~120 t/km2
      ## EcoBase literature figure) is actively worse than leaving the
      ## cell genuinely missing (which at least shows up in
      ## fg_missing_ecopath_B_REVIEW.csv for review).
      FG_ECOLOGY_TYPE == "survey_exempt" & !is.na(stock_assessment_density_t_km2), stock_assessment_density_t_km2,
      FG_ECOLOGY_TYPE == "survey_exempt", NA_real_,
      FG_ECOLOGY_TYPE == "single_species_assessed" & !is.na(stock_assessment_density_t_km2), stock_assessment_density_t_km2,
      !is.na(medias_density), medias_density,
      !is.na(medits_density), medits_density,
      ## 2026-09-24 fix, per Andrea: a "mixed" (multi-species)
      ## stock-assessment/megafauna FG - e.g. a cetacean or seabird FG
      ## that lumps several species - used to have NO fallback here at
      ## all: FG_ECOLOGY_TYPE only equals "single_species_assessed" for
      ## an EXCLUSIVELY single-species FG, so a mixed FG's own group-
      ## level megafauna density was simply discarded even when it was
      ## the ONLY density available (medias/medits/avg_density all NA
      ## for an FG the survey never samples). This is exactly the case
      ## load_manual_cited_biomass_group() exists for - its output IS
      ## already the right group-level total for that whole FG, so use
      ## it here rather than let the row fall through to NA.
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
  ## Guard (2026-09-24, per Andrea): this backfill must NEVER touch a
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
  ## STEP 9h: Multistanza (juvenile/adult) biomass split (2026-09-26,
  ## per Andrea - fix for code-review finding #3: "European hake adult"
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
  ## Andrea's explicit choice (2026-09-26): split whatever total density
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
    
    STECF_FDI_PARENT_DIR_BIOMASS <- file.path(pcloud_dir, "data/fisheries/FDI")
    stecf_fdi_dir_biomass <- resolve_versioned_data_subdir(STECF_FDI_PARENT_DIR_BIOMASS, "Biological")
    if (is.na(stecf_fdi_dir_biomass)) stecf_fdi_dir_biomass <- STECF_FDI_PARENT_DIR_BIOMASS
    stecf_bio_dir_biomass <- file.path(stecf_fdi_dir_biomass, "Biological")
    
    fao_species_biomass <- tryCatch(resolve_fao_species_reference(pcloud_dir, csv_out_dir), error = function(e) {
      message("[Multistanza biomass split] Could not load the FAO species reference (", conditionMessage(e),
              ") - the multistanza biomass split is skipped this run, FG(s) stay whatever STEP 9g resolved.")
      NULL
    })
    
    multistanza_biomass_split_applied <- data.table()
    if (!is.null(fao_species_biomass)) {
      multistanza_age_proportion <- compute_multistanza_age_proportion(
        multistanza_fg_pairs, stecf_bio_dir_biomass, fao_species_biomass)
      
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
                                    "% of '", sci_name, "' combined total, via STECF FDI Biological Age data's",
                                    " juvenile:adult catch proportion - see multistanza_biomass_split_REVIEW.csv)")
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
                  "\") now carries the remaining ", round(100 * missing_share, 1), "%, split via STECF FDI's",
                  " juvenile:adult catch-age proportion (", round(100 * p_juv, 1), "% juvenile).")
        }
        setorder(fg_index_regional_combined, FG_num, Year)
        if (nrow(multistanza_biomass_split_applied) > 0) {
          fwrite(multistanza_biomass_split_applied, file.path(csv_out_dir, "multistanza_biomass_split_REVIEW.csv"))
          message("[Multistanza biomass split] Applied to ", nrow(multistanza_biomass_split_applied),
                  " of ", nrow(multistanza_fg_pairs), " multistanza species pair(s) - see",
                  " multistanza_biomass_split_REVIEW.csv. This is a PROXY (catch-age selectivity, not a direct",
                  " biomass-age measurement) - review before trusting.")
        }
      }
    }
  }
  
  ## 2026-09-26: the single most useful "at a glance" biomass validation
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
    ## 2026-09-27 fix: a handful of FGs (typically Detritus/
    ## plankton/megafauna-adjacent groups) sit 1-2+ orders of magnitude
    ## above everything else, so on a LINEAR x-axis every other bar gets
    ## compressed down to a sliver next to them. pseudo_log_trans behaves
    ## like log10 once values get large but stays linear (and defined) near
    ## zero, so FGs with a real but small density are still visible AND a
    ## genuine 0 (missing) still plots at 0 instead of erroring the way a
    ## true log scale would.
    ggplot(d, aes(x = mean_density, y = reorder(FG_label, mean_density), fill = Tier)) +
      geom_col() +
      scale_x_continuous(trans = scales::pseudo_log_trans(base = 10)) +
      scale_fill_manual(values = c("Survey (MEDITS/MEDIAS)" = "#4C6FE7", "Stock assessment (ICCAT/STAR-RAM)" = "#2E7D32",
                                   "Manual-cited literature" = "#F9A825", "EcoBase (literature model)" = "#8E24AA",
                                   "Missing" = "grey70", "Other" = "grey40")) +
      labs(title = "Biomass by functional group at the Ecopath baseline, by data source",
           subtitle = paste0("Mean density, Year ", paste(range(YEAR_ECOPATH), collapse = "-"), " - color = which source actually supplied the value.",
                             " x-axis is log-like (pseudo-log) so small-density FGs stay visible next to a few very large ones"),
           x = "t/km2 (pseudo-log scale)", y = NULL, fill = "Source") +
      theme_minimal(base_size = 7) + theme(legend.position = "bottom")
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

## Read back Ecopath_B's per-FG Biomass (just written above) so STEP 11
## below can reconcile FG_spp_Ecopath's species-level proportions
## against the SAME FG-level Biomass Ecopath_B actually uses - see that
## step's own comment for why this matters (the two used to be built
## from different sources and could disagree).
ecopath_b_by_fg <- as.data.table(openxlsx::read.xlsx(file.path(out_dir, "ecopath_ecosim_inputs.xlsx"), sheet = "Ecopath_B"))
ecopath_b_by_fg[, FG_num := as.integer(FG_num)]

## =================================================================
## STEP 10b: "Ecobase" workbook sheet (2026-09-24, per Andrea: "add a
## section where it adds a Ecobase sheet on the excel input file
## produced with data from the model biomass pbqb reference year etc").
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
## 2026-09-24, per Andrea: a species genuinely present in the West Med
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
##     for that FG. Andrea (2026-09-28), from exactly this case ("Other
##     dolphins" showing Density=0/prop_sp_fg=0 for both species): "this
##     biomass is resulted from the sum of species and this should be
##     noted on the FG_spp".
## Fixed by falling back to an EVEN split (1/n species) whenever the
## whole FG's survey density is zero - so Biomass_t_km2 always sums back
## to Ecopath_B's real total, never silently to zero - and by adding a
## prop_sp_fg_basis column that says in plain language whether a row's
## prop_sp_fg is a real density-weighted split or an even-split
## assumption (i.e. exactly the note Andrea asked for), so an even split
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
## 2026-09-29, per Andrea: "the proportion of species within FG should
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
## the flat even split Andrea correctly flagged as implausible ("is this
## actually the same exact proportion among species? i think that this
## isnt accurate" / "this needs review").
##
## "Other dolphins" (Delphinus delphis + Stenella coeruleoalba): its
## ACCOBAMS Survey Initiative figure is documented (2026-09-25, per
## Andrea - see the MEGAFAUNA_TAXON_KEYWORDS comment above) as a STRIPED
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
## 2026-09-29, per Andrea (pasted a detailed literature reconstruction):
## Zotier, Thibault & Guyot 1992 (Mediterranean-wide breeding-pair census,
## close to this pipeline's 1994-1996 baseline) gives real, documented
## breeding populations for the three species that make up "Pelagic/
## Offshore seabirds" (FG7) with a real population figure: Calonectris
## diomedea 57,000-76,000 pairs, Puffinus yelkouan 18,000 pairs "known",
## Hydrobates pelagicus 8,500-15,000 pairs "known" (Puffinus mauretanicus
## is the fourth FG7 species but Andrea's own notes give no population
## figure for it - left out of this weighting, not guessed). Converted to
## a relative biomass weight via breeding pairs x 2 (breeding adults only
## - non-breeders/juveniles are NOT included, a real, flagged
## underestimate of true standing biomass, but irrelevant to a RELATIVE
## weight as long as the undercount is roughly proportional across the
## three species) x typical adult body mass - species-typical mass is
## NOT from Andrea's literature (she didn't supply one) - Calonectris
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
## actually zero. Add a real population figure for it (Andrea's notes
## flag Balearic-only breeding monitoring exists but gave no number here)
## to fix this properly.
## 2026-09-29 CORRECTION: an earlier note here claimed "Coastal/inshore
## seabirds" (FG8) has NO species assigned to it at all in Andrea's
## taxonomy - WRONG, confirmed by Andrea's own FG_spp screenshot: FG8
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
## HISTORY (same day, 2026-09-29, kept short - see the project doc
## `prop_sp_fg_species_weight_code_only_2026-09-29.md` for the full
## back-and-forth): this started as hardcoded R literals; per Andrea
## ("the biomass from literature should be read it from the csv file in
## pcloud dir") it briefly became a CSV read from pcloud_dir; per Andrea
## ("wouldnt be better to read the species density biomass from
## literature reference and then calculate it?") that CSV was revised to
## carry raw Abundance/Body_mass_kg components instead of a pre-multiplied
## figure; per Andrea ("the species weight within fg should be calculated
## inside the code, not created manually") the CSV is now removed
## entirely and replaced with the plain computed-in-R list below - same
## raw-components transparency (explicit Abundance x Body_mass_kg
## multiplication, not an opaque pre-multiplied number), just no longer a
## file to maintain, since these are DERIVED relative weights, not their
## own literature observation (see the REVERSAL note directly below for
## the reasoning on where that line sits).
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
## 2026-09-29 REVERSAL, per Andrea ("the species weight within fg should be",
## " calculated inside the code, not created manually"): the CSV-loading
## mechanism above (SPECIES_FG_WEIGHT_PATH/load_species_fg_weights(),
## introduced earlier the same day) is REMOVED. Andrea's distinction,
## worked out over this session's back-and-forth: a manually-cited CSV
## belongs to a number that IS an actual observation from the literature
## (marine_megafauna_biomass.csv's Biomass_t rows, which also set
## Ecopath_B - see the "species-level split from the SAME literature
## figures..." block above) - that kind of number should live in an
## editable CSV so Andrea can add/correct it without a code change. A
## RELATIVE weight with no FG-total significance of its own (this list)
## isn't that - it is a derived quantity computed FROM literature
## abundance/body-mass figures that are already fixed citations, so it
## belongs in code as a transparent calculation, not in a second file to
## maintain. Every entry below is still computed from its own raw
## Abundance x Body_mass_kg components (kept as explicit multiplication,
## not a pre-multiplied opaque number - the same transparency Andrea
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
  ## 2026-09-29, per Andrea ("missing input biomass for delphinus
  ## delphis"): Stenella coeruleoalba basin-wide 1991 survey (Forcada et
  ## al. 1994, 117,880 individuals x 120 kg, ~593,660 km2, excl.
  ## Tyrrhenian) vs. Delphinus delphis's much smaller, Alboran-Sea-ONLY
  ## 1991 figure (Forcada & Hammond 1998, 14,736 individuals x 150 kg,
  ## ~90,670 km2) - Andrea's own worked calculation ("B_Delphinus_WMed <-
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
  ## 2026-09-29: skip any FG that already got a direct literature
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
## for those FGs. Andrea (2026-09-28): "the proportion of biomass in the
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

## --- 2026-09-29, per Andrea: "the density in the FG_spp should be in ---
## --- line with the Ecopath_B and it isnt right now" --------------------
## Until now FG_spp kept the RAW MEDITS/MEDIAS survey `Density` sitting
## right next to `Biomass_t_km2`/`Ecopath_B_Biomass`, disagreeing for
## every FG whose real Ecopath_B comes from somewhere other than the
## survey (megafauna/primary-producer literature, stock assessment,
## EcoBase) - e.g. a cetacean species showing Density = 0 alongside a
## real nonzero Ecopath_B_Biomass for the same row. Explained once
## already this session as "expected" (Density is a genuinely different
## measurement), but Andrea is right that having two disagreeing
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
        " within each FG, whatever sourced that FG's total (2026-09-29, per Andrea: 'the density in the FG_spp should be",
        " in line with the Ecopath_B'); Density_survey_raw is the original MEDITS/MEDIAS-observed density, kept for audit -",
        " it's what prop_sp_fg was computed from, but is NOT what 'Density' means in this sheet any more.)")

## --- review list: FGs whose species-level split is NOT a real
## measurement (i.e. NOT MEDITS/MEDIAS density-weighted and not a
## documented single-species/literature-weighted override like the
## cetacean fixes above) - just an equal split across the FG's own
## species, because no per-species density/abundance source exists for
## it at all. Andrea (2026-09-29): "we need to break down the biomass
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
## STEP 12: traits_ewe sheet - MOVED to 03_pbqb-traits.R (2026-09-22).
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
## STEP 12b: FG_References + per-sheet Reference column - rebuilt HERE
## TOO, not only from 04_diets.R.
##
## 2026-09-29, per Andrea: "i dont see the references for each
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
## at all (2026-09-24, per Andrea: "print the FGs at the end of the
## scripts that dont have data on Ecopath B, or L or Di"). Checked
## against fg_index_regional_combined (the exact table that feeds
## Ecopath_B via export_ecopath_ecosim_excel() in STEP 10), restricted
## to the Ecopath base year(s) (YEAR_ECOPATH) - an FG with no row at
## all there, or a row whose mean_density is NA or exactly 0, has no
## real biomass estimate behind it from ANY source (survey, stock
## assessment, ICCAT, marine megafauna, or primary producer/plankton),
## regardless of which fallback ultimately would have applied.
## =================================================================
## 2026-09-24: mirrors the same nearest-year borrow fallback applied to
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
## (2026-09-24, per Andrea: "try to get biomass from other models
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
## 2026-09-26 update, per Andrea - two explicit exclusions from the
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
          " still-missing FG(s) are EXCLUDED from the EcoBase/literature fallback below, per Andrea",
          " (2026-09-26): fish FGs (MEDITS/MEDIAS survey data is their intended source, no literature/",
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
    if (!is.null(ecobase_missing_fill) && nrow(ecobase_missing_fill) > 0) {
      ecobase_missing_fill[, FG_num := as.integer(TargetGroup)]
      ecobase_missing_fill[, Biomass_t := Biomass_t_km2 * sum(strata_area_by_area$area_km2, na.rm = TRUE)]
      
      ## 2026-09-29, per Andrea's validation request (cross-referencing
      ## pipeline_code_review_and_validation_findings.md finding 2, item
      ## 2: "add a plausibility check on any auto-filled EcoBase density
      ## before it's allowed to patch Ecopath_B - reject or flag anything
      ## more than 5-10x the highest density already seen among
      ## comparable FGs in this same model"). Concrete case this catches:
      ## a real prior run auto-filled "Expanding omnivore fish"/
      ## "Expanding herbivore fish" at 311.2 t/km2 from a Bay of Biscay
      ## EcoBase model - a coincidental keyword match ("fish" alone),
      ## ~15-50x this model's own biomass-dominant small-pelagic FGs, and
      ## nothing caught it before it silently patched Ecopath_B. (Those
      ## two FG names are now excluded from this fallback entirely via
      ## is_expanding_fg above, but this bound guards every OTHER FG that
      ## reaches this fallback too - the plausibility problem isn't
      ## specific to those two names.) PLAUSIBILITY_MAX_DENSITY_MULTIPLE
      ## is the threshold, an adjustable constant, not tuned against a
      ## real distribution this session.
      if (!exists("PLAUSIBILITY_MAX_DENSITY_MULTIPLE", envir = .GlobalEnv, inherits = FALSE)) PLAUSIBILITY_MAX_DENSITY_MULTIPLE <- 8
      max_known_density <- suppressWarnings(max(fg_biomass_base_year[!is.na(mean_density) & mean_density > 0, mean_density], na.rm = TRUE))
      if (is.finite(max_known_density) && max_known_density > 0) {
        implausible_threshold <- max_known_density * PLAUSIBILITY_MAX_DENSITY_MULTIPLE
        ecobase_missing_fill[, plausible := Biomass_t_km2 <= implausible_threshold]
      } else {
        message("[Completeness check] No existing non-fallback Ecopath_B density found to bound the EcoBase",
                " fallback against - plausibility check skipped this run (every candidate accepted, same",
                " behavior as before this fix).")
        ecobase_missing_fill[, plausible := TRUE]
      }
      n_implausible <- ecobase_missing_fill[plausible == FALSE, .N]
      if (n_implausible > 0) {
        fwrite(ecobase_missing_fill[plausible == FALSE, .(FG_num, Biomass_t_km2, EwE_model, ecosystem_name, year, Source_citation)],
               file.path(csv_out_dir, "ecobase_fallback_implausible_REJECTED_REVIEW.csv"))
        message("\n[Completeness check] REJECTED ", n_implausible, " EcoBase fallback candidate(s) as implausible",
                " (Biomass_t_km2 more than ", PLAUSIBILITY_MAX_DENSITY_MULTIPLE, "x the highest density already",
                " seen among this model's own non-fallback FGs, ", round(max_known_density, 4), " t/km2) - left",
                " genuinely missing (0/NA) instead of silently patching Ecopath_B with an implausible value.",
                " See ecobase_fallback_implausible_REJECTED_REVIEW.csv.")
      }
      ecobase_missing_fill_accepted <- ecobase_missing_fill[plausible == TRUE]
      
      fwrite(ecobase_missing_fill_accepted[, .(FG_num, Biomass_t_km2, Biomass_t, EwE_model, ecosystem_name, year, Source_citation)],
             file.path(csv_out_dir, "fg_missing_biomass_ecobase_fallback_REVIEW.csv"))
      fg_biomass_base_year[ecobase_missing_fill_accepted, on = "FG_num",
                           mean_density := fifelse(is.na(mean_density) | mean_density == 0, i.Biomass_t_km2, mean_density)]
      ecobase_missing_fill <- ecobase_missing_fill_accepted  # so every downstream reference below (Source_citation patch, count messages) only sees ACCEPTED rows
      message("\n[Completeness check] EcoBase fallback (other published Mediterranean models, keyword-matched",
              " on each missing FG's own name/scientific name) filled ", nrow(ecobase_missing_fill), " of the ",
              nrow(fg_missing_biomass_eligible), " non-fish/non-Expanding/non-Detritus-Discards FG(s) that had",
              " no biomass from any other source - see fg_missing_biomass_ecobase_fallback_REVIEW.csv for",
              " exactly which model/year each came from. These are AUTO-DRAFT, closest-available-OTHER-MODEL",
              " figures, not measurements for this study area/year - review before trusting. This patches",
              " Ecopath_B's baseline-year value only; the full Ecosim_ts series for these FG(s) is NOT",
              " backfilled by this step.")
      
      ## 2026-09-29, per Andrea ("i would like real reference to be
      ## cited in the FG_ref"): this block fills mean_density but, until
      ## now, never touched biomass_source - and biomass_source was
      ## already written to survey_fg_annual_index_regional_combined.csv
      ## (this script's fwrite() near line ~3272, long before this
      ## fallback runs), so build_fg_references_sheet()'s B_ref kept
      ## showing the stale "MISSING - ..." tag for exactly the FG(s)
      ## this step just filled, even though the real citation
      ## (EwE_model/ecosystem_name/year/Source_citation) was sitting
      ## right here in ecobase_missing_fill. Patch that CSV in place.
      ecobase_missing_fill[, fallback_citation := paste0(
        "EcoBase fallback (other Mediterranean model, closest keyword match - not a measurement for this study area): ",
        EwE_model, " (", ecosystem_name, ", ", year, ")",
        fifelse(!is.na(Source_citation) & Source_citation != "", paste0(" - ", Source_citation), "")
      )]
      fg_idx_combined_path <- file.path(csv_out_dir, "survey_fg_annual_index_regional_combined.csv")
      if (file.exists(fg_idx_combined_path)) {
        fg_idx_on_disk <- fread(fg_idx_combined_path)
        fg_idx_on_disk <- merge(fg_idx_on_disk, unique(ecobase_missing_fill[, .(FG_num, fallback_citation)]),
                                by = "FG_num", all.x = TRUE)
        n_patched <- fg_idx_on_disk[!is.na(fallback_citation), uniqueN(FG_num)]
        fg_idx_on_disk[!is.na(fallback_citation), biomass_source := fallback_citation]
        fg_idx_on_disk[, fallback_citation := NULL]
        fwrite(fg_idx_on_disk, fg_idx_combined_path)
        message("Patched biomass_source in ", fg_idx_combined_path, " for ", n_patched, " FG(s) filled by the",
                " EcoBase last-resort fallback above, so FG_References' B_ref carries the real",
                " model/ecosystem/year citation instead of the stale 'MISSING' tag.")
      } else {
        message("WARNING: ", fg_idx_combined_path, " not found - can't patch biomass_source for the",
                " EcoBase-filled FG(s) above; FG_References' B_ref will still show 'MISSING' for them.")
      }
      
      fg_missing_biomass <- merge(full_fg_list, fg_biomass_base_year, by = c("FG_num", "FG_name"), all.x = TRUE)
      fg_missing_biomass <- fg_missing_biomass[is.na(mean_density) | mean_density == 0]
    } else {
      message("\n[Completeness check] EcoBase fallback found no matching group for any of the ",
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