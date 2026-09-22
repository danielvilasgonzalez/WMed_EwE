## =================================================================
## run_pipeline_demo.R
##
## Demo/driver script for the WMed EwE pipeline. Shows how to run
## 01_biomass.R -> 02_fisheries.R -> 03_pbqb-traits.R -> 04_diets.R
## (in that order - 02, 03 and 04 all require files that 01 produces;
## 04 specifically needs 01's biomass_proportion_by_species_fg.csv for
## its species-biomass-share-within-FG values) as SOURCED
## steps, with region/year/other options set BEFORE each source() call
## rather than hand-edited inside the scripts themselves.
##
## 03b_ecobase.R (EcoBase literature PB/QB) is NOT a separate step here -
## 03_pbqb-traits.R sources it and calls fetch_ecobase_literature_pb_qb()
## directly, partway through its own run. ENABLE_ECOBASE_QUERY/
## ECOBASE_FORCE_REFRESH below control that call from this driver script,
## same "set before source()" pattern as every other knob.
##
## 04_diets.R is optional (diet composition, not every run needs it)
## but auto-runs to completion as soon as it's sourced, same as 01/02/03
## - there's no separate function call to remember afterward.
##
## How this works: 01/02/03/04 were each updated to only assign their
## config variables (paths, FILTER_AREAS/TARGET_COUNTRIES, YEAR_ECOPATH,
## START_YEAR/END_YEAR, etc.) when that variable ISN'T ALREADY SET in
## the calling environment - see the "SOURCE-ABLE SCRIPT" comment block
## near the top of each script's Configuration section. So:
##   - set nothing  -> every script falls back to its own original
##                      Western Med (GSA 1-11) default, unchanged.
##   - set a variable here, in this script's global environment,
##     BEFORE calling source() -> that script picks up your value
##     instead of its default.
## This is purely additive - nothing about running 01/02/03 the old
## way (opening one in RStudio and hitting Source) changed.
##
## Two example runs are below: (A) the West Med default, unmodified,
## and (B) a custom region/year example. Only ONE of the two `RUN_MODE`
## branches actually executes in a given run of this script - edit
## RUN_MODE, or copy this file and adapt the "custom" block for your
## own region.
## =================================================================

RUN_MODE <- "westmed_default"   # "westmed_default" | "custom_example"

## -----------------------------------------------------------------
## Shared paths - set ONCE here, reused by all three scripts. Replace
## these three with your own out_dir/pcloud_dir/git_dir (or delete
## this block entirely and let each script fall back to its own
## hardcoded-user-or-interactive-prompt resolution, exactly as before).
## -----------------------------------------------------------------
out_dir    <- "/Users/daniel/Work/iMARES/WMed EwE Model/output/"
pcloud_dir <- "/Users/daniel/pCloud Drive/EwE Western Med 2026/"
git_dir    <- "/Users/daniel/Documents/GitHub/WMed_EwE/"

if (RUN_MODE == "westmed_default") {
  
  ## -----------------------------------------------------------------
  ## (A) WEST MED DEFAULT - do not set FILTER_AREAS/TARGET_COUNTRIES/
  ## START_YEAR/END_YEAR/YEAR_ECOPATH/TS_YEARS at all here. Each script
  ## falls through to its own built-in Western Med default (GSA 1-11,
  ## 1994-2023, Spain/France/Italy/Tunisia/Algeria/Morocco) exactly as
  ## it did before this change - nothing below is new behavior, it's
  ## just being triggered by source() from this driver instead of by
  ## opening each script directly.
  ##
  ## EcoBase query left at its own default too (ENABLE_ECOBASE_QUERY =
  ## TRUE, ECOBASE_FORCE_REFRESH = FALSE inside 03_pbqb-traits.R) - set
  ## either one here before source()-ing 03_pbqb-traits.R if this run
  ## should skip the network call (ENABLE_ECOBASE_QUERY <- FALSE) or
  ## force a fresh EcoBase fetch instead of reusing a cached CSV
  ## (ECOBASE_FORCE_REFRESH <- TRUE).
  ## -----------------------------------------------------------------
  message("\n=== Running WEST MED DEFAULT pipeline ===\n")
  
} else if (RUN_MODE == "custom_example") {
  
  ## -----------------------------------------------------------------
  ## (B) CUSTOM REGION/YEAR EXAMPLE - Adriatic GSAs (17-18), a shorter
  ## time series, and a different YEAR_ECOPATH snapshot. Every one of
  ## these is picked up by the matching `if (!exists(...))` guard in
  ## 01/02/03 instead of that script's own default. Add more knobs
  ## here the same way (see each script's own "SOURCE-ABLE SCRIPT"
  ## comment block for the full list it recognizes: OUTLIER_METHOD,
  ## DROP_OUTLIERS, NORMALIZE_TS, DATASET_VERSION, FISHERIES_DATA_SOURCE,
  ## PB_QB_SELECTION_MODE, DEFAULT_TEMP, etc.)
  ## -----------------------------------------------------------------
  FILTER_AREAS     <- 17:18                              # Adriatic GSAs instead of West Med's 1:11
  TARGET_COUNTRIES <- c("Italy", "Croatia", "Slovenia")  # used by 02_fisheries.R
  YEAR_ECOPATH     <- 2005:2007                           # must stay consistent across 01/02/03 - set once here
  TS_YEARS         <- 2000:2020                           # used by 01_biomass.R
  START_YEAR       <- 2000                                # used by 02_fisheries.R
  END_YEAR         <- 2020
  ENABLE_ECOBASE_QUERY  <- TRUE   # used by 03_pbqb-traits.R (sources/calls 03b_ecobase.R); FALSE skips the network call entirely
  ECOBASE_FORCE_REFRESH <- FALSE  # TRUE re-queries EcoBase even if a cached ecobase_literature_pb_qb_simple.csv already exists for this region
  
  message("\n=== Running CUSTOM REGION pipeline (GSA ", paste(FILTER_AREAS, collapse=","),
          ", ", START_YEAR, "-", END_YEAR, ") ===\n")
  
} else {
  stop("Unknown RUN_MODE: '", RUN_MODE, "'. Use 'westmed_default' or 'custom_example'.")
}

## -----------------------------------------------------------------
## Run the four pipeline steps IN ORDER. 01 must run first - 02, 03
## and 04 all require species_density_regional_combined.csv /
## strata_area_by_area.csv / biomass_proportion_by_species_fg.csv,
## which don't exist until 01 has produced them (see each script's own
## header comment). 02 and 03 are otherwise order-independent with
## respect to each other and the shared workbook (per
## pipeline_documentation.Rmd's "Data and code locations" section). 04
## (diets) is optional but, if run, must come after 01 for the
## biomass-share-within-FG values. Each script also trims the shared
## ecopath_ecosim_inputs.xlsx workbook down to the final target sheets
## that exist so far, right after it runs - not just at the end.
##
## 03_pbqb-traits.R (Step 3) sources 03b_ecobase.R itself and calls
## fetch_ecobase_literature_pb_qb() partway through its own run - there
## is no separate "Step 3b" to source here.
## -----------------------------------------------------------------
message("--- Step 1/4: source(01_biomass.R) ---")
source(file.path(git_dir, "scripts/01_biomass.R"))

message("--- Step 2/4: source(02_fisheries.R) ---")
source(file.path(git_dir, "scripts/02_fisheries.R"))

message("--- Step 3/4: source(03_pbqb-traits.R) [includes the EcoBase literature query] ---")
source(file.path(git_dir, "scripts/03_pbqb-traits.R"))

message("--- Step 4/4: source(04_diets.R) [optional - diet composition] ---")
source(file.path(git_dir, "scripts/04_diets.R"))

message("\n=== Pipeline run complete (RUN_MODE = '", RUN_MODE, "'). ",
        "Outputs written under: ", out_dir, " ===\n")