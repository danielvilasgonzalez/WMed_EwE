## =================================================================
## Created by: Daniel Vilas
## PIPELINE STEP 4 of 4 - run LAST
## REQUIRES Step 1's species_density_regional_combined.csv. OPTIONALLY
## uses Step 2's fg_catch_timeseries CSV (FG_YIELD_SOURCE toggle, near
## species_df's Yield loading) and Step 3's ecobase_literature_pb_qb_
## simple.csv, if present - both degrade gracefully with a clear
## message if missing, they don't hard-fail this script.
## Produces: PB_QB/Ecobase sheets in output/ecopath_ecosim_inputs.xlsx.
## =================================================================

## =================================================================
## PB and QB estimation for EwE Functional Groups - FULLY AUTOMATED
## Taxon-specific methods per: "Quick guide on how to calculate P/B
## and Q/B for EwE models" - Vilas, Coll, Piroddi, Steenbeek
##
## INPUT: species_df with:
##   Species  - scientific name (REQUIRED)
##   FG       - functional group id (REQUIRED)
##   Biomass  - species biomass DENSITY, t/km^2 (REQUIRED)
##   Yield    - OPTIONAL, species catch DENSITY, t/km^2/year - MUST be
##              in the SAME area-normalized units as Biomass (both
##              per km^2) for F = Yield/Biomass to be a valid rate.
##              Mixing an absolute total catch with a density Biomass
##              would silently produce a meaningless F - if you don't
##              have a true density for Yield, leave it NA rather than
##              guess a conversion.
##
## Everything else (growth params, a/b, maturity, body weight,
## longevity, trophic level, temperature, depth, aspect ratio) is
## fetched automatically from FishBase + SeaLifeBase.
## =================================================================

## =================================================================
## Package loading - same pattern as 01_biomass.R for
## the plain CRAN packages this script always needs. patchwork (used
## later for combining plots) is included here now too, rather than
## being loaded separately mid-script. worrms is no longer needed
## here - taxonomic classification (Step 1 below) was migrated from
## WoRMS to FishBase/SeaLifeBase (via rfishbase, same as the survey
## scripts) for project-wide consistency.
##
## FOUR packages are deliberately NOT in this always-installed list,
## each for a different reason - flagged here together so it's clear
## at a glance which packages this script actually depends on and why
## none of them go through the naive install.packages()/library()
## path above (answers a 2026-09 question about "packages
## used in the pbqb... there was one tropfishR"):
##   - rfishbase   REQUIRED, but version-pinned - a naive
##                 install.packages("rfishbase") installs a broken
##                 pre-4.0 CRAN build that crashes on species(). See
##                 the explicit version check right below instead.
##   - duckdbfs    REQUIRED (rfishbase 4.0+'s own dependency), same
##                 reason - CRAN's release can lag the duckdb_config
##                 export rfishbase needs. Checked right below too.
##   - FishLife    OPTIONAL - one of six fish M-estimation methods
##                 (M_FishLife_2023). Guarded with requireNamespace()
##                 at its own usage site (fetch_fishlife(), further
##                 down) and skipped with a message if missing - the
##                 other five M methods still run fine without it.
##   - TropFishR   OPTIONAL - supplies three of six fish M-estimation
##                 methods (M_Hoenig_1983, M_Then_2015,
##                 M_AlversonCarney_1975, all via M_empirical()).
##                 Guarded with requireNamespace() at its own usage
##                 site (fetch_tropfishr_M(), further down) and
##                 skipped with a message if missing - Pauly (1980),
##                 Gascuel (2008), and FishLife (if installed) still
##                 run without it. NOT declared in `pkgs` on purpose:
##                 unlike rfishbase/duckdbfs it has no version-pinning
##                 issue, it's simply optional, so it's installed only
##                 if/when actually needed rather than on every run.
## Full citations for every M/PB/QB method that can end up in the
## output - Pauly/Hoenig/Then/Alverson&Carney/Gascuel/FishLife for M,
## Palomares&Pauly/Christensen&Pauly/Q-P-3 for fish QB, plus the
## mammal/seabird/invertebrate equivalents - are in METHOD_REFERENCES,
## defined further down and written out as its own workbook sheet
## (distinct from the "References" sheet, which documents WHERE each
## species' raw FishBase trait data came from, Locality/Year, not
## WHICH equation was used to turn those traits into M/PB/QB).
## =================================================================
pkgs <- c("data.table", "stringr", "ggplot2", "progress", "patchwork",
          "readxl", "openxlsx", "purrr")
new_pkgs <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new_pkgs) > 0) install.packages(new_pkgs)
invisible(lapply(pkgs, library, character.only = TRUE))

message("[Package check] Always-loaded: ", paste(pkgs, collapse = ", "), ". Version-pinned",
        " (checked separately below): rfishbase, duckdbfs. Optional, checked at their own",
        " usage site: FishLife (", if (requireNamespace("FishLife", quietly = TRUE)) "installed" else "NOT installed - M_FishLife_2023 will be skipped",
        "), TropFishR (", if (requireNamespace("TropFishR", quietly = TRUE)) "installed" else "NOT installed - M_Hoenig_1983/M_Then_2015/M_AlversonCarney_1975 will be skipped",
        ").")

## =================================================================
## STEP 1: Configuration
##
## SOURCE-ABLE SCRIPT: out_dir/pcloud_dir/git_dir (and the other knobs
## further down - FISHERIES_DATA_SOURCE, FAO_GFCM_DATASET_VERSION_FOR_04,
## YEAR_ECOPATH, etc.) are only set to their West Med defaults when not
## already defined in the calling environment - a driver script can
## pre-set any of them before source()-ing this script. Standalone runs
## (nothing pre-set) behave exactly as before.
## =================================================================
if (exists("out_dir", envir = .GlobalEnv, inherits = FALSE) &&
    exists("pcloud_dir", envir = .GlobalEnv, inherits = FALSE) &&
    exists("git_dir", envir = .GlobalEnv, inherits = FALSE)) {
  message("[03_pbqb-traits.R] Using pre-set out_dir/pcloud_dir/git_dir from calling environment:\n  out_dir  = ", out_dir, "\n  pcloud_dir = ", pcloud_dir, "\n  git_dir  = ", git_dir)
} else if (tolower(Sys.info()[["user"]]) == "daniel") {
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

## plot_dir nested inside out_dir, same convention as
## 01_biomass.R - everything this script produces lands
## somewhere under out_dir, nothing written to a separate location.
plot_dir <- file.path(out_dir, "plots")
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

## =================================================================
## FISHERIES_DATA_SOURCE: which of 02a_fisheries_multisource.R's four
## sources (SAU, SAU_no_unreported, FishMIP, FAO_GFCM - see that
## script's own header comment for what each one is/isn't) drives
## Yield/Fmort/PB=M+F below. Every source's own catch still appears
## in the workbook's DataSources_Catch/Catches_by_Fleet/Fishing_
## Effort_by_Fleet sheets regardless of this setting - written by
## 02a_fisheries_multisource.R, not this script - only THIS script's
## own Fmort/PB downstream follow whichever source is picked here.
##
## "FAO_GFCM" here reads the SAME file 02_fao_catches.R itself wrote
## (fg_catch_timeseries_<FAO_GFCM_DATASET_VERSION_HERE>.csv, default
## "GFCM_2025" below) - 02a_fisheries_multisource.R passes that file
## through unchanged for this source, it doesn't write its own
## FAO_GFCM-suffixed copy.
##
## Set to NULL to fall back to the ORIGINAL placeholder paths this
## script used before 02a_fisheries_multisource.R existed (a hand-
## maintained landings CSV that isn't one of 02a_fisheries_multisource.R's four sources) -
## YIELD_SOURCE/FG_YIELD_SOURCE/LANDINGS_CSV_PATH/FG_CATCH_CSV_PATH
## below can still be set directly in that case, exactly as before.
##
## Defaults to "SAU" (not NULL) - species-level resolution AND
## discards/unreported/IUU catch are treated as always-wanted for the
## Fmort/PB=M+F chain here, matching 02_fao_catches.R's own
## CATCHES_DATA_SOURCE default (also "SAU", for the same reason).
## Nothing here auto-sources this script the way 02a_fisheries_multisource.R auto-sources
## 02_fao_catches.R, so there's no recursion risk to guard against -
## just change this directly if you want a different run's Fmort
## driven by something else.
## =================================================================
if (!exists("FISHERIES_DATA_SOURCE", envir = .GlobalEnv, inherits = FALSE)) FISHERIES_DATA_SOURCE <- "SAU"   # NULL | "SAU" | "SAU_no_unreported" | "FishMIP" | "FAO_GFCM"
if (!exists("FAO_GFCM_DATASET_VERSION_FOR_04", envir = .GlobalEnv, inherits = FALSE)) FAO_GFCM_DATASET_VERSION_FOR_04 <- "GFCM_2025"   # must match 02_fao_catches.R's own DATASET_VERSION

## EcoBase literature PB/QB query (03b_ecobase.R) - sourced and called
## directly from this script (see "Supplement with EcoBase literature
## values" further down), not run as a standalone step. ENABLE_ECOBASE_QUERY
## <- FALSE skips it entirely (useful offline, or if the network egress
## doesn't reach sirs.agrocampus-ouest.fr); ECOBASE_FORCE_REFRESH <- TRUE
## re-queries EcoBase even if ecobase_literature_pb_qb_simple.csv already
## exists from a previous run (default FALSE just reuses that cache).
if (!exists("ENABLE_ECOBASE_QUERY",  envir = .GlobalEnv, inherits = FALSE)) ENABLE_ECOBASE_QUERY  <- TRUE
if (!exists("ECOBASE_FORCE_REFRESH", envir = .GlobalEnv, inherits = FALSE)) ECOBASE_FORCE_REFRESH <- FALSE
source(file.path(git_dir, "scripts/03b_ecobase.R"))   # defines fetch_ecobase_literature_pb_qb()

## =================================================================
## rfishbase/duckdbfs compatibility check - catches, at the very start,
## the exact class of bug that cost a long debugging session: an old
## rfishbase (<4.0, e.g. the "3.1.9.99" transitional build) can crash
## outright on a basic species() call, and even a modern rfishbase can
## still fail if duckdbfs is a CRAN release that predates duckdb_config
## being exported (confirmed: CRAN's 0.1.0 lacked it, GitHub's 0.1.2.99
## had it). Checking the ACTUAL exported function directly, not just a
## version number string, since that's the more reliable test - a
## version comparison can be fooled by how different CRAN/GitHub builds
## number themselves.
## =================================================================

rfishbase_version <- tryCatch(packageVersion("rfishbase"), error = function(e) NULL)
if (is.null(rfishbase_version) || rfishbase_version < "4.0.0") {
  stop(
    "rfishbase is missing or too old (found: ", if (is.null(rfishbase_version)) "not installed" else as.character(rfishbase_version), ").\n",
    "Versions before 4.0.0 use an outdated architecture known to crash on basic\n",
    "calls like species(). Fix with:\n\n",
    "  install.packages('remotes')\n",
    "  remotes::install_github('ropensci/rfishbase')\n\n",
    "Then restart R completely (quit and reopen, not just clear the workspace)\n",
    "and re-run this script."
  )
}

duckdbfs_ok <- requireNamespace("duckdbfs", quietly = TRUE) &&
  exists("duckdb_config", where = asNamespace("duckdbfs"))
if (!duckdbfs_ok) {
  stop(
    "duckdbfs is missing 'duckdb_config', which rfishbase 4.0+ requires.\n",
    "This happens when duckdbfs was installed from CRAN, which can lag behind\n",
    "the version rfishbase actually needs (confirmed: CRAN 0.1.0 lacks this\n",
    "export, GitHub's dev build has it). Fix with:\n\n",
    "  remotes::install_github('cboettig/duckdbfs')\n\n",
    "Then restart R completely (quit and reopen, not just clear the workspace)\n",
    "and re-run this script."
  )
}

message("Version check passed - rfishbase ", as.character(rfishbase_version),
        ", duckdbfs ", as.character(packageVersion("duckdbfs")), " (duckdb_config available).")

## --- Load species_df (built and saved by test_species_df.R, or your
## own real data saved the same way) --------------------------------
PIPELINE_START_TIME <- Sys.time()

## Top-level stage tracker - covers the WHOLE pipeline, not just the
## per-species loops (which have their own finer-grained progress bars
## already). Call STAGE_PB$tick(tokens=list(stage_name="...")) once
## each labeled stage below completes.
## 2026-09-24 fix: this list must have EXACTLY one entry per
## STAGE_PB$tick() call below, in the same order, or the progress bar's
## total is wrong. "Fetch 2a2: occurrence status" (the rfishbase::
## country()-based Occurrence_status fetch, ticked at line ~1675) was
## added without adding its stage name here, so the bar's total stayed
## at 13 while 14 ticks actually fire - the 14th tick (at "Export CSVs
## + generate plots") then hits progress::progress_bar's own guard
## against ticking past 100% and crashes with "!self$finished is not
## TRUE". If you add another STAGE_PB$tick() call anywhere, add its
## stage name here too, in call order.
PIPELINE_STAGES <- c(
  "Load species_df", "Taxonomic classification (WoRMS)",
  "Fetch 2a: species() traits", "Fetch 2a2: occurrence status",
  "Fetch 2b: growth params",
  "Fetch 2c: length-weight a/b", "Fetch 2d: maturity",
  "Fetch 2e: ecology/trophic level", "Fetch 2f: swimming/aspect ratio",
  "Assemble + derive traits (Froese-Binohlan)", "Raw-trait gap filling",
  "Calculate PB/QB (all groups)", "Aggregate to FG level",
  "Export CSVs + generate plots"
)

## Set to FALSE to force the plain console bar instead of a GUI window
## (e.g. if running on a headless server, or if Tcl/Tk isn't available -
## on macOS this sometimes needs XQuartz installed for the Tk graphics
## backend to work). Falls back to console automatically either way if
## the window can't actually be opened, rather than erroring out.
USE_GUI_PROGRESS <- TRUE

make_stage_tracker <- function(stages, use_gui = TRUE) {
  n <- length(stages)
  current <- 0
  start_time <- Sys.time()
  
  gui_ok <- FALSE
  tk_pb <- NULL
  if (use_gui && requireNamespace("tcltk", quietly = TRUE)) {
    tk_pb <- tryCatch(
      tcltk::tkProgressBar(title = "Pipeline Progress", label = "Starting...",
                           min = 0, max = n, width = 420),
      error = function(e) {
        message("Could not open a GUI progress window (", conditionMessage(e),
                ") - falling back to the console bar. On macOS this often",
                " means Tcl/Tk needs XQuartz installed.")
        NULL
      }
    )
    gui_ok <- !is.null(tk_pb)
  }
  
  console_pb <- if (!gui_ok) {
    progress_bar$new(
      format = "PIPELINE [:bar] :percent | Stage :current/:total: :stage_name | Elapsed: :elapsedfull",
      total = n, clear = FALSE, width = 100
    )
  } else NULL
  
  list(
    tick = function(tokens = list()) {
      current <<- current + 1
      stage_name <- if (!is.null(tokens$stage_name)) tokens$stage_name else ""
      if (gui_ok) {
        elapsed_s <- round(as.numeric(Sys.time() - start_time, units = "secs"))
        pct <- round(100 * current / n)
        tcltk::setTkProgressBar(tk_pb, value = current,
                                title = paste0("Pipeline Progress - ", pct, "%"),
                                label = paste0("Stage ", current, "/", n, ": ", stage_name, " | Elapsed: ", elapsed_s, "s"))
        if (current >= n) close(tk_pb)
        invisible(NULL)
      } else {
        invisible(console_pb$tick(tokens = tokens))
      }
    }
  )
}

STAGE_PB <- make_stage_tracker(PIPELINE_STAGES, use_gui = USE_GUI_PROGRESS)
message(strrep("=", 70))
message("Starting pipeline - ", length(PIPELINE_STAGES), " stages",
        if (USE_GUI_PROGRESS) " (GUI progress window, if available)" else " (console progress bar)")
message(strrep("=", 70))

## SPECIES_DF_SOURCE controls where species_df comes from:
##  "survey" - the real MEDITS+MEDIAS combined species density from
##             01_biomass.R (species_density_regional_combined.csv),
##             reshaped into species_df inline, right below. This is the
##             real data source - use this for actual model runs.
##  "test"   - the old test_species_df.rds placeholder. Kept only for
##             quick pipeline smoke-testing when survey outputs aren't
##             available/up to date.
if (!exists("SPECIES_DF_SOURCE", envir = .GlobalEnv, inherits = FALSE)) SPECIES_DF_SOURCE <- "survey"

## SURVEY_OUT_DIR removed - it duplicated out_dir from STEP 1 above
## (both pointed at the same ".../WMed EwE Model/output/" folder).
## out_dir is used directly everywhere below instead.
## 2026-09-17 update: this block's own native/intermediate CSV outputs
## go into their own "pbqb-traits" subfolder (matching 01_biomass.R's
## "biomass" and 02_fisheries.R's "fisheries" subfolders); the shared
## workbook stays at the top-level out_dir. BIOMASS_CSV_DIR/
## FISHERIES_CSV_DIR point at the other two blocks' subfolders for this
## script's cross-block reads.
csv_out_dir <- file.path(out_dir, "pbqb-traits")
if (!dir.exists(csv_out_dir)) dir.create(csv_out_dir, recursive = TRUE)
BIOMASS_CSV_DIR   <- file.path(out_dir, "biomass")
FISHERIES_CSV_DIR <- file.path(out_dir, "fisheries")

SURVEY_DENSITY_CSV <- file.path(BIOMASS_CSV_DIR, "species_density_regional_combined.csv")

## Same shared workbook 01_biomass.R and 02_fao_catches.R
## write to (order-independent - this can run before, after, or
## between those two). add_pbqb_to_ecopath_workbook() is defined in
## lib_survey_fg_density_functions.R, sourced here since this script
## doesn't otherwise need it.
source(file.path(git_dir, "scripts/lib_survey_fg_density_functions.R"))
ECOPATH_WORKBOOK_PATH <- file.path(out_dir, "ecopath_ecosim_inputs.xlsx")
## Must match YEAR_ECOPATH in 01_biomass.R exactly - this is
## what defines the Biomass snapshot species_df's density values (and
## therefore fg_weighted's Biomass_FG below, and ecopath_ready's
## "Biomass in habitat area (t/km^2)") are drawn from. If that script's
## YEAR_ECOPATH ever changes, update this to match or the PB/QB weights
## and the Ecopath Biomass column will be describing two different
## time snapshots without any warning. Guarded the same way as every
## other source-able knob - a driver script that sets YEAR_ECOPATH
## once, before source()-ing 01/02/03 in order, keeps all three scripts
## in sync automatically.
if (!exists("YEAR_ECOPATH", envir = .GlobalEnv, inherits = FALSE)) YEAR_ECOPATH <- 1994:1996

if (!exists("FG_REFERENCE_CSV_PATH", envir = .GlobalEnv, inherits = FALSE)) FG_REFERENCE_CSV_PATH <- file.path(pcloud_dir, "data/FG_WMed_2026.csv")   # same file 01_biomass.R's fg_species_file / 02_fisheries.R's fg_file / 04_diets.R's FG_REFERENCE_CSV_PATH all read
FG_INDEX_COMBINED_CSV  <- file.path(BIOMASS_CSV_DIR, "survey_fg_annual_index_regional_combined.csv")   # 01_biomass.R's FG-level table, now (2026-09-24 fix) includes stock-assessment/megafauna-only FGs the survey never samples at all, not just survey-caught ones
BIOMASS_PROPORTION_CSV <- file.path(BIOMASS_CSV_DIR, "biomass_proportion_by_species_fg.csv")   # same file 04_diets.R reads for the identical "species' share of its FG's biomass" purpose

if (SPECIES_DF_SOURCE == "survey") {
  if (!file.exists(FG_REFERENCE_CSV_PATH)) {
    stop("SPECIES_DF_SOURCE = 'survey' but FG_REFERENCE_CSV_PATH ('", FG_REFERENCE_CSV_PATH, "') not found.",
         " 2026-09-24, per Andrea: species_df's species UNIVERSE is now FG_WMed_2026.csv itself - every",
         " species any FG actually contains - not just whichever species the MEDITS/MEDIAS surveys",
         " happened to observe. Without this file there is no species list to build species_df from at all.")
  }
  
  ## --- Species universe: FG_WMed_2026.csv, NOT the survey -------------
  ## 2026-09-24 fix, per Andrea ("species_df shouldnt be never
  ## referenced, should be the FG_WMed_2026.csv"): species_df used to be
  ## built ENTIRELY from species_density_regional_combined.csv (01_biomass.R's
  ## MEDITS+MEDIAS combined SURVEY table), which silently limited its
  ## species list to whatever those two surveys actually caught. Bluefin
  ## tuna, swordfish (highly migratory - not trawl/acoustic-caught) and
  ## every cetacean/seabird/turtle megafauna species were therefore
  ## structurally absent from species_df, so calc_fish()/calc_mammal()/
  ## calc_seabird() never ran for them at all - not a formula problem,
  ## those functions are fully able to compute PB/QB once handed a
  ## Biomass value, they just never got the chance. Flagged by Andrea
  ## noticing these groups had no PB/QB anywhere in the output.
  ##
  ## Now the species list is read directly from FG_REFERENCE_CSV_PATH -
  ## every species any FG contains - and Biomass is filled per species
  ## from whichever source actually has it, in priority order:
  ##   1. species_density_regional_combined.csv (real MEDITS+MEDIAS
  ##      species-level density, YEAR_ECOPATH average) - used wherever
  ##      the survey genuinely observed that species.
  ##   2. Otherwise, that species' FG's own FG-level density from
  ##      survey_fg_annual_index_regional_combined.csv - which, after
  ##      01_biomass.R's matching 2026-09-24 fix, now actually has a row
  ##      for stock-assessment/megafauna-only FGs the survey never
  ##      samples at all, instead of silently having none - split across
  ##      the FG's member species by biomass_proportion_by_species_fg.csv's
  ##      prop_sp_fg where available, else an equal split among whichever
  ##      of that FG's species also need this same fallback.
  fg_ref_raw <- fread(FG_REFERENCE_CSV_PATH)
  species_col <- intersect(c("species", "ESPECIE"), names(fg_ref_raw))[1]
  fgnum_col   <- intersect(c("FG_num", "FG_number", "GF"), names(fg_ref_raw))[1]
  fgname_col  <- intersect("FG_name", names(fg_ref_raw))[1]
  if (is.na(species_col) || is.na(fgnum_col) || is.na(fgname_col)) {
    stop("FG_REFERENCE_CSV_PATH ('", FG_REFERENCE_CSV_PATH, "') is missing an expected column - looked for a",
         " species column (species/ESPECIE), an FG-number column (FG_num/FG_number/GF), and FG_name; found: ",
         paste(names(fg_ref_raw), collapse = ", "), ".")
  }
  fg_species_universe <- unique(fg_ref_raw[, .(Species = get(species_col), FG = as.integer(get(fgnum_col)), FG_name = get(fgname_col))])
  fg_species_universe <- fg_species_universe[!is.na(Species) & Species != ""]
  message("Species universe: ", nrow(fg_species_universe), " (species, FG) row(s) from ", FG_REFERENCE_CSV_PATH,
          " (", uniqueN(fg_species_universe$FG), " distinct FG(s)) - this, not the survey, now defines which",
          " species enter species_df.")
  
  ## --- Real survey density, where it exists (same source as before,
  ## just no longer what DEFINES the species list - merged onto the
  ## universe above by Species alone, trusting FG_REFERENCE_CSV_PATH's
  ## own FG assignment rather than the survey table's) ------------------
  survey_biomass <- data.table(Species = character(), Biomass_survey = numeric())
  if (file.exists(SURVEY_DENSITY_CSV)) {
    sp_density <- fread(SURVEY_DENSITY_CSV)
    required_cols <- c("Year", "FG_num", "FG_name", "ScientificName", "mean_density")
    missing_cols <- setdiff(required_cols, names(sp_density))
    if (length(missing_cols) > 0) {
      stop("species_density_regional_combined.csv is missing expected column(s): ", paste(missing_cols, collapse = ", "),
           " - check it wasn't regenerated with a different schema.")
    }
    in_range <- sp_density[Year %in% YEAR_ECOPATH]
    message("Loaded ", nrow(sp_density), " species/FG/year rows from ", SURVEY_DENSITY_CSV, "; ", nrow(in_range),
            " within YEAR_ECOPATH (", paste(range(YEAR_ECOPATH), collapse = "-"), ").")
    
    dup_fg <- unique(in_range[, .(ScientificName, FG_num)])[, .N, by = ScientificName][N > 1, ScientificName]
    if (length(dup_fg) > 0) {
      message("NOTE: ", length(dup_fg), " species have more than one FG_num within species_density_regional_combined.csv's",
              " own YEAR_ECOPATH rows - their survey density is still averaged in below, but FG_REFERENCE_CSV_PATH's",
              " own FG assignment (not the survey's) is what's actually used for these species: ", paste(dup_fg, collapse = ", "))
    }
    survey_biomass <- in_range[, .(Biomass_survey = mean(mean_density, na.rm = TRUE)), by = .(Species = ScientificName)]
    message("Real survey-observed density available for ", nrow(survey_biomass), " species (YEAR_ECOPATH average).")
  } else {
    message(SURVEY_DENSITY_CSV, " not found - every species in the FG universe will fall back to its FG's own",
            " density (see below). Run 01_biomass.R first for real survey-observed species-level values.")
  }
  
  ## --- FG-level density fallback, for species the survey never observed
  ## (now includes stock-assessment/megafauna-only FGs, per 01_biomass.R's
  ## matching 2026-09-24 fix - previously this table had no row at all
  ## for those FGs) --------------------------------------------------------
  fg_biomass <- data.table(FG = integer(), Biomass_fg = numeric())
  if (file.exists(FG_INDEX_COMBINED_CSV)) {
    fg_idx <- fread(FG_INDEX_COMBINED_CSV)
    fg_biomass <- fg_idx[Year %in% YEAR_ECOPATH, .(Biomass_fg = mean(mean_density, na.rm = TRUE)), by = .(FG = FG_num)]
    message("Loaded FG-level density (fallback for species with no direct survey observation) for ", nrow(fg_biomass),
            " FG(s), YEAR_ECOPATH average, from ", FG_INDEX_COMBINED_CSV, ".")
  } else {
    message(FG_INDEX_COMBINED_CSV, " not found - species with no direct survey observation will have NA Biomass",
            " (no FG-level density to fall back to either). Run 01_biomass.R first.")
  }
  
  ## --- Per-species share of its FG's biomass, where known - same file
  ## 04_diets.R already reads for the identical purpose. Falls back to an
  ## equal split among whichever OTHER species in that FG also need this
  ## same fallback (not among every species in the FG - a species with
  ## real survey density already has its own Biomass_survey and isn't
  ## touched by this split at all).
  species_share <- data.table(Species = character(), prop_sp_fg = numeric())
  if (file.exists(BIOMASS_PROPORTION_CSV)) {
    bp <- fread(BIOMASS_PROPORTION_CSV)
    if (all(c("Species", "prop_sp_fg") %in% names(bp))) {
      species_share <- unique(bp[, .(Species, prop_sp_fg)])
    }
  }
  
  species_df <- merge(fg_species_universe, survey_biomass, by = "Species", all.x = TRUE)
  species_df <- merge(species_df, fg_biomass, by = "FG", all.x = TRUE)
  species_df <- merge(species_df, species_share, by = "Species", all.x = TRUE)
  
  needs_fallback <- is.na(species_df$Biomass_survey)
  species_df[needs_fallback & is.na(prop_sp_fg), n_needing_fallback_in_fg := .N, by = FG]
  species_df[needs_fallback & is.na(prop_sp_fg), prop_sp_fg := 1 / n_needing_fallback_in_fg]
  species_df[, n_needing_fallback_in_fg := NULL]
  
  species_df[, Biomass := fifelse(!is.na(Biomass_survey), Biomass_survey, Biomass_fg * prop_sp_fg)]
  species_df[, Biomass_source := fifelse(!is.na(Biomass_survey), "survey (MEDITS/MEDIAS, species-level)",
                                         fifelse(!is.na(Biomass_fg), "FG-level density split by biomass share (stock assessment/megafauna/survey FG total)", NA_character_))]
  species_df[, Yield := NA_real_]
  species_df <- species_df[, .(Species, FG, FG_name, Biomass, Biomass_source, Yield)]
  
  n_no_biomass <- species_df[is.na(Biomass), .N]
  if (n_no_biomass > 0) {
    message("WARNING: ", n_no_biomass, " species in FG_WMed_2026.csv have NO Biomass at all (no survey observation",
            " AND no FG-level density to fall back to) - excluded from PB/QB weighting downstream since there's",
            " nothing to weight by. Affected:")
    print(species_df[is.na(Biomass), .(Species, FG, FG_name)])
  }
  
  n_from_fg_fallback <- species_df[!is.na(Biomass_source) & Biomass_source %like% "FG-level", .N]
  message("\nBuilt species_df: ", nrow(species_df), " species x FG rows from FG_WMed_2026.csv (",
          nrow(species_df[!is.na(Biomass_source) & Biomass_source %like% "survey"]), " with real species-level",
          " survey density, ", n_from_fg_fallback, " filled from their FG's own density - this is what now lets",
          " bluefin tuna/swordfish/megafauna species reach calc_fish()/calc_mammal()/calc_seabird() at all,",
          " instead of never entering species_df in the first place). Yield is NA for all rows (no catch/",
          " landings data in this survey pipeline - F = Yield/Biomass will not be computable downstream unless",
          " you fill this in separately from landings data, in matching t/km^2/year units).")
  
  survey_species_df_rds_path <- file.path(out_dir, "survey_species_df.rds")
  saveRDS(species_df, survey_species_df_rds_path)
  message("Saved species_df to ", survey_species_df_rds_path)
  species_df <- species_df[]
} else {
  SPECIES_DF_PATH <- file.path(out_dir, "test_species_df.rds")
  message("SPECIES_DF_SOURCE = 'test' - using placeholder test data, NOT the real",
          " survey pipeline output. Set SPECIES_DF_SOURCE <- 'survey' for real runs.")
  species_df <- readRDS(SPECIES_DF_PATH)
}

setDT(species_df)
stopifnot(all(c("Species", "FG", "Biomass") %in% names(species_df)))
if (!"Yield" %in% names(species_df)) species_df[, Yield := NA_real_]
sp_list <- unique(species_df$Species)
invisible(STAGE_PB$tick(tokens = list(stage_name = "Load species_df")))

## =================================================================
## Fishing mortality (F = Yield/Biomass) - TWO possible attachment
## points, kept separate because the data actually available doesn't
## support both the same way:
##
##  (A) SPECIES-level, via attach_yield_from_landings() below - stays
##      "none" for now. This needs a source with genuine species x
##      GSA x year granularity, which nothing currently available has.
##      Kept here for later (e.g. if STECF_FDI turns out to report at
##      that resolution once its format is confirmed).
##
##  (B) FG-level, further down (after fg_weighted is built) - this IS
##      wired up and working, reading 02_fao_catches.R's real output
##      directly (fg_catch_timeseries_<DATASET_VERSION>.csv - the one
##      real, working catch source right now is FAO_GFCM). FAO/GFCM
##      catch data only resolves reliably to FG (many records are
##      NEI/genus-level aggregates, not exact species), so F is
##      applied at FG level, on top of the biomass-weighted PB_FG -
##      not folded into individual species' PB, which stays M-only
##      throughout Step "Calculate PB/QB" below regardless of which
##      path is used here.
##
## species_df$Yield being NA (path A off) does NOT mean F is unused -
## check the FG-level section after fg_weighted for the actual applied
## F, and n_species_with_F / F_FG in the exported CSVs either way.
## =================================================================
## Driven by FISHERIES_DATA_SOURCE (set near out_dir/pcloud_dir/git_dir
## above) whenever it names a source with real species-level
## resolution (SAU/SAU_no_unreported - the only two 02a_fisheries_
## multisource.R writes a landings_by_species_gsa_year_<SOURCE>.csv
## for; FishMIP/FAO_GFCM have no species-level resolution there, so
## FISHERIES_DATA_SOURCE set to either of those leaves YIELD_SOURCE at
## "none" here - species-level Yield stays NA, only the FG-level path
## below (FG_YIELD_SOURCE) applies for them). FISHERIES_DATA_SOURCE
## NULL restores the original hardcoded "none"/placeholder-path
## behavior from before 02a_fisheries_multisource.R existed.
YIELD_SOURCE <- if (!is.null(FISHERIES_DATA_SOURCE) && FISHERIES_DATA_SOURCE %in% c("SAU", "SAU_no_unreported")) {
  "landings_csv"
} else {
  "none"
}

## PLACEHOLDER path AND placeholder column names inside
## attach_yield_from_landings() below, used only when FISHERIES_DATA_SOURCE
## is NULL - the actual landings/catch data source for this project
## (STECF/GFCM data-call catch tables, national logbook data, FAO
## capture statistics, etc.) hasn't been identified yet in that case.
## Point this at the real file once you have one, and confirm the
## required_cols list inside attach_yield_from_landings() matches its
## actual column names before trusting the output.
## Raw/reference data - lives under pcloud_dir, same convention as
## 01_biomass.R's own fg_file/tm_list_file.
LANDINGS_CSV_PATH <- if (!is.null(FISHERIES_DATA_SOURCE) && FISHERIES_DATA_SOURCE %in% c("SAU", "SAU_no_unreported")) {
  file.path(csv_out_dir, paste0("landings_by_species_gsa_year_", FISHERIES_DATA_SOURCE, ".csv"))
} else {
  file.path(pcloud_dir, "data/landings_by_species_gsa_year.csv")
}

## area_lookup_csv_path expects strata_area_by_area.csv - written by
## 01_biomass.R's compute_strata_area_by_area() cache
## (Step 6 there) - reused here rather than re-deriving GSA areas, so
## landings density (t/km^2/year) is computed against the SAME area
## figures the survey Biomass density already uses. Without this, a
## landings total in tonnes has no way to become a density comparable
## to species_df$Biomass.
attach_yield_from_landings <- function(species_df, landings_csv_path, area_lookup_csv_path, year_range) {
  if (!file.exists(landings_csv_path)) {
    message("YIELD_SOURCE = 'landings_csv' but no file found at '", landings_csv_path,
            "' - species_df$Yield stays NA for every species. Fishing mortality (F)",
            " will NOT be computed anywhere below - PB will be NATURAL MORTALITY (M)",
            " ONLY for the whole pipeline, which underestimates PB for any",
            " commercially exploited species/FG. Point LANDINGS_CSV_PATH at your",
            " real landings file once you have one.")
    return(species_df)
  }
  if (!file.exists(area_lookup_csv_path)) {
    message("Landings file found but area_lookup_csv_path is missing at '", area_lookup_csv_path,
            "' - this should be strata_area_by_area.csv, written by 01_biomass.R's Step 6",
            " (NOT written if 01_biomass.R was run with STOP_AFTER_SHINY_CACHE <- TRUE - see that",
            " script's own comment on that flag). Needed to convert landings totals (t) into a",
            " density (t/km^2/year) comparable to species_df$Biomass, so species_df$Yield stays NA",
            " for every species instead - fishing mortality (F) will NOT be computed from landings",
            " anywhere below, and PB will be NATURAL MORTALITY (M) ONLY via this path (the FG-level",
            " fg_catch_csv fallback further below may still supply Fmort_FG independently). Run",
            " 01_biomass.R first (in full, not the Shiny-cache shortcut), or point this at wherever",
            " it actually saved that file, to get real Yield-based F.")
    return(species_df)
  }
  
  landings <- fread(landings_csv_path)
  ## PLACEHOLDER schema - CONFIRM against your actual landings file and
  ## edit this list (and the fread column references below) to match
  ## its real column names before trusting anything downstream of this.
  required_cols <- c("ScientificName", "Year", "AreaID", "catch_t")
  missing_cols <- setdiff(required_cols, names(landings))
  if (length(missing_cols) > 0) {
    stop("landings_csv_path is missing expected column(s): ", paste(missing_cols, collapse = ", "),
         " - this is a placeholder schema (ScientificName/Year/AreaID/catch_t),",
         " not yet confirmed against your real landings file. Update",
         " attach_yield_from_landings() to match its actual column names.")
  }
  
  area_lookup <- fread(area_lookup_csv_path)
  ## strata_area_by_area.csv is per AreaID x Stratum (depth band) -
  ## summed here to one total area per GSA, since landings aren't
  ## reported by depth stratum the way survey hauls are.
  area_by_gsa <- area_lookup[, .(area_km2 = sum(area_km2, na.rm = TRUE)), by = AreaID]
  
  landings_in_range <- landings[Year %in% year_range]
  message("Landings: ", nrow(landings_in_range), " of ", nrow(landings), " rows fall within",
          " YEAR_ECOPATH (", paste(range(year_range), collapse = "-"), ").")
  
  landings_by_area <- landings_in_range[
    , .(catch_t = sum(catch_t, na.rm = TRUE)), by = .(ScientificName, AreaID)]
  landings_by_area <- merge(landings_by_area, area_by_gsa, by = "AreaID", all.x = TRUE)
  
  no_area <- unique(landings_by_area[is.na(area_km2), AreaID])
  if (length(no_area) > 0) {
    message(length(no_area), " AreaID(s) in the landings file have no matching area in",
            " strata_area_by_area.csv - excluded from the Yield density calculation: ",
            paste(no_area, collapse = ", "))
  }
  landings_by_area <- landings_by_area[!is.na(area_km2)]
  
  ## region-wide annual Yield density per species: total catch across
  ## GSAs and years in range, divided by total area and by the number
  ## of years - mirrors how Biomass above is a mean annual density, not
  ## a multi-year sum, so Fmort = Yield/Biomass stays a genuine
  ## per-year rate rather than an accumulated multi-year ratio.
  yield_density <- landings_by_area[
    , .(Yield = sum(catch_t, na.rm = TRUE) / sum(area_km2, na.rm = TRUE) / length(year_range)),
    by = ScientificName]
  
  message("Computed Yield density for ", nrow(yield_density), " species (t/km^2/year,",
          " region-wide, averaged over ", length(year_range), " Ecopath years).")
  
  species_df <- merge(species_df, yield_density, by.x = "Species", by.y = "ScientificName",
                      all.x = TRUE, suffixes = c("", "_landings"))
  species_df[!is.na(Yield_landings), Yield := Yield_landings]
  species_df[, Yield_landings := NULL]
  
  n_with_yield <- species_df[!is.na(Yield), .N]
  n_total <- nrow(species_df)
  message(n_with_yield, " of ", n_total, " species (", round(100 * n_with_yield / n_total, 1),
          "%) now have a Yield value - fishing mortality (F = Yield/Biomass) will be",
          " computed for these below. The remaining ", n_total - n_with_yield,
          " species have no landings match and stay M-only (natural mortality),",
          " NOT a genuine zero-fishing assumption - worth checking whether that's a",
          " real gap in the landings source or a name-matching mismatch",
          " (species_df$Species vs landings$ScientificName spelling/synonymy).")
  
  species_df
}

if (YIELD_SOURCE == "landings_csv") {
  species_df <- attach_yield_from_landings(
    species_df, LANDINGS_CSV_PATH,
    file.path(BIOMASS_CSV_DIR, "strata_area_by_area.csv"),
    YEAR_ECOPATH
  )
} else {
  message("YIELD_SOURCE = 'none' - species_df$Yield stays NA for every species (species-",
          " level path). This is expected right now - see the FG-level catch loading",
          " right below for the path that's actually wired to real data",
          " (02_fao_catches.R's own output) and feeds calc_fish()'s F fallback.")
}

## =================================================================
## FG-level catch data - loaded HERE, before dispatch, so it can feed
## a per-species Fmort FALLBACK inside calc_fish() below, not just a
## post-hoc bolt-on after PB is already computed. Several fish PB
## methods (Pauly, Hoenig, Then et al., Alverson & Carney) are
## Z = M+F, and F was previously ALWAYS NA - species-level Yield above
## is NA with no species-level landings source, so every "+F(Y/B)"
## term was silently reducing to M-only for every fish species, all
## along.
##
## FAO/GFCM catch data only resolves reliably to FG (many records are
## NEI/genus-level aggregates - see 02_fao_catches.R), so F here is a
## per-FG RATE (year^-1), applied identically to every species within
## that FG - not a per-species value. That's a normal simplification,
## not a special-cased one: F, like M, is a mortality RATE, and
## several of the M methods already used here (e.g. Gascuel's, a
## function of trophic level and temperature only) also don't vary
## within a species beyond those traits either.
##
## Species-level F (from species_df$Yield above) ALWAYS takes priority
## over this FG-wide rate when both exist, for the same reason
## hierarchical catchability matching prioritizes rank specificity
## elsewhere in this pipeline - a more specific match beats a broader
## fallback.
##
## FG_YIELD_SOURCE:
##  "fg_catch_csv" - read FG_CATCH_CSV_PATH (02_fao_catches.R's output),
##                   compute a per-FG F, and store it as species_df$Fmort_FG
##  "none"         - skip; every fish species' Fmort stays NA unless a
##                   species-level Yield source is added separately above
## =================================================================
## Driven by FISHERIES_DATA_SOURCE (see near out_dir/pcloud_dir/git_dir
## above) whenever it is set - all four of 02a_fisheries_multisource.R's
## sources have FG-level resolution, so any non-NULL FISHERIES_DATA_SOURCE
## turns this on. FISHERIES_DATA_SOURCE NULL restores the original
## hardcoded "none" default from before 02a_fisheries_multisource.R
## existed.
FG_YIELD_SOURCE <- if (!is.null(FISHERIES_DATA_SOURCE)) "fg_catch_csv" else "none"

## FAO_GFCM reads 02_fao_catches.R's own output file directly (that
## script writes fg_catch_timeseries_<DATASET_VERSION>.csv, unrelated
## to 02a_fisheries_multisource.R, which just passes it through for
## this source rather than writing its own copy - see this script's
## FISHERIES_DATA_SOURCE comment above). SAU/SAU_no_unreported/FishMIP
## instead read the fg_catch_timeseries_<SOURCE>.csv 02a_fisheries_
## multisource.R itself writes. FISHERIES_DATA_SOURCE NULL falls back
## to the original hardcoded default filename ("GFCM_2025") - update
## FAO_GFCM_DATASET_VERSION_FOR_04 above (not this line) if 02_fao_
## catches.R's own DATASET_VERSION ever changes from that default.
FG_CATCH_CSV_PATH <- if (is.null(FISHERIES_DATA_SOURCE) || FISHERIES_DATA_SOURCE == "FAO_GFCM") {
  file.path(FISHERIES_CSV_DIR, paste0("fg_catch_timeseries_", FAO_GFCM_DATASET_VERSION_FOR_04, ".csv"))
} else {
  file.path(FISHERIES_CSV_DIR, paste0("fg_catch_timeseries_", FISHERIES_DATA_SOURCE, ".csv"))
}

species_df[, Fmort_FG := NA_real_]   # populated below if FG_YIELD_SOURCE == "fg_catch_csv"

## 2026-09-23 addition: 02_fisheries.R (the actual, currently-run
## fisheries script - NOT 02_fao_catches.R/02a_fisheries_multisource.R,
## which this file's FG_YIELD_SOURCE/FG_CATCH_CSV_PATH logic just below
## was written against and which aren't part of this pipeline's real
## run order per run_pipeline_demo.R) already computes F directly -
## F_by_species_fg.csv and F_by_fg.csv, written into FISHERIES_CSV_DIR
## (same folder this script already points FG_CATCH_CSV_PATH at, so no
## path fix needed there, just the wrong FILENAME). These are F itself
## (Catch_density/Biomass_density, from GFCM catch + survey biomass, at
## FG resolution - see 02_fisheries.R's own "# obtain F for species in
## Ecopath years" section), not a raw catch table this script would
## need to re-derive F from - so when present, this takes priority over
## the legacy fg_catch_csv reconstruction below and that whole block is
## skipped. This is the actual fix for Fmort/PB=M+F silently staying
## M-only: FG_CATCH_CSV_PATH was pointing at a file 02_fao_catches.R
## writes (a script that isn't part of this pipeline's run order), so
## it was never found; F_by_species_fg.csv/F_by_fg.csv are what
## 02_fisheries.R (the script actually run) produces.
f_by_species_fg_path <- file.path(FISHERIES_CSV_DIR, "F_by_species_fg.csv")
f_by_fg_path <- file.path(FISHERIES_CSV_DIR, "F_by_fg.csv")
used_fisheries_R_f <- FALSE
if (file.exists(f_by_species_fg_path)) {
  f_by_species_fg <- fread(f_by_species_fg_path)
  if (all(c("Species", "F") %in% names(f_by_species_fg))) {
    f_lookup <- unique(f_by_species_fg[!is.na(F), .(Species, F)])
    species_df[f_lookup, Fmort_FG := i.F, on = "Species"]
    used_fisheries_R_f <- TRUE
    message("[F] Read F_by_species_fg.csv from 02_fisheries.R (", f_by_species_fg_path, ") - ",
            species_df[!is.na(Fmort_FG), .N], " of ", nrow(species_df), " species in species_df now have",
            " an Fmort_FG value from it (F is the FG-uniform value 02_fisheries.R computed, repeated per",
            " species in that FG - see that script's own comment on this). Legacy fg_catch_csv",
            " reconstruction below is skipped since this is the real, current F source.")
  } else {
    message("[F] F_by_species_fg.csv found at ", f_by_species_fg_path, " but missing expected column(s)",
            " 'Species'/'F' (got: ", paste(names(f_by_species_fg), collapse = ", "), ") - falling back",
            " to F_by_fg.csv / the legacy fg_catch_csv path below.")
  }
}
if (!used_fisheries_R_f && file.exists(f_by_fg_path)) {
  f_by_fg_direct <- fread(f_by_fg_path)
  if (all(c("FG_num", "F") %in% names(f_by_fg_direct))) {
    f_fg_lookup <- unique(f_by_fg_direct[!is.na(F), .(FG_num, F)])
    species_df[f_fg_lookup, Fmort_FG := i.F, on = c(FG = "FG_num")]
    used_fisheries_R_f <- TRUE
    message("[F] Read F_by_fg.csv from 02_fisheries.R (", f_by_fg_path, ") - ",
            species_df[!is.na(Fmort_FG), .N], " of ", nrow(species_df), " species in species_df now have",
            " an Fmort_FG value from it. Legacy fg_catch_csv reconstruction below is skipped.")
  } else {
    message("[F] F_by_fg.csv found at ", f_by_fg_path, " but missing expected column(s) 'FG_num'/'F'",
            " (got: ", paste(names(f_by_fg_direct), collapse = ", "), ") - falling back to the legacy",
            " fg_catch_csv path below.")
  }
}
if (!used_fisheries_R_f) {
  message("[F] Neither F_by_species_fg.csv nor F_by_fg.csv found in ", FISHERIES_CSV_DIR,
          " - falling back to the legacy fg_catch_csv reconstruction below (FG_CATCH_CSV_PATH),",
          " which needs 02_fao_catches.R's own output and is unlikely to exist if only",
          " 02_fisheries.R has been run.")
}

if (!used_fisheries_R_f && FG_YIELD_SOURCE == "fg_catch_csv") {
  area_lookup_path <- file.path(BIOMASS_CSV_DIR, "strata_area_by_area.csv")
  
  if (!file.exists(FG_CATCH_CSV_PATH)) {
    message("FG_YIELD_SOURCE = 'fg_catch_csv' but no file found at '", FG_CATCH_CSV_PATH,
            "' - run 02_fao_catches.R first (check DATASET_VERSION there matches the",
            " filename above). Every fish species' Fmort stays NA (M-only PB) until this exists.")
  } else if (!file.exists(area_lookup_path)) {
    message("FG_YIELD_SOURCE = 'fg_catch_csv' but strata_area_by_area.csv not found at '",
            area_lookup_path, "' - needed to convert FG catch totals (t) into a density",
            " (t/km^2/yr) comparable to Biomass. Run 01_biomass.R first.",
            " Every fish species' Fmort stays NA (M-only PB).")
  } else {
    fg_catch <- fread(FG_CATCH_CSV_PATH)
    ## strata_area_by_area.csv was already built restricted to
    ## FILTER_AREAS (the Western Med GSAs actually used) by
    ## 01_biomass.R - summed here as one region-wide total,
    ## same as how Biomass is a region-wide density everywhere else.
    area_total_km2 <- fread(area_lookup_path)[, sum(area_km2, na.rm = TRUE)]
    
    ## KNOWN, UNRESOLVED MISMATCH (same root cause as 02_fao_catches.R's own
    ## flag on this): fg_catch_timeseries_*.csv is filtered by GFCM Division
    ## (37.1.1-37.1.3), which per GFCM's own GSA-to-Division table covers
    ## GSA 1-11 PLUS GSA 12 (Northern Tunisia); area_total_km2 above is
    ## summed from strata_area_by_area.csv, which only covers 01_survey_
    ## density_westmed.R's FILTER_AREAS (GSA 1-11, GSA 12 NOT included).
    ## So Yield_FG's numerator (Catch_t) includes some landings from outside
    ## the area its own denominator (area_total_km2) represents - Yield_FG,
    ## and therefore Fmort_FG and the fish PB it feeds into calc_fish(),
    ## is a SLIGHT OVERESTIMATE for FGs disproportionately caught in GSA 12/
    ## Division 37.1.3, not a fixable bug (GFCM's Division-resolution catch
    ## reporting can't isolate GSA 12's specific share to subtract it out).
    ## Retargeting this pipeline to a different region: re-derive the
    ## Division/GSA correspondence for that region and re-check whether the
    ## same kind of boundary mismatch applies there too.
    message("NOTE: fg_catch_timeseries's Division-based filter (37.1.1-37.1.3) covers GSA 1-11",
            " PLUS GSA 12, which FILTER_AREAS (area_total_km2's basis) does NOT include - Yield_FG",
            " (and the Fmort_FG/PB it feeds) is therefore a slight OVERESTIMATE relative to Biomass's",
            " area, same root cause as 02_fao_catches.R's own Catches_Ecopath/Ecosim sheets.")
    
    fg_catch_in_range <- fg_catch[Year %in% YEAR_ECOPATH]
    message("FG catch data: ", nrow(fg_catch_in_range), " of ", nrow(fg_catch), " rows fall",
            " within YEAR_ECOPATH (", paste(range(YEAR_ECOPATH), collapse = "-"), ").",
            " REMINDER: this is landings-only (see 02_fao_catches.R) - true F is",
            " underestimated wherever discards are non-trivial for a given FG.")
    
    fg_yield_density <- fg_catch_in_range[
      , .(Yield_FG = sum(Catch_t, na.rm = TRUE) / area_total_km2 / length(YEAR_ECOPATH)),
      by = FG_num]
    
    ## Biomass_FG computed directly from species_df here (fg_weighted
    ## doesn't exist yet at this point) - same region-wide sum-of-
    ## species-densities definition used everywhere Biomass_FG appears
    ## later, so this Fmort_FG is consistent with the one the later
    ## FG-level section would otherwise compute independently.
    biomass_fg <- species_df[, .(Biomass_FG = sum(Biomass, na.rm = TRUE)), by = FG]
    fg_fmort <- merge(fg_yield_density, biomass_fg, by.x = "FG_num", by.y = "FG", all.x = TRUE)
    fg_fmort[, Fmort_FG_computed := Yield_FG / Biomass_FG]
    
    species_df[fg_fmort, Fmort_FG := i.Fmort_FG_computed, on = c(FG = "FG_num")]
    
    n_fg_with_fmort <- fg_fmort[!is.na(Fmort_FG_computed), .N]
    n_species_covered <- species_df[!is.na(Fmort_FG), .N]
    message(n_fg_with_fmort, " FG(s) have a computed Fmort_FG rate, covering ",
            n_species_covered, " of ", nrow(species_df), " species in species_df.",
            " calc_fish() below uses this as a FALLBACK wherever a species doesn't",
            " have its own species-level Yield (currently: always, since no species-",
            " level landings source exists) - species-level F, if ever available,",
            " always takes priority over this FG-wide rate.")
  }
} else if (!used_fisheries_R_f) {
  message("FG_YIELD_SOURCE = 'none' - every fish species' Fmort stays NA (M-only PB)",
          " unless a species-level Yield source is added separately above.")
}


## =================================================================
## STEP 1: taxonomic classification -> dispatch group
## Primary signal: FishBase/SeaLifeBase Class (via rfishbase::load_taxa(),
## same function/cache used by the biomass survey scripts - standardized
## on FishBase/SeaLifeBase project-wide per the instruction, 2026-09,
## replacing the WoRMS-based lookup this step used previously). FishBase
## covers fish (Actinopteri/Elasmobranchii/etc.); SeaLifeBase covers
## everything else (invertebrates, mammals, birds, algae) - both are
## queried and combined by fetch_taxonomy_fishbase(), same as the survey
## scripts' species-to-FG matching.
## =================================================================

## fetch_taxonomy_fishbase() comes from lib_survey_fg_density_functions.R,
## already source()'d near the top of this script (see the
## ECOPATH_WORKBOOK_PATH block above) for add_pbqb_to_ecopath_workbook().

FISHBASE_TAXONOMY_CACHE_PATH <- file.path(out_dir, "fishbase_taxonomy_cache.rds")

taxonomy <- fetch_taxonomy_fishbase(sp_list, cache_path = FISHBASE_TAXONOMY_CACHE_PATH)
setnames(taxonomy, "ScientificName", "Species")

FISH_CLASSES <- c("Teleostei", "Elasmobranchii", "Chondrichthyes", "Actinopteri",
                  "Actinopterygii", "Myxini", "Petromyzonti", "Holocephali")
MAMMAL_CLASSES <- c("Mammalia")
BIRD_CLASSES <- c("Aves")
PHYTO_CLASSES <- c("Bacillariophyceae", "Dinophyceae", "Cyanophyceae")

## reusable so the same logic applies both to the initial lookup and
## after any synonym-resolution fills in more Class values below
classify_dispatch <- function(class_vec) {
  fifelse(class_vec %in% FISH_CLASSES, "fish", fifelse(
    class_vec %in% MAMMAL_CLASSES, "mammal", fifelse(
      class_vec %in% BIRD_CLASSES, "seabird", fifelse(
        class_vec %in% PHYTO_CLASSES, "phytoplankton", NA_character_
      ))))
}

taxonomy[, dispatch_group := classify_dispatch(Class)]

## 2026-09-24, per Andrea: a SEPARATE, finer classification for the
## traits_ewe/Ecopath_traits "Organism" column - bacteria/fungi/algae/
## plants/invertebrates/fishes/birds/mammals/reptiles/other. Kept
## entirely independent of dispatch_group above (which exists only to
## pick a PB/QB empirical formula and deliberately lumps everything
## non-fish/mammal/bird/phyto into "invertebrate", including reptiles -
## fine for that narrow purpose, wrong for a species-level trait column
## someone will read directly). Built from the SAME Class/Phylum this
## script already fetches via fetch_taxonomy_fishbase() - no extra
## network call needed.
REPTILE_CLASSES <- c("Reptilia", "Testudines")   # sea turtles
## Macroalgae Class-level taxa (red/brown/green algae) - distinct from
## PHYTO_CLASSES above, which is single-celled phytoplankton, not the
## macroalgae/"other macroalgae" FG.
ALGAE_CLASSES <- c("Phaeophyceae", "Florideophyceae", "Rhodophyceae", "Ulvophyceae",
                   "Chlorophyceae", "Bangiophyceae", "Compsopogonophyceae")
## Seagrass (Posidonia) - vascular plants; Phylum-level since FishBase/
## SeaLifeBase taxonomy for a marine angiosperm may not carry a Class.
PLANT_PHYLA <- c("Tracheophyta", "Magnoliophyta", "Streptophyta")
BACTERIA_KINGDOMS <- c("Bacteria", "Monera")
FUNGI_KINGDOMS <- c("Fungi")

## class_vec/phylum_vec/kingdom_vec: kingdom_vec is optional (NULL if
## fetch_taxonomy_fishbase()'s output doesn't carry a Kingdom column at
## all in your rfishbase version - bacteria/fungi will then just fall
## through to "Other" rather than erroring).
classify_organism <- function(class_vec, phylum_vec, kingdom_vec = NULL) {
  out <- fifelse(class_vec %in% FISH_CLASSES, "Fishes", fifelse(
    class_vec %in% MAMMAL_CLASSES, "Mammals", fifelse(
      class_vec %in% BIRD_CLASSES, "Birds", fifelse(
        class_vec %in% REPTILE_CLASSES, "Reptiles", fifelse(
          class_vec %in% PHYTO_CLASSES, "Algae", fifelse(
            class_vec %in% ALGAE_CLASSES, "Algae", fifelse(
              phylum_vec %in% PLANT_PHYLA, "Plants", NA_character_
            )))))))
  if (!is.null(kingdom_vec)) {
    out <- fifelse(is.na(out) & kingdom_vec %in% BACTERIA_KINGDOMS, "Bacteria", fifelse(
      is.na(out) & kingdom_vec %in% FUNGI_KINGDOMS, "Fungi", out))
  }
  out
}
taxonomy[, Organism := classify_organism(Class, Phylum, if ("Kingdom" %in% names(taxonomy)) Kingdom else NULL)]

## --- Fallback for species with no Class: the usual cause is that the
## name FishBase/SeaLifeBase's load_taxa() doesn't recognize as a valid
## current name (a synonym, or a genus-level placeholder like "Sepiola
## spp."). rfishbase::synonyms() maps a synonym back to its currently
## valid name, which is then re-queried directly - the FishBase-side
## equivalent of the old WoRMS valid_AphiaID resolution step.
unresolved <- taxonomy[is.na(dispatch_group), Species]
if (length(unresolved) > 0) {
  message("\n", length(unresolved), " species had no FishBase/SeaLifeBase Class from the initial lookup -",
          " attempting synonym -> valid-name resolution via rfishbase::synonyms().")
  
  ## resolve_valid_name() ONLY resolves each species' valid name via
  ## rfishbase::synonyms() - it does NOT fetch taxonomy itself. That's
  ## deliberate: the previous version called fetch_taxonomy_fishbase_
  ## uncached() (ONE valid_name at a time) inside this same per-species
  ## loop, and that function reloads rfishbase::load_taxa() - THE ENTIRE
  ## FishBase table, then THE ENTIRE SeaLifeBase table - from scratch on
  ## every single call. With up to a few hundred unresolved species, that
  ## was up to a few hundred x 2 full-table reloads, which is what was
  ## actually "taking forever" here - not the (much cheaper, genuinely
  ## per-species) synonyms() lookup. Collecting every resolved valid_name
  ## first and fetching taxonomy for ALL of them in ONE batched call
  ## below (exactly how fetch_taxonomy_fishbase() is already called for
  ## the full species list earlier in this script) turns that into 2
  ## full-table loads total, regardless of how many species need this
  ## fallback.
  ## Strip trailing "spp."/"sp." the SAME way fetch_taxonomy_fishbase()'s
  ## underlying load_taxa() call needs a real binomial to match against -
  ## without this, a genus-level placeholder like "Sepiola spp." never
  ## matches any synonym record and just falls through to the
  ## "invertebrate" default below. Querying the bare genus instead
  ## ("Sepiola") can still succeed and return real Class/Family/Order/
  ## Phylum via a species within that genus - all dispatch_group
  ## classification needs, species-level resolution isn't required for
  ## that.
  query_map <- data.table(Species = unresolved, query_term = str_trim(str_remove(unresolved, "\\s+spp?\\.?$")))
  unique_terms <- unique(query_map$query_term)
  
  ## 2026-09-23 fix: this used to call rfishbase::synonyms() ONE QUERY
  ## TERM AT A TIME via lapply() - with up to a few hundred unresolved
  ## species, that's a few hundred separate network/duckdb round trips,
  ## which IS what was "taking ages" here (the earlier fix above only
  ## batched the taxonomy fetch that follows synonym resolution, not this
  ## step itself). rfishbase::synonyms(), like its other table-based
  ## functions (species(), ecology(), morphology(), ...), accepts a
  ## VECTOR of names and returns every matching synonym row across all of
  ## them in ONE call - turning up to a few hundred round trips into 1.
  ## The synonyms table's own queried-name column is literally called
  ## "synonym" (confirmed against the real column layout returned here:
  ## synonym, Status, SpecCode, SynCode, CoL_ID, TSN, ZooBank_ID,
  ## TaxonLevel, Species) - Species holds the CURRENTLY VALID name for
  ## that record. Matching "synonym" back to query_term is what maps each
  ## result row back to the species that produced it. If the batched
  ## call errors out, or ever returns a column layout this doesn't
  ## recognize (e.g. a future rfishbase version renames these), this
  ## falls back to the original slower but known-correct one-name-at-a-
  ## time loop rather than risk a silent mismatch.
  valid_names <- data.table(Species = character(), valid_name = character())
  syn_all <- tryCatch(rfishbase::synonyms(unique_terms), error = function(e) {
    message("  rfishbase::synonyms() batched call (", length(unique_terms), " name(s) in one request) failed: ",
            conditionMessage(e), " - falling back to the slower one-name-at-a-time loop.")
    NULL
  })
  if (!is.null(syn_all) && nrow(syn_all) > 0 && all(c("synonym", "Species") %in% names(syn_all))) {
    setDT(syn_all)  # rfishbase::synonyms() returns a plain data.frame/tibble, not a data.table - the data.table `[...,by=]` syntax below silently dispatches to data.frame's own `[` (wrong result / "unused argument" error) without this
    first_valid <- syn_all[!is.na(Species), .(valid_name = Species[1]), by = synonym]
    valid_names <- merge(query_map, first_valid, by.x = "query_term", by.y = "synonym")
    valid_names <- valid_names[!is.na(valid_name) & valid_name != query_term, .(Species, valid_name)]
    message("  rfishbase::synonyms() batched call resolved ", nrow(valid_names), " of ", length(unique_terms),
            " distinct query term(s) in one request.")
  } else {
    if (!is.null(syn_all)) {
      message("  rfishbase::synonyms() batched result had an unexpected column layout (columns: ",
              paste(names(syn_all), collapse = ", "), ") - falling back to the slower one-name-at-a-time loop.")
    }
    resolve_valid_name <- function(sp, query_term) {
      valid_name <- tryCatch({
        syn <- rfishbase::synonyms(query_term)
        if (is.null(syn) || nrow(syn) == 0) NA_character_ else syn$Species[1]
      }, error = function(e) {
        message("  rfishbase::synonyms() failed for '", sp, "' (queried as '", query_term, "'): ", conditionMessage(e))
        NA_character_
      })
      if (is.na(valid_name) || valid_name == query_term) return(NULL)
      data.table(Species = sp, valid_name = valid_name)
    }
    
    ## This loop only runs at all if the single batched call above
    ## somehow failed/came back unrecognizable - a rare fallback path,
    ## but still one network round trip per species if it does fire, so
    ## it's parallelized across a FEW cores rather than run serially or
    ## across every core (this machine likely runs other things too -
    ## leaving at least half the logical cores free, capped at 4, rather
    ## than handing rfishbase's API every core available).
    n_cores_avail <- tryCatch(parallel::detectCores(logical = TRUE), error = function(e) 1L)
    n_cores <- max(1, min(4, floor(n_cores_avail / 2)))
    message("  Falling back to a one-name-at-a-time loop, parallelized across ", n_cores, " of ",
            n_cores_avail, " available core(s) (the rest left free).")
    if (n_cores > 1 && .Platform$OS.type == "unix") {
      ## mclapply forks - works on macOS/Linux, not Windows.
      valid_names_list <- parallel::mclapply(seq_len(nrow(query_map)), function(i) {
        resolve_valid_name(query_map$Species[i], query_map$query_term[i])
      }, mc.cores = n_cores)
    } else if (n_cores > 1) {
      ## Windows fallback: a small PSOCK cluster instead of forking.
      cl <- parallel::makeCluster(n_cores)
      tryCatch({
        parallel::clusterEvalQ(cl, requireNamespace("rfishbase", quietly = TRUE))
        parallel::clusterExport(cl, c("resolve_valid_name", "query_map"), envir = environment())
        valid_names_list <- parallel::parLapply(cl, seq_len(nrow(query_map)), function(i) {
          resolve_valid_name(query_map$Species[i], query_map$query_term[i])
        })
      }, finally = parallel::stopCluster(cl))
    } else {
      valid_names_list <- mapply(resolve_valid_name, query_map$Species, query_map$query_term, SIMPLIFY = FALSE)
    }
    valid_names <- rbindlist(valid_names_list, fill = TRUE)
  }
  
  resolved <- data.table()
  if (nrow(valid_names) > 0) {
    ## ONE batched taxonomy fetch for every resolved valid_name, instead
    ## of one call per species (see comment above) - this is the actual
    ## fix for the slowdown. No on-disk caching here (unlike the initial
    ## fetch_taxonomy_fishbase() call above) since this is already a
    ## small, one-off fallback batch, not the full species list.
    valid_name_taxonomy <- fetch_taxonomy_fishbase_uncached(unique(valid_names$valid_name))
    resolved <- merge(valid_names, valid_name_taxonomy, by.x = "valid_name", by.y = "ScientificName", all.x = TRUE)
    resolved <- resolved[!is.na(Class)]
    setnames(resolved, c("Genus", "Family", "Order", "Class", "Phylum"),
             c("Genus_r", "Family_r", "Order_r", "Class_r", "Phylum_r"))
    resolved[, valid_name := NULL]
  }
  
  if (nrow(resolved) > 0) {
    taxonomy <- merge(taxonomy, resolved, by = "Species", all.x = TRUE)
    for (col in c("Genus", "Family", "Order", "Class", "Phylum")) {
      resolved_col <- paste0(col, "_r")
      taxonomy[is.na(get(col)), (col) := get(resolved_col)]
      taxonomy[, (resolved_col) := NULL]
    }
    taxonomy[is.na(dispatch_group), dispatch_group := classify_dispatch(Class)]
    taxonomy[is.na(Organism), Organism := classify_organism(Class, Phylum, if ("Kingdom" %in% names(taxonomy)) Kingdom else NULL)]
  }
  
  ## whatever's still unresolved after a genuine FishBase/SeaLifeBase
  ## attempt defaults to invertebrate as a taxonomically-neutral
  ## fallback, not a fish/non-fish guess borrowed from an unrelated source
  still_unresolved <- taxonomy[is.na(dispatch_group), Species]
  taxonomy[Species %in% still_unresolved, dispatch_group := "invertebrate"]
  ## Organism gets its OWN default, separately from dispatch_group's
  ## "invertebrate" default above - whatever never resolved a Class/
  ## Phylum/Kingdom at all genuinely isn't known to be any of Andrea's
  ## categories, so "Other" is the honest default here, not a borrowed
  ## "invertebrate" guess. A species left NA for dispatch_group but with
  ## a real Class/Phylum (rare - only species with a Class outside every
  ## classify_dispatch bucket AND every classify_organism bucket) still
  ## correctly gets a non-"Other" Organism if Phylum alone identifies it
  ## (e.g. a plant Phylum with no informative Class).
  taxonomy[is.na(Organism), Organism := "Other"]
  
  message(length(unresolved) - length(still_unresolved), " resolved via FishBase/SeaLifeBase synonym lookup, ",
          length(still_unresolved), " still fully unresolved (defaulted to invertebrate):")
  if (length(still_unresolved) > 0) print(still_unresolved)
}

message("\nUnclassified/unusual Class values that fell through to a fallback",
        " (audit these - may reveal a fish/mammal class not yet in the lists above):")
print(unique(taxonomy[is.na(Class) | !Class %in% c(FISH_CLASSES, MAMMAL_CLASSES, BIRD_CLASSES, PHYTO_CLASSES),
                      .(Species, Class, dispatch_group)]))

species_df <- merge(species_df, taxonomy, by = "Species", all.x = TRUE)

message("\nDispatch group counts:")
print(species_df[, .N, by = dispatch_group])
invisible(STAGE_PB$tick(tokens = list(stage_name = "Taxonomic classification (FishBase/SeaLifeBase)")))

## 2026-09-25, per Andrea ("the pbqb code should look for growth
## parameters and other things only for fish groups. (no invertebrates)"):
## growth params (Loo/K/Winfinity/tmax, via rfishbase::popgrowth()),
## length-weight a/b (poplw()), maturity (Lm/Tmat, maturity()), and
## swimming/aspect ratio (swimming()) are all genuinely FISH life-history
## concepts - they feed calc_fish() ONLY (Pauly 1980 M, the Froese-
## Binohlan t0 estimate, FishLife/TropFishR's M methods, and the
## PalomaresPauly/ChristensenPauly QB formulas that need Winf/AspectRatio).
## calc_mammal()/calc_seabird()/calc_invertebrate() (further down) use a
## completely disjoint set of traits - TrophicLevel, MaxWeight, Longevity,
## Depth - none of which come from these four fetches. Previously all four
## were queried against `sp_list` (EVERY species in species_df, including
## mammals/seabirds/invertebrates/phytoplankton) - wasted SeaLifeBase
## queries for groups that were never going to use the result (and, for
## poplw()/maturity(), SeaLifeBase DOES carry some invertebrate rows,
## which could have been silently merged into species_df's Loo/K/Lm/Tmat
## columns for an invertebrate row even though nothing downstream ever
## reads them for that dispatch_group - confusing in the audit CSVs at
## best). Restricted to fish only, right after dispatch_group becomes
## available. `sp_list` itself is left untouched - species()/country()/
## ecology() (2a/2a2/2e below) are still fetched for every species, since
## TrophicLevel (ecology) and MaxWeight/Longevity (species()) ARE used by
## the non-fish dispatch functions too.
sp_list_fish <- unique(species_df[dispatch_group == "fish", Species])
message("Fish-only species list for growth/length-weight/maturity/swimming fetches: ",
        length(sp_list_fish), " of ", length(sp_list), " total species in species_df ",
        "(the rest are ", paste(setdiff(unique(species_df$dispatch_group), "fish"), collapse = "/"),
        " - popgrowth()/poplw()/maturity()/swimming() are no longer queried for them).")
if (length(sp_list_fish) == 0) {
  message("WARNING: sp_list_fish is EMPTY (no species classified dispatch_group == \"fish\" -",
          " check the taxonomy fetch above/fetch_taxonomy_fishbase() output). The 2b/2c/2d/2f",
          " fetches below (popgrowth/poplw/maturity/swimming) will be skipped entirely rather",
          " than calling rfishbase with a zero-length species vector, which some rfishbase",
          " functions error on rather than returning an empty result.")
}

## =================================================================
## HELPER: pick the best row per species from a multi-record FishBase/
## SeaLifeBase table (popgrowth, poplw, maturity all have this shape -
## multiple population-specific studies per species). Priority, tiered:
##   1. Western Mediterranean locality keywords, if any row matches
##   2. Mediterranean generally (broader), if no Western Med row exists
##   3. Most recent Year among whatever tier was selected
##   4. First available, if neither locality nor year exists
## =================================================================

## Keywords for the WESTERN Mediterranean specifically - country/region
## names commonly appearing in FishBase/SeaLifeBase locality fields for
## GSA 1-11 area studies. Extend this list if you notice a relevant
## study getting missed (e.g. a specific bay/coast name).
WMED_KEYWORDS <- c("western mediterranean", "spain", "spanish", "balearic",
                   "catalonia", "catalan", "gulf of lion", "golfe du lion",
                   "france", "french mediterranean", "alboran", "ligurian",
                   "tyrrhenian", "sardinia", "corsica", "algeria", "tunisia",
                   "gsa 1", "gsa 2", "gsa 5", "gsa 6", "gsa 7", "gsa 9", "gsa 10", "gsa 11")
MED_KEYWORDS <- c("mediterran")  # broader fallback tier - any Mediterranean mention

select_best_rows <- function(dt) {
  if (nrow(dt) == 0) return(dt)
  loc_col  <- intersect(c("Locality", "Country", "Loc"), names(dt))[1]
  year_col <- intersect(c("Year", "YearStart"), names(dt))[1]
  
  loc_lower <- if (!is.na(loc_col)) str_to_lower(dt[[loc_col]]) else rep(NA_character_, nrow(dt))
  dt[, .is_wmed := str_detect(loc_lower, paste(WMED_KEYWORDS, collapse = "|"))]
  dt[, .is_med  := str_detect(loc_lower, paste(MED_KEYWORDS, collapse = "|"))]
  dt[is.na(.is_wmed), .is_wmed := FALSE]
  dt[is.na(.is_med), .is_med := FALSE]
  
  pick_one <- function(sub) {
    if (any(sub$.is_wmed)) sub <- sub[.is_wmed == TRUE]          # tier 1: Western Med
    else if (any(sub$.is_med)) sub <- sub[.is_med == TRUE]        # tier 2: Mediterranean generally
    if (!is.na(year_col) && any(!is.na(sub[[year_col]]))) {
      sub <- sub[order(-get(year_col))]                            # tier 3: most recent
    }
    sub[1]
  }
  
  split(dt, by = "Species", keep.by = TRUE) |> lapply(pick_one) |> rbindlist(fill = TRUE)
}

## =================================================================
## Capture WHERE a selected best-row's parameters came from - Locality,
## Year, and a reference/author field if FishBase exposes one. This is
## metadata select_best_rows() would otherwise discard once it's picked
## the winning row, and it's needed for the species reference/
## provenance output table.
## =================================================================

extract_provenance <- function(best_dt, param_type, refno_candidates = c("RefNo", "MainRefNo", "Ref_no")) {
  if (nrow(best_dt) == 0) return(data.table(Species = character()))
  loc_col  <- intersect(c("Locality", "Country", "Loc"), names(best_dt))[1]
  year_col <- intersect(c("Year", "YearStart"), names(best_dt))[1]
  
  ## FishBase/SeaLifeBase do NOT embed a citation string directly in
  ## popgrowth()/poplw()/maturity() - per FishBase's own REFERENCES
  ## table documentation (fishbase.org/manual), every data table
  ## stores a Ref-style column pointing into a SEPARATE references()
  ## table (Author/Year/Title columns), by RDB design. CONFIRMED live:
  ## popgrowth()'s actual column is "PopGrowthRef" - none of the
  ## originally-guessed "RefNo"/"MainRefNo"/"Ref_no"/"Author"/"Ref"/
  ## "Reference"/"RefID" names exist there at all, which is why
  ## Reference came back NA for every row with no error raised.
  ##
  ## Each FishBase population-dynamics table apparently has SEVERAL
  ## Ref-style columns for different sub-measurements within the same
  ## row (popgrowth() alone has PopGrowthRef, tmaxRef, MRef,
  ## unsexedRef, DataSourceRef) - refno_candidates lets the caller
  ## specify which one is actually right for this param_type, since
  ## that can't be guessed generically from inside this function.
  refno_col <- intersect(refno_candidates, names(best_dt))[1]
  
  ## Fallback: if none of the caller's explicit candidates exist,
  ## auto-detect any *Ref-style column that ISN'T already known to be
  ## a narrow per-subfield reference (tmax/M/unsexed/temperature all
  ## have their own dedicated Ref column that would misattribute the
  ## citation if used as the general one). Not guaranteed correct -
  ## flagged explicitly either way so it's visible which path was taken.
  used_fallback <- FALSE
  if (is.na(refno_col)) {
    ref_like_cols <- grep("Ref$", names(best_dt), value = TRUE)
    ref_like_cols <- setdiff(ref_like_cols, c("tmaxRef", "MRef", "unsexedRef", "TempRef", "DataSourceRef"))
    if (length(ref_like_cols) > 0) {
      refno_col <- ref_like_cols[1]
      used_fallback <- TRUE
    }
  }
  
  citation <- rep(NA_character_, nrow(best_dt))
  if (is.na(refno_col)) {
    message("  NOTE: no usable Ref-style column found for '", param_type, "' - checked",
            " explicit candidates (", paste(refno_candidates, collapse = ", "), ") and",
            " auto-fallback found none either. All *Ref-style columns present: ",
            paste(grep("Ref$", names(best_dt), value = TRUE), collapse = ", "),
            ". Full column list: ", paste(names(best_dt), collapse = ", "),
            ". Reference stays NA for these rows.")
  } else {
    if (used_fallback) {
      message("  '", param_type, "': explicit refno_candidates not found - auto-fell back to",
              " '", refno_col, "' (other *Ref-style columns seen: ",
              paste(grep("Ref$", names(best_dt), value = TRUE), collapse = ", "),
              ") - verify this is actually the right one for this parameter type,",
              " not a narrower per-subfield reference.")
    }
    ref_nos <- suppressWarnings(as.integer(best_dt[[refno_col]]))
    ## Unconditional diagnostic - printed EVERY time refno_col is found,
    ## not just on failure, since a column that exists but is entirely
    ## NA (or has some other unexpected content) would otherwise fail
    ## SILENTLY further down (the old `if (length(idx) == 0) next` had
    ## no message attached to it at all) and look identical to success
    ## from the console output alone.
    message("  '", param_type, "': found refno_col = '", refno_col, "', ",
            sum(!is.na(ref_nos)), " of ", length(ref_nos), " values are non-NA integers.",
            " Sample raw values: ", paste(utils::head(best_dt[[refno_col]], 5), collapse = ", "))
    
    ## RefNo numbering is independent between FishBase and SeaLifeBase -
    ## the same integer can point to a DIFFERENT, unrelated reference
    ## in each, so this has to query each server's references() table
    ## separately using the .fetch_server tag from fetch_both(), not
    ## just assume everything is FishBase.
    servers <- if (".fetch_server" %in% names(best_dt)) best_dt$.fetch_server else rep("fishbase", nrow(best_dt))
    
    for (srv in unique(stats::na.omit(servers))) {
      idx <- which(servers == srv & !is.na(ref_nos))
      if (length(idx) == 0) {
        message("  '", param_type, "' (server = ", srv, "): 0 rows have a usable (non-NA)",
                " ", refno_col, " value - skipping this server, Reference stays NA for",
                " its rows.")
        next
      }
      valid_ref_nos <- unique(ref_nos[idx])
      
      ref_meta <- tryCatch(
        as.data.table(rfishbase::references(codes = valid_ref_nos, server = srv,
                                            fields = c("RefNo", "Author", "Year", "Title"))),
        error = function(e) {
          message("  rfishbase::references() lookup failed for '", param_type, "' (server = ",
                  srv, "): ", conditionMessage(e))
          NULL
        }
      )
      if (is.null(ref_meta) || nrow(ref_meta) == 0) {
        message("  rfishbase::references() returned no rows for '", param_type, "' (server = ",
                srv, ", ", length(valid_ref_nos), " RefNo(s) queried) - Reference stays NA for these.",
                " Queried codes sample: ", paste(utils::head(valid_ref_nos, 5), collapse = ", "))
        next
      }
      message("  '", param_type, "' (server = ", srv, "): rfishbase::references() returned ",
              nrow(ref_meta), " row(s) for ", length(valid_ref_nos), " queried RefNo(s).")
      
      ref_meta[, citation_str := paste0(
        fifelse(is.na(Author) | Author == "", "Unknown author", Author),
        " (", fifelse(is.na(Year) | Year == "", "n.d.", as.character(Year)), ")",
        fifelse(is.na(Title) | Title == "", "", paste0(" - ", Title))
      )]
      citation_lookup <- setNames(ref_meta$citation_str, as.character(ref_meta$RefNo))
      citation[idx] <- unname(citation_lookup[as.character(ref_nos[idx])])
    }
  }
  
  message("  '", param_type, "': ", sum(!is.na(citation)), " of ", length(citation),
          " rows ended up with a resolved Reference.")
  
  best_dt[, .(
    Species,
    parameter_type = param_type,
    Locality  = if (!is.na(loc_col)) get(loc_col) else NA_character_,
    Year      = if (!is.na(year_col)) get(year_col) else NA_real_,
    Reference = citation
  )]
}

## =================================================================
## Fetch species-INTRINSIC traits (growth, weight, longevity, trophic
## level, aspect ratio) for an arbitrary external species list - reuses
## the exact same fetch infrastructure as Step 2, just applied to
## taxonomic donor candidates instead of your modeled species list.
## =================================================================

fetch_intrinsic_traits <- function(candidates) {
  if (length(candidates) == 0) return(data.table(Species = character()))
  
  growth_c <- select_best_rows(fetch_both(rfishbase::popgrowth, candidates))
  gt <- if (nrow(growth_c) > 0) growth_c[, .(
    Species,
    Loo  = if ("Loo" %in% names(growth_c)) Loo else NA_real_,
    K    = if ("K" %in% names(growth_c)) K else NA_real_,
    Winf = if ("Winfinity" %in% names(growth_c)) Winfinity else NA_real_,
    tmax = if ("tmax" %in% names(growth_c)) tmax else NA_real_
  )] else data.table(Species = character())
  
  ecol_c <- fetch_both(rfishbase::ecology, candidates)
  tl_col_c <- if (nrow(ecol_c) > 0) intersect(c("DietTroph", "FoodTroph"), names(ecol_c))[1] else NA
  et <- if (nrow(ecol_c) > 0) unique(ecol_c[, .(
    Species, TrophicLevel = if (!is.na(tl_col_c)) get(tl_col_c) else NA_real_
  )], by = "Species") else data.table(Species = character())
  
  sp_c <- fetch_both(rfishbase::species, candidates)
  wcol <- intersect(c("Weight", "WeightMax"), names(sp_c))[1]
  lcol <- intersect(c("LongevityWild", "LongevityCaptive", "MaxAge"), names(sp_c))[1]
  st <- if (nrow(sp_c) > 0) unique(sp_c[, .(
    Species, MaxWeight = if (!is.na(wcol)) get(wcol) else NA_real_,
    Longevity = if (!is.na(lcol)) get(lcol) else NA_real_
  )], by = "Species") else data.table(Species = character())
  
  swim_c <- fetch_both(rfishbase::swimming, candidates, fishbase_only = TRUE)
  arcol <- if (nrow(swim_c) > 0) intersect(c("AspectRatio", "Aspect"), names(swim_c))[1] else NA
  swt <- if (nrow(swim_c) > 0) unique(swim_c[, .(
    Species, AspectRatio = if (!is.na(arcol)) get(arcol) else NA_real_
  )], by = "Species") else data.table(Species = character())
  
  Reduce(function(x, y) merge(x, y, by = "Species", all.x = TRUE),
         list(data.table(Species = candidates), gt, et, st, swt))
}

## Registry of formulas expressible purely from species-intrinsic
## traits (no dependency on this model's own Biomass/Yield) - these are
## the only methods that can legitimately be extended to EXTERNAL
## donor species not present in your species_df. Tumbiolo & Downing's
## invertebrate PB is deliberately excluded: it needs THIS model's own
## Biomass as an input, which an external, non-modeled species simply
## doesn't have - that one stays local-donor-only.
EXTERNAL_FORMULA_REGISTRY <- list(
  M_Pauly_1980  = function(d) 10^(-0.0066 - 0.279 * log10(d$Loo) + 0.6543 * log10(d$K) + 0.4634 * log10(DEFAULT_TEMP)),
  M_Gascuel_2008 = function(d) 2.31 * d$TrophicLevel^(-1.72) * exp(0.053 * DEFAULT_TEMP),
  PB_Gascuel_2008 = function(d) 20.19 * d$TrophicLevel^(-3.26) * exp(0.041 * DEFAULT_TEMP),
  QB_PalomaresPauly_1998noZ = function(d) {
    Tprime <- 1000 / (DEFAULT_TEMP + 273.15)
    10^(7.964 - 0.204 * log10(d$Winf) - 1.965 * Tprime + 0.083 * d$AspectRatio)
  },
  QB_ChristensenPauly_1992 = function(d) {
    Tprime <- 1000 / (DEFAULT_TEMP + 273.15)
    10^(6.37 - 1.5045 * Tprime - 0.168 * log10(d$Winf) + 0.1399)
  },
  QB_InnesTrites_1997 = function(d) { w_kg <- d$MaxWeight / 1000; (0.1 * w_kg^0.8 / w_kg) * 365 },
  QB_NilssonNilsson_1976 = function(d) (10^(-0.293 + 0.85 * log10(d$MaxWeight)) / d$MaxWeight) * 365,
  
  ## Identity mappings for RAW traits - lets the same fill mechanism
  ## backfill Winf/TrophicLevel/MaxWeight/AspectRatio themselves,
  ## before any derived formula even runs. This matters because a
  ## missing Winf currently blanks THREE different QB methods at once
  ## (eq26, eq24, and indirectly eq27 via Winf) - filling the trait
  ## once, at the root, is more effective than patching each derived
  ## result separately.
  Winf = function(d) d$Winf,
  TrophicLevel = function(d) d$TrophicLevel,
  MaxWeight = function(d) d$MaxWeight,
  AspectRatio = function(d) d$AspectRatio,
  Longevity = function(d) d$Longevity
)

## M_Hoenig_1983/M_Then_2015/M_AlversonCarney_1975 use TropFishR rather than a simple
## closed-form expression - handled separately since they need the
## fetch_tropfishr_M() call, not a formula lookup
TROPFISHR_EXTERNAL_METHODS <- c("M_Hoenig_1983", "M_Then_2015", "M_AlversonCarney_1975")

## =================================================================
## Fill gaps in a specific method column, in two stages:
##   1. Local donors within dt itself (Genus > Family > Order > Class),
##      restricted to DIRECTLY-computed values only
##   2. If NO local donor exists at any level AND this method is in
##      EXTERNAL_FORMULA_REGISTRY, query FishBase/SeaLifeBase directly
##      for OTHER species in that same taxonomic group (via
##      species_list()) and compute the same formula for them - drawing
##      on the full external taxonomic universe, not just whichever
##      species happen to already be in your own species_df
## Every filled value is tagged in a companion "_source" column so it's
## never confused with a directly-computed one.
## =================================================================

fill_by_taxonomic_proximity <- function(dt, value_col, levels = c("Genus", "Family", "Order", "Class"),
                                        allow_external = TRUE) {
  source_col <- paste0(value_col, "_source")
  if (!source_col %in% names(dt)) dt[, (source_col) := NA_character_]
  dt[!is.na(get(value_col)) & is.na(get(source_col)), (source_col) := "direct"]
  
  ## --- Stage 1: local donors within dt --------------------------------
  for (lvl in levels) {
    if (!lvl %in% names(dt)) next
    
    donors <- dt[get(source_col) == "direct", .(.donor = mean(get(value_col), na.rm = TRUE)), by = lvl]
    donors <- donors[!is.na(get(lvl)) & !is.na(.donor)]
    if (nrow(donors) == 0) next
    
    dt <- merge(dt, donors, by = lvl, all.x = TRUE)
    dt[is.na(get(value_col)) & !is.na(.donor), (source_col) := paste0("borrowed (", lvl, ")")]
    dt[is.na(get(value_col)) & !is.na(.donor), (value_col) := .donor]
    dt[, .donor := NULL]
  }
  
  ## --- Stage 2: external donors from FishBase/SeaLifeBase, only for
  ## species where NO local donor was found at any level -----------------
  still_missing <- dt[is.na(get(value_col))]
  formula_fn <- EXTERNAL_FORMULA_REGISTRY[[value_col]]
  use_tropfishr <- value_col %in% TROPFISHR_EXTERNAL_METHODS
  
  if (allow_external && nrow(still_missing) > 0 && (!is.null(formula_fn) || use_tropfishr)) {
    pb <- progress_bar$new(
      format = paste0("  External donor lookup [", value_col, "] [:bar] :percent | :current/:total | Elapsed: :elapsedfull | ETA: :eta"),
      total = nrow(still_missing), clear = FALSE, width = 90
    )
    for (i in seq_len(nrow(still_missing))) {
      invisible(pb$tick())
      row <- still_missing[i]
      for (lvl in levels) {
        if (!lvl %in% names(dt) || is.na(row[[lvl]])) next
        
        candidates <- tryCatch({
          args <- setNames(list(row[[lvl]]), lvl)
          c(do.call(rfishbase::species_list, c(args, list(server = "fishbase"))),
            do.call(rfishbase::species_list, c(args, list(server = "sealifebase"))))
        }, error = function(e) character())
        candidates <- setdiff(unique(candidates), dt$Species)
        candidates <- head(candidates, 20)  # cap to avoid excessive API calls for large taxa
        if (length(candidates) == 0) next
        
        traits <- fetch_intrinsic_traits(candidates)
        
        ext_val <- if (use_tropfishr) {
          tfr <- fetch_tropfishr_M(traits[, .(Species, Loo, K, Temp = DEFAULT_TEMP, tmax)])
          method_col <- switch(value_col, M_Hoenig_1983 = "M_Hoenig_1983", M_Then_2015 = "M_Then_2015",
                               M_AlversonCarney_1975 = "M_AlversonCarney_1975")
          if (nrow(tfr) > 0 && method_col %in% names(tfr)) mean(tfr[[method_col]], na.rm = TRUE) else NA_real_
        } else {
          vals <- tryCatch(formula_fn(traits), error = function(e) NA_real_)
          mean(vals, na.rm = TRUE)
        }
        
        if (!is.na(ext_val) && !is.nan(ext_val) && !is.infinite(ext_val)) {
          dt[Species == row$Species, (value_col) := ext_val]
          dt[Species == row$Species, (source_col) := paste0("external FishBase/SeaLifeBase donor (", lvl, ")")]
          message("  '", row$Species, "' ", value_col, " filled from ", length(candidates),
                  " external ", lvl, "='", row[[lvl]], "' species (no local donor available)")
          break
        }
      }
    }
  }
  
  dt
}

## =================================================================
## STEP 2: fetch and prioritize traits from FishBase + SeaLifeBase
## =================================================================

## rfishbase fetches its underlying data from a Hugging Face-hosted
## mirror, which occasionally returns a transient 504 Gateway Timeout -
## an upstream infrastructure hiccup, not a bug in this code. Retry
## with backoff rather than fail outright on the first timeout.
retry_fetch <- function(expr_fun, attempts = 3, wait_seconds = 5) {
  for (i in seq_len(attempts)) {
    result <- tryCatch(expr_fun(), error = function(e) {
      message("Fetch attempt ", i, "/", attempts, " failed: ", conditionMessage(e))
      NULL
    })
    if (!is.null(result)) return(result)
    if (i < attempts) {
      message("Retrying in ", wait_seconds, " seconds...")
      Sys.sleep(wait_seconds)
    }
  }
  message("All ", attempts, " attempts failed - returning empty result. This is",
          " usually transient (Hugging Face mirror timeout) - try re-running later",
          " if this keeps happening.")
  data.table()
}

## Some rfishbase sub-functions (e.g. swimming()) can hit an internal
## bug - "missing value where TRUE/FALSE needed" - triggered by a
## specific species having no matching record, not a network issue
## (confirmed by identical failure across all retry attempts). Retrying
## the same batch call won't fix a deterministic bug in someone else's
## function. Falling back to per-species calls isolates just the
## problem species instead of losing trait data for the whole batch.
fetch_per_species <- function(fun, sp_list, server) {
  results <- vector("list", length(sp_list))
  pb <- progress_bar$new(
    format = paste0("  Per-species fallback (", server, ") [:bar] :percent | :current/:total | Elapsed: :elapsedfull | ETA: :eta"),
    total = length(sp_list), clear = FALSE, width = 90
  )
  for (i in seq_along(sp_list)) {
    invisible(pb$tick())
    results[[i]] <- tryCatch(as.data.table(fun(sp_list[i], server = server)),
                             error = function(e) {
                               message("  Skipping '", sp_list[i], "' for this trait: ", conditionMessage(e))
                               data.table()
                             })
  }
  rbindlist(results, fill = TRUE)
}

fetch_both <- function(fun, sp_list, ..., fishbase_only = FALSE) {
  fb <- retry_fetch(function() as.data.table(fun(sp_list, server = "fishbase", ...)))
  if (nrow(fb) == 0 && length(sp_list) > 1) {
    message("Batch fetch (fishbase) returned nothing - falling back to per-species",
            " calls to isolate which species is causing it...")
    fb <- fetch_per_species(fun, sp_list, "fishbase")
  }
  ## .fetch_server tracks which database each row came from - needed
  ## downstream because RefNo numbering is independent between
  ## FishBase and SeaLifeBase (the same integer can point to a
  ## DIFFERENT, unrelated reference in each), so resolving a citation
  ## requires knowing which server's references() table to query.
  if (nrow(fb) > 0) fb[, .fetch_server := "fishbase"]
  
  if (fishbase_only) return(fb)
  
  slb <- retry_fetch(function() as.data.table(fun(sp_list, server = "sealifebase", ...)))
  if (nrow(slb) == 0 && length(sp_list) > 1) {
    message("Batch fetch (sealifebase) returned nothing - falling back to per-species calls...")
    slb <- fetch_per_species(fun, sp_list, "sealifebase")
  }
  if (nrow(slb) > 0) slb[, .fetch_server := "sealifebase"]
  
  rbindlist(list(fb, slb), fill = TRUE)
}

## --- 2a. General species table: max weight, max length, depth range --
sp_table <- fetch_both(rfishbase::species, sp_list)
message("\nspecies() columns available:")
print(names(sp_table))

weight_col <- intersect(c("Weight", "WeightMax"), names(sp_table))[1]
length_col <- intersect(c("Length", "LengthMax"), names(sp_table))[1]
long_col   <- intersect(c("LongevityWild", "LongevityCaptive", "MaxAge"), names(sp_table))[1]
## Vulnerability: FishBase's own Cheung et al. intrinsic vulnerability
## index (0-100) - added 2026-09-23 so the traits_ewe/Ecopath_traits
## table below can populate Vulnerability_index straight from FishBase
## instead of the retired FG_WMed.xlsx sheet.
vuln_col   <- intersect(c("Vulnerability"), names(sp_table))[1]
## CommonLength: FishBase's species() table separately carries a
## "typical/common length" field (distinct from Length/LengthMax,
## which are the MAXIMUM recorded length) - this is the field the
## traits_ewe/Ecopath_traits table's Mean_length column is populated
## from below. Not independently re-verified against a live FishBase
## pull in this environment (no network access here to check the
## exact column name rfishbase's current version returns) - if this
## candidate list doesn't match what your version of rfishbase
## actually returns, Mean_length will just stay NA and
## common_length_col will print as "NONE FOUND" in the coverage
## message a few hundred lines down, rather than error - check that
## message on your first run and add the real column name here if
## it's missing from this list.
common_length_col <- intersect(c("CommonLength", "CommonLengthF", "CommonLengthM"), names(sp_table))[1]
## 2026-09-24, per Andrea: Ecology/IUCN_conservation_status/Exploitation_
## status were previously hand-curated (left NA - see the traits_ewe
## header comment near where traits_reconciled is built) - she correctly
## identified these as real FishBase/SeaLifeBase species() fields, not
## something that needs manual entry. Resolved defensively (same pattern
## as vuln_col/common_length_col above) since this session has no
## network access to verify the exact column names against a live
## rfishbase pull - the coverage message below prints whichever field
## name was actually found (or "NONE FOUND") so a wrong guess is visible
## on the first real run rather than silently blank.
##   - Ecology              <- DemersPelag (FishBase/SeaLifeBase's own
##                             ecology category - bathydemersal/
##                             bathypelagic/benthic/benthopelagic/
##                             demersal/pelagic/pelagic-neritic/pelagic-
##                             oceanic/reef-associated - exactly Andrea's
##                             list except "land-based", which FishBase/
##                             SeaLifeBase has no reason to carry since
##                             it's a fish/aquatic-organism database -
##                             see the Organism/land-based note below).
##   - IUCN_conservation_status <- IUCN_Code (FishBase/SeaLifeBase's own
##                             IUCN Red List category field).
##   - Exploitation_status  <- Importance (FishBase/SeaLifeBase's own
##                             commercial-importance category - e.g.
##                             "highly commercial"/"minor commercial"/
##                             "subsistence fisheries"/"of no interest" -
##                             the closest FishBase-native field to
##                             "exploitation status"; if your version of
##                             rfishbase calls this something else, add
##                             the real name to the candidate list below).
demerspelag_col <- intersect(c("DemersPelag"), names(sp_table))[1]
iucn_col        <- intersect(c("IUCN_Code", "IUCNCode", "IUCN_code"), names(sp_table))[1]
importance_col  <- intersect(c("Importance", "importance"), names(sp_table))[1]

sp_traits <- if (nrow(sp_table) > 0) unique(sp_table[, .(
  Species,
  MaxWeight = if (!is.na(weight_col)) get(weight_col) else NA_real_,
  MaxLength = if (!is.na(length_col)) get(length_col) else NA_real_,
  Depth = fifelse(!is.na(DepthRangeDeep) & !is.na(DepthRangeShallow),
                  (DepthRangeDeep + DepthRangeShallow) / 2,
                  fcoalesce(DepthRangeDeep, DepthRangeShallow)),
  Longevity = if (!is.na(long_col)) get(long_col) else NA_real_,
  Vulnerability = if (!is.na(vuln_col)) get(vuln_col) else NA_real_,
  CommonLength = if (!is.na(common_length_col)) get(common_length_col) else NA_real_,
  Ecology = if (!is.na(demerspelag_col)) as.character(get(demerspelag_col)) else NA_character_,
  IUCN_conservation_status = if (!is.na(iucn_col)) as.character(get(iucn_col)) else NA_character_,
  Exploitation_status = if (!is.na(importance_col)) as.character(get(importance_col)) else NA_character_
  ## NOTE: Family deliberately NOT extracted here - it's already merged
  ## in from WoRMS taxonomy (Step 1, with proper synonym resolution),
  ## and duplicating it here would collide into Family.x/Family.y on
  ## the merge below instead of a clean single column
)], by = "Species") else data.table(Species = character(), MaxWeight = numeric(),
                                    MaxLength = numeric(), Depth = numeric(), Longevity = numeric(),
                                    Vulnerability = numeric(), CommonLength = numeric(),
                                    Ecology = character(), IUCN_conservation_status = character(),
                                    Exploitation_status = character())

message("Coverage - Vulnerability: ", sp_traits[!is.na(Vulnerability), .N], "/", length(sp_list),
        " (field: ", ifelse(is.na(vuln_col), "NONE FOUND", vuln_col), ")",
        " | CommonLength (-> Mean_length): ", sp_traits[!is.na(CommonLength), .N], "/", length(sp_list),
        " (field: ", ifelse(is.na(common_length_col), "NONE FOUND", common_length_col), ")")

message("Coverage - MaxWeight: ", sp_traits[!is.na(MaxWeight), .N], "/", length(sp_list),
        " | Depth: ", sp_traits[!is.na(Depth), .N], "/", length(sp_list),
        " | Longevity: ", sp_traits[!is.na(Longevity), .N], "/", length(sp_list),
        " (field: ", ifelse(is.na(long_col), "NONE FOUND", long_col), ")")

message("Coverage - Ecology: ", sp_traits[!is.na(Ecology), .N], "/", length(sp_list),
        " (field: ", ifelse(is.na(demerspelag_col), "NONE FOUND", demerspelag_col), ")",
        " | IUCN_conservation_status: ", sp_traits[!is.na(IUCN_conservation_status), .N], "/", length(sp_list),
        " (field: ", ifelse(is.na(iucn_col), "NONE FOUND", iucn_col), ")",
        " | Exploitation_status: ", sp_traits[!is.na(Exploitation_status), .N], "/", length(sp_list),
        " (field: ", ifelse(is.na(importance_col), "NONE FOUND", importance_col), ")")
invisible(STAGE_PB$tick(tokens = list(stage_name = "Fetch 2a: species() traits")))

## --- 2a2. Occurrence status (native/introduced/questionable in the ---
## Western Mediterranean) - via rfishbase::country(), FishBase/
## SeaLifeBase's own per-country status table. UNVERIFIED against a
## live pull (same caveat as everything else in this script that has no
## network access here to check real column names) - this table's exact
## column names/status vocabulary are a best-effort guess
## (Status/status; native/introduced/questionable/endemic-style values)
## and this is genuinely new functionality, not a refinement of
## something already working - check the coverage message below
## carefully on the first real run.
## Falls back to a hardcoded Western Med country list if TARGET_COUNTRIES
## isn't set in this session (e.g. this script run standalone, without
## 02_fisheries.R having run first).
OCCURRENCE_COUNTRIES <- if (exists("TARGET_COUNTRIES", envir = .GlobalEnv, inherits = FALSE)) {
  TARGET_COUNTRIES
} else {
  c("Spain", "France", "Italy", "Morocco", "Algeria", "Tunisia")
}
occurrence_traits <- tryCatch({
  country_raw <- fetch_both(rfishbase::country, sp_list)
  if (nrow(country_raw) == 0) stop("rfishbase::country() returned no rows for this species list.")
  country_col_name <- intersect(c("country", "Country"), names(country_raw))[1]
  status_col       <- intersect(c("Status", "status"), names(country_raw))[1]
  if (is.na(country_col_name) || is.na(status_col)) {
    stop("Expected columns (country/Country, Status/status) not found - columns present: ",
         paste(names(country_raw), collapse = ", "))
  }
  message("\nrfishbase::country() columns available:")
  print(names(country_raw))
  setnames(country_raw, c(country_col_name, status_col), c("CountryName", "Status"))
  med_rows <- country_raw[CountryName %in% OCCURRENCE_COUNTRIES]
  ## One species can have a different status per country (e.g. native
  ## in Spain, questionable in Tunisia) - collapsed to the distinct set
  ## found across the Western Med countries above, joined with "; " so
  ## nothing is silently picked over another; genuinely one-status
  ## species just get that one value.
  med_rows[, .(Occurrence_status = paste(sort(unique(Status)), collapse = "; ")), by = Species]
}, error = function(e) {
  message("Occurrence_status (rfishbase::country()) fetch failed - ", conditionMessage(e),
          ". Occurrence_status will be blank for every species this run.")
  data.table(Species = character(), Occurrence_status = character())
})
message("Coverage - Occurrence_status: ", nrow(occurrence_traits), "/", length(sp_list),
        " species have at least one Western Med country record.")
invisible(STAGE_PB$tick(tokens = list(stage_name = "Fetch 2a2: occurrence status")))

## --- 2b. Growth params (Loo, K, Winfinity) - Mediterranean/recent-prioritized
## FISH ONLY (sp_list_fish, not sp_list) - see the 2026-09-25 comment
## right after dispatch_group is attached to species_df, above.
## 2026-09-26 fix, per Andrea ("is taking foreverer... optimize that
## with lower the search only for certain pars for fish"): fetch_both()
## defaults to ALSO querying SeaLifeBase (fishbase_only = FALSE) - fine
## when sp_list could contain non-fish species, but sp_list_fish is
## ALREADY restricted to species whose Class is in FISH_CLASSES (real
## fish live in FishBase, not SeaLifeBase). Querying SeaLifeBase for
## them anyway was pure waste, and worse: SeaLifeBase's remote parquet
## backend (the "cboettig/fishbase/slb/..." S3 bucket) was timing out
## repeatedly in a real run (10+ minutes per failed batch, confirmed from
## the actual console log: "Operation too slow"/"Connection timed out"),
## which then fell back to 400+ sequential PER-SPECIES SeaLifeBase calls,
## each eating the same multi-minute timeout - this alone accounted for
## multiple hours of the run (ETA readings up to "5d" were observed).
## Setting fishbase_only = TRUE here (matching what the swimming() call
## in 2f already correctly did) skips the SeaLifeBase attempt entirely
## for these three genuinely fish-only fetches.
growth_raw <- if (length(sp_list_fish) > 0) fetch_both(rfishbase::popgrowth, sp_list_fish, fishbase_only = TRUE) else data.table()
growth_best <- select_best_rows(growth_raw)
tmax_col <- if (nrow(growth_best) > 0) intersect(c("tmax", "TMax"), names(growth_best))[1] else NA
growth_traits <- if (nrow(growth_best) > 0) growth_best[, .(
  Species,
  Loo  = if ("Loo" %in% names(growth_best)) Loo else NA_real_,
  K    = if ("K" %in% names(growth_best)) K else NA_real_,
  Winf = if ("Winfinity" %in% names(growth_best)) Winfinity else NA_real_,
  tmax = if (!is.na(tmax_col)) get(tmax_col) else NA_real_
)] else data.table(Species = character(), Loo = numeric(), K = numeric(),
                   Winf = numeric(), tmax = numeric())

## Capture WHERE this selected record came from (Locality/Year), not
## just the numeric values - needed for the reference/provenance table
growth_provenance <- extract_provenance(growth_best, "growth (Loo/K/Winf/tmax)",
                                        refno_candidates = c("PopGrowthRef", "RefNo", "MainRefNo"))
invisible(STAGE_PB$tick(tokens = list(stage_name = "Fetch 2b: growth params")))

## --- 2c. Length-weight a/b params - same prioritization ---------------
## FISH ONLY (sp_list_fish) - same reasoning as 2b above.
## FISHBASE ONLY - same 2026-09-26 fix/reasoning as popgrowth above.
lw_raw <- if (length(sp_list_fish) > 0) fetch_both(rfishbase::poplw, sp_list_fish, fishbase_only = TRUE) else data.table()
lw_best <- select_best_rows(lw_raw)
lw_traits <- if (nrow(lw_best) > 0) lw_best[, .(
  Species,
  a_lw = if ("a" %in% names(lw_best)) a else NA_real_,
  b_lw = if ("b" %in% names(lw_best)) b else NA_real_
)] else data.table(Species = character(), a_lw = numeric(), b_lw = numeric())
lw_provenance <- extract_provenance(lw_best, "length-weight (a/b)",
                                    refno_candidates = c("LWRef", "PopLWRef", "LengthWeightRef", "RefNo", "MainRefNo"))
invisible(STAGE_PB$tick(tokens = list(stage_name = "Fetch 2c: length-weight a/b")))

## --- 2d. Maturity: Lm (length at maturity), needed for Froese-Binohlan
## FISH ONLY (sp_list_fish) - same reasoning as 2b above.
## FISHBASE ONLY - same 2026-09-26 fix/reasoning as popgrowth above.
maturity_raw <- if (length(sp_list_fish) > 0) fetch_both(rfishbase::maturity, sp_list_fish, fishbase_only = TRUE) else data.table()
maturity_best <- select_best_rows(maturity_raw)
lm_col <- if (nrow(maturity_best) > 0) intersect(c("Lm", "LengthMatMin"), names(maturity_best))[1] else NA
tmat_col <- if (nrow(maturity_best) > 0) intersect(c("tm", "AgeMatMin"), names(maturity_best))[1] else NA
maturity_traits <- if (nrow(maturity_best) > 0) maturity_best[, .(
  Species,
  Lm   = if (!is.na(lm_col)) get(lm_col) else NA_real_,
  Tmat = if (!is.na(tmat_col)) get(tmat_col) else NA_real_
)] else data.table(Species = character(), Lm = numeric(), Tmat = numeric())
maturity_provenance <- extract_provenance(maturity_best, "maturity (Lm/Tmat)",
                                          refno_candidates = c("MaturityRef", "MatRef", "RefNo", "MainRefNo"))

message("Coverage - Lm (length at maturity): ", maturity_traits[!is.na(Lm), .N], "/", length(sp_list_fish),
        " fish species (field: ", ifelse(is.na(lm_col), "NONE FOUND", lm_col), ")")
invisible(STAGE_PB$tick(tokens = list(stage_name = "Fetch 2d: maturity")))

## --- 2e. Ecology: trophic level ---------------------------------------
ecol <- fetch_both(rfishbase::ecology, sp_list)
tl_col <- if (nrow(ecol) > 0) intersect(c("DietTroph", "FoodTroph"), names(ecol))[1] else NA
ecol_traits <- if (nrow(ecol) > 0) unique(ecol[, .(
  Species, TrophicLevel = if (!is.na(tl_col)) get(tl_col) else NA_real_
)], by = "Species") else data.table(Species = character(), TrophicLevel = numeric())
invisible(STAGE_PB$tick(tokens = list(stage_name = "Fetch 2e: ecology/trophic level")))

## --- 2f. Swimming: aspect ratio (fish-specific caudal-fin morphology -
## SeaLifeBase never has this, so skip that call entirely rather than
## waste time on a query that can never succeed) -----------------------
## FISH ONLY (sp_list_fish) - `fishbase_only = TRUE` already meant this
## never returned rows for a non-fish species, but it was still QUERIED
## against the full sp_list (mammals/seabirds/invertebrates included)
## every run - narrowed to sp_list_fish so the query itself is smaller,
## consistent with 2b/2c/2d above.
swim <- if (length(sp_list_fish) > 0) fetch_both(rfishbase::swimming, sp_list_fish, fishbase_only = TRUE) else data.table()
ar_col <- if (nrow(swim) > 0) intersect(c("AspectRatio", "Aspect"), names(swim))[1] else NA
swim_traits <- if (nrow(swim) > 0) unique(swim[, .(
  Species, AspectRatio = if (!is.na(ar_col)) get(ar_col) else NA_real_
)], by = "Species") else data.table(Species = character(), AspectRatio = numeric())
invisible(STAGE_PB$tick(tokens = list(stage_name = "Fetch 2f: swimming/aspect ratio")))

## --- assemble ----------------------------------------------------------
species_df <- Reduce(function(x, y) merge(x, y, by = "Species", all.x = TRUE),
                     list(species_df, sp_traits, growth_traits, lw_traits,
                          maturity_traits, ecol_traits, swim_traits, occurrence_traits))

if (!exists("DEFAULT_TEMP", envir = .GlobalEnv, inherits = FALSE)) DEFAULT_TEMP <- 16
species_df[, Temp := DEFAULT_TEMP]

## =================================================================
## How to pick the FINAL "chosen" PB/QB when multiple independent
## methods are available for a species. Most relevant for fish (6 PB
## methods, 4 QB methods) but applies wherever more than one method
## exists (mammal PB/QB, invertebrate PB).
##   "priority" - use the single best-validated method first (e.g.
##                Then et al. 2015 for fish M), falling back down an
##                ordered list only if it's missing. This is what the
##                pipeline did before this option existed.
##   "mean"     - average across every method that succeeded for that
##                species, giving equal weight to each rather than
##                trusting one method's validated performance over
##                the others.
## =================================================================
if (!exists("PB_QB_SELECTION_MODE", envir = .GlobalEnv, inherits = FALSE)) PB_QB_SELECTION_MODE <- "priority"  # "priority" or "mean"

select_chosen <- function(dt, method_cols_in_priority_order) {
  if (PB_QB_SELECTION_MODE == "mean") {
    vals <- rowMeans(dt[, ..method_cols_in_priority_order], na.rm = TRUE)
    vals[is.nan(vals)] <- NA_real_  # rowMeans on an all-NA row gives NaN, not NA
    vals
  } else {
    do.call(fcoalesce, as.list(dt[, ..method_cols_in_priority_order]))
  }
}

## Matching label helper - "priority" mode names the specific method
## that won (same as before); "mean" mode says how many methods went
## into the average, since naming just one would misrepresent it
describe_chosen <- function(dt, method_col_to_label) {
  cols <- names(method_col_to_label)
  if (PB_QB_SELECTION_MODE == "mean") {
    n_avail <- rowSums(!is.na(dt[, ..cols]))
    fifelse(n_avail == 0, NA_character_, paste0("Mean of ", n_avail, " method(s)"))
  } else {
    result <- rep(NA_character_, nrow(dt))
    for (i in rev(seq_along(cols))) {
      result <- fifelse(!is.na(dt[[cols[i]]]), unname(method_col_to_label[i]), result)
    }
    result
  }
}

## derive Winf from a/b length-weight relationship (protocol Eq. 11)
## when not directly available from popgrowth's Winfinity field
species_df[, Winf := fcoalesce(Winf, a_lw * Loo^b_lw)]

## Froese & Binohlan (2000) fallback for Loo/K when missing entirely -
## now fully automatic: Lmax from species(), Lm/Tmat from maturity().
## NOTE: corrected from "(2003)" in earlier comments here - the actual
## paper (Loo = Lmax/0.95, t0 from Loo/K/Lm/Tmat) is Froese, R. &
## Binohlan, C. (2000). J. Fish Biology 56(4):758-773 - see
## METHOD_REFERENCES below for the full citation.
species_df[, Loo_fb := MaxLength / 0.95]
needs_fb <- species_df[is.na(Loo) & !is.na(Loo_fb) & !is.na(Lm) & !is.na(Tmat)]
if (nrow(needs_fb) > 0) {
  needs_fb[, t0_est := 0]
  for (iter in 1:3) {
    needs_fb[, k_est := -log(1 - Lm / Loo_fb) / (Tmat - t0_est)]
    needs_fb[, t0_est := -10^(-0.3922 - 0.2752 * log10(Loo_fb) - 1.038 * log10(k_est))]
  }
  species_df[needs_fb, on = "Species", `:=`(Loo = fifelse(is.na(Loo), i.Loo_fb, Loo),
                                            K = fifelse(is.na(K), i.k_est, K))]
  message("\nFroese-Binohlan fallback filled Loo/K for ", nrow(needs_fb), " species",
          " using auto-fetched Lmax/Lm/Tmat.")
}

message("\n=== Overall trait coverage before calculation ===")
print(species_df[, .(
  n = .N, MaxWeight = sum(!is.na(MaxWeight)), Loo = sum(!is.na(Loo)), K = sum(!is.na(K)),
  Winf = sum(!is.na(Winf)), Lm = sum(!is.na(Lm)), TrophicLevel = sum(!is.na(TrophicLevel)),
  AspectRatio = sum(!is.na(AspectRatio)), Longevity = sum(!is.na(Longevity)), Depth = sum(!is.na(Depth))
), by = dispatch_group])
invisible(STAGE_PB$tick(tokens = list(stage_name = "Assemble + derive traits (Froese-Binohlan)")))

## Fill gaps in the RAW TRAITS themselves (before any formula runs) by
## taxonomic proximity - local donors within species_df first, external
## FishBase/SeaLifeBase donors if none exist locally. This is more
## effective than only patching derived results downstream, since a
## single missing Winf currently blanks multiple different QB methods
## at once (eq26, eq24) - filling it once here fixes all of them together.
for (trait in c("Winf", "TrophicLevel", "MaxWeight", "AspectRatio", "Longevity")) {
  species_df <- fill_by_taxonomic_proximity(species_df, trait)
}

message("\n=== Trait coverage AFTER taxonomic-proximity gap filling ===")
print(species_df[, .(
  n = .N, MaxWeight = sum(!is.na(MaxWeight)), Winf = sum(!is.na(Winf)),
  TrophicLevel = sum(!is.na(TrophicLevel)), AspectRatio = sum(!is.na(AspectRatio)),
  Longevity = sum(!is.na(Longevity))
), by = dispatch_group])
invisible(STAGE_PB$tick(tokens = list(stage_name = "Raw-trait gap filling")))

## =================================================================
## FISH: Pauly (1980) M [Eq. 9] -> FishLife (phylogenetic imputation,
## Thorson et al. 2017/2020/2023) -> Gascuel fallback;
## F = Yield/Biomass (density-consistent); PB = M+F
## QB via Palomares & Pauly (1998) [Eq. 27] -> Q/P=3 fallback
## =================================================================

## FishLife: for species with NO direct FishBase growth studies, this
## borrows information from phylogenetically related taxa rather than
## jumping straight to the trophic-level-only Gascuel fallback - a
## genuinely better-informed estimate for data-poor species. Not part
## of the original protocol document, but a legitimate, actively
## maintained (2023) addition specifically suited to this exact gap.
## Requires: devtools::install_github("james-thorson/FishLife", dep=TRUE)
##
## NOTE: FishLife's exact function signature has changed across its
## 2017/2020/2023 releases - this uses the longest-standing, most-cited
## interface (Search_species + Plot_taxa). If your installed version
## errors here, check ?FishLife::Search_species for the current API
## and adjust - the coverage printout below will make failures visible
## rather than silently skipping species.

## Class-name translation: WoRMS and FishLife's bundled (older FishBase-
## based) taxonomy use different terms for the same group in places -
## CONFIRMED via direct testing (installed and ran FishLife in an
## isolated environment to verify): WoRMS says "Teleostei", FishLife's
## database uses "Actinopterygii" for the same broad group. Without
## FishLife: for species with NO direct FishBase growth studies, this
## borrows information from phylogenetically related taxa rather than
## jumping straight to the trophic-level-only Gascuel fallback - a
## genuinely better-informed estimate for data-poor species.
##
## FishLife::Search_species() calls rfishbase::fishbase, an object that
## doesn't exist in your installed rfishbase (confirmed directly:
## "'fishbase' is not an exported object from 'namespace:rfishbase'").
## This bypasses Search_species() entirely, using the taxonomy you
## already fetched via WoRMS (Step 1) to do the same matching against
## FishLife's own bundled prediction tree (ParentChild_gz) - verified
## working this way against real species in isolated testing. No
## reinstall needed - Plot_taxa() itself doesn't touch rfishbase at all,
## only Search_species() did.
##
## NOTE: verified against a recent build of FishLife - you're on 3.1.0,
## an older release, so the internal object names below (ParentChild_gz,
## Find_ancestors, ChildName) are assumed but not confirmed for your
## exact version. The diagnostic print will make it obvious immediately
## if something doesn't match, rather than silently returning NA again.
FISHLIFE_CLASS_TRANSLATION <- c(
  "Teleostei" = "Actinopterygii",
  "Actinopteri" = "Actinopterygii"
)

fetch_fishlife <- function(taxonomy_dt) {
  if (nrow(taxonomy_dt) == 0) return(data.table())
  if (!requireNamespace("FishLife", quietly = TRUE)) {
    message("FishLife not installed - skipping this method",
            " (fine, five other fish M methods are still available).")
    return(data.table())
  }
  
  db <- tryCatch(FishLife::FishBase_and_RAM, error = function(e) NULL)
  if (is.null(db) || is.null(db$ParentChild_gz)) {
    message("FishLife's bundled database (FishBase_and_RAM$ParentChild_gz)",
            " wasn't found under that name in your installed version (3.1.0) -",
            " the internal structure may differ from what this was verified",
            " against. Skipping FishLife for this run.")
    return(data.table())
  }
  ParentChild_gz <- db$ParentChild_gz
  
  match_taxon <- function(class_, order_, family_, genus_, species_epithet) {
    class_ <- if (!is.na(class_) && class_ %in% names(FISHLIFE_CLASS_TRANSLATION)) {
      FISHLIFE_CLASS_TRANSLATION[[class_]]
    } else class_
    match_taxonomy <- c(class_, order_, family_, genus_, species_epithet)
    match_taxonomy[is.na(match_taxonomy)] <- "predictive"
    
    Count <- 0
    Group <- NA
    while (is.na(Group) && Count <= 5) {
      Group <- match(paste(tolower(match_taxonomy), collapse = "_"), tolower(ParentChild_gz[, 'ChildName']))
      if (is.na(Group)) match_taxonomy[length(match_taxonomy) - Count] <- "predictive"
      Count <- Count + 1
    }
    if (is.na(Group)) return(NULL)
    
    Group <- tryCatch(FishLife:::Find_ancestors(child_num = Group, ParentChild_gz = ParentChild_gz),
                      error = function(e) NULL)
    if (is.null(Group)) return(NULL)
    
    Add_predictive <- function(char_vec) {
      return_vec <- char_vec
      for (i in seq_along(return_vec)) {
        vec <- strsplit(as.character(return_vec[i]), "_")[[1]]
        return_vec[i] <- paste(c(vec, rep("predictive", 5 - length(vec))), collapse = "_")
      }
      return_vec
    }
    unique(as.character(Add_predictive(ParentChild_gz[Group, 'ChildName'])))
  }
  
  results <- vector("list", nrow(taxonomy_dt))
  printed_diagnostic <- FALSE
  
  pb <- progress_bar$new(
    format = "  FishLife lookup [:bar] :percent | :current/:total | Elapsed: :elapsedfull | ETA: :eta",
    total = nrow(taxonomy_dt), clear = FALSE, width = 90
  )
  for (i in seq_len(nrow(taxonomy_dt))) {
    invisible(pb$tick())
    row <- taxonomy_dt[i]
    parts <- strsplit(row$Species, " ")[[1]]
    if (length(parts) < 2) next
    
    pred <- tryCatch({
      taxon_match <- match_taxon(row$Class, row$Order, row$Family, row$Genus, parts[2])
      if (is.null(taxon_match)) return(NULL)
      tmp_plot <- tempfile(fileext = ".pdf")
      grDevices::pdf(tmp_plot)
      on.exit({ grDevices::dev.off(); unlink(tmp_plot) }, add = TRUE)
      FishLife::Plot_taxa(taxon_match, mfrow = c(1, 1))
    }, error = function(e) {
      message("  FishLife lookup failed for ", row$Species, ": ", conditionMessage(e))
      NULL
    })
    
    if (is.null(pred)) next
    mean_pred <- pred[[1]]$Mean_pred
    if (is.null(mean_pred)) next
    
    if (!printed_diagnostic) {
      message("\n>>> DIAGNOSTIC: FishLife Mean_pred names (first species, '", row$Species,
              "') - verify Loo/K/M/Winfinity appear here <<<")
      print(names(mean_pred))
      printed_diagnostic <- TRUE
    }
    
    loo_n  <- intersect(c("Loo", "ln_Loo"), names(mean_pred))[1]
    k_n    <- intersect(c("K", "ln_K"), names(mean_pred))[1]
    m_n    <- intersect(c("M", "ln_M"), names(mean_pred))[1]
    winf_n <- intersect(c("Winfinity", "ln_Winfinity"), names(mean_pred))[1]
    
    results[[i]] <- data.table(
      Species = row$Species,
      Loo_fishlife  = if (!is.na(loo_n)) exp(mean_pred[[loo_n]]) else NA_real_,
      K_fishlife    = if (!is.na(k_n)) exp(mean_pred[[k_n]]) else NA_real_,
      M_FishLife_2023    = if (!is.na(m_n)) exp(mean_pred[[m_n]]) else NA_real_,
      Winf_fishlife = if (!is.na(winf_n)) exp(mean_pred[[winf_n]]) else NA_real_
    )
  }
  out <- rbindlist(results, fill = TRUE)
  message("FishLife: resolved ", nrow(out), " of ", nrow(taxonomy_dt), " species attempted.")
  out
}

## TropFishR::M_empirical() - bundles several established, distinct M
## estimators beyond Pauly (1980), including Then et al. (2015), which
## its own paper identifies as the best-performing empirical estimator
## across a 200+ species validation set - arguably the current
## methodological standard, not just an alternative. Same underlying
## logic as the fishmethods package that powers the Barefoot Ecologist's
## Natural Mortality Tool Shiny app, but with self-documenting named
## methods instead of a numbered method list.
## Requires: install.packages("TropFishR")

fetch_tropfishr_M <- function(sp_dt) {
  if (!requireNamespace("TropFishR", quietly = TRUE)) {
    message("TropFishR not installed - skipping these M methods.",
            " Install via: install.packages('TropFishR')")
    return(data.table())
  }
  
  ## VERIFIED against the actual TropFishR source (M_empirical.R): the
  ## function returns a MATRIX with a single column literally named "M" -
  ## each requested method is a ROW, identified only by its ROW NAME
  ## (e.g. "Hoenig (1983) - Joint Equation", "Then (2015) - growth"),
  ## NOT by a per-method column. Confirmed empirically by running the
  ## exact source locally. The previous version searched for columns
  ## named "Hoenig"/"Then_growth"/etc., which never existed - that's why
  ## every method came back NA regardless of input data availability.
  ## Note also: "Hoenig" produces TWO rows (Joint Equation, Fish
  ## Equation) - averaged here into one M_Hoenig_1983 value.
  methods_wanted <- c("Hoenig", "Then_growth", "AlversonCarney")
  results <- vector("list", nrow(sp_dt))
  printed_diagnostic <- FALSE
  
  pb <- progress_bar$new(
    format = "  TropFishR M methods [:bar] :percent | :current/:total | Elapsed: :elapsedfull | ETA: :eta",
    total = nrow(sp_dt), clear = FALSE, width = 90
  )
  for (i in seq_len(nrow(sp_dt))) {
    invisible(pb$tick())
    row <- sp_dt[i]
    pred <- tryCatch(
      TropFishR::M_empirical(Linf = row$Loo, K_l = row$K, temp = row$Temp,
                             tmax = row$tmax, method = methods_wanted),
      error = function(e) {
        message("  TropFishR::M_empirical() failed for ", row$Species, ": ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(pred)) next
    
    ## keep.rownames is the actual fix - without it, the method-
    ## identifying row names are silently dropped entirely
    pred_dt <- as.data.table(pred, keep.rownames = "method_name")
    
    if (!printed_diagnostic) {
      message("\n>>> DIAGNOSTIC: TropFishR::M_empirical() output (first species, '",
              row$Species, "') <<<")
      print(pred_dt)
      printed_diagnostic <- TRUE
    }
    
    find_val <- function(pattern) {
      hits <- pred_dt[grepl(pattern, method_name, ignore.case = TRUE)]
      if (nrow(hits) == 0) return(NA_real_)
      mean(hits$M, na.rm = TRUE)
    }
    
    results[[i]] <- data.table(
      Species = row$Species,
      M_Hoenig_1983 = find_val("hoenig"),
      M_Then_2015   = find_val("then"),
      M_AlversonCarney_1975 = find_val("alverson|carney")
    )
  }
  out <- rbindlist(results, fill = TRUE)
  message("TropFishR M methods: resolved ", nrow(out), " of ", nrow(sp_dt), " species attempted",
          " (", out[!is.na(M_Hoenig_1983), .N], " with Hoenig, ", out[!is.na(M_Then_2015), .N],
          " with Then, ", out[!is.na(M_AlversonCarney_1975), .N], " with Alverson-Carney).")
  out
}

calc_fish <- function(out) {
  ## --- Fetch FishLife for ALL fish species (not gated by missing Loo/K) -
  ## computing every method for every species, so they're genuinely
  ## comparable against Pauly/Hoenig/Then/etc., not just a fallback
  ## chain that skips FishLife whenever direct data already exists -----
  fl <- fetch_fishlife(out[, .(Species, Genus, Family, Order, Class)])
  if (nrow(fl) > 0) out <- merge(out, fl, by = "Species", all.x = TRUE)
  for (col in c("Loo_fishlife", "K_fishlife", "M_FishLife_2023", "Winf_fishlife")) {
    if (!col %in% names(out)) out[, (col) := NA_real_]
  }
  
  ## =================================================================
  ## PB - three independent methods, each as its own column
  ## =================================================================
  
  ## Method 1: Pauly (1980), protocol Eq.9, from direct FishBase growth data
  out[, M_Pauly_1980 := 10^(-0.0066 - 0.279 * log10(Loo) + 0.6543 * log10(K) + 0.4634 * log10(Temp))]
  
  ## Method 2: FishLife's own M (phylogenetic imputation) - used directly,
  ## not re-derived via Pauly from FishLife's Loo/K, which would discard
  ## its joint-uncertainty modeling across correlated traits
  ## (M_FishLife_2023 column already exists from the merge/default above)
  
  ## Method 3: Gascuel et al. (2008), fish-specific, protocol Eq.15 -
  ## only needs trophic level + temperature, computable for almost every
  ## species regardless of growth-data availability
  out[, M_Gascuel_2008 := 2.31 * TrophicLevel^(-1.72) * exp(0.053 * Temp)]
  
  ## Methods 4-6: TropFishR::M_empirical() - Hoenig (1983), Then et al.
  ## (2015, identified in its own validation paper as the best-performing
  ## empirical estimator across 200+ species), and Alverson & Carney
  ## (1975) - genuinely distinct methods, not variations on Pauly's
  tfr <- fetch_tropfishr_M(out[, .(Species, Loo, K, Temp, tmax)])
  if (nrow(tfr) > 0) {
    out <- merge(out, tfr, by = "Species", all.x = TRUE)
  } else {
    out[, c("M_Hoenig_1983", "M_Then_2015", "M_AlversonCarney_1975") := NA_real_]
  }
  
  ## Fill gaps in each M method independently by borrowing from the
  ## closest taxonomic relative that has a DIRECT value for that same
  ## method - e.g. a species missing growth data for Pauly's equation
  ## can borrow its genus-mates' Pauly M, rather than that method
  ## column just staying NA. Every borrowed value is tagged, not
  ## silently blended in.
  for (col in c("M_Pauly_1980", "M_Hoenig_1983", "M_Then_2015", "M_AlversonCarney_1975", "M_Gascuel_2008")) {
    out <- fill_by_taxonomic_proximity(out, col)
  }
  
  ## F = Yield/Biomass at SPECIES level (both must be densities) when
  ## available; falls back to Fmort_FG (the FG-wide catch/biomass rate
  ## computed early in the script from 02_fao_catches.R's data) when
  ## species-level Yield is NA - which right now is EVERY fish species,
  ## since no species-level landings source is implemented. Species-
  ## level F always takes priority when present. Applies identically
  ## on top of whichever M source, since Z = M+F regardless of how M
  ## was obtained.
  ##
  ## NOT IMPLEMENTED, worth flagging explicitly: neither Fmort_species
  ## nor Fmort_FG is disaggregated by fishing fleet, gear, or GSA-level
  ## effort - both are a single Yield/Biomass ratio lumped across the
  ## WHOLE region and ALL gears/fleets that landed that species/FG.
  ## Neither GFCM_Capture_Quantity.csv (what 02_fao_catches.R actually
  ## reads - Country x Species x Division x Year only) nor the
  ## landings_by_species_gsa_year.csv placeholder above (ScientificName x
  ## Year x AreaID x catch_t) carries a fleet or gear column - there is
  ## currently NOTHING in this pipeline that could compute a fleet- or
  ## gear-specific F even if a real landings file were plugged in.
  ## GFCM DOES define exactly this kind of disaggregation for its own
  ## stock assessments - "Operational Units" (fleet segment x ISSCFG gear
  ## code x GSA), collected via the DCRF's Task 2 (Catch), Task 4
  ## (Fleet) and Task 5 (Effort), plus the Regional Fleet Register - but
  ## that OU-level data is a genuinely different GFCM product than the
  ## STATLANT-derived capture-production file this pipeline reads, is
  ## submitted by national correspondents, and isn't confirmed to have
  ## the same kind of open bulk-download this script's current source
  ## does. Adding real fleet/gear resolution here means sourcing THAT
  ## data (via GFCM's DCRF/SAC channels, or STECF's FDI database for the
  ## EU-flagged portion of the fleet specifically), not something
  ## derivable from GFCM_Capture_Quantity.csv itself.
  ##
  ## SINGLE FLEET HERE STILL, EVEN THOUGH Catches_ NOW SUPPORTS SPLITS:
  ## Fmort/Fmort_species/Fmort_FG above/below remain a single lumped
  ## Yield/Biomass ratio regardless of whether add_catches_to_ecopath_
  ## workbook() (lib_survey_fg_density_functions.R, called from
  ## 02_fao_catches.R) was given a fleet_structure - this script never
  ## sees fleet_structure itself, and Yield/Biomass here is computed
  ## from the same un-split fg_catch_timeseries_*.csv either way.
  ## Fleet-specific F would need Yield split by fleet BEFORE this
  ## ratio, not just the Catches_ sheets split after; not implemented.
  out[, Fmort_species := Yield / Biomass]
  out[, Fmort := fifelse(!is.na(Fmort_species), Fmort_species, Fmort_FG)]
  out[, Fmort_source := fifelse(!is.na(Fmort_species), "species (Y/B)",
                                fifelse(!is.na(Fmort_FG), "FG rate (02_fao_catches.R)", NA_character_))]
  out[, PB_Pauly_1980    := fifelse(!is.na(Fmort), M_Pauly_1980 + Fmort, M_Pauly_1980)]
  out[, PB_FishLife_2023 := fifelse(!is.na(Fmort), M_FishLife_2023 + Fmort, M_FishLife_2023)]
  out[, PB_Gascuel_2008  := fifelse(!is.na(Fmort), M_Gascuel_2008 + Fmort, M_Gascuel_2008)]
  out[, PB_Hoenig_1983   := fifelse(!is.na(Fmort), M_Hoenig_1983 + Fmort, M_Hoenig_1983)]
  out[, PB_Then_2015     := fifelse(!is.na(Fmort), M_Then_2015 + Fmort, M_Then_2015)]
  out[, PB_AlversonCarney_1975 := fifelse(!is.na(Fmort), M_AlversonCarney_1975 + Fmort, M_AlversonCarney_1975)]
  
  ## chosen PB for FG-level weighting: Then et al. (2015) prioritized
  ## first given its validated best-in-class performance, then Pauly
  ## (the protocol's own primary method), then the rest - but ALL method
  ## columns remain available for direct comparison regardless of
  ## what gets chosen here
  fish_pb_labels <- c(PB_Then_2015 = "Then et al. 2015", PB_Pauly_1980 = "Pauly 1980 (Eq.9)",
                      PB_Hoenig_1983 = "Hoenig 1983", PB_AlversonCarney_1975 = "Alverson & Carney 1975",
                      PB_FishLife_2023 = "FishLife", PB_Gascuel_2008 = "Gascuel 2008 (Eq.15)")
  out[, PB := select_chosen(out, names(fish_pb_labels))]
  out[, PB_method := describe_chosen(out, fish_pb_labels)]
  out[!is.na(Fmort) & !is.na(PB), PB_method := paste0(PB_method, "+F(", Fmort_source, ")")]
  
  ## =================================================================
  ## QB - four independent methods, each as its own column
  ## =================================================================
  
  out[, Tprime := 1000 / (Temp + 273.15)]
  h_dummy <- 0; d_dummy <- 0   # herbivore/detritivore dummies default to
  # carnivore (0) - no reliable automated
  # diet-type classification available yet
  Pf_dummy <- 1                 # predator dummy for Eq.24 - defaults to 1
  # (predator), the common case for real fish
  
  ## Method 1: Palomares & Pauly (1998), protocol Eq.27 - uses Z (=PB)
  ## directly, the most information-rich when a PB estimate is available
  out[, logQB_PalomaresPauly_1998Z := 5.847 + 0.280 * log10(PB) - 0.152 * log10(Winf) -
        1.360 * Tprime + 0.062 * AspectRatio + 0.510 * h_dummy + 0.390 * d_dummy]
  out[, QB_PalomaresPauly_1998Z := 10^logQB_PalomaresPauly_1998Z]
  
  ## Method 2: Palomares & Pauly (1998), protocol Eq.26 - does NOT need
  ## Z/PB at all, only morphometrics/temperature/diet-type - useful as
  ## an independent cross-check since it doesn't inherit any PB error
  out[, logQB_PalomaresPauly_1998noZ := 7.964 - 0.204 * log10(Winf) - 1.965 * Tprime +
        0.083 * AspectRatio + 0.532 * h_dummy + 0.398 * d_dummy]
  out[, QB_PalomaresPauly_1998noZ := 10^logQB_PalomaresPauly_1998noZ]
  
  ## Method 3: Christensen & Pauly (1992) adapted, protocol Eq.24 - a
  ## different independent formula, uses predator/herbivore dummies
  ## instead of aspect ratio, doesn't need Z either.
  ## NOTE: uses T' directly, NOT log10(T') - the source PDF extraction
  ## showed "log10 T'" here, but that's almost certainly an OCR artifact:
  ## Eq.26 and Eq.27 in the same protocol both use T' directly, and
  ## log-transforming it here produces Q/B values in the tens of
  ## thousands (physically implausible - real fish Q/B is ~1-20/year).
  ## Using T' directly brings this back in line with the other two
  ## equations' order of magnitude.
  out[, logQB_ChristensenPauly_1992 := 6.37 - 1.5045 * Tprime - 0.168 * log10(Winf) +
        0.1399 * Pf_dummy + 0.2765 * h_dummy]
  out[, QB_ChristensenPauly_1992 := 10^logQB_ChristensenPauly_1992]
  
  ## Fill gaps in the two Winf/AspectRatio-dependent QB methods by
  ## borrowing from the closest taxonomic relative - QB_ChristensenPauly_1992 doesn't
  ## need AspectRatio so it's less prone to gaps, but still benefits
  for (col in c("QB_PalomaresPauly_1998Z", "QB_PalomaresPauly_1998noZ", "QB_ChristensenPauly_1992")) {
    out <- fill_by_taxonomic_proximity(out, col)
  }
  
  ## Method 4: Q/P=3 fallback (Eq.29), using chosen PB
  out[, QB_ChristensenEtAl_2008 := 3 * PB]
  
  ## chosen QB for FG-level weighting: prefer the Z-based equation
  ## (most information used) > the two Z-independent equations > Q/P=3
  fish_qb_labels <- c(QB_PalomaresPauly_1998Z = "Palomares & Pauly 1998 (Z-based)",
                      QB_PalomaresPauly_1998noZ = "Palomares & Pauly 1998 (non-Z)",
                      QB_ChristensenPauly_1992 = "Christensen & Pauly 1992",
                      QB_ChristensenEtAl_2008 = "Q/P=3 (Christensen et al. 2008)")
  out[, QB := select_chosen(out, names(fish_qb_labels))]
  out[, QB_method := describe_chosen(out, fish_qb_labels)]
  
  out[, .(Species, FG, FG_name, Biomass, dispatch_group,
          M_Pauly_1980, M_FishLife_2023, M_Gascuel_2008, M_Hoenig_1983, M_Then_2015, M_AlversonCarney_1975,
          Fmort, Fmort_species, Fmort_source,
          PB_Pauly_1980, PB_FishLife_2023, PB_Gascuel_2008, PB_Hoenig_1983, PB_Then_2015, PB_AlversonCarney_1975, PB, PB_method,
          QB_PalomaresPauly_1998Z, QB_PalomaresPauly_1998noZ, QB_ChristensenPauly_1992, QB_ChristensenEtAl_2008, QB, QB_method)]
}

## =================================================================
## MARINE MAMMALS: Barlow & Boveng Siler PB -> Gascuel fallback;
## Innes/Trites QB -> Q/P=3 fallback
## =================================================================

siler_pb <- function(longevity, surrogate_type) {
  params <- list(
    "2" = list(a1 = 14.343, a2 = 0.171,  a3 = 0.0121, b1 = 10.259, b3 = 6.6878),
    "3" = list(a1 = 30.43,  a2 = 0,      a3 = 0.7276, b1 = 206.72, b3 = 2.3188),
    "4" = list(a1 = 40.409, a2 = 0.4772, a3 = 0.0047, b1 = 310.36, b3 = 8.029)
  )
  p <- params[[as.character(surrogate_type)]]
  if (is.null(p) || is.na(longevity)) return(NA_real_)
  W <- longevity
  x <- 1:floor(W)
  lj <- exp((-p$a1 / p$b1) * (1 - exp(-p$b1 * x / W)))
  lc <- exp(-p$a2 * x / W)
  ls <- exp((p$a3 / p$b3) * (1 - exp(p$b3 * x / W)))
  lx <- lj * lc * ls
  ## 2026-09-25 fix, per Daniel's real run (crashed with "Error in
  ## (function (classes, fdef, mtable) ... : unable to find an inherited
  ## method for function 'shift' for signature '"numeric"'"): a bare
  ## `shift()` call is ambiguous once a package that registers `shift` as
  ## an S4 generic for spatial objects is loaded (e.g. `raster`/`terra` -
  ## both are `library()`'d by 01_biomass.R, sourced earlier in the same
  ## pipeline run) - R then dispatches through S4 method lookup instead of
  ## calling the plain `data.table::shift()` function this code actually
  ## wants, and there is no S4 method registered for a bare numeric
  ## vector, so it errors instead of lagging the vector. Explicitly
  ## namespaced to remove the ambiguity - this is a lag-by-one on a plain
  ## numeric vector, nothing spatial.
  survival <- lx / data.table::shift(lx, fill = 1)
  mean(-log(survival), na.rm = TRUE)
}

MAMMAL_SURROGATE_DEFAULT <- data.table(
  Family = c("Phocidae", "Monachidae", "Otariidae", "Delphinidae", "Ziphiidae", "Physeteridae"),
  surrogate_type = c(2, 2, 2, 3, 3, 3)
)

calc_mammal <- function(out) {
  out <- merge(out, MAMMAL_SURROGATE_DEFAULT, by = "Family", all.x = TRUE)
  out[is.na(surrogate_type), surrogate_type := 2]
  
  ## Method 1: Barlow & Boveng (1991) Siler survivorship - needs Longevity
  out[, PB_BarlowBoveng_1991 := mapply(siler_pb, Longevity, surrogate_type)]
  ## Method 2: Gascuel general (Eq.23) - only needs trophic level + temp,
  ## computable independently regardless of Longevity availability
  out[, PB_Gascuel_2008 := 20.19 * TrophicLevel^(-3.26) * exp(0.041 * Temp)]
  
  ## Fill gaps in BOTH PB methods by borrowing from the closest
  ## taxonomic relative (local first, external FishBase/SeaLifeBase
  ## donors if no local match) - PB_Gascuel_2008 was previously left
  ## unfilled, meaning it silently failed whenever TrophicLevel was
  ## missing for a species with no genus/family-mate in your own
  ## species_df, even though it's the ONLY fallback once Siler fails
  out <- fill_by_taxonomic_proximity(out, "PB_BarlowBoveng_1991")
  out <- fill_by_taxonomic_proximity(out, "PB_Gascuel_2008")
  
  mammal_pb_labels <- c(PB_BarlowBoveng_1991 = "Barlow & Boveng 1991 (Siler)",
                        PB_Gascuel_2008 = "Gascuel 2008 general (Eq.23)")
  out[, PB := select_chosen(out, names(mammal_pb_labels))]
  out[, PB_method := describe_chosen(out, mammal_pb_labels)]
  
  ## Method 1: Innes/Trites (Eq.31) - needs MaxWeight
  out[, W_kg := MaxWeight / 1000]
  out[, QB_InnesTrites_1997 := (0.1 * W_kg^0.8 / W_kg) * 365]
  out <- fill_by_taxonomic_proximity(out, "QB_InnesTrites_1997")
  ## Method 2: Q/P=3 fallback, using chosen PB
  out[, QB_ChristensenEtAl_2008 := 3 * PB]
  
  mammal_qb_labels <- c(QB_InnesTrites_1997 = "Innes/Trites 1987/1997 (Eq.31)",
                        QB_ChristensenEtAl_2008 = "Q/P=3 (Christensen et al. 2008)")
  out[, QB := select_chosen(out, names(mammal_qb_labels))]
  out[, QB_method := describe_chosen(out, mammal_qb_labels)]
  
  out[, .(Species, FG, FG_name, Biomass, dispatch_group,
          PB_BarlowBoveng_1991, PB_Gascuel_2008, PB, PB_method,
          QB_InnesTrites_1997, QB_ChristensenEtAl_2008, QB, QB_method)]
}

## =================================================================
## SEABIRDS: Gascuel general PB (only automatable option - no
## dedicated seabird PB method exists in the protocol); Nilsson &
## Nilsson QB, and Q/P=3 as an independent second QB method
## =================================================================

calc_seabird <- function(out) {
  out[, PB_Gascuel_2008 := 20.19 * TrophicLevel^(-3.26) * exp(0.041 * Temp)]
  ## this is the ONLY PB method for seabirds (no dedicated equation in
  ## the protocol) - previously left unfilled, so any species missing
  ## TrophicLevel got NO PB at all, with nothing else to fall back to
  out <- fill_by_taxonomic_proximity(out, "PB_Gascuel_2008")
  out[, PB := PB_Gascuel_2008]
  out[, PB_method := fifelse(!is.na(PB), "Gascuel 2008 general (Eq.23) - no dedicated seabird PB method in protocol", NA_character_)]
  
  ## Method 1: Nilsson & Nilsson (1976) - needs MaxWeight
  out[, logDR := -0.293 + 0.85 * log10(MaxWeight)]
  out[, QB_NilssonNilsson_1976 := (10^logDR / MaxWeight) * 365]
  out <- fill_by_taxonomic_proximity(out, "QB_NilssonNilsson_1976")
  
  ## NOTE: deliberately NO Q/P=3 fallback here. That heuristic is
  ## derived largely from fish/ectotherm biology - birds are endotherms
  ## with much higher metabolic costs, so a fixed 3x ratio would likely
  ## badly underestimate seabird QB rather than serve as a reasonable
  ## fallback. If Nilsson & Nilsson fails (missing MaxWeight even after
  ## gap-filling), QB stays NA for that species rather than guessing.
  out[, QB := QB_NilssonNilsson_1976]
  out[, QB_method := fifelse(!is.na(QB_NilssonNilsson_1976), "Nilsson & Nilsson 1976 (Eq.30)", NA_character_)]
  
  out[, .(Species, FG, FG_name, Biomass, dispatch_group, PB_Gascuel_2008, PB, PB_method,
          QB_NilssonNilsson_1976, QB, QB_method)]
}

## =================================================================
## INVERTEBRATES: Tumbiolo & Downing (1994) and Gascuel general (2008)
## as two independent PB methods; Q/P=3 for QB (only automatable
## option - see the Brey 2012 manual-review flag further down for the
## more accurate but non-automatable alternative)
## =================================================================

calc_invertebrate <- function(out) {
  out[, logP := 0.24 + 0.96 * log10(Biomass) - 0.21 * log10(MaxWeight) +
        0.03 * Temp - 0.16 * log10(Depth + 1)]
  out[, PB_TumbioloDowning_1994 := (10^logP) / Biomass]
  out[, PB_Gascuel_2008 := 20.19 * TrophicLevel^(-3.26) * exp(0.041 * Temp)]
  
  ## Method 3: Brey (1999), protocol Eq.19 - log(P/B) = 1.672 +
  ## 0.993*log(1/Amax) - 0.035*log(Mmax) - 300.447*(1/(T+273)).
  ## Genuinely different inputs than the other two methods: max age
  ## (Amax) and max body mass in KJ (Mmax), not Depth/Biomass or
  ## TrophicLevel - so this can succeed for species where BOTH other
  ## methods fail due to missing Depth/TrophicLevel, as long as max
  ## age or max weight data exists.
  ## Amax reuses the Longevity field (max age, years) already fetched
  ## for other groups. Mmax needs body mass in KJ, not grams - converting
  ## requires an energy-density constant that genuinely varies by tissue
  ## type (roughly 4-24 KJ/g across taxa). Using ~4.5 KJ/g as a rough
  ## wet-weight marine invertebrate estimate (commonly cited order of
  ## magnitude in the literature) - this is an approximation, not a
  ## precise per-taxon value, and adds real uncertainty on top of the
  ## formula itself. Flagged here so it's not mistaken for an exact input.
  ENERGY_DENSITY_KJ_PER_G <- 4.5
  out[, Mmax_KJ := MaxWeight * ENERGY_DENSITY_KJ_PER_G]
  out[, PB_Brey_1999 := 10^(1.672 + 0.993 * log10(1 / Longevity) -
                              0.035 * log10(Mmax_KJ) - 300.447 * (1 / (Temp + 273)))]
  
  ## PB_TumbioloDowning_1994 can only ever be "direct" for a species that HAS its
  ## own MaxWeight+Depth - if none of your invertebrates have that
  ## (common: SeaLifeBase's MaxWeight coverage for inverts is patchy),
  ## the local donor pool for THIS method is permanently empty and
  ## local-only filling can't help.
  out <- fill_by_taxonomic_proximity(out, "PB_TumbioloDowning_1994", allow_external = FALSE)
  out <- fill_by_taxonomic_proximity(out, "PB_Gascuel_2008")
  out <- fill_by_taxonomic_proximity(out, "PB_Brey_1999", allow_external = FALSE)
  
  invert_pb_labels <- c(PB_TumbioloDowning_1994 = "Tumbiolo & Downing 1994 (Eq.17)",
                        PB_Brey_1999 = "Brey 1999 (Eq.19)",
                        PB_Gascuel_2008 = "Gascuel 2008 general fallback (Eq.23)")
  out[, PB := select_chosen(out, names(invert_pb_labels))]
  out[, PB_method := describe_chosen(out, invert_pb_labels)]
  
  out[, QB_ChristensenEtAl_2008 := 3 * PB]
  out[, QB := QB_ChristensenEtAl_2008]
  out[, QB_method := fifelse(!is.na(QB), "Q/P=3 (Christensen et al. 2008)", NA_character_)]
  
  out[, .(Species, FG, FG_name, Biomass, dispatch_group, PB_TumbioloDowning_1994, PB_Brey_1999, PB_Gascuel_2008, PB, PB_method,
          QB_ChristensenEtAl_2008, QB, QB_method)]
}

## =================================================================
## RUN
## =================================================================

results <- rbindlist(list(
  if (nrow(species_df[dispatch_group == "fish"]) > 0) calc_fish(species_df[dispatch_group == "fish"]),
  if (nrow(species_df[dispatch_group == "mammal"]) > 0) calc_mammal(species_df[dispatch_group == "mammal"]),
  if (nrow(species_df[dispatch_group == "seabird"]) > 0) calc_seabird(species_df[dispatch_group == "seabird"]),
  if (nrow(species_df[dispatch_group == "invertebrate"]) > 0) calc_invertebrate(species_df[dispatch_group == "invertebrate"])
), fill = TRUE)
invisible(STAGE_PB$tick(tokens = list(stage_name = "Calculate PB/QB (all groups)")))

## Merge the raw traits (Loo/K/Winf/Longevity/TrophicLevel/AspectRatio/
## Depth/Temp) back onto `results` by Species - calc_fish()/
## calc_mammal()/calc_seabird()/calc_invertebrate() each select down
## to a narrow PB/QB-only column set (see their own final `out[, .(...)]`
## lines above), so these don't survive the rbindlist() above on their
## own. Needed for the FG-level trait aggregation right below (feeds
## the Ecopath_traits summary sheet, added 2026-09-16) -
## PB/QB themselves are untouched by this, it only adds columns.
trait_cols <- intersect(c("Loo", "K", "Winf", "Longevity", "TrophicLevel", "AspectRatio", "Depth", "Temp"), names(species_df))
if (length(trait_cols) > 0) {
  results <- merge(results, unique(species_df[, c("Species", trait_cols), with = FALSE], by = "Species"),
                   by = "Species", all.x = TRUE)
} else {
  message("[Ecopath_traits] None of Loo/K/Winf/Longevity/TrophicLevel/AspectRatio/Depth/Temp found in species_df -",
          " Ecopath_traits will be skipped further down.")
}

phyto_flagged <- species_df[dispatch_group == "phytoplankton",
                            .(Species, FG, Biomass, note = "phytoplankton - needs separate production sampling data, not computed here")]

## --- Brey (2012) manual-review flag ----------------------------------
## Brey's ANN model is the current best-practice standard for benthic
## invertebrate P/B (multi-parameter neural network, 1252 training
## datasets - a real improvement over Tumbiolo & Downing 1994 used
## above), but it's a trained network with no published weights or
## maintained R package I could find - not something to fake an
## approximation of and mislabel. Instead: flag the highest-biomass
## invertebrate species (where getting P/B right matters most) for
## manual cross-check against Brey's own calculator, rather than
## silently leaving this as a known gap across the whole list.
brey_candidates <- results[dispatch_group == "invertebrate"][order(-Biomass)][1:min(10, .N)]
message("\nTop invertebrate species by biomass - worth a manual Brey (2012) cross-check",
        " (see Thomas Brey's Virtual Handbook calculator) since that model is more",
        " accurate than the Tumbiolo & Downing/Gascuel fallback used here, but isn't",
        " automatable (no accessible weights or R package):")
print(brey_candidates[, .(Species, FG, Biomass, PB, PB_method)])
fwrite(brey_candidates, file.path(csv_out_dir, "invertebrates_for_brey_manual_check.csv"))

message("\n=== Species-level PB/QB - which method was CHOSEN for FG weighting ===")
print(results[, .N, by = PB_method])
print(results[, .N, by = QB_method])

message("\n=== Direct vs. taxonomically-borrowed values ===")
source_cols <- grep("_source$", names(results), value = TRUE)
if (length(source_cols) > 0) {
  for (col in source_cols) {
    tab <- results[, .N, by = col]
    setnames(tab, col, "status")
    message(sub("_source$", "", col), ":")
    print(tab[order(status)])
  }
  message("Borrowed values are tagged in the *_source columns in the CSV export -",
          " treat these as weaker evidence than direct computations, especially",
          " anything borrowed at Order or Class level (very broad relatives).")
}

message("\n=== Coverage of EVERY individual method attempted (not just the chosen one) ===")
message("Fish PB methods:")
print(results[dispatch_group == "fish", .(
  Pauly = sum(!is.na(PB_Pauly_1980)), FishLife = sum(!is.na(PB_FishLife_2023)), Gascuel = sum(!is.na(PB_Gascuel_2008)),
  Hoenig = sum(!is.na(PB_Hoenig_1983)), Then2015 = sum(!is.na(PB_Then_2015)), AlversonCarney = sum(!is.na(PB_AlversonCarney_1975))
)])
message("Fish QB methods:")
print(results[dispatch_group == "fish", .(
  PalomaresPauly_Z = sum(!is.na(QB_PalomaresPauly_1998Z)), PalomaresPauly_noZ = sum(!is.na(QB_PalomaresPauly_1998noZ)),
  ChristensenPauly_1992 = sum(!is.na(QB_ChristensenPauly_1992)), QP3 = sum(!is.na(QB_ChristensenEtAl_2008))
)])
message("These per-method columns (PB_Pauly_1980, PB_FishLife_2023, PB_Gascuel_2008, QB_PalomaresPauly_1998Z, QB_PalomaresPauly_1998noZ,",
        " QB_ChristensenPauly_1992, QB_ChristensenEtAl_2008, etc.) are all preserved in species_pb_qb_by_taxon_group.csv -",
        " worth comparing them directly for any species where the methods disagree a lot,",
        " since that's a more useful signal than trusting whichever one happened to be chosen.")

## =================================================================
## Biomass-weighted average up to Functional Group level
## =================================================================

## True total biomass per FG - computed from ALL species regardless of
## whether PB/QB were successfully computed, since Biomass is directly
## observed input data, not dependent on the PB/QB calculation succeeding
fg_biomass_total <- results[, .(Biomass_FG = sum(Biomass, na.rm = TRUE)), by = FG]

fg_weighted <- results[!is.na(PB) | !is.na(QB), .(
  PB_FG = sum(Biomass * PB, na.rm = TRUE) / sum(Biomass[!is.na(PB)], na.rm = TRUE),
  QB_FG = sum(Biomass * QB, na.rm = TRUE) / sum(Biomass[!is.na(QB)], na.rm = TRUE),
  n_species_with_PB = sum(!is.na(PB)),
  n_species_with_QB = sum(!is.na(QB)),
  n_species_total = .N,
  biomass_coverage_PB = sum(Biomass[!is.na(PB)], na.rm = TRUE) / sum(Biomass, na.rm = TRUE),
  biomass_coverage_QB = sum(Biomass[!is.na(QB)], na.rm = TRUE) / sum(Biomass, na.rm = TRUE),
  ## F coverage - separate from PB/QB coverage above, since a species
  ## can have a perfectly good PB estimate that's still M-only (Fmort
  ## NA). Tracked explicitly so a fully-M-only FG is visible here
  ## rather than looking identical to one where F was genuinely zero.
  n_species_with_F = sum(!is.na(Fmort)),
  biomass_coverage_F = sum(Biomass[!is.na(Fmort)], na.rm = TRUE) / sum(Biomass, na.rm = TRUE)
), by = FG]
fg_weighted <- merge(fg_biomass_total, fg_weighted, by = "FG", all.x = TRUE)

## FG_name attached here (fg_weighted was FG-num-only up to this point) -
## needed below both for readability and as the join key for the
## EcoBase literature merge (EcoBase has no FG_num of its own, only
## group names, so FG_name has to exist on this table to match against).
fg_name_lookup <- unique(results[!is.na(FG_name), .(FG, FG_name)])
fg_weighted <- merge(fg_weighted, fg_name_lookup, by = "FG", all.x = TRUE)

## =================================================================
## Biomass-weighted FG-level TRAITS (feeds the Ecopath_traits summary
## sheet, added 2026-09-16) - same weighting convention as
## PB_FG/QB_FG just above: each species' trait value contributes in
## proportion to its own share of the FG's total Biomass, not a plain
## unweighted mean across however many species happen to be in the FG.
## =================================================================
fg_traits_weighted <- data.table(FG = fg_weighted$FG)
if (length(trait_cols) > 0) {
  weighted_trait_mean <- function(fg_id, col) {
    sub <- results[FG == fg_id & !is.na(get(col)) & !is.na(Biomass) & Biomass > 0]
    if (nrow(sub) == 0) return(NA_real_)
    sum(sub$Biomass * sub[[col]]) / sum(sub$Biomass)
  }
  for (col in trait_cols) {
    fg_traits_weighted[, (paste0(col, "_FG")) := vapply(FG, weighted_trait_mean, numeric(1), col = col)]
  }
}
fg_traits_weighted <- merge(fg_traits_weighted, fg_name_lookup, by = "FG", all.x = TRUE)
setnames(fg_traits_weighted, "FG", "FG_num")

## Same reason as add_pbqb_to_ecopath_workbook()'s PB_QB expansion just
## above: fg_traits_weighted so far only has rows for FGs fg_weighted
## happened to cover this run, but Ecopath_traits (like Ecopath_PBQB)
## feeds straight into EwE, which needs one row per model FG regardless
## of whether this particular run had trait data for it.
full_fg_ref_traits <- read_full_fg_reference(ECOPATH_WORKBOOK_PATH, csv_dir = BIOMASS_CSV_DIR)
if (!is.null(full_fg_ref_traits)) {
  n_before_traits <- nrow(fg_traits_weighted)
  fg_traits_weighted <- merge(full_fg_ref_traits, fg_traits_weighted, by = "FG_num", all.x = TRUE, suffixes = c("_ref", ""))
  fg_traits_weighted[is.na(FG_name), FG_name := FG_name_ref]
  fg_traits_weighted[, FG_name_ref := NULL]
  n_added_traits <- nrow(fg_traits_weighted) - n_before_traits
  if (n_added_traits > 0) {
    message("Expanded Ecopath_traits from ", n_before_traits, " to ", nrow(fg_traits_weighted),
            " row(s) using the full FG reference already in the workbook - ", n_added_traits,
            " FG(s) had no trait data this run and are included with blank trait columns rather",
            " than omitted.")
  }
} else {
  message("No 'FG'/'FG_lookup' sheet found yet in ", ECOPATH_WORKBOOK_PATH, " to expand Ecopath_traits",
          " against - it will only cover the FG(s) with trait data in THIS run. Run 01_biomass.R",
          " against this same workbook first to guarantee every model FG gets a row here.")
}

setcolorder(fg_traits_weighted, c("FG_num", "FG_name", setdiff(names(fg_traits_weighted), c("FG_num", "FG_name"))))
setorder(fg_traits_weighted, FG_num)
message("\n=== FG-level traits (biomass-weighted average) - feeds Ecopath_traits ===")
print(fg_traits_weighted)

message("\n=== FG-level PB/QB (biomass-weighted average) ===")
print(fg_weighted[order(FG)])

message("\nFGs with LOW biomass coverage (<50%):")
print(fg_weighted[biomass_coverage_PB < 0.5 | biomass_coverage_QB < 0.5,
                  .(FG, biomass_coverage_PB, biomass_coverage_QB, n_species_total)])

## FGs with NO fishing mortality data at all - their PB above is
## M-only, not M+F, for every species in the group. Printed
## unconditionally (not just when YIELD_SOURCE == "none") since even
## with a real landings source attached, individual FGs can still end
## up with zero matched species.
n_fg_no_F <- fg_weighted[n_species_with_F == 0, .N]
if (n_fg_no_F > 0) {
  message("\n", n_fg_no_F, " of ", nrow(fg_weighted), " FG(s) have ZERO species with a Fmort",
          " value - PB_FG for these is NATURAL MORTALITY (M) ONLY, which underestimates",
          " true PB for anything actually fished (a real biomass-removal rate silently",
          " treated as 0 rather than unknown). Review before using PB_FG for these",
          " FGs as-is, especially any that are commercially targeted:")
  print(fg_weighted[n_species_with_F == 0, .(FG, FG_name, PB_FG, n_species_total)])
}
invisible(STAGE_PB$tick(tokens = list(stage_name = "Aggregate to FG level")))

## =================================================================
## Export
## =================================================================

fwrite(results, file.path(csv_out_dir, "species_pb_qb_by_taxon_group.csv"))
fwrite(fg_weighted, file.path(csv_out_dir, "fg_pb_qb_weighted.csv"))
fwrite(phyto_flagged, file.path(csv_out_dir, "phytoplankton_needs_separate_method.csv"))

## =================================================================
## Supplement with EcoBase literature values (03b_ecobase.R output)
## =================================================================
## 03b_ecobase.R's fetch_ecobase_literature_pb_qb() is called directly
## here (rather than expecting ecobase_literature_pb_qb_simple.csv to
## already exist from a separate manual run) - it produces PB/QB from
## PUBLISHED Ecopath models, one row per FG_name per source model.
## Matched here on FG_name (EcoBase has no FG_num of its own - group
## naming won't line up automatically across different models' own
## definitions, so this is a text match and should be spot-checked,
## not trusted blindly). Where a model_id column exists, values are
## averaged across all matching EcoBase models per FG_name first, so
## one FG doesn't get weighted toward whichever model happened to have
## the most rows.
ECOBASE_CSV_PATH <- file.path(csv_out_dir, "ecobase_literature_pb_qb_simple.csv")

if (ENABLE_ECOBASE_QUERY) {
  ## 2026-09-23 fix: this was passing the top-level out_dir, so every
  ## ecobase_*.csv (and the raw XML dumps) landed one level up from
  ## where everything else in this script writes - while ECOBASE_CSV_PATH
  ## just above already (correctly) pointed at csv_out_dir (the pbqb
  ## subfolder). That mismatch meant a successful query's own CSV was
  ## never found by the read-back check right below. Now both point at
  ## csv_out_dir, so the EcoBase CSVs land inside the pbqb subfolder
  ## alongside this script's other output.
  ## 2026-09-24, per Andrea: EcoBase should consider each candidate
  ## model's OWN reference year, not treat every matching model as
  ## equally relevant - target_year narrows the "simple" CSV below to
  ## whichever model is closest to this model's own YEAR_ECOPATH per
  ## FG_name (see fetch_ecobase_literature_pb_qb()'s own header comment;
  ## the full multi-model detail is still written to
  ## ecobase_literature_pb_qb_full.csv either way).
  fetch_ecobase_literature_pb_qb(out_dir = csv_out_dir, force_refresh = ECOBASE_FORCE_REFRESH,
                                 target_year = round(mean(YEAR_ECOPATH)))
} else {
  message("ENABLE_ECOBASE_QUERY = FALSE - skipping the EcoBase literature query entirely.",
          " Continuing with empirical PB/QB only",
          if (file.exists(ECOBASE_CSV_PATH)) " (a cached ecobase_literature_pb_qb_simple.csv exists but will NOT be read while this is FALSE)." else ".")
}

if (ENABLE_ECOBASE_QUERY && file.exists(ECOBASE_CSV_PATH)) {
  ecobase_raw <- fread(ECOBASE_CSV_PATH)
  message("\nLoaded EcoBase literature PB/QB: ", nrow(ecobase_raw), " rows across ",
          uniqueN(ecobase_raw$FG_name), " distinct FG_name values from ",
          uniqueN(ecobase_raw$EwE_model), " published model(s).")
  
  ecobase_by_fgname <- ecobase_raw[, .(
    PB_ecobase = mean(PB, na.rm = TRUE),
    QB_ecobase = mean(QB, na.rm = TRUE),
    n_ecobase_models = uniqueN(EwE_model[!is.na(PB) | !is.na(QB)]),
    ## one "Model (authors, year)" entry per contributing model,
    ## semicolon-separated - so the Ecobase sheet's PB_ecobase/QB_ecobase
    ## average is directly traceable to what it's an average OF, not
    ## just how many models it came from. Deduplicated on the model/
    ## authors/year combination itself (not just EwE_model) in case the
    ## same model name appears with genuinely different author/year
    ## metadata across rows.
    References = paste(
      unique(sprintf("%s (%s, %s)",
                     fifelse(is.na(EwE_model), "unknown model", as.character(EwE_model)),
                     fifelse(is.na(authors), "authors unknown", as.character(authors)),
                     fifelse(is.na(year), "year unknown", as.character(year)))),
      collapse = "; ")
  ), by = FG_name]
  
  fg_weighted_ecobase <- merge(fg_weighted, ecobase_by_fgname, by = "FG_name", all.x = TRUE)
  
  n_matched <- fg_weighted_ecobase[!is.na(PB_ecobase) | !is.na(QB_ecobase), .N]
  message(n_matched, " of ", nrow(fg_weighted_ecobase), " FGs matched an EcoBase FG_name -",
          " unmatched FGs likely need a manual name alignment (check FG_name spelling/",
          " wording against ecobase_literature_pb_qb_raw.csv's group_name values),",
          " not necessarily a genuine absence in the literature.")
  
  ## Divergence flag - a >2x difference between the empirical estimate
  ## and the literature value is worth a manual look (not automatically
  ## "wrong" - real ecosystems differ - but worth checking before trusting
  ## either one blindly), not silently averaged together.
  fg_weighted_ecobase[, PB_ratio := PB_FG / PB_ecobase]
  fg_weighted_ecobase[, QB_ratio := QB_FG / QB_ecobase]
  fg_weighted_ecobase[, PB_diverges := !is.na(PB_ratio) & (PB_ratio > 2 | PB_ratio < 0.5)]
  fg_weighted_ecobase[, QB_diverges := !is.na(QB_ratio) & (QB_ratio > 2 | QB_ratio < 0.5)]
  
  if (fg_weighted_ecobase[PB_diverges == TRUE | QB_diverges == TRUE, .N] > 0) {
    message("\nFGs where empirical and EcoBase-literature PB/QB differ by more than 2x",
            " (review before trusting either value for these):")
    print(fg_weighted_ecobase[PB_diverges == TRUE | QB_diverges == TRUE,
                              .(FG, FG_name, PB_FG, PB_ecobase, QB_FG, QB_ecobase)])
  }
  
  ## Gap-fill: only for FGs where the empirical method produced NOTHING
  ## (e.g. no species matched, or the taxon-specific method genuinely
  ## doesn't apply) - never overwrites an existing empirical estimate,
  ## since fg_weighted's own biomass-weighted species-level calculation
  ## is more directly tied to this specific model's own species
  ## composition than a borrowed literature value.
  fg_weighted_ecobase[, PB_FG_filled := fifelse(is.na(PB_FG), PB_ecobase, PB_FG)]
  fg_weighted_ecobase[, QB_FG_filled := fifelse(is.na(QB_FG), QB_ecobase, QB_FG)]
  fg_weighted_ecobase[, PB_source := fifelse(is.na(PB_FG), "EcoBase (literature)", "empirical")]
  fg_weighted_ecobase[, QB_source := fifelse(is.na(QB_FG), "EcoBase (literature)", "empirical")]
  
  n_pb_filled <- fg_weighted_ecobase[PB_source == "EcoBase (literature)" & !is.na(PB_FG_filled), .N]
  n_qb_filled <- fg_weighted_ecobase[QB_source == "EcoBase (literature)" & !is.na(QB_FG_filled), .N]
  message(n_pb_filled, " FG(s) had PB gap-filled from EcoBase; ",
          n_qb_filled, " FG(s) had QB gap-filled from EcoBase.",
          " PB_FG/QB_FG above are left as the pure empirical estimate (NA where absent) -",
          " PB_FG_filled/QB_FG_filled are what's recommended for the Ecopath basic input",
          " where an empirical estimate wasn't available.")
  
  fwrite(fg_weighted_ecobase, file.path(csv_out_dir, "fg_pb_qb_weighted_with_ecobase.csv"))
  message("Saved fg_pb_qb_weighted_with_ecobase.csv.")
  
  ## Dedicated Ecobase sheet in the shared workbook - the raw per-FG
  ## literature values on their own, for audit/comparison, separate
  ## from PB_QB's gap-filled result (which only shows where EcoBase
  ## was actually USED to fill a gap, not every FG it has a value for).
  ecobase_sheet <- merge(fg_name_lookup, ecobase_by_fgname, by = "FG_name", all.x = TRUE)
  setorder(ecobase_sheet, FG)
  setnames(ecobase_sheet, "FG", "FG_num")
  
  ## Ecobase_by_Model - the SAME literature values, but one row per
  ## FG x MODEL rather than ecobase_sheet's one row per FG (which only
  ## keeps an averaged PB_ecobase/QB_ecobase plus a squished References
  ## text string). Every FG here typically has MULTIPLE published
  ## EcoBase models behind it - this sheet is what actually shows that
  ## FG-and-model granularity (which specific model, which year/authors,
  ## that model's own PB/QB for this FG), rather than only the average
  ## ecobase_sheet reduces it to. Matched on FG_name, same caveat as
  ## ecobase_by_fgname above (text match, not a real FG_num join -
  ## EcoBase has no FG_num of its own).
  ecobase_by_model <- merge(fg_name_lookup, ecobase_raw, by = "FG_name", all.x = FALSE)
  ecobase_by_model <- ecobase_by_model[, .(FG_num = FG, FG_name, EwE_model, year, authors, PB, QB)]
  setorder(ecobase_by_model, FG_num, EwE_model)
  message(nrow(ecobase_by_model), " FG x model row(s) written to the Ecobase_by_Model sheet",
          " (", uniqueN(ecobase_by_model$FG_num), " FG(s) x up to ",
          uniqueN(ecobase_by_model$EwE_model), " distinct published model(s) - the per-model",
          " detail behind Ecobase's own FG-level PB_ecobase/QB_ecobase averages).")
  
  ## 2026-09-17 update, CSV-not-workbook-sheet refactor:
  ## Ecobase/Ecobase_by_Model are native/intermediate reference tables,
  ## not final target sheets - written as CSV only.
  write_native_sheets_csv(list(Ecobase = ecobase_sheet, Ecobase_by_Model = ecobase_by_model),
                          csv_out_dir)
  
  ## downstream Ecopath export (below) uses the gap-filled values so FGs
  ## with no empirical estimate aren't just left blank when a literature
  ## value was available
  fg_weighted <- copy(fg_weighted_ecobase)
  fg_weighted[, `:=`(PB_FG = PB_FG_filled, QB_FG = QB_FG_filled)]
} else if (ENABLE_ECOBASE_QUERY) {
  message("\nNo usable EcoBase literature file at ", ECOBASE_CSV_PATH,
          " after querying EcoBase (see messages above for why - e.g. no network,",
          " or no models returned usable data). Continuing with empirical",
          " estimates only.")
}

message("\nSaved: species_pb_qb_by_taxon_group.csv, fg_pb_qb_weighted.csv,",
        " phytoplankton_needs_separate_method.csv")

## =================================================================
## FG-level fishing mortality (F) - applied to fg_weighted's PB_FG
## =================================================================
## fg_yield_density was already loaded EARLY (right after species_df's
## Yield block, before dispatch) so calc_fish() could use it as a
## per-species F fallback - see that section for FG_YIELD_SOURCE/
## FG_CATCH_CSV_PATH and why F is a per-FG rate. Reused here rather
## than re-reading the same file a second time.
##
## In practice this section now mainly matters for NON-fish FGs
## (mammal/seabird/invertebrate) - those dispatch functions don't
## compute a species-level Fmort at all, so their species never have
## n_species_with_F > 0, and this correctly applies F_FG on top of
## their M-only PB_FG. Fish FGs already got F baked in at the species
## level above (in calc_fish, before this PB_FG aggregate was even
## built) - the same double-count guard below detects that
## (n_species_with_F > 0 for those FGs) and correctly SKIPS them here,
## so nothing is added twice.
if (FG_YIELD_SOURCE == "fg_catch_csv" && exists("fg_yield_density")) {
  fg_yield_for_merge <- copy(fg_yield_density)
  setnames(fg_yield_for_merge, "FG_num", "FG")
  
  fg_weighted <- merge(fg_weighted, fg_yield_for_merge, by = "FG", all.x = TRUE)
  fg_weighted[, F_FG := Yield_FG / Biomass_FG]
  
  already_has_species_F <- fg_weighted[n_species_with_F > 0 & !is.na(F_FG), .N]
  if (already_has_species_F > 0) {
    message("\n", already_has_species_F, " FG(s) already have species-level F baked into",
            " PB_FG (from calc_fish()'s Fmort fallback) - FG-level F is NOT added again",
            " for these, to avoid double-counting the same fishing removal:")
    print(fg_weighted[n_species_with_F > 0 & !is.na(F_FG), .(FG, FG_name, n_species_with_F, F_FG)])
  }
  
  fg_weighted[, PB_FG_before_F := PB_FG]
  fg_weighted[n_species_with_F == 0 & !is.na(F_FG), PB_FG := PB_FG_before_F + F_FG]
  
  n_fg_with_F <- fg_weighted[n_species_with_F == 0 & !is.na(F_FG), .N]
  message(n_fg_with_F, " of ", nrow(fg_weighted), " FG(s) had FG-level F added here",
          " (typically non-fish groups; fish FGs got F earlier, at the species level).",
          " PB_FG_before_F keeps the pre-F value for comparison. FGs with no catch",
          " matched at all stay M-only - if one of those is commercially fished, check",
          " species_fg_matched.csv for an 'unresolved' status or a naming mismatch.")
  
  fwrite(fg_weighted, file.path(csv_out_dir, "fg_pb_qb_weighted_with_F.csv"))
  message("Saved fg_pb_qb_weighted_with_F.csv.")
} else {
  message("\nFG_YIELD_SOURCE = 'none' (or catch data wasn't found earlier) - fg_weighted's",
          " PB stays NATURAL MORTALITY (M) ONLY for every FG (M+F not applied anywhere).",
          " Set FG_YIELD_SOURCE <- 'fg_catch_csv' near species_df's Yield loading, once",
          " 02_fao_catches.R has been run - until then, treat every PB value below as a",
          " lower bound for any FG that's actually fished.")
}

## =================================================================
## Add PB_QB (+ PB_QB_spp) to output/ecopath_ecosim_inputs.xlsx
##
## Same shared workbook 01_biomass.R (Biomass sheets) and
## 02_fao_catches.R (Catches sheets) write to, via the same order-
## independent upsert - this can run before, after, or between those
## two scripts. fg_weighted here reflects whichever of the EcoBase-fill
## and FG-level-F steps above actually ran (or neither), so re-running
## this after changing either toggle updates the PB_QB sheet in place.
##
## `results` (species-level, built earlier by the calc_fish()/calc_
## mammal()/calc_seabird()/calc_invertebrate() dispatch and rbindlist'd
## above) is passed as species_pb_qb so the workbook also gets a
## PB_QB_spp sheet - one row per species, with the proportion of each
## FG's biomass it represents - rather than PB_QB's FG-level rollup
## being the only place PB/QB shows up in the workbook. Without this,
## the only place to see which species (and how much of each FG) went
## into PB_FG/QB_FG is species_pb_qb_by_taxon_group.csv, outside the
## workbook entirely.
## =================================================================
add_pbqb_to_ecopath_workbook(fg_weighted = fg_weighted, out_path = ECOPATH_WORKBOOK_PATH,
                             species_pb_qb = results,
                             csv_out_dir = csv_out_dir, biomass_csv_dir = BIOMASS_CSV_DIR)

## =================================================================
## Final-sheet consolidation (2026-09-17, revised): the excel file's
## Ecopath_traits sheet is the FG_name/species-level traits table "as
## it was saved" - 01_biomass.R writes that directly (from
## traits_reconciled, see its STEP 12). This script's OWN
## fg_traits_weighted table (biomass-weighted average of those same
## traits, rolled up to one row per FG) is a different, coarser shape -
## useful for review, but not the final Ecopath_traits sheet, and
## writing it under that same sheet name here would just overwrite
## whatever 01_biomass.R already wrote depending on run order. Written
## as an audit-only CSV instead. The other final sheets this script can
## help complete (Ecopath_L, Ecopath_Di, Ecosim_ts) are consolidated
## from sheets 01_biomass.R/02_fisheries.R/this script already wrote
## natively, via finalize_ecopath_ecosim_summary_sheets()
## (lib_survey_fg_density_functions.R) - additive, every native sheet
## (Ecopath, Catches_Ecopath, Catches_Discards_FG_ts, PB_QB, Ecosim,
## Catches_Ecosim, Fishing_Effort_by_Fleet) stays in the workbook
## untouched. Run this LAST, same rule as finalize_workbook_sheet_
## order() - after 01_biomass.R and 02_fisheries.R have both run at
## least once against this same out_path (a sheet whose source hasn't
## run yet is simply skipped with a message, not an error).
## =================================================================
write_native_sheet_csv(fg_traits_weighted, "fg_traits_weighted", csv_out_dir)
finalize_ecopath_ecosim_summary_sheets(out_path = ECOPATH_WORKBOOK_PATH, year_ecopath = YEAR_ECOPATH,
                                       biomass_csv_dir = BIOMASS_CSV_DIR, fisheries_csv_dir = FISHERIES_CSV_DIR)

## Refresh the "Ecobase" comparison sheet (03b_ecobase.R) now that this
## script's own PB/QB query above may have added rows the raw cache
## didn't have when 01_biomass.R first wrote this sheet (or added it for
## the first time, if ENABLE_ECOBASE_BIOMASS_QUERY was FALSE there).
ecobase_sheet_dt <- build_ecobase_sheet_dt(csv_out_dir, target_year = round(mean(YEAR_ECOPATH)))
if (!is.null(ecobase_sheet_dt) && nrow(ecobase_sheet_dt) > 0) {
  upsert_workbook_sheets(list(Ecobase = ecobase_sheet_dt), ECOPATH_WORKBOOK_PATH)
}

## 2026-09-17 update: the excel ecopath_ecosim file must have exactly
## the intended sheets, trimmed script by script - trim the workbook
## down to EXACTLY whichever of the 9 final target sheets (info,
## FG_spp, Ecopath_B, Ecopath_L, Ecopath_Di, Ecopath_PBQB,
## Ecopath_traits, Ecopath_diet, Ecosim_ts) exist at this point in the
## pipeline - drops every native/intermediate sheet (all CSV-only now).
## Safe/idempotent to call again here even if 01_biomass.R/
## 02_fisheries.R already trimmed it earlier this session.
trim_workbook_to_final_sheets(ECOPATH_WORKBOOK_PATH)

## =================================================================
## Method comparison plots - fish only, since that's the group with
## the most independent methods (6 for PB, 4 for QB) and therefore the
## most informative to compare. One point per method per species,
## faceted by species so you can see at a glance how much the methods
## agree or disagree - the CHOSEN method (the one actually used for the
## FG-weighted average) is highlighted distinctly from the rest.
## =================================================================

fish_results <- results[dispatch_group == "fish"]

## --- PB methods ---------------------------------------------------------

pb_cols <- c("PB_Pauly_1980", "PB_FishLife_2023", "PB_Gascuel_2008", "PB_Hoenig_1983", "PB_Then_2015", "PB_AlversonCarney_1975")
fish_pb_long <- melt(fish_results[, c("Species", "PB_method", ..pb_cols)],
                     id.vars = c("Species", "PB_method"), variable.name = "method", value.name = "PB")
fish_pb_long[, method := gsub("^PB_", "", method)]
fish_pb_long <- fish_pb_long[!is.na(PB)]

## mark which row corresponds to the method actually chosen - PB_method
## has suffixes like "+F(Y/B)" appended, so match on the method name
## being contained in PB_method rather than requiring an exact match.
## Keys here must match what gsub("^PB_", "", ...) produces from the
## Estimate_Author_Year column names above.
method_label_map <- c(Pauly_1980 = "Pauly 1980", FishLife_2023 = "FishLife",
                      Gascuel_2008 = "Gascuel 2008", Hoenig_1983 = "Hoenig 1983",
                      Then_2015 = "Then et al. 2015", AlversonCarney_1975 = "Alverson & Carney 1975")
fish_pb_long[, is_chosen := mapply(function(m, chosen) grepl(method_label_map[[m]], chosen, ignore.case = TRUE),
                                   method, PB_method)]

p_pb <- ggplot(fish_pb_long, aes(x = method, y = PB)) +
  geom_point(aes(color = is_chosen, size = is_chosen)) +
  scale_color_manual(values = c(`TRUE` = "firebrick", `FALSE` = "grey50"),
                     labels = c(`TRUE` = "Chosen for FG average", `FALSE` = "Other method"),
                     name = NULL) +
  scale_size_manual(values = c(`TRUE` = 4, `FALSE` = 2.5), guide = "none") +
  facet_wrap(~ Species, scales = "free_y") +
  theme_bw(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
  labs(title = "P/B method comparison - fish species", x = NULL, y = expression(P/B~(year^-1)))

ggsave(file.path(plot_dir, "fish_PB_methods_comparison.png"), p_pb, width = 12, height = 9, dpi = 150)

## --- QB methods ---------------------------------------------------------

qb_cols <- c("QB_PalomaresPauly_1998Z", "QB_PalomaresPauly_1998noZ", "QB_ChristensenPauly_1992", "QB_ChristensenEtAl_2008")
fish_qb_long <- melt(fish_results[, c("Species", "QB_method", ..qb_cols)],
                     id.vars = c("Species", "QB_method"), variable.name = "method", value.name = "QB")
fish_qb_long[, method := gsub("^QB_", "", method)]
fish_qb_long <- fish_qb_long[!is.na(QB)]

qb_label_map <- c(PalomaresPauly_1998Z = "Z-based", PalomaresPauly_1998noZ = "non-Z",
                  ChristensenPauly_1992 = "Christensen & Pauly", ChristensenEtAl_2008 = "Q/P=3")
fish_qb_long[, is_chosen := mapply(function(m, chosen) grepl(qb_label_map[[m]], chosen, fixed = TRUE),
                                   method, QB_method)]

p_qb <- ggplot(fish_qb_long, aes(x = method, y = QB)) +
  geom_point(aes(color = is_chosen, size = is_chosen)) +
  scale_color_manual(values = c(`TRUE` = "firebrick", `FALSE` = "grey50"),
                     labels = c(`TRUE` = "Chosen for FG average", `FALSE` = "Other method"),
                     name = NULL) +
  scale_size_manual(values = c(`TRUE` = 4, `FALSE` = 2.5), guide = "none") +
  facet_wrap(~ Species, scales = "free_y") +
  theme_bw(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
  labs(title = "Q/B method comparison - fish species", x = NULL, y = expression(Q/B~(year^-1)))

ggsave(file.path(plot_dir, "fish_QB_methods_comparison.png"), p_qb, width = 12, height = 9, dpi = 150)

print(p_pb)
print(p_qb)

message("\nSaved: fish_PB_methods_comparison.png, fish_QB_methods_comparison.png")
invisible(STAGE_PB$tick(tokens = list(stage_name = "Export CSVs + generate plots")))

## =================================================================
## OUTPUT 1: Reference/provenance table - which study (Locality, Year)
## backs each species' growth/length-weight/maturity parameters. This
## is what select_best_rows() used to decide the Western-Med/recent
## priority, exposed here rather than discarded after the fact.
## =================================================================

reference_table <- rbindlist(list(growth_provenance, lw_provenance, maturity_provenance), fill = TRUE)
reference_table <- merge(reference_table, species_df[, .(Species, dispatch_group)], by = "Species", all.x = TRUE)
setcolorder(reference_table, c("Species", "dispatch_group", "parameter_type", "Locality", "Year", "Reference"))
setorder(reference_table, Species, parameter_type)

fwrite(reference_table, file.path(csv_out_dir, "species_parameter_references.csv"))
message("\nSaved: species_parameter_references.csv (", nrow(reference_table),
        " rows - which study/locality/year backs each species' growth,",
        " length-weight, and maturity parameters)")

## Same table, added to the shared workbook (same file PB_QB and
## Ecobase sheets live in) as "References" - so a parameter's source
## study is one sheet-tab away from the value itself, not only
## available as a separate CSV. Species-level only (growth/length-
## weight/maturity provenance) - EcoBase's own FG-level literature
## citations already live in their own "Ecobase" sheet (model/year/
## authors columns), a different grain that doesn't merge cleanly
## into this species-level table.
## 2026-09-17 update: native/intermediate reference table, not a final
## target sheet - written as CSV only.
write_native_sheets_csv(list(References = reference_table), csv_out_dir)

## =================================================================
## OUTPUT 1b: METHOD_REFERENCES - which published equation/method
## backs each M_*/PB_*/QB_* column this script can produce, across
## every dispatch_group (fish, marine mammals, seabirds,
## invertebrates). Distinct from the "References" sheet above:
## References says WHERE a species' own growth/length-weight/maturity
## DATA came from (a FishBase study's Locality/Year); this says WHICH
## published EQUATION was used to turn traits into M/PB/QB. Built as
## a static lookup (one row per method column actually used anywhere
## in this script), not derived from species_df, so it's the same
## regardless of which species happen to be in this run.
##
## Confidence column flags citations verified this session against
## the primary source (via literature search) vs. ones cited the way
## they're conventionally referenced within the Ecopath/EwE community
## (commonly through Christensen & Walters' EwE user guide or
## secondary compilations) but not independently re-verified against
## the original paper here - worth a check against your own protocol
## document's reference list if you need certainty on the exact
## year/journal for those specific rows.
##
## NOTE: "Froese & Binohlan (2003)" in older comments elsewhere in
## this script was a wrong year - corrected to 2000 here and in that
## comment (see the Loo/K fallback block above, protocol Eq. 11 area).
## =================================================================
METHOD_REFERENCES <- data.table::data.table(
  Method = c(
    "M_Pauly_1980", "M_FishLife_2023", "M_Gascuel_2008", "M_Hoenig_1983", "M_Then_2015", "M_AlversonCarney_1975",
    "Froese_Binohlan_2000_fallback",
    "QB_PalomaresPauly_1998Z", "QB_PalomaresPauly_1998noZ", "QB_ChristensenPauly_1992", "QB_ChristensenEtAl_2008_QP3",
    "PB_BarlowBoveng_1991", "QB_InnesTrites_1987_1997",
    "QB_NilssonNilsson_1976",
    "PB_TumbioloDowning_1994", "PB_Brey_1999"
  ),
  Category = c(
    "Fish - natural mortality (M)", "Fish - natural mortality (M)", "Fish - natural mortality (M)",
    "Fish - natural mortality (M)", "Fish - natural mortality (M)", "Fish - natural mortality (M)",
    "Fish - growth-parameter fallback (Loo/K)",
    "Fish - consumption/biomass (QB)", "Fish - consumption/biomass (QB)", "Fish - consumption/biomass (QB)", "All groups - QB fallback",
    "Marine mammals - production/biomass (PB)", "Marine mammals - consumption/biomass (QB)",
    "Seabirds - consumption/biomass (QB)",
    "Invertebrates - production/biomass (PB)", "Invertebrates - production/biomass (PB)"
  ),
  Citation = c(
    "Pauly, D. (1980). On the interrelationships between natural mortality, growth parameters, and mean environmental temperature in 175 fish stocks. Journal du Conseil International pour l'Exploration de la Mer, 39(2), 175-192.",
    "Thorson, J.T. (2020, and updates through 2023). Predicting life history parameters for all fishes worldwide (FishLife R package/database). Originally: Thorson, J.T., Munch, S.B., Cope, J.M., Gao, J. (2017). Predicting life history parameters for all fishes worldwide. Ecological Applications, 27(8), 2262-2276.",
    "Gascuel, D., Bozec, Y.-M., Chassot, E., Colomb, A., Laurans, M. (2005/2008). The trophic-level based ecosystem modelling approach - trophic-level/temperature empirical M relationship used here as the general fish M fallback (protocol Eq.15).",
    "Hoenig, J.M. (1983). Empirical use of longevity data to estimate mortality rates. Fishery Bulletin, 81(4), 898-903. (via TropFishR::M_empirical(), method 'Hoenig'.)",
    "Then, A.Y., Hoenig, J.M., Hall, N.G., Hewitt, D.A. (2015). Evaluating the predictive performance of empirical estimators of natural mortality rate using information on over 200 fish species. ICES Journal of Marine Science, 72(1), 82-92. (via TropFishR::M_empirical(), method 'Then_growth'.)",
    "Alverson, D.L., Carney, M.J. (1975). A graphic review of the growth and decay of population cohorts. Journal du Conseil International pour l'Exploration de la Mer, 36(2), 133-143. (via TropFishR::M_empirical(), method 'AlversonCarney'.)",
    "Froese, R., Binohlan, C. (2000). Empirical relationships to estimate asymptotic length, length at first maturity and length at maximum yield per recruit in fishes, with a simple method to evaluate length frequency data. Journal of Fish Biology, 56(4), 758-773.",
    "Palomares, M.L.D., Pauly, D. (1998). Predicting food consumption of fish populations as functions of mortality, food type, morphometrics, temperature and salinity. Marine and Freshwater Research, 49(5), 447-453. (Eq.27, uses Z/PB.)",
    "Palomares, M.L.D., Pauly, D. (1998). Predicting food consumption of fish populations as functions of mortality, food type, morphometrics, temperature and salinity. Marine and Freshwater Research, 49(5), 447-453. (Eq.26, does not require Z.)",
    "Christensen, V., Pauly, D. (1992). ECOPATH II - a software for balancing steady-state ecosystem models and calculating network characteristics. Ecological Modelling, 61(3-4), 169-185. (QB predictor adapted here, protocol Eq.24.)",
    "Q/P (QB/PB) = 3 heuristic, as conventionally applied in Ecopath models per Christensen, V., Walters, C.J., Pauly, D. (2008 update of the EwE user guide). Fisheries Centre, UBC. Used here as the QB fallback wherever a dedicated QB method/data requirement isn't met.",
    "Barlow, J., Boveng, P. (1991). Modeling age-specific mortality for marine mammal populations. Marine Mammal Science, 7(1), 50-65. (Siler competing-risks survivorship model, parameterized here by taxonomic surrogate group.)",
    "Innes, S., Lavigne, D.M., Earle, W.M., Kovacs, K.M. (1987). Feeding rates of seals and whales. Journal of Animal Ecology, 56(1), 115-130; coefficients as commonly re-applied in Ecopath marine-mammal QB estimation (via Trites, A.W. and co-authors through the late 1990s). Verify the exact coefficient source against your own protocol document - not independently re-derived from the primary paper this session.",
    "Nilsson, S.G., Nilsson, I.N. (1976), as conventionally cited for the seabird daily-ration/body-mass regression used in Ecopath QB estimation (Eq.30). Verify the exact citation against your own protocol document - not independently re-derived from the primary paper this session.",
    "Tumbiolo, M.L., Downing, J.A. (1994). An empirical model for the prediction of secondary production in marine benthic invertebrate populations. Marine Ecology Progress Series, 114, 165-174.",
    "Brey, T. (1999/2001). A collection of empirical relations for use in ecological modelling (temperature/longevity-based invertebrate P/B, protocol Eq.19). Newsletter of the Fisheries Research Report Series / later formalized in Brey, T. (2012), Population dynamics in marine benthic invertebrates - a virtual handbook."
  ),
  Confidence = c(
    "Verified this session", "Verified this session (package/database, not a single paper)", "Verified this session (relationship confirmed; exact 2005 vs 2008 paper not disambiguated)",
    "Verified this session", "Verified this session", "Verified this session",
    "Verified this session (year corrected from an earlier '2003' typo in this script's own comments)",
    "Verified this session", "Verified this session", "Verified this session", "Conventional EwE citation - not independently re-verified",
    "Verified this session", "Conventional EwE citation - not independently re-verified",
    "Conventional EwE citation - not independently re-verified",
    "Verified this session", "Verified this session (exact year/venue for the Eq.19 coefficients not fully disambiguated)"
  )
)

fwrite(METHOD_REFERENCES, file.path(csv_out_dir, "pbqb_method_references.csv"))
message("\nSaved: pbqb_method_references.csv (", nrow(METHOD_REFERENCES),
        " rows - the published equation/method behind every M_*/PB_*/QB_* column this",
        " script can produce, across fish/marine mammals/seabirds/invertebrates). ",
        sum(METHOD_REFERENCES$Confidence == "Conventional EwE citation - not independently re-verified"),
        " row(s) are flagged as conventionally-cited-but-not-independently-verified - worth",
        " a check against your own protocol document if exact citations matter.")

## Added as its own workbook sheet, named distinctly from "References"
## (that sheet's own species-level FishBase provenance) so the two
## don't get confused - this one answers "which formula", not "which
## study backs this species' trait value".
## 2026-09-17 update: native/intermediate reference table, not a final
## target sheet - written as CSV only.
write_native_sheets_csv(list(PBQB_Method_References = METHOD_REFERENCES), csv_out_dir)

## =================================================================
## Load the full FG reference (FGnum -> FGname) once, used both by the
## FG-level plot below (for "FGnum_FGname" axis labels) and the
## Ecopath CSV export later.
## =================================================================

## FG_WMed_2026.csv - the reviewed 2026 species -> FG reference (same
## file 01_biomass.R's fg_species_file and 02_fisheries.R's fg_file both
## read), NOT the old FG_WMed.xlsx. This is a pure FG_number/FG_name
## lookup (only used for "FGnum_FGname" axis labels and the Ecopath CSV
## below) - a plain fread(), no sheet-name ambiguity to guess at the way
## the old xlsx had (it used to carry a SECOND, unrelated "fg_ebro
## delta_21" sheet and duplicate FG_name-like columns within the
## fg_wmed_95 sheet itself - none of that exists in the CSV).
FG_REFERENCE_PATH <- file.path(pcloud_dir, "data/FG_WMed_2026.csv")

fg_ref_unique <- NULL
## fg_species_all (2026-09-23 addition): the FULL species-level table -
## every (Species, FG, FG_name) row FG_WMed_2026.csv defines, not just
## the deduplicated FG-number/FG-name pairs in fg_ref_unique above.
## Used below so the traits_ewe/Ecopath_traits output can include EVERY
## species in the FG reference, not only the ~505 species_df happens to
## have survey density for in the YEAR_ECOPATH window.
fg_species_all <- NULL
if (file.exists(FG_REFERENCE_PATH)) {
  fg_ref_raw <- fread(FG_REFERENCE_PATH)
  num_col <- intersect(c("FG_number", "FG_num", "GF"), names(fg_ref_raw))[1]
  name_col <- intersect("FG_name", names(fg_ref_raw))[1]
  species_col <- intersect(c("species", "Species", "ESPECIE", "ScientificName"), names(fg_ref_raw))[1]
  if (!is.na(num_col) && !is.na(name_col)) {
    fg_ref_unique <- unique(fg_ref_raw[, .(FG = as.character(as.integer(get(num_col))),
                                           FG_name = get(name_col))])
    message("\nFG reference loaded from ", FG_REFERENCE_PATH, " (", nrow(fg_ref_unique),
            " FGs), using columns '", num_col, "' and '", name_col, "'.")
    if (!is.na(species_col)) {
      fg_species_all <- unique(fg_ref_raw[, .(Species = trimws(get(species_col)),
                                              FG = as.character(as.integer(get(num_col))),
                                              FG_name = get(name_col))])
      fg_species_all <- fg_species_all[!is.na(Species) & Species != ""]
      message("Species-level FG reference also loaded (", nrow(fg_species_all),
              " species rows) using column '", species_col, "' - traits_ewe/Ecopath_traits will",
              " include every one of these, not just the species_df subset.")
    } else {
      message("FG_REFERENCE_PATH has no recognizable species-name column (checked: species,",
              " Species, ESPECIE, ScientificName) - traits_ewe/Ecopath_traits will fall back to",
              " species_df's own species list only (the YEAR_ECOPATH survey subset).")
    }
  } else {
    message("\n", strrep("!", 70))
    message("FG_REFERENCE_PATH exists but doesn't have both a FG-number-like column",
            " (checked: FG_number, FG_num, GF) and a 'FG_name' column - found: ",
            paste(names(fg_ref_raw), collapse = ", "), ".")
    message("FG plot axis and Ecopath CSV will use bare FG numbers, not FGnum_FGname.")
    message(strrep("!", 70))
  }
} else {
  message("\n", strrep("!", 70))
  message("FG_REFERENCE_PATH NOT FOUND: ", FG_REFERENCE_PATH)
  message("This path has been a GUESS on my part - if your FG reference file is",
          " actually somewhere else, update FG_REFERENCE_PATH above to the real",
          " path and re-run. Until then, the FG plot axis and Ecopath CSV will",
          " use bare FG numbers instead of FGnum_FGname labels.")
  message(strrep("!", 70))
}

## =================================================================
## traits_ewe sheet - species-level life-history/ecology traits
## (Organism, Ecology, Occurrence status, Biomass/Catch contribution,
## IUCN status, Exploitation status, Vulnerability index, Mean/Max
## length, Mean weight, Mean life span).
##
## MOVED HERE from 01_biomass.R (2026-09-22): this script is the one
## place FG_WMed_2026.csv (fg_ref_unique, loaded above) and species_df's
## own trait fetches are both in scope.
##
## 2026-09-23 rewrite: no longer reads the old FG_WMed.xlsx workbook at
## all - this pipeline's only file dependencies are FG_WMed_2026.csv
## (FG numbering/grouping AND, now, the full species list per FG) plus
## the actual data sources/databases (surveys, FishBase/SeaLifeBase,
## EcoBase). The table below is built directly from FG_WMed_2026.csv's
## own species list (fg_species_all, loaded above), left-joined with
## whatever species_df has computed for each species - so every FG and
## every species FG_WMed_2026.csv defines is present, not just the
## ~500 species_df has 1994-1996 survey density for. A species FG_WMed
## _2026.csv lists but species_df doesn't cover (no survey record in
## the YEAR_ECOPATH window) still gets its own row, with the
## FishBase-derived columns simply blank:
##   - Organism            <- dispatch_group (fish/invertebrate/mammal/
##                            seabird) - only known for species_df's
##                            own species (classified in Step 1); NA
##                            for species outside that survey window
##   - Max_length           <- MaxLength (FishBase species())
##   - Mean_length           <- CommonLength (FishBase species() "typical/
##                            common length", distinct from the MAXIMUM
##                            Length/LengthMax field used for Max_length -
##                            see the CommonLength comment near its
##                            fetch in Step 2a for the caveat on this
##                            field name)
##   - Mean_weight           <- a_lw * Mean_length ^ b_lw (the same
##                            length-weight regression already used
##                            elsewhere in this script for Winf, applied
##                            at Mean_length instead of Loo) - NA
##                            whenever Mean_length or a_lw/b_lw is
##                            missing, rather than guessed
##   - Mean_lifespan_years  <- Longevity (FishBase species())
##   - Vulnerability_index  <- Vulnerability (FishBase species(), added
##                            in Step 2a specifically for this column)
##   - Biomass_contribution <- this species' share of its FG's total
##                            Biomass (survey-derived, species_df$Biomass)
##   - Catch_contribution   <- this species' share of its FG's total
##                            Yield (fisheries-derived, species_df$Yield -
##                            NA whenever YIELD_SOURCE has no data, same
##                            as species_df$Yield itself)
## 2026-09-24 update, per Andrea (correctly identified these as real
## FishBase/SeaLifeBase fields, not something that needs hand-curation):
##   - Organism  <- classify_organism(Class, Phylum[, Kingdom]) (Step 1,
##                  lib taxonomy fetch) - Fishes/Mammals/Birds/Reptiles/
##                  Algae/Plants/Bacteria/Fungi/Invertebrates/Other -
##                  finer than dispatch_group (which only exists to pick
##                  a PB/QB formula and lumps reptiles into
##                  "invertebrate") - see classify_organism()'s own
##                  comment near where taxonomy is built, Step 1.
##   - Ecology   <- DemersPelag (FishBase/SeaLifeBase species(), Step 2a) -
##                  bathydemersal/bathypelagic/benthic/benthopelagic/
##                  demersal/pelagic/pelagic-neritic/pelagic-oceanic/
##                  reef-associated. NOT populated for "land-based"
##                  species (seabirds/some mammals) - FishBase/SeaLifeBase
##                  has no reason to carry that value at all, since it's
##                  an aquatic-organism database; those rows just stay
##                  blank here rather than guessed.
##   - IUCN_conservation_status <- IUCN_Code (species(), Step 2a).
##   - Exploitation_status <- Importance (species(), Step 2a) - FishBase's
##                  own commercial-importance category, the closest
##                  native field to "exploitation status".
##   - Occurrence_status <- rfishbase::country() Status, Western Med
##                  countries only (Step 2a2) - genuinely NEW/UNVERIFIED
##                  functionality (see that step's own comment) - check
##                  its coverage message on the first real run.
## All four are still genuinely NA wherever FishBase/SeaLifeBase itself
## has no value for that species (or the species isn't in species_df at
## all - no 1994-1996 survey record) - never guessed/fabricated.

species_universe <- if (!is.null(fg_species_all)) {
  copy(fg_species_all)
} else {
  message("fg_species_all not available (FG_WMed_2026.csv missing, or no species-name column",
          " found in it) - traits_ewe/Ecopath_traits falls back to species_df's own species list",
          " (the YEAR_ECOPATH survey subset) instead of every species in the FG reference.")
  unique(species_df[, .(Species, FG, FG_name)])
}
## FG is character in fg_species_all (built as as.character(as.integer(...))
## a few hundred lines up) but numeric/integer in species_df - normalize
## both sides to character before merging on FG below, or data.table's
## bmerge refuses with "Incompatible join types" (confirmed by an actual
## run: x.FG integer vs i.FG character).
species_universe[, FG := as.character(FG)]

fg_totals <- species_df[, .(FG_Biomass_total = sum(Biomass, na.rm = TRUE),
                            FG_Yield_total   = sum(Yield, na.rm = TRUE)), by = FG]
fg_totals[, FG := as.character(FG)]

traits_reconciled <- merge(species_universe,
                           species_df[, .(Species, dispatch_group, Organism, MaxLength, CommonLength,
                                          Longevity, Vulnerability, a_lw, b_lw, Biomass, Yield,
                                          Ecology, IUCN_conservation_status, Exploitation_status,
                                          Occurrence_status)],
                           by = "Species", all.x = TRUE)
traits_reconciled <- merge(traits_reconciled, fg_totals, by = "FG", all.x = TRUE)

traits_reconciled[, `:=`(
  Biomass_contribution = fifelse(!is.na(FG_Biomass_total) & FG_Biomass_total > 0,
                                 Biomass / FG_Biomass_total, NA_real_),
  Catch_contribution   = fifelse(!is.na(FG_Yield_total) & FG_Yield_total > 0,
                                 Yield / FG_Yield_total, NA_real_),
  Vulnerability_index  = Vulnerability,
  Mean_length          = CommonLength,
  Max_length           = MaxLength,
  Mean_weight          = fifelse(!is.na(CommonLength) & !is.na(a_lw) & !is.na(b_lw),
                                 a_lw * CommonLength ^ b_lw, NA_real_),
  Mean_lifespan_years  = Longevity
)]
traits_reconciled[, c("dispatch_group", "MaxLength", "CommonLength", "Longevity", "Vulnerability",
                      "a_lw", "b_lw", "Biomass", "Yield", "FG_Biomass_total", "FG_Yield_total") := NULL]
traits_reconciled[, in_fg_master := Species %in% species_df$Species]
traits_reconciled[, missing_traits := is.na(Max_length) & is.na(Mean_lifespan_years) & is.na(Vulnerability_index)]
## FG is character (see the as.character() normalization above) -
## sort by its NUMERIC value so FG order is 1, 2, ..., 10, not the
## character-sort order "1", "10", "2", ... that plain setorder(FG)
## would otherwise produce.
traits_reconciled[, FG_sort_key := suppressWarnings(as.numeric(FG))]
setorder(traits_reconciled, FG_sort_key, Species, na.last = TRUE)
traits_reconciled[, FG_sort_key := NULL]

message("traits_ewe built directly from FG_WMed_2026.csv + species_df: ", nrow(traits_reconciled),
        " species total across ", uniqueN(traits_reconciled$FG), " FGs (",
        sum(traits_reconciled$in_fg_master), " with 1994-1996 survey data in species_df, ",
        sum(!traits_reconciled$in_fg_master), " listed in FG_WMed_2026.csv only - FishBase-derived",
        " columns blank for those). ", sum(!traits_reconciled$missing_traits),
        " species have at least one FishBase trait, ", sum(traits_reconciled$missing_traits), " have none.",
        " Ecology/Occurrence_status/IUCN_conservation_status/Exploitation_status have no automated",
        " source in this pipeline and are left blank (previously hand-curated in the retired",
        " FG_WMed.xlsx sheet) - fill these manually if you need them.")

## 2026-09-17 update, revised 2026-09-23: Ecopath_traits is the FG_name/
## species-level traits table "as it was saved" - i.e. this species-level
## traits_reconciled table, not the FG-level biomass-weighted rollup this
## script computes from it elsewhere. traits_reconciled (tidy, one row
## per species, FG/FG_name on every row) is kept as the machine-readable
## source of truth - written as traits_ewe.csv below, for anything that
## reads this data back programmatically. This script's own FG-level
## rollup is written as an audit-only CSV instead of a workbook sheet, so
## nothing else contends for the Ecopath_traits sheet name.
write_native_sheets_csv(
  sheets  = list(traits_ewe = traits_reconciled),
  out_dir = csv_out_dir
)

## --- Excel-style grouped layout, now the ACTUAL Ecopath_traits sheet --
## Mirrors the OLD FG_WMed.xlsx traits_ewe sheet's own visual convention
## (confirmed against a user-supplied example export of that sheet): a
## "<FG_num>: <FG_name>" header row, one blank row, then that FG's
## species rows - repeated per FG, in FG order. 2026-09-23: Andrea
## confirmed she wants the WORKBOOK sheet itself in this layout (not just
## an audit-only CSV alongside a flat workbook sheet) - nothing downstream
## in this pipeline reads the Ecopath_traits sheet back programmatically
## (grep confirmed: only trim_workbook_to_final_sheets()'s target_order
## and comments reference the sheet name), so it's safe to make this the
## sheet's actual shape. traits_ewe.csv (flat, written above) remains the
## machine-readable source of truth for anything that needs to read this
## data back in.
build_grouped_traits_sheet <- function(dt) {
  trait_cols <- setdiff(names(dt), c("FG", "FG_name", "Species", "in_fg_master", "missing_traits"))
  out_cols <- c("Species", trait_cols)
  blank_row <- as.list(rep(NA_character_, length(out_cols) + 1))
  names(blank_row) <- c("row_index", out_cols)
  rows <- list()
  for (fg in unique(dt$FG)) {
    fg_rows <- dt[FG == fg]
    fg_name_i <- fg_rows$FG_name[1]
    header <- as.list(rep(NA_character_, length(out_cols) + 1))
    names(header) <- c("row_index", out_cols)
    header$Species <- paste0(fg, ": ", fg_name_i)
    rows[[length(rows) + 1]] <- as.data.table(header)
    rows[[length(rows) + 1]] <- as.data.table(blank_row)
    species_block <- fg_rows[, ..out_cols]
    species_block[, row_index := as.character(.I)]
    setcolorder(species_block, c("row_index", out_cols))
    species_block[] <- lapply(species_block, as.character)
    rows[[length(rows) + 1]] <- species_block
  }
  rbindlist(rows, use.names = TRUE, fill = TRUE)
}
traits_grouped <- build_grouped_traits_sheet(traits_reconciled)
fwrite(traits_grouped, file.path(csv_out_dir, "traits_ewe_grouped.csv"))
upsert_workbook_sheets(
  list(Ecopath_traits = traits_grouped),
  ECOPATH_WORKBOOK_PATH
)
message("Saved: traits_ewe_grouped.csv and workbook sheet Ecopath_traits (", uniqueN(traits_reconciled$FG),
        " FG header rows + ", nrow(traits_reconciled), " species rows) - the same data as traits_ewe.csv,",
        " laid out with a '<FG_num>: <FG_name>' header row and blank separator above each FG's species,",
        " matching the old FG_WMed.xlsx sheet's visual convention. Anything reading this data back",
        " programmatically should use traits_ewe.csv instead of the Ecopath_traits sheet.")

## =================================================================
## OUTPUT 2: Dot-whisker comparison plots - species-level (spread
## across ALL methods attempted, any dispatch group) and FG-level
## (spread across the chosen PB/QB of species within each FG). PB and
## QB shown side by side via patchwork for both.
## =================================================================

## patchwork already loaded at the top of this script.

## --- Species-level: melt EVERY method column across ALL groups, not
## just fish - dynamically detected by column name pattern rather than
## hardcoded per group, so this stays correct if methods are added later
exclude_cols <- c("PB", "QB", "PB_method", "QB_method")
all_pb_method_cols <- setdiff(grep("^PB_", names(results), value = TRUE),
                              c(exclude_cols, grep("_source$", names(results), value = TRUE)))
all_qb_method_cols <- setdiff(grep("^QB_", names(results), value = TRUE),
                              c(exclude_cols, grep("_source$", names(results), value = TRUE)))

species_pb_long <- melt(results[, c("Species", "FG", "FG_name", "dispatch_group", "Biomass", ..all_pb_method_cols)],
                        id.vars = c("Species", "FG", "FG_name", "dispatch_group", "Biomass"),
                        variable.name = "method", value.name = "PB")
species_pb_long[, method := gsub("^PB_", "", method)]
species_pb_long <- species_pb_long[!is.na(PB)]

species_qb_long <- melt(results[, c("Species", "FG", "FG_name", "dispatch_group", "Biomass", ..all_qb_method_cols)],
                        id.vars = c("Species", "FG", "FG_name", "dispatch_group", "Biomass"),
                        variable.name = "method", value.name = "QB")
species_qb_long[, method := gsub("^QB_", "", method)]
species_qb_long <- species_qb_long[!is.na(QB)]

species_pb_mean <- species_pb_long[, .(PB_mean = mean(PB, na.rm = TRUE), PB_sd = sd(PB, na.rm = TRUE)), by = .(Species, FG)]
species_qb_mean <- species_qb_long[, .(QB_mean = mean(QB, na.rm = TRUE), QB_sd = sd(QB, na.rm = TRUE)), by = .(Species, FG)]

## y-axis label includes the FG number in brackets, e.g. "Diplodus
## annularis (35)" - built from FG (a species belongs to exactly one,
## so this is a 1:1 label, not an aggregation)
species_pb_mean[, Species_label := paste0(Species, " (", FG, ")")]
species_pb_long[, Species_label := paste0(Species, " (", FG, ")")]
species_qb_mean[, Species_label := paste0(Species, " (", FG, ")")]
species_qb_long[, Species_label := paste0(Species, " (", FG, ")")]

## Species ordered ALPHABETICALLY on the species name itself (not by
## mean value, and not thrown off by the "(FG)" suffix), and the SAME
## order used for both PB and QB plots so a species sits on the same
## row in both, making the side-by-side comparison meaningful
species_order_dt <- unique(rbindlist(list(species_pb_mean[, .(Species, Species_label)],
                                          species_qb_mean[, .(Species, Species_label)])))
setorder(species_order_dt, -Species)  # reversed so A is at the top, not bottom, on a ggplot y-axis
species_label_order <- species_order_dt$Species_label

species_pb_mean[, Species_label := factor(Species_label, levels = species_label_order)]
species_pb_long[, Species_label := factor(Species_label, levels = species_label_order)]
species_qb_mean[, Species_label := factor(Species_label, levels = species_label_order)]
species_qb_long[, Species_label := factor(Species_label, levels = species_label_order)]

## =================================================================
## save_paginated_pb_qb_plot()
##
## height = 0.35 * n_rows (one row per species/FG) exceeds ggsave's
## 50in hard limit once there are more than ~140 rows - hit exactly
## this on the species-level plot. Raising the limit
## (limitsize = FALSE) would "fix" the error but produce an image
## nobody can actually read at any zoom level; paginating into
## several page-sized PNGs is the useful fix, not a bigger file.
##
## Takes standardized-name copies of the mean/long data (Label, Mean,
## SD, method, Value - renamed at each call site from the real
## Species_label/PB_mean/... or FG_label/... columns) so this one
## function serves both the species-level and FG-level plots below,
## which are otherwise near-identical ggplot code.
## =================================================================
save_paginated_pb_qb_plot <- function(pb_mean, pb_long, qb_mean, qb_long, label_order,
                                      pb_x_lab, qb_x_lab, file_prefix, plot_dir,
                                      height_per_row = 0.35, width_in = 16, dpi = 150,
                                      max_height_in = 40) {
  n_per_page <- max(10, floor(max_height_in / height_per_row))
  pages <- if (length(label_order) <= n_per_page) {
    list(label_order)
  } else {
    split(label_order, ceiling(seq_along(label_order) / n_per_page))
  }
  
  if (length(pages) > 1) {
    message(length(label_order), " rows would need height = ",
            round(height_per_row * length(label_order), 1), "in - over ggsave's 50in hard",
            " limit. Split into ", length(pages), " page(s) of up to ", n_per_page,
            " rows each (", file_prefix, "_page1.png, _page2.png, ...) instead of one",
            " oversized image nobody could actually read.")
  }
  
  saved <- character(0)
  for (i in seq_along(pages)) {
    page_labels <- pages[[i]]
    page_height <- max(6, height_per_row * length(page_labels))
    
    p_pb_i <- ggplot() +
      geom_segment(data = pb_mean[Label %in% page_labels],
                   aes(x = Mean - SD, xend = Mean + SD, y = Label, yend = Label),
                   linewidth = 0.7, colour = "black") +
      geom_point(data = pb_mean[Label %in% page_labels], aes(x = Mean, y = Label),
                 shape = "|", size = 5, colour = "black") +
      geom_point(data = pb_long[Label %in% page_labels], aes(x = Value, y = Label, colour = method, fill = method),
                 shape = 21, size = 2.5, alpha = 0.7) +
      scale_colour_brewer(palette = "Set1") + scale_fill_brewer(palette = "Set1") +
      theme_bw(base_size = 12) +
      theme(legend.position = "bottom", panel.grid.minor.y = element_blank()) +
      labs(x = pb_x_lab, y = NULL, colour = "Method", fill = "Method")
    
    p_qb_i <- ggplot() +
      geom_segment(data = qb_mean[Label %in% page_labels],
                   aes(x = Mean - SD, xend = Mean + SD, y = Label, yend = Label),
                   linewidth = 0.7, colour = "black") +
      geom_point(data = qb_mean[Label %in% page_labels], aes(x = Mean, y = Label),
                 shape = "|", size = 5, colour = "black") +
      geom_point(data = qb_long[Label %in% page_labels], aes(x = Value, y = Label, colour = method, fill = method),
                 shape = 21, size = 2.5, alpha = 0.7) +
      scale_colour_brewer(palette = "Set1") + scale_fill_brewer(palette = "Set1") +
      theme_bw(base_size = 12) +
      theme(legend.position = "bottom", panel.grid.minor.y = element_blank()) +
      labs(x = qb_x_lab, y = NULL, colour = "Method", fill = "Method")
    
    p_combined_i <- p_pb_i + p_qb_i
    fname <- if (length(pages) > 1) paste0(file_prefix, "_page", i, ".png") else paste0(file_prefix, ".png")
    fpath <- file.path(plot_dir, fname)
    ggsave(fpath, p_combined_i, width = width_in, height = page_height, dpi = dpi)
    saved <- c(saved, fpath)
    if (i == 1) print(p_combined_i)   # only the first page previewed inline
  }
  message("Saved: ", paste(basename(saved), collapse = ", "))
  invisible(saved)
}

save_paginated_pb_qb_plot(
  pb_mean   = copy(species_pb_mean)[, .(Label = Species_label, Mean = PB_mean, SD = PB_sd)],
  pb_long   = copy(species_pb_long)[, .(Label = Species_label, Value = PB, method)],
  qb_mean   = copy(species_qb_mean)[, .(Label = Species_label, Mean = QB_mean, SD = QB_sd)],
  qb_long   = copy(species_qb_long)[, .(Label = Species_label, Value = QB, method)],
  label_order = species_label_order,
  pb_x_lab  = expression(P/B~(year^-1)),
  qb_x_lab  = expression(Q/B~(year^-1)),
  file_prefix = "species_PB_QB_comparison",
  plot_dir    = plot_dir
)



## --- FG-level: for EACH method, a biomass-weighted average across the
## species within that FG that have a value for that method - mirrors
## exactly what the species-level plot does (spread across methods),
## just aggregated up one level, and colour-coded by method using the
## SAME palette/mapping so a colour means the same thing in both plots.
fg_pb_by_method <- species_pb_long[, .(
  PB = sum(Biomass * PB, na.rm = TRUE) / sum(Biomass[!is.na(PB)], na.rm = TRUE)
), by = .(FG, FG_name, method)]

fg_qb_by_method <- species_qb_long[, .(
  QB = sum(Biomass * QB, na.rm = TRUE) / sum(Biomass[!is.na(QB)], na.rm = TRUE)
), by = .(FG, FG_name, method)]

## Build "FGnum_FGname" labels - PRIORITIZES FG_name already present in
## species_df/results (the reliable source: known FGs hand-labeled at
## the input stage) over the external FG_WMed_2026.csv join, which is only
## used as a fallback for FGs where species_df didn't already have a
## name (e.g. a real run with many species not individually annotated).
build_fg_label <- function(dt, label = "") {
  dt[, FG_num := as.character(as.integer(FG))]
  
  ## fall back to the external reference ONLY where FG_name is missing
  if (!is.null(fg_ref_unique) && dt[is.na(FG_name), .N] > 0) {
    match_idx <- match(dt$FG_num, fg_ref_unique$FG)
    dt[is.na(FG_name), FG_name := fg_ref_unique$FG_name[match_idx[is.na(FG_name)]]]
  }
  
  n_matched <- dt[!is.na(FG_name), uniqueN(FG_num)]
  n_total <- uniqueN(dt$FG_num)
  message("  [", label, "] FG name available: ", n_matched, "/", n_total, " FGs.")
  if (n_matched < n_total) {
    message("    Still missing a name for FG numbers: ", paste(sort(unique(dt[is.na(FG_name), FG_num])), collapse = ", "))
  }
  
  dt[, FG_label := fifelse(!is.na(FG_name), paste0(FG_num, "_", FG_name), as.character(FG_num))]
  dt
}
fg_pb_by_method <- build_fg_label(fg_pb_by_method, "PB")
fg_qb_by_method <- build_fg_label(fg_qb_by_method, "QB")

fg_pb_mean <- fg_pb_by_method[, .(PB_mean = mean(PB, na.rm = TRUE), PB_sd = sd(PB, na.rm = TRUE)),
                              by = .(FG_num, FG_label)]
fg_qb_mean <- fg_qb_by_method[, .(QB_mean = mean(QB, na.rm = TRUE), QB_sd = sd(QB, na.rm = TRUE)),
                              by = .(FG_num, FG_label)]

## sorted NUMERICALLY by FG number (not alphabetically on the label,
## which would wrongly put "10_x" before "2_y"), same order shared by
## both PB and QB plots so an FG sits on the same row in both
fg_order_dt <- unique(rbindlist(list(fg_pb_mean[, .(FG_num, FG_label)],
                                     fg_qb_mean[, .(FG_num, FG_label)])))
fg_order_dt[, FG_num := as.numeric(FG_num)]
setorder(fg_order_dt, -FG_num)  # descending so lowest FG number is at the TOP of the y-axis
fg_label_order <- fg_order_dt$FG_label

fg_pb_mean[, FG_label := factor(FG_label, levels = fg_label_order)]
fg_pb_by_method[, FG_label := factor(FG_label, levels = fg_label_order)]
fg_qb_mean[, FG_label := factor(FG_label, levels = fg_label_order)]
fg_qb_by_method[, FG_label := factor(FG_label, levels = fg_label_order)]

save_paginated_pb_qb_plot(
  pb_mean   = copy(fg_pb_mean)[, .(Label = FG_label, Mean = PB_mean, SD = PB_sd)],
  pb_long   = copy(fg_pb_by_method)[, .(Label = FG_label, Value = PB, method)],
  qb_mean   = copy(fg_qb_mean)[, .(Label = FG_label, Mean = QB_mean, SD = QB_sd)],
  qb_long   = copy(fg_qb_by_method)[, .(Label = FG_label, Value = QB, method)],
  label_order = fg_label_order,
  pb_x_lab  = expression(P/B~(year^-1)),
  qb_x_lab  = expression(Q/B~(year^-1)),
  file_prefix = "FG_PB_QB_comparison",
  plot_dir    = plot_dir
)

message("\nSaved: species_PB_QB_comparison*.png, FG_PB_QB_comparison*.png")

## =================================================================
## OUTPUT 3: Ecopath-ready CSV - matches the REAL Ecopath Basic Input
## format exactly (confirmed against an actual exported file from this
## project: westernmed90s-Basic_input.csv), not a simplified guess:
##   [blank], Group name, Hab area (proportion),
##   Biomass in habitat area (t/km^2), Total mortality (/year),
##   Production / biomass (/year), Consumption / biomass (/year),
##   Ecotrophic Efficiency, Other mortality, Production / consumption,
##   Unassim. consumption, Detritus import (t/km^2/year)
## Uses EUROPEAN COMMA-DECIMAL formatting ("0,003222" not "0.003222"),
## since that's what the real file uses throughout - a plain fwrite()
## with R's default period decimals would not import correctly.
##
## Columns intentionally left blank, matching the real file's own
## convention: Total mortality (only used for multi-stanza juv/adult
## groups, which this pipeline doesn't currently build), Ecotrophic
## Efficiency and Other mortality (Ecopath solves for these itself,
## not inputs), Production/consumption (redundant once PB and QB are
## both given), and Detritus import (only relevant for the Detritus/
## Discards housekeeping groups themselves, which aren't species-based
## and outside this pipeline's scope).
## =================================================================

## Comma-decimal formatter matching the real file's style - empty
## string for NA (not "NA" literal, which Ecopath's importer would
## choke on), otherwise the number with a comma in place of the period
format_ecopath_num <- function(x, digits = 4) {
  fifelse(is.na(x), "", sub("\\.", ",", formatC(x, format = "f", digits = digits)))
}

ecopath_ready <- copy(fg_weighted)
ecopath_ready[, FG := as.character(FG)]

## fg_weighted already carries FG_name (attached earlier for the EcoBase
## merge) - dropped here and re-merged fresh from results rather than
## trusting it as-is, since results is still the reliable, hand-labeled
## source and this re-merge also lets fg_ref_unique below fill in FGs
## that have no FG_name yet (fg_weighted's own FG_name would be NA for
## those, same as before).
ecopath_ready[, FG_name := NULL]
results_fg_names <- unique(results[!is.na(FG_name), .(FG = as.character(FG), FG_name)])
ecopath_ready <- merge(ecopath_ready, results_fg_names, by = "FG", all.x = TRUE)

if (!is.null(fg_ref_unique)) {
  ## expand to ALL FGs in the reference (including ones with no species
  ## data this run), and fill in FG_name from the reference ONLY where
  ## results didn't already have it
  ecopath_ready <- merge(fg_ref_unique, ecopath_ready, by = "FG", all.x = TRUE, suffixes = c("_ref", ""))
  ecopath_ready[is.na(FG_name), FG_name := FG_name_ref]
  ecopath_ready[, FG_name_ref := NULL]
  message("\nJoined full FG reference (", nrow(fg_ref_unique), " total FGs) -",
          " Ecopath output now includes every FG in your model, not just",
          " the ones with species data in this run.")
} else {
  message("\nNo external FG reference loaded - Ecopath CSV will only include",
          " FGs present in this run's results (using FG_name already in",
          " species_df where available), not the full model FG list.")
}

ecopath_ready[, FG_num := as.numeric(FG)]
setorder(ecopath_ready, FG_num)

## primary producers (phytoplankton) don't consume anything, so QB and
## Unassim. consumption correctly stay blank for them - matches the
## real file's own convention for its seagrass/algae/phytoplankton rows
is_primary_producer <- ecopath_ready$FG_num %in% species_df[dispatch_group == "phytoplankton", FG]

ecopath_final <- data.table(
  ` ` = ecopath_ready$FG_num,                                             # blank header, matches the real file's unnamed first column
  `Group name` = ecopath_ready$FG_name,
  `Hab area (proportion)` = format_ecopath_num(rep(1, nrow(ecopath_ready)), digits = 4),
  `Biomass in habitat area (t/km^2)` = format_ecopath_num(ecopath_ready$Biomass_FG, digits = 4),
  `Total mortality (/year)` = "",                                          # multi-stanza only - not built by this pipeline
  `Production / biomass (/year)` = format_ecopath_num(ecopath_ready$PB_FG, digits = 4),
  `Consumption / biomass (/year)` = fifelse(is_primary_producer, "", format_ecopath_num(ecopath_ready$QB_FG, digits = 4)),
  `Ecotrophic Efficiency` = "",                                            # Ecopath solves for this - not an input
  `Other mortality` = "",
  `Production / consumption` = "",                                        # redundant once PB and QB are both given
  `Unassim. consumption` = fifelse(is_primary_producer, "", "0,2000"),     # standard default for consumers
  `Detritus import (t/km^2/year)` = ""                                    # only relevant for Detritus/Discards housekeeping groups
)

n_missing <- ecopath_ready[is.na(PB_FG) | (is.na(QB_FG) & !is_primary_producer), .N]
if (n_missing > 0) {
  message(n_missing, " FG(s) have no PB and/or QB from this run - these rows",
          " are included but blank, flagged for manual completion:")
  print(ecopath_ready[is.na(PB_FG) | (is.na(QB_FG) & !is_primary_producer), .(FG_num, FG_name)])
}

fwrite(ecopath_final, file.path(csv_out_dir, "ecopath_ready_PB_QB.csv"), quote = "auto")
message("\nSaved: ecopath_ready_PB_QB.csv (", nrow(ecopath_final), " FG rows,",
        " FG_num ", min(ecopath_ready$FG_num), "-", max(ecopath_ready$FG_num), ") -",
        " formatted to match Ecopath's real Basic Input structure",
        " (comma-decimal, correct column set) - selection mode: '",
        PB_QB_SELECTION_MODE, "'.")
message("NOTE: multi-stanza groups (e.g. 'European sardine juv'/'adult' pairs) need",
        " their own header row above the stanza members in the real Ecopath format",
        " (see row 20 'European sardine' in westernmed90s-Basic_input.csv for the",
        " pattern) - this export doesn't build those automatically since this",
        " pipeline doesn't currently model juvenile/adult stanza splits.")

## =================================================================
## Total pipeline runtime
## =================================================================
elapsed <- Sys.time() - PIPELINE_START_TIME
message("\n", strrep("=", 50))
message("Total pipeline runtime: ", round(as.numeric(elapsed, units = "mins"), 2), " minutes",
        " (", round(as.numeric(elapsed, units = "secs"), 1), " seconds)")
message(strrep("=", 50))