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
## Shared paths - set ONCE here, reused by all three scripts.
##
## Packaged as a repo other people can clone and run - rather than
## every person editing these three lines directly (and constantly
## re-diffing/re-committing over each other's local paths), this looks
## for a config.R file living alongside this script FIRST. Copy
## config.R.example to config.R, edit YOUR OWN three paths in there,
## and this block picks them up automatically - config.R is in
## .gitignore, so your local paths never end up in a commit or collide
## with anyone else's.
##
## Nobody's workflow breaks: if config.R doesn't exist yet, this falls
## back to Daniel's own hardcoded defaults below, exactly as before -
## just with a warning so it's obvious a personal config.R was never
## set up, instead of silently running on someone else's paths.
## -----------------------------------------------------------------
## NOTE: file.exists("config.R") is relative to the current working
## directory, not to this script's own location - in RStudio, hitting
## Source on this file sets the working directory to wherever this
## file lives, so config.R just needs to sit right next to it. Running
## from a plain Rscript call elsewhere, setwd() into this script's
## folder first (or pass an absolute path here instead).
if (file.exists("config.R")) {
  message("[run_pipeline_demo.R] Found config.R next to this script - using its out_dir/pcloud_dir/git_dir.")
  source("config.R")
} else if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  
  ## A native folder-picker dialog is used here rather than readline()
  ## console prompts, which break under RStudio's "Source" (it echoes
  ## the WHOLE file's text into the console as if typed, so readline()
  ## ends up reading later LINES OF THIS SCRIPT as if they were folder
  ## paths). This uses the same rstudioapi::selectDirectory() dialog
  ## 01_biomass.R/02_fisheries.R/03_pbqb-traits.R/04_diets.R already use
  ## for their own out_dir/pcloud_dir/git_dir fallback - a real OS
  ## window, immune to console-echo issues, and consistent with the
  ## rest of the pipeline. Falls through to the old warn-and-use-
  ## Daniel's-defaults behavior when RStudio isn't available at all
  ## (e.g. a plain `Rscript` call with no RStudio session behind it) -
  ## see the final `else` branch below.
  pick_dir <- function(title, message_text) {
    repeat {
      rstudioapi::showQuestion(title = title, message = message_text)
      picked <- rstudioapi::selectDirectory()
      if (is.null(picked) || picked == "") {
        stop("[run_pipeline_demo.R] No folder selected for '", title, "' - aborting (re-run to try again).",
             call. = FALSE)
      }
      if (dir.exists(picked)) return(picked)
      message("[run_pipeline_demo.R] '", picked, "' doesn't exist - try again.")
    }
  }
  
  message("\n[run_pipeline_demo.R] No config.R found next to this script - a folder-picker window will",
          " open for each of the three paths below (this only needs to happen once - they'll be saved",
          " to config.R for next time).\n")
  out_dir    <- pick_dir("Select Output Directory",
                         "Please select the (writable) folder where pipeline outputs and intermediate results will be saved.")
  pcloud_dir <- pick_dir("Select pCloud EwE West Med Directory",
                         "Please select your local 'pCloud Drive/EwE Western Med 2026' folder.")
  git_dir    <- pick_dir("Select WMed_EwE Repo Directory",
                         "Please select your local clone of the WMed_EwE repo (the folder containing a 'scripts/' subfolder).")
  
  save_config <- tryCatch(
    rstudioapi::showQuestion(title = "Save these paths?",
                             message = "Save these three paths to config.R next to this script, so you aren't asked again?",
                             ok = "Yes", cancel = "No"),
    error = function(e) TRUE  # if showQuestion() itself errors for some reason, default to "just save it"
  )
  if (isTRUE(save_config)) {
    config_lines <- c(
      "## config.R - generated by run_pipeline_demo.R's interactive folder-picker prompt.",
      "## Edit any of these three paths by hand any time, or delete this file",
      "## to be prompted again on the next run. Never committed (see .gitignore).",
      "",
      sprintf('out_dir <- "%s"', out_dir),
      sprintf('pcloud_dir <- "%s"', pcloud_dir),
      sprintf('git_dir <- "%s"', git_dir),
      ""
    )
    writeLines(config_lines, "config.R")
    message("[run_pipeline_demo.R] Saved to config.R - future runs will use these automatically",
            " (edit or delete that file any time).")
  } else {
    message("[run_pipeline_demo.R] Not saved - you'll be asked again next run.")
  }
  
} else {
  warning(
    "[run_pipeline_demo.R] No config.R found next to this script, and RStudio isn't available to pop up\n",
    "a folder picker (this looks like a plain `Rscript` call) - falling back to Daniel's own hardcoded\n",
    "default paths below, which almost certainly don't exist on your machine.\n",
    "Copy config.R.example to config.R (same folder) and edit its three paths to your own\n",
    "out_dir/pcloud_dir/git_dir before running this again - or run this script from RStudio (Source)",
    " to get the folder-picker prompts instead.",
    call. = FALSE
  )
  out_dir    <- "/Users/daniel/Work/iMARES/WMed EwE Model/output/"
  pcloud_dir <- "/Users/daniel/pCloud Drive/EwE Western Med 2026/"
  git_dir    <- "/Users/daniel/Documents/GitHub/WMed_EwE/"
}

## This check guards against a failure mode where pbqb-traits/diet/
## validation plots silently weren't created. Every one of 01/02/03/
## 04's own "SOURCE-ABLE SCRIPT" guards (see their Configuration sections)
## trusts out_dir/pcloud_dir/git_dir blindly the moment they're already set
## in the calling environment - it never checks they actually exist on
## THIS machine. Daniel's own hardcoded paths above are the out-of-the-box
## default, so anyone else running this driver without editing them first
## silently inherits paths that don't exist (and, for git_dir, usually
## can't even be created - it's someone else's home directory). The
## failure then surfaces much later and confusingly: 01_biomass.R may
## still work if you already have your own out_dir cached from an earlier
## interactive run, while 02/03/04 - sourced fresh from git_dir here -
## error out trying to open a script or write a plot under a path that
## isn't yours, and NOTHING downstream of that source() call runs
## (including every 03_pbqb-traits.R / 04_diets.R plot).
##
## Fix: edit the three lines above to YOUR OWN out_dir/pcloud_dir/git_dir
## before running. The check below fails fast, at the top of this script,
## with a clear message - instead of a cryptic "cannot open file" deep
## inside some later ggsave() call.
missing_paths <- c(
  if (!dir.exists(out_dir)) sprintf("  out_dir    = \"%s\"", out_dir),
  if (!dir.exists(pcloud_dir)) sprintf("  pcloud_dir = \"%s\"", pcloud_dir),
  if (!dir.exists(git_dir)) sprintf("  git_dir    = \"%s\"", git_dir)
)
if (length(missing_paths) > 0) {
  stop(
    "run_pipeline_demo.R: the following path(s) don't exist on this machine:\n",
    paste(missing_paths, collapse = "\n"), "\n\n",
    "These are still set to Daniel's own machine (the placeholder defaults ",
    "near the top of this file). Edit those three lines (out_dir/pcloud_dir/",
    "git_dir, just above this check) to point at YOUR OWN output folder, ",
    "pCloud folder, and local clone of the WMed_EwE repo, then re-run. ",
    "Nothing has been sourced yet, so no partial/corrupted output was written."
  )
}
required_scripts <- file.path(git_dir, "scripts", c("01_biomass.R", "02_fisheries.R", "03_pbqb-traits.R", "04_diets.R"))
missing_scripts <- required_scripts[!file.exists(required_scripts)]
if (length(missing_scripts) > 0) {
  stop(
    "run_pipeline_demo.R: git_dir (\"", git_dir, "\") exists but is missing:\n",
    paste0("  ", missing_scripts, collapse = "\n"), "\n\n",
    "Check git_dir points at the root of your WMed_EwE clone (the folder ",
    "that itself contains a 'scripts/' subfolder), and that you've pulled ",
    "the latest scripts."
  )
}

if (RUN_MODE == "westmed_default") {
  
  ## -----------------------------------------------------------------
  ## (A) WEST MED DEFAULT - do not set FILTER_AREAS/TARGET_COUNTRIES/
  ## START_YEAR/END_YEAR/YEAR_ECOPATH/TS_YEARS at all here. Each script
  ## falls through to its own built-in Western Med default (GSA 1-11,
  ## Spain/France/Italy/Tunisia/Algeria/Morocco) - specifically:
  ##   YEAR_ECOPATH = 1994:1996 (Ecopath baseline - 3-year average snapshot)
  ##   TS_YEARS/START_YEAR/END_YEAR = 1995:2023 (Ecosim time series)
  ## THIS is the one enforced default for the West Med model - every
  ## numbered script's own `if (!exists(...))` guard
  ## resolves to these exact values whenever nothing overrides them, so
  ## there is no separate place these need to be kept in sync. Nothing
  ## below is new behavior, it's just being triggered by source() from
  ## this driver instead of by opening each script directly.
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

## Validation step - compares the just-written
## Ecopath_B/L/Di/PBQB against the OLD West Med workbook's "estimates"
## sheet and consolidates every plausibility/REVIEW csv the four steps
## above already wrote into one summary. Optional and best-effort: a
## missing/unreachable old workbook (different machine, wrong path)
## shouldn't fail the whole pipeline run after 01-04 already succeeded,
## so this is wrapped in tryCatch rather than a plain source() like the
## four required steps above. Set RUN_VALIDATION <- FALSE before
## sourcing this file to skip it entirely (e.g. no access to the old
## workbook on this machine at all).
if (!exists("RUN_VALIDATION", envir = .GlobalEnv, inherits = FALSE)) RUN_VALIDATION <- TRUE
if (RUN_VALIDATION) {
  message("--- Step 5/5: source(05_validation.R) [optional - old-workbook comparison + validation summary] ---")
  tryCatch(
    source(file.path(git_dir, "scripts/05_validation.R")),
    error = function(e) message("05_validation.R failed - the rest of the pipeline already ran successfully,",
                                " so this is reported rather than stopping the run: ", conditionMessage(e))
  )
} else {
  message("--- Step 5/5: skipped (RUN_VALIDATION = FALSE) ---")
}

message("\n=== Pipeline run complete (RUN_MODE = '", RUN_MODE, "'). ",
        "Outputs written under: ", out_dir, " ===\n")

## =================================================================
## Auto-launch the Shiny/Quarto doc after the pipeline finishes, instead
## of leaving that as a separate manual step.
##
## The one Shiny/Quarto artifact referenced elsewhere in this pipeline
## is pipeline_documentation_shiny.qmd (see 01_biomass.R's
## SAVE_SHINY_CACHE_PATH/STOP_AFTER_SHINY_CACHE comments) - a Quarto doc
## with a Shiny runtime that reads the just-saved Shiny data cache
## (SAVE_SHINY_CACHE_PATH, if you set it before this run) for its
## reactive "Try it" figures. No fixed path for that file was given
## anywhere in the scripts this driver sources, so AUTO_LAUNCH_SHINY_DOC_
## PATH below is a best guess at the repo layout (alongside this
## script's own git_dir) - update it once to your real path if it lives
## somewhere else (e.g. under a docs/ subfolder), and this will keep
## working on every future run without touching this block again.
##
## Best-effort and opt-in-by-default: set AUTO_LAUNCH_SHINY_DOC <- FALSE
## before sourcing this script to skip it entirely (e.g. running
## headless/non-interactively, or SAVE_SHINY_CACHE_PATH wasn't set this
## run so the doc would open with stale/no cached data). Never fails the
## pipeline run itself - every launch attempt is wrapped in tryCatch, and
## this whole block runs only AFTER every pipeline step above has
## already completed.
if (!exists("AUTO_LAUNCH_SHINY_DOC", envir = .GlobalEnv, inherits = FALSE)) AUTO_LAUNCH_SHINY_DOC <- TRUE
if (!exists("AUTO_LAUNCH_SHINY_DOC_PATH", envir = .GlobalEnv, inherits = FALSE)) {
  AUTO_LAUNCH_SHINY_DOC_PATH <- file.path(git_dir, "scripts", "pipeline_documentation_shiny.qmd")
}

if (AUTO_LAUNCH_SHINY_DOC) {
  tryCatch({
    if (!file.exists(AUTO_LAUNCH_SHINY_DOC_PATH)) {
      message("[Auto-launch] AUTO_LAUNCH_SHINY_DOC_PATH ('", AUTO_LAUNCH_SHINY_DOC_PATH, "') not found -",
              " skipping. Set AUTO_LAUNCH_SHINY_DOC_PATH to this file's real location before sourcing",
              " this script (or set AUTO_LAUNCH_SHINY_DOC <- FALSE to silence this message).")
    } else if (requireNamespace("quarto", quietly = TRUE)) {
      ## quarto_preview() renders + opens in the system browser (or the
      ## RStudio Viewer, if running inside RStudio) and returns
      ## immediately without blocking the rest of the session, unlike
      ## quarto_render() followed by a separate open step.
      message("[Auto-launch] Opening ", AUTO_LAUNCH_SHINY_DOC_PATH, " via quarto::quarto_preview()...")
      quarto::quarto_preview(AUTO_LAUNCH_SHINY_DOC_PATH)
    } else if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
      ## No 'quarto' R package installed - fall back to just opening the
      ## .qmd file in the RStudio editor (one click away from its own
      ## "Render" button) rather than doing nothing silently.
      message("[Auto-launch] 'quarto' R package not installed - opening ", AUTO_LAUNCH_SHINY_DOC_PATH,
              " in the RStudio editor instead (click Render to view it). Install the quarto package",
              " (install.packages(\"quarto\")) for this to render/preview automatically next time.")
      rstudioapi::navigateToFile(AUTO_LAUNCH_SHINY_DOC_PATH)
    } else {
      message("[Auto-launch] Neither the 'quarto' R package nor RStudio is available in this session -",
              " can't auto-open ", AUTO_LAUNCH_SHINY_DOC_PATH, ". Open/render it manually, or install",
              " the quarto package / run this from RStudio for automatic launch next time.")
    }
  }, error = function(e) {
    message("[Auto-launch] Failed to open the Shiny/Quarto doc - ", conditionMessage(e),
            ". The pipeline run above already completed successfully regardless.")
  })
}