## =================================================================
## run_pipeline_demo.R
##
## Runs the whole WMed EwE input pipeline in one go:
##   01_biomass.R -> 02_fisheries.R -> 03_pbqb-traits.R -> 04_diets.R
##   -> 05_validation.R
## The order matters: 02 needs Ecopath_B and strata_area_by_area.csv from
## 01 (F = C / B), 03 needs F_by_fg.csv from 02 (P/B = M + F), 04 needs
## the species biomass shares from 01, and 05 reads everything.
## 01 sources 01b_biomass_unsurveyed.R; 03 sources 03b_ecobase.R.
##
## Each script only sets a configuration variable when it does not exist
## yet ("SOURCE-ABLE SCRIPT" blocks), so any value set here before
## source() overrides the script default. To make sure a value left over
## from an earlier run in the same R session does not silently override
## a new default, the global environment is cleared at the start
## (CLEAN_START = TRUE).
##
## Every script writes its own log (output/YYYYMMDD_<script>_log.txt,
## overwritten by a rerun on the same day).
##
## Last updated 2026-10-08 (12:30).
## =================================================================

## ---- 0. Fresh session ----------------------------------------------
## Set CLEAN_START <- FALSE before sourcing this file to keep your
## workspace (e.g. to pass your own overrides from the console).
if (!exists("CLEAN_START", envir = .GlobalEnv, inherits = FALSE) || isTRUE(CLEAN_START)) {
  rm(list = setdiff(ls(envir = .GlobalEnv, all.names = TRUE), "CLEAN_START"), envir = .GlobalEnv)
  invisible(gc())
}
options(warn = 1)

RUN_MODE <- "westmed_default"   # "westmed_default" | "custom_example"

## ---- 1. Paths ------------------------------------------------------
## out_dir    = writable output folder
## pcloud_dir = local "pCloud Drive/EwE Western Med 2026" folder
## git_dir    = local clone of the WMed_EwE repo (folder with scripts/)
## Read from config.R next to this script (copy config.R.example and edit
## it; config.R is git-ignored). Without config.R, RStudio opens a folder
## picker for each path and offers to save them to config.R.
.this_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) getwd())
.config <- c(file.path(.this_dir, "config.R"), file.path(dirname(.this_dir), "config.R"), "config.R")
.config <- .config[file.exists(.config)][1]

if (!is.na(.config)) {
  message("[run_pipeline_demo.R] Using paths from ", .config)
  source(.config)
} else if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  pick_dir <- function(title, message_text) {
    rstudioapi::showQuestion(title = title, message = message_text)
    picked <- rstudioapi::selectDirectory()
    if (is.null(picked) || picked == "" || !dir.exists(picked)) {
      stop("[run_pipeline_demo.R] No valid folder selected for '", title, "' - run again.", call. = FALSE)
    }
    picked
  }
  out_dir    <- pick_dir("Output folder", "Select the folder where the pipeline outputs are written.")
  pcloud_dir <- pick_dir("pCloud data folder", "Select your 'pCloud Drive/EwE Western Med 2026' folder.")
  git_dir    <- pick_dir("WMed_EwE repo", "Select your local clone of the WMed_EwE repo (the folder with 'scripts/').")
  if (isTRUE(tryCatch(rstudioapi::showQuestion("Save paths?", "Save these paths to config.R so you are not asked again?",
                                               ok = "Yes", cancel = "No"), error = function(e) TRUE))) {
    writeLines(c("## config.R - written by run_pipeline_demo.R. Not committed (.gitignore).",
                 sprintf('out_dir    <- "%s"', out_dir),
                 sprintf('pcloud_dir <- "%s"', pcloud_dir),
                 sprintf('git_dir    <- "%s"', git_dir)),
               file.path(.this_dir, "config.R"))
    message("[run_pipeline_demo.R] Paths saved to ", file.path(.this_dir, "config.R"))
  }
} else {
  stop("[run_pipeline_demo.R] No config.R found and RStudio is not available for a folder picker.\n",
       "Copy config.R.example to config.R (next to this script), set out_dir, pcloud_dir and git_dir, and run again.",
       call. = FALSE)
}

.missing <- c(if (!dir.exists(out_dir)) paste0("out_dir = ", out_dir),
              if (!dir.exists(pcloud_dir)) paste0("pcloud_dir = ", pcloud_dir),
              if (!dir.exists(git_dir)) paste0("git_dir = ", git_dir))
if (length(.missing)) stop("[run_pipeline_demo.R] These folders do not exist on this machine:\n  ",
                           paste(.missing, collapse = "\n  "), "\nFix config.R and run again. Nothing was run.",
                           call. = FALSE)
.scripts <- file.path(git_dir, "scripts", c("01_biomass.R", "02_fisheries.R", "03_pbqb-traits.R",
                                            "04_diets.R", "05_validation.R"))
if (any(!file.exists(.scripts))) stop("[run_pipeline_demo.R] Missing in git_dir:\n  ",
                                      paste(.scripts[!file.exists(.scripts)], collapse = "\n  "), call. = FALSE)

## ---- 2. Run settings -----------------------------------------------
if (RUN_MODE == "westmed_default") {
  ## Western Med model (GSAs 1, 2, 5-11; Spain, France, Italy;
  ## Ecopath 1994-1996; Ecosim 1995-2023). Nothing needs to be set: each
  ## script uses its own defaults. The main ones, with their current
  ## default values - uncomment a line to change it for this run only:
  ##
  ## Biomass (01)
  # DROP_OUTLIERS                 <- TRUE   # remove flagged outlier hauls (haul-level rule)
  # SURVEY_IMPUTE_UNSAMPLED_CELLS <- TRUE   # unsampled GSA left out of that year; unsampled strata of a sampled GSA filled from the same GSA
  # AQUAMAPS_B_MAX_DEPTH          <- 1000   # AquaMaps pseudo-strata biomass counted down to this depth (bottom trawling banned below 1000 m)
  # ECOSIM_HAUL_CAP_Q             <- 0.95   # Ecosim trend only: cap haul FG density at this quantile (GSA x stratum); NA = off
  # ECOSIM_MIN_AREA_COVERAGE      <- 0      # Ecosim trend only: blank survey years below this sampled-area share; 0 = off
  # ECOSIM_SPIKE_FILTER           <- TRUE   # Ecosim B series: blank isolated upward spikes
  # ECOPATH_B_HAUL_CAP_FGS        <- c("Commercial small demersal fish")  # Ecopath_B baseline also from the haul-capped index
  # GEL_MACRO_SOURCE              <- "datasets"  # Jellyfish, Salps (Luo et al. 2020) and Macro zooplankton (MAREDAT); "none" = old behaviour
  # MACRO_C_FRACTION_WW           <- 0.04   # macrozooplankton C as share of WW (krill: 0.086)
  # ECOPATH_B_FROM_EE             <- c("Suprabenthos", "Other macro-benthos", "Benthic mollusc")  # B left for Ecopath (EE_input); default adds the gelatinous/macro FGs only when their dataset estimate is missing
  # ECOPATH_B_EE_VALUE            <- 0.95
  ##
  ## P/B, Q/B (03)
  # ENABLE_ECOBASE_QUERY  <- TRUE    # FALSE = no EcoBase network call
  # ECOBASE_FORCE_REFRESH <- FALSE   # TRUE = re-query EcoBase instead of the cached CSV
  ##
  ## Validation (05)
  # RUN_VALIDATION <- TRUE
  message("\n=== Running the Western Med pipeline ===\n")

} else if (RUN_MODE == "custom_example") {
  ## Example of another region / period (Adriatic GSAs 17-18).
  FILTER_AREAS     <- 17:18
  TARGET_COUNTRIES <- c("Italy", "Croatia", "Slovenia")   # 02_fisheries.R
  YEAR_ECOPATH     <- 2005:2007                            # same baseline for 01, 02, 03
  TS_YEARS         <- 2000:2020                            # 01_biomass.R
  START_YEAR       <- 2000                                 # 02_fisheries.R
  END_YEAR         <- 2020
  message("\n=== Running a CUSTOM region (GSA ", paste(FILTER_AREAS, collapse = ", "), ", ",
          START_YEAR, "-", END_YEAR, ") ===\n")
} else {
  stop("Unknown RUN_MODE '", RUN_MODE, "' - use 'westmed_default' or 'custom_example'.")
}

## ---- 3. Run the five steps -----------------------------------------
## Each step runs in the global environment (so later steps see the
## objects and settings of earlier ones). A failing step stops the run
## and names the step and its log. 05 is optional and never stops the run.
.t0 <- Sys.time()
.run_step <- function(i, script, note = "") {
  message(sprintf("\n--- Step %d/5: %s %s---", i, script, note))
  t <- Sys.time()
  tryCatch(source(file.path(git_dir, "scripts", script), local = globalenv()),
           error = function(e) {
             while (sink.number() > 0) sink()   # close the step's log so the error shows in the console
             stop(sprintf("Step %d (%s) failed: %s\nSee %s for the full log.", i, script, conditionMessage(e),
                          file.path(out_dir, paste0(format(Sys.Date(), "%Y%m%d"), "_", sub("\\.R$", "", script), "_log.txt"))),
                  call. = FALSE)
           })
  while (sink.number() > 0) sink()
  message(sprintf("--- Step %d/5 done in %.1f min ---", i, as.numeric(difftime(Sys.time(), t, units = "mins"))))
}

.run_step(1, "01_biomass.R", "[MEDITS/MEDIAS + unsurveyed groups; the first run after a strata change rebuilds the strata areas from NOAA bathymetry - needs internet] ")
.run_step(2, "02_fisheries.R", "[catch, landings, discards, effort, F] ")
.run_step(3, "03_pbqb-traits.R", "[P/B, Q/B, traits; includes the EcoBase query] ")
.run_step(4, "04_diets.R", "[diet matrix] ")

if (!exists("RUN_VALIDATION", envir = .GlobalEnv, inherits = FALSE)) RUN_VALIDATION <- TRUE
if (isTRUE(RUN_VALIDATION)) {
  tryCatch(.run_step(5, "05_validation.R", "[comparison with the 1995 model + all checks] "),
           error = function(e) message("05_validation.R failed (steps 1-4 are complete): ", conditionMessage(e)))
} else {
  message("--- Step 5/5: skipped (RUN_VALIDATION = FALSE) ---")
}

message(sprintf("\n=== Pipeline finished in %.1f min. Workbook: %s ===",
                as.numeric(difftime(Sys.time(), .t0, units = "mins")),
                file.path(out_dir, "ecopath_ecosim_inputs.xlsx")))
message("Start with: plots/validation/validation_plots_ALL.pdf and validation/validation_summary_ALL.csv\n")
