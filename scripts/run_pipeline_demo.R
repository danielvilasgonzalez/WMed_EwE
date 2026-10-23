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
## Last updated 2026-10-14.
## =================================================================

## ---- 0. Fresh session ----------------------------------------------
## Set CLEAN_START <- FALSE before sourcing this file to keep your
## workspace (e.g. to pass your own overrides from the console).
if (!exists("CLEAN_START", envir = .GlobalEnv, inherits = FALSE) || isTRUE(CLEAN_START)) {
  rm(list = setdiff(ls(envir = .GlobalEnv, all.names = TRUE), c("CLEAN_START", "RUN_STEPS")), envir = .GlobalEnv)
  invisible(gc())
}
options(warn = 1)
## RUN_STEPS: which scripts to run (1 = 01_biomass ... 5 = 05_validation).
## Default all; e.g. RUN_STEPS <- 5 redraws the validation plots from the
## outputs already on disk (used by medbs_pipeline_methodology.qmd).
if (!exists("RUN_STEPS", envir = .GlobalEnv, inherits = FALSE)) RUN_STEPS <- 1:5

RUN_MODE <- "westmed_default"   # "westmed_default" | "custom_example"

## ---- 1. Paths ------------------------------------------------------
## out_dir    = writable output folder (local to each laptop)
## pcloud_dir = the pCloud Drive folder "EwE Western Med 2026"
## git_dir    = the WMed_EwE repo (folder with scripts/)
## Found by wmed_paths.R on any laptop with the repo and pCloud Drive:
## config.local.R (this computer, git-ignored), then config.R, then the
## standard pCloud Drive location; out_dir defaults to ~/WMed_EwE_output.
## If the pCloud folder is not found, RStudio asks for it once and saves
## it to config.local.R.
.this_dir <- tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) getwd())
if (!file.exists(file.path(.this_dir, "wmed_paths.R")) && file.exists(file.path(.this_dir, "scripts", "wmed_paths.R")))
  .this_dir <- file.path(.this_dir, "scripts")
source(file.path(.this_dir, "wmed_paths.R"))
.wp <- wmed_set_paths(git_dir = dirname(.this_dir))
if (!isTRUE(.wp$ok) && requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  rstudioapi::showDialog("pCloud folder", paste0("Select your pCloud folder '", .wmed_pcloud_name,
                                                 "'. It is saved in config.local.R for this computer only."))
  .picked <- rstudioapi::selectDirectory()
  if (!is.null(.picked) && dir.exists(.picked)) {
    wmed_save_local_config(dirname(.this_dir), pcloud_dir = .picked)
    .wp <- wmed_set_paths(git_dir = dirname(.this_dir))
  }
}
if (!isTRUE(.wp$ok)) stop("[run_pipeline_demo.R] ", .wp$problem, "Nothing was run.", call. = FALSE)

.missing <- c(if (!dir.exists(out_dir)) paste0("out_dir = ", out_dir),
              if (!dir.exists(pcloud_dir)) paste0("pcloud_dir = ", pcloud_dir),
              if (!dir.exists(git_dir)) paste0("git_dir = ", git_dir))
if (length(.missing)) stop("[run_pipeline_demo.R] These folders do not exist on this machine:\n  ",
                           paste(.missing, collapse = "\n  "), "\nFix config.local.R (this computer) and run again. Nothing was run.",
                           call. = FALSE)
.scripts <- file.path(git_dir, "scripts", c("01_biomass.R", "02_fisheries.R", "03_pbqb-traits.R",
                                            "04_diets.R", "05_validation.R"))
if (any(!file.exists(.scripts))) stop("[run_pipeline_demo.R] Missing in git_dir:\n  ",
                                      paste(.scripts[!file.exists(.scripts)], collapse = "\n  "), call. = FALSE)

## ---- 2. Run settings -----------------------------------------------
if (RUN_MODE == "westmed_default") {
  ## Western Med model. Model area and years are set here (all scripts
  ## read them); change them to filter the area or the period.
  ##
  ## MODEL_GSAS: GSAs that make up the model area. Default = the whole
  ##   Western Med (GSAs 1-11) plus GSA 12 (Northern Tunisia, part of the
  ##   MSFD Western Mediterranean subregion). Filter with e.g. c(6, 7) (Northern Spain +
  ##   Gulf of Lion). Survey data are used only for these GSAs; a GSA the
  ##   surveys do not cover (3 Southern Alboran, 4 Algeria, 12 Northern Tunisia) takes the survey
  ##   density per km2 of the others (ASSUMPTION). Catch is kept for every
  ##   coastal state of these GSAs (GSA 3 Morocco, 4 Algeria, 1-2 and 5-6
  ##   Spain, 7-8 France, 9-11 Italy, 12 Tunisia).
  ## MODEL_AREA_BASIS: "whole_gsa" = A_model is the whole sea area of
  ##   MODEL_GSAS, coast to bottom (depths outside 10-800 m filled with
  ##   AquaMaps); "medits_10_800" = only the 10-800 m MEDITS strata.
  ## A value already set before this file is sourced wins (e.g. from
  ## medbs_pipeline_methodology.qmd's params, with CLEAN_START <- FALSE).
  .set_default <- function(name, value) if (!exists(name, envir = .GlobalEnv, inherits = FALSE)) assign(name, value, envir = .GlobalEnv)
  .set_default("MODEL_GSAS",       1:12)
  .set_default("MODEL_AREA_BASIS", "whole_gsa")
  .set_default("YEAR_ECOPATH",     1994:1996)   # Ecopath baseline (01, 02, 03)
  .set_default("TS_YEARS",         1995:2023)   # Ecosim biomass series (01)
  ## Catch / effort series (02): follow the Ecopath and Ecosim years -
  ## from the first Ecopath or Ecosim year to the last Ecosim year.
  .set_default("START_YEAR",       min(c(YEAR_ECOPATH, TS_YEARS)))
  .set_default("END_YEAR",         max(TS_YEARS))
  ##
  ## Other settings, with their current default values - uncomment a line
  ## to change it for this run only:
  ##
  ## All scripts
  # FG_SPECIES_FILE               <- file.path(pcloud_dir, "data/FG_WMed_2026.csv")   # species -> FG table (FG_number, FG_name, species, taxonomy)
  # APPLY_CATCHABILITY            <- TRUE   # MEDITS catchability correction (q)
  # CATCHABILITY_FILE             <- "my_q.csv"   # own catchability table (ScientificName, q)
  # PLOT_DPI                      <- 300    # PNG resolution of every plot; 150 = smaller files, blurrier when zoomed
  ##
  ## Biomass (01)
  # STOCK_ASSESSMENT_SOURCE       <- "GFCM"   # stock assessments: "GFCM" = newest GFCM SAFs, downloaded from the GFCM website and read automatically (default; check biomass/gfcm_saf_parse_REVIEW.csv); "RAM" = RAM Legacy (combined_medbs_star_ramlegacy.csv)
  # GFCM_SAF_UPDATE               <- TRUE     # FALSE = reuse the last GFCM SAF download (no internet needed)
  # ECOPATH_B_FILL_MODE           <- "window_mean"  # FG with no biomass in YEAR_ECOPATH: mean over ECOPATH_B_FILL_YEARS (else nearest year); "nearest" = nearest year only
  # ECOPATH_B_FILL_YEARS          <- 1995:2000
  # ECOPATH_B_EXPANDING_MIN       <- 0.00001  # Ecopath_B of the "Expanding" FGs (t/km2); NA = leave missing
  # DROP_OUTLIERS                 <- TRUE   # remove flagged outlier hauls (haul-level rule)
  # SURVEY_IMPUTE_UNSAMPLED_CELLS <- TRUE   # unsampled GSA left out of that year; unsampled strata of a sampled GSA filled from the same GSA
  # AQUAMAPS_B_MAX_DEPTH          <- Inf    # AquaMaps pseudo-strata biomass counted down to this depth (whole_gsa: all depths; medits_10_800: 1000)
  # ECOSIM_HAUL_CAP_Q             <- 0.95   # Ecosim trend only: cap haul FG density at this quantile (GSA x stratum); NA = off
  # ECOSIM_MIN_AREA_COVERAGE      <- 0      # Ecosim trend only: blank survey years below this sampled-area share; 0 = off
  # ECOSIM_SPIKE_FILTER           <- TRUE   # Ecosim B series: blank isolated upward spikes
  # ECOPATH_B_HAUL_CAP_FGS        <- c("Commercial small demersal fish")  # Ecopath_B baseline also from the haul-capped index
  # GEL_MACRO_SOURCE              <- "datasets"  # Jellyfish, Salps (Luo et al. 2020) and Macro zooplankton (MAREDAT); "none" = old behaviour
  # MACRO_C_FRACTION_WW           <- 0.04   # macrozooplankton C as share of WW (krill: 0.086)
  # ECOPATH_B_FROM_EE             <- c("Suprabenthos", "Other macro-benthos", "Benthic mollusc")  # B left for Ecopath (EE_input); default adds the gelatinous/macro FGs only when their dataset estimate is missing
  # ECOPATH_B_EE_VALUE            <- 0.95
  ##
  ## Fisheries (02)
  # CATCH_FDI_FIRST       <- TRUE    # EU catch = STECF FDI where reported (2014+); GFCM (scaled to FDI) before and for non-EU countries
  # MOLLUSC_NEI_SPLIT     <- TRUE    # "Marine molluscs nei" split by each country's identified mollusc catch
  ##
  ## P/B, Q/B (03)
  # ENABLE_ECOBASE_QUERY  <- TRUE    # FALSE = no EcoBase network call
  # ECOBASE_FORCE_REFRESH <- FALSE   # TRUE = re-query EcoBase instead of the cached CSV
  ##
  ## Validation (05)
  # RUN_VALIDATION <- TRUE
  if (!identical(get0("AREA_MODE", envir = .GlobalEnv, ifnotfound = "westmed"), "custom"))
  message("\n=== Running the Western Med pipeline: GSAs ", paste(MODEL_GSAS, collapse = ", "), " (", MODEL_AREA_BASIS, "), Ecopath ",
          paste(range(YEAR_ECOPATH), collapse = "-"), ", Ecosim ", paste(range(TS_YEARS), collapse = "-"), " ===\n")

} else if (RUN_MODE == "custom_example") {
  ## Example of another region / period (Adriatic GSAs 17-18).
  MODEL_GSAS       <- 17:18
  MODEL_AREA_BASIS <- "whole_gsa"
  TARGET_COUNTRIES_OVERRIDE <- c("Italy", "Croatia", "Slovenia")   # 02_fisheries.R
  YEAR_ECOPATH     <- 2005:2007                            # same baseline for 01, 02, 03
  TS_YEARS         <- 2000:2020                            # 01_biomass.R
  START_YEAR       <- 2000                                 # 02_fisheries.R
  END_YEAR         <- 2020
  message("\n=== Running a CUSTOM region (GSA ", paste(MODEL_GSAS, collapse = ", "), ", ",
          START_YEAR, "-", END_YEAR, ") ===\n")
} else {
  stop("Unknown RUN_MODE '", RUN_MODE, "' - use 'westmed_default' or 'custom_example'.")
}
## ---- Custom region (AREA_MODE = "custom", set on the page) ------------
## All five steps run for the region, with every output in out_dir/<AREA_NAME>:
## 01 the survey biomass inside the region; 02 the catch of the GSA(s) the
## region overlaps (custom_gsa_overlap.csv, per km2); 03-05 then use the
## region's own biomass, species shares and catch, so P/B, Q/B, F and the
## diet weights follow the region.
.custom_run <- identical(get0("AREA_MODE", envir = .GlobalEnv, ifnotfound = "westmed"), "custom")
if (.custom_run) {
  if (!exists("AREA_NAME", envir = .GlobalEnv, inherits = FALSE)) AREA_NAME <- "custom_region"
  AREA_NAME <- gsub("[^A-Za-z0-9_-]", "_", AREA_NAME)
  if (basename(sub("/+$", "", out_dir)) != AREA_NAME) out_dir <- file.path(sub("/+$", "", out_dir), AREA_NAME)
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  if (exists("MODEL_GSAS", envir = .GlobalEnv, inherits = FALSE)) rm(MODEL_GSAS, envir = .GlobalEnv)   # 01 uses the region's own polygons
  FILTER_AREAS <- NULL
  message("\n=== Custom region '", AREA_NAME, "': steps ", paste(RUN_STEPS, collapse = ", "), "; outputs in ", out_dir, " ===\n")
} else {
  FILTER_AREAS <- MODEL_GSAS   # the survey/catch filters of 01 and 02 follow the model GSAs
}
## Custom region, before 02: the catch GSAs from 01's overlap table
.set_custom_catch_gsas <- function() {
  ov <- file.path(out_dir, "biomass", "custom_gsa_overlap.csv")
  if (!file.exists(ov)) stop("[run_pipeline_demo.R] ", ov, " not found - run step 1 (01_biomass.R) for this region first.", call. = FALSE)
  ov <- data.table::fread(ov)
  assign("MODEL_GSAS", sort(ov[used_for_catch == TRUE, GSA]), envir = .GlobalEnv)
  assign("FILTER_AREAS", get("MODEL_GSAS", envir = .GlobalEnv), envir = .GlobalEnv)
  message("[Custom region] Catch from GSA(s) ", paste(get("MODEL_GSAS", envir = .GlobalEnv), collapse = ", "), " (custom_gsa_overlap.csv).")
}

## ---- 3. Run the five steps -----------------------------------------
## Each step runs in the global environment (so later steps see the
## objects and settings of earlier ones). A failing step stops the run
## and names the step and its log. 05 is optional and never stops the run.
.t0 <- Sys.time()
## Progress file (set WMED_PROGRESS_FILE, as the Run button of
## medbs_pipeline_methodology.qmd does): one line when a step starts and
## one when it ends, written with base::cat so the scripts' own logging
## cannot redirect it.
.progress_file <- Sys.getenv("WMED_PROGRESS_FILE", "")
.progress <- function(txt) if (nzchar(.progress_file)) base::cat(txt, "\n", sep = "", file = .progress_file, append = TRUE)
.run_step <- function(i, script, note = "") {
  message(sprintf("\n--- Step %d/5: %s %s---", i, script, note))
  .progress(sprintf("--- Step %d/5: %s", i, script))
  t <- Sys.time()
  ## Each script sets options(error = ...) to write errors to its log; put
  ## the session's own handler back afterwards, or every later error in this
  ## R session (e.g. running a .qmd chunk) prints a stale "ERROR: ..." line.
  .old_error_opt <- getOption("error")
  on.exit(options(error = .old_error_opt), add = TRUE)
  tryCatch(source(file.path(git_dir, "scripts", script), local = globalenv()),
           error = function(e) {
             while (sink.number() > 0) sink()   # close the step's log so the error shows in the console
             stop(sprintf("Step %d (%s) failed: %s\nSee %s for the full log.", i, script, conditionMessage(e),
                          file.path(out_dir, paste0(format(Sys.Date(), "%Y%m%d"), "_", sub("\\.R$", "", script), "_log.txt"))),
                  call. = FALSE)
           })
  while (sink.number() > 0) sink()
  message(sprintf("--- Step %d/5 done in %.1f min ---", i, as.numeric(difftime(Sys.time(), t, units = "mins"))))
  .progress(sprintf("--- Step %d/5 done in %.1f min", i, as.numeric(difftime(Sys.time(), t, units = "mins"))))
}

if (1 %in% RUN_STEPS) .run_step(1, "01_biomass.R", "[MEDITS/MEDIAS + unsurveyed groups; the first run after a strata change rebuilds the strata areas from NOAA bathymetry - needs internet] ")
if (.custom_run && any(2:5 %in% RUN_STEPS)) .set_custom_catch_gsas()
if (2 %in% RUN_STEPS) .run_step(2, "02_fisheries.R", "[catch, landings, discards, effort, F] ")
if (3 %in% RUN_STEPS) .run_step(3, "03_pbqb-traits.R", "[P/B, Q/B, traits; includes the EcoBase query] ")
if (4 %in% RUN_STEPS) .run_step(4, "04_diets.R", "[diet matrix] ")

if (!exists("RUN_VALIDATION", envir = .GlobalEnv, inherits = FALSE)) RUN_VALIDATION <- TRUE
if (isTRUE(RUN_VALIDATION) && 5 %in% RUN_STEPS) {
  tryCatch(.run_step(5, "05_validation.R", "[comparison with the 1995 model + all checks] "),
           error = function(e) message("05_validation.R failed (steps 1-4 are complete): ", conditionMessage(e)))
} else {
  message("--- Step 5/5: skipped (RUN_VALIDATION = FALSE) ---")
}

message(sprintf("\n=== Pipeline finished in %.1f min. Workbook: %s ===",
                as.numeric(difftime(Sys.time(), .t0, units = "mins")),
                file.path(out_dir, "ecopath_ecosim_inputs.xlsx")))
message("Start with: plots/validation/validation_plots_ALL.pdf and validation/validation_summary_ALL.csv\n")
