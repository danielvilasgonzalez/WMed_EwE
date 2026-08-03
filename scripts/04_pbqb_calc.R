## =================================================================
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
## Package loading - same pattern as 01_survey_density_westmed.R for
## the plain CRAN packages this script always needs. rfishbase is
## deliberately NOT in this list - see the version-compatibility
## check right below, which exists specifically because a naive
## install.packages("rfishbase") installs a broken pre-4.0 CRAN
## version. patchwork (used later for combining plots) is included
## here now too, rather than being loaded separately mid-script.
## =================================================================
pkgs <- c("data.table", "stringr", "ggplot2", "progress", "patchwork",
          "readxl", "openxlsx", "worrms", "purrr")
new_pkgs <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new_pkgs) > 0) install.packages(new_pkgs)
invisible(lapply(pkgs, library, character.only = TRUE))

## =================================================================
## STEP 1: Configuration
## =================================================================
if (tolower(Sys.info()[["user"]]) == "daniel") {
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
## 01_survey_density_westmed.R - everything this script produces lands
## somewhere under out_dir, nothing written to a separate location.
plot_dir <- file.path(out_dir, "plots")
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

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
PIPELINE_STAGES <- c(
  "Load species_df", "Taxonomic classification (WoRMS)",
  "Fetch 2a: species() traits", "Fetch 2b: growth params",
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
##             01_survey_density_westmed.R (species_density_regional_combined.csv),
##             reshaped via lib_build_species_df_from_survey.R. This is the
##             real data source - use this for actual model runs.
##  "test"   - the old test_species_df.rds placeholder. Kept only for
##             quick pipeline smoke-testing when survey outputs aren't
##             available/up to date.
SPECIES_DF_SOURCE <- "survey"

## SURVEY_OUT_DIR removed - it duplicated out_dir from STEP 1 above
## (both pointed at the same ".../WMed EwE Model/output/" folder).
## out_dir is used directly everywhere below instead.
SURVEY_DENSITY_CSV <- file.path(out_dir, "species_density_regional_combined.csv")

## Same shared workbook 01_survey_density_westmed.R and 02_fao_catches.R
## write to (order-independent - this can run before, after, or
## between those two). add_pbqb_to_ecopath_workbook() is defined in
## lib_survey_fg_density_functions.R, sourced here since this script
## doesn't otherwise need it.
source(file.path(git_dir, "scripts/lib_survey_fg_density_functions.R"))
ECOPATH_WORKBOOK_PATH <- file.path(out_dir, "ecopath_ecosim_inputs.xlsx")
## Must match YEAR_ECOPATH in 01_survey_density_westmed.R exactly - this is
## what defines the Biomass snapshot species_df's density values (and
## therefore fg_weighted's Biomass_FG below, and ecopath_ready's
## "Biomass in habitat area (t/km^2)") are drawn from. If that script's
## YEAR_ECOPATH ever changes, update this to match or the PB/QB weights
## and the Ecopath Biomass column will be describing two different
## time snapshots without any warning.
YEAR_ECOPATH <- 1994:1996

if (SPECIES_DF_SOURCE == "survey") {
  source(file.path(git_dir, "scripts/lib_build_species_df_from_survey.R"))
  species_df <- build_species_df_from_survey(
    survey_csv_path    = SURVEY_DENSITY_CSV,
    year_ecopath_range = YEAR_ECOPATH,
    out_rds_path        = file.path(out_dir, "survey_species_df.rds")
  )
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
YIELD_SOURCE <- "none"

## PLACEHOLDER path AND placeholder column names inside
## attach_yield_from_landings() below - the actual landings/catch data
## source for this project (STECF/GFCM data-call catch tables, national
## logbook data, FAO capture statistics, etc.) hasn't been identified
## yet. Point this at the real file once you have one, and confirm the
## required_cols list inside attach_yield_from_landings() matches its
## actual column names before trusting the output.
## Raw/reference data - lives under pcloud_dir, same convention as
## 01_survey_density_westmed.R's own fg_file/tm_list_file.
LANDINGS_CSV_PATH <- file.path(pcloud_dir, "data/landings_by_species_gsa_year.csv")

## area_lookup_csv_path expects strata_area_by_area.csv - written by
## 01_survey_density_westmed.R's compute_strata_area_by_area() cache
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
    stop("Landings file found but area_lookup_csv_path is missing at '", area_lookup_csv_path,
         "' - this should be strata_area_by_area.csv written by 01_survey_density_westmed.R.",
         " Needed to convert landings totals (t) into a density (t/km^2/year)",
         " comparable to species_df$Biomass. Run that script first, or point this",
         " at wherever it actually saved that file.")
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
    file.path(out_dir, "strata_area_by_area.csv"),
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
FG_YIELD_SOURCE <- "none"

## Filename must match DATASET_VERSION set in 02_fao_catches.R - default
## here assumes its default ("GFCM_2025"); update both if that changes.
## 02_fao_catches.R writes this into its own out_dir - the SAME
## canonical out_dir this script uses (Step 1 above), so no separate
## path guess is needed.
FG_CATCH_CSV_PATH <- file.path(out_dir, "fg_catch_timeseries_GFCM_2025.csv")

species_df[, Fmort_FG := NA_real_]   # populated below if FG_YIELD_SOURCE == "fg_catch_csv"

if (FG_YIELD_SOURCE == "fg_catch_csv") {
  area_lookup_path <- file.path(out_dir, "strata_area_by_area.csv")
  
  if (!file.exists(FG_CATCH_CSV_PATH)) {
    message("FG_YIELD_SOURCE = 'fg_catch_csv' but no file found at '", FG_CATCH_CSV_PATH,
            "' - run 02_fao_catches.R first (check DATASET_VERSION there matches the",
            " filename above). Every fish species' Fmort stays NA (M-only PB) until this exists.")
  } else if (!file.exists(area_lookup_path)) {
    message("FG_YIELD_SOURCE = 'fg_catch_csv' but strata_area_by_area.csv not found at '",
            area_lookup_path, "' - needed to convert FG catch totals (t) into a density",
            " (t/km^2/yr) comparable to Biomass. Run 01_survey_density_westmed.R first.",
            " Every fish species' Fmort stays NA (M-only PB).")
  } else {
    fg_catch <- fread(FG_CATCH_CSV_PATH)
    ## strata_area_by_area.csv was already built restricted to
    ## FILTER_AREAS (the Western Med GSAs actually used) by
    ## 01_survey_density_westmed.R - summed here as one region-wide total,
    ## same as how Biomass is a region-wide density everywhere else.
    area_total_km2 <- fread(area_lookup_path)[, sum(area_km2, na.rm = TRUE)]
    
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
} else {
  message("FG_YIELD_SOURCE = 'none' - every fish species' Fmort stays NA (M-only PB)",
          " unless a species-level Yield source is added separately above.")
}


## =================================================================
## STEP 1: taxonomic classification -> dispatch group
## Primary signal: WoRMS Class. Fallback signal for species where
## WoRMS returns no Class at all: whether the species has ANY
## FishBase record (fish-specific database) vs only SeaLifeBase -
## more robust than relying on Class string-matching alone.
## =================================================================

source(file.path(git_dir, "scripts/lib_worms_taxonomy_lookup.R"))

taxonomy <- as.data.table(worms_taxonomy_lookup(sp_list))[
  , .(Species = original_name, Genus = genus, Family = family, Order = order, Class = class, Phylum = phylum)
]

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

## --- Fallback for species with no Class: the usual cause is that the
## name is a SYNONYM - WoRMS records for synonym/unaccepted names often
## have an incomplete classification chain even though the full chain
## exists under the ACCEPTED name. Follow that resolution within WoRMS
## itself (synonym -> valid_AphiaID -> accepted record's classification)
## rather than falling back to an unrelated database as an indirect proxy.
unresolved <- taxonomy[is.na(dispatch_group), Species]
if (length(unresolved) > 0) {
  message("\n", length(unresolved), " species had no WoRMS Class from the initial lookup -",
          " attempting synonym -> accepted-name resolution via WoRMS.")
  
  resolve_via_accepted_name <- function(sp) {
    ## Strip trailing "spp."/"sp." the SAME way worms_taxonomy_lookup()'s
    ## main batch lookup already does - without this, a genus-level
    ## placeholder like "Sepiola spp." gets queried LITERALLY, which
    ## WoRMS's API always rejects (204 No Content, not a real taxon
    ## name) - guaranteed failure every time, and worse, it means these
    ## entries never actually get classified at all, just silently
    ## fall through to the "invertebrate" default below. Querying the
    ## bare genus instead ("Sepiola") can actually succeed and return
    ## real Class/Family/Order/Phylum - which is all dispatch_group
    ## classification needs anyway, species-level resolution isn't
    ## required for that.
    query_term <- str_trim(str_remove(sp, "\\s+spp?\\.?$"))
    Sys.sleep(1)  # space out requests in case WoRMS's API is rate-sensitive
    rec <- tryCatch(worrms::wm_records_names(query_term, marine_only = FALSE)[[1]], error = function(e) {
      message("  WoRMS lookup failed for '", sp, "' (queried as '", query_term, "'): ", conditionMessage(e))
      NULL
    })
    if (is.null(rec) || nrow(rec) == 0) return(NULL)
    rec <- rec[1, ]
    ## if this is a synonym/unaccepted record with its own classification
    ## missing, re-query WoRMS directly by the accepted name's AphiaID
    if (!is.na(rec$valid_AphiaID) && rec$valid_AphiaID != rec$AphiaID && is.na(rec$class)) {
      accepted <- tryCatch(worrms::wm_record(id = rec$valid_AphiaID), error = function(e) {
        message("  WoRMS accepted-name lookup failed for '", sp, "': ", conditionMessage(e))
        NULL
      })
      if (!is.null(accepted) && nrow(accepted) > 0) rec <- accepted[1, ]
    }
    data.table(Species = sp, Genus_r = rec$genus, Family_r = rec$family,
               Order_r = rec$order, Class_r = rec$class, Phylum_r = rec$phylum)
  }
  
  resolved <- rbindlist(lapply(unresolved, resolve_via_accepted_name), fill = TRUE)
  
  if (nrow(resolved) > 0) {
    taxonomy <- merge(taxonomy, resolved, by = "Species", all.x = TRUE)
    for (col in c("Genus", "Family", "Order", "Class", "Phylum")) {
      resolved_col <- paste0(col, "_r")
      taxonomy[is.na(get(col)), (col) := get(resolved_col)]
      taxonomy[, (resolved_col) := NULL]
    }
    taxonomy[is.na(dispatch_group), dispatch_group := classify_dispatch(Class)]
  }
  
  ## whatever's still unresolved after a genuine WoRMS attempt defaults
  ## to invertebrate as a taxonomically-neutral fallback, not a fish/
  ## non-fish guess borrowed from an unrelated database
  still_unresolved <- taxonomy[is.na(dispatch_group), Species]
  taxonomy[Species %in% still_unresolved, dispatch_group := "invertebrate"]
  
  message(length(unresolved) - length(still_unresolved), " resolved via WoRMS synonym lookup, ",
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
invisible(STAGE_PB$tick(tokens = list(stage_name = "Taxonomic classification (WoRMS)")))

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

extract_provenance <- function(best_dt, param_type) {
  if (nrow(best_dt) == 0) return(data.table(Species = character()))
  loc_col  <- intersect(c("Locality", "Country", "Loc"), names(best_dt))[1]
  year_col <- intersect(c("Year", "YearStart"), names(best_dt))[1]
  ref_col  <- intersect(c("Author", "Authors", "Ref", "Reference", "RefID"), names(best_dt))[1]
  best_dt[, .(
    Species,
    parameter_type = param_type,
    Locality  = if (!is.na(loc_col)) get(loc_col) else NA_character_,
    Year      = if (!is.na(year_col)) get(year_col) else NA_real_,
    Reference = if (!is.na(ref_col)) as.character(get(ref_col)) else NA_character_
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
  
  growth_c <- select_best_rows(fetch_both(popgrowth, candidates))
  gt <- if (nrow(growth_c) > 0) growth_c[, .(
    Species,
    Loo  = if ("Loo" %in% names(growth_c)) Loo else NA_real_,
    K    = if ("K" %in% names(growth_c)) K else NA_real_,
    Winf = if ("Winfinity" %in% names(growth_c)) Winfinity else NA_real_,
    tmax = if ("tmax" %in% names(growth_c)) tmax else NA_real_
  )] else data.table(Species = character())
  
  ecol_c <- fetch_both(ecology, candidates)
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
  
  swim_c <- fetch_both(swimming, candidates, fishbase_only = TRUE)
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
  
  if (fishbase_only) return(fb)
  
  slb <- retry_fetch(function() as.data.table(fun(sp_list, server = "sealifebase", ...)))
  if (nrow(slb) == 0 && length(sp_list) > 1) {
    message("Batch fetch (sealifebase) returned nothing - falling back to per-species calls...")
    slb <- fetch_per_species(fun, sp_list, "sealifebase")
  }
  
  rbindlist(list(fb, slb), fill = TRUE)
}

## --- 2a. General species table: max weight, max length, depth range --
sp_table <- fetch_both(rfishbase::species, sp_list)
message("\nspecies() columns available:")
print(names(sp_table))

weight_col <- intersect(c("Weight", "WeightMax"), names(sp_table))[1]
length_col <- intersect(c("Length", "LengthMax"), names(sp_table))[1]
long_col   <- intersect(c("LongevityWild", "LongevityCaptive", "MaxAge"), names(sp_table))[1]

sp_traits <- if (nrow(sp_table) > 0) unique(sp_table[, .(
  Species,
  MaxWeight = if (!is.na(weight_col)) get(weight_col) else NA_real_,
  MaxLength = if (!is.na(length_col)) get(length_col) else NA_real_,
  Depth = fifelse(!is.na(DepthRangeDeep) & !is.na(DepthRangeShallow),
                  (DepthRangeDeep + DepthRangeShallow) / 2,
                  fcoalesce(DepthRangeDeep, DepthRangeShallow)),
  Longevity = if (!is.na(long_col)) get(long_col) else NA_real_
  ## NOTE: Family deliberately NOT extracted here - it's already merged
  ## in from WoRMS taxonomy (Step 1, with proper synonym resolution),
  ## and duplicating it here would collide into Family.x/Family.y on
  ## the merge below instead of a clean single column
)], by = "Species") else data.table(Species = character())

message("Coverage - MaxWeight: ", sp_traits[!is.na(MaxWeight), .N], "/", length(sp_list),
        " | Depth: ", sp_traits[!is.na(Depth), .N], "/", length(sp_list),
        " | Longevity: ", sp_traits[!is.na(Longevity), .N], "/", length(sp_list),
        " (field: ", ifelse(is.na(long_col), "NONE FOUND", long_col), ")")
invisible(STAGE_PB$tick(tokens = list(stage_name = "Fetch 2a: species() traits")))

## --- 2b. Growth params (Loo, K, Winfinity) - Mediterranean/recent-prioritized
growth_raw <- fetch_both(popgrowth, sp_list)
growth_best <- select_best_rows(growth_raw)
tmax_col <- if (nrow(growth_best) > 0) intersect(c("tmax", "TMax"), names(growth_best))[1] else NA
growth_traits <- if (nrow(growth_best) > 0) growth_best[, .(
  Species,
  Loo  = if ("Loo" %in% names(growth_best)) Loo else NA_real_,
  K    = if ("K" %in% names(growth_best)) K else NA_real_,
  Winf = if ("Winfinity" %in% names(growth_best)) Winfinity else NA_real_,
  tmax = if (!is.na(tmax_col)) get(tmax_col) else NA_real_
)] else data.table(Species = character())

## Capture WHERE this selected record came from (Locality/Year), not
## just the numeric values - needed for the reference/provenance table
growth_provenance <- extract_provenance(growth_best, "growth (Loo/K/Winf/tmax)")
invisible(STAGE_PB$tick(tokens = list(stage_name = "Fetch 2b: growth params")))

## --- 2c. Length-weight a/b params - same prioritization ---------------
lw_raw <- fetch_both(poplw, sp_list)
lw_best <- select_best_rows(lw_raw)
lw_traits <- if (nrow(lw_best) > 0) lw_best[, .(
  Species,
  a_lw = if ("a" %in% names(lw_best)) a else NA_real_,
  b_lw = if ("b" %in% names(lw_best)) b else NA_real_
)] else data.table(Species = character())
lw_provenance <- extract_provenance(lw_best, "length-weight (a/b)")
invisible(STAGE_PB$tick(tokens = list(stage_name = "Fetch 2c: length-weight a/b")))

## --- 2d. Maturity: Lm (length at maturity), needed for Froese-Binohlan
maturity_raw <- fetch_both(maturity, sp_list)
maturity_best <- select_best_rows(maturity_raw)
lm_col <- if (nrow(maturity_best) > 0) intersect(c("Lm", "LengthMatMin"), names(maturity_best))[1] else NA
tmat_col <- if (nrow(maturity_best) > 0) intersect(c("tm", "AgeMatMin"), names(maturity_best))[1] else NA
maturity_traits <- if (nrow(maturity_best) > 0) maturity_best[, .(
  Species,
  Lm   = if (!is.na(lm_col)) get(lm_col) else NA_real_,
  Tmat = if (!is.na(tmat_col)) get(tmat_col) else NA_real_
)] else data.table(Species = character())
maturity_provenance <- extract_provenance(maturity_best, "maturity (Lm/Tmat)")

message("Coverage - Lm (length at maturity): ", maturity_traits[!is.na(Lm), .N], "/", length(sp_list),
        " (field: ", ifelse(is.na(lm_col), "NONE FOUND", lm_col), ")")
invisible(STAGE_PB$tick(tokens = list(stage_name = "Fetch 2d: maturity")))

## --- 2e. Ecology: trophic level ---------------------------------------
ecol <- fetch_both(ecology, sp_list)
tl_col <- if (nrow(ecol) > 0) intersect(c("DietTroph", "FoodTroph"), names(ecol))[1] else NA
ecol_traits <- if (nrow(ecol) > 0) unique(ecol[, .(
  Species, TrophicLevel = if (!is.na(tl_col)) get(tl_col) else NA_real_
)], by = "Species") else data.table(Species = character())
invisible(STAGE_PB$tick(tokens = list(stage_name = "Fetch 2e: ecology/trophic level")))

## --- 2f. Swimming: aspect ratio (fish-specific caudal-fin morphology -
## SeaLifeBase never has this, so skip that call entirely rather than
## waste time on a query that can never succeed) -----------------------
swim <- fetch_both(swimming, sp_list, fishbase_only = TRUE)
ar_col <- if (nrow(swim) > 0) intersect(c("AspectRatio", "Aspect"), names(swim))[1] else NA
swim_traits <- if (nrow(swim) > 0) unique(swim[, .(
  Species, AspectRatio = if (!is.na(ar_col)) get(ar_col) else NA_real_
)], by = "Species") else data.table(Species = character())
invisible(STAGE_PB$tick(tokens = list(stage_name = "Fetch 2f: swimming/aspect ratio")))

## --- assemble ----------------------------------------------------------
species_df <- Reduce(function(x, y) merge(x, y, by = "Species", all.x = TRUE),
                     list(species_df, sp_traits, growth_traits, lw_traits,
                          maturity_traits, ecol_traits, swim_traits))

DEFAULT_TEMP <- 16
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
PB_QB_SELECTION_MODE <- "priority"  # "priority" or "mean"

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

## Froese & Binohlan (2003) fallback for Loo/K when missing entirely -
## now fully automatic: Lmax from species(), Lm/Tmat from maturity()
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
  survival <- lx / shift(lx, fill = 1)
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
fwrite(brey_candidates, file.path(out_dir, "invertebrates_for_brey_manual_check.csv"))

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

fwrite(results, file.path(out_dir, "species_pb_qb_by_taxon_group.csv"))
fwrite(fg_weighted, file.path(out_dir, "fg_pb_qb_weighted.csv"))
fwrite(phyto_flagged, file.path(out_dir, "phytoplankton_needs_separate_method.csv"))

## =================================================================
## Supplement with EcoBase literature values (03_ecobase_query.R output)
## =================================================================
## 03_ecobase_query.R produces ecobase_literature_pb_qb_simple.csv - PB/QB
## from PUBLISHED Ecopath models, one row per FG_name per source model.
## Matched here on FG_name (EcoBase has no FG_num of its own - group
## naming won't line up automatically across different models' own
## definitions, so this is a text match and should be spot-checked,
## not trusted blindly). Where a model_id column exists, values are
## averaged across all matching EcoBase models per FG_name first, so
## one FG doesn't get weighted toward whichever model happened to have
## the most rows.
ECOBASE_CSV_PATH <- file.path(out_dir, "ecobase_literature_pb_qb_simple.csv")

if (file.exists(ECOBASE_CSV_PATH)) {
  ecobase_raw <- fread(ECOBASE_CSV_PATH)
  message("\nLoaded EcoBase literature PB/QB: ", nrow(ecobase_raw), " rows across ",
          uniqueN(ecobase_raw$FG_name), " distinct FG_name values from ",
          uniqueN(ecobase_raw$EwE_model), " published model(s).")
  
  ecobase_by_fgname <- ecobase_raw[, .(
    PB_ecobase = mean(PB, na.rm = TRUE),
    QB_ecobase = mean(QB, na.rm = TRUE),
    n_ecobase_models = uniqueN(EwE_model[!is.na(PB) | !is.na(QB)])
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
  
  fwrite(fg_weighted_ecobase, file.path(out_dir, "fg_pb_qb_weighted_with_ecobase.csv"))
  message("Saved fg_pb_qb_weighted_with_ecobase.csv.")
  
  ## Dedicated Ecobase sheet in the shared workbook - the raw per-FG
  ## literature values on their own, for audit/comparison, separate
  ## from PB_QB's gap-filled result (which only shows where EcoBase
  ## was actually USED to fill a gap, not every FG it has a value for).
  ecobase_sheet <- merge(fg_name_lookup, ecobase_by_fgname, by = "FG_name", all.x = TRUE)
  setorder(ecobase_sheet, FG)
  setnames(ecobase_sheet, "FG", "FG_num")
  upsert_workbook_sheets(list(Ecobase = ecobase_sheet), ECOPATH_WORKBOOK_PATH)
  
  ## downstream Ecopath export (below) uses the gap-filled values so FGs
  ## with no empirical estimate aren't just left blank when a literature
  ## value was available
  fg_weighted <- copy(fg_weighted_ecobase)
  fg_weighted[, `:=`(PB_FG = PB_FG_filled, QB_FG = QB_FG_filled)]
} else {
  message("\nNo EcoBase literature file found at ", ECOBASE_CSV_PATH,
          " - run 03_ecobase_query.R first if you want literature PB/QB values",
          " merged in as a comparison/gap-fill. Continuing with empirical",
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
  
  fwrite(fg_weighted, file.path(out_dir, "fg_pb_qb_weighted_with_F.csv"))
  message("Saved fg_pb_qb_weighted_with_F.csv.")
} else {
  message("\nFG_YIELD_SOURCE = 'none' (or catch data wasn't found earlier) - fg_weighted's",
          " PB stays NATURAL MORTALITY (M) ONLY for every FG (M+F not applied anywhere).",
          " Set FG_YIELD_SOURCE <- 'fg_catch_csv' near species_df's Yield loading, once",
          " 02_fao_catches.R has been run - until then, treat every PB value below as a",
          " lower bound for any FG that's actually fished.")
}

## =================================================================
## Add PB_QB to output/ecopath_ecosim_inputs.xlsx
##
## Same shared workbook 01_survey_density_westmed.R (Biomass sheets) and
## 02_fao_catches.R (Catches sheets) write to, via the same order-
## independent upsert - this can run before, after, or between those
## two scripts. fg_weighted here reflects whichever of the EcoBase-fill
## and FG-level-F steps above actually ran (or neither), so re-running
## this after changing either toggle updates the PB_QB sheet in place.
## =================================================================
add_pbqb_to_ecopath_workbook(fg_weighted = fg_weighted, out_path = ECOPATH_WORKBOOK_PATH)

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

fwrite(reference_table, file.path(out_dir, "species_parameter_references.csv"))
message("\nSaved: species_parameter_references.csv (", nrow(reference_table),
        " rows - which study/locality/year backs each species' growth,",
        " length-weight, and maturity parameters)")

## =================================================================
## Load the full FG reference (FGnum -> FGname) once, used both by the
## FG-level plot below (for "FGnum_FGname" axis labels) and the
## Ecopath CSV export later.
## =================================================================

FG_REFERENCE_PATH <- file.path(pcloud_dir, "data/FG_WMed.xlsx")

fg_ref_unique <- NULL
if (file.exists(FG_REFERENCE_PATH)) {
  ## Target "fg_wmed_95" BY NAME specifically - confirmed via diagnostic
  ## to be the Western Med FG scheme this project actually uses. NOT
  ## safe to auto-detect by structure alone: this file also has an
  ## "fg_ebro delta_21" sheet (a DIFFERENT model's FG numbering) that
  ## would also structurally match a generic GF+FG_name check, and
  ## picking it by accident would silently mislabel every FG.
  sheet_names <- tryCatch(readxl::excel_sheets(FG_REFERENCE_PATH), error = function(e) character())
  target_sheet <- if ("fg_wmed_95" %in% sheet_names) "fg_wmed_95" else NA
  
  if (!is.na(target_sheet)) {
    candidate <- tryCatch(as.data.table(readxl::read_excel(FG_REFERENCE_PATH, sheet = target_sheet)),
                          error = function(e) NULL)
    if (!is.null(candidate)) {
      ## this sheet has duplicate "FG_name" headers in the source file,
      ## which readxl disambiguates to FG_name...2/FG_name...7 - using
      ## GF + the first one (...2) as the best guess pending confirmation
      ## of which duplicate is the canonical one
      num_col <- intersect(c("GF", "FG_num"), names(candidate))[1]
      name_col <- grep("^FG_name", names(candidate), value = TRUE)[1]
      if (!is.na(num_col) && !is.na(name_col)) {
        fg_ref_unique <- unique(candidate[, .(FG = as.character(as.integer(get(num_col))),
                                              FG_name = get(name_col))])
        message("\nFG reference loaded from sheet '", target_sheet, "' of ", FG_REFERENCE_PATH,
                " (", nrow(fg_ref_unique), " FGs), using columns '", num_col, "' and '", name_col, "'.")
        message("NOTE: this sheet has TWO differently-named FG_name-like columns in the source",
                " file (readxl renamed them FG_name...2/FG_name...7) - using '", name_col,
                "' as a best guess. If FG names in the plot/CSV look wrong, run",
                " diagnose_fg_wmed_95_columns.R to check whether the other one should be used instead.")
      }
    }
  }
  if (is.null(fg_ref_unique)) {
    message("\n", strrep("!", 70))
    message("FG_REFERENCE_PATH exists but NO sheet in it has both 'GF' and",
            " 'FG_name' columns - checked sheets: ", paste(sheet_names, collapse = ", "), ".")
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
## the input stage) over the external FG_WMed.xlsx join, which is only
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
  
  dt[, FG_label := fifelse(!is.na(FG_name), paste0(FG_num, "_", FG_name), FG_num)]
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

fwrite(ecopath_final, file.path(out_dir, "ecopath_ready_PB_QB.csv"), quote = "auto")
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