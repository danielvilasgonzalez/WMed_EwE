## =================================================================
## FISHERIES MASTER SCRIPT - ONE SELF-CONTAINED SOURCE FILE.
##
## Runs the whole fisheries flow end to end. Andrea's flow (below) is
## the CONCEPTUAL order this script follows; the literal section
## headers further down still read as the original 10-step list
## (get GFCM catches -> distribute over sector/fleet -> discards ->
## unreported -> bycatch -> Ecopath-by-fleet -> F -> ts per FG ->
## effort -> append to workbook) since steps 1-3 below map onto them
## directly - only steps 4-5 changed what actually happens inside
## "distribute catches over sector and fleet" and "obtain fishing
## effort":
##   1) get data (GFCM, STECF FDI, SAU, FishMIP - all loaded/downloaded
##      directly in this file, see STANDING CONSTRAINTS below)
##   2) match species -> FG (the matching cascade, shared by GFCM/SAU/
##      STECF FDI - one cascade, not three)
##   3) GFCM catch data - the trusted total, but with NO gear/fleet
##      dimension of its own (confirmed by the fleet-evaluation
##      diagnostic below) - Division-resolution only, never GSA
##   4) STECF FDI - Spain/France/Italy's REAL landings, discards, and
##      effort (total_fishing_days/total_days_at_sea x total_kW_days_at_sea/
##      total_GT_days_at_sea), by metier (finer than plain gear) x GSA, 2013-2024
##      (FDI's own coverage window) - the PRIMARY fleet source for
##      these 3 countries over that window
##   5) SAU fills the gaps STECF FDI can't reach: the years before
##      2014 (back to 1994) for Spain/France/Italy, Morocco/Algeria/
##      Tunisia entirely (not EU Member States, don't report into
##      FDI), and the Artisanal/Recreational split via SAU's own
##      sector field (Artisanal/Industrial/Recreational proportions)
##      wherever STECF alone can't give it
##
## STANDING CONSTRAINTS (both explicit, both respected here):
##   - No EMODnet effort anywhere. FishMIP nom_active is the effort
##     source for everything STECF FDI's own effort doesn't cover
##     (pre-2014, and Morocco/Algeria/Tunisia always).
##   - No dependency on any other fisheries script having run first.
##     02_fao_catches.R / 02a_fisheries_multisource.R / 02b_fisheries_
##     priority_merge.R do NOT need to run before this one - GFCM raw
##     capture data, the species->FG matching cascade, and SAU's raw
##     per-EEZ extract are all loaded/computed/downloaded directly in
##     this file (copied in, not sourced), exactly like the standalone
##     02_fisheries_catches_discards_effort.R this script supersedes.
##   Two files DO still get read as-is, because they are this
##   pipeline's fixed-name "contract files" from Step 1, not another
##   fisheries script's output: species_density_regional_combined.csv
##   and strata_area_by_area.csv (needed for the F-for-species step
##   below only). If Step 1 hasn't been run yet, that one step is
##   skipped with a loud message - everything else in this script
##   still runs.
##
## WHAT THIS SCRIPT DOES NOT SOLVE - stated plainly, not papered over:
##   - GSA vs Division: GFCM's own raw capture data in this pipeline
##     only resolves to FAO DIVISION (37.1.1 / 37.1.2 / 37.1.3), never
##     to GSA. "Catches by species, year and GSA" is therefore reported
##     at Division resolution, with the Division kept in its own column
##     (never relabeled "GSA") - see STEP 1 below for exactly why.
##   - Fleet/sector split, unreported %, and bycatch % are all only
##     available for the 6 named TARGET_COUNTRIES (Morocco, Algeria,
##     Tunisia, France, Spain, Italy) - FLEET_REGISTER/GEAR_TO_FLEETTYPE
##     and SAU's raw extract only cover these.
##   - Discard ratio is FG-level, Mediterranean-wide (FishMIP cannot
##     cross country x FG), applied uniformly to every country.
##   - Bycatch has NO identified data source anywhere in this pipeline
##     (GFCM, SAU, FishMIP) - see step below; a flagged manual-entry
##     placeholder only, same convention as recreational catch.
## =================================================================

pkgs <- c("data.table", "openxlsx", "stringr", "readxl", "rfishbase", "httr", "jsonlite", "dplyr",
          "ggplot2", "sf")   # ggplot2/sf are loaded by the sourced lib_survey_fg_density_functions.R itself
new_pkgs <- pkgs[!pkgs %in% installed.packages()[, "Package"]]  # find which packages are not yet installed
if (length(new_pkgs) > 0) install.packages(new_pkgs)  # install any missing packages
invisible(lapply(pkgs, library, character.only = TRUE))  # load all required packages

## =================================================================
## CONFIGURATION - same resolve_config_dir() convention as the rest
## of this pipeline; reused verbatim if already set in this session.
## =================================================================
resolve_config_dir <- function(var_name, hardcoded_value, prompt_title, prompt_message) {
  if (exists(var_name, envir = .GlobalEnv, inherits = FALSE)) {  # reuse an already-set variable from this session, if valid
    val <- get(var_name, envir = .GlobalEnv)
    if (!is.null(val) && is.character(val) && length(val) == 1 && !is.na(val) && val != "" && dir.exists(val)) {
      message("[02_fisheries_master.R] ", var_name, " already set - using '", val, "' rather than re-prompting.")
      return(val)  # existing value is valid, use it as-is
    }
    message("[02_fisheries_master.R] ", var_name, " was already set but to something invalid - re-resolving.")
  }
  if (tolower(Sys.info()[["user"]]) == "daniel" && .Platform$OS.type == "unix") return(hardcoded_value)  # Daniel's machine: skip prompting, use hardcoded path
  if (!requireNamespace("rstudioapi", quietly = TRUE) || !rstudioapi::isAvailable()) {
    stop("This script requires RStudio, or ", var_name, " set manually before running this script - ", prompt_message)
  }
  rstudioapi::showQuestion(title = prompt_title, message = prompt_message)  # show an RStudio prompt describing what to pick
  val <- rstudioapi::selectDirectory()  # let the user pick a folder via RStudio's directory chooser
  if (is.null(val) || val == "" || !dir.exists(val)) stop("No valid directory selected for ", var_name, ".")
  val
}

out_dir <- resolve_config_dir("out_dir", "/Users/daniel/Work/iMARES/WMed EwE Model/output/",
                              "Select Output Directory", "Please select the directory where output files and intermediate results are saved.")  # resolve the output directory
pcloud_dir <- resolve_config_dir("pcloud_dir", "/Users/daniel/pCloud Drive/EwE Western Med 2026/",
                                 "Select pCloud EwE West Med Directory", "Please select the pCloud Drive/EwE Western Med 2026 folder.")  # resolve the pCloud data directory
git_dir <- resolve_config_dir("git_dir", "/Users/daniel/Documents/GitHub/WMed_EwE/",
                              "Select Github WMed_EwE Directory", "Please select the directory where you cloned the WMed_EwE repository.")  # resolve the local git repo directory

ECOPATH_WORKBOOK_PATH <- file.path(out_dir, "ecopath_ecosim_inputs.xlsx")  # path to the EwE Ecopath/Ecosim workbook to write into

DATASET_VERSION <- "GFCM_2025"   # "FAO_2020" | "GFCM_2025"
START_YEAR <- 1994
END_YEAR   <- 2023                # GFCM_2025's full available series. FishMIP/SAU below have no
# data past ~2017-2019 regardless - their own coverage messages
# say so explicitly rather than truncating GFCM's longer series.
YEAR_ECOPATH <- 1994:1996         # single-snapshot averaging window for the Ecopath-by-fleet and F steps

TARGET_COUNTRIES <- c("Spain", "France", "Italy", "Tunisia", "Algeria", "Morocco")

## Unreported-% adjustment (SAU reported-vs-total ratio, per country) -
## OFF by default: the ratios it produces came out implausibly high
## against SAU's raw extract, so Catch_t_incl_unreported is not applied
## to the fleet-level output for now. unreported_by_country/the
## diagnostic ratio is still computed and written out (CSV + workbook
## sheet) so the numbers stay visible for review - only the downstream
## Catch_t_incl_unreported column is switched off. Flip back to TRUE
## once the ratio itself has been checked/fixed.
APPLY_UNREPORTED_ADJUSTMENT <- FALSE

## GFCM's own download is a VERSIONED folder (e.g. "FI_Regional_2025.1.0",
## "FI_Regional_2026.1.0" as of Andrea's latest pCloud listing) - the
## version number bumps on its own schedule, not this script's, so
## hardcoding one exact folder name breaks every time GFCM re-exports.
## find_versioned_subdir() below looks for whichever subfolder actually
## has the marker file/dir in it, rather than assuming a fixed name.
## Two possible locations are supported, tried in this order:
##   1) pcloud_dir/data/fisheries/GFCM/<any version folder> - Andrea's
##      current layout (moved here alongside FDI/FishMIP/SAU, Sept 2026).
##   2) git_dir/data/raw/<any version folder> - the older convention
##      (Daniel's clone), kept as a fallback so this still runs unchanged
##      for anyone still using that layout.
find_versioned_subdir <- function(parent_dir, marker) {
  ## marker: a file or directory name that must exist directly inside
  ## the returned folder. Checks parent_dir itself first (unversioned/
  ## flat layout), then each of its immediate subfolders (versioned
  ## layout) - returns the first one that actually has the marker, or
  ## NA if parent_dir doesn't exist or nothing under it has the marker.
  if (!dir.exists(parent_dir)) return(NA_character_)
  candidates <- c(parent_dir, list.dirs(parent_dir, recursive = FALSE, full.names = TRUE))  # parent dir itself plus its immediate subfolders
  has_marker <- file.exists(file.path(candidates, marker)) | dir.exists(file.path(candidates, marker))  # flag which candidates contain the marker
  hits <- candidates[has_marker]
  if (length(hits) == 0) return(NA_character_)
  if (length(hits) > 1) message("[Paths] Multiple candidate folders under '", parent_dir, "' have '", marker,
                                "' - using the first one found: '", hits[1], "'.")
  hits[1]  # return the first matching folder
}

GFCM_PCLOUD_PARENT_DIR <- file.path(pcloud_dir, "data/fisheries/GFCM")
GFCM_GIT_PARENT_DIR    <- file.path(git_dir, "data/raw")
gfcm_data_dir_resolved <- find_versioned_subdir(GFCM_PCLOUD_PARENT_DIR, "GFCM_Capture_Quantity.csv")  # try pCloud location first
if (is.na(gfcm_data_dir_resolved)) {
  gfcm_data_dir_resolved <- find_versioned_subdir(GFCM_GIT_PARENT_DIR, "GFCM_Capture_Quantity.csv")  # fall back to the git repo location
  if (!is.na(gfcm_data_dir_resolved)) {
    message("\n[Paths] GFCM data found under git_dir ('", gfcm_data_dir_resolved, "'), not under pcloud_dir/",
            "data/fisheries/GFCM - using the git_dir copy. Move it into pcloud_dir/data/fisheries/GFCM/ to",
            " match FDI/FishMIP/SAU's convention if you'd rather it live there.")
  }
}
if (is.na(gfcm_data_dir_resolved)) {
  message("\n[Paths] Could not find a folder containing 'GFCM_Capture_Quantity.csv' under either '",
          GFCM_PCLOUD_PARENT_DIR, "' or '", GFCM_GIT_PARENT_DIR, "' - falling back to the hardcoded",
          " 2025.1.0 path; GFCM catches will fail to load if that's not actually there.")
  gfcm_data_dir_resolved <- file.path(git_dir, "data/raw/FI_Regional_2025.1.0")  # last-resort hardcoded fallback path
}
message("\n[Paths] GFCM data_dir resolved to: '", gfcm_data_dir_resolved, "'.")

FAO_2020_DATA_SUBDIR  <- "data/Capture_2020"              # UNCONFIRMED - only used if DATASET_VERSION <- "FAO_2020"
FG_REFERENCE_SUBPATH  <- "data/FG_WMed.xlsx"              # relative to pcloud_dir

FISHMIP_EFFORT_PARQUET <- file.path(pcloud_dir, "data/fisheries/FishMIP/effort_histsoc_1841_2017_western-mediterranean-sea.parquet")  # FishMIP effort parquet path
FISHMIP_CATCH_PARQUET  <- file.path(pcloud_dir, "data/fisheries/FishMIP/calibration_catch_histsoc_1850_2017_western-mediterranean-sea.parquet")  # FishMIP catch parquet path
FISHMIP_FG_CROSSWALK_PATH <- NULL   # columns: fishmip_f_group, FG_num - leave NULL to keep FishMIP's own naming

## Rousseau et al. 2024 (Scientific Data 11:260) global fishing-capacity/
## effort database - manual download, same convention as FDI/FishMIP/SAU
## (place the repo's Data/Final_DataStudyFAO_AllGears_wCode.csv here).
## Per Andrea (2026-09): used below (a) to hindcast Spain/France/Italy's
## pre-STECF_FDI_START_YEAR effort, IN PLACE OF the old FDI-ratio x
## SAU-hindcasted-catch method, and (b) as a second, independent effort
## figure alongside FishMIP nom_active for Morocco/Algeria/Tunisia (SAU
## itself has no effort variable to draw on for the "other" non-EU
## countries - see the comment at ROUSSEAU_EFFORT_NONEU block below).
## CAVEAT logged at runtime: checked against STECF FDI's own real
## Effort_days for Spain/France/Italy over their one overlap window
## (2013-2017), Rousseau's NomEffort correlates at r=-0.21 (Spain),
## r=-0.01 (Italy), r=0.62 (France) - i.e. it does NOT reproduce FDI's
## real effort shape for 2 of the 3 countries. It is used anyway per
## Andrea's explicit direction, ANCHORED (not trusted on its own trend)
## to FDI's real level via a per-country calibration factor computed
## from that same overlap window - see rousseau_calibration below.
ROUSSEAU_EFFORT_PATH <- file.path(pcloud_dir, "data/fisheries/Rousseau/Final_DataStudyFAO_AllGears_wCode.csv")  # Rousseau et al. 2024 effort CSV path

## SAU is now a manual-download FOLDER, same convention as FDI/FishMIP
## (pcloud_dir/data/fisheries/SAU/ - one or more CSVs, whatever Sea
## Around Us's current site hands you per-EEZ; all *.csv files in this
## folder are read and stacked together). The old api.seaaroundus.org
## v1 endpoint this script could auto-download from is unconfirmed as
## still live (SAU's own current tools-guide page documents per-EEZ
## manual "Download data" buttons or contacting them for bulk multi-
## country data - no public bulk API is mentioned there anymore) and
## this environment's proxy blocks that host outright, so auto-download
## is now OFF by default; flip AUTO_DOWNLOAD_SAU back to TRUE only after
## confirming that endpoint still responds.
SAU_DIR <- file.path(pcloud_dir, "data/fisheries/SAU")  # folder of manually-downloaded SAU CSVs
SAU_RAW_CSV <- file.path(pcloud_dir, "data/fisheries/sau_raw_combined_west_med.csv")  # legacy single-file fallback, kept only for AUTO_DOWNLOAD_SAU's own cache
AUTO_DOWNLOAD_SAU <- FALSE          # SAU's v1 API is unconfirmed as still live - manually download into SAU_DIR instead (see comment above)

## Morocco/Algeria real local data (2026-09, per Andrea's uploaded workbook
## "Mediterranean_Morocco_Algeria_Fisheries_Data_1.xlsx") - place the 8
## non-README sheets here as individual CSVs, one file per sheet, named
## exactly after the sheet (e.g. "MAR_Catch_by_Species.csv"). Fills the
## gap these two countries otherwise have no local source for (unlike
## Spain/France/Italy's STECF FDI): Belhabib et al.'s Sea-Around-Us-style
## reconstructed catch by taxon group (1994-2010), Algeria's real measured
## trawl-hours effort series, and Rousseau et al. (2024)'s independent
## fleet-capacity/effort database for both countries. See the "Morocco/
## Algeria local data" block near catch_with_discards below and the
## "Morocco/Algeria effort" block further down for exactly how each sheet
## is used. Missing entirely -> every block below degrades gracefully
## (message + fall back to the existing GFCM/FishMIP-only treatment),
## same convention as every other optional source in this script.
MOROCCO_ALGERIA_DIR <- file.path(pcloud_dir, "data/fisheries/Morocco_Algeria")  # folder of manually-exported CSVs, one per source workbook sheet
safe_fread_optional <- function(path, label) {
  if (!file.exists(path)) {
    message("[Morocco/Algeria] '", label, "' not found at '", path, "' - skipping (see MOROCCO_ALGERIA_DIR comment above).")
    return(data.table())
  }
  fread(path, encoding = "UTF-8")  # read the CSV with UTF-8 encoding
}

## Step 1's fixed-name contract files - needed for the F-for-species
## step only. Not another fisheries script's output; if these don't
## exist yet (Step 1 hasn't been run), only that one step is skipped.
SPECIES_DENSITY_PATH <- file.path(out_dir, "species_density_regional_combined.csv")  # Step 1's contract file: species density
STRATA_AREA_PATH     <- file.path(out_dir, "strata_area_by_area.csv")  # Step 1's contract file: strata area

N_TOP_GEARS <- 8   # effort-gears beyond the top N are folded into "Other gear" - same convention
# 02a_fisheries_multisource.R uses for both SAU catch and FishMIP effort.
FISHMIP_SAUP_TO_COUNTRY <- c(`12` = "Algeria", `250` = "France", `380` = "Italy",
                             `504` = "Morocco", `724` = "Spain", `788` = "Tunisia")  # FishMIP's numeric country codes mapped to country names

## Shared function library - add_catches_to_ecopath_workbook() (builds
## Catches_Ecopath/Catches_Ecosim in the EXACT same structure/units as
## Biomass's own Ecopath/Ecosim sheets), upsert_workbook_sheets(), and
## finalize_workbook_sheet_order(). Sourcing a pure function library is
## NOT the same as depending on another fisheries SCRIPT'S OUTPUT
## having already run (see this file's header) - nothing here reads a
## file another fisheries script produced; this is the SAME shared
## library 01_survey_density_westmed.R (Biomass) and the old
## 02_fao_catches.R (Catches) both already source, so Catches_Ecopath/
## Catches_Ecosim below come out byte-for-byte the same shape Biomass's
## Ecopath/Ecosim sheets do, not a reimplementation that could quietly
## drift from it.
source(file.path(git_dir, "scripts/lib_survey_fg_density_functions.R"))  # load shared workbook-writing helper functions

## Study area for the Catches<->Biomass density conversion (t/km^2/year,
## matching Biomass's own units) - REQUIRED from here on (same stop()
## convention 02_fao_catches.R used for this same figure), since
## Catches_Ecopath/Catches_Ecosim/the fleet-level Ecopath snapshot all
## need it, not just the F step.
if (!file.exists(STRATA_AREA_PATH)) {
  stop("'", STRATA_AREA_PATH, "' not found - needed to convert catch tonnes into a t/km^2 density",  # abort if the required contract file is missing
       " matching Biomass's units in the Ecopath/Ecosim sheets. Run 01_survey_density_westmed.R",
       " first (it writes this file), or check out_dir matches where it saved it.")
}
Total_Area_km2 <- fread(STRATA_AREA_PATH)[, sum(area_km2, na.rm = TRUE)]  # total study area = sum of all strata areas
message("\n[Area] Study area for the Catches<->Biomass density conversion: ", round(Total_Area_km2, 1),
        " km^2 (sum of strata_area_by_area.csv's area_km2 - the same figure 01_survey_density_westmed.R/",
        "04_pbqb_calc.R use).")

## =================================================================
## # get GFCM catches by species and year and GSA
## =================================================================
DATASETS <- list(
  GFCM_2025 = list(
    format = "gfcm_regional", data_dir = gfcm_data_dir_resolved,
    capture_file = "GFCM_Capture_Quantity.csv", species_file = "CL_FI_SPECIES_GROUPS.csv",
    countries_file = "CL_FI_COUNTRY_GROUPS.csv", area_file = "CL_FI_WATERAREA_DIVISION.csv",
    area_code_col = "DIVISION.CODE", area_join_col = "Code",
    area_filter_values = c("37.1.1", "37.1.2", "37.1.3"), fg_file = file.path(pcloud_dir, FG_REFERENCE_SUBPATH)
  ),
  FAO_2020 = list(
    format = "legacy_excel", catch_file = file.path(pcloud_dir, FAO_2020_DATA_SUBDIR, "FAO-GFCM-CapturepProduction-1970_2023.xlsx"),
    fg_file = file.path(pcloud_dir, FG_REFERENCE_SUBPATH)
  )
)
cfg <- DATASETS[[DATASET_VERSION]]  # select the active dataset config block

safe_fread <- function(path, label) {
  if (!file.exists(path)) stop("'", label, "' not found at '", path, "'.")
  fread(path, encoding = "UTF-8")  # read the CSV with UTF-8 encoding
}

load_gfcm_regional <- function(cfg) {
  p <- function(f) file.path(cfg$data_dir, f)  # helper to build a full path inside the GFCM data dir
  capture   <- safe_fread(p(cfg$capture_file), "capture_file")  # raw capture-quantity records
  species   <- safe_fread(p(cfg$species_file), "species_file")  # species code lookup
  if (!all(c("3A_Code", "Name_En") %in% names(species))) {
    stop("species_file is missing expected column(s) '3A_Code'/'Name_En' - got: ", paste(names(species), collapse = ", "))
  }
  species <- species[, .(SPECIES.ALPHA_3_CODE = `3A_Code`, Species = Name_En)]  # keep/rename only the needed species columns
  countries <- safe_fread(p(cfg$countries_file), "countries_file")[, .(COUNTRY.UN_CODE = UN_Code, Country = Name_En)]  # country code lookup
  area_ref  <- safe_fread(p(cfg$area_file), "area_file")  # area/division code lookup
  setnames(area_ref, cfg$area_join_col, "AreaCode")  # standardize the join column name
  area_ref  <- unique(area_ref[, .(AreaCode, Division = Name_En)])  # keep only area code and division name
  
  capture <- merge(capture, species, by = "SPECIES.ALPHA_3_CODE", all.x = TRUE)  # attach species names to capture rows
  capture <- merge(capture, countries, by = "COUNTRY.UN_CODE", all.x = TRUE)  # attach country names to capture rows
  setnames(capture, cfg$area_code_col, "AreaCode")  # standardize the area code column name
  capture <- merge(capture, area_ref, by = "AreaCode", all.x = TRUE)  # attach division names to capture rows
  capture[, PERIOD := as.numeric(PERIOD)]  # ensure the year column is numeric
  capture
}
load_legacy_excel <- function(cfg) as.data.table(readxl::read_excel(cfg$catch_file))  # read the legacy FAO Excel catch file

## NOTE ON "GSA" - GFCM's own Division field (37.1.1/37.1.2/37.1.3) is
## NOT the same resolution as a GSA number. This pipeline has no GFCM-
## native GSA-level catch anywhere; Division is the finest area
## resolution available and is kept under its own name below, never
## relabeled "GSA", so nothing downstream mistakes one for the other.
build_westmed_timeseries <- function(cfg) {
  if (cfg$format == "gfcm_regional") {
    capture <- load_gfcm_regional(cfg)  # load and join the raw GFCM capture data
    westmed <- capture[AreaCode %in% cfg$area_filter_values & MEASURE == "Q_tlw" & PERIOD >= START_YEAR]  # keep only West Med divisions, live-weight quantity, from START_YEAR
    species_ts <- westmed[, .(Catch = sum(VALUE, na.rm = TRUE)), by = .(Year = PERIOD, Species, Division)]  # sum catch by year x species x division
    country_species_ts <- westmed[, .(Catch = sum(VALUE, na.rm = TRUE)), by = .(Year = PERIOD, Country, Species, Division)]  # sum catch by year x country x species x division
    list(westmed = westmed, species_ts = species_ts, country_species_ts = country_species_ts,
         value_col = "VALUE", species_col = "Species")
  } else if (cfg$format == "legacy_excel") {
    catch_data <- load_legacy_excel(cfg)  # load the legacy Excel catch table
    setnames(catch_data, c("Species (scientific name)", "Area (FAO division)", "Area (FAO subarea)"),
             c("Species", "Division", "Subarea"), skip_absent = TRUE)  # standardize column names
    westmed <- catch_data[Subarea == "Western Med (37.1)" & Year >= START_YEAR]  # keep only West Med subarea rows from START_YEAR
    species_ts <- westmed[, .(Catch = sum(Quantity, na.rm = TRUE)), by = .(Year, Species, Division)]  # sum catch by year x species x division
    country_species_ts <- data.table()   # legacy_excel format has no Country column available
    list(westmed = westmed, species_ts = species_ts, country_species_ts = country_species_ts,
         value_col = "Quantity", species_col = "Species")
  }
}

ts_data <- build_westmed_timeseries(cfg)  # build the West Med catch timeseries for the active dataset
message("\n[GFCM] Dataset loaded: ", DATASET_VERSION, " - ", nrow(ts_data$species_ts),
        " Species x Year x Division row(s) (Division, NOT GSA - see note above build_westmed_timeseries()).")
if (nrow(ts_data$country_species_ts) == 0) {
  message("[GFCM] No country-level breakdown available for DATASET_VERSION = '", DATASET_VERSION,
          "' - the fleet/sector split step below will have nothing to work with. Use 'GFCM_2025' for a",
          " country-resolved run.")
}

fg <- as.data.table(readxl::read_excel(cfg$fg_file, sheet = 4))  # load the functional-group reference table

unmatched_species <- ts_data$westmed[
  !get(ts_data$species_col) %in% fg$ESPECIE,
  .(Catch = sum(get(ts_data$value_col), na.rm = TRUE)), by = c(ts_data$species_col)
][order(-Catch)]  # species not already in the FG table, summed by catch and sorted descending
setnames(unmatched_species, ts_data$species_col, "Species")  # standardize the species column name
unmatched_species[, `:=`(FG_num = NA, FG_name = NA)]  # add empty FG columns to fill in via the matching cascade
message("[GFCM] ", nrow(unmatched_species), " species need FG matching.")

## --- species -> FG matching cascade (same pipeline as 02_fao_catches.R,
## copied in rather than sourced - see this file's header) -------------
setDT(unmatched_species); setDT(fg)  # ensure both tables are data.tables

resolve_matches_safely <- function(merged_dt, query_col = "Species", fg_col = "FG_num") {
  n_fg <- merged_dt[, .(n_distinct_fg = uniqueN(get(fg_col))), by = query_col]  # count distinct FG matches per query
  safe_queries <- n_fg[n_distinct_fg == 1][[query_col]]  # queries with exactly one FG match
  ambiguous_queries <- n_fg[n_distinct_fg > 1][[query_col]]  # queries with more than one FG match
  list(safe = unique(merged_dt[get(query_col) %in% safe_queries], by = query_col),
       ambiguous = merged_dt[get(query_col) %in% ambiguous_queries])
}
extract_genus <- function(sci_name) str_extract(sci_name, "^[A-Za-z]+")  # pull the genus (first word) from a scientific name

blank_row <- unmatched_species[is.na(Species) | Species == ""]  # rows with no species name at all
if (nrow(blank_row) > 0) message(nrow(blank_row), " row(s) with blank Species name - excluded.")
unmatched_species <- unmatched_species[!is.na(Species) & Species != ""]  # drop blank-species rows

fg_lookup <- unique(fg[, .(ScientificName = ESPECIE, FG_num = GF, FG_name)])  # build the FG reference lookup
fg_lookup[, genus := extract_genus(ScientificName)]  # add a genus column for the genus-fallback match

## Full FG catalog (FG_num x FG_name only) - fg_lookup above for the
## matching cascade, this for add_catches_to_ecopath_workbook()'s own
## fg_lookup argument (every FG, not just ones with a matched species).
full_fg_list <- unique(fg_lookup[, .(FG_num, FG_name)])  # every FG number/name, deduplicated
setorder(full_fg_list, FG_num)  # sort by FG number

fg_name_lookup <- unique(fg[, .(Species = FG_name, FG_num = GF, FG_name)])  # lookup keyed by FG name itself
direct_merged <- merge(unmatched_species[, .(Species, Catch)], fg_name_lookup, by = "Species")  # try matching species name directly to an FG name
direct_resolved <- resolve_matches_safely(direct_merged)
direct_matches <- direct_resolved$safe[, .(Species, FG_num, FG_name)]  # keep only the unambiguous direct matches
direct_matches[, match_method := "direct_fg_name"]  # tag how these matches were resolved
message("STEP - Direct Species==FG_name matches: ", nrow(direct_matches))

sci_names <- unique(fg_lookup$ScientificName)  # all distinct scientific names in the FG table
sci_names <- sci_names[str_detect(sci_names, "^[A-Z][a-z]+ [a-z]+$")]  # keep only well-formed "Genus species" names

duckdb_home <- path.expand("~/.duckdb")
if (!dir.exists(duckdb_home)) dir.create(file.path(duckdb_home, "extensions"), recursive = TRUE, showWarnings = FALSE)  # ensure rfishbase's duckdb cache dir exists

message("\nSTEP - Querying common names for ", length(sci_names), " species...")
fb_common  <- tryCatch(as.data.table(common_names(sci_names, server = "fishbase")), error = function(e) data.table())  # look up English common names on FishBase
slb_common <- tryCatch(as.data.table(common_names(sci_names, server = "sealifebase")), error = function(e) data.table())  # look up English common names on SeaLifeBase
fb_lookup <- unique(rbindlist(list(fb_common, slb_common), fill = TRUE)[
  Language == "English" & !is.na(ComName), .(ScientificName = Species, Species = ComName)])  # combine and keep only English common names
message("Common names found: ", nrow(fb_lookup), " covering ", uniqueN(fb_lookup$ScientificName), " of ", length(sci_names), " species")

clean_name <- function(x) {
  x <- str_remove(x, "\\(.*\\)"); x <- str_remove(x, regex("\\bnei\\b", ignore_case = TRUE))  # strip parentheticals and "nei"
  x <- str_remove(x, "'s\\b"); str_to_lower(str_squish(x))  # strip possessive, lowercase, collapse whitespace
}
remaining <- unmatched_species[!Species %in% direct_matches$Species]  # species still unmatched after the direct pass
remaining[, clean_species := clean_name(Species)]  # normalize species name for fuzzy matching
fb_lookup[, clean_species := clean_name(Species)]  # normalize common name for fuzzy matching

exact_merged <- merge(remaining[, .(Species, Catch, clean_species)], fb_lookup[, .(clean_species, ScientificName)],
                      by = "clean_species", allow.cartesian = TRUE)  # match on normalized common name
exact_merged <- merge(exact_merged, fg_lookup[, .(ScientificName, FG_num, FG_name)], by = "ScientificName")  # attach FG via the matched scientific name
exact_resolved <- resolve_matches_safely(exact_merged)
exact_matches <- exact_resolved$safe[, .(Species, FG_num, FG_name)]  # keep only unambiguous matches
exact_matches[, match_method := "fishbase_common_name"]  # tag how these matches were resolved
message("STEP - Resolved (common name): ", nrow(exact_matches), " | Ambiguous (excluded): ", uniqueN(exact_resolved$ambiguous$Species))

remaining <- remaining[!Species %in% exact_matches$Species]  # species still unmatched after the common-name pass

find_or_download_fao_species <- function() {
  found <- list.files(pcloud_dir, pattern = "CL_FI_SPECIES_GROUPS.csv$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)  # look for a local copy first
  if (length(found) > 0) return(found[1])
  url <- "https://data.apps.fao.org/catalog/dataset/b70c52c1-475f-4951-a8ac-de44016abd9b/resource/2c0f936d-6c36-4715-9c7f-fa5a70c00249/download/cl_fi_species_groups.csv"
  destfile <- file.path(out_dir, "CL_FI_SPECIES_GROUPS.csv")
  max_retries <- 3; last_err <- NULL
  for (attempt in seq_len(max_retries)) {
    ok <- tryCatch({
      download.file(url, destfile = destfile, mode = "wb", method = "libcurl", quiet = (attempt > 1)); TRUE  # try downloading via download.file
    }, error = function(e) { last_err <<- e; FALSE }, warning = function(w) { last_err <<- w; FALSE })
    if (isTRUE(ok) && file.exists(destfile) && file.size(destfile) > 0) return(destfile)  # success, return the downloaded path
    if (attempt < max_retries) Sys.sleep(2 * attempt)  # back off before retrying
  }
  if (!requireNamespace("httr", quietly = TRUE)) install.packages("httr")
  resp <- tryCatch(httr::GET(url, httr::config(http_version = 1.1), httr::write_disk(destfile, overwrite = TRUE), httr::timeout(120)), error = function(e) e)  # fallback download via httr
  if (inherits(resp, "error") || httr::status_code(resp) != 200 || !file.exists(destfile) || file.size(destfile) == 0) {
    stop("Could not download cl_fi_species_groups.csv - download it manually into pcloud_dir as CL_FI_SPECIES_GROUPS.csv.")
  }
  destfile
}
fao_species <- fread(file = find_or_download_fao_species(), encoding = "UTF-8")  # load the FAO species reference table
name_col <- grep("english|name.*en$|^name$", names(fao_species), ignore.case = TRUE, value = TRUE)[1]  # find the English-name column
sci_col  <- grep("scientific", names(fao_species), ignore.case = TRUE, value = TRUE)[1]  # find the scientific-name column
setnames(fao_species, c(name_col, sci_col), c("Name_En", "Scientific_Name"), skip_absent = TRUE)  # standardize their names
fao_species[, genus := extract_genus(Scientific_Name)]  # add a genus column
remaining <- merge(remaining, unique(fao_species[, .(Name_En, genus)], by = "Name_En"), by.x = "Species", by.y = "Name_En", all.x = TRUE)  # attach genus via FAO's English name
genus_merged <- merge(remaining[!is.na(genus), .(Species, Catch, genus)], fg_lookup[!is.na(genus), .(genus, FG_num, FG_name)], by = "genus", allow.cartesian = TRUE)  # match on genus
genus_resolved <- resolve_matches_safely(genus_merged)
genus_matches <- genus_resolved$safe[, .(Species, FG_num, FG_name)]  # keep only unambiguous genus matches
genus_matches[, match_method := "fao_genus"]  # tag how these matches were resolved
message("\nSTEP - Resolved via genus: ", nrow(genus_matches), " | Ambiguous (excluded): ", uniqueN(genus_resolved$ambiguous$Species))

remaining <- remaining[!Species %in% genus_matches$Species]  # species still unmatched after the genus pass
FILLER_WORDS <- c("nei", "spp", "sp", "etc", "and", "or", "the", "of")
tokenize <- function(x) {
  words <- str_split(str_to_lower(str_remove_all(x, "[,().']")), "\\s+")[[1]]  # lowercase, strip punctuation, split into words
  words[!words %in% FILLER_WORDS & words != ""]  # drop filler words and empty tokens
}
fb_with_fg <- merge(fb_lookup, fg_lookup[, .(ScientificName, FG_num, FG_name)], by = "ScientificName")  # common names already linked to an FG
candidate_names <- unique(fb_with_fg$Species)  # distinct candidate common names to match against
candidate_tokens <- setNames(lapply(candidate_names, tokenize), candidate_names)  # tokenize each candidate name once
match_by_containment <- function(query) {
  q_tokens <- tokenize(query)  # tokenize the unmatched species name
  if (length(q_tokens) == 0) return(NULL)
  scores <- vapply(candidate_tokens, function(c_tokens) {
    if (length(c_tokens) == 0) return(0)
    sum(q_tokens %in% c_tokens) / length(q_tokens)  # fraction of query tokens found in the candidate
  }, numeric(1))
  best <- which(scores == 1)  # candidates where every query token is contained
  if (length(best) == 0) return(NULL)
  data.table(Species = query, candidate = names(candidate_tokens)[best])
}
containment_results <- rbindlist(lapply(remaining$Species, match_by_containment))  # run word-containment matching over all remaining species
if (nrow(containment_results) > 0) {
  containment_merged <- merge(containment_results, fb_with_fg[, .(Species, FG_num, FG_name)], by.x = "candidate", by.y = "Species")  # attach FG via the matched candidate name
  containment_resolved <- resolve_matches_safely(containment_merged)
  containment_matches <- containment_resolved$safe[, .(Species, FG_num, FG_name)]  # keep only unambiguous matches
  containment_matches[, match_method := "word_containment"]  # tag how these matches were resolved
  message("\nSTEP - Resolved via word containment: ", nrow(containment_matches))
} else {
  containment_matches <- data.table(Species = character(), FG_num = numeric(), FG_name = character(), match_method = character())  # empty placeholder when nothing matched
  message("\nSTEP - No word-containment matches found.")
}

all_matches <- rbindlist(list(direct_matches, exact_matches, genus_matches, containment_matches))  # combine matches from every cascade step
resolved <- merge(unmatched_species[, .(Species, Catch)], all_matches, by = "Species", all.x = TRUE)  # attach matches back onto every unmatched species

MANUAL_OVERRIDES <- data.table(Species = c("Turbot"), CorrectScientificName = c("Scophthalmus maximus"))
overrides_resolved <- merge(MANUAL_OVERRIDES, fg_lookup[, .(ScientificName, FG_num, FG_name)], by.x = "CorrectScientificName", by.y = "ScientificName")  # resolve manual overrides to their FG
overrides_resolved[, match_method := "manual_override"]  # tag how these matches were resolved
resolved_final <- resolved[!Species %in% overrides_resolved$Species]  # drop species that a manual override will replace
resolved_final <- rbindlist(list(
  resolved_final,
  merge(unmatched_species[Species %in% overrides_resolved$Species, .(Species, Catch)],
        overrides_resolved[, .(Species, FG_num, FG_name, match_method)], by = "Species")
), use.names = TRUE)  # add the manually-overridden matches back in

resolved_final[, status := fifelse(is.na(FG_num), "unresolved", "resolved")]  # flag whether each species ended up matched
fwrite(resolved_final, file.path(out_dir, "species_fg_matched.csv"))  # write the full matching result to CSV
message("\n=== FINAL MATCHING SUMMARY === Resolved ", sum(resolved_final$status == "resolved"), " of ",
        nrow(resolved_final), " species (", round(100 * mean(resolved_final$status == "resolved"), 1), "%).")

species_to_fg <- unique(resolved_final[status == "resolved", .(Species, FG_num, FG_name)])  # final species->FG lookup, resolved rows only

## GFCM catches by species/FG x year x Division (all West Med reporting
## countries - the "by species and year and GSA[Division]" deliverable):
gfcm_species_division_fg <- merge(ts_data$species_ts, species_to_fg, by = "Species")  # attach FG to the species-level catch timeseries
gfcm_species_division_fg <- gfcm_species_division_fg[Year >= START_YEAR & Year <= END_YEAR,
                                                     .(Landings_t = sum(Catch, na.rm = TRUE)), by = .(Year, Division, FG_num, FG_name, Species)]  # sum landings by year x division x FG x species, within the study period
fwrite(gfcm_species_division_fg, file.path(out_dir, paste0("gfcm_catches_by_species_year_division_", DATASET_VERSION, ".csv")))  # write result to CSV
message("\n[GFCM] gfcm_catches_by_species_year_division_", DATASET_VERSION, ".csv written - ",
        nrow(gfcm_species_division_fg), " Year x Division x FG x Species row(s). 'Division' here is",
        " GFCM's FAO-division resolution (37.1.1/37.1.2/37.1.3), NOT a GSA number - see this script's header.")

## Country x FG x Year landings for the 6 target countries (backbone for
## the fleet/sector split, discard, and unreported steps below):
gfcm_country_fg <- data.table()
if (nrow(ts_data$country_species_ts) > 0) {
  gfcm_country_fg <- merge(ts_data$country_species_ts, species_to_fg, by = "Species")  # attach FG to the country-level catch timeseries
  gfcm_country_fg <- gfcm_country_fg[Country %in% TARGET_COUNTRIES & Year >= START_YEAR & Year <= END_YEAR,
                                     .(Landings_t = sum(Catch, na.rm = TRUE)), by = .(Country, FG_num, FG_name, Year)]  # sum landings by country x FG x year, target countries and study period only
  message("[GFCM] Country x FG x Year backbone: ", nrow(gfcm_country_fg), " row(s), ",
          uniqueN(gfcm_country_fg$Country), " of ", length(TARGET_COUNTRIES), " target countries present.")
  missing_countries <- setdiff(TARGET_COUNTRIES, unique(gfcm_country_fg$Country))  # target countries with zero rows here
  if (length(missing_countries) > 0) {
    message("[GFCM] WARNING - 0 rows for: ", paste(missing_countries, collapse = ", "),
            ". Check GFCM's own Name_En country strings match TARGET_COUNTRIES exactly.")
  }
} else {
  stop("No country-level GFCM breakdown available (DATASET_VERSION = '", DATASET_VERSION, "') - the fleet/",
       "sector split, discard, and unreported steps below all need this. Use 'GFCM_2025'.")
}

## =================================================================
## DIAGNOSTIC - evaluate what fleet/gear info GFCM's OWN raw data
## actually carries, and tabulate its catches by area (Division -
## GFCM's GSA-proxy, see note above build_westmed_timeseries()). This
## exists so the fleet split below is built on what's actually in the
## data, not assumed - it changes nothing computed elsewhere in this
## script, it only reports.
## =================================================================
if (cfg$format == "gfcm_regional") {
  gfcm_raw_cols <- names(safe_fread(file.path(cfg$data_dir, cfg$capture_file), "capture_file"))  # re-read just the column names of the raw capture file
  fleet_like_cols <- gfcm_raw_cols[grepl("FLEET|GEAR|VESSEL|TECHNIQUE|SECTOR", gfcm_raw_cols, ignore.case = TRUE)]  # find any columns that look fleet-related
  message("\n[Fleet eval] GFCM_Capture_Quantity.csv raw column(s): ", paste(gfcm_raw_cols, collapse = ", "))
  if (length(fleet_like_cols) == 0) {
    message("[Fleet eval] No fleet/gear/vessel/sector-like column in GFCM's own raw capture file -",
            " confirms (in code, not just by assumption) that GFCM's capture-quantity product carries",
            " NO fleet/gear dimension of its own. Every 'fleet' below is therefore necessarily borrowed",
            " from SAU's gear-level catch via FLEET_REGISTER/GEAR_TO_FLEETTYPE a few lines down, never a",
            " GFCM-native fleet breakdown.")
  } else {
    message("[Fleet eval] Possible fleet-like column(s) found: ", paste(fleet_like_cols, collapse = ", "),
            " - these are NOT currently read into load_gfcm_regional(); inspect their unique values",
            " directly before assuming they're unused/irrelevant.")
  }
} else {
  message("\n[Fleet eval] DATASET_VERSION = '", DATASET_VERSION, "' (legacy_excel) - no raw-column fleet",
          " check available for this format; only 'GFCM_2025' has been checked.")
}

## Catches by Country x Division (GSA-proxy) x Year - the actual
## "catches by GSA" GFCM's own data can support, at GFCM's own area
## resolution (Division, NOT a real GSA number):
gfcm_catches_by_area <- ts_data$country_species_ts[
  Country %in% TARGET_COUNTRIES & Year >= START_YEAR & Year <= END_YEAR,
  .(Catch_t = sum(Catch, na.rm = TRUE)), by = .(Country, Division, Year)]  # sum catch by country x division x year
setorder(gfcm_catches_by_area, Country, Division, Year)  # sort rows for readability
fwrite(gfcm_catches_by_area, file.path(out_dir, paste0("gfcm_catches_by_country_division_year_", DATASET_VERSION, ".csv")))  # write result to CSV
gfcm_catches_by_area_summary <- gfcm_catches_by_area[
  , .(Catch_t_total = sum(Catch_t, na.rm = TRUE), n_years = uniqueN(Year)), by = .(Country, Division)]  # total catch and year count per country x division
setorder(gfcm_catches_by_area_summary, Country, -Catch_t_total)  # sort by country, largest catch first
message("\n[Fleet eval] Catches by Country x Division written to gfcm_catches_by_country_division_year_",
        DATASET_VERSION, ".csv - ", nrow(gfcm_catches_by_area), " Country x Division x Year row(s) across ",
        uniqueN(gfcm_catches_by_area$Division), " Division(s). Division is GFCM's own area resolution,",
        " NOT a real GSA number - see this script's header.")
print(gfcm_catches_by_area_summary)  # print the summary to console

## =================================================================
## # distribute catches over sector and fleet (country and gear)
## =================================================================

## --- fleet TAXONOMY (which fleets exist, per country and GSA) - hand-
## transcribed from GFCM Regional Fleet Register / stock-assessment
## reporting. No vessel counts, no weights: the actual catch-share
## weighting a few lines below comes from SAU's real gear-level catch,
## mapped onto these named fleet types via GEAR_TO_FLEETTYPE. ---------
FLEET_REGISTER <- data.table(
  Country  = c(rep("Morocco", 4), rep("Algeria", 5), rep("Tunisia", 4),
               rep("France", 6), rep("Spain", 5), rep("Italy", 5)),
  GSA      = c("GSA 3", "GSA 3", "GSA 3", "GSA 3",
               "GSA 4", "GSA 4", "GSA 4", "GSA 4", "GSA 4",
               "GSA 12", "GSA 12", "GSA 12", "GSA 12",
               "GSA 7", "GSA 7", "GSA 7,8", "GSA 7,8", "GSA 7,8", "GSA 7,8",
               "GSA 1,3,4,5,6", "GSA 1,2,4,5,6,7", "GSA 1-6", "GSA 1,5,6", "GSA 1-6",
               "GSA 9,10,11.2", "GSA 9,10,11.2", "GSA 9,10,11.2", "GSA 9,10,11.2", "GSA 9,10,11.2"),
  FleetType = c("Artisanal", "Longlines", "Purse seiners", "Trawls -n.e.i-",
                "Artisanal", "Longlines", "Purse seiners", "Midwater trawls (nei)", "Bottom trawls",
                "Artisanal", "Longlines", "Purse seiners", "Trawls -n.e.i-",
                "Purse seiners", "Midwater trawls", "Bottom trawls", "Artisanal", "Drifting longlines", "Hooks and line not drifting lines",
                "Purse seiners", "Bottom trawls", "Artisanal", "Drifting longlines", "Set longlines",
                "Purse seiners", "Bottom trawls", "Artisanal", "Drifting longlines", "Set longlines"),
  Comment  = c("only partially reported. Handlines and hand-operated pole-and-lines. Some smaller than 15 meters",
               "Not identified in the reporting (nei)", "", "need to check whether these include midwater trawls - stock assessment info doesn't say",
               "", "", "surprisingly high count - verify", "~11% sardines", "",
               "", "", "", "",
               "targeting tuna - need to better understand this", "", "", "", "", "",
               "", "unconfirmed whether GSA 7 belongs here", "", "", "",
               "", "do NOT use beam (5) or midwater (3) trawler categories here", "", "", "")
)
FLEET_REGISTER[, Sector := fifelse(FleetType == "Artisanal", "Artisanal", "Industrial")]  # derive Sector from FleetType

recreational_register <- unique(FLEET_REGISTER[, .(Country, GSA)])[, .(GSA = paste(unique(GSA), collapse = "; ")), by = Country]  # one row per country, GSAs collapsed into one string
recreational_register[, `:=`(FleetType = "Recreational", Sector = "Recreational",
                             Comment = "not enumerated in this register - see RECREATIONAL_CATCH_MANUAL")]  # add a placeholder Recreational fleet row per country
FLEET_REGISTER <- rbindlist(list(FLEET_REGISTER, recreational_register), use.names = TRUE)  # append the Recreational rows onto the register
setcolorder(FLEET_REGISTER, c("Country", "GSA", "Sector", "FleetType", "Comment"))  # standardize column order
message("\n[Fleet register] ", nrow(FLEET_REGISTER), " Country x FleetType row(s) - taxonomy only,",
        " no weighting attached here.")

## --- DIAGNOSTIC - fleet definition vs matching check: does FLEET_
## REGISTER's declared GSA(s) per country look plausible against
## where GFCM's own catch (gfcm_catches_by_area_summary, just above)
## actually sits for that country? GSA and Division are two different
## area schemes (see this script's header) - there is no real join
## between them, so this reports a coverage comparison side by side,
## it does not resolve or silently reassign anything. -----------------
fleet_gsa_by_country <- unique(FLEET_REGISTER[Sector != "Recreational", .(Country, GSA, FleetType)])[
  , .(Declared_GSAs = paste(sort(unique(GSA)), collapse = " | "),
      FleetTypes = paste(sort(unique(FleetType)), collapse = ", "),
      n_fleets = uniqueN(FleetType)), by = Country]  # summarize declared GSAs and fleet types per country
division_by_country <- gfcm_catches_by_area_summary[
  , .(Divisions_with_GFCM_catch = paste(sort(unique(Division)), collapse = ", "),
      n_divisions = uniqueN(Division)), by = Country]  # summarize which GFCM divisions have catch per country
fleet_vs_division_check <- merge(fleet_gsa_by_country, division_by_country, by = "Country", all = TRUE)  # side-by-side comparison table
setorder(fleet_vs_division_check, Country)  # sort by country
fwrite(fleet_vs_division_check, file.path(out_dir, "fleet_definition_vs_gfcm_division_check.csv"))  # write result to CSV
message("\n[Fleet eval] Fleet-definition-vs-GFCM-coverage check written to",
        " fleet_definition_vs_gfcm_division_check.csv - read as 'does the register's declared GSA claim",
        " look plausible given where GFCM's own catch is concentrated', NOT a resolved GSA<->Division join.")
print(fleet_vs_division_check)  # print the comparison table to console

## --- SAU gear -> your named FleetType, per country (this is what
## actually WEIGHTS the split below - see GEAR_TO_FLEETTYPE's own
## comment for why `weight` exists) -----------------------------------
GEAR_TO_FLEETTYPE <- data.table(
  Country = c(
    "Morocco","Morocco","Morocco","Morocco","Morocco","Morocco",
    "Algeria","Algeria","Algeria","Algeria","Algeria","Algeria",
    "Tunisia","Tunisia","Tunisia","Tunisia","Tunisia","Tunisia",
    "France","France","France","France","France","France","France",
    "Spain","Spain","Spain","Spain","Spain","Spain",
    "Italy","Italy","Italy","Italy","Italy","Italy"
  ),
  sau_gear = c(
    "Purse seine", "Longline", "Set longline", "Bottom trawl", "Midwater trawl", "Gillnet",
    "Purse seine", "Longline", "Set longline", "Bottom trawl", "Midwater trawl", "Gillnet",
    "Purse seine", "Longline", "Set longline", "Bottom trawl", "Midwater trawl", "Gillnet",
    "Purse seine", "Midwater trawl", "Bottom trawl", "Longline", "Handline", "Pole-and-line", "Gillnet",
    "Purse seine", "Bottom trawl", "Longline", "Longline", "Set longline", "Gillnet",
    "Purse seine", "Bottom trawl", "Longline", "Longline", "Set longline", "Gillnet"
  ),
  FleetType = c(
    "Purse seiners", "Longlines", "Longlines", "Trawls -n.e.i-", "Trawls -n.e.i-", "Artisanal",
    "Purse seiners", "Longlines", "Longlines", "Bottom trawls", "Midwater trawls (nei)", "Artisanal",
    "Purse seiners", "Longlines", "Longlines", "Trawls -n.e.i-", "Trawls -n.e.i-", "Artisanal",
    "Purse seiners", "Midwater trawls", "Bottom trawls", "Drifting longlines", "Hooks and line not drifting lines", "Hooks and line not drifting lines", "Artisanal",
    "Purse seiners", "Bottom trawls", "Drifting longlines", "Set longlines", "Set longlines", "Artisanal",
    "Purse seiners", "Bottom trawls", "Drifting longlines", "Set longlines", "Set longlines", "Artisanal"
  ),
  weight = c(
    1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1,
    1, 1, 1, 1, 1, 1, 1,
    1, 1, 0.5, 1, 0.5, 1,
    1, 1, 0.5, 1, 0.5, 1
  )
)
GEAR_TO_FLEETTYPE[, weight := weight / sum(weight), by = .(Country, sau_gear)]  # normalize weights to sum to 1 within each country x gear (splits ambiguous gear-to-fleet mappings)

## --- SAU raw extract, loaded/downloaded directly (copied in from
## lib_sau_extraction.R, not sourced - see this file's header) --------
SAU_WEST_MED_EEZS <- list(
  "12"  = list(country = "Algeria", area = "Algeria"),
  "788" = list(country = "Tunisia", area = "Tunisia"),
  "947" = list(country = "Morocco", area = "Morocco (Mediterranean)"),
  "918" = list(country = "France", area = "France (Mediterranean)"),
  "899" = list(country = "France", area = "Corsica"),
  "380" = list(country = "Italy", area = "Italy (mainland)"),
  "902" = list(country = "Italy", area = "Sardinia"),
  "901" = list(country = "Italy", area = "Sicily"),
  "962" = list(country = "Spain", area = "Spain (mainland, Med + Gulf of Cadiz)"),
  "903" = list(country = "Spain", area = "Balearic Islands")
)
SAU_BASE_URL <- "https://api.seaaroundus.org/api/v1/"
SAU_UA <- httr::user_agent("sau-west-med-master/1.0")

sau_sanitize_raw_types <- function(df) {
  if ("year" %in% names(df)) df$year <- suppressWarnings(as.integer(as.character(df$year)))  # coerce year to integer
  numeric_like <- grep("tonnes|value|catch|landed", names(df), ignore.case = TRUE, value = TRUE)  # find columns that should be numeric
  for (col in numeric_like) df[[col]] <- suppressWarnings(as.numeric(as.character(df[[col]])))  # coerce them to numeric
  for (col in names(df)) if (is.logical(df[[col]]) && all(is.na(df[[col]]))) df[[col]] <- as.character(df[[col]])  # avoid all-NA logical columns breaking later rbinds
  df
}
sau_readr_or_base_read_csv <- function(path) {
  if (requireNamespace("readr", quietly = TRUE)) readr::read_csv(path, show_col_types = FALSE, progress = FALSE)  # prefer readr if available
  else utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)  # fall back to base read.csv
}
SAU_MEASURE <- "tonnage"
sau_get_raw_extract <- function(region_id, retries = 3, timeout_s = 180, year_min = 1994, year_max = 2019) {
  url <- sprintf("%s%s/%s/sector/?format=csv&limit=10&sciname=false&region_id=%s", SAU_BASE_URL, "eez", SAU_MEASURE, region_id)  # build the SAU API request URL for this EEZ
  resp <- NULL
  for (attempt in seq_len(retries)) {
    resp <- tryCatch(httr::GET(url, SAU_UA, httr::timeout(timeout_s)), error = function(e) e)  # request the CSV export
    if (!inherits(resp, "error") && httr::status_code(resp) == 200) break  # success, stop retrying
    if (attempt == retries) {
      message(sprintf("  ! failed raw extract for eez %s", region_id))
      return(tibble::tibble())  # give up after exhausting retries, return empty
    }
    Sys.sleep(3 * attempt)  # back off before retrying
  }
  zip_path <- tempfile(fileext = ".zip")
  writeBin(httr::content(resp, as = "raw"), zip_path)  # save the downloaded zip to a temp file
  csv_name <- tryCatch(utils::unzip(zip_path, list = TRUE)$Name[1], error = function(e) NA)  # find the CSV's name inside the zip
  if (is.na(csv_name)) { unlink(zip_path); return(tibble::tibble()) }
  exdir <- tempfile("sau_"); dir.create(exdir)  # temp dir to extract into
  utils::unzip(zip_path, files = csv_name, exdir = exdir)  # extract just that CSV
  df <- suppressWarnings(sau_readr_or_base_read_csv(file.path(exdir, csv_name)))  # read the extracted CSV
  unlink(zip_path); unlink(exdir, recursive = TRUE)  # clean up temp files
  df <- sau_sanitize_raw_types(df)  # coerce column types
  if ("year" %in% names(df)) df <- df[df$year >= year_min & df$year <= year_max, , drop = FALSE]  # restrict to the requested year range
  df
}
sau_download_raw_extract <- function(csv_path, year_min = 1994, year_max = 2019) {
  message("[SAU] Fetching raw per-EEZ extracts from SAU's API (this can take a minute)...")
  raw_frames <- list(); failed_eez <- character(0)
  for (region_id in names(SAU_WEST_MED_EEZS)) {
    meta <- SAU_WEST_MED_EEZS[[region_id]]
    message(sprintf("  EEZ %s (%s)", region_id, meta$area))
    df <- sau_get_raw_extract(region_id, year_min = year_min, year_max = year_max)  # download this EEZ's raw extract
    if (nrow(df) > 0) { df$country <- meta$country; df$area <- meta$area; raw_frames[[length(raw_frames) + 1]] <- df }  # tag country/area and collect
    else failed_eez <- c(failed_eez, sprintf("%s (%s)", region_id, meta$area))  # record the failure
  }
  if (length(raw_frames) == 0) { message("[SAU] All per-EEZ raw extracts failed. No file written."); return(FALSE) }
  raw_combined <- dplyr::bind_rows(raw_frames)  # stack all EEZ extracts into one table
  csv_dir <- dirname(csv_path)
  if (!dir.exists(csv_dir)) dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)  # ensure the output dir exists
  if (requireNamespace("readr", quietly = TRUE)) readr::write_csv(raw_combined, csv_path)  # prefer readr for writing
  else utils::write.csv(raw_combined, csv_path, row.names = FALSE)  # fall back to base write.csv
  message(sprintf("[SAU] Wrote %s (%d rows, %d of %d EEZs succeeded).", csv_path, nrow(raw_combined),
                  length(raw_frames), length(SAU_WEST_MED_EEZS)))
  if (length(failed_eez) > 0) message("[SAU] NOTE: failed EEZ(s): ", paste(failed_eez, collapse = ", "))
  TRUE
}

sau_dir_files <- if (dir.exists(SAU_DIR)) list.files(SAU_DIR, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE) else character(0)  # list manually-downloaded SAU CSVs, if any

if (length(sau_dir_files) == 0 && isTRUE(AUTO_DOWNLOAD_SAU) && !file.exists(SAU_RAW_CSV)) {
  sau_download_raw_extract(SAU_RAW_CSV, year_min = START_YEAR, year_max = END_YEAR)  # auto-download as a last resort, if enabled
}

sau_country_fg_gear <- data.table()
sau_country_fg_gear_year <- data.table()
sau_total_cy <- data.table(); sau_reported_cy <- data.table()
sau_sector_prop <- data.table()
sau_discard_ratio_by_year <- data.table()

if (length(sau_dir_files) == 0 && !file.exists(SAU_RAW_CSV)) {
  message("\n[SAU] No CSV file(s) found in '", SAU_DIR, "' (and no legacy '", SAU_RAW_CSV, "' cache) -",
          " the fleet-weighting and unreported-% steps below will be skipped (fleet split falls back to",
          " equal shares across a country's FleetTypes, flagged fleet_split_source; unreported % will be",
          " NA). Manually download SAU's catch-by-EEZ data (seaaroundus.org - open each of Spain/France/",
          " Italy/Morocco/Algeria/Tunisia's EEZ page and use its 'Download data' button for catch by",
          " species/gear/sector/year) into '", SAU_DIR, "', one CSV per country/EEZ is fine - all *.csv",
          " files found there are read and stacked together.")
} else {
  sau_raw <- if (length(sau_dir_files) > 0) {
    message("\n[SAU] Reading ", length(sau_dir_files), " CSV file(s) from '", SAU_DIR, "': ",
            paste(basename(sau_dir_files), collapse = ", "))
    rbindlist(lapply(sau_dir_files, fread), use.names = TRUE, fill = TRUE)  # read and stack all CSVs found in SAU_DIR
  } else {
    message("\n[SAU] '", SAU_DIR, "' has no CSVs - falling back to the legacy combined file '", SAU_RAW_CSV, "'.")
    fread(SAU_RAW_CSV)  # read the legacy single combined file instead
  }
  sau_candidates <- list(gear = c("gear_type", "gear"), sci_name = c("scientific_name"),
                         tonnes = c("tonnes", "catch_sum", "value"), report = c("reporting_status"),
                         sector = c("fishing_sector", "fishing_entity", "sector"),
                         catch_type = c("catch_type"))  # possible column-name variants per required field
  sau_resolved <- list()
  for (field in names(sau_candidates)) for (opt in sau_candidates[[field]]) if (opt %in% names(sau_raw)) { sau_resolved[[field]] <- opt; break }  # find which variant is actually present for each field
  missing_sau_cols <- setdiff(c("gear", "sci_name", "tonnes"), names(sau_resolved))  # required fields with no matching column
  if (length(missing_sau_cols) > 0) stop("[SAU] Raw extract missing required column(s): ", paste(missing_sau_cols, collapse = ", "))
  setnames(sau_raw, unlist(sau_resolved), names(sau_resolved))  # rename resolved columns to standard names
  if (!"report" %in% names(sau_resolved)) sau_raw[, report := NA_character_]  # add a placeholder if no reporting-status column exists
  
  sau_raw <- sau_raw[!is.na(year) & year >= START_YEAR & year <= END_YEAR & country %in% TARGET_COUNTRIES]  # keep only target countries within the study period
  message("\n[SAU] ", nrow(sau_raw), " rows after year/country filter.")
  
  ## species -> FG match on SAU's own scientific names, reusing fg_lookup
  ## built above for the GFCM cascade (direct match, then genus fallback -
  ## a second, lighter cascade than GFCM's, appropriate since SAU already
  ## gives scientific names directly rather than English common names).
  sau_sci <- data.table(ScientificName = unique(sau_raw$sci_name))  # distinct scientific names in the SAU extract
  sau_direct <- merge(sau_sci, fg_lookup[, .(ScientificName, FG_num, FG_name)], by = "ScientificName")  # direct scientific-name match
  sau_direct_safe <- resolve_matches_safely(sau_direct, query_col = "ScientificName")$safe  # keep only unambiguous direct matches
  sau_remaining <- sau_sci[!ScientificName %in% sau_direct_safe$ScientificName]  # names still unmatched
  sau_remaining[, genus := extract_genus(ScientificName)]  # extract genus for the fallback match
  sau_genus <- merge(sau_remaining[!is.na(genus)], fg_lookup[!is.na(genus), .(genus, FG_num, FG_name)], by = "genus", allow.cartesian = TRUE)  # match on genus
  sau_genus_safe <- resolve_matches_safely(sau_genus, query_col = "ScientificName")$safe  # keep only unambiguous genus matches
  sau_species_fg <- rbindlist(list(sau_direct_safe[, .(ScientificName, FG_num, FG_name)],
                                   sau_genus_safe[, .(ScientificName, FG_num, FG_name)]), use.names = TRUE)  # combine direct and genus matches
  message("[SAU] ", nrow(sau_species_fg), " of ", nrow(sau_sci), " distinct SAU species matched to an FG.")
  
  sau_raw <- merge(sau_raw, sau_species_fg, by.x = "sci_name", by.y = "ScientificName", all.x = TRUE)  # attach FG to every SAU catch row
  
  ## Country x FG x gear catch, summed across all years SAU has -
  ## this is what weights the fleet split below (time-invariant
  ## fallback - kept for Country x FG x Year cells the year-resolved
  ## version below has no SAU catch for at all).
  sau_country_fg_gear <- sau_raw[!is.na(FG_num), .(Catch_t = sum(tonnes, na.rm = TRUE)),
                                 by = .(Country = country, FG_num, sau_gear = gear)]  # sum catch by country x FG x gear, across all years
  
  ## Same, but keeping Year - this is what actually lets SAU's real
  ## year-to-year variation feed the pre-2014 hindcast below, instead
  ## of one flat average applied to every year.
  sau_country_fg_gear_year <- sau_raw[!is.na(FG_num), .(Catch_t = sum(tonnes, na.rm = TRUE)),
                                      by = .(Country = country, FG_num, sau_gear = gear, Year = year)]  # sum catch by country x FG x gear x year
  
  ## SAU's own reconstructed total vs its "reported" subset - see the
  ## unreported-% step below for how this is used.
  sau_total_cy <- sau_raw[, .(tonnes = sum(tonnes, na.rm = TRUE)), by = .(Country = country, Year = year)]  # SAU's total reconstructed catch by country x year
  if (!is.null(sau_resolved$report)) {
    sau_reported_cy <- sau_raw[grepl("^report", report, ignore.case = TRUE) & !grepl("unreport", report, ignore.case = TRUE),
                               .(tonnes = sum(tonnes, na.rm = TRUE)), by = .(Country = country, Year = year)]  # SAU's "reported" subset only, by country x year
  } else {
    message("[SAU] No reporting-status column in this extract - unreported % will be NA.")
  }
  
  ## Country x FG x Sector (Artisanal/Industrial/Subsistence/Recreational)
  ## proportions - SAU's own sector split, used below (a) as a real,
  ## if approximate, estimate for the Recreational bucket GFCM/FDI can't
  ## give at all, and (b) as a cross-check on the Artisanal share of the
  ## fleet split for the years/countries STECF FDI doesn't cover. Kept
  ## separate from sau_country_fg_gear (gear-level) - this is a coarser,
  ## sector-level cut of the same SAU extract.
  sau_sector_prop <- data.table()
  if (!is.null(sau_resolved$sector)) {
    sau_sector_cy <- sau_raw[!is.na(FG_num), .(Catch_t = sum(tonnes, na.rm = TRUE)),
                             by = .(Country = country, FG_num, Sector = sector)]  # sum catch by country x FG x sector
    sau_sector_prop <- copy(sau_sector_cy)
    sau_sector_prop[, prop_sector := Catch_t / sum(Catch_t), by = .(Country, FG_num)]  # convert to a proportion within each country x FG
    message("[SAU] Sector field found ('", sau_resolved$sector, "') - sector value(s): ",
            paste(sort(unique(sau_sector_prop$Sector)), collapse = ", "),
            ". sau_sector_prop: ", nrow(sau_sector_prop), " Country x FG x Sector row(s).")
  } else {
    message("[SAU] No fishing-sector column in this extract (tried: ", paste(sau_candidates$sector, collapse = ", "),
            ") - Recreational catch stays 'not estimated' and the Artisanal-share cross-check is skipped.")
  }
  
  ## SAU's own Landings-vs-Discards split (catch_type field, part of
  ## SAU's standard reconstructed-catch schema) - a REAL discard ratio,
  ## not a proxy, used below (hindcast section) to replace FishMIP's
  ## Med-wide ratio as the discards fallback for every Country x FG x
  ## Year cell STECF FDI doesn't cover.
  sau_discard_ratio_by_year <- data.table()
  if (!is.null(sau_resolved$catch_type)) {
    sau_catch_type_cfy <- sau_raw[!is.na(FG_num) & !is.na(catch_type),
                                  .(tonnes = sum(tonnes, na.rm = TRUE)),
                                  by = .(Country = country, FG_num, Year = year, catch_type)]  # sum catch by country x FG x year x catch_type
    sau_catch_type_cfy[, is_discard := grepl("discard", catch_type, ignore.case = TRUE)]  # flag which catch_type rows are discards
    sau_discard_ratio_by_year <- sau_catch_type_cfy[, .(Discarded_t = sum(tonnes[is_discard], na.rm = TRUE),
                                                        Landed_t    = sum(tonnes[!is_discard], na.rm = TRUE)),
                                                    by = .(Country, FG_num, Year)]  # split each cell into discarded vs landed tonnes
    sau_discard_ratio_by_year[, discard_ratio := Discarded_t / (Landed_t + Discarded_t)]  # compute discard ratio
    sau_discard_ratio_by_year <- sau_discard_ratio_by_year[is.finite(discard_ratio) & discard_ratio >= 0 & discard_ratio <= 0.95,
                                                           .(Country, FG_num, Year, discard_ratio)]  # drop implausible/invalid ratios
    message("[SAU] catch_type field found - sau_discard_ratio_by_year: ", nrow(sau_discard_ratio_by_year),
            " Country x FG x Year row(s) with a real SAU-derived discard ratio.")
  } else {
    message("[SAU] No catch_type column in this extract (tried: 'catch_type') - SAU can't supply a discard",
            " ratio; the discards fallback for non-FDI cells stays FishMIP's Med-wide ratio.")
  }
}

## Map SAU's gear vocabulary onto named FleetType per country, splitting
## evenly wherever GEAR_TO_FLEETTYPE flags real ambiguity.
fleet_types_ref <- unique(FLEET_REGISTER[Sector %in% c("Artisanal", "Industrial"), .(Country, GSA, Sector, FleetType, Comment)])  # non-recreational fleet taxonomy reference

if (nrow(sau_country_fg_gear) > 0) {
  mapped <- merge(sau_country_fg_gear, GEAR_TO_FLEETTYPE, by = c("Country", "sau_gear"), allow.cartesian = TRUE)  # map SAU gear onto named fleet types
  mapped[, Catch_t := Catch_t * weight]  # apply the ambiguity-splitting weight
  unmatched_gear <- sau_country_fg_gear[!mapped, on = c("Country", "sau_gear")]  # gear combinations with no mapping entry
  if (nrow(unmatched_gear) > 0) {
    unmatched_gear[, FleetType := "Unclassified"]  # bucket unmapped gear as Unclassified
    unmatched_share <- round(100 * sum(unmatched_gear$Catch_t) / sum(sau_country_fg_gear$Catch_t), 1)  # what share of catch this represents
    message("\n[Fleet split] ", nrow(unmatched_gear), " Country x gear combination(s) (", unmatched_share,
            "% of SAU's catch value here) aren't in GEAR_TO_FLEETTYPE - kept as 'Unclassified'.")
  }
  mapped_all <- rbindlist(list(mapped[, .(Country, FG_num, FleetType, Catch_t)],
                               unmatched_gear[, .(Country, FG_num, FleetType, Catch_t)]),
                          use.names = TRUE, fill = TRUE)  # combine mapped and unclassified catch
  mapped_all <- mapped_all[, .(Catch_t = sum(Catch_t, na.rm = TRUE)), by = .(Country, FG_num, FleetType)]  # sum catch by country x FG x fleet type
  mapped_all[, prop_fleet := Catch_t / sum(Catch_t), by = .(Country, FG_num)]  # convert to a share within each country x FG
  
  country_overall <- mapped_all[, .(Catch_t = sum(Catch_t, na.rm = TRUE)), by = .(Country, FleetType)]  # coarser country-level fleet mix, ignoring FG
  country_overall[, prop_fleet := Catch_t / sum(Catch_t), by = Country]  # convert to a share within each country
} else {
  mapped_all <- data.table(Country = character(), FG_num = integer(), FleetType = character(), prop_fleet = numeric())  # empty placeholder when SAU has no gear-level data
  country_overall <- data.table(Country = character(), FleetType = character(), prop_fleet = numeric())  # empty placeholder
}

fleet_prop <- merge(CJ(Country = unique(fleet_types_ref$Country), FG_num = unique(gfcm_country_fg$FG_num)),
                    fleet_types_ref, by = "Country", allow.cartesian = TRUE)  # cross join every country x FG with that country's fleet types
fleet_prop <- merge(fleet_prop, mapped_all[, .(Country, FG_num, FleetType, prop_fleet)],
                    by = c("Country", "FG_num", "FleetType"), all.x = TRUE)  # attach the SAU-derived fleet share, where available
fleet_prop[, fleet_split_source := NA_character_]  # placeholder column to track how each row's share was derived
missing_fg_split <- fleet_prop[is.na(prop_fleet)]  # rows with no FG-specific SAU share
if (nrow(missing_fg_split) > 0 && nrow(country_overall) > 0) {
  fleet_prop <- merge(fleet_prop, country_overall[, .(Country, FleetType, prop_fleet_country = prop_fleet)],
                      by = c("Country", "FleetType"), all.x = TRUE)  # attach the coarser country-level fleet share as a fallback
  fleet_prop[is.na(prop_fleet), `:=`(prop_fleet = prop_fleet_country,
                                     fleet_split_source = "SAU country-level mix (no SAU catch for this Country x FG - fallback)")]  # fill missing FG-level shares from the country-level fallback
  fleet_prop[, prop_fleet_country := NULL]  # drop the now-unneeded helper column
} else if (nrow(country_overall) == 0) {
  fleet_prop[, `:=`(prop_fleet = 1 / .N, fleet_split_source = "no SAU data at all - equal share across this country's fleet types (last resort)"), by = .(Country, FG_num)]  # last-resort equal split
}
fleet_prop[is.na(fleet_split_source) | fleet_split_source == "", fleet_split_source :=
             "SAU (country x FG x gear, mapped to FleetType via GEAR_TO_FLEETTYPE)"]  # label the remaining rows as coming from the primary SAU mapping
message("\n[Fleet split] fleet_prop: ", nrow(fleet_prop), " Country x FG x FleetType share row(s).")

## Same mapping, but keeping Year - SAU's own real year-by-year fleet
## shares (not the flat average above), the raw material for the
## pre-2014 hindcast a few sections down. Sparse cells (a Country x FG
## x Year SAU has zero catch for) are simply absent here - the hindcast
## step falls back to fleet_prop's flat average for those.
if (nrow(sau_country_fg_gear_year) > 0) {
  mapped_year <- merge(sau_country_fg_gear_year, GEAR_TO_FLEETTYPE, by = c("Country", "sau_gear"), allow.cartesian = TRUE)  # map SAU gear onto fleet types, keeping Year
  mapped_year[, Catch_t := Catch_t * weight]  # apply the ambiguity-splitting weight
  unmatched_gear_year <- sau_country_fg_gear_year[!mapped_year, on = c("Country", "sau_gear", "Year")]  # gear combinations with no mapping entry
  if (nrow(unmatched_gear_year) > 0) unmatched_gear_year[, FleetType := "Unclassified"]  # bucket unmapped gear as Unclassified
  mapped_all_year <- rbindlist(list(mapped_year[, .(Country, FG_num, Year, FleetType, Catch_t)],
                                    unmatched_gear_year[, .(Country, FG_num, Year, FleetType, Catch_t)]),
                               use.names = TRUE, fill = TRUE)  # combine mapped and unclassified catch
  mapped_all_year <- mapped_all_year[, .(Catch_t = sum(Catch_t, na.rm = TRUE)), by = .(Country, FG_num, Year, FleetType)]  # sum catch by country x FG x year x fleet type
  mapped_all_year[, prop_fleet := Catch_t / sum(Catch_t), by = .(Country, FG_num, Year)]  # convert to a share within each country x FG x year
} else {
  mapped_all_year <- data.table(Country = character(), FG_num = integer(), Year = integer(),
                                FleetType = character(), prop_fleet = numeric())  # empty placeholder when SAU has no year-resolved gear data
}
sau_fleet_prop_by_year <- merge(mapped_all_year, fleet_types_ref, by = c("Country", "FleetType"), all.x = TRUE)  # attach GSA/Sector/Comment metadata
sau_fleet_prop_by_year[is.na(GSA), GSA := "(SAU-derived, see Catches_ByCountryFleetSector)"]  # fill missing GSA with a note
sau_fleet_prop_by_year[is.na(Sector), Sector := fifelse(FleetType == "Artisanal", "Artisanal", "Industrial")]  # derive missing Sector from FleetType
sau_fleet_prop_by_year <- sau_fleet_prop_by_year[, .(Country, FG_num, Year, Sector, FleetType, GSA, Comment, prop_fleet)]  # reorder/select final columns
message("[Fleet split] sau_fleet_prop_by_year: ", nrow(sau_fleet_prop_by_year), " Country x FG x FleetType x",
        " Year share row(s), ", uniqueN(sau_fleet_prop_by_year$Year), " year(s) - raw material for the",
        " pre-2014 hindcast below.")

## =================================================================
## STECF FDI (Fisheries Dependent Information) - EU fleet-dependent
## landings, DISCARDS, and effort (days x capacity) by gear/metier x
## GSA x quarter x species, for Spain/France/Italy ONLY (Morocco/
## Algeria/Tunisia aren't EU Member States and don't report into FDI).
## Per Andrea's flow: FDI is the PRIMARY fleet source for these 3
## countries from 2014 onward (its own trusted coverage window); SAU fills the
## gaps - years before 2014, and any Country x FG STECF doesn't cover
## (see the fleet_prop_final blend and the hindcast section below, and
## sau_sector_prop above for the Artisanal/Recreational cross-check).
##
## Manual download (STECF has no API): grab the "Effort, landings,
## catches, capacity, biological" bulk file from https://stecf.ec.
## europa.eu/data-dissemination/fdi_en, unzip it, and point
## STECF_FDI_DIR at the unzipped folder. Real structure (confirmed
## against Andrea's own download, Sept 2026) is five subfolders, NOT
## one single catch table:
##   Catches/FDI Catches by country<YEAR>.csv   - one file per year,
##     2013-2024, country/gear/metier/GSA-resolved, WITH BOTH
##     total_live_weight_landed AND tot_discards_tonnes as separate
##     columns on the same row (no landings/discards flag needed -
##     this is the table used below for catch + discards + fleet
##     split).
##   Landings/FDI Landings EU <YEAR>.csv        - same grain, landings
##     only, no discards column - NOT used here since Catches/ already
##     has everything (landings AND discards) this pipeline needs.
##   Effort/FDI Effort by country.csv           - ONE file, all years,
##     country/gear/metier/GSA-resolved days-at-sea x kW/GT-days
##     capacity metrics (used below for STECF_FDI_Effort_by_GSA). NOTE:
##     several of its column names carry a stray space (e.g.
##     "total _GT_days_at_sea") - stripped on load, see clean_fdi_names().
##   Effort/FDI Effort EU.csv                   - EU-aggregate version
##     of the same effort metrics, clean column names - not used here
##     (country-level detail is what this pipeline needs).
##   Capacity/FDI Capacity by country.csv       - fleet CAPACITY
##     (vessel counts/GT/kW by broad fishing_tech, no gear/metier) -
##     NOT the same thing as capacity-WEIGHTED EFFORT above; not used
##     here, but available as a structural cross-check if ever needed.
## Entirely optional - skipped with a message, fleet_prop_final falls
## back to SAU/default everywhere, if STECF_FDI_DIR isn't there yet.
## =================================================================
## Same versioned-subfolder problem as GFCM above - Andrea's real
## download unzips into pcloud_dir/data/fisheries/FDI/<year>_Effort-
## landings-catches-capacity-biological/ (e.g. "2025_Effort-landings-
## catches-capacity-biological"), not directly into FDI/ itself, and
## that year-stamped name will change on every future download.
## find_versioned_subdir() (defined above) looks for whichever folder
## actually has a Catches/ subfolder in it, so this doesn't need
## re-fixing next time the folder name changes.
STECF_FDI_PARENT_DIR <- file.path(pcloud_dir, "data/fisheries/FDI")
STECF_FDI_DIR <- find_versioned_subdir(STECF_FDI_PARENT_DIR, "Catches")  # locate the year-stamped unzipped FDI folder
if (is.na(STECF_FDI_DIR)) {
  message("\n[Paths] No folder with a 'Catches' subfolder found under '", STECF_FDI_PARENT_DIR, "' - falling",
          " back to that parent folder itself; the STECF FDI block below will report it missing if that's",
          " not where the unzipped download actually is.")
  STECF_FDI_DIR <- STECF_FDI_PARENT_DIR  # fall back to the parent folder itself
} else {
  message("\n[Paths] STECF_FDI_DIR resolved to: '", STECF_FDI_DIR, "'.")
}
## 2013 is FDI's first reporting year and, per Andrea's own review, its
## data quality that year isn't trustworthy - member states were still
## ramping up their DCF submissions. Bumping this to 2014 means every
## downstream mechanism keyed off it (the STECF/SAU tier split, the
## hindcast calibration, the diagram's coverage-start line) automatically
## treats 2013 as "before FDI" - it falls through to the SAU hindcast
## tier (bias-corrected against FDI's real 2014+ overlap) rather than
## being read from FDI's own unreliable first year.
STECF_FDI_START_YEAR <- 2014   # FDI's own TRUSTED coverage window starts here - 2013 excluded (data quality), SAU/hindcast covers everything before this

## Reference year for the technology-creep correction below (Effort_
## kWdays_per_vessel_effective) - i.e. "express every year's effective
## fishing power relative to this year's technology". STECF_FDI_START_YEAR
## itself is the natural choice: it's the first year with a real, reported
## kW-days-per-vessel figure to correct FROM.
TECH_CREEP_BASE_YEAR <- STECF_FDI_START_YEAR  # reference year for the technology-creep correction, tied to FDI's start year

## Per Andrea: the creep % should (1) vary across countries - EU vs
## non-EU, not every country individually - and (2) vary over time
## rather than being one flat rate for the whole series.
##
## (1) Country differential: EU member states' EMFF/CFP-subsidized fleet
## modernization (engine, gear, and electronics grants) is documented to
## have driven FASTER capacity upgrades than Morocco/Algeria/Tunisia's
## largely artisanal, less-subsidized fleets. Palomares & Pauly (2019)
## give no country-specific number for this at all - there is NO
## literature figure behind this multiplier, it is A JUDGMENT CALL:
## EU-3 (Spain/France/Italy) keep the full rate (multiplier 1); Morocco/
## Algeria/Tunisia get it reduced by 30% (multiplier 0.7). Change these
## two numbers directly if you have a better source or a different view.
TECH_CREEP_COUNTRY_MULTIPLIER <- data.table(
  Country = c("Spain", "France", "Italy", "Morocco", "Algeria", "Tunisia"),
  creep_multiplier = c(1, 1, 1, 0.7, 0.7, 0.7)
)

## (2) Time-varying rate, kept inside the realistic 0-5%/year creep range
## Andrea named (the earlier draft re-evaluated Palomares & Pauly's
## duration-average formula at each year's own short elapsed distance from
## the base year, which spiked to 13.8%/year right at the start - correct
## algebraically but not a realistic ANNUAL rate, since that formula's Y is
## meant as a whole-window average, not something to re-derive one year at
## a time). Replaced with a plain per-PERIOD annual rate, each one a flat
## %, applied by simple proportional year-by-year compounding (multiplier
## for year Y = product, across every calendar year between the base year
## and Y, of (1 + that year's own period rate/100)) - no formula re-
## evaluation, no spikes, each step is just "X% more than last year":
##  - 1994-2003: 2.0%/year - slower gear/electronics turnover, ASSUMPTION.
##  - 2004-2013: 3.0%/year - GPS/plotters/sonar becoming mainstream,
##    ASSUMPTION.
##  - 2014-2023: 4.5%/year - the one period-rate actually tied to a
##    source: Palomares & Pauly's own C% = 13.8 x Y^-0.511 evaluated ONCE
##    at Y = 9 (2014-2023's own full length, the FDI-trusted window this
##    pipeline actually corrects) = 4.49%/year.
## All three sit inside the realistic 0-5%/year range - change
## TECH_CREEP_PERIOD_RATES directly for different breakpoints/rates.
TECH_CREEP_PERIOD_RATES <- data.table(
  period_start = c(1994, 2004, 2014),
  period_end   = c(2003, 2013, 2023),
  annual_pct   = c(2.0, 3.0, 4.5)
)

## Looks up which period a calendar year falls in and returns that
## period's flat annual %; a year outside every listed period's range
## (e.g. END_YEAR extended past 2023) uses the nearest period's rate.
tech_creep_rate_for_year <- function(yr) {
  hit <- TECH_CREEP_PERIOD_RATES[yr >= period_start & yr <= period_end]  # find the period this year falls in
  if (nrow(hit) > 0) return(hit$annual_pct[1])  # return that period's flat annual rate
  mids <- (TECH_CREEP_PERIOD_RATES$period_start + TECH_CREEP_PERIOD_RATES$period_end) / 2  # midpoint year of each period
  TECH_CREEP_PERIOD_RATES$annual_pct[which.min(abs(yr - mids))]  # use the rate of whichever period's midpoint is closest
}

## Cumulative effective multiplier for a Country x Year, built by walking
## year by year from TECH_CREEP_BASE_YEAR to Year (forward or backward),
## multiplying in (1 + that calendar year's own period rate x the
## country's creep_multiplier / 100) at each step - a plain proportional
## year-by-year increase, not a formula re-evaluated at each distance.
## Years before the base year divide instead of multiply at each step
## (older effort discounted relative to the base year's technology).
tech_creep_multiplier <- function(country, year, base_year = TECH_CREEP_BASE_YEAR) {
  mult_lookup <- setNames(TECH_CREEP_COUNTRY_MULTIPLIER$creep_multiplier, TECH_CREEP_COUNTRY_MULTIPLIER$Country)  # country -> creep multiplier lookup
  vapply(seq_along(year), function(i) {
    cc <- country[i]; yr <- year[i]
    if (is.na(cc) || is.na(yr)) return(NA_real_)
    cm <- if (cc %in% names(mult_lookup)) mult_lookup[[cc]] else 1  # this country's creep multiplier, default 1
    if (yr == base_year) return(1)  # no correction needed at the base year itself
    step <- if (yr > base_year) 1L else -1L  # walk forward or backward from base_year
    mult <- 1
    for (y in seq(base_year + step, yr, by = step)) {
      r <- tech_creep_rate_for_year(y) * cm / 100  # this calendar year's country-adjusted creep rate
      mult <- if (step > 0) mult * (1 + r) else mult / (1 + r)  # compound forward, or discount backward
    }
    mult
  }, numeric(1))
}

STECF_COUNTRY_CODES <- c(ESP = "Spain", FRA = "France", ITA = "Italy",
                         Spain = "Spain", France = "France", Italy = "Italy")  # FDI's country codes/names mapped to this script's country names

## Strips stray whitespace from column names - FDI's own "by country"
## effort file has names like "total _GT_days_at_sea" (space where the
## EU-aggregate file's equivalent, "total_GT_days_at_sea", has none).
## Removing all whitespace makes both files' column names match.
clean_fdi_names <- function(dt) { setnames(dt, gsub("\\s+", "", names(dt))); dt }  # strip whitespace from column names

## The full FAO/DCF gear-code -> gear-group crosswalk (confirmed against
## the DCF's own reference list, September 2026 - every code FDI's
## `metier`/`gear_type` fields can carry, not just the handful this
## pipeline had fine-grained fleet names for). Used as the FALLBACK below
## when a gear code isn't one of FLEET_REGISTER's own named fleets for
## that country (e.g. gillnets, traps, dredges, seine nets, misc gear) -
## so those rows get a real broad gear-group label instead of being
## dumped into "Unclassified" just because no country-specific fleet
## name exists for them.
DCF_GEAR_CODE_TO_GROUP <- data.table(
  gear_code = c(
    "DRB", "DRH", "DRM",
    "FAR", "FIX", "FPN", "FPO", "FSN", "FWR", "FYK",
    "GEN", "GNC", "GND", "GNF", "GNS", "GTN", "GTR", "GN",
    "HAR",
    "LA",
    "LHM", "LHP", "LL", "LLD", "LLS", "LTL", "LVT", "LX",
    "MDR", "MDV", "MEL", "MHI", "MIS", "MPM", "MPN", "MSP",
    "NK",
    "OTB", "OTM", "OTP", "OTT",
    "PS", "PS1",
    "PTB", "PTM",
    "SB", "SUX", "SV", "SX",
    "TB", "TBB", "TM", "TX", "TBS"
  ),
  gear_group = c(
    "Dredges", "Dredges", "Dredges",
    "Traps", "Traps", "Traps", "Traps", "Traps", "Traps", "Traps",
    "Gillnets and entangling nets", "Gillnets and entangling nets", "Gillnets and entangling nets",
    "Gillnets and entangling nets", "Gillnets and entangling nets", "Gillnets and entangling nets",
    "Gillnets and entangling nets", "Gillnets and entangling nets",
    "Miscellaneous gear",
    "Surrounding nets",
    "Hooks and lines", "Hooks and lines", "Hooks and lines", "Hooks and lines", "Hooks and lines",
    "Hooks and lines", "Hooks and lines", "Hooks and lines",
    "Miscellaneous gear", "Miscellaneous gear", "Miscellaneous gear", "Miscellaneous gear",
    "Miscellaneous gear", "Miscellaneous gear", "Miscellaneous gear", "Miscellaneous gear",
    "Gear Not Known or Not Specified",
    "Trawls", "Trawls", "Trawls", "Trawls",
    "Surrounding nets", "Surrounding nets",
    "Trawls", "Trawls",
    "Surrounding nets", "Surrounding nets", "Seine nets", "Seine nets",
    "Trawls", "Trawls", "Trawls", "Trawls", "Trawls"
  )
)

## FAO/DCF gear_type code -> this script's FleetType, per country - one
## row per gear code FLEET_REGISTER actually names a distinct fleet for
## in that country (expanded September 2026 against the DCF's own gear-
## code list, not just the handful of codes first spot-checked). Used
## both directly on gear_type AND as the primary lookup for the gear
## code extracted from the front of a `metier` string (metiers are
## coded "<GEAR>_<TARGET>_<MESH>_..." e.g. "OTB_DEF_>=70_0_0" - see
## metier handling below). Vessel-length-based "Artisanal" (small-
## scale, <12m) is applied AFTER this map, same as the EU's own
## small-scale fleet definition - see STECF_VESSEL_LENGTH_ARTISANAL -
## so this table only needs the named industrial/targeted gears. Gear
## codes not covered here fall through to DCF_GEAR_CODE_TO_GROUP's broad
## gear-group name (see resolve_stecf_fleettype() below), not straight to
## "Unclassified". Codes deliberately left OUT here, by design:
##  - LL ("Longlines nei") for France/Spain/Italy - genuinely ambiguous
##    between Drifting/Set longlines, handled as an even split by
##    resolve_stecf_fleettype() (see STECF_AMBIGUOUS_LONGLINE_SPLIT).
##  - LX/LHM/LHP/LTL/LVT (handlines/trolling/vertical lines - not
##    longline gear at all) for Spain/Italy, which have no catch-all
##    "hooks and lines" fleet name the way France does - these fall to
##    the DCF group fallback ("Hooks and lines") rather than being
##    force-fit into a Drifting/Set longline bucket they don't belong in.
##  - TBB (beam trawls)/OTM/TM/PTM (midwater trawls) for Italy - the
##    register's own comment says NOT to fold these into "Bottom trawls".
STECF_GEAR_TO_FLEETTYPE <- data.table(
  Country   = c(
    rep("France", 6 + 6 + 6), rep("Spain", 2 + 11 + 2), rep("Italy", 2 + 7 + 2),
    rep("Morocco", 2 + 8 + 10), rep("Algeria", 2 + 8 + 3 + 8), rep("Tunisia", 2 + 8 + 10)
  ),
  gear_type = c(
    ## France: Purse seiners, Midwater trawls, Bottom trawls, Artisanal,
    ## Drifting longlines, Hooks and line not drifting lines
    "PS", "PS1", "OTM", "TM", "PTM", "OTB",
    "OTP", "OTT", "TB", "PTB", "TBS", "LLD",
    "LLS", "LHM", "LHP", "LTL", "LVT", "LX",
    ## Spain: Purse seiners, Bottom trawls, Artisanal, Drifting longlines,
    ## Set longlines - Spain's register doesn't split trawl subtypes, so
    ## every trawl-family code lumps into "Bottom trawls".
    "PS", "PS1",
    "OTB", "OTM", "OTP", "OTT", "TB", "TBB", "TM", "TX", "PTB", "PTM", "TBS",
    "LLD", "LLS",
    ## Italy: Purse seiners, Bottom trawls (excludes beam/midwater - see
    ## FLEET_REGISTER's own comment), Artisanal, Drifting longlines, Set
    ## longlines.
    "PS", "PS1",
    "OTB", "OTP", "OTT", "TB", "TX", "PTB", "TBS",
    "LLD", "LLS",
    ## Morocco: Artisanal, Longlines (nei), Purse seiners, Trawls -n.e.i-
    ## (register has no trawl-subtype split, so every trawl code lumps in).
    "PS", "PS1",
    "LL", "LLD", "LLS", "LX", "LHM", "LHP", "LTL", "LVT",
    "OTB", "OTM", "OTP", "OTT", "TB", "TBB", "TM", "TX", "PTB", "TBS",
    ## Algeria: Artisanal, Longlines, Purse seiners, Midwater trawls (nei),
    ## Bottom trawls.
    "PS", "PS1",
    "LL", "LLD", "LLS", "LX", "LHM", "LHP", "LTL", "LVT",
    "OTM", "TM", "PTM",
    "OTB", "OTP", "OTT", "TB", "TBB", "TX", "PTB", "TBS",
    ## Tunisia: Artisanal, Longlines, Purse seiners, Trawls -n.e.i-
    "PS", "PS1",
    "LL", "LLD", "LLS", "LX", "LHM", "LHP", "LTL", "LVT",
    "OTB", "OTM", "OTP", "OTT", "TB", "TBB", "TM", "TX", "PTB", "TBS"
  ),
  FleetType = c(
    rep("Purse seiners", 2), "Midwater trawls", "Midwater trawls", "Midwater trawls", "Bottom trawls",
    rep("Bottom trawls", 5),
    "Drifting longlines", rep("Hooks and line not drifting lines", 6),
    rep("Purse seiners", 2), rep("Bottom trawls", 11),
    "Drifting longlines", "Set longlines",
    rep("Purse seiners", 2), rep("Bottom trawls", 7),
    "Drifting longlines", "Set longlines",
    rep("Purse seiners", 2), rep("Longlines", 8), rep("Trawls -n.e.i-", 10),
    rep("Purse seiners", 2), rep("Longlines", 8), rep("Midwater trawls", 3), rep("Bottom trawls", 8),
    rep("Purse seiners", 2), rep("Longlines", 8), rep("Trawls -n.e.i-", 10)
  )
)

## LL ("Longlines nei") for France/Spain/Italy: their own real coverage
## split (Drifting vs Set/"not drifting") isn't distinguishable from FDI's
## metier/gear_type vocabulary alone. Split EVENLY across each country's
## two named longline fleets - same "can't tell, don't guess one" logic
## already used for SAU's ambiguous gear elsewhere in this script. Not
## needed for Morocco/Algeria/Tunisia, whose register has a single
## "Longlines" catch-all - see STECF_GEAR_TO_FLEETTYPE above.
STECF_AMBIGUOUS_HOOK_GEAR_CODES <- c("LL")  # gear code that's genuinely ambiguous between longline subtypes
STECF_AMBIGUOUS_LONGLINE_SPLIT <- data.table(
  Country = c("France",              "Spain",               "Italy"),
  fleet_a = c("Drifting longlines",  "Drifting longlines",  "Drifting longlines"),
  fleet_b = c("Hooks and line not drifting lines", "Set longlines", "Set longlines")
)

## Resolves FleetType for a data.table that already has Country/gear_code
## columns (both Catches and Effort use this - the two call it with their
## own respective numeric columns as split_cols, since the "split
## evenly" step below needs to halve whichever measure columns exist).
## Priority order: (1) STECF_GEAR_TO_FLEETTYPE's per-country named-fleet
## mapping; (2) for LL/France-Spain-Italy, an even split across the
## country's two longline fleets (duplicates the row); (3)
## DCF_GEAR_CODE_TO_GROUP's broad gear-group name, for any gear code
## that's real but isn't one of FLEET_REGISTER's own named fleets; (4)
## 'Unclassified' for a gear code not found in either table at all.
resolve_stecf_fleettype <- function(dt, split_cols) {
  dt <- merge(dt, STECF_GEAR_TO_FLEETTYPE, by.x = c("Country", "gear_code"), by.y = c("Country", "gear_type"), all.x = TRUE)  # (1) try the per-country named-fleet mapping
  
  is_ambiguous <- is.na(dt$FleetType) & dt$gear_code %in% STECF_AMBIGUOUS_HOOK_GEAR_CODES &
    dt$Country %in% STECF_AMBIGUOUS_LONGLINE_SPLIT$Country  # rows with the ambiguous longline code, unmatched so far
  if (any(is_ambiguous)) {
    ambiguous_rows <- dt[is_ambiguous]  # pull out the ambiguous rows
    dt <- dt[!is_ambiguous]  # remove them from the main table for now
    split_parts <- unlist(lapply(seq_len(nrow(STECF_AMBIGUOUS_LONGLINE_SPLIT)), function(i) {
      cc <- STECF_AMBIGUOUS_LONGLINE_SPLIT$Country[i]
      rows_cc <- ambiguous_rows[Country == cc]
      if (nrow(rows_cc) == 0) return(NULL)
      lapply(c(STECF_AMBIGUOUS_LONGLINE_SPLIT$fleet_a[i], STECF_AMBIGUOUS_LONGLINE_SPLIT$fleet_b[i]), function(ft) {
        out <- copy(rows_cc)
        for (col in intersect(split_cols, names(out))) out[[col]] <- out[[col]] / 2  # halve each measure column
        out[, FleetType := ft]  # assign this half to one of the two longline fleets
        out
      })
    }), recursive = FALSE)
    split_parts <- split_parts[!vapply(split_parts, is.null, logical(1))]  # drop empty entries
    dt <- rbindlist(c(list(dt), split_parts), use.names = TRUE, fill = TRUE)  # (2) add the evenly-split longline rows back in
    message("[STECF FDI] ", nrow(ambiguous_rows), " row(s) with an ambiguous 'Longlines (nei)' gear code",
            " split evenly across that country's Drifting/Set (or Drifting/not-drifting) longline fleet -",
            " FDI's own vocabulary doesn't say which type.")
  }
  
  n_before_group_fallback <- sum(is.na(dt$FleetType))  # rows still unmatched
  if (n_before_group_fallback > 0) {
    dt <- merge(dt, DCF_GEAR_CODE_TO_GROUP, by = "gear_code", all.x = TRUE)  # (3) fall back to the broad DCF gear-group name
    dt[is.na(FleetType) & !is.na(gear_group), FleetType := gear_group]  # fill FleetType from the gear group where possible
    n_group_fallback <- n_before_group_fallback - sum(is.na(dt$FleetType))
    if (n_group_fallback > 0) {
      message("[STECF FDI] ", n_group_fallback, " row(s) used the broad DCF gear-group name as FleetType",
              " (real gear code, just not one of FLEET_REGISTER's own named fleets for that country): ",
              paste(sort(unique(dt[!is.na(gear_group)]$gear_code)), collapse = ", "))
    }
    dt[, gear_group := NULL]  # drop the helper column
  }
  n_unclassified <- sum(is.na(dt$FleetType))  # rows still unmatched after every fallback
  if (n_unclassified > 0) {
    message("[STECF FDI] ", n_unclassified, " row(s) with gear code(s) not found in STECF_GEAR_TO_FLEETTYPE",
            " or DCF_GEAR_CODE_TO_GROUP (kept as 'Unclassified' unless overridden by vessel_length below): ",
            paste(sort(unique(dt[is.na(FleetType)]$gear_code)), collapse = ", "))
    dt[is.na(FleetType), FleetType := "Unclassified"]  # (4) last resort - label the rest Unclassified
  }
  dt
}
STECF_VESSEL_LENGTH_ARTISANAL <- c("VL0006", "VL0612")  # <12m - EU small-scale fleet convention (real FDI length classes are VL0006/VL0612/VL1218/VL1824/VL2440/VL40XX - confirmed at runtime, not VL0610/VL1012)

## Capacity file has NO gear/metier field - only a broader "fishing_tech"
## category (standard DCF/FDI technology groups: DTS demersal trawlers/
## seiners, TBB beam trawlers, PMP pelagic trawlers/purse seiners, HOK
## hook gears, DFN drift/fixed netters, FPO pots/traps, DRB dredgers, PGP/
## PGO polyvalent). THIS MAPPING IS A BEST GUESS from the standard DCF code
## list, NOT confirmed against Andrea's real fishing_tech values the way
## STECF_GEAR_TO_FLEETTYPE was - the Capacity block below prints every
## fishing_tech value it actually finds; check that against this table
## before trusting the FleetType assignment, same caution as everywhere
## else real FDI values were guessed wrong on the first pass.
FISHING_TECH_TO_FLEETTYPE <- data.table(
  Country   = c("France", "France", "France",
                "Spain",  "Spain",  "Spain",
                "Italy",  "Italy",  "Italy"),
  fishing_tech = c("PMP", "DTS", "HOK",
                   "PMP", "DTS", "HOK",
                   "PMP", "DTS", "HOK"),
  FleetType = c("Purse seiners", "Bottom trawls", "Drifting longlines",
                "Purse seiners", "Bottom trawls", "Drifting longlines",
                "Purse seiners", "Bottom trawls", "Drifting longlines")
)

stecf_fdi_catch_by_gsa <- data.table()  # placeholder, filled below if FDI data is available
stecf_fleet_prop_by_year <- data.table()  # placeholder, filled below if FDI data is available
stecf_discard_ratio <- data.table()  # placeholder, filled below if FDI data is available
stecf_discard_ratio_by_fleet <- data.table()  # placeholder, filled below if FDI data is available
stecf_fdi_effort_by_gsa <- data.table()  # placeholder, filled below if FDI data is available

stecf_catches_dir <- file.path(STECF_FDI_DIR, "Catches")
if (!dir.exists(stecf_catches_dir)) {
  message("\n[STECF FDI] Catches folder not found at '", stecf_catches_dir, "' - fleet_prop_final below",
          " falls back to SAU/default for Spain/France/Italy too (see comment above this block for how",
          " to get the file; STECF_FDI_DIR should point at the unzipped download's root).")
} else if (cfg$format != "gfcm_regional") {
  message("\n[STECF FDI] DATASET_VERSION = '", DATASET_VERSION, "' - the species-code crosswalk needs",
          " GFCM's own CL_FI_SPECIES_GROUPS.csv (3-alpha codes), only available for 'GFCM_2025'. Skipped.")
} else {
  catch_files <- list.files(stecf_catches_dir, pattern = "^FDI Catches by country[0-9]{4}\\.csv$", full.names = TRUE)  # find one file per year
  if (length(catch_files) == 0) {
    message("\n[STECF FDI] No 'FDI Catches by country<YEAR>.csv' files found in '", stecf_catches_dir, "'.")
  } else {
    stecf_raw <- clean_fdi_names(rbindlist(lapply(catch_files, fread), use.names = TRUE, fill = TRUE))  # read and stack every year's Catches file
    message("\n[STECF FDI] ", nrow(stecf_raw), " row(s) loaded from ", length(catch_files), " Catches file(s).")
    ## Coerce to numeric IMMEDIATELY after load - fread() reads these as
    ## character whenever any row in any year's file carries a non-
    ## numeric value (e.g. a "confidential" flag in place of a figure),
    ## and every sum()/filter below on these two columns needs them
    ## numeric, not just the discard-ratio step further down.
    if ("total_live_weight_landed" %in% names(stecf_raw)) {
      stecf_raw[, total_live_weight_landed := suppressWarnings(as.numeric(total_live_weight_landed))]  # force landings to numeric
    }
    if ("tot_discards_tonnes" %in% names(stecf_raw)) {
      stecf_raw[, tot_discards_tonnes := suppressWarnings(as.numeric(tot_discards_tonnes))]  # force discards to numeric
    }
    
    required_cols <- c("country", "year", "gear_type", "sub_region", "species", "total_live_weight_landed", "tot_discards_tonnes")
    missing_cols <- setdiff(required_cols, names(stecf_raw))  # any required columns not present
    if (length(missing_cols) > 0) {
      message("\n[STECF FDI] Missing expected column(s): ", paste(missing_cols, collapse = ", "), " - got: ",
              paste(names(stecf_raw), collapse = ", "), ". Skipped.")
    } else {
      other_countries <- setdiff(unique(stecf_raw$country), names(STECF_COUNTRY_CODES))  # country codes present but not in the expected 3
      if (length(other_countries) > 0) {
        message("[STECF FDI] country value(s) present but not Spain/France/Italy (excluded, not silently -",
                " likely other EU states also fishing these waters): ", paste(sort(other_countries), collapse = ", "))
      }
      stecf_raw[, Country := STECF_COUNTRY_CODES[country]]  # translate country code to country name
      stecf_raw <- stecf_raw[!is.na(Country) & Country %in% c("Spain", "France", "Italy")]  # keep only the 3 FDI-reporting target countries
      stecf_raw[, year := suppressWarnings(as.integer(year))]  # coerce year to integer
      stecf_raw <- stecf_raw[!is.na(year) & year >= STECF_FDI_START_YEAR & year <= END_YEAR]  # restrict to FDI's trusted coverage window
      
      if ("supra_region" %in% names(stecf_raw)) {
        message("[STECF FDI] supra_region value(s) found: ", paste(sort(unique(stecf_raw$supra_region)), collapse = ", "))
        if ("MBS" %in% unique(stecf_raw$supra_region)) {
          stecf_raw <- stecf_raw[supra_region == "MBS"]  # keep only Mediterranean & Black Sea rows
          message("[STECF FDI] Filtered to supra_region == 'MBS' (Mediterranean & Black Sea).")
        } else {
          message("[STECF FDI] 'MBS' not found in supra_region - NOT filtering by it (check the value(s)",
                  " above and adjust this block if the Mediterranean/Black Sea code is spelled differently).")
        }
      }
      message("[STECF FDI] ", nrow(stecf_raw), " row(s) after country/year(>=", STECF_FDI_START_YEAR,
              ")/region filter. sub_region (GSA) value(s) found: ", paste(sort(unique(stecf_raw$sub_region)), collapse = ", "))
      
      ## FleetType, preferring `metier` (finer: gear + target assemblage +
      ## mesh) over the bare `gear_type` - "distribute to fishing fleets
      ## based on metiers" per request. The metier's own leading gear-code
      ## token drives FleetType (via STECF_GEAR_TO_FLEETTYPE, a gear
      ## taxonomy) - metier itself stays as its own column throughout so
      ## the finer mesh/target distinction is visible, not collapsed away.
      stecf_raw[, Metier := metier]  # keep the finer metier string in its own column
      stecf_raw[, gear_code := str_extract(Metier, "^[A-Za-z]+")]  # extract the leading gear-code token from the metier
      stecf_raw[is.na(gear_code) | gear_code == "", gear_code := gear_type]  # fall back to the bare gear_type if metier had no gear code
      
      stecf_raw <- resolve_stecf_fleettype(stecf_raw, split_cols = c("total_live_weight_landed", "tot_discards_tonnes"))  # assign FleetType via the gear-code cascade
      message("[STECF FDI] vessel_length value(s) found: ", paste(sort(unique(stecf_raw$vessel_length)), collapse = ", "))
      stecf_raw[vessel_length %in% STECF_VESSEL_LENGTH_ARTISANAL, FleetType := "Artisanal"]  # small vessels are always Artisanal, overriding the gear-based assignment
      unclassified_share_stecf <- round(100 * sum(stecf_raw[FleetType == "Unclassified"]$total_live_weight_landed, na.rm = TRUE) /
                                          sum(stecf_raw$total_live_weight_landed, na.rm = TRUE), 1)  # what share of landed weight is still Unclassified
      if (!is.na(unclassified_share_stecf) && unclassified_share_stecf > 0) {
        message("[STECF FDI] 'Unclassified' FleetType is ", unclassified_share_stecf, "% of matched STECF",
                " landed weight (same caveat as SAU's own Unclassified bucket - not renormalized away).")
      }
      
      ## Species (FAO 3-alpha code) -> FG, via the SAME reference file
      ## GFCM's own species matching used (CL_FI_SPECIES_GROUPS.csv) -
      ## FDI's species field uses the same FAO 3-alpha standard.
      species_code_ref <- safe_fread(file.path(cfg$data_dir, cfg$species_file), "species_file")[, .(SpeciesCode = `3A_Code`, Species = Name_En)]  # FAO 3-alpha code lookup
      species_code_to_fg <- unique(merge(species_code_ref, species_to_fg, by = "Species")[, .(SpeciesCode, FG_num, FG_name)])  # build a 3-alpha code -> FG lookup
      stecf_raw <- merge(stecf_raw, species_code_to_fg, by.x = "species", by.y = "SpeciesCode", all.x = TRUE)  # attach FG to each FDI row
      n_unmatched_sp <- sum(is.na(stecf_raw$FG_num))  # rows whose species code had no FG match
      if (n_unmatched_sp > 0) message("[STECF FDI] ", n_unmatched_sp, " row(s) with a species code not in",
                                      " CL_FI_SPECIES_GROUPS.csv's 3A_Code - dropped (no FG to assign).")
      stecf_raw <- stecf_raw[!is.na(FG_num)]  # drop rows with no FG match
      
      ## Discards: real columns, no flag-detection needed - landed
      ## weight and discards are separate numeric fields on the same row.
      stecf_discard_ratio <- stecf_raw[, .(Landed_t = sum(total_live_weight_landed, na.rm = TRUE),
                                           Discarded_t = sum(tot_discards_tonnes, na.rm = TRUE)),
                                       by = .(Country, FG_num, Year = year)]  # sum landed/discarded tonnes by country x FG x year
      stecf_discard_ratio[, discard_ratio := Discarded_t / (Landed_t + Discarded_t)]  # compute the discard ratio
      stecf_discard_ratio <- stecf_discard_ratio[is.finite(discard_ratio), .(Country, FG_num, Year, discard_ratio)]  # drop non-finite ratios
      message("[STECF FDI] stecf_discard_ratio: ", nrow(stecf_discard_ratio), " Country x FG x Year row(s) -",
              " preferred over FishMIP's Med-wide ratio for these cells (see the discards step below).")
      
      ## Same ratio, but resolved by FLEET too (Country x FG x FleetType
      ## x Year) - a trawl fleet and a longline fleet do not discard at
      ## the same rate, so this is what actually lets discard % vary by
      ## fleet over time instead of just inheriting the country-level
      ## ratio uniformly across every fleet (see the fleet-split step).
      stecf_discard_ratio_by_fleet <- stecf_raw[, .(Landed_t = sum(total_live_weight_landed, na.rm = TRUE),
                                                    Discarded_t = sum(tot_discards_tonnes, na.rm = TRUE)),
                                                by = .(Country, FG_num, FleetType, Year = year)]  # sum landed/discarded tonnes by country x FG x fleet x year
      stecf_discard_ratio_by_fleet[, discard_ratio := Discarded_t / (Landed_t + Discarded_t)]  # compute the fleet-level discard ratio
      stecf_discard_ratio_by_fleet <- stecf_discard_ratio_by_fleet[is.finite(discard_ratio),
                                                                   .(Country, FG_num, FleetType, Year, discard_ratio)]  # drop non-finite ratios
      message("[STECF FDI] stecf_discard_ratio_by_fleet: ", nrow(stecf_discard_ratio_by_fleet),
              " Country x FG x FleetType x Year row(s) - used to give each fleet its own discard rate",
              " instead of distributing the country-level total by catch share.")
      
      ## Genuine gear/metier+GSA-resolved LANDINGS - written out as its
      ## own sheet, a real cross-check/upgrade over the SAU-weighted
      ## split, at a finer (GSA, not just Country) grain than fleet_prop:
      stecf_fdi_catch_by_gsa <- stecf_raw[, .(Catch_t = sum(total_live_weight_landed, na.rm = TRUE),
                                              Discard_t = sum(tot_discards_tonnes, na.rm = TRUE)),
                                          by = .(Country, GSA = sub_region, FleetType, Metier, FG_num, FG_name, Year = year)]  # sum catch/discards by country x GSA x fleet x metier x FG x year
      setorder(stecf_fdi_catch_by_gsa, Country, GSA, FG_num, Year)  # sort for readability
      fwrite(stecf_fdi_catch_by_gsa, file.path(out_dir, "stecf_fdi_catch_by_gsa_gear_year.csv"))  # write result to CSV
      message("[STECF FDI] stecf_fdi_catch_by_gsa_gear_year.csv written - ", nrow(stecf_fdi_catch_by_gsa),
              " Country x GSA x FleetType x Metier x FG x Year row(s).")
      
      ## Fleet shares, by YEAR (not collapsed across FDI's whole 2014-
      ## 2024 window) - lets fleet_prop_final apply FDI's own per-year
      ## split for 2014+ instead of one static average. GSA/Sector/
      ## Comment are pulled from FLEET_REGISTER (fleet_types_ref, built
      ## above) purely so this table's shape matches fleet_prop's for
      ## the rbindlist below - STECF_FDI_Catch_by_GSA above is where the
      ## REAL per-GSA breakdown lives; this table stays at Country grain.
      stecf_fleet_prop_by_year <- stecf_raw[, .(Catch_t = sum(total_live_weight_landed, na.rm = TRUE)),
                                            by = .(Country, FG_num, FleetType, Year = year)]  # sum landed catch by country x FG x fleet x year
      stecf_fleet_prop_by_year[, prop_fleet := Catch_t / sum(Catch_t), by = .(Country, FG_num, Year)]  # convert to a fleet share within each country x FG x year
      stecf_fleet_prop_by_year <- merge(stecf_fleet_prop_by_year, fleet_types_ref, by = c("Country", "FleetType"), all.x = TRUE)  # attach GSA/Sector/Comment metadata
      stecf_fleet_prop_by_year[is.na(GSA), GSA := "(STECF-derived - see STECF_FDI_Catch_by_GSA for the real GSA breakdown)"]  # fill missing GSA with a note
      stecf_fleet_prop_by_year[is.na(Sector), Sector := fifelse(FleetType == "Artisanal", "Artisanal", "Industrial")]  # derive missing Sector from FleetType
      stecf_fleet_prop_by_year[, fleet_split_source := paste0("STECF FDI (real gear/metier x GSA landings, year ", Year, ")")]  # tag the source of this share
      stecf_fleet_prop_by_year <- stecf_fleet_prop_by_year[, .(Country, FG_num, Year, Sector, FleetType, GSA, Comment, prop_fleet, fleet_split_source)]  # reorder/select final columns
      message("[STECF FDI] stecf_fleet_prop_by_year: ", nrow(stecf_fleet_prop_by_year), " Country x FG x",
              " FleetType x Year share row(s), ", uniqueN(stecf_fleet_prop_by_year$Year), " year(s) (",
              min(stecf_fleet_prop_by_year$Year), "-", max(stecf_fleet_prop_by_year$Year),
              ") - blended with SAU/hindcast below (fleet_prop_final).")
    }
  }
}

## --- Effort (days x capacity) - separate file, one row per Country x
## GSA x gear/metier x quarter x Year, ALL years already in one file
## (unlike Catches, which is split by year). -------------------------
stecf_effort_file <- file.path(STECF_FDI_DIR, "Effort", "FDI Effort by country.csv")
if (!file.exists(stecf_effort_file)) {
  message("\n[STECF FDI] Effort file not found at '", stecf_effort_file, "' - Fishing_Effort_by_Fleet",
          " below comes from FishMIP's nom_active for every year/country, as before.")
} else {
  stecf_effort_raw <- clean_fdi_names(safe_fread(stecf_effort_file, "stecf_effort_file"))  # load the FDI effort file
  stecf_effort_raw[, Country := STECF_COUNTRY_CODES[country]]  # translate country code to country name
  stecf_effort_raw <- stecf_effort_raw[!is.na(Country) & Country %in% c("Spain", "France", "Italy")]  # keep only the 3 target countries
  stecf_effort_raw[, year := suppressWarnings(as.integer(year))]  # coerce year to integer
  stecf_effort_raw <- stecf_effort_raw[!is.na(year) & year >= STECF_FDI_START_YEAR & year <= END_YEAR]  # restrict to FDI's trusted coverage window
  if ("supra_region" %in% names(stecf_effort_raw) && "MBS" %in% unique(stecf_effort_raw$supra_region)) {
    stecf_effort_raw <- stecf_effort_raw[supra_region == "MBS"]  # keep only Mediterranean & Black Sea rows
  }
  stecf_effort_raw[, Metier := metier]  # keep the finer metier string in its own column
  stecf_effort_raw[, gear_code := str_extract(Metier, "^[A-Za-z]+")]  # extract the leading gear-code token
  stecf_effort_raw[is.na(gear_code) | gear_code == "", gear_code := gear_type]  # fall back to bare gear_type
  stecf_effort_raw <- resolve_stecf_fleettype(stecf_effort_raw,
                                              split_cols = c("total_fishing_days", "total_days_at_sea", "total_kW_days_at_sea",
                                                             "total_GT_days_at_sea", "total_kW_fishing_days", "total_GT_fishing_days"))  # assign FleetType via the gear-code cascade
  stecf_effort_raw[vessel_length %in% STECF_VESSEL_LENGTH_ARTISANAL, FleetType := "Artisanal"]  # small vessels are always Artisanal
  
  effort_cols <- intersect(c("total_fishing_days", "total_days_at_sea", "total_kW_days_at_sea",
                             "total_GT_days_at_sea", "total_kW_fishing_days", "total_GT_fishing_days"),
                           names(stecf_effort_raw))  # which effort metrics are actually present
  if (length(effort_cols) == 0) {
    message("\n[STECF FDI] No recognized days/capacity effort column in '", stecf_effort_file,
            "' (got: ", paste(names(stecf_effort_raw), collapse = ", "), ") - Fishing_Effort_by_Fleet",
            " below still comes from FishMIP's nom_active.")
  } else {
    for (cc in effort_cols) stecf_effort_raw[[cc]] <- suppressWarnings(as.numeric(stecf_effort_raw[[cc]]))  # coerce effort columns to numeric
    stecf_fdi_effort_by_gsa <- stecf_effort_raw[, c("Country", "sub_region", "FleetType", "Metier", "year", effort_cols), with = FALSE]  # keep only needed columns
    setnames(stecf_fdi_effort_by_gsa, c("sub_region", "year"), c("GSA", "Year"))  # standardize column names
    setnames(stecf_fdi_effort_by_gsa, effort_cols, paste0("Effort_", effort_cols))  # prefix effort metric names
    stecf_fdi_effort_by_gsa <- stecf_fdi_effort_by_gsa[, lapply(.SD, sum, na.rm = TRUE),
                                                       by = .(Country, GSA, FleetType, Metier, Year),
                                                       .SDcols = paste0("Effort_", effort_cols)]  # sum effort metrics by country x GSA x fleet x metier x year
    setorder(stecf_fdi_effort_by_gsa, Country, GSA, FleetType, Year)  # sort for readability
    fwrite(stecf_fdi_effort_by_gsa, file.path(out_dir, "stecf_fdi_effort_by_gsa_gear_year.csv"))  # write result to CSV
    message("\n[STECF FDI] stecf_fdi_effort_by_gsa_gear_year.csv written - ", nrow(stecf_fdi_effort_by_gsa),
            " Country x GSA x FleetType x Metier x Year row(s), effort metric(s): ",
            paste(paste0("Effort_", effort_cols), collapse = ", "), ". Note: total_kW_days_at_sea/",
            "total_GT_days_at_sea/total_kW_fishing_days/total_GT_fishing_days ARE already capacity-weighted",
            " effort (days x the fleet's own engine power/tonnage) - that's a genuine days x capacity",
            " figure straight from FDI, not something this script has to construct. The separate Capacity",
            " file (vessel counts/GT/kW by fishing_tech, no gear/metier) is loaded next.")
  }
}

## --- Capacity (vessel counts/GT/kW/trips/age/length by broad fishing_tech,
## no gear/metier/GSA field at all) - a genuinely different thing from the
## capacity-WEIGHTED EFFORT above (kW-days/GT-days, which already exist in
## the Effort file). This file gives fleet SIZE: how many vessels, how big,
## how old - used here to add average-per-vessel figures (avg_days_per_vessel,
## avg_kW_per_vessel, avg_GT_per_vessel) alongside the real effort metrics,
## NOT multiplied into Effort_total_fishing_days directly - that total is
## already summed across every vessel in the fleet, so Effort_days x
## Capacity_vessel_count would double-count every vessel's own days. -------
stecf_fdi_capacity_by_fleet <- data.table()
stecf_capacity_file <- file.path(STECF_FDI_DIR, "Capacity", "FDI Capacity by country.csv")
if (!file.exists(stecf_capacity_file)) {
  message("\n[STECF FDI] Capacity file not found at '", stecf_capacity_file, "' - no vessel-count/GT/kW",
          " figures or average-per-vessel effort ratios will be available.")
} else {
  stecf_capacity_raw <- clean_fdi_names(safe_fread(stecf_capacity_file, "stecf_capacity_file"))  # load the FDI capacity file
  stecf_capacity_raw[, Country := STECF_COUNTRY_CODES[country]]  # translate country code to country name
  stecf_capacity_raw <- stecf_capacity_raw[!is.na(Country) & Country %in% c("Spain", "France", "Italy")]  # keep only the 3 target countries
  stecf_capacity_raw[, year := suppressWarnings(as.integer(year))]  # coerce year to integer
  stecf_capacity_raw <- stecf_capacity_raw[!is.na(year) & year >= STECF_FDI_START_YEAR & year <= END_YEAR]  # restrict to FDI's trusted coverage window
  if ("supra_region" %in% names(stecf_capacity_raw) && "MBS" %in% unique(stecf_capacity_raw$supra_region)) {
    stecf_capacity_raw <- stecf_capacity_raw[supra_region == "MBS"]  # keep only Mediterranean & Black Sea rows
  }
  if ("fishing_tech" %in% names(stecf_capacity_raw)) {
    message("[STECF FDI] Capacity fishing_tech value(s) found: ", paste(sort(unique(stecf_capacity_raw$fishing_tech)), collapse = ", "))
    stecf_capacity_raw <- merge(stecf_capacity_raw, FISHING_TECH_TO_FLEETTYPE, by.x = c("Country", "fishing_tech"), by.y = c("Country", "fishing_tech"), all.x = TRUE)  # map fishing_tech to FleetType
    unmatched_tech <- unique(stecf_capacity_raw[is.na(FleetType)]$fishing_tech)  # tech codes with no mapping
    if (length(unmatched_tech) > 0) {
      message("[STECF FDI] fishing_tech code(s) not in FISHING_TECH_TO_FLEETTYPE (kept as 'Unclassified'",
              " unless overridden by vessel_length below - THIS MAPPING IS A BEST GUESS, not confirmed",
              " against a real DCF code list; check these against the values printed above): ",
              paste(unmatched_tech, collapse = ", "))
      stecf_capacity_raw[is.na(FleetType), FleetType := "Unclassified"]  # last resort for unmapped tech codes
    }
  } else {
    stecf_capacity_raw[, FleetType := "Unclassified"]  # no fishing_tech column at all - everything Unclassified
    message("[STECF FDI] No fishing_tech column in the Capacity file (got: ", paste(names(stecf_capacity_raw), collapse = ", "), ").")
  }
  if ("vessel_length" %in% names(stecf_capacity_raw)) {
    stecf_capacity_raw[vessel_length %in% STECF_VESSEL_LENGTH_ARTISANAL, FleetType := "Artisanal"]  # small vessels are always Artisanal
  }
  
  capacity_cols <- intersect(c("total_trips", "total_kW", "total_GT", "total_vessels",
                               "average_age", "average_length", "max_sea_days"), names(stecf_capacity_raw))  # which capacity metrics are present
  if (length(capacity_cols) == 0) {
    message("\n[STECF FDI] No recognized capacity column in '", stecf_capacity_file, "' (got: ",
            paste(names(stecf_capacity_raw), collapse = ", "), ") - skipped.")
  } else {
    for (cc in capacity_cols) stecf_capacity_raw[[cc]] <- suppressWarnings(as.numeric(stecf_capacity_raw[[cc]]))  # coerce capacity columns to numeric
    sum_cols <- intersect(c("total_trips", "total_kW", "total_GT", "total_vessels"), capacity_cols)  # metrics to sum
    mean_cols <- intersect(c("average_age", "average_length", "max_sea_days"), capacity_cols)  # metrics to average
    agg_list <- c(
      if (length(sum_cols) > 0) list(stecf_capacity_raw[, lapply(.SD, sum, na.rm = TRUE), by = .(Country, FleetType, Year = year), .SDcols = sum_cols]) else NULL,  # sum-type metrics by country x fleet x year
      if (length(mean_cols) > 0) list(stecf_capacity_raw[, lapply(.SD, mean, na.rm = TRUE), by = .(Country, FleetType, Year = year), .SDcols = mean_cols]) else NULL  # mean-type metrics by country x fleet x year
    )
    stecf_fdi_capacity_by_fleet <- Reduce(function(a, b) merge(a, b, by = c("Country", "FleetType", "Year"), all = TRUE), agg_list)  # merge sum and mean tables together
    setnames(stecf_fdi_capacity_by_fleet, capacity_cols, paste0("Capacity_", capacity_cols))  # prefix capacity metric names
    setorder(stecf_fdi_capacity_by_fleet, Country, FleetType, Year)  # sort for readability
    fwrite(stecf_fdi_capacity_by_fleet, file.path(out_dir, "stecf_fdi_capacity_by_fleet_year.csv"))  # write result to CSV
    message("\n[STECF FDI] stecf_fdi_capacity_by_fleet_year.csv written - ", nrow(stecf_fdi_capacity_by_fleet),
            " Country x FleetType x Year row(s), capacity metric(s): ", paste(paste0("Capacity_", capacity_cols), collapse = ", "),
            ". No GSA/metier resolution in this file (broader fishing_tech categories only).")
  }
}

## =================================================================
## HINDCAST - the years before STECF FDI's coverage window (1994 to
## STECF_FDI_START_YEAR - 1) aren't just handed SAU's flat, all-years
## average share (fleet_prop). Per request: use SAU's own REAL year-
## by-year proportions (sau_fleet_prop_by_year, built above), and for
## Spain/France/Italy, bias-correct them using the years where SAU and
## STECF FDI both exist (2014+) - i.e. calibrate SAU against FDI's
## ground truth during the overlap, then apply that same correction
## backward into the years FDI doesn't reach at all. Morocco/Algeria/
## Tunisia have no FDI to calibrate against, so they keep SAU's own
## year-by-year shares as-is (still real annual variation, not a flat
## average - just uncalibrated).
## =================================================================
fleet_calibration <- data.table()
if (nrow(stecf_fleet_prop_by_year) > 0 && nrow(sau_fleet_prop_by_year) > 0) {
  overlap_years <- sort(intersect(unique(stecf_fleet_prop_by_year$Year), unique(sau_fleet_prop_by_year$Year)))  # years where both STECF FDI and SAU have data
  if (length(overlap_years) > 0) {
    stecf_overlap <- stecf_fleet_prop_by_year[Year %in% overlap_years, .(prop_fleet_stecf = mean(prop_fleet, na.rm = TRUE)),
                                              by = .(Country, FleetType)]  # average FDI's fleet share over the overlap years
    sau_overlap <- sau_fleet_prop_by_year[Year %in% overlap_years & Country %in% c("Spain", "France", "Italy"),
                                          .(prop_fleet_sau = mean(prop_fleet, na.rm = TRUE)), by = .(Country, FleetType)]  # average SAU's fleet share over the overlap years
    fleet_calibration <- merge(stecf_overlap, sau_overlap, by = c("Country", "FleetType"))  # pair up FDI vs SAU shares
    fleet_calibration[, calibration_factor := fifelse(prop_fleet_sau > 0, prop_fleet_stecf / prop_fleet_sau, NA_real_)]  # ratio of FDI's share to SAU's share
    fleet_calibration <- fleet_calibration[is.finite(calibration_factor), .(Country, FleetType, calibration_factor)]  # drop non-finite factors
    fwrite(fleet_calibration, file.path(out_dir, "sau_stecf_fleet_calibration_factors.csv"))  # write result to CSV
    message("\n[Hindcast] SAU-vs-STECF FDI calibration factors, overlap years ", min(overlap_years), "-",
            max(overlap_years), ": ", nrow(fleet_calibration), " Country x FleetType factor(s) - written to",
            " sau_stecf_fleet_calibration_factors.csv, applied below to SAU's pre-", STECF_FDI_START_YEAR,
            " proportions for Spain/France/Italy. A factor > 1 means FDI shows that fleet catching MORE of",
            " an FG than SAU's own gear-catch weighting implies, and vice versa.")
  } else {
    message("\n[Hindcast] No years where both SAU and STECF FDI have data - pre-", STECF_FDI_START_YEAR,
            " proportions for Spain/France/Italy fall back to SAU's own year-by-year shares, uncalibrated.")
  }
} else {
  message("\n[Hindcast] STECF FDI and/or SAU's year-resolved fleet shares aren't available - the pre-",
          STECF_FDI_START_YEAR, " hindcast can't be calibrated; SAU's own year-by-year shares (or, failing",
          " that, fleet_prop's flat average) are used uncalibrated.")
}

sau_fleet_prop_by_year_hindcast <- copy(sau_fleet_prop_by_year)  # start from SAU's own real year-by-year shares
if (nrow(sau_fleet_prop_by_year_hindcast) > 0) {
  if (nrow(fleet_calibration) > 0) {
    sau_fleet_prop_by_year_hindcast <- merge(sau_fleet_prop_by_year_hindcast, fleet_calibration,
                                             by = c("Country", "FleetType"), all.x = TRUE)  # attach the FDI-vs-SAU calibration factor
    sau_fleet_prop_by_year_hindcast[, calibration_factor := fifelse(is.na(calibration_factor), 1, calibration_factor)]  # default to no correction where uncalibrated
  } else {
    sau_fleet_prop_by_year_hindcast[, calibration_factor := 1]  # no calibration data at all - apply no correction
  }
  sau_fleet_prop_by_year_hindcast[, prop_fleet := prop_fleet * calibration_factor]  # apply the bias correction
  sau_fleet_prop_by_year_hindcast[, prop_fleet := prop_fleet / sum(prop_fleet), by = .(Country, FG_num, Year)]  # renormalize back to sum to 1
  sau_fleet_prop_by_year_hindcast[, calibration_factor := NULL]  # drop the now-unneeded helper column
  sau_fleet_prop_by_year_hindcast[, fleet_split_source := fifelse(
    Country %in% fleet_calibration$Country,
    "SAU (own year, hindcast - bias-corrected against STECF FDI's overlap years)",
    "SAU (own year, hindcast - uncalibrated, no STECF FDI overlap for this country)"
  )]  # tag the source of this share
  n_calibrated_countries <- uniqueN(fleet_calibration$Country)
  message("[Hindcast] sau_fleet_prop_by_year_hindcast: ", nrow(sau_fleet_prop_by_year_hindcast), " row(s) across ",
          uniqueN(sau_fleet_prop_by_year_hindcast$Year), " year(s) (", n_calibrated_countries,
          " of the up-to-3 EU countries had a calibration factor to apply). Renormalized to sum to 1 within",
          " each Country x FG x Year after applying the calibration factor.")
}

## --- Same calibrate-against-FDI-overlap logic, applied to SAU's own
## discard ratio (sau_discard_ratio_by_year, built in the SAU block
## above from its catch_type field) - Spain/France/Italy get a real,
## bias-corrected discard-ratio hindcast for the pre-2014 years instead
## of falling straight to FishMIP's Med-wide ratio; Morocco/Algeria/
## Tunisia keep SAU's own ratio uncalibrated (no FDI to check it
## against). This is what lets the discards fallback below prefer SAU
## over FishMIP for every cell STECF FDI itself doesn't cover. ---------
discard_calibration <- data.table()
if (nrow(stecf_discard_ratio) > 0 && nrow(sau_discard_ratio_by_year) > 0) {
  overlap_years_d <- sort(intersect(unique(stecf_discard_ratio$Year), unique(sau_discard_ratio_by_year$Year)))
  if (length(overlap_years_d) > 0) {
    stecf_overlap_d <- stecf_discard_ratio[Year %in% overlap_years_d, .(ratio_stecf = mean(discard_ratio, na.rm = TRUE)),
                                           by = .(Country, FG_num)]
    sau_overlap_d <- sau_discard_ratio_by_year[Year %in% overlap_years_d & Country %in% c("Spain", "France", "Italy"),
                                               .(ratio_sau = mean(discard_ratio, na.rm = TRUE)), by = .(Country, FG_num)]  # average SAU's discard ratio over the overlap years
    discard_calibration <- merge(stecf_overlap_d, sau_overlap_d, by = c("Country", "FG_num"))  # pair up FDI vs SAU discard ratios
    discard_calibration[, calibration_factor := fifelse(ratio_sau > 0, ratio_stecf / ratio_sau, NA_real_)]  # ratio of FDI's discard rate to SAU's
    discard_calibration <- discard_calibration[is.finite(calibration_factor), .(Country, FG_num, calibration_factor)]  # drop non-finite factors
    message("\n[Hindcast] SAU-vs-STECF FDI discard-ratio calibration, overlap years ", min(overlap_years_d), "-",
            max(overlap_years_d), ": ", nrow(discard_calibration), " Country x FG factor(s), applied below to",
            " SAU's pre-", STECF_FDI_START_YEAR, " discard ratio for Spain/France/Italy.")
  }
}
sau_discard_ratio_by_year_hindcast <- copy(sau_discard_ratio_by_year)  # start from SAU's own real discard ratios
if (nrow(sau_discard_ratio_by_year_hindcast) > 0) {
  if (nrow(discard_calibration) > 0) {
    sau_discard_ratio_by_year_hindcast <- merge(sau_discard_ratio_by_year_hindcast, discard_calibration,
                                                by = c("Country", "FG_num"), all.x = TRUE)  # attach the FDI-vs-SAU calibration factor
    sau_discard_ratio_by_year_hindcast[, calibration_factor := fifelse(is.na(calibration_factor), 1, calibration_factor)]  # default to no correction where uncalibrated
  } else {
    sau_discard_ratio_by_year_hindcast[, calibration_factor := 1]  # no calibration data at all - apply no correction
  }
  sau_discard_ratio_by_year_hindcast[, discard_ratio := pmin(pmax(discard_ratio * calibration_factor, 0), 0.95)]  # apply the correction, clamped to a plausible range
  sau_discard_ratio_by_year_hindcast[, calibration_factor := NULL]  # drop the now-unneeded helper column
  message("[Hindcast] sau_discard_ratio_by_year_hindcast: ", nrow(sau_discard_ratio_by_year_hindcast),
          " Country x FG x Year row(s) - used below in place of FishMIP's Med-wide ratio wherever STECF",
          " FDI itself doesn't cover the cell.")
}

## --- Effort hindcast: an FDI-derived effort-per-tonne ratio (Country x
## FleetType, from the overlap years) applied to SAU's own hindcasted
## catch-by-fleet for Spain/France/Italy's pre-2014 years - "effort
## implied by how much this fleet caught, at this fleet's own real
## catch-to-effort ratio". This is deliberately Spain/France/Italy only:
## Morocco/Algeria/Tunisia have no FDI effort/catch ratio to build a
## calibration from, and SAU carries no effort variable of its own to
## hindcast with directly - FishMIP nom_active remains their effort
## source (see the effort step further down). --------------------------
stecf_effort_catch_ratio <- data.table()
if (nrow(stecf_fdi_effort_by_gsa) > 0 && nrow(stecf_fdi_catch_by_gsa) > 0 && "Effort_total_fishing_days" %in% names(stecf_fdi_effort_by_gsa)) {
  effort_cfy <- stecf_fdi_effort_by_gsa[, .(Effort_days = sum(Effort_total_fishing_days, na.rm = TRUE)),
                                        by = .(Country, FleetType, Year)]  # sum fishing days by country x fleet x year
  catch_cfy <- stecf_fdi_catch_by_gsa[, .(Catch_t = sum(Catch_t, na.rm = TRUE)), by = .(Country, FleetType, Year)]  # sum catch by country x fleet x year
  stecf_effort_catch_ratio <- merge(effort_cfy, catch_cfy, by = c("Country", "FleetType", "Year"))  # pair up effort and catch per cell
  stecf_effort_catch_ratio <- stecf_effort_catch_ratio[Catch_t > 0, .(days_per_tonne = mean(Effort_days / Catch_t, na.rm = TRUE)),
                                                       by = .(Country, FleetType)]  # average days-per-tonne over the overlap years
  stecf_effort_catch_ratio <- stecf_effort_catch_ratio[is.finite(days_per_tonne)]  # drop non-finite ratios
  message("\n[Hindcast] stecf_effort_catch_ratio: ", nrow(stecf_effort_catch_ratio), " Country x FleetType",
          " days-per-tonne ratio(s) (FDI's own 2014+ effort/catch relationship) - applied below to hindcast",
          " Spain/France/Italy's pre-2014 effort from their SAU-hindcasted catch-by-fleet.")
}

## --- Rousseau et al. 2024 effort, loaded once and used two ways below:
## (a) calibrated pre-STECF_FDI_START_YEAR hindcast for Spain/France/
## Italy (replacing the FDI-ratio x SAU-catch method as primary, per
## Andrea's direction), and (b) a second effort figure alongside FishMIP
## nom_active for Morocco/Algeria/Tunisia (see ROUSSEAU_EFFORT_PATH's own
## comment above for the validation caveat). Country x Year only (not
## Country x Gear x Year) - Rousseau's own Gear scheme doesn't map onto
## GEAR_TO_FLEETTYPE (that crosswalk is keyed to SAU's gear strings), so
## re-deriving a Rousseau-specific gear crosswalk was out of scope here;
## the Country x Year total is instead distributed across FleetTypes
## below using each FleetType's own catch share, same proportional-
## allocation logic already used for the fleet split itself. ------------
rousseau_effort_cy <- data.table()
if (!file.exists(ROUSSEAU_EFFORT_PATH)) {
  message("\n[Rousseau] Effort CSV not found at '", ROUSSEAU_EFFORT_PATH, "' - EU-3 pre-",
          STECF_FDI_START_YEAR, " hindcast falls back to the FDI-ratio x SAU-catch method,",
          " and Morocco/Algeria/Tunisia effort stays FishMIP-only (no Rousseau comparison column).")
} else {
  rousseau_raw <- fread(ROUSSEAU_EFFORT_PATH, select = c("Year", "Country", "NomEffort"))  # only what's needed here
  rousseau_country_map <- c("Spain" = "Spain", "France" = "France", "Italy" = "Italy",
                            "Morocco" = "Morocco", "Algeria" = "Algeria", "Tunisia" = "Tunisia")  # Rousseau's own Country field already matches these names
  rousseau_effort_cy <- rousseau_raw[Country %in% rousseau_country_map,
                                     .(NomEffort = sum(NomEffort, na.rm = TRUE)), by = .(Country, Year)]
  message("\n[Rousseau] rousseau_effort_cy: ", nrow(rousseau_effort_cy), " Country x Year row(s), ",
          min(rousseau_effort_cy$Year), "-", max(rousseau_effort_cy$Year), " (hard ceiling, not extrapolated -",
          " does NOT cover ", max(rousseau_effort_cy$Year) + 1, "-", END_YEAR, ", same gap as FishMIP).")
}

## Calibration: for Spain/France/Italy, anchor Rousseau's Country x Year
## total to FDI's own real Effort_days total (summed across FleetType) in
## their overlap years - a single per-country multiplicative factor, not
## a trend fit, since the year-to-year correlation itself is weak (see
## caveat above). Wherever a country has no usable factor (no overlap, or
## Rousseau missing), the OLD FDI-ratio x SAU-catch hindcast is kept as
## the fallback for that country - never silently dropped to NA.
rousseau_calibration <- data.table(Country = character(), calib_factor = numeric())
if (nrow(rousseau_effort_cy) > 0 && nrow(stecf_fdi_effort_by_gsa) > 0 && "Effort_total_fishing_days" %in% names(stecf_fdi_effort_by_gsa)) {
  fdi_days_cy <- stecf_fdi_effort_by_gsa[Country %in% c("Spain", "France", "Italy"),
                                         .(Effort_days = sum(Effort_total_fishing_days, na.rm = TRUE)), by = .(Country, Year)]
  cal <- merge(fdi_days_cy, rousseau_effort_cy, by = c("Country", "Year"))
  rousseau_calibration <- cal[, .(fdi_total = sum(Effort_days, na.rm = TRUE), rou_total = sum(NomEffort, na.rm = TRUE)), by = Country]
  rousseau_calibration <- rousseau_calibration[rou_total > 0, .(Country, calib_factor = fdi_total / rou_total)]
  message("[Rousseau] rousseau_calibration (FDI real Effort_days / Rousseau NomEffort, summed over the overlap",
          " years, per country): ", paste(sprintf("%s=%.6g", rousseau_calibration$Country, rousseau_calibration$calib_factor), collapse = ", "),
          ". Applied below as a flat scaling factor to Rousseau's pre-", STECF_FDI_START_YEAR,
          " series - this ANCHORS the level to FDI's real effort but does NOT fix Rousseau's weak",
          " year-to-year correlation with FDI (see ROUSSEAU_EFFORT_PATH comment) - treat the resulting",
          " pre-", STECF_FDI_START_YEAR, " hindcast as a judgment call, same status as TECH_CREEP_COUNTRY_MULTIPLIER.")
}

## =================================================================
## # assign a percentage of total catch to discards
## =================================================================
discard_by_fg <- data.table(FG_num = integer(), discard_ratio = numeric())  # placeholder, filled below if FishMIP data is available
fishmip_fg_scheme_used <- NA_character_

if (!requireNamespace("arrow", quietly = TRUE)) {
  message("\n[Discards] Package 'arrow' not installed - Discard_t will be NA for every FG.")
} else if (!file.exists(FISHMIP_CATCH_PARQUET)) {
  message("\n[Discards] FishMIP catch parquet not found at '", FISHMIP_CATCH_PARQUET, "' - Discard_t will be NA.")
} else {
  catch <- as.data.table(arrow::read_parquet(FISHMIP_CATCH_PARQUET))  # load FishMIP's catch data
  catch[, year := as.integer(year)]  # coerce year to integer
  catch <- catch[year >= START_YEAR & year <= END_YEAR]  # restrict to the study period
  catch[, Yield_t := reported + discards]  # total yield = reported landings + discards
  
  if (!is.null(FISHMIP_FG_CROSSWALK_PATH) && file.exists(FISHMIP_FG_CROSSWALK_PATH)) {
    fg_crosswalk <- fread(FISHMIP_FG_CROSSWALK_PATH)  # load the FishMIP-group -> model-FG crosswalk
    if (!all(c("fishmip_f_group", "FG_num") %in% names(fg_crosswalk))) stop("FISHMIP_FG_CROSSWALK_PATH must have columns fishmip_f_group, FG_num.")
    catch_fg <- merge(catch, fg_crosswalk, by.x = "f_group", by.y = "fishmip_f_group", all.x = TRUE)  # attach model FG_num via the crosswalk
    fishmip_total    <- catch_fg[!is.na(FG_num), .(tonnes = sum(Yield_t, na.rm = TRUE)), by = .(FG_num, Year = year)]  # sum total yield by FG x year
    fishmip_reported <- catch_fg[!is.na(FG_num), .(tonnes_reported = sum(reported, na.rm = TRUE)), by = .(FG_num, Year = year)]  # sum reported landings by FG x year
    fishmip_fg_scheme_used <- "FG_num (via FISHMIP_FG_CROSSWALK_PATH)"
  } else {
    message("\n[Discards] No FISHMIP_FG_CROSSWALK_PATH - discard ratio computed in FishMIP's own f_group naming.")
    fishmip_total    <- catch[, .(tonnes = sum(Yield_t, na.rm = TRUE)), by = .(FG_num = f_group, Year = year)]  # sum total yield by FishMIP group x year
    fishmip_reported <- catch[, .(tonnes_reported = sum(reported, na.rm = TRUE)), by = .(FG_num = f_group, Year = year)]  # sum reported landings by FishMIP group x year
    fishmip_fg_scheme_used <- "fishmip_f_group (no crosswalk supplied)"
  }
  fm <- merge(fishmip_total, fishmip_reported, by = c("FG_num", "Year")); fm <- fm[tonnes > 0]  # pair up total and reported, drop zero-yield cells
  discard_ratio_fg <- fm[, .(discard_ratio = 1 - sum(tonnes_reported, na.rm = TRUE) / sum(tonnes, na.rm = TRUE)), by = FG_num]  # compute discard ratio per FG, across all years
  implausible <- discard_ratio_fg[discard_ratio < 0 | discard_ratio > 0.95]  # ratios outside a plausible range
  if (nrow(implausible) > 0) {
    message("[Discards] ", nrow(implausible), " FG(s) implausible discard_ratio - excluded.")
    discard_ratio_fg <- discard_ratio_fg[!(discard_ratio < 0 | discard_ratio > 0.95)]  # drop implausible ratios
  }
  discard_by_fg <- discard_ratio_fg
  if (is.na(fishmip_fg_scheme_used) || !grepl("^FG_num", fishmip_fg_scheme_used)) {
    ## No crosswalk (or no row in it) ties this FishMIP f_group to a real
    ## model FG_num - rather than dropping the discard estimate entirely,
    ## fall back to a transparent name-keyword match against full_fg_list's
    ## own FG_name: e.g. FishMIP's "Demersals" f_group keyword-matches every
    ## model FG whose FG_name contains "demersal". Per Andrea's instruction
    ## (2026-09): "assign to FG closest, and if targeted by multiple FG in
    ## the model then split it" - here "closest" = shares a keyword with the
    ## FishMIP category name, and "split" = the SAME discard ratio is applied
    ## to every matched FG's own landings (each FG keeps its own tonnage, so
    ## a broad category's discard is automatically apportioned by each FG's
    ## real catch, not divided evenly). This is a heuristic, not a verified
    ## taxonomic crosswalk - every match is written out for manual review,
    ## and any FishMIP category matching ZERO model FG is still dropped
    ## (never guessed at random) and reported below.
    message("[Discards] discard_by_fg is in FishMIP's own f_group scheme, NOT joined onto real FG_num",
            " via FISHMIP_FG_CROSSWALK_PATH. Written separately to discards_by_fishmip_fgroup_timeseries.csv.",
            " Attempting a name-keyword 'closest FG' fallback instead of dropping it entirely.")
    fwrite(discard_by_fg, file.path(out_dir, "discards_by_fishmip_fgroup_timeseries.csv"))  # write the FishMIP-scheme discard ratios to CSV for review
    
    ## Build every (fishmip_f_group, FG_num) pair where the FULL word-set of
    ## the shorter label is contained in the word-set of the longer one -
    ## e.g. "Demersals" (1 word) is a subset of "Demersal fish >90cm"'s
    ## words, so it matches; but "Small pelagics" is NOT a subset of "Large
    ## pelagic fish"'s words (they only share "pelagic", not "small"/
    ## "large" too), so it correctly does NOT match. Requiring the WHOLE
    ## shorter phrase to be contained (not just any one shared word) avoids
    ## false positives from a single generic word like "pelagic" or
    ## "demersal" that recurs across several distinctly-qualified FGs.
    STOPWORDS_FG_MATCH <- c("and", "the", "of", "or", "fish", "other", "spp", "sp")
    destem_word <- function(w) sub("(?<=[a-z]{3})s$", "", w, perl = TRUE)  # crude de-pluralization ("demersals" -> "demersal") so plural/singular forms still match
    words_of <- function(x) { w <- tolower(unlist(strsplit(as.character(x), "[^A-Za-z]+"))); destem_word(w[nchar(w) > 2 & !w %in% STOPWORDS_FG_MATCH]) }  # tokenize a name into meaningful lowercase words
    
    fg_word_sets <- setNames(lapply(full_fg_list$FG_name, words_of), full_fg_list$FG_num)  # word set for every model FG name
    fgroup_names <- unique(discard_by_fg$FG_num)  # still FishMIP's own f_group labels at this point
    fgroup_word_sets <- setNames(lapply(fgroup_names, words_of), fgroup_names)  # word set for every FishMIP group name
    
    match_pairs <- rbindlist(lapply(names(fgroup_word_sets), function(g) {
      gw <- fgroup_word_sets[[g]]
      if (length(gw) == 0) return(NULL)
      hits <- vapply(fg_word_sets, function(fw) {
        length(fw) > 0 && (all(gw %in% fw) || all(fw %in% gw))  # one word set is fully contained in the other
      }, logical(1))
      if (!any(hits)) return(NULL)
      data.table(fgroup = g, FG_num = as.integer(names(fg_word_sets)[hits]))
    }))  # build every FishMIP group -> model FG pair that passes the containment test
    closest_fg_matches <- if (is.null(match_pairs) || nrow(match_pairs) == 0) data.table(fgroup = character(), FG_num = integer()) else unique(match_pairs)  # empty placeholder if nothing matched
    n_matched_fgroups <- uniqueN(closest_fg_matches$fgroup)  # how many FishMIP groups matched at least one FG
    n_unmatched_fgroups <- length(setdiff(fgroup_names, unique(closest_fg_matches$fgroup)))  # how many matched nothing
    message("[Discards] 'closest FG' keyword match: ", n_matched_fgroups, " of ", length(fgroup_names),
            " FishMIP f_group(s) matched to at least one model FG (", n_unmatched_fgroups,
            " unmatched - dropped, not guessed). ",
            sum(table(closest_fg_matches$fgroup) > 1), " matched f_group(s) mapped to MORE THAN ONE model FG",
            " - discard_ratio applied identically to each (split across FGs via their own differing catch tonnage).")
    
    setnames(discard_by_fg, "FG_num", "fgroup")  # this column is still FishMIP's own label at this point, not a real model FG_num
    discard_by_fg <- merge(discard_by_fg, closest_fg_matches, by = "fgroup", allow.cartesian = TRUE)  # attach real model FG_num via the keyword match
    discard_by_fg <- unique(discard_by_fg[, .(FG_num, discard_ratio)])  # keep only the final FG_num x discard_ratio columns
    fwrite(closest_fg_matches, file.path(out_dir, "discards_fishmip_closest_fg_match_REVIEW.csv"))  # write the keyword matches to CSV for manual review
  }
  message("[Discards] discard_ratio available for ", nrow(discard_by_fg), " FG(s) (Mediterranean-wide,",
          " applied uniformly across countries - FishMIP cannot cross country x FG). Max FishMIP year: ", max(catch$year), ".")
}

## =================================================================
## GFCM catch-MAGNITUDE calibration against STECF FDI (2026-09, per
## Andrea's instruction: "FDI is the most trustable dataset ... when
## using GFCM data to fill in gaps on catch for 1994-2013, GFCM should
## be scaled to match FDI magnitude, and the same for the other data").
## Same overlap-year calibration-factor pattern as fleet_calibration/
## discard_calibration above, but applied to GFCM's own absolute
## landed tonnage (gfcm_country_fg$Landings_t) rather than a share or
## a ratio - GFCM and FDI can disagree on the absolute tonnage for the
## same Country x FG x Year even where both cover it (different
## methodology/coverage), and FDI is the more trusted of the two. The
## factor is applied across GFCM's WHOLE series for Spain/France/Italy
## (not just the pre-2014 hindcast years, per "the same for the other
## data needs to be adapted to FDI") so GFCM's magnitude is consistent
## with FDI everywhere, not only in the years GFCM alone fills the gap
## - this also avoids a step-change discontinuity right at 2014, where
## the hindcast portion would otherwise jump to meet FDI's own figures.
## Morocco/Algeria/Tunisia have no FDI to calibrate against, so their
## GFCM landings stay uncalibrated (same "never guess a correction with
## nothing to base it on" convention used everywhere else here).
## =================================================================
catch_magnitude_calibration <- data.table()
if (nrow(stecf_fdi_catch_by_gsa) > 0) {
  stecf_catch_cfy <- stecf_fdi_catch_by_gsa[, .(Catch_t_stecf = sum(Catch_t, na.rm = TRUE)), by = .(Country, FG_num, Year)]  # collapse FDI's fine-grained catch up to Country x FG x Year
  overlap_years_m <- sort(intersect(unique(stecf_catch_cfy$Year), unique(gfcm_country_fg$Year)))  # years where both GFCM and FDI have data
  if (length(overlap_years_m) > 0) {
    stecf_overlap_m <- stecf_catch_cfy[Year %in% overlap_years_m, .(Catch_t_stecf = mean(Catch_t_stecf, na.rm = TRUE)),
                                       by = .(Country, FG_num)]  # FDI's average landed tonnage over the overlap years
    gfcm_overlap_m <- gfcm_country_fg[Year %in% overlap_years_m & Country %in% c("Spain", "France", "Italy"),
                                      .(Catch_t_gfcm = mean(Landings_t, na.rm = TRUE)), by = .(Country, FG_num)]  # GFCM's average landed tonnage over the same years
    catch_magnitude_calibration <- merge(stecf_overlap_m, gfcm_overlap_m, by = c("Country", "FG_num"))  # pair up FDI vs GFCM magnitude
    catch_magnitude_calibration[, calibration_factor := fifelse(Catch_t_gfcm > 0, Catch_t_stecf / Catch_t_gfcm, NA_real_)]  # ratio of FDI's tonnage to GFCM's
    catch_magnitude_calibration <- catch_magnitude_calibration[is.finite(calibration_factor), .(Country, FG_num, calibration_factor)]  # drop non-finite factors
    fwrite(catch_magnitude_calibration, file.path(out_dir, "gfcm_stecf_catch_magnitude_calibration_factors.csv"))  # write result to CSV
    message("\n[Catch magnitude] GFCM-vs-STECF FDI catch magnitude calibration, overlap years ", min(overlap_years_m), "-",
            max(overlap_years_m), ": ", nrow(catch_magnitude_calibration), " Country x FG factor(s) - applied to GFCM's",
            " own Landings_t across its WHOLE series for Spain/France/Italy (a factor > 1 means FDI's own landed",
            " tonnage runs higher than GFCM's for that Country x FG, and vice versa).")
  } else {
    message("\n[Catch magnitude] No years where both GFCM and STECF FDI have data - GFCM's landings stay uncalibrated.")
  }
} else {
  message("\n[Catch magnitude] STECF FDI catch data not available - GFCM's landings stay uncalibrated against it.")
}

if (nrow(catch_magnitude_calibration) > 0) {
  gfcm_country_fg <- merge(gfcm_country_fg, catch_magnitude_calibration, by = c("Country", "FG_num"), all.x = TRUE)  # attach the FDI-vs-GFCM magnitude factor
  gfcm_country_fg[, magnitude_source := fifelse(is.na(calibration_factor), "GFCM (uncalibrated - no STECF FDI overlap)",
                                                "GFCM (scaled to STECF FDI magnitude)")]  # tag the source/status of this row's magnitude
  gfcm_country_fg[, calibration_factor := fifelse(is.na(calibration_factor), 1, calibration_factor)]  # default to no correction where uncalibrated
  gfcm_country_fg[, Landings_t := Landings_t * calibration_factor]  # apply the magnitude correction
  gfcm_country_fg[, calibration_factor := NULL]  # drop the now-unneeded helper column
  n_calibrated_cfg <- uniqueN(catch_magnitude_calibration[, .(Country, FG_num)])
  message("[Catch magnitude] Rescaled ", n_calibrated_cfg, " Country x FG combination(s) of GFCM's Landings_t to",
          " match STECF FDI's magnitude across the whole series.")
} else {
  gfcm_country_fg[, magnitude_source := "GFCM (uncalibrated - no STECF FDI data to compare against)"]  # flag every row as uncalibrated, no factor available at all
}

## Country x FG x Year catch WITH discards added back onto GFCM's landings
## (now scaled to STECF FDI's magnitude above wherever a factor exists):
catch_with_discards <- merge(gfcm_country_fg, discard_by_fg, by = "FG_num", all.x = TRUE)  # attach the FishMIP discard ratio to every country x FG x year row
catch_with_discards[, `:=`(
  Catch_t   = fifelse(!is.na(discard_ratio), Landings_t / (1 - discard_ratio), Landings_t),  # gross up landings to include discards where a ratio exists
  Discard_t = fifelse(!is.na(discard_ratio), Landings_t / (1 - discard_ratio) - Landings_t, NA_real_),  # implied discard tonnage
  discard_source = fifelse(!is.na(discard_ratio), "FishMIP reported-vs-total ratio (FG-level, Med-wide)", "not available - landings-only")
)]

## Prefer SAU's own discard ratio (hindcast/bias-corrected against FDI
## for Spain/France/Italy, uncalibrated for Morocco/Algeria/Tunisia)
## over FishMIP's blanket Med-wide FG-level ratio, for every Country x
## FG x Year cell SAU actually has one for - a real, country/year-
## specific figure instead of one Mediterranean-wide average applied
## to every country alike. STECF FDI's own ratio (applied next) still
## wins over this wherever FDI itself covers the cell.
if (nrow(sau_discard_ratio_by_year_hindcast) > 0) {
  catch_with_discards <- merge(catch_with_discards, sau_discard_ratio_by_year_hindcast[, .(Country, FG_num, Year, discard_ratio_sau = discard_ratio)],
                               by = c("Country", "FG_num", "Year"), all.x = TRUE)  # attach SAU's own discard ratio, where available
  n_sau_discard <- sum(!is.na(catch_with_discards$discard_ratio_sau))  # cells where SAU's ratio applies
  catch_with_discards[!is.na(discard_ratio_sau), `:=`(
    Catch_t   = Landings_t / (1 - discard_ratio_sau),  # recompute gross catch using SAU's ratio instead
    Discard_t = Landings_t / (1 - discard_ratio_sau) - Landings_t,
    discard_ratio = discard_ratio_sau,
    discard_source = fifelse(Country %in% c("Spain", "France", "Italy"),
                             "SAU (own year, hindcast - bias-corrected against STECF FDI's overlap years)",
                             "SAU (own year, hindcast - uncalibrated, no STECF FDI overlap for this country)")
  )]  # overwrite FishMIP's estimate wherever SAU's own ratio is available
  catch_with_discards[, discard_ratio_sau := NULL]  # drop the now-unneeded helper column
  message("\n[Discards] ", n_sau_discard, " Country x FG x Year cell(s) now use SAU's own discard ratio",
          " instead of FishMIP's Med-wide FG-level ratio.")
}

## Prefer STECF FDI's own Country x GSA x Year discards (2014+, Spain/
## France/Italy) over both of the above, wherever FDI's file actually
## separated landings from discards (see the STECF FDI block above) -
## a real, country/year-specific figure that beats every fallback tier.
if (nrow(stecf_discard_ratio) > 0) {
  catch_with_discards <- merge(catch_with_discards, stecf_discard_ratio[, .(Country, FG_num, Year, discard_ratio_stecf = discard_ratio)],
                               by = c("Country", "FG_num", "Year"), all.x = TRUE)  # attach STECF FDI's own discard ratio, where available
  n_stecf_discard <- sum(!is.na(catch_with_discards$discard_ratio_stecf))  # cells where FDI's ratio applies
  catch_with_discards[!is.na(discard_ratio_stecf), `:=`(
    Catch_t   = Landings_t / (1 - discard_ratio_stecf),  # recompute gross catch using FDI's ratio instead
    Discard_t = Landings_t / (1 - discard_ratio_stecf) - Landings_t,
    discard_ratio = discard_ratio_stecf,
    discard_source = "STECF FDI (own Country x GSA x Year discards, 2014+, Spain/France/Italy)"
  )]  # overwrite the previous estimate wherever FDI's own ratio is available
  catch_with_discards[, discard_ratio_stecf := NULL]  # drop the now-unneeded helper column
  message("\n[Discards] ", n_stecf_discard, " Country x FG x Year cell(s) now use STECF FDI's own",
          " discard ratio instead of FishMIP's Med-wide FG-level ratio.")
}

## =================================================================
## Morocco/Algeria real local catch data (2026-09, per Andrea's uploaded
## workbook - see MOROCCO_ALGERIA_DIR comment near the top of this
## script). Belhabib et al.'s reconstructed catch-by-taxon-group tables
## (Morocco: Belhabib, Harper, Zeller & Pauly 2013, Table A2a;
## Algeria: Belhabib, Pauly, Harper & Zeller 2012, Table A2), 1994-2010,
## are the closest thing either country has to a real, independent,
## FG-attributable catch source - GFCM's own reported statistics for
## non-EU-reporting countries are the only other option, and are widely
## considered to understate true catch (the whole reason Sea-Around-Us-
## style reconstructions like this one exist). Where this table covers a
## Country x Year, it REPLACES GFCM's own Landings_t/Catch_t for that
## cell (not just a magnitude-calibration factor, unlike the STECF FDI
## block above) - Belhabib's own total already nets out to a genuine
## independent total catch figure, not a proxy to rescale GFCM by.
##
## Both tables report broad TAXON GROUPS (e.g. "Sparidae", "Scombroids"),
## not species - too coarse for the normal species->FG matching cascade
## above. Instead, each taxon group is matched to a model FG by name-
## keyword against full_fg_list$FG_name (same technique as the FishMIP
## discard "closest FG" fallback earlier in this script): only a taxon
## group matching EXACTLY ONE FG is used; anything matching zero or more
## than one FG is written to a REVIEW csv and its catch is left out of
## the override entirely (never guessed) - the affected cells simply
## keep GFCM's own figure for now.
## =================================================================
mar_catch_by_taxon <- safe_fread_optional(file.path(MOROCCO_ALGERIA_DIR, "MAR_Catch_by_Species.csv"), "MAR_Catch_by_Species.csv")
dza_catch_by_taxon <- safe_fread_optional(file.path(MOROCCO_ALGERIA_DIR, "DZA_Catch_by_Species.csv"), "DZA_Catch_by_Species.csv")

## Manual taxon-group -> FG_name keyword crosswalk (regex, matched
## case-insensitively against full_fg_list$FG_name below) - built from
## the actual column headers on each sheet. Kept separate per country
## since Morocco's and Algeria's taxon groupings differ (e.g. Morocco
## splits out "Sardina pilchardus" on its own column; Algeria calls the
## same thing "Sardine" but ALSO has a separate "Anchovy" column Morocco
## folds into "Other small pelagics" instead).
BELHABIB_TAXON_KEYWORDS <- list(
  Sparidae = "sparid|seabream|bream",
  `Sardina pilchardus` = "sardine|pilchard",
  Sardine = "sardine|pilchard",
  Anchovy = "anchov",
  `Other small pelagics` = "small pelagic",
  `Misc. pelagics` = "small pelagic|pelagic",
  Groupers = "grouper",
  Sparids = "sparid|seabream|bream",
  `Sharks & rays` = "shark|ray|elasmobranch",
  Cephalopods = "cephalopod|octopus|squid|cuttlefish",
  Scombroids = "scombrid|tuna|mackerel|bonito",
  Crustaceans = "crustacean|decapod|shrimp|lobster",
  Crustacea = "crustacean|decapod|shrimp|lobster",
  `Molluscs & bivalves` = "mollusc|bivalve",
  Miscellaneous = NA_character_,   # deliberately no keyword - too broad to attribute to one FG, always sent to review
  `Misc.` = NA_character_          # same
)

match_taxon_to_fg <- function(taxon_names, fg_name_vec, fg_num_vec) {
  rbindlist(lapply(taxon_names, function(tx) {
    kw <- BELHABIB_TAXON_KEYWORDS[[tx]]
    if (is.null(kw) || is.na(kw)) return(data.table(taxon = tx, FG_num = NA_integer_, n_match = 0L))
    hits <- grepl(kw, fg_name_vec, ignore.case = TRUE)
    data.table(taxon = tx, FG_num = if (sum(hits) == 1) fg_num_vec[hits] else NA_integer_, n_match = sum(hits))
  }))
}

belhabib_catch_by_fg <- data.table()
if (nrow(mar_catch_by_taxon) > 0 || nrow(dza_catch_by_taxon) > 0) {
  belhabib_long <- rbindlist(list(
    if (nrow(mar_catch_by_taxon) > 0) melt(mar_catch_by_taxon[, setdiff(names(mar_catch_by_taxon), "Total (=SUM)"), with = FALSE],
                                           id.vars = "Year", variable.name = "taxon", value.name = "Catch_t")[, Country := "Morocco"] else NULL,
    if (nrow(dza_catch_by_taxon) > 0) melt(dza_catch_by_taxon[, setdiff(names(dza_catch_by_taxon), "Total (=SUM)"), with = FALSE],
                                           id.vars = "Year", variable.name = "taxon", value.name = "Catch_t")[, Country := "Algeria"] else NULL
  ), use.names = TRUE)  # both sheets stacked into one long Country x Year x taxon x Catch_t table
  belhabib_long <- belhabib_long[!is.na(Catch_t)]
  
  taxon_fg_matches <- match_taxon_to_fg(unique(belhabib_long$taxon), full_fg_list$FG_name, full_fg_list$FG_num)
  fwrite(taxon_fg_matches, file.path(out_dir, "belhabib_taxon_to_fg_match_REVIEW.csv"))  # every taxon->FG attempt, for manual review
  n_taxon_matched <- taxon_fg_matches[n_match == 1, .N]
  message("\n[Morocco/Algeria catch] Belhabib taxon-group -> FG keyword match: ", n_taxon_matched, " of ",
          nrow(taxon_fg_matches), " taxon group(s) matched to exactly one FG (the rest matched zero or several FGs",
          " and are excluded from the override below - see belhabib_taxon_to_fg_match_REVIEW.csv).")
  
  belhabib_long <- merge(belhabib_long, taxon_fg_matches[n_match == 1, .(taxon, FG_num)], by = "taxon")  # keep only cleanly-matched taxon groups
  belhabib_catch_by_fg <- belhabib_long[, .(Catch_t_belhabib = sum(Catch_t, na.rm = TRUE)), by = .(Country, FG_num, Year)]  # aggregate to Country x FG x Year
  fwrite(belhabib_catch_by_fg, file.path(out_dir, "belhabib_catch_by_fg_timeseries.csv"))
  message("[Morocco/Algeria catch] ", nrow(belhabib_catch_by_fg), " Country x FG x Year cell(s) available from",
          " Belhabib et al.'s reconstructed catch, years ", min(belhabib_catch_by_fg$Year), "-", max(belhabib_catch_by_fg$Year), ".")
} else {
  message("\n[Morocco/Algeria catch] No Belhabib catch-by-taxon CSV(s) found in MOROCCO_ALGERIA_DIR - Morocco/Algeria",
          " keep GFCM's own (uncalibrated) landings as their only catch source, same as before.")
}

if (nrow(belhabib_catch_by_fg) > 0) {
  catch_with_discards <- merge(catch_with_discards, belhabib_catch_by_fg, by = c("Country", "FG_num", "Year"), all.x = TRUE)  # attach Belhabib's own catch figure, where available
  n_belhabib_override <- sum(!is.na(catch_with_discards$Catch_t_belhabib))  # cells about to be overridden
  catch_with_discards[!is.na(Catch_t_belhabib), `:=`(
    Catch_t = Catch_t_belhabib,  # Belhabib's own reconstructed total (already gross, discards included in the source methodology)
    Discard_t = fifelse(!is.na(discard_ratio), Catch_t_belhabib * discard_ratio, NA_real_),  # split via whichever discard_ratio tier already resolved for this cell, if any
    Landings_t = fifelse(!is.na(discard_ratio), Catch_t_belhabib * (1 - discard_ratio), Landings_t),  # same split, applied to the landings side
    discard_source = fifelse(!is.na(discard_ratio),
                             paste0("Belhabib et al. total catch (gross), split via ", discard_source),
                             "Belhabib et al. total catch (gross) - no discard_ratio tier available to split it"),
    magnitude_source = "Belhabib et al. (2012/2013) reconstructed catch - overrides GFCM entirely for this cell"
  )]
  catch_with_discards[, Catch_t_belhabib := NULL]  # drop the now-unneeded helper column
  message("[Morocco/Algeria catch] ", n_belhabib_override, " Country x FG x Year cell(s) now use Belhabib et al.'s",
          " own reconstructed catch instead of GFCM's landings.")
}

## =================================================================
## # obtain ts of catches per FG
## Full West Med series, all GFCM-reporting countries (not restricted
## to the 6 TARGET_COUNTRIES - this is the overall catch/discard series,
## and the correct scope to compare against Biomass, which also covers
## the whole modeled area, not just the 6 target countries' fleets).
## Moved up here (ahead of unreported/bycatch/fleet-split below) because
## Catches_Ecopath/Catches_Ecosim and the F step both need it.
## =================================================================
fg_catch_timeseries <- gfcm_species_division_fg[, .(Landings_t = sum(Landings_t, na.rm = TRUE)), by = .(Year, FG_num, FG_name)]  # sum landings by year x FG, across all countries/divisions

## Complete the grid to EVERY FG in full_fg_list (built above from the
## FG reference file) x every year in START_YEAR:END_YEAR - a FG with
## no GFCM catch at all, or missing in just some years, must still get
## an explicit row with Catch_t = 0, not simply be absent. Absent would
## read as "not modeled here"; 0 correctly reads as "modeled, no catch
## recorded". add_catches_to_ecopath_workbook() needs one row per FG
## regardless either way.
fg_catch_grid <- CJ(FG_num = full_fg_list$FG_num, Year = START_YEAR:END_YEAR)  # every FG x every year in the study period, cross-joined
fg_catch_grid <- merge(fg_catch_grid, full_fg_list, by = "FG_num", all.x = TRUE)  # attach FG names to the grid
fg_catch_timeseries <- merge(fg_catch_grid, fg_catch_timeseries, by = c("FG_num", "FG_name", "Year"), all.x = TRUE)  # fill the grid with actual landings where available
fg_catch_timeseries[is.na(Landings_t), Landings_t := 0]  # explicit zero landings for FG x years with no GFCM catch
setorder(fg_catch_timeseries, FG_num, Year)  # sort for readability
n_fg_zero_catch <- uniqueN(fg_catch_timeseries[, .(has_catch = sum(Landings_t) > 0), by = FG_num][has_catch == FALSE]$FG_num)  # count FGs with zero catch in every year
message("\n[Catches] fg_catch_timeseries completed to ", uniqueN(full_fg_list$FG_num), " FG(s) x ",
        length(START_YEAR:END_YEAR), " year(s) - ", n_fg_zero_catch, " FG(s) have zero GFCM catch in every",
        " year (kept as explicit 0 rows, not dropped).")

catches_discards_fg <- copy(fg_catch_timeseries)  # start from the full FG x year landings grid
if (nrow(discard_by_fg) > 0) {
  catches_discards_fg <- merge(catches_discards_fg, discard_by_fg, by = "FG_num", all.x = TRUE)  # attach FishMIP's discard ratio
  catches_discards_fg[, `:=`(
    Catch_t   = fifelse(!is.na(discard_ratio), Landings_t / (1 - discard_ratio), Landings_t),  # gross up landings to include discards
    Discard_t = fifelse(!is.na(discard_ratio), Landings_t / (1 - discard_ratio) - Landings_t, NA_real_)
  )]
} else {
  catches_discards_fg[, `:=`(Catch_t = Landings_t, Discard_t = NA_real_, discard_ratio = NA_real_)]  # no discard data at all - Catch_t equals Landings_t
}
setcolorder(catches_discards_fg, c("Year", "FG_num", "FG_name", "Landings_t", "Catch_t", "Discard_t"))  # standardize column order
message("\n[Catches] fg_catch_timeseries: ", nrow(fg_catch_timeseries), " FG x Year row(s), full West Med series.")

## --- Catches_Ecopath / Catches_Ecosim (FG-only) -------------------------
## Same structure/units as Biomass's own Ecopath/Ecosim sheets (t/km^2/
## year density) - built with the SAME shared function Step 1 uses for
## Biomass (add_catches_to_ecopath_workbook(), sourced above), not a
## separate reimplementation that could drift from it. Total catch
## (landings + discards where FishMIP's ratio is available), full West
## Med scope. Writes directly into the workbook.
add_catches_to_ecopath_workbook(  # write Catches_Ecopath/Catches_Ecosim sheets into the workbook
  fg_catch        = catches_discards_fg[, .(Year, FG_num, Catch_t)],
  fg_lookup       = full_fg_list,
  out_path        = ECOPATH_WORKBOOK_PATH,
  year_ecopath    = YEAR_ECOPATH,
  area_km2        = Total_Area_km2,
  ts_years        = START_YEAR:END_YEAR,
  fleet_structure = NULL
)

## =================================================================
## # any percentage of unreported
## Separate from discards: SAU's own reconstruction adds back
## unreported/IUU catch on top of officially reported landings - this
## is SAU's reported-vs-total ratio, by country, NOT FishMIP's
## reported-vs-(reported+discards) ratio used above. Reported here as
## its own multiplier layered on top of catch_with_discards, so the two
## adjustments (discards, unreported) never get silently conflated.
## =================================================================
unreported_by_country <- data.table(Country = character(), unreported_ratio = numeric())  # placeholder, filled below if SAU data is available
if (nrow(sau_total_cy) > 0 && nrow(sau_reported_cy) > 0) {
  sau_tot <- sau_total_cy[, .(tonnes = sum(tonnes, na.rm = TRUE)), by = Country]  # SAU's total reconstructed catch, whole period, per country
  sau_rep <- sau_reported_cy[, .(tonnes_reported = sum(tonnes, na.rm = TRUE)), by = Country]  # SAU's reported subset, whole period, per country
  unreported_by_country <- merge(sau_tot, sau_rep, by = "Country", all.x = TRUE)  # pair up total and reported per country
  unreported_by_country[, unreported_ratio := 1 - tonnes_reported / tonnes]  # compute the unreported share
  unreported_by_country[is.na(unreported_ratio) | unreported_ratio < 0, unreported_ratio := NA_real_]  # drop implausible ratios
  message("\n[Unreported] unreported_ratio (SAU reported-vs-total, whole period, per country): ")
  print(unreported_by_country[, .(Country, unreported_ratio = round(unreported_ratio, 3))])  # print the ratio table to console
} else {
  message("\n[Unreported] SAU data unavailable - unreported_ratio will be NA for every country.")
}

catch_with_unreported <- merge(catch_with_discards, unreported_by_country[, .(Country, unreported_ratio)], by = "Country", all.x = TRUE)  # attach the unreported ratio to every catch row
if (isTRUE(APPLY_UNREPORTED_ADJUSTMENT)) {
  catch_with_unreported[, Catch_t_incl_unreported := fifelse(!is.na(unreported_ratio), Catch_t / (1 - unreported_ratio), NA_real_)]  # gross up catch to include unreported/IUU catch
} else {
  catch_with_unreported[, Catch_t_incl_unreported := NA_real_]  # adjustment switched off - leave this column empty
  message("\n[Unreported] APPLY_UNREPORTED_ADJUSTMENT is FALSE - Catch_t_incl_unreported left as NA",
          " everywhere (the ratio above is kept for reference/review only, not applied). Set",
          " APPLY_UNREPORTED_ADJUSTMENT <- TRUE once the ratio has been checked.")
}

## =================================================================
## # bycatch
## No source anywhere in this pipeline (GFCM, SAU, FishMIP) gives a
## bycatch rate. Flagged manual-entry placeholder ONLY, same convention
## as recreational catch elsewhere in this pipeline - fill in by hand
## (Country/FG_num/Year/bycatch_rate/source_citation) if you have
## literature or stock-assessment figures; anything left unfilled stays
## explicitly "not estimated" rather than defaulting to zero.
## =================================================================
BYCATCH_RATE_MANUAL <- data.table(
  Country = character(), FG_num = integer(), Year = integer(),
  bycatch_rate = numeric(), source_citation = character()
)
bycatch_placeholder <- unique(catch_with_unreported[, .(Country, FG_num, FG_name)])  # one row per country x FG for the bycatch placeholder table
if (nrow(BYCATCH_RATE_MANUAL) > 0) {
  bycatch_placeholder <- merge(bycatch_placeholder, BYCATCH_RATE_MANUAL, by = c("Country", "FG_num"), all.x = TRUE)  # attach any manually-entered rates
} else {
  bycatch_placeholder[, `:=`(bycatch_rate = NA_real_, source_citation = NA_character_)]  # no manual entries at all - leave empty
}
bycatch_placeholder[, data_status := fifelse(is.na(bycatch_rate), "not estimated - no source in this pipeline captures bycatch", "manually entered - see source_citation")]  # flag whether each row is filled or not
message("\n[Bycatch] ", sum(!is.na(bycatch_placeholder$bycatch_rate)), " of ", nrow(bycatch_placeholder),
        " Country x FG cell(s) filled from BYCATCH_RATE_MANUAL; the rest flagged 'not estimated'.")

## =================================================================
## # recreational fishing EFFORT - manual-entry placeholder only
## Recreational CATCH has a proxy (recreational_rows above, from SAU's
## own sector split). Recreational EFFORT does not - checked GFCM, FDI,
## SAU and FishMIP, none carries a recreational-effort variable of any
## kind (days, boats, anglers) for these 6 countries. Per Andrea (2026-
## 09): "recreational effort is added later as expert knowledge" - same
## flagged manual-entry convention as BYCATCH_RATE_MANUAL, fill in by
## hand (Country/Year/recreational_effort/units/source_citation) if/when
## an expert figure becomes available; left empty this stays explicitly
## "not estimated", never defaulted to zero or silently omitted from the
## output like it was before this placeholder existed.
## =================================================================
RECREATIONAL_EFFORT_MANUAL <- data.table(
  Country = character(), Year = integer(),
  recreational_effort = numeric(), units = character(), source_citation = character()
)
recreational_effort_placeholder <- CJ(Country = unique(FLEET_REGISTER[Sector == "Recreational"]$Country), Year = YEAR_ECOPATH)  # one row per country x Ecopath-year
if (nrow(RECREATIONAL_EFFORT_MANUAL) > 0) {
  recreational_effort_placeholder <- merge(recreational_effort_placeholder, RECREATIONAL_EFFORT_MANUAL, by = c("Country", "Year"), all.x = TRUE)  # attach any manually-entered figures
} else {
  recreational_effort_placeholder[, `:=`(recreational_effort = NA_real_, units = NA_character_, source_citation = NA_character_)]  # no manual entries at all - leave empty
}
recreational_effort_placeholder[, data_status := fifelse(is.na(recreational_effort),
                                                         "not estimated - no source in this pipeline captures recreational effort", "manually entered - see source_citation")]  # flag whether each row is filled or not
fwrite(recreational_effort_placeholder, file.path(out_dir, "recreational_effort_placeholder.csv"))  # write result to CSV
message("\n[Recreational effort] ", sum(!is.na(recreational_effort_placeholder$recreational_effort)), " of ",
        nrow(recreational_effort_placeholder), " Country x Year cell(s) filled from RECREATIONAL_EFFORT_MANUAL;",
        " the rest flagged 'not estimated' - written to recreational_effort_placeholder.csv.")

## =================================================================
## GFCM DCRF Task 2 (catch by fleet segment) - NOT automatable.
## Task 2 is the right slot in GFCM's own framework for exactly this
## (fleet-segment-resolved catch, covering ALL GFCM members including
## Morocco/Algeria/Tunisia, not just the 3 EU countries STECF FDI
## covers above) - but it is submitted by national authorities
## through GFCM's DCRF platform (fao.org/gfcm/data/dcrf/platform/en),
## which is a data-SUBMISSION portal gated by CPC-specific credentials
## issued by the GFCM Secretariat, not a public bulk-download product.
## No aggregate Task 2 output was found published anywhere else either
## (it appears to feed GFCM's own stock assessments and the "State of
## Mediterranean and Black Sea Fisheries" report, not to come back out
## as open data). Flagged manual-entry placeholder ONLY, same
## convention as BYCATCH_RATE_MANUAL/RECREATIONAL_CATCH_MANUAL above -
## fill in by hand (Country/GSA/FleetType/FG_num/Year/Catch_t/
## source_citation) if you or a GFCM contact ever obtains real Task 2
## figures; everything else stays explicitly "not available" rather
## than silently falling back to the SAU/STECF-derived split above.
## =================================================================
GFCM_TASK2_CATCH_MANUAL <- data.table(
  Country = character(), GSA = character(), FleetType = character(),
  FG_num = integer(), Year = integer(), Catch_t = numeric(), source_citation = character()
)
gfcm_task2_placeholder <- unique(fleet_prop[, .(Country, GSA, FleetType, FG_num)])  # one row per country x GSA x fleet x FG for the Task 2 placeholder table
if (nrow(GFCM_TASK2_CATCH_MANUAL) > 0) {
  gfcm_task2_placeholder <- merge(gfcm_task2_placeholder, GFCM_TASK2_CATCH_MANUAL,
                                  by = c("Country", "GSA", "FleetType", "FG_num"), all.x = TRUE)  # attach any manually-entered figures
} else {
  gfcm_task2_placeholder[, `:=`(Year = NA_integer_, Catch_t = NA_real_, source_citation = NA_character_)]  # no manual entries at all - leave empty
}
gfcm_task2_placeholder[, data_status := fifelse(is.na(Catch_t),
                                                "not available - GFCM DCRF Task 2 is CPC-credential-gated, no public bulk export exists",
                                                "manually entered - see source_citation")]  # flag whether each row is filled or not
message("\n[GFCM Task 2] ", sum(!is.na(gfcm_task2_placeholder$Catch_t)), " of ", nrow(gfcm_task2_placeholder),
        " Country x GSA x FleetType x FG cell(s) filled from GFCM_TASK2_CATCH_MANUAL; the rest flagged",
        " 'not available' (see this block's comment for why Task 2 can't be pulled automatically).")

## =================================================================
## Assemble the fleet-level table (Country x FG x Sector x FleetType x
## Year). Per Andrea's flow: STECF FDI's own PER-YEAR gear/metier x GSA
## split is used for Spain/France/Italy from 2014 onward wherever FDI
## actually covers that Country x FG x Year; SAU/default's time-
## invariant split (fleet_prop, built above) fills every other cell -
## Morocco/Algeria/Tunisia always, and Spain/France/Italy for 1994-2012
## or any Country x FG x Year FDI doesn't reach.
## =================================================================
catch_country_fg_year <- unique(catch_with_unreported[, .(Country, FG_num, Year)])  # every country x FG x year cell needing a fleet split
if (nrow(stecf_fleet_prop_by_year) > 0) {
  stecf_coverage <- unique(stecf_fleet_prop_by_year[, .(Country, FG_num, Year, stecf_covers = TRUE)])  # cells FDI actually covers
  catch_country_fg_year <- merge(catch_country_fg_year, stecf_coverage, by = c("Country", "FG_num", "Year"), all.x = TRUE)  # flag which cells FDI covers
} else {
  catch_country_fg_year[, stecf_covers := NA]  # FDI has nothing at all - no cell is covered
}
catch_country_fg_year[, use_stecf := !is.na(stecf_covers)]  # TRUE for cells to source from FDI

stecf_rows_needed <- catch_country_fg_year[use_stecf == TRUE, .(Country, FG_num, Year)]  # cells to fill from FDI
non_stecf_rows     <- catch_country_fg_year[use_stecf == FALSE, .(Country, FG_num, Year)]  # cells needing a fallback

fleet_prop_stecf_part <- if (nrow(stecf_rows_needed) > 0) {
  merge(stecf_rows_needed, stecf_fleet_prop_by_year, by = c("Country", "FG_num", "Year"))  # attach FDI's own fleet split for these cells
} else {
  data.table(Country = character(), FG_num = integer(), Year = integer(), Sector = character(),
             FleetType = character(), GSA = character(), Comment = character(),
             prop_fleet = numeric(), fleet_split_source = character())
}

## Two-tier fallback for every cell STECF doesn't cover: (1) SAU's own
## real year-by-year shares, hindcast/bias-corrected above - covers
## any Country x FG x Year SAU actually has catch for, including the
## pre-2014 years this hindcast exists for; (2) fleet_prop's flat,
## all-years average, only for the rarer cells even SAU has zero catch
## for in that specific year.
fleet_prop_hindcast_part <- if (nrow(sau_fleet_prop_by_year_hindcast) > 0) {
  merge(non_stecf_rows, sau_fleet_prop_by_year_hindcast, by = c("Country", "FG_num", "Year"))  # attach SAU's hindcasted fleet split for these cells
} else {
  data.table(Country = character(), FG_num = integer(), Year = integer(), Sector = character(),
             FleetType = character(), GSA = character(), Comment = character(),
             prop_fleet = numeric(), fleet_split_source = character())
}
hindcast_covered <- unique(fleet_prop_hindcast_part[, .(Country, FG_num, Year)])  # cells the hindcast tier actually filled
flat_rows_needed <- non_stecf_rows[!hindcast_covered, on = c("Country", "FG_num", "Year")]  # cells still needing the flat fallback
fleet_prop_flat_part <- if (nrow(flat_rows_needed) > 0) {
  merge(flat_rows_needed, fleet_prop[, .(Country, FG_num, Sector, FleetType, GSA, Comment, prop_fleet, fleet_split_source)],
        by = c("Country", "FG_num"), allow.cartesian = TRUE)  # attach fleet_prop's flat all-years average for these cells
} else {
  data.table(Country = character(), FG_num = integer(), Year = integer(), Sector = character(),
             FleetType = character(), GSA = character(), Comment = character(),
             prop_fleet = numeric(), fleet_split_source = character())
}

fleet_prop_final <- rbindlist(list(fleet_prop_stecf_part, fleet_prop_hindcast_part, fleet_prop_flat_part), use.names = TRUE, fill = TRUE)  # combine all three tiers into the final fleet split
n_stecf_cells    <- nrow(unique(fleet_prop_stecf_part[, .(Country, FG_num, Year)]))  # cells sourced from FDI
n_hindcast_cells <- nrow(hindcast_covered)  # cells sourced from the SAU hindcast
n_flat_cells     <- nrow(unique(fleet_prop_flat_part[, .(Country, FG_num, Year)]))  # cells sourced from the flat fallback
message("\n[Fleet split] fleet_prop_final: ", n_stecf_cells, " cell(s) use STECF FDI's own per-year",
        " gear/metier x GSA split (2014+, Spain/France/Italy); ", n_hindcast_cells, " cell(s) use SAU's",
        " own year-by-year hindcast (bias-corrected against FDI where a calibration factor exists); ",
        n_flat_cells, " cell(s) fall all the way back to fleet_prop's flat all-years average (SAU has no",
        " catch at all for that specific Country x FG x Year). Total: ", nrow(catch_country_fg_year), " cell(s).")

fleet_split <- merge(catch_with_unreported, fleet_prop_final, by = c("Country", "FG_num", "Year"), allow.cartesian = TRUE)  # apply the fleet split to the country-level catch
fleet_split <- fleet_split[!is.na(prop_fleet)]  # drop rows with no fleet share at all

## Default: distribute the country-level catch/discard total by each
## fleet's catch share - this is a UNIFORM discard rate assumption
## (every fleet in the country discards at the same rate), only ever
## right by coincidence.
fleet_split[, `:=`(
  Catch_t_by_fleet   = Catch_t * prop_fleet,  # this fleet's share of country-level catch
  Discard_t_by_fleet = Discard_t * prop_fleet,  # this fleet's share of country-level discards
  Landings_t_by_fleet = Landings_t * prop_fleet  # this fleet's share of country-level landings
)]

## Prefer a real per-fleet discard rate wherever STECF FDI actually
## gives one (stecf_discard_ratio_by_fleet, built above): a trawl
## fleet and a longline fleet do not discard at the same rate, so this
## replaces the uniform assumption above for exactly the Country x FG x
## FleetType x Year cells FDI covers - everywhere else (SAU-covered
## fleets, pre-2014, Morocco/Algeria/Tunisia) still falls back to the
## country-level ratio distributed by catch share, flagged as such.
fleet_split[, discard_split_source := "country-level discard ratio distributed by catch share (uniform across fleets - not a per-fleet rate)"]  # default assumption, may be overwritten below
if (nrow(stecf_discard_ratio_by_fleet) > 0) {
  fleet_split <- merge(fleet_split, stecf_discard_ratio_by_fleet[, .(Country, FG_num, FleetType, Year, discard_ratio_fleet = discard_ratio)],
                       by = c("Country", "FG_num", "FleetType", "Year"), all.x = TRUE)  # attach FDI's own per-fleet discard rate, where available
  n_fleet_discard <- sum(!is.na(fleet_split$discard_ratio_fleet))  # cells where the per-fleet rate applies
  fleet_split[!is.na(discard_ratio_fleet), `:=`(
    Catch_t_by_fleet   = Landings_t_by_fleet / (1 - discard_ratio_fleet),  # recompute this fleet's gross catch using its own discard rate
    Discard_t_by_fleet = Landings_t_by_fleet / (1 - discard_ratio_fleet) - Landings_t_by_fleet,
    discard_split_source = "STECF FDI's own per-fleet discard rate (Country x FG x FleetType x Year)"
  )]  # overwrite the uniform assumption wherever a real per-fleet rate exists
  fleet_split[, discard_ratio_fleet := NULL]  # drop the now-unneeded helper column
  message("\n[Discards by fleet] ", n_fleet_discard, " Country x FG x FleetType x Year cell(s) now use",
          " STECF FDI's own per-fleet discard rate instead of the country-level ratio distributed by catch share.")
}

## --- Recreational: GFCM/FDI have no recreational catch at all. Where
## SAU's own sector field (sau_sector_prop, built above) actually has a
## Recreational row for this Country x FG, use its share of SAU's non-
## recreational (Artisanal+Industrial) catch as an INFERRED proxy,
## scaled onto GFCM's own commercial total for that cell - flagged
## clearly as an estimate, not a measurement. Everywhere SAU has no
## sector field (or no Recreational row for that cell), stays
## explicitly "not estimated", same as before this change. -------------
recreational_rows <- CJ(Country = unique(FLEET_REGISTER[Sector == "Recreational"]$Country), FG_num = unique(gfcm_country_fg$FG_num), Year = YEAR_ECOPATH)  # every country x FG x Ecopath-year combination
recreational_rows <- merge(recreational_rows, FLEET_REGISTER[Sector == "Recreational", .(Country, GSA, FleetType, Comment)], by = "Country")  # attach the Recreational fleet's metadata
recreational_rows[, `:=`(Sector = "Recreational", Catch_t_by_fleet = NA_real_, Discard_t_by_fleet = NA_real_,
                         fleet_split_source = "not estimated - no source in this pipeline captures recreational catch")]  # default to "not estimated"

if (nrow(sau_sector_prop) > 0) {
  sau_recreational_ratio <- dcast(sau_sector_prop, Country + FG_num ~ Sector, value.var = "Catch_t", fun.aggregate = sum, fill = 0)  # reshape sectors into columns
  commercial_cols <- intersect(c("Artisanal", "Industrial", "Subsistence"), names(sau_recreational_ratio))  # which non-recreational sector columns exist
  if ("Recreational" %in% names(sau_recreational_ratio) && length(commercial_cols) > 0) {
    sau_recreational_ratio[, Commercial_t := rowSums(.SD), .SDcols = commercial_cols]  # total commercial catch across sectors
    sau_recreational_ratio[, recreational_ratio := fifelse(Commercial_t > 0, Recreational / Commercial_t, NA_real_)]  # recreational-to-commercial ratio
    sau_recreational_ratio <- sau_recreational_ratio[!is.na(recreational_ratio), .(Country, FG_num, recreational_ratio)]  # keep only valid ratios
    
    fg_commercial_catch <- fleet_split[, .(Catch_t_commercial = sum(Catch_t_by_fleet, na.rm = TRUE)), by = .(Country, FG_num, Year)]  # GFCM's own commercial total by country x FG x year
    recreational_rows <- merge(recreational_rows, sau_recreational_ratio, by = c("Country", "FG_num"), all.x = TRUE)  # attach the SAU ratio
    recreational_rows <- merge(recreational_rows, fg_commercial_catch, by = c("Country", "FG_num", "Year"), all.x = TRUE)  # attach the commercial total to scale against
    n_recreational_est <- sum(!is.na(recreational_rows$recreational_ratio) & !is.na(recreational_rows$Catch_t_commercial))  # cells with an estimate
    recreational_rows[!is.na(recreational_ratio) & !is.na(Catch_t_commercial), `:=`(
      Catch_t_by_fleet = Catch_t_commercial * recreational_ratio,  # infer recreational catch from the commercial total x ratio
      fleet_split_source = "inferred - SAU sector split's Recreational:Commercial ratio applied to GFCM's commercial total (proxy, not a measurement)"
    )]
    recreational_rows[, `:=`(recreational_ratio = NULL, Catch_t_commercial = NULL)]  # drop the now-unneeded helper columns
    message("\n[Recreational] ", n_recreational_est, " of ", nrow(recreational_rows), " Country x FG x Year",
            " cell(s) now carry an SAU-derived recreational-catch estimate (proxy - see fleet_split_source);",
            " the rest stay 'not estimated'.")
  }
}

fleet_split_out <- rbindlist(list(
  fleet_split[, .(Country, FG_num, FG_name, Year, Sector, FleetType, GSA, Comment,
                  Catch_t = Catch_t_by_fleet, Discard_t = Discard_t_by_fleet,
                  Catch_t_incl_unreported = Catch_t_incl_unreported * prop_fleet,
                  discard_source, discard_split_source, fleet_split_source)],
  recreational_rows[, .(Country, FG_num, Year, Sector, FleetType, GSA, Comment,
                        Catch_t = Catch_t_by_fleet, Discard_t = Discard_t_by_fleet,
                        Catch_t_incl_unreported = NA_real_, discard_source = NA_character_,
                        discard_split_source = NA_character_, fleet_split_source)]
), use.names = TRUE, fill = TRUE)  # combine commercial fleet rows and recreational rows into one output table
setorder(fleet_split_out, Country, FG_num, Year, -Catch_t)  # sort for readability
message("\n[Fleet split] fleet_split_out: ", nrow(fleet_split_out), " Country x FG x FleetType x Year row(s).")

## --- Effort hindcast for Spain/France/Italy's pre-2014 years: FDI's
## own days-per-tonne ratio (stecf_effort_catch_ratio, Country x
## FleetType) applied to that fleet's SAU-hindcasted TOTAL catch
## (summed across every FG - effort is a fleet-level activity, not a
## per-FG one, so it must use the fleet's whole catch, not any single
## FG's share of it). Combined below with STECF FDI's own real 2014+
## effort into one Country x FleetType x Year sheet for the 3 EU
## countries - Morocco/Algeria/Tunisia have no FDI ratio to build this
## from, so they keep FishMIP nom_active as their only effort source
## (see the effort step further down). ---------------------------------
catch_by_fleet_total_pre2013 <- fleet_split[Country %in% c("Spain", "France", "Italy") & Year < STECF_FDI_START_YEAR,
                                            .(Catch_t_total = sum(Catch_t_by_fleet, na.rm = TRUE)),
                                            by = .(Country, FleetType, Year)]  # each fleet's SAU-hindcasted total catch across all FGs, pre-2014
effort_hindcast_sau_by_fleet <- data.table(Country = character(), FleetType = character(), Year = integer(),
                                           Effort_days = numeric(), effort_source = character())  # placeholder, filled below if a ratio exists
if (nrow(stecf_effort_catch_ratio) > 0 && nrow(catch_by_fleet_total_pre2013) > 0) {
  effort_hindcast_sau_by_fleet <- merge(catch_by_fleet_total_pre2013, stecf_effort_catch_ratio, by = c("Country", "FleetType"))  # attach FDI's own days-per-tonne ratio
  effort_hindcast_sau_by_fleet[, `:=`(
    Effort_days = Catch_t_total * days_per_tonne,  # implied effort = catch x FDI's real days-per-tonne ratio
    effort_source = "FALLBACK: SAU-hindcasted catch x FDI's own days-per-tonne ratio (Country x FleetType, pre-2014, used only where no Rousseau calibration exists)"
  )]
  message("\n[Effort hindcast] effort_hindcast_sau_by_fleet (fallback method): ", nrow(effort_hindcast_sau_by_fleet),
          " Country x FleetType x Year row(s), ", uniqueN(effort_hindcast_sau_by_fleet$Country),
          " of the up-to-3 EU countries had a days-per-tonne ratio to apply.")
}

## PRIMARY pre-2014 method (per Andrea, 2026-09): Rousseau's Country x
## Year effort, calibrated to FDI's real level (rousseau_calibration
## above), distributed across FleetTypes using each FleetType's own
## share of catch_by_fleet_total_pre2013 - a country-year total split
## the same proportional way the fleet split itself works, since
## Rousseau carries no FleetType/Gear breakdown usable here (see the
## rousseau_effort_cy comment above). Falls back to
## effort_hindcast_sau_by_fleet, row by row, wherever a Country has no
## calibration factor or a Country x Year cell has no Rousseau value.
effort_hindcast_rousseau_by_fleet <- data.table()
if (nrow(rousseau_calibration) > 0 && nrow(catch_by_fleet_total_pre2013) > 0) {
  fleet_share <- copy(catch_by_fleet_total_pre2013)
  fleet_share[, catch_share := Catch_t_total / sum(Catch_t_total), by = .(Country, Year)]  # each fleet's share of that country x year's total pre-2014 catch
  effort_hindcast_rousseau_by_fleet <- merge(fleet_share, rousseau_effort_cy, by = c("Country", "Year"))  # attach Rousseau's country x year total
  effort_hindcast_rousseau_by_fleet <- merge(effort_hindcast_rousseau_by_fleet, rousseau_calibration, by = "Country")  # attach the FDI-anchored calibration factor
  effort_hindcast_rousseau_by_fleet[, `:=`(
    Effort_days = NomEffort * calib_factor * catch_share,  # FDI-anchored Rousseau total, split across fleets by catch share
    effort_source = "Rousseau et al. 2024 NomEffort, calibrated to FDI's real level, split by FleetType's catch share (Country x FleetType, pre-2014) - see ROUSSEAU_EFFORT_PATH comment for validation caveat"
  )]
  effort_hindcast_rousseau_by_fleet <- effort_hindcast_rousseau_by_fleet[, .(Country, FleetType, Year, Effort_days, effort_source)]
  message("[Effort hindcast] effort_hindcast_rousseau_by_fleet (PRIMARY pre-", STECF_FDI_START_YEAR, " method): ",
          nrow(effort_hindcast_rousseau_by_fleet), " Country x FleetType x Year row(s), ",
          uniqueN(effort_hindcast_rousseau_by_fleet$Country), " of the up-to-3 EU countries covered.")
}

## Combine: Rousseau-calibrated wherever it exists for a Country x
## FleetType x Year cell, the SAU-ratio fallback filling any gap
## Rousseau doesn't cover (missing calibration, or a year outside
## Rousseau's own 1950-2017 coverage).
effort_hindcast_by_fleet <- effort_hindcast_sau_by_fleet  # start from the fallback, full coverage
if (nrow(effort_hindcast_rousseau_by_fleet) > 0) {
  effort_hindcast_by_fleet <- rbindlist(list(
    effort_hindcast_rousseau_by_fleet,
    effort_hindcast_sau_by_fleet[!effort_hindcast_rousseau_by_fleet, on = .(Country, FleetType, Year)]  # only the cells Rousseau didn't cover
  ), use.names = TRUE, fill = TRUE)
  setorder(effort_hindcast_by_fleet, Country, FleetType, Year)
  message("[Effort hindcast] effort_hindcast_by_fleet: ", nrow(effort_hindcast_by_fleet),
          " Country x FleetType x Year row(s) combined - ", nrow(effort_hindcast_rousseau_by_fleet),
          " from Rousseau (primary), ", nrow(effort_hindcast_by_fleet) - nrow(effort_hindcast_rousseau_by_fleet),
          " from the SAU-ratio fallback.")
}

effort_by_fleettype_eu3 <- data.table()
if ("Effort_total_fishing_days" %in% names(stecf_fdi_effort_by_gsa)) {
  ## Effort_total_kW_fishing_days (kW x days actually fishing, not just at
  ## sea) is FDI's own capacity-weighted fishing-power total for the whole
  ## fleet-stratum - summed here alongside plain days so the per-vessel kW-
  ## days metric below (Andrea's "effort should be KW days per boat") can be
  ## built without re-deriving it from Capacity x Effort separately.
  kwdays_col <- intersect(c("Effort_total_kW_fishing_days", "Effort_total_kW_days_at_sea"),
                          names(stecf_fdi_effort_by_gsa))[1]  # find whichever kW-days column is actually present
  agg_cols <- c("Effort_total_fishing_days", if (!is.na(kwdays_col)) kwdays_col)
  stecf_effort_real_agg <- stecf_fdi_effort_by_gsa[, lapply(.SD, sum, na.rm = TRUE),
                                                   by = .(Country, FleetType, Year), .SDcols = agg_cols]  # sum effort metrics up to country x fleet x year (dropping GSA/metier detail)
  setnames(stecf_effort_real_agg, "Effort_total_fishing_days", "Effort_days")  # standardize column name
  if (!is.na(kwdays_col)) setnames(stecf_effort_real_agg, kwdays_col, "Effort_kWdays_total")  # standardize column name
  stecf_effort_real_agg[, effort_source := "STECF FDI (own effort, 2014+)"]  # tag the source of this effort figure
  keep_cols <- c("Country", "FleetType", "Year", "Effort_days", "effort_source",
                 if ("Effort_kWdays_total" %in% names(stecf_effort_real_agg)) "Effort_kWdays_total")
  effort_by_fleettype_eu3 <- rbindlist(list(
    stecf_effort_real_agg[, ..keep_cols],
    effort_hindcast_by_fleet[, .(Country, FleetType, Year, Effort_days, effort_source)]
  ), use.names = TRUE, fill = TRUE)  # combine FDI's real 2014+ effort with the pre-2014 hindcast (Rousseau-calibrated where available, SAU-ratio fallback otherwise)
  setorder(effort_by_fleettype_eu3, Country, FleetType, Year)  # sort for readability
  
  ## Join in the Capacity file's fleet-size figures where available (2014+
  ## only - Capacity has no pre-2014 SAU-hindcast counterpart, so the SAU-
  ## hindcasted rows above just get NA here) and derive average-per-vessel
  ## ratios - NOT Effort_days x Capacity_total_vessels, which would double-
  ## count every vessel's own days (Effort_days is already fleet-summed).
  ## Effort_kWdays_per_vessel is the primary requested effort metric - a
  ## fleet's total capacity-weighted fishing effort (kW x days) divided by
  ## its own vessel count, i.e. KW-days per boat - comparable across years
  ## even as the fleet's vessel count itself changes.
  if (nrow(stecf_fdi_capacity_by_fleet) > 0) {
    effort_by_fleettype_eu3 <- merge(effort_by_fleettype_eu3, stecf_fdi_capacity_by_fleet,
                                     by = c("Country", "FleetType", "Year"), all.x = TRUE)  # attach fleet-size figures from the Capacity file
    if ("Capacity_total_vessels" %in% names(effort_by_fleettype_eu3)) {
      effort_by_fleettype_eu3[, Avg_days_per_vessel := fifelse(Capacity_total_vessels > 0, Effort_days / Capacity_total_vessels, NA_real_)]  # average fishing days per vessel
    }
    if (all(c("Capacity_total_kW", "Capacity_total_vessels") %in% names(effort_by_fleettype_eu3))) {
      effort_by_fleettype_eu3[, Avg_kW_per_vessel := fifelse(Capacity_total_vessels > 0, Capacity_total_kW / Capacity_total_vessels, NA_real_)]  # average engine power per vessel
    }
    if (all(c("Capacity_total_GT", "Capacity_total_vessels") %in% names(effort_by_fleettype_eu3))) {
      effort_by_fleettype_eu3[, Avg_GT_per_vessel := fifelse(Capacity_total_vessels > 0, Capacity_total_GT / Capacity_total_vessels, NA_real_)]  # average tonnage per vessel
    }
    if (all(c("Effort_kWdays_total", "Capacity_total_vessels") %in% names(effort_by_fleettype_eu3))) {
      effort_by_fleettype_eu3[, Effort_kWdays_per_vessel := fifelse(Capacity_total_vessels > 0, Effort_kWdays_total / Capacity_total_vessels, NA_real_)]  # average kW-days per vessel
    }
    message("[Effort hindcast] Joined Capacity_* columns and Avg_days_per_vessel/Avg_kW_per_vessel/",
            "Avg_GT_per_vessel/Effort_kWdays_per_vessel into effort_by_fleettype_eu3 (2014+ rows only -",
            " Capacity has no pre-2014 SAU counterpart, so pre-2014 rows keep NA for all of these).")
  }
  
  ## --- PRIMARY EFFORT metric, per Andrea's own spec: EFFORT = kW x days x
  ## n_boats - i.e. capacity-weighted fishing power, aggregated across the
  ## WHOLE fleet (not divided down to a per-vessel figure). This is exactly
  ## `Effort_kWdays_total` above (FDI's own `total_kW_fishing_days`,
  ## Sum_i(kW_i x days_i) over every vessel i in the Country x FleetType x
  ## Year stratum) - already in kW*days*nboats units, nothing further to
  ## construct. The Avg_*_per_vessel/Effort_kWdays_per_vessel columns above
  ## remain as secondary per-vessel diagnostics, not the headline Effort
  ## figure. FishMIP's `nom_active_kWdays` (built further down, the only
  ## effort source for Morocco/Algeria/Tunisia) is in the same units.
  ##
  ## Technology creep correction on top of it. Raw kW*days reflects each
  ## vessel's fishing power AT THE TIME it fished - the same nominal kW*days
  ## catches more today than it did decades ago because of better gear,
  ## sonar, engines, etc. `tech_creep_multiplier()` (defined above, with
  ## TECH_CREEP_COUNTRY_MULTIPLIER) gives the EFFECTIVE (today's-
  ## technology-equivalent) multiplier for a given Country x Year, varying
  ## BOTH by country (EU-3 vs Morocco/Algeria/Tunisia) and by year
  ## (decelerating the further a year sits from TECH_CREEP_BASE_YEAR) -
  ## see that function's own comment for the full derivation. This is an
  ## ASSUMPTION-DRIVEN adjustment (no fishery-specific creep rate exists for
  ## this pipeline), applied as a separate `Effort_kWdays_total_effective`
  ## column so the raw reported total above is never overwritten.
  if ("Effort_kWdays_total" %in% names(effort_by_fleettype_eu3)) {
    effort_by_fleettype_eu3[, creep_mult := tech_creep_multiplier(Country, Year)]  # compute the technology-creep multiplier per row
    effort_by_fleettype_eu3[, Effort_kWdays_total_effective := Effort_kWdays_total * creep_mult]  # apply it to the primary effort metric
    if ("Effort_kWdays_per_vessel" %in% names(effort_by_fleettype_eu3)) {
      effort_by_fleettype_eu3[, Effort_kWdays_per_vessel_effective := Effort_kWdays_per_vessel * creep_mult]  # apply it to the per-vessel metric too
    }
    effort_by_fleettype_eu3[, creep_mult := NULL]  # drop the now-unneeded helper column
    creep_range <- effort_by_fleettype_eu3[!is.na(Effort_kWdays_total_effective),
                                           .(pct = round(100 * (Effort_kWdays_total_effective / Effort_kWdays_total - 1), 1)), by = .(Country, Year)]  # % change from the raw figure, for reporting
    message("[Effort hindcast] Technology-creep correction applied to the PRIMARY effort metric:",
            " Effort_kWdays_total_effective = Effort_kWdays_total (kW x days x nboats) x tech_creep_multiplier(Country, Year)",
            " (same multiplier also applied to the secondary per-vessel figure, as Effort_kWdays_per_vessel_effective).",
            " Varies by country (EU-3 multiplier 1, Morocco/Algeria/Tunisia multiplier 0.7 - see",
            " TECH_CREEP_COUNTRY_MULTIPLIER) and by year (decelerating year-by-year increments from",
            " Palomares & Pauly 2019's C% = 13.8 x y^-0.511, y = years elapsed since TECH_CREEP_BASE_YEAR = ",
            TECH_CREEP_BASE_YEAR, "). Cumulative effect ranges from ", min(creep_range$pct), "% to ",
            max(creep_range$pct), "% across the covered years. This is an ASSUMPTION (no fishery-specific",
            " creep rate is available), applied only where FDI's own kW-days figure exists (2014+); pre-2014",
            " hindcasted rows are left uncorrected since they carry no kW-days figure to begin with.")
  }
  
  fwrite(effort_by_fleettype_eu3, file.path(out_dir, "effort_by_fleettype_eu3_hindcast.csv"))  # write result to CSV
  message("[Effort hindcast] effort_by_fleettype_eu3: ", nrow(effort_by_fleettype_eu3), " Country x FleetType x",
          " Year row(s) for Spain/France/Italy (FDI's own effort 2014+, SAU-hindcasted before that) - written",
          " to effort_by_fleettype_eu3_hindcast.csv / Effort_by_FleetType_EU3. FishMIP's Fishing_Effort_by_Fleet",
          " below remains the only effort source for Morocco/Algeria/Tunisia (no FDI basis to hindcast from).")
}

## =================================================================
## # create catches for Ecopath by fleet in 1994:1996 (average)
## Same t/km^2/year density unit as Catches_Ecopath above (and Biomass) -
## converted here using the SAME Total_Area_km2, so an FG's fleet-split
## columns sum back to its Catches_Ecopath total (they're two views of
## the same underlying catch, one FG-only, one FG x Fleet). Recreational
## is INCLUDED wherever recreational_rows above actually has an
## SAU-derived estimate for that Country x FG x Year cell (fixed 2026-09
## - this used to exclude every Recreational row unconditionally, even
## cells that DID carry a real estimate, on a comment claiming none
## existed anywhere in the pipeline; that comment was stale). Cells still
## flagged "not estimated" (Catch_t is NA) are still dropped here, same
## as before - only cells with a real number are added in.
## =================================================================
ecopath_fleet_long <- fleet_split_out[Year %in% YEAR_ECOPATH & (Sector != "Recreational" | !is.na(Catch_t)),
                                      .(Catch_t_avg = mean(Catch_t, na.rm = TRUE)), by = .(Country, FG_num, FG_name, Sector, FleetType)]  # average catch by country x FG x fleet over the Ecopath snapshot years, Recreational included where an SAU-derived estimate exists
ecopath_fleet_long[, `:=`(Fleet = paste(Country, FleetType, sep = " - "),
                          Catch_t_km2_avg = Catch_t_avg / Total_Area_km2)]  # build the Fleet label and convert catch to density

catches_ecopath_by_fleet_wide <- dcast(ecopath_fleet_long, FG_num + FG_name ~ Fleet,
                                       value.var = "Catch_t_km2_avg", fill = 0)  # reshape to one column per fleet

## Complete to EVERY FG in full_fg_list, not just the ones some fleet
## actually caught something of - a FG with no fleet catch at all still
## needs its row, with 0 across every fleet column, rather than being
## silently absent from the sheet.
n_fg_before_complete <- nrow(catches_ecopath_by_fleet_wide)  # row count before completing to every FG
catches_ecopath_by_fleet_wide <- merge(full_fg_list, catches_ecopath_by_fleet_wide, by = c("FG_num", "FG_name"), all.x = TRUE)  # add rows for FGs with no fleet catch at all
fleet_cols <- setdiff(names(catches_ecopath_by_fleet_wide), c("FG_num", "FG_name"))  # every fleet column
for (cc in fleet_cols) catches_ecopath_by_fleet_wide[is.na(get(cc)), (cc) := 0]  # fill missing fleet catch with explicit 0
setorder(catches_ecopath_by_fleet_wide, FG_num)  # sort by FG number
message("\n[Ecopath by fleet] catches_ecopath_by_fleet_wide: completed from ", n_fg_before_complete, " to ",
        nrow(catches_ecopath_by_fleet_wide), " FG(s) (every FG in full_fg_list, 0 where no fleet catch) x ",
        length(fleet_cols), " fleet(s) (t/km^2/year), averaged over ", paste(range(YEAR_ECOPATH), collapse = "-"),
        " (e.g. FG x 'Spain - Bottom trawls', 'France - Drifting longlines', ...).")

## Fleet_Structure - each fleet's ACTUAL share of its FG's total catch
## (across all 6 target countries combined, not per-country), from the
## real catch amounts above rather than re-derived from fleet_prop's
## per-country gear shares - so this is the genuine "how much of FG X's
## catch is Spain's trawlers vs. France's longliners" breakdown.
fleet_structure_out <- ecopath_fleet_long[, .(FG_num, FG_name, Fleet, Catch_t_avg)]  # start from the average catch by FG x fleet
fleet_structure_out[, prop_catch := ifelse(sum(Catch_t_avg, na.rm = TRUE) > 0,
                                           Catch_t_avg / sum(Catch_t_avg, na.rm = TRUE), NA_real_), by = FG_num]  # convert to each fleet's share of the FG's total catch
setorder(fleet_structure_out, FG_num, -prop_catch)  # sort by FG, largest fleet share first
message("[Fleet_Structure] ", nrow(fleet_structure_out), " FG x Fleet row(s), prop_catch sums to 1 within",
        " each FG_num (across all 6 target countries' fleets combined).")

## =================================================================
## # obtain F for species in Ecopath years
## F = Yield / Biomass, at FG resolution, averaged over YEAR_ECOPATH,
## using the SAME Total_Area_km2 and the SAME full-West-Med catch total
## (catches_discards_fg) as Catches_Ecopath above - F, Catches_Ecopath
## and Biomass's own Ecopath sheet are therefore all mutually consistent
## (same area, same catch scope). Needs Step 1's species_density_
## regional_combined.csv for the biomass side - skipped with a message,
## not guessed, if that one file is missing (Total_Area_km2 itself is
## already required above, so only the density file is checked here).
## =================================================================
f_by_fg <- data.table()
if (!file.exists(SPECIES_DENSITY_PATH)) {
  message("\n[F] Skipped - '", SPECIES_DENSITY_PATH, "' not found. Run Step 1",
          " (01_survey_density_westmed.R / 01_survey_density_custom.R) first if you want F computed here.")
} else {
  species_density <- fread(SPECIES_DENSITY_PATH)   # Year, FG_num, FG_name, ScientificName, mean_density (t/km2)
  req_density_cols <- c("Year", "FG_num", "FG_name", "ScientificName", "mean_density")
  if (!all(req_density_cols %in% names(species_density))) stop("[F] species_density_regional_combined.csv missing expected column(s): ", paste(setdiff(req_density_cols, names(species_density)), collapse = ", "))
  
  fg_density_year <- species_density[Year %in% YEAR_ECOPATH, .(Biomass_density = sum(mean_density, na.rm = TRUE)), by = .(Year, FG_num, FG_name)]  # sum species density up to FG level, by year
  fg_biomass_avg <- fg_density_year[, .(Biomass_density_avg = mean(Biomass_density, na.rm = TRUE)), by = .(FG_num, FG_name)]  # average biomass density over the Ecopath years
  
  fg_catch_avg <- catches_discards_fg[Year %in% YEAR_ECOPATH, .(FG_num, FG_name, Year, Catch_t)]  # catch in the Ecopath years
  fg_catch_avg <- fg_catch_avg[, .(Catch_t_avg = mean(Catch_t, na.rm = TRUE)), by = .(FG_num, FG_name)]  # average catch over the Ecopath years
  
  f_by_fg <- merge(fg_biomass_avg, fg_catch_avg, by = c("FG_num", "FG_name"), all.x = TRUE)  # pair up biomass and catch by FG
  f_by_fg[, `:=`(
    Catch_density_avg = Catch_t_avg / Total_Area_km2,  # catch as a density, matching biomass's units
    F = (Catch_t_avg / Total_Area_km2) / Biomass_density_avg  # fishing mortality = catch density / biomass density
  )]
  message("[F] F_by_FG (", paste(range(YEAR_ECOPATH), collapse = "-"), " average): ", nrow(f_by_fg), " FG(s).",
          " F undefined (NA/Inf) where Catch_t_avg is 0 (no GFCM catch matched to that FG for those years)",
          " or Biomass_density_avg is 0/NA (FG not observed in the survey).")
}

## =================================================================
## # obtain fishing effort
## Time series of fishing effort BY FLEET (country x gear), not by FG -
## FishMIP nom_active, the only effort source in this script (no
## EMODnet anywhere - dropped entirely, see this file's header).
## Uses FishMIP's OWN saup country code + gear string directly (top
## N_TOP_GEARS gears per the usual convention, rest folded into "Other
## gear") - no FISHMIP_FG_CROSSWALK_PATH needed for this deliverable at
## all, since it was only ever needed to translate FishMIP's f_group
## scheme onto FG_num, and effort-by-fleet never goes through FG_num.
## =================================================================
effort_by_fleet <- data.table()
if (!requireNamespace("arrow", quietly = TRUE)) {
  message("\n[Effort] Package 'arrow' not installed - no effort output.")
} else if (!file.exists(FISHMIP_EFFORT_PARQUET)) {
  message("\n[Effort] FishMIP effort parquet not found at '", FISHMIP_EFFORT_PARQUET, "' - no effort output.")
} else {
  effort <- as.data.table(arrow::read_parquet(FISHMIP_EFFORT_PARQUET))  # load FishMIP's effort data
  effort[, year := as.integer(year)]  # coerce year to integer
  effort[, country := FISHMIP_SAUP_TO_COUNTRY[as.character(saup)]]  # translate FishMIP's numeric country code to a country name
  effort <- effort[!is.na(country) & year >= START_YEAR & year <= END_YEAR]  # keep only target countries within the study period
  
  top_effort_gears <- effort[, .(g_tot = sum(nom_active, na.rm = TRUE)), by = gear][order(-g_tot)][seq_len(min(N_TOP_GEARS, .N)), gear]  # the N_TOP_GEARS gears with the most effort
  effort[, gear_grp := ifelse(gear %in% top_effort_gears, gear, "Other gear")]  # fold minor gears into "Other gear"
  
  ## FishMIP's own `sector` column (Industrial/Artisanal) was previously
  ## dropped entirely here - Fleet was built from country x gear alone,
  ## so Morocco/Algeria/Tunisia's artisanal effort was never separated
  ## out, just silently mixed into whichever gear bucket it fell under.
  ## Andrea flagged this as a real gap (2026-09) - fixed by carrying
  ## `sector` into the Fleet label, same convention as FLEET_REGISTER's
  ## own Sector field for the catch side.
  effort[, Fleet := paste0(country, " - ", gear_grp, " - ", sector)]  # build the Fleet label, now including FishMIP's own Artisanal/Industrial split
  effort_by_fleet <- effort[, .(nom_active_kWdays = sum(nom_active, na.rm = TRUE)),
                            by = .(Fleet, Country = country, gear = gear_grp, Sector = sector, Year = year)]  # sum effort by fleet x year
  setorder(effort_by_fleet, Country, Sector, gear, Year)  # sort for readability
  
  ## Same tech_creep_multiplier() as effort_by_fleettype_eu3 above,
  ## applied here too since `nom_active_kWdays` is FishMIP's own kW*days
  ## (x nboats, aggregated) effort figure - the same units/convention as
  ## FDI's Effort_kWdays_total, and the ONLY effort source at all for
  ## Morocco/Algeria/Tunisia, so it needs the same country x year varying
  ## "effective fishing power" adjustment to be comparable across this
  ## time series (this is also where TECH_CREEP_COUNTRY_MULTIPLIER's
  ## Morocco/Algeria/Tunisia = 0.7 reduced rate actually applies, since the
  ## EU-3's own effort_by_fleettype_eu3 block above never reaches this one).
  effort_by_fleet[, nom_active_kWdays_effective := nom_active_kWdays * tech_creep_multiplier(Country, Year)]  # apply the technology-creep correction
  fishmip_creep_range <- effort_by_fleet[!is.na(nom_active_kWdays_effective),
                                         .(pct = round(100 * (nom_active_kWdays_effective / nom_active_kWdays - 1), 1)), by = .(Country, Year)]  # % change from the raw figure, for reporting
  message("[Effort] Fishing_Effort_by_Fleet: ", uniqueN(effort_by_fleet$Fleet), " fleet(s) (country x top ",
          N_TOP_GEARS, " gears + 'Other gear'), ", nrow(effort_by_fleet), " Fleet x Year row(s). Max FishMIP",
          " year: ", max(effort$year), " (hard ceiling, not extrapolated). Technology-creep correction added",
          " as nom_active_kWdays_effective = nom_active_kWdays x tech_creep_multiplier(Country, Year)",
          " (varies by country - EU-3 multiplier 1, Morocco/Algeria/Tunisia multiplier 0.7 - and by year,",
          " decelerating from TECH_CREEP_BASE_YEAR = ", TECH_CREEP_BASE_YEAR, "). Cumulative effect ranges",
          " from ", min(fishmip_creep_range$pct), "% to ", max(fishmip_creep_range$pct), "% across the",
          " covered years - same ASSUMPTION-DRIVEN adjustment as effort_by_fleettype_eu3, not a measurement.")
  
  ## Rousseau NomEffort added as a SECOND, independent effort figure per
  ## Country x Year (not replacing FishMIP - SAU itself has no effort
  ## variable for these "other" non-EU countries, so Rousseau is the only
  ## candidate second source here; per Andrea's direction to bring
  ## Rousseau in for the non-EU side too). Country x Year only, joined
  ## onto every Fleet row for that country x year so it reads alongside
  ## FishMIP's fleet-level breakdown - NOT split by gear/sector itself
  ## (Rousseau's Gear scheme doesn't map onto this script's fleet
  ## taxonomy, same limitation as the EU-3 hindcast above). The two
  ## sources track each other's SHAPE reasonably well for these 3
  ## countries (Algeria r=0.98, Tunisia r=0.98, Morocco r=0.82 across
  ## 1950-2017) but differ ~5-10x in absolute magnitude - kept as a
  ## side-by-side comparison column, not blended into nom_active_kWdays,
  ## since there's no ground truth here to say which scale is "right".
  if (nrow(rousseau_effort_cy) > 0) {
    effort_by_fleet <- merge(effort_by_fleet, rousseau_effort_cy, by = c("Country", "Year"), all.x = TRUE)
    setnames(effort_by_fleet, "NomEffort", "rousseau_nom_effort_country_year")
    message("[Effort] Rousseau NomEffort joined as rousseau_nom_effort_country_year (Country x Year, same value",
            " repeated across every Fleet row for that country/year) - a comparison figure only, not blended",
            " into nom_active_kWdays. NA where Rousseau's own 1950-2017 coverage doesn't reach (", END_YEAR,
            "'s tail years, same as FishMIP's own ceiling).")
  }
}

## =================================================================
## Algeria real trawl effort (2026-09, per Andrea's uploaded workbook -
## see MOROCCO_ALGERIA_DIR comment near the top of this script).
## Belhabib, Pauly, Harper & Zeller (2012), Table 5: REAL, MEASURED
## annual trawl-fleet hours-at-sea, 1994-2010 - explicitly flagged in the
## source workbook as "the single best genuine, measured, multi-year
## fishing-effort series found anywhere in this whole research project,
## for any gear, in either country". This overrides FishMIP's own
## nom_active-derived effort for Algeria's trawl fleet(s) over the years
## it covers, the same way STECF FDI's own effort overrides FishMIP for
## the EU-3 - a real measurement beats a modeled estimate wherever both
## exist. Morocco has no equivalent real effort series (its own sheet is
## boat-count anchor points requiring interpolation, not a direct annual
## series) and Algeria's OTHER fleets (purse seine, artisanal, etc.)
## have no real series either - both keep FishMIP nom_active as before.
##
## Hours -> kW-days conversion: no fleet-wide power figure is tabulated
## in the source; a single reference vessel (368 kW, 20m trawler,
## Bouaicha 2011) is named elsewhere in it as a usable stand-in average
## trawler power (same figure Fleet_Register_Capacity.csv uses for
## Algeria's bottom-trawl fleet, for the same reason - no per-vessel
## power data exists). kW-days = Number of hours x 368 kW / 24.
## =================================================================
dza_effort_trawl <- safe_fread_optional(file.path(MOROCCO_ALGERIA_DIR, "DZA_Effort_Trawl.csv"), "DZA_Effort_Trawl.csv")
ALGERIA_TRAWL_REFERENCE_KW <- 368  # Bouaicha (2011) reference vessel, 20m trawler - see comment above
if (nrow(dza_effort_trawl) > 0 && nrow(effort_by_fleet) > 0) {
  dza_effort_trawl[, kWdays_real := `Number of hours` * ALGERIA_TRAWL_REFERENCE_KW / 24]  # convert measured hours to kW-days via the single reference vessel
  
  ## Algeria's trawl-labeled FishMIP gear bucket(s), whatever this run's
  ## top-N-gears grouping happened to produce - matched by name-keyword
  ## rather than an exact string, since "gear_grp" is FishMIP's own gear
  ## vocabulary and can't be hardcoded here. If more than one bucket
  ## matches, the real total is distributed across them in proportion to
  ## their EXISTING FishMIP shares (same "recalibrate the total, keep the
  ## internal structure" convention as the SAU-vs-FDI hindcast calibration
  ## above) rather than picked arbitrarily or split evenly.
  dza_trawl_fleets <- effort_by_fleet[Country == "Algeria" & grepl("trawl", gear, ignore.case = TRUE)]
  if (nrow(dza_trawl_fleets) == 0) {
    message("\n[Morocco/Algeria effort] DZA_Effort_Trawl.csv found, but no Algeria trawl-labeled gear bucket",
            " exists in this run's effort_by_fleet (check FishMIP's own gear naming) - real trawl effort NOT applied.")
  } else {
    dza_trawl_shares <- dza_trawl_fleets[, .(share = sum(nom_active_kWdays, na.rm = TRUE)), by = gear]  # each trawl bucket's own share of Algeria's trawl total, this run
    dza_trawl_shares[, share := share / sum(share)]
    n_dza_trawl_override <- 0L
    for (g in dza_trawl_shares$gear) {
      g_share <- dza_trawl_shares[gear == g, share]
      real_g <- dza_effort_trawl[, .(Year, nom_active_kWdays_real = kWdays_real * g_share)]  # this bucket's share of the real measured total
      before_n <- effort_by_fleet[Country == "Algeria" & gear == g & Year %in% real_g$Year, .N]
      effort_by_fleet[real_g, on = c("Year"), `:=`(
        nom_active_kWdays = fifelse(Country == "Algeria" & gear == g, i.nom_active_kWdays_real, nom_active_kWdays)
      ), by = .EACHI]  # overwrite with the real measured figure for this bucket x year
      n_dza_trawl_override <- n_dza_trawl_override + before_n
    }
    ## re-derive the tech-creep-adjusted column from the now-corrected
    ## raw figure, and re-tag the years actually overridden - the real
    ## measured hours ALREADY reflect whatever technology-creep really
    ## happened in Algeria's trawl fleet over 1994-2010, so the assumption-
    ## driven multiplier is redundant (and double-counting) for exactly
    ## these rows; kept at 1 for them rather than removed from the column
    ## entirely, so nom_active_kWdays_effective stays populated everywhere.
    effort_by_fleet[Country == "Algeria" & gear %in% dza_trawl_shares$gear & Year %in% dza_effort_trawl$Year,
                    nom_active_kWdays_effective := nom_active_kWdays]
    message("\n[Morocco/Algeria effort] Algeria trawl fleet: ", n_dza_trawl_override, " Fleet x Year row(s) across ",
            nrow(dza_trawl_shares), " FishMIP gear bucket(s) now use Belhabib et al.'s real measured trawl-hours",
            " (converted via a single ", ALGERIA_TRAWL_REFERENCE_KW, " kW reference vessel - see comment above)",
            " instead of FishMIP's nom_active, years ", min(dza_effort_trawl$Year), "-", max(dza_effort_trawl$Year), ".")
  }
} else {
  message("\n[Morocco/Algeria effort] DZA_Effort_Trawl.csv not found (or no FishMIP effort loaded this run) -",
          " Algeria's trawl fleet keeps FishMIP nom_active as its only effort source, same as before.")
}

## =================================================================
## Rousseau et al. (2024) independent fleet-effort database, Morocco/
## Algeria (2026-09, per Andrea's uploaded workbook). Written out as a
## cross-check series only - NOT wired into TECH_CREEP_COUNTRY_MULTIPLIER
## or nom_active_kWdays above - because its own "Effective effort,
## linear-creep-adjusted (kW-days)" column is wildly implausible as
## extracted (in some rows, 100-1000x its own "Nominal effort (kW-days)"
## for the SAME year, which is not achievable by any realistic annual
## creep rate compounded over a 1994-2010 window - this smells like a
## units/compounding error in how that column was originally computed,
## not a real technology-creep effect). Andrea/Daniel should sanity-
## check rousseau_effort_review.csv against the source repository
## directly before using this column for anything; the "Nominal effort"
## and "Active-vessel effort" columns look internally consistent and are
## kept for reference alongside it. TECH_CREEP_COUNTRY_MULTIPLIER's own
## Morocco/Algeria/Tunisia = 0.7 figure (see its definition above) is
## therefore UNCHANGED by this workbook, still a flagged judgment call,
## not replaced by a real figure - Rousseau's per-gear NOMINAL effort
## trend could still support deriving a real creep rate later (comparing
## nom_active_kWdays' own year-over-year growth to Rousseau's), just not
## via this specific pre-computed column.
## Note also (per the source workbook's own caveat): Rousseau_Effort_MAR
## is Morocco NATIONWIDE (Atlantic + Mediterranean combined, no sub-
## national split available in that database) - never mix it with the
## Mediterranean-only Belhabib figures above without an explicit
## Mediterranean-share adjustment. Rousseau_Effort_DZA needs no such
## caveat (Algeria has no Atlantic coast).
## =================================================================
rousseau_mar <- safe_fread_optional(file.path(MOROCCO_ALGERIA_DIR, "Rousseau_Effort_MAR.csv"), "Rousseau_Effort_MAR.csv")
rousseau_dza <- safe_fread_optional(file.path(MOROCCO_ALGERIA_DIR, "Rousseau_Effort_DZA.csv"), "Rousseau_Effort_DZA.csv")
if (nrow(rousseau_mar) > 0 || nrow(rousseau_dza) > 0) {
  rousseau_effort_review <- rbindlist(list(
    if (nrow(rousseau_mar) > 0) rousseau_mar[, Country := "Morocco (NATIONWIDE - see caveat above, not Med-only)"] else NULL,
    if (nrow(rousseau_dza) > 0) rousseau_dza[, Country := "Algeria (Mediterranean-only, no caveat)"] else NULL
  ), use.names = TRUE, fill = TRUE)
  rousseau_effort_review[, implied_creep_ratio := `Effective effort, linear-creep-adjusted (kW-days)` / `Nominal effort (kW-days)`]  # flagged as implausible, see comment above - kept for Andrea/Daniel to inspect directly
  fwrite(rousseau_effort_review, file.path(out_dir, "rousseau_effort_review.csv"))
  message("\n[Morocco/Algeria effort] Rousseau et al. (2024) effort database written to rousseau_effort_review.csv",
          " (", nrow(rousseau_effort_review), " Year x Sector x Gear row(s)) as a CROSS-CHECK only - its own",
          " implied_creep_ratio column looks implausible (see comment above) and is NOT used to replace",
          " TECH_CREEP_COUNTRY_MULTIPLIER's existing Morocco/Algeria/Tunisia = 0.7 judgment-call figure.")
} else {
  message("\n[Morocco/Algeria effort] No Rousseau_Effort_MAR.csv/Rousseau_Effort_DZA.csv found - no cross-check",
          " written; TECH_CREEP_COUNTRY_MULTIPLIER stays exactly as before.")
}

## =================================================================
## # GFCM Stock Assessment Forms (SAFs) - possible internal-report effort
## for Morocco/Algeria/Tunisia (GSA 1-3, GSA 4, GSA 12 respectively)
## FishMIP's nom_active above is the only BULK-downloadable effort source
## for these 3 non-EU countries, but GFCM's own per-stock Stock Assessment
## Forms (published yearly per GSA at fao.org/gfcm's stock-assessment
## pages) DO typically include a fleet-level effort figure for the stock's
## assessed GSA(s) - this is real data, but it comes as individual PDF/XLS
## reports per stock, not a bulk export this script can fetch automatically.
## Flagged manual-entry placeholder ONLY, same convention as
## GFCM_TASK2_CATCH_MANUAL/BYCATCH_RATE_MANUAL above - fill in by hand
## (Country/GSA/FleetType/Year/Effort_days/source_citation, e.g. the SAF's
## own stock code and publication year) if a specific stock's SAF is ever
## pulled for one of these countries; everywhere else stays explicitly
## "not available", not silently defaulted to FishMIP.
## =================================================================
GFCM_SAF_EFFORT_MANUAL <- data.table(
  Country = character(), GSA = character(), FleetType = character(),
  Year = integer(), Effort_days = numeric(), source_citation = character()
)
gfcm_saf_effort_placeholder <- unique(FLEET_REGISTER[Country %in% c("Morocco", "Algeria", "Tunisia"),
                                                     .(Country, GSA, FleetType)])  # one row per non-EU country x GSA x fleet
if (nrow(GFCM_SAF_EFFORT_MANUAL) > 0) {
  gfcm_saf_effort_placeholder <- merge(gfcm_saf_effort_placeholder, GFCM_SAF_EFFORT_MANUAL,
                                       by = c("Country", "GSA", "FleetType"), all.x = TRUE)  # attach any manually-entered figures
} else {
  gfcm_saf_effort_placeholder[, `:=`(Year = NA_integer_, Effort_days = NA_real_, source_citation = NA_character_)]  # no manual entries at all - leave empty
}
gfcm_saf_effort_placeholder[, data_status := fifelse(is.na(Effort_days),
                                                     "not available - GFCM Stock Assessment Forms are per-stock PDF/XLS reports, no bulk export exists; FishMIP nom_active above remains the effort figure actually used",
                                                     "manually entered from a GFCM SAF - see source_citation")]  # flag whether each row is filled or not
message("\n[GFCM SAF] ", sum(!is.na(gfcm_saf_effort_placeholder$Effort_days)), " of ", nrow(gfcm_saf_effort_placeholder),
        " Country x GSA x FleetType cell(s) filled from GFCM_SAF_EFFORT_MANUAL; the rest flagged 'not available'",
        " (Morocco/Algeria/Tunisia keep FishMIP nom_active as the effort figure this pipeline actually uses -",
        " this placeholder only tracks whether a real GFCM SAF has been manually pulled in for cross-checking).")

## =================================================================
## # append to the ecopath_ecosim_inputs spreadsheet
## upsert_workbook_sheets()/finalize_workbook_sheet_order() come from
## the sourced lib_survey_fg_density_functions.R (see CONFIGURATION
## above) - Catches_Ecopath/Catches_Ecosim were already written
## earlier by add_catches_to_ecopath_workbook() itself; everything else
## this script produces is written here in one pass.
## =================================================================
fwrite(gfcm_species_division_fg, file.path(out_dir, paste0("gfcm_catches_by_species_year_division_", DATASET_VERSION, ".csv")))  # re-write, in case anything changed above
fwrite(gfcm_catches_by_area, file.path(out_dir, paste0("gfcm_catches_by_country_division_year_", DATASET_VERSION, ".csv")))  # re-write, in case anything changed above
fwrite(fleet_vs_division_check, file.path(out_dir, "fleet_definition_vs_gfcm_division_check.csv"))  # re-write, in case anything changed above
fwrite(fg_catch_timeseries, file.path(out_dir, paste0("catches_by_FG_timeseries_", DATASET_VERSION, ".csv")))  # write the FG-level catch timeseries
fwrite(catches_discards_fg, file.path(out_dir, paste0("catches_and_discards_by_FG_timeseries_", DATASET_VERSION, ".csv")))  # write the FG-level catch+discards timeseries
fwrite(fleet_split_out, file.path(out_dir, "catches_by_country_fleet_sector_year.csv"))  # write the full fleet-split table
fwrite(bycatch_placeholder, file.path(out_dir, "bycatch_placeholder.csv"))  # write the bycatch placeholder table
fwrite(gfcm_task2_placeholder, file.path(out_dir, "gfcm_task2_catch_placeholder.csv"))  # write the GFCM Task 2 placeholder table
fwrite(gfcm_saf_effort_placeholder, file.path(out_dir, "gfcm_saf_effort_placeholder.csv"))  # write the GFCM SAF placeholder table
if (nrow(stecf_fdi_catch_by_gsa) > 0) fwrite(stecf_fdi_catch_by_gsa, file.path(out_dir, "stecf_fdi_catch_by_gsa_gear_year.csv"))  # re-write, if available
if (nrow(stecf_fdi_effort_by_gsa) > 0) fwrite(stecf_fdi_effort_by_gsa, file.path(out_dir, "stecf_fdi_effort_by_gsa_gear_year.csv"))  # re-write, if available
fwrite(catches_ecopath_by_fleet_wide, file.path(out_dir, "catches_ecopath_by_fleet_wide.csv"))  # write the Ecopath-by-fleet snapshot table
fwrite(fleet_structure_out, file.path(out_dir, "fleet_structure.csv"))  # write the Fleet_Structure table
if (nrow(effort_by_fleet) > 0) fwrite(effort_by_fleet, file.path(out_dir, "fishing_effort_by_fleet_timeseries_FishMIP.csv"))  # write the FishMIP effort table, if available
if (nrow(f_by_fg) > 0) fwrite(f_by_fg, file.path(out_dir, "F_by_FG_ecopath_years.csv"))  # write the fishing-mortality table, if available
if (nrow(unreported_by_country) > 0) fwrite(unreported_by_country, file.path(out_dir, "unreported_pct_by_country.csv"))  # write the unreported-ratio table, if available

sheets_to_write <- list(
  Catches_Species_Division_FG   = gfcm_species_division_fg,
  GFCM_Catches_by_Division      = gfcm_catches_by_area,
  Fleet_vs_Division_Check       = fleet_vs_division_check,
  Catches_ByCountryFleetSector  = fleet_split_out,
  Bycatch_Placeholder           = bycatch_placeholder,
  GFCM_Task2_Placeholder        = gfcm_task2_placeholder,
  GFCM_SAF_Effort_Placeholder   = gfcm_saf_effort_placeholder,
  Catches_Ecopath_ByFleet       = catches_ecopath_by_fleet_wide,
  Fleet_Structure               = fleet_structure_out
)
if (nrow(unreported_by_country) > 0) sheets_to_write$Unreported_Pct_by_Country <- unreported_by_country  # add this sheet only if it has data
if (nrow(effort_by_fleet) > 0) sheets_to_write$Fishing_Effort_by_Fleet <- effort_by_fleet  # add this sheet only if it has data
if (nrow(f_by_fg) > 0) sheets_to_write$F_by_FG_EcopathYears <- f_by_fg  # add this sheet only if it has data
if (nrow(stecf_fdi_catch_by_gsa) > 0) sheets_to_write$STECF_FDI_Catch_by_GSA <- stecf_fdi_catch_by_gsa  # add this sheet only if it has data
if (nrow(stecf_fdi_effort_by_gsa) > 0) sheets_to_write$STECF_FDI_Effort_by_GSA <- stecf_fdi_effort_by_gsa  # add this sheet only if it has data
if (nrow(effort_by_fleettype_eu3) > 0) sheets_to_write$Effort_by_FleetType_EU3 <- effort_by_fleettype_eu3  # add this sheet only if it has data
if (nrow(stecf_fdi_capacity_by_fleet) > 0) sheets_to_write$STECF_FDI_Capacity_by_Fleet <- stecf_fdi_capacity_by_fleet  # add this sheet only if it has data
if (nrow(discard_calibration) > 0) sheets_to_write$SAU_STECF_Discard_Calibration <- discard_calibration  # add this sheet only if it has data

upsert_workbook_sheets(sheets_to_write, ECOPATH_WORKBOOK_PATH)  # write/replace all these sheets in the workbook

## Fix sheet order/names last, same order-independent convention every
## other script that touches this workbook uses (see this function's
## own header comment in lib_survey_fg_density_functions.R) - safe to
## run here even if Biomass/PB_QB haven't run yet this session; their
## target sheets are simply skipped with a message until they do.
finalize_workbook_sheet_order(  # reorder/rename the workbook's sheets into their final layout
  out_path = ECOPATH_WORKBOOK_PATH,
  rename_map = c(FG_lookup = "FG", References = "PB_QB_References_"),
  target_order = c(
    "FG", "Ecopath", "Catches_Ecopath", "Catches_Ecopath_ByFleet", "Fleet_Structure", "FG_spp_Ecopath",
    "PB_QB", "PB_QB_spp", "F_by_FG_EcopathYears", "Ecobase", "PB_QB_References_",
    "AquaMaps_Depth_Adjustment", "FG_Density_by_Stratum", "traits_ewe", "Ecosim", "FG_spp_Ecosim",
    "Catches_Ecosim", "Catches_Species_Division_FG", "GFCM_Catches_by_Division", "Fleet_vs_Division_Check",
    "Catches_ByCountryFleetSector", "STECF_FDI_Catch_by_GSA", "STECF_FDI_Effort_by_GSA", "STECF_FDI_Capacity_by_Fleet", "Effort_by_FleetType_EU3",
    "SAU_STECF_Discard_Calibration", "GFCM_Task2_Placeholder", "GFCM_SAF_Effort_Placeholder",
    "Unreported_Pct_by_Country", "Bycatch_Placeholder", "Fishing_Effort_by_Fleet", "DataSources_Catch"
  )
)

message("\n=== Done (02_fisheries_master.R) === Wrote ", length(sheets_to_write), " sheet(s) directly, plus",
        " Catches_Ecopath/Catches_Ecosim earlier: ", paste(names(sheets_to_write), collapse = ", "),
        ", Catches_Ecopath, Catches_Ecosim. Every catch/discard/fleet-split/unreported/bycatch value",
        " carries its own *_source or data_status column - check those before treating any row as",
        " GFCM-direct versus an inferred proportion or an unfilled placeholder. Entirely self-contained -",
        " nothing else needs to run before this script; no EMODnet anywhere.")

## =================================================================
## # validation figures (by country, gear/fleet, and FG, over time)
## Purely diagnostic - reads only the tables already built above,
## writes nothing back into the workbook or any *.csv this script's
## own numbers depend on. Six figures, one PDF (one figure per page)
## plus each also saved as its own PNG, so you can flip through the
## PDF quickly or drop a single PNG into a slide/email:
##   1. Catches per FG over time (landings vs. catch-incl-discards)
##   2. Catch by Country x fleet over time (stacked)
##   3. Discards by Country x fleet over time, colored by which data
##      tier actually supplied that cell's discard rate
##   4. Fleet-composition proportions over time (checks the STECF FDI
##      hindcast transition at STECF_FDI_START_YEAR for a visible jump)
##   5. Effort by Country x fleet over time (FDI/SAU-hindcast for the
##      EU-3, FishMIP for Morocco/Algeria/Tunisia)
##   6. Data-source coverage tile: Country x Year, colored by which
##      tier (STECF FDI / SAU calibrated / SAU uncalibrated / flat
##      fallback) supplied that cell's fleet split - the fastest single
##      glance at what's real vs. inferred, per country and year.
## Every plot is wrapped in its own tryCatch() - one plot failing (e.g.
## a table that's empty because a data source wasn't found this run)
## never blocks the others or the rest of the script having already run.
## =================================================================
VALIDATION_PLOTS_PDF <- file.path(out_dir, "fisheries_validation_plots.pdf")
validation_png_dir <- file.path(out_dir, "validation_plots")
if (!dir.exists(validation_png_dir)) dir.create(validation_png_dir, recursive = TRUE, showWarnings = FALSE)  # ensure the PNG output dir exists

## Short, consistent label for which tier actually produced a row's
## fleet split / discard rate - parsed from the *_source text every
## step above already writes, so this doesn't need its own bookkeeping.
classify_source <- function(x) {
  fifelse(is.na(x), "not available",  # no source string at all
          fifelse(grepl("^STECF FDI", x), "STECF FDI",  # FDI-derived
                  fifelse(grepl("bias-corrected", x), "SAU (calibrated hindcast)",  # SAU, calibrated against FDI's overlap
                          fifelse(grepl("uncalibrated", x), "SAU (uncalibrated)",  # SAU, no FDI calibration available
                                  fifelse(grepl("^SAU", x), "SAU",  # any other SAU-sourced tier
                                          fifelse(grepl("fleet_prop|flat", x, ignore.case = TRUE), "Flat fallback (all-years average)",  # last-resort flat average
                                                  "other"))))))
}

validation_plots <- list()  # collect each figure by name, for the PDF/PNG export below

## 1. Catches per FG over time - landings vs. catch-incl-discards,
## small multiples so every FG (including the zero-catch ones from the
## completed grid above) is visible in one figure.
validation_plots[["01_catches_per_FG"]] <- tryCatch({
  d <- melt(catches_discards_fg[, .(Year, FG_num, FG_name, Landings_t, Catch_t)],
            id.vars = c("Year", "FG_num", "FG_name"), variable.name = "Measure", value.name = "t")  # reshape landings/catch into long format for plotting
  ggplot(d, aes(x = Year, y = t, color = Measure)) +
    geom_line(linewidth = 0.4) +
    facet_wrap(~ paste0(FG_num, " - ", FG_name), scales = "free_y") +
    scale_color_manual(values = c(Landings_t = "#4C6FE7", Catch_t = "#D9534F"),
                       labels = c(Landings_t = "Landings", Catch_t = "Catch (incl. discards)")) +
    labs(title = "Catches per functional group over time", subtitle = "GFCM landings vs. landings+discards, full West Med series",
         x = NULL, y = "t/year", color = NULL) +
    theme_minimal(base_size = 7) + theme(legend.position = "bottom")
}, error = function(e) { message("[Validation plot 1] skipped - ", conditionMessage(e)); NULL })

## 2. Catch by Country x fleet over time (the fleet_split_final result)
validation_plots[["02_catch_by_country_fleet"]] <- tryCatch({
  d <- fleet_split_out[Sector != "Recreational", .(Catch_t = sum(Catch_t, na.rm = TRUE)), by = .(Country, FleetType, Year)]  # sum catch by country x fleet x year, commercial fleets only
  ggplot(d, aes(x = Year, y = Catch_t, fill = FleetType)) +
    geom_area(position = "stack") +
    facet_wrap(~ Country, scales = "free_y") +
    geom_vline(xintercept = STECF_FDI_START_YEAR, linetype = "dashed", color = "grey30") +
    labs(title = "Catch by country and fleet over time", subtitle = paste0("Dashed line = STECF FDI coverage starts (", STECF_FDI_START_YEAR, ")"),
         x = NULL, y = "t/year (all FG summed)", fill = "Fleet") +
    theme_minimal(base_size = 8) + theme(legend.position = "bottom")
}, error = function(e) { message("[Validation plot 2] skipped - ", conditionMessage(e)); NULL })

## 3. Discards by Country x fleet over time, colored by data tier -
## lets you see at a glance whether a spike/drop tracks a real change
## or just a switch in which source is supplying that cell.
validation_plots[["03_discards_by_country_fleet"]] <- tryCatch({
  d <- fleet_split_out[Sector != "Recreational" & !is.na(Discard_t),
                       .(Discard_t = sum(Discard_t, na.rm = TRUE)), by = .(Country, FleetType, Year, discard_split_source)]  # sum discards by country x fleet x year x source
  d[, Tier := classify_source(discard_split_source)]  # classify each row's data tier for coloring
  ggplot(d, aes(x = Year, y = Discard_t, color = Tier)) +
    geom_point(size = 0.6, alpha = 0.7) + geom_line(aes(group = FleetType), linewidth = 0.2, alpha = 0.4) +
    facet_wrap(~ Country, scales = "free_y") +
    labs(title = "Discards by country and fleet over time", subtitle = "Colored by which data tier supplied that cell's discard rate",
         x = NULL, y = "Discard_t/year (all FG, all fleets summed per point)", color = NULL) +
    theme_minimal(base_size = 8) + theme(legend.position = "bottom")
}, error = function(e) { message("[Validation plot 3] skipped - ", conditionMessage(e)); NULL })

## 4. Fleet-composition proportions over time (fleet_prop_final) -
## stacked to 100%, checks for a visible discontinuity right at the
## STECF FDI start year (would flag a calibration problem).
validation_plots[["04_fleet_proportions"]] <- tryCatch({
  d <- fleet_prop_final[, .(prop_fleet = mean(prop_fleet, na.rm = TRUE)), by = .(Country, FleetType, Year)]  # average fleet share across FGs, by country x fleet x year
  ggplot(d, aes(x = Year, y = prop_fleet, fill = FleetType)) +
    geom_area(position = "fill") +
    facet_wrap(~ Country) +
    geom_vline(xintercept = STECF_FDI_START_YEAR, linetype = "dashed", color = "white", linewidth = 0.5) +
    scale_y_continuous(labels = function(x) paste0(round(x * 100), "%")) +
    labs(title = "Fleet-composition proportions over time", subtitle = paste0("Averaged across FG per Country x FleetType x Year - dashed line = STECF FDI starts (", STECF_FDI_START_YEAR, ")"),
         x = NULL, y = "Share of catch", fill = "Fleet") +
    theme_minimal(base_size = 8) + theme(legend.position = "bottom")
}, error = function(e) { message("[Validation plot 4] skipped - ", conditionMessage(e)); NULL })

## 5. Effort by Country x fleet over time - FDI/SAU-hindcast for the
## EU-3 (Effort_by_FleetType_EU3), FishMIP nom_active for Morocco/
## Algeria/Tunisia (effort_by_fleet, own "Country - gear" naming).
validation_plots[["05_effort_by_country_fleet"]] <- tryCatch({
  eu3 <- if (nrow(effort_by_fleettype_eu3) > 0) {
    effort_by_fleettype_eu3[, .(Country, Fleet = FleetType, Year, Effort = Effort_days, Source = effort_source)]  # standardize columns for the EU-3 effort source
  } else data.table()
  mat <- if (nrow(effort_by_fleet) > 0) {
    effort_by_fleet[!Country %in% c("Spain", "France", "Italy"),
                    .(Country, Fleet = gear, Year, Effort = nom_active_kWdays, Source = "FishMIP nom_active")]  # standardize columns for the non-EU FishMIP effort source
  } else data.table()
  d <- rbindlist(list(eu3, mat), use.names = TRUE, fill = TRUE)  # combine both effort sources into one plotting table
  if (nrow(d) == 0) stop("no effort data available from either source")
  ggplot(d, aes(x = Year, y = Effort, color = Fleet)) +
    geom_line(linewidth = 0.4) +
    facet_wrap(~ Country, scales = "free_y") +
    labs(title = "Fishing effort by country and fleet over time", subtitle = "EU-3: FDI days (2014+) / SAU-hindcasted days (pre-2014). Morocco/Algeria/Tunisia: FishMIP nom_active",
         x = NULL, y = "Effort (units vary by source - see legend/table)", color = "Fleet") +
    theme_minimal(base_size = 8) + theme(legend.position = "bottom")
}, error = function(e) { message("[Validation plot 5] skipped - ", conditionMessage(e)); NULL })

## 6. Data-source coverage tile - Country x Year, colored by which
## tier supplied the fleet split for that cell. The fastest single
## glance at what's real (STECF FDI) vs. inferred (SAU, calibrated or
## not) vs. last-resort (flat fallback), per country and year.
validation_plots[["06_data_source_coverage"]] <- tryCatch({
  d <- unique(fleet_prop_final[, .(Country, Year, fleet_split_source)])  # one row per country x year x source
  d[, Tier := classify_source(fleet_split_source)]  # classify each row's data tier for coloring
  d <- unique(d[, .(Country, Year, Tier)])  # collapse to one row per country x year x tier
  ggplot(d, aes(x = Year, y = Country, fill = Tier)) +
    geom_tile(color = "white", linewidth = 0.2) +
    scale_fill_manual(values = c("STECF FDI" = "#2E7D32", "SAU (calibrated hindcast)" = "#F9A825",
                                 "SAU (uncalibrated)" = "#EF6C00", "Flat fallback (all-years average)" = "#B71C1C",
                                 "SAU" = "#F9A825", "not available" = "grey85", "other" = "grey60")) +
    labs(title = "Fleet-split data source, by country and year", subtitle = "What actually supplied each cell's fleet proportions",
         x = NULL, y = NULL, fill = NULL) +
    theme_minimal(base_size = 8) + theme(legend.position = "bottom", panel.grid = element_blank())
}, error = function(e) { message("[Validation plot 6] skipped - ", conditionMessage(e)); NULL })

validation_plots <- validation_plots[!sapply(validation_plots, is.null)]  # drop any plot that failed and returned NULL
if (length(validation_plots) == 0) {
  message("\n[Validation plots] None could be built - the tables they read from are all empty (no data",
          " sources were found this run). Nothing written.")
} else {
  pdf(VALIDATION_PLOTS_PDF, width = 11, height = 8)  # open the PDF device
  for (p in validation_plots) print(p)  # draw each figure onto its own PDF page
  dev.off()  # close the PDF device
  for (nm in names(validation_plots)) {
    ggsave(file.path(validation_png_dir, paste0(nm, ".png")), validation_plots[[nm]], width = 11, height = 8, dpi = 150)  # also save each figure as its own PNG
  }
  message("\n[Validation plots] ", length(validation_plots), " of 6 figure(s) written to '", VALIDATION_PLOTS_PDF,
          "' (one PDF, all figures) and as individual PNGs under '", validation_png_dir, "'. Any figure not",
          " listed here was skipped because the table it needs came back empty this run - check the",
          " message above naming which one and why.")
}