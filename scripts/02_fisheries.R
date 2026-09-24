## =================================================================
## Created by: Daniel Vilas
## FISHERIES MASTER SCRIPT - ONE SELF-CONTAINED SOURCE FILE.
##
## Runs the whole fisheries flow end to end. The flow (below) is
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
##   One file DOES still get read as-is, because it is this pipeline's
##   fixed-name "contract file" from Step 1, not another fisheries
##   script's output: species_density_regional_combined.csv (needed for
##   the F-for-species step below only). If Step 1 hasn't been run yet,
##   that one step is skipped with a loud message - everything else in
##   this script still runs. The study-area figure (Total_Area_km2,
##   used to convert catch tonnes into a t/km^2 density matching
##   Biomass's units) used to be read from Step 1's own
##   strata_area_by_area.csv too - that's gone now, since catches have
##   no depth-stratum dimension for a per-stratum file to matter for;
##   it's computed directly here instead, from the official GSA
##   shapefile's own plain geometric area (see that computation's own
##   comment further down).
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
      message("[02_fisheries.R] ", var_name, " already set - using '", val, "' rather than re-prompting.")
      return(val)  # existing value is valid, use it as-is
    }
    message("[02_fisheries.R] ", var_name, " was already set but to something invalid - re-resolving.")
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

ECOPATH_WORKBOOK_PATH <- file.path(out_dir, "ecopath_ecosim_inputs.xlsx")  # path to the EwE Ecopath/Ecosim workbook to write into - stays at the shared top-level out_dir since every block reads/writes it

## 2026-09-17 update: this block's own native/intermediate CSV outputs
## now go into their own "fisheries" subfolder, matching 01_biomass.R's
## "biomass" subfolder - each block keeps its native CSVs separate,
## with only the shared workbook staying at the top-level out_dir.
## BIOMASS_CSV_DIR points at 01_biomass.R's subfolder for this script's
## cross-block reads of that block's own outputs (species density,
## Ecosim.csv, FG_lookup.csv) - NOT strata area any more, see the
## Total_Area_km2 computation further down for why.
csv_out_dir <- file.path(out_dir, "fisheries")
if (!dir.exists(csv_out_dir)) dir.create(csv_out_dir, recursive = TRUE)
BIOMASS_CSV_DIR <- file.path(out_dir, "biomass")

## SOURCE-ABLE SCRIPT: as with out_dir/pcloud_dir/git_dir above, every
## knob below is only set to its West Med default when not already set
## by a calling driver script (e.g. run_pipeline_demo.R) - set any
## subset of these before source()-ing this script for a custom
## region/year run; anything left unset keeps its original default.
if (!exists("DATASET_VERSION", envir = .GlobalEnv, inherits = FALSE)) DATASET_VERSION <- "GFCM_2025"   # "FAO_2020" | "GFCM_2025"
if (!exists("START_YEAR",      envir = .GlobalEnv, inherits = FALSE)) START_YEAR <- 1994
if (!exists("END_YEAR",        envir = .GlobalEnv, inherits = FALSE)) END_YEAR   <- 2023                # GFCM_2025's full available series. FishMIP/SAU below have no
# data past ~2017-2019 regardless - their own coverage messages
# say so explicitly rather than truncating GFCM's longer series.
if (!exists("YEAR_ECOPATH",    envir = .GlobalEnv, inherits = FALSE)) YEAR_ECOPATH <- 1994:1996         # single-snapshot averaging window for the Ecopath-by-fleet and F steps - CONFIRMED intentional (2026-09-23): the Ecopath reference period is 1994-1996 (or 1995 alone), NOT a stale placeholder - several catch sources (ICCAT, STECF FDI, some STAR/RAM assessments) genuinely have no coverage this early, which is why quite a few commercial FGs show zero in Ecopath_L/Ecopath_Di for exactly this window even though they have real catch in later years - see species_group_fg_crosswalk.csv to check whether a given FG's species matched at all vs. simply has no data yet for 1994-1996

if (!exists("TARGET_COUNTRIES", envir = .GlobalEnv, inherits = FALSE)) TARGET_COUNTRIES <- c("Spain", "France", "Italy", "Tunisia", "Algeria", "Morocco")

## Which GSAs' plain geometric area to sum for the Catches<->Biomass
## density conversion further down - defaults to the same Western Med
## GSA 1-11 range 01_biomass.R's own AREA_MODE == "westmed" default
## covers, but resolved independently here (NOT read from 01_biomass.R's
## own FILTER_AREAS/area_shp) since this script must stay runnable on
## its own - see that conversion's own comment below for why it's a
## plain area sum now, not a read of 01_biomass.R's strata_area_by_area.csv.
if (!exists("FILTER_AREAS", envir = .GlobalEnv, inherits = FALSE)) FILTER_AREAS <- 1:11   # GSA numbers

message("[02_fisheries.R] Region/year config in effect: TARGET_COUNTRIES = ", paste(TARGET_COUNTRIES, collapse=", "),
        " | FILTER_AREAS (GSA) = ", paste(range(FILTER_AREAS), collapse="-"),
        " | START_YEAR-END_YEAR = ", START_YEAR, "-", END_YEAR, " | DATASET_VERSION = ", DATASET_VERSION)

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
## "FI_Regional_2026.1.0" as of the latest pCloud listing) - the
## version number bumps on its own schedule, not this script's, so
## hardcoding one exact folder name breaks every time GFCM re-exports.
## find_versioned_subdir() below looks for whichever subfolder actually
## has the marker file/dir in it, rather than assuming a fixed name.
## Two possible locations are supported, tried in this order:
##   1) pcloud_dir/data/fisheries/GFCM/<any version folder> - the
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
## FG_WMed_2026.csv - the reviewed 2026 species -> FG reference (species/
## FG_number/FG_name/taxonomy/source/status), same file 01_biomass.R's own
## fg_species_file points at - NOT the old FG_WMed.xlsx sheet 4. Read with
## fread() below (see the `fg <- fread(...)` line further down), not
## readxl::read_excel(). The traits_ewe sheet that's the ONE remaining
## legitimate reason to still open FG_WMed.xlsx anywhere in this pipeline
## (01_biomass.R's fg_traits_file, STEP 12) has nothing to do with species
## -> FG matching, so this script - which only ever needed the species/FG
## reference, never traits - has no reason to touch the xlsx file at all.
FG_REFERENCE_SUBPATH  <- "data/FG_WMed_2026.csv"          # relative to pcloud_dir

FISHMIP_EFFORT_PARQUET <- file.path(pcloud_dir, "data/fisheries/FishMIP/effort_histsoc_1841_2017_western-mediterranean-sea.parquet")  # FishMIP effort parquet path
FISHMIP_CATCH_PARQUET  <- file.path(pcloud_dir, "data/fisheries/FishMIP/calibration_catch_histsoc_1850_2017_western-mediterranean-sea.parquet")  # FishMIP catch parquet path
FISHMIP_FG_CROSSWALK_PATH <- NULL   # columns: fishmip_f_group, FG_num - leave NULL to keep FishMIP's own naming

## Rousseau et al. 2024 (Scientific Data 11:260) global fishing-capacity/
## effort database - manual download, same convention as FDI/FishMIP/SAU
## (place the repo's Data/Final_DataStudyFAO_AllGears_wCode.csv here).
## (2026-09) used below (a) to hindcast Spain/France/Italy's
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
## the explicit direction given, ANCHORED (not trusted on its own trend)
## to FDI's real level via a per-country calibration factor computed
## from that same overlap window - see rousseau_calibration below.
ROUSSEAU_EFFORT_PATH <- file.path(pcloud_dir, "data/fisheries/RousseauEtAl2023/Data/Final_DataStudyFAO_AllGears_wCode.csv")  # Rousseau et al. 2024 effort CSV path - confirmed on disk under this exact folder name (2026-09)

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

## Morocco/Algeria real local data (2026-09, per the uploaded workbook
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

## Step 1's fixed-name contract file - needed for the F-for-species step
## only. Not another fisheries script's output; if this doesn't exist
## yet (Step 1 hasn't been run), only that one step is skipped (see its
## own file.exists() check further down).
SPECIES_DENSITY_PATH <- file.path(BIOMASS_CSV_DIR, "species_density_regional_combined.csv")  # Step 1's contract file: species density (biomass block's own subfolder)

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
## library 01_biomass.R (Biomass) and the old
## 02_fao_catches.R (Catches) both already source, so Catches_Ecopath/
## Catches_Ecosim below come out byte-for-byte the same shape Biomass's
## Ecopath/Ecosim sheets do, not a reimplementation that could quietly
## drift from it.
source(file.path(git_dir, "scripts/lib_survey_fg_density_functions.R"))  # load shared workbook-writing helper functions

## download_gfcm_gsa_shapefile() - copied in, not sourced, same
## "no dependency on another script having run first" philosophy this
## file's own header comment describes for its catch/discard/effort data
## (and the identical copy 01_biomass.R and fig_WMed_basemap.R each keep
## of their own) - this script's area figure below should never depend
## on whether 01_biomass.R happened to run first against this out_dir.
download_gfcm_gsa_shapefile <- function() {
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
  shp$gsa_num[shp$gsa_num %in% c(111, 112)] <- 11   # W/E Sardinia fix, same as 01_biomass.R/fig_WMed_basemap.R
  shp
}

## Study area for the Catches<->Biomass density conversion (t/km^2/year,
## matching Biomass's own units) - computed directly from the official
## GSA polygons' own geometric area, NOT read from 01_biomass.R's
## strata_area_by_area.csv. That file is a per-DEPTH-STRATUM breakdown,
## built specifically for the MEDITS survey's stratified sampling design
## (bathymetry-derived area within each depth band, within each area) -
## catches have no stratum dimension at all: a catch is reported for the
## whole area/GSA(s) a fleet fishes, then scaled to whatever area the
## fleet actually represents (a subdivision, one GSA, or several GSAs
## combined), never broken down by depth band the way a survey haul is.
## Borrowing biomass's strata-level file for this figure was never the
## right area conceptually - on top of being an avoidable dependency on
## 01_biomass.R having run first against this same out_dir. Summing the
## plain polygon area of every GSA in FILTER_AREAS is the correct,
## self-contained equivalent instead.
gsa_shp_for_area <- download_gfcm_gsa_shapefile()
gsa_shp_for_area <- gsa_shp_for_area[gsa_shp_for_area$gsa_num %in% FILTER_AREAS, ]
if (nrow(gsa_shp_for_area) == 0) {
  stop("None of FILTER_AREAS (", paste(FILTER_AREAS, collapse = ", "), ") matched a GSA in the",
       " official GFCM shapefile - check FILTER_AREAS is set to real GSA numbers (gsa_num, 1-30,",
       " with 111/112 already folded into 11).")
}
Total_Area_km2 <- sum(as.numeric(sf::st_area(gsa_shp_for_area))) / 1e6  # m^2 -> km^2
message("\n[Area] Study area for the Catches<->Biomass density conversion: ", round(Total_Area_km2, 1),
        " km^2 (plain geometric area of GSA(s) ", paste(sort(FILTER_AREAS), collapse = ", "),
        " from the official GFCM shapefile, computed directly here - NOT read from",
        " 01_biomass.R's strata_area_by_area.csv, since catches have no depth-stratum dimension",
        " for that file's per-stratum breakdown to matter for).")

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

## fread(), not readxl::read_excel() - cfg$fg_file is FG_WMed_2026.csv
## (species/FG_number/FG_name/taxonomy/source/status), not the old
## FG_WMed.xlsx sheet 4 (ESPECIE/GF/FG_name). Renamed to the old
## ESPECIE/GF column names right here so every downstream reference in
## the matching cascade below (fg$ESPECIE, GF, ...) works unchanged -
## same rename 01_biomass.R itself applies at its own fread() call.
fg <- fread(cfg$fg_file)  # load the functional-group reference table
setnames(fg, c("species", "FG_number"), c("ESPECIE", "GF"), skip_absent = TRUE)

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
  
  ## 2026-09-23 addition: a query that's "ambiguous" only because it
  ## spans a commercial vs non-commercial split of the SAME taxon (e.g.
  ## a generic name matching both "Non-commercial decapods" and "Other
  ## commercial decapods" in FG_WMed_2026.csv) is a real, recurring
  ## pattern here, not a genuine multi-species ambiguity - name/taxonomy
  ## matching can't see commercial status on its own (same reasoning
  ## 01_biomass.R's own taxonomy fallback already applies, via its
  ## exclude_fg_regex parameter). Everything reaching this function
  ## comes from an actual GFCM/STECF/SAU/STAR catch or landings record,
  ## which means it WAS caught/reported - so it always belongs in a
  ## commercial FG, never the non-commercial one. When dropping the
  ## non-commercial candidate(s) leaves exactly ONE remaining FG, that
  ## commercial FG is used instead of discarding the query as ambiguous.
  ## If more than one non-non-commercial candidate remains, it's a
  ## genuine ambiguity and still gets dropped, same as before.
  commercial_rescued <- data.table()
  if (length(ambiguous_queries) > 0 && "FG_name" %in% names(merged_dt)) {
    amb_dt <- copy(merged_dt[get(query_col) %in% ambiguous_queries])
    amb_dt[, is_noncommercial := grepl("non-commercial|noncommercial", FG_name, ignore.case = TRUE)]
    amb_n <- amb_dt[is_noncommercial == FALSE, .(n_commercial_fg = uniqueN(get(fg_col))), by = query_col]
    rescued_queries <- amb_n[n_commercial_fg == 1][[query_col]]
    if (length(rescued_queries) > 0) {
      commercial_rescued <- unique(amb_dt[get(query_col) %in% rescued_queries & is_noncommercial == FALSE], by = query_col)
      commercial_rescued[, is_noncommercial := NULL]
      ambiguous_queries <- setdiff(ambiguous_queries, rescued_queries)
    }
  }
  
  safe <- unique(merged_dt[get(query_col) %in% safe_queries], by = query_col)
  if (nrow(commercial_rescued) > 0) {
    safe <- rbindlist(list(safe, commercial_rescued), use.names = TRUE, fill = TRUE)
  }
  list(safe = safe, ambiguous = merged_dt[get(query_col) %in% ambiguous_queries])
}
extract_genus <- function(sci_name) str_extract(sci_name, "^[A-Za-z]+")  # pull the genus (first word) from a scientific name

blank_row <- unmatched_species[is.na(Species) | Species == ""]  # rows with no species name at all
if (nrow(blank_row) > 0) message(nrow(blank_row), " row(s) with blank Species name - excluded.")
unmatched_species <- unmatched_species[!is.na(Species) & Species != ""]  # drop blank-species rows

fg_lookup <- unique(fg[, .(ScientificName = trimws(ESPECIE), FG_num = GF, FG_name)])  # build the FG reference lookup (ScientificName trimmed - stray whitespace here would silently break every EXACT-match cascade downstream, including ICCAT's own species match, without ever showing up as an "unmatched" warning since the row is just never looked for correctly)

## --- FG_name canonicalization (2026-09-23): FG_WMed_2026.csv should
## carry exactly ONE FG_name spelling per FG_num, but a few rows carry
## a slightly different FG_name for the same FG_num (stray leading/
## trailing whitespace, a capitalization slip, or a genuine retyped
## label) - trimws() alone does not always catch it. Left unfixed,
## every FG_num x FG_name COMBINATION downstream (full_fg_list,
## fg_catch_grid's CJ() a few hundred lines below, and anything else
## keyed on FG_name rather than FG_num alone) silently multiplies:
## a FG_num with 2 distinct FG_name spellings becomes 2 grid rows per
## year instead of 1, and add_catches_to_ecopath_workbook()'s
## build_catch_ts_column() then has to collapse the resulting
## duplicate (Year, Catch) rows back down with a warning (confirmed
## happening for real - FG 22, 120 -> 30 rows). Fix it once, here, at
## the source: force every FG_num onto a single canonical FG_name (the
## most frequent trimmed spelling; ties broken alphabetically) so every
## later FG_num x FG_name join is 1:1 the way it's meant to be.
fg_lookup[, FG_name := trimws(FG_name)]  # strip stray leading/trailing whitespace first - the cheapest fix
fg_name_variants <- unique(fg_lookup[, .(FG_num, FG_name)])[, .N, by = FG_num][N > 1, FG_num]  # FG_nums that still carry more than one distinct FG_name after trimming
if (length(fg_name_variants) > 0) {
  variant_detail <- unique(fg_lookup[FG_num %in% fg_name_variants, .(FG_num, FG_name)])
  warning(length(fg_name_variants), " FG_num(s) map to more than one distinct FG_name in FG_WMed_2026.csv even",
          " after trimming whitespace - collapsing each to its most frequent spelling (this used to silently",
          " duplicate every FG_num x Year row downstream). Affected FG_num(s) and spelling(s): ",
          paste(capture.output(print(variant_detail, row.names = FALSE)), collapse = " | "))
  fg_name_counts <- fg_lookup[FG_num %in% fg_name_variants, .N, by = .(FG_num, FG_name)]
  setorder(fg_name_counts, FG_num, -N, FG_name)  # most frequent spelling first per FG_num; alphabetical tie-break
  canonical_fg_name <- fg_name_counts[, .(FG_name_canonical = FG_name[1]), by = FG_num]  # keep only the top row per FG_num
  fg_lookup <- merge(fg_lookup, canonical_fg_name, by = "FG_num", all.x = TRUE)  # attach the canonical spelling to every row
  fg_lookup[!is.na(FG_name_canonical), FG_name := FG_name_canonical]  # overwrite with the canonical spelling
  fg_lookup[, FG_name_canonical := NULL]  # drop the now-unneeded helper column
  fg_lookup <- unique(fg_lookup)  # re-collapse now-identical rows (e.g. the stanza tie-break just below relies on ScientificName duplication only, not FG_name)
}

## Full FG catalog (FG_num x FG_name only) - built from the UN-deduplicated
## fg_lookup above (every FG number FG_WMed_2026.csv defines, including a
## juvenile/adult stanza FG with no catch source of its own - see the
## stanza tie-break right below) for add_catches_to_ecopath_workbook()'s
## own fg_lookup argument (every FG, not just ones with a matched species).
full_fg_list <- unique(fg_lookup[, .(FG_num, FG_name)])  # every FG number/name, deduplicated
setorder(full_fg_list, FG_num)  # sort by FG number

## --- Species/group -> FG crosswalk, collected per data source (2026-
## 09-23) --------------------------------------------------------------
## Answers "which raw species/group name from which catch data source
## matched (or failed to match) which FG?" directly, as its own CSV -
## the excel workbook only shows the FG-level RESULT (landings/discards
## by FG x fleet), so there was no single place to check "does the catch
## data even contain something that should have landed in FG 13?"
## without re-deriving it from console messages. Each source's own
## matching block below appends one data.table here, right where that
## source's match/no-match outcome is already known - written out as one
## combined CSV further down, once every source has had its turn.
species_fg_crosswalk_parts <- list()

## --- Stanza tie-break (2026-09-22): FG_WMed_2026.csv deliberately maps
## some species to MORE THAN ONE FG when it splits that species into
## life-stage stanzas (e.g. Merluccius merluccius -> both "European hake
## juv." and "European hake adult"). No catch/landings source in this
## pipeline (GFCM, STECF FDI, SAU, STAR/RAM) reports an age/length-class
## breakdown - they report one undifferentiated tonnage per species - so
## a species-keyed match against fg_lookup is inherently ambiguous
## wherever this happens. Two failure modes downstream if left
## unresolved: (a) resolve_matches_safely() (the main GFCM/SAU cascade)
## treats it as ambiguous and DROPS it entirely - both stanzas end up
## with zero catch; (b) a plain merge() (the STAR/RAM cross-check below)
## silently duplicates the row onto every stanza FG instead of picking
## one. Neither is what you want: all of a stanza-split species' catch
## should land on its ADULT stanza (the juvenile FG stays explicitly at
## zero catch from these sources, same as any FG with no matched species
## at all - not silently dropped, just honestly empty pending a real
## juvenile/adult split rule). Applied ONCE here, to the copy of
## fg_lookup every matching step below actually uses, so every cascade
## (direct/common-name/genus/containment/STAR-RAM) agrees.
dupe_stanza_species <- fg_lookup[, .N, by = ScientificName][N > 1, ScientificName]
if (length(dupe_stanza_species) > 0) {
  dupe_stanza_detail <- fg_lookup[ScientificName %in% dupe_stanza_species]
  has_adult <- dupe_stanza_detail[, .(has_adult = any(str_detect(FG_name, regex("adult", ignore_case = TRUE)))), by = ScientificName]
  no_adult_species <- has_adult[has_adult == FALSE, ScientificName]
  if (length(no_adult_species) > 0) {
    warning(length(no_adult_species), " species map to more than one FG (stanza split) but none of their FGs",
            " is named '...adult' - keeping the FIRST FG listed for each, review by hand: ",
            paste(no_adult_species, collapse = ", "))
  }
  fg_lookup[ScientificName %in% dupe_stanza_species, .keep := str_detect(FG_name, regex("adult", ignore_case = TRUE)) | ScientificName %in% no_adult_species]
  fg_lookup <- fg_lookup[is.na(.keep) | .keep == TRUE]
  fg_lookup[, .keep := NULL]
  fg_lookup <- unique(fg_lookup, by = "ScientificName")  # for a no-adult species, .keep is TRUE on every row above - keep only the first
  message("\n[Stanza tie-break] ", length(dupe_stanza_species), " species mapped to more than one FG (life-stage",
          " stanza split) - all their catch/landings will be assigned to the '...adult' FG; the matching juvenile",
          " FG(s) stay in full_fg_list (zero catch from these sources, not dropped) until a real juvenile/adult",
          " split rule exists: ", paste(dupe_stanza_species, collapse = ", "))
}
fg_lookup[, genus := extract_genus(ScientificName)]  # add a genus column for the genus-fallback match

fg_name_lookup <- unique(fg[, .(Species = FG_name, FG_num = GF, FG_name)])  # lookup keyed by FG name itself
## Case/whitespace-insensitive join key (2026-09-22) - GFCM's own Species
## field is Name_En (a common name, not the scientific name - see
## load_gfcm_regional()'s own comment), so this is the step that has to
## catch "Swordfish" == "Swordfish" even if one side has different
## capitalization or stray whitespace. A strict by="Species" merge here
## previously required byte-for-byte identical text.
norm_name <- function(x) str_squish(str_to_lower(x))
unmatched_species[, .join_key := norm_name(Species)]
fg_name_lookup[, .join_key := norm_name(Species)]
direct_merged <- merge(unmatched_species[, .(Species, Catch, .join_key)],
                       fg_name_lookup[, .(.join_key, FG_num, FG_name)], by = ".join_key")  # try matching species name directly to an FG name, case/whitespace-insensitive
direct_resolved <- resolve_matches_safely(direct_merged)
direct_matches <- direct_resolved$safe[, .(Species, FG_num, FG_name)]  # keep only the unambiguous direct matches
direct_matches[, match_method := "direct_fg_name"]  # tag how these matches were resolved
message("STEP - Direct Species==FG_name matches: ", nrow(direct_matches))
unmatched_species[, .join_key := NULL]
fg_name_lookup[, .join_key := NULL]

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
  destfile <- file.path(csv_out_dir, "CL_FI_SPECIES_GROUPS.csv")
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

## --- FAO Name_En -> Scientific_Name bridge (2026-09-22), inserted
## BEFORE the genus fallback: a species-EXACT match, not just genus, for
## the case that motivated this - GFCM reports "Swordfish" (Name_En),
## FishBase's own common-names table (fb_lookup, used by the exact_merged
## step above) may not carry that exact ComName for Xiphias gladius, so
## the common-name step above can miss it even after the case/whitespace
## fix to the direct step - but FAO's OWN species reference (the same
## file GFCM itself is built from) maps "Swordfish" -> "Xiphias gladius"
## directly and unambiguously. This bridge catches exactly that gap:
## GFCM's common name resolved through FAO's own crosswalk to a single
## scientific name, then joined to fg_lookup by that scientific name -
## more precise than the genus fallback below (which would also match
## every OTHER species in the same genus, not just this one).
fao_sci_bridge <- merge(remaining[, .(Species, Catch)],
                        unique(fao_species[!is.na(Scientific_Name) & Scientific_Name != "", .(Name_En, Scientific_Name)]),
                        by.x = "Species", by.y = "Name_En")  # attach FAO's own scientific name for this common name
fao_sci_merged <- merge(fao_sci_bridge, fg_lookup[, .(ScientificName, FG_num, FG_name)],
                        by.x = "Scientific_Name", by.y = "ScientificName")  # attach FG via that scientific name
fao_sci_resolved <- resolve_matches_safely(fao_sci_merged)
fao_sci_matches <- fao_sci_resolved$safe[, .(Species, FG_num, FG_name)]  # keep only unambiguous matches
fao_sci_matches[, match_method := "fao_scientific_name"]  # tag how these matches were resolved
message("\nSTEP - Resolved via FAO Name_En -> Scientific_Name bridge: ", nrow(fao_sci_matches),
        " | Ambiguous (excluded): ", uniqueN(fao_sci_resolved$ambiguous$Species))

remaining <- remaining[!Species %in% fao_sci_matches$Species]  # species still unmatched after the FAO scientific-name bridge
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

all_matches <- rbindlist(list(direct_matches, exact_matches, fao_sci_matches, genus_matches, containment_matches))  # combine matches from every cascade step
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
fwrite(resolved_final, file.path(csv_out_dir, "species_fg_matched.csv"))  # write the full matching result to CSV

## % of SPECIES resolved (as before) says nothing about how much CATCH
## that represents - a handful of unresolved species can carry most of
## the tonnage if they happen to be major stocks, while dozens of
## unresolved minor/rare species barely move the number. Reporting both,
## plus the actual top offenders by tonnage, is what actually lets you
## tell "a few bycatch species didn't match, no big deal" apart from
## "a major stock silently vanished from landings-by-FG".
pct_species_resolved <- round(100 * mean(resolved_final$status == "resolved"), 1)
total_catch_all <- sum(resolved_final$Catch, na.rm = TRUE)
pct_catch_resolved <- if (total_catch_all > 0) round(100 * sum(resolved_final[status == "resolved"]$Catch, na.rm = TRUE) / total_catch_all, 1) else NA
message("\n=== FINAL MATCHING SUMMARY === Resolved ", sum(resolved_final$status == "resolved"), " of ",
        nrow(resolved_final), " species (", pct_species_resolved, "% of species, ", pct_catch_resolved,
        "% of total GFCM catch tonnage).")
top_unresolved <- resolved_final[status == "unresolved"][order(-Catch)][seq_len(min(15, .N))]
if (nrow(top_unresolved) > 0) {
  message("Top unresolved species by catch tonnage (these are the ones actually worth chasing down -",
          " check species_fg_matched.csv for the full list):")
  print(top_unresolved[, .(Species, Catch)])
}

species_to_fg <- unique(resolved_final[status == "resolved", .(Species, FG_num, FG_name)])  # final species->FG lookup, resolved rows only

# 2026-09-23: bridge GFCM's raw common/reported name through fao_species's exact
# Name_En -> Scientific_Name lookup, same pattern used for STECF FDI below, so the
# crosswalk's ScientificName column is populated for auditing instead of hardcoded
# NA. This does NOT create new FG matches by itself - GFCM's own cascade (genus
# fallback, FishBase common name, exact fao_scientific_name step already inside
# rerun_species_fg_cascade()) already tried a scientific-name route wherever one
# existed; this just surfaces the resolved scientific name (when one exists) next
# to whatever FG_num/FG_name/Matched status that cascade already produced. Aggregate/
# NEI category names (e.g. "Morays eels etc. NEI") have no single scientific name in
# fao_species and will still show ScientificName = NA here, as expected.
gfcm_name_to_sci <- unique(fao_species[!is.na(Scientific_Name) & Scientific_Name != "",
                                       .(Name_En, Scientific_Name)])
gfcm_name_to_sci[, n_distinct_sci := uniqueN(Scientific_Name), by = Name_En]
gfcm_name_to_sci <- unique(gfcm_name_to_sci[n_distinct_sci == 1, .(Name_En, Scientific_Name)])

species_fg_crosswalk_parts[["GFCM"]] <- resolved_final[, .(DataSource = "GFCM", RawIdentifier = Species,
                                                           FG_num, FG_name,
                                                           Matched = status == "resolved")]
species_fg_crosswalk_parts[["GFCM"]] <- merge(species_fg_crosswalk_parts[["GFCM"]], gfcm_name_to_sci,
                                              by.x = "RawIdentifier", by.y = "Name_En", all.x = TRUE)
setnames(species_fg_crosswalk_parts[["GFCM"]], "Scientific_Name", "ScientificName")
setcolorder(species_fg_crosswalk_parts[["GFCM"]],
            c("DataSource", "RawIdentifier", "ScientificName", "FG_num", "FG_name", "Matched"))

## --- Re-runnable version of the cascade above, for species that show
## up LATER in the pipeline (FDI's own species catalog, below) but never
## went through this cascade in the first place because GFCM itself
## never reported catch for them. This matters specifically for ICCAT-
## managed species (Bluefin tuna, Swordfish) - GFCM_Capture_Quantity
## commonly has zero or near-zero West Med rows for these (ICCAT, not
## GFCM, is their competent reporting body), so they never entered
## unmatched_species/resolved_final above AT ALL - not because matching
## failed, but because they were never given a chance to match. FDI DOES
## report real landings for them, and the FDI block further down was
## silently dropping those rows ("no FG to assign") as if matching had
## failed, when actually no matching had been attempted yet. Reuses
## every lookup already built above (fg_name_lookup, fb_lookup,
## fg_lookup, fao_species, fb_with_fg, candidate_tokens,
## resolve_matches_safely(), clean_name(), tokenize(),
## match_by_containment()) - same cascade, same match_method tags, just
## callable again for a different set of names.
rerun_species_fg_cascade <- function(species_names) {
  todo <- data.table(Species = unique(species_names))
  todo <- todo[!is.na(Species) & Species != "" & !Species %in% species_to_fg$Species]
  empty_out <- data.table(Species = character(), FG_num = numeric(), FG_name = character(), match_method = character())
  if (nrow(todo) == 0) return(empty_out)
  out <- copy(empty_out)
  
  dm <- merge(todo, fg_name_lookup, by = "Species")
  if (nrow(dm) > 0) {
    r <- resolve_matches_safely(dm)$safe[, .(Species, FG_num, FG_name)]
    r[, match_method := "direct_fg_name_rerun"]
    out <- rbind(out, r, fill = TRUE)
  }
  todo <- todo[!Species %in% out$Species]
  
  if (nrow(todo) > 0) {
    todo_cn <- copy(todo)
    todo_cn[, clean_species := clean_name(Species)]
    em <- merge(todo_cn[, .(Species, clean_species)], fb_lookup[, .(clean_species, ScientificName)], by = "clean_species", allow.cartesian = TRUE)
    em <- merge(em, fg_lookup[, .(ScientificName, FG_num, FG_name)], by = "ScientificName")
    if (nrow(em) > 0) {
      r <- resolve_matches_safely(em)$safe[, .(Species, FG_num, FG_name)]
      r[, match_method := "fishbase_common_name_rerun"]
      out <- rbind(out, r, fill = TRUE)
    }
  }
  todo <- todo[!Species %in% out$Species]
  
  ## 2026-09-23 fix: this rerun cascade was missing the exact FAO
  ## Name_En -> Scientific_Name bridge (fao_sci_bridge/fao_sci_merged,
  ## lines ~658-667 above) that the ONE-TIME GFCM cascade already has -
  ## it jumped straight from FishBase common-name matching to the much
  ## coarser genus-level fallback below. Since this rerun cascade is the
  ## ONLY matching STECF FDI's still-unmatched 3-alpha codes ever get
  ## (via code_has_name$Species, further down), any code whose FAO
  ## Name_En resolves to an exact scientific name that genus-matching
  ## alone would miss (e.g. because match_by_containment/genus never
  ## fires, or the genus already maps to a DIFFERENT FG so the genus
  ## match comes out ambiguous and gets excluded) was silently staying
  ## unresolved even though an unambiguous exact match was available.
  if (nrow(todo) > 0 && exists("fao_species")) {
    fsb <- merge(todo, unique(fao_species[!is.na(Scientific_Name) & Scientific_Name != "",
                                          .(Name_En, Scientific_Name)]),
                 by.x = "Species", by.y = "Name_En")
    fsm <- merge(fsb, fg_lookup[, .(ScientificName, FG_num, FG_name)],
                 by.x = "Scientific_Name", by.y = "ScientificName")
    if (nrow(fsm) > 0) {
      r <- resolve_matches_safely(fsm)$safe[, .(Species, FG_num, FG_name)]
      r[, match_method := "fao_scientific_name_rerun"]
      out <- rbind(out, r, fill = TRUE)
    }
  }
  todo <- todo[!Species %in% out$Species]
  
  if (nrow(todo) > 0 && exists("fao_species")) {
    tg <- merge(todo, unique(fao_species[, .(Name_En, genus)], by = "Name_En"), by.x = "Species", by.y = "Name_En", all.x = TRUE)
    gm <- merge(tg[!is.na(genus), .(Species, genus)], fg_lookup[!is.na(genus), .(genus, FG_num, FG_name)], by = "genus", allow.cartesian = TRUE)
    if (nrow(gm) > 0) {
      r <- resolve_matches_safely(gm)$safe[, .(Species, FG_num, FG_name)]
      r[, match_method := "fao_genus_rerun"]
      out <- rbind(out, r, fill = TRUE)
    }
  }
  todo <- todo[!Species %in% out$Species]
  
  if (nrow(todo) > 0) {
    cr <- rbindlist(lapply(todo$Species, match_by_containment))
    if (nrow(cr) > 0) {
      cm <- merge(cr, fb_with_fg[, .(Species, FG_num, FG_name)], by.x = "candidate", by.y = "Species")
      if (nrow(cm) > 0) {
        r <- resolve_matches_safely(cm)$safe[, .(Species, FG_num, FG_name)]
        r[, match_method := "word_containment_rerun"]
        out <- rbind(out, r, fill = TRUE)
      }
    }
  }
  unique(out, by = "Species")
}

## GFCM catches by species/FG x year x Division (all West Med reporting
## countries - the "by species and year and GSA[Division]" deliverable):
gfcm_species_division_fg <- merge(ts_data$species_ts, species_to_fg, by = "Species")  # attach FG to the species-level catch timeseries
gfcm_species_division_fg <- gfcm_species_division_fg[Year >= START_YEAR & Year <= END_YEAR,
                                                     .(Landings_t = sum(Catch, na.rm = TRUE)), by = .(Year, Division, FG_num, FG_name, Species)]  # sum landings by year x division x FG x species, within the study period
fwrite(gfcm_species_division_fg, file.path(csv_out_dir, paste0("gfcm_catches_by_species_year_division_", DATASET_VERSION, ".csv")))  # write result to CSV
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
fwrite(gfcm_catches_by_area, file.path(csv_out_dir, paste0("gfcm_catches_by_country_division_year_", DATASET_VERSION, ".csv")))  # write result to CSV
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
fwrite(fleet_vs_division_check, file.path(csv_out_dir, "fleet_definition_vs_gfcm_division_check.csv"))  # write result to CSV
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
  if (!"sector" %in% names(sau_resolved)) sau_raw[, sector := NA_character_]  # add a placeholder if no fishing-sector column exists (see GEAR_TO_FLEETTYPE's Artisanal-override comment below for why sector needs to travel through sau_country_fg_gear(_year) now, not just sau_sector_prop)
  
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
  
  sau_crosswalk <- merge(sau_sci, sau_species_fg, by = "ScientificName", all.x = TRUE)  # every distinct SAU scientific name, matched or not
  species_fg_crosswalk_parts[["SAU"]] <- sau_crosswalk[, .(DataSource = "SAU", RawIdentifier = ScientificName,
                                                           ScientificName, FG_num, FG_name, Matched = !is.na(FG_num))]
  
  sau_raw <- merge(sau_raw, sau_species_fg, by.x = "sci_name", by.y = "ScientificName", all.x = TRUE)  # attach FG to every SAU catch row
  
  ## Country x FG x gear catch, summed across all years SAU has -
  ## this is what weights the fleet split below (time-invariant
  ## fallback - kept for Country x FG x Year cells the year-resolved
  ## version below has no SAU catch for at all).
  ## sau_gear_sector: SAU's OWN sector value for the country x gear
  ## combination that dominates that cell's catch - carried alongside
  ## sau_gear (not replacing it) so the Artisanal override below can
  ## fire without changing sau_country_fg_gear(_year)'s existing grain
  ## (still one row per Country x FG x gear(x Year); a gear can have
  ## more than one sector value across its rows in principle, so the
  ## single dominant one - by catch tonnage - is taken, same spirit as
  ## fallback_match_detail's majority-vote elsewhere in this pipeline).
  dominant_sector <- function(tonnes_vec, sector_vec) {
    tot <- data.table(tonnes = tonnes_vec, sector = sector_vec)[, .(t = sum(tonnes, na.rm = TRUE)), by = sector]
    if (nrow(tot) == 0 || all(is.na(tot$sector))) return(NA_character_)
    tot[which.max(t), sector]
  }
  sau_country_fg_gear <- sau_raw[!is.na(FG_num), .(Catch_t = sum(tonnes, na.rm = TRUE),
                                                   sau_gear_sector = dominant_sector(tonnes, sector)),
                                 by = .(Country = country, FG_num, sau_gear = gear)]  # sum catch by country x FG x gear, across all years
  
  ## Same, but keeping Year - this is what actually lets SAU's real
  ## year-to-year variation feed the pre-2014 hindcast below, instead
  ## of one flat average applied to every year.
  sau_country_fg_gear_year <- sau_raw[!is.na(FG_num), .(Catch_t = sum(tonnes, na.rm = TRUE),
                                                        sau_gear_sector = dominant_sector(tonnes, sector)),
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
  ## Artisanal override (2026-09-23) - see STECF's identical-in-spirit
  ## "small vessels are always Artisanal, overriding the gear-based
  ## assignment" rule further down (STECF_VESSEL_LENGTH_ARTISANAL). Before
  ## this fix, SAU's pre-2014 hindcast only ever reached FleetType =
  ## "Artisanal" via GEAR_TO_FLEETTYPE's crude "Gillnet -> Artisanal"
  ## row - a gear-only heuristic with no relation to STECF's real,
  ## vessel-length-based Artisanal definition (which reclassifies SMALL
  ## VESSELS of ANY gear as Artisanal). That definitional mismatch is
  ## exactly what produced the validation-plot discontinuity right at
  ## the STECF_FDI_START_YEAR boundary: "Artisanal" (and whichever gear-
  ## specific FleetTypes STECF's small vessels get pulled out of) jumped
  ## because the two periods were measuring genuinely different things
  ## under the same label, not because of a units/scalar bug. SAU's own
  ## sau_gear_sector (its real fishing_sector field, see sau_sector_prop
  ## above) is the actual scale-of-operation signal - using it here to
  ## override FleetType to "Artisanal" whenever SAU itself calls a
  ## Country x FG x gear cell Artisanal brings both periods back onto
  ## the same definition (small-scale overrides gear, on both sides of
  ## 2014), rather than a gear-only proxy on one side and a real vessel-
  ## length rule on the other.
  mapped[grepl("artisanal", sau_gear_sector, ignore.case = TRUE), FleetType := "Artisanal"]
  unmatched_gear <- sau_country_fg_gear[!mapped, on = c("Country", "sau_gear")]  # gear combinations with no mapping entry
  if (nrow(unmatched_gear) > 0) {
    unmatched_gear[, FleetType := fifelse(grepl("artisanal", sau_gear_sector, ignore.case = TRUE), "Artisanal", "Unclassified")]  # same Artisanal override as `mapped` above, else bucket unmapped gear as Unclassified
    unmatched_share <- round(100 * sum(unmatched_gear$Catch_t) / sum(sau_country_fg_gear$Catch_t), 1)  # what share of catch this represents
    message("\n[Fleet split] ", nrow(unmatched_gear), " Country x gear combination(s) (", unmatched_share,
            "% of SAU's catch value here) aren't in GEAR_TO_FLEETTYPE - kept as 'Unclassified' (or 'Artisanal' where SAU's own sector field says so).")
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

## --- GFCM Fleet Register vessel-count override (Morocco/Algeria) -----
## 2026-09-23, per Andrea: real registered-vessel counts by gear exist
## for every FLEET_REGISTER country in GFCM-FleetRegister.xlsx
## (pcloud_dir/data/Complementary data/GFCM-FleetRegister.xlsx, sheet
## "FleetRegister") - a genuine GFCM fleet-register source (checked:
## GFCM's own public Regional Fleet Register is a view-only Power BI
## dashboard with no bulk export, so this local copy is the only usable
## form of it). Applied HERE only to Morocco and Algeria, per Andrea's
## explicit scope (Tunisia intentionally excluded this round; France/
## Spain/Italy already have a better source - STECF FDI's own real
## per-year catch-based split). Vessel-count share is used directly as
## prop_fleet - i.e. assumes roughly equal catch-per-vessel across gear
## types within a country, since no per-gear catch or CPUE figure exists
## for these two countries either; a real, country-specific GFCM
## registry count is still a much better basis than the arbitrary flat
## 1/N split or thin SAU country-level mix these two countries would
## otherwise fall into. Like country_overall, this doesn't vary by FG (a
## registered vessel's gear doesn't change per species) or by year
## (single fixed-count snapshot, not a time series in this export) -
## applied uniformly to every FG_num and every Year for these countries.
##
## Sheet layout: repeating blocks, one per country - a header row whose
## first cell is the country name and second cell is literally
## "Operant a:", followed by FleetType/GSA/Comments/Number-of-vessels
## data rows, then a blank row before the next country's block.
GFCM_FLEET_REGISTER_XLSX <- file.path(pcloud_dir, "data/Complementary data/GFCM-FleetRegister.xlsx")
GFCM_FLEET_REGISTER_SHEET <- "FleetRegister"
GFCM_FLEET_REGISTER_OVERRIDE_COUNTRIES <- c("Morocco", "Algeria")  # scope, per Andrea - add "Tunisia" here if she wants it included later
GFCM_FLEET_REGISTER_SOURCE_LABEL <- "GFCM Fleet Register (real registered-vessel count by gear, Complementary data/GFCM-FleetRegister.xlsx) - assumes roughly equal catch-per-vessel across gears, no FG/year variation"

parse_gfcm_fleet_register <- function(path, sheet) {
  if (!file.exists(path)) return(data.table())
  raw <- as.data.table(readxl::read_excel(path, sheet = sheet, col_names = FALSE))
  setnames(raw, seq_len(min(4, ncol(raw))), c("c1", "c2", "c3", "c4")[seq_len(min(4, ncol(raw)))])
  ## Not every country's block has the same header style - Morocco/
  ## Algeria/Tunisia's blocks start with a "c2 == 'Operant a:'" sub-
  ## header row, but France/Spain/Italy's blocks are just a bare country-
  ## name row (c2/c3/c4 all blank) straight into data rows, confirmed
  ## against Andrea's actual pasted sheet content (2026-09-23) - relying
  ## on "Operant a:" alone silently mis-attributed every France/Spain/
  ## Italy row to whichever country came right before them. Robust rule
  ## instead: c4 (Number of vessels) is a real number ONLY on an actual
  ## data row, in EITHER block style - so any row where c1 is non-blank
  ## but c4 does NOT parse as numeric (the header text "Number of
  ## vessels", or simply blank) marks the start of a new country block,
  ## regardless of whether it also happens to carry an "Operant a:" row.
  out <- list()
  current_country <- NA_character_
  for (i in seq_len(nrow(raw))) {
    c1 <- trimws(as.character(raw$c1[i])); if (is.na(c1)) c1 <- ""
    c2 <- trimws(as.character(raw$c2[i])); if (is.na(c2)) c2 <- ""
    if (c1 == "") next  # blank separator row
    n_vessels <- suppressWarnings(as.numeric(raw$c4[i]))
    if (is.na(n_vessels)) { current_country <- c1; next }  # country-marker row (header-style or bare) - not a data row
    if (is.na(current_country) || current_country == "") next  # a data-looking row before any country marker was seen - ignore defensively
    out[[length(out) + 1]] <- data.table(Country = current_country, FleetType = c1, GSA = c2, N_vessels = n_vessels)
  }
  if (length(out) == 0) return(data.table())
  rbindlist(out)
}

gfcm_fleet_register <- parse_gfcm_fleet_register(GFCM_FLEET_REGISTER_XLSX, GFCM_FLEET_REGISTER_SHEET)
if (nrow(gfcm_fleet_register) == 0) {
  message("\n[Fleet split] GFCM-FleetRegister.xlsx not found/empty at '", GFCM_FLEET_REGISTER_XLSX,
          "' - Morocco/Algeria keep whatever fleet_prop tier they'd otherwise fall into (see message above).")
} else {
  fwrite(gfcm_fleet_register, file.path(csv_out_dir, "gfcm_fleet_register_vessel_counts.csv"))
  vessel_share <- gfcm_fleet_register[Country %in% GFCM_FLEET_REGISTER_OVERRIDE_COUNTRIES]
  vessel_share[, prop_fleet_vessels := N_vessels / sum(N_vessels), by = Country]
  n_before <- fleet_prop[Country %in% GFCM_FLEET_REGISTER_OVERRIDE_COUNTRIES, .N]
  fleet_prop[vessel_share, `:=`(
    prop_fleet = i.prop_fleet_vessels,
    fleet_split_source = GFCM_FLEET_REGISTER_SOURCE_LABEL
  ), on = c("Country", "FleetType")]
  n_overridden <- fleet_prop[Country %in% GFCM_FLEET_REGISTER_OVERRIDE_COUNTRIES & fleet_split_source == GFCM_FLEET_REGISTER_SOURCE_LABEL, .N]
  untouched <- unique(fleet_prop[Country %in% GFCM_FLEET_REGISTER_OVERRIDE_COUNTRIES & fleet_split_source != GFCM_FLEET_REGISTER_SOURCE_LABEL, .(Country, FleetType)])
  message("\n[Fleet split] GFCM Fleet Register vessel-count override: ", n_overridden, " of ", n_before,
          " Country x FG x FleetType row(s) for ", paste(GFCM_FLEET_REGISTER_OVERRIDE_COUNTRIES, collapse = "/"),
          " now use real registered-vessel-count shares instead of a flat/borrowed fallback.",
          if (nrow(untouched) > 0) paste0(" ", nrow(untouched), " Country x FleetType combo(s) weren't in the",
                                          " register and kept their previous fallback: ",
                                          paste(paste(untouched$Country, untouched$FleetType, sep = ": "), collapse = "; "), ".") else "")
}

## Same mapping, but keeping Year - SAU's own real year-by-year fleet
## shares (not the flat average above), the raw material for the
## pre-2014 hindcast a few sections down. Sparse cells (a Country x FG
## x Year SAU has zero catch for) are simply absent here - the hindcast
## step falls back to fleet_prop's flat average for those.
if (nrow(sau_country_fg_gear_year) > 0) {
  mapped_year <- merge(sau_country_fg_gear_year, GEAR_TO_FLEETTYPE, by = c("Country", "sau_gear"), allow.cartesian = TRUE)  # map SAU gear onto fleet types, keeping Year
  mapped_year[, Catch_t := Catch_t * weight]  # apply the ambiguity-splitting weight
  mapped_year[grepl("artisanal", sau_gear_sector, ignore.case = TRUE), FleetType := "Artisanal"]  # same Artisanal override as the flat `mapped` table above - see its comment
  unmatched_gear_year <- sau_country_fg_gear_year[!mapped_year, on = c("Country", "sau_gear", "Year")]  # gear combinations with no mapping entry
  if (nrow(unmatched_gear_year) > 0) {
    unmatched_gear_year[, FleetType := fifelse(grepl("artisanal", sau_gear_sector, ignore.case = TRUE), "Artisanal", "Unclassified")]  # same Artisanal override as `mapped_year` above, else bucket unmapped gear as Unclassified
  }
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
## FDI is the PRIMARY fleet source for these 3
## countries from 2014 onward (its own trusted coverage window); SAU fills the
## gaps - years before 2014, and any Country x FG STECF doesn't cover
## (see the fleet_prop_final blend and the hindcast section below, and
## sau_sector_prop above for the Artisanal/Recreational cross-check).
##
## Manual download (STECF has no API): grab the "Effort, landings,
## catches, capacity, biological" bulk file from https://stecf.ec.
## europa.eu/data-dissemination/fdi_en, unzip it, and point
## STECF_FDI_DIR at the unzipped folder. Real structure (confirmed
## against the real download, Sept 2026) is five subfolders, NOT
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
## Same versioned-subfolder problem as GFCM above - the real
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
## 2013 is FDI's first reporting year and, its
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

## Requirement: the creep % should (1) vary across countries - EU vs
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

## (1b) Sector/gear differential, creep should also
## differ between artisanal and industrial (mechanized) gears, since
## engine/electronics/net-material upgrades reach large industrial vessels
## (trawlers, purse seiners) faster than small artisanal boats. As with the
## country multiplier above, NO fishery-specific literature figure exists
## for this split either (checked Marchal et al. 2007 ICES J. Mar. Sci. -
## qualitative harbour-interview data and CPUE-GLM coefficients only, no
## quantitative annual gear-specific creep rate) - this is ALSO A JUDGMENT
## CALL, same status/convention as TECH_CREEP_COUNTRY_MULTIPLIER: Industrial
## keeps the full rate (multiplier 1); Artisanal is reduced by 40%
## (multiplier 0.6), reflecting slower capital-equipment turnover on small
## boats. Combined with the country multiplier by simple multiplication
## (Sector multiplier x Country multiplier), so an artisanal Moroccan fleet
## gets 0.6 x 0.7 = 0.42 of the base rate, an industrial Spanish fleet gets
## 1 x 1 = 1 (the full base rate). Change these two numbers directly if you
## have a better source or a different view.
TECH_CREEP_SECTOR_MULTIPLIER <- data.table(
  Sector = c("Industrial", "Artisanal", "Recreational"),
  sector_creep_multiplier = c(1, 0.6, 0.6)
)

## (2) Time- AND gear-varying rate, kept inside the realistic 0-5%/year
## creep range specified (the earlier draft re-evaluated Palomares &
## Pauly's duration-average formula at each year's own short elapsed
## distance from the base year, which spiked to 13.8%/year right at the
## start - correct algebraically but not a realistic ANNUAL rate, since
## that formula's Y is meant as a whole-window average, not something to
## re-derive one year at a time). Applied by simple proportional year-
## by-year compounding (multiplier for year Y = product, across every
## calendar year between the base year and Y, of (1 + that year's own
## period/gear rate/100)) - no formula re-evaluation, no spikes, each
## step is just "X% more than last year":
##  - 1994-2013 (pre-FDI window), GEAR-SPECIFIC, (2026-09 update) -
##    both period-rates now tied to real Mediterranean literature rather
##    than being assumption-only placeholders:
##      * bottom-trawl-type gear (FDI's own "DTS" fishing-technology
##        code): 0.79%/year - Tsagarakis et al. (2022, Frontiers in
##        Marine Science 9:919793) directly quote this as "estimated by
##        Damalas et al." from Damalas, D., Maravelias, C.D., Osio, G.C.,
##        Maynou, F., Sbrana, M., Sartor, P. (2015). "Once upon a Time in
##        the Mediterranean" - long term trends of Mediterranean
##        fisheries resources based on fishers' Traditional Ecological
##        Knowledge. PLoS ONE, 10(3), e0119330. (Tsagarakis et al. cite
##        this as "Damalas et al., 2014" - same study, accepted/online
##        year vs. PLoS ONE's 2015 publication year.)
##      * every OTHER gear: 2.0%/year - Tsagarakis et al. (2022) tested
##        0%/1%/2%/year (their own summary of the wider Mediterranean
##        creep literature) as an Ecopath hindcast proxy for 1993-2020
##        and selected 2%/year as the best-fitting rate for non-trawl
##        gears.
##  - 2014-2023 (FDI-trusted window): 4.5%/year, uniform across gears -
##    Palomares & Pauly (2019)'s own C% = 13.8 x Y^-0.511 evaluated ONCE
##    at Y = 9 (2014-2023's own full length) = 4.49%/year. No gear-
##    specific split exists in that source, so kept flat here.
## All rates sit inside the realistic 0-5%/year range - change
## TECH_CREEP_PERIOD_RATES directly for different breakpoints/rates/gears.
## gear_class "all" matches every gear (used for the 2014-2023 row,
## which doesn't vary by gear); "bottom_trawl"/"other" only match a
## request for that specific gear_class (see tech_creep_rate_for_year()).
TECH_CREEP_PERIOD_RATES <- data.table(
  period_start = c(1994, 1994, 2014),
  period_end   = c(2013, 2013, 2023),
  rate_gear    = c("bottom_trawl", "other", "all"),  # named differently from the gear_class argument below to avoid a data.table column/variable name collision
  annual_pct   = c(0.79, 2.0, 4.5)
)

## Looks up which period/gear a calendar year + gear_class falls in and
## returns that row's flat annual % - gear_class defaults to "other"
## (the general, non-bottom-trawl Mediterranean rate) when the caller
## doesn't know the gear, which only matters in the 1994-2013 window
## (the 2014-2023 row is gear_class "all", matching every request). A
## year outside every listed period's range (e.g. END_YEAR extended past
## 2023) uses the nearest matching-gear period's rate.
tech_creep_rate_for_year <- function(yr, gear_class = "other") {
  candidates <- TECH_CREEP_PERIOD_RATES[rate_gear == "all" | rate_gear == gear_class]  # rows that apply to every gear, or specifically to this gear_class
  hit <- candidates[yr >= period_start & yr <= period_end]  # find the period this year falls in
  if (nrow(hit) > 0) return(hit$annual_pct[1])  # return that period/gear's flat annual rate
  mids <- (candidates$period_start + candidates$period_end) / 2  # midpoint year of each candidate period
  candidates$annual_pct[which.min(abs(yr - mids))]  # use the rate of whichever period's midpoint is closest
}

## Cumulative effective multiplier for a Country x Year (x optional
## Sector), built by walking year by year from TECH_CREEP_BASE_YEAR to
## Year (forward or backward), multiplying in (1 + that calendar year's
## own period rate x the country's creep_multiplier x the sector's
## sector_creep_multiplier / 100) at each step - a plain proportional
## year-by-year increase, not a formula re-evaluated at each distance.
## Years before the base year divide instead of multiply at each step
## (older effort discounted relative to the base year's technology).
## `sector` is optional (NULL/NA/omitted) for backward compatibility -
## when not supplied, only the country multiplier applies (sector
## multiplier defaults to 1), same behavior as before this was added.
## `gear_class` is optional (NULL/NA/omitted), same backward-compatible
## convention as `sector` - when not supplied, every row uses the
## general "other" gear rate from TECH_CREEP_PERIOD_RATES (see
## tech_creep_rate_for_year()'s own default), i.e. behaves exactly as
## before gear-specific rates existed for any caller that hasn't been
## updated to pass one.
tech_creep_multiplier <- function(country, year, sector = NULL, gear_class = NULL, base_year = TECH_CREEP_BASE_YEAR) {
  mult_lookup <- setNames(TECH_CREEP_COUNTRY_MULTIPLIER$creep_multiplier, TECH_CREEP_COUNTRY_MULTIPLIER$Country)  # country -> creep multiplier lookup
  sector_lookup <- setNames(TECH_CREEP_SECTOR_MULTIPLIER$sector_creep_multiplier, TECH_CREEP_SECTOR_MULTIPLIER$Sector)  # sector -> creep multiplier lookup
  if (is.null(sector)) sector <- rep(NA_character_, length(year))  # no sector supplied - treat every row as sector multiplier 1
  if (is.null(gear_class)) gear_class <- rep(NA_character_, length(year))  # no gear_class supplied - tech_creep_rate_for_year() falls back to "other"
  vapply(seq_along(year), function(i) {
    cc <- country[i]; yr <- year[i]; ss <- sector[i]
    gg <- if (is.na(gear_class[i])) "other" else gear_class[i]
    if (is.na(cc) || is.na(yr)) return(NA_real_)
    cm <- if (cc %in% names(mult_lookup)) mult_lookup[[cc]] else 1  # this country's creep multiplier, default 1
    sm <- if (!is.na(ss) && ss %in% names(sector_lookup)) sector_lookup[[ss]] else 1  # this sector's creep multiplier, default 1 if unknown/not supplied
    cm <- cm * sm  # combine country x sector multiplier
    if (yr == base_year) return(1)  # no correction needed at the base year itself
    step <- if (yr > base_year) 1L else -1L  # walk forward or backward from base_year
    mult <- 1
    for (y in seq(base_year + step, yr, by = step)) {
      r <- tech_creep_rate_for_year(y, gg) * cm / 100  # this calendar year's gear x country x sector-adjusted creep rate
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
    "LHM", "LHP", "LL", "LLD", "LLS", "LTL", "LVT", "LX", "LH", "LN",
    "MDR", "MDV", "MEL", "MHI", "MIS", "MPM", "MPN", "MSP",
    "NK",
    "OTB", "OTM", "OTP", "OTT",
    "PS", "PS1",
    "PTB", "PTM",
    "SB", "SUX", "SV", "SX",
    "TB", "TBB", "TM", "TX", "TBS",
    "DIV", "FOO"
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
    "Hooks and lines", "Hooks and lines", "Hooks and lines", "Hooks and lines", "Hooks and lines",
    "Miscellaneous gear", "Miscellaneous gear", "Miscellaneous gear", "Miscellaneous gear",
    "Miscellaneous gear", "Miscellaneous gear", "Miscellaneous gear", "Miscellaneous gear",
    "Gear Not Known or Not Specified",
    "Trawls", "Trawls", "Trawls", "Trawls",
    "Surrounding nets", "Surrounding nets",
    "Trawls", "Trawls",
    "Surrounding nets", "Surrounding nets", "Seine nets", "Seine nets",
    "Trawls", "Trawls", "Trawls", "Trawls", "Trawls",
    ## Added 2026-09-23 from a real FDI run's "genuinely unclassified"
    ## list: DIV = diving (harvesting by hand/diving gear - FAO ISSCFG
    ## "MDV"-family, grouped here with the existing hand-collection
    ## codes' "Miscellaneous gear" label rather than a new one-off
    ## group); FOO = "on foot" gear (shore-based hand collection, same
    ## reasoning as DIV). LH/LN were also flagged unclassified - both
    ## are FAO hook-and-line variants (LH = hooks and lines nei/hand,
    ## LN = hand lines) so they join LHM/LHP/LX etc. under "Hooks and
    ## lines" above instead of getting their own group.
    "Miscellaneous gear", "Miscellaneous gear"
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
  ## Defensive normalization (2026-09-23): a real run showed codes like
  ## "OTB"/"PS"/"OTM"/"PTM" - which ARE named fleets in
  ## STECF_GEAR_TO_FLEETTYPE for the country in question - still falling
  ## through to the broad DCF group fallback for a large share of rows.
  ## The two tables' own vocabularies are correct; the most likely cause
  ## is stray whitespace or inconsistent casing on Country/gear_code in
  ## the raw FDI file breaking the exact-string join. Trim + fix case on
  ## both sides of the join so a real match isn't missed over
  ## formatting alone (this does not change which FleetType a genuinely
  ## unrecognized or genuinely-uncovered code falls to).
  dt[, `:=`(Country = trimws(Country), gear_code = trimws(toupper(gear_code)))]
  STECF_GEAR_TO_FLEETTYPE_NORM <- copy(STECF_GEAR_TO_FLEETTYPE)
  STECF_GEAR_TO_FLEETTYPE_NORM[, `:=`(Country = trimws(Country), gear_type = trimws(toupper(gear_type)))]
  dt <- merge(dt, STECF_GEAR_TO_FLEETTYPE_NORM, by.x = c("Country", "gear_code"), by.y = c("Country", "gear_type"), all.x = TRUE)  # (1) try the per-country named-fleet mapping
  
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
## list, NOT confirmed against the real fishing_tech values the way
## STECF_GEAR_TO_FLEETTYPE was - the Capacity block below prints every
## fishing_tech value it actually finds; check that against this table
## before trusting the FleetType assignment, same caution as everywhere
## else real FDI values were guessed wrong on the first pass.
## Extended 2026-09-23 with the real fishing_tech codes a run's own
## diagnostic message reported (DFN, DRB, FPO, INACTIVE, MGO, MGP,
## PGO, PGP, PS, TM, TBB - beyond the original PMP/DTS/HOK). Added
## only where a country's FLEET_REGISTER already names an equivalent
## fleet at the gear-code level above (STECF_GEAR_TO_FLEETTYPE), so
## the same per-country judgment call is reused rather than guessed
## fresh here:
##  - PS (pelagic seiners) -> "Purse seiners", all 3 countries - same
##    fleet PMP already maps to; FDI's capacity file just also uses
##    the finer PS code for some vessels.
##  - TM (midwater trawlers) -> France has a real "Midwater trawls"
##    fleet name; Spain lumps every trawl subtype into "Bottom
##    trawls" (see STECF_GEAR_TO_FLEETTYPE's Spain comment); Italy is
##    deliberately left unmapped for TM, same as at the gear-code
##    level (FLEET_REGISTER's own comment says NOT to fold Italy's
##    midwater trawlers into "Bottom trawls").
##  - TBB (beam trawlers) -> Spain lumps into "Bottom trawls" (same
##    gear-code decision); France has no beam-trawl fleet name at
##    all and Italy is explicitly excluded - both left unmapped here
##    too, consistent with the gear-code table.
## Deliberately LEFT UNMAPPED for every country (falls to
## "Unclassified", which is the semantically correct label, not a gap
## to fill in):
##  - INACTIVE - these vessel-years reported no activity; folding them
##    into any active fleet's capacity would be wrong, not just
##    imprecise.
##  - PGO/PGP (polyvalent, passive-only / passive+active gears) and
##    MGO/MGP (active gears other than trawls, nei) - genuinely mixed-
##    gear vessels with no single-gear FLEET_REGISTER name to assign
##    them to; guessing one specific named fleet would misattribute
##    their capacity.
##  - DFN (drift/fixed netters), DRB (dredgers), FPO (pots/traps) -
##    none of France/Spain/Italy's FLEET_REGISTER names a gillnet,
##    dredge, or trap fleet (same reason these gear codes fall to the
##    DCF group name rather than a named fleet in the Catches/Effort
##    files - see STECF_GEAR_TO_FLEETTYPE's own comments).
FISHING_TECH_TO_FLEETTYPE <- data.table(
  Country   = c("France", "France", "France", "France",
                "Spain",  "Spain",  "Spain",  "Spain",  "Spain",
                "Italy",  "Italy",  "Italy"),
  fishing_tech = c("PMP", "DTS", "HOK", "TM",
                   "PMP", "DTS", "HOK", "TM", "TBB",
                   "PMP", "DTS", "HOK"),
  FleetType = c("Purse seiners", "Bottom trawls", "Drifting longlines", "Midwater trawls",
                "Purse seiners", "Bottom trawls", "Drifting longlines", "Bottom trawls", "Bottom trawls",
                "Purse seiners", "Bottom trawls", "Drifting longlines")
)
FISHING_TECH_TO_FLEETTYPE <- rbind(
  FISHING_TECH_TO_FLEETTYPE,
  data.table(Country = c("France", "Spain", "Italy"), fishing_tech = "PS",
             FleetType = "Purse seiners")
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
      species_code_ref <- unique(safe_fread(file.path(cfg$data_dir, cfg$species_file), "species_file")[, .(SpeciesCode = `3A_Code`, Species = Name_En)])  # FAO 3-alpha code lookup (unique()'d defensively - a duplicate SpeciesCode row here would fan out every merge keyed on it below)
      species_code_to_fg <- unique(merge(species_code_ref, species_to_fg, by = "Species")[, .(SpeciesCode, FG_num, FG_name)])  # build a 3-alpha code -> FG lookup
      stecf_raw <- merge(stecf_raw, species_code_to_fg, by.x = "species", by.y = "SpeciesCode", all.x = TRUE)  # attach FG to each FDI row
      
      ## --- ASFIS bridge (2026-09-23 addition), tried BEFORE the retry
      ## cascade below - CL_FI_SPECIES_GROUPS.csv above is missing some
      ## 3-alpha codes entirely (44 in a confirmed run) and doesn't carry
      ## a genuine Scientific_Name for many others, so codes/names that
      ## fail there never get a chance at an EXACT taxonomic match. The
      ## full ASFIS list (FAO's actual 3-alpha code registry, distinct
      ## from CL_FI_SPECIES_GROUPS.csv's own ISSCAAP-derived species-
      ## groups extract) carries Alpha3_Code -> Scientific_Name directly.
      ## Looked for locally only (no download attempted here - unlike
      ## find_or_download_fao_species()'s CL_FI_SPECIES_GROUPS.csv, no
      ## live download URL for this specific one has been confirmed
      ## working from this environment) - place a copy anywhere under
      ## pcloud_dir (e.g. ASFIS_sp_2026.1.csv, as distributed by FAO) and
      ## it will be picked up automatically; skipped with a clear message
      ## if none is found, falling back to CL_FI_SPECIES_GROUPS.csv/the
      ## common-name cascade alone exactly as before.
      asfis_path <- list.files(pcloud_dir, pattern = "^ASFIS_sp.*\\.(csv|xlsx)$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
      if (length(asfis_path) == 0) {
        message("[STECF FDI] No ASFIS_sp_*.csv/.xlsx file found under pcloud_dir - skipping the ASFIS exact-",
                "scientific-name bridge for 3-alpha codes. Place a copy of the FAO ASFIS species list",
                " (e.g. ASFIS_sp_2026.1.csv) anywhere under pcloud_dir to enable it.")
      } else {
        asfis_path <- asfis_path[1]
        asfis_raw <- if (grepl("\\.xlsx$", asfis_path, ignore.case = TRUE)) {
          as.data.table(readxl::read_excel(asfis_path))
        } else {
          fread(asfis_path, encoding = "UTF-8")
        }
        alpha3_col <- grep("alpha.?3|3.?a.?code|3a_code", names(asfis_raw), ignore.case = TRUE, value = TRUE)[1]
        asfis_sci_col <- grep("scientific", names(asfis_raw), ignore.case = TRUE, value = TRUE)[1]
        if (is.na(alpha3_col) || is.na(asfis_sci_col)) {
          message("[STECF FDI] ASFIS file found at ", asfis_path, " but couldn't find both an alpha-3-code-like",
                  " column and a 'scientific'-named column (got: ", paste(names(asfis_raw), collapse = ", "),
                  ") - skipping the ASFIS bridge.")
        } else {
          asfis_sci_lookup <- unique(asfis_raw[, .(SpeciesCode = get(alpha3_col), Scientific_Name = get(asfis_sci_col))])
          asfis_sci_lookup <- asfis_sci_lookup[!is.na(SpeciesCode) & SpeciesCode != "" & !is.na(Scientific_Name) & Scientific_Name != ""]
          ## defensive, same reasoning as the STECF/GFCM crosswalk fix
          ## above - a duplicate SpeciesCode with two different
          ## Scientific_Name values would fan out the merge below.
          asfis_sci_lookup[, n_distinct_sci := uniqueN(Scientific_Name), by = SpeciesCode]
          n_dupe_codes <- uniqueN(asfis_sci_lookup[n_distinct_sci > 1]$SpeciesCode)
          if (n_dupe_codes > 0) {
            message("[STECF FDI] ", n_dupe_codes, " ASFIS Alpha3_Code value(s) map to more than one distinct",
                    " Scientific_Name in the loaded file - excluded from the bridge rather than guessed.")
          }
          asfis_sci_lookup <- unique(asfis_sci_lookup[n_distinct_sci == 1, .(SpeciesCode, Scientific_Name)])
          
          still_missing_fg <- unique(stecf_raw[is.na(FG_num)]$species)
          asfis_bridge <- merge(data.table(SpeciesCode = still_missing_fg), asfis_sci_lookup, by = "SpeciesCode")
          asfis_bridge <- merge(asfis_bridge, fg_lookup[, .(ScientificName, FG_num, FG_name)],
                                by.x = "Scientific_Name", by.y = "ScientificName")
          asfis_resolved <- resolve_matches_safely(asfis_bridge, query_col = "SpeciesCode")
          if (nrow(asfis_resolved$safe) > 0) {
            stecf_raw[asfis_resolved$safe, `:=`(FG_num = i.FG_num, FG_name = i.FG_name), on = c(species = "SpeciesCode")]
            message("[STECF FDI] ASFIS bridge (", asfis_path, "): ", nrow(asfis_resolved$safe), " of ",
                    length(still_missing_fg), " still-unmatched code(s) resolved via an exact Scientific_Name",
                    " match (", uniqueN(asfis_resolved$ambiguous$SpeciesCode), " ambiguous, excluded).")
          } else {
            message("[STECF FDI] ASFIS bridge (", asfis_path, "): 0 of ", length(still_missing_fg),
                    " still-unmatched code(s) resolved.")
          }
        }
      }
      
      ## Rows still missing FG_num split into TWO genuinely different
      ## failure modes that a single "dropped (no FG to assign)" message
      ## used to conflate:
      ##  (a) the species code isn't in species_code_ref at all - a
      ##      genuinely unrecognized FAO 3-alpha code.
      ##  (b) the code DOES resolve to a Name_En via species_code_ref,
      ##      but that Name_En was never in species_to_fg - meaning GFCM
      ##      itself never reported catch for that species, so it never
      ##      even entered the species->FG matching cascade above (not
      ##      that matching was attempted and failed). This is exactly
      ##      what happens for ICCAT-managed species like Bluefin tuna/
      ##      Swordfish: GFCM_Capture_Quantity commonly has zero or
      ##      near-zero West Med rows for them (ICCAT, not GFCM, is their
      ##      competent reporting body) even though FDI reports real
      ##      landings - these were being silently dropped from
      ##      landings-by-FG entirely, not because they couldn't match,
      ##      but because nothing had tried yet.
      ## Case (b) gets a second chance here, via rerun_species_fg_cascade()
      ## (defined right after species_to_fg above) - the SAME direct/
      ## common-name/genus/word-containment cascade, just run again on
      ## whatever Name_En values these still-unmatched codes resolve to.
      unmatched_codes <- unique(stecf_raw[is.na(FG_num)]$species)
      code_has_name <- species_code_ref[SpeciesCode %in% unmatched_codes]
      n_code_unknown <- length(setdiff(unmatched_codes, code_has_name$SpeciesCode))
      if (n_code_unknown > 0) {
        message("[STECF FDI] ", n_code_unknown, " species code(s) not found in CL_FI_SPECIES_GROUPS.csv's",
                " 3A_Code at all - genuinely unrecognized codes, dropped: ",
                paste(setdiff(unmatched_codes, code_has_name$SpeciesCode), collapse = ", "))
      }
      if (nrow(code_has_name) > 0) {
        rerun_matches <- rerun_species_fg_cascade(unique(code_has_name$Species))
        if (nrow(rerun_matches) > 0) {
          message("[STECF FDI] ", nrow(rerun_matches), " species resolved to an FG on retry (FDI reports",
                  " landings for these but GFCM never reported catch for them, so they never went through",
                  " the matching cascade the first time): ", paste(rerun_matches$Species, collapse = ", "))
          rerun_code_to_fg <- unique(merge(code_has_name, rerun_matches[, .(Species, FG_num, FG_name)], by = "Species")[, .(SpeciesCode, FG_num, FG_name)])
          stecf_raw[species %in% rerun_code_to_fg$SpeciesCode, `:=`(
            FG_num  = rerun_code_to_fg$FG_num[match(species, rerun_code_to_fg$SpeciesCode)],
            FG_name = rerun_code_to_fg$FG_name[match(species, rerun_code_to_fg$SpeciesCode)]
          )]
        }
        still_unmatched_names <- setdiff(unique(code_has_name$Species), rerun_matches$Species)
        if (length(still_unmatched_names) > 0) {
          ## stecf_raw has no "Species" column at all (only the lowercase
          ## "species" 3-alpha CODE column, from FDI itself) - Name_En
          ## only exists on code_has_name, keyed by SpeciesCode. Joining
          ## via the code (not a nonexistent "Species" column on
          ## stecf_raw) is what actually lets this compute real tonnage
          ## per still-unmatched species, sorted so the worst offenders
          ## (if any carry real tonnage) are easy to spot instead of
          ## just dumping an alphabetical name list.
          still_unmatched_codes <- code_has_name[Species %in% still_unmatched_names]
          landed_by_name <- stecf_raw[species %in% still_unmatched_codes$SpeciesCode,
                                      .(Landed_t = sum(total_live_weight_landed, na.rm = TRUE)), by = species]
          landed_by_name <- merge(landed_by_name, still_unmatched_codes, by.x = "species", by.y = "SpeciesCode")
          setorder(landed_by_name, -Landed_t)
          message("[STECF FDI] ", length(still_unmatched_names), " species have a valid Name_En but still",
                  " couldn't be matched to any FG (checked against fg's own FG names, FishBase/SeaLifeBase",
                  " common names, FAO genus, and word containment) - dropped, by landed tonnage:")
          print(landed_by_name[, .(Species, Landed_t)])
          message("If any of these carry real tonnage, add them to species_fg_matched.csv's cascade",
                  " manually (MANUAL_OVERRIDES above) rather than letting them drop silently.")
        }
      }
      n_unmatched_sp <- sum(is.na(stecf_raw$FG_num))  # rows still without an FG match after the retry
      unmatched_landed_t <- sum(stecf_raw[is.na(FG_num)]$total_live_weight_landed, na.rm = TRUE)
      total_landed_t <- sum(stecf_raw$total_live_weight_landed, na.rm = TRUE)
      if (n_unmatched_sp > 0) message("[STECF FDI] ", n_unmatched_sp, " row(s) still with no FG match after",
                                      " the retry above - dropped (", round(unmatched_landed_t, 1), " t of ",
                                      round(total_landed_t, 1), " t total landed weight, ",
                                      round(100 * unmatched_landed_t / total_landed_t, 2), "%).")
      
      ## Crosswalk row per distinct FAO 3-alpha code, captured BEFORE the
      ## unmatched-row filter just below so both matched and dropped codes
      ## are represented (FG_num/FG_name here already reflect the direct
      ## code match AND the rerun_species_fg_cascade() retry above - this
      ## is the final per-code outcome, not just the first pass).
      stecf_code_crosswalk <- unique(stecf_raw[, .(species, FG_num, FG_name)])
      stecf_code_crosswalk <- merge(stecf_code_crosswalk, species_code_ref[, .(SpeciesCode, Species)],
                                    by.x = "species", by.y = "SpeciesCode", all.x = TRUE)  # attach the FAO Name_En where the code is recognized at all
      setnames(stecf_code_crosswalk, "Species", "CommonName")  # this is Name_En (a common name), NOT a scientific name - renamed so the column below isn't mislabeled
      ## 2026-09-23 fix: the crosswalk's ScientificName column was being
      ## set to the FAO common name (Name_En) above, mislabeled as a
      ## scientific name. Bridge through fao_species (same Name_En ->
      ## Scientific_Name lookup used for the exact-match cascade step
      ## earlier in this file) to get the REAL taxonomic name here.
      ##
      ## 2026-09-23 second fix (confirmed by an actual run - "Join
      ## results in 1295282 rows" cartesian error right here): some
      ## Name_En values in the full FAO reference map to MORE THAN ONE
      ## distinct Scientific_Name (generic/NEI-style common names in
      ## particular), so a plain unique(Name_En, Scientific_Name) still
      ## lets one CommonName join to many Scientific_Name rows -
      ## multiplying every stecf_code_crosswalk row that shares it. The
      ## fao_sci_bridge step earlier in this file (the one-time GFCM
      ## cascade) avoids exactly this via resolve_matches_safely(); apply
      ## the same unambiguous-only filter here - a Name_En with more than
      ## one distinct Scientific_Name is dropped to NA (never guessed)
      ## rather than silently fanning out the join.
      name_en_to_sci <- unique(fao_species[!is.na(Scientific_Name) & Scientific_Name != "",
                                           .(Name_En, Scientific_Name)])
      name_en_to_sci[, n_distinct_sci := uniqueN(Scientific_Name), by = Name_En]
      n_ambiguous_name_en <- uniqueN(name_en_to_sci[n_distinct_sci > 1]$Name_En)
      if (n_ambiguous_name_en > 0) {
        message("[STECF FDI] ", n_ambiguous_name_en, " FAO Name_En value(s) map to more than one distinct",
                " Scientific_Name (e.g. generic/NEI common names shared across several taxa) - excluded from",
                " the ScientificName bridge below rather than guessed; ScientificName stays NA for those.")
      }
      name_en_to_sci <- unique(name_en_to_sci[n_distinct_sci == 1, .(Name_En, Scientific_Name)])
      stecf_code_crosswalk <- merge(stecf_code_crosswalk, name_en_to_sci,
                                    by.x = "CommonName", by.y = "Name_En", all.x = TRUE)
      species_fg_crosswalk_parts[["STECF_FDI"]] <- stecf_code_crosswalk[, .(DataSource = "STECF FDI", RawIdentifier = species,
                                                                            ScientificName = Scientific_Name, FG_num, FG_name,
                                                                            Matched = !is.na(FG_num))]
      
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
      fwrite(stecf_fdi_catch_by_gsa, file.path(csv_out_dir, "stecf_fdi_catch_by_gsa_gear_year.csv"))  # write result to CSV
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
    fwrite(stecf_fdi_effort_by_gsa, file.path(csv_out_dir, "stecf_fdi_effort_by_gsa_gear_year.csv"))  # write result to CSV
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
    ## Codes deliberately left out of FISHING_TECH_TO_FLEETTYPE (see its
    ## own header comment): INACTIVE (no real activity that year - correctly
    ## excluded from any fleet's capacity) and the polyvalent/mixed-gear
    ## codes PGO/PGP/MGO/MGP/DFN/DRB/FPO (no single FLEET_REGISTER-named
    ## fleet they belong to without guessing). Seeing exactly these codes
    ## here is expected, not a gap - anything ELSE in this list is new and
    ## worth checking against the values printed above.
    ## TBB/TM are ALSO deliberately unmapped for some countries (e.g.
    ## Italy - see FISHING_TECH_TO_FLEETTYPE's per-country judgment
    ## calls above) even though they ARE mapped for others (Spain/
    ## France), so a TBB/TM row can legitimately still fall through here
    ## for a country where no mapping was added on purpose.
    expected_unclassified_tech <- c("INACTIVE", "PGO", "PGP", "MGO", "MGP", "DFN", "DRB", "FPO", "TBB", "TM")
    genuinely_new_tech <- setdiff(unmatched_tech, expected_unclassified_tech)
    if (length(unmatched_tech) > 0) {
      message("[STECF FDI] fishing_tech code(s) not in FISHING_TECH_TO_FLEETTYPE (kept as 'Unclassified'",
              " unless overridden by vessel_length below): ", paste(unmatched_tech, collapse = ", "),
              if (length(genuinely_new_tech) == 0) ". All of these are deliberately-excluded codes" else
                paste0(". Of these, UNEXPECTED/not yet reviewed: ", paste(genuinely_new_tech, collapse = ", "),
                       " - THIS MAPPING IS A BEST GUESS for everything else, not confirmed against a real",
                       " DCF code list"),
              " (see FISHING_TECH_TO_FLEETTYPE's header comment for why each excluded code is left unmapped).")
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
    fwrite(stecf_fdi_capacity_by_fleet, file.path(csv_out_dir, "stecf_fdi_capacity_by_fleet_year.csv"))  # write result to CSV
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
    fwrite(fleet_calibration, file.path(csv_out_dir, "sau_stecf_fleet_calibration_factors.csv"))  # write result to CSV
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
## that direction), and (b) a second effort figure alongside FishMIP
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
## (2026-09) discard data should carry BOTH a time
## dimension (proportion over time) and a species/FG dimension
## (proportion by FG) - this used to collapse straight to ONE flat
## ratio per FG_num across the WHOLE study period, throwing away the
## Year dimension even though FishMIP's own catch parquet is Year-
## resolved. Fixed below: the PRIMARY ratio is now FG_num x Year; a
## Year x FG_num cell only falls back to that FG's own all-years
## pooled ratio when the per-year cell itself is missing or
## implausible (never falls back across FGs).
## =================================================================
discard_by_fg <- data.table(FG_num = integer(), Year = integer(), discard_ratio = numeric(), discard_ratio_source = character())  # placeholder, filled below if FishMIP data is available
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
  
  ## `gear` is kept through this step purely for the ambiguous-match
  ## gear-plausibility filter further down (it plays no role in the
  ## ratio itself, which stays Mediterranean-wide) - resolved against
  ## several known column-name variants rather than assumed to be
  ## literally "gear" (same robust-resolution pattern already used for
  ## SAU's raw extract above), since a FishMIP catch export that doesn't
  ## use that exact name was crashing this whole block with "object
  ## 'gear' not found" instead of degrading gracefully. If genuinely
  ## none of the candidates are present, gear is filled with a
  ## placeholder instead of stopping - the gear-plausibility filter
  ## below already has its own "keep everything unfiltered" path for
  ## exactly that case (no gear info to filter on).
  gear_candidates <- c("gear", "gear_type", "gear_name", "gear_group", "fishing_gear", "Gear")
  gear_col <- gear_candidates[gear_candidates %in% names(catch)][1]
  if (is.na(gear_col)) {
    message("\n[Discards] FishMIP catch parquet has no recognizable gear column (looked for: ",
            paste(gear_candidates, collapse = ", "), ", found: ", paste(names(catch), collapse = ", "),
            ") - filling 'gear' with a placeholder. The discard ratio itself is unaffected (it's",
            " Mediterranean-wide, not gear-specific); only the ambiguous-FG gear-plausibility filter",
            " further down loses its ability to narrow multi-candidate matches, and will keep every",
            " name-matched candidate unfiltered instead.")
    catch[, gear := NA_character_]
  } else if (gear_col != "gear") {
    setnames(catch, gear_col, "gear")
  }
  
  if (!is.null(FISHMIP_FG_CROSSWALK_PATH) && file.exists(FISHMIP_FG_CROSSWALK_PATH)) {
    fg_crosswalk <- fread(FISHMIP_FG_CROSSWALK_PATH)  # load the FishMIP-group -> model-FG crosswalk
    if (!all(c("fishmip_f_group", "FG_num") %in% names(fg_crosswalk))) stop("FISHMIP_FG_CROSSWALK_PATH must have columns fishmip_f_group, FG_num.")
    catch_fg <- merge(catch, fg_crosswalk, by.x = "f_group", by.y = "fishmip_f_group", all.x = TRUE)  # attach model FG_num via the crosswalk
    fishmip_total    <- catch_fg[!is.na(FG_num), .(tonnes = sum(Yield_t, na.rm = TRUE)), by = .(FG_num, gear, Year = year)]  # sum total yield by FG x gear x year
    fishmip_reported <- catch_fg[!is.na(FG_num), .(tonnes_reported = sum(reported, na.rm = TRUE)), by = .(FG_num, gear, Year = year)]  # sum reported landings by FG x gear x year
    fishmip_fg_scheme_used <- "FG_num (via FISHMIP_FG_CROSSWALK_PATH)"
  } else {
    message("\n[Discards] No FISHMIP_FG_CROSSWALK_PATH - discard ratio computed in FishMIP's own f_group naming.")
    fishmip_total    <- catch[, .(tonnes = sum(Yield_t, na.rm = TRUE)), by = .(FG_num = f_group, gear, Year = year)]  # sum total yield by FishMIP group x gear x year
    fishmip_reported <- catch[, .(tonnes_reported = sum(reported, na.rm = TRUE)), by = .(FG_num = f_group, gear, Year = year)]  # sum reported landings by FishMIP group x gear x year
    fishmip_fg_scheme_used <- "fishmip_f_group (no crosswalk supplied)"
  }
  fm <- merge(fishmip_total, fishmip_reported, by = c("FG_num", "gear", "Year")); fm <- fm[tonnes > 0]  # pair up total and reported, drop zero-yield cells
  
  ## PRIMARY: per FG_num x Year (collapsing gear out again - it was
  ## only carried this far for the ambiguous-match filter below).
  fm_fg_year <- fm[, .(tonnes = sum(tonnes, na.rm = TRUE), tonnes_reported = sum(tonnes_reported, na.rm = TRUE)), by = .(FG_num, Year)]
  discard_ratio_fg_year <- fm_fg_year[, .(discard_ratio = 1 - tonnes_reported / tonnes), by = .(FG_num, Year)]
  discard_ratio_fg_year <- discard_ratio_fg_year[is.finite(discard_ratio) & discard_ratio >= 0 & discard_ratio <= 0.95]  # drop implausible per-year cells (not the whole FG)
  
  ## FALLBACK: this FG's own all-years pooled ratio - used only for the
  ## specific Year x FG_num cells that didn't survive the per-year pass
  ## (thin FishMIP data that year, or an implausible single-year ratio).
  fm_fg_flat <- fm_fg_year[, .(tonnes = sum(tonnes, na.rm = TRUE), tonnes_reported = sum(tonnes_reported, na.rm = TRUE)), by = FG_num]
  discard_ratio_fg_flat <- fm_fg_flat[, .(discard_ratio_flat = 1 - tonnes_reported / tonnes), by = FG_num]
  discard_ratio_fg_flat <- discard_ratio_fg_flat[is.finite(discard_ratio_flat) & discard_ratio_flat >= 0 & discard_ratio_flat <= 0.95]
  
  all_fg_years <- CJ(FG_num = unique(fm_fg_year$FG_num), Year = START_YEAR:END_YEAR)  # every FG x Year combination in the study window
  discard_by_fg <- merge(all_fg_years, discard_ratio_fg_year, by = c("FG_num", "Year"), all.x = TRUE)
  discard_by_fg <- merge(discard_by_fg, discard_ratio_fg_flat, by = "FG_num", all.x = TRUE)
  discard_by_fg[, discard_ratio_source := fifelse(!is.na(discard_ratio), "FishMIP reported-vs-total ratio (FG x Year, time-varying)",
                                                  fifelse(!is.na(discard_ratio_flat), "FishMIP reported-vs-total ratio (FG, all-years pooled - this specific Year's own cell was too thin/implausible)", NA_character_))]
  discard_by_fg[is.na(discard_ratio), discard_ratio := discard_ratio_flat]  # backfill from the flat ratio only where the per-year cell is missing
  discard_by_fg[, discard_ratio_flat := NULL]
  discard_by_fg <- discard_by_fg[!is.na(discard_ratio)]  # drop cells with neither a per-year nor a flat ratio available
  n_year_specific <- sum(discard_by_fg$discard_ratio_source == "FishMIP reported-vs-total ratio (FG x Year, time-varying)")
  message("\n[Discards] discard_by_fg: ", nrow(discard_by_fg), " FG x Year row(s) - ", n_year_specific,
          " with a real year-specific ratio, ", nrow(discard_by_fg) - n_year_specific, " backfilled from that",
          " same FG's own all-years pooled ratio (that Year's own FishMIP cell was too thin or implausible).")
  
  if (is.na(fishmip_fg_scheme_used) || !grepl("^FG_num", fishmip_fg_scheme_used)) {
    ## No crosswalk (or no row in it) ties this FishMIP f_group to a real
    ## model FG_num - rather than dropping the discard estimate entirely,
    ## fall back to a transparent name-keyword match against full_fg_list's
    ## own FG_name: e.g. FishMIP's "Demersals" f_group keyword-matches every
    ## model FG whose FG_name contains "demersal". Per the instruction
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
            " Attempting a name-keyword AMBIGUOUS/MISMATCH 'closest FG' fallback instead of dropping it entirely.")
    fwrite(discard_by_fg, file.path(csv_out_dir, "discards_by_fishmip_fgroup_timeseries.csv"))  # write the FishMIP-scheme discard ratios to CSV for review
    
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
    n_ambiguous_fgroups <- sum(table(closest_fg_matches$fgroup) > 1)  # fgroups matching MORE than one FG before any gear filtering
    message("[Discards] name-keyword match: ", n_matched_fgroups, " of ", length(fgroup_names),
            " FishMIP f_group(s) matched to at least one model FG (", n_unmatched_fgroups,
            " unmatched - dropped, not guessed). ", n_ambiguous_fgroups, " matched f_group(s) are AMBIGUOUS/",
            "MISMATCHED (map to MORE than one model FG) - resolving with the gear-plausibility filter below",
            " before falling back to an even name-only split.")
    
    ## AMBIGUOUS/MISMATCH FILTER (2026-09, "split them into FG
    ## that could belong to that group, depending on the fleet and the
    ## name"). For every fgroup that name-matched MORE than one model FG,
    ## narrow the candidates using which gear(s) actually reported that
    ## fgroup's catch (fm above, before gear was collapsed out) against
    ## SAU's own real Country x FG x gear catch (sau_country_fg_gear,
    ## already built earlier in this script for the fleet split) - i.e.
    ## "does SAU show this gear actually catching this candidate FG at
    ## all, anywhere". A candidate with essentially zero gear-plausible
    ## catch is dropped as an implausible name-only coincidence (e.g. a
    ## keyword match that isn't really the same fish group); a candidate
    ## survives if it keeps at least FISHMIP_GEAR_FILTER_MIN_SHARE of the
    ## fgroup's total gear-plausible catch across all its candidates. If
    ## the gear crosswalk can't place a single candidate at all (e.g. the
    ## fgroup's own gear is one of FishMIP's "Others_*" catch-all buckets,
    ## or sau_country_fg_gear has nothing for any candidate), every
    ## candidate is kept exactly as before - this filter only NARROWS an
    ## ambiguous match, it never invents a match the name-only pass didn't
    ## already find, and it never turns an ambiguous match into zero.
    FISHMIP_GEAR_KEYWORD <- list(   # FishMIP's own gear tokens -> a regex matched against SAU's own gear strings (Purse seine/Longline/Set longline/Bottom trawl/Midwater trawl/Gillnet/Handline/Pole-and-line)
      Trawl_Bottom = "trawl", Trawl_Midwater_or_Unsp = "trawl",
      Seine_Purse_Seine = "purse seine", Seine_Danish_and_Other = "seine",
      Lines_Longlines = "longline", Lines_Unspecified = "line|hand",
      Gillnets = "gillnet|net", Pots_and_Traps = "trap|pot",
      Others_Others = NA_character_, Others_Multiple_Gears = NA_character_, Others_Unknown = NA_character_
    )
    FISHMIP_GEAR_FILTER_MIN_SHARE <- 0.05  # a candidate FG keeping less than 5% of its fgroup's total gear-plausible catch is dropped as implausible
    
    ambiguous_fgroups <- names(which(table(closest_fg_matches$fgroup) > 1))
    n_gear_filtered <- 0L
    if (length(ambiguous_fgroups) > 0 && nrow(sau_country_fg_gear) > 0) {
      fgroup_gear <- fm[FG_num %in% ambiguous_fgroups, .(tonnes = sum(tonnes, na.rm = TRUE)), by = .(fgroup = FG_num, gear)]  # which gear(s) actually reported each ambiguous fgroup's catch, and how much
      sau_gear_totals <- sau_country_fg_gear[, .(Catch_t = sum(Catch_t, na.rm = TRUE)), by = .(FG_num, sau_gear)]  # SAU's real Country-pooled catch by FG x gear
      kept_rows <- rbindlist(lapply(ambiguous_fgroups, function(g) {
        candidates <- closest_fg_matches[fgroup == g, FG_num]
        gears_here <- fgroup_gear[fgroup == g & tonnes > 0, gear]  # this fgroup's own real reporting gear(s)
        keywords_here <- na.omit(unlist(FISHMIP_GEAR_KEYWORD[gears_here]))
        if (length(keywords_here) == 0) return(data.table(fgroup = g, FG_num = candidates))  # no usable gear info at all - keep every name-matched candidate, unfiltered
        pattern <- paste(keywords_here, collapse = "|")
        plaus <- sau_gear_totals[FG_num %in% candidates & grepl(pattern, sau_gear, ignore.case = TRUE), .(Catch_t = sum(Catch_t)), by = FG_num]
        if (nrow(plaus) == 0 || sum(plaus$Catch_t) == 0) return(data.table(fgroup = g, FG_num = candidates))  # gear crosswalk found nothing for ANY candidate - keep all, unfiltered (never worse than before)
        plaus[, share := Catch_t / sum(Catch_t)]
        survivors <- plaus[share >= FISHMIP_GEAR_FILTER_MIN_SHARE, FG_num]
        if (length(survivors) == 0) survivors <- candidates  # degenerate case (shouldn't happen given the share test) - fall back to unfiltered rather than dropping everything
        data.table(fgroup = g, FG_num = survivors)
      }))
      n_gear_filtered <- sum(table(closest_fg_matches$fgroup) > 1) - sum(table(kept_rows$fgroup) > 1)  # how many ambiguous fgroups got narrowed to a single candidate
      closest_fg_matches <- rbindlist(list(closest_fg_matches[!fgroup %in% ambiguous_fgroups], kept_rows), use.names = TRUE)
      message("[Discards] gear-plausibility filter (split by fleet AND name): of ", length(ambiguous_fgroups),
              " ambiguous/mismatched f_group(s), ", n_gear_filtered, " narrowed to exactly one model FG using",
              " SAU's own real gear x FG catch as a plausibility check; the rest either kept multiple gear-",
              " plausible candidates or had no usable gear crosswalk (kept unfiltered, same as before this fix).")
    } else if (length(ambiguous_fgroups) > 0) {
      message("[Discards] ", length(ambiguous_fgroups), " ambiguous/mismatched f_group(s) - sau_country_fg_gear",
              " is empty (no SAU data loaded this run), so the gear-plausibility filter is skipped; falling back",
              " to the even name-only split (identical ratio applied to every name-matched candidate).")
    }
    
    setnames(discard_by_fg, "FG_num", "fgroup")  # this column is still FishMIP's own label at this point, not a real model FG_num
    discard_by_fg <- merge(discard_by_fg, closest_fg_matches, by = "fgroup", allow.cartesian = TRUE)  # attach real model FG_num via the (now gear-narrowed) keyword match
    
    ## BUG FIX (2026-09): a plain unique(FG_num, Year, discard_ratio) here
    ## is NOT enough to guarantee one row per FG_num x Year - if TWO
    ## DIFFERENT FishMIP f_groups (e.g. "Demersals" and "Miscellaneous
    ## demersal fishes") both keyword-match onto the SAME model FG for the
    ## SAME year but carry different ratios, unique() keeps both rows,
    ## leaving more than one row per FG_num x Year. The downstream
    ## merge(..., by = c("FG_num","Year")) into gfcm_country_fg then fans
    ## every one of those duplicate keys out across every matching row
    ## (the "9582 rows > 5894" cartesian-join error). Fixed by explicitly
    ## collapsing to ONE row per FG_num x Year - the mean of every
    ## f_group's ratio that landed on it - and logging every cell where
    ## this actually resolved a real conflict (never silently averaged
    ## without saying so).
    n_before_fg_collapse <- uniqueN(discard_by_fg[, .(FG_num, Year)])
    conflict_check <- discard_by_fg[, .(n_sources = uniqueN(discard_ratio)), by = .(FG_num, Year)]
    n_conflicting_fg <- sum(conflict_check$n_sources > 1)
    discard_by_fg <- discard_by_fg[, .(discard_ratio = mean(discard_ratio, na.rm = TRUE),
                                       discard_ratio_source = discard_ratio_source[1]), by = .(FG_num, Year)]  # ONE row per FG_num x Year, guaranteed
    if (n_conflicting_fg > 0) {
      message("[Discards] ", n_conflicting_fg, " of ", n_before_fg_collapse, " FG_num x Year cell(s) matched more",
              " than one FishMIP f_group with DIFFERING discard ratios - averaged to a single ratio per cell",
              " rather than left as duplicate rows (which previously caused a cartesian-join error downstream).")
    }
    fwrite(closest_fg_matches, file.path(csv_out_dir, "discards_fishmip_closest_fg_match_REVIEW.csv"))  # write the keyword matches to CSV for manual review
  }
  message("[Discards] discard_ratio available for ", uniqueN(discard_by_fg$FG_num), " FG(s) x ",
          uniqueN(discard_by_fg$Year), " Year(s) (Mediterranean-wide, applied uniformly across countries -",
          " FishMIP cannot cross country x FG). Max FishMIP year: ", max(catch$year), ".")
}

## =================================================================
## GFCM catch-MAGNITUDE calibration against STECF FDI (2026-09, per
## the instruction: "FDI is the most trustable dataset ... when
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
    fwrite(catch_magnitude_calibration, file.path(csv_out_dir, "gfcm_stecf_catch_magnitude_calibration_factors.csv"))  # write result to CSV
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
## (now scaled to STECF FDI's magnitude above wherever a factor exists).
## Joined on FG_num x Year (2026-09, was FG_num alone) - discard_by_fg is
## now time-varying, not one flat ratio per FG for the whole period; see
## discard_by_fg's own header comment above.
catch_with_discards <- merge(gfcm_country_fg, discard_by_fg, by = c("FG_num", "Year"), all.x = TRUE)  # attach the FishMIP discard ratio to every country x FG x year row
catch_with_discards[, `:=`(
  Catch_t   = fifelse(!is.na(discard_ratio), Landings_t / (1 - discard_ratio), Landings_t),  # gross up landings to include discards where a ratio exists
  Discard_t = fifelse(!is.na(discard_ratio), Landings_t / (1 - discard_ratio) - Landings_t, NA_real_),  # implied discard tonnage
  discard_source = fifelse(!is.na(discard_ratio), discard_ratio_source, "not available - landings-only")
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
## Morocco/Algeria real local catch data (2026-09, per the uploaded
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
  fwrite(taxon_fg_matches, file.path(csv_out_dir, "belhabib_taxon_to_fg_match_REVIEW.csv"))  # every taxon->FG attempt, for manual review
  n_taxon_matched <- taxon_fg_matches[n_match == 1, .N]
  message("\n[Morocco/Algeria catch] Belhabib taxon-group -> FG keyword match: ", n_taxon_matched, " of ",
          nrow(taxon_fg_matches), " taxon group(s) matched to exactly one FG (the rest matched zero or several FGs",
          " and are excluded from the override below - see belhabib_taxon_to_fg_match_REVIEW.csv).")
  
  belhabib_long <- merge(belhabib_long, taxon_fg_matches[n_match == 1, .(taxon, FG_num)], by = "taxon")  # keep only cleanly-matched taxon groups
  belhabib_catch_by_fg <- belhabib_long[, .(Catch_t_belhabib = sum(Catch_t, na.rm = TRUE)), by = .(Country, FG_num, Year)]  # aggregate to Country x FG x Year
  fwrite(belhabib_catch_by_fg, file.path(csv_out_dir, "belhabib_catch_by_fg_timeseries.csv"))
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
  catches_discards_fg <- merge(catches_discards_fg, discard_by_fg, by = c("FG_num", "Year"), all.x = TRUE)  # attach FishMIP's discard ratio (time-varying, 2026-09 - see discard_by_fg's own header comment)
  catches_discards_fg[, `:=`(
    Catch_t   = fifelse(!is.na(discard_ratio), Landings_t / (1 - discard_ratio), Landings_t),  # gross up landings to include discards
    Discard_t = fifelse(!is.na(discard_ratio), Landings_t / (1 - discard_ratio) - Landings_t, NA_real_)
  )]
} else {
  catches_discards_fg[, `:=`(Catch_t = Landings_t, Discard_t = NA_real_, discard_ratio = NA_real_)]  # no discard data at all - Catch_t equals Landings_t
}
setcolorder(catches_discards_fg, c("Year", "FG_num", "FG_name", "Landings_t", "Catch_t", "Discard_t"))  # standardize column order
message("\n[Catches] fg_catch_timeseries: ", nrow(fg_catch_timeseries), " FG x Year row(s), full West Med series.")

## =================================================================
## # GFCM STAR + RAM Legacy stock-assessment catch/landings figures
## (2026-09), using the combine_STAR_RAMlegacy.R /
## analysis_STAR.R scripts (GFCM STAR Power BI scrape + RAM Legacy stock
## database) - these give a SECOND, independent catch/landings series
## for the subset of species that actually have a real stock assessment
## (a handful of commercially important species/stocks, e.g. hake, red
## mullet, sardine, anchovy, deep-water rose shrimp, and - notably -
## highly-migratory large pelagics like tuna/swordfish that ICCAT/GFCM
## assess directly but that GFCM's own STATLANT capture-production
## product resolves only weakly).
##
## Built here first purely as comparison columns against the GFCM/FDI/
## SAU-built catch series above (star_catch_pct_diff/
## star_discard_ratio_diff_pp below); a SEPARATE step further down
## ("Stock-assessment catch/landings PRIORITY") then actually REPLACES
## Catch_t/Landings_t/Discard_t with STAR/RAM's own figures, but only
## for single-species/stanza assessed FGs (2026-09-17 update - see that
## section's own header for the exact rule) - every other FG's
## comparison here stays a cross-check only, never blended into
## Catch_t.
##
## Species -> FG matching reuses fg_lookup's own exact-name-then-genus
## cascade (same convention already used for SAU's species -> FG match
## above - see sau_direct/sau_genus), so a stock assessed under a
## synonym or at a slightly different taxonomic rank still has a decent
## chance of resolving to the right FG rather than being silently
## dropped.
##
## Expects the combine_STAR_RAMlegacy.R output,
## combined_medbs_star_ramlegacy.csv (source/stock_key/species/
## common_name/gsa/subregion/year/biomass/catches/landings/
## landings_flag/... - see that script's own header), placed under
## STAR_RAMLEGACY_DIR. Optional - message + skip if not found, same
## convention as every other optional source in this script (e.g.
## MOROCCO_ALGERIA_DIR above).
## =================================================================
STAR_RAMLEGACY_DIR <- file.path(pcloud_dir, "data/fisheries/STAR_RAMLegacy")  # folder for combine_STAR_RAMlegacy.R's own output CSV
star_ram_path <- file.path(STAR_RAMLEGACY_DIR, "combined_medbs_star_ramlegacy.csv")
star_ram_combined <- if (file.exists(star_ram_path)) {
  fread(star_ram_path, encoding = "UTF-8")
} else {
  message("\n[STAR/RAM cross-check] '", star_ram_path, "' not found - skipping (run combine_STAR_RAMlegacy.R",
          " first and place its output under STAR_RAMLEGACY_DIR if you want this cross-check).")
  data.table()
}

star_catch_by_fg <- data.table()
if (nrow(star_ram_combined) > 0) {
  ## Restrict to the West Med subregion (same field/convention
  ## analysis_STAR.R already computes and combine_STAR_RAMlegacy.R already
  ## filters on: str_detect(subregion, "Western Mediterranean")) - GFCM
  ## Divisions 37.1.1-37.1.3 / GSA 1-11, matching this whole pipeline's
  ## scope everywhere else.
  star_wm <- star_ram_combined[grepl("Western Mediterranean", subregion, fixed = TRUE)]
  
  ## Species -> FG_num, exact scientific-name match first, genus fallback
  ## second - identical cascade to the SAU species match above.
  star_wm[, genus := extract_genus(species)]
  star_direct <- merge(star_wm, fg_lookup[, .(ScientificName, FG_num, FG_name)], by.x = "species", by.y = "ScientificName")
  star_remaining <- fsetdiff(star_wm[, .(source, stock_key, species, gsa, year)], star_direct[, .(source, stock_key, species, gsa, year)])  # rows not resolved by the exact-name match
  star_remaining <- merge(star_remaining, star_wm, by = c("source", "stock_key", "species", "gsa", "year"))
  star_genus <- merge(star_remaining[!is.na(genus)], fg_lookup[!is.na(genus), .(genus, FG_num, FG_name)], by = "genus", allow.cartesian = TRUE)
  star_matched <- rbindlist(list(star_direct, star_genus), use.names = TRUE, fill = TRUE)
  
  n_star_unmatched <- uniqueN(star_wm$species) - uniqueN(star_matched$species)
  message("\n[STAR/RAM cross-check] ", uniqueN(star_matched$species), " of ", uniqueN(star_wm$species),
          " assessed species matched to a FG (exact name or genus fallback); ", max(n_star_unmatched, 0),
          " species could not be matched and are excluded from this cross-check.")
  
  star_crosswalk <- merge(data.table(species = unique(star_wm$species)),
                          unique(star_matched[, .(species, FG_num, FG_name)]), by = "species", all.x = TRUE)
  species_fg_crosswalk_parts[["STAR_RAM"]] <- star_crosswalk[, .(DataSource = "STAR/RAM", RawIdentifier = species,
                                                                 ScientificName = species, FG_num, FG_name,
                                                                 Matched = !is.na(FG_num))]
  
  ## Sum Catches/Landings across every West Med GSA (stock assessments
  ## report an absolute total, not a density, so summing across GSAs
  ## within the model's West Med scope gives the whole-domain total
  ## directly - same principle already used for the GSA-level area
  ## discussion elsewhere in this script). Landings rows flagged
  ## landings_flag (Landings > Catches - a known data-entry error per
  ## analysis_STAR.R's own sanity check) are excluded from the landings
  ## sum, same as her own combine_STAR_RAMlegacy.R plots already do.
  star_catch_by_fg <- star_matched[, .(
    star_catches_t  = sum(catches, na.rm = TRUE),
    star_landings_t = sum(landings[landings_flag %in% FALSE], na.rm = TRUE),
    star_n_stocks   = uniqueN(stock_key),
    star_sources    = paste(sort(unique(source)), collapse = "+")
  ), by = .(FG_num, FG_name, Year = year)]
  
  ## Discard ratio implied by STAR/RAM: neither source reports a
  ## separate discard figure (see this block's own header comment), but
  ## Catches (= Landings + Discards, by definition) minus Landings gives
  ## an implied discard total, and dividing by Catches gives the SAME
  ## Di/C ratio this pipeline computes elsewhere (discard_ratio =
  ## Discard_t / Catch_t) - directly comparable, just fleet-blind and
  ## restricted to assessed stocks. NA where star_landings_t is 0 (would
  ## make this look like a 100% discard ratio, which is really "no
  ## landings figure available for this FG/year", not a real 100% rate).
  star_catch_by_fg[, star_discard_ratio := fifelse(
    star_catches_t > 0 & star_landings_t > 0, (star_catches_t - star_landings_t) / star_catches_t, NA_real_
  )]
  fwrite(star_catch_by_fg, file.path(csv_out_dir, "star_ram_catch_by_fg_crosscheck.csv"))
  message("[STAR/RAM cross-check] star_catch_by_fg: ", nrow(star_catch_by_fg), " FG x Year row(s) with an assessed-stock",
          " catch figure (", uniqueN(star_catch_by_fg$FG_num), " FG(s) total) - written to star_ram_catch_by_fg_crosscheck.csv.")
}

## =================================================================
## ICCAT nominal catches - Atlantic bluefin tuna, Mediterranean
## swordfish, Mediterranean albacore (2026-09-23, added at the user's
## explicit request as a THIRD, independent catch/landings source
## alongside GFCM/FDI/SAU above and the GFCM STAR/RAM Legacy cross-
## check below). These three are ICCAT-managed highly-migratory
## stocks, assessed by ICCAT directly - not GFCM - and GFCM's own
## STATLANT capture-production product resolves them only weakly
## (this is very likely why the validation plots showed essentially no
## swordfish catch at all through the GFCM route). RAM Legacy above
## sometimes carries an ICCAT-origin stock, but not reliably for all
## three every run - this is a dedicated, always-checked source for
## exactly these three species, independent of whether RAM Legacy
## happens to include them.
##
## Downloaded and parsed directly here (2026-09-23) - no manual CSV
## export step. ICCAT's own bulk "Task I - nominal catches" data is
## published as a dated zip (Excel pivot export) linked from
## https://www.iccat.int/en/accesingdb.HTML - fetch_iccat_task1_
## nominal_catches() below scrapes that page for the CURRENT
## "Data/t1nc_YYYYMMDD.zip" link each run (the date changes whenever
## ICCAT refreshes the underlying data, so it can't be hardcoded
## reliably long-term), falling back to the one known-good URL
## captured when this was written if that scrape ever breaks. The
## download is cached under ICCAT_DIR (same "don't redo expensive work
## every run" principle as this pipeline's bathymetry cache) - delete
## the extracted folder there to force a fresh pull.
##
## IMPORTANT CAVEAT: I could not actually reach iccat.int from my own
## sandbox to inspect the real file (outbound access to it was
## blocked there), so the parsing below is a best-effort guess at
## ICCAT's actual pivot-export layout, not something verified against
## the real file. It tries several plausible column-header aliases per
## required field (ICCAT_COL_ALIASES) across every sheet/file the zip
## contains, and if NONE of them look right, it writes every
## candidate's actual header row to ICCAT_DIR/iccat_download_diagnostic.csv
## and skips gracefully (message, not a hard stop - a parsing miss on
## an enhancement source shouldn't take the whole fisheries run down)
## rather than silently mis-parsing. If this fires on your first real
## run, send me that diagnostic file (or just the real column headers)
## so ICCAT_COL_ALIASES/the sheet-selection logic can be corrected
## against ground truth.
##
## IMPORTANT (checked directly against the current FG_WMed_2026.csv,
## 2026-09-23): Bluefin tuna (FG 12, Thunnus thynnus) and Swordfish
## (FG 13, Xiphias gladius) are already their own single-species FGs,
## so their ICCAT rows will attach automatically below. Albacore
## (Thunnus alalunga) is NOT currently in FG_WMed_2026.csv at all - no
## FG represents it yet - so its ICCAT catch figures will load and
## cross-check fine but have nowhere to attach until Thunnus alalunga
## is added to FG_WMed_2026.csv as its own FG (same convention as
## Bluefin tuna/Swordfish; not something this script invents on its
## own - see 01_biomass.R's "Seed FG rules: REMOVED ENTIRELY" comment
## for why a code-side FG assignment isn't the right fix here either).
## =================================================================
ICCAT_DIR <- file.path(pcloud_dir, "data/fisheries/ICCAT")

## The 3 species this pipeline asks ICCAT for, by ICCAT's own 3-letter
## species code AND scientific name (matched on whichever the download
## actually provides - code first, name as fallback).
ICCAT_SPECIES <- data.table(
  iccat_code     = c("BFT", "SWO", "ALB"),
  ScientificName = c("Thunnus thynnus", "Xiphias gladius", "Thunnus alalunga")
)

## Plausible column-header spellings per required field, tried in
## order. Extend this list (rather than hand-editing the loader below)
## if the real ICCAT download uses a header not listed here - see this
## block's own header comment for how to find out what that is.
ICCAT_COL_ALIASES <- list(
  year         = c("Yearc", "YearC", "Year", "YEAR", "year", "Yr"),
  species      = c("Species", "SpeciesCode", "sp_code"),
  species_name = c("SpeciesName", "SpName", "CommonName"),
  area         = c("AreaName", "Area", "Ocean", "Region", "Stock"),
  flag         = c("Flag", "FlagName", "Country"),
  catch_t      = c("Qty_t", "Qty", "Catch_t", "CatchWt", "Catch(t)", "Value")
)
resolve_iccat_col <- function(dt_names, aliases) {
  hit <- intersect(aliases, dt_names)
  if (length(hit) == 0) NA_character_ else hit[1]
}

## Downloads (and caches) ICCAT's own Task I nominal-catches zip, then
## scans every file/sheet it contains for one that has a recognizable
## year + catch-weight + species column set (via ICCAT_COL_ALIASES
## above), returning the FIRST one that resolves. Returns NULL (never
## errors) if the download itself fails, or if nothing inside it can
## be recognized - see this block's own header comment for what
## happens then (a diagnostic CSV + a graceful skip).
fetch_iccat_task1_nominal_catches <- function(cache_dir) {
  if (!dir.exists(cache_dir)) dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  
  page_url <- "https://www.iccat.int/en/accesingdb.HTML"
  fallback_zip_url <- "https://www.iccat.int/Data/t1nc_20260129.zip"  # known-good as of 2026-09-23 - used only if the page scrape below fails
  zip_url <- tryCatch({
    page_txt <- paste(readLines(page_url, warn = FALSE), collapse = "\n")
    hit <- regmatches(page_txt, regexpr("Data/t1nc_[0-9]{8}\\.zip", page_txt))
    if (length(hit) == 1 && nzchar(hit)) paste0("https://www.iccat.int/", hit) else fallback_zip_url
  }, error = function(e) fallback_zip_url)
  
  zip_file    <- file.path(cache_dir, basename(zip_url))
  extract_dir <- file.path(cache_dir, tools::file_path_sans_ext(basename(zip_url)))
  if (!dir.exists(extract_dir)) {
    if (!file.exists(zip_file)) {
      message("[ICCAT] Downloading ICCAT's own Task I nominal-catches export from '", zip_url, "' ...")
      ok <- tryCatch({
        download.file(zip_url, destfile = zip_file, mode = "wb", method = "libcurl")
        TRUE
      }, error = function(e) { message("[ICCAT] Download failed: ", conditionMessage(e)); FALSE })
      if (!ok) return(NULL)
    }
    unzip(zip_file, exdir = extract_dir)
  } else {
    message("[ICCAT] Using cached extract at '", extract_dir, "' (delete this folder to force a fresh download).")
  }
  
  candidate_files <- list.files(extract_dir, pattern = "\\.(csv|xlsx|xls)$",
                                full.names = TRUE, recursive = TRUE, ignore.case = TRUE)
  if (length(candidate_files) == 0) {
    message("[ICCAT] No .csv/.xlsx/.xls file found inside the downloaded archive - contents: ",
            paste(list.files(extract_dir, recursive = TRUE), collapse = ", "))
    return(NULL)
  }
  
  looks_valid <- function(dt) {
    !is.na(resolve_iccat_col(names(dt), ICCAT_COL_ALIASES$year)) &&
      !is.na(resolve_iccat_col(names(dt), ICCAT_COL_ALIASES$catch_t)) &&
      (!is.na(resolve_iccat_col(names(dt), ICCAT_COL_ALIASES$species)) ||
         !is.na(resolve_iccat_col(names(dt), ICCAT_COL_ALIASES$species_name)))
  }
  
  diagnostic <- list()
  for (f in candidate_files) {
    if (grepl("\\.csv$", f, ignore.case = TRUE)) {
      dt <- tryCatch(fread(f, encoding = "UTF-8"), error = function(e) NULL)
      if (!is.null(dt) && nrow(dt) > 0) {
        if (looks_valid(dt)) { message("[ICCAT] Parsed '", basename(f), "' directly."); return(dt) }
        diagnostic[[f]] <- names(dt)
      }
    } else {
      sheets <- tryCatch(readxl::excel_sheets(f), error = function(e) character(0))
      for (sh in sheets) {
        dt <- tryCatch(as.data.table(readxl::read_excel(f, sheet = sh)), error = function(e) NULL)
        if (!is.null(dt) && nrow(dt) > 0) {
          if (looks_valid(dt)) {
            message("[ICCAT] Parsed sheet '", sh, "' of '", basename(f), "'.")
            return(dt)
          }
          diagnostic[[paste0(basename(f), "::", sh)]] <- names(dt)
        }
      }
    }
  }
  
  ## Nothing recognized - this is a download/parse issue on THIS
  ## script's side (the file downloaded fine), not something wrong
  ## with the user's own data, so dump every candidate's actual header
  ## row for a fix against ground truth, and degrade gracefully
  ## (message + skip, matching every other optional source in this
  ## pipeline) rather than stop() and take the whole fisheries run
  ## down with it.
  if (length(diagnostic) > 0) {
    diag_dt <- rbindlist(lapply(names(diagnostic), function(nm) {
      data.table(source = nm, columns = paste(diagnostic[[nm]], collapse = " | "))
    }))
    diag_path <- file.path(cache_dir, "iccat_download_diagnostic.csv")
    fwrite(diag_dt, diag_path)
    message("[ICCAT] Downloaded the archive but couldn't automatically recognize a year + catch-weight +",
            " species column set in any of its ", length(candidate_files), " file(s)/sheet(s) - see '",
            diag_path, "' for every candidate's actual header row. Send that file back (or just the real",
            " column headers) to fix ICCAT_COL_ALIASES/the sheet-selection logic against the real structure -",
            " skipping the ICCAT catch cross-check/priority for this run.")
  }
  NULL
}

iccat_catch_by_fg <- data.table()
iccat_raw <- fetch_iccat_task1_nominal_catches(ICCAT_DIR)
if (is.null(iccat_raw)) {
  message("\n[ICCAT] No usable Task I data this run - skipping (see the messages above for why: download",
          " failure, no recognizable file inside the archive, or no candidate matched ICCAT_COL_ALIASES).",
          " Bluefin tuna/swordfish/albacore fall back to GFCM STAR/RAM Legacy (if available) or the",
          " GFCM/FDI/SAU-derived figure, same as before this addition existed.")
} else {
  col_year    <- resolve_iccat_col(names(iccat_raw), ICCAT_COL_ALIASES$year)
  col_catch   <- resolve_iccat_col(names(iccat_raw), ICCAT_COL_ALIASES$catch_t)
  col_species <- resolve_iccat_col(names(iccat_raw), ICCAT_COL_ALIASES$species)
  col_spname  <- resolve_iccat_col(names(iccat_raw), ICCAT_COL_ALIASES$species_name)
  col_area    <- resolve_iccat_col(names(iccat_raw), ICCAT_COL_ALIASES$area)
  col_flag    <- resolve_iccat_col(names(iccat_raw), ICCAT_COL_ALIASES$flag)
  
  ## No is.na() guard needed here (unlike the old hand-prepared-CSV
  ## version of this block) - fetch_iccat_task1_nominal_catches() above
  ## only ever returns a candidate that already passed this exact
  ## looks_valid() check, so col_year/col_catch/(col_species or
  ## col_spname) are guaranteed non-NA at this point.
  setnames(iccat_raw, col_year, "Year")
  setnames(iccat_raw, col_catch, "Catch_t_iccat")
  iccat_raw[, `:=`(Year = as.integer(Year), Catch_t_iccat = as.numeric(Catch_t_iccat))]
  
  ## Restrict to the 3 requested species, matched on whichever of
  ## species-code/species-name the file actually has - code first
  ## (unambiguous; ICCAT's own SpeciesName spelling varies by export,
  ## e.g. "Albacore" vs "Albacore tuna" vs "ALB - Albacore").
  if (!is.na(col_species)) {
    setnames(iccat_raw, col_species, "iccat_code")
    iccat_raw <- merge(iccat_raw, ICCAT_SPECIES, by = "iccat_code")
  } else {
    setnames(iccat_raw, col_spname, "iccat_species_name")
    iccat_name_match <- data.table(
      iccat_species_name = c("Bluefin tuna", "Atlantic bluefin tuna", "Swordfish",
                             "Albacore", "Albacore tuna", "Albacore, N Atl."),
      ScientificName     = c("Thunnus thynnus", "Thunnus thynnus", "Xiphias gladius",
                             "Thunnus alalunga", "Thunnus alalunga", "Thunnus alalunga")
    )
    iccat_raw <- merge(iccat_raw, iccat_name_match, by = "iccat_species_name")
  }
  
  ## Mediterranean-only where an area/stock field exists to filter on.
  ## Bluefin tuna is assessed by ICCAT as ONE Eastern Atlantic +
  ## Mediterranean stock (no separate Med-only breakdown exists - per
  ## the species table this addition was requested against), so this
  ## filter is deliberately a no-op for BFT specifically and only
  ## actually restricts SWO/ALB, which ICCAT does assess as their own
  ## Mediterranean-specific stock unit.
  if (!is.na(col_area)) {
    setnames(iccat_raw, col_area, "iccat_area")
    n_before_area <- nrow(iccat_raw)
    ## 2026-09-23 fix: ICCAT's real area/stock field is often a short
    ## code (e.g. "MED") rather than the spelled-out word
    ## "Mediterranean", so the original grepl("medit", ...) - which
    ## needs a 5-letter "medit" substring - silently matched ZERO rows
    ## for swordfish/albacore whenever the download used the short
    ## code, even though bluefin tuna (exempted from this filter
    ## entirely) kept working. That is the confirmed cause of "[ICCAT] 2
    ## of the 3 requested ICCAT species have no matching FG" always
    ## naming exactly SWO + ALB: their rows were being filtered down to
    ## nothing here, before ever reaching the fg_lookup merge - not a
    ## fg_lookup/ScientificName matching problem at all. "\\bmed" (word-
    ## boundary "med") matches both the short code ("MED", "MED-SWO")
    ## and the spelled-out word ("Mediterranean") without also matching
    ## unrelated area codes.
    species_before <- unique(iccat_raw[ScientificName != "Thunnus thynnus", .(ScientificName, iccat_area)])
    iccat_raw <- iccat_raw[grepl("\\bmed", iccat_area, ignore.case = TRUE) | ScientificName == "Thunnus thynnus"]
    message("[ICCAT] Area filter: ", nrow(iccat_raw), " of ", n_before_area, " row(s) kept (Mediterranean-",
            "labeled area/stock - matched on either the spelled-out word or a short code like \"MED\" -",
            " or bluefin tuna - assessed as one Eastern Atlantic + Mediterranean stock with",
            " no separate Med breakdown, so kept unfiltered by area).")
    ## Cross-check: if a non-bluefin species now has ZERO rows post-
    ## filter despite having had rows pre-filter, the area filter is
    ## still not recognizing that species' real area/stock code -
    ## surface the actual values so this can be fixed against the real
    ## download rather than failing silently downstream as "no matching
    ## FG".
    species_after <- unique(iccat_raw[, ScientificName])
    species_zeroed <- setdiff(unique(species_before$ScientificName), species_after)
    if (length(species_zeroed) > 0) {
      zeroed_areas <- unique(species_before[ScientificName %in% species_zeroed, .(ScientificName, iccat_area)])
      message("[ICCAT] WARNING: the area filter kept 0 row(s) for ", paste(species_zeroed, collapse = ", "),
              " even though it had row(s) before filtering. Its real area/stock value(s) in this download: ",
              paste(sprintf("%s='%s'", zeroed_areas$ScientificName, zeroed_areas$iccat_area), collapse = "; "),
              " - none of these matched the Mediterranean pattern used above; adjust that pattern (or",
              " ICCAT_COL_ALIASES$area) to match whatever code this download actually uses.")
    }
  } else {
    message("[ICCAT] No area/stock column found to filter to the Mediterranean - using every row for the 3",
            " requested species as-is (ICCAT's Task I export is Atlantic-wide, so this may include non-",
            " Mediterranean catch for swordfish/albacore). Add the real area/stock column's header to",
            " ICCAT_COL_ALIASES$area above once the download's actual structure is known.")
  }
  
  iccat_raw <- iccat_raw[Year >= START_YEAR & Year <= END_YEAR & !is.na(Catch_t_iccat)]
  
  iccat_matched <- merge(unique(iccat_raw[, .(ScientificName)]), fg_lookup[, .(ScientificName, FG_num, FG_name)],
                         by = "ScientificName")
  iccat_raw <- merge(iccat_raw, iccat_matched, by = "ScientificName")
  
  n_iccat_unmatched <- uniqueN(ICCAT_SPECIES$ScientificName) - uniqueN(iccat_matched$ScientificName)
  if (n_iccat_unmatched > 0) {
    unmatched_sp <- setdiff(ICCAT_SPECIES$ScientificName, iccat_matched$ScientificName)
    message("[ICCAT] ", n_iccat_unmatched, " of the ", uniqueN(ICCAT_SPECIES$ScientificName), " requested ICCAT",
            " species have no matching FG in the current FG_WMed_2026.csv, so their ICCAT rows loaded but",
            " couldn't attach anywhere: ", paste(unmatched_sp, collapse = ", "), ". Add the species to",
            " FG_WMed_2026.csv as its own FG (same convention as Bluefin tuna/FG12, Swordfish/FG13) if you want",
            " it picked up here - this loader deliberately does not invent an FG for it.")
  }
  
  iccat_crosswalk <- merge(ICCAT_SPECIES, iccat_matched, by = "ScientificName", all.x = TRUE)  # every requested ICCAT species, matched or not
  species_fg_crosswalk_parts[["ICCAT"]] <- iccat_crosswalk[, .(DataSource = "ICCAT", RawIdentifier = ScientificName,
                                                               ScientificName, FG_num, FG_name, Matched = !is.na(FG_num))]
  
  ## Sum across flags/gears to one Catch_t per FG x Year - ICCAT's Task
  ## I is reported per flag(country) x gear x year, and the whole-
  ## stock total (summed across every reporting flag) is what belongs
  ## in a FG's whole-domain catch figure, same principle as GFCM STAR/
  ## RAM's own summed-across-GSA total below.
  iccat_catch_by_fg <- iccat_raw[, .(
    iccat_catches_t = sum(Catch_t_iccat, na.rm = TRUE),
    iccat_n_flags   = if (!is.na(col_flag) && "Flag" %in% names(iccat_raw)) uniqueN(Flag) else NA_integer_
  ), by = .(FG_num, FG_name, Year)]
  
  fwrite(iccat_catch_by_fg, file.path(csv_out_dir, "iccat_catch_by_fg_crosscheck.csv"))
  message("[ICCAT] iccat_catch_by_fg: ", nrow(iccat_catch_by_fg), " FG x Year row(s) (",
          uniqueN(iccat_catch_by_fg$FG_num), " FG(s) total) - written to iccat_catch_by_fg_crosscheck.csv.")
}

## Attach as comparison-only columns on catches_discards_fg - NEVER
## replacing Catch_t/Landings_t, just sitting alongside them so a
## meaningful disagreement is visible rather than requiring a separate
## join every time someone wants to check.
catches_discards_fg[, `:=`(iccat_catches_t = NA_real_)]
if (nrow(iccat_catch_by_fg) > 0) {
  catches_discards_fg[iccat_catch_by_fg, on = c("FG_num", "Year"), iccat_catches_t := i.iccat_catches_t]
  catches_discards_fg[, iccat_catch_pct_diff := fifelse(!is.na(iccat_catches_t) & iccat_catches_t > 0,
                                                        round(100 * (Catch_t - iccat_catches_t) / iccat_catches_t, 1), NA_real_)]  # this pipeline's Catch_t vs ICCAT's, % difference - positive = this pipeline reports MORE
  n_flagged_iccat <- catches_discards_fg[!is.na(iccat_catch_pct_diff) & abs(iccat_catch_pct_diff) > 50, .N]
  message("[ICCAT cross-check] ", catches_discards_fg[!is.na(iccat_catches_t), .N], " FG x Year cell(s) have an",
          " ICCAT catch figure; ", n_flagged_iccat, " of those disagree with this pipeline's own Catch_t by more",
          " than 50% - see iccat_catch_pct_diff. Computed against Catch_t BEFORE the stock-assessment PRIORITY",
          " override below runs - for single-species/stanza assessed FGs, Catch_t is then replaced with ICCAT's",
          " own figure first (ICCAT takes priority over GFCM STAR/RAM below for any FG x Year cell it covers,",
          " since ICCAT is the actual assessing body for these highly-migratory stocks); every other FG stays a",
          " cross-check only.")
} else {
  catches_discards_fg[, iccat_catch_pct_diff := NA_real_]
}

## Attach as comparison-only columns on catches_discards_fg - NEVER
## replacing Catch_t/Landings_t, just sitting alongside them so a
## meaningful disagreement is visible rather than requiring a separate
## join every time someone wants to check.
catches_discards_fg[, `:=`(star_catches_t = NA_real_, star_landings_t = NA_real_, star_n_stocks = NA_integer_,
                           star_sources = NA_character_, star_discard_ratio = NA_real_)]
if (nrow(star_catch_by_fg) > 0) {
  catches_discards_fg[star_catch_by_fg, on = c("FG_num", "Year"), `:=`(
    star_catches_t = i.star_catches_t, star_landings_t = i.star_landings_t,
    star_n_stocks = i.star_n_stocks, star_sources = i.star_sources, star_discard_ratio = i.star_discard_ratio
  )]
  catches_discards_fg[, star_catch_pct_diff := fifelse(!is.na(star_catches_t) & star_catches_t > 0,
                                                       round(100 * (Catch_t - star_catches_t) / star_catches_t, 1), NA_real_)]  # this pipeline's Catch_t vs STAR/RAM's, % difference - positive = this pipeline reports MORE
  ## Same comparison for the discard ratio - this pipeline's own
  ## discard_ratio (FishMIP/SAU-derived, FG x Year, time-varying) vs
  ## STAR/RAM's implied Di/C for the same cell. Both use the SAME Di/C
  ## definition (see the fisheries flow diagram's Stage 3 formula), so
  ## a disagreement here is a real signal, not a units/definition
  ## mismatch.
  catches_discards_fg[, star_discard_ratio_diff_pp := fifelse(
    !is.na(star_discard_ratio) & !is.na(discard_ratio), round(100 * (discard_ratio - star_discard_ratio), 1), NA_real_
  )]  # difference in PERCENTAGE POINTS (not %), since both are already ratios - positive = this pipeline's discard ratio is higher
  n_flagged <- catches_discards_fg[!is.na(star_catch_pct_diff) & abs(star_catch_pct_diff) > 50, .N]  # arbitrary but generous threshold - just surfaces the biggest disagreements for a human look
  n_flagged_discard <- catches_discards_fg[!is.na(star_discard_ratio_diff_pp) & abs(star_discard_ratio_diff_pp) > 15, .N]  # 15 percentage points - generous, just surfaces the biggest gaps
  message("[STAR/RAM cross-check] ", catches_discards_fg[!is.na(star_catches_t), .N], " FG x Year cell(s) have a",
          " STAR/RAM cross-check figure; ", n_flagged, " of those disagree with this pipeline's own Catch_t by",
          " more than 50% - see star_catch_pct_diff. Of the cells with an implied STAR/RAM discard ratio (Catches",
          " and Landings both > 0), ", n_flagged_discard, " disagree with this pipeline's own discard_ratio by more",
          " than 15 percentage points - see star_discard_ratio_diff_pp. These comparisons are computed against",
          " Catch_t/discard_ratio BEFORE the stock-assessment PRIORITY override below runs - for single-species/",
          " stanza assessed FGs, Catch_t/discard_ratio are then replaced with STAR/RAM's own figures; for every",
          " other FG, these stay cross-checks only.")
} else {
  catches_discards_fg[, `:=`(star_catch_pct_diff = NA_real_, star_discard_ratio_diff_pp = NA_real_)]
}

## =================================================================
## Stock-assessment catch/landings PRIORITY for single-species/stanza
## assessed FGs (2026-09-17) - e.g. tuna, swordfish, and any other
## highly-migratory or otherwise individually-assessed large pelagic
## whose real Mediterranean-stock catch record is its own GFCM STAR/
## RAM Legacy stock assessment, not GFCM's STATLANT capture-production
## aggregate. Mirrors the biomass-side priority rule already
## implemented in 01_biomass.R (single-species/stanza FG + a real
## stock-assessment value -> use it, else fall back) - applied here to
## Catch_t/Landings_t/Discard_t instead of biomass density, and no
## longer just a cross-check column (star_catches_t/star_landings_t
## above): for exactly the FG x Year cells that qualify, STAR/RAM's own
## Catches/Landings figures REPLACE the GFCM/FDI/SAU-derived Catch_t/
## Landings_t/Discard_t.
##
## Qualifies = single-species FG (n_species_in_fg == 1, same FG
## resolution as fg_lookup above - computed independently here rather
## than read back from 01_biomass.R's fg_ecology_classification.csv,
## so this script keeps working standalone) AND a real
## star_catches_t > 0 for that specific Year. Every other cell is
## untouched - including OTHER years of the SAME single-species FG
## where STAR/RAM simply has no value for that year; those still fall
## back to the GFCM-derived figure, same row-wise (never blanket)
## fallback principle as the biomass cascade.
##
## Landings_t: uses star_landings_t directly where STAR/RAM reports a
## real (>0) landings figure; where STAR/RAM only reports Catches (no
## usable landings breakdown that Year), Landings_t is backed out
## using this pipeline's own discard_ratio for that FG/Year if
## available, else assumed equal to Catch_t (discard_ratio = 0) -
## flagged via catch_source either way, never silently blended with
## the GFCM Catch_t itself.
## =================================================================
n_species_in_fg_catch <- unique(fg_lookup[, .(ScientificName, FG_num)])[, .(n_species_in_fg = uniqueN(ScientificName)), by = FG_num]
catches_discards_fg <- merge(catches_discards_fg, n_species_in_fg_catch, by = "FG_num", all.x = TRUE)
catches_discards_fg[, catch_source := "GFCM/FDI/SAU-derived (default)"]

## Tier 1: ICCAT (2026-09-23) - applied FIRST and takes priority over
## STAR/RAM below wherever it covers a cell, since ICCAT is the actual
## RFMO assessing bluefin tuna/swordfish/albacore directly (GFCM STAR/
## RAM Legacy's own bluefin/swordfish coverage, when present at all, is
## typically itself just a re-publication of ICCAT's assessment one
## step removed). Same single-species/stanza-FG qualification rule as
## STAR/RAM.
if (nrow(iccat_catch_by_fg) > 0) {
  use_iccat <- catches_discards_fg[, !is.na(n_species_in_fg) & n_species_in_fg == 1 & !is.na(iccat_catches_t) & iccat_catches_t > 0]
  n_overridden_iccat <- sum(use_iccat, na.rm = TRUE)
  if (n_overridden_iccat > 0) {
    catches_discards_fg[use_iccat, `:=`(
      Catch_t      = iccat_catches_t,
      ## ICCAT's Task I nominal catches has no separate landings/discard
      ## breakdown either (same limitation as STAR/RAM above) - back out
      ## Landings_t via this pipeline's own discard_ratio for that FG/
      ## Year if available, else assume Landings_t = Catch_t (discard_ratio = 0).
      Landings_t   = iccat_catches_t * (1 - fifelse(is.na(discard_ratio), 0, discard_ratio)),
      catch_source = "stock assessment (ICCAT Task I nominal catches) - single species/stanza FG"
    )]
    catches_discards_fg[use_iccat, Discard_t := Catch_t - Landings_t]
    message("\n[Catches] ICCAT PRIORITY applied: ", n_overridden_iccat, " FG x Year cell(s) (",
            uniqueN(catches_discards_fg[use_iccat == TRUE, FG_num]), " single-species/stanza assessed FG(s)) now",
            " use ICCAT's own nominal catch directly as Catch_t/Landings_t/Discard_t, replacing the GFCM/FDI/",
            "SAU-derived value for exactly those cells - see catch_source. These cells are now also excluded",
            " from the STAR/RAM tier below (ICCAT wins wherever both cover the same cell).")
  } else {
    message("\n[Catches] No FG qualifies for the ICCAT catch-priority override (needs a single-species FG AND a",
            " real iccat_catches_t > 0 for at least one Year). If bluefin tuna/swordfish were expected to",
            " qualify, check iccat_catch_by_fg_crosscheck.csv - both are already their own single-species FG",
            " (12/13) in the current FG_WMed_2026.csv, so an empty result here means the ICCAT CSV itself had",
            " no matching rows for them, not a FG-reference gap.")
  }
} else {
  use_iccat <- rep(FALSE, nrow(catches_discards_fg))
}

## Tier 2: GFCM STAR/RAM Legacy - same as before, but now only applied
## to cells ICCAT's tier above did NOT already override (!use_iccat),
## so ICCAT's more-authoritative figure for bluefin tuna/swordfish/
## albacore is never silently overwritten by a less-authoritative one.
if (nrow(star_catch_by_fg) > 0) {
  use_star <- catches_discards_fg[, !use_iccat & !is.na(n_species_in_fg) & n_species_in_fg == 1 & !is.na(star_catches_t) & star_catches_t > 0]
  n_overridden <- sum(use_star, na.rm = TRUE)
  if (n_overridden > 0) {
    catches_discards_fg[use_star, `:=`(
      Catch_t    = star_catches_t,
      Landings_t = fifelse(!is.na(star_landings_t) & star_landings_t > 0, star_landings_t,
                           star_catches_t * (1 - fifelse(is.na(discard_ratio), 0, discard_ratio))),
      catch_source = paste0("stock assessment (STAR/RAM, ", star_sources, ") - single species/stanza FG")
    )]
    catches_discards_fg[use_star, Discard_t := Catch_t - Landings_t]
    message("\n[Catches] Stock-assessment PRIORITY applied: ", n_overridden, " FG x Year cell(s) (",
            uniqueN(catches_discards_fg[use_star == TRUE, FG_num]), " single-species/stanza assessed FG(s)) now use",
            " STAR/RAM's own Catches/Landings directly as Catch_t/Landings_t/Discard_t, replacing the",
            " GFCM/FDI/SAU-derived value for exactly those cells - see catch_source. Every other cell (including",
            " other Years for the SAME FG where no star value exists, and any cell ICCAT's tier above already",
            " claimed) is untouched.")
  } else {
    message("\n[Catches] No FG qualifies for the stock-assessment catch-priority override (needs a single-species",
            " FG AND a real star_catches_t > 0 for at least one Year, on a cell ICCAT didn't already claim) -",
            " catches_discards_fg stays fully GFCM/FDI/SAU-derived (or ICCAT-derived, per the tier above) for",
            " every remaining cell. If tuna/swordfish/other assessed stocks were expected to qualify, check",
            " they're each their own single-species FG in the FG reference AND have a matching row in",
            " combined_medbs_star_ramlegacy.csv (exact species name or genus).")
  }
}
catches_discards_fg[, n_species_in_fg := NULL]

## --- Final "expected catch but got zero" cross-check (2026-09-23) ------
## Answers directly the question "am I losing FG data because the catch
## data's species/groups don't match my FG list?" A FG showing zero
## Catch_t across the WHOLE series is only "honestly empty" (see the
## fg_catch_grid comment above) if NO species is even assigned to it in
## FG_WMed_2026.csv, or it's a juvenile stanza whose catch is deliberately
## folded into its adult stanza (see the stanza tie-break near fg_lookup's
## own definition). A FG that DOES have one or more species assigned to
## it in fg_lookup, but still ends up at zero catch in every year, is the
## real signature of this failure mode: those species' names in the catch
## sources (GFCM/STECF FDI/SAU/ICCAT/STAR-RAM) never matched fg_lookup's
## spelling, so their real catch was dropped upstream (see each source's
## own "unresolved"/"dropped" message and species_fg_matched.csv) instead
## of ever reaching this FG.
fg_zero_catch_ever <- catches_discards_fg[, .(total_catch = sum(Catch_t, na.rm = TRUE)), by = .(FG_num, FG_name)][total_catch == 0]
if (nrow(fg_zero_catch_ever) > 0) {
  fg_species_assigned <- fg_lookup[, .(species_assigned = paste(unique(ScientificName), collapse = "; ")), by = FG_num]
  fg_zero_catch_ever <- merge(fg_zero_catch_ever, fg_species_assigned, by = "FG_num", all.x = TRUE)
  fg_zero_catch_with_species <- fg_zero_catch_ever[!is.na(species_assigned) & species_assigned != ""]
  fg_zero_catch_no_species <- fg_zero_catch_ever[is.na(species_assigned) | species_assigned == ""]
  message("\n[Catches] FINAL CHECK - ", nrow(fg_zero_catch_ever), " FG(s) have ZERO total Catch_t across the",
          " entire series. ", nrow(fg_zero_catch_no_species), " of these have NO species assigned to them at",
          " all in FG_WMed_2026.csv (honestly empty - nothing to chase). The other ", nrow(fg_zero_catch_with_species),
          " DO have species assigned but still show zero catch - THIS is the 'catch data doesn't match my FG",
          " list' failure mode: check each species below against species_fg_matched.csv's 'status' column and",
          " the console's own 'unresolved'/'dropped' messages further up (GFCM's own FINAL MATCHING SUMMARY,",
          " STECF FDI's retry-and-drop message, SAU's, STAR/RAM's, ICCAT's) to see exactly why its catch never",
          " reached this FG.")
  if (nrow(fg_zero_catch_with_species) > 0) print(fg_zero_catch_with_species[, .(FG_num, FG_name, species_assigned)])
  fwrite(fg_zero_catch_ever, file.path(csv_out_dir, "fg_zero_catch_diagnostic.csv"))  # full detail for offline review
}

## --- Combined species/group -> FG crosswalk, every data source in one
## CSV (2026-09-23) -------------------------------------------------------
## One row per distinct raw species/group name PER SOURCE (GFCM, STECF
## FDI, SAU, STAR/RAM, ICCAT), whether it matched an FG or not - this is
## the direct answer to "which group in which catch/discards data source
## is assigned to which FG", to check against the FGs that show missing
## landings/discards in the workbook. FG_name here is the FG this
## species/code/group name resolved to (NA if it never matched anything);
## Matched = FALSE rows are exactly the ones worth chasing down first.
species_fg_crosswalk_all <- rbindlist(species_fg_crosswalk_parts, use.names = TRUE, fill = TRUE)
if (nrow(species_fg_crosswalk_all) > 0) {
  ## 2026-09-23 addition: flag any RawIdentifier that resolves to MORE
  ## THAN ONE distinct FG within the same DataSource - e.g. a raw
  ## species/group name/code matched inconsistently across different
  ## rows in that source's own matching cascade (could be a genuine
  ## stanza split, or could be a real matching bug worth chasing - this
  ## doesn't guess which, it just surfaces it). A name that legitimately
  ## split into juvenile/adult stanza FGs will show up here too; check
  ## FG_name for both rows before assuming it's an error.
  ambiguous_matches <- species_fg_crosswalk_all[
    Matched == TRUE, .(n_distinct_fg = uniqueN(FG_num)), by = .(DataSource, RawIdentifier)
  ][n_distinct_fg > 1]
  species_fg_crosswalk_all[, ambiguous_multi_FG := FALSE]
  if (nrow(ambiguous_matches) > 0) {
    species_fg_crosswalk_all[
      ambiguous_matches, ambiguous_multi_FG := TRUE,
      on = c("DataSource", "RawIdentifier")
    ]
  }
  
  ## 2026-09-23 addition: a best-effort reason for every UNMATCHED row -
  ## not another matching attempt, just categorizing why the existing
  ## cascades (direct FG-name, FishBase common name, FAO exact
  ## scientific name, FAO genus, word containment) already came up
  ## empty, so it's obvious at a glance which unmatched rows are worth
  ## chasing (a specific, real Mediterranean species FG_WMed_2026.csv
  ## just doesn't happen to list yet) versus which are structurally
  ## unresolvable (a taxonomic aggregate with no single species to map
  ## to an FG at all).
  ## NOTE: ScientificName is only ever populated for STECF FDI/SAU/
  ## STAR-RAM/ICCAT here - GFCM's own crosswalk row (species_fg_
  ## crosswalk_parts[["GFCM"]] above) deliberately sets it to NA for
  ## every row, matched or not, since resolved_final doesn't retain
  ## which scientific name (if any) an unmatched common name resolved
  ## to along GFCM's multi-step cascade. So the "resolved a name but no
  ## FG" vs "nothing recognized at all" distinction below only applies
  ## to the sources where ScientificName is actually meaningful; GFCM's
  ## unmatched rows get the generic message instead of a guess.
  aggregate_pattern <- "\\bnei\\b|\\betc\\b|\\bspp?\\.?$|,\\s*etc\\.?$"
  species_fg_crosswalk_all[, Reason_unmatched := NA_character_]
  species_fg_crosswalk_all[
    Matched == FALSE & grepl(aggregate_pattern, RawIdentifier, ignore.case = TRUE),
    Reason_unmatched := "Aggregate/NEI category (family- or order-level group) - no single scientific species to map to one FG"
  ]
  species_fg_crosswalk_all[
    Matched == FALSE & is.na(Reason_unmatched) & DataSource == "GFCM",
    Reason_unmatched := "No cascade step (direct FG name, FishBase common name, FAO scientific name, FAO genus, word containment) matched this common name to an FG"
  ]
  species_fg_crosswalk_all[
    Matched == FALSE & is.na(Reason_unmatched) & DataSource != "GFCM" & is.na(ScientificName),
    Reason_unmatched := "Code/name not recognized in the FAO reference at all - dropped before a scientific name could even be attempted"
  ]
  species_fg_crosswalk_all[
    Matched == FALSE & is.na(Reason_unmatched),
    Reason_unmatched := "Resolved to a scientific name, but that species/group isn't in FG_WMed_2026.csv - add it there if you want this captured (may also be a non-Mediterranean species that's genuinely out of scope)"
  ]
  
  setorder(species_fg_crosswalk_all, DataSource, -Matched, FG_num, RawIdentifier)
  fwrite(species_fg_crosswalk_all, file.path(csv_out_dir, "species_group_fg_crosswalk.csv"))
  match_summary <- species_fg_crosswalk_all[, .(n_names = .N, n_matched = sum(Matched)), by = DataSource]
  message("\n[Species/group -> FG crosswalk] species_group_fg_crosswalk.csv written - ", nrow(species_fg_crosswalk_all),
          " row(s) across ", uniqueN(species_fg_crosswalk_all$DataSource), " data source(s). Matched by source:")
  print(match_summary)
  
  if (nrow(ambiguous_matches) > 0) {
    message("\n", strrep("!", 70))
    message(nrow(ambiguous_matches), " raw species/group name(s) matched to MORE THAN ONE distinct FG",
            " within the same data source (flagged ambiguous_multi_FG = TRUE in the CSV) - review",
            " whether this is a genuine juvenile/adult stanza split or a real matching inconsistency:")
    print(merge(ambiguous_matches, unique(species_fg_crosswalk_all[Matched == TRUE,
                                                                   .(DataSource, RawIdentifier, FG_num, FG_name)]), by = c("DataSource", "RawIdentifier")))
    message(strrep("!", 70))
  } else {
    message("No raw species/group name matched more than one distinct FG within the same data source.")
  }
} else {
  message("\n[Species/group -> FG crosswalk] No source contributed any row - none of GFCM/STECF FDI/SAU/",
          "STAR-RAM/ICCAT's matching blocks ran this time (check each source's own file-not-found message above).")
}

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
  fleet_structure = NULL,
  csv_out_dir     = csv_out_dir,      # this block's own native CSVs (Catches_Ecopath/Catches_Ecosim/Fleet_Structure) go under output/fisheries/
  biomass_csv_dir = BIOMASS_CSV_DIR   # ts_years is passed explicitly above so this isn't actually used this call, but set for correctness if that ever changes
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
## bycatch rate for most FGs. Manual-entry table, resolved by Country x
## FleetType x FG (not just Country x FG - bycatch is fundamentally a
## GEAR effect, e.g. bottom trawls take far more elasmobranch bycatch
## than longlines) - fill in by hand (add rows with FG_num/FleetType/
## bycatch_rate/source_citation) as more literature/stock-assessment
## figures are found; anything left unfilled stays explicitly "not
## estimated" rather than defaulting to zero.
##
## Seeded (2026-09, to look for real data) with the
## one quantitative, gear-resolved figure found so far, for sharks & rays
## in EU Mediterranean fisheries: Bargnesi et al. 2024 (Sustainability,
## "Assessing the relevance of sharks and rays for Mediterranean EU
## fisheries") - elasmobranch discard rate by gear: bottom trawls ~40%
## (of the ~75% of elasmobranch catch trawls take); fixed nets/longlines
## <2.5%. Applied here to EU-3 (Spain/France/Italy) trawl and longline
## FleetTypes only, matched by gear-name keyword (robust to this run's
## exact FleetType spelling, e.g. "Bottom trawls" vs "Trawls -n.e.i-").
## NOT extended to Morocco/Algeria/Tunisia - the source paper covers EU
## Med fisheries specifically, and non-EU gear/discard practice is not
## confirmed to match. Every other Country x FleetType x FG cell (every
## other FG entirely, and every non-trawl/non-longline gear for sharks &
## rays too) stays "not estimated" - this is a STARTING POINT, not a
## comprehensive multi-species Mediterranean bycatch dataset.
## =================================================================
sharkray_fg <- full_fg_list[grepl("shark|ray|elasmobranch", FG_name, ignore.case = TRUE)]  # find this run's own FG_num/FG_name for sharks & rays, whatever its exact numbering
BYCATCH_RATE_MANUAL <- if (nrow(sharkray_fg) > 0) {
  rbindlist(lapply(sharkray_fg$FG_num, function(fg) data.table(
    Country = rep(c("Spain", "France", "Italy"), 2),
    FleetType_keyword = c(rep("trawl", 3), rep("longlin", 3)),  # matched against FleetType by regex below, not an exact FleetType string
    FG_num = fg,
    bycatch_rate = c(rep(0.40, 3), rep(0.025, 3)),
    source_citation = "Bargnesi et al. 2024, Sustainability - elasmobranch discard rate by gear, EU Mediterranean fisheries (bottom trawl ~40%, fixed nets/longlines <2.5%)"
  )))
} else {
  data.table(Country = character(), FleetType_keyword = character(), FG_num = integer(),
             bycatch_rate = numeric(), source_citation = character())
}
bycatch_placeholder <- unique(fleet_prop[, .(Country, FleetType, FG_num)])  # one row per country x fleet x FG for the bycatch placeholder table (fleet-resolved, not just country x FG)
bycatch_placeholder <- merge(bycatch_placeholder, unique(catch_with_unreported[, .(FG_num, FG_name)]), by = "FG_num", all.x = TRUE)  # attach FG_name for readability
if (nrow(BYCATCH_RATE_MANUAL) > 0) {
  bycatch_placeholder[, bycatch_rate := NA_real_]; bycatch_placeholder[, source_citation := NA_character_]
  for (i in seq_len(nrow(BYCATCH_RATE_MANUAL))) {
    r <- BYCATCH_RATE_MANUAL[i]
    hit <- bycatch_placeholder$Country == r$Country & bycatch_placeholder$FG_num == r$FG_num &
      grepl(r$FleetType_keyword, bycatch_placeholder$FleetType, ignore.case = TRUE)
    bycatch_placeholder[hit, `:=`(bycatch_rate = r$bycatch_rate, source_citation = r$source_citation)]
  }
} else {
  bycatch_placeholder[, `:=`(bycatch_rate = NA_real_, source_citation = NA_character_)]  # no manual entries at all - leave empty
}
n_bargnesi_filled <- sum(!is.na(bycatch_placeholder$bycatch_rate))
message("\n[Bycatch] ", n_bargnesi_filled, " of ", nrow(bycatch_placeholder),
        " Country x FleetType x FG cell(s) filled from Bargnesi et al. 2024 (sharks & rays x",
        " EU-3 trawl/longline).")

## 2026-09-23 addition: for every cell Bargnesi doesn't cover, fall
## back to this pipeline's OWN discard-rate-from-catch data, already
## computed above from real STECF FDI/SAU/FishMIP catch+discard
## tonnage - a discard % derived from actual reported catch for that
## FG, rather than leaving every non-shark/ray cell "not estimated".
## Priority (most gear/country-resolved first, same reasoning as the
## Bargnesi table being fleet-resolved rather than country-only):
##   1. stecf_discard_ratio_by_fleet - Country x FG x FleetType x Year,
##      real landed/discarded tonnage from STECF FDI, fleet-resolved
##   2. stecf_discard_ratio         - Country x FG x Year, STECF FDI,
##      not fleet-resolved (same ratio applied to every FleetType)
##   3. sau_discard_ratio_by_year_hindcast - Country x FG x Year, SAU's
##      own landings-vs-discards split (calibrated against STECF where
##      both overlap - see the calibration step above)
##   4. discard_by_fg              - FG x Year only (FishMIP), no
##      Country/FleetType resolution at all - broadest fallback,
##      applied to every Country x FleetType cell for that FG
## Each tier only fills cells still NA after the previous ones (and
## after Bargnesi) - never overwrites a more specific/reliable value.
if (nrow(stecf_discard_ratio_by_fleet) > 0) {
  fleet_fill <- stecf_discard_ratio_by_fleet[, .(bycatch_rate_fill = mean(discard_ratio, na.rm = TRUE)),
                                             by = .(Country, FG_num, FleetType)]
  bycatch_placeholder[fleet_fill, `:=`(
    bycatch_rate = fifelse(is.na(bycatch_rate), i.bycatch_rate_fill, bycatch_rate),
    source_citation = fifelse(is.na(source_citation) & !is.na(i.bycatch_rate_fill),
                              "STECF FDI (this run's own catch data) - discarded/(landed+discarded) tonnage, Country x FG x FleetType x Year, averaged over available years",
                              source_citation)
  ), on = c("Country", "FG_num", "FleetType")]
}
if (nrow(stecf_discard_ratio) > 0) {
  country_fg_fill <- stecf_discard_ratio[, .(bycatch_rate_fill = mean(discard_ratio, na.rm = TRUE)), by = .(Country, FG_num)]
  bycatch_placeholder[country_fg_fill, `:=`(
    bycatch_rate = fifelse(is.na(bycatch_rate), i.bycatch_rate_fill, bycatch_rate),
    source_citation = fifelse(is.na(source_citation) & !is.na(i.bycatch_rate_fill),
                              "STECF FDI (this run's own catch data) - discarded/(landed+discarded) tonnage, Country x FG x Year, averaged over available years (not fleet-resolved)",
                              source_citation)
  ), on = c("Country", "FG_num")]
}
if (nrow(sau_discard_ratio_by_year_hindcast) > 0) {
  sau_fill <- sau_discard_ratio_by_year_hindcast[, .(bycatch_rate_fill = mean(discard_ratio, na.rm = TRUE)), by = .(Country, FG_num)]
  bycatch_placeholder[sau_fill, `:=`(
    bycatch_rate = fifelse(is.na(bycatch_rate), i.bycatch_rate_fill, bycatch_rate),
    source_citation = fifelse(is.na(source_citation) & !is.na(i.bycatch_rate_fill),
                              "SAU (this run's own catch data) - discarded/(landed+discarded) tonnage, Country x FG x Year, calibrated against STECF FDI where both overlap",
                              source_citation)
  ), on = c("Country", "FG_num")]
}
if (nrow(discard_by_fg) > 0) {
  fg_fill <- discard_by_fg[, .(bycatch_rate_fill = mean(discard_ratio, na.rm = TRUE)), by = FG_num]
  bycatch_placeholder[fg_fill, `:=`(
    bycatch_rate = fifelse(is.na(bycatch_rate), i.bycatch_rate_fill, bycatch_rate),
    source_citation = fifelse(is.na(source_citation) & !is.na(i.bycatch_rate_fill),
                              "FishMIP (this run's own catch data) - reported-vs-total catch ratio, FG x Year only, no Country/FleetType resolution - broadest fallback",
                              source_citation)
  ), on = "FG_num"]
}

bycatch_placeholder[, data_status := fifelse(
  is.na(bycatch_rate), "not estimated - no source in this pipeline captures bycatch/discards for this Country x FleetType x FG cell",
  fifelse(grepl("^Bargnesi", source_citation), "literature-sourced (elasmobranch-specific rate) - see source_citation",
          "data-derived (this run's own catch/discard tonnage) - see source_citation")
)]
message("[Bycatch] After the catch-data fallback: ", sum(!is.na(bycatch_placeholder$bycatch_rate)), " of ",
        nrow(bycatch_placeholder), " Country x FleetType x FG cell(s) filled total (", n_bargnesi_filled,
        " literature-sourced from Bargnesi et al. 2024, ", sum(!is.na(bycatch_placeholder$bycatch_rate)) - n_bargnesi_filled,
        " data-derived from this run's own STECF FDI/SAU/FishMIP catch+discard tonnage); the rest still",
        " flagged 'not estimated'.")

## =================================================================
## # recreational fishing EFFORT - default vector + manual override
## Recreational CATCH has a proxy (recreational_rows above, from SAU's
## own sector split). Recreational EFFORT does not - checked GFCM, FDI,
## SAU and FishMIP, none carries a recreational-effort variable of any
## kind (days, boats, anglers) for these 6 countries.
##
## (2026-09) default every Country x Year cell to 1 (a
## neutral, unscaled index - "no adjustment" - rather than leaving it
## NA), and let a specific fleet/year be overridden by hand wherever a
## real or expert figure becomes available. RECREATIONAL_EFFORT_MANUAL
## is now the OVERRIDE table only (add a row per Country x Year you want
## to replace); every cell without a matching row keeps the default of
## 1, tracked via `effort_source` ("default (=1, no adjustment)" vs
## "manual override - see source_citation") so default and overridden
## cells are always distinguishable downstream - never silently
## indistinguishable from a real measurement.
## =================================================================
RECREATIONAL_EFFORT_DEFAULT <- 1  # neutral default applied to every Country x Year cell unless overridden below
RECREATIONAL_EFFORT_MANUAL <- data.table(
  Country = character(), Year = integer(),
  recreational_effort = numeric(), units = character(), source_citation = character()
)  # OVERRIDE table only - add rows here (Country/Year/recreational_effort/units/source_citation) for any fleet/year you want to replace the default of 1 with a real or expert figure
recreational_effort_placeholder <- CJ(Country = unique(FLEET_REGISTER[Sector == "Recreational"]$Country), Year = YEAR_ECOPATH)  # one row per country x Ecopath-year
recreational_effort_placeholder[, `:=`(recreational_effort = RECREATIONAL_EFFORT_DEFAULT, units = "index (default = 1, no adjustment)", source_citation = NA_character_, effort_source = "default (=1, no adjustment)")]  # start every cell at the default
if (nrow(RECREATIONAL_EFFORT_MANUAL) > 0) {
  recreational_effort_placeholder[RECREATIONAL_EFFORT_MANUAL, on = c("Country", "Year"), `:=`(
    recreational_effort = i.recreational_effort, units = i.units, source_citation = i.source_citation,
    effort_source = "manual override - see source_citation"
  )]  # overwrite the default wherever a manual override row exists for that Country x Year
}
fwrite(recreational_effort_placeholder, file.path(csv_out_dir, "recreational_effort_placeholder.csv"))  # write result to CSV
message("\n[Recreational effort] ", sum(recreational_effort_placeholder$effort_source == "manual override - see source_citation"), " of ",
        nrow(recreational_effort_placeholder), " Country x Year cell(s) manually overridden from RECREATIONAL_EFFORT_MANUAL;",
        " the rest default to ", RECREATIONAL_EFFORT_DEFAULT, " (no adjustment) - written to recreational_effort_placeholder.csv,",
        " with effort_source marking which is which.")

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
## Year). STECF FDI's own PER-YEAR gear/metier x GSA
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

## Three-tier fallback for every cell STECF doesn't cover: (1) SAU's
## own real year-by-year shares, hindcast/bias-corrected above -
## covers any Country x FG x Year SAU actually has catch for,
## including the pre-2014 years this hindcast exists for; (2) STECF
## FDI's own real fleet composition for that same Country x FG,
## AVERAGED across whatever years FDI covers it (used as a pre-2014
## prior wherever SAU has no data at all for that FG - see note
## below); (3) fleet_prop's flat, all-years, all-FG average, only for
## the rarest cells with no SAU data AND no FDI coverage of that FG in
## any year.
##
## Tier (2) exists because of a bug this pipeline's boundary-
## consistency check (STECF_FDI_START_YEAR - 1 vs STECF_FDI_START_YEAR,
## further below) surfaced concretely: when SAU is entirely absent
## from a run, EVERY Country x FG cell that isn't STECF-covered used
## to fall straight to fleet_prop's flat tier, whose "no SAU data at
## all" branch splits EQUALLY (1/.N) across a country's FleetTypes -
## identically for every single FG regardless of that FG's actual
## catch. Summed across many FGs to build a fleet's total pre-2014
## catch (see catch_by_fleet_total_pre2013 below), an identical-every-
## FG split necessarily nets out to an EXACTLY equal 1/.N share of the
## country's whole catch for every FleetType - not just similar, but
## bit-for-bit identical - which is exactly the "Effort_days_pre
## identical across every FleetType" pattern the diagnostic printed.
## Using STECF's own real, FG-specific fleet composition (even just
## its multi-year average) as the prior instead is far better
## grounded than a blind equal split, and it keeps pre-2014 shares
## roughly continuous with FDI's real 2014+ shares for the same FG,
## which directly narrows the boundary discontinuity.
fleet_prop_hindcast_part <- if (nrow(sau_fleet_prop_by_year_hindcast) > 0) {
  merge(non_stecf_rows, sau_fleet_prop_by_year_hindcast, by = c("Country", "FG_num", "Year"))  # attach SAU's hindcasted fleet split for these cells
} else {
  data.table(Country = character(), FG_num = integer(), Year = integer(), Sector = character(),
             FleetType = character(), GSA = character(), Comment = character(),
             prop_fleet = numeric(), fleet_split_source = character())
}
hindcast_covered <- unique(fleet_prop_hindcast_part[, .(Country, FG_num, Year)])  # cells the hindcast tier actually filled
flat_rows_needed <- non_stecf_rows[!hindcast_covered, on = c("Country", "FG_num", "Year")]  # cells still needing a fallback below the SAU hindcast

stecf_fg_avg_part <- if (nrow(flat_rows_needed) > 0 && nrow(stecf_fleet_prop_by_year) > 0) {
  stecf_fg_avg <- stecf_fleet_prop_by_year[, .(prop_fleet = mean(prop_fleet, na.rm = TRUE)),
                                           by = .(Country, FG_num, Sector, FleetType, GSA, Comment)]  # FDI's own real composition for this Country x FG, averaged across every year FDI covers it
  stecf_fg_avg[, prop_fleet := prop_fleet / sum(prop_fleet), by = .(Country, FG_num)]  # renormalize - averaging across years with different FleetType coverage can leave the sum slightly off 1
  stecf_fg_avg[, fleet_split_source := "STECF FDI's own real fleet composition for this Country x FG, averaged across all years FDI covers it (used as a pre-2014 prior - far better grounded than an equal split when SAU has no data at all for this FG)"]
  merge(flat_rows_needed, stecf_fg_avg, by = c("Country", "FG_num"), allow.cartesian = TRUE)  # attach FDI's own FG-specific average composition for these cells
} else {
  data.table(Country = character(), FG_num = integer(), Year = integer(), Sector = character(),
             FleetType = character(), GSA = character(), Comment = character(),
             prop_fleet = numeric(), fleet_split_source = character())
}
stecf_fg_avg_covered <- unique(stecf_fg_avg_part[, .(Country, FG_num)])  # Country x FG cells the FDI-average tier actually filled
flat_rows_needed_final <- flat_rows_needed[!stecf_fg_avg_covered, on = c("Country", "FG_num")]  # cells still needing the crude flat fallback (FDI never covers this FG in any year)
fleet_prop_flat_part <- if (nrow(flat_rows_needed_final) > 0) {
  merge(flat_rows_needed_final, fleet_prop[, .(Country, FG_num, Sector, FleetType, GSA, Comment, prop_fleet, fleet_split_source)],
        by = c("Country", "FG_num"), allow.cartesian = TRUE)  # attach fleet_prop's flat all-years average for these cells
} else {
  data.table(Country = character(), FG_num = integer(), Year = integer(), Sector = character(),
             FleetType = character(), GSA = character(), Comment = character(),
             prop_fleet = numeric(), fleet_split_source = character())
}

fleet_prop_final <- rbindlist(list(fleet_prop_stecf_part, fleet_prop_hindcast_part, stecf_fg_avg_part, fleet_prop_flat_part), use.names = TRUE, fill = TRUE)  # combine all four tiers into the final fleet split
n_stecf_cells    <- nrow(unique(fleet_prop_stecf_part[, .(Country, FG_num, Year)]))  # cells sourced from FDI
n_hindcast_cells <- nrow(hindcast_covered)  # cells sourced from the SAU hindcast
n_fdiavg_cells   <- nrow(unique(stecf_fg_avg_part[, .(Country, FG_num, Year)]))  # cells sourced from FDI's own FG-specific multi-year average
n_flat_cells     <- nrow(unique(fleet_prop_flat_part[, .(Country, FG_num, Year)]))  # cells sourced from the flat fallback
message("\n[Fleet split] fleet_prop_final: ", n_stecf_cells, " cell(s) use STECF FDI's own per-year",
        " gear/metier x GSA split (2014+, Spain/France/Italy); ", n_hindcast_cells, " cell(s) use SAU's",
        " own year-by-year hindcast (bias-corrected against FDI where a calibration factor exists); ",
        n_fdiavg_cells, " cell(s) use STECF FDI's own FG-specific multi-year average composition (pre-2014,",
        " SAU has no data for this FG but FDI covers it in some year); ",
        n_flat_cells, " cell(s) fall all the way back to fleet_prop's flat all-years average (SAU has no",
        " catch at all for that specific Country x FG x Year, and FDI never covers that FG either).",
        " Total: ", nrow(catch_country_fg_year), " cell(s).")

## --- Catch-preservation top-up for (Country, FG_num) pairs fleet_prop ---
## never anticipated. 2026-09-23, per Andrea: "try to minimize leaving
## landings out of fg or fleet countries". ROOT CAUSE: fleet_prop's own
## cross-join (built earlier, from unique(gfcm_country_fg$FG_num) as it
## stood AT THAT POINT) is every downstream tier's foundation, including
## fleet_prop_flat_part's own merge against it - every one of those
## merges is a default data.table merge() with no all.x, i.e. an INNER
## join. A (Country, FG_num) combination that GFCM's own per-country
## table never had a row for AT ALL simply never entered fleet_prop's
## universe, so it silently has NO row anywhere in fleet_prop_final - not
## an NA prop_fleet (which the very next line already dropped defensively
## and would have caught), but a combination missing outright. catch_
## with_unreported can still carry real catch for exactly such a
## combination: ICCAT's Bluefin tuna/Swordfish catch, Belhabib's Morocco/
## Algeria override, and STAR/RAM's catch-priority override can each
## attach a catch figure for a Country x FG pair GFCM's own per-country
## breakdown never covered - and the merge below (previously a plain
## inner join, same default) then dropped that catch ENTIRELY, with no
## message and no trace. Very likely why Bluefin tuna, Swordfish and
## several other FGs showed real matched catch in the crosswalk/
## Catches_Ecopath but zero across every fleet column in Ecopath_L.
## Fixed by topping up fleet_prop_final here with exactly the missing
## (Country, FG_num) pairs, using the SAME two-tier fallback fleet_prop's
## own cascade already uses (country_overall's Country x FleetType mix,
## then a flat equal share across that country's own named FleetTypes as
## the last resort) - so every catch cell gets a real fleet share instead
## of being silently dropped by the join below.
missing_country_fg <- unique(catch_country_fg_year[, .(Country, FG_num)])
missing_country_fg <- missing_country_fg[!unique(fleet_prop_final[, .(Country, FG_num)]), on = c("Country", "FG_num")]
if (nrow(missing_country_fg) > 0) {
  fleet_prop_topup <- merge(missing_country_fg, fleet_types_ref, by = "Country", allow.cartesian = TRUE)
  if (nrow(country_overall) > 0) {
    fleet_prop_topup <- merge(fleet_prop_topup, country_overall[, .(Country, FleetType, prop_fleet)],
                              by = c("Country", "FleetType"), all.x = TRUE)
  } else {
    fleet_prop_topup[, prop_fleet := NA_real_]
  }
  fleet_prop_topup[, fleet_split_source := fifelse(!is.na(prop_fleet),
                                                   "SAU country-level mix (catch-preservation top-up - GFCM's own per-country table never had this FG at all, but a later source added catch for it)",
                                                   NA_character_)]
  fleet_prop_topup[is.na(prop_fleet), prop_fleet := 1 / .N, by = .(Country, FG_num)]
  fleet_prop_topup[is.na(fleet_split_source), fleet_split_source :=
                     "no SAU data at all - equal share across this country's fleet types (catch-preservation top-up, last resort)"]
  fleet_prop_final <- rbindlist(list(fleet_prop_final, fleet_prop_topup), use.names = TRUE, fill = TRUE)
  message("\n[Fleet split] Catch-preservation top-up: ", nrow(missing_country_fg), " Country x FG combination(s)",
          " had NO fleet-share row anywhere in fleet_prop_final (GFCM's own per-country table never covered them -",
          " likely ICCAT/Belhabib/STAR-RAM catch added for a Country x FG cell GFCM itself never split by country) -",
          " backfilled via country_overall/flat-equal-share so their catch isn't silently dropped below. Affected: ",
          paste(unique(missing_country_fg$FG_num), collapse = ", "), ".")
}

fleet_split <- merge(catch_with_unreported, fleet_prop_final, by = c("Country", "FG_num", "Year"), all.x = TRUE, allow.cartesian = TRUE)  # LEFT join (2026-09-23 fix, was an inner join) - the top-up above should mean nothing is missing now, but all.x is kept as a defensive backstop
n_no_fleet_share <- sum(is.na(fleet_split$prop_fleet))
if (n_no_fleet_share > 0) {
  dropped_catch <- fleet_split[is.na(prop_fleet), sum(Catch_t, na.rm = TRUE)]
  message("\n[Fleet split] WARNING: ", n_no_fleet_share, " catch row(s) (", round(dropped_catch, 1), " t total) still have",
          " no fleet share at all even after the top-up above, and will be dropped from Ecopath_L/Ecopath_Di/",
          " fleet_split_out entirely - check fleet_types_ref for a Country with zero named FleetTypes. This total",
          " still reaches Catches_Ecopath/the 'Other GFCM countries' residual column, just not broken out by fleet.")
}
fleet_split <- fleet_split[!is.na(prop_fleet)]  # drop rows with no fleet share at all (should be ~0 rows now - see WARNING above if not)

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

## PRIMARY pre-2014 method (2026-09 update): Rousseau's Country x
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
  ## Effort-share weight (2026-09-23 fix): use FDI's own days-per-tonne
  ## ratio (Country x FleetType, stecf_effort_catch_ratio) to convert
  ## each fleet's catch into an IMPLIED effort (catch x days/tonne), and
  ## share THAT across fleets - not raw catch share. Raw catch share
  ## badly misallocates effort across gears with very different catch-
  ## per-day efficiency (purse seiners/trawls land far more per day than
  ## artisanal/longline/trap gear), which was confirmed as the dominant
  ## cause of the sharp 2013->2014 effort discontinuity in the
  ## boundary_check diagnostic and validation plots: efficient gears'
  ## catch share overstated their pre-2014 effort share relative to
  ## FDI's real 2014 effort (ratio << 1), while inefficient/high-
  ## activity gears' catch share understated theirs (ratio >> 1). Any
  ## FleetType FDI never priced a ratio for falls back to that
  ## Country's own mean ratio across its other FleetTypes; if a whole
  ## Country has no ratio at all, days_per_tonne collapses to 1 and this
  ## degenerates back to the old raw-catch-share behavior for just that
  ## Country, rather than failing.
  fleet_share <- merge(fleet_share, stecf_effort_catch_ratio[, .(Country, FleetType, days_per_tonne)],
                       by = c("Country", "FleetType"), all.x = TRUE)
  fleet_share[, days_per_tonne_country_avg := mean(days_per_tonne, na.rm = TRUE), by = Country]
  n_no_fleet_ratio <- sum(is.na(fleet_share$days_per_tonne))
  fleet_share[is.na(days_per_tonne), days_per_tonne := days_per_tonne_country_avg]
  n_no_country_ratio <- sum(is.na(fleet_share$days_per_tonne))
  fleet_share[is.na(days_per_tonne), days_per_tonne := 1]  # whole-country fallback: degenerates to raw catch share
  fleet_share[, implied_effort := Catch_t_total * days_per_tonne]
  fleet_share[, catch_share := implied_effort / sum(implied_effort), by = .(Country, Year)]  # effort-IMPLIED share (variable name kept as catch_share so nothing downstream needs to change)
  fleet_share[, `:=`(days_per_tonne = NULL, days_per_tonne_country_avg = NULL, implied_effort = NULL)]
  if (n_no_fleet_ratio > 0) {
    message("[Effort hindcast] fleet_share: ", n_no_fleet_ratio, " Country x FleetType x Year row(s) had no FDI",
            " days-per-tonne ratio for that FleetType - fell back to that Country's own mean ratio",
            " across its other FleetTypes", if (n_no_country_ratio > 0) paste0(" (", n_no_country_ratio, " row(s) had no ratio anywhere in that Country and fell all the way back to raw catch share)") else "", ".")
  }
  effort_hindcast_rousseau_by_fleet <- merge(fleet_share, rousseau_effort_cy, by = c("Country", "Year"))  # attach Rousseau's country x year total
  effort_hindcast_rousseau_by_fleet <- merge(effort_hindcast_rousseau_by_fleet, rousseau_calibration, by = "Country")  # attach the FDI-anchored calibration factor
  effort_hindcast_rousseau_by_fleet[, `:=`(
    Effort_days = NomEffort * calib_factor * catch_share,  # FDI-anchored Rousseau total, split across fleets by FDI-implied effort share (catch x days-per-tonne), not raw catch share
    effort_source = "Rousseau et al. 2024 NomEffort, calibrated to FDI's real level, split by FleetType's FDI-implied effort share (catch x days-per-tonne ratio, Country x FleetType, pre-2014) - see ROUSSEAU_EFFORT_PATH comment for validation caveat"
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
  ## days metric below (per spec, "effort should be KW days per boat") can be
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
  
  ## --- PRIMARY EFFORT metric, EFFORT = kW x days x
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
    effort_by_fleettype_eu3[, Sector := fifelse(FleetType == "Artisanal", "Artisanal", "Industrial")]  # derive Sector from FleetType, same rule as FLEET_REGISTER
    ## Gear class for the creep rate itself (Damalas et al. 2015 / Tsagarakis
    ## et al. 2022, see TECH_CREEP_PERIOD_RATES's own comment) - FDI's "DTS"
    ## fishing-technology code IS bottom/demersal trawlers & seiners, so it's
    ## the direct match for the literature's "bottom trawl" gear; everything
    ## else (PMP, HOK, and any other FDI code that reaches this table) gets
    ## the general "other" rate.
    effort_by_fleettype_eu3[, gear_class := fifelse(FleetType == "DTS", "bottom_trawl", "other")]
    effort_by_fleettype_eu3[, creep_mult := tech_creep_multiplier(Country, Year, Sector, gear_class)]  # compute the technology-creep multiplier per row (country x sector x gear x year)
    effort_by_fleettype_eu3[, Effort_kWdays_total_effective := Effort_kWdays_total * creep_mult]  # apply it to the primary effort metric
    if ("Effort_kWdays_per_vessel" %in% names(effort_by_fleettype_eu3)) {
      effort_by_fleettype_eu3[, Effort_kWdays_per_vessel_effective := Effort_kWdays_per_vessel * creep_mult]  # apply it to the per-vessel metric too
    }
    effort_by_fleettype_eu3[, `:=`(creep_mult = NULL, gear_class = NULL)]  # drop the now-unneeded helper columns
    ## Excludes Effort_kWdays_total == 0 rows (a fleet with zero reported
    ## kW-days that year) BEFORE computing pct - dividing by zero there
    ## produces Inf, or NaN when the effective figure is also 0 (0/0), and
    ## either one poisons a plain min()/max() over the whole vector into
    ## Inf/NaN even though every OTHER row has a perfectly good percentage
    ## (confirmed happening for real: "Cumulative effect ... NaN% to NaN%").
    creep_range <- effort_by_fleettype_eu3[!is.na(Effort_kWdays_total_effective) & Effort_kWdays_total != 0,
                                           .(pct = round(100 * (Effort_kWdays_total_effective / Effort_kWdays_total - 1), 1)), by = .(Country, Year)]  # % change from the raw figure, for reporting
    creep_by_sector <- effort_by_fleettype_eu3[!is.na(Effort_kWdays_total_effective) & Effort_kWdays_total != 0,
                                               .(pct = round(100 * (Effort_kWdays_total_effective / Effort_kWdays_total - 1), 1)), by = .(Sector, Year)][
                                                 , .(min_pct = min(pct, na.rm = TRUE), max_pct = max(pct, na.rm = TRUE)), by = Sector]  # cumulative range per sector, for the flow-diagram bullet
    creep_range_pct <- if (nrow(creep_range) > 0) sprintf("%s%% to %s%%", min(creep_range$pct, na.rm = TRUE), max(creep_range$pct, na.rm = TRUE)) else "n/a (no row had a non-zero Effort_kWdays_total to compare against)"
    message("[Effort hindcast] Technology-creep correction applied to the PRIMARY effort metric:",
            " Effort_kWdays_total_effective = Effort_kWdays_total (kW x days x nboats) x tech_creep_multiplier(Country, Year, Sector, gear_class)",
            " (same multiplier also applied to the secondary per-vessel figure, as Effort_kWdays_per_vessel_effective).",
            " Varies by country (EU-3 multiplier 1, Morocco/Algeria/Tunisia multiplier 0.7 - see",
            " TECH_CREEP_COUNTRY_MULTIPLIER), by sector (Industrial multiplier 1, Artisanal/Recreational multiplier 0.6 -",
            " see TECH_CREEP_SECTOR_MULTIPLIER), by gear (bottom-trawl/DTS 0.79%/year, every other gear 2.0%/year,",
            " both 1994-2013 - Damalas et al. 2015 & Tsagarakis et al. 2022), and by year (4.5%/year 2014-2023, uniform",
            " across gears - Palomares & Pauly 2019's C% = 13.8 x y^-0.511 evaluated once at y=9 - see",
            " TECH_CREEP_PERIOD_RATES for all of the above). Cumulative effect",
            " ranges from ", creep_range_pct, " across the",
            " covered Country x Year cells; by sector: ", paste(sprintf("%s %s%% to %s%%", creep_by_sector$Sector, creep_by_sector$min_pct, creep_by_sector$max_pct), collapse = "; "),
            ". This is an ASSUMPTION-DRIVEN adjustment for both the country and sector dimensions (no fishery-specific",
            " creep rate exists for either split), applied only where FDI's own kW-days figure exists (2014+); pre-2014",
            " hindcasted rows are left uncorrected since they carry no kW-days figure to begin with.")
  }
  
  fwrite(effort_by_fleettype_eu3, file.path(csv_out_dir, "effort_by_fleettype_eu3_hindcast.csv"))  # write result to CSV
  message("[Effort hindcast] effort_by_fleettype_eu3: ", nrow(effort_by_fleettype_eu3), " Country x FleetType x",
          " Year row(s) for Spain/France/Italy (FDI's own effort 2014+, SAU-hindcasted before that) - written",
          " to effort_by_fleettype_eu3_hindcast.csv / Effort_by_FleetType_EU3. FishMIP's Fishing_Effort_by_Fleet",
          " below remains the only effort source for Morocco/Algeria/Tunisia (no FDI basis to hindcast from).")
  
  ## --- STECF_FDI_START_YEAR boundary-consistency check (2026-09-22) -
  ## the whole point of the Rousseau/SAU-ratio hindcast + calibration
  ## machinery above is that Effort_days should NOT jump at the FDI
  ## boundary - the calibration factor is chosen exactly so the
  ## hindcast lands on FDI's own scale. This prints the actual
  ## last-hindcast-year vs first-FDI-year ratio per Country x FleetType
  ## so a real discontinuity (an unfired calibration, a units mismatch,
  ## a metier double-count in the FDI side) shows up in the console
  ## instead of only being visible later in validation plot 5.
  boundary_check <- dcast(
    effort_by_fleettype_eu3[Year %in% c(STECF_FDI_START_YEAR - 1, STECF_FDI_START_YEAR),
                            .(Country, FleetType, Year, Effort_days)],
    Country + FleetType ~ Year, value.var = "Effort_days"
  )
  boundary_cols <- as.character(c(STECF_FDI_START_YEAR - 1, STECF_FDI_START_YEAR))
  if (all(boundary_cols %in% names(boundary_check))) {
    setnames(boundary_check, boundary_cols, c("Effort_days_pre", "Effort_days_fdi"))
    boundary_check <- boundary_check[!is.na(Effort_days_pre) & !is.na(Effort_days_fdi) & Effort_days_pre > 0]
    boundary_check[, ratio := round(Effort_days_fdi / Effort_days_pre, 2)]
    setorder(boundary_check, -ratio)
    n_jump <- boundary_check[ratio > 1.5 | ratio < (1 / 1.5), .N]  # arbitrary but generous - a >50% jump either way is worth a look
    message("\n[Effort hindcast] Boundary check, ", STECF_FDI_START_YEAR - 1, " (hindcast) -> ", STECF_FDI_START_YEAR,
            " (FDI real), Effort_days by Country x FleetType (ratio should be close to 1 - that's exactly what the",
            " Rousseau/SAU-ratio calibration above is meant to achieve):")
    print(boundary_check)
    if (n_jump > 0) {
      message(n_jump, " Country x FleetType row(s) show a >50% jump either direction right at the boundary -",
              " calibration likely isn't actually covering these cells (check n_calibrated_countries in the",
              " [Hindcast] messages above - a cell using the uncalibrated SAU fallback, or Rousseau missing that",
              " Country/Year, would show exactly this pattern). Not auto-corrected here since guessing which side",
              " is wrong would be worse than flagging it.")
    } else {
      message("No Country x FleetType row shows more than a 50% jump at the boundary - the calibration is",
              " holding across the transition for this run.")
    }
  } else {
    message("\n[Effort hindcast] Boundary check skipped - Effort_days not available for both ",
            STECF_FDI_START_YEAR - 1, " and ", STECF_FDI_START_YEAR, " in this run.")
  }
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
                                      .(Catch_t_avg = mean(Catch_t, na.rm = TRUE), Discard_t_avg = mean(Discard_t, na.rm = TRUE)), by = .(Country, FG_num, FG_name, Sector, FleetType)]  # average catch AND discards by country x FG x fleet over the Ecopath snapshot years, Recreational included where an SAU-derived estimate exists
ecopath_fleet_long[, `:=`(Fleet = paste(Country, FleetType, sep = " - "),
                          Catch_t_km2_avg = Catch_t_avg / Total_Area_km2,
                          Discard_t_km2_avg = Discard_t_avg / Total_Area_km2,
                          Landings_t_km2_avg = (Catch_t_avg - Discard_t_avg) / Total_Area_km2)]  # build the Fleet label and convert catch/discards/landings (Catch_t = gross catch = Landings + Discards) to density

## --- Ecopath_L / Ecopath_Di: FG x Fleet, one column PER FLEET -------
## Final workbook sheets, per spec: "FG_number, FG_name, landings/
## discards, each column a fleet". Built the same way as
## catches_ecopath_by_fleet_wide below (dcast to one column per Fleet,
## then completed to every FG in full_fg_list with 0-fill) but kept as
## their own tables since Ecopath_L/Ecopath_Di are landings-only/
## discards-only, not gross catch.
landings_ecopath_by_fleet_wide <- dcast(ecopath_fleet_long, FG_num + FG_name ~ Fleet,
                                        value.var = "Landings_t_km2_avg", fill = 0)
landings_ecopath_by_fleet_wide <- merge(full_fg_list, landings_ecopath_by_fleet_wide, by = c("FG_num", "FG_name"), all.x = TRUE)
landings_fleet_cols <- setdiff(names(landings_ecopath_by_fleet_wide), c("FG_num", "FG_name"))
for (cc in landings_fleet_cols) landings_ecopath_by_fleet_wide[is.na(get(cc)), (cc) := 0]
setorder(landings_ecopath_by_fleet_wide, FG_num)

discards_ecopath_by_fleet_wide <- dcast(ecopath_fleet_long, FG_num + FG_name ~ Fleet,
                                        value.var = "Discard_t_km2_avg", fill = 0)
discards_ecopath_by_fleet_wide <- merge(full_fg_list, discards_ecopath_by_fleet_wide, by = c("FG_num", "FG_name"), all.x = TRUE)
discards_fleet_cols <- setdiff(names(discards_ecopath_by_fleet_wide), c("FG_num", "FG_name"))
for (cc in discards_fleet_cols) discards_ecopath_by_fleet_wide[is.na(get(cc)), (cc) := 0]
setorder(discards_ecopath_by_fleet_wide, FG_num)

## --- Residual "Other GFCM countries" fleet column - closes the ------
## country-scope gap Andrea flagged (2026-09-23), comparing
## species_group_fg_crosswalk.csv against this sheet: FLEET_REGISTER only
## defines named fleets for 6 countries (Morocco/Algeria/Tunisia/France/
## Spain/Italy), so ecopath_fleet_long/fleet_split_out structurally has
## NO row at all for catch attributed to any OTHER GFCM-reporting
## Mediterranean country (Greece, Libya, Malta, Cyprus, Turkey, Egypt,
## etc.) - that catch was silently vanishing from Ecopath_L/Ecopath_Di
## (whole FG rows showing as all-zero) even though the SAME FG shows
## real matched catch in the crosswalk and in the broader, all-country
## catches_discards_fg total that Catches_Ecopath is built from (48 of
## 63 all-zero FG rows in a real run had matched crosswalk catch - this
## is not a small edge case). Rather than build a full per-country fleet
## taxonomy for every other Mediterranean GFCM reporter (out of scope -
## FLEET_REGISTER has no entry for them), add one residual column per
## FG: whatever's left of the broader FG-level landings/discards total
## after subtracting the 6-country fleet-split total, floored at 0. This
## keeps Ecopath_L/Ecopath_Di's row totals consistent with
## Catches_Ecopath's own FG totals (the same invariant already
## documented for the 6-country fleet columns), while being explicit
## that this slice isn't broken out by fleet/gear.
OTHER_GFCM_COL <- "Other GFCM countries - Unclassified"
broad_landings_density <- catches_discards_fg[Year %in% YEAR_ECOPATH, .(Landings_t_km2_broad = mean(Landings_t, na.rm = TRUE) / Total_Area_km2), by = FG_num]
broad_discards_density <- catches_discards_fg[Year %in% YEAR_ECOPATH, .(Discard_t_km2_broad = mean(Discard_t, na.rm = TRUE) / Total_Area_km2), by = FG_num]

landings_ecopath_by_fleet_wide <- merge(landings_ecopath_by_fleet_wide, broad_landings_density, by = "FG_num", all.x = TRUE)
landings_ecopath_by_fleet_wide[is.na(Landings_t_km2_broad), Landings_t_km2_broad := 0]
landings_ecopath_by_fleet_wide[, (OTHER_GFCM_COL) := pmax(0, Landings_t_km2_broad - rowSums(.SD, na.rm = TRUE)), .SDcols = landings_fleet_cols]
landings_ecopath_by_fleet_wide[, Landings_t_km2_broad := NULL]
landings_fleet_cols <- c(landings_fleet_cols, OTHER_GFCM_COL)

discards_ecopath_by_fleet_wide <- merge(discards_ecopath_by_fleet_wide, broad_discards_density, by = "FG_num", all.x = TRUE)
discards_ecopath_by_fleet_wide[is.na(Discard_t_km2_broad), Discard_t_km2_broad := 0]
discards_ecopath_by_fleet_wide[, (OTHER_GFCM_COL) := pmax(0, Discard_t_km2_broad - rowSums(.SD, na.rm = TRUE)), .SDcols = discards_fleet_cols]
discards_ecopath_by_fleet_wide[, Discard_t_km2_broad := NULL]
discards_fleet_cols <- c(discards_fleet_cols, OTHER_GFCM_COL)

n_fg_using_residual <- sum(landings_ecopath_by_fleet_wide[[OTHER_GFCM_COL]] > 0)
message("\n[Ecopath by fleet] Ecopath_L/Ecopath_Di: ", nrow(landings_ecopath_by_fleet_wide), " FG(s) x ",
        length(landings_fleet_cols), " fleet(s) (t/km^2/year), averaged over ", paste(range(YEAR_ECOPATH), collapse = "-"),
        " - Landings_t = Catch_t - Discard_t per fleet, same fleet columns as Ecopath_B's catches_ecopath_by_fleet_wide.",
        " ", n_fg_using_residual, " FG(s) carry a nonzero '", OTHER_GFCM_COL, "' column - catch attributed to a",
        " GFCM-reporting country outside FLEET_REGISTER's 6 named countries, not broken out by fleet/gear.")

## Written directly to the workbook here (not via
## finalize_ecopath_ecosim_summary_sheets(), which used to build these
## two sheets from the FG-only Catches_Discards_FG_ts CSV - that had no
## fleet dimension at all, so it can't produce the per-fleet columns
## the final spec requires).
upsert_workbook_sheets(
  list(Ecopath_L = landings_ecopath_by_fleet_wide, Ecopath_Di = discards_ecopath_by_fleet_wide),
  ECOPATH_WORKBOOK_PATH
)

## --- Diagnostic: does Ecopath_L's fleet-split scope match the broader
## FG-level catch total everyone else is built from? -------------------
## 2026-09-23, added per Andrea: "i dont know if [it] doesnt update the
## sheets... or that the FG are not accounted in the landings sheet,
## because in the crosswalk i see a lot of fg with catches". Root cause:
## Ecopath_L/Ecopath_Di are built ONLY from fleet_split_out/ecopath_fleet_long
## - a SEPARATE, narrower GFCM-country-level fleet-split chain, restricted
## to FLEET_REGISTER's 6 named countries (Morocco/Algeria/Tunisia/France/
## Spain/Italy) and to Sector != "Recreational" rows that lack an SAU
## estimate - NOT derived from catches_discards_fg, the broader all-GFCM-
## reporting-country total that species_group_fg_crosswalk.csv,
## Catches_Ecopath and F_by_fg.csv are all built from. Confirmed against
## a real run: 48 of 63 FG rows that showed zero across every fleet
## column here had real matched catch in the crosswalk - almost all of
## it attributable to GFCM-reporting countries with no FLEET_REGISTER
## entry (Greece, Libya, Malta, Cyprus, Turkey, Egypt, etc.), not to a
## matching failure. FIXED just above via the "Other GFCM countries -
## Unclassified" residual column, which now absorbs that gap so
## Ecopath_L/Ecopath_Di's row totals match Catches_Ecopath's FG totals.
## The workbook trim itself was never the problem - Ecopath_L/Ecopath_Di
## are in every trim's target_order, so they always survive and get
## freshly rewritten each run; only the sheets deliberately excluded
## from the final 9/10-sheet contract (Catches_Ecopath, Catches_Ecosim,
## etc.) get dropped from the xlsx, by design, staying available as CSV
## under output/fisheries/.
## This diagnostic now reports the SPLIT (how much of each FG's total
## landings is broken out by named 6-country fleet vs. how much sits in
## the unclassified residual), for visibility - not a gap to chase.
fleet_split_fg_totals <- ecopath_fleet_long[, .(Landings_t_fleetsplit = sum(Catch_t_avg - Discard_t_avg, na.rm = TRUE)), by = FG_num]
broad_fg_totals <- catches_discards_fg[Year %in% YEAR_ECOPATH, .(Landings_t_broad = mean(Landings_t, na.rm = TRUE)), by = FG_num]
ecopath_l_coverage_check <- merge(full_fg_list, broad_fg_totals, by = "FG_num", all.x = TRUE)
ecopath_l_coverage_check <- merge(ecopath_l_coverage_check, fleet_split_fg_totals, by = "FG_num", all.x = TRUE)
ecopath_l_coverage_check[, `:=`(
  Landings_t_broad      = fifelse(is.na(Landings_t_broad), 0, Landings_t_broad),
  Landings_t_fleetsplit = fifelse(is.na(Landings_t_fleetsplit), 0, Landings_t_fleetsplit)
)]
ecopath_l_coverage_check[, named_fleet_share := fifelse(Landings_t_broad > 0, Landings_t_fleetsplit / Landings_t_broad, NA_real_)]  # share broken out by named 6-country fleet, rest is the residual column
ecopath_l_coverage_check[, mostly_other_countries := Landings_t_broad > 0 & (is.na(named_fleet_share) | named_fleet_share < 0.5)]
setorder(ecopath_l_coverage_check, -Landings_t_broad)
n_mostly_other <- sum(ecopath_l_coverage_check$mostly_other_countries, na.rm = TRUE)
message("\n[Ecopath_L fleet-split coverage] ", n_mostly_other, " of ", nrow(ecopath_l_coverage_check), " FG(s) get less than",
        " half their total landings (catches_discards_fg, ", paste(range(YEAR_ECOPATH), collapse = "-"), " average) from a",
        " named 6-country fleet - the rest sits in Ecopath_L/Di's 'Other GFCM countries - Unclassified' column (see",
        " ecopath_L_fg_coverage_check.csv). Totals still reconcile with Catches_Ecopath; only the fleet/gear breakdown",
        " is coarser for these FGs.")
write_native_sheet_csv(ecopath_l_coverage_check, "ecopath_L_fg_coverage_check", csv_out_dir)

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
          " (01_biomass.R / 01_survey_density_custom.R) first if you want F computed here.")
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
  fwrite(f_by_fg, file.path(csv_out_dir, "F_by_fg.csv"))
  
  ## Species-resolved view of the same F. GFCM catch (catches_discards_fg)
  ## is only ever available at FG resolution - there is no species-level
  ## catch anywhere in this pipeline to split further - so F itself is
  ## NOT independently estimated per species here; it's the SAME FG-level
  ## F value repeated for every species in that FG (the standard EwE
  ## simplifying assumption: catch is taken from an FG's species in
  ## proportion to their share of that FG's biomass, i.e. uniform fishing
  ## pressure within the FG). What genuinely differs per species is its
  ## own biomass density, its resulting share of the FG's total biomass
  ## (prop_sp_fg - the same "proportion of biomass within FG" 01_biomass.R
  ## computes for FG_spp_Ecopath), and the per-species catch density that
  ## falls out of applying the shared F to that species' own biomass.
  species_biomass_avg <- species_density[Year %in% YEAR_ECOPATH,
                                         .(Species_density_avg = mean(mean_density, na.rm = TRUE)), by = .(FG_num, FG_name, ScientificName)]  # average each species' own density over the Ecopath years
  f_by_species_fg <- merge(species_biomass_avg,
                           f_by_fg[, .(FG_num, FG_name, Biomass_density_avg, F)],
                           by = c("FG_num", "FG_name"), all.x = TRUE)  # attach the FG's biomass total and F to every one of its species
  f_by_species_fg[, `:=`(
    prop_sp_fg = ifelse(!is.na(Biomass_density_avg) & Biomass_density_avg > 0, Species_density_avg / Biomass_density_avg, NA_real_),  # this species' share of its FG's biomass
    Species_catch_density_avg = F * Species_density_avg  # this species' implied catch density under the uniform-F-within-FG assumption
  )]
  setnames(f_by_species_fg, "ScientificName", "Species")
  setcolorder(f_by_species_fg, c("FG_num", "FG_name", "Species", "Species_density_avg", "Biomass_density_avg",
                                 "prop_sp_fg", "F", "Species_catch_density_avg"))
  setorder(f_by_species_fg, FG_num, -prop_sp_fg)
  fwrite(f_by_species_fg, file.path(csv_out_dir, "F_by_species_fg.csv"))
  message("[F] F_by_species_fg.csv written: ", nrow(f_by_species_fg), " Species x FG row(s) (F is the FG-level",
          " value repeated per species - see comment above; prop_sp_fg is each species' own share of its FG's biomass).")
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
  ## This was flagged as a real gap (2026-09) - fixed by carrying
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
  ## Gear class for the creep rate itself (same Damalas et al. 2015 /
  ## Tsagarakis et al. 2022 split as effort_by_fleettype_eu3 above) -
  ## FishMIP's own `gear` field doesn't carry FDI's "DTS" code, so bottom-
  ## trawl-type gear is identified here by name-keyword instead (matches
  ## "trawl" but excludes "midwater"/"pelagic" trawls, which aren't the
  ## demersal gear either literature source is about) - a best-effort
  ## match against FishMIP's own gear-name taxonomy, not an exact code
  ## lookup like the FDI side has.
  effort_by_fleet[, gear_class := fifelse(
    grepl("trawl", gear, ignore.case = TRUE) & !grepl("midwater|pelagic", gear, ignore.case = TRUE),
    "bottom_trawl", "other"
  )]
  effort_by_fleet[, nom_active_kWdays_effective := nom_active_kWdays * tech_creep_multiplier(Country, Year, Sector, gear_class)]  # apply the technology-creep correction (country x sector x gear x year)
  fishmip_creep_range <- effort_by_fleet[!is.na(nom_active_kWdays_effective),
                                         .(pct = round(100 * (nom_active_kWdays_effective / nom_active_kWdays - 1), 1)), by = .(Country, Year)]  # % change from the raw figure, for reporting
  fishmip_creep_by_sector <- effort_by_fleet[!is.na(nom_active_kWdays_effective),
                                             .(pct = round(100 * (nom_active_kWdays_effective / nom_active_kWdays - 1), 1)), by = .(Sector, Year)][
                                               , .(min_pct = min(pct), max_pct = max(pct)), by = Sector]  # cumulative range per sector, for the flow-diagram bullet
  message("[Effort] Fishing_Effort_by_Fleet: ", uniqueN(effort_by_fleet$Fleet), " fleet(s) (country x top ",
          N_TOP_GEARS, " gears + 'Other gear'), ", nrow(effort_by_fleet), " Fleet x Year row(s). Max FishMIP",
          " year: ", max(effort$year), " (hard ceiling, not extrapolated). Technology-creep correction added",
          " as nom_active_kWdays_effective = nom_active_kWdays x tech_creep_multiplier(Country, Year, Sector, gear_class)",
          " (varies by country - EU-3 multiplier 1, Morocco/Algeria/Tunisia multiplier 0.7 - by sector -",
          " Industrial multiplier 1, Artisanal multiplier 0.6 - by gear - bottom-trawl 0.79%/year, other gears",
          " 2.0%/year, both 1994-2013 (Damalas et al. 2015 & Tsagarakis et al. 2022) - and by year, 4.5%/year",
          " 2014-2023 (Palomares & Pauly 2019), decelerating from TECH_CREEP_BASE_YEAR = ",
          TECH_CREEP_BASE_YEAR, "). Cumulative effect (Industrial) ranges from ", min(fishmip_creep_range$pct),
          "% to ", max(fishmip_creep_range$pct), "% across the covered years; by sector: ",
          paste(sprintf("%s %s%% to %s%%", fishmip_creep_by_sector$Sector, fishmip_creep_by_sector$min_pct, fishmip_creep_by_sector$max_pct), collapse = "; "),
          " - country/sector split is still an ASSUMPTION (no literature figure exists for that split); the",
          " gear x period rates themselves are now literature-backed (see TECH_CREEP_PERIOD_RATES).")
  
  ## Rousseau NomEffort added as a SECOND, independent effort figure per
  ## Country x Year (not replacing FishMIP - SAU itself has no effort
  ## variable for these "other" non-EU countries, so Rousseau is the only
  ## candidate second source here; to bring
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
## Algeria real trawl effort (2026-09, per the uploaded workbook -
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
## Algeria (2026-09, per the uploaded workbook). Written out as a
## cross-check series only - NOT wired into TECH_CREEP_COUNTRY_MULTIPLIER
## or nom_active_kWdays above - because its own "Effective effort,
## linear-creep-adjusted (kW-days)" column is wildly implausible as
## extracted (in some rows, 100-1000x its own "Nominal effort (kW-days)"
## for the SAME year, which is not achievable by any realistic annual
## creep rate compounded over a 1994-2010 window - this smells like a
## units/compounding error in how that column was originally computed,
## not a real technology-creep effect). the numbers should be sanity-
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
  rousseau_effort_review[, implied_creep_ratio := `Effective effort, linear-creep-adjusted (kW-days)` / `Nominal effort (kW-days)`]  # flagged as implausible, see comment above - kept for manual inspection
  fwrite(rousseau_effort_review, file.path(csv_out_dir, "rousseau_effort_review.csv"))
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
fwrite(gfcm_species_division_fg, file.path(csv_out_dir, paste0("gfcm_catches_by_species_year_division_", DATASET_VERSION, ".csv")))  # re-write, in case anything changed above
fwrite(gfcm_catches_by_area, file.path(csv_out_dir, paste0("gfcm_catches_by_country_division_year_", DATASET_VERSION, ".csv")))  # re-write, in case anything changed above
fwrite(fleet_vs_division_check, file.path(csv_out_dir, "fleet_definition_vs_gfcm_division_check.csv"))  # re-write, in case anything changed above
fwrite(fg_catch_timeseries, file.path(csv_out_dir, paste0("catches_by_FG_timeseries_", DATASET_VERSION, ".csv")))  # write the FG-level catch timeseries
fwrite(catches_discards_fg, file.path(csv_out_dir, paste0("catches_and_discards_by_FG_timeseries_", DATASET_VERSION, ".csv")))  # write the FG-level catch+discards timeseries
fwrite(fleet_split_out, file.path(csv_out_dir, "catches_by_country_fleet_sector_year.csv"))  # write the full fleet-split table

## --- Diagnostic: how much of the fleet split is a REAL per-gear ------
## breakdown vs. a fallback that just divides a country-level (or
## equal-share) total evenly across that country's named fleets?
## 2026-09-23, added per Andrea: "I cant believe all countries have
## catches on sardine anchovy etc... and other groups" - looking at a
## real Ecopath_L export, several FG rows show the EXACT SAME catch
## value repeated across every one of a country's fleet columns (e.g.
## all 5 Algeria columns identical for one FG). That's not a coincidence
## - it's fleet_prop_final's own fallback cascade: Algeria/Tunisia/
## Morocco have no STECF FDI coverage at all (EU-only: Spain/France/
## Italy), so whenever SAU also has no FG-specific gear breakdown for
## that Country x FG, the split falls back to the country's OVERALL SAU
## gear mix (same proportions for every FG in that country - explains
## same-country-different-FG rows looking similar), and if even THAT is
## missing, to a flat 1/N equal share across the country's fleet types
## (explains identical values within one FG row, exactly like Algeria's
## repeated 1.47952894198202E-05 above). The catch TOTAL for that
## Country x FG is still real; only the per-gear breakdown is fabricated
## in these rows. This table quantifies how much of fleet_split_out (by
## row count and by catch value) falls into each tier, per country, so
## it's visible rather than discovered by eye in the wide sheet.
fleet_split_method_summary <- fleet_split_out[, .(n_rows = .N, Catch_t_total = sum(Catch_t, na.rm = TRUE)),
                                              by = .(Country, fleet_split_source)]
fleet_split_method_summary[, pct_of_country_catch := round(100 * Catch_t_total / sum(Catch_t_total), 1), by = Country]
setorder(fleet_split_method_summary, Country, -Catch_t_total)
fwrite(fleet_split_method_summary, file.path(csv_out_dir, "fleet_split_method_by_country.csv"))
n_flat_country <- uniqueN(fleet_split_method_summary[grepl("no SAU data at all - equal share", fleet_split_source)]$Country)
message("\n[Fleet split method check] fleet_split_method_by_country.csv: breaks down fleet_split_out's catch",
        " total per Country x fleet_split_source, so a same-value-across-every-fleet row (uniform 1/N last-resort",
        " split, or a country-level mix reused across every FG) is visible instead of looking like a real per-gear",
        " catch report. ", n_flat_country, " of ", uniqueN(fleet_split_out$Country), " country/ies have at least",
        " one row using the deepest 'no SAU data at all - equal share' fallback.")
fwrite(bycatch_placeholder, file.path(csv_out_dir, "bycatch_placeholder.csv"))  # write the bycatch placeholder table
fwrite(gfcm_task2_placeholder, file.path(csv_out_dir, "gfcm_task2_catch_placeholder.csv"))  # write the GFCM Task 2 placeholder table
fwrite(gfcm_saf_effort_placeholder, file.path(csv_out_dir, "gfcm_saf_effort_placeholder.csv"))  # write the GFCM SAF placeholder table
if (nrow(stecf_fdi_catch_by_gsa) > 0) fwrite(stecf_fdi_catch_by_gsa, file.path(csv_out_dir, "stecf_fdi_catch_by_gsa_gear_year.csv"))  # re-write, if available
if (nrow(stecf_fdi_effort_by_gsa) > 0) fwrite(stecf_fdi_effort_by_gsa, file.path(csv_out_dir, "stecf_fdi_effort_by_gsa_gear_year.csv"))  # re-write, if available
fwrite(catches_ecopath_by_fleet_wide, file.path(csv_out_dir, "catches_ecopath_by_fleet_wide.csv"))  # write the Ecopath-by-fleet snapshot table
fwrite(fleet_structure_out, file.path(csv_out_dir, "fleet_structure.csv"))  # write the Fleet_Structure table
if (nrow(effort_by_fleet) > 0) fwrite(effort_by_fleet, file.path(csv_out_dir, "fishing_effort_by_fleet_timeseries_FishMIP.csv"))  # write the FishMIP effort table, if available
if (nrow(f_by_fg) > 0) fwrite(f_by_fg, file.path(csv_out_dir, "F_by_FG_ecopath_years.csv"))  # write the fishing-mortality table, if available
if (nrow(unreported_by_country) > 0) fwrite(unreported_by_country, file.path(csv_out_dir, "unreported_pct_by_country.csv"))  # write the unreported-ratio table, if available

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
if (nrow(star_catch_by_fg) > 0) sheets_to_write$STAR_RAM_Catch_CrossCheck <- catches_discards_fg[!is.na(star_catches_t), .(Year, FG_num, FG_name, Catch_t, star_catches_t, star_landings_t, star_catch_pct_diff, star_n_stocks, star_sources)]  # add this sheet only if the STAR/RAM cross-check found data

## Written here as its own sheet (2026-09-16) IN ADDITION
## to its existing CSV (catches_and_discards_by_FG_timeseries_*.csv,
## further up) - this is the clean Year/FG_num/FG_name/Landings_t/
## Catch_t/Discard_t table finalize_ecopath_ecosim_summary_sheets()
## (lib_survey_fg_density_functions.R, called at the end of
## 03_pbqb-traits.R) reads to build the Ecopath_L/Ecopath_Di summary
## sheets - kept as a plain FG x Year table (not the Ecopath/Ecosim-
## specific meta-row format) since it's meant to be read back
## programmatically, not opened as an Ecopath forcing function itself.
sheets_to_write$Catches_Discards_FG_ts <- catches_discards_fg[, .(Year, FG_num, FG_name, Landings_t, Catch_t, Discard_t, catch_source)]  # catch_source kept here too (not just in the DATASET_VERSION-suffixed CSV) so build_fg_references_sheet() can read it back under this fixed filename

## 2026-09-17 update: the excel ecopath_ecosim file must have exactly
## the intended sheets, trimmed script by script; other sheets should
## be saved as csv files, not kept in the final output excel file.
## Every table above (native/intermediate - none of these are among the
## 9 final target sheets) is written as CSV only, never to the workbook.
write_native_sheets_csv(sheets_to_write, csv_out_dir)  # write/replace all these tables as CSV, never in the workbook - lands in output/fisheries/

## Ecopath_L/Ecopath_Di were already written directly to the workbook
## above (per FG x Fleet, from fleet_split_out). What's left to build
## here is just Ecosim_ts, if Ecosim.csv/Catches_Ecosim.csv/
## Fishing_Effort_by_Fleet.csv already exist - then trim the workbook
## down to EXACTLY the final target sheets that exist at this point in
## the pipeline - never any native/intermediate sheet, since those are
## all CSV-only now. Safe to run here even if Biomass/PB_QB haven't run
## yet this session; whichever isn't ready yet is simply skipped with a
## message until it is. Add this same pair of calls to
## 03_pbqb-traits.R and 04_diets.R too.
finalize_ecopath_ecosim_summary_sheets(
  out_path           = ECOPATH_WORKBOOK_PATH,
  year_ecopath       = YEAR_ECOPATH,
  biomass_csv_dir    = BIOMASS_CSV_DIR,
  fisheries_csv_dir  = csv_out_dir
)
trim_workbook_to_final_sheets(ECOPATH_WORKBOOK_PATH)

message("\n=== Done (02_fisheries.R) === Wrote ", length(sheets_to_write), " sheet(s) directly, plus",
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
## Nested under out_dir/plots/validation/fisheries/ - same "plots"
## convention as 01_biomass.R/03_pbqb-traits.R's plot_dir (out_dir/
## plots), with a shared "validation" subfolder (for every script's
## validation figures) and a per-module "fisheries" subfolder under
## that.
validation_png_dir <- file.path(out_dir, "plots", "validation", "fisheries")
if (!dir.exists(validation_png_dir)) dir.create(validation_png_dir, recursive = TRUE, showWarnings = FALSE)  # ensure the PNG output dir exists
VALIDATION_PLOTS_PDF <- file.path(validation_png_dir, "fisheries_validation_plots.pdf")

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
  ## 2026-09-23 fix: a Country x FleetType x Year cell can legitimately
  ## carry MORE THAN ONE row here - one per distinct discard_split_source
  ## its underlying FGs used that year (e.g. some FGs get FDI's own
  ## per-fleet discard rate, others fall back to the uniform country-
  ## level ratio) - that's real and worth keeping visible, not a
  ## duplicate to collapse. But geom_line(group = FleetType) alone
  ## ignores that split and connects every one of a FleetType's points,
  ## across both years AND tiers, into a single path - exactly the same
  ## "connects unrelated points, draws a zigzag" bug already fixed for
  ## plot 5's sawtooth. Grouping by FleetType x Tier together instead
  ## gives each tier its own continuous sub-line per fleet, so a genuine
  ## color/tier switch reads as two clean, separately-colored segments
  ## instead of one multi-colored zigzag.
  ggplot(d, aes(x = Year, y = Discard_t, color = Tier)) +
    geom_point(size = 0.6, alpha = 0.7) + geom_line(aes(group = interaction(FleetType, Tier)), linewidth = 0.2, alpha = 0.4) +
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
    ## 2026-09-23 fix: effort_by_fleet has one row per Country x gear x
    ## Sector x Year (Sector = Industrial/Artisanal, see the "Fleet"
    ## build above) - selecting Fleet = gear alone (dropping Sector)
    ## left TWO rows sharing the same (Country, Fleet, Year) whenever a
    ## gear had both an Industrial and an Artisanal component. geom_line
    ## sorts by x (Year) within each color group but does NOT sort ties
    ## by y, so those same-year duplicate points got connected in
    ## whatever row order they happened to be in, drawing a sawtooth
    ## that zigzags between the two sectors' values every single year
    ## (exactly the "weird line pattern... multiple sources/gear
    ## assigned" the validation plots flagged for Morocco/Algeria/
    ## Tunisia). Summed across Sector here - this diagnostic plot's own
    ## legend is keyed by gear only, so a single combined line per gear
    ## is the correct fix for IT specifically; effort_by_fleet itself
    ## still keeps Sector as its own column everywhere else in the
    ## pipeline, nothing upstream of this plot is touched.
    effort_by_fleet[!Country %in% c("Spain", "France", "Italy"),
                    .(Effort = sum(nom_active_kWdays, na.rm = TRUE)),
                    by = .(Country, Fleet = gear, Year)][, Source := "FishMIP nom_active"]
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