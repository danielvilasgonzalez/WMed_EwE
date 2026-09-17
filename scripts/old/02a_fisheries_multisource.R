## =================================================================
## PIPELINE STEP 2a of 4 (renamed from "2b" - RUNS BEFORE Script 2, not
## alongside it: Script 2's own CATCHES_DATA_SOURCE/FLEET_STRUCTURE_
## PATH now default to reading THIS script's own output, so this needs
## to have already run for that default to actually find anything).
## REQUIRES Step 1 (01_survey_density_westmed.R or
## 01_survey_density_custom.R) to have already run - reads
## strata_area_by_area.csv (density conversion) and species_density_
## regional_combined.csv (FG reference), same contract files 02_fao_
## catches.R and 04_pbqb_calc.R already depend on.
##
## Produces: Catches_by_Fleet, Fishing_Effort_by_Fleet, and
## DataSources_Catch sheets in output/ecopath_ecosim_inputs.xlsx, plus
## a fg_catch_timeseries_<DATA_SOURCE>.csv and (SAU only)
## landings_by_species_gsa_year_<DATA_SOURCE>.csv shaped to feed
## straight into 04_pbqb_calc.R's existing FG_YIELD_SOURCE/YIELD_SOURCE
## mechanism (see "Wiring into 04_pbqb_calc.R" below) - NO changes to
## 04_pbqb_calc.R's own F/PB=M+F math are needed, this script only
## supplies a differently-sourced Yield input to it.
## =================================================================

## =================================================================
## WHY THIS SCRIPT EXISTS
##
## 02_fao_catches.R's only catch source is GFCM_Capture_Quantity.csv -
## Country x Species x Division x Year, LANDINGS ONLY (no discards),
## no fleet/gear dimension at all (see that script's own "NOT
## IMPLEMENTED" notes). This script adds three genuinely different,
## already-assembled catch/effort sources on top of that one, each
## with real gaps of its own - none of the four is a strict upgrade
## over the others, which is why all four get written into the
## workbook side by side (DataSources_Catch) rather than silently
## picked for you:
##
##   SAU (Sea Around Us)     - reconstructed catch (adds back
##                              unreported/discarded/IUU on top of
##                              official landings), resolved to
##                              country x GEAR x SPECIES x year. This
##                              is the ONLY source here with a real
##                              gear dimension, so Catches_by_Fleet
##                              below is built from SAU alone.
##   SAU (w/o unreported)    - the same SAU extract, filtered to rows
##                              SAU itself flags reporting_status ==
##                              "Reported". This does NOT cleanly
##                              isolate "landings + discards" as a
##                              formula - it's SAU's own reporting-
##                              status split, kept as its own source
##                              rather than mislabeled as a computed
##                              Yield.
##   FishMIP                 - a SAU-DERIVED calibration product
##                              (NOT independent of SAU - see the
##                              fishmip module's own header comment),
##                              resolved to country x SECTOR x
##                              FUNCTIONAL-GROUP x year, with reported/
##                              iuu/discards already split out. NO
##                              species and NO gear dimension on the
##                              catch side (gear only exists on
##                              FishMIP's EFFORT file, a different
##                              file entirely - see Fishing_Effort_by_
##                              Fleet below). FishMIP's own functional
##                              groups are NOT this pipeline's FG
##                              scheme - see FISHMIP_FG_CROSSWALK_PATH.
##   FAO-GFCM                - this pipeline's own existing source
##                              (02_fao_catches.R's output) - passed
##                              through here unchanged. Landings only,
##                              Division-resolution (not GSA, not
##                              gear/fleet) - see that script's own
##                              caveats, which apply identically here.
##
## AREA SCOPE CAVEAT (same pattern as the GSA-12/Division mismatch
## already flagged in 02_fao_catches.R): SAU and FishMIP resolve to
## COUNTRY/EEZ, not GSA. Restricting either one to "the same area as
## the survey" (e.g. GSA 1-11 for the Western Med example) means
## restricting to the COUNTRIES whose EEZs overlap that GSA range,
## which is an approximation, not an exact area match - a country
## with only a small sliver of coastline inside the modeled GSA range
## still contributes its ENTIRE national catch/effort. This is
## unavoidable at these sources' native resolution and is reported
## explicitly below (TARGET_COUNTRIES_NOTE), not silently absorbed.
## =================================================================

pkgs <- c("data.table", "dplyr", "tidyr", "openxlsx")
new_pkgs <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new_pkgs) > 0) install.packages(new_pkgs)
invisible(lapply(pkgs, library, character.only = TRUE))

## =================================================================
## STEP 1: Configuration
## Byte-identical out_dir/pcloud_dir/git_dir resolution to
## 02_fao_catches.R's own Step 1 (same resolve_config_dir() helper,
## same rstudioapi::showQuestion() message window shown right before
## each selectDirectory() picker, so you always see which folder
## you're being asked for before the OS file-browser pops up) -
## copied rather than sourced from it since it has no exported
## function for this, just top-level script code. Each of the three
## is resolved INDEPENDENTLY (not an all-or-nothing block): reused
## verbatim if already set - e.g. by 02_fao_catches.R's own STEP 7
## auto-run of THIS script, or by you hardcoding just one of them by
## hand above this block - and only the ones not already set fall
## through to the daniel-check/interactive-picker below.
## =================================================================
resolve_config_dir <- function(var_name, hardcoded_value, prompt_title, prompt_message) {
  if (exists(var_name, envir = .GlobalEnv, inherits = FALSE)) {
    val <- get(var_name, envir = .GlobalEnv)
    ## Same reused-value validation as 02_fao_catches.R's own copy of this
    ## function - see its comment for why this matters (a stale/corrupted
    ## binding like var_name <- NULL was previously being reused blindly,
    ## silently producing character(0) paths that only crashed much later,
    ## far from here, with a cryptic "missing value where TRUE/FALSE
    ## needed" error).
    if (!is.null(val) && is.character(val) && length(val) == 1 && !is.na(val) && val != "" && dir.exists(val)) {
      message("[02a_fisheries_multisource.R] ", var_name, " already set - using '", val,
              "' rather than re-prompting.")
      return(val)
    }
    message("[02a_fisheries_multisource.R] ", var_name, " was already set but to something invalid (",
            if (is.null(val)) "NULL" else if (length(val) == 0) "character(0)" else paste0("'", val, "'"),
            ", not a real directory) - re-resolving it from scratch instead of reusing it.")
  }
  if (tolower(Sys.info()[["user"]]) == "daniel" && .Platform$OS.type == "unix") {
    return(hardcoded_value)
  }
  if (!requireNamespace("rstudioapi", quietly = TRUE) || !rstudioapi::isAvailable()) {
    stop("This script requires RStudio, or ", var_name, " set manually before running this",
         " script - ", prompt_message)
  }
  rstudioapi::showQuestion(title = prompt_title, message = prompt_message)
  val <- rstudioapi::selectDirectory()
  if (is.null(val) || val == "" || !dir.exists(val)) {
    stop("No valid directory selected for ", var_name, ".")
  }
  val
}

out_dir <- resolve_config_dir(
  "out_dir", "/Users/daniel/Work/iMARES/WMed EwE Model/output/",
  "Select Output Directory",
  "Please select the directory where output files and intermediate results will be saved (same one Step 1/2 used)."
)
pcloud_dir <- resolve_config_dir(
  "pcloud_dir", "/Users/daniel/pCloud Drive/EwE Western Med 2026/",
  "Select pCloud EwE West Med Directory",
  "Please select the location of the pCloud Drive/EwE Western Med 2026 folder."
)
git_dir <- resolve_config_dir(
  "git_dir", "/Users/daniel/Documents/GitHub/WMed_EwE/",
  "Select Github WMed_EwE Directory",
  "Please select the directory where you cloned the WMed_EwE repository."
)

source(file.path(git_dir, "scripts/lib_survey_fg_density_functions.R"))
source(file.path(git_dir, "scripts/lib_worms_taxonomy_lookup.R"))

ECOPATH_WORKBOOK_PATH <- file.path(out_dir, "ecopath_ecosim_inputs.xlsx")

## Must match Step 1's own YEAR_ECOPATH/FILTER_AREAS-equivalent period -
## these sources aren't filtered to GSA at all (see area-scope caveat
## above), so the year range is the only scoping lever actually applied.
START_YEAR <- 1994
END_YEAR   <- 2019   # SAU's public reconstruction currently stops at 2019 - see sau_west_med_analysis.Rmd

## Countries approximating the modeled area (see AREA SCOPE CAVEAT
## above) - defaults to the Western Med 6-country set already used
## throughout the attached SAU/FAO/FishMIP analysis code. Edit this for
## a different custom region; there is no way to narrow it below
## country level for these three sources.
TARGET_ISO3 <- c(Spain = "ESP", France = "FRA", Italy = "ITA",
                 Tunisia = "TUN", Algeria = "DZA", Morocco = "MAR")
TARGET_COUNTRIES_NOTE <- paste0(
  "SAU/FishMIP catch and effort below are scoped to these ", length(TARGET_ISO3),
  " countries' FULL national EEZs (", paste(names(TARGET_ISO3), collapse = ", "),
  "), not to the survey's GSA/custom boundary specifically - see this script's header comment."
)
message(TARGET_COUNTRIES_NOTE)

N_TOP_GEARS <- 8   # gears/effort-gears beyond the top N are folded into "Other gear" - same convention as
# sau_gear_species_region_analysis.R/westmed_fisheries_analysis.R, avoids a combinatorial
# explosion of near-empty FG x Fleet x Year rows

## Must match Step 1's own YEAR_ECOPATH exactly - this is the single-
## snapshot period Catches_by_Fleet (below) is averaged over, same
## convention add_catches_to_ecopath_workbook() already uses for its
## own Catches_Ecopath sheet (one row per FG, values averaged across
## year_ecopath) - kept as a separate constant here rather than read
## back from the workbook because Catches_Ecopath's own column naming
## doesn't expose year_ecopath in a form this script can parse
## unambiguously.
YEAR_ECOPATH <- 1994:1996

## =================================================================
## STEP 2: Data source paths
## =================================================================
SAU_RAW_CSV <- file.path(pcloud_dir, "data/fisheries/sau_raw_combined_west_med.csv")
FISHMIP_EFFORT_PARQUET <- file.path(pcloud_dir, "data/fisheries/effort_histsoc_1841_2017_western-mediterranean-sea.parquet")
FISHMIP_CATCH_PARQUET  <- file.path(pcloud_dir, "data/fisheries/calibration_catch_histsoc_1850_2017_western-mediterranean-sea.parquet")

## FAO-GFCM here means "whatever 02_fao_catches.R already wrote" - this
## script does NOT re-read GFCM_Capture_Quantity.csv itself, it passes
## that script's own output through unchanged (see the FAO_GFCM branch
## below). Must match 02_fao_catches.R's own DATASET_VERSION.
FAO_GFCM_DATASET_VERSION <- "GFCM_2025"
FAO_GFCM_FG_CSV <- file.path(out_dir, paste0("fg_catch_timeseries_", FAO_GFCM_DATASET_VERSION, ".csv"))

## OPTIONAL: FishMIP's own functional-group scheme is NOT this
## pipeline's FG scheme (dataframe2/FG_WMed.xlsx) - there is no
## automatic crosswalk between them (they were built by different
## groups for different purposes and don't share a common code). If
## you have a hand-built crosswalk (columns: fishmip_f_group, FG_num),
## point this at it to let FishMIP feed the FG-level sheets/DATA_SOURCE
## selection below; leave NULL to keep FishMIP informational-only
## (still written to DataSources_Catch at country level, just never at
## FG level and never selectable as DATA_SOURCE).
FISHMIP_FG_CROSSWALK_PATH <- NULL

## =================================================================
## STEP 3: Which source drives the workbook's Yield/Fmort/PB=M+F chain
## downstream in 04_pbqb_calc.R. This does NOT change anything in THIS
## script's own output - every available source is still written to
## DataSources_Catch/Catches_by_Fleet/Fishing_Effort_by_Fleet
## regardless. It only controls which fg_catch_timeseries_<DATA_SOURCE>.csv
## (and, for SAU, landings_by_species_gsa_year_<DATA_SOURCE>.csv) this
## script writes as "the" contract file - see "Wiring into
## 04_pbqb_calc.R" at the bottom of this script for the two lines to
## change there to point at it.
## =================================================================
DATA_SOURCE <- "SAU"   # "SAU" | "SAU_no_unreported" | "FishMIP" | "FAO_GFCM"

## =================================================================
## STEP 3b: Auto-run a source's own upstream step when its expected
## input file is missing, instead of just skipping it and telling you
## to go run something else first by hand. Applies to SAU (STEP 5,
## below - downloads sau_raw_combined_west_med.csv straight from
## SAU's API via lib_sau_extraction.R, the same per-EEZ logic as
## sau_west_med_analysis.Rmd's own "Part 0" chunk) and FAO-GFCM (STEP
## 7 - auto-sources 02_fao_catches.R itself to regenerate
## fg_catch_timeseries_<FAO_GFCM_DATASET_VERSION>.csv).
##
## FishMIP is NOT covered by this and never will be - its two input
## files are a manual download from the FishMIP Input Explorer
## website (fishmip.global-ecosystem-model.cloud.edu.au), not
## something any R script in this pipeline generates. There is
## nothing to auto-run for it; see STEP 6's own message below.
##
## Set FALSE to go back to the old behaviour (message + skip, no
## auto-run) - e.g. if you're offline and don't want SAU's download
## attempt to spend a minute timing out, or you deliberately want
## last run's FAO-GFCM file left alone rather than regenerated.
## =================================================================
if (!exists("AUTO_RUN_MISSING_SOURCES", inherits = FALSE) || !is.logical(AUTO_RUN_MISSING_SOURCES) ||
    is.na(AUTO_RUN_MISSING_SOURCES)) {
  if (exists("AUTO_RUN_MISSING_SOURCES", inherits = FALSE)) {
    message("[02a_fisheries_multisource.R] AUTO_RUN_MISSING_SOURCES was set to something other than",
            " TRUE/FALSE ('", AUTO_RUN_MISSING_SOURCES, "') - resetting it to the default (TRUE).",
            " Set it to TRUE or FALSE explicitly above this line if you want a different value.")
  }
  AUTO_RUN_MISSING_SOURCES <- TRUE
}

## =================================================================
## STEP 4: FG reference + species->FG matching cascade
## Reuses the SAME dataframe2/fg_lookup_safe convention and the exact
## match_species_to_fg() -> match_nominate_subspecies_fg() ->
## fallback_match_fg_by_taxonomy() cascade Step 1/02_fao_catches.R use,
## so a species matched (or left unresolved) here is matched the same
## way it would be anywhere else in this pipeline - not a second,
## divergent matching implementation.
## =================================================================
fg_file <- file.path(pcloud_dir, "data/FG_WMed.xlsx")
fg_raw <- as.data.table(readxl::read_excel(fg_file, sheet = 4))
dataframe2 <- unique(fg_raw[, .(ScientificName = ESPECIE, FG_num = GF, FG_name)])
fg_lookup_safe <- prepare_fg_lookup(dataframe2)

match_scientific_names_to_fg <- function(scientific_names, label) {
  dt <- data.table(ScientificName = unique(scientific_names))
  dt <- match_species_to_fg(dt, fg_lookup_safe)
  dt <- match_nominate_subspecies_fg(dt, fg_lookup_safe)
  dt <- fallback_match_fg_by_taxonomy(dt, fg_lookup_safe, taxonomy_source = "worms")
  n_matched <- dt[!is.na(FG_num), .N]
  message("[", label, "] ", n_matched, " of ", nrow(dt), " distinct species matched to an FG",
          " (unmatched species are dropped from FG-level sheets below, NOT guessed - still",
          " present in the raw source rows for manual review if needed).")
  dt[, .(ScientificName, FG_num, FG_name)]
}

## =================================================================
## STEP 5: SAU (Sea Around Us)
## =================================================================
if (!file.exists(SAU_RAW_CSV) && isTRUE(AUTO_RUN_MISSING_SOURCES)) {
  message("[SAU] Raw extract not found at '", SAU_RAW_CSV, "' - attempting to download it now,",
          " straight from SAU's own API (api.seaaroundus.org), per-EEZ (this can take a minute;",
          " set AUTO_RUN_MISSING_SOURCES <- FALSE above to skip this and just fall back to",
          " 'not found' instead) ...")
  sau_auto_ok <- tryCatch({
    source(file.path(git_dir, "scripts/lib_sau_extraction.R"))
    sau_download_raw_extract(SAU_RAW_CSV, year_min = START_YEAR, year_max = END_YEAR)
  }, error = function(e) {
    message("[SAU] Auto-download failed: ", conditionMessage(e), " - falling back to 'not found'.")
    FALSE
  })
  if (isTRUE(sau_auto_ok) && !file.exists(SAU_RAW_CSV)) {
    message("[SAU] Auto-download reported success but '", SAU_RAW_CSV, "' still doesn't exist -",
            " check sau_download_raw_extract()'s own messages above for what actually happened.")
  }
}

sau_available <- file.exists(SAU_RAW_CSV)
sau_species_fg <- data.table(); sau_catches_by_fleet <- data.table()
sau_catches_by_fleet_wide <- data.table()
sau_total_cy <- data.table(); sau_reported_cy <- data.table()
sau_fg_total <- data.table(); sau_fg_reported <- data.table()
sau_species_yield <- data.table()

if (!sau_available) {
  message("[SAU] Raw extract not found at '", SAU_RAW_CSV, "'",
          if (isTRUE(AUTO_RUN_MISSING_SOURCES)) {
            " (auto-download was attempted above and did not produce it - see its own messages for why;"
          } else {
            " - set AUTO_RUN_MISSING_SOURCES <- TRUE above to have this script download it automatically, or"
          },
          " knit sau_west_med_analysis.Rmd (Part 0) or run sau_west_med_full.R yourself if you want SAU",
          " included. Skipping SAU for this run.")
} else {
  sau_raw <- fread(SAU_RAW_CSV)
  
  ## defensive column resolution - SAU's dimension-aggregation export and
  ## its raw CSV export use slightly different names (same candidates
  ## list as sau_west_med_analysis.Rmd/westmed_fisheries_analysis_source_sau.R)
  sau_candidates <- list(
    gear = c("gear_type", "gear"), sci_name = c("scientific_name"), com_name = c("common_name"),
    tonnes = c("tonnes", "catch_sum", "value"), report = c("reporting_status")
  )
  sau_resolved <- list()
  for (field in names(sau_candidates)) {
    for (opt in sau_candidates[[field]]) if (opt %in% names(sau_raw)) { sau_resolved[[field]] <- opt; break }
  }
  missing_sau_cols <- setdiff(c("gear", "sci_name", "tonnes"), names(sau_resolved))
  if (length(missing_sau_cols) > 0) {
    stop("[SAU] Raw extract is missing required column(s): ", paste(missing_sau_cols, collapse = ", "))
  }
  setnames(sau_raw, unlist(sau_resolved), names(sau_resolved))
  if (!"report" %in% names(sau_resolved)) sau_raw[, report := NA_character_]
  
  sau_raw <- sau_raw[!is.na(year) & year >= START_YEAR & year <= END_YEAR & country %in% names(TARGET_ISO3)]
  message("[SAU] ", nrow(sau_raw), " rows after year/country filter (", START_YEAR, "-", END_YEAR, ", ",
          paste(names(TARGET_ISO3), collapse = ", "), ").")
  
  ## --- species -> FG match ------------------------------------------------
  sau_species_fg <- match_scientific_names_to_fg(sau_raw$sci_name, "SAU")
  sau_raw <- merge(sau_raw, sau_species_fg, by.x = "sci_name", by.y = "ScientificName", all.x = TRUE)
  
  ## --- Yield definitions ---------------------------------------------------
  ## "SAU" (total): SAU's own reconstruction already adds back
  ## unreported + discarded + IUU catch on top of official landings -
  ## its `tonnes` figure IS the Yield (landings + discards, +
  ## unreported/IUU) as SAU defines the term, not a component that
  ## still needs a discard figure added on top.
  sau_total_cy <- sau_raw[, .(tonnes = sum(tonnes, na.rm = TRUE)), by = .(country, year)]
  sau_total_cy[, source := "SAU total (reconstructed)"]
  
  ## "SAU_no_unreported": SAU's own reporting_status dimension, kept as
  ## its own labeled source rather than presented as a clean landings-
  ## only or landings+discards figure - see this script's header note.
  if (!is.null(sau_resolved$report)) {
    sau_reported_cy <- sau_raw[grepl("^report", report, ignore.case = TRUE) & !grepl("unreport", report, ignore.case = TRUE),
                               .(tonnes = sum(tonnes, na.rm = TRUE)), by = .(country, year)]
    sau_reported_cy[, source := "SAU w/o unreported"]
  } else {
    message("[SAU] No reporting-status column in this extract - 'SAU w/o unreported' source unavailable this run.")
  }
  
  ## --- FG-level yield (for DataSources_Catch / DATA_SOURCE = 'SAU'*) ------
  sau_fg_total <- sau_raw[!is.na(FG_num), .(Catch_t = sum(tonnes, na.rm = TRUE)), by = .(Year = year, FG_num, FG_name)]
  if (nrow(sau_reported_cy) > 0) {
    sau_fg_reported <- sau_raw[!is.na(FG_num) & grepl("^report", report, ignore.case = TRUE) & !grepl("unreport", report, ignore.case = TRUE),
                               .(Catch_t = sum(tonnes, na.rm = TRUE)), by = .(Year = year, FG_num, FG_name)]
  }
  
  ## --- species-level yield (for landings_by_species_gsa_year_SAU*.csv) ----
  ## AreaID left NA - SAU has no GSA/AreaID concept, only country; this
  ## mirrors 04_pbqb_calc.R's own landings_by_species_gsa_year.csv
  ## PLACEHOLDER schema (ScientificName x Year x AreaID x catch_t) so
  ## attach_yield_from_landings() can read it as-is, but AreaID is not
  ## meaningful here and should not be used for any area-based logic.
  sau_species_yield <- sau_raw[!is.na(FG_num), .(catch_t = sum(tonnes, na.rm = TRUE)),
                               by = .(ScientificName = sci_name, Year = year)]
  sau_species_yield[, AreaID := NA_integer_]
  
  ## --- Catches_by_Fleet (SAU is the only source with real gear x
  ## country x species resolution - see this script's header note) -----
  ## Yield here already includes discards (+ unreported + IUU, by SAU's
  ## own reconstruction) - see this script's header note on Yield
  ## definitions per source; there is no separate landings-only vs
  ## discards-only split available at fleet resolution, only SAU's
  ## already-combined total.
  top_gears <- sau_raw[, .(g_tot = sum(tonnes, na.rm = TRUE)), by = gear][order(-g_tot)][seq_len(min(N_TOP_GEARS, .N)), gear]
  sau_raw[, gear_grp := ifelse(gear %in% top_gears, gear, "Other gear")]
  sau_raw[, Fleet := paste0(country, " - ", gear_grp)]
  sau_catches_by_fleet <- sau_raw[!is.na(FG_num), .(Catch_t = sum(tonnes, na.rm = TRUE)),
                                  by = .(FG_num, FG_name, Species = sci_name, Fleet, country, gear = gear_grp, Year = year)]
  setorder(sau_catches_by_fleet, FG_num, -Catch_t)
  message("[SAU] Catches_by_Fleet_AllYears: ", uniqueN(sau_catches_by_fleet$Fleet), " fleet(s) (country x gear, top ",
          N_TOP_GEARS, " gears + 'Other gear'), ", uniqueN(sau_catches_by_fleet$FG_num), " FG(s), ",
          uniqueN(sau_catches_by_fleet$Year), " year(s).")
  
  ## --- Catches_by_Fleet (WIDE, Ecopath-style): one row per FG, one
  ## COLUMN PER FLEET, value = that FG x Fleet's catch (t) averaged over
  ## YEAR_ECOPATH - the same single-snapshot convention
  ## add_catches_to_ecopath_workbook() already uses for its own
  ## Catches_Ecopath sheet, but built directly from SAU's real per-
  ## fleet annual values (not a static proportion split of an FG
  ## total), so each fleet's actual year-to-year share is reflected,
  ## not assumed constant across years. Catches_by_Fleet_AllYears above
  ## keeps the full per-year detail this collapses out of, for anyone
  ## who needs the annual breakdown rather than the YEAR_ECOPATH average.
  fg_fleet_year <- sau_raw[!is.na(FG_num), .(Catch_t = sum(tonnes, na.rm = TRUE)),
                           by = .(FG_num, FG_name, Fleet, Year = year)]
  sau_catches_by_fleet_wide <- data.table()
  if (nrow(fg_fleet_year) > 0) {
    fg_ref <- unique(fg_fleet_year[, .(FG_num, FG_name)])
    fleet_avg <- fg_fleet_year[Year %in% YEAR_ECOPATH,
                               .(Catch_t = mean(Catch_t, na.rm = TRUE)), by = .(FG_num, Fleet)]
    if (nrow(fleet_avg) == 0) {
      message("[SAU] Catches_by_Fleet (wide): no SAU rows fall inside YEAR_ECOPATH (",
              min(YEAR_ECOPATH), "-", max(YEAR_ECOPATH), ") - check YEAR_ECOPATH above matches",
              " Step 1's own value and that SAU's year range actually covers it. Wide sheet",
              " NOT written this run; Catches_by_Fleet_AllYears above still has every year SAU has.")
    } else {
      sau_catches_by_fleet_wide <- dcast(fleet_avg, FG_num ~ Fleet, value.var = "Catch_t")
      sau_catches_by_fleet_wide <- merge(fg_ref, sau_catches_by_fleet_wide, by = "FG_num", all.x = TRUE)
      setorder(sau_catches_by_fleet_wide, FG_num)
      fleet_cols <- setdiff(names(sau_catches_by_fleet_wide), c("FG_num", "FG_name"))
      n_fg_no_catch <- sau_catches_by_fleet_wide[, sum(apply(.SD, 1, function(r) all(is.na(r)))), .SDcols = fleet_cols]
      message("[SAU] Catches_by_Fleet (wide): ", nrow(sau_catches_by_fleet_wide), " FG row(s) x ",
              length(fleet_cols), " fleet column(s), averaged over ", min(YEAR_ECOPATH), "-",
              max(YEAR_ECOPATH), ". ", n_fg_no_catch, " FG(s) have NO SAU catch from ANY fleet in",
              " that period (blank row, not zero) - genuinely unfished and simply unmatched/",
              " unresolved look identical here, same caveat as Catches_Ecopath's own equivalent.")
      
      ## --- fleet_structure_from_sau.csv: an AUTO-DERIVED fleet_structure
      ## input for 02_fao_catches.R's own FLEET_STRUCTURE_PATH mechanism -
      ## (FG_num, Fleet, prop_catch), each FG's fleets summing to 1 - so
      ## FAO-GFCM's catches (Country x Species x Division x Year, NO gear
      ## dimension at all - see 02_fao_catches.R's own caveats) can be
      ## split by country x gear WITHOUT hand-typing a fleet_structure
      ## CSV, by borrowing SAU's real gear-share proportions per FG.
      ##
      ## IMPORTANT CAVEAT, stated plainly rather than silently absorbed:
      ## this applies SAU's gear proportions to FAO-GFCM's totals - two
      ## DIFFERENT catch reconstructions for the same species/FG, from
      ## two different organizations, with no guarantee their relative
      ## gear mix actually matches. It is a genuine approximation, not a
      ## real fleet-resolved FAO-GFCM figure - FAO-GFCM has no gear data
      ## of its own to resolve directly. If you want the fleet split to
      ## come from the SAME reconstruction as the catch totals, use SAU's
      ## own Catches_by_Fleet (wide, above) instead of feeding this into
      ## 02_fao_catches.R's FAO-GFCM catches.
      fleet_structure_from_sau <- fleet_avg[, .(FG_num, Fleet, Catch_t)]
      fleet_structure_from_sau[, prop_catch := Catch_t / sum(Catch_t), by = FG_num]
      fleet_structure_from_sau <- fleet_structure_from_sau[, .(FG_num, Fleet, prop_catch)]
      setorder(fleet_structure_from_sau, FG_num, -prop_catch)
      fleet_structure_path <- file.path(out_dir, "fleet_structure_from_sau.csv")
      fwrite(fleet_structure_from_sau, fleet_structure_path)
      message("\nWrote ", fleet_structure_path, " (", nrow(fleet_structure_from_sau), " FG x Fleet",
              " proportion row(s), from SAU's own country x gear shares averaged over ",
              min(YEAR_ECOPATH), "-", max(YEAR_ECOPATH), ") - point 02_fao_catches.R's",
              " FLEET_STRUCTURE_PATH at this file to split FAO-GFCM's Catches_Ecopath/",
              "Catches_Ecosim by country x gear (e.g. 'Spain - bottom trawl', 'France -",
              " purse seine', ...) using these SAU-derived shares. See the caveat above",
              " this block before doing so - it borrows SAU's gear mix, not a real FAO-GFCM one.")
    }
  }
}

## =================================================================
## STEP 6: FishMIP
## =================================================================
fishmip_available <- requireNamespace("arrow", quietly = TRUE) &&
  file.exists(FISHMIP_EFFORT_PARQUET) && file.exists(FISHMIP_CATCH_PARQUET)
fishmip_effort_by_fleet <- data.table()
fishmip_fg_total <- data.table(); fishmip_fg_reported <- data.table()
fishmip_country_total <- data.table(); fishmip_country_reported <- data.table()

FISHMIP_SAUP_TO_COUNTRY <- c(`12` = "Algeria", `250` = "France", `380` = "Italy",
                             `504` = "Morocco", `724` = "Spain", `788` = "Tunisia")

if (!fishmip_available) {
  if (!requireNamespace("arrow", quietly = TRUE)) {
    message("[FishMIP] Package 'arrow' isn't installed - install.packages('arrow') to include FishMIP.")
  } else {
    message("[FishMIP] Parquet file(s) not found - unlike SAU/FAO-GFCM above, THIS source cannot be",
            " auto-run: its two files are a manual download from the FishMIP Input Explorer",
            " (fishmip.global-ecosystem-model.cloud.edu.au), not something any R script here",
            " generates. Download them yourself and update FISHMIP_EFFORT_PARQUET/",
            " FISHMIP_CATCH_PARQUET above. Skipping FishMIP for this run.")
  }
} else {
  effort <- as.data.table(arrow::read_parquet(FISHMIP_EFFORT_PARQUET))
  effort[, year := as.integer(year)]
  effort[, country := FISHMIP_SAUP_TO_COUNTRY[as.character(saup)]]
  effort <- effort[!is.na(country) & year >= START_YEAR & year <= END_YEAR]
  
  ## --- Fishing_Effort_by_Fleet - the ONLY genuine effort data in this
  ## whole pipeline (SAU has no effort variable at all) -------------------
  top_effort_gears <- effort[, .(g_tot = sum(nom_active, na.rm = TRUE)), by = gear][order(-g_tot)][seq_len(min(N_TOP_GEARS, .N)), gear]
  effort[, gear_grp := ifelse(gear %in% top_effort_gears, gear, "Other gear")]
  effort[, Fleet := paste0(country, " - ", gear_grp)]
  fishmip_effort_by_fleet <- effort[, .(nom_active_kWdays = sum(nom_active, na.rm = TRUE)),
                                    by = .(Fleet, country, gear = gear_grp, sector, f_group, Year = year)]
  setorder(fishmip_effort_by_fleet, -nom_active_kWdays)
  message("[FishMIP] Fishing_Effort_by_Fleet: ", uniqueN(fishmip_effort_by_fleet$Fleet), " fleet(s) (country x gear).",
          " NOTE: this is a DIFFERENT fleet resolution than Catches_by_Fleet (SAU) - FishMIP's own catch file",
          " (below) has NO gear dimension, so effort-by-gear and catch-by-gear can never be joined directly,",
          " only compared side by side (see the source module's own header comment).")
  
  catch <- as.data.table(arrow::read_parquet(FISHMIP_CATCH_PARQUET))
  catch[, year := as.integer(year)]
  catch[, country := FISHMIP_SAUP_TO_COUNTRY[as.character(saup)]]
  catch <- catch[!is.na(country) & year >= START_YEAR & year <= END_YEAR]
  
  ## "FishMIP" Yield = reported + discards (landings + discards, per
  ## this script's Yield definition) - IUU kept as its own audit column,
  ## NOT included in Yield by default, since the user's definition was
  ## landings + discards specifically. total_catch (reported+iuu+discards,
  ## FishMIP's own definition) is kept alongside for reference.
  catch[, Yield_t := reported + discards]
  fishmip_country_total <- catch[, .(tonnes = sum(Yield_t, na.rm = TRUE),
                                     tonnes_incl_iuu = sum(reported + iuu + discards, na.rm = TRUE)),
                                 by = .(country, year)]
  fishmip_country_total[, source := "FishMIP (reported+discards)"]
  fishmip_country_reported <- catch[, .(tonnes = sum(reported, na.rm = TRUE)), by = .(country, year)]
  fishmip_country_reported[, source := "FishMIP reported-only"]
  
  if (!is.null(FISHMIP_FG_CROSSWALK_PATH) && file.exists(FISHMIP_FG_CROSSWALK_PATH)) {
    fg_crosswalk <- fread(FISHMIP_FG_CROSSWALK_PATH)
    if (!all(c("fishmip_f_group", "FG_num") %in% names(fg_crosswalk))) {
      stop("FISHMIP_FG_CROSSWALK_PATH must have columns fishmip_f_group, FG_num.")
    }
    catch_fg <- merge(catch, fg_crosswalk, by.x = "f_group", by.y = "fishmip_f_group", all.x = TRUE)
    catch_fg <- merge(catch_fg, unique(dataframe2[, .(FG_num, FG_name)]), by = "FG_num", all.x = TRUE)
    fishmip_fg_total <- catch_fg[!is.na(FG_num), .(Catch_t = sum(Yield_t, na.rm = TRUE)), by = .(Year = year, FG_num, FG_name)]
    fishmip_fg_reported <- catch_fg[!is.na(FG_num), .(Catch_t = sum(reported, na.rm = TRUE)), by = .(Year = year, FG_num, FG_name)]
    message("[FishMIP] FG-level yield built via FISHMIP_FG_CROSSWALK_PATH - ", uniqueN(catch_fg[!is.na(FG_num), FG_num]),
            " FG(s) covered.")
  } else {
    message("[FishMIP] No FISHMIP_FG_CROSSWALK_PATH supplied - FishMIP stays informational-only (country-level,",
            " in DataSources_Catch) and CANNOT be selected as DATA_SOURCE, since it has no FG_num mapping without",
            " one. FishMIP's own functional groups are not this pipeline's FG scheme (see this script's header).")
  }
}

## =================================================================
## STEP 7: FAO-GFCM passthrough (this pipeline's existing source)
## =================================================================
if (!file.exists(FAO_GFCM_FG_CSV) && isTRUE(AUTO_RUN_MISSING_SOURCES)) {
  message("[FAO-GFCM] '", FAO_GFCM_FG_CSV, "' not found - attempting to auto-run 02_fao_catches.R now",
          " to produce it (set AUTO_RUN_MISSING_SOURCES <- FALSE above to skip this and just fall",
          " back to 'not found' instead). This re-runs that script's OWN full pipeline (its own",
          " species matching, WoRMS lookups, GSA area calc, etc.) - can take a while ...")
  ## Setting DATASET_VERSION here (matching FAO_GFCM_DATASET_VERSION above)
  ## before sourcing is what makes 02_fao_catches.R produce the SAME
  ## <DATASET_VERSION> this script is actually looking for, instead of
  ## silently falling back to its own unconditional "GFCM_2025" default -
  ## see 02_fao_catches.R's own STEP 1/DATASET_VERSION comments for the
  ## "reuse if the caller already set it" pattern this relies on.
  DATASET_VERSION <- FAO_GFCM_DATASET_VERSION
  ## Setting CATCHES_DATA_SOURCE <- NULL here is NOT optional - this
  ## auto-run exists specifically to regenerate the genuine FAO-GFCM-
  ## only figure this script's own DataSources_Catch/fg_gfcm_fg_total
  ## (and Script 4's FISHERIES_DATA_SOURCE = "FAO_GFCM") depend on.
  ## 02_fao_catches.R's OWN default for CATCHES_DATA_SOURCE is "SAU" (not
  ## NULL) - by the time this STEP runs, STEP 5 above has already
  ## written fg_catch_timeseries_SAU.csv/fleet_structure_from_sau.csv
  ## into out_dir, so WITHOUT this override, 02_fao_catches.R's own
  ## default would happily pick those up and hand back SAU's total
  ## mislabeled as "FAO-GFCM" - silently corrupting the one sheet this
  ## whole STEP exists to keep genuinely FAO-GFCM-only. See 02_fao_
  ## catches.R's own CATCHES_DATA_SOURCE comment for the same "reuse if
  ## the caller already set it" pattern this relies on.
  CATCHES_DATA_SOURCE <- NULL
  fao_auto_ok <- tryCatch({
    source(file.path(git_dir, "scripts/02_fao_catches.R"))
    TRUE
  }, error = function(e) {
    message("[FAO-GFCM] Auto-run of 02_fao_catches.R failed: ", conditionMessage(e),
            " - falling back to 'not found'.")
    FALSE
  })
  if (isTRUE(fao_auto_ok) && !file.exists(FAO_GFCM_FG_CSV)) {
    message("[FAO-GFCM] Auto-run finished without an error but '", FAO_GFCM_FG_CSV, "' still doesn't",
            " exist - double check FAO_GFCM_DATASET_VERSION above actually matches one of",
            " 02_fao_catches.R's own DATASET_VERSION options (\"FAO_2020\"/\"GFCM_2025\"/\"FAO_2026\").")
  }
}

fao_gfcm_fg_total <- data.table()
if (file.exists(FAO_GFCM_FG_CSV)) {
  fao_gfcm_fg_total <- fread(FAO_GFCM_FG_CSV)
  message("[FAO-GFCM] Read ", nrow(fao_gfcm_fg_total), " rows from 02_fao_catches.R's own output ('", FAO_GFCM_FG_CSV, "').",
          " Landings only (no discards) - see 02_fao_catches.R's own caveats, unchanged here.")
} else {
  message("[FAO-GFCM] '", FAO_GFCM_FG_CSV, "' not found",
          if (isTRUE(AUTO_RUN_MISSING_SOURCES)) {
            " (auto-run was attempted above and did not produce it - see its own messages for why)."
          } else {
            " - set AUTO_RUN_MISSING_SOURCES <- TRUE above to have this script auto-run 02_fao_catches.R,"
          },
          " or run 02_fao_catches.R yourself first if you want FAO-GFCM included.",
          " (Its own DATASET_VERSION must match FAO_GFCM_DATASET_VERSION above.)")
}
## FAO-GFCM's own country-year totals aren't computed here - it's
## Division-, not country-, resolved, so it doesn't share a join key
## with the SAU/FishMIP country tables below. It still appears in
## DataSources_Catch, at FG level only, alongside the others.

## =================================================================
## STEP 8: DataSources_Catch - every source's Yield, side by side
## =================================================================
country_sources <- rbindlist(list(
  if (nrow(sau_total_cy) > 0) sau_total_cy,
  if (nrow(sau_reported_cy) > 0) sau_reported_cy,
  if (nrow(fishmip_country_total) > 0) fishmip_country_total[, .(country, year, tonnes, source)],
  if (nrow(fishmip_country_reported) > 0) fishmip_country_reported
), fill = TRUE)
fg_sources <- rbindlist(list(
  if (nrow(sau_fg_total) > 0) sau_fg_total[, `:=`(source = "SAU total (reconstructed)")],
  if (nrow(sau_fg_reported) > 0) sau_fg_reported[, `:=`(source = "SAU w/o unreported")],
  if (nrow(fishmip_fg_total) > 0) fishmip_fg_total[, `:=`(source = "FishMIP (reported+discards)")],
  if (nrow(fishmip_fg_reported) > 0) fishmip_fg_reported[, `:=`(source = "FishMIP reported-only")],
  if (nrow(fao_gfcm_fg_total) > 0) fao_gfcm_fg_total[, `:=`(source = "FAO-GFCM (landings only)")]
), fill = TRUE)

message("\nDataSources_Catch: ", nrow(country_sources), " country-year row(s) across ",
        uniqueN(country_sources$source), " source(s); ", nrow(fg_sources), " FG-year row(s) across ",
        uniqueN(fg_sources$source), " source(s).")

## =================================================================
## STEP 9: Write everything into the shared workbook
## =================================================================
sheets_to_write <- list()
if (nrow(sau_catches_by_fleet_wide) > 0) sheets_to_write$Catches_by_Fleet <- sau_catches_by_fleet_wide
if (nrow(sau_catches_by_fleet) > 0) sheets_to_write$Catches_by_Fleet_AllYears <- sau_catches_by_fleet
if (nrow(fishmip_effort_by_fleet) > 0) sheets_to_write$Fishing_Effort_by_Fleet <- fishmip_effort_by_fleet
if (nrow(country_sources) > 0 || nrow(fg_sources) > 0) {
  sheets_to_write$DataSources_Catch <- rbindlist(list(
    if (nrow(country_sources) > 0) country_sources[, .(level = "country", country, FG_num = NA_integer_,
                                                       FG_name = NA_character_, Year = year, tonnes, source)],
    if (nrow(fg_sources) > 0) fg_sources[, .(level = "FG", country = NA_character_, FG_num, FG_name,
                                             Year, tonnes = Catch_t, source)]
  ), fill = TRUE)
}
if (length(sheets_to_write) > 0) {
  upsert_workbook_sheets(sheets_to_write, ECOPATH_WORKBOOK_PATH)
} else {
  message("Nothing to write - no source produced usable output this run (check the [SAU]/[FishMIP]/[FAO-GFCM]",
          " messages above for why).")
}

## =================================================================
## STEP 10: Wiring into 04_pbqb_calc.R
##
## This script does NOT call attach_yield_from_landings()/write into
## 04_pbqb_calc.R itself - it only writes the contract files that
## mechanism already reads. To make DATA_SOURCE (set in Step 3 above)
## actually drive Fmort/PB=M+F there, update TWO lines in
## 04_pbqb_calc.R to match:
##
##   FG_YIELD_SOURCE   <- "fg_catch_csv"
##   FG_CATCH_CSV_PATH <- file.path(out_dir, "fg_catch_timeseries_<DATA_SOURCE>.csv")
##
## and, for SAU specifically (the only source with real species-level
## resolution), optionally also:
##
##   YIELD_SOURCE     <- "landings_csv"
##   LANDINGS_CSV_PATH <- file.path(out_dir, "landings_by_species_gsa_year_<DATA_SOURCE>.csv")
##
## (species-level always takes priority over FG-level inside
## 04_pbqb_calc.R when both are present - see that script's own notes).
## Neither file is written by 04_pbqb_calc.R itself, so this script
## writes them below, named after DATA_SOURCE so switching sources
## later doesn't silently reuse a stale file from a previous run.
## =================================================================
fg_out_path <- file.path(out_dir, paste0("fg_catch_timeseries_", DATA_SOURCE, ".csv"))
species_out_path <- file.path(out_dir, paste0("landings_by_species_gsa_year_", DATA_SOURCE, ".csv"))

fg_yield_for_source <- switch(DATA_SOURCE,
                              SAU = sau_fg_total,
                              SAU_no_unreported = sau_fg_reported,
                              FishMIP = fishmip_fg_total,
                              FAO_GFCM = fao_gfcm_fg_total,
                              stop("DATA_SOURCE must be one of: SAU, SAU_no_unreported, FishMIP, FAO_GFCM")
)

if (is.null(fg_yield_for_source) || nrow(fg_yield_for_source) == 0) {
  message("\nDATA_SOURCE = '", DATA_SOURCE, "' produced NO FG-level rows this run (see the source's own",
          " message above for why) - fg_catch_timeseries_", DATA_SOURCE, ".csv was NOT written. Fix the",
          " underlying source, or change DATA_SOURCE, before pointing 04_pbqb_calc.R at it.")
} else {
  fwrite(fg_yield_for_source[, .(Year, FG_num, FG_name, Catch_t)], fg_out_path)
  message("\nWrote ", fg_out_path, " (", nrow(fg_yield_for_source), " rows) - point 04_pbqb_calc.R's",
          " FG_CATCH_CSV_PATH at this file (FG_YIELD_SOURCE <- 'fg_catch_csv') to drive Fmort_FG/PB=M+F",
          " from DATA_SOURCE = '", DATA_SOURCE, "'.")
}

if (DATA_SOURCE %in% c("SAU", "SAU_no_unreported") && nrow(sau_species_yield) > 0) {
  fwrite(sau_species_yield[, .(ScientificName, Year, AreaID, catch_t)], species_out_path)
  message("Wrote ", species_out_path, " (", nrow(sau_species_yield), " rows, species-level, AreaID left NA -",
          " see this script's header) - point 04_pbqb_calc.R's LANDINGS_CSV_PATH at this file",
          " (YIELD_SOURCE <- 'landings_csv') for species-level Fmort, which takes priority over FG-level",
          " there whenever both are present.")
} else if (DATA_SOURCE %in% c("FishMIP", "FAO_GFCM")) {
  message("DATA_SOURCE = '", DATA_SOURCE, "' has no species-level resolution in this script - only the",
          " FG-level file above was written. (FishMIP: functional-group only, no species; FAO-GFCM: species-",
          " level catch is matched then aggregated straight to FG inside 02_fao_catches.R, never written to",
          " its own species-level CSV.)")
}

message("\nDone (02a_fisheries_multisource.R). Re-run this script any time a source's underlying data changes -",
        " every step above is idempotent (upsert_workbook_sheets() + fwrite() overwrite, they don't append).")