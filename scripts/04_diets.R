## =================================================================
## Created by: Daniel Vilas
## PIPELINE STEP 4 of 4 (optional) - run AFTER 01_biomass.R
## Builds an EwE-format functional-group (FG) diet-composition matrix
## from the Mediterranean trophic metaweb database (DATA_ENTRY
## + Taxonomic_codes + Non_taxonomic_groups tabs), instead of from a
## stomach-content Access export or a FishBase/SeaLifeBase pull (those
## were the two source paths the earlier diet_to_ewe.R supported - see
## diet_to_ewe_tool_notes.md in the project for that history).
## REQUIRES 01_biomass.R to have run at least once against the SAME
## ECOPATH_WORKBOOK_PATH first - species->FG membership comes from
## FG_WMed_2026.csv directly (no dependency on 01_biomass.R for that part),
## but each species' SHARE OF ITS FG's BIOMASS (needed to blend several
## species' diets into one FG-level diet) is read from 01_biomass.R's
## own biomass_proportion_by_species_fg.csv - that's "proportion
## B[iomass]" straight from the biomass code, not a hand-typed number.
## Produces: diet_composition_ewe.csv (plain-text EwE import format)
## and the Ecopath_diet sheet in the shared workbook.
## =================================================================

## =================================================================
## 04_diets.R
##
## (2026-09-16, two rounds of changes; updated again to stop reading
## FG_WMed.xlsx entirely): species->FG membership is read from
## FG_WMed_2026.csv (same file 01_biomass.R's fg_species_file reads),
## NOT a hand-built species_to_fg.csv and NOT the old FG_WMed.xlsx, and each species' share
## of its FG's biomass is read from the biomass code's own output
## (01_biomass.R's biomass_proportion_by_species_fg.csv, prop_sp_fg
## column) - NOT a hand-built species_biomass_in_fg.csv either. Both manual CSVs are
## now fallback-only, for species this diet run needs that genuinely
## aren't covered by either source. Everything else (predator/prey
## identity, diet proportions) comes straight from the metaweb file.
## =================================================================

pkgs <- c("data.table", "openxlsx", "stringr")
new_pkgs <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new_pkgs) > 0) install.packages(new_pkgs)
invisible(lapply(pkgs, library, character.only = TRUE))

## rfishbase - OPTIONAL here (unlike 03_pbqb-traits.R, which hard-stops
## on a missing/too-old install): the FishBase/SeaLifeBase diet()
## fallback added below (STEP 3b) is a best-effort tier below the real
## metaweb, not this script's core purpose, so a missing/broken
## rfishbase install just disables that one tier (message(), not
## stop()) rather than blocking the whole diet run the way it does in
## 03_pbqb-traits.R.

## =================================================================
## STEP 1: Configuration - SOURCE-ABLE SCRIPT, same convention as
## 01_biomass.R/02_fisheries.R/03_pbqb-traits.R: out_dir/pcloud_dir/
## git_dir and every knob below only take their default when not
## already set by a calling driver script (e.g. run_pipeline_demo.R).
## Running this standalone (nothing pre-set) resolves paths the same
## hardcoded-user-or-interactive-prompt way 01_biomass.R does.
## =================================================================
if (exists("out_dir", envir = .GlobalEnv, inherits = FALSE) &&
    exists("pcloud_dir", envir = .GlobalEnv, inherits = FALSE) &&
    exists("git_dir", envir = .GlobalEnv, inherits = FALSE)) {
  message("[04_diets.R] Using pre-set out_dir/pcloud_dir/git_dir from calling environment:\n  out_dir  = ", out_dir, "\n  pcloud_dir = ", pcloud_dir, "\n  git_dir  = ", git_dir)
} else if (tolower(Sys.info()[["user"]]) == "daniel" && .Platform$OS.type == "unix") {
  out_dir <- "/Users/daniel/Work/iMARES/WMed EwE Model/output/"
  pcloud_dir   <- "/Users/daniel/pCloud Drive/EwE Western Med 2026/"
  git_dir <-"/Users/daniel/Documents/GitHub/WMed_EwE/"
} else {
  if (!requireNamespace("rstudioapi", quietly = TRUE) || !rstudioapi::isAvailable()) {
    stop("This script requires RStudio. Please select the output directory manually.")
  }
  rstudioapi::showQuestion(title = "Select Output Directory",
                           message = "Please select the directory where output files and intermediate results will be saved.")
  out_dir <- rstudioapi::selectDirectory()
  if (is.null(out_dir) || out_dir == "" || !dir.exists(out_dir)) stop("No valid output directory selected.")
  
  rstudioapi::showQuestion(title = "Select pCloud EwE West Med Directory",
                           message = "Please select the location of the pCloud Drive/EwE Western Med 2026 folder.")
  pcloud_dir <- rstudioapi::selectDirectory()
  if (is.null(pcloud_dir) || pcloud_dir == "" || !dir.exists(pcloud_dir)) stop("No valid pcloud directory selected.")
  
  rstudioapi::showQuestion(title = "Select Github WMed_EwE Directory",
                           message = "Please select the directory where you cloned the WMed_EwE repository.")
  git_dir <- rstudioapi::selectDirectory()
  if (is.null(git_dir) || git_dir == "" || !dir.exists(git_dir)) stop("No valid Github directory selected.")
}

source(file.path(git_dir, "scripts/lib_survey_fg_density_functions.R"))  # for upsert_workbook_sheets() - writes Ecopath_diet into the same shared workbook
source(file.path(git_dir, "scripts/03b_ecobase.R"))  # for fetch_ecobase_raw_inputs()/WESTMED_BBOX (biomass/PB-QB fallback machinery, reused here - STEP 3c - for the EcoBase diet-matrix fallback) and fetch_ecobase_diet_matrix() added below in that same file

if (!exists("ECOPATH_WORKBOOK_PATH", envir = .GlobalEnv, inherits = FALSE)) ECOPATH_WORKBOOK_PATH <- file.path(out_dir, "ecopath_ecosim_inputs.xlsx")   # same shared workbook 01_biomass.R/02_fisheries.R/03_pbqb-traits.R write to

## 2026-09-17 update: this block's own native/intermediate CSV outputs
## go into their own "diet" subfolder, matching the other three blocks.
## BIOMASS_CSV_DIR points at 01_biomass.R's subfolder for this script's
## cross-block read of biomass_proportion_by_species_fg.csv.
if (!exists("csv_out_dir", envir = .GlobalEnv, inherits = FALSE)) csv_out_dir <- file.path(out_dir, "diet")
if (!dir.exists(csv_out_dir)) dir.create(csv_out_dir, recursive = TRUE)
if (!exists("BIOMASS_CSV_DIR", envir = .GlobalEnv, inherits = FALSE)) BIOMASS_CSV_DIR <- file.path(out_dir, "biomass")
if (!exists("METAWEB_XLSX_PATH",        envir = .GlobalEnv, inherits = FALSE)) METAWEB_XLSX_PATH        <- file.path(pcloud_dir, "data/Complementary data/data_entry_metaweb_empty.xlsx")   # Mediterranean trophic metaweb database - currently empty (template only, no DATA_ENTRY rows yet), but this is the file to read once it's populated
## FG_WMed_2026.csv - same file 01_biomass.R's fg_species_file and
## 02_fisheries.R's fg_file both read (species/FG_number/FG_name/
## taxonomy/source/status), NOT the old FG_WMed.xlsx sheet 4. No sheet
## parameter needed anymore - a CSV has no sheets.
if (!exists("FG_REFERENCE_CSV_PATH",    envir = .GlobalEnv, inherits = FALSE)) FG_REFERENCE_CSV_PATH    <- file.path(pcloud_dir, "data/FG_WMed_2026.csv")
if (!exists("SPECIES_TO_FG_MISSING_CSV_PATH", envir = .GlobalEnv, inherits = FALSE)) SPECIES_TO_FG_MISSING_CSV_PATH <- file.path(pcloud_dir, "data/species_to_fg_missing.csv")   # species, fg_name, proportion - ONLY for species not found in FG_REFERENCE_CSV_PATH; ok if the file doesn't exist (treated as empty)
if (!exists("SPECIES_BIOMASS_IN_FG_CSV_PATH", envir = .GlobalEnv, inherits = FALSE)) SPECIES_BIOMASS_IN_FG_CSV_PATH <- file.path(pcloud_dir, "data/species_biomass_in_fg_missing.csv")   # species, fg_name, biomass_proportion - ONLY for species x FG pairs not covered by ECOPATH_WORKBOOK_PATH's own FG_spp_Ecopath sheet; ok if the file doesn't exist
if (!exists("EWE_GROUP_TABLE_CSV_PATH", envir = .GlobalEnv, inherits = FALSE)) EWE_GROUP_TABLE_CSV_PATH <- file.path(pcloud_dir, "data/ewe_group_table.csv")   # group_number, group_name, is_predator - row/column order for the output matrix
if (!exists("OUTPUT_CSV_PATH",          envir = .GlobalEnv, inherits = FALSE)) OUTPUT_CSV_PATH          <- file.path(csv_out_dir, "diet_composition_ewe.csv")

## Which diet metric to use, in priority order, when a DATA_ENTRY row
## has more than one filled in (a study rarely reports all of them for
## the same predator-prey pair). IRI folds in Frequency AND either
## Weight or Number, so it's preferred when present; %Weight next
## (biomass-based, closest to what Ecopath actually wants); %Number;
## then Frequency alone (weakest - presence/occurrence only, no real
## proportion); Presence (1/0) is the last resort, treated as an equal
## split among a study's present prey when nothing else is available.
if (!exists("DIET_METRIC_PRIORITY", envir = .GlobalEnv, inherits = FALSE)) {
  DIET_METRIC_PRIORITY <- c("IRI", "WEIGHT", "NUMBER", "FREQUENCY", "Presence_(no_number_data)")
}

## --- Fallback tiers (added 2026-09-24) for predators the metaweb has
## NO usable DATA_ENTRY rows for - see STEP 3b/3c below and the
## run_pipeline() rewrite at the bottom. Metaweb rows always win where
## they exist; these only ever fill a GAP, never override a real
## metaweb entry, and every predator's diet is tagged with which of
## the three tiers (or "still missing") it actually came from.
if (!exists("DIET_FALLBACK_ENABLE_FISHBASE", envir = .GlobalEnv, inherits = FALSE)) DIET_FALLBACK_ENABLE_FISHBASE <- TRUE
if (!exists("DIET_FALLBACK_ENABLE_ECOBASE",  envir = .GlobalEnv, inherits = FALSE)) DIET_FALLBACK_ENABLE_ECOBASE  <- TRUE
if (!exists("DIET_STILL_MISSING_CSV_PATH",   envir = .GlobalEnv, inherits = FALSE)) DIET_STILL_MISSING_CSV_PATH   <- file.path(csv_out_dir, "diet_still_missing_REVIEW.csv")

message("[04_diets.R] Config: METAWEB_XLSX_PATH = ", METAWEB_XLSX_PATH,
        " | FG_REFERENCE_CSV_PATH = ", FG_REFERENCE_CSV_PATH,
        " | ECOPATH_WORKBOOK_PATH = ", ECOPATH_WORKBOOK_PATH,
        " | SPECIES_TO_FG_MISSING_CSV_PATH = ", SPECIES_TO_FG_MISSING_CSV_PATH,
        " | SPECIES_BIOMASS_IN_FG_CSV_PATH = ", SPECIES_BIOMASS_IN_FG_CSV_PATH,
        " | EWE_GROUP_TABLE_CSV_PATH = ", EWE_GROUP_TABLE_CSV_PATH)

## =================================================================
## STEP 2: Read the metaweb workbook
## =================================================================
read_metaweb <- function(path) {
  data_entry <- as.data.table(openxlsx::read.xlsx(path, sheet = "DATA_ENTRY", detectDates = FALSE))
  tax_codes  <- as.data.table(openxlsx::read.xlsx(path, sheet = "Taxonomic_codes", detectDates = FALSE))
  nontax     <- as.data.table(openxlsx::read.xlsx(path, sheet = "Non_taxonomic_groups", startRow = 4, detectDates = FALSE))  # real header is row 4 (rows 1-3 are a title block)
  setnames(nontax, old = "Group.code", new = "Group_code")
  list(data_entry = data_entry, tax_codes = tax_codes, nontax_groups = nontax)
}

## =================================================================
## STEP 3: Resolve a Code_predator/Code_prey value to a scientific
## name (or a generic-group label, e.g. "Pelagic fish" for Pefi_grp).
## Taxonomic_codes is checked first (real species/genus-level codes),
## then Non_taxonomic_groups (catch-all/ambiguous categories) - a code
## should only ever match one of the two tables, but Taxonomic_codes
## wins if a code somehow appears in both, since a real taxonomic
## identification is always more useful downstream than a generic bin.
## =================================================================
build_code_lookup <- function(tax_codes, nontax_groups) {
  from_tax <- data.table(
    code      = tax_codes$Valid_code,
    name      = tax_codes$valid_name,
    is_group  = FALSE
  )
  from_nontax <- data.table(
    code      = nontax_groups$Group_code,
    name      = nontax_groups$Group_name,
    is_group  = TRUE
  )
  lookup <- rbind(from_tax, from_nontax)
  lookup <- lookup[!is.na(code) & code != ""]
  lookup <- unique(lookup, by = "code")   # Taxonomic_codes rows come first in rbind, so duplicates keep the taxonomic match
  lookup
}

resolve_codes <- function(codes, lookup) {
  m <- lookup[match(codes, code)]
  unresolved <- codes[is.na(m$name)]
  if (length(unresolved) > 0) {
    message("[04_diets.R] ", length(unique(unresolved)), " code(s) not found in Taxonomic_codes or ",
            "Non_taxonomic_groups - left as their raw code instead of a resolved name: ",
            paste(head(unique(unresolved), 10), collapse = ", "),
            if (length(unique(unresolved)) > 10) ", ..." else "")
  }
  fifelse(is.na(m$name), codes, m$name)
}

## =================================================================
## STEP 4: Pick ONE diet-proportion value per DATA_ENTRY row, using
## DIET_METRIC_PRIORITY. Presence is handled specially further down
## (STEP 5) since it needs to be turned into an equal-split proportion
## per predator-study group, not read as a raw value.
## =================================================================
pick_diet_metric <- function(dt) {
  metric_cols <- intersect(DIET_METRIC_PRIORITY, names(dt))
  if (length(metric_cols) == 0) stop("None of DIET_METRIC_PRIORITY's columns (", paste(DIET_METRIC_PRIORITY, collapse=", "), ") found in DATA_ENTRY.")
  dt[, diet_value  := NA_real_]
  dt[, diet_metric := NA_character_]
  for (col in metric_cols) {
    if (col == "Presence_(no_number_data)") next   # handled in STEP 5
    take <- is.na(dt$diet_value) & !is.na(dt[[col]])
    dt[take, diet_value  := as.numeric(get(col))]
    dt[take, diet_metric := col]
  }
  dt
}

## =================================================================
## STEP 5: Build one SPECIES-LEVEL diet vector per predator - mean
## proportion contributed by each prey across every DATA_ENTRY record
## for that predator (weighted by predator_number/samples_number when
## given, otherwise a plain mean across records), renormalized to sum
## to 1. Presence-only records (no quantitative metric at all) are
## converted to an equal split among that STUDY's present prey first,
## then folded into the same average as everything else.
## =================================================================
build_species_diet <- function(data_entry, lookup) {
  dt <- copy(data_entry)
  dt <- dt[!is.na(Code_predator) & !is.na(Code_prey)]
  dt <- pick_diet_metric(dt)
  
  presence_col <- "Presence_(no_number_data)"
  if (presence_col %in% names(dt)) {
    dt[, has_any_quant := any(!is.na(diet_value)), by = .(Reference, Code_predator)]
    presence_rows <- !dt$has_any_quant & !is.na(dt[[presence_col]]) & dt[[presence_col]] == 1
    if (any(presence_rows)) {
      dt[presence_rows, n_present := sum(get(presence_col) == 1, na.rm = TRUE), by = .(Reference, Code_predator)]
      dt[presence_rows, diet_value  := 1 / n_present]
      dt[presence_rows, diet_metric := presence_col]
    }
    dt[, has_any_quant := NULL]
  }
  
  dt <- dt[!is.na(diet_value)]
  if (nrow(dt) == 0) {
    ## 2026-09-24 change: no longer a hard stop() - the empty metaweb
    ## template genuinely ships with zero usable DATA_ENTRY rows, and
    ## run_pipeline() now has two more fallback tiers (FishBase/
    ## SeaLifeBase diet(), then EcoBase - see STEP 3b/3c below) it can
    ## try per-predator instead of failing the whole diet block. An
    ## empty result here just means "the metaweb tier contributes
    ## nothing this run", which is a normal, expected state, not an error.
    message("[04_diets.R] build_species_diet(): no usable diet records after applying DIET_METRIC_PRIORITY - ",
            "the metaweb DATA_ENTRY tab has no real diet-study rows yet (the empty template ships with none). ",
            "Falling through to the FishBase/SeaLifeBase and EcoBase fallback tiers for every predator.")
    return(list(species_diet = data.table(predator_name = character(), prey_name = character(),
                                          proportion = numeric(), n_studies = integer()),
                predator_references = data.table(predator_name = character(), references = character())))
  }
  
  dt[, predator_name := resolve_codes(Code_predator, lookup)]
  dt[, prey_name     := resolve_codes(Code_prey, lookup)]
  
  dt[, study_weight := fifelse(!is.na(predator_number) & predator_number > 0, as.numeric(predator_number), 1)]
  
  dt[, study_total := sum(diet_value), by = .(Reference, Code_predator)]
  dt <- dt[study_total > 0]
  dt[, diet_norm := diet_value / study_total]
  
  species_diet <- dt[, .(
    proportion = sum(diet_norm * study_weight) / sum(study_weight),
    n_studies  = uniqueN(Reference)
  ), by = .(predator_name, prey_name)]
  
  species_diet[, proportion := proportion / sum(proportion), by = predator_name]
  
  ## predator_references (added 2026-09-17): the distinct study
  ## citations (metaweb's own "Reference" column) actually used for
  ## each predator - carried separately from species_diet itself
  ## (which only keeps a study COUNT, n_studies) so run_pipeline() can
  ## roll this up to per-FG citations for the final workbook's
  ## References sheet (ref_diet column) without re-reading DATA_ENTRY.
  predator_references <- dt[, .(references = paste(sort(unique(Reference)), collapse = "; ")), by = predator_name]
  
  list(species_diet = species_diet[], predator_references = predator_references[])
}

## =================================================================
## STEP 3b (added 2026-09-24): FishBase/SeaLifeBase diet() fallback for
## predators with NO usable metaweb DATA_ENTRY rows. Same rfishbase
## dependency 03_pbqb-traits.R already uses (see that file's own
## version-pinning check - NOT repeated here since this tier is
## optional/best-effort, not required for this script to run at all).
##
## rfishbase::diet() itself: returns one row per (species, prey item)
## for whatever stomach-content studies FishBase/SeaLifeBase have
## digitized. Its exact column layout has changed across rfishbase
## versions (confirmed by 03_pbqb-traits.R's own comments elsewhere in
## this pipeline about schema drift) - NOT independently verified from
## this session (no network access here, no R environment either), so
## every column this function needs is resolved DEFENSIVELY from a
## small set of candidate names, with whatever is actually found
## printed so a wrong guess is visible immediately rather than a silent
## empty/garbage result. Known/likely candidates, based on rfishbase's
## documented diet() table: a prey-item text field (FoodI/FoodII/
## FoodIII/Foodname/PreyStage), a study identifier (DietCode/StudyDuration/
## SampleStage or similar - used the same way metaweb's "Reference"
## groups records into one study), and a proportion field
## (DietPercent/Percentage/PropDietCompo - FishBase reports this on a
## 0-100 %Weight-or-%Volume-ish scale, hence the /100 below).
##
## Output shape matches build_species_diet()'s own return EXACTLY
## (list(species_diet, predator_references), same columns) so nothing
## downstream (build_species_to_fg/build_biomass_in_fg/build_fg_diet)
## needs to change - the fallback is a drop-in additional row source,
## not a parallel code path.
fetch_fishbase_diet_for_species <- function(species_names) {
  empty_result <- list(species_diet = data.table(predator_name = character(), prey_name = character(),
                                                 proportion = numeric(), n_studies = integer()),
                       predator_references = data.table(predator_name = character(), references = character()))
  if (length(species_names) == 0) return(empty_result)
  if (!requireNamespace("rfishbase", quietly = TRUE)) {
    message("[04_diets.R] fetch_fishbase_diet_for_species(): 'rfishbase' package not installed - skipping the ",
            "FishBase/SeaLifeBase diet fallback for ", length(species_names), " predator(s): ",
            paste(head(species_names, 10), collapse = ", "), if (length(species_names) > 10) ", ..." else "")
    return(empty_result)
  }
  
  fetch_one_server <- function(server) {
    tryCatch(as.data.table(rfishbase::diet(species_names, server = server)),
             error = function(e) {
               message("[04_diets.R] rfishbase::diet(server = '", server, "') failed: ", conditionMessage(e))
               data.table()
             })
  }
  diet_raw <- rbindlist(list(fetch_one_server("fishbase"), fetch_one_server("sealifebase")), fill = TRUE)
  if (nrow(diet_raw) == 0) {
    message("[04_diets.R] fetch_fishbase_diet_for_species(): rfishbase::diet() returned no rows for any of the ",
            length(species_names), " predator(s) queried (checked both fishbase and sealifebase servers).")
    return(empty_result)
  }
  
  message("[04_diets.R] rfishbase::diet() columns available (resolving defensively - schema drifts across ",
          "rfishbase versions): ", paste(names(diet_raw), collapse = ", "))
  
  species_col  <- intersect(c("Species", "SpecCode", "sciname"), names(diet_raw))[1]
  prey_col     <- intersect(c("FoodI", "FoodII", "FoodIII", "Foodname", "PreyStage", "Prey"), names(diet_raw))[1]
  pct_col      <- intersect(c("DietPercent", "Percentage", "PropDietCompo", "PercentFood"), names(diet_raw))[1]
  study_col    <- intersect(c("DietCode", "StudyDuration", "SampleStage", "C_Code", "StockCode"), names(diet_raw))[1]
  
  if (is.na(species_col) || is.na(prey_col)) {
    message("[04_diets.R] fetch_fishbase_diet_for_species(): couldn't find a usable species and/or prey-item ",
            "column in rfishbase::diet()'s output (columns present: ", paste(names(diet_raw), collapse = ", "),
            ") - skipping this fallback tier. Update the candidate names above once the real column is known.")
    return(empty_result)
  }
  if (is.na(pct_col)) {
    message("[04_diets.R] fetch_fishbase_diet_for_species(): no diet-percentage column found (looked for ",
            "DietPercent/Percentage/PropDietCompo/PercentFood among: ", paste(names(diet_raw), collapse = ", "),
            ") - treating every returned prey item as an equal-split PRESENCE record instead (same fallback rule ",
            "build_species_diet() uses for the metaweb's own Presence_(no_number_data) column).")
  }
  if (is.na(study_col)) study_col <- NA_character_  # no per-study grouping available - treat every row as its own "study"
  
  dt <- copy(diet_raw)
  setnames(dt, species_col, "predator_name")
  setnames(dt, prey_col, "prey_name")
  dt <- dt[!is.na(predator_name) & predator_name != "" & !is.na(prey_name) & prey_name != ""]
  if (nrow(dt) == 0) return(empty_result)
  
  dt[, study_id := if (!is.na(study_col)) as.character(get(study_col)) else paste(predator_name, seq_len(.N))]
  
  if (!is.na(pct_col)) {
    dt[, diet_value := suppressWarnings(as.numeric(get(pct_col)))]
  } else {
    dt[, diet_value := NA_real_]
  }
  ## Presence-only fallback (no usable % column, or a % value missing on
  ## some rows) - equal split among that predator+study's present prey,
  ## same rule as build_species_diet()'s STEP 5 Presence handling.
  dt[, n_present := .N, by = .(predator_name, study_id)]
  dt[is.na(diet_value), diet_value := 1 / n_present]
  
  dt <- dt[!is.na(diet_value) & diet_value > 0]
  if (nrow(dt) == 0) return(empty_result)
  
  dt[, study_total := sum(diet_value), by = .(predator_name, study_id)]
  dt <- dt[study_total > 0]
  dt[, diet_norm := diet_value / study_total]
  
  species_diet <- dt[, .(
    proportion = mean(diet_norm),
    n_studies  = uniqueN(study_id)
  ), by = .(predator_name, prey_name)]
  species_diet[, proportion := proportion / sum(proportion), by = predator_name]
  
  predator_references <- dt[, .(references = paste0("FishBase/SeaLifeBase diet() [", uniqueN(study_id), " study record(s)]")), by = predator_name]
  
  message("[04_diets.R] FishBase/SeaLifeBase diet() fallback resolved ", uniqueN(species_diet$predator_name),
          " of ", length(species_names), " requested predator(s): ",
          paste(head(unique(species_diet$predator_name), 10), collapse = ", "),
          if (uniqueN(species_diet$predator_name) > 10) ", ..." else "")
  
  list(species_diet = species_diet[], predator_references = predator_references[])
}

## =================================================================
## STEP 3c (added 2026-09-24): EcoBase diet-matrix fallback, for
## predator FGs still uncovered after metaweb + FishBase/SeaLifeBase
## (STEP 3b above). Unlike those two tiers, this one operates at the
## FG level directly (predator_fg/prey_fg/weight - the SAME shape
## build_fg_diet() itself returns), rather than at species level, since
## EcoBase's own <group> nodes are already FG-like model compartments,
## not individual species - there's no meaningful species_to_fg step to
## run for an EcoBase match. fetch_ecobase_diet_by_keyword()
## (03b_ecobase.R) does the actual query/matching (same keyword-match-
## against-other-Mediterranean-models + closest-model-year pattern as
## fetch_ecobase_literature_biomass()); this just reshapes its output
## and is where the "diet matrix field may not exist on this endpoint
## at all" caveat (see 03b_ecobase.R's own header comment on this) is
## surfaced to the caller.
fetch_ecobase_diet_for_predator_fgs <- function(predator_fg_names, out_dir, target_year = NULL) {
  empty_result <- data.table(predator_fg = character(), prey_fg = character(), weight = numeric(), diet_ref = character())
  if (length(predator_fg_names) == 0) return(empty_result)
  
  keywords <- setNames(lapply(predator_fg_names, function(nm) unique(tolower(strsplit(nm, "[^A-Za-z0-9]+")[[1]]))), predator_fg_names)
  keywords <- lapply(keywords, function(k) k[nchar(k) >= 4])   # drop short/uninformative tokens (e.g. "sp", "of")
  keywords <- keywords[lengths(keywords) > 0]
  if (length(keywords) == 0) {
    message("[04_diets.R] fetch_ecobase_diet_for_predator_fgs(): no usable keyword could be derived from the ",
            length(predator_fg_names), " predator FG name(s) needing this fallback - skipping.")
    return(empty_result)
  }
  
  matches <- tryCatch(fetch_ecobase_diet_by_keyword(out_dir, target_predator_keywords = keywords, target_year = target_year),
                      error = function(e) {
                        message("[04_diets.R] fetch_ecobase_diet_for_predator_fgs(): fetch_ecobase_diet_by_keyword() failed - ",
                                conditionMessage(e))
                        NULL
                      })
  if (is.null(matches) || nrow(matches) == 0) {
    message("[04_diets.R] EcoBase diet fallback found nothing usable for any of the ", length(predator_fg_names),
            " predator FG(s) still missing a diet source: ", paste(predator_fg_names, collapse = ", "))
    return(empty_result)
  }
  
  ## Resolve prey-name/value columns defensively (see 03b_ecobase.R's
  ## fetch_ecobase_diet_matrix_for_model() header - the nested field
  ## names it parses are unverified guesses).
  prey_col  <- intersect(c("group_name", "prey_group_name", "prey_name", "Prey_name"), names(matches))[1]
  value_col <- intersect(c("value", "diet_proportion", "percent", "Percent", "DC"), names(matches))[1]
  if (is.na(prey_col) || is.na(value_col)) {
    message("[04_diets.R] fetch_ecobase_diet_for_predator_fgs(): EcoBase diet rows were found but have no ",
            "recognizable prey-name/value column pair (columns: ", paste(names(matches), collapse = ", "),
            ") - can't turn them into FG diet rows. Update the candidate names once the real field is known.")
    return(empty_result)
  }
  
  dt <- copy(matches)
  setnames(dt, prey_col, "prey_fg")
  setnames(dt, value_col, "raw_value")
  dt[, raw_value := suppressWarnings(as.numeric(raw_value))]
  dt <- dt[!is.na(raw_value) & raw_value > 0]
  if (nrow(dt) == 0) return(empty_result)
  
  dt[, weight := raw_value / sum(raw_value), by = TargetPredatorFG]
  if (!"Source_citation" %in% names(dt)) dt[, Source_citation := paste0("EcoBase model_id ", model_id)]
  
  out <- dt[, .(predator_fg = TargetPredatorFG, prey_fg, weight, diet_ref = Source_citation)]
  message("[04_diets.R] EcoBase diet fallback resolved ", uniqueN(out$predator_fg), " of ", length(predator_fg_names),
          " predator FG(s): ", paste(unique(out$predator_fg), collapse = ", "))
  out[]
}

## =================================================================
## STEP 6: Species -> FG, sourced from FG_WMed_2026.csv FIRST (the
## real starting point - every species already assigned an FG there),
## falling back to a small manual CSV ONLY for species this diet run
## actually needs but aren't in that reference file at all.
## =================================================================
read_species_to_fg_from_reference <- function(path) {
  ## fread(), not openxlsx::read.xlsx() - path is FG_WMed_2026.csv
  ## (species/FG_number/FG_name/taxonomy/source/status), not the old
  ## FG_WMed.xlsx sheet 4 (ESPECIE/GF/FG_name). Accepts either column
  ## naming so a pre-set FG_REFERENCE_CSV_PATH pointing at an older-style
  ## file still works.
  fg_raw <- fread(path)
  species_col <- intersect(c("species", "ESPECIE"), names(fg_raw))[1]
  name_col    <- intersect("FG_name", names(fg_raw))[1]
  if (is.na(species_col) || is.na(name_col)) {
    stop("read_species_to_fg_from_reference(): '", path, "' is missing expected column(s) - looked for a",
         " species column (species/ESPECIE) and FG_name, found: ", paste(names(fg_raw), collapse = ", "),
         " (expected the same species/FG_number/FG_name layout 01_biomass.R's fg_species_file reads).")
  }
  out <- unique(fg_raw[, .(species = get(species_col), fg_name = get(name_col), proportion = 1)])
  out <- out[!is.na(species) & species != ""]
  out
}

## =================================================================
## STEP 6c (added 2026-09-17): taxonomy-based fallback for
## species the metaweb needs but that are in NEITHER FG_WMed_2026.csv NOR
## the manual missing-species CSV. The diet database's own sheet of
## classification of diet items means the taxonomic classification can
## be used to adjust to the FG scheme: the metaweb workbook's own
## "Taxonomic_codes" tab carries a full WoRMS-derived classification
## (Kingdom -> ... -> Species) for every predator/prey taxon it uses,
## which resolve_codes() already draws on to name things in the first
## place. This reuses that SAME table (via fetch_taxonomy_metaweb(),
## lib_survey_fg_density_functions.R) - no network call, no new data
## source to configure - to assign a still-unresolved species to
## whichever FG its genus/family/order/class/phylum relatives among the
## ALREADY-assigned species (from FG_WMed_2026.csv + the missing-CSV) map to
## exclusively, or by majority if most (not tied) relatives agree. Same
## safe rule used throughout this project (see fallback_match_fg_by_
## taxonomy() in the shared lib): a rank value spanning multiple FGs
## with no clear majority is left unresolved rather than guessed.
##
## Deliberately a SEPARATE, self-contained implementation rather than
## reusing fallback_match_fg_by_taxonomy() directly - that function's
## data shape is FG_num/FG_name-keyed (built for 01_biomass.R's own
## fg_lookup_safe), while here there's no FG_num at all, just fg_name
## strings from FG_WMed_2026.csv/the missing CSV. Same algorithm, adapted
## to this script's own simpler (species, fg_name) shape.
## =================================================================
resolve_fg_votes_for_rank_diet <- function(reference_tax, rank) {
  if (!rank %in% names(reference_tax)) {
    return(data.table(rank_value_lower = character(0), fg_name = character(0), match_type = character(0)))
  }
  votes <- reference_tax[!is.na(get(rank)), .(n_species = uniqueN(species)), by = c(rank, "fg_name")]
  if (nrow(votes) == 0) return(data.table(rank_value_lower = character(0), fg_name = character(0), match_type = character(0)))
  setnames(votes, rank, "rank_value")
  votes[, rank_value_lower := tolower(rank_value)]
  votes[, n_fg := uniqueN(fg_name), by = rank_value_lower]
  votes[, is_top := n_species == max(n_species), by = rank_value_lower]
  tied <- unique(votes[n_fg > 1 & is_top == TRUE, .N, by = rank_value_lower][N > 1, rank_value_lower])
  out <- votes[n_fg == 1 | (is_top == TRUE & !(rank_value_lower %in% tied))]
  out[, match_type := fifelse(n_fg == 1, "exclusive", "majority")]
  unique(out[, .(rank_value_lower, fg_name, match_type)])
}

fallback_species_to_fg_via_taxonomy <- function(still_missing, already_assigned, tax_codes,
                                                rank_levels = c("Genus", "Family", "Order", "Class", "Phylum")) {
  if (length(still_missing) == 0) {
    return(data.table(species = character(0), fg_name = character(0), proportion = numeric(0),
                      match_type = character(0), match_rank = character(0)))
  }
  all_names <- unique(c(still_missing, already_assigned$species))
  tax_lookup <- fetch_taxonomy_metaweb(all_names, tax_codes)
  
  reference_tax <- merge(already_assigned, tax_lookup, by.x = "species", by.y = "ScientificName", all.x = TRUE)
  rank_cols <- c("Genus", "Family", "Order", "Class", "Phylum")
  missing_tax <- tax_lookup[match(still_missing, ScientificName)]
  
  resolved <- data.table(species = character(0), fg_name = character(0), match_type = character(0), match_rank = character(0))
  remaining <- data.table(species = still_missing, missing_tax[, ..rank_cols])
  for (rank in rank_levels) {
    if (nrow(remaining) == 0) break
    rank_votes <- resolve_fg_votes_for_rank_diet(reference_tax, rank)
    if (nrow(rank_votes) == 0 || !rank %in% names(remaining)) next
    remaining[, rank_value_lower := tolower(get(rank))]
    hits <- merge(remaining[!is.na(get(rank))], rank_votes, by = "rank_value_lower")
    if (nrow(hits) > 0) {
      hits[, match_rank := rank]
      resolved <- rbindlist(list(resolved, hits[, .(species, fg_name, match_type, match_rank)]), fill = TRUE)
      remaining <- remaining[!species %in% hits$species]
    }
    remaining[, rank_value_lower := NULL]
  }
  if (nrow(resolved) > 0) {
    message("[04_diets.R] ", nrow(resolved), " of ", length(still_missing), " species missing from both ",
            "FG_WMed_2026.csv and the fallback CSV were resolved via the metaweb's own Taxonomic_codes classification ",
            "(genus/family/order/class/phylum match against already-assigned relatives): ",
            paste(head(resolved$species, 10), collapse = ", "), if (nrow(resolved) > 10) ", ..." else "")
  }
  resolved[, proportion := 1]
  resolved[, .(species, fg_name, proportion, match_type, match_rank)]
}

build_species_to_fg <- function(needed_species, reference_path, missing_csv_path, tax_codes = NULL) {
  from_reference <- read_species_to_fg_from_reference(reference_path)
  still_missing  <- setdiff(needed_species, from_reference$species)
  
  from_missing_csv <- data.table(species = character(), fg_name = character(), proportion = numeric())
  if (length(still_missing) > 0) {
    if (file.exists(missing_csv_path)) {
      from_missing_csv <- as.data.table(read.csv(missing_csv_path, stringsAsFactors = FALSE))
      covered_by_csv <- intersect(still_missing, from_missing_csv$species)
      still_missing  <- setdiff(still_missing, from_missing_csv$species)
      if (length(covered_by_csv) > 0) {
        message("[04_diets.R] ", length(covered_by_csv), " species not in ", reference_path,
                " were covered by ", missing_csv_path, ": ", paste(covered_by_csv, collapse = ", "))
      }
    }
  }
  
  from_taxonomy <- data.table(species = character(), fg_name = character(), proportion = numeric())
  if (length(still_missing) > 0 && !is.null(tax_codes)) {
    already_assigned <- rbind(from_reference[species %in% needed_species], from_missing_csv, fill = TRUE)
    from_taxonomy <- fallback_species_to_fg_via_taxonomy(still_missing, already_assigned, tax_codes)
    if (nrow(from_taxonomy) > 0) {
      fwrite(from_taxonomy, file.path(dirname(missing_csv_path), "diet_species_fg_taxonomy_fallback.csv"))
      still_missing <- setdiff(still_missing, from_taxonomy$species)
    }
    from_taxonomy[, c("match_type", "match_rank") := NULL]
  }
  
  if (length(still_missing) > 0) {
    message("[04_diets.R] ", length(still_missing), " species appear in the metaweb diet data but have ",
            "NO FG assignment from FG_WMed_2026.csv, ", missing_csv_path, ", or the metaweb's own taxonomic ",
            "classification - they'll be dropped from the FG-level diet matrix. Add them to ", missing_csv_path,
            " (species, fg_name, proportion) to include them: ", paste(still_missing, collapse = ", "))
  }
  
  combined <- rbind(from_reference[species %in% needed_species], from_missing_csv, from_taxonomy, fill = TRUE)
  message("[04_diets.R] species_to_fg: ", uniqueN(combined$species), " of ", length(needed_species),
          " needed species resolved to an FG (", nrow(from_reference[species %in% needed_species]), " from ",
          reference_path, ", ", nrow(from_missing_csv), " from ", missing_csv_path, ", ", nrow(from_taxonomy),
          " from Taxonomic_codes classification fallback).")
  combined
}

## =================================================================
## STEP 6b: Each species' share of its FG's BIOMASS - read from
## 01_biomass.R's own biomass_proportion_by_species_fg.csv (columns
## FG_num, FG_name, Species, Density, prop_sp_fg), written next to the
## shared workbook - NOT a workbook sheet, and NOT a hand-typed number.
## The diet code should grab the proportion of biomass of species
## within FG that was produced as CSV from the biomass code run
## (2026-09-17): this is exactly that CSV, the same one
## 01_biomass.R's FG_spp_Ecopath.csv duplicates in wide form. Falls
## back to SPECIES_BIOMASS_IN_FG_CSV_PATH only for species x FG pairs
## that CSV doesn't cover (e.g. a species added via the missing-species
## fallback above, which 01_biomass.R never saw). Any pair covered by
## neither source still gets a sane default inside build_fg_diet()
## itself (an equal split among that FG's other mapped species).
## =================================================================
read_biomass_proportion_from_workbook <- function(workbook_path, csv_name = "biomass_proportion_by_species_fg",
                                                  biomass_csv_dir = NULL) {
  csv_path <- file.path(if (is.null(biomass_csv_dir)) dirname(workbook_path) else biomass_csv_dir,
                        paste0(csv_name, ".csv"))
  if (!file.exists(csv_path)) {
    message("[04_diets.R] ", csv_path, " not found - run 01_biomass.R against this same output ",
            "folder first to get real biomass shares. Falling back to SPECIES_BIOMASS_IN_FG_CSV_PATH only ",
            "for now (or the equal-split default inside build_fg_diet() for anything that misses too).")
    return(data.table(species = character(), fg_name = character(), biomass_proportion = numeric()))
  }
  fg_spp <- as.data.table(read.csv(csv_path, stringsAsFactors = FALSE))
  needed <- c("Species", "FG_name", "prop_sp_fg")
  missing_cols <- setdiff(needed, names(fg_spp))
  if (length(missing_cols) > 0) {
    stop("read_biomass_proportion_from_workbook(): '", csv_path, "' is missing expected column(s): ", paste(missing_cols, collapse = ", "))
  }
  out <- unique(fg_spp[, .(species = Species, fg_name = FG_name, biomass_proportion = prop_sp_fg)])
  out[!is.na(species) & species != ""]
}

build_biomass_in_fg <- function(needed_species_fg, workbook_path, fallback_csv_path, biomass_csv_dir = NULL) {
  from_workbook <- read_biomass_proportion_from_workbook(workbook_path, biomass_csv_dir = biomass_csv_dir)
  covered_pairs <- from_workbook[, paste(species, fg_name)]
  still_missing <- needed_species_fg[!paste(species, fg_name) %in% covered_pairs]
  
  from_fallback <- data.table(species = character(), fg_name = character(), biomass_proportion = numeric())
  if (nrow(still_missing) > 0 && file.exists(fallback_csv_path)) {
    from_fallback <- as.data.table(read.csv(fallback_csv_path, stringsAsFactors = FALSE))
    n_covered_by_fallback <- sum(paste(still_missing$species, still_missing$fg_name) %in% paste(from_fallback$species, from_fallback$fg_name))
    message("[04_diets.R] ", n_covered_by_fallback, " of ", nrow(still_missing), " species x FG pair(s) missing biomass share ",
            "from biomass_proportion_by_species_fg.csv were covered by ", fallback_csv_path, ".")
  } else if (nrow(still_missing) > 0) {
    message("[04_diets.R] ", nrow(still_missing), " species x FG pair(s) have no biomass share in either ",
            "biomass_proportion_by_species_fg.csv or ", fallback_csv_path, " - build_fg_diet() will fall back to ",
            "an equal split among that FG's other mapped species for these: ",
            paste(unique(still_missing$species), collapse = ", "))
  }
  
  combined <- rbind(from_workbook, from_fallback, fill = TRUE)
  message("[04_diets.R] biomass_in_fg: proportion B sourced from 01_biomass.R's FG_spp_Ecopath sheet for ",
          nrow(from_workbook), " species x FG pair(s), plus ", nrow(from_fallback), " from ", fallback_csv_path, ".")
  combined
}

## =================================================================
## STEP 7: Expand the species-level diet table across every (predator
## FG, prey FG) combination implied by species_to_fg's splits, weight
## predator-side rows by biomass_in_fg (so an FG's diet is dominated by
## its highest-biomass member species, not split evenly across however
## many species happen to be in it), then collapse to one number per
## (predator_fg, prey_fg).
## =================================================================
build_fg_diet <- function(species_diet, species_to_fg, biomass_in_fg) {
  chk1 <- species_to_fg[, .(total = sum(proportion)), by = species][abs(total - 1) > 1e-6]
  if (nrow(chk1) > 0) message("[04_diets.R] WARNING - species_to_fg: these species' proportions don't sum to 1 across their FG rows: ",
                              paste(chk1$species, collapse = ", "))
  chk2 <- biomass_in_fg[, .(total = sum(biomass_proportion)), by = fg_name][abs(total - 1) > 1e-6]
  if (nrow(chk2) > 0) message("[04_diets.R] WARNING - biomass_in_fg: these FGs' biomass proportions don't sum to 1 across their species rows: ",
                              paste(chk2$fg_name, collapse = ", "))
  
  pred_map <- merge(species_to_fg, biomass_in_fg, by = c("species", "fg_name"), all.x = TRUE)
  missing_biomass <- pred_map[is.na(biomass_proportion)]
  if (nrow(missing_biomass) > 0) {
    message("[04_diets.R] ", nrow(missing_biomass), " species x FG row(s) have no biomass share from any source - ",
            "defaulting biomass_proportion to an equal split among that FG's other mapped species. Affected: ",
            paste(unique(missing_biomass$species), collapse = ", "))
    pred_map[, n_in_fg := .N, by = fg_name]
    pred_map[is.na(biomass_proportion), biomass_proportion := 1 / n_in_fg]
    pred_map[, n_in_fg := NULL]
  }
  setnames(pred_map, c("species", "fg_name", "proportion", "biomass_proportion"),
           c("predator_name", "predator_fg", "pred_fg_share", "pred_biomass_share"))
  pred_map[, pred_weight := pred_fg_share * pred_biomass_share]
  
  prey_map <- copy(species_to_fg)
  setnames(prey_map, c("species", "fg_name", "proportion"), c("prey_name", "prey_fg", "prey_fg_share"))
  
  sd1 <- merge(species_diet, pred_map[, .(predator_name, predator_fg, pred_weight)], by = "predator_name", allow.cartesian = TRUE)
  sd2 <- merge(sd1, prey_map, by = "prey_name", all.x = TRUE, allow.cartesian = TRUE)
  sd2[is.na(prey_fg), `:=`(prey_fg = prey_name, prey_fg_share = 1)]
  
  fg_diet <- sd2[, .(
    weight = sum(proportion * pred_weight * prey_fg_share)
  ), by = .(predator_fg, prey_fg)]
  
  fg_diet[, weight := weight / sum(weight), by = predator_fg]
  fg_diet[]
}

## =================================================================
## STEP 8a: Write out in EwE's own plain-text diet-composition CSV
## layout - blank+"Prey \ predator" header, predator columns by group
## NUMBER (only groups flagged is_predator == TRUE/1), prey rows by
## group number + name (the FULL group list), decimal-comma QUOTED
## values, blank for zero, Import/Sum/(1-Sum) rows at the bottom.
## =================================================================
export_ewe_matrix_csv <- function(fg_diet, group_table, out_path) {
  group_table <- group_table[order(group_number)]
  predators   <- group_table[is_predator == TRUE | is_predator == 1]
  fmt <- function(x) {
    if (is.na(x) || x == 0) return("")
    paste0('"', sub("\\.", ",", format(round(x, 6), scientific = FALSE, trim = TRUE)), '"')
  }
  
  header <- c("Prey \\ predator", predators$group_number)
  lines  <- character(0)
  lines  <- c(lines, paste(header, collapse = ","))
  
  for (i in seq_len(nrow(group_table))) {
    target_prey_fg <- group_table$group_name[i]
    prey_num  <- group_table$group_number[i]
    row_vals  <- vapply(predators$group_name, function(pfg) {
      hit <- fg_diet[predator_fg == pfg & prey_fg == target_prey_fg]
      if (nrow(hit) == 0) 0 else hit$weight[1]
    }, numeric(1))
    lines <- c(lines, paste(c(paste0(prey_num, " ", target_prey_fg), vapply(row_vals, fmt, "")), collapse = ","))
  }
  
  col_sums <- vapply(predators$group_name, function(pfg) sum(fg_diet[predator_fg == pfg]$weight), numeric(1))
  lines <- c(lines, paste(c("Import", vapply(rep(0, length(col_sums)), fmt, "")), collapse = ","))
  lines <- c(lines, paste(c("Sum",    vapply(col_sums, fmt, "")), collapse = ","))
  lines <- c(lines, paste(c("1-Sum",  vapply(1 - col_sums, fmt, "")), collapse = ","))
  
  writeLines(lines, out_path)
  message("[04_diets.R] Wrote ", out_path, " (", nrow(predators), " predator column(s), ", nrow(group_table), " prey row(s)).")
  invisible(out_path)
}

## =================================================================
## STEP 8b: Ecopath_diet sheet (added 2026-09-16) - the
## SAME predator-by-FG-number x prey-by-FG-number matrix as the CSV
## above, written into ECOPATH_WORKBOOK_PATH as plain numeric values
## (no decimal-comma/quoting - that convention is specific to EwE's
## own plain-text import format, not needed inside an Excel sheet).
## =================================================================
build_ecopath_diet_sheet <- function(fg_diet, group_table) {
  group_table <- group_table[order(group_number)]
  predators   <- group_table[is_predator == TRUE | is_predator == 1]
  
  out <- data.table(Prey_FG_num = group_table$group_number, Prey_FG_name = group_table$group_name)
  for (i in seq_len(nrow(predators))) {
    pnum <- predators$group_number[i]; pname <- predators$group_name[i]
    vals <- vapply(group_table$group_name, function(target_prey_fg) {
      hit <- fg_diet[predator_fg == pname & prey_fg == target_prey_fg]
      if (nrow(hit) == 0) 0 else round(hit$weight[1], 6)
    }, numeric(1))
    out[, (paste0("Predator_", pnum, "_", gsub("[^A-Za-z0-9]+", "", pname))) := vals]
  }
  out[]
}

## =================================================================
## STEP 9: run_pipeline() - the one call this script itself makes at
## the bottom (auto-runs on source(), same convention as 01_biomass.R/
## 02_fisheries.R/03_pbqb-traits.R - unlike the earlier diet_to_ewe.R,
## which required a separate explicit call).
## =================================================================
run_pipeline <- function(metaweb_path = METAWEB_XLSX_PATH,
                         fg_reference_path = FG_REFERENCE_CSV_PATH,
                         species_to_fg_missing_path = SPECIES_TO_FG_MISSING_CSV_PATH,
                         biomass_fallback_path = SPECIES_BIOMASS_IN_FG_CSV_PATH,
                         group_table_path = EWE_GROUP_TABLE_CSV_PATH,
                         workbook_path = ECOPATH_WORKBOOK_PATH,
                         out_path = OUTPUT_CSV_PATH) {
  metaweb        <- read_metaweb(metaweb_path)
  lookup         <- build_code_lookup(metaweb$tax_codes, metaweb$nontax_groups)
  group_table    <- as.data.table(read.csv(group_table_path, stringsAsFactors = FALSE))   # moved up from STEP 9's old position - the fallback tiers below need the predator FG list before the diet matrix is built, not just at export time
  predator_fg_names_all <- unique(group_table[is_predator == TRUE | is_predator == 1]$group_name)
  
  species_diet_result <- build_species_diet(metaweb$data_entry, lookup)
  species_diet   <- species_diet_result$species_diet
  predator_references <- species_diet_result$predator_references
  if (nrow(predator_references) > 0) predator_references[, source := "metaweb"]
  metaweb_predators <- unique(species_diet$predator_name)
  message("[04_diets.R] Metaweb (DATA_ENTRY) tier: ", uniqueN(metaweb_predators), " predator(s) with real diet-study rows.")
  
  ## --- Fallback tier 1: FishBase/SeaLifeBase diet() (STEP 3b) --------
  ## "Predators needing this fallback" = every real species mapped (via
  ## FG_WMed_2026.csv) into a predator FG that the metaweb tier above did
  ## NOT already cover - independent of whether the metaweb has ANY rows
  ## at all (needed for the current empty-template case, where
  ## metaweb_predators is empty and every predator needs a fallback).
  species_to_fg_reference_only <- read_species_to_fg_from_reference(fg_reference_path)
  all_predator_species <- unique(species_to_fg_reference_only[fg_name %in% predator_fg_names_all]$species)
  missing_after_metaweb <- setdiff(all_predator_species, metaweb_predators)
  message("[04_diets.R] ", length(missing_after_metaweb), " of ", length(all_predator_species),
          " predator species (per ", fg_reference_path, ") have no metaweb diet-study rows and are candidates",
          " for the FishBase/SeaLifeBase and EcoBase fallback tiers.")
  
  fb_predators <- character(0)
  if (DIET_FALLBACK_ENABLE_FISHBASE && length(missing_after_metaweb) > 0) {
    fb_result <- fetch_fishbase_diet_for_species(missing_after_metaweb)
    if (nrow(fb_result$predator_references) > 0) fb_result$predator_references[, source := "fishbase_sealifebase"]
    species_diet         <- rbind(species_diet, fb_result$species_diet, fill = TRUE)
    predator_references  <- rbind(predator_references, fb_result$predator_references, fill = TRUE)
    fb_predators          <- unique(fb_result$species_diet$predator_name)
  } else if (!DIET_FALLBACK_ENABLE_FISHBASE) {
    message("[04_diets.R] DIET_FALLBACK_ENABLE_FISHBASE is FALSE - skipping the FishBase/SeaLifeBase diet fallback tier.")
  }
  missing_after_fishbase <- setdiff(missing_after_metaweb, fb_predators)
  
  all_named     <- unique(c(species_diet$predator_name, species_diet$prey_name))
  generic_names <- unique(metaweb$nontax_groups$Group_name)   # source of truth for "generic category, not a real species" - see build_code_lookup()'s own header comment for why lookup$is_group isn't used here
  needed_species <- setdiff(all_named, generic_names)
  if (length(intersect(all_named, generic_names)) > 0) {
    message("[04_diets.R] ", length(intersect(all_named, generic_names)), " generic non-taxonomic label(s) in play ",
            "(", paste(intersect(all_named, generic_names), collapse = ", "), ") - these pass through as their own FG ",
            "automatically and don't need a species_to_fg entry.")
  }
  
  species_to_fg <- build_species_to_fg(needed_species, fg_reference_path, species_to_fg_missing_path, tax_codes = metaweb$tax_codes)
  biomass_in_fg <- build_biomass_in_fg(unique(species_to_fg[, .(species, fg_name)]), workbook_path, biomass_fallback_path,
                                       biomass_csv_dir = BIOMASS_CSV_DIR)
  fg_diet       <- if (nrow(species_diet) > 0) build_fg_diet(species_diet, species_to_fg, biomass_in_fg) else data.table(predator_fg = character(), prey_fg = character(), weight = numeric())
  
  ## --- Fallback tier 2: EcoBase diet matrix (STEP 3c) ----------------
  ## Operates at the FG level directly (see that function's own header)
  ## and only for predator FGs that STILL have zero coverage after the
  ## metaweb + FishBase/SeaLifeBase tiers above - i.e. not one single
  ## mapped species contributed a diet row to fg_diet for that FG.
  predator_fg_covered   <- unique(fg_diet$predator_fg)
  predator_fg_missing    <- setdiff(predator_fg_names_all, predator_fg_covered)
  ecobase_diet_rows <- data.table(predator_fg = character(), prey_fg = character(), weight = numeric(), diet_ref = character())
  if (DIET_FALLBACK_ENABLE_ECOBASE && length(predator_fg_missing) > 0) {
    target_year_ecobase <- if (exists("YEAR_ECOPATH", envir = .GlobalEnv, inherits = FALSE)) round(mean(YEAR_ECOPATH)) else NULL
    ecobase_diet_rows <- fetch_ecobase_diet_for_predator_fgs(predator_fg_missing, out_dir = csv_out_dir, target_year = target_year_ecobase)
    if (nrow(ecobase_diet_rows) > 0) {
      fg_diet <- rbind(fg_diet, ecobase_diet_rows[, .(predator_fg, prey_fg, weight)], fill = TRUE)
    }
  } else if (!DIET_FALLBACK_ENABLE_ECOBASE) {
    message("[04_diets.R] DIET_FALLBACK_ENABLE_ECOBASE is FALSE - skipping the EcoBase diet-matrix fallback tier.")
  }
  predator_fg_still_missing <- setdiff(predator_fg_missing, unique(ecobase_diet_rows$predator_fg))
  
  ## --- Provenance ("diet_ref") per predator FG, and the still-missing -
  ## REVIEW list (per the task: proceed with a clearly flagged partial
  ## matrix rather than hard-stopping, UNLESS truly nothing covers a
  ## predator FG anywhere - that case is written out below for a human
  ## to fix, not silently guessed at). This mirrors the existing
  ## predator_references mechanism (build_species_diet()'s own STEP 5
  ## comment) but rolled up to FG level and across all three tiers.
  species_source_by_fg <- if (nrow(predator_references) > 0) {
    merge(species_to_fg[, .(species, fg_name)], predator_references, by.x = "species", by.y = "predator_name")
  } else data.table(species = character(), fg_name = character(), references = character(), source = character())
  fg_source_tags <- if (nrow(species_source_by_fg) > 0) {
    species_source_by_fg[, .(diet_ref = paste(sort(unique(unlist(strsplit(references, "; ")))), collapse = "; "),
                             diet_source = paste(sort(unique(source)), collapse = "+")), by = fg_name]
  } else data.table(fg_name = character(), diet_ref = character(), diet_source = character())
  if (nrow(ecobase_diet_rows) > 0) {
    ecobase_tags <- unique(ecobase_diet_rows[, .(fg_name = predator_fg, diet_ref = diet_ref, diet_source = "ecobase_model")])
    fg_source_tags <- rbind(fg_source_tags, ecobase_tags, fill = TRUE)
  }
  diet_provenance_by_fg <- data.table(FG_name = predator_fg_names_all)
  diet_provenance_by_fg <- merge(diet_provenance_by_fg, fg_source_tags, by.x = "FG_name", by.y = "fg_name", all.x = TRUE)
  diet_provenance_by_fg[is.na(diet_source), diet_source := "still_missing"]
  fwrite(diet_provenance_by_fg, file.path(csv_out_dir, "diet_provenance_by_predator_fg.csv"))
  message("[04_diets.R] Saved diet_provenance_by_predator_fg.csv - source breakdown: ",
          paste(capture.output(print(diet_provenance_by_fg[, .N, by = diet_source])), collapse = " | "))
  
  if (length(predator_fg_still_missing) > 0) {
    still_missing_dt <- data.table(
      predator_fg = predator_fg_still_missing,
      example_species = vapply(predator_fg_still_missing, function(fg) {
        sp <- species_to_fg_reference_only[fg_name == fg]$species
        paste(head(sp, 5), collapse = "; ")
      }, character(1))
    )
    fwrite(still_missing_dt, DIET_STILL_MISSING_CSV_PATH)
    message("[04_diets.R] WARNING - ", nrow(still_missing_dt), " predator FG(s) have NO diet source at all ",
            "(metaweb, FishBase/SeaLifeBase, AND EcoBase all came up empty) - written to ",
            DIET_STILL_MISSING_CSV_PATH, " for manual review/hand entry. Proceeding with a PARTIAL diet matrix ",
            "(these FG(s) will be all-zero/absent in Ecopath_diet): ", paste(predator_fg_still_missing, collapse = ", "))
  } else if (file.exists(DIET_STILL_MISSING_CSV_PATH)) {
    file.remove(DIET_STILL_MISSING_CSV_PATH)   # clean up a stale review file from an earlier, less-complete run
  }
  
  ## diet_references_by_fg.csv (added 2026-09-17; 2026-09-24: now also
  ## folds in the FishBase/SeaLifeBase + EcoBase fallback tiers'
  ## citations, not just the metaweb's) - rolls predator_references up
  ## to per-FG, via the same species_to_fg mapping used for the diet
  ## matrix itself. Read back by build_fg_references_sheet()
  ## (lib_survey_fg_density_functions.R) to populate the final
  ## workbook's FG_References sheet's diet_ref column. Joined against
  ## FG_lookup.csv (01_biomass.R's own output, read from BIOMASS_CSV_DIR)
  ## to resolve FG_num, matching by FG_name text - same join key
  ## build_biomass_in_fg() above already relies on.
  fg_lookup_for_refs <- read_full_fg_reference(workbook_path, csv_dir = BIOMASS_CSV_DIR)
  if (!is.null(fg_lookup_for_refs)) {
    diet_refs_by_fg <- merge(fg_lookup_for_refs, fg_source_tags, by.x = "FG_name", by.y = "fg_name", all.x = TRUE)
    diet_refs_by_fg <- diet_refs_by_fg[, .(FG_num, FG_name, references = diet_ref)]
    setorder(diet_refs_by_fg, FG_num)
    fwrite(diet_refs_by_fg, file.path(csv_out_dir, "diet_references_by_fg.csv"))
    message("[04_diets.R] Saved diet_references_by_fg.csv (", diet_refs_by_fg[!is.na(references), .N],
            " of ", nrow(diet_refs_by_fg), " FG(s) have at least one diet-study citation, across all three tiers).")
  } else {
    message("[04_diets.R] No FG_lookup.csv found in ", BIOMASS_CSV_DIR, " (run 01_biomass.R first) -",
            " diet_references_by_fg.csv not written this run; the FG_References sheet's diet_ref column",
            " will be blank until it is.")
  }
  
  export_ewe_matrix_csv(fg_diet, group_table, out_path)
  ecopath_diet_sheet <- build_ecopath_diet_sheet(fg_diet, group_table)
  upsert_workbook_sheets(list(Ecopath_diet = ecopath_diet_sheet), workbook_path)
  message("[04_diets.R] Wrote Ecopath_diet sheet to ", workbook_path, ".")
  
  ## --- Final workbook trim (2026-09-17 update, revised) --------------
  ## 04_diets.R is the LAST script in the documented run order (01 -> 02
  ## -> 03 -> 04), so this is where the workbook gets reduced to EXACTLY
  ## the 10 final target sheets: "info" (run metadata), "FG_spp" (FG x
  ## species x taxonomy list, written by 01_biomass.R), "FG_References"
  ## (built just above by build_fg_references_sheet()), plus the seven
  ## Ecopath_*/Ecosim_ts summary sheets. Every native/intermediate table
  ## the individual scripts wrote along the way (Ecopath, Catches_Ecopath,
  ## PB_QB, Ecosim, Catches_Ecosim, Fishing_Effort_by_Fleet, FG_spp_Ecopath,
  ## FG_spp_Ecosim, FG_lookup, traits_ewe, fg_traits_weighted, PB_QB_spp,
  ## References (species-parameter-level, 03_pbqb-traits.R's own CSV -
  ## not the same table as the FG_References workbook sheet above),
  ## Ecobase, Catches_Discards_FG_ts, diet_references_by_fg, ...) lives
  ## only as CSV now (never a workbook sheet), so there's nothing left
  ## to drop here except whatever leftovers an older run of this
  ## workbook might still be carrying - trim_workbook_to_final_sheets()
  ## (drop_extras = TRUE) handles that regardless.
  info_sheet <- build_info_sheet()
  upsert_workbook_sheets(list(info = info_sheet), workbook_path)
  
  ## References sheet (added 2026-09-17): one row per FG, consolidating
  ## which data source/citation fed each block - see build_fg_references_
  ## sheet()'s own header comment (lib_survey_fg_density_functions.R) for
  ## exactly what each column is read from. Run here (04_diets.R, always
  ## last) so it picks up whichever of 01/02/03's outputs exist by now,
  ## same "safe to run any time, skips what's not ready yet" convention
  ## as finalize_ecopath_ecosim_summary_sheets().
  build_fg_references_sheet(workbook_path,   # writes the "FG_References" sheet
                            biomass_csv_dir   = BIOMASS_CSV_DIR,
                            fisheries_csv_dir = file.path(out_dir, "fisheries"),
                            pbqb_csv_dir      = file.path(out_dir, "pbqb-traits"),
                            diet_csv_dir      = csv_out_dir)
  
  trim_workbook_to_final_sheets(workbook_path)
  message("[04_diets.R] Final workbook trimmed to the 9 final target sheets (whichever exist so far).")
  
  fg_diet[]
}

diet_result <- run_pipeline()
message("[04_diets.R] Done - ", nrow(diet_result), " predator-FG x prey-FG diet fraction(s) computed.")