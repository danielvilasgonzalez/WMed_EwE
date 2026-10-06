## --- Reset any logging left over from a previous run in this R session -
## Each script replaces message() with a version that also writes to its
## log file. If R restores an old workspace (.RData) or a previous run
## stopped early, that replacement survives pointing at a CLOSED log
## connection, and the very first message() fails with
## "sink(.run_log_con, split = TRUE): invalid connection". Remove it and
## close any open sinks before anything else runs.
if (exists("message", envir = .GlobalEnv, inherits = FALSE)) rm("message", envir = .GlobalEnv)
while (sink.number() > 0) sink()
## -----------------------------------------------------------------------

## =================================================================
## PIPELINE STEP 5 - run AFTER 01_biomass.R /
## 02_fisheries.R / 03_pbqb-traits.R / 04_diets.R have written (or
## re-written) ecopath_ecosim_inputs.xlsx.
##
## Per "can you work on the validation code, including some
## validation process ... like the comparison with the input values
## from the old west med [workbook] ... in sheet estimates, and other
## validation in the ewe manual and this points i highlighted."
##
## Two jobs:
##   STEP A - compare this pipeline's new Ecopath_B/Ecopath_L/
##            Ecopath_Di/Ecopath_PBQB values against the OLD West Med
##            model's own "estimates" sheet, FG by FG, and flag large
##            deviations - NOT a pass/fail gate, a REVIEW prompt (a
##            real difference can be a real improvement, e.g. the
##            literature-sourced biomass fixes; the point is
##            to surface every large difference so a human decides
##            which number is right).
##   STEP B - consolidate every plausibility/validation REVIEW csv this
##            pipeline already writes (PB/QB/GE bounds, diet-matrix
##            column-sum/cannibalism, EcoBase-fallback rejections, the
##            fleet-coverage check, temporal-mismatch flags, etc. - see
##            pipeline_code_review_and_validation_findings.md and
##            the EwE User Guide compliance review (project documentation) for the
##            full list these come from) into ONE summary table, since
##            today they only exist as separate files scattered across
##            each script's own output subfolder with nothing tying
##            them together.
##
## This script does NOT re-implement checks that already live inside
## 01_biomass.R/02_fisheries.R/03_pbqb-traits.R/04_diets.R (PB/QB/GE
## plausibility, diet-sum, EcoBase-fallback plausibility, group
## ordering, cannibalism) - it only READS what they already wrote.
## Still-open EwE User Guide gaps NOT covered anywhere in this pipeline
## yet (see the EwE User Guide compliance review (project documentation)): discard
## mortality rate (no column for it anywhere in the 9-sheet workbook
## contract - a scope decision, not a bug, left as a project decision
## whether it belongs in this pipeline's output at all) and formal
## multi-stanza input blocks (only a juvenile/adult biomass-split
## heuristic exists, not a real EwE stanza parameter table).
## =================================================================

## STEP C (further below) builds an actual plot for every check this
## script can interpret, plus the PB/QB/F/method-comparison plots
## (fg_pb_qb_scatter.png, F_by_fg.png, PQ_ratio_by_fg.png, the fish PB/QB
## method-comparison plots, FG_PB_QB_comparison*.png) built directly from
## 03_pbqb-traits.R's own CSVs, and the standalone PNGs 04_diets.R and
## 01_biomass.R produce (diet_matrix_heatmap.png, biomass_by_fg_source.png)
## - compiling all of it into ONE multi-page PDF, `validation_plots_ALL.pdf`,
## so there's a single file to look through instead of hunting across
## every script's own plot folder. `validation_summary_ALL.csv` (STEP B)
## is kept alongside it as a plain-text index of the same checks, for
## grepping/filtering rather than paging through.

## =================================================================
## NOTATION, EQUATIONS AND DATA SOURCES (05_validation.R)
## -----------------------------------------------------------------
## Reference model: Western Mediterranean 1995 Ecopath model
## (Coll & Steenbeek; WMed_EwE.xlsx "estimates" sheet, model area
## A_old = 846,002 km2). New model area A_new = sum of strata_area_by_
## area.csv (MEDITS 10-800 m strata of the model GSAs).
##  ratio = X_new / X_old (same units, t/km2 of each model's own area)
##  X_old on new area = X_old x A_old / A_new (same total tonnes)
##  ratio_same_total = X_new / (X_old x A_old / A_new)
## Flags: ratio outside [VALIDATION_RATIO_LOW, VALIDATION_RATIO_HIGH],
## values appearing/disappearing, P/Q outside 0.05-0.3 (Christensen,
## Walters & Pauly 2005, EwE User Guide), diet columns not summing to 1,
## cannibalism > 0.1 (same guide).
## Old -> new FG crosswalk (old_to_new_fg_crosswalk.csv): many old -> one
## new summed (P/B, Q/B biomass-weighted); one old -> several new
## compared against the sum of the new FGs.
## =================================================================

pkgs <- c("data.table", "openxlsx", "ggplot2", "png", "patchwork")
new_pkgs <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new_pkgs) > 0) install.packages(new_pkgs)
invisible(lapply(pkgs, library, character.only = TRUE))

## =================================================================
## STEP 1: Configuration - same out_dir/pcloud_dir/git_dir convention
## as every other script in this pipeline (pre-set by a driver script,
## hardcoded per-machine defaults, or an interactive RStudio picker).
## =================================================================
if (exists("out_dir", envir = .GlobalEnv, inherits = FALSE) &&
    exists("pcloud_dir", envir = .GlobalEnv, inherits = FALSE) &&
    exists("git_dir", envir = .GlobalEnv, inherits = FALSE)) {
  message("[05_validation.R] Using pre-set out_dir/pcloud_dir/git_dir from calling environment:\n  out_dir  = ", out_dir, "\n  pcloud_dir = ", pcloud_dir, "\n  git_dir  = ", git_dir)
  if (!dir.exists(out_dir)) {
    stop("[05_validation.R] out_dir was pre-set by the calling script but doesn't exist on this machine: \"",
         out_dir, "\". Fix it in the driver script (e.g. run_pipeline_demo.R) before sourcing this file.")
  }
} else if (tolower(Sys.info()[["user"]]) == "daniel" && .Platform$OS.type == "unix") {
  out_dir <- "/Users/daniel/Work/iMARES/WMed EwE Model/output/"
  pcloud_dir   <- "/Users/daniel/pCloud Drive/EwE Western Med 2026/"
  git_dir <-"/Users/daniel/Documents/GitHub/WMed_EwE/"
} else {
  if (!requireNamespace("rstudioapi", quietly = TRUE) || !rstudioapi::isAvailable()) {
    stop("This script requires RStudio. Please select the output directory manually.")
  }
  rstudioapi::showQuestion(title = "Select Output Directory",
                           message = "Please select the directory where output files and intermediate results are saved (same folder 01_biomass.R etc. wrote to).")
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

## --- Run log (plain text, for sharing/debugging) ------------------------
.run_log_path <- file.path(out_dir, paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_05_validation_log.txt"))
.run_log_con  <- file(.run_log_path, open = "wt")
sink(.run_log_con, split = TRUE)  # stdout (cat/print): teed to console + file
## Deliberately NOT sinking the message/stderr stream: sink(type = "message")
## has previously been seen to silently swallow ALL console output, including
## real errors, if anything goes wrong with the redirect - exactly the
## "script just stops, no warning" failure mode. Mirror message() into the
## log file by wrapping the function itself instead, leaving real
## message()/stop() error visibility completely untouched.
.orig_message <- base::message
assign("message", function(..., domain = NULL, appendLF = TRUE) {
  ## Pop the output sink before writing directly to its own connection,
  ## then restore it - writing to a connection that's an ACTIVE split
  ## sink target echoes that write back to the real console too, which
  ## would double-print every message() call there (confirmed by testing).
  sink()
  try(cat(paste0(..., collapse = ""), if (appendLF) "\n" else "",
          sep = "", file = .run_log_con), silent = TRUE)
  try(sink(.run_log_con, split = TRUE), silent = TRUE)  # never let a closed log connection break message()
  .orig_message(..., domain = domain, appendLF = appendLF)
}, envir = .GlobalEnv)
message("[Log] This run's console output is also being written to: ", .run_log_path)

if (!exists("ECOPATH_WORKBOOK_PATH", envir = .GlobalEnv, inherits = FALSE)) ECOPATH_WORKBOOK_PATH <- file.path(out_dir, "ecopath_ecosim_inputs.xlsx")
## Always reassigned (never inherited from 01-04 in the same R session -
## inheriting sent validation output into output/pbqb-traits).
csv_out_dir <- file.path(out_dir, "validation")
if (!dir.exists(csv_out_dir)) dir.create(csv_out_dir, recursive = TRUE)
## Matches the folder convention in output_subfolder_refactor_notes.md -
## every validation-type plot for reviewing Ecopath inputs, including
## the PB/QB/F/method-comparison plots this script itself builds
## further below, lands in one place: out_dir/plots/validation.
plot_dir <- file.path(out_dir, "plots", "validation")
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

## The OLD West Med model workbook, sheet "estimates" - the comparison
## baseline. Per project decision ("it should come from the pclouddir
## to allow other to run it"), this
## is derived from pcloud_dir - the same shared pCloud folder every
## other script in this pipeline already resolves per-machine - rather
## than a second, separate hardcoded absolute machine path. Falls back
## to an RStudio file picker, same "ask rather than crash" convention
## used everywhere else here, if it isn't at the expected place within
## pcloud_dir.
if (!exists("OLD_WMED_WORKBOOK_PATH", envir = .GlobalEnv, inherits = FALSE)) {
  pcloud_default <- file.path(pcloud_dir, "data", "WMed_EwE.xlsx")
  if (file.exists(pcloud_default)) {
    OLD_WMED_WORKBOOK_PATH <- pcloud_default
  } else if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
    rstudioapi::showQuestion(title = "Select the OLD West Med workbook",
                             message = "Please select the old WMed_EwE.xlsx file (the one with the 'estimates' sheet) to validate against.")
    OLD_WMED_WORKBOOK_PATH <- rstudioapi::selectFile(caption = "Select WMed_EwE.xlsx", filter = "Excel Files (*.xlsx)")
    if (is.null(OLD_WMED_WORKBOOK_PATH) || OLD_WMED_WORKBOOK_PATH == "") stop("No old-workbook file selected.")
  } else {
    stop("[05_validation.R] OLD_WMED_WORKBOOK_PATH not set, the expected path within pcloud_dir (",
         pcloud_default, ") doesn't exist, and RStudio isn't available for an interactive picker.",
         " Set OLD_WMED_WORKBOOK_PATH explicitly before sourcing this file.")
  }
}
if (!exists("OLD_WMED_ESTIMATES_SHEET", envir = .GlobalEnv, inherits = FALSE)) OLD_WMED_ESTIMATES_SHEET <- "estimates"

## Review thresholds - adjustable constants, not tuned against a real
## distribution. A ratio this far from 1 (new/old) gets
## flagged for a human look; it is NOT evidence the new value is wrong
## - many of the pipeline's fixes (real literature citations replacing
## a generic EcoBase-fallback guess, the FG_spp architecture switch,
## the temporal-baseline corrections) are EXPECTED to move some FGs
## a long way from the old workbook's own figure.
if (!exists("VALIDATION_RATIO_HIGH", envir = .GlobalEnv, inherits = FALSE)) VALIDATION_RATIO_HIGH <- 2
if (!exists("VALIDATION_RATIO_LOW",  envir = .GlobalEnv, inherits = FALSE)) VALIDATION_RATIO_LOW  <- 0.5
## Fuzzy FG-name matching tolerance (base R agrep's max.distance, as a
## fraction of pattern length) - used only for names that don't match
## exactly after normalization, e.g. "Sardine" (old) vs "European
## sardine" (new). Kept deliberately tight so it doesn't produce a
## false match between two genuinely different FGs.
if (!exists("VALIDATION_FUZZY_MAX_DIST", envir = .GlobalEnv, inherits = FALSE)) VALIDATION_FUZZY_MAX_DIST <- 0.15

## Same lightweight column-alias resolver used throughout this codebase
## (01_biomass.R/02_fisheries.R each carry their own copy under the
## same name) - kept local here too rather than sourcing another
## script just for this one helper.
## Handles R's make.names() mangling, not just spaces: make.names()
## mangles EVERY character it doesn't allow in a name into a dot -
## e.g. make.names("Biomass (t/km^2)") -> "Biomass..t.km.2.". The
## biomass/PB/QB/landings/discards aliases all contain "(", ")", "/",
## "^", so this strips down to alphanumeric characters only (letters +
## digits, lowercased) on BOTH sides before comparing, matching
## regardless of whether the sheet was read with its original
## punctuated headers or make.names()-mangled ones.
norm_colname <- function(x) tolower(gsub("[^[:alnum:]]+", "", x))
resolve_col <- function(dt_names, aliases) {
  hit <- aliases[norm_colname(aliases) %in% norm_colname(dt_names)]
  if (length(hit) == 0) return(NA_character_)
  dt_names[norm_colname(dt_names) == norm_colname(hit[1])][1]
}

normalize_name <- function(x) tolower(trimws(gsub("[[:space:]]+", " ", gsub("[._-]+", " ", x))))

## VALIDATION_REFERENCE_DIR: hand-typed reference/lookup tables for
## this script, externalized to CSV so they're editable without
## touching code - same convention as read_medits_reference()/
## MEDITS_REFERENCE_DIR in 01_biomass.R, read_fisheries_reference()/
## FISHERIES_REFERENCE_DIR in 02_fisheries.R, and read_pbqb_reference()/
## PBQB_REFERENCE_DIR in 03_pbqb-traits.R.
VALIDATION_REFERENCE_DIR <- file.path(pcloud_dir, "data/Complementary data/validation_reference_tables")
read_validation_reference <- function(filename, required_cols = NULL) {
  path <- file.path(VALIDATION_REFERENCE_DIR, filename)
  if (!file.exists(path)) {
    stop("[read_validation_reference] Reference table not found: \"", path, "\".")
  }
  dt <- fread(path, encoding = "UTF-8")
  if (!is.null(required_cols) && !all(required_cols %in% names(dt))) {
    stop("[read_validation_reference] \"", filename, "\" is missing required column(s): ",
         paste(setdiff(required_cols, names(dt)), collapse = ", "))
  }
  dt
}

message("\n[05_validation.R] STEP A - comparing new pipeline output against the old West Med workbook's '",
        OLD_WMED_ESTIMATES_SHEET, "' sheet (", OLD_WMED_WORKBOOK_PATH, ").")

## =================================================================
## STEP A1: read the new pipeline's own Ecopath_B / Ecopath_L /
## Ecopath_Di / Ecopath_PBQB sheets, straight from the shared workbook
## - not from CSVs, so this validates exactly what a real EwE import
## would see.
## =================================================================
if (!file.exists(ECOPATH_WORKBOOK_PATH)) {
  stop("[05_validation.R] ", ECOPATH_WORKBOOK_PATH, " doesn't exist yet - run 01_biomass.R (and ideally",
       " 02_fisheries.R/03_pbqb-traits.R too) first.")
}
wb_sheets <- openxlsx::getSheetNames(ECOPATH_WORKBOOK_PATH)

read_sheet_safe <- function(sheet_name, needed_cols) {
  if (!sheet_name %in% wb_sheets) {
    message("[05_validation.R] '", sheet_name, "' not found in the workbook yet - skipping that metric",
            " (run the script that writes it first).")
    return(NULL)
  }
  dt <- as.data.table(openxlsx::read.xlsx(ECOPATH_WORKBOOK_PATH, sheet = sheet_name))
  missing <- setdiff(needed_cols, names(dt))
  if (length(missing) > 0) {
    message("[05_validation.R] '", sheet_name, "' is missing expected column(s): ", paste(missing, collapse = ", "),
            " (found: ", paste(names(dt), collapse = ", "), ") - skipping that metric.")
    return(NULL)
  }
  dt[, FG_num := as.integer(FG_num)]
  dt
}

new_B    <- read_sheet_safe("Ecopath_B",    c("FG_num", "FG_name", "Biomass"))
## Ecopath_L/Ecopath_Di are wide: one column per fleet (t/km2/year) plus a
## trailing text 'Reference' column. The FG total is the row sum across
## every fleet column.
read_fleet_wide_total <- function(sheet_name, total_col) {
  dt <- read_sheet_safe(sheet_name, c("FG_num", "FG_name"))
  if (is.null(dt)) return(NULL)
  fleet_cols <- setdiff(names(dt), c("FG_num", "FG_name", "Reference"))
  if (length(fleet_cols) == 0) {
    message("[05_validation.R] '", sheet_name, "' has no fleet columns - skipping that metric.")
    return(NULL)
  }
  dt[, (fleet_cols) := lapply(.SD, function(v) suppressWarnings(as.numeric(v))), .SDcols = fleet_cols]
  dt[, (total_col) := rowSums(.SD, na.rm = TRUE), .SDcols = fleet_cols]
  message("[05_validation.R] '", sheet_name, "': FG total = sum across ", length(fleet_cols), " fleet column(s).")
  dt[, c("FG_num", "FG_name", total_col), with = FALSE]
}
new_L    <- read_fleet_wide_total("Ecopath_L",  "Landings_t_km2_broad")
new_Di   <- read_fleet_wide_total("Ecopath_Di", "Discard_t_km2_broad")
new_PBQB <- read_sheet_safe("Ecopath_PBQB", c("FG_num", "FG_name", "PB_FG", "QB_FG"))

new_metrics <- list()
if (!is.null(new_B))    new_metrics[["Biomass"]]  <- new_B[,    .(FG_num, FG_name, new_value = as.numeric(Biomass))]
if (!is.null(new_L))    new_metrics[["Landings"]] <- new_L[,    .(FG_num, FG_name, new_value = as.numeric(Landings_t_km2_broad))]
if (!is.null(new_Di))   new_metrics[["Discards"]] <- new_Di[,   .(FG_num, FG_name, new_value = as.numeric(Discard_t_km2_broad))]
if (!is.null(new_PBQB)) {
  new_metrics[["PB"]] <- new_PBQB[, .(FG_num, FG_name, new_value = as.numeric(PB_FG))]
  new_metrics[["QB"]] <- new_PBQB[, .(FG_num, FG_name, new_value = as.numeric(QB_FG))]
}

if (length(new_metrics) == 0) {
  stop("[05_validation.R] None of Ecopath_B/Ecopath_L/Ecopath_Di/Ecopath_PBQB have the expected columns yet -",
       " nothing to validate. Run the upstream scripts first.")
}

## =================================================================
## STEP A2: read the OLD workbook's 'estimates' sheet, with flexible
## column-name resolution - this file's
## actual columns, so every alias list below is a best guess at common
## Ecopath-estimates-sheet naming, not a confirmed match. Anything not
## resolved is reported (not guessed).
## =================================================================
if (!file.exists(OLD_WMED_WORKBOOK_PATH)) {
  stop("[05_validation.R] OLD_WMED_WORKBOOK_PATH ('", OLD_WMED_WORKBOOK_PATH, "') doesn't exist - check the",
       " path (it needs to be reachable from wherever this script actually runs).")
}
old_sheets <- openxlsx::getSheetNames(OLD_WMED_WORKBOOK_PATH)
if (!OLD_WMED_ESTIMATES_SHEET %in% old_sheets) {
  stop("[05_validation.R] Sheet '", OLD_WMED_ESTIMATES_SHEET, "' not found in ", OLD_WMED_WORKBOOK_PATH,
       " - sheets actually present: ", paste(old_sheets, collapse = ", "),
       ". Set OLD_WMED_ESTIMATES_SHEET to the right one and re-run.")
}
old_raw <- as.data.table(openxlsx::read.xlsx(OLD_WMED_WORKBOOK_PATH, sheet = OLD_WMED_ESTIMATES_SHEET))
message("[05_validation.R] Old workbook '", OLD_WMED_ESTIMATES_SHEET, "' sheet columns found: ",
        paste(names(old_raw), collapse = ", "))

old_col_aliases <- list(
  group_num = c("Group_num", "FG_num", "Group", "No", "#", "Group number"),
  group_name = c("Group name", "Group", "FG_name", "Name", "Functional group", "Functional Group"),
  ## "Biomass (t/km^2)" (the whole-model-domain density, what Ecopath_B
  ## is) is listed BEFORE "Biomass in habitat area (t/km^2)" (biomass
  ## density only within the FG's own habitat patch - a DIFFERENT,
  ## larger number whenever Hab area proportion < 1), since the WMed_
  ## EwE.xlsx "estimates" sheet has BOTH columns and domain-wide is the
  ## correct comparator against Ecopath_B.
  biomass   = c("Biomass (t/km^2)", "Biomass (t/km2)", "Biomass", "B", "Biomass in habitat area (t/km^2)"),
  pb        = c("Production / biomass (/year)", "P/B (/year)", "PB", "P/B", "Production/biomass"),
  qb        = c("Consumption / biomass (/year)", "Q/B (/year)", "QB", "Q/B", "Consumption/biomass"),
  landings  = c("Landings", "Landings (t/km2/year)", "Catch", "Catch (t/km2/year)", "Fishery Catch"),
  discards  = c("Discards", "Discards (t/km2/year)", "Discard")
)
col_num  <- resolve_col(names(old_raw), old_col_aliases$group_num)
col_name <- resolve_col(names(old_raw), old_col_aliases$group_name)
if (is.na(col_name)) {
  ## Include the actual column names read from the file directly in
  ## the error itself (not just what was tried), since a real failure
  ## here can be an invisible character (e.g. a non-breaking space) in
  ## the header that looks identical to "Group name" on the console.
  stop("[05_validation.R] Could not find a group-name column in the old '", OLD_WMED_ESTIMATES_SHEET,
       "' sheet (tried: ", paste(old_col_aliases$group_name, collapse = ", "), "). Columns actually found in",
       " the file: ", paste(names(old_raw), collapse = " | "), ". If one of those looks like it should have",
       " matched (e.g. \"Group name\"), it likely contains an invisible character (non-breaking space, etc.)",
       " - run dput(names(old_raw)) on the old sheet to see it, or just add that exact string to",
       " old_col_aliases$group_name above. Cannot match FGs without a name column.")
}

## The real WMed_EwE.xlsx "estimates" sheet mixes true numeric cells
## with text cells typed using a European comma decimal (e.g. QB
## "23,2"); plain as.numeric() returns NA for those. This converts ","
## to "." before parsing - a safe, unambiguous fix for text cells. It
## does NOT fix a separate, more serious problem in the same sheet
## (see the sanity check right after old_dt below): many already-
## numeric cells appear to have LOST their decimal point entirely
## (e.g. a biomass of 322207 where the true value is likely
## 0.322207) - there's no "," left to convert and no safe way to know
## how many places to shift it back without guessing.
parse_num_eu <- function(x) suppressWarnings(as.numeric(gsub(",", ".", trimws(as.character(x)), fixed = TRUE)))

old_dt <- data.table(
  Old_FG_num  = if (!is.na(col_num)) suppressWarnings(as.integer(old_raw[[col_num]])) else NA_integer_,
  Old_FG_name = as.character(old_raw[[col_name]])
)
for (metric in c("biomass", "pb", "qb", "landings", "discards")) {
  col <- resolve_col(names(old_raw), old_col_aliases[[metric]])
  old_dt[[metric]] <- if (!is.na(col)) parse_num_eu(old_raw[[col]]) else NA_real_
  if (is.na(col)) message("[05_validation.R] Old sheet has no recognizable '", metric, "' column - that metric",
                          " will be skipped in the comparison (tried: ", paste(old_col_aliases[[metric]], collapse = ", "), ").")
}
old_dt <- old_dt[!is.na(Old_FG_name) & trimws(Old_FG_name) != ""]
old_dt[, norm_name := normalize_name(Old_FG_name)]

## Manual correction: the "lost decimal point"
## corruption described below hit these 7 marine-mammal/seabird rows
## particularly badly (e.g. Bottlenose dolphins' biomass read as the
## whole number 322207 instead of 0.003222). The TRUE values were
## supplied directly from the original model records - not reverse-engineered from
## the corrupted cells - so they override whatever old_dt picked up
## from the raw sheet for just these 7 rows. QB is NA for "Endangered
## and pelagic seabirds" because no value was supplied for it.
old_estimates_manual_overrides <- data.table(
  Old_FG_name = c("Bottlenose dolphins", "Striped dolphins", "Short-beaked common dolphin",
                  "Fin whale", "Deep sea-cetacean feeders", "Monk seals",
                  "Endangered and pelagic seabirds"),
  biomass_override = c(0.003222, 0.007, 0.001174, 0.008638, 0.007105, 0.000021, 0.000054),
  pb_override       = c(0.067, 0.04, 0.086, 0.034, 0.069, 0.082, 0.395),
  qb_override       = c(23.2, 32.5, 31.2, 7.2, 16.9, 27.3, NA_real_)
)
old_estimates_manual_overrides[, norm_name := normalize_name(Old_FG_name)]
n_overridden <- sum(old_dt$norm_name %in% old_estimates_manual_overrides$norm_name)
if (n_overridden > 0) {
  old_dt <- merge(old_dt, old_estimates_manual_overrides[, .(norm_name, biomass_override, pb_override, qb_override)],
                  by = "norm_name", all.x = TRUE)
  old_dt[!is.na(biomass_override), biomass := biomass_override]
  old_dt[!is.na(pb_override), pb := pb_override]
  old_dt[!is.na(qb_override), qb := qb_override]
  old_dt[, c("biomass_override", "pb_override", "qb_override") := NULL]
  message("[05_validation.R] Applied manually-corrected biomass/PB/QB for ", n_overridden,
          " marine-mammal/seabird row(s) in the old estimates sheet (overriding the sheet's own, likely",
          " decimal-corrupted, values) - see old_estimates_manual_overrides above for the exact figures used.")
} else {
  message("[05_validation.R] Note: none of the 7 manually-corrected marine-mammal/seabird group names",
          " matched a row in this run's old estimates sheet - check old_estimates_manual_overrides' names",
          " against Old_FG_name above if that's unexpected.")
}

## Old -> new FG crosswalk: groups the 2026 FG list merged
## or renamed (sardine/anchovy juv+adult, seagrasses, algae, corals,
## bivalves+gastropods, dolphins, seabirds...). Listed in
## validation_reference_tables/old_to_new_fg_crosswalk.csv; the old
## rows are combined before matching - biomass/landings/discards summed
## (all t/km2 over the same old model area), PB/QB biomass-weighted.
## The correspondence is an ASSUMPTION (FG definitions changed) - edit
## the CSV to change it.
crosswalk_path <- file.path(VALIDATION_REFERENCE_DIR, "old_to_new_fg_crosswalk.csv")
## Two directions, both from the crosswalk CSV:
##  - many old -> one new: old rows sharing a target are combined
##    (B, landings, discards summed; P/B, Q/B B-weighted means), also
##    when an old group already carries the new FG's exact name;
##  - one old -> several new (New_FG_name "A + B", e.g. European hake ->
##    juv. + adult): a combined NEW row is built per metric (B, L, Di
##    summed; P/B, Q/B weighted by new B), FG_num = -k (synthetic).
.combine_old <- function(d) d[, .(Old_FG_num = if (.N == 1) Old_FG_num[1] else NA_integer_,
                                  Old_FG_name = paste(Old_FG_name, collapse = " + "),
                                  biomass = if (all(is.na(biomass))) NA_real_ else sum(biomass, na.rm = TRUE),
                                  pb = if (all(is.na(pb) | is.na(biomass))) NA_real_ else weighted.mean(pb, biomass, na.rm = TRUE),
                                  qb = if (all(is.na(qb) | is.na(biomass))) NA_real_ else weighted.mean(qb, biomass, na.rm = TRUE),
                                  landings = if (all(is.na(landings))) NA_real_ else sum(landings, na.rm = TRUE),
                                  discards = if (all(is.na(discards))) NA_real_ else sum(discards, na.rm = TRUE)),
                              by = norm_name]
if (file.exists(crosswalk_path)) {
  xw <- unique(fread(crosswalk_path)[, .(norm_name = normalize_name(Old_FG_name), New_FG_name = trimws(New_FG_name))])
  ## one -> several: synthetic combined new FGs
  split_targets <- unique(xw[grepl("+", New_FG_name, fixed = TRUE), New_FG_name])
  for (k in seq_along(split_targets)) {
    parts <- normalize_name(trimws(strsplit(split_targets[k], "+", fixed = TRUE)[[1]]))
    bw <- if (!is.null(new_metrics$Biomass)) new_metrics$Biomass[normalize_name(FG_name) %in% parts, .(FG_name, w = new_value)] else NULL
    for (m in names(new_metrics)) {
      d <- new_metrics[[m]][normalize_name(FG_name) %in% parts]
      if (nrow(d) == 0) next
      v <- if (m %in% c("PB", "QB") && !is.null(bw)) {
        dw <- merge(d, bw, by = "FG_name"); if (nrow(dw) && sum(dw$w, na.rm = TRUE) > 0) weighted.mean(dw$new_value, dw$w, na.rm = TRUE) else mean(d$new_value, na.rm = TRUE)
      } else if (all(is.na(d$new_value))) NA_real_ else sum(d$new_value, na.rm = TRUE)
      new_metrics[[m]] <- rbindlist(list(new_metrics[[m]], data.table(FG_num = -k, FG_name = split_targets[k], new_value = v)), use.names = TRUE)
    }
  }
  hit <- old_dt$norm_name %in% xw$norm_name
  if (any(hit)) {
    old_dt[hit, norm_name := normalize_name(xw$New_FG_name[match(norm_name, xw$norm_name)])]
    dup <- old_dt[, .N, by = norm_name][N > 1, norm_name]
    old_dt <- rbindlist(list(old_dt[!norm_name %in% dup], .combine_old(old_dt[norm_name %in% dup])), use.names = TRUE, fill = TRUE)
    message("[05_validation.R] Old->new FG crosswalk applied to ", sum(hit), " old group(s) (", basename(crosswalk_path),
            "); combined old rows: ", paste(old_dt[grepl(" + ", Old_FG_name, fixed = TRUE), Old_FG_name], collapse = "; "),
            if (length(split_targets)) paste0("; split new FGs compared as sums: ", paste(split_targets, collapse = "; ")) else "", ".")
  }
} else {
  message("[05_validation.R] No ", crosswalk_path, " - old groups split/merged differently from the 2026 FGs stay unmatched.")
}

## Sanity check for the "lost decimal point" problem described
## above - flag it loudly rather than let it silently blow up every ratio
## in STEP A4. Heuristic: a metric column where most of the NUMERIC
## (non-comma-text) values look implausibly large for that metric is very
## likely affected. This does not touch the data, it only warns.
suspect_numeric_corruption <- function(colvals, raw_colname, typical_max) {
  v <- suppressWarnings(as.numeric(colvals))
  v <- v[!is.na(v) & v != 0]
  if (length(v) < 5) return(invisible(NULL))
  frac_too_big <- mean(v > typical_max)
  if (frac_too_big > 0.5) {
    message("[05_validation.R] WARNING: old sheet column '", raw_colname, "' - ", round(100 * frac_too_big),
            "% of its numeric values exceed a plausible upper bound (", typical_max, ") for this metric.",
            " This looks like the known 'missing decimal point' issue in WMed_EwE.xlsx (values typed with a",
            " comma decimal, e.g. 0,322207, that got stored as a whole number, e.g. 322207, somewhere upstream",
            " of this file) rather than a real biology signal. NOT auto-corrected here - the true decimal",
            " position isn't safely inferable per-cell. Recommend re-checking/re-exporting this column at the",
            " source before trusting comparisons against it.")
  }
}
if (!is.na(resolve_col(names(old_raw), old_col_aliases$biomass))) {
  suspect_numeric_corruption(old_raw[[resolve_col(names(old_raw), old_col_aliases$biomass)]],
                             resolve_col(names(old_raw), old_col_aliases$biomass), typical_max = 500)
}
if (!is.na(resolve_col(names(old_raw), old_col_aliases$pb))) {
  suspect_numeric_corruption(old_raw[[resolve_col(names(old_raw), old_col_aliases$pb)]],
                             resolve_col(names(old_raw), old_col_aliases$pb), typical_max = 20)
}

## =================================================================
## STEP A3: match old FG names to new FG_num/FG_name - exact
## normalized match first, then a tight fuzzy match (base R agrep) for
## whatever's left, then report anything still unmatched rather than
## guessing.
## =================================================================
new_fg_lookup <- unique(rbindlist(lapply(new_metrics, function(d) d[, .(FG_num, FG_name)])))
new_fg_lookup <- unique(new_fg_lookup)
new_fg_lookup[, norm_name := normalize_name(FG_name)]

exact_match <- merge(old_dt, new_fg_lookup[, .(FG_num, FG_name, norm_name)], by = "norm_name", all.x = TRUE)
still_unmatched <- exact_match[is.na(FG_num)]
exact_match <- exact_match[!is.na(FG_num)]

if (nrow(still_unmatched) > 0) {
  fuzzy_rows <- lapply(seq_len(nrow(still_unmatched)), function(i) {
    nm <- still_unmatched$norm_name[i]
    hit_idx <- agrep(nm, new_fg_lookup$norm_name, max.distance = VALIDATION_FUZZY_MAX_DIST, ignore.case = TRUE)
    if (length(hit_idx) == 0) return(NULL)
    ## agrep can return more than one candidate - keep only the single
    ## best (shortest edit distance isn't directly exposed by base
    ## agrep, so ties are reported, not silently picked) to avoid a
    ## many-to-one false match.
    if (length(hit_idx) > 1) {
      message("[05_validation.R] '", still_unmatched$Old_FG_name[i], "' fuzzy-matched MORE than one new FG (",
              paste(new_fg_lookup$FG_name[hit_idx], collapse = " / "), ") - left UNMATCHED rather than guessing",
              " which one is right.")
      return(NULL)
    }
    cbind(still_unmatched[i], new_fg_lookup[hit_idx, .(FG_num, FG_name)])
  })
  fuzzy_matched <- rbindlist(fuzzy_rows[!vapply(fuzzy_rows, is.null, logical(1))], fill = TRUE)
  if (nrow(fuzzy_matched) > 0) {
    message("[05_validation.R] ", nrow(fuzzy_matched), " old FG name(s) matched a new FG only via fuzzy matching",
            " (not an exact name match) - double-check these are really the same group:")
    print(fuzzy_matched[, .(Old_FG_name, FG_name)])
  }
  still_unmatched <- still_unmatched[!Old_FG_name %in% fuzzy_matched$Old_FG_name]
  matched_dt <- rbindlist(list(exact_match, fuzzy_matched), fill = TRUE)
} else {
  matched_dt <- exact_match
}

if (nrow(still_unmatched) > 0) {
  fwrite(still_unmatched[, .(Old_FG_name)], file.path(csv_out_dir, "old_workbook_unmatched_fg_names_REVIEW.csv"))
  message("[05_validation.R] ", nrow(still_unmatched), " old FG name(s) could not be matched to any new FG at all",
          " (exact or fuzzy) - see old_workbook_unmatched_fg_names_REVIEW.csv. Common causes: the old model used",
          " a different FG breakdown entirely (e.g. one combined group split into several new ones, or vice",
          " versa), or a genuinely renamed group this fuzzy match is too conservative to catch.")
}

## =================================================================
## STEP A4: build the long comparison table and flag large deviations.
## =================================================================
comparison_rows <- list()
for (metric_name in names(new_metrics)) {
  old_col <- switch(metric_name, Biomass = "biomass", Landings = "landings", Discards = "discards",
                    PB = "pb", QB = "qb")
  if (all(is.na(matched_dt[[old_col]]))) next  # old sheet had no usable column for this metric at all
  new_dt <- new_metrics[[metric_name]]
  cmp <- merge(matched_dt[, .(Old_FG_name, FG_num, FG_name, old_value = get(old_col))],
               new_dt[, .(FG_num, new_value)], by = "FG_num", all.x = TRUE)
  cmp[, metric := metric_name]
  cmp[, ratio := new_value / old_value]
  cmp[, flag := fcase(
    is.na(old_value) & !is.na(new_value), "NEW value where old had none",
    !is.na(old_value) & old_value > 0 & is.na(new_value), "OLD value now MISSING in the new pipeline",
    !is.na(old_value) & old_value == 0 & !is.na(new_value) & new_value > 0, "Old was zero, new is nonzero",
    !is.na(ratio) & is.finite(ratio) & (ratio > VALIDATION_RATIO_HIGH | ratio < VALIDATION_RATIO_LOW), "Large deviation - review",
    default = "Within review range"
  )]
  comparison_rows[[metric_name]] <- cmp
}
comparison_dt <- rbindlist(comparison_rows, fill = TRUE)
setcolorder(comparison_dt, c("FG_num", "FG_name", "Old_FG_name", "metric", "old_value", "new_value", "ratio", "flag"))
setorder(comparison_dt, metric, -ratio, na.last = TRUE)
## Area normalisation. Ecopath B, landings and discards are
## t per km2 of EACH model's own area: the old 1995 model covers
## 846,002 km2 (EcopathModel.Area in WestMed_Ges4Seas.ewemdb, i.e. the
## whole West Med incl. the deep basin and N Africa), the new one only
## the MEDITS 10-800 m strata of the model GSAs (strata_area_by_area.csv).
## Comparing densities directly makes every old value ~5x too small, so
## the old density is also expressed on the NEW area (same total tonnes):
## old_value_on_new_area = old_value x OLD_MODEL_AREA_KM2 / NEW_MODEL_AREA_KM2.
if (!exists("OLD_MODEL_AREA_KM2")) OLD_MODEL_AREA_KM2 <- 846002
if (!exists("NEW_MODEL_AREA_KM2")) {
  sa_path <- file.path(out_dir, "biomass", "strata_area_by_area.csv")
  NEW_MODEL_AREA_KM2 <- if (file.exists(sa_path)) sum(fread(sa_path)$area_km2, na.rm = TRUE) else NA_real_
}
comparison_dt[metric %in% c("Biomass", "Landings", "Discards"),
              `:=`(old_value_on_new_area = old_value * OLD_MODEL_AREA_KM2 / NEW_MODEL_AREA_KM2,
                   old_total_t = old_value * OLD_MODEL_AREA_KM2, new_total_t = new_value * NEW_MODEL_AREA_KM2)]
comparison_dt[, ratio_same_total := new_value / old_value_on_new_area]
message("[05_validation.R] Model areas: old ", format(OLD_MODEL_AREA_KM2, big.mark = ","), " km2, new ",
        format(round(NEW_MODEL_AREA_KM2), big.mark = ","), " km2 - old densities also shown on the new area",
        " (old_value_on_new_area, ratio_same_total = new / old in total tonnes).")
fwrite(comparison_dt, file.path(csv_out_dir, "comparison_with_old_westmed_estimates_REVIEW.csv"))

## Biomass-only view, FG by FG (what to look at first after 01_biomass.R)
bio_cmp <- comparison_dt[metric == "Biomass", .(FG_num, FG_name, Old_FG_name, new_B_t_km2 = new_value,
                                                 old_B_t_km2_old_area = old_value, old_B_t_km2_on_new_area = old_value_on_new_area,
                                                 new_total_t, old_total_t, ratio_same_total)]
setorder(bio_cmp, FG_num)
fwrite(bio_cmp, file.path(csv_out_dir, "biomass_vs_old_model_REVIEW.csv"))
p_bio <- tryCatch({
  d <- melt(bio_cmp[!is.na(FG_num)], id.vars = c("FG_num", "FG_name"),
            measure.vars = c("new_B_t_km2", "old_B_t_km2_on_new_area", "old_B_t_km2_old_area"),
            variable.name = "series", value.name = "B")[is.finite(B) & B > 0]
  d[, series := factor(series, levels = c("new_B_t_km2", "old_B_t_km2_on_new_area", "old_B_t_km2_old_area"),
                       labels = c("New pipeline", "Old model, same total tonnes on new area", "Old model, as published (846,002 km2)"))]
  d[, label := factor(paste0(FG_num, " ", FG_name), levels = rev(unique(paste0(bio_cmp$FG_num, " ", bio_cmp$FG_name))))]
  ggplot(d, aes(x = B, y = label, colour = series, shape = series)) +
    geom_point(size = 2) + scale_x_log10() +
    scale_colour_manual(values = c("firebrick", "grey20", "grey65")) +
    labs(title = "Ecopath B: new pipeline vs old WMed 1995 model",
         subtitle = paste0("t/km2, log scale. New area ", format(round(NEW_MODEL_AREA_KM2), big.mark = ","),
                           " km2; old area 846,002 km2. Compare red with black (same total tonnes)."),
         x = "Biomass (t/km2)", y = NULL, colour = NULL, shape = NULL) +
    theme_minimal(base_size = 8) + theme(legend.position = "bottom")
}, error = function(e) { message("[05_validation.R] biomass comparison plot skipped - ", conditionMessage(e)); NULL })
if (!is.null(p_bio)) {
  ggsave(file.path(plot_dir, "biomass_vs_old_model_by_fg.png"), p_bio, width = 9, height = 13, dpi = 150, bg = "white")
  message("[05_validation.R] Saved: biomass_vs_old_model_by_fg.png and biomass_vs_old_model_REVIEW.csv")
}

n_flagged <- comparison_dt[flag != "Within review range", .N]
message("\n[05_validation.R] comparison_with_old_westmed_estimates_REVIEW.csv written: ", nrow(comparison_dt),
        " FG x metric row(s), ", n_flagged, " flagged (ratio outside ", VALIDATION_RATIO_LOW, "-",
        VALIDATION_RATIO_HIGH, "x, a value that appeared/disappeared, or a zero that became nonzero).",
        " A flag here is a REVIEW PROMPT, not proof the new value is wrong - many of the pipeline's own fixes",
        " (real literature citations, the FG_spp architecture switch, temporal-baseline corrections) are",
        " expected to move some FGs a long way from the old workbook's figure.")

p_comparison <- tryCatch({
  d <- comparison_dt[!is.na(old_value) & !is.na(new_value) & old_value > 0 & new_value > 0]
  if (nrow(d) == 0) stop("no FG x metric row has both a real old and new value to plot")
  ggplot(d, aes(x = old_value, y = new_value, color = flag)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +
    geom_point(alpha = 0.7) +
    scale_x_log10() + scale_y_log10() +
    facet_wrap(~metric, scales = "free") +
    scale_color_manual(values = c("Within review range" = "grey30", "Large deviation - review" = "firebrick",
                                  "Old was zero, new is nonzero" = "darkorange")) +
    labs(title = "New pipeline vs. old West Med 'estimates' sheet",
         subtitle = "Dashed line = perfect agreement (log-log axes) - points far from it are flagged, not necessarily wrong",
         x = "Old workbook value", y = "New pipeline value", color = NULL) +
    theme_minimal(base_size = 9) + theme(legend.position = "bottom")
}, error = function(e) { message("[05_validation.R] comparison plot skipped - ", conditionMessage(e)); NULL })
if (!is.null(p_comparison)) {
  ggsave(file.path(plot_dir, "comparison_with_old_westmed_estimates.png"), p_comparison,
         width = 10, height = 8, dpi = 150, bg = "white", limitsize = FALSE)
  message("[05_validation.R] Saved: comparison_with_old_westmed_estimates.png (in ", plot_dir, ")")
}

## =================================================================
## STEP B: consolidate every plausibility/validation REVIEW csv this
## pipeline's other scripts already write, into one summary table -
## purely a convenience index (reads existing files, writes nothing
## new to any of them). File names/locations below are documented in
## pipeline_code_review_and_validation_findings.md /
## the EwE User Guide compliance review (project documentation). Any file not found
## yet (script hasn't run, or an older pipeline version) is listed as
## "not found" rather than causing an error - this index degrades
## gracefully the same way every other cross-script read in this
## pipeline does.
## =================================================================
known_review_files <- read_validation_reference("validation_known_review_files.csv",
                                                required_cols = c("name", "dir", "file"))
summary_rows <- lapply(seq_len(nrow(known_review_files)), function(i) {
  spec <- known_review_files[i]
  path <- file.path(out_dir, spec$dir, spec$file)
  if (!file.exists(path)) {
    return(data.table(check = spec$name, file = path, status = "not found (script hasn't run, or nothing to flag)", n_rows = NA_integer_))
  }
  n <- tryCatch(nrow(fread(path)), error = function(e) NA_integer_)
  data.table(check = spec$name, file = path,
             status = if (is.na(n)) "found but unreadable" else if (n == 0) "found, 0 rows (clean)" else paste0("found, ", n, " row(s) flagged"),
             n_rows = n)
})
validation_summary <- rbindlist(summary_rows)
fwrite(validation_summary, file.path(csv_out_dir, "validation_summary_ALL.csv"))
message("\n[05_validation.R] STEP B - validation_summary_ALL.csv written (", nrow(validation_summary),
        " known checks indexed). Console summary:")
print(validation_summary[, .(check, status)])

## =================================================================
## STEP C: a compilation of plots, one per validation check - per
## project requirement: "the validation code should be a compilation of
## plots to check these validations." STEP B above already indexes every
## REVIEW csv as a row count, which is fast to scan but doesn't show
## WHERE the problem is or how bad it is - a plot does that at a glance.
## Every check below reads a file STEP B already listed (nothing new is
## computed here), builds the most informative plot that file's own
## columns support, and - together with the standalone validation PNGs
## 01-04_*.R already produce - all of it gets compiled into ONE PDF,
## `validation_plots_ALL.pdf`, so there's a single file to page through
## instead of opening every script's own plot folder in turn. Each
## check degrades independently (tryCatch, same convention as every
## other plot in this pipeline) - one broken/missing check never blocks
## the rest, and only checks that actually produced a plot make it into
## the final PDF.
## =================================================================
validation_plots <- list()

## --- helper: a simple "list of names" page for a check whose CSV has ---
## --- no natural numeric axis to plot (e.g. a bare list of unmatched ----
## --- FG names) - still a page in the compiled PDF, not a silent gap. ---
plot_text_list <- function(lines, title, subtitle = NULL, max_lines = 45) {
  if (length(lines) == 0) return(NULL)
  shown <- if (length(lines) > max_lines) c(lines[seq_len(max_lines)], paste0("... and ", length(lines) - max_lines, " more (see the REVIEW csv)")) else lines
  ggplot() +
    annotate("text", x = 0, y = rev(seq_along(shown)), label = shown, hjust = 0, size = 3.1, family = "mono") +
    xlim(0, 1) + ylim(0, length(shown) + 1) +
    labs(title = title, subtitle = subtitle) +
    theme_void(base_size = 10) +
    theme(plot.title = element_text(hjust = 0, face = "bold"), plot.subtitle = element_text(hjust = 0, size = 8, colour = "grey40"))
}

## 1) PB/QB plausibility - richer than the pre-existing PQ_ratio_by_fg.png
## (that one only ever covered QB_FG > 0 rows; this reads the newer
## plausibility_flag_REVIEW.csv, which also covers the genuinely
## IMPOSSIBLE cases - QB_FG <= PB_FG, or either <= 0 - that the older
## plot silently excluded).
validation_plots[["02_pbqb_plausibility"]] <- tryCatch({
  p <- fread(file.path(out_dir, "pbqb-traits", "plausibility_flag_REVIEW.csv"))
  d <- p[!is.na(PB_FG) & !is.na(QB_FG) & PB_FG > 0 & QB_FG > 0]
  if (nrow(d) == 0) stop("no FG has both a positive PB_FG and QB_FG to plot")
  ggplot(d, aes(x = PB_FG, y = QB_FG, color = plausibility_flag)) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", colour = "grey50") +           # QB = PB (P/Q = 1, the impossible boundary)
    geom_abline(slope = 1/0.05, intercept = 0, linetype = "dotted", colour = "grey70") +      # P/Q = 0.05 (bottom of typical band)
    geom_abline(slope = 1/0.3, intercept = 0, linetype = "dotted", colour = "grey70") +       # P/Q = 0.3 (top of typical band)
    geom_point(size = 2, alpha = 0.8) +
    scale_x_log10() + scale_y_log10() +
    scale_color_manual(values = c("Plausible" = "grey30", "Outside typical 0.05-0.3 P/Q range - review" = "darkorange",
                                  "IMPOSSIBLE (PB_FG <= 0)" = "firebrick", "IMPOSSIBLE (QB_FG <= 0)" = "firebrick",
                                  "IMPOSSIBLE (QB_FG <= PB_FG, i.e. P/Q >= 1 - a group cannot out-produce what it eats)" = "firebrick")) +
    labs(title = "PB/QB plausibility by functional group", subtitle = "Dashed = QB=PB (impossible boundary); dotted = typical 0.05-0.3 P/Q band",
         x = "PB_FG (log scale)", y = "QB_FG (log scale)", color = NULL) +
    theme_minimal(base_size = 9) + theme(legend.position = "bottom", legend.text = element_text(size = 6))
}, error = function(e) { message("[05_validation.R plot] PB/QB plausibility skipped - ", conditionMessage(e)); NULL })

## 2) Diet matrix column sum (should be 1 for every predator FG).
validation_plots[["03_diet_column_sum"]] <- tryCatch({
  d <- fread(file.path(out_dir, "diet", "diet_matrix_column_sum_REVIEW.csv"))
  if (nrow(d) == 0) stop("no predator FG flagged")
  d <- d[order(total)]
  d[, predator_fg := factor(predator_fg, levels = predator_fg)]
  ggplot(d, aes(x = total, y = predator_fg)) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
    geom_point(size = 2, colour = "firebrick") +
    labs(title = "Diet matrix columns NOT summing to 1", subtitle = "Every predator FG's diet column should sum to 1 (EwE's own 'Sum to one' convention) - dashed line = 1",
         x = "Column sum", y = NULL) +
    theme_minimal(base_size = 9)
}, error = function(e) { message("[05_validation.R plot] diet column sum skipped - ", conditionMessage(e)); NULL })

## 3) Diet cannibalism (self-prey fraction should stay under ~0.1).
validation_plots[["04_diet_cannibalism"]] <- tryCatch({
  d <- fread(file.path(out_dir, "diet", "diet_cannibalism_REVIEW.csv"))
  if (nrow(d) == 0) stop("no FG flagged")
  d <- d[order(weight)]
  d[, predator_fg := factor(predator_fg, levels = predator_fg)]
  ggplot(d, aes(x = weight, y = predator_fg)) +
    geom_vline(xintercept = 0.1, linetype = "dashed", colour = "grey50") +
    geom_col(fill = "firebrick", width = 0.6) +
    labs(title = "Cannibalism (self-prey) fraction above the ~0.1 guideline", subtitle = "EwE User Guide: avoid a group's own diet fraction of itself going much above 0.1",
         x = "Self-prey fraction of diet", y = NULL) +
    theme_minimal(base_size = 9)
}, error = function(e) { message("[05_validation.R plot] diet cannibalism skipped - ", conditionMessage(e)); NULL })

## 4) EcoBase biomass EVALUATION (reference only, per the project
## owner - "ecobase biomass should be evaluated but it shouldnt be
## incorporated in the Ecopath_B" - so this is not an accepted-vs-
## rejected fallback plot, just every candidate EcoBase found,
## plausible or not, clearly labeled as reference material rather
## than applied values). Combines both the general missing-biomass
## evaluation and the primary-producer/plankton one into one page.
validation_plots[["05_ecobase_fallback_plausibility"]] <- tryCatch({
  gen_path <- file.path(out_dir, "biomass", "ecobase_biomass_evaluation_NOT_INCORPORATED.csv")
  pp_path  <- file.path(out_dir, "biomass", "ecobase_biomass_evaluation_primary_producers_NOT_INCORPORATED.csv")
  gen <- if (file.exists(gen_path)) fread(gen_path)[, .(FG_num = as.character(FG_num), Biomass_t_km2,
                                                        plausible = as.character(plausible))] else NULL
  pp  <- if (file.exists(pp_path))  fread(pp_path)[, .(FG_num = Group, Biomass_t_km2 = Biomass_t, plausible = NA_character_)] else NULL
  d <- rbindlist(list(gen, pp), fill = TRUE)
  if (is.null(d) || nrow(d) == 0) stop("no EcoBase evaluation candidates in either file")
  d[, plausible := fifelse(is.na(plausible) | plausible == "NA", "Not checked", plausible)]
  d[, FG_num := factor(FG_num, levels = FG_num[order(Biomass_t_km2)])]
  ggplot(d, aes(x = Biomass_t_km2, y = FG_num, colour = plausible)) +
    geom_point(size = 2) +
    scale_x_log10() +
    scale_color_manual(values = c("TRUE" = "grey30", "FALSE" = "firebrick", "Not checked" = "steelblue")) +
    labs(title = "EcoBase biomass evaluation - reference only, NOT incorporated into Ecopath_B",
         subtitle = "EcoBase is evaluated for context but never applied to Ecopath_B - a human decides whether to add a real cited row",
         x = "EcoBase candidate Biomass_t_km2 (log scale)", y = "FG_num / Group", color = "Plausible vs. known densities") +
    theme_minimal(base_size = 9) + theme(legend.position = "bottom")
}, error = function(e) { message("[05_validation.R plot] EcoBase evaluation skipped - ", conditionMessage(e)); NULL })

## 5) How much of each FG's total biomass rides on an even-split
## (no real per-species measurement) species-level assumption.
validation_plots[["06_species_even_split_biomass_at_risk"]] <- tryCatch({
  d <- fread(file.path(out_dir, "biomass", "fg_species_biomass_needs_review.csv"))
  if (nrow(d) == 0) stop("no FG on an even split")
  d <- d[order(-FG_total_Biomass_t_km2)][seq_len(min(.N, 20))]
  d[, FG_label := paste0(FG_num, " - ", FG_name)]
  d[, FG_label := factor(FG_label, levels = rev(FG_label))]
  ggplot(d, aes(x = FG_total_Biomass_t_km2, y = FG_label, fill = n_species)) +
    geom_col() +
    scale_fill_gradient(low = "grey70", high = "firebrick") +
    labs(title = "FG biomass riding on an even-split species assumption (top 20 by biomass)",
         subtitle = "No real per-species density/literature measurement exists for these species within their FG",
         x = "FG total Biomass_t_km2", y = NULL, fill = "n species") +
    theme_minimal(base_size = 9)
}, error = function(e) { message("[05_validation.R plot] species even-split skipped - ", conditionMessage(e)); NULL })

## 5b) EcoBase-sourced rows excluded from a manual-cited biomass CSV -
## any row someone (or an older pipeline version) put into
## marine_megafauna_biomass.csv / primary_producer_plankton_biomass.csv /
## benthic_habitat_biomass_literature.csv with an EcoBase citation gets
## pulled out by 01b_biomass_unsurveyed.R's load_manual_cited_biomass_group()
## rather than incorporated into Ecopath_B (EcoBase is reference-only) -
## text-list page since these need a real citation added by hand, not a plot.
validation_plots[["05b_ecobase_sourced_rows_excluded"]] <- tryCatch({
  files <- Sys.glob(file.path(out_dir, "biomass", "*_ecobase_sourced_rows_EXCLUDED_REVIEW.csv"))
  if (length(files) == 0) stop("no *_ecobase_sourced_rows_EXCLUDED_REVIEW.csv files found")
  d <- rbindlist(lapply(files, function(f) { x <- fread(f); x[, source_file := basename(f)]; x }), fill = TRUE)
  if (nrow(d) == 0) stop("all *_ecobase_sourced_rows_EXCLUDED_REVIEW.csv files were empty")
  lines <- paste0(d$Group, " (", d$Year, ", from ", d$source_file, "): ", d$Source_citation)
  plot_text_list(lines, "EcoBase-sourced row(s) excluded from Ecopath_B - need a real citation",
                 "These carried an EcoBase citation in a manual-cited biomass CSV and were pulled out rather than incorporated - EcoBase is reference-only in this pipeline")
}, error = function(e) { message("[05_validation.R plot] ecobase-sourced rows excluded skipped - ", conditionMessage(e)); NULL })

## 6) Temporal-baseline mismatches (any manual-cited CSV with a
## Collection_year column differing from the Ecopath target Year) -
## every temporal_mismatch_REVIEW_*.csv this run produced, combined.
validation_plots[["07_temporal_mismatch"]] <- tryCatch({
  files <- Sys.glob(file.path(out_dir, "biomass", "temporal_mismatch_REVIEW_*.csv"))
  if (length(files) == 0) stop("no temporal_mismatch_REVIEW_*.csv files found")
  d <- rbindlist(lapply(files, function(f) { x <- fread(f); x[, source_file := basename(f)]; x }), fill = TRUE)
  if (nrow(d) == 0) stop("all temporal_mismatch_REVIEW_*.csv files were empty")
  ## One bar per Group x file: several Years of the same Group gave
  ## duplicated labels ("factor level [3] is duplicated") - keep the
  ## largest gap per label.
  d[, label := paste0(Group, " (", source_file, ")")]
  d <- d[order(-abs(gap_years))][!duplicated(label)][seq_len(min(.N, 25))]
  d[, label := factor(label, levels = rev(label))]
  ggplot(d, aes(x = gap_years, y = label, fill = source_file)) +
    geom_col() +
    geom_vline(xintercept = 0, colour = "grey50") +
    labs(title = "Temporal-baseline mismatch: how far each measurement is from the Ecopath target year",
         subtitle = "Positive = measured AFTER the Ecopath baseline year; flagged, not numerically corrected",
         x = "Gap (years)", y = NULL, fill = "Source file") +
    theme_minimal(base_size = 9) + theme(legend.position = "bottom")
}, error = function(e) { message("[05_validation.R plot] temporal mismatch skipped - ", conditionMessage(e)); NULL })

## 7) Fleet-level landings coverage - which FGs get most of their
## landings from the "Other GFCM countries - Unclassified" residual
## rather than a named 6-country fleet.
validation_plots[["08_fleet_coverage"]] <- tryCatch({
  d <- fread(file.path(out_dir, "fisheries", "ecopath_L_fg_coverage_check.csv"))
  d <- d[Landings_t_broad > 0][order(named_fleet_share)][seq_len(min(.N, 25))]
  if (nrow(d) == 0) stop("no FG with nonzero landings")
  d[, FG_label := paste0(FG_num, " - ", FG_name)]
  d[, FG_label := factor(FG_label, levels = rev(FG_label))]
  ggplot(d, aes(x = named_fleet_share, y = FG_label, fill = mostly_other_countries)) +
    geom_col() +
    geom_vline(xintercept = 0.5, linetype = "dashed", colour = "grey50") +
    scale_fill_manual(values = c(`TRUE` = "firebrick", `FALSE` = "grey30")) +
    labs(title = "Named 6-country fleet share of total landings (25 lowest-coverage FGs)",
         subtitle = "The rest sits in Ecopath_L/Di's 'Other GFCM countries - Unclassified' column, not lost - just coarser fleet/gear detail",
         x = "Named-fleet share", y = NULL, fill = "< 50% named") +
    theme_minimal(base_size = 9) + theme(legend.position = "bottom")
}, error = function(e) { message("[05_validation.R plot] fleet coverage skipped - ", conditionMessage(e)); NULL })

## 8) FG(s) with no Ecopath_B biomass at all, and old-workbook FG names
## that couldn't be matched - simple text-list pages (no numeric axis to
## plot, but still a page, not a silent gap).
validation_plots[["09_fg_missing_ecopath_b"]] <- tryCatch({
  d <- fread(file.path(out_dir, "biomass", "fg_missing_ecopath_B_REVIEW.csv"))
  if (nrow(d) == 0) stop("no FG missing Ecopath_B")
  plot_text_list(paste0(d$FG_num, " - ", d$FG_name), "FG(s) with NO Ecopath_B biomass at all",
                 "Every biomass source (survey, stock assessment, EcoBase, manual-cited literature) came back empty for these")
}, error = function(e) { message("[05_validation.R plot] fg_missing_ecopath_B skipped - ", conditionMessage(e)); NULL })

validation_plots[["10_old_workbook_unmatched_names"]] <- tryCatch({
  path <- file.path(csv_out_dir, "old_workbook_unmatched_fg_names_REVIEW.csv")
  d <- fread(path)
  if (nrow(d) == 0) stop("every old FG name matched")
  plot_text_list(d$Old_FG_name, "Old-workbook FG names that couldn't be matched to any new FG",
                 "Neither an exact nor a fuzzy name match - check for a renamed or split/merged group")
}, error = function(e) { message("[05_validation.R plot] old-workbook unmatched names skipped - ", conditionMessage(e)); NULL })

## Per project decision ("i am missing a plot with other validation like
## the biomass of FG relative to old model"): p_comparison (now
## "01_old_vs_new_comparison") plots every metric together as an old-vs-
## new scatter, faceted by metric - useful for spotting outliers overall,
## but it doesn't answer "how does THIS FG's biomass compare to the old
## model" at a glance, since FG identity isn't readable off a scatter
## with ~90 overlapping points. This is a dedicated per-FG view for
## Biomass specifically: one horizontal bar per FG, the new/old ratio on
## a log axis, a reference line at ratio = 1 (no change), colored by the
## same flag as comparison_with_old_westmed_estimates_REVIEW.csv, sorted
## by ratio so the biggest increases and decreases are at the ends.
validation_plots[["01b_biomass_vs_old_model_by_fg"]] <- tryCatch({
  d <- comparison_dt[metric == "Biomass" & !is.na(ratio) & is.finite(ratio) & ratio > 0]
  if (nrow(d) == 0) stop("no FG has both an old and new Biomass value to plot")
  d[, fg_label := paste0(FG_num, " - ", FG_name)]
  d <- d[order(ratio)]
  d[, fg_label := factor(fg_label, levels = fg_label)]
  ggplot(d, aes(x = ratio, y = fg_label, color = flag)) +
    geom_vline(xintercept = 1, linetype = "dashed", colour = "grey50") +
    geom_segment(aes(x = 1, xend = ratio, y = fg_label, yend = fg_label), linewidth = 0.4, alpha = 0.6) +
    geom_point(size = 1.8) +
    scale_x_log10() +
    scale_color_manual(values = c("Within review range" = "grey30", "Large deviation - review" = "firebrick",
                                  "Old was zero, new is nonzero" = "darkorange",
                                  "NEW value where old had none" = "steelblue",
                                  "OLD value now MISSING in the new pipeline" = "grey60")) +
    labs(title = "New pipeline Biomass vs. old West Med model, by functional group",
         subtitle = paste0("Dashed line = no change (ratio = 1, log scale). ", nrow(d), " of ", length(unique(comparison_dt$FG_name)),
                           " FGs had a usable old-model biomass to compare against - see comparison_with_old_westmed_estimates_REVIEW.csv for the rest."),
         x = "New Ecopath_B / old workbook Biomass (log scale)", y = NULL, color = NULL) +
    theme_minimal(base_size = 8) +
    theme(legend.position = "bottom", axis.text.y = element_text(size = 6))
}, error = function(e) { message("[05_validation.R plot] biomass_vs_old_model_by_fg skipped - ", conditionMessage(e)); NULL })

validation_plots <- c(list("01_old_vs_new_comparison" = p_comparison), validation_plots)
validation_plots <- validation_plots[!sapply(validation_plots, is.null)]

## =================================================================
## STEP C0: the PB/QB/F/method-comparison validation plots - moved here
## from 03_pbqb-traits.R (per project decision: "some of the plots can
## be moved on validation code and save the plots on validation
## subfolders"). Built directly from the CSVs 03_pbqb-traits.R already
## writes to out_dir/pbqb-traits (species_pb_qb_by_taxon_group.csv,
## fg_pb_qb_weighted.csv / fg_pb_qb_weighted_with_F.csv,
## PQ_ratio_by_fg_REVIEW.csv - STEP B above already indexes the last
## one), not from in-memory objects, since this script runs as its own
## step after 03_pbqb-traits.R has already finished. Every PNG is saved
## into plot_dir (out_dir/plots/validation) under the same filename the
## embed step right below expects, so that step needs no change.
## =================================================================
PBQB_CSV_DIR <- file.path(out_dir, "pbqb-traits")
species_pb_qb_path <- file.path(PBQB_CSV_DIR, "species_pb_qb_by_taxon_group.csv")
fg_pb_qb_with_f_path <- file.path(PBQB_CSV_DIR, "fg_pb_qb_weighted_with_F.csv")
fg_pb_qb_path <- file.path(PBQB_CSV_DIR, "fg_pb_qb_weighted.csv")

if (!file.exists(species_pb_qb_path) || !(file.exists(fg_pb_qb_with_f_path) || file.exists(fg_pb_qb_path))) {
  message("[05_validation.R] Skipping PB/QB/F validation plots - species_pb_qb_by_taxon_group.csv and/or",
          " fg_pb_qb_weighted*.csv not found in ", PBQB_CSV_DIR, " (run 03_pbqb-traits.R first).")
} else {
  results_pb_qb <- fread(species_pb_qb_path)
  ## fg_pb_qb_weighted_with_F.csv (has F_FG) is preferred over the plain
  ## fg_pb_qb_weighted.csv - it only exists when FG_YIELD_SOURCE ==
  ## "fg_catch_csv" was set, same conditional 03_pbqb-traits.R itself uses.
  fg_weighted_pb_qb <- fread(if (file.exists(fg_pb_qb_with_f_path)) fg_pb_qb_with_f_path else fg_pb_qb_path)
  
  ## --- FG-level PB vs QB scatter (fg_pb_qb_scatter.png) - lets a -------
  ## --- reviewer see every FG's final PB_FG/QB_FG position at a glance, -
  ## --- colored by dominant dispatch group, sized by FG biomass. --------
  p_fg_pb_qb <- tryCatch({
    dg_by_fg <- results_pb_qb[!is.na(dispatch_group), .(Biomass_dg = sum(Biomass, na.rm = TRUE)), by = .(FG, dispatch_group)]
    dg_dominant <- dg_by_fg[dg_by_fg[, .I[which.max(Biomass_dg)], by = FG]$V1][, .(FG, dispatch_group)]
    d <- merge(fg_weighted_pb_qb, dg_dominant, by = "FG", all.x = TRUE)
    d <- d[!is.na(PB_FG) & !is.na(QB_FG)]
    if (nrow(d) == 0) stop("no FG has both PB_FG and QB_FG")
    label_layer <- if (requireNamespace("ggrepel", quietly = TRUE)) {
      ggrepel::geom_text_repel(aes(label = FG_name), size = 2.2, max.overlaps = 15, show.legend = FALSE)
    } else {
      geom_text(aes(label = FG_name), size = 2.2, vjust = -0.6, show.legend = FALSE)
    }
    ggplot(d, aes(x = PB_FG, y = QB_FG, color = dispatch_group, size = Biomass_FG)) +
      geom_point(alpha = 0.75) +
      label_layer +
      scale_size_continuous(guide = "none") +
      labs(title = "PB vs. QB by functional group", subtitle = "Point size = FG biomass; color = dominant dispatch group (fish/mammal/seabird/invertebrate)",
           x = "PB_FG (/year)", y = "QB_FG (/year)", color = NULL) +
      theme_minimal(base_size = 8) + theme(legend.position = "bottom")
  }, error = function(e) { message("[05_validation.R plot] FG PB/QB scatter skipped - ", conditionMessage(e)); NULL })
  if (!is.null(p_fg_pb_qb)) ggsave(file.path(plot_dir, "fg_pb_qb_scatter.png"), p_fg_pb_qb, width = 11, height = 9, dpi = 150, bg = "white")
  
  ## --- Fishing mortality (F) by FG (F_by_fg.png). F isn't one single ---
  ## --- column: a fish FG with at least one species carrying a real -----
  ## --- per-species Fmort has F already folded into PB_FG upstream; -----
  ## --- every other FG relies on fg_weighted's own F_FG (Yield_FG / -----
  ## --- Biomass_FG). Combined into one F_for_plot per FG. ----------------
  p_f_by_fg <- tryCatch({
    species_F_by_fg <- results_pb_qb[!is.na(Fmort), .(
      F_species_weighted = sum(Biomass * Fmort, na.rm = TRUE) / sum(Biomass[!is.na(Fmort)], na.rm = TRUE)
    ), by = .(FG, FG_name)]
    f_dt <- merge(fg_weighted_pb_qb[, .(FG, FG_name, Biomass_FG,
                                        F_FG = if ("F_FG" %in% names(fg_weighted_pb_qb)) F_FG else NA_real_)],
                  species_F_by_fg, by = c("FG", "FG_name"), all = TRUE)
    f_dt[, F_for_plot := fifelse(!is.na(F_species_weighted), F_species_weighted, F_FG)]
    f_dt <- f_dt[!is.na(F_for_plot)]
    if (nrow(f_dt) == 0) stop("no FG has an F value yet (species-level Fmort AND fg_weighted's F_FG both empty)")
    f_dt[, F_source := fifelse(!is.na(F_species_weighted), "Species-level (Fmort)", "FG-level (Yield_FG/Biomass_FG)")]
    f_dt[, FG_label := paste0(as.integer(FG), "_", FG_name)]
    f_dt <- f_dt[order(-F_for_plot)]
    f_dt[, FG_label := factor(FG_label, levels = FG_label)]
    ggplot(f_dt, aes(x = F_for_plot, y = reorder(FG_label, F_for_plot), fill = F_source)) +
      geom_col() +
      labs(title = "Fishing mortality (F) by functional group",
           subtitle = "Biomass-weighted mean of species-level Fmort where any exists, else Yield_FG / Biomass_FG",
           x = expression(F~(year^-1)), y = NULL, fill = "Source") +
      theme_minimal(base_size = 7) + theme(legend.position = "bottom")
  }, error = function(e) { message("[05_validation.R plot] F by FG skipped - ", conditionMessage(e)); NULL })
  if (!is.null(p_f_by_fg)) {
    ggsave(file.path(plot_dir, "F_by_fg.png"), p_f_by_fg,
           width = 10, height = max(8, 0.16 * nrow(p_f_by_fg$data)), dpi = 150, bg = "white", limitsize = FALSE)
  }
  
  ## --- P/Q ratio (PQ_ratio_by_fg.png), from the REVIEW csv -------------
  ## --- 03_pbqb-traits.R already writes (STEP B above already indexes ---
  ## --- it as a row count; this reads the same file back and plots it). -
  p_pq_ratio <- tryCatch({
    pq_path <- file.path(PBQB_CSV_DIR, "PQ_ratio_by_fg_REVIEW.csv")
    if (!file.exists(pq_path)) stop("PQ_ratio_by_fg_REVIEW.csv not found")
    pq_dt <- fread(pq_path)
    if (nrow(pq_dt) == 0) stop("PQ_ratio_by_fg_REVIEW.csv is empty")
    pq_dt[, FG_label := paste0(as.integer(FG), "_", FG_name)]
    pq_dt <- pq_dt[order(-PQ_ratio)]
    pq_dt[, FG_label := factor(FG_label, levels = FG_label)]
    ggplot(pq_dt, aes(x = PQ_ratio, y = reorder(FG_label, PQ_ratio), color = flag)) +
      geom_point(size = 2) +
      geom_vline(xintercept = c(0.05, 0.3), linetype = "dashed", colour = "grey40") +
      scale_color_manual(values = c("Within typical range" = "grey30", "Outside typical 0.05-0.3 range" = "firebrick")) +
      labs(title = "P/Q ratio (PB_FG / QB_FG) by functional group",
           subtitle = "Dashed lines = typical 0.05-0.3 review band, not a hard cutoff",
           x = "P/Q", y = NULL, color = NULL) +
      theme_minimal(base_size = 7) + theme(legend.position = "bottom")
  }, error = function(e) { message("[05_validation.R plot] P/Q ratio skipped - ", conditionMessage(e)); NULL })
  if (!is.null(p_pq_ratio)) {
    ggsave(file.path(plot_dir, "PQ_ratio_by_fg.png"), p_pq_ratio,
           width = 10, height = max(8, 0.16 * nrow(p_pq_ratio$data)), dpi = 150, bg = "white", limitsize = FALSE)
  }
  
  ## --- Fish PB/QB method-comparison plots (fish only - the group with --
  ## --- the most independent methods: 6 for PB, 4 for QB). One point ----
  ## --- per method per species, faceted by FG, chosen method -----------
  ## --- highlighted distinctly from the rest. ----------------------------
  fish_results <- results_pb_qb[dispatch_group == "fish"]
  if (nrow(fish_results) == 0) {
    message("[05_validation.R plot] fish PB/QB method comparison skipped - no dispatch_group == 'fish' rows in species_pb_qb_by_taxon_group.csv")
  } else {
    fish_results[, FG_label := paste0(FG, "_", FG_name)]
    fish_fg_label_order <- unique(fish_results[, .(FG, FG_label)])[order(FG)]$FG_label
    
    pb_cols <- intersect(c("PB_Pauly_1980", "PB_FishLife_2023", "PB_Gascuel_2008", "PB_Hoenig_1983", "PB_Then_2015", "PB_AlversonCarney_1975"), names(fish_results))
    fish_pb_long <- melt(fish_results[, c("Species", "FG_label", "PB_method", ..pb_cols)],
                         id.vars = c("Species", "FG_label", "PB_method"), variable.name = "method", value.name = "PB")
    fish_pb_long[, method := gsub("^PB_", "", method)]
    fish_pb_long <- fish_pb_long[!is.na(PB)]
    fish_pb_long[, FG_label := factor(FG_label, levels = fish_fg_label_order)]
    method_label_map_dt <- read_validation_reference("pb_method_label_map.csv", required_cols = c("method", "label"))
    method_label_map <- setNames(method_label_map_dt$label, method_label_map_dt$method)
    fish_pb_long[, is_chosen := mapply(function(m, chosen) grepl(method_label_map[[m]], chosen, ignore.case = TRUE),
                                       method, PB_method)]
    p_pb_methods <- ggplot(fish_pb_long, aes(x = method, y = PB)) +
      geom_point(aes(color = is_chosen, size = is_chosen)) +
      scale_color_manual(values = c(`TRUE` = "firebrick", `FALSE` = "grey50"),
                         labels = c(`TRUE` = "Chosen for FG average", `FALSE` = "Other method"), name = NULL) +
      scale_size_manual(values = c(`TRUE` = 4, `FALSE` = 2.5), guide = "none") +
      facet_wrap(~ FG_label, scales = "free_y") +
      theme_bw(base_size = 11) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
      labs(title = "P/B method comparison - fish, by functional group",
           subtitle = "Each point is one species x method; panels are FGs (not individual species)",
           x = NULL, y = expression(P/B~(year^-1)))
    ggsave(file.path(plot_dir, "fish_PB_methods_comparison_by_FG.png"), p_pb_methods, width = 14, height = 11, dpi = 150)
    
    qb_cols <- intersect(c("QB_PalomaresPauly_1998Z", "QB_PalomaresPauly_1998noZ", "QB_ChristensenPauly_1992", "QB_ChristensenEtAl_2008"), names(fish_results))
    fish_qb_long <- melt(fish_results[, c("Species", "FG_label", "QB_method", ..qb_cols)],
                         id.vars = c("Species", "FG_label", "QB_method"), variable.name = "method", value.name = "QB")
    fish_qb_long[, method := gsub("^QB_", "", method)]
    fish_qb_long <- fish_qb_long[!is.na(QB)]
    fish_qb_long[, FG_label := factor(FG_label, levels = fish_fg_label_order)]
    qb_label_map_dt <- read_validation_reference("qb_method_label_map.csv", required_cols = c("method", "label"))
    qb_label_map <- setNames(qb_label_map_dt$label, qb_label_map_dt$method)
    fish_qb_long[, is_chosen := mapply(function(m, chosen) grepl(qb_label_map[[m]], chosen, fixed = TRUE),
                                       method, QB_method)]
    p_qb_methods <- ggplot(fish_qb_long, aes(x = method, y = QB)) +
      geom_point(aes(color = is_chosen, size = is_chosen)) +
      scale_color_manual(values = c(`TRUE` = "firebrick", `FALSE` = "grey50"),
                         labels = c(`TRUE` = "Chosen for FG average", `FALSE` = "Other method"), name = NULL) +
      scale_size_manual(values = c(`TRUE` = 4, `FALSE` = 2.5), guide = "none") +
      facet_wrap(~ FG_label, scales = "free_y") +
      theme_bw(base_size = 11) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
      labs(title = "Q/B method comparison - fish, by functional group",
           subtitle = "Each point is one species x method; panels are FGs (not individual species)",
           x = NULL, y = expression(Q/B~(year^-1)))
    ggsave(file.path(plot_dir, "fish_QB_methods_comparison_by_FG.png"), p_qb_methods, width = 14, height = 11, dpi = 150)
  }
  
  ## --- FG_PB_QB_comparison*.png - the final, chosen PB/QB-by-FG figure -
  ## --- (dot-whisker, spread across ALL methods/dispatch groups, not ----
  ## --- just fish). Paginated since height scales with FG count and -----
  ## --- ggsave has a 50in hard limit. -------------------------------------
  save_paginated_pb_qb_plot <- function(pb_mean, pb_long, qb_mean, qb_long, label_order,
                                        pb_x_lab, qb_x_lab, file_prefix, plot_dir,
                                        height_per_row = 0.35, width_in = 16, dpi = 150,
                                        max_height_in = 40) {
    n_per_page <- max(10, floor(max_height_in / height_per_row))
    pages <- if (length(label_order) <= n_per_page) list(label_order) else split(label_order, ceiling(seq_along(label_order) / n_per_page))
    saved <- character(0)
    for (i in seq_along(pages)) {
      page_labels <- pages[[i]]
      page_height <- max(6, height_per_row * length(page_labels))
      p_pb_i <- ggplot() +
        geom_segment(data = pb_mean[Label %in% page_labels], aes(x = Mean - SD, xend = Mean + SD, y = Label, yend = Label), linewidth = 0.7, colour = "black") +
        geom_point(data = pb_mean[Label %in% page_labels], aes(x = Mean, y = Label), shape = "|", size = 5, colour = "black") +
        geom_point(data = pb_long[Label %in% page_labels], aes(x = Value, y = Label, colour = method, fill = method), shape = 21, size = 2.5, alpha = 0.7) +
        scale_colour_brewer(palette = "Set1") + scale_fill_brewer(palette = "Set1") +
        theme_bw(base_size = 12) + theme(legend.position = "bottom", panel.grid.minor.y = element_blank()) +
        labs(x = pb_x_lab, y = NULL, colour = "Method", fill = "Method")
      p_qb_i <- ggplot() +
        geom_segment(data = qb_mean[Label %in% page_labels], aes(x = Mean - SD, xend = Mean + SD, y = Label, yend = Label), linewidth = 0.7, colour = "black") +
        geom_point(data = qb_mean[Label %in% page_labels], aes(x = Mean, y = Label), shape = "|", size = 5, colour = "black") +
        geom_point(data = qb_long[Label %in% page_labels], aes(x = Value, y = Label, colour = method, fill = method), shape = 21, size = 2.5, alpha = 0.7) +
        scale_colour_brewer(palette = "Set1") + scale_fill_brewer(palette = "Set1") +
        theme_bw(base_size = 12) + theme(legend.position = "bottom", panel.grid.minor.y = element_blank()) +
        labs(x = qb_x_lab, y = NULL, colour = "Method", fill = "Method")
      p_combined_i <- p_pb_i + p_qb_i
      fname <- if (length(pages) > 1) paste0(file_prefix, "_page", i, ".png") else paste0(file_prefix, ".png")
      fpath <- file.path(plot_dir, fname)
      ggsave(fpath, p_combined_i, width = width_in, height = page_height, dpi = dpi)
      saved <- c(saved, fpath)
    }
    message("[05_validation.R plot] Saved: ", paste(basename(saved), collapse = ", "))
    invisible(saved)
  }
  
  exclude_cols <- c("PB", "QB", "PB_method", "QB_method")
  all_pb_method_cols <- setdiff(grep("^PB_", names(results_pb_qb), value = TRUE), c(exclude_cols, grep("_source$", names(results_pb_qb), value = TRUE)))
  all_qb_method_cols <- setdiff(grep("^QB_", names(results_pb_qb), value = TRUE), c(exclude_cols, grep("_source$", names(results_pb_qb), value = TRUE)))
  
  if (length(all_pb_method_cols) == 0 && length(all_qb_method_cols) == 0) {
    message("[05_validation.R plot] FG_PB_QB_comparison skipped - no PB_*/QB_* method columns found in species_pb_qb_by_taxon_group.csv")
  } else {
    species_pb_long <- melt(results_pb_qb[, c("Species", "FG", "FG_name", "dispatch_group", "Biomass", ..all_pb_method_cols)],
                            id.vars = c("Species", "FG", "FG_name", "dispatch_group", "Biomass"), variable.name = "method", value.name = "PB")
    species_pb_long[, method := gsub("^PB_", "", method)]
    species_pb_long <- species_pb_long[!is.na(PB)]
    
    species_qb_long <- melt(results_pb_qb[, c("Species", "FG", "FG_name", "dispatch_group", "Biomass", ..all_qb_method_cols)],
                            id.vars = c("Species", "FG", "FG_name", "dispatch_group", "Biomass"), variable.name = "method", value.name = "QB")
    species_qb_long[, method := gsub("^QB_", "", method)]
    species_qb_long <- species_qb_long[!is.na(QB)]
    
    fg_pb_by_method <- species_pb_long[, .(PB = sum(Biomass * PB, na.rm = TRUE) / sum(Biomass[!is.na(PB)], na.rm = TRUE)), by = .(FG, FG_name, method)]
    fg_qb_by_method <- species_qb_long[, .(QB = sum(Biomass * QB, na.rm = TRUE) / sum(Biomass[!is.na(QB)], na.rm = TRUE)), by = .(FG, FG_name, method)]
    
    ## FG_name already comes straight from species_pb_qb_by_taxon_group.csv
    ## (03_pbqb-traits.R already resolved it there, including its own
    ## FG_WMed_2026.csv fallback) - no separate external-reference join
    ## needed here.
    fg_pb_by_method[, FG_num := as.character(as.integer(FG))]
    fg_qb_by_method[, FG_num := as.character(as.integer(FG))]
    fg_pb_by_method[, FG_label := fifelse(!is.na(FG_name), paste0(FG_num, "_", FG_name), as.character(FG_num))]
    fg_qb_by_method[, FG_label := fifelse(!is.na(FG_name), paste0(FG_num, "_", FG_name), as.character(FG_num))]
    
    fg_pb_mean <- fg_pb_by_method[, .(PB_mean = mean(PB, na.rm = TRUE), PB_sd = sd(PB, na.rm = TRUE)), by = .(FG_num, FG_label)]
    fg_qb_mean <- fg_qb_by_method[, .(QB_mean = mean(QB, na.rm = TRUE), QB_sd = sd(QB, na.rm = TRUE)), by = .(FG_num, FG_label)]
    
    ## sorted NUMERICALLY by FG number, descending so the lowest FG number
    ## sits at the TOP of the y-axis - same order shared by both PB and QB
    ## plots so an FG sits on the same row in both.
    fg_order_dt <- unique(rbindlist(list(fg_pb_mean[, .(FG_num, FG_label)], fg_qb_mean[, .(FG_num, FG_label)])))
    fg_order_dt[, FG_num := as.numeric(FG_num)]
    setorder(fg_order_dt, -FG_num)
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
  }
}

## --- embed the standalone validation PNGs generated just above (and by --
## --- 04_diets.R), so the compiled PDF really is "everything in one -----
## --- place" rather than just this script's own new checks. -------------
existing_pngs <- c(
  file.path(out_dir, "plots", "validation", "PQ_ratio_by_fg.png"),
  file.path(out_dir, "plots", "validation", "fg_pb_qb_scatter.png"),
  file.path(out_dir, "plots", "validation", "F_by_fg.png"),
  file.path(out_dir, "plots", "validation", "fish_PB_methods_comparison_by_FG.png"),
  file.path(out_dir, "plots", "validation", "fish_QB_methods_comparison_by_FG.png"),
  file.path(out_dir, "plots", "validation", "biomass_by_fg_source.png"),
  file.path(out_dir, "plots", "diets", "diet_matrix_heatmap.png")
)
embedded_png_pages <- list()
for (png_path in existing_pngs) {
  if (!file.exists(png_path)) next
  embedded_png_pages[[basename(png_path)]] <- tryCatch({
    img <- png::readPNG(png_path)
    ggplot() + annotation_raster(img, xmin = 0, xmax = 1, ymin = 0, ymax = 1) +
      xlim(0, 1) + ylim(0, 1) +
      labs(caption = paste0("(already produced by an earlier pipeline script: ", basename(png_path), ")")) +
      theme_void() + theme(plot.caption = element_text(size = 7, colour = "grey50"))
  }, error = function(e) { message("[05_validation.R plot] could not embed ", png_path, " - ", conditionMessage(e)); NULL })
}

## FG_PB_QB_comparison_*.png - the final, chosen PB/QB-by-FG figure(s),
## one or more numbered pages depending on how many FGs there are,
## generated by save_paginated_pb_qb_plot() just above. Still globbed
## rather than a fixed filename, since the page count varies with FG count.
pb_qb_comparison_pngs <- sort(Sys.glob(file.path(out_dir, "plots", "validation", "FG_PB_QB_comparison*.png")))
for (png_path in pb_qb_comparison_pngs) {
  embedded_png_pages[[basename(png_path)]] <- tryCatch({
    img <- png::readPNG(png_path)
    ggplot() + annotation_raster(img, xmin = 0, xmax = 1, ymin = 0, ymax = 1) +
      xlim(0, 1) + ylim(0, 1) +
      labs(caption = paste0("(already produced by an earlier pipeline script: ", basename(png_path), ")")) +
      theme_void() + theme(plot.caption = element_text(size = 7, colour = "grey50"))
  }, error = function(e) { message("[05_validation.R plot] could not embed ", png_path, " - ", conditionMessage(e)); NULL })
}
embedded_png_pages <- embedded_png_pages[!sapply(embedded_png_pages, is.null)]
if (length(embedded_png_pages) > 0) {
  message("[05_validation.R] Embedding ", length(embedded_png_pages), " already-existing validation PNG(s) from",
          " earlier pipeline scripts into the compiled PDF too: ", paste(names(embedded_png_pages), collapse = ", "), ".")
}
validation_plots <- c(validation_plots, embedded_png_pages)

VALIDATION_PLOTS_PDF <- file.path(plot_dir, "validation_plots_ALL.pdf")
if (length(validation_plots) == 0) {
  message("\n[05_validation.R] STEP C - no validation plot could be built (every source REVIEW csv was either",
          " missing or empty) - nothing written to ", VALIDATION_PLOTS_PDF, ".")
} else {
  pdf(VALIDATION_PLOTS_PDF, width = 11, height = 8)   # same open-loop-close PDF pattern 02_fisheries.R's own validation_plots list uses
  for (p in validation_plots) print(p)
  dev.off()
  ## The per-FG biomass-vs-old-model plot has ~one row per FG
  ## (potentially 90+) - the shared 10x7in PNG size every other check uses
  ## would cram all those labels unreadably. Its OWN standalone PNG (not
  ## the PDF page, which stays a fixed page size like every other page)
  ## gets a height that scales with how many FGs it actually plotted, so
  ## the labels stay legible even if it's a tall image.
  n_fg_biomass_plot <- tryCatch(
    comparison_dt[metric == "Biomass" & !is.na(ratio) & is.finite(ratio) & ratio > 0, uniqueN(FG_name)],
    error = function(e) 0)
  for (nm in names(validation_plots)) {
    ## embedded_png_pages' names are basename(png_path) - already-existing
    ## PNGs produced by an earlier pipeline script, keyed WITH their ".png"
    ## extension (so they can be embedded as PDF pages above). Re-saving
    ## one of those here via paste0(nm, ".png") was producing a duplicate
    ## file named "<name>.png.png" for no reason - the real PNG already
    ## exists at its own original path, so just skip it.
    if (nm %in% names(embedded_png_pages)) next
    ht <- if (nm == "01b_biomass_vs_old_model_by_fg" && n_fg_biomass_plot > 0) max(7, 0.16 * n_fg_biomass_plot) else 7
    tryCatch(ggsave(file.path(plot_dir, paste0(nm, ".png")), validation_plots[[nm]], width = 10, height = ht, dpi = 150, bg = "white", limitsize = FALSE),
             error = function(e) NULL)
  }
  message("\n[05_validation.R] STEP C - ", length(validation_plots), " page(s) written to '", VALIDATION_PLOTS_PDF,
          "' (one PDF, every check this script could build a plot for, plus the standalone validation PNGs already",
          " produced by 01-04_*.R) and as individual PNGs under '", plot_dir, "'. A check missing from this PDF",
          " either had nothing to flag (clean) or its source REVIEW csv doesn't exist yet - see the message above",
          " naming which one and why, and validation_summary_ALL.csv for the full checklist either way.")
}

message("\n[05_validation.R] Done. Three files to look at first: ",
        "validation_plots_ALL.pdf (a compiled page per check - start here), ",
        "comparison_with_old_westmed_estimates_REVIEW.csv (old vs. new, by FG), and ",
        "validation_summary_ALL.csv (a plain-text index of every check, in one place).")

## --- Close run log --------------------------------------------------------
if (exists(".orig_message", envir = .GlobalEnv, inherits = FALSE)) {
  assign("message", .orig_message, envir = .GlobalEnv)  # undo the message() mirror
  rm(.orig_message, envir = .GlobalEnv)
}
sink()
close(.run_log_con)