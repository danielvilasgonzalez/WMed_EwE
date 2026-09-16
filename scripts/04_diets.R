## =================================================================
## PIPELINE STEP 4 of 4 (optional) - run AFTER 01_biomass.R
## Builds an EwE-format functional-group (FG) diet-composition matrix
## from Marta Coll's Mediterranean trophic metaweb database (DATA_ENTRY
## + Taxonomic_codes + Non_taxonomic_groups tabs), instead of from a
## stomach-content Access export or a FishBase/SeaLifeBase pull (those
## were the two source paths the earlier diet_to_ewe.R supported - see
## diet_to_ewe_tool_notes.md in the project for that history).
## REQUIRES 01_biomass.R to have run at least once against the SAME
## ECOPATH_WORKBOOK_PATH first - species->FG membership comes from
## FG_WMed.xlsx directly (no dependency on 01_biomass.R for that part),
## but each species' SHARE OF ITS FG's BIOMASS (needed to blend several
## species' diets into one FG-level diet) is read from 01_biomass.R's
## own FG_spp_Ecopath sheet - that's "proportion B[iomass]" straight
## from the biomass code, not a hand-typed number.
## Produces: diet_composition_ewe.csv (plain-text EwE import format)
## and the Ecopath_diet sheet in the shared workbook.
## =================================================================

## =================================================================
## 04_diets.R
##
## Per Andrea (2026-09-16, two rounds of changes): species->FG
## membership is read from FG_WMed.xlsx (same file/sheet 01_biomass.R
## reads), NOT a hand-built species_to_fg.csv, and each species' share
## of its FG's biomass is read from the biomass code's own output
## (01_biomass.R's FG_spp_Ecopath sheet, prop_sp_fg column) - NOT a
## hand-built species_biomass_in_fg.csv either. Both manual CSVs are
## now fallback-only, for species this diet run needs that genuinely
## aren't covered by either source. Everything else (predator/prey
## identity, diet proportions) comes straight from the metaweb file.
## =================================================================

pkgs <- c("data.table", "openxlsx", "stringr")
new_pkgs <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new_pkgs) > 0) install.packages(new_pkgs)
invisible(lapply(pkgs, library, character.only = TRUE))

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

if (!exists("ECOPATH_WORKBOOK_PATH", envir = .GlobalEnv, inherits = FALSE)) ECOPATH_WORKBOOK_PATH <- file.path(out_dir, "ecopath_ecosim_inputs.xlsx")   # same shared workbook 01_biomass.R/02_fisheries.R/03_pbqb-traits.R write to
if (!exists("METAWEB_XLSX_PATH",        envir = .GlobalEnv, inherits = FALSE)) METAWEB_XLSX_PATH        <- file.path(pcloud_dir, "data/data_entry_metaweb.xlsx")   # Marta Coll's Mediterranean trophic metaweb database
if (!exists("FG_REFERENCE_XLSX_PATH",   envir = .GlobalEnv, inherits = FALSE)) FG_REFERENCE_XLSX_PATH   <- file.path(pcloud_dir, "data/FG_WMed.xlsx")   # same fg_file 01_biomass.R reads - sheet 4: ESPECIE/GF/FG_name
if (!exists("FG_REFERENCE_SHEET",       envir = .GlobalEnv, inherits = FALSE)) FG_REFERENCE_SHEET       <- 4               # sheet index/name within FG_REFERENCE_XLSX_PATH - matches 01_biomass.R's `read_excel(fg_file, sheet = 4)`
if (!exists("SPECIES_TO_FG_MISSING_CSV_PATH", envir = .GlobalEnv, inherits = FALSE)) SPECIES_TO_FG_MISSING_CSV_PATH <- file.path(pcloud_dir, "data/species_to_fg_missing.csv")   # species, fg_name, proportion - ONLY for species not found in FG_REFERENCE_XLSX_PATH; ok if the file doesn't exist (treated as empty)
if (!exists("SPECIES_BIOMASS_IN_FG_CSV_PATH", envir = .GlobalEnv, inherits = FALSE)) SPECIES_BIOMASS_IN_FG_CSV_PATH <- file.path(pcloud_dir, "data/species_biomass_in_fg_missing.csv")   # species, fg_name, biomass_proportion - ONLY for species x FG pairs not covered by ECOPATH_WORKBOOK_PATH's own FG_spp_Ecopath sheet; ok if the file doesn't exist
if (!exists("EWE_GROUP_TABLE_CSV_PATH", envir = .GlobalEnv, inherits = FALSE)) EWE_GROUP_TABLE_CSV_PATH <- file.path(pcloud_dir, "data/ewe_group_table.csv")   # group_number, group_name, is_predator - row/column order for the output matrix
if (!exists("OUTPUT_CSV_PATH",          envir = .GlobalEnv, inherits = FALSE)) OUTPUT_CSV_PATH          <- file.path(out_dir, "diet_composition_ewe.csv")

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

message("[04_diets.R] Config: METAWEB_XLSX_PATH = ", METAWEB_XLSX_PATH,
        " | FG_REFERENCE_XLSX_PATH = ", FG_REFERENCE_XLSX_PATH, " (sheet ", FG_REFERENCE_SHEET, ")",
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
  if (nrow(dt) == 0) stop("No usable diet records after applying DIET_METRIC_PRIORITY - check DATA_ENTRY has real data (the empty metaweb template ships with none).")

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
  species_diet[]
}

## =================================================================
## STEP 6: Species -> FG, sourced from FG_WMed.xlsx FIRST (Andrea's
## real starting point - every species already assigned an FG there),
## falling back to a small manual CSV ONLY for species this diet run
## actually needs but aren't in that reference file at all.
## =================================================================
read_species_to_fg_from_reference <- function(path, sheet = FG_REFERENCE_SHEET) {
  fg_raw <- as.data.table(openxlsx::read.xlsx(path, sheet = sheet, detectDates = FALSE))
  needed <- c("ESPECIE", "GF", "FG_name")
  missing_cols <- setdiff(needed, names(fg_raw))
  if (length(missing_cols) > 0) {
    stop("read_species_to_fg_from_reference(): '", path, "' sheet ", sheet, " is missing expected column(s): ",
         paste(missing_cols, collapse = ", "), " (expected the same ESPECIE/GF/FG_name layout 01_biomass.R reads).")
  }
  out <- unique(fg_raw[, .(species = ESPECIE, fg_name = FG_name, proportion = 1)])
  out <- out[!is.na(species) & species != ""]
  out
}

build_species_to_fg <- function(needed_species, reference_path, missing_csv_path) {
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
    if (length(still_missing) > 0) {
      message("[04_diets.R] ", length(still_missing), " species appear in the metaweb diet data but have ",
              "NO FG assignment in either ", reference_path, " or ", missing_csv_path, " - they'll be dropped from ",
              "the FG-level diet matrix. Add them to ", missing_csv_path, " (species, fg_name, proportion) to include them: ",
              paste(still_missing, collapse = ", "))
    }
  }

  combined <- rbind(from_reference[species %in% needed_species], from_missing_csv, fill = TRUE)
  message("[04_diets.R] species_to_fg: ", uniqueN(combined$species), " of ", length(needed_species),
          " needed species resolved to an FG (", nrow(from_reference[species %in% needed_species]), " from ",
          reference_path, ", ", nrow(from_missing_csv), " from ", missing_csv_path, ").")
  combined
}

## =================================================================
## STEP 6b: Each species' share of its FG's BIOMASS - read from
## 01_biomass.R's own FG_spp_Ecopath sheet (column prop_sp_fg) in the
## shared workbook, NOT a hand-typed number. Falls back to
## SPECIES_BIOMASS_IN_FG_CSV_PATH only for species x FG pairs that
## sheet doesn't cover (e.g. a species added via the missing-species
## fallback above, which 01_biomass.R never saw). Any pair covered by
## neither source still gets a sane default inside build_fg_diet()
## itself (an equal split among that FG's other mapped species).
## =================================================================
read_biomass_proportion_from_workbook <- function(workbook_path, sheet = "FG_spp_Ecopath") {
  if (!file.exists(workbook_path) || !(sheet %in% openxlsx::getSheetNames(workbook_path))) {
    message("[04_diets.R] '", sheet, "' sheet not found in ", workbook_path, " - run 01_biomass.R against this ",
            "same workbook first to get real biomass shares. Falling back to SPECIES_BIOMASS_IN_FG_CSV_PATH only ",
            "for now (or the equal-split default inside build_fg_diet() for anything that misses too).")
    return(data.table(species = character(), fg_name = character(), biomass_proportion = numeric()))
  }
  fg_spp <- as.data.table(openxlsx::read.xlsx(workbook_path, sheet = sheet, detectDates = FALSE))
  needed <- c("Species", "FG_name", "prop_sp_fg")
  missing_cols <- setdiff(needed, names(fg_spp))
  if (length(missing_cols) > 0) {
    stop("read_biomass_proportion_from_workbook(): '", sheet, "' is missing expected column(s): ", paste(missing_cols, collapse = ", "))
  }
  out <- unique(fg_spp[, .(species = Species, fg_name = FG_name, biomass_proportion = prop_sp_fg)])
  out[!is.na(species) & species != ""]
}

build_biomass_in_fg <- function(needed_species_fg, workbook_path, fallback_csv_path) {
  from_workbook <- read_biomass_proportion_from_workbook(workbook_path)
  covered_pairs <- from_workbook[, paste(species, fg_name)]
  still_missing <- needed_species_fg[!paste(species, fg_name) %in% covered_pairs]

  from_fallback <- data.table(species = character(), fg_name = character(), biomass_proportion = numeric())
  if (nrow(still_missing) > 0 && file.exists(fallback_csv_path)) {
    from_fallback <- as.data.table(read.csv(fallback_csv_path, stringsAsFactors = FALSE))
    n_covered_by_fallback <- sum(paste(still_missing$species, still_missing$fg_name) %in% paste(from_fallback$species, from_fallback$fg_name))
    message("[04_diets.R] ", n_covered_by_fallback, " of ", nrow(still_missing), " species x FG pair(s) missing biomass share ",
            "from ", workbook_path, "'s FG_spp_Ecopath sheet were covered by ", fallback_csv_path, ".")
  } else if (nrow(still_missing) > 0) {
    message("[04_diets.R] ", nrow(still_missing), " species x FG pair(s) have no biomass share in either ",
            workbook_path, "'s FG_spp_Ecopath sheet or ", fallback_csv_path, " - build_fg_diet() will fall back to ",
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
## STEP 8b: Ecopath_diet sheet (added 2026-09-16, per Andrea) - the
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
                          fg_reference_path = FG_REFERENCE_XLSX_PATH,
                          species_to_fg_missing_path = SPECIES_TO_FG_MISSING_CSV_PATH,
                          biomass_fallback_path = SPECIES_BIOMASS_IN_FG_CSV_PATH,
                          group_table_path = EWE_GROUP_TABLE_CSV_PATH,
                          workbook_path = ECOPATH_WORKBOOK_PATH,
                          out_path = OUTPUT_CSV_PATH) {
  metaweb       <- read_metaweb(metaweb_path)
  lookup        <- build_code_lookup(metaweb$tax_codes, metaweb$nontax_groups)
  species_diet  <- build_species_diet(metaweb$data_entry, lookup)

  all_named     <- unique(c(species_diet$predator_name, species_diet$prey_name))
  generic_names <- unique(metaweb$nontax_groups$Group_name)   # source of truth for "generic category, not a real species" - see build_code_lookup()'s own header comment for why lookup$is_group isn't used here
  needed_species <- setdiff(all_named, generic_names)
  if (length(intersect(all_named, generic_names)) > 0) {
    message("[04_diets.R] ", length(intersect(all_named, generic_names)), " generic non-taxonomic label(s) in play ",
            "(", paste(intersect(all_named, generic_names), collapse = ", "), ") - these pass through as their own FG ",
            "automatically and don't need a species_to_fg entry.")
  }

  species_to_fg <- build_species_to_fg(needed_species, fg_reference_path, species_to_fg_missing_path)
  biomass_in_fg <- build_biomass_in_fg(unique(species_to_fg[, .(species, fg_name)]), workbook_path, biomass_fallback_path)
  fg_diet       <- build_fg_diet(species_diet, species_to_fg, biomass_in_fg)
  group_table   <- as.data.table(read.csv(group_table_path, stringsAsFactors = FALSE))

  export_ewe_matrix_csv(fg_diet, group_table, out_path)
  ecopath_diet_sheet <- build_ecopath_diet_sheet(fg_diet, group_table)
  upsert_workbook_sheets(list(Ecopath_diet = ecopath_diet_sheet), workbook_path)
  message("[04_diets.R] Wrote Ecopath_diet sheet to ", workbook_path, ".")

  ## --- Final workbook trim (added 2026-09-16, per Andrea) --------------
  ## 04_diets.R is the LAST script in the documented run order (01 -> 02
  ## -> 03 -> 04), so this is where the workbook gets reduced to EXACTLY
  ## the sheets Andrea specified: "info" (run metadata) plus the seven
  ## Ecopath_*/Ecosim_ts summary sheets - every native sheet the
  ## individual scripts wrote along the way (Ecopath, Catches_Ecopath,
  ## PB_QB, Ecosim, Catches_Ecosim, Fishing_Effort_by_Fleet,
  ## FG_spp_Ecopath, FG_spp_Ecosim, FG, traits_ewe, PB_QB_spp, References,
  ## Ecobase, Catches_Discards_FG_ts, ...) gets DROPPED here, not kept.
  ## This must run AFTER the read above that used FG_spp_Ecopath (native
  ## sheet, written by 01_biomass.R) for the biomass-proportion lookup -
  ## doing the trim any earlier in the pipeline would delete that sheet
  ## before 04 got a chance to read it.
  FINAL_WORKBOOK_SHEETS <- c("info", "Ecopath_B", "Ecopath_L", "Ecopath_Di", "Ecopath_PBQB",
                            "Ecopath_traits", "Ecopath_diet", "Ecosim_ts")
  info_sheet <- build_info_sheet()
  upsert_workbook_sheets(list(info = info_sheet), workbook_path)
  finalize_workbook_sheet_order(out_path = workbook_path, target_order = FINAL_WORKBOOK_SHEETS, drop_extras = TRUE)
  message("[04_diets.R] Final workbook trimmed to: ", paste(FINAL_WORKBOOK_SHEETS, collapse = ", "), ".")

  fg_diet[]
}

diet_result <- run_pipeline()
message("[04_diets.R] Done - ", nrow(diet_result), " predator-FG x prey-FG diet fraction(s) computed.")
