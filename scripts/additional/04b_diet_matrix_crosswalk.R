## =================================================================
## 04b_diet_matrix_crosswalk.R
##
## Converts an EXISTING diet composition matrix (rows = prey FG,
## columns = predator FG, values = proportion of that predator's diet
## coming from that prey - the same shape as the "Ecopath_diet" sheet)
## from one functional-group (FG) structure to a DIFFERENT one.
##
## Real use cases this was built for:
##   (a) You have a diet matrix from a published/EcoBase model (its own
##       FG list, e.g. a Gulf of Lion or whole-Mediterranean model) and
##       want to re-express it against THIS model's 79-FG structure
##       (FG_WMed_2026) to use as a starting diet matrix or cross-check.
##   (b) Your OWN FG structure changes between model versions (an FG
##       gets split into age/stanza groups, or several FGs get merged
##       into one "Expanding ambush predators"-style group) and you
##       need the old diet matrix re-expressed against the new FG list
##       instead of re-deriving it from scratch.
##
## Sourced standalone, not from 01-04 (unlike those, this doesn't need
## to run in pipeline order - it only needs a diet matrix + a crosswalk
## table, both supplied by you). Point the CONFIG paths below at your
## own files and run this script on its own.
##
## THE CORE PROBLEM: an FG crosswalk is rarely 1-to-1. A source FG can
## SPLIT into several target FGs (e.g. "Hake" -> "Hake juvenile" +
## "Hake adult"), and/or several source FGs can MERGE into one target
## FG (e.g. "Gorgonians" + "Corals" -> "GorgoniansCorals"). Both can
## happen to the SAME source FG at once (part of it merges into one
## target, part into another). Handling this needs two DIFFERENT kinds
## of weight, because the prey axis and the predator axis behave
## differently:
##
##   - PREY axis (rows) is a MASS/CONTRIBUTION axis: a predator's total
##     diet has to still sum to 1 after conversion. Splitting a prey FG
##     across several target preys means dividing its contribution
##     (Split_Weight, fractions of the SOURCE FG that sum to 1 across
##     all its target destinations); merging several prey FGs into one
##     target just ADDS their contributions - no dilution.
##
##   - PREDATOR axis (columns) is a COMPOSITION axis: each predator's
##     diet vector is already internally-consistent, so splitting one
##     source predator into several target predators means each target
##     gets a full COPY of the same diet vector (nothing to weight -
##     they're the same animal's diet, just recategorized). Merging
##     several source predators into one target predator means taking
##     a WEIGHTED AVERAGE of their diet vectors (Merge_Weight, the
##     relative biomass/abundance each source predator contributes to
##     the merged target group - fractions that sum to 1 across all
##     source predators feeding into that target).
##
## Both Split_Weight and Merge_Weight default to an EQUAL split/average
## when not supplied (with a warning) - override them in the crosswalk
## CSV with real biomass shares (e.g. from fg_index_regional_combined's
## mean_density, or biomass_proportion_by_species_fg.csv, both written
## by 01_biomass.R) whenever you have them; that's what makes the
## conversion a genuine biomass-weighted aggregation rather than a
## naive average. See apply_biomass_weights_to_crosswalk() below for a
## helper that fills them in from a biomass lookup table automatically.
##
## Nothing here is guessed: any source FG appearing in the diet matrix
## with NO crosswalk row is EXCLUDED and flagged in
## diet_crosswalk_UNMAPPED_REVIEW.csv - never silently dropped or
## silently kept as its own category. Any target predator whose
## converted diet no longer sums to 1 (because some of its prey got
## excluded as unmapped) is flagged in
## diet_mass_conservation_REVIEW.csv with the % of diet mass lost,
## before being renormalized back to 1.
## =================================================================

suppressPackageStartupMessages({
  library(data.table)
})

## ---- CONFIG - edit these for your run ---------------------------------
## Every knob below follows the same `if (!exists(...))` pattern as the
## rest of this pipeline's scripts (01_biomass.R etc): if you've already
## set a variable of that name (e.g. by assigning it before `source()`-
## ing this file, as a test harness or a wrapper script would), your
## value is kept; otherwise this default is used.
if (!exists("git_dir")) git_dir <- "."

## Wide CSV: first column = prey FG identifier (num or name), remaining
## column headers = predator FG identifiers, cell values = diet
## proportion (0-1). This is the same shape as an EwE "Diet
## composition" input / this pipeline's own Ecopath_diet sheet.
if (!exists("SOURCE_DIET_MATRIX_PATH")) {
  SOURCE_DIET_MATRIX_PATH <- file.path(git_dir, "data/diet/source_model_diet_matrix.csv")
}

## Long CSV, one row per source-FG -> target-FG mapping. Required
## columns: Source_FG_num, Source_FG_name, Target_FG_num, Target_FG_name.
## Optional columns: Split_Weight, Merge_Weight (see header comment
## above for what each means) - leave blank/omit to default to equal
## split/average, with a warning.
if (!exists("CROSSWALK_PATH")) {
  CROSSWALK_PATH <- file.path(git_dir, "data/diet/fg_crosswalk_source_to_target.csv")
}

## Optional: an FG biomass lookup (e.g. fg_index_regional_combined.csv
## or survey_fg_annual_index_regional_combined.csv from 01_biomass.R's
## output) used to replace the default equal-split/equal-merge weights
## with real biomass shares. Set to NULL to skip and just use equal
## weights everywhere (with the usual warning).
if (!exists("BIOMASS_WEIGHT_LOOKUP_PATH")) BIOMASS_WEIGHT_LOOKUP_PATH <- NULL

if (!exists("OUT_DIR")) {
  OUT_DIR <- if (exists("csv_out_dir")) csv_out_dir else file.path(git_dir, "output/diet")
}
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

## =================================================================
## STEP 1: read the source diet matrix (wide) and melt to long form.
## =================================================================
read_wide_diet_matrix <- function(path, prey_id_col = 1) {
  if (!file.exists(path)) stop("SOURCE_DIET_MATRIX_PATH not found: '", path, "'")
  wide <- as.data.table(fread(path))
  prey_col_name <- names(wide)[prey_id_col]
  predator_cols <- setdiff(names(wide), prey_col_name)
  long <- melt(wide, id.vars = prey_col_name, measure.vars = predator_cols,
              variable.name = "Predator_FG_id", value.name = "Proportion")
  setnames(long, prey_col_name, "Prey_FG_id")
  long[, Predator_FG_id := as.character(Predator_FG_id)]
  long[, Prey_FG_id := as.character(Prey_FG_id)]
  long[, Proportion := as.numeric(Proportion)]
  long <- long[!is.na(Proportion) & Proportion > 0]
  message("read_wide_diet_matrix(): ", nrow(wide), " prey FG(s) x ", length(predator_cols),
          " predator FG(s) in source matrix - ", nrow(long), " nonzero diet link(s) after melting.")
  long
}

## =================================================================
## STEP 2: load + validate the crosswalk, fill in default weights.
## =================================================================
load_fg_crosswalk <- function(path) {
  if (!file.exists(path)) stop("CROSSWALK_PATH not found: '", path, "'")
  cw <- as.data.table(fread(path, colClasses = "character"))
  required <- c("Source_FG_num", "Source_FG_name", "Target_FG_num", "Target_FG_name")
  missing_cols <- setdiff(required, names(cw))
  if (length(missing_cols) > 0) {
    stop("Crosswalk is missing required column(s): ", paste(missing_cols, collapse = ", "),
         ". Expected at least: ", paste(required, collapse = ", "))
  }
  if (!"Split_Weight" %in% names(cw)) cw[, Split_Weight := NA_character_]
  if (!"Merge_Weight" %in% names(cw)) cw[, Merge_Weight := NA_character_]
  cw[, Split_Weight := suppressWarnings(as.numeric(Split_Weight))]
  cw[, Merge_Weight := suppressWarnings(as.numeric(Merge_Weight))]

  ## default Split_Weight: equal share among all Target FGs that this
  ## Source FG maps to (1 for a pure 1-to-1 or pure-merge row, since
  ## then it only maps to one target).
  cw[, n_targets_for_source := .N, by = Source_FG_num]
  n_split_defaulted <- cw[is.na(Split_Weight), .N]
  cw[is.na(Split_Weight), Split_Weight := 1 / n_targets_for_source]
  cw[, Split_Weight := Split_Weight / sum(Split_Weight), by = Source_FG_num]  # renormalize even partial overrides

  ## default Merge_Weight: equal share among all Source FGs that feed
  ## into this Target FG (1 for a pure 1-to-1 or pure-split row).
  cw[, n_sources_for_target := .N, by = Target_FG_num]
  n_merge_defaulted <- cw[is.na(Merge_Weight), .N]
  cw[is.na(Merge_Weight), Merge_Weight := 1 / n_sources_for_target]
  cw[, Merge_Weight := Merge_Weight / sum(Merge_Weight), by = Target_FG_num]

  cw[, relationship := fifelse(n_targets_for_source > 1 & n_sources_for_target > 1, "split-and-merge",
                        fifelse(n_targets_for_source > 1, "split (1 source FG -> many targets)",
                        fifelse(n_sources_for_target > 1, "merge (many source FGs -> 1 target)",
                                "one-to-one")))]

  if (n_split_defaulted > 0) {
    message("load_fg_crosswalk(): ", n_split_defaulted, " row(s) had no Split_Weight - defaulted to an",
            " EQUAL split among that Source FG's target(s). Fill in real biomass shares if you have",
            " them (see apply_biomass_weights_to_crosswalk()).")
  }
  if (n_merge_defaulted > 0) {
    message("load_fg_crosswalk(): ", n_merge_defaulted, " row(s) had no Merge_Weight - defaulted to an",
            " EQUAL average among that Target FG's source(s). Fill in real biomass shares if you have",
            " them (see apply_biomass_weights_to_crosswalk()).")
  }
  message("load_fg_crosswalk(): ", nrow(cw), " mapping row(s) - ",
          cw[relationship == "one-to-one", .N], " one-to-one, ",
          cw[relationship == "split (1 source FG -> many targets)", .N], " split, ",
          cw[relationship == "merge (many source FGs -> 1 target)", .N], " merge, ",
          cw[relationship == "split-and-merge", .N], " split-and-merge.")
  cw
}

## Optional helper: overwrite the default equal Split_Weight/Merge_Weight
## with real biomass shares from a lookup table (expects columns
## FG_num and a biomass/density column - pass its name via biomass_col).
## Anything not resolvable from the lookup keeps its equal-weight default.
apply_biomass_weights_to_crosswalk <- function(crosswalk, biomass_lookup, biomass_col = "mean_density") {
  if (is.null(biomass_lookup)) return(crosswalk)
  bl <- as.data.table(biomass_lookup)[, .(FG_num = as.character(FG_num), Biomass = get(biomass_col))]
  bl <- bl[, .(Biomass = mean(Biomass, na.rm = TRUE)), by = FG_num]

  cw <- copy(crosswalk)
  cw <- merge(cw, bl, by.x = "Source_FG_num", by.y = "FG_num", all.x = TRUE)

  cw[!is.na(Biomass), Split_Weight := Biomass, by = Source_FG_num]
  cw[!is.na(Biomass) & n_targets_for_source > 1, Split_Weight := Biomass / sum(Biomass), by = Source_FG_num]
  cw[!is.na(Biomass) & n_sources_for_target > 1, Merge_Weight := Biomass / sum(Biomass), by = Target_FG_num]

  n_resolved <- cw[!is.na(Biomass) & (n_targets_for_source > 1 | n_sources_for_target > 1), .N]
  message("apply_biomass_weights_to_crosswalk(): replaced equal-weight defaults with real biomass",
          " shares for ", n_resolved, " split/merge row(s) (", biomass_col, " from the supplied lookup).")
  cw[, Biomass := NULL]
  cw
}

## =================================================================
## STEP 3: unmapped-FG check - never guess, always flag.
## =================================================================
check_unmapped_fgs <- function(diet_long, crosswalk, out_dir) {
  fgs_in_matrix <- unique(c(diet_long$Prey_FG_id, diet_long$Predator_FG_id))
  mapped_by_num  <- unique(crosswalk$Source_FG_num)
  mapped_by_name <- unique(crosswalk$Source_FG_name)
  unmapped <- setdiff(fgs_in_matrix, union(mapped_by_num, mapped_by_name))
  if (length(unmapped) > 0) {
    fwrite(data.table(Unmapped_Source_FG_id = unmapped), file.path(out_dir, "diet_crosswalk_UNMAPPED_REVIEW.csv"))
    message(length(unmapped), " source FG identifier(s) in the diet matrix have NO crosswalk entry",
            " (matched neither Source_FG_num nor Source_FG_name) - EXCLUDED from the converted matrix,",
            " not guessed. Written to diet_crosswalk_UNMAPPED_REVIEW.csv - add them to the crosswalk",
            " and re-run if they should be included:")
    print(unmapped)
  } else {
    message("check_unmapped_fgs(): every source FG in the diet matrix has a crosswalk entry.")
  }
  unmapped
}

## =================================================================
## STEP 4: prey-side aggregation - additive, mass-conserving.
## =================================================================
## Builds a single id -> target lookup from a crosswalk, accepting
## EITHER the Source_FG_num OR the Source_FG_name as the identifier
## used in the diet matrix (whichever the matrix actually uses) -
## collapsed to one candidate list per (id, target) pair so a source
## row where num and name happen to coincide is never double-counted.
build_id_lookup <- function(crosswalk, target_num_col, target_name_col, weight_col) {
  unique(rbindlist(list(
    crosswalk[, .(id = Source_FG_num, Target_num = get(target_num_col),
                  Target_name = get(target_name_col), Weight = get(weight_col))],
    crosswalk[, .(id = Source_FG_name, Target_num = get(target_num_col),
                  Target_name = get(target_name_col), Weight = get(weight_col))]
  )), by = c("id", "Target_num"))
}

apply_prey_crosswalk <- function(diet_long, crosswalk) {
  cw_prey <- build_id_lookup(crosswalk, "Target_FG_num", "Target_FG_name", "Split_Weight")
  setnames(cw_prey, c("Target_num", "Target_name", "Weight"),
          c("Target_Prey_FG_num", "Target_Prey_FG_name", "Split_Weight"))

  merged <- merge(diet_long, cw_prey, by.x = "Prey_FG_id", by.y = "id", allow.cartesian = TRUE)

  merged[, Contribution := Proportion * Split_Weight]
  merged[, .(Contribution = sum(Contribution, na.rm = TRUE)),
         by = .(Target_Prey_FG_num, Target_Prey_FG_name, Predator_FG_id)]
}

## =================================================================
## STEP 5: predator-side aggregation - weighted average (merges get
## diluted by relative weight; pure splits get a full, undiminished
## copy automatically, because Merge_Weight = 1 whenever only one
## source predator feeds a given target).
## =================================================================
apply_predator_crosswalk <- function(prey_aggregated, crosswalk) {
  cw_pred <- build_id_lookup(crosswalk, "Target_FG_num", "Target_FG_name", "Merge_Weight")
  setnames(cw_pred, c("Target_num", "Target_name", "Weight"),
          c("Target_Predator_FG_num", "Target_Predator_FG_name", "Merge_Weight"))

  merged <- merge(prey_aggregated, cw_pred, by.x = "Predator_FG_id", by.y = "id", allow.cartesian = TRUE)

  merged[, Weighted_Contribution := Contribution * Merge_Weight]
  merged[, .(Proportion = sum(Weighted_Contribution, na.rm = TRUE)),
         by = .(Target_Predator_FG_num, Target_Predator_FG_name, Target_Prey_FG_num, Target_Prey_FG_name)]
}

## =================================================================
## STEP 6: mass-conservation check + renormalize each target
## predator's column back to summing to 1 (any shortfall means some of
## that predator's original diet mass fell on an unmapped prey FG -
## flagged, not silently absorbed).
## =================================================================
renormalize_and_flag <- function(target_diet_long, out_dir, tolerance_pct = 1) {
  target_diet_long[, col_sum := sum(Proportion, na.rm = TRUE), by = Target_Predator_FG_num]
  flagged <- unique(target_diet_long[abs(1 - col_sum) * 100 > tolerance_pct,
                                     .(Target_Predator_FG_num, Target_Predator_FG_name, col_sum,
                                       Diet_mass_lost_pct = round((1 - col_sum) * 100, 2))])
  if (nrow(flagged) > 0) {
    fwrite(flagged, file.path(out_dir, "diet_mass_conservation_REVIEW.csv"))
    message(nrow(flagged), " target predator(s) lost more than ", tolerance_pct, "% of their original diet",
            " mass (their surviving prey items no longer sum to 1 - almost always because some of their",
            " original prey FG(s) were unmapped/excluded, see diet_crosswalk_UNMAPPED_REVIEW.csv).",
            " Renormalized to sum to 1 anyway so the output matrix is usable, but REVIEW this file",
            " before trusting these predators' diets:")
    print(flagged)
  } else {
    message("renormalize_and_flag(): every target predator's diet sums to 1 within ", tolerance_pct,
            "% (no material mass lost to unmapped prey).")
  }
  target_diet_long[col_sum > 0, Proportion := Proportion / col_sum]
  target_diet_long[, col_sum := NULL]
  target_diet_long
}

## =================================================================
## RUN
## =================================================================
diet_long <- read_wide_diet_matrix(SOURCE_DIET_MATRIX_PATH)
crosswalk <- load_fg_crosswalk(CROSSWALK_PATH)

if (!is.null(BIOMASS_WEIGHT_LOOKUP_PATH) && file.exists(BIOMASS_WEIGHT_LOOKUP_PATH)) {
  biomass_lookup <- fread(BIOMASS_WEIGHT_LOOKUP_PATH)
  crosswalk <- apply_biomass_weights_to_crosswalk(crosswalk, biomass_lookup)
}

check_unmapped_fgs(diet_long, crosswalk, OUT_DIR)

prey_aggregated   <- apply_prey_crosswalk(diet_long, crosswalk)
target_diet_long  <- apply_predator_crosswalk(prey_aggregated, crosswalk)
target_diet_long  <- renormalize_and_flag(target_diet_long, OUT_DIR)

setorder(target_diet_long, Target_Predator_FG_num, -Proportion)
fwrite(target_diet_long, file.path(OUT_DIR, "target_diet_matrix_long.csv"))

target_diet_wide <- dcast(target_diet_long,
                          Target_Prey_FG_num + Target_Prey_FG_name ~ Target_Predator_FG_name,
                          value.var = "Proportion", fill = 0)
setorder(target_diet_wide, Target_Prey_FG_num)
fwrite(target_diet_wide, file.path(OUT_DIR, "target_diet_matrix_wide.csv"))

crosswalk_summary <- unique(crosswalk[, .(Source_FG_num, Source_FG_name, Target_FG_num, Target_FG_name,
                                          Split_Weight = round(Split_Weight, 4),
                                          Merge_Weight = round(Merge_Weight, 4), relationship)])
setorder(crosswalk_summary, Target_FG_num, Source_FG_num)
fwrite(crosswalk_summary, file.path(OUT_DIR, "diet_crosswalk_relationship_summary.csv"))

message("\nDone. Converted diet matrix (", uniqueN(target_diet_long$Target_Predator_FG_num), " target predator FG(s) x ",
        uniqueN(target_diet_long$Target_Prey_FG_num), " target prey FG(s)) written to:\n",
        "  - ", file.path(OUT_DIR, "target_diet_matrix_long.csv"), "\n",
        "  - ", file.path(OUT_DIR, "target_diet_matrix_wide.csv"), " (Ecopath_diet-shaped: prey rows x predator columns)\n",
        "Review files: diet_crosswalk_UNMAPPED_REVIEW.csv (if any), diet_mass_conservation_REVIEW.csv (if any),\n",
        "  diet_crosswalk_relationship_summary.csv (every split/merge and the weight actually used).")
