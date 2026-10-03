## =================================================================
## lib_aquamaps_depth_extension.R - LIBRARY FILE, not a pipeline step.
## Sourced (opt-in, see APPLY_AQUAMAPS_DEPTH_ADJUSTMENT) by
## 01_survey_density_westmed.R / 01_survey_density_custom.R, right
## after Step 6/7's REAL strata-weighted fg_index/species_density_
## regional are built from actual MEDITS samples - not a replacement
## for that, an EXTENSION on top of it.
##
## WHY THIS EXISTS
##
## MEDITS' own bathymetric design only samples 10-800m (MEDITS_STRATA
## in 01_survey_density_westmed.R - 5 strata, 10-800m). Depths
## shallower than 10m (coastal/nursery habitat for many species) and
## deeper than 800m (bathyal) are structurally never sampled by this
## survey - not "sampled and found empty", genuinely never trawled.
## compute_strata_area_by_area()'s own "prop" (each stratum's share of
## the study area) is normalized ACROSS ONLY THOSE 5 STRATA, so the
## existing Biomass/density figures already implicitly treat the whole
## modeled area as if it were entirely 10-800m seafloor - any species
## with real biomass in the <10m or >800m band is invisible to this
## pipeline's numbers, not just imprecisely measured.
##
## THE ADJUSTMENT, AND ITS CORE ASSUMPTION (stated plainly, not
## silently absorbed): AquaMaps (aquamaps.org) publishes, per species,
## a depth ENVELOPE - DepthMin/DepthPrefMin/DepthPrefMax/DepthMax - a
## trapezoidal relative-suitability curve over depth (flat P=1 between
## DepthPrefMin/DepthPrefMax, ramping linearly to P=0 at DepthMin/
## DepthMax; see AquaMaps' own "Algorithm and Data Sources" document).
## This script assumes a species' RELATIVE density across depth is
## proportional to that curve - i.e. if AquaMaps' envelope says a
## species has the SAME suitability level in a pseudo-stratum as in the
## one REAL MEDITS stratum immediately adjacent to it, this script
## assumes the species' real density in the pseudo-stratum equals
## whatever density MEDITS actually measured in that adjacent stratum
## (not some area-wide average across all 5 real strata - see MECHANISM
## below for why adjacency, specifically, is what's used). This is a
## genuine approximation (occurrence suitability is not the same thing
## as density), standard in depth-range-extrapolation work when direct
## sampling doesn't cover the species' full depth range, but an
## approximation nonetheless - every multiplier this produces is
## written to an audit sheet (AquaMaps_Depth_Adjustment) specifically
## so it can be reviewed/questioned per species, not trusted blindly.
##
## MECHANISM: rather than a flat per-species rescaling, this builds
## genuine EXTRA strata - one shallow (0-10m) plus however many deep
## sub-bands are passed in deep_ranges (default: 800-1000m and
## 1000-2850m, splitting what used to be a single 800-6000m lump into
## two - 2850m being a practical ceiling near the western Mediterranean's
## real deepest points, rather than the Mediterranean-wide ~5267m Calypso
## Deep, which is in the Ionian/eastern basin, not the western GSAs this
## pipeline covers) - each with its own real bathymetric area (via the
## SAME compute_strata_area_by_area() already used for the 5 real strata
## - no new area-calculation logic) and a density DERIVED (not measured):
## each pseudo-stratum is anchored to the ONE real MEDITS stratum
## immediately next to it - the shallowest real stratum for the shallow
## pseudo-band, the deepest real stratum for every deep pseudo-band - and
## its density is that anchor stratum's own MEASURED density x the ratio
## of AquaMaps mean suitability between the pseudo-band and that SAME
## anchor stratum's own depth range (NOT a whole-range 10-800m composite
## average, which would dilute a species' extrapolated density with
## however much of the survey range was actually low-suitability for
## it). A species at equal suitability in both bands gets a pseudo-
## density that exactly equals what was measured next door. These
## pseudo-strata are then folded into the SAME weight_by_strata() /
## weight_species_by_area() call used for the real strata - not a
## separate formula - so the final numbers come from one consistent
## area-weighted sum across all strata, real and derived alike (5 real +
## 1 shallow + however many deep_ranges).
##
## DATA SOURCE: the `aquamapsdata` R package (github.com/raquamaps/
## aquamapsdata) - the only verified way to get AquaMaps' per-species
## HSPEN depth-envelope table without scraping. Its download_db() pulls
## a ~2GB (~10GB unpacked) local SQLite database ONCE; every run after
## that reads the local copy, no further network needed. This is a
## real, deliberate cost - confirmed acceptable before building this.
## =================================================================

suppressPackageStartupMessages({
  library(data.table)
})

## =================================================================
## STEP A: one-time local AquaMaps database setup
## =================================================================
ensure_aquamaps_db <- function() {
  if (!requireNamespace("aquamapsdata", quietly = TRUE)) {
    message("[AquaMaps] 'aquamapsdata' package not installed - installing from GitHub",
            " (raquamaps/aquamapsdata)...")
    if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
    devtools::install_github("raquamaps/aquamapsdata", dependencies = TRUE)
  }
  suppressPackageStartupMessages(library(aquamapsdata))
  ## force = FALSE (never TRUE here) - this is a ~2GB/~10GB-unpacked
  ## ONE-TIME download. IMPORTANT: download_db(force = FALSE) does NOT
  ## silently skip when a db already exists - it STOPS with "An existing
  ## db exists at <path>, to overwrite, pls rerun with force = TRUE".
  ## That's exactly the desired outcome (never re-download, never
  ## overwrite an existing db just because this ran again) - it just
  ## surfaces as an error rather than a no-op, so it has to be caught
  ## and treated as success here, not left to propagate and abort the
  ## whole pipeline run on every single re-run after the first.
  tryCatch({
    aquamapsdata::download_db(force = FALSE)
  }, error = function(e) {
    if (grepl("existing db exists", conditionMessage(e), ignore.case = TRUE)) {
      message("[AquaMaps] Local database already downloaded - using the existing copy.")
    } else {
      stop(e)  # a genuinely different failure (disk space, network, etc.) - don't swallow it
    }
  })
  aquamapsdata::default_db("sqlite")
  
  ## Sanity-check the connection with a trivial real query BEFORE
  ## resolve_aquamaps_species_ids() runs 491 (or however many) separate
  ## per-species queries against it. A file existing at am_db_sqlite()'s
  ## path (which is all download_db()'s "existing db" check above
  ## actually verifies) does NOT guarantee it's a complete, valid
  ## database - an earlier interrupted/partial download leaves a file
  ## there too. Without this check, a corrupt/partial db makes EVERY
  ## single species query fail identically, and each failure was being
  ## silently caught and reported as an ordinary per-species "no
  ## AquaMaps match" - which is how "491 of 491 species unmatched"
  ## happened: not 491 real misses, one systemic connection failure
  ## reported 491 times with no clue what actually went wrong.
  sanity <- tryCatch(nrow(dplyr::collect(head(aquamapsdata::am_hspen(), 1))),
                     error = function(e) e)
  if (inherits(sanity, "error") || is.null(sanity) || length(sanity) == 0 || sanity == 0) {
    db_path <- tryCatch(aquamapsdata::am_db_sqlite(), error = function(e) "<unknown - am_db_sqlite() itself failed>")
    stop("[AquaMaps] The local database at '", db_path, "' exists but a test query against it",
         " (am_hspen()) ", if (inherits(sanity, "error")) paste0("failed: ", conditionMessage(sanity)) else "returned 0 rows",
         " - it's very likely corrupt or an incomplete download, not a real 'no species match' situation.",
         " Delete that file and re-run with aquamapsdata::download_db(force = TRUE) to get a clean copy",
         " (or delete it and let ensure_aquamaps_db() download fresh next run).")
  }
  message("[AquaMaps] Database connection verified with a live test query.")
  invisible(TRUE)
}

## =================================================================
## STEP B: ScientificName -> AquaMaps SpeciesID resolution
## =================================================================
## Same qualifier-stripping idea as lib_worms_taxonomy_lookup.R's own
## search_terms cleanup (strip trailing/embedded spp./cf./aff.), kept
## as its own small copy here rather than a cross-file dependency,
## since AquaMaps' own Genus/Species split needs the FIRST TWO tokens
## specifically (subspecies as a third token would break
## am_search_exact()'s exact Genus/Species match).
split_scientific_name <- function(scientific_name) {
  cleaned <- stringr::str_squish(stringr::str_remove_all(
    scientific_name, "(?i)\\b(spp?|cf|aff)\\.?(?=\\s|$)"))
  parts <- strsplit(cleaned, "\\s+")[[1]]
  list(genus = if (length(parts) >= 1) parts[1] else NA_character_,
       species = if (length(parts) >= 2) parts[2] else NA_character_)
}

#' Resolve a vector of ScientificNames to AquaMaps SpeciesIDs.
#' Exact Genus/Species match first (am_search_exact); if that finds
#' nothing, falls back to a fuzzy full-text search on the whole name
#' (am_search_fuzzy) - same two-tier idea as this pipeline's other
#' species-matching cascades, never a silent single-shot lookup.
#' Genuinely unmatched species get SpeciesID = NA, match_method =
#' "unmatched" - flagged, not dropped, so they're visible in the audit
#' sheet rather than silently missing no shallow/deep adjustment.
resolve_aquamaps_species_ids <- function(scientific_names) {
  scientific_names <- unique(scientific_names[!is.na(scientific_names) & scientific_names != ""])
  results <- vector("list", length(scientific_names))
  ## Captured once, not per-species: if am_search_exact()/am_search_fuzzy()
  ## are erroring (a bad connection, an API mismatch, whatever) every
  ## single call fails identically - printing that same error 491 times
  ## would be useless noise, but silently swallowing all 491 (the
  ## previous behavior) is worse: it looked exactly like "genuinely no
  ## AquaMaps match", which is how a systemic connection failure got
  ## reported as "491 of 491 species unmatched" with no diagnostic at
  ## all. The actual error text is now shown ONCE, the first time either
  ## function throws.
  first_error_shown <- FALSE
  show_first_error_once <- function(e) {
    if (!first_error_shown) {
      message("[AquaMaps] First species-lookup error (shown once - if this repeats for every",
              " species, it's a systemic problem, not 491 individual real misses): ", conditionMessage(e))
      first_error_shown <<- TRUE
    }
    NULL
  }
  for (i in seq_along(scientific_names)) {
    nm <- scientific_names[i]
    parts <- split_scientific_name(nm)
    sp_id <- NA_character_
    method <- "unmatched"
    ## MUST call via do.call(), not a normal `f(Genus = x, ...)` call -
    ## confirmed from aquamapsdata's own source (R/data.R):
    ## am_search_exact() grabs its arguments UNEVALUATED via
    ## match.call(), then does eval() on each captured expression with
    ## no envir specified. eval()'s default envir is the frame that
    ## CALLED eval() - i.e. somewhere inside am_search_exact()'s own
    ## call stack - never the environment this function (resolve_
    ## aquamaps_species_ids()) is running in. So if we call it the
    ## normal way with `Genus = genus_val`, match.call() captures the
    ## bare symbol `genus_val`, and their eval() then looks for a
    ## variable called genus_val INSIDE THEIR OWN function's frame,
    ## where it obviously doesn't exist - this is exactly what produced
    ## the real error ("In index: 1, With name: Genus ... object
    ## 'parts' not found", back when the expression passed was
    ## literally `parts$genus`). am_search_exact() only works when
    ## called with LITERAL values written directly at the call site
    ## (e.g. the package's own vignette example, Genus = "Caranx") -
    ## which is exactly what do.call() constructs: it embeds the ACTUAL
    ## VALUE into the call rather than a variable reference, so there's
    ## no symbol left for their eval() to mis-resolve. Verified against
    ## a reproduction of their exact match.call()+eval() pattern before
    ## shipping this fix - a plain call fails from inside a function
    ## frame exactly like this one, do.call() doesn't.
    if (!is.na(parts$genus) && !is.na(parts$species)) {
      hit <- tryCatch(do.call(aquamapsdata::am_search_exact, list(Genus = parts$genus, Species = parts$species)),
                      error = show_first_error_once)
      if (!is.null(hit) && nrow(hit) > 0) {
        sp_id <- as.character(hit$SpeciesID[1])
        method <- "exact"
      }
    }
    if (is.na(sp_id)) {
      ## am_search_fuzzy(search_term) doesn't use match.call()+eval()
      ## internally (it's a plain sprintf() into a SQL query - see
      ## aquamapsdata's own source), so it isn't subject to the same
      ## bug and a normal call is fine here.
      hit <- tryCatch(aquamapsdata::am_search_fuzzy(search_term = nm), error = show_first_error_once)
      if (!is.null(hit) && nrow(hit) > 0) {
        sp_id <- as.character(hit$SpeciesID[1])
        method <- "fuzzy"
      }
    }
    results[[i]] <- data.table(ScientificName = nm, SpeciesID = sp_id, match_method = method)
  }
  out <- rbindlist(results)
  n_unmatched <- out[match_method == "unmatched", .N]
  if (n_unmatched > 0) {
    message("[AquaMaps] ", n_unmatched, " of ", nrow(out), " species had no AquaMaps match",
            " (exact or fuzzy) - these get NO shallow/deep depth adjustment, flagged as",
            " 'unmatched' in the audit sheet, not silently zero-adjusted.",
            if (n_unmatched == nrow(out)) paste0(
              " ALL species failed - if the error shown above (if any) doesn't explain why,",
              " double-check the database connection (ensure_aquamaps_db()'s own sanity check",
              " should have caught a corrupt/empty database before getting here).") else "")
  }
  out
}

## =================================================================
## STEP C: fetch each matched species' depth envelope (am_hspen())
## =================================================================
fetch_aquamaps_depth_envelope <- function(species_id_lookup) {
  matched_ids <- species_id_lookup[!is.na(SpeciesID), SpeciesID]
  if (length(matched_ids) == 0) {
    envelope <- data.table(SpeciesID = character(0), DepthMin = numeric(0),
                           DepthPrefMin = numeric(0), DepthPrefMax = numeric(0), DepthMax = numeric(0))
  } else {
    hspen <- aquamapsdata::am_hspen()
    envelope <- as.data.table(
      dplyr::collect(dplyr::filter(hspen, SpeciesID %in% matched_ids))
    )[, .(SpeciesID = as.character(SpeciesID), DepthMin, DepthPrefMin, DepthPrefMax, DepthMax)]
  }
  out <- merge(species_id_lookup, envelope, by = "SpeciesID", all.x = TRUE)
  n_no_envelope <- out[!is.na(SpeciesID) & is.na(DepthMin), .N]
  if (n_no_envelope > 0) {
    message("[AquaMaps] ", n_no_envelope, " matched species have no depth envelope in am_hspen()",
            " (HSPEN depth fields blank for that SpeciesID) - also flagged, not adjusted.")
  }
  out
}

## =================================================================
## STEP D: AquaMaps' own trapezoidal depth-suitability shape
## (per AquaMaps' "Algorithm and Data Sources" documentation: P=0
## below DepthMin, ramps linearly 0->1 between DepthMin and
## DepthPrefMin, flat P=1 between DepthPrefMin and DepthPrefMax, ramps
## linearly 1->0 between DepthPrefMax and DepthMax, P=0 beyond DepthMax)
## =================================================================
aquamaps_envelope_value <- function(depth, DepthMin, DepthPrefMin, DepthPrefMax, DepthMax) {
  p <- rep(0, length(depth))
  ## flat top
  p[depth >= DepthPrefMin & depth <= DepthPrefMax] <- 1
  ## rising ramp (DepthMin -> DepthPrefMin) - guard against a
  ## degenerate/zero-width ramp (DepthMin == DepthPrefMin) dividing by 0
  rising <- depth > DepthMin & depth < DepthPrefMin
  if (any(rising)) {
    span <- DepthPrefMin - DepthMin
    p[rising] <- if (span > 0) (depth[rising] - DepthMin) / span else 1
  }
  ## falling ramp (DepthPrefMax -> DepthMax)
  falling <- depth > DepthPrefMax & depth < DepthMax
  if (any(falling)) {
    span <- DepthMax - DepthPrefMax
    p[falling] <- if (span > 0) (DepthMax - depth[falling]) / span else 1
  }
  p
}

#' Probability MASS (area under the envelope curve) within
#' [depth_from, depth_to] - integrated numerically on a fine depth
#' grid rather than solved algebraically, so edge cases (a requested
#' interval only partially overlapping a ramp, a zero-width preferred
#' range, an interval entirely outside DepthMin/DepthMax, missing
#' envelope values) don't need to be enumerated by hand. NA envelope
#' inputs propagate to NA (never silently treated as zero).
aquamaps_envelope_mass <- function(depth_from, depth_to, DepthMin, DepthPrefMin, DepthPrefMax, DepthMax,
                                   resolution_m = 1) {
  if (any(is.na(c(DepthMin, DepthPrefMin, DepthPrefMax, DepthMax)))) return(NA_real_)
  if (depth_to <= depth_from) return(0)
  d <- seq(depth_from, depth_to, by = resolution_m)
  if (d[length(d)] < depth_to) d <- c(d, depth_to)  # make sure the interval's right edge is included
  p <- aquamaps_envelope_value(d, DepthMin, DepthPrefMin, DepthPrefMax, DepthMax)
  sum((p[-1] + p[-length(p)]) / 2 * diff(d))  # trapezoidal numerical integration
}

## =================================================================
## STEP E: per-species shallow/deep multipliers, each relative to the
## SPECIFIC adjacent REAL stratum used to anchor that pseudo-stratum -
## NOT the whole 10-800m composite. See STEP F/G below for how these
## get applied to that anchor stratum's own MEASURED density.
## =================================================================
#' envelope_dt: output of fetch_aquamaps_depth_envelope() (one row per
#' ScientificName, with SpeciesID/match_method/Depth* columns, possibly
#' NA where unmatched or no envelope).
#'
#' THE DESIGN, AND WHY: the shallow pseudo-stratum (e.g. 0-10m) sits
#' immediately next to the shallowest REAL MEDITS stratum (e.g. 10-50m);
#' the deep pseudo-strata (e.g. 800-1000m, 1000-2850m) sit immediately
#' beyond the deepest REAL stratum (e.g. 500-800m). Rather than scaling
#' from a single area-weighted density averaged across ALL 5 real strata
#' (which dilutes a species' density with however much of its measured
#' range was actually low-suitability for it), each pseudo-stratum's
#' density is derived from the ONE real stratum it's actually adjacent
#' to: pseudo_density = (that adjacent stratum's own MEASURED density) x
#' (AquaMaps mean-suitability ratio between the pseudo-band and that
#' SAME adjacent stratum's own depth range). A species at the SAME
#' AquaMaps suitability level in both the pseudo-band and its adjacent
#' real stratum gets multiplier = 1 exactly - i.e. the pseudo-stratum's
#' derived density literally equals whatever was measured next door,
#' with no extrapolation distortion - and only diverges from 1 to the
#' extent AquaMaps' own envelope says suitability actually differs
#' between the two adjacent bands. anchor_shallow_range/anchor_deep_range
#' (passed in by apply_aquamaps_depth_adjustment(), derived from
#' medits_strata_def) are that adjacent real stratum's own depth_min/
#' depth_max - NOT the full 10-800m span.
#'
#' IMPORTANT ON UNITS: depth_envelope_mass_* below is NOT biomass, NOT
#' density, and has no biological unit at all - it's the numerical
#' integral (STEP D's aquamaps_envelope_mass()) of AquaMaps' DIMENSIONLESS
#' 0-1 suitability curve over a depth interval measured in metres, so its
#' "unit" is just metres (probability x depth). It only exists as an
#' intermediate for computing multiplier_shallow/multiplier_deep_* below,
#' which divide by each range's own WIDTH before taking a ratio (mean
#' suitability LEVEL, not raw accumulated mass) - dividing two raw masses
#' directly would conflate suitability level with each range's width,
#' understating a pseudo-band's multiplier purely because it's narrower
#' than its anchor stratum, independent of actual suitability.
#'
#' Species with zero AquaMaps suitability mass in the relevant anchor
#' stratum get NA multipliers there, not Inf/huge ones - a ratio against
#' a near-zero reference is unstable, not a real extrapolation; these
#' stay flagged in the audit sheet's note column rather than silently
#' producing an enormous, meaningless multiplier.
#'
#' deep_ranges: a LIST of c(depth_from, depth_to) pairs (one shallow
#' range still, but any number of deep sub-bands, e.g.
#' list(c(800, 1000), c(1000, 2850))) - ALL deep sub-bands anchor to the
#' SAME deepest real stratum (anchor_deep_range), not chained through
#' each other. Each gets its own depth_envelope_mass_deep_<from>_<to> and
#' multiplier_deep_<from>_<to> column, so the audit sheet shows each deep
#' sub-band's contribution separately. The generated multiplier_deep_*
#' column names are returned as the "deep_mult_cols" attribute on the
#' result, for downstream functions to pick up without having to
#' re-derive them from deep_ranges themselves.
build_aquamaps_depth_multipliers <- function(envelope_dt, anchor_shallow_range, anchor_deep_range,
                                             shallow_range, deep_ranges, resolution_m = 1) {
  out <- copy(envelope_dt)
  width_anchor_shallow <- anchor_shallow_range[2] - anchor_shallow_range[1]
  width_anchor_deep <- anchor_deep_range[2] - anchor_deep_range[1]
  width_shallow <- shallow_range[2] - shallow_range[1]
  
  out[, depth_envelope_mass_anchor_shallow := mapply(aquamaps_envelope_mass, anchor_shallow_range[1], anchor_shallow_range[2],
                                                     DepthMin, DepthPrefMin, DepthPrefMax, DepthMax,
                                                     MoreArgs = list(resolution_m = resolution_m))]
  out[, depth_envelope_mass_anchor_deep := mapply(aquamaps_envelope_mass, anchor_deep_range[1], anchor_deep_range[2],
                                                  DepthMin, DepthPrefMin, DepthPrefMax, DepthMax,
                                                  MoreArgs = list(resolution_m = resolution_m))]
  out[, depth_envelope_mass_shallow_range := mapply(aquamaps_envelope_mass, shallow_range[1], shallow_range[2],
                                                    DepthMin, DepthPrefMin, DepthPrefMax, DepthMax,
                                                    MoreArgs = list(resolution_m = resolution_m))]
  ## Ratio of MEAN suitability levels (mass / own width) between the
  ## pseudo-band and its OWN adjacent anchor stratum - see this
  ## function's header comment for the full reasoning.
  out[, multiplier_shallow := ifelse(!is.na(depth_envelope_mass_anchor_shallow) & depth_envelope_mass_anchor_shallow > 0,
                                     (depth_envelope_mass_shallow_range / width_shallow) /
                                       (depth_envelope_mass_anchor_shallow / width_anchor_shallow), NA_real_)]
  
  deep_mult_cols <- character(length(deep_ranges))
  for (i in seq_along(deep_ranges)) {
    dr <- deep_ranges[[i]]
    width_deep <- dr[2] - dr[1]
    suffix <- paste0(dr[1], "_", dr[2])
    mass_col <- paste0("depth_envelope_mass_deep_", suffix)
    mult_col <- paste0("multiplier_deep_", suffix)
    deep_mult_cols[i] <- mult_col
    out[, (mass_col) := mapply(aquamaps_envelope_mass, dr[1], dr[2],
                               DepthMin, DepthPrefMin, DepthPrefMax, DepthMax,
                               MoreArgs = list(resolution_m = resolution_m))]
    out[, (mult_col) := ifelse(!is.na(depth_envelope_mass_anchor_deep) & depth_envelope_mass_anchor_deep > 0,
                               (get(mass_col) / width_deep) / (depth_envelope_mass_anchor_deep / width_anchor_deep),
                               NA_real_)]
  }
  
  ## A rough COMBINED indicator only (not used in the actual density
  ## calculation - build_pseudo_strata_density() applies each multiplier_*
  ## to its OWN anchor stratum's density separately): 1 (the reference
  ## itself) plus every band's own density-level multiplier summed
  ## together. A species at or near full preferred suitability in a
  ## pseudo-band can show multiplier_shallow or multiplier_deep_* well
  ## above OR below 1 depending on how that band's suitability compares
  ## to its specific adjacent anchor stratum - so this combined figure
  ## can run well past 1 for such species, which is expected, not a red
  ## flag on its own; treat the per-band multiplier_* columns as the
  ## meaningful ones, this as a quick summary. NOTE this is NOT the same
  ## as the sheet's net_density_multiplier column (added in
  ## 01_survey_density_westmed.R) - that one captures the REAL final
  ## effect on fg_index/species_density_regional, which can go either
  ## way even when this number is large, because folding pseudo-strata
  ## AREA into the total also shrinks every real stratum's own area-
  ## proportion (see that script's own comment on this).
  all_mult_cols <- c("multiplier_shallow", deep_mult_cols)
  out[, depth_extrapolation_multiplier := {
    vals <- lapply(all_mult_cols, function(cn) get(cn))
    any_na <- Reduce(`|`, lapply(vals, is.na))
    total <- Reduce(`+`, vals)
    ifelse(any_na, NA_real_, 1 + total)
  }]
  out[, note := fifelse(match_method == "unmatched", "not adjusted (no AquaMaps match)",
                        fifelse(is.na(DepthMin), "not adjusted (no depth envelope in am_hspen())",
                                fifelse((is.na(depth_envelope_mass_anchor_shallow) | depth_envelope_mass_anchor_shallow == 0) &
                                          (is.na(depth_envelope_mass_anchor_deep) | depth_envelope_mass_anchor_deep == 0),
                                        "not adjusted (no AquaMaps suitability in either adjacent real anchor stratum)",
                                        fifelse(is.na(depth_envelope_mass_anchor_shallow) | depth_envelope_mass_anchor_shallow == 0,
                                                "shallow not adjusted (no AquaMaps suitability in the shallow anchor stratum) - deep may still be",
                                                fifelse(is.na(depth_envelope_mass_anchor_deep) | depth_envelope_mass_anchor_deep == 0,
                                                        "deep not adjusted (no AquaMaps suitability in the deep anchor stratum) - shallow may still be",
                                                        "adjusted")))))]
  setattr(out, "deep_mult_cols", deep_mult_cols)
  out
}

## =================================================================
## STEP F: species' own MEASURED density in ONE SPECIFIC real stratum -
## the raw per-sample density (density_sum / n_samples, NOT area-
## weighted across strata) for that (AreaID, Year, Species) in exactly
## the adjacent real stratum used as a pseudo-stratum's extrapolation
## anchor (see STEP E's header comment for why a single adjacent
## stratum, not an area-weighted composite across all 5 real strata).
## Kept per AreaID (not collapsed) since the pseudo-strata below need a
## per-AreaID starting point to scale from, same as every other
## per-AreaID density table in this file.
##
## TRADE-OFF worth knowing: a species with real catch elsewhere in the
## 10-800m range but NONE specifically in this one anchor stratum gets
## no anchor density here, so its pseudo-stratum comes out NA/excluded
## for that AreaID - more species end up "not adjusted" than under a
## whole-range composite reference, but what DOES get adjusted is
## anchored to real, geographically-adjacent evidence rather than
## diluted by that species' own abundance in unrelated, non-adjacent
## strata.
## =================================================================
compute_species_anchor_stratum_density <- function(per_group_sp, n_samples_by_stratum, anchor_stratum_num) {
  sp_at_anchor <- per_group_sp[Stratum == anchor_stratum_num]
  samples_at_anchor <- n_samples_by_stratum[Stratum == anchor_stratum_num, .(AreaID, Year, Stratum, n_samples)]
  merged <- merge(sp_at_anchor, samples_at_anchor, by = c("AreaID", "Year", "Stratum"), all.x = TRUE)
  merged <- merged[!is.na(n_samples) & n_samples > 0]
  merged[, anchor_density := density_sum / n_samples]
  merged[, .(AreaID, Year, FG_num, FG_name, ScientificName, anchor_density)]
}

## =================================================================
## STEP G: build the shallow + N-deep pseudo-strata's species- and
## FG-level density tables, shaped to plug straight into
## weight_by_strata()/weight_species_by_area() alongside the real strata.
## =================================================================
#' shallow_anchor_density / deep_anchor_density: output of
#' compute_species_anchor_stratum_density() for the shallowest and
#' deepest real strata respectively - the shallow pseudo-stratum scales
#' from shallow_anchor_density, ALL deep pseudo-strata scale from the
#' SAME deep_anchor_density (they don't chain off each other).
#' deep_stratum_nums / deep_mult_cols: parallel vectors (same length,
#' same order) - deep_stratum_nums[i] is the stratum_num to tag rows
#' built from multiplier column deep_mult_cols[i].
build_pseudo_strata_density <- function(shallow_anchor_density, deep_anchor_density, multipliers,
                                        shallow_stratum_num, deep_stratum_nums, deep_mult_cols) {
  ## NOTE the nrow(d) == 0 guard below in every branch: data.table
  ## recycles a scalar j-expression (like `Stratum = shallow_stratum_num`)
  ## as if it were the "longest item" when every OTHER column comes out
  ## length-0 from an empty input - i.e. d[, .(col1, col2, Stratum = 6L)]
  ## on a 0-row `d` silently produces ONE phantom row of NAs with
  ## Stratum = 6, not the expected 0 rows. Confirmed via direct
  ## reproduction before shipping this - without the guard, a species (or
  ## every species) with no usable multiplier for a given band leaves a
  ## ghost row in species_level/fg_level with real Stratum tags but NA
  ## density, silently polluting downstream sums.
  empty_pseudo_row <- function() {
    data.table(AreaID = integer(0), Year = integer(0), FG_num = integer(0), FG_name = character(0),
               ScientificName = character(0), density_sum = numeric(0), Stratum = integer(0))
  }
  
  m_shallow <- merge(shallow_anchor_density, multipliers[, .(ScientificName, multiplier_shallow)], by = "ScientificName")
  m_shallow <- m_shallow[!is.na(multiplier_shallow)]
  shallow <- if (nrow(m_shallow) == 0) empty_pseudo_row() else m_shallow[
    , .(AreaID, Year, FG_num, FG_name, ScientificName,
        density_sum = anchor_density * multiplier_shallow, Stratum = shallow_stratum_num)]
  
  deep_keep_cols <- c("ScientificName", deep_mult_cols)
  m_deep <- merge(deep_anchor_density, multipliers[, ..deep_keep_cols], by = "ScientificName")
  deep_parts <- lapply(seq_along(deep_mult_cols), function(i) {
    col <- deep_mult_cols[i]
    d <- m_deep[!is.na(get(col))]
    if (nrow(d) == 0) return(empty_pseudo_row())
    d[, .(AreaID, Year, FG_num, FG_name, ScientificName,
          density_sum = anchor_density * get(col), Stratum = deep_stratum_nums[i])]
  })
  
  if (nrow(shallow) == 0 && sum(vapply(deep_parts, nrow, integer(1))) == 0) {
    message("[AquaMaps] No species had both a usable depth-envelope multiplier AND a measured density",
            " in the relevant adjacent real anchor stratum - pseudo-strata are empty this run",
            " (check the audit sheet's 'note' column for why).")
  }
  
  species_level <- rbind(shallow, rbindlist(deep_parts))
  fg_level <- species_level[, .(density_sum = sum(density_sum, na.rm = TRUE)),
                            by = .(AreaID, Year, Stratum, FG_num, FG_name)]
  ## n_samples = 1 for the pseudo-strata: weight_by_strata()'s formula is
  ## (density_sum / n_samples) * prop - these density_sum values are
  ## already the full derived density (not a per-sample sum needing
  ## averaging), so n_samples = 1 makes weighted_density = density_sum *
  ## prop exactly, with no double-division.
  n_samples_pseudo <- unique(species_level[, .(AreaID, Year, Stratum)])
  n_samples_pseudo[, n_samples := 1L]
  list(species_level = species_level, fg_level = fg_level, n_samples = n_samples_pseudo)
}

## =================================================================
## STEP H: top-level orchestrator - the one function
## 01_survey_density_westmed.R actually calls.
## =================================================================
#' Returns an EXTENDED (strata_def, strata_area, per_group_fg,
#' per_group_sp, n_samples_by_stratum, audit) - feed strata_area/
## per_group_fg/n_samples_by_stratum straight into weight_by_strata(),
## and strata_area/per_group_sp/n_samples_by_stratum into
## weight_species_by_area(), exactly as the REAL-strata-only versions
## already are in the pipeline script - same functions, extended inputs.
##
## deep_ranges: a LIST of c(depth_from, depth_to) pairs, each becoming
## its own pseudo-stratum - default splits what used to be one 800-6000m
## lump into two sub-bands (800-1000m and 1000-2850m, 2850m being a
## practical ceiling for the western Mediterranean specifically). Pass a
## single-element list (e.g. list(c(800, 6000))) to go back to one lump,
## or more elements for finer resolution still - nothing else in this
## function assumes exactly 1 or 2 deep bands.
apply_aquamaps_depth_adjustment <- function(dt, per_group_fg, per_group_sp, n_samples_by_stratum,
                                            medits_strata_def,
                                            area_ids, area_shp, area_id_col,
                                            shallow_range = c(0, 10),
                                            deep_ranges = list(c(800, 1000), c(1000, 2850)),
                                            cache_path = NULL, resolution_m = 1) {
  ## NOTE: no longer takes strata_area_by_area - the anchor-stratum design
  ## (see build_aquamaps_depth_multipliers()'s header comment) scales each
  ## pseudo-stratum from ONE specific adjacent real stratum's own raw
  ## per-sample density (compute_species_anchor_stratum_density()), not
  ## an area-weighted composite across all 5 real strata, so the area
  ## table that composite needed is no longer used here.
  ensure_aquamaps_db()
  
  species_names <- sort(unique(dt$ScientificName[!is.na(dt$ScientificName) & dt$ScientificName != ""]))
  id_lookup <- resolve_aquamaps_species_ids(species_names)
  envelope <- fetch_aquamaps_depth_envelope(id_lookup)
  
  ## The shallow pseudo-stratum anchors to the SHALLOWEST real MEDITS
  ## stratum's own depth range (e.g. 10-49.99m); every deep pseudo-
  ## stratum anchors to the DEEPEST real stratum's own depth range (e.g.
  ## 500-799.99m) - NOT the full 10-800m composite. See
  ## build_aquamaps_depth_multipliers()'s header comment for why.
  shallow_anchor_row <- medits_strata_def[which.min(stratum_num)]
  deep_anchor_row <- medits_strata_def[which.max(stratum_num)]
  anchor_shallow_range <- c(shallow_anchor_row$depth_min, shallow_anchor_row$depth_max)
  anchor_deep_range <- c(deep_anchor_row$depth_min, deep_anchor_row$depth_max)
  
  multipliers <- build_aquamaps_depth_multipliers(envelope, anchor_shallow_range, anchor_deep_range,
                                                  shallow_range, deep_ranges, resolution_m)
  deep_mult_cols <- attr(multipliers, "deep_mult_cols")
  
  shallow_stratum_num <- min(medits_strata_def$stratum_num) - 1L
  deep_stratum_nums <- max(medits_strata_def$stratum_num) + seq_along(deep_ranges)
  deep_strata_def <- rbindlist(lapply(seq_along(deep_ranges), function(i) {
    data.table(stratum_num = deep_stratum_nums[i], depth_min = deep_ranges[[i]][1], depth_max = deep_ranges[[i]][2])
  }))
  extended_strata_def <- rbind(
    data.table(stratum_num = shallow_stratum_num, depth_min = shallow_range[1], depth_max = shallow_range[2] - 0.0001),
    medits_strata_def,
    deep_strata_def
  )
  deep_ranges_label <- paste(vapply(deep_ranges, function(r) paste0(r[1], "-", r[2], "m"), character(1)), collapse = ", ")
  message("[AquaMaps] Computing bathymetric area for the ", 1 + length(deep_ranges), " new pseudo-strata (",
          shallow_range[1], "-", shallow_range[2], "m and ", deep_ranges_label, ") alongside the ",
          nrow(medits_strata_def), " real MEDITS strata. Shallow anchors to real stratum ",
          shallow_anchor_row$stratum_num, " (", anchor_shallow_range[1], "-", anchor_shallow_range[2],
          "m), deep bands anchor to real stratum ", deep_anchor_row$stratum_num, " (",
          anchor_deep_range[1], "-", anchor_deep_range[2], "m)...")
  extended_strata_area <- compute_strata_area_by_area(
    area_ids = area_ids, area_shp = area_shp, area_id_col = area_id_col,
    strata_def = extended_strata_def, cache_path = cache_path)
  
  shallow_anchor_density <- compute_species_anchor_stratum_density(
    per_group_sp, n_samples_by_stratum, shallow_anchor_row$stratum_num)
  deep_anchor_density <- compute_species_anchor_stratum_density(
    per_group_sp, n_samples_by_stratum, deep_anchor_row$stratum_num)
  pseudo <- build_pseudo_strata_density(shallow_anchor_density, deep_anchor_density, multipliers,
                                        shallow_stratum_num, deep_stratum_nums, deep_mult_cols)
  
  list(
    strata_def = extended_strata_def,
    strata_area = extended_strata_area,
    per_group_fg = rbind(per_group_fg, pseudo$fg_level, fill = TRUE),
    per_group_sp = rbind(per_group_sp, pseudo$species_level, fill = TRUE),
    n_samples_by_stratum = rbind(n_samples_by_stratum, pseudo$n_samples, fill = TRUE),
    audit = multipliers
  )
}