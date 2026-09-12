## =================================================================
## lib_survey_fg_density_functions.R - LIBRARY FILE, not a pipeline step.
## sourced automatically by the numbered pipeline scripts (01-04) -
## do not run this directly, it has no top-level driver code of its own.
## =================================================================

## =================================================================
## lib_survey_fg_density_functions.R
##
## Generalized, survey-agnostic functions for converting a fishery-
## independent survey (biomass/abundance by species, haul/station,
## year, area, stratum) into a Functional-Group-level density index,
## suitable as Ecopath/Ecosim input.
##
## Adapted from the MEDITS/MEDBS pipeline (medbs_pipeline_current.R)
## built earlier in this project - the STATISTICAL LOGIC (strata-
## weighting formula, region-wide area-weighting formula) is preserved
## exactly as validated there, since that's the mathematically-checked
## part. What's generalized is the DATA SHAPE: instead of assuming
## MEDITS' specific TA.csv/TB.csv structure and column names, every
## function here takes a standardized input format (documented below)
## that any survey's raw data can be mapped into.
##
## =================================================================
## FUNCTION ATTRIBUTES (as specified)
## =================================================================
##   strata      - TRUE/FALSE. If TRUE, apply within-area strata
##                 weighting (Step 6) using bathymetry-derived stratum
##                 areas. If FALSE, Step 6 is skipped and Step 5's
##                 per-area densities are used directly in Step 7.
##   area_shp    - an sf polygon object (or a path to one) defining the
##                 spatial areas to aggregate over (e.g. GSAs, ICES
##                 rectangles, EEZs) - MEDITS used the GFCM GSA
##                 shapefile; any polygon layer with a numeric/character
##                 ID column works, as long as area_id_col is set to
##                 match.
##   year_ecopath - e.g. c(1994:1996). Years averaged for the Ecopath
##                 base-year biomass output (Step 9).
##   ts_years    - e.g. first.yr:last.yr. If specified, the full year
##                 range for the Ecosim time-series sheet (Step 9). If
##                 NULL, defaults to min(year) in the data through
##                 max(year).
##
## =================================================================
## STANDARDIZED INPUT FORMAT (Step 2)
## =================================================================
##   dataframe1 (survey observations, one row per species x sample):
##     ScientificName - character
##     Biomass        - numeric, TONNES (not kg/g - convert before
##                       calling, same convention as the original
##                       pipeline's own weight_tonnes)
##     Year           - integer
##     Lat, Lon       - numeric, decimal degrees (used for spatial join
##                       to area_shp if AreaID isn't already provided;
##                       see match_samples_to_area())
##     AreaID         - optional. If your survey already assigns a
##                       station/haul to an area code matching
##                       area_shp's ID column, supply it directly and
##                       skip the spatial join. If NULL, derived from
##                       Lat/Lon.
##     Stratum        - optional (only needed if strata=TRUE). Either a
##                       pre-assigned stratum code (if your survey
##                       already has one, e.g. a depth-derived
##                       stratum_num), or NULL to derive from Depth
##                       (see assign_depth_stratum()).
##     Depth          - optional, needed only if Stratum is NULL and
##                       strata=TRUE
##     SampleID       - unique identifier per sample/haul/station -
##                       needed to count replication per area/stratum.
##                       MUST BE GLOBALLY UNIQUE if combining multiple
##                       surveys (see Survey below) - two different
##                       surveys reusing the same SampleID string (e.g.
##                       both just using "1", "2", ...) would silently
##                       miscount samples as if they were the same one.
##                       validate_survey_data() checks for this.
##     Effort         - numeric, the denominator for density (e.g. swept
##                       area in km^2 for trawl, or 1 for
##                       already-standardized acoustic estimates -
##                       supply 1 uniformly if the survey's own values
##                       are already a density/absolute estimate needing
##                       no further division)
##     Survey         - optional but recommended when combining data
##                       from more than one survey/program (e.g. a
##                       bottom trawl survey and an acoustic survey, or
##                       the same protocol run by two different
##                       institutes). Purely a label - not used directly
##                       by any function's math, but carried through so
##                       results can be broken down or filtered by
##                       source later, and so a SampleID collision
##                       across surveys is easier to spot and fix (e.g.
##                       by prefixing SampleID with Survey before
##                       calling these functions, which guarantees
##                       global uniqueness). To combine multiple
##                       surveys: build each one's dataframe1
##                       separately in its own standardized form, then
##                       rbind() them together before Step 3 onward -
##                       every function from that point on operates on
##                       whatever rows are in dt, regardless of how many
##                       original surveys they came from.
##
##   dataframe2 (species -> FG reference):
##     ScientificName - character, matched against dataframe1
##     FG_num         - numeric/character FG code
##     FG_name        - character FG label
## =================================================================

library(data.table)
library(dplyr)
library(stringr)
library(ggplot2)
library(sf)

## =================================================================
## STEP 1: Configuration
## =================================================================
## Deliberately NOT a function - configuration (working directory,
## file paths, filters) is inherently survey-specific and belongs in
## the calling script, not this shared library. See the example script
## for what one survey's config looks like; a different survey's
## example script would set its own equivalents (input file paths,
## area filters, etc.) the same way.

## =================================================================
## STEP 2: Input data validation
## =================================================================
## Not a data transformation - just a fail-fast check that the
## standardized shape above is actually what was passed in, so a
## missing/misnamed column shows up as a clear error here rather than
## a cryptic failure three functions later.

validate_survey_data <- function(dataframe1, dataframe2, strata = TRUE) {
  required_1 <- c("ScientificName", "Biomass", "Year", "SampleID", "Effort")
  missing_1 <- setdiff(required_1, names(dataframe1))
  if (length(missing_1) > 0) {
    stop("dataframe1 is missing required column(s): ", paste(missing_1, collapse = ", "))
  }
  if (!all(c("Lat", "Lon") %in% names(dataframe1)) && !"AreaID" %in% names(dataframe1)) {
    stop("dataframe1 needs either AreaID, or both Lat and Lon (for spatial join to area_shp).")
  }
  if (strata && !"Stratum" %in% names(dataframe1) && !"Depth" %in% names(dataframe1)) {
    stop("strata=TRUE needs either a Stratum column, or Depth (to derive strata via",
         " assign_depth_stratum()) in dataframe1.")
  }
  
  ## SampleID collision check - a real risk when combining more than
  ## one survey, since two different surveys' own row-numbering
  ## conventions could easily produce the same SampleID string by
  ## coincidence, which would silently merge unrelated samples together
  ## in every downstream n_samples count. If Survey is present, check
  ## per-survey; if Survey isn't present at all, just confirm SampleID
  ## itself is unique (the single-survey case).
  if ("Survey" %in% names(dataframe1)) {
    dup_check <- unique(dataframe1[, .(SampleID, Survey)])
    n_dup <- dup_check[, .N, by = SampleID][N > 1, .N]
    if (n_dup > 0) {
      stop(n_dup, " SampleID value(s) appear under more than one Survey - this means",
           " different surveys are reusing the same SampleID string, which will",
           " silently miscount samples. Prefix SampleID with Survey (e.g.",
           " paste(Survey, SampleID)) before calling these functions.")
    }
  }
  
  required_2 <- c("ScientificName", "FG_num", "FG_name")
  missing_2 <- setdiff(required_2, names(dataframe2))
  if (length(missing_2) > 0) {
    stop("dataframe2 is missing required column(s): ", paste(missing_2, collapse = ", "))
  }
  
  message("Input validation passed: ", nrow(dataframe1), " observations, ",
          uniqueN(dataframe1$SampleID), " distinct samples",
          if ("Survey" %in% names(dataframe1)) paste0(" across ", uniqueN(dataframe1$Survey), " survey(s)") else "",
          ", ", nrow(dataframe2), " species-FG reference rows.")
  invisible(TRUE)
}

## =================================================================
## STEP 3: Scientific name -> FG (direct match) + de-duplication
## =================================================================
## Generalized from Section 2b/4 of the MEDITS pipeline.
##
## Multistanza detection is STRUCTURAL, not text-based: a species split
## across multiple FGs is treated as a stanza split (juv/adult or
## similar) only when EVERY one of those FGs is exclusive to that one
## species (no other species shares it) - that's the actual shape of a
## life-stage split, and it doesn't depend on the FG names being in
## English or using any particular wording ("juv.", "juvenile", etc.),
## so it holds for any survey/FG scheme, not just this one.
##
## If a species maps to multiple FGs and at least one of those FGs
## also contains OTHER species, that's genuine ambiguity (which real FG
## does this species belong to?), not a stanza split - still flagged
## for manual review rather than guessed.

prepare_fg_lookup <- function(dataframe2) {
  fg_lookup <- as.data.table(dataframe2)
  fg_lookup <- unique(fg_lookup[, .(ScientificName, FG_num, FG_name)])
  
  ## how many distinct species does each FG contain?
  fg_species_counts <- fg_lookup[, .(n_species_in_fg = uniqueN(ScientificName)), by = FG_num]
  fg_lookup <- merge(fg_lookup, fg_species_counts, by = "FG_num")
  
  ## how many FGs does each species map to?
  sp_fg_counts <- fg_lookup[, .(n_fg = uniqueN(FG_num)), by = ScientificName]
  multi_fg_species <- sp_fg_counts[n_fg > 1, ScientificName]
  
  ## a species is a stanza split iff EVERY FG it maps to is exclusive
  ## to that species alone (n_species_in_fg == 1 for all of them)
  stanza_check <- fg_lookup[ScientificName %in% multi_fg_species,
                            .(all_exclusive = all(n_species_in_fg == 1)), by = ScientificName]
  stanza_species <- stanza_check[all_exclusive == TRUE, ScientificName]
  ambiguous_species <- stanza_check[all_exclusive == FALSE, ScientificName]
  
  if (length(stanza_species) > 0) {
    message(length(stanza_species), " species found in multiple, single-species-exclusive",
            " FGs (a stanza split - e.g. juvenile/adult) - just one stanza kept per",
            " species to avoid duplicates:")
    print(fg_lookup[ScientificName %in% stanza_species][order(ScientificName, FG_num)])
  }
  ## keep exactly one row per stanza-split species (first by FG_num - an
  ## arbitrary but consistent tiebreak, since both rows are otherwise
  ## equally "one stanza" with nothing to prefer between them)
  fg_deduped <- rbindlist(list(
    fg_lookup[!ScientificName %in% c(stanza_species, ambiguous_species)],
    fg_lookup[ScientificName %in% stanza_species][order(FG_num), .SD[1], by = ScientificName]
  ), fill = TRUE)
  
  if (length(ambiguous_species) > 0) {
    message(length(ambiguous_species), " species map to multiple FGs where at least one",
            " FG also contains other species (genuine ambiguity, not a stanza split) -",
            " excluded from automatic FG assignment, needs manual review:")
    print(fg_lookup[ScientificName %in% ambiguous_species][order(ScientificName, FG_num)])
  }
  
  unique(fg_deduped[, .(ScientificName, FG_num, FG_name)])
}

match_species_to_fg <- function(dataframe1, fg_lookup_safe) {
  dt <- as.data.table(dataframe1)
  dt <- merge(dt, fg_lookup_safe, by = "ScientificName", all.x = TRUE)
  message("Direct match: ", dt[!is.na(FG_num), uniqueN(ScientificName)], " of ",
          dt[, uniqueN(ScientificName)], " distinct species matched directly to an FG.")
  dt
}

## Some scientific names are trinomials for the nominate subspecies,
## where the subspecies epithet repeats the species epithet by taxonomic
## convention (e.g. "Diplodus sargus sargus" is the nominate subspecies
## of "Diplodus sargus") - these fail a direct match if fg_lookup_safe
## only has the binomial form. Detects any name with a repeated word,
## strips it down to the first two words (Genus + species), and retries
## the match against fg_lookup_safe. Purely string-based, no taxonomy
## lookup needed, so this runs cheaply right after the direct match and
## before the (much more expensive) taxonomy-based fallback.
match_nominate_subspecies_fg <- function(dt, fg_lookup_safe) {
  dt <- copy(dt)  # avoid data.table shallow-copy warning on := after this dt passed through merge()/subsetting upstream
  still_unmatched <- dt[is.na(FG_num) & !is.na(ScientificName), unique(ScientificName)]
  if (length(still_unmatched) == 0) {
    message("No unmatched species - nominate-subspecies check not needed.")
    return(dt)
  }
  
  has_repeated_word <- sapply(strsplit(still_unmatched, " "),
                              function(words) any(duplicated(words)))
  repeated_names <- still_unmatched[has_repeated_word]
  if (length(repeated_names) == 0) {
    message("No unmatched species have a repeated-word (nominate subspecies) name pattern.")
    return(dt)
  }
  
  binomial_form <- sapply(strsplit(repeated_names, " "), function(words) paste(words[1:2], collapse = " "))
  lookup <- data.table(ScientificName = repeated_names, binomial = binomial_form)
  lookup <- merge(lookup, fg_lookup_safe, by.x = "binomial", by.y = "ScientificName")
  
  if (nrow(lookup) == 0) {
    message(length(repeated_names), " unmatched species have a repeated-word name pattern",
            " (e.g. trinomial nominate subspecies), but none of their binomial forms",
            " matched fg_lookup_safe either:")
    print(repeated_names)
    return(dt)
  }
  
  dt <- merge(dt, lookup[, .(ScientificName, FG_num, FG_name)], by = "ScientificName",
              all.x = TRUE, suffixes = c("", "_ns"))
  dt[is.na(FG_num) & !is.na(FG_num_ns), `:=`(FG_num = FG_num_ns, FG_name = FG_name_ns)]
  dt[, c("FG_num_ns", "FG_name_ns") := NULL]
  
  message("Resolved via nominate-subspecies binomial match: ", nrow(lookup), " species",
          " (e.g. '", lookup$ScientificName[1], "' -> matched as '", lookup$binomial[1], "')")
  dt
}

## =================================================================
## STEP 4: Maximize FG assignment via taxonomy fallback
## (WoRMS and/or FishBase)
## =================================================================
## Generalized from Section 4c of the MEDITS pipeline: for species with
## no direct FG match, try genus then family fallback, under the
## assumption that congeners/confamilials are ecologically similar
## enough to share an FG. Same ambiguity-safe rule preserved exactly:
## a genus/family is only used for fallback if it maps to EXACTLY ONE
## FG among the reference species - anything spanning multiple FGs is
## too ecologically diverse to assign safely and is left unresolved.
##
## taxonomy_source: "worms" (default, requires worms_taxonomy_lookup()
## already sourced - the same function used throughout this project),
## "fishbase" (requires rfishbase, uses species()'s own Genus/Family
## fields - fish/SeaLifeBase coverage only, won't resolve algae or taxa
## outside FishBase/SeaLifeBase's scope), or "both" (tries worms first,
## then fishbase for whatever worms left unresolved - the two sources
## don't depend on each other, so this is a genuine combined fallback,
## not just redundancy).

fetch_taxonomy_worms <- function(species_names) {
  if (!exists("worms_taxonomy_lookup")) {
    stop("worms_taxonomy_lookup() not found - source() the same",
         " lib_worms_taxonomy_lookup.R used elsewhere in this project first.")
  }
  raw <- worms_taxonomy_lookup(species_names)
  as.data.table(raw)[, .(ScientificName = original_name, Genus = genus, Family = family,
                         Order = order, Class = class, Phylum = phylum)]
}

fetch_taxonomy_fishbase <- function(species_names) {
  if (!requireNamespace("rfishbase", quietly = TRUE)) {
    stop("rfishbase not installed - required for taxonomy_source='fishbase' or 'both'.")
  }
  results <- rbindlist(lapply(c("fishbase", "sealifebase"), function(server) {
    tryCatch({
      sp <- as.data.table(rfishbase::species(species_names, server = server))
      if (nrow(sp) == 0) return(NULL)
      sp[, .(ScientificName = Species, Genus = Genus, Family = Family)]
    }, error = function(e) NULL)
  }))
  if (nrow(results) == 0) return(data.table(ScientificName = character(0), Genus = character(0), Family = character(0)))
  unique(results, by = "ScientificName")
}

fetch_taxonomy <- function(species_names, taxonomy_source = "worms") {
  if (taxonomy_source == "worms") return(fetch_taxonomy_worms(species_names))
  if (taxonomy_source == "fishbase") return(fetch_taxonomy_fishbase(species_names))
  if (taxonomy_source == "both") {
    worms_tax <- fetch_taxonomy_worms(species_names)
    still_missing <- setdiff(species_names, worms_tax[!is.na(Genus) | !is.na(Family), ScientificName])
    if (length(still_missing) > 0) {
      fb_tax <- fetch_taxonomy_fishbase(still_missing)
      worms_tax <- rbindlist(list(worms_tax[!ScientificName %in% still_missing], fb_tax), fill = TRUE)
    }
    return(worms_tax)
  }
  stop("taxonomy_source must be 'worms', 'fishbase', or 'both' - got '", taxonomy_source, "'")
}

## rank_levels tried in order, most specific first ("the lowest level
## possible", per how this was requested) - a species is matched at
## the first rank where its value maps to EXACTLY ONE FG among
## fg_lookup_safe's own species (the same "safe" rule used throughout
## this project: a genus/family/order/class/phylum spanning multiple
## FGs is too ecologically diverse to assign safely, and is skipped
## rather than guessed). This is fully data-driven from fg_lookup_safe
## - no hardcoded "Class Bivalvia -> FG Bivalves"-style rule tables
## needed anywhere; if the FG scheme changes, these safe mappings are
## re-derived automatically on the next run rather than needing manual
## updates to a separate rules table.
fallback_match_fg_by_taxonomy <- function(dt, fg_lookup_safe, taxonomy_source = "worms",
                                          rank_levels = c("Genus", "Family", "Order", "Class", "Phylum")) {
  dt <- copy(dt)  # avoid data.table shallow-copy warning on := after this dt passed through merge()/subsetting upstream
  unmatched_sci <- unique(dt[!is.na(ScientificName) & is.na(FG_num), ScientificName])
  if (length(unmatched_sci) == 0) {
    message("No unmatched species - taxonomy fallback not needed.")
    return(dt)
  }
  message(length(unmatched_sci), " distinct species have no direct FG match -",
          " attempting taxonomy fallback via ", taxonomy_source,
          " (", paste(rank_levels, collapse = " -> "), ").")
  
  ## taxonomy for the FG reference species themselves too, for whichever
  ## rank columns aren't already present
  missing_rank_cols <- setdiff(rank_levels, names(fg_lookup_safe))
  if (length(missing_rank_cols) > 0) {
    fg_taxonomy <- fetch_taxonomy(fg_lookup_safe$ScientificName, taxonomy_source)
    fg_lookup_safe <- merge(fg_lookup_safe, fg_taxonomy[, c("ScientificName", rank_levels), with = FALSE],
                            by = "ScientificName", all.x = TRUE)
  }
  
  ## attached as an attribute on the return value (see bottom of this
  ## function, "fetched_taxonomy") so a caller with its own additional
  ## taxonomy-based logic can reuse this fetch instead of re-querying
  ## the same species
  unmatched_taxonomy <- fetch_taxonomy(unmatched_sci, taxonomy_source)
  message(unmatched_taxonomy[!is.na(Genus) | !is.na(Family), .N], " of ",
          length(unmatched_sci), " found with usable genus/family.")
  
  remaining <- copy(unmatched_taxonomy)
  matches_by_level <- list()
  
  for (rank in rank_levels) {
    if (nrow(remaining) == 0) break
    if (!rank %in% names(fg_lookup_safe)) next
    
    rank_fg_counts <- fg_lookup_safe[!is.na(get(rank)), .(n_fg = uniqueN(FG_num)), by = rank]
    safe_values <- rank_fg_counts[n_fg == 1, get(rank)]
    ambiguous_values <- rank_fg_counts[n_fg > 1, get(rank)]
    
    if (length(safe_values) > 0) {
      rank_safe <- unique(fg_lookup_safe[get(rank) %in% safe_values, c(rank, "FG_num", "FG_name"), with = FALSE])
      level_matches <- merge(remaining, rank_safe, by = rank)
      if (nrow(level_matches) > 0) {
        matches_by_level[[rank]] <- level_matches[, .(ScientificName, FG_num, FG_name)]
        message("Resolved via ", rank, " fallback: ", nrow(level_matches))
        remaining <- remaining[!ScientificName %in% level_matches$ScientificName]
      }
    }
    if (length(ambiguous_values) > 0) {
      message(length(ambiguous_values), " ", rank, "-level value(s) span multiple FGs",
              " (too ecologically diverse for fallback, not used): ",
              paste(head(ambiguous_values, 10), collapse = ", "),
              if (length(ambiguous_values) > 10) ", ..." else "")
    }
  }
  
  ## rbindlist() on a completely empty list (nothing resolved at ANY
  ## level - a real, reachable case, not just theoretical) produces a
  ## zero-column table that breaks the merge below since it has no
  ## ScientificName column to join on - guard against that explicitly
  ## rather than letting a genuinely-zero-matches batch crash.
  fallback_matches <- if (length(matches_by_level) > 0) {
    rbindlist(matches_by_level, fill = TRUE)
  } else {
    data.table(ScientificName = character(0), FG_num = numeric(0), FG_name = character(0))
  }
  message("Still unresolved after all taxonomy levels: ", length(unmatched_sci) - nrow(fallback_matches))
  
  dt <- merge(dt, fallback_matches, by = "ScientificName", all.x = TRUE, suffixes = c("", "_fb"))
  dt[is.na(FG_num) & !is.na(FG_num_fb), `:=`(FG_num = FG_num_fb, FG_name = FG_name_fb)]
  dt[, c("FG_num_fb", "FG_name_fb") := NULL]
  
  message("After taxonomy fallback: ", dt[!is.na(FG_num), uniqueN(ScientificName)], " of ",
          dt[, uniqueN(ScientificName)], " distinct species matched.")
  
  ## Two different attributes, deliberately named to avoid the confusion
  ## of a single "unmatched_taxonomy" name: "fetched_taxonomy" is the
  ## FULL set this function queried (species unmatched at the START,
  ## before this fallback ran) - reuse this if you need Order/Class/
  ## Phylum for your own additional logic. "still_unresolved_taxonomy"
  ## is the genuinely different, smaller set that remains unmatched
  ## AFTER every fallback level was tried - a species can legitimately
  ## appear in the first but not the second, if this function resolved
  ## it along the way.
  attr(dt, "fetched_taxonomy") <- unmatched_taxonomy
  attr(dt, "still_unresolved_taxonomy") <- remaining
  dt
}

## =================================================================
## Seed FG rules - for the genuine remainder fallback_match_fg_
## by_taxonomy() can't resolve: FGs with NO species already assigned
## to them in dataframe2 at all (e.g. "Other macro-benthos", a bucket
## FG with nothing pre-listed against it to learn an exclusive mapping
## from). This is a real, unavoidable limitation of the data-driven
## approach, not a bug - if the FG reference never assigns any species
## to a bucket FG, there is nothing in the data to infer that mapping
## from, and it has to come from a person who knows the scheme.
##
## Kept deliberately small and separate from the main fallback: only
## for FGs confirmed to have zero reference species (check first -
## dataframe2[FG_name %in% "X", .N] == 0 - a genuinely empty FG is why
## this is needed; a nonzero count means the automatic fallback should
## already handle it, and adding a rule here would just be masking a
## different problem, like an ambiguous rank elsewhere in the
## reference, worth investigating instead of overriding).
##
## Resolved by FG_name text (not a hardcoded FG_num), so this survives
## the FG scheme being renumbered - only breaks if a FG_name itself is
## reworded, at which point the resolution check below will flag it
## with an NA rather than silently assigning nothing.
##
## species_exceptions (optional): data.table with columns ScientificName,
## fg_name_target - applied BEFORE the rank-based seed_rules, so a named
## species is never caught by a broader rule that would be wrong for it
## specifically. This is for the same situation as the Nephrops
## norvegicus override used elsewhere in this project: a rank-level
## rule is right for MOST members of that rank but wrong for one named
## exception (e.g. Squilla mantis is commercially exploited even though
## the rest of Stomatopoda isn't, so "Order Stomatopoda -> Non-commercial
## decapods" would misclassify it without this).
apply_seed_fg_rules <- function(dt, dataframe2, seed_rules, species_exceptions = NULL) {
  dt <- copy(dt)  # avoid data.table shallow-copy warning on := after this dt passed through merge()/subsetting upstream
  n_before <- dt[!is.na(FG_num), uniqueN(ScientificName)]
  
  ## IMPORTANT: both captured here, before any merge() below - merge()
  ## strips custom attributes entirely (unlike copy(), which preserves
  ## them), so grabbing these now and using the local variables
  ## throughout is what keeps them available within this function, and
  ## both are re-attached to the returned dt at the end so a caller
  ## relying on either attribute after this function (e.g. the example
  ## script's own use of still_unresolved_taxonomy for the manual-review
  ## export) still finds it there.
  taxonomy_source_data <- attr(dt, "fetched_taxonomy")
  still_unresolved_taxonomy_data <- attr(dt, "still_unresolved_taxonomy")
  if (is.null(taxonomy_source_data)) {
    stop("apply_seed_fg_rules() needs the 'fetched_taxonomy' attribute from",
         " fallback_match_fg_by_taxonomy() - run that first.")
  }
  
  if (!is.null(species_exceptions)) {
    resolved_exceptions <- merge(species_exceptions, unique(dataframe2[, .(FG_num, FG_name)]),
                                 by.x = "fg_name_target", by.y = "FG_name", all.x = TRUE)
    message("Species-exception resolution check (FG_num should not be NA):")
    print(resolved_exceptions)
    
    n_unresolved_exc <- resolved_exceptions[is.na(FG_num), .N]
    if (n_unresolved_exc > 0) {
      stop(n_unresolved_exc, " species_exceptions entr(y/ies) have a fg_name_target that",
           " doesn't exactly match a real FG_name in dataframe2 - fix the entries above",
           " before proceeding, since a silently-unresolved exception would let its",
           " species fall through to the rank-based rule instead, exactly what this",
           " exception mechanism exists to prevent.")
    }
    
    dt <- merge(dt, resolved_exceptions[, .(ScientificName, FG_num_exc = FG_num, FG_name_exc = fg_name_target)],
                by = "ScientificName", all.x = TRUE)
    dt[is.na(FG_num) & !is.na(FG_num_exc), `:=`(FG_num = FG_num_exc, FG_name = FG_name_exc)]
    dt[, c("FG_num_exc", "FG_name_exc") := NULL]
    
    n_after_exceptions <- dt[!is.na(FG_num), uniqueN(ScientificName)]
    message("Species exceptions applied: ", n_after_exceptions - n_before, " species assigned/reassigned",
            " before rank-based rules run (these will NOT be touched by seed_rules below,",
            " even if they'd also match a rank-level rule).")
  }
  
  ## seed_rules: data.table with columns rank ("Class"/"Order"/
  ## "Phylum"/"Family"), rank_value (e.g. "Holothuroidea"), fg_name_target
  resolved <- merge(seed_rules, unique(dataframe2[, .(FG_num, FG_name)]),
                    by.x = "fg_name_target", by.y = "FG_name", all.x = TRUE)
  message("Seed rule resolution check (FG_num should not be NA - if it is,",
          " fg_name_target doesn't exactly match a real FG_name):")
  print(resolved)
  
  exception_species <- if (!is.null(species_exceptions)) species_exceptions$ScientificName else character(0)
  
  for (i in seq_len(nrow(resolved))) {
    rank <- resolved$rank[i]; val <- resolved$rank_value[i]
    fg_num <- resolved$FG_num[i]; fg_name <- resolved$fg_name_target[i]
    if (is.na(fg_num)) next
    if (!rank %in% names(taxonomy_source_data)) next
    
    matching_species <- taxonomy_source_data[get(rank) == val, ScientificName]
    ## exclude anything already handled by species_exceptions above,
    ## regardless of whether it's currently matched or not - an
    ## exception's own answer always wins over a rank-level rule
    matching_species <- setdiff(matching_species, exception_species)
    dt[ScientificName %in% matching_species & is.na(FG_num),
       `:=`(FG_num = fg_num, FG_name = fg_name)]
  }
  n_after <- dt[!is.na(FG_num), uniqueN(ScientificName)]
  message("Seed rules + exceptions together resolved ", n_after - n_before, " additional species.")
  
  attr(dt, "fetched_taxonomy") <- taxonomy_source_data
  attr(dt, "still_unresolved_taxonomy") <- still_unresolved_taxonomy_data
  dt
}

## =================================================================
## Summary of species still unmatched after every resolution step -
## mean biomass and appearance count (how many samples/observations
## the species shows up in), so whoever does the manual review can
## prioritize: a species appearing 500 times with substantial biomass
## matters a lot more than one appearing once with a trace amount.
## Taxonomy (Genus/Family/Order/Class/Phylum) included for context,
## e.g. spotting that a batch of unmatched entries are all Chlorophyta
## (green algae) rather than needing to look each one up individually.
## =================================================================

summarize_unresolved_species <- function(dt, taxonomy = NULL) {
  unresolved <- dt[is.na(FG_num) & !is.na(ScientificName)]
  summary_dt <- unresolved[, .(
    mean_biomass = mean(Biomass, na.rm = TRUE),
    n_appearances = .N
  ), by = ScientificName]
  setorder(summary_dt, -n_appearances)
  
  if (!is.null(taxonomy)) {
    summary_dt <- merge(summary_dt, taxonomy, by = "ScientificName", all.x = TRUE)
    setorder(summary_dt, -n_appearances)
  }
  
  message("\n", nrow(summary_dt), " species still unmatched - summary",
          " (sorted by number of appearances, most first):")
  print(summary_dt)
  summary_dt
}

## =================================================================
## STEP 5 (part A): assign each sample to an area (spatial join) and
## a stratum (from depth, if not already provided)
## =================================================================
## Generalized from Section 5a/5b of the MEDITS pipeline. MEDITS
## already had a GSA code directly in the data (AreaID can be supplied
## directly, skipping this) - for surveys that only have Lat/Lon, this
## spatially joins each sample against area_shp's polygons.

match_samples_to_area <- function(dt, area_shp, area_id_col) {
  dt <- copy(dt)  # avoid data.table shallow-copy warning on := after this dt passed through merge()/subsetting upstream
  if ("AreaID" %in% names(dt) && all(!is.na(dt$AreaID))) {
    message("AreaID already present in all rows - skipping spatial join.")
    return(dt)
  }
  if (!all(c("Lat", "Lon") %in% names(dt))) {
    stop("No AreaID and no Lat/Lon to spatially join - can't determine sample area.")
  }
  if (is.character(area_shp)) area_shp <- sf::st_read(area_shp, quiet = TRUE)
  
  pts <- sf::st_as_sf(dt, coords = c("Lon", "Lat"), crs = sf::st_crs(area_shp), remove = FALSE)
  joined <- sf::st_join(pts, area_shp[, area_id_col])
  dt[, AreaID := sf::st_drop_geometry(joined)[[area_id_col]]]
  
  n_unmatched <- dt[is.na(AreaID), .N]
  if (n_unmatched > 0) {
    message(n_unmatched, " sample(s) fell outside every polygon in area_shp",
            " (e.g. a coordinate error, or genuinely outside the study area) -",
            " these will be excluded from area-level aggregation.")
  }
  dt
}

## strata_def: data.table with columns stratum_num, depth_min, depth_max
## (same structure as MEDITS_STRATA in the original pipeline) - pass
## your survey's own depth strata definition here, or reuse
## MEDITS_STRATA-style bounds if genuinely the same design.
##
## Supports fully pre-assigned strata (every row already has Stratum -
## skips depth-derivation entirely), PARTIALLY pre-assigned strata
## (some rows have it, some don't - existing values are preserved,
## only the missing ones are derived from Depth), or no Stratum column
## at all (fully derived from Depth).
assign_depth_stratum <- function(dt, strata_def) {
  dt <- copy(dt)  # avoid data.table shallow-copy warning on := after this dt passed through merge()/subsetting upstream
  if ("Stratum" %in% names(dt) && all(!is.na(dt$Stratum))) {
    message("Stratum already present in all rows - skipping depth-based assignment entirely.")
    dt[, Stratum := as.integer(Stratum)]
    return(dt)
  }
  if (!"Depth" %in% names(dt)) {
    stop("No Stratum and no Depth column - can't derive strata.")
  }
  
  if (!"Stratum" %in% names(dt)) dt[, Stratum := NA_integer_]
  dt[, Stratum := as.integer(Stratum)]
  n_preassigned <- dt[!is.na(Stratum), .N]
  if (n_preassigned > 0) {
    message(n_preassigned, " row(s) already have a Stratum value - kept as-is.",
            " Deriving from Depth only for the remaining rows that don't.")
  }
  
  ## all.inside=TRUE clamps findInterval()'s result to 1:(length(depth_min)-1)
  ## at BOTH ends, not just the lower one. The lower-end clamp is harmless -
  ## the explicit Depth < min(...) check below re-nulls anything that would've
  ## been wrongly caught there. But the upper-end clamp is not harmless: it
  ## silently folds every Depth in the deepest real stratum's range (e.g.
  ## 500-799.99m for the standard 5-row MEDITS_STRATA) into the SECOND-
  ## deepest stratum instead (e.g. 200-499.99m) - contaminating that
  ## stratum's density with deep-water catch and leaving the true deepest
  ## stratum with zero samples. Use plain findInterval() (no clamping)
  ## instead, which correctly returns index length(depth_min) for any Depth
  ## >= the deepest depth_min; the only downside is it returns 0 (not NA)
  ## for Depth below the shallowest depth_min, which would silently shorten
  ## the vector if used directly as an index (rather than returning NA in
  ## that position) - so remap 0 to NA_integer_ first.
  raw_idx <- findInterval(dt$Depth, strata_def$depth_min)
  raw_idx[raw_idx == 0L] <- NA_integer_
  dt[, derived_stratum := strata_def$stratum_num[raw_idx]]
  dt[Depth < min(strata_def$depth_min) | Depth > max(strata_def$depth_max), derived_stratum := NA_integer_]
  dt[is.na(Stratum), Stratum := derived_stratum]
  dt[, derived_stratum := NULL]
  
  n_out <- dt[is.na(Stratum), .N]
  if (n_out > 0) {
    excluded_depths <- dt[is.na(Stratum), Depth]
    message(n_out, " sample(s) have Depth outside strata_def's range (", min(strata_def$depth_min),
            "-", max(strata_def$depth_max), "m) - excluded from the strata-weighted index.",
            " Excluded depths range from ", round(min(excluded_depths, na.rm = TRUE), 1), "m to ",
            round(max(excluded_depths, na.rm = TRUE), 1), "m - worth checking whether this is",
            " genuinely out-of-protocol sampling (e.g. deeper hauls than the standard strata cover)",
            " or a data issue (e.g. a unit mismatch), rather than assuming either.")
  }
  dt
}

## =================================================================
## Custom sub-region filtering - lets you extract survey data for a
## DIFFERENT model boundary than the GSA polygons used elsewhere in
## this pipeline. Useful when the same underlying survey data needs to
## support multiple EwE models covering different, possibly smaller or
## differently-shaped areas within the broader survey region (e.g. a
## specific bay, a sub-area spanning parts of several GSAs, or any
## other custom boundary that doesn't line up with GSA lines at all).
##
## filter_type = "shapefile": area_filter is an sf polygon object (or a
## path to one) - only samples whose Lat/Lon fall within it are kept.
## filter_type = "bbox": area_filter is a named vector/list with xmin,
## xmax, ymin, ymax (decimal degrees) - a simple rectangular filter,
## no shapefile needed.
##
## This is independent of AreaID/GSA - a sample can be kept or dropped
## by this filter regardless of which GSA it's assigned to, since the
## custom boundary may not respect GSA lines at all. Apply this BEFORE
## match_samples_to_area()/assign_depth_stratum() if you want strata
## area calculations etc. to only reflect the custom sub-region, not
## the full GSA.
## =================================================================

filter_samples_by_area <- function(dt, area_filter, filter_type = c("shapefile", "bbox"), points_crs = 4326) {
  filter_type <- match.arg(filter_type)
  if (!all(c("Lat", "Lon") %in% names(dt))) {
    stop("filter_samples_by_area() needs Lat/Lon columns in dt to determine which",
         " samples fall within the custom area.")
  }
  dt <- copy(dt)
  
  sample_pts_dt <- unique(dt[!is.na(Lat) & !is.na(Lon), .(SampleID, Lat, Lon)])
  sample_pts_sf <- st_as_sf(sample_pts_dt, coords = c("Lon", "Lat"), crs = points_crs, remove = FALSE)
  
  if (filter_type == "shapefile") {
    if (is.character(area_filter)) area_filter <- st_read(area_filter, quiet = TRUE)
    if (is.na(st_crs(area_filter))) {
      stop("area_filter has no CRS defined - set it explicitly (st_crs(area_filter) <- ...) or",
           " st_transform() it to a known CRS before calling this.")
    }
    area_filter <- st_transform(area_filter, points_crs)
    within_area <- lengths(st_intersects(sample_pts_sf, st_union(area_filter))) > 0
  } else {
    required <- c("xmin", "xmax", "ymin", "ymax")
    missing <- setdiff(required, names(area_filter))
    if (length(missing) > 0) {
      stop("filter_type='bbox' needs area_filter to have: ", paste(required, collapse = ", "),
           " - missing: ", paste(missing, collapse = ", "))
    }
    within_area <- sample_pts_dt$Lon >= area_filter["xmin"] & sample_pts_dt$Lon <= area_filter["xmax"] &
      sample_pts_dt$Lat >= area_filter["ymin"] & sample_pts_dt$Lat <= area_filter["ymax"]
  }
  
  keep_ids <- sample_pts_dt$SampleID[within_area]
  message(length(keep_ids), " of ", nrow(sample_pts_dt), " sample(s) fall within the custom",
          " area filter (", filter_type, ") and are kept; ", nrow(sample_pts_dt) - length(keep_ids),
          " fall outside and are dropped.")
  dt[SampleID %in% keep_ids]
}

## =================================================================
## STEP 5 (part B): mean/sum densities across species within FG,
## per sample, then summed within FG/year/stratum/area
## =================================================================
## Generalized from Section 5f's per_haul_fg/per_stratum_fg logic.
## Density = Biomass / Effort per sample; species within the same FG
## are summed per sample first (an FG's total catch in that sample),
## then samples are summed within each FG/year/stratum/area group -
## exactly the MEDITS pipeline's own two-stage sum, just relabeled to
## the generic Sample/Area/Stratum terms.

compute_sample_densities <- function(dt) {
  dt <- copy(dt)  # avoid data.table shallow-copy warning on := after this dt passed through merge()/subsetting upstream
  dt[, Density := Biomass / Effort]
  dt
}


## Shared dedup guard - collapses duplicate key entries in an external
## lookup table (species_q/genus_q/broad_q) to one row per key (mean q)
## BEFORE merging into dt. Left un-deduped, a merge() where the
## right-hand table has duplicate keys fans out multiplicatively against
## every dt row sharing that key - the actual cause of a "Join
## results in ... rows" cartesian error. Warns loudly since a duplicate
## entry is almost always a real data issue in catchability_table (e.g.
## the same taxon name entered twice with different q values), not
## something to average away silently without being seen.
dedupe_lookup <- function(lookup_dt, key_col, source_label) {
  dup_keys <- lookup_dt[, .N, by = key_col][N > 1][[key_col]]
  if (length(dup_keys) == 0) return(lookup_dt)
  message("WARNING: catchability_table has ", length(dup_keys), " duplicate ",
          source_label, " entry name(s) - averaging their q values",
          " (check catchability_table for these names directly, this is",
          " likely a data-entry duplicate in the CSV):")
  print(lookup_dt[get(key_col) %in% dup_keys][order(get(key_col))])
  lookup_dt[, .(q = mean(q, na.rm = TRUE)), by = key_col]
}

## =================================================================
## Catchability correction - trawl surveys don't catch 100% of what's
## actually in the swept area (some individuals escape under/over/
## around the net, avoid it entirely, etc.), so the raw survey density
## (Biomass/Effort) is a systematic UNDERESTIMATE of true density for
## most species, by an amount that varies by species (behavior, size,
## how well it's caught by this particular gear). Catchability (q,
## typically 0 < q <= 1) corrects this: true_density = raw_density / q.
##
## Dividing Biomass by q before Density is computed, or dividing
## Density by q after, are mathematically equivalent (Density =
## Biomass/Effort, and Effort doesn't change) - this divides Density
## directly since that's the quantity every downstream function
## actually uses.
##
## catchability_table: data.table with ScientificName and q columns -
## rename/reshape your source to exactly these two column names before
## calling this (drop anything else, e.g. an FG/FG_name column if your
## source has one - not needed here).
##
## Matching proceeds through several levels, most specific first, each
## one only filling in species still unresolved by the level(s) before it:
##   1. Direct species match (table has an entry for this exact species)
##   2. Genus-level "spp" entries (e.g. "Cirolana spp" matches any
##      species in genus Cirolana)
##   3. Explicit broader-rank entries (e.g. the table has a row literally
##      named "Isopoda"/"Hydrozoa"/"Cnidaria" - tried as Order, then
##      Class, then Phylum)
##   4. Taxonomic-proximity fallback: for species still unmatched, look
##      for OTHER species already in the table (from step 1) that share
##      the same Genus, then Family, then Order, and borrow the average
##      of their q values - different from step 3, which only fires if
##      the table has an entry EXPLICITLY named after the rank itself;
##      this instead finds relatives among the table's own species rows
##      even when no such explicit broader entry exists.
##   5. default_q for anything still unresolved after all of the above.
## Needs species_taxonomy (ScientificName, Genus, Family, Order, Class,
## Phylum) for steps 2-4 - species-level-only matching (step 1) still
## works without it.
##
## exempt_fg_names / exempt_species: force q=1 (no correction) for
## specific FGs or species, REGARDLESS of what steps 1-5 would
## otherwise resolve - applied last, so it always wins. Intended for
## groups a bottom trawl survey isn't designed to sample
## representatively at all (pelagic/planktonic taxa, seagrass/algae) -
## for these, "catchability" as a concept doesn't really apply the same
## way, so no correction is more appropriate than any resolved q.
## =================================================================

apply_catchability_correction <- function(dt, catchability_table, default_q = 1, species_taxonomy = NULL,
                                          exempt_fg_names = NULL, exempt_species = NULL) {
  dt <- copy(dt)
  required <- c("ScientificName", "q")
  missing <- setdiff(required, names(catchability_table))
  if (length(missing) > 0) {
    stop("apply_catchability_correction(): catchability_table is missing column(s): ",
         paste(missing, collapse = ", "), ". Expected columns: ScientificName, q",
         " (q = catchability coefficient, 0 < q <= 1, one row per species/group).")
  }
  bad_q <- catchability_table[!is.na(q) & (q <= 0 | q > 1)]
  if (nrow(bad_q) > 0) {
    message("WARNING: ", nrow(bad_q), " catchability_table entries have q outside the",
            " expected (0, 1] range - check these aren't a units/scale mistake",
            " (e.g. entered as a percentage, 0-100, instead of a fraction, 0-1):")
    print(bad_q)
  }
  
  ## classify each entry by apparent taxonomic level, purely from its
  ## text shape - "Genus species" (two words) -> species level;
  ## "Genus spp" / "Genus spp." (second word is "spp"/"spp.") ->
  ## genus level; anything else (one word) -> broader rank, tried
  ## against Order/Class/Phylum
  ct <- copy(catchability_table)
  ct[, n_words := lengths(strsplit(trimws(ScientificName), "\\s+"))]
  ct[, second_word := sapply(strsplit(trimws(ScientificName), "\\s+"), function(w) if (length(w) >= 2) w[2] else NA_character_)]
  ct[, level := fcase(
    n_words >= 2 & !second_word %in% c("spp", "spp."), "species",
    n_words >= 2 & second_word %in% c("spp", "spp."), "genus",
    default = "broad_rank"
  )]
  ## match_value is initialized as character FIRST, before any
  ## level-specific := assignment below - this matters because of a
  ## real data.table gotcha: if a level (e.g. "genus") happens to match
  ## zero rows in ct, `ct[level=="genus", match_value := sapply(...)]`
  ## still creates the new column from that zero-row assignment's
  ## result type, and sapply() over empty input returns list() rather
  ## than character(0) - so match_value would get created as type
  ## list, silently corrupting every later assignment to that same
  ## column (including the real, non-empty species-level one) into
  ## list-wrapped values instead of plain strings, which then breaks
  ## the merge() below with "x.ScientificName is type list which is
  ## not supported by data.table join". Explicitly setting the type
  ## here first avoids this regardless of which levels are present.
  ct[, match_value := NA_character_]
  ct[level == "genus", match_value := sapply(strsplit(trimws(ScientificName), "\\s+"), `[`, 1)]
  ct[level %in% c("species", "broad_rank"), match_value := trimws(ScientificName)]
  
  message("catchability_table entries classified by level: ",
          paste(names(table(ct$level)), table(ct$level), sep = "=", collapse = ", "))
  
  species_q <- ct[level == "species", .(ScientificName = match_value, q)]
  species_q <- dedupe_lookup(species_q, "ScientificName", "species-level")
  
  genus_q   <- ct[level == "genus",   .(Genus = match_value, q)]
  genus_q   <- dedupe_lookup(genus_q, "Genus", "genus-level")
  
  broad_q   <- ct[level == "broad_rank", .(match_value, q)]
  ## broad_q is deliberately NOT deduped here - its key column is still
  ## the generic "match_value" and hasn't been filtered down to just the
  ## rank(s) actually present in dt yet. Deduping happens per-rank
  ## inside apply_explicit_at_rank() below, right before that rank's
  ## merge, once broad_q has been subset to that rank's actual values.
  
  dt[, q_resolved := NA_real_]
  dt[, match_level := NA_character_]
  
  ## 1. direct species match (most specific, applied first so nothing
  ## below overwrites it)
  dt <- merge(dt, species_q, by = "ScientificName", all.x = TRUE, suffixes = c("", "_sp"))
  dt[is.na(q_resolved) & !is.na(q), `:=`(q_resolved = q, match_level = "species")]
  dt[, q := NULL]
  
  if (!is.null(species_taxonomy)) {
    dt <- merge(dt, species_taxonomy[, .(ScientificName, Genus, Family, Order, Class, Phylum)],
                by = "ScientificName", all.x = TRUE)
    
    ## species_q's OWN taxonomy - needed for the proximity fallback at
    ## each rank tier below (relatives are found among catchability_
    ## table's own species-level rows, not dt's species)
    species_q_tax <- merge(species_q, species_taxonomy, by = "ScientificName")
    
    ## helper: apply proximity fallback at a single rank - borrows the
    ## average q of species_q's own rows sharing that rank value
    apply_proximity_at_rank <- function(dt, rank_col) {
      proximity_q <- species_q_tax[!is.na(get(rank_col)), .(q_prox = mean(q, na.rm = TRUE)), by = rank_col]
      if (nrow(proximity_q) == 0) return(dt)
      dt <- merge(dt, proximity_q, by = rank_col, all.x = TRUE)
      newly_resolved <- dt[is.na(q_resolved) & !is.na(q_prox), .N]
      if (newly_resolved > 0) {
        message(newly_resolved, " row(s) resolved via ", rank_col, "-level taxonomic proximity",
                " (borrowed from related species already in catchability_table).")
      }
      dt[is.na(q_resolved) & !is.na(q_prox), `:=`(q_resolved = q_prox, match_level = paste0(rank_col, " (proximity, borrowed)"))]
      dt[, q_prox := NULL]
      dt
    }
    
    ## helper: apply an explicit broad-rank entry (e.g. table has a row
    ## literally named "Isopoda") at a single rank
    apply_explicit_at_rank <- function(dt, rank_col) {
      rank_q <- broad_q[match_value %in% unique(dt[[rank_col]])]
      if (nrow(rank_q) == 0) return(dt)
      ## dedup BEFORE the merge - broad_q can legitimately contain
      ## entries for ranks other than rank_col too, so dedup only after
      ## subsetting down to this rank's actual matching names, not on
      ## the full broad_q up front (which could falsely flag two
      ## different-rank entries that happen to share a match_value as
      ## "duplicates" of each other).
      rank_q <- dedupe_lookup(rank_q, "match_value", paste0(rank_col, "-level"))
      
      setnames(rank_q, "match_value", rank_col)
      dt <- merge(dt, rank_q, by = rank_col, all.x = TRUE, suffixes = c("", paste0("_", tolower(rank_col))))
      dt[is.na(q_resolved) & !is.na(q), `:=`(q_resolved = q, match_level = paste0(rank_col, " (explicit entry)"))]
      dt[, q := NULL]
      dt
    }
    
    ## IMPORTANT ordering: processed rank-by-rank, most to least
    ## specific (Genus -> Family -> Order -> Class -> Phylum) - at each
    ## rank, BOTH the explicit entry (where that rank supports one) and
    ## the proximity fallback are tried before moving to a broader
    ## rank. This is what makes a genus-level proximity match correctly
    ## beat a broader but explicit order-level entry - rank specificity
    ## takes priority over whether the match was explicit vs inferred,
    ## matching "closest related by genus, family, order" as requested.
    ## 2. genus-level: explicit "Genus spp" entries, then proximity
    dt <- merge(dt, genus_q, by = "Genus", all.x = TRUE, suffixes = c("", "_gen"))
    dt[is.na(q_resolved) & !is.na(q), `:=`(q_resolved = q, match_level = "genus (spp entry)")]
    dt[, q := NULL]
    dt <- apply_proximity_at_rank(dt, "Genus")
    
    ## 3. family-level: proximity only (no "Family spp"-style notation)
    dt <- apply_proximity_at_rank(dt, "Family")
    
    ## 4. order-level: explicit entry, then proximity
    dt <- apply_explicit_at_rank(dt, "Order")
    dt <- apply_proximity_at_rank(dt, "Order")
    
    ## 5. class-level: explicit entry only
    dt <- apply_explicit_at_rank(dt, "Class")
    
    ## 6. phylum-level: explicit entry only
    dt <- apply_explicit_at_rank(dt, "Phylum")
    
    dt[, c("Genus", "Family", "Order", "Class", "Phylum") := NULL]
  } else if (nrow(genus_q) > 0 || nrow(broad_q) > 0) {
    message("WARNING: catchability_table has ", nrow(genus_q) + nrow(broad_q), " genus/order/class/phylum-level",
            " entries, but species_taxonomy wasn't provided - these, and the taxonomic-proximity fallback,",
            " can't be resolved and will fall back to default_q. Pass species_taxonomy to enable them.")
  }
  
  ## 7. exemptions - override everything above, always q=1
  n_exempt <- 0
  if (!is.null(exempt_fg_names) && "FG_name" %in% names(dt)) {
    n_exempt <- n_exempt + dt[FG_name %in% exempt_fg_names & (is.na(q_resolved) | q_resolved != 1), uniqueN(ScientificName)]
    dt[FG_name %in% exempt_fg_names, `:=`(q_resolved = 1, match_level = "exempt (FG)")]
  }
  if (!is.null(exempt_species)) {
    n_exempt <- n_exempt + dt[ScientificName %in% exempt_species & (is.na(q_resolved) | q_resolved != 1), uniqueN(ScientificName)]
    dt[ScientificName %in% exempt_species, `:=`(q_resolved = 1, match_level = "exempt (species)")]
  }
  if (n_exempt > 0) {
    message(n_exempt, " species forced to q=1 via exempt_fg_names/exempt_species",
            " (overrides any other match - e.g. pelagic/planktonic/algae groups a bottom trawl",
            " isn't designed to sample representatively).")
  }
  
  n_default <- dt[is.na(q_resolved) & !is.na(ScientificName), uniqueN(ScientificName)]
  if (n_default > 0) {
    message(n_default, " species have no matching entry in catchability_table at any level",
            " (including taxonomic proximity) - using default_q = ", default_q, " for these:")
    print(unique(dt[is.na(q_resolved) & !is.na(ScientificName), .(ScientificName)]))
  }
  dt[is.na(q_resolved), `:=`(q_resolved = default_q, match_level = "default_q")]
  
  message("\nCatchability match level breakdown (distinct species):")
  print(unique(dt[, .(ScientificName, match_level)])[, .N, by = match_level][order(-N)])
  
  dt[, Density := Density / q_resolved]
  dt[, Biomass := Biomass / q_resolved]
  dt[, c("q_resolved", "match_level") := NULL]
  message("\nCatchability correction applied - Density and Biomass divided by q",
          " (species-specific where available, default_q = ", default_q, " otherwise).")
  dt
}
## =================================================================
## Sample-level outlier removal - operates on individual (Sample,
## Species) observations, BEFORE any aggregation into FG/area sums, so
## a single anomalous haul doesn't get to inflate that entire year's
## FG density. Deliberately per-species (not per-FG): an outlier in one
## species shouldn't implicate other, unrelated species sharing the
## same FG. Compares each observation only against that SAME species'
## own history within the SAME area (different areas can have
## genuinely different typical densities for the same species, so
## comparing across areas would be misleading).
##
## IMPORTANT - log scale: catch data for schooling/aggregating species
## (e.g. horse mackerel, anchovy, boarfish) is naturally, heavily
## right-skewed - most hauls catch little to nothing, occasionally a
## haul hits a school and catches a lot, which is genuine ecology, not
## an error. On the raw scale this makes median/MAD both tiny, so even
## a real high catch produces a huge z-score - this was the actual
## cause of the excessive removal seen in testing (over 1000 "outliers"
## for common schooling species). Computing the z-score on log1p(Density)
## instead substantially reduces this, though testing against a
## realistic synthetic distribution (lognormal base + 2 genuine school
## events + 1 clear data error) showed even log-scale z-scores for
## genuine school events can reach ~40-60, while a clear error reached
## ~290 - there's real separation, just much further out than a naive
## threshold like 5 would suggest. threshold=70 is calibrated against
## that test (catches the clear error, doesn't flag either genuine
## event) - review what your own data's flagged/not-flagged split looks
## like and adjust from there, since the right value genuinely depends
## on how patchy your species' real catch distributions are.
##
## This can ACTUALLY REMOVE flagged observations (sets Density/Biomass
## to NA, so they're excluded from every downstream sum via the
## existing !is.na(Density) filters - not deleting the row, so there's
## still a record the sample existed) when drop_outliers=TRUE (the
## default), or just report them without touching the data when
## drop_outliers=FALSE - detected outliers are always printed either
## way, so switching this off is for "show me what would be removed
## without actually changing anything yet", not for silencing the
## detection.
## =================================================================

remove_sample_outliers <- function(dt, threshold = 70, min_samples = 5, drop_outliers = TRUE, log_scale = TRUE) {
  dt <- copy(dt)
  dt[, obs_id := .I]  # stable row identifier, survives the grouped computation below
  dt[, eval_value := if (log_scale) log1p(Density) else Density]
  
  stats <- dt[!is.na(eval_value) & !is.na(ScientificName),
              .(group_median = median(eval_value, na.rm = TRUE),
                group_mad = mad(eval_value, na.rm = TRUE),
                n_obs_in_group = .N),
              by = .(AreaID, ScientificName)]
  
  dt <- merge(dt, stats, by = c("AreaID", "ScientificName"), all.x = TRUE)
  dt[, robust_z := ifelse(!is.na(eval_value) & group_mad > 0,
                          abs(eval_value - group_median) / group_mad, NA_real_)]
  dt[, is_outlier := !is.na(robust_z) & robust_z > threshold & n_obs_in_group >= min_samples]
  
  flagged <- dt[is_outlier == TRUE, .(ScientificName, AreaID, Year, SampleID,
                                      flagged_density = Density, typical_density = expm1(group_median), robust_z)]
  setorder(flagged, -robust_z)
  flagged[, was_dropped := drop_outliers]
  
  if (nrow(flagged) > 0) {
    message("\n", nrow(flagged), " sample-level observation(s) flagged as high-confidence",
            " outliers (", if (log_scale) "log-scale " else "", "robust z-score > ", threshold,
            ", within at least ", min_samples, " observations of that species in that area to judge against).",
            if (drop_outliers) " REMOVED from the data (drop_outliers=TRUE)." else
              " NOT removed - drop_outliers=FALSE, this is a report only.",
            " Full detail (most extreme first):")
    print(flagged)
    
    message("\n", if (drop_outliers) "Outliers removed" else "Outliers flagged", ", by species:")
    print(flagged[, .(n = .N), by = ScientificName][order(-n)])
    
    if (drop_outliers) {
      dt[is_outlier == TRUE, `:=`(Density = NA_real_, Biomass = NA_real_)]
    }
  } else {
    message("\nNo sample-level observations exceeded the outlier threshold",
            " (robust z-score > ", threshold, ").")
  }
  
  dt[, c("obs_id", "eval_value", "group_median", "group_mad", "n_obs_in_group", "robust_z", "is_outlier") := NULL]
  attr(dt, "flagged_outliers") <- flagged
  dt
}

compute_fg_densities_by_stratum <- function(dt, strata = TRUE) {
  dt <- copy(dt)  # avoid data.table shallow-copy warning on := after this dt passed through merge()/subsetting upstream
  group_cols <- c("AreaID", "Year", if (strata) "Stratum", "FG_num", "FG_name")
  
  ## When strata=TRUE, exclude samples with Stratum=NA (depth outside
  ## strata_def's range, already flagged by assign_depth_stratum()'s
  ## own message) upfront - these can never match n_samples_by_stratum
  ## or strata_area_by_area (both only have valid stratum numbers), so
  ## leaving them in here just produces a second, confusing "no
  ## area-proportion" warning downstream about the same already-known
  ## cause rather than a genuinely new problem.
  base_dt <- if (strata) dt[!is.na(Stratum)] else dt
  
  per_sample_fg <- base_dt[
    !is.na(FG_num) & !is.na(Density),
    .(fg_density = sum(Density, na.rm = TRUE)),
    by = c("SampleID", group_cols)
  ]
  
  ## IMPORTANT: deliberately NOT computing n_samples here, even though
  ## SampleID is right there in per_sample_fg. uniqueN(SampleID) at
  ## this point would only count samples where THIS SPECIFIC FG had
  ## non-zero catch - understating the true sampling effort, since
  ## samples where the FG was genuinely absent still count toward the
  ## denominator in a proper mean-density estimator (they contribute a
  ## true zero, not a missing observation). The one correct source of
  ## sample count is n_samples_by_stratum, computed separately from the
  ## full dt (all species together, not filtered to one FG) - matches
  ## the original pipeline exactly, which computed n_hauls from the
  ## haul metadata table, never from the per-FG catch table.
  per_group_fg <- per_sample_fg[
    , .(density_sum = sum(fg_density, na.rm = TRUE)),
    by = group_cols
  ]
  
  message("Per-", if (strata) "area/stratum" else "area", "/year/FG density table built: ",
          nrow(per_group_fg), " rows.")
  per_group_fg
}

## =================================================================
## STEP 6: Weight densities per stratum (if stratification design) to
## get densities over FG, year, and area
## =================================================================
## Ported directly from compute_strata_fact_for_gsa() in the MEDITS
## pipeline - the bathymetry -> reclassify -> mask -> per-cell-area
## logic is unchanged (it was already generic: takes a strata
## definition and an area polygon as parameters), just renamed
## area_num/area_id_col to be explicit this isn't GSA-specific anymore.
## Uses raster::area() for latitude-corrected cell areas - grid cells
## are NOT equal-area (they shrink toward the poles), which matters
## when areas span different latitudes.

compute_strata_area_for_polygon <- function(area_poly, strata_def, resolution = 1) {
  if (nrow(area_poly) == 0) return(NULL)
  bbox <- sf::st_bbox(area_poly)
  
  bathy <- tryCatch(
    marmap::getNOAA.bathy(lon1 = bbox["xmin"], lon2 = bbox["xmax"],
                          lat1 = bbox["ymin"], lat2 = bbox["ymax"],
                          resolution = resolution),
    error = function(e) { message("  Bathymetry download failed: ", conditionMessage(e)); NULL }
  )
  if (is.null(bathy)) return(NULL)
  
  bathy_r <- marmap::as.raster(bathy)
  bathy_r <- raster::reclassify(bathy_r, cbind(0, Inf, NA), right = FALSE)
  bathy_r <- raster::mask(bathy_r, as(area_poly, "Spatial"))
  area_r <- raster::area(bathy_r)
  
  bathy_df <- as.data.frame(bathy_r, xy = TRUE); names(bathy_df)[3] <- "layer"
  area_df  <- as.data.frame(area_r,  xy = TRUE); names(area_df)[3]  <- "cell_area_km2"
  bathy_df <- merge(bathy_df, area_df, by = c("x", "y"))
  bathy_df <- bathy_df[complete.cases(bathy_df[, c("layer", "cell_area_km2")]), ]
  bathy_df$depth <- bathy_df$layer * -1
  bathy_df <- bathy_df[bathy_df$depth >= min(strata_def$depth_min) &
                         bathy_df$depth <= max(strata_def$depth_max), ]
  if (nrow(bathy_df) == 0) return(NULL)
  
  bathy_dt <- as.data.table(bathy_df)
  bathy_dt[, Stratum := strata_def$stratum_num[
    findInterval(depth, strata_def$depth_min, all.inside = TRUE)]]
  
  strata_areas <- bathy_dt[, .(area_km2 = sum(cell_area_km2, na.rm = TRUE)), by = Stratum]
  strata_areas[, prop := area_km2 / sum(area_km2)]
  setorder(strata_areas, Stratum)
  strata_areas[, .(Stratum, area_km2, prop)]
}

compute_strata_area_by_area <- function(area_ids, area_shp, area_id_col, strata_def,
                                        cache_path = NULL, resolution = 1) {
  if (is.character(area_shp)) area_shp <- sf::st_read(area_shp, quiet = TRUE)
  
  cache_valid <- !is.null(cache_path) && file.exists(cache_path) &&
    "area_km2" %in% names(fread(cache_path, nrows = 1))
  if (cache_valid) {
    message("Loading cached strata areas from ", cache_path)
    return(fread(cache_path))
  }
  
  message("Computing strata areas via bathymetry for ", length(area_ids), " area(s)",
          " (slow - downloads bathymetry once per area)...")
  results <- lapply(area_ids, function(a) {
    message(" Processing area ", a, "...")
    poly <- area_shp[area_shp[[area_id_col]] == a, ]
    res <- compute_strata_area_for_polygon(poly, strata_def, resolution)
    if (!is.null(res)) res[, AreaID := a]
    res
  })
  strata_area_by_area <- rbindlist(results, fill = TRUE)
  
  if (!is.null(cache_path)) {
    fwrite(strata_area_by_area, cache_path)
    message("Saved to ", cache_path, " - delete this file to force recomputation.")
  }
  strata_area_by_area
}

## the actual strata-weighted estimator - unchanged formula from the
## MEDITS pipeline: FG_density(area, year) = sum over strata of
## (summed FG density in stratum / n_samples in stratum) * area_proportion
weight_by_strata <- function(per_group_fg, n_samples_by_stratum, strata_area_by_area) {
  ## no suffix collision risk here - per_group_fg has only density_sum
  ## (see compute_fg_densities_by_stratum's own comment on why it
  ## deliberately doesn't compute its own sample count), so n_samples
  ## below is unambiguously n_samples_by_stratum's TOTAL sample count.
  merged <- merge(per_group_fg, n_samples_by_stratum,
                  by = c("AreaID", "Year", "Stratum"), all.x = TRUE)
  
  ## Kept BEFORE the strata_area_by_area merge/filter below, specifically
  ## for plot_strata_profile() - that plot's whole point is to sanity-check
  ## raw per-sample density across strata independent of whether a usable
  ## area-proportion could be computed for a given (AreaID, Stratum) (e.g.
  ## AquaMaps' 800-6000m pseudo-stratum can have valid sample-derived
  ## density but no/NA area for a given AreaID's bathymetry, which would
  ## otherwise make that stratum vanish from the plot with zero trace,
  ## rather than the "no area - genuinely expected or a real mismatch,
  ## worth checking" situation the WARNING two blocks down is about).
  per_stratum_raw <- merged[!is.na(n_samples) & n_samples > 0]
  
  merged <- merge(merged, strata_area_by_area, by = c("AreaID", "Stratum"), all.x = TRUE)
  
  n_missing <- merged[is.na(prop), .N]
  if (n_missing > 0) {
    missing_combos <- unique(merged[is.na(prop), .(AreaID, Stratum)])
    message("WARNING: ", n_missing, " row(s) across ", nrow(missing_combos), " distinct area/stratum",
            " combination(s) have no computed area-proportion - dropped from the weighted sum",
            " rather than treated as zero weight. This can be genuinely expected (e.g. a shallow",
            " area has no seafloor at all within a deep stratum's depth range, so there's nothing",
            " to compute an area for) or a sign of a real mismatch (e.g. AreaID type/values not",
            " lining up between your sample data and strata_area_by_area) - worth checking which:")
    print(missing_combos[order(AreaID, Stratum)])
  }
  merged <- merged[!is.na(prop) & !is.na(n_samples) & n_samples > 0]
  merged[, weighted_density := (density_sum / n_samples) * prop]
  
  fg_index <- merged[
    , .(mean_density = sum(weighted_density, na.rm = TRUE),
        n_strata_contributing = .N, n_samples_total = sum(n_samples)),
    by = .(AreaID, Year, FG_num, FG_name)
  ]
  attr(fg_index, "per_stratum") <- merged  # kept for weight_by_area() (needs area_km2/prop)
  attr(fg_index, "per_stratum_raw") <- per_stratum_raw  # kept for plot_strata_profile() (area-independent)
  message("Strata-weighted FG index built: ", nrow(fg_index), " rows.")
  fg_index
}

## When strata=FALSE, per_group_fg from Step 5 is already at
## area/year/FG level with no stratum dimension - just divide by
## n_samples per area/year to get a simple mean density, no weighting
## needed since there's no stratum structure to correct for.
## When strata=FALSE, per_group_fg from Step 5 has no stratum
## dimension - needs its own, separately-computed total sample count
## per (AreaID, Year), same correctness reasoning as the stratified
## path: this must come from the FULL dt (all species/FGs together),
## not from counting within the FG-filtered density table itself,
## which would only count samples where that specific FG had non-zero
## catch and understate the true sampling effort.
simple_area_density <- function(per_group_fg, n_samples_by_area) {
  merged <- merge(per_group_fg, n_samples_by_area, by = c("AreaID", "Year"), all.x = TRUE)
  merged <- merged[!is.na(n_samples) & n_samples > 0]
  merged[, mean_density := density_sum / n_samples]
  message("Simple (unstratified) per-area FG index built: ", nrow(merged), " rows.")
  merged[, .(AreaID, Year, FG_num, FG_name, mean_density, n_samples)]
}

## =================================================================
## STEP 7: Weight densities per area to get densities over FG and year
## (region-wide, all areas combined)
## =================================================================
## Ported from Section 5g of the MEDITS pipeline - the single-stage,
## area-weighted estimator: D_region = sum(D_ah * A_ah) / sum(A_ah)
## across every (area, stratum) cell directly, using each cell's
## ABSOLUTE area (not its proportion within its own area) - avoids
## compounding two separate weighting stages (strata-within-area, then
## area-within-region), which would otherwise double-weight incorrectly.
##
## Requires strata=TRUE (the per_stratum attribute from weight_by_strata()
## carries the area_km2 needed here). For strata=FALSE, region-wide
## aggregation would need each area's TOTAL surveyable area (not
## stratum-specific) as the weight instead - a different, simpler
## calculation not currently implemented here since it depends on what
## "area" means for a non-stratified survey; flag if you need this path.

weight_by_area <- function(fg_index_stratified) {
  per_stratum <- attr(fg_index_stratified, "per_stratum")
  if (is.null(per_stratum)) {
    stop("weight_by_area() needs the per_stratum attribute from weight_by_strata() -",
         " region-wide aggregation for strata=FALSE isn't implemented here (see comment above).")
  }
  fg_index_regional <- per_stratum[
    !is.na(area_km2),
    .(mean_density = sum((density_sum / n_samples) * area_km2, na.rm = TRUE) / sum(area_km2, na.rm = TRUE),
      n_areas_contributing = uniqueN(AreaID),
      n_samples_total = sum(n_samples)),
    by = .(Year, FG_num, FG_name)
  ]
  message("Region-wide (area-weighted) FG index built: ", nrow(fg_index_regional), " rows.")
  fg_index_regional
}

## =================================================================
## Excel export: region-wide density by FG x depth stratum, wide
## format (one row per FG, one column per stratum, plus Total_Density).
## =================================================================
## Same region-wide area weighting as weight_by_area() above - each
## stratum's column is that stratum's own ADDITIVE CONTRIBUTION to the
## final region-wide density (numerator restricted to that stratum,
## divided by the SAME grand-total area every stratum uses), so
## Total_Density = rowSums(stratum columns) reproduces weight_by_area()'s
## own mean_density exactly (averaged over year_filter if given) - this
## is deliberately NOT "mean density conditional on being in that
## stratum" (dividing by just that stratum's own area), which would NOT
## sum to the region total. Works on ANY strata_def/per_stratum pairing,
## including the AquaMaps-extended one (pass strata_def_for_plotting -
## see 01_survey_density_westmed.R Step 7b/8) - a stratum absent from a
## given FG's rows (e.g. no catch in that FG/stratum combo) becomes an
## explicit 0 column via dcast's fill, not a missing column.
## year_filter: e.g. YEAR_ECOPATH, to match how other summary sheets
## (density_in_base_years, net_density_multiplier) are already restricted
## to the pipeline's baseline period rather than averaged over every
## survey year on file. NULL averages over every year present.
build_fg_density_by_stratum_sheet <- function(fg_index_stratified, strata_def, year_filter = NULL) {
  per_stratum <- attr(fg_index_stratified, "per_stratum")
  if (is.null(per_stratum)) {
    stop("build_fg_density_by_stratum_sheet() needs the per_stratum attribute from weight_by_strata().")
  }
  d <- copy(per_stratum)
  if (!is.null(year_filter)) d <- d[Year %in% year_filter]
  d <- d[!is.na(area_km2)]
  
  ## Grand-total area per Year, de-duplicated to one row per
  ## (AreaID, Stratum, Year) first - d itself has one row per
  ## (AreaID, Year, Stratum, FG), so summing area_km2 straight off d
  ## would multiply it by however many FGs happen to be present.
  area_by_year <- unique(d[, .(AreaID, Stratum, Year, area_km2)])
  total_area_by_year <- area_by_year[, .(total_area = sum(area_km2, na.rm = TRUE)), by = Year]
  
  contrib <- d[, .(numerator = sum((density_sum / n_samples) * area_km2, na.rm = TRUE)),
               by = .(Year, FG_num, FG_name, Stratum)]
  contrib <- merge(contrib, total_area_by_year, by = "Year", all.x = TRUE)
  contrib[, density_contribution := ifelse(!is.na(total_area) & total_area > 0,
                                           numerator / total_area, NA_real_)]
  
  ## Average each stratum's contribution across the filtered years -
  ## additive (mean of sums == sum of means), so this still reproduces
  ## weight_by_area()'s own mean_density averaged over the same years.
  by_stratum <- contrib[, .(density_contribution = mean(density_contribution, na.rm = TRUE)),
                        by = .(FG_num, FG_name, Stratum)]
  
  by_stratum <- merge(by_stratum, strata_def, by.x = "Stratum", by.y = "stratum_num", all.x = TRUE)
  by_stratum[, stratum_label := fifelse(
    is.na(depth_min) | is.na(depth_max), paste0("Stratum_", Stratum, "_unknown_depth"),
    paste0(formatC(depth_min, format = "f", digits = 2), "_", formatC(depth_max, format = "f", digits = 2), "m"))]
  
  label_order <- unique(by_stratum[, .(Stratum, stratum_label)])[order(Stratum)]
  
  wide <- dcast(by_stratum, FG_num + FG_name ~ stratum_label, value.var = "density_contribution", fill = 0)
  stratum_cols <- label_order$stratum_label[label_order$stratum_label %in% names(wide)]
  setcolorder(wide, c("FG_num", "FG_name", stratum_cols))
  wide[, Total_Density := rowSums(.SD, na.rm = TRUE), .SDcols = stratum_cols]
  setorder(wide, FG_num)
  wide
}

## =================================================================
## CV-log weight for Ecosim's "Weight" row - approximates CV.log from
## fn.survey_to_ecosim_ts()'s Monte-Carlo-simulated abundance index:
## there, CV.log = sd(log(index+1)) / mean(log(index+1)) per year,
## averaged across years per FG, drawn from a delta-GLM/LSmeans
## simulated distribution. This pipeline doesn't fit that same model,
## so this is a SIMPLER, DIRECT analog: CV.log computed straight from
## whatever replicate observations exist at that survey's finest
## resolution for a given FG/year (MEDITS: per-haul density; MEDIAS:
## per-country/GSA density), log1p-transformed to match the reference's
## log(x+1) treatment, then averaged across years. It captures the same
## idea (higher variability -> lower confidence -> higher CV -> less
## weight in Ecosim) but is NOT a reproduction of the simulated CI
## width the reference computes.
##
## value_dt: any data.table with FG_num, FG_name, Year, and a numeric
## value_col holding one observation per replicate (a haul's density,
## a GSA's density, etc.) - NOT already aggregated to one row per
## FG/Year, since the within-year spread across replicates is exactly
## what's being measured.
## min_replicates: years with fewer than this many replicate
## observations for a given FG have no meaningful spread to compute a
## CV from - excluded from that FG's across-year average rather than
## contributing an NA/Inf.
## =================================================================
compute_cv_log_by_fg <- function(value_dt, value_col, min_replicates = 2) {
  value_dt <- copy(value_dt)
  value_dt[, log_value := log1p(get(value_col))]
  
  cv_by_year <- value_dt[
    , .(cv_log = sd(log_value, na.rm = TRUE) / mean(log_value, na.rm = TRUE), n = .N),
    by = .(FG_num, FG_name, Year)
  ]
  n_excluded <- cv_by_year[n < min_replicates, .N]
  if (n_excluded > 0) {
    message(n_excluded, " FG/Year combination(s) had fewer than ", min_replicates,
            " replicate observation(s) - excluded from that FG's CV.log average",
            " (no meaningful spread to measure from a single observation).")
  }
  cv_by_year <- cv_by_year[n >= min_replicates & is.finite(cv_log)]
  
  cv_by_fg <- cv_by_year[, .(cv_log = mean(cv_log, na.rm = TRUE), n_years = .N), by = .(FG_num, FG_name)]
  message("CV.log computed for ", nrow(cv_by_fg), " FG(s), averaged across years.")
  cv_by_fg
}

## =================================================================
## Study area / sample coverage map - study area polygons colored by
## sampling intensity (total distinct samples per area), with
## individual sample locations overlaid as points if Lat/Lon are
## available. Useful as a survey-design diagnostic - spotting coverage
## gaps, checking a new area's samples actually fall inside its own
## polygon (not a neighboring one), or just getting oriented before
## digging into the density results themselves.
##
## IMPORTANT - CRS handling: BOTH layers (area_shp and the sample
## points) are EXPLICITLY transformed to a common target CRS
## (points_crs, default WGS84/EPSG:4326) before plotting, rather than
## relying on geom_sf()/coord_sf()'s automatic layer-alignment. This
## is more robust: automatic alignment depends on every layer already
## having a valid, correctly-read CRS, and a shapefile can have a
## missing or malformed CRS (e.g. no .prj file, or one geopandas/sf
## doesn't parse cleanly) without erroring - it just silently plots in
## its own raw coordinate space, which produces exactly the "polygon
## appears as a tiny, misplaced shape in the corner while points show
## the real geography" symptom. This function checks area_shp's CRS
## explicitly and stops with a clear message if it's missing entirely,
## rather than producing a silently-broken plot.
## =================================================================

plot_sample_map <- function(dt, area_shp, area_id_col, title = "Sample locations and coverage by area",
                            by_year = FALSE, points_crs = 4326, selected_areas = NULL, show_land = TRUE,
                            land_on_top = FALSE) {
  message("area_shp CRS: ", if (is.na(st_crs(area_shp))) "MISSING/UNDEFINED" else st_crs(area_shp)$input)
  if (is.na(st_crs(area_shp))) {
    stop("area_shp has no CRS defined (st_crs(area_shp) is NA) - this is very likely the cause of a",
         " badly misplaced polygon layer (it gets plotted in raw, unprojected coordinate units",
         " instead of actual geographic space). Set it explicitly before calling this function,",
         " e.g. st_crs(area_shp) <- <the CRS the shapefile is actually supposed to be in> if you",
         " know it (check the shapefile's .prj file or its source documentation), or st_transform()",
         " it if it has a CRS but the wrong one.")
  }
  ## explicit, common target CRS for both layers - not relying on
  ## automatic alignment between geom_sf() layers
  area_shp <- st_transform(area_shp, points_crs)
  
  ## Display extent = union of area_shp's own bbox AND the sample
  ## points' own Lat/Lon distribution - NOT area_shp's bbox alone. A
  ## small custom region (a bounding box for one specific model,
  ## covering a fraction of the Mediterranean) would otherwise force
  ## the whole map to zoom tightly into just that box, losing the
  ## surrounding geographic context entirely - if dt itself carries the
  ## full, unfiltered survey distribution (not pre-filtered to the
  ## custom area), the map now shows that full context, with the
  ## smaller region's contour highlighted within it via selected_areas.
  area_bbox <- st_bbox(area_shp)
  if (all(c("Lat", "Lon") %in% names(dt)) && dt[!is.na(Lat) & !is.na(Lon), .N] > 0) {
    pts_bbox <- c(xmin = min(dt$Lon, na.rm = TRUE), xmax = max(dt$Lon, na.rm = TRUE),
                  ymin = min(dt$Lat, na.rm = TRUE), ymax = max(dt$Lat, na.rm = TRUE))
    display_bbox <- c(
      xmin = min(unname(area_bbox["xmin"]), pts_bbox["xmin"]),
      xmax = max(unname(area_bbox["xmax"]), pts_bbox["xmax"]),
      ymin = min(unname(area_bbox["ymin"]), pts_bbox["ymin"]),
      ymax = max(unname(area_bbox["ymax"]), pts_bbox["ymax"])
    )
  } else {
    display_bbox <- area_bbox
  }
  
  group_cols <- if (by_year) c("AreaID", "Year") else "AreaID"
  intensity <- dt[, .(n_samples = uniqueN(SampleID)), by = group_cols]
  
  if (by_year) {
    ## need every (area, year) combination present, even 0-sample ones,
    ## so faceted panels don't just silently omit years/areas with no data
    all_areas <- unique(area_shp[[area_id_col]])
    all_years <- sort(unique(dt$Year))
    full_grid <- data.table(expand.grid(AreaID = all_areas, Year = all_years))
    intensity <- merge(full_grid, intensity, by = c("AreaID", "Year"), all.x = TRUE)
    intensity[is.na(n_samples), n_samples := 0]
    
    area_shp_plot <- do.call(rbind, lapply(all_years, function(yr) {
      piece <- area_shp
      piece$Year <- yr
      piece
    }))
    area_shp_plot <- merge(area_shp_plot, intensity, by.x = c(area_id_col, "Year"), by.y = c("AreaID", "Year"), all.x = TRUE)
  } else {
    area_shp_plot <- merge(area_shp, intensity, by.x = area_id_col, by.y = "AreaID", all.x = TRUE)
    area_shp_plot$n_samples[is.na(area_shp_plot$n_samples)] <- 0
  }
  
  p <- ggplot()
  
  ## Land basemap. Clipped to display_bbox (plus padding) - the FULL
  ## extent computed above, not just area_shp's own bbox - so the whole
  ## world isn't downloaded/rendered, just the region actually shown on
  ## the map, while still covering the full sample distribution.
  ##
  ## land_on_top: when area_shp is a simple rectangle (a bounding-box
  ## custom region, not a real coastline-aware shapefile like the GFCM
  ## GSA polygons), the rectangle can genuinely overlap land - GSAs are
  ## official marine-zone boundaries that don't, so land drawn first
  ## (underneath, the default) is normally correct there. For a bbox,
  ## draw land AFTER (on top of) the intensity fill instead, so any
  ## part of the rectangle that's actually land is visibly masked -
  ## only marine area should read as part of the study region, and a
  ## plain rectangle can't know the coastline to exclude it itself.
  build_land_layer <- function() {
    tryCatch({
      pad <- 0.5
      xlim <- c(unname(display_bbox["xmin"]) - pad, unname(display_bbox["xmax"]) + pad)
      ylim <- c(unname(display_bbox["ymin"]) - pad, unname(display_bbox["ymax"]) + pad)
      
      ## Temporarily disable S2 spherical geometry for the crop
      ## specifically - S2 (sf's default) treats polygon edges as
      ## great-circle arcs on the sphere, which can introduce visible
      ## curvature in a rendered edge, particularly right where a crop
      ## boundary cuts through a coastline and for lower-resolution
      ## coastline data (this fallback's rnaturalearth scale=50, or the
      ## maps package fallback, are both simplified/lower-detail than a
      ## full-resolution coastline, where the difference between a
      ## great-circle arc and a straight line becomes more visually
      ## apparent over a longer, under-resolved edge segment). GEOS/
      ## planar geometry (S2 off) crops with straight-line edges
      ## instead. Restored via on.exit() regardless of how this
      ## function exits, so it doesn't affect anything else in the
      ## library that may depend on S2 being on (e.g. accurate area
      ## calculations elsewhere).
      old_s2 <- sf::sf_use_s2()
      sf::sf_use_s2(FALSE)
      on.exit(sf::sf_use_s2(old_s2), add = TRUE)
      
      if (requireNamespace("rnaturalearth", quietly = TRUE)) {
        countries <- rnaturalearth::ne_countries(scale = 50, returnclass = "sf")
        countries <- st_transform(countries, points_crs)
        suppressWarnings(st_crop(countries, c(xmin = xlim[1], xmax = xlim[2], ymin = ylim[1], ymax = ylim[2])))
      } else if (requireNamespace("maps", quietly = TRUE)) {
        ## fallback - lower-resolution coastline, but doesn't need
        ## rnaturalearth/rnaturalearthdata installed. maps' own "world"
        ## database commonly has invalid/self-intersecting geometries
        ## once converted to sf (a known issue with this data source),
        ## so st_make_valid() is required before cropping - without it
        ## st_crop() throws a geometry error and the land layer would
        ## silently fail to render, right back to the "transparent
        ## land" symptom this is meant to fix.
        message("rnaturalearth not installed - using maps package as a lower-resolution",
                " fallback for the land basemap (install.packages('rnaturalearth') for better detail).")
        world_map <- maps::map("world", plot = FALSE, fill = TRUE)
        world_sf <- sf::st_as_sf(world_map)
        st_crs(world_sf) <- 4326
        world_sf <- st_make_valid(world_sf)
        world_sf <- st_transform(world_sf, points_crs)
        suppressWarnings(st_crop(world_sf, c(xmin = xlim[1], xmax = xlim[2], ymin = ylim[1], ymax = ylim[2])))
      } else {
        message("Neither rnaturalearth nor maps is installed - skipping land basemap layer",
                " (install either package to enable, or set show_land=FALSE to silence this).")
        NULL
      }
    }, error = function(e) {
      message("Land basemap failed to load (", conditionMessage(e), ") - continuing without it.")
      NULL
    })
  }
  
  land <- if (show_land) build_land_layer() else NULL
  if (!is.null(land) && !land_on_top) {
    p <- p + geom_sf(data = land, fill = "grey85", color = "grey60", linewidth = 0.2)
  }
  
  p <- p +
    geom_sf(data = area_shp_plot, aes(fill = n_samples), color = "grey40", linewidth = 0.3) +
    scale_fill_gradient(name = "Samples", low = "white", high = "#54278f", na.value = "grey90") +
    ## default_crs makes the coordinate handling explicit (matching
    ## points_crs) rather than relying on coord_sf()'s own default
    ## logic for an unprojected/geographic CRS, which can otherwise
    ## interpolate long polygon edges as curved great-circle arcs
    ## rather than straight lines in the displayed projection.
    coord_sf(xlim = c(unname(display_bbox["xmin"]) - 0.3, unname(display_bbox["xmax"]) + 0.3),
             ylim = c(unname(display_bbox["ymin"]) - 0.3, unname(display_bbox["ymax"]) + 0.3),
             default_crs = sf::st_crs(points_crs)) +
    labs(title = title) +
    theme_minimal(base_size = 11) +
    theme(
      axis.title = element_blank(),
      ## graticule (lat/lon reference lines) drawn ON TOP of the land
      ## and GSA fill layers via panel.ontop - otherwise ggplot's
      ## default draws grid lines BEHIND the data, where they're
      ## completely covered/invisible under any opaque fill.
      ## panel.background needs fill=NA so the layers underneath still
      ## show through once the grid panel is brought to the front.
      panel.ontop = TRUE,
      panel.background = element_rect(fill = NA),
      panel.grid.major = element_line(color = scales::alpha("black", 0.4), linetype = "dashed", linewidth = 0.3),
      panel.grid.minor = element_line(color = scales::alpha("black", 0.25), linetype = "dashed", linewidth = 0.2),
      ## axis text (lat/lon tick labels) sits in the plot margin,
      ## OUTSIDE the transparent panel above - but with panel.ontop
      ## drawing the map content right up to that boundary, dark text
      ## in the default theme_minimal() grey can get lost against a
      ## busy map edge. Solid white plot.background plus black,
      ## slightly bolder axis text keeps these readable regardless of
      ## what's rendered directly under the panel.
      plot.background = element_rect(fill = "white", color = NA),
      axis.text = element_text(color = "black", face = "plain")
    )
  
  ## A gradient FILL is meaningless with only a single polygon (the
  ## custom-region bounding-box case) - one feature, one value, so the
  ## whole box just renders as one flat color with no visual gradation
  ## at all, unlike the westmed GSA case where 11 differently-shaded
  ## polygons genuinely show relative coverage. When there's only one
  ## area, print the actual sample count as a text label on the map
  ## instead of relying on a fill scale that can't convey anything
  ## with a single value.
  n_distinct_areas <- length(unique(area_shp_plot[[area_id_col]]))
  if (n_distinct_areas == 1) {
    message("Only one area in area_shp_plot (the custom-region case) - the fill gradient can't",
            " show meaningful variation with a single polygon, so the sample count is also",
            " printed directly on the map as a text label.")
    centroid_pts <- suppressWarnings(sf::st_centroid(area_shp_plot))
    p <- p + geom_sf_text(data = centroid_pts, aes(label = paste0(n_samples, " samples")),
                          color = "grey15", fontface = "bold", size = 4.5)
  }
  
  ## land_on_top handling moved to AFTER the selected-area contour
  ## block below - land needs to sit on top of the contour too, not
  ## just the intensity fill, so it's drawn last (right before the
  ## sample points, which always stay on top of everything).
  
  ## Highlight the areas actually being analyzed (e.g. FILTER_AREAS) -
  ## outer contour only, not the internal lines where individual
  ## selected GSAs border each other.
  ##
  ## IMPORTANT: plain st_union() can still leave visible artifacts at
  ## the seams between adjacent polygons in a REAL shapefile, even
  ## though it works cleanly on hand-built, perfectly-aligned test
  ## polygons - real GIS data is rarely perfectly topologically snapped
  ## (there can be tiny gaps/overlaps of a few map units at a shared
  ## boundary from how the shapefile was originally digitized), and
  ## st_union() preserves that imprecision rather than resolving it.
  ## Rasterizing the unioned shape to a grid, then re-polygonizing with
  ## dissolve=TRUE, sidesteps this: grid cells are simply inside or
  ## outside the shape regardless of sub-cell vector imprecision, which
  ## smooths over exactly the kind of seam artifact st_union() alone
  ## can leave behind.
  selected_shp <- NULL
  if (!is.null(selected_areas)) {
    selected_shp <- area_shp[area_shp[[area_id_col]] %in% selected_areas, ]
    if (nrow(selected_shp) == 0) {
      message("WARNING: none of selected_areas matched any value in area_shp[[area_id_col]] -",
              " check selected_areas uses the same type/values as ", area_id_col, ".")
      selected_shp <- NULL
    } else {
      unioned <- st_union(selected_shp)
      selected_outline <- tryCatch({
        v <- terra::vect(unioned)
        r <- terra::rast(terra::ext(v), resolution = 0.01, crs = terra::crs(v))
        r <- terra::rasterize(v, r, field = 1)
        v_poly <- terra::as.polygons(r, dissolve = TRUE)
        clean_shp <- sf::st_as_sf(v_poly)
        clean_shp <- clean_shp[which.max(sf::st_area(clean_shp)), ]  # keep only the largest piece, drop tiny slivers
        sf::st_boundary(clean_shp)
      }, error = function(e) {
        message("Rasterize-based contour cleanup failed (", conditionMessage(e), ") - falling back",
                " to plain st_union(), which may still show minor seam artifacts.")
        unioned
      })
      p <- p + geom_sf(data = selected_outline, fill = NA, color = "black", linewidth = 0.8)
    }
  }
  
  ## land_on_top: draw land LAST here (after both the intensity fill
  ## AND the selected-area contour above), so it visibly masks any part
  ## of a simple bounding-box "study area" - contour included - that's
  ## actually land. Only marine area should read as part of the study
  ## region, and a plain rectangle (unlike the GFCM GSA polygons) has
  ## no way to exclude land on its own.
  if (!is.null(land) && land_on_top) {
    p <- p + geom_sf(data = land, fill = "grey85", color = "grey60", linewidth = 0.2)
  }
  
  if (by_year) p <- p + facet_wrap(vars(Year))
  
  if (all(c("Lat", "Lon") %in% names(dt))) {
    sample_pts_dt <- unique(dt[!is.na(Lat) & !is.na(Lon), c("SampleID", "Lat", "Lon", if (by_year) "Year"), with = FALSE])
    sample_pts_sf <- st_as_sf(sample_pts_dt, coords = c("Lon", "Lat"), crs = points_crs, remove = FALSE)
    message("Sample points CRS (as constructed): ", st_crs(sample_pts_sf)$input)
    
    ## diagnostic: how many points fall genuinely outside every polygon,
    ## now that both layers are confirmed in the same, explicit CRS -
    ## distinguishes "was actually a projection problem" (this count
    ## should be near 0 once fixed) from "some points are genuinely bad
    ## data" (stays nonzero even with CRS handled correctly)
    within_any <- lengths(st_intersects(sample_pts_sf, area_shp)) > 0
    n_outside <- sum(!within_any)
    if (n_outside > 0) {
      message(n_outside, " of ", nrow(sample_pts_dt), " sample point(s) fall outside EVERY area polygon",
              " even with both layers in the same CRS - these are likely genuine data issues",
              " (bad coordinates, wrong sign, swapped lat/lon), not a projection mismatch:")
      print(sample_pts_dt[!within_any][, .(SampleID)])
    } else {
      message("All ", nrow(sample_pts_dt), " sample points fall within at least one area",
              " polygon - no evidence of a projection mismatch or bad coordinates.")
    }
    
    if (!is.null(selected_shp)) {
      ## a DIFFERENT distinction from within_any above - a point can be
      ## inside some non-selected GSA (within_any=TRUE) while still
      ## being outside the actual study area (selected_areas). This is
      ## the one that matters for "am I actually looking at my study
      ## area's own coverage" rather than just "is this a valid
      ## coordinate at all".
      within_selected <- lengths(st_intersects(sample_pts_sf, st_union(selected_shp))) > 0
      sample_pts_sf$in_study_area <- ifelse(within_selected, "Inside study area", "Outside study area")
      n_out_of_study <- sum(!within_selected)
      message(n_out_of_study, " of ", nrow(sample_pts_dt), " sample point(s) fall outside the",
              " SELECTED study area (selected_areas) specifically, shown in a different color",
              " below - separate from the projection/bad-data diagnostic above, since a point can be",
              " a perfectly valid coordinate in a real, neighboring GSA and still be outside this",
              " particular analysis's scope.")
      ## shape = 4 (an "x" cross) for BOTH categories - color is what
      ## distinguishes inside/outside here, not shape. Fixed as a
      ## constant param rather than mapped via aes() specifically so
      ## every point renders the same symbol regardless of category;
      ## only scale_color_manual varies by in_study_area. size/stroke
      ## sit between two failure modes: too small (0.6/0.7, the
      ## original) and an "x" is indistinguishable from a dot; too
      ## opaque and hundreds of overlapping points blend into a solid
      ## haze regardless of shape. alpha does more work than size here
      ## for reducing overplotting clutter specifically - dropped
      ## further (0.4 -> 0.25) while keeping size/stroke just large
      ## enough that individual crosses still read as crosses.
      p <- p + geom_sf(data = sample_pts_sf, aes(color = in_study_area),
                       shape = 4, size = 0.9, stroke = 0.8, alpha = 0.25) +
        scale_color_manual(name = NULL, values = c("Inside study area" = "black", "Outside study area" = "red3")) +
        ## legend symbols shown larger and fully opaque than the actual
        ## map points (which stay small/semi-transparent to reduce
        ## overplotting clutter with many samples) - purely a legend-
        ## readability fix, the plotted points themselves are unaffected
        guides(color = guide_legend(override.aes = list(size = 3, alpha = 1, shape = 4, stroke = 1.3)))
    } else {
      p <- p + geom_sf(data = sample_pts_sf, shape = 4, size = 0.9, stroke = 0.8, alpha = 0.12, color = "black")
    }
    message("Plotted ", nrow(sample_pts_dt), " distinct sample location(s)",
            if (by_year) paste0(" across ", uniqueN(sample_pts_dt$Year), " year(s)") else "", ".")
  } else {
    message("No Lat/Lon columns in dt - showing area-level intensity shading only,",
            " no individual sample points.")
  }
  
  print(intensity[order(-n_samples)])
  p
}

## =================================================================
## STEP 8: Output plots
## =================================================================
## Generalized from the MEDITS pipeline's plotting section - same
## top-N filtering logic and faceted-timeseries style, just with
## generic FG_num/FG_name/AreaID/Year/mean_density column names
## instead of MEDITS-specific ones.

filter_top_n <- function(df, group_col, value_col, area_col = NULL, top_n_areas = NULL, top_n_groups = 40) {
  if (!is.null(area_col) && !is.null(top_n_areas)) {
    top_areas <- df %>% group_by(.data[[area_col]]) %>%
      dplyr::summarise(tot = sum(.data[[value_col]], na.rm = TRUE), .groups = "drop") %>%
      slice_max(tot, n = top_n_areas) %>% pull(.data[[area_col]])
    df <- filter(df, .data[[area_col]] %in% top_areas)
  }
  top_groups <- df %>% group_by(.data[[group_col]]) %>%
    dplyr::summarise(tot = sum(.data[[value_col]], na.rm = TRUE), .groups = "drop") %>%
    slice_max(tot, n = top_n_groups) %>% pull(.data[[group_col]])
  filter(df, .data[[group_col]] %in% top_groups)
}

plot_fg_timeseries_by_area <- function(fg_index, title = "FG density by area", y_lab = "Density",
                                       top_n_areas = 20, top_n_fg = 40) {
  plot_data <- filter_top_n(as_tibble(fg_index), "FG_name", "mean_density",
                            area_col = "AreaID", top_n_areas = top_n_areas, top_n_groups = top_n_fg)
  ggplot(plot_data, aes(x = Year, y = mean_density, color = as.factor(AreaID))) +
    geom_line(linewidth = 0.6, alpha = 0.7) +
    facet_wrap(vars(FG_name), scales = "free_y") +
    labs(title = title, x = "Year", y = y_lab, color = "Area") +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom", axis.text.x = element_text(angle = 45, hjust = 1)) +
    ## legend lines shown thicker and fully opaque than the actual plot
    ## lines (which are thin/semi-transparent to reduce clutter with
    ## many areas overlapping) - purely a legend-readability fix, the
    ## plotted data itself is unaffected
    guides(color = guide_legend(override.aes = list(linewidth = 3, alpha = 1)))
}

plot_fg_timeseries_regional <- function(fg_index_regional, title = "FG density, region-wide",
                                        y_lab = "Area-weighted density", top_n_fg = 40) {
  fg_totals <- fg_index_regional[, .(tot = sum(mean_density, na.rm = TRUE)), by = FG_name]
  top_fg <- fg_totals[order(-tot)][seq_len(min(top_n_fg, .N)), FG_name]
  plot_data <- as_tibble(fg_index_regional[FG_name %in% top_fg])
  ggplot(plot_data, aes(x = Year, y = mean_density)) +
    geom_line(linewidth = 0.6, alpha = 0.8, colour = "steelblue") +
    facet_wrap(vars(FG_name), scales = "free_y") +
    labs(title = title, x = "Year", y = y_lab) +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

## Depth-strata validation plot: raw per-sample density across strata,
## from weight_by_strata()'s per_stratum_raw attribute - deliberately
## AREA-INDEPENDENT (unlike the per_stratum attribute weight_by_area()
## uses), so a stratum with valid sample data but no computed area-
## proportion (e.g. AquaMaps' 800-6000m pseudo-stratum, for an AreaID
## whose bathymetry didn't yield a usable area there) still shows up
## here instead of silently vanishing - this plot's whole point is to
## sanity-check density BEFORE any area weighting, so it should never be
## gated on area being computable. Falls back to the older per_stratum
## attribute (with a warning) for callers on a stale weight_by_strata().
plot_strata_profile <- function(fg_index_stratified, strata_def, top_n_fg = 40) {
  per_stratum <- attr(fg_index_stratified, "per_stratum_raw")
  if (is.null(per_stratum)) {
    warning("[plot_strata_profile] No per_stratum_raw attribute found (stale weight_by_strata()?) -",
            " falling back to the area-merged per_stratum attribute, which silently drops any",
            " stratum with no computed area-proportion. Re-source lib_survey_fg_density_functions.R",
            " to get the area-independent version of this plot.")
    per_stratum <- attr(fg_index_stratified, "per_stratum")
  }
  if (is.null(per_stratum)) stop("plot_strata_profile() needs the per_stratum_raw (or per_stratum) attribute from weight_by_strata().")
  per_stratum <- copy(per_stratum)  # avoid data.table shallow-copy warning - this came from an attribute set after a merge() chain inside weight_by_strata()
  
  per_stratum[, mean_density_per_sample := density_sum / n_samples]
  profile <- per_stratum[, .(mean_density_per_sample = mean(mean_density_per_sample, na.rm = TRUE)),
                         by = .(FG_num, FG_name, Stratum)]
  
  ## Strata present in the data but absent from strata_def would silently
  ## merge to NA depth_min/depth_max and render as "N (NA-NAm)" - surface
  ## that loudly instead, since it means the wrong strata_def was passed
  ## in (e.g. the plain MEDITS_STRATA when AquaMaps pseudo-strata are
  ## active) rather than a real "no data" situation.
  unmatched_strata <- setdiff(unique(profile$Stratum), strata_def$stratum_num)
  if (length(unmatched_strata) > 0) {
    warning("[plot_strata_profile] Stratum value(s) ", paste(sort(unmatched_strata), collapse = ", "),
            " have data but no matching row in strata_def (which only defines stratum_num ",
            paste(sort(strata_def$stratum_num), collapse = ", "), ") - their depth range will show as",
            " 'unknown depth range' below rather than a real m range. Pass the strata_def that matches",
            " what actually built fg_index_stratified (e.g. the AquaMaps-extended strata_def, not the",
            " plain 5-row MEDITS_STRATA, whenever the AquaMaps depth adjustment was applied).")
  }
  
  profile <- merge(profile, strata_def, by.x = "Stratum", by.y = "stratum_num", all.x = TRUE)
  ## round to 2 decimals for the axis label - the underlying depth_min/
  ## depth_max can carry long floating-point tails (e.g. the AquaMaps
  ## shallow pseudo-stratum's depth_max = 10 - 0.0001 = 9.9999...) that
  ## are meaningless past 2 decimals for a depth label.
  profile[, depth_label := fifelse(
    is.na(depth_min) | is.na(depth_max), "unknown depth range",
    paste0(formatC(depth_min, format = "f", digits = 2), "-", formatC(depth_max, format = "f", digits = 2), "m"))]
  profile[, stratum_label := paste0(Stratum, " (", depth_label, ")")]
  profile[, stratum_label := factor(stratum_label, levels = unique(stratum_label[order(Stratum)]))]
  
  top_fg <- profile[, .(tot = sum(mean_density_per_sample, na.rm = TRUE)), by = FG_name][
    order(-tot)][seq_len(min(top_n_fg, .N)), FG_name]
  
  ggplot(profile[FG_name %in% top_fg], aes(x = stratum_label, y = mean_density_per_sample, fill = Stratum)) +
    geom_col() +
    scale_fill_gradient(low = "lightblue", high = "navy", guide = "none") +
    facet_wrap(vars(FG_name), scales = "free_y") +
    labs(title = "Depth-strata density profile by FG", x = "Depth stratum", y = "Mean density per sample") +
    theme_minimal(base_size = 10) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1))
}

## =================================================================
## Species-level regional density (for the FG_spp Excel sheet - species
## breakdown WITHIN each FG, not just the FG total). Same strata- and
## area-weighting logic as the FG-level path, just grouped by
## ScientificName (nested within FG) instead of FG alone. Mirrors
## Section 5h of the MEDITS pipeline.
## =================================================================

compute_species_densities_by_stratum <- function(dt, strata = TRUE) {
  dt <- copy(dt)  # avoid data.table shallow-copy warning on := after this dt passed through merge()/subsetting upstream
  group_cols <- c("AreaID", "Year", if (strata) "Stratum", "FG_num", "FG_name", "ScientificName")
  
  ## same reasoning as compute_fg_densities_by_stratum(): exclude
  ## Stratum=NA samples upfront when strata=TRUE, since they can never
  ## match n_samples_by_stratum/strata_area_by_area anyway
  base_dt <- if (strata) dt[!is.na(Stratum)] else dt
  
  per_sample_sp <- base_dt[
    !is.na(FG_num) & !is.na(ScientificName) & !is.na(Density),
    .(sp_density = sum(Density, na.rm = TRUE)),
    by = c("SampleID", group_cols)
  ]
  ## same reasoning as compute_fg_densities_by_stratum(): deliberately
  ## no n_samples here - a species-specific sample count would only
  ## count samples where THIS species had non-zero catch, understating
  ## true sampling effort. The one correct source is n_samples_by_stratum,
  ## computed from the full dt (this is also what was causing "Object
  ## 'n_samples' not found. Perhaps you intended [n_samples.x, n_samples.y]"
  ## in weight_species_by_area() - two different n_samples colliding on
  ## merge with no suffix specified).
  per_sample_sp[, .(density_sum = sum(sp_density, na.rm = TRUE)), by = group_cols]
}

weight_species_by_area <- function(per_group_sp, n_samples_by_stratum, strata_area_by_area) {
  ## no suffix collision risk - per_group_sp has only density_sum now,
  ## so n_samples below is unambiguously the total from n_samples_by_stratum
  merged <- merge(per_group_sp, n_samples_by_stratum, by = c("AreaID", "Year", "Stratum"), all.x = TRUE)
  merged <- merge(merged, strata_area_by_area, by = c("AreaID", "Stratum"), all.x = TRUE)
  merged <- merged[!is.na(prop) & !is.na(area_km2) & !is.na(n_samples) & n_samples > 0]
  
  merged[, .(mean_density = sum((density_sum / n_samples) * area_km2, na.rm = TRUE) / sum(area_km2, na.rm = TRUE)),
         by = .(Year, FG_num, FG_name, ScientificName)]
}

## =================================================================
## STEP 9: Output Excel file (Ecopath/Ecosim-ready workbook)
## =================================================================
## =================================================================
## upsert_workbook_sheets() / read_existing_ts_years()
##
## Shared building block for progressively assembling ONE workbook
## (output/ecopath_ecosim_inputs.xlsx) from THREE independent scripts
## that can run in ANY order - 01_survey_density_westmed.R (Biomass:
## FG_spp_Ecopath/Ecopath/Ecosim/FG_spp_Ecosim), 02_fao_catches.R
## (Catches_Ecopath/Catches_Ecosim), 04_pbqb_calc.R (PB_QB). None of them
## needs to run before/after the others, and none of them should be
## able to silently erase what another one already wrote.
##
## This replaces the previous approach (export_ecopath_ecosim_excel()
## calling openxlsx::write.xlsx() directly), which unconditionally
## overwrote the ENTIRE file - if 02_fao_catches.R ran first and wrote
## Catches_Ecopath/Catches_Ecosim, then 01_survey_density_westmed.R ran
## and called write.xlsx(), it would have silently wiped those two
## sheets out. upsert_workbook_sheets() loads the existing file if
## there is one and only touches the sheet names it's given.
##
##   sheets   - named list of data.frames/data.tables; each name
##              becomes a sheet name. A sheet that already exists in
##              the workbook under that name is REPLACED (safe to
##              re-run the same script repeatedly); any OTHER,
##              differently-named sheet already present is always
##              left untouched.
##   out_path - path to the shared workbook
## =================================================================
upsert_workbook_sheets <- function(sheets, out_path) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("openxlsx is required to write/append to '", out_path, "'.")
  }
  if (is.null(names(sheets)) || any(names(sheets) == "")) {
    stop("upsert_workbook_sheets(): every element of `sheets` needs a name",
         " (used as the sheet name) - got: ", paste(names(sheets), collapse = ", "))
  }
  
  if (file.exists(out_path)) {
    wb <- openxlsx::loadWorkbook(out_path)
    message("Found existing workbook at '", out_path, "' (sheets: ",
            paste(openxlsx::getSheetNames(out_path), collapse = ", "), ") - adding/replacing: ",
            paste(names(sheets), collapse = ", "), ". Other sheets left untouched.")
  } else {
    wb <- openxlsx::createWorkbook()
    if (!dir.exists(dirname(out_path))) dir.create(dirname(out_path), recursive = TRUE)
    message("No existing workbook at '", out_path, "' - creating a NEW one with ONLY: ",
            paste(names(sheets), collapse = ", "), ". Sheets from the OTHER pipeline",
            " scripts (survey/catches/PB-QB, whichever haven't run against this exact",
            " path yet) will be missing until they are.")
  }
  
  for (nm in names(sheets)) {
    if (nm %in% names(wb)) {
      openxlsx::removeWorksheet(wb, nm)
      message("Sheet '", nm, "' already existed in the workbook - replaced.")
    }
    openxlsx::addWorksheet(wb, nm)
    openxlsx::writeData(wb, nm, sheets[[nm]], colNames = TRUE)
  }
  
  openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)
  message("Saved '", out_path, "' (sheets now: ", paste(names(wb), collapse = ", "), ")")
  invisible(wb)
}

## =================================================================
## finalize_workbook_sheet_order()
##
## Fixes sheet NAMES and ORDER in ecopath_ecosim_inputs.xlsx to a
## fixed target layout. Run this LAST, after every upstream script
## that writes to the workbook (01_survey_density_*.R, 02_fao_catches.R,
## 04_pbqb_calc.R, and whatever writes Ecobase) has already run -
## upsert_workbook_sheets() itself is order-independent (each script
## just adds/replaces its own sheets wherever the workbook happens to
## be), so nothing upstream ever needs to know about final order.
##
## rename_map: named character vector, c(old_name = new_name). Applied
## BEFORE reordering, so target_order should use the NEW names.
## target_order: character vector of sheet names in the desired final
## order (using the NEW names, post-rename).
##
## Sheets in target_order that don't exist in the workbook are skipped
## with a message (not an error) - upstream scripts run at different
## times, so not every sheet is guaranteed to exist yet.
## Sheets that exist in the workbook but are NOT in target_order are
## kept and appended at the end, in their original relative order -
## never silently dropped, just flagged with a warning so an
## unexpected/forgotten sheet doesn't slip by unnoticed.
## =================================================================
finalize_workbook_sheet_order <- function(out_path, rename_map = character(0), target_order) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("openxlsx is required to finalize sheet order for '", out_path, "'.")
  }
  if (!file.exists(out_path)) {
    stop("finalize_workbook_sheet_order(): workbook not found at '", out_path, "' - ",
         "run the upstream export scripts first.")
  }
  
  wb <- openxlsx::loadWorkbook(out_path)
  current_names <- names(wb)
  message("finalize_workbook_sheet_order(): workbook currently has ", length(current_names),
          " sheet(s): ", paste(current_names, collapse = ", "))
  
  ## --- renames ------------------------------------------------------------
  if (length(rename_map) > 0) {
    missing_to_rename <- setdiff(names(rename_map), current_names)
    if (length(missing_to_rename) > 0) {
      message("finalize_workbook_sheet_order(): rename requested for sheet(s) not present",
              " (skipped): ", paste(missing_to_rename, collapse = ", "))
    }
    for (old_nm in intersect(names(rename_map), current_names)) {
      new_nm <- rename_map[[old_nm]]
      if (new_nm %in% setdiff(names(wb), old_nm)) {
        ## This is expected on a RE-run, not a genuine conflict: an earlier
        ## call to this same function already renamed old_nm -> new_nm, and
        ## since then an upstream script re-ran upsert_workbook_sheets() and
        ## wrote fresh data back under old_nm again (upsert always writes to
        ## the pre-rename literal name - it has no way to know a previous
        ## finalize_workbook_sheet_order() call already renamed it). So
        ## new_nm here is STALE data from the previous run, and old_nm is
        ## this run's current data - old_nm wins. Drop the stale new_nm
        ## sheet, then proceed with the rename as normal.
        message("finalize_workbook_sheet_order(): '", new_nm, "' already exists (leftover from",
                " a previous rename) while '", old_nm, "' has fresh data from this run - replacing",
                " the stale '", new_nm, "' with '", old_nm, "''s current content rather than erroring.")
        openxlsx::removeWorksheet(wb, new_nm)
      }
      openxlsx::renameWorksheet(wb, sheet = old_nm, newName = new_nm)
      message("Renamed sheet '", old_nm, "' -> '", new_nm, "'")
    }
  }
  
  ## --- reorder --------------------------------------------------------------
  current_names <- names(wb)  # re-read post-rename
  dupe_targets <- target_order[duplicated(target_order)]
  if (length(dupe_targets) > 0) {
    warning("finalize_workbook_sheet_order(): target_order has duplicate name(s) - using only",
            " the FIRST occurrence of each, extras dropped from the ordering request (a sheet",
            " can only appear once in a workbook): ", paste(unique(dupe_targets), collapse = ", "))
    target_order <- unique(target_order)
  }
  
  target_present <- intersect(target_order, current_names)
  target_missing <- setdiff(target_order, current_names)
  if (length(target_missing) > 0) {
    message("finalize_workbook_sheet_order(): target sheet(s) not in the workbook yet (skipped,",
            " re-run this after the script that creates them has run): ",
            paste(target_missing, collapse = ", "))
  }
  
  extra_sheets <- setdiff(current_names, target_order)
  if (length(extra_sheets) > 0) {
    warning("finalize_workbook_sheet_order(): sheet(s) in the workbook but NOT in target_order -",
            " kept, appended at the end rather than dropped: ", paste(extra_sheets, collapse = ", "))
  }
  
  final_order_names <- c(target_present, extra_sheets)
  new_position_of_current_index <- match(final_order_names, current_names)
  openxlsx::worksheetOrder(wb) <- new_position_of_current_index
  
  openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)
  message("finalize_workbook_sheet_order(): saved '", out_path, "' - final sheet order: ",
          paste(names(wb), collapse = ", "))
  invisible(wb)
}

## Lets a sheet-writer match ITS year-row range to another sheet
## already in the workbook (e.g. Catches_Ecosim matching Ecosim's
## years) WITHOUT requiring that other sheet to have been written
## first - if it isn't there yet, this just returns NULL and the
## caller falls back to deriving its own range, order-independently.
read_existing_ts_years <- function(out_path, sheet_name) {
  if (!file.exists(out_path)) return(NULL)
  if (!(sheet_name %in% openxlsx::getSheetNames(out_path))) return(NULL)
  existing <- openxlsx::read.xlsx(out_path, sheet = sheet_name)
  year_rows <- suppressWarnings(as.numeric(existing[[1]]))
  ts_years <- sort(year_rows[!is.na(year_rows)])
  if (length(ts_years) == 0) return(NULL)
  ts_years
}

## Generalized from the MEDITS pipeline's 3-sheet workbook (FG_spp,
## Ecopath, Ecosim). year_ecopath and ts_years are the function
## attributes specified up top.
##
## Ecosim sheet format matched directly against a real exported
## example in the original MEDITS work - NOT independently re-verified
## here, carried over as-is. See the original pipeline's own note: if
## an actual Ecosim-exported CSV becomes available for cross-checking,
## verify this format against it directly.
##
## Also writes a PB_QB_spp SCAFFOLD (Species/FG_num/Biomass/
## prop_biomass_FG only, PB/QB left NA) so that sheet exists after this
## function alone - i.e. after running just 01_survey_density_westmed.R/
## 01_survey_density_custom.R, without needing 04_pbqb_calc.R (Step 4)
## to have run first. Real PB/QB values still only come from Step 4 -
## this function has no PB/QB computation of its own - but the sheet's
## STRUCTURE (which species, which FG, what share of the FG's biomass)
## no longer requires Step 4 just to appear. If PB_QB_spp already has
## real (non-NA) PB values from a prior Step 4 run, this scaffold is
## skipped so re-running Step 1 never overwrites Step 4's real numbers.
export_ecopath_ecosim_excel <- function(fg_index_regional, species_density_regional,
                                        n_samples_by_area_year, dataframe2, year_ecopath = 1994:1996,
                                        ts_years = NULL, out_path, species_taxonomy = NULL,
                                        extra_sheets = NULL,
                                        fg_cv_log = NULL,        ## data.table(FG_num, cv_log) for the Weight row
                                        normalize_ts = TRUE) {   ## TRUE = rescale to reference index (first value = 1); FALSE = raw density
  if (!requireNamespace("openxlsx", quietly = TRUE)) stop("openxlsx package required for Excel export.")
  
  ## Validate expected columns upfront, by name, so a mismatch (e.g. a
  ## lowercase "year" instead of "Year" in whatever the caller built
  ## n_samples_by_area_year from) fails here with a clear message
  ## naming the actual argument and column at fault - rather than deep
  ## inside a data.table [] expression where the only symptom is a
  ## generic "Object 'Year' not found. Perhaps you intended [year]".
  required_cols <- list(
    fg_index_regional = c("Year", "FG_num", "FG_name", "mean_density"),
    species_density_regional = c("Year", "FG_num", "FG_name", "ScientificName", "mean_density"),
    n_samples_by_area_year = c("AreaID", "Year", "n_samples"),
    dataframe2 = c("FG_num", "FG_name")
  )
  args_to_check <- list(fg_index_regional = fg_index_regional,
                        species_density_regional = species_density_regional,
                        n_samples_by_area_year = n_samples_by_area_year,
                        dataframe2 = dataframe2)
  for (arg_name in names(required_cols)) {
    missing <- setdiff(required_cols[[arg_name]], names(args_to_check[[arg_name]]))
    if (length(missing) > 0) {
      stop("export_ecopath_ecosim_excel(): '", arg_name, "' is missing column(s): ",
           paste(missing, collapse = ", "), ". Actual columns present: ",
           paste(names(args_to_check[[arg_name]]), collapse = ", "),
           ". Check capitalization - this function expects exactly 'Year' (capital Y),",
           " matching the standardized dataframe1 format, not 'year'.")
    }
  }
  
  full_fg_list <- unique(dataframe2[, .(FG_num, FG_name)]); setorder(full_fg_list, FG_num)
  
  ## --- FG_spp sheet -----------------------------------------------------------
  ## IMPORTANT: built from the FULL species_density_regional (every
  ## year), NOT filtered to year_ecopath first. A species genuinely
  ## belongs to its FG regardless of which years it happened to be
  ## sampled in - filtering to year_ecopath before building the row
  ## list was silently dropping any species (or, for the Ecopath sheet
  ## below, entire FGs) with no observations in that specific 3-year
  ## window, even though they're correctly matched and have real data
  ## for other years. Species/FGs not sampled in year_ecopath still get
  ## a row here, just with a blank Density_<base years> value for that
  ## column specifically - "no data for this period" is different
  ## information from "doesn't belong to this FG", and collapsing them
  ## by omitting the row entirely was losing that distinction.
  all_species_fg <- unique(species_density_regional[, .(FG_num, FG_name, Species = ScientificName)])
  density_in_base_years <- species_density_regional[
    Year %in% year_ecopath, .(Density = mean(mean_density, na.rm = TRUE)),
    by = .(FG_num, FG_name, Species = ScientificName)]
  fg_spp_sheet <- merge(all_species_fg, density_in_base_years,
                        by = c("FG_num", "FG_name", "Species"), all.x = TRUE)
  
  ## prop_sp_fg: this species' share of its FG's total density among
  ## species that DO have base-year data (NA Density species can't
  ## contribute a proportion, and don't affect other species' shares)
  fg_spp_sheet[, fg_total_density := sum(Density, na.rm = TRUE), by = FG_num]
  fg_spp_sheet[, prop_sp_fg := ifelse(fg_total_density > 0, Density / fg_total_density, NA_real_)]
  fg_spp_sheet[, fg_total_density := NULL]
  
  if (!is.null(species_taxonomy)) {
    n_before_tax <- nrow(fg_spp_sheet)
    fg_spp_sheet <- merge(fg_spp_sheet, species_taxonomy, by.x = "Species", by.y = "ScientificName", all.x = TRUE)
    n_missing_tax <- fg_spp_sheet[is.na(Genus) & is.na(Family) & is.na(Class), .N]
    if (n_missing_tax > 0) {
      message("NOTE: ", n_missing_tax, " of ", n_before_tax, " species in FG_spp have no taxonomy",
              " match in species_taxonomy - these rows will have blank taxonomy columns:")
      print(fg_spp_sheet[is.na(Genus) & is.na(Family) & is.na(Class), .(Species)])
    }
  } else {
    message("species_taxonomy not provided - FG_spp will have no taxonomy columns",
            " (pass a data.table with ScientificName, Genus, Family, Order, Class, Phylum to include them).")
  }
  setorder(fg_spp_sheet, FG_num, -prop_sp_fg)
  
  ## sanity check - each FG's proportions should sum to ~1 (only checked
  ## where any species had base-year data at all, otherwise 0/0 is
  ## expected and not a problem)
  prop_check <- fg_spp_sheet[, .(total_prop = sum(prop_sp_fg, na.rm = TRUE)), by = FG_num]
  prop_check <- prop_check[total_prop > 0]
  if (any(abs(prop_check$total_prop - 1) > 0.01)) {
    message("WARNING: some FG(s) have prop_sp_fg not summing to ~1 - check for NA Density",
            " values within that FG:")
    print(prop_check[abs(total_prop - 1) > 0.01])
  }
  
  ## --- Ecopath sheet ----------------------------------------------------------
  ## Built from full_fg_list (every FG in dataframe2), not from
  ## fg_index_regional directly - an FG with zero observed biomass in
  ## year_ecopath (e.g. nothing in that FG was caught during the base
  ## years specifically) still needs a row for Ecopath model-building,
  ## just with blank biomass rather than being silently absent from the
  ## whole sheet. NOT rescaled by normalize_ts - it's a single base-year
  ## snapshot, not a time series, so "first value = 1" doesn't apply.
  base_year <- year_ecopath[1]
  ecopath_base <- fg_index_regional[Year == base_year, .(FG_num, Biomass_baseyear = mean_density)]
  ecopath_avg  <- fg_index_regional[Year %in% year_ecopath,
                                    .(Biomass_avg = mean(mean_density, na.rm = TRUE)), by = FG_num]
  ecopath_sheet <- merge(full_fg_list, ecopath_base, by = "FG_num", all.x = TRUE)
  ecopath_sheet <- merge(ecopath_sheet, ecopath_avg, by = "FG_num", all.x = TRUE)
  setnames(ecopath_sheet, c("Biomass_baseyear", "Biomass_avg"),
           c(paste0("Biomass_", base_year), paste0("Biomass_", min(year_ecopath), "_", max(year_ecopath))))
  setorder(ecopath_sheet, FG_num)
  
  n_fg_no_biomass <- ecopath_sheet[is.na(get(paste0("Biomass_", min(year_ecopath), "_", max(year_ecopath)))), .N]
  if (n_fg_no_biomass > 0) {
    message(n_fg_no_biomass, " of ", nrow(ecopath_sheet), " FG(s) have no observed biomass in ",
            min(year_ecopath), "-", max(year_ecopath), " - included with blank biomass, not omitted:")
    print(ecopath_sheet[is.na(get(paste0("Biomass_", min(year_ecopath), "_", max(year_ecopath)))), .(FG_num, FG_name)])
  }
  
  ## --- Ecosim sheet -----------------------------------------------------------
  if (is.null(ts_years)) {
    ts_years <- min(fg_index_regional$Year, na.rm = TRUE):max(fg_index_regional$Year, na.rm = TRUE)
  }
  years_with_effort <- sort(unique(n_samples_by_area_year[Year %in% ts_years, Year]))
  
  ## Rescales each FG's time series to a REFERENCE INDEX when
  ## normalize_ts=TRUE: the first non-NA value in the series becomes 1,
  ## every other value expressed relative to it - matches the "Scaling:
  ## relative" metadata row literally, rather than leaving raw absolute
  ## density under a "relative" label. If ts_years' actual first
  ## calendar year has no survey effort (NA), the reference is taken
  ## from the first year that DOES have a value instead - flagged
  ## explicitly, since it means "first row = 1" in the output isn't
  ## literally ts_years[1] in that case. When normalize_ts=FALSE, the
  ## raw density (whatever unit fg_index_regional is in) is returned
  ## unchanged.
  build_ts_column <- function(fg_num) {
    vals <- fg_index_regional[FG_num == fg_num, .(Year, mean_density)]
    full_years <- data.table(Year = ts_years)
    vals <- merge(full_years, vals, by = "Year", all.x = TRUE)
    vals[Year %in% years_with_effort & is.na(mean_density), mean_density := 0]
    vals <- vals[order(Year)]
    
    if (!normalize_ts) return(vals$mean_density)
    
    first_valid_idx <- which(!is.na(vals$mean_density))[1]
    if (is.na(first_valid_idx)) {
      message("FG ", fg_num, ": no non-NA values across the whole time series - column left blank.")
      return(vals$mean_density)
    }
    ref_value <- vals$mean_density[first_valid_idx]
    ref_year  <- vals$Year[first_valid_idx]
    if (ref_year != ts_years[1]) {
      message("FG ", fg_num, ": relative-scaling reference is year ", ref_year,
              " (first year WITH data), not the series' first year ", ts_years[1],
              " (no survey effort that year).")
    }
    if (is.na(ref_value) || ref_value == 0) {
      message("WARNING: FG ", fg_num, "'s reference value (year ", ref_year, ") is ",
              ifelse(is.na(ref_value), "NA", "zero"), " - cannot rescale to a relative index,",
              " leaving this column as raw density instead.")
      return(vals$mean_density)
    }
    vals$mean_density / ref_value
  }
  
  ## Weight row: CV.log per FG if fg_cv_log was provided (see
  ## compute_cv_log_by_fg()), else "1" (the old placeholder) with an
  ## explicit per-FG warning so a missing weight is visible rather than
  ## silently defaulting.
  get_weight <- function(fg_num) {
    if (is.null(fg_cv_log)) return("1")
    w <- fg_cv_log[FG_num == fg_num, cv_log]
    if (length(w) == 0 || is.na(w)) {
      message("FG ", fg_num, ": no CV.log available - Weight defaults to 1",
              " (check whether this FG had enough replicate observations).")
      return("1")
    }
    as.character(round(w, 4))
  }
  
  meta_labels <- c("Name", "Type", "Usage", "Scaling", "Weight", "Target", "2nd target", "Interval")
  ecosim_sheet <- data.table(` ` = c(meta_labels, as.character(ts_years)))
  for (i in seq_len(nrow(full_fg_list))) {
    fg_num <- full_fg_list$FG_num[i]; fg_name <- full_fg_list$FG_name[i]
    ts_name <- paste0("B_", gsub("[^A-Za-z0-9]+", "", fg_name))
    col <- c(ts_name, "Biomass (relative)", "reference", "relative", get_weight(fg_num),
             paste0(fg_num, ": ", fg_name), "", "Annual", as.character(build_ts_column(fg_num)))
    ecosim_sheet[, (paste0("fg_", fg_num)) := col]
  }
  
  ## --- FG_spp_Ecosim sheet -----------------------------------------------------
  ## Species-level mean density across the FULL ts_years range - one row
  ## per species, no Year column, since this is the species' overall
  ## average over the whole time series, not a per-year breakdown.
  species_mean_ts <- species_density_regional[Year %in% ts_years,
                                              .(Density = mean(mean_density, na.rm = TRUE)),
                                              by = .(FG_num, FG_name, Species = ScientificName)]
  fg_spp_ecosim_sheet <- merge(all_species_fg, species_mean_ts,
                               by = c("FG_num", "FG_name", "Species"), all.x = TRUE)
  setorder(fg_spp_ecosim_sheet, FG_num, Species)
  
  sheets_to_write <- list(FG_spp_Ecopath = fg_spp_sheet, Ecopath = ecopath_sheet,
                          Ecosim = ecosim_sheet, FG_spp_Ecosim = fg_spp_ecosim_sheet)
  
  ## --- PB_QB_spp scaffold (species x FG list, no PB/QB yet) --------------
  ## Written here so the PB_QB_spp sheet EXISTS after Step 1 alone, not
  ## only once 04_pbqb_calc.R (Step 4) has run - Step 1 has no PB/QB
  ## computation of its own (that's entirely Step 4's job: life-history-
  ## based estimates from FishBase/SeaLifeBase - growth, mortality,
  ## trophic level - not derivable from survey density), so this
  ## scaffold only carries what Step 1 DOES already know: which species
  ## make up each FG and their biomass share (the same Species/FG_num/
  ## Biomass/proportion as fg_spp_sheet above, renamed to PB_QB_spp's own
  ## column convention: Density -> Biomass, prop_sp_fg -> prop_biomass_FG).
  ## PB/QB and their own proportion columns are left blank (NA) until
  ## Step 4 fills them in for real.
  ##
  ## Guarded against clobbering: if PB_QB_spp already exists in out_path
  ## WITH real (non-NA) PB values - i.e. 04_pbqb_calc.R has already run
  ## against this exact workbook - this scaffold is skipped entirely, so
  ## re-running Step 1 (e.g. to refresh biomass after new survey data)
  ## never wipes Step 4's real numbers back to blank. Step 4 itself
  ## always fully replaces whatever PB_QB_spp it finds (scaffold or not)
  ## via add_pbqb_to_ecopath_workbook()/upsert_workbook_sheets() - no
  ## special-casing needed on that side.
  pbqb_spp_has_real_data <- FALSE
  if (file.exists(out_path) && "PB_QB_spp" %in% openxlsx::getSheetNames(out_path)) {
    existing_pbqb_spp <- tryCatch(openxlsx::read.xlsx(out_path, sheet = "PB_QB_spp"),
                                  error = function(e) NULL)
    if (!is.null(existing_pbqb_spp) && "PB" %in% names(existing_pbqb_spp) &&
        any(!is.na(existing_pbqb_spp$PB))) {
      pbqb_spp_has_real_data <- TRUE
    }
  }
  
  if (!pbqb_spp_has_real_data) {
    pbqb_spp_scaffold <- fg_spp_sheet[, .(FG_num, FG_name, Species,
                                          Biomass = Density, prop_biomass_FG = prop_sp_fg)]
    pbqb_spp_scaffold[, `:=`(PB = NA_real_, prop_biomass_PB = NA_real_,
                             QB = NA_real_, prop_biomass_QB = NA_real_,
                             Note = "PB/QB not yet computed - run 04_pbqb_calc.R (Step 4) to fill in")]
    setorder(pbqb_spp_scaffold, FG_num, -Biomass)
    sheets_to_write$PB_QB_spp <- pbqb_spp_scaffold
    message("PB_QB_spp written as a SCAFFOLD (Species/FG_num/Biomass/prop_biomass_FG only -",
            " PB/QB left blank) since Step 4 (04_pbqb_calc.R) hasn't run against this workbook",
            " yet. Run 04_pbqb_calc.R afterward to fill in real PB/QB values - it replaces this",
            " scaffold with the full sheet automatically, no extra step needed here.")
  } else {
    message("PB_QB_spp already has real PB/QB data (04_pbqb_calc.R has already run against",
            " this workbook) - leaving it untouched rather than overwriting it with a blank",
            " scaffold.")
  }
  
  ## extra_sheets: named list of additional data.tables/data.frames to
  ## include in the SAME workbook/write call - appended here rather
  ## than requiring a separate loadWorkbook()/saveWorkbook() round-trip
  ## after this function already wrote the file.
  if (!is.null(extra_sheets)) {
    dup_names <- intersect(names(extra_sheets), names(sheets_to_write))
    if (length(dup_names) > 0) {
      stop("export_ecopath_ecosim_excel(): extra_sheets name(s) collide with",
           " built-in sheet names: ", paste(dup_names, collapse = ", "))
    }
    sheets_to_write <- c(sheets_to_write, extra_sheets)
  }
  
  upsert_workbook_sheets(sheets_to_write, out_path)
  invisible(sheets_to_write)
}

## =================================================================
## add_catches_to_ecopath_workbook()
##
## Formats an FG-level catch time series (Year x FG_num x FG_name x
## Catch_t - e.g. from a catch/landings pipeline like 02_fao_catches.R)
## as two more sheets in the SAME shape as export_ecopath_ecosim_excel()
## above, and adds them to the SAME workbook - Catches_Ecopath next to
## Ecopath, Catches_Ecosim next to Ecosim - rather than as a separate
## standalone catches file.
##
## If out_path already exists (e.g. export_ecopath_ecosim_excel() has
## already been run against it), this loads it, matches Catches_Ecosim's
## year rows to whatever Ecosim's already are, and adds/replaces just
## these two sheets - the rest of the workbook is untouched. If it
## doesn't exist yet, creates a new workbook with ONLY these two sheets,
## flagged explicitly since the Biomass-side sheets are still missing.
##
## Catches_Ecopath mirrors the Ecopath sheet exactly: one row per FG
## (every FG in fg_lookup, not just ones with matched catch), a
## base-year snapshot column, and a year_ecopath-range average column -
## named Catch_<year>/Catch_<start>_<end> instead of Biomass_<...>.
##
## Catches_Ecosim mirrors the Ecosim sheet's meta-row + year-row shape,
## but is NOT rescaled to a first-year=1 reference the way
## build_ts_column() rescales biomass above - EwE drives Ecosim with
## the catch series in ABSOLUTE units (t/km^2/year), so Type/Scaling
## are "Catches"/"absolute" rather than "Biomass (relative)"/"relative".
## Missing Year x FG combinations stay NA (unknown), never filled with
## 0 - catch data being absent for a species/FG/year isn't the same
## claim as a confirmed zero catch that year.
##
##   fg_catch      - data.table: Year, FG_num, FG_name, Catch_t
##   fg_lookup     - data.table: FG_num, FG_name for EVERY FG in the
##                   scheme (same reference used elsewhere in the
##                   pipeline) - needed so zero-catch FGs still get a
##                   blank row instead of being silently absent
##   out_path      - path to output/ecopath_ecosim_inputs.xlsx (or
##                   wherever export_ecopath_ecosim_excel() wrote to)
##   year_ecopath  - MUST match the year_ecopath used to build the
##                   Ecopath sheet in the same workbook, or Catches_
##                   Ecopath's snapshot describes a different period
##                   than Biomass's
##   ts_years      - optional; if NULL and an Ecosim sheet already
##                   exists in the workbook, its year rows are reused
##                   so both Ecosim sheets line up. Otherwise derived
##                   from fg_catch's own Year range.
##
## SINGLE FLEET BY DEFAULT, MULTI-FLEET VIA fleet_structure (optional):
## Catches_Ecopath/Catches_Ecosim have historically had no fleet
## dimension at all - one combined Catch_<year> column per FG, and
## Catches_Ecosim's meta-rows (Name, Type, Usage, Scaling, Weight,
## Target, 2nd target, Interval) with no Fleet field. EwE's Basic Input
## Catches table is properly indexed by FG x Fleet, and Ecosim policy
## scenarios need fleet-specific catch/effort series, so this was a
## known simplification. fleet_structure (below) lets a caller supply
## a real fleet split; when omitted, behavior AND output are unchanged
## from before this argument existed - single implicit fleet, same
## column names, no Fleet_Structure sheet.
## =================================================================
## area_km2: total study area (km^2) to convert fg_catch's Catch_t
## (RAW TOTAL TONNES landed across the whole region - see
## 02_fao_catches.R, MEASURE == "Q_tlw") into a density, t/km^2/year -
## the same units Biomass_<year> in the Ecopath sheet is already in.
## Without this conversion, Catches_Ecopath/Catches_Ecosim would be off
## by a factor of the whole study area (tens of thousands of km^2) versus
## Biomass, which breaks Ecopath's mass balance (Ecotrophic Efficiency
## is computed from Biomass and Catches together, in the same units).
##
## fleet_structure - OPTIONAL. A data.table/data.frame with columns
##   FG_num, Fleet, prop_catch: what share (0-1) of FG_num's total
##   catch goes to Fleet, e.g.
##     FG_num  Fleet          prop_catch
##     3       Trawl          0.7
##     3       Small-scale    0.3
##     5       Trawl          1.0
##   prop_catch should sum to ~1 per FG_num (checked, with a warning -
##   not an error - if it doesn't, since a deliberately partial split,
##   e.g. modeling only the fleets that matter and leaving the rest
##   unallocated, is a legitimate use case too). Any FG_num present in
##   fg_lookup but ABSENT from fleet_structure is assigned a single
##   implicit "Fleet_1" fleet with prop_catch = 1 - i.e. every FG
##   without an explicit split still gets its full catch, just under
##   one default fleet, so partial fleet_structure tables (only the
##   FGs you've actually got fleet data for) are fine.
##
##   Pass NULL (the default) to skip fleet splitting entirely: every FG
##   is treated as one implicit fleet, and Catches_Ecopath/Catches_Ecosim
##   keep their original (pre-fleet) shape exactly - no Fleet-suffixed
##   columns, no Fleet_Structure sheet. This is what running this
##   function has always done, so existing callers/workbooks are
##   unaffected by fleet_structure existing as an argument.
##
##   Once ANY FG in fleet_structure resolves to more than one fleet,
##   ALL FG rows switch to the multi-fleet shape (fleet-suffixed
##   columns for Catches_Ecopath, one column-set per FG x Fleet for
##   Catches_Ecosim) for consistency across the sheet - an FG with no
##   real split still gets its one Fleet_1 column, just fleet-suffixed
##   like everything else. The fleet_structure table actually used
##   (after filling in Fleet_1 defaults) is also written out as its
##   own Fleet_Structure sheet, so the split is traceable from the
##   workbook alone.
add_catches_to_ecopath_workbook <- function(fg_catch, fg_lookup, out_path, year_ecopath, area_km2,
                                            ts_years = NULL, fleet_structure = NULL) {
  
  if (missing(area_km2) || is.null(area_km2) || is.na(area_km2) || area_km2 <= 0) {
    stop("add_catches_to_ecopath_workbook(): area_km2 must be a positive number - fg_catch's",
         " Catch_t is a RAW TOTAL (tonnes landed across the whole region), not a density,",
         " and needs to be divided by the study area to match Biomass's t/km^2 units in",
         " the Ecopath sheet. Pass the same total area used elsewhere in the pipeline",
         " (e.g. sum(area_km2) from strata_area_by_area.csv).")
  }
  
  ## convert once, up front - everything below (Catches_Ecopath AND
  ## Catches_Ecosim) reads Catch_t_km2, never the raw Catch_t
  fg_catch <- copy(fg_catch)
  fg_catch[, Catch_t_km2 := Catch_t / area_km2]
  message("Converted fg_catch's raw total tonnes to a density using area_km2 = ",
          round(area_km2, 1), " km^2 (t/km^2/year, matching Biomass's units in the",
          " Ecopath sheet) - e.g. total catch of ", round(fg_catch[1, Catch_t], 1),
          " t for FG ", fg_catch[1, FG_num], " in ", fg_catch[1, Year], " becomes ",
          signif(fg_catch[1, Catch_t_km2], 4), " t/km^2.")
  
  full_fg_list <- unique(fg_lookup[, .(FG_num, FG_name)])[order(FG_num)]
  
  if (is.null(ts_years)) {
    ts_years <- read_existing_ts_years(out_path, "Ecosim")
    if (!is.null(ts_years)) {
      message("ts_years taken from the existing Ecosim sheet: ",
              min(ts_years), "-", max(ts_years), " (", length(ts_years), " years) -",
              " keeps Catches_Ecosim's year rows lined up with it, order-independently",
              " of whether Ecosim was written before or after this.")
    } else {
      ts_years <- min(fg_catch$Year, na.rm = TRUE):max(fg_catch$Year, na.rm = TRUE)
      message("No existing Ecosim sheet found to match years against yet - ts_years",
              " derived from fg_catch's own range instead: ", min(ts_years), "-",
              max(ts_years), ". If Ecosim gets added to this workbook LATER with a",
              " different range, re-run this function afterward to pick it up.")
    }
  }
  
  ## --- Resolve fleet_structure (or the single-fleet default) -------------
  if (is.null(fleet_structure)) {
    fleet_tbl <- data.table(FG_num = full_fg_list$FG_num, Fleet = "Fleet_1", prop_catch = 1)
    multi_fleet <- FALSE
  } else {
    fleet_tbl <- copy(as.data.table(fleet_structure))
    required_fleet_cols <- c("FG_num", "Fleet", "prop_catch")
    missing_fleet_cols <- setdiff(required_fleet_cols, names(fleet_tbl))
    if (length(missing_fleet_cols) > 0) {
      stop("add_catches_to_ecopath_workbook(): fleet_structure is missing required column(s): ",
           paste(missing_fleet_cols, collapse = ", "), " - expected FG_num, Fleet, prop_catch",
           " (see this function's header comment for the expected shape).")
    }
    fleet_tbl <- fleet_tbl[, ..required_fleet_cols]
    
    prop_check <- fleet_tbl[, .(total_prop = sum(prop_catch, na.rm = TRUE)), by = FG_num]
    off_fgs <- prop_check[abs(total_prop - 1) > 1e-6]
    if (nrow(off_fgs) > 0) {
      message("fleet_structure: ", nrow(off_fgs), " FG(s) have prop_catch NOT summing to 1",
              " (fine if that's deliberate - e.g. only some fleets modeled for that FG -",
              " but check it's not a typo):")
      print(off_fgs)
    }
    
    missing_fg <- setdiff(full_fg_list$FG_num, fleet_tbl$FG_num)
    if (length(missing_fg) > 0) {
      message(length(missing_fg), " of ", nrow(full_fg_list), " FG(s) not present in",
              " fleet_structure - defaulted to a single implicit Fleet_1 (prop_catch = 1)",
              " each, same as if fleet_structure had been NULL for just those FGs.")
      fleet_tbl <- rbind(fleet_tbl,
                         data.table(FG_num = missing_fg, Fleet = "Fleet_1", prop_catch = 1))
    }
    multi_fleet <- uniqueN(fleet_tbl$Fleet) > 1
    if (!multi_fleet) {
      message("fleet_structure resolved to a single fleet ('", fleet_tbl$Fleet[1], "') across",
              " every FG - Catches_Ecopath/Catches_Ecosim keep their original (non-fleet-",
              "suffixed) shape. Pass more than one distinct Fleet value to switch to the",
              " multi-fleet sheet shape.")
    }
  }
  setorder(fleet_tbl, FG_num, Fleet)
  fleet_names <- sort(unique(fleet_tbl$Fleet))
  
  ## --- Catches_Ecopath ---------------------------------------------------
  base_year <- year_ecopath[1]
  catch_base <- fg_catch[Year == base_year, .(FG_num, Catch_baseyear = Catch_t_km2)]
  catch_avg  <- fg_catch[Year %in% year_ecopath,
                         .(Catch_avg = mean(Catch_t_km2, na.rm = TRUE)), by = FG_num]
  catches_ecopath <- merge(full_fg_list, catch_base, by = "FG_num", all.x = TRUE)
  catches_ecopath <- merge(catches_ecopath, catch_avg, by = "FG_num", all.x = TRUE)
  range_col <- paste0("Catch_", min(year_ecopath), "_", max(year_ecopath))
  base_col <- paste0("Catch_", base_year)
  setnames(catches_ecopath, c("Catch_baseyear", "Catch_avg"), c(base_col, range_col))
  setorder(catches_ecopath, FG_num)
  
  n_fg_no_catch <- catches_ecopath[is.na(get(range_col)), .N]
  if (n_fg_no_catch > 0) {
    message(n_fg_no_catch, " of ", nrow(catches_ecopath), " FG(s) have no catch data in ",
            min(year_ecopath), "-", max(year_ecopath), " - included with a blank catch value,",
            " NOT assumed zero (genuinely unfished and simply unmatched/unresolved look",
            " identical here):")
    print(catches_ecopath[is.na(get(range_col)), .(FG_num, FG_name)])
  }
  
  if (multi_fleet) {
    ## Split the two total-catch columns above into one pair per Fleet,
    ## FG x Fleet's share = FG's total x that Fleet's prop_catch. FGs
    ## with no data for a given Fleet's share still get a (blank x
    ## prop =) blank cell, not a fabricated zero.
    fleet_split <- catches_ecopath[, .(FG_num, FG_name, base_col_val = get(base_col), range_col_val = get(range_col))]
    fleet_split <- merge(fleet_split, fleet_tbl, by = "FG_num", all.x = TRUE, allow.cartesian = TRUE)
    fleet_split[, (base_col)  := base_col_val * prop_catch]
    fleet_split[, (range_col) := range_col_val * prop_catch]
    
    catches_ecopath <- full_fg_list
    for (fl in fleet_names) {
      fl_cols <- fleet_split[Fleet == fl, .(FG_num, v_base = get(base_col), v_range = get(range_col))]
      setnames(fl_cols, c("v_base", "v_range"), c(paste0(base_col, "_", fl), paste0(range_col, "_", fl)))
      catches_ecopath <- merge(catches_ecopath, fl_cols, by = "FG_num", all.x = TRUE)
    }
    setorder(catches_ecopath, FG_num)
  }
  
  ## --- Catches_Ecosim ------------------------------------------------------
  meta_labels <- c("Name", "Type", "Usage", "Scaling", "Weight", "Target", "2nd target", "Interval")
  catches_ecosim <- data.table(` ` = c(meta_labels, as.character(ts_years)))
  
  build_catch_ts_column <- function(fg_num, prop = 1) {
    vals <- fg_catch[FG_num == fg_num, .(Year, Catch_t_km2)]
    full_years <- data.table(Year = ts_years)
    vals <- merge(full_years, vals, by = "Year", all.x = TRUE)
    vals[order(Year)]$Catch_t_km2 * prop
  }
  
  if (!multi_fleet) {
    for (i in seq_len(nrow(full_fg_list))) {
      fg_num <- full_fg_list$FG_num[i]; fg_name <- full_fg_list$FG_name[i]
      ts_name <- paste0("C_", gsub("[^A-Za-z0-9]+", "", fg_name))
      col <- c(ts_name, "Catches", "reference", "absolute", "1",
               paste0(fg_num, ": ", fg_name), "", "Annual",
               as.character(build_catch_ts_column(fg_num)))
      catches_ecosim[, (paste0("fg_", fg_num)) := col]
    }
  } else {
    for (i in seq_len(nrow(fleet_tbl))) {
      fg_num <- fleet_tbl$FG_num[i]; fl <- fleet_tbl$Fleet[i]; prop <- fleet_tbl$prop_catch[i]
      fg_name <- full_fg_list[FG_num == fg_num, FG_name]
      if (length(fg_name) == 0) next  # fleet_structure referenced an FG_num not in fg_lookup
      ts_name <- paste0("C_", gsub("[^A-Za-z0-9]+", "", fg_name), "_", gsub("[^A-Za-z0-9]+", "", fl))
      col <- c(ts_name, "Catches", "reference", "absolute", "1",
               paste0(fg_num, ": ", fg_name, " (", fl, ")"), "", "Annual",
               as.character(build_catch_ts_column(fg_num, prop)))
      catches_ecosim[, (paste0("fg_", fg_num, "_", fl)) := col]
    }
  }
  
  sheets_to_write <- list(Catches_Ecopath = catches_ecopath, Catches_Ecosim = catches_ecosim)
  if (multi_fleet) {
    sheets_to_write$Fleet_Structure <- fleet_tbl
    message("Fleet_Structure sheet written - ", uniqueN(fleet_tbl$Fleet), " fleet(s) (",
            paste(fleet_names, collapse = ", "), ") across ", uniqueN(fleet_tbl$FG_num), " FG(s).")
  }
  upsert_workbook_sheets(sheets_to_write, out_path)
  
  invisible(sheets_to_write)
}

## =================================================================
## add_pbqb_to_ecopath_workbook()
##
## Adds 04_pbqb_calc.R's FG-level PB/QB estimates as one more sheet -
## "PB_QB" - in the SAME shared workbook as the Biomass sheets
## (01_survey_density_westmed.R) and Catches sheets (02_fao_catches.R),
## via the same order-independent upsert_workbook_sheets() both of
## those use. This can be run before, after, or between those two -
## no dependency on either having run first.
##
##   fg_weighted    - 04_pbqb_calc.R's FG-level table. Requires FG, FG_name,
##                     PB_FG, QB_FG at minimum. A handful of other columns
##                     (Biomass_FG, F_FG, PB_source, QB_source, etc.) are
##                     carried through as extra audit-trail columns IF
##                     present, but aren't required - different 04_pbqb_calc.R
##                     runs may or may not have gone through the EcoBase-fill
##                     or FG-level-F steps.
##   out_path       - path to output/ecopath_ecosim_inputs.xlsx
##   species_pb_qb  - OPTIONAL. 04_pbqb_calc.R's own species-level `results`
##                     table (the one written to
##                     species_pb_qb_by_taxon_group.csv) - every species that
##                     went into computing PB_FG/QB_FG above, not just the
##                     FG-level rollup. Requires FG, Species, Biomass, PB, QB
##                     at minimum; FG_name/dispatch_group/PB_method/QB_method/
##                     Fmort/Fmort_source are carried through if present.
##                     When supplied, this also writes a "PB_QB_spp" sheet -
##                     one row per species, mirroring FG_spp_Ecopath's shape,
##                     WITH a proportion column so it's actually possible to
##                     see which species (and how much of each FG's biomass)
##                     PB_FG/QB_FG were computed from - not just the FG-level
##                     final number. Omit this argument (or pass NULL) to
##                     write PB_QB only, same as before this argument existed.
## =================================================================
add_pbqb_to_ecopath_workbook <- function(fg_weighted, out_path, species_pb_qb = NULL) {
  required_cols <- c("FG", "FG_name", "PB_FG", "QB_FG")
  missing_cols <- setdiff(required_cols, names(fg_weighted))
  if (length(missing_cols) > 0) {
    stop("add_pbqb_to_ecopath_workbook(): fg_weighted is missing required column(s): ",
         paste(missing_cols, collapse = ", "), " - check it's the table produced",
         " further up in 04_pbqb_calc.R (after the FG-level aggregation step), not",
         " something else.")
  }
  
  optional_cols <- intersect(
    c("Biomass_FG", "n_species_with_PB", "n_species_with_QB", "n_species_total",
      "biomass_coverage_PB", "biomass_coverage_QB", "n_species_with_F",
      "PB_FG_before_F", "F_FG", "PB_source", "QB_source"),
    names(fg_weighted)
  )
  pbqb_sheet <- fg_weighted[, c("FG", "FG_name", "PB_FG", "QB_FG", optional_cols), with = FALSE]
  setnames(pbqb_sheet, "FG", "FG_num")
  setorder(pbqb_sheet, FG_num)
  
  sheets <- list(PB_QB = pbqb_sheet)
  
  if (is.null(species_pb_qb)) {
    message("add_pbqb_to_ecopath_workbook(): no species_pb_qb table passed - PB_QB stays",
            " FG-level only (one row per FG), same as before this argument existed. Pass",
            " 04_pbqb_calc.R's own species-level `results` table as species_pb_qb to ALSO",
            " write a PB_QB_spp sheet - one row per species that went into each FG's",
            " PB_FG/QB_FG, with its weighting proportion - rather than only the FG-level",
            " rollup.")
  } else {
    required_spp_cols <- c("FG", "Species", "Biomass", "PB", "QB")
    missing_spp_cols <- setdiff(required_spp_cols, names(species_pb_qb))
    if (length(missing_spp_cols) > 0) {
      stop("add_pbqb_to_ecopath_workbook(): species_pb_qb is missing required column(s): ",
           paste(missing_spp_cols, collapse = ", "), " - check it's 04_pbqb_calc.R's own",
           " `results` table (species-level, after PB/QB dispatch), not fg_weighted or",
           " something else.")
    }
    
    spp <- copy(as.data.table(species_pb_qb))
    
    ## Three different "share of the FG" proportions, because PB_FG and
    ## QB_FG are each their OWN biomass-weighted mean over a DIFFERENT
    ## subset of species (whichever ones have a non-NA PB, resp. QB) -
    ## a single generic proportion column would silently misrepresent
    ## one or the other. All three are computed the same way fg_weighted's
    ## own PB_FG/QB_FG were: weight = Biomass / sum(Biomass) within the
    ## relevant group, so prop_biomass_PB summed within an FG reproduces
    ## exactly the weights PB_FG = sum(Biomass*PB)/sum(Biomass[!is.na(PB)])
    ## used, species by species.
    ##
    ##   prop_biomass_FG - this species' share of the FG's TOTAL biomass
    ##                      (every species in the FG, whether or not PB/QB
    ##                      succeeded) - general reference, same convention
    ##                      as FG_spp_Ecopath's own prop_sp_fg column.
    ##   prop_biomass_PB - this species' actual weight inside PB_FG's own
    ##                      biomass-weighted average - NA for a species
    ##                      whose own PB is NA (it contributed nothing to
    ##                      PB_FG, regardless of its Biomass).
    ##   prop_biomass_QB - same idea, for QB_FG.
    spp[, Biomass_FG_total    := sum(Biomass, na.rm = TRUE), by = FG]
    spp[, prop_biomass_FG     := ifelse(Biomass_FG_total > 0, Biomass / Biomass_FG_total, NA_real_)]
    spp[, Biomass_FG_PB_total := sum(Biomass[!is.na(PB)], na.rm = TRUE), by = FG]
    spp[, prop_biomass_PB     := ifelse(!is.na(PB) & Biomass_FG_PB_total > 0,
                                        Biomass / Biomass_FG_PB_total, NA_real_)]
    spp[, Biomass_FG_QB_total := sum(Biomass[!is.na(QB)], na.rm = TRUE), by = FG]
    spp[, prop_biomass_QB     := ifelse(!is.na(QB) & Biomass_FG_QB_total > 0,
                                        Biomass / Biomass_FG_QB_total, NA_real_)]
    
    spp_optional_cols <- intersect(
      c("FG_name", "dispatch_group", "PB_method", "QB_method", "Fmort", "Fmort_source"),
      names(spp)
    )
    spp_sheet <- spp[, c("FG", "Species", spp_optional_cols, "Biomass", "prop_biomass_FG",
                         "PB", "prop_biomass_PB", "QB", "prop_biomass_QB"), with = FALSE]
    setnames(spp_sheet, "FG", "FG_num")
    setorder(spp_sheet, FG_num, -Biomass)
    
    n_species_no_pb_qb <- spp_sheet[is.na(PB) & is.na(QB), .N]
    message("add_pbqb_to_ecopath_workbook(): PB_QB_spp sheet built - ", nrow(spp_sheet),
            " species across ", uniqueN(spp_sheet$FG_num), " FG(s). ", n_species_no_pb_qb,
            " species have neither a PB nor a QB value (still listed, for biomass-coverage",
            " context, but prop_biomass_PB/prop_biomass_QB are both NA for these - they did",
            " not contribute to PB_FG/QB_FG).")
    
    sheets$PB_QB_spp <- spp_sheet
  }
  
  upsert_workbook_sheets(sheets, out_path)
  invisible(sheets)
}