## =================================================================
## Created by: Daniel Vilas
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
## resolve_pcloud_file() - added 2026-09-16, pCloud data
## keeps getting reorganized into "complementary" subfolders (a file
## that used to sit directly under pcloud_dir/data/ moves one or two
## levels deeper), which breaks every hardcoded `paste0(pcloud_dir,
## "/data/<filename>")` path in the numbered scripts with a bare
## "cannot open file" error that doesn't say WHERE to look next.
##
## This wraps any such hardcoded path: if it exists as given, use it
## unchanged (zero behavior change for anyone whose data hasn't moved).
## If not, recursively search under `search_root` (default:
## file.path(pcloud_dir, "data")) for a file with that EXACT basename.
## - Exactly one match -> use it, with a loud message saying where it
##   was actually found, so the hardcoded path in the script can be
##   updated to match next time.
## - More than one match -> stop() naming all candidates - silently
##   picking one of several same-named files (e.g. a "_v2" copy some-
##   where) is the wrong kind of "helpful" here.
## - No match at all -> if `required = TRUE` (default), stop() with a
##   clear "place this file somewhere under <search_root>" message; if
##   `required = FALSE`, returns the original expected_path unchanged
##   so a caller's own `if (file.exists(...))`-guarded optional-input
##   logic (e.g. 01_biomass.R's catchability correction) still sees a
##   normal "not found" and skips gracefully, exactly as before.
## =================================================================
resolve_pcloud_file <- function(expected_path, pcloud_dir, search_root = file.path(pcloud_dir, "data"), required = TRUE) {
  if (file.exists(expected_path)) return(expected_path)
  
  fname <- basename(expected_path)
  message("[resolve_pcloud_file] '", expected_path, "' not found - searching under '", search_root,
          "' for a file named '", fname, "' (in case it moved into a subfolder)...")
  if (!dir.exists(search_root)) {
    if (required) stop("[resolve_pcloud_file] '", expected_path, "' doesn't exist, and search_root '", search_root, "' doesn't exist either - check pcloud_dir is set correctly.")
    return(expected_path)
  }
  
  ## Exact basename comparison, not a regex `pattern=` match - several of
  ## these real filenames contain regex-special characters themselves
  ## (e.g. "TM_list_(April_2019).xlsx"), so matching on the literal
  ## basename after a plain recursive listing sidesteps escaping entirely.
  all_files <- list.files(search_root, recursive = TRUE, full.names = TRUE, all.files = FALSE)
  hits <- all_files[basename(all_files) == fname]
  
  if (length(hits) == 1) {
    message("  Found it at: '", hits[1], "' - using this path. Consider updating the hardcoded path near the",
            " top of the script to match, so this search doesn't need to run again next time.")
    return(hits[1])
  } else if (length(hits) > 1) {
    stop("[resolve_pcloud_file] '", fname, "' is not at its expected path ('", expected_path, "'), and more than",
         " one file with that exact name was found under '", search_root, "': ", paste(hits, collapse = "; "),
         " - set the exact path explicitly (which one is current) instead of relying on auto-search.")
  } else {
    if (required) {
      stop("[resolve_pcloud_file] No file named '", fname, "' found anywhere under '", search_root, "'",
           " (searched recursively) - either place it there, in any subfolder, or fix the hardcoded path",
           " near the top of the script if it's meant to live somewhere else entirely.")
    }
    message("  Not found anywhere under '", search_root, "' either - treating as genuinely absent.")
    return(expected_path)
  }
}

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
## (FishBase/SeaLifeBase, or WoRMS)
## =================================================================
## Generalized from Section 4c of the MEDITS pipeline: for species with
## no direct FG match, try genus then family fallback, under the
## assumption that congeners/confamilials are ecologically similar
## enough to share an FG. Same ambiguity-safe rule preserved exactly:
## a genus/family is only used for fallback if it maps to EXACTLY ONE
## FG among the reference species - anything spanning multiple FGs is
## too ecologically diverse to assign safely and is left unresolved.
##
## taxonomy_source: "fishbase" (DEFAULT, since 2026-09 - requires
## rfishbase; uses load_taxa() over BOTH the fishbase server (fish) and
## the sealifebase server (everything else - invertebrates, algae,
## etc.), covering the full Genus/Family/Order/Class rank hierarchy,
## plus Phylum for sealifebase taxa - matches the taxonomy source
## 04_pbqb_calc.R already standardized on, so a species' family/order
## etc. is never compared across two different authorities between
## that script and this one), "worms" (requires worms_taxonomy_lookup()
## already sourced - the source this project used before 2026-09; kept
## available, not removed, in case FishBase/SeaLifeBase genuinely lacks
## a taxon WoRMS has), or "both" (tries fishbase first, then worms for
## whatever fishbase left unresolved - the two sources don't depend on
## each other, so this is a genuine combined fallback, not just
## redundancy).

fetch_taxonomy_worms <- function(species_names, cache_path = NULL) {
  if (!exists("worms_taxonomy_lookup")) {
    stop("worms_taxonomy_lookup() not found - source() the same",
         " lib_worms_taxonomy_lookup.R used elsewhere in this project first.")
  }
  raw <- worms_taxonomy_lookup(species_names, cache_path = cache_path)
  as.data.table(raw)[, .(ScientificName = original_name, Genus = genus, Family = family,
                         Order = order, Class = class, Phylum = phylum)]
}

## =================================================================
## fetch_taxonomy_metaweb(): a THIRD taxonomy source, added 2026-09-17
## for 04_diets.R - not an API call at all. The metaweb
## workbook's own "Taxonomic_codes" tab already carries a full WoRMS-
## derived classification (Kingdom -> ... -> Species, 3350 rows in the
## template) for every predator/prey taxon the metaweb itself uses, so
## for any name that came FROM that same tab (every "needed_species" in
## 04_diets.R did, via resolve_codes()) this is a free, instant, no-
## network lookup - not a fuzzy/fallback match, an exact one, since
## it's the very same table the name was resolved from in the first
## place. Also useful for a reference-file species (e.g. from
## FG_WMed.xlsx) that HAPPENS to also appear in this tab, on a best-
## effort case-insensitive basis - those aren't guaranteed to match.
##
## Same 6-column output contract as fetch_taxonomy_worms()/
## fetch_taxonomy_fishbase() (ScientificName/Genus/Family/Order/Class/
## Phylum) so it can be dropped into the same rank_levels-driven
## exclusive/majority-vote fallback logic used elsewhere in this
## project, just sourced locally instead of from an external API.
##
## tax_codes: the data.table read from Taxonomic_codes (must have
## valid_name, Old_name, Genus, Family, Order, Class, Phylum columns -
## exactly what read_metaweb()'s $tax_codes already is in 04_diets.R).
## Matching is case-insensitive against valid_name first (the name
## resolve_codes() actually returns), then Old_name (covers a
## reference-file species spelled under an old/synonym name). Where a
## name has more than one row in tax_codes (185 in the template - a
## code's old vs. current WoRMS status can duplicate a name), the row
## with WORMS_status == "accepted" wins if there is one, else the first
## row - never an arbitrary/unstable pick left to match()'s default.
## =================================================================
fetch_taxonomy_metaweb <- function(species_names, tax_codes) {
  rank_cols <- c("Genus", "Family", "Order", "Class", "Phylum")
  missing_cols <- setdiff(c("valid_name", "Old_name", rank_cols), names(tax_codes))
  if (length(missing_cols) > 0) {
    stop("fetch_taxonomy_metaweb(): tax_codes is missing expected column(s): ", paste(missing_cols, collapse = ", "),
         " - pass the Taxonomic_codes sheet's own data.table (read_metaweb()$tax_codes), not something else.")
  }
  tc <- as.data.table(tax_codes)
  tc[, is_accepted := WORMS_status == "accepted" & !is.na(WORMS_status)]
  setorder(tc, -is_accepted)
  
  by_valid <- unique(tc, by = c("valid_name"))[, .(name_lower = tolower(valid_name), Genus, Family, Order, Class, Phylum)]
  by_old   <- unique(tc, by = c("Old_name"))[, .(name_lower = tolower(Old_name), Genus, Family, Order, Class, Phylum)]
  
  query_lower <- tolower(species_names)
  match_idx <- match(query_lower, by_valid$name_lower)
  out <- by_valid[match_idx]
  still_missing <- is.na(match_idx)
  if (any(still_missing)) {
    old_idx <- match(query_lower[still_missing], by_old$name_lower)
    out[still_missing] <- by_old[old_idx]
  }
  out[, ScientificName := species_names]
  setcolorder(out, c("ScientificName", rank_cols))
  out[]
}

## fetch_taxonomy_fishbase(): same output contract as
## fetch_taxonomy_worms() (ScientificName/Genus/Family/Order/Class/
## Phylum), but sourced from FishBase (fish) + SeaLifeBase (everything
## else - invertebrates, algae, etc.) via rfishbase::load_taxa(),
## instead of WoRMS. Made the project's default per the
## instruction (2026-09) to keep taxonomy consistent with 04_pbqb_calc.R,
## which already standardized on FishBase/SeaLifeBase (via rfishbase)
## rather than WoRMS. load_taxa() (not species()) is used deliberately -
## species() only carries Genus/Family, while load_taxa() returns the
## full Class/Order/Family/Genus/Species hierarchy needed for every
## rank_levels fallback step below, matching WoRMS's coverage. Phylum/
## Kingdom are only returned by SeaLifeBase's own load_taxa() (fish
## don't need a Phylum column to disambiguate FG assignment in practice,
## so its absence for the "fishbase" server is not filled in with a
## guess - it stays genuinely NA and Phylum-level fallback simply never
## fires for a fish species, same as it would for any other truly
## missing rank).
##
## Each server is retried (same 3-attempt exponential backoff idea as
## fetch_names_robust() in lib_worms_taxonomy_lookup.R) since FishBase's
## own Hugging Face-hosted backend is occasionally slow/flaky under
## load, not because a name is bad.
fetch_taxonomy_fishbase_uncached <- function(species_names, max_retries = 3) {
  if (!requireNamespace("rfishbase", quietly = TRUE)) {
    stop("rfishbase not installed - required for taxonomy_source='fishbase' or 'both'.")
  }
  fetch_one_server <- function(server) {
    for (attempt in seq_len(max_retries)) {
      ## load_taxa() has NEVER taken a species-name filter argument (old
      ## rfishbase: update/cache/server/limit; current: server/version/...)
      ## - it always returns the WHOLE taxa table for that server. Passing
      ## species_names positionally here (as this call used to) silently
      ## binds to whichever formal comes next after `server` is matched by
      ## name (e.g. `version`), handing rfishbase's internal code a
      ## 900+-element character vector where it expects a scalar - which is
      ## exactly the "the condition has length > 1" error this produced on
      ## every attempt/every server (not transient network flakiness, so
      ## the retry loop below never actually helped for THIS failure mode).
      ## Fix: call load_taxa() with no species argument at all and let the
      ## existing `tax[Species %in% species_names, ...]` filter a few lines
      ## down (in the caller) do the filtering, same as it already does.
      tax <- tryCatch(as.data.table(rfishbase::load_taxa(server = server)), error = function(e) e)
      if (!inherits(tax, "error")) return(tax)
      if (attempt < max_retries) {
        message("  rfishbase::load_taxa() failed on server '", server, "' (attempt ", attempt, "/", max_retries,
                "): ", conditionMessage(tax), " - retrying in ", 2^attempt, "s.")
        Sys.sleep(2^attempt)
      } else {
        message("  rfishbase::load_taxa() permanently failed on server '", server, "' after ", max_retries,
                " attempt(s): ", conditionMessage(tax))
      }
    }
    data.table()
  }
  ## Not every query is a clean "Genus species" binomial that can equal
  ## something in the Species column directly:
  ##   - a bare genus ("Alloteuthis", or whatever's left of a "Genus
  ##     spp."/"Genus sp." record once fallback_match_fg_by_taxonomy()
  ##     strips that marker before calling here) has no species epithet
  ##     at all;
  ##   - a trinomial/subspecies name ("Astropecten irregularis
  ##     pentacanthus") has ONE MORE word than Species ever does (always
  ##     just "Genus species", never "Genus species subspecies").
  ## Either way the exact Species match below will always miss, even
  ## though FishBase/SeaLifeBase almost certainly has the genus (and
  ## often the binomial) itself. Handled per-server in three tiers,
  ## most specific first, so a name only falls through to a coarser
  ## match when the more specific one genuinely isn't there:
  ##   1. exact Species match (a normal, fully-resolved binomial)
  ##   2. first-two-words match against Species (recovers a trinomial by
  ##      dropping its subspecies word - "Astropecten irregularis
  ##      pentacanthus" matches FishBase's "Astropecten irregularis" row)
  ##   3. first-word match against Genus (recovers a bare genus, or
  ##      anything tier 1/2 still missed, using any one row of that
  ##      genus - every species in it shares the same Family/Order/
  ##      Class/Phylum, so genus-level taxonomy is still real information
  ##      even without a species-level hit)
  results <- rbindlist(lapply(c("fishbase", "sealifebase"), function(server) {
    tax <- fetch_one_server(server)
    if (nrow(tax) == 0) return(NULL)
    keep_cols <- intersect(c("Species", "Genus", "Family", "Order", "Class", "Phylum"), names(tax))
    
    species_hits <- tax[Species %in% species_names, ..keep_cols]
    for (missing_col in setdiff(c("Genus", "Family", "Order", "Class", "Phylum"), keep_cols)) species_hits[, (missing_col) := NA_character_]
    setnames(species_hits, "Species", "ScientificName")
    still_missing_1 <- setdiff(species_names, species_hits$ScientificName)
    
    first_n_words <- function(x, n) sub(paste0("^((?:\\S+\\s+){", n - 1, "}\\S+).*$"), "\\1", x)
    
    binomial_hits <- data.table()
    if (length(still_missing_1) > 0 && "Species" %in% names(tax)) {
      query_binomial <- first_n_words(still_missing_1, 2)
      match_idx <- match(query_binomial, tax$Species)
      hit_rows <- !is.na(match_idx)
      if (any(hit_rows)) {
        binomial_hits <- tax[match_idx[hit_rows], ..keep_cols]
        binomial_hits[, ScientificName := still_missing_1[hit_rows]]
        binomial_hits[, Species := NULL]
      }
    }
    still_missing_2 <- setdiff(still_missing_1, if (nrow(binomial_hits) > 0) binomial_hits$ScientificName else character(0))
    
    genus_hits <- data.table()
    if (length(still_missing_2) > 0 && "Genus" %in% keep_cols) {
      query_genus <- first_n_words(still_missing_2, 1)
      genus_cols <- setdiff(keep_cols, "Species")
      genus_lookup <- unique(tax[, ..genus_cols], by = "Genus")
      match_idx <- match(query_genus, genus_lookup$Genus)
      hit_rows <- !is.na(match_idx)
      if (any(hit_rows)) {
        genus_hits <- genus_lookup[match_idx[hit_rows]]
        genus_hits[, ScientificName := still_missing_2[hit_rows]]
      }
    }
    rbindlist(list(species_hits, binomial_hits, genus_hits), fill = TRUE)
  }), fill = TRUE)
  not_found <- data.table(ScientificName = species_names, Genus = NA_character_, Family = NA_character_,
                          Order = NA_character_, Class = NA_character_, Phylum = NA_character_)
  if (is.null(results) || nrow(results) == 0) return(not_found)
  
  ## fishbase and sealifebase are disjoint in practice (a species is one
  ## or the other), but if a name genuinely comes back from both, keep
  ## whichever row has more ranks filled in rather than an arbitrary pick.
  results[, n_filled := rowSums(!is.na(.SD)), .SDcols = c("Genus", "Family", "Order", "Class", "Phylum")]
  setorder(results, ScientificName, -n_filled)
  results <- unique(results, by = "ScientificName")[, n_filled := NULL]
  
  ## Explicit "not found" rows for every input species NEITHER server
  ## resolved (same convention as worms_taxonomy_lookup_uncached() - see
  ## its own header comment: "'not found' results ARE cached too", so a
  ## genuinely unresolvable name isn't re-queried against FishBase/
  ## SeaLifeBase again on every future run just because it has no row here).
  still_missing <- setdiff(species_names, results$ScientificName)
  if (length(still_missing) > 0) results <- rbindlist(list(results, not_found[ScientificName %in% still_missing]))
  results
}

## Same on-disk RDS caching pattern as worms_taxonomy_lookup() (see that
## function's own header comment in lib_worms_taxonomy_lookup.R for the
## full rationale) - without this, switching the default taxonomy
## source from WoRMS to FishBase/SeaLifeBase would silently lose the
## re-run speedup every taxonomy_source = "worms" call site already
## relied on (cache_path was always threaded through for WoRMS; the
## un-cached fetch_taxonomy_fishbase() above never had that on its own).
fetch_taxonomy_fishbase <- function(species_names, cache_path = NULL) {
  cache <- if (!is.null(cache_path) && file.exists(cache_path)) readRDS(cache_path) else NULL
  
  ## Only trust a cached row as "already cached" if it actually resolved
  ## to something (at least one rank filled in). An all-NA "not found"
  ## row in the cache is indistinguishable from one that failed because
  ## the FETCH ITSELF was broken (e.g. the load_taxa() bug fixed earlier
  ## this session - every one of the 939 species affected by it would
  ## have been written to the cache as an all-NA "not found" row, and the
  ## OLD logic here trusted that forever, even after the code fix, since
  ## the cache is checked before load_taxa() is ever called again). So
  ## all-NA cache rows are treated as NOT cached and re-queried every
  ## run; only genuinely-resolved rows are skipped. This costs a small
  ## repeat query for names that are truly unresolvable (they can no
  ## longer be cached as "not found" once and skipped forever), in
  ## exchange for never permanently trusting a "not found" that was
  ## actually just a broken fetch.
  rank_cols <- c("Genus", "Family", "Order", "Class", "Phylum")
  resolved_cache_names <- if (!is.null(cache) && nrow(cache) > 0) {
    cache[rowSums(!is.na(cache[, ..rank_cols])) > 0, ScientificName]
  } else character(0)
  already_cached <- intersect(species_names, resolved_cache_names)
  to_query <- setdiff(species_names, already_cached)
  
  if (!is.null(cache_path)) {
    n_stale_not_found <- length(intersect(species_names, if (!is.null(cache)) cache$ScientificName else character(0))) - length(already_cached)
    message("  FishBase/SeaLifeBase taxonomy cache (", cache_path, "): ", length(already_cached),
            " of ", length(species_names), " name(s) already resolved & cached, ", length(to_query),
            " to (re-)query", if (n_stale_not_found > 0) paste0(" (", n_stale_not_found, " of those were cached 'not found' - retrying in case that was a stale/failed fetch)") else "", ".")
  }
  
  fresh_result <- if (length(to_query) > 0) fetch_taxonomy_fishbase_uncached(to_query) else NULL
  
  ## Drop any stale row for a name we just re-queried (it's either
  ## replaced by fresh_result or, if that still came back all-NA,
  ## re-added fresh below) - never keep the old row AND append a new
  ## one for the same ScientificName. Computed whether or not cache_path
  ## is set, since `all_known` below needs the same de-duplication.
  cache_kept <- if (!is.null(cache)) cache[!ScientificName %in% to_query] else NULL
  
  if (!is.null(cache_path)) {
    updated_cache <- if (is.null(cache_kept)) fresh_result else if (is.null(fresh_result)) cache_kept else rbindlist(list(cache_kept, fresh_result), fill = TRUE)
    if (!is.null(updated_cache)) {
      dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
      saveRDS(updated_cache, cache_path)
    }
  }
  
  all_known <- if (is.null(cache_kept)) fresh_result else if (is.null(fresh_result)) cache_kept else rbindlist(list(cache_kept, fresh_result), fill = TRUE)
  if (is.null(all_known) || nrow(all_known) == 0) {
    return(data.table(ScientificName = species_names, Genus = NA_character_, Family = NA_character_,
                      Order = NA_character_, Class = NA_character_, Phylum = NA_character_))
  }
  ## Every row of fetch_taxonomy_fishbase_uncached()'s own output already
  ## carries one row per requested name (see its "not found" backfill),
  ## so match() here should always find every name - but re-assert
  ## ScientificName from species_names explicitly rather than trusting
  ## the joined-in column to survive the reorder untouched, since a
  ## caller-supplied name genuinely absent from `all_known` (e.g. a
  ## brand-new name added after the cache file was last written, with no
  ## cache_path re-query triggered for some other reason) must still come
  ## back as ITS OWN name with NA taxonomy, never as a bare NA row.
  out <- all_known[match(species_names, ScientificName)]
  out[, ScientificName := species_names]
  out
}

## worms_cache_path: separate cache file for the WoRMS half of
## taxonomy_source = "both" (or a bare "worms" call). Kept as its own
## argument rather than reusing `cache_path` because the two sources'
## caches are keyed on completely different lookups (FishBase/
## SeaLifeBase load_taxa() tables vs WoRMS AphiaRecordsByNames results)
## and mixing them into one RDS file would mean every "both" run either
## re-queries WoRMS from scratch (if cache_path is reused verbatim, the
## fishbase cache read/write logic in fetch_taxonomy_fishbase() would
## never see WoRMS's own columns anyway) or silently drops the fishbase
## cache's speedup. NULL (default) = WoRMS calls are never cached, same
## as this function's behavior before "both" existed.
fetch_taxonomy <- function(species_names, taxonomy_source = "fishbase", cache_path = NULL, worms_cache_path = NULL) {
  if (taxonomy_source == "worms") return(fetch_taxonomy_worms(species_names, cache_path = if (!is.null(worms_cache_path)) worms_cache_path else cache_path))
  if (taxonomy_source == "fishbase") return(fetch_taxonomy_fishbase(species_names, cache_path = cache_path))
  if (taxonomy_source == "both") {
    fb_tax <- fetch_taxonomy_fishbase(species_names, cache_path = cache_path)
    ## Only what genuinely has NOTHING from FishBase/SeaLifeBase goes to
    ## WoRMS - Phylum-only rows (e.g. a Phylum-rank bare name FishBase
    ## can't resolve at all) still count as "nothing", so also fall
    ## through to WoRMS for those, not just fully-blank rows.
    still_missing <- setdiff(species_names, fb_tax[!is.na(Genus) | !is.na(Family) | !is.na(Order) | !is.na(Class) | !is.na(Phylum), ScientificName])
    if (length(still_missing) > 0) {
      message("  ", length(still_missing), " name(s) unresolved by FishBase/SeaLifeBase - trying WoRMS as well.")
      worms_tax <- fetch_taxonomy_worms(still_missing, cache_path = worms_cache_path)
      fb_tax <- rbindlist(list(fb_tax[!ScientificName %in% still_missing], worms_tax), fill = TRUE)
    }
    return(fb_tax)
  }
  stop("taxonomy_source must be 'fishbase' (default), 'worms', or 'both' - got '", taxonomy_source, "'")
}

## rank_levels tried in order, most specific first ("the lowest level
## possible", per how this was requested) - a species is matched at
## the first rank where its value maps to EXACTLY ONE FG among
## fg_lookup_safe's own species (the same "safe" rule used throughout
## this project: a genus/family/order/class spanning multiple FGs is
## too ecologically diverse to assign safely, and is skipped rather
## than guessed). This is fully data-driven from fg_lookup_safe - no
## hardcoded "Class Bivalvia -> FG Bivalves"-style rule tables needed
## anywhere; if the FG scheme changes, these safe mappings are re-
## derived automatically on the next run rather than needing manual
## updates to a separate rules table.
##
## 2026-09-17 review update (after inspecting real fallback output):
## several systemic mismatches, fixed here rather than by hand-patching
## individual species:
##   - Phylum DROPPED from the default rank_levels ("phylum usually
##     wrong" - a phylum is almost always too broad to safely stand in
##     for one specific FG). Still usable if a caller explicitly asks
##     for it via rank_levels, but no longer tried by default.
##   - allow_majority_vote now defaults to FALSE ("avoid match if
##     matched by multiple groups") - an ambiguous rank value (already-
##     assigned relatives split across 2+ FGs) is now left unresolved
##     rather than guessed at the FG with the most relatives. This
##     supersedes the earlier majority-vote behavior; set
##     allow_majority_vote = TRUE to restore it.
##   - exclude_fg_regex (case-insensitive, matched against FG_name)
##     removes whole FGs as fallback TARGETS - i.e. taxonomy is never
##     used to assign an unmatched species INTO one of these FGs, no
##     matter how exclusive the rank match looks. Default covers three
##     confirmed-bad cases: "commercial" (e.g. "Non-commercial decapods"
##     vs "Other commercial decapods" - a taxonomy-invisible distinction,
##     a name/commercial-status split, not a taxonomic one - see
##     Squilla mantis in SPECIES_EXCEPTIONS for why this matters),
##     "jellyfish" ("jellyfish usually wrong"), and "suprabenthos"/
##     "macrozooplankton" (ecologically-, not taxonomically-, defined
##     groups with no real rank of their own - e.g. Suprabenthos is
##     "small crustaceans living just above the seabed", a habitat/size
##     definition covering parts of Isopoda/Amphipoda, not those orders
##     whole; explicit SEED_RULES entries still assign into them by
##     exact Order name - this exclusion only blocks the GENERIC
##     genus/family/class fallback from also roping in unrelated
##     relatives).
##   - single_species_fg_broad_ranks: for these ranks (default Class,
##     Order), an FG that currently has only ONE species already
##     assigned is excluded as a fallback target - "if a single sp is
##     the FG then avoid that match" (e.g. a FG that's really just "the
##     purple sea urchin", "red coral", or "mackerels" as one specific
##     species shouldn't absorb every other unmatched species that
##     happens to share its Class/Order; Genus/Family fallback into a
##     single-species FG is still allowed, since a shared genus/family
##     is specific enough to be a real signal).
fallback_match_fg_by_taxonomy <- function(dt, fg_lookup_safe, taxonomy_source = "fishbase",
                                          rank_levels = c("Genus", "Family", "Order", "Class"),
                                          cache_path = NULL, worms_cache_path = NULL,
                                          allow_majority_vote = FALSE,
                                          exclude_fg_regex = "commercial|jellyfish|suprabenthos|macrozooplankton",
                                          single_species_fg_broad_ranks = c("Class", "Order")) {
  dt <- copy(dt)  # avoid data.table shallow-copy warning on := after this dt passed through merge()/subsetting upstream
  
  ## Excluded-FG filtering applied to fg_lookup_safe ONCE, up front - every
  ## rank-vote/exclusive-match computation below reads from this filtered
  ## copy, so an excluded FG can never become a fallback target via ANY
  ## rank, and a single-species FG is blocked only for the broad ranks
  ## listed in single_species_fg_broad_ranks (computed once here too,
  ## since "single species" is a property of the FULL fg_lookup_safe,
  ## not of whichever rank happens to be under consideration).
  fg_species_counts <- unique(fg_lookup_safe[, .(ScientificName, FG_num)])[, .(n_species_in_fg = uniqueN(ScientificName)), by = FG_num]
  excluded_fg_nums <- if (!is.null(exclude_fg_regex) && nzchar(exclude_fg_regex)) {
    unique(fg_lookup_safe[grepl(exclude_fg_regex, FG_name, ignore.case = TRUE), FG_num])
  } else integer(0)
  single_species_fg_nums <- fg_species_counts[n_species_in_fg == 1, FG_num]
  if (length(excluded_fg_nums) > 0) {
    message("Taxonomy fallback: ", length(excluded_fg_nums), " FG(s) excluded as fallback TARGETS entirely",
            " (name matches exclude_fg_regex = '", exclude_fg_regex, "'): ",
            paste(unique(fg_lookup_safe[FG_num %in% excluded_fg_nums, FG_name]), collapse = ", "))
  }
  ## Applied to the TARGET FG only, and only AFTER ambiguity (n_fg) has
  ## already been computed from the full, unfiltered fg_lookup_safe -
  ## filtering excluded/single-species FGs out of the reference BEFORE
  ## computing ambiguity would make an otherwise-ambiguous rank value
  ## (e.g. Order Decapoda, genuinely spanning "Deep shrimps" AND two
  ## commercial-status-named FGs) look falsely EXCLUSIVE once the
  ## commercial FGs are removed from the candidate pool - silently
  ## routing an unrelated decapod into "Deep shrimps" just because its
  ## real competitors happened to be excluded. Blocking the target
  ## after the fact instead just drops that match entirely (leaves the
  ## species unresolved), which is what "avoid this match" means.
  is_fg_blocked_for_rank <- function(fg_num_vec, rank) {
    fg_num_vec %in% excluded_fg_nums | (rank %in% single_species_fg_broad_ranks & fg_num_vec %in% single_species_fg_nums)
  }
  unmatched_sci <- unique(dt[!is.na(ScientificName) & is.na(FG_num), ScientificName])
  if (length(unmatched_sci) == 0) {
    message("No unmatched species - taxonomy fallback not needed.")
    return(dt)
  }
  message(length(unmatched_sci), " distinct species have no direct FG match -",
          " attempting taxonomy fallback via ", taxonomy_source,
          " (", paste(rank_levels, collapse = " -> "), ").")
  
  ## taxonomy for the FG reference species themselves too, for whichever
  ## rank columns aren't already present. Selects only missing_rank_cols
  ## from fg_taxonomy (NOT the full rank_levels) - selecting all of
  ## rank_levels here would re-merge in a second, differently-named copy
  ## (Genus.x/Genus.y) of any rank column fg_lookup_safe already had,
  ## silently breaking every later `rank %in% names(fg_lookup_safe)`
  ## check for that rank (caught via a synthetic test where fg_lookup_safe
  ## arrived with some but not all rank columns already populated - the
  ## real caller in 01_biomass.R always passes one with NONE of them
  ## populated, so all 5 are "missing" there and this collision never
  ## actually fired in production, but it's a real latent bug regardless).
  missing_rank_cols <- setdiff(rank_levels, names(fg_lookup_safe))
  if (length(missing_rank_cols) > 0) {
    fg_taxonomy <- fetch_taxonomy(fg_lookup_safe$ScientificName, taxonomy_source, cache_path = cache_path, worms_cache_path = worms_cache_path)
    fg_lookup_safe <- merge(fg_lookup_safe, fg_taxonomy[, c("ScientificName", missing_rank_cols), with = FALSE],
                            by = "ScientificName", all.x = TRUE)
  }
  
  ## Some unmatched records aren't full binomial species names at all -
  ## a genus-only / indeterminate-species identification ("Genus spp."
  ## or "Genus sp.", the formal marker for "identified only to genus"),
  ## or a bare higher-rank name used when even genus wasn't certain
  ## (e.g. survey data recording "Porifera" for an unidentified sponge).
  ## Querying FishBase/SeaLifeBase's Species column for either of these
  ## would always come back "not found" - they were never going to BE a
  ## Species value - even though the record already IS a real
  ## taxonomic identification, just coarser than species. Handled in
  ## two passes: (1) strip the "spp."/"sp." marker so the bare genus
  ## gets queried instead of the un-queryable whole string (fetch_
  ## taxonomy_fishbase_uncached() also now matches a bare genus against
  ## FishBase's own Genus column, not just Species, so this alone
  ## recovers real Family/Order/Class/Phylum for a lot of these); (2)
  ## whatever's STILL a single bare word with no space at all (a genus,
  ## family, order, class or phylum name and nothing else) is ALSO
  ## checked directly against fg_lookup_safe's own rank columns - free,
  ## no network call, and the only way to resolve a name like "Porifera"
  ## that will never be in FishBase's Genus column since Porifera is a
  ## phylum, not a genus.
  query_name <- sub("\\s+spp?\\.?\\s*$", "", unmatched_sci, ignore.case = TRUE)
  n_stripped <- sum(query_name != unmatched_sci)
  if (n_stripped > 0) {
    example_idx <- which(query_name != unmatched_sci)[1]
    message(n_stripped, " of ", length(unmatched_sci), " unmatched name(s) carry a 'spp.'/'sp.' genus-only",
            " marker - querying the bare genus instead (e.g. '", unmatched_sci[example_idx], "' -> '",
            query_name[example_idx], "').")
  }
  name_map <- data.table(ScientificName = unmatched_sci, query_name = query_name,
                         is_bare_word = !grepl("\\s", query_name))
  
  ## Reusable exclusive/majority resolver for one rank, computed fresh
  ## per rank from the full fg_lookup_safe (excluded-FG/single-species
  ## blocking applied only at the end, to the resolved target - see
  ## is_fg_blocked_for_rank()'s own comment at the top of this function
  ## for why) and reused both for the direct bare-word match here and
  ## for the main taxonomy-driven loop just below - same rule either way: exclusive
  ## if every already-assigned relative at this rank agrees on one FG;
  ## "majority" (most, not all, agree) is only ever returned when
  ## allow_majority_vote = TRUE (default FALSE as of 2026-09-17 - see
  ## this function's header comment).
  resolve_fg_votes_for_rank <- function(rank) {
    if (!rank %in% names(fg_lookup_safe)) {
      return(data.table(rank_value_lower = character(0), FG_num = numeric(0), FG_name = character(0),
                        match_type = character(0), vote_share = character(0)))
    }
    ## Votes computed from the FULL, unfiltered fg_lookup_safe - n_fg
    ## (genuine ambiguity) must reflect every FG that really shares this
    ## rank value, excluded or not (see is_fg_blocked_for_rank()'s own
    ## comment above for why). Exclusion is applied only at the very end,
    ## to the resolved TARGET FG.
    votes <- fg_lookup_safe[!is.na(get(rank)), .(n_species = uniqueN(ScientificName)), by = c(rank, "FG_num", "FG_name")]
    if (nrow(votes) == 0) return(data.table(rank_value_lower = character(0), FG_num = numeric(0), FG_name = character(0),
                                            match_type = character(0), vote_share = character(0)))
    setnames(votes, rank, "rank_value")
    votes[, rank_value_lower := tolower(rank_value)]
    votes[, total_at_value := sum(n_species), by = rank_value_lower]
    votes[, n_fg := uniqueN(FG_num), by = rank_value_lower]
    votes[, is_top := n_species == max(n_species), by = rank_value_lower]
    tied <- unique(votes[n_fg > 1 & is_top == TRUE, .N, by = rank_value_lower][N > 1, rank_value_lower])
    out <- if (allow_majority_vote) {
      votes[n_fg == 1 | (is_top == TRUE & !(rank_value_lower %in% tied))]
    } else {
      votes[n_fg == 1]  # exclusive-only: a rank value spanning multiple FGs is never a fallback target, guessed or not
    }
    out[, match_type := fifelse(n_fg == 1, "exclusive", "majority")]
    out[, vote_share := fifelse(match_type == "majority", paste0(n_species, "/", total_at_value), NA_character_)]
    out <- unique(out[, .(rank_value_lower, FG_num, FG_name, match_type, vote_share)])
    out[!is_fg_blocked_for_rank(FG_num, rank)]
  }
  
  direct_matches <- data.table(ScientificName = character(0), FG_num = numeric(0), FG_name = character(0),
                               match_type = character(0), match_rank = character(0), vote_share = character(0))
  bare_lookup <- name_map[is_bare_word == TRUE, .(ScientificName, rank_value_lower = tolower(query_name))]
  for (rank in rank_levels) {
    if (nrow(bare_lookup) == 0) break
    rank_votes <- resolve_fg_votes_for_rank(rank)
    if (nrow(rank_votes) == 0) next
    hits <- merge(bare_lookup, rank_votes, by = "rank_value_lower")
    if (nrow(hits) > 0) {
      hits[, match_rank := rank]
      direct_matches <- rbindlist(list(direct_matches, hits[, .(ScientificName, FG_num, FG_name, match_type, match_rank, vote_share)]), fill = TRUE)
      bare_lookup <- bare_lookup[!ScientificName %in% hits$ScientificName]
    }
  }
  if (nrow(direct_matches) > 0) {
    message(nrow(direct_matches), " bare genus/higher-rank name(s) matched DIRECTLY against already-assigned",
            " species' own Genus/Family/Order/Class/Phylum (no taxonomy fetch needed): ",
            paste(head(direct_matches$ScientificName, 10), collapse = ", "),
            if (nrow(direct_matches) > 10) ", ..." else "")
  }
  
  ## attached as an attribute on the return value (see bottom of this
  ## function, "fetched_taxonomy") so a caller with its own additional
  ## taxonomy-based logic can reuse this fetch instead of re-querying
  ## the same species. cache_path (see worms_taxonomy_lookup()'s own
  ## header comment) is what actually makes a RE-run of this pipeline
  ## fast - species already resolved on a previous run are read off
  ## disk instead of re-queried from WoRMS. Only names direct_matches
  ## didn't already resolve are fetched at all, using query_name (the
  ## spp./sp.-stripped form) so a genus-only record queries FishBase as
  ## its bare genus instead of the un-queryable full string.
  still_needs_fetch <- name_map[!ScientificName %in% direct_matches$ScientificName]
  fetch_targets <- unique(still_needs_fetch$query_name)
  ## Empty-case initialized with the full rank column set (fetch_taxonomy()
  ## always returns Genus/Family/Order/Class/Phylum regardless of
  ## rank_levels) - a bare data.table(query_name = character(0)) here
  ## would have zero rank columns at all, crashing the message() a few
  ## lines down the same way an empty data.table() crashed elsewhere in
  ## this project (see stock_assessment_fg_year in 01_biomass.R).
  fetched <- if (length(fetch_targets) > 0) {
    fetch_taxonomy(fetch_targets, taxonomy_source, cache_path = cache_path, worms_cache_path = worms_cache_path)
  } else {
    data.table(query_name = character(0), Genus = character(0), Family = character(0),
               Order = character(0), Class = character(0), Phylum = character(0))
  }
  if (length(fetch_targets) > 0) setnames(fetched, "ScientificName", "query_name")
  unmatched_taxonomy <- merge(still_needs_fetch[, .(ScientificName, query_name)], fetched, by = "query_name", all.x = TRUE)
  unmatched_taxonomy[, query_name := NULL]
  message(unmatched_taxonomy[!is.na(Genus) | !is.na(Family), .N], " of ",
          nrow(unmatched_taxonomy), " (of ", length(unmatched_sci), " total unmatched) found with usable genus/family via taxonomy fetch.")
  
  remaining <- copy(unmatched_taxonomy)
  matches_by_level <- list(direct = direct_matches)
  
  for (rank in rank_levels) {
    if (nrow(remaining) == 0) break
    if (!rank %in% names(fg_lookup_safe)) next
    
    ## n_fg (true ambiguity) computed from the FULL, unfiltered
    ## fg_lookup_safe - see is_fg_blocked_for_rank()'s own comment for
    ## why exclusion must NOT be applied before this. The excluded-FG/
    ## single-species-broad-rank block is applied afterward, only to
    ## the resolved target.
    rank_fg_counts <- fg_lookup_safe[!is.na(get(rank)), .(n_fg = uniqueN(FG_num)), by = rank]
    safe_values <- rank_fg_counts[n_fg == 1, get(rank)]
    ambiguous_values <- rank_fg_counts[n_fg > 1, get(rank)]
    
    if (length(safe_values) > 0) {
      rank_safe <- unique(fg_lookup_safe[get(rank) %in% safe_values, c(rank, "FG_num", "FG_name"), with = FALSE])
      rank_safe <- rank_safe[!is_fg_blocked_for_rank(FG_num, rank)]
      level_matches <- merge(remaining, rank_safe, by = rank)
      if (nrow(level_matches) > 0) {
        level_matches[, `:=`(match_type = "exclusive", match_rank = rank, vote_share = NA_character_)]
        matches_by_level[[paste0(rank, "_exclusive")]] <- level_matches[, .(ScientificName, FG_num, FG_name, match_type, match_rank, vote_share)]
        message("Resolved via ", rank, " fallback (exclusive - every already-assigned relative is in one FG): ", nrow(level_matches))
        remaining <- remaining[!ScientificName %in% level_matches$ScientificName]
      }
    }
    ## 2026-09-17 update: majority-vote-among-ambiguous-relatives is now
    ## OFF by default (allow_majority_vote = FALSE - "avoid match if
    ## matched by multiple groups") - an unmatched species whose rank
    ## value spans 2+ FGs among its already-assigned relatives is left
    ## unresolved (falls through to the next, coarser rank_levels entry
    ## or into "still_unresolved_taxonomy") rather than guessed at
    ## whichever FG happens to hold the most relatives. Set
    ## allow_majority_vote = TRUE on the call to restore the old
    ## behavior.
    if (allow_majority_vote && length(ambiguous_values) > 0 && nrow(remaining) > 0) {
      ## By design: an unmatched species can still be
      ## assigned by its closest relative even when that genus/family
      ## spans more than one FG - assign it to whichever FG holds the
      ## MAJORITY of its already-assigned relatives at this rank (e.g.
      ## 4 of 5 already-assigned Plesionika species sit in "Deep
      ## shrimps" -> the 5th Plesionika goes there too). A genuine tie
      ## (two or more FGs equally represented, no real majority) is left
      ## unresolved rather than guessed - falls through to the next,
      ## coarser rank_levels entry or into "still_unresolved_taxonomy".
      votes <- fg_lookup_safe[get(rank) %in% ambiguous_values & !is.na(FG_num),
                              .(n_species = uniqueN(ScientificName)), by = c(rank, "FG_num", "FG_name")]
      votes[, total_at_rank := sum(n_species), by = rank]
      votes[, is_top := n_species == max(n_species), by = rank]
      tied_values <- unique(votes[is_top == TRUE, .N, by = rank][N > 1, get(rank)])
      majority <- votes[is_top == TRUE & !(get(rank) %in% tied_values)]
      majority <- majority[!is_fg_blocked_for_rank(FG_num, rank)]
      
      if (length(tied_values) > 0) {
        message(length(tied_values), " ", rank, "-level value(s) tied between two or more FGs with no clear",
                " majority among their already-assigned relatives - left unresolved: ",
                paste(head(tied_values, 10), collapse = ", "), if (length(tied_values) > 10) ", ..." else "")
      }
      
      if (nrow(majority) > 0) {
        majority[, vote_share := paste0(n_species, "/", total_at_rank)]
        rank_majority <- unique(majority[, c(rank, "FG_num", "FG_name", "vote_share"), with = FALSE])
        level_matches <- merge(remaining, rank_majority, by = rank)
        if (nrow(level_matches) > 0) {
          level_matches[, `:=`(match_type = "majority", match_rank = rank)]
          matches_by_level[[paste0(rank, "_majority")]] <- level_matches[, .(ScientificName, FG_num, FG_name, match_type, match_rank, vote_share)]
          message("Resolved via ", rank, " fallback (majority vote among relatives): ", nrow(level_matches),
                  " (vote share e.g. ", paste(head(unique(level_matches$vote_share), 5), collapse = ", "),
                  if (uniqueN(level_matches$vote_share) > 5) ", ..." else "", ") - flagged match_type='majority' for review.")
          remaining <- remaining[!ScientificName %in% level_matches$ScientificName]
        }
      }
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
    data.table(ScientificName = character(0), FG_num = numeric(0), FG_name = character(0),
               match_type = character(0), match_rank = character(0), vote_share = character(0))
  }
  message("Still unresolved after all taxonomy levels: ", length(unmatched_sci) - nrow(fallback_matches))
  if (nrow(fallback_matches[match_type == "majority"]) > 0) {
    message(fallback_matches[match_type == "majority", .N], " of those were resolved by MAJORITY vote",
            " (ambiguous genus/family, assigned to the FG with the most already-assigned relatives) -",
            " see the 'fallback_match_detail' attribute / the taxonomy fallback review CSV for which ones.")
  }
  
  ## dt itself only gets FG_num/FG_name merged in (same as before) - the
  ## match_type/match_rank/vote_share detail is deliberately kept OFF dt
  ## (which downstream code reshapes/rbinds in ways that don't expect
  ## extra columns) and exposed only via the "fallback_match_detail"
  ## attribute below, for an audit CSV of exactly which species were
  ## assigned by majority vote vs. unanimous agreement among relatives.
  dt <- merge(dt, fallback_matches[, .(ScientificName, FG_num, FG_name)], by = "ScientificName", all.x = TRUE, suffixes = c("", "_fb"))
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
  ## Every species this call actually resolved via the taxonomy
  ## fallback (both "exclusive" and "majority" match_type), for an
  ## audit trail of which FG assignments came from real data vs. an
  ## inferred closest-relative guess.
  attr(dt, "fallback_match_detail") <- fallback_matches
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
  fallback_match_detail_data <- attr(dt, "fallback_match_detail")
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
  attr(dt, "fallback_match_detail") <- fallback_match_detail_data
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
## build_fg_manual_review_template() - added 2026-09-18. Turns the two
## per-source "still unmatched, needs manual review" exports written
## above (survey_unmatched_for_manual_review.csv from 01_biomass.R's
## MEDITS pass, medias_unmatched_for_manual_review.csv from its MEDIAS
## pass) into ONE combined, de-duplicated review workbook, with GF/
## FG_name columns ready to hand-fill.
##
## Uses the exact same 3 column names FG_WMed.xlsx's own sheet 4
## (fg_wmed_95) uses - ESPECIE, GF, FG_name (see `dataframe2 <-
## fg_raw[, .(ScientificName = ESPECIE, FG_num = GF, FG_name)]` near
## the top of 01_biomass.R) - so once GF/FG_name are filled in, columns
## A:C of the "For_review" sheet can be copy-pasted straight into that
## sheet with no renaming.
##
## Deliberately does NOT try to write the filled-in result back into
## FG_WMed.xlsx itself, or merge it into FG_spp automatically.
## FG_WMed.xlsx is the literal master species catalog every run reads
## fresh from pCloud (fg_file in 01_biomass.R's Configuration section) -
## the intended workflow is manual and one-directional: fill in GF/
## FG_name here, paste columns A:C into a NEW version of FG_WMed.xlsx's
## sheet 4, point future runs at that new file. That new file is then
## the single source of truth - there is nothing here to keep in sync
## with it afterward.
##
## csv_out_dir: the `output/biomass/` folder both unmatched CSVs were
## written into by 01_biomass.R (either file may be absent - e.g. if
## only one of MEDITS/MEDIAS has been run so far - handled gracefully).
## out_path: where to write the review workbook; defaults alongside
## csv_out_dir.
## =================================================================

## combine_unmatched_review_csvs() - shared helper factored out of
## build_fg_manual_review_template() on 2026-09-19 so
## build_full_fg_species_catalog() (below) can reuse the exact same
## read/merge/de-duplicate logic for its own "needs review" tail,
## rather than a second, driftable copy of it. Returns NULL if
## neither unmatched CSV exists. Does NOT add ESPECIE/GF/FG_name -
## callers add those themselves, since the two callers want slightly
## different final column sets.
combine_unmatched_review_csvs <- function(csv_out_dir) {
  taxonomy_cols <- c("Genus", "Family", "Order", "Class", "Phylum")
  
  read_one <- function(fname, source_label) {
    fpath <- file.path(csv_out_dir, fname)
    if (!file.exists(fpath)) {
      message("'", fname, "' not found in '", csv_out_dir, "' - skipping (run the matching ",
              "01_biomass.R pass first if you expected species from this source here).")
      return(NULL)
    }
    dt <- fread(fpath)
    if (nrow(dt) == 0) return(NULL)
    setnames(dt, "mean_biomass", paste0("mean_biomass_", source_label))
    setnames(dt, "n_appearances", paste0("n_appearances_", source_label))
    dt
  }
  
  survey_dt <- read_one("survey_unmatched_for_manual_review.csv", "survey")
  medias_dt <- read_one("medias_unmatched_for_manual_review.csv", "medias")
  
  if (is.null(survey_dt) && is.null(medias_dt)) return(NULL)
  
  if (!is.null(survey_dt) && !is.null(medias_dt)) {
    ## Full outer join on ScientificName. Taxonomy columns are coalesced
    ## afterward, not joined on - both sources should agree on a given
    ## species' taxonomy, and joining on those columns risks silently
    ## dropping a row over a formatting mismatch (e.g. trailing whitespace).
    shared_taxonomy <- intersect(taxonomy_cols, intersect(names(survey_dt), names(medias_dt)))
    medias_for_merge <- if (length(shared_taxonomy) > 0) {
      medias_dt[, setdiff(names(medias_dt), shared_taxonomy), with = FALSE]
    } else {
      medias_dt
    }
    combined <- merge(survey_dt, medias_for_merge, by = "ScientificName", all = TRUE)
    for (col in shared_taxonomy) {
      medias_lookup <- medias_dt[[col]][match(combined$ScientificName, medias_dt$ScientificName)]
      filled <- ifelse(is.na(combined[[col]]) | combined[[col]] == "", medias_lookup, combined[[col]])
      set(combined, j = col, value = filled)
    }
  } else {
    combined <- if (!is.null(survey_dt)) copy(survey_dt) else copy(medias_dt)
  }
  
  n_cols <- intersect(c("n_appearances_survey", "n_appearances_medias"), names(combined))
  combined[, n_appearances_total := rowSums(as.data.frame(combined)[, n_cols, drop = FALSE], na.rm = TRUE)]
  setorder(combined, -n_appearances_total)
  combined
}

build_fg_manual_review_template <- function(csv_out_dir,
                                            out_path = file.path(csv_out_dir, "FG_manual_review_template.xlsx")) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) {
    stop("openxlsx is required to write '", out_path, "'.")
  }
  
  taxonomy_cols <- c("Genus", "Family", "Order", "Class", "Phylum")
  
  combined <- combine_unmatched_review_csvs(csv_out_dir)
  if (is.null(combined)) {
    stop("Neither survey_unmatched_for_manual_review.csv nor medias_unmatched_for_manual_review.csv",
         " was found in '", csv_out_dir, "' - nothing to build a review template from.",
         " Run 01_biomass.R first.")
  }
  
  ## GF/FG_name go right after the species name, using FG_WMed.xlsx sheet 4's
  ## own raw column names verbatim - see the function header comment above.
  combined[, ESPECIE := ScientificName]
  combined[, GF := NA_character_]
  combined[, FG_name := NA_character_]
  
  present_taxonomy <- intersect(taxonomy_cols, names(combined))
  ordered_cols <- c("ESPECIE", "GF", "FG_name", "n_appearances_total",
                    intersect(c("n_appearances_survey", "mean_biomass_survey",
                                "n_appearances_medias", "mean_biomass_medias"), names(combined)),
                    present_taxonomy)
  review <- combined[, ordered_cols, with = FALSE]
  
  readme <- data.table(
    Step = 1:5,
    Instructions = c(
      paste0("This sheet lists every species 01_biomass.R could not match to a Functional ",
             "Group this run, combined across MEDITS and MEDIAS and de-duplicated by scientific name."),
      "Sorted by n_appearances_total (most-observed species first) - review those first, they carry the most weight in the model.",
      "For each row, fill in GF (the FG number) and FG_name (must match an existing FG_name exactly, or be a deliberate new one) on the 'For_review' sheet.",
      "Once filled in, copy columns A:C (ESPECIE, GF, FG_name) and paste them as new rows into FG_WMed.xlsx's own sheet 4 (fg_wmed_95). Save that as a NEW version of FG_WMed.xlsx - don't overwrite the original in place.",
      "Point fg_file (in 01_biomass.R's Configuration section) at the new FG_WMed.xlsx and re-run - these species will no longer appear in survey_unmatched_for_manual_review.csv / medias_unmatched_for_manual_review.csv."
    )
  )
  
  openxlsx::write.xlsx(
    list(README = readme, For_review = review),
    file = out_path,
    colNames = TRUE
  )
  message("Wrote ", nrow(review), " unmatched species to '", out_path, "'.")
  invisible(review)
}

## =================================================================
## snapshot_full_extent_species() - added 2026-09-19, rewritten
## 2026-09-20. Writes ONE row per distinct species a given source's
## FG-matching step actually saw - BEFORE that source's own
## FILTER_AREAS/year restriction, i.e. every species anywhere in the
## raw file(s) that source loaded (every GSA, every year the raw data
## covers), independent of whatever FILTER_AREAS/YEAR_ECOPATH/TS_YEARS
## THIS run of 01_biomass.R happens to be configured for. Called once
## per source (MEDITS, MEDIAS, stock assessment) at the call site right
## after that source's own matching step - see each call site's own
## comment in 01_biomass.R for exactly where and why that point is
## already before any area/year restriction.
##
## matched_dt: the source's own post-matching table (dt for MEDITS,
## acoustic_matched for MEDIAS, etc.) - must have a scientific-name
## column (sci_name_col) plus FG_num/FG_name (NA where unmatched).
## species_taxonomy: the run's already-built taxonomy lookup
## (ScientificName + Genus/Family/Order/Class/Phylum) to attach by
## left join - built once per run, reused across every source's
## snapshot rather than re-fetched per source.
## =================================================================
snapshot_full_extent_species <- function(matched_dt, species_taxonomy, source_label, csv_out_dir,
                                         sci_name_col = "ScientificName") {
  taxonomy_cols <- c("Genus", "Family", "Order", "Class", "Phylum")
  
  snap <- unique(matched_dt[!is.na(get(sci_name_col)), c(sci_name_col, "FG_num", "FG_name"), with = FALSE])
  if (sci_name_col != "ScientificName") setnames(snap, sci_name_col, "ScientificName")
  
  present_taxonomy <- intersect(taxonomy_cols, names(species_taxonomy))
  if (length(present_taxonomy) > 0) {
    snap <- merge(snap, unique(species_taxonomy[, c("ScientificName", present_taxonomy), with = FALSE]),
                  by = "ScientificName", all.x = TRUE)
  }
  snap[, source := source_label]
  
  out_path <- file.path(csv_out_dir, paste0("all_species_", tolower(source_label), "_full_extent.csv"))
  fwrite(snap, out_path)
  message("Saved ", nrow(snap), " distinct species (", source_label, ", full available extent -",
          " every GSA/year the raw data covers, independent of this run's FILTER_AREAS/",
          "YEAR_ECOPATH/TS_YEARS) to '", out_path, "'.")
  invisible(snap)
}

## =================================================================
## build_full_fg_species_catalog() - added 2026-09-19, rewritten
## 2026-09-20 to (a) cover MEDITS + MEDIAS + stock assessment, not just
## MEDITS+MEDIAS, (b) always reflect the whole available extent
## regardless of this run's FILTER_AREAS/YEAR_ECOPATH/TS_YEARS - not
## something that requires a special "widest FILTER_AREAS" run - and
## (c) write a plain CSV instead of an .xlsx workbook.
##
## Reads back the three all_species_<source>_full_extent.csv snapshots
## snapshot_full_extent_species() writes during Steps 3-4 (MEDITS), 9
## (MEDIAS), and the stock-assessment block (all BEFORE that source's
## own area/year restriction - see each snapshot call site in
## 01_biomass.R) - never the FILTER_AREAS-scoped FG_spp_Ecopath.csv or
## the per-run unmatched-review CSVs, which is what made the OLD version
## of this function only reflect one run's configured region/years.
## A source's snapshot simply won't exist for a run where that source
## didn't execute at all (AREA_MODE == "custom" skips MEDIAS and stock
## assessment entirely - see 01_biomass.R's STEP 9 header comment) -
## handled gracefully below, same convention as combine_unmatched_review_csvs().
##
## One row per distinct species across whichever sources exist:
## ESPECIE/FG_number/FG_name (FG_number mirrors FG_WMed.xlsx sheet 4's
## "GF" column under a clearer name), status ("matched" if any source
## resolved it to an FG, else "needs
## review"), sources (which of MEDITS/MEDIAS/stock assessment actually
## observed it - e.g. "MEDITS+MEDIAS"), and taxonomy.
##
## csv_out_dir: `output/biomass/` from any 01_biomass.R run (any
## AREA_MODE/FILTER_AREAS/YEAR_ECOPATH - see above). out_path: where to
## write the catalog; defaults alongside csv_out_dir.
## =================================================================
build_full_fg_species_catalog <- function(csv_out_dir,
                                          out_path = file.path(csv_out_dir, "FG_WMed_full_species_catalog.csv")) {
  taxonomy_cols <- c("Genus", "Family", "Order", "Class", "Phylum")
  sources <- c(medits = "MEDITS", medias = "MEDIAS", stock_assessment = "stock_assessment")
  
  snapshots <- lapply(names(sources), function(key) {
    fpath <- file.path(csv_out_dir, paste0("all_species_", key, "_full_extent.csv"))
    if (!file.exists(fpath)) {
      message("'", basename(fpath), "' not found in '", csv_out_dir, "' - skipping ", sources[[key]],
              " (either this run's AREA_MODE doesn't run that source, or 01_biomass.R hasn't been",
              " run yet).")
      return(NULL)
    }
    fread(fpath)
  })
  names(snapshots) <- names(sources)
  snapshots <- snapshots[!vapply(snapshots, is.null, logical(1))]
  if (length(snapshots) == 0) {
    stop("None of the all_species_*_full_extent.csv snapshots were found in '", csv_out_dir,
         "' - run 01_biomass.R first (these are written during Steps 3-4/9/stock-assessment,",
         " each right after that source's own FG-matching step).")
  }
  all_snapshots <- rbindlist(snapshots, fill = TRUE)
  
  ## One row per species: FG_num/FG_name coalesced across sources (they
  ## should agree - same fg_lookup_safe matching cascade everywhere -
  ## first non-NA value wins if they ever don't), taxonomy coalesced the
  ## same way, sources joined into one "MEDITS+MEDIAS"-style label.
  present_taxonomy <- intersect(taxonomy_cols, names(all_snapshots))
  full_catalog <- all_snapshots[, c(
    list(
      FG_number = FG_num[which(!is.na(FG_num))[1]],
      FG_name   = FG_name[which(!is.na(FG_name))[1]],
      sources   = paste(sort(unique(source)), collapse = "+")
    ),
    lapply(present_taxonomy, function(col) {
      vals <- get(col)
      vals[which(!is.na(vals) & vals != "")[1]]
    }) |> setNames(present_taxonomy)
  ), by = ScientificName]
  
  setnames(full_catalog, "ScientificName", "ESPECIE")
  full_catalog[, status := ifelse(!is.na(FG_number), "matched", "needs review")]
  setcolorder(full_catalog, c("ESPECIE", "FG_number", "FG_name", "status", "sources", present_taxonomy))
  setorder(full_catalog, status, FG_number, ESPECIE)
  
  fwrite(full_catalog, out_path)
  n_matched <- full_catalog[status == "matched", .N]
  message("Wrote ", nrow(full_catalog), " species (", n_matched, " matched, ",
          nrow(full_catalog) - n_matched, " needing review) from ", paste(names(snapshots), collapse = "+"),
          " to '", out_path, "'.")
  invisible(full_catalog)
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

## =================================================================
## Multi-method sample-level outlier detection - runs up to three
## complementary tests on the same (AreaID, ScientificName) grouping,
## log1p(Density), and min_samples floor as remove_sample_outliers()
## above (see that function's own comments for why each of those
## choices), then combines them via a consensus rule:
##
##  - "mad"        robust z-score (median/MAD) > mad_threshold - the
##                 method remove_sample_outliers() implements above;
##                 see its header comment for the calibration story
##                 (threshold=70, tuned against a synthetic genuine-
##                 event-vs-error test).
##  - "percentile" outside the [pctl_lower, pctl_upper] percentile band
##                 of that species' own within-area distribution - the
##                 METHOD RoME's check_weight() uses for official MEDITS
##                 QC (5th-95th percentile of mean individual weight).
##                 RoME's own reference bands come from a fixed external
##                 2012-2022 Mediterranean-wide dataset baked into that
##                 package and not exposed as a reusable table from R,
##                 so this reproduces the percentile-band METHOD using
##                 each species' own distribution in THIS survey's data
##                 as the reference instead - not a literal reproduction
##                 of RoME's specific numeric bands.
##  - "boxplot"    outside Tukey's classic IQR fences (Q1 - boxplot_coef
##                 * IQR, Q3 + boxplot_coef*IQR; boxplot_coef=1.5 is
##                 R's own boxplot() default) - the same rule RoME's
##                 check_abundance() draws as a box-plot for visual
##                 screening; this automates that rule instead of
##                 leaving it to eyeballing a plot.
##
## consensus = "all" (default) requires every ACTIVE method (i.e. every
## method named in `methods`) to agree before a sample is flagged/
## removed - the conservative choice. EXPECT "percentile" and "boxplot"
## to each catch many more samples on their own than "mad" does at its
## calibrated threshold=70 - they are much more aggressive at their
## textbook defaults (5%, 1.5xIQR), so requiring all three to agree
## keeps this from becoming far more aggressive than the single-method
## version above. consensus = "any" flags anything ANY active method
## catches (most aggressive); consensus = "majority" needs a majority
## of the active methods.
## =================================================================
remove_sample_outliers_multi <- function(dt, methods = c("mad", "percentile", "boxplot"),
                                         consensus = c("all", "any", "majority"),
                                         mad_threshold = 70, pctl_lower = 0.05, pctl_upper = 0.95,
                                         boxplot_coef = 1.5, min_samples = 5, drop_outliers = TRUE,
                                         log_scale = TRUE) {
  consensus <- match.arg(consensus)
  methods <- intersect(methods, c("mad", "percentile", "boxplot"))
  if (length(methods) == 0) stop("methods must include at least one of 'mad', 'percentile', 'boxplot'.")
  
  dt <- copy(dt)
  dt[, obs_id := .I]
  dt[, eval_value := if (log_scale) log1p(Density) else Density]
  
  stats <- dt[!is.na(eval_value) & !is.na(ScientificName),
              .(group_median = median(eval_value, na.rm = TRUE),
                group_mad = mad(eval_value, na.rm = TRUE),
                q_lo = quantile(eval_value, pctl_lower, na.rm = TRUE, names = FALSE),
                q_hi = quantile(eval_value, pctl_upper, na.rm = TRUE, names = FALSE),
                iqr_q1 = quantile(eval_value, 0.25, na.rm = TRUE, names = FALSE),
                iqr_q3 = quantile(eval_value, 0.75, na.rm = TRUE, names = FALSE),
                n_obs_in_group = .N),
              by = .(AreaID, ScientificName)]
  stats[, iqr := iqr_q3 - iqr_q1]
  stats[, box_lo := iqr_q1 - boxplot_coef * iqr]
  stats[, box_hi := iqr_q3 + boxplot_coef * iqr]
  
  dt <- merge(dt, stats, by = c("AreaID", "ScientificName"), all.x = TRUE)
  enough <- !is.na(dt$eval_value) & !is.na(dt$n_obs_in_group) & dt$n_obs_in_group >= min_samples
  
  dt[, is_outlier_mad := FALSE]
  dt[, is_outlier_percentile := FALSE]
  dt[, is_outlier_boxplot := FALSE]
  
  if ("mad" %in% methods) {
    dt[, robust_z := ifelse(enough & group_mad > 0, abs(eval_value - group_median) / group_mad, NA_real_)]
    dt[, is_outlier_mad := enough & !is.na(robust_z) & robust_z > mad_threshold]
  }
  if ("percentile" %in% methods) {
    dt[, is_outlier_percentile := enough & (eval_value < q_lo | eval_value > q_hi)]
  }
  if ("boxplot" %in% methods) {
    dt[, is_outlier_boxplot := enough & (eval_value < box_lo | eval_value > box_hi)]
  }
  
  flag_cols <- paste0("is_outlier_", methods)
  n_active <- length(methods)
  dt[, n_methods_flagged := rowSums(.SD), .SDcols = flag_cols]
  dt[, is_outlier := switch(consensus,
                            all = n_methods_flagged == n_active,
                            any = n_methods_flagged >= 1,
                            majority = n_methods_flagged > n_active / 2)]
  
  flagged <- dt[is_outlier == TRUE, c("ScientificName", "AreaID", "Year", "SampleID",
                                      "Density", flag_cols, "n_methods_flagged"), with = FALSE]
  setnames(flagged, "Density", "flagged_density")
  setorder(flagged, -n_methods_flagged)
  flagged[, was_dropped := drop_outliers]
  
  if (nrow(flagged) > 0) {
    message("\n", nrow(flagged), " sample-level observation(s) flagged as outliers under the '",
            consensus, "' consensus rule across method(s): ", paste(methods, collapse = ", "), ".",
            if (drop_outliers) " REMOVED from the data (drop_outliers=TRUE)." else
              " NOT removed - drop_outliers=FALSE, this is a report only.",
            " Full detail (most methods in agreement first):")
    print(flagged)
    message("\nFlagged, by species:")
    print(dt[is_outlier == TRUE, .N, by = ScientificName][order(-N)])
    if (drop_outliers) {
      dt[is_outlier == TRUE, `:=`(Density = NA_real_, Biomass = NA_real_)]
    }
  } else {
    message("\nNo sample-level observations flagged under the '", consensus, "' consensus rule",
            " across method(s): ", paste(methods, collapse = ", "), ".")
  }
  
  drop_cols <- c("obs_id", "eval_value", "group_median", "group_mad", "q_lo", "q_hi",
                 "iqr_q1", "iqr_q3", "iqr", "box_lo", "box_hi", "n_obs_in_group",
                 "robust_z", "n_methods_flagged", flag_cols, "is_outlier")
  dt[, (intersect(drop_cols, names(dt))) := NULL]
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
## Outlier diagnostic plot - visualizes what remove_sample_outliers()
## or remove_sample_outliers_multi() actually did: per-species log-
## density distribution (as a box-plot, the same visual convention
## RoME's check_abundance() uses for MEDITS QC screening) with flagged/
## removed observations overlaid as points. Pass the SAME dt that went
## INTO the outlier-removal function (i.e. BEFORE removal, so the
## removed values still have their real Density rather than NA) plus
## the flagged_outliers table pulled off its output via
## attr(dt_after, "flagged_outliers"). Restricted to the species that
## actually had something flagged by default (top_n_species caps how
## many get shown, since a full-community plot would be unreadable).
## =================================================================
plot_outlier_diagnostic <- function(dt_before, flagged_outliers, top_n_species = 12) {
  if (is.null(flagged_outliers) || nrow(flagged_outliers) == 0) {
    stop("plot_outlier_diagnostic() needs a non-empty flagged_outliers table",
         " (attr(<result of remove_sample_outliers[_multi]()>, \"flagged_outliers\")) -",
         " nothing was flagged, so there's nothing to diagnose.")
  }
  species_to_show <- flagged_outliers[, .N, by = ScientificName][
    order(-N)][seq_len(min(top_n_species, .N)), ScientificName]
  
  plot_data <- dt_before[ScientificName %in% species_to_show & !is.na(Density),
                         .(ScientificName, AreaID, SampleID, Density)]
  plot_data[, log_density := log1p(Density)]
  
  flagged_pts <- unique(flagged_outliers[ScientificName %in% species_to_show,
                                         .(ScientificName, AreaID, SampleID,
                                           log_density = log1p(flagged_density))])
  
  ggplot(plot_data, aes(x = ScientificName, y = log_density)) +
    geom_boxplot(outlier.shape = NA, fill = "grey90", color = "grey40") +
    geom_jitter(width = 0.15, alpha = 0.25, size = 0.8, color = "steelblue") +
    geom_point(data = flagged_pts, color = "firebrick", size = 2.2, shape = 17) +
    coord_flip() +
    labs(title = "Sample-level outlier diagnostic",
         subtitle = "Grey box-plots: within-species distribution (log1p density). Red triangles: flagged outliers.",
         x = NULL, y = "log1p(Density)") +
    theme_minimal(base_size = 11)
}

## =================================================================
## Area-weight composition plot - what fraction of each area's density
## estimate is contributed by each depth stratum (i.e. the prop =
## area_km2/sum(area_km2) weights weight_by_strata() actually applies),
## shown as a donut per area so the relative influence of the
## AquaMaps pseudo-strata (when active) versus the real MEDITS strata
## is visible at a glance, rather than buried in a wide sheet of
## numbers. Reads the SAME per_stratum attribute plot_strata_profile()
## and build_fg_density_by_stratum_sheet() use, so it automatically
## reflects whichever strata_def actually built fg_index_stratified
## (5 real strata, or the 8-strata AquaMaps-extended set).
## =================================================================
plot_area_weight_donut <- function(fg_index_stratified, strata_def, area_ids = NULL) {
  per_stratum <- attr(fg_index_stratified, "per_stratum")
  if (is.null(per_stratum)) {
    stop("plot_area_weight_donut() needs the per_stratum attribute from weight_by_strata()",
         " (the area-merged one, since this plot is specifically about each stratum's AREA",
         " weight - prop - not just whether it has samples).")
  }
  props <- unique(per_stratum[, .(AreaID, Stratum, prop)])
  if (!is.null(area_ids)) props <- props[AreaID %in% area_ids]
  
  props <- merge(props, strata_def, by.x = "Stratum", by.y = "stratum_num", all.x = TRUE)
  props[, depth_label := fifelse(
    is.na(depth_min) | is.na(depth_max), paste0("Stratum ", Stratum),
    paste0(formatC(depth_min, format = "f", digits = 0), "-",
           formatC(depth_max, format = "f", digits = 0), "m"))]
  props[, depth_label := factor(depth_label, levels = unique(depth_label[order(Stratum)]))]
  
  ## donut = pie with a hole, built the standard ggplot way (stacked bar
  ## + coord_polar), ymax/ymin per wedge computed manually so a hole can
  ## be punched via xlim() rather than needing a separate library
  props[, ymax := cumsum(prop), by = AreaID]
  props[, ymin := ymax - prop, by = AreaID]
  
  ggplot(props, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = depth_label)) +
    geom_rect() +
    coord_polar(theta = "y") +
    xlim(c(2, 4)) +
    facet_wrap(vars(AreaID), labeller = label_both) +
    labs(title = "Area proportion (prop) by depth stratum", fill = "Depth stratum") +
    theme_void(base_size = 11) +
    theme(legend.position = "right")
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
## write_native_sheet_csv() / write_native_sheets_csv() / read_native_sheet_csv()
##
## Added 2026-09-17: the excel ecopath_ecosim file must have exactly the
## intended sheets, trimmed script by script - every other table should be
## saved as a csv file, not kept in the final output excel file. Every
## table that USED TO go into the
## shared workbook as its own native/intermediate sheet (FG_spp_Ecopath,
## Ecopath, Ecosim, Catches_Ecopath, PB_QB, References, Ecobase, and so
## on - none of them one of the 9 intended final
## final workbook: info, FG_spp, Ecopath_B, Ecopath_L, Ecopath_Di, Ecopath_PBQB,
## Ecopath_traits, Ecopath_diet, Ecosim_ts) now goes here instead - a
## plain CSV in out_dir, named after the sheet it replaces, so a LATER
## script/function that used to read it back out of the workbook
## (finalize_ecopath_ecosim_summary_sheets(), read_full_fg_reference(),
## 04_diets.R's own biomass-share reader, the PB_QB_spp cross-run
## "already has real data" guard) can still find it - just from disk
## instead of from a workbook sheet that no longer exists there.
##
## out_dir is always dirname(the shared workbook's own path) - every
## call site already has that workbook path in scope, so no new
## argument needs threading through the numbered scripts for this.
## =================================================================
write_native_sheet_csv <- function(dt, name, out_dir) {
  path <- file.path(out_dir, paste0(name, ".csv"))
  fwrite(dt, path)
  invisible(path)
}

write_native_sheets_csv <- function(sheets, out_dir) {
  if (is.null(names(sheets)) || any(names(sheets) == "")) {
    stop("write_native_sheets_csv(): every element of `sheets` needs a name (used as the CSV filename).")
  }
  for (nm in names(sheets)) write_native_sheet_csv(sheets[[nm]], nm, out_dir)
  message("write_native_sheets_csv(): wrote ", length(sheets), " native/intermediate sheet(s) as CSV",
          " in ", out_dir, " (kept OUT of the final workbook): ", paste(names(sheets), collapse = ", "))
  invisible(sheets)
}

read_native_sheet_csv <- function(name, out_dir) {
  path <- file.path(out_dir, paste0(name, ".csv"))
  if (!file.exists(path)) return(NULL)
  as.data.table(fread(path))
}

## =================================================================
## trim_workbook_to_final_sheets()
##
## The single, canonical definition of "the exact intended sheets"
## in ecopath_ecosim_inputs.xlsx - info, then the 7 Ecopath_*/Ecosim_ts
## summary sheets, in that order, nothing else. Every numbered script
## (01/02/03/04) now calls this at the very end of its own run, AFTER
## writing whichever of these 8 sheets it's responsible for (native/
## intermediate tables having already gone to write_native_sheets_csv()
## instead) - so the workbook is trimmed to ONLY the target sheets that
## exist so far after every single script, not just after the last one
## to run. A target sheet that doesn't exist yet (because an earlier
## script in the 01->02->03->04 order hasn't run against this workbook
## yet) is simply not there yet - finalize_workbook_sheet_order() skips
## it with a message rather than erroring, same as before.
## =================================================================
trim_workbook_to_final_sheets <- function(out_path) {
  finalize_workbook_sheet_order(
    out_path = out_path,
    ## FG_References added 2026-09-17 - see build_fg_references_sheet()
    ## below. 10 final sheets now, not 9.
    target_order = c("info", "FG_spp", "Ecopath_B", "Ecopath_L", "Ecopath_Di", "Ecopath_PBQB",
                     "Ecopath_traits", "Ecopath_diet", "Ecosim_ts", "FG_References"),
    drop_extras = TRUE
  )
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
## Sheets that exist in the workbook but are NOT in target_order are,
## by DEFAULT (drop_extras = FALSE), kept and appended at the end, in
## their original relative order - never silently dropped, just
## flagged with a warning so an unexpected/forgotten sheet doesn't
## slip by unnoticed. Pass drop_extras = TRUE (added 2026-09-16, per
## the request for a workbook containing ONLY the final summary
## sheets - "info", Ecopath_B/L/Di/PBQB/traits/diet, Ecosim_ts - not
## every native sheet the individual pipeline scripts wrote along the
## way) to instead REMOVE every sheet not in target_order. Only call
## this with drop_extras = TRUE once, at the very END of the WHOLE
## pipeline (currently: at the end of 04_diets.R's run_pipeline(), the
## last script to run) - calling it mid-pipeline would delete native
## sheets (e.g. FG_spp_Ecopath) that a LATER script still needs to read.
## =================================================================
finalize_workbook_sheet_order <- function(out_path, rename_map = character(0), target_order, drop_extras = FALSE) {
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
    if (drop_extras) {
      message("finalize_workbook_sheet_order(): drop_extras = TRUE - removing sheet(s) not in",
              " target_order: ", paste(extra_sheets, collapse = ", "))
      ## Pin the active sheet to 1 BEFORE removing anything. openxlsx's
      ## Workbook keeps an "active sheet" index and tries to restore it
      ## on save; removeWorksheet() can drop the sheet that index points
      ## at (or shift indices below it), so by the time saveWorkbook()
      ## runs, that stored index can point past the end of the now-
      ## shorter sheet list - which fails with "wb$setactiveSheet(...):
      ## N doesn't exist as sheet index." Setting it to a sheet that's
      ## guaranteed to survive (1 = one of the target sheets, since this
      ## whole workbook always keeps at least "info") before any removal
      ## avoids that entirely.
      tryCatch(openxlsx::activeSheet(wb) <- 1, error = function(e) NULL)
      for (nm in extra_sheets) openxlsx::removeWorksheet(wb, nm)
      tryCatch(openxlsx::activeSheet(wb) <- 1, error = function(e) NULL)
    } else {
      warning("finalize_workbook_sheet_order(): sheet(s) in the workbook but NOT in target_order -",
              " kept, appended at the end rather than dropped: ", paste(extra_sheets, collapse = ", "))
    }
  }
  
  current_names <- names(wb)  # re-read again - removeWorksheet() above (if it ran) changes both the
  # sheet count and index positions, so target_present's indices into
  # the ORIGINAL current_names would silently point at the wrong sheets
  final_order_names <- if (drop_extras) target_present else c(target_present, extra_sheets)
  new_position_of_current_index <- match(final_order_names, current_names)
  ## Belt-and-suspenders: openxlsx's own worksheetOrder<-() captures
  ## wb$ActiveSheet, reassigns the order, then tries to restore that
  ## captured index - deleteWorksheet() never updates wb$ActiveSheet, so
  ## a stale index (from before any sheets were removed above) can be
  ## out of range for the CURRENT, possibly-shorter sheet list and make
  ## that restore fail. Pinning it to 1 immediately before this call
  ## guarantees a valid index going in, whether or not sheets were
  ## removed just above.
  tryCatch(openxlsx::activeSheet(wb) <- 1, error = function(e) NULL)
  openxlsx::worksheetOrder(wb) <- new_position_of_current_index
  
  openxlsx::saveWorkbook(wb, out_path, overwrite = TRUE)
  message("finalize_workbook_sheet_order(): saved '", out_path, "' - final sheet order: ",
          paste(names(wb), collapse = ", "))
  invisible(wb)
}

## =================================================================
## finalize_ecopath_ecosim_summary_sheets()
##
## Added 2026-09-16 "beside [keeping] csv files or
## intermediate files ... the excel file should include the following
## sheets: Ecopath_B, Ecopath_L, Ecopath_Di, Ecopath_PBQB,
## Ecopath_traits, Ecosim_ts (with B, L, Di and effort)". Ecopath_traits
## is written directly by 03_pbqb-traits.R, Ecopath_B directly by
## export_ecopath_ecosim_excel() (01_biomass.R), and Ecopath_PBQB
## directly by add_pbqb_to_ecopath_workbook() (03_pbqb-traits.R) - none
## of those three need building here. This function builds the
## remaining two, Ecopath_L/Ecopath_Di and Ecosim_ts, by reading the
## native/intermediate CSVs 01_biomass.R/02_fisheries.R write via
## write_native_sheets_csv() (2026-09-17 update: other sheets should be
## saved as csv files, not kept in the final output excel file - these
## used to be read back from native WORKBOOK sheets
## of the same name; the source data is identical, just off disk now
## instead of out of a sheet that no longer exists in the workbook).
## Call this after 02_fisheries.R (for Ecopath_L/Di and the L/Di/Effort
## parts of Ecosim_ts) and again after 03_pbqb-traits.R if anything
## upstream changed - a source CSV that doesn't exist yet is skipped
## with a message, not an error, so this is always safe to call.
##
## Ecopath_L/Ecopath_Di are built from Catches_Discards_FG_ts, NOT from
## Catches_Ecopath's own Catch_* columns - that sheet's columns can be
## split by Fleet depending on settings, and it's not always clear by
## name alone whether it holds plain landings or gross catch (landings
## + discards). Catches_Discards_FG_ts has unambiguous Landings_t/
## Catch_t/Discard_t columns at plain FG x Year grain, which is what
## both new sheets actually need.
## =================================================================
finalize_ecopath_ecosim_summary_sheets <- function(out_path, year_ecopath,
                                                   biomass_csv_dir = NULL,
                                                   fisheries_csv_dir = NULL) {
  ## This function combines native CSVs written by TWO different pipeline
  ## blocks - Ecosim (biomass block) with Catches_Discards_FG_ts/Catches_
  ## Ecosim/Fishing_Effort_by_Fleet (fisheries block) - so it needs two
  ## separate directories, not one. Both default to dirname(out_path) for
  ## back-compat with the old flat-output-directory layout.
  biomass_csv_dir   <- if (is.null(biomass_csv_dir)) dirname(out_path) else biomass_csv_dir
  fisheries_csv_dir <- if (is.null(fisheries_csv_dir)) dirname(out_path) else fisheries_csv_dir
  out_dir <- dirname(out_path)
  read_sheet <- function(nm, dir) read_native_sheet_csv(nm, dir)
  
  out_sheets <- list()
  
  ## --- Catches_Discards_FG_ts read: feeds Ecosim_ts's Discards block
  ## below, NOT Ecopath_L/Ecopath_Di anymore. Those two final sheets are
  ## now built and written directly in 02_fisheries.R, per FG x Fleet
  ## (one column per fleet, from fleet_split_out's Landings_t/Discard_t)
  ## - this FG-only, no-fleet-dimension time series can't produce that
  ## shape, so it's no longer used for them (2026-09-17).
  cd_ts <- read_sheet("Catches_Discards_FG_ts", fisheries_csv_dir)
  if (is.null(cd_ts)) {
    message("finalize_ecopath_ecosim_summary_sheets(): 'Catches_Discards_FG_ts.csv' not found in ", fisheries_csv_dir,
            " - skipping the Ecosim_ts Discards block (run 02_fisheries.R against this workbook first).",
            " Ecopath_L/Ecopath_Di are unaffected - they're written directly by 02_fisheries.R now.")
  }
  
  ## --- Ecosim_ts: B (from Ecosim) + L (from Catches_Ecosim) + Di
  ## (built fresh from Catches_Discards_FG_ts - no existing Ecosim-
  ## format Di sheet to borrow) + Effort (pivoted from Fishing_Effort_
  ## by_Fleet's long Fleet x Year shape into one column per fleet) -
  ## all combined into ONE sheet using the same meta-row-then-one-row-
  ## per-year convention every native Ecosim-format sheet already uses,
  ## so this reads as "the same kind of sheet, just with every driver
  ## in one place" rather than a different layout altogether.
  ecosim_b <- read_sheet("Ecosim", biomass_csv_dir)
  ecosim_l <- read_sheet("Catches_Ecosim", fisheries_csv_dir)
  effort   <- read_sheet("Fishing_Effort_by_Fleet", fisheries_csv_dir)
  meta_labels <- c("Name", "Type", "Usage", "Scaling", "Weight", "Target", "2nd target", "Interval")
  
  if (!is.null(ecosim_b)) {
    years <- suppressWarnings(as.numeric(ecosim_b[[1]][-seq_along(meta_labels)]))
    combined <- data.table(` ` = c(meta_labels, as.character(years)))
    
    add_block <- function(sheet_dt, new_prefix) {
      if (is.null(sheet_dt)) return(invisible(NULL))
      value_cols <- setdiff(names(sheet_dt), names(sheet_dt)[1])
      for (col in value_cols) {
        vals <- sheet_dt[[col]]
        meta   <- vals[seq_along(meta_labels)]   # keep the source sheet's own descriptive "Name" row as-is (it's already e.g. "B_Hake"/"C_Hake") - only the combined data.table's own COLUMN KEY gets prefixed below, so B_/L_/Di_/Effort_ columns pulled from different source sheets never collide once combined into one sheet
        yrvals <- vals[-seq_along(meta_labels)]
        combined[, (paste0(new_prefix, "_", col)) := c(meta, yrvals)]
      }
    }
    add_block(ecosim_b, "B")
    add_block(ecosim_l, "L")
    
    if (!is.null(cd_ts)) {
      for (fg_num in unique(cd_ts$FG_num)) {
        fg_name <- cd_ts[FG_num == fg_num, FG_name][1]
        ts_vals <- vapply(years, function(y) {
          v <- cd_ts[FG_num == fg_num & Year == y, Discard_t]
          if (length(v) == 0) NA_real_ else v[1]
        }, numeric(1))
        col <- c(paste0("Di_", gsub("[^A-Za-z0-9]+", "", fg_name)), "Discards", "reference", "absolute",
                 "1", paste0(fg_num, ": ", fg_name), "", "Annual", as.character(ts_vals))
        combined[, (paste0("Di_fg_", fg_num)) := col]
      }
    }
    
    if (!is.null(effort)) {
      value_col <- intersect(c("nom_active_kWdays_effective", "nom_active_kWdays"), names(effort))[1]
      if (!is.na(value_col)) {
        effort[, Year := as.numeric(Year)]
        for (fl in unique(effort$Fleet)) {
          ts_vals <- vapply(years, function(y) {
            v <- effort[Fleet == fl & Year == y, get(value_col)]
            if (length(v) == 0) NA_real_ else v[1]
          }, numeric(1))
          col <- c(paste0("Effort_", gsub("[^A-Za-z0-9]+", "", fl)), "Effort", "reference", "absolute",
                   "1", fl, "", "Annual", as.character(ts_vals))
          combined[, (paste0("Effort_fleet_", gsub("[^A-Za-z0-9]+", "", fl))) := col]
        }
      }
    }
    
    out_sheets$Ecosim_ts <- combined
  } else {
    message("finalize_ecopath_ecosim_summary_sheets(): 'Ecosim.csv' not found in ", biomass_csv_dir,
            " - skipping Ecosim_ts (run 01_biomass.R against this workbook first).")
  }
  
  if (length(out_sheets) == 0) {
    message("finalize_ecopath_ecosim_summary_sheets(): nothing to write - none of the source CSVs were found yet.")
    return(invisible(NULL))
  }
  upsert_workbook_sheets(out_sheets, out_path)
  message("finalize_ecopath_ecosim_summary_sheets(): wrote ", paste(names(out_sheets), collapse = ", "), ".")
  invisible(out_sheets)
}

## =================================================================
## build_info_sheet() - added 2026-09-16, the final
## workbook should lead with an "info" sheet ("with data from the run,
## GSAs, time ecopath, time ecosim region...."). A plain Field/Value
## table, best-effort - every argument defaults to reading the matching
## config variable straight out of .GlobalEnv (the numbered scripts all
## set these - FILTER_AREAS/TARGET_COUNTRIES/YEAR_ECOPATH/TS_YEARS - and
## since the whole pipeline runs in ONE R session via run_pipeline_demo.R,
## they're all still in scope by the time this is called at the very
## end), and falls back to "not available" rather than erroring if a
## script was run standalone/out of order and that variable was never set.
## =================================================================
build_info_sheet <- function(filter_areas = NULL, target_countries = NULL, year_ecopath = NULL, ts_years = NULL,
                             region_name = NULL) {
  get_or_default <- function(val, var_name, envir = .GlobalEnv) {
    if (!is.null(val)) return(val)
    if (exists(var_name, envir = envir, inherits = FALSE)) get(var_name, envir = envir) else NULL
  }
  filter_areas     <- get_or_default(filter_areas, "FILTER_AREAS")
  target_countries <- get_or_default(target_countries, "TARGET_COUNTRIES")
  year_ecopath     <- get_or_default(year_ecopath, "YEAR_ECOPATH")
  ts_years         <- get_or_default(ts_years, "TS_YEARS")
  region_name      <- get_or_default(region_name, "AREA_NAME")
  
  fmt <- function(x, as_range = FALSE) {
    if (is.null(x) || length(x) == 0 || all(is.na(x))) return("not available")
    if (as_range && is.numeric(x) && length(x) > 1) return(paste(range(x), collapse = "-"))
    paste(x, collapse = ", ")
  }
  
  data.table(
    Field = c("Run timestamp", "Region", "GSAs (FILTER_AREAS)", "Countries (TARGET_COUNTRIES)",
              "Ecopath base year(s) (YEAR_ECOPATH)", "Ecosim time series range (TS_YEARS)"),
    Value = c(as.character(Sys.time()), fmt(region_name), fmt(filter_areas), fmt(target_countries),
              fmt(year_ecopath, as_range = TRUE), fmt(ts_years, as_range = TRUE))
  )
}

## Lets a sheet-writer match ITS year-row range to another sheet
## already in the workbook (e.g. Catches_Ecosim matching Ecosim's
## years) WITHOUT requiring that other sheet to have been written
## first - if it isn't there yet, this just returns NULL and the
## caller falls back to deriving its own range, order-independently.
## 2026-09-17 update, CSV-not-workbook-sheet refactor: sheet_name
## (e.g. "Ecosim") is now a native/intermediate table written
## as a CSV alongside the workbook, not a workbook sheet - read from
## there instead of openxlsx::read.xlsx().
read_existing_ts_years <- function(out_path, sheet_name, csv_dir = NULL) {
  existing <- read_native_sheet_csv(sheet_name, if (is.null(csv_dir)) dirname(out_path) else csv_dir)
  if (is.null(existing)) return(NULL)
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
                                        normalize_ts = TRUE,     ## TRUE = rescale to reference index (first value = 1); FALSE = raw density
                                        csv_out_dir = NULL) {    ## directory for this function's own native CSV outputs - defaults to dirname(out_path) for back-compat, but 01_biomass.R passes its own "biomass" subfolder here so this block's CSVs land there instead of at the shared workbook's top level
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
  out_dir <- if (is.null(csv_out_dir)) dirname(out_path) else csv_out_dir
  
  ## --- FG_spp_Ecopath (native/intermediate, CSV-only) --------------------------
  ## NOT the final workbook's FG_spp sheet - that one is built from the
  ## more complete union (observed + full reference catalog) in
  ## 01_biomass.R's own STEP 11 and written directly there. This table
  ## only covers species actually observed in species_density_regional.
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
  
  ## --- Ecopath_B sheet ----------------------------------------------------------
  ## Built from full_fg_list (every FG in dataframe2), not from
  ## fg_index_regional directly - an FG with zero observed biomass in
  ## year_ecopath (e.g. nothing in that FG was caught during the base
  ## years specifically) still needs a row for Ecopath model-building,
  ## just with blank biomass rather than being silently absent from the
  ## whole sheet. NOT rescaled by normalize_ts - it's a single base-year
  ## snapshot, not a time series, so "first value = 1" doesn't apply.
  ##
  ## Final Ecopath_B sheet is exactly FG_num, FG_name, Biomass (t/km2) -
  ## ONE biomass column, not two. The value used is the year_ecopath-
  ## range average (mean across every year in year_ecopath, not just
  ## year_ecopath[1]) - this matches the averaging window Ecopath_PBQB/
  ## Ecopath_L/Ecopath_Di use elsewhere for the same base period, so all
  ## final sheets describe the same "year_ecopath average" snapshot
  ## rather than mixing a single-year value into one sheet and a
  ## multi-year average into the others. The single-base-year value is
  ## NOT dropped - it's still written out, alongside the range average,
  ## in an audit-only CSV (biomass_by_fg_ecopath_detail.csv) for anyone
  ## who wants to compare the two.
  base_year <- year_ecopath[1]
  ecopath_base <- fg_index_regional[Year == base_year, .(FG_num, Biomass_baseyear = mean_density)]
  ecopath_avg  <- fg_index_regional[Year %in% year_ecopath,
                                    .(Biomass_avg = mean(mean_density, na.rm = TRUE)), by = FG_num]
  ecopath_detail <- merge(full_fg_list, ecopath_base, by = "FG_num", all.x = TRUE)
  ecopath_detail <- merge(ecopath_detail, ecopath_avg, by = "FG_num", all.x = TRUE)
  setnames(ecopath_detail, c("Biomass_baseyear", "Biomass_avg"),
           c(paste0("Biomass_", base_year), paste0("Biomass_", min(year_ecopath), "_", max(year_ecopath))))
  setorder(ecopath_detail, FG_num)
  write_native_sheet_csv(ecopath_detail, "biomass_by_fg_ecopath_detail", out_dir)
  
  ecopath_sheet <- ecopath_detail[, .(FG_num, FG_name,
                                      Biomass = get(paste0("Biomass_", min(year_ecopath), "_", max(year_ecopath))))]
  setorder(ecopath_sheet, FG_num)
  
  n_fg_no_biomass <- ecopath_sheet[is.na(Biomass), .N]
  if (n_fg_no_biomass > 0) {
    message(n_fg_no_biomass, " of ", nrow(ecopath_sheet), " FG(s) have no observed biomass in ",
            min(year_ecopath), "-", max(year_ecopath), " - included with blank biomass, not omitted:")
    print(ecopath_sheet[is.na(Biomass), .(FG_num, FG_name)])
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
  
  ## 2026-09-17 update: the excel ecopath_ecosim file must have exactly
  ## the intended sheets, trimmed script by script - every other table
  ## should be saved as a csv file, not kept in the final output excel
  ## file. Only Ecopath_B (the final target sheet built here) goes to the
  ## workbook directly. FG_spp_Ecopath / Ecosim / FG_spp_Ecosim are native/
  ## intermediate tables now written as CSV only (never as workbook
  ## sheets), via write_native_sheets_csv() below. (Note: the final
  ## workbook's FG_spp sheet is written separately, from 01_biomass.R's
  ## more complete FG_spp_Ecopath table which also includes reference-
  ## catalog species with zero observed density - see that script's
  ## STEP 11.)
  write_native_sheets_csv(list(FG_spp_Ecopath = fg_spp_sheet,
                               Ecosim = ecosim_sheet,
                               FG_spp_Ecosim = fg_spp_ecosim_sheet),
                          out_dir)
  sheets_to_write <- list(Ecopath_B = ecopath_sheet)
  
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
  ## 2026-09-17 update: PB_QB_spp is a native/intermediate table (not one
  ## of the 9 final sheets), so its cross-run "has real data" guard now
  ## reads the CSV written by add_pbqb_to_ecopath_workbook() instead of a
  ## workbook sheet - that CSV is the only place PB_QB_spp lives now.
  pbqb_spp_has_real_data <- FALSE
  existing_pbqb_spp <- read_native_sheet_csv("PB_QB_spp", out_dir)
  if (!is.null(existing_pbqb_spp) && "PB" %in% names(existing_pbqb_spp) &&
      any(!is.na(existing_pbqb_spp$PB))) {
    pbqb_spp_has_real_data <- TRUE
  }
  
  if (!pbqb_spp_has_real_data) {
    pbqb_spp_scaffold <- fg_spp_sheet[, .(FG_num, FG_name, Species,
                                          Biomass = Density, prop_biomass_FG = prop_sp_fg)]
    pbqb_spp_scaffold[, `:=`(PB = NA_real_, prop_biomass_PB = NA_real_,
                             QB = NA_real_, prop_biomass_QB = NA_real_,
                             Note = "PB/QB not yet computed - run 04_pbqb_calc.R (Step 4) to fill in")]
    setorder(pbqb_spp_scaffold, FG_num, -Biomass)
    write_native_sheet_csv(pbqb_spp_scaffold, "PB_QB_spp", out_dir)
    message("PB_QB_spp written as a SCAFFOLD CSV (Species/FG_num/Biomass/prop_biomass_FG only -",
            " PB/QB left blank) since Step 4 (04_pbqb_calc.R) hasn't run against this workbook",
            " yet. Run 04_pbqb_calc.R afterward to fill in real PB/QB values - it replaces this",
            " scaffold CSV with the full table automatically, no extra step needed here.")
  } else {
    message("PB_QB_spp already has real PB/QB data (04_pbqb_calc.R has already run against",
            " this workbook) - leaving its CSV untouched rather than overwriting it with a",
            " blank scaffold.")
  }
  
  ## extra_sheets: named list of additional data.tables/data.frames the
  ## caller wants in the FINAL workbook alongside Ecopath_B - kept as a
  ## workbook escape hatch (not CSV) since a caller passing this argument
  ## is explicitly asking for it to land in the deliverable workbook.
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
                                            ts_years = NULL, fleet_structure = NULL,
                                            csv_out_dir = NULL, biomass_csv_dir = NULL) {
  ## csv_out_dir: directory for this function's OWN native CSV outputs
  ## (Catches_Ecopath/Catches_Ecosim/Fleet_Structure) - defaults to
  ## dirname(out_path) for back-compat. biomass_csv_dir: directory to
  ## read Ecosim.csv from (a biomass-block output, read via
  ## read_existing_ts_years() below) when ts_years isn't passed
  ## explicitly - also defaults to dirname(out_path).
  
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
    ts_years <- read_existing_ts_years(out_path, "Ecosim", csv_dir = biomass_csv_dir)
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
  
  ## 2026-09-17 update: Catches_Ecopath/Catches_Ecosim/Fleet_Structure are
  ## native/intermediate tables (not final target sheets) - written as
  ## CSV only, never to the workbook directly. Ecopath_L/Ecopath_Di (the
  ## actual final sheets derived from catch data) are built by
  ## finalize_ecopath_ecosim_summary_sheets() from Catches_Discards_FG_ts,
  ## which 02_fisheries.R writes separately - not from this function's
  ## own output - so no summary-sheet call is added here.
  sheets_to_write <- list(Catches_Ecopath = catches_ecopath, Catches_Ecosim = catches_ecosim)
  if (multi_fleet) {
    sheets_to_write$Fleet_Structure <- fleet_tbl
    message("Fleet_Structure sheet written - ", uniqueN(fleet_tbl$Fleet), " fleet(s) (",
            paste(fleet_names, collapse = ", "), ") across ", uniqueN(fleet_tbl$FG_num), " FG(s).")
  }
  write_native_sheets_csv(sheets_to_write, if (is.null(csv_out_dir)) dirname(out_path) else csv_out_dir)
  
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
## Reads whichever FG name/number reference is already IN the shared
## workbook - "FG" if 01_biomass.R's finalize_workbook_sheet_order()
## has already renamed FG_lookup -> FG, or "FG_lookup" itself if that
## rename hasn't run yet (03_pbqb-traits.R has no fixed run-order
## requirement relative to that finalize call). Returns NULL (not an
## error) if neither sheet exists yet - callers degrade to "whatever
## FGs this run's own data happened to cover" the same way they always
## did before this existed.
## 2026-09-17 update: other sheets should be saved as csv files, not
## kept in the final output excel file - FG_lookup is a native
## reference table, not one of the 9 final sheets, so it no longer
## lives in the workbook at all (01_biomass.R now writes it via
## write_native_sheets_csv() as FG_lookup.csv). Reads that CSV from the
## same directory as the workbook instead of a "FG"/"FG_lookup" sheet.
read_full_fg_reference <- function(out_path, csv_dir = NULL) {
  ref <- read_native_sheet_csv("FG_lookup", if (is.null(csv_dir)) dirname(out_path) else csv_dir)
  if (is.null(ref) || !all(c("FG_num", "FG_name") %in% names(ref))) return(NULL)
  unique(ref[, .(FG_num, FG_name)])
}

## =================================================================
## build_fg_references_sheet() - added 2026-09-17. Writes the final
## workbook's "FG_References" sheet: one row per FG, consolidating
## WHICH DATA SOURCE fed each block for that FG, so the model's data
## provenance is reviewable FG by FG rather than scattered across each
## block's own audit CSVs. Columns: FG_num, FG_name, ref_B (biomass
## source), ref_fisheries (catch/discard source), ref_pbqb_traits (PB/QB
## data source), ref_diet (diet-study citations), ref_methods (the
## calculation METHOD/literature behind PB_FG/QB_FG - distinct from
## ref_pbqb_traits, which is about the DATA feeding that method, not
## the method itself).
##
## Each argument is a directory to read that block's own native CSV
## outputs from (see the 2026-09-17 per-block CSV subfolder update) -
## every one defaults to dirname(out_path) for back-compat, same
## pattern as every other cross-block reader in this file. Safe to
## call any time - a block that hasn't run yet simply leaves its
## column NA for every FG, with a message, rather than erroring; run
## it again (04_diets.R does, always last) once every block has run to
## get a fully populated sheet.
##
## This is a best-effort SUMMARY, not a full audit trail - each column
## folds together whatever distinct source values that block used
## across every year/species/study for that FG (" | "-joined if more
## than one). For the full detail behind any one of these, the
## individual block's own audit CSV (biomass_source in
## survey_fg_annual_index_regional_combined.csv, catch_source in
## Catches_Discards_FG_ts.csv, PB_source/QB_source in PB_QB.csv,
## dispatch_group in PB_QB_spp.csv, the Reference column in the
## metaweb DATA_ENTRY sheet via diet_references_by_fg.csv) is what to
## open instead.
## =================================================================
build_fg_references_sheet <- function(out_path,
                                      biomass_csv_dir = NULL,
                                      fisheries_csv_dir = NULL,
                                      pbqb_csv_dir = NULL,
                                      diet_csv_dir = NULL) {
  biomass_csv_dir   <- if (is.null(biomass_csv_dir))   dirname(out_path) else biomass_csv_dir
  fisheries_csv_dir <- if (is.null(fisheries_csv_dir)) dirname(out_path) else fisheries_csv_dir
  pbqb_csv_dir      <- if (is.null(pbqb_csv_dir))      dirname(out_path) else pbqb_csv_dir
  diet_csv_dir      <- if (is.null(diet_csv_dir))      dirname(out_path) else diet_csv_dir
  
  full_fg_ref <- read_full_fg_reference(out_path, csv_dir = biomass_csv_dir)
  if (is.null(full_fg_ref)) {
    message("build_fg_references_sheet(): no FG_lookup.csv found in ", biomass_csv_dir,
            " - run 01_biomass.R against this workbook first. Skipping the FG_References sheet",
            " for now (nothing written this call).")
    return(invisible(NULL))
  }
  refs <- copy(full_fg_ref)
  
  ## --- ref_B: biomass_source, per FG, from 01_biomass.R's own
  ## survey_fg_annual_index_regional_combined.csv (Year x FG_num, one
  ## biomass_source value per row already - see 01_biomass.R's "FG
  ## biomass-source priority" section).
  biomass_src_path <- file.path(biomass_csv_dir, "survey_fg_annual_index_regional_combined.csv")
  if (file.exists(biomass_src_path)) {
    b <- fread(biomass_src_path)
    if ("biomass_source" %in% names(b)) {
      b_by_fg <- b[, .(ref_B = paste(sort(unique(biomass_source)), collapse = " | ")), by = FG_num]
      refs <- merge(refs, b_by_fg, by = "FG_num", all.x = TRUE)
    } else {
      message("build_fg_references_sheet(): '", biomass_src_path, "' has no biomass_source column",
              " (older run?) - ref_B left blank.")
    }
  } else {
    message("build_fg_references_sheet(): '", biomass_src_path, "' not found - run 01_biomass.R",
            " against this workbook first. ref_B left blank for now.")
  }
  if (!"ref_B" %in% names(refs)) refs[, ref_B := NA_character_]
  
  ## --- ref_fisheries: catch_source, per FG, from 02_fisheries.R's own
  ## Catches_Discards_FG_ts.csv (native/intermediate, fixed filename -
  ## unlike the DATASET_VERSION-suffixed CSV of the same data).
  cd_ts <- read_native_sheet_csv("Catches_Discards_FG_ts", fisheries_csv_dir)
  if (!is.null(cd_ts) && "catch_source" %in% names(cd_ts)) {
    f_by_fg <- cd_ts[, .(ref_fisheries = paste(sort(unique(catch_source)), collapse = " | ")), by = FG_num]
    refs <- merge(refs, f_by_fg, by = "FG_num", all.x = TRUE)
  } else {
    message("build_fg_references_sheet(): 'Catches_Discards_FG_ts.csv' not found (or has no catch_source",
            " column) in ", fisheries_csv_dir, " - run 02_fisheries.R against this workbook first.",
            " ref_fisheries left blank for now.")
  }
  if (!"ref_fisheries" %in% names(refs)) refs[, ref_fisheries := NA_character_]
  
  ## --- ref_pbqb_traits + ref_methods: from 03_pbqb-traits.R's own
  ## PB_QB.csv (FG-level: PB_source/QB_source if EcoBase gap-filling
  ## ran this session, otherwise every FG is the same "empirical"
  ## default) and PB_QB_spp.csv (species-level: dispatch_group, which
  ## calculation method actually ran for each species feeding that FG -
  ## mapped below to its literature citation for ref_methods).
  pbqb <- read_native_sheet_csv("PB_QB", pbqb_csv_dir)
  if (!is.null(pbqb)) {
    if (all(c("PB_source", "QB_source") %in% names(pbqb))) {
      pbqb[, ref_pbqb_traits := paste0("PB: ", fifelse(is.na(PB_source), "n/a", PB_source),
                                       "; QB: ", fifelse(is.na(QB_source), "n/a", QB_source))]
    } else {
      pbqb[, ref_pbqb_traits := "empirical (species-level PB/QB, biomass-weighted to FG) - see PB_QB_spp.csv for the species behind each FG"]
    }
    refs <- merge(refs, pbqb[, .(FG_num, ref_pbqb_traits)], by = "FG_num", all.x = TRUE)
  } else {
    message("build_fg_references_sheet(): 'PB_QB.csv' not found in ", pbqb_csv_dir,
            " - run 03_pbqb-traits.R against this workbook first. ref_pbqb_traits left blank for now.")
  }
  if (!"ref_pbqb_traits" %in% names(refs)) refs[, ref_pbqb_traits := NA_character_]
  
  ## dispatch_group -> literature citation. Extend this lookup if
  ## 03_pbqb-traits.R's own dispatch logic (calc_fish()/calc_invert()/
  ## etc.) ever adds a new dispatch_group value - anything not listed
  ## here just falls through with a generic note instead of erroring.
  method_citation <- c(
    fish        = "Fish P/B, Q/B from growth (VBGF) and natural/fishing mortality - method for estimating P/B and Q/B for EwE models (see pipeline_documentation.Rmd's Methodology reference)",
    invert      = "Benthic invertebrate P/B, Q/B from empirical length/weight-based relationships",
    cephalopod  = "Cephalopod P/B, Q/B from short-lived life-history convention",
    literature  = "EcoBase model repository (published literature P/B, Q/B)"
  )
  spp <- read_native_sheet_csv("PB_QB_spp", pbqb_csv_dir)
  if (!is.null(spp) && "dispatch_group" %in% names(spp)) {
    m_by_fg <- spp[!is.na(dispatch_group), .(dispatch_groups = paste(sort(unique(dispatch_group)), collapse = ",")), by = FG_num]
    m_by_fg[, ref_methods := vapply(strsplit(dispatch_groups, ","), function(groups) {
      hits <- unique(method_citation[groups])
      hits <- hits[!is.na(hits)]
      unmatched <- setdiff(groups, names(method_citation))
      if (length(unmatched) > 0) hits <- c(hits, paste0("dispatch_group='", unmatched, "' (no citation on file)"))
      if (length(hits) == 0) return(NA_character_)
      paste(hits, collapse = " | ")
    }, character(1))]
    refs <- merge(refs, m_by_fg[, .(FG_num, ref_methods)], by = "FG_num", all.x = TRUE)
  } else {
    message("build_fg_references_sheet(): 'PB_QB_spp.csv' not found (or has no dispatch_group column)",
            " in ", pbqb_csv_dir, " - run 03_pbqb-traits.R with species_pb_qb passed to",
            " add_pbqb_to_ecopath_workbook() first. ref_methods left blank for now.")
  }
  if (!"ref_methods" %in% names(refs)) refs[, ref_methods := NA_character_]
  refs[is.na(ref_methods) & !is.na(ref_pbqb_traits) & grepl("EcoBase", ref_pbqb_traits),
       ref_methods := "EcoBase model repository (published literature P/B, Q/B)"]
  
  ## --- ref_diet: from 04_diets.R's own diet_references_by_fg.csv
  ## (per-FG study citations, rolled up from the metaweb's own
  ## Reference column via build_species_diet()/predator_references).
  diet_refs_path <- file.path(diet_csv_dir, "diet_references_by_fg.csv")
  if (file.exists(diet_refs_path)) {
    d <- fread(diet_refs_path)
    if (all(c("FG_num", "references") %in% names(d))) {
      refs <- merge(refs, d[, .(FG_num, ref_diet = references)], by = "FG_num", all.x = TRUE)
    } else {
      message("build_fg_references_sheet(): '", diet_refs_path, "' is missing FG_num/references column(s) -",
              " ref_diet left blank.")
    }
  } else {
    message("build_fg_references_sheet(): '", diet_refs_path, "' not found - run 04_diets.R against this",
            " workbook first. ref_diet left blank for now.")
  }
  if (!"ref_diet" %in% names(refs)) refs[, ref_diet := NA_character_]
  refs[!is.na(ref_diet) & ref_diet == "", ref_diet := NA_character_]  # fwrite()/fread() round-trips NA as "" for character columns by default - restore true NA rather than a blank string
  
  refs <- refs[, .(FG_num, FG_name, ref_B, ref_fisheries, ref_pbqb_traits, ref_diet, ref_methods)]
  setorder(refs, FG_num)
  upsert_workbook_sheets(list(FG_References = refs), out_path)
  n_populated <- refs[, sum(!is.na(ref_B) | !is.na(ref_fisheries) | !is.na(ref_pbqb_traits) | !is.na(ref_diet) | !is.na(ref_methods))]
  message("build_fg_references_sheet(): wrote 'FG_References' sheet - ", nrow(refs), " FG(s), ", n_populated,
          " with at least one reference filled in so far. Columns: ", paste(names(refs), collapse = ", "), ".")
  invisible(refs)
}

add_pbqb_to_ecopath_workbook <- function(fg_weighted, out_path, species_pb_qb = NULL,
                                         csv_out_dir = NULL, biomass_csv_dir = NULL) {
  ## csv_out_dir: directory for this function's OWN native CSV outputs
  ## (PB_QB/PB_QB_spp) - defaults to dirname(out_path) for back-compat.
  ## biomass_csv_dir: directory to read FG_lookup.csv from (a biomass-
  ## block output, read via read_full_fg_reference() below) - also
  ## defaults to dirname(out_path).
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
  
  ## Expand to EVERY FG in the model (this feeds Ecopath_PBQB,
  ## which goes straight into the EwE software - a row missing for an
  ## FG that just happens to have had no species data THIS run breaks a
  ## direct import, since EwE expects one row per functional group in
  ## the model, not just the ones with a computed value). A genuinely
  ## un-computed FG still gets a row here, with PB_FG/QB_FG left NA and
  ## flagged below - "no data yet" rather than "silently absent."
  full_fg_ref <- read_full_fg_reference(out_path, csv_dir = biomass_csv_dir)
  if (!is.null(full_fg_ref)) {
    n_before <- nrow(pbqb_sheet)
    pbqb_sheet <- merge(full_fg_ref, pbqb_sheet, by = "FG_num", all.x = TRUE, suffixes = c("_ref", ""))
    pbqb_sheet[is.na(FG_name), FG_name := FG_name_ref]
    pbqb_sheet[, FG_name_ref := NULL]
    n_added <- nrow(pbqb_sheet) - n_before
    if (n_added > 0) {
      message("add_pbqb_to_ecopath_workbook(): expanded PB_QB from ", n_before, " to ", nrow(pbqb_sheet),
              " row(s) using the full FG reference already in the workbook - ", n_added, " FG(s) had no",
              " species data this run and are included with blank PB_FG/QB_FG rather than omitted: ",
              paste(pbqb_sheet[is.na(PB_FG), FG_name], collapse = ", "))
    }
  } else {
    message("add_pbqb_to_ecopath_workbook(): no 'FG'/'FG_lookup' sheet found yet in ", out_path,
            " to expand against - PB_QB will only cover the FG(s) with species data in THIS run.",
            " Run 01_biomass.R against this same workbook first (it writes that reference sheet)",
            " to guarantee every model FG gets a row here, even ones with no data yet.")
  }
  setorder(pbqb_sheet, FG_num)
  
  ## 2026-09-17 update: the excel ecopath_ecosim file must have exactly
  ## the intended sheets; other sheets should be saved as csv files, not
  ## kept in the final output excel file. PB_QB (FG-level) IS one
  ## of the 9 final target sheets - written directly to the workbook as
  ## Ecopath_PBQB, plus a PB_QB.csv mirror for audit/back-compat naming
  ## and so other functions (e.g. export_ecopath_ecosim_excel()'s cross-
  ## run guard) can find it without opening the workbook. PB_QB_spp
  ## (species-level detail) is native/intermediate - CSV only, never a
  ## workbook sheet - built further below.
  out_dir <- if (is.null(csv_out_dir)) dirname(out_path) else csv_out_dir
  write_native_sheet_csv(pbqb_sheet, "PB_QB", out_dir)
  workbook_sheets <- list(Ecopath_PBQB = pbqb_sheet)
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
    write_native_sheet_csv(spp_sheet, "PB_QB_spp", out_dir)
  }
  
  upsert_workbook_sheets(workbook_sheets, out_path)
  invisible(sheets)
}