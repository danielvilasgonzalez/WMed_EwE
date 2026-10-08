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
## safe_fwrite() / csv_has_nul() - cloud-synced folder guard
## Files in the shared pCloud folder have repeatedly been found with
## runs of NUL (0x00) bytes where rows should be (FG_WMed_2026.csv tail;
## the EUSeaMap area cache lost its coral rows this way): the FUSE mount
## kept the file length but not the last block(s) written. safe_fwrite()
## removes the old file first (a fresh file, never an in-place
## overwrite), writes it single-threaded, then reads the bytes back and
## rewrites once if any NUL is found. csv_has_nul() lets readers treat
## a corrupted cache as missing (so it is recomputed, not half-read).
## =================================================================
csv_has_nul <- function(path) {
  if (!file.exists(path)) return(FALSE)
  sz <- file.info(path)$size
  if (is.na(sz) || sz == 0) return(FALSE)
  any(readBin(path, "raw", sz) == as.raw(0))
}
safe_fwrite <- function(x, file, ...) {
  for (attempt in 1:2) {
    if (file.exists(file)) unlink(file)
    data.table::fwrite(x, file, nThread = 1L, ...)
    if (!csv_has_nul(file)) return(invisible(file))
    message("[safe_fwrite] NUL bytes found in '", file, "' after writing (cloud-sync folder) - ",
            if (attempt == 1) "rewriting once." else "still corrupted; check the sync client / write it to a local folder.")
  }
  invisible(file)
}

## =================================================================
## compute_stanza_catch_split_from_length() - juvenile share of catch
## (landings, discards) for a multistanza species from MEDITS TC length
## frequencies, when no age-resolved catch data exist for the model area.
##  Age at length (inverse VBGF): t(L) = t0 - ln(1 - L/Linf) / K
##  Juvenile stanza: t < age_adult (hake: 24 months, L < 32.9 cm).
##  Discards = fish below the Minimum Conservation Reference Size
##   (MCRS), landings = fish >= MCRS (ASSUMPTION: full retention above
##   MCRS, survey length structure ~ commercial catch above MCRS).
##  Share by weight: p_juv = sum_{L in set, t<age_adult} n_L L^b /
##   sum_{L in set} n_L L^b  (W = a L^b; a cancels).
## Hake defaults: Linf 110 cm, K 0.178 /yr, t0 0 (Mellon-Duval et al.
## 2010, ICES J. Mar. Sci. 67:62-70); b 3.035 (GFCM SAF GSA 1/5/6);
## MCRS 20 cm TL (Council Regulation (EC) No 1967/2006, Annex III;
## Regulation (EU) 2019/1022, Western Mediterranean MAP).
## Returns rows shaped like the FDI age split: ScientificName,
## source_file ("... Landings ..." / "... Discards ..."), prop_juvenile.
## =================================================================
STANZA_GROWTH_PARAMS <- list(
  "Merluccius merluccius" = list(Linf = 110, K = 0.178, t0 = 0, b = 3.035, age_adult = 2, mcrs_cm = 20)
)
compute_stanza_catch_split_from_length <- function(multistanza_fg_pairs, tc_path, year_range = NULL, area_filter = NULL,
                                                   params = STANZA_GROWTH_PARAMS) {
  out <- data.table()
  if (!nrow(multistanza_fg_pairs) || !file.exists(tc_path)) {
    message("[Stanza catch split - length] TC.csv not found or no stanza pair - skipped.")
    return(out)
  }
  tc <- fread(tc_path, encoding = "UTF-8"); setnames(tc, tolower(gsub("\\s+", "", names(tc))))
  if (!all(c("genus", "species", "nblon", "length_class") %in% names(tc))) {
    message("[Stanza catch split - length] TC.csv lacks genus/species/nblon/length_class - skipped."); return(out)
  }
  for (sci in intersect(multistanza_fg_pairs$ScientificName, names(params))) {
    pr <- params[[sci]]; parts <- strsplit(sci, " ")[[1]]
    d <- tc[toupper(genus) == toupper(substr(parts[1], 1, 4)) & toupper(species) == toupper(substr(parts[2], 1, 3))]
    if (!is.null(area_filter) && "area" %in% names(d)) d <- d[area %in% area_filter]
    yrs_used <- "all years"
    if (!is.null(year_range) && "year" %in% names(d) && nrow(d[year %in% year_range])) { d <- d[year %in% year_range]; yrs_used <- paste(range(year_range), collapse = "-") }
    d[, `:=`(L = suppressWarnings(as.numeric(length_class)) / 10, n = suppressWarnings(as.numeric(nblon)))]   # length_class in mm
    d <- d[is.finite(L) & L > 0 & is.finite(n) & n > 0]
    if (!nrow(d)) next
    d[, age := fifelse(L < pr$Linf, pr$t0 - log(1 - L / pr$Linf) / pr$K, Inf)]
    d[, `:=`(w = n * L^pr$b, juv = age < pr$age_adult, landed = L >= pr$mcrs_cm)]
    p_land <- d[landed == TRUE, sum(w[juv]) / sum(w)]
    p_disc <- d[landed == FALSE, if (.N) sum(w[juv]) / sum(w) else NA_real_]
    L_adult <- pr$Linf * (1 - exp(-pr$K * (pr$age_adult - pr$t0)))
    out <- rbind(out, data.table(ScientificName = sci, Country = NA_character_, Year = NA_integer_,
                                 source_file = c("MEDITS TC length - Landings (>= MCRS)", "MEDITS TC length - Discards (< MCRS)"),
                                 prop_juvenile = c(p_land, p_disc)), fill = TRUE)
    message("[Stanza catch split - length] ", sci, " (MEDITS TC ", yrs_used, ", GSAs ", paste(area_filter, collapse = ","),
            "): juvenile (< ", pr$age_adult, " yr, L < ", round(L_adult, 1), " cm) share by weight = ",
            round(100 * p_land, 1), "% of landings (L >= MCRS ", pr$mcrs_cm, " cm), ", round(100 * p_disc, 1),
            "% of discards (L < MCRS).")
  }
  out
}

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
## resolve_pcloud_file() - pCloud data
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
## fetch_taxonomy_metaweb(): a THIRD taxonomy source,
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
  ## this pipeline - every one of the 939 species affected by it would
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
## Several systemic mismatches found by inspecting real fallback output,
## fixed here rather than by hand-patching individual species:
##   - Phylum DROPPED from the default rank_levels - almost always too
##     broad to safely stand in for one specific FG. Still usable if a
##     caller explicitly asks for it via rank_levels, but not tried by
##     default.
##   - allow_majority_vote defaults to FALSE: an ambiguous rank value
##     (already-assigned relatives split across 2+ FGs) is left
##     unresolved rather than guessed at the FG with the most relatives.
##     Set allow_majority_vote = TRUE to restore the old majority-vote
##     behavior.
##   - exclude_fg_regex (case-insensitive, matched against FG_name)
##     removes whole FGs as fallback TARGETS - taxonomy is never used to
##     assign an unmatched species INTO one of these FGs, no matter how
##     exclusive the rank match looks. Default covers three confirmed-bad
##     cases: "commercial" (e.g. "Non-commercial decapods" vs "Other
##     commercial decapods" - a name/commercial-status split, not a
##     taxonomic one; Squilla mantis is the real-world example, now
##     listed by name in FG_WMed_2026.csv so it resolves via the ordinary
##     direct scientific-name match before this fallback ever runs),
##     "jellyfish", and "suprabenthos"/"macrozooplankton" (ecologically-,
##     not taxonomically-, defined groups with no real rank of their own
##     - e.g. Suprabenthos is "small crustaceans living just above the
##     seabed", a habitat/size definition covering parts of
##     Isopoda/Amphipoda, not those orders whole - this exclusion blocks
##     the GENERIC genus/family/class fallback from roping in unrelated
##     relatives. A species that only this exclusion blocks from an
##     automatic match surfaces as genuinely unresolved (manual review),
##     same as everything else this fallback can't safely resolve on its
##     own.
##   - single_species_fg_broad_ranks: for these ranks (default Class,
##     Order), an FG that currently has only ONE species already
##     assigned is excluded as a fallback target (e.g. a FG that's really
##     just "the purple sea urchin", "red coral", or "mackerels" as one
##     specific species shouldn't absorb every other unmatched species
##     that happens to share its Class/Order; Genus/Family fallback into
##     a single-species FG is still allowed, since a shared genus/family
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
    ## Still set the SAME attributes a normal run sets (all empty, same
    ## column structure as the non-empty case below) rather than
    ## returning bare dt. Every caller treats these attributes as
    ## always-present, not conditional on whether a fetch actually
    ## happened - apply_seed_fg_rules() hard-requires 'fetched_taxonomy'
    ## and errors if it's missing, even though "everything already
    ## matched" (zero unmatched here) is a SUCCESS case.
    empty_taxonomy <- data.table(ScientificName = character(0), Genus = character(0), Family = character(0),
                                 Order = character(0), Class = character(0), Phylum = character(0))
    attr(dt, "fetched_taxonomy") <- empty_taxonomy
    attr(dt, "still_unresolved_taxonomy") <- empty_taxonomy
    attr(dt, "fallback_match_detail") <- data.table(ScientificName = character(0), FG_num = numeric(0), FG_name = character(0),
                                                    match_type = character(0), match_rank = character(0), vote_share = character(0))
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
  ## allow_majority_vote = TRUE (default FALSE - see this function's
  ## header comment).
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
    ## majority-vote-among-ambiguous-relatives is OFF by default
    ## (allow_majority_vote = FALSE) - an unmatched species whose rank
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
## build_fg_manual_review_template() - combines the two per-source
## "still unmatched, needs manual review" exports written above
## (survey_unmatched_for_manual_review.csv from 01_biomass.R's MEDITS
## pass, medias_unmatched_for_manual_review.csv from its MEDIAS pass)
## into ONE combined, de-duplicated review workbook, with GF/FG_name
## columns ready to hand-fill.
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
## build_fg_manual_review_template() so build_full_fg_species_catalog()
## (below) can reuse the exact same read/merge/de-duplicate logic for
## its own "needs review" tail, rather than a second, driftable copy of
## it. Returns NULL if neither unmatched CSV exists. Does NOT add
## ESPECIE/GF/FG_name - callers add those themselves, since the two
## callers want slightly different final column sets.
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
## snapshot_full_extent_species() - writes ONE row per distinct species
## a given source's FG-matching step actually saw - BEFORE that source's own
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
## build_full_fg_species_catalog() - (a) covers MEDITS + MEDIAS + stock
## assessment, (b) always reflects the whole available extent
## regardless of this run's FILTER_AREAS/YEAR_ECOPATH/TS_YEARS - not
## something that requires a special "widest FILTER_AREAS" run - and
## (c) writes a plain CSV instead of an .xlsx workbook.
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

## =================================================================
## remove_sample_outliers_haul() - MEDITS-style haul-level screening
## (project decision; default OUTLIER_METHOD = "haul").
##
##  - The observation judged is ONE species in ONE haul.
##  - It is compared with hauls of the SAME species in the SAME GSA, depth
##    stratum and year (MEDITS catches are strongly spatially structured,
##    so a deep-stratum haul is never judged against shallow ones, and a
##    good year is never judged against a poor one). If that cell has
##    fewer than min_samples (10) positive hauls, the comparison falls back to the same
##    species x GSA x year (all strata pooled); still too few -> not judged.
##  - Only unusually HIGH values are flagged. Low or zero catches are the
##    normal shape of trawl data, not errors (the previous boxplot rule
##    also removed low values, which is most of what was being dropped).
##  - Fence: log1p(density) > Q3 + k x IQR of the comparison group's
##    POSITIVE catches (zeros excluded), k = 3 (Tukey's "far out" fence).
##  - Repeated highs are kept: if 2 or more hauls of the same species in
##    the same GSA x year cross the fence, that is evidence of a real
##    aggregation/good year, not a recording error - none of them is
##    flagged.
##  - Never removes a whole year.
## =================================================================
remove_sample_outliers_haul <- function(dt, strata_def = NULL, k = 3, min_samples = 10,
                                        min_repeated_high = 2, drop_outliers = TRUE) {
  dt <- copy(dt)
  dt[, eval_value := log1p(Density)]
  has_depth <- "Depth" %in% names(dt) && !is.null(strata_def)
  if (has_depth) {
    sd <- as.data.table(strata_def)[order(depth_min)]
    ## findInterval on the lower bounds: the CSV's integer bounds (10-50,
    ## 51-100, ...) leave gaps like 50.5 m that a min/max test would miss.
    idx <- findInterval(dt$Depth, sd$depth_min)
    idx[is.na(dt$Depth) | idx == 0 | dt$Depth > max(sd$depth_max)] <- NA
    dt[, Stratum_qc := as.integer(sd$stratum_num[idx])]
  } else {
    dt[, Stratum_qc := NA_integer_]
    message("[Outliers] No Depth column / strata definition - comparing within species x GSA x year only.")
  }
  ## Fence built from POSITIVE catches only: with many zero hauls the
  ## quartiles collapse to 0 and every presence would look "high".
  ok <- !is.na(dt$eval_value) & !is.na(dt$ScientificName) & dt$Density > 0
  fence_stats <- function(by_cols) dt[ok, .(n_grp = .N,
                                            q3 = quantile(eval_value, 0.75, names = FALSE),
                                            iqr = IQR(eval_value)), by = by_cols]
  fine   <- fence_stats(c("ScientificName", "AreaID", "Stratum_qc", "Year"))
  coarse <- fence_stats(c("ScientificName", "AreaID", "Year"))
  setnames(coarse, c("n_grp", "q3", "iqr"), c("n_grp_c", "q3_c", "iqr_c"))
  dt <- merge(dt, fine, by = c("ScientificName", "AreaID", "Stratum_qc", "Year"), all.x = TRUE)
  dt <- merge(dt, coarse, by = c("ScientificName", "AreaID", "Year"), all.x = TRUE)
  dt[, use_fine := !is.na(n_grp) & n_grp >= min_samples & !is.na(Stratum_qc)]
  dt[, fence := fifelse(use_fine, q3 + k * iqr,
                        fifelse(!is.na(n_grp_c) & n_grp_c >= min_samples, q3_c + k * iqr_c, NA_real_))]
  dt[, comparison := fifelse(use_fine, "species x GSA x stratum x year",
                             fifelse(!is.na(fence), "species x GSA x year (stratum cell too small)", NA_character_))]
  dt[, above := !is.na(eval_value) & !is.na(fence) & eval_value > fence & Density > 0]
  dt[, n_above_gsa_year := sum(above), by = .(ScientificName, AreaID, Year)]
  dt[, is_outlier := above & n_above_gsa_year < min_repeated_high]
  n_kept_repeated <- dt[above & !is_outlier, .N]

  flagged <- dt[is_outlier == TRUE, .(ScientificName, AreaID, Year, Stratum = Stratum_qc, SampleID,
                                      flagged_density = Density, fence_density = expm1(fence), comparison)]
  setorder(flagged, ScientificName, AreaID, Year)
  flagged[, was_dropped := drop_outliers]
  n_obs <- sum(!is.na(dt$Density) & dt$Density > 0)
  message("\n[Outliers] Haul-level screening (species x haul vs same species x GSA x stratum x year; high values",
          " only; fence Q3 + ", k, " x IQR on log1p density): ", nrow(flagged), " of ", n_obs, " observation(s) flagged (",
          round(100 * nrow(flagged) / max(n_obs, 1), 2), "%). ", n_kept_repeated, " high observation(s) kept because ",
          min_repeated_high, "+ hauls of that species were high in the same GSA x year (real aggregation, not an error).",
          if (drop_outliers) " Flagged rows REMOVED." else " Report only (drop_outliers = FALSE).")
  if (nrow(flagged) > 0) {
    message("[Outliers] Flagged by species (top 15):")
    print(head(flagged[, .N, by = ScientificName][order(-N)], 15))
    message("[Outliers] Flagged by year:")
    print(flagged[, .N, by = Year][order(Year)])
    if (drop_outliers) dt[is_outlier == TRUE, `:=`(Density = NA_real_, Biomass = NA_real_)]
  }
  dt[, c("eval_value", "Stratum_qc", "n_grp", "q3", "iqr", "n_grp_c", "q3_c", "iqr_c", "use_fine",
         "fence", "comparison", "above", "n_above_gsa_year", "is_outlier") := NULL]
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
                                        cache_path = NULL, resolution = 1,
                                        area_id_label = "AreaID") {
  ## The output alone would carry only Stratum (a bare integer, 1:5 for
  ## MEDITS_STRATA) and a generic "AreaID" column, with no indication
  ## that AreaID actually means GSA in the Western Med run, and no real
  ## depth bounds to go with the stratum number. Two additive changes,
  ## same cache format either way
  ## (cache_valid below only checks for "area_km2", so an old cache file
  ## without Depth_min_m/Depth_max_m is still reused as-is - delete it to
  ## pick up the new columns on a re-run):
  ##   - area_id_label lets the caller rename the id column to "GSA"
  ##     when area_ids really are GSA numbers (AREA_MODE == "westmed", or
  ##     AREA_MODE == "custom" with CUSTOM_AREA_TYPE == "gsa") - left as
  ##     the generic "AreaID" (the default) for a bbox/shapefile custom
  ##     area, where the id is NOT a real GSA.
  ##   - strata_def's own Depth_min_m/Depth_max_m (already known for
  ##     every strata_def this function is ever called with - MEDITS_
  ##     STRATA, the AquaMaps-extended set, or MEDIAS_DEPTH_RANGE) are
  ##     merged in by Stratum, so the real meter bounds travel with the
  ##     row instead of just the bare stratum number.
  if (is.character(area_shp)) area_shp <- sf::st_read(area_shp, quiet = TRUE)
  
  cache_valid <- !is.null(cache_path) && file.exists(cache_path) &&
    "area_km2" %in% names(fread(cache_path, nrows = 1))
  if (cache_valid) {
    message("Loading cached strata areas from ", cache_path)
    cached <- fread(cache_path)
    ## The CSV is written with the id column renamed to area_id_label
    ## (e.g. "GSA"), but every caller merges on "AreaID" - rename it back,
    ## same as the freshly-computed object returned below.
    if (!"AreaID" %in% names(cached) && area_id_label %in% names(cached)) setnames(cached, area_id_label, "AreaID")
    if (!"AreaID" %in% names(cached)) stop("Cached strata-area file '", cache_path, "' has neither 'AreaID' nor '",
                                           area_id_label, "' - delete it so it is recomputed.")
    return(cached)
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
  
  strata_depth_bounds <- unique(as.data.table(strata_def)[, .(Stratum = stratum_num,
                                                              Depth_min_m = depth_min,
                                                              Depth_max_m = depth_max)])
  strata_area_by_area <- merge(strata_area_by_area, strata_depth_bounds, by = "Stratum", all.x = TRUE)
  setcolorder(strata_area_by_area, c("Stratum", "Depth_min_m", "Depth_max_m",
                                     setdiff(names(strata_area_by_area), c("Stratum", "Depth_min_m", "Depth_max_m"))))
  ## Written to disk under area_id_label (e.g. "GSA") when requested, but
  ## the OBJECT RETURNED keeps the plain "AreaID" name regardless - every
  ## downstream merge in this library (weight_by_strata(), weight_species_
  ## by_area(), etc.) still looks for "AreaID" and would break silently
  ## otherwise. Only the on-disk CSV's column header changes.
  if (!is.null(cache_path)) {
    on_disk <- copy(strata_area_by_area)
    if (!identical(area_id_label, "AreaID")) setnames(on_disk, "AreaID", area_id_label)
    safe_fwrite(on_disk, cache_path)
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
  
  ## When a study area is selected, zoom the map to it (as in
  ## westmed_gsa_map.png) instead of every GSA + every MEDITS haul across
  ## the whole Mediterranean and Black Sea.
  if (!is.null(selected_areas) && any(area_shp[[area_id_col]] %in% selected_areas)) {
    sel_bbox <- st_bbox(area_shp[area_shp[[area_id_col]] %in% selected_areas, ])
    display_bbox <- c(xmin = unname(sel_bbox["xmin"]) - 1, xmax = unname(sel_bbox["xmax"]) + 1,
                      ymin = unname(sel_bbox["ymin"]) - 0.5, ymax = unname(sel_bbox["ymax"]) + 0.5)
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
      ## Same method as scripts/additional/fig_WMed_basemap.R (whose
      ## westmed_gsa_map.png has no seam line): the stray line is a real
      ## hairline GAP between neighbouring GFCM GSA polygons, not a
      ## rasterisation artifact (the old rasterize approach left it in).
      ## Measure the widest gap under 0.05 deg between selected GSAs,
      ## buffer out and back in by just over half of it (closes the gap
      ## without coarsening the coastline), repair, keep the largest piece.
      s2_was_on <- sf::sf_use_s2()
      selected_outline <- tryCatch({
        sf::sf_use_s2(FALSE)
        planar <- sf::st_set_crs(sf::st_geometry(selected_shp), NA)
        gaps <- numeric(0)
        if (length(planar) > 1) for (i in seq_len(length(planar) - 1)) {
          d <- suppressWarnings(as.numeric(sf::st_distance(planar[i], planar[(i + 1):length(planar)])))
          gaps <- c(gaps, d[d > 0 & d < 0.05])
        }
        gap_close <- if (length(gaps) > 0) max(gaps) / 2 * 1.2 else 0.001
        u <- sf::st_union(sf::st_geometry(selected_shp))
        u <- sf::st_buffer(sf::st_buffer(u, gap_close), -gap_close)
        u <- sf::st_make_valid(u)
        parts <- suppressWarnings(sf::st_cast(sf::st_cast(u, "MULTIPOLYGON"), "POLYGON"))
        parts <- parts[which.max(as.numeric(sf::st_area(sf::st_set_crs(parts, NA))))]
        sf::st_boundary(parts)
      }, error = function(e) {
        message("Gap-closing contour cleanup failed (", conditionMessage(e), ") - falling back",
                " to plain st_union(), which may still show a seam line.")
        st_union(selected_shp)
      }, finally = sf::sf_use_s2(s2_was_on))
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
                                        y_lab = "Area-weighted density", top_n_fg = 40,
                                        normalize = FALSE) {
  fg_totals <- fg_index_regional[, .(tot = sum(mean_density, na.rm = TRUE)), by = FG_name]
  top_fg <- fg_totals[order(-tot)][seq_len(min(top_n_fg, .N)), FG_name]
  plot_data <- as_tibble(fg_index_regional[FG_name %in% top_fg])
  
  ## normalize = TRUE rescales each FG's own series to its first
  ## NON-ZERO value (that value becomes 1, every other year relative to
  ## it) - same
  ## "first value = reference index" idea export_ecopath_ecosim_excel()'s
  ## normalize_ts already uses for the Ecosim sheet, just applied here
  ## directly to fg_index_regional (not ts_years-gapped/zero-filled) so
  ## the raw survey time series itself can be read as relative change
  ## rather than absolute density. An FG with every value zero/NA is
  ## left as-is (can't normalize to nothing) and will show as a flat
  ## line at 0/NA, same as it would un-normalized.
  if (normalize) {
    setDT(plot_data)
    setorder(plot_data, FG_name, Year)
    plot_data[, mean_density := {
      first_nonzero <- mean_density[which(!is.na(mean_density) & mean_density != 0)[1]]
      if (is.na(first_nonzero) || length(first_nonzero) == 0) mean_density else mean_density / first_nonzero
    }, by = FG_name]
    plot_data <- as_tibble(plot_data)
    y_lab <- paste0(y_lab, " (relative to first non-zero value = 1)")
  }
  
  ## Facet labels (FG_name) get cut off against each other / the panel
  ## edge for long FG names - wrapped at ~22 characters instead of left
  ## as one unbroken line, using label_wrap_gen() ggplot2 already ships
  ## for exactly this.
  ggplot(plot_data, aes(x = Year, y = mean_density)) +
    geom_line(linewidth = 0.6, alpha = 0.8, colour = "steelblue") +
    facet_wrap(vars(FG_name), scales = "free_y", labeller = label_wrap_gen(width = 22)) +
    labs(title = title, x = "Year", y = y_lab) +
    theme_minimal(base_size = 11) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1),
          strip.text = element_text(size = 7))
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
  ## Rounded to whole meters for the x-axis label - the underlying
  ## depth_min/depth_max can carry long floating-point tails (e.g. the
  ## AquaMaps shallow pseudo-stratum's depth_max = 10 - 0.0001 =
  ## 9.9999...) that are meaningless clutter on an axis label at any
  ## precision finer than whole meters.
  profile[, depth_label := fifelse(
    is.na(depth_min) | is.na(depth_max), "unknown depth range",
    paste0(formatC(depth_min, format = "f", digits = 0), "-", formatC(depth_max, format = "f", digits = 0), "m"))]
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
         subtitle = "Grey box-plots: all hauls of the species (log1p density). Red triangles: flagged hauls - judged against the same species x GSA x stratum x year, so a red point can sit inside the all-years box.",
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
## Notes:
##   1. The deepest AquaMaps pseudo-strata (800-1000m/1000-2850m) could
##      go missing from the donut if `props` were derived from
##      fg_index_stratified's own "per_stratum" attribute
##      (weight_by_strata()'s output), which is FG/catch-conditioned -
##      an AreaID x Stratum cell only survives into that attribute if
##      at least one FG actually had a sample there. A real, computed
##      area proportion for a deep stratum with few/no catches (exactly
##      what the deepest strata are) could therefore be silently absent
##      from the donut even though strata_area_by_area/its AquaMaps-
##      extended version has a perfectly good area% for it. Avoided by
##      taking strata_area_by_area (or its AquaMaps-extended
##      equivalent) DIRECTLY as a required argument - it already
##      carries Stratum/area_km2/prop/AreaID for every stratum
##      regardless of whether anything was ever caught there, which is
##      what this plot is actually about (area weight, not catch
##      presence).
##   2. facet labels say "GSA: <n>" instead of "AreaID: <n>" when
##      area_id_label = "GSA" (the caller already knows whether AreaID
##      really is a GSA - same area_id_label convention
##      compute_strata_area_by_area() uses).
plot_area_weight_donut <- function(strata_area_by_area, strata_def, area_ids = NULL, area_id_label = "AreaID") {
  props <- unique(strata_area_by_area[, .(AreaID, Stratum, prop)])
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
  setnames(props, "AreaID", area_id_label)
  
  ggplot(props, aes(ymax = ymax, ymin = ymin, xmax = 4, xmin = 3, fill = depth_label)) +
    geom_rect() +
    coord_polar(theta = "y") +
    xlim(c(2, 4)) +
    facet_wrap(vars(!!rlang::sym(area_id_label)), labeller = label_both) +
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
## The excel ecopath_ecosim file must have exactly the intended sheets,
## trimmed script by script - every other table is saved as a csv
## file, not kept in the final output excel file. Every table that
## USED TO go into the
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
    ## FG_References: see build_fg_references_sheet() below. Ecobase: see
    ## add_ecobase_sheet_to_workbook() in 03b_ecobase.R (per-group
    ## Biomass/PB/QB/reference-year from other published Western Med
    ## Ecopath models, for direct comparison against this model's own
    ## Ecopath_B/Ecopath_PBQB values). 11 final sheets total.
    target_order = c("info", "FG_spp", "Ecopath_B", "Ecopath_L", "Ecopath_Di", "Ecopath_PBQB",
                     "Ecopath_traits", "Ecopath_diet", "Ecosim_ts", "FG_References", "Ecobase"),
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
## slip by unnoticed. Pass drop_extras = TRUE for a workbook containing
## ONLY the final summary
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
  ## names(wb) keeps openxlsx's internal creation order even after
  ## worksheetOrder<- (the saved file IS reordered) - report the order
  ## actually written, not names(wb).
  message("finalize_workbook_sheet_order(): saved '", out_path, "' - final sheet order: ",
          paste(final_order_names, collapse = ", "))
  invisible(wb)
}

## =================================================================
## finalize_ecopath_ecosim_summary_sheets()
##
## The excel file keeps only: Ecopath_B, Ecopath_L, Ecopath_Di,
## Ecopath_PBQB, Ecopath_traits, Ecosim_ts (with B, L, Di and effort) -
## everything else goes to csv/intermediate files. Ecopath_traits
## is written directly by 03_pbqb-traits.R, Ecopath_B directly by
## export_ecopath_ecosim_excel() (01_biomass.R), and Ecopath_PBQB
## directly by add_pbqb_to_ecopath_workbook() (03_pbqb-traits.R) - none
## of those three need building here. This function builds the
## remaining two, Ecopath_L/Ecopath_Di and Ecosim_ts, by reading the
## native/intermediate CSVs 01_biomass.R/02_fisheries.R write via
## write_native_sheets_csv() - other sheets are saved as csv files,
## not kept in the final output excel file; the source data is
## identical to what the old native WORKBOOK sheets of the same name
## held, just off disk now instead of out of a sheet that no longer
## exists in the workbook.
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
  ## shape, so it's no longer used for them.
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
  ## Native Ecosim-format CSVs carry a column-id row (" ,fg_1,fg_2,...")
  ## ABOVE the "Name" meta row. When fread reads that row as data rather
  ## than as a header, every position-based "drop the 8 meta rows" step
  ## below is off by one, and the "Interval" label lands in the Year
  ## vector as NA - which made every effort fleet fall back to absolute
  ## kW-days. Drop anything above the "Name" row so positions line up.
  .strip_pre_meta <- function(dt) {
    if (is.null(dt) || nrow(dt) == 0) return(dt)
    idx <- match("Name", trimws(as.character(dt[[1]])))
    if (is.na(idx)) stop("Ecosim-format sheet has no 'Name' meta row in its first column - unexpected layout.")
    if (idx > 1) dt <- dt[-seq_len(idx - 1)]
    dt
  }
  ecosim_b <- .strip_pre_meta(ecosim_b)
  ecosim_l <- .strip_pre_meta(ecosim_l)
  
  if (!is.null(ecosim_b)) {
    years <- suppressWarnings(as.numeric(ecosim_b[[1]][-seq_along(meta_labels)]))
    combined <- data.table(` ` = c(meta_labels, as.character(years)))
    
    add_block <- function(sheet_dt, new_prefix) {
      if (is.null(sheet_dt)) return(invisible(NULL))
      ## Matched against the reference `years` vector (from ecosim_b) BY
      ## YEAR VALUE, not assumed to line up positionally - Catches_Ecosim.csv
      ## (from 02_fisheries.R, new_prefix "L") can genuinely cover a
      ## different year range/length than Ecosim.csv (01_biomass.R's own
      ## TS_YEARS: this script has no guarantee the two ever match, and
      ## nothing forces them to), and a straight c(meta, yrvals) positional
      ## assignment silently assumed they always did - which is exactly
      ## what crashed here ("Supplied 34 items to be assigned to 38 items"
      ## the moment the two sheets' year counts differed). Missing years
      ## are filled with NA instead, the same convention the Discards/
      ## Effort blocks below already use for exactly this reason.
      sheet_years <- suppressWarnings(as.numeric(sheet_dt[[1]][-seq_along(meta_labels)]))
      value_cols <- setdiff(names(sheet_dt), names(sheet_dt)[1])
      for (col in value_cols) {
        vals <- sheet_dt[[col]]
        meta          <- vals[seq_along(meta_labels)]   # keep the source sheet's own descriptive "Name" row as-is (it's already e.g. "B_Hake"/"C_Hake") - only the combined data.table's own COLUMN KEY gets prefixed below, so B_/L_/Di_/Effort_ columns pulled from different source sheets never collide once combined into one sheet
        sheet_yrvals  <- vals[-seq_along(meta_labels)]
        yrvals <- vapply(years, function(y) {
          idx <- which(sheet_years == y)
          if (length(idx) == 0) NA_character_ else as.character(sheet_yrvals[idx[1]])
        }, character(1))
        ## No biomass index / no catch recorded is not an observed zero:
        ## leave it blank so Ecosim does not fit to 0.
        yrvals[!is.na(suppressWarnings(as.numeric(yrvals))) & suppressWarnings(as.numeric(yrvals)) == 0] <- NA_character_
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
        ts_vals[!is.na(ts_vals) & ts_vals == 0] <- NA_real_   # no recorded discards -> blank, not an observed 0
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
          ## Shipping raw FishMIP kW-days as Type="Effort"/Scaling="absolute"
          ## would be wrong here - Ecosim doesn't use the Effort
          ## driver as a physical quantity, it uses it as a MULTIPLIER
          ## on the Ecopath base year's own fishing mortality (F), which
          ## is itself derived here from Catch/Biomass, not from any
          ## catchability coefficient linking kW-days to F. Feeding it
          ## raw absolute kW-days only works if effort in the Ecopath
          ## base year is independently calibrated to reproduce F_base -
          ## nothing in this pipeline does that calibration, so
          ## "absolute" here was liable to silently misscale F for every
          ## other year. Rescaled to a first-valid-year=1 index instead -
          ## same convention build_ts_column() already uses for Biomass
          ## above (Scaling="relative") - so the fleet's effort MULTIPLIER
          ## on baseline F is correct regardless of what physical units
          ## the underlying FishMIP figure is in.
          first_valid_idx <- which(!is.na(ts_vals))[1]
          ## `years` (parsed via suppressWarnings(as.numeric(ecosim_b[[1]][...])))
          ## up above can itself contain NA - e.g. a blank/non-numeric cell in
          ## Ecosim.csv's own Year column for one row - and `years[1]`
          ## specifically being NA is enough to make ANY comparison against
          ## it evaluate to NA (real_number != NA is NA, not FALSE), which
          ## `if()` cannot evaluate ("Error in if (years[first_valid_idx] !=
          ## years[1]) { : missing value where TRUE/FALSE needed"). This is a
          ## genuinely different failure mode than "no non-NA effort values"
          ## (already guarded by the is.na(first_valid_idx) branch above) -
          ## the effort column itself can be perfectly fine while the `years`
          ## reference vector has the hole. Guarded every years[...]
          ## comparison below with explicit is.na() checks instead of
          ## relying on `!=` to short-circuit safely (it does not, for NA).
          if (is.na(first_valid_idx)) {
            message("Effort fleet '", fl, "': no non-NA effort values across the whole time series - column left blank.")
            ts_vals_rel <- ts_vals
          } else if (any(is.na(years))) {
            message("Effort fleet '", fl, "': the reference 'years' vector (from Ecosim.csv's own Year column) has ",
                    sum(is.na(years)), " NA/unparseable entry(ies) - cannot safely determine whether the relative-",
                    "scaling reference year matches the series' first year. Check Ecosim.csv for a blank or non-",
                    "numeric Year cell. Falling back to raw kW-days for this fleet only (Scaling stays 'absolute').")
            ts_vals_rel <- ts_vals
          } else {
            ref_value <- ts_vals[first_valid_idx]
            if (years[first_valid_idx] != years[1]) {
              message("Effort fleet '", fl, "': relative-scaling reference is year ", years[first_valid_idx],
                      " (first year WITH data), not the series' first year ", years[1], ".")
            }
            if (is.na(ref_value) || ref_value == 0) {
              message("WARNING: Effort fleet '", fl, "'s reference value (year ", years[first_valid_idx], ") is ",
                      ifelse(is.na(ref_value), "NA", "zero"), " - cannot rescale to a relative index,",
                      " leaving this column as raw kW-days instead (Scaling stays 'absolute' for this fleet only).")
              ts_vals_rel <- ts_vals
            } else {
              ts_vals_rel <- ts_vals / ref_value
            }
          }
          is_relative <- !is.na(first_valid_idx) && !(is.na(ts_vals[first_valid_idx]) || ts_vals[first_valid_idx] == 0)
          col <- c(paste0("Effort_", gsub("[^A-Za-z0-9]+", "", fl)),
                   if (is_relative) "Effort (relative)" else "Effort", "reference",
                   if (is_relative) "relative" else "absolute",
                   "1", fl, "", "Annual", as.character(ts_vals_rel))
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
## build_info_sheet() - the final workbook leads with an "info" sheet
## (data from the run: GSAs, time ecopath, time ecosim, region). A plain Field/Value
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
## sheet_name (e.g. "Ecosim") is a native/intermediate table written
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

## =================================================================
## resolve_baseline_with_nearest_year_fallback() - bottom-trawl survey
## groups (fish, cephalopods, benthos,
## corals, invertebrates - i.e. everything MEDITS/MEDIAS actually
## samples) can show a real ZERO or NA density in the 1994-1996
## Ecopath baseline years purely from survey catchability/rarity
## (patchy schools, low encounter probability at the shallow/deep
## edges of the sampled band) - NOT because the group was genuinely
## absent from the West Med at that time. This is the case
## handled here, distinct from a genuinely
## range-expanding/colonizing group (FG_name containing "Expanding"),
## where a real baseline zero/near-zero IS the correct signal (the
## group hadn't established yet) and must NOT be papered over with a
## later year's value.
##
## For every group (id_cols - MUST include a FG_name-like column,
## named by fg_name_col, for the exclusion check), if the group's
## baseline_years average is NA or exactly 0 AND its FG_name does not
## match exclude_pattern (case-insensitive), this looks across the
## group's FULL available time series in dt (every Year present, not
## just baseline_years) for the SINGLE closest year (by
## |Year - round(mean(baseline_years))|, ties broken toward the
## earlier year) that has a real (>0) observed value, and substitutes
## THAT one year's value as the baseline estimate - flagged via the
## returned borrowed/borrowed_from_year columns, never silently
## blended into an average. A group matching exclude_pattern, or with
## no positive value anywhere in its own time series, is returned
## completely untouched (final_value stays NA/0, borrowed = FALSE).
##
## dt must have: Year, value_col, and every column in id_cols. Returns
## one row per id_cols group: id_cols, final_value, borrowed (logical),
## borrowed_from_year (NA unless borrowed).
## =================================================================
resolve_baseline_with_nearest_year_fallback <- function(dt, id_cols, value_col = "mean_density",
                                                        fg_name_col = "FG_name",
                                                        baseline_years,
                                                        exclude_pattern = "Expanding") {
  if (!fg_name_col %in% id_cols) {
    stop("resolve_baseline_with_nearest_year_fallback(): fg_name_col ('", fg_name_col,
         "') must be one of id_cols so the 'Expanding' exclusion can be checked - got id_cols = ",
         paste(id_cols, collapse = ", "))
  }
  dt <- copy(dt)
  baseline_mid <- round(mean(baseline_years))
  
  ## each group's baseline value exactly as computed today, no fallback yet.
  ## mean(x, na.rm=TRUE) over a vector that is ENTIRELY NA (every baseline
  ## year genuinely missing - e.g. a survey_exempt FG with no stock-
  ## assessment/EcoBase/manual source at all) returns NaN, not NA, because
  ## na.rm=TRUE strips the NAs first and then takes mean(numeric(0)) = NaN.
  ## is.na(NaN) is TRUE in R, so the borrowed-value logic below still works
  ## correctly (NaN is treated as missing when deciding whether to borrow a
  ## nearby year), but whenever there is ALSO no other-year value to borrow
  ## (nearest_value stays NA, borrowed = FALSE), final_value would fall back
  ## to the untouched baseline_value - which is NaN, not NA. openxlsx writes
  ## that literal NaN into the workbook as Excel's #NUM! error rather than
  ## leaving the cell blank. Guarded here so an all-NA group's baseline_value
  ## is a clean NA_real_ instead - a genuinely missing figure should render
  ## as an empty cell, never as a spreadsheet ERROR that looks like
  ## something crashed.
  baseline_dt <- dt[Year %in% baseline_years,
                    .(baseline_value = {
                      v <- get(value_col)
                      if (all(is.na(v))) NA_real_ else mean(v, na.rm = TRUE)
                    }), by = id_cols]
  
  ## each group's own closest OTHER-year real (>0) observation, if any
  candidates <- dt[!(Year %in% baseline_years) & !is.na(get(value_col)) & get(value_col) > 0]
  if (nrow(candidates) > 0) {
    candidates[, .dist := abs(Year - baseline_mid)]
    setorder(candidates, .dist, Year)
    nearest <- candidates[, .SD[1], by = id_cols][, c(id_cols, "Year", value_col), with = FALSE]
    setnames(nearest, c("Year", value_col), c("borrowed_from_year", "nearest_value"))
  } else {
    nearest <- unique(dt[, id_cols, with = FALSE])
    nearest[, `:=`(borrowed_from_year = NA_integer_, nearest_value = NA_real_)]
  }
  
  out <- merge(baseline_dt, nearest, by = id_cols, all.x = TRUE)
  out[, .is_expanding := grepl(exclude_pattern, get(fg_name_col), ignore.case = TRUE)]
  out[, borrowed := (is.na(baseline_value) | baseline_value == 0) & !.is_expanding & !is.na(nearest_value)]
  out[, final_value := fifelse(borrowed, nearest_value, baseline_value)]
  out[borrowed == FALSE, borrowed_from_year := NA_integer_]
  out[, c(id_cols, "final_value", "borrowed", "borrowed_from_year"), with = FALSE]
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
                                        no_zero_fill_fg = integer(0),  ## FGs whose Ecosim gaps stay BLANK (not 0) in survey years - non-survey sources (model, literature, stock assessment) and recording-era FGs
                                        estimate_b_from_ee = character(0),  ## FG names whose Ecopath B is left blank for Ecopath to estimate from EE (poorly sampled groups)
                                        ee_value = 0.95,                    ## EE given to those FGs (EE_input column of Ecopath_B)

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
  ## Same nearest-year borrow fallback as Ecopath_B below,
  ## applied here at species level for consistency (see that block's
  ## comment for the full reasoning; skipped for "Expanding" FGs).
  species_baseline_fallback <- resolve_baseline_with_nearest_year_fallback(
    species_density_regional[, .(Year, FG_num, FG_name, Species = ScientificName, mean_density)],
    id_cols = c("FG_num", "FG_name", "Species"), value_col = "mean_density",
    baseline_years = year_ecopath)
  density_in_base_years <- species_baseline_fallback[, .(FG_num, FG_name, Species, Density = final_value)]
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
  
  ## An FG with a zero/no-data year_ecopath
  ## baseline but a real (>0) density in some OTHER survey year is very
  ## likely a trawl-catchability/rarity artifact, not a true absence -
  ## see resolve_baseline_with_nearest_year_fallback()'s own header for
  ## the full reasoning. Borrows the nearest such year's value instead
  ## of reporting a false zero for every FG EXCEPT one whose FG_name
  ## contains "Expanding" (a genuinely range-expanding/colonizing group,
  ## where a real baseline zero is the correct signal and must stay).
  fg_baseline_fallback <- resolve_baseline_with_nearest_year_fallback(
    fg_index_regional, id_cols = c("FG_num", "FG_name"), value_col = "mean_density",
    baseline_years = year_ecopath)
  n_fg_borrowed <- sum(fg_baseline_fallback$borrowed, na.rm = TRUE)
  if (n_fg_borrowed > 0) {
    fwrite(fg_baseline_fallback[borrowed == TRUE], file.path(out_dir, "ecopath_B_baseline_year_borrowed_REVIEW.csv"))
    message(n_fg_borrowed, " FG(s) had a zero/no-data ", min(year_ecopath), "-", max(year_ecopath),
            " biomass baseline but a real nonzero density in another survey year - borrowed the",
            " nearest such year's value for Ecopath_B instead of reporting a false zero (skipped for",
            " any FG whose name contains \"Expanding\"). See ecopath_B_baseline_year_borrowed_REVIEW.csv.")
  }
  ecopath_avg <- fg_baseline_fallback[, .(FG_num, Biomass_avg = final_value)]
  
  ecopath_detail <- merge(full_fg_list, ecopath_base, by = "FG_num", all.x = TRUE)
  ecopath_detail <- merge(ecopath_detail, ecopath_avg, by = "FG_num", all.x = TRUE)
  setnames(ecopath_detail, c("Biomass_baseyear", "Biomass_avg"),
           c(paste0("Biomass_", base_year), paste0("Biomass_", min(year_ecopath), "_", max(year_ecopath))))
  ## Biomass in the first Ecosim year (ts_years[1], normally 1995), so the
  ## Ecopath baseline (year_ecopath mean) can be compared with, or
  ## replaced by, the Ecosim starting-year value.
  ecosim_start_year <- if (!is.null(ts_years)) min(ts_years) else min(fg_index_regional$Year, na.rm = TRUE)
  col_start <- paste0("Biomass_", ecosim_start_year)
  if (!col_start %in% names(ecopath_detail)) {
    ecopath_detail <- merge(ecopath_detail,
                            fg_index_regional[Year == ecosim_start_year, .(FG_num, tmp_start = mean_density)],
                            by = "FG_num", all.x = TRUE)
    setnames(ecopath_detail, "tmp_start", col_start)
  }
  setorder(ecopath_detail, FG_num)
  write_native_sheet_csv(ecopath_detail, "biomass_by_fg_ecopath_detail", out_dir)
  
  ## Ecopath_B: Biomass = year_ecopath mean (the value Ecopath uses, and the
  ## column every other script reads), plus the same value under its
  ## explicit name and the Ecosim starting-year value, side by side.
  col_avg <- paste0("Biomass_", min(year_ecopath), "_", max(year_ecopath))
  ecopath_sheet <- ecopath_detail[, c("FG_num", "FG_name", col_avg, col_start), with = FALSE]
  ecopath_sheet[, Biomass := get(col_avg)]
  setcolorder(ecopath_sheet, c("FG_num", "FG_name", "Biomass", col_avg, col_start))
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
    ## Aggregated to exactly one row per Year here, not
    ## a plain column selection - fg_index_regional is grouped by
    ## .(Year, FG_num, FG_name), so if the SAME FG_num carries more than
    ## one literal FG_name string across its source rows (whitespace,
    ## a stale vs. current spelling, anything upstream not yet fully
    ## reconciled to one canonical FG_name per FG_num), filtering on
    ## FG_num alone still lets every one of those FG_name variants
    ## through as its OWN row for the same Year. The merge below (all
    ## Years x all matching rows) then multiplies rows further - this is
    ## exactly what produced "Supplied 124 items to be assigned to 37
    ## items of column 'fg_22'": more (Year, mean_density) rows existed
    ## for FG 22 than there are actual years in ts_years. Averaging by
    ## Year here makes the column length depend only on ts_years, never
    ## on how many FG_name variants happen to exist upstream for this FG_num.
    vals <- fg_index_regional[FG_num == fg_num, .(Year, mean_density)]
    n_dupe_years <- vals[, .N, by = Year][N > 1, .N]
    if (n_dupe_years > 0) {
      message("FG ", fg_num, ": ", n_dupe_years, " year(s) had more than one mean_density row",
              " (likely more than one FG_name variant sharing this FG_num upstream) - averaged",
              " down to one value per year rather than left as-is (would otherwise misalign the",
              " Ecosim sheet's fixed-length year column, or crash the column assignment).")
    }
    ## Same NaN-vs-NA guard as resolve_baseline_with_nearest_year_fallback()
    ## above - a Year where every
    ## contributing row is NA must average to NA_real_, not NaN, or this
    ## Ecosim_ts cell renders as Excel's #NUM! error instead of blank.
    vals <- vals[, .(mean_density = {
      v <- mean_density
      if (all(is.na(v))) NA_real_ else mean(v, na.rm = TRUE)
    }), by = Year]
    full_years <- data.table(Year = ts_years)
    vals <- merge(full_years, vals, by = "Year", all.x = TRUE)
    ## A survey year without a catch is a real zero only for survey-sourced
    ## FGs; for model/literature/stock-assessment FGs a missing year is
    ## simply missing (blank in Ecosim_ts), never 0.
    ## Ecosim fitting reads a 0 as an observed zero biomass. A survey year in
    ## which an FG was simply not caught is NOT an observation of zero, so
    ## both missing years and zero densities stay blank (NA) for every FG
    ## (changed 2026-10-07; previously survey years without a catch were
    ## filled with 0 for survey-sourced FGs).
    vals[!is.na(mean_density) & mean_density == 0, mean_density := NA_real_]
    vals <- vals[order(Year)]
    
    if (!normalize_ts) return(vals$mean_density)
    
    ## Force the series' first value
    ## (ts_years[1], normally 1995) to equal the Ecopath_B baseline for
    ## this FG (the year_ecopath, e.g. 1994-1996, average already
    ## computed above as ecopath_sheet$Biomass) BEFORE normalizing - so
    ## the normalized Ecosim series and the separate Ecopath_B baseline
    ## agree at their shared starting point, instead of the raw survey
    ## density at ts_years[1] (which can differ slightly from the
    ## 1994-1996 average) silently becoming the de facto reference value.
    ecopath_fg_biomass <- ecopath_sheet[FG_num == fg_num, Biomass]
    if (length(ecopath_fg_biomass) == 1 && !is.na(ecopath_fg_biomass)) {
      first_year_idx <- which(vals$Year == ts_years[1])
      if (length(first_year_idx) == 1) vals$mean_density[first_year_idx] <- ecopath_fg_biomass
    }
    
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
  
  ## The excel ecopath_ecosim file must have exactly
  ## the intended sheets, trimmed script by script - every other table
  ## is saved as a csv file, not kept in the final output excel
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
  ## Poorly sampled FGs (estimate_b_from_ee): Biomass blank and EE_input
  ## given, so Ecopath estimates B from the predation and catch on the
  ## group (Christensen & Walters 2004, Ecol. Model. 172:109-139). The
  ## survey/literature value stays visible in the Biomass_<years> column
  ## as a reference only, and the Ecosim relative index above was already
  ## scaled with it. biomass_by_fg_ecopath_detail.csv gets a flag column
  ## so 02_fisheries.R does not compute F from that reference value.
  ecopath_sheet[, EE_input := NA_real_]
  ee_hit <- ecopath_sheet$FG_name %in% estimate_b_from_ee
  if (any(ee_hit)) {
    ecopath_sheet[ee_hit, `:=`(Biomass = NA_real_, EE_input = ee_value)]
    message("[Ecopath_B] B left for Ecopath to estimate (EE_input = ", ee_value, ") for: ",
            paste(ecopath_sheet[ee_hit, FG_name], collapse = ", "), ".")
  }
  ecopath_detail[, B_estimated_by_Ecopath := FG_name %in% estimate_b_from_ee]
  write_native_sheet_csv(ecopath_detail, "biomass_by_fg_ecopath_detail", out_dir)
  sheets_to_write <- list(Ecopath_B = ecopath_sheet)
  
  ## PB_QB_spp.csv belongs to 03_pbqb-traits.R exclusively
  ## (add_pbqb_to_ecopath_workbook(), which writes it with REAL PB/QB
  ## values into out_dir/pbqb-traits), same as traits_ewe.csv (see that
  ## script's own comment on traits_ewe for the parallel case). No
  ## PB_QB_spp SCAFFOLD (species/FG_num/Biomass/prop_biomass_FG, PB/QB
  ## left blank) is written here into 01_biomass.R's own "biomass" output
  ## folder - PB_QB_spp.csv only ever exists after 03_pbqb-traits.R has
  ## actually run, with real PB/QB in it from the start, never a blank
  ## placeholder version living in a different script's folder first.
  
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
    ## Same "Supplied N items to be assigned to M items" failure mode as
    ## build_ts_column() in fallback_match_fg_by_taxonomy()'s Ecosim-sheet
    ## path: if fg_catch has more than one row for the
    ## same (Year, FG_num) - e.g. it wasn't pre-aggregated across sources/
    ## fleets before being passed in here - merging against full_years
    ## (exactly one row per Year) lets the duplicates through, producing
    ## more rows than length(ts_years) and crashing the := column
    ## assignment below with a length mismatch. Aggregate to one row per
    ## Year first (summed - catches from multiple sources/fleets for the
    ## same FG and year are genuinely additive, unlike a density mean).
    n_before_agg <- nrow(vals)
    vals <- vals[, .(Catch_t_km2 = sum(Catch_t_km2, na.rm = TRUE)), by = Year]
    if (nrow(vals) < n_before_agg) {
      warning("build_catch_ts_column(): FG ", fg_num, " had ", n_before_agg,
              " (Year, Catch_t_km2) rows collapsed to ", nrow(vals), " (one per Year) -",
              " fg_catch had duplicate Year rows for this FG (summed). Check upstream",
              " (catches_discards_fg) for an unintended duplicate FG_name/source split.")
    }
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
  
  ## Catches_Ecopath/Catches_Ecosim/Fleet_Structure are
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
## Other sheets are saved as csv files, not
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
## build_fg_references_sheet() - writes the final workbook's
## "FG_References" sheet: one row per FG, consolidating WHICH DATA
## SOURCE fed each of the OTHER final sheets for that FG, named to
## match those sheets' own codes (Ecopath_B -> B_ref, Ecopath_L ->
## L_ref, Ecopath_Di -> Di_ref, Ecopath_PBQB -> PBQB_ref/
## PBQB_method_ref, Ecopath_traits -> traits_ref, Ecopath_diet ->
## diet_ref) so the model's data provenance is reviewable FG by FG,
## sheet by sheet, rather than scattered across each block's own audit
## CSVs. L_ref/Di_ref currently carry the SAME value (this pipeline's
## catch_source is tracked once per FG/Year, applied to Catch_t/
## Landings_t/Discard_t together - there's no independently-tracked
## landings-only vs. discards-only source), kept as two columns anyway
## so every final sheet has its own matching reference column.
## PBQB_ref/PBQB_method_ref stay separate: PBQB_ref is about the DATA
## feeding PB_FG/QB_FG, PBQB_method_ref is the calculation METHOD/
## literature behind it - collapsing them would lose real information.
##
## Each argument is a directory to read that block's own native CSV
## outputs from (per-block CSV subfolders) -
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
                                      diet_csv_dir = NULL,
                                      dispatch_group_citation_path = if (exists("pcloud_dir", inherits = TRUE)) {
                                        file.path(pcloud_dir, "data/Complementary data/pbqb_reference_tables", "pbqb_dispatch_group_fallback_citation.csv")
                                      } else {
                                        file.path("reference_tables", "pbqb_dispatch_group_fallback_citation.csv")
                                      }) {
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
  
  ## --- B_ref: biomass_source, per FG, from 01_biomass.R's own
  ## survey_fg_annual_index_regional_combined.csv (Year x FG_num, one
  ## biomass_source value per row already - see 01_biomass.R's "FG
  ## biomass-source priority" section). Includes the
  ## nearest-year borrow fallback where it applied (biomass_source
  ## itself doesn't carry a separate "(borrowed from YYYY)" tag - see
  ## species_baseline_year_borrowed_REVIEW.csv / ecopath_B_baseline_
  ## year_borrowed_REVIEW.csv for exactly which FGs/years were borrowed).
  biomass_src_path <- file.path(biomass_csv_dir, "survey_fg_annual_index_regional_combined.csv")
  if (file.exists(biomass_src_path)) {
    b <- fread(biomass_src_path)
    if ("biomass_source" %in% names(b)) {
      b_by_fg <- b[, .(B_ref = paste(sort(unique(biomass_source)), collapse = " | ")), by = FG_num]
      refs <- merge(refs, b_by_fg, by = "FG_num", all.x = TRUE)
    } else {
      message("build_fg_references_sheet(): '", biomass_src_path, "' has no biomass_source column",
              " (older run?) - B_ref left blank.")
    }
  } else {
    message("build_fg_references_sheet(): '", biomass_src_path, "' not found - run 01_biomass.R",
            " against this workbook first. B_ref left blank for now.")
  }
  if (!"B_ref" %in% names(refs)) refs[, B_ref := NA_character_]
  
  ## --- L_ref / Di_ref: catch_source, per FG, from 02_fisheries.R's own
  ## Catches_Discards_FG_ts.csv (native/intermediate, fixed filename -
  ## unlike the DATASET_VERSION-suffixed CSV of the same data). Two
  ## columns (matching Ecopath_L/Ecopath_Di) carrying the SAME value -
  ## this pipeline tracks one catch_source per FG/Year, applied to
  ## Catch_t/Landings_t/Discard_t together, not independently for
  ## landings vs. discards.
  cd_ts <- read_native_sheet_csv("Catches_Discards_FG_ts", fisheries_csv_dir)
  if (!is.null(cd_ts) && "catch_source" %in% names(cd_ts)) {
    f_by_fg <- cd_ts[, .(catch_ref = paste(sort(unique(catch_source)), collapse = " | ")), by = FG_num]
    refs <- merge(refs, f_by_fg, by = "FG_num", all.x = TRUE)
    refs[, `:=`(L_ref = catch_ref, Di_ref = catch_ref)]
    refs[, catch_ref := NULL]
  } else {
    message("build_fg_references_sheet(): 'Catches_Discards_FG_ts.csv' not found (or has no catch_source",
            " column) in ", fisheries_csv_dir, " - run 02_fisheries.R against this workbook first.",
            " L_ref/Di_ref left blank for now.")
  }
  if (!"L_ref" %in% names(refs))  refs[, L_ref := NA_character_]
  if (!"Di_ref" %in% names(refs)) refs[, Di_ref := NA_character_]
  
  ## --- PBQB_ref + PBQB_method_ref: from 03_pbqb-traits.R's own
  ## PB_QB.csv (FG-level: PB_source/QB_source if EcoBase gap-filling
  ## ran in this R session, otherwise every FG is the same "empirical"
  ## default), PB_QB_spp.csv (species-level: which SPECIFIC published
  ## equation - PB_method/QB_method, e.g. "Then et al. 2015", "Palomares
  ## & Pauly 1998 (Z-based)" - actually ran for each species feeding that
  ## FG), pbqb_method_references.csv (the real citation text behind each
  ## of those method names), and species_parameter_references.csv (which
  ## FishBase study's Locality/Year backs each species' own growth/
  ## length-weight/maturity input data). PBQB_ref is about the DATA
  ## feeding PB_FG/QB_FG (empirical vs. EcoBase-filled, plus which
  ## growth-parameter studies back it); PBQB_method_ref is the actual
  ## calculation EQUATION/literature behind it - both now real, per-FG,
  ## per-method citations, not a generic word like "empirical" or
  ## "literature" - every reference for parameters, literature, model/
  ## ecobase source, or method equation (for fish and other FGs alike)
  ## should appear here, not just a generic placeholder.
  pbqb <- read_native_sheet_csv("PB_QB", pbqb_csv_dir)
  if (!is.null(pbqb)) {
    if (all(c("PB_source", "QB_source") %in% names(pbqb))) {
      pbqb[, PBQB_ref := paste0("PB: ", fifelse(is.na(PB_source), "n/a", PB_source),
                                "; QB: ", fifelse(is.na(QB_source), "n/a", QB_source))]
    } else {
      pbqb[, PBQB_ref := "empirical (species-level PB/QB, biomass-weighted to FG) - see PB_QB_spp.csv for the species behind each FG"]
    }
    refs <- merge(refs, pbqb[, .(FG_num, PBQB_ref)], by = "FG_num", all.x = TRUE)
  } else {
    message("build_fg_references_sheet(): 'PB_QB.csv' not found in ", pbqb_csv_dir,
            " - run 03_pbqb-traits.R against this workbook first. PBQB_ref left blank for now.")
  }
  if (!"PBQB_ref" %in% names(refs)) refs[, PBQB_ref := NA_character_]
  
  ## Real per-species-parameter-study citations (Locality/Year/Reference,
  ## from FishBase - "which study backs this species' own growth/length-
  ## weight/maturity DATA"), rolled up per FG and appended onto PBQB_ref -
  ## this is a different, finer-grained thing than PBQB_method_ref below
  ## (which equation was used), but equally a "literature reference" in
  ## the project's sense, so it belongs here rather than nowhere.
  species_param_refs <- read_native_sheet_csv("References", pbqb_csv_dir)
  if (!is.null(species_param_refs) && all(c("Species", "Reference") %in% names(species_param_refs))) {
    spp_fg_lookup <- read_native_sheet_csv("PB_QB_spp", pbqb_csv_dir)
    if (!is.null(spp_fg_lookup) && all(c("Species", "FG_num") %in% names(spp_fg_lookup))) {
      param_by_fg <- merge(species_param_refs[!is.na(Reference) & Reference != ""],
                           unique(spp_fg_lookup[, .(Species, FG_num)]), by = "Species")
      param_by_fg <- param_by_fg[, .(param_refs = paste(sort(unique(Reference)), collapse = " | ")), by = FG_num]
      refs <- merge(refs, param_by_fg, by = "FG_num", all.x = TRUE)
      refs[!is.na(param_refs), PBQB_ref := paste0(PBQB_ref, " - parameter studies: ", param_refs)]
      refs[, param_refs := NULL]
    } else {
      message("build_fg_references_sheet(): 'PB_QB_spp.csv' not found (or has no Species/FG_num columns)",
              " in ", pbqb_csv_dir, " - can't map species_parameter_references.csv's per-species citations",
              " to an FG, so they're left out of PBQB_ref.")
    }
  } else {
    message("build_fg_references_sheet(): 'species_parameter_references.csv' (native sheet 'References')",
            " not found in ", pbqb_csv_dir, " - PBQB_ref won't include per-species growth-study citations.")
  }
  
  ## PBQB_method_ref: real per-FG method citations, from PB_QB_spp's own
  ## PB_method/QB_method columns (the specific equation actually used for
  ## each species, e.g. "Pauly 1980 (Eq.9)", "Then et al. 2015",
  ## "Palomares & Pauly 1998 (Z-based)") joined against
  ## pbqb_method_references.csv's real citation text for each one -
  ## replacing the old coarse dispatch_group -> generic-sentence mapping,
  ## which threw away exactly which equation ran and cited it with one
  ## vague paragraph per taxon group instead. dispatch_group -> generic
  ## text is kept ONLY as a last-resort fallback for a species/FG that
  ## has a dispatch_group but no PB_method/QB_method value at all (e.g.
  ## every method failed for it) - it should basically never fire once
  ## PB_QB_spp.csv includes PB_method/QB_method.
  method_citation_lookup <- read_native_sheet_csv("PBQB_Method_References", pbqb_csv_dir)
  ## Fallback citation text per dispatch_group, read from an externalized
  ## CSV (dispatch_group, citation columns) when available; falls back to
  ## the hardcoded table below if the CSV is missing, so this still works
  ## for a caller that hasn't set up reference_tables/.
  if (file.exists(dispatch_group_citation_path)) {
    dg_ref <- fread(dispatch_group_citation_path)
    dispatch_group_fallback_citation <- setNames(dg_ref$citation, dg_ref$dispatch_group)
  } else {
    dispatch_group_fallback_citation <- c(
      fish        = "Fish P/B, Q/B from growth (VBGF) and natural/fishing mortality - see pbqb_method_references.csv for the specific equation (no PB_method/QB_method recorded for this FG's species)",
      mammal      = "Marine mammal P/B, Q/B from taxonomic-surrogate life-history parameters - see pbqb_method_references.csv",
      seabird     = "Seabird Q/B from daily-ration/body-mass regression - see pbqb_method_references.csv",
      invertebrate = "Benthic invertebrate P/B, Q/B from empirical length/weight- or temperature/longevity-based relationships - see pbqb_method_references.csv",
      invert      = "Benthic invertebrate P/B, Q/B from empirical length/weight-based relationships - see pbqb_method_references.csv",
      cephalopod  = "Cephalopod P/B, Q/B from short-lived life-history convention - see pbqb_method_references.csv",
      literature  = "EcoBase model repository (published literature P/B, Q/B) - see Ecobase sheet for the specific model/authors/year"
    )
  }
  spp <- read_native_sheet_csv("PB_QB_spp", pbqb_csv_dir)
  if (!is.null(spp)) {
    has_method_cols <- all(c("PB_method", "QB_method") %in% names(spp))
    if (has_method_cols && !is.null(method_citation_lookup) &&
        all(c("Method", "Citation") %in% names(method_citation_lookup))) {
      cite_lookup <- setNames(method_citation_lookup$Citation, method_citation_lookup$Method)
      cite_one <- function(method_name) {
        if (is.na(method_name) || method_name == "") return(NA_character_)
        ## `[[` on an ATOMIC named vector (this is a
        ## character vector, not a list) throws "subscript out of
        ## bounds" the moment method_name doesn't match any name, rather
        ## than returning NULL the way list indexing would. `[` (single
        ## bracket) returns NA for a no-match instead of erroring - safe
        ## for any method name, matched or not.
        hit <- unname(cite_lookup[method_name])
        if (is.na(hit)) paste0(method_name, " (no citation on file in pbqb_method_references.csv - check the Method name spelling)") else hit
      }
      m_by_fg <- spp[, .(
        pb_cites = paste(sort(unique(vapply(unique(na.omit(PB_method)), cite_one, character(1)))), collapse = " || "),
        qb_cites = paste(sort(unique(vapply(unique(na.omit(QB_method)), cite_one, character(1)))), collapse = " || ")
      ), by = FG_num]
      m_by_fg[, PBQB_method_ref := paste0(
        fifelse(nzchar(pb_cites), paste0("PB method: ", pb_cites), "PB method: not recorded"), " ; ",
        fifelse(nzchar(qb_cites), paste0("QB method: ", qb_cites), "QB method: not recorded"))]
      refs <- merge(refs, m_by_fg[, .(FG_num, PBQB_method_ref)], by = "FG_num", all.x = TRUE)
    } else if ("dispatch_group" %in% names(spp)) {
      if (!has_method_cols) {
        message("build_fg_references_sheet(): 'PB_QB_spp.csv' has no PB_method/QB_method columns",
                " (older run of 03_pbqb-traits.R, or species_pb_qb was built without them) - falling",
                " back to the generic dispatch_group description for PBQB_method_ref.")
      }
      if (is.null(method_citation_lookup)) {
        message("build_fg_references_sheet(): 'pbqb_method_references.csv' (native sheet",
                " 'PBQB_Method_References') not found in ", pbqb_csv_dir, " - falling back to the",
                " generic dispatch_group description for PBQB_method_ref.")
      }
      m_by_fg <- spp[!is.na(dispatch_group), .(dispatch_groups = paste(sort(unique(dispatch_group)), collapse = ",")), by = FG_num]
      m_by_fg[, PBQB_method_ref := vapply(strsplit(dispatch_groups, ","), function(groups) {
        hits <- unique(dispatch_group_fallback_citation[groups])
        hits <- hits[!is.na(hits)]
        unmatched <- setdiff(groups, names(dispatch_group_fallback_citation))
        if (length(unmatched) > 0) hits <- c(hits, paste0("dispatch_group='", unmatched, "' (no citation on file)"))
        if (length(hits) == 0) return(NA_character_)
        paste(hits, collapse = " | ")
      }, character(1))]
      refs <- merge(refs, m_by_fg[, .(FG_num, PBQB_method_ref)], by = "FG_num", all.x = TRUE)
    } else {
      message("build_fg_references_sheet(): 'PB_QB_spp.csv' has neither PB_method/QB_method nor",
              " dispatch_group columns - PBQB_method_ref left blank.")
    }
  } else {
    message("build_fg_references_sheet(): 'PB_QB_spp.csv' not found in ", pbqb_csv_dir,
            " - run 03_pbqb-traits.R with species_pb_qb passed to add_pbqb_to_ecopath_workbook()",
            " first. PBQB_method_ref left blank for now.")
  }
  if (!"PBQB_method_ref" %in% names(refs)) refs[, PBQB_method_ref := NA_character_]
  refs[is.na(PBQB_method_ref) & !is.na(PBQB_ref) & grepl("EcoBase", PBQB_ref),
       PBQB_method_ref := "EcoBase model repository (published literature P/B, Q/B) - see Ecobase sheet for the specific model/authors/year"]
  
  ## --- traits_ref: 03_pbqb-traits.R's
  ## traits_ewe/Ecopath_traits table is NOT a static hand-curated sheet
  ## - its trait columns (Max_length/Mean_length/Mean_weight/
  ## Mean_lifespan_years/Vulnerability_index/Ecology/IUCN_conservation_
  ## status/Exploitation_status/Occurrence_status) are live per-species
  ## fetches from FishBase/SeaLifeBase via the rfishbase package
  ## (species()/country() - see that script's own "traits_ewe sheet"
  ## comment block for exactly which field feeds which column), with
  ## Organism from the taxonomic Class/Phylum/Kingdom lookup (Step 1).
  ## Every FG gets the same fixed citation (real, but not per-FG) since
  ## this is a database source, not a per-FG literature figure - the
  ## per-species GROWTH/maturity study behind the underlying life-
  ## history parameters (a different, finer-grained thing) is already
  ## in PBQB_ref's "parameter studies" rollup above.
  refs[, traits_ref := paste0(
    "FishBase / SeaLifeBase (via the rfishbase R package, species()/country() calls) - Max_length, ",
    "Mean_length, Mean_weight, Mean_lifespan_years, Vulnerability_index, Ecology, IUCN_conservation_status, ",
    "Exploitation_status, Occurrence_status. Froese, R. and D. Pauly, Editors. FishBase. World Wide Web ",
    "electronic publication. www.fishbase.org; Palomares, M.L.D. and D. Pauly, Editors. SeaLifeBase. World ",
    "Wide Web electronic publication. www.sealifebase.org. Organism from taxonomic classification (WoRMS). ",
    "See PBQB_ref's 'parameter studies' entries for the specific growth/maturity study behind each species' ",
    "own life-history parameters."
  )]
  
  ## --- diet_ref: from 04_diets.R's own diet_references_by_fg.csv
  ## (per-FG study citations, rolled up from the metaweb's own
  ## Reference column via build_species_diet()/predator_references).
  diet_refs_path <- file.path(diet_csv_dir, "diet_references_by_fg.csv")
  if (file.exists(diet_refs_path)) {
    d <- fread(diet_refs_path)
    if (all(c("FG_num", "references") %in% names(d))) {
      refs <- merge(refs, d[, .(FG_num, diet_ref = references)], by = "FG_num", all.x = TRUE)
    } else {
      message("build_fg_references_sheet(): '", diet_refs_path, "' is missing FG_num/references column(s) -",
              " diet_ref left blank.")
    }
  } else {
    message("build_fg_references_sheet(): '", diet_refs_path, "' not found - run 04_diets.R against this",
            " workbook first. diet_ref left blank for now.")
  }
  if (!"diet_ref" %in% names(refs)) refs[, diet_ref := NA_character_]
  refs[!is.na(diet_ref) & diet_ref == "", diet_ref := NA_character_]  # fwrite()/fread() round-trips NA as "" for character columns by default - restore true NA rather than a blank string
  
  refs <- refs[, .(FG_num, FG_name, B_ref, L_ref, Di_ref, PBQB_ref, PBQB_method_ref, traits_ref, diet_ref)]
  setorder(refs, FG_num)
  upsert_workbook_sheets(list(FG_References = refs), out_path)
  n_populated <- refs[, sum(!is.na(B_ref) | !is.na(L_ref) | !is.na(Di_ref) | !is.na(PBQB_ref) | !is.na(diet_ref))]
  message("build_fg_references_sheet(): wrote 'FG_References' sheet - ", nrow(refs), " FG(s), ", n_populated,
          " with at least one reference filled in so far. Columns: ", paste(names(refs), collapse = ", "), ".")
  invisible(refs)
}

## =================================================================
## append_reference_columns_to_final_sheets()
##
## Ecopath_B/Ecopath_L/Ecopath_Di/Ecopath_PBQB/Ecopath_traits should each
## include a reference column for where their value is calculated from
## (e.g. a literature reference, MEDITS, etc). build_fg_references_
## sheet() above already tracks exactly this, per FG, but only in its
## OWN separate "FG_References" lookup sheet - this function copies
## that same provenance onto each of the five sheets themselves, as a
## trailing "Reference" column, so the source for a value is visible
## right next to it rather than requiring a second sheet to cross-
## reference.
##
##   Ecopath_B      <- FG_References$B_ref
##   Ecopath_L      <- FG_References$L_ref
##   Ecopath_Di     <- FG_References$Di_ref
##   Ecopath_traits <- FG_References$traits_ref
##   Ecopath_PBQB   <- FG_References$PBQB_ref combined with
##                      $PBQB_method_ref (data source and calculation
##                      method/literature are two separate FG_References
##                      columns, per that function's own header - both
##                      are folded into this sheet's one trailing column)
##
## Must run AFTER build_fg_references_sheet() has (re)written
## FG_References for this workbook - it reads FG_References straight
## back out of the workbook, so it always reflects whatever data each
## block actually used, not a stale snapshot. Safe to call repeatedly:
## replaces its own "Reference" column each time rather than stacking
## duplicates. Any of the five sheets that doesn't exist in the
## workbook yet (an earlier script hasn't run against this exact path
## yet) is skipped with a message, never an error.
## =================================================================
append_reference_columns_to_final_sheets <- function(out_path) {
  if (!requireNamespace("openxlsx", quietly = TRUE)) stop("openxlsx package required.")
  if (!file.exists(out_path)) {
    message("append_reference_columns_to_final_sheets(): no workbook at '", out_path, "' yet - skipping.")
    return(invisible(NULL))
  }
  existing_sheets <- openxlsx::getSheetNames(out_path)
  if (!"FG_References" %in% existing_sheets) {
    message("append_reference_columns_to_final_sheets(): no 'FG_References' sheet in '", out_path,
            "' yet - run build_fg_references_sheet() first. Skipping.")
    return(invisible(NULL))
  }
  refs <- as.data.table(openxlsx::read.xlsx(out_path, sheet = "FG_References"))
  if (!"FG_num" %in% names(refs)) {
    message("append_reference_columns_to_final_sheets(): 'FG_References' sheet has no FG_num column - skipping.")
    return(invisible(NULL))
  }
  refs[, FG_num := as.integer(FG_num)]
  
  ## sheet name -> which FG_References column becomes its own trailing
  ## "Reference" column (Ecopath_PBQB handled separately below, since
  ## it combines two FG_References columns into one).
  sheet_ref_map <- list(
    Ecopath_B      = "B_ref",
    Ecopath_L      = "L_ref",
    Ecopath_Di     = "Di_ref",
    Ecopath_traits = "traits_ref"
  )
  
  updated <- list()
  for (sheet_name in names(sheet_ref_map)) {
    if (!sheet_name %in% existing_sheets) next
    dt <- as.data.table(openxlsx::read.xlsx(out_path, sheet = sheet_name))
    if (!"FG_num" %in% names(dt)) {
      message("append_reference_columns_to_final_sheets(): '", sheet_name, "' has no FG_num column - skipping.")
      next
    }
    dt[, FG_num := as.integer(FG_num)]
    ref_col <- sheet_ref_map[[sheet_name]]
    if ("Reference" %in% names(dt)) dt[, Reference := NULL]  # drop a stale copy from a previous run before re-merging
    dt <- merge(dt, refs[, .(FG_num, Reference = get(ref_col))], by = "FG_num", all.x = TRUE)
    setorder(dt, FG_num)
    updated[[sheet_name]] <- dt
  }
  
  ## Ecopath_PBQB: PBQB_ref (the DATA feeding PB_FG/QB_FG) and
  ## PBQB_method_ref (the calculation METHOD/literature behind it) are
  ## two separate FG_References columns by design (see that function's
  ## header) - combined here into one "<data> | method: <method>"
  ## string since this sheet gets only one trailing column.
  if ("Ecopath_PBQB" %in% existing_sheets) {
    dt <- as.data.table(openxlsx::read.xlsx(out_path, sheet = "Ecopath_PBQB"))
    if ("FG_num" %in% names(dt)) {
      dt[, FG_num := as.integer(FG_num)]
      if ("Reference" %in% names(dt)) dt[, Reference := NULL]
      pbqb_refs <- refs[, .(FG_num, PBQB_ref, PBQB_method_ref)]
      pbqb_refs[, Reference := fifelse(
        !is.na(PBQB_ref) & !is.na(PBQB_method_ref), paste0(PBQB_ref, " | method: ", PBQB_method_ref),
        fifelse(!is.na(PBQB_ref), PBQB_ref,
                fifelse(!is.na(PBQB_method_ref), paste0("method: ", PBQB_method_ref), NA_character_)))]
      dt <- merge(dt, pbqb_refs[, .(FG_num, Reference)], by = "FG_num", all.x = TRUE)
      setorder(dt, FG_num)
      updated[["Ecopath_PBQB"]] <- dt
    } else {
      message("append_reference_columns_to_final_sheets(): 'Ecopath_PBQB' has no FG_num column - skipping.")
    }
  }
  
  if (length(updated) == 0) {
    message("append_reference_columns_to_final_sheets(): none of the five target sheets exist yet in '",
            out_path, "' - nothing to do.")
    return(invisible(NULL))
  }
  upsert_workbook_sheets(updated, out_path)
  message("append_reference_columns_to_final_sheets(): added/updated a trailing 'Reference' column on: ",
          paste(names(updated), collapse = ", "), ".")
  invisible(updated)
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
  
  ## The excel ecopath_ecosim file must have exactly
  ## the intended sheets; other sheets are saved as csv files, not
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
## =================================================================
## Multistanza (juvenile/adult) biomass split - shared support
## functions. Addresses European hake adult (FG "European hake adult")
## getting no real biomass at all under the current species->FG lookup
## design - see
## prepare_fg_lookup()'s own header comment above for the root cause:
## its stanza tie-break keeps exactly one row per stanza species,
## by lowest FG_num, so a stanza's higher-FG_num FG - "European hake
## adult" in the current FG_WMed_2026.csv - gets ZERO species mapped to
## it anywhere downstream, including ordinary MEDITS/MEDIAS survey
## matching, not just stock-assessment attachment).
##
## Both 01_biomass.R and 02_fisheries.R independently hit the SAME
## underlying data limitation: no catch/landings/survey source in this
## pipeline reports an age/length-resolved split for a stanza species -
## every source (GFCM, STECF's own Catches file, STAR/RAM, MEDITS/
## MEDIAS) reports one undifferentiated figure per species. STECF FDI's
## own "Biological/FDI Discards Age.csv"/"FDI Landings Age.csv" are the
## ONE age-resolved source in this pipeline (Spain/France/Italy, 2014+
## only). 02_fisheries.R already uses them to derive a real
## juvenile:adult proportion for the CATCH side (see that script's own
## "Multistanza age split" section) - kept as its own separate inline
## block there rather than switched onto these shared functions in this
## pass, to avoid touching already-validated production catch logic
## without being able to re-run it against real data when written.
##
## detect_multistanza_fg_pairs()/compute_multistanza_age_proportion()
## below generalize that same approach so 01_biomass.R can compute and
## apply the IDENTICAL proportion to the BIOMASS/survey side too - this
## reuses the catch-side age
## proportion as the best-available real signal for the biomass split,
## now that fish FGs are barred from any literature/EcoBase substitute
## (see STEP 15b in 01_biomass.R). This is an acknowledged PROXY - catch
## selectivity at age is not the same as standing-biomass age structure
## - not a measurement of biomass age structure itself, and every use
## of it downstream says so explicitly (catch_source/biomass_source
## text, REVIEW csv).
## =================================================================

## Same marker-folder search 02_fisheries.R uses locally
## (find_versioned_subdir(), defined in that script BEFORE it sources
## this file) to resolve a year-stamped/versioned data folder (e.g.
## STECF FDI's unzipped download). Duplicated here under a DIFFERENT
## name, deliberately - not sharing the name means sourcing this file
## from 02_fisheries.R can never silently overwrite (or be overwritten
## by) that script's own copy, so a future edit to one can't change the
## other's behavior through the shared name.
resolve_versioned_data_subdir <- function(parent_dir, marker) {
  if (!dir.exists(parent_dir)) return(NA_character_)
  candidates <- c(parent_dir, list.dirs(parent_dir, recursive = FALSE, full.names = TRUE))
  has_marker <- file.exists(file.path(candidates, marker)) | dir.exists(file.path(candidates, marker))
  hits <- candidates[has_marker]
  if (length(hits) == 0) return(NA_character_)
  if (length(hits) > 1) message("[Paths] Multiple candidate folders under '", parent_dir, "' have '", marker,
                                "' - using the first one found: '", hits[1], "'.")
  hits[1]
}

## detect_multistanza_fg_pairs(): STRUCTURAL stanza-pair detection (same
## rule prepare_fg_lookup() and 02_fisheries.R's own stanza tie-break
## both already use) - a species is a clean juvenile/adult pair iff it
## maps to more than one FG, EVERY one of those FGs is exclusive to that
## species alone (no other species shares it), exactly one of them is
## NOT named "...adult", and at least one of them IS. species_fg_dt must
## have ScientificName/FG_num/FG_name columns - pass the UNDEDUPED
## species->FG catalog (e.g. dataframe2 in 01_biomass.R, or fg_lookup
## before its own tie-break in 02_fisheries.R), never
## prepare_fg_lookup()'s own deduped output, which has already collapsed
## each stanza down to one row by the time it exists.
detect_multistanza_fg_pairs <- function(species_fg_dt) {
  sf <- unique(as.data.table(species_fg_dt)[, .(ScientificName, FG_num, FG_name)])
  fg_species_counts <- sf[, .(n_species_in_fg = uniqueN(ScientificName)), by = FG_num]
  sf <- merge(sf, fg_species_counts, by = "FG_num")
  sp_fg_counts <- sf[, .(n_fg = uniqueN(FG_num)), by = ScientificName]
  multi_fg_species <- sp_fg_counts[n_fg > 1, ScientificName]
  
  empty_pairs <- data.table(ScientificName = character(), FG_num_juv = integer(), FG_name_juv = character(),
                            FG_num_adult = integer(), FG_name_adult = character())
  if (length(multi_fg_species) == 0) return(empty_pairs)
  
  stanza_check <- sf[ScientificName %in% multi_fg_species,
                     .(all_exclusive = all(n_species_in_fg == 1)), by = ScientificName]
  stanza_species <- stanza_check[all_exclusive == TRUE, ScientificName]
  if (length(stanza_species) == 0) return(empty_pairs)
  
  ms <- sf[ScientificName %in% stanza_species, .(ScientificName, FG_num, FG_name)]
  ms[, is_adult := str_detect(FG_name, regex("adult", ignore_case = TRUE))]
  has_adult_ms <- ms[, .(has_adult = any(is_adult), n_juv = sum(!is_adult)), by = ScientificName]
  clean_pair_species <- has_adult_ms[has_adult == TRUE & n_juv == 1, ScientificName]
  if (length(clean_pair_species) == 0) return(empty_pairs)
  
  ms_clean <- ms[ScientificName %in% clean_pair_species]
  juv_part   <- unique(ms_clean[is_adult == FALSE, .(ScientificName, FG_num_juv = FG_num, FG_name_juv = FG_name)])
  adult_part <- unique(ms_clean[is_adult == TRUE,  .(ScientificName, FG_num_adult = FG_num, FG_name_adult = FG_name)])[
    , .SD[1], by = ScientificName]  # a species could in principle have >1 adult-named FG too - keep just the first, defensively
  merge(juv_part, adult_part, by = "ScientificName")
}

## resolve_fao_species_reference(): finds (or downloads) FAO's
## CL_FI_SPECIES_GROUPS.csv reference and standardizes its English-name/
## scientific-name columns - the same source 02_fisheries.R already uses
## for its own species-code resolution, factored out here so
## 01_biomass.R can resolve a multistanza species' FAO 3-alpha code
## (needed to filter the FDI age files below) without needing the rest
## of 02_fisheries.R's GFCM/SAU matching machinery.
resolve_fao_species_reference <- function(pcloud_dir, csv_out_dir) {
  found <- list.files(pcloud_dir, pattern = "CL_FI_SPECIES_GROUPS.csv$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  path <- if (length(found) > 0) found[1] else {
    url <- "https://data.apps.fao.org/catalog/dataset/b70c52c1-475f-4951-a8ac-de44016abd9b/resource/2c0f936d-6c36-4715-9c7f-fa5a70c00249/download/cl_fi_species_groups.csv"
    destfile <- file.path(csv_out_dir, "CL_FI_SPECIES_GROUPS.csv")
    ok <- tryCatch({ download.file(url, destfile = destfile, mode = "wb", method = "libcurl", quiet = TRUE); TRUE },
                   error = function(e) FALSE, warning = function(w) FALSE)
    if (!isTRUE(ok) || !file.exists(destfile) || file.size(destfile) == 0) {
      if (!requireNamespace("httr", quietly = TRUE)) install.packages("httr")
      resp <- tryCatch(httr::GET(url, httr::config(http_version = 1.1), httr::write_disk(destfile, overwrite = TRUE), httr::timeout(120)), error = function(e) e)
      if (inherits(resp, "error") || httr::status_code(resp) != 200 || !file.exists(destfile) || file.size(destfile) == 0) {
        stop("Could not download cl_fi_species_groups.csv - download it manually into pcloud_dir as CL_FI_SPECIES_GROUPS.csv.")
      }
    }
    destfile
  }
  fao_species <- fread(path, encoding = "UTF-8")
  name_col <- grep("english|name.*en$|^name$", names(fao_species), ignore.case = TRUE, value = TRUE)[1]
  sci_col  <- grep("scientific", names(fao_species), ignore.case = TRUE, value = TRUE)[1]
  setnames(fao_species, c(name_col, sci_col), c("Name_En", "Scientific_Name"), skip_absent = TRUE)
  fao_species
}

## compute_multistanza_age_proportion(): loads STECF FDI's "Biological/
## FDI Discards Age.csv"/"FDI Landings Age.csv", restricts to whichever
## multistanza species have a resolvable FAO 3-alpha code, and computes
## a real juvenile:adult proportion per species/Country/Year/source file
## - the exact same logic 02_fisheries.R uses for the catch side (see
## this function group's own header comment above), factored out so it
## can be called from more than one script. juv_max_age: an ASSUMPTION
## (age <= this counts as juvenile), not a measurement - no per-stock
## maturity-at-age figure is wired into this pipeline yet. Default 0
## (only age-0 recruits count as juvenile) matches 02_fisheries.R's own
## default; pass a named vector (by ScientificName) to override per
## species.
compute_multistanza_age_proportion <- function(multistanza_fg_pairs, stecf_bio_dir, fao_species,
                                               country_codes = c(ESP = "Spain", FRA = "France", ITA = "Italy"),
                                               juv_max_age = NULL) {
  empty_result <- data.table()
  if (nrow(multistanza_fg_pairs) == 0) {
    message("\n[Multistanza age split] No multistanza FG pair to compute a proportion for - skipped.")
    return(empty_result)
  }
  discards_age_file <- file.path(stecf_bio_dir, "FDI Discards Age.csv")
  landings_age_file <- file.path(stecf_bio_dir, "FDI Landings Age.csv")
  if (!file.exists(discards_age_file) || !file.exists(landings_age_file)) {
    message("\n[Multistanza age split] 'FDI Discards Age.csv'/'FDI Landings Age.csv' not found in '", stecf_bio_dir,
            "' - no age-resolved proportion available.")
    return(empty_result)
  }
  code_col <- grep("^3A_Code$|alpha.?3", names(fao_species), ignore.case = TRUE, value = TRUE)[1]
  if (is.na(code_col)) {
    message("\n[Multistanza age split] fao_species has no 3-alpha-code column (checked: ",
            paste(names(fao_species), collapse = ", "), ") - can't resolve FDI's species codes.")
    return(empty_result)
  }
  sci_to_code <- unique(fao_species[!is.na(Scientific_Name) & Scientific_Name %in% multistanza_fg_pairs$ScientificName,
                                    .(ScientificName = Scientific_Name, SpeciesCode = get(code_col))])
  sci_to_code <- sci_to_code[!is.na(SpeciesCode) & SpeciesCode != ""]
  unresolved_sci <- setdiff(multistanza_fg_pairs$ScientificName, sci_to_code$ScientificName)
  if (length(unresolved_sci) > 0) {
    message("[Multistanza age split] No FAO 3-alpha code found for: ", paste(unresolved_sci, collapse = ", "), ".")
  }
  if (nrow(sci_to_code) == 0) {
    message("[Multistanza age split] None of the multistanza species resolved to a 3-alpha code. Skipped.")
    return(empty_result)
  }
  
  load_fdi_age_file <- function(path, label) {
    raw <- fread(path, encoding = "UTF-8")
    setnames(raw, gsub("\\s+", "", names(raw)))
    message("[Multistanza age split] ", label, " columns found: ", paste(names(raw), collapse = ", "))
    species_col <- grep("^species$", names(raw), ignore.case = TRUE, value = TRUE)[1]
    country_col <- grep("^country$", names(raw), ignore.case = TRUE, value = TRUE)[1]
    year_col    <- grep("^year$", names(raw), ignore.case = TRUE, value = TRUE)[1]
    age_col     <- grep("^age$|^age_?class$|^age_?group$", names(raw), ignore.case = TRUE, value = TRUE)[1]
    if (any(is.na(c(species_col, country_col, year_col)))) {
      message("[Multistanza age split] ", label, " is missing an expected species/country/year column",
              " (checked: ", paste(names(raw), collapse = ", "), ") - skipped.")
      return(data.table())
    }
    raw <- raw[get(species_col) %in% sci_to_code$SpeciesCode]
    if (nrow(raw) == 0) {
      message("[Multistanza age split] ", label, " has no row for any multistanza species' 3-alpha code",
              " (", paste(sci_to_code$SpeciesCode, collapse = ", "), ") - skipped.")
      return(data.table())
    }
    raw[, Country := country_codes[get(country_col)]]
    raw <- raw[!is.na(Country)]
    if (nrow(raw) == 0) return(data.table())
    if (!is.na(age_col)) {
      value_col <- setdiff(grep("weight|number|value|discard|land", names(raw), ignore.case = TRUE, value = TRUE),
                           c(species_col, country_col, year_col, age_col))[1]
      if (is.na(value_col)) {
        message("[Multistanza age split] ", label, " has an age column but no recognizable value column",
                " (checked: ", paste(names(raw), collapse = ", "), ") - skipped.")
        return(data.table())
      }
      raw[, age_num := suppressWarnings(as.numeric(gsub("[^0-9.]", "", get(age_col))))]
      out <- raw[, .(SpeciesCode = get(species_col), Country, Year = suppressWarnings(as.integer(get(year_col))),
                     age_num, value = suppressWarnings(as.numeric(get(value_col))))]
    } else {
      age_cols <- grep("^[0-9]+\\+?$|^age[_]?[0-9]+\\+?$", names(raw), ignore.case = TRUE, value = TRUE)
      if (length(age_cols) == 0) {
        message("[Multistanza age split] ", label, " has no 'age' column and no numeric-looking age",
                " columns (checked: ", paste(names(raw), collapse = ", "), ") - skipped.")
        return(data.table())
      }
      long <- melt(raw, id.vars = c(species_col, country_col, year_col), measure.vars = age_cols,
                   variable.name = "age_col", value.name = "value")
      long[, age_num := suppressWarnings(as.numeric(gsub("[^0-9.]", "", age_col)))]
      out <- long[, .(SpeciesCode = get(species_col), Country, Year = suppressWarnings(as.integer(get(year_col))),
                      age_num, value = suppressWarnings(as.numeric(value)))]
    }
    out[!is.na(value) & !is.na(age_num)]
  }
  
  discards_age <- load_fdi_age_file(discards_age_file, "FDI Discards Age.csv")
  landings_age <- load_fdi_age_file(landings_age_file, "FDI Landings Age.csv")
  
  if (is.null(juv_max_age)) juv_max_age <- setNames(rep(0, nrow(multistanza_fg_pairs)), multistanza_fg_pairs$ScientificName)
  
  compute_juv_prop <- function(age_dt, label) {
    if (nrow(age_dt) == 0) return(data.table())
    age_dt <- merge(age_dt, sci_to_code, by = "SpeciesCode")
    age_dt[, juv_cutoff := juv_max_age[ScientificName]]
    age_dt[, stanza := fifelse(age_num <= juv_cutoff, "Juvenile", "Adult")]
    agg <- age_dt[, .(value = sum(value, na.rm = TRUE)), by = .(ScientificName, Country, Year, stanza)]
    wide <- dcast(agg, ScientificName + Country + Year ~ stanza, value.var = "value", fill = 0)
    if (!"Juvenile" %in% names(wide)) wide[, Juvenile := 0]
    if (!"Adult" %in% names(wide)) wide[, Adult := 0]
    wide[, prop_juvenile := fifelse((Juvenile + Adult) > 0, Juvenile / (Juvenile + Adult), NA_real_)]
    wide[, source_file := label]
    wide[!is.na(prop_juvenile)]
  }
  discards_prop <- compute_juv_prop(discards_age, "FDI Discards Age.csv")
  landings_prop <- compute_juv_prop(landings_age, "FDI Landings Age.csv")
  result <- rbindlist(list(discards_prop, landings_prop), use.names = TRUE, fill = TRUE)
  if (nrow(result) > 0) {
    summary_prop <- result[, .(prop_juvenile_avg = mean(prop_juvenile, na.rm = TRUE), n_country_year = .N),
                           by = .(ScientificName, source_file)]
    message("[Multistanza age split] Juvenile proportion by species/source (averaged across FDI's own",
            " Spain/France/Italy 2014+ coverage):")
    print(summary_prop)
  } else {
    message("[Multistanza age split] Discards/Landings Age files loaded but produced no usable juvenile proportion.")
  }
  result
}

## compute_multistanza_age_proportion_from_medits_tc(): an
## alternative to compute_multistanza_age_proportion() above - that function
## derives juvenile:adult proportion from STECF FDI's CATCH-at-age (a proxy:
## catch selectivity at age != standing-biomass age structure). This one
## derives it directly from MEDITS' own TC.csv (biological/length-frequency
## file), which actually records maturity STAGE per individual at the haul
## level - a real survey-based measurement of the standing population's
## age structure, not a proxy via the fishery.
##
## MEDITS/JRC TC.csv does not carry ScientificName - species are coded as
## genus (4-letter) + species (3-letter) abbreviations per the MEDITS
## reference list (Annex XV), e.g. Merluccius merluccius -> genus "MERL",
## species "MER". By default this function DERIVES that code the standard
## way (first 4 letters of the genus, first 3 of the species epithet,
## uppercased) rather than requiring a hardcoded per-species lookup - this
## matches the one confirmed example (hake) exactly. Pass `code_overrides`
## (a named character vector, "ScientificName" = "GENU_SPE") for any species
## where the standard truncation doesn't match the real MEDITS code.
##
## Column meanings, checked against the real 2024_MEDBSsurvey TC.csv:
## `maturity` = MEDITS maturity STAGE ("0" undetermined, "1" immature/
## virgin, "2" developing, "3" mature/spawner, "4" spent/resting, "ND");
## `matsub` = sub-stage LETTER only (A/B/C); `nblon` = individual COUNT for
## that length-class/sex/stage cell. The proportion returned is
## biomass-weighted (count x length^lw_b), since it splits Ecopath_B.
##
## juvenile_stage_cutoff (default 2): an individual counts as "Adult" once
## its matsub's LEADING DIGIT is >= this cutoff (so default: stage 1 =
## juvenile; 2A/2B/2C/3/4A/4B = adult). This is a judgment call, not a
## measurement - stage 2A ("virgin developing") hasn't spawned yet either,
## so pass juvenile_stage_cutoff = 3 to count only stage 3+ as adult if
## that's the more appropriate cutoff for a given species/stock. rows with
## matsub "0"/"ND"/NA are excluded from the proportion entirely (truly
## unknown maturity, not assumed either way).
compute_multistanza_age_proportion_from_medits_tc <- function(multistanza_fg_pairs, tc_path,
                                                              code_overrides = NULL,
                                                              juvenile_stage_cutoff = 2,
                                                              year_range = NULL, area_filter = NULL,
                                                              stage0_as_juvenile = TRUE, lw_b = 3) {
  empty_result <- data.table()
  if (nrow(multistanza_fg_pairs) == 0) {
    message("\n[Multistanza age split - MEDITS TC] No multistanza FG pair to compute a proportion for - skipped.")
    return(empty_result)
  }
  if (!file.exists(tc_path)) {
    message("\n[Multistanza age split - MEDITS TC] TC.csv not found at '", tc_path, "' - skipped.")
    return(empty_result)
  }
  derive_code <- function(sci_name) {
    parts <- strsplit(trimws(sci_name), "\\s+")[[1]]
    if (length(parts) < 2) return(NA_character_)
    c(genus = toupper(substr(parts[1], 1, 4)), species = toupper(substr(parts[2], 1, 3)))
  }
  sci_names <- unique(multistanza_fg_pairs$ScientificName)
  code_map <- rbindlist(lapply(sci_names, function(sci) {
    if (!is.null(code_overrides) && sci %in% names(code_overrides)) {
      parts <- strsplit(code_overrides[[sci]], "_")[[1]]
      data.table(ScientificName = sci, genus_code = parts[1], species_code = parts[2])
    } else {
      dc <- derive_code(sci)
      data.table(ScientificName = sci, genus_code = dc["genus"], species_code = dc["species"])
    }
  }))
  code_map <- code_map[!is.na(genus_code) & !is.na(species_code)]
  if (nrow(code_map) == 0) {
    message("\n[Multistanza age split - MEDITS TC] Could not derive a MEDITS genus/species code for any",
            " multistanza species - skipped.")
    return(empty_result)
  }
  message("[Multistanza age split - MEDITS TC] Using genus/species code(s): ",
          paste0(code_map$ScientificName, " -> ", code_map$genus_code, "/", code_map$species_code, collapse = ", "),
          " - if these don't match the real MEDITS reference-list code for a species, pass it via code_overrides.")
  
  tc <- fread(tc_path, encoding = "UTF-8")
  setnames(tc, tolower(gsub("\\s+", "", names(tc))))
  required_cols <- c("genus", "species", "maturity", "nblon", "length_class")
  missing_cols <- setdiff(required_cols, names(tc))
  if (length(missing_cols) > 0) {
    message("\n[Multistanza age split - MEDITS TC] TC.csv is missing expected column(s): ",
            paste(missing_cols, collapse = ", "), " (found: ", paste(names(tc), collapse = ", "), ") - skipped.")
    return(empty_result)
  }
  tc[, genus := toupper(trimws(genus))]
  tc[, species := toupper(trimws(species))]
  tc <- merge(tc, code_map, by.x = c("genus", "species"), by.y = c("genus_code", "species_code"))
  if (nrow(tc) == 0) {
    message("\n[Multistanza age split - MEDITS TC] No TC.csv rows matched any derived genus/species code -",
            " the real MEDITS code for this species likely differs from the standard truncation; pass the",
            " correct code via code_overrides.")
    return(empty_result)
  }
  if (!is.null(year_range) && "year" %in% names(tc)) tc <- tc[year %in% year_range]
  if (!is.null(area_filter) && "area" %in% names(tc)) tc <- tc[area %in% area_filter]
  
  ## COLUMN MEANINGS, CHECKED AGAINST THE REAL 2024_MEDBSsurvey TC.csv
## (hake rows): `maturity` holds the MEDITS maturity STAGE (values 0, 1,
  ## 2, 3, 4, ND); `matsub` holds only the sub-stage LETTER (A, B, C, ND);
  ## `nblon` is the number of individuals in that length class x sex x
  ## stage cell. The earlier version read the stage from `matsub` (no
  ## digits -> every row dropped) and the count from `maturity`.
  tc[, stage_clean := toupper(trimws(as.character(maturity)))]
  tc[, stage_num := suppressWarnings(as.numeric(gsub("[^0-9]", "", stage_clean)))]
  tc[, n_individuals := suppressWarnings(as.numeric(nblon))]
  tc[, length_mm := suppressWarnings(as.numeric(length_class))]
  n_stage0 <- tc[stage_num == 0, sum(n_individuals, na.rm = TRUE)]
  ## FLAGGED ASSUMPTION: stage 0 = "undetermined" (sex not determinable).
  ## In MEDITS these are overwhelmingly small, unsexed fish, so they are
  ## counted as Juvenile here. Excluding them instead would bias the split
  ## strongly toward adults. Set stage0_as_juvenile = FALSE to exclude.
  if (stage0_as_juvenile) {
    tc[stage_num == 0, stage_num := 1]
    message("[Multistanza age split - MEDITS TC] ", round(n_stage0), " individual(s) at maturity stage 0",
            " (undetermined, unsexed) counted as Juvenile (stage0_as_juvenile = TRUE).")
  } else {
    tc <- tc[stage_num != 0]
  }
  tc <- tc[!is.na(stage_num) & !is.na(n_individuals) & n_individuals > 0]
  if (nrow(tc) == 0) {
    message("\n[Multistanza age split - MEDITS TC] No rows with a usable maturity stage and nblon count -",
            " skipped.")
    return(empty_result)
  }
  tc[, stanza := fifelse(stage_num < juvenile_stage_cutoff, "Juvenile", "Adult")]
  ## Ecopath_B is split by BIOMASS, not by numbers. Each individual is
  ## weighted by length^lw_b (W = a*L^b; the `a` cancels in a proportion).
  ## FLAGGED ASSUMPTION: lw_b = 3 (isometric growth) unless a species-
  ## specific exponent is supplied - hake's published b is close to 3.
  if (anyNA(tc$length_mm)) stop("[Multistanza age split - MEDITS TC] length_class is missing/non-numeric for ",
                                sum(is.na(tc$length_mm)), " row(s) - cannot weight by biomass.")
  tc[, w_rel := n_individuals * length_mm^lw_b]

  agg <- tc[, .(n = sum(n_individuals), w = sum(w_rel)), by = .(ScientificName, stanza)]
  wide <- dcast(agg, ScientificName ~ stanza, value.var = c("n", "w"), fill = 0)
  for (cc in c("n_Juvenile", "n_Adult", "w_Juvenile", "w_Adult")) if (!cc %in% names(wide)) wide[, (cc) := 0]
  wide[, `:=`(Juvenile = n_Juvenile, Adult = n_Adult)]
  wide[, prop_juvenile_numbers := fifelse((n_Juvenile + n_Adult) > 0, n_Juvenile / (n_Juvenile + n_Adult), NA_real_)]
  wide[, prop_juvenile := fifelse((w_Juvenile + w_Adult) > 0, w_Juvenile / (w_Juvenile + w_Adult), NA_real_)]
  wide[, source_file := paste0("MEDITS TC.csv (maturity stage < ", juvenile_stage_cutoff,
                               " = juvenile; biomass-weighted by length^", lw_b, ")")]
  result <- wide[!is.na(prop_juvenile)]
  if (nrow(result) > 0) {
    message("[Multistanza age split - MEDITS TC] Juvenile proportion by species (maturity stage < ",
            juvenile_stage_cutoff, " = Juvenile; prop_juvenile is BIOMASS-weighted, prop_juvenile_numbers by count):")
    print(result[, .(ScientificName, Juvenile, Adult, prop_juvenile_numbers, prop_juvenile)])
  } else {
    message("[Multistanza age split - MEDITS TC] TC.csv matched but produced no usable juvenile proportion.")
  }
  result
}