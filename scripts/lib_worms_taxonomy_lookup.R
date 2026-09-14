## =================================================================
## lib_worms_taxonomy_lookup.R - LIBRARY FILE, not a pipeline step.
## sourced automatically by the numbered pipeline scripts (01-04) -
## do not run this directly, it has no top-level driver code of its own.
## =================================================================

# ============================================================
# Reusable WoRMS taxonomy lookup function
# (World Register of Marine Species)
#
# Used as the SINGLE taxonomy source everywhere in the pipeline,
# so genus/family comparisons are always apples-to-apples - no
# risk of comparing a FishBase-derived family name against a
# WoRMS-derived one for the same actual family.
# ============================================================

#load libraries
required_packages <- c(
  "worrms",
  "dplyr",
  "purrr",
  "stringr"
)
missing_packages <- required_packages[
  !sapply(required_packages, requireNamespace, quietly = TRUE)
]
if (length(missing_packages) > 0) {
  message(
    "Installing missing packages: ",
    paste(missing_packages, collapse = ", ")
  )
  install.packages(missing_packages)
}
invisible(lapply(required_packages, library, character.only = TRUE))

## =================================================================
## fetch_names_robust(): the actual fix for the "(500) Internal Server
## Error - AphiaRecordsByNames" failures that were happening even for
## perfectly ordinary, correctly-spelled species (Eledone cirrhosa,
## Calappa granulata, etc.) - this was never about those species being
## unusual. AphiaRecordsByNames processes an entire batch of names in
## ONE HTTP request, and WoRMS's own production server is well known
## (see the worrms package's own GitHub issues, and general WoRMS API
## discussion) to intermittently 500 on that endpoint - sometimes from
## a single malformed/edge-case name inside an otherwise-fine batch of
## 50 (which takes down the WHOLE batch, common names included, since
## the server errors before it gets to return anything), sometimes
## from plain server-side flakiness/load with no bad name involved at
## all. The original code had no retry and no way to tell which case
## it was in - one 500 meant all 50 names in that chunk were marked
## "failed", permanently, even the 49 that were fine.
##
## This fixes both failure modes without guessing which one occurred:
##   1. Retry the SAME batch up to max_retries times with exponential
##      backoff (2s, 4s, 8s, ...) - covers plain transient/load-related
##      500s, which very often succeed on the 2nd or 3rd attempt.
##   2. If a batch is STILL failing after all retries, split it in
##      half and retry each half independently (recursively, down to
##      individual names if necessary) rather than giving up on the
##      whole thing - isolates a genuinely bad name to just that one
##      name, so it costs you one lookup, not 49 good ones alongside it.
## `terms` may contain NA entries (blank/unqueryable placeholders,
## e.g. a bare qualifier that reduced to "") - these are skipped
## entirely, never sent to the API, and returned as NULL in-place so
## the result vector stays the same length/order as `terms`.
## =================================================================
fetch_names_robust <- function(terms, max_retries = 3) {
  n <- length(terms)
  result <- vector("list", n)
  valid <- which(!is.na(terms) & nzchar(terms))
  if (length(valid) == 0) return(result)
  
  query_terms <- terms[valid]
  last_err <- NULL
  for (attempt in seq_len(max_retries)) {
    res <- tryCatch(wm_records_names(query_terms, marine_only = FALSE), error = function(e) e)
    if (!inherits(res, "error")) {
      result[valid] <- res
      return(result)
    }
    last_err <- res
    if (attempt < max_retries) {
      wait <- 2^attempt   # 2s, 4s, 8s, ... - gives WoRMS's server room to recover
      message("  WoRMS API call failed for a batch of ", length(query_terms),
              " name(s) (attempt ", attempt, "/", max_retries, "): ",
              conditionMessage(res), " - retrying in ", wait, "s.")
      Sys.sleep(wait)
    }
  }
  
  ## still failing after every retry - if there's more than one name
  ## left, split in half and isolate; a single name that still fails
  ## after retries is reported and left NULL (genuinely not
  ## resolvable this run, not silently guessed at).
  if (length(query_terms) > 1) {
    message("  Batch of ", length(query_terms), " still failing after ", max_retries,
            " attempts - splitting in half to isolate which name(s) are the problem",
            " (this is what turns \"49 good names lost alongside 1 bad one\" into",
            " just the 1 bad one failing).")
    mid <- ceiling(length(valid) / 2)
    left  <- valid[seq_len(mid)]
    right <- valid[(mid + 1):length(valid)]
    left_terms  <- rep(NA_character_, n); left_terms[left]  <- terms[left]
    right_terms <- rep(NA_character_, n); right_terms[right] <- terms[right]
    result_left  <- fetch_names_robust(left_terms,  max_retries = max_retries)
    Sys.sleep(1)   # brief pause between the two halves too, not just between top-level chunks
    result_right <- fetch_names_robust(right_terms, max_retries = max_retries)
    for (i in left)  result[[i]] <- result_left[[i]]
    for (i in right) result[[i]] <- result_right[[i]]
  } else {
    message("  WoRMS lookup permanently failed for '", query_terms, "' after ", max_retries,
            " attempt(s): ", conditionMessage(last_err))
  }
  result
}

## =================================================================
## cache_path (optional, default NULL = old behavior, always queries
## WoRMS fresh) - this is what actually fixes "taking forever": WoRMS
## taxonomy for a given ScientificName never changes between runs of
## THIS pipeline in normal development (re-running after a code change
## elsewhere, debugging, re-running with different FILTER_AREAS, etc.),
## so re-querying all ~900+ species from scratch every single run is
## pure waste. When cache_path is set, names already resolved on a
## previous run are read from an on-disk RDS file and skipped entirely
## - only genuinely NEW names (first time seen, or a name that
## previously came back "not found" - see below) go to WoRMS at all.
## A cache hit costs a disk read, not an HTTP round-trip.
##
## "not found" results ARE cached too (not just successful ones) -
## otherwise every run would keep re-querying the same handful of
## always-unresolvable names (typos, non-marine taxa, etc.) forever,
## which is exactly the kind of name most likely to trigger WoRMS's
## flaky 500s in the first place (see fetch_names_robust()'s own
## comment). Delete the cache file (or pass a fresh cache_path) if you
## fix an upstream spelling issue and want a "not found" name re-tried.
## =================================================================
worms_taxonomy_lookup <- function(names_vector, chunk_size = 50, max_retries = 3,
                                  inter_chunk_sleep = 1, cache_path = NULL) {
  
  cache <- if (!is.null(cache_path) && file.exists(cache_path)) {
    readRDS(cache_path)
  } else {
    NULL
  }
  
  already_cached <- if (!is.null(cache)) intersect(names_vector, cache$original_name) else character(0)
  to_query <- setdiff(names_vector, already_cached)
  
  if (!is.null(cache_path)) {
    message("  WoRMS taxonomy cache (", cache_path, "): ", length(already_cached),
            " of ", length(names_vector), " name(s) already cached, ",
            length(to_query), " to actually query.")
  }
  
  fresh_result <- if (length(to_query) > 0) {
    worms_taxonomy_lookup_uncached(to_query, chunk_size = chunk_size,
                                   max_retries = max_retries, inter_chunk_sleep = inter_chunk_sleep)
  } else {
    NULL
  }
  
  if (!is.null(cache_path)) {
    updated_cache <- if (is.null(cache)) fresh_result else if (is.null(fresh_result)) cache else
      dplyr::bind_rows(cache, fresh_result)
    if (!is.null(updated_cache)) {
      dir.create(dirname(cache_path), recursive = TRUE, showWarnings = FALSE)
      saveRDS(updated_cache, cache_path)
    }
  }
  
  ## final result must be in names_vector's own order (including
  ## duplicates, if any) - same contract worms_taxonomy_lookup_uncached()
  ## always had - so pull each row (cached or fresh) back out by name
  ## rather than just returning fresh_result alone.
  all_known <- if (is.null(cache)) fresh_result else if (is.null(fresh_result)) cache else
    dplyr::bind_rows(cache, fresh_result)
  all_known %>% dplyr::slice(match(names_vector, original_name))
}

worms_taxonomy_lookup_uncached <- function(names_vector, chunk_size = 50, max_retries = 3,
                                           inter_chunk_sleep = 1) {
  
  ## Strip common non-taxonomic qualifiers (spp./sp./cf./aff., case-
  ## insensitive) so those still resolve, and NEVER send a literal,
  ## guaranteed-invalid term to the WoRMS API - the original code only
  ## stripped a trailing "spp." (leaving "Genus" for open nomenclature
  ## like "Sepiola spp."). "cf." and "aff." don't behave like "spp." -
  ## they sit BETWEEN genus and species ("Diplodus cf. sargus" =
  ## "resembles D. sargus but not confirmed"), not at the end, so an
  ## end-anchored removal misses them entirely and the literal
  ## "Diplodus cf. sargus" / "Mullus aff. barbatus" still gets sent to
  ## WoRMS, which always rejects it. Removing the qualifier token
  ## wherever it appears (word-boundary-matched, so real species/genus
  ## names like "affinis" are never touched) turns "Diplodus cf.
  ## sargus" into "Diplodus sargus" and "Sepiola spp." into "Sepiola" -
  ## both then resolve normally. Anything that reduces to an empty
  ## string is blanked to NA and skipped entirely rather than queried
  ## as "" - an empty/blank name inside a batch request is itself a
  ## plausible cause of the WHOLE batch 500ing (WoRMS processes every
  ## name in one request), not just a wasted lookup.
  search_terms <- str_squish(str_remove_all(names_vector, "(?i)\\b(spp?|cf|aff)\\.?(?=\\s|$)"))
  search_terms[!nzchar(search_terms)] <- NA_character_
  
  chunk_idx <- split(seq_along(search_terms), ceiling(seq_along(search_terms) / chunk_size))
  
  ## Namespaced explicitly (purrr::map, not bare map()) - deliberately
  ## NOT left unqualified: the "maps" package (used elsewhere in this
  ## pipeline for basemap plotting) ALSO exports a function called
  ## map(), with a completely different signature (map(database, ...)
  ## for drawing geographic maps, vs purrr's map(list, function) for
  ## list iteration). Whichever of the two was attached most recently
  ## wins an unqualified call - fragile and silently wrong when it
  ## picks maps::map() instead, which is exactly what happened here
  ## once "maps" was added to the calling script's package list.
  ## Progress message per chunk - without this, a long fallback (many
  ## hundreds of species, each chunk taking a few seconds plus whatever
  ## retry/backoff time a flaky 500 costs) prints NOTHING between the
  ## opening "N distinct species..." message and completion, which
  ## looks identical to a hung process from the outside. This alone
  ## doesn't make it faster, but it's the difference between "is this
  ## stuck?" and "it's on chunk 12 of 19, working normally."
  message("  Querying WoRMS: ", length(chunk_idx), " chunk(s) of up to ", chunk_size,
          " name(s) each (", length(search_terms), " total).")
  raw_results <- vector("list", length(search_terms))
  for (k in seq_along(chunk_idx)) {
    idx <- chunk_idx[[k]]
    t0 <- Sys.time()
    raw_results[idx] <- fetch_names_robust(search_terms[idx], max_retries = max_retries)
    message("    chunk ", k, "/", length(chunk_idx), " (", length(idx), " name(s)) done in ",
            round(as.numeric(difftime(Sys.time(), t0, units = "secs")), 1), "s.")
    if (k < length(chunk_idx)) Sys.sleep(inter_chunk_sleep)   # space out batches - see fetch_names_robust()'s own note
  }
  
  taxonomy_lookup <- purrr::map2_dfr(raw_results, search_terms, function(res, term) {
    if (is.null(res) || nrow(res) == 0) {
      return(tibble(
        search_term = term, AphiaID = NA_integer_, matched_name = NA_character_,
        rank = NA_character_, status = "not found",
        phylum = NA_character_, class = NA_character_, order = NA_character_,
        family = NA_character_, genus = NA_character_
      ))
    }
    best <- res %>% arrange(status != "accepted") %>% slice(1)
    safe_col <- function(col) if (col %in% names(best)) best[[col]] else NA_character_
    tibble(
      search_term = term, AphiaID = best$AphiaID, matched_name = best$scientificname,
      rank = best$rank, status = best$status,
      phylum = safe_col("phylum"), class = safe_col("class"), order = safe_col("order"),
      family = safe_col("family"), genus = safe_col("genus")
    )
  })
  
  taxonomy_lookup %>% mutate(original_name = names_vector, .before = 1)
}