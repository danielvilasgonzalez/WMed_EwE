# ============================================================
# Reusable WoRMS taxonomy lookup function
# (World Register of Marine Species)
#
# Used as the SINGLE taxonomy source everywhere in the pipeline,
# so genus/family comparisons are always apples-to-apples - no
# risk of comparing a FishBase-derived family name against a
# WoRMS-derived one for the same actual family.
# ============================================================
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

#function
worms_taxonomy_lookup <- function(names_vector) {
  
  # strip "Genus spp." suffixes so those still resolve at genus level
  search_terms <- str_trim(str_remove(names_vector, "\\s+spp\\.?$"))
  
  chunk_size <- 50
  chunks <- split(search_terms, ceiling(seq_along(search_terms) / chunk_size))
  
  raw_results <- map(chunks, function(chunk) {
    tryCatch(
      wm_records_names(chunk, marine_only = FALSE),
      error = function(e) {
        # SURFACE the real error instead of silently returning NULLs -
        # if every single lookup comes back "not found" including species
        # as common as Abra alba, that's not a real taxonomy result, it's
        # this error being masked
        message("WoRMS API call failed for a chunk of ", length(chunk),
                " names: ", conditionMessage(e))
        rep(list(NULL), length(chunk))
      }
    )
  })
  raw_results <- flatten(raw_results)
  
  taxonomy_lookup <- map2_dfr(raw_results, search_terms, function(res, term) {
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