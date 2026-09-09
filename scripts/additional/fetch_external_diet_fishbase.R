#!/usr/bin/env Rscript
# ==========================================================================
# fetch_external_diet_fishbase.R
# ==========================================================================
#
# Pulls diet-composition data straight from FishBase (finfish) and/or
# SeaLifeBase (everything else -- invertebrates, marine mammals, reptiles,
# etc.) via the `rfishbase` R package, reshapes it with the `dietr`
# package's FishBase converters, maps predators/prey onto your West Med EwE
# functional groups (same species_to_fg_weight.csv used by
# diet_to_ewe.R), and writes a ready-to-use "external diet source" CSV --
# i.e. exactly the (predator_group, prey_group, proportion) format that
# diet_to_ewe.R's --external flag expects, so its output plugs straight
# into the main pipeline as a second diet source alongside your local
# stomach-content database.
#
# --------------------------------------------------------------------------
# WHY TWO PACKAGES
# --------------------------------------------------------------------------
# `rfishbase` (ropensci) is the low-level interface to FishBase/SeaLifeBase.
# Its diet-related tables are split across two functions -- diet() (one row
# per published diet *study*) and diet_items() (one row per prey item
# within a study) -- that you're expected to join yourself, and the
# packaged docs don't pin down every column name.
#
# `dietr` (Borstein) ships purpose-built converters --
# ConvertFishbaseDiet() and ConvertFishbaseFood() -- that do that join and
# hand back a stable, documented long-format table: one row per
# (predator species, prey taxon, life stage) with a DietPercent column.
# That's the well-documented interface, so this script builds on dietr
# rather than hand-rolling the diet()/diet_items() join.
#
# Two FishBase/SeaLifeBase diet sources exist and this script uses both:
#   - "diet" studies: quantitative (%diet by weight/volume/number) --
#     the good stuff, used directly as a proportion.
#   - "food items" lists: many more species only have a *ranked* list of
#     food items (no quantities). These are converted to an approximate
#     proportion by rank (heavier weight for lower rank number) and tagged
#     with source "fishbase_ranked"/"sealifebase_ranked" in the output so
#     you can see at a glance which numbers are real percentages and which
#     are a rank-based approximation -- treat the latter as lower
#     confidence.
#
# --------------------------------------------------------------------------
# REQUIREMENTS
# --------------------------------------------------------------------------
#   install.packages(c("rfishbase", "dietr", "dplyr", "readr"))
#
# rfishbase downloads its (large-ish) reference tables from Source
# Cooperative on first use and caches them locally (see
# rfishbase::available_releases() / ?rfishbase::fb_tbl) -- the first run
# needs network access and can take a while; subsequent runs reuse the
# local cache.
#
# NOTE: this script was written and syntax-checked without network access
# to CRAN/FishBase, so column names inside FishBase/SeaLifeBase's own
# tables couldn't be executed against live data here. dietr's
# ConvertFishbaseDiet()/ConvertFishbaseFood() output columns (Species,
# FoodI/FoodII/FoodIII, Stage, DietPercent / FoodItem-rank) are documented
# and stable across versions, so this script is built on those rather than
# the raw rfishbase tables. The first time you run it, check the printed
# sample rows (`fetch_fishbase_diet()` prints a preview) against what you
# expect before trusting the output wholesale.
#
# --------------------------------------------------------------------------
# USAGE
# --------------------------------------------------------------------------
#   Rscript fetch_external_diet_fishbase.R \
#     --species "Merluccius merluccius,Mullus barbatus,Delphinus delphis" \
#     --mapping species_to_fg_weight.csv \
#     --out external_diet_fishbase.csv
#
# Species that FishBase doesn't know (fish) are automatically retried
# against SeaLifeBase (invertebrates, marine mammals, reptiles...), so you
# can pass one mixed species list and not worry about which database each
# species lives in.
#
# Then feed the result straight into the main pipeline:
#   Rscript diet_to_ewe.R --stomach_csv ... --mapping species_to_fg_weight.csv \
#     --external external_diet_fishbase.csv:FishBase:0.5 \
#     --out ewe_diet_composition.xlsx
# ==========================================================================

suppressMessages({
  library(dplyr)
  library(readr)
})

# Rank-based pseudo-proportion for food-item lists that have no quantities:
# rank 1 (most important prey) gets the most weight, decaying geometrically.
# Purely a way to turn "these are the top N prey items, in order" into
# something normalisable -- treat it as a rough steer, not a real
# percentage. Change RANK_DECAY (0-1, closer to 1 = flatter) to taste.
RANK_DECAY <- 0.6


# ==========================================================================
# FETCHING
# ==========================================================================

#' Try FishBase first, then SeaLifeBase, for whichever species aren't found
#' on FishBase (e.g. crustaceans, cephalopods, marine mammals -- FishBase is
#' finfish-only). Returns a named list with one element per server actually
#' queried, `$fishbase` and/or `$sealifebase`, each the raw dietr
#' ConvertFishbaseDiet() output for that server (list of DietItems/Taxonomy
#' data frames), already filtered down to `species_list`.
fetch_fishbase_diet_raw <- function(species_list, exclude_stage = NULL) {
  if (!requireNamespace("rfishbase", quietly = TRUE) || !requireNamespace("dietr", quietly = TRUE)) {
    stop("Install rfishbase and dietr first: install.packages(c('rfishbase', 'dietr'))")
  }
  
  results <- list()
  
  fetch_one_server <- function(server) {
    cat(sprintf("[fetch_fishbase_diet_raw] Querying %s for diet data (all species, then filtering "
                %+% "-- ConvertFishbaseDiet() doesn't take a species filter itself)...\n", server))
    # dietr::ConvertFishbaseDiet() has changed signature across versions --
    # some accept `server`, older ones default to fishbase only. Pass it
    # through when supported, otherwise fall back and warn.
    has_server_arg <- "server" %in% names(formals(dietr::ConvertFishbaseDiet))
    converted <- if (has_server_arg) {
      dietr::ConvertFishbaseDiet(ExcludeStage = exclude_stage, server = server)
    } else {
      if (server != "fishbase") {
        warning(
          "Installed dietr::ConvertFishbaseDiet() has no `server` argument -- it can only ",
          "reach FishBase, not SeaLifeBase, in this version. Update dietr, or fetch SeaLifeBase ",
          "diet data manually via rfishbase::diet(species_list, server = 'sealifebase') / ",
          "rfishbase::diet_items(...) and join them yourself."
        )
        return(NULL)
      }
      dietr::ConvertFishbaseDiet(ExcludeStage = exclude_stage)
    }
    
    items <- converted$DietItems
    items <- items[tolower(trimws(items$Species)) %in% tolower(trimws(species_list)), , drop = FALSE]
    items
  }
  
  fb <- tryCatch(fetch_one_server("fishbase"), error = function(e) {
    warning("FishBase diet fetch failed: ", conditionMessage(e)); NULL
  })
  if (!is.null(fb) && nrow(fb) > 0) results$fishbase <- fb
  
  found_species <- if (!is.null(fb)) unique(tolower(trimws(fb$Species))) else character(0)
  missing_species <- species_list[!(tolower(trimws(species_list)) %in% found_species)]
  
  if (length(missing_species) > 0) {
    slb <- tryCatch(fetch_one_server("sealifebase"), error = function(e) {
      warning("SeaLifeBase diet fetch failed: ", conditionMessage(e)); NULL
    })
    if (!is.null(slb) && nrow(slb) > 0) {
      slb <- slb[tolower(trimws(slb$Species)) %in% tolower(trimws(missing_species)), , drop = FALSE]
      if (nrow(slb) > 0) results$sealifebase <- slb
    }
  }
  
  results
}

# small helper since base R has no built-in string-paste operator
`%+%` <- function(a, b) paste0(a, b)

#' Quantitative diet studies (DietPercent-based), both servers, tagged with
#' which server each row came from. Long format:
#'   predator_species, prey_taxon, proportion, source
fetch_fishbase_diet <- function(species_list, exclude_stage = NULL) {
  raw <- fetch_fishbase_diet_raw(species_list, exclude_stage = exclude_stage)
  if (length(raw) == 0) {
    warning("No quantitative FishBase/SeaLifeBase diet studies found for the given species list.")
    return(data.frame(predator_species = character(0), prey_taxon = character(0),
                      proportion = double(0), source = character(0)))
  }
  
  rows <- lapply(names(raw), function(server) {
    df <- raw[[server]]
    df$prey_taxon <- most_specific_taxon(df$FoodI, df$FoodII, df$FoodIII)
    df$proportion <- as.numeric(df$DietPercent) / 100  # FishBase DietPercent is 0-100
    df$source <- server
    df %>%
      transmute(predator_species = Species, prey_taxon, proportion, source) %>%
      filter(!is.na(proportion), !is.na(prey_taxon), prey_taxon != "")
  })
  
  bound <- bind_rows(rows)
  cat("[fetch_fishbase_diet] preview of first rows fetched -- sanity-check this against what you expect:\n")
  print(utils::head(bound, 10))
  bound
}

#' Ranked (non-quantitative) food-item lists, converted to an approximate
#' proportion by rank within each (species, source) group. Same long format
#' as fetch_fishbase_diet(), source tagged "*_ranked" so it's visibly lower
#' confidence than real percentages.
fetch_fishbase_fooditems <- function(species_list) {
  if (!requireNamespace("rfishbase", quietly = TRUE) || !requireNamespace("dietr", quietly = TRUE)) {
    stop("Install rfishbase and dietr first: install.packages(c('rfishbase', 'dietr'))")
  }
  
  fetch_one_server <- function(server) {
    raw <- tryCatch(
      as.data.frame(rfishbase::fooditems(species_list, server = server)),
      error = function(e) {
        warning(sprintf("%s fooditems() fetch failed: %s", server, conditionMessage(e))); NULL
      }
    )
    if (is.null(raw) || nrow(raw) == 0) return(NULL)
    converted <- tryCatch(dietr::ConvertFishbaseFood(FishBaseFood = raw), error = function(e) {
      warning(sprintf("ConvertFishbaseFood() failed for %s: %s", server, conditionMessage(e))); NULL
    })
    if (is.null(converted)) return(NULL)
    items <- converted$FoodItems
    if (is.null(items) || nrow(items) == 0) return(NULL)
    items$server <- server
    items
  }
  
  fb <- fetch_one_server("fishbase")
  found <- if (!is.null(fb)) unique(tolower(trimws(fb$Species))) else character(0)
  missing <- species_list[!(tolower(trimws(species_list)) %in% found)]
  slb <- if (length(missing) > 0) fetch_one_server("sealifebase") else NULL
  
  raw_items <- bind_rows(fb, slb)
  if (nrow(raw_items) == 0) {
    warning("No FishBase/SeaLifeBase ranked food-item lists found for the given species list either.")
    return(data.frame(predator_species = character(0), prey_taxon = character(0),
                      proportion = double(0), source = character(0)))
  }
  
  raw_items$prey_taxon <- most_specific_taxon(raw_items$FoodI, raw_items$FoodII, raw_items$FoodIII)
  
  # rfishbase/dietr food-item output doesn't carry an explicit rank column
  # in every version -- if one exists (commonly named "FoodSeq" or
  # "Rank"), use it; otherwise fall back to row order within each species
  # as a proxy for the order FishBase listed them in.
  rank_col <- intersect(c("FoodSeq", "Rank", "Seq"), names(raw_items))
  raw_items <- raw_items %>%
    group_by(Species, server) %>%
    mutate(.rank = if (length(rank_col) > 0) as.numeric(.data[[rank_col[1]]]) else row_number()) %>%
    ungroup()
  
  raw_items %>%
    filter(!is.na(prey_taxon), prey_taxon != "") %>%
    group_by(Species, server) %>%
    mutate(weight = RANK_DECAY ^ (.rank - 1), proportion = weight / sum(weight)) %>%
    ungroup() %>%
    transmute(predator_species = Species, prey_taxon,
              proportion, source = paste0(server, "_ranked"))
}

#' Pick the most specific (rightmost non-NA/non-empty) of FoodI/FoodII/FoodIII
#' as a single prey taxon name to map against your species_to_fg_weight.csv.
#' FishBase's diet categories are hierarchical (e.g. FoodI="Crustacea",
#' FoodII="Decapoda", FoodIII="Penaeidae") -- if you'd rather map at a
#' coarser level, change which column this picks.
most_specific_taxon <- function(food_i, food_ii, food_iii) {
  clean <- function(x) ifelse(is.na(x) | trimws(x) == "", NA_character_, trimws(x))
  fi <- clean(food_i); fii <- clean(food_ii); fiii <- clean(food_iii)
  dplyr::coalesce(fiii, fii, fi)
}


# ==========================================================================
# MAP TO EwE GROUPS + BUILD EXTERNAL-SOURCE CSV
# ==========================================================================

#' Full pipeline: fetch quantitative diet studies, fall back to ranked food
#' items for species with no quantitative data, map predator species + prey
#' taxa onto EwE groups via the same species_to_fg_weight.csv used by
#' diet_to_ewe.R, aggregate to (predator_group, prey_group, proportion),
#' and write it out ready for diet_to_ewe.R's --external flag.
build_external_diet_from_fishbase <- function(species_list, mapping_path, out_path,
                                              exclude_stage = NULL, prefer_quantitative = TRUE) {
  # species_to_fg_weight.csv: species, fg_name, proportion -- same weighted
  # mapping used by diet_to_ewe.R. A species may appear on more than one row
  # (split across fg's, e.g. by size stanza); every record for that species
  # is then expanded across all of its fg rows, scaled by `proportion`.
  mapping <- read_csv(mapping_path, show_col_types = FALSE, comment = "#")
  if (!all(c("species", "fg_name", "proportion") %in% names(mapping))) {
    stop("Mapping file must have columns species, fg_name, proportion")
  }
  mapping <- mapping %>% transmute(key = tolower(trimws(species)), fg_name, weight = as.numeric(proportion))
  
  quant <- fetch_fishbase_diet(species_list, exclude_stage = exclude_stage)
  
  species_with_quant <- unique(tolower(trimws(quant$predator_species)))
  species_needing_ranked <- if (prefer_quantitative) {
    species_list[!(tolower(trimws(species_list)) %in% species_with_quant)]
  } else {
    species_list
  }
  
  ranked <- if (length(species_needing_ranked) > 0) {
    fetch_fishbase_fooditems(species_needing_ranked)
  } else {
    quant[0, ]
  }
  
  combined <- bind_rows(quant, ranked)
  if (nrow(combined) == 0) {
    stop("No diet data (quantitative or ranked) found on FishBase/SeaLifeBase for any species in the list.")
  }
  
  combined$.pred_key <- tolower(trimws(combined$predator_species))
  combined$.prey_key <- tolower(trimws(combined$prey_taxon))
  
  unmapped_pred <- sort(unique(combined$predator_species[!(combined$.pred_key %in% mapping$key)]))
  unmapped_prey <- sort(unique(combined$prey_taxon[!(combined$.prey_key %in% mapping$key)]))
  if (length(unmapped_pred) > 0) {
    cat("\nPredator species from FishBase/SeaLifeBase NOT in your mapping table (dropped):\n")
    for (s in unmapped_pred) cat("  -", s, "\n")
  }
  if (length(unmapped_prey) > 0) {
    cat("\nPrey taxa from FishBase/SeaLifeBase NOT in your mapping table (dropped) -- these are\n",
        "FishBase's own diet categories (e.g. 'Crustacea', 'Teleostei'), so you'll likely need to\n",
        "add rows for them (not just species names) to species_to_fg_weight.csv:\n", sep = "")
    for (s in unmapped_prey) cat("  -", s, "\n")
  }
  
  # expand each record across every (predator fg, prey fg) combination
  # implied by the predator's and prey's species-to-fg splits, scaling the
  # original proportion by both weights -- same logic as diet_to_ewe.R's
  # map_to_ewe_groups().
  pred_split <- combined %>%
    inner_join(mapping %>% rename(predator_group = fg_name, predator_weight = weight),
               by = c(".pred_key" = "key"), relationship = "many-to-many")
  mapped <- pred_split %>%
    inner_join(mapping %>% rename(prey_group = fg_name, prey_weight = weight),
               by = c(".prey_key" = "key"), relationship = "many-to-many") %>%
    mutate(proportion = proportion * predator_weight * prey_weight)
  
  if (nrow(mapped) == 0) {
    stop("Every fetched record was dropped for lack of a mapping -- extend species_to_fg_weight.csv ",
         "with the species/taxa listed above and re-run.")
  }
  
  result <- mapped %>%
    group_by(predator_group, prey_group) %>%
    summarise(proportion = mean(proportion), .groups = "drop") %>%
    group_by(predator_group) %>%
    mutate(proportion = proportion / sum(proportion)) %>%
    ungroup()
  
  write_csv(result, out_path)
  cat(sprintf("\nWrote %d predator/prey rows -> %s\n", nrow(result), out_path))
  cat("Feed this into diet_to_ewe.R as, e.g.:\n")
  cat(sprintf("  --external %s:FishBase:0.5\n", out_path))
  
  invisible(result)
}


# ==========================================================================
# CLI
# ==========================================================================

if (identical(environment(), globalenv()) && sys.nframe() == 0L) {
  suppressMessages(library(optparse))
  
  option_list <- list(
    make_option("--species", type = "character", default = NULL,
                help = "Comma-separated list of predator+prey species scientific names to fetch, e.g. "
                %+% "'Merluccius merluccius,Delphinus delphis'"),
    make_option("--mapping", type = "character", default = NULL,
                help = "CSV mapping species_name -> ewe_group (same file used by diet_to_ewe.R)"),
    make_option("--out", type = "character", default = "external_diet_fishbase.csv"),
    make_option("--exclude_stage", type = "character", default = NULL,
                help = "Comma-separated life stages to exclude, e.g. 'larvae,rec./juveniles'")
  )
  opt <- parse_args(OptionParser(option_list = option_list))
  
  if (is.null(opt$species) || is.null(opt$mapping)) {
    stop("--species and --mapping are required. Example:\n",
         "  Rscript fetch_external_diet_fishbase.R --species \"Merluccius merluccius,Delphinus delphis\" ",
         "--mapping species_to_fg_weight.csv --out external_diet_fishbase.csv")
  }
  
  species_list <- trimws(strsplit(opt$species, ",")[[1]])
  exclude_stage <- if (!is.null(opt$exclude_stage)) trimws(strsplit(opt$exclude_stage, ",")[[1]]) else NULL
  
  build_external_diet_from_fishbase(
    species_list = species_list, mapping_path = opt$mapping, out_path = opt$out,
    exclude_stage = exclude_stage
  )
}