## =================================================================
## PB and QB estimation for EwE Functional Groups - FULLY AUTOMATED
## Taxon-specific methods per: "Quick guide on how to calculate P/B
## and Q/B for EwE models" - Vilas, Coll, Piroddi, Steenbeek
##
## INPUT: species_df with:
##   Species  - scientific name (REQUIRED)
##   FG       - functional group id (REQUIRED)
##   Biomass  - species biomass DENSITY, t/km^2 (REQUIRED)
##   Yield    - OPTIONAL, species catch DENSITY, t/km^2/year - MUST be
##              in the SAME area-normalized units as Biomass (both
##              per km^2) for F = Yield/Biomass to be a valid rate.
##              Mixing an absolute total catch with a density Biomass
##              would silently produce a meaningless F - if you don't
##              have a true density for Yield, leave it NA rather than
##              guess a conversion.
##
## Everything else (growth params, a/b, maturity, body weight,
## longevity, trophic level, temperature, depth, aspect ratio) is
## fetched automatically from FishBase + SeaLifeBase.
## =================================================================

library(data.table)
library(stringr)
library(ggplot2)
if (!requireNamespace("progress", quietly = TRUE)) install.packages("progress")
library(progress)
if (!requireNamespace("rfishbase", quietly = TRUE)) install.packages("rfishbase")
library(rfishbase)

## --- Load species_df (built and saved by test_species_df.R, or your
## own real data saved the same way) --------------------------------
PIPELINE_START_TIME <- Sys.time()

## Top-level stage tracker - covers the WHOLE pipeline, not just the
## per-species loops (which have their own finer-grained progress bars
## already). Call STAGE_PB$tick(tokens=list(stage_name="...")) once
## each labeled stage below completes.
PIPELINE_STAGES <- c(
  "Load species_df", "Taxonomic classification (WoRMS)",
  "Fetch 2a: species() traits", "Fetch 2b: growth params",
  "Fetch 2c: length-weight a/b", "Fetch 2d: maturity",
  "Fetch 2e: ecology/trophic level", "Fetch 2f: swimming/aspect ratio",
  "Assemble + derive traits (Froese-Binohlan)", "Raw-trait gap filling",
  "Calculate PB/QB (all groups)", "Aggregate to FG level",
  "Export CSVs + generate plots"
)
STAGE_PB <- progress_bar$new(
  format = "PIPELINE [:bar] :percent | Stage :current/:total: :stage_name | Elapsed: :elapsedfull",
  total = length(PIPELINE_STAGES), clear = FALSE, width = 100
)
message(strrep("=", 70))
message("Starting pipeline - ", length(PIPELINE_STAGES), " stages")
message(strrep("=", 70))

SPECIES_DF_PATH <- "/Users/daniel/Documents/GitHub/WMed_EwE/data/processed/test_species_df.rds"
species_df <- readRDS(SPECIES_DF_PATH)

setDT(species_df)
stopifnot(all(c("Species", "FG", "Biomass") %in% names(species_df)))
if (!"Yield" %in% names(species_df)) species_df[, Yield := NA_real_]
sp_list <- unique(species_df$Species)
STAGE_PB$tick(tokens = list(stage_name = "Load species_df"))

## =================================================================
## STEP 1: taxonomic classification -> dispatch group
## Primary signal: WoRMS Class. Fallback signal for species where
## WoRMS returns no Class at all: whether the species has ANY
## FishBase record (fish-specific database) vs only SeaLifeBase -
## more robust than relying on Class string-matching alone.
## =================================================================

source('/Users/daniel/Documents/GitHub/WMed_EwE/scripts/worms_taxonomy_lookup.R')

taxonomy <- as.data.table(worms_taxonomy_lookup(sp_list))[
  , .(Species = original_name, Genus = genus, Family = family, Order = order, Class = class, Phylum = phylum)
]

FISH_CLASSES <- c("Teleostei", "Elasmobranchii", "Chondrichthyes", "Actinopteri",
                  "Actinopterygii", "Myxini", "Petromyzonti", "Holocephali")
MAMMAL_CLASSES <- c("Mammalia")
BIRD_CLASSES <- c("Aves")
PHYTO_CLASSES <- c("Bacillariophyceae", "Dinophyceae", "Cyanophyceae")

## reusable so the same logic applies both to the initial lookup and
## after any synonym-resolution fills in more Class values below
classify_dispatch <- function(class_vec) {
  fifelse(class_vec %in% FISH_CLASSES, "fish", fifelse(
    class_vec %in% MAMMAL_CLASSES, "mammal", fifelse(
      class_vec %in% BIRD_CLASSES, "seabird", fifelse(
        class_vec %in% PHYTO_CLASSES, "phytoplankton", NA_character_
      ))))
}

taxonomy[, dispatch_group := classify_dispatch(Class)]

## --- Fallback for species with no Class: the usual cause is that the
## name is a SYNONYM - WoRMS records for synonym/unaccepted names often
## have an incomplete classification chain even though the full chain
## exists under the ACCEPTED name. Follow that resolution within WoRMS
## itself (synonym -> valid_AphiaID -> accepted record's classification)
## rather than falling back to an unrelated database as an indirect proxy.
unresolved <- taxonomy[is.na(dispatch_group), Species]
if (length(unresolved) > 0) {
  message("\n", length(unresolved), " species had no WoRMS Class from the initial lookup -",
          " attempting synonym -> accepted-name resolution via WoRMS.")
  
  resolve_via_accepted_name <- function(sp) {
    rec <- tryCatch(worrms::wm_records_names(sp, marine_only = FALSE)[[1]], error = function(e) NULL)
    if (is.null(rec) || nrow(rec) == 0) return(NULL)
    rec <- rec[1, ]
    ## if this is a synonym/unaccepted record with its own classification
    ## missing, re-query WoRMS directly by the accepted name's AphiaID
    if (!is.na(rec$valid_AphiaID) && rec$valid_AphiaID != rec$AphiaID && is.na(rec$class)) {
      accepted <- tryCatch(worrms::wm_record(id = rec$valid_AphiaID), error = function(e) NULL)
      if (!is.null(accepted) && nrow(accepted) > 0) rec <- accepted[1, ]
    }
    data.table(Species = sp, Genus_r = rec$genus, Family_r = rec$family,
               Order_r = rec$order, Class_r = rec$class, Phylum_r = rec$phylum)
  }
  
  resolved <- rbindlist(lapply(unresolved, resolve_via_accepted_name), fill = TRUE)
  
  if (nrow(resolved) > 0) {
    taxonomy <- merge(taxonomy, resolved, by = "Species", all.x = TRUE)
    for (col in c("Genus", "Family", "Order", "Class", "Phylum")) {
      resolved_col <- paste0(col, "_r")
      taxonomy[is.na(get(col)), (col) := get(resolved_col)]
      taxonomy[, (resolved_col) := NULL]
    }
    taxonomy[is.na(dispatch_group), dispatch_group := classify_dispatch(Class)]
  }
  
  ## whatever's still unresolved after a genuine WoRMS attempt defaults
  ## to invertebrate as a taxonomically-neutral fallback, not a fish/
  ## non-fish guess borrowed from an unrelated database
  still_unresolved <- taxonomy[is.na(dispatch_group), Species]
  taxonomy[Species %in% still_unresolved, dispatch_group := "invertebrate"]
  
  message(length(unresolved) - length(still_unresolved), " resolved via WoRMS synonym lookup, ",
          length(still_unresolved), " still fully unresolved (defaulted to invertebrate):")
  if (length(still_unresolved) > 0) print(still_unresolved)
}

message("\nUnclassified/unusual Class values that fell through to a fallback",
        " (audit these - may reveal a fish/mammal class not yet in the lists above):")
print(unique(taxonomy[is.na(Class) | !Class %in% c(FISH_CLASSES, MAMMAL_CLASSES, BIRD_CLASSES, PHYTO_CLASSES),
                      .(Species, Class, dispatch_group)]))

species_df <- merge(species_df, taxonomy, by = "Species", all.x = TRUE)

message("\nDispatch group counts:")
print(species_df[, .N, by = dispatch_group])
STAGE_PB$tick(tokens = list(stage_name = "Taxonomic classification (WoRMS)"))

## =================================================================
## HELPER: pick the best row per species from a multi-record FishBase/
## SeaLifeBase table (popgrowth, poplw, maturity all have this shape -
## multiple population-specific studies per species). Priority, tiered:
##   1. Western Mediterranean locality keywords, if any row matches
##   2. Mediterranean generally (broader), if no Western Med row exists
##   3. Most recent Year among whatever tier was selected
##   4. First available, if neither locality nor year exists
## =================================================================

## Keywords for the WESTERN Mediterranean specifically - country/region
## names commonly appearing in FishBase/SeaLifeBase locality fields for
## GSA 1-11 area studies. Extend this list if you notice a relevant
## study getting missed (e.g. a specific bay/coast name).
WMED_KEYWORDS <- c("western mediterranean", "spain", "spanish", "balearic",
                   "catalonia", "catalan", "gulf of lion", "golfe du lion",
                   "france", "french mediterranean", "alboran", "ligurian",
                   "tyrrhenian", "sardinia", "corsica", "algeria", "tunisia",
                   "gsa 1", "gsa 2", "gsa 5", "gsa 6", "gsa 7", "gsa 9", "gsa 10", "gsa 11")
MED_KEYWORDS <- c("mediterran")  # broader fallback tier - any Mediterranean mention

select_best_rows <- function(dt) {
  if (nrow(dt) == 0) return(dt)
  loc_col  <- intersect(c("Locality", "Country", "Loc"), names(dt))[1]
  year_col <- intersect(c("Year", "YearStart"), names(dt))[1]
  
  loc_lower <- if (!is.na(loc_col)) str_to_lower(dt[[loc_col]]) else rep(NA_character_, nrow(dt))
  dt[, .is_wmed := str_detect(loc_lower, paste(WMED_KEYWORDS, collapse = "|"))]
  dt[, .is_med  := str_detect(loc_lower, paste(MED_KEYWORDS, collapse = "|"))]
  dt[is.na(.is_wmed), .is_wmed := FALSE]
  dt[is.na(.is_med), .is_med := FALSE]
  
  pick_one <- function(sub) {
    if (any(sub$.is_wmed)) sub <- sub[.is_wmed == TRUE]          # tier 1: Western Med
    else if (any(sub$.is_med)) sub <- sub[.is_med == TRUE]        # tier 2: Mediterranean generally
    if (!is.na(year_col) && any(!is.na(sub[[year_col]]))) {
      sub <- sub[order(-get(year_col))]                            # tier 3: most recent
    }
    sub[1]
  }
  
  split(dt, by = "Species", keep.by = TRUE) |> lapply(pick_one) |> rbindlist(fill = TRUE)
}

## =================================================================
## Fetch species-INTRINSIC traits (growth, weight, longevity, trophic
## level, aspect ratio) for an arbitrary external species list - reuses
## the exact same fetch infrastructure as Step 2, just applied to
## taxonomic donor candidates instead of your modeled species list.
## =================================================================

fetch_intrinsic_traits <- function(candidates) {
  if (length(candidates) == 0) return(data.table(Species = character()))
  
  growth_c <- select_best_rows(fetch_both(popgrowth, candidates))
  gt <- if (nrow(growth_c) > 0) growth_c[, .(
    Species,
    Loo  = if ("Loo" %in% names(growth_c)) Loo else NA_real_,
    K    = if ("K" %in% names(growth_c)) K else NA_real_,
    Winf = if ("Winfinity" %in% names(growth_c)) Winfinity else NA_real_,
    tmax = if ("tmax" %in% names(growth_c)) tmax else NA_real_
  )] else data.table(Species = character())
  
  ecol_c <- fetch_both(ecology, candidates)
  tl_col_c <- if (nrow(ecol_c) > 0) intersect(c("DietTroph", "FoodTroph"), names(ecol_c))[1] else NA
  et <- if (nrow(ecol_c) > 0) unique(ecol_c[, .(
    Species, TrophicLevel = if (!is.na(tl_col_c)) get(tl_col_c) else NA_real_
  )], by = "Species") else data.table(Species = character())
  
  sp_c <- fetch_both(rfishbase::species, candidates)
  wcol <- intersect(c("Weight", "WeightMax"), names(sp_c))[1]
  lcol <- intersect(c("LongevityWild", "LongevityCaptive", "MaxAge"), names(sp_c))[1]
  st <- if (nrow(sp_c) > 0) unique(sp_c[, .(
    Species, MaxWeight = if (!is.na(wcol)) get(wcol) else NA_real_,
    Longevity = if (!is.na(lcol)) get(lcol) else NA_real_
  )], by = "Species") else data.table(Species = character())
  
  swim_c <- fetch_both(swimming, candidates)
  arcol <- if (nrow(swim_c) > 0) intersect(c("AspectRatio", "Aspect"), names(swim_c))[1] else NA
  swt <- if (nrow(swim_c) > 0) unique(swim_c[, .(
    Species, AspectRatio = if (!is.na(arcol)) get(arcol) else NA_real_
  )], by = "Species") else data.table(Species = character())
  
  Reduce(function(x, y) merge(x, y, by = "Species", all.x = TRUE),
         list(data.table(Species = candidates), gt, et, st, swt))
}

## Registry of formulas expressible purely from species-intrinsic
## traits (no dependency on this model's own Biomass/Yield) - these are
## the only methods that can legitimately be extended to EXTERNAL
## donor species not present in your species_df. Tumbiolo & Downing's
## invertebrate PB is deliberately excluded: it needs THIS model's own
## Biomass as an input, which an external, non-modeled species simply
## doesn't have - that one stays local-donor-only.
EXTERNAL_FORMULA_REGISTRY <- list(
  M_Pauly_1980  = function(d) 10^(-0.0066 - 0.279 * log10(d$Loo) + 0.6543 * log10(d$K) + 0.4634 * log10(DEFAULT_TEMP)),
  M_Gascuel_2008 = function(d) 2.31 * d$TrophicLevel^(-1.72) * exp(0.053 * DEFAULT_TEMP),
  PB_Gascuel_2008 = function(d) 20.19 * d$TrophicLevel^(-3.26) * exp(0.041 * DEFAULT_TEMP),
  QB_PalomaresPauly_1998noZ = function(d) {
    Tprime <- 1000 / (DEFAULT_TEMP + 273.15)
    10^(7.964 - 0.204 * log10(d$Winf) - 1.965 * Tprime + 0.083 * d$AspectRatio)
  },
  QB_ChristensenPauly_1992 = function(d) {
    Tprime <- 1000 / (DEFAULT_TEMP + 273.15)
    10^(6.37 - 1.5045 * Tprime - 0.168 * log10(d$Winf) + 0.1399)
  },
  QB_InnesTrites_1997 = function(d) { w_kg <- d$MaxWeight / 1000; (0.1 * w_kg^0.8 / w_kg) * 365 },
  QB_NilssonNilsson_1976 = function(d) (10^(-0.293 + 0.85 * log10(d$MaxWeight)) / d$MaxWeight) * 365,
  
  ## Identity mappings for RAW traits - lets the same fill mechanism
  ## backfill Winf/TrophicLevel/MaxWeight/AspectRatio themselves,
  ## before any derived formula even runs. This matters because a
  ## missing Winf currently blanks THREE different QB methods at once
  ## (eq26, eq24, and indirectly eq27 via Winf) - filling the trait
  ## once, at the root, is more effective than patching each derived
  ## result separately.
  Winf = function(d) d$Winf,
  TrophicLevel = function(d) d$TrophicLevel,
  MaxWeight = function(d) d$MaxWeight,
  AspectRatio = function(d) d$AspectRatio,
  Longevity = function(d) d$Longevity
)

## M_Hoenig_1983/M_Then_2015/M_AlversonCarney_1975 use TropFishR rather than a simple
## closed-form expression - handled separately since they need the
## fetch_tropfishr_M() call, not a formula lookup
TROPFISHR_EXTERNAL_METHODS <- c("M_Hoenig_1983", "M_Then_2015", "M_AlversonCarney_1975")

## =================================================================
## Fill gaps in a specific method column, in two stages:
##   1. Local donors within dt itself (Genus > Family > Order > Class),
##      restricted to DIRECTLY-computed values only
##   2. If NO local donor exists at any level AND this method is in
##      EXTERNAL_FORMULA_REGISTRY, query FishBase/SeaLifeBase directly
##      for OTHER species in that same taxonomic group (via
##      species_list()) and compute the same formula for them - drawing
##      on the full external taxonomic universe, not just whichever
##      species happen to already be in your own species_df
## Every filled value is tagged in a companion "_source" column so it's
## never confused with a directly-computed one.
## =================================================================

fill_by_taxonomic_proximity <- function(dt, value_col, levels = c("Genus", "Family", "Order", "Class"),
                                        allow_external = TRUE) {
  source_col <- paste0(value_col, "_source")
  if (!source_col %in% names(dt)) dt[, (source_col) := NA_character_]
  dt[!is.na(get(value_col)) & is.na(get(source_col)), (source_col) := "direct"]
  
  ## --- Stage 1: local donors within dt --------------------------------
  for (lvl in levels) {
    if (!lvl %in% names(dt)) next
    
    donors <- dt[get(source_col) == "direct", .(.donor = mean(get(value_col), na.rm = TRUE)), by = lvl]
    donors <- donors[!is.na(get(lvl)) & !is.na(.donor)]
    if (nrow(donors) == 0) next
    
    dt <- merge(dt, donors, by = lvl, all.x = TRUE)
    dt[is.na(get(value_col)) & !is.na(.donor), (source_col) := paste0("borrowed (", lvl, ")")]
    dt[is.na(get(value_col)) & !is.na(.donor), (value_col) := .donor]
    dt[, .donor := NULL]
  }
  
  ## --- Stage 2: external donors from FishBase/SeaLifeBase, only for
  ## species where NO local donor was found at any level -----------------
  still_missing <- dt[is.na(get(value_col))]
  formula_fn <- EXTERNAL_FORMULA_REGISTRY[[value_col]]
  use_tropfishr <- value_col %in% TROPFISHR_EXTERNAL_METHODS
  
  if (allow_external && nrow(still_missing) > 0 && (!is.null(formula_fn) || use_tropfishr)) {
    pb <- progress_bar$new(
      format = paste0("  External donor lookup [", value_col, "] [:bar] :percent | :current/:total | Elapsed: :elapsedfull | ETA: :eta"),
      total = nrow(still_missing), clear = FALSE, width = 90
    )
    for (i in seq_len(nrow(still_missing))) {
      pb$tick()
      row <- still_missing[i]
      for (lvl in levels) {
        if (!lvl %in% names(dt) || is.na(row[[lvl]])) next
        
        candidates <- tryCatch({
          args <- setNames(list(row[[lvl]]), lvl)
          c(do.call(rfishbase::species_list, c(args, list(server = "fishbase"))),
            do.call(rfishbase::species_list, c(args, list(server = "sealifebase"))))
        }, error = function(e) character())
        candidates <- setdiff(unique(candidates), dt$Species)
        candidates <- head(candidates, 20)  # cap to avoid excessive API calls for large taxa
        if (length(candidates) == 0) next
        
        traits <- fetch_intrinsic_traits(candidates)
        
        ext_val <- if (use_tropfishr) {
          tfr <- fetch_tropfishr_M(traits[, .(Species, Loo, K, Temp = DEFAULT_TEMP, tmax)])
          method_col <- switch(value_col, M_Hoenig_1983 = "M_Hoenig_1983", M_Then_2015 = "M_Then_2015",
                               M_AlversonCarney_1975 = "M_AlversonCarney_1975")
          if (nrow(tfr) > 0 && method_col %in% names(tfr)) mean(tfr[[method_col]], na.rm = TRUE) else NA_real_
        } else {
          vals <- tryCatch(formula_fn(traits), error = function(e) NA_real_)
          mean(vals, na.rm = TRUE)
        }
        
        if (!is.na(ext_val) && !is.nan(ext_val) && !is.infinite(ext_val)) {
          dt[Species == row$Species, (value_col) := ext_val]
          dt[Species == row$Species, (source_col) := paste0("external FishBase/SeaLifeBase donor (", lvl, ")")]
          message("  '", row$Species, "' ", value_col, " filled from ", length(candidates),
                  " external ", lvl, "='", row[[lvl]], "' species (no local donor available)")
          break
        }
      }
    }
  }
  
  dt
}

## =================================================================
## STEP 2: fetch and prioritize traits from FishBase + SeaLifeBase
## =================================================================

## rfishbase fetches its underlying data from a Hugging Face-hosted
## mirror, which occasionally returns a transient 504 Gateway Timeout -
## an upstream infrastructure hiccup, not a bug in this code. Retry
## with backoff rather than fail outright on the first timeout.
retry_fetch <- function(expr_fun, attempts = 3, wait_seconds = 5) {
  for (i in seq_len(attempts)) {
    result <- tryCatch(expr_fun(), error = function(e) {
      message("Fetch attempt ", i, "/", attempts, " failed: ", conditionMessage(e))
      NULL
    })
    if (!is.null(result)) return(result)
    if (i < attempts) {
      message("Retrying in ", wait_seconds, " seconds...")
      Sys.sleep(wait_seconds)
    }
  }
  message("All ", attempts, " attempts failed - returning empty result. This is",
          " usually transient (Hugging Face mirror timeout) - try re-running later",
          " if this keeps happening.")
  data.table()
}

## Some rfishbase sub-functions (e.g. swimming()) can hit an internal
## bug - "missing value where TRUE/FALSE needed" - triggered by a
## specific species having no matching record, not a network issue
## (confirmed by identical failure across all retry attempts). Retrying
## the same batch call won't fix a deterministic bug in someone else's
## function. Falling back to per-species calls isolates just the
## problem species instead of losing trait data for the whole batch.
fetch_per_species <- function(fun, sp_list, server) {
  results <- vector("list", length(sp_list))
  pb <- progress_bar$new(
    format = paste0("  Per-species fallback (", server, ") [:bar] :percent | :current/:total | Elapsed: :elapsedfull | ETA: :eta"),
    total = length(sp_list), clear = FALSE, width = 90
  )
  for (i in seq_along(sp_list)) {
    pb$tick()
    results[[i]] <- tryCatch(as.data.table(fun(sp_list[i], server = server)),
                             error = function(e) {
                               message("  Skipping '", sp_list[i], "' for this trait: ", conditionMessage(e))
                               data.table()
                             })
  }
  rbindlist(results, fill = TRUE)
}

fetch_both <- function(fun, sp_list, ...) {
  fb <- retry_fetch(function() as.data.table(fun(sp_list, server = "fishbase", ...)))
  if (nrow(fb) == 0 && length(sp_list) > 1) {
    message("Batch fetch (fishbase) returned nothing - falling back to per-species",
            " calls to isolate which species is causing it...")
    fb <- fetch_per_species(fun, sp_list, "fishbase")
  }
  
  slb <- retry_fetch(function() as.data.table(fun(sp_list, server = "sealifebase", ...)))
  if (nrow(slb) == 0 && length(sp_list) > 1) {
    message("Batch fetch (sealifebase) returned nothing - falling back to per-species calls...")
    slb <- fetch_per_species(fun, sp_list, "sealifebase")
  }
  
  rbindlist(list(fb, slb), fill = TRUE)
}

## --- 2a. General species table: max weight, max length, depth range --
sp_table <- fetch_both(rfishbase::species, sp_list)
message("\nspecies() columns available:")
print(names(sp_table))

weight_col <- intersect(c("Weight", "WeightMax"), names(sp_table))[1]
length_col <- intersect(c("Length", "LengthMax"), names(sp_table))[1]
long_col   <- intersect(c("LongevityWild", "LongevityCaptive", "MaxAge"), names(sp_table))[1]

sp_traits <- if (nrow(sp_table) > 0) unique(sp_table[, .(
  Species,
  MaxWeight = if (!is.na(weight_col)) get(weight_col) else NA_real_,
  MaxLength = if (!is.na(length_col)) get(length_col) else NA_real_,
  Depth = fifelse(!is.na(DepthRangeDeep) & !is.na(DepthRangeShallow),
                  (DepthRangeDeep + DepthRangeShallow) / 2,
                  fcoalesce(DepthRangeDeep, DepthRangeShallow)),
  Longevity = if (!is.na(long_col)) get(long_col) else NA_real_
  ## NOTE: Family deliberately NOT extracted here - it's already merged
  ## in from WoRMS taxonomy (Step 1, with proper synonym resolution),
  ## and duplicating it here would collide into Family.x/Family.y on
  ## the merge below instead of a clean single column
)], by = "Species") else data.table(Species = character())

message("Coverage - MaxWeight: ", sp_traits[!is.na(MaxWeight), .N], "/", length(sp_list),
        " | Depth: ", sp_traits[!is.na(Depth), .N], "/", length(sp_list),
        " | Longevity: ", sp_traits[!is.na(Longevity), .N], "/", length(sp_list),
        " (field: ", ifelse(is.na(long_col), "NONE FOUND", long_col), ")")
STAGE_PB$tick(tokens = list(stage_name = "Fetch 2a: species() traits"))

## --- 2b. Growth params (Loo, K, Winfinity) - Mediterranean/recent-prioritized
growth_raw <- fetch_both(popgrowth, sp_list)
growth_best <- select_best_rows(growth_raw)
tmax_col <- if (nrow(growth_best) > 0) intersect(c("tmax", "TMax"), names(growth_best))[1] else NA
growth_traits <- if (nrow(growth_best) > 0) growth_best[, .(
  Species,
  Loo  = if ("Loo" %in% names(growth_best)) Loo else NA_real_,
  K    = if ("K" %in% names(growth_best)) K else NA_real_,
  Winf = if ("Winfinity" %in% names(growth_best)) Winfinity else NA_real_,
  tmax = if (!is.na(tmax_col)) get(tmax_col) else NA_real_
)] else data.table(Species = character())
STAGE_PB$tick(tokens = list(stage_name = "Fetch 2b: growth params"))

## --- 2c. Length-weight a/b params - same prioritization ---------------
lw_raw <- fetch_both(poplw, sp_list)
lw_best <- select_best_rows(lw_raw)
lw_traits <- if (nrow(lw_best) > 0) lw_best[, .(
  Species,
  a_lw = if ("a" %in% names(lw_best)) a else NA_real_,
  b_lw = if ("b" %in% names(lw_best)) b else NA_real_
)] else data.table(Species = character())
STAGE_PB$tick(tokens = list(stage_name = "Fetch 2c: length-weight a/b"))

## --- 2d. Maturity: Lm (length at maturity), needed for Froese-Binohlan
maturity_raw <- fetch_both(maturity, sp_list)
maturity_best <- select_best_rows(maturity_raw)
lm_col <- if (nrow(maturity_best) > 0) intersect(c("Lm", "LengthMatMin"), names(maturity_best))[1] else NA
tmat_col <- if (nrow(maturity_best) > 0) intersect(c("tm", "AgeMatMin"), names(maturity_best))[1] else NA
maturity_traits <- if (nrow(maturity_best) > 0) maturity_best[, .(
  Species,
  Lm   = if (!is.na(lm_col)) get(lm_col) else NA_real_,
  Tmat = if (!is.na(tmat_col)) get(tmat_col) else NA_real_
)] else data.table(Species = character())

message("Coverage - Lm (length at maturity): ", maturity_traits[!is.na(Lm), .N], "/", length(sp_list),
        " (field: ", ifelse(is.na(lm_col), "NONE FOUND", lm_col), ")")
STAGE_PB$tick(tokens = list(stage_name = "Fetch 2d: maturity"))

## --- 2e. Ecology: trophic level ---------------------------------------
ecol <- fetch_both(ecology, sp_list)
tl_col <- if (nrow(ecol) > 0) intersect(c("DietTroph", "FoodTroph"), names(ecol))[1] else NA
ecol_traits <- if (nrow(ecol) > 0) unique(ecol[, .(
  Species, TrophicLevel = if (!is.na(tl_col)) get(tl_col) else NA_real_
)], by = "Species") else data.table(Species = character())
STAGE_PB$tick(tokens = list(stage_name = "Fetch 2e: ecology/trophic level"))

## --- 2f. Swimming: aspect ratio (fish only) ---------------------------
swim <- fetch_both(swimming, sp_list)
ar_col <- if (nrow(swim) > 0) intersect(c("AspectRatio", "Aspect"), names(swim))[1] else NA
swim_traits <- if (nrow(swim) > 0) unique(swim[, .(
  Species, AspectRatio = if (!is.na(ar_col)) get(ar_col) else NA_real_
)], by = "Species") else data.table(Species = character())
STAGE_PB$tick(tokens = list(stage_name = "Fetch 2f: swimming/aspect ratio"))

## --- assemble ----------------------------------------------------------
species_df <- Reduce(function(x, y) merge(x, y, by = "Species", all.x = TRUE),
                     list(species_df, sp_traits, growth_traits, lw_traits,
                          maturity_traits, ecol_traits, swim_traits))

DEFAULT_TEMP <- 16
species_df[, Temp := DEFAULT_TEMP]

## derive Winf from a/b length-weight relationship (protocol Eq. 11)
## when not directly available from popgrowth's Winfinity field
species_df[, Winf := fcoalesce(Winf, a_lw * Loo^b_lw)]

## Froese & Binohlan (2003) fallback for Loo/K when missing entirely -
## now fully automatic: Lmax from species(), Lm/Tmat from maturity()
species_df[, Loo_fb := MaxLength / 0.95]
needs_fb <- species_df[is.na(Loo) & !is.na(Loo_fb) & !is.na(Lm) & !is.na(Tmat)]
if (nrow(needs_fb) > 0) {
  needs_fb[, t0_est := 0]
  for (iter in 1:3) {
    needs_fb[, k_est := -log(1 - Lm / Loo_fb) / (Tmat - t0_est)]
    needs_fb[, t0_est := -10^(-0.3922 - 0.2752 * log10(Loo_fb) - 1.038 * log10(k_est))]
  }
  species_df[needs_fb, on = "Species", `:=`(Loo = fifelse(is.na(Loo), i.Loo_fb, Loo),
                                            K = fifelse(is.na(K), i.k_est, K))]
  message("\nFroese-Binohlan fallback filled Loo/K for ", nrow(needs_fb), " species",
          " using auto-fetched Lmax/Lm/Tmat.")
}

message("\n=== Overall trait coverage before calculation ===")
print(species_df[, .(
  n = .N, MaxWeight = sum(!is.na(MaxWeight)), Loo = sum(!is.na(Loo)), K = sum(!is.na(K)),
  Winf = sum(!is.na(Winf)), Lm = sum(!is.na(Lm)), TrophicLevel = sum(!is.na(TrophicLevel)),
  AspectRatio = sum(!is.na(AspectRatio)), Longevity = sum(!is.na(Longevity)), Depth = sum(!is.na(Depth))
), by = dispatch_group])
STAGE_PB$tick(tokens = list(stage_name = "Assemble + derive traits (Froese-Binohlan)"))

## Fill gaps in the RAW TRAITS themselves (before any formula runs) by
## taxonomic proximity - local donors within species_df first, external
## FishBase/SeaLifeBase donors if none exist locally. This is more
## effective than only patching derived results downstream, since a
## single missing Winf currently blanks multiple different QB methods
## at once (eq26, eq24) - filling it once here fixes all of them together.
for (trait in c("Winf", "TrophicLevel", "MaxWeight", "AspectRatio", "Longevity")) {
  species_df <- fill_by_taxonomic_proximity(species_df, trait)
}

message("\n=== Trait coverage AFTER taxonomic-proximity gap filling ===")
print(species_df[, .(
  n = .N, MaxWeight = sum(!is.na(MaxWeight)), Winf = sum(!is.na(Winf)),
  TrophicLevel = sum(!is.na(TrophicLevel)), AspectRatio = sum(!is.na(AspectRatio)),
  Longevity = sum(!is.na(Longevity))
), by = dispatch_group])
STAGE_PB$tick(tokens = list(stage_name = "Raw-trait gap filling"))

## =================================================================
## FISH: Pauly (1980) M [Eq. 9] -> FishLife (phylogenetic imputation,
## Thorson et al. 2017/2020/2023) -> Gascuel fallback;
## F = Yield/Biomass (density-consistent); PB = M+F
## QB via Palomares & Pauly (1998) [Eq. 27] -> Q/P=3 fallback
## =================================================================

## FishLife: for species with NO direct FishBase growth studies, this
## borrows information from phylogenetically related taxa rather than
## jumping straight to the trophic-level-only Gascuel fallback - a
## genuinely better-informed estimate for data-poor species. Not part
## of the original protocol document, but a legitimate, actively
## maintained (2023) addition specifically suited to this exact gap.
## Requires: devtools::install_github("james-thorson/FishLife", dep=TRUE)
##
## NOTE: FishLife's exact function signature has changed across its
## 2017/2020/2023 releases - this uses the longest-standing, most-cited
## interface (Search_species + Plot_taxa). If your installed version
## errors here, check ?FishLife::Search_species for the current API
## and adjust - the coverage printout below will make failures visible
## rather than silently skipping species.

## Class-name translation: WoRMS and FishLife's bundled (older FishBase-
## based) taxonomy use different terms for the same group in places -
## CONFIRMED via direct testing (installed and ran FishLife in an
## isolated environment to verify): WoRMS says "Teleostei", FishLife's
## database uses "Actinopterygii" for the same broad group. Without
## FishLife: for species with NO direct FishBase growth studies, this
## borrows information from phylogenetically related taxa rather than
## jumping straight to the trophic-level-only Gascuel fallback - a
## genuinely better-informed estimate for data-poor species.
##
## FishLife::Search_species() calls rfishbase::fishbase, an object that
## doesn't exist in your installed rfishbase (confirmed directly:
## "'fishbase' is not an exported object from 'namespace:rfishbase'").
## This bypasses Search_species() entirely, using the taxonomy you
## already fetched via WoRMS (Step 1) to do the same matching against
## FishLife's own bundled prediction tree (ParentChild_gz) - verified
## working this way against real species in isolated testing. No
## reinstall needed - Plot_taxa() itself doesn't touch rfishbase at all,
## only Search_species() did.
##
## NOTE: verified against a recent build of FishLife - you're on 3.1.0,
## an older release, so the internal object names below (ParentChild_gz,
## Find_ancestors, ChildName) are assumed but not confirmed for your
## exact version. The diagnostic print will make it obvious immediately
## if something doesn't match, rather than silently returning NA again.
FISHLIFE_CLASS_TRANSLATION <- c(
  "Teleostei" = "Actinopterygii",
  "Actinopteri" = "Actinopterygii"
)

fetch_fishlife <- function(taxonomy_dt) {
  if (nrow(taxonomy_dt) == 0) return(data.table())
  if (!requireNamespace("FishLife", quietly = TRUE)) {
    message("FishLife not installed - skipping this method",
            " (fine, five other fish M methods are still available).")
    return(data.table())
  }
  
  db <- tryCatch(FishLife::FishBase_and_RAM, error = function(e) NULL)
  if (is.null(db) || is.null(db$ParentChild_gz)) {
    message("FishLife's bundled database (FishBase_and_RAM$ParentChild_gz)",
            " wasn't found under that name in your installed version (3.1.0) -",
            " the internal structure may differ from what this was verified",
            " against. Skipping FishLife for this run.")
    return(data.table())
  }
  ParentChild_gz <- db$ParentChild_gz
  
  match_taxon <- function(class_, order_, family_, genus_, species_epithet) {
    class_ <- if (!is.na(class_) && class_ %in% names(FISHLIFE_CLASS_TRANSLATION)) {
      FISHLIFE_CLASS_TRANSLATION[[class_]]
    } else class_
    match_taxonomy <- c(class_, order_, family_, genus_, species_epithet)
    match_taxonomy[is.na(match_taxonomy)] <- "predictive"
    
    Count <- 0
    Group <- NA
    while (is.na(Group) && Count <= 5) {
      Group <- match(paste(tolower(match_taxonomy), collapse = "_"), tolower(ParentChild_gz[, 'ChildName']))
      if (is.na(Group)) match_taxonomy[length(match_taxonomy) - Count] <- "predictive"
      Count <- Count + 1
    }
    if (is.na(Group)) return(NULL)
    
    Group <- tryCatch(FishLife:::Find_ancestors(child_num = Group, ParentChild_gz = ParentChild_gz),
                      error = function(e) NULL)
    if (is.null(Group)) return(NULL)
    
    Add_predictive <- function(char_vec) {
      return_vec <- char_vec
      for (i in seq_along(return_vec)) {
        vec <- strsplit(as.character(return_vec[i]), "_")[[1]]
        return_vec[i] <- paste(c(vec, rep("predictive", 5 - length(vec))), collapse = "_")
      }
      return_vec
    }
    unique(as.character(Add_predictive(ParentChild_gz[Group, 'ChildName'])))
  }
  
  results <- vector("list", nrow(taxonomy_dt))
  printed_diagnostic <- FALSE
  
  pb <- progress_bar$new(
    format = "  FishLife lookup [:bar] :percent | :current/:total | Elapsed: :elapsedfull | ETA: :eta",
    total = nrow(taxonomy_dt), clear = FALSE, width = 90
  )
  for (i in seq_len(nrow(taxonomy_dt))) {
    pb$tick()
    row <- taxonomy_dt[i]
    parts <- strsplit(row$Species, " ")[[1]]
    if (length(parts) < 2) next
    
    pred <- tryCatch({
      taxon_match <- match_taxon(row$Class, row$Order, row$Family, row$Genus, parts[2])
      if (is.null(taxon_match)) return(NULL)
      tmp_plot <- tempfile(fileext = ".pdf")
      grDevices::pdf(tmp_plot)
      on.exit({ grDevices::dev.off(); unlink(tmp_plot) }, add = TRUE)
      FishLife::Plot_taxa(taxon_match, mfrow = c(1, 1))
    }, error = function(e) {
      message("  FishLife lookup failed for ", row$Species, ": ", conditionMessage(e))
      NULL
    })
    
    if (is.null(pred)) next
    mean_pred <- pred[[1]]$Mean_pred
    if (is.null(mean_pred)) next
    
    if (!printed_diagnostic) {
      message("\n>>> DIAGNOSTIC: FishLife Mean_pred names (first species, '", row$Species,
              "') - verify Loo/K/M/Winfinity appear here <<<")
      print(names(mean_pred))
      printed_diagnostic <- TRUE
    }
    
    loo_n  <- intersect(c("Loo", "ln_Loo"), names(mean_pred))[1]
    k_n    <- intersect(c("K", "ln_K"), names(mean_pred))[1]
    m_n    <- intersect(c("M", "ln_M"), names(mean_pred))[1]
    winf_n <- intersect(c("Winfinity", "ln_Winfinity"), names(mean_pred))[1]
    
    results[[i]] <- data.table(
      Species = row$Species,
      Loo_fishlife  = if (!is.na(loo_n)) exp(mean_pred[[loo_n]]) else NA_real_,
      K_fishlife    = if (!is.na(k_n)) exp(mean_pred[[k_n]]) else NA_real_,
      M_FishLife_2023    = if (!is.na(m_n)) exp(mean_pred[[m_n]]) else NA_real_,
      Winf_fishlife = if (!is.na(winf_n)) exp(mean_pred[[winf_n]]) else NA_real_
    )
  }
  out <- rbindlist(results, fill = TRUE)
  message("FishLife: resolved ", nrow(out), " of ", nrow(taxonomy_dt), " species attempted.")
  out
}

## TropFishR::M_empirical() - bundles several established, distinct M
## estimators beyond Pauly (1980), including Then et al. (2015), which
## its own paper identifies as the best-performing empirical estimator
## across a 200+ species validation set - arguably the current
## methodological standard, not just an alternative. Same underlying
## logic as the fishmethods package that powers the Barefoot Ecologist's
## Natural Mortality Tool Shiny app, but with self-documenting named
## methods instead of a numbered method list.
## Requires: install.packages("TropFishR")

fetch_tropfishr_M <- function(sp_dt) {
  if (!requireNamespace("TropFishR", quietly = TRUE)) {
    message("TropFishR not installed - skipping these M methods.",
            " Install via: install.packages('TropFishR')")
    return(data.table())
  }
  
  ## VERIFIED against the actual TropFishR source (M_empirical.R): the
  ## function returns a MATRIX with a single column literally named "M" -
  ## each requested method is a ROW, identified only by its ROW NAME
  ## (e.g. "Hoenig (1983) - Joint Equation", "Then (2015) - growth"),
  ## NOT by a per-method column. Confirmed empirically by running the
  ## exact source locally. The previous version searched for columns
  ## named "Hoenig"/"Then_growth"/etc., which never existed - that's why
  ## every method came back NA regardless of input data availability.
  ## Note also: "Hoenig" produces TWO rows (Joint Equation, Fish
  ## Equation) - averaged here into one M_Hoenig_1983 value.
  methods_wanted <- c("Hoenig", "Then_growth", "AlversonCarney")
  results <- vector("list", nrow(sp_dt))
  printed_diagnostic <- FALSE
  
  pb <- progress_bar$new(
    format = "  TropFishR M methods [:bar] :percent | :current/:total | Elapsed: :elapsedfull | ETA: :eta",
    total = nrow(sp_dt), clear = FALSE, width = 90
  )
  for (i in seq_len(nrow(sp_dt))) {
    pb$tick()
    row <- sp_dt[i]
    pred <- tryCatch(
      TropFishR::M_empirical(Linf = row$Loo, K_l = row$K, temp = row$Temp,
                             tmax = row$tmax, method = methods_wanted),
      error = function(e) {
        message("  TropFishR::M_empirical() failed for ", row$Species, ": ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(pred)) next
    
    ## keep.rownames is the actual fix - without it, the method-
    ## identifying row names are silently dropped entirely
    pred_dt <- as.data.table(pred, keep.rownames = "method_name")
    
    if (!printed_diagnostic) {
      message("\n>>> DIAGNOSTIC: TropFishR::M_empirical() output (first species, '",
              row$Species, "') <<<")
      print(pred_dt)
      printed_diagnostic <- TRUE
    }
    
    find_val <- function(pattern) {
      hits <- pred_dt[grepl(pattern, method_name, ignore.case = TRUE)]
      if (nrow(hits) == 0) return(NA_real_)
      mean(hits$M, na.rm = TRUE)
    }
    
    results[[i]] <- data.table(
      Species = row$Species,
      M_Hoenig_1983 = find_val("hoenig"),
      M_Then_2015   = find_val("then"),
      M_AlversonCarney_1975 = find_val("alverson|carney")
    )
  }
  out <- rbindlist(results, fill = TRUE)
  message("TropFishR M methods: resolved ", nrow(out), " of ", nrow(sp_dt), " species attempted",
          " (", out[!is.na(M_Hoenig_1983), .N], " with Hoenig, ", out[!is.na(M_Then_2015), .N],
          " with Then, ", out[!is.na(M_AlversonCarney_1975), .N], " with Alverson-Carney).")
  out
}

calc_fish <- function(out) {
  ## --- Fetch FishLife for ALL fish species (not gated by missing Loo/K) -
  ## computing every method for every species, so they're genuinely
  ## comparable against Pauly/Hoenig/Then/etc., not just a fallback
  ## chain that skips FishLife whenever direct data already exists -----
  fl <- fetch_fishlife(out[, .(Species, Genus, Family, Order, Class)])
  if (nrow(fl) > 0) out <- merge(out, fl, by = "Species", all.x = TRUE)
  for (col in c("Loo_fishlife", "K_fishlife", "M_FishLife_2023", "Winf_fishlife")) {
    if (!col %in% names(out)) out[, (col) := NA_real_]
  }
  
  ## =================================================================
  ## PB - three independent methods, each as its own column
  ## =================================================================
  
  ## Method 1: Pauly (1980), protocol Eq.9, from direct FishBase growth data
  out[, M_Pauly_1980 := 10^(-0.0066 - 0.279 * log10(Loo) + 0.6543 * log10(K) + 0.4634 * log10(Temp))]
  
  ## Method 2: FishLife's own M (phylogenetic imputation) - used directly,
  ## not re-derived via Pauly from FishLife's Loo/K, which would discard
  ## its joint-uncertainty modeling across correlated traits
  ## (M_FishLife_2023 column already exists from the merge/default above)
  
  ## Method 3: Gascuel et al. (2008), fish-specific, protocol Eq.15 -
  ## only needs trophic level + temperature, computable for almost every
  ## species regardless of growth-data availability
  out[, M_Gascuel_2008 := 2.31 * TrophicLevel^(-1.72) * exp(0.053 * Temp)]
  
  ## Methods 4-6: TropFishR::M_empirical() - Hoenig (1983), Then et al.
  ## (2015, identified in its own validation paper as the best-performing
  ## empirical estimator across 200+ species), and Alverson & Carney
  ## (1975) - genuinely distinct methods, not variations on Pauly's
  tfr <- fetch_tropfishr_M(out[, .(Species, Loo, K, Temp, tmax)])
  if (nrow(tfr) > 0) {
    out <- merge(out, tfr, by = "Species", all.x = TRUE)
  } else {
    out[, c("M_Hoenig_1983", "M_Then_2015", "M_AlversonCarney_1975") := NA_real_]
  }
  
  ## Fill gaps in each M method independently by borrowing from the
  ## closest taxonomic relative that has a DIRECT value for that same
  ## method - e.g. a species missing growth data for Pauly's equation
  ## can borrow its genus-mates' Pauly M, rather than that method
  ## column just staying NA. Every borrowed value is tagged, not
  ## silently blended in.
  for (col in c("M_Pauly_1980", "M_Hoenig_1983", "M_Then_2015", "M_AlversonCarney_1975", "M_Gascuel_2008")) {
    out <- fill_by_taxonomic_proximity(out, col)
  }
  
  ## F = Yield/Biomass - BOTH must be densities (t/km^2, t/km^2/yr) for
  ## this ratio to be a valid rate; applies identically on top of
  ## whichever M source, since Z = M+F regardless of how M was obtained
  out[, Fmort := Yield / Biomass]
  out[, PB_Pauly_1980    := fifelse(!is.na(Fmort), M_Pauly_1980 + Fmort, M_Pauly_1980)]
  out[, PB_FishLife_2023 := fifelse(!is.na(Fmort), M_FishLife_2023 + Fmort, M_FishLife_2023)]
  out[, PB_Gascuel_2008  := fifelse(!is.na(Fmort), M_Gascuel_2008 + Fmort, M_Gascuel_2008)]
  out[, PB_Hoenig_1983   := fifelse(!is.na(Fmort), M_Hoenig_1983 + Fmort, M_Hoenig_1983)]
  out[, PB_Then_2015     := fifelse(!is.na(Fmort), M_Then_2015 + Fmort, M_Then_2015)]
  out[, PB_AlversonCarney_1975 := fifelse(!is.na(Fmort), M_AlversonCarney_1975 + Fmort, M_AlversonCarney_1975)]
  
  ## chosen PB for FG-level weighting: Then et al. (2015) prioritized
  ## first given its validated best-in-class performance, then Pauly
  ## (the protocol's own primary method), then the rest - but ALL method
  ## columns remain available for direct comparison regardless of
  ## what gets chosen here
  out[, PB := fcoalesce(PB_Then_2015, PB_Pauly_1980, PB_Hoenig_1983, PB_AlversonCarney_1975, PB_FishLife_2023, PB_Gascuel_2008)]
  out[, PB_method := fifelse(!is.na(PB_Then_2015), "Then et al. 2015",
                             fifelse(!is.na(PB_Pauly_1980), "Pauly 1980 (Eq.9)",
                                     fifelse(!is.na(PB_Hoenig_1983), "Hoenig 1983",
                                             fifelse(!is.na(PB_AlversonCarney_1975), "Alverson & Carney 1975",
                                                     fifelse(!is.na(PB_FishLife_2023), "FishLife",
                                                             fifelse(!is.na(PB_Gascuel_2008), "Gascuel 2008 (Eq.15)", NA_character_))))))]
  out[!is.na(Fmort) & !is.na(PB), PB_method := paste0(PB_method, "+F(Y/B)")]
  
  ## =================================================================
  ## QB - four independent methods, each as its own column
  ## =================================================================
  
  out[, Tprime := 1000 / (Temp + 273.15)]
  h_dummy <- 0; d_dummy <- 0   # herbivore/detritivore dummies default to
  # carnivore (0) - no reliable automated
  # diet-type classification available yet
  Pf_dummy <- 1                 # predator dummy for Eq.24 - defaults to 1
  # (predator), the common case for real fish
  
  ## Method 1: Palomares & Pauly (1998), protocol Eq.27 - uses Z (=PB)
  ## directly, the most information-rich when a PB estimate is available
  out[, logQB_PalomaresPauly_1998Z := 5.847 + 0.280 * log10(PB) - 0.152 * log10(Winf) -
        1.360 * Tprime + 0.062 * AspectRatio + 0.510 * h_dummy + 0.390 * d_dummy]
  out[, QB_PalomaresPauly_1998Z := 10^logQB_PalomaresPauly_1998Z]
  
  ## Method 2: Palomares & Pauly (1998), protocol Eq.26 - does NOT need
  ## Z/PB at all, only morphometrics/temperature/diet-type - useful as
  ## an independent cross-check since it doesn't inherit any PB error
  out[, logQB_PalomaresPauly_1998noZ := 7.964 - 0.204 * log10(Winf) - 1.965 * Tprime +
        0.083 * AspectRatio + 0.532 * h_dummy + 0.398 * d_dummy]
  out[, QB_PalomaresPauly_1998noZ := 10^logQB_PalomaresPauly_1998noZ]
  
  ## Method 3: Christensen & Pauly (1992) adapted, protocol Eq.24 - a
  ## different independent formula, uses predator/herbivore dummies
  ## instead of aspect ratio, doesn't need Z either.
  ## NOTE: uses T' directly, NOT log10(T') - the source PDF extraction
  ## showed "log10 T'" here, but that's almost certainly an OCR artifact:
  ## Eq.26 and Eq.27 in the same protocol both use T' directly, and
  ## log-transforming it here produces Q/B values in the tens of
  ## thousands (physically implausible - real fish Q/B is ~1-20/year).
  ## Using T' directly brings this back in line with the other two
  ## equations' order of magnitude.
  out[, logQB_ChristensenPauly_1992 := 6.37 - 1.5045 * Tprime - 0.168 * log10(Winf) +
        0.1399 * Pf_dummy + 0.2765 * h_dummy]
  out[, QB_ChristensenPauly_1992 := 10^logQB_ChristensenPauly_1992]
  
  ## Fill gaps in the two Winf/AspectRatio-dependent QB methods by
  ## borrowing from the closest taxonomic relative - QB_ChristensenPauly_1992 doesn't
  ## need AspectRatio so it's less prone to gaps, but still benefits
  for (col in c("QB_PalomaresPauly_1998Z", "QB_PalomaresPauly_1998noZ", "QB_ChristensenPauly_1992")) {
    out <- fill_by_taxonomic_proximity(out, col)
  }
  
  ## Method 4: Q/P=3 fallback (Eq.29), using chosen PB
  out[, QB_ChristensenEtAl_2008 := 3 * PB]
  
  ## chosen QB for FG-level weighting: prefer the Z-based equation
  ## (most information used) > the two Z-independent equations > Q/P=3
  out[, QB := fcoalesce(QB_PalomaresPauly_1998Z, QB_PalomaresPauly_1998noZ, QB_ChristensenPauly_1992, QB_ChristensenEtAl_2008)]
  out[, QB_method := fifelse(!is.na(QB_PalomaresPauly_1998Z), "Palomares & Pauly 1998 (Z-based)",
                             fifelse(!is.na(QB_PalomaresPauly_1998noZ), "Palomares & Pauly 1998 (non-Z)",
                                     fifelse(!is.na(QB_ChristensenPauly_1992), "Christensen & Pauly 1992",
                                             fifelse(!is.na(QB_ChristensenEtAl_2008), "Q/P=3 (Christensen et al. 2008)", NA_character_))))]
  
  out[, .(Species, FG, Biomass, dispatch_group,
          M_Pauly_1980, M_FishLife_2023, M_Gascuel_2008, M_Hoenig_1983, M_Then_2015, M_AlversonCarney_1975, Fmort,
          PB_Pauly_1980, PB_FishLife_2023, PB_Gascuel_2008, PB_Hoenig_1983, PB_Then_2015, PB_AlversonCarney_1975, PB, PB_method,
          QB_PalomaresPauly_1998Z, QB_PalomaresPauly_1998noZ, QB_ChristensenPauly_1992, QB_ChristensenEtAl_2008, QB, QB_method)]
}

## =================================================================
## MARINE MAMMALS: Barlow & Boveng Siler PB -> Gascuel fallback;
## Innes/Trites QB -> Q/P=3 fallback
## =================================================================

siler_pb <- function(longevity, surrogate_type) {
  params <- list(
    "2" = list(a1 = 14.343, a2 = 0.171,  a3 = 0.0121, b1 = 10.259, b3 = 6.6878),
    "3" = list(a1 = 30.43,  a2 = 0,      a3 = 0.7276, b1 = 206.72, b3 = 2.3188),
    "4" = list(a1 = 40.409, a2 = 0.4772, a3 = 0.0047, b1 = 310.36, b3 = 8.029)
  )
  p <- params[[as.character(surrogate_type)]]
  if (is.null(p) || is.na(longevity)) return(NA_real_)
  W <- longevity
  x <- 1:floor(W)
  lj <- exp((-p$a1 / p$b1) * (1 - exp(-p$b1 * x / W)))
  lc <- exp(-p$a2 * x / W)
  ls <- exp((p$a3 / p$b3) * (1 - exp(p$b3 * x / W)))
  lx <- lj * lc * ls
  survival <- lx / shift(lx, fill = 1)
  mean(-log(survival), na.rm = TRUE)
}

MAMMAL_SURROGATE_DEFAULT <- data.table(
  Family = c("Phocidae", "Monachidae", "Otariidae", "Delphinidae", "Ziphiidae", "Physeteridae"),
  surrogate_type = c(2, 2, 2, 3, 3, 3)
)

calc_mammal <- function(out) {
  out <- merge(out, MAMMAL_SURROGATE_DEFAULT, by = "Family", all.x = TRUE)
  out[is.na(surrogate_type), surrogate_type := 2]
  
  ## Method 1: Barlow & Boveng (1991) Siler survivorship - needs Longevity
  out[, PB_BarlowBoveng_1991 := mapply(siler_pb, Longevity, surrogate_type)]
  ## Method 2: Gascuel general (Eq.23) - only needs trophic level + temp,
  ## computable independently regardless of Longevity availability
  out[, PB_Gascuel_2008 := 20.19 * TrophicLevel^(-3.26) * exp(0.041 * Temp)]
  
  ## Fill gaps in BOTH PB methods by borrowing from the closest
  ## taxonomic relative (local first, external FishBase/SeaLifeBase
  ## donors if no local match) - PB_Gascuel_2008 was previously left
  ## unfilled, meaning it silently failed whenever TrophicLevel was
  ## missing for a species with no genus/family-mate in your own
  ## species_df, even though it's the ONLY fallback once Siler fails
  out <- fill_by_taxonomic_proximity(out, "PB_BarlowBoveng_1991")
  out <- fill_by_taxonomic_proximity(out, "PB_Gascuel_2008")
  
  out[, PB := fcoalesce(PB_BarlowBoveng_1991, PB_Gascuel_2008)]
  out[, PB_method := fifelse(!is.na(PB_BarlowBoveng_1991), "Barlow & Boveng 1991 (Siler)",
                             fifelse(!is.na(PB_Gascuel_2008), "Gascuel 2008 general (Eq.23)", NA_character_))]
  
  ## Method 1: Innes/Trites (Eq.31) - needs MaxWeight
  out[, W_kg := MaxWeight / 1000]
  out[, QB_InnesTrites_1997 := (0.1 * W_kg^0.8 / W_kg) * 365]
  out <- fill_by_taxonomic_proximity(out, "QB_InnesTrites_1997")
  ## Method 2: Q/P=3 fallback, using chosen PB
  out[, QB_ChristensenEtAl_2008 := 3 * PB]
  
  out[, QB := fcoalesce(QB_InnesTrites_1997, QB_ChristensenEtAl_2008)]
  out[, QB_method := fifelse(!is.na(QB_InnesTrites_1997), "Innes/Trites 1987/1997 (Eq.31)",
                             fifelse(!is.na(QB_ChristensenEtAl_2008), "Q/P=3 (Christensen et al. 2008)", NA_character_))]
  
  out[, .(Species, FG, Biomass, dispatch_group,
          PB_BarlowBoveng_1991, PB_Gascuel_2008, PB, PB_method,
          QB_InnesTrites_1997, QB_ChristensenEtAl_2008, QB, QB_method)]
}

## =================================================================
## SEABIRDS: Gascuel general PB (only automatable option - no
## dedicated seabird PB method exists in the protocol); Nilsson &
## Nilsson QB, and Q/P=3 as an independent second QB method
## =================================================================

calc_seabird <- function(out) {
  out[, PB_Gascuel_2008 := 20.19 * TrophicLevel^(-3.26) * exp(0.041 * Temp)]
  ## this is the ONLY PB method for seabirds (no dedicated equation in
  ## the protocol) - previously left unfilled, so any species missing
  ## TrophicLevel got NO PB at all, with nothing else to fall back to
  out <- fill_by_taxonomic_proximity(out, "PB_Gascuel_2008")
  out[, PB := PB_Gascuel_2008]
  out[, PB_method := fifelse(!is.na(PB), "Gascuel 2008 general (Eq.23) - no dedicated seabird PB method in protocol", NA_character_)]
  
  ## Method 1: Nilsson & Nilsson (1976) - needs MaxWeight
  out[, logDR := -0.293 + 0.85 * log10(MaxWeight)]
  out[, QB_NilssonNilsson_1976 := (10^logDR / MaxWeight) * 365]
  out <- fill_by_taxonomic_proximity(out, "QB_NilssonNilsson_1976")
  ## Method 2: Q/P=3 fallback, using chosen PB
  out[, QB_ChristensenEtAl_2008 := 3 * PB]
  
  out[, QB := fcoalesce(QB_NilssonNilsson_1976, QB_ChristensenEtAl_2008)]
  out[, QB_method := fifelse(!is.na(QB_NilssonNilsson_1976), "Nilsson & Nilsson 1976 (Eq.30)",
                             fifelse(!is.na(QB_ChristensenEtAl_2008), "Q/P=3 (Christensen et al. 2008)", NA_character_))]
  
  out[, .(Species, FG, Biomass, dispatch_group, PB_Gascuel_2008, PB, PB_method,
          QB_NilssonNilsson_1976, QB_ChristensenEtAl_2008, QB, QB_method)]
}

## =================================================================
## INVERTEBRATES: Tumbiolo & Downing (1994) and Gascuel general (2008)
## as two independent PB methods; Q/P=3 for QB (only automatable
## option - see the Brey 2012 manual-review flag further down for the
## more accurate but non-automatable alternative)
## =================================================================

calc_invertebrate <- function(out) {
  out[, logP := 0.24 + 0.96 * log10(Biomass) - 0.21 * log10(MaxWeight) +
        0.03 * Temp - 0.16 * log10(Depth + 1)]
  out[, PB_TumbioloDowning_1994 := (10^logP) / Biomass]
  out[, PB_Gascuel_2008 := 20.19 * TrophicLevel^(-3.26) * exp(0.041 * Temp)]
  
  ## Method 3: Brey (1999), protocol Eq.19 - log(P/B) = 1.672 +
  ## 0.993*log(1/Amax) - 0.035*log(Mmax) - 300.447*(1/(T+273)).
  ## Genuinely different inputs than the other two methods: max age
  ## (Amax) and max body mass in KJ (Mmax), not Depth/Biomass or
  ## TrophicLevel - so this can succeed for species where BOTH other
  ## methods fail due to missing Depth/TrophicLevel, as long as max
  ## age or max weight data exists.
  ## Amax reuses the Longevity field (max age, years) already fetched
  ## for other groups. Mmax needs body mass in KJ, not grams - converting
  ## requires an energy-density constant that genuinely varies by tissue
  ## type (roughly 4-24 KJ/g across taxa). Using ~4.5 KJ/g as a rough
  ## wet-weight marine invertebrate estimate (commonly cited order of
  ## magnitude in the literature) - this is an approximation, not a
  ## precise per-taxon value, and adds real uncertainty on top of the
  ## formula itself. Flagged here so it's not mistaken for an exact input.
  ENERGY_DENSITY_KJ_PER_G <- 4.5
  out[, Mmax_KJ := MaxWeight * ENERGY_DENSITY_KJ_PER_G]
  out[, PB_Brey_1999 := 10^(1.672 + 0.993 * log10(1 / Longevity) -
                              0.035 * log10(Mmax_KJ) - 300.447 * (1 / (Temp + 273)))]
  
  ## PB_TumbioloDowning_1994 can only ever be "direct" for a species that HAS its
  ## own MaxWeight+Depth - if none of your invertebrates have that
  ## (common: SeaLifeBase's MaxWeight coverage for inverts is patchy),
  ## the local donor pool for THIS method is permanently empty and
  ## local-only filling can't help.
  out <- fill_by_taxonomic_proximity(out, "PB_TumbioloDowning_1994", allow_external = FALSE)
  out <- fill_by_taxonomic_proximity(out, "PB_Gascuel_2008")
  out <- fill_by_taxonomic_proximity(out, "PB_Brey_1999", allow_external = FALSE)
  
  out[, PB := fcoalesce(PB_TumbioloDowning_1994, PB_Brey_1999, PB_Gascuel_2008)]
  out[, PB_method := fifelse(!is.na(PB_TumbioloDowning_1994), "Tumbiolo & Downing 1994 (Eq.17)",
                             fifelse(!is.na(PB_Brey_1999), "Brey 1999 (Eq.19)",
                                     fifelse(!is.na(PB_Gascuel_2008), "Gascuel 2008 general fallback (Eq.23)", NA_character_)))]
  
  out[, QB_ChristensenEtAl_2008 := 3 * PB]
  out[, QB := QB_ChristensenEtAl_2008]
  out[, QB_method := fifelse(!is.na(QB), "Q/P=3 (Christensen et al. 2008)", NA_character_)]
  
  out[, .(Species, FG, Biomass, dispatch_group, PB_TumbioloDowning_1994, PB_Brey_1999, PB_Gascuel_2008, PB, PB_method,
          QB_ChristensenEtAl_2008, QB, QB_method)]
}

## =================================================================
## RUN
## =================================================================

results <- rbindlist(list(
  if (nrow(species_df[dispatch_group == "fish"]) > 0) calc_fish(species_df[dispatch_group == "fish"]),
  if (nrow(species_df[dispatch_group == "mammal"]) > 0) calc_mammal(species_df[dispatch_group == "mammal"]),
  if (nrow(species_df[dispatch_group == "seabird"]) > 0) calc_seabird(species_df[dispatch_group == "seabird"]),
  if (nrow(species_df[dispatch_group == "invertebrate"]) > 0) calc_invertebrate(species_df[dispatch_group == "invertebrate"])
), fill = TRUE)
STAGE_PB$tick(tokens = list(stage_name = "Calculate PB/QB (all groups)"))

phyto_flagged <- species_df[dispatch_group == "phytoplankton",
                            .(Species, FG, Biomass, note = "phytoplankton - needs separate production sampling data, not computed here")]

## --- Brey (2012) manual-review flag ----------------------------------
## Brey's ANN model is the current best-practice standard for benthic
## invertebrate P/B (multi-parameter neural network, 1252 training
## datasets - a real improvement over Tumbiolo & Downing 1994 used
## above), but it's a trained network with no published weights or
## maintained R package I could find - not something to fake an
## approximation of and mislabel. Instead: flag the highest-biomass
## invertebrate species (where getting P/B right matters most) for
## manual cross-check against Brey's own calculator, rather than
## silently leaving this as a known gap across the whole list.
brey_candidates <- results[dispatch_group == "invertebrate"][order(-Biomass)][1:min(10, .N)]
message("\nTop invertebrate species by biomass - worth a manual Brey (2012) cross-check",
        " (see Thomas Brey's Virtual Handbook calculator) since that model is more",
        " accurate than the Tumbiolo & Downing/Gascuel fallback used here, but isn't",
        " automatable (no accessible weights or R package):")
print(brey_candidates[, .(Species, FG, Biomass, PB, PB_method)])
fwrite(brey_candidates, "invertebrates_for_brey_manual_check.csv")

message("\n=== Species-level PB/QB - which method was CHOSEN for FG weighting ===")
print(results[, .N, by = PB_method])
print(results[, .N, by = QB_method])

message("\n=== Direct vs. taxonomically-borrowed values ===")
source_cols <- grep("_source$", names(results), value = TRUE)
if (length(source_cols) > 0) {
  for (col in source_cols) {
    tab <- results[, .N, by = col]
    setnames(tab, col, "status")
    message(sub("_source$", "", col), ":")
    print(tab[order(status)])
  }
  message("Borrowed values are tagged in the *_source columns in the CSV export -",
          " treat these as weaker evidence than direct computations, especially",
          " anything borrowed at Order or Class level (very broad relatives).")
}

message("\n=== Coverage of EVERY individual method attempted (not just the chosen one) ===")
message("Fish PB methods:")
print(results[dispatch_group == "fish", .(
  Pauly = sum(!is.na(PB_Pauly_1980)), FishLife = sum(!is.na(PB_FishLife_2023)), Gascuel = sum(!is.na(PB_Gascuel_2008)),
  Hoenig = sum(!is.na(PB_Hoenig_1983)), Then2015 = sum(!is.na(PB_Then_2015)), AlversonCarney = sum(!is.na(PB_AlversonCarney_1975))
)])
message("Fish QB methods:")
print(results[dispatch_group == "fish", .(
  PalomaresPauly_Z = sum(!is.na(QB_PalomaresPauly_1998Z)), PalomaresPauly_noZ = sum(!is.na(QB_PalomaresPauly_1998noZ)),
  ChristensenPauly_1992 = sum(!is.na(QB_ChristensenPauly_1992)), QP3 = sum(!is.na(QB_ChristensenEtAl_2008))
)])
message("These per-method columns (PB_Pauly_1980, PB_FishLife_2023, PB_Gascuel_2008, QB_PalomaresPauly_1998Z, QB_PalomaresPauly_1998noZ,",
        " QB_ChristensenPauly_1992, QB_ChristensenEtAl_2008, etc.) are all preserved in species_pb_qb_by_taxon_group.csv -",
        " worth comparing them directly for any species where the methods disagree a lot,",
        " since that's a more useful signal than trusting whichever one happened to be chosen.")

## =================================================================
## Biomass-weighted average up to Functional Group level
## =================================================================

fg_weighted <- results[!is.na(PB) | !is.na(QB), .(
  PB_FG = sum(Biomass * PB, na.rm = TRUE) / sum(Biomass[!is.na(PB)], na.rm = TRUE),
  QB_FG = sum(Biomass * QB, na.rm = TRUE) / sum(Biomass[!is.na(QB)], na.rm = TRUE),
  n_species_with_PB = sum(!is.na(PB)),
  n_species_with_QB = sum(!is.na(QB)),
  n_species_total = .N,
  biomass_coverage_PB = sum(Biomass[!is.na(PB)], na.rm = TRUE) / sum(Biomass, na.rm = TRUE),
  biomass_coverage_QB = sum(Biomass[!is.na(QB)], na.rm = TRUE) / sum(Biomass, na.rm = TRUE)
), by = FG]

message("\n=== FG-level PB/QB (biomass-weighted average) ===")
print(fg_weighted[order(FG)])

message("\nFGs with LOW biomass coverage (<50%):")
print(fg_weighted[biomass_coverage_PB < 0.5 | biomass_coverage_QB < 0.5,
                  .(FG, biomass_coverage_PB, biomass_coverage_QB, n_species_total)])
STAGE_PB$tick(tokens = list(stage_name = "Aggregate to FG level"))

## =================================================================
## Export
## =================================================================

fwrite(results, "species_pb_qb_by_taxon_group.csv")
fwrite(fg_weighted, "fg_pb_qb_weighted.csv")
fwrite(phyto_flagged, "phytoplankton_needs_separate_method.csv")

message("\nSaved: species_pb_qb_by_taxon_group.csv, fg_pb_qb_weighted.csv,",
        " phytoplankton_needs_separate_method.csv")

## =================================================================
## Method comparison plots - fish only, since that's the group with
## the most independent methods (6 for PB, 4 for QB) and therefore the
## most informative to compare. One point per method per species,
## faceted by species so you can see at a glance how much the methods
## agree or disagree - the CHOSEN method (the one actually used for the
## FG-weighted average) is highlighted distinctly from the rest.
## =================================================================

fish_results <- results[dispatch_group == "fish"]

## --- PB methods ---------------------------------------------------------

pb_cols <- c("PB_Pauly_1980", "PB_FishLife_2023", "PB_Gascuel_2008", "PB_Hoenig_1983", "PB_Then_2015", "PB_AlversonCarney_1975")
fish_pb_long <- melt(fish_results[, c("Species", "PB_method", ..pb_cols)],
                     id.vars = c("Species", "PB_method"), variable.name = "method", value.name = "PB")
fish_pb_long[, method := gsub("^PB_", "", method)]
fish_pb_long <- fish_pb_long[!is.na(PB)]

## mark which row corresponds to the method actually chosen - PB_method
## has suffixes like "+F(Y/B)" appended, so match on the method name
## being contained in PB_method rather than requiring an exact match.
## Keys here must match what gsub("^PB_", "", ...) produces from the
## Estimate_Author_Year column names above.
method_label_map <- c(Pauly_1980 = "Pauly 1980", FishLife_2023 = "FishLife",
                      Gascuel_2008 = "Gascuel 2008", Hoenig_1983 = "Hoenig 1983",
                      Then_2015 = "Then et al. 2015", AlversonCarney_1975 = "Alverson & Carney 1975")
fish_pb_long[, is_chosen := mapply(function(m, chosen) grepl(method_label_map[[m]], chosen, ignore.case = TRUE),
                                   method, PB_method)]

p_pb <- ggplot(fish_pb_long, aes(x = method, y = PB)) +
  geom_point(aes(color = is_chosen, size = is_chosen)) +
  scale_color_manual(values = c(`TRUE` = "firebrick", `FALSE` = "grey50"),
                     labels = c(`TRUE` = "Chosen for FG average", `FALSE` = "Other method"),
                     name = NULL) +
  scale_size_manual(values = c(`TRUE` = 4, `FALSE` = 2.5), guide = "none") +
  facet_wrap(~ Species, scales = "free_y") +
  theme_bw(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
  labs(title = "P/B method comparison - fish species", x = NULL, y = expression(P/B~(year^-1)))

ggsave("fish_PB_methods_comparison.png", p_pb, width = 12, height = 9, dpi = 150)

## --- QB methods ---------------------------------------------------------

qb_cols <- c("QB_PalomaresPauly_1998Z", "QB_PalomaresPauly_1998noZ", "QB_ChristensenPauly_1992", "QB_ChristensenEtAl_2008")
fish_qb_long <- melt(fish_results[, c("Species", "QB_method", ..qb_cols)],
                     id.vars = c("Species", "QB_method"), variable.name = "method", value.name = "QB")
fish_qb_long[, method := gsub("^QB_", "", method)]
fish_qb_long <- fish_qb_long[!is.na(QB)]

qb_label_map <- c(PalomaresPauly_1998Z = "Z-based", PalomaresPauly_1998noZ = "non-Z",
                  ChristensenPauly_1992 = "Christensen & Pauly", ChristensenEtAl_2008 = "Q/P=3")
fish_qb_long[, is_chosen := mapply(function(m, chosen) grepl(qb_label_map[[m]], chosen, fixed = TRUE),
                                   method, QB_method)]

p_qb <- ggplot(fish_qb_long, aes(x = method, y = QB)) +
  geom_point(aes(color = is_chosen, size = is_chosen)) +
  scale_color_manual(values = c(`TRUE` = "firebrick", `FALSE` = "grey50"),
                     labels = c(`TRUE` = "Chosen for FG average", `FALSE` = "Other method"),
                     name = NULL) +
  scale_size_manual(values = c(`TRUE` = 4, `FALSE` = 2.5), guide = "none") +
  facet_wrap(~ Species, scales = "free_y") +
  theme_bw(base_size = 11) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position = "bottom") +
  labs(title = "Q/B method comparison - fish species", x = NULL, y = expression(Q/B~(year^-1)))

ggsave("fish_QB_methods_comparison.png", p_qb, width = 12, height = 9, dpi = 150)

print(p_pb)
print(p_qb)

message("\nSaved: fish_PB_methods_comparison.png, fish_QB_methods_comparison.png")
STAGE_PB$tick(tokens = list(stage_name = "Export CSVs + generate plots"))

## =================================================================
## Total pipeline runtime
## =================================================================
elapsed <- Sys.time() - PIPELINE_START_TIME
message("\n", strrep("=", 50))
message("Total pipeline runtime: ", round(as.numeric(elapsed, units = "mins"), 2), " minutes",
        " (", round(as.numeric(elapsed, units = "secs"), 1), " seconds)")
message(strrep("=", 50))
