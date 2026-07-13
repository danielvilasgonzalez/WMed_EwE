## =================================================================
## WMed EwE Model - Catch data pipeline
##
## Structure: config-driven instead of repeated if/else branches per
## stage. GFCM_2025 and FAO_2026 share one processing path (they're
## structurally identical - same CL_FI_* reference files); FAO_2020
## stays separate since it's a genuinely different source format.
##
## NOTE on FAO_2026: the global FAO capture database does NOT have
## the fine 37.1.1/37.1.2/37.1.3 division breakdown that GFCM's own
## regional database has - it only reports at Major Fishing Area
## (37) level. The area-filter config below reflects this explicitly
## rather than silently applying a division filter that wouldn't work.
## =================================================================

library(data.table)
library(ggplot2)

setwd("/Users/daniel/Work/iMARES/")
BASE_DIR   <- "./WMed EwE Model"
START_YEAR <- 1995
TOP_N_SPECIES <- 20
OUT_DIR <- file.path(BASE_DIR, "data", "processed")
if (!dir.exists(OUT_DIR)) dir.create(OUT_DIR, recursive = TRUE)

DATASET_VERSION <- "GFCM_2025"   # "FAO_2020" | "GFCM_2025" | "FAO_2026"

## =================================================================
## Dataset registry - one entry per source, all config in one place.
## Adding a new dataset version means adding ONE entry here, not
## touching six separate processing blocks.
## =================================================================

DATASETS <- list(
  GFCM_2025 = list(
    format = "gfcm_regional",
    data_dir = file.path(BASE_DIR, "data/FI_Regional_2025.1.0"),
    capture_file   = "GFCM_Capture_Quantity.csv",
    species_file   = "CL_FI_SPECIES_GROUPS.csv",
    countries_file = "CL_FI_COUNTRY_GROUPS.csv",
    area_file      = "CL_FI_WATERAREA_DIVISION.csv",
    area_code_col  = "DIVISION.CODE",     # column in capture data to filter on
    area_join_col  = "Code",              # matching column in the area reference file
    area_filter_values = c("37.1.1", "37.1.2", "37.1.3"),
    fg_file = file.path(BASE_DIR, "data/FG_WMed.xlsx")
  ),
  FAO_2026 = list(
    format = "gfcm_regional",   # same processing shape as GFCM_2025
    data_dir = file.path(BASE_DIR, "data/Capture_2026.1.0"),
    capture_file   = "Capture_Quantity.csv",
    species_file   = "CL_FI_SPECIES_GROUPS.csv",
    countries_file = "CL_FI_COUNTRY_GROUPS.csv",
    area_file      = "CL_FI_WATERAREA_GROUPS.csv",
    area_code_col  = "AREA.CODE",
    area_join_col  = "Code",
    ## global FAO data only has Major Fishing Area level (37), not the
    ## finer 37.1.x subdivisions GFCM's own regional database has -
    ## filtering to "37" here reflects that limitation explicitly
    area_filter_values = c("37"),
    fg_file = file.path(BASE_DIR, "data/raw/FG_WMed.xlsx")
  ),
  FAO_2020 = list(
    format = "legacy_excel",
    catch_file = file.path(BASE_DIR, "data/raw/FAO-GFCM-CapturepProduction-1970_2020.xlsx"),
    fg_file    = file.path(BASE_DIR, "data/FG_WMed.xlsx")
  )
)

cfg <- DATASETS[[DATASET_VERSION]]

## =================================================================
## Loaders - one function per format, not per dataset version
## =================================================================

load_gfcm_regional <- function(cfg) {
  p <- function(f) file.path(cfg$data_dir, f)
  
  capture   <- fread(p(cfg$capture_file))
  species   <- fread(p(cfg$species_file))[, .(SPECIES.ALPHA_3_CODE = `3A_Code`, Species = Name_En)]
  countries <- fread(p(cfg$countries_file))[, .(COUNTRY.UN_CODE = UN_Code, Country = Name_En)]
  area_ref  <- fread(p(cfg$area_file))
  setnames(area_ref, cfg$area_join_col, "AreaCode")
  area_ref  <- unique(area_ref[, .(AreaCode, Division = Name_En)])
  
  capture <- merge(capture, species, by = "SPECIES.ALPHA_3_CODE", all.x = TRUE)
  capture <- merge(capture, countries, by = "COUNTRY.UN_CODE", all.x = TRUE)
  setnames(capture, cfg$area_code_col, "AreaCode")
  capture <- merge(capture, area_ref, by = "AreaCode", all.x = TRUE)
  capture[, PERIOD := as.numeric(PERIOD)]
  
  capture
}

load_legacy_excel <- function(cfg) {
  as.data.table(readxl::read_excel(cfg$catch_file))
}

## =================================================================
## Build Western Med time series - one function, format-aware only
## where genuinely necessary (column names differ by source)
## =================================================================

build_westmed_timeseries <- function(cfg) {
  if (cfg$format == "gfcm_regional") {
    capture <- load_gfcm_regional(cfg)
    
    westmed <- capture[
      AreaCode %in% cfg$area_filter_values &
        MEASURE == "Q_tlw" &
        PERIOD >= START_YEAR
    ]
    
    country_ts <- westmed[, .(Catch = sum(VALUE, na.rm = TRUE)),
                          by = .(Year = PERIOD, Country, Division)]
    species_ts <- westmed[, .(Catch = sum(VALUE, na.rm = TRUE)),
                          by = .(Year = PERIOD, Species, Division)]
    
    list(westmed = westmed, country_ts = country_ts, species_ts = species_ts,
         value_col = "VALUE", species_col = "Species")
    
  } else if (cfg$format == "legacy_excel") {
    catch_data <- load_legacy_excel(cfg)
    setnames(catch_data,
             c("Species (scientific name)", "Area (FAO division)", "Area (FAO subarea)"),
             c("Species", "Division", "Subarea"), skip_absent = TRUE)
    
    westmed <- catch_data[Subarea == "Western Med (37.1)" & Year >= START_YEAR]
    
    country_ts <- westmed[, .(Catch = sum(Quantity, na.rm = TRUE)),
                          by = .(Year, Country, Division)]
    species_ts <- westmed[, .(Catch = sum(Quantity, na.rm = TRUE)),
                          by = .(Year, Species, Division)]
    
    list(westmed = westmed, country_ts = country_ts, species_ts = species_ts,
         value_col = "Quantity", species_col = "Species")
  }
}

ts_data <- build_westmed_timeseries(cfg)
message("Dataset loaded and processed: ", DATASET_VERSION)

## =================================================================
## Functional Group matching
## Delegates to the dedicated matching pipeline (common-name bridge,
## FAO genus fallback, ambiguity-safe resolution, manual overrides -
## see species_fg_matching_final.R) rather than a naive anti_join,
## which only catches exact scientific-name matches and silently
## leaves everything else unresolved.
## =================================================================

fg <- as.data.table(readxl::read_excel(cfg$fg_file, sheet = 4))

unmatched_species <- ts_data$westmed[
  !get(ts_data$species_col) %in% fg$ESPECIE,
  .(Catch = sum(get(ts_data$value_col), na.rm = TRUE)),
  by = c(ts_data$species_col)
][order(-Catch)]
setnames(unmatched_species, ts_data$species_col, "Species")
unmatched_species[, `:=`(FG_num = NA, FG_name = NA)]

message(nrow(unmatched_species), " species need FG matching - running matching pipeline...")
source("species_fg_matching_final.R")   # uses unmatched_species + fg from this environment,
# produces `output` (species_fg_matched.csv)

## =================================================================
## Summary statistics
## =================================================================

top_species <- ts_data$westmed[
  , .(TotalCatch = sum(get(ts_data$value_col), na.rm = TRUE)),
  by = c(ts_data$species_col)
][order(-TotalCatch)][1:TOP_N_SPECIES, get(ts_data$species_col)]

species_plot <- ts_data$species_ts[Species %in% top_species]

## =================================================================
## Plots - one shared definition, both sources feed the same schema
## (Year, Country/Species, Division, Catch) by this point
## =================================================================

p_country <- ggplot(ts_data$country_ts, aes(x = Year, y = Catch, colour = Country)) +
  geom_line() +
  facet_wrap(~ Division, scales = "free_y") +
  theme_bw() +
  labs(x = "Year", y = "Catch (t)")

p_species <- ggplot(species_plot, aes(x = Year, y = Catch, colour = Division, group = Division)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ Species, scales = "free_y", ncol = 4) +
  theme_bw() +
  labs(x = "Year", y = "Catch (t)", colour = NULL) +
  theme(legend.position = "bottom", strip.text = element_text(size = 8))

print(p_country)
print(p_species)

## =================================================================
## Export
## =================================================================

fwrite(unmatched_species, file.path(OUT_DIR, "unmatched_species_FG_assignment.csv"))
fwrite(ts_data$species_ts, file.path(OUT_DIR, "westmed_species_timeseries.csv"))

ggsave(file.path(OUT_DIR, "country_catch_timeseries.png"), p_country, width = 12, height = 8, dpi = 300)
ggsave(file.path(OUT_DIR, "species_catch_timeseries.png"), p_species, width = 14, height = 10, dpi = 300)

message("Analysis completed.")

# unmatched species ####

## =================================================================
## Species -> Functional Group matching pipeline (clean, final)
##
## Goal: assign FG_num / FG_name to every species in unmatched_species.
##
## Inputs assumed to exist:
##   unmatched_species : data.table - Species (common name), Catch,
##                        FG_num (NA), FG_name (NA)
##   fg                : FG reference - GF, FG_name, ESPECIE (sci name)
##
## Matching stages, in order of confidence:
##   1. Direct match: Species text == an FG_name itself
##   2. Common name -> scientific name (FishBase + SeaLifeBase, queried
##      FOR the known species list - not searched from all of FishBase,
##      which caused wrong-species matches in an earlier version)
##   3. FAO genus-level fallback (for NEI/aggregate categories that
##      aren't real species, e.g. "Sardinellas nei")
##   4. Word-containment fallback (for anything still unmatched)
##
## Every stage uses the SAME safety rule: a query is only auto-resolved
## if it maps to exactly ONE distinct FG. Anything mapping to multiple
## different FGs is flagged for manual review, never guessed - this is
## what prevents a single Catch value from being silently duplicated
## across multiple species/FGs.
## =================================================================

library(data.table)
library(stringr)
if (!requireNamespace("rfishbase", quietly = TRUE)) install.packages("rfishbase")
library(rfishbase)

setDT(unmatched_species)
setDT(fg)

## --- Shared safety helper, used at every matching stage --------------------
## query_col: the original species name being matched
## fg_col:    the FG_num column in the merged candidate table
## Only collapses to one row when a query maps to exactly one distinct FG;
## anything ambiguous is returned separately, never auto-picked.
resolve_matches_safely <- function(merged_dt, query_col = "Species", fg_col = "FG_num") {
  n_fg <- merged_dt[, .(n_distinct_fg = uniqueN(get(fg_col))), by = query_col]
  safe_queries <- n_fg[n_distinct_fg == 1][[query_col]]
  ambiguous_queries <- n_fg[n_distinct_fg > 1][[query_col]]
  list(
    safe = unique(merged_dt[get(query_col) %in% safe_queries], by = query_col),
    ambiguous = merged_dt[get(query_col) %in% ambiguous_queries]
  )
}

extract_genus <- function(sci_name) str_extract(sci_name, "^[A-Za-z]+")

## =================================================================
## STEP 0: data quality - set aside rows with no species name
## =================================================================

blank_row <- unmatched_species[is.na(Species) | Species == ""]
if (nrow(blank_row) > 0) {
  message(nrow(blank_row), " row(s) with blank Species name - excluded,",
          " investigate separately (upstream data issue):")
  print(blank_row)
}
unmatched_species <- unmatched_species[!is.na(Species) & Species != ""]

fg_lookup <- unique(fg[, .(ScientificName = ESPECIE, FG_num = GF, FG_name)])
fg_lookup[, genus := extract_genus(ScientificName)]

## =================================================================
## STEP 1: direct match - Species text is itself an FG_name
## e.g. "Echinoderms" catch record matching straight to the FG
## literally named "Echinoderms"
## =================================================================

fg_name_lookup <- unique(fg[, .(Species = FG_name, FG_num = GF, FG_name)])
direct_merged <- merge(unmatched_species[, .(Species, Catch)], fg_name_lookup, by = "Species")
direct_resolved <- resolve_matches_safely(direct_merged)

direct_matches <- direct_resolved$safe[, .(Species, FG_num, FG_name)]
direct_matches[, match_method := "direct_fg_name"]

message("STEP 1 - Direct Species==FG_name matches: ", nrow(direct_matches))

## =================================================================
## STEP 2: common name -> scientific name, queried FOR the known
## species list (not searched from all of FishBase/SeaLifeBase -
## that caused wrong-species matches, since common names aren't
## globally unique, e.g. "Red mullet" also names an Australian species)
## =================================================================

sci_names <- unique(fg_lookup$ScientificName)
sci_names <- sci_names[str_detect(sci_names, "^[A-Z][a-z]+ [a-z]+$")]  # proper binomials only

message("\nSTEP 2 - Querying common names for ", length(sci_names), " species...")

fb_common  <- tryCatch(as.data.table(common_names(sci_names, server = "fishbase")),
                       error = function(e) data.table())
slb_common <- tryCatch(as.data.table(common_names(sci_names, server = "sealifebase")),
                       error = function(e) data.table())

fb_lookup <- unique(
  rbindlist(list(fb_common, slb_common), fill = TRUE)[
    Language == "English" & !is.na(ComName), .(ScientificName = Species, Species = ComName)]
)
message("Common names found: ", nrow(fb_lookup), " covering ",
        uniqueN(fb_lookup$ScientificName), " of ", length(sci_names), " species")

clean_name <- function(x) {
  x <- str_remove(x, "\\(.*\\)")
  x <- str_remove(x, regex("\\bnei\\b", ignore_case = TRUE))
  x <- str_remove(x, "'s\\b")
  str_to_lower(str_squish(x))
}

remaining <- unmatched_species[!Species %in% direct_matches$Species]
remaining[, clean_species := clean_name(Species)]
fb_lookup[, clean_species := clean_name(Species)]

exact_merged <- merge(remaining[, .(Species, Catch, clean_species)],
                      fb_lookup[, .(clean_species, ScientificName)],
                      by = "clean_species", allow.cartesian = TRUE)
exact_merged <- merge(exact_merged, fg_lookup[, .(ScientificName, FG_num, FG_name)], by = "ScientificName")
exact_resolved <- resolve_matches_safely(exact_merged)

exact_matches <- exact_resolved$safe[, .(Species, FG_num, FG_name)]
exact_matches[, match_method := "fishbase_common_name"]

## Distinguish genuine ambiguity (different species sharing a wrong/loose
## common name, e.g. "Turbot" incorrectly listing Balistes capriscus)
## from life-stage splits (the SAME species appearing as separate FG rows
## for juvenile/adult stanzas - a normal Ecopath model structure, not an
## error - a single catch record genuinely belongs to multiple FGs here)
if (nrow(exact_resolved$ambiguous) > 0) {
  ambiguity_type <- exact_resolved$ambiguous[, .(n_species = uniqueN(ScientificName)), by = Species]
  exact_resolved$ambiguous <- merge(exact_resolved$ambiguous, ambiguity_type, by = "Species")
  exact_resolved$ambiguous[, ambiguity_type := fifelse(
    n_species == 1, "life_stage_split", "genuine_name_ambiguity"
  )]
}

message("STEP 2 - Resolved: ", nrow(exact_matches),
        " | Ambiguous (multiple FGs, excluded): ", uniqueN(exact_resolved$ambiguous$Species))
if (uniqueN(exact_resolved$ambiguous$Species) > 0) {
  print(unique(exact_resolved$ambiguous[, .(Species, ScientificName, FG_num, FG_name, ambiguity_type)]))
}

## =================================================================
## STEP 3: FAO genus-level fallback for NEI/aggregate categories
## (e.g. "Sardinellas nei" -> genus Sardinella -> FG)
## =================================================================

remaining <- remaining[!Species %in% exact_matches$Species]

find_or_download_fao_species <- function() {
  found <- list.files(".", pattern = "CL_FI_SPECIES_GROUPS.csv$",
                      recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  if (length(found) > 0) return(found[1])
  url <- "https://data.apps.fao.org/catalog/dataset/b70c52c1-475f-4951-a8ac-de44016abd9b/resource/2c0f936d-6c36-4715-9c7f-fa5a70c00249/download/cl_fi_species_groups.csv"
  destfile <- "CL_FI_SPECIES_GROUPS.csv"
  download.file(url, destfile = destfile, mode = "wb", method = "libcurl")
  destfile
}

fao_species <- fread(find_or_download_fao_species(), encoding = "UTF-8")
name_col <- grep("english|name.*en$|^name$", names(fao_species), ignore.case = TRUE, value = TRUE)[1]
sci_col  <- grep("scientific", names(fao_species), ignore.case = TRUE, value = TRUE)[1]
setnames(fao_species, c(name_col, sci_col), c("Name_En", "Scientific_Name"), skip_absent = TRUE)
fao_species[, genus := extract_genus(Scientific_Name)]

remaining <- merge(remaining, unique(fao_species[, .(Name_En, genus)], by = "Name_En"),
                   by.x = "Species", by.y = "Name_En", all.x = TRUE)

genus_merged <- merge(remaining[!is.na(genus), .(Species, Catch, genus)],
                      fg_lookup[!is.na(genus), .(genus, FG_num, FG_name)],
                      by = "genus", allow.cartesian = TRUE)
genus_resolved <- resolve_matches_safely(genus_merged)

genus_matches <- genus_resolved$safe[, .(Species, FG_num, FG_name)]
genus_matches[, match_method := "fao_genus"]

message("\nSTEP 3 - Resolved via genus: ", nrow(genus_matches),
        " | Ambiguous (excluded): ", uniqueN(genus_resolved$ambiguous$Species))
if (uniqueN(genus_resolved$ambiguous$Species) > 0) {
  print(unique(genus_resolved$ambiguous[, .(Species, genus, FG_num, FG_name)]))
}

## =================================================================
## STEP 4: word-containment fallback for anything still unmatched
## =================================================================

remaining <- remaining[!Species %in% genus_matches$Species]

FILLER_WORDS <- c("nei", "spp", "sp", "etc", "and", "or", "the", "of")
tokenize <- function(x) {
  words <- str_split(str_to_lower(str_remove_all(x, "[,().']")), "\\s+")[[1]]
  words[!words %in% FILLER_WORDS & words != ""]
}

fb_with_fg <- merge(fb_lookup, fg_lookup[, .(ScientificName, FG_num, FG_name)], by = "ScientificName")
candidate_names <- unique(fb_with_fg$Species)
candidate_tokens <- setNames(lapply(candidate_names, tokenize), candidate_names)

match_by_containment <- function(query) {
  q_tokens <- tokenize(query)
  if (length(q_tokens) == 0) return(NULL)
  scores <- vapply(candidate_tokens, function(c_tokens) {
    if (length(c_tokens) == 0) return(0)
    sum(q_tokens %in% c_tokens) / length(q_tokens)
  }, numeric(1))
  best <- which(scores == 1)   # full containment only - partial is too unreliable to auto-use
  if (length(best) == 0) return(NULL)
  data.table(Species = query, candidate = names(candidate_tokens)[best])
}

containment_results <- rbindlist(lapply(remaining$Species, match_by_containment))

if (nrow(containment_results) > 0) {
  containment_merged <- merge(containment_results, fb_with_fg[, .(Species, FG_num, FG_name)],
                              by.x = "candidate", by.y = "Species")
  containment_resolved <- resolve_matches_safely(containment_merged)
  containment_matches <- containment_resolved$safe[, .(Species, FG_num, FG_name)]
  containment_matches[, match_method := "word_containment"]
  
  message("\nSTEP 4 - Resolved via word containment: ", nrow(containment_matches),
          " | Ambiguous (excluded): ", uniqueN(containment_resolved$ambiguous$Species))
} else {
  containment_matches <- data.table(Species = character(), FG_num = numeric(),
                                    FG_name = character(), match_method = character())
  message("\nSTEP 4 - No word-containment matches found.")
}

## =================================================================
## COMBINE + FINAL RESULT
## =================================================================

all_matches <- rbindlist(list(direct_matches, exact_matches, genus_matches, containment_matches))

resolved <- merge(unmatched_species[, .(Species, Catch)], all_matches, by = "Species", all.x = TRUE)
still_failing <- resolved[is.na(FG_num)]

## sanity check - Catch must never be duplicated by any matching stage
original_total <- sum(unmatched_species$Catch)
resolved_total <- sum(resolved[!is.na(FG_num)]$Catch, na.rm = TRUE) +
  sum(unmatched_species[Species %in% still_failing$Species]$Catch)
message("\n=== CATCH TOTAL CHECK (must match exactly) ===")
message("Original total: ", sum(unmatched_species$Catch))
message("Sum across resolved+unresolved: ", sum(resolved$Catch))
if (abs(sum(unmatched_species$Catch) - sum(resolved$Catch)) > 0.01) {
  message("WARNING: totals differ - duplication somewhere, do not trust results yet.")
} else {
  message("OK - no duplication.")
}

message("\n=== SUMMARY (before manual overrides) ===")
message("Total species: ", nrow(resolved))
message("Resolved: ", nrow(resolved) - nrow(still_failing),
        " (", round(100 * (nrow(resolved) - nrow(still_failing)) / nrow(resolved), 1), "%)")
message("By method:")
print(all_matches[, .N, by = match_method])

## =================================================================
## MANUAL OVERRIDES
## Add a row per species you've manually confirmed the correct
## scientific name for (e.g. Turbot really is Scophthalmus maximus,
## not Balistes capriscus, which was a wrong FishBase/SeaLifeBase
## common-name entry). The script looks up that scientific name's
## real FG from fg_lookup - you never need to hardcode an FG number
## by hand, so this stays correct even if fg_lookup changes.
## =================================================================

MANUAL_OVERRIDES <- data.table(
  Species = c("Turbot"),
  CorrectScientificName = c("Scophthalmus maximus")
  # add more rows here as you confirm them, e.g.:
  # "European hake" -> "Merluccius merluccius" (if the ambiguity turns out
  # to be genuine mismatch rather than a life-stage split - check the
  # ambiguity_type column from STEP 2 first)
)

overrides_resolved <- merge(MANUAL_OVERRIDES, fg_lookup[, .(ScientificName, FG_num, FG_name)],
                            by.x = "CorrectScientificName", by.y = "ScientificName")
overrides_resolved[, match_method := "manual_override"]

unresolved_override_names <- setdiff(MANUAL_OVERRIDES$Species, overrides_resolved$Species)
if (length(unresolved_override_names) > 0) {
  message("\nWARNING - these override scientific names weren't found in fg_lookup",
          " at all - check spelling/whether they're really in your FG reference:")
  print(unresolved_override_names)
}

## overrides replace whatever the automatic stages produced (or didn't)
## for these species
resolved_final <- resolved[!Species %in% overrides_resolved$Species]
resolved_final <- rbindlist(list(
  resolved_final,
  merge(unmatched_species[Species %in% overrides_resolved$Species, .(Species, Catch)],
        overrides_resolved[, .(Species, FG_num, FG_name, match_method)], by = "Species")
), use.names = TRUE)

still_failing_final <- resolved_final[is.na(FG_num)]

message("\n=== FINAL SUMMARY (after ", nrow(overrides_resolved), " manual override(s)) ===")
message("Resolved: ", nrow(resolved_final) - nrow(still_failing_final), " of ", nrow(resolved_final),
        " (", round(100 * (nrow(resolved_final) - nrow(still_failing_final)) / nrow(resolved_final), 1), "%)")

## final Catch total check, including overrides
if (abs(sum(unmatched_species$Catch) - sum(resolved_final$Catch)) > 0.01) {
  message("WARNING: totals differ after overrides - investigate before trusting results.")
} else {
  message("Catch total check OK - no duplication.")
}

## =================================================================
## SAVE EVERYTHING TO CSV FOR MANUAL REVIEW/CORRECTION
## One row per species, with status and method so you can filter and
## fix in Excel, then re-import if needed.
## =================================================================

resolved_final[, status := fifelse(is.na(FG_num), "unresolved", "resolved")]

## attach ambiguity detail (if any) for species that ended up unresolved,
## so the CSV explains WHY rather than just showing a blank
ambiguity_notes <- rbindlist(list(
  if (nrow(exact_resolved$ambiguous) > 0) unique(exact_resolved$ambiguous[, .(Species, note = ambiguity_type)]) else NULL,
  if (nrow(genus_resolved$ambiguous) > 0) unique(genus_resolved$ambiguous[, .(Species, note = "ambiguous_genus_match")]) else NULL
), use.names = TRUE, fill = TRUE)

output <- merge(resolved_final, ambiguity_notes, by = "Species", all.x = TRUE)
setorder(output, status, -Catch)

fwrite(output, "species_fg_matched.csv")
message("\nSaved full results to species_fg_matched.csv (", nrow(output), " rows) -",
        " open in Excel to review/correct 'unresolved' rows, or anything you want to double-check.")
