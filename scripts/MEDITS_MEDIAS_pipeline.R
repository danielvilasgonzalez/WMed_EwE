## MEDBS Survey Analysis: Demersal & Acoustic, with Functional ####
## Group matching, STRATA-WEIGHTED survey-index aggregation, and plots

## --- 0. Packages -----------------------------------------------------------
#load libraries
pkgs <- c("readr", "dplyr", "tidyr", "ggplot2", "stringr", "forcats", "data.table",
          "marmap", "raster", "terra", "sf")
new_pkgs <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new_pkgs) > 0) install.packages(new_pkgs)
invisible(lapply(pkgs, library, character.only = TRUE))

## --- 1. Config ---------------------------------------------------------
#set working directory - Github
if (tolower(Sys.info()[["user"]]) == "daniel") {
  setwd("/Users/daniel/Documents/Github/")
} else {
  if (!requireNamespace("rstudioapi", quietly = TRUE) ||
      !rstudioapi::isAvailable()) {
    stop(
      "This script requires RStudio. Please set the working directory manually."
    )
  }
  rstudioapi::showQuestion(
    title = "Select GitHub Directory",
    message = paste(
      "Please select your local GitHub directory.",
      "\n\nBefore continuing, ensure that you have cloned:",
      "\ngithub.com/danielvilasgonzalez/WMed_EwE"
    )
  )
  wd <- rstudioapi::selectDirectory()
  if (is.null(wd) || wd == "" || !dir.exists(wd)) {
    stop("No valid GitHub directory selected.")
  }
  setwd(wd)
}

#MEDITS data folder
in_dir <- "./WMed_EwE/data/raw/2024_MEDBSsurvey/"
#output folder
if (tolower(Sys.info()[["user"]]) == "daniel") {
  out_dir <- "/Users/daniel/Work/iMARES/WMed EwE Model/data/processed/"
} else {
  if (!requireNamespace("rstudioapi", quietly = TRUE) ||
      !rstudioapi::isAvailable()) {
    stop(
      "This script requires RStudio. Please select the output directory manually."
    )
  }
  rstudioapi::showQuestion(
    title = "Select Output Directory",
    message = paste(
      "Please select the directory where output files",
      "and intermediate results will be saved."
    )
  )
  out_dir <- rstudioapi::selectDirectory()
  if (is.null(out_dir) || out_dir == "" || !dir.exists(out_dir)) {
    stop("No valid output directory selected.")
  }
}

#fg, species and taxonomy files
fg_file           <- "./WMed_EwE/data/raw/FG_WMed.xlsx"
fao_species_file  <- "./WMed_EwE/data/raw/FI_Regional_2025.1.0/CL_FI_SPECIES_GROUPS.csv" #FAO list of species
tm_list_file      <- "./WMed_EwE/data/raw/2024_MEDBSsurvey/TM_list_(April_2019).xlsx" #taxonomic list downloaded from MEDITS

#filters if modification is needed
FILTER_COUNTRIES <- NULL    # Country filter
FILTER_AREAS     <- 1:11   # GSA filter
TOP_N_AREAS      <- 20    # filter by most abundant areas
TOP_N_SPECIES    <- 40    # also used as top-N when faceting by FG instead of species

## --- 2. FG reference ---------------------------------------------------------
#read fg and species from reference excel file
fg <- as.data.table(readxl::read_excel(fg_file, sheet = 4))
fg_lookup <- unique(fg[, .(ScientificName = ESPECIE, FG_num = GF, FG_name)])

## --- 2b. Safe-dedup fg_lookup by ScientificName before ANY merge ----------
#remove juvenile or other multistanza groups to avoid duplicate observations (1sp -- 1 fg)
JUV_PATTERN <- regex("\\bjuv\\.?\\b|juvenile", ignore_case = TRUE)
n_juv_dropped <- fg_lookup[str_detect(FG_name, JUV_PATTERN), .N]
message(n_juv_dropped, " juvenile-labeled FG row(s) dropped, keeping the adult FG",
        " for those species:")
print(fg_lookup[str_detect(FG_name, JUV_PATTERN)][order(ScientificName)])

fg_lookup_no_juv <- fg_lookup[!str_detect(FG_name, JUV_PATTERN)]
fg_dupe_check <- fg_lookup_no_juv[, .(n_fg = uniqueN(FG_num)), by = ScientificName]
ambiguous_sci <- fg_dupe_check[n_fg > 1, ScientificName]

if (length(ambiguous_sci) > 0) {
  message("\n", length(ambiguous_sci), " scientific names STILL map to multiple FGs",
          " after dropping juvenile rows (not a juv/adult split - a different",
          " kind of duplicate) - excluded from automatic FG assignment,",
          " needs manual review:")
  print(fg_lookup_no_juv[ScientificName %in% ambiguous_sci][order(ScientificName)])
} else {
  message("\nNo remaining ambiguity after dropping juvenile rows - all resolved to adult FG.")
}

fg_lookup_safe <- unique(fg_lookup_no_juv[!ScientificName %in% ambiguous_sci,
                                          .(ScientificName, FG_num, FG_name)])

## --- 2c. Taxonomic enrichment (family/order/class) via WoRMS -------------
#load taxonomy from worms to fix potential unmatches
source("./WMed_EwE/scripts/worms_taxonomy_lookup.R")
fg_taxonomy <- worms_taxonomy_lookup(fg_lookup_safe$ScientificName)
fg_taxonomy_dt <- as.data.table(fg_taxonomy)[
  , .(ScientificName = original_name, Genus = genus, Family = family,
      Order = order, Class = class, Phylum = phylum)]

existing_taxa_cols <- intersect(c("Genus", "Family", "Order", "Class", "Phylum"), names(fg_lookup_safe))
if (length(existing_taxa_cols) > 0) fg_lookup_safe[, (existing_taxa_cols) := NULL]
fg_lookup_safe <- merge(fg_lookup_safe, fg_taxonomy_dt, by = "ScientificName", all.x = TRUE)

message("\nTaxonomy matched for ", fg_lookup_safe[!is.na(Family), .N], " of ",
        nrow(fg_lookup_safe), " species in fg_lookup")
message("Rows with no taxonomy match (check spelling, or these may be non-species",
        " entries like 'Detritus'/'Discards' which won't have taxonomy):")
print(fg_lookup_safe[is.na(Family), .(ScientificName, FG_name)])

## DEMERSAL - MEDITS SURVEY #####

## --- 3. Swept area per haul (TA.csv) ----------------------------------------
#read haul/sampling stations data
ta <- read_csv(file.path(in_dir, "Demersal", "TA.csv"), show_col_types = FALSE)

#units verification
## Swept area = distance towed x net wing opening.
## vertical_opening: that field is
## a constant 20 across hauls for gear "GC73" (the standard MEDITS GOC73
## trawl) - 20 decimetres = 2.0m, which matches GOC73's real vertical
## opening (~2-4m); 20 METRES would be absurd for a bottom trawl. Wing
## opening of 130 decimetres = 13.0m also matches GOC73's known ~12-15m
## wing spread almost exactly. 

WING_OPENING_UNIT <- "decimetres"
ta_swept <- ta %>%
  mutate(
    distance_m = distance,
    wing_opening_m = if (WING_OPENING_UNIT == "decimetres") wing_opening / 10 else wing_opening,
    swept_area_km2 = (distance_m * wing_opening_m) / 1e6
  ) %>%
  dplyr::select(country, area, vessel, year, haul_number, month, day, swept_area_km2)

## automatic cross-check using vertical_opening as a second signal, so
## this stays self-verifying if it's ever run against a different gear
## type where the decimetres assumption might not hold

vert_median <- median(ta$vertical_opening[ta$vertical_opening > 0], na.rm = TRUE)
if (WING_OPENING_UNIT == "decimetres" && (vert_median < 5 || vert_median > 100)) {
  message("WARNING: vertical_opening median (", vert_median, ") doesn't look like",
          " decimetres for a bottom trawl (expected roughly 15-40) - the unit",
          " assumption may not hold for this gear type, check before trusting results.")
}

message("Swept area (km^2) summary - typical MEDITS range ~0.01-0.08:")
print(summary(ta_swept$swept_area_km2))
med_swept <- median(ta_swept$swept_area_km2, na.rm = TRUE)
if (med_swept > 0.5 || med_swept < 0.001) {
  message("WARNING: median swept area outside typical range - check WING_OPENING_UNIT",
          " against the JRC spec PDF.")
}

## --- 4. Catch per haul (TB.csv)  -------------
#biomass estimates per haul
tb <- read_csv(file.path(in_dir, "Demersal", "TB.csv"), show_col_types = FALSE)
demersal_haul <- tb %>%
  mutate(
    species_code = paste0(genus, species),
    ptot  = ifelse(ptot  < 0, NA, ptot),
    nbtot = ifelse(nbtot < 0, NA, nbtot),
    gsa   = as.numeric(area)
  ) %>%
  as.data.table()

#get MEDITS coding to get species scientific name
tm_list <- as.data.table(readxl::read_excel(tm_list_file, sheet = 1,skip = 2))
code_col <- grep("MEDITS", names(tm_list), ignore.case = TRUE, value = TRUE)[1]
name_col <- grep("Scientific", names(tm_list), ignore.case = TRUE, value = TRUE)[1]
tm_lookup <- unique(tm_list[, .(species_code = get(code_col), ScientificName = get(name_col))])

#merge species with taxonomy and fg data
demersal_haul <- merge(demersal_haul, tm_lookup, by = "species_code", all.x = TRUE)
demersal_haul <- merge(demersal_haul, fg_lookup_safe[, .(ScientificName, FG_num, FG_name)],
                       by = "ScientificName", all.x = TRUE)

message("\nDemersal: ", demersal_haul[!is.na(FG_num), .N], " of ", nrow(demersal_haul),
        " rows matched to species AND functional group")

## --- 4b. Manual overrides for some species without taxonomic data  -------------
## for codes not in the MEDITS list, identified at
## various taxonomic ranks (not all are species-level). Matched against
## the appropriate rank column from the taxonomy enrichment (Genus/Family/
## Class), with the same ambiguity-safe rule as everywhere else: if a rank
## match resolves to more than one distinct FG, it's flagged, not guessed.

MANUAL_DEMERSAL_OVERRIDES <- data.table(
  species_code = c("ARGRACU", "FMBONEL", "GASTRDA", "ILLESPP",
                   "BUCCSPP", "ASCDCEA", "PTEDGRI"),
  taxon_name   = c("Argyropelecus aculeatus", "Bonellidae", "Gastropoda", "Illex",
                   "Buccinum", "Ascidiacea", "Pteroeides griseum"),
  taxon_rank   = c("species", "family", "class", "genus",
                   "genus", "class", "species")
  # NOTE: "Buccinum" used for BUCCSPP (not "Buccinus" as originally written -
  # Buccinum is the standard whelk genus; flag if you meant something else)
)

## SOLEAEG deliberately left unresolved (excluded, not guessed) per instruction
message("\nSOLEAEG intentionally left out - will remain unmatched.")

resolve_override <- function(taxon_name, rank) {
  rank_col <- switch(rank,
                     species = "ScientificName",
                     genus   = "Genus",
                     family  = "Family",
                     class   = "Class"
  )
  matches <- unique(fg_lookup_safe[get(rank_col) == taxon_name, .(FG_num, FG_name)])
  if (nrow(matches) == 0) return(data.table(FG_num = NA_real_, FG_name = NA_character_))
  if (nrow(matches) > 1) {
    message("WARNING: '", taxon_name, "' (", rank, "-level) matches MULTIPLE FGs - not auto-assigned:")
    print(matches)
    return(data.table(FG_num = NA_real_, FG_name = NA_character_))
  }
  matches
}

override_results <- MANUAL_DEMERSAL_OVERRIDES[
  , resolve_override(taxon_name, taxon_rank), by = species_code]
message("\nManual override results:")
print(override_results)

## apply overrides - fill in FG_num/FG_name for these specific codes,
## replacing whatever NA was there before (they weren't matched by the
## MEDITS list lookup at all, so nothing to conflict with)

demersal_haul <- merge(demersal_haul, override_results, by = "species_code",
                       all.x = TRUE, suffixes = c("", "_override"))
demersal_haul[!is.na(FG_num_override), `:=`(FG_num = FG_num_override, FG_name = FG_name_override)]
demersal_haul[, c("FG_num_override", "FG_name_override") := NULL]

message("\nDemersal after manual overrides: ", demersal_haul[!is.na(FG_num), .N],
        " of ", nrow(demersal_haul), " rows matched")

## --- 4c. Assign FG to sp with taxonomic data ------ 
## Genus -> Family fallback for species with a scientific name but
## no direct FG match (the exact species isn't in the FG reference, but a
## close relative might be, under the assumption that congeners/confamilials
## are ecologically similar enough to share a functional group). Same
## ambiguity-safe rule as everywhere else: a genus or family spanning
## MULTIPLE different FGs is too ecologically diverse to assign safely,
## so it's flagged and left unresolved rather than guessed.
##
## Uses the same worms_taxonomy_lookup() as Section 2c (single source for
## both sides of the genus/family comparison - no cross-authority mismatch
## risk between fg_lookup_safe's taxonomy and this batch's taxonomy).

unmatched_sci <- unique(demersal_haul[!is.na(ScientificName) & is.na(FG_num), ScientificName])
message("\n", length(unmatched_sci), " distinct scientific names have no direct FG match -",
        " attempting genus/family fallback via WoRMS.")

unmatched_taxonomy_raw <- worms_taxonomy_lookup(unmatched_sci)
unmatched_taxonomy <- as.data.table(unmatched_taxonomy_raw)[
  , .(ScientificName = original_name, Genus = genus, Family = family)]
message(unmatched_taxonomy[!is.na(Genus) | !is.na(Family), .N], " of ",
        nrow(unmatched_taxonomy), " found in WoRMS with usable genus/family.")

## genus-level: keep only genus that map to exactly ONE FG among fg_lookup_safe

genus_fg_counts <- fg_lookup_safe[!is.na(Genus), .(n_fg = uniqueN(FG_num)), by = Genus]
genus_fg_safe <- unique(fg_lookup_safe[Genus %in% genus_fg_counts[n_fg == 1, Genus],
                                       .(Genus, FG_num, FG_name)])
genus_ambiguous <- genus_fg_counts[n_fg > 1, Genus]

genus_matches <- merge(unmatched_taxonomy, genus_fg_safe, by = "Genus")
genus_matches[, match_method := "genus_fallback"]
still_unresolved <- unmatched_taxonomy[!ScientificName %in% genus_matches$ScientificName]

## family-level: same rule, only for what genus couldn't resolve

family_fg_counts <- fg_lookup_safe[!is.na(Family), .(n_fg = uniqueN(FG_num)), by = Family]
family_fg_safe <- unique(fg_lookup_safe[Family %in% family_fg_counts[n_fg == 1, Family],
                                        .(Family, FG_num, FG_name)])
family_ambiguous <- family_fg_counts[n_fg > 1, Family]

family_matches <- merge(still_unresolved, family_fg_safe, by = "Family")
family_matches[, match_method := "family_fallback"]

fallback_matches <- rbindlist(list(
  genus_matches[, .(ScientificName, FG_num, FG_name, match_method)],
  family_matches[, .(ScientificName, FG_num, FG_name, match_method)]
))

message("Resolved via genus fallback: ", nrow(genus_matches))
message("Resolved via family fallback: ", nrow(family_matches))
message("Still unresolved after both: ", nrow(unmatched_taxonomy) - nrow(fallback_matches))

if (length(genus_ambiguous) > 0) {
  message("\nGenera spanning multiple FGs (too ecologically diverse, not used for fallback):")
  print(fg_lookup_safe[Genus %in% genus_ambiguous, .(Genus, ScientificName, FG_num, FG_name)][order(Genus)])
}
if (length(family_ambiguous) > 0) {
  message("\nFamilies spanning multiple FGs (too ecologically diverse, not used for fallback):")
  print(fg_lookup_safe[Family %in% family_ambiguous, .(Family, ScientificName, FG_num, FG_name)][order(Family)])
}

## apply fallback matches

demersal_haul <- merge(demersal_haul, fallback_matches[, .(ScientificName, FG_num, FG_name)],
                       by = "ScientificName", all.x = TRUE, suffixes = c("", "_fb"))
demersal_haul[is.na(FG_num) & !is.na(FG_num_fb), `:=`(FG_num = FG_num_fb, FG_name = FG_name_fb)]
demersal_haul[, c("FG_num_fb", "FG_name_fb") := NULL]

message("\nDemersal after genus/family fallback: ", demersal_haul[!is.na(FG_num), .N],
        " of ", nrow(demersal_haul), " rows matched")

## --- 4d. Attach taxonomy to the still-unmatched list for manual review -----
## Reuses unmatched_taxonomy_raw from the fallback step above (already
## fetched from WoRMS) - no need to re-query.

still_unmatched <- unique(demersal_haul[is.na(FG_num) & !is.na(ScientificName),
                                        .(species_code, ScientificName)])
still_unmatched_taxa <- as.data.table(unmatched_taxonomy_raw)[
  , .(ScientificName = original_name, Genus = genus, Family = family,
      Order = order, Class = class, Phylum = phylum)]
still_unmatched <- merge(still_unmatched, still_unmatched_taxa, by = "ScientificName", all.x = TRUE)
setorder(still_unmatched, Phylum, Class, Order, Family, ScientificName)

message("\nStill genuinely unmatched (", nrow(still_unmatched),
        " species) - with taxonomy attached for manual review:")
print(still_unmatched, topn = 20)

## Prioritization: which Family/Class groups have the MOST unmatched
## species - one manual FG decision for a 15-species family covers more
## ground than reviewing 15 species individually

message("\nUnmatched species count by Family (largest first - review these first):")
print(still_unmatched[!is.na(Family), .N, by = Family][order(-N)])
message("\nUnmatched species count by Class (for ones with no Family at all):")
print(still_unmatched[is.na(Family), .N, by = Class][order(-N)])

fwrite(still_unmatched, file.path(out_dir, "/processed/demersal_unmatched_for_manual_review.csv"))
message("\nSaved to ", file.path(out_dir, "/processed/demersal_unmatched_for_manual_review.csv"),
        " - fill in an FG_num/FG_name column in Excel, then re-import as a",
        " MANUAL_DEMERSAL_OVERRIDES-style table (see Section 4b) to apply them.")

## --- 4e. Data cleaning  --------------
## Remove rows that aren't real taxa at all:
##  - "NO ..." prefix (a data artifact from an upstream column concatenation,
##    not a real species name prefix)
##  - "Eggs capsules of ..." entries (egg cases, not an assignable organism)

n_before <- nrow(still_unmatched)
still_unmatched_clean <- still_unmatched[
  !str_detect(ScientificName, "^NO\\b") &
    !str_detect(ScientificName, regex("eggs?", ignore_case = TRUE))]
message("\nRemoved ", n_before - nrow(still_unmatched_clean),
        " non-taxon row(s) (NO-prefixed or egg-capsule entries).")

## --- 4f. Manually taxonomy-based bulk assignment rules FG ----------
## Bulk assignment rules: only where the FG name essentially IS the taxon's
## name (high confidence, not a judgment call). FG_num is resolved by
## matching fg_name_target against your live fg_lookup$FG_name text, not
## hardcoded - check the "rule resolution check" printout below to confirm
## every rule found a match; if any show FG_num = NA, the wording here
## doesn't exactly match your FG list and needs a small text fix.

CLASS_RULES <- data.table(
  Class = c("Bivalvia", "Gastropoda", "Holothuroidea", "Scyphozoa", "Thaliacea",
            "Ascidiacea", "Asteroidea", "Demospongia", "Echinoidea", "Gymnolaemata",
            "Hydrozoa", "Ophiuroidea", "Polychaeta", "Hexacorallia", "Octocorallia",
            "Anthozoa"),
  fg_name_target = c("Bivalves", "Gastropods", "Sea cucumbers", "Jellyfish",
                     "Salps and other gelatinous zooplankton",
                     "Other macro-benthos", "Other macro-benthos", "Other macro-benthos",
                     "Other macro-benthos", "Other macro-benthos", "Other macro-benthos",
                     "Other macro-benthos", "Other macro-benthos",
                     "Other corals and gorgonians", "Other corals and gorgonians",
                     "Other macro-benthos")
  # NOTE: Anthozoa (the parent class of Hexacorallia/Octocorallia) only
  # fires when WoRMS returns the broader "Anthozoa" directly rather than
  # the more specific subclass - it's deliberately set to "Other macro-
  # benthos" (78) rather than "Other corals and gorgonians" (82), since an
  # unresolved-to-subclass Anthozoa record could just as easily be an
  # anemone as a coral - safer default than assuming coral.
  # NOTE: Echinoidea -> "Other macro-benthos" (FG78) per your review, not
  # "Other sea urchins" (FG76) - Paracentrotus lividus (Purple sea urchin,
  # FG75) is a single species resolved by direct match earlier, so it
  # never reaches this fallback stage regardless.
  # NOTE: "Demospongia" used exactly as reviewed - the standard spelling is
  # "Demospongiae", but matching must be exact against whatever WoRMS
  # actually returned in your data, so using your reviewed text as-is.
  # Verify in the rule resolution check below that this isn't silently
  # failing to match due to the spelling difference.
)

ORDER_RULES <- data.table(
  Order = c("Torpediniformes", "Alcyonacea", "Scleractinia", "Decapoda", "Actiniaria"),
  fg_name_target = c("Torpedos", "Other corals and gorgonians", "Other corals and gorgonians",
                     "Non-commercial decapods", "Other macro-benthos")
  # NOTE: Actiniaria (sea anemones) -> "Other macro-benthos", NOT the coral/
  # gorgonian FG - and because Order beats Class in the priority chain
  # below, this correctly overrides the broader Hexacorallia (Class) rule
  # for anemones specifically, so they don't get silently folded into
  # "corals and gorgonians" just because they share that class.
  # NOTE: Decapoda -> "Non-commercial decapods" only fires for species that
  # reach this fallback stage at all - i.e. ones that already failed direct/
  # genus/family matching, so commercially important shrimp/crab/lobster
  # species with their own dedicated FGs (66-73) were already resolved
  # earlier and never reach this broad Order-level default.
)

PHYLUM_RULES <- data.table(
  Phylum = c("Annelida", "Bryozoa", "Cnidaria", "Porifera"),
  fg_name_target = c("Other macro-benthos", "Other macro-benthos",
                     "Other macro-benthos", "Other macro-benthos")
  # Broadest, lowest-priority rank - only fires when a species has NO
  # Family/Order/Class resolved at all, just a bare phylum. Since more
  # specific Cnidaria descendants (Scyphozoa, Hydrozoa, Hexacorallia,
  # Octocorallia, Actiniaria) already have their own Class/Order rules
  # above, this Cnidaria-phylum rule only catches genuine leftover cases
  # where WoRMS didn't resolve anything more specific.
)

FAMILY_RULES <- data.table(
  Family = c("Mugilidae"),
  fg_name_target = c("Mugilidae")
)

resolve_fg_name <- function(rules_dt) {
  merge(rules_dt, unique(fg_lookup[, .(FG_num, FG_name)]),
        by.x = "fg_name_target", by.y = "FG_name", all.x = TRUE)
}
CLASS_RULES  <- resolve_fg_name(CLASS_RULES)
ORDER_RULES  <- resolve_fg_name(ORDER_RULES)
FAMILY_RULES <- resolve_fg_name(FAMILY_RULES)
PHYLUM_RULES <- resolve_fg_name(PHYLUM_RULES)

message("\nRule resolution check (FG_num should NOT be NA for any row - if it is,",
        " the fg_name_target text doesn't exactly match your FG list):")
print(rbindlist(list(
  CLASS_RULES[,  .(rule_on = Class,  fg_name_target, FG_num)],
  ORDER_RULES[,  .(rule_on = Order,  fg_name_target, FG_num)],
  FAMILY_RULES[, .(rule_on = Family, fg_name_target, FG_num)],
  PHYLUM_RULES[, .(rule_on = Phylum, fg_name_target, FG_num)])))

## apply in priority order - Family (most specific) > Order > Class >
## Phylum (broadest) - via four separate merges + fcoalesce, so the more
## specific rule always wins if a species happens to match more than one

still_unmatched_clean <- merge(still_unmatched_clean, FAMILY_RULES[, .(Family, FG_num, fg_name_target)],
                               by = "Family", all.x = TRUE)
setnames(still_unmatched_clean, c("FG_num", "fg_name_target"), c("FG_num_family", "FG_name_family"))
still_unmatched_clean <- merge(still_unmatched_clean, ORDER_RULES[, .(Order, FG_num, fg_name_target)],
                               by = "Order", all.x = TRUE)
setnames(still_unmatched_clean, c("FG_num", "fg_name_target"), c("FG_num_order", "FG_name_order"))
still_unmatched_clean <- merge(still_unmatched_clean, CLASS_RULES[, .(Class, FG_num, fg_name_target)],
                               by = "Class", all.x = TRUE)
setnames(still_unmatched_clean, c("FG_num", "fg_name_target"), c("FG_num_class", "FG_name_class"))
still_unmatched_clean <- merge(still_unmatched_clean, PHYLUM_RULES[, .(Phylum, FG_num, fg_name_target)],
                               by = "Phylum", all.x = TRUE)
setnames(still_unmatched_clean, c("FG_num", "fg_name_target"), c("FG_num_phylum", "FG_name_phylum"))

still_unmatched_clean[, FG_num_assigned  := fcoalesce(FG_num_family, FG_num_order, FG_num_class, FG_num_phylum)]
still_unmatched_clean[, FG_name_assigned := fcoalesce(FG_name_family, FG_name_order, FG_name_class, FG_name_phylum)]
still_unmatched_clean[, c("FG_num_family", "FG_name_family", "FG_num_order", "FG_name_order",
                          "FG_num_class", "FG_name_class", "FG_num_phylum", "FG_name_phylum") := NULL]

## "taxon" display column for manual review: prefer the most specific rank
## available (Family > Order > Class > Phylum), falling back to the raw
## scientific name itself only when NONE of those resolved at all

still_unmatched_clean[, taxon_for_review := fcoalesce(Family, Order, Class, Phylum, ScientificName)]

n_auto <- still_unmatched_clean[!is.na(FG_num_assigned), .N]
message("\nAuto-assigned via taxonomy rules: ", n_auto, " of ", nrow(still_unmatched_clean), " species")
message("These are HIGH CONFIDENCE (FG name = taxon name) but still worth a quick",
        " scan before trusting - printing them for verification:")
print(still_unmatched_clean[!is.na(FG_num_assigned),
                            .(species_code, ScientificName, taxon_for_review, FG_num_assigned, FG_name_assigned)])

fwrite(still_unmatched_clean, file.path(out_dir, "/processed/demersal_unmatched_after_taxonomy_rules.csv"))
message("\nSaved to ", file.path(out_dir, "/processed/demersal_unmatched_after_taxonomy_rules.csv"),
        " - rows with FG_num_assigned filled in are the auto-rule matches;",
        " rows with it blank still need your manual FG_num/FG_name (use",
        " taxon_for_review to group/prioritize, same as the Family/Class",
        " count summary above).")

## apply the auto-assigned rules back onto demersal_haul

taxonomy_rule_matches <- still_unmatched_clean[
  !is.na(FG_num_assigned), .(species_code, FG_num_assigned, FG_name_assigned)]
demersal_haul <- merge(demersal_haul, taxonomy_rule_matches, by = "species_code",
                       all.x = TRUE, suffixes = c("", "_rule"))
demersal_haul[is.na(FG_num) & !is.na(FG_num_assigned),
              `:=`(FG_num = FG_num_assigned, FG_name = FG_name_assigned)]
demersal_haul[, c("FG_num_assigned", "FG_name_assigned") := NULL]

message("\nDemersal after taxonomy-based bulk rules: ", demersal_haul[!is.na(FG_num), .N],
        " of ", nrow(demersal_haul), " rows matched")

## 5. STRATA-WEIGHTED DENSITY ESTIMATION ----
##
## stratified-survey estimator:
##   FG_density(gsa, year) = sum over strata of:
##     (summed FG biomass in that stratum / n_hauls in that stratum)
##     * strata_area_proportion(gsa, stratum)
##
## This matters because MEDITS is a stratified-random design - haul
## allocation AIMS to be proportional to stratum area by design, but
## real surveys deviate from that ideal in specific years (weather,
## gear damage, logistics). If allocation were always exactly
## proportional, a simple mean across all hauls and this weighted
## estimator would converge to the same result - but that's an
## assumption, not something to rely on, which is why survey
## statistics practice always applies the explicit area-weighting
## rather than trusting realized haul counts to match the design
## every single year.

## --- 5a. Standard MEDITS depth strata --------------------------------------

MEDITS_STRATA <- data.table(
  stratum_num = 1:5,
  stratum_letter = LETTERS[1:5],
  depth_min = c(10, 50, 100, 200, 500),
  depth_max = c(49.9999, 99.9999, 199.9999, 499.9999, 799.9999)
)

## ta$number_of_the_stratum turned out NOT to be a clean 1-5 code -
## Most values are large multi-digit numbers
## (e.g. 11101, 22405, 32105), which look like a GSA/area-prefixed
## station or site ID for some countries' data rather than a plain
## depth-stratum index, despite the field name. Rather than guess at
## a parsing rule for an undocumented encoding, deriving the stratum
## directly from depth (shooting_depth/hauling_depth, both already in
## ta) sidesteps the ambiguity entirely 

ta_dt <- as.data.table(ta)
ta_dt[, gsa := as.numeric(area)]
ta_dt[, mean_depth := (shooting_depth + hauling_depth) / 2]
ta_dt[, stratum_num := MEDITS_STRATA$stratum_num[
  findInterval(mean_depth, MEDITS_STRATA$depth_min, all.inside = TRUE)]]
## hauls outside the 10-800m range (if any) don't belong to any of the
## 5 standard strata - flagged and excluded
ta_dt[mean_depth < min(MEDITS_STRATA$depth_min) | mean_depth > max(MEDITS_STRATA$depth_max),
      stratum_num := NA_integer_]

n_out_of_range <- ta_dt[is.na(stratum_num), .N]
if (n_out_of_range > 0) {
  message("\n", n_out_of_range, " haul(s) have mean_depth outside the 10-800m",
          " standard strata range - excluded from the strata-weighted index:")
  print(summary(ta_dt[is.na(stratum_num), mean_depth]))
}

message("\nHauls per depth-derived stratum (sanity check - should roughly match",
        " the expected sampling intensity per stratum:")
print(ta_dt[!is.na(stratum_num), .N, by = stratum_num][order(stratum_num)])

## --- 5b. Load official GFCM GSA shapefile (same source/cache logic as the
## WMed map script) ----------------------------------------------------------

gsa_zip_url  <- "https://gfcmsitestorage.blob.core.windows.net/website/5.Data/ArcGIS/GFCM_GSA.zip"
gsa_zip_file <- paste0(dirname(out_dir),"/GFCM_GSA.zip")
gsa_shp_dir  <- paste0(dirname(out_dir),"/GFCM_GSA_shp")

if (!dir.exists(gsa_shp_dir)) {
  if (!file.exists(gsa_zip_file)) {
    dir.create(dirname(gsa_zip_file), recursive = TRUE, showWarnings = FALSE)
    download.file(gsa_zip_url, destfile = gsa_zip_file, mode = "wb", method = "libcurl")
  }
  unzip(gsa_zip_file, exdir = gsa_shp_dir)
}
gsa_shp_path <- list.files(gsa_shp_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)[1]
gsa_sf_all <- st_read(gsa_shp_path, quiet = TRUE)
gsa_sf_all$gsa_num <- as.numeric(gsa_sf_all$SMU_CODE)
## Western/Eastern Sardinia stored as 111/112, not 11.1/11.2 - same fix as the map script
gsa_sf_all$gsa_num[gsa_sf_all$gsa_num %in% c(111, 112)] <- 11

## --- 5c. Compute strata area-proportions for ONE GSA via bathymetry --------
## Adapted from the older Spanish survey-processing script's bathymetry
## approach (marmap -> reclassify by depth -> mask to GSA polygon ->
## proportion table), wrapped as a reusable function so it runs once per
## GSA in the study rather than being tied to one fixed study area.

compute_strata_fact_for_gsa <- function(gsa_num, gsa_sf_all, strata_def, resolution = 1) {
  gsa_poly <- gsa_sf_all[gsa_sf_all$gsa_num == gsa_num, ]
  if (nrow(gsa_poly) == 0) {
    message("  GSA ", gsa_num, " not found in shapefile - skipping.")
    return(NULL)
  }
  bbox <- sf::st_bbox(gsa_poly)
  
  bathy <- tryCatch(
    marmap::getNOAA.bathy(lon1 = bbox["xmin"], lon2 = bbox["xmax"],
                          lat1 = bbox["ymin"], lat2 = bbox["ymax"],
                          resolution = resolution),
    error = function(e) {
      message("  Bathymetry download failed for GSA ", gsa_num, ": ", conditionMessage(e))
      NULL
    }
  )
  if (is.null(bathy)) return(NULL)
  
  bathy_r <- marmap::as.raster(bathy)
  bathy_r <- raster::reclassify(bathy_r, cbind(0, Inf, NA), right = FALSE)   # land -> NA
  bathy_r <- raster::mask(bathy_r, as(gsa_poly, "Spatial"))                  # clip to GSA polygon
  
  bathy_df <- as.data.frame(bathy_r, xy = TRUE)
  names(bathy_df)[3] <- "layer"
  bathy_df <- bathy_df[complete.cases(bathy_df), ]
  bathy_df$depth <- bathy_df$layer * -1
  bathy_df <- bathy_df[bathy_df$depth >= min(strata_def$depth_min) &
                         bathy_df$depth <= max(strata_def$depth_max), ]
  
  if (nrow(bathy_df) == 0) {
    message("  GSA ", gsa_num, ": no bathymetry cells in the 10-800m strata range.")
    return(NULL)
  }
  
  bathy_dt <- as.data.table(bathy_df)
  bathy_dt[, stratum_num := strata_def$stratum_num[
    findInterval(depth, strata_def$depth_min, all.inside = TRUE)]]
  
  props <- bathy_dt[, .N, by = stratum_num][, prop := N / sum(N)][order(stratum_num)]
  props[, gsa := gsa_num]
  props[, .(gsa, stratum_num, prop)]
}

## --- 5d. Run for every GSA in the study, cache to disk (bathymetry ) -------------------
## downloads are slow - don't repeat if already computed

strata_fact_cache_path <- file.path(out_dir, "./processed/strata_fact_by_gsa.csv")

if (file.exists(strata_fact_cache_path)) {
  message("\nLoading cached strata area-proportions from ", strata_fact_cache_path)
  strata_fact_by_gsa <- fread(strata_fact_cache_path)
} else {
  message("\nComputing strata area-proportions per GSA via bathymetry",
          " (slow - downloads bathymetry once per GSA, cached afterward)...")
  gsas_to_process <- sort(unique(demersal_haul$gsa[demersal_haul$gsa %in% FILTER_AREAS]))
  strata_fact_list <- lapply(gsas_to_process, function(g) {
    message(" Processing GSA ", g, "...")
    compute_strata_fact_for_gsa(g, gsa_sf_all, MEDITS_STRATA)
  })
  strata_fact_by_gsa <- rbindlist(strata_fact_list, fill = TRUE)
  fwrite(strata_fact_by_gsa, strata_fact_cache_path)
  message("Saved to ", strata_fact_cache_path, " - delete this file to force recomputation.")
}

message("\nStrata area-proportions by GSA (should sum to ~1 within each GSA):")
print(strata_fact_by_gsa[, .(total = sum(prop)), by = gsa])
print(strata_fact_by_gsa[order(gsa, stratum_num)])

## --- 5e. Number of hauls per GSA/year/stratum ------------------------------
## Uses the depth-derived stratum_num already added to ta_dt in 5a -
## no need to rebuild ta_dt here

n_hauls_by_stratum <- ta_dt[
  !is.na(stratum_num),
  .(n_hauls = uniqueN(haul_number)),
  by = .(gsa, year, stratum_num)
]

message("\nHauls per GSA/year/stratum (spot-check a few - very low counts in a",
        " stratum-year make that cell's estimate noisy regardless of weighting):")
print(n_hauls_by_stratum[order(gsa, year, stratum_num)][1:20])

## --- 5f. Per-haul FG density (weight / swept_area - standard, unchanged),
## then the strata-weighted aggregation to gsa/year/FG -----------------------

demersal_haul <- merge(
  demersal_haul, ta_swept,
  by = c("country", "area", "vessel", "year", "haul_number", "month", "day"),
  all.x = TRUE
)
demersal_haul[, weight_tonnes := ptot / 1e6]   # ptot native unit = grams
demersal_haul[, density_t_km2 := weight_tonnes / swept_area_km2]

## attach the depth-derived stratum to each haul

demersal_haul <- merge(
  demersal_haul, ta_dt[, .(country, area, vessel, year, haul_number, month, day, stratum_num)],
  by = c("country", "area", "vessel", "year", "haul_number", "month", "day"),
  all.x = TRUE
)

## sum biomass within FG, per haul

per_haul_fg <- demersal_haul[
  !is.na(FG_num) & !is.na(density_t_km2),
  .(fg_density_t_km2 = sum(density_t_km2, na.rm = TRUE)),
  by = .(gsa, year, haul_number, stratum_num, FG_num, FG_name)
]

## sum density WITHIN each gsa/year/stratum/FG, matching the "group by
## strata, species, year" step of the stratified estimator

per_stratum_fg <- per_haul_fg[
  , .(density_sum = sum(fg_density_t_km2, na.rm = TRUE)),
  by = .(gsa, year, stratum_num, FG_num, FG_name)
]

## divide by n_hauls in that stratum, multiply by that stratum's area
## proportion - the actual stratified estimator

per_stratum_fg <- merge(per_stratum_fg, n_hauls_by_stratum,
                        by = c("gsa", "year", "stratum_num"), all.x = TRUE)
per_stratum_fg <- merge(per_stratum_fg, strata_fact_by_gsa,
                        by = c("gsa", "stratum_num"), all.x = TRUE)

n_missing_strata_fact <- per_stratum_fg[is.na(prop), .N]
if (n_missing_strata_fact > 0) {
  message("\nWARNING: ", n_missing_strata_fact, " gsa/stratum combination(s) have",
          " no computed area-proportion (bathymetry download may have failed",
          " for that GSA) - these rows will be dropped from the weighted sum",
          " rather than silently treated as zero weight:")
  print(unique(per_stratum_fg[is.na(prop), .(gsa, stratum_num)]))
}

per_stratum_fg <- per_stratum_fg[!is.na(prop) & !is.na(n_hauls) & n_hauls > 0]
per_stratum_fg[, weighted_density := (density_sum / n_hauls) * prop]

## sum across strata -> the final GSA/year/FG density estimate

demersal_fg_index <- per_stratum_fg[
  , .(mean_density_t_km2 = sum(weighted_density, na.rm = TRUE),
      n_strata_contributing = .N,
      n_hauls_total = sum(n_hauls)),
  by = .(gsa, year, FG_num, FG_name)
]

message("\nStrata-weighted demersal FG annual index built: ", nrow(demersal_fg_index), " rows")

## =================================================================
## ACOUSTIC
## =================================================================

sum_lengthclasses <- function(df) {
  lc_cols <- grep("^lengthclass", names(df), value = TRUE)
  mat <- as.matrix(df[lc_cols])
  mat[mat < 0] <- NA
  totals <- rowSums(mat, na.rm = TRUE)
  totals[rowSums(!is.na(mat)) == 0] <- NA
  totals
}

## check how many RAW rows collapse into each country/gsa/year/species group
## BEFORE aggregating - if this is commonly >1, a sum() conflates real
## abundance with how many replicate records happened to exist that year,
## same problem as demersal's hauls, and mean() would be more appropriate

check_raw_replication <- function(file) {
  df <- read_csv(file.path(in_dir, "Acoustic", file), show_col_types = FALSE) %>%
    mutate(gsa = as.numeric(str_extract(area, "\\d+")))
  if (any(df$sex == "C")) df <- filter(df, sex == "C")
  rep_counts <- df %>% dplyr::count(country, gsa, year, species)
  message(file, " - raw records per country/gsa/year/species group:")
  print(summary(rep_counts$n))
  rep_counts
}

message("\nChecking acoustic replication before deciding sum vs mean:")
abund_reps <- check_raw_replication("abundance.csv")
biom_reps  <- check_raw_replication("biomass.csv")

## if max(n) above is 1 everywhere, sum() and mean() give identical
## results and the existing per-species aggregation is already correct
## as-is. If not, switch AGG_FUN below to "mean" instead of "sum".

AGG_FUN <- if (max(abund_reps$n, biom_reps$n) > 1) "mean" else "sum"
message("Using ", AGG_FUN, "() for acoustic per-species aggregation",
        " (based on the replication check above)")

load_acoustic <- function(file, value_name, agg_fun = AGG_FUN) {
  df <- read_csv(file.path(in_dir, "Acoustic", file), show_col_types = FALSE)
  df$total_value <- sum_lengthclasses(df)
  result <- df %>%
    dplyr::mutate(gsa = as.numeric(str_extract(area, "\\d+"))) %>%
    { if (any(.$sex == "C")) dplyr::filter(., sex == "C") else . } %>%
    dplyr::group_by(country, gsa, year, species) %>%
    dplyr::summarise(value = if (agg_fun == "mean") mean(total_value, na.rm = TRUE)
                     else sum(total_value, na.rm = TRUE),
                     .groups = "drop")
  # rename via setnames() (a plain function, not an operator) rather than
  # dplyr's dynamic !!value_name := syntax, which relies on rlang's `:=`
  # and gets silently masked by data.table's own `:=` operator once
  # data.table is loaded later in this same session
  data.table::setnames(result, "value", value_name)
  result
}

acoustic_abund <- load_acoustic("abundance.csv", "total_abundance")
acoustic_biom  <- load_acoustic("biomass.csv",   "total_biomass")
acoustic <- full_join(acoustic_abund, acoustic_biom, by = c("country", "gsa", "year", "species"))

## --- Species -> scientific name -> FG (FAO 3-alpha code bridge) -----------

fao_species <- fread(file = fao_species_file, encoding = "UTF-8")
fao_code_lookup <- unique(fao_species[, .(species = `3A_Code`, ScientificName = Scientific_Name)])

acoustic <- as.data.table(acoustic)
acoustic <- merge(acoustic, fao_code_lookup, by = "species", all.x = TRUE)
acoustic <- merge(acoustic, fg_lookup_safe, by = "ScientificName", all.x = TRUE)

message("\nAcoustic: ", acoustic[!is.na(FG_num), .N], " of ", nrow(acoustic),
        " rows matched to species AND functional group")

## NOTE: no swept-area division here - that's a trawl-specific concept.
## Acoustic biomass/abundance from MEDIAS is already a survey-area
## estimate; if you know it should be standardized some other way
## (e.g. by transect length or GSA area), that's a different calculation
## and I'd want to confirm the right denominator before adding it, since
## getting this wrong would misrepresent absolute biomass as a density.

## --- Sum across species WITHIN the same FG, per country/gsa/year ---------

acoustic_fg_index <- acoustic[
  !is.na(FG_num),
  .(total_biomass_fg   = sum(total_biomass, na.rm = TRUE),
    total_abundance_fg = sum(total_abundance, na.rm = TRUE)),
  by = .(country, gsa, year, FG_num, FG_name)
]

message("Acoustic FG annual index built: ", nrow(acoustic_fg_index), " rows")

## =================================================================
## PLOTS - reusing the existing helpers, now at FG level
## =================================================================

apply_filters <- function(df, area_col = "gsa", species_col, value_col) {
  if (!is.null(FILTER_COUNTRIES)) df <- filter(df, country %in% FILTER_COUNTRIES)
  if (!is.null(FILTER_AREAS)) {
    df <- filter(df, .data[[area_col]] %in% FILTER_AREAS)
  } else {
    top_areas <- df %>% group_by(.data[[area_col]]) %>%
      dplyr::summarise(tot = sum(.data[[value_col]], na.rm = TRUE), .groups = "drop") %>%
      slice_max(tot, n = TOP_N_AREAS) %>% pull(.data[[area_col]])
    df <- filter(df, .data[[area_col]] %in% top_areas)
  }
  top_species <- df %>% group_by(.data[[species_col]]) %>%
    dplyr::summarise(tot = sum(.data[[value_col]], na.rm = TRUE), .groups = "drop") %>%
    slice_max(tot, n = TOP_N_SPECIES) %>% pull(.data[[species_col]])
  filter(df, .data[[species_col]] %in% top_species)
}

plot_timeseries <- function(df, y, species_col, title, y_lab) {
  ggplot(df, aes(x = year, y = .data[[y]], color=as.factor(gsa))) + #, color = country
    geom_line(linewidth = 0.6) +
    geom_point(size = 1) +
    scale_x_continuous(breaks = c(1995,2000,2005,2010,2015,2020),minor_breaks = c(1995:2023)) +
    facet_wrap(vars(.data[[species_col]]), scales = "free_y") +
    labs(title = title, x = "Year", y = y_lab, color = "GSA") +
    theme_minimal(base_size = 11) +
    theme(strip.text.y = element_text(angle = 0), legend.position = "bottom")
}

## --- Demersal FG-level plot --------------------------------------------------

d_fg_plot_data <- apply_filters(as_tibble(demersal_fg_index), species_col = "FG_name",
                                value_col = "mean_density_t_km2")
p5 <- plot_timeseries(d_fg_plot_data, "mean_density_t_km2", "FG_name",
                      "MEDITS trawl survey by FG and GSA (strata-weighted)",
                      "Strata-weighted mean density (t/km^2)")
ggsave(file.path(out_dir, "./plots/demersal_fg_density_timeseries.png"), p5, width = 14, height = 10, dpi = 150)

## --- Depth-strata validation plot --------------------------------------------
## Sanity check on the strata-weighting logic itself, not the final
## area-weighted index: shows each FG's RAW per-haul density (before
## area-weighting) across the 5 depth strata (A=shallowest, E=deepest).
## If this pipeline is working correctly, FGs known to live in deep
## water (e.g. "Bathydemersal (deep sea) fish", the deep-water shrimp
## FGs) should show density concentrated in strata D/E, and coastal/
## shallow FGs should concentrate in A/B. A flat profile or an
## unexpected peak for a well-known species is worth investigating -
## either a real ecological finding or a sign something upstream
## (stratum coding, swept area, species matching) needs a second look.
##
## Uses per_stratum_fg (built during strata-weighting, Section 5) -
## mean_density_per_haul = density_sum / n_hauls, i.e. the per-haul
## average WITHIN that stratum, before the area-weighting step. This is
## the right metric for "where does this FG actually live" - the area
## weighting is about scaling up to a population estimate, not habitat
## preference, so it would distort this specific comparison.

per_stratum_fg[, mean_density_per_haul := density_sum / n_hauls]

strata_profile <- per_stratum_fg[
  , .(mean_density_per_haul = mean(mean_density_per_haul, na.rm = TRUE)),
  by = .(FG_num, FG_name, stratum_num)]
strata_profile <- merge(strata_profile, MEDITS_STRATA[, .(stratum_num, stratum_letter, depth_min, depth_max)],
                        by = "stratum_num", all.x = TRUE)
strata_profile[, stratum_label := paste0(stratum_letter, " (", depth_min, "-",
                                         ifelse(depth_max == 800, "800", depth_max), "m)")]
strata_profile[, stratum_label := factor(stratum_label, levels = unique(stratum_label[order(stratum_num)]))]

## same top-N-by-total-density filter as the other FG plots, so this
## stays readable rather than faceting on every FG in the model
top_fg_for_profile <- strata_profile[, .(tot = sum(mean_density_per_haul, na.rm = TRUE)), by = FG_name][
  order(-tot)][seq_len(min(TOP_N_SPECIES, .N)), FG_name]

p_strata_profile <- ggplot(strata_profile[FG_name %in% top_fg_for_profile],
                           aes(x = stratum_label, y = mean_density_per_haul)) +
  geom_col(fill = "steelblue") +
  facet_wrap(vars(FG_name), scales = "free_y") +
  labs(title = "Depth-strata density profile by FG (validation check)",
       subtitle = "Deep-water FGs should peak toward D/E; shallow/coastal FGs toward A/B",
       x = "Depth stratum", y = "Mean density per haul (t/km^2, pre-area-weighting)") +
  theme_minimal(base_size = 10) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(out_dir, "/plots/demersal_fg_depth_strata_profile.png"), p_strata_profile,
       width = 16, height = 12, dpi = 150)

fwrite(strata_profile, file.path(out_dir, "/processed/demersal_fg_depth_strata_profile.csv"))
message("\nDepth-strata validation plot saved - check that known deep-water FGs",
        " (e.g. Bathydemersal fish, deep-water shrimp) actually peak in the",
        " deeper strata (D/E) as expected, and shallow/coastal FGs peak in A/B.",
        " An unexpected profile for a well-known species is worth investigating.")

## --- Acoustic FG-level plots -------------------------------------------------

a_fg_biom_data <- apply_filters(as_tibble(acoustic_fg_index) %>% filter(total_biomass_fg > 0),
                                species_col = "FG_name", value_col = "total_biomass_fg")
p6 <- plot_timeseries(a_fg_biom_data, "total_biomass_fg", "FG_name",
                      "Acoustic survey by FG and GSA",
                      "Total biomass")
ggsave(file.path(out_dir, "/plots/acoustic_fg_biomass_timeseries.png"), p6, width = 14, height = 10, dpi = 150)

a_fg_abund_data <- apply_filters(as_tibble(acoustic_fg_index) %>% filter(total_abundance_fg > 0),
                                 species_col = "FG_name", value_col = "total_abundance_fg")
p7 <- plot_timeseries(a_fg_abund_data, "total_abundance_fg", "FG_name",
                      "Acoustic survey by FG and GSA",
                      "Total abundance")
ggsave(file.path(out_dir, "/plots/acoustic_fg_abundance_timeseries.png"), p7, width = 14, height = 10, dpi = 150)

## =================================================================
## Export
## =================================================================

fwrite(demersal_fg_index, file.path(out_dir, "/processed/demersal_fg_annual_index.csv"))
fwrite(acoustic_fg_index, file.path(out_dir, "/processed/acoustic_fg_annual_index.csv"))

message("\nDone. New FG-level outputs:")
message("- ", file.path(out_dir, "/processed/demersal_fg_annual_index.csv"), " (now strata-weighted)")
message("- ", file.path(out_dir, "/processed/acoustic_fg_annual_index.csv"))
message("- ", strata_fact_cache_path, " (bathymetry-derived strata area proportions, cached)")
message("- ", file.path(out_dir, "/processed/demersal_fg_depth_strata_profile.csv"), " (per-stratum density, for validation)")
message("- plots/demersal_fg_density_timeseries.png")
message("- plots/demersal_fg_depth_strata_profile.png (validation: deep-water FGs should peak in D/E)")
message("- plots/acoustic_fg_biomass_timeseries.png")
message("- plots/acoustic_fg_abundance_timeseries.png")