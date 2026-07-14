## ---------------------------------------------------------------
## MEDBS Survey Analysis: Demersal & Acoustic, with Functional
## Group matching, correct survey-index aggregation, and plots
## ---------------------------------------------------------------

## --- 0. Packages -----------------------------------------------------------
pkgs <- c("readr", "dplyr", "tidyr", "ggplot2", "stringr", "forcats", "data.table")
new_pkgs <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new_pkgs) > 0) install.packages(new_pkgs)
invisible(lapply(pkgs, library, character.only = TRUE))

## --- 1. Config ---------------------------------------------------------
setwd('/Users/daniel/Work/iMARES/')
base_dir <- "./WMed EwE Model/data/raw/2024_MEDBSsurvey/"
out_dir  <- "./WMed EwE Model/data/plots"
if (!dir.exists(out_dir)) dir.create(out_dir)

fg_file           <- "./WMed EwE Model/data/raw/FG_WMed.xlsx"
fao_species_file  <- "./WMed EwE Model/data/raw/FAO-GFCM_catches/FI_Regional_2025.1.0/CL_FI_SPECIES_GROUPS.csv" #FAO list of species
tm_list_file      <- "./WMed EwE Model/data/raw/2024_MEDBSsurvey/TM_list_(April_2019).xlsx" #taxonomic list downloaded from MEDITS

FILTER_COUNTRIES <- NULL    # Country filter
FILTER_AREAS     <- 1:12   # GSA filter
TOP_N_AREAS      <- 20    # filter by most abundant areas
TOP_N_SPECIES    <- 40    # also used as top-N when faceting by FG instead of species

## --- 2. FG reference ---------------------------------------------------------

fg <- as.data.table(readxl::read_excel(fg_file, sheet = 4))
fg_lookup <- unique(fg[, .(ScientificName = ESPECIE, FG_num = GF, FG_name)])

## --- 2b. Safe-dedup fg_lookup by ScientificName before ANY merge ----------
## A ScientificName mapping to multiple FG_num usually means a life-stage
## split (e.g. "European hake juv." and "European hake adult" as separate
## FG rows for the SAME species). Rule: drop the juvenile-labeled row and
## keep the adult one - this resolves that specific, expected case
## automatically. Anything STILL ambiguous after removing juv. rows is a
## genuinely different kind of duplicate and gets flagged, not guessed -
## merging with duplicate keys as-is would cartesian-explode the join and
## silently duplicate catch/density values for those species.

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
## Single taxonomy source used everywhere in this pipeline (see also
## Section 4c) - this matters, not just for tidiness: if fg_lookup_safe's
## own Genus/Family came from a DIFFERENT authority than the unmatched
## species' Genus/Family, the exact-string-match fallback in 4c could
## silently fail on real matches whenever the two sources spell/classify
## something differently. WoRMS also covers algae/phytoplankton entries
## (e.g. "Cystoseira spinosa", "Diatomes") that FishBase/SeaLifeBase can't,
## since those aren't fish or the invertebrate taxa SeaLifeBase covers.

source("worms_taxonomy_lookup.R")

fg_taxonomy <- worms_taxonomy_lookup(fg_lookup_safe$ScientificName)
fg_taxonomy_dt <- as.data.table(fg_taxonomy)[
  , .(ScientificName = original_name, Genus = genus, Family = family,
      Order = order, Class = class, Phylum = phylum)
]

## Idempotency guard: if this section already ran once (e.g. re-running
## after fixing a package issue mid-session), Genus/Family/Order/Class/
## Phylum already exist on fg_lookup_safe - merging again without dropping
## them first would cause data.table to auto-suffix into Genus.x/Genus.y
## instead of cleanly overwriting, silently leaving the fallback logic
## reading stale/empty columns. Drop them first so this is safe to re-run.
existing_taxa_cols <- intersect(c("Genus", "Family", "Order", "Class", "Phylum"), names(fg_lookup_safe))
if (length(existing_taxa_cols) > 0) fg_lookup_safe[, (existing_taxa_cols) := NULL]

fg_lookup_safe <- merge(fg_lookup_safe, fg_taxonomy_dt, by = "ScientificName", all.x = TRUE)

message("\nTaxonomy matched for ", fg_lookup_safe[!is.na(Family), .N], " of ",
        nrow(fg_lookup_safe), " species in fg_lookup")
message("Rows with no taxonomy match (check spelling, or these may be non-species",
        " entries like 'Detritus'/'Discards' which won't have taxonomy):")
print(fg_lookup_safe[is.na(Family), .(ScientificName, FG_name)])

## =================================================================
## DEMERSAL
## =================================================================

## --- 3. Swept area per haul (TA.csv) ----------------------------------------

ta <- read_csv(file.path(base_dir, "Demersal", "TA.csv"), show_col_types = FALSE)

## Swept area = distance towed x net wing opening.
## UNIT CONFIRMED via cross-check against vertical_opening: that field is
## a constant 20 across hauls for gear "GC73" (the standard MEDITS GOC73
## trawl) - 20 decimetres = 2.0m, which matches GOC73's real vertical
## opening (~2-4m); 20 METRES would be absurd for a bottom trawl. Wing
## opening of 130 decimetres = 13.0m also matches GOC73's known ~12-15m
## wing spread almost exactly. Both fields point the same direction, so
## this is a confirmed unit, not just a plausibility guess.
WING_OPENING_UNIT <- "decimetres"

ta_swept <- ta %>%
  mutate(
    distance_m = distance,
    wing_opening_m = if (WING_OPENING_UNIT == "decimetres") wing_opening / 10 else wing_opening,
    swept_area_km2 = (distance_m * wing_opening_m) / 1e6
  ) %>%
  select(country, area, vessel, year, haul_number, month, day, swept_area_km2)

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

## --- 4. Catch per haul (TB.csv), MEDITS code -> scientific name -> FG ------

tb <- read_csv(file.path(base_dir, "Demersal", "TB.csv"), show_col_types = FALSE)

demersal_haul <- tb %>%
  mutate(
    species_code = paste0(genus, species),
    ptot  = ifelse(ptot  < 0, NA, ptot),
    nbtot = ifelse(nbtot < 0, NA, nbtot),
    gsa   = as.numeric(area)
  ) %>%
  as.data.table()

tm_list <- as.data.table(readxl::read_excel(tm_list_file, sheet = 1,skip = 2))
code_col <- grep("MEDITS", names(tm_list), ignore.case = TRUE, value = TRUE)[1]
name_col <- grep("Scientific", names(tm_list), ignore.case = TRUE, value = TRUE)[1]
tm_lookup <- unique(tm_list[, .(species_code = get(code_col), ScientificName = get(name_col))])

demersal_haul <- merge(demersal_haul, tm_lookup, by = "species_code", all.x = TRUE)
demersal_haul <- merge(demersal_haul, fg_lookup_safe[, .(ScientificName, FG_num, FG_name)],
                       by = "ScientificName", all.x = TRUE)

message("\nDemersal: ", demersal_haul[!is.na(FG_num), .N], " of ", nrow(demersal_haul),
        " rows matched to species AND functional group")

## --- 4b. Manual overrides for codes not in the MEDITS list, identified at
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
  , resolve_override(taxon_name, taxon_rank), by = species_code
]

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

## --- 4c. Genus -> Family fallback for species with a scientific name but
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
  , .(ScientificName = original_name, Genus = genus, Family = family)
]

message(unmatched_taxonomy[!is.na(Genus) | !is.na(Family), .N], " of ",
        nrow(unmatched_taxonomy), " found in WoRMS with usable genus/family.")

## genus-level: keep only genera that map to exactly ONE FG among fg_lookup_safe
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
      Order = order, Class = class, Phylum = phylum)
]

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

fwrite(still_unmatched, file.path(out_dir, "demersal_unmatched_for_manual_review.csv"))
message("\nSaved to ", file.path(out_dir, "demersal_unmatched_for_manual_review.csv"),
        " - fill in an FG_num/FG_name column in Excel, then re-import as a",
        " MANUAL_DEMERSAL_OVERRIDES-style table (see Section 4b) to apply them.")

## --- 4e. Data cleaning + taxonomy-based bulk assignment rules --------------

## Remove rows that aren't real taxa at all:
##  - "NO ..." prefix (a data artifact from an upstream column concatenation,
##    not a real species name prefix)
##  - "Eggs capsules of ..." entries (egg cases, not an assignable organism)
n_before <- nrow(still_unmatched)
still_unmatched_clean <- still_unmatched[
  !str_detect(ScientificName, "^NO\\b") &
    !str_detect(ScientificName, regex("eggs?", ignore_case = TRUE))
]
message("\nRemoved ", n_before - nrow(still_unmatched_clean),
        " non-taxon row(s) (NO-prefixed or egg-capsule entries).")

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
  PHYLUM_RULES[, .(rule_on = Phylum, fg_name_target, FG_num)]
)))

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

fwrite(still_unmatched_clean, file.path(out_dir, "demersal_unmatched_after_taxonomy_rules.csv"))
message("\nSaved to ", file.path(out_dir, "demersal_unmatched_after_taxonomy_rules.csv"),
        " - rows with FG_num_assigned filled in are the auto-rule matches;",
        " rows with it blank still need your manual FG_num/FG_name (use",
        " taxon_for_review to group/prioritize, same as the Family/Class",
        " count summary above).")

## apply the auto-assigned rules back onto demersal_haul
taxonomy_rule_matches <- still_unmatched_clean[
  !is.na(FG_num_assigned), .(species_code, FG_num_assigned, FG_name_assigned)
]

demersal_haul <- merge(demersal_haul, taxonomy_rule_matches, by = "species_code",
                       all.x = TRUE, suffixes = c("", "_rule"))
demersal_haul[is.na(FG_num) & !is.na(FG_num_assigned),
              `:=`(FG_num = FG_num_assigned, FG_name = FG_name_assigned)]
demersal_haul[, c("FG_num_assigned", "FG_name_assigned") := NULL]

message("\nDemersal after taxonomy-based bulk rules: ", demersal_haul[!is.na(FG_num), .N],
        " of ", nrow(demersal_haul), " rows matched")

## --- 5. Density (t/km^2) per haul, then sum within FG, then average -------

demersal_haul <- merge(
  demersal_haul, ta_swept,
  by = c("country", "area", "vessel", "year", "haul_number", "month", "day"),
  all.x = TRUE
)

demersal_haul[, weight_tonnes := ptot / 1e6]   # ptot native unit = grams
demersal_haul[, density_t_km2 := weight_tonnes / swept_area_km2]

per_haul_fg <- demersal_haul[
  !is.na(FG_num) & !is.na(density_t_km2),
  .(fg_density_t_km2 = sum(density_t_km2, na.rm = TRUE)),
  by = .(gsa, year, haul_number, FG_num, FG_name)
]

demersal_fg_index <- per_haul_fg[
  , .(mean_density_t_km2 = mean(fg_density_t_km2, na.rm = TRUE),
      sd_density_t_km2   = sd(fg_density_t_km2, na.rm = TRUE),
      n_hauls            = .N),
  by = .(gsa, year, FG_num, FG_name)
]

message("Demersal FG annual index built: ", nrow(demersal_fg_index), " rows")

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
  df <- read_csv(file.path(base_dir, "Acoustic", file), show_col_types = FALSE) %>%
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

## if max(n) above is 1 everywhere, sum() and mean() give identical
## results and the existing per-species aggregation is already correct
## as-is. If not, switch AGG_FUN below to "mean" instead of "sum".
AGG_FUN <- if (max(abund_reps$n, biom_reps$n) > 1) "mean" else "sum"
message("Using ", AGG_FUN, "() for acoustic per-species aggregation",
        " (based on the replication check above)")

load_acoustic <- function(file, value_name, agg_fun = AGG_FUN) {
  df <- read_csv(file.path(base_dir, "Acoustic", file), show_col_types = FALSE)
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
                      "MEDITS trawl survey by FG and GSA",
                      "Mean density (t/km^2)")
ggsave(file.path(out_dir, "demersal_fg_density_timeseries.png"), p5, width = 14, height = 10, dpi = 150)

## --- Acoustic FG-level plots -------------------------------------------------

a_fg_biom_data <- apply_filters(as_tibble(acoustic_fg_index) %>% filter(total_biomass_fg > 0),
                                species_col = "FG_name", value_col = "total_biomass_fg")
p6 <- plot_timeseries(a_fg_biom_data, "total_biomass_fg", "FG_name",
                      "Acoustic survey by FG and GSA",
                      "Total biomass")
ggsave(file.path(out_dir, "acoustic_fg_biomass_timeseries.png"), p6, width = 14, height = 10, dpi = 150)

a_fg_abund_data <- apply_filters(as_tibble(acoustic_fg_index) %>% filter(total_abundance_fg > 0),
                                 species_col = "FG_name", value_col = "total_abundance_fg")
p7 <- plot_timeseries(a_fg_abund_data, "total_abundance_fg", "FG_name",
                      "Acoustic survey by FG and GSA",
                      "Total abundance")
ggsave(file.path(out_dir, "acoustic_fg_abundance_timeseries.png"), p7, width = 14, height = 10, dpi = 150)

## =================================================================
## Export
## =================================================================

fwrite(demersal_fg_index, file.path(out_dir, "demersal_fg_annual_index.csv"))
fwrite(acoustic_fg_index, file.path(out_dir, "acoustic_fg_annual_index.csv"))

message("\nDone. New FG-level outputs:")
message("- ", file.path(out_dir, "demersal_fg_annual_index.csv"))
message("- ", file.path(out_dir, "acoustic_fg_annual_index.csv"))
message("- plots/demersal_fg_density_timeseries.png")
message("- plots/acoustic_fg_biomass_timeseries.png")
message("- plots/acoustic_fg_abundance_timeseries.png")