## =================================================================
## 01b_biomass_unsurveyed.R
##
## Biomass for FGs that MEDITS/MEDIAS bottom-trawl surveys and the
## GFCM/STAR-RAM/ICCAT stock assessments cannot cover at all: marine
## megafauna (cetaceans, seabirds, sea turtles) and the benthic/
## pelagic groups a bottom-trawl survey isn't designed to sample
## (phytoplankton, zooplankton, seagrass, macroalgae, coralligenous
## fauna, suprabenthos, and similar). Every figure here comes from a
## manually-curated, cited CSV (literature, aerial/acoustic survey
## reports, EMODnet/EUSeaMap habitat-extent shapefiles) rather than
## from 01_biomass.R's own survey/stock-assessment pipeline - hence
## the separate file: 01_biomass.R covers what MEDITS/MEDIAS/STAR-RAM/
## ICCAT actually measured, this file covers what only a literature
## citation can supply.
##
## Sourced by 01_biomass.R (inside its `AREA_MODE == "westmed"` block,
## right after the STAR/RAM Legacy + ICCAT stock-assessment biomass has
## been combined into `stock_assessment_fg_year`) via:
##   source(file.path(git_dir, "scripts/01b_biomass_unsurveyed.R"), local = TRUE)
## `local = TRUE` keeps it in 01_biomass.R's own execution environment,
## so it reads and extends the same variables in place - no separate
## script invocation, no intermediate CSV round-trip, no risk of the
## two files drifting out of sync on what a variable means.
##
## Expects already in scope from 01_biomass.R: `stock_assessment_fg_year`
## (STAR/RAM + ICCAT combined, before any manual-cited literature is
## merged in), `csv_out_dir`, `strata_area_by_area`, `pcloud_dir`,
## `git_dir`, `YEAR_ECOPATH`, `MEDITS_STRATA`, `resolve_iccat_col()`,
## and `fetch_ecobase_literature_biomass()` (sourced from
## 03b_ecobase.R earlier in 01_biomass.R).
##
## Produces (read by 01_biomass.R immediately after this file returns):
## `stock_assessment_fg_year` (re-assigned - now also carries the
## megafauna/primary-producer-plankton/benthic-habitat literature
## merged in), `stock_assessment_fg_year_fisheries_only` (a snapshot of
## the survey/stock-assessment-only version, taken before those merges
## - used downstream so files literally named "stock assessment" never
## include an FG that was never actually stock-assessed), and
## `species_lit_biomass` (species-level detail from whichever literature
## rows supplied a Species column - feeds FG_spp_Ecopath's prop_sp_fg
## further down in 01_biomass.R).
##
## METHOD, in brief (each mechanism is documented in full where it's
## used below):
##  - load_manual_cited_biomass_group(): generic loader for a manual-
##    cited CSV (megafauna / primary producer-plankton / benthic
##    habitat), keyword-matching its Group column to an FG, summing
##    subgroup rows into one FG total, and optionally converting a
##    literature DENSITY (Density_value/Density_unit) into a total
##    Biomass_t via a real habitat area rather than assuming uniform
##    coverage across a whole depth band.
##  - extrapolate_density_row() / estimate_habitat_area_km2(): the
##    density -> habitat-area -> Biomass_t conversion itself, sourcing
##    habitat area either from a real EMODnet/EUSeaMap shapefile extent
##    (Habitat_area_km2, supplied directly in the CSV) or from a depth-
##    band estimate against the pipeline's own bathymetry-derived
##    strata_area_by_area.
##  - EcoBase (other published Mediterranean Ecopath models) is queried
##    for context wherever a group's manual-cited CSV doesn't exist yet,
##    but only ever written to a `*_NOT_INCORPORATED.csv` evaluation
##    file - never applied to Ecopath_B directly. A human decides
##    whether to add a real cited row.
##  - merge_manual_cited_biomass(): folds one group's FG x Year biomass
##    table into stock_assessment_fg_year, flagging (never silently
##    resolving) any FG x Year cell where a literature figure and a real
##    stock assessment disagree.
## =================================================================


## =================================================================
## Marine megafauna BIOMASS - cetaceans, seabirds, sea turtles
## (2026-09-24, per the explicit request: "we need to get marine
## mammals, seabirds and seaturtles biomass time series"). UNLIKE
## bluefin tuna/swordfish above, there is NO RFMO-style bulk catch/
## stock-assessment database for any of these three groups - checked
## directly (2026-09-24):
##   - Cetaceans: the ACCOBAMS Survey Initiative (ASI) is the one real,
##     peer-reviewed, Mediterranean-wide density/abundance estimate
##     that exists, by species and sub-region - but it is essentially
##     ONE synoptic snapshot (aerial+ship surveys run in 2018, a
##     second ASI round has since followed) published as a PDF report
##     (accobams.org), not an annual bulk-downloadable time series the
##     way GFCM/FDI catch data is. Two data points, decades apart, is
##     the realistic ceiling here, not a real 1994-2024 series.
##   - Sea turtles: no Mediterranean-wide biomass series either, but
##     several individual nesting beaches (Zakynthos/Kyparissia in
##     Greece, Dalyan in Turkey, etc.) DO have genuine multi-decade
##     annual NESTING COUNT series - the closest thing to a real long
##     time series among these three groups, but it measures nesting
##     females/nests, not total population biomass, and needs a
##     documented nests-to-population conversion (e.g. Casale et al.)
##     to become a biomass figure - not attempted automatically here.
##   - Seabirds: no Mediterranean-wide population database found at
##     all - coverage is scattered, species-specific literature (e.g.
##     the Balearic shearwater population-trend papers), each with its
##     own methodology and reporting units.
## None of this is a "wrong URL" situation the way the earlier ICCAT
## biomass placeholder's problem was a real bulk download waiting to
## be found - there genuinely isn't a bulk source, so this stays a
## manual, per-record, CITED entry table, same convention as
## BYCATCH_RATE_MANUAL/RECREATIONAL_CATCH_MANUAL/iccat_ssb_biomass.csv
## above. This block is the CODE PLUMBING so that whatever real
## figures the team pull from ACCOBAMS ASI, a nesting-count
## series (converted to population), or a seabird paper can be
## dropped straight into one small CSV and flow into the model with
## the priority rule below, instead of being pasted into the workbook
## by hand.
##
## Expected schema at MEGAFAUNA_BIOMASS_PATH - one row per group/
## species x year: Group (or Species/FG_name - several header
## spellings accepted), Year, Biomass_t (total, whole West Med study
## area - not a density; this script converts to density itself using
## the SAME Total_Area_km2 every other biomass figure here uses), and
## Source_citation (REQUIRED in spirit, not enforced in code - every
## row here is a literature/survey figure, never a measurement this
## pipeline made itself, so it must be traceable to where it came
## from). Group is matched to FG by KEYWORD against FG_name (same
## "closest FG" technique BELHABIB_TAXON_KEYWORDS uses in
## 02_fisheries.R for broad taxon-group catch) - a Group matching zero
## or more than one FG is written to
## marine_megafauna_group_to_fg_REVIEW.csv and excluded, never guessed.
## =================================================================
## 2026-09-24, per project decision: these groups don't need a real time series -
## the minimum is a single baseline biomass figure for the Ecopath base
## year (1994-1996). load_manual_cited_biomass_group() below accepts
## that as-is: a CSV with just one Year value (or one row per year of
## 1994/1995/1996) works fine, since everything downstream already
## groups by FG x Year - it never assumed a full annual series.
##
## Generic loader, used twice below (once for marine megafauna, once
## for the lower-trophic groups that MEDITS/MEDIAS also can't sample -
## see EXEMPT_FG_NAMES/catchability-correction comment near the top of
## this script: a bottom-trawl survey isn't designed to represent
## phytoplankton/zooplankton/macroalgae/seagrass at all). Both are the
## SAME situation - no bulk API, no survey coverage, manual cited entry
## is the only honest option - so one function, two csv files, two
## keyword sets.
## =================================================================
## 2026-09-26, per project decision: two extensions to the manual-cited-CSV
## mechanism above, both driven by the SAME underlying idea - a
## literature-sourced FG total should be built from its real
## taxonomic composition, not one lumped guess:
##
##  (a) SUM OF SUBGROUPS - already free. load_manual_cited_biomass_
##      group() below sums group_biomass_t across every CSV row that
##      keyword-matches the same FG_num x Year (see the `out <-
##      matched[, .(group_biomass_t = sum(...))]` aggregation). So
##      "Benthic mollusc" = Bivalvia + Gastropoda + Scaphopoda +
##      Polyplacophora, or "Other macro-benthos" = sum of its 18
##      classes, works TODAY as soon as each subgroup gets its own
##      keyword category (pointing at the same FG) and its own CSV
##      row/citation - no code change needed for this half.
##
##  (b) HABITAT-AREA EXTRAPOLATION - genuinely new. Previously every
##      manual-cited row had to already BE a whole-study-area total
##      (Biomass_t), converted to density by dividing by the FULL
##      strata_area_by_area sum - i.e. a literature density figure
##      for, say, bivalves would get smeared uniformly across the
##      ENTIRE West Med domain, including depth ranges bivalves don't
##      even occupy. The three functions below let a CSV row instead
##      supply a DENSITY (Density_value + Density_unit, from the
##      literature site) plus that taxon's real habitat depth range
##      (Depth_min_m/Depth_max_m if the paper states it directly, or
##      Habitat_species - a ";"-separated species list resolved via
##      AquaMaps' preferred-depth envelope) - and load_manual_cited_
##      biomass_group() converts density x REAL habitat area (not the
##      full domain) -> Biomass_t itself, with the full calculation
##      audit-trailed into Source_citation so nothing is a silent
##      guess. A row that already supplies Biomass_t directly (the
##      original schema) is untouched - this is purely additive.
## =================================================================

## approximate multiplier to convert a literature density figure to
## g wet-weight / m2 - which is numerically identical to t/km2 (1
## t/km2 = 1e6 g / 1e6 m2 = 1 g/m2), so once a density is in this
## unit it can multiply directly against a km2 habitat area to get
## tonnes. The "_dw" (dry-weight) entries carry a generic wet:dry
## ratio of 4.5 (a commonly-cited macrobenthos DW->WW default, e.g.
## Ricciardi & Bourget 1998-style conversions) - ONLY used when a
## row's Density_unit is explicitly a dry-weight unit, and always
## flagged as an assumption in that row's audit trail (see
## extrapolate_density_row() below), never silently applied.
DENSITY_UNIT_TO_WET_G_M2 <- c(
  "g_m2" = 1, "g_per_m2" = 1,
  "kg_m2" = 1000,
  "mg_m2" = 0.001,
  "t_km2" = 1,
  "g_m2_dw" = 4.5, "g_m2_dry" = 4.5, "gdw_m2" = 4.5
)

## Pro-rate each MEDITS stratum's REAL bathymetry-derived area
## (strata_area_by_area, computed once above from actual seafloor
## depth, not a flat guess) by how much of that stratum's depth band
## overlaps the target group's [depth_min_m, depth_max_m] range, then
## sums across strata (and across every AreaID already in scope,
## since strata_area_by_area is built only for FILTER_AREAS). This is
## the "study inhabitat by group" piece of the request - a taxon
## confined to 10-100m gets only the 10-100m slice of the domain, not
## the whole 10-800m study area.
estimate_habitat_area_km2 <- function(depth_min_m, depth_max_m, strata_area_by_area, strata_def = MEDITS_STRATA) {
  if (is.na(depth_min_m) || is.na(depth_max_m) || depth_max_m <= depth_min_m) return(NA_real_)
  overlap_by_stratum <- rbindlist(lapply(seq_len(nrow(strata_def)), function(i) {
    s_min <- strata_def$depth_min[i]; s_max <- strata_def$depth_max[i]
    overlap <- max(0, min(depth_max_m, s_max) - max(depth_min_m, s_min))
    frac <- if ((s_max - s_min) > 0) overlap / (s_max - s_min) else 0
    data.table(Stratum = strata_def$stratum_num[i], frac = frac)
  }))
  merged <- merge(strata_area_by_area, overlap_by_stratum, by = "Stratum", all.x = TRUE)
  merged[is.na(frac), frac := 0]
  sum(merged$area_km2 * merged$frac, na.rm = TRUE)
}

## Resolves a taxonomic subgroup's own depth range from AquaMaps
## (lib_aquamaps_depth_extension.R's resolve_aquamaps_species_ids() +
## fetch_aquamaps_depth_envelope()), independent of whether the
## APPLY_AQUAMAPS_DEPTH_ADJUSTMENT opt-in flag is TRUE this run - that
## flag only gates a DIFFERENT, unrelated use of AquaMaps (per-species
## shallow/deep density adjustment earlier in this script); this
## manual-CSV path needs the same library defensively source()'d on
## its own. Uses the 10th/90th percentile of matched species'
## PREFERRED depth envelope (DepthPrefMin/DepthPrefMax), not the bare
## min/max, so one outlier deep- or shallow-water species in the list
## doesn't blow the habitat range out past where the bulk of the
## group actually lives.
resolve_group_depth_range_aquamaps <- function(species_names, label = "") {
  species_names <- unique(species_names[!is.na(species_names) & species_names != ""])
  out <- list(depth_min_m = NA_real_, depth_max_m = NA_real_, n_matched = 0L, n_total = length(species_names))
  if (length(species_names) == 0) return(out)
  if (!exists("resolve_aquamaps_species_ids", mode = "function")) {
    aquamaps_lib_path <- file.path(git_dir, "scripts/lib_aquamaps_depth_extension.R")
    if (file.exists(aquamaps_lib_path)) {
      source(aquamaps_lib_path)
    } else {
      message("[", label, "] AquaMaps depth-extension library not found at ", aquamaps_lib_path,
              " - cannot resolve a habitat depth range for ", paste(species_names, collapse = ", "),
              "; falling back to the full study-domain depth range for this row.")
      return(out)
    }
  }
  id_lookup <- tryCatch(resolve_aquamaps_species_ids(species_names),
                        error = function(e) { message("[", label, "] AquaMaps species lookup failed: ", conditionMessage(e)); NULL })
  if (is.null(id_lookup) || nrow(id_lookup) == 0) return(out)
  envelope <- tryCatch(fetch_aquamaps_depth_envelope(id_lookup),
                       error = function(e) { message("[", label, "] AquaMaps depth-envelope fetch failed: ", conditionMessage(e)); NULL })
  if (is.null(envelope)) return(out)
  ok <- envelope[!is.na(DepthPrefMin) & !is.na(DepthPrefMax)]
  out$n_matched <- if (nrow(ok) > 0) uniqueN(ok$ScientificName) else 0L
  if (nrow(ok) == 0) return(out)
  out$depth_min_m <- as.numeric(quantile(ok$DepthPrefMin, 0.10, na.rm = TRUE))
  out$depth_max_m <- as.numeric(quantile(ok$DepthPrefMax, 0.90, na.rm = TRUE))
  out
}

## One row's density -> Biomass_t conversion, with a full audit trail
## string returned alongside so the caller can append it to
## Source_citation - never a silent number. Depth-range priority:
## CSV Depth_min_m/Depth_max_m (the literature site's own stated
## range) > AquaMaps via Habitat_species > full study-domain range
## (the old uniform-smear behavior, kept as a last-resort fallback,
## always flagged as such).
extrapolate_density_row <- function(density_value, density_unit, depth_min_m, depth_max_m, habitat_species,
                                    strata_area_by_area, strata_def = MEDITS_STRATA, label = "",
                                    habitat_area_km2_override = NA_real_) {
  ## 2026-09-28: a depth-band area is a poor stand-in for a genuinely
  ## PATCHY habitat - Posidonia/macroalgae/coralligenous fauna don't
  ## carpet their entire depth range, they occupy a much smaller real
  ## footprint within it (e.g. Posidonia's real West Med meadow extent
  ## is ~10,511 km^2, per a dedicated EUSeaMap habitat shapefile -
  ## nowhere near the full area of the 0-40m band summed across GSA
  ## 1-11). Multiplying an in-habitat density measurement by the whole
  ## depth-band area silently assumes the habitat is contiguous and
  ## complete across that band, overstating total biomass by
  ## potentially an order of magnitude for anything patchy. When the
  ## CSV row supplies a real, independently-measured Habitat_area_km2
  ## (e.g. from a habitat-extent shapefile clipped to the study
  ## domain), that number is used directly instead of the depth-band
  ## estimate - depth range/Habitat_species are then not needed for
  ## this row at all.
  if (!is.na(habitat_area_km2_override)) {
    habitat_area_km2 <- habitat_area_km2_override
    depth_source <- paste0("REAL HABITAT-EXTENT OVERRIDE (Habitat_area_km2 = ", round(habitat_area_km2, 2),
                           " km2, supplied directly in the CSV row - not derived from a depth band; this is",
                           " the correct method for a patchy habitat like seagrass/macroalgae/coralligenous,",
                           " where the depth band it occupies is far larger than its actual footprint)")
    if (is.na(density_value)) {
      return(list(biomass_t = NA_real_,
                  audit_note = "EXTRAPOLATION FAILED (missing density value) - left as NA, not guessed."))
    }
  } else {
    depth_source <- "CSV Depth_min_m/Depth_max_m (literature site's own stated depth range)"
    if (is.na(depth_min_m) || is.na(depth_max_m)) {
      if (!is.na(habitat_species) && nzchar(habitat_species)) {
        sp_list_hab <- trimws(strsplit(habitat_species, ";")[[1]])
        am <- resolve_group_depth_range_aquamaps(sp_list_hab, label = label)
        depth_min_m <- am$depth_min_m; depth_max_m <- am$depth_max_m
        depth_source <- paste0("AquaMaps preferred-depth envelope (10th/90th pct across ", am$n_matched, "/",
                               am$n_total, " matched species: ", habitat_species, ")")
      }
    }
    if (is.na(depth_min_m) || is.na(depth_max_m)) {
      depth_min_m <- min(strata_def$depth_min); depth_max_m <- max(strata_def$depth_max)
      depth_source <- "FULL STUDY-DOMAIN DEPTH RANGE (no Depth_min_m/Depth_max_m or resolvable Habitat_species given - uniform extrapolation across the whole domain, same as the old whole-area approach; add a depth range or Habitat_species to narrow this)"
    }
    habitat_area_km2 <- estimate_habitat_area_km2(depth_min_m, depth_max_m, strata_area_by_area, strata_def)
    if (is.na(habitat_area_km2) || is.na(density_value)) {
      return(list(biomass_t = NA_real_,
                  audit_note = "EXTRAPOLATION FAILED (missing habitat area or density value) - left as NA, not guessed."))
    }
  }
  unit_key <- gsub("/", "_", tolower(gsub("[[:space:]]+", "_", trimws(as.character(density_unit)))))
  mult <- DENSITY_UNIT_TO_WET_G_M2[unit_key]
  if (is.na(mult) || length(mult) == 0) {
    message("[", label, "] Density_unit '", density_unit, "' not recognized (known: ",
            paste(names(DENSITY_UNIT_TO_WET_G_M2), collapse = ", "),
            ") - assuming it's already g/m2 wet-weight (== t/km2). Add it to DENSITY_UNIT_TO_WET_G_M2 if that's wrong.")
    mult <- 1
  }
  density_t_km2 <- density_value * mult
  biomass_t <- density_t_km2 * habitat_area_km2
  depth_note <- if (is.na(depth_min_m) || is.na(depth_max_m)) "" else paste0(" (depth ", round(depth_min_m), "-", round(depth_max_m), " m)")
  audit_note <- paste0("EXTRAPOLATED from density ", density_value, " ", density_unit, " (x", mult, " -> ",
                       round(density_t_km2, 4), " t/km2) over habitat area ", round(habitat_area_km2, 1),
                       " km2", depth_note, "; ", depth_source,
                       " => ", round(biomass_t, 3), " t")
  list(biomass_t = biomass_t, audit_note = audit_note)
}

load_manual_cited_biomass_group <- function(csv_path, taxon_keywords, label, review_csv_name, output_csv_name,
                                            fallback_message) {
  col_aliases <- list(
    year      = c("Year", "year", "Yearc"),
    group     = c("Group", "Species", "FG_name", "Taxon", "CommonName"),
    biomass_t = c("Biomass_t", "Biomass", "Population_t", "Total_t"),
    source    = c("Source_citation", "Source", "Citation", "Reference"),
    ## 2026-09-29, per project decision: "for FG with biomass from present days
    ## (2010-2020) used for ecopath (1995), this should be adjusted
    ## to reflect if there was lower or higher biomass in the past
    ## using references in literature". `Year` above is the TARGET
    ## baseline year this row is being applied to (usually forced to
    ## 1994-1996 so it actually lands in the Ecopath baseline window
    ## via the FG_num/Year merge below) - it is NOT necessarily when
    ## the underlying measurement/survey was actually taken. This
    ## OPTIONAL column lets a source CSV say when the real
    ## measurement is from, distinct from the target Year, so a
    ## genuine temporal mismatch (a 2018 ACCOBAMS survey applied to
    ## 1995, say) can be flagged automatically instead of only being
    ## caught by hand (as done for the ICCAT BFT/SWO case above).
    ## Optional - every row without it is treated as already
    ## contemporaneous with Year, same as before this fix.
    collection_year = c("Collection_year", "Survey_year", "Data_year", "Observed_year", "Measurement_year")
  )
  out <- data.table(FG_num = integer(0), FG_name = character(0), Year = integer(0),
                    group_biomass_t = numeric(0), group_sources = character(0),
                    stock_assessment_density_t_km2 = numeric(0))
  if (!file.exists(csv_path)) {
    message("\n[", label, "] '", csv_path, "' not found - skipping. ", fallback_message)
    return(out)
  }
  raw <- fread(csv_path, encoding = "UTF-8")
  col_year <- resolve_iccat_col(names(raw), col_aliases$year)
  col_bio  <- resolve_iccat_col(names(raw), col_aliases$biomass_t)
  col_grp  <- resolve_iccat_col(names(raw), col_aliases$group)
  col_src  <- resolve_iccat_col(names(raw), col_aliases$source)
  if (any(is.na(c(col_year, col_bio, col_grp)))) {
    message("\n[", label, "] '", csv_path, "' is missing a required column - found: ",
            paste(names(raw), collapse = ", "), ". Needed a year column (tried ",
            paste(col_aliases$year, collapse = "/"), "), a biomass column (tried ",
            paste(col_aliases$biomass_t, collapse = "/"), "), and a group/species column (tried ",
            paste(col_aliases$group, collapse = "/"), ") - skipping rather than guessing.")
    return(out)
  }
  setnames(raw, col_year, "Year")
  setnames(raw, col_bio, "group_biomass_t")
  setnames(raw, col_grp, "Group")
  if (!is.na(col_src)) setnames(raw, col_src, "Source_citation") else raw[, Source_citation := NA_character_]
  raw[, `:=`(Year = as.integer(Year), group_biomass_t = as.numeric(group_biomass_t))]
  
  ## 2026-09-29, per project decision ("the proportion of species within FG should
  ## include all values inputted in B Ecopath, including the literature
  ## ones... it will be used for the pbqb"): an OPTIONAL Species column,
  ## distinct from Group (Group can still be an FG-level keyword like
  ## "OtherDolphins" or "PelagicSeabirds" - Species, when present, names
  ## the actual species that row's Biomass_t is FOR). Species-level rows
  ## already exist in the megafauna CSV (Stenella vs. Delphinus within
  ## "OtherDolphins"; Calonectris/Puffinus yelkouan/Hydrobates within
  ## "PelagicSeabirds") but were previously only ever summed to one FG
  ## total here, discarding exactly the species-level detail
  ## FG_spp_Ecopath's prop_sp_fg needs. Capturing it here (rather than
  ## needing a SEPARATE species-weight CSV, which risks drifting out of
  ## sync with these numbers) means the same literature figure that
  ## builds the FG's Ecopath_B total is, by construction, also what
  ## splits it across species - one source of truth, not two. See the
  ## species_lit_biomass merge in the FG_spp_Ecopath build below for
  ## where this actually gets applied to prop_sp_fg.
  col_sp <- resolve_iccat_col(names(raw), c("Species", "Species_name", "ScientificName", "Scientific_name"))
  if (!is.na(col_sp)) setnames(raw, col_sp, "Species") else raw[, Species := NA_character_]
  
  ## --- temporal-baseline mismatch flag (optional Collection_year
  ## column - see col_aliases$collection_year comment above). Flags,
  ## never numerically corrects - this pipeline doesn't guess a
  ## correction factor without a literature-sourced ratio (same
  ## "closest available, flagged not guessed" rule as everywhere
  ## else here). A row with no Collection_year column, or one equal
  ## to Year, is left completely untouched.
  col_coll_year <- resolve_iccat_col(names(raw), col_aliases$collection_year)
  if (!is.na(col_coll_year)) {
    setnames(raw, col_coll_year, "Collection_year")
    raw[, Collection_year := as.integer(Collection_year)]
    year_ecopath_mid <- if (exists("YEAR_ECOPATH", envir = .GlobalEnv, inherits = FALSE)) round(mean(YEAR_ECOPATH)) else NA_integer_
    mismatched <- !is.na(raw$Collection_year) & raw$Collection_year != raw$Year
    if (any(mismatched) && !is.na(year_ecopath_mid)) {
      gap <- raw$Collection_year[mismatched] - year_ecopath_mid
      temporal_note <- paste0(
        "TEMPORAL MISMATCH: measured in ", raw$Collection_year[mismatched], ", applied here to the ",
        year_ecopath_mid, " Ecopath baseline (gap ~", gap, " year(s)) - no literature-sourced numeric",
        " trend correction applied (direction/magnitude not verified this run); review whether this",
        " FG's biomass is known to have been higher or lower ~", gap, " year(s) earlier."
      )
      raw[mismatched, Source_citation := fifelse(is.na(Source_citation), temporal_note,
                                                 paste0(Source_citation, " -- ", temporal_note))]
      review_rows <- unique(raw[mismatched, .(Group, Year, Collection_year, gap_years = Collection_year - year_ecopath_mid)])
      fwrite(review_rows, file.path(csv_out_dir, paste0("temporal_mismatch_REVIEW_", label, ".csv")))
      message("[", label, "] ", nrow(review_rows), " Group(s) flagged for a temporal mismatch (Collection_year",
              " != the ", year_ecopath_mid, " Ecopath baseline) - see temporal_mismatch_REVIEW_", label,
              ".csv and Source_citation. NOT numerically corrected - only flagged.")
    }
  }
  
  ## --- optional density -> habitat-area extrapolation (2026-09-26) -------
  ## A row can supply Density_value/Density_unit instead of a direct
  ## Biomass_t, plus either Depth_min_m/Depth_max_m or Habitat_species
  ## (";"-separated scientific names resolved via AquaMaps) to size the
  ## REAL habitat area that density applies over, rather than the whole
  ## study domain. Purely additive - a row with Biomass_t already filled
  ## in is left completely alone.
  col_dens_val  <- resolve_iccat_col(names(raw), c("Density_value", "Density", "Density_t_km2"))
  col_dens_unit <- resolve_iccat_col(names(raw), c("Density_unit", "DensityUnit", "Unit"))
  col_depth_min <- resolve_iccat_col(names(raw), c("Depth_min_m", "DepthMin_m", "Depth_min"))
  col_depth_max <- resolve_iccat_col(names(raw), c("Depth_max_m", "DepthMax_m", "Depth_max"))
  col_hab_sp    <- resolve_iccat_col(names(raw), c("Habitat_species", "HabitatSpecies", "Taxa_list"))
  ## 2026-09-28: optional REAL habitat-extent override (e.g. a habitat
  ## shapefile clipped to the study domain and summed) - see
  ## extrapolate_density_row()'s own header comment for why this beats
  ## the depth-band estimate for a patchy habitat (seagrass, macroalgae,
  ## coralligenous fauna). When given, Depth_min_m/Depth_max_m/
  ## Habitat_species are ignored for that row - this area is used directly.
  col_hab_area  <- resolve_iccat_col(names(raw), c("Habitat_area_km2", "HabitatArea_km2", "Habitat_area", "Real_habitat_area_km2"))
  if (!is.na(col_dens_val)) {
    setnames(raw, col_dens_val, "Density_value")
    if (!is.na(col_dens_unit)) setnames(raw, col_dens_unit, "Density_unit") else raw[, Density_unit := NA_character_]
    if (!is.na(col_depth_min)) setnames(raw, col_depth_min, "Depth_min_m") else raw[, Depth_min_m := NA_real_]
    if (!is.na(col_depth_max)) setnames(raw, col_depth_max, "Depth_max_m") else raw[, Depth_max_m := NA_real_]
    if (!is.na(col_hab_sp)) setnames(raw, col_hab_sp, "Habitat_species") else raw[, Habitat_species := NA_character_]
    if (!is.na(col_hab_area)) setnames(raw, col_hab_area, "Habitat_area_km2") else raw[, Habitat_area_km2 := NA_real_]
    raw[, `:=`(Density_value = as.numeric(Density_value), Depth_min_m = as.numeric(Depth_min_m),
               Depth_max_m = as.numeric(Depth_max_m), Habitat_area_km2 = as.numeric(Habitat_area_km2))]
    needs_row <- which(!is.na(raw$Density_value) & (is.na(raw$group_biomass_t) | raw$group_biomass_t == 0))
    if (length(needs_row) > 0) {
      n_with_override <- sum(!is.na(raw$Habitat_area_km2[needs_row]))
      message("[", label, "] ", length(needs_row), " row(s) supply Density_value instead of Biomass_t - ",
              "extrapolating via real habitat area (", n_with_override, " using an explicit Habitat_area_km2",
              " override, ", length(needs_row) - n_with_override, " estimated from depth range instead;",
              " see Source_citation for the per-row audit trail).")
      for (i in needs_row) {
        row_result <- extrapolate_density_row(
          density_value = raw$Density_value[i], density_unit = raw$Density_unit[i],
          depth_min_m = raw$Depth_min_m[i], depth_max_m = raw$Depth_max_m[i],
          habitat_species = raw$Habitat_species[i], strata_area_by_area = strata_area_by_area,
          strata_def = MEDITS_STRATA, label = label,
          habitat_area_km2_override = raw$Habitat_area_km2[i])
        set(raw, i, "group_biomass_t", row_result$biomass_t)
        set(raw, i, "Source_citation",
            paste0(if (is.na(raw$Source_citation[i])) "" else paste0(raw$Source_citation[i], " | "),
                   row_result$audit_note))
      }
    }
  }
  
  ## --- exclude any row whose Source_citation is an EcoBase draft ---------
  ## Per project decision: EcoBase is evaluated for context
  ## (ecobase_biomass_evaluation_*_NOT_INCORPORATED.csv, written
  ## elsewhere in this script) but never incorporated into Ecopath_B -
  ## every fetch that pulls FROM EcoBase already writes only to those
  ## NOT_INCORPORATED files, never into this CSV. This guard catches
  ## the other way an EcoBase-sourced number can still end up here: a
  ## row copied into this CSV by hand (or left over from before that
  ## decision existed) carrying an EcoBase citation. Any row whose
  ## Source_citation mentions "ecobase" is pulled out here and written
  ## to a REVIEW csv instead of being incorporated - replace it with a
  ## real literature/report citation for that FG/Year by hand rather
  ## than this pipeline silently keeping a reference-only EcoBase figure.
  is_ecobase_sourced <- !is.na(raw$Source_citation) & grepl("ecobase", raw$Source_citation, ignore.case = TRUE)
  if (any(is_ecobase_sourced)) {
    ecobase_rows <- raw[is_ecobase_sourced, .(Group, Year, group_biomass_t, Source_citation)]
    fwrite(ecobase_rows, file.path(csv_out_dir, paste0(gsub("[^A-Za-z0-9]+", "_", label), "_ecobase_sourced_rows_EXCLUDED_REVIEW.csv")))
    message("[", label, "] ", nrow(ecobase_rows), " row(s) in ", csv_path, " carry an EcoBase-derived",
            " Source_citation - EXCLUDED from Ecopath_B (EcoBase is reference-only, never incorporated -",
            " see the *_NOT_INCORPORATED.csv evaluation files instead). Written to a REVIEW csv for you",
            " to replace with a real cited source by hand: ", paste(ecobase_rows$Group, collapse = ", "), ".")
    raw <- raw[!is_ecobase_sourced]
  }
  
  ## Full FG catalog isn't built until later in this script (full_fg_list,
  ## from dataframe2) - fg_lookup_safe is already in scope this far up
  ## (built at line ~637) and carries the same FG_num/FG_name universe.
  full_fg_catalog <- unique(fg_lookup_safe[, .(FG_num, FG_name)])
  group_word_sets <- lapply(taxon_keywords, tolower)
  fg_word_hits <- rbindlist(lapply(names(group_word_sets), function(g) {
    hits <- full_fg_catalog[sapply(tolower(FG_name), function(nm) any(sapply(group_word_sets[[g]], function(kw) grepl(kw, nm, fixed = TRUE))))]
    if (nrow(hits) == 0) return(NULL)
    data.table(Group = g, FG_num = hits$FG_num, FG_name = hits$FG_name)
  }))
  if (is.null(fg_word_hits) || nrow(fg_word_hits) == 0) fg_word_hits <- data.table(Group = character(), FG_num = integer(), FG_name = character())
  
  unmatched_groups_dt <- unique(raw[!Group %in% unique(fg_word_hits$Group), .(Group)])
  if (nrow(unmatched_groups_dt) > 0) {
    fwrite(unmatched_groups_dt, file.path(csv_out_dir, review_csv_name))
    message("[", label, "] ", nrow(unmatched_groups_dt), " Group value(s) in ", csv_path,
            " matched ZERO FG by keyword (checked against FG_WMed_2026.csv's real FG_name text - add a",
            " keyword to the taxon-keyword list above if the FG really exists under a different name): ",
            paste(unmatched_groups_dt$Group, collapse = ", "), " - written to ", review_csv_name, ", excluded below.")
  }
  ambiguous_groups <- fg_word_hits[, .N, by = Group][N > 1, Group]
  if (length(ambiguous_groups) > 0) {
    message("[", label, "] ", length(ambiguous_groups), " Group value(s) matched MORE than one",
            " FG by keyword - kept (biomass is apportioned across all matching FGs, same 'split it' rule the",
            " Belhabib taxon-keyword match in 02_fisheries.R uses): ", paste(ambiguous_groups, collapse = ", "))
  }
  matched <- merge(raw, fg_word_hits, by = "Group", allow.cartesian = TRUE)
  species_detail <- data.table(FG_num = integer(0), FG_name = character(0), Year = integer(0),
                               Species = character(0), species_biomass_t = numeric(0),
                               species_source = character(0))
  if (nrow(matched) > 0) {
    matched[, n_fg_for_group := uniqueN(FG_num), by = .(Group, Year)]
    matched[, group_biomass_t := group_biomass_t / n_fg_for_group]  # split evenly across ambiguous FGs - never guessed at full weight onto each
    ## Capture the per-species detail BEFORE it's summed away into one
    ## FG total below - this is what lets prop_sp_fg reuse the exact
    ## same literature figures that built Ecopath_B, instead of a
    ## second, separately-maintained relative-weight source.
    if (any(!is.na(matched$Species))) {
      species_detail <- matched[!is.na(Species), .(
        FG_num, FG_name, Year, Species, species_biomass_t = group_biomass_t,
        species_source = fifelse(is.na(Source_citation), paste0(label, " (source_citation not filled in)"), Source_citation)
      )]
      message("[", label, "] ", uniqueN(species_detail$Species), " row(s) also carry a Species-level",
              " biomass figure across ", uniqueN(species_detail$FG_num), " FG(s) - these will directly drive",
              " prop_sp_fg for those species in FG_spp_Ecopath (see species_lit_biomass below), taking",
              " priority over both raw survey density and any separate relative-weighting CSV.")
    }
    out <- matched[, .(
      group_biomass_t = sum(group_biomass_t, na.rm = TRUE),
      group_sources = paste(unique(na.omit(Source_citation)), collapse = "; ")
    ), by = .(FG_num, FG_name, Year)]
    out[group_sources == "", group_sources := paste0(label, " (source_citation not filled in)")]
    out[, stock_assessment_density_t_km2 := group_biomass_t / sum(strata_area_by_area$area_km2, na.rm = TRUE)]
    out <- out[group_biomass_t > 0]
    fwrite(out, file.path(csv_out_dir, output_csv_name))
    message("[", label, "] ", nrow(out), " FG x Year row(s) (", uniqueN(out$FG_num), " FG(s)) - written to ", output_csv_name, ".")
  }
  attr(out, "species_detail") <- species_detail
  out
}

## Merge one manual-cited group's output into stock_assessment_fg_year -
## it wins on any FG x Year overlap (it's always the single most direct
## source available for these FGs; no MEDITS/MEDIAS trawl survey samples
## megafauna or plankton/primary-producer groups at all).
merge_manual_cited_biomass <- function(stock_assessment_fg_year, group_fg_year, source_label) {
  if (nrow(group_fg_year) == 0) return(stock_assessment_fg_year)
  overlap_cells <- fintersect(stock_assessment_fg_year[, .(FG_num, Year)], group_fg_year[, .(FG_num, Year)])
  if (nrow(overlap_cells) > 0) {
    message("[Stock-assessment biomass] ", source_label, " overrides an existing source for ",
            nrow(overlap_cells), " FG x Year cell(s).")
    stock_assessment_fg_year <- stock_assessment_fg_year[!overlap_cells, on = c("FG_num", "Year")]
  }
  ## stock_assessment_source now carries the REAL literature citation
  ## (group_sources - read straight from the input CSV's own
  ## Source_citation column, see load_manual_cited_biomass_group()
  ## above), prefixed with source_label, instead of the generic
  ## "literature/survey estimate (... - manual, cited entry)"
  ## placeholder phrase this used to write no matter what the row's
  ## actual citation was. The source_label prefix ("marine megafauna",
  ## "primary producer/plankton") is kept so the Tier classification in
  ## the biomass-by-source QA plot below (which matches on those exact
  ## words) still recognizes these rows - only the "manual, cited
  ## entry" filler is replaced with content. group_sources already
  ## reads "<label> (source_citation not filled in)" for a row whose
  ## input CSV genuinely left Source_citation blank, so that case still
  ## surfaces as a visible gap rather than a silent fake citation.
  stock_assessment_fg_year <- rbindlist(list(
    stock_assessment_fg_year,
    group_fg_year[, .(FG_num, FG_name, Year, stock_assessment_density_t_km2,
                      star_biomass_t = group_biomass_t, star_n_stocks = NA_integer_,
                      star_sources = group_sources,
                      stock_assessment_source = paste0(source_label, ": ", group_sources))]
  ), use.names = TRUE, fill = TRUE)
  message("[Stock-assessment biomass] stock_assessment_fg_year now also includes ", source_label, ": ",
          nrow(stock_assessment_fg_year), " FG x Year row(s) total.")
  stock_assessment_fg_year
}

## --- Marine megafauna (cetaceans, seabirds, sea turtles) ---------------
## Real sources for a 1994-1996 baseline figure: ACCOBAMS Survey
## Initiative (accobams.org - cetacean density/abundance by species and
## sub-region; note its own survey rounds are ~2018+, so treat it as the
## closest available proxy, not a real 1994-1996 measurement, unless a
## published back-cast exists); a long-running nesting-beach count series
## (e.g. Zakynthos/Kyparissia, Greece; Dalyan, Turkey) converted to a
## population estimate via a published nests-to-population factor (e.g.
## Casale et al.) for sea turtles - these series DO reach back to the
## 1990s, so this is the one megafauna group with a real shot at an
## actual base-year figure; species-specific published population papers
## for seabirds (no single Mediterranean-wide source found). Whole-
## Mediterranean EwE models covering this exact period also exist and
## are worth pulling a cited baseline from directly - see
## Piroddi et al. 2015 (Mar Ecol Prog Ser 533:47-65, "Modelling the
## Mediterranean marine ecosystem as a whole" - two baseline periods,
## "1950s" and "2000s", 4 Mediterranean sub-regions including a Western
## Mediterranean one, tables S2/S3 in the supplementary material carry
## the actual B t/km2 by functional group INCLUDING cetaceans, seabirds,
## sea turtles) and, as a smaller-scale cross-check, the GOLEM Gulf of
## Lion Ecopath model (Ecosyst. modelling in the NW Med Sea, 2010-2014
## baseline - reports dolphins+seabirds combined at <0.01% of total
## system biomass, a useful order-of-magnitude sanity check even though
## its reference period is 20 years later than ours).
## 2026-09-25, per project decision: moved from data/ to data/Complementary data/,
## alongside the other manual/cited reference files that live there
## (matching where westmed_posidonia_coralligenous.shp and similar
## hand-curated inputs already live).
MEGAFAUNA_BIOMASS_PATH <- file.path(pcloud_dir, "data/Complementary data/marine_megafauna_biomass.csv")
## 2026-09-24, per project decision (Ecopath_B still showing no cetaceans/
## seabirds/sea turtles even after the fg_year_full fix): "Pinnipeds"
## added as its OWN keyword group - Monk seals (FG6) matched NEITHER
## Cetaceans/Seabirds/SeaTurtles before this (no "seal" keyword
## anywhere), so a manual CSV would have loaded fine but Monk seals
## specifically could never have received a value regardless of what
## the CSV said - a real gap, not just a missing CSV.
## 2026-09-25, per project decision: real, species-specific ACCOBAMS Survey
## Initiative density figures are now available for bottlenose dolphins,
## "other dolphins" (striped dolphin proxy), and fin whale (see
## marine_megafauna_biomass.csv below) - but the broad "Cetaceans"
## bucket above matches ALL FIVE cetacean FGs by keyword (every one of
## them contains "dolphin" or "whale"), so a Group="Cetaceans" row would
## get its biomass SPLIT EVENLY across Bottlenose dolphins/Other
## dolphins/Fin whale/Deep sea-cetacean feeders/Sperm whale regardless
## of their real relative abundance - throwing away exactly the
## species-specificity these new figures provide. Added five
## FG-specific categories below so a CSV row can target exactly one
## FG; each keyword is checked to match ONLY its intended FG_name
## and no other (verified against the real FG_WMed_2026.csv text -
## "bottlenose" only appears in "Bottlenose dolphins", "other dolphin"
## only in "Other dolphins", etc.). "Cetaceans" is kept too, as a
## fallback bucket for a future figure that's genuinely only available
## at the whole-cetacean-guild level (e.g. a total abundance survey that
## doesn't break out species) - it still splits evenly across all 5 FGs,
## which is the correct behavior for a genuinely undifferentiated figure.
## 2026-09-25, per project decision ("i am still missing two FGs of seabirds..."):
## same bug as the original single "Cetaceans" bucket - the broad
## "Seabirds" keyword list below (still kept, as a genuinely-
## undifferentiated-figure fallback) matches BOTH "Pelagic/Offshore
## seabirds" (FG7) and "Coastal/inshore seabirds" (FG8) by keyword
## (both FG_names literally contain the substring "seabirds"), so a
## Group="Seabirds" row would still split evenly across both FGs
## regardless of which one a real count actually describes. Added
## PelagicSeabirds/CoastalSeabirds as their own keyword categories -
## each keyword checked to match ONLY its intended FG_name text
## ("pelagic/offshore seabird" only appears in FG7, "coastal/inshore
## seabird" only in FG8).
MEGAFAUNA_TAXON_KEYWORDS <- list(
  Cetaceans              = c("cetacean", "dolphin", "whale", "porpoise"),
  BottlenoseDolphins     = c("bottlenose"),
  OtherDolphins          = c("other dolphin"),
  FinWhale               = c("fin whale"),
  SpermWhale             = c("sperm whale"),
  DeepSeaCetaceanFeeders = c("deep sea-cetacean", "deep sea cetacean"),
  Pinnipeds  = c("seal", "monk seal"),
  Seabirds   = c("seabird", "shearwater", "gull", "petrel", "tern", "auk", "cormorant"),
  PelagicSeabirds = c("pelagic/offshore seabird", "pelagic seabird"),
  CoastalSeabirds = c("coastal/inshore seabird", "coastal seabird"),
  SeaTurtles = c("turtle")
)
megafauna_biomass_fg_year <- load_manual_cited_biomass_group(
  csv_path = MEGAFAUNA_BIOMASS_PATH, taxon_keywords = MEGAFAUNA_TAXON_KEYWORDS,
  label = "Marine megafauna biomass", review_csv_name = "marine_megafauna_group_to_fg_REVIEW.csv",
  output_csv_name = "marine_megafauna_biomass_by_fg.csv",
  fallback_message = paste0("A single 1994-1996 baseline figure per group is enough - no time series needed.",
                            " Preferred real sources, per project decision (2026-09-24): AERIAL SURVEY density/abundance for cetaceans -",
                            " the ACCOBAMS Survey Initiative (ASI-Med-Report, accobams.org) and the dedicated Central/Western",
                            " Mediterranean aerial survey (Panigada et al., ScienceDirect S0967064517301418) both report real",
                            " aerial-survey density/abundance by species and sub-region; CENSUS data for seabirds - UNEPMAP's",
                            " Mediterranean Quality Status Report 'Common Indicator 4: Population abundance - Seabirds'",
                            " (medqsr.org) is the closest thing to a single Mediterranean-wide seabird census, with the World",
                            " Seabird Union's database directory (worldseabirdunion.org) as a second place to check for a",
                            " colony-count series; nesting-beach count series x a published nests-to-population factor, e.g.",
                            " Casale et al. (sea turtles - the one group with count series reaching back to the 1990s). A cited",
                            " B t/km2 straight out of Piroddi et al. 2015 (Mar Ecol Prog Ser 533:47-65, supplementary tables",
                            " S2/S3, Western Mediterranean sub-region) remains a fallback/cross-check if a real aerial-survey or",
                            " census figure isn't available for a given species/year. Expected columns: Year, Group (Cetaceans/Seabirds/SeaTurtles, or a",
                            " specific FG_name), Biomass_t (total for the whole West Med study area, not a density), Source_citation.")
)
megafauna_species_lit_biomass <- attr(megafauna_biomass_fg_year, "species_detail")

## --- Lower trophic levels (zooplankton, phytoplankton, macroalgae, ------
## seagrass/Posidonia, gorgonians/corals) - same situation as megafauna:
## MEDITS/MEDIAS are bottom-trawl surveys and were never designed to
## sample these groups representatively (see EXEMPT_FG_NAMES near the
## top of this script, which already excludes them from the trawl
## catchability correction for exactly this reason) - so there's no
## survey density figure to fall back on here either.
##
## 2026-09-24, per project decision: pull this from EcoBase (existing published
## Ecopath models covering the Mediterranean) rather than hand-copying
## numbers out of a paper - EcoBase already has these exact groups as
## Biomass inputs in OTHER models' own Ecopath parameterizations, and is
## therefore the fastest, most directly comparable (same B t/km2 EwE
## unit) source, on top of whatever satellite/other-source estimates are
## closest to 1994-1996.
##
## fetch_ecobase_literature_biomass() (03b_ecobase.R) queries EcoBase,
## keyword-matches every returned group_name against the 7 target
## groups below, and for each one picks whichever candidate model's
## year is CLOSEST to 1995 (midpoint of YEAR_ECOPATH) - written to
## ecobase_literature_biomass_best_by_group.csv with the source model/
## year/authors and how far that year actually is from 1995, so "closest
## available, not a real 1994-1996 measurement" stays explicit. If
## PRIMARY_PRODUCER_BIOMASS_PATH doesn't exist yet, that EcoBase result
## is used to AUTO-DRAFT it (clearly tagged as an EcoBase draft, not a
## final reviewed figure) - so load_manual_cited_biomass_group() below
## always has something to read, without needing the team to
## manually type numbers in first. Once a real, reviewed CSV is placed
## at that path, this auto-draft step is skipped entirely (existing file
## always wins - never overwritten by this block).
##
## A target group EcoBase has no match for at all (reported by
## fetch_ecobase_literature_biomass() itself) still needs a non-EcoBase
## source - candidates from this research: satellite chlorophyll-a ->
## phytoplankton biomass conversions (ocean-colour record starts
## ~1997/1998 (SeaWiFS), so also a "closest available year" proxy, not
## a real 1994-1996 measurement); Posidonia standing biomass per m2 from
## the seagrass-ecology literature (e.g. Pergent et al.) x the actual
## meadow area within the study area's strata, if known.
PRIMARY_PRODUCER_BIOMASS_PATH <- file.path(pcloud_dir, "data/primary_producer_plankton_biomass.csv")
## 2026-09-30, per the 3-CSV split (see the keyword-list comment
## above): benthic/seagrass/coralligenous groups (Posidonia, Cymodocea,
## Macroalgae, Suprabenthos, GorgoniansCorals, BenthicMollusc_*/
## Macrobenthos_* subgroups) get their own file, separate from
## PRIMARY_PRODUCER_BIOMASS_PATH's plankton-only scope - these are the
## groups whose real West Med extent (where available) comes from an
## EMODnet/EUSeaMap habitat shapefile, clipped to the study domain and
## fed into extrapolate_density_row() via Habitat_area_km2, not a
## CMEMS/EcoBase biogeochemical-model query.
BENTHIC_HABITAT_BIOMASS_PATH <- file.path(pcloud_dir, "data/Complementary data/benthic_habitat_biomass_literature.csv")
## Full keyword catalog - used below to match the manual/auto-drafted
## CSV's Group column to an FG_name, REGARDLESS of which source (EcoBase
## or satellite) actually supplied each group's number.
## 2026-09-25, per project decision ("suprabenthos... salps and gelatinous
## zooplankton and jellyfish... and cymodocea for biomass"): these four
## groups were NEVER in this keyword list before - meaning the EcoBase
## auto-draft block above (fetch_ecobase_literature_biomass()) has never
## actually searched for them, even though EcoBase model group_names
## commonly use exactly these labels (per project decision, re: Suprabenthos
## specifically: "usually a lot of models include this FG with that
## name") - a real, silent gap, not just missing manual data. Added as
## their own categories so a future EcoBase query (or a manual/cited CSV
## row) can target each one specifically. NOTE: because
## PRIMARY_PRODUCER_BIOMASS_PATH already exists (the team's real,
## reviewed data/primary_producer_plankton_biomass.csv), the auto-draft
## block above is SKIPPED entirely this run ("existing file always wins
## - never overwritten") - these new categories only take effect once
## EITHER (a) that file is deleted/renamed so the EcoBase auto-draft
## runs fresh and can populate them, or (b) real cited rows for
## Suprabenthos/Cymodocea/GelatinousZooplankton are added to that CSV by
## hand, same as the megafauna file above. Web search this session
## (2026-09-25) could NOT find a citable Western-Med-specific standing-
## biomass figure for any of these three (Suprabenthos: Corrales et al.
## 2015/South Catalan Sea Ecopath papers exist but are paywalled;
## Cymodocea nodosa: literature has LEAF/RHIZOME PRODUCTION rates, e.g.
## Pérez & Camp 1986 Mar Menor lagoon 160-427 g DW/m2/year, but no
## standing-biomass figure; gelatinous zooplankton/salps: Mediterranean-
## specific trawl-survey biomass papers found by title but not
## accessible full-text) - genuinely still open, not filled with a
## guess. "Jellyfish" (FG68) and "Other macro-benthos" (FG67) are NOT
## added here - both already receive a small non-NA MEDITS-survey-
## derived value in the current real output (confirmed against Daniel's
## actual ecopath_ecosim_inputs.xlsx, Ecopath_B sheet: Jellyfish =
## 0.000196 t/km2, Other macro-benthos = 0.003135 t/km2) - genuinely
## missing is Suprabenthos/Cymodocea/"Salps and other gelatinous
## zooplankton" (all = NA in that same real output), which is what these
## three new categories target. The tiny Jellyfish/macro-benthos MEDITS
## values are likely a real undersample (bottom trawls are known to
## catch gelatinous fauna poorly) rather than a bug - worth a literature
## cross-check later, but that is a "is this number too low" question,
## not a "this cell is empty" one, so left untouched here.
## 2026-09-30: split into two keyword lists feeding two separate CSVs
## ("csv of biomass literature should be one for ICCAT (because are
## reports) [already its own file - iccat_biomass_path], one for
## megafauna (literature) [already its own file - MEGAFAUNA_BIOMASS_
## PATH], and one for posidonia, cymodocea macroalgae suprabenthos
## corals and gorgonians etc (literature and shapefiles from emodnet)"):
## PRIMARY_PRODUCER_TAXON_KEYWORDS now covers ONLY the pelagic plankton
## groups (CMEMS/EcoBase-sourced, no habitat-extent shapefile involved
## at all - open water, not a mapped polygon); every benthic/seagrass/
## coralligenous group (real or candidate EMODnet/EUSeaMap extent
## shapefile + a literature density figure) moved to
## BENTHIC_HABITAT_TAXON_KEYWORDS below, reading from its own new
## BENTHIC_HABITAT_BIOMASS_PATH file instead of PRIMARY_PRODUCER_
## BIOMASS_PATH. BenthicMollusc_*/Macrobenthos_* subgroups moved too -
## they're MEDITS-undersampled BENTHOS needing a literature figure,
## same category as Posidonia/Cymodocea/etc, not plankton (flag this
## back if that grouping isn't what was meant).
PRIMARY_PRODUCER_TAXON_KEYWORDS <- list(
  MacroZooplankton     = c("macrozooplankton", "macro-zooplankton", "macro zooplankton"),
  MesoMicroZooplankton = c("mesozooplankton", "meso-zooplankton", "meso zooplankton",
                           "microzooplankton", "micro-zooplankton", "micro zooplankton"),
  LargePhytoplankton   = c("large phytoplankton", "diatom"),
  SmallPhytoplankton   = c("small phytoplankton", "picophytoplankton", "nanophytoplankton"),
  GelatinousZooplankton = c("gelatinous zooplankton", "salp", "salpidae", "thaliacea", "jellyfish")
)

BENTHIC_HABITAT_TAXON_KEYWORDS <- list(
  Posidonia            = c("posidonia", "seagrass"),
  Macroalgae           = c("macroalga", "macro-alga"),
  GorgoniansCorals     = c("gorgonian", "coral"),
  Suprabenthos         = c("suprabenthos", "supra-benthos", "supra benthos"),
  Cymodocea            = c("cymodocea"),
  
  ## 2026-09-26, per project decision: "Benthic mollusc" and "Other macro-benthos"
  ## are NOT survey-exempt (MEDITS does sample them - see the real
  ## Ecopath_B values referenced near EXEMPT_FG_NAMES above, ~0.00314
  ## and ~0.000196 t/km2), but a bottom-trawl survey badly under-samples
  ## small/soft-bodied/burrowing benthos, so per project decision these two FGs
  ## should ALSO be built from literature, one keyword category PER REAL
  ## TAXONOMIC SUBGROUP (each pointing at the SAME FG) so load_manual_
  ## cited_biomass_group()'s existing sum-by-FG_num/Year aggregation adds
  ## them back up into one FG total - e.g. "Benthic mollusc" = Bivalvia +
  ## Gastropoda + Scaphopoda + Polyplacophora. Composition confirmed
  ## against Daniel's real FG_WMed_2026.csv (2026-09-26): Benthic
  ## mollusc = 96 Bivalvia spp, 129 Gastropoda spp, 4 Scaphopoda spp, 2
  ## Polyplacophora spp; Other macro-benthos = 18 classes (Echinoidea,
  ## Holothuroidea, Hexacorallia, Demospongiae, Gymnolaemata, Thecostraca,
  ## Ophiuroidea, Polychaeta, Asteroidea, Crinoidea, Ascidiacea, Hydrozoa,
  ## Stenolaemata, Octocorallia, Articulata, Clitellata, Palaeonemertea).
  ## Each key below is the exact "Group" value a CSV row must use - the
  ## keyword itself (the FG_name substring to match) is the SAME for
  ## every subgroup of one FG, on purpose, since it's the FG they all
  ## belong to, not a per-class FG_name difference.
  ##
  ## NO REAL BIOMASS/DENSITY NUMBERS EXIST YET for any of these rows -
  ## this only wires the matching so a real CSV row (Biomass_t, or a
  ## Density_value + Depth_min_m/Depth_max_m/Habitat_species per the
  ## 2026-09-26 extrapolation extension above) drops straight in. Until
  ## then these categories simply find zero raw rows and do nothing -
  ## the current MEDITS-survey figures for both FGs are untouched.
  BenthicMollusc_Bivalvia       = c("benthic mollusc"),
  BenthicMollusc_Gastropoda     = c("benthic mollusc"),
  BenthicMollusc_Scaphopoda     = c("benthic mollusc"),
  BenthicMollusc_Polyplacophora = c("benthic mollusc"),
  
  Macrobenthos_Echinoidea      = c("other macro-benthos"),
  Macrobenthos_Holothuroidea   = c("other macro-benthos"),
  Macrobenthos_Hexacorallia    = c("other macro-benthos"),
  Macrobenthos_Demospongiae    = c("other macro-benthos"),
  Macrobenthos_Gymnolaemata    = c("other macro-benthos"),
  Macrobenthos_Thecostraca     = c("other macro-benthos"),
  Macrobenthos_Ophiuroidea     = c("other macro-benthos"),
  Macrobenthos_Polychaeta      = c("other macro-benthos"),
  Macrobenthos_Asteroidea      = c("other macro-benthos"),
  Macrobenthos_Crinoidea       = c("other macro-benthos"),
  Macrobenthos_Ascidiacea      = c("other macro-benthos"),
  Macrobenthos_Hydrozoa        = c("other macro-benthos"),
  Macrobenthos_Stenolaemata    = c("other macro-benthos"),
  Macrobenthos_Octocorallia    = c("other macro-benthos"),
  Macrobenthos_Articulata      = c("other macro-benthos"),
  Macrobenthos_Clitellata      = c("other macro-benthos"),
  Macrobenthos_Palaeonemertea  = c("other macro-benthos")
)
## 2026-09-24, per project decision (refining the 2026-09-24-earlier satellite
## approach): phytoplankton should come from a Mediterranean
## BIOGEOCHEMICAL MODEL (Copernicus Marine's Med BGC Reanalysis,
## MedBFM/OGSTM-BFM - reports phytoplankton biomass directly as carbon,
## not a Chl-a proxy) - see lib_cmems_phytoplankton_biomass.R. Satellite
## chlorophyll-a (lib_satellite_phytoplankton_biomass.R, kept as-is) is
## now the FALLBACK if CMEMS access isn't set up (it needs a free
## Copernicus Marine account + the copernicusmarine CLI - see that
## function's own header) or the query fails for any reason.
##
## Zooplankton (Macro-/MesoMicroZooplankton) - the SAME reanalysis has
## NO public zooplankton biomass variable at all (confirmed from its
## own product documentation - only phytoplankton/chlorophyll/
## nutrients/carbon system are released, even though the underlying
## BFM model simulates zooplankton internally). Zooplankton therefore
## still goes through EcoBase below, same as macroalgae/seagrass/
## gorgonians+corals - flagged explicitly so this isn't mistaken for
## an oversight; ask CMCC/OGS directly for their model's internal
## zooplankton output if you want the true biogeochemical-model value.
## 2026-09-30: split to match the two now-separate files - a group's
## keyword only enters this EcoBase evaluation query if ITS OWN file
## (PRIMARY_PRODUCER_BIOMASS_PATH for plankton, BENTHIC_HABITAT_
## BIOMASS_PATH for seagrass/macroalgae/coralligenous) doesn't exist
## yet - previously this whole block gated on PRIMARY_PRODUCER_
## BIOMASS_PATH alone, which (now that the benthic groups live in a
## different file) would have wrongly skipped the benthic groups'
## EcoBase evaluation whenever the plankton file happened to exist,
## and vice versa.
ECOBASE_LOWTROPHIC_KEYWORDS <- c(
  if (!file.exists(PRIMARY_PRODUCER_BIOMASS_PATH))
    PRIMARY_PRODUCER_TAXON_KEYWORDS[!names(PRIMARY_PRODUCER_TAXON_KEYWORDS) %in% c("LargePhytoplankton", "SmallPhytoplankton")]
  else list(),
  if (!file.exists(BENTHIC_HABITAT_BIOMASS_PATH)) BENTHIC_HABITAT_TAXON_KEYWORDS else list()
)

## This block used to auto-draft EcoBase's closest-matching-other-model
## figure DIRECTLY into the reviewed CSV whenever it didn't exist yet -
## i.e. once written, load_manual_cited_biomass_group() below treated
## an EcoBase guess exactly like a real reviewed literature row, and it
## became a real Ecopath_B input with no further review step. EcoBase
## is evaluation-only and must never be incorporated this way. Still
## queries EcoBase (still "evaluated"), but now writes its candidates
## to a clearly separate, clearly-named evaluation csv instead of the
## file the pipeline actually reads for Ecopath_B. Neither
## PRIMARY_PRODUCER_BIOMASS_PATH nor BENTHIC_HABITAT_BIOMASS_PATH is
## auto-created by this block at all - if a file doesn't exist, those
## FG(s) stay genuinely missing until a real literature/survey value
## is added by hand (same as every other FG with no EcoBase substitute).
if (length(ECOBASE_LOWTROPHIC_KEYWORDS) > 0 && ENABLE_ECOBASE_BIOMASS_QUERY) {
  ecobase_best_lowtrophic <- fetch_ecobase_literature_biomass(
    out_dir = csv_out_dir, force_refresh = ECOBASE_BIOMASS_FORCE_REFRESH,
    target_fg_keywords = ECOBASE_LOWTROPHIC_KEYWORDS, target_year = round(mean(YEAR_ECOPATH))
  )
  if (!is.null(ecobase_best_lowtrophic) && nrow(ecobase_best_lowtrophic) > 0) {
    ## Biomass_t_km2 (EcoBase's native Ecopath unit) -> a total tonnage,
    ## same convention load_manual_cited_biomass_group() expects
    ## (Biomass_t for the whole West Med study area) - scaled by the
    ## SAME Total_Area_km2 every other biomass figure in this script
    ## uses, via strata_area_by_area (already in scope this far down).
    ecobase_draft <- ecobase_best_lowtrophic[, .(
      Year = round(mean(YEAR_ECOPATH)),
      Group = TargetGroup,
      Biomass_t = Biomass_t_km2 * sum(strata_area_by_area$area_km2, na.rm = TRUE),
      Source_citation
    )]
    fwrite(ecobase_draft, file.path(csv_out_dir, "ecobase_biomass_evaluation_primary_producers_NOT_INCORPORATED.csv"))
    message("\n[Primary producer/plankton biomass] No reviewed CSV exists yet for one or more of these group(s) -",
            " EcoBase evaluation found a candidate for ", nrow(ecobase_draft), " group(s) (",
            paste(ecobase_draft$Group, collapse = ", "), "), written to",
            " ecobase_biomass_evaluation_primary_producers_NOT_INCORPORATED.csv for REFERENCE ONLY. This is",
            " NOT written into PRIMARY_PRODUCER_BIOMASS_PATH or BENTHIC_HABITAT_BIOMASS_PATH and does NOT",
            " feed Ecopath_B - these group(s) remain genuinely missing until you add a real literature/",
            " survey/biogeochemical-model row to the relevant file by hand.")
  }
}

## 2026-09-29, per project decision ("I see there is no phytoplankton biomass that",
## " should be extracted from the biogeochemical model"): root cause -
## the CMEMS/satellite phytoplankton fetch used to be nested INSIDE the
## `if (!file.exists(PRIMARY_PRODUCER_BIOMASS_PATH))` block above, i.e.
## an all-or-nothing gate on the WHOLE draft mechanism. The moment
## the team's own reviewed primary_producer_plankton_biomass.csv
## existed on disk (even with zero Phytoplankton rows in it, same as
## the already-documented Suprabenthos/Cymodocea/GelatinousZooplankton
## gap above), this entire block - including the phytoplankton fetch -
## was skipped ENTIRELY ("existing file always wins"), so
## fetch_cmems_phytoplankton_biomass()/fetch_satellite_phytoplankton_
## biomass() would never even be attempted, silently, no matter how
## many times the pipeline was re-run. Fixed by pulling the
## phytoplankton fetch out of that gate entirely: it now runs whenever
## Large/SmallPhytoplankton rows are missing from whatever CSV exists
## right now (freshly EcoBase-drafted above, or the project's own existing
## file), and APPENDS the fetched row(s) to that file rather than
## requiring the whole file to not exist - so it can never again be
## silently blocked by the rest of the file already being reviewed and
## real. Existing Phytoplankton rows (if the project owner has already reviewed
## and added her own) still always win - this only fills a genuine gap,
## never overwrites.
existing_primary_producer_csv <- if (file.exists(PRIMARY_PRODUCER_BIOMASS_PATH)) {
  tryCatch(fread(PRIMARY_PRODUCER_BIOMASS_PATH, encoding = "UTF-8"), error = function(e) NULL)
} else NULL
has_phyto_row <- FALSE
if (!is.null(existing_primary_producer_csv)) {
  grp_col <- resolve_iccat_col(names(existing_primary_producer_csv), c("Group", "Species", "FG_name", "Taxon", "CommonName"))
  if (!is.na(grp_col)) {
    has_phyto_row <- any(tolower(trimws(existing_primary_producer_csv[[grp_col]])) %in%
                           c("largephytoplankton", "smallphytoplankton"))
  }
}
if (!has_phyto_row) {
  if (!exists("ENABLE_CMEMS_PHYTOPLANKTON", envir = .GlobalEnv, inherits = FALSE)) ENABLE_CMEMS_PHYTOPLANKTON <- TRUE
  if (!exists("ENABLE_SATELLITE_PHYTOPLANKTON", envir = .GlobalEnv, inherits = FALSE)) ENABLE_SATELLITE_PHYTOPLANKTON <- TRUE
  phyto_draft <- NULL
  phyto_source_label <- NA_character_
  ## Both lib files below are sourced DEFENSIVELY - a missing file (e.g.
  ## these two new scripts haven't been copied into your local
  ## scripts/ folder yet alongside 01_biomass.R) degrades to a message
  ## and moves on to the next fallback, exactly like a failed network
  ## query does, rather than crashing the whole 01_biomass.R run over
  ## one optional lower-trophic biomass source.
  cmems_lib_path <- file.path(git_dir, "scripts/lib_cmems_phytoplankton_biomass.R")
  satellite_lib_path <- file.path(git_dir, "scripts/lib_satellite_phytoplankton_biomass.R")
  
  if (ENABLE_CMEMS_PHYTOPLANKTON) {
    if (!file.exists(cmems_lib_path)) {
      message("[Primary producer/plankton biomass] '", cmems_lib_path, "' not found - copy",
              " lib_cmems_phytoplankton_biomass.R into your scripts/ folder to enable this source.",
              " Skipping straight to the satellite fallback for this run.")
    } else {
      source(cmems_lib_path)
      cmems_phyto <- fetch_cmems_phytoplankton_biomass(out_dir = csv_out_dir)
      if (!is.null(cmems_phyto) && nrow(cmems_phyto) > 0) {
        phyto_draft <- cmems_phyto
        phyto_source_label <- "CMEMS MED BGC REANALYSIS (biogeochemical model, phytoplankton carbon)"
      }
    }
  }
  if (is.null(phyto_draft) && ENABLE_SATELLITE_PHYTOPLANKTON) {
    message("[Primary producer/plankton biomass] CMEMS phytoplankton unavailable this run",
            " (ENABLE_CMEMS_PHYTOPLANKTON = FALSE, the lib file wasn't found, or the query above",
            " failed/wasn't set up) - falling back to satellite chlorophyll-a.")
    if (!file.exists(satellite_lib_path)) {
      message("[Primary producer/plankton biomass] '", satellite_lib_path, "' ALSO not found - copy",
              " lib_satellite_phytoplankton_biomass.R into your scripts/ folder to enable this fallback.",
              " Large/SmallPhytoplankton will remain missing this run - both the CMEMS biogeochemical-model",
              " source and its satellite fallback are unavailable in this environment.")
    } else {
      source(satellite_lib_path)
      satellite_phyto <- fetch_satellite_phytoplankton_biomass(out_dir = csv_out_dir)
      if (!is.null(satellite_phyto) && nrow(satellite_phyto) > 0) {
        phyto_draft <- satellite_phyto
        phyto_source_label <- "SATELLITE CHLOROPHYLL-A (fallback - CMEMS biogeochemical model was unavailable)"
      }
    }
  }
  if (!is.null(phyto_draft)) {
    phyto_rows <- phyto_draft[, .(
      Year = round(mean(YEAR_ECOPATH)),
      Group = TargetGroup,
      Biomass_t = Biomass_t_km2 * sum(strata_area_by_area$area_km2, na.rm = TRUE),
      Source_citation = paste0("AUTO-DRAFT FROM ", phyto_source_label, " (2026-09-29) - REVIEW BEFORE TRUSTING: ", Source_citation)
    )]
    if (file.exists(PRIMARY_PRODUCER_BIOMASS_PATH)) {
      fwrite(phyto_rows, PRIMARY_PRODUCER_BIOMASS_PATH, append = TRUE)
      message("\n[Primary producer/plankton biomass] Appended ", nrow(phyto_rows), " Phytoplankton row(s) (",
              paste(phyto_rows$Group, collapse = ", "), ") from ", phyto_source_label, " to the EXISTING '",
              PRIMARY_PRODUCER_BIOMASS_PATH, "' - every other row in that file is untouched. REVIEW the",
              " appended row(s) before trusting them.")
    } else {
      fwrite(phyto_rows, PRIMARY_PRODUCER_BIOMASS_PATH)
      message("\n[Primary producer/plankton biomass] Created '", PRIMARY_PRODUCER_BIOMASS_PATH, "' with ",
              nrow(phyto_rows), " Phytoplankton row(s) from ", phyto_source_label, ". REVIEW before trusting.")
    }
  } else {
    message("[Primary producer/plankton biomass] No Phytoplankton row exists yet and BOTH the CMEMS",
            " biogeochemical-model source and the satellite chlorophyll-a fallback were unavailable this run",
            " (lib file(s) not found in '", file.path(git_dir, "scripts"), "', or the query failed) - Large/",
            "SmallPhytoplankton will have no Ecopath_B this run. If lib_cmems_phytoplankton_biomass.R exists in",
            " your real scripts/ folder, check it's actually being found at that exact path, and that a",
            " Copernicus Marine account/copernicusmarine CLI is set up for it to query.")
  }
}

primary_producer_biomass_fg_year <- load_manual_cited_biomass_group(
  csv_path = PRIMARY_PRODUCER_BIOMASS_PATH, taxon_keywords = PRIMARY_PRODUCER_TAXON_KEYWORDS,
  label = "Primary producer/plankton biomass", review_csv_name = "primary_producer_group_to_fg_REVIEW.csv",
  output_csv_name = "primary_producer_plankton_biomass_by_fg.csv",
  fallback_message = paste0("A single 1994-1996 baseline figure per group is enough - no time series needed.",
                            " MEDITS/MEDIAS are bottom-trawl surveys and don't sample these groups at all (same reason they're in",
                            " EXEMPT_FG_NAMES above). With ENABLE_ECOBASE_BIOMASS_QUERY/ENABLE_CMEMS_PHYTOPLANKTON/",
                            " ENABLE_SATELLITE_PHYTOPLANKTON = TRUE (all default) this file should have been auto-drafted just",
                            " above - if you're seeing this message, every query failed (check the [EcoBase biomass]/",
                            " fetch_cmems_phytoplankton_biomass()/fetch_satellite_phytoplankton_biomass() messages above) or",
                            " matched nothing. Other real sources: Posidonia standing biomass per m2 (seagrass-ecology",
                            " literature, e.g. Pergent et al., or satellite/Sentinel-2-derived meadow extent x that per-m2",
                            " figure) x actual meadow area. Expected columns: Year, Group (a target group name above, or a",
                            " specific FG_name), Biomass_t (total for the whole West Med study area, not a density),",
                            " Source_citation.")
)
primary_producer_species_lit_biomass <- attr(primary_producer_biomass_fg_year, "species_detail")

## 2026-09-30: benthic/seagrass/coralligenous groups' own file, separate
## from plankton (see BENTHIC_HABITAT_TAXON_KEYWORDS/BENTHIC_HABITAT_
## BIOMASS_PATH comments above). Same load function, same Density_value/
## Depth_min_m/Depth_max_m/Habitat_species/Habitat_area_km2 extrapolation
## mechanism - Habitat_area_km2 is the column a real EMODnet/EUSeaMap
## habitat-extent shapefile area (clipped to the study domain) belongs
## in, in place of a depth-band assumption, for exactly the patchy
## habitats this file covers.
benthic_habitat_biomass_fg_year <- load_manual_cited_biomass_group(
  csv_path = BENTHIC_HABITAT_BIOMASS_PATH, taxon_keywords = BENTHIC_HABITAT_TAXON_KEYWORDS,
  label = "Benthic habitat biomass (seagrass/macroalgae/coralligenous)",
  review_csv_name = "benthic_habitat_group_to_fg_REVIEW.csv",
  output_csv_name = "benthic_habitat_biomass_by_fg.csv",
  fallback_message = paste0("A single 1994-1996 baseline figure per group is enough - no time series needed.",
                            " MEDITS/MEDIAS are bottom-trawl surveys and don't sample these groups at all (same reason",
                            " they're in EXEMPT_FG_NAMES above). Real sources: a literature density figure",
                            " (Density_value/Density_unit) combined with either that taxon's real habitat depth range",
                            " (Depth_min_m/Depth_max_m) or, better for a patchy habitat, its real extent from an",
                            " EMODnet Seabed Habitats/EUSeaMap shapefile clipped to the study domain, supplied directly",
                            " as Habitat_area_km2 (see extrapolate_density_row()'s Habitat_area_km2_override -",
                            " already confirmed real West Med areas exist for Posidonia (~10,511 km2, EUNIS MB252/",
                            " MB2522) and coralligenous/gorgonian habitat (~1,469 km2, EUNIS MC151/MC251); no",
                            " EUSeaMap class exists for Cymodocea specifically - see benthic_habitat_biomass_sourcing_",
                            " guide project notes). Expected columns: Year, Group (a target group name above, or a",
                            " specific FG_name), Biomass_t OR Density_value/Density_unit (+ Depth_min_m/Depth_max_m",
                            " or Habitat_species or Habitat_area_km2), Source_citation.")
)
benthic_habitat_species_lit_biomass <- attr(benthic_habitat_biomass_fg_year, "species_detail")

## 2026-09-29, per project decision ("the proportion of species within FG should
## include all values inputted in B Ecopath, including the literature
## ones... it will be used for the pbqb"): combine both manual-cited
## sources' per-species detail (where a Species column was supplied -
## see load_manual_cited_biomass_group()'s Species handling above) into
## one table used below, when FG_spp_Ecopath is built, to set prop_sp_fg
## DIRECTLY from whichever literature figures actually built that FG's
## Ecopath_B total - instead of a separately-maintained relative-weight
## CSV that could in principle drift out of step with the real numbers.
species_lit_biomass <- rbindlist(list(megafauna_species_lit_biomass, primary_producer_species_lit_biomass,
                                      benthic_habitat_species_lit_biomass),
                                 use.names = TRUE, fill = TRUE)
if (nrow(species_lit_biomass) > 0) {
  message("[Species-level literature biomass] ", uniqueN(species_lit_biomass$Species), " species across ",
          uniqueN(species_lit_biomass$FG_num), " FG(s) have a direct literature biomass figure (not just an",
          " FG-level total) - these will set prop_sp_fg directly in FG_spp_Ecopath below.")
}

## 2026-09-29, per project decision (pasted two real sources: Telesca et al. 2015 on
## Posidonia decline, Linares et al. 2021 on gorgonian collapse from
## marine heatwaves): same "flag the real direction, never fabricate a
## ratio" pattern already used for ICCAT BFT/SWO (ICCAT_TEMPORAL_BASELINE_
## NOTE above) and the generic Collection_year mechanism inside
## load_manual_cited_biomass_group() - but those two only flag a generic
## "review whether this was higher/lower" message. These two groups now
## get the actual documented direction and a real citation, same as BFT/
## SWO got by hand. Matched on FG_name (the aggregated group_fg_year
## output collapses away the original "Group" column), using the same
## keyword text as PRIMARY_PRODUCER_TAXON_KEYWORDS's Posidonia/
## GorgoniansCorals entries above.
PRIMARY_PRODUCER_TEMPORAL_BASELINE_NOTE <- list(
  Posidonia = paste0(
    "TEMPORAL TREND (Mediterranean-wide, real citation): Telesca et al. 2015 (Scientific Reports 5:12505,",
    " 'Seagrass meadows (Posidonia oceanica) distribution and trajectories of change') report a 34%",
    " regression in Posidonia oceanica meadow AREA over the last ~50 years across sites with historical",
    " data (Spain, France/Monaco, Italy, Albania, Tunisia, Egypt, Turkey) - a basin-wide DECLINING",
    " trend, not a Western-Med-specific or 1994-1996-anchored figure. If this FG's standing-biomass",
    " density comes from a present-day/2010s+ field measurement (e.g. Bernardeau-Esteller et al. 2023,",
    " Cadiz), the 1994-1996 Ecopath baseline biomass was very likely HIGHER than that measurement",
    " implies. No numeric correction factor applied - the 34%-over-50-years figure is basin-wide areal",
    " extent, not a per-site biomass-density trend, so it is not safely convertible into a multiplier",
    " for one site's density figure."
  ),
  GorgoniansCorals = paste0(
    "TEMPORAL TREND (NW Mediterranean, real citation): Linares et al. 2021 (Proceedings of the Royal",
    " Society B 288:20212384, 'Population collapse of habitat-forming species in the Mediterranean: a",
    " long-term study of gorgonian populations affected by recurrent marine heatwaves') document",
    " gorgonian population COLLAPSE driven by recurrent marine-heatwave mass-mortality events",
    " (documented from the early 2000s onward). If this FG's AFDM-density figure comes from a",
    " post-2010s field survey (e.g. Ambroso et al. 2019, Cap de Creus), the 1994-1996 Ecopath baseline",
    " biomass was very likely HIGHER than that measurement implies (i.e. before most of the documented",
    " heatwave mortality). No numeric correction factor applied - the paper reports population/cover",
    " trend at specific monitored sites, not a basin-wide biomass-density multiplier."
  )
)
## 2026-09-30: Posidonia/GorgoniansCorals now live in benthic_habitat_
## biomass_fg_year (see the 3-CSV split above), not primary_producer_
## biomass_fg_year - this block moved to match, unchanged otherwise.
if (nrow(benthic_habitat_biomass_fg_year) > 0) {
  temporal_trend_hits <- list(
    Posidonia        = grepl("posidonia|seagrass", benthic_habitat_biomass_fg_year$FG_name, ignore.case = TRUE),
    GorgoniansCorals = grepl("gorgonian|coral", benthic_habitat_biomass_fg_year$FG_name, ignore.case = TRUE)
  )
  for (grp_nm in names(PRIMARY_PRODUCER_TEMPORAL_BASELINE_NOTE)) {
    hit_rows <- temporal_trend_hits[[grp_nm]]
    if (any(hit_rows)) {
      benthic_habitat_biomass_fg_year[hit_rows, group_sources :=
                                        paste0(group_sources, " -- ", PRIMARY_PRODUCER_TEMPORAL_BASELINE_NOTE[[grp_nm]])]
      message("[Benthic habitat biomass] Temporal-trend note appended for '", grp_nm, "' (",
              sum(hit_rows), " FG x Year row(s)) - see group_sources / B_ref.")
    }
  }
  fwrite(benthic_habitat_biomass_fg_year[temporal_trend_hits$Posidonia | temporal_trend_hits$GorgoniansCorals,
                                         .(FG_num, FG_name, Year, group_biomass_t, group_sources)],
         file.path(csv_out_dir, "manual_cited_biomass_temporal_trend_REVIEW.csv"))
}

## 2026-09-30, per project decision ("why there are FGs without stock assessment",
## " in the stock_assessment_biomass_crosscheck.csv? it shouldnt be",
## " (remove megafauna, invertebrates and algae)"): root cause found -
## stock_assessment_fg_year gets megafauna_biomass_fg_year and
## primary_producer_biomass_fg_year (which, despite its name, also
## covers invertebrates like Suprabenthos/GelatinousZooplankton/benthic
## mollusc & macrobenthos subgroups, and "algae" - Macroalgae/Posidonia/
## GorgoniansCorals) merged INTO it right below, for the FG biomass-
## PRIORITY step further down (that merge is correct and unrelated to
## this fix). The problem: stock_assessment_biomass_crosscheck.csv,
## built further down, was built from this SAME merged variable - so
## megafauna/invertebrate/algae FGs that were never actually stock-
## assessed ended up as rows in a file literally named "stock
## assessment crosscheck", with no real stock_assessment_density_t_km2
## behind them. Snapshotting the GENUINE fisheries stock-assessment-only
## table here, before the merges, so the crosscheck built later can use
## THIS instead - megafauna/invertebrates/algae then simply never appear
## in that file at all, rather than appearing with a blank/NA assessment
## value.
stock_assessment_fg_year_fisheries_only <- copy(stock_assessment_fg_year)

stock_assessment_fg_year <- merge_manual_cited_biomass(stock_assessment_fg_year, megafauna_biomass_fg_year, "marine megafauna")
stock_assessment_fg_year <- merge_manual_cited_biomass(stock_assessment_fg_year, primary_producer_biomass_fg_year, "primary producer/plankton")
stock_assessment_fg_year <- merge_manual_cited_biomass(stock_assessment_fg_year, benthic_habitat_biomass_fg_year, "benthic habitat (seagrass/macroalgae/coralligenous)")