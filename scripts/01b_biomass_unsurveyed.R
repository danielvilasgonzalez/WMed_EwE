## =================================================================
## 01b_biomass_unsurveyed.R
##
## Every "literature/stock-assessment" biomass source this pipeline
## uses, consolidated in one file: GFCM STAR/RAM Legacy + ICCAT
## stock-assessment biomass (single-species/stanza FGs a real
## assessment covers), AND
## biomass for FGs MEDITS/MEDIAS bottom-trawl surveys cannot reach at
## all - marine megafauna (cetaceans, seabirds, sea turtles) and the
## benthic/pelagic groups a bottom-trawl survey isn't designed to
## sample (phytoplankton, zooplankton, seagrass, macroalgae,
## coralligenous fauna, suprabenthos, and similar). Every figure here
## comes from either a real stock assessment (STAR/RAM Legacy, ICCAT)
## or a manually-curated, cited CSV (literature, aerial/acoustic survey
## reports, EMODnet/EUSeaMap habitat-extent shapefiles) - never from
## 01_biomass.R's own MEDITS/MEDIAS survey pipeline. 01_biomass.R
## itself now covers only what MEDITS/MEDIAS actually measured; this
## file covers everything else that feeds Ecopath_B.
##
## Sourced by 01_biomass.R (inside its `AREA_MODE == "westmed"` block,
## right after MEDIAS' own FG/species density is built) via:
##   source(file.path(git_dir, "scripts/01b_biomass_unsurveyed.R"), local = TRUE)
## `local = TRUE` keeps it in 01_biomass.R's own execution environment,
## so it reads and extends the same variables in place - no separate
## script invocation, no intermediate CSV round-trip, no risk of the
## two files drifting out of sync on what a variable means.
##
## Expects already in scope from 01_biomass.R: `csv_out_dir`,
## `strata_area_by_area`, `pcloud_dir`, `git_dir`, `YEAR_ECOPATH`,
## `FILTER_AREAS`, `MEDITS_STRATA`, `fg_lookup_safe`, `dt`,
## `resolve_iccat_col()`, `fetch_ecobase_literature_biomass()`
## (sourced from 03b_ecobase.R earlier in 01_biomass.R), and the
## `MEDITS_REFERENCE_DIR`/`read_medits_reference()` reference-table
## helper (defined earlier in 01_biomass.R). `total_area_km2_biomass`
## is not needed from outside - the STAR/RAM block below computes its
## own copy, self-contained. `stock_assessment_fg_year` is not
## pre-built either; this file builds it itself, from scratch, as its
## first step.
##
## Produces (read by 01_biomass.R immediately after this file returns):
## `stock_assessment_fg_year` (built here from STAR/RAM Legacy + ICCAT,
## then extended with the megafauna/primary-producer-plankton/benthic-
## habitat literature merges), `stock_assessment_fg_year_fisheries_only`
## (a snapshot of the survey/stock-assessment-only version, taken
## before those literature merges - used downstream so files literally
## named "stock assessment" never include an FG that was never actually
## stock-assessed), and `species_lit_biomass` (species-level detail from
## whichever literature rows supplied a Species column - feeds
## FG_spp_Ecopath's prop_sp_fg further down in 01_biomass.R).
##
## SOURCE ROADMAP (project decision 2026-10-05) - which FG gets its
## biomass from which kind of source (search "SOURCE A" ... "SOURCE D"):
##
##  A. STOCK ASSESSMENT (ICCAT + GFCM STAR/RAM Legacy): single-species FGs
##     the surveys don't measure - bluefin tuna, swordfish. Used only when
##     MEDIAS and MEDITS have no value for that FG (01_biomass.R priority).
##     Marine megafauna (cetaceans, seabirds, turtles) come from their own
##     cited CSV (marine_megafauna_biomass.csv) in the same section.
##  B. LITERATURE density x depth band (literature_density_biomass.csv):
##     benthic molluscs (bivalves, gastropods...), other macrobenthos
##     (echinoderms, polychaetes, sponges, ascidians, bryozoans,
##     hydrozoans...), soft-bottom octocorals (sea pens) in Corals and
##     gorgonians, suprabenthos. Molluscs/macrobenthos: literature wins
##     over MEDITS, MEDITS is the fallback ("literature_first").
##  C. SHAPEFILE habitat area x literature density x occupancy
##     (habitat_shapefile_biomass.csv): coralligenous gorgonians (Corals
##     and gorgonians), Posidonia, Cymodocea, macroalgae.
##  D. BIOGEOCHEMICAL MODELS / SATELLITE (PLANKTON BIOMASS section):
##     large + small phytoplankton (MedBFM reanalysis; PISCES and
##     satellite chlorophyll as cross-check/fallback), macro and
##     meso+micro zooplankton (SEAPODYM LMTL).
##  B and C are added together per FG x Year; A-D all end up in
##  stock_assessment_fg_year for 01_biomass.R's priority rule.
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

## #################################################################
## SOURCE A - STOCK ASSESSMENTS (+ marine megafauna literature)
## #################################################################
## =================================================================
## Stock-assessment (GFCM STAR/RAM Legacy + ICCAT) biomass. Builds
## stock_assessment_fg_year from scratch (STAR/RAM Legacy load + ICCAT
## load + the ICCAT-wins-on-overlap merge); 01_biomass.R does not
## pre-build it. Needs star_ram_combined's own helper extract_genus()
## plus whatever resolve_iccat_col()/fg_lookup_safe/strata_area_by_area/
## FILTER_AREAS/dt/csv_out_dir/pcloud_dir/git_dir/YEAR_ECOPATH are
## already in scope from 01_biomass.R (same local=TRUE execution
## environment as always).
## =================================================================
## =================================================================
## Stock-assessment (GFCM STAR / RAM Legacy) biomass, matched to FG x
## Year - built before the MEDITS+MEDIAS combine step below, so it
## can feed the priority rule that step applies (single-species /
## stanza FGs use it as PRIMARY; every other FG keeps it validation-
## only: "priority of FG estimates MEDIAS>MEDITS, then if FG single
## species and have stock assessment then stock assessment, if a
## species is FG stanza then stock assessment, the rest MEDITS" -
## see the "FG biomass-source priority" block below for the actual
## rule). Expects the combine_STAR_RAMlegacy.R output,
## combined_medbs_star_ramlegacy.csv (source/stock_key/species/
## common_name/gsa/subregion/year/biomass/catches/landings/
## landings_flag/... - see that script's own header), placed under
## STAR_RAMLEGACY_DIR. Optional - message + skip if not found, same
## convention as every other optional source in this pipeline.
## =================================================================
extract_genus <- function(sci_name) str_extract(sci_name, "^[A-Za-z]+")  # pull the genus (first word) from a scientific name

STAR_RAMLEGACY_DIR <- file.path(pcloud_dir, "data/fisheries/STAR_RAMLegacy")  # folder for combine_STAR_RAMlegacy.R's own output CSV
star_ram_path <- file.path(STAR_RAMLEGACY_DIR, "combined_medbs_star_ramlegacy.csv")
star_ram_combined <- if (file.exists(star_ram_path)) {
  fread(star_ram_path, encoding = "UTF-8")
} else {
  message("\n[Stock-assessment biomass] '", star_ram_path, "' not found - skipping (run",
          " combine_STAR_RAMlegacy.R first and place its output under STAR_RAMLEGACY_DIR",
          " if you want stock-assessment biomass available for the priority rule below).")
  data.table()
}

## Empty but with the right COLUMNS, not a bare data.table() - a
## zero-column empty data.table crashes any later `dt[, .(FG_num, ...)]`
## column select with "object 'FG_num' not found" the moment
## combine_STAR_RAMlegacy.R's output genuinely isn't there (the normal,
## documented "optional, skip if not found" case above, not an error
## condition) - this is what was hit during testing.
stock_assessment_fg_year <- data.table(FG_num = integer(0), FG_name = character(0), Year = integer(0),
                                       star_biomass_t = numeric(0), star_n_stocks = integer(0),
                                       star_sources = character(0), stock_assessment_density_t_km2 = numeric(0),
                                       stock_assessment_source = character(0))  # per-row source label - "STAR/RAM" or "ICCAT", see the ICCAT block below
if (nrow(star_ram_combined) > 0) {
  ## Restrict to the West Med subregion, same convention as the
  ## fisheries-side STAR/RAM catch cross-check (02_fisheries.R).
  ## Subregion in the real file is "Mediterranean-Black Sea" for every
  ## row - the old grepl("Western Mediterranean", subregion) kept 0 rows.
  ## Filter on the stock's own GSA list instead: keep a stock only if
  ## EVERY GSA it covers is inside FILTER_AREAS (a Med-wide or partly
  ## eastern stock can't be attributed to this domain). Blank GSA = drop.
  .gsa_in_scope <- function(g, areas) {
    v <- suppressWarnings(as.numeric(floor(as.numeric(trimws(unlist(strsplit(g, "[,;]")))))))
    length(v) > 0 && !anyNA(v) && all(v %in% areas)
  }
  target_gsas <- if (exists("FILTER_AREAS") && !is.null(FILTER_AREAS)) as.numeric(FILTER_AREAS) else 1:11
  star_ram_combined[, gsa := as.character(gsa)]
  star_wm <- star_ram_combined[!is.na(gsa) & nzchar(gsa)][vapply(gsa, .gsa_in_scope, logical(1), areas = target_gsas)]
  star_wm <- star_wm[!is.na(biomass)]
  ## Avoid double counting: the same species x year x GSA can be covered by
  ## more than one assessment (e.g. a single-GSA and a joint multi-GSA
  ## stock). Keep, per species x year, only stocks whose GSAs don't overlap
  ## a stock with WIDER coverage already kept.
  if (nrow(star_wm) > 0) {
    star_wm[, n_gsa := lengths(strsplit(gsa, "[,;]"))]
    setorder(star_wm, species, year, -n_gsa)
    keep_idx <- star_wm[, {
      covered <- character(0); keep <- logical(.N)
      for (i in seq_len(.N)) {
        g <- trimws(unlist(strsplit(gsa[i], "[,;]")))
        if (!any(g %in% covered)) { keep[i] <- TRUE; covered <- c(covered, g) }
      }
      .(keep = keep)
    }, by = .(species, year)]$keep
    n_overlap <- sum(!keep_idx)
    star_wm <- star_wm[keep_idx]
    if (n_overlap > 0) message("[Stock-assessment biomass] ", n_overlap, " species x year x stock row(s) dropped",
                               " because another assessment with wider GSA coverage already covers those GSAs (no double counting).")
    star_wm[, n_gsa := NULL]
  }
  message("[Stock-assessment biomass] ", nrow(star_wm), " row(s), ", uniqueN(star_wm$stock_key),
          " stock(s) with every GSA inside ", paste(range(target_gsas), collapse = "-"), ".")
  star_wm[, genus := extract_genus(species)]
  
  ## Species -> FG_num, exact scientific-name match first, genus
  ## fallback second - identical cascade to the fisheries-side match,
  ## reusing fg_lookup_safe's own Genus column (from fetch_taxonomy())
  ## rather than re-deriving genus for every FG species.
  star_direct <- merge(star_wm, fg_lookup_safe[, .(ScientificName, FG_num, FG_name)],
                       by.x = "species", by.y = "ScientificName")
  star_remaining <- fsetdiff(star_wm[, .(source, stock_key, species, gsa, year)],
                             star_direct[, .(source, stock_key, species, gsa, year)])
  star_remaining <- merge(star_remaining, star_wm, by = c("source", "stock_key", "species", "gsa", "year"))
  star_genus <- merge(star_remaining[!is.na(genus)],
                      unique(fg_lookup_safe[!is.na(Genus), .(genus = Genus, FG_num, FG_name)]),
                      by = "genus", allow.cartesian = TRUE)
  star_matched <- rbindlist(list(star_direct, star_genus), use.names = TRUE, fill = TRUE)
  ## A species mapped to more than one FG (juvenile/adult stanzas, e.g.
  ## hake FG26/FG27) would get its WHOLE assessed stock biomass copied into
  ## every stanza. Assessments report total stock biomass, so these are
  ## left to the survey + MEDITS maturity split instead.
  multi_fg_sp <- unique(c(star_matched[, uniqueN(FG_num), by = species][V1 > 1, species],
                          ## stanza FGs (juvenile/adult) - the lookup may keep only one
                          ## stanza per species, so check the FG name too
                          star_matched[grepl("\\bjuv|\\badult", FG_name, ignore.case = TRUE), species]))
  if (length(multi_fg_sp) > 0) {
    message("[Stock-assessment biomass] Excluded (maps to more than one FG - stanza split handled by the survey):",
            " ", paste(multi_fg_sp, collapse = ", "))
    star_matched <- star_matched[!species %in% multi_fg_sp]
  }
  ## Small pelagics (sardine, anchovy...) come from the MEDIAS acoustic
  ## survey, per project decision - any FG that MEDIAS measures is never
  ## taken from a stock assessment, in any year.
  medias_fgs <- if (exists("medias_fg_index_regional") && nrow(medias_fg_index_regional) > 0)
    unique(medias_fg_index_regional$FG_num) else integer(0)
  ## Kept aside (not used as biomass) for the MEDIAS 1995 back-cast
  ## comparison in 01_biomass.R: the assessments' own trend between the
  ## Ecopath baseline years and MEDIAS' first years.
  small_pelagic_assessments <- star_matched[FG_num %in% medias_fgs]
  if (length(medias_fgs) > 0 && any(star_matched$FG_num %in% medias_fgs)) {
    message("[Stock-assessment biomass] Excluded - FG measured by MEDIAS (acoustic survey is the source for these): ",
            paste(unique(star_matched[FG_num %in% medias_fgs, paste0(FG_name, " (", species, ")")]), collapse = ", "))
    star_matched <- star_matched[!FG_num %in% medias_fgs]
  }
  
  n_star_unmatched <- uniqueN(star_wm$species) - uniqueN(star_matched$species)
  message("\n[Stock-assessment biomass] ", uniqueN(star_matched$species), " of ", uniqueN(star_wm$species),
          " assessed species matched to a FG (exact name or genus fallback); ", max(n_star_unmatched, 0),
          " species could not be matched and are excluded.")
  
  ## Full-extent stock-assessment species snapshot for
  ## build_full_fg_species_catalog() - star_wm is already the whole West
  ## Med subregion (not restricted by THIS run's FILTER_AREAS), so this is
  ## every species combine_STAR_RAMlegacy.R's output contains for the
  ## region, matched (a real FG_num/FG_name) or not (NA, excluded above).
  star_unmatched_species <- setdiff(unique(star_wm$species), unique(star_matched$species))
  star_all_species <- rbindlist(list(
    unique(star_matched[, .(species, FG_num, FG_name)]),
    if (length(star_unmatched_species) > 0) {
      ## Guard against a zero-length `species` recycling a scalar NA into
      ## a single phantom NA row (a genuine data.table/data.frame gotcha)
      ## when every assessed species matched - i.e. nothing to add here.
      data.table(species = star_unmatched_species, FG_num = NA_real_, FG_name = NA_character_)
    } else {
      NULL
    }
  ), fill = TRUE)
  snapshot_full_extent_species(star_all_species, species_taxonomy, "stock_assessment", csv_out_dir,
                               sci_name_col = "species")
  
  ## Sum assessed biomass (absolute tons) across every West Med GSA in
  ## scope - stock assessments report an absolute stock total, not a
  ## density, so summing across GSAs gives the whole-domain total
  ## directly (same principle as the fisheries-side STAR/RAM catch
  ## cross-check).
  ## Density over the area the assessments actually cover. A stock
  ## assessed for GSA 6 + 7 says nothing about GSAs 1-5 or 8-11, so its
  ## tonnage is divided by the area of the GSAs it covers (from
  ## strata_area_by_area), not by the whole domain - dividing by the whole
  ## domain silently assumed zero biomass everywhere else.
  ## FLAGGED ASSUMPTION: that covered-area density is then applied to the
  ## whole domain. gsa_coverage_frac reports how much of the domain the
  ## assessments actually cover, so partial coverage stays visible.
  total_area_km2_biomass <- sum(strata_area_by_area$area_km2, na.rm = TRUE)
  gsa_area <- strata_area_by_area[, .(gsa_area_km2 = sum(area_km2, na.rm = TRUE)), by = .(GSA = as.character(AreaID))]  # in-memory id column is AreaID (= GSA number in the West Med run)
  star_biomass_by_fg <- star_matched[, {
    gs <- unique(trimws(unlist(strsplit(gsa, "[,;]"))))
    cov_area <- sum(gsa_area[GSA %in% gs, gsa_area_km2])
    .(star_biomass_t = sum(biomass, na.rm = TRUE),
      star_n_stocks  = uniqueN(stock_key),
      star_sources   = paste(sort(unique(source)), collapse = "+"),
      star_gsas      = paste(sort(as.numeric(gs)), collapse = ","),
      covered_area_km2 = cov_area)
  }, by = .(FG_num, FG_name, Year = year)]
  star_biomass_by_fg[, gsa_coverage_frac := covered_area_km2 / total_area_km2_biomass]
  star_biomass_by_fg[, stock_assessment_density_t_km2 := fifelse(covered_area_km2 > 0, star_biomass_t / covered_area_km2, NA_real_)]
  if (any(star_biomass_by_fg$gsa_coverage_frac < 0.5, na.rm = TRUE)) {
    message("[Stock-assessment biomass] FG(s) whose assessments cover < 50% of the domain area (density from the",
            " covered GSAs is applied domain-wide - review before letting it override the survey):")
    print(unique(star_biomass_by_fg[gsa_coverage_frac < 0.5, .(FG_num, FG_name, star_gsas,
                                                               coverage = round(gsa_coverage_frac, 2))]))
  }
  
  ## Minimum share of the domain area the assessments must cover before
  ## they're allowed into the priority rule (0 = any coverage, the
  ## current project rule). Below it, the survey estimate is kept.
  if (!exists("STOCK_ASSESSMENT_MIN_COVERAGE")) STOCK_ASSESSMENT_MIN_COVERAGE <- 0
  n_low_cov <- star_biomass_by_fg[star_biomass_t > 0 & gsa_coverage_frac < STOCK_ASSESSMENT_MIN_COVERAGE, .N]
  if (n_low_cov > 0) message("[Stock-assessment biomass] ", n_low_cov, " FG x Year row(s) dropped: assessments cover less than ",
                             STOCK_ASSESSMENT_MIN_COVERAGE * 100, "% of the domain (STOCK_ASSESSMENT_MIN_COVERAGE).")
  stock_assessment_fg_year <- star_biomass_by_fg[star_biomass_t > 0 & gsa_coverage_frac >= STOCK_ASSESSMENT_MIN_COVERAGE]
  ## star_sources (just above) carries the real per-row provenance from
  ## star_ram_combined's own "source" column (which GFCM/STAR/RAM Legacy
  ## assessment) - embed it instead of a generic label with no way to
  ## trace which specific assessment it came from.
  stock_assessment_fg_year[, stock_assessment_source := paste0("stock assessment (STAR/RAM Legacy Database): ", star_sources)]
  fwrite(stock_assessment_fg_year, file.path(csv_out_dir, "stock_assessment_biomass_by_fg.csv"))
  message("[Stock-assessment biomass] stock_assessment_fg_year: ", nrow(stock_assessment_fg_year),
          " FG x Year row(s) (", uniqueN(stock_assessment_fg_year$FG_num), " FG(s)) - written to",
          " stock_assessment_biomass_by_fg.csv. Area used for the density conversion: ",
          round(total_area_km2_biomass, 1), " km^2.")
}

## =================================================================
## ICCAT stock-assessment BIOMASS (SSB) - Atlantic bluefin tuna,
## Mediterranean swordfish, Mediterranean albacore: the biomass-side
## counterpart to the ICCAT catch block in 02_fisheries.R. Same 3
## ICCAT-managed highly-migratory stocks, same reasoning: ICCAT is the
## RFMO that actually assesses them directly (GFCM STAR/RAM Legacy's
## own bluefin/swordfish coverage, on the rare years it has any, is
## typically a re-publication of ICCAT's own assessment one step
## removed).
##
## Folded into the SAME stock_assessment_fg_year table STAR/RAM built
## above, rather than a parallel table with its own copy of the
## priority-rule logic below - ICCAT simply wins wherever it and
## STAR/RAM both cover a FG x Year cell.
##
## IMPORTANT: Bluefin tuna (FG 12, Thunnus thynnus) and Swordfish
## (FG 13, Xiphias gladius) are already their own single-species FGs,
## so their ICCAT biomass rows attach automatically below. Albacore
## (Thunnus alalunga) is NOT currently in FG_WMed_2026.csv at all - no
## FG represents it yet - so its ICCAT biomass will load and
## cross-check fine but has nowhere to attach until Thunnus alalunga
## is added to FG_WMed_2026.csv as its own FG (same convention as
## Bluefin tuna/Swordfish - see 01_biomass.R's own "Seed FG rules:
## REMOVED ENTIRELY" comment elsewhere in this file for why a
## code-side FG assignment isn't the right fix here either).
##
## Expects a small per-stock CSV at iccat_biomass_path with a year
## column, a biomass/SSB-in-tonnes column, and EITHER a species-code
## OR a species-name/stock column (several plausible header spellings
## accepted for each - see ICCAT_BIOMASS_COL_ALIASES). ICCAT doesn't
## publish SSB across every stock assessment via one uniform bulk
## export the way its Task I catch database works (see the ICCAT
## catch block in 02_fisheries.R) - this file is expected to be built
## by hand from the relevant stock assessment's own SSB table (one
## row per stock x year is all this needs). Optional - message + skip
## if not found, same convention as every other optional source in
## this pipeline.
##
## ICCAT_DIR is the same path 02_fisheries.R's own ICCAT catch block
## computes, redefined here the same cheap/harmless way
## STAR_RAMLEGACY_DIR already is in both scripts - this script is a
## standalone SOURCE-ABLE SCRIPT with no dependency on 02_fisheries.R
## having run first.
## =================================================================
ICCAT_DIR <- file.path(pcloud_dir, "data/fisheries/ICCAT")
iccat_biomass_path <- file.path(ICCAT_DIR, "iccat_ssb_biomass.csv")

## ICCAT_SPECIES: externalized to reference_tables/iccat_species.csv
## (read via 01_biomass.R's read_medits_reference()/MEDITS_REFERENCE_DIR).
ICCAT_SPECIES <- read_medits_reference("iccat_species.csv", required_cols = c("iccat_code", "ScientificName"))
ICCAT_BIOMASS_COL_ALIASES <- list(
  year         = c("Yearc", "YearC", "Year", "year"),
  species      = c("Species", "SpeciesCode", "sp_code"),
  species_name = c("SpeciesName", "SpName", "CommonName", "Stock"),
  biomass_t    = c("SSB_t", "SSB", "Biomass_t", "Biomass", "TotalBiomass_t"),
  ## iccat_ssb_biomass.csv carries a real per-row Source_citation (see
  ## the GBYP/JABBA citations in the comment block above, for BFT/SWO).
  ## Optional (NA if not found), same tolerant lookup as
  ## load_manual_cited_biomass_group()'s col_src.
  source       = c("Source_citation", "Source", "Citation", "Reference")
)
resolve_iccat_col <- function(dt_names, aliases) {
  hit <- intersect(aliases, dt_names)
  if (length(hit) == 0) NA_character_ else hit[1]
}

iccat_biomass_fg_year <- data.table(FG_num = integer(0), FG_name = character(0), Year = integer(0),
                                    iccat_biomass_t = numeric(0), iccat_sources = character(0),
                                    stock_assessment_density_t_km2 = numeric(0))
if (!file.exists(iccat_biomass_path)) {
  message("\n[ICCAT biomass] '", iccat_biomass_path, "' not found - skipping (build a small per-stock SSB",
          " table from the relevant ICCAT stock assessment - bluefin tuna/swordfish/albacore - and place it",
          " at iccat_biomass_path if you want ICCAT's own assessed biomass available for the priority rule",
          " below; it takes priority over GFCM STAR/RAM Legacy wherever both cover the same FG x Year cell).")
} else {
  iccat_bio_raw <- fread(iccat_biomass_path, encoding = "UTF-8")
  col_year <- resolve_iccat_col(names(iccat_bio_raw), ICCAT_BIOMASS_COL_ALIASES$year)
  col_bio  <- resolve_iccat_col(names(iccat_bio_raw), ICCAT_BIOMASS_COL_ALIASES$biomass_t)
  col_sp   <- resolve_iccat_col(names(iccat_bio_raw), ICCAT_BIOMASS_COL_ALIASES$species)
  col_spn  <- resolve_iccat_col(names(iccat_bio_raw), ICCAT_BIOMASS_COL_ALIASES$species_name)
  if (is.na(col_year) || is.na(col_bio) || (is.na(col_sp) && is.na(col_spn))) {
    stop("[ICCAT biomass] '", iccat_biomass_path, "' is missing required column(s) - found columns: ",
         paste(names(iccat_bio_raw), collapse = ", "), ". Needed: a year column (tried ",
         paste(ICCAT_BIOMASS_COL_ALIASES$year, collapse = "/"), "), a biomass column (tried ",
         paste(ICCAT_BIOMASS_COL_ALIASES$biomass_t, collapse = "/"), "), and EITHER a species-code column",
         " (tried ", paste(ICCAT_BIOMASS_COL_ALIASES$species, collapse = "/"), ") OR a species-name/stock",
         " column (tried ", paste(ICCAT_BIOMASS_COL_ALIASES$species_name, collapse = "/"), "). Rename the",
         " real column(s) to match one of these, or add the actual header spelling to",
         " ICCAT_BIOMASS_COL_ALIASES above.")
  }
  setnames(iccat_bio_raw, col_year, "Year")
  setnames(iccat_bio_raw, col_bio, "iccat_biomass_t")
  iccat_bio_raw[, `:=`(Year = as.integer(Year), iccat_biomass_t = as.numeric(iccat_biomass_t))]
  
  col_src <- resolve_iccat_col(names(iccat_bio_raw), ICCAT_BIOMASS_COL_ALIASES$source)
  if (!is.na(col_src)) {
    setnames(iccat_bio_raw, col_src, "Source_citation")
  } else {
    iccat_bio_raw[, Source_citation := NA_character_]
    message("[ICCAT biomass] '", iccat_biomass_path, "' has no Source_citation/Source/Citation/Reference",
            " column - FG_References' B_ref will fall back to the generic 'ICCAT stock assessment (SSB)'",
            " label for these FG(s) rather than the real per-stock citation. Add that column (see the",
            " GBYP aerial-survey/JABBA citations in the code comments above for what it should contain)",
            " to get the real reference through.")
  }
  
  if (!is.na(col_sp)) {
    setnames(iccat_bio_raw, col_sp, "iccat_code")
    iccat_bio_raw <- merge(iccat_bio_raw, ICCAT_SPECIES, by = "iccat_code")
  } else {
    setnames(iccat_bio_raw, col_spn, "iccat_species_name")
    ## externalized to reference_tables/iccat_species_name_match.csv
    iccat_bio_name_match <- read_medits_reference("iccat_species_name_match.csv",
                                                  required_cols = c("iccat_species_name", "ScientificName"))
    iccat_bio_raw <- merge(iccat_bio_raw, iccat_bio_name_match, by = "iccat_species_name")
  }
  
  iccat_bio_matched <- merge(unique(iccat_bio_raw[, .(ScientificName)]),
                             fg_lookup_safe[, .(ScientificName, FG_num, FG_name)], by = "ScientificName")
  iccat_bio_raw <- merge(iccat_bio_raw, iccat_bio_matched, by = "ScientificName")
  
  ## ---------------------------------------------------------------
  ## Spatial allocation: ICCAT does NOT assess a "Western Mediterranean"
  ## stock for any of these three species - it assesses Bluefin tuna as
  ## ONE Eastern Atlantic + Mediterranean stock, and Swordfish/Albacore
  ## as their own whole-Mediterranean stocks. Dividing biomass_t by
  ## sum(strata_area_by_area$area_km2) (the West Med survey-strata area
  ## alone) would silently inflate density for a stock whose real range
  ## is much bigger than the West Med. Instead each row is divided by
  ## the STOCK'S OWN assessed range area - an explicit "uniform density
  ## across the whole assessed range" assumption, documented and
  ## bounded rather than a silent multiplier error - wherever that
  ## assumption is defensible.
  ##
  ## Swordfish/Albacore: Mediterranean-only stock, so "uniform density
  ## across the whole Mediterranean" is the least-bad assumption
  ## available with no finer-grained spatial data in hand. Mediterranean
  ## Sea total surface area ~2,510,000 km^2 (standard oceanographic
  ## figure, e.g. Bethoux 1979-style Mediterranean physical geography
  ## references) is used as the stock's range.
  ##
  ## Swordfish: ICCAT's own SS assessment reports SSB only as a B/Bmsy
  ## ratio, not a tonnage - but the 2020 Mediterranean swordfish
  ## assessment's JABBA (surplus-production) model gives an absolute
  ## Bmsy: joint posterior median 71,319 t (67,509-73,928 t across model
  ## variants), with B2018/Bmsy = 0.72 for the terminal year -> B2018 =
  ## 0.72 x 71,319 = 51,350 t (whole Mediterranean stock, one year only -
  ## no public year-by-year SSB table exists, so this is used as the
  ## single closest-available point to the 1994-1996 baseline, same
  ## "closest available, flagged not guessed" convention as every other
  ## manual-cited source in this pipeline). See iccat_ssb_biomass.csv's
  ## SWO row for the full citation. Albacore (ALB) has no equivalent
  ## figure sourced yet - the mechanism is ready, the number isn't found.
  ##
  ## Bluefin tuna: area-ratio allocation across the whole Eastern
  ## Atlantic+Mediterranean stock range is NOT used - the fish aren't
  ## spread evenly over that huge range, so any area ratio would be
  ## invented, not sourced. Using GBYP's own aerial-survey biomass
  ## density instead - a real, direct, regionally-specific measurement
  ## of the Balearic Sea spawning aggregation ("A-core" survey block),
  ## not a back-calculation from the whole-stock SSB at all:
  ##   2017: 130.54 kg/km2, 2018: 217.84 kg/km2, 2019: 188.38 kg/km2,
  ##   2021: 76.27 kg/km2 (CREEM's own statistical analysis of ICCAT's
  ##   GBYP Phase 11 aerial survey, A-core area, Balearic Sea -
  ##   https://iccat.int/GBYP/DOCS/Aerial_Survey_Phase_11_CREEM_2021_Data_Analysis.pdf)
  ##   -> 4-year average 153.26 kg/km2 = 0.15326 t/km2, used below as
  ##   iccat_ssb_biomass.csv's BFT row (see that file's Source_citation).
  ## Caveats, same "closest available, flagged" spirit as every other
  ## site-specific figure in this pipeline (matches how Posidonia/
  ## gorgonian density is sourced from a single site and applied
  ## domain-wide - see claude/benthic_habitat_megafauna_biomass_sourcing_guide.md):
  ##   (a) SPAWNING-SEASON snapshot (survey flown during the June
  ##       spawning aggregation), not a year-round average - likely
  ##       overstates the annual mean if the fish disperse to the
  ##       Atlantic for much of the rest of the year;
  ##   (b) density WITHIN the core aggregation block itself, applied
  ##       here as the FG's domain-wide West Med average - same
  ##       simplification already used for gorgonians/Posidonia;
  ##   (c) 2017-2021 data used as a stand-in for 1995 (no aerial survey
  ##       existed then - GBYP itself only started ~2010).
  ## Because this is already a density, not a whole-stock tonnage/area
  ## division like Swordfish/Albacore, the mechanism is reused by
  ## setting stock_area_km2 = 1 for BFT - iccat_ssb_biomass.csv's BFT
  ## Biomass_t column is therefore expected to already BE the density in
  ## t/km2 (dividing by 1 is a no-op), not a real tonnage figure - flagged
  ## here so this isn't misread as an actual whole-stock biomass number.
  ## Externalized to reference_tables/iccat_stock_area_km2.csv.
  ICCAT_STOCK_AREA_KM2 <- read_medits_reference(
    "iccat_stock_area_km2.csv",
    required_cols = c("iccat_code", "stock_area_km2", "uniform_density_valid", "area_note")
  )
  ICCAT_STOCK_AREA_KM2[, uniform_density_valid := as.logical(uniform_density_valid)]
  if ("iccat_code" %in% names(iccat_bio_raw)) iccat_bio_raw[, iccat_code := NULL]  # re-derive fresh below regardless of which branch (col_sp vs col_spn) ran above
  iccat_bio_raw <- merge(iccat_bio_raw, ICCAT_SPECIES, by = "ScientificName")
  iccat_bio_raw <- merge(iccat_bio_raw, ICCAT_STOCK_AREA_KM2, by = "iccat_code", all.x = TRUE)
  iccat_bio_raw[, row_density_t_km2 := fifelse(uniform_density_valid, iccat_biomass_t / stock_area_km2, NA_real_)]
  for (note_code in unique(iccat_bio_raw[uniform_density_valid == FALSE, iccat_code])) {
    message("[ICCAT biomass] '", note_code, "': ", ICCAT_STOCK_AREA_KM2[iccat_code == note_code, area_note],
            " - biomass recorded, density left NA (excluded from the FG priority rule until fixed).")
  }
  
  n_iccat_bio_unmatched <- uniqueN(ICCAT_SPECIES$ScientificName) - uniqueN(iccat_bio_matched$ScientificName)
  if (n_iccat_bio_unmatched > 0) {
    unmatched_sp <- setdiff(ICCAT_SPECIES$ScientificName, iccat_bio_matched$ScientificName)
    message("[ICCAT biomass] ", n_iccat_bio_unmatched, " of the ", uniqueN(ICCAT_SPECIES$ScientificName),
            " requested ICCAT species have no matching FG in the current FG_WMed_2026.csv, so their ICCAT",
            " biomass rows loaded but couldn't attach anywhere: ", paste(unmatched_sp, collapse = ", "),
            ". Add the species to FG_WMed_2026.csv as its own FG (same convention as Bluefin tuna/FG12,",
            " Swordfish/FG13) if you want it picked up here - this loader deliberately does not invent an",
            " FG for it.")
  }
  
  iccat_biomass_fg_year <- iccat_bio_raw[, .(
    iccat_biomass_t = sum(iccat_biomass_t, na.rm = TRUE),
    ## same stock -> same stock_area_km2/uniform_density_valid for every row being summed here (one
    ## species per FG), so summing row_density_t_km2 * iccat_biomass_t and dividing back out is
    ## equivalent to biomass_t / stock_area_km2 even if a stock ever had >1 row per FG x Year
    stock_assessment_density_t_km2 = fifelse(all(uniform_density_valid), sum(iccat_biomass_t, na.rm = TRUE) / stock_area_km2[1], NA_real_),
    ## Real per-stock citation where iccat_ssb_biomass.csv has one
    ## (Source_citation, resolved above), falling back to the old
    ## generic label only for a row that genuinely has none.
    iccat_sources   = {
      real_cites <- sort(unique(na.omit(Source_citation)))
      if (length(real_cites) > 0) paste0("ICCAT stock assessment (SSB): ", paste(real_cites, collapse = " | "))
      else "ICCAT stock assessment (SSB) - no Source_citation on file for this row"
    }
  ), by = .(FG_num, FG_name, Year, iccat_code)]
  iccat_biomass_fg_year <- iccat_biomass_fg_year[iccat_biomass_t > 0]
  
  ## --- temporal-baseline mismatch flag ---------------------------
  ## BFT's density comes from a 2017-2021 GBYP aerial survey (see the
  ## code comment above this ICCAT block) and SWO's from a 2018 JABBA
  ## model estimate - both real measurements, but for the wrong year,
  ## applied here to a 1994-1996 baseline as-is. The DIRECTION of the
  ## bias is documented for both stocks (see ICCAT_TEMPORAL_BASELINE_NOTE
  ## below for the full citations and reasoning), but no source gives a
  ## citable 1994-1996 vs. modern-year numeric ratio (ICCAT's own
  ## SSB-by-year series is graphical/appendix-only in the reports
  ## checked) - so no numeric correction is applied (that would be
  ## guessing a number, which this pipeline deliberately never does -
  ## see the "mechanism is ready, number isn't found" convention used
  ## elsewhere, e.g. Albacore). The direction and likely bias are
  ## flagged prominently in stock_assessment_source/biomass_source (so
  ## it reaches B_ref) rather than left as a silent mismatch.
  ICCAT_TEMPORAL_BASELINE_NOTE <- list(
    BFT = paste0(
      "TEMPORAL MISMATCH: this density is from a 2017-2021 GBYP aerial survey, applied as-is to the ",
      "1994-1996 baseline. ICCAT SCRS 2022 BFT-E assessment (ASAP model, Collect. Vol. Sci. Pap. ICCAT ",
      "79(3), SCRS/2022/013): SSB declined from the 1970s to a trough ~2007, then rose steadily 2010-2020 ",
      "to the highest level since the 1960s - so 1995 SSB was LOWER than 2017-2021. No citable numeric ",
      "ratio found in the SCRS report tables checked (SSB-by-year is graphical/appendix-only there) - NOT ",
      "numerically adjusted. Likely OVERESTIMATES 1995 biomass - review before trusting as a baseline value."
    ),
    SWO = paste0(
      "TEMPORAL MISMATCH: this density is from a 2018 JABBA model estimate, applied as-is to the ",
      "1994-1996 baseline. ICCAT SCRS 2020 SWO-MED assessment (JABBA Bayesian state-space model): stock ",
      "was already overfished by the mid-1990s (sharp 1950-1970 decline), with only modest further decline ",
      "1996-2010 before accelerating decline into 2018 - so 1995 biomass was likely HIGHER than the 2018 ",
      "estimate used here. No citable numeric ratio found - NOT numerically adjusted. Likely UNDERESTIMATES ",
      "1995 biomass, probably by a smaller margin than BFT's overestimate - review before trusting."
    )
  )
  for (code in names(ICCAT_TEMPORAL_BASELINE_NOTE)) {
    flagged_rows <- iccat_biomass_fg_year$iccat_code == code
    if (any(flagged_rows)) {
      iccat_biomass_fg_year[flagged_rows, iccat_sources := paste0(iccat_sources, " -- ", ICCAT_TEMPORAL_BASELINE_NOTE[[code]])]
    }
  }
  temporal_mismatch_review <- iccat_biomass_fg_year[iccat_code %in% names(ICCAT_TEMPORAL_BASELINE_NOTE),
                                                    .(FG_num, FG_name, iccat_code, Year, stock_assessment_density_t_km2,
                                                      ## `[` on a LIST returns a sub-list, which becomes a
                                                      ## list-column here (fwrite() mishandles those) -
                                                      ## unlist() to a plain character vector instead.
                                                      temporal_note = unlist(ICCAT_TEMPORAL_BASELINE_NOTE[iccat_code], use.names = FALSE))]
  ## The temporal-mismatch note is already written directly into
  ## iccat_sources above (see the ICCAT_TEMPORAL_BASELINE_NOTE loop), so
  ## no separate review CSV is written; temporal_mismatch_review itself
  ## is harmless to keep computing (nothing downstream reads it).
  
  fwrite(iccat_biomass_fg_year, file.path(csv_out_dir, "iccat_biomass_by_fg.csv"))
  message("[ICCAT biomass] iccat_biomass_fg_year: ", nrow(iccat_biomass_fg_year), " FG x Year row(s) (",
          uniqueN(iccat_biomass_fg_year$FG_num), " FG(s)) - written to iccat_biomass_by_fg.csv.")
}

## Merge ICCAT into stock_assessment_fg_year, ICCAT winning on overlap
## (an anti-join drops any STAR/RAM row for a FG x Year cell ICCAT
## also covers, before appending ICCAT's own rows for every cell it
## covers, overlapping or not).
if (nrow(iccat_biomass_fg_year) > 0) {
  overlap_cells <- fintersect(stock_assessment_fg_year[, .(FG_num, Year)], iccat_biomass_fg_year[, .(FG_num, Year)])
  if (nrow(overlap_cells) > 0) {
    message("[Stock-assessment biomass] ICCAT overrides STAR/RAM Legacy for ", nrow(overlap_cells),
            " FG x Year cell(s) both sources cover for the same FG (ICCAT takes priority).")
    stock_assessment_fg_year <- stock_assessment_fg_year[!overlap_cells, on = c("FG_num", "Year")]
  }
  stock_assessment_fg_year <- rbindlist(list(
    stock_assessment_fg_year,
    iccat_biomass_fg_year[, .(FG_num, FG_name, Year, stock_assessment_density_t_km2,
                              star_biomass_t = iccat_biomass_t, star_n_stocks = NA_integer_,
                              star_sources = iccat_sources, stock_assessment_source = iccat_sources)]
  ), use.names = TRUE, fill = TRUE)
  message("[Stock-assessment biomass] stock_assessment_fg_year now combines STAR/RAM Legacy and ICCAT: ",
          nrow(stock_assessment_fg_year), " FG x Year row(s) total (", uniqueN(stock_assessment_fg_year$FG_num),
          " FG(s)) - ICCAT wins on any FG x Year overlap.")
}


## =================================================================
## Marine megafauna BIOMASS - cetaceans, seabirds, sea turtles.
## Unlike bluefin tuna/swordfish above, there is no RFMO-style bulk
## catch/stock-assessment database for any of these three groups:
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
## These groups don't need a real time series - the minimum is a
## single baseline biomass figure for the Ecopath base year
## (1994-1996). load_manual_cited_biomass_group() below accepts that
## as-is: a CSV with just one Year value (or one row per year of
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
## Two extensions to the manual-cited-CSV mechanism above, both driven
## by the same underlying idea - a literature-sourced FG total should
## be built from its real taxonomic composition, not one lumped guess:
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
##
## Added 2026-10-04 - two more specific factors, for the benthic
## habitat FGs (Posidonia/Cymodocea/macroalgae, gorgonians/
## coralligenous fauna) this generic 4.5 macrobenthos default was
## never meant for:
##  - "g_afdm_m2_cnidaria" = 6.99 - converts AFDM (ash-free dry mass)
##    of CNIDARIAN SOFT TISSUE to wet weight, for gorgonians/
##    coralligenous fauna densities like Ambroso et al. 2019's Cap de
##    Creus figures (benthic_habitat_megafauna_biomass_sourcing_guide.md).
##    No gorgonian/octocoral-specific AFDM:WW factor exists in the
##    literature (checked directly against Ricciardi & Bourget 1998,
##    Mar Ecol Prog Ser 163:245-251 - the same paper this script's
##    generic 4.5 already comes from, which gives per-taxon factors,
##    not one macrobenthos-wide number). Their closest cnidarian
##    analog, Actiniaria (sea anemones), gives AFDW = 14.3% of WW
##    (range 6.0-22.6%), i.e. WW = AFDM / 0.143 = AFDM x 6.99 - used
##    here as the best available substitute, NOT a gorgonian-specific
##    measurement. IMPORTANT CAVEAT: this converts SOFT TISSUE ONLY -
##    it does NOT add the gorgonian's calcified/proteinaceous axis
##    (the bulk of a colony's real physical mass), since AFDM
##    methodology burns off organics and excludes it entirely. Whether
##    this FG's Ecopath biomass should mean "living tissue only" (the
##    trophically-active fraction, consistent with how many coral-reef
##    Ecopath models treat calcified groups) or "whole colony including
##    skeleton" is a modeling-convention decision, not something this
##    factor resolves on its own - confirm against your protocol's
##    treatment of other calcified/shelled FGs before trusting the
##    resulting absolute biomass number.
##  - "g_dw_m2_macrophyte" = 7 - converts seagrass/macroalgae DRY
##    WEIGHT to wet weight (Posidonia, Cymodocea, macroalgae). NOT a
##    single pinned citation - no Mediterranean-specific seagrass
##    DW:WW study was found with the actual ratio disclosed (several
##    papers were checked directly: Bernardeau-Esteller et al. 2023,
##    the MDPI Water 2025 P. oceanica leaf-biomass paper, and a
##    ScienceDirect NE-Pacific seagrass/macroalgae wet-dry calibration
##    study - none publish the actual ratio in an openly accessible
##    form). This is a general-marine-macrophyte-physiology estimate
##    (leaf/thallus tissue is commonly ~85-90% water by fresh weight,
##    consistent with Posidonia %C/%N-by-dry-weight literature), kept
##    as the midpoint of the 1:5-1:10 range this guide previously
##    carried as an unconfirmed placeholder - narrower, but still not
##    a specific-study citation. Replace with a real one if you find
##    it.
## Externalized to reference_tables/density_unit_to_wet_g_m2.csv.
.density_unit_tbl <- read_medits_reference("density_unit_to_wet_g_m2.csv", required_cols = c("unit", "factor"))
DENSITY_UNIT_TO_WET_G_M2 <- setNames(.density_unit_tbl$factor, .density_unit_tbl$unit)

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
  ## A depth-band area is a poor stand-in for a genuinely PATCHY
  ## habitat - Posidonia/macroalgae/coralligenous fauna don't
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
                                            fallback_message, write_review_outputs = TRUE) {
  ## write_review_outputs = FALSE (these CSVs aren't used for the
  ## primary-producer/plankton call) skips the three fwrite()s below
  ## (the EcoBase-excluded-rows REVIEW,
  ## the unmatched-group REVIEW, and the main FG x Year output CSV) -
  ## everything is still COMPUTED and returned in-memory as normal
  ## (merge_manual_cited_biomass() downstream needs `out` regardless of
  ## whether it was also written to disk), only the disk writes are
  ## suppressed. Defaults to TRUE so megafauna/benthic-habitat keep
  ## writing their CSVs exactly as before.
  col_aliases <- list(
    year      = c("Year", "year", "Yearc"),
    group     = c("Group", "Species", "FG_name", "Taxon", "CommonName"),
    biomass_t = c("Biomass_t", "Biomass", "Population_t", "Total_t"),
    source    = c("Source_citation", "Source", "Citation", "Reference"),
    ## `Year` above is the TARGET baseline year this row is applied to
    ## (usually forced to 1994-1996 so it lands in the Ecopath baseline
    ## window via the FG_num/Year merge below) - not necessarily when
    ## the underlying measurement/survey was actually taken. This
    ## optional column lets a source CSV say when the real measurement
    ## is from, distinct from the target Year, so a genuine temporal
    ## mismatch (a 2018 ACCOBAMS survey applied to 1995, say) can be
    ## flagged automatically instead of only being caught by hand (as
    ## done for the ICCAT BFT/SWO case above). Every row without it is
    ## treated as already contemporaneous with Year.
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
  ## A biomass column is required only when the file has no density
  ## column either - a density-only file (Density_value/Density_unit +
  ## Habitat_area_km2 or depth range) is a valid input: its rows get
  ## group_biomass_t from extrapolate_density_row() further down.
  col_dens_present <- resolve_iccat_col(names(raw), c("Density_value", "Density", "Density_t_km2"))
  if (any(is.na(c(col_year, col_grp))) || (is.na(col_bio) && is.na(col_dens_present))) {
    message("\n[", label, "] '", csv_path, "' is missing a required column - found: ",
            paste(names(raw), collapse = ", "), ". Needed a year column (tried ",
            paste(col_aliases$year, collapse = "/"), "), a biomass column (tried ",
            paste(col_aliases$biomass_t, collapse = "/"), ") OR a density column (Density_value),",
            " and a group/species column (tried ",
            paste(col_aliases$group, collapse = "/"), ") - skipping rather than guessing.")
    return(out)
  }
  setnames(raw, col_year, "Year")
  if (!is.na(col_bio)) {
    setnames(raw, col_bio, "group_biomass_t")
  } else {
    message("[", label, "] No Biomass_t column - every row must supply Density_value; biomass is",
            " computed from density x habitat area below.")
    raw[, group_biomass_t := NA_real_]
  }
  setnames(raw, col_grp, "Group")
  if (!is.na(col_src)) setnames(raw, col_src, "Source_citation") else raw[, Source_citation := NA_character_]
  raw[, `:=`(Year = as.integer(Year), group_biomass_t = as.numeric(group_biomass_t))]
  
  ## An optional Species column, distinct from Group (Group can still be
  ## an FG-level keyword like "OtherDolphins" or "PelagicSeabirds" -
  ## Species, when present, names the actual species that row's
  ## Biomass_t is for). Species-level rows
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
      fwrite(review_rows, file.path(csv_out_dir, paste0("temporal_mismatch_REVIEW_", gsub("[^A-Za-z0-9]+", "_", label), ".csv")))
      message("[", label, "] ", nrow(review_rows), " Group(s) flagged for a temporal mismatch (Collection_year",
              " != the ", year_ecopath_mid, " Ecopath baseline) - see temporal_mismatch_REVIEW_", label,
              ".csv and Source_citation. NOT numerically corrected - only flagged.")
    }
  }
  
  ## --- optional density -> habitat-area extrapolation --------------------
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
  ## Optional REAL habitat-extent override (e.g. a habitat
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
    ## Optional Occupancy_fraction: share of the MAPPED habitat polygon
    ## actually occupied at the stated density (e.g. Guidetti et al. 2002
    ## convention for seagrass meadows). Applied to Habitat_area_km2 only -
    ## kept as its own column so the mapped area and the occupancy
    ## assumption stay separately visible and separately adjustable.
    ## FLAGGED ASSUMPTION: occupancy is a model assumption, not a
    ## measurement; a blank value means 1 (polygon fully occupied).
    col_occ <- resolve_iccat_col(names(raw), c("Occupancy_fraction", "occupancy_fraction", "Occupancy"))
    if (!is.na(col_occ)) {
      setnames(raw, col_occ, "Occupancy_fraction")
      raw[, Occupancy_fraction := as.numeric(Occupancy_fraction)]
      bad_occ <- raw[!is.na(Occupancy_fraction) & (Occupancy_fraction <= 0 | Occupancy_fraction > 1)]
      if (nrow(bad_occ) > 0) stop("[", label, "] Occupancy_fraction must be in (0, 1] - offending Group(s): ",
                                  paste(unique(bad_occ$Group), collapse = ", "))
      has_occ <- !is.na(raw$Occupancy_fraction) & !is.na(raw$Habitat_area_km2)
      if (any(has_occ)) {
        message("[", label, "] Occupancy_fraction applied to Habitat_area_km2 for: ",
                paste(paste0(raw$Group[has_occ], " (", raw$Occupancy_fraction[has_occ], ")"), collapse = ", "))
        raw[has_occ, Habitat_area_km2 := Habitat_area_km2 * Occupancy_fraction]
      }
      no_area_occ <- !is.na(raw$Occupancy_fraction) & is.na(raw$Habitat_area_km2)
      if (any(no_area_occ)) message("[", label, "] Occupancy_fraction given but no Habitat_area_km2 for: ",
                                    paste(unique(raw$Group[no_area_occ]), collapse = ", "),
                                    " - occupancy NOT applied (depth-band area is used as-is for these rows).")
    }
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
    if (write_review_outputs) {
      fwrite(ecobase_rows, file.path(csv_out_dir, paste0(gsub("[^A-Za-z0-9]+", "_", label), "_ecobase_sourced_rows_EXCLUDED_REVIEW.csv")))
    }
    message("[", label, "] ", nrow(ecobase_rows), " row(s) in ", csv_path, " carry an EcoBase-derived",
            " Source_citation - EXCLUDED from Ecopath_B (EcoBase is reference-only, never incorporated -",
            " see the *_NOT_INCORPORATED.csv evaluation files instead). Written to a REVIEW csv for you",
            " to replace with a real cited source by hand: ", paste(ecobase_rows$Group, collapse = ", "), ".")
    raw <- raw[!is_ecobase_sourced]
  }
  
  ## Full FG catalog isn't built until later in this script (full_fg_list,
  ## from dataframe2) - fg_lookup_safe is already in scope this far up
  ## (built at line ~637) and carries the same FG_num/FG_name universe.
  ## dataframe2 (from 01_biomass.R) carries EVERY FG incl. non-taxon ones
  ## like Detritus (FG 79, species 'Shell drebis') that fg_lookup_safe
  ## drops; union both so keyword matching sees the full FG list.
  full_fg_catalog <- unique(rbindlist(list(
    if (exists("dataframe2")) unique(as.data.table(dataframe2)[, .(FG_num = as.integer(FG_num), FG_name)]) else NULL,
    unique(fg_lookup_safe[, .(FG_num = as.integer(FG_num), FG_name)])), use.names = TRUE))
  full_fg_catalog <- full_fg_catalog[!is.na(FG_num) & !is.na(FG_name)]
  group_word_sets <- lapply(taxon_keywords, tolower)
  fg_word_hits <- rbindlist(lapply(names(group_word_sets), function(g) {
    hits <- full_fg_catalog[sapply(tolower(FG_name), function(nm) any(sapply(group_word_sets[[g]], function(kw) grepl(kw, nm, fixed = TRUE))))]
    if (nrow(hits) == 0) return(NULL)
    data.table(Group = g, FG_num = hits$FG_num, FG_name = hits$FG_name)
  }))
  if (is.null(fg_word_hits) || nrow(fg_word_hits) == 0) fg_word_hits <- data.table(Group = character(), FG_num = integer(), FG_name = character())
  
  unmatched_groups_dt <- unique(raw[!Group %in% unique(fg_word_hits$Group), .(Group)])
  if (nrow(unmatched_groups_dt) > 0) {
    if (write_review_outputs) {
      fwrite(unmatched_groups_dt, file.path(csv_out_dir, review_csv_name))
    }
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
    if (write_review_outputs) {
      fwrite(out, file.path(csv_out_dir, output_csv_name))
      message("[", label, "] ", nrow(out), " FG x Year row(s) (", uniqueN(out$FG_num), " FG(s)) - written to ", output_csv_name, ".")
    } else {
      message("[", label, "] ", nrow(out), " FG x Year row(s) (", uniqueN(out$FG_num), " FG(s)) computed - not written",
              " to disk (", output_csv_name, " suppressed per Andrea's review; still used in-memory downstream).")
    }
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
## Lives under data/Complementary data/, alongside the other
## manual/cited reference files there (matching where
## westmed_posidonia_coralligenous.shp and similar hand-curated inputs
## already live).
MEGAFAUNA_BIOMASS_PATH <- file.path(pcloud_dir, "data/Complementary data/marine_megafauna_biomass.csv")
## "Pinnipeds" is its own keyword group - Monk seals (FG6) match
## NEITHER Cetaceans/Seabirds/SeaTurtles otherwise (no "seal" keyword
## anywhere), so a manual CSV would load fine but Monk seals
## specifically could never receive a value regardless of the CSV - a
## real gap, not just a missing CSV.
##
## Real, species-specific ACCOBAMS Survey Initiative density figures
## exist for bottlenose dolphins, "other dolphins" (striped dolphin
## proxy), and fin whale (see marine_megafauna_biomass.csv below) -
## but the broad "Cetaceans" bucket matches ALL FIVE cetacean FGs by
## keyword (every one contains "dolphin" or "whale"), so a
## Group="Cetaceans" row would get its biomass split evenly across all
## five regardless of their real relative abundance, throwing away
## exactly the species-specificity these figures provide. Five
## FG-specific categories below let a CSV row target exactly one FG;
## each keyword is checked to match ONLY its intended FG_name and no
## other (verified against the real FG_WMed_2026.csv text -
## "bottlenose" only appears in "Bottlenose dolphins", "other dolphin"
## only in "Other dolphins", etc.). "Cetaceans" is kept as a fallback
## bucket for a figure genuinely only available at the whole-guild
## level (e.g. a total abundance survey that doesn't break out
## species) - splitting evenly across all 5 FGs is correct there.
##
## Same bug applied to seabirds: the broad "Seabirds" keyword list
## (kept as a genuinely-undifferentiated-figure fallback) matches BOTH
## "Pelagic/Offshore seabirds" (FG7) and "Coastal/inshore seabirds"
## (FG8), so a Group="Seabirds" row would split evenly across both FGs
## regardless of which one a real count describes. PelagicSeabirds/
## CoastalSeabirds are their own keyword categories below, each
## checked to match ONLY its intended FG_name text ("pelagic/offshore
## seabird" only in FG7, "coastal/inshore seabird" only in FG8).
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
                            " Preferred real sources: AERIAL SURVEY density/abundance for cetaceans -",
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
## Pull this from EcoBase (existing published Ecopath models covering
## the Mediterranean) rather than hand-copying numbers out of a paper -
## EcoBase already has these exact groups as Biomass inputs in OTHER
## models' own Ecopath parameterizations, and is therefore the
## fastest, most directly comparable (same B t/km2 EwE unit) source,
## on top of whatever satellite/other-source estimates are closest to
## 1994-1996.
##
## fetch_ecobase_literature_biomass() (03b_ecobase.R) queries EcoBase,
## keyword-matches every returned group_name against the 7 target
## groups below, and for each one picks whichever candidate model's
## year is CLOSEST to 1995 (midpoint of YEAR_ECOPATH) - returned
## in-memory with the source model/year/authors and how far that year
## actually is from 1995 (not written to a CSV - see 03b_ecobase.R's
## own header), so "closest available, not a real 1994-1996
## measurement" stays explicit. If
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
## Benthic/seagrass/coralligenous groups (Posidonia, Cymodocea,
## Macroalgae, Suprabenthos, GorgoniansCorals, BenthicMollusc_*/
## Macrobenthos_* subgroups) get their own file, separate from
## PRIMARY_PRODUCER_BIOMASS_PATH's plankton-only scope - these are the
## groups whose real West Med extent (where available) comes from an
## EMODnet/EUSeaMap habitat shapefile, clipped to the study domain and
## fed into extrapolate_density_row() via Habitat_area_km2, not a
## CMEMS/EcoBase biogeochemical-model query.
## Canonical copy lives in benthic_habitat_reference_tables/ (alongside
## density_unit_to_wet_g_m2.csv and benthic_habitat_density_reference.csv).
## The older copy directly under Complementary data/ was renamed
## *_SUPERSEDED_2026-09-30.csv - it lacked the shelf Suprabenthos row.
## Two input files, one per source type (see the SOURCE ROADMAP at the top):
##  - LITERATURE_DENSITY_BIOMASS_PATH: literature density x depth band
##    (Depth_min_m/Depth_max_m) - benthic molluscs, other macrobenthos,
##    soft-bottom octocorals (sea pens etc.), suprabenthos.
##  - HABITAT_SHAPEFILE_BIOMASS_PATH: literature density x MAPPED habitat
##    area (Habitat_area_km2 from a shapefile) x Occupancy_fraction -
##    Posidonia, Cymodocea, macroalgae, coralligenous gorgonians.
## Both use the same columns and the same loader; their FG x Year totals
## are added together (e.g. FG 70 = coralligenous gorgonians from the
## shapefile + soft-bottom octocorals from the literature file).
BENTHIC_REF_DIR <- file.path(pcloud_dir, "data/Complementary data/benthic_habitat_reference_tables")
LITERATURE_DENSITY_BIOMASS_PATH <- file.path(BENTHIC_REF_DIR, "literature_density_biomass.csv")
HABITAT_SHAPEFILE_BIOMASS_PATH  <- file.path(BENTHIC_REF_DIR, "habitat_shapefile_biomass.csv")
BENTHIC_HABITAT_BIOMASS_PATH <- HABITAT_SHAPEFILE_BIOMASS_PATH  # kept: still referenced by the EcoBase-evaluation gate further down
## Full keyword catalog - used below to match the manual/auto-drafted
## CSV's Group column to an FG_name, REGARDLESS of which source (EcoBase
## or satellite) actually supplied each group's number.
## Suprabenthos, Cymodocea, and gelatinous zooplankton/jellyfish each
## get their own keyword category so an EcoBase query (or a manual/
## cited CSV row) can target each one specifically - EcoBase model
## group_names commonly use exactly these labels. Because
## PRIMARY_PRODUCER_BIOMASS_PATH already exists (the team's real,
## reviewed data/primary_producer_plankton_biomass.csv), the auto-draft
## block above is skipped this run ("existing file always wins - never
## overwritten") - these categories take effect once either (a) that
## file is deleted/renamed so the EcoBase auto-draft runs fresh, or (b)
## real cited rows for Suprabenthos/Cymodocea/GelatinousZooplankton are
## added to that CSV by hand, same as the megafauna file above. No
## citable Western-Med-specific standing-biomass figure has been found
## yet for any of these three (Suprabenthos: Corrales et al. 2015/
## South Catalan Sea Ecopath papers exist but are paywalled; Cymodocea
## nodosa: literature has LEAF/RHIZOME PRODUCTION rates, e.g. Pérez &
## Camp 1986 Mar Menor lagoon 160-427 g DW/m2/year, but no standing-
## biomass figure; gelatinous zooplankton/salps: Mediterranean-specific
## trawl-survey biomass papers found by title but not accessible
## full-text) - genuinely still open, not filled with a guess.
## "Jellyfish" (FG68) and "Other macro-benthos" (FG67) are NOT added
## here - both already receive a small non-NA MEDITS-survey-derived
## value in the current real output (confirmed against Daniel's
## actual ecopath_ecosim_inputs.xlsx, Ecopath_B sheet: Jellyfish =
## 0.000196 t/km2, Other macro-benthos = 0.003135 t/km2) - genuinely
## missing is Suprabenthos/Cymodocea/"Salps and other gelatinous
## zooplankton" (all = NA in that same real output), which is what these
## three new categories target. The tiny Jellyfish/macro-benthos MEDITS
## values are likely a real undersample (bottom trawls are known to
## catch gelatinous fauna poorly) rather than a bug - worth a literature
## cross-check later, but that is a "is this number too low" question,
## not a "this cell is empty" one, so left untouched here.
## Split into two keyword lists feeding two separate CSVs:
## PRIMARY_PRODUCER_TAXON_KEYWORDS covers ONLY the pelagic plankton
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
  GelatinousZooplankton = c("gelatinous zooplankton", "salp", "salpidae", "thaliacea", "jellyfish"),
  Detritus             = c("detritus")  # FG 79 - Pauly et al. 1993 equation, see DETRITUS BIOMASS section
)

BENTHIC_HABITAT_TAXON_KEYWORDS <- list(
  Posidonia            = c("posidonia", "seagrass"),
  Macroalgae           = c("macroalga", "macro-alga"),
  GorgoniansCorals     = c("gorgonian", "coral"),
  Suprabenthos         = c("suprabenthos", "supra-benthos", "supra benthos"),
  Cymodocea            = c("cymodocea"),
  
  ## "Benthic mollusc" and "Other macro-benthos" are NOT survey-exempt
  ## (MEDITS does sample them - see the real Ecopath_B values
  ## referenced near EXEMPT_FG_NAMES above, ~0.00314 and ~0.000196
  ## t/km2), but a bottom-trawl survey badly under-samples
  ## small/soft-bodied/burrowing benthos, so these two FGs should ALSO
  ## be built from literature, one keyword category PER REAL
  ## TAXONOMIC SUBGROUP (each pointing at the SAME FG) so load_manual_
  ## cited_biomass_group()'s existing sum-by-FG_num/Year aggregation adds
  ## them back up into one FG total - e.g. "Benthic mollusc" = Bivalvia +
  ## Gastropoda + Scaphopoda + Polyplacophora. Composition confirmed
  ## against Daniel's real FG_WMed_2026.csv: Benthic
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
  ## extrapolation extension above) drops straight in. Until then
  ## these categories simply find zero raw rows and do nothing -
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
## Phytoplankton comes from a Mediterranean BIOGEOCHEMICAL MODEL
## (Copernicus Marine's Med BGC Reanalysis,
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
## A group's keyword only enters this EcoBase evaluation query if ITS
## OWN file (PRIMARY_PRODUCER_BIOMASS_PATH for plankton,
## BENTHIC_HABITAT_BIOMASS_PATH for seagrass/macroalgae/coralligenous)
## doesn't exist yet - gating on either path alone would wrongly skip
## the other group's EcoBase evaluation whenever its file happened to
## exist.
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
    ## ecobase_biomass_evaluation_primary_producers_NOT_INCORPORATED.csv
    ## is not written - EcoBase is still queried/evaluated above
    ## (ecobase_best_lowtrophic/ecobase_draft) but not written to disk.
    message("\n[Primary producer/plankton biomass] No reviewed CSV exists yet for one or more of these group(s) -",
            " EcoBase evaluation found a candidate for ", nrow(ecobase_draft), " group(s) (",
            paste(ecobase_draft$Group, collapse = ", "), "), but it is NOT written into PRIMARY_PRODUCER_BIOMASS_PATH",
            " or BENTHIC_HABITAT_BIOMASS_PATH and does NOT feed Ecopath_B - these group(s) remain genuinely missing",
            " until you add a real literature/survey/biogeochemical-model row to the relevant file by hand.")
  }
}

## The CMEMS/satellite phytoplankton fetch is NOT gated on whether
## PRIMARY_PRODUCER_BIOMASS_PATH already exists as a whole - that would
## skip fetch_cmems_phytoplankton_biomass()/fetch_satellite_phytoplankton_
## biomass() entirely the moment the file exists on disk, even with
## zero Phytoplankton rows in it (same gap pattern as the documented
## Suprabenthos/Cymodocea/GelatinousZooplankton case above). Instead it
## runs whenever Large/SmallPhytoplankton rows are missing from
## whatever CSV exists right now (freshly EcoBase-drafted above, or the
## project's own existing file), and APPENDS the fetched row(s) to that
## file. Existing Phytoplankton rows still always win - this only fills
## a genuine gap, never overwrites.
## #################################################################
## SOURCE D - BIOGEOCHEMICAL MODELS / SATELLITE (plankton)
## #################################################################
## =================================================================
## PLANKTON BIOMASS (FG 71 Macro zooplankton, 72 Meso and micro
## zooplankton, 77 Large phytoplankton, 78 Small phytoplankton)
## Method from scripts/additional/zoo_phyto.R, downloading its own inputs
## with the copernicusmarine CLI (CLI/credential helpers come from
## lib_cmems_phytoplankton_biomass.R). Dataset ids verified in the CMEMS
## STAC catalogue (2026-10-05).
##
## Step 1 - Phytoplankton carbon (MAIN): MEDSEA_MULTIYEAR_BGC_006_008
##   (MedBFM Mediterranean reanalysis, 4.2 km, assimilates satellite
##   chlorophyll; Cossarini et al. 2021), annual-mean dataset
##   cmems_mod_med_bgc-plankton_my_4.2km_P1Y-m, variable phyc (mmol C m-3).
##   FLAGGED: 1999-2001 mean is a proxy for 1994-1996 (reanalysis starts 1999).
##   CROSS-CHECK only (logged, never used): GLOBAL_MULTIYEAR_BGC_001_029
##   (PISCES global hindcast) for 1995 itself.
## Step 2 - Mean per cell, integrated over 0-200 m (layer thickness from
##   depth-centre midpoints), x 0.012 g C/mmol -> g C m-2 (= t C km-2).
## Step 3 - Zooplankton carbon: GLOBAL_MULTIYEAR_BGC_001_033 (SEAPODYM
##   LMTL, 1/12 deg, daily, 1998 onward), dataset
##   cmems_mod_glo_bgc_my_0.083deg-lmtl_P1D-i, variable zooc (g C m-2,
##   already water-column integrated). FLAGGED: 1998-2000 mean is a
##   proxy for 1994-1996 (the product starts in 1998).
## Step 4 - Area-weighted (cos latitude) mean over ocean cells in the
##   West Med bbox (-6..16 E, 35..45 N), for both.
## Step 5 - Size split + carbon -> wet weight (all PROVISIONAL, values
##   as in zoo_phyto.R):
##   phytoplankton: small 0.60 / large 0.40; C:WW 0.16 (Yacobi & Zohary 2010)
##   zooplankton: meso+micro 0.70 / macro 0.30 (Fernandez de Puelles et al. 2014);
##     within meso+micro: micro 0.35 (C:WW 0.07, Fenchel & Finlay 1983),
##     meso 0.65 (C:WW 0.09, Kiorboe 2013); macro C:WW 0.09 (Kiorboe 2013)
## Step 6 - t WW km-2 x study area -> Biomass_t rows appended to
##   PRIMARY_PRODUCER_BIOMASS_PATH (only for groups with no non-EcoBase
##   row yet). Cached as csv_out_dir/cmems_plankton_biomass_by_fg.csv.
## =================================================================
PLANKTON_ALLOCATION <- list(
  small_phyto_fraction = 0.60, large_phyto_fraction = 0.40, phyto_C_per_WW = 0.16,
  meso_micro_fraction = 0.70, macro_zoo_fraction = 0.30,
  micro_within_meso_micro = 0.35, meso_within_meso_micro = 0.65,
  microzoo_C_per_WW = 0.07, mesozoo_C_per_WW = 0.09, macrozoo_C_per_WW = 0.09
)

.cmems_subset <- function(dataset_id, variable, t0, t1, bbox, depth_max = NULL, cred_args, timeout_sec) {
  nc_path <- tempfile(fileext = ".nc")
  args <- c("subset", "--dataset-id", dataset_id, "--variable", variable,
            "--start-datetime", t0, "--end-datetime", t1,
            "--minimum-longitude", bbox[["lon_min"]], "--maximum-longitude", bbox[["lon_max"]],
            "--minimum-latitude", bbox[["lat_min"]], "--maximum-latitude", bbox[["lat_max"]],
            if (!is.null(depth_max)) c("--minimum-depth", "0", "--maximum-depth", as.character(depth_max)),
            "--output-filename", basename(nc_path), "--output-directory", dirname(nc_path))  # login comes from COPERNICUSMARINE_SERVICE_* env vars, never the command line
  message("[CMEMS plankton] Downloading ", dataset_id, " / ", variable, " (", t0, " to ", t1, ")...")
  out <- suppressWarnings(system2("copernicusmarine", args, stdout = TRUE, stderr = TRUE, timeout = timeout_sec))
  ## Rejected login -> ask THIS user for their own account once, retry once.
  if (!file.exists(nc_path) && any(grepl("Invalid username or password", out, ignore.case = TRUE))) {
    ensure_copernicusmarine_credentials(force_prompt = TRUE)
    out <- suppressWarnings(system2("copernicusmarine", args, stdout = TRUE, stderr = TRUE, timeout = timeout_sec))
  }
  if (!file.exists(nc_path)) {
    stop("copernicusmarine produced no file for ", dataset_id, ". Last output:\n",
         paste(utils::tail(out, 15), collapse = "\n"))
  }
  nc_path
}

## Read a variable plus its named dimensions; returns list(values, dims)
.read_nc_var <- function(nc_path, variable) {
  nc <- ncdf4::nc_open(nc_path)
  on.exit(ncdf4::nc_close(nc))
  if (!variable %in% names(nc$var)) stop("Variable '", variable, "' not in ", nc_path,
                                         " (found: ", paste(names(nc$var), collapse = ", "), ").")
  v <- nc$var[[variable]]
  dim_names <- vapply(v$dim, function(d) d$name, character(1))
  dims <- setNames(lapply(v$dim, function(d) d$vals), dim_names)
  units <- ncdf4::ncatt_get(nc, variable, "units")$value
  list(values = ncdf4::ncvar_get(nc, variable, collapse_degen = FALSE), dims = dims, units = units)
}

.find_dim <- function(dim_names, pattern) {
  hit <- grep(pattern, dim_names, ignore.case = TRUE)
  if (length(hit) == 0) stop("No dimension matching '", pattern, "' among: ", paste(dim_names, collapse = ", "))
  hit[1]
}

## Model-domain mask for a model grid (lon x lat logical matrix): cell
## centre inside the FILTER_AREAS polygons (area_shp) AND seafloor depth
## within the MEDITS strata range (e.g. 10-500 m), i.e. the same area
## Ecopath_B is expressed over. Without it the mean ran over the whole
## West Med bbox (open ocean, African coast), which is far more
## oligotrophic than the shelf/slope the model covers. Bathymetry from
## marmap (NOAA ETOPO, same source as compute_strata_area_by_area());
## if that download fails the mask falls back to the polygons only.
.domain_mask_cache <- new.env()
.model_domain_mask <- function(lon, lat) {
  key <- paste(length(lon), signif(range(lon), 6), length(lat), signif(range(lat), 6), collapse = "_")
  if (!is.null(.domain_mask_cache[[key]])) return(.domain_mask_cache[[key]])
  mask <- matrix(TRUE, nrow = length(lon), ncol = length(lat))
  if (!exists("area_shp") || is.null(area_shp)) {
    message("[Plankton domain] area_shp not in scope - averaging over the whole download bbox.")
    return(mask)
  }
  shp <- area_shp
  if (exists("AREA_ID_COL") && exists("FILTER_AREAS") && length(FILTER_AREAS) > 0 && AREA_ID_COL %in% names(shp))
    shp <- shp[shp[[AREA_ID_COL]] %in% FILTER_AREAS, ]
  old_s2 <- sf::sf_use_s2(); suppressMessages(sf::sf_use_s2(FALSE)); on.exit(suppressMessages(sf::sf_use_s2(old_s2)), add = TRUE)
  grid <- expand.grid(lon = as.numeric(lon), lat = as.numeric(lat))
  pts <- sf::st_as_sf(grid, coords = c("lon", "lat"), crs = 4326)
  shp <- sf::st_transform(sf::st_make_valid(shp), 4326)
  in_poly <- lengths(suppressMessages(sf::st_intersects(pts, shp))) > 0
  depth_ok <- rep(TRUE, nrow(grid))
  dmin <- if (exists("MEDITS_STRATA")) min(MEDITS_STRATA$depth_min) else 10
  dmax <- if (exists("MEDITS_STRATA")) max(MEDITS_STRATA$depth_max) else 800
  bathy <- tryCatch(marmap::getNOAA.bathy(lon1 = min(lon) - 0.1, lon2 = max(lon) + 0.1,
                                          lat1 = min(lat) - 0.1, lat2 = max(lat) + 0.1, resolution = 2),
                    error = function(e) { message("[Plankton domain] Bathymetry download failed (", conditionMessage(e),
                                                  ") - masking by GSA polygons only, no depth filter."); NULL })
  if (!is.null(bathy)) {
    blon <- as.numeric(rownames(bathy)); blat <- as.numeric(colnames(bathy))
    ix <- pmax(1, pmin(length(blon), round(approx(blon, seq_along(blon), grid$lon, rule = 2)$y)))
    iy <- pmax(1, pmin(length(blat), round(approx(blat, seq_along(blat), grid$lat, rule = 2)$y)))
    depth <- -as.numeric(bathy)[ix + (iy - 1) * length(blon)]
    depth_ok <- is.finite(depth) & depth >= dmin & depth <= dmax
  }
  mask <- matrix(in_poly & depth_ok, nrow = length(lon), ncol = length(lat))
  message("[Plankton domain] ", sum(mask), " of ", length(mask), " grid cells inside the model domain (FILTER_AREAS polygons, ",
          dmin, "-", dmax, " m).")
  if (sum(mask) == 0) { message("[Plankton domain] Empty mask - falling back to the whole bbox."); mask[] <- TRUE }
  assign(key, mask, envir = .domain_mask_cache)
  mask
}

## Area-weighted (cos latitude) mean of a lon x lat matrix, ocean cells
## inside the model domain only (pass lon to apply the domain mask)
.area_weighted_mean <- function(mat, lat, lon = NULL) {
  w <- matrix(cos(lat * pi / 180), nrow = nrow(mat), ncol = ncol(mat), byrow = TRUE)
  if (!is.null(lon)) mat[!.model_domain_mask(lon, lat)] <- NA
  ok <- is.finite(mat)
  if (!any(ok)) return(NA_real_)
  sum(mat[ok] * w[ok]) / sum(w[ok])
}

.depth_thickness <- function(depth) {
  depth <- as.numeric(depth)
  if (length(depth) == 1) return(1)
  edges <- c(0, (depth[-1] + depth[-length(depth)]) / 2,
             depth[length(depth)] + (depth[length(depth)] - depth[length(depth) - 1]) / 2)
  diff(edges)
}

## Steps 1-2 and 4 for one phyc dataset: download, annual mean per cell,
## depth integration (mmol C m-3 -> g C m-2), area-weighted mean.
.phyc_depth_integrated_tC_km2 <- function(dataset_id, years, bbox, cred_args, timeout_sec) {
  nc <- .cmems_subset(dataset_id, "phyc", paste0(min(years), "-01-01"), paste0(max(years), "-12-31"),
                      bbox, depth_max = 200, cred_args, timeout_sec)  # FLAGGED: 0-200 m; phytoplankton carbon below 200 m is negligible and the full column multiplies download size
  ph <- .read_nc_var(nc, "phyc")
  dn <- names(ph$dims)
  i_lon <- .find_dim(dn, "^lon"); i_lat <- .find_dim(dn, "^lat")
  i_dep <- .find_dim(dn, "depth"); i_tim <- .find_dim(dn, "time")
  unit_to_mmol <- if (grepl("^mmol", ph$units)) 1 else if (grepl("^mol", ph$units)) 1000 else
    stop("Unrecognised phyc units '", ph$units, "' in ", dataset_id, ".")
  arr <- aperm(ph$values, c(i_lon, i_lat, i_dep, i_tim)) * unit_to_mmol
  annual <- apply(arr, c(1, 2, 3), mean, na.rm = TRUE)          # lon x lat x depth, mmol C m-3
  dz <- .depth_thickness(ph$dims[[i_dep]])
  n_valid <- apply(is.finite(annual), c(1, 2), sum)
  annual[!is.finite(annual)] <- 0
  integ <- apply(sweep(annual, 3, dz, `*`), c(1, 2), sum)       # mmol C m-2
  integ[n_valid == 0] <- NA                                      # land
  .area_weighted_mean(integ * 0.012, ph$dims[[i_lat]], ph$dims[[i_lon]])  # g C m-2 == t C km-2, model domain only
}

fetch_cmems_plankton_biomass <- function(out_dir, force_refresh = FALSE,
                                         bbox = c(lon_min = -6, lon_max = 16, lat_min = 35, lat_max = 45),
                                         phyto_years = 1999:2001,
                                         zoo_proxy_years = 1998:2000,
                                         phyto_dataset_id = "cmems_mod_med_bgc-plankton_my_4.2km_P1Y-m",  # annual means: the monthly 4.2 km file for 1999-2001 would be several GB
                                         phyto_crosscheck = TRUE,
                                         crosscheck_years = 1995,
                                         crosscheck_dataset_id = "cmems_mod_glo_bgc_my_0.25deg_P1M-m",
                                         zoo_dataset_id = "cmems_mod_glo_bgc_my_0.083deg-lmtl_P1D-i",
                                         alloc = PLANKTON_ALLOCATION,
                                         auto_install = TRUE, timeout_sec = 1800) {
  out_csv_path <- file.path(out_dir, "cmems_plankton_biomass_by_fg_modeldomain.csv")  # new name: older caches averaged over the whole bbox
  if (!force_refresh && file.exists(out_csv_path)) {
    message("[CMEMS plankton] Using cached ", out_csv_path, " (force_refresh = TRUE to re-download).")
    return(invisible(fread(out_csv_path)))
  }
  if (!requireNamespace("ncdf4", quietly = TRUE)) {
    message("[CMEMS plankton] FAILED - package 'ncdf4' is required (install.packages('ncdf4')).")
    return(invisible(NULL))
  }
  if (!exists("ensure_copernicusmarine_cli", mode = "function")) {
    stop("[CMEMS plankton] source lib_cmems_phytoplankton_biomass.R first (provides the CLI/credential helpers).")
  }
  if (!ensure_copernicusmarine_cli(auto_install = auto_install)) {
    message("[CMEMS plankton] FAILED at CLI check - see [CMEMS diag] messages above.")
    return(invisible(NULL))
  }
  stopifnot(abs(alloc$small_phyto_fraction + alloc$large_phyto_fraction - 1) < 1e-10,
            abs(alloc$meso_micro_fraction + alloc$macro_zoo_fraction - 1) < 1e-10,
            abs(alloc$micro_within_meso_micro + alloc$meso_within_meso_micro - 1) < 1e-10)

  result <- tryCatch({
    creds <- ensure_copernicusmarine_credentials()
    cred_args <- character(0)  # unused - kept so the helper signatures stay the same

    ## --- Phytoplankton (main): MedBFM phyc, 1999-2001 ------------------------
    phyto_tC_km2 <- .phyc_depth_integrated_tC_km2(phyto_dataset_id, phyto_years, bbox, cred_args, timeout_sec)
    message("[CMEMS plankton] Phytoplankton MAIN (MedBFM Med reanalysis, ", paste(range(phyto_years), collapse = "-"),
            " as proxy for 1994-1996): ", signif(phyto_tC_km2, 4), " t C/km2 (depth-integrated 0-200 m, mean over the model domain).")
    ## --- Phytoplankton cross-check: PISCES global hindcast, 1995 -------------
    ## Logged only, never used for Ecopath_B. A large gap between the two is
    ## worth a look before trusting either.
    phyto_check_tC_km2 <- NA_real_
    if (phyto_crosscheck) {
      phyto_check_tC_km2 <- tryCatch(.phyc_depth_integrated_tC_km2(crosscheck_dataset_id, crosscheck_years, bbox, cred_args, timeout_sec),
                                     error = function(e) { message("[CMEMS plankton] PISCES cross-check failed: ", conditionMessage(e)); NA_real_ })
      if (is.finite(phyto_check_tC_km2)) message("[CMEMS plankton] Phytoplankton CROSS-CHECK (PISCES global, ",
                                                 paste(range(crosscheck_years), collapse = "-"), "): ", signif(phyto_check_tC_km2, 4),
                                                 " t C/km2 - ratio PISCES/MedBFM = ", round(phyto_check_tC_km2 / phyto_tC_km2, 2), ".")
    }

    ## --- Zooplankton: LMTL zooc, proxy years --------------------------------
    zoo_nc <- .cmems_subset(zoo_dataset_id, "zooc", paste0(min(zoo_proxy_years), "-01-01"),
                            paste0(max(zoo_proxy_years), "-12-31"), bbox, depth_max = NULL, cred_args, timeout_sec)
    zo <- .read_nc_var(zoo_nc, "zooc")
    zn <- names(zo$dims)
    j_lon <- .find_dim(zn, "^lon"); j_lat <- .find_dim(zn, "^lat"); j_tim <- .find_dim(zn, "time")
    if (!grepl("^g m-2|^g/m2|^g m\\^-2", zo$units)) message("[CMEMS plankton] NOTE: zooc units are '", zo$units,
                                                            "' - expected g m-2 (carbon). Check before trusting.")
    zarr <- zo$values
    other <- setdiff(seq_along(dim(zarr)), c(j_lon, j_lat, j_tim))
    if (length(other) > 0) zarr <- apply(zarr, c(j_lon, j_lat, j_tim), mean, na.rm = TRUE) else
      zarr <- aperm(zarr, c(j_lon, j_lat, j_tim))
    zoo_mean <- apply(zarr, c(1, 2), mean, na.rm = TRUE)
    zoo_mean[!is.finite(zoo_mean)] <- NA
    zoo_tC_km2 <- .area_weighted_mean(zoo_mean, zo$dims[[j_lat]], zo$dims[[j_lon]])
    message("[CMEMS plankton] Zooplankton (LMTL zooc, ", paste(range(zoo_proxy_years), collapse = "-"),
            " proxy): ", signif(zoo_tC_km2, 4), " t C/km2 (mean over the model domain).")

    ## --- Size split + carbon -> wet weight (PROVISIONAL) --------------------
    a <- alloc
    meso_micro_C <- zoo_tC_km2 * a$meso_micro_fraction
    out <- data.table(
      TargetGroup = c("SmallPhytoplankton", "LargePhytoplankton", "MesoMicroZooplankton", "MacroZooplankton"),
      Biomass_tC_km2 = c(phyto_tC_km2 * a$small_phyto_fraction, phyto_tC_km2 * a$large_phyto_fraction,
                         meso_micro_C, zoo_tC_km2 * a$macro_zoo_fraction),
      Biomass_t_km2 = c(phyto_tC_km2 * a$small_phyto_fraction / a$phyto_C_per_WW,
                        phyto_tC_km2 * a$large_phyto_fraction / a$phyto_C_per_WW,
                        meso_micro_C * a$micro_within_meso_micro / a$microzoo_C_per_WW +
                          meso_micro_C * a$meso_within_meso_micro / a$mesozoo_C_per_WW,
                        zoo_tC_km2 * a$macro_zoo_fraction / a$macrozoo_C_per_WW)
    )
    out[, Source_citation := c(
      rep(paste0("Copernicus MEDSEA_MULTIYEAR_BGC_006_008 (MedBFM Mediterranean reanalysis, Cossarini et al. 2021), ",
                 phyto_dataset_id, ", phyc, ", paste(range(phyto_years), collapse = "-"),
                 " mean as proxy for the 1994-1996 baseline (reanalysis starts 1999), depth-integrated 0-200 m, mean over the model domain (FILTER_AREAS GSAs within the MEDITS strata depth range)",
                 if (is.finite(phyto_check_tC_km2)) paste0(" [cross-check PISCES global ", paste(range(crosscheck_years), collapse = "-"),
                                                           ": ", signif(phyto_check_tC_km2, 3), " t C/km2 vs ", signif(phyto_tC_km2, 3), "]") else "",
                 "; PROVISIONAL ",
                 a$small_phyto_fraction, "/", a$large_phyto_fraction, " small/large split; C:WW ",
                 a$phyto_C_per_WW, " (Yacobi & Zohary 2010)"), 2),
      paste0("Copernicus GLOBAL_MULTIYEAR_BGC_001_033 (SEAPODYM LMTL), ", zoo_dataset_id, ", zooc, ",
             paste(range(zoo_proxy_years), collapse = "-"), " mean as proxy for the 1994-1996 baseline (product starts",
             " 1998); PROVISIONAL ", a$meso_micro_fraction, " of total zooplankton (Fernandez de Puelles et al. 2014),",
             " micro/meso ", a$micro_within_meso_micro, "/", a$meso_within_meso_micro, ", C:WW ", a$microzoo_C_per_WW,
             " (Fenchel & Finlay 1983) / ", a$mesozoo_C_per_WW, " (Kiorboe 2013)"),
      paste0("Copernicus GLOBAL_MULTIYEAR_BGC_001_033 (SEAPODYM LMTL), ", zoo_dataset_id, ", zooc, ",
             paste(range(zoo_proxy_years), collapse = "-"), " mean as proxy for the 1994-1996 baseline; PROVISIONAL ",
             a$macro_zoo_fraction, " of total zooplankton; C:WW ", a$macrozoo_C_per_WW, " (Kiorboe 2013)")
    )]
    fwrite(out, out_csv_path)
    message("[CMEMS plankton] === SUCCESS === saved ", out_csv_path, ":")
    print(out[, .(TargetGroup, Biomass_tC_km2 = signif(Biomass_tC_km2, 4), Biomass_t_km2 = signif(Biomass_t_km2, 4))])
    out
  }, error = function(e) {
    message("[CMEMS plankton] === FAILED === ", conditionMessage(e))
    NULL
  })
  invisible(result)
}

## =================================================================
## DETRITUS BIOMASS (FG 79) - Pauly, Soriano-Bartz & Palomares (1993)
## empirical relationship (the standard EwE approach for detritus):
##   log10(D) = 0.954 log10(PP) + 0.863 log10(E) - 2.41
##   D  = detritus standing stock, g C m-2
##   PP = primary production, g C m-2 yr-1
##   E  = euphotic depth, m
## Step 1 - PP: MedBFM reanalysis (same product as phytoplankton),
##   dataset cmems_mod_med_bgc-bio_my_4.2km_P1Y-m, variable nppv
##   (mg C m-3 d-1), 1999-2001, integrated 0-200 m, x 365 / 1000.
## Step 2 - E: SEAPODYM LMTL euphotic depth zeu (m), 1998-2000 mean.
##   (MedBFM does not publish euphotic depth.)
## Step 3 - area-weighted means over the model domain (FILTER_AREAS GSAs, strata depth range), apply the equation.
## Step 4 - carbon -> wet weight x 9 (1 g C = 9 g WW, Pauly & Christensen
##   1995) - FLAGGED: the conventional EwE factor, not a measured one.
## Cached as csv_out_dir/cmems_detritus_biomass.csv.
## =================================================================
fetch_cmems_detritus_biomass <- function(out_dir, force_refresh = FALSE,
                                         bbox = c(lon_min = -6, lon_max = 16, lat_min = 35, lat_max = 45),
                                         pp_years = 1999:2001, zeu_years = 1998:2000,
                                         pp_dataset_id = "cmems_mod_med_bgc-bio_my_4.2km_P1Y-m",
                                         zeu_dataset_id = "cmems_mod_glo_bgc_my_0.083deg-lmtl_P1D-i",
                                         c_to_ww = 9, timeout_sec = 1800) {
  out_csv_path <- file.path(out_dir, "cmems_detritus_biomass_modeldomain.csv")  # new name: older caches averaged over the whole bbox
  if (!force_refresh && file.exists(out_csv_path)) {
    message("[CMEMS detritus] Using cached ", out_csv_path, " (force_refresh = TRUE to re-download).")
    return(invisible(fread(out_csv_path)))
  }
  if (!requireNamespace("ncdf4", quietly = TRUE) || !ensure_copernicusmarine_cli()) {
    message("[CMEMS detritus] FAILED - ncdf4 or the copernicusmarine CLI is unavailable.")
    return(invisible(NULL))
  }
  res <- tryCatch({
    ensure_copernicusmarine_credentials()
    ## Step 1 - primary production
    pp_nc <- .cmems_subset(pp_dataset_id, "nppv", paste0(min(pp_years), "-01-01"), paste0(max(pp_years), "-12-31"),
                           bbox, depth_max = 200, character(0), timeout_sec)
    pp <- .read_nc_var(pp_nc, "nppv"); dn <- names(pp$dims)
    i_lon <- .find_dim(dn, "^lon"); i_lat <- .find_dim(dn, "^lat"); i_dep <- .find_dim(dn, "depth"); i_tim <- .find_dim(dn, "time")
    arr <- aperm(pp$values, c(i_lon, i_lat, i_dep, i_tim))
    mean_yr <- apply(arr, c(1, 2, 3), mean, na.rm = TRUE)                  # mg C m-3 d-1
    n_valid <- apply(is.finite(mean_yr), c(1, 2), sum); mean_yr[!is.finite(mean_yr)] <- 0
    integ <- apply(sweep(mean_yr, 3, .depth_thickness(pp$dims[[i_dep]]), `*`), c(1, 2), sum)  # mg C m-2 d-1
    integ[n_valid == 0] <- NA
    PP <- .area_weighted_mean(integ * 365 / 1000, pp$dims[[i_lat]], pp$dims[[i_lon]])  # g C m-2 yr-1, model domain
    ## Step 2 - euphotic depth
    z_nc <- .cmems_subset(zeu_dataset_id, "zeu", paste0(min(zeu_years), "-01-01"), paste0(max(zeu_years), "-12-31"),
                          bbox, depth_max = NULL, character(0), timeout_sec)
    zu <- .read_nc_var(z_nc, "zeu"); zn <- names(zu$dims)
    j_lon <- .find_dim(zn, "^lon"); j_lat <- .find_dim(zn, "^lat"); j_tim <- .find_dim(zn, "time")
    zarr <- apply(zu$values, c(j_lon, j_lat), mean, na.rm = TRUE); zarr[!is.finite(zarr)] <- NA
    E <- .area_weighted_mean(zarr, zu$dims[[j_lat]], zu$dims[[j_lon]])
    ## Step 3-4
    D_gC <- 10^(0.954 * log10(PP) + 0.863 * log10(E) - 2.41)
    out <- data.table(TargetGroup = "Detritus", PP_gC_m2_yr = PP, euphotic_depth_m = E,
                      Biomass_tC_km2 = D_gC, Biomass_t_km2 = D_gC * c_to_ww,
                      Source_citation = paste0("Pauly, Soriano-Bartz & Palomares 1993 detritus equation (log10 D = 0.954 log10 PP",
                                               " + 0.863 log10 E - 2.41) with PP = ", signif(PP, 3), " g C m-2 yr-1 (MedBFM ",
                                               pp_dataset_id, " nppv, ", paste(range(pp_years), collapse = "-"), ", 0-200 m) and E = ",
                                               signif(E, 3), " m (SEAPODYM LMTL zeu, ", paste(range(zeu_years), collapse = "-"),
                                               "); C -> WW x ", c_to_ww, " (Pauly & Christensen 1995) - FLAGGED conventional factor"))
    fwrite(out, out_csv_path)
    message("[CMEMS detritus] === SUCCESS === PP ", signif(PP, 3), " g C/m2/yr, euphotic depth ", signif(E, 3),
            " m -> detritus ", signif(D_gC, 3), " t C/km2 = ", signif(D_gC * c_to_ww, 3), " t WW/km2.")
    out
  }, error = function(e) { message("[CMEMS detritus] === FAILED === ", conditionMessage(e)); NULL })
  invisible(res)
}

## --- Plankton (FG 71/72/77/78) + Detritus (FG 79) from Copernicus ----
## PRIMARY_PRODUCER_BIOMASS_PATH (pCloud) holds HAND-CITED rows only and is
## NEVER written by this script (overwrite+append on the pCloud drive
## corrupted it with NUL bytes). Model rows (MedBFM/PISCES phyto, LMTL zoo,
## Pauly detritus) are built in memory, written to csv_out_dir, and merged
## with the hand-cited rows into a temp CSV in csv_out_dir that is passed to
## load_manual_cited_biomass_group(). A hand-cited row for a group always
## wins over the model row for that group.
if (!exists("ENABLE_CMEMS_PLANKTON", envir = .GlobalEnv, inherits = FALSE)) ENABLE_CMEMS_PLANKTON <- TRUE
plankton_targets <- c("smallphytoplankton", "largephytoplankton", "mesomicrozooplankton", "macrozooplankton", "detritus")
pp_cols <- c("Year", "Group", "Biomass_t", "Source_citation")
pp_hand <- if (file.exists(PRIMARY_PRODUCER_BIOMASS_PATH)) {
  tryCatch(fread(PRIMARY_PRODUCER_BIOMASS_PATH, encoding = "UTF-8", fill = TRUE), error = function(e) NULL)
} else NULL
if (!is.null(pp_hand) && nrow(pp_hand) > 0 && all(c("Group", "Source_citation") %in% names(pp_hand))) {
  pp_hand <- pp_hand[!is.na(Group) & nzchar(trimws(Group))]
  ## AUTO-DRAFT rows from earlier runs are ignored (model rows are rebuilt
  ## every run); EcoBase rows stay so the loader can report/exclude them.
  pp_hand <- pp_hand[is.na(Source_citation) | !grepl("^AUTO-DRAFT", Source_citation)]
} else pp_hand <- data.table(Year = integer(), Group = character(), Biomass_t = numeric(), Source_citation = character())
pp_hand_groups <- unique(tolower(trimws(pp_hand[is.na(Source_citation) | !grepl("ecobase", Source_citation, ignore.case = TRUE), Group])))
plankton_missing <- setdiff(plankton_targets, pp_hand_groups)
study_area_km2 <- sum(strata_area_by_area$area_km2, na.rm = TRUE)
.to_model_rows <- function(draft, label) {
  if (is.null(draft) || nrow(draft) == 0) return(NULL)
  draft[, .(Year = round(mean(YEAR_ECOPATH)), Group = TargetGroup,
            Biomass_t = Biomass_t_km2 * study_area_km2,
            Source_citation = paste0("AUTO-DRAFT FROM ", label, " - REVIEW BEFORE TRUSTING: ", Source_citation))]
}
plankton_model_rows <- NULL
cmems_helper_path <- file.path(git_dir, "scripts/lib_cmems_phytoplankton_biomass.R")
if (ENABLE_CMEMS_PLANKTON && length(plankton_missing) > 0) {
  if (!file.exists(cmems_helper_path)) {
    message("[Plankton biomass] lib_cmems_phytoplankton_biomass.R not found in ", file.path(git_dir, "scripts"),
            " - skipping the Copernicus plankton/detritus source.")
  } else {
    source(cmems_helper_path)
    plankton_draft <- tryCatch(fetch_cmems_plankton_biomass(out_dir = csv_out_dir), error = function(e) {
      message("[Plankton biomass] FAILED: ", conditionMessage(e)); NULL })
    plankton_model_rows <- .to_model_rows(plankton_draft, "COPERNICUS PLANKTON REANALYSES (01b plankton section)")
    if ("detritus" %in% plankton_missing) {
      detritus_draft <- fetch_cmems_detritus_biomass(out_dir = csv_out_dir)
      if (!is.null(detritus_draft) && nrow(detritus_draft) > 0)
        plankton_model_rows <- rbindlist(list(plankton_model_rows,
          .to_model_rows(detritus_draft[, .(TargetGroup, Biomass_t_km2, Source_citation)],
                         "MEDBFM NPP + LMTL EUPHOTIC DEPTH, PAULY ET AL. 1993 (01b detritus section)")),
          use.names = TRUE, fill = TRUE)
    }
  }
} else if (length(plankton_missing) == 0) {
  message("[Plankton biomass] All plankton groups + Detritus have a hand-cited row in ",
          PRIMARY_PRODUCER_BIOMASS_PATH, " - Copernicus source not queried.")
}

## Phytoplankton-only fallback: runs ONLY if phyto is still missing after
## the main plankton source (e.g. the MedBFM query failed).
phyto_targets <- c("largephytoplankton", "smallphytoplankton")
phyto_have <- union(intersect(phyto_targets, pp_hand_groups),
                    if (!is.null(plankton_model_rows)) tolower(plankton_model_rows$Group) else character(0))
if (length(intersect(phyto_targets, plankton_missing)) > 0 && !all(phyto_targets %in% phyto_have)) {
  if (!exists("ENABLE_CMEMS_PHYTOPLANKTON", envir = .GlobalEnv, inherits = FALSE)) ENABLE_CMEMS_PHYTOPLANKTON <- TRUE
  if (!exists("ENABLE_SATELLITE_PHYTOPLANKTON", envir = .GlobalEnv, inherits = FALSE)) ENABLE_SATELLITE_PHYTOPLANKTON <- TRUE
  phyto_draft <- NULL; phyto_source_label <- NA_character_
  satellite_lib_path <- file.path(git_dir, "scripts/lib_satellite_phytoplankton_biomass.R")
  if (ENABLE_CMEMS_PHYTOPLANKTON && file.exists(cmems_helper_path)) {
    source(cmems_helper_path)
    cmems_phyto <- tryCatch(fetch_cmems_phytoplankton_biomass(out_dir = csv_out_dir), error = function(e) NULL)
    if (!is.null(cmems_phyto) && nrow(cmems_phyto) > 0) {
      phyto_draft <- cmems_phyto; phyto_source_label <- "CMEMS MED BGC REANALYSIS 0-50 m (phyto fallback)"
    }
  }
  if (is.null(phyto_draft) && ENABLE_SATELLITE_PHYTOPLANKTON && file.exists(satellite_lib_path)) {
    message("[Primary producer/plankton biomass] CMEMS phytoplankton unavailable - falling back to satellite chlorophyll-a.")
    source(satellite_lib_path)
    satellite_phyto <- tryCatch(fetch_satellite_phytoplankton_biomass(out_dir = csv_out_dir), error = function(e) NULL)
    if (!is.null(satellite_phyto) && nrow(satellite_phyto) > 0) {
      phyto_draft <- satellite_phyto; phyto_source_label <- "SATELLITE CHLOROPHYLL-A (fallback)"
    }
  }
  fb_rows <- .to_model_rows(phyto_draft, phyto_source_label)
  if (!is.null(fb_rows)) {
    fb_rows <- fb_rows[!tolower(Group) %in% phyto_have]
    plankton_model_rows <- rbindlist(list(plankton_model_rows, fb_rows), use.names = TRUE, fill = TRUE)
  } else {
    message("[Primary producer/plankton biomass] Phytoplankton still missing: MedBFM plankton source, the CMEMS",
            " phyto fallback and the satellite fallback all failed or were disabled this run.")
  }
}

if (!is.null(plankton_model_rows) && nrow(plankton_model_rows) > 0) {
  plankton_model_rows <- plankton_model_rows[tolower(Group) %in% plankton_missing]
  fwrite(plankton_model_rows, file.path(csv_out_dir, "plankton_detritus_model_biomass_rows.csv"))
  message("[Plankton biomass] Model rows used (", nrow(plankton_model_rows), "): ",
          paste(sprintf("%s %.2f t/km2", plankton_model_rows$Group, plankton_model_rows$Biomass_t / study_area_km2),
                collapse = "; "), ". Written to csv_out_dir only - pCloud file untouched.")
}
pp_cols_keep <- intersect(pp_cols, names(pp_hand))
pp_merged <- rbindlist(list(pp_hand[, ..pp_cols_keep], plankton_model_rows), use.names = TRUE, fill = TRUE)
PRIMARY_PRODUCER_MERGED_PATH <- file.path(csv_out_dir, "primary_producer_plankton_biomass_MERGED.csv")
fwrite(pp_merged, PRIMARY_PRODUCER_MERGED_PATH)

primary_producer_biomass_fg_year <- load_manual_cited_biomass_group(
  csv_path = PRIMARY_PRODUCER_MERGED_PATH, taxon_keywords = PRIMARY_PRODUCER_TAXON_KEYWORDS,
  label = "Primary producer/plankton biomass", review_csv_name = "primary_producer_group_to_fg_REVIEW.csv",
  output_csv_name = "primary_producer_plankton_biomass_by_fg.csv",
  ## None of this call's CSVs (primary_producer_plankton_biomass_by_fg.csv,
  ## primary_producer_group_to_fg_REVIEW.csv, and the EcoBase-excluded-
  ## rows REVIEW csv) are used - suppressed here only; megafauna/
  ## benthic-habitat calls below are untouched and keep writing theirs.
  write_review_outputs = FALSE,
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

## Benthic/seagrass/coralligenous groups' own file, separate from
## plankton (see BENTHIC_HABITAT_TAXON_KEYWORDS/BENTHIC_HABITAT_
## BIOMASS_PATH comments above). Same load function, same Density_value/
## Depth_min_m/Depth_max_m/Habitat_species/Habitat_area_km2 extrapolation
## mechanism - Habitat_area_km2 is the column a real EMODnet/EUSeaMap
## habitat-extent shapefile area (clipped to the study domain) belongs
## in, in place of a depth-band assumption, for exactly the patchy
## habitats this file covers.
## #################################################################
## SOURCES B + C - LITERATURE DENSITY (depth band) and SHAPEFILE AREA
## #################################################################
## ---- SOURCE C: habitat shapefile x literature density x occupancy ------
habitat_shapefile_biomass_fg_year <- load_manual_cited_biomass_group(
  csv_path = HABITAT_SHAPEFILE_BIOMASS_PATH, taxon_keywords = BENTHIC_HABITAT_TAXON_KEYWORDS,
  label = "Habitat shapefile biomass",
  review_csv_name = "habitat_shapefile_group_to_fg_REVIEW.csv",
  output_csv_name = "habitat_shapefile_biomass_by_fg.csv",
  fallback_message = paste0("Expected: one row per habitat group (Posidonia, Cymodocea, Macroalgae, GorgoniansCorals)",
                            " with Density_value/Density_unit, Habitat_area_km2 (mapped area from the shapefile),",
                            " Occupancy_fraction and Source_citation."))

## ---- SOURCE B: literature density x depth band --------------------------
literature_density_biomass_fg_year <- load_manual_cited_biomass_group(
  csv_path = LITERATURE_DENSITY_BIOMASS_PATH, taxon_keywords = BENTHIC_HABITAT_TAXON_KEYWORDS,
  label = "Literature density biomass",
  review_csv_name = "literature_density_group_to_fg_REVIEW.csv",
  output_csv_name = "literature_density_biomass_by_fg.csv",
  fallback_message = paste0("Expected: one row per group/subgroup (BenthicMollusc_*, Macrobenthos_*, GorgoniansCorals for",
                            " soft-bottom octocorals, Suprabenthos) with Density_value/Density_unit, Depth_min_m/",
                            " Depth_max_m and Source_citation. Rows for the same FG are added together."))

## Combine B + C into one FG x Year table (same columns as either input),
## so everything downstream keeps reading benthic_habitat_biomass_fg_year.
.combine_cited <- function(...) {
  parts <- Filter(function(x) !is.null(x) && nrow(x) > 0, list(...))
  if (length(parts) == 0) return(list(...)[[1]])
  dt <- rbindlist(parts, use.names = TRUE, fill = TRUE)
  dt[, .(group_biomass_t = sum(group_biomass_t, na.rm = TRUE),
         group_sources = paste(unique(na.omit(group_sources)), collapse = " || "),
         stock_assessment_density_t_km2 = sum(stock_assessment_density_t_km2, na.rm = TRUE)),
     by = .(FG_num, FG_name, Year)]
}
benthic_habitat_biomass_fg_year <- .combine_cited(habitat_shapefile_biomass_fg_year, literature_density_biomass_fg_year)
benthic_habitat_species_lit_biomass <- rbindlist(list(attr(habitat_shapefile_biomass_fg_year, "species_detail"),
                                                      attr(literature_density_biomass_fg_year, "species_detail")),
                                                 use.names = TRUE, fill = TRUE)
message("[Benthic biomass] Combined literature-density + shapefile sources: ",
        if (nrow(benthic_habitat_biomass_fg_year) > 0) paste(unique(benthic_habitat_biomass_fg_year$FG_name), collapse = ", ") else "none", ".")

## Combine both manual-cited sources' per-species detail (where a
## Species column was supplied -
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

## Two real sources (Telesca et al. 2015 on Posidonia decline, Linares
## et al. 2021 on gorgonian collapse from marine heatwaves), same "flag
## the real direction, never fabricate a ratio" pattern already used
## for ICCAT BFT/SWO (ICCAT_TEMPORAL_BASELINE_
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
## Posidonia/GorgoniansCorals live in benthic_habitat_biomass_fg_year
## (see the 3-CSV split above), not primary_producer_biomass_fg_year.
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