## =================================================================
## combine_STAR_RAMlegacy.R
##
## Builds combined_medbs_star_ramlegacy.csv - the GFCM STAR / RAM
## Legacy stock-assessment biomass+catch table that 01_biomass.R and
## 02_fisheries.R both expect to find at
## <pcloud_dir>/data/fisheries/STAR_RAMLegacy/combined_medbs_star_ramlegacy.csv
## (STAR_RAMLEGACY_DIR in both scripts - see their own "Stock-
## assessment (GFCM STAR / RAM Legacy) biomass" / "STAR/RAM cross-
## check" blocks). Neither consuming script depends on THIS script
## having run in the same session - they just fread() the CSV this
## script writes, message()+skip if it isn't there yet.
##
## THIS SCRIPT DID NOT EXIST BEFORE (see README.md "Known gaps" and
## pipeline_documentation.qmd, both as of 2026-09-24) - it was a
## documented no-op. This is the first implementation.
##
## -----------------------------------------------------------------
## EXACT OUTPUT SCHEMA (confirmed against the consuming code, not
## guessed - see the two file:line references below for exactly how
## every column is read):
##
##   source          chr   short source label, e.g. "RAM Legacy" or
##                         "GFCM MEDBS" or "STECF". Free text - only
##                         ever pasted into a "sources used" string
##                         downstream (paste(sort(unique(source)),
##                         collapse = "+")), never matched against a
##                         fixed vocabulary.
##   stock_key       chr   unique stock identifier (RAM Legacy's own
##                         "stockid", or a hand-picked key for a
##                         manual GFCM/STECF row). Only used via
##                         uniqueN(stock_key) (counts distinct
##                         assessed stocks per FG x Year) - never
##                         parsed, so any unique string works.
##   species         chr   scientific name (Genus species), matched
##                         against fg_lookup_safe$ScientificName /
##                         fg_lookup$ScientificName - exact match
##                         first, genus fallback second (extract_genus()
##                         pulls the first word). Must be a real
##                         binomial for the genus fallback to work at
##                         all.
##   common_name     chr   descriptive only, not consumed by either
##                         script - carried through for human review.
##   gsa             chr/int  GFCM GSA number/label if known, else NA.
##                         Only ever carried through a fsetdiff()/merge
##                         join key, never filtered on - safe to leave
##                         NA when a stock has no GSA-level resolution
##                         (RAM Legacy assessments typically don't).
##   subregion       chr   THE FILTER COLUMN - both consuming scripts
##                         do star_ram_combined[grepl("Western
##                         Mediterranean", subregion, fixed = TRUE)]
##                         (01_biomass.R ~L1568, 02_fisheries.R ~L3643).
##                         A row whose subregion text does NOT contain
##                         the literal substring "Western Mediterranean"
##                         is silently excluded downstream - not an
##                         error, just out of scope for this model.
##   year            int   calendar year. Read as `year` (lowercase)
##                         and renamed to Year downstream - column
##                         must be exactly lowercase `year`.
##   biomass         num   stock biomass or SSB, ABSOLUTE TONNES for
##                         the whole assessed stock (NOT a density,
##                         NOT per-GSA) - both scripts sum this across
##                         every West Med GSA row to get a whole-
##                         domain total (star_biomass_t = sum(biomass)),
##                         so a stock reported over several GSA rows
##                         must have its total biomass already split
##                         across those rows, or reported once with
##                         gsa = NA, never repeated at full value on
##                         every GSA row (that would double-count).
##   catches         num   total catch, absolute tonnes, same
##                         summing convention as biomass.
##   landings        num   total landings, absolute tonnes.
##   landings_flag   logi  TRUE when landings > catches for that row
##                         (a known data-entry inconsistency, since
##                         by definition Catch = Landings + Discards
##                         >= Landings) - such rows are EXCLUDED from
##                         the landings sum (star_landings_t =
##                         sum(landings[landings_flag %in% FALSE])),
##                         so a bad/unclear row still contributes to
##                         the catch total but not the landings one,
##                         rather than being dropped outright.
##
## Confirmed against, read in full (~100 lines around each match):
##   01_biomass.R    L1524-1638  ("Stock-assessment (GFCM STAR / RAM
##                   Legacy) biomass" block - star_wm/star_direct/
##                   star_genus/star_matched/star_biomass_by_fg)
##   02_fisheries.R  L3596-3695  ("STAR/RAM cross-check" block - same
##                   match cascade, star_catch_by_fg, landings_flag use)
##
## If a stock appears in BOTH the RAM Legacy source and the manual
## GFCM/STECF CSV for the same species/year, both are kept as SEPARATE
## rows (different `source` and `stock_key`), never silently merged -
## the consuming code already aggregates multiple source rows per
## FG/Year (star_biomass_by_fg groups `by = .(FG_num, FG_name, Year =
## year)` and sums; the priority/override logic that picks ONE
## authoritative figure per FG x Year lives entirely in 01_biomass.R's
## `merge_manual_cited_biomass()`/ICCAT-override step, not here).
## =================================================================


## =================================================================
## PART 1 of 2: GFCM STAR / RAM Legacy Stock Assessment Database
## -----------------------------------------------------------------
## The genuinely automatable half - RAM Legacy is a real, versioned,
## bulk-downloadable public database, unlike GFCM's/STECF's own SAF/
## assessment reports (Part 2 below).
##
## Uses the `ramlegacy` R package (rOpenSci, on CRAN -
## https://docs.ropensci.org/ramlegacy/). download_ramlegacy() fetches
## the whole database (a zipped RData/Access-db bundle) from its
## Zenodo archive and caches it locally; read_ramlegacy() then loads
## it into a named list of tables (one per table in the source
## database/spreadsheet).
##
## VERSION: as of this writing the current release is commonly cited
## as RAM Legacy v4.66, archived at
## https://zenodo.org/records/14043038 (cite the DOI in any output
## you publish). RAM Legacy is updated periodically (roughly every 1-2
## years) with a NEW Zenodo record/DOI each time, and the `ramlegacy`
## package itself tracks whichever version its maintainers have most
## recently pointed it at - RE-CHECK docs.ropensci.org/ramlegacy/ and
## the package's own DESCRIPTION/NEWS at run time rather than trusting
## this comment indefinitely.
##
## !! CANNOT BE VERIFIED FROM HERE !!  I have no internet-connected R
## environment in this sandbox, so none of the following was actually
## run:
##   - that download_ramlegacy()/read_ramlegacy() are still the
##     correct, current function names (the ramlegacy package
##     vignette history has used slightly different names across
##     versions - e.g. an earlier `load_ramlegacy()`) - the loader
##     below tries a short list of plausible names and stops with a
##     clear message (not a silent guess) if none exist;
##   - the EXACT current column names inside RAM Legacy's own `stock`/
##     `area`/`timeseries`/`timeseries_values_views`/`bioparams`
##     tables. Everywhere below that assumes a specific column name
##     (e.g. `scientificname`, `region`, `stockid`, `TB`, `SSB`, `TC`,
##     `TL`) is flagged "ASSUMED SCHEMA" in a comment right next to it.
##     These assumptions are based on RAM Legacy's long-published table
##     layout and its own tutorials/vignettes, not on having actually
##     inspected v4.66's real objects - print(names(ram_data)) and
##     str(ram_data$stock) etc. the FIRST time you run this for real,
##     and fix any mismatch against RAM_STOCK_COL_ALIASES/
##     RAM_METRIC_COL_ALIASES below rather than the hardcoded names
##     deeper in the code.
## =================================================================

pkgs <- c("data.table", "stringr")
new_pkgs <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new_pkgs) > 0) install.packages(new_pkgs)
invisible(lapply(pkgs, library, character.only = TRUE))

if (!requireNamespace("ramlegacy", quietly = TRUE)) {
  message("[combine_STAR_RAMlegacy] installing 'ramlegacy' from CRAN",
          " (https://docs.ropensci.org/ramlegacy/)...")
  install.packages("ramlegacy")
}
if (!requireNamespace("ramlegacy", quietly = TRUE)) {
  stop("[combine_STAR_RAMlegacy] the 'ramlegacy' package could not be installed - install it manually",
       " (install.packages(\"ramlegacy\")) and re-run. See https://docs.ropensci.org/ramlegacy/.")
}

## =================================================================
## STEP 1: Configuration - SAME "only set if unset" convention as
## 01_biomass.R/02_fisheries.R, so this can be sourced from a driver
## script that already resolved pcloud_dir, or run standalone.
## =================================================================
if (!exists("pcloud_dir", envir = .GlobalEnv, inherits = FALSE)) {
  if (!requireNamespace("rstudioapi", quietly = TRUE) || !rstudioapi::isAvailable()) {
    stop("This script requires RStudio to pick pcloud_dir interactively, or set pcloud_dir before sourcing.")
  }
  rstudioapi::showQuestion(
    title = "Select pCloud EwE West Med Directory",
    message = "Please select the location of the pCloud Drive/EwE Western Med 2026 folder."
  )
  pcloud_dir <- rstudioapi::selectDirectory()
  if (is.null(pcloud_dir) || pcloud_dir == "" || !dir.exists(pcloud_dir)) stop("No valid pcloud directory selected.")
}

## Only needed to reuse prepare_fg_lookup()/fetch_taxonomy() from the
## shared library, exactly like 01_biomass.R/02_fisheries.R do - same
## git_dir convention as both.
if (!exists("git_dir", envir = .GlobalEnv, inherits = FALSE)) {
  if (!requireNamespace("rstudioapi", quietly = TRUE) || !rstudioapi::isAvailable()) {
    stop("This script requires RStudio to pick git_dir interactively, or set git_dir before sourcing.")
  }
  rstudioapi::showQuestion(
    title = "Select Github WMed_EwE Directory",
    message = "Please select the directory where you cloned the WMed_EwE repository."
  )
  git_dir <- rstudioapi::selectDirectory()
  if (is.null(git_dir) || git_dir == "" || !dir.exists(git_dir)) stop("No valid Github directory selected.")
}
source(paste0(git_dir, "/scripts/lib_survey_fg_density_functions.R"))
source(paste0(git_dir, "/scripts/lib_worms_taxonomy_lookup.R"))

## Species -> FG reference, SAME file/columns 01_biomass.R reads (see
## its STEP 2, fg_species_file/dataframe2/prepare_fg_lookup()).
fg_species_file <- resolve_pcloud_file(paste0(pcloud_dir, "/data/FG_WMed_2026.csv"), pcloud_dir)
dataframe2 <- fread(fg_species_file, encoding = "UTF-8")
fg_lookup_safe <- prepare_fg_lookup(dataframe2)
if (!exists("FISHBASE_TAXONOMY_CACHE_PATH")) FISHBASE_TAXONOMY_CACHE_PATH <- file.path(pcloud_dir, "data/fishbase_taxonomy_cache.csv")
if (!exists("WORMS_TAXONOMY_CACHE_PATH"))    WORMS_TAXONOMY_CACHE_PATH    <- file.path(pcloud_dir, "data/worms_taxonomy_cache.csv")
fg_taxonomy <- fetch_taxonomy(fg_lookup_safe$ScientificName, taxonomy_source = "both",
                               cache_path = FISHBASE_TAXONOMY_CACHE_PATH, worms_cache_path = WORMS_TAXONOMY_CACHE_PATH)
fg_lookup_safe <- merge(fg_lookup_safe, fg_taxonomy, by = "ScientificName", all.x = TRUE)
extract_genus <- function(sci_name) str_extract(sci_name, "^[A-Za-z]+")

## Output locations
STAR_RAMLEGACY_DIR <- file.path(pcloud_dir, "data/fisheries/STAR_RAMLegacy")
if (!dir.exists(STAR_RAMLEGACY_DIR)) dir.create(STAR_RAMLEGACY_DIR, recursive = TRUE)
COMBINED_OUTPUT_PATH   <- file.path(STAR_RAMLEGACY_DIR, "combined_medbs_star_ramlegacy.csv")
MATCH_REVIEW_PATH      <- file.path(STAR_RAMLEGACY_DIR, "star_ramlegacy_species_match_review.csv")
RAM_DOWNLOAD_DIR       <- file.path(STAR_RAMLEGACY_DIR, "ram_legacy_cache")  # ramlegacy package's own on-disk cache

## Force a fresh RAM Legacy download even if a cached copy exists
## (e.g. when a new Zenodo version has been released) - see the
## VERSION note in this script's header.
if (!exists("FORCE_RAM_REDOWNLOAD", envir = .GlobalEnv, inherits = FALSE)) FORCE_RAM_REDOWNLOAD <- FALSE

## Path to the hand-filled GFCM MEDBS/STECF manual CSV (Part 2 below).
## Ships as an EMPTY TEMPLATE alongside this script
## (gfcm_stecf_manual_stock_assessments.csv) - copy it into place here,
## or point this at wherever you keep the real, filled-in copy.
if (!exists("GFCM_STECF_MANUAL_PATH", envir = .GlobalEnv, inherits = FALSE)) {
  GFCM_STECF_MANUAL_PATH <- file.path(pcloud_dir, "data/fisheries/STAR_RAMLegacy/gfcm_stecf_manual_stock_assessments.csv")
}

message("[combine_STAR_RAMlegacy] Output will be written to: ", COMBINED_OUTPUT_PATH)

## =================================================================
## STEP 2: Download + load RAM Legacy
## =================================================================
options(timeout = max(600, getOption("timeout")))  # the bundle is large (tens of MB); avoid a premature download timeout

## The function names below (download_ramlegacy/read_ramlegacy) are
## the ones documented at https://docs.ropensci.org/ramlegacy/ at the
## time this was written. Tried defensively in case a version
## installed at run time uses different names (e.g. an older/newer
## `load_ramlegacy()`) - NOT verified by actually running either.
ram_download_fn <- Find(function(f) exists(f, where = asNamespace("ramlegacy"), inherits = FALSE),
                         c("download_ramlegacy"))
ram_read_fn <- Find(function(f) exists(f, where = asNamespace("ramlegacy"), inherits = FALSE),
                     c("read_ramlegacy", "load_ramlegacy"))
if (is.null(ram_download_fn) || is.null(ram_read_fn)) {
  stop("[combine_STAR_RAMlegacy] Could not find the expected 'ramlegacy' package functions",
       " (tried download_ramlegacy() + read_ramlegacy()/load_ramlegacy()). The installed ramlegacy",
       " version's real exported functions are: ", paste(ls(asNamespace("ramlegacy")), collapse = ", "),
       ". Update ram_download_fn/ram_read_fn above to match, then re-run.")
}

message("[combine_STAR_RAMlegacy] Downloading RAM Legacy Stock Assessment Database (cached under '",
        RAM_DOWNLOAD_DIR, "' - delete that folder, or set FORCE_RAM_REDOWNLOAD <- TRUE, to force a fresh",
        " pull of a newer version). Source: https://zenodo.org/records/14043038 (verify this is still the",
        " current DOI before citing it).")
do.call(ram_download_fn, list(overwrite = FORCE_RAM_REDOWNLOAD, ram_dir = RAM_DOWNLOAD_DIR))
ram_data <- do.call(ram_read_fn, list(ram_dir = RAM_DOWNLOAD_DIR))

if (!is.list(ram_data) || length(ram_data) == 0) {
  stop("[combine_STAR_RAMlegacy] ramlegacy's own loader returned nothing usable - inspect it manually",
       " (str(ram_data)) before continuing; something about the download/parse likely failed.")
}
message("[combine_STAR_RAMlegacy] RAM Legacy tables loaded: ", paste(names(ram_data), collapse = ", "))

## -----------------------------------------------------------------
## Locate the stock-list table (ASSUMED SCHEMA - ramlegacy's list is
## usually named "stock" or "metadata", one row per assessed stock,
## with columns including a stock id, a scientific name, and a
## region/area label). Matched by NAME PATTERN, not hardcoded to one
## exact table name, since this is exactly the part that could not be
## verified here.
## -----------------------------------------------------------------
stock_table_name <- names(ram_data)[grepl("^(stock|metadata)$", names(ram_data), ignore.case = TRUE)][1]
if (is.na(stock_table_name)) {
  stop("[combine_STAR_RAMlegacy] Could not find a stock-list table among RAM Legacy's tables (looked for one",
       " named 'stock' or 'metadata'; real names are: ", paste(names(ram_data), collapse = ", "), "). Open",
       " ram_data in the console and find the right one, then set stock_table_name manually above.")
}
ram_stock <- as.data.table(ram_data[[stock_table_name]])
message("[combine_STAR_RAMlegacy] Using '", stock_table_name, "' as the stock-list table (", nrow(ram_stock),
        " stock(s) total). Its columns: ", paste(names(ram_stock), collapse = ", "))

## Column-name aliases (ASSUMED SCHEMA - extend if the real names
## differ - see this script's own header caveat).
RAM_STOCK_COL_ALIASES <- list(
  stock_id   = c("stockid", "STOCKID", "assessid"),
  sci_name   = c("scientificname", "SCIENTIFICNAME", "sciname"),
  common     = c("commonname", "COMMONNAME"),
  region     = c("region", "REGION", "areaname", "AREANAME", "primary_country"),
  gsa        = c("gsa", "GSA", "areaid", "AREAID")
)
resolve_ram_col <- function(dt_names, aliases) {
  hit <- intersect(aliases, dt_names)
  if (length(hit) == 0) NA_character_ else hit[1]
}
col_stock_id <- resolve_ram_col(names(ram_stock), RAM_STOCK_COL_ALIASES$stock_id)
col_sci_name <- resolve_ram_col(names(ram_stock), RAM_STOCK_COL_ALIASES$sci_name)
col_common   <- resolve_ram_col(names(ram_stock), RAM_STOCK_COL_ALIASES$common)
col_region   <- resolve_ram_col(names(ram_stock), RAM_STOCK_COL_ALIASES$region)
col_gsa      <- resolve_ram_col(names(ram_stock), RAM_STOCK_COL_ALIASES$gsa)

if (is.na(col_stock_id) || is.na(col_sci_name) || is.na(col_region)) {
  stop("[combine_STAR_RAMlegacy] '", stock_table_name, "' is missing a required column - found: ",
       paste(names(ram_stock), collapse = ", "), ". Needed a stock-id column (tried ",
       paste(RAM_STOCK_COL_ALIASES$stock_id, collapse = "/"), "), a scientific-name column (tried ",
       paste(RAM_STOCK_COL_ALIASES$sci_name, collapse = "/"), "), and a region/area column (tried ",
       paste(RAM_STOCK_COL_ALIASES$region, collapse = "/"), "). Add the real header spelling to",
       " RAM_STOCK_COL_ALIASES above rather than guessing which column means what.")
}

## -----------------------------------------------------------------
## Filter to Mediterranean stocks - GREP case-insensitively across
## whatever the region column actually contains (per this task's own
## instruction: don't assume an exact string like "Mediterranean-Black
## Sea" - log what's actually found).
## -----------------------------------------------------------------
setnames(ram_stock, col_region, "ram_region_raw")
med_mask <- grepl("mediterranean", ram_stock$ram_region_raw, ignore.case = TRUE)
message("[combine_STAR_RAMlegacy] Region values found in '", stock_table_name, "$", col_region, "': ",
        paste(sort(unique(ram_stock$ram_region_raw)), collapse = " | "))
message("[combine_STAR_RAMlegacy] ", sum(med_mask), " of ", nrow(ram_stock),
        " RAM Legacy stock(s) have 'Mediterranean' (case-insensitive) somewhere in their region text.")
ram_med_stock <- ram_stock[med_mask]

if (nrow(ram_med_stock) == 0) {
  message("[combine_STAR_RAMlegacy] No Mediterranean stocks found in RAM Legacy this run - the region column",
          " resolved to '", col_region, "' but nothing matched 'mediterranean'. Double-check col_region/",
          " RAM_STOCK_COL_ALIASES above against the real values printed just above before assuming there",
          " genuinely are none (RAM Legacy's Mediterranean coverage is thin but not empty historically).")
}

setnames(ram_med_stock, col_stock_id, "stock_id")
setnames(ram_med_stock, col_sci_name, "sci_name")
if (!is.na(col_common)) setnames(ram_med_stock, col_common, "common_name") else ram_med_stock[, common_name := NA_character_]
if (!is.na(col_gsa)) setnames(ram_med_stock, col_gsa, "gsa") else ram_med_stock[, gsa := NA_character_]

## Whether a Mediterranean stock is specifically WESTERN Mediterranean
## (the substring both consuming scripts filter on) is NOT assumed -
## the raw region/area text is carried straight through as `subregion`
## below. A stock whose text doesn't literally contain "Western
## Mediterranean" is out of scope for THIS model and will be silently
## excluded by 01_biomass.R/02_fisheries.R's own filter - not an error
## here, just logged so it's visible rather than mysterious.
n_westmed_text <- sum(grepl("western mediterranean", ram_med_stock$ram_region_raw, ignore.case = TRUE))
message("[combine_STAR_RAMlegacy] Of those, ", n_westmed_text, " stock(s) have region text that itself",
        " contains 'Western Mediterranean' (the exact substring the consuming scripts filter on) - the rest",
        " will still be WRITTEN to the combined CSV (for transparency/other uses) but will be excluded by",
        " 01_biomass.R/02_fisheries.R's own Western-Med-only filter.")

## -----------------------------------------------------------------
## Locate the biomass/catch time-series table (ASSUMED SCHEMA - RAM
## Legacy ships a convenience "timeseries_values_views" table: one row
## per stockid x year, one column per metric abbreviation, e.g. TB
## (total biomass), SSB (spawning stock biomass), TC (total catch), TL
## (total landings) - this is the layout used in most RAM Legacy
## tutorials/vignettes, NOT verified against v4.66 directly here. Falls
## back to a long-format "timeseries" table (columns tsid/stockid/
## year/value or similar) if the wide view isn't present, pivoting it
## wide by whatever its metric-id column is called.
## -----------------------------------------------------------------
wide_table_name <- names(ram_data)[grepl("^timeseries_values_views$", names(ram_data), ignore.case = TRUE)][1]
long_table_name <- names(ram_data)[grepl("^timeseries$", names(ram_data), ignore.case = TRUE)][1]

RAM_METRIC_COL_ALIASES <- list(
  ssb     = c("SSB", "ssb"),
  tb      = c("TB", "TN", "tb"),           # TB = total biomass, TN = total abundance (last-resort only, not tonnes-comparable, so not used as a biomass fallback below - flagged if it's ever the ONLY option)
  catch   = c("TC", "tc"),                  # total catch
  landing = c("TL", "landings", "tl")       # total landings
)

ram_ts_wide <- NULL
if (!is.na(wide_table_name)) {
  ram_ts_wide <- as.data.table(ram_data[[wide_table_name]])
  message("[combine_STAR_RAMlegacy] Using '", wide_table_name, "' for time-series metrics. Columns: ",
          paste(names(ram_ts_wide), collapse = ", "))
} else if (!is.na(long_table_name)) {
  message("[combine_STAR_RAMlegacy] No wide 'timeseries_values_views' table found - falling back to long",
          " table '", long_table_name, "'. ASSUMED SCHEMA: expects stockid/year/tsid(or tsunique)/value",
          " columns - inspect names(ram_data$", long_table_name, ") if this fails.")
  ram_ts_long <- as.data.table(ram_data[[long_table_name]])
  id_col     <- resolve_ram_col(names(ram_ts_long), c("stockid", "STOCKID"))
  year_col   <- resolve_ram_col(names(ram_ts_long), c("year", "YEAR"))
  metric_col <- resolve_ram_col(names(ram_ts_long), c("tsid", "tsunique", "TSID"))
  value_col  <- resolve_ram_col(names(ram_ts_long), c("value", "tsvalue", "VALUE"))
  if (any(is.na(c(id_col, year_col, metric_col, value_col)))) {
    stop("[combine_STAR_RAMlegacy] Long-format '", long_table_name, "' table doesn't have the expected",
         " stockid/year/metric/value columns (found: ", paste(names(ram_ts_long), collapse = ", "),
         ") - inspect it manually and adjust the id_col/year_col/metric_col/value_col lines above.")
  }
  ram_ts_wide <- dcast(ram_ts_long, as.formula(paste(id_col, "+", year_col, "~", metric_col)), value.var = value_col)
  setnames(ram_ts_wide, id_col, "stockid")
  setnames(ram_ts_wide, year_col, "year")
} else {
  stop("[combine_STAR_RAMlegacy] Could not find a time-series table among RAM Legacy's tables (looked for",
       " 'timeseries_values_views' or 'timeseries'; real names are: ", paste(names(ram_data), collapse = ", "),
       "). Open ram_data in the console to find the right one.")
}

ts_id_col   <- resolve_ram_col(names(ram_ts_wide), c("stockid", "STOCKID"))
ts_year_col <- resolve_ram_col(names(ram_ts_wide), c("year", "YEAR"))
if (is.na(ts_id_col) || is.na(ts_year_col)) {
  stop("[combine_STAR_RAMlegacy] Time-series table is missing a stockid or year column - found: ",
       paste(names(ram_ts_wide), collapse = ", "), ".")
}
setnames(ram_ts_wide, ts_id_col, "stock_id")
setnames(ram_ts_wide, ts_year_col, "year")

col_ssb     <- resolve_ram_col(names(ram_ts_wide), RAM_METRIC_COL_ALIASES$ssb)
col_tb      <- resolve_ram_col(names(ram_ts_wide), RAM_METRIC_COL_ALIASES$tb)
col_catch   <- resolve_ram_col(names(ram_ts_wide), RAM_METRIC_COL_ALIASES$catch)
col_landing <- resolve_ram_col(names(ram_ts_wide), RAM_METRIC_COL_ALIASES$landing)
message("[combine_STAR_RAMlegacy] Metric columns resolved - SSB: ", col_ssb, " | biomass fallback: ", col_tb,
        " | catch: ", col_catch, " | landings: ", col_landing,
        " (any NA above means that metric genuinely wasn't found and will be left blank, never invented).")

## Restrict the time series to the Mediterranean stocks found above
## BEFORE reshaping - keeps this fast and keeps every downstream row
## traceable to a real ram_med_stock row.
ram_ts_med <- ram_ts_wide[stock_id %in% ram_med_stock$stock_id]

## Biomass: SSB preferred, total biomass (TB) as fallback, PER THIS
## TASK'S OWN INSTRUCTION ("prefer SSB or total biomass, whichever the
## metric type indicates"). Never both summed together (that would
## double count) - SSB used whenever present for that stock/year, TB
## only filling in where SSB is NA.
ram_ts_med[, biomass := NA_real_]
if (!is.na(col_ssb)) ram_ts_med[, biomass := as.numeric(get(col_ssb))]
if (!is.na(col_tb))  ram_ts_med[is.na(biomass), biomass := as.numeric(get(col_tb))]
ram_ts_med[, catches  := if (!is.na(col_catch))   as.numeric(get(col_catch))   else NA_real_]
ram_ts_med[, landings := if (!is.na(col_landing)) as.numeric(get(col_landing)) else NA_real_]
ram_ts_med[, landings_flag := !is.na(catches) & !is.na(landings) & (landings > catches)]

## -----------------------------------------------------------------
## Assemble RAM Legacy's contribution to the final schema.
## -----------------------------------------------------------------
ram_final <- merge(ram_ts_med, ram_med_stock[, .(stock_id, sci_name, common_name, gsa, ram_region_raw)],
                   by = "stock_id")
ram_final <- ram_final[!is.na(biomass) | !is.na(catches) | !is.na(landings)]  # drop pure filler rows with nothing usable
ram_final[, `:=`(
  source    = "RAM Legacy",
  stock_key = stock_id,
  species   = sci_name,
  subregion = ram_region_raw
)]
ram_final <- ram_final[, .(source, stock_key, species, common_name, gsa, subregion,
                           year = as.integer(year), biomass, catches, landings, landings_flag)]
message("[combine_STAR_RAMlegacy] RAM Legacy contributes ", nrow(ram_final), " stock x year row(s) across ",
        uniqueN(ram_final$stock_key), " Mediterranean stock(s).")

## -----------------------------------------------------------------
## Species -> FG match review (audit only - the actual FG matching
## happens downstream in 01_biomass.R/02_fisheries.R, this is a
## SEPARATE, EARLY sanity check written here so a bad match is
## visible before the consuming scripts even run - same exact-name-
## then-genus cascade as their own logic).
## -----------------------------------------------------------------
ram_species <- unique(ram_final[, .(species)])
ram_species[, genus := extract_genus(species)]
direct_hits <- merge(ram_species, unique(fg_lookup_safe[, .(ScientificName, FG_num, FG_name)]),
                     by.x = "species", by.y = "ScientificName")
direct_hits[, match_type := "exact"]
remaining   <- ram_species[!species %in% direct_hits$species]
genus_hits  <- merge(remaining[!is.na(genus)],
                     unique(fg_lookup_safe[!is.na(Genus), .(genus = Genus, FG_num, FG_name)]),
                     by = "genus", allow.cartesian = TRUE)
if (nrow(genus_hits) > 0) genus_hits[, match_type := "genus"]
matched     <- rbindlist(list(direct_hits, genus_hits), use.names = TRUE, fill = TRUE)
unmatched   <- setdiff(ram_species$species, matched$species)
match_review <- rbindlist(list(
  matched[, .(species, FG_num, FG_name, match_type)],
  if (length(unmatched) > 0) data.table(species = unmatched, FG_num = NA_real_, FG_name = NA_character_, match_type = "UNMATCHED") else NULL
), fill = TRUE)
fwrite(match_review, MATCH_REVIEW_PATH)
message("[combine_STAR_RAMlegacy] ", nrow(matched), " RAM Legacy species checked against",
        " FG_WMed_2026.csv - ", length(unmatched), " unmatched (excluded from any FG anywhere downstream) -",
        " full detail written to ", MATCH_REVIEW_PATH, ".")


## =================================================================
## PART 2 of 2: GFCM MEDBS / STECF manual, cited stock assessments
## -----------------------------------------------------------------
## Neither GFCM's own Stock Assessment Forms (SAFs, per-stock PDF/XLS
## reports - https://www.fao.org/gfcm/data/safs) nor STECF's
## Mediterranean & Black Sea stock-assessment database (per-stock
## reports - https://stecf.ec.europa.eu/data-dissemination/medbs_en)
## are bulk-downloadable the way RAM Legacy is - so this half is a
## MANUAL, CITED-ENTRY CSV, same convention as iccat_ssb_biomass.csv/
## MEGAFAUNA_BIOMASS_PATH in 01_biomass.R (see merge_manual_cited_
## biomass()/load_manual_cited_biomass_group() there).
##
## HOW TO FILL THIS IN (for Daniel/Andrea, not automated - deliberately
## so, since these numbers should be read off a real report, not
## guessed or scraped unreliably):
##   1. GFCM SAFs: https://www.fao.org/gfcm/data/safs - filter by
##      species/GSA, open the relevant SAF PDF, and read off the
##      assessed stock's biomass/SSB (tonnes) and reported catches/
##      landings (tonnes) for the years you need.
##   2. STECF MEDBS: https://stecf.ec.europa.eu/data-dissemination/medbs_en
##      - STECF's Mediterranean & Black Sea Expert Working Group (EWG)
##      reports carry the same kind of per-stock biomass/SSB and
##      catch/landings tables, by GSA.
##   3. Add ONE ROW per stock x year to
##      gfcm_stecf_manual_stock_assessments.csv (shipped alongside
##      this script as an EMPTY TEMPLATE - copy/rename it to
##      GFCM_STECF_MANUAL_PATH above, or point that variable at it
##      directly), following the EXACT same schema documented at the
##      top of this script (source/stock_key/species/common_name/gsa/
##      subregion/year/biomass/catches/landings/landings_flag), PLUS a
##      mandatory Source_citation column (which report, page/table,
##      and URL/DOI it came from) - same non-negotiable convention as
##      iccat_ssb_biomass.csv. NEVER fill a row with a guessed or
##      interpolated number without saying so explicitly in
##      Source_citation.
##   4. subregion must contain the literal text "Western Mediterranean"
##      for a row to actually be picked up by 01_biomass.R/
##      02_fisheries.R's own filter (see this script's header) -
##      anything else is written through but excluded downstream.
## =================================================================
MANUAL_COL_ALIASES <- list(
  source    = c("source", "Source"),
  stock_key = c("stock_key", "Stock_key", "StockKey"),
  species   = c("species", "Species", "ScientificName"),
  common    = c("common_name", "Common_name", "CommonName"),
  gsa       = c("gsa", "GSA"),
  subregion = c("subregion", "Subregion"),
  year      = c("year", "Year"),
  biomass   = c("biomass", "Biomass", "Biomass_t"),
  catches   = c("catches", "Catches", "Catch_t"),
  landings  = c("landings", "Landings", "Landings_t"),
  citation  = c("Source_citation", "source_citation")
)

manual_final <- data.table(source = character(0), stock_key = character(0), species = character(0),
                            common_name = character(0), gsa = character(0), subregion = character(0),
                            year = integer(0), biomass = numeric(0), catches = numeric(0),
                            landings = numeric(0), landings_flag = logical(0))
if (!file.exists(GFCM_STECF_MANUAL_PATH)) {
  message("\n[combine_STAR_RAMlegacy] '", GFCM_STECF_MANUAL_PATH, "' not found - skipping the GFCM MEDBS/STECF",
          " manual half entirely (the RAM Legacy half above is unaffected). Copy",
          " gfcm_stecf_manual_stock_assessments.csv into place and fill it in by hand (see this script's",
          " Part 2 header for exactly how) if you want GFCM's/STECF's own per-stock assessments included.")
} else {
  manual_raw <- fread(GFCM_STECF_MANUAL_PATH, encoding = "UTF-8")
  resolved <- lapply(MANUAL_COL_ALIASES, function(a) resolve_ram_col(names(manual_raw), a))
  missing_required <- names(resolved)[sapply(resolved[c("species", "year", "biomass")], is.na)]
  if (length(missing_required) > 0) {
    message("[combine_STAR_RAMlegacy] '", GFCM_STECF_MANUAL_PATH, "' is missing required column(s): ",
            paste(missing_required, collapse = ", "), " (found: ", paste(names(manual_raw), collapse = ", "),
            ") - skipping this source rather than guessing.")
  } else {
    ## Rename whatever alias was actually found to the canonical name,
    ## same resolve-then-setnames pattern as every other loader in
    ## this pipeline (e.g. resolve_iccat_col in 01_biomass.R).
    for (canon in names(resolved)) {
      found <- resolved[[canon]]
      target <- switch(canon, source = "source", stock_key = "stock_key", species = "species",
                       common = "common_name", gsa = "gsa", subregion = "subregion", year = "year",
                       biomass = "biomass", catches = "catches", landings = "landings",
                       citation = "Source_citation")
      if (!is.na(found) && found != target) setnames(manual_raw, found, target)
    }
    if (!"Source_citation" %in% names(manual_raw)) manual_raw[, Source_citation := NA_character_]
    n_no_citation <- manual_raw[is.na(Source_citation) | Source_citation == "", .N]
    if (n_no_citation > 0) {
      message("[combine_STAR_RAMlegacy] WARNING: ", n_no_citation, " row(s) in '", GFCM_STECF_MANUAL_PATH,
              "' have no Source_citation - these are kept (not silently dropped) but should be filled in",
              " before trusting the numbers.")
    }
    ## Drop the obvious example/placeholder row(s) the template ships
    ## with, flagged by stock_key starting with "EXAMPLE" - never
    ## treated as real data.
    n_examples <- manual_raw[grepl("^EXAMPLE", stock_key, ignore.case = TRUE), .N]
    if (n_examples > 0) {
      message("[combine_STAR_RAMlegacy] Excluding ", n_examples, " example/placeholder row(s) from '",
              GFCM_STECF_MANUAL_PATH, "' (stock_key starts with \"EXAMPLE\") - these are template",
              " illustrations, not real assessments.")
      manual_raw <- manual_raw[!grepl("^EXAMPLE", stock_key, ignore.case = TRUE)]
    }
    if (!"source" %in% names(manual_raw) || all(is.na(manual_raw$source))) manual_raw[, source := "GFCM MEDBS/STECF (manual)"]
    if (!"catches" %in% names(manual_raw))  manual_raw[, catches := NA_real_]
    if (!"landings" %in% names(manual_raw)) manual_raw[, landings := NA_real_]
    if (!"gsa" %in% names(manual_raw))      manual_raw[, gsa := NA_character_]
    if (!"subregion" %in% names(manual_raw)) manual_raw[, subregion := NA_character_]
    if (!"common_name" %in% names(manual_raw)) manual_raw[, common_name := NA_character_]
    if (!"stock_key" %in% names(manual_raw) || any(is.na(manual_raw$stock_key))) {
      manual_raw[is.na(stock_key) | stock_key == "", stock_key := paste0("GFCM_STECF_", species, "_", year)]
    }
    manual_raw[, `:=`(year = as.integer(year), biomass = as.numeric(biomass),
                      catches = as.numeric(catches), landings = as.numeric(landings))]
    manual_raw[, landings_flag := !is.na(catches) & !is.na(landings) & (landings > catches)]
    manual_final <- manual_raw[, .(source, stock_key, species, common_name, gsa, subregion, year, biomass,
                                   catches, landings, landings_flag)]
    message("[combine_STAR_RAMlegacy] GFCM MEDBS/STECF manual CSV contributes ", nrow(manual_final),
            " real (non-example) stock x year row(s).")
  }
}


## =================================================================
## STEP 3: Union both sources and write the final output.
## -----------------------------------------------------------------
## Rows are UNIONED, never merged/deduplicated across source - a
## stock covered by both RAM Legacy and the manual CSV for the same
## species/year keeps BOTH rows (different source/stock_key), exactly
## per this script's own header note and the task's own instruction.
## =================================================================
combined <- rbindlist(list(ram_final, manual_final), use.names = TRUE, fill = TRUE)
if (nrow(combined) == 0) {
  message("[combine_STAR_RAMlegacy] WARNING: combined output has ZERO rows (no Mediterranean RAM Legacy",
          " stocks matched, and no usable manual CSV) - writing an empty (header-only) file anyway so",
          " downstream scripts see a consistent, if empty, source rather than a missing file.")
}
fwrite(combined, COMBINED_OUTPUT_PATH)
message("[combine_STAR_RAMlegacy] Wrote ", nrow(combined), " row(s) (", uniqueN(combined$stock_key),
        " distinct stock(s), ", uniqueN(combined$species), " distinct species) to ", COMBINED_OUTPUT_PATH, ".")
message("[combine_STAR_RAMlegacy] Of those, ", combined[grepl("Western Mediterranean", subregion, fixed = TRUE), .N],
        " row(s) contain the literal 'Western Mediterranean' substring in `subregion` and will actually be",
        " picked up by 01_biomass.R/02_fisheries.R's own filter - the rest are written for completeness/",
        " other regional models but are out of scope for THIS model.")
message("[combine_STAR_RAMlegacy] Done. Re-run 01_biomass.R / 02_fisheries.R to pick up this output.")
