## --- Reset any logging left over from a previous run in this R session -
## Each script replaces message() with a version that also writes to its
## log file. If R restores an old workspace (.RData) or a previous run
## stopped early, that replacement survives pointing at a CLOSED log
## connection, and the very first message() fails with
## "sink(.run_log_con, split = TRUE): invalid connection". Remove it and
## close any open sinks before anything else runs.
if (exists("message", envir = .GlobalEnv, inherits = FALSE)) rm("message", envir = .GlobalEnv)
while (sink.number() > 0) sink()
## -----------------------------------------------------------------------

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
## This script did not exist before (see README.md "Known gaps" and
## pipeline_documentation.qmd) - it was a documented no-op. This is
## the first implementation.
##
## -----------------------------------------------------------------
## OUTPUT SCHEMA (confirmed against the consuming code - see the
## file:line references below):
##
##   source          chr  short source label (e.g. "RAM Legacy",
##                        "GFCM MEDBS"). Free text, only ever pasted
##                        into a "sources used" string downstream.
##   stock_key       chr  unique stock id (RAM's "stockid", or a
##                        hand-picked key for a manual row). Only used
##                        via uniqueN(stock_key); any unique string works.
##   species         chr  scientific name (Genus species), matched
##                        against fg_lookup_safe/fg_lookup$ScientificName
##                        - exact match first, genus fallback second
##                        (extract_genus() takes the first word, so a
##                        real binomial is required for that fallback).
##   common_name     chr  descriptive only, not consumed downstream.
##   gsa             chr/int  GFCM GSA number/label if known, else NA.
##                        Only carried through joins, never filtered on.
##   subregion       chr  THE FILTER COLUMN - both consuming scripts do
##                        star_ram_combined[grepl("Western Mediterranean",
##                        subregion, fixed = TRUE)] (01_biomass.R ~L1568,
##                        02_fisheries.R ~L3643). A row without that
##                        literal substring is silently out of scope.
##   year            int  calendar year, exactly lowercase `year`
##                        (renamed to Year downstream).
##   biomass         num  stock biomass or SSB, ABSOLUTE TONNES for the
##                        whole assessed stock (not a density, not
##                        per-GSA) - both scripts sum this across every
##                        West Med GSA row, so a stock split across
##                        several GSA rows must already have its total
##                        divided across them (or reported once with
##                        gsa = NA) - never repeat the full value on
##                        every GSA row, which would double-count.
##   catches         num  total catch, absolute tonnes, same convention.
##   landings        num  total landings, absolute tonnes.
##   landings_flag   logi TRUE when landings > catches (Catch = Landings
##                        + Discards >= Landings, so this flags a data
##                        error) - such rows are excluded from the
##                        landings sum but still count toward catches.
##
## Confirmed against: 01_biomass.R L1524-1638 ("Stock-assessment (GFCM
## STAR / RAM Legacy) biomass" block) and 02_fisheries.R L3596-3695
## ("STAR/RAM cross-check" block) - same match cascade in both.
##
## A stock appearing in both the RAM Legacy source and the manual
## GFCM/STECF CSV for the same species/year keeps separate rows (never
## merged here) - the FG x Year aggregation and priority/override logic
## live downstream in 01_biomass.R's merge_manual_cited_biomass().
## =================================================================


## =================================================================
## PART 1 of 2: GFCM STAR / RAM Legacy Stock Assessment Database
## -----------------------------------------------------------------
## The automatable half - RAM Legacy is a real, versioned, bulk-
## downloadable public database, unlike GFCM's/STECF's own SAF/
## assessment reports (Part 2 below).
##
## Uses the `ramlegacy` R package (rOpenSci, on CRAN -
## https://docs.ropensci.org/ramlegacy/): download_ramlegacy() fetches
## the whole database from its Zenodo archive and caches it locally;
## read_ramlegacy() loads it into a named list of tables.
##
## VERSION: v4.44 (see RAM_RDS pattern below) - RAM Legacy gets a new
## Zenodo record/DOI each update, so re-check docs.ropensci.org/
## ramlegacy/ at run time rather than trusting this comment indefinitely.
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

fg_species_file <- resolve_pcloud_file(paste0(pcloud_dir, "/data/FG_WMed_2026.csv"), pcloud_dir)
dataframe2 <- fread(fg_species_file, encoding = "UTF-8")
dataframe2 <- dataframe2[, .(
  ScientificName = species,
  FG_num = FG_number,
  FG_name
)]
fg_lookup_safe <- prepare_fg_lookup(dataframe2)
if (!exists("FISHBASE_TAXONOMY_CACHE_PATH")) FISHBASE_TAXONOMY_CACHE_PATH <- file.path(pcloud_dir, "data/fishbase_taxonomy_cache.csv")
if (!exists("WORMS_TAXONOMY_CACHE_PATH"))    WORMS_TAXONOMY_CACHE_PATH    <- file.path(pcloud_dir, "data/worms_taxonomy_cache.csv")
fg_taxonomy <- fetch_taxonomy(fg_lookup_safe$ScientificName, taxonomy_source = "both",
                              cache_path = FISHBASE_TAXONOMY_CACHE_PATH, worms_cache_path = WORMS_TAXONOMY_CACHE_PATH)
fg_lookup_safe <- merge(fg_lookup_safe, fg_taxonomy, by = "ScientificName", all.x = TRUE)
extract_genus <- function(sci_name) str_extract(sci_name, "^[A-Za-z]+")

STAR_RAMLEGACY_DIR <- file.path(pcloud_dir, "data/fisheries/STAR_RAMLegacy")
if (!dir.exists(STAR_RAMLEGACY_DIR)) dir.create(STAR_RAMLEGACY_DIR, recursive = TRUE)

## --- Run log (plain text, for sharing/debugging) ------------------------
.run_log_path <- file.path(STAR_RAMLEGACY_DIR, paste0(format(Sys.time(), "%Y%m%d_%H%M%S"), "_combine_STAR_RAMlegacy_log.txt"))
.run_log_con  <- file(.run_log_path, open = "wt")
sink(.run_log_con, split = TRUE)  # stdout (cat/print): teed to console + file
## Deliberately NOT sinking the message/stderr stream: sink(type = "message")
## has previously been seen to silently swallow ALL console output, including
## real errors, if anything goes wrong with the redirect - exactly the
## "script just stops, no warning" failure mode. Mirror message() into the
## log file by wrapping the function itself instead, leaving real
## message()/stop() error visibility completely untouched.
.orig_message <- base::message
assign("message", function(..., domain = NULL, appendLF = TRUE) {
  ## Pop the output sink before writing directly to its own connection,
  ## then restore it - writing to a connection that's an ACTIVE split
  ## sink target echoes that write back to the real console too, which
  ## would double-print every message() call there (confirmed by testing).
  sink()
  try(cat(paste0(..., collapse = ""), if (appendLF) "\n" else "",
          sep = "", file = .run_log_con), silent = TRUE)
  try(sink(.run_log_con, split = TRUE), silent = TRUE)  # never let a closed log connection break message()
  .orig_message(..., domain = domain, appendLF = appendLF)
}, envir = .GlobalEnv)
message("[Log] This run's console output is also being written to: ", .run_log_path)

COMBINED_OUTPUT_PATH   <- file.path(STAR_RAMLEGACY_DIR, "combined_medbs_star_ramlegacy.csv")
MATCH_REVIEW_PATH      <- file.path(STAR_RAMLEGACY_DIR, "star_ramlegacy_species_match_review.csv")
RAM_DOWNLOAD_DIR       <- file.path(STAR_RAMLEGACY_DIR, "ram_legacy_cache")

if (!exists("FORCE_RAM_REDOWNLOAD", envir = .GlobalEnv, inherits = FALSE)) FORCE_RAM_REDOWNLOAD <- FALSE

if (!exists("GFCM_STECF_MANUAL_PATH", envir = .GlobalEnv, inherits = FALSE)) {
  GFCM_STECF_MANUAL_PATH <- file.path(pcloud_dir, "data/fisheries/STAR_RAMLegacy/gfcm_stecf_manual_stock_assessments.csv")
}

message("[combine_STAR_RAMlegacy] Output will be written to: ", COMBINED_OUTPUT_PATH)

options(timeout = max(600, getOption("timeout")))

## Resolve the actual FUNCTION OBJECT from the package's namespace, not
## just its name - "download_ramlegacy" etc. aren't necessarily exported,
## so a plain name string can't later be resolved by formals()/do.call()
## via the normal search path (both do their own get(..., mode="function")
## lookup, which doesn't see inside an unattached package namespace).
.resolve_ramlegacy_fn <- function(candidate_names) {
  found_name <- Find(function(f) exists(f, where = asNamespace("ramlegacy"), inherits = FALSE),
                     candidate_names)
  if (is.null(found_name)) return(NULL)
  get(found_name, envir = asNamespace("ramlegacy"), mode = "function")
}
ram_download_fn <- .resolve_ramlegacy_fn(c("download_ramlegacy"))
ram_read_fn <- .resolve_ramlegacy_fn(c("read_ramlegacy", "load_ramlegacy"))
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
ram_download_args <- list(
  overwrite = FORCE_RAM_REDOWNLOAD
)

ram_download_formals <- names(formals(ram_download_fn))

if ("ram_path" %in% ram_download_formals) {
  ram_download_args$ram_path <- RAM_DOWNLOAD_DIR
} else if ("ram_dir" %in% ram_download_formals) {
  ram_download_args$ram_dir <- RAM_DOWNLOAD_DIR
}

do.call(ram_download_fn, ram_download_args)
ram_rds <- list.files(
  RAM_DOWNLOAD_DIR,
  pattern = "^v4\\.44\\.rds$",
  recursive = TRUE,
  full.names = TRUE
)

if (length(ram_rds) != 1) {
  stop(
    "[combine_STAR_RAMlegacy] Expected exactly one RAM Legacy v4.44 RDS file, found ",
    length(ram_rds)
  )
}

ram_data <- ramlegacy::load_ramlegacy(
  version = "4.44",
  ram_path = ram_rds
)
if (!is.list(ram_data) || length(ram_data) == 0) {
  stop("[combine_STAR_RAMlegacy] ramlegacy's own loader returned nothing usable - inspect it manually",
       " (str(ram_data)) before continuing; something about the download/parse likely failed.")
}
message("[combine_STAR_RAMlegacy] RAM Legacy tables loaded: ", paste(names(ram_data), collapse = ", "))

stock_table_name <- names(ram_data)[grepl("^(stock|metadata)$", names(ram_data), ignore.case = TRUE)][1]
if (is.na(stock_table_name)) {
  stop("[combine_STAR_RAMlegacy] Could not find a stock-list table among RAM Legacy's tables (looked for one",
       " named 'stock' or 'metadata'; real names are: ", paste(names(ram_data), collapse = ", "), "). Open",
       " ram_data in the console and find the right one, then set stock_table_name manually above.")
}
ram_stock <- as.data.table(ram_data[[stock_table_name]])
message("[combine_STAR_RAMlegacy] Using '", stock_table_name, "' as the stock-list table (", nrow(ram_stock),
        " stock(s) total). Its columns: ", paste(names(ram_stock), collapse = ", "))

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

if (is.na(col_stock_id) || is.na(col_sci_name) || is.na(col_region)) {
  stop("[combine_STAR_RAMlegacy] '", stock_table_name, "' is missing a required column - found: ",
       paste(names(ram_stock), collapse = ", "), ". Needed a stock-id column (tried ",
       paste(RAM_STOCK_COL_ALIASES$stock_id, collapse = "/"), "), a scientific-name column (tried ",
       paste(RAM_STOCK_COL_ALIASES$sci_name, collapse = "/"), "), and a region/area column (tried ",
       paste(RAM_STOCK_COL_ALIASES$region, collapse = "/"), "). Add the real header spelling to",
       " RAM_STOCK_COL_ALIASES above rather than guessing which column means what.")
}

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

ram_med_stock <- merge(
  ram_med_stock,
  ram_data$area[, c("areaid", "areacode", "areaname", "alternateareaname")],
  by = "areaid",
  all.x = TRUE
)

setnames(ram_med_stock, col_stock_id, "stock_id")
setnames(ram_med_stock, col_sci_name, "sci_name")
if (!is.na(col_common)) setnames(ram_med_stock, col_common, "common_name") else ram_med_stock[, common_name := NA_character_]

ram_med_stock[, gsa := vapply(
  areacode,
  function(x) {
    if (is.na(x)) return(NA_character_)
    x <- gsub("GSA", "", toupper(x))
    parts <- unlist(strsplit(x, "-"))
    vals <- suppressWarnings(as.numeric(parts))
    vals <- floor(vals)
    if (length(vals) == 2 && all(!is.na(vals))) {
      paste(seq(vals[1], vals[2]), collapse = ",")
    } else if (length(vals) >= 1 && any(!is.na(vals))) {
      paste(unique(vals[!is.na(vals)]), collapse = ",")
    } else {
      NA_character_
    }
  },
  character(1)
)]

ram_med_stock[, western_med_gsa := vapply(
  strsplit(gsa, ",", fixed = TRUE),
  function(x) any(suppressWarnings(as.numeric(x)) %in% 1:11),
  logical(1)
)]

n_westmed_text <- sum(grepl("western mediterranean", ram_med_stock$ram_region_raw, ignore.case = TRUE))
message("[combine_STAR_RAMlegacy] Of those, ", n_westmed_text, " stock(s) have region text that itself",
        " contains 'Western Mediterranean'. GSA 1-11 are instead identified from the linked RAM area",
        " table because RAM's stock region field uses 'Mediterranean-Black Sea' rather than GSA-level",
        " Western Mediterranean labels.")

ram_med_stock[, subregion := ifelse(
  western_med_gsa,
  paste0("Western Mediterranean (GSA ", gsa, ")"),
  ram_region_raw
)]

wide_table_name <- names(ram_data)[grepl("^timeseries_values_views$", names(ram_data), ignore.case = TRUE)][1]
long_table_name <- names(ram_data)[grepl("^timeseries$", names(ram_data), ignore.case = TRUE)][1]

RAM_METRIC_COL_ALIASES <- list(
  ssb     = c("SSB", "ssb"),
  tb      = c("TB", "TN", "tb"),
  catch   = c("TC", "tc"),
  landing = c("TL", "landings", "tl")
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

ram_ts_med <- ram_ts_wide[stock_id %in% ram_med_stock$stock_id]

ram_ts_med[, biomass := NA_real_]
if (!is.na(col_ssb)) ram_ts_med[, biomass := as.numeric(get(col_ssb))]
if (!is.na(col_tb))  ram_ts_med[is.na(biomass), biomass := as.numeric(get(col_tb))]
ram_ts_med[, catches  := if (!is.na(col_catch))   as.numeric(get(col_catch))   else NA_real_]
ram_ts_med[, landings := if (!is.na(col_landing)) as.numeric(get(col_landing)) else NA_real_]
ram_ts_med[, landings_flag := !is.na(catches) & !is.na(landings) & (landings > catches)]

ram_final <- merge(ram_ts_med, ram_med_stock[, .(stock_id, sci_name, common_name, gsa, ram_region_raw)],
                   by = "stock_id")
ram_final <- ram_final[!is.na(biomass) | !is.na(catches) | !is.na(landings)]
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
message("[combine_STAR_RAMlegacy] Done. Re-run 01_biomass.R / 02_fisheries.R to pick up this output.")

## --- Close run log --------------------------------------------------------
if (exists(".orig_message", envir = .GlobalEnv, inherits = FALSE)) {
  assign("message", .orig_message, envir = .GlobalEnv)  # undo the message() mirror
  rm(.orig_message, envir = .GlobalEnv)
}
sink()
close(.run_log_con)