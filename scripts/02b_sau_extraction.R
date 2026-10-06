## =================================================================
## 02b_sau_extraction.R
##
## Builds the SAU (Sea Around Us) raw extract + derived Country x FG x
## gear/sector/catch_type tables that 02_fisheries.R's own fleet-split
## and discards logic consumes. Split out into its own source()-able
## script rather than pasted inline into 02_fisheries.R.
##
## source()'d from 02_fisheries.R with local = TRUE, so every object
## this script creates lands in 02_fisheries.R's own environment. Not
## meant to be run standalone (unlike combine_STAR_RAMlegacy.R) - it
## depends on variables 02_fisheries.R has already set up.
##
## Expects already in scope (all set earlier in 02_fisheries.R):
##   SAU_DIR, SAU_RAW_CSV, AUTO_DOWNLOAD_SAU  - SAU path config
##   START_YEAR, END_YEAR, TARGET_COUNTRIES   - study scope
##   fg_lookup                                - species -> FG reference
##                                               (ScientificName/FG_num/FG_name)
##   extract_genus()                          - genus-extraction helper
##   resolve_matches_safely()                 - ambiguous-match resolver
##   species_fg_crosswalk_parts               - list this script appends
##                                               its own "SAU" entry into
##
## Produces (all left in 02_fisheries.R's environment after sourcing):
##   sau_country_fg_gear       - Country x FG x gear, summed across all years
##   sau_country_fg_gear_year  - same, year-resolved (feeds the pre-2014 hindcast)
##   sau_total_cy              - SAU's total reconstructed catch, Country x Year
##   sau_reported_cy           - SAU's "reported" subset only, Country x Year
##   sau_sector_prop           - Country x FG x Sector (Artisanal/Industrial/
##                                Recreational/...) proportions
##   sau_discard_ratio_by_year - Country x FG x Year real SAU-derived discard ratio
## Any of the above may come back as an empty data.table() if SAU_DIR has
## no files and SAU_RAW_CSV doesn't exist either - every downstream
## consumer in 02_fisheries.R already handles that (message + fallback),
## same convention as every other optional source in this pipeline.
## =================================================================

## West Med EEZ -> country/area reference table, read via 02_fisheries.R's
## own read_fisheries_reference()/FISHERIES_REFERENCE_DIR (in scope here
## since this script is source()'d into 02_fisheries.R's environment).
sau_west_med_eezs_dt <- read_fisheries_reference("sau_west_med_eezs.csv",
                                                 required_cols = c("eez_id", "country", "area"))
SAU_WEST_MED_EEZS <- setNames(
  lapply(seq_len(nrow(sau_west_med_eezs_dt)), function(i) {
    list(country = sau_west_med_eezs_dt$country[i], area = sau_west_med_eezs_dt$area[i])
  }),
  as.character(sau_west_med_eezs_dt$eez_id)
)
SAU_BASE_URL <- "https://api.seaaroundus.org/api/v1/"
SAU_UA <- httr::user_agent("sau-west-med-master/1.0")

sau_sanitize_raw_types <- function(df) {
  if ("year" %in% names(df)) df$year <- suppressWarnings(as.integer(as.character(df$year)))  # coerce year to integer
  numeric_like <- grep("tonnes|value|catch|landed", names(df), ignore.case = TRUE, value = TRUE)  # find columns that should be numeric
  for (col in numeric_like) df[[col]] <- suppressWarnings(as.numeric(as.character(df[[col]])))  # coerce them to numeric
  for (col in names(df)) if (is.logical(df[[col]]) && all(is.na(df[[col]]))) df[[col]] <- as.character(df[[col]])  # avoid all-NA logical columns breaking later rbinds
  df
}
sau_readr_or_base_read_csv <- function(path) {
  if (requireNamespace("readr", quietly = TRUE)) readr::read_csv(path, show_col_types = FALSE, progress = FALSE)  # prefer readr if available
  else utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)  # fall back to base read.csv
}
SAU_MEASURE <- "tonnage"
sau_get_raw_extract <- function(region_id, retries = 3, timeout_s = 180, year_min = 1994, year_max = 2019) {
  url <- sprintf("%s%s/%s/sector/?format=csv&limit=10&sciname=false&region_id=%s", SAU_BASE_URL, "eez", SAU_MEASURE, region_id)  # build the SAU API request URL for this EEZ
  resp <- NULL
  for (attempt in seq_len(retries)) {
    resp <- tryCatch(httr::GET(url, SAU_UA, httr::timeout(timeout_s)), error = function(e) e)  # request the CSV export
    if (!inherits(resp, "error") && httr::status_code(resp) == 200) break  # success, stop retrying
    if (attempt == retries) {
      message(sprintf("  ! failed raw extract for eez %s", region_id))
      return(tibble::tibble())  # give up after exhausting retries, return empty
    }
    Sys.sleep(3 * attempt)  # back off before retrying
  }
  zip_path <- tempfile(fileext = ".zip")
  writeBin(httr::content(resp, as = "raw"), zip_path)  # save the downloaded zip to a temp file
  csv_name <- tryCatch(utils::unzip(zip_path, list = TRUE)$Name[1], error = function(e) NA)  # find the CSV's name inside the zip
  if (is.na(csv_name)) { unlink(zip_path); return(tibble::tibble()) }
  exdir <- tempfile("sau_"); dir.create(exdir)  # temp dir to extract into
  utils::unzip(zip_path, files = csv_name, exdir = exdir)  # extract just that CSV
  df <- suppressWarnings(sau_readr_or_base_read_csv(file.path(exdir, csv_name)))  # read the extracted CSV
  unlink(zip_path); unlink(exdir, recursive = TRUE)  # clean up temp files
  df <- sau_sanitize_raw_types(df)  # coerce column types
  if ("year" %in% names(df)) df <- df[df$year >= year_min & df$year <= year_max, , drop = FALSE]  # restrict to the requested year range
  df
}
sau_download_raw_extract <- function(csv_path, year_min = 1994, year_max = 2019) {
  message("[SAU] Fetching raw per-EEZ extracts from SAU's API (this can take a minute)...")
  raw_frames <- list(); failed_eez <- character(0)
  for (region_id in names(SAU_WEST_MED_EEZS)) {
    meta <- SAU_WEST_MED_EEZS[[region_id]]
    message(sprintf("  EEZ %s (%s)", region_id, meta$area))
    df <- sau_get_raw_extract(region_id, year_min = year_min, year_max = year_max)  # download this EEZ's raw extract
    if (nrow(df) > 0) { df$country <- meta$country; df$area <- meta$area; raw_frames[[length(raw_frames) + 1]] <- df }  # tag country/area and collect
    else failed_eez <- c(failed_eez, sprintf("%s (%s)", region_id, meta$area))  # record the failure
  }
  if (length(raw_frames) == 0) { message("[SAU] All per-EEZ raw extracts failed. No file written."); return(FALSE) }
  raw_combined <- dplyr::bind_rows(raw_frames)  # stack all EEZ extracts into one table
  csv_dir <- dirname(csv_path)
  if (!dir.exists(csv_dir)) dir.create(csv_dir, recursive = TRUE, showWarnings = FALSE)  # ensure the output dir exists
  if (requireNamespace("readr", quietly = TRUE)) readr::write_csv(raw_combined, csv_path)  # prefer readr for writing
  else utils::write.csv(raw_combined, csv_path, row.names = FALSE)  # fall back to base write.csv
  message(sprintf("[SAU] Wrote %s (%d rows, %d of %d EEZs succeeded).", csv_path, nrow(raw_combined),
                  length(raw_frames), length(SAU_WEST_MED_EEZS)))
  if (length(failed_eez) > 0) message("[SAU] NOTE: failed EEZ(s): ", paste(failed_eez, collapse = ", "))
  TRUE
}

sau_dir_files <- if (dir.exists(SAU_DIR)) list.files(SAU_DIR, pattern = "\\.csv$", full.names = TRUE, ignore.case = TRUE) else character(0)  # list manually-downloaded SAU CSVs, if any

if (length(sau_dir_files) == 0 && isTRUE(AUTO_DOWNLOAD_SAU) && !file.exists(SAU_RAW_CSV)) {
  sau_download_raw_extract(SAU_RAW_CSV, year_min = START_YEAR, year_max = END_YEAR)  # auto-download as a last resort, if enabled
}

sau_country_fg_gear <- data.table()
sau_country_fg_gear_year <- data.table()
sau_total_cy <- data.table(); sau_reported_cy <- data.table()
sau_sector_prop <- data.table()
sau_discard_ratio_by_year <- data.table()

if (length(sau_dir_files) == 0 && !file.exists(SAU_RAW_CSV)) {
  message("\n[SAU] No CSV file(s) found in '", SAU_DIR, "' (and no legacy '", SAU_RAW_CSV, "' cache) -",
          " the fleet-weighting and unreported-% steps below will be skipped (fleet split falls back to",
          " equal shares across a country's FleetTypes, flagged fleet_split_source; unreported % will be",
          " NA). Manually download SAU's catch-by-EEZ data (seaaroundus.org - open each of Spain/France/",
          " Italy/Morocco/Algeria/Tunisia's EEZ page and use its 'Download data' button for catch by",
          " species/gear/sector/year) into '", SAU_DIR, "', one CSV per country/EEZ is fine - all *.csv",
          " files found there are read and stacked together.")
} else {
  sau_raw <- if (length(sau_dir_files) > 0) {
    message("\n[SAU] Reading ", length(sau_dir_files), " CSV file(s) from '", SAU_DIR, "': ",
            paste(basename(sau_dir_files), collapse = ", "))
    rbindlist(lapply(sau_dir_files, fread), use.names = TRUE, fill = TRUE)  # read and stack all CSVs found in SAU_DIR
  } else {
    message("\n[SAU] '", SAU_DIR, "' has no CSVs - falling back to the legacy combined file '", SAU_RAW_CSV, "'.")
    fread(SAU_RAW_CSV)  # read the legacy single combined file instead
  }
  sau_candidates <- list(gear = c("gear_type", "gear"), sci_name = c("scientific_name"),
                         tonnes = c("tonnes", "catch_sum", "value"), report = c("reporting_status"),
                         sector = c("fishing_sector", "fishing_entity", "sector"),
                         catch_type = c("catch_type"))  # possible column-name variants per required field
  sau_resolved <- list()
  for (field in names(sau_candidates)) for (opt in sau_candidates[[field]]) if (opt %in% names(sau_raw)) { sau_resolved[[field]] <- opt; break }  # find which variant is actually present for each field
  missing_sau_cols <- setdiff(c("gear", "sci_name", "tonnes"), names(sau_resolved))  # required fields with no matching column
  if (length(missing_sau_cols) > 0) stop("[SAU] Raw extract missing required column(s): ", paste(missing_sau_cols, collapse = ", "))
  setnames(sau_raw, unlist(sau_resolved), names(sau_resolved))  # rename resolved columns to standard names
  if (!"report" %in% names(sau_resolved)) sau_raw[, report := NA_character_]  # add a placeholder if no reporting-status column exists
  if (!"sector" %in% names(sau_resolved)) sau_raw[, sector := NA_character_]  # add a placeholder if no fishing-sector column exists (see GEAR_TO_FLEETTYPE's Artisanal-override comment below for why sector needs to travel through sau_country_fg_gear(_year) now, not just sau_sector_prop)
  
  sau_raw <- sau_raw[!is.na(year) & year >= START_YEAR & year <= END_YEAR & country %in% TARGET_COUNTRIES]  # keep only target countries within the study period
  message("\n[SAU] ", nrow(sau_raw), " rows after year/country filter.")
  
  ## species -> FG match on SAU's own scientific names, reusing fg_lookup
  ## built above for the GFCM cascade (direct match, then genus fallback -
  ## a second, lighter cascade than GFCM's, appropriate since SAU already
  ## gives scientific names directly rather than English common names).
  sau_sci <- data.table(ScientificName = unique(sau_raw$sci_name))  # distinct scientific names in the SAU extract
  sau_direct <- merge(sau_sci, fg_lookup[, .(ScientificName, FG_num, FG_name)], by = "ScientificName")  # direct scientific-name match
  sau_direct_safe <- resolve_matches_safely(sau_direct, query_col = "ScientificName")$safe  # keep only unambiguous direct matches
  sau_remaining <- sau_sci[!ScientificName %in% sau_direct_safe$ScientificName]  # names still unmatched
  sau_remaining[, genus := extract_genus(ScientificName)]  # extract genus for the fallback match
  sau_genus <- merge(sau_remaining[!is.na(genus)], fg_lookup[!is.na(genus), .(genus, FG_num, FG_name)], by = "genus", allow.cartesian = TRUE)  # match on genus
  sau_genus_safe <- resolve_matches_safely(sau_genus, query_col = "ScientificName")$safe  # keep only unambiguous genus matches
  sau_species_fg <- rbindlist(list(sau_direct_safe[, .(ScientificName, FG_num, FG_name)],
                                   sau_genus_safe[, .(ScientificName, FG_num, FG_name)]), use.names = TRUE)  # combine direct and genus matches
  message("[SAU] ", nrow(sau_species_fg), " of ", nrow(sau_sci), " distinct SAU species matched to an FG.")
  
  sau_crosswalk <- merge(sau_sci, sau_species_fg, by = "ScientificName", all.x = TRUE)  # every distinct SAU scientific name, matched or not
  species_fg_crosswalk_parts[["SAU"]] <- sau_crosswalk[, .(DataSource = "SAU", RawIdentifier = ScientificName,
                                                           ScientificName, FG_num, FG_name, Matched = !is.na(FG_num))]
  
  sau_raw <- merge(sau_raw, sau_species_fg, by.x = "sci_name", by.y = "ScientificName", all.x = TRUE)  # attach FG to every SAU catch row
  
  ## Country x FG x gear catch, summed across all years - weights the
  ## fleet split below (time-invariant fallback for Country x FG x Year
  ## cells the year-resolved version has no SAU catch for).
  ## sau_gear_sector: SAU's dominant sector (by tonnage) for that
  ## country x gear cell, carried alongside sau_gear so the Artisanal
  ## override below can fire without changing this table's grain.
  dominant_sector <- function(tonnes_vec, sector_vec) {
    tot <- data.table(tonnes = tonnes_vec, sector = sector_vec)[, .(t = sum(tonnes, na.rm = TRUE)), by = sector]
    if (nrow(tot) == 0 || all(is.na(tot$sector))) return(NA_character_)
    tot[which.max(t), sector]
  }
  sau_country_fg_gear <- sau_raw[!is.na(FG_num), .(Catch_t = sum(tonnes, na.rm = TRUE),
                                                   sau_gear_sector = dominant_sector(tonnes, sector)),
                                 by = .(Country = country, FG_num, sau_gear = gear)]  # sum catch by country x FG x gear, across all years
  
  ## Same, but keeping Year - this is what actually lets SAU's real
  ## year-to-year variation feed the pre-2014 hindcast below, instead
  ## of one flat average applied to every year.
  sau_country_fg_gear_year <- sau_raw[!is.na(FG_num), .(Catch_t = sum(tonnes, na.rm = TRUE),
                                                        sau_gear_sector = dominant_sector(tonnes, sector)),
                                      by = .(Country = country, FG_num, sau_gear = gear, Year = year)]  # sum catch by country x FG x gear x year
  
  ## SAU's own reconstructed total vs its "reported" subset - see the
  ## unreported-% step below for how this is used.
  sau_total_cy <- sau_raw[, .(tonnes = sum(tonnes, na.rm = TRUE)), by = .(Country = country, Year = year)]  # SAU's total reconstructed catch by country x year
  if (!is.null(sau_resolved$report)) {
    sau_reported_cy <- sau_raw[grepl("^report", report, ignore.case = TRUE) & !grepl("unreport", report, ignore.case = TRUE),
                               .(tonnes = sum(tonnes, na.rm = TRUE)), by = .(Country = country, Year = year)]  # SAU's "reported" subset only, by country x year
  } else {
    message("[SAU] No reporting-status column in this extract - unreported % will be NA.")
  }
  
  ## Country x FG x Sector (Artisanal/Industrial/Subsistence/Recreational)
  ## proportions - used (a) as an approximate Recreational-catch estimate
  ## GFCM/FDI can't give, and (b) as a cross-check on the Artisanal share
  ## of the fleet split where STECF FDI has no coverage.
  sau_sector_prop <- data.table()
  if (!is.null(sau_resolved$sector)) {
    sau_sector_cy <- sau_raw[!is.na(FG_num), .(Catch_t = sum(tonnes, na.rm = TRUE)),
                             by = .(Country = country, FG_num, Sector = sector)]  # sum catch by country x FG x sector
    sau_sector_prop <- copy(sau_sector_cy)
    sau_sector_prop[, prop_sector := Catch_t / sum(Catch_t), by = .(Country, FG_num)]  # convert to a proportion within each country x FG
    message("[SAU] Sector field found ('", sau_resolved$sector, "') - sector value(s): ",
            paste(sort(unique(sau_sector_prop$Sector)), collapse = ", "),
            ". sau_sector_prop: ", nrow(sau_sector_prop), " Country x FG x Sector row(s).")
  } else {
    message("[SAU] No fishing-sector column in this extract (tried: ", paste(sau_candidates$sector, collapse = ", "),
            ") - Recreational catch stays 'not estimated' and the Artisanal-share cross-check is skipped.")
  }
  
  ## SAU's Landings-vs-Discards split (catch_type field) gives a real
  ## discard ratio (not a proxy), used in the hindcast section to
  ## replace FishMIP's Med-wide ratio for cells STECF FDI doesn't cover.
  sau_discard_ratio_by_year <- data.table()
  if (!is.null(sau_resolved$catch_type)) {
    sau_catch_type_cfy <- sau_raw[!is.na(FG_num) & !is.na(catch_type),
                                  .(tonnes = sum(tonnes, na.rm = TRUE)),
                                  by = .(Country = country, FG_num, Year = year, catch_type)]  # sum catch by country x FG x year x catch_type
    sau_catch_type_cfy[, is_discard := grepl("discard", catch_type, ignore.case = TRUE)]  # flag which catch_type rows are discards
    sau_discard_ratio_by_year <- sau_catch_type_cfy[, .(Discarded_t = sum(tonnes[is_discard], na.rm = TRUE),
                                                        Landed_t    = sum(tonnes[!is_discard], na.rm = TRUE)),
                                                    by = .(Country, FG_num, Year)]  # split each cell into discarded vs landed tonnes
    sau_discard_ratio_by_year[, discard_ratio := Discarded_t / (Landed_t + Discarded_t)]  # compute discard ratio
    sau_discard_ratio_by_year <- sau_discard_ratio_by_year[is.finite(discard_ratio) & discard_ratio >= 0 & discard_ratio <= 0.95,
                                                           .(Country, FG_num, Year, discard_ratio)]  # drop implausible/invalid ratios
    message("[SAU] catch_type field found - sau_discard_ratio_by_year: ", nrow(sau_discard_ratio_by_year),
            " Country x FG x Year row(s) with a real SAU-derived discard ratio.")
  } else {
    message("[SAU] No catch_type column in this extract (tried: 'catch_type') - SAU can't supply a discard",
            " ratio; the discards fallback for non-FDI cells stays FishMIP's Med-wide ratio.")
  }
}