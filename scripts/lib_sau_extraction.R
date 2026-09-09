## =================================================================
## lib_sau_extraction.R - LIBRARY FILE, not a pipeline step.
## Sourced automatically by 02b_fisheries_multisource.R's own STEP 5
## when sau_raw_combined_west_med.csv doesn't exist yet (see
## AUTO_RUN_MISSING_SOURCES there) - do not run this directly, it has
## no top-level driver code of its own besides the function
## definitions below.
##
## This is the SAME per-EEZ extraction logic as sau_west_med_analysis.
## Rmd's own "Part 0: Data extraction" chunk (that Rmd's own comment
## calls it "same logic as sau_west_med_full.R") - lifted out here so
## 02b_fisheries_multisource.R can call it directly instead of you
## having to knit that Rmd (or run sau_west_med_full.R) by hand first
## every time the raw extract is missing. Knitting the Rmd is still
## the richer, documented way to get the SAU side AND the SAU-vs-FAO
## comparison tables/plots in one pass - this file only reproduces the
## download-and-write-the-raw-CSV step, nothing else from that Rmd.
## =================================================================

suppressPackageStartupMessages({
  library(httr)
  library(jsonlite)
  library(dplyr)
})

SAU_BASE_URL <- "https://api.seaaroundus.org/api/v1/"
SAU_UA <- httr::user_agent("sau-west-med-auto-extract/1.0")
SAU_MEASURE <- "tonnage"

## Same EEZ set/region_ids as sau_west_med_analysis.Rmd's own
## WEST_MED_EEZS - kept in sync with that file by hand, since there's
## no shared config source between an .Rmd and a .R file to read this
## from automatically. If you ever add/remove a country or EEZ there,
## mirror the change here too.
SAU_WEST_MED_EEZS <- list(
  "12"  = list(country = "Algeria", area = "Algeria"),
  "788" = list(country = "Tunisia", area = "Tunisia"),
  "947" = list(country = "Morocco", area = "Morocco (Mediterranean)"),
  "918" = list(country = "France", area = "France (Mediterranean)"),
  "899" = list(country = "France", area = "Corsica"),
  "380" = list(country = "Italy", area = "Italy (mainland)"),
  "902" = list(country = "Italy", area = "Sardinia"),
  "901" = list(country = "Italy", area = "Sicily"),
  "962" = list(country = "Spain", area = "Spain (mainland, Med + Gulf of Cadiz)"),
  "903" = list(country = "Spain", area = "Balearic Islands")
)

sau_sanitize_raw_types <- function(df) {
  if ("year" %in% names(df)) df$year <- suppressWarnings(as.integer(as.character(df$year)))
  numeric_like <- grep("tonnes|value|catch|landed", names(df), ignore.case = TRUE, value = TRUE)
  for (col in numeric_like) df[[col]] <- suppressWarnings(as.numeric(as.character(df[[col]])))
  for (col in names(df)) {
    if (is.logical(df[[col]]) && all(is.na(df[[col]]))) df[[col]] <- as.character(df[[col]])
  }
  df
}

sau_readr_or_base_read_csv <- function(path) {
  if (requireNamespace("readr", quietly = TRUE)) {
    readr::read_csv(path, show_col_types = FALSE, progress = FALSE)
  } else {
    utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  }
}

#' Raw row-level CSV extract for one EEZ (SAU's `format=csv` endpoint,
#' returned as a ZIP) - every dimension (sector, gear, taxon, country,
#' area, year, tonnes, landed value...) as a column on one row. Empty
#' tibble (with a message), never an error, on any failure - the
#' per-EEZ loop below keeps going with whatever EEZs DID succeed
#' rather than losing the whole extraction to one bad EEZ.
sau_get_raw_extract <- function(region_id, retries = 3, timeout_s = 180,
                                 year_min = 1994, year_max = 2019) {
  url <- sprintf("%s%s/%s/sector/?format=csv&limit=10&sciname=false&region_id=%s",
                 SAU_BASE_URL, "eez", SAU_MEASURE, region_id)
  resp <- NULL
  for (attempt in seq_len(retries)) {
    resp <- tryCatch(httr::GET(url, SAU_UA, httr::timeout(timeout_s)), error = function(e) e)
    if (!inherits(resp, "error") && httr::status_code(resp) == 200) break
    if (attempt == retries) {
      status_msg <- if (inherits(resp, "error")) conditionMessage(resp) else httr::status_code(resp)
      message(sprintf("  ! failed raw extract for eez %s (%s)", region_id, status_msg))
      return(tibble())
    }
    Sys.sleep(3 * attempt)
  }
  zip_path <- tempfile(fileext = ".zip")
  writeBin(httr::content(resp, as = "raw"), zip_path)
  csv_name <- tryCatch(utils::unzip(zip_path, list = TRUE)$Name[1], error = function(e) NA)
  if (is.na(csv_name)) { unlink(zip_path); return(tibble()) }
  exdir <- tempfile("sau_"); dir.create(exdir)
  utils::unzip(zip_path, files = csv_name, exdir = exdir)
  df <- suppressWarnings(sau_readr_or_base_read_csv(file.path(exdir, csv_name)))
  unlink(zip_path); unlink(exdir, recursive = TRUE)
  df <- sau_sanitize_raw_types(df)
  if ("year" %in% names(df)) df <- df[df$year >= year_min & df$year <= year_max, , drop = FALSE]
  df
}

#' Download SAU's per-EEZ raw extracts for every EEZ in
#' SAU_WEST_MED_EEZS, concatenate them, and write the combined table to
#' csv_path (sau_raw_combined_west_med.csv, normally) - this is the
#' file 02b_fisheries_multisource.R's own SAU_RAW_CSV points at.
#'
#' Returns TRUE and writes csv_path on success; returns FALSE (no file
#' written) if every single EEZ's extract failed - a partial success
#' (some EEZs failed, at least one didn't) still writes whatever came
#' back and returns TRUE, exactly like sau_west_med_analysis.Rmd's own
#' Part 0 chunk does, since a partial SAU extract is still more useful
#' than none and the specific EEZs that failed are reported by name.
sau_download_raw_extract <- function(csv_path, year_min = 1994, year_max = 2019) {
  message("[SAU] Fetching raw per-EEZ extracts from SAU's API (this can take a minute)...")
  raw_frames <- list()
  failed_eez <- character(0)
  for (region_id in names(SAU_WEST_MED_EEZS)) {
    meta <- SAU_WEST_MED_EEZS[[region_id]]
    message(sprintf("  EEZ %s (%s)", region_id, meta$area))
    df <- sau_get_raw_extract(region_id, year_min = year_min, year_max = year_max)
    if (nrow(df) > 0) {
      df$country <- meta$country
      df$area <- meta$area
      raw_frames[[length(raw_frames) + 1]] <- df
    } else {
      failed_eez <- c(failed_eez, sprintf("%s (%s)", region_id, meta$area))
    }
  }

  if (length(raw_frames) == 0) {
    message("[SAU] All per-EEZ raw extracts failed - check internet access to api.seaaroundus.org",
            " and retry. No file written.")
    return(FALSE)
  }

  raw_combined <- dplyr::bind_rows(raw_frames)
  if (requireNamespace("readr", quietly = TRUE)) {
    readr::write_csv(raw_combined, csv_path)
  } else {
    utils::write.csv(raw_combined, csv_path, row.names = FALSE)
  }
  message(sprintf("[SAU] Wrote %s (%d rows, %d of %d EEZs succeeded).",
                   csv_path, nrow(raw_combined), length(raw_frames), length(SAU_WEST_MED_EEZS)))
  if (length(failed_eez) > 0) {
    message("[SAU] NOTE: ", length(failed_eez), " EEZ(s) failed and are NOT included in this extract: ",
            paste(failed_eez, collapse = ", "), " - re-run later if you want another attempt at those",
            " specifically (this function always re-fetches everything, there's no per-EEZ resume).")
  }
  TRUE
}
