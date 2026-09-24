## =================================================================
## VALIDATION MASTER SCRIPT - ONE SELF-CONTAINED SOURCE FILE.
## Created 2026-09-24, per Andrea's request to start a validation layer
## that checks pipeline inputs/outputs against INDEPENDENT sources,
## rather than trusting the pipeline's own numbers uncritically. Same
## conventions as 01/02/03/04: every source is OPTIONAL and fails soft
## (message + skip, never a hard stop), every comparison is written to
## its own CSV for human review, and every gap is stated explicitly
## rather than silently skipped.
##
## SIX validation checks were asked for. Scope for THIS pass, per
## Andrea's own prioritization (2026-09-24: "GFW should be compare with
## the input ts... FDI for some countries and later SAU maybe"):
##
##   BUILT NOW:
##   1) GFW effort vs. this pipeline's own effort input time series
##      (STECF FDI 2014+ / SAU-hindcasted pre-2014 for Spain/France/
##      Italy, FishMIP for Morocco/Algeria/Tunisia) - SECTION 1 below.
##   2) Comparison against the PREVIOUS WMed model's own biomass
##      figures - now buildable, Andrea gave the file path
##      (FG_WMed_old.xlsx) - SECTION 2 below.
##
##   STUBBED (clear TODO, not yet built - each needs either a dataset
##   Andrea/Daniel still has to locate, or a scope decision):
##   3) Visual census, regional scale - "internal dataset pending to
##      get" (Andrea's own words) - SECTION 3.
##   4) Diet validation (FishBase/EcoBase) inside the diet block -
##      "to rethink" (Andrea's own words, i.e. the approach itself is
##      still undecided, not just the data) - SECTION 4.
##   5) Sector split (industrial/artisanal) - SAU vs. a possible
##      regional Catalan database - SECTION 5.
##   6) RLS (Reef Life Survey) visual census, 2015 only, qualitative -
##      SECTION 6.
## =================================================================

pkgs <- c("data.table", "openxlsx", "httr", "jsonlite")
new_pkgs <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new_pkgs) > 0) install.packages(new_pkgs)
invisible(lapply(pkgs, library, character.only = TRUE))

## =================================================================
## CONFIGURATION - same resolve_config_dir() convention as 01/02/03/04.
## =================================================================
if (!exists("resolve_config_dir")) {
  resolve_config_dir <- function(var_name, hardcoded_value, prompt_title, prompt_message) {
    if (exists(var_name, envir = .GlobalEnv, inherits = FALSE)) {
      val <- get(var_name, envir = .GlobalEnv)
      if (!is.null(val) && is.character(val) && length(val) == 1 && !is.na(val) && val != "" && dir.exists(val)) return(val)
    }
    if (tolower(Sys.info()[["user"]]) == "daniel" && .Platform$OS.type == "unix") return(hardcoded_value)
    stop("This script requires ", var_name, " set manually before running - ", prompt_message)
  }
}
out_dir    <- resolve_config_dir("out_dir", "/Users/daniel/Work/iMARES/WMed EwE Model/output/",
                                 "Select Output Directory", "Please select the directory where output files are saved.")
pcloud_dir <- resolve_config_dir("pcloud_dir", "/Users/daniel/pCloud Drive/EwE Western Med 2026/",
                                 "Select pCloud EwE West Med Directory", "Please select the pCloud Drive/EwE Western Med 2026 folder.")

csv_out_dir       <- file.path(out_dir, "fisheries")
BIOMASS_CSV_DIR   <- file.path(out_dir, "biomass")
VALIDATION_DIR    <- file.path(out_dir, "validation")
if (!dir.exists(VALIDATION_DIR)) dir.create(VALIDATION_DIR, recursive = TRUE)

if (!exists("START_YEAR", envir = .GlobalEnv, inherits = FALSE)) START_YEAR <- 1994
if (!exists("END_YEAR",   envir = .GlobalEnv, inherits = FALSE)) END_YEAR   <- 2023
if (!exists("WESTMED_BBOX", envir = .GlobalEnv, inherits = FALSE)) {
  WESTMED_BBOX <- list(lon_min = -6.0, lon_max = 12.5, lat_min = 34.0, lat_max = 45.0)  # same bbox used throughout 01/03b
}

## =================================================================
## SECTION 1: GFW (Global Fishing Watch) apparent fishing effort vs.
## this pipeline's own effort input time series.
##
## WHAT THIS VALIDATES: a TREND check, not an absolute-magnitude check
## - GFW's AIS-based "apparent fishing hours" and this pipeline's own
## kW-days-based effort are different units measuring different things
## (AIS-detectable vessel-hours vs. engine-power x days-at-sea), so they
## are compared as INDEXED series (each rescaled to its own first
## common year = 1), same "relative, not absolute" convention already
## used for Ecosim_ts's own Biomass/Effort columns (see
## lib_survey_fg_density_functions.R's build_ts_column()).
##
## COVERAGE MISMATCH, stated up front, not papered over: GFW's AIS
## fishing-effort dataset only starts ~2012 (AIS transceivers were not
## required/widespread on most Mediterranean small-scale vessels before
## then) - it CANNOT validate this pipeline's 1994-2011 effort figures
## at all, only whatever of 2012-END_YEAR both series cover. Also: GFW
## is AIS-detected effort only - most Mediterranean artisanal/small-
## scale vessels do not carry AIS at all, so GFW's own coverage is
## itself biased toward industrial/larger vessels, not a ground truth.
##
## REQUIRES a free GFW API token (https://globalfishingwatch.org/our-apis/)
## set as the GFW_API_TOKEN environment variable - fails soft (message +
## skip) if absent, same convention as CMEMS/copernicusmarine credentials
## in lib_cmems_phytoplankton_biomass.R.
## =================================================================
fetch_gfw_effort_by_year <- function(bbox = WESTMED_BBOX, start_year = max(2012, START_YEAR), end_year = END_YEAR,
                                     out_dir = VALIDATION_DIR, force_refresh = FALSE) {
  out_csv <- file.path(out_dir, "gfw_apparent_fishing_effort_by_year.csv")
  if (!force_refresh && file.exists(out_csv)) {
    message("fetch_gfw_effort_by_year(): using cached ", out_csv, " (pass force_refresh = TRUE to re-query GFW).")
    return(fread(out_csv))
  }
  token <- Sys.getenv("GFW_API_TOKEN")
  if (!nzchar(token)) {
    message("\n[GFW validation] GFW_API_TOKEN environment variable not set - skipping. Register for a free",
            " token at https://globalfishingwatch.org/our-apis/ (Stats/4wings API) and set",
            " Sys.setenv(GFW_API_TOKEN = '...') before sourcing this script to enable this check.")
    return(data.table(Year = integer(0), gfw_apparent_fishing_hours = numeric(0)))
  }

  ## GFW's 4wings/report endpoint aggregates apparent fishing effort
  ## (hours) within a polygon/bbox, one call per year (the API's own
  ## date-range aggregation is coarser than a clean per-year total, so
  ## looping one year at a time keeps this simple and auditable, at the
  ## cost of one HTTP call per year - fine for a ~12-year window).
  years <- seq(start_year, end_year)
  results <- vector("list", length(years))
  for (i in seq_along(years)) {
    yr <- years[i]
    url <- "https://gateway.api.globalfishingwatch.org/v3/4wings/report"
    body <- list(
      spatialResolution = "LOW",
      temporalResolution = "YEARLY",
      datasets = list("public-global-fishing-effort:latest"),
      filters = list(),
      region = list(
        dataset = "public-eez-areas",
        geojson = list(
          type = "Polygon",
          coordinates = list(list(
            c(bbox$lon_min, bbox$lat_min), c(bbox$lon_max, bbox$lat_min),
            c(bbox$lon_max, bbox$lat_max), c(bbox$lon_min, bbox$lat_max),
            c(bbox$lon_min, bbox$lat_min)
          ))
        )
      ),
      `date-range` = paste0(yr, "-01-01,", yr, "-12-31")
    )
    resp <- tryCatch(
      httr::POST(url, httr::add_headers(Authorization = paste("Bearer", token), `Content-Type` = "application/json"),
                 body = jsonlite::toJSON(body, auto_unbox = TRUE)),
      error = function(e) { message("[GFW validation] Request failed for ", yr, ": ", conditionMessage(e)); NULL }
    )
    if (is.null(resp) || httr::status_code(resp) != 200) {
      message("[GFW validation] Year ", yr, ": HTTP ", if (!is.null(resp)) httr::status_code(resp) else "no response",
              " - skipping this year (this endpoint/payload shape is UNVERIFIED against the real API in this",
              " sandbox - no outbound access to globalfishingwatch.org was available here. If this fires on the",
              " first real run, check the actual v3 4wings/report request shape against",
              " https://globalfishingwatch.org/our-apis/documentation/docs/api-workflows/analyzing-fishing-effort-in-a-region",
              " and fix the body above - it is a best-effort guess, not a confirmed-working call.")
      next
    }
    parsed <- tryCatch(jsonlite::fromJSON(httr::content(resp, "text", encoding = "UTF-8"), simplifyVector = TRUE),
                       error = function(e) NULL)
    hours_total <- tryCatch(sum(unlist(parsed$entries), na.rm = TRUE), error = function(e) NA_real_)
    results[[i]] <- data.table(Year = yr, gfw_apparent_fishing_hours = hours_total)
  }
  gfw_by_year <- rbindlist(results, fill = TRUE)
  gfw_by_year <- gfw_by_year[!is.na(gfw_apparent_fishing_hours)]
  if (nrow(gfw_by_year) == 0) {
    message("[GFW validation] No usable year returned anything - see per-year messages above.")
    return(gfw_by_year)
  }
  fwrite(gfw_by_year, out_csv)
  message("[GFW validation] gfw_apparent_fishing_effort_by_year.csv: ", nrow(gfw_by_year), " year(s), ",
          min(gfw_by_year$Year), "-", max(gfw_by_year$Year), ".")
  gfw_by_year
}

## --- This pipeline's OWN effort input ts (Andrea: "compare with the
## input ts... FDI for some countries and later SAU maybe") - reads the
## two files 02_fisheries.R already writes, rather than re-deriving
## anything: effort_by_fleettype_eu3_hindcast.csv (Spain/France/Italy -
## STECF FDI's own effort 2014+, SAU-hindcasted 1994-2013) and
## fishing_effort_by_fleet_timeseries_FishMIP.csv (Morocco/Algeria/
## Tunisia - FishMIP nom_active, the only source for these 3 countries).
pipeline_effort_path_eu3    <- file.path(csv_out_dir, "effort_by_fleettype_eu3_hindcast.csv")
pipeline_effort_path_fishmip <- file.path(csv_out_dir, "fishing_effort_by_fleet_timeseries_FishMIP.csv")

pipeline_effort_by_year <- data.table(Year = integer(0), pipeline_effort_kWdays = numeric(0))
eu3_ok <- file.exists(pipeline_effort_path_eu3)
fishmip_ok <- file.exists(pipeline_effort_path_fishmip)
if (!eu3_ok && !fishmip_ok) {
  message("\n[GFW validation] Neither '", pipeline_effort_path_eu3, "' nor '", pipeline_effort_path_fishmip,
          "' exists - run 02_fisheries.R first (this script reads its effort output, never re-derives it).")
} else {
  parts <- list()
  if (eu3_ok) {
    eu3 <- fread(pipeline_effort_path_eu3)
    val_col_eu3 <- intersect(c("Effort_kWdays_total_effective", "Effort_kWdays_total"), names(eu3))[1]
    if (!is.na(val_col_eu3)) {
      parts$eu3 <- eu3[, .(pipeline_effort_kWdays = sum(get(val_col_eu3), na.rm = TRUE)), by = Year]
      message("[GFW validation] Spain/France/Italy effort from '", basename(pipeline_effort_path_eu3), "' ('", val_col_eu3, "').")
    }
  }
  if (fishmip_ok) {
    fm <- fread(pipeline_effort_path_fishmip)
    val_col_fm <- intersect(c("nom_active_kWdays_effective", "nom_active_kWdays", "nom_active"), names(fm))[1]
    if (!is.na(val_col_fm)) {
      parts$fishmip <- fm[, .(pipeline_effort_kWdays = sum(get(val_col_fm), na.rm = TRUE)), by = Year]
      message("[GFW validation] Morocco/Algeria/Tunisia effort from '", basename(pipeline_effort_path_fishmip), "' ('", val_col_fm, "').")
    }
  }
  if (length(parts) > 0) {
    pipeline_effort_by_year <- rbindlist(parts)[, .(pipeline_effort_kWdays = sum(pipeline_effort_kWdays, na.rm = TRUE)), by = Year]
  }
}

gfw_effort_by_year <- fetch_gfw_effort_by_year()

gfw_validation <- merge(pipeline_effort_by_year, gfw_effort_by_year, by = "Year", all = FALSE)
if (nrow(gfw_validation) < 2) {
  message("\n[GFW validation] Fewer than 2 overlapping years between this pipeline's effort ts and GFW",
          " (pipeline: ", nrow(pipeline_effort_by_year), " year(s); GFW: ", nrow(gfw_effort_by_year), " year(s)) -",
          " nothing to compare yet. Most likely cause: GFW_API_TOKEN not set (see message above), or 02_fisheries.R",
          " hasn't been run for this out_dir yet.")
} else {
  setorder(gfw_validation, Year)
  ## Index both series to their FIRST COMMON year = 1 - same
  ## "relative, not absolute" convention as every Ecosim_ts Biomass/
  ## Effort column (build_ts_column()) - absolute magnitudes are not
  ## comparable across these two completely different effort metrics,
  ## but a shared trend (both rising, both falling, diverging) is a
  ## real, meaningful signal.
  gfw_validation[, `:=`(
    pipeline_effort_index = pipeline_effort_kWdays / pipeline_effort_kWdays[1],
    gfw_effort_index       = gfw_apparent_fishing_hours / gfw_apparent_fishing_hours[1]
  )]
  corr <- suppressWarnings(cor(gfw_validation$pipeline_effort_index, gfw_validation$gfw_effort_index, use = "complete.obs"))
  fwrite(gfw_validation, file.path(VALIDATION_DIR, "gfw_effort_validation.csv"))
  message("\n[GFW validation] ", nrow(gfw_validation), " overlapping year(s) (", min(gfw_validation$Year), "-",
          max(gfw_validation$Year), ") - indexed-trend correlation (pipeline effort vs. GFW apparent fishing",
          " hours) = ", round(corr, 3), ". Written to gfw_effort_validation.csv (raw + indexed columns for both",
          " series) - LOOK AT THE PLOT/TABLE, not just this one number: a correlation near 1 across only a few",
          " years is not strong evidence, and GFW cannot see most Mediterranean small-scale/artisanal vessels",
          " (no AIS), so a real divergence there is expected, not necessarily a pipeline error.")
}

## =================================================================
## SECTION 2: comparison against the PREVIOUS WMed model's own biomass
## figures (Andrea: '/Users/daniel/pCloud Drive/EwE Western Med 2026/
## data/Complementary data/FG_WMed_old.xlsx').
##
## Reads the old model's own FG/biomass sheet and this pipeline's
## current Ecopath_B (via fg_missing_ecopath_B_REVIEW.csv's companion
## table - the actual current biomass CSV written by 01_biomass.R,
## survey_fg_annual_index_regional_combined.csv, restricted to
## YEAR_ECOPATH) and compares FG by FG. Matched by FG_name text
## (case-insensitive, trimmed) - NOT by FG_num, since FG numbering is
## not guaranteed stable between the old and new model's own FG
## tables; a name that doesn't match either side is reported, not
## silently dropped.
## =================================================================
OLD_MODEL_PATH <- file.path(pcloud_dir, "data/Complementary data/FG_WMed_old.xlsx")

if (!file.exists(OLD_MODEL_PATH)) {
  message("\n[Old-model comparison] '", OLD_MODEL_PATH, "' not found - skipping.")
} else {
  old_sheets <- readxl::excel_sheets(OLD_MODEL_PATH)
  ## The old workbook's own sheet/column names are UNCONFIRMED against
  ## a real look at this file in this sandbox (no file access here) -
  ## tries the most likely candidates, prints the actual sheet names/
  ## columns found either way so a wrong guess is visible, not silent.
  candidate_sheets <- intersect(c("Ecopath_B", "FG_spp", "Biomass", "EcopathB", "Sheet1"), old_sheets)
  if (length(candidate_sheets) == 0) {
    message("[Old-model comparison] None of the expected sheet names (Ecopath_B/FG_spp/Biomass/EcopathB) found in",
            " '", OLD_MODEL_PATH, "' - actual sheets present: ", paste(old_sheets, collapse = ", "),
            ". Open the file and tell me the right sheet/column names to fix this.")
  } else {
    old_raw <- readxl::read_excel(OLD_MODEL_PATH, sheet = candidate_sheets[1])
    old_raw <- as.data.table(old_raw)
    name_col_old <- intersect(c("FG_name", "Group name", "GroupName", "Name", "Group"), names(old_raw))[1]
    bio_col_old  <- intersect(c("Biomass", "B", "Biomass (t/km2)", "Biomass_t_km2"), names(old_raw))[1]
    if (is.na(name_col_old) || is.na(bio_col_old)) {
      message("[Old-model comparison] Sheet '", candidate_sheets[1], "' found but couldn't identify a",
              " name/biomass column pair - columns present: ", paste(names(old_raw), collapse = ", "),
              ". Tell me the right column names to fix this.")
    } else {
      old_biomass <- old_raw[, .(FG_name_norm = tolower(trimws(get(name_col_old))), old_biomass_t_km2 = as.numeric(get(bio_col_old)))]
      old_biomass <- old_biomass[!is.na(old_biomass_t_km2)]

      new_biomass_path <- file.path(BIOMASS_CSV_DIR, "survey_fg_annual_index_regional_combined.csv")
      if (!file.exists(new_biomass_path)) {
        message("[Old-model comparison] '", new_biomass_path, "' not found - run 01_biomass.R first.")
      } else {
        new_raw <- fread(new_biomass_path)
        new_biomass <- new_raw[Year %in% (if (exists("YEAR_ECOPATH", envir = .GlobalEnv, inherits = FALSE)) YEAR_ECOPATH else 1994:1996),
                               .(new_biomass_t_km2 = mean(mean_density, na.rm = TRUE)), by = .(FG_name_norm = tolower(trimws(FG_name)))]

        model_comparison <- merge(old_biomass, new_biomass, by = "FG_name_norm", all = TRUE)
        model_comparison[, pct_diff := fifelse(!is.na(old_biomass_t_km2) & !is.na(new_biomass_t_km2) & old_biomass_t_km2 > 0,
                                               round(100 * (new_biomass_t_km2 - old_biomass_t_km2) / old_biomass_t_km2, 1), NA_real_)]
        fwrite(model_comparison, file.path(VALIDATION_DIR, "old_model_biomass_comparison.csv"))
        n_only_old <- model_comparison[is.na(new_biomass_t_km2), .N]
        n_only_new <- model_comparison[is.na(old_biomass_t_km2), .N]
        n_big_diff <- model_comparison[!is.na(pct_diff) & abs(pct_diff) > 50, .N]
        message("\n[Old-model comparison] ", nrow(model_comparison), " FG name(s) total - ", n_only_old,
                " only in the OLD model, ", n_only_new, " only in the CURRENT pipeline (name-matching gap or a",
                " genuinely new/removed FG - check by hand), ", n_big_diff, " with a biomass differing by more",
                " than 50% between old and new. Written to old_model_biomass_comparison.csv - positive",
                " pct_diff = current pipeline's biomass is HIGHER than the old model's.")
      }
    }
  }
}

## =================================================================
## SECTION 3: Visual census, regional scale.
## STUB - per Andrea (2026-09-24): "internal dataset pending to get".
## Nothing built here on purpose - there is no source file yet to read.
## Fill in VISUAL_CENSUS_PATH below once that dataset is in hand; the
## comparison itself should follow the exact same shape as SECTION 2
## above (match by FG_name/species, compare density/biomass, %diff,
## write a REVIEW csv) once real data exists.
## =================================================================
message("\n[Visual census validation] STUB - internal dataset not yet available (per Andrea, 2026-09-24).",
        " Set VISUAL_CENSUS_PATH and mirror SECTION 2's comparison pattern once the data is in hand.")

## =================================================================
## SECTION 4: Diet validation (FishBase/EcoBase) inside the diet block.
## STUB - per Andrea (2026-09-24): "to rethink" - i.e. the APPROACH
## itself (not just the data) still needs deciding: e.g. compare
## 04_diets.R's fg_diet output diet-composition percentages against
## EcoBase's own diet-matrix inputs for matching predator groups
## (fetch_ecobase_literature_pb_qb()'s sibling function for diet
## composition doesn't exist yet - would need building), and/or
## against FishBase's own qualitative diet descriptions (much harder
## to turn into a quantitative %diff). Deliberately not started until
## Andrea decides which of these (or something else) is the right
## comparison to build.
## =================================================================
message("\n[Diet validation] STUB - approach still to be decided (per Andrea, 2026-09-24: 'to rethink').",
        " Candidates: 04_diets.R's fg_diet vs. EcoBase's own diet-matrix inputs (needs a new EcoBase diet",
        " fetcher, not yet built); vs. FishBase's qualitative diet descriptions (harder to quantify).")

## =================================================================
## SECTION 5: sector split (industrial/artisanal) validation - SAU vs.
## possibly a regional Catalan database.
## STUB - Andrea's own note flags this as an open question ("maybe it
## is worth it to get regional catalan database to validate regional
## catches?"), not yet a decision. SAU's own Industrial/Artisanal/
## Recreational sector proportions are already used as an INPUT in
## 02_fisheries.R (not a validation source there) - using it again
## here as its own validation check would be circular for whatever
## cells 02_fisheries.R already took SAU's sector split FROM. A
## genuinely independent regional Catalan fisheries database (if one
## with a sector breakdown exists and covers the right species/years)
## would be the real, non-circular check - not yet located/confirmed.
## =================================================================
message("\n[Sector-split validation] STUB - SAU vs. SAU would be circular wherever 02_fisheries.R already used",
        " SAU's own sector split as an input; a genuinely independent regional Catalan database is the right",
        " check but hasn't been located/confirmed yet (per Andrea, 2026-09-24).")

## =================================================================
## SECTION 6: RLS (Reef Life Survey) visual census, 2015 only.
## STUB - Andrea's own note: qualitative only, single year (2015), so
## this can only ever be a "does the model's relative FG ranking/
## presence agree with what RLS actually observed that one year"
## sanity check, never a quantitative biomass %diff like SECTION 2.
## RLS's own public data portal (https://reeflifesurvey.com/data/) is
## the real source once/if a West Med RLS transect subset is confirmed
## to exist - not yet checked this session.
## =================================================================
message("\n[RLS validation] STUB - qualitative-only by nature (2015, single year) - would compare presence/",
        "relative-abundance RANKING against the model's FGs, not a biomass %diff. RLS data portal:",
        " https://reeflifesurvey.com/data/ - not yet checked whether it has a usable West Med subset.")

message("\n[05_validation.R] Done. Outputs in ", VALIDATION_DIR, ".")
