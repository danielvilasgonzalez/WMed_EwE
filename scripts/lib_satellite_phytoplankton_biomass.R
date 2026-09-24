## =================================================================
## lib_satellite_phytoplankton_biomass.R
##
## Library file - no top-level driver code, same convention as
## lib_survey_fg_density_functions.R / 03b_ecobase.R. Sourced from
## 01_biomass.R and called directly (fetch_satellite_phytoplankton_
## biomass()) - not meant to run standalone.
##
## Per Andrea (2026-09-24): phytoplankton biomass (Large/Small
## Phytoplankton FGs) should come from SATELLITE data, not from
## EcoBase's OTHER-model biomass values the way the rest of the
## un-sampled lower-trophic FGs do (zooplankton/macroalgae/seagrass/
## gorgonians+corals - see 03b_ecobase.R's fetch_ecobase_literature_
## biomass()). Ocean-colour satellite chlorophyll-a IS a direct,
## Mediterranean-specific measurement of phytoplankton standing stock
## (via a Chl-a -> carbon -> wet-weight conversion), unlike borrowing
## another model's whole-group biomass figure.
##
## The real constraint: satellite ocean-colour missions don't reach
## back to 1994-1996 at all. SeaWiFS (the earliest ocean-colour sensor
## with a continuous open Mediterranean record) launched Sept 1997.
## There is NO way to get an actual satellite MEASUREMENT for
## 1994-1996 - this function uses the EARLIEST available years
## (proxy_years, default 1997-1999) as the closest available proxy,
## exactly like the ACCOBAMS/GOLEM caveats elsewhere in this pipeline
## - never claimed as a real base-year measurement.
##
## fetch_satellite_phytoplankton_biomass(out_dir, force_refresh = FALSE, ...)
##
## Queries an ERDDAP server (NOAA CoastWatch, no login required, unlike
## NASA Earthdata) for gridded chlorophyll-a over the study bounding
## box and proxy_years, averages it, and converts to a Biomass_t per
## Large/Small Phytoplankton FG via a documented, ADJUSTABLE conversion
## chain:
##   Chl-a (mg/m3) --x C_CHL_RATIO--> phytoplankton carbon (mgC/m3)
##                 --x INTEGRATION_DEPTH_M--> areal carbon (mgC/m2, i.e. g/m2 after /1000)
##                 --x WET_WEIGHT_PER_C--> areal wet weight (g/m2 == t/km2 numerically)
##                 --x LARGE_PHYTO_FRACTION / (1 - LARGE_PHYTO_FRACTION)--> split Large/Small
## Every constant in that chain is a named argument with a literature-
## informed DEFAULT and a comment on where it comes from - override any
## of them once you have a Med-specific number instead of a generic one.
##
## THE DATASET ID AND VARIABLE NAME BELOW ARE UNVERIFIED - this session
## has no network access to any ERDDAP server, so DATASET_ID/CHL_VAR
## are best guesses at a real, long-running, no-login ERDDAP mirror of
## SeaWiFS monthly chlorophyll. The function prints the actual columns
## it gets back so a wrong guess is visible immediately rather than
## silently wrong - if the request 404s, browse
## https://coastwatch.pfeg.noaa.gov/erddap/griddap/index.html?searchFor=chlorophyll
## for the current dataset id/variable name and pass them in directly.
##
##   out_dir              - where to write the output CSV (same out_dir
##                           the rest of the pipeline uses)
##   force_refresh         - FALSE (default): reuse the cached CSV if present.
##   bbox                  - named vector c(lon_min, lon_max, lat_min, lat_max),
##                           default the Western Mediterranean.
##   proxy_years           - default 1997:1999 (earliest ocean-colour years
##                           available at all - see header comment above).
##   erddap_base/dataset_id/chl_var - ERDDAP endpoint pieces (see caveat above).
##   c_chl_ratio           - mgC per mg Chl-a, default 50 (Behrenfeld & Falkowski
##                           1997 report a wide natural range, roughly 20-100,
##                           depending on light/nutrient status; 50 is a commonly
##                           used generic mid-range default - replace with a
##                           Mediterranean-specific value if you have one).
##   integration_depth_m   - default 50 m, a typical Western Mediterranean
##                           euphotic-zone depth used to convert a surface
##                           concentration into a water-column-integrated
##                           areal value - replace with a real mixed-layer/
##                           euphotic-depth climatology for the study area
##                           if you have one; this is the single biggest lever
##                           on the final number.
##   wet_weight_per_c      - default 10 (standard EwE rule-of-thumb: wet weight
##                           ~= 10x carbon biomass for phytoplankton).
##   large_phyto_fraction  - default 0.35 (a simplification, NOT the published
##                           Uitz et al. 2006 three-component size-class model -
##                           that model derives microphytoplankton/nanophyto-
##                           plankton/picophytoplankton fractions AS A FUNCTION
##                           of total Chl-a itself, and would be a real
##                           improvement over this fixed split if you want to
##                           implement it later).
##   timeout_sec           - per-request HTTP timeout, default 60.
fetch_satellite_phytoplankton_biomass <- function(out_dir, force_refresh = FALSE,
                                                   bbox = c(lon_min = -2, lon_max = 12, lat_min = 36, lat_max = 44),
                                                   proxy_years = 1997:1999,
                                                   erddap_base = "https://coastwatch.pfeg.noaa.gov/erddap/griddap/",
                                                   dataset_id = "erdSWchlamday",
                                                   chl_var = "chlorophyll",
                                                   c_chl_ratio = 50,
                                                   integration_depth_m = 50,
                                                   wet_weight_per_c = 10,
                                                   large_phyto_fraction = 0.35,
                                                   timeout_sec = 60) {
  out_csv_path <- file.path(out_dir, "satellite_phytoplankton_biomass_by_fg.csv")

  if (!force_refresh && file.exists(out_csv_path)) {
    message("fetch_satellite_phytoplankton_biomass(): using cached ", out_csv_path,
            " (pass force_refresh = TRUE to re-query instead).")
    return(invisible(as.data.table(fread(out_csv_path))))
  }

  if (!requireNamespace("httr", quietly = TRUE)) {
    message("fetch_satellite_phytoplankton_biomass(): 'httr' package is required - skipping.",
            " Install it to enable this step.")
    return(invisible(NULL))
  }

  result <- tryCatch({
    library(httr); library(data.table)

    t0 <- paste0(min(proxy_years), "-01-01")
    t1 <- paste0(max(proxy_years), "-12-31")
    url <- paste0(erddap_base, dataset_id, ".csv?", chl_var,
                 "[(", t0, "):1:(", t1, ")]",
                 "[(", bbox["lat_min"], "):1:(", bbox["lat_max"], ")]",
                 "[(", bbox["lon_min"], "):1:(", bbox["lon_max"], ")]")
    message("Fetching satellite chlorophyll-a from ERDDAP:\n  ", url,
            "\n(", min(proxy_years), "-", max(proxy_years), " - the earliest ocean-colour years available at",
            " all, used here as the closest proxy to 1994-1996, which NO satellite record reaches back to.)")

    resp <- tryCatch(httr::GET(url, httr::timeout(timeout_sec)),
                     error = function(e) {
                       message("Failed to reach ERDDAP endpoint: ", conditionMessage(e))
                       NULL
                     })
    if (is.null(resp) || httr::status_code(resp) != 200) {
      stop("Could not fetch chlorophyll data from '", url, "' (status: ",
           if (!is.null(resp)) httr::status_code(resp) else "no response", "). The dataset_id ('",
           dataset_id, "') or chl_var ('", chl_var, "') may have moved/changed - browse",
           " https://coastwatch.pfeg.noaa.gov/erddap/griddap/index.html?searchFor=chlorophyll",
           " for the current id/variable name and pass them in as arguments.")
    }

    ## ERDDAP .csv responses have 2 header rows (names, then units) -
    ## skip the units row explicitly rather than assuming a fixed layout.
    raw_text <- httr::content(resp, as = "text", encoding = "UTF-8")
    chl_raw <- fread(text = raw_text, skip = 0, header = TRUE)
    chl_raw <- chl_raw[-1]  # drop the units row (row 1 after the header)
    message("\nColumns returned by ERDDAP (verify one of these is really the chlorophyll",
            " concentration, not just time/lat/lon):")
    print(names(chl_raw))

    col_chl <- intersect(c(chl_var, "chlorophyll", "chla", "chlor_a", "CHL1_mean"), names(chl_raw))[1]
    if (is.na(col_chl)) {
      stop("None of the expected chlorophyll column names were found in the ERDDAP response",
           " (columns present: ", paste(names(chl_raw), collapse = ", "), ").")
    }
    chl_values <- as.numeric(chl_raw[[col_chl]])
    chl_values <- chl_values[is.finite(chl_values) & chl_values > 0]
    if (length(chl_values) == 0) {
      stop("No finite, positive chlorophyll values were returned - check the URL/bbox/date",
           " range above directly in a browser.")
    }
    chl_mean_mg_m3 <- mean(chl_values)
    message("\nMean Chl-a over ", length(chl_values), " grid cell x time observation(s), ",
            min(proxy_years), "-", max(proxy_years), ", bbox lon ", bbox["lon_min"], " to ",
            bbox["lon_max"], "/lat ", bbox["lat_min"], " to ", bbox["lat_max"], ": ",
            round(chl_mean_mg_m3, 4), " mg/m3 (range ", round(min(chl_values), 4), "-",
            round(max(chl_values), 4), ").")

    ## Conversion chain (see function header comment for the source/
    ## caveat behind each constant) - g/m2 numerically equals t/km2
    ## (1 t/km2 = 1e6 g / 1e6 m2 = 1 g/m2), so no further unit juggling
    ## is needed once wet weight is expressed per m2.
    total_biomass_t_km2 <- (chl_mean_mg_m3 * c_chl_ratio / 1000) * integration_depth_m * wet_weight_per_c

    citation <- paste0("Satellite ocean-colour chlorophyll-a (ERDDAP ", dataset_id, ", ", chl_var,
                       "), ", min(proxy_years), "-", max(proxy_years), " mean (", round(chl_mean_mg_m3, 3),
                       " mg Chl/m3) - closest available years to 1994-1996 (no ocean-colour sensor reaches",
                       " back that far; SeaWiFS starts Sept 1997) - converted via C:Chl=", c_chl_ratio,
                       ", integration depth=", integration_depth_m, "m, wet weight=", wet_weight_per_c,
                       "xC (all adjustable in fetch_satellite_phytoplankton_biomass()'s arguments).",
                       " Large/Small split is a fixed ", large_phyto_fraction, "/", round(1 - large_phyto_fraction, 2),
                       " fraction (a simplification - Uitz et al. 2006's Chl-based size-class model would",
                       " be more precise if implemented later).")

    satellite_result <- data.table(
      TargetGroup = c("LargePhytoplankton", "SmallPhytoplankton"),
      Biomass_t_km2 = c(total_biomass_t_km2 * large_phyto_fraction,
                        total_biomass_t_km2 * (1 - large_phyto_fraction)),
      Source_citation = citation
    )

    fwrite(satellite_result, out_csv_path)
    message("\nSaved satellite_phytoplankton_biomass_by_fg.csv (", round(total_biomass_t_km2, 4),
            " t/km2 total, split ", large_phyto_fraction, "/", round(1 - large_phyto_fraction, 2),
            " Large/Small).")

    satellite_result

  }, error = function(e) {
    message("fetch_satellite_phytoplankton_biomass(): failed - ", conditionMessage(e),
            ". Continuing without a satellite-derived phytoplankton biomass figure.")
    NULL
  })

  invisible(result)
}
