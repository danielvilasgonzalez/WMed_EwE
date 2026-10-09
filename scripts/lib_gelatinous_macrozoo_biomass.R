## =================================================================
## lib_gelatinous_macrozoo_biomass.R - LIBRARY FILE, not a pipeline step.
## Sourced by 01b_biomass_unsurveyed.R.
##
## Biomass (t WW per km2 of model area) of three groups no trawl or
## acoustic survey samples:
##   FG "Macro zooplankton"                       <- MAREDAT macrozooplankton
##   FG "Salps and other gelatinous zooplankton"  <- Luo et al. 2020: Chordata (pelagic tunicates) + Ctenophora
##   FG "Jellyfish"                               <- Luo et al. 2020: Cnidaria
## Structure from the macrozoo_salps_jelly.R draft (2026-10-08).
##
## DATA
##   MAREDAT macrozooplankton: Moriarty, Buitenhuis, Le Quere & Gosselin
##     2013, Earth Syst. Sci. Data 5:241-257, doi:10.5194/essd-5-241-2013;
##     data doi:10.1594/PANGAEA.777398 (CC-BY 3.0). Carbon concentration
##     (ug C/L = mg C/m3), 1 x 1 degree, 33 depth levels, monthly
##     climatology of 1926-2010 samples. Taxa: macrozooplankton > 2 mm
##     WITHOUT copepods - includes euphausiids, mysids, amphipods,
##     decapods, pteropods, chaetognaths AND gelatinous taxa (thaliaceans,
##     ctenophores, cnidarians).
##   Luo et al. 2020: Luo, J.Y. et al. 2020, Global Biogeochem. Cycles 34,
##     e2020GB006704, doi:10.1029/2020GB006704; data Zenodo record
##     3891704 (Gridded_Biomass.csv, CC-BY 4.0). Time-averaged biomass
##     (mg C/m3) and density (ind/m3) per 1-degree cell, by phylum:
##     Cnidaria, Ctenophora, Chordata (pelagic tunicates); from JeDI
##     (Lucas et al. 2014, Global Ecol. Biogeogr. 23:701-714) + added
##     surveys.
##   Neither dataset is a 1995 measurement: both are climatologies. Used
##   as the 1994-1996 baseline (ASSUMPTION).
##
## EQUATIONS
##   Depth integration (concentration -> areal), per model area:
##     z_eff(Z) = sum_k A_k min(zmid_k, Z) / sum_k A_k
##     A_k, zmid_k = area and mid-depth of each MEDITS stratum (10-800 m);
##     Z = depth the concentration represents (MAREDAT 350 m: the depth
##     Moriarty et al. 2013 use for their epipelagic estimate; Luo 200 m:
##     ASSUMPTION, epipelagic layer - verify in Luo et al. 2020 Methods).
##     B_C (t C/km2) = c (mg C/m3) x z_eff (m) / 1000
##   Western Med value = area-weighted mean over the 1-degree cells in
##   lon -6..16, lat 35..45 that have data, cell area = R^2 dlat dlon cos(lat).
##   Per cell: MAREDAT median over months and depth levels <= 350 m
##   (Moriarty et al. 2013 use the median because the data are skewed);
##   Luo = the cell value.
##
##   Carbon -> wet weight:
##     Cnidaria, Ctenophora: W = 173.78 C (g), isometric
##     Salps, doliolids: W = 446.68 C^1.54 (g, per individual), fitted on
##       individuals of ~0.03-0.13 g C; outside that range the paper's mean
##       C content of tunicates, 1.04 % of W, is used (the power law gives
##       up to ~9 % C for 1 mg C individuals, which is not realistic)
##     Both: Molina-Ramirez, A. et al. 2015, J. Plankton Res. 37:989-1000,
##       doi:10.1093/plankt/fbv037 (cited as "Lucas et al. 2015" in the draft).
##     Individual C = biomass (mg C/m3) / density (ind/m3).
##     Macrozooplankton (crustacean-dominated): C = MACRO_C_FRACTION_WW of
##       WW, default 0.04 (draft value, ASSUMPTION). Krill alternative:
##       C:DW 0.45 x DW:WW 0.19 (Ikeda & Kirkwood 1989) = 0.086 - halves B.
##
##   Double counting: MAREDAT includes gelatinous taxa, which are separate
##   FGs here. With subtract_gelatinous = TRUE (off by default since the
##   2026-10-08 run: Luo gelatinous carbon, 16 mg C/m3, was 25x the whole
##   MAREDAT macrozooplankton carbon, 0.59 mg C/m3, so the two are not
##   comparable concentrations):
##     c_macro = max(c_MAREDAT - c_Luo(Chordata + Ctenophora + Cnidaria), 0)
##   on the Western Med means (the two datasets do not share cells).
##   If that leaves nothing, the macrozooplankton value is NA and 01b
##   falls back to the krill field minimum.
## =================================================================

suppressPackageStartupMessages(library(data.table))

## 1-degree cell area (km2) on a sphere
.cell_area_km2 <- function(lat, R = 6371) R^2 * (pi / 180)^2 * cos(lat * pi / 180)

## Effective integration depth of the model area for a layer 0-Z m
gel_effective_depth <- function(strata_area_by_area, Z) {
  sa <- as.data.table(strata_area_by_area)
  sa <- sa[!is.na(area_km2) & !is.na(Depth_min_m) & !is.na(Depth_max_m)]
  sa[, sum(area_km2 * pmin((Depth_min_m + Depth_max_m) / 2, Z)) / sum(area_km2)]
}

.download_once <- function(url, dest) {
  if (file.exists(dest) && file.size(dest) > 0) return(TRUE)
  dir.create(dirname(dest), recursive = TRUE, showWarnings = FALSE)
  ok <- tryCatch({ utils::download.file(url, dest, mode = "wb", quiet = TRUE); TRUE },
                 error = function(e) { message("  download failed (", url, "): ", conditionMessage(e)); FALSE })
  ok && file.exists(dest) && file.size(dest) > 0
}

## MAREDAT macrozooplankton: per-cell median carbon (mg C/m3), 0-z_max m
read_maredat_macrozoo_wmed <- function(data_dir, bbox = c(-6, 16, 35, 45), z_max = 350) {
  zip <- file.path(data_dir, "PANGAEA.777398_PDI-1453-1455.zip")
  url <- "https://store.pangaea.de/Publications/ESSD_Special_Issue-MAREDAT/PANGAEA.777398_%28PDI-1453-1455%29.zip"
  nc_files <- list.files(data_dir, "\\.nc$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  if (!length(nc_files)) {
    if (!.download_once(url, zip)) return(NULL)
    utils::unzip(zip, exdir = data_dir)
    nc_files <- list.files(data_dir, "\\.nc$", recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  }
  f <- nc_files[grepl("macro", basename(nc_files), ignore.case = TRUE)][1]
  if (is.na(f)) { message("  MAREDAT: no macrozooplankton NetCDF in ", data_dir); return(NULL) }
  if (!requireNamespace("ncdf4", quietly = TRUE)) stop("package ncdf4 needed for MAREDAT")
  nc <- ncdf4::nc_open(f); on.exit(ncdf4::nc_close(nc))
  vname <- names(nc$var)[grepl("biomass", names(nc$var), ignore.case = TRUE)][1]
  if (is.na(vname)) { message("  MAREDAT: no BIOMASS variable in ", basename(f)); return(NULL) }
  v <- nc$var[[vname]]
  dn <- vapply(v$dim, function(d) toupper(d$name), "")
  dv <- lapply(v$dim, function(d) d$vals)
  arr <- ncdf4::ncvar_get(nc, vname, collapse_degen = FALSE)
  arr[!is.finite(arr) | arr >= 1e30] <- NA
  grid <- do.call(CJ, c(setNames(rev(dv), rev(dn)), sorted = FALSE))   # CJ varies the LAST arg fastest
  setcolorder(grid, dn)
  grid[, c_mgC_m3 := as.vector(arr)]
  lonn <- dn[grepl("LON", dn)][1]; latn <- dn[grepl("LAT", dn)][1]; depn <- dn[grepl("DEP", dn)][1]
  setnames(grid, c(lonn, latn, depn), c("lon", "lat", "depth"))
  grid[lon > 180, lon := lon - 360]
  w <- grid[!is.na(c_mgC_m3) & lon >= bbox[1] & lon <= bbox[2] & lat >= bbox[3] & lat <= bbox[4] & depth <= z_max]
  message("  MAREDAT (", basename(f), ", variable ", vname, "): ", nrow(w), " non-missing values in the Western Med box, ",
          uniqueN(w[, .(lon, lat)]), " cells")
  if (!nrow(w)) return(w[, .(lon, lat, c_mgC_m3, n_obs = integer())])
  w[, .(c_mgC_m3 = stats::median(c_mgC_m3), n_obs = .N), by = .(lon, lat)]
}

## Luo et al. 2020: per-cell carbon and density by phylum
read_luo_gelatinous_wmed <- function(data_dir, bbox = c(-6, 16, 35, 45)) {
  f <- file.path(data_dir, "Gridded_Biomass.csv")
  if (!.download_once("https://zenodo.org/records/3891704/files/Gridded_Biomass.csv?download=1", f)) return(NULL)
  x <- fread(f)
  nm <- tolower(gsub("_$", "", gsub("[^a-z0-9]+", "_", tolower(trimws(names(x))))))
  setnames(x, nm)
  pick <- function(pat, what) {
    k <- nm[grepl(pat, nm)]
    if (length(k) != 1) stop("Luo Gridded_Biomass.csv: expected one ", what, " column, found: ",
                             paste(k, collapse = ", "), " (all: ", paste(nm, collapse = ", "), ")")
    k
  }
  lat_c <- pick("lat", "latitude"); lon_c <- pick("lon", "longitude"); ph_c <- pick("phyl", "phylum")
  b_c <- pick("biomass", "biomass"); d_c <- pick("density|ind", "density")
  x <- x[, .(lat = as.numeric(get(lat_c)), lon = as.numeric(get(lon_c)), phylum = as.character(get(ph_c)),
             c_mgC_m3 = as.numeric(get(b_c)), dens_ind_m3 = as.numeric(get(d_c)))]
  x <- x[lon >= bbox[1] & lon <= bbox[2] & lat >= bbox[3] & lat <= bbox[4] & !is.na(c_mgC_m3) & c_mgC_m3 >= 0]
  message("  Luo et al. 2020: ", nrow(x), " Western Med cell x phylum values (",
          paste(x[, .N, by = phylum][, paste0(phylum, " ", N)], collapse = ", "), ")")
  x
}

## Main: returns a data.table FG_name, biomass_t_km2, + the pieces of each estimate
estimate_gelatinous_macrozoo_biomass <- function(maredat_dir, luo_dir, strata_area_by_area,
                                                 bbox = c(-6, 16, 35, 45),
                                                 maredat_depth_m = 350, luo_depth_m = 200,
                                                 macro_c_fraction_ww = 0.04,
                                                 subtract_gelatinous = FALSE,
                                                 luo_stat = c("median", "mean"),
                                                 carnivore_ww_per_c = 173.78,
                                                 salp_a = 446.68, salp_b = 1.54, salp_c_range_g = c(0.03, 0.13),
                                                 salp_c_fraction_ww = 0.0104) {
  luo_stat <- match.arg(luo_stat)
  z_mar <- gel_effective_depth(strata_area_by_area, maredat_depth_m)
  z_luo <- gel_effective_depth(strata_area_by_area, luo_depth_m)
  out <- list()

  ## --- Luo: gelatinous groups ---------------------------------------
  luo <- read_luo_gelatinous_wmed(luo_dir, bbox)
  gel_c_mean <- NA_real_
  if (!is.null(luo) && nrow(luo)) {
    luo[, area := .cell_area_km2(lat)]
    luo[, c_ind_g := fifelse(!is.na(dens_ind_m3) & dens_ind_m3 > 0, c_mgC_m3 / dens_ind_m3 / 1000, NA_real_)]
    ## wet weight per m3 (g WW/m3)
    luo[, ww_g_m3 := fcase(
      phylum %in% c("Cnidaria", "Ctenophora"), carnivore_ww_per_c * c_mgC_m3 / 1000,
      phylum == "Chordata" & !is.na(c_ind_g) & c_ind_g >= salp_c_range_g[1] & c_ind_g <= salp_c_range_g[2],
        dens_ind_m3 * salp_a * c_ind_g^salp_b,
      phylum == "Chordata", (c_mgC_m3 / 1000) / salp_c_fraction_ww,
      default = NA_real_)]
    luo[, ww_rule := fcase(phylum %in% c("Cnidaria", "Ctenophora"), "W = 173.78 C",
                           phylum == "Chordata" & !is.na(c_ind_g) & c_ind_g >= salp_c_range_g[1] & c_ind_g <= salp_c_range_g[2], "W = 446.68 C^1.54 (per individual)",
                           phylum == "Chordata", "C = 1.04 % of W (outside the power-law range)", default = NA_character_)]
    ## luo_stat = "median" (default): median over cells - the JeDI-based cell
    ## values are strongly right-skewed (bloom and presence-biased records;
    ## Western Med Cnidaria mean 9.4 vs median 5.5 mg C/m3), same reason
    ## Moriarty et al. 2013 use the median for MAREDAT.
    ph <- luo[!is.na(ww_g_m3), .(n_cells = .N,
                                 c_mgC_m3 = if (luo_stat == "median") stats::median(c_mgC_m3) else weighted.mean(c_mgC_m3, area),
                                 ww_g_m3 = if (luo_stat == "median") stats::median(ww_g_m3) else weighted.mean(ww_g_m3, area),
                                 c_mgC_m3_mean = weighted.mean(c_mgC_m3, area),
                                 rules = paste(unique(ww_rule), collapse = "; ")), by = phylum]
    ## g WW/m3 x z_eff m = g WW/m2 = t WW/km2
    ph[, biomass_t_km2 := ww_g_m3 * z_luo]
    gel_c_mean <- ph[phylum %in% c("Chordata", "Ctenophora", "Cnidaria"), sum(c_mgC_m3)]
    sal <- ph[phylum %in% c("Chordata", "Ctenophora")]
    jel <- ph[phylum == "Cnidaria"]
    cite_luo <- paste0("Luo et al. 2020 (Global Biogeochem. Cycles 34:e2020GB006704; Zenodo 3891704, JeDI-based 1-degree ",
                       "climatology, ", paste(bbox, collapse = "/"), " lon/lat box, ", luo_stat, " over cells); C->WW Molina-Ramirez et al. 2015 ",
                       "(J. Plankton Res. 37:989-1000): carnivores W = 173.78 C; tunicates W = 446.68 C^1.54 within ",
                       paste(salp_c_range_g, collapse = "-"), " g C ind-1, else C = 1.04 % W; x z_eff = ", round(z_luo, 1),
                       " m (0-", luo_depth_m, " m layer over the MEDITS strata; ASSUMPTION layer depth). Climatology used as 1994-1996.")
    if (nrow(sal)) out[[length(out) + 1]] <- data.table(
      FG_name = "Salps and other gelatinous zooplankton", biomass_t_km2 = sum(sal$biomass_t_km2),
      detail = paste0(paste0(sal$phylum, " ", signif(sal$biomass_t_km2, 3), " t/km2 (", sal$n_cells, " cells)"), collapse = " + "),
      Source_citation = cite_luo)
    if (nrow(jel)) out[[length(out) + 1]] <- data.table(
      FG_name = "Jellyfish", biomass_t_km2 = jel$biomass_t_km2,
      detail = paste0("Cnidaria ", signif(jel$c_mgC_m3, 3), " mg C/m3 (", jel$n_cells, " cells)"),
      Source_citation = cite_luo)
    attr(out, "luo_by_phylum") <- ph
  }

  ## --- MAREDAT: macrozooplankton ------------------------------------
  mar <- read_maredat_macrozoo_wmed(maredat_dir, bbox, maredat_depth_m)
  if (!is.null(mar) && nrow(mar)) {
    mar[, area := .cell_area_km2(lat)]
    c_mar <- mar[, weighted.mean(c_mgC_m3, area)]
    c_macro <- if (isTRUE(subtract_gelatinous) && is.finite(gel_c_mean)) c_mar - gel_c_mean else c_mar
    b <- if (is.finite(c_macro) && c_macro > 0) c_macro * z_mar / 1000 / macro_c_fraction_ww else NA_real_
    out[[length(out) + 1]] <- data.table(
      FG_name = "Macro zooplankton", biomass_t_km2 = b,
      detail = paste0("MAREDAT ", signif(c_mar, 3), " mg C/m3 (median per cell, ", nrow(mar), " cells, ",
                      sum(mar$n_obs), " values)", if (isTRUE(subtract_gelatinous) && is.finite(gel_c_mean))
                        paste0(" - Luo gelatinous ", signif(gel_c_mean, 3), " mg C/m3") else "",
                      " = ", signif(c_macro, 3), " mg C/m3"),
      Source_citation = paste0("MAREDAT macrozooplankton (Moriarty et al. 2013, Earth Syst. Sci. Data 5:241-257; PANGAEA.777398), ",
                               "1-degree cells in the Western Med box, median 0-", maredat_depth_m, " m, area-weighted",
                               if (isTRUE(subtract_gelatinous)) "; gelatinous carbon (Luo et al. 2020) subtracted to avoid double counting" else "",
                               "; x z_eff = ", round(z_mar, 1), " m (0-", maredat_depth_m, " m layer over the MEDITS strata); C = ",
                               macro_c_fraction_ww * 100, " % of WW (ASSUMPTION; krill 8.6 %). Climatology 1926-2010 used as 1994-1996."))
    attr(out, "maredat_cells") <- mar
  }
  res <- rbindlist(out, fill = TRUE)
  attr(res, "luo_by_phylum") <- attr(out, "luo_by_phylum")
  attr(res, "maredat_cells") <- attr(out, "maredat_cells")
  attr(res, "z_eff") <- c(maredat = z_mar, luo = z_luo)
  res
}
