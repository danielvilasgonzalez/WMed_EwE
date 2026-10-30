# ============================================================
# WESTERN MEDITERRANEAN EwE ENVIRONMENTAL WORKFLOW
#
# Historical: monthly Copernicus Marine reanalysis
# Future:     SSP2-4.5 and SSP5-8.5, 2021-2040
#
# Outputs:
#   NetCDF/
#   ASCII/Monthly/
#   ASCII/Static_means/
#   ASCII/Monthly_climatology/
#   ASCII/Annual_means/
#   ASCII/Estimated_gaps/
#   Timeseries/
#   Diagnostics/
#
# IMPORTANT:
#   1. Set depth_file below.
#   2. Confirm variable names and units using describe output.
#   3. PP integration requires the actual vertical coordinates
#      and units from the source NetCDF.
# ============================================================

# 1. Packages -------------------------------------------------

packages <- c("terra", "ncdf4", "data.table")

for (p in packages) {
  if (!requireNamespace(p, quietly = TRUE)) {
    install.packages(p)
  }
}

library(terra)
library(ncdf4)
library(data.table)

# 2. USER SETTINGS -------------------------------------------

depth_file <- "C:/YOUR_PATH/depth.asc"  # EDIT THIS

out_dir <- "C:/Copernicus_WMed_EwE"

start_date <- as.Date("1995-01-01")
end_date   <- Sys.Date()

future_start <- as.Date("2021-01-01")
future_end   <- as.Date("2040-12-31")

# Historical biogeochemistry reanalysis starts in 1999.
bgc_start <- as.Date("1999-01-01")

# Set TRUE only if you intentionally want to download the
# complete, very large future scenario archives.
download_full_future_archives <- FALSE

# 3. Folders --------------------------------------------------

folders <- c(
  "NetCDF",
  "ASCII/Monthly",
  "ASCII/Static_means",
  "ASCII/Monthly_climatology",
  "ASCII/Annual_means",
  "ASCII/Estimated_gaps",
  "Timeseries",
  "Diagnostics",
  "Future/SSP245",
  "Future/SSP585"
)

for (d in folders) {
  dir.create(
    file.path(out_dir, d),
    recursive = TRUE,
    showWarnings = FALSE
  )
}

if (!file.exists(depth_file)) {
  stop("Edit depth_file to the full path of your depth ASCII raster.")
}

depth <- terra::rast(depth_file)

if (terra::nlyr(depth) != 1) {
  stop("The reference depth raster must contain one layer.")
}

cat("\nREFERENCE GRID\n")
print(depth)
print(terra::ext(depth))
print(terra::res(depth))

write.csv(
  data.frame(
    nrow = nrow(depth),
    ncol = ncol(depth),
    xmin = xmin(depth),
    xmax = xmax(depth),
    ymin = ymin(depth),
    ymax = ymax(depth),
    xres = res(depth)[1],
    yres = res(depth)[2],
    crs = crs(depth)
  ),
  file.path(out_dir, "Diagnostics", "reference_grid.csv"),
  row.names = FALSE
)

# 4. Copernicus Marine Toolbox -------------------------------

cli <- file.path(
  Sys.getenv("LOCALAPPDATA"),
  "Programs", "Python", "PYTHON~1", "Scripts",
  "copernicusmarine.exe"
)

if (!file.exists(cli)) {
  candidates <- Sys.glob(file.path(
    Sys.getenv("LOCALAPPDATA"),
    "Programs", "Python", "*", "Scripts",
    "copernicusmarine.exe"
  ))
  
  if (length(candidates) > 0) {
    cli <- candidates[1]
  }
}

if (!file.exists(cli)) {
  stop("Could not find copernicusmarine.exe.")
}

# Use the bounding box of the depth raster.
xmin_ref <- xmin(depth)
xmax_ref <- xmax(depth)
ymin_ref <- ymin(depth)
ymax_ref <- ymax(depth)

# 5. Inspect source datasets ---------------------------------

datasets <- c(
  "cmems_mod_med_phy-temp_my_4.2km_P1M-m",
  "cmems_mod_med_phy-sal_my_4.2km_P1M-m",
  "cmems_mod_med_bgc-bio_my_4.2km_P1M-m",
  "cmems_mod_med_bgc-plankton_my_4.2km_P1M-m"
)

for (ds in datasets) {
  cat("\n\n==========================================\n")
  cat("DATASET:", ds, "\n")
  
  result <- system2(
    cli,
    args = c("describe", "--dataset-id", ds),
    stdout = TRUE,
    stderr = TRUE
  )
  
  cat(result, sep = "\n")
  
  writeLines(
    result,
    file.path(
      out_dir, "Diagnostics",
      paste0(ds, "_describe.txt")
    )
  )
}

# 6. Download monthly NetCDF variables ------------------------
#
# Variable names below must be verified against the saved
# describe output. Do not assume bottomT exists.
#
# thetao: temperature
# so:     salinity
# nppv:   primary production
# phyc:   phytoplankton carbon
#
# SST/SSS use a surface-depth subset.
# PP and phyc must retain their complete vertical dimension.

download_cmems <- function(
    dataset, variable, start, end, filename,
    surface_only = FALSE
) {
  
  args <- c(
    "subset",
    "--dataset-id", dataset,
    "--variable", variable,
    "--start-datetime", format(start, "%Y-%m-%d"),
    "--end-datetime", format(end, "%Y-%m-%d"),
    "--minimum-longitude", as.character(xmin_ref),
    "--maximum-longitude", as.character(xmax_ref),
    "--minimum-latitude", as.character(ymin_ref),
    "--maximum-latitude", as.character(ymax_ref),
    "--output-directory", file.path(out_dir, "NetCDF"),
    "--output-filename", filename,
    "--file-format", "netcdf"
  )
  
  if (surface_only) {
    args <- c(
      args,
      "--minimum-depth", "0",
      "--maximum-depth", "1"
    )
  }
  
  log <- system2(
    cli,
    args = args,
    stdout = TRUE,
    stderr = TRUE
  )
  
  status <- attr(log, "status")
  
  if (!is.null(status) && status != 0) {
    cat(log, sep = "\n")
    stop("Copernicus download failed: ", filename)
  }
  
  result_file <- file.path(out_dir, "NetCDF", filename)
  
  if (!file.exists(result_file)) {
    stop(
      "Expected NetCDF was not found: ", result_file,
      "\nCheck the Toolbox output and filename."
    )
  }
  
  result_file
}

# Physical data from 1995
sst_nc <- download_cmems(
  "cmems_mod_med_phy-temp_my_4.2km_P1M-m",
  "thetao", start_date, end_date,
  "SST.nc", surface_only = TRUE
)

sss_nc <- download_cmems(
  "cmems_mod_med_phy-sal_my_4.2km_P1M-m",
  "so", start_date, end_date,
  "SSS.nc", surface_only = TRUE
)

# Full-depth temperature for calculating bottom temperature.
# This file is also used to verify the available vertical levels.
temp3d_nc <- download_cmems(
  "cmems_mod_med_phy-temp_my_4.2km_P1M-m",
  "thetao", start_date, end_date,
  "Temperature_3D.nc"
)

# BGC reanalysis from 1999
pp_nc <- download_cmems(
  "cmems_mod_med_bgc-bio_my_4.2km_P1M-m",
  "nppv", bgc_start, end_date,
  "PP_3D.nc"
)

phyc_nc <- download_cmems(
  "cmems_mod_med_bgc-plankton_my_4.2km_P1M-m",
  "phyc", bgc_start, end_date,
  "Phytoplankton_carbon_3D.nc"
)

# 7. Inspect NetCDF dimensions and units ----------------------

inspect_nc <- function(path) {
  
  nc <- ncdf4::nc_open(path)
  on.exit(ncdf4::nc_close(nc))
  
  cat("\nFILE:", basename(path), "\n")
  cat("VARIABLES:\n")
  
  for (v in names(nc$var)) {
    variable <- nc$var[[v]]
    
    units <- ncdf4::ncatt_get(nc, v, "units")$value
    
    cat(
      v,
      "| units:", if (is.null(units)) "not specified" else units,
      "| dimensions:",
      paste(
        vapply(variable$dim, function(d) d$name, character(1)),
        collapse = ", "
      ),
      "\n"
    )
  }
  
  cat("DIMENSIONS:\n")
  
  for (d in names(nc$dim)) {
    cat(
      d, ":", nc$dim[[d]]$len,
      "| units:", nc$dim[[d]]$units, "\n"
    )
  }
}

for (f in c(sst_nc, sss_nc, temp3d_nc, pp_nc, phyc_nc)) {
  inspect_nc(f)
}

# Stop here if the variable units, time coordinates, or vertical
# coordinates are not clear. Correct PP integration depends on
# those metadata. Do not interpret surface nppv as integrated PP.

# 8. Raster processing helper --------------------------------
#
# Use this only for datasets that terra can read as a monthly
# raster stack with the time dimension in the correct order.
# The time vector must come from the actual NetCDF coordinates.

export_monthly_maps <- function(r, dates, name) {
  
  if (length(dates) != nlyr(r)) {
    stop("Date count does not match raster layers for ", name)
  }
  
  # Match the reference depth grid.
  # Bilinear resampling changes the grid; the output is aligned
  # to the EwE grid, not the original source resolution.
  if (!compareGeom(r, depth, stopOnError = FALSE)) {
    r <- resample(r, depth, method = "bilinear")
  }
  
  r <- mask(r, depth)
  
  for (i in seq_len(nlyr(r))) {
    
    f <- file.path(
      out_dir, "ASCII", "Monthly",
      paste0(name, "_", format(dates[i], "%Y-%m"), ".asc")
    )
    
    writeRaster(
      r[[i]], f,
      filetype = "AAIGrid",
      NAflag = -9999,
      overwrite = TRUE
    )
  }
  
  r
}

# 9. Static maps, climatology, annual means and trends ----------
#
# r: monthly raster stack already aligned to depth.asc
# dates: true dates extracted from the source NetCDF
#
# This function preserves original data and writes separate
# static summaries. It does not itself calculate depth-integrated
# PP or reconstruct missing years.

summarize_hindcast <- function(r, dates, name) {
  
  stopifnot(length(dates) == nlyr(r))
  
  # Overall mean across original available months
  overall <- app(r, mean, na.rm = TRUE)
  
  writeRaster(
    overall,
    file.path(
      out_dir, "ASCII", "Static_means",
      paste0(name, "_overall_mean.asc")
    ),
    filetype = "AAIGrid",
    NAflag = -9999,
    overwrite = TRUE
  )
  
  # Monthly climatology: 12 maps
  for (m in 1:12) {
    
    idx <- which(as.integer(format(dates, "%m")) == m)
    if (!length(idx)) next
    
    clim <- app(r[[idx]], mean, na.rm = TRUE)
    
    writeRaster(
      clim,
      file.path(
        out_dir, "ASCII", "Monthly_climatology",
        paste0(name, "_month_", sprintf("%02d", m), ".asc")
      ),
      filetype = "AAIGrid",
      NAflag = -9999,
      overwrite = TRUE
    )
  }
  
  # Annual mean maps and spatial mean time series
  years <- sort(unique(as.integer(format(dates, "%Y"))))
  annual_series <- data.table()
  
  for (yr in years) {
    
    idx <- which(as.integer(format(dates, "%Y")) == yr)
    if (!length(idx)) next
    
    annual <- app(r[[idx]], mean, na.rm = TRUE)
    
    writeRaster(
      annual,
      file.path(
        out_dir, "ASCII", "Annual_means",
        paste0(name, "_annual_mean_", yr, ".asc")
      ),
      filetype = "AAIGrid",
      NAflag = -9999,
      overwrite = TRUE
    )
    
    annual_series <- rbind(
      annual_series,
      data.table(
        variable = name,
        year = yr,
        spatial_mean = global(annual, "mean", na.rm = TRUE)[1, 1]
      )
    )
  }
  
  fwrite(
    annual_series,
    file.path(
      out_dir, "Timeseries",
      paste0(name, "_annual_timeseries.csv")
    )
  )
  
  # Long-term linear trend in the annual spatial mean
  if (nrow(annual_series) >= 5) {
    
    fit <- lm(spatial_mean ~ year, data = annual_series)
    
    writeLines(
      capture.output(summary(fit)),
      file.path(
        out_dir, "Timeseries",
        paste0(name, "_annual_trend.txt")
      )
    )
  }
  
  # Save monthly anomalies relative to their calendar-month mean.
  # These represent seasonal-adjusted variability, not the trend.
  for (i in seq_len(nlyr(r))) {
    
    m <- as.integer(format(dates[i], "%m"))
    idx <- which(as.integer(format(dates, "%m")) == m)
    
    clim <- app(r[[idx]], mean, na.rm = TRUE)
    anomaly <- r[[i]] - clim
    
    writeRaster(
      anomaly,
      file.path(
        out_dir, "ASCII", "Monthly",
        paste0(name, "_anomaly_", format(dates[i], "%Y-%m"), ".asc")
      ),
      filetype = "AAIGrid",
      NAflag = -9999,
      overwrite = TRUE
    )
  }
  
  invisible(annual_series)
}

# 10. Gap-estimation method -----------------------------------
#
# For each cell, the intended estimate is:
# long-term trend + month-specific climatological effect.
#
# Use this after the correct monthly raster stack and true dates
# have been extracted and aligned. Only gaps should be filled;
# original values remain unchanged.
#
# For PP, use this on depth-integrated PP maps, not on nppv
# concentration layers.

estimate_monthly_gaps <- function(r, dates, target_dates) {
  
  if (length(dates) != nlyr(r)) {
    stop("Dates do not match raster layers.")
  }
  
  out <- list()
  
  for (td in target_dates) {
    
    target_year <- as.integer(format(td, "%Y"))
    target_month <- as.integer(format(td, "%m"))
    
    # Annual trend for this calendar month, per cell
    idx <- which(as.integer(format(dates, "%m")) == target_month)
    
    if (length(idx) < 5) {
      warning("Too few same-month years to fit a reliable trend: ", td)
      next
    }
    
    month_stack <- r[[idx]]
    month_years <- as.integer(format(dates[idx], "%Y"))
    
    # Pixel-wise linear trend; predict for the missing year.
    # This is computationally intensive for a large raster.
    trend_estimate <- app(
      month_stack,
      fun = function(x) {
        ok <- is.finite(x)
        if (sum(ok) < 5) return(NA_real_)
        
        fit <- lm(x[ok] ~ month_years[ok])
        as.numeric(predict(
          fit,
          newdata = data.frame(month_years = target_year)
        ))
      }
    )
    
    out[[format(td, "%Y-%m")]] <- trend_estimate
  }
  
  out
}

# 11. Future scenarios ----------------------------------------
#
# Mediterranean CMIP6 downscaled projections:
# https://zenodo.org/records/10794636
#
# This is a separate archive, not the Copernicus Marine
# historical reanalysis. Each SSP archive is ~35.9 GB.
# Variables include temperature, salinity, chlorophyll,
# oxygen and pH, at specified depth levels.
#
# It does not provide a matching depth-integrated PP product.
# Do not derive PP or phytoplankton carbon directly from
# chlorophyll without a separately justified model.
#
# The following optional block downloads the full archives.
# It is disabled by default to avoid downloading ~72 GB.

if (download_full_future_archives) {
  
  future_url <- c(
    SSP245 = "https://zenodo.org/records/10794636/files/ssp245.zip?download=1",
    SSP585 = "https://zenodo.org/records/10794636/files/ssp585.zip?download=1"
  )
  
  for (sc in names(future_url)) {
    
    zip_file <- file.path(
      out_dir, "Future",
      paste0(sc, ".zip")
    )
    
    download.file(
      future_url[[sc]],
      zip_file,
      mode = "wb",
      method = "libcurl"
    )
    
    unzip(
      zip_file,
      exdir = file.path(out_dir, "Future", sc)
    )
  }
}

# 12. Final inventory -----------------------------------------

writeLines(
  c(
    paste("Depth grid:", depth_file),
    paste("Output directory:", out_dir),
    paste("Historical start:", start_date),
    paste("Historical end:", end_date),
    paste("Future period:", future_start, "to", future_end),
    "Scenarios: SSP2-4.5 and SSP5-8.5",
    "PP must be integrated from the verified vertical profile.",
    "Estimated gaps must be flagged separately from reanalysis."
  ),
  file.path(out_dir, "Diagnostics", "workflow_settings.txt")
)

cat(
  "\nDownloads and diagnostic files created.\n",
  "Review Diagnostics/*_describe.txt and NetCDF units before\n",
  "integrating PP or estimating missing years.\n",
  "Output directory:", out_dir, "\n"
)