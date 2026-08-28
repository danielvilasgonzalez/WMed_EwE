## =============================================================
## export_gsa_boundaries.R
##
## Standalone - does NOT need 01_survey_density_westmed.R or the lib
## file sourced first. Downloads/loads the GSA shapefile exactly the
## same way that script does (same URL, same GSA-numbering fix), then
## exports a simplified GeoJSON small enough to embed in the
## medbs_pipeline_methodology.qmd live map.
## =============================================================

pkgs <- c("sf", "rmapshaper")
new_pkgs <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new_pkgs) > 0) install.packages(new_pkgs)
library(sf)

## Where to put the downloaded shapefile + output GeoJSON. Defaults to
## a "gsa_export" folder in your current working directory - change
## if you'd rather it go somewhere specific.
work_dir <- file.path(getwd(), "gsa_export")
if (!dir.exists(work_dir)) dir.create(work_dir, recursive = TRUE)

## --- identical download/load logic to 01_survey_density_westmed.R -----
gsa_zip_url  <- "https://gfcmsitestorage.blob.core.windows.net/website/5.Data/ArcGIS/GFCM_GSA.zip"
gsa_zip_file <- file.path(work_dir, "GFCM_GSA.zip")
gsa_shp_dir  <- file.path(work_dir, "GFCM_GSA_shp")

if (!dir.exists(gsa_shp_dir)) {
  if (!file.exists(gsa_zip_file)) {
    message("Downloading GFCM GSA shapefile...")
    download.file(gsa_zip_url, destfile = gsa_zip_file, mode = "wb", method = "libcurl")
  }
  unzip(gsa_zip_file, exdir = gsa_shp_dir)
}

gsa_shp_path <- list.files(gsa_shp_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)[1]
if (is.na(gsa_shp_path)) {
  stop("No .shp file found after unzipping - check ", gsa_shp_dir, " manually.")
}

area_shp <- st_read(gsa_shp_path, quiet = TRUE)
area_shp$gsa_num <- as.numeric(area_shp$SMU_CODE)
area_shp$gsa_num[area_shp$gsa_num %in% c(111, 112)] <- 11   # W/E Sardinia fix, same as the survey script
message("Loaded ", nrow(area_shp), " GSA polygon(s), gsa_num range: ",
        min(area_shp$gsa_num, na.rm = TRUE), "-", max(area_shp$gsa_num, na.rm = TRUE))

## --- restrict to the Western Med GSAs this pipeline actually uses -----
## (FILTER_AREAS default in 01_survey_density_westmed.R is 1:11) - keeps
## the exported file smaller and matches what the live map's GSA
## checkboxes actually offer.
area_shp <- area_shp[area_shp$gsa_num %in% 1:11, ]
message(nrow(area_shp), " polygon(s) kept after restricting to GSA 1-11 (Western Med).")

## --- simplify for web embedding -----------------------------------------
## keep = 0.05 retains 5% of vertices - plenty for a small reference map
## at this zoom level. keep_shapes = TRUE stops simplification from
## deleting a whole small polygon outright (matters for e.g. Alboran
## Island, GSA 2, which is tiny).
area_shp_simplified <- rmapshaper::ms_simplify(area_shp, keep = 0.05, keep_shapes = TRUE)

out_path <- file.path(work_dir, "gsa_boundaries.geojson")
st_write(area_shp_simplified, out_path, delete_dsn = TRUE)

size_kb <- round(file.info(out_path)$size / 1024, 1)
message("\nDone. Wrote: ", out_path, " (", size_kb, " KB)")
if (size_kb > 1000) {
  message("That's larger than expected for an embedded webpage file - consider lowering",
          " `keep` above (e.g. 0.02) and re-running.")
}
message("Upload this file (gsa_boundaries.geojson) to continue.")
