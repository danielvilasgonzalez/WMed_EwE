## WMed-EwEbuilder - building Ecopath with Ecosim inputs for the Western Mediterranean
## (biomass, fisheries, P/B, Q/B, traits and diets). This file: install_packages.R
## =================================================================
## install_packages.R
##
## Run this ONCE (or after a fresh R install) before running any of
## the pipeline scripts. Two tiers:
##   1) plain CRAN packages every script's own "install any missing
##      packages" block would eventually pull in anyway - installed
##      here up front so the FIRST real pipeline run doesn't stall
##      partway through on a package prompt.
##   2) three packages that MUST come from GitHub/a dev build, not
##      the CRAN release - 03_pbqb-traits.R checks for these exact
##      version/export requirements itself and stop()s with the same
##      install command if it finds the wrong build, so this section
##      just does that up front instead of failing mid-run.
## =================================================================

## --- 1) CRAN packages -------------------------------------------------
cran_pkgs <- c(
  "data.table", "readr", "dplyr", "tidyr", "ggplot2", "stringr",
  "marmap", "raster", "terra", "sf", "openxlsx", "readxl", "maps",
  "scales", "rnaturalearth", "rnaturalearthdata", "httr", "jsonlite",
  "progress", "patchwork", "purrr", "arrow",
  ## optional but recommended - each gated behind its own
  ## requireNamespace() check in the scripts, with a graceful
  ## skip-with-message if you choose not to install them:
  "TropFishR", "ggrepel", "remotes", "devtools",
  ## used by 01b, 04, the interactive page and the helpers
  "ncdf4", "png", "xml2", "pdftools", "zip", "tibble", "rlang", "worrms", "emodnet.wfs",
  "rmapshaper", "getPass", "rstudioapi", "shiny", "knitr", "rmarkdown"
)
new_cran <- cran_pkgs[!cran_pkgs %in% installed.packages()[, "Package"]]
if (length(new_cran) > 0) {
  message("Installing ", length(new_cran), " CRAN package(s): ", paste(new_cran, collapse = ", "))
  install.packages(new_cran)
} else {
  message("All CRAN packages already installed.")
}

## --- 2) GitHub/dev-build packages - CRAN releases will NOT work ------

## rfishbase >= 4.0.0 - CRAN releases predate the architecture rfishbase
## 4+ needs (pre-4.0 crashes on basic calls like species()).
rfishbase_version <- tryCatch(packageVersion("rfishbase"), error = function(e) NULL)
if (is.null(rfishbase_version) || rfishbase_version < "4.0.0") {
  message("Installing rfishbase from GitHub (ropensci/rfishbase) - CRAN version is too old...")
  if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
  remotes::install_github("ropensci/rfishbase")
} else {
  message("rfishbase ", as.character(rfishbase_version), " already installed (>= 4.0.0, OK).")
}

## duckdbfs with duckdb_config exported - rfishbase 4+ requires this
## export; CRAN's 0.1.0 release lacks it, only the GitHub dev build has it.
duckdbfs_ok <- requireNamespace("duckdbfs", quietly = TRUE) &&
  exists("duckdb_config", where = asNamespace("duckdbfs"))
if (!duckdbfs_ok) {
  message("Installing duckdbfs from GitHub (cboettig/duckdbfs) - CRAN version is missing duckdb_config...")
  if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
  remotes::install_github("cboettig/duckdbfs")
} else {
  message("duckdbfs already installed with duckdb_config exported (OK).")
}

## FishLife - optional but recommended: without it, species with no
## direct FishBase growth studies fall through to the trophic-level-
## only Gascuel fallback instead of FishLife's phylogenetic estimate.
if (!requireNamespace("FishLife", quietly = TRUE)) {
  message("FishLife not installed (optional). To enable the phylogenetic PB/QB fallback:")
  message("  devtools::install_github('james-thorson/FishLife', dep = TRUE)")
} else {
  message("FishLife already installed (OK).")
}

## aquamapsdata - AquaMaps depth envelopes (01 pseudo-strata, 01b habitat
## depth factor). GitHub only. The ~2 GB database itself is downloaded the
## first time 01_biomass.R uses it.
if (!requireNamespace("aquamapsdata", quietly = TRUE)) {
  message("Installing aquamapsdata from GitHub (raquamaps/aquamapsdata)...")
  if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes")
  remotes::install_github("raquamaps/aquamapsdata", dependencies = TRUE)
} else {
  message("aquamapsdata already installed (OK).")
}

## ramlegacy - only needed to rebuild the stock-assessment table
## (combine_STAR_RAMlegacy.R); the pipeline reads the CSV it wrote.
if (!requireNamespace("ramlegacy", quietly = TRUE)) {
  ok <- tryCatch({ install.packages("ramlegacy"); requireNamespace("ramlegacy", quietly = TRUE) }, error = function(e) FALSE)
  if (!isTRUE(ok)) { if (!requireNamespace("remotes", quietly = TRUE)) install.packages("remotes"); remotes::install_github("ropensci/ramlegacy") }
}

## --- 3) Restart reminder ----------------------------------------------
message(
  "\nDone. If rfishbase or duckdbfs were just (re)installed above, RESTART R\n",
  "completely (quit and reopen, not just clear the workspace/environment) before\n",
  "running any pipeline script - both packages have been observed to misbehave\n",
  "if loaded fresh in the same session they were just installed in."
)
