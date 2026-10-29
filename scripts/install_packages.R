## WMed-EwEbuilder - building Ecopath with Ecosim inputs for the Western Mediterranean
## (biomass, fisheries, P/B, Q/B, traits and diets). This file: install_packages.R
## =================================================================
## install_packages.R
## Created by: Daniel Vilas
##
## Checks and installs every package the pipeline uses, on any laptop
## (macOS, Windows, Linux). run_pipeline_demo.R sources it at the start
## of every run (and so does a run from WMed-EwEbuilder.qmd):
## when everything is installed it only checks (about a second); a new
## laptop gets everything installed before step 01 starts, instead of
## the run stopping halfway on a missing package. Can also be run on its
## own: source("install_packages.R").
##
## - CRAN packages: installed from CRAN (binaries on Windows/macOS).
## - Packages not on CRAN (rfishbase >= 4, duckdbfs, aquamapsdata,
##   ramlegacy): installed from r-universe (binaries, no GitHub login),
##   and if that fails from the GitHub source archive without the GitHub
##   API. remotes::install_github() is NOT used: it sends any GitHub token
##   saved on the laptop, and an expired one fails with "HTTP error 401
##   Bad credentials" even for these public repos (Windows, 2026-10-27).
## - Optional (FishLife) is only reported.
## - R older than 4.3 gets a warning: some packages no longer build
##   binaries for it, so installs fall back to source and can fail.
## =================================================================

wmed_check_packages <- function(quiet = FALSE) {
  say <- function(...) if (!quiet) message("[packages] ", ...)
  ## A run started with Rscript has no CRAN mirror set ("@CRAN@") and
  ## install.packages() would stop asking for one
  repos <- getOption("repos")
  if (is.null(repos) || !length(repos) || any(repos %in% c("@CRAN@", "")))
    options(repos = c(CRAN = "https://cloud.r-project.org"))
  if (getRversion() < "4.3.0")
    warning("[packages] R ", getRversion(), " is old: some packages have no binaries for it any more. ",
            "Install R 4.4 or newer from https://cran.r-project.org if an install below fails.", call. = FALSE)

  have <- rownames(utils::installed.packages())

  ## --- CRAN ----------------------------------------------------------
  cran <- c(
    ## core (01-05)
    "data.table", "readr", "dplyr", "tidyr", "ggplot2", "stringr", "purrr", "tibble", "rlang",
    "openxlsx", "readxl", "scales", "patchwork", "ggrepel", "progress", "png", "zip", "ragg",
    ## spatial
    "sf", "raster", "terra", "marmap", "maps", "rnaturalearth", "rnaturalearthdata", "rmapshaper",
    ## web data and taxonomy
    "httr", "jsonlite", "xml2", "pdftools", "worrms", "emodnet.wfs", "arrow", "duckdb", "ncdf4",
    "DBI", "RSQLite", "dbplyr",
    ## P/B, Q/B
    "TropFishR",
    ## the interactive page and helpers
    "shiny", "knitr", "rmarkdown", "htmltools", "rstudioapi", "getPass", "remotes")
  miss <- setdiff(cran, have)
  if (length(miss)) {
    say("installing ", length(miss), " CRAN package(s): ", paste(miss, collapse = ", "))
    utils::install.packages(miss)
    have <- rownames(utils::installed.packages())
    still <- setdiff(miss, have)
    if (length(still)) warning("[packages] could not install: ", paste(still, collapse = ", "),
                               " - see the messages above.", call. = FALSE)
  }

  ## --- not on CRAN -----------------------------------------------------
  ## name, r-universe, GitHub owner/repo, and a test that the right build is installed
  extra <- list(
    list(pkg = "rfishbase", univ = "https://ropensci.r-universe.dev", gh = "ropensci/rfishbase",
         ok = function() utils::packageVersion("rfishbase") >= "4.0.0"),
    list(pkg = "duckdbfs", univ = "https://cboettig.r-universe.dev", gh = "cboettig/duckdbfs",
         ok = function() exists("duckdb_config", where = asNamespace("duckdbfs"))),
    list(pkg = "aquamapsdata", univ = "https://raquamaps.r-universe.dev", gh = "raquamaps/aquamapsdata",
         ok = function() TRUE),
    list(pkg = "ramlegacy", univ = "https://ropensci.r-universe.dev", gh = "ropensci/ramlegacy",
         ok = function() TRUE))
  for (x in extra) {
    good <- x$pkg %in% have && isTRUE(tryCatch(x$ok(), error = function(e) FALSE))
    if (good) next
    say("installing ", x$pkg, " (not on CRAN, or too old there)...")
    tryCatch(utils::install.packages(x$pkg, repos = c(x$univ, getOption("repos"))),
             error = function(e) say("  r-universe failed: ", conditionMessage(e)))
    good <- isTRUE(tryCatch({ requireNamespace(x$pkg, quietly = TRUE) && x$ok() }, error = function(e) FALSE))
    if (!good) {
      say("  trying the GitHub source archive of ", x$gh, "...")
      tryCatch(remotes::install_url(paste0("https://github.com/", x$gh, "/archive/HEAD.tar.gz"), upgrade = "never"),
               error = function(e) say("  GitHub archive failed: ", conditionMessage(e)))
      good <- isTRUE(tryCatch({ requireNamespace(x$pkg, quietly = TRUE) && x$ok() }, error = function(e) FALSE))
    }
    if (!good) warning("[packages] ", x$pkg, " is still missing or too old. Install it by hand: ",
                       "install.packages('", x$pkg, "', repos = c('", x$univ, "', getOption('repos')))", call. = FALSE)
  }

  ## --- optional ----------------------------------------------------------
  ## FishLife (growth and M from the phylogeny, used in 03 for P/B): tried
  ## once from GitHub; it may need build tools (Rtools on Windows). If it
  ## cannot be installed, 03 uses the FishLife results computed on another
  ## laptop (shared cache in pCloud).
  ## A failed attempt is remembered for 30 days (marker file), so runs do
  ## not retry the GitHub install every time.
  fl_mark <- file.path(tools::R_user_dir("wmed", "cache"), "fishlife_install_failed")
  fl_recent <- file.exists(fl_mark) && as.numeric(difftime(Sys.time(), file.mtime(fl_mark), units = "days")) < 30
  if (!"FishLife" %in% have && !fl_recent) {
    say("installing FishLife from GitHub (James-Thorson-NOAA/FishLife)...")
    ok <- tryCatch({ remotes::install_url("https://github.com/James-Thorson-NOAA/FishLife/archive/HEAD.tar.gz", upgrade = "never")
      requireNamespace("FishLife", quietly = TRUE) }, error = function(e) FALSE)
    if (!isTRUE(ok)) {
      say("FishLife could not be installed here (build tools missing?) - 03 uses the shared FishLife results instead.",
          " Not retried for 30 days (delete ", fl_mark, " to retry).")
      try({ dir.create(dirname(fl_mark), recursive = TRUE, showWarnings = FALSE); writeLines(format(Sys.time()), fl_mark) }, silent = TRUE)
    }
  }
  say("all required packages are installed.")
  invisible(TRUE)
}

wmed_check_packages()
