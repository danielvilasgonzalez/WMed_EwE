## WMed-EwEbuilder - building Ecopath with Ecosim inputs for the Western Mediterranean
## (biomass, fisheries, P/B, Q/B, traits and diets). This file: wmed_paths.R
## =================================================================
## wmed_paths.R
## Created by: Daniel Vilas
##
## Finds the three folders the pipeline needs, on any laptop that has
## the WMed_EwE repo (GitHub) and the pCloud Drive folder
## "EwE Western Med 2026":
##   git_dir    - the WMed_EwE repo (the folder that holds scripts/);
##                always the repo this file sits in
##   pcloud_dir - the shared data folder "EwE Western Med 2026"
##   out_dir    - where the outputs are written (local to each laptop)
##
## Order for pcloud_dir and out_dir (the first folder that EXISTS on this
## computer wins, so a path written for another laptop is skipped):
##   1. config.local.R  - this computer only (git-ignored; written by
##                        medbs_pipeline_methodology.qmd or
##                        run_pipeline_demo.R when you pick a folder)
##   2. config.R        - shared in the repo (Daniel's paths)
##   3. environment variables WMED_PCLOUD_DIR / WMED_OUT_DIR
##   4. standard places: the pCloud Drive mount
##        macOS   ~/pCloud Drive/EwE Western Med 2026
##        Windows P:/EwE Western Med 2026   (pCloud's default drive)
##        Linux   ~/pCloudDrive/EwE Western Med 2026
##      and for out_dir ~/WMed_EwE_output (created if missing)
##
## Used by run_pipeline_demo.R, 01-05 (when run on their own) and
## medbs_pipeline_methodology.qmd.
## =================================================================

.wmed_pcloud_name <- "EwE Western Med 2026"

## This file's own path, taken while it is being source()d
.wmed_self_file <- local({
  self <- NA_character_
  for (i in rev(seq_len(sys.nframe()))) {
    o <- tryCatch(sys.frame(i)$ofile, error = function(e) NULL)
    if (!is.null(o) && grepl("wmed_paths\\.R$", o)) { self <- normalizePath(o); break }
  }
  self
})

## Repo root: the folder above scripts/ (this file lives in scripts/)
.wmed_repo_root <- function() {
  if (!is.na(.wmed_self_file)) return(dirname(dirname(.wmed_self_file)))
  for (d in c(getwd(), dirname(getwd()))) if (file.exists(file.path(d, "scripts", "wmed_paths.R"))) return(normalizePath(d))
  NA_character_
}

## Values of out_dir / pcloud_dir written in one config file
.wmed_read_config <- function(f) {
  e <- new.env()
  ok <- tryCatch({ sys.source(f, envir = e); TRUE }, error = function(err) FALSE)
  if (!ok) return(list(out_dir = NA_character_, pcloud_dir = NA_character_))
  get1 <- function(n) if (exists(n, envir = e, inherits = FALSE)) as.character(get(n, envir = e))[1] else NA_character_
  list(out_dir = get1("out_dir"), pcloud_dir = get1("pcloud_dir"))
}

wmed_find_paths <- function(git_dir = NULL) {
  if (is.null(git_dir) || is.na(git_dir)) git_dir <- .wmed_repo_root()
  git_dir <- sub("/+$", "", path.expand(git_dir))
  cfg_files <- c(file.path(git_dir, "config.local.R"), file.path(git_dir, "scripts", "config.local.R"),
                 file.path(git_dir, "config.R"), file.path(git_dir, "scripts", "config.R"))
  cfg_files <- cfg_files[file.exists(cfg_files)]
  cfg <- lapply(cfg_files, .wmed_read_config)
  pick <- function(name, extra) {
    cand <- c(vapply(cfg, function(x) x[[name]], ""), extra)
    src <- c(basename(cfg_files), names(extra))
    keep <- !is.na(cand) & nzchar(cand)
    cand <- path.expand(cand[keep]); src <- src[keep]
    hit <- which(dir.exists(cand))[1]
    if (is.na(hit)) list(path = NA_character_, from = NA_character_) else list(path = sub("/+$", "", cand[hit]), from = src[hit])
  }
  pc <- pick("pcloud_dir", c(env = Sys.getenv("WMED_PCLOUD_DIR"),
                             `pCloud Drive (macOS)` = file.path("~/pCloud Drive", .wmed_pcloud_name),
                             `pCloud Drive (Windows)` = file.path("P:", .wmed_pcloud_name),
                             `pCloud Drive (Linux)` = file.path("~/pCloudDrive", .wmed_pcloud_name),
                             `pCloud Drive (volume)` = file.path("/Volumes/pCloud Drive", .wmed_pcloud_name)))
  od <- pick("out_dir", c(env = Sys.getenv("WMED_OUT_DIR")))
  if (is.na(od$path)) {
    od <- list(path = file.path(path.expand("~"), "WMed_EwE_output"), from = "default (~/WMed_EwE_output)")
    dir.create(od$path, recursive = TRUE, showWarnings = FALSE)
  }
  ok <- !is.na(git_dir) && file.exists(file.path(git_dir, "scripts", "01_biomass.R")) && !is.na(pc$path) && dir.exists(od$path)
  list(git_dir = git_dir, pcloud_dir = pc$path, out_dir = od$path,
       pcloud_from = pc$from, out_from = od$from, ok = ok,
       problem = if (ok) "" else paste0(
         if (is.na(git_dir) || !file.exists(file.path(git_dir, "scripts", "01_biomass.R"))) "WMed_EwE repo not found. " else "",
         if (is.na(pc$path)) paste0("pCloud folder '", .wmed_pcloud_name, "' not found (is pCloud Drive running?). ",
                                    "Set it in config.local.R or with the environment variable WMED_PCLOUD_DIR. ") else ""))
}

## Sets out_dir / pcloud_dir / git_dir in the global environment (only
## when all three are found). Returns the wmed_find_paths() result.
wmed_set_paths <- function(git_dir = NULL, quiet = FALSE) {
  p <- wmed_find_paths(git_dir)
  if (isTRUE(p$ok)) {
    assign("out_dir", p$out_dir, envir = .GlobalEnv)
    assign("pcloud_dir", p$pcloud_dir, envir = .GlobalEnv)
    assign("git_dir", p$git_dir, envir = .GlobalEnv)
    if (!quiet) message("[wmed_paths] git_dir = ", p$git_dir, "\n[wmed_paths] pcloud_dir = ", p$pcloud_dir,
                        "  (from ", p$pcloud_from, ")\n[wmed_paths] out_dir = ", p$out_dir, "  (from ", p$out_from, ")")
  } else if (!quiet) message("[wmed_paths] ", p$problem)
  invisible(p)
}

## Saves this computer's folders to config.local.R (git-ignored)
wmed_save_local_config <- function(git_dir, out_dir = NULL, pcloud_dir = NULL) {
  f <- file.path(git_dir, "config.local.R")
  old <- if (file.exists(f)) .wmed_read_config(f) else list(out_dir = NA, pcloud_dir = NA)
  out_dir <- if (is.null(out_dir)) old$out_dir else out_dir
  pcloud_dir <- if (is.null(pcloud_dir)) old$pcloud_dir else pcloud_dir
  ## "/" in the saved paths: a Windows path with "\" inside "..." is not valid R
  if (!is.na(out_dir)) out_dir <- gsub("\\\\", "/", out_dir)
  if (!is.na(pcloud_dir)) pcloud_dir <- gsub("\\\\", "/", pcloud_dir)
  writeLines(c("## config.local.R - folders on THIS computer (not committed; see wmed_paths.R).",
               if (!is.na(out_dir)) sprintf('out_dir    <- "%s"', out_dir),
               if (!is.na(pcloud_dir)) sprintf('pcloud_dir <- "%s"', pcloud_dir)), f)
  invisible(f)
}
