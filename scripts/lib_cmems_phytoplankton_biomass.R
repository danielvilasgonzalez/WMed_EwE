## =================================================================
## lib_cmems_phytoplankton_biomass.R
##
## Library file - sourced from 01_biomass.R, called directly
## (fetch_cmems_phytoplankton_biomass()) - not meant to run standalone.
##
## 2026-09-24, per Andrea: phytoplankton (Large/SmallPhytoplankton FGs)
## should come from a Mediterranean BIOGEOCHEMICAL MODEL, not a
## chlorophyll-a-to-carbon conversion off satellite ocean colour (the
## previous approach, still kept in lib_satellite_phytoplankton_biomass.R
## as a fallback - see below). The Copernicus Marine Service's
## Mediterranean Sea Biogeochemistry Reanalysis (MEDSEA_MULTIYEAR_BGC_
## 006_008, built on the MedBFM/OGSTM-BFM model) is exactly this - and
## it reports phytoplankton biomass DIRECTLY as carbon (variable "phyc",
## "mole concentration of phytoplankton expressed as carbon in sea
## water"), so this is a real model estimate, not another conversion
## chain on top of a proxy measurement.
##
## THE CATCH (same "closest available, not a real 1994-1996 measurement"
## situation as every other source in this pipeline): the reanalysis
## only covers January 1999 onward - there is no biogeochemical model
## run publicly available for 1994-1996 itself. proxy_years (default
## 1999-2001, the earliest available) is used as the hindcast proxy -
## genuinely a "closest we can get", not a backward-extrapolated
## 1994-1996 estimate. If you have access to a longer/older MedBFM run
## (some research groups hold pre-1999 hindcasts that were never
## released as a CMEMS product), point dataset_id at that instead - the
## function doesn't otherwise care where the NetCDF variable comes from.
##
## NO ZOOPLANKTON BIOMASS VARIABLE in this product (confirmed from the
## product's own documentation: it lists phytoplankton carbon, net
## primary production, chlorophyll, nutrients, oxygen and the carbon
## system - no meso-/microzooplankton state variable is released,
## even though the underlying BFM model simulates zooplankton
## internally). Zooplankton FGs (MacroZooplankton/MesoMicroZooplankton)
## THEREFORE STILL GO THROUGH EcoBase (fetch_ecobase_literature_biomass()
## in 03b_ecobase.R, already wired into 01_biomass.R) - flagged here so
## this isn't mistaken for an oversight; ask CMCC/OGS (the group that
## runs MedBFM) directly if you want their model's own internal
## zooplankton output, since it isn't in the public CMEMS catalogue.
##
## ACCESS: unlike ERDDAP/EcoBase (anonymous HTTP), Copernicus Marine
## data requires a FREE account (register at data.marine.copernicus.eu)
## and the `copernicusmarine` command-line toolbox (`pip install
## copernicusmarine`, then `copernicusmarine login` once to store
## credentials). This function shells out to that CLI's `subset`
## command and reads the resulting NetCDF with the `ncdf4` package -
## both checked for and reported clearly if missing, same graceful-
## degradation philosophy as every other external-data function in
## this pipeline (never a stop() that would take down the whole run).
##
## THE DATASET ID BELOW IS UNVERIFIED - CMEMS dataset ids are per
## variable-group/frequency ("layer"), not the top-level product id
## (MEDSEA_MULTIYEAR_BGC_006_008 is the PRODUCT; the actual downloadable
## dataset for the "plankton" variable group is typically named
## something like "med-ogs-plankton-rean-monthly", but the exact string
## depends on Copernicus Marine's current catalogue and could not be
## confirmed from this session - no network access here to either the
## Copernicus Marine catalogue API or to run copernicusmarine itself).
## Run `copernicusmarine describe --contains phyc` (or browse
## https://data.marine.copernicus.eu/product/MEDSEA_MULTIYEAR_BGC_006_008)
## to get the real dataset id/variable name and pass them in directly if
## the default below 404s or the variable isn't found in the output.
## ensure_copernicusmarine_cli(auto_install = TRUE)
##
## 2026-09-24, per Andrea ("how can i automate that?"): the ONLY genuinely
## manual step left is creating a free Copernicus Marine account (a
## real-world signup this code can't do for you - register at
## data.marine.copernicus.eu). Everything downstream of having an
## account is now automated:
##   - Installing the CLI: if 'copernicusmarine' isn't on PATH, this
##     shells out to 'pip install copernicusmarine' itself (set
##     auto_install = FALSE to disable and just get the message instead).
##   - Authenticating: rather than the interactive 'copernicusmarine
##     login', ensure_copernicusmarine_credentials() below checks for
##     COPERNICUSMARINE_SERVICE_USERNAME/COPERNICUSMARINE_SERVICE_PASSWORD
##     as environment variables first, and if they're not set, POPS UP an
##     RStudio dialog (rstudioapi::showPrompt / askForPassword - a real
##     "Enter your username" / "Enter your password" window, password
##     masked) asking for them ONCE. Whatever you type gets written to
##     ~/.Renviron (so future R sessions have it automatically, no
##     restart needed for the rest of THIS session) and used right away
##     for this run. If R isn't running inside RStudio (no pop-up
##     available - e.g. Rscript from a terminal), it falls back to
##     console prompts (masked via the 'getPass' package if installed).
ensure_copernicusmarine_cli <- function(auto_install = TRUE) {
  if (Sys.which("copernicusmarine") != "") return(TRUE)
  if (!auto_install) return(FALSE)
  message("'copernicusmarine' not found on PATH - attempting to install it now via pip",
          " ('pip install copernicusmarine')...")
  pip_bin <- if (Sys.which("pip3") != "") "pip3" else if (Sys.which("pip") != "") "pip" else NA
  if (is.na(pip_bin)) {
    message("Neither 'pip3' nor 'pip' found on PATH either - can't auto-install. Install Python/pip",
            " first, or install copernicusmarine manually.")
    return(FALSE)
  }
  install_out <- system2(pip_bin, c("install", "copernicusmarine"), stdout = TRUE, stderr = TRUE)
  
  if (Sys.which("copernicusmarine") == "") {
    ## 2026-09-24, per Andrea's real run: pip frequently installs to a
    ## per-user "user site" bin directory (e.g. macOS:
    ## ~/Library/Python/3.X/bin, Linux: ~/.local/bin) that isn't on PATH
    ## by default - pip even warns about this in its own output ("WARNING:
    ## The script copernicusmarine is installed in '...' which is not on
    ## PATH"). Rather than give up, ask Python itself where its user-site
    ## bin directory is and add it to PATH for the rest of THIS R session
    ## if the binary is actually there.
    py_bin <- if (Sys.which("python3") != "") "python3" else if (Sys.which("python") != "") "python" else NA
    if (!is.na(py_bin)) {
      user_base <- tryCatch(system2(py_bin, c("-m", "site", "--user-base"), stdout = TRUE),
                            error = function(e) character(0))
      if (length(user_base) > 0 && nzchar(trimws(user_base[1]))) {
        candidate_bin <- file.path(trimws(user_base[1]), "bin")
        candidate_exe <- file.path(candidate_bin, "copernicusmarine")
        if (file.exists(candidate_exe)) {
          Sys.setenv(PATH = paste(candidate_bin, Sys.getenv("PATH"), sep = .Platform$path.sep))
          message("Found 'copernicusmarine' in the pip user-install bin directory (", candidate_bin,
                  ") - added to PATH for this R session. (To avoid this check on every run, add '",
                  candidate_bin, "' to your PATH permanently, e.g. in ~/.zshrc or ~/.bash_profile.)")
        }
      }
    }
  }
  
  if (Sys.which("copernicusmarine") == "") {
    message("Auto-install did not put 'copernicusmarine' on PATH (and it wasn't found in the pip",
            " user-install bin directory either). pip output:\n",
            paste(install_out, collapse = "\n"),
            "\n(Check where pip installed it, e.g. 'python3 -m site --user-base', and add its bin/",
            " subfolder to PATH yourself.)")
    return(FALSE)
  }
  message("copernicusmarine ready (installed successfully and resolvable on PATH).")
  TRUE
}

## Checks for COPERNICUSMARINE_SERVICE_USERNAME/PASSWORD as env vars first
## (already-set env vars, or a value already saved to ~/.Renviron and
## picked up at R startup). If they're missing, pops up an RStudio dialog
## (or falls back to console prompts outside RStudio) asking for them
## once, then saves the answer to ~/.Renviron so this never happens again.
## Returns list(username=, password=), or NULL if the user cancels/leaves
## a field blank (in which case the caller proceeds with no credentials -
## which still works if 'copernicusmarine login' was run by hand before).
ensure_copernicusmarine_credentials <- function(renviron_path = "~/.Renviron") {
  cm_user <- Sys.getenv("COPERNICUSMARINE_SERVICE_USERNAME", unset = NA)
  cm_pass <- Sys.getenv("COPERNICUSMARINE_SERVICE_PASSWORD", unset = NA)
  if (!is.na(cm_user) && !is.na(cm_pass) && nzchar(cm_user) && nzchar(cm_pass)) {
    return(list(username = cm_user, password = cm_pass))
  }
  
  message("Copernicus Marine credentials not found (COPERNICUSMARINE_SERVICE_USERNAME/PASSWORD not set).")
  
  have_rstudio_popup <- requireNamespace("rstudioapi", quietly = TRUE) &&
    tryCatch(isTRUE(rstudioapi::isAvailable()), error = function(e) FALSE)
  
  if (have_rstudio_popup) {
    cm_user <- rstudioapi::showPrompt("Copernicus Marine login",
                                      "Copernicus Marine username (free account at data.marine.copernicus.eu):",
                                      default = "")
    if (is.null(cm_user) || !nzchar(cm_user)) {
      message("No username entered - proceeding without CMEMS credentials for this run.")
      return(NULL)
    }
    cm_pass <- rstudioapi::askForPassword("Copernicus Marine password:")
    if (is.null(cm_pass) || !nzchar(cm_pass)) {
      message("No password entered - proceeding without CMEMS credentials for this run.")
      return(NULL)
    }
  } else {
    message("(No RStudio pop-up available in this session - asking in the console instead.)")
    cm_user <- tryCatch(readline("Copernicus Marine username: "), error = function(e) "")
    if (!nzchar(cm_user)) {
      message("No username entered - proceeding without CMEMS credentials for this run.")
      return(NULL)
    }
    cm_pass <- if (requireNamespace("getPass", quietly = TRUE)) {
      getPass::getPass("Copernicus Marine password: ")
    } else {
      message("(Install the 'getPass' package for a masked password prompt - falling back to a",
              " plain console prompt, which WILL echo your password to the screen.)")
      tryCatch(readline("Copernicus Marine password (visible while typing): "), error = function(e) "")
    }
    if (is.null(cm_pass) || !nzchar(cm_pass)) {
      message("No password entered - proceeding without CMEMS credentials for this run.")
      return(NULL)
    }
  }
  
  ## Make the credentials usable for the rest of THIS session immediately...
  Sys.setenv(COPERNICUSMARINE_SERVICE_USERNAME = cm_user,
             COPERNICUSMARINE_SERVICE_PASSWORD = cm_pass)
  
  ## ...and persist them to ~/.Renviron so future R sessions have them too,
  ## without ever asking again. Replaces any previous lines for these two
  ## variables rather than duplicating them.
  renviron_full_path <- path.expand(renviron_path)
  existing_lines <- if (file.exists(renviron_full_path)) readLines(renviron_full_path, warn = FALSE) else character(0)
  existing_lines <- existing_lines[!grepl("^COPERNICUSMARINE_SERVICE_(USERNAME|PASSWORD)=", existing_lines)]
  new_lines <- c(existing_lines,
                 paste0("COPERNICUSMARINE_SERVICE_USERNAME=", cm_user),
                 paste0("COPERNICUSMARINE_SERVICE_PASSWORD=", cm_pass))
  writeLines(new_lines, renviron_full_path)
  message("Saved Copernicus Marine credentials to ", renviron_full_path,
          " - you won't be asked again on future runs.")
  
  list(username = cm_user, password = cm_pass)
}

fetch_cmems_phytoplankton_biomass <- function(out_dir, force_refresh = FALSE,
                                              bbox = c(lon_min = -6, lon_max = 12, lat_min = 30, lat_max = 46),
                                              proxy_years = 1999:2001,
                                              dataset_id = "med-ogs-plankton-rean-monthly",
                                              variable = "phyc",
                                              integration_depth_m = 50,
                                              c_molar_mass = 12.011,       # g C per mol C - to convert mol/m3 -> gC/m3
                                              wet_weight_per_c = 10,       # wet weight ~= 10x carbon biomass (same rule-of-thumb as the satellite fallback)
                                              large_phyto_fraction = 0.35,
                                              auto_install = TRUE,
                                              timeout_sec = 300) {
  ## This CSV cache is itself the "just one download" Andrea asked about:
  ## proxy_years is a fixed historical window (1999-2001), so there is
  ## nothing new to fetch on a later pipeline run once this file exists -
  ## every subsequent 01_biomass.R run just reads it back, no network
  ## call at all, until you delete it or pass force_refresh = TRUE.
  out_csv_path <- file.path(out_dir, "cmems_phytoplankton_biomass_by_fg.csv")
  
  if (!force_refresh && file.exists(out_csv_path)) {
    message("fetch_cmems_phytoplankton_biomass(): using cached ", out_csv_path,
            " (pass force_refresh = TRUE to re-query instead) - this is the one-time download;",
            " nothing is re-fetched from CMEMS while this file exists.")
    return(invisible(as.data.table(fread(out_csv_path))))
  }
  
  if (!ensure_copernicusmarine_cli(auto_install = auto_install)) {
    message("fetch_cmems_phytoplankton_biomass(): 'copernicusmarine' CLI unavailable (see message(s)",
            " above). Falling back to the satellite chlorophyll-a proxy instead (see",
            " lib_satellite_phytoplankton_biomass.R) for this run.")
    return(invisible(NULL))
  }
  if (!requireNamespace("ncdf4", quietly = TRUE)) {
    message("fetch_cmems_phytoplankton_biomass(): 'ncdf4' package is required to read the downloaded",
            " NetCDF file - install with install.packages('ncdf4'). Falling back to the satellite",
            " chlorophyll-a proxy instead for this run.")
    return(invisible(NULL))
  }
  
  result <- tryCatch({
    nc_path <- tempfile(fileext = ".nc")
    t0 <- paste0(min(proxy_years), "-01-01")
    t1 <- paste0(max(proxy_years), "-12-31")
    
    ## Credentials: env vars if already set, else an RStudio pop-up (or
    ## console prompt outside RStudio) asking once and saving the answer
    ## to ~/.Renviron for next time - see ensure_copernicusmarine_credentials().
    ## Substitutes entirely for having run 'copernicusmarine login' by hand.
    ## Left out of the args vector (not just blank strings) if the user
    ## declines, so the CLI falls back to its own stored login credentials
    ## (if any) as normal.
    cm_creds <- ensure_copernicusmarine_credentials()
    cm_pass <- if (!is.null(cm_creds)) cm_creds$password else NA
    cred_args <- if (!is.null(cm_creds)) c("--username", cm_creds$username, "--password", cm_creds$password) else character(0)
    
    args <- c("subset",
              "--dataset-id", dataset_id,
              "--variable", variable,
              "--start-datetime", t0, "--end-datetime", t1,
              "--minimum-longitude", bbox["lon_min"], "--maximum-longitude", bbox["lon_max"],
              "--minimum-latitude", bbox["lat_min"], "--maximum-latitude", bbox["lat_max"],
              "--minimum-depth", "0", "--maximum-depth", as.character(integration_depth_m),
              "--output-filename", basename(nc_path), "--output-directory", dirname(nc_path),
              "--force-download", cred_args)
    printable_args <- if (!is.na(cm_pass)) gsub(cm_pass, "***", args, fixed = TRUE) else args
    message("Fetching CMEMS phytoplankton carbon via: copernicusmarine ",
            paste(printable_args, collapse = " "),
            "\n(", min(proxy_years), "-", max(proxy_years), " - the earliest years the Med BGC reanalysis",
            " covers at all, used as the closest hindcast proxy to 1994-1996, which this reanalysis does",
            " not reach back to. This is a ONE-TIME download - see the cache note above.)",
            if (length(cred_args) == 0) "\n(No COPERNICUSMARINE_SERVICE_USERNAME/PASSWORD env vars set -" else "",
            if (length(cred_args) == 0) " relying on credentials already stored by a prior 'copernicusmarine login'.)" else "")
    
    exit_status <- system2("copernicusmarine", args, stdout = TRUE, stderr = TRUE, timeout = timeout_sec)
    if (!file.exists(nc_path)) {
      stop("copernicusmarine subset did not produce the expected file. CLI output:\n",
           paste(exit_status, collapse = "\n"),
           "\nCheck dataset_id ('", dataset_id, "')/variable ('", variable, "') are still current",
           " (run 'copernicusmarine describe --contains ", variable, "' to check) and that your",
           " credentials are stored ('copernicusmarine login').")
    }
    
    nc <- ncdf4::nc_open(nc_path)
    nc_var_names <- names(nc$var)
    message("\nVariables in the downloaded NetCDF (verify '", variable, "' or something close to it is",
            " present):")
    print(nc_var_names)
    col_var <- intersect(c(variable, "phyc", "PHYC"), nc_var_names)[1]
    if (is.na(col_var)) {
      ncdf4::nc_close(nc)
      stop("None of the expected variable names were found in the downloaded NetCDF",
           " (variables present: ", paste(nc_var_names, collapse = ", "), ").")
    }
    phyc_vals <- ncdf4::ncvar_get(nc, col_var)
    ncdf4::nc_close(nc)
    phyc_vals <- phyc_vals[is.finite(phyc_vals) & phyc_vals > 0]
    if (length(phyc_vals) == 0) stop("No finite, positive '", col_var, "' values in the downloaded subset.")
    
    phyc_mean_mol_m3 <- mean(phyc_vals)
    message("\nMean ", col_var, " over ", length(phyc_vals), " grid cell x depth x time observation(s), ",
            min(proxy_years), "-", max(proxy_years), ": ", signif(phyc_mean_mol_m3, 4), " mol C/m3",
            " (range ", signif(min(phyc_vals), 4), "-", signif(max(phyc_vals), 4), ").")
    
    ## mol C/m3 -> g C/m3 -> areal g C/m2 (integrated over integration_depth_m)
    ## -> g wet weight/m2 == t/km2 numerically (1 t/km2 = 1 g/m2).
    c_g_m3 <- phyc_mean_mol_m3 * c_molar_mass
    c_g_m2 <- c_g_m3 * integration_depth_m
    total_biomass_t_km2 <- c_g_m2 * wet_weight_per_c
    
    citation <- paste0("Copernicus Marine Mediterranean Sea Biogeochemistry Reanalysis (",
                       "MEDSEA_MULTIYEAR_BGC_006_008, MedBFM/OGSTM-BFM model, dataset '", dataset_id,
                       "', variable '", col_var, "'), ", min(proxy_years), "-", max(proxy_years),
                       " mean (", signif(phyc_mean_mol_m3, 4), " mol C/m3) - closest available years to",
                       " 1994-1996 (the reanalysis starts Jan 1999) - converted via molar mass=",
                       c_molar_mass, " gC/mol, integration depth=", integration_depth_m,
                       "m, wet weight=", wet_weight_per_c, "xC. Large/Small split is a fixed ",
                       large_phyto_fraction, "/", round(1 - large_phyto_fraction, 2),
                       " fraction (a simplification, same as the satellite fallback's own caveat).")
    
    cmems_result <- data.table(
      TargetGroup = c("LargePhytoplankton", "SmallPhytoplankton"),
      Biomass_t_km2 = c(total_biomass_t_km2 * large_phyto_fraction,
                        total_biomass_t_km2 * (1 - large_phyto_fraction)),
      Source_citation = citation
    )
    fwrite(cmems_result, out_csv_path)
    message("\nSaved cmems_phytoplankton_biomass_by_fg.csv (", round(total_biomass_t_km2, 4),
            " t/km2 total, split ", large_phyto_fraction, "/", round(1 - large_phyto_fraction, 2),
            " Large/Small).")
    cmems_result
    
  }, error = function(e) {
    message("fetch_cmems_phytoplankton_biomass(): failed - ", conditionMessage(e),
            ". Falling back to the satellite chlorophyll-a proxy instead for this run.")
    NULL
  })
  
  invisible(result)
}