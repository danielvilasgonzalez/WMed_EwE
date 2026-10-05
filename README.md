# WMed EwE Pipeline

R pipeline that turns survey, fisheries, and literature data into Ecopath-with-Ecosim (EwE) input tables for the Western Mediterranean model (GSA 1-11). Produces the shared `ecopath_ecosim_inputs.xlsx` workbook, per-step CSVs, and QA/validation plots.

## Pipeline steps

Run in order - each later step depends on files the earlier ones write:

| Step | Script | Produces |
|---|---|---|
| 1 | `01_biomass.R` | Species/FG biomass density from MEDITS/MEDIAS survey data + stock-assessment/ICCAT/megafauna/EcoBase fallbacks for FGs the survey doesn't sample. Sources `03b_ecobase.R` and `lib_worms_taxonomy_lookup.R`. |
| 2 | `02_fisheries.R` | Catch/landings/discards/effort by country, fleet, and FG - GFCM, STECF FDI, Sea Around Us, FishMIP, ICCAT, STAR/RAM, Belhabib et al. (Morocco/Algeria), Rousseau et al. (effort), blended by an explicit source-priority cascade per FG x Year. |
| 3 | `03_pbqb-traits.R` | P/B (production/biomass) and Q/B (consumption/biomass) per FG, from FishBase growth/consumption parameters, TropFishR/FishLife estimators, and EcoBase literature values. Sources `03b_ecobase.R`. |
| 4 | `04_diets.R` (optional) | Diet composition matrix (predator x prey), from EcoBase's diet-matrix fallback plus manual metaweb entries. |
| 5 | `05_validation.R` (optional) | Cross-cutting validation plots across all of the above. |

`scripts/run_pipeline_demo.R` sources all four (1-4) in order, with example config for both the West Med default region and a custom region.

## Setup

### 1. Get the data

You need read access to the shared external data folder (pCloud: "EwE Western Med 2026") - `FG_WMed_2026.csv` and the `fisheries/` subfolders (GFCM, FDI, SAU, Morocco_Algeria, RousseauEtAl2023, FishMIP, STAR_RAMLegacy, ICCAT). This data isn't part of the repo - request access from whoever administers the shared pCloud folder.

`ewe_group_table.csv` is NOT needed - `04_diets.R` builds that table itself automatically (group_number/group_name from the FG reference `01_biomass.R` already writes; is_predator defaults to TRUE for every FG except Detritus/Discards and primary producers - phytoplankton, Posidonia/seagrass, macroalgae, Cymodocea). It only matters as an optional override if you ever want to manually correct `is_predator` for a specific FG.

### 2. Install R packages

```r
source("scripts/install_packages.R")
```

This installs every plain CRAN package the scripts use, plus three packages that **must** come from GitHub rather than CRAN (`rfishbase`, `duckdbfs`, `FishLife`) - the CRAN releases of the first two are missing exports `03_pbqb-traits.R` requires and it will `stop()` with an install command if it detects the wrong build. Restart R after this script runs.

### 3. Configure your paths

```
cp config.R.example config.R
```

Edit `config.R` to point at your own:
- `out_dir` - any writable empty folder (the pipeline creates every subfolder it needs)
- `pcloud_dir` - your local copy/sync of the shared data folder from step 1
- `git_dir` - this repo's own local clone

`config.R` is gitignored - it's personal, never committed, so everyone running this keeps their own paths without colliding in git history.

### 4. Run

Open `scripts/run_pipeline_demo.R` in RStudio (with the `WMed_EwE.Rproj` project open, so the working directory is the repo root where `config.R` lives) and hit Source, or:

```r
source("scripts/run_pipeline_demo.R")
```

It automatically picks up `config.R` if present (warns and falls back to Daniel's own hardcoded paths if not - create your `config.R` first). `RUN_MODE` at the top switches between the West Med default region/years and a custom-region example (edit that block for e.g. a different set of GSAs).

Every step fails fast with a clear message if `out_dir`/`pcloud_dir`/`git_dir` don't exist, or if `git_dir/scripts/` is missing one of the required files - see below.

## Repository structure

```
scripts/
  01_biomass.R
  02_fisheries.R
  03_pbqb-traits.R
  04_diets.R
  05_validation.R
  03b_ecobase.R                          # sourced by 01/03/04 - EcoBase literature PB/QB, biomass, and diet-matrix fallbacks
  lib_survey_fg_density_functions.R      # shared workbook-writing helpers
  lib_worms_taxonomy_lookup.R            # sourced by 01 - WoRMS taxonomy lookup
  lib_aquamaps_depth_extension.R         # optional - only if APPLY_AQUAMAPS_DEPTH_ADJUSTMENT <- TRUE
  lib_cmems_phytoplankton_biomass.R      # copernicusmarine CLI/credential helpers + MedBFM phytoplankton fallback
  lib_satellite_phytoplankton_biomass.R  # optional - phytoplankton biomass fallback
  run_pipeline_demo.R
  install_packages.R
  additional/                            # one-off helper scripts, not part of the pipeline run
config.R.example
config.R          # your own, gitignored - not in the repo
.gitignore
```

All six required `scripts/` files above (`03b_ecobase.R`, `lib_worms_taxonomy_lookup.R`, `05_validation.R` included) need to exist in `git_dir/scripts/` before running - `01_biomass.R`, `03_pbqb-traits.R`, and `04_diets.R` `source()` the first two unconditionally, so a missing file crashes the run the moment that step starts, even if the feature it enables is turned off.

## Output layout (under `out_dir`)

```
ecopath_ecosim_inputs.xlsx     # shared workbook - every step writes/trims its own sheets into this
biomass/                       # Step 1's CSVs (species density, biomass proportions, taxonomy)
fisheries/                     # Step 2's CSVs
plots/
  biomass/                     # Step 1's diagnostic plots
  fisheries/                   # Step 2's diagnostic plots
  pbqb-traits/                 # Step 3's final PB/QB-by-FG figures (FG_PB_QB_comparison*, F_by_fg, PQ_ratio_by_fg)
  validation/                  # Cross-script QA/methods-comparison plots (Steps 1-3's own validation figures, plus Step 5 if run)
  diets/                       # Step 4's diet-matrix heatmap
fishbase_taxonomy_cache.rds    # auto-generated cache - delete to force a refresh
worms_taxonomy_cache.rds       # auto-generated cache - delete to force a refresh
```