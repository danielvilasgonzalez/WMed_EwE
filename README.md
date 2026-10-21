# Inputs for Western Mediterranean EwE models

R pipeline that turns survey, fisheries and literature data into Ecopath with Ecosim (EwE) inputs for the Western Mediterranean (GSAs 1–12: GSAs 1–11 plus GSA 12, Northern Tunisia, as in the MSFD Western Mediterranean subregion). It writes one workbook, `ecopath_ecosim_inputs.xlsx`, plus a CSV and plot for every step, and a validation report.

Created by: Daniel Vilas

## What you need

- **This repository** (GitHub): the code.
- **The shared pCloud folder "EwE Western Med 2026"**: the data. Ask Daniel Vilas for access. Install pCloud Drive so the folder appears as a drive on your computer.
- **R** (≥ 4.3) and **RStudio**.
- **Quarto** (only for the interactive page; it comes with recent RStudio versions).
- **Internet on the first run**: several sources are downloaded once and cached (see [First run](#first-run)).

## Quick start

1. Clone the repo and open `WMed_EwE.Rproj` in RStudio.
2. Install the packages (once, then restart R):
   ```r
   source("scripts/install_packages.R")
   ```
3. Run the whole pipeline:
   ```r
   source("scripts/run_pipeline_demo.R")
   ```

You don't need to edit any paths. `scripts/wmed_paths.R` finds the three folders on each computer:

| Folder | What it is | How it is found (first one that exists wins) |
|---|---|---|
| `git_dir` | this repo | the folder the scripts are in |
| `pcloud_dir` | the shared data folder | `config.local.R` → `config.R` → environment variable `WMED_PCLOUD_DIR` → the standard pCloud Drive place (macOS `~/pCloud Drive/EwE Western Med 2026`, Windows `P:/EwE Western Med 2026`, Linux `~/pCloudDrive/EwE Western Med 2026`) |
| `out_dir` | where the outputs go (your own computer) | `config.local.R` → `config.R` → `WMED_OUT_DIR` → `~/WMed_EwE_output` (created if missing) |

If the pCloud folder is somewhere else, RStudio asks you to pick it once and saves your answer to `config.local.R`. That file is git-ignored, so your paths never reach other users. `config.R` in the repo holds Daniel's paths; they are skipped on any computer where they don't exist.

## Interactive page

`scripts/medbs_pipeline_methodology.qmd` is a Quarto/Shiny page. From it you can set the folders, model area and years, run the pipeline, watch its progress, and download the outputs. It also documents every method, equation and source.

```bash
quarto serve scripts/medbs_pipeline_methodology.qmd
```

In RStudio, you can instead open the file and click **Run Document**.

On the page you can:

- **Model area**: choose Western Med GSAs on a map, or a custom region (upload a shapefile or draw a bounding box).
- **Functional groups**: upload your own FG table (`FG_number`, `FG_name`, `species`; optional `FG_group`). The default is `FG_WMed_2026.csv`, built for the EwE Western Med model.
- **Years**: Ecopath and Ecosim years. Catch and effort years follow the Ecosim years.
- **Options**: outlier removal, Ecosim normalisation, and filling missing Ecopath biomass with the 1995–2000 mean.
- **Results**: download the workbook, the diet matrix, the review tables, or everything as one zip.

## Pipeline steps

Run in this order; each step reads files the earlier ones wrote. `run_pipeline_demo.R` runs them all (`RUN_STEPS <- 1:5`; e.g. `RUN_STEPS <- 5` redraws only the validation).

| Step | Script | What it does |
|---|---|---|
| 1 | `01_biomass.R` (sources `01b_biomass_unsurveyed.R`) | **Biomass** from the MEDITS bottom-trawl and MEDIAS acoustic surveys. AquaMaps depth pseudo-strata cover 0–10 m, 800–1000 m and 1000 m to the seafloor (3,785 m). Stock assessments are used for assessed single-species FGs and hake stanzas. Small pelagics are back-cast to 1994–96. Ecopath_B, Ecosim biomass series, FG_spp. |
| 1b | `01b_biomass_unsurveyed.R` | **Groups the surveys can't measure.** Seagrass, macroalgae and corals: density × mapped area × occupancy. Plankton: COBALT model. Detritus: Pauly et al. 1993. Macrozooplankton: MAREDAT. Cetaceans, seabirds, turtles and monk seal: survey density × body mass. Bluefin tuna and swordfish: ICCAT. Some FGs are left for Ecopath to estimate (EE = 0.95). |
| 2 | `02_fisheries.R` | **Catch, landings, discards, effort and F** by country, fleet and FG. Catch from GFCM, STECF FDI and Sea Around Us; effort from FishMIP; plus ICCAT; technology creep applied to effort. |
| 3 | `03_pbqb-traits.R` | **P/B and Q/B** per FG from FishBase/SeaLifeBase traits and empirical equations. |
| 4 | `04_diets.R` | **Diet matrix.** Sources in order: Mediterranean metaweb → MAISHA → FishBase/SeaLifeBase → EcoBase → literature fallback (`diet_literature_fallback.csv`). FG order follows the FG table. |
| 5 | `05_validation.R` | **Validation.** Compares with the 1995 Western Med model and runs the EwE plausibility checks (P/Q, diet sums, cannibalism). Writes one summary table and one PDF of all plots. |

Every method, equation, assumption and reference is in `scripts/medbs_pipeline_methodology.qmd`.

## Main options

Set these at the top of `scripts/run_pipeline_demo.R`, or on the interactive page. Every option has a comment next to it in the script.

| Option | Default | Meaning |
|---|---|---|
| `MODEL_GSAS` | `1:12` | GSAs in the model area. A subset filters area, survey data and catch countries together. |
| `MODEL_AREA_BASIS` | `"whole_gsa"` | `"whole_gsa"` = coast to bottom; `"medits_10_800"` = MEDITS 10–800 m strata only. |
| `YEAR_ECOPATH` / `TS_YEARS` | `1994:1996` / `1995:2023` | Ecopath baseline / Ecosim years. |
| `ECOPATH_B_FILL_MODE`, `ECOPATH_B_FILL_YEARS` | `"window_mean"`, `1995:2000` | FGs with no biomass in the Ecopath years take this mean; "Expanding" FGs get 0.00001. |
| `STOCK_ASSESSMENT_SOURCE` | `"RAM"` | `"RAM"` = GFCM/STECF assessments via RAM Legacy; `"GFCM"` = hand-transcribed GFCM assessment forms (`data/fisheries/GFCM_SAF/`). |
| `DROP_OUTLIERS` | `TRUE` | Remove flagged outlier hauls. |
| `FG_SPECIES_FILE` | `data/FG_WMed_2026.csv` | Species → FG table. |
| `AREA_MODE` (custom region) | `"westmed"` | `"custom"` with `CUSTOM_AREA_TYPE = "shapefile"` or `"bbox"`. Runs step 1 only; outputs go to `out_dir/<AREA_NAME>/`. |
| `PLOT_DPI` | `300` | Resolution of every PNG. |

## First run

Some sources are downloaded once and cached; later runs reuse the cache.

| What | Size | Used by | Notes |
|---|---|---|---|
| AquaMaps database (`aquamapsdata`) | ~2 GB download, ~10 GB unpacked | 01, 01b | depth suitability for the pseudo-strata and habitat depth |
| NOAA ETOPO bathymetry (`marmap`) | small, per area | 01, 01b | stratum and habitat depths; needs internet |
| FishBase / SeaLifeBase (`rfishbase`) | small | 01, 03, 04 | taxonomy, traits, diets |
| ISIMIP3a COBALT plankton fields | hundreds of MB | 01b | plankton and primary production |
| EUSeaMap 2025 geodatabase | > 1 GB | 01b | put it in `data/Complementary data/EUSeaMap_2025/`; areas are cached after the first read |
| Copernicus Marine (optional) | – | 01b | needs a free account; you are prompted for your own login. Credentials are never stored in the code. |

The packages that must come from GitHub (`rfishbase`, `duckdbfs`, `FishLife`) are installed by `install_packages.R`.

## Data in the pCloud folder

The main inputs, under `EwE Western Med 2026/data/`:

- `FG_WMed_2026.csv`: species → functional groups (80 FGs).
- `Medits_Medias_JRC2026/`: MEDITS and MEDIAS survey data.
- `fisheries/`: GFCM, FDI, Sea Around Us, FishMIP, ICCAT, RousseauEtAl2023, STAR_RAMLegacy, GFCM_SAF.
- `Complementary data/`: reference tables with a citation on every row. Includes habitat biomass, megafauna, unit conversions, diet tables and literature fallbacks.
- `diet/`: metaweb and MAISHA files.
- `WMed_EwE.xlsx`: the 1995 Western Med model, used for validation.

## Outputs (under `out_dir`)

```
ecopath_ecosim_inputs.xlsx   # the EwE workbook
biomass/ fisheries/ pbqb-traits/ diet/ validation/   # CSVs of each step, incl. *_REVIEW.csv tables to check
plots/                       # PNGs by step
validation_summary_ALL.csv   # every check in one table
validation_plots_ALL.pdf     # every plot in one file
logs/                        # one log per script run
```

## Repository structure

```
scripts/
  run_pipeline_demo.R              # runs steps 1-5; all options
  wmed_paths.R                     # finds git_dir, pcloud_dir, out_dir on any computer
  install_packages.R
  01_biomass.R  01b_biomass_unsurveyed.R  02_fisheries.R  03_pbqb-traits.R  04_diets.R  05_validation.R
  03b_ecobase.R                    # EcoBase fallbacks
  lib_*.R                          # shared functions (survey density, AquaMaps, FishBase diets, plankton, taxonomy)
  medbs_pipeline_methodology.qmd   # interactive page + full method documentation
  gsa_boundaries_westmed.geojson   # GSA 1-12 polygons for the page map
  additional/                      # one-off helpers and backups, not run by the pipeline
config.R            # Daniel's paths (skipped where they don't exist)
config.local.R      # your paths, written for you, git-ignored
ewe_infography.jpg  # workflow figure
ewe_infography_references.md     # references by block of the workflow figure
```

## Known limitations

- The survey density of GSAs 1, 2 and 5–11 is applied to the unsurveyed GSAs 3, 4 and 12.
- Several groups rest on literature values from one site or on surveys from 2011–2019 (flagged as temporal mismatches in the review tables).
- The GFCM fleet register file is not parsed correctly yet. The vessel-based fleet split for Morocco, Algeria and Tunisia falls back to the GFCM fleet taxonomy or Sea Around Us.
- The literature diet fallback for corals and gorgonians, meso- and microzooplankton and suprabenthos has no proportions yet, so those three FGs have no diet until it is filled.
- Package versions are not pinned (no `renv` lockfile yet).

See the "Known limitations" section of the interactive page for the full list.
