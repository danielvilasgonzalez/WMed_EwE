# WMed_EwE pipeline

An R pipeline that turns fishery-independent survey data and catch/effort
records into ready-to-use input sheets for an Ecopath-with-Ecosim (EwE)
model — for **any region in the Mediterranean Sea**: a single GFCM
Geographical Sub-Area (GSA), any subset or the full set of GSAs, or a
fully custom region (a bounding box or an arbitrary shapefile that doesn't
follow GSA lines at all — a bay, an MPA, or a footprint spanning parts of
several GSAs).

The scripts ship configured for the **Western Mediterranean (GSA 1–11)**
as a worked example. Nothing about the underlying logic is Western-Med
specific — see [Adapting to a different region](#adapting-to-a-different-region).

## What it does

Four scripts, run in order, build up one shared Excel workbook
(`output/ecopath_ecosim_inputs.xlsx`) and a set of native/audit CSVs:

| Step | Script | Produces |
|---|---|---|
| 1 | `01_biomass.R` | Functional-Group (FG) biomass, from MEDITS trawl + MEDIAS acoustic survey density, with a stock-assessment override for single-species FGs |
| 2 | `02_fisheries.R` | FG catch/landings/discard time series, from GFCM/FDI/SAU/FishMIP catch sources, with the same stock-assessment override |
| 3 | `03_pbqb-traits.R` | FG production/biomass (P/B) and consumption/biomass (Q/B) rates, plus life-history traits |
| 4 | `04_diets.R` | FG × FG diet-composition matrix, from a predator–prey metaweb database |

A shared library, `lib_survey_fg_density_functions.R`, holds every function
all four scripts call — species→FG matching, area/density weighting,
Excel I/O, PB/QB helpers. Nothing in the numbered scripts duplicates logic
that belongs there.

Each script is **source-able**: every region/year/method knob
(`FILTER_AREAS`, `TARGET_COUNTRIES`, `YEAR_ECOPATH`, `START_YEAR`/
`END_YEAR`, `TS_YEARS`, `FISHERIES_DATA_SOURCE`, `out_dir`/`pcloud_dir`/
`git_dir`, and others — see each script's own "Configuration" section) is
wrapped in `if (!exists("VAR")) VAR <- <default>`, so a driver script can
set any subset of them before `source()`-ing, and everything else falls
back to the built-in Western Med default. `run_pipeline_demo.R` shows this
pattern end to end.

## Data sources

| Source | Feeds | Role |
|---|---|---|
| MEDITS bottom-trawl survey | Biomass (Step 1) | Per-haul density, the primary survey source |
| MEDIAS acoustic survey | Biomass (Step 1) | Second density source, best for small pelagics a trawl under-samples; averaged with MEDITS on overlap |
| GFCM STATLANT / FDI / SAU / FishMIP | Catches (Step 2) | Landings, discards, and fishing effort by FG, fleet, and year |
| GFCM STAR / RAM Legacy stock assessments | Biomass + catches (Steps 1–2) | Overrides the survey/GFCM-derived value for single-species/stanza FGs (e.g. bluefin tuna, swordfish) whenever a real assessment exists for that FG and year |
| GFCM GSA shapefile / NOAA bathymetry | Study-area definition (Step 1) | Official GSA polygons; depth-stratum area, downloaded per region actually in scope |
| Species→FG reference workbook (`FG_WMed.xlsx`) | All steps | The species-to-Functional-Group assignment every script matches against |
| EcoBase model repository | PB/QB (Step 3) | Literature P/B, Q/B values, used only to gap-fill an FG the empirical calculation couldn't produce a value for |
| Mediterranean trophic metaweb database | Diet (Step 4) | Predator–prey diet records; optional step, currently an empty template pending real data entry |

## Methods, briefly

- **FG matching** is a fixed fallback cascade, each stage only attempting
  species the previous one left unmatched: direct scientific-name match →
  nominate-subspecies retry (strips a repeated trinomial word) → manual
  seed rules (for FGs with no reference species to match against) →
  taxonomy fallback (Genus → Family → Order → Class; a rank value is used
  only when it resolves to exactly one FG among already-assigned species,
  never a majority guess, and never into a single-species FG, a
  jellyfish/suprabenthos/macrozooplankton FG, or one with "commercial" in
  its name) → manual review for whatever's still unmatched.
- **Density** is computed per haul (catch/swept area), corrected for
  species-specific catchability where a correction factor exists, and
  flagged for outliers via a robust per-species/per-area z-score before
  being weighted up to a strata-weighted, then region-wide, area-weighted
  FG density index.
- **Biomass and catch priority**: for FGs that are a genuinely
  single-species (or single-stanza) stock with its own GFCM stock
  assessment, that assessment's biomass and catch figures are used
  directly wherever available, in preference to the survey- or
  GFCM-catch-derived value, for both the static Ecopath snapshot and the
  full Ecosim time series.
- **PB/QB** is calculated per species (growth-and-mortality-based for
  fish, empirical relationships for invertebrates), biomass-weighted up
  to one value per FG, then gap-filled from EcoBase literature values only
  for an FG the empirical method produced nothing for.
- **Diet** is built per predator species from the metaweb's own study
  records (weighted by sample size, presence-only records converted to an
  equal split), then expanded to FG × FG by weighting each predator
  species' contribution by its share of that FG's biomass.

## Output

Everything lands under one `out_dir`. The shared workbook stays at the top
level (every script reads and writes it); each block's own
native/intermediate CSVs go into their own subfolder:

```
output/
├── ecopath_ecosim_inputs.xlsx   ← the final, shared workbook
├── biomass/                     ← 01_biomass.R's own CSVs
├── fisheries/                   ← 02_fisheries.R's own CSVs
├── pbqb-traits/                 ← 03_pbqb-traits.R's own CSVs
└── diet/                        ← 04_diets.R's own CSVs
```

The final workbook carries exactly 10 sheets, trimmed to this set after
every run:

`info` · `FG_spp` · `Ecopath_B` · `Ecopath_L` · `Ecopath_Di` ·
`Ecopath_PBQB` · `Ecopath_traits` · `Ecopath_diet` · `Ecosim_ts` ·
`FG_References`

`FG_References` is a provenance sheet — one row per FG, with a column per
block (`ref_B`, `ref_fisheries`, `ref_pbqb_traits`, `ref_diet`) naming which
data source actually fed that FG, plus `ref_methods` for the calculation
method/literature behind its PB/QB. Every other native/intermediate table
each script builds along the way (matching detail, audit CSVs, validation
plots) stays as CSV in that script's own subfolder, never as a workbook
sheet.

## Adapting to a different region

- **A different GSA or GSA subset** (Adriatic, Ionian, Aegean, Levant, the
  whole basin): set `FILTER_AREAS` to the target GSA number(s) and point
  `out_dir`/`pcloud_dir`/`git_dir` at that region's own data — no other
  code changes needed. GSA-specific corrections (e.g. the Alboran
  longitude-sign fix) are harmless no-ops outside GSA 1–3.
- **A fully custom, non-GSA region anywhere in the Mediterranean**: use
  `01_survey_density_custom.R` directly, with `AREA_NAME`,
  `CUSTOM_AREA_TYPE` (`"bbox"` or `"shapefile"`), and
  `CUSTOM_BBOX`/`CUSTOM_SHAPEFILE_PATH` set to the target boundary — no
  GSA-specific code path runs at all.
- `YEAR_ECOPATH` (the Ecopath snapshot years) and `TS_YEARS` (the Ecosim
  time-series range) are read by every script and must stay consistent
  across all of them — set them once in a driver script before sourcing
  the four in order, rather than editing each script's own copy by hand.

## Further reading

- `pipeline_documentation.Rmd` — the full script-by-script reference:
  every input file, every function's signature and return value, every
  output column.
- `medbs_pipeline_methodology.qmd` — the methodology write-up, with an
  interactive map for previewing a GSA-based vs. custom-region setup and
  generating a ready-to-paste config block.
