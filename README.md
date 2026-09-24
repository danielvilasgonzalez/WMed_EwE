---

editor_options: 
  markdown: 
    wrap: 72
---

# WMed EwE Pipeline

Created by: Daniel Vilas

An R pipeline that turns fishery-independent survey data and catch/effort records into ready-to-use input sheets for an Ecopath-with-Ecosim (EwE) model — for **any region in the Mediterranean Sea**: a single GFCM Geographical Sub-Area (GSA), any subset or the full set of GSAs, or a fully custom region (a bounding box or an arbitrary shapefile that doesn't follow GSA lines at all — a bay, an MPA, or a footprint spanning parts of several GSAs).

The scripts ship configured for the **Western Mediterranean (GSA 1–11)** as a worked example. Nothing about the underlying logic is Western-Med specific — see [Adapting to a different region](#adapting-to-a-different-region).

## What it does

Four scripts, run in order, build up one shared Excel workbook (`output/ecopath_ecosim_inputs.xlsx`) and a set of native/audit CSVs:

| Step | Script | Produces |
|------------------------|------------------------|------------------------|
| 1 | `01_biomass.R` | Functional-Group (FG) biomass, from MEDITS trawl + MEDIAS acoustic survey density, with a stock-assessment override for single-species FGs |
| 2 | `02_fisheries.R` | FG catch/landings/discard time series, from GFCM/FDI/SAU/FishMIP catch sources, with the same stock-assessment override |
| 3 | `03_pbqb-traits.R` | FG production/biomass (P/B) and consumption/biomass (Q/B) rates, plus life-history traits |
| 4 | `04_diets.R` | FG × FG diet-composition matrix, from a predator–prey metaweb database |

A shared library, `lib_survey_fg_density_functions.R`, holds every function all four scripts call — species→FG matching, area/density weighting, Excel I/O, PB/QB helpers. Nothing in the numbered scripts duplicates logic that belongs there.

Each script is **source-able**: every region/year/method knob (`FILTER_AREAS`, `TARGET_COUNTRIES`, `YEAR_ECOPATH`, `START_YEAR`/ `END_YEAR`, `TS_YEARS`, `FISHERIES_DATA_SOURCE`, `out_dir`/`pcloud_dir`/ `git_dir`, and others — see each script's own "Configuration" section) is wrapped in `if (!exists("VAR")) VAR <- <default>`, so a driver script can set any subset of them before `source()`-ing, and everything else falls back to the built-in Western Med default. `run_pipeline_demo.R` shows this pattern end to end.

## Data sources

| Source | Feeds | Role |
|------------------------|------------------------|------------------------|
| MEDITS bottom-trawl survey | Biomass (Step 1) | Per-haul density, the primary survey source |
| MEDIAS acoustic survey | Biomass (Step 1) | Second density source, best for small pelagics a trawl under-samples; averaged with MEDITS on overlap |
| GFCM STATLANT / FDI / SAU / FishMIP | Catches (Step 2) | Landings, discards, and fishing effort by FG, fleet, and year |
| GFCM STAR / RAM Legacy stock assessments | Biomass + catches (Steps 1–2) | Overrides the survey/GFCM-derived value for single-species/stanza FGs whenever a real assessment exists for that FG and year. **`combine_STAR_RAMlegacy.R` does not exist yet** — this source is currently a no-op; see [Known gaps](#known-gaps) |
| ICCAT stock assessments (SSB) | Biomass + catches (Step 1–2) | Bluefin tuna and swordfish are basin/ocean-wide migratory stocks a Western-Med-only survey never catches representatively — their base-year density comes from a manual, cited `iccat_ssb_biomass.csv` instead: swordfish/albacore use whole-Mediterranean-stock SSB ÷ Mediterranean Sea area (\~2,510,000 km², a flagged uniform-density approximation); bluefin tuna uses a real, direct GBYP aerial-survey density (ICCAT's own Grand Bluefin Tuna Year Program, Balearic Sea "A-core" spawning-aggregation block, 2017–2021 average, CREEM analysis) rather than an area-ratio split, since the East Atlantic+Mediterranean stock is mostly Atlantic and only concentrates in the Med to spawn |
| EcoBase (other published Mediterranean Ecopath models) | Biomass (Step 1, generic fallback) | Whatever FG still has no biomass after every other source is matched against EcoBase's other Mediterranean models by keyword; patches only the `Ecopath_B` baseline-year snapshot, not the full `Ecosim_ts` series |
| GFCM GSA shapefile / NOAA bathymetry | Study-area definition (Step 1) | Official GSA polygons; depth-stratum area, downloaded per region actually in scope |
| Species→FG reference workbook (`FG_WMed.xlsx`) | All steps | The species-to-Functional-Group assignment every script matches against |
| EcoBase model repository | PB/QB (Step 3) | Literature P/B, Q/B values, used only to gap-fill an FG the empirical calculation couldn't produce a value for |
| Mediterranean trophic metaweb database | Diet (Step 4) | Predator–prey diet records; optional step, currently an empty template pending real data entry |

## Methods, briefly

- **FG matching** is a fixed fallback cascade, each stage only attempting species the previous one left unmatched: direct scientific-name match → nominate-subspecies retry (strips a repeated trinomial word) → manual seed rules (for FGs with no reference species to match against) → taxonomy fallback (Genus → Family → Order → Class; a rank value is used only when it resolves to exactly one FG among already-assigned species, never a majority guess, and never into a single-species FG, a jellyfish/suprabenthos/macrozooplankton FG, or one with "commercial" in its name) → manual review for whatever's still unmatched.
- **Density** is computed per haul (catch/swept area), corrected for species-specific catchability where a correction factor exists, and flagged for outliers via a robust per-species/per-area z-score before being weighted up to a strata-weighted, then region-wide, area-weighted FG density index.
- **Biomass and catch priority**: for FGs that are a genuinely single-species (or single-stanza) stock with its own GFCM/ICCAT stock assessment, that assessment's biomass and catch figures are used directly wherever available, in preference to the survey- or GFCM-catch-derived value, for both the static Ecopath snapshot and the full Ecosim time series.
- **Survey-exempt FGs** (`EXEMPT_FG_NAMES` — currently Cymodocea, Posidonia, Macroalgae, Corals and gorgonians, Macro zooplankton, Meso and micro zooplankton, Suprabenthos): MEDITS/MEDIAS are not designed to sample these representatively at all, so their incidental bycatch density is never allowed to win over a real stock-assessment/EcoBase/ literature figure, and — critically — an exempt FG with **no** such figure is left genuinely missing rather than silently backfilled with survey noise. Marine megafauna (cetaceans, monk seals, seabirds, sea turtles) work the same way structurally, via a manual, cited `marine_megafauna_biomass.csv` that **does not exist yet** — see [Known gaps](#known-gaps).
- **PB/QB** is calculated per species (growth-and-mortality-based for fish, empirical relationships for invertebrates), biomass-weighted up to one value per FG, then gap-filled from EcoBase literature values only for an FG the empirical method produced nothing for.
- **Diet** is built per predator species from the metaweb's own study records (weighted by sample size, presence-only records converted to an equal split), then expanded to FG × FG by weighting each predator species' contribution by its share of that FG's biomass.

## Output

Everything lands under one `out_dir`. The shared workbook stays at the top level (every script reads and writes it); each block's own native/intermediate CSVs go into their own subfolder:

```         
output/
├── ecopath_ecosim_inputs.xlsx   ← the final, shared workbook
├── biomass/                     ← 01_biomass.R's own CSVs
├── fisheries/                   ← 02_fisheries.R's own CSVs
├── pbqb-traits/                 ← 03_pbqb-traits.R's own CSVs
└── diet/                        ← 04_diets.R's own CSVs
```

The final workbook carries exactly 10 sheets, trimmed to this set after every run:

`info` · `FG_spp` · `Ecopath_B` · `Ecopath_L` · `Ecopath_Di` · `Ecopath_PBQB` · `Ecopath_traits` · `Ecopath_diet` · `Ecosim_ts` · `FG_References`

`FG_References` is a provenance sheet — one row per FG, with a column per block (`ref_B`, `ref_fisheries`, `ref_pbqb_traits`, `ref_diet`) naming which data source actually fed that FG, plus `ref_methods` for the calculation method/literature behind its PB/QB. Every other native/intermediate table each script builds along the way (matching detail, audit CSVs, validation plots) stays as CSV in that script's own subfolder, never as a workbook sheet.

## Adapting to a different region {#adapting-to-a-different-region}

- **A different GSA or GSA subset** (Adriatic, Ionian, Aegean, Levant, the whole basin): set `FILTER_AREAS` to the target GSA number(s) and point `out_dir`/`pcloud_dir`/`git_dir` at that region's own data — no other code changes needed. GSA-specific corrections (e.g. the Alboran longitude-sign fix) are harmless no-ops outside GSA 1–3.
- **A fully custom, non-GSA region anywhere in the Mediterranean**: set `AREA_MODE <- "custom"` before sourcing `01_biomass.R`, plus `AREA_NAME`, `CUSTOM_AREA_TYPE` (`"bbox"` (default), `"shapefile"`, or `"gsa"` for a custom GSA subset/grouping), and `CUSTOM_BBOX`/`CUSTOM_SHAPEFILE_PATH`/`CUSTOM_GSA_IDS` set to the target boundary — see `01_biomass.R`'s own "Configuration" section for the full set of `AREA_MODE`/`CUSTOM_*` variables. `AREA_MODE == "custom"` skips MEDIAS acoustic survey and GFCM stock-assessment biomass (both are West-Med-subregion-scoped data sources with no meaning outside named GSAs) and runs MEDITS-only — everything else (FG matching, density weighting, PB/QB, diet) is unchanged. (`01_survey_density_custom.R`, the old separate script for this, is deprecated — it now just stops with a pointer back here.)
- `YEAR_ECOPATH` (the Ecopath snapshot years) and `TS_YEARS` (the Ecosim time-series range) are read by every script and must stay consistent across all of them — set them once in a driver script before sourcing the four in order, rather than editing each script's own copy by hand.

## Known gaps {#known-gaps}

As of 2026-09-24, honestly flagged rather than silently worked around:

- **`combine_STAR_RAMlegacy.R` does not exist** — neither the script nor its expected output (`data/fisheries/STAR_RAMLegacy/ combined_medbs_star_ramlegacy.csv`) is anywhere on disk. The GFCM STAR/RAM Legacy stock-assessment override described above is currently a complete no-op for every FG except bluefin tuna/swordfish, which get their own dedicated ICCAT path instead (see the data-sources table above). Building this script — a RAM Legacy Database loader plus a manual-CSV convention for GFCM's own MEDBS/STECF per-stock assessment reports (neither is bulk-downloadable) — is on the to-do list.
- **`marine_megafauna_biomass.csv` does not exist** — cetaceans (5 FGs), monk seals, seabirds (2 FGs), and loggerhead turtles have no biomass source at all yet and are correctly left blank in `Ecopath_B`, not filled with survey noise (see "Survey-exempt FGs" above). Real, cited West-Med-specific density figures are still being sourced.
- **No fleet- or gear-specific fishing mortality (F)** anywhere in the pipeline — `Ecopath_L`/`Ecopath_Di` do carry a fleet split, but `F` and the `PB = M + F` chain it feeds are a single region-wide, all-gears rate per FG. A real fix needs GFCM's Operational-Unit-level DCRF data or STECF's FDI database at species/fleet resolution, neither of which is wired in for this purpose yet.
- **The diet metaweb (`data_entry_metaweb_empty.xlsx`) ships empty** — `04_diets.R` will stop with an explicit error until real predator/prey diet records are entered into its `DATA_ENTRY` sheet.

See the new "Detailed methodology per output block" section of `pipeline_documentation.Rmd` for the full per-block writeup (inputs, formulas, caveats, code locations, and literature references) behind every one of B, C, L, Di, M, TRAITS, F, PB, QB, and DIET.

## Further reading

- `pipeline_documentation.Rmd` — the full script-by-script reference: every input file, every function's signature and return value, every output column — plus a dedicated "Detailed methodology per output block" section covering Biomass, Catches, Landings, Discards, Natural mortality, Traits, Fishing mortality, PB, QB, and Diet, each with its own inputs/sources, exact computation method, known caveats, code location, and references.
- `medbs_pipeline_methodology.qmd` — the methodology write-up, with an interactive map for previewing a GSA-based vs. custom-region setup and generating a ready-to-paste config block.
