# EwE diet composition builder (West Med model) — R version

Builds a predator x prey diet-composition matrix ready to import into EwE,
starting from a local Access stomach-content database and optionally
blended with diet proportions imported from other sources (papers, other
EwE models, expert opinion).

## 1. Install dependencies

```r
install.packages(c("dplyr", "tidyr", "readr", "readxl", "openxlsx", "optparse"))
```

For the Access database itself, you need **one** of:

- **Windows** (simplest, works with the driver that ships with Access/Office):
  ```r
  install.packages("RODBC")
  ```
- **Cross-platform** (odbc/DBI + a 3rd-party Access ODBC driver, e.g. the
  free "Microsoft Access Database Engine" redistributable on Windows, or an
  Access ODBC driver via unixODBC on macOS/Linux):
  ```r
  install.packages(c("odbc", "DBI"))
  ```

If you'd rather not deal with ODBC drivers at all, export the stomach-content
table to CSV/XLSX from within Access and use `--stomach_csv` instead of
`--access_db` — the rest of the pipeline (mapping, aggregation, blending,
export) is identical either way.

## 2. Edit the config block at the top of `diet_to_ewe.R`

- `stomach_columns` — map your Access table's actual field names onto the
  names this script expects (predator species, prey species, %W, stomach ID...).
- `stomach_table` — the table (or a SQL query) inside the Access DB.
- `ewe_group_list` — paste in your West Med model's full functional group
  list, in Ecopath order, so the output matrix has every group as a row
  even where you have no diet data yet.
- `min_n_stomachs` / `local_blend_weight` — tune how much to trust local
  data vs. external sources (see below).

## 3. Fill in the species -> EwE group mapping

Copy `species_to_ewe_group_TEMPLATE.csv`, rename it, and fill in every
species that appears in your stomach-content DB (as both predator and
prey) mapped onto your EwE functional group names. The script tells you
(in the QA report) which species from the database it couldn't find in
this file, so you can run once, extend the mapping, and re-run.

## 4. (Optional) Prepare external diet sources

If you want to bring in diet proportions from elsewhere (a published diet
matrix, a neighbouring EwE model's basic estimates, expert judgement) to
fill gaps or supplement thin local data, format them like
`external_diet_source_TEMPLATE.csv`: one row per (predator_group,
prey_group, proportion), already using your EwE group names. You can pass
as many of these as you like.

## 5. Run it

From a terminal (Rscript):

```
Rscript diet_to_ewe.R \
    --access_db "C:/data/StomachDB.accdb" \
    --mapping species_to_ewe_group.csv \
    --external lit_diets.csv:Coll2018:1.0 \
    --external other_model.csv:AdriaticModel:0.5 \
    --out ewe_diet_composition.xlsx
```

Or, without Access:

```
Rscript diet_to_ewe.R \
    --stomach_csv stomach_export.csv \
    --mapping species_to_ewe_group.csv \
    --out ewe_diet_composition.xlsx
```

`--external PATH:SOURCE_NAME:WEIGHT` is repeatable; `SOURCE_NAME` and
`WEIGHT` are optional (defaults to the filename and weight 1.0).

### Running interactively in RStudio instead

The script guards its command-line block so you can also `source()` it and
call the pipeline function directly, e.g.:

```r
source("diet_to_ewe.R")
run_pipeline(
  stomach_csv = "stomach_export.csv",
  mapping_path = "species_to_ewe_group.csv",
  external_specs = c("lit_diets.csv:Coll2018:1.0"),
  out_path = "ewe_diet_composition.xlsx",
  min_n = 10,
  local_weight = 0.7
)
```

## What it does, in order

1. Reads raw stomach-content records (one row per prey item per stomach).
2. Maps predator/prey species names to EwE functional groups.
3. Aggregates into mean diet proportions per (predator group, prey group),
   tracking how many stomachs back each predator group's estimate.
4. Loads any external diet-proportion tables you supplied.
5. Blends the two: for a predator with >= `min_n_stomachs` local stomachs,
   local data is weighted `local_blend_weight` against external sources
   (default 70% local / 30% external); predators with too few local
   stomachs (or none at all) fall back to external sources; predators with
   neither keep whatever sparse local data exists so you can see the gap.
6. Pivots into a full prey x predator matrix reindexed to your complete EwE
   group list, renormalises every predator column to sum to 1, and writes
   it to Excel in EwE's expected layout (predator groups across the top,
   prey groups down the side).

A QA report prints at the end: species found in the DB that aren't in your
mapping table yet (so records involving them get dropped until you add
them), predator groups below the stomach-count threshold, and any column
that didn't sum to ~1 (~100) before normalisation, in case that signals a
data problem upstream rather than something to just silently fix.

## Customizing the aggregation method

`aggregate_local_diet()` currently uses the standard "mean %W" method
(average each prey's proportion across all stomachs for a predator group,
then renormalise). If you'd rather weight by prey item count, use %N, or
weight by predator size class, that function is the only place you need
to change — it takes the mapped long-format stomach table and returns
`data.frame(predator_group, prey_group, proportion, n_stomachs)`.

## Note

This is a straight R port of an earlier Python version of the same tool
(same config layout, same blending logic, same output format) — tested
against identical synthetic input and confirmed to produce numerically
identical results.

---

## Pulling an external source straight from FishBase / SeaLifeBase

`fetch_external_diet_fishbase.R` is a companion script: it fetches
published diet-composition data directly from FishBase (finfish) and
SeaLifeBase (invertebrates, marine mammals, reptiles — anything not a
fish) via the R packages `rfishbase` and `dietr`, and writes it out in
exactly the `predator_group,prey_group,proportion` shape that
`diet_to_ewe.R`'s `--external` flag expects — so it's a ready-made
external diet source, no manual digitising needed.

### Install

```r
install.packages(c("rfishbase", "dietr", "dplyr", "readr"))
```

`rfishbase` downloads and locally caches FishBase/SeaLifeBase's reference
tables the first time you use it (needs network access; can take a
minute or two the first run, instant after that).

### Run it

```
Rscript fetch_external_diet_fishbase.R \
    --species "Merluccius merluccius,Mullus barbatus,Delphinus delphis" \
    --mapping species_to_ewe_group.csv \
    --out external_diet_fishbase.csv
```

Pass one mixed species list — fish and non-fish together — the script
tries FishBase first and automatically retries anything not found there
against SeaLifeBase, so you don't need to sort species by database
yourself.

Then feed the result straight into the main pipeline as another
`--external` source, same as any other:

```
Rscript diet_to_ewe.R --stomach_csv stomach_export.csv \
    --mapping species_to_ewe_group.csv \
    --external external_diet_fishbase.csv:FishBase:0.5 \
    --out ewe_diet_composition.xlsx
```

### What it does

1. Fetches quantitative diet studies (real %diet-by-weight/volume/number,
   via `dietr::ConvertFishbaseDiet()`) for every species in your list.
2. For species with no quantitative studies, falls back to FishBase's
   *ranked* food-item lists (no percentages, just "these are the top prey
   items, in this order") and converts rank into an approximate proportion
   (geometric decay by rank) — these rows are tagged `fishbase_ranked` /
   `sealifebase_ranked` in the console output so you can tell them apart
   from real percentages; treat them as a rough steer, not hard data.
3. Maps both predator species and prey taxa onto your EwE groups via
   `species_to_ewe_group.csv` — **note**: FishBase/SeaLifeBase often name
   prey at a coarse taxonomic level (e.g. "Crustacea", "Teleostei") rather
   than species, so your mapping file needs entries for those category
   names too (the template now includes some illustrative examples at the
   bottom — adjust to your actual groups). Anything it can't map is
   dropped and listed on the console so you can extend the mapping and
   re-run.
4. Aggregates to mean proportion per (predator group, prey group),
   renormalises each predator to sum to 1, writes the CSV.

### A caveat worth knowing

This script was written and syntax/logic-tested (the taxon-picking,
mapping, aggregation, and rank-weighting logic all pass unit tests against
synthetic data) in an environment with no network access to CRAN or
FishBase, so it could not be run end-to-end against live FishBase/
SeaLifeBase data before delivery. It's built on `dietr`'s
`ConvertFishbaseDiet()`/`ConvertFishbaseFood()`, whose output columns
(`Species`, `FoodI`/`FoodII`/`FoodIII`, `Stage`, `DietPercent`) are
documented and have been stable across package versions, rather than
hand-parsing `rfishbase`'s raw `diet()`/`diet_items()` tables (whose exact
column names aren't fully pinned down in the package docs). The first time
you run it for real, check the preview rows it prints against what you'd
expect before trusting the output — if a column name has drifted in a
newer `dietr`/`rfishbase` release, that preview is where it'll show up.
