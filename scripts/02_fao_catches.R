## =================================================================
## PIPELINE STEP 2 of 4
## REQUIRES Step 1 (01_survey_density_westmed.R or
## 01_survey_density_custom.R) to have already run - this script reads
## strata_area_by_area.csv, written by that step, to convert catch
## totals into a t/km^2 density. Running this before Step 1 will fail
## with a clear error pointing back here.
## Produces: Catches_Ecopath/Catches_Ecosim sheets in
## output/ecopath_ecosim_inputs.xlsx.
## =================================================================

## =================================================================
## WMed EwE Model - Catch data pipeline
##
## Structure: config-driven instead of repeated if/else branches per
## stage. GFCM_2025 and FAO_2026 share one processing path (they're
## structurally identical - same CL_FI_* reference files); FAO_2020
## stays separate since it's a genuinely different source format.
##
## NOTE on FAO_2026: the global FAO capture database does NOT have
## the fine 37.1.1/37.1.2/37.1.3 division breakdown that GFCM's own
## regional database has - it only reports at Major Fishing Area
## (37) level. The area-filter config below reflects this explicitly
## rather than silently applying a division filter that wouldn't work.
## =================================================================

## =================================================================
## STEP 1: Configuration
## =================================================================
if (tolower(Sys.info()[["user"]]) == "daniel") {
  out_dir <- "/Users/daniel/Work/iMARES/WMed EwE Model/output/"
  pcloud_dir   <- "/Users/daniel/pCloud Drive/EwE Western Med 2026/"
  git_dir <-"/Users/daniel/Documents/GitHub/WMed_EwE/"
} else {
  ## Falls back to an interactive directory picker in RStudio, rather
  ## than just stopping with "set it manually" - so this script works
  ## for anyone, not just the one hardcoded username above.
  if (!requireNamespace("rstudioapi", quietly = TRUE) ||
      !rstudioapi::isAvailable()) {
    stop(
      "This script requires RStudio. Please select the output directory manually."
    )
  }
  rstudioapi::showQuestion(
    title = "Select Output Directory",
    message = paste(
      "Please select the directory where output files",
      "and intermediate results will be saved."
    )
  )
  out_dir <- rstudioapi::selectDirectory()
  if (is.null(out_dir) || out_dir == "" || !dir.exists(out_dir)) {
    stop("No valid output directory selected.")
  }
  
  if (!requireNamespace("rstudioapi", quietly = TRUE) ||
      !rstudioapi::isAvailable()) {
    stop(
      "This script requires RStudio. Please select the pCloud Drive/EwE Western Med 2026 folder."
    )
  }
  rstudioapi::showQuestion(
    title = "Select pCloud EwE West Med Directory",
    message = paste(
      "Please select the location of the the pCloud Drive/EwE Western Med 2026 folder."
    )
  )
  
  pcloud_dir <- rstudioapi::selectDirectory()
  if (is.null(out_dir) || out_dir == "" || !dir.exists(out_dir)) {
    stop("No valid pcloud directory selected.")
  }
  
  if (!requireNamespace("rstudioapi", quietly = TRUE) ||
      !rstudioapi::isAvailable()) {
    stop(
      "This script requires RStudio. Please select the github directory manually."
    )
  }
  rstudioapi::showQuestion(
    title = "Select Github WMed_EwE Directory",
    message = paste(
      "Please select the directory where you cloned the WMed_EwE repository."
    )
  )
  git_dir <- rstudioapi::selectDirectory()
  if (is.null(out_dir) || out_dir == "" || !dir.exists(out_dir)) {
    stop("No valid Github directory selected.")
  }
}

## =================================================================
## Package loading - same pattern as 01_survey_density_westmed.R:
## one list, auto-installs anything missing, loads everything. This
## also covers the rfishbase common_names() lookup used later in the
## inline FG-matching pipeline (previously loaded separately, mid-
## script) and stringr (previously only loaded there too).
## =================================================================
pkgs <- c("data.table", "ggplot2", "stringr", "rfishbase", "readxl", "openxlsx")
new_pkgs <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new_pkgs) > 0) install.packages(new_pkgs)
invisible(lapply(pkgs, library, character.only = TRUE))

source(file.path(git_dir, "scripts/lib_survey_fg_density_functions.R"))

## Must match 01_survey_density_westmed.R's own YEAR_ECOPATH - this is the
## snapshot period Catches_Ecopath averages over, and it needs to
## describe the SAME years as the workbook's existing Ecopath (Biomass)
## sheet, or the two sheets end up describing different time periods
## under the same base-year label.
YEAR_ECOPATH <- 1994:1996
ECOPATH_WORKBOOK_PATH <- file.path(out_dir, "ecopath_ecosim_inputs.xlsx")

## plot_dir nested inside out_dir, same convention as
## 01_survey_density_westmed.R - everything this script produces lands
## somewhere under out_dir, nothing written to a separate location.
plot_dir <- file.path(out_dir, "plots")
if (!dir.exists(plot_dir)) dir.create(plot_dir, recursive = TRUE)

START_YEAR <- 1995
TOP_N_SPECIES <- 20

## =================================================================
## RAW DATA SUBPATHS - confirmed against the actual files on disk
## (via `find`/`file.exists()` checks, see chat history) rather than
## guessed. GFCM_2025's capture/reference CSVs are confirmed under
## git_dir/data/raw/ - genuinely git-versioned, not synced via pCloud.
## FG_WMed.xlsx is confirmed under pcloud_dir/data/, matching
## 01_survey_density_westmed.R's own fg_file convention - there may be
## a second copy under git_dir too (unconfirmed either way), so if the
## two ever drift out of sync, whichever script ran would silently use
## a different FG reference than the other - worth checking if that
## becomes relevant.
##
## FAO_2020_DATA_SUBDIR is UNCONFIRMED - only GFCM_2025 has been
## verified on disk. DATASET_VERSION below defaults to GFCM_2025, so
## this doesn't block anything right now, but expect the same kind of
## "file not found" error if you switch DATASET_VERSION to "FAO_2020"
## before confirming its actual location the same way.
GFCM_2025_DATA_SUBDIR  <- "data/raw/FI_Regional_2025.1.0"
FAO_2020_DATA_SUBDIR   <- "data/raw/FAO-GFCM_catches"
FG_REFERENCE_SUBPATH   <- "data/FG_WMed.xlsx"

## Forces file= interpretation (bypasses fread's input-must-be-guessed
## heuristic entirely - the actual fix for the shell-command fallback,
## per the fread() warning's own pointer to NEWS item 5 for v1.11.6),
## AND checks file.exists() itself first so a genuinely missing file
## fails with a plain, specific message instead of either fread's
## generic "size 0 - returning NULL" warning or the shell error.
safe_fread <- function(path, label = path) {
  if (!file.exists(path)) {
    stop("File not found for '", label, "': '", path, "'",
         " - check pcloud_dir/git_dir and the *_DATA_SUBDIR constants above",
         " match this file's actual location.")
  }
  fread(file = path)
}

DATASET_VERSION <- "GFCM_2025"   # "FAO_2020" | "GFCM_2025" | "FAO_2026"

## =================================================================
## Dataset registry - one entry per source, all config in one place.
## Adding a new dataset version means adding ONE entry here, not
## touching six separate processing blocks.
##
## GFCM_2025's data_dir (capture/reference CSVs) is under git_dir
## (confirmed on disk); fg_file is under pcloud_dir (confirmed on
## disk, matches 01_survey_density_westmed.R). FAO_2020's are still
## under pcloud_dir but UNCONFIRMED.
## =================================================================

DATASETS <- list(
  GFCM_2025 = list(
    format = "gfcm_regional",
    data_dir = file.path(git_dir, GFCM_2025_DATA_SUBDIR),
    capture_file   = "GFCM_Capture_Quantity.csv",
    species_file   = "CL_FI_SPECIES_GROUPS.csv",
    countries_file = "CL_FI_COUNTRY_GROUPS.csv",
    area_file      = "CL_FI_WATERAREA_DIVISION.csv",
    area_code_col  = "DIVISION.CODE",     # column in capture data to filter on
    area_join_col  = "Code",              # matching column in the area reference file
    area_filter_values = c("37.1.1", "37.1.2", "37.1.3"),
    fg_file = file.path(pcloud_dir, FG_REFERENCE_SUBPATH)
  ),
  # FAO_2026 = list(
  #   format = "gfcm_regional",   # same processing shape as GFCM_2025
  #   data_dir = file.path(pcloud_dir, "data/Capture_2026.1.0"),
  #   capture_file   = "Capture_Quantity.csv",
  #   species_file   = "CL_FI_SPECIES_GROUPS.csv",
  #   countries_file = "CL_FI_COUNTRY_GROUPS.csv",
  #   area_file      = "CL_FI_WATERAREA_GROUPS.csv",
  #   area_code_col  = "AREA.CODE",
  #   area_join_col  = "Code",
  #   ## global FAO data only has Major Fishing Area level (37), not the
  #   ## finer 37.1.x subdivisions GFCM's own regional database has -
  #   ## filtering to "37" here reflects that limitation explicitly
  #   area_filter_values = c("37"),
  #   fg_file = file.path(pcloud_dir, "data/FG_WMed.xlsx")
  # ),
  FAO_2020 = list(
    format = "legacy_excel",
    catch_file = file.path(pcloud_dir, FAO_2020_DATA_SUBDIR, "FAO-GFCM-CapturepProduction-1970_2023.xlsx"),
    fg_file    = file.path(pcloud_dir, FG_REFERENCE_SUBPATH)
  )
)

cfg <- DATASETS[[DATASET_VERSION]]

## =================================================================
## Loaders - one function per format, not per dataset version
## =================================================================

load_gfcm_regional <- function(cfg) {
  p <- function(f) file.path(cfg$data_dir, f)
  
  capture   <- safe_fread(p(cfg$capture_file), "capture_file")
  species   <- safe_fread(p(cfg$species_file), "species_file")
  if (!all(c("3A_Code", "Name_En") %in% names(species))) {
    stop("species_file ('", cfg$species_file, "') is missing expected column(s)",
         " '3A_Code'/'Name_En' - got: ", paste(names(species), collapse = ", "),
         ". Either the file read as empty (check the file.exists() error above",
         " didn't get skipped) or this dataset version uses different column names.")
  }
  species <- species[, .(SPECIES.ALPHA_3_CODE = `3A_Code`, Species = Name_En)]
  
  countries <- safe_fread(p(cfg$countries_file), "countries_file")[
    , .(COUNTRY.UN_CODE = UN_Code, Country = Name_En)]
  area_ref  <- safe_fread(p(cfg$area_file), "area_file")
  setnames(area_ref, cfg$area_join_col, "AreaCode")
  area_ref  <- unique(area_ref[, .(AreaCode, Division = Name_En)])
  
  capture <- merge(capture, species, by = "SPECIES.ALPHA_3_CODE", all.x = TRUE)
  capture <- merge(capture, countries, by = "COUNTRY.UN_CODE", all.x = TRUE)
  setnames(capture, cfg$area_code_col, "AreaCode")
  capture <- merge(capture, area_ref, by = "AreaCode", all.x = TRUE)
  capture[, PERIOD := as.numeric(PERIOD)]
  
  capture
}


load_legacy_excel <- function(cfg) {
  as.data.table(readxl::read_excel(cfg$catch_file))
}

## =================================================================
## Build Western Med time series - one function, format-aware only
## where genuinely necessary (column names differ by source)
## =================================================================

build_westmed_timeseries <- function(cfg) {
  if (cfg$format == "gfcm_regional") {
    capture <- load_gfcm_regional(cfg)
    
    westmed <- capture[
      AreaCode %in% cfg$area_filter_values &
        MEASURE == "Q_tlw" &
        PERIOD >= START_YEAR
    ]
    
    country_ts <- westmed[, .(Catch = sum(VALUE, na.rm = TRUE)),
                          by = .(Year = PERIOD, Country, Division)]
    species_ts <- westmed[, .(Catch = sum(VALUE, na.rm = TRUE)),
                          by = .(Year = PERIOD, Species, Division)]
    
    list(westmed = westmed, country_ts = country_ts, species_ts = species_ts,
         value_col = "VALUE", species_col = "Species")
    
  } else if (cfg$format == "legacy_excel") {
    catch_data <- load_legacy_excel(cfg)
    setnames(catch_data,
             c("Species (scientific name)", "Area (FAO division)", "Area (FAO subarea)"),
             c("Species", "Division", "Subarea"), skip_absent = TRUE)
    
    westmed <- catch_data[Subarea == "Western Med (37.1)" & Year >= START_YEAR]
    
    country_ts <- westmed[, .(Catch = sum(Quantity, na.rm = TRUE)),
                          by = .(Year, Country, Division)]
    species_ts <- westmed[, .(Catch = sum(Quantity, na.rm = TRUE)),
                          by = .(Year, Species, Division)]
    
    list(westmed = westmed, country_ts = country_ts, species_ts = species_ts,
         value_col = "Quantity", species_col = "Species")
  }
}

ts_data <- build_westmed_timeseries(cfg)
message("Dataset loaded and processed: ", DATASET_VERSION)

## =================================================================
## Functional Group matching
## Delegates to the dedicated matching pipeline (common-name bridge,
## FAO genus fallback, ambiguity-safe resolution, manual overrides -
## see species_fg_matching_final.R) rather than a naive anti_join,
## which only catches exact scientific-name matches and silently
## leaves everything else unresolved.
## =================================================================

fg <- as.data.table(readxl::read_excel(cfg$fg_file, sheet = 4))

unmatched_species <- ts_data$westmed[
  !get(ts_data$species_col) %in% fg$ESPECIE,
  .(Catch = sum(get(ts_data$value_col), na.rm = TRUE)),
  by = c(ts_data$species_col)
][order(-Catch)]
setnames(unmatched_species, ts_data$species_col, "Species")
unmatched_species[, `:=`(FG_num = NA, FG_name = NA)]

message(nrow(unmatched_species), " species need FG matching - the matching pipeline",
        " further down in this same script (STEP 0-4, starting at '# unmatched",
        " species ####') runs automatically after this point and produces `output`",
        " (species_fg_matched.csv) before it's needed below.")
## NOTE: there is no separate species_fg_matching_final.R file on disk - the
## matching logic lives entirely in the second half of THIS script (below
## the Export section). A source() call to an external copy of that name
## used to sit here and failed with "cannot open the connection" because
## that file was never actually saved on its own - removed rather than
## pointed at a real path, since the inline copy already runs in sequence
## and does the same job. If you DO want it split into its own file later
## (e.g. to reuse it from 04_pbqb_calc.R or elsewhere), cut everything from
## '# unmatched species ####' down to the species_fg_matched.csv fwrite()
## into a real species_fg_matching_final.R and source() that instead.

## =================================================================
## Summary statistics
## =================================================================

top_species <- ts_data$westmed[
  , .(TotalCatch = sum(get(ts_data$value_col), na.rm = TRUE)),
  by = c(ts_data$species_col)
][order(-TotalCatch)][1:TOP_N_SPECIES, get(ts_data$species_col)]

species_plot <- ts_data$species_ts[Species %in% top_species]

## =================================================================
## Plots - one shared definition, both sources feed the same schema
## (Year, Country/Species, Division, Catch) by this point
## =================================================================

p_country <- ggplot(ts_data$country_ts, aes(x = Year, y = Catch, colour = Country)) +
  geom_line() +
  facet_wrap(~ Division, scales = "free_y") +
  theme_bw() +
  labs(x = "Year", y = "Catch (t)")

p_species <- ggplot(species_plot, aes(x = Year, y = Catch, colour = Division, group = Division)) +
  geom_line(linewidth = 0.8) +
  facet_wrap(~ Species, scales = "free_y", ncol = 4) +
  theme_bw() +
  labs(x = "Year", y = "Catch (t)", colour = NULL) +
  theme(legend.position = "bottom", strip.text = element_text(size = 8))

print(p_country)
print(p_species)

## =================================================================
## Export
## =================================================================

fwrite(unmatched_species, file.path(out_dir, "unmatched_species_FG_assignment.csv"))
fwrite(ts_data$species_ts, file.path(out_dir, "westmed_species_timeseries.csv"))

ggsave(file.path(plot_dir, "country_catch_timeseries.png"), p_country, width = 12, height = 8, dpi = 300)
ggsave(file.path(plot_dir, "species_catch_timeseries.png"), p_species, width = 14, height = 10, dpi = 300)

message("Analysis completed.")

# unmatched species ####

## =================================================================
## Species -> Functional Group matching pipeline (clean, final)
##
## Goal: assign FG_num / FG_name to every species in unmatched_species.
##
## Inputs assumed to exist:
##   unmatched_species : data.table - Species (common name), Catch,
##                        FG_num (NA), FG_name (NA)
##   fg                : FG reference - GF, FG_name, ESPECIE (sci name)
##
## Matching stages, in order of confidence:
##   1. Direct match: Species text == an FG_name itself
##   2. Common name -> scientific name (FishBase + SeaLifeBase, queried
##      FOR the known species list - not searched from all of FishBase,
##      which caused wrong-species matches in an earlier version)
##   3. FAO genus-level fallback (for NEI/aggregate categories that
##      aren't real species, e.g. "Sardinellas nei")
##   4. Word-containment fallback (for anything still unmatched)
##
## Every stage uses the SAME safety rule: a query is only auto-resolved
## if it maps to exactly ONE distinct FG. Anything mapping to multiple
## different FGs is flagged for manual review, never guessed - this is
## what prevents a single Catch value from being silently duplicated
## across multiple species/FGs.
## =================================================================

## data.table/stringr/rfishbase already loaded at the top of this
## script - no need to reload them here mid-pipeline.

setDT(unmatched_species)
setDT(fg)

## --- Shared safety helper, used at every matching stage --------------------
## query_col: the original species name being matched
## fg_col:    the FG_num column in the merged candidate table
## Only collapses to one row when a query maps to exactly one distinct FG;
## anything ambiguous is returned separately, never auto-picked.
resolve_matches_safely <- function(merged_dt, query_col = "Species", fg_col = "FG_num") {
  n_fg <- merged_dt[, .(n_distinct_fg = uniqueN(get(fg_col))), by = query_col]
  safe_queries <- n_fg[n_distinct_fg == 1][[query_col]]
  ambiguous_queries <- n_fg[n_distinct_fg > 1][[query_col]]
  list(
    safe = unique(merged_dt[get(query_col) %in% safe_queries], by = query_col),
    ambiguous = merged_dt[get(query_col) %in% ambiguous_queries]
  )
}

extract_genus <- function(sci_name) str_extract(sci_name, "^[A-Za-z]+")

## =================================================================
## STEP 0: data quality - set aside rows with no species name
## =================================================================

blank_row <- unmatched_species[is.na(Species) | Species == ""]
if (nrow(blank_row) > 0) {
  message(nrow(blank_row), " row(s) with blank Species name - excluded,",
          " investigate separately (upstream data issue):")
  print(blank_row)
}
unmatched_species <- unmatched_species[!is.na(Species) & Species != ""]

fg_lookup <- unique(fg[, .(ScientificName = ESPECIE, FG_num = GF, FG_name)])
fg_lookup[, genus := extract_genus(ScientificName)]

## =================================================================
## STEP 1: direct match - Species text is itself an FG_name
## e.g. "Echinoderms" catch record matching straight to the FG
## literally named "Echinoderms"
## =================================================================

fg_name_lookup <- unique(fg[, .(Species = FG_name, FG_num = GF, FG_name)])
direct_merged <- merge(unmatched_species[, .(Species, Catch)], fg_name_lookup, by = "Species")
direct_resolved <- resolve_matches_safely(direct_merged)

direct_matches <- direct_resolved$safe[, .(Species, FG_num, FG_name)]
direct_matches[, match_method := "direct_fg_name"]

message("STEP 1 - Direct Species==FG_name matches: ", nrow(direct_matches))

## =================================================================
## STEP 2: common name -> scientific name, queried FOR the known
## species list (not searched from all of FishBase/SeaLifeBase -
## that caused wrong-species matches, since common names aren't
## globally unique, e.g. "Red mullet" also names an Australian species)
## =================================================================

sci_names <- unique(fg_lookup$ScientificName)
sci_names <- sci_names[str_detect(sci_names, "^[A-Z][a-z]+ [a-z]+$")]  # proper binomials only

message("\nSTEP 2 - Querying common names for ", length(sci_names), " species...")

fb_common  <- tryCatch(as.data.table(common_names(sci_names, server = "fishbase")),
                       error = function(e) data.table())
slb_common <- tryCatch(as.data.table(common_names(sci_names, server = "sealifebase")),
                       error = function(e) data.table())

fb_lookup <- unique(
  rbindlist(list(fb_common, slb_common), fill = TRUE)[
    Language == "English" & !is.na(ComName), .(ScientificName = Species, Species = ComName)]
)
message("Common names found: ", nrow(fb_lookup), " covering ",
        uniqueN(fb_lookup$ScientificName), " of ", length(sci_names), " species")

clean_name <- function(x) {
  x <- str_remove(x, "\\(.*\\)")
  x <- str_remove(x, regex("\\bnei\\b", ignore_case = TRUE))
  x <- str_remove(x, "'s\\b")
  str_to_lower(str_squish(x))
}

remaining <- unmatched_species[!Species %in% direct_matches$Species]
remaining[, clean_species := clean_name(Species)]
fb_lookup[, clean_species := clean_name(Species)]

exact_merged <- merge(remaining[, .(Species, Catch, clean_species)],
                      fb_lookup[, .(clean_species, ScientificName)],
                      by = "clean_species", allow.cartesian = TRUE)
exact_merged <- merge(exact_merged, fg_lookup[, .(ScientificName, FG_num, FG_name)], by = "ScientificName")
exact_resolved <- resolve_matches_safely(exact_merged)

exact_matches <- exact_resolved$safe[, .(Species, FG_num, FG_name)]
exact_matches[, match_method := "fishbase_common_name"]

## Distinguish genuine ambiguity (different species sharing a wrong/loose
## common name, e.g. "Turbot" incorrectly listing Balistes capriscus)
## from life-stage splits (the SAME species appearing as separate FG rows
## for juvenile/adult stanzas - a normal Ecopath model structure, not an
## error - a single catch record genuinely belongs to multiple FGs here)
if (nrow(exact_resolved$ambiguous) > 0) {
  ambiguity_type <- exact_resolved$ambiguous[, .(n_species = uniqueN(ScientificName)), by = Species]
  exact_resolved$ambiguous <- merge(exact_resolved$ambiguous, ambiguity_type, by = "Species")
  exact_resolved$ambiguous[, ambiguity_type := fifelse(
    n_species == 1, "life_stage_split", "genuine_name_ambiguity"
  )]
}

message("STEP 2 - Resolved: ", nrow(exact_matches),
        " | Ambiguous (multiple FGs, excluded): ", uniqueN(exact_resolved$ambiguous$Species))
if (uniqueN(exact_resolved$ambiguous$Species) > 0) {
  print(unique(exact_resolved$ambiguous[, .(Species, ScientificName, FG_num, FG_name, ambiguity_type)]))
}

## =================================================================
## STEP 3: FAO genus-level fallback for NEI/aggregate categories
## (e.g. "Sardinellas nei" -> genus Sardinella -> FG)
## =================================================================

remaining <- remaining[!Species %in% exact_matches$Species]

find_or_download_fao_species <- function() {
  ## searches pcloud_dir recursively - this is very likely already
  ## present there anyway, since GFCM_2025's own species_file
  ## (CL_FI_SPECIES_GROUPS.csv) is the same reference file - only
  ## downloads a fresh copy if genuinely not found anywhere under
  ## pcloud_dir. Absolute path search/download, not "." (ambient working
  ## directory) - relying on "." here would have the same class of bug
  ## the setwd()-removal above was meant to eliminate.
  found <- list.files(pcloud_dir, pattern = "CL_FI_SPECIES_GROUPS.csv$",
                      recursive = TRUE, full.names = TRUE, ignore.case = TRUE)
  if (length(found) > 0) return(found[1])
  url <- "https://data.apps.fao.org/catalog/dataset/b70c52c1-475f-4951-a8ac-de44016abd9b/resource/2c0f936d-6c36-4715-9c7f-fa5a70c00249/download/cl_fi_species_groups.csv"
  destfile <- file.path(out_dir, "CL_FI_SPECIES_GROUPS.csv")
  download.file(url, destfile = destfile, mode = "wb", method = "libcurl")
  destfile
}

fao_species <- fread(file = find_or_download_fao_species(), encoding = "UTF-8")
name_col <- grep("english|name.*en$|^name$", names(fao_species), ignore.case = TRUE, value = TRUE)[1]
sci_col  <- grep("scientific", names(fao_species), ignore.case = TRUE, value = TRUE)[1]
setnames(fao_species, c(name_col, sci_col), c("Name_En", "Scientific_Name"), skip_absent = TRUE)
fao_species[, genus := extract_genus(Scientific_Name)]

remaining <- merge(remaining, unique(fao_species[, .(Name_En, genus)], by = "Name_En"),
                   by.x = "Species", by.y = "Name_En", all.x = TRUE)

genus_merged <- merge(remaining[!is.na(genus), .(Species, Catch, genus)],
                      fg_lookup[!is.na(genus), .(genus, FG_num, FG_name)],
                      by = "genus", allow.cartesian = TRUE)
genus_resolved <- resolve_matches_safely(genus_merged)

genus_matches <- genus_resolved$safe[, .(Species, FG_num, FG_name)]
genus_matches[, match_method := "fao_genus"]

message("\nSTEP 3 - Resolved via genus: ", nrow(genus_matches),
        " | Ambiguous (excluded): ", uniqueN(genus_resolved$ambiguous$Species))
if (uniqueN(genus_resolved$ambiguous$Species) > 0) {
  print(unique(genus_resolved$ambiguous[, .(Species, genus, FG_num, FG_name)]))
}

## =================================================================
## STEP 4: word-containment fallback for anything still unmatched
## =================================================================

remaining <- remaining[!Species %in% genus_matches$Species]

FILLER_WORDS <- c("nei", "spp", "sp", "etc", "and", "or", "the", "of")
tokenize <- function(x) {
  words <- str_split(str_to_lower(str_remove_all(x, "[,().']")), "\\s+")[[1]]
  words[!words %in% FILLER_WORDS & words != ""]
}

fb_with_fg <- merge(fb_lookup, fg_lookup[, .(ScientificName, FG_num, FG_name)], by = "ScientificName")
candidate_names <- unique(fb_with_fg$Species)
candidate_tokens <- setNames(lapply(candidate_names, tokenize), candidate_names)

match_by_containment <- function(query) {
  q_tokens <- tokenize(query)
  if (length(q_tokens) == 0) return(NULL)
  scores <- vapply(candidate_tokens, function(c_tokens) {
    if (length(c_tokens) == 0) return(0)
    sum(q_tokens %in% c_tokens) / length(q_tokens)
  }, numeric(1))
  best <- which(scores == 1)   # full containment only - partial is too unreliable to auto-use
  if (length(best) == 0) return(NULL)
  data.table(Species = query, candidate = names(candidate_tokens)[best])
}

containment_results <- rbindlist(lapply(remaining$Species, match_by_containment))

if (nrow(containment_results) > 0) {
  containment_merged <- merge(containment_results, fb_with_fg[, .(Species, FG_num, FG_name)],
                              by.x = "candidate", by.y = "Species")
  containment_resolved <- resolve_matches_safely(containment_merged)
  containment_matches <- containment_resolved$safe[, .(Species, FG_num, FG_name)]
  containment_matches[, match_method := "word_containment"]
  
  message("\nSTEP 4 - Resolved via word containment: ", nrow(containment_matches),
          " | Ambiguous (excluded): ", uniqueN(containment_resolved$ambiguous$Species))
} else {
  containment_matches <- data.table(Species = character(), FG_num = numeric(),
                                    FG_name = character(), match_method = character())
  message("\nSTEP 4 - No word-containment matches found.")
}

## =================================================================
## COMBINE + FINAL RESULT
## =================================================================

all_matches <- rbindlist(list(direct_matches, exact_matches, genus_matches, containment_matches))

resolved <- merge(unmatched_species[, .(Species, Catch)], all_matches, by = "Species", all.x = TRUE)
still_failing <- resolved[is.na(FG_num)]

## sanity check - Catch must never be duplicated by any matching stage
original_total <- sum(unmatched_species$Catch)
resolved_total <- sum(resolved[!is.na(FG_num)]$Catch, na.rm = TRUE) +
  sum(unmatched_species[Species %in% still_failing$Species]$Catch)
message("\n=== CATCH TOTAL CHECK (must match exactly) ===")
message("Original total: ", sum(unmatched_species$Catch))
message("Sum across resolved+unresolved: ", sum(resolved$Catch))
if (abs(sum(unmatched_species$Catch) - sum(resolved$Catch)) > 0.01) {
  message("WARNING: totals differ - duplication somewhere, do not trust results yet.")
} else {
  message("OK - no duplication.")
}

message("\n=== SUMMARY (before manual overrides) ===")
message("Total species: ", nrow(resolved))
message("Resolved: ", nrow(resolved) - nrow(still_failing),
        " (", round(100 * (nrow(resolved) - nrow(still_failing)) / nrow(resolved), 1), "%)")
message("By method:")
print(all_matches[, .N, by = match_method])

## =================================================================
## MANUAL OVERRIDES
## Add a row per species you've manually confirmed the correct
## scientific name for (e.g. Turbot really is Scophthalmus maximus,
## not Balistes capriscus, which was a wrong FishBase/SeaLifeBase
## common-name entry). The script looks up that scientific name's
## real FG from fg_lookup - you never need to hardcode an FG number
## by hand, so this stays correct even if fg_lookup changes.
## =================================================================

MANUAL_OVERRIDES <- data.table(
  Species = c("Turbot"),
  CorrectScientificName = c("Scophthalmus maximus")
  # add more rows here as you confirm them, e.g.:
  # "European hake" -> "Merluccius merluccius" (if the ambiguity turns out
  # to be genuine mismatch rather than a life-stage split - check the
  # ambiguity_type column from STEP 2 first)
)

overrides_resolved <- merge(MANUAL_OVERRIDES, fg_lookup[, .(ScientificName, FG_num, FG_name)],
                            by.x = "CorrectScientificName", by.y = "ScientificName")
overrides_resolved[, match_method := "manual_override"]

unresolved_override_names <- setdiff(MANUAL_OVERRIDES$Species, overrides_resolved$Species)
if (length(unresolved_override_names) > 0) {
  message("\nWARNING - these override scientific names weren't found in fg_lookup",
          " at all - check spelling/whether they're really in your FG reference:")
  print(unresolved_override_names)
}

## overrides replace whatever the automatic stages produced (or didn't)
## for these species
resolved_final <- resolved[!Species %in% overrides_resolved$Species]
resolved_final <- rbindlist(list(
  resolved_final,
  merge(unmatched_species[Species %in% overrides_resolved$Species, .(Species, Catch)],
        overrides_resolved[, .(Species, FG_num, FG_name, match_method)], by = "Species")
), use.names = TRUE)

still_failing_final <- resolved_final[is.na(FG_num)]

message("\n=== FINAL SUMMARY (after ", nrow(overrides_resolved), " manual override(s)) ===")
message("Resolved: ", nrow(resolved_final) - nrow(still_failing_final), " of ", nrow(resolved_final),
        " (", round(100 * (nrow(resolved_final) - nrow(still_failing_final)) / nrow(resolved_final), 1), "%)")

## final Catch total check, including overrides
if (abs(sum(unmatched_species$Catch) - sum(resolved_final$Catch)) > 0.01) {
  message("WARNING: totals differ after overrides - investigate before trusting results.")
} else {
  message("Catch total check OK - no duplication.")
}

## =================================================================
## SAVE EVERYTHING TO CSV FOR MANUAL REVIEW/CORRECTION
## One row per species, with status and method so you can filter and
## fix in Excel, then re-import if needed.
## =================================================================

resolved_final[, status := fifelse(is.na(FG_num), "unresolved", "resolved")]

## attach ambiguity detail (if any) for species that ended up unresolved,
## so the CSV explains WHY rather than just showing a blank
ambiguity_notes <- rbindlist(list(
  if (nrow(exact_resolved$ambiguous) > 0) unique(exact_resolved$ambiguous[, .(Species, note = ambiguity_type)]) else NULL,
  if (nrow(genus_resolved$ambiguous) > 0) unique(genus_resolved$ambiguous[, .(Species, note = "ambiguous_genus_match")]) else NULL
), use.names = TRUE, fill = TRUE)

output <- merge(resolved_final, ambiguity_notes, by = "Species", all.x = TRUE)
setorder(output, status, -Catch)

fwrite(output, file.path(out_dir, "species_fg_matched.csv"))
message("\nSaved full results to ", file.path(out_dir, "species_fg_matched.csv"), " (", nrow(output), " rows) -",
        " open in Excel to review/correct 'unresolved' rows, or anything you want to double-check.")

## =================================================================
## FG-level Year timeseries - what 04_pbqb_calc.R's fishing-mortality
## step actually needs (Year x FG_num x Catch), not the lifetime-total-
## per-species table above. `output` only has ONE Catch value per
## species (summed across all years, since unmatched_species was built
## without a Year dimension - it exists purely to resolve each species
## name to an FG once). ts_data$species_ts DOES have the Year dimension
## (Year x Species[common name] x Division x Catch) but no FG yet - so
## this joins the two: ts_data$species_ts's per-year catch against
## output's Species -> FG_num mapping, then re-aggregates by Year/FG.
##
## CAVEAT: a species whose common name happened to exactly equal a
## scientific name in fg$ESPECIE would have been excluded from
## unmatched_species entirely (STEP 0 upstream) and so has no row in
## `output` at all - vanishingly unlikely in practice (a common English
## name matching a Latin binomial verbatim) but means "unmatched" here
## isn't a perfect complement of "resolved".
## =================================================================
species_to_fg <- unique(output[!is.na(FG_num), .(Species, FG_num, FG_name)])

fg_year_matched <- merge(ts_data$species_ts, species_to_fg, by = "Species")
fg_catch_timeseries <- fg_year_matched[
  , .(Catch_t = sum(Catch, na.rm = TRUE)), by = .(Year, FG_num, FG_name)]

catch_matched_value  <- sum(fg_year_matched$Catch, na.rm = TRUE)
catch_total_value    <- sum(ts_data$species_ts$Catch, na.rm = TRUE)
message("\n=== FG-level Year timeseries ===")
message("Catch value matched to an FG: ", round(catch_matched_value, 1), " of ",
        round(catch_total_value, 1), " t total (",
        round(100 * catch_matched_value / catch_total_value, 1), "%) -",
        " the remainder is catch from species that never resolved to an FG",
        " (see species_fg_matched.csv, status == 'unresolved') and is NOT",
        " included in fg_catch_timeseries.csv below, not silently distributed",
        " across the FGs that DID resolve.")

## NOTE ON SCOPE: this is nominal CATCH/LANDINGS as reported by
## FAO/GFCM capture production statistics - it is NOT confirmed to
## include discards. Treat fg_catch_timeseries.csv as a landings-only
## lower bound on total removals until a discard-inclusive source
## (e.g. STECF FDI, GFCM DCRF discard tables) is added and combined in.
fwrite(fg_catch_timeseries, file.path(out_dir, paste0("fg_catch_timeseries_", DATASET_VERSION, ".csv")))
message("Saved fg_catch_timeseries_", DATASET_VERSION, ".csv (", nrow(fg_catch_timeseries), " rows,",
        " Year x FG_num x FG_name x Catch_t) - this is a LANDINGS-ONLY figure from ", DATASET_VERSION,
        ", not confirmed to include discards. Kept as an audit/intermediate CSV; the actual",
        " EwE-ready deliverable is the Catches_Ecopath/Catches_Ecosim sheets below.")

## =================================================================
## Add Catches_Ecopath/Catches_Ecosim to output/ecopath_ecosim_inputs.xlsx
##
## DATASET_VERSION is currently the ONE real, working catch source
## (FAO_GFCM/GFCM_2025) - written straight to the workbook rather than
## through a multi-source combiner, since there's nothing else
## implemented to combine with yet. If a second source (most likely
## STECF_FDI, for discards) gets implemented later, that's the point
## to reintroduce a combination step - not before, since a combiner
## with only one real input just adds indirection.
##
## fg_catch_timeseries's Catch_t is a RAW TOTAL (tonnes landed across
## the whole region, MEASURE == "Q_tlw" above) - add_catches_to_
## ecopath_workbook() converts this to a t/km^2 density using
## area_km2 below, to match Biomass's units in the Ecopath sheet.
## strata_area_by_area.csv is written by 01_survey_density_westmed.R
## (Step 6) - the SAME area figure 04_pbqb_calc.R's FG-level F step uses,
## so Catches and F are consistent with each other too, not just with
## Biomass.
## =================================================================
## strata_area_by_area.csv is written by 01_survey_density_westmed.R into
## its own out_dir - since this script now uses the SAME canonical out_dir
## (Step 1 above), no separate BASE_DIR-based guess is needed anymore.
strata_area_path <- file.path(out_dir, "strata_area_by_area.csv")
if (!file.exists(strata_area_path)) {
  stop("strata_area_by_area.csv not found at '", strata_area_path, "' - needed to convert",
       " fg_catch_timeseries's raw total tonnes into a t/km^2 density matching Biomass's",
       " units in the Ecopath sheet. Run 01_survey_density_westmed.R first (Step 6 writes",
       " this file), or check out_dir matches where it actually saved it.")
}
area_total_km2 <- fread(strata_area_path)[, sum(area_km2, na.rm = TRUE)]
message("Study area for the Catches density conversion: ", round(area_total_km2, 1), " km^2",
        " (sum of strata_area_by_area.csv, from 01_survey_density_westmed.R).")

## KNOWN, UNRESOLVED MISMATCH: per GFCM's own GSA-to-Division table,
## Divisions 37.1.1/37.1.2/37.1.3 (area_filter_values above) cover
## GSA 1-11 - matching 01_survey_density_westmed.R's FILTER_AREAS - PLUS
## GSA 12 (Northern Tunisia), which FILTER_AREAS does NOT include.
## So fg_catch_timeseries's Catch_t (filtered by Division) contains
## some landings from OUTSIDE the survey's modeled area, while
## area_total_km2 above does not include GSA 12's area - the resulting
## Catches density is therefore a SLIGHT OVERESTIMATE relative to
## Biomass's area, not an underestimate. GFCM's capture statistics are
## reported at Division resolution, not GSA resolution, so GSA 12's
## specific contribution can't be subtracted out of Catch_t directly -
## this is a genuine data-resolution limitation, not a fixable bug.
## Likely small in practice (Tunisia's share of Division 37.1.3's total
## catch), but not quantified here - if that matters for a specific FG,
## check whether it's disproportionately caught in GSA 12/Division
## 37.1.3 specifically.
message("NOTE: Divisions 37.1.1-37.1.3 (used to filter catch data) cover GSA 1-11 PLUS",
        " GSA 12 (Northern Tunisia) per GFCM's own GSA-to-Division table - GSA 12 is",
        " NOT in FILTER_AREAS (the survey's modeled area). Catches density above is",
        " therefore a slight OVERESTIMATE relative to Biomass's area - GFCM's Division-",
        " level catch reporting can't isolate GSA 12's specific contribution to subtract it out.")

add_catches_to_ecopath_workbook(
  fg_catch      = fg_catch_timeseries,
  fg_lookup     = fg_lookup,
  out_path      = ECOPATH_WORKBOOK_PATH,
  year_ecopath  = YEAR_ECOPATH,
  area_km2      = area_total_km2
)