## =================================================================
## lib_build_species_df_from_survey.R - LIBRARY FILE, not a pipeline step.
## sourced automatically by the numbered pipeline scripts (01-04) -
## do not run this directly, it has no top-level driver code of its own.
## =================================================================

## =================================================================
## lib_build_species_df_from_survey.R
##
## Bridge between 01_survey_density_westmed.R (survey pipeline) and
## 04_pbqb_calc.R (PB/QB estimation). Reads the real MEDITS+MEDIAS
## combined species-level density table
## (species_density_regional_combined.csv, written by
## 01_survey_density_westmed.R Step 9g) and reshapes it into the
## species_df schema 04_pbqb_calc.R requires:
##   Species  - scientific name
##   FG       - functional group id (numeric, matches FG_num)
##   Biomass  - species biomass DENSITY, t/km^2
##   Yield    - NA here; this survey pipeline has no catch/landings
##              data, so F = Yield/Biomass can't be computed from it.
##              Fill this in separately if you have landings data,
##              in the SAME t/km^2/year units as Biomass.
##
## Biomass is collapsed to ONE value per species by averaging density
## over YEAR_ECOPATH_RANGE - the same snapshot years used to build the
## Ecopath sheet in 01_survey_density_westmed.R (STRATA/YEAR_ECOPATH),
## not the full 1995-2023 time series. This keeps species_df's Biomass
## consistent with the Biomass values that actually go into the
## Ecopath Basic Input, since fg_weighted's Biomass_FG (computed in
## 04_pbqb_calc.R from this species_df) is what ecopath_ready ultimately
## reports as "Biomass in habitat area (t/km^2)" - if 04_pbqb_calc.R's
## species_df used a different year range than the Ecopath sheet, the
## PB/QB values would be biomass-weighted against a different snapshot
## than the Biomass they're paired with downstream.
## =================================================================

library(data.table)

build_species_df_from_survey <- function(survey_csv_path,
                                         year_ecopath_range,
                                         out_rds_path = NULL) {
  
  if (!file.exists(survey_csv_path)) {
    stop("Survey species density file not found at: ", survey_csv_path,
         "\nThis is written by 01_survey_density_westmed.R (Step 9g) as",
         " species_density_regional_combined.csv - run that script first,",
         " or check out_dir matches.")
  }
  
  sp_density <- fread(survey_csv_path)
  required_cols <- c("Year", "FG_num", "FG_name", "ScientificName", "mean_density")
  missing_cols <- setdiff(required_cols, names(sp_density))
  if (length(missing_cols) > 0) {
    stop("species_density_regional_combined.csv is missing expected column(s): ",
         paste(missing_cols, collapse = ", "),
         " - check it wasn't regenerated with a different schema.")
  }
  
  message("Loaded ", nrow(sp_density), " species/FG/year rows from ", survey_csv_path,
          " (", uniqueN(sp_density$ScientificName), " distinct species, ",
          uniqueN(sp_density$FG_num), " distinct FGs, years ",
          min(sp_density$Year), "-", max(sp_density$Year), ").")
  
  ## --- restrict to the Ecopath snapshot years, matching how the
  ## Biomass column in the actual Ecopath Basic Input is defined
  in_range <- sp_density[Year %in% year_ecopath_range]
  message("Restricting to YEAR_ECOPATH range (", paste(range(year_ecopath_range), collapse = "-"),
          "): ", nrow(in_range), " of ", nrow(sp_density), " rows kept.")
  
  species_missing_in_range <- setdiff(unique(sp_density$ScientificName), unique(in_range$ScientificName))
  if (length(species_missing_in_range) > 0) {
    message(length(species_missing_in_range), " species have density data outside YEAR_ECOPATH",
            " but none within it - these will be ABSENT from species_df entirely",
            " (no Biomass value to give PB/QB weighting), not filled from other years:")
    print(species_missing_in_range)
  }
  
  ## --- collapse to one Biomass value per species (mean density across
  ## the Ecopath snapshot years; a species can appear in >1 year within
  ## that range). FG_name carried through unique() by species/FG - if a
  ## species somehow maps to more than one FG_num across years (shouldn't
  ## happen given how FG matching works upstream, but checked explicitly
  ## rather than silently picking one).
  fg_per_species <- unique(in_range[, .(ScientificName, FG_num)])
  dup_fg <- fg_per_species[, .N, by = ScientificName][N > 1, ScientificName]
  if (length(dup_fg) > 0) {
    stop(length(dup_fg), " species map to more than one FG_num within the Ecopath",
         " year range - this shouldn't happen and needs investigation before",
         " proceeding: ", paste(dup_fg, collapse = ", "))
  }
  
  species_df <- in_range[
    , .(Biomass = mean(mean_density, na.rm = TRUE)),
    by = .(Species = ScientificName, FG = FG_num, FG_name)
  ]
  species_df[, Yield := NA_real_]
  
  message("\nBuilt species_df: ", nrow(species_df), " species x FG rows.",
          " Yield is NA for all rows (no catch/landings data in this survey pipeline -",
          " F = Yield/Biomass will not be computable downstream unless you fill",
          " this in separately from landings data, in matching t/km^2/year units).")
  
  n_na_biomass <- species_df[is.na(Biomass), .N]
  if (n_na_biomass > 0) {
    message("WARNING: ", n_na_biomass, " species have NA Biomass after averaging -",
            " check for all-NA mean_density in the source data for these rows.")
  }
  
  if (!is.null(out_rds_path)) {
    saveRDS(species_df, out_rds_path)
    message("Saved species_df to ", out_rds_path)
  }
  
  species_df[]
}

## =================================================================
## Standalone usage (uncomment to run this file directly rather than
## sourcing it from 04_pbqb_calc.R):
## =================================================================
# out_dir_survey <- "/Users/daniel/Work/iMARES/WMed EwE Model/output/"
# out_dir_pbqb   <- "/Users/daniel/Work/iMARES/WMed EwE Model/data/processed/"
# YEAR_ECOPATH   <- 1994:1996   # must match 01_survey_density_westmed.R's own YEAR_ECOPATH
#
# species_df <- build_species_df_from_survey(
#   survey_csv_path    = file.path(out_dir_survey, "species_density_regional_combined.csv"),
#   year_ecopath_range = YEAR_ECOPATH,
#   out_rds_path        = file.path(out_dir_pbqb, "survey_species_df.rds")
# )