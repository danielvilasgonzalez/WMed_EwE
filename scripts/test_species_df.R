## =================================================================
## Test input for calculate_pb_qb_fg.R
##
## Covers all five dispatch paths (fish, marine mammal, seabird,
## invertebrate, phytoplankton), includes TWO genuinely multi-species
## FGs to exercise the biomass-weighted averaging, and Yield for two
## fish species to exercise the F = Yield/Biomass path.
##
## VALIDATION NOTE: Monachus monachus, Tursiops truncatus, and
## Phalacrocorax carbo match the protocol's own worked examples
## (PB ~ 0.1006, QB ~ 11.66, QB ~ 62.68 respectively) - but since the
## current script only uses AUTO-FETCHED Longevity/BodyWeight (no
## manual override columns anymore), whether these reproduce the
## worked-example numbers now depends on whether SeaLifeBase actually
## has matching data for these exact species. That's genuinely
## uncertain - check the coverage printout when you run it. If
## Longevity/MaxWeight come back NA for these three, the calculation
## will fall through to the Gascuel/Q-P=3 fallbacks instead, and won't
## match the worked examples - that's a real data-coverage finding,
## not a bug to force around.
## =================================================================

library(data.table)

species_df <- data.table(
  Species = c(
    ## --- Marine mammals (protocol worked examples, IF SeaLifeBase covers them)
    "Monachus monachus",       # FG 6  Monk seals
    "Tursiops truncatus",      # FG 1  Bottlenose dolphins
    
    ## --- Seabird (protocol worked example, IF SeaLifeBase covers it) --
    "Phalacrocorax carbo",     # FG 8  Gulls and cormorants
    
    ## --- Fish, single-species FGs -------------------------------------
    "Sardina pilchardus",      # FG 19 European sardine adult
    "Engraulis encrasicolus",  # FG 22 European anchovy adult
    "Merluccius merluccius",   # FG 30 European hake adult - has Yield, tests F=Y/B
    "Mullus barbatus",         # FG 50 Red mullet - has Yield, tests F=Y/B
    
    ## --- Fish, MULTI-SPECIES FG (Sparidae+, FG 35) - tests weighting --
    "Diplodus annularis",
    "Pagellus acarne",
    "Pagellus bogaraveo",
    
    ## --- Fish, second MULTI-SPECIES FG (Scorpaenidae+, FG 40) --------
    "Scorpaena notata",
    "Scorpaena porcus",
    
    ## --- Invertebrates --------------------------------------------------
    "Octopus vulgaris",        # FG 60 Coastal benthic cephalopods
    "Aristeus antennatus",     # FG 67 Blue and red shrimp
    "Nephrops norvegicus",     # FG 71 Norway lobster
    "Paracentrotus lividus",   # FG 75 Purple sea urchin
    
    ## --- Phytoplankton (should be FLAGGED, not computed) ---------------
    "Skeletonema costatum"     # FG 90 Small phytoplankton
  ),
  
  FG = c(
    6, 1,
    8,
    19, 22, 30, 50,
    35, 35, 35,
    40, 40,
    60, 67, 71, 75,
    90
  ),
  
  ## FG_name straight from the FG reference (FG_WMed.xlsx, sheet
  ## "fg_wmed_95") - baked directly into species_df at the source
  ## rather than joined from the external file later, since that join
  ## turned out to be fragile (wrong guessed path, multiple sheets
  ## across different models, duplicate FG_name headers in the source
  ## file). This is simpler and more reliable for known, fixed FGs -
  ## the external file is still useful separately for covering FGs
  ## with no species in this particular run.
  FG_name = c(
    "Monk seals", "Bottlenose dolphins",
    "Gulls and cormorants",
    "European sardine adult", "European anchovy adult", "European hake adult", "Red mullet",
    "Sparidae+", "Sparidae+", "Sparidae+",
    "Scorpaenidae+", "Scorpaenidae+",
    "Coastal benthic cephalopods", "Blue and red shrimp", "Norway lobster", "Purple sea urchin",
    "Small phytoplankton"
  ),
  
  ## Biomass in t/km^2 - Sparidae+ and Scorpaenidae+ deliberately have
  ## uneven biomass split across species so the weighted average is
  ## visibly pulled toward whichever species dominates
  Biomass = c(
    0.015, 0.08,             # mammals
    0.02,                    # seabird
    3.5, 2.1, 1.8, 0.9,      # single-species fish FGs
    0.6, 0.3, 0.1,           # Sparidae+ : annularis dominates
    0.25, 0.05,              # Scorpaenidae+ : notata dominates
    0.4, 0.35, 0.5, 0.6,     # invertebrates
    12.0                     # phytoplankton
  )
)

## --- Yield (t/km^2/year) - MUST be a density matching Biomass's units,
## not an absolute catch total. Only supplying it for hake and red
## mullet, at levels giving a moderate, plausible F when divided by
## their Biomass above (F ~ 0.15-0.22/year - reasonable, not extreme
## exploitation) - everything else stays NA, meaning PB = M only for
## those species, same as before.
species_df[, Yield := NA_real_]
species_df[Species == "Merluccius merluccius", Yield := 0.30]   # F = 0.30/1.8 = 0.167/yr
species_df[Species == "Mullus barbatus",       Yield := 0.20]   # F = 0.20/0.9 = 0.222/yr

message("Test species_df built: ", nrow(species_df), " species across ",
        uniqueN(species_df$FG), " functional groups, ",
        species_df[!is.na(Yield), .N], " with Yield supplied (tests F=Yield/Biomass).")
print(species_df)

message("\nAfter running calculate_pb_qb_fg.R, check species_pb_qb_by_taxon_group.csv for:")
message("- Merluccius merluccius and Mullus barbatus: PB_method should show",
        " 'M(Pauly)+F(Y/B)' or 'M(Gascuel)+F(Y/B)', with Fmort ~0.167 and ~0.222 respectively")
message("- Monachus monachus / Tursiops truncatus / Phalacrocorax carbo: check",
        " whether PB_method/QB_method show the named protocol formulas (SeaLifeBase",
        " had matching data) or a fallback (it didn't) - both are informative results")
message("- FG 35 and FG 40 in fg_pb_qb_weighted.csv should show n_species_total > 1",
        " and PB_FG/QB_FG pulled toward whichever species has the larger Biomass")

## --- Save so this can be loaded independently by calculate_pb_qb_fg.R,
## without needing to re-run this generation script in the same session
## (e.g. after an R restart). Adjust the path below if your project's
## data folder is structured differently.
SPECIES_DF_PATH <- "/Users/daniel/Work/iMARES/WMed EwE Model/data/processed/test_species_df.rds"
saveRDS(species_df, SPECIES_DF_PATH)
message("\nSaved to ", SPECIES_DF_PATH)