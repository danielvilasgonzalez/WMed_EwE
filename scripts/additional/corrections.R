# comparison between biomass of old wmed model - to add
# sector (industrial or artisanal) from SAU, but maybe it is worth it to get regional catalan database to validate regional catches?
# recreational as sector - need effort manual from Valerio feedback
# OPENOCEAN Stock assessment (tunas and big pèlagics) - to add
# RLS only in 2015 - any qualitative indicator -- for regional validation?
# joan i fran intenta agrupar seabirds - Fran Joan
# cesc scianena and groupers
# diets keep as past and lñater change - working on diet code almost implemented
# 9-11 15 oct marian xavi i marta para dietes
# catch from gfcm, discards (%)... bycatch (%).... black market (%)..... illegal and proportions - to discuss unreported/black market

# i will repeat what should contain the excel: info (with the info of the run and time and space), FG_spp (list of FG and species and taxononmy), Ecopath_B (FG_number, FG_name, Biomass t/km2), Ecopath_L (FG_number, FG_name,landings each column a fleet), Ecopath_Di (FG_number, FG_name, discards each column a fleet), Ecopath_PBQB (FG_number, FG_name, PB, QB), Ecopath_traits (FG_name, species and the table of traits as it was saved), Ecopath_diet (diet format EwE), Ecosim_ts (same format as saved, biomass, landings, discards, fishing effort).    THe rest of the outputs as csv files out of the excel. I got this error > trim_workbook_to_final_sheets(file.path(out_dir, "ecopath_ecosim_inputs.xlsx"))
# finalize_workbook_sheet_order(): workbook currently has 10 sheet(s): FG, Ecopath, FG_spp_Ecopath, PB_QB_spp, AquaMaps_Depth_Adjustment, FG_Density_by_Stratum, traits_ewe, Ecosim, FG_spp_Ecosim, Ecopath_B
# finalize_workbook_sheet_order(): target sheet(s) not in the workbook yet (skipped, re-run this after the script that creates them has run): info, Ecopath_L, Ecopath_Di, Ecopath_PBQB, Ecopath_traits, Ecopath_diet, Ecosim_ts
# finalize_workbook_sheet_order(): drop_extras = TRUE - removing sheet(s) not in target_order: FG, Ecopath, FG_spp_Ecopath, PB_QB_spp, AquaMaps_Depth_Adjustment, FG_Density_by_Stratum, traits_ewe, Ecosim, FG_spp_Ecosim
# Error in wb$setactiveSheet(old_ActiveSheet) : 
#   2 doesn't exist as sheet index.

# it needs to review the taxonomy fallback matches for review
# genus fallback ok, class ok except for (the purple sea urchin, if a single sp is the FG then avoid that match like red coral or mackerels)
# also error when commercial is in the name could bring missmatch, avoid match FG taxononmy with commercial, example decapods)
# also avoid match if matched by multiple groups 
# phylum usually wrong
# to add from unmatched: FGseaweeds - rhodophyta, Chlorophyt, FG: Other macrobenthos:Ectoprocta, Brachipoda, scaphopoda
# jellyfish usually wrong, avoid match taxonomic with jellyfish
# remove Sea ball of Posidonia oceanica, shell debris, Leaves of Posidonia oceanica
# there are some groups that cannot be match because they belong to small groups with no taoxnomic definition, like suprabenthos (isopoda, amphipoda) and macrozooplankton (euphasiacea)


#compile validation section:
# - GFW effort (at least at regional level)
# - Visual Census regional scale
# - Diet validation inside diet block  fishbase</ecobae
# sector (industrial or artisanal) from SAU, but maybe it is worth it to get regional catalan database to validate regional catches?










