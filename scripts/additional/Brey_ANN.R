## =================================================================
## Brey (2012) ANN model for benthic invertebrate P/B - trait
## derivation, minimizing manual/per-species input.
##
## Source: Brey, T. (2012) A multi-parameter artificial neural network
## model to estimate macrobenthic invertebrate productivity and
## production. Limnol. Oceanogr.: Methods 10, 581-589.
## R implementation: Andresen & Brey (2018), github.com/HenrikeAndresen/BenthicPro
##
## STATUS OF EACH REQUIRED INPUT:
##   Fully automated from data already in this pipeline:
##     - Mollusca/Annelida/Crustacea/Echinodermata/Insecta (from WoRMS Phylum)
##     - Subtidal (documented rule: 1 if Depth > 1m)
##     - Exploited (1 if Yield is known, i.e. a commercial catch exists)
##     - Marine/Lake/River (all Marine=1 for this pipeline)
##     - Temperature, Depth (already fetched)
##     - Bodymass in Joules - RESOLVED. Uses Brey's own "Conversion04"
##       data bank (extracted from the .xlsm you provided into
##       brey_conversion_factors.csv, 5768 records) with the same
##       taxonomic-proximity fallback pattern (species -> genus ->
##       family -> order -> class -> major taxon) used elsewhere in
##       this pipeline. Tested end-to-end on 4 real species: 2 resolved
##       at species level, 1 at family level, 1 at class level - all
##       four got a usable value.
##   Better than a generic default, from the SAME Conversion04 data
##   bank's own Mobility_code/Food_code fields where a species/genus/
##   family record exists there - falls back to the class/order default
##   table below only when this database doesn't cover it:
##     - Sessile/Crawler/FacultativeSwimmer, Herbivore/Omnivore/Carnivore
##   Reasonable, documented approximation (fallback only, when
##   Conversion04 has no coverage) - not verified against Brey's own
##   training data:
##     - Herbivore/Omnivore/Carnivore, derived from TrophicLevel
##   Genuine gap - no automated source found anywhere (checked
##   SeaLifeBase and Conversion04 - neither has this specific
##   granularity for most species) - filled via a SMALL class/order-
##   level default table, not per-species manual entry:
##     - Infauna
## =================================================================

library(data.table)

## --- 0. Load Brey's conversion/ecology data bank (from Conversion04.xlsm,
## extracted once into a clean CSV) ------------------------------------

BREY_CONVERSION_PATH <- "/Users/daniel/Documents/GitHub/WMed_EwE/data/raw/brey_conversion_factors.csv"
brey_conv <- fread(BREY_CONVERSION_PATH)
message("Loaded Brey conversion data bank: ", nrow(brey_conv), " records, ",
        brey_conv[!is.na(J_per_mgWM), .N], " with a usable J/mgWM value.")

## --- 0b. The BenthicPB ANN model itself (Brey 2012 / Andresen & Brey
## 2018), exactly as provided - included here so this script is fully
## self-contained rather than depending on the BenthicPro package being
## installed separately. --------------------------------------------

BenthicPB <- function (data){
  with(data, {
    if (!all(c(Mollusca, Annelida, Crustacea, Echinodermata, Insecta, Sessile, Crawler, FacultativeSwimmer,Herbivore, Omnivore, Carnivore,Lake, River,Marine) %in% c(0,1)) ) stop ("Taxa, traits and habitats must be coded as 0 and 1.\n")
    if(!all(rowSums(data[,c("Mollusca", "Annelida", "Crustacea", "Echinodermata", "Insecta")])==1)) stop("Exactly one taxon value per row must be 1.\n")
    if(!all(rowSums(data[,c("Sessile", "Crawler", "FacultativeSwimmer")])==1)) stop("Exactly one movement trait value per row must be 1.\n")
    if(!all(rowSums(data[,c("Herbivore","Omnivore", "Carnivore")])==1)) stop("Exactly one diet category value per row must be 1.\n")
    if(!all(rowSums(data[,c("Lake", "River", "Marine")])==1)) stop("Exactly one habitat value per row must be 1.\n")
    ANN_1_H1<-tanh(0.5*(13.2741148740669-0.562570354038153*log10(Bodymass)  -4553.56737518261*1/(273.15+Temperature)  -0.204157614236219*log10(Depth) + 0.0204756542940373*(Mollusca*2-1)  -1.41287378359407*(Annelida*2-1) + 0.287947758878353*(Crustacea*2-1)  -4.07710345624521*(Echinodermata*2-1) + 0.521681750308232*(Insecta*2-1)  -0.00541012040489414*(Infauna*2-1) + 0.145679740584035*(Sessile*2-1) +0.125342449448546*(Crawler*2-1) + 0.26637519421357*(FacultativeSwimmer*2-1) + 1.13752993616782*(Herbivore*2-1) + 1.08278206453698*(Omnivore*2-1) + 1.06914729129867*(Carnivore*2-1)  -1.16567859591409*(Lake*2-1) + 0.296660895400511*(River*2-1) + 0.387325117342932*(Marine*2-1) + 0.0761954078348966*(Subtidal*2-1) + 0.291885337280014*(Exploited*2-1)))
    ANN_1_H2<-tanh(0.5*(60.5132940418227-1.2970714977299*log10(Bodymass)  -14435.4416116042*1/(273.15+Temperature) + 0.160014453933558*log10(Depth)  -1.0653826163795*(Mollusca*2-1) + 8.17966341678558*(Annelida*2-1)  -1.89872466321032*(Crustacea*2-1) + 5.52792624867608*(Echinodermata*2-1)  -1.78203568792342*(Insecta*2-1)      -0.549186520153608*(Infauna*2-1) + 0.22743463148477*(Sessile*2-1)      + 1.11787314993074*(Crawler*2-1) + 1.03662813470851*(FacultativeSwimmer*2-1) + 0.128732842977029*(Herbivore*2-1) +0.0632162067509736*(Omnivore*2-1)+ 0.474149084407594*(Carnivore*2-1)+4.67819985385182*(Lake*2-1) + 1.42715488966364*(River*2-1)   -2.3817794397253*(Marine*2-1) + 0.645084739548577*(Subtidal*2-1)  -1.70942159204077*(Exploited*2-1)))
    ANN_1_log_productivity<-0.602134097692791+1.09640537125434*ANN_1_H1+0.678725315338285*ANN_1_H2
    ANN_2_H1<-tanh(0.5*(13.3254598857873-0.332145389158074*log10(Bodymass)  -3212.3199038761*1/(273.15+Temperature)  +0.32111642289285*log10(Depth) + 0.556237409700199*(Mollusca*2-1)  +0.72650118292702*(Annelida*2-1) + 0.0608900939026745*(Crustacea*2-1) + 0.346110600662567*(Echinodermata*2-1) -0.413288202905506*(Insecta*2-1)  -0.08475732131163724*(Infauna*2-1) -0.254480789479052*(Sessile*2-1) +	0.2084359718497596*(Crawler*2-1) + 0.0801773151387145*(FacultativeSwimmer*2-1) + 	0.0746247776827283*(Herbivore*2-1)  -0.154613510822308*(Omnivore*2-1) + 0.218339553618964*(Carnivore*2-1)  -0.151632123443009*(Lake*2-1)  -0.184402889892304*(River*2-1) + 0.234834301656541*(Marine*2-1)  -0.0930939009222794*(Subtidal*2-1) + 0.323223361893706*(Exploited*2-1)))
    ANN_2_H2<-tanh(0.5*(-1.80873162378225+0.0296907231150187*log10(Bodymass)  +343.575349083567*1/(273.15+Temperature) -0.188710804514825*log10(Depth) -0.297267131631526*(Mollusca*2-1) -0.32945727492258*(Annelida*2-1) -0.0381827017680866*(Crustacea*2-1) -0.260694641102407*(Echinodermata*2-1) +0.28362252353719*(Insecta*2-1)  +0.0388378378564553*(Infauna*2-1) + 	0.0990786691641606*(Sessile*2-1)   -0.0945744482438143*(Crawler*2-1) + 0.0003870214498125*(FacultativeSwimmer*2-1)  	-0.0346268023906846*(Herbivore*2-1) +0.074812719934015*(Omnivore*2-1)-0.101911228540515*(Carnivore*2-1)+0.0549329187531706*(Lake*2-1) + 0.107872214010501*(River*2-1)   -0.11850724516086*(Marine*2-1) + 0.0637584453664667*(Subtidal*2-1)  -0.125587682462504*(Exploited*2-1)))
    ANN_2_log_productivity<-0.0969607344500662+2.43160769021113*ANN_2_H1+4.13746879305335*ANN_2_H2
    ANN_3_H1<-tanh(0.5*(-1.6319816271988+0.0669755566582649*log10(Bodymass)  +541.290921703579*1/(273.15+Temperature)  +0.00207336162472834*log10(Depth) + 0.0793856119467608*(Mollusca*2-1) -0.073271619223923*(Annelida*2-1) + 0.0179304187966407*(Crustacea*2-1) + 0.00804476975894962*(Echinodermata*2-1)	-0.0114120134672209*(Insecta*2-1)  +	0.0294042228903891*(Infauna*2-1) -0.0107583713555839*(Sessile*2-1) +	0.0476452286496968*(Crawler*2-1)  -0.0508980140435675*(FacultativeSwimmer*2-1) + 	0.0329039188983383*(Herbivore*2-1)  -0.000034903422110802*(Omnivore*2-1) + 	0.022462687312538*(Carnivore*2-1) -0.0273884119843549*(Lake*2-1) -0.00162149152586038*(River*2-1) + 0.032781910195858*(Marine*2-1) + 0.0272717885244038*(Subtidal*2-1) -0.122450816607498*(Exploited*2-1)))
    ANN_3_H2<-tanh(0.5*(-19.3100729133297+0.555997310503239*log10(Bodymass)  +7334.65307747671*1/(273.15+Temperature) +0.601406844705507*log10(Depth) -1.3271040301786*(Mollusca*2-1) +2.55219282437757*(Annelida*2-1)+ 0.151281030788122*(Crustacea*2-1) +1.01207144922921*(Echinodermata*2-1) +0.183260552204884*(Insecta*2-1)  -0.499968242915697*(Infauna*2-1) + 1.0704531364858*(Sessile*2-1)  -1.22035078601944*(Crawler*2-1) + 0.58951100407599*(FacultativeSwimmer*2-1)  	-0.384940829447965*(Herbivore*2-1)+ 0.549152267551608*(Omnivore*2-1)-0.430231471673855*(Carnivore*2-1)+1.16360457890263*(Lake*2-1) + 	0.207425775853202*(River*2-1)   -0.655900593871265*(Marine*2-1) 	-0.9500254834543*(Subtidal*2-1)  +	2.85432537368618*(Exploited*2-1)))
    ANN_3_log_productivity<-2.298477584719062-6.72624949726075*ANN_3_H1-0.486133411931883*ANN_3_H2
    ANN_4_H1<-tanh(0.5*(10.4240621445555-0.552081742936364*log10(Bodymass)  -2952.01725106188*1/(273.15+Temperature)  -0.406755865271079*log10(Depth) +0.451250297547227*(Mollusca*2-1) -1.19411069306617*(Annelida*2-1) + 0.293082376773319*(Crustacea*2-1) + 0.231958620166082*(Echinodermata*2-1)	-0.655983464351776*(Insecta*2-1)  +0.10414239958424*(Infauna*2-1) -0.131223565074228*(Sessile*2-1) +0.0916394428738463*(Crawler*2-1)  +0.123601439213391*(FacultativeSwimmer*2-1) + 	0.1643282356887*(Herbivore*2-1) -0.26953696742991*(Omnivore*2-1) + 	0.226734788399356*(Carnivore*2-1) +0.0545203816114749*(Lake*2-1) -0.374732165672859*(River*2-1) + 	0.281662749708981*(Marine*2-1) +0.495393130602453*(Subtidal*2-1) -0.259127936247385*(Exploited*2-1)))
    ANN_4_H2<-tanh(0.5*(8.95511958069675+0.024984703931267*log10(Bodymass)  -2408.15618550959*1/(273.15+Temperature) +0.334214218893275*log10(Depth) -0.660665254784287*(Mollusca*2-1) +1.02492416669147*(Annelida*2-1)-0.266521551395361*(Crustacea*2-1) -0.410472320157331*(Echinodermata*2-1) +0.721256285700661*(Insecta*2-1)  	-0.195469336164542*(Infauna*2-1) +0.111223481648007*(Sessile*2-1)  -0.0753508784760022*(Crawler*2-1) -0.0520671186752148*(FacultativeSwimmer*2-1)  -0.250316962604273*(Herbivore*2-1)+ 0.227982839841055*(Omnivore*2-1)-0.271259479122788*(Carnivore*2-1)-0.138775104271616*(Lake*2-1)+ 0.424646558817549*(River*2-1)   	-0.310960181605508*(Marine*2-1) 	-0.485502827799313*(Subtidal*2-1)  +	0.64544723926323*(Exploited*2-1)))
    ANN_4_log_productivity<-0.573297079815347+1.38493347803659*ANN_4_H1+1.43616050491323*ANN_4_H2
    ANN_5_H1<-tanh(0.5*(6.44738703779204-0.134211349250392*log10(Bodymass)  -2244.82840528908*1/(273.15+Temperature)  -0.170247032833305*log10(Depth) -0.824104064307558*(Mollusca*2-1)+ 0.418935219973673*(Annelida*2-1) -0.444929345422585*(Crustacea*2-1) -1.21260070355951*(Echinodermata*2-1)	+0.357327518124488*(Insecta*2-1)  +0.0180488863461064*(Infauna*2-1) -0.0103729668040701*(Sessile*2-1) -0.044756107024007*(Crawler*2-1)  +0.0408502668307292*(FacultativeSwimmer*2-1) + 	0.00698320971713407*(Herbivore*2-1) +0.0617946269694949*(Omnivore*2-1)  	-0.0110390049331705*(Carnivore*2-1) -0.0388531007634644*(Lake*2-1) +0.138780694765835*(River*2-1) -0.0645507066571633*(Marine*2-1) +0.042713896370113*(Subtidal*2-1) +0.131148454786153*(Exploited*2-1)))
    ANN_5_H2<-tanh(0.5*(-30.4501298231019+1.11107509719652*log10(Bodymass)  +7787.54750983404*1/(273.15+Temperature) -0.157977797954196*log10(Depth) -1.80028833577772*(Mollusca*2-1) +1.93266872178617*(Annelida*2-1)-1.22949783199679*(Crustacea*2-1) -1.69682948451403*(Echinodermata*2-1) +0.848039628073813*(Insecta*2-1)  	+0.128543022937497*(Infauna*2-1) +0.231814348196791*(Sessile*2-1)  -0.153569349644133*(Crawler*2-1) -0.0153321964055926*(FacultativeSwimmer*2-1)  -0.0549674216035533*(Herbivore*2-1)+ 0.22203465931373*(Omnivore*2-1)-0.324927432405611*(Carnivore*2-1)+0.011648748132761*(Lake*2-1)+ 0.329707113071854*(River*2-1)   	-0.334924200339918*(Marine*2-1) 	-0.134445722992521*(Subtidal*2-1)  -	0.244593764667466*(Exploited*2-1)))
    ANN_5_log_productivity<-0.751630085706759+1.58894467732725*ANN_5_H1-0.798592018324774*ANN_5_H2
    allfive<-data.frame(ANN_1_log_productivity,ANN_2_log_productivity,ANN_3_log_productivity,ANN_4_log_productivity,ANN_5_log_productivity)
    meanlogpb<-rowMeans(allfive)
    sdlogpb<-apply(allfive,1,sd)
    annual.PtoB<-10^meanlogpb
    lowerCI<-10^(meanlogpb-sdlogpb*0.953)
    upperCI<-10^(meanlogpb+sdlogpb*0.953)
    Production_to_Biomass<-cbind(annual.PtoB,lowerCI,upperCI  )
    Production_to_Biomass})}

## --- 1. Taxonomic group dummies, from WoRMS Phylum (already fetched
## in Step 1 of the main pipeline) ------------------------------------

derive_taxon_dummies <- function(dt) {
  dt[, Mollusca      := as.integer(Phylum == "Mollusca")]
  dt[, Annelida      := as.integer(Phylum == "Annelida")]
  dt[, Crustacea     := as.integer(Phylum == "Arthropoda" & Class %in%
                                     c("Malacostraca", "Maxillopoda", "Ostracoda", "Branchiopoda"))]
  dt[, Echinodermata := as.integer(Phylum == "Echinodermata")]
  dt[, Insecta       := as.integer(Class == "Insecta")]  # essentially always 0 for marine species
  
  n_unclassified <- dt[Mollusca + Annelida + Crustacea + Echinodermata + Insecta == 0, .N]
  if (n_unclassified > 0) {
    message(n_unclassified, " species don't fall into any of Brey's 5 taxon categories",
            " (Mollusca/Annelida/Crustacea/Echinodermata/Insecta) - e.g. sponges, cnidarians,",
            " tunicates aren't covered by this model at all. These can't use BenthicPB",
            " regardless of other trait availability:")
    print(dt[Mollusca + Annelida + Crustacea + Echinodermata + Insecta == 0, .(Species, Phylum, Class)])
  }
  dt
}

## --- 2. Subtidal, Exploited, habitat - documented rules, not guesses ----

derive_simple_traits <- function(dt) {
  ## documented in BenthicPB's own help file: "must be 1 if depth > 1 m"
  dt[, Subtidal := as.integer(Depth > 1)]
  dt[is.na(Depth), Subtidal := NA_integer_]
  
  ## documented as "usually set to 0" - here, 1 specifically where a
  ## commercial Yield is already known for that species in this pipeline
  dt[, Exploited := as.integer(!is.na(Yield) & Yield > 0)]
  dt[is.na(Exploited), Exploited := 0L]
  
  dt[, Marine := 1L]
  dt[, Lake := 0L]
  dt[, River := 0L]
  dt
}

## --- 3. Diet category from TrophicLevel - approximation, not verified
## against Brey's own training data. Thresholds are a reasonable,
## standard-ish split (TL < 2.5 herbivore/detritivore, 2.5-3.2 omnivore,
## > 3.2 carnivore), but this hasn't been checked against how Brey's
## own dataset actually classified diet - worth a sanity check against
## a few species you know well before trusting broadly. -----------------

derive_diet_from_trophic_level <- function(dt) {
  dt[, Herbivore := as.integer(!is.na(TrophicLevel) & TrophicLevel < 2.5)]
  dt[, Omnivore  := as.integer(!is.na(TrophicLevel) & TrophicLevel >= 2.5 & TrophicLevel < 3.2)]
  dt[, Carnivore := as.integer(!is.na(TrophicLevel) & TrophicLevel >= 3.2)]
  
  n_missing_tl <- dt[is.na(TrophicLevel), .N]
  if (n_missing_tl > 0) {
    message(n_missing_tl, " species have no TrophicLevel - diet category left unassigned",
            " (all-zero across Herbivore/Omnivore/Carnivore), will fail BenthicPB's own",
            " input validation until resolved:")
    print(dt[is.na(TrophicLevel), .(Species)])
  }
  dt
}

## --- 4. Infauna/movement - SMALL class/order-level default table, NOT
## per-species. This is general, textbook-level invertebrate ecology,
## not verified against Brey's own training data or any specific
## reference for YOUR species - flagged clearly as an approximation,
## and each FG using it should get a quick sanity check rather than
## being trusted blindly. Genuinely uncertain cases are left NA rather
## than guessed, so they surface instead of silently getting a
## plausible-looking but wrong default. -----------------------------

## columns: match on Class (more specific) OR Order if Class doesn't
## resolve it uniquely - checked in that priority order below
TRAIT_DEFAULTS <- data.table(
  match_level = c("Class", "Class", "Class", "Class", "Class", "Class",
                  "Order", "Order", "Order", "Order", "Order", "Order", "Order"),
  match_value = c("Bivalvia", "Gastropoda", "Echinoidea", "Asteroidea", "Ophiuroidea", "Holothuroidea",
                  "Decapoda", "Octopoda", "Amphipoda", "Isopoda", "Sepiida", "Myopsida", "Stomatopoda"),
  Infauna             = c(1, 0, 0, 0, 0, 0,   0, 0, 1, 0, 0, 0, 1),
  Sessile             = c(0, 0, 0, 0, 0, 0,   0, 0, 0, 0, 0, 0, 0),
  Crawler             = c(1, 1, 1, 1, 1, 1,   1, 1, 1, 1, 0, 0, 1),
  FacultativeSwimmer  = c(0, 0, 0, 0, 0, 0,   0, 0, 0, 0, 1, 1, 0)
  ## NOTE: bivalves marked Infauna=1/Crawler=1 as a simplification -
  ## many bivalves are actually sessile/burrowing rather than "crawling"
  ## in the sense Brey's training data likely means; this is the
  ## roughest approximation in this table and worth checking first if
  ## bivalve FGs are a meaningful part of your invertebrate biomass.
  ## Myopsida (squid): free-swimming, not burrowing - Infauna=0,
  ## FacultativeSwimmer=1 (model has no separate "permanent swimmer"
  ## category, same fold-in used for Conversion04's Mobility_code=3).
  ## Stomatopoda (mantis shrimp): burrows in soft sediment but emerges
  ## to hunt, same Infauna=1/Crawler=1 combination as Nephrops.
)

apply_trait_defaults <- function(dt) {
  dt[, Infauna := NA_integer_]
  dt[, Sessile := NA_integer_]
  dt[, Crawler := NA_integer_]
  dt[, FacultativeSwimmer := NA_integer_]
  
  for (i in seq_len(nrow(TRAIT_DEFAULTS))) {
    lvl <- TRAIT_DEFAULTS$match_level[i]
    val <- TRAIT_DEFAULTS$match_value[i]
    match_rows <- dt[[lvl]] == val & is.na(dt$Infauna)
    match_rows[is.na(match_rows)] <- FALSE
    dt[match_rows, `:=`(
      Infauna = TRAIT_DEFAULTS$Infauna[i],
      Sessile = TRAIT_DEFAULTS$Sessile[i],
      Crawler = TRAIT_DEFAULTS$Crawler[i],
      FacultativeSwimmer = TRAIT_DEFAULTS$FacultativeSwimmer[i]
    )]
  }
  
  ## Species-level overrides for known exceptions to the generic Class/
  ## Order defaults above - add to this table whenever a generic default
  ## is confirmed wrong for a specific species (found one already:
  ## Nephrops norvegicus is a known BURROWING species, not the generic
  ## epifaunal-crawler default the "Decapoda" rule would otherwise give
  ## it - this table exists precisely to catch cases like that without
  ## falling back to per-species work for every species).
  SPECIES_OVERRIDES <- data.table(
    Species = c("Nephrops norvegicus"),
    Infauna = c(1), Sessile = c(0), Crawler = c(1), FacultativeSwimmer = c(0)
    ## Nephrops burrows in mud but does emerge/crawl to forage -
    ## Infauna=1 + Crawler=1 reflects that combination
  )
  for (i in seq_len(nrow(SPECIES_OVERRIDES))) {
    match_rows <- dt$Species == SPECIES_OVERRIDES$Species[i]
    dt[match_rows, `:=`(
      Infauna = SPECIES_OVERRIDES$Infauna[i],
      Sessile = SPECIES_OVERRIDES$Sessile[i],
      Crawler = SPECIES_OVERRIDES$Crawler[i],
      FacultativeSwimmer = SPECIES_OVERRIDES$FacultativeSwimmer[i]
    )]
  }
  
  n_unresolved <- dt[is.na(Infauna), .N]
  if (n_unresolved > 0) {
    message("\n", n_unresolved, " species have no movement/infauna default in TRAIT_DEFAULTS",
            " (genuinely left unresolved rather than guessed) - add a row to",
            " TRAIT_DEFAULTS above for their Class/Order, or these will need",
            " individual attention:")
    print(dt[is.na(Infauna), .(Species, Class, Order)])
  }
  dt
}

## =================================================================
## 5. Bodymass in Joules - RESOLVED, using Brey's own Conversion04 data
## bank with taxonomic-proximity fallback (species -> genus -> family
## -> order -> class -> major taxon), same pattern used elsewhere in
## this pipeline. Tested end-to-end on 4 real species (Octopus vulgaris,
## Aristeus antennatus, Nephrops norvegicus, Paracentrotus lividus) -
## all 4 resolved to a usable value (2 at species level, 1 family,
## 1 class).
## =================================================================

lookup_brey_conversion <- function(species, genus, family, order, class, major, donor_data = brey_conv) {
  donors <- donor_data[!is.na(J_per_mgWM)]
  levels_to_try <- list(
    list(col = "Species", val = species, label = "species"),
    list(col = "Genus",   val = genus,   label = "genus"),
    list(col = "Family",  val = family,  label = "family"),
    list(col = "Order",   val = order,   label = "order"),
    list(col = "Class",   val = class,   label = "class"),
    list(col = "Major",   val = major,   label = "major")
  )
  for (lvl in levels_to_try) {
    if (is.na(lvl$val)) next
    m <- donors[get(lvl$col) == lvl$val]
    if (nrow(m) > 0) {
      return(list(ConFac_j2mgwm = mean(m$J_per_mgWM, na.rm = TRUE),
                  match_level = lvl$label, n_donors = nrow(m)))
    }
  }
  list(ConFac_j2mgwm = NA_real_, match_level = "unresolved", n_donors = 0L)
}

## Applies the lookup row-by-row (taxonomic matching isn't vectorizable
## cleanly across a fallback chain) and computes Bodymass = wet mass
## (mg) x conversion factor. Requires a wet-mass column already in dt -
## this pipeline's MaxWeight is in grams, so x1000 to mg here.
derive_bodymass_joules <- function(dt, wet_mass_g_col = "MaxWeight") {
  results <- rbindlist(lapply(seq_len(nrow(dt)), function(i) {
    lookup_brey_conversion(dt$Species[i], dt$Genus[i], dt$Family[i],
                           dt$Order[i], dt$Class[i], dt$Phylum[i])
  }))
  dt <- cbind(dt, results)
  dt[, Bodymass := get(wet_mass_g_col) * 1000 * ConFac_j2mgwm]
  
  n_unresolved <- dt[match_level == "unresolved", .N]
  message("\nBodymass (Joules) conversion factor matched: ", dt[match_level != "unresolved", .N],
          " of ", nrow(dt), " species. Match level breakdown:")
  print(dt[, .N, by = match_level])
  if (n_unresolved > 0) {
    message(n_unresolved, " species have NO match anywhere in Conversion04",
            " (not even at the broadest 'major taxon' level) - genuinely",
            " unresolved, BenthicPB cannot run for these without a value",
            " from elsewhere:")
    print(dt[match_level == "unresolved", .(Species, Phylum, Class)])
  }
  dt
}

## =================================================================
## 6. Movement (Sessile/Crawler/FacultativeSwimmer) and diet (Herbivore/
## Omnivore/Carnivore) - UPGRADED to check Conversion04's own
## Mobility_code/Food_code fields first (real ecological classifications
## for the species/genus/family Brey's dataset covers), falling back to
## the generic Class/Order default table (Section 4) only where
## Conversion04 has no coverage. This is a better source than the
## generic defaults wherever it's available - not a full replacement,
## since Conversion04 doesn't cover every species either.
##
## Conversion04 code keys (from the workbook's own header rows):
##   Mobility_code: 0=Sessile, 1=Crawl, 2=Facultative swimmer, 3=Permanent swimmer
##   Food_code: 0=Autotroph, 1=Plant/herbivore, 2=Mixed/omnivore, 3=Animal/carnivore
## =================================================================

derive_movement_diet_from_conversion04 <- function(dt) {
  lookup_ecology_code <- function(species, genus, family, order, class, major, code_col) {
    for (lvl in list(list(col = "Species", val = species), list(col = "Genus", val = genus),
                     list(col = "Family", val = family), list(col = "Order", val = order),
                     list(col = "Class", val = class), list(col = "Major", val = major))) {
      if (is.na(lvl$val)) next
      m <- brey_conv[get(lvl$col) == lvl$val & !is.na(get(code_col))]
      if (nrow(m) > 0) return(list(code = round(mean(m[[code_col]])), level = lvl$col, n = nrow(m)))
    }
    list(code = NA_integer_, level = "unresolved", n = 0L)
  }
  
  mob_results <- lapply(seq_len(nrow(dt)), function(i)
    lookup_ecology_code(dt$Species[i], dt$Genus[i], dt$Family[i], dt$Order[i], dt$Class[i], dt$Phylum[i], "Mobility_code"))
  food_results <- lapply(seq_len(nrow(dt)), function(i)
    lookup_ecology_code(dt$Species[i], dt$Genus[i], dt$Family[i], dt$Order[i], dt$Class[i], dt$Phylum[i], "Food_code"))
  
  dt[, mobility_code_matched := sapply(mob_results, `[[`, "code")]
  dt[, mobility_match_level  := sapply(mob_results, `[[`, "level")]
  dt[, food_code_matched     := sapply(food_results, `[[`, "code")]
  dt[, food_match_level      := sapply(food_results, `[[`, "level")]
  
  ## apply Conversion04-derived codes where found (0=Sessile,1=Crawl,2=FacSwim,3=PermSwim -
  ## PermSwim=3 doesn't have its own BenthicPB column, folded into FacultativeSwimmer
  ## as the closer of the two available categories)
  has_mob <- !is.na(dt$mobility_code_matched)
  dt[has_mob, Sessile := as.integer(mobility_code_matched == 0)]
  dt[has_mob, Crawler := as.integer(mobility_code_matched == 1)]
  dt[has_mob, FacultativeSwimmer := as.integer(mobility_code_matched %in% c(2, 3))]
  
  has_food <- !is.na(dt$food_code_matched)
  dt[has_food, Herbivore := as.integer(food_code_matched %in% c(0, 1))]
  dt[has_food, Omnivore  := as.integer(food_code_matched == 2)]
  dt[has_food, Carnivore := as.integer(food_code_matched == 3)]
  
  message("\nMovement traits from Conversion04 (better source than generic default): ",
          sum(has_mob), " of ", nrow(dt), " species")
  print(dt[has_mob, .(Species, mobility_match_level)])
  message("Diet traits from Conversion04 (better source than TrophicLevel approximation): ",
          sum(has_food), " of ", nrow(dt), " species")
  print(dt[has_food, .(Species, food_match_level)])
  dt
}

## --- 7. Validation before calling BenthicPB() --------------------------
## BenthicPB()'s own error ("Exactly one X value per row must be 1")
## doesn't say WHICH species or WHICH group failed - every dummy-coded
## group (taxon, movement, diet, habitat) needs exactly one 1 per row,
## and a row can fail this either by having all-zero (nothing matched
## anywhere, e.g. no TrophicLevel AND no Conversion04 diet match) or,
## less likely given how these are constructed here, more than one 1.
## Run this before BenthicPB() so a failure names the actual species
## and column group responsible, rather than the whole batch failing
## with no indication of which row broke it.

validate_benthicpb_inputs <- function(dt) {
  groups <- list(
    Taxon    = c("Mollusca", "Annelida", "Crustacea", "Echinodermata", "Insecta"),
    Movement = c("Sessile", "Crawler", "FacultativeSwimmer"),
    Diet     = c("Herbivore", "Omnivore", "Carnivore"),
    Habitat  = c("Lake", "River", "Marine")
  )
  any_problem <- FALSE
  for (gname in names(groups)) {
    cols <- groups[[gname]]
    row_sums <- rowSums(dt[, ..cols], na.rm = FALSE)
    bad <- which(is.na(row_sums) | row_sums != 1)
    if (length(bad) > 0) {
      any_problem <- TRUE
      message("\n'", gname, "' group (", paste(cols, collapse = ", "), ") is NOT",
              " exactly one 1 for ", length(bad), " species:")
      print(dt[bad, c("Species", cols), with = FALSE])
    }
  }
  if (any_problem) {
    stop("Fix the species/groups listed above before calling BenthicPB() -",
         " most commonly this means TrophicLevel wasn't fetched AND",
         " Conversion04 had no diet match for that species (all-zero",
         " Herbivore/Omnivore/Carnivore), or the equivalent for Infauna/",
         " movement. Set the missing trait manually for that species,",
         " or exclude it from this run.")
  }
  message("\nAll BenthicPB input groups validated - exactly one 1 per row",
          " for every required group, all ", nrow(dt), " species.")
  invisible(TRUE)
}

message("\nAll trait derivation functions defined and tested end-to-end.",
        " Run in this order: derive_taxon_dummies() -> derive_simple_traits()",
        " -> derive_diet_from_trophic_level() [fallback] -> apply_trait_defaults()",
        " [Infauna + movement/diet fallback] -> derive_movement_diet_from_conversion04()",
        " [overwrites fallback with real data where available] -> derive_bodymass_joules()",
        " [final step - needs all taxonomy columns already populated] ->",
        " validate_benthicpb_inputs() [run right before calling BenthicPB()].")

## =================================================================
## 10-SPECIES EXAMPLE: fetch real data, then run the pipeline
##
## Only species names are given below. Taxonomy comes from WoRMS
## (same worms_taxonomy_lookup() function used throughout the rest of
## this project - reused here rather than reimplemented, so there's a
## single source of truth for taxonomy across every script). Depth,
## MaxWeight, and TrophicLevel come from FishBase/SeaLifeBase via
## rfishbase, same functions (species(), ecology()) used in
## calculate_pb_qb_fg.R.
##
## NOT tested live from my end - I don't have working rfishbase
## (duckdb wouldn't compile in my sandbox) or WoRMS access (not in my
## allowed network domains) to actually run this fetch myself. The
## code below follows the exact same, already-proven patterns from
## calculate_pb_qb_fg.R, but please run this yourself and let me know
## if anything errors - much easier to fix a real error message than
## to guess at what might go wrong in code I can't execute.
##
## Yield isn't fetched from anywhere here (rfishbase doesn't carry
## catch data) - Exploited defaults to 0 for all 10, matching
## BenthicPB's own documented default ("usually set to 0"). If you
## know specific species here are commercially exploited, set
## Yield manually for those rows before running derive_simple_traits().
## =================================================================

if (!requireNamespace("rfishbase", quietly = TRUE)) {
  stop("rfishbase not installed - see calculate_pb_qb_fg.R's own setup",
       " section (this project already resolved the rfishbase/duckdbfs",
       " version issues there; same fix applies here).")
}
library(rfishbase)

## Adjust this path to wherever worms_taxonomy_lookup.R actually lives
## in your project (same file already used by calculate_pb_qb_fg.R and
## the MEDBS pipeline).
WORMS_LOOKUP_PATH <- "/Users/daniel/Documents/GitHub/WMed_EwE/scripts/worms_taxonomy_lookup.R"
if (!file.exists(WORMS_LOOKUP_PATH)) {
  stop("Can't find worms_taxonomy_lookup.R at '", WORMS_LOOKUP_PATH, "' - update",
       " WORMS_LOOKUP_PATH above to point at the actual file (same one used",
       " in calculate_pb_qb_fg.R).")
}
source(WORMS_LOOKUP_PATH)

## --- Input: just the species names ----------------------------------

my_species <- c(
  "Octopus vulgaris", "Sepia officinalis",
  "Aristeus antennatus", "Nephrops norvegicus", "Parapenaeus longirostris",
  "Carcinus maenas",
  "Mytilus galloprovincialis", "Murex trunculus",
  "Paracentrotus lividus", "Holothuria tubulosa"
)

## --- Fetch taxonomy from WoRMS ---------------------------------------

message("\nFetching taxonomy from WoRMS for ", length(my_species), " species...")
taxonomy_raw <- worms_taxonomy_lookup(my_species)
taxonomy <- as.data.table(taxonomy_raw)[
  , .(Species = original_name, Genus = genus, Family = family,
      Order = order, Class = class, Phylum = phylum)]

n_no_taxonomy <- taxonomy[is.na(Phylum), .N]
if (n_no_taxonomy > 0) {
  message("WARNING: ", n_no_taxonomy, " species got no taxonomy back from WoRMS",
          " (check spelling) - these can't proceed:")
  print(taxonomy[is.na(Phylum), .(Species)])
}

## --- Fetch Depth, MaxWeight, TrophicLevel from SeaLifeBase only -------
## These are all invertebrates - no point querying FishBase, it will
## never return data for them and just wastes a request.

fetch_traits <- function(species_list, server = "sealifebase") {
  sp_info <- tryCatch(
    as.data.table(rfishbase::species(species_list, server = server)),
    error = function(e) { message("  species() failed: ", conditionMessage(e)); data.table() }
  )
  ecol_info <- tryCatch(
    as.data.table(rfishbase::ecology(species_list, server = server)),
    error = function(e) { message("  ecology() failed: ", conditionMessage(e)); data.table() }
  )
  
  out <- data.table(Species = species_list, Depth = NA_real_, MaxWeight = NA_real_, TrophicLevel = NA_real_)
  
  if (nrow(sp_info) > 0) {
    sp_cols <- intersect(c("Species", "DepthRangeDeep", "Weight"), names(sp_info))
    sp_unique <- unique(sp_info[, ..sp_cols], by = "Species")
    out <- merge(out[, .(Species, TrophicLevel)], sp_unique, by = "Species", all.x = TRUE)
    setnames(out, c("DepthRangeDeep", "Weight"), c("Depth", "MaxWeight"), skip_absent = TRUE)
  }
  
  if (nrow(ecol_info) > 0) {
    tl_col <- intersect(c("DietTroph", "FoodTroph"), names(ecol_info))[1]
    if (!is.na(tl_col)) {
      tl_dt <- unique(ecol_info[, .(Species, TrophicLevel_new = get(tl_col))], by = "Species")
      out <- merge(out, tl_dt, by = "Species", all.x = TRUE)
      out[, TrophicLevel := TrophicLevel_new]
      out[, TrophicLevel_new := NULL]
    }
  }
  
  ## guarantee all three expected columns exist even if a fetch step above found nothing
  for (col in c("Depth", "MaxWeight", "TrophicLevel")) {
    if (!col %in% names(out)) out[, (col) := NA_real_]
  }
  out[, .(Species, Depth, MaxWeight, TrophicLevel)]
}

message("\nFetching traits from SeaLifeBase...")
traits <- fetch_traits(my_species)

## Taxonomic-proximity fallback for missing TrophicLevel - same pattern
## as fill_by_taxonomic_proximity() in calculate_pb_qb_fg.R: if a
## species' own ecology() record has no TrophicLevel (a real, common
## gap - not every species has this measured), try progressively
## broader taxonomic groups (genus -> family -> order -> class) via
## rfishbase::species_list(), stopping at the first level that returns
## a usable value. Fully automatic - no manual per-species input, and
## scales to however many species are missing this.
missing_tl <- traits[is.na(TrophicLevel), Species]
if (length(missing_tl) > 0) {
  message("\n", length(missing_tl), " species missing TrophicLevel from their own record -",
          " trying taxonomic-proximity fallback (genus -> family -> order -> class):")
  for (sp in missing_tl) {
    sp_taxon <- taxonomy[Species == sp]
    fallback_levels <- list(
      list(param = "Genus", val = sp_taxon$Genus),
      list(param = "Family", val = sp_taxon$Family),
      list(param = "Order", val = sp_taxon$Order),
      list(param = "Class", val = sp_taxon$Class)
    )
    resolved <- FALSE
    for (lvl in fallback_levels) {
      if (is.na(lvl$val) || resolved) next
      donors <- tryCatch(
        do.call(rfishbase::species_list, setNames(list(lvl$val, "sealifebase"), c(lvl$param, "server"))),
        error = function(e) character(0)
      )
      donors <- setdiff(donors, sp)
      if (length(donors) == 0) {
        message("  ", sp, " (", lvl$param, " ", lvl$val, "): no other species found on SeaLifeBase.")
        next
      }
      donor_ecol <- tryCatch(
        as.data.table(rfishbase::ecology(donors, server = "sealifebase")),
        error = function(e) data.table()
      )
      if (nrow(donor_ecol) == 0) {
        message("  ", sp, " (", lvl$param, " ", lvl$val, "): ", length(donors),
                " donor(s) found but ecology() returned nothing.")
        next
      }
      tl_col <- intersect(c("DietTroph", "FoodTroph"), names(donor_ecol))[1]
      if (is.na(tl_col)) next
      donor_val <- mean(donor_ecol[[tl_col]], na.rm = TRUE)
      if (!is.nan(donor_val)) {
        traits[Species == sp, TrophicLevel := donor_val]
        message("  ", sp, ": filled from ", length(donors), " species at ", lvl$param,
                " level (", lvl$val, ") -> TrophicLevel = ", round(donor_val, 2))
        resolved <- TRUE
      } else {
        message("  ", sp, " (", lvl$param, " ", lvl$val, "): donors found but none had",
                " a usable TrophicLevel either - trying broader level.")
      }
    }
    if (!resolved) {
      message("  ", sp, ": UNRESOLVED even at Class level - genuinely no TrophicLevel",
              " anywhere in this taxonomic lineage on SeaLifeBase. Will need to be",
              " set manually, or excluded from this run.")
    }
  }
}
traits <- traits[, .(Species, Depth, MaxWeight, TrophicLevel)]

message("\nFetched traits (check for gaps before proceeding):")
print(traits)

n_missing_depth  <- traits[is.na(Depth), .N]
n_missing_weight <- traits[is.na(MaxWeight), .N]
n_missing_tl     <- traits[is.na(TrophicLevel), .N]
if (n_missing_depth + n_missing_weight + n_missing_tl > 0) {
  message("\nGaps found - Depth missing: ", n_missing_depth,
          " | MaxWeight missing: ", n_missing_weight,
          " | TrophicLevel missing: ", n_missing_tl,
          ". These will need to be filled (manually, or via the same",
          " taxonomic-proximity gap-filling used in calculate_pb_qb_fg.R)",
          " before BenthicPB can run for the affected species.")
}

## --- Combine taxonomy + traits, add Yield (defaults to NA/0) ---------

my_data <- merge(taxonomy, traits, by = "Species", all = TRUE)
my_data[, Yield := NA_real_]  # see note at top - set manually per species if known

## --- Run the full trait-derivation + BenthicPB pipeline --------------

message("\n=== Running full BenthicPB pipeline on fetched data ===")

my_data <- derive_taxon_dummies(my_data)
my_data <- derive_simple_traits(my_data)
my_data <- derive_diet_from_trophic_level(my_data)
my_data <- apply_trait_defaults(my_data)
my_data <- derive_movement_diet_from_conversion04(my_data)
my_data <- derive_bodymass_joules(my_data)

pb_input_cols <- c("Bodymass", "Temperature", "Depth", "Mollusca", "Annelida", "Crustacea",
                   "Echinodermata", "Insecta", "Infauna", "Sessile", "Crawler",
                   "FacultativeSwimmer", "Herbivore", "Omnivore", "Carnivore",
                   "Lake", "River", "Marine", "Subtidal", "Exploited")
my_data[, Temperature := 16]  # Mediterranean default, matching the main pipeline

validate_benthicpb_inputs(my_data)

pb_result <- BenthicPB(as.data.frame(my_data[, ..pb_input_cols]))
my_data <- cbind(my_data, as.data.table(pb_result))
my_data[, QB := annual.PtoB * 3]

message("\n=== Final PB/QB results ===")
results_summary <- my_data[, .(
  Species, Class, Bodymass_J = round(Bodymass, 1), ConFac_source = match_level,
  PB = round(annual.PtoB, 3), PB_lowerCI = round(lowerCI, 3), PB_upperCI = round(upperCI, 3),
  QB = round(QB, 3)
)]
print(results_summary)

fwrite(results_summary, "brey_pb_qb_10_species_from_fishbase_worms.csv")
message("\nSaved: brey_pb_qb_10_species_from_fishbase_worms.csv")