# ecopathAI/
#   
#   ├── app.R
# ├── DESCRIPTION
# ├── NAMESPACE
# │
# ├── R/
#   │   ├── get_species.R
# │   ├── get_traits.R
# │   ├── create_fgs.R
# │   ├── build_diet.R
# │   ├── estimate_parameters.R
# │   ├── evaluate_foodweb.R
# │   ├── export_ecopath.R
# │   ├── ai_assistant.R
# │   └── utils.R
# │
# ├── data/
#   │
# ├── cache/
#   │
# ├── inst/
#   │   ├── prompts/
#   │   └── templates/
#   │
# └── tests/

#species retrieval
library(data.table)
library(robis)

library(rfishbase)
library(data.table)

get_species <- function(
    ecosystem_name = "Mediterranean Sea"
){
  
  spp <- species_by_ecosystem(
    ecosystem = ecosystem_name
  )
  
  spp <- spp[
    spp$CurrentPresence == "Present",
  ]
  
  if(nrow(spp) == 0)
    stop(
      paste(
        "No species found for ecosystem:",
        ecosystem_name
      )
    )
  
  spp <- data.table(
    Species = unique(spp$Species)
  )
  
  spp <- spp[
    !is.na(Species)
  ]
  
  spp
}

#get base groups
add_base_groups <- function(traits){
  
  base_groups <- data.table(
    
    Species = c(
      "Phytoplankton",
      "Microzooplankton",
      "Mesozooplankton",
      "Macrozooplankton",
      "Gelatinous zooplankton",
      "Benthos",
      "Detritus"
    ),
    
    DietTroph = c(
      1.0,
      2.0,
      2.2,
      2.5,
      3.0,
      2.5,
      1.0
    ),
    
    FoodTroph = c(
      1.0,
      2.0,
      2.2,
      2.5,
      3.0,
      2.5,
      1.0
    ),
    
    K = NA_real_,
    M = NA_real_,
    Temperature = NA_real_,
    Winfinity = NA_real_,
    
    Demersal = c(
      NA,
      NA,
      NA,
      NA,
      NA,
      "yes",
      "yes"
    ),
    
    Pelagic = c(
      "yes",
      "yes",
      "yes",
      "yes",
      "yes",
      NA,
      NA
    ),
    
    Benthic = c(
      NA,
      NA,
      NA,
      NA,
      NA,
      "yes",
      "yes"
    ),
    
    Oceanic = c(
      NA,
      NA,
      NA,
      NA,
      "yes",
      NA,
      NA
    ),
    
    ArtificialGroup = TRUE
  )
  
  if(!"ArtificialGroup" %in% names(traits)){
    traits[, ArtificialGroup := FALSE]
  }
  
  missing_cols <- setdiff(
    names(traits),
    names(base_groups)
  )
  
  for(col in missing_cols){
    base_groups[, (col) := NA]
  }
  
  base_groups <- base_groups[, names(traits), with = FALSE]
  
  rbind(
    traits,
    base_groups,
    fill = TRUE
  )
}


#fishbase and sealifebase traits
library(rfishbase)
library(data.table)

get_traits <- function(species){
  
  message("Downloading ecology data...")
  eco <- ecology(species)
  
  message("Downloading species data...")
  sp_info <- as.data.table(
    species(species)
  )
  
  message("Downloading growth data...")
  growth <- as.data.table(
    popgrowth(species)
  )
  
  message("Downloading taxonomy data...")
  tax <- load_taxa()
  
  #----------------------------------------
  # Ecology + growth
  #----------------------------------------
  
  traits <- merge(
    eco,
    growth,
    by = c(
      "Species",
      "SpecCode"
    ),
    all = TRUE
  )
  
  #----------------------------------------
  # Species information
  #----------------------------------------
  
  traits <- merge(
    traits,
    sp_info[
      ,
      c(
        "SpecCode",
        "DepthRangeShallow",
        "DepthRangeDeep",
        "DepthRangeComShallow",
        "DepthRangeComDeep",
        "Vulnerability",
        "Importance",
        "DemersPelag",
        "CommonLength",
        "Length",
        "LongevityWild"
      )
    ],
    by = "SpecCode",
    all.x = TRUE
  )
  
  #----------------------------------------
  # Taxonomy
  #----------------------------------------
  
  tax <- as.data.table(
    load_taxa()
  )
  
  tax <- tax[
    get("Species") %in% species
  ]
  
  tax <- tax[
    ,
    c(
      "SpecCode",
      "Family",
      "Order",
      "Class",
      "SuperClass"
    ),
    with = FALSE
  ]
  
  traits <- merge(
    traits,
    tax[
      ,
      .(
        SpecCode,
        Family,
        Order,
        Class,
        SuperClass
      )
    ],
    by = "SpecCode",
    all.x = TRUE
  )
  
  # Force data.table after merges
  traits <- data.table::as.data.table(traits)
  
  # Check
  stopifnot(
    data.table::is.data.table(traits)
  )
  
  #----------------------------------------
  # Select best record per species
  #----------------------------------------
  
  traits[
    ,
    info_score :=
      (!is.na(DietTroph)) +
      (!is.na(K)) +
      (!is.na(M)) +
      (!is.na(DepthRangeShallow)) +
      (!is.na(Vulnerability)) +
      (!is.na(CommonLength))
  ]
  
  setorder(
    traits,
    Species,
    -info_score
  )
  
  traits <- traits[
    ,
    .SD[1],
    by = Species
  ]
  
  traits[
    ,
    info_score := NULL
  ]
  
  #----------------------------------------
  # Derived variables
  #----------------------------------------
  
  traits[
    ,
    MeanTL := DietTroph
  ]
  
  traits[
    ,
    MeanDepth :=
      (
        DepthRangeShallow +
          DepthRangeDeep
      ) / 2
  ]
  
  traits[
    is.na(MeanDepth),
    MeanDepth :=
      (
        DepthRangeComShallow +
          DepthRangeComDeep
      ) / 2
  ]
  
  traits[
    ,
    Winf := Winfinity
  ]
  
  #----------------------------------------
  # Fill missing values
  #----------------------------------------
  
  traits[
    is.na(K),
    K := median(
      K,
      na.rm = TRUE
    )
  ]
  
  traits[
    is.na(M),
    M := median(
      M,
      na.rm = TRUE
    )
  ]
  
  traits[
    is.na(MeanDepth),
    MeanDepth := median(
      MeanDepth,
      na.rm = TRUE
    )
  ]
  
  traits[
    is.na(Vulnerability),
    Vulnerability := median(
      Vulnerability,
      na.rm = TRUE
    )
  ]
  
  traits[
    is.na(DietTroph),
    DietTroph := median(
      DietTroph,
      na.rm = TRUE
    )
  ]
  
  message(
    "Species retained: ",
    nrow(traits)
  )
  
  return(traits)
}

#FG generator
library(data.table)

create_fgs <- function(
    traits,
    n_fg = 50
){
  
  #--------------------------------------------------
  # 1. Habitat
  #--------------------------------------------------
  
  traits[
    ,
    Habitat := "Other"
  ]
  
  traits[
    tolower(as.character(Pelagic)) == "yes",
    Habitat := "Pelagic"
  ]
  
  traits[
    tolower(as.character(Demersal)) == "yes",
    Habitat := "Demersal"
  ]
  
  traits[
    tolower(as.character(Benthic)) == "yes",
    Habitat := "Benthic"
  ]
  
  traits[
    tolower(as.character(Oceanic)) == "yes",
    Habitat := "Oceanic"
  ]
  
  #--------------------------------------------------
  # 2. Trophic guild
  #--------------------------------------------------
  
  traits[
    ,
    Guild := fcase(
      DietTroph <= 2.0, "Herbivore",
      DietTroph <= 3.0, "Omnivore",
      DietTroph <= 4.0, "Consumer",
      DietTroph <= 4.5, "Predator",
      default = "ApexPredator"
    )
  ]
  
  #--------------------------------------------------
  # 3. Taxonomic group
  #--------------------------------------------------
  
  #------------------------------------
  # Taxonomic classification
  #------------------------------------
  
  traits[
    ,
    TaxonGroup := "Other"
  ]
  
  # Elasmobranchs
  
  traits[
    Class %in% c(
      "Elasmobranchii"
    ),
    TaxonGroup := "Elasmobranch"
  ]
  
  # Cephalopods
  
  traits[
    Class %in% c(
      "Cephalopoda"
    ),
    TaxonGroup := "Cephalopod"
  ]
  
  # Crustaceans
  
  traits[
    Class %in% c(
      "Malacostraca",
      "Branchiopoda",
      "Maxillopoda"
    ),
    TaxonGroup := "Crustacean"
  ]
  
  # Bivalves
  
  traits[
    Class %in% c(
      "Bivalvia"
    ),
    TaxonGroup := "Bivalve"
  ]
  
  # Gastropods
  
  traits[
    Class %in% c(
      "Gastropoda"
    ),
    TaxonGroup := "Gastropod"
  ]
  
  # Jellyfish
  
  traits[
    Class %in% c(
      "Scyphozoa",
      "Hydrozoa"
    ),
    TaxonGroup := "Gelatinous"
  ]
  
  # Fish
  
  traits[
    Class %in% c(
      "Actinopterygii"
    ),
    TaxonGroup := "Fish"
  ]
  
  ############ COMMERCIAL
  
  traits[
    ,
    CommercialGroup := "Other"
  ]
  
  # Small pelagics
  
  traits[
    Family %in% c(
      "Clupeidae",
      "Engraulidae"
    ),
    CommercialGroup := "SmallPelagic"
  ]
  
  # Large pelagics
  
  traits[
    Family %in% c(
      "Scombridae",
      "Xiphiidae",
      "Istiophoridae"
    ),
    CommercialGroup := "LargePelagic"
  ]
  
  # Hakes
  
  traits[
    Family %in% c(
      "Merlucciidae"
    ),
    CommercialGroup := "Hake"
  ]
  
  # Flatfish
  
  traits[
    Order %in% c(
      "Pleuronectiformes"
    ),
    CommercialGroup := "Flatfish"
  ]
  
  # Sharks and rays
  
  traits[
    TaxonGroup == "Elasmobranch",
    CommercialGroup := "Elasmobranch"
  ]
  
  # Cephalopods
  
  traits[
    TaxonGroup == "Cephalopod",
    CommercialGroup := "Cephalopod"
  ]
  
  # Crustaceans
  
  traits[
    TaxonGroup == "Crustacean",
    CommercialGroup := "Crustacean"
  ]
  
  #--------------------------------------------------
  # 4. Ecological template
  #--------------------------------------------------
  
  traits[
    ,
    EcoGroup := paste(
      Habitat,
      Guild,
      TaxonGroup,
      CommercialGroup,
      sep = "_"
    )
  ]
  
  traits[
    ,
    FG := NA_integer_
  ]
  
  current_fg <- 1
  
  eco_groups <- unique(
    traits[
      ArtificialGroup == FALSE,
      EcoGroup
    ]
  )
  
  for(g in eco_groups){
    
    ids <- which(
      traits$EcoGroup == g &
        traits$ArtificialGroup == FALSE
    )
    
    if(length(ids) < 3){
      
      traits[
        ids,
        FG := current_fg
      ]
      
      current_fg <- current_fg + 1
      
      next
    }
    
    sub <- traits[ids]
    
    vars <- c(
      "DietTroph",
      "K",
      "M",
      "Temperature"
    )
    
    X <- sub[, ..vars]
    
    cc <- complete.cases(X)
    
    X <- X[cc]
    
    if(nrow(X) < 3){
      
      traits[
        ids,
        FG := current_fg
      ]
      
      current_fg <- current_fg + 1
      
      next
    }
    
    n_subgroups <- max(
      1,
      round(
        n_fg *
          nrow(sub) /
          nrow(
            traits[
              ArtificialGroup == FALSE
            ]
          )
      )
    )
    
    n_subgroups <- min(
      n_subgroups,
      floor(nrow(X)/2)
    )
    
    if(n_subgroups <= 1){
      
      traits[
        ids,
        FG := current_fg
      ]
      
      current_fg <- current_fg + 1
      
    } else {
      
      km <- kmeans(
        scale(X),
        centers = n_subgroups,
        nstart = 50
      )
      
      complete_ids <- ids[cc]
      
      traits[
        complete_ids,
        FG := km$cluster + current_fg - 1
      ]
      
      current_fg <- current_fg + n_subgroups
    }
  }
  
  #--------------------------------------------------
  # 5. Force Ecopath base groups
  #--------------------------------------------------
  
  base_groups <- c(
    "Phytoplankton",
    "Microzooplankton",
    "Mesozooplankton",
    "Macrozooplankton",
    "Gelatinous zooplankton",
    "Benthos",
    "Detritus"
  )
  
  for(bg in base_groups){
    
    traits[
      Species == bg,
      FG := current_fg
    ]
    
    current_fg <- current_fg + 1
  }
  
  #--------------------------------------------------
  # 6. Create FG names
  #--------------------------------------------------
  
  fg_names <- traits[
    ,
    .(
      Habitat = names(sort(table(Habitat),
                           decreasing = TRUE))[1],
      Guild = names(sort(table(Guild),
                         decreasing = TRUE))[1],
      TaxonGroup = names(sort(table(TaxonGroup),
                              decreasing = TRUE))[1]
    ),
    by = FG
  ]
  
  fg_names[
    ,
    FG_Name := paste(
      Habitat,
      Guild,
      TaxonGroup
    )
  ]
  
  traits[
    Species == "Phytoplankton",
    FG_Name := "Phytoplankton"
  ]
  
  traits[
    Species == "Microzooplankton",
    FG_Name := "Microzooplankton"
  ]
  
  traits[
    Species == "Mesozooplankton",
    FG_Name := "Mesozooplankton"
  ]
  
  traits[
    Species == "Macrozooplankton",
    FG_Name := "Macrozooplankton"
  ]
  
  traits[
    Species == "Gelatinous zooplankton",
    FG_Name := "Gelatinous zooplankton"
  ]
  
  traits[
    Species == "Benthos",
    FG_Name := "Benthos"
  ]
  
  traits[
    Species == "Detritus",
    FG_Name := "Detritus"
  ]
  
  traits <- merge(
    traits,
    fg_names[
      ,
      .(
        FG,
        FG_Name
      )
    ],
    by = "FG",
    all.x = TRUE
  )
  
  return(traits)
}


#diet matrix builder
library(data.table)
library(rfishbase)

build_diet <- function(species){
  
  diet <- diet(species)
  
  fooditems <- fooditems(species)
  
  if(nrow(fooditems)==0){
    
    warning("Fallback diet method")
    
    return(NULL)
    
  }
  
  fooditems
}

build_diet_fallback <- function(traits){
  
  links <- list()
  
  for(i in seq_len(nrow(traits))){
    
    predator <- traits$Species[i]
    
    TL <- traits$DietTroph[i]
    
    prey <- traits[
      DietTroph < TL &
        DietTroph > TL - 2
    ]
    
    links[[i]] <- data.table(
      Predator = predator,
      Prey = prey$Species
    )
  }
  
  rbindlist(links)
}


#network contruction
library(igraph)

build_network <- function(edges){
  
  graph_from_data_frame(
    edges,
    directed = TRUE
  )
}

foodweb_metrics <- function(g){
  
  data.table(
    
    Connectance =
      edge_density(g),
    
    MeanDegree =
      mean(degree(g)),
    
    Clustering =
      transitivity(g)
    
  )
}

library(httr2)
library(jsonlite)
library(httr2)

call_llm <- function(
    prompt,
    model = "llama3"
){
  
  req <- request(
    "http://localhost:11434/api/generate"
  ) |>
    req_body_json(
      list(
        model = model,
        prompt = prompt,
        stream = FALSE
      )
    )
  
  resp <- req_perform(req)
  
  out <- resp_body_json(resp)
  
  out$response
}

#ecopath parameter estimation
estimate_parameters <- function(traits){
  
  traits[
    ,
    PB := M
  ]
  
  traits[
    ,
    QB := NA_real_
  ]
  
  traits
}

evaluate_foodweb <- function(
    traits,
    network
){
  
  metrics <- foodweb_metrics(network)
  
  missing_groups <- character()
  
  spp <- tolower(traits$Species)
  
  if(!any(grepl("zooplankton", spp)))
    missing_groups <- c(missing_groups, "Zooplankton")
  
  if(!any(grepl("phytoplankton", spp)))
    missing_groups <- c(missing_groups, "Phytoplankton")
  
  if(!any(grepl("detrit", spp)))
    missing_groups <- c(missing_groups, "Detritus")
  
  if(!any(grepl("shark", spp)))
    missing_groups <- c(missing_groups, "Large sharks")
  
  if(!any(grepl("dolphin|whale|cetace", spp)))
    missing_groups <- c(missing_groups, "Marine mammals")
  
  fg_summary <- traits[
    ,
    .(
      Nspecies = .N,
      MeanTL = mean(
        DietTroph,
        na.rm = TRUE
      ),
      MeanK = mean(
        K,
        na.rm = TRUE
      ),
      MeanM = mean(
        M,
        na.rm = TRUE
      )
    ),
    by = FG
  ]
  
  list(
    metrics = metrics,
    missing_groups = missing_groups,
    fg_summary = fg_summary
  )
}

#AI ecology reviewer
review_foodweb <- function(
    traits,
    review
){
  
  metrics <- review$metrics
  missing_groups <- review$missing_groups
  fg_summary <- review$fg_summary
  
  top_predators <- traits[
    order(-DietTroph)
  ][1:min(10, nrow(traits))]
  
  prompt <- paste0(
    
    "You are a senior Ecopath and ecosystem modelling expert.

Review the ecosystem model below.

=========================
ECOSYSTEM SUMMARY
=========================

Number of species: ", nrow(traits), "

Number of functional groups: ", nrow(fg_summary), "

Trophic level range: ",
    round(min(traits$DietTroph, na.rm=TRUE),2),
    " - ",
    round(max(traits$DietTroph, na.rm=TRUE),2), "

Growth K range: ",
    round(min(traits$K, na.rm=TRUE),3),
    " - ",
    round(max(traits$K, na.rm=TRUE),3), "

=========================
NETWORK METRICS
=========================

Connectance: ", round(metrics$Connectance,3), "
Mean degree: ", round(metrics$MeanDegree,3), "
Clustering: ", round(metrics$Clustering,3), "

=========================
TOP PREDATORS
=========================

",
    paste(top_predators$Species,
          collapse = "\n"),
    
    "

=========================
POTENTIAL MISSING GROUPS
=========================

",
    paste(missing_groups,
          collapse = "\n"),
    
    "

=========================
FUNCTIONAL GROUPS
=========================

",
    paste(
      paste0(
        "FG ",
        fg_summary$FG,
        ": ",
        fg_summary$Nspecies,
        " species, Mean TL=",
        round(fg_summary$MeanTL,2)
      ),
      collapse = "\n"
    ),
    
    "

Please evaluate:

1. Missing trophic compartments.
2. Potential Ecopath balancing problems.
3. Over-aggregated functional groups.
4. Missing prey resources.
5. Missing predators.
6. Recommendations for improving realism.

Provide sections:
Strengths
Weaknesses
Missing Groups
Recommendations

"
  )
  
  call_llm(prompt)
}


#Ecopath export
export_ecopath <- function(
    traits,
    diet,
    path
){
  
  fwrite(
    traits,
    file.path(
      path,
      "groups.csv"
    )
  )
  
  fwrite(
    diet,
    file.path(
      path,
      "diet.csv"
    )
  )
}

#Main pipeline
build_ecosystem <- function(
    
  ecosystem_name,
  n_fg
  
){
  
  spp <- get_species(
    ecosystem_name
  )
  
  traits <- get_traits(
    spp$Species
  )
  
  traits <- add_base_groups(
    traits
  )
  
  traits <- create_fgs(
    traits,
    n_fg
  )
  
  diet <- build_diet(
    traits$Species
  )
  
  if(is.null(diet))
    diet <- build_diet_fallback(traits)
  
  g <- build_network(diet)
  
  traits <- estimate_parameters(traits)
  
  review <- evaluate_foodweb(
    traits,
    g
  )
  
  ai_review <- review_foodweb(
    traits,
    review
  )
  
  list(
    traits = traits,
    species = traits,
    diet = diet,
    network = g,
    metrics = review$metrics,
    missing_groups = review$missing_groups,
    fg_summary = review$fg_summary,
    ai_review = ai_review
  )
}

model <- build_ecosystem(
  ecosystem_name = "Mediterranean Sea",
  n_fg = 50
)

cat(model$ai_review)



#FG has 338 species which are very high
#lack of fg name
# check model$species there are repeated column names .x .y


# 12. Future Research Version
# 
# The version I think would be scientifically novel would replace the species retrieval section with:
#   
#   OBIS occurrences
# FishBase
# SeaLifeBase
# AquaMaps
# FAO catches
# GFW fishing effort
# VAST biomass estimates
# Environmental layers (SST, depth, chlorophyll)
# 
# Then use:
#   
#   Species occurrence
# ↓
# Trait extraction
# ↓
# Graph neural network
# ↓
# Functional group optimization
# ↓
# Food-web assembly
# ↓
# Ecopath parameter estimation
# ↓
# LLM ecological review
# ↓
# Ecopath/Ecosim/Ecospace export