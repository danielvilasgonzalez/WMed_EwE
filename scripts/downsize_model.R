
library(readxl)
library(data.table)
#library(openxlsx)

#====================================================
# USER INPUT
#====================================================

file <- "C:/Users/danie/Downloads/WMed_EwE.xlsx"

#====================================================
# READ DATA
#====================================================

fg <- as.data.table(read_excel(file, sheet = "FG"))
est <- as.data.table(read_excel(file, sheet = "estimates"))
hab <- as.data.table(read_excel(file, sheet = "habitat"))
prey <- as.data.table(read_excel(file, sheet = "preyoverlap"))
pred <- as.data.table(read_excel(file, sheet = "predatoroverlap"))
fleet <- as.data.table(read_excel(file, sheet = "fleets"))

#====================================================
# CLEAN ESTIMATES
#====================================================

setnames(
  est,
  old = names(est)[1:2],
  new = c("FG", "FG_name")
)

fix_tl <- function(x){
  
  x <- as.character(x)
  
  sapply(x, function(v){
    
    if(is.na(v) || v == "")
      return(NA_real_)
    
    if(grepl(",", v))
      return(as.numeric(sub(",", ".", v, fixed = TRUE)))
    
    if(grepl("^\\d+\\.0$", v)){
      
      num <- sub("\\.0$", "", v)
      
      if(nchar(num) == 1)
        return(as.numeric(num))
      
      first <- substr(num, 1, 1)
      rest  <- substr(num, 2, 5)
      
      return(as.numeric(paste0(first, ".", rest)))
    }
    
    as.numeric(v)
  })
}

# trophic level
est[, TL := fix_tl(`Trophic level`)]

# other variables
parse_ewe_num <- function(x){
  suppressWarnings(
    as.numeric(gsub(",", ".", as.character(x), fixed = TRUE))
  )
}

est[, B  := parse_ewe_num(`Biomass (t/km^2)`)]
est[, PB := parse_ewe_num(`Production / biomass (/year)`)]
est[, QB := parse_ewe_num(`Consumption / biomass (/year)`)]

est <- est[, .(
  FG,
  FG_name,
  TL,
  B,
  PB,
  QB
)]

summary(est$TL)

#====================================================
# CLEAN HABITAT
#====================================================

setnames(hab,
         c(names(hab)[1], names(hab)[2]),
         c("FG", "FG_name"))

hab_cols <- setdiff(names(hab), c("FG", "FG_name"))

for(col in hab_cols){
  hab[[col]] <- as.numeric(gsub(",", ".", hab[[col]]))
}

#====================================================
# CLEAN FLEETS
#====================================================

setnames(fleet,
         c(names(fleet)[1], names(fleet)[2]),
         c("FG", "FG_name"))

fleet_cols <- setdiff(names(fleet), c("FG", "FG_name"))

for(col in fleet_cols){
  fleet[[col]] <- suppressWarnings(
    as.numeric(gsub(",", ".", fleet[[col]]))
  )
}

#====================================================
# CLEAN OVERLAP MATRICES
#====================================================

clean_overlap <- function(x){
  
  fg_names <- x[[2]]
  
  mat <- as.matrix(x[, -(1:2)])
  
  mode(mat) <- "numeric"
  
  rownames(mat) <- fg_names
  
  mat <- mat / max(mat, na.rm = TRUE)
  
  list(
    names = fg_names,
    matrix = mat
  )
}

prey_obj <- clean_overlap(prey)
pred_obj <- clean_overlap(pred)

library(data.table)

# =====================================================
# MASTER FG LIST
# =====================================================

fg_names <- trimws(as.character(prey[[2]]))

# Estimates
est <- est[FG_name %in% fg_names]
setkey(est, FG_name)
est <- est[fg_names]

# Habitat
hab <- hab[FG_name %in% fg_names]
setkey(hab, FG_name)
hab <- hab[fg_names]

# Fleet
fleet <- fleet[FG_name %in% fg_names]
fleet <- fleet[FG_name != "Sum"]
setkey(fleet, FG_name)
fleet <- fleet[fg_names]

# overlap matrices already correspond to fg_names
prey_mat <- prey_obj$matrix
pred_mat <- pred_obj$matrix

nFG <- length(fg_names)

# =====================================================
# COSINE SIMILARITY
# =====================================================

cosine_similarity <- function(a, b){
  
  a[is.na(a)] <- 0
  b[is.na(b)] <- 0
  
  if(sum(a) == 0 | sum(b) == 0)
    return(0)
  
  sum(a * b) /
    (sqrt(sum(a^2)) * sqrt(sum(b^2)))
}

# =====================================================
# HABITAT COLUMNS
# =====================================================

hab_cols <- setdiff(
  names(hab),
  c("FG","FG_name")
)

# =====================================================
# FLEET COLUMNS
# =====================================================

fleet_cols <- setdiff(
  names(fleet),
  c("FG","FG_name")
)

# =====================================================
# PAIRWISE SCORES
# =====================================================

results <- vector(
  mode = "list",
  length = choose(nFG,2)
)

counter <- 1

for(i in 1:(nFG-1)) {
  
  cat("\rFG", i, "of", nFG)
  
  for(j in (i+1):nFG) {
    
    # -------------------------
    # prey overlap
    # -------------------------
    
    prey_overlap <- prey_mat[i,j]
    
    # -------------------------
    # predator overlap
    # -------------------------
    
    pred_overlap <- pred_mat[i,j]
    
    # -------------------------
    # habitat similarity
    # -------------------------
    
    h1 <- as.numeric(hab[i, ..hab_cols])
    h2 <- as.numeric(hab[j, ..hab_cols])
    
    habitat_sim <- cosine_similarity(h1,h2)
    
    # -------------------------
    # fleet similarity
    # -------------------------
    
    #f1 <- as.numeric(fleet[i, ..fleet_cols])
    #f2 <- as.numeric(fleet[j, ..fleet_cols])
    
    #fleet_sim <- cosine_similarity(f1,f2)
    
    # -------------------------
    # trophic similarity
    # -------------------------
    
    tl_diff <- abs(est$TL[i] - est$TL[j])
    
    tl_sim <- max(
      0,
      1 - tl_diff/2
    )
    
    # -------------------------
    # biomass similarity
    # -------------------------
    
    #b1 <- est$B[i]
    #b2 <- est$B[j]
    
    #biomass_ratio <- min(b1,b2) /
    #  max(b1,b2)
    
    # -------------------------
    # final score
    # -------------------------
    
    score <-
      0.35 * prey_overlap +
      0.25 * pred_overlap +
      0.15 * habitat_sim +
      #0.10 * fleet_sim +
      0.10 * tl_sim #+
      #0.05 * biomass_ratio
    
    results[[counter]] <- data.table(
      FG1 = fg_names[i],
      FG2 = fg_names[j],
      Score = score,
      PreyOverlap = prey_overlap,
      PredatorOverlap = pred_overlap,
      HabitatSimilarity = habitat_sim,
      #FleetSimilarity = fleet_sim,
      TLSimilarity = tl_sim,
      #BiomassSimilarity = biomass_ratio,
      TLdiff = tl_diff
    )
    
    counter <- counter + 1
  }
}

merge_candidates <- rbindlist(results)

setorder(
  merge_candidates,
  -Score
)

# =====================================================
# RECOMMENDATION
# =====================================================

merge_candidates[
  ,
  Recommendation :=
    fifelse(
      Score >= 0.85 & TLdiff < 0.3,
      "Strong candidate",
      fifelse(
        Score >= 0.70 & TLdiff < 0.5,
        "Candidate",
        "Keep separate"
      )
    )
]

# top candidates
merge_candidates[1:100]

#====================================================
# EXPORT
#====================================================

write.xlsx(
  merge_candidates,
  "FG_merge_candidates.xlsx",
  overwrite = TRUE
)

#====================================================
# TOP 50
#====================================================

merge_candidates[1:50]