## =================================================================
## lib_fishbase_diet_matrix.R - LIBRARY FILE, sourced by 04_diets.R.
## Builds an EwE functional-group (FG) diet matrix from the diet data
## held in FishBase and SeaLifeBase.
## =================================================================
##
## DATA SOURCES
##  FishBase (Froese & Pauly, eds., www.fishbase.org) and SeaLifeBase
##  (Palomares & Pauly, eds., www.sealifebase.org), read with rfishbase
##  (Boettiger, Lang & Wainwright 2012, J. Fish Biol. 81:2030-2039):
##   - DIET       one row per diet study: DietCode, SpecCode, Locality,
##                SampleStage, SampleSize, Troph (Pauly et al. 1998,
##                Mar. Freshw. Res. 49:447-453 for the diet tables).
##   - DIETITEMS  quantitative prey composition of each study:
##                DietCode, FoodI/FoodII/FoodIII (FishBase food
##                categories), ItemName, PreySpecCode, Stage,
##                DietPercent (% of the diet, volume or weight).
##   - FOODITEMS  qualitative prey records per species (FoodI-III,
##                Foodname, PreySpecCode, PreyStage, PredatorStage),
##                no percentages.
##  FG membership and taxonomy: FG_WMed_2026.csv.
##  FG biomass B_j (t/km2): 01_biomass.R biomass_proportion_by_species_
##  fg.csv (Ecopath_B_Biomass), species share of FG biomass b_s|f
##  (prop_sp_fg) from the same file.
##  Food-category -> FG crosswalk: diet_reference_tables/
##  fishbase_food_category_to_fg.csv (editable, regex per category).
##
## NOTATION
##  s predator species, f predator FG (s in f), d diet study of s,
##  k prey item of study d, j prey FG.
##  p_s,d,k = DietPercent_k / sum_k DietPercent_k (study-normalised)
##  w_k->j  = share of item k assigned to prey FG j:
##            1  if the prey taxon resolves to one FG (species, or genus
##               with an exclusive/majority FG in FG_WMed_2026.csv);
##            B_j / sum_{j' in J(k)} B_j' if k only resolves to a FishBase
##               food category mapped to the FG set J(k) (prey taken in
##               proportion to its biomass - ASSUMPTION, the standard
##               "biomass-proportional" allocation, e.g. Christensen &
##               Walters 2004, Ecol. Model. 172:109-139 for diet matrix
##               construction);
##            item left unassigned (and the rest renormalised) if no
##               rule matches - listed in the REVIEW file.
##  DC_s,j  = (1/n_s) sum_d sum_k p_s,d,k w_k->j   (studies weighted
##            equally; Mediterranean studies only, when s has any -
##            Locality matched against FB_DIET_MED_REGEX; stage-matched
##            studies for juvenile/adult stanza FGs when available)
##  FOODITEMS fallback (no quantitative study for s): every distinct
##            prey record gets equal weight (ASSUMPTION - presence only).
##  DC_f,j  = sum_{s in f} b_s|f DC_s,j / sum_{s in f, with diet} b_s|f
##            (biomass-weighted over the species that have diet data)
##  Each predator column is renormalised to sum to 1 (EwE "sum to one";
##  Christensen, Walters & Pauly 2005, EwE User Guide).
## =================================================================

if (!exists("FB_DIET_MED_REGEX")) {
  FB_DIET_MED_REGEX <- paste0("(?i)mediterr|alboran|balear|catal|gulf of lions|golfe du lion|ligur|tyrrhen|adriat|",
                              "ionian|aegean|sicil|sardin|corsic|spain|france|italy|malta|algeria|morocco|tunisia|",
                              "greece|turkey|ebro|valencia|marseille|naples|genoa")
}

## --- small helpers -------------------------------------------------
.fb_col <- function(dt, candidates) intersect(candidates, names(dt))[1]

.fb_call <- function(fn, ..., server) {
  if (!requireNamespace("rfishbase", quietly = TRUE)) return(data.table())
  if (!exists(fn, where = asNamespace("rfishbase"), inherits = FALSE)) return(data.table())
  tryCatch(as.data.table(do.call(get(fn, envir = asNamespace("rfishbase")), list(..., server = server))),
           error = function(e) { message("[FishBase diet] rfishbase::", fn, "(server = '", server, "') failed: ",
                                         conditionMessage(e)); data.table() })
}

## Pull DIET / DIETITEMS / FOODITEMS for the predator species, from both
## servers, and cache them as CSV so a later run (or a run without
## network) reuses the same pull. force_refresh = TRUE re-queries.
fb_fetch_diet_tables <- function(species, cache_dir, force_refresh = FALSE) {
  paths <- file.path(cache_dir, c(diet = "fishbase_diet_raw_DIET.csv", items = "fishbase_diet_raw_DIETITEMS.csv",
                                  food = "fishbase_diet_raw_FOODITEMS.csv", taxa = "fishbase_diet_raw_TAXA.csv"))
  names(paths) <- c("diet", "items", "food", "taxa")
  if (!force_refresh && all(file.exists(paths[c("diet", "items", "food")]))) {
    out <- lapply(paths, function(p) if (file.exists(p)) fread(p, encoding = "UTF-8") else data.table())
    message("[FishBase diet] Using cached FishBase/SeaLifeBase diet tables in ", cache_dir,
            " (force_refresh = TRUE to re-query).")
    return(out)
  }
  srv <- c("fishbase", "sealifebase")
  diet  <- rbindlist(lapply(srv, function(s) { x <- .fb_call("diet", species, server = s); if (nrow(x)) x[, server := s]; x }), fill = TRUE)
  items <- data.table()
  if (nrow(diet) && "DietCode" %in% names(diet)) {
    codes <- unique(as.character(diet$DietCode))
    items <- rbindlist(lapply(srv, function(s) { x <- .fb_call("diet_items", server = s); if (nrow(x)) x[, server := s]; x }), fill = TRUE)
    if (nrow(items) && "DietCode" %in% names(items)) items <- items[as.character(DietCode) %in% codes]
  }
  food <- rbindlist(lapply(srv, function(s) { x <- .fb_call("fooditems", species, server = s); if (nrow(x)) x[, server := s]; x }), fill = TRUE)
  taxa <- rbindlist(lapply(srv, function(s) { x <- .fb_call("load_taxa", server = s); if (nrow(x)) x[, server := s]; x }), fill = TRUE)
  if (nrow(taxa)) taxa <- unique(taxa[, intersect(c("SpecCode", "Species", "Genus", "Family", "server"), names(taxa)), with = FALSE])
  out <- list(diet = diet, items = items, food = food, taxa = taxa)
  for (n in names(out)) if (nrow(out[[n]])) {
    if (exists("safe_fwrite")) safe_fwrite(out[[n]], paths[[n]]) else fwrite(out[[n]], paths[[n]])
  }
  message("[FishBase diet] Pulled DIET ", nrow(diet), ", DIETITEMS ", nrow(items), ", FOODITEMS ", nrow(food),
          " row(s) for ", length(species), " predator species; cached in ", cache_dir, ".")
  out
}

## Genus -> FG votes from FG_WMed_2026.csv (exclusive, else strict majority).
.fb_genus_to_fg <- function(fg_ref) {
  if (!"Genus" %in% names(fg_ref)) return(data.table(genus = character(), fg_name = character()))
  v <- fg_ref[!is.na(Genus) & Genus != "", .(n = uniqueN(species)), by = .(genus = tolower(Genus), fg_name)]
  v[, `:=`(tot = sum(n), top = n == max(n), n_top = sum(n == max(n))), by = genus]
  v[top & n_top == 1 & n / tot > 0.5, .(genus, fg_name)]
}

## Resolve one prey item to FG weights. Returns data.table(prey_fg, w, how).
fb_resolve_items <- function(items, fg_ref, xwalk, fg_B, fg_universe) {
  sp2fg   <- unique(fg_ref[, .(name = tolower(trimws(species)), fg_name)])
  sp2fg[, n_fg := uniqueN(fg_name), by = name]          # multi-FG species (e.g. hake juv./adult stanzas): split by FG biomass below
  gen2fg  <- .fb_genus_to_fg(fg_ref)
  xwalk   <- copy(xwalk)[, ord := .I]
  B       <- fg_B[fg_name %in% fg_universe]
  res <- vector("list", nrow(items))
  for (i in seq_len(nrow(items))) {
    nm  <- tolower(trimws(items$prey_name[i])); gen <- sub(" .*", "", nm)
    hit <- sp2fg[name == nm]
    if (nrow(hit) && !is.na(nm) && nchar(nm) > 0) {
      if (nrow(hit) > 1) {
        bt <- B[fg_name %in% hit$fg_name & is.finite(B) & B > 0]
        w  <- if (nrow(bt)) bt$B / sum(bt$B) else rep(1 / nrow(hit), nrow(hit))
        res[[i]] <- data.table(item_id = i, prey_fg = if (nrow(bt)) bt$fg_name else hit$fg_name, w = w,
                               how = "species in several FGs (stanzas) - biomass split")
      } else res[[i]] <- data.table(item_id = i, prey_fg = hit$fg_name, w = 1, how = "species")
      next
    }
    g <- gen2fg[genus == gen]
    if (nrow(g) && !is.na(gen) && nchar(gen) > 2) {
      res[[i]] <- data.table(item_id = i, prey_fg = g$fg_name, w = 1, how = "genus (FG_WMed_2026.csv)")
      next
    }
    cats <- c(FoodIII = items$FoodIII[i], FoodII = items$FoodII[i], FoodI = items$FoodI[i], ItemName = items$prey_name[i])
    found <- NULL
    for (lvl in names(cats)) {
      txt <- cats[[lvl]]
      if (is.na(txt) || !nzchar(txt)) next
      m <- xwalk[(level == lvl | level == "any") & vapply(pattern, function(p) grepl(p, txt, ignore.case = TRUE, perl = TRUE), logical(1))]
      if (nrow(m)) { found <- list(row = m[order(ord)][1], lvl = lvl, txt = txt); break }
    }
    if (is.null(found) || is.na(found$row$FG_names) || !nzchar(found$row$FG_names)) {
      res[[i]] <- data.table(item_id = i, prey_fg = NA_character_, w = NA_real_, how = "unassigned")
      next
    }
    tgt <- intersect(trimws(strsplit(found$row$FG_names, ";", fixed = TRUE)[[1]]), fg_universe)
    bt  <- B[fg_name %in% tgt & is.finite(B) & B > 0]
    w   <- if (nrow(bt)) bt$B / sum(bt$B) else rep(1 / length(tgt), length(tgt))
    fgs <- if (nrow(bt)) bt$fg_name else tgt
    res[[i]] <- if (length(fgs)) data.table(item_id = i, prey_fg = fgs, w = w,
                                            how = paste0("category ", found$lvl, " '", found$txt, "' -> ",
                                                         if (length(fgs) > 1) "biomass split" else "single FG"))
                else data.table(item_id = i, prey_fg = NA_character_, w = NA_real_, how = "unassigned (crosswalk FGs not in model)")
  }
  rbindlist(res)
}

## Main builder.
## predators: data.table(species, fg_name, b_share) - predator species,
##   their FG and their share of the FG's biomass (prop_sp_fg).
## fg_ref: FG_WMed_2026.csv as data.table (species, fg_name, Genus, ...).
## fg_B:   data.table(fg_name, B) - Ecopath B per FG.
build_fishbase_diet_matrix <- function(predators, fg_ref, fg_B, xwalk, cache_dir, fg_universe = NULL,
                                       force_refresh = FALSE, prefer_med = TRUE) {
  empty <- list(fg_diet = data.table(predator_fg = character(), prey_fg = character(), weight = numeric()),
                species_diet = data.table(), item_map = data.table(), provenance = data.table())
  if (is.null(fg_universe)) fg_universe <- unique(fg_ref$fg_name)
  sp <- unique(predators$species)
  if (!length(sp)) return(empty)
  tb <- fb_fetch_diet_tables(sp, cache_dir, force_refresh = force_refresh)
  diet <- tb$diet; items <- tb$items; food <- tb$food; taxa <- tb$taxa

  ## SpecCode -> scientific name, for predators (DIET) and prey (PreySpecCode)
  code2sp <- if (nrow(taxa) && all(c("SpecCode", "Species") %in% names(taxa))) unique(taxa[, .(SpecCode = as.character(SpecCode), sci = Species)]) else data.table(SpecCode = character(), sci = character())
  .sp_name <- function(dt) {
    sc <- .fb_col(dt, c("Species", "sciname"))
    if (!is.na(sc)) return(as.character(dt[[sc]]))
    if ("SpecCode" %in% names(dt)) return(code2sp$sci[match(as.character(dt$SpecCode), code2sp$SpecCode)])
    rep(NA_character_, nrow(dt))
  }

  ## ---- quantitative tier: DIET x DIETITEMS ----
  q <- data.table()
  if (nrow(diet) && nrow(items) && "DietCode" %in% names(diet) && "DietCode" %in% names(items)) {
    d <- copy(diet)[, predator := .sp_name(diet)]
    loc_col <- .fb_col(d, c("Locality", "Location", "Country", "C_Code"))
    stg_col <- .fb_col(d, c("SampleStage", "Stage"))
    d <- d[, .(DietCode = as.character(DietCode), predator,
               locality = if (!is.na(loc_col)) as.character(get(loc_col)) else NA_character_,
               stage = if (!is.na(stg_col)) as.character(get(stg_col)) else NA_character_)]
    it <- copy(items)
    pct_col  <- .fb_col(it, c("DietPercent", "Percentage", "PercentFood"))
    name_col <- .fb_col(it, c("ItemName", "Foodname", "PreyName", "Prey"))
    it[, prey_name := if (!is.na(name_col)) as.character(get(name_col)) else NA_character_]
    if ("PreySpecCode" %in% names(it)) it[is.na(prey_name) | prey_name == "", prey_name := code2sp$sci[match(as.character(PreySpecCode), code2sp$SpecCode)]]
    if ("PreySpecCode" %in% names(it)) it[, prey_sci := code2sp$sci[match(as.character(PreySpecCode), code2sp$SpecCode)]][!is.na(prey_sci), prey_name := prey_sci]
    for (cc in c("FoodI", "FoodII", "FoodIII")) if (!cc %in% names(it)) it[, (cc) := NA_character_]
    it <- it[, .(DietCode = as.character(DietCode), FoodI, FoodII, FoodIII, prey_name,
                 pct = if (!is.na(pct_col)) suppressWarnings(as.numeric(get(pct_col))) else NA_real_)]
    q <- merge(it, d, by = "DietCode", allow.cartesian = TRUE)[!is.na(predator) & predator %in% sp]
    q[, n_items := .N, by = DietCode]
    q[!is.finite(pct) | pct <= 0, pct := NA_real_]
    q[, pct := fifelse(is.na(pct) & all(is.na(pct)), 1 / n_items, pct), by = DietCode]   # study with no % at all: equal split (presence)
    q <- q[is.finite(pct) & pct > 0]
    q[, p := pct / sum(pct), by = DietCode]
    q[, `:=`(tier = "FishBase/SeaLifeBase DIETITEMS (quantitative)", study = DietCode)]
  }

  ## ---- qualitative tier: FOODITEMS for species without DIETITEMS ----
  f <- data.table()
  if (nrow(food)) {
    fo <- copy(food)[, predator := .sp_name(food)]
    fo <- fo[!is.na(predator) & predator %in% setdiff(sp, unique(q$predator))]
    if (nrow(fo)) {
      name_col <- .fb_col(fo, c("Foodname", "ItemName", "PreyName", "Prey"))
      fo[, prey_name := if (!is.na(name_col)) as.character(get(name_col)) else NA_character_]
      if ("PreySpecCode" %in% names(fo)) fo[, prey_sci := code2sp$sci[match(as.character(PreySpecCode), code2sp$SpecCode)]][!is.na(prey_sci), prey_name := prey_sci]
      for (cc in c("FoodI", "FoodII", "FoodIII")) if (!cc %in% names(fo)) fo[, (cc) := NA_character_]
      loc_col <- .fb_col(fo, c("Locality", "Country", "C_Code"))
      stg_col <- .fb_col(fo, c("PredatorStage", "Stage"))
      f <- unique(fo[, .(predator, FoodI, FoodII, FoodIII, prey_name,
                         locality = if (!is.na(loc_col)) as.character(get(loc_col)) else NA_character_,
                         stage = if (!is.na(stg_col)) as.character(get(stg_col)) else NA_character_)])
      f[, `:=`(study = paste(predator, "FOODITEMS"), tier = "FishBase/SeaLifeBase FOODITEMS (presence only, equal weights - ASSUMPTION)")]
      f[, p := 1 / .N, by = study]
    }
  }
  allq <- rbindlist(list(q, f), use.names = TRUE, fill = TRUE)
  if (!nrow(allq)) { message("[FishBase diet] No usable FishBase/SeaLifeBase diet records for the ", length(sp), " predator species."); return(empty) }

  ## ---- study selection: Mediterranean first, then life stage ----
  allq <- merge(allq, unique(predators[, .(predator = species, fg_name)]), by = "predator", allow.cartesian = TRUE)
  allq[, is_med := !is.na(locality) & grepl(FB_DIET_MED_REGEX, locality, perl = TRUE)]
  if (prefer_med) allq <- allq[, if (any(is_med)) .SD[is_med == TRUE] else .SD, by = .(predator, fg_name)]
  allq[, want := fifelse(grepl("(?i)juv", fg_name, perl = TRUE), "(?i)juv|larv|recruit|young",
                  fifelse(grepl("(?i)adult", fg_name, perl = TRUE), "(?i)adult|mature", NA_character_))]
  allq[, stage_ok := is.na(want) | (!is.na(stage) & mapply(function(p, s) grepl(p, s, perl = TRUE), want, stage))]
  allq <- allq[, if (any(stage_ok)) .SD[stage_ok == TRUE] else .SD, by = .(predator, fg_name)]

  ## ---- prey resolution ----
  ukey <- unique(allq[, .(FoodI, FoodII, FoodIII, prey_name)])
  map <- fb_resolve_items(ukey, fg_ref, xwalk, fg_B, fg_universe)
  ukey[, item_id := .I]
  map <- merge(map, ukey, by = "item_id")
  allq <- merge(allq, map, by = c("FoodI", "FoodII", "FoodIII", "prey_name"), allow.cartesian = TRUE)
  unassigned <- allq[is.na(prey_fg)]
  allq <- allq[!is.na(prey_fg)]
  allq[, p := p / sum(p * w), by = .(predator, fg_name, study)]   # renormalise after dropping unassigned items (w sums to 1 per item)

  ## ---- species and FG diet ----
  sp_diet <- allq[, .(dc = sum(p * w)), by = .(predator, fg_name, study, prey_fg)][
    , .(dc = sum(dc) / uniqueN(study), n_studies = uniqueN(study)), by = .(predator, fg_name, prey_fg)]
  sp_diet[, dc := dc / sum(dc), by = .(predator, fg_name)]
  wts <- unique(predators[, .(predator = species, fg_name, b_share)])
  sp_diet <- merge(sp_diet, wts, by = c("predator", "fg_name"), all.x = TRUE)
  sp_diet[, b_share := fifelse(is.finite(b_share) & b_share > 0, b_share, NA_real_)]
  sp_diet[, b_share := fifelse(is.na(b_share), 1 / uniqueN(predator), b_share), by = fg_name]
  fg_diet <- sp_diet[, .(weight = sum(b_share * dc)), by = .(predator_fg = fg_name, prey_fg)]
  fg_diet[, weight := weight / sum(weight), by = predator_fg]

  prov <- allq[, .(n_species = uniqueN(predator), n_studies = uniqueN(study),
                   med_only = all(is_med), tiers = paste(sort(unique(tier)), collapse = " + ")), by = .(FG_name = fg_name)]
  item_map <- unique(rbindlist(list(map[, .(FoodI, FoodII, FoodIII, prey_name, prey_fg, w, how)],
                                    unassigned[, .(FoodI, FoodII, FoodIII, prey_name, prey_fg, w, how)]), fill = TRUE))
  message("[FishBase diet] Diet matrix from FishBase/SeaLifeBase: ", uniqueN(fg_diet$predator_fg), " predator FG(s), ",
          uniqueN(sp_diet$predator), " species, ", uniqueN(allq$study), " study record(s); ",
          uniqueN(unassigned[, .(FoodI, FoodII, FoodIII, prey_name)]), " prey item type(s) unassigned (see REVIEW file).")
  list(fg_diet = fg_diet[], species_diet = sp_diet[], item_map = item_map[], provenance = prov[],
       unassigned = unique(unassigned[, .(predator, fg_name, FoodI, FoodII, FoodIII, prey_name)]))
}
