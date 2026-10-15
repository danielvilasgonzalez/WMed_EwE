## =================================================================
## lib_fishbase_diet_matrix.R - LIBRARY FILE, sourced by 04_diets.R.
## Builds EwE functional-group (FG) diet matrices from two external
## diet databases:
##   (A) MAISHA - Mediterranean Archive of Integrated Stomach and
##       feeding Habits Analysis (Vascotto, Loschi, Holodkov, Ricci,
##       Cascione, Libralato, Fortibuoni, Raicevich & Agnetta 2026,
##       v1.2, OGS NODC IPT, doi:10.13120/hwpn1e, CC-BY 4.0): 213
##       Mediterranean stomach-content studies (1977-2025), 287 consumer
##       species, prey and consumers standardised to WoRMS, diet
##       contributions normalised per study.
##   (B) FishBase (Froese & Pauly, eds., www.fishbase.org) and
##       SeaLifeBase (Palomares & Pauly, eds., www.sealifebase.org) diet
##       tables, read with rfishbase (Boettiger, Lang & Wainwright 2012,
##       J. Fish Biol. 81:2030-2039):
##         DIET       one row per study (DietCode, SpecCode, Locality,
##                    SampleStage, SampleSize, Troph);
##         DIETITEMS  % composition per study (DietCode, FoodI/II/III,
##                    ItemName, DietPercent, DietSpeccode = prey SpecCode);
##         FOODITEMS  qualitative prey records (FoodI/II/III, Foodname,
##                    PreySpecCode, PredatorStage, Locality).
##       (rfishbase::ecology() is NOT a diet table: its Food*/Diet*
##       columns are trophic levels, FoodTroph/DietTroph.)
## Other inputs: FG membership and taxonomy FG_WMed_2026.csv; FG biomass
## B_j and species shares b_s|f from 01_biomass.R
## (biomass_proportion_by_species_fg.csv); food-category -> FG rules
## diet_reference_tables/fishbase_food_category_to_fg.csv.
##
## NOTATION
##  s predator species, f predator FG (s in f), d study, k prey item,
##  j prey FG, r taxonomic rank.
##  Study-level proportion  p_s,d,k = v_k / sum_k v_k, v = %W, %N, %FO or
##    DietPercent (first available in DIET_METRIC_PRIORITY order; %FO is
##    an occurrence index, used as a relative weight only when nothing
##    better exists - Hyslop 1980, J. Fish Biol. 17:411-429).
##  Prey -> FG allocation w_k->j (sum_j w = 1), first rule that applies:
##   1. species: prey name (or its SpecCode) = a species in FG_WMed_2026.csv;
##   2. taxonomic rank r = genus, family, order, class, phylum: prey name
##      (cleaned of "unidentified/unspecified ... sp./spp./remains") = the
##      rank value of species in FG_WMed_2026.csv; J(k) = all FGs holding
##      that taxon;
##   3. food category: regex rules on ItemName, FoodIII, FoodII, FoodI
##      (fishbase_food_category_to_fg.csv); J(k) = the FGs of the rule;
##      non-food items (plastic, sand, ...) are excluded.
##   Split over J(k): DIET_TAXON_SPLIT for rules 1-2 (default "equal",
##   w = 1/|J|) and DIET_CATEGORY_SPLIT for rule 3 (default "biomass",
##   w_j = B_j / sum_J B, prey taken in proportion to availability -
##   ASSUMPTION).
##  Detritus/debris items are dropped for carnivores (FishBase trophic
##  level >= DIET_DETRITUS_MAX_TL = 3, ASSUMPTION: digested remains, not
##  detritus feeding).
##  Unassigned items are dropped and the study renormalised; they are
##  listed in the *_unassigned_items_REVIEW.csv files.
##  Species diet  DC_s,j = (1/n_s) sum_d sum_k p_s,d,k w_k->j  (studies
##    weighted equally; Mediterranean studies only when s has any;
##    for "juv."/"adult" stanza FGs, studies of the matching stage when
##    available).
##  FG diet  DC_f,j = sum_{s in f} b_s|f DC_s,j / sum_{s in f} b_s|f, over
##    the species of f with diet data; every column renormalised to 1
##    (Christensen, Walters & Pauly 2005, EwE User Guide).
## =================================================================

if (!exists("FB_DIET_MED_REGEX")) {
  FB_DIET_MED_REGEX <- paste0("(?i)mediterr|alboran|balear|catal|gulf of lions|golfe du lion|ligur|tyrrhen|adriat|",
                              "ionian|aegean|sicil|sardin|corsic|spain|france|italy|malta|algeria|morocco|tunisia|",
                              "greece|turkey|ebro|valencia|marseille|naples|genoa")
}
if (!exists("DIET_TAXON_SPLIT"))    DIET_TAXON_SPLIT    <- "equal"     # "equal" | "biomass"
if (!exists("DIET_CATEGORY_SPLIT")) DIET_CATEGORY_SPLIT <- "biomass"   # "biomass" | "equal"

## --- helpers -------------------------------------------------------
.fb_col <- function(dt, candidates) intersect(candidates, names(dt))[1]
.dt_safe_write <- function(x, path) if (exists("safe_fwrite")) safe_fwrite(x, path) else fwrite(x, path)

.fb_call <- function(fn, ..., server) {
  if (!requireNamespace("rfishbase", quietly = TRUE)) return(data.table())
  if (!exists(fn, where = asNamespace("rfishbase"), inherits = FALSE)) return(data.table())
  tryCatch(as.data.table(do.call(get(fn, envir = asNamespace("rfishbase")), list(..., server = server))),
           error = function(e) { message("[Diet sources] rfishbase::", fn, "(server = '", server, "') failed: ",
                                         conditionMessage(e)); data.table() })
}

## Clean a prey label for taxonomic matching: lower case, drop
## "unidentified/unspecified/n.a.", "sp./spp./cf.", "remains/larvae/eggs",
## brackets and HTML tags.
.clean_taxon <- function(x) {
  x <- tolower(gsub("<[^>]+>", "", as.character(x)))
  x <- gsub("\\(.*?\\)", " ", x)
  x <- gsub("\\b(unidentified|unidentifed|unspecified|undetermined|indet\\.?|n\\.a\\./?|other|others|cf\\.|aff\\.)\\b", " ", x)
  x <- gsub("\\b(spp?\\.?|remains|fragments?|pieces|larvae|larva|juveniles?|eggs?|adults?)\\b", " ", x)
  trimws(gsub("[[:space:]]+", " ", gsub("[^a-z ]", " ", x)))
}

## Same cleaning as .clean_taxon() but keeps capitalisation (WoRMS queries).
.clean_taxon_keep_case <- function(x) {
  x <- gsub("<[^>]+>", "", as.character(x)); x <- gsub("\\(.*?\\)", " ", x)
  x <- gsub("(?i)\\b(unidentified|unidentifed|unspecified|undetermined|indet\\.?|n\\.a\\./?|cf\\.|aff\\.|spp?\\.?|remains|fragments?|pieces)\\b", " ", x, perl = TRUE)
  trimws(gsub("[[:space:]]+", " ", gsub("[^A-Za-z ]", " ", x)))
}

## =================================================================
## (A) MAISHA records
## Accepts the MAISHA spreadsheet (.xlsx, every sheet scored, the one
## with predator + prey + diet columns used), a .csv, or the Darwin
## Core Archive from the OGS IPT (folder or .zip with occurrence.txt +
## extendedmeasurementorfact.txt). Column names are matched from the
## candidate lists below and printed, so a schema change shows up in
## the log instead of giving a silently wrong matrix.
## =================================================================
MAISHA_COLS <- list(
  predator = c("predator_scientific_name", "predator_species", "predator", "consumer_scientific_name", "consumer",
               "predator_valid_name", "predator_name", "scientific_name_predator"),
  prey     = c("prey_scientific_name", "prey_valid_name", "prey_species", "prey", "prey_name", "prey_item", "food_item", "item"),
  w        = c("w_percent", "percent_w", "weight_percent", "weight_percentage", "w", "w_norm", "w_normalised", "w_normalized",
               "weight", "biomass_percent", "volume_percent", "v_percent", "volume"),
  n        = c("n_percent", "percent_n", "number_percent", "number_percentage", "n", "n_norm", "n_normalised", "n_normalized", "number"),
  fo       = c("fo_percent", "percent_fo", "f_percent", "frequency_of_occurrence", "fo", "fo_norm", "fo_normalised", "fo_normalized", "frequency"),
  iri      = c("iri_percent", "percent_iri", "iri"),
  contrib  = c("c", "diet_contribution", "contribution", "normalised_contribution", "normalized_contribution", "relative_importance",
               "diet_proportion", "proportion", "diet_percent", "percentage", "value"),
  study    = c("study_id", "reference", "source", "doi", "citation", "paper", "study", "id_study", "ref"),
  locality = c("locality", "area", "region", "gsa", "country", "location", "study_area"),
  stage    = c("life_stage", "stage", "predator_stage", "size_class", "ontogenetic_stage"),
  lon      = c("decimal_longitude", "longitude", "lon"),
  lat      = c("decimal_latitude", "latitude", "lat")
)

.clean_names <- function(x) {
  x <- tolower(gsub("([a-z])([A-Z])", "\\1_\\2", x))
  x <- gsub("%", "_percent_", x)
  x <- gsub("^_|_$", "", gsub("_+", "_", gsub("[^a-z0-9]+", "_", x)))
  sub("^percent_(.*)$", "\\1_percent", x)                 # "%W" -> "w_percent", same as "W%"
}

.maisha_from_table <- function(raw, label) {
  setnames(raw, .clean_names(names(raw)))
  pick <- lapply(MAISHA_COLS, function(cands) .fb_col(raw, cands))
  message("[MAISHA] ", label, " - columns used: ",
          paste(names(pick), vapply(pick, function(x) ifelse(is.na(x), "-", x), ""), sep = "=", collapse = "; "))
  if (is.na(pick$predator) || is.na(pick$prey)) {
    message("[MAISHA] No predator and/or prey column recognised (columns: ", paste(names(raw), collapse = ", "),
            ") - add the right name to MAISHA_COLS in lib_fishbase_diet_matrix.R.")
    return(data.table())
  }
  metric_cols <- c(W = pick$w, IRI = pick$iri, N = pick$n, FO = pick$fo, C = pick$contrib)
  metric_cols <- metric_cols[!is.na(metric_cols)]
  if (!length(metric_cols)) { message("[MAISHA] No diet-contribution column recognised in ", label, "."); return(data.table()) }
  ## metric order as in the metaweb tier (diet_metric_priority.csv): IRI > %W > %N > %FO,
  ## then a single pre-normalised contribution column
  pr <- intersect(c("IRI", "W", "N", "FO", "C"), names(metric_cols))
  num <- function(col) suppressWarnings(as.numeric(gsub(",", ".", as.character(raw[[col]]))))
  vals <- sapply(metric_cols[pr], num)
  if (is.null(dim(vals))) vals <- matrix(vals, ncol = length(pr), dimnames = list(NULL, pr))
  first_ok <- apply(vals, 1, function(v) { i <- which(is.finite(v) & v > 0)[1]; if (is.na(i)) NA_integer_ else i })
  out <- data.table(
    predator  = trimws(as.character(raw[[pick$predator]])),
    prey_name = trimws(as.character(raw[[pick$prey]])),
    v         = vals[cbind(seq_len(nrow(raw)), ifelse(is.na(first_ok), 1L, first_ok))],
    metric    = ifelse(is.na(first_ok), NA_character_, pr[first_ok]),
    study     = if (!is.na(pick$study)) as.character(raw[[pick$study]]) else NA_character_,
    locality  = if (!is.na(pick$locality)) as.character(raw[[pick$locality]]) else NA_character_,
    stage     = if (!is.na(pick$stage)) as.character(raw[[pick$stage]]) else NA_character_,
    lon       = if (!is.na(pick$lon)) suppressWarnings(as.numeric(raw[[pick$lon]])) else NA_real_
  )
  out <- out[!is.na(predator) & predator != "" & !is.na(prey_name) & prey_name != "" & is.finite(v) & v > 0 & !is.na(metric)]
  ## one study = one metric (the best one available in that study)
  out[is.na(study) | study == "", study := paste(predator, "MAISHA")]
  out[, metric_rank := match(metric, pr)]
  out <- out[, .SD[metric_rank == min(metric_rank)], by = .(predator, study)]
  out[, p := v / sum(v), by = .(predator, study)][, metric_rank := NULL]
  out
}

## MAISHA Darwin Core Archive layout (v1.2, checked on the real files):
##  occurrence.txt (tab): one "Predator" record per study x predator
##   (occurrenceRemarks == "Predator"; bibliographicCitation, eventDate,
##   decimalLatitude/Longitude) whose associatedOccurrences lists its
##   prey records ("Stomach content: OCC482, OCC483, ..."); prey records
##   carry scientificName (WoRMS) and verbatimIdentification.
##  extendedmeasurementorfact.txt (tab): per prey record "Gravimetric
##   percentage", "Numerosity percentage", "Frequency of occurrence"
##   (proportions); per predator record the "... of unknown / debris /
##   detritus" fractions. Detritus is kept as a prey item; unknown and
##   debris are dropped (the study is renormalised).
## Studies outside the Mediterranean (some MAISHA predators come from
## Atlantic studies) are tagged by coordinates (lon -6 to 36.5, lat 30 to
## 46) so the Mediterranean-first rule can drop them when the predator
## has Mediterranean studies.
.maisha_from_dwca <- function(dir) {
  occ_f <- list.files(dir, pattern = "^occurrence\\.(txt|csv)$", full.names = TRUE, ignore.case = TRUE)[1]
  emf_f <- list.files(dir, pattern = "measurementorfact", full.names = TRUE, ignore.case = TRUE)[1]
  if (is.na(occ_f) || is.na(emf_f)) { message("[MAISHA] occurrence.txt / extendedmeasurementorfact.txt not found in ", dir, "."); return(data.table()) }
  occ <- fread(occ_f, sep = "\t", quote = "", encoding = "UTF-8", colClasses = "character")
  emf <- fread(emf_f, sep = "\t", quote = "", encoding = "UTF-8", colClasses = "character")
  if (!all(c("id", "occurrenceRemarks", "associatedOccurrences", "scientificName") %in% names(occ))) {
    message("[MAISHA] Unexpected occurrence.txt columns: ", paste(names(occ), collapse = ", ")); return(data.table())
  }
  pred <- occ[occurrenceRemarks == "Predator",
              .(predator_id = id, predator = scientificName, study_ref = bibliographicCitation, year = eventDate,
                lat = suppressWarnings(as.numeric(decimalLatitude)), lon = suppressWarnings(as.numeric(decimalLongitude)),
                assoc = associatedOccurrences)]
  links <- pred[, .(prey_id = unlist(regmatches(assoc, gregexpr("OCC[0-9]+", assoc)))), by = predator_id]
  prey  <- occ[, .(prey_id = id, prey_name = fifelse(nzchar(scientificName), scientificName, verbatimIdentification))]
  m <- emf[, .(id, type = measurementType, val = suppressWarnings(as.numeric(measurementValue)))]
  m[, metric := fcase(type == "Gravimetric percentage", "W", type == "Numerosity percentage", "N",
                      type == "Frequency of occurrence", "FO",
                      type == "Gravimetric percentage of detritus", "W_det", type == "Numerosity percentage of detritus", "N_det",
                      type == "Frequency of detritus", "FO_det", default = NA_character_)]
  m <- m[!is.na(metric) & is.finite(val)]
  pm <- dcast(m[metric %in% c("W", "N", "FO")], id ~ metric, value.var = "val", fun.aggregate = function(x) x[1])
  tab <- merge(merge(links, prey, by = "prey_id"), pm, by.x = "prey_id", by.y = "id", all.x = TRUE)
  ## predator-level detritus fraction as an extra prey row
  det <- dcast(m[metric %like% "_det$"], id ~ metric, value.var = "val", fun.aggregate = function(x) x[1])
  if (nrow(det)) {
    setnames(det, sub("_det$", "", names(det)))
    tab <- rbindlist(list(tab, det[, c(list(predator_id = id, prey_id = paste0(id, "_det"), prey_name = "detritus"), .SD), .SDcols = intersect(c("W", "N", "FO"), names(det))]), fill = TRUE)
  }
  tab <- merge(tab, pred[, .(predator_id, predator, study_ref, year, lat, lon)], by = "predator_id")
  tab[, locality := fifelse(is.finite(lon) & is.finite(lat) & lon >= -6 & lon <= 36.5 & lat >= 30 & lat <= 46,
                            "Mediterranean (coordinates)", "outside Mediterranean (coordinates)")]
  for (cc in c("W", "N", "FO")) if (!cc %in% names(tab)) tab[, (cc) := NA_real_]
  message("[MAISHA] DwC-A: ", nrow(pred), " predator records (study x predator), ", nrow(tab), " prey records; ",
          pred[is.finite(lon) & lon >= -6 & lon <= 36.5 & lat >= 30 & lat <= 46, .N], " predator records inside the Mediterranean box.")
  out <- tab[, .(predator, prey_name, W, N, FO, study = predator_id, locality, lon, ref = study_ref)]
  out <- .maisha_from_table(out, "DwC-A")
  out
}

maisha_diet_records <- function(path) {
  if (is.null(path) || !length(path) || is.na(path) || !file.exists(path)) return(data.table())
  if (dir.exists(path)) return(.maisha_from_dwca(path))
  ext <- tolower(tools::file_ext(path))
  if (ext == "zip") { td <- file.path(tempdir(), "maisha_dwca"); utils::unzip(path, exdir = td); return(.maisha_from_dwca(td)) }
  if (ext == "csv") return(.maisha_from_table(fread(path, encoding = "UTF-8"), basename(path)))
  if (ext %in% c("xlsx", "xls")) {
    sh <- readxl::excel_sheets(path)
    tabs <- lapply(sh, function(s) as.data.table(readxl::read_excel(path, sheet = s)))
    score <- vapply(tabs, function(x) { nm <- .clean_names(names(x))
      sum(vapply(MAISHA_COLS[c("predator", "prey", "w", "n", "fo", "iri", "contrib")], function(c) any(nm %in% c), logical(1))) }, numeric(1))
    best <- which.max(score)
    message("[MAISHA] ", basename(path), ": sheets ", paste0(sh, " (score ", score, ")", collapse = ", "), " - using '", sh[best], "'.")
    return(.maisha_from_table(tabs[[best]], paste0(basename(path), " [", sh[best], "]")))
  }
  message("[MAISHA] Unsupported file type: ", path); data.table()
}

## =================================================================
## (B) FishBase / SeaLifeBase records
## =================================================================
fb_fetch_diet_tables <- function(species, cache_dir, force_refresh = FALSE) {
  paths <- file.path(cache_dir, c("fishbase_diet_raw_DIET.csv", "fishbase_diet_raw_DIETITEMS.csv",
                                  "fishbase_diet_raw_FOODITEMS.csv", "fishbase_diet_raw_TAXA.csv"))
  names(paths) <- c("diet", "items", "food", "taxa")
  if (!force_refresh && all(file.exists(paths[c("diet", "items", "food")]))) {
    message("[FishBase diet] Using cached FishBase/SeaLifeBase diet tables in ", cache_dir, " (force_refresh = TRUE to re-query).")
    return(lapply(paths, function(p) if (file.exists(p)) fread(p, encoding = "UTF-8") else data.table()))
  }
  srv <- c("fishbase", "sealifebase")
  get_all <- function(fn, ...) rbindlist(lapply(srv, function(s) { x <- .fb_call(fn, ..., server = s); if (nrow(x)) x[, server := s]; x }), fill = TRUE)
  diet  <- get_all("diet", species)
  items <- data.table()
  if (nrow(diet) && "DietCode" %in% names(diet)) {
    items <- get_all("diet_items")
    if (nrow(items) && "DietCode" %in% names(items)) items <- items[paste(server, DietCode) %in% diet[, paste(server, DietCode)]]
  }
  food <- get_all("fooditems", species)
  taxa <- get_all("load_taxa")
  if (nrow(taxa)) taxa <- unique(taxa[, intersect(c("SpecCode", "Species", "server"), names(taxa)), with = FALSE])
  out <- list(diet = diet, items = items, food = food, taxa = taxa)
  for (n in names(out)) if (nrow(out[[n]])) .dt_safe_write(out[[n]], paths[[n]])
  message("[FishBase diet] Pulled DIET ", nrow(diet), ", DIETITEMS ", nrow(items), ", FOODITEMS ", nrow(food),
          " row(s) for ", length(species), " predator species; cached in ", cache_dir, ".")
  out
}

fishbase_diet_records <- function(species, cache_dir, force_refresh = FALSE) {
  tb <- fb_fetch_diet_tables(species, cache_dir, force_refresh)
  diet <- tb$diet; items <- tb$items; food <- tb$food; taxa <- tb$taxa
  if (!"server" %in% names(diet) && nrow(diet)) diet[, server := "fishbase"]
  if (!"server" %in% names(items) && nrow(items)) items[, server := "fishbase"]
  code2sp <- if (nrow(taxa) && all(c("SpecCode", "Species") %in% names(taxa)))
    unique(taxa[, .(key = paste(if ("server" %in% names(taxa)) server else "fishbase", SpecCode), sci = Species)]) else data.table(key = character(), sci = character())
  sp_of <- function(server, code) code2sp$sci[match(paste(server, code), code2sp$key)]
  q <- data.table()
  if (nrow(diet) && nrow(items) && all(c("DietCode", "Species") %in% names(diet))) {
    d <- diet[Species %in% species, .(server, DietCode = as.character(DietCode), predator = Species,
                                      troph = suppressWarnings(as.numeric(if ("Troph" %in% names(diet)) Troph else NA)),
                                      locality = as.character(if ("Locality" %in% names(diet)) Locality else NA),
                                      stage = as.character(if ("SampleStage" %in% names(diet)) SampleStage else NA))]
    it <- copy(items)[, DietCode := as.character(DietCode)]
    for (cc in c("FoodI", "FoodII", "FoodIII", "ItemName")) if (!cc %in% names(it)) it[, (cc) := NA_character_]
    it[, prey_sci := NA_character_]
    if ("DietSpeccode" %in% names(it))    it[, prey_sci := sp_of("fishbase", DietSpeccode)]
    if ("DietSpeccodeSLB" %in% names(it)) it[is.na(prey_sci), prey_sci := sp_of("sealifebase", DietSpeccodeSLB)]
    pct <- .fb_col(it, c("DietPercent", "Percentage", "PercentFood"))
    it <- it[, .(server, DietCode, FoodI, FoodII, FoodIII, prey_name = fifelse(!is.na(prey_sci), prey_sci, as.character(ItemName)),
                 v = if (!is.na(pct)) suppressWarnings(as.numeric(get(pct))) else NA_real_)]
    q <- merge(it, d, by = c("server", "DietCode"), allow.cartesian = TRUE)
    q[, study := paste(server, DietCode)]
    q[, n_items := .N, by = study]
    q[!is.finite(v) | v <= 0, v := NA_real_]
    q[, v := if (all(is.na(v))) 1 / n_items else v, by = study]     # study with no % at all: equal split (presence)
    q <- q[is.finite(v) & v > 0]
    q[, `:=`(p = v / sum(v), metric = "DietPercent"), by = study]
    q[, tier := "FishBase/SeaLifeBase DIETITEMS"]
  }
  f <- data.table()
  if (nrow(food) && "Species" %in% names(food)) {
    fo <- food[Species %in% setdiff(species, unique(q$predator))]
    if (nrow(fo)) {
      if (!"server" %in% names(fo)) fo[, server := "fishbase"]
      for (cc in c("FoodI", "FoodII", "FoodIII", "Foodname")) if (!cc %in% names(fo)) fo[, (cc) := NA_character_]
      fo[, prey_sci := if ("PreySpecCode" %in% names(fo)) sp_of("fishbase", PreySpecCode) else NA_character_]
      if ("PreySpecCodeSLB" %in% names(fo)) fo[is.na(prey_sci), prey_sci := sp_of("sealifebase", PreySpecCodeSLB)]
      f <- unique(fo[, .(predator = Species, FoodI, FoodII, FoodIII,
                         prey_name = fifelse(!is.na(prey_sci), prey_sci, as.character(Foodname)),
                         locality = as.character(if ("Locality" %in% names(fo)) Locality else NA),
                         stage = as.character(if ("PredatorStage" %in% names(fo)) PredatorStage else NA))])
      f[, `:=`(study = paste(predator, "FOODITEMS"), metric = "presence", tier = "FishBase/SeaLifeBase FOODITEMS (presence, equal weights - ASSUMPTION)")]
      f[, p := 1 / .N, by = study]
    }
  }
  rbindlist(list(q, f), use.names = TRUE, fill = TRUE)
}

## =================================================================
## Prey -> FG resolution (shared by both sources)
## =================================================================
.split_w <- function(fgs, B, how) {
  fgs <- unique(fgs)
  if (identical(how, "biomass")) {
    bt <- B[fg_name %in% fgs & is.finite(B) & B > 0]
    if (nrow(bt)) return(list(fg = bt$fg_name, w = bt$B / sum(bt$B)))
  }
  list(fg = fgs, w = rep(1 / length(fgs), length(fgs)))
}

diet_resolve_prey <- function(keys, fg_ref, xwalk, fg_B, fg_universe,
                              taxon_split = DIET_TAXON_SPLIT, category_split = DIET_CATEGORY_SPLIT) {
  ref <- copy(fg_ref)[fg_name %in% fg_universe]
  ranks <- intersect(c("Genus", "Family", "Order", "Class", "Phylum"), names(ref))
  ref[, sp_l := tolower(trimws(species))]
  for (r in ranks) ref[, (paste0(r, "_l")) := tolower(trimws(get(r)))]
  B <- fg_B[fg_name %in% fg_universe]
  xw <- copy(xwalk)[, ord := .I]
  res <- vector("list", nrow(keys))
  for (i in seq_len(nrow(keys))) {
    raw <- keys$prey_name[i]; nm <- tolower(trimws(raw)); cl <- .clean_taxon(raw)
    out <- NULL
    ## eggs and larvae are plankton, whatever their taxon: skip taxonomic
    ## matching and let the category rules place them (zooplankton).
    early <- grepl("(?i)larv|\\beggs?\\b|zoea|megalop|naupli|phyllosom", raw, perl = TRUE)
    hit <- if (early) character(0) else unique(ref[sp_l == nm | sp_l == cl, fg_name])
    if (length(hit)) out <- c(.split_w(hit, B, taxon_split), how = "species")
    if (is.null(out) && nzchar(cl) && !early) {
      for (r in ranks) {
        val <- if (r == "Genus") unique(c(cl, sub(" .*", "", cl))) else cl
        hit <- unique(ref[get(paste0(r, "_l")) %in% val, fg_name])
        if (length(hit)) { out <- c(.split_w(hit, B, taxon_split), how = tolower(r)); break }
      }
    }
    if (is.null(out)) {
      cats <- c(ItemName = raw, FoodIII = keys$FoodIII[i], FoodII = keys$FoodII[i], FoodI = keys$FoodI[i])
      for (lvl in names(cats)) {
        txt <- cats[[lvl]]
        if (is.na(txt) || !nzchar(txt)) next
        m <- xw[(level == lvl | level == "any") & vapply(pattern, function(p) grepl(p, txt, perl = TRUE), logical(1))]
        if (!nrow(m)) next
        row <- m[order(ord)][1]
        tgt <- if (is.na(row$FG_names) || !nzchar(row$FG_names)) character(0) else intersect(trimws(strsplit(row$FG_names, ";", fixed = TRUE)[[1]]), fg_universe)
        out <- if (length(tgt)) c(.split_w(tgt, B, category_split), how = paste0("category ", lvl, " '", txt, "'"))
               else list(fg = NA_character_, w = NA_real_, how = paste0("excluded (non-food / not in model): ", lvl, " '", txt, "'"))
        break
      }
    }
    if (is.null(out)) out <- list(fg = NA_character_, w = NA_real_, how = "unassigned")
    res[[i]] <- data.table(item_id = i, prey_fg = out$fg, w = out$w, how = out$how)
  }
  rbindlist(res)
}

## =================================================================
## Records -> species and FG diet matrices (shared by both sources)
## records: predator, prey_name, p, study, locality, stage, tier
##          (+ FoodI/FoodII/FoodIII for FishBase)
## predators: species, fg_name, b_share
## =================================================================
diet_records_to_fg_matrix <- function(records, predators, fg_ref, fg_B, xwalk, fg_universe, prefer_med = TRUE, label = "",
                                      worms_cache = NULL) {
  empty <- list(fg_diet = data.table(predator_fg = character(), prey_fg = character(), weight = numeric()),
                species_diet = data.table(), item_map = data.table(), provenance = data.table(), unassigned = data.table())
  if (!nrow(records)) return(empty)
  for (cc in c("FoodI", "FoodII", "FoodIII", "locality", "stage", "tier")) if (!cc %in% names(records)) records[, (cc) := NA_character_]
  r <- merge(records, unique(predators[, .(predator = species, fg_name)]), by = "predator", allow.cartesian = TRUE)
  if (!nrow(r)) { message("[Diet sources] ", label, ": no record matched a model predator species."); return(empty) }
  r[, is_med := !is.na(locality) & grepl(FB_DIET_MED_REGEX, locality, perl = TRUE)]
  if (prefer_med) r <- r[, if (any(is_med)) .SD[is_med == TRUE] else .SD, by = .(predator, fg_name)]
  r[, want := fifelse(grepl("(?i)juv", fg_name, perl = TRUE), "(?i)juv|larv|recruit|young|small",
               fifelse(grepl("(?i)adult", fg_name, perl = TRUE), "(?i)adult|mature|large", NA_character_))]
  r[, stage_ok := is.na(want) | (!is.na(stage) & mapply(function(p, s) grepl(p, s, perl = TRUE), want, stage))]
  r <- r[, if (any(stage_ok)) .SD[stage_ok == TRUE] else .SD, by = .(predator, fg_name)]

  keys <- unique(r[, .(FoodI, FoodII, FoodIII, prey_name)])
  map  <- diet_resolve_prey(keys, fg_ref, xwalk, fg_B, fg_universe)
  keys[, item_id := .I]
  ## Second pass for prey names nothing matched (e.g. copepod genera,
  ## "Hyperiidea"): WoRMS classification (World Register of Marine
  ## Species, via worrms - Chamberlain 2023; lib_worms_taxonomy_lookup.R,
  ## cached) gives genus/family/order/class/phylum, which are matched to
  ## the FG taxonomy and then to the category rules.
  un_ids <- map[how == "unassigned", unique(item_id)]
  if (length(un_ids) && exists("worms_taxonomy_lookup") && !is.null(worms_cache)) {
    nm <- unique(.clean_taxon_keep_case(keys$prey_name[un_ids]))
    nm <- nm[nzchar(nm)]
    wt <- tryCatch(as.data.table(worms_taxonomy_lookup(nm, cache_path = worms_cache)), error = function(e) {
      message("[Diet sources] WoRMS lookup failed: ", conditionMessage(e)); data.table() })
    if (nrow(wt)) {
      for (id in un_ids) {
        w <- wt[original_name == .clean_taxon_keep_case(keys$prey_name[id])][1]
        if (!nrow(w) || is.na(w$original_name)) next
        ranks <- c(genus = w$genus, family = w$family, order = w$order, class = w$class, phylum = w$phylum)
        ranks <- ranks[!is.na(ranks) & nzchar(ranks)]
        if (!length(ranks)) next
        k2 <- data.table(FoodI = NA_character_, FoodII = NA_character_, FoodIII = NA_character_, prey_name = ranks)
        m2 <- diet_resolve_prey(k2, fg_ref, xwalk, fg_B, fg_universe)
        first_ok <- m2[how != "unassigned" & !grepl("^excluded", how), min(item_id)]
        if (is.finite(first_ok)) {
          map <- rbind(map[item_id != id], m2[item_id == first_ok][, `:=`(item_id = id, how = paste0("WoRMS ", names(ranks)[first_ok], " '", ranks[first_ok], "' -> ", how))])
        }
      }
    }
  }
  map  <- merge(map, keys, by = "item_id")[, item_id := NULL]
  r <- merge(r, map, by = c("FoodI", "FoodII", "FoodIII", "prey_name"), allow.cartesian = TRUE)
  ## Carnivores: "debris/carcasses/remains" in their stomachs are digested
  ## prey, not detritus intake - dropped (ASSUMPTION) when the predator's
  ## FishBase trophic level (DIET Troph, species mean) >= DIET_DETRITUS_MAX_TL.
  if (!exists("DIET_DETRITUS_MAX_TL")) DIET_DETRITUS_MAX_TL <- 3
  if ("troph" %in% names(r)) {
    tl <- r[is.finite(troph), .(tl = mean(troph)), by = predator]
    r <- merge(r, tl, by = "predator", all.x = TRUE)
    r[prey_fg == "Detritus" & is.finite(tl) & tl >= DIET_DETRITUS_MAX_TL,
      `:=`(prey_fg = NA_character_, how = paste0("excluded: detritus/debris in a carnivore (TL ", round(tl, 1), " >= ", DIET_DETRITUS_MAX_TL, ")"))]
    r[, tl := NULL]
  }
  unassigned <- unique(r[is.na(prey_fg), .(predator, fg_name, FoodI, FoodII, FoodIII, prey_name, how)])
  r <- r[!is.na(prey_fg)]
  r[, p := p / sum(p * w), by = .(predator, fg_name, study)]   # renormalise after dropping unassigned items (w sums to 1 per item)

  sp_diet <- r[, .(dc = sum(p * w)), by = .(predator, fg_name, study, prey_fg)][
    , .(dc = sum(dc) / uniqueN(study), n_studies = uniqueN(study)), by = .(predator, fg_name, prey_fg)]
  sp_diet[, dc := dc / sum(dc), by = .(predator, fg_name)]
  sp_diet <- merge(sp_diet, unique(predators[, .(predator = species, fg_name, b_share)]), by = c("predator", "fg_name"), all.x = TRUE)
  sp_diet[!is.finite(b_share) | b_share <= 0, b_share := NA_real_]
  sp_diet[, b_share := fifelse(is.na(b_share), 1 / uniqueN(predator), b_share), by = fg_name]
  fg_diet <- sp_diet[, .(weight = sum(b_share * dc)), by = .(predator_fg = fg_name, prey_fg)]
  fg_diet[, weight := weight / sum(weight), by = predator_fg]

  prov <- r[, .(n_species = uniqueN(predator), n_studies = uniqueN(study), med_only = all(is_med),
                tiers = paste(sort(unique(tier)), collapse = " + ")), by = .(FG_name = fg_name)]
  message("[Diet sources] ", label, ": ", uniqueN(fg_diet$predator_fg), " predator FG(s), ", uniqueN(sp_diet$predator),
          " species, ", uniqueN(r$study), " study record(s); ", nrow(unique(unassigned[, .(prey_name, FoodIII)])),
          " prey item type(s) unassigned or excluded (REVIEW file).")
  list(fg_diet = fg_diet[], species_diet = sp_diet[], item_map = unique(map)[], provenance = prov[], unassigned = unassigned[])
}

## Backwards-compatible wrapper (FishBase only)
build_fishbase_diet_matrix <- function(predators, fg_ref, fg_B, xwalk, cache_dir, fg_universe = NULL,
                                       force_refresh = FALSE, prefer_med = TRUE) {
  if (is.null(fg_universe)) fg_universe <- unique(fg_ref$fg_name)
  rec <- fishbase_diet_records(unique(predators$species), cache_dir, force_refresh)
  diet_records_to_fg_matrix(rec, predators, fg_ref, fg_B, xwalk, fg_universe, prefer_med, label = "FishBase/SeaLifeBase")
}

## Write the standard set of outputs for one source.
write_diet_source_outputs <- function(res, group_table, out_dir, prefix) {
  if (!nrow(res$fg_diet)) return(invisible(NULL))
  fwrite(res$fg_diet, file.path(out_dir, paste0(prefix, "_diet_matrix_fg_long.csv")))
  if (exists("build_ecopath_diet_sheet")) fwrite(build_ecopath_diet_sheet(res$fg_diet, group_table), file.path(out_dir, paste0(prefix, "_diet_matrix_fg_wide.csv")))
  fwrite(res$species_diet, file.path(out_dir, paste0(prefix, "_diet_species_level.csv")))
  fwrite(res$item_map, file.path(out_dir, paste0(prefix, "_diet_item_mapping_REVIEW.csv")))
  fwrite(res$unassigned, file.path(out_dir, paste0(prefix, "_diet_unassigned_items_REVIEW.csv")))
  fwrite(res$provenance, file.path(out_dir, paste0(prefix, "_diet_provenance_by_fg.csv")))
  fwrite(res$fg_diet[, .(total = sum(weight), n_prey_fg = .N), by = predator_fg], file.path(out_dir, paste0(prefix, "_diet_matrix_checks.csv")))
  invisible(NULL)
}
