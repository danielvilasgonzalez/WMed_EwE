## =================================================================
## Created by: Daniel Vilas
## 03b_ecobase.R
##
## Library file - no top-level driver code, same convention as
## lib_survey_fg_density_functions.R. Sourced automatically from
## 03_pbqb-traits.R (fetch_ecobase_literature_pb_qb()) and from
## 01_biomass.R (fetch_ecobase_literature_biomass()) - it is not meant
## to be run as its own standalone numbered pipeline step.
##
## Compiles PB/QB/Biomass values from EXISTING PUBLISHED ECOPATH MODELS
## (via EcoBase), as a literature-derived alternative/supplement to the
## empirical-formula PB/QB estimates computed in 03_pbqb-traits.R, and
## - per Andrea's 2026-09-24 request - as the PREFERRED source (over
## hand-copying numbers out of a paper's supplementary tables) for
## biomass on functional groups no survey here samples at all:
## macro-/meso-/microzooplankton, large/small phytoplankton, Posidonia/
## seagrass, other macroalgae, gorgonians and corals.
##
## EcoBase (https://ecobase.ecopath.org/) is an open-access repository
## of published Ecopath models, queryable via a SOAP/XML web service
## documented at https://ecobase.ecopath.org/ (see "script to play
## with EcoBase"). This adapts their official example from RCurl/XML
## to httr/xml2 (more modern, better maintained).
##
## NOT TESTED against the live service from this session - there is no
## network access to sirs.agrocampus-ouest.fr or ecobase.ecopath.org
## from this sandbox, so this needs to be verified by actually running
## it, ESPECIALLY the biomass column name guessed at below (see
## BIOMASS_COL_CANDIDATES) - EcoBase's own group-input XML fields
## aren't independently documented anywhere this session could reach;
## the diagnostics below print every column name actually found so a
## wrong guess is visible immediately rather than silently wrong.
##
## 2026-09-24, per Andrea: two more changes -
##   1. Default bounding box narrowed to the WESTERN Mediterranean
##      specifically (WESTMED_BBOX below), not the whole Mediterranean
##      basin (Gibraltar to the Levant) - this pipeline is a Western Med
##      model, so an Aegean/Levantine EcoBase model isn't a meaningfully
##      comparable "other model of this same sea" the way a Gulf of
##      Lion/Catalan/Balearic/Alboran/Tyrrhenian one is.
##   2. EcoBase queries now consider the REFERENCE YEAR of each candidate
##      model, not just whether it's geographically in the West Med -
##      both fetch_ecobase_literature_pb_qb() and fetch_ecobase_literature_
##      biomass() take a target_year argument and, when supplied, reduce
##      multiple candidate models down to whichever one's own model year
##      is CLOSEST to target_year (per FG/group), rather than reporting
##      every match with equal weight regardless of how old/recent it is
##      relative to this model's own 1994-1996 base period.
## =================================================================

## The Western Mediterranean sub-basin - Gibraltar/Alboran Sea through
## the Sicily/Sardinia Channel - matches this pipeline's own FILTER_AREAS
## (GSA 1-11) more closely than the full Mediterranean basin. Used as the
## new default med_bbox for every EcoBase query below; still overridable
## per call (e.g. widen it back out if you deliberately want Eastern
## Mediterranean comparison models too).
WESTMED_BBOX <- c(lon_min = -6, lat_min = 30, lon_max = 12, lat_max = 46)

## fetch_ecobase_raw_inputs(out_dir, force_refresh, med_bbox, timeout_sec)
##
## Shared core: queries EcoBase for every published model geographically
## overlapping med_bbox, fetches per-group input values (Biomass/PB/QB/
## whatever else EcoBase's <group> node carries) for the ones flagged
## dissemination_allow == "true", merges in model-level metadata (name/
## year/authors), and returns ONE data.table with every raw per-group
## column plus model_id/EwE_model/year/authors - or NULL on failure.
## Both fetch_ecobase_literature_pb_qb() and fetch_ecobase_literature_
## biomass() call this and then just pick out/rename the columns they
## each need, so there's only ever ONE network round-trip against
## EcoBase per pipeline run (whichever of the two functions runs
## first populates the shared cache below; the second one just reads
## it back).
##
## Caches to ecobase_all_inputs_with_meta.csv in out_dir.
fetch_ecobase_raw_inputs <- function(out_dir, force_refresh = FALSE,
                                     med_bbox = WESTMED_BBOX,
                                     timeout_sec = 60) {
  raw_meta_path <- file.path(out_dir, "ecobase_all_inputs_with_meta.csv")
  
  if (!force_refresh && file.exists(raw_meta_path)) {
    message("fetch_ecobase_raw_inputs(): using cached ", raw_meta_path,
            " (pass force_refresh = TRUE to re-query EcoBase instead).")
    return(invisible(as.data.table(fread(raw_meta_path))))
  }
  
  if (!requireNamespace("httr", quietly = TRUE) || !requireNamespace("xml2", quietly = TRUE)) {
    message("fetch_ecobase_raw_inputs(): 'httr' and 'xml2' packages are required -",
            " skipping the EcoBase query. Install them to enable this step.")
    return(invisible(NULL))
  }
  
  result <- tryCatch({
    
    library(httr); library(xml2); library(data.table)
    
    ECOBASE_LIST_URL   <- "http://sirs.agrocampus-ouest.fr/EcoBase/php/webser/soap-client_3.php"
    ECOBASE_INPUT_URL  <- "http://sirs.agrocampus-ouest.fr/EcoBase/php/webser/soap-client.php"
    
    ## --- 1. Get the full list of available models ------------------------
    message("Fetching EcoBase model list...")
    resp <- tryCatch(httr::GET(ECOBASE_LIST_URL, httr::timeout(timeout_sec)),
                     error = function(e) {
                       message("Failed to reach EcoBase model list endpoint: ", conditionMessage(e))
                       NULL
                     })
    if (is.null(resp) || httr::status_code(resp) != 200) {
      stop("Could not fetch the EcoBase model list (check the URL still works in a",
           " browser, and that ", ECOBASE_LIST_URL, " hasn't moved).")
    }
    
    xml_content <- xml2::read_xml(httr::content(resp, as = "text", encoding = "UTF-8"))
    
    message("\nTop-level XML structure (inspect this if parsing below fails):")
    xml2::xml_structure(xml_content, indent = 2)
    
    ## Convert one XML node's children into a single-row data.table -
    ## DEFENSIVELY, because a single <model> (or, in fetch_model_inputs()
    ## below, a single <group>) node can repeat the SAME child tag
    ## (confirmed in EcoBase's own XML, e.g. a repeated
    ## <comments_objectives>) - setNames(as.list(...), xml_name(fields))
    ## would then produce a list with two elements sharing one name, and
    ## as.data.table() on that silently creates a data.table with two
    ## SAME-NAMED columns (data.table allows this at creation time; it's
    ## only the moment something needs unique names, like merge(), that
    ## it gets rejected). Collapsing same-named values into ONE column
    ## here (joined with "; ", nothing dropped) fixes it at the source.
    xml_node_to_dt_row <- function(node) {
      fields <- xml2::xml_children(node)
      field_names <- xml2::xml_name(fields)
      field_vals  <- xml2::xml_text(fields)
      if (anyDuplicated(field_names) > 0) {
        collapsed <- tapply(field_vals, field_names, paste, collapse = "; ")
        field_names <- names(collapsed)
        field_vals  <- as.character(collapsed)
      }
      as.data.table(setNames(as.list(field_vals), field_names))
    }
    
    model_nodes <- xml2::xml_find_all(xml_content, ".//model")
    message("\nFound ", length(model_nodes), " model node(s) in the response.")
    if (length(model_nodes) == 0) {
      message("No <model> nodes found with that path - check the printed structure",
              " above and adjust the xml_find_all() XPath to match what's actually there.")
    }
    
    model_list <- rbindlist(lapply(model_nodes, xml_node_to_dt_row), fill = TRUE)
    message("\nParsed model list - columns available:")
    print(names(model_list))
    
    fwrite(model_list, file.path(out_dir, "ecobase_model_list_full.csv"))
    message("\nSaved full model list to ecobase_model_list_full.csv")
    
    ## --- 2. Filter to models geographically in med_bbox -------------------
    ## Uses geographic_extent (format: "BOX(lon1 lat1,lon2 lat2)") rather
    ## than keyword-matching text fields - more robust, since it doesn't
    ## depend on how a model's country/ecosystem name happens to be
    ## written. Verified the parsing + overlap logic against real sample
    ## geographic_extent values before relying on it here (an Aegean Sea
    ## entry correctly flagged TRUE; Alaska/PNG/Bering/Antarctica/Florida
    ## entries correctly flagged FALSE).
    parse_geo_box <- function(box_str) {
      m <- regmatches(box_str, regexec("BOX\\(([-0-9.]+) ([-0-9.]+),([-0-9.]+) ([-0-9.]+)\\)", box_str))[[1]]
      if (length(m) != 5) return(c(lon_min = NA_real_, lat_min = NA_real_, lon_max = NA_real_, lat_max = NA_real_))
      lons <- as.numeric(c(m[2], m[4]))
      lats <- as.numeric(c(m[3], m[5]))
      c(lon_min = min(lons), lat_min = min(lats), lon_max = max(lons), lat_max = max(lats))
    }
    box_overlaps <- function(lon_min, lat_min, lon_max, lat_max,
                             ref_lon_min, ref_lat_min, ref_lon_max, ref_lat_max) {
      lon_min <= ref_lon_max & lon_max >= ref_lon_min &
        lat_min <= ref_lat_max & lat_max >= ref_lat_min
    }
    
    if (!"geographic_extent" %in% names(model_list)) {
      stop("model_list has no 'geographic_extent' column - check the printed",
           " column names above and adjust this section to match the actual field name.")
    }
    
    geo_parsed <- as.data.table(t(sapply(model_list$geographic_extent, parse_geo_box)))
    model_list <- cbind(model_list, geo_parsed)
    model_list[, in_bbox := box_overlaps(lon_min, lat_min, lon_max, lat_max,
                                         med_bbox["lon_min"], med_bbox["lat_min"],
                                         med_bbox["lon_max"], med_bbox["lat_max"])]
    
    bbox_models <- model_list[in_bbox == TRUE]
    message("\n", nrow(bbox_models), " of ", nrow(model_list),
            " models have a geographic_extent overlapping the requested bounding box",
            " (lon ", med_bbox["lon_min"], " to ", med_bbox["lon_max"],
            ", lat ", med_bbox["lat_min"], " to ", med_bbox["lat_max"], "):")
    print(bbox_models[, .SD, .SDcols = intersect(
      c("model_number", "model_name", "ecosystem_name", "country", "lon_min", "lat_min", "lon_max", "lat_max"),
      names(bbox_models))])
    
    fwrite(bbox_models, file.path(out_dir, "ecobase_mediterranean_models.csv"))
    message("\nSaved to ecobase_mediterranean_models.csv - review this list directly",
            " (a bounding-box overlap can still catch models that only brush the edge",
            " of the region, or miss ones with an unusually large/imprecise extent).")
    
    ## dissemination_allow: EcoBase's OWN flag for which models actually
    ## have downloadable input data (per their site, only ~233 of ~500
    ## listed models do; the rest are metadata/reference-only). Their own
    ## official R example filters on this BEFORE attempting to fetch
    ## anything - missing this filter is almost certainly why input
    ## fetching would otherwise come back empty for most attempted models.
    if (!"dissemination_allow" %in% names(bbox_models)) {
      message("\nWARNING: no 'dissemination_allow' column found - check the column names",
              " printed earlier and adjust the field name below if it's called something",
              " else. Proceeding WITHOUT this filter means every matching model will be",
              " attempted, including likely metadata-only ones - expect many failures.")
      bbox_models[, dissemination_allow := NA_character_]
    }
    
    n_disseminable <- bbox_models[tolower(dissemination_allow) == "true", .N]
    message("\nOf the ", nrow(bbox_models), " matching model(s): ", n_disseminable,
            " have dissemination_allow == 'true' (EcoBase's own flag for actually-",
            " downloadable data) and will actually be queried for input values below.")
    
    known_relevant_model_ids <- bbox_models[tolower(dissemination_allow) == "true", model_number]
    
    ## --- 3. Fetch input values (Biomass, PB, QB, ... per group) -----------
    fetch_model_inputs <- function(model_id) {
      url <- paste0(ECOBASE_INPUT_URL, "?no_model=", model_id)
      resp <- tryCatch(httr::GET(url, httr::timeout(timeout_sec)),
                       error = function(e) {
                         message("  Model ", model_id, ": request failed - ", conditionMessage(e))
                         NULL
                       })
      if (is.null(resp)) return(list(data = NULL, status = "request_failed"))
      if (httr::status_code(resp) != 200) {
        message("  Model ", model_id, ": HTTP request did not succeed (status ",
                httr::status_code(resp), ").")
        return(list(data = NULL, status = paste0("http_status_", httr::status_code(resp))))
      }
      
      xml_data <- tryCatch(xml2::read_xml(httr::content(resp, as = "text", encoding = "UTF-8")),
                           error = function(e) {
                             message("  Model ", model_id, ": response wasn't valid XML - ", conditionMessage(e))
                             NULL
                           })
      if (is.null(xml_data)) return(list(data = NULL, status = "invalid_xml"))
      
      ## EcoBase distinguishes "listed with metadata" from "data openly
      ## downloadable" - a model can be searchable/documented without its
      ## actual input data being public. Confirmed directly: model 418's
      ## raw response was exactly <EcoBaseModel><Description>Datas for
      ## this model are not available</Description></EcoBaseModel> -
      ## checked for explicitly here for a clear, distinct reason rather
      ## than a generic "unexpected structure" message. Kept as a safety
      ## net even with the dissemination_allow pre-filter above (in case
      ## that flag and this endpoint ever disagree for a given model).
      description_node <- xml2::xml_find_first(xml_data, ".//Description")
      if (!is.na(description_node) && grepl("not available", xml2::xml_text(description_node), ignore.case = TRUE)) {
        message("  Model ", model_id, ": EcoBase reports this model's data is NOT",
                " openly available for download (listed with metadata only) -",
                " not a parsing issue.")
        return(list(data = NULL, status = "not_available_despite_flag"))
      }
      
      group_nodes <- xml2::xml_find_all(xml_data, ".//group")
      if (length(group_nodes) == 0) {
        message("  Model ", model_id, ": no <group> nodes found - response structure",
                " may differ from what this function assumes. Raw response saved for inspection.")
        writeLines(httr::content(resp, as = "text", encoding = "UTF-8"),
                   file.path(out_dir, paste0("ecobase_raw_response_model_", model_id, ".xml")))
        return(list(data = NULL, status = "no_group_nodes_found"))
      }
      
      groups_dt <- rbindlist(lapply(group_nodes, xml_node_to_dt_row), fill = TRUE)
      groups_dt[, model_id := model_id]
      list(data = groups_dt, status = "success")
    }
    
    message("\nFetching input values (Biomass/PB/QB/... per group) for models with",
            " dissemination_allow == 'true'...")
    fetch_results <- lapply(known_relevant_model_ids, function(id) {
      message(" Model ", id, "...")
      fetch_model_inputs(id)
    })
    names(fetch_results) <- as.character(known_relevant_model_ids)
    
    fetched_status <- data.table(
      model_number = names(fetch_results),
      status = vapply(fetch_results, function(x) x$status, character(1))
    )
    
    bbox_models[, model_number := as.character(model_number)]
    skipped_status <- data.table(
      model_number = bbox_models[tolower(dissemination_allow) != "true" | is.na(dissemination_allow), model_number],
      status = "dissemination_not_allowed"
    )
    all_status <- rbindlist(list(fetched_status, skipped_status), use.names = TRUE)
    
    bbox_models_status <- merge(bbox_models, all_status, by = "model_number", all.x = TRUE)
    fwrite(bbox_models_status, file.path(out_dir, "ecobase_mediterranean_models_status.csv"))
    message("\nSaved ecobase_mediterranean_models_status.csv - every matching model with its",
            " outcome (success / dissemination_not_allowed / a specific fetch-failure reason).")
    
    models_without_data <- bbox_models_status[status != "success"]
    fwrite(models_without_data, file.path(out_dir, "ecobase_models_without_data.csv"))
    message("\nSaved ecobase_models_without_data.csv (", nrow(models_without_data), " of ",
            nrow(bbox_models_status), " matching models have NO usable data).")
    
    all_inputs <- rbindlist(lapply(fetch_results, `[[`, "data"), fill = TRUE)
    
    if (nrow(all_inputs) == 0) {
      message("\nNo input values were successfully retrieved - check the diagnostic",
              " messages above for each model to see what went wrong.")
      return(NULL)
    }
    
    message("\nColumns returned by EcoBase for each group (verify these actually contain",
            " group name/biomass/pb/qb - names may differ from what's assumed downstream):")
    print(names(all_inputs))
    
    ## Pull model-level metadata (name/year/authors/ecosystem/country) from
    ## model_list, matched on model_id. Column names below are best guesses
    ## at what the XML fields are actually called - print what's used so a
    ## wrong guess is visible.
    model_id_col   <- intersect(c("model_number", "model_id", "no_model", "id"), names(model_list))[1]
    model_name_col <- intersect(c("model_name", "name", "ecosystem_name"), names(model_list))[1]
    year_col       <- intersect(c("year", "period", "model_year", "year_start"), names(model_list))[1]
    author_col     <- intersect(c("author", "authors", "author_name"), names(model_list))[1]
    country_col    <- intersect(c("country"), names(model_list))[1]
    ecosystem_col  <- intersect(c("ecosystem_name"), names(model_list))[1]
    message("Model metadata columns used - id: ", model_id_col, " | name: ", model_name_col,
            " | year: ", year_col, " | authors: ", author_col)
    
    if (!is.na(model_id_col)) {
      meta <- model_list[, .(
        model_id      = get(model_id_col),
        EwE_model     = if (!is.na(model_name_col)) get(model_name_col) else NA_character_,
        year          = if (!is.na(year_col)) get(year_col) else NA_character_,
        authors       = if (!is.na(author_col)) get(author_col) else NA_character_,
        country       = if (!is.na(country_col)) get(country_col) else NA_character_,
        ecosystem_name = if (!is.na(ecosystem_col)) get(ecosystem_col) else NA_character_
      )]
      all_inputs[, model_id := as.character(model_id)]
      meta[, model_id := as.character(model_id)]
      all_inputs_with_meta <- merge(all_inputs, meta, by = "model_id", all.x = TRUE)
    } else {
      message("Could not match a model ID column in model_list - EwE_model/year/",
              "authors will be blank on the raw output.")
      all_inputs_with_meta <- all_inputs[, `:=`(EwE_model = NA_character_, year = NA_character_,
                                                authors = NA_character_, country = NA_character_,
                                                ecosystem_name = NA_character_)]
    }
    
    fwrite(all_inputs_with_meta, raw_meta_path)
    message("\nSaved ", raw_meta_path, " (", nrow(all_inputs_with_meta), " group x model rows,",
            " every raw EcoBase input column + model_id/EwE_model/year/authors/country/ecosystem_name).",
            " Both fetch_ecobase_literature_pb_qb() and fetch_ecobase_literature_biomass() read from",
            " this one cache - only refetched from EcoBase itself if force_refresh = TRUE or this file",
            " is deleted.")
    
    all_inputs_with_meta
    
  }, error = function(e) {
    message("fetch_ecobase_raw_inputs(): EcoBase query failed - ", conditionMessage(e),
            ". Continuing without EcoBase literature values.")
    NULL
  })
  
  invisible(result)
}

## fetch_ecobase_literature_pb_qb(out_dir, force_refresh = FALSE, target_year = NULL, ...)
##
## Public contract extended 2026-09-24 (still backward compatible when
## target_year is left NULL - see below): still writes
## ecobase_literature_pb_qb_simple.csv (FG_name, PB, QB, EwE_model, year,
## authors), called from 03_pbqb-traits.R. Internally a thin derivation
## off fetch_ecobase_raw_inputs()'s shared cache.
##
##   target_year - NULL (old behavior): every matching model's PB/QB for
##                 every FG_name is kept, one row each, no year
##                 preference at all (multiple rows per FG_name are
##                 possible, exactly as before this change).
##                 A number (e.g. round(mean(YEAR_ECOPATH))): for each
##                 FG_name, ONLY the single candidate whose model year is
##                 CLOSEST to target_year is kept in the "simple" summary
##                 - per Andrea's request that EcoBase "consider the year
##                 of the ecopath model" rather than treating every
##                 matching model as equally relevant regardless of how
##                 old/recent it is relative to this model's own base
##                 period. The FULL multi-model detail (every FG_name x
##                 model, with year_distance) is still written in full to
##                 ecobase_literature_pb_qb_full.csv either way, so
##                 nothing is lost - just not all blended into the
##                 "simple" file 03_pbqb-traits.R actually reads.
fetch_ecobase_literature_pb_qb <- function(out_dir, force_refresh = FALSE, target_year = NULL,
                                           med_bbox = WESTMED_BBOX,
                                           timeout_sec = 60) {
  simple_csv_path <- file.path(out_dir, "ecobase_literature_pb_qb_simple.csv")
  if (!force_refresh && file.exists(simple_csv_path)) {
    message("fetch_ecobase_literature_pb_qb(): using cached ", simple_csv_path,
            " (pass force_refresh = TRUE to re-query EcoBase instead).")
    return(invisible(as.data.table(fread(simple_csv_path))))
  }
  
  all_inputs_with_meta <- fetch_ecobase_raw_inputs(out_dir, force_refresh = force_refresh,
                                                   med_bbox = med_bbox, timeout_sec = timeout_sec)
  if (is.null(all_inputs_with_meta) || nrow(all_inputs_with_meta) == 0) return(invisible(NULL))
  
  if (!"group_name" %in% names(all_inputs_with_meta) ||
      !all(c("pb", "qb") %in% names(all_inputs_with_meta))) {
    message("fetch_ecobase_literature_pb_qb(): expected columns 'group_name'/'pb'/'qb' not both",
            " present in the raw EcoBase output (columns present: ",
            paste(names(all_inputs_with_meta), collapse = ", "), ") - can't build the PB/QB summary.")
    return(invisible(NULL))
  }
  
  ## -9999 is Ecopath's own sentinel for "not provided/unknown" (confirmed
  ## by cross-checking b_hab_area_input='false' correlating with a -9999
  ## biomass_habitat_area value in the raw output) - converted to NA here
  ## rather than left as a literal number that would otherwise silently
  ## look like a real (and wildly implausible) PB/QB value.
  pb_qb_full <- all_inputs_with_meta[, .(
    FG_name = group_name,
    PB = as.numeric(pb),
    QB = as.numeric(qb),
    EwE_model, year, authors, country, ecosystem_name, model_id
  )]
  pb_qb_full[PB == -9999, PB := NA_real_]
  pb_qb_full[QB == -9999, QB := NA_real_]
  pb_qb_full[, year_numeric := as.numeric(regmatches(year, regexpr("[0-9]{4}", year)))]
  
  fwrite(pb_qb_full, file.path(out_dir, "ecobase_literature_pb_qb_full.csv"))
  message("\nSaved ecobase_literature_pb_qb_full.csv (", nrow(pb_qb_full), " FG_name x model rows,",
          " every matching model - review this directly for anything the closest-year pick below drops).")
  
  if (is.null(target_year)) {
    pb_qb_final <- pb_qb_full[, .(FG_name, PB, QB, EwE_model, year, authors)]
  } else {
    pb_qb_full[, year_distance := abs(year_numeric - target_year)]
    with_year <- pb_qb_full[!is.na(year_distance)]
    without_year <- pb_qb_full[is.na(year_distance)]
    if (nrow(without_year) > 0) {
      message("[EcoBase PB/QB] ", nrow(without_year), " row(s) had no parseable model year - excluded from",
              " the closest-to-", target_year, " pick (still visible in ecobase_literature_pb_qb_full.csv).")
    }
    setorder(with_year, FG_name, year_distance)
    pb_qb_final <- with_year[, .SD[1], by = FG_name][, .(FG_name, PB, QB, EwE_model, year, authors, year_distance)]
    message("[EcoBase PB/QB] Closest-to-", target_year, " model picked per FG_name (", nrow(pb_qb_final),
            " FG_name(s), from ", nrow(pb_qb_full), " total candidate rows).")
  }
  
  fwrite(pb_qb_final, simple_csv_path)
  message("\nSaved final summary table to ecobase_literature_pb_qb_simple.csv",
          " (", nrow(pb_qb_final), " rows: FG_name, PB, QB, EwE_model, year, authors",
          if (!is.null(target_year)) ", year_distance" else "", ").")
  
  invisible(pb_qb_final)
}

## fetch_ecobase_literature_biomass(out_dir, force_refresh = FALSE,
##                                  target_fg_keywords = NULL, target_year = NULL, ...)
##
## NEW (2026-09-24, per Andrea: "it is more straightforward to pull that
## from the ecobase" than hand-copying figures out of a paper's
## supplementary tables). Same EcoBase source as the PB/QB fetch above,
## but for Biomass - meant specifically for FGs no survey here samples
## at all (macro-/meso-/microzooplankton, large/small phytoplankton,
## Posidonia/seagrass, other macroalgae, gorgonians and corals), called
## from 01_biomass.R.
##
## Behavior:
##   - Every Mediterranean-matching, dissemination-allowed EcoBase model's
##     Biomass per group is written in full to
##     ecobase_literature_biomass_full.csv (every group, every model -
##     for manual review/spot-checking, since model-to-model values can
##     vary a lot with area/habitat definitions).
##   - If target_fg_keywords is supplied (named list of character vectors,
##     same shape as MEGAFAUNA_TAXON_KEYWORDS/PRIMARY_PRODUCER_TAXON_KEYWORDS
##     in 01_biomass.R), group_name is keyword-matched (case-insensitive,
##     substring) against each target group, and for EACH target group the
##     single candidate row whose model 'year' is CLOSEST to target_year
##     (default 1995, the midpoint of a 1994-1996 base period) is picked -
##     written to ecobase_literature_biomass_best_by_group.csv, tagged with
##     which model/year/authors it came from and how far that year is from
##     target_year, so the "closest available, not a real measurement for
##     this exact year" caveat is explicit and traceable, never silent.
##     A target group with zero keyword matches across every fetched model
##     is reported (not silently dropped).
##   - Returns the "best by group" table invisibly (or NULL on failure) -
##     the caller decides what to do with it (01_biomass.R uses it to
##     auto-draft primary_producer_plankton_biomass.csv/
##     marine_megafauna_biomass.csv when that manual CSV doesn't exist yet).
##
## Biomass column name is a GUESS (see file header) - resolved
## defensively against BIOMASS_COL_CANDIDATES, with the columns actually
## found printed so a wrong guess is visible rather than silently wrong.
fetch_ecobase_literature_biomass <- function(out_dir, force_refresh = FALSE,
                                             target_fg_keywords = NULL, target_year = 1995,
                                             med_bbox = WESTMED_BBOX,
                                             timeout_sec = 60) {
  full_csv_path <- file.path(out_dir, "ecobase_literature_biomass_full.csv")
  best_csv_path <- file.path(out_dir, "ecobase_literature_biomass_best_by_group.csv")
  
  if (!force_refresh && file.exists(full_csv_path)) {
    message("fetch_ecobase_literature_biomass(): using cached ", full_csv_path,
            " (pass force_refresh = TRUE to re-query EcoBase instead).")
    biomass_full <- as.data.table(fread(full_csv_path))
  } else {
    all_inputs_with_meta <- fetch_ecobase_raw_inputs(out_dir, force_refresh = force_refresh,
                                                     med_bbox = med_bbox, timeout_sec = timeout_sec)
    if (is.null(all_inputs_with_meta) || nrow(all_inputs_with_meta) == 0) return(invisible(NULL))
    
    if (!"group_name" %in% names(all_inputs_with_meta)) {
      message("fetch_ecobase_literature_biomass(): no 'group_name' column in the raw EcoBase",
              " output (columns present: ", paste(names(all_inputs_with_meta), collapse = ", "), ").")
      return(invisible(NULL))
    }
    BIOMASS_COL_CANDIDATES <- c("biomass", "b", "biomass_area", "biomass_habitat_area", "B", "Biomass")
    col_bio <- intersect(BIOMASS_COL_CANDIDATES, names(all_inputs_with_meta))[1]
    if (is.na(col_bio)) {
      message("fetch_ecobase_literature_biomass(): none of the expected biomass column names (",
              paste(BIOMASS_COL_CANDIDATES, collapse = "/"), ") were found - columns actually present: ",
              paste(names(all_inputs_with_meta), collapse = ", "),
              ". Add the real column name to BIOMASS_COL_CANDIDATES above once you see it here.")
      return(invisible(NULL))
    }
    message("fetch_ecobase_literature_biomass(): using '", col_bio, "' as the biomass column",
            " (t/km2, Ecopath's native unit).")
    
    biomass_full <- all_inputs_with_meta[, .(
      FG_name = group_name,
      Biomass_t_km2 = as.numeric(get(col_bio)),
      EwE_model, year, authors, country, ecosystem_name, model_id
    )]
    biomass_full[Biomass_t_km2 == -9999, Biomass_t_km2 := NA_real_]  # Ecopath's own "not provided" sentinel
    biomass_full <- biomass_full[!is.na(Biomass_t_km2)]
    
    fwrite(biomass_full, full_csv_path)
    message("\nSaved ecobase_literature_biomass_full.csv (", nrow(biomass_full), " group x model rows,",
            " every Mediterranean-matching model with usable Biomass) - review this directly, values",
            " vary a lot model to model with area/habitat definitions.")
  }
  
  if (is.null(target_fg_keywords) || nrow(biomass_full) == 0) return(invisible(biomass_full))
  
  ## --- Keyword-match to target FG groups, pick closest-year candidate ---
  group_word_sets <- lapply(target_fg_keywords, tolower)
  matches <- rbindlist(lapply(names(group_word_sets), function(g) {
    hits <- biomass_full[sapply(tolower(FG_name), function(nm) any(sapply(group_word_sets[[g]], function(kw) grepl(kw, nm, fixed = TRUE))))]
    if (nrow(hits) == 0) return(NULL)
    hits[, TargetGroup := g]
    hits
  }), fill = TRUE)
  
  unmatched_targets <- setdiff(names(target_fg_keywords), if (is.null(matches)) character() else unique(matches$TargetGroup))
  if (length(unmatched_targets) > 0) {
    message("[EcoBase biomass] No EcoBase group name matched these target groups at all (checked ",
            nrow(biomass_full), " candidate rows): ", paste(unmatched_targets, collapse = ", "),
            " - these will need a different source (literature/manual entry).")
  }
  if (is.null(matches) || nrow(matches) == 0) return(invisible(biomass_full))
  
  ## year is text from EcoBase's XML (could be "1995", "1994-1996", blank,
  ## etc.) - extract the first 4-digit year found, best-effort, rather
  ## than assuming a clean integer.
  matches[, year_numeric := as.numeric(regmatches(year, regexpr("[0-9]{4}", year)))]
  matches[, year_distance := abs(year_numeric - target_year)]
  
  ## Closest year wins per target group; a tie keeps the first (arbitrary
  ## but deterministic) - genuinely ambiguous ties are rare enough here
  ## not to warrant a majority-vote-style mechanism like the FG-matching
  ## elsewhere in this pipeline.
  matches_with_year <- matches[!is.na(year_distance)]
  matches_without_year <- matches[is.na(year_distance)]
  if (nrow(matches_without_year) > 0) {
    message("[EcoBase biomass] ", nrow(matches_without_year), " matching row(s) had no parseable",
            " model year - excluded from the closest-year pick (still visible in",
            " ecobase_literature_biomass_full.csv).")
  }
  if (nrow(matches_with_year) == 0) {
    message("[EcoBase biomass] No matched candidate had a parseable model year - can't pick a",
            " closest-to-", target_year, " candidate.")
    return(invisible(biomass_full))
  }
  setorder(matches_with_year, TargetGroup, year_distance)
  best_by_group <- matches_with_year[, .SD[1], by = TargetGroup]
  best_by_group[, Source_citation := paste0("EcoBase model '", EwE_model, "' (", ecosystem_name, ", ", country,
                                            "), year ", year, " [", year_distance, " yr from target ", target_year,
                                            "] - ", authors, " (via EcoBase, model_id ", model_id, ")")]
  
  fwrite(best_by_group, best_csv_path)
  message("\n[EcoBase biomass] Closest-to-", target_year, " candidate picked for ", nrow(best_by_group),
          " of ", length(target_fg_keywords), " target group(s) - written to",
          " ecobase_literature_biomass_best_by_group.csv:")
  print(best_by_group[, .(TargetGroup, FG_name, Biomass_t_km2, EwE_model, year, year_distance)])
  
  invisible(best_by_group)
}

## build_ecobase_sheet_dt(out_dir, target_year = NULL)
##
## NEW (2026-09-24, per Andrea: "add a section where it adds a Ecobase
## sheet on the excel input file produced with data from the model
## biomass pbqb reference year etc"). Builds the data.table for a new
## "Ecobase" sheet in ecopath_ecosim_inputs.xlsx - a side-by-side
## comparison table of what OTHER published Western Med (or wider, if
## med_bbox was widened) Ecopath models report for Biomass/PB/QB per
## group, next to their own model name, ecosystem, country, reference
## year and authors - sitting alongside this model's own Ecopath_B/
## Ecopath_PBQB sheets for a direct sanity-check/comparison.
##
## Reads straight from ecobase_all_inputs_with_meta.csv (the shared raw
## cache written by fetch_ecobase_raw_inputs() - whichever of
## fetch_ecobase_literature_pb_qb()/fetch_ecobase_literature_biomass()
## ran first already populated it, so this needs no network call of its
## own). Returns NULL (with a message) if that cache doesn't exist yet -
## i.e. if neither EcoBase function has successfully run at all this
## pipeline - so the caller can skip adding the sheet rather than error.
##
## If target_year is supplied, adds a year_distance column (|model year -
## target_year|) and sorts by it (closest first) so the sheet itself
## reads with the most relevant comparison models at the top - it does
## NOT filter rows out, unlike the "best_by_group"/"closest-year-per-FG"
## reduction the two fetch functions do for their own single-value
## outputs; this sheet is meant as the full comparison reference.
build_ecobase_sheet_dt <- function(out_dir, target_year = NULL) {
  raw_meta_path <- file.path(out_dir, "ecobase_all_inputs_with_meta.csv")
  if (!file.exists(raw_meta_path)) {
    message("build_ecobase_sheet_dt(): '", raw_meta_path, "' not found - neither",
            " fetch_ecobase_literature_pb_qb() nor fetch_ecobase_literature_biomass() has produced",
            " it yet this run. Skipping the Ecobase workbook sheet.")
    return(invisible(NULL))
  }
  all_inputs_with_meta <- as.data.table(fread(raw_meta_path))
  if (nrow(all_inputs_with_meta) == 0) {
    message("build_ecobase_sheet_dt(): ", raw_meta_path, " is empty - skipping the Ecobase workbook sheet.")
    return(invisible(NULL))
  }
  
  BIOMASS_COL_CANDIDATES <- c("biomass", "b", "biomass_area", "biomass_habitat_area", "B", "Biomass")
  col_bio <- intersect(BIOMASS_COL_CANDIDATES, names(all_inputs_with_meta))[1]
  
  sheet_dt <- all_inputs_with_meta[, .(
    FG_name = if ("group_name" %in% names(all_inputs_with_meta)) group_name else NA_character_,
    Biomass_t_km2 = if (!is.na(col_bio)) as.numeric(get(col_bio)) else NA_real_,
    PB = if ("pb" %in% names(all_inputs_with_meta)) as.numeric(pb) else NA_real_,
    QB = if ("qb" %in% names(all_inputs_with_meta)) as.numeric(qb) else NA_real_,
    EwE_model, ecosystem_name, country, year, authors, model_id
  )]
  for (col in c("Biomass_t_km2", "PB", "QB")) sheet_dt[get(col) == -9999, (col) := NA_real_]
  
  if (!is.null(target_year)) {
    sheet_dt[, year_numeric := as.numeric(regmatches(year, regexpr("[0-9]{4}", year)))]
    sheet_dt[, year_distance := abs(year_numeric - target_year)]
    setorder(sheet_dt, year_distance, na.last = TRUE)
    sheet_dt[, year_numeric := NULL]
  }
  
  message("build_ecobase_sheet_dt(): ", nrow(sheet_dt), " row(s) (", uniqueN(sheet_dt$model_id),
          " model(s)) ready for the Ecobase workbook sheet.")
  invisible(sheet_dt)
}

## =================================================================
## fetch_ecobase_diet_matrix(out_dir, target_predator_keywords, ...)
##
## NEW (2026-09-24, added for 04_diets.R's diet-composition fallback -
## same three-source idea as fetch_ecobase_literature_biomass()/pb_qb(),
## but for a predator's DIET COMPOSITION rather than a single scalar
## trait). Reuses fetch_ecobase_raw_inputs()'s own model list/status
## CSVs (ecobase_mediterranean_models_status.csv) to know which models
## actually returned data, then re-fetches EACH such model's raw input
## XML directly (fetch_ecobase_raw_inputs()'s own per-group parsing,
## xml_node_to_dt_row(), FLATTENS every group node's children to plain
## text - fine for scalar fields like Biomass/PB/QB, but would silently
## mangle a NESTED diet-composition sub-structure, e.g. a repeated
## <prey><group>3</group><value>0.12</value></prey> list under each
## <group> node, into one useless concatenated text blob. This function
## instead re-parses each group node's own XML children directly,
## looking for anything diet-related.
##
## HONESTLY UNVERIFIED (no network access from this session, exactly
## like the rest of this file - see the file header): it is NOT
## confirmed that EcoBase's public webservice exposes a per-model diet
## matrix at all through this endpoint (ECOBASE_INPUT_URL). The official
## EcoBase SOAP example this file was adapted from only demonstrates
## group-level scalar inputs (Biomass/PB/QB/EE/...), not a diet matrix -
## a full diet composition might live in a different endpoint/field
## entirely, or might not be exposed publicly at all. DIET_NODE_NAME_
## CANDIDATES below is a best-effort guess at what such a field might be
## called if it exists (mirroring how BIOMASS_COL_CANDIDATES guesses at
## the biomass field name above) - the diagnostic prints below make a
## wrong/absent guess immediately visible rather than silently wrong,
## and this function returns NULL (with a clear message, not a
## fabricated result) if nothing diet-shaped is found for a model.
fetch_ecobase_diet_matrix_for_model <- function(model_id, timeout_sec = 60) {
  if (!requireNamespace("httr", quietly = TRUE) || !requireNamespace("xml2", quietly = TRUE)) return(NULL)
  ECOBASE_INPUT_URL <- "http://sirs.agrocampus-ouest.fr/EcoBase/php/webser/soap-client.php"
  url <- paste0(ECOBASE_INPUT_URL, "?no_model=", model_id)
  resp <- tryCatch(httr::GET(url, httr::timeout(timeout_sec)), error = function(e) NULL)
  if (is.null(resp) || httr::status_code(resp) != 200) return(NULL)
  xml_data <- tryCatch(xml2::read_xml(httr::content(resp, as = "text", encoding = "UTF-8")), error = function(e) NULL)
  if (is.null(xml_data)) return(NULL)
  
  group_nodes <- xml2::xml_find_all(xml_data, ".//group")
  if (length(group_nodes) == 0) return(NULL)
  
  ## Names a nested diet-composition child might plausibly carry -
  ## unverified guesses (see header above).
  DIET_NODE_NAME_CANDIDATES <- c("diet", "diet_composition", "dietcomp", "dc", "preys", "prey_list", "diet_matrix")
  
  diet_rows <- rbindlist(lapply(group_nodes, function(g) {
    pred_name_node <- xml2::xml_find_first(g, "./group_name")
    pred_name <- if (!is.na(pred_name_node)) xml2::xml_text(pred_name_node) else NA_character_
    children <- xml2::xml_children(g)
    child_names_lower <- tolower(xml2::xml_name(children))
    diet_child_idx <- which(child_names_lower %in% DIET_NODE_NAME_CANDIDATES)
    if (length(diet_child_idx) == 0) return(NULL)
    rbindlist(lapply(diet_child_idx, function(i) {
      diet_node <- children[[i]]
      prey_entries <- xml2::xml_children(diet_node)
      if (length(prey_entries) == 0) return(NULL)
      rbindlist(lapply(prey_entries, function(pe) {
        pe_children <- xml2::xml_children(pe)
        if (length(pe_children) > 0) {
          ## Structured <prey><group_name>X</group_name><value>Y</value></prey>-style entry.
          row <- xml_node_to_dt_row(pe)
          if (!"predator_group_name" %in% names(row)) row[, predator_group_name := pred_name]
          row
        } else {
          NULL
        }
      }), fill = TRUE)
    }), fill = TRUE)
  }), fill = TRUE)
  
  if (is.null(diet_rows) || nrow(diet_rows) == 0) return(NULL)
  diet_rows[, model_id := model_id]
  diet_rows[]
}

## fetch_ecobase_diet_by_keyword(out_dir, target_predator_keywords,
##                               force_refresh = FALSE, target_year = NULL, ...)
##
## Public entry point, mirroring fetch_ecobase_literature_biomass()'s
## contract: target_predator_keywords is a named list (name = this
## pipeline's own predator FG name, value = character vector of
## keywords), same shape as MEGAFAUNA_TAXON_KEYWORDS elsewhere. For each
## target predator FG, keyword-matches EcoBase's own predator_group_name
## (case-insensitive substring) across every model that (a) is in
## med_bbox/dissemination-allowed (reusing fetch_ecobase_raw_inputs()'s
## already-filtered model-status CSV, so no duplicate geographic query)
## and (b) actually returned a parseable diet sub-structure via
## fetch_ecobase_diet_matrix_for_model() above. Picks the closest-year
## model per target FG (same rule as the biomass/PB-QB fallbacks) and
## tags the result with Source_citation for provenance. Returns NULL
## (with a message) if EcoBase exposes no diet field at all for any
## matched model - the honest "not available via this endpoint" case -
## so 04_diets.R's caller can fall through to "still missing" rather
## than being handed a fabricated result.
fetch_ecobase_diet_by_keyword <- function(out_dir, target_predator_keywords, force_refresh = FALSE,
                                          target_year = NULL, med_bbox = WESTMED_BBOX, timeout_sec = 60) {
  ## Ensure the model list/status cache exists (reuses fetch_ecobase_
  ## raw_inputs()'s own geographic + dissemination_allow filtering -
  ## does NOT re-query the model list itself).
  status_path <- file.path(out_dir, "ecobase_mediterranean_models_status.csv")
  if (!file.exists(status_path) || force_refresh) {
    invisible(fetch_ecobase_raw_inputs(out_dir, force_refresh = force_refresh, med_bbox = med_bbox, timeout_sec = timeout_sec))
  }
  if (!file.exists(status_path)) {
    message("fetch_ecobase_diet_by_keyword(): ", status_path, " still not found after attempting ",
            "fetch_ecobase_raw_inputs() - can't determine which models to query for diet data.")
    return(invisible(NULL))
  }
  models_status <- as.data.table(fread(status_path))
  candidate_model_ids <- unique(models_status[status == "success", model_number])
  if (length(candidate_model_ids) == 0) {
    message("fetch_ecobase_diet_by_keyword(): no model in ", status_path, " has status == 'success' - nothing to query.")
    return(invisible(NULL))
  }
  
  message("fetch_ecobase_diet_by_keyword(): checking ", length(candidate_model_ids), " model(s) for a parseable ",
          "diet-composition sub-structure (UNVERIFIED whether EcoBase exposes this at all - see this function's own header comment)...")
  diet_all <- rbindlist(lapply(candidate_model_ids, function(id) {
    message("  Model ", id, "...")
    fetch_ecobase_diet_matrix_for_model(id, timeout_sec = timeout_sec)
  }), fill = TRUE)
  
  if (is.null(diet_all) || nrow(diet_all) == 0) {
    message("fetch_ecobase_diet_by_keyword(): NO model among the ", length(candidate_model_ids), " checked exposed ",
            "a diet-composition field under any of the guessed node names - EcoBase's public webservice does not ",
            "appear to expose a diet matrix through this endpoint (or the guessed field names are wrong; see the ",
            "DIET_NODE_NAME_CANDIDATES comment above). The EcoBase diet fallback is UNAVAILABLE this run.")
    return(invisible(NULL))
  }
  
  meta_path <- file.path(out_dir, "ecobase_all_inputs_with_meta.csv")
  meta <- if (file.exists(meta_path)) unique(as.data.table(fread(meta_path))[, .(model_id = as.character(model_id), EwE_model, year, authors, country, ecosystem_name)]) else NULL
  diet_all[, model_id := as.character(model_id)]
  if (!is.null(meta)) diet_all <- merge(diet_all, meta, by = "model_id", all.x = TRUE)
  
  fwrite(diet_all, file.path(out_dir, "ecobase_diet_matrix_full.csv"))
  message("fetch_ecobase_diet_by_keyword(): saved ecobase_diet_matrix_full.csv (", nrow(diet_all), " raw prey-entry row(s) ",
          "across ", uniqueN(diet_all$model_id), " model(s)) - review directly, this is unverified/experimental parsing.")
  
  if (is.null(target_predator_keywords)) return(invisible(diet_all))
  
  pred_name_col <- intersect(c("predator_group_name", "group_name"), names(diet_all))[1]
  if (is.na(pred_name_col)) {
    message("fetch_ecobase_diet_by_keyword(): parsed diet rows have no predator-name column to keyword-match against ",
            "(columns: ", paste(names(diet_all), collapse = ", "), ") - can't reduce to per-target-FG picks.")
    return(invisible(diet_all))
  }
  
  group_word_sets <- lapply(target_predator_keywords, tolower)
  matches <- rbindlist(lapply(names(group_word_sets), function(g) {
    hits <- diet_all[sapply(tolower(get(pred_name_col)), function(nm) any(sapply(group_word_sets[[g]], function(kw) grepl(kw, nm, fixed = TRUE))))]
    if (nrow(hits) == 0) return(NULL)
    hits[, TargetPredatorFG := g]
    hits
  }), fill = TRUE)
  
  unmatched <- setdiff(names(target_predator_keywords), if (is.null(matches)) character() else unique(matches$TargetPredatorFG))
  if (length(unmatched) > 0) {
    message("[EcoBase diet] No parsed diet row matched these target predator FG(s) by keyword: ",
            paste(unmatched, collapse = ", "), " - these need a different source.")
  }
  if (is.null(matches) || nrow(matches) == 0) return(invisible(diet_all))
  
  if (!is.null(target_year) && "year" %in% names(matches)) {
    matches[, year_numeric := as.numeric(regmatches(year, regexpr("[0-9]{4}", year)))]
    matches[, year_distance := abs(year_numeric - target_year)]
    setorder(matches, TargetPredatorFG, year_distance, na.last = TRUE)
    best_model_by_fg <- matches[, .SD[1], by = .(TargetPredatorFG)][, .(TargetPredatorFG, model_id)]
    matches <- merge(matches, best_model_by_fg, by = c("TargetPredatorFG", "model_id"))
  }
  if (all(c("EwE_model", "ecosystem_name", "country", "year", "authors") %in% names(matches))) {
    matches[, Source_citation := paste0("EcoBase model '", EwE_model, "' (", ecosystem_name, ", ", country, "), year ", year,
                                        " - ", authors, " (via EcoBase, model_id ", model_id, ")")]
  }
  
  fwrite(matches, file.path(out_dir, "ecobase_diet_matrix_best_by_predator_fg.csv"))
  message("[EcoBase diet] ", uniqueN(matches$TargetPredatorFG), " of ", length(target_predator_keywords),
          " target predator FG(s) matched to an EcoBase model diet - written to ecobase_diet_matrix_best_by_predator_fg.csv.")
  invisible(matches)
}