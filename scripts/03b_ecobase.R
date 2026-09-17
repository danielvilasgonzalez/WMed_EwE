## =================================================================
## 03b_ecobase.R
##
## Library file - no top-level driver code, same convention as
## lib_survey_fg_density_functions.R. Sourced automatically from
## 03_pbqb-traits.R and called directly from there
## (fetch_ecobase_literature_pb_qb()) - it is not meant to be run as
## its own standalone numbered pipeline step.
##
## Compiles PB/QB/Biomass values from EXISTING PUBLISHED ECOPATH MODELS
## (via EcoBase), as a literature-derived alternative/supplement to the
## empirical-formula PB/QB estimates computed earlier in
## 03_pbqb-traits.R.
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
## it. If the SOAP endpoint has changed or moved since EcoBase's own
## documentation was last updated, this may need adjustment - the
## diagnostics below are there specifically to catch and surface that
## rather than fail silently. Because this now runs automatically as
## part of 03_pbqb-traits.R rather than as its own manually-run script,
## the ENTIRE fetch is wrapped in one outer tryCatch() (see the bottom
## of fetch_ecobase_literature_pb_qb()) - any failure here (network
## down, endpoint moved, unexpected response shape) degrades to a
## clear message and a NULL return, never a stop() that would take
## down the whole 03_pbqb-traits.R run over an optional literature
## comparison step.
## =================================================================

## fetch_ecobase_literature_pb_qb(out_dir, force_refresh = FALSE, ...)
##
## Queries EcoBase for every published model geographically overlapping
## the Mediterranean, fetches per-group Biomass/PB/QB for the ones
## flagged as actually downloadable (dissemination_allow == "true"),
## and writes:
##   ecobase_model_list_full.csv            - every model EcoBase lists
##   ecobase_mediterranean_models.csv       - geographically-matching subset
##   ecobase_mediterranean_models_status.csv - + fetch outcome per model
##   ecobase_models_without_data.csv        - matching models with NO usable data, + why
##   ecobase_literature_pb_qb_raw.csv       - raw per-group Biomass/PB/QB, every fetched model
##   ecobase_literature_pb_qb_simple.csv    - the file 03_pbqb-traits.R actually reads:
##                                             FG_name, PB, QB, EwE_model, year, authors
## into out_dir, and returns the final `data.table` (same content as
## ecobase_literature_pb_qb_simple.csv) invisibly - or NULL if the fetch
## failed or genuinely returned nothing usable.
##
##   out_dir        - where to write the CSVs above (same out_dir the
##                     rest of the pipeline uses)
##   force_refresh  - FALSE (default): if ecobase_literature_pb_qb_simple.csv
##                     already exists in out_dir, skip the network call
##                     entirely and just read/return that cached file -
##                     same caching philosophy as strata_area_by_area.csv
##                     elsewhere in this pipeline, and keeps re-running
##                     03_pbqb-traits.R fast/offline-friendly once this
##                     has succeeded once. TRUE forces a fresh query even
##                     if a cached file is present.
##   med_bbox       - named vector c(lon_min, lat_min, lon_max, lat_max),
##                     default the FULL Mediterranean (Gibraltar to the
##                     Levant) - narrow it (e.g. lon -6 to 12, lat 35 to
##                     44 for Western Med only) to exclude literature
##                     models from elsewhere in the basin.
##   timeout_sec    - per-request HTTP timeout, default 60.
fetch_ecobase_literature_pb_qb <- function(out_dir, force_refresh = FALSE,
                                           med_bbox = c(lon_min = -6, lat_min = 30,
                                                       lon_max = 36, lat_max = 46),
                                           timeout_sec = 60) {
  simple_csv_path <- file.path(out_dir, "ecobase_literature_pb_qb_simple.csv")

  ## Checked BEFORE the httr/xml2 package check below - reading an
  ## already-cached CSV needs neither package, so a machine without them
  ## installed can still reuse a cache produced elsewhere.
  if (!force_refresh && file.exists(simple_csv_path)) {
    message("fetch_ecobase_literature_pb_qb(): using cached ", simple_csv_path,
            " (pass force_refresh = TRUE to re-query EcoBase instead).")
    return(invisible(as.data.table(fread(simple_csv_path))))
  }

  if (!requireNamespace("httr", quietly = TRUE) || !requireNamespace("xml2", quietly = TRUE)) {
    message("fetch_ecobase_literature_pb_qb(): 'httr' and 'xml2' packages are required -",
            " skipping the EcoBase literature query. Install them to enable this step.")
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

    ## --- 2. Filter to models geographically in the Mediterranean --------
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
    model_list[, in_mediterranean := box_overlaps(lon_min, lat_min, lon_max, lat_max,
                                                  med_bbox["lon_min"], med_bbox["lat_min"],
                                                  med_bbox["lon_max"], med_bbox["lat_max"])]

    mediterranean_models <- model_list[in_mediterranean == TRUE]
    message("\n", nrow(mediterranean_models), " of ", nrow(model_list),
            " models have a geographic_extent overlapping the Mediterranean bounding box",
            " (lon ", med_bbox["lon_min"], " to ", med_bbox["lon_max"],
            ", lat ", med_bbox["lat_min"], " to ", med_bbox["lat_max"], "):")
    print(mediterranean_models[, .SD, .SDcols = intersect(
      c("model_number", "model_name", "ecosystem_name", "country", "lon_min", "lat_min", "lon_max", "lat_max"),
      names(mediterranean_models))])

    fwrite(mediterranean_models, file.path(out_dir, "ecobase_mediterranean_models.csv"))
    message("\nSaved to ecobase_mediterranean_models.csv - review this list directly",
            " (a bounding-box overlap can still catch models that only brush the edge",
            " of the Mediterranean, or miss ones with an unusually large/imprecise",
            " extent).")

    ## dissemination_allow: EcoBase's OWN flag for which models actually
    ## have downloadable input data (per their site, only ~233 of ~500
    ## listed models do; the rest are metadata/reference-only). Their own
    ## official R example filters on this BEFORE attempting to fetch
    ## anything - missing this filter is almost certainly why input
    ## fetching would otherwise come back empty for most attempted models.
    if (!"dissemination_allow" %in% names(mediterranean_models)) {
      message("\nWARNING: no 'dissemination_allow' column found - check the column names",
              " printed earlier and adjust the field name below if it's called something",
              " else. Proceeding WITHOUT this filter means every Mediterranean model will",
              " be attempted, including likely metadata-only ones - expect many failures.")
      mediterranean_models[, dissemination_allow := NA_character_]
    }

    n_disseminable <- mediterranean_models[tolower(dissemination_allow) == "true", .N]
    message("\nOf the ", nrow(mediterranean_models), " Mediterranean model(s): ", n_disseminable,
            " have dissemination_allow == 'true' (EcoBase's own flag for actually-",
            " downloadable data) and will actually be queried for input values below.")

    known_relevant_model_ids <- mediterranean_models[tolower(dissemination_allow) == "true", model_number]

    ## --- 3. Fetch input values (Biomass, PB, QB per group) for those models
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

    message("\nFetching input values (Biomass/PB/QB per group) for models with",
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

    mediterranean_models[, model_number := as.character(model_number)]
    skipped_status <- data.table(
      model_number = mediterranean_models[tolower(dissemination_allow) != "true" | is.na(dissemination_allow), model_number],
      status = "dissemination_not_allowed"
    )
    all_status <- rbindlist(list(fetched_status, skipped_status), use.names = TRUE)

    mediterranean_models_status <- merge(mediterranean_models, all_status, by = "model_number", all.x = TRUE)
    fwrite(mediterranean_models_status, file.path(out_dir, "ecobase_mediterranean_models_status.csv"))
    message("\nSaved ecobase_mediterranean_models_status.csv - every Mediterranean-",
            " matching model with its outcome (success / dissemination_not_allowed /",
            " a specific fetch-failure reason).")

    models_without_data <- mediterranean_models_status[status != "success"]
    fwrite(models_without_data, file.path(out_dir, "ecobase_models_without_data.csv"))
    message("\nSaved ecobase_models_without_data.csv (", nrow(models_without_data), " of ",
            nrow(mediterranean_models_status), " Mediterranean-matching models have NO usable",
            " data).")

    all_inputs <- rbindlist(lapply(fetch_results, `[[`, "data"), fill = TRUE)

    if (nrow(all_inputs) == 0) {
      message("\nNo input values were successfully retrieved - check the diagnostic",
              " messages above for each model to see what went wrong.")
      return(NULL)
    }

    message("\nColumns returned (verify these actually contain group name/Biomass/PB/QB -",
            " names may differ from what's assumed here):")
    print(names(all_inputs))
    fwrite(all_inputs, file.path(out_dir, "ecobase_literature_pb_qb_raw.csv"))
    message("\nSaved raw compiled values to ecobase_literature_pb_qb_raw.csv.")

    ## -9999 is Ecopath's own sentinel for "not provided/unknown"
    ## (confirmed by cross-checking b_hab_area_input='false' correlating
    ## with a -9999 biomass_habitat_area value in the raw output) -
    ## converted to NA here rather than left as a literal number that
    ## would otherwise silently look like a real (and wildly implausible)
    ## PB/QB value.
    pb_qb_simple <- all_inputs[, .(
      model_id,
      FG_name = group_name,
      PB = as.numeric(pb),
      QB = as.numeric(qb)
    )]
    pb_qb_simple[PB == -9999, PB := NA_real_]
    pb_qb_simple[QB == -9999, QB := NA_real_]

    ## Pull model-level metadata (name/year/authors) from model_list,
    ## matched on model_id. Column names below are best guesses at what
    ## the XML fields are actually called.
    model_id_col   <- intersect(c("model_number", "model_id", "no_model", "id"), names(model_list))[1]
    model_name_col <- intersect(c("model_name", "name", "ecosystem_name"), names(model_list))[1]
    year_col       <- intersect(c("year", "period", "model_year", "year_start"), names(model_list))[1]
    author_col     <- intersect(c("author", "authors", "author_name"), names(model_list))[1]

    if (!is.na(model_id_col)) {
      meta <- model_list[, .(
        model_id = get(model_id_col),
        EwE_model = if (!is.na(model_name_col)) get(model_name_col) else NA_character_,
        year      = if (!is.na(year_col)) get(year_col) else NA_character_,
        authors   = if (!is.na(author_col)) get(author_col) else NA_character_
      )]
      pb_qb_simple[, model_id := as.character(model_id)]
      meta[, model_id := as.character(model_id)]
      pb_qb_final <- merge(pb_qb_simple, meta, by = "model_id", all.x = TRUE)
    } else {
      message("Could not match a model ID column in model_list - EwE_model/year/",
              "authors will be blank.")
      pb_qb_final <- pb_qb_simple[, `:=`(EwE_model = NA_character_, year = NA_character_, authors = NA_character_)]
    }

    pb_qb_final <- pb_qb_final[, .(FG_name, PB, QB, EwE_model, year, authors)]

    fwrite(pb_qb_final, file.path(out_dir, "ecobase_literature_pb_qb_simple.csv"))
    message("\nSaved final summary table to ecobase_literature_pb_qb_simple.csv",
            " (", nrow(pb_qb_final), " rows: FG_name, PB, QB, EwE_model, year, authors).")

    pb_qb_final

  }, error = function(e) {
    message("fetch_ecobase_literature_pb_qb(): EcoBase query failed - ", conditionMessage(e),
            ". Continuing 03_pbqb-traits.R without EcoBase literature values",
            " (the empirical PB/QB estimates are unaffected).")
    NULL
  })

  invisible(result)
}
