## =================================================================
## Compile PB/QB/Biomass values from EXISTING PUBLISHED ECOPATH
## MODELS (via EcoBase), as a literature-derived alternative/
## supplement to the empirical-formula estimates in
## calculate_pb_qb_fg.R.
##
## EcoBase (https://ecobase.ecopath.org/) is an open-access repository
## of published Ecopath models, queryable via a SOAP/XML web service
## documented at https://ecobase.ecopath.org/ (see "script to play
## with EcoBase"). This adapts their official example from RCurl/XML
## to httr/xml2 (more modern, better maintained).
##
## NOT TESTED against the live service from this session - I don't
## have network access to sirs.agrocampus-ouest.fr or ecobase.ecopath.org
## from my own sandbox, so this needs to be verified by actually
## running it. If the SOAP endpoint has changed or moved since EcoBase's
## own documentation was last updated, this may need adjustment - the
## diagnostics below are there specifically to catch and surface that
## rather than fail silently.
## =================================================================

library(httr)
library(xml2)
library(data.table)

#output folder
if (tolower(Sys.info()[["user"]]) == "daniel") {
  out_dir <- "/Users/daniel/Work/iMARES/WMed EwE Model/data/processed/"
  plot_dir <- paste0(dirname(dirname(out_dir)),'/plots')
  if (!dir.exists(plot_dir)) {
    dir.create(plot_dir)
  }
} else {
  if (!requireNamespace("rstudioapi", quietly = TRUE) ||
      !rstudioapi::isAvailable()) {
    stop(
      "This script requires RStudio. Please select the output directory manually."
    )
  }
  rstudioapi::showQuestion(
    title = "Select Output Directory",
    message = paste(
      "Please select the directory where output files",
      "and intermediate results will be saved."
    )
  )
  out_dir <- rstudioapi::selectDirectory()
  if (is.null(out_dir) || out_dir == "" || !dir.exists(out_dir)) {
    stop("No valid output directory selected.")
  }
  plot_dir <- paste0(dirname(dirname(out_dir)),'/plots')
  if (!dir.exists(plot_dir)) {
    dir.create(plot_dir)
  }
}

#ecobase info
ECOBASE_LIST_URL   <- "http://sirs.agrocampus-ouest.fr/EcoBase/php/webser/soap-client_3.php"
ECOBASE_INPUT_URL  <- "http://sirs.agrocampus-ouest.fr/EcoBase/php/webser/soap-client.php"
ECOBASE_OUTPUT_URL <- "http://sirs.agrocampus-ouest.fr/EcoBase/php/webser/soap-client_output.php"

## --- 1. Get the full list of available models --------------------------

message("Fetching EcoBase model list...")
resp <- tryCatch(httr::GET(ECOBASE_LIST_URL, httr::timeout(60)),
                 error = function(e) {
                   message("Failed to reach EcoBase model list endpoint: ", conditionMessage(e))
                   NULL
                 })

if (is.null(resp) || httr::status_code(resp) != 200) {
  stop("Could not fetch the EcoBase model list (check the URL still works in a",
       " browser, and that ", ECOBASE_LIST_URL, " hasn't moved).")
}

xml_content <- xml2::read_xml(httr::content(resp, as = "text", encoding = "UTF-8"))

## The exact node structure isn't confirmed against a live response -
## print the raw top-level structure first so you can see what's
## actually there and adjust the xml_find_all() path below if needed
message("\nTop-level XML structure (inspect this if parsing below fails):")
xml2::xml_structure(xml_content, indent = 2)

model_nodes <- xml2::xml_find_all(xml_content, ".//model")
message("\nFound ", length(model_nodes), " model node(s) in the response.")

if (length(model_nodes) == 0) {
  message("No <model> nodes found with that path - check the printed structure",
          " above and adjust the xml_find_all() XPath to match what's actually there.")
}

model_list <- rbindlist(lapply(model_nodes, function(node) {
  fields <- xml2::xml_children(node)
  vals <- setNames(as.list(xml2::xml_text(fields)), xml2::xml_name(fields))
  as.data.table(vals)
}), fill = TRUE)

message("\nParsed model list - columns available:")
print(names(model_list))
message("\nFirst few rows:")
print(head(model_list))

fwrite(model_list, paste0(out_dir,"ecobase_model_list_full.csv"))
message("\nSaved full model list to ecobase_model_list_full.csv")

## --- 2. Filter to models geographically in the Mediterranean -------------
## Uses geographic_extent (format: "BOX(lon1 lat1,lon2 lat2)") rather than
## keyword-matching text fields - more robust, since it doesn't depend on
## how a model's country/ecosystem name happens to be written. Verified
## the parsing + overlap logic against real sample geographic_extent
## values before relying on it here (an Aegean Sea entry correctly
## flagged TRUE; Alaska/PNG/Bering/Antarctica/Florida entries correctly
## flagged FALSE).
##
## Bounding box covers the FULL Mediterranean (Gibraltar to the Levant) -
## narrow MED_LON_MIN/MED_LAT_MIN/etc. below if you want Western Med only
## (e.g. lon -6 to 12, lat 35 to 44 would restrict to roughly Gibraltar
## through the Sicily Channel).

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

MED_LON_MIN <- -6; MED_LON_MAX <- 36
MED_LAT_MIN <- 30; MED_LAT_MAX <- 46

if (!"geographic_extent" %in% names(model_list)) {
  stop("model_list has no 'geographic_extent' column - check the printed",
       " column names above and adjust this section to match the actual field name.")
}

geo_parsed <- as.data.table(t(sapply(model_list$geographic_extent, parse_geo_box)))
model_list <- cbind(model_list, geo_parsed)
model_list[, in_mediterranean := box_overlaps(lon_min, lat_min, lon_max, lat_max,
                                              MED_LON_MIN, MED_LAT_MIN, MED_LON_MAX, MED_LAT_MAX)]

mediterranean_models <- model_list[in_mediterranean == TRUE]
message("\n", nrow(mediterranean_models), " of ", nrow(model_list),
        " models have a geographic_extent overlapping the Mediterranean bounding box",
        " (lon ", MED_LON_MIN, " to ", MED_LON_MAX, ", lat ", MED_LAT_MIN, " to ", MED_LAT_MAX, "):")
print(mediterranean_models[, .SD, .SDcols = intersect(
  c("model_number", "model_name", "ecosystem_name", "country", "lon_min", "lat_min", "lon_max", "lat_max"),
  names(mediterranean_models))])

fwrite(mediterranean_models, paste0(out_dir,"ecobase_mediterranean_models.csv"))
message("\nSaved to ecobase_mediterranean_models.csv - review this list directly",
        " (a bounding-box overlap can still catch models that only brush the edge",
        " of the Mediterranean, or miss ones with an unusually large/imprecise",
        " extent) and pick the model ID column to build KNOWN_RELEVANT_MODEL_IDS below.")

## Still worth keeping explicit, high-confidence IDs found by directly
## browsing the site (bounding-box filtering catches broad geographic
## overlap, but doesn't tell you which specific Western Med models are
## the most directly relevant, e.g. covering your specific GSAs, or
## that models 766/767 may be your own published work)
KNOWN_RELEVANT_MODEL_IDS <- c(mediterranean_models$model_number)
print(KNOWN_RELEVANT_MODEL_IDS)

## --- 3. Fetch input values (Biomass, PB, QB per group) for specific models

fetch_model_inputs <- function(model_id) {
  url <- paste0(ECOBASE_INPUT_URL, "?no_model=", model_id)
  resp <- tryCatch(httr::GET(url, httr::timeout(60)),
                   error = function(e) {
                     message("  Model ", model_id, ": request failed - ", conditionMessage(e))
                     NULL
                   })
  if (is.null(resp) || httr::status_code(resp) != 200) {
    message("  Model ", model_id, ": HTTP request did not succeed.")
    return(NULL)
  }
  
  xml_data <- tryCatch(xml2::read_xml(httr::content(resp, as = "text", encoding = "UTF-8")),
                       error = function(e) {
                         message("  Model ", model_id, ": response wasn't valid XML - ", conditionMessage(e))
                         NULL
                       })
  if (is.null(xml_data)) return(NULL)
  
  ## EcoBase distinguishes "listed with metadata" from "data openly
  ## downloadable" (per their own site: 233 available for download out
  ## of 500 with metadata) - a model can be searchable/documented without
  ## its actual input data being public. Confirmed directly: model 418's
  ## raw response was exactly <EcoBaseModel><Description>Datas for this
  ## model are not available</Description></EcoBaseModel> - checked for
  ## explicitly here so this shows as a clear, distinct reason rather
  ## than the generic "unexpected structure" message.
  description_node <- xml2::xml_find_first(xml_data, ".//Description")
  if (!is.na(description_node) && grepl("not available", xml2::xml_text(description_node), ignore.case = TRUE)) {
    message("  Model ", model_id, ": EcoBase reports this model's data is NOT",
            " openly available for download (listed with metadata only) -",
            " not a parsing issue. Consider contacting the model's authors",
            " directly, or checking the original publication for a",
            " supplementary parameter table.")
    return(NULL)
  }
  
  group_nodes <- xml2::xml_find_all(xml_data, ".//group")
  if (length(group_nodes) == 0) {
    message("  Model ", model_id, ": no <group> nodes found - response structure",
            " may differ from what this script assumes. Raw response saved for inspection.")
    writeLines(httr::content(resp, as = "text", encoding = "UTF-8"),
               paste0("ecobase_raw_response_model_", model_id, ".xml"))
    return(NULL)
  }
  
  groups_dt <- rbindlist(lapply(group_nodes, function(node) {
    fields <- xml2::xml_children(node)
    vals <- setNames(as.list(xml2::xml_text(fields)), xml2::xml_name(fields))
    as.data.table(vals)
  }), fill = TRUE)
  groups_dt[, model_id := model_id]
  groups_dt
}

message("\nFetching input values (Biomass/PB/QB per group) for known relevant models...")
all_inputs <- rbindlist(lapply(KNOWN_RELEVANT_MODEL_IDS, function(id) {
  message(" Model ", id, "...")
  fetch_model_inputs(id)
}), fill = TRUE)

if (nrow(all_inputs) > 0) {
  message("\nColumns returned (verify these actually contain group name/Biomass/PB/QB",
          " - names may differ from what's assumed here):")
  print(names(all_inputs))
  fwrite(all_inputs, paste0(out_dir,"ecobase_literature_pb_qb_raw.csv"))
  message("\nSaved raw compiled values to ecobase_literature_pb_qb_raw.csv -",
          " inspect this directly, then map its group names to your own",
          " fg_lookup FG_num/FG_name by hand (species/group naming won't match",
          " automatically across different models' own group definitions).")
  
  ## --- Final summary table: FG_name, PB, QB, EwE_model, year, authors ---
  ## -9999 is Ecopath's own sentinel for "not provided/unknown" (confirmed
  ## by cross-checking b_hab_area_input='false' correlating with a -9999
  ## biomass_habitat_area value in the raw output) - converted to NA here
  ## rather than left as a literal number that would otherwise silently
  ## look like a real (and wildly implausible) PB/QB value.
  pb_qb_simple <- all_inputs[, .(
    model_id,
    FG_name = group_name,
    PB = as.numeric(pb),
    QB = as.numeric(qb)
  )]
  pb_qb_simple[PB == -9999, PB := NA_real_]
  pb_qb_simple[QB == -9999, QB := NA_real_]
  
  ## Pull model-level metadata (name/year/authors) from model_list, matched
  ## on model_id. Column names below are best guesses at what the XML
  ## fields are actually called - NOT yet confirmed against a real
  ## model_list printout the way group_name/pb/qb were. If any of these
  ## come back all-NA after merging, check names(model_list) (printed
  ## earlier in this script) for the real field names and fix the
  ## candidate list below.
  model_id_col   <- intersect(c("model_number", "model_id", "no_model", "id"), names(model_list))[1]
  model_name_col <- intersect(c("model_name", "name", "ecosystem_name"), names(model_list))[1]
  year_col       <- intersect(c("year", "period", "model_year", "year_start"), names(model_list))[1]
  author_col     <- intersect(c("author", "authors", "author_name"), names(model_list))[1]
  
  message("\nMetadata columns matched in model_list (NA below means no candidate",
          " name matched - check names(model_list) printed earlier and adjust):")
  message("  model ID column: ", if (is.na(model_id_col)) "NOT FOUND" else model_id_col)
  message("  model name column: ", if (is.na(model_name_col)) "NOT FOUND" else model_name_col)
  message("  year column: ", if (is.na(year_col)) "NOT FOUND" else year_col)
  message("  author column: ", if (is.na(author_col)) "NOT FOUND" else author_col)
  
  if (!is.na(model_id_col)) {
    meta <- model_list[, .(
      model_id = get(model_id_col),
      EwE_model = if (!is.na(model_name_col)) get(model_name_col) else NA_character_,
      year      = if (!is.na(year_col)) get(year_col) else NA_character_,
      authors   = if (!is.na(author_col)) get(author_col) else NA_character_
    )]
    ## model_id types may not match (character from XML vs whatever type
    ## model_id_col is) - force both sides to character before merging
    pb_qb_simple[, model_id := as.character(model_id)]
    meta[, model_id := as.character(model_id)]
    pb_qb_final <- merge(pb_qb_simple, meta, by = "model_id", all.x = TRUE)
  } else {
    message("Could not match a model ID column in model_list - EwE_model/year/",
            "authors will be blank. Add the real column name to model_id_col above.")
    pb_qb_final <- pb_qb_simple[, `:=`(EwE_model = NA_character_, year = NA_character_, authors = NA_character_)]
  }
  
  pb_qb_final <- pb_qb_final[, .(FG_name, PB, QB, EwE_model, year, authors)]
  
  fwrite(pb_qb_final, paste0(out_dir,"ecobase_literature_pb_qb_simple.csv"))
  message("\nSaved final summary table to ecobase_literature_pb_qb_simple.csv",
          " (", nrow(pb_qb_final), " rows: FG_name, PB, QB, EwE_model, year, authors).")
  print(pb_qb_final)
} else {
  message("\nNo input values were successfully retrieved - check the diagnostic",
          " messages above for each model to see what went wrong.")
}