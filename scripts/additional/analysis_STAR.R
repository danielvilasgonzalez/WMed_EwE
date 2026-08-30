## =================================================================
## GFCM STAR data pipeline
## Fetch -> parse (Power BI DSR decompression) -> clean -> verify -> save
##
## Produces two output files:
##   star_data_clean.csv  - full 74-column cleaned table
##   star_data_tidy.csv   - standardized subset, ready to merge with
##                           RAM Legacy (source, species, gsa, year,
##                           biomass, landings, subregion)
## =================================================================

pkgs <- c("httr2", "jsonlite", "dplyr", "stringr", "readr")
new_pkgs <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new_pkgs) > 0) install.packages(new_pkgs)
invisible(lapply(pkgs, library, character.only = TRUE))

if (!dir.exists("star_raw")) dir.create("star_raw")

## -----------------------------------------------------------------
## 1. Query parameters
## Discovered by intercepting the Power BI "publish to web" report's
## own network traffic (see star_network_capture.R / star_capture_
## querydata.R for how these were found - only needs re-running if
## GFCM republishes the report under a new resourceKey/dataset).
## -----------------------------------------------------------------

RESOURCE_KEY <- "ded7718c-fa0e-4728-9cf4-64224e17c572"
DATASET_ID   <- "9289272e-4b90-45d6-8b8c-412447802d94"
REPORT_ID    <- "cafd8728-bf05-4375-84e0-fa6717dd3a40"
MODEL_ID     <- 1136218
ENDPOINT     <- "https://wabi-north-europe-p-primary-api.analysis.windows.net/public/reports/querydata?synchronous=true"

STAR_COLUMNS <- c(
  "fileid","Assessment_ID","Species","Species_Common_Name","GSA","Reference_Year",
  "Reporting_Year","Assessment_Method","Expert_Group","Status_Fref","Status_Btarget",
  "Status_Bthreshold","Status_Blimit","Status_F_Ftarget","Status_Text_B","Status_Text_E",
  "Scientific_Advice","WG_Comments","Stock Coordinator","VPA_Model","Forecast_Included",
  "Fmsy","F","F0.1","Fmax","F40","E0.4","Bmsy","Bpa","Blim","Current_F","Current_B",
  "B40","Bloss","SSBMSY","SPR_fmax","Type_Confint","Recruitment_Unit","Recruitment_Age",
  "Recruitment_Length","Stock1_Indicator","Stock1_Unit","Stock2_Indicator","Stock2_Unit",
  "Catches_Unit","Exploitation_Unit","Fishing_Pressure_Type","Effort_Unit",
  "Fbar_First_Age","Fbar_Last_Age","Fbar_First_Length","Fbar_Last_Length",
  "Advice_Refpts","Advice_Levels","Advice_Quant_Status","F-Ftarget","F-Fref",
  "B-Btarget","B-Bthreshold","B-Blimit","Percentage of F reduction",
  "Status_of_the_Stock","Advice_Stock_Status","Countries_Inferred","GSA_Names",
  "sourcestar","Template_Version","3AlphaCode","ValidationStatus","Catches",
  "Landings","FileURL","Reference year v2","Reporting year v2"
)

## -----------------------------------------------------------------
## 2. Fetch the full table from the live Power BI endpoint
## -----------------------------------------------------------------

fetch_star_raw <- function(out_path = "star_raw/full_query_response.json") {
  select_list <- lapply(seq_along(STAR_COLUMNS), function(i) {
    list(
      Column = list(Expression = list(SourceRef = list(Source = "v")), Property = STAR_COLUMNS[i]),
      Name = paste0("View_STAR_Metadata.", STAR_COLUMNS[i])
    )
  })
  projections <- as.list(seq(0, length(STAR_COLUMNS) - 1))
  
  body <- list(
    version = "1.0.0",
    queries = list(list(
      Query = list(Commands = list(list(SemanticQueryDataShapeCommand = list(
        Query = list(Version = 2,
                     From = list(list(Name = "v", Entity = "View_STAR_Metadata", Type = 0)),
                     Select = select_list),
        Binding = list(Primary = list(Groupings = list(list(Projections = projections))),
                       DataReduction = list(DataVolume = 4, Primary = list(Window = list(Count = 1000))),
                       Version = 1),
        ExecutionMetricsKind = 1
      )))),
      QueryId = "",
      ApplicationContext = list(DatasetId = DATASET_ID, Sources = list(list(ReportId = REPORT_ID)))
    )),
    cancelQueries = list(),
    modelId = MODEL_ID
  )
  
  resp <- request(ENDPOINT) %>%
    req_headers(
      "Content-Type" = "application/json;charset=UTF-8",
      "X-PowerBI-ResourceKey" = RESOURCE_KEY,
      "Origin" = "https://app.powerbi.com",
      "Referer" = "https://app.powerbi.com/",
      "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0 Safari/537.36"
    ) %>%
    req_body_json(body) %>%
    req_error(is_error = function(resp) FALSE) %>%
    req_perform()
  
  message("HTTP status: ", resp_status(resp))
  raw_body <- resp_body_string(resp)
  writeLines(raw_body, out_path)
  message("Saved raw response to ", out_path, " (", nchar(raw_body), " chars)")
  out_path
}

## -----------------------------------------------------------------
## 3. Parse the Power BI DSR-compressed response into a data frame
## Faithful R port of https://gist.github.com/svavassori/3319ff9d7e16a8788665ca59a5a04889
## -----------------------------------------------------------------

is_bit_set <- function(index, bitset) {
  if (is.null(bitset) || bitset == 0) return(FALSE)
  (bitset %/% (2^index)) %% 2 >= 1
}

reconstruct_rows <- function(dm0, n_cols) {
  prev <- NULL
  for (r in seq_along(dm0)) {
    current <- dm0[[r]]$C
    R_bits <- dm0[[r]][["R"]]
    O_bits <- dm0[[r]][["\u00d8"]]
    
    if (!is.null(R_bits) || !is.null(O_bits)) {
      for (i in 0:(n_cols - 1)) {
        if (is_bit_set(i, R_bits)) {
          current <- append(current, list(prev[[i + 1]]), after = i)
        } else if (is_bit_set(i, O_bits)) {
          current <- append(current, list(NULL), after = i)
        }
      }
    }
    dm0[[r]]$C <- current
    prev <- current
  }
  dm0
}

expand_values <- function(dm0, columns_schema, value_dicts) {
  for (idx in seq_along(columns_schema)) {
    dn <- columns_schema[[idx]][["DN"]]
    if (!is.null(dn)) {
      dict <- value_dicts[[dn]]
      for (r in seq_along(dm0)) {
        v <- dm0[[r]]$C[[idx]]
        if (!is.null(v) && is.numeric(v)) dm0[[r]]$C[[idx]] <- dict[[v + 1]]
      }
    }
  }
  dm0
}

parse_star_response <- function(path) {
  resp <- fromJSON(path, simplifyVector = FALSE)
  data <- resp$results[[1]]$result$data
  
  dm0            <- data$dsr$DS[[1]]$PH[[1]]$DM0
  columns_schema <- dm0[[1]]$S
  value_dicts    <- data$dsr$DS[[1]]$ValueDicts
  n_cols         <- length(columns_schema)
  
  message("Rows in DM0: ", length(dm0), " | Columns per schema: ", n_cols)
  
  dm0 <- reconstruct_rows(dm0, n_cols)
  dm0 <- expand_values(dm0, columns_schema, value_dicts)
  
  col_names <- sapply(data$descriptor$Select, function(item) {
    if (!is.null(item$Kind) && item$Kind == 1 && !is.null(item$GroupKeys)) {
      item$GroupKeys[[1]]$Source$Property
    } else item$Value
  })
  
  rows_list <- lapply(dm0, function(row) {
    vals <- lapply(row$C, function(v) if (is.null(v)) NA else v)
    if (length(vals) < n_cols) vals <- c(vals, rep(list(NA), n_cols - length(vals)))
    vals
  })
  
  df <- as.data.frame(do.call(rbind, lapply(rows_list, function(r) {
    sapply(r, function(x) if (length(x) == 0) NA else x)
  })), stringsAsFactors = FALSE)
  names(df) <- col_names
  
  for (col in c("Reference year v2", "Reporting year v2")) {
    if (col %in% names(df)) {
      ms <- suppressWarnings(as.numeric(df[[col]]))
      df[[col]] <- as.Date(as.POSIXct(ms / 1000, origin = "1970-01-01", tz = "UTC"))
    }
  }
  
  df
}

## -----------------------------------------------------------------
## 4. GSA -> GFCM subregion lookup (FAO Major Fishing Area 37)
## -----------------------------------------------------------------

gsa_to_subregion <- function(gsa_num) {
  case_when(
    gsa_num %in% 1:11 ~ "Western Mediterranean",
    gsa_num %in% c(12, 13, 14, 15, 16, 19, 20, 21) ~ "Central Mediterranean (Ionian)",
    gsa_num %in% c(17, 18) ~ "Adriatic Sea",
    gsa_num %in% 22:27 ~ "Eastern Mediterranean",
    gsa_num %in% 28:30 ~ "Black Sea",
    TRUE ~ NA_character_
  )
}

assign_subregion <- function(gsa_string) {
  if (is.na(gsa_string)) return(NA_character_)
  parts <- str_split(gsa_string, ",")[[1]] %>% str_trim() %>% as.numeric() %>% floor()
  subregions <- unique(gsa_to_subregion(parts))
  subregions <- subregions[!is.na(subregions)]
  if (length(subregions) == 0) return(NA_character_)
  if (length(subregions) == 1) return(subregions)
  paste(subregions, collapse = " + ")
}

## -----------------------------------------------------------------
## 5. Verification checks (internal consistency only - see conversation
## notes: this does NOT prove correctness against GFCM's live database,
## only that parsing didn't scramble rows/columns internally)
## -----------------------------------------------------------------

verify_star_data <- function(df) {
  message("\n--- Verification ---")
  message("Rows: ", nrow(df), " (expected 550)")
  
  parsed_id <- df %>%
    mutate(
      id_species = str_match(Assessment_ID, "STAR_\\d{4}_([A-Z0-9]+)_")[, 2],
      id_gsa     = str_match(Assessment_ID, "_([0-9.]+)$")[, 2]
    )
  species_match <- mean(parsed_id$id_species == parsed_id$`3AlphaCode`, na.rm = TRUE)
  message("Species code in ID matches 3AlphaCode: ", round(species_match * 100, 1), "%")
  
  char_cols <- names(df)[sapply(df, is.character)]
  suspicious <- sapply(char_cols, function(col) {
    vals <- na.omit(df[[col]])
    if (length(vals) == 0) return(FALSE)
    all(str_detect(vals, "^[0-9]+$")) && all(as.numeric(vals) < 200)
  })
  if (any(suspicious)) {
    message("WARNING - possibly half-decoded columns: ", paste(char_cols[suspicious], collapse = ", "))
  } else {
    message("No half-decoded columns detected.")
  }
  
  ## Landings can never legitimately exceed Catches (landings are a subset
  ## of total catch = landings + discards, by definition). A violation here
  ## is a strong signal of a data-entry error (e.g. kg entered where tonnes
  ## was expected, off by ~1000x) rather than normal variation.
  impossible <- df %>%
    filter(!is.na(Landings), !is.na(Catches), Landings > Catches) %>%
    select(Assessment_ID, Species_Common_Name, GSA, Reference_Year, Landings, Catches, FileURL)
  
  if (nrow(impossible) > 0) {
    message("\nWARNING - ", nrow(impossible), " row(s) have Landings > Catches (impossible - landings",
            " are a subset of total catch). Check these against their FileURL before using:")
    print(impossible)
  } else {
    message("No rows with Landings > Catches - passes this sanity check.")
  }
}

## =================================================================
## RUN THE PIPELINE
## =================================================================

raw_path <- fetch_star_raw()
star_raw_df <- parse_star_response(raw_path)

star_clean <- star_raw_df %>%
  mutate(
    GSA = as.character(GSA),
    Reference_Year = as.integer(Reference_Year),
    Reporting_Year = as.integer(Reporting_Year),
    Current_B = as.numeric(Current_B),
    Current_F = as.numeric(Current_F),
    Bmsy = as.numeric(Bmsy),
    Blim = as.numeric(Blim),
    Bpa  = as.numeric(Bpa),
    Fmsy = as.numeric(Fmsy),
    Catches  = as.numeric(Catches),
    Landings = as.numeric(Landings),
    Stock1_Indicator = as.numeric(Stock1_Indicator),
    GFCM_Subregion = vapply(GSA, assign_subregion, character(1))
  )

verify_star_data(star_clean)

## -----------------------------------------------------------------
## Optional correction for confirmed unit-entry errors (Landings > Catches).
## OFF by default - only enable after checking the FileURL for a specific
## Assessment_ID and confirming it's genuinely a kg-vs-tonnes mistake, not
## something else. Nothing here applies automatically/silently.
## -----------------------------------------------------------------

APPLY_MANUAL_CORRECTIONS <- FALSE

manual_corrections <- tibble::tribble(
  ~Assessment_ID,       ~fix,
  "STAR_2025_HKE_3",    "divide_landings_by_1000"
  # add more rows here as you confirm them against FileURL
)

if (APPLY_MANUAL_CORRECTIONS) {
  for (i in seq_len(nrow(manual_corrections))) {
    id <- manual_corrections$Assessment_ID[i]
    fix <- manual_corrections$fix[i]
    if (fix == "divide_landings_by_1000") {
      old_val <- star_clean$Landings[star_clean$Assessment_ID == id]
      star_clean$Landings[star_clean$Assessment_ID == id] <- old_val / 1000
      message("Corrected ", id, ": Landings ", old_val, " -> ", old_val / 1000)
    }
  }
} else {
  message("\nManual corrections defined but NOT applied (APPLY_MANUAL_CORRECTIONS = FALSE).",
          " Set to TRUE once you've confirmed ", nrow(manual_corrections),
          " correction(s) against their FileURL.")
}

write_csv(star_clean, "star_data_clean.csv")
message("\nSaved full cleaned table: star_data_clean.csv (", nrow(star_clean), " rows x ", ncol(star_clean), " cols)")

## -----------------------------------------------------------------
## 6. Tidy/standardized subset - for merging with RAM Legacy later
## Column names deliberately mirror what the RAM Legacy extract uses
## (see ramlegacy_westmed_extract.R): stockid-like id, species,
## common_name, gsa, year, biomass, landings, subregion, source
## -----------------------------------------------------------------

## GSA field normalizer - canonical format shared with the RAM Legacy
## pipeline: sorted, comma-separated, no prefix. Ensures "6" and "GSA6"
## (or any other formatting variant) land in the same facet when combined.
normalize_gsa <- function(gsa_string) {
  if (is.na(gsa_string)) return(NA_character_)
  nums <- str_split(gsa_string, ",")[[1]] %>% str_trim() %>% as.numeric()
  nums <- sort(unique(nums))
  paste(nums, collapse = ",")
}

## --- 6b. Units --------------------------------------------------------------
## Catches_Unit is a real field in the data - used directly, not guessed.
## There is no equivalent explicit unit field for Current_B in the STAR
## schema. Stock1_Unit exists but describes Stock1_Indicator, which is not
## necessarily the same field as Current_B - print how often they co-occur
## as a diagnostic rather than assuming they match.

message("\nCatches_Unit values observed: ")
print(star_clean %>% filter(!is.na(Landings)) %>% count(Catches_Unit, sort = TRUE))

message("\nStock1_Unit values where Current_B is populated (diagnostic only -",
        " NOT necessarily the unit for Current_B, just checking for a pattern):")
print(star_clean %>% filter(!is.na(Current_B)) %>% count(Stock1_Unit, sort = TRUE))

## --- 6c. Weight unit harmonization -------------------------------------
## Same function as used in the RAM Legacy pipeline - "tonnes"/"t"/"MT"
## are the same real-world unit (label fix only); "kg" would need an
## actual value conversion, handled here too rather than assumed away.
harmonize_weight <- function(value, unit) {
  u <- str_trim(tolower(unit))
  tonnes_aliases <- c("mt", "t", "tonnes", "tonne", "metric tonnes", "metric tons", "metric ton")
  kg_aliases <- c("kg", "kgs", "kilogram", "kilograms")
  
  unit_out <- dplyr::case_when(
    u %in% tonnes_aliases ~ "MT",
    u %in% kg_aliases     ~ "MT",
    TRUE ~ unit
  )
  value_out <- dplyr::if_else(u %in% kg_aliases, value / 1000, value)
  list(value = value_out, unit = unit_out)
}

star_tidy <- star_clean %>%
  transmute(
    source = "STAR",
    stock_key = Assessment_ID,
    species = Species,
    common_name = Species_Common_Name,
    gsa = vapply(GSA, normalize_gsa, character(1)),
    subregion = GFCM_Subregion,
    year = Reference_Year,
    biomass = Current_B,
    biomass_unit_raw = NA_character_,   # no explicit field in the STAR schema - see diagnostic above
    b_ratio = Current_B / Bmsy,   # computed ratio, matches RAM Legacy's b_ratio semantics
    landings = Landings,
    landings_unit_raw = Catches_Unit,   # STAR has one shared unit field for Catches and Landings
    catches = Catches,
    catches_unit_raw = Catches_Unit
  )

landings_h <- harmonize_weight(star_tidy$landings, coalesce(star_tidy$landings_unit_raw, ""))
catches_h  <- harmonize_weight(star_tidy$catches, coalesce(star_tidy$catches_unit_raw, ""))

star_tidy <- star_tidy %>%
  mutate(
    biomass_unit = biomass_unit_raw,   # stays NA - nothing to harmonize
    landings = landings_h$value,
    landings_unit = if_else(landings_unit_raw == "" | is.na(landings_unit_raw), NA_character_, landings_h$unit),
    catches = catches_h$value,
    catches_unit = if_else(catches_unit_raw == "" | is.na(catches_unit_raw), NA_character_, catches_h$unit)
  ) %>%
  select(-biomass_unit_raw, -landings_unit_raw, -catches_unit_raw)

message("\nSTAR unit values after harmonization:")
print(star_tidy %>% count(landings_unit, catches_unit))

star_tidy <- star_tidy %>%
  mutate(landings_flag = !is.na(landings) & !is.na(catches) & landings > catches)

if (any(star_tidy$landings_flag)) {
  message("\n", sum(star_tidy$landings_flag), " row(s) flagged with landings_flag = TRUE",
          " (Landings > Catches) - kept in the data but excluded from plots by default.",
          " See star_plots.R / combine_sources.R.")
}

write_csv(star_tidy, "star_data_tidy.csv")
message("Saved tidy subset for merging: star_data_tidy.csv (", nrow(star_tidy), " rows)")




## =================================================================
## GFCM STAR plots - reads star_data_tidy.csv (from star_data_pipeline.R)
## Facets by species (own axis scales via free), colors by GSA.
## Unit (where known) is appended directly to the y-axis title.
## =================================================================

pkgs <- c("readr", "dplyr", "ggplot2", "stringr")
new_pkgs <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new_pkgs) > 0) install.packages(new_pkgs)
invisible(lapply(pkgs, library, character.only = TRUE))

if (!dir.exists("plots")) dir.create("plots")

star <- read_csv("star_data_tidy.csv", col_types = cols(
  source = col_character(), stock_key = col_character(), species = col_character(),
  common_name = col_character(), gsa = col_character(), subregion = col_character(),
  year = col_integer(), biomass = col_double(), biomass_unit = col_character(),
  b_ratio = col_double(), landings = col_double(), landings_unit = col_character(),
  catches = col_double(), catches_unit = col_character(), landings_flag = col_logical()
))

## --- Config ----------------------------------------------------------------

REGION_FILTER <- "Western Mediterranean"
COLOR_BY      <- "gsa"
TOP_N_SPECIES <- 10
TOP_N_GSA     <- 8
MIN_OBS_PER_FACET <- 3

## --- Filter ------------------------------------------------------------------

df <- star
if (!is.null(REGION_FILTER)) df <- df %>% filter(str_detect(subregion, REGION_FILTER))

## facet label: common name on one line, scientific name in parentheses below
df <- df %>% mutate(species_label = paste0(common_name, "\n(", species, ")"))
FACET_BY <- "species_label"

message(nrow(df), " assessments after region filter (",
        ifelse(is.null(REGION_FILTER), "all regions", REGION_FILTER), ")")
message("Year range: ", paste(range(df$year, na.rm = TRUE), collapse = " - "))

## --- Helpers -----------------------------------------------------------------

drop_sparse <- function(data, facet_col, min_n = MIN_OBS_PER_FACET, label = "") {
  counts <- data %>% count(.data[[facet_col]])
  keep <- counts %>% filter(n >= min_n) %>% pull(.data[[facet_col]])
  dropped <- counts %>% filter(n < min_n)
  if (nrow(dropped) > 0) {
    message("[", label, "] dropping ", nrow(dropped), " sparse facet group(s): ",
            paste(dropped[[facet_col]], collapse = ", "))
  }
  data %>% filter(.data[[facet_col]] %in% keep)
}

## Single unit string for the y-axis title: the value itself if uniform
## across the plotted data, "mixed units" if it genuinely varies, or
## "unit unknown" if never populated.
get_unit_label <- function(data, unit_col) {
  units <- unique(na.omit(data[[unit_col]]))
  if (length(units) == 0) return("unit unknown")
  if (length(units) == 1) return(units)
  "mixed units"
}

top_species <- df %>%
  filter(!is.na(biomass)) %>%
  count(common_name, sort = TRUE) %>%
  slice_head(n = TOP_N_SPECIES) %>%
  pull(common_name)

top_gsa <- df %>%
  filter(!is.na(biomass)) %>%
  count(gsa, sort = TRUE) %>%
  slice_head(n = TOP_N_GSA) %>%
  pull(gsa)

## --- Plot 1: raw biomass (Current_B) ----------------------------------------

biomass_data <- df %>%
  filter(common_name %in% top_species, gsa %in% top_gsa, !is.na(biomass)) %>%
  drop_sparse(FACET_BY, label = "Biomass")

p_biomass <- ggplot(biomass_data, aes(x = year, y = biomass, color = .data[[COLOR_BY]])) +
  geom_point(size = 2.3, alpha = 0.85) +
  geom_line(aes(group = .data[[COLOR_BY]]), alpha = 0.4, linewidth = 0.4) +
  facet_wrap(vars(.data[[FACET_BY]]), scales = "free") +
  labs(title = paste0("GFCM STAR: Current biomass by species",
                      if (!is.null(REGION_FILTER)) paste0(" (", REGION_FILTER, ")") else ""),
       x = "Reference year", y = paste0("Current B (", get_unit_label(biomass_data, "biomass_unit"), ")"),
       color = "GSA") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", legend.text = element_text(size = 8))

ggsave("plots/star_biomass_raw.png", p_biomass, width = 13, height = 9, dpi = 150)

## --- Plot 2: B / Bmsy ratio (dimensionless) --------------------------------

status_data <- df %>%
  filter(common_name %in% top_species, gsa %in% top_gsa, !is.na(b_ratio)) %>%
  drop_sparse(FACET_BY, label = "B/Bmsy")

p_status <- ggplot(status_data, aes(x = year, y = b_ratio, color = .data[[COLOR_BY]])) +
  geom_hline(yintercept = 1, linetype = "dashed", color = "grey40") +
  geom_point(size = 2.3, alpha = 0.85) +
  facet_wrap(vars(.data[[FACET_BY]]), scales = "free") +
  labs(title = paste0("GFCM STAR: B / Bmsy ratio by species",
                      if (!is.null(REGION_FILTER)) paste0(" (", REGION_FILTER, ")") else ""),
       x = "Reference year", y = "Current B / Bmsy", color = "GSA") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", legend.text = element_text(size = 8))

ggsave("plots/star_b_over_bmsy.png", p_status, width = 13, height = 9, dpi = 150)

## --- Plot 3: Total landings ---------------------------------------------

landings_data <- df %>%
  filter(common_name %in% top_species, gsa %in% top_gsa, !is.na(landings), !landings_flag) %>%
  drop_sparse(FACET_BY, label = "Total landings")

n_excluded <- sum(df$landings_flag %in% TRUE)
if (n_excluded > 0) message("Excluded ", n_excluded, " flagged row(s) with landings > catches from the landings plot.")

p_landings <- ggplot(landings_data, aes(x = year, y = landings, color = .data[[COLOR_BY]])) +
  geom_point(size = 2.3, alpha = 0.85) +
  geom_line(aes(group = .data[[COLOR_BY]]), alpha = 0.4, linewidth = 0.4) +
  facet_wrap(vars(.data[[FACET_BY]]), scales = "free") +
  labs(title = paste0("GFCM STAR: Total landings by species",
                      if (!is.null(REGION_FILTER)) paste0(" (", REGION_FILTER, ")") else ""),
       x = "Reference year", y = paste0("Total landings (", get_unit_label(landings_data, "landings_unit"), ")"),
       color = "GSA") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", legend.text = element_text(size = 8))

ggsave("plots/star_landings.png", p_landings, width = 13, height = 9, dpi = 150)

## --- Plot 4: Total catch (kept separate from landings - not the same thing) -

catches_data <- df %>%
  filter(common_name %in% top_species, gsa %in% top_gsa, !is.na(catches)) %>%
  drop_sparse(FACET_BY, label = "Total catch")

p_catches <- ggplot(catches_data, aes(x = year, y = catches, color = .data[[COLOR_BY]])) +
  geom_point(size = 2.3, alpha = 0.85) +
  geom_line(aes(group = .data[[COLOR_BY]]), alpha = 0.4, linewidth = 0.4) +
  facet_wrap(vars(.data[[FACET_BY]]), scales = "free") +
  labs(title = paste0("GFCM STAR: Total catch by species",
                      if (!is.null(REGION_FILTER)) paste0(" (", REGION_FILTER, ")") else ""),
       x = "Reference year", y = paste0("Total catch (", get_unit_label(catches_data, "catches_unit"), ")"),
       color = "GSA") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "bottom", legend.text = element_text(size = 8))

ggsave("plots/star_catches.png", p_catches, width = 13, height = 9, dpi = 150)

message("\nSaved:")
message("- plots/star_biomass_raw.png")
message("- plots/star_b_over_bmsy.png")
message("- plots/star_landings.png")
message("- plots/star_catches.png")