## ---------------------------------------------------------------
## Map of the Western Mediterranean (FAO Major Fishing Area 37,
## Subarea 37.1 = GSA 1-11), with the Sicily Channel divide
## (Cape Bon - Cape Lilibeo) marking the separation from the
## Central Mediterranean, passing through GSA 12 and GSA 16
## ---------------------------------------------------------------

## --- 0. Packages ---------------------------------------------------------

pkgs <- c("sf", "terra", "ggplot2", "dplyr", "stringr", "tibble", "scales",
          "rnaturalearth", "rnaturalearthdata", "polylabelr")
new_pkgs <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new_pkgs) > 0) install.packages(new_pkgs)
invisible(lapply(pkgs, library, character.only = TRUE))

# High-resolution coastline (10m) - needed so islands (Balearics, Corsica,
# Sardinia, Sicily) actually render instead of being dropped at coarser
# scales. Not on CRAN by default; installed from the ROpenSci r-universe.
if (!requireNamespace("rnaturalearthhires", quietly = TRUE)) {
  install.packages("rnaturalearthhires", repos = "https://ropensci.r-universe.dev", type = "source")
}

## =================================================================
## STEP 1: Configuration
##
## SOURCE-ABLE SCRIPT: out_dir is only set below when it isn't already
## defined in the calling environment - if a driver script (e.g.
## run_pipeline_demo.R) sets out_dir BEFORE calling
## source("fig_WMed_basemap.R"), that value is used as-is and none of
## the interactive prompts/hardcoded defaults below fire, so the
## shapefile cache and rendered map land right next to 01_biomass.R's/
## 02_fisheries.R's/etc.'s own outputs instead of wherever the R
## session happened to have as its working directory. Running this
## script standalone (nothing pre-set) reproduces the exact original
## hardcoded-user-or-interactive-prompt behavior - this change is
## purely additive. No setwd() anymore either way - every path below
## is built explicitly off out_dir with file.path().
## =================================================================
## Checks the pre-set value itself, not just whether the NAME out_dir
## exists - exists() alone would happily accept a leftover out_dir <- NULL
## from an earlier cancelled rstudioapi::selectDirectory() call (in this
## session or an earlier 01_biomass.R run) and skip every resolution
## branch below, which is exactly what produced the "invalid filename
## argument" / "out_dir did not resolve to a valid..." errors - out_dir
## existed as a NAME, just not as a usable value.
out_dir_preset_valid <- exists("out_dir", envir = .GlobalEnv, inherits = FALSE) &&
  is.character(out_dir) && length(out_dir) == 1 && !is.na(out_dir) && nzchar(out_dir)

if (out_dir_preset_valid) {
  message("[fig_WMed_basemap.R] Using pre-set out_dir from calling environment:\n  out_dir = ", out_dir)
} else if (exists("out_dir", envir = .GlobalEnv, inherits = FALSE) && !out_dir_preset_valid) {
  message("[fig_WMed_basemap.R] out_dir exists in the calling environment but isn't a valid path (",
          if (is.null(out_dir)) "NULL" else paste0("class ", class(out_dir), ", length ", length(out_dir)),
          ") - ignoring it and resolving out_dir fresh below.")
  rm(out_dir, envir = .GlobalEnv)
}
if (!out_dir_preset_valid) {
  if (tolower(Sys.info()[["user"]]) == "daniel" && .Platform$OS.type == "unix") {
    out_dir <- "/Users/daniel/Work/iMARES/WMed EwE Model"
  } else if (tolower(Sys.info()[["user"]]) == "danie" && .Platform$OS.type == "windows") {
    out_dir <- "C:/Users/danie/Desktop/iMARES/WMed EwE Model"
  } else {
    ## Falls back to an interactive directory picker in RStudio, rather
    ## than just stopping with "set it manually" - so this script works
    ## for anyone, not just the two hardcoded usernames above.
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
    if (is.null(out_dir) || length(out_dir) != 1 || is.na(out_dir) || out_dir == "") {
      stop("No valid output directory selected.")
    }
  }
}

## Defensive check on whatever out_dir ended up being - whether it came
## pre-set from the calling environment, one of the two hardcoded
## defaults above, or the RStudio picker. dir.exists()/dir.create()
## themselves fail with the unhelpful "invalid filename argument" if
## out_dir isn't a single non-NA character string (e.g. NULL, character(0),
## or accidentally a vector of more than one path) - catching that here
## instead gives a clear, specific reason rather than that cryptic error.
if (is.null(out_dir) || !is.character(out_dir) || length(out_dir) != 1 || is.na(out_dir) || out_dir == "") {
  stop("out_dir did not resolve to a valid single directory path (got: ",
       if (is.null(out_dir)) "NULL" else paste0("class ", class(out_dir), ", length ", length(out_dir), ", value(s): ", paste(out_dir, collapse = ", ")),
       "). Set out_dir explicitly to a single path (a string) before source()-ing this script, ",
       "e.g. out_dir <- \"/path/to/output\".")
}
if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)

## --- 1. Download & unzip official GFCM GSA shapefile ---------------------

shapefile_dir <- file.path(out_dir, "shapefiles")
dir.create(shapefile_dir, recursive = TRUE, showWarnings = FALSE)
zip_url  <- "https://gfcmsitestorage.blob.core.windows.net/website/5.Data/ArcGIS/GFCM_GSA.zip"
zip_file <- file.path(shapefile_dir, "GFCM_GSA.zip")
shp_dir  <- file.path(shapefile_dir, "GFCM_GSA_shp")

if (!dir.exists(shp_dir)) {
  if (!file.exists(zip_file)) {
    dir.create(dirname(zip_file), recursive = TRUE, showWarnings = FALSE)
    download.file(zip_url, destfile = zip_file, mode = "wb", method = "libcurl")
  }
  unzip(zip_file, exdir = shp_dir)
} else {
  message("Shapefile folder already exists. Using existing files.")
}

shp_path <- list.files(shp_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)[1]
gsa_sf <- st_read(shp_path, quiet = TRUE)   # native CRS: WGS84 (EPSG:4326)

## --- 2. Inspect attributes (uncomment to check column names/values) ------

# print(names(gsa_sf))
# print(st_drop_geometry(gsa_sf) %>% select(SMU_CODE, SMU_NAME, F_DIVISION, CENTER_X, CENTER_Y))

## SMU_CODE = numeric GSA id. NOTE: Western/Eastern Sardinia are stored as
## SMU_CODE 111 / 112 (not 11.1 / 11.2), so they need a manual bucket back
## into GSA "11" for filtering/coloring purposes.
## F_DIVISION = FAO statistical division code (37.1.1 Balearic, 37.1.2 Gulf
## of Lion, 37.1.3 Sardinia - these three divisions together = Subarea 37.1,
## the official "Western Mediterranean")
## CENTER_X/CENTER_Y = GFCM's own official label-placement coordinates per
## GSA - used directly below instead of an auto-computed point-on-surface,
## which is both more reliable for irregular/elongated GSA shapes and
## avoids the st_point_on_surface warning entirely.

gsa_sf$gsa_num <- as.numeric(gsa_sf$SMU_CODE)
gsa_sf$gsa_group <- case_when(
  gsa_sf$gsa_num %in% c(111, 112) ~ 11,
  TRUE ~ gsa_sf$gsa_num
)

## --- 3. Split into Western Med (target) vs Central Med (context) ---------

westmed_ids <- 1:11
context_ids <- c(12, 16)   # Northern Tunisia, South of Sicily

division_labels <- c(
  "37.1.1" = "Balearic (GSA 1-6)",
  "37.1.2" = "Gulf of Lion (GSA 7)",
  "37.1.3" = "Sardinia (GSA 8-11)"
)

westmed_sf <- gsa_sf %>%
  filter(gsa_group %in% westmed_ids) %>%
  mutate(division_label = recode(F_DIVISION, !!!division_labels))

context_sf <- gsa_sf %>%
  filter(gsa_group %in% context_ids) %>%
  mutate(context_label = paste0("GSA ", gsa_group))   # "GSA 12", not just "12"

if (nrow(westmed_sf) == 0) {
  stop("No polygons matched GSA 1-11 - check gsa_sf$gsa_num values (should be 1-30).")
}

## Sanity check: confirm both Sardinia sub-areas are actually present
message("Rows in westmed_sf: ", nrow(westmed_sf))
message("F_GSA_LIB values present: ", paste(sort(westmed_sf$F_GSA_LIB), collapse = ", "))
if (!any(str_detect(westmed_sf$F_GSA_LIB, "11.1"))) {
  message("WARNING: GSA 11.1 not found in westmed_sf - check SMU_CODE values directly:")
  print(gsa_sf %>% st_drop_geometry() %>% filter(gsa_group == 11) %>% select(SMU_CODE, F_GSA_LIB))
}

## --- 4. Label placement ---------------------------------------------------

## st_point_on_surface only guarantees a point INSIDE the polygon, not one
## far from the edges - for narrow, coastline-hugging GSAs (1, 4, 12
## especially) that point can still land right next to land. The correct
## tool is the "pole of inaccessibility" (the point maximizing distance
## from any edge) - the same technique Mapbox and other map labeling tools
## use. polylabelr implements this.

compute_polylabel <- function(sf_obj) {
  coords_out <- matrix(NA_real_, nrow = nrow(sf_obj), ncol = 2)
  for (i in seq_len(nrow(sf_obj))) {
    geom <- sf::st_geometry(sf_obj)[[i]]
    result <- tryCatch({
      # if MULTIPOLYGON, keep only the largest part by area (avoids picking
      # a tiny fragment's interior for the label)
      parts <- sf::st_cast(sf::st_sfc(geom, crs = sf::st_crs(sf_obj)), "POLYGON")
      areas <- sf::st_area(parts)
      largest <- parts[[which.max(areas)]]
      ring <- largest[[1]]   # exterior ring coordinates matrix
      poi(list(ring[, 1], ring[, 2]), precision = 0.001)
    }, error = function(e) NULL)
    
    if (!is.null(result)) {
      coords_out[i, ] <- c(result$x, result$y)
    } else {
      # fallback if polylabel fails for this geometry - point_on_surface is
      # at least guaranteed to be inside, even if close to an edge
      pt <- suppressWarnings(sf::st_point_on_surface(sf::st_geometry(sf_obj)[[i]]))
      coords_out[i, ] <- sf::st_coordinates(pt)
    }
  }
  sf_obj$label_x <- coords_out[, 1]
  sf_obj$label_y <- coords_out[, 2]
  sf_obj
}

westmed_sf <- compute_polylabel(westmed_sf)
context_sf <- compute_polylabel(context_sf)

## GSA 16 specifically looks better with GFCM's official center point - it's
## a large, elongated area (South of Sicily, extending toward Malta/Libya),
## so its true pole of inaccessibility sits further south than where the
## label is intuitively expected. GSA 12 looks fine with polylabel, so only
## GSA 16 gets overridden here rather than applying one rule to both.
context_sf <- context_sf %>%
  mutate(
    label_x = if_else(gsa_group == 16, CENTER_X, label_x),
    label_y = if_else(gsa_group == 16, CENTER_Y, label_y)
  )

## Diagnostic: label position vs each polygon's own bounding box, so you
## can see numerically if a label landed somewhere unexpected (e.g. pulled
## toward the southern edge of a multi-part or irregularly shaped GSA)
message("\nContext label positions vs polygon bounding box:")
print(context_sf %>% st_drop_geometry() %>%
        mutate(bbox_ymin = sapply(seq_len(n()), function(i) st_bbox(context_sf[i, ])["ymin"]),
               bbox_ymax = sapply(seq_len(n()), function(i) st_bbox(context_sf[i, ])["ymax"])) %>%
        dplyr::select(context_label, label_x, label_y, bbox_ymin, bbox_ymax))

## Manual overrides - if a specific label still looks wrong after the
## automatic placement (common for irregular/multi-part shapes where
## "farthest from any edge" isn't the same as "where you'd expect it"),
## add a row here to nudge it. Matches on the label text shown on the map.
label_overrides <- tribble(
  ~label_text, ~new_x, ~new_y
  # ~"GSA 16",  13.7,   37.0    # example - uncomment and adjust once you see the diagnostic above
)

apply_overrides <- function(sf_obj, label_col) {
  for (i in seq_len(nrow(label_overrides))) {
    match_rows <- sf_obj[[label_col]] == label_overrides$label_text[i]
    sf_obj$label_x[match_rows] <- label_overrides$new_x[i]
    sf_obj$label_y[match_rows] <- label_overrides$new_y[i]
  }
  sf_obj
}

westmed_sf <- apply_overrides(westmed_sf, "F_GSA_LIB")
context_sf <- apply_overrides(context_sf, "context_label")

## --- 5. Sicily Channel divide (western/central Med boundary) -------------

sicily_divide <- st_sfc(
  st_linestring(matrix(c(
    11.0308, 37.0708,   # Cape Bon, Tunisia
    12.4333, 37.8000    # Cape Lilibeo / Marsala, Sicily
  ), ncol = 2, byrow = TRUE)),
  crs = 4326
)

## --- 6. Basemap (coastline) ------------------------------------------------
#if error, then lower resolution
coast <- tryCatch(
  ne_countries(scale = 10, returnclass = "sf"),
  error = function(e) {
    message("High-resolution coastline unavailable. Falling back to medium resolution.")
    ne_countries(scale = 50, returnclass = "sf")
  }
)

## --- 7. Clean Western Mediterranean outer contour -------------------------
## Union all GSAs into one outer outline, then keep only the largest piece
## (drops any tiny disconnected slivers) and extract its boundary.
##
## A short, isolated black line used to show up inside this outline right
## at the GSA 9/10/11.2 junction. Two rasterize-based fixes were tried
## first (a fixed-distance morphological "closing", then a finer rasterize
## grid) on the assumption that this was a rasterization artifact - a
## seam band failing rasterize()'s cell-center test along a border two
## GSAs share exactly. Neither actually fixed it: the finer-grid version
## made the line MORE visible, not less, which only makes sense if the
## line isn't a rasterization artifact at all - it's a genuine sliver GAP
## between adjacent GSA polygons in the official GFCM shapefile (a common
## digitization issue: neighboring zones drawn independently don't always
## share an exact edge, leaving a hairline unfilled strip between them).
## A finer raster grid resolves a real gap MORE precisely/visibly instead
## of bridging it, which is exactly the "looks larger now" symptom
## reported - confirming this diagnosis. Switching to a small, targeted
## vector-level gap closing (below) fixes the actual gap directly, and
## skips rasterize()/terra entirely, so there's no raster-resolution
## trade-off (artifact width vs. runtime/memory) to tune at all.
##
## The very first fix attempt (buffering the polygon out and back in by a
## fixed real-world distance, a "closing") DID remove the line, but at
## the cost of visibly degrading the WHOLE outline into a coarse,
## staircase-like shape - GEOS's buffer approximates curves as short
## straight segments, and re-approximating thousands of coastline
## vertices that way replaced the natural detail with a chunky,
## low-resolution-looking silhouette. That failure was because the
## buffer distance used (0.005 deg, ~550 m) was far bigger than it needed
## to be for closing a hairline sliver gap. Buffer approximation error
## scales with the buffer distance itself, so a MUCH smaller, gap-sized
## buffer (below) closes the real gap without visibly coarsening the
## coastline anywhere else - verified via a synthetic two-almost-touching-
## polygons test before applying here.
## s2 kept off through this ENTIRE block (gap measurement -> union ->
## gap-closing buffer -> validity repair -> cast -> area -> boundary),
## not just around one call - restoring it partway through was the
## actual bug in an earlier version of this fix: a ring/gap-closing
## result that GEOS considers valid can still trip S2's OWN, stricter
## validity check (S2 revalidates geometry internally whenever it
## computes area on it under a geographic CRS), which is why
## st_area()/which.max() kept throwing "Loop N is not valid: Edge X
## crosses edge Y" even after the geometry had already been repaired
## under GEOS. s2 is restored right after, for every sf operation
## elsewhere in this script.
s2_was_on <- sf::sf_use_s2()
sf::sf_use_s2(FALSE)

## GAP_CLOSE_DIST: how far to buffer out-and-back-in to bridge a
## hairline digitization gap between two GSA polygons that were meant to
## share an edge (see the long comment above this block for how that
## diagnosis was reached). A first attempt hardcoded this to 0.001 deg
## (~110 m) - that shortened the stray line but didn't fully close it,
## meaning the real gap is WIDER than that in at least one spot; the
## very first fix attempt (before the diagnosis above) had used 0.005
## deg and DID fully close it, but that was measured on a
## rasterized/blocky version of the polygon, where a buffer of any size
## compounds with the raster's own blockiness - not necessarily a fair
## estimate of how much a buffer this size would smooth a normal,
## already-smooth vector polygon like westmed_sf.
##
## Rather than guess a third fixed number, measure every real gap
## directly from the loaded GSA polygons and size the buffer off the
## worst one (plus a 20% margin), so this keeps working correctly even
## if the underlying GFCM shapefile changes. Only gaps under 0.05 deg
## (~5 km) are considered "real" adjacency gaps meant to be closed - two
## GSAs that are simply far apart on the map (not meant to touch) will
## have a much larger distance between them than that, and must NOT
## pull GAP_CLOSE_DIST up to something that would coarsen the coastline
## everywhere. Distances computed with the CRS stripped (same reasoning
## as the area computation below: only a relative degree-unit distance
## is needed to size a closing buffer, not a real-world metric one, so
## no lwgeom dependency).
westmed_planar <- sf::st_set_crs(westmed_sf, NA)
n_gsa <- nrow(westmed_planar)
adjacency_gaps <- numeric(0)
if (n_gsa > 1) {
  for (i in seq_len(n_gsa - 1)) {
    d <- suppressWarnings(as.numeric(sf::st_distance(westmed_planar[i, ], westmed_planar[(i + 1):n_gsa, ])))
    adjacency_gaps <- c(adjacency_gaps, d[d > 0 & d < 0.05])
  }
}
GAP_CLOSE_DIST <- if (length(adjacency_gaps) > 0) max(adjacency_gaps) / 2 * 1.2 else 0.001
message("[fig_WMed_basemap.R] Gap-closing buffer distance: ", signif(GAP_CLOSE_DIST, 3), " deg (",
        if (length(adjacency_gaps) > 0) {
          paste0("measured from ", length(adjacency_gaps), " candidate GSA-pair gap(s) under 0.05 deg;",
                 " worst real gap found = ", signif(max(adjacency_gaps), 3), " deg")
        } else {
          paste0("no measurable sub-0.05deg gaps found between GSA polygons - using fallback default;",
                 " if a seam line still shows up, check whether it's actually a real gap wider than 0.05 deg")
        },
        "). If a seam line is still visible after this, the real gap is wider than this measurement",
        " expects (e.g. it isn't between two ADJACENT GSA polygons in westmed_sf, but between a GSA",
        " and something outside westmed_sf entirely) - increase the 0.05 deg cutoff above to search",
        " further before concluding there's no real gap to measure.")

study_area_union <- st_union(westmed_sf)

## Positive buffer bridges every gap under GAP_CLOSE_DIST*2 wide (both
## sides now overlap across it and union into one piece); the matching
## negative buffer shrinks back by the same distance, undoing the
## outward growth everywhere except inside the now-closed gap(s).
study_area_clean <- sf::st_buffer(study_area_union, GAP_CLOSE_DIST)
study_area_clean <- sf::st_buffer(study_area_clean, -GAP_CLOSE_DIST)

## st_make_valid() is the standard repair for any self-intersecting
## "bowtie" ring the buffer round-trip might leave at a pinch point
## (splits it into its real separate lobes rather than silently
## misrepresenting the shape) - harmless no-op on an already-valid
## geometry. st_make_valid() under sf's default s2 (spherical) engine
## doesn't reliably repair this specific kind of invalidity (confirmed
## directly: it can leave st_is_valid() FALSE) - GEOS (s2 off, set
## above) repairs it properly.
study_area_clean <- sf::st_make_valid(study_area_clean)

## st_union() can return a single MULTIPOLYGON feature/row even when
## it's really several disconnected pieces - so which.max() over ROWS
## would never actually drop small disconnected slivers, only ever
## operate on the one row that exists. Casting to individual POLYGON
## parts first makes "keep only the largest piece" operate on the right
## unit (each disconnected part), same as originally intended.
study_area_parts <- suppressWarnings(sf::st_cast(sf::st_cast(study_area_clean, "MULTIPOLYGON"), "POLYGON"))

## Only RELATIVE area (which piece is bigger) is needed here, not a
## real-world km^2 figure - computed with the CRS stripped off first,
## so GEOS's plain planar (degree^2) area is used directly rather than
## needing the lwgeom package that geographic-CRS area under GEOS
## (s2 off) otherwise requires. Comparing degree^2 "areas" is perfectly
## fine for picking the biggest of a handful of GSA-scale polygon
## pieces - it would only distort real-world comparisons between
## regions at very different latitudes, which isn't the case here.
## study_area_parts is a bare geometry list (sfc), not a data.frame-like
## sf object - st_union(westmed_sf) drops the attribute columns by
## design (there's nothing meaningful to keep once every GSA is merged
## into one shape), and every step since has stayed sfc throughout
## rather than re-wrapping into sf. So it's indexed with a single-bracket
## sfc subscript here (no trailing comma/row-selector - that's data.frame
## indexing, and throws "incorrect number of dimensions" on a plain sfc).
part_areas <- as.numeric(sf::st_area(sf::st_set_crs(study_area_parts, NA)))
study_area_clean <- study_area_parts[which.max(part_areas)]
study_area_outline <- st_boundary(study_area_clean)
sf::sf_use_s2(s2_was_on)

## --- 8. Plot ----------------------------------------------------------------

bbox <- st_bbox(bind_rows(westmed_sf, context_sf))
pad  <- 0.7

p <- ggplot() +
  geom_sf(data = coast, fill = "bisque3", color = "grey70", linewidth = 0.2) +
  
  geom_sf(data = context_sf, fill = "grey85", color = "grey50", linewidth = 0.3, alpha = 0.5) +
  geom_text(data = context_sf, aes(x = label_x, y = label_y, label = context_label),
            size = 3, color = "grey40") +
  
  geom_sf(data = westmed_sf, aes(fill = division_label), color = "grey30", linewidth = 0.2, alpha = 0.65) +
  
  # clean outer Western Mediterranean contour
  geom_sf(data = study_area_outline, color = "black", linewidth = 0.8) +
  
  geom_sf(data = sicily_divide, color = "red", linewidth = 0.5, linetype = "dashed") +
  
  coord_sf(
    xlim = c(bbox["xmin"] - pad, bbox["xmax"] + pad),
    ylim = c(bbox["ymin"] - pad, bbox["ymax"] + pad)
  ) +
  
  geom_label(data = westmed_sf, aes(x = label_x, y = label_y, label = F_GSA_LIB),
             size = 3, fill = alpha("white", 0.7), label.size = 0.2) +
  
  scale_fill_brewer(palette = "Set2", name = "FAO Division") +
  
  labs(title = "Western Mediterranean Sea (FAO Subarea 37.1)", x = "Longitude", y = "Latitude") +
  
  theme_bw(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.ontop = TRUE,
    panel.background = element_rect(fill = NA),
    panel.grid.major = element_line(color = alpha("grey50", 0.5), linetype = "dashed", linewidth = 0.3),
    panel.grid.minor = element_line(color = alpha("grey60", 0.3), linetype = "dashed", linewidth = 0.2)
  )

print(p)

plots_dir <- file.path(out_dir, "plots")
dir.create(plots_dir, recursive = TRUE, showWarnings = FALSE)
map_out_path <- file.path(plots_dir, "westmed_gsa_map.png")
ggsave(map_out_path, p, width = 10, height = 8, dpi = 300)
message("Map saved to ", map_out_path)