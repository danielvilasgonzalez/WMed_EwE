## ---------------------------------------------------------------
## Map of the Western Mediterranean (FAO Major Fishing Area 37,
## Subarea 37.1 = GSA 1-11), with the Sicily Channel divide
## (Cape Bon - Cape Lilibeo) marking the separation from the
## Central Mediterranean, passing through GSA 12 and GSA 16
## ---------------------------------------------------------------

## --- 0. Packages -------------------------------------------------------

pkgs <- c("sf", "ggplot2", "dplyr", "stringr", "rnaturalearth", "rnaturalearthdata")
new_pkgs <- pkgs[!pkgs %in% installed.packages()[, "Package"]]
if (length(new_pkgs) > 0) install.packages(new_pkgs)
invisible(lapply(pkgs, library, character.only = TRUE))

# High-resolution coastline (10m) - needed so islands (Balearics, Corsica,
# Sardinia, Sicily) actually render instead of being dropped at coarser scales.
# Not on CRAN by default; installed from the ROpenSci r-universe.
if (!requireNamespace("rnaturalearthhires", quietly = TRUE)) {
  install.packages("rnaturalearthhires", repos = "https://ropensci.r-universe.dev", type = "source")
}

## --- 1. Download & unzip official GFCM GSA shapefile -------------------

zip_url  <- "https://gfcmsitestorage.blob.core.windows.net/website/5.Data/ArcGIS/GFCM_GSA.zip"
zip_file <- "GFCM_GSA.zip"
shp_dir  <- "GFCM_GSA_shp"

if (!file.exists(zip_file)) {
  download.file(zip_url, destfile = zip_file, mode = "wb", method = "libcurl")
}
if (!dir.exists(shp_dir)) dir.create(shp_dir)
unzip(zip_file, exdir = shp_dir)

shp_path <- list.files(shp_dir, pattern = "\\.shp$", full.names = TRUE, recursive = TRUE)[1]
gsa_sf <- st_read(shp_path, quiet = TRUE)   # native CRS: WGS84 (EPSG:4326)

## --- 2. Inspect attributes (uncomment to check column names/values) ----

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

## --- 3. Split into Western Med (target) vs Central Med (context) -------

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

## Label placement: st_point_on_surface only guarantees a point INSIDE the
## polygon, not one far from the edges - for narrow, coastline-hugging GSAs
## (1, 4, 12 especially) that point can still land right next to land.
## The correct tool is the "pole of inaccessibility" (the point maximizing
## distance from any edge) - the same technique Mapbox and other map
## labeling tools use. polylabelr implements this.
if (!requireNamespace("polylabelr", quietly = TRUE)) install.packages("polylabelr")
library(polylabelr)

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
        select(context_label, label_x, label_y, bbox_ymin, bbox_ymax))

## Manual overrides - if a specific label still looks wrong after the
## automatic placement (common for irregular/multi-part shapes where
## "farthest from any edge" isn't the same as "where you'd expect it"),
## add a row here to nudge it. Matches on the label text shown on the map.
label_overrides <- tibble::tribble(
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

## --- 4. Sicily Channel divide (western/central Med boundary) -----------

sicily_divide <- st_sfc(
  st_linestring(matrix(c(
    11.0308, 37.0708,   # Cape Bon, Tunisia
    12.4333, 37.8000    # Cape Lilibeo / Marsala, Sicily
  ), ncol = 2, byrow = TRUE)),
  crs = 4326
)

## --- 5. Basemap (coastline) ----------------------------------------------

coast <- ne_countries(scale = 10, returnclass = "sf")

## --- 6. Plot -------------------------------------------------------------

bbox <- st_bbox(bind_rows(westmed_sf, context_sf))
pad  <- 0.7   # degrees

p <- ggplot() +
  geom_sf(data = coast, fill = "grey92", color = "grey70", linewidth = 0.2) +
  geom_sf(data = context_sf, fill = "grey85", color = "grey50",
          linewidth = 0.3, alpha = 0.5) +
  geom_text(data = context_sf, aes(x = label_x, y = label_y, label = context_label),
            size = 3, color = "grey40") +
  geom_sf(data = westmed_sf, aes(fill = division_label), color = "grey30",
          linewidth = 0.2, alpha = 0.65) +
  geom_label(data = westmed_sf, aes(x = label_x, y = label_y, label = F_GSA_LIB),
             size = 3, fill = scales::alpha("white", 0.6), label.size = 0.2) +
  geom_sf(data = sicily_divide, color = "red", linewidth = 0.5, linetype = "dashed") +
  coord_sf(
    xlim = c(bbox["xmin"] - pad, bbox["xmax"] + pad),
    ylim = c(bbox["ymin"] - pad, bbox["ymax"] + pad)
  ) +
  scale_fill_brewer(palette = "Set2", name = "FAO Division") +
  labs(title = "Western Mediterranean (FAO Subarea 37.1)") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        panel.grid = element_line(color = "grey85"))

print(p)

if (!dir.exists("plots")) dir.create("plots")
ggsave("plots/westmed_gsa_map.png", p, width = 10, height = 8, dpi = 150)

message("Map saved to plots/westmed_gsa_map.png")