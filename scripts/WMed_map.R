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
# print(st_drop_geometry(gsa_sf) %>% select(SMU_CODE, SMU_NAME, F_DIVISION))

## SMU_CODE = numeric GSA id. NOTE: Western/Eastern Sardinia are stored as
## SMU_CODE 111 / 112 (not 11.1 / 11.2), so they need a manual bucket back
## into GSA "11" for filtering/coloring purposes.
## F_DIVISION = FAO statistical division code (37.1.1 Balearic, 37.1.2 Gulf
## of Lion, 37.1.3 Sardinia - these three divisions together = Subarea 37.1,
## the official "Western Mediterranean")
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
  filter(gsa_group %in% context_ids)

if (nrow(westmed_sf) == 0) {
  stop("No polygons matched GSA 1-11 - check gsa_sf$gsa_num values (should be 1-30).")
}

## --- 4. Sicily Channel divide (western/central Med boundary) -----------

## Standard oceanographic boundary between the western and central/eastern
## Mediterranean basins (IHO "Limits of Oceans and Seas"): a line from
## Cape Bon, Tunisia to Cape Lilibeo (Marsala), Sicily. This is the line
## that runs through GSA 12 (Northern Tunisia) and GSA 16 (South of Sicily)
## rather than following the clean GSA polygon edges.
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
  geom_sf_text(data = context_sf, aes(label = gsa_group), size = 3, color = "grey40") +
  geom_sf(data = westmed_sf, aes(fill = division_label), color = "grey30",
          linewidth = 0.2, alpha = 0.65) +
  geom_sf_label(data = westmed_sf, aes(label = F_GSA_LIB), size = 3, fill = "white",
                label.size = 0.2, alpha = 0.9) +
  geom_sf(data = sicily_divide, color = "red", linewidth = 0.5, linetype = "dashed") +
  coord_sf(
    xlim = c(bbox["xmin"] - pad, bbox["xmax"] + pad),
    ylim = c(bbox["ymin"] - pad, bbox["ymax"] + pad)
  ) +
  scale_fill_brewer(palette = "Set2", name = "FAO Division") +
  labs(title = "Western Mediterranean (FAO Subarea 37.1)")+
       #subtitle = "GSA 1-11; bold line = Sicily Channel divide (Cape Bon - Cape Lilibeo),\nthe conventional western/central Mediterranean boundary, running through GSA 12 and GSA 16") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "bottom",
        panel.grid = element_line(color = "grey85"))

suppressWarnings(print(p))

if (!dir.exists("plots")) dir.create("plots")
suppressWarnings(ggsave("plots/westmed_gsa_map.png", p, width = 10, height = 8, dpi = 150))

message("Map saved to plots/westmed_gsa_map.png")

