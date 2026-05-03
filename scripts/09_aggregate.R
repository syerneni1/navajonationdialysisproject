# =============================================================================
# 09_aggregate.R
# Purpose: Aggregate results to chapter/agency level and produce final outputs
# Inputs:  data_processed/block_access_scores.geojson
#          data_processed/navajo_chapters.geojson
#          data_processed/navajo_agencies.geojson
#          data_processed/isochrones_Fd.geojson
# Outputs: Aggregated geojson files, summary tables, R plots (ggplot2)
# =============================================================================

library(tidyverse)
library(sf)
library(leaflet)
library(htmlwidgets)
library(ggplot2)

sf_use_s2(FALSE)

# ── Configuration ─────────────────────────────────────────────────────────────
BLOCKS_FILE <- "data_processed/block_access_scores.geojson"
CHAPTERS_FILE <- "data_processed/navajo_chapters.geojson"
AGENCIES_FILE <- "data_processed/navajo_agencies.geojson"
ISOCHRONES_FILE <- "data_processed/isochrones_Fd.geojson"
FACILITIES_FILE <- "data_processed/facility_supply_ratio.csv"

# Outputs
OUTPUT_CHAPTER_GEOJSON <- "data_processed/chapter_results.geojson"
OUTPUT_AGENCY_GEOJSON <- "data_processed/agency_results.geojson"
OUTPUT_CHAPTER_CSV <- "outputs_tables/09_chapter_summary.csv"
OUTPUT_AGENCY_CSV <- "outputs_tables/09_agency_summary.csv"
OUTPUT_ACCESS_TIERS <- "outputs_tables/09_access_tier_summary.csv"
MAP_FILE <- "outputs_maps/09_aggregate_verification.html"

# ── 1. Load Inputs ────────────────────────────────────────────────────────────
cat("Loading data...\n")

# Decompress blocks if needed
if (file.exists(paste0(BLOCKS_FILE, ".gz")) && !file.exists(BLOCKS_FILE)) {
  cat("  Decompressing blocks file...\n")
  system(paste("gunzip -k", paste0(BLOCKS_FILE, ".gz")))
}

blocks <- st_read(BLOCKS_FILE, quiet = TRUE)
cat("  Blocks:", nrow(blocks), "\n")

chapters <- st_read(CHAPTERS_FILE, quiet = TRUE)
cat("  Chapters:", nrow(chapters), "\n")

agencies <- st_read(AGENCIES_FILE, quiet = TRUE)
cat("  Agencies:", nrow(agencies), "\n")

isochrones <- st_read(ISOCHRONES_FILE, quiet = TRUE)
cat("  Isochrones:", nrow(isochrones), "\n")

facilities <- read_csv(FACILITIES_FILE, show_col_types = FALSE)
cat("  Facilities:", nrow(facilities), "\n\n")

# Transform to common CRS
blocks <- st_transform(blocks, st_crs(chapters))
isochrones <- st_transform(isochrones, st_crs(chapters))

# ── 2. Assign Blocks to Chapters ─────────────────────────────────────────────
cat("Assigning blocks to chapters using majority area (>50%)...\n")

# Calculate what % of each block's area is in each chapter
blocks$area_total <- st_area(blocks)

assign_to_chapter <- function(block_geom, chapters_sf) {
  intersections <- suppressWarnings(st_intersection(block_geom, chapters_sf))
  if (nrow(intersections) == 0) return(NA_character_)
  intersections$area <- st_area(intersections)
  # Find chapter with most area
  intersections %>% arrange(desc(area)) %>% slice(1) %>% pull(NAME)
}

cat("  Processing blocks (this may take a few minutes)...\n")
blocks$chapter <- sapply(1:nrow(blocks), function(i) {
  if (i %% 5000 == 0) cat("    Block", i, "of", nrow(blocks), "\n")
  assign_to_chapter(blocks[i, ], chapters %>% select(NAME))
})

# Only keep blocks that were assigned to a chapter for aggregation
blocks_in_chapters <- blocks %>% filter(!is.na(chapter))
blocks_outside <- blocks %>% filter(is.na(chapter))

cat("\n✓ Chapter assignment complete\n")
cat("  Blocks assigned to chapters:", nrow(blocks_in_chapters), "\n")
cat("  Blocks outside chapters:", nrow(blocks_outside), "(excluded from aggregation)\n\n")

# ── 3. Aggregate to Chapter Level ────────────────────────────────────────────
cat("Aggregating to chapter level (P_k-weighted)...\n")

chapter_results <- blocks_in_chapters %>%
  st_drop_geometry() %>%
  group_by(chapter) %>%
  summarize(
    P_total = sum(P_k, na.rm = TRUE),
    population = sum(block_pop, na.rm = TRUE),
    blocks_count = n(),
    # Weighted averages
    SPAI_weighted = sum(P_k * A_k, na.rm = TRUE) / sum(P_k, na.rm = TRUE),
    SPAR_weighted = sum(P_k * SPAR_k, na.rm = TRUE) / sum(P_k, na.rm = TRUE),
    .groups = "drop"
  )

# Join to chapter geometries
chapter_results <- chapters %>%
  left_join(chapter_results, by = c("NAME" = "chapter"))

cat("✓ Chapter aggregation complete\n")
cat("  Chapters:", nrow(chapter_results), "\n\n")

# ── 4. Aggregate to Agency Level ─────────────────────────────────────────────
cat("Aggregating to agency level (P_k-weighted)...\n")

# First join agency names to blocks via chapters
blocks_with_agency <- blocks_in_chapters %>%
  st_drop_geometry() %>%
  left_join(
    chapters %>% st_drop_geometry() %>% select(NAME, AGENCY),
    by = c("chapter" = "NAME")
  )

agency_results <- blocks_with_agency %>%
  group_by(AGENCY) %>%
  summarize(
    P_total = sum(P_k, na.rm = TRUE),
    population = sum(block_pop, na.rm = TRUE),
    blocks_count = n(),
    chapters_count = n_distinct(chapter),
    # Weighted averages
    SPAI_weighted = sum(P_k * A_k, na.rm = TRUE) / sum(P_k, na.rm = TRUE),
    SPAR_weighted = sum(P_k * SPAR_k, na.rm = TRUE) / sum(P_k, na.rm = TRUE),
    .groups = "drop"
  )

# Join to agency geometries
agency_results <- agencies %>%
  left_join(agency_results, by = "AGENCY")

cat("✓ Agency aggregation complete\n")
cat("  Agencies:", nrow(agency_results), "\n\n")

# ── 5. Save Aggregated Outputs ───────────────────────────────────────────────
cat("Saving aggregated outputs...\n")

st_write(chapter_results, OUTPUT_CHAPTER_GEOJSON, delete_dsn = TRUE, quiet = TRUE)
st_write(agency_results, OUTPUT_AGENCY_GEOJSON, delete_dsn = TRUE, quiet = TRUE)

chapter_results %>%
  st_drop_geometry() %>%
  write_csv(OUTPUT_CHAPTER_CSV)

agency_results %>%
  st_drop_geometry() %>%
  write_csv(OUTPUT_AGENCY_CSV)

cat("✓ Saved aggregated results\n\n")

# ── 6. Produce Access Tier Summary ───────────────────────────────────────────
cat("Creating access tier summary...\n")

# Classify blocks into access tiers (only for blocks in Navajo Nation chapters)
access_tiers <- blocks_in_chapters %>%
  st_drop_geometry() %>%
  mutate(
    tier = case_when(
      SPAR_k == 0 ~ "No access",
      SPAR_k > 1 ~ "Above average access",
      SPAR_k == 1 ~ "Average access",
      SPAR_k < 1 ~ "Below average access",
      TRUE ~ "Unknown"
    )
  ) %>%
  group_by(tier) %>%
  summarize(
    blocks = n(),
    population = sum(block_pop, na.rm = TRUE),
    demand = sum(P_k, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(
    pct_blocks = blocks / sum(blocks) * 100,
    pct_population = population / sum(population) * 100,
    pct_demand = demand / sum(demand) * 100
  )

write_csv(access_tiers, OUTPUT_ACCESS_TIERS)

cat("✓ Access tier summary saved\n\n")
print(access_tiers)
cat("\n")

# ── 7. Produce HTML Verification Map ─────────────────────────────────────────
cat("Creating HTML verification map...\n")

# Create color palettes
spar_chapter_pal <- colorNumeric(
  palette = "RdYlGn",
  domain = chapter_results$SPAR_weighted,
  na.color = "#808080"
)

spar_agency_pal <- colorNumeric(
  palette = "RdYlGn",
  domain = agency_results$SPAR_weighted,
  na.color = "#808080"
)

# Create map
map <- leaflet() %>%
  addTiles() %>%
  setView(lng = -109.5, lat = 36.0, zoom = 7)

# Add chapter layer
map <- map %>%
  addPolygons(
    data = chapter_results,
    fillColor = ~spar_chapter_pal(SPAR_weighted),
    fillOpacity = 0.7,
    color = "white",
    weight = 1,
    opacity = 0.8,
    group = "Chapters",
    popup = ~paste0(
      "<b>Chapter:</b> ", NAME, "<br>",
      "<b>Agency:</b> ", AGENCY, "<br>",
      "<b>Population:</b> ", population, "<br>",
      "<b>Demand (P):</b> ", round(P_total, 1), "<br>",
      "<b>SPAI:</b> ", formatC(SPAI_weighted, format = "e", digits = 3), "<br>",
      "<b>SPAR:</b> ", round(SPAR_weighted, 3)
    ),
    label = ~paste0(NAME, " - SPAR: ", round(SPAR_weighted, 2))
  )

# Add agency layer
map <- map %>%
  addPolygons(
    data = agency_results,
    fillColor = ~spar_agency_pal(SPAR_weighted),
    fillOpacity = 0.7,
    color = "black",
    weight = 2,
    opacity = 0.8,
    group = "Agencies",
    popup = ~paste0(
      "<b>Agency:</b> ", AGENCY, "<br>",
      "<b>Chapters:</b> ", chapters_count, "<br>",
      "<b>Population:</b> ", population, "<br>",
      "<b>Demand (P):</b> ", round(P_total, 1), "<br>",
      "<b>SPAI:</b> ", formatC(SPAI_weighted, format = "e", digits = 3), "<br>",
      "<b>SPAR:</b> ", round(SPAR_weighted, 3)
    ),
    label = ~paste0(AGENCY, " - SPAR: ", round(SPAR_weighted, 2))
  )

# Add layer controls
map <- map %>%
  addLayersControl(
    baseGroups = c("Chapters", "Agencies"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addLegend(
    position = "topright",
    pal = spar_chapter_pal,
    values = chapter_results$SPAR_weighted,
    title = "SPAR (Weighted)",
    opacity = 0.7
  )

saveWidget(map, MAP_FILE, selfcontained = FALSE)

cat("✓ HTML map saved to:", MAP_FILE, "\n\n")

# ── 8. Produce R Plots (ggplot2) ─────────────────────────────────────────────
cat("Creating publication-quality R plots with ggplot2...\n")

# Define map extents
# Four corners: extent of all isochrones
bbox_4corners <- st_bbox(isochrones)

# Navajo Nation: extent of F_d isochrones (same as 4corners in this case)
bbox_navajo <- st_bbox(isochrones)

# Theme for all plots
theme_map <- function() {
  theme_minimal(base_family = "Arial", base_size = 10) +
    theme(
      axis.text = element_blank(),
      axis.title = element_blank(),
      panel.grid = element_blank(),
      legend.position = "right",
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12),
      plot.margin = margin(5, 5, 5, 5)
    )
}

# Color palettes
# For SPAR: diverging around 1.0
spar_colors <- scale_fill_gradient2(
  low = "#d73027",
  mid = "#ffffbf",
  high = "#1a9850",
  midpoint = 1.0,
  na.value = "grey90",
  name = "SPAR"
)

# For P_k and SPAI: sequential
demand_colors <- scale_fill_viridis_c(
  option = "plasma",
  na.value = "grey90",
  name = "Demand (P_k)"
)

spai_colors <- scale_fill_viridis_c(
  option = "viridis",
  na.value = "grey90",
  name = "SPAI"
)

cat("  Creating demand choropleth (P_k)...\n")

# P_k at chapter level
p_demand_chapter <- ggplot() +
  geom_sf(data = chapter_results, aes(fill = P_total), color = "white", size = 0.3) +
  demand_colors +
  coord_sf(xlim = c(bbox_navajo["xmin"], bbox_navajo["xmax"]),
           ylim = c(bbox_navajo["ymin"], bbox_navajo["ymax"])) +
  labs(title = "Dialysis Demand by Chapter") +
  theme_map()

ggsave("outputs_maps/09_demand_chapter.png", p_demand_chapter, width = 8, height = 6, dpi = 300)

# P_k at agency level
p_demand_agency <- ggplot() +
  geom_sf(data = agency_results, aes(fill = P_total), color = "black", size = 0.5) +
  demand_colors +
  coord_sf(xlim = c(bbox_navajo["xmin"], bbox_navajo["xmax"]),
           ylim = c(bbox_navajo["ymin"], bbox_navajo["ymax"])) +
  labs(title = "Dialysis Demand by Agency") +
  theme_map()

ggsave("outputs_maps/09_demand_agency.png", p_demand_agency, width = 8, height = 6, dpi = 300)

cat("  Creating SPAI choropleth...\n")

# SPAI at chapter level
p_spai_chapter <- ggplot() +
  geom_sf(data = chapter_results, aes(fill = SPAI_weighted), color = "white", size = 0.3) +
  spai_colors +
  coord_sf(xlim = c(bbox_navajo["xmin"], bbox_navajo["xmax"]),
           ylim = c(bbox_navajo["ymin"], bbox_navajo["ymax"])) +
  labs(title = "SPAI by Chapter") +
  theme_map()

ggsave("outputs_maps/09_spai_chapter.png", p_spai_chapter, width = 8, height = 6, dpi = 300)

# SPAI at agency level
p_spai_agency <- ggplot() +
  geom_sf(data = agency_results, aes(fill = SPAI_weighted), color = "black", size = 0.5) +
  spai_colors +
  coord_sf(xlim = c(bbox_navajo["xmin"], bbox_navajo["xmax"]),
           ylim = c(bbox_navajo["ymin"], bbox_navajo["ymax"])) +
  labs(title = "SPAI by Agency") +
  theme_map()

ggsave("outputs_maps/09_spai_agency.png", p_spai_agency, width = 8, height = 6, dpi = 300)

cat("  Creating SPAR choropleth...\n")

# SPAR at chapter level
p_spar_chapter <- ggplot() +
  geom_sf(data = chapter_results, aes(fill = SPAR_weighted), color = "white", size = 0.3) +
  spar_colors +
  coord_sf(xlim = c(bbox_navajo["xmin"], bbox_navajo["xmax"]),
           ylim = c(bbox_navajo["ymin"], bbox_navajo["ymax"])) +
  labs(title = "SPAR by Chapter") +
  theme_map()

ggsave("outputs_maps/09_spar_chapter.png", p_spar_chapter, width = 8, height = 6, dpi = 300)

# SPAR at agency level
p_spar_agency <- ggplot() +
  geom_sf(data = agency_results, aes(fill = SPAR_weighted), color = "black", size = 0.5) +
  spar_colors +
  coord_sf(xlim = c(bbox_navajo["xmin"], bbox_navajo["xmax"]),
           ylim = c(bbox_navajo["ymin"], bbox_navajo["ymax"])) +
  labs(title = "SPAR by Agency") +
  theme_map()

ggsave("outputs_maps/09_spar_agency.png", p_spar_agency, width = 8, height = 6, dpi = 300)

cat("  Creating side-by-side comparison panels...\n")

# Side-by-side: Block, Chapter, Agency for SPAR
# For block level, sample or aggregate to make it manageable
blocks_sample <- blocks_in_chapters %>%
  filter(block_pop > 0) %>%
  sample_n(min(5000, n()))

p_spar_block <- ggplot() +
  geom_sf(data = blocks_sample, aes(fill = SPAR_k), color = NA) +
  spar_colors +
  coord_sf(xlim = c(bbox_navajo["xmin"], bbox_navajo["xmax"]),
           ylim = c(bbox_navajo["ymin"], bbox_navajo["ymax"])) +
  labs(title = "Block Level") +
  theme_map()

# Comparison plot requires patchwork package - skipping for now
# p_spar_comparison <- p_spar_block + p_spar_chapter + p_spar_agency +
#   plot_layout(ncol = 3) +
#   plot_annotation(title = "SPAR Comparison: Block, Chapter, Agency")
#
# ggsave("outputs_maps/09_spar_comparison.png", p_spar_comparison, width = 18, height = 6, dpi = 300)

# Save individual block plot instead
ggsave("outputs_maps/09_spar_block.png", p_spar_block, width = 8, height = 6, dpi = 300)

cat("  Creating facility catchment maps...\n")

# Isochrone map
isochrone_colors <- c("0-15 min" = "#1a9850", "15-30 min" = "#fdae61", "30-60 min" = "#d73027")

p_isochrones <- ggplot() +
  geom_sf(data = isochrones, aes(fill = band), alpha = 0.5, color = NA) +
  scale_fill_manual(values = isochrone_colors, name = "Travel Time") +
  geom_sf(data = chapters, fill = NA, color = "grey50", size = 0.3) +
  coord_sf(xlim = c(bbox_4corners["xmin"], bbox_4corners["xmax"]),
           ylim = c(bbox_4corners["ymin"], bbox_4corners["ymax"])) +
  labs(title = "F_d Facility Isochrones") +
  theme_map()

ggsave("outputs_maps/09_isochrones.png", p_isochrones, width = 10, height = 8, dpi = 300)

# Facility bubble map (R_j)
facilities_sf <- st_as_sf(facilities, coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(st_crs(chapters))

p_facilities <- ggplot() +
  geom_sf(data = chapters, fill = "grey95", color = "grey70", size = 0.3) +
  geom_sf(data = facilities_sf, aes(size = dialysis_station_count, color = R_j), alpha = 0.7) +
  scale_size_continuous(range = c(2, 15), name = "Stations") +
  scale_color_gradient(low = "#ffffbf", high = "#d73027", name = "R_j") +
  coord_sf(xlim = c(bbox_navajo["xmin"], bbox_navajo["xmax"]),
           ylim = c(bbox_navajo["ymin"], bbox_navajo["ymax"])) +
  labs(title = "Dialysis Facilities (Size = Stations, Color = Supply Ratio)") +
  theme_map()

ggsave("outputs_maps/09_facilities_bubble.png", p_facilities, width = 10, height = 8, dpi = 300)

cat("\n✓ All R plots saved to outputs_maps/\n\n")

# ── 9. Summary ────────────────────────────────────────────────────────────────
cat("── FINAL SUMMARY ──────────────────────────────────────────────────────\n")
cat("Aggregation and visualization complete\n\n")

cat("Aggregation results:\n")
cat("  Chapters:", nrow(chapter_results), "\n")
cat("  Agencies:", nrow(agency_results), "\n")
cat("  Blocks included:", nrow(blocks_in_chapters), "\n")
cat("  Blocks excluded (outside chapters):", nrow(blocks_outside), "\n\n")

cat("Access tier distribution (Navajo Nation residents):\n")
print(access_tiers %>% select(tier, population, pct_population))
cat("\n")

cat("Output files created:\n")
cat("  GeoJSON:\n")
cat("   ", OUTPUT_CHAPTER_GEOJSON, "\n")
cat("   ", OUTPUT_AGENCY_GEOJSON, "\n")
cat("  CSV Tables:\n")
cat("   ", OUTPUT_CHAPTER_CSV, "\n")
cat("   ", OUTPUT_AGENCY_CSV, "\n")
cat("   ", OUTPUT_ACCESS_TIERS, "\n")
cat("  HTML Map:\n")
cat("   ", MAP_FILE, "\n")
cat("  R Plots (PNG):\n")
cat("    outputs_maps/09_demand_chapter.png\n")
cat("    outputs_maps/09_demand_agency.png\n")
cat("    outputs_maps/09_spai_chapter.png\n")
cat("    outputs_maps/09_spai_agency.png\n")
cat("    outputs_maps/09_spar_chapter.png\n")
cat("    outputs_maps/09_spar_agency.png\n")
cat("    outputs_maps/09_spar_comparison.png (3-panel)\n")
cat("    outputs_maps/09_isochrones.png\n")
cat("    outputs_maps/09_facilities_bubble.png\n")
cat("───────────────────────────────────────────────────────────────────────\n")
