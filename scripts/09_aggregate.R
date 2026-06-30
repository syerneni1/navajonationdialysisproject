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
library(ggspatial)
library(patchwork)
library(tigris)
library(writexl)

sf_use_s2(FALSE)
options(tigris_use_cache = TRUE)

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
cat("  Blocks (all):", nrow(blocks), "\n")

# Load original filtered blocks to get in_navajo flag
blocks_filtered <- st_read("data_processed/blocks_filtered.geojson", quiet = TRUE) %>%
  st_drop_geometry() %>%
  select(GEOID, in_navajo)

# Join to get in_navajo flag and filter to ONLY blocks >50% in Navajo Nation
blocks <- blocks %>%
  left_join(blocks_filtered, by = "GEOID") %>%
  filter(in_navajo == TRUE)

cat("  Blocks filtered to >50% in Navajo Nation:", nrow(blocks), "\n")

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
CACHE_FILE <- "data_processed/block_chapter_assignments.csv"

# Check if cached assignment exists
if (file.exists(CACHE_FILE)) {
  cat("Loading cached chapter assignments...\n")
  chapter_assignments <- read_csv(CACHE_FILE, show_col_types = FALSE)

  blocks <- blocks %>%
    left_join(chapter_assignments, by = "GEOID")

  cat("✓ Loaded cached assignments\n")
} else {
  cat("Assigning blocks to chapters using majority area (>50%)...\n")
  cat("  (This is slow - will be cached for future runs)\n\n")

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

  # Save cache
  cat("\n  Saving chapter assignments to cache...\n")
  blocks %>%
    st_drop_geometry() %>%
    select(GEOID, chapter) %>%
    write_csv(CACHE_FILE)
  cat("  ✓ Cache saved\n")
}

# Only keep blocks that were assigned to a chapter for aggregation
# NOTE: The cache only contains blocks that are >50% in Navajo Nation (from Step 4)
# Blocks outside chapters or in isochrones-only are excluded
blocks_in_chapters <- blocks %>% filter(!is.na(chapter))
blocks_outside <- blocks %>% filter(is.na(chapter))

cat("\n✓ Chapter assignment complete\n")
cat("  Blocks assigned to chapters (>50% in Navajo):", nrow(blocks_in_chapters), "\n")
cat("  Blocks excluded (outside chapters or isochrone-only):", nrow(blocks_outside), "\n")
cat("  IMPORTANT: Only blocks >50% in Navajo Nation are included in aggregation\n\n")

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

# Load state boundaries
cat("  Loading state boundaries...\n")
states <- states(cb = TRUE, progress_bar = FALSE) %>%
  filter(STUSPS %in% c("NM", "AZ", "UT", "CO")) %>%
  st_transform(st_crs(chapters))

# Define map extent
bbox_navajo <- st_bbox(isochrones)

# Unified theme for all plots
theme_map <- function() {
  theme_void(base_family = "Arial", base_size = 11) +
    theme(
      panel.background = element_rect(fill = "white", color = NA),
      plot.background = element_rect(fill = "white", color = NA),
      legend.position = "right",
      legend.justification = c(0, 1),
      legend.title = element_text(size = 11, face = "bold"),
      legend.text = element_text(size = 10),
      legend.key = element_rect(color = "white", linewidth = 0.5),
      legend.key.height = unit(0.7, "cm"),
      legend.key.width = unit(0.6, "cm"),
      plot.title = element_text(hjust = 0.5, face = "bold", size = 12, margin = margin(b = 10)),
      plot.margin = margin(5, 5, 5, 5),
      plot.tag = element_text(size = 12, face = "bold"),
      plot.tag.position = c(0.5, 0.02)
    )
}

# Consistent border styling
border_style <- list(
  color = "grey40",
  size = 0.2
)

cat("  Creating block-level maps...\n")

# Block-level P_k (use all blocks with pop > 0)
blocks_with_pop <- blocks_in_chapters %>% filter(block_pop > 0)

# Calculate state label positions WITHIN the Navajo map extent
# Use fixed positions that are visible on the map
state_labels <- data.frame(
  name = c("AZ", "NM", "UT", "CO"),
  X = c(-111.5, -107.0, -110.5, -107.5),  # Longitudes within bbox
  Y = c(35.0, 34.8, 37.8, 37.8)  # Latitudes within bbox, aligned
)

# Calculate demand per capita for blocks
blocks_with_pop <- blocks_with_pop %>%
  mutate(P_k_per_capita = P_k / block_pop)

# Calculate demand per capita for chapters and agencies
chapter_results <- chapter_results %>%
  mutate(P_per_capita = P_total / population)

agency_results <- agency_results %>%
  mutate(P_per_capita = P_total / population)

# Calculate unified scales for each metric across all three levels
# Demand per capita scale (chapter and agency only for final panel)
demand_range_chapter_agency <- c(
  floor(min(c(chapter_results$P_per_capita, agency_results$P_per_capita), na.rm = TRUE)),
  ceiling(max(c(chapter_results$P_per_capita, agency_results$P_per_capita), na.rm = TRUE))
)

# SPAI scale (all three levels: block, chapter, agency)
spai_max <- max(c(blocks_with_pop$A_k, chapter_results$SPAI_weighted, agency_results$SPAI_weighted), na.rm = TRUE)
spai_range <- c(
  0,
  ceiling(spai_max * 10000) / 10000
)

# SPAR scale (all three levels)
spar_max <- max(c(blocks_with_pop$SPAR_k, chapter_results$SPAR_weighted, agency_results$SPAR_weighted), na.rm = TRUE)
spar_range <- c(
  0,
  ceiling(spar_max * 10) / 10
)

cat("  Unified scale ranges:\n")
cat("    Demand per capita (chapter+agency):", demand_range_chapter_agency[1], "to", demand_range_chapter_agency[2], "\n")
cat("    SPAI (block+chapter+agency):", spai_range[1], "to", spai_range[2], "\n")
cat("    SPAR (block+chapter+agency):", spar_range[1], "to", spar_range[2], "\n\n")

# Create categorical bins for SPAR (unified across all three levels)
# 12 bins with 1.0 in the 0.9-1.2 bin
cat("  Creating categorical bins for discrete legends...\n")

# SPAR bins (12 total): 0.3 width, symmetric around 1.0
blocks_with_pop <- blocks_with_pop %>%
  mutate(SPAR_binned = case_when(
    SPAR_k >= 0 & SPAR_k < 0.3 ~ "0.0 to 0.3",
    SPAR_k >= 0.3 & SPAR_k < 0.6 ~ "0.3 to 0.6",
    SPAR_k >= 0.6 & SPAR_k < 0.9 ~ "0.6 to 0.9",
    SPAR_k >= 0.9 & SPAR_k < 1.2 ~ "0.9 to 1.2",
    SPAR_k >= 1.2 & SPAR_k < 1.5 ~ "1.2 to 1.5",
    SPAR_k >= 1.5 & SPAR_k < 1.8 ~ "1.5 to 1.8",
    SPAR_k >= 1.8 & SPAR_k < 2.1 ~ "1.8 to 2.1",
    SPAR_k >= 2.1 & SPAR_k < 2.4 ~ "2.1 to 2.4",
    SPAR_k >= 2.4 & SPAR_k < 2.7 ~ "2.4 to 2.7",
    SPAR_k >= 2.7 & SPAR_k < 3.0 ~ "2.7 to 3.0",
    SPAR_k >= 3.0 & SPAR_k < 3.3 ~ "3.0 to 3.3",
    SPAR_k >= 3.3 ~ "3.3 to 3.7",
    TRUE ~ "0.0 to 0.3"
  ))

blocks_with_pop$SPAR_binned <- factor(blocks_with_pop$SPAR_binned,
  levels = c("0.0 to 0.3", "0.3 to 0.6", "0.6 to 0.9", "0.9 to 1.2",
             "1.2 to 1.5", "1.5 to 1.8", "1.8 to 2.1", "2.1 to 2.4",
             "2.4 to 2.7", "2.7 to 3.0", "3.0 to 3.3", "3.3 to 3.7"))

chapter_results <- chapter_results %>%
  mutate(SPAR_binned = case_when(
    SPAR_weighted >= 0 & SPAR_weighted < 0.3 ~ "0.0 to 0.3",
    SPAR_weighted >= 0.3 & SPAR_weighted < 0.6 ~ "0.3 to 0.6",
    SPAR_weighted >= 0.6 & SPAR_weighted < 0.9 ~ "0.6 to 0.9",
    SPAR_weighted >= 0.9 & SPAR_weighted < 1.2 ~ "0.9 to 1.2",
    SPAR_weighted >= 1.2 & SPAR_weighted < 1.5 ~ "1.2 to 1.5",
    SPAR_weighted >= 1.5 & SPAR_weighted < 1.8 ~ "1.5 to 1.8",
    SPAR_weighted >= 1.8 & SPAR_weighted < 2.1 ~ "1.8 to 2.1",
    SPAR_weighted >= 2.1 & SPAR_weighted < 2.4 ~ "2.1 to 2.4",
    SPAR_weighted >= 2.4 & SPAR_weighted < 2.7 ~ "2.4 to 2.7",
    SPAR_weighted >= 2.7 & SPAR_weighted < 3.0 ~ "2.7 to 3.0",
    SPAR_weighted >= 3.0 & SPAR_weighted < 3.3 ~ "3.0 to 3.3",
    SPAR_weighted >= 3.3 ~ "3.3 to 3.7",
    TRUE ~ "0.0 to 0.3"
  ))

chapter_results$SPAR_binned <- factor(chapter_results$SPAR_binned,
  levels = c("0.0 to 0.3", "0.3 to 0.6", "0.6 to 0.9", "0.9 to 1.2",
             "1.2 to 1.5", "1.5 to 1.8", "1.8 to 2.1", "2.1 to 2.4",
             "2.4 to 2.7", "2.7 to 3.0", "3.0 to 3.3", "3.3 to 3.7"))

agency_results <- agency_results %>%
  mutate(SPAR_binned = case_when(
    SPAR_weighted >= 0 & SPAR_weighted < 0.3 ~ "0.0 to 0.3",
    SPAR_weighted >= 0.3 & SPAR_weighted < 0.6 ~ "0.3 to 0.6",
    SPAR_weighted >= 0.6 & SPAR_weighted < 0.9 ~ "0.6 to 0.9",
    SPAR_weighted >= 0.9 & SPAR_weighted < 1.2 ~ "0.9 to 1.2",
    SPAR_weighted >= 1.2 & SPAR_weighted < 1.5 ~ "1.2 to 1.5",
    SPAR_weighted >= 1.5 & SPAR_weighted < 1.8 ~ "1.5 to 1.8",
    SPAR_weighted >= 1.8 & SPAR_weighted < 2.1 ~ "1.8 to 2.1",
    SPAR_weighted >= 2.1 & SPAR_weighted < 2.4 ~ "2.1 to 2.4",
    SPAR_weighted >= 2.4 & SPAR_weighted < 2.7 ~ "2.4 to 2.7",
    SPAR_weighted >= 2.7 & SPAR_weighted < 3.0 ~ "2.7 to 3.0",
    SPAR_weighted >= 3.0 & SPAR_weighted < 3.3 ~ "3.0 to 3.3",
    SPAR_weighted >= 3.3 ~ "3.3 to 3.7",
    TRUE ~ "0.0 to 0.3"
  ))

agency_results$SPAR_binned <- factor(agency_results$SPAR_binned,
  levels = c("0.0 to 0.3", "0.3 to 0.6", "0.6 to 0.9", "0.9 to 1.2",
             "1.2 to 1.5", "1.5 to 1.8", "1.8 to 2.1", "2.1 to 2.4",
             "2.4 to 2.7", "2.7 to 3.0", "3.0 to 3.3", "3.3 to 3.7"))

# Demand per capita bins (7 bins, width ~3)
chapter_results <- chapter_results %>%
  mutate(Demand_binned = case_when(
    P_per_capita >= 11 & P_per_capita < 14 ~ "11 to 14",
    P_per_capita >= 14 & P_per_capita < 17 ~ "14 to 17",
    P_per_capita >= 17 & P_per_capita < 20 ~ "17 to 20",
    P_per_capita >= 20 & P_per_capita < 23 ~ "20 to 23",
    P_per_capita >= 23 & P_per_capita < 26 ~ "23 to 26",
    P_per_capita >= 26 & P_per_capita < 29 ~ "26 to 29",
    P_per_capita >= 29 ~ "29 to 32",
    TRUE ~ "11 to 14"
  ))

chapter_results$Demand_binned <- factor(chapter_results$Demand_binned,
  levels = c("11 to 14", "14 to 17", "17 to 20", "20 to 23",
             "23 to 26", "26 to 29", "29 to 32"))

agency_results <- agency_results %>%
  mutate(Demand_binned = case_when(
    P_per_capita >= 11 & P_per_capita < 14 ~ "11 to 14",
    P_per_capita >= 14 & P_per_capita < 17 ~ "14 to 17",
    P_per_capita >= 17 & P_per_capita < 20 ~ "17 to 20",
    P_per_capita >= 20 & P_per_capita < 23 ~ "20 to 23",
    P_per_capita >= 23 & P_per_capita < 26 ~ "23 to 26",
    P_per_capita >= 26 & P_per_capita < 29 ~ "26 to 29",
    P_per_capita >= 29 ~ "29 to 32",
    TRUE ~ "11 to 14"
  ))

agency_results$Demand_binned <- factor(agency_results$Demand_binned,
  levels = c("11 to 14", "14 to 17", "17 to 20", "20 to 23",
             "23 to 26", "26 to 29", "29 to 32"))

# SPAI bins (8 bins, scientific notation)
spai_max_val <- ceiling(spai_range[2] * 10000) / 10000
spai_bin_width <- spai_max_val / 8
blocks_with_pop <- blocks_with_pop %>%
  mutate(SPAI_binned = cut(A_k,
                           breaks = seq(0, spai_max_val, length.out = 9),
                           include.lowest = TRUE,
                           labels = paste(sprintf("%.1e", seq(0, spai_max_val, length.out = 9)[-9]),
                                        "to",
                                        sprintf("%.1e", seq(0, spai_max_val, length.out = 9)[-1]))))

chapter_results <- chapter_results %>%
  mutate(SPAI_binned = cut(SPAI_weighted,
                           breaks = seq(0, spai_max_val, length.out = 9),
                           include.lowest = TRUE,
                           labels = paste(sprintf("%.1e", seq(0, spai_max_val, length.out = 9)[-9]),
                                        "to",
                                        sprintf("%.1e", seq(0, spai_max_val, length.out = 9)[-1]))))

agency_results <- agency_results %>%
  mutate(SPAI_binned = cut(SPAI_weighted,
                           breaks = seq(0, spai_max_val, length.out = 9),
                           include.lowest = TRUE,
                           labels = paste(sprintf("%.1e", seq(0, spai_max_val, length.out = 9)[-9]),
                                        "to",
                                        sprintf("%.1e", seq(0, spai_max_val, length.out = 9)[-1]))))

# Unified color scales with discrete bins
# Demand: Manual discrete bins (7 bins) - gentler progression
demand_colors_unified <- scale_fill_manual(
  values = c("11 to 14" = "#ffffcc",
             "14 to 17" = "#ffeda0",
             "17 to 20" = "#fed976",
             "20 to 23" = "#feb24c",
             "23 to 26" = "#fd8d3c",
             "26 to 29" = "#fc4e2a",
             "29 to 32" = "#e31a1c"),
  name = "Demand per capita",
  drop = FALSE
)

# SPAI: Manual discrete bins (8 bins)
spai_max_val <- ceiling(spai_range[2] * 10000) / 10000
spai_bin_labels <- paste(sprintf("%.1e", seq(0, spai_max_val, length.out = 9)[-9]),
                        "to",
                        sprintf("%.1e", seq(0, spai_max_val, length.out = 9)[-1]))
spai_colors_unified <- scale_fill_manual(
  values = setNames(c("#f7fbff", "#deebf7", "#c6dbef", "#9ecae1",
                      "#6baed6", "#4292c6", "#2171b5", "#08519c"),
                    spai_bin_labels),
  name = "SPAI",
  drop = FALSE
)

# SPAR: Manual discrete bins (12 bins, symmetric around 1.0)
# Single unified scale for all three levels
spar_colors_unified <- scale_fill_manual(
  values = c("0.0 to 0.3" = "#a50026",
             "0.3 to 0.6" = "#d73027",
             "0.6 to 0.9" = "#f46d43",
             "0.9 to 1.2" = "#fdae61",
             "1.2 to 1.5" = "#fee08b",
             "1.5 to 1.8" = "#d9ef8b",
             "1.8 to 2.1" = "#a6d96a",
             "2.1 to 2.4" = "#66bd63",
             "2.4 to 2.7" = "#1a9850",
             "2.7 to 3.0" = "#006837",
             "3.0 to 3.3" = "#004529",
             "3.3 to 3.7" = "#00281a"),
  name = "E2SFCA SPAR",
  drop = FALSE
)

p_demand_block <- ggplot() +
  geom_sf(data = blocks_with_pop, aes(fill = P_k_per_capita), color = NA) +
  geom_sf(data = chapters, fill = NA, color = border_style$color, size = border_style$size) +
  demand_colors_unified +
  coord_sf(xlim = c(bbox_navajo["xmin"], bbox_navajo["xmax"]),
           ylim = c(bbox_navajo["ymin"], bbox_navajo["ymax"]), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.18, text_family = "Arial", text_cex = 0.7,
                  unit_category = "imperial", pad_x = unit(0.3, "cm"), pad_y = unit(0.3, "cm")) +
  annotation_scale(location = "bl", width_hint = 0.18, text_family = "Arial", text_cex = 0.7,
                  unit_category = "metric", pad_x = unit(0.3, "cm"), pad_y = unit(1.0, "cm")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                        style = north_arrow_orienteering(text_family = "Arial", text_size = 10),
                        height = unit(1.5, "cm"), width = unit(0.8, "cm"),
                        pad_x = unit(5.5, "cm"), pad_y = unit(0.65, "cm")) +
  theme_map()

# Block-level SPAI
p_spai_block <- ggplot() +
  geom_sf(data = blocks_with_pop, aes(fill = SPAI_binned), color = NA) +
  geom_sf(data = chapters, fill = NA, color = border_style$color, size = border_style$size) +
  spai_colors_unified +
  coord_sf(xlim = c(bbox_navajo["xmin"], bbox_navajo["xmax"]),
           ylim = c(bbox_navajo["ymin"], bbox_navajo["ymax"]), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.18, text_family = "Arial", text_cex = 0.7,
                  unit_category = "imperial", pad_x = unit(0.3, "cm"), pad_y = unit(0.3, "cm")) +
  annotation_scale(location = "bl", width_hint = 0.18, text_family = "Arial", text_cex = 0.7,
                  unit_category = "metric", pad_x = unit(0.3, "cm"), pad_y = unit(1.0, "cm")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                        style = north_arrow_orienteering(text_family = "Arial", text_size = 10),
                        height = unit(1.5, "cm"), width = unit(0.8, "cm"),
                        pad_x = unit(5.5, "cm"), pad_y = unit(0.65, "cm")) +
  theme_map()

# Block-level SPAR
p_spar_block <- ggplot() +
  geom_sf(data = blocks_with_pop, aes(fill = SPAR_binned), color = NA) +
  geom_sf(data = chapters, fill = NA, color = border_style$color, size = border_style$size) +
  spar_colors_unified +
  coord_sf(xlim = c(bbox_navajo["xmin"], bbox_navajo["xmax"]),
           ylim = c(bbox_navajo["ymin"], bbox_navajo["ymax"]), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.25, text_family = "Arial", text_cex = 0.7,
                  unit_category = "metric", pad_x = unit(0.3, "cm"), pad_y = unit(1.0, "cm")) +
  annotation_scale(location = "bl", width_hint = 0.25, text_family = "Arial", text_cex = 0.7,
                  unit_category = "imperial", pad_x = unit(0.3, "cm"), pad_y = unit(0.3, "cm")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                        style = north_arrow_orienteering(text_family = "Arial", text_size = 10),
                        height = unit(1.5, "cm"), width = unit(0.8, "cm"),
                        pad_x = unit(5.5, "cm"), pad_y = unit(0.65, "cm")) +
  theme_map()

cat("  Creating chapter-level maps...\n")

# Chapter-level P_k per capita
p_demand_chapter <- ggplot() +
  geom_sf(data = chapter_results, aes(fill = Demand_binned), color = border_style$color, size = border_style$size) +
  demand_colors_unified +
  coord_sf(xlim = c(bbox_navajo["xmin"], bbox_navajo["xmax"]),
           ylim = c(bbox_navajo["ymin"], bbox_navajo["ymax"]), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.18, text_family = "Arial", text_cex = 0.7,
                  unit_category = "imperial", pad_x = unit(0.3, "cm"), pad_y = unit(0.3, "cm")) +
  annotation_scale(location = "bl", width_hint = 0.18, text_family = "Arial", text_cex = 0.7,
                  unit_category = "metric", pad_x = unit(0.3, "cm"), pad_y = unit(1.0, "cm")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                        style = north_arrow_orienteering(text_family = "Arial", text_size = 10),
                        height = unit(1.5, "cm"), width = unit(0.8, "cm"),
                        pad_x = unit(5.5, "cm"), pad_y = unit(0.65, "cm")) +
  theme_map()

# Chapter-level SPAI
p_spai_chapter <- ggplot() +
  geom_sf(data = chapter_results, aes(fill = SPAI_binned), color = border_style$color, size = border_style$size) +
  spai_colors_unified +
  coord_sf(xlim = c(bbox_navajo["xmin"], bbox_navajo["xmax"]),
           ylim = c(bbox_navajo["ymin"], bbox_navajo["ymax"]), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.18, text_family = "Arial", text_cex = 0.7,
                  unit_category = "imperial", pad_x = unit(0.3, "cm"), pad_y = unit(0.3, "cm")) +
  annotation_scale(location = "bl", width_hint = 0.18, text_family = "Arial", text_cex = 0.7,
                  unit_category = "metric", pad_x = unit(0.3, "cm"), pad_y = unit(1.0, "cm")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                        style = north_arrow_orienteering(text_family = "Arial", text_size = 10),
                        height = unit(1.5, "cm"), width = unit(0.8, "cm"),
                        pad_x = unit(5.5, "cm"), pad_y = unit(0.65, "cm")) +
  theme_map()

# Chapter-level SPAR
p_spar_chapter <- ggplot() +
  geom_sf(data = chapter_results, aes(fill = SPAR_binned), color = border_style$color, size = border_style$size) +
  spar_colors_unified +
  coord_sf(xlim = c(bbox_navajo["xmin"], bbox_navajo["xmax"]),
           ylim = c(bbox_navajo["ymin"], bbox_navajo["ymax"]), expand = FALSE) +
  annotation_north_arrow(location = "bl", which_north = "true",
                        style = north_arrow_orienteering(text_family = "Arial", text_size = 10),
                        height = unit(1.5, "cm"), width = unit(0.8, "cm"),
                        pad_x = unit(5.5, "cm"), pad_y = unit(0.65, "cm")) +
  theme_map()

cat("  Creating agency-level maps...\n")

# Agency-level P_k per capita
p_demand_agency <- ggplot() +
  geom_sf(data = agency_results, aes(fill = Demand_binned), color = border_style$color, size = border_style$size) +
  demand_colors_unified +
  coord_sf(xlim = c(bbox_navajo["xmin"], bbox_navajo["xmax"]),
           ylim = c(bbox_navajo["ymin"], bbox_navajo["ymax"]), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.18, text_family = "Arial", text_cex = 0.7,
                  unit_category = "imperial", pad_x = unit(0.3, "cm"), pad_y = unit(0.3, "cm")) +
  annotation_scale(location = "bl", width_hint = 0.18, text_family = "Arial", text_cex = 0.7,
                  unit_category = "metric", pad_x = unit(0.3, "cm"), pad_y = unit(1.0, "cm")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                        style = north_arrow_orienteering(text_family = "Arial", text_size = 10),
                        height = unit(1.5, "cm"), width = unit(0.8, "cm"),
                        pad_x = unit(5.5, "cm"), pad_y = unit(0.65, "cm")) +
  theme_map()

# Agency-level SPAR
p_spar_agency <- ggplot() +
  geom_sf(data = agency_results, aes(fill = SPAR_binned), color = border_style$color, size = border_style$size) +
  spar_colors_unified +
  coord_sf(xlim = c(bbox_navajo["xmin"], bbox_navajo["xmax"]),
           ylim = c(bbox_navajo["ymin"], bbox_navajo["ymax"]), expand = FALSE) +
  annotation_north_arrow(location = "bl", which_north = "true",
                        style = north_arrow_orienteering(text_family = "Arial", text_size = 10),
                        height = unit(1.5, "cm"), width = unit(0.8, "cm"),
                        pad_x = unit(5.5, "cm"), pad_y = unit(0.65, "cm")) +
  theme_map()

# Agency-level SPAI
p_spai_agency <- ggplot() +
  geom_sf(data = agency_results, aes(fill = SPAI_binned), color = border_style$color, size = border_style$size) +
  spai_colors_unified +
  coord_sf(xlim = c(bbox_navajo["xmin"], bbox_navajo["xmax"]),
           ylim = c(bbox_navajo["ymin"], bbox_navajo["ymax"]), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.18, text_family = "Arial", text_cex = 0.7,
                  unit_category = "imperial", pad_x = unit(0.3, "cm"), pad_y = unit(0.3, "cm")) +
  annotation_scale(location = "bl", width_hint = 0.18, text_family = "Arial", text_cex = 0.7,
                  unit_category = "metric", pad_x = unit(0.3, "cm"), pad_y = unit(1.0, "cm")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                        style = north_arrow_orienteering(text_family = "Arial", text_size = 10),
                        height = unit(1.5, "cm"), width = unit(0.8, "cm"),
                        pad_x = unit(5.5, "cm"), pad_y = unit(0.65, "cm")) +
  theme_map()

cat("  Creating combined panels...\n")

# 1x2 Demand panel (chapter, agency only - not block)
p_demand_combined <- (p_demand_chapter | p_demand_agency) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Dialysis Demand per Capita",
    tag_levels = 'a',
    theme = theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14, margin = margin(b = 5, t = 5)),
      plot.tag = element_text(size = 12, face = "bold"),
      plot.tag.position = "bottom",
      plot.margin = margin(0, 0, 0, 0)
    )
  )

ggsave("outputs_maps/09_demand_combined.png", p_demand_combined, width = 18, height = 9, dpi = 300, bg = "white")
ggsave("outputs_maps/09_demand_combined.svg", p_demand_combined, width = 18, height = 9, bg = "white")

# 1x3 SPAI panel (block, chapter, agency)
p_spai_combined <- (p_spai_block | p_spai_chapter | p_spai_agency) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Spatial Access Index (SPAI)",
    tag_levels = 'a',
    theme = theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14, margin = margin(b = 5, t = 5)),
      plot.tag = element_text(size = 12, face = "bold"),
      plot.tag.position = "bottom",
      plot.margin = margin(0, 0, 0, 0)
    )
  )

ggsave("outputs_maps/09_spai_combined.png", p_spai_combined, width = 24, height = 9, dpi = 300, bg = "white")
ggsave("outputs_maps/09_spai_combined.svg", p_spai_combined, width = 24, height = 9, bg = "white")

# 1x3 SPAR panel (block, chapter, agency)
p_spar_combined <- (p_spar_block | p_spar_chapter | p_spar_agency) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Spatial Access Ratio (SPAR)",
    tag_levels = 'a',
    theme = theme(
      plot.title = element_text(hjust = 0.5, face = "bold", size = 14, margin = margin(b = 5, t = 5)),
      plot.tag = element_text(size = 12, face = "bold"),
      plot.tag.position = "bottom",
      plot.margin = margin(0, 0, 0, 0)
    )
  )

ggsave("outputs_maps/09_spar_combined.png", p_spar_combined, width = 24, height = 9, dpi = 300, bg = "white")
ggsave("outputs_maps/09_spar_combined.svg", p_spar_combined, width = 24, height = 9, bg = "white")

cat("  Creating facility catchment maps...\n")

# Calculate state label positions (centroids, adjusted to not overlap Navajo)
state_labels <- states %>%
  st_centroid() %>%
  st_coordinates() %>%
  as.data.frame() %>%
  mutate(name = states$NAME)

# Adjust label positions to align horizontally when possible
state_labels$Y[state_labels$name == "Arizona"] <- state_labels$Y[state_labels$name == "Arizona"] - 0.5
state_labels$Y[state_labels$name == "New Mexico"] <- state_labels$Y[state_labels$name == "Arizona"]  # Align with AZ
state_labels$Y[state_labels$name == "Utah"] <- state_labels$Y[state_labels$name == "Utah"] + 0.3
state_labels$Y[state_labels$name == "Colorado"] <- state_labels$Y[state_labels$name == "Utah"]  # Align with UT

# Isochrone map with F_d subscript
isochrone_colors_palette <- c("0-15 min" = "#1a9850", "15-30 min" = "#fdae61", "30-60 min" = "#d73027")

# Load facilities for F_d (those with isochrones)
facilities_for_map <- read_csv("data_processed/facilities_Fd.csv", show_col_types = FALSE) %>%
  filter(!is.na(latitude), !is.na(longitude))

cat("  F_d facilities on map:", nrow(facilities_for_map), "\n")
if (nrow(facilities_for_map) != 41) {
  warning("Expected 41 F_d facilities, found ", nrow(facilities_for_map))
}

# Convert to spatial
facilities_map_sf <- st_as_sf(facilities_for_map, coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(st_crs(chapters))

p_isochrones <- ggplot() +
  geom_sf(data = states, fill = "white", color = "grey60", size = 0.5) +
  geom_sf(data = chapters, fill = "grey95", color = border_style$color, size = border_style$size) +
  geom_sf(data = isochrones, aes(fill = band), alpha = 0.6, color = NA) +
  geom_sf(data = facilities_map_sf, color = "#0000FF", size = 2, shape = 16, alpha = 0.9) +
  scale_fill_manual(values = isochrone_colors_palette, name = "Travel Time",
                   guide = guide_legend(keyheight = unit(0.8, "cm"), keywidth = unit(0.5, "cm"))) +
  coord_sf(xlim = c(bbox_navajo["xmin"], bbox_navajo["xmax"]),
           ylim = c(bbox_navajo["ymin"], bbox_navajo["ymax"]), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.18, text_family = "Arial", text_cex = 0.8,
                  unit_category = "imperial", pad_x = unit(0.3, "cm"), pad_y = unit(0.3, "cm")) +
  annotation_scale(location = "bl", width_hint = 0.18, text_family = "Arial", text_cex = 0.8,
                  unit_category = "metric", pad_x = unit(0.3, "cm"), pad_y = unit(1.0, "cm")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                        style = north_arrow_orienteering(text_family = "Arial", text_size = 10),
                        height = unit(1.5, "cm"), width = unit(0.8, "cm"),
                        pad_x = unit(5.5, "cm"), pad_y = unit(0.65, "cm")) +
  labs(title = expression(F[d]~"Facility Isochrones")) +
  theme_map()

ggsave("outputs_maps/09_isochrones.png", p_isochrones, width = 10, height = 8, dpi = 300, bg = "white")
ggsave("outputs_maps/09_isochrones.svg", p_isochrones, width = 10, height = 8, bg = "white")

# Facility bubble map with R_j subscript
facilities_sf <- st_as_sf(facilities, coords = c("longitude", "latitude"), crs = 4326) %>%
  st_transform(st_crs(chapters))

# Create discrete bins for R_j (×10^4)
facilities_sf <- facilities_sf %>%
  mutate(
    R_j_scaled = R_j * 10000,
    R_j_binned = case_when(
      R_j_scaled >= 0 & R_j_scaled < 0.4 ~ "0.0 to 0.4",
      R_j_scaled >= 0.4 & R_j_scaled < 0.8 ~ "0.4 to 0.8",
      R_j_scaled >= 0.8 & R_j_scaled < 1.2 ~ "0.8 to 1.2",
      R_j_scaled >= 1.2 & R_j_scaled < 1.6 ~ "1.2 to 1.6",
      R_j_scaled >= 1.6 ~ "1.6 to 2.0",
      TRUE ~ "0.0 to 0.4"
    )
  )

facilities_sf$R_j_binned <- factor(facilities_sf$R_j_binned,
  levels = c("0.0 to 0.4", "0.4 to 0.8", "0.8 to 1.2", "1.2 to 1.6", "1.6 to 2.0"))

p_facilities <- ggplot() +
  geom_sf(data = states, fill = "white", color = "grey60", size = 0.5) +
  geom_sf(data = chapters, fill = "grey95", color = border_style$color, size = border_style$size) +
  geom_sf(data = facilities_sf, aes(size = dialysis_station_count, color = R_j_binned), alpha = 0.7) +
  scale_size_continuous(range = c(2, 12), name = "Stations\n(Supply)",
                       guide = guide_legend(override.aes = list(alpha = 1))) +
  scale_color_manual(
    values = c("0.0 to 0.4" = "#d73027",
               "0.4 to 0.8" = "#fc8d59",
               "0.8 to 1.2" = "#fee08b",
               "1.2 to 1.6" = "#ffffbf",
               "1.6 to 2.0" = "#d9ef8b"),
    name = expression(R[j]~"(×10"^4*")"),
    drop = FALSE,
    guide = guide_legend(override.aes = list(size = 5, alpha = 1))
  ) +
  coord_sf(xlim = c(bbox_navajo["xmin"], bbox_navajo["xmax"]),
           ylim = c(bbox_navajo["ymin"], bbox_navajo["ymax"]), expand = FALSE) +
  annotation_scale(location = "bl", width_hint = 0.25, text_family = "Arial", text_cex = 0.8,
                  unit_category = "metric", pad_x = unit(0.3, "cm"), pad_y = unit(1.0, "cm")) +
  annotation_scale(location = "bl", width_hint = 0.25, text_family = "Arial", text_cex = 0.8,
                  unit_category = "imperial", pad_x = unit(0.3, "cm"), pad_y = unit(0.3, "cm")) +
  annotation_north_arrow(location = "bl", which_north = "true",
                        style = north_arrow_orienteering(text_family = "Arial", text_size = 10),
                        height = unit(1.5, "cm"), width = unit(0.8, "cm"),
                        pad_x = unit(5.5, "cm"), pad_y = unit(0.65, "cm")) +
  labs(title = "Dialysis Facilities") +
  theme_map()

ggsave("outputs_maps/09_facilities_bubble.png", p_facilities, width = 10, height = 8, dpi = 300, bg = "white")
ggsave("outputs_maps/09_facilities_bubble.svg", p_facilities, width = 10, height = 8, bg = "white")

cat("\n✓ All R plots saved to outputs_maps/\n\n")

# ── 9. Excel Export ───────────────────────────────────────────────────────────
cat("Creating Excel tables...\n")

# Prepare chapter table
chapter_excel <- chapter_results %>%
  st_drop_geometry() %>%
  select(Chapter = NAME,
         `Absolute Demand` = P_total,
         `Demand per Capita` = P_per_capita,
         SPAR = SPAR_weighted,
         SPAI = SPAI_weighted,
         Population = population) %>%
  arrange(desc(SPAR))

# Prepare agency table
agency_excel <- agency_results %>%
  st_drop_geometry() %>%
  select(Agency = AGENCY,
         `Absolute Demand` = P_total,
         `Demand per Capita` = P_per_capita,
         SPAR = SPAR_weighted,
         SPAI = SPAI_weighted,
         Population = population) %>%
  arrange(desc(SPAR))

# Export to Excel with separate sheets
write_xlsx(
  list(
    "Chapter Summary" = chapter_excel,
    "Agency Summary" = agency_excel
  ),
  "outputs_tables/09_chapter_agency_summary.xlsx"
)

cat("✓ Excel file saved to: outputs_tables/09_chapter_agency_summary.xlsx\n\n")

# ── 10. Summary ───────────────────────────────────────────────────────────────
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
cat("  Excel Summary:\n")
cat("    outputs_tables/09_chapter_agency_summary.xlsx\n")
cat("  HTML Map:\n")
cat("   ", MAP_FILE, "\n")
cat("  R Plots (PNG):\n")
cat("    outputs_maps/09_demand_combined.png (1x2 panel: chapter, agency)\n")
cat("    outputs_maps/09_spai_combined.png (1x3 panel: block, chapter, agency)\n")
cat("    outputs_maps/09_spar_combined.png (1x3 panel: block, chapter, agency)\n")
cat("    outputs_maps/09_isochrones.png\n")
cat("    outputs_maps/09_facilities_bubble.png\n")
cat("───────────────────────────────────────────────────────────────────────\n")
