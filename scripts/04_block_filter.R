# =============================================================================
# 04_block_filter.R
# Purpose: Download census blocks for 4-state region and filter to study area
#          to define final spatial population set K
# Inputs:  data_processed/navajo_nation.geojson
#          data_processed/isochrones_Fd.geojson
# Outputs: data_processed/blocks_filtered.geojson (FULL DETAIL - used in analysis)
#          outputs_maps/04_blocks_verification.html (simplified for display only)
# =============================================================================

library(tidyverse)
library(sf)
library(tidycensus)
library(leaflet)
library(htmlwidgets)

# Use GEOS for geometry operations
sf_use_s2(FALSE)

# Enable caching for tigris shapefiles (speeds up future runs)
options(tigris_use_cache = TRUE)

# ── Configuration ─────────────────────────────────────────────────────────────
# Census API key should be set via census_api_key() or CENSUS_API_KEY env var
# Get free key at: https://api.census.gov/data/key_signup.html

STATES <- c("AZ", "CO", "NM", "UT")
NAVAJO_FILE <- "data_processed/navajo_nation.geojson"
ISOCHRONES_FILE <- "data_processed/isochrones_Fd.geojson"
OUTPUT_FILE <- "data_processed/blocks_filtered.geojson"
MAP_FILE <- "outputs_maps/04_blocks_verification.html"

# ── 1. Download Block Geometries and Population ──────────────────────────────
cat("Downloading 2020 census blocks for", paste(STATES, collapse = ", "), "...\n")
cat("This will take 5-10 minutes and requires ~2-3GB RAM\n\n")

# Download blocks with population for all 4 states
# P1_001N = 2020 Decennial total population
blocks_list <- list()

for (state in STATES) {
  cat("  Downloading", state, "...\n")

  blocks_list[[state]] <- get_decennial(
    geography = "block",
    variables = "P1_001N",
    year = 2020,
    state = state,
    geometry = TRUE,
    output = "wide"
  )

  cat("    Loaded", nrow(blocks_list[[state]]), "blocks for", state, "\n")
}

# Combine all states
blocks_all <- bind_rows(blocks_list)

cat("\n✓ Downloaded", nrow(blocks_all), "total blocks across 4 states\n")
cat("  Population range:", min(blocks_all$P1_001N), "to", max(blocks_all$P1_001N), "\n\n")

# ── 2. Derive Tract GEOID ─────────────────────────────────────────────────────
cat("Deriving tract GEOID from block GEOID...\n")

blocks_all <- blocks_all %>%
  mutate(
    tract_geoid = substr(GEOID, 1, 11),
    block_pop = P1_001N
  )

cat("✓ Tract GEOID added (first 11 characters of GEOID)\n")
cat("  Example: Block", blocks_all$GEOID[1], "→ Tract", blocks_all$tract_geoid[1], "\n\n")

# ── 3. Filter to Study Area ───────────────────────────────────────────────────
cat("Loading study area boundaries...\n")

navajo <- st_read(NAVAJO_FILE, quiet = TRUE)
isochrones_Fd <- st_read(ISOCHRONES_FILE, quiet = TRUE)

# Transform to match blocks CRS (NAD83)
blocks_crs <- st_crs(blocks_all)
navajo <- st_transform(navajo, blocks_crs)
isochrones_Fd <- st_transform(isochrones_Fd, blocks_crs)

cat("  Navajo Nation loaded\n")
cat("  F_d isochrones loaded (", n_distinct(isochrones_Fd$facility_id), " facilities)\n")
cat("  Transformed to blocks CRS:", blocks_crs$input, "\n\n")

cat("Filtering blocks to study area...\n")
cat("This may take 10-15 minutes for spatial operations on", nrow(blocks_all), "blocks\n\n")

# Test 1: Block intersects Navajo Nation (for initial filter)
cat("  Step 1/4: Testing which blocks intersect Navajo Nation...\n")
in_navajo_any <- st_intersects(blocks_all, navajo, sparse = FALSE)
blocks_all$intersects_navajo <- rowSums(in_navajo_any) > 0

cat("    Blocks intersecting Navajo Nation:", sum(blocks_all$intersects_navajo), "\n")

# Step 2: Calculate majority area for blocks that intersect
cat("  Step 2/4: Calculating majority area for intersecting blocks...\n")
blocks_all$area_total <- st_area(blocks_all)
blocks_all$pct_in_navajo <- 0

intersecting_idx <- which(blocks_all$intersects_navajo)
for (i in seq_along(intersecting_idx)) {
  idx <- intersecting_idx[i]
  if (i %% 1000 == 0) cat("    Processing block", i, "of", length(intersecting_idx), "\n")

  intersection <- suppressWarnings(st_intersection(blocks_all[idx, ], navajo))
  if (nrow(intersection) > 0) {
    area_in <- as.numeric(st_area(intersection))
    blocks_all$pct_in_navajo[idx] <- (area_in / as.numeric(blocks_all$area_total[idx])) * 100
  }
}

# Classify based on majority (>50%)
blocks_all$in_navajo <- blocks_all$pct_in_navajo > 50

cat("    Blocks with >50% area in Navajo Nation:", sum(blocks_all$in_navajo), "\n")

# Test 3: Geometry intersects any F_d isochrone
cat("  Step 3/4: Testing which blocks intersect F_d isochrones...\n")
intersects_isochrone <- st_intersects(blocks_all, isochrones_Fd, sparse = FALSE)
blocks_all$intersects_isochrone <- rowSums(intersects_isochrone) > 0

cat("    Blocks intersecting any F_d isochrone:", sum(blocks_all$intersects_isochrone), "\n")

# Apply filtering logic
cat("  Step 4/4: Applying filtering rules...\n")

blocks_filtered <- blocks_all %>%
  filter(
    # Keep if >50% in Navajo Nation OR intersects isochrone
    in_navajo | intersects_isochrone
  ) %>%
  filter(
    # For blocks OUTSIDE Navajo Nation, remove if population = 0
    # Blocks INSIDE Navajo Nation keep even if population = 0
    in_navajo | block_pop > 0
  ) %>%
  select(GEOID, tract_geoid, block_pop, in_navajo, intersects_isochrone, pct_in_navajo, geometry)

cat("\n✓ Filtering complete\n")
cat("  Starting blocks:", nrow(blocks_all), "\n")
cat("  Blocks in Navajo Nation (kept regardless of pop):", sum(blocks_filtered$in_navajo), "\n")
cat("  Blocks outside Navajo but in isochrone:", sum(!blocks_filtered$in_navajo), "\n")
cat("  Final blocks in K:", nrow(blocks_filtered), "\n")
cat("  Total population:", sum(blocks_filtered$block_pop), "\n\n")

# ── 4. Save Output ────────────────────────────────────────────────────────────
cat("Saving filtered blocks (FULL DETAIL - for analysis)...\n")

st_write(blocks_filtered, OUTPUT_FILE, delete_dsn = TRUE, quiet = TRUE)

cat("✓ Saved to:", OUTPUT_FILE, "\n")
cat("  This file contains FULL-RESOLUTION geometries\n")
cat("  File size:", round(file.size(OUTPUT_FILE) / 1024^2, 1), "MB\n\n")

# ── 5. Produce Verification Map ──────────────────────────────────────────────
cat("Creating verification map (simplified geometries for display only)...\n")

# Simplify geometries ONLY for visualization (50m tolerance)
# This does NOT affect the saved data - only what's shown in the browser
blocks_for_map <- blocks_filtered %>%
  st_simplify(dTolerance = 50, preserveTopology = TRUE)

cat("  Simplified geometries for map display (does not affect saved data)\n")
cat("  Original vertices: ~", format(object.size(blocks_filtered), units = "MB"), "\n")
cat("  Simplified for map: ~", format(object.size(blocks_for_map), units = "MB"), "\n\n")

# Load chapter boundaries for context
navajo_chapters <- st_read("data_processed/navajo_chapters.geojson", quiet = TRUE)

# Create color palette for population
pop_breaks <- c(0, 1, 10, 50, 100, 500, max(blocks_for_map$block_pop))
pop_colors <- colorBin(
  palette = "YlOrRd",
  domain = blocks_for_map$block_pop,
  bins = pop_breaks,
  na.color = "#CCCCCC"
)

# Create map
map <- leaflet() %>%
  addTiles() %>%
  setView(lng = -109.5, lat = 36.0, zoom = 7)

# Add chapter boundaries (for reference)
map <- map %>%
  addPolygons(
    data = navajo_chapters,
    fillColor = "transparent",
    color = "blue",
    weight = 1,
    opacity = 0.5,
    label = ~paste0("Chapter: ", NAME)
  )

# Add filtered blocks
map <- map %>%
  addPolygons(
    data = blocks_for_map,
    fillColor = ~pop_colors(block_pop),
    fillOpacity = 0.7,
    color = "white",
    weight = 0.5,
    opacity = 0.8,
    popup = ~paste0(
      "<b>Block GEOID:</b> ", GEOID, "<br>",
      "<b>Tract GEOID:</b> ", tract_geoid, "<br>",
      "<b>Population:</b> ", block_pop, "<br>",
      "<b>In Navajo Nation:</b> ", ifelse(in_navajo, "Yes", "No (in isochrone)")
    ),
    label = ~paste0("Pop: ", block_pop)
  )

# Add legend
map <- map %>%
  addLegend(
    position = "topright",
    pal = pop_colors,
    values = blocks_for_map$block_pop,
    title = paste0("Block Population<br>(",
                   format(nrow(blocks_for_map), big.mark = ","),
                   " blocks)"),
    opacity = 0.7
  )

# Save map
saveWidget(map, MAP_FILE, selfcontained = FALSE)

cat("✓ Verification map saved to:", MAP_FILE, "\n")
cat("  (Uses simplified geometries for display only)\n\n")

# ── 6. Summary ────────────────────────────────────────────────────────────────
cat("── FINAL SUMMARY ──────────────────────────────────────────────────────\n")
cat("Study area defined as K: spatial population set\n\n")
cat("Input:\n")
cat("  4-state region (AZ, CO, NM, UT):", format(nrow(blocks_all), big.mark = ","), "blocks\n\n")
cat("Filtering criteria:\n")
cat("  1. Centroid in Navajo Nation OR intersects F_d isochrone\n")
cat("  2. If outside Navajo Nation, must have population > 0\n\n")
cat("Output (K):\n")
cat("  Blocks in final set:", format(nrow(blocks_filtered), big.mark = ","), "\n")
cat("  Total population:", format(sum(blocks_filtered$block_pop), big.mark = ","), "\n")
cat("  Blocks in Navajo Nation:", format(sum(blocks_filtered$in_navajo), big.mark = ","), "\n")
cat("  Blocks in isochrones only:", format(sum(!blocks_filtered$in_navajo), big.mark = ","), "\n\n")
cat("Data file (FULL DETAIL):\n")
cat(" ", OUTPUT_FILE, "\n")
cat("  └─ Use this file for all subsequent analysis\n\n")
cat("Verification map (simplified display):\n")
cat(" ", MAP_FILE, "\n")
cat("  └─ For visual review only, not used in analysis\n")
cat("───────────────────────────────────────────────────────────────────────\n")
