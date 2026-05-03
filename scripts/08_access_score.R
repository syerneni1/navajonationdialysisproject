# =============================================================================
# 08_access_score.R
# Purpose: Compute block-level accessibility scores (E2SFCA Step 2)
# Inputs:  data_processed/block_demand.geojson (blocks with P_k)
#          data_processed/facility_supply_ratio.csv (facilities with R_j)
#          data_processed/isochrones_Fd.geojson (catchment areas)
# Outputs: data_processed/block_access_scores.geojson (blocks with A_k and SPAR_k)
#          outputs_maps/08_access_score_verification.html (verification map)
# =============================================================================

library(tidyverse)
library(sf)
library(leaflet)
library(htmlwidgets)

sf_use_s2(FALSE)

# ── Configuration ─────────────────────────────────────────────────────────────
BLOCKS_FILE <- "data_processed/block_demand.geojson"
FACILITIES_FILE <- "data_processed/facility_supply_ratio.csv"
ISOCHRONES_FILE <- "data_processed/isochrones_Fd.geojson"
OUTPUT_FILE <- "data_processed/block_access_scores.geojson"
MAP_FILE <- "outputs_maps/08_access_score_verification.html"

# Impedance weights by travel time band
WEIGHTS <- data.frame(
  band = c("0-15 min", "15-30 min", "30-60 min"),
  W_r = c(1.00, 0.75, 0.32)
)

# ── 1. Load Inputs ────────────────────────────────────────────────────────────
cat("Loading blocks, facilities, and isochrones...\n")

# Decompress blocks if needed
if (file.exists(paste0(BLOCKS_FILE, ".gz")) && !file.exists(BLOCKS_FILE)) {
  cat("  Decompressing blocks file...\n")
  system(paste("gunzip -k", paste0(BLOCKS_FILE, ".gz")))
}

blocks <- st_read(BLOCKS_FILE, quiet = TRUE)
cat("  Blocks:", nrow(blocks), "\n")

facilities <- read_csv(FACILITIES_FILE, show_col_types = FALSE)
cat("  Facilities with R_j:", nrow(facilities), "\n")

isochrones <- st_read(ISOCHRONES_FILE, quiet = TRUE)
cat("  Isochrones:", nrow(isochrones), "polygons\n\n")

# Transform to common CRS
blocks <- st_transform(blocks, st_crs(isochrones))

# ── 2. Compute SPAI (A_k) ─────────────────────────────────────────────────────
cat("Computing SPAI (A_k) using Formula 2...\n")
cat("Formula: A_k = Σ_(j ∈ i_k) R_j · W_r\n\n")

# For each block, find all facilities whose isochrones intersect it
# and sum weighted R_j values

cat("Finding block-facility intersections...\n")
cat("This will take a few minutes for", nrow(blocks), "blocks\n\n")

# Initialize A_k to 0 for all blocks
blocks$A_k <- 0

# Process each facility
for (fac_id in unique(isochrones$facility_id)) {
  cat("  Facility", fac_id, "...\n")

  # Get R_j for this facility
  R_j <- facilities %>%
    filter(facility_id == fac_id) %>%
    pull(R_j)

  if (length(R_j) == 0 || is.na(R_j)) {
    cat("    Skipping (no R_j)\n")
    next
  }

  # Get isochrone bands for this facility
  iso_fac <- isochrones %>% filter(facility_id == fac_id)

  # Separate by band
  band_0_15 <- iso_fac %>% filter(band == "0-15 min")
  band_15_30 <- iso_fac %>% filter(band == "15-30 min")
  band_30_60 <- iso_fac %>% filter(band == "30-60 min")

  # Find blocks intersecting each band
  intersects_0_15 <- st_intersects(blocks, band_0_15, sparse = FALSE)
  intersects_15_30 <- st_intersects(blocks, band_15_30, sparse = FALSE)
  intersects_30_60 <- st_intersects(blocks, band_30_60, sparse = FALSE)

  # Add weighted R_j to A_k based on innermost band
  # 0-15 min takes precedence over 15-30, which takes precedence over 30-60
  blocks$A_k <- blocks$A_k + ifelse(rowSums(intersects_0_15) > 0, R_j * 1.00,
                             ifelse(rowSums(intersects_15_30) > 0, R_j * 0.75,
                             ifelse(rowSums(intersects_30_60) > 0, R_j * 0.32, 0)))

  n_blocks <- sum(rowSums(intersects_0_15) > 0 |
                  rowSums(intersects_15_30) > 0 |
                  rowSums(intersects_30_60) > 0)
  cat("    Blocks reached:", n_blocks, "\n")
}

cat("\n✓ SPAI computed for all blocks\n")
cat("  Blocks with A_k > 0:", sum(blocks$A_k > 0), "/", nrow(blocks), "\n")
cat("  Blocks with A_k = 0 (no access):", sum(blocks$A_k == 0), "\n\n")

cat("SPAI (A_k) statistics:\n")
cat("  Mean A_k:", formatC(mean(blocks$A_k, na.rm = TRUE), format = "e", digits = 3), "\n")
cat("  Median A_k:", formatC(median(blocks$A_k, na.rm = TRUE), format = "e", digits = 3), "\n")
cat("  SD A_k:", formatC(sd(blocks$A_k, na.rm = TRUE), format = "e", digits = 3), "\n")
cat("  Min A_k:", formatC(min(blocks$A_k, na.rm = TRUE), format = "e", digits = 3), "\n")
cat("  Max A_k:", formatC(max(blocks$A_k, na.rm = TRUE), format = "e", digits = 3), "\n\n")

# ── 3. Compute SPAR ───────────────────────────────────────────────────────────
cat("Computing SPAR using Formula 3...\n")
cat("Formula: SPAR_k = A_k / ((1/N) Σ A_k')\n")
cat("Note: Only blocks with population > 0 are included in normalization\n\n")

# Calculate mean SPAI excluding blocks with zero population
# Blocks with pop = 0 shouldn't affect the normalization
blocks_with_pop <- blocks %>% filter(block_pop > 0)
mean_A_k <- mean(blocks_with_pop$A_k, na.rm = TRUE)

cat("  Blocks with population > 0:", nrow(blocks_with_pop), "\n")
cat("  Mean SPAI (pop > 0 blocks only):", formatC(mean_A_k, format = "e", digits = 3), "\n")

# Compute SPAR for all blocks (using mean from populated blocks only)
blocks <- blocks %>%
  mutate(SPAR_k = A_k / mean_A_k)

# Handle division by zero (if mean is 0)
if (mean_A_k == 0) {
  blocks$SPAR_k <- 0
  cat("  WARNING: Mean SPAI is 0, all SPAR values set to 0\n")
}

cat("\n✓ SPAR computed for all blocks\n\n")

cat("SPAR statistics:\n")
cat("  Mean SPAR_k:", round(mean(blocks$SPAR_k, na.rm = TRUE), 3), "\n")
cat("  Median SPAR_k:", round(median(blocks$SPAR_k, na.rm = TRUE), 3), "\n")
cat("  SD SPAR_k:", round(sd(blocks$SPAR_k, na.rm = TRUE), 3), "\n")
cat("  Min SPAR_k:", round(min(blocks$SPAR_k, na.rm = TRUE), 3), "\n")
cat("  Max SPAR_k:", round(max(blocks$SPAR_k, na.rm = TRUE), 3), "\n")
cat("  Blocks with SPAR_k > 1 (above avg):", sum(blocks$SPAR_k > 1, na.rm = TRUE), "\n")
cat("  Blocks with SPAR_k < 1 (below avg):", sum(blocks$SPAR_k < 1, na.rm = TRUE), "\n")
cat("  Blocks with SPAR_k = 0 (no access):", sum(blocks$SPAR_k == 0, na.rm = TRUE), "\n\n")

# ── 4. Save Output ────────────────────────────────────────────────────────────
cat("Saving block access scores...\n")

# Select final columns
blocks_output <- blocks %>%
  select(GEOID, tract_geoid, block_pop, P_k, A_k, SPAR_k, geometry)

st_write(blocks_output, OUTPUT_FILE, delete_dsn = TRUE, quiet = TRUE)

cat("✓ Saved to:", OUTPUT_FILE, "\n")
cat("  File size:", round(file.size(OUTPUT_FILE) / 1024^2, 1), "MB\n\n")

# ── 5. Produce Verification Map ──────────────────────────────────────────────
cat("Creating verification map...\n")

# Load context layers
navajo <- st_read("data_processed/navajo_nation.geojson", quiet = TRUE) %>%
  st_transform(st_crs(blocks))

chapters <- st_read("data_processed/navajo_chapters.geojson", quiet = TRUE) %>%
  st_transform(st_crs(blocks))

# Create color palettes
# SPAI palette (A_k)
spai_palette <- colorNumeric(
  palette = "YlGnBu",
  domain = blocks$A_k,
  na.color = "#808080"
)

# SPAR palette (normalized)
spar_palette <- colorBin(
  palette = "RdYlGn",
  domain = blocks$SPAR_k,
  bins = c(0, 0.5, 0.75, 1.0, 1.5, 2.0, max(blocks$SPAR_k, na.rm = TRUE)),
  na.color = "#808080"
)

# Create map
map <- leaflet() %>%
  addTiles() %>%
  setView(lng = -109.5, lat = 36.0, zoom = 7)

# Add blocks colored by SPAR (default)
# Grey out blocks with zero population
map <- map %>%
  addPolygons(
    data = blocks,
    fillColor = ~ifelse(block_pop == 0, "#CCCCCC", spar_palette(SPAR_k)),
    fillOpacity = 0.7,
    color = "white",
    weight = 0.3,
    opacity = 0.8,
    group = "SPAR (Normalized Access)",
    popup = ~paste0(
      "<b>Block GEOID:</b> ", GEOID, "<br>",
      "<b>Population:</b> ", block_pop, "<br>",
      "<b>Demand (P_k):</b> ", round(P_k, 2), "<br>",
      "<b>SPAI (A_k):</b> ", formatC(A_k, format = "e", digits = 3), "<br>",
      "<b>SPAR:</b> ", round(SPAR_k, 3), "<br>",
      "<b>Access Level:</b> ", ifelse(block_pop == 0, "No population",
                                ifelse(SPAR_k == 0, "No access",
                                ifelse(SPAR_k < 1, "Below average", "Above average")))
    ),
    label = ~paste0("SPAR: ", round(SPAR_k, 2))
  )

# Add blocks colored by SPAI
# Grey out blocks with zero population
map <- map %>%
  addPolygons(
    data = blocks,
    fillColor = ~ifelse(block_pop == 0, "#CCCCCC", spai_palette(A_k)),
    fillOpacity = 0.7,
    color = "white",
    weight = 0.3,
    opacity = 0.8,
    group = "SPAI (Raw Access Score)",
    popup = ~paste0(
      "<b>Block GEOID:</b> ", GEOID, "<br>",
      "<b>Population:</b> ", block_pop, ifelse(block_pop == 0, " (unpopulated)", ""), "<br>",
      "<b>Demand (P_k):</b> ", round(P_k, 2), "<br>",
      "<b>SPAI (A_k):</b> ", formatC(A_k, format = "e", digits = 3), "<br>",
      "<b>SPAR:</b> ", round(SPAR_k, 3)
    ),
    label = ~paste0("SPAI: ", formatC(A_k, format = "e", digits = 2))
  )

# Add chapter boundaries
map <- map %>%
  addPolygons(
    data = chapters,
    fillColor = "transparent",
    color = "blue",
    weight = 1,
    opacity = 0.7,
    group = "Navajo Chapters",
    label = ~paste0("Chapter: ", NAME)
  )

# Add layer controls
map <- map %>%
  addLayersControl(
    baseGroups = c("SPAR (Normalized Access)", "SPAI (Raw Access Score)"),
    overlayGroups = "Navajo Chapters",
    options = layersControlOptions(collapsed = FALSE)
  )

# Add legends
map <- map %>%
  addLegend(
    position = "topright",
    pal = spar_palette,
    values = blocks$SPAR_k,
    title = "SPAR<br><small>(Spatial Access Ratio)</small>",
    opacity = 0.7,
    group = "SPAR (Normalized Access)"
  ) %>%
  addLegend(
    position = "topright",
    pal = spai_palette,
    values = blocks$A_k,
    title = "SPAI (A_k × 10⁴)<br><small>(Access Score)</small>",
    opacity = 0.7,
    labFormat = labelFormat(transform = function(x) x * 10000, digits = 2),
    group = "SPAI (Raw Access Score)"
  )

# Save map
saveWidget(map, MAP_FILE, selfcontained = FALSE)

cat("✓ Verification map saved to:", MAP_FILE, "\n\n")

# ── 6. Summary ────────────────────────────────────────────────────────────────
cat("── FINAL SUMMARY ──────────────────────────────────────────────────────\n")
cat("E2SFCA Step 2: Block-level accessibility scores computed\n\n")

cat("Formulas:\n")
cat("  SPAI:  A_k = Σ_(j ∈ i_k) R_j · W_r\n")
cat("  SPAR:  SPAR_k = A_k / ((1/N) Σ A_k')\n\n")

cat("Study area (K):\n")
cat("  Total blocks:", nrow(blocks), "\n")
cat("  Total population:", format(sum(blocks$block_pop, na.rm = TRUE), big.mark = ","), "\n\n")

cat("Access statistics:\n")
cat("  Blocks with access (A_k > 0):", sum(blocks$A_k > 0),
    "(", round(100 * sum(blocks$A_k > 0) / nrow(blocks), 1), "%)\n")
cat("  Blocks with no access (A_k = 0):", sum(blocks$A_k == 0),
    "(", round(100 * sum(blocks$A_k == 0) / nrow(blocks), 1), "%)\n\n")

cat("  Population with access:",
    format(sum(blocks$block_pop[blocks$A_k > 0], na.rm = TRUE), big.mark = ","), "\n")
cat("  Population with no access:",
    format(sum(blocks$block_pop[blocks$A_k == 0], na.rm = TRUE), big.mark = ","), "\n\n")

cat("SPAR distribution:\n")
cat("  Above average (SPAR > 1):", sum(blocks$SPAR_k > 1, na.rm = TRUE), "blocks\n")
cat("  Below average (SPAR < 1):", sum(blocks$SPAR_k < 1 & blocks$SPAR_k > 0, na.rm = TRUE), "blocks\n")
cat("  No access (SPAR = 0):", sum(blocks$SPAR_k == 0, na.rm = TRUE), "blocks\n\n")

cat("Output files:\n")
cat(" ", OUTPUT_FILE, "\n")
cat("  └─ Block geometries with SPAI (A_k) and SPAR\n\n")
cat(" ", MAP_FILE, "\n")
cat("  └─ Interactive map with toggleable SPAI/SPAR layers\n")
cat("───────────────────────────────────────────────────────────────────────\n")
