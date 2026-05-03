# =============================================================================
# 06_block_demand.R
# Purpose: Compute block-level dialysis demand P_k using Formula 4
# Inputs:  data_processed/blocks_filtered.geojson (K: spatial population set)
#          data_processed/block_demographics.csv (demographic strata with HR)
# Outputs: data_processed/block_demand.geojson (blocks with P_k attached)
#          outputs_maps/06_block_demand_verification.html (verification map)
# =============================================================================

library(tidyverse)
library(sf)
library(leaflet)
library(htmlwidgets)

sf_use_s2(FALSE)

# ── Configuration ─────────────────────────────────────────────────────────────
BLOCKS_FILE <- "data_processed/blocks_filtered.geojson"
DEMOGRAPHICS_FILE <- "data_processed/block_demographics.csv"
OUTPUT_FILE <- "data_processed/block_demand.geojson"
MAP_FILE <- "outputs_maps/06_block_demand_verification.html"

# ── 1. Load Inputs ────────────────────────────────────────────────────────────
cat("Loading filtered blocks and demographics...\n")

# Decompress blocks if needed
if (file.exists(paste0(BLOCKS_FILE, ".gz")) && !file.exists(BLOCKS_FILE)) {
  cat("  Decompressing blocks file...\n")
  system(paste("gunzip -k", paste0(BLOCKS_FILE, ".gz")))
}

blocks <- st_read(BLOCKS_FILE, quiet = TRUE)
cat("  Blocks loaded:", nrow(blocks), "\n")

demographics <- read_csv(DEMOGRAPHICS_FILE, show_col_types = FALSE)
cat("  Demographics loaded:", nrow(demographics), "rows\n")
cat("    (All 4-state blocks × demographic strata)\n\n")

# ── 2. Filter Demographics to Study Area ──────────────────────────────────────
cat("Filtering demographics to study area blocks only...\n")

demographics_filtered <- demographics %>%
  filter(GEOID %in% blocks$GEOID)

cat("✓ Filtered to study area\n")
cat("  Blocks in study area:", n_distinct(demographics_filtered$GEOID), "\n")
cat("  Demographic rows:", nrow(demographics_filtered), "\n")
cat("  Expected: ", n_distinct(demographics_filtered$GEOID), "blocks ×",
    n_distinct(demographics_filtered$race), "races ×",
    n_distinct(demographics_filtered$sex), "sexes ×",
    n_distinct(demographics_filtered$age_group), "age groups =",
    n_distinct(demographics_filtered$GEOID) *
    n_distinct(demographics_filtered$race) *
    n_distinct(demographics_filtered$sex) *
    n_distinct(demographics_filtered$age_group), "rows\n\n")

# ── 3. Apply Formula 4: Compute P_k ───────────────────────────────────────────
cat("Computing dialysis demand P_k using Formula 4...\n")
cat("Formula: P_k = Σ_(s,a,r) N_(s,a,r,k) · HR_s · HR_a · HR_r · (1 + DM_k × 5.10 + HTN_k × 0.44)\n\n")

# For each block, compute demand
demand_by_block <- demographics_filtered %>%
  mutate(
    # Disease multiplier (same for all strata within a block)
    disease_multiplier = 1 + (DM_k / 100) * 5.10 + (HTN_k / 100) * 0.44,

    # Individual stratum contribution
    # N_(s,a,r,k) × HR_s × HR_a × HR_r × disease_multiplier
    stratum_demand = population * HR_sex * HR_age * HR_race * disease_multiplier
  ) %>%
  group_by(GEOID) %>%
  summarize(
    P_k = sum(stratum_demand, na.rm = TRUE),
    total_pop = sum(population, na.rm = TRUE),
    avg_disease_mult = mean(disease_multiplier, na.rm = TRUE),
    .groups = "drop"
  )

cat("✓ Demand computed for", nrow(demand_by_block), "blocks\n")
cat("  Total demand (P_k sum):", round(sum(demand_by_block$P_k, na.rm = TRUE), 1), "\n")
cat("  Mean demand per block:", round(mean(demand_by_block$P_k, na.rm = TRUE), 2), "\n")
cat("  Median demand per block:", round(median(demand_by_block$P_k, na.rm = TRUE), 2), "\n")
cat("  Max demand:", round(max(demand_by_block$P_k, na.rm = TRUE), 2), "\n\n")

# ── 4. Join to Block Geometries ───────────────────────────────────────────────
cat("Joining demand to block geometries...\n")

blocks_with_demand <- blocks %>%
  left_join(demand_by_block, by = "GEOID") %>%
  select(GEOID, tract_geoid, block_pop, P_k, total_pop, avg_disease_mult, geometry)

# Check for missing P_k
missing_demand <- blocks_with_demand %>%
  filter(is.na(P_k))

if (nrow(missing_demand) > 0) {
  cat("  WARNING:", nrow(missing_demand), "blocks missing P_k calculation\n")
} else {
  cat("✓ All blocks have P_k computed\n")
}

cat("\n")

# ── 5. Save Output ────────────────────────────────────────────────────────────
cat("Saving block demand file...\n")

st_write(blocks_with_demand, OUTPUT_FILE, delete_dsn = TRUE, quiet = TRUE)

cat("✓ Saved to:", OUTPUT_FILE, "\n")
cat("  File size:", round(file.size(OUTPUT_FILE) / 1024^2, 1), "MB\n\n")

# ── 6. Produce Verification Map ──────────────────────────────────────────────
cat("Creating verification map...\n")

# Calculate 95th percentile for color scale cap
p95 <- quantile(blocks_with_demand$P_k, 0.95, na.rm = TRUE)
cat("  95th percentile P_k:", round(p95, 2), "(used for color scale cap)\n")

# Cap P_k for visualization only
blocks_with_demand <- blocks_with_demand %>%
  mutate(P_k_display = pmin(P_k, p95))

# Simplify geometries for faster map rendering
blocks_map <- blocks_with_demand %>%
  st_simplify(dTolerance = 50, preserveTopology = TRUE)

cat("  Simplified geometries for display\n")

# Create color palette (smooth gradient)
demand_palette <- colorNumeric(
  palette = "YlOrRd",
  domain = c(0, p95),
  na.color = "#808080"
)

# Load Navajo Nation boundary for context
navajo <- st_read("data_processed/navajo_nation.geojson", quiet = TRUE) %>%
  st_transform(st_crs(blocks_map))

# Create map
map <- leaflet() %>%
  addTiles() %>%
  setView(lng = -109.5, lat = 36.0, zoom = 7)

# Add Navajo Nation boundary
map <- map %>%
  addPolygons(
    data = navajo,
    fillColor = "transparent",
    color = "blue",
    weight = 2,
    opacity = 0.6,
    label = "Navajo Nation"
  )

# Add blocks colored by demand
map <- map %>%
  addPolygons(
    data = blocks_map,
    fillColor = ~demand_palette(P_k_display),
    fillOpacity = 0.7,
    color = "white",
    weight = 0.3,
    opacity = 0.8,
    popup = ~paste0(
      "<b>Block GEOID:</b> ", GEOID, "<br>",
      "<b>Population:</b> ", block_pop, "<br>",
      "<b>Dialysis Demand (P_k):</b> ", round(P_k, 2), "<br>",
      "<b>Disease Multiplier:</b> ", round(avg_disease_mult, 3), "<br>",
      "<b>Demand per capita:</b> ", round(P_k / block_pop, 4)
    ),
    label = ~paste0("P_k: ", round(P_k, 2))
  )

# Add legend
map <- map %>%
  addLegend(
    position = "topright",
    pal = demand_palette,
    values = blocks_map$P_k_display,
    title = paste0("Dialysis Demand (P_k)<br>",
                   "(capped at 95th percentile)<br>",
                   nrow(blocks_map), " blocks"),
    opacity = 0.7,
    labFormat = labelFormat(digits = 1)
  )

# Save map
saveWidget(map, MAP_FILE, selfcontained = FALSE)

cat("✓ Verification map saved to:", MAP_FILE, "\n\n")

# ── 7. Summary Statistics ─────────────────────────────────────────────────────
cat("── FINAL SUMMARY ──────────────────────────────────────────────────────\n")
cat("Dialysis demand P_k computed using Formula 4\n\n")

cat("Formula components:\n")
cat("  • Demographic hazard ratios: HR_sex × HR_age × HR_race\n")
cat("  • Disease multiplier: 1 + DM_k × 5.10 + HTN_k × 0.44\n")
cat("  • Tract-level prevalence from CDC PLACES\n\n")

cat("Study area (K):\n")
cat("  Blocks:", nrow(blocks_with_demand), "\n")
cat("  Total population:", format(sum(blocks_with_demand$block_pop, na.rm = TRUE), big.mark = ","), "\n\n")

cat("Demand statistics:\n")
cat("  Total demand (ΣP_k):", round(sum(blocks_with_demand$P_k, na.rm = TRUE), 1), "\n")
cat("  Mean P_k:", round(mean(blocks_with_demand$P_k, na.rm = TRUE), 2), "\n")
cat("  Median P_k:", round(median(blocks_with_demand$P_k, na.rm = TRUE), 2), "\n")
cat("  SD P_k:", round(sd(blocks_with_demand$P_k, na.rm = TRUE), 2), "\n")
cat("  Min P_k:", round(min(blocks_with_demand$P_k, na.rm = TRUE), 2), "\n")
cat("  Max P_k:", round(max(blocks_with_demand$P_k, na.rm = TRUE), 2), "\n")
cat("  95th percentile:", round(p95, 2), "\n\n")

cat("Per-capita demand:\n")
blocks_with_demand_pc <- blocks_with_demand %>%
  filter(block_pop > 0) %>%
  mutate(demand_per_capita = P_k / block_pop)

cat("  Mean demand per capita:", round(mean(blocks_with_demand_pc$demand_per_capita, na.rm = TRUE), 4), "\n")
cat("  Median demand per capita:", round(median(blocks_with_demand_pc$demand_per_capita, na.rm = TRUE), 4), "\n\n")

cat("Output files:\n")
cat(" ", OUTPUT_FILE, "\n")
cat("  └─ Block geometries with P_k (full resolution)\n\n")
cat(" ", MAP_FILE, "\n")
cat("  └─ Verification map (simplified for display)\n")
cat("───────────────────────────────────────────────────────────────────────\n")
