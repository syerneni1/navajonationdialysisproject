# =============================================================================
# 07_supply_ratio.R
# Purpose: Compute facility supply-to-demand ratios R_j (E2SFCA Step 1)
# Inputs:  data_processed/facilities_Fd.csv (facility supply S_j)
#          data_processed/isochrones_Fd.geojson (catchment areas)
#          data_processed/block_demand.geojson (block demand P_k)
# Outputs: data_processed/facility_supply_ratio.csv (facilities with R_j)
#          outputs_maps/07_supply_ratio_verification.html (bubble map)
# =============================================================================

library(tidyverse)
library(sf)
library(leaflet)
library(htmlwidgets)

sf_use_s2(FALSE)

# ── Configuration ─────────────────────────────────────────────────────────────
FACILITIES_FILE <- "data_processed/facilities_Fd.csv"
ISOCHRONES_FILE <- "data_processed/isochrones_Fd.geojson"
BLOCKS_FILE <- "data_processed/block_demand.geojson"
OUTPUT_FILE <- "data_processed/facility_supply_ratio.csv"
MAP_FILE <- "outputs_maps/07_supply_ratio_verification.html"

# Impedance weights by travel time band
WEIGHTS <- data.frame(
  band = c("0-15 min", "15-30 min", "30-60 min"),
  W_r = c(1.00, 0.75, 0.32)
)

# ── 1. Load Inputs ────────────────────────────────────────────────────────────
cat("Loading facilities, isochrones, and block demand...\n")

facilities <- read_csv(FACILITIES_FILE, show_col_types = FALSE)
cat("  Facilities (F_d):", nrow(facilities), "\n")

isochrones <- st_read(ISOCHRONES_FILE, quiet = TRUE)
cat("  Isochrones:", nrow(isochrones), "polygons\n")
cat("    Facilities:", n_distinct(isochrones$facility_id), "\n")
cat("    Bands:", paste(unique(isochrones$band), collapse = ", "), "\n")

# Decompress blocks if needed
if (file.exists(paste0(BLOCKS_FILE, ".gz")) && !file.exists(BLOCKS_FILE)) {
  cat("  Decompressing blocks file...\n")
  system(paste("gunzip -k", paste0(BLOCKS_FILE, ".gz")))
}

blocks <- st_read(BLOCKS_FILE, quiet = TRUE)
cat("  Blocks:", nrow(blocks), "\n")
cat("  Total demand (ΣP_k):", round(sum(blocks$P_k, na.rm = TRUE), 1), "\n\n")

# Transform to common CRS
blocks <- st_transform(blocks, st_crs(isochrones))

# ── 2. Build Block–Facility Relationships ────────────────────────────────────
cat("Building block-facility relationships with impedance weights...\n")
cat("Using st_intersects() to match blocks to isochrone bands\n\n")

# For each facility, find intersecting blocks and assign weights
block_facility_weights <- list()

for (fac_id in unique(isochrones$facility_id)) {
  cat("  Facility", fac_id, "...\n")

  # Get isochrone bands for this facility
  iso_fac <- isochrones %>% filter(facility_id == fac_id)

  # Separate by band
  band_0_15 <- iso_fac %>% filter(band == "0-15 min")
  band_15_30 <- iso_fac %>% filter(band == "15-30 min")
  band_30_60 <- iso_fac %>% filter(band == "30-60 min")

  # Find blocks intersecting each band (innermost band takes precedence)
  intersects_0_15 <- st_intersects(blocks, band_0_15, sparse = FALSE)
  intersects_15_30 <- st_intersects(blocks, band_15_30, sparse = FALSE)
  intersects_30_60 <- st_intersects(blocks, band_30_60, sparse = FALSE)

  # Assign weights based on innermost band
  blocks_in_catchment <- blocks %>%
    mutate(
      in_0_15 = rowSums(intersects_0_15) > 0,
      in_15_30 = rowSums(intersects_15_30) > 0,
      in_30_60 = rowSums(intersects_30_60) > 0
    ) %>%
    filter(in_0_15 | in_15_30 | in_30_60) %>%
    mutate(
      # Assign weight based on innermost band (0-15 > 15-30 > 30-60)
      W_r = case_when(
        in_0_15 ~ 1.00,
        in_15_30 ~ 0.75,
        in_30_60 ~ 0.32,
        TRUE ~ NA_real_
      ),
      facility_id = fac_id
    ) %>%
    st_drop_geometry() %>%
    select(facility_id, GEOID, P_k, W_r)

  cat("    Blocks in catchment:", nrow(blocks_in_catchment), "\n")
  cat("      0-15 min:", sum(blocks_in_catchment$W_r == 1.00, na.rm = TRUE), "\n")
  cat("      15-30 min:", sum(blocks_in_catchment$W_r == 0.75, na.rm = TRUE), "\n")
  cat("      30-60 min:", sum(blocks_in_catchment$W_r == 0.32, na.rm = TRUE), "\n")

  block_facility_weights[[as.character(fac_id)]] <- blocks_in_catchment
}

# Combine all relationships
all_relationships <- bind_rows(block_facility_weights)

cat("\n✓ Block-facility relationships built\n")
cat("  Total block-facility pairs:", nrow(all_relationships), "\n")
cat("  Weight distribution:\n")
cat("    W_r = 1.00 (0-15 min):", sum(all_relationships$W_r == 1.00), "pairs\n")
cat("    W_r = 0.75 (15-30 min):", sum(all_relationships$W_r == 0.75), "pairs\n")
cat("    W_r = 0.32 (30-60 min):", sum(all_relationships$W_r == 0.32), "pairs\n\n")

# ── 3. Compute R_j ────────────────────────────────────────────────────────────
cat("Computing supply-to-demand ratios R_j using Formula 1...\n")
cat("Formula: R_j = S_j / Σ_(k ∈ i_j) P_k · W_r\n\n")

# Calculate weighted demand for each facility
facility_demand <- all_relationships %>%
  mutate(weighted_demand = P_k * W_r) %>%
  group_by(facility_id) %>%
  summarize(
    total_weighted_demand = sum(weighted_demand, na.rm = TRUE),
    blocks_in_catchment = n(),
    .groups = "drop"
  )

# Join to facilities and compute R_j
facilities_with_ratio <- facilities %>%
  left_join(facility_demand, by = "facility_id") %>%
  mutate(
    # R_j = S_j / total_weighted_demand
    R_j = dialysis_station_count / total_weighted_demand,
    # Replace Inf (no demand) with NA
    R_j = if_else(is.infinite(R_j), NA_real_, R_j)
  )

cat("✓ Supply-to-demand ratios computed\n")
cat("  Facilities with R_j:", sum(!is.na(facilities_with_ratio$R_j)), "/", nrow(facilities_with_ratio), "\n\n")

# Summary statistics
cat("R_j statistics:\n")
cat("  Mean R_j:", round(mean(facilities_with_ratio$R_j, na.rm = TRUE), 4), "\n")
cat("  Median R_j:", round(median(facilities_with_ratio$R_j, na.rm = TRUE), 4), "\n")
cat("  SD R_j:", round(sd(facilities_with_ratio$R_j, na.rm = TRUE), 4), "\n")
cat("  Min R_j:", round(min(facilities_with_ratio$R_j, na.rm = TRUE), 4), "\n")
cat("  Max R_j:", round(max(facilities_with_ratio$R_j, na.rm = TRUE), 4), "\n\n")

# ── 4. Save Output ────────────────────────────────────────────────────────────
cat("Saving facility supply-to-demand ratios...\n")

write_csv(facilities_with_ratio, OUTPUT_FILE)

cat("✓ Saved to:", OUTPUT_FILE, "\n")
cat("  Columns:", paste(names(facilities_with_ratio), collapse = ", "), "\n\n")

# ── 5. Produce Verification Map ──────────────────────────────────────────────
cat("Creating verification bubble map...\n")

# Load Navajo Nation boundary for context
navajo <- st_read("data_processed/navajo_nation.geojson", quiet = TRUE)

# Filter to facilities with valid R_j
facilities_for_map <- facilities_with_ratio %>%
  filter(!is.na(R_j) & !is.na(latitude) & !is.na(longitude))

cat("  Facilities on map:", nrow(facilities_for_map), "\n")

# Scale R_j to per-10000 for more informative legend
facilities_for_map <- facilities_for_map %>%
  mutate(R_j_per_10k = R_j * 10000)

cat("  R_j range (×10⁴):", round(min(facilities_for_map$R_j_per_10k), 3), "to", round(max(facilities_for_map$R_j_per_10k), 3), "\n")

# Create REVERSED color palette (low R_j = more burdened = dark red)
r_palette <- colorNumeric(
  palette = "YlOrRd",
  domain = facilities_for_map$R_j_per_10k,
  reverse = TRUE,  # Reverse so low values = dark red
  na.color = "#808080"
)

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

# Add facility bubbles: size by supply, color by burden
# Larger bubble = more stations, Darker red = more burdened (lower R_j)
map <- map %>%
  addCircleMarkers(
    data = facilities_for_map,
    lng = ~longitude,
    lat = ~latitude,
    radius = ~dialysis_station_count,  # Size by station count
    fillColor = ~r_palette(R_j_per_10k),  # Color by burden (reversed scale)
    fillOpacity = 0.6,  # Semi-transparent
    color = "black",
    weight = 1,
    popup = ~paste0(
      "<b>", facility_name_cms, "</b><br>",
      "ID: ", facility_id, "<br>",
      "City: ", city_cms, ", ", state_cms, "<br>",
      "Stations (S_j): ", dialysis_station_count, "<br>",
      "Blocks in catchment: ", blocks_in_catchment, "<br>",
      "Weighted demand: ", round(total_weighted_demand, 1), "<br>",
      "<b>R_j (×10⁴):</b> ", round(R_j_per_10k, 3)
    ),
    label = ~paste0(facility_name_cms, " - Stations: ", dialysis_station_count)
  )

# Add legend
map <- map %>%
  addLegend(
    position = "topright",
    pal = r_palette,
    values = facilities_for_map$R_j_per_10k,
    title = "R_j (×10⁴)<br><small>Darker = More Burdened<br>Size = Station Count</small>",
    opacity = 0.7,
    labFormat = labelFormat(digits = 2)
  )

# Save map
saveWidget(map, MAP_FILE, selfcontained = FALSE)

cat("✓ Verification map saved to:", MAP_FILE, "\n\n")

# ── 6. Summary ────────────────────────────────────────────────────────────────
cat("── FINAL SUMMARY ──────────────────────────────────────────────────────\n")
cat("E2SFCA Step 1: Supply-to-demand ratios computed\n\n")

cat("Formula:\n")
cat("  R_j = S_j / Σ_(k ∈ i_j) P_k · W_r\n")
cat("  Where:\n")
cat("    S_j = dialysis station count at facility j\n")
cat("    P_k = dialysis demand at block k\n")
cat("    W_r = impedance weight by travel time band\n")
cat("    i_j = set of blocks within 60 minutes of facility j\n\n")

cat("Impedance weights:\n")
cat("  0-15 min:  W_r = 1.00\n")
cat("  15-30 min: W_r = 0.75\n")
cat("  30-60 min: W_r = 0.32\n\n")

cat("Results:\n")
cat("  Facilities in F_d:", nrow(facilities_with_ratio), "\n")
cat("  Facilities with R_j:", sum(!is.na(facilities_with_ratio$R_j)), "\n")
cat("  Block-facility pairs:", nrow(all_relationships), "\n\n")

cat("R_j distribution:\n")
summary_stats <- summary(facilities_with_ratio$R_j)
print(summary_stats)
cat("\n")

cat("Output files:\n")
cat(" ", OUTPUT_FILE, "\n")
cat("  └─ Facilities with R_j (supply-to-demand ratios)\n\n")
cat(" ", MAP_FILE, "\n")
cat("  └─ Bubble map showing facilities scaled by R_j\n")
cat("───────────────────────────────────────────────────────────────────────\n")
