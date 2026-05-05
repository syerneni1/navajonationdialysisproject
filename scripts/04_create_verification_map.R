# Create Step 4 verification map
library(sf)
library(leaflet)
library(htmlwidgets)
library(dplyr)

sf_use_s2(FALSE)

cat("Loading data...\n")

# Load blocks
blocks <- st_read("data_processed/blocks_filtered.geojson", quiet = TRUE)
cat("  Blocks:", nrow(blocks), "\n")

# Load chapters
chapters <- st_read("data_processed/navajo_chapters.geojson", quiet = TRUE)
cat("  Chapters:", nrow(chapters), "\n")

# Load isochrones
isochrones <- st_read("data_processed/isochrones_Fd.geojson", quiet = TRUE)
cat("  Isochrones:", nrow(isochrones), "\n")

# Load facilities
facilities_ids <- read.csv("data_processed/facilities_Fd.csv")$facility_id
all_facilities <- read.csv("data_processed/facilities_geocoded_verified.csv")
facilities <- all_facilities %>% filter(facility_id %in% facilities_ids)
cat("  Facilities:", nrow(facilities), "\n\n")

cat("Assigning blocks to chapters based on majority area overlap...\n")

# Transform to common CRS
blocks <- st_transform(blocks, st_crs(chapters))

# Separate blocks into 3 categories:
# 1. Blocks >50% in Navajo Nation (colored by chapter)
# 2. Blocks intersecting isochrones but <50% in Navajo (gray layer)
# 3. Excluded blocks (not in dataset - filtered out in step 4)
blocks_navajo <- blocks %>% filter(in_navajo == TRUE)
blocks_iso_only <- blocks %>% filter(in_navajo == FALSE & intersects_isochrone == TRUE)

cat("  Blocks >50% in Navajo Nation:", nrow(blocks_navajo), "\n")
cat("  Blocks intersecting isochrones (<50% in Navajo):", nrow(blocks_iso_only), "\n")

# For each block, find which chapter it has the most overlap with
cat("  Calculating area-based chapter assignment (this may take a minute)...\n")

assign_to_chapter <- function(block_geom, chapters_sf) {
  # Calculate intersection area with each chapter
  intersections <- st_intersection(block_geom, chapters_sf)

  if (nrow(intersections) == 0) {
    return(NA_character_)
  }

  # Calculate area of each intersection
  intersections$area <- st_area(intersections)

  # Return chapter with largest intersection area
  intersections %>%
    arrange(desc(area)) %>%
    slice(1) %>%
    pull(NAME)
}

# Apply to each block
blocks_navajo$chapter <- sapply(1:nrow(blocks_navajo), function(i) {
  if (i %% 1000 == 0) cat("    Processing block", i, "of", nrow(blocks_navajo), "\n")
  assign_to_chapter(blocks_navajo[i, ], chapters %>% select(NAME))
})

cat("  Assigned", sum(!is.na(blocks_navajo$chapter)), "blocks to chapters\n\n")

# Save chapter assignments for use in Step 9
cat("  Saving chapter assignments to cache...\n")
blocks_navajo %>%
  st_drop_geometry() %>%
  select(GEOID, chapter) %>%
  write.csv("data_processed/block_chapter_assignments.csv", row.names = FALSE)
cat("  ✓ Saved to data_processed/block_chapter_assignments.csv\n\n")

cat("Creating map...\n")

# Create color palette for chapters
chapter_colors <- colorFactor(
  palette = "Set3",
  domain = chapters$NAME,
  na.color = "#CCCCCC"
)

# Create color palette for isochrones
iso_colors <- colorFactor(
  palette = c("#228B22", "#FFD700", "#DC143C"),
  domain = c("0-15 min", "15-30 min", "30-60 min")
)

# Create map
map <- leaflet() %>%
  addTiles() %>%
  setView(lng = -109.5, lat = 36.0, zoom = 7)

# Add chapter boundaries
map <- map %>%
  addPolygons(
    data = chapters,
    fillColor = "transparent",
    color = "blue",
    weight = 1,
    opacity = 0.5,
    group = "Chapters",
    label = ~paste0("Chapter: ", NAME)
  )

# Add all isochrone bands (0-15, 15-30, 30-60)
iso_0_15 <- isochrones %>% filter(band == "0-15 min")
iso_15_30 <- isochrones %>% filter(band == "15-30 min")
iso_30_60 <- isochrones %>% filter(band == "30-60 min")

map <- map %>%
  addPolygons(
    data = iso_0_15,
    fillColor = "green",
    fillOpacity = 0.3,
    color = "darkgreen",
    weight = 1,
    group = "Isochrones",
    label = ~paste0("Facility ", facility_id, " - 0-15 min")
  ) %>%
  addPolygons(
    data = iso_15_30,
    fillColor = "yellow",
    fillOpacity = 0.3,
    color = "gold",
    weight = 1,
    group = "Isochrones",
    label = ~paste0("Facility ", facility_id, " - 15-30 min")
  ) %>%
  addPolygons(
    data = iso_30_60,
    fillColor = "red",
    fillOpacity = 0.3,
    color = "darkred",
    weight = 1,
    group = "Isochrones",
    label = ~paste0("Facility ", facility_id, " - 30-60 min")
  )

# Add blocks in Navajo Nation (colored by chapter)
map <- map %>%
  addPolygons(
    data = blocks_navajo,
    fillColor = ~chapter_colors(chapter),
    fillOpacity = 0.6,
    color = "white",
    weight = 0.3,
    opacity = 0.8,
    group = "Census Blocks (Navajo)",
    popup = ~paste0(
      "<b>Block GEOID:</b> ", GEOID, "<br>",
      "<b>Tract GEOID:</b> ", tract_geoid, "<br>",
      "<b>Population:</b> ", block_pop, "<br>",
      "<b>Chapter:</b> ", ifelse(is.na(chapter), "Unknown", chapter)
    ),
    label = ~paste0("Chapter: ", ifelse(is.na(chapter), "Unknown", chapter))
  )

# Add blocks in isochrones only (all gray)
map <- map %>%
  addPolygons(
    data = blocks_iso_only,
    fillColor = "#888888",
    fillOpacity = 0.4,
    color = "white",
    weight = 0.3,
    opacity = 0.6,
    group = "Census Blocks (Isochrones only)",
    popup = ~paste0(
      "<b>Block GEOID:</b> ", GEOID, "<br>",
      "<b>Tract GEOID:</b> ", tract_geoid, "<br>",
      "<b>Population:</b> ", block_pop, "<br>",
      "<b>% in Navajo:</b> ", round(pct_in_navajo, 1), "%<br>",
      "<b>Status:</b> Intersects isochrone (<50% in Navajo)"
    ),
    label = ~paste0(round(pct_in_navajo, 0), "% in Navajo")
  )

# Add facilities
map <- map %>%
  addCircleMarkers(
    data = facilities,
    lng = ~longitude,
    lat = ~latitude,
    radius = 6,
    color = "green",
    fillColor = "green",
    fillOpacity = 0.9,
    weight = 2,
    group = "F_d Facilities",
    popup = ~paste0(
      "<b>", facility_name_cms, "</b><br>",
      "ID: ", facility_id, "<br>",
      "City: ", city_cms, ", ", state_cms, "<br>",
      "Stations: ", dialysis_station_count
    ),
    label = ~facility_name_cms
  )

# Add layer controls for toggling isochrones and chapter borders
map <- map %>%
  addLayersControl(
    overlayGroups = c("Chapters",
                      "Isochrones",
                      "Census Blocks (Navajo)",
                      "Census Blocks (Isochrones only)",
                      "F_d Facilities"),
    options = layersControlOptions(collapsed = FALSE)
  )

# Save map
cat("Saving map...\n")
saveWidget(map, "outputs_maps/04_blocks_verification.html", selfcontained = FALSE)

cat("\n✓ Map saved to: outputs_maps/04_blocks_verification.html\n")
cat("\nMap includes:\n")
cat("  -", nrow(blocks_navajo), "blocks >50% in Navajo Nation (colored by chapter via majority area)\n")
cat("  -", nrow(blocks_iso_only), "blocks intersecting isochrones but <50% in Navajo (gray)\n")
cat("  - 41 F_d facilities (green markers)\n")
cat("  - All isochrone bands: 0-15 min (green), 15-30 min (yellow), 30-60 min (red)\n")
cat("  - 111 Navajo chapters (blue borders)\n")
