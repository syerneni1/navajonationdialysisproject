# =============================================================================
# 02_dine_boundary.R
# Purpose: Retrieve Navajo Nation chapter boundaries from Census TIGERweb
#          and create agency-level and nation-level boundaries
# Inputs:  Census TIGERweb AIANNHA MapServer REST API
# Outputs: data_processed/navajo_chapters.geojson
#          data_processed/navajo_agencies.geojson
#          data_processed/navajo_nation.geojson
#          outputs_maps/02_dine_boundary_verification.html
# =============================================================================

library(tidyverse)
library(sf)
library(httr)
library(jsonlite)
library(leaflet)
library(htmlwidgets)

# Turn off s2 spherical geometry for Census data compatibility
sf_use_s2(FALSE)

# ── 1. Retrieve chapter boundaries from TIGERweb ─────────────────────────────

# Census TIGERweb AIANNHA MapServer endpoint
base_url <- "https://tigerweb.geo.census.gov/arcgis/rest/services/TIGERweb/AIANNHA/MapServer/1/query"

# Query parameters
# AIANNH='2430' = Navajo Nation
# MTFCC='G2300' = American Indian Tribal Subdivision
query_params <- list(
  where = "AIANNH='2430' AND MTFCC='G2300'",
  outFields = "*",
  outSR = "4326",
  f = "geojson"
)

cat("Querying Census TIGERweb for Navajo Nation chapter boundaries...\n")

# First request to get total count
count_params <- c(query_params, returnCountOnly = "true")
count_resp <- GET(base_url, query = count_params)
count_result <- content(count_resp, as = "text", encoding = "UTF-8") %>% fromJSON()
total_features <- count_result$count

cat("Total features available:", total_features, "\n")

# Paginate to retrieve all features (max 1000 per request)
max_records <- 1000
all_features <- list()
offset <- 0

repeat {
  cat("  Retrieving features", offset + 1, "to", min(offset + max_records, total_features), "\n")

  page_params <- c(query_params,
                   resultRecordCount = max_records,
                   resultOffset = offset)

  resp <- GET(base_url, query = page_params)

  if (http_error(resp)) {
    stop("API request failed with status: ", status_code(resp))
  }

  geojson_text <- content(resp, as = "text", encoding = "UTF-8")
  features_sf <- st_read(geojson_text, quiet = TRUE)

  all_features[[length(all_features) + 1]] <- features_sf

  offset <- offset + nrow(features_sf)

  if (offset >= total_features) break

  Sys.sleep(0.5)  # Rate limiting
}

# Combine all pages
chapters_raw <- bind_rows(all_features)

cat("\nRaw features retrieved:", nrow(chapters_raw), "\n")

# ── 2. Filter out unwanted features ───────────────────────────────────────────

# Exclude:
# 1. Winslow Tract (not a designated chapter)
# 2. Features where BASENAME is null or empty

chapters_filtered <- chapters_raw %>%
  filter(
    !is.na(BASENAME),
    BASENAME != "",
    BASENAME != "Winslow Tract"
  )

cat("After filtering:", nrow(chapters_filtered), "features\n")
cat("  (Expected: 111 features = 110 chapters + 2 San Juan Southern Paiute polygons)\n")

# ── 3. Assign chapters to agencies ────────────────────────────────────────────

# Agency lookup table
agency_lookup <- tribble(
  ~BASENAME, ~AGENCY,
  # Chinle Agency
  "Black Mesa", "Chinle",
  "Chinle", "Chinle",
  "Forest Lake", "Chinle",
  "Hard Rock", "Chinle",
  "Low Mountain", "Chinle",
  "Lukachukai", "Chinle",
  "Many Farms", "Chinle",
  "Nazlini", "Chinle",
  "Piñon", "Chinle",
  "Rough Rock", "Chinle",
  "Round Rock", "Chinle",
  "Tachee", "Chinle",
  "Tsaile-Wheatfields", "Chinle",
  "Tselani", "Chinle",
  "Whippoorwill", "Chinle",
  # Crownpoint Agency
  "Alamo", "Crownpoint",
  "Baca", "Crownpoint",
  "Becenti", "Crownpoint",
  "Bread Springs", "Crownpoint",
  "Cañoncito", "Crownpoint",
  "Casamero Lake", "Crownpoint",
  "Chi Chil Tah", "Crownpoint",
  "Church Rock", "Crownpoint",
  "Counselor", "Crownpoint",
  "Crownpoint", "Crownpoint",
  "Huerfano", "Crownpoint",
  "Iyanbito", "Crownpoint",
  "Lake Valley", "Crownpoint",
  "Littlewater", "Crownpoint",
  "Manuelito", "Crownpoint",
  "Mariano Lake", "Crownpoint",
  "Nageezi", "Crownpoint",
  "Nahodishgish", "Crownpoint",
  "Ojo Encino", "Crownpoint",
  "Pinedale", "Crownpoint",
  "Pueblo Pintado", "Crownpoint",
  "Ramah", "Crownpoint",
  "Red Rock", "Crownpoint",
  "Rock Springs", "Crownpoint",
  "Smith Lake", "Crownpoint",
  "Standing Rock", "Crownpoint",
  "Thoreau", "Crownpoint",
  "Torreon", "Crownpoint",
  "Tsayatoh", "Crownpoint",
  "White Horse Lake", "Crownpoint",
  "White Rock", "Crownpoint",
  # Fort Defiance Agency
  "Cornfields", "Fort Defiance",
  "Coyote Canyon", "Fort Defiance",
  "Crystal", "Fort Defiance",
  "Dilcon", "Fort Defiance",
  "Fort Defiance", "Fort Defiance",
  "Ganado", "Fort Defiance",
  "Greasewood", "Fort Defiance",
  "Houck", "Fort Defiance",
  "Indian Wells", "Fort Defiance",
  "Jeddito", "Fort Defiance",
  "Kinlichee", "Fort Defiance",
  "Klagetoh", "Fort Defiance",
  "Lupton", "Fort Defiance",
  "Mexican Springs", "Fort Defiance",
  "Nahatadziil", "Fort Defiance",
  "Naschitti", "Fort Defiance",
  "Oak Springs", "Fort Defiance",
  "Red Lake", "Fort Defiance",
  "Sawmill", "Fort Defiance",
  "St. Michaels", "Fort Defiance",
  "Steamboat", "Fort Defiance",
  "Teesto", "Fort Defiance",
  "Tohatchi", "Fort Defiance",
  "Twin Lakes", "Fort Defiance",
  "White Cone", "Fort Defiance",
  "Wide Ruins", "Fort Defiance",
  # Shiprock Agency
  "Aneth", "Shiprock",
  "Beclabito", "Shiprock",
  "Burnham", "Shiprock",
  "Cove", "Shiprock",
  "Fruitland", "Shiprock",
  "Gadii'ahi", "Shiprock",
  "Hogback", "Shiprock",
  "Mexican Water", "Shiprock",
  "Nenahnezad/San Juan", "Shiprock",
  "Newcomb", "Shiprock",
  "Red Mesa", "Shiprock",
  "Red Valley", "Shiprock",
  "Rock Point", "Shiprock",
  "Sanostee", "Shiprock",
  "Sheep Springs", "Shiprock",
  "Shiprock", "Shiprock",
  "Sweetwater", "Shiprock",
  "Teec Nos Pos", "Shiprock",
  "Two Grey Hills", "Shiprock",
  "Upper Fruitland", "Shiprock",
  # Tuba City Agency
  "Bird Springs", "Tuba City",
  "Bodaway", "Tuba City",
  "Cameron", "Tuba City",
  "Chilchinbeto", "Tuba City",
  "Coalmine Mesa", "Tuba City",
  "Coppermine", "Tuba City",
  "Dennehotso", "Tuba City",
  "Inscription House", "Tuba City",
  "Kaibeto", "Tuba City",
  "Kayenta", "Tuba City",
  "LeChee", "Tuba City",
  "Leupp", "Tuba City",
  "Navajo Mountain", "Tuba City",
  "Oljato", "Tuba City",
  "Shonto", "Tuba City",
  "Tolani Lake", "Tuba City",
  "Tonalea", "Tuba City",
  "Tuba City", "Tuba City",
  # San Juan Southern Paiute (assigned to Tuba City per methodology)
  "San Juan Southern Paiute Northern", "Tuba City",
  "San Juan Southern Paiute Southern", "Tuba City"
)

# Join agency assignments
chapters <- chapters_filtered %>%
  left_join(agency_lookup, by = "BASENAME")

# Check for unmatched chapters
unmatched <- chapters %>%
  filter(is.na(AGENCY)) %>%
  pull(BASENAME)

if (length(unmatched) > 0) {
  warning("Unmatched chapters: ", paste(unmatched, collapse = ", "))
}

cat("\nAgency assignments:\n")
chapters %>%
  st_drop_geometry() %>%
  count(AGENCY) %>%
  print()

# ── 4. Save chapter boundaries ────────────────────────────────────────────────

# Save all chapters including San Juan Southern Paiute
st_write(chapters, "data_processed/navajo_chapters.geojson",
         delete_dsn = TRUE, quiet = TRUE)

cat("\n✓ Chapter boundaries saved to: data_processed/navajo_chapters.geojson\n")
cat("  Total chapters:", nrow(chapters), "\n")

# ── 5. Dissolve to agency boundaries ──────────────────────────────────────────

cat("\nDissolving to agency boundaries...\n")

# Group by agency and dissolve
# San Juan Southern Paiute already assigned to Tuba City in lookup table
agencies <- chapters %>%
  st_make_valid() %>%  # Fix any invalid geometries
  group_by(AGENCY) %>%
  summarise(
    n_chapters = n(),
    .groups = "drop"
  ) %>%
  st_cast("MULTIPOLYGON")  # Ensure consistent geometry type

st_write(agencies, "data_processed/navajo_agencies.geojson",
         delete_dsn = TRUE, quiet = TRUE)

cat("✓ Agency boundaries saved to: data_processed/navajo_agencies.geojson\n")
cat("  Total agencies:", nrow(agencies), "\n")

agencies %>%
  st_drop_geometry() %>%
  arrange(AGENCY) %>%
  print()

# ── 6. Dissolve to nation boundary ────────────────────────────────────────────

cat("\nDissolving to nation boundary...\n")

# Dissolve all chapters (including San Juan Southern Paiute) into nation boundary
# Apply buffer(0) to clean topology, then union
cat("  Including all", nrow(chapters), "features in nation boundary\n")
cat("  (110 Navajo chapters + 1 combined Nenahnezad/San Juan + 2 San Juan Southern Paiute)\n")

nation <- chapters %>%
  st_make_valid() %>%  # Fix any invalid geometries
  st_buffer(0) %>%     # Clean up topology issues
  st_union() %>%       # Merge ALL chapters into single geometry
  st_sf(
    AIANNH = "2430",
    NAME = "Navajo Nation (incl. San Juan Southern Paiute)",
    n_chapters = nrow(chapters),
    geometry = .
  ) %>%
  st_cast("MULTIPOLYGON")  # Ensure multipolygon type

st_write(nation, "data_processed/navajo_nation.geojson",
         delete_dsn = TRUE, quiet = TRUE)

cat("✓ Nation boundary saved to: data_processed/navajo_nation.geojson\n")
cat("  Total area covers", nation$n_chapters, "chapters\n")

# ── 7. Produce verification map ───────────────────────────────────────────────

cat("\nGenerating verification map...\n")

# Color palette for agencies
agency_pal <- colorFactor(
  palette = c("#e41a1c", "#377eb8", "#4daf4a", "#984ea3", "#ff7f00", "#a65628"),
  domain = agencies$AGENCY
)

# Create map
map <- leaflet() %>%
  addProviderTiles("CartoDB.Positron") %>%
  # Agency fills
  addPolygons(
    data = agencies,
    fillColor = ~agency_pal(AGENCY),
    fillOpacity = 0.4,
    color = "#666666",
    weight = 2,
    opacity = 0.8,
    popup = ~paste0("<b>", AGENCY, " Agency</b><br>",
                    n_chapters, " chapters"),
    group = "Agencies"
  ) %>%
  # Chapter borders (clickable with agency info)
  addPolygons(
    data = chapters,
    fillColor = "transparent",
    fillOpacity = 0,
    color = "#333333",
    weight = 1,
    opacity = 0.6,
    popup = ~paste0("<b>Chapter:</b> ", BASENAME, "<br>",
                    "<b>Agency:</b> ", AGENCY),
    highlightOptions = highlightOptions(
      color = "#FF0000",
      weight = 3,
      bringToFront = TRUE
    ),
    group = "Chapters"
  ) %>%
  # Nation boundary (bold outer border only)
  addPolylines(
    data = st_cast(st_boundary(nation), "MULTILINESTRING"),
    color = "#000000",
    weight = 4,
    opacity = 1,
    popup = ~paste0("<b>", NAME, "</b><br>",
                    n_chapters, " chapters"),
    group = "Nation"
  ) %>%
  # Legend
  addLegend(
    position = "bottomright",
    pal = agency_pal,
    values = agencies$AGENCY,
    title = "BIA Agency",
    opacity = 0.8
  ) %>%
  # Layer controls
  addLayersControl(
    overlayGroups = c("Agencies", "Chapters", "Nation"),
    options = layersControlOptions(collapsed = FALSE)
  ) %>%
  addControl(
    html = paste0("<b>Navajo Nation (Diné Bikéyah)</b><br>",
                  nrow(chapters), " chapters | ",
                  nrow(agencies), " agencies"),
    position = "topright"
  )

map_path <- "outputs_maps/02_dine_boundary_verification.html"
saveWidget(map, file = map_path, selfcontained = FALSE)

cat("✓ Verification map saved to:", map_path, "\n")

# ── 8. Summary ────────────────────────────────────────────────────────────────

cat("\n── FINAL SUMMARY ──────────────────────────────────────────────────────\n")
cat("Chapters retrieved:", nrow(chapters), "\n")
cat("Agencies:", nrow(agencies), "\n")
cat("  ", paste(sort(agencies$AGENCY), collapse = ", "), "\n")
cat("\nOutputs:\n")
cat("  data_processed/navajo_chapters.geojson\n")
cat("  data_processed/navajo_agencies.geojson\n")
cat("  data_processed/navajo_nation.geojson\n")
cat("  outputs_maps/02_dine_boundary_verification.html\n")
cat("───────────────────────────────────────────────────────────────────────\n")
