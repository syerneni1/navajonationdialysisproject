# =============================================================================
# 03_facility_isochrones.R
# Purpose: Generate drive-time isochrones for all facilities, convert to
#          non-overlapping bands, and filter to define F_d (facilities whose
#          service area intersects Navajo Nation)
# Inputs:  data_processed/facilities_geocoded_verified.csv
#          data_processed/navajo_nation.geojson
# Outputs: data_processed/isochrone_cache.geojson
#          data_processed/isochrones_Fd.geojson
#          data_processed/facilities_Fd.csv
#          outputs_maps/03_isochrones_verification.html
# =============================================================================

library(tidyverse)
library(sf)
library(httr)
library(jsonlite)
library(leaflet)
library(htmlwidgets)

# Use GEOS instead of s2 for more tolerant geometry operations
sf_use_s2(FALSE)

# ── Configuration ─────────────────────────────────────────────────────────────
# Load ORS API key from environment variable
ORS_API_KEY <- Sys.getenv("ORS_API_KEY")
if (ORS_API_KEY == "") {
  stop("ORS_API_KEY environment variable not set. ",
       "Add to .Renviron or set via Sys.setenv(ORS_API_KEY = 'your-key')")
}

# Impedance weights (evaluated at starting point of each band)
WEIGHTS <- tribble(
  ~band,        ~d_start, ~weight,
  "0-15 min",   0,        1.00,
  "15-30 min",  15,       0.75,
  "30-60 min",  30,       0.32
)

FACILITIES_FILE <- "data_processed/facilities_geocoded_verified.csv"
NAVAJO_FILE     <- "data_processed/navajo_nation.geojson"
CACHE_FILE      <- "data_processed/isochrone_cache.geojson"
ISOCHRONES_FILE <- "data_processed/isochrones_Fd.geojson"
FACILITIES_FD   <- "data_processed/facilities_Fd.csv"
MAP_FILE        <- "outputs_maps/03_isochrones_verification.html"

# ── 1. Load Facilities ────────────────────────────────────────────────────────
cat("Loading facilities...\n")
facilities <- read_csv(FACILITIES_FILE, show_col_types = FALSE)
cat("  Total facilities:", nrow(facilities), "\n\n")

# ── 2. Generate Isochrones via ORS API ────────────────────────────────────────
cat("Generating isochrones via OpenRouteService API...\n")

# Check if cache exists
if (file.exists(CACHE_FILE)) {
  cat("  Loading from cache:", CACHE_FILE, "\n")
  isochrones_sf <- st_read(CACHE_FILE, quiet = TRUE)
  cat("  Loaded", nrow(isochrones_sf), "cached isochrones\n\n")
} else {
  cat("  No cache found - calling ORS API for all facilities\n")
  cat("  This may take several minutes...\n\n")

  # Function to call ORS Isochrones API
  get_isochrones <- function(lon, lat, facility_id, api_key) {
    url <- "https://api.openrouteservice.org/v2/isochrones/driving-car"

    body <- list(
      locations = list(c(lon, lat)),
      range = c(900, 1800, 3600),  # 15, 30, 60 minutes in seconds
      range_type = "time"
    )

    resp <- POST(
      url,
      add_headers(Authorization = api_key),
      body = body,
      encode = "json"
    )

    if (http_error(resp)) {
      warning("API error for facility ", facility_id, ": ", http_status(resp)$message)
      return(NULL)
    }

    content <- content(resp, as = "text", encoding = "UTF-8")
    geojson <- fromJSON(content, simplifyVector = FALSE)

    # Extract polygons
    features <- geojson$features
    if (length(features) == 0) return(NULL)

    # Convert to sf
    polygons <- lapply(seq_along(features), function(i) {
      coords <- features[[i]]$geometry$coordinates[[1]]
      # ORS returns [lon, lat] format - convert to numeric matrix
      poly_matrix <- matrix(
        as.numeric(unlist(coords)),
        ncol = 2,
        byrow = TRUE
      )
      st_polygon(list(poly_matrix))
    })

    # Create sf object with metadata
    time_limits <- c(15, 30, 60)  # minutes

    sf_df <- st_sf(
      facility_id = facility_id,
      time_limit = time_limits[1:length(polygons)],
      geometry = st_sfc(polygons, crs = 4326)
    )

    return(sf_df)
  }

  # Call API for all facilities
  all_isochrones <- list()

  for (i in seq_len(nrow(facilities))) {
    fac <- facilities[i, ]

    iso <- get_isochrones(
      lon = fac$longitude,
      lat = fac$latitude,
      facility_id = fac$facility_id,
      api_key = ORS_API_KEY
    )

    if (!is.null(iso)) {
      all_isochrones[[i]] <- iso
    }

    if (i %% 25 == 0) {
      cat("  ...processed", i, "of", nrow(facilities), "\n")
    }

    # Rate limiting: ORS free tier = 40 requests/minute
    Sys.sleep(1.5)
  }

  # Combine all isochrones
  isochrones_sf <- bind_rows(all_isochrones)

  # Save cache
  st_write(isochrones_sf, CACHE_FILE, delete_dsn = TRUE, quiet = TRUE)
  cat("\n✓ Cached isochrones to:", CACHE_FILE, "\n\n")
}

# ── 3. Convert to Non-Overlapping Bands ───────────────────────────────────────
cat("Converting to non-overlapping bands...\n")

# Process each facility separately to create non-overlapping rings
facility_ids <- unique(isochrones_sf$facility_id)
all_bands <- list()

for (fid in facility_ids) {
  # Get all isochrones for this facility
  fac_iso <- isochrones_sf %>%
    filter(facility_id == fid) %>%
    arrange(time_limit)

  if (nrow(fac_iso) != 3) {
    warning("Facility ", fid, " has ", nrow(fac_iso), " isochrones (expected 3)")
    next
  }

  # Extract the three nested polygons
  iso_15 <- fac_iso %>% filter(time_limit == 15) %>% pull(geometry)
  iso_30 <- fac_iso %>% filter(time_limit == 30) %>% pull(geometry)
  iso_60 <- fac_iso %>% filter(time_limit == 60) %>% pull(geometry)

  # Create non-overlapping rings
  # Ring 1: 0-15 min (innermost, no subtraction)
  ring1 <- st_sf(
    facility_id = fid,
    band = "0-15 min",
    time_limit = 15,
    geometry = iso_15
  )

  # Ring 2: 15-30 min (subtract ring 1)
  ring2_geom <- st_difference(iso_30, iso_15)
  ring2 <- st_sf(
    facility_id = fid,
    band = "15-30 min",
    time_limit = 30,
    geometry = ring2_geom
  )

  # Ring 3: 30-60 min (subtract rings 1 and 2)
  ring3_geom <- st_difference(iso_60, st_union(c(iso_15, iso_30)))
  ring3 <- st_sf(
    facility_id = fid,
    band = "30-60 min",
    time_limit = 60,
    geometry = ring3_geom
  )

  # Combine rings for this facility
  all_bands[[fid]] <- bind_rows(ring1, ring2, ring3)
}

# Combine all facilities
isochrones_nonoverlap <- bind_rows(all_bands)

# Add impedance weights
isochrones_weighted <- isochrones_nonoverlap %>%
  left_join(WEIGHTS, by = "band") %>%
  select(facility_id, band, time_limit, d_start, weight, geometry)

cat("  Total isochrone bands:", nrow(isochrones_weighted), "\n")
cat("  Bands per facility: 3 (0-15, 15-30, 30-60 min)\n\n")

# ── 4. Define F_d ─────────────────────────────────────────────────────────────
cat("Filtering facilities by Navajo Nation intersection...\n")

# Load Navajo Nation boundary
navajo <- st_read(NAVAJO_FILE, quiet = TRUE)

# For each facility, test if 60-min isochrone intersects Navajo Nation
facilities_with_60min <- isochrones_weighted %>%
  filter(time_limit == 60) %>%
  select(facility_id, geometry)

# Test intersection
intersects <- st_intersects(facilities_with_60min, navajo, sparse = FALSE)
facilities_Fd_ids <- facilities_with_60min$facility_id[rowSums(intersects) > 0]

cat("  Facilities before filtering:", n_distinct(isochrones_weighted$facility_id), "\n")
cat("  Facilities in F_d (intersect Navajo Nation):", length(facilities_Fd_ids), "\n")
cat("  Facilities removed:", n_distinct(isochrones_weighted$facility_id) - length(facilities_Fd_ids), "\n\n")

# Filter isochrones to F_d only
isochrones_Fd <- isochrones_weighted %>%
  filter(facility_id %in% facilities_Fd_ids)

# Filter facilities list
facilities_Fd <- facilities %>%
  filter(facility_id %in% facilities_Fd_ids)

# ── 5. Save Outputs ───────────────────────────────────────────────────────────
cat("Saving outputs...\n")

st_write(isochrones_Fd, ISOCHRONES_FILE, delete_dsn = TRUE, quiet = TRUE)
write_csv(facilities_Fd, FACILITIES_FD)

cat("✓ Filtered isochrones saved to:", ISOCHRONES_FILE, "\n")
cat("✓ Filtered facilities saved to:", FACILITIES_FD, "\n")
cat("  F_d contains:", nrow(facilities_Fd), "facilities\n")
cat("  With", nrow(isochrones_Fd), "isochrone bands\n\n")

# ── 6. Generate Verification Map ──────────────────────────────────────────────
cat("Generating verification map...\n")

# Prepare data for mapping
facilities_all <- facilities %>%
  mutate(in_Fd = facility_id %in% facilities_Fd_ids)

# Get 60-min isochrones for visualization
iso_60min <- isochrones_weighted %>%
  filter(time_limit == 60)

# Create map
map <- leaflet() %>%
  addTiles() %>%
  setView(lng = -109.5, lat = 35.5, zoom = 6)

# Add Navajo Nation boundary
map <- map %>%
  addPolygons(
    data = navajo,
    fillColor = "transparent",
    color = "blue",
    weight = 2,
    opacity = 0.8,
    label = "Navajo Nation"
  )

# Add 60-min isochrones for F_d facilities
iso_Fd_60 <- iso_60min %>%
  filter(facility_id %in% facilities_Fd_ids)

if (nrow(iso_Fd_60) > 0) {
  map <- map %>%
    addPolygons(
      data = iso_Fd_60,
      fillColor = "orange",
      fillOpacity = 0.2,
      color = "orange",
      weight = 1,
      label = ~paste0("Facility ", facility_id, " - 60 min")
    )
}

# Add all facilities (grey for excluded, colored for F_d)
map <- map %>%
  addCircleMarkers(
    data = facilities_all,
    lng = ~longitude,
    lat = ~latitude,
    radius = 5,
    color = ~ifelse(in_Fd, "green", "grey"),
    fillColor = ~ifelse(in_Fd, "green", "grey"),
    fillOpacity = 0.7,
    popup = ~paste0(
      "<b>", facility_name_cms, "</b><br>",
      "ID: ", facility_id, "<br>",
      "City: ", city_cms, ", ", state_cms, "<br>",
      "Status: ", ifelse(in_Fd, "In F_d", "Excluded")
    ),
    label = ~paste0(facility_name_cms, " - ", ifelse(in_Fd, "F_d", "Excluded"))
  )

# Add legend
map <- map %>%
  addLegend(
    position = "topright",
    colors = c("green", "grey", "orange", "blue"),
    labels = c(
      paste0("F_d Facilities (", sum(facilities_all$in_Fd), ")"),
      paste0("Excluded (", sum(!facilities_all$in_Fd), ")"),
      "60-min Isochrones",
      "Navajo Nation"
    ),
    title = "Step 3: Facility Isochrones"
  )

# Save map
saveWidget(map, MAP_FILE, selfcontained = FALSE)
cat("✓ Verification map saved to:", MAP_FILE, "\n\n")

# ── 7. Summary ────────────────────────────────────────────────────────────────
cat("── FINAL SUMMARY ──────────────────────────────────────────────────────\n")
cat("Total facilities analyzed:", nrow(facilities), "\n")
cat("F_d facilities (service area intersects Navajo Nation):", nrow(facilities_Fd), "\n")
cat("Excluded facilities:", nrow(facilities) - nrow(facilities_Fd), "\n")
cat("Total isochrone bands in F_d:", nrow(isochrones_Fd), "\n")
cat("\nImpedance weights:\n")
print(WEIGHTS)
cat("\nOutputs:\n")
cat(" ", ISOCHRONES_FILE, "\n")
cat(" ", FACILITIES_FD, "\n")
cat(" ", MAP_FILE, "\n")
cat("───────────────────────────────────────────────────────────────────────\n")
