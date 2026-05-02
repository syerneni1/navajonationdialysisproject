# =============================================================================
# 03b_visualize_all_isochrones.R
# Purpose: Create comprehensive map showing ALL isochrones, with F_d highlighted
# =============================================================================

library(dplyr)
library(sf)
library(leaflet)
library(htmlwidgets)

sf_use_s2(FALSE)

cat("Loading data...\n")

# Load all isochrones from cache
all_isochrones <- st_read("data_processed/isochrone_cache.geojson", quiet = TRUE)

# Load F_d facility IDs
facilities_Fd <- read.csv("data_processed/facilities_Fd.csv")
Fd_ids <- facilities_Fd$facility_id

# Load all facilities
facilities_all <- read.csv("data_processed/facilities_geocoded_verified.csv")

# Load Navajo Nation
navajo <- st_read("data_processed/navajo_nation.geojson", quiet = TRUE)

cat("  Total isochrones:", nrow(all_isochrones), "\n")
cat("  F_d facilities:", length(Fd_ids), "\n")
cat("  Total facilities:", nrow(facilities_all), "\n\n")

# Mark which isochrones are in F_d
all_isochrones <- all_isochrones %>%
  mutate(
    in_Fd = facility_id %in% Fd_ids,
    facility_id_char = as.character(facility_id)
  )

# Mark which facilities are in F_d
facilities_all <- facilities_all %>%
  mutate(in_Fd = facility_id %in% Fd_ids)

cat("Creating comprehensive map...\n")

# Create map
map <- leaflet() %>%
  addTiles() %>%
  setView(lng = -109.5, lat = 35.5, zoom = 6)

# Add Navajo Nation boundary
map <- map %>%
  addPolygons(
    data = navajo,
    fillColor = "blue",
    fillOpacity = 0.1,
    color = "blue",
    weight = 2,
    label = "Navajo Nation"
  )

# Add 60-min isochrones for NON-F_d facilities (grey)
iso_60_non_Fd <- all_isochrones %>%
  filter(time_limit == 60, !in_Fd)

if (nrow(iso_60_non_Fd) > 0) {
  map <- map %>%
    addPolygons(
      data = iso_60_non_Fd,
      fillColor = "grey",
      fillOpacity = 0.15,
      color = "grey",
      weight = 1,
      opacity = 0.3,
      label = ~paste0("Facility ", facility_id_char, " - Excluded (60 min)")
    )
}

# Add 60-min isochrones for F_d facilities (colored)
iso_60_Fd <- all_isochrones %>%
  filter(time_limit == 60, in_Fd)

if (nrow(iso_60_Fd) > 0) {
  map <- map %>%
    addPolygons(
      data = iso_60_Fd,
      fillColor = "orange",
      fillOpacity = 0.25,
      color = "orange",
      weight = 2,
      label = ~paste0("Facility ", facility_id_char, " - F_d (60 min)")
    )
}

# Add facility markers
map <- map %>%
  addCircleMarkers(
    data = facilities_all,
    lng = ~longitude,
    lat = ~latitude,
    radius = 4,
    color = ~ifelse(in_Fd, "green", "grey"),
    fillColor = ~ifelse(in_Fd, "green", "grey"),
    fillOpacity = ~ifelse(in_Fd, 0.8, 0.4),
    weight = 1,
    popup = ~paste0(
      "<b>", facility_name_cms, "</b><br>",
      "ID: ", facility_id, "<br>",
      "City: ", city_cms, ", ", state_cms, "<br>",
      "Status: ", ifelse(in_Fd, "<b style='color:green'>In F_d</b>",
                                 "<b style='color:grey'>Excluded</b>")
    ),
    label = ~paste0(facility_name_cms, " - ", ifelse(in_Fd, "F_d", "Excluded"))
  )

# Add legend
map <- map %>%
  addLegend(
    position = "topright",
    colors = c("green", "orange", "grey", "blue"),
    labels = c(
      paste0("F_d Facilities (", sum(facilities_all$in_Fd), ")"),
      "F_d 60-min Isochrones",
      paste0("Excluded Facilities (", sum(!facilities_all$in_Fd), ")"),
      "Navajo Nation"
    ),
    title = "All Facilities & Isochrones",
    opacity = 1
  )

# Save
output_file <- "outputs_maps/03_all_isochrones_comprehensive.html"
saveWidget(map, output_file, selfcontained = FALSE)

cat("✓ Comprehensive map saved to:", output_file, "\n")
cat("\nMap shows:\n")
cat("  - All", nrow(facilities_all), "facilities\n")
cat("  -", sum(facilities_all$in_Fd), "F_d facilities (green) with orange 60-min isochrones\n")
cat("  -", sum(!facilities_all$in_Fd), "Excluded facilities (grey) with grey 60-min isochrones\n")
cat("  - Navajo Nation boundary (blue)\n")
