# =============================================================================
# 01_facilities_clean.R
# Purpose: Load CMS dialysis facility data, verify via Google Maps Places API,
#          geocode confirmed facilities, and output cleaned dataset
# Inputs:  data_raw/cms_dialysis_facilities.csv
# Outputs: data_processed/facilities_geocoded_verified.csv
#          outputs_maps/01_facilities_verification.html
# =============================================================================

library(tidyverse)
library(httr)
library(jsonlite)
library(leaflet)
library(htmlwidgets)
library(stringdist)
library(janitor)

# ── Configuration ─────────────────────────────────────────────────────────────
# Load API key from environment variable
# Set in .Renviron file or via Sys.setenv(GOOGLE_API_KEY = "your-key-here")
GOOGLE_API_KEY <- Sys.getenv("GOOGLE_API_KEY")
if (GOOGLE_API_KEY == "") {
  stop("GOOGLE_API_KEY environment variable not set. ",
       "Add to .Renviron or set via Sys.setenv(GOOGLE_API_KEY = 'your-key')")
}

RAW_FILE       <- "data_raw/cms_dialysis_facilities.csv"
TARGET_STATES  <- c("AZ", "NM", "UT", "CO")

# ── 1. Load and filter raw CMS data ───────────────────────────────────────────
cat("Loading raw CMS dialysis facility data...\n")

raw <- read_csv(RAW_FILE, show_col_types = FALSE) %>%
  clean_names()

cat("Raw rows loaded:", nrow(raw), "\n")

# Rename to standardized column names
fac <- raw %>%
  rename(
    facility_id            = any_of(c("cms_certification_number_ccn", "ccn", "facility_id")),
    facility_name_cms      = any_of(c("facility_name", "name")),
    address_line_1_cms     = any_of(c("address_line_1", "address")),
    address_line_2_cms     = any_of(c("address_line_2", "suite")),
    city_cms               = any_of(c("city_town", "city")),
    state_cms              = any_of(c("state")),
    zip_code_cms           = any_of(c("zip_code", "zip")),
    county_cms             = any_of(c("county_parish", "county")),
    telephone_cms          = any_of(c("telephone_number", "phone_number", "phone")),
    dialysis_station_count = any_of(c("number_of_dialysis_stations",
                                       "dialysis_stations", "stations"))
  )

# Filter: target states + non-zero stations
fac_filtered <- fac %>%
  filter(state_cms %in% TARGET_STATES) %>%
  filter(!is.na(dialysis_station_count), dialysis_station_count > 0)

cat("After filtering (states + stations > 0):", nrow(fac_filtered), "facilities\n")

# ── 2. Verify facilities via Google Maps Places API ──────────────────────────
# Uses findplacefromtext endpoint
# Verification logic: street address + city match (provider names may differ)

verify_via_places <- function(facility_name, address1, address2, city, state, zip, api_key) {
  # Construct search query
  address_full <- paste(
    address1,
    ifelse(!is.na(address2) && address2 != "", address2, ""),
    city, state, zip
  ) %>% str_squish()

  query <- paste(facility_name, address_full, sep = ", ")

  url <- "https://maps.googleapis.com/maps/api/place/findplacefromtext/json"

  resp <- GET(url, query = list(
    input = query,
    inputtype = "textquery",
    fields = "name,formatted_address,place_id,geometry",
    key = api_key
  ))

  if (http_error(resp)) {
    return(list(
      status = "API_ERROR",
      facility_name_google = NA,
      formatted_address_google = NA,
      place_id_google = NA,
      lat_places = NA,
      lng_places = NA,
      match_status = "manual"
    ))
  }

  body   <- content(resp, as = "text", encoding = "UTF-8")
  parsed <- fromJSON(body, simplifyVector = FALSE)

  if (parsed$status != "OK" || length(parsed$candidates) == 0) {
    return(list(
      status = parsed$status,
      facility_name_google = NA,
      formatted_address_google = NA,
      place_id_google = NA,
      lat_places = NA,
      lng_places = NA,
      match_status = "manual"
    ))
  }

  candidate <- parsed$candidates[[1]]

  goog_name    <- candidate$name
  goog_addr    <- candidate$formatted_address
  place_id     <- candidate$place_id
  lat_places   <- candidate$geometry$location$lat
  lng_places   <- candidate$geometry$location$lng

  # Verification logic: street + city match
  # Provider names may legitimately differ at the same address

  city_match <- grepl(city, goog_addr, ignore.case = TRUE)

  # Extract and compare street addresses
  cms_street  <- tolower(trimws(gsub("[^0-9a-z\\s]", "", address1)))
  goog_street <- tolower(trimws(gsub("[^0-9a-z\\s]", "", goog_addr)))

  street_match <- grepl(cms_street, goog_street, fixed = FALSE) ||
                  stringdist(cms_street, goog_street, method = "jw") <= 0.2

  # Match status: verified if street + city match
  match_status <- if_else(street_match && city_match, "verified", "manual")

  list(
    status = "OK",
    facility_name_google = goog_name,
    formatted_address_google = goog_addr,
    place_id_google = place_id,
    lat_places = lat_places,
    lng_places = lng_places,
    match_status = match_status
  )
}

cat("\nVerifying facilities via Google Places API (findplacefromtext)...\n")

results <- vector("list", nrow(fac_filtered))

for (i in seq_len(nrow(fac_filtered))) {
  row <- fac_filtered[i, ]
  results[[i]] <- verify_via_places(
    facility_name = row$facility_name_cms,
    address1      = row$address_line_1_cms,
    address2      = row$address_line_2_cms,
    city          = row$city_cms,
    state         = row$state_cms,
    zip           = row$zip_code_cms,
    api_key       = GOOGLE_API_KEY
  )
  if (i %% 25 == 0) cat("  ...verified", i, "of", nrow(fac_filtered), "\n")
  Sys.sleep(0.15)
}

api_df <- bind_rows(lapply(results, as_tibble))
fac_verified <- bind_cols(fac_filtered, api_df)

# ── 3. Apply manual corrections ──────────────────────────────────────────────
# Manual corrections for facilities with verified reference coordinates
# Coordinates confirmed via Google Maps and reference data validation

manual_corrections <- tribble(
  ~facility_id, ~latitude, ~longitude, ~formatted_address_google,
  # Original manual corrections
  "462504", 40.25753458576979, -111.66220919256065, "1675 Freedom Blvd 200 W Ste 15, Provo, UT 84604",
  "322545", 36.914633409026194, -106.96740474016809, "450 N Mundo Dr, Dulce, NM 87528",
  NA_character_, 35.658670176588586, -109.04031321553796, "1580 NM-264 ste a, Gallup, NM 87301",
  # Corrected coordinates from reference data validation (2026-05-01)
  "463502", 40.74747652, -111.8922903, "5848 S Fashion Blvd #50, Murray, UT 84107, United States",
  "32592", 35.8022226, -110.4260281, "HWY 264 MILE MARKER 388, Polacca, AZ 86042, United States",
  "322524", 35.06919549, -107.5701094, "501 Sunrise Ct, Acoma Pueblo, NM 87034, United States",
  "322531", 35.0979968, -106.6286975, "1500 Indian School Rd NE, Albuquerque, NM 87102, United States",
  "32518", 36.1568845, -109.584472, "US Hwy 191, Chinle, AZ 86503, United States",
  "32559", 36.7104912, -110.2479834, "Highway 163 Box 217, Kayenta, AZ 86033, United States",
  "32633", 33.0194561, -111.3916021, "300 W HIGHWAY 287 #300300, Florence, AZ 85132, United States"
) %>%
  mutate(match_status = "manual")

# Additional facilities verified as correct (upgrade from manual to verified)
verified_overrides <- c(
  "032616", "062556", "062587", "DaVita Dixie Dialysis Center",
  "SRS- Weber Valley, LLC", "DaVita Pleasant View Dialysis"
)

fac_verified <- fac_verified %>%
  mutate(
    match_status = case_when(
      facility_id %in% verified_overrides ~ "verified",
      facility_name_cms %in% verified_overrides ~ "verified",
      TRUE ~ match_status
    )
  )

# Apply manual coordinate corrections
# Match by facility_id for facilities with confirmed coordinates
fac_verified <- fac_verified %>%
  left_join(
    manual_corrections %>%
      filter(!is.na(facility_id)) %>%
      select(facility_id,
             manual_lat = latitude,
             manual_lng = longitude,
             manual_addr = formatted_address_google),
    by = "facility_id"
  ) %>%
  mutate(
    lat_places = coalesce(manual_lat, lat_places),
    lng_places = coalesce(manual_lng, lng_places),
    formatted_address_google = coalesce(manual_addr, formatted_address_google),
    match_status = if_else(!is.na(manual_lat), "manual", match_status)
  ) %>%
  select(-manual_lat, -manual_lng, -manual_addr)

# Handle USRC SOLID ROCK DIALYSIS (no facility_id, match by name)
usrc_idx <- which(str_detect(fac_verified$facility_name_cms, "USRC SOLID ROCK"))
if (length(usrc_idx) > 0) {
  fac_verified$lat_places[usrc_idx] <- 35.658670176588586
  fac_verified$lng_places[usrc_idx] <- -109.04031321553796
  fac_verified$formatted_address_google[usrc_idx] <- "1580 NM-264 ste a, Gallup, NM 87301"
  fac_verified$match_status[usrc_idx] <- "manual"
}

cat("\nMatch status summary:\n")
fac_verified %>% count(match_status) %>% print()

# ── 4. Geocode verified facilities via Geocoding API ─────────────────────────
# Use Geocoding API for final high-precision coordinates

geocode_address <- function(address, api_key) {
  url  <- "https://maps.googleapis.com/maps/api/geocode/json"
  resp <- GET(url, query = list(address = address, key = api_key))

  if (http_error(resp)) return(list(latitude = NA_real_, longitude = NA_real_))

  body   <- content(resp, as = "text", encoding = "UTF-8")
  parsed <- fromJSON(body, simplifyVector = FALSE)

  if (parsed$status != "OK" || length(parsed$results) == 0)
    return(list(latitude = NA_real_, longitude = NA_real_))

  loc <- parsed$results[[1]]$geometry$location
  list(latitude = loc$lat, longitude = loc$lng)
}

cat("\nGeocoding all facilities via Geocoding API for final coordinates...\n")

gc_results <- vector("list", nrow(fac_verified))

for (i in seq_len(nrow(fac_verified))) {
  # Use Google's formatted address for verified facilities
  # Use manual address for manual facilities
  # Fall back to CMS address if needed
  addr <- coalesce(
    fac_verified$formatted_address_google[i],
    paste(fac_verified$address_line_1_cms[i],
          fac_verified$city_cms[i],
          fac_verified$state_cms[i],
          fac_verified$zip_code_cms[i])
  )

  gc_results[[i]] <- geocode_address(addr, GOOGLE_API_KEY)
  if (i %% 25 == 0) cat("  ...geocoded", i, "of", nrow(fac_verified), "\n")
  Sys.sleep(0.15)
}

gc_df <- bind_rows(lapply(gc_results, as_tibble))
fac_geocoded <- bind_cols(fac_verified, gc_df)

# For manual facilities, use the manual coordinates as final
fac_geocoded <- fac_geocoded %>%
  mutate(
    latitude = if_else(match_status == "manual" & !is.na(lat_places),
                       lat_places, latitude),
    longitude = if_else(match_status == "manual" & !is.na(lng_places),
                        lng_places, longitude)
  )

missing_coords <- sum(is.na(fac_geocoded$latitude))
if (missing_coords > 0) {
  warning(missing_coords, " facilities still missing coordinates after geocoding.")
}

# ── 5. Save output ────────────────────────────────────────────────────────────
# Select and order columns per specification

fac_final <- fac_geocoded %>%
  select(
    facility_id,
    facility_name_cms,
    address_line_1_cms,
    address_line_2_cms,
    city_cms,
    state_cms,
    zip_code_cms,
    county_cms,
    telephone_cms,
    dialysis_station_count,
    facility_name_google,
    formatted_address_google,
    place_id_google,
    latitude,
    longitude,
    match_status
  )

output_path <- "data_processed/facilities_geocoded_verified.csv"
write_csv(fac_final, output_path)

cat("\n✓ Facilities saved to:", output_path, "\n")
cat("  Total facilities:", nrow(fac_final), "\n")
cat("  Verified:", sum(fac_final$match_status == "verified"), "\n")
cat("  Manual:", sum(fac_final$match_status == "manual"), "\n")
cat("  Missing coords:", sum(is.na(fac_final$latitude)), "\n")

# ── 6. Generate verification map ──────────────────────────────────────────────

pal <- colorFactor(
  palette = c("verified" = "#2166ac", "manual" = "#f4a582"),
  domain  = c("verified", "manual")
)

map <- leaflet(fac_final) %>%
  addProviderTiles("CartoDB.Positron") %>%
  addCircleMarkers(
    lng         = ~longitude,
    lat         = ~latitude,
    radius      = 7,
    color       = ~pal(match_status),
    fillOpacity = 0.85,
    stroke      = TRUE,
    weight      = 1,
    popup       = ~paste0(
      "<b>", facility_name_cms, "</b><br>",
      address_line_1_cms, ", ", city_cms, ", ", state_cms, "<br>",
      "<i>Google:</i> ", facility_name_google, "<br>",
      "<i>Status:</i> ", match_status, "<br>",
      "<i>Stations:</i> ", dialysis_station_count
    )
  ) %>%
  addLegend(
    position = "bottomright",
    colors   = c("#2166ac", "#f4a582"),
    labels   = c("Verified (API)", "Manual"),
    title    = "Match Status"
  ) %>%
  addControl(
    html     = paste0("<b>CMS Dialysis Facilities — ",
                      paste(TARGET_STATES, collapse = "/"), "</b><br>",
                      nrow(fac_final), " facilities verified"),
    position = "topright"
  )

map_path <- "outputs_maps/01_facilities_verification.html"
saveWidget(map, file = map_path, selfcontained = FALSE)
cat("✓ Verification map saved to:", map_path, "\n")

# ── 7. Summary ────────────────────────────────────────────────────────────────
cat("\n── FINAL SUMMARY ──────────────────────────────────────────────────────\n")
cat("Input file:", RAW_FILE, "\n")
cat("Output file:", output_path, "\n")
cat("States:", paste(TARGET_STATES, collapse = ", "), "\n")
cat("Total facilities:", nrow(fac_final), "\n")
cat("  Verified (API):", sum(fac_final$match_status == "verified"), "\n")
cat("  Manual:", sum(fac_final$match_status == "manual"), "\n")
cat("Missing coordinates:", sum(is.na(fac_final$latitude)), "\n")
cat("───────────────────────────────────────────────────────────────────────\n")
