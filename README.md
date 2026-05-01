# Navajo Nation Dialysis Spatial Access Analysis

E2SFCA spatial accessibility analysis of hemodialysis care for Navajo Nation residents.

## Setup

### 1. Google Maps API Key

The facility cleaning script requires a Google Maps API key with access to:
- Places API
- Geocoding API

**To set up:**

Create a `.Renviron` file in the project root:
```
GOOGLE_API_KEY=your-google-api-key-here
```

This file is gitignored and will not be committed.

### 2. Required R Packages

```r
install.packages(c(
  "tidyverse",
  "httr",
  "jsonlite",
  "leaflet",
  "htmlwidgets",
  "stringdist",
  "janitor"
))
```

### 3. Run Script 01

```r
source("scripts/01_facilities_clean.R")
```

**Outputs:**
- `data_processed/facilities_geocoded_verified.csv` - Verified facility locations
- `outputs_maps/01_facilities_verification.html` - Interactive verification map

## Project Structure

```
├── data_raw/              # Original downloaded datasets (not modified)
├── data_processed/        # Cleaned spatial files
├── scripts/               # Numbered analysis scripts
├── outputs_maps/          # HTML maps
├── outputs_tables/        # Summary tables and reports
└── docs/                  # Documentation
```
