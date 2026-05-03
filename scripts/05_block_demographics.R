# =============================================================================
# 05_block_demographics.R
# Purpose: Download block demographics and disease prevalence needed for demand formula
# Inputs:  data_processed/blocks_filtered.geojson (for GEOID list)
# Outputs: data_processed/block_demographics.csv (full demographic breakdown)
#          data_processed/cdc_places_tract.csv (tract-level DM and HTN prevalence)
#          outputs_tables/05_hazard_ratios.csv (verification table)
# =============================================================================

library(tidyverse)
library(sf)
library(tidycensus)
library(httr)
library(jsonlite)

# ── Configuration ─────────────────────────────────────────────────────────────
# Census API key should be set via census_api_key() or CENSUS_API_KEY env var

BLOCKS_FILE <- "data_processed/blocks_filtered.geojson"
OUTPUT_DEMOGRAPHICS <- "data_processed/block_demographics.csv"
OUTPUT_CDC <- "data_processed/cdc_places_tract.csv"
OUTPUT_HR_TABLE <- "outputs_tables/05_hazard_ratios.csv"

STATES <- c("AZ", "CO", "NM", "UT")

# ── 1. Load Filtered Blocks ───────────────────────────────────────────────────
cat("Loading filtered blocks...\n")

# Read blocks (compressed version if needed)
if (file.exists(paste0(BLOCKS_FILE, ".gz")) && !file.exists(BLOCKS_FILE)) {
  cat("  Decompressing blocks file...\n")
  system(paste("gunzip -k", paste0(BLOCKS_FILE, ".gz")))
}

blocks <- st_read(BLOCKS_FILE, quiet = TRUE)
block_geoids <- blocks %>%
  st_drop_geometry() %>%
  select(GEOID, tract_geoid, block_pop)

cat("✓ Loaded", nrow(block_geoids), "blocks\n\n")

# ── 2. Download P12H–P12M Demographic Tables ──────────────────────────────────
cat("Downloading P12H-P12M demographic tables for", paste(STATES, collapse = ", "), "...\n")
cat("This will take 5-10 minutes per table\n\n")

# Define race/ethnicity tables
race_tables <- list(
  "Hispanic or Latino" = "P12H",
  "White Alone, Not Hispanic or Latino" = "P12I",
  "Black or African American Alone, Not Hispanic or Latino" = "P12J",
  "American Indian and Alaska Native Alone, Not Hispanic or Latino" = "P12K",
  "Asian Alone, Not Hispanic or Latino" = "P12L",
  "Native Hawaiian and Other Pacific Islander Alone, Not Hispanic or Latino" = "P12M"
)

# Download all tables
demographics_list <- list()

for (race_name in names(race_tables)) {
  table_code <- race_tables[[race_name]]
  cat("  Downloading", table_code, "-", race_name, "...\n")

  table_data <- list()
  for (state in STATES) {
    cat("    State:", state, "\n")

    table_data[[state]] <- get_decennial(
      geography = "block",
      table = table_code,
      year = 2020,
      state = state,
      sumfile = "dhc",
      output = "wide",
      cache_table = TRUE
    ) %>%
      mutate(race = race_name, state = state)
  }

  demographics_list[[race_name]] <- bind_rows(table_data)
  cat("    Loaded", nrow(demographics_list[[race_name]]), "blocks\n\n")
}

# ── 3. Reshape and Sum Age Groups ─────────────────────────────────────────────
cat("Reshaping demographic data and summing age groups...\n")

# Helper function to extract and sum age groups from P12 tables
# Census variables follow pattern: P12X_###N where X is table letter
# Variables are ordered: Total, Male Total, Male ages..., Female Total, Female ages...

reshape_p12_table <- function(df, race_name) {
  # Get all variable names except GEOID, NAME, race, state
  value_cols <- setdiff(names(df), c("GEOID", "NAME", "race", "state"))

  # P12 tables have structure:
  # _001N = Total
  # _002N = Male total
  # _003N-_025N = Male by age (detailed)
  # _026N = Female total
  # _027N-_049N = Female by age (detailed)

  # Age groups in Census (starting at _003N for male, _027N for female):
  # 1: Under 5, 2: 5-9, 3: 10-14, 4: 15-17,
  # 5: 18-19, 6: 20, 7: 21, 8: 22-24, 9: 25-29, 10: 30-34, 11: 35-39, 12: 40-44,
  # 13: 45-49, 14: 50-54, 15: 55-59, 16: 60-61, 17: 62-64,
  # 18: 65-66, 19: 67-69, 20: 70-74,
  # 21: 75-79, 22: 80-84, 23: 85+

  # Map to target age groups (indices):
  # 0-17: 1,2,3,4 (Under 5, 5-9, 10-14, 15-17)
  # 18-44: 5,6,7,8,9,10,11,12 (18-19, 20, 21, 22-24, 25-29, 30-34, 35-39, 40-44)
  # 45-64: 13,14,15,16,17 (45-49, 50-54, 55-59, 60-61, 62-64)
  # 65-74: 18,19,20 (65-66, 67-69, 70-74)
  # 75+: 21,22,23 (75-79, 80-84, 85+)

  table_prefix <- sub("_.*", "", value_cols[1])  # e.g., "P12H"

  age_mapping <- list(
    "0-17" = 1:4,
    "18-44" = 5:12,
    "45-64" = 13:17,
    "65-74" = 18:20,
    "75+" = 21:23
  )

  result <- data.frame()

  for (age_group in names(age_mapping)) {
    indices <- age_mapping[[age_group]]

    # Male variables (start at _003N)
    male_vars <- paste0(table_prefix, "_", sprintf("%03d", indices + 2), "N")
    male_vars <- intersect(male_vars, value_cols)

    # Female variables (start at _027N)
    female_vars <- paste0(table_prefix, "_", sprintf("%03d", indices + 26), "N")
    female_vars <- intersect(female_vars, value_cols)

    # Sum male counts
    male_data <- df %>%
      mutate(
        population = rowSums(select(., all_of(male_vars)), na.rm = TRUE),
        sex = "Male",
        age_group = age_group
      ) %>%
      select(GEOID, race, sex, age_group, population)

    # Sum female counts
    female_data <- df %>%
      mutate(
        population = rowSums(select(., all_of(female_vars)), na.rm = TRUE),
        sex = "Female",
        age_group = age_group
      ) %>%
      select(GEOID, race, sex, age_group, population)

    result <- bind_rows(result, male_data, female_data)
  }

  return(result)
}

# Reshape all race tables
demographics_long <- map2_dfr(
  demographics_list,
  names(demographics_list),
  ~reshape_p12_table(.x, .y)
)

# Combine Asian and NH/PI into single category
demographics_long <- demographics_long %>%
  mutate(
    race = case_when(
      race %in% c("Asian Alone, Not Hispanic or Latino",
                  "Native Hawaiian and Other Pacific Islander Alone, Not Hispanic or Latino") ~ "Asian/NH/PI",
      TRUE ~ race
    )
  ) %>%
  group_by(GEOID, race, sex, age_group) %>%
  summarize(population = sum(population, na.rm = TRUE), .groups = "drop")

cat("✓ Reshaped to long format\n")
cat("  Total rows:", nrow(demographics_long), "\n")
cat("  Demographic strata:", n_distinct(demographics_long$race), "races ×",
    n_distinct(demographics_long$sex), "sexes ×",
    n_distinct(demographics_long$age_group), "age groups\n\n")

# ── 4. Attach Hazard Ratio Multipliers ───────────────────────────────────────
cat("Attaching hazard ratio multipliers (USRDS 2025, Figure 1.2)...\n")

# Define multipliers (baseline: white adult female = 1.0)
hr_sex <- data.frame(
  sex = c("Female", "Male"),
  HR_sex = c(1.000, 1.637)
)

hr_age <- data.frame(
  age_group = c("0-17", "18-44", "45-64", "65-74", "75+"),
  HR_age = c(0.099, 1.000, 5.058, 10.077, 12.922)
)

hr_race <- data.frame(
  race = c("White Alone, Not Hispanic or Latino",
           "Black or African American Alone, Not Hispanic or Latino",
           "Hispanic or Latino",
           "Asian/NH/PI",
           "American Indian and Alaska Native Alone, Not Hispanic or Latino"),
  HR_race = c(1.000, 3.795, 2.115, 1.574, 2.319)
)

# Join multipliers
demographics_long <- demographics_long %>%
  left_join(hr_sex, by = "sex") %>%
  left_join(hr_age, by = "age_group") %>%
  left_join(hr_race, by = "race")

cat("✓ Hazard ratios attached\n")
cat("  Overall HR = HR_sex × HR_age × HR_race\n\n")

# ── 5. Download CDC PLACES Tract-Level Prevalence ────────────────────────────
cat("Downloading CDC PLACES tract-level prevalence for DM and HTN...\n")

# CDC PLACES API endpoint (2025 release)
places_url <- "https://data.cdc.gov/resource/cwsq-ngmh.json"

# Query for each state - filter for DIABETES and BPHIGH measures only
cdc_data <- list()

for (state in STATES) {
  cat("  State:", state, "...\n")

  # Paginated requests (1000 records per page)
  offset <- 0
  state_data <- list()

  repeat {
    response <- GET(
      url = places_url,
      query = list(
        `$where` = paste0("stateabbr='", state, "' AND (measureid='DIABETES' OR measureid='BPHIGH')"),
        `$select` = "locationname,measureid,data_value",
        `$limit` = 1000,
        `$offset` = offset
      )
    )

    if (status_code(response) != 200) {
      stop("CDC PLACES API request failed for ", state, ": ", status_code(response))
    }

    page_data <- fromJSON(content(response, as = "text", encoding = "UTF-8"))

    if (nrow(page_data) == 0) break

    state_data[[length(state_data) + 1]] <- page_data
    offset <- offset + 1000

    if (nrow(page_data) < 1000) break
  }

  cdc_data[[state]] <- bind_rows(state_data)
  cat("    Retrieved", nrow(cdc_data[[state]]), "records\n")
}

cdc_all <- bind_rows(cdc_data)

# Reshape wide: one row per tract with DM and HTN columns
cdc_tracts <- cdc_all %>%
  mutate(
    tract_geoid = locationname,  # locationname is the 11-digit tract GEOID
    data_value = as.numeric(data_value)
  ) %>%
  select(tract_geoid, measureid, data_value) %>%
  pivot_wider(
    names_from = measureid,
    values_from = data_value
  ) %>%
  rename(
    DM_crude_prev = DIABETES,
    HTN_crude_prev = BPHIGH
  )

cat("✓ CDC PLACES data downloaded\n")
cat("  Tracts with prevalence:", nrow(cdc_tracts), "\n\n")

# Save CDC data
write_csv(cdc_tracts, OUTPUT_CDC)
cat("✓ Saved to:", OUTPUT_CDC, "\n\n")

# ── 6. Assign Prevalence to Blocks ───────────────────────────────────────────
cat("Joining tract-level prevalence to blocks...\n")

demographics_full <- demographics_long %>%
  left_join(block_geoids, by = "GEOID") %>%
  left_join(cdc_tracts, by = "tract_geoid")

# Check for missing prevalence
missing_prev <- demographics_full %>%
  filter(is.na(DM_crude_prev) | is.na(HTN_crude_prev)) %>%
  distinct(tract_geoid)

if (nrow(missing_prev) > 0) {
  cat("  WARNING:", nrow(missing_prev), "tracts missing CDC PLACES data\n")
}

cat("✓ Prevalence joined to blocks\n\n")

# ── 7. Save Output ────────────────────────────────────────────────────────────
cat("Saving block demographics file...\n")

# Rename prevalence columns to match spec
demographics_output <- demographics_full %>%
  rename(DM_k = DM_crude_prev, HTN_k = HTN_crude_prev) %>%
  arrange(GEOID, race, sex, age_group)

write_csv(demographics_output, OUTPUT_DEMOGRAPHICS)

cat("✓ Saved to:", OUTPUT_DEMOGRAPHICS, "\n")
cat("  Rows:", nrow(demographics_output), "\n")
cat("  Columns:", paste(names(demographics_output), collapse = ", "), "\n\n")

# ── 8. Produce Verification Table ────────────────────────────────────────────
cat("Creating hazard ratio verification table...\n")

# Create table showing all multipliers
hr_verification <- bind_rows(
  hr_sex %>% mutate(category = "Sex", group = sex, multiplier = HR_sex) %>% select(category, group, multiplier),
  hr_age %>% mutate(category = "Age", group = age_group, multiplier = HR_age) %>% select(category, group, multiplier),
  hr_race %>% mutate(category = "Race", group = race, multiplier = HR_race) %>% select(category, group, multiplier)
)

write_csv(hr_verification, OUTPUT_HR_TABLE)

cat("✓ Saved to:", OUTPUT_HR_TABLE, "\n\n")

# Print verification table
print(hr_verification)

# ── 9. Summary ────────────────────────────────────────────────────────────────
cat("\n── FINAL SUMMARY ──────────────────────────────────────────────────────\n")
cat("Block demographics and prevalence attached\n\n")
cat("Input:\n")
cat("  Blocks:", nrow(block_geoids), "\n")
cat("  Census tables: P12H-P12M (6 race/ethnicity categories)\n")
cat("  CDC PLACES tracts:", nrow(cdc_tracts), "\n\n")
cat("Output:\n")
cat("  Demographic strata per block:",
    n_distinct(demographics_output$race), "races ×",
    n_distinct(demographics_output$sex), "sexes ×",
    n_distinct(demographics_output$age_group), "age groups =",
    n_distinct(demographics_output$race) *
    n_distinct(demographics_output$sex) *
    n_distinct(demographics_output$age_group), "combinations\n")
cat("  Total population:", sum(demographics_output$population, na.rm = TRUE), "\n")
cat("  Baseline hazard ratio: White adult female = 1.0\n")
cat("  Prevalence sources: DM and HTN from CDC PLACES 2024\n\n")
cat("Files saved:\n")
cat(" ", OUTPUT_DEMOGRAPHICS, "\n")
cat(" ", OUTPUT_CDC, "\n")
cat(" ", OUTPUT_HR_TABLE, "\n")
cat("───────────────────────────────────────────────────────────────────────\n")
