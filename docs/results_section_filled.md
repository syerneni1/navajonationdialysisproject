# Results Section - Filled with Data

## 3. Results

### 3.1 Facilities and Supply

316 dialysis facilities met inclusion criteria across Arizona, New Mexico, Utah, and Colorado. Of these, 41 facilities in F_d had 60-minute isochrones intersecting the Navajo Nation boundary, collectively reporting 787 dialysis stations. 

**[INSERT TABLE 1: Facility count, total stations, and mean isochrone coverage by BIA agency area.]**

The spatial distribution of F_d facilities was concentrated in regional centers including Gallup, NM; Flagstaff, AZ; Farmington, NM; and Page, AZ, with comparatively sparse coverage across the reservation interior. 

**[INSERT FIGURE 1: Map of F_d facility locations and 60-minute isochrones overlaid on the Navajo Nation boundary.]**  
*(Available: `outputs_maps/09_isochrones.png`)*

---

### 3.2 Risk-Adjusted Demand

Total risk-adjusted demand across all Navajo Nation blocks was 3,425,147. Demand was heterogeneously distributed, with the highest block-level concentrations in Shiprock Chapter (P_total = 151,737), Tuba City Chapter (P_total = 150,043), Chinle Chapter (P_total = 133,950), St. Michaels Chapter (P_total = 105,340), and Fort Defiance Chapter (P_total = 101,676). 

The Fort Defiance agency accounted for the largest share of total Navajo Nation demand at 26.1%, reflecting its combination of population size and elevated age and disease burden. Tuba City agency accounted for 20.4%, Crownpoint for 20.3%, Shiprock for 17.8%, and Chinle for 15.4% of total demand.

**[INSERT TABLE 2: P_k by chapter and agency.]**  
*(Available: `outputs_tables/09_chapter_summary.csv` and `outputs_tables/09_agency_summary.csv`)*  
*(Also: `outputs_tables/09_chapter_agency_summary.xlsx` with both sheets)*

**[INSERT FIGURE 2: Choropleth map of block-level P_k by Navajo Nation chapter.]**  
*(Can create from `data_processed/block_access_scores.geojson` - P_k variable)*

---

### 3.3 Facility Supply-to-Demand Ratios

Supply-to-demand ratios R_j varied substantially across facilities in F_d. The highest ratios were observed at facilities in Page, AZ and Flagstaff, AZ, where station counts were high relative to the risk-weighted population within the 60-minute catchment. The lowest ratios were observed at facilities in Gallup, NM and Farmington, NM, which serve the densest concentrations of Navajo Nation residents.

**[INSERT FIGURE 3: Bubble map of R_j values, with bubble size proportional to R_j and color indicating BIA agency area.]**  
*(Available: `outputs_maps/09_facilities_bubble.png` - shows R_j × 10^4)*

**[INSERT TABLE 3: R_j summary statistics by agency area.]**  
*(Note: R_j values available in block-level calculations; facility-level aggregation may be needed)*

---

### 3.4 Block-Level Accessibility and SPAR

Block-level SPAI scores ranged from 0.000 to 0.000131 across the Navajo Nation, with a mean of 0.000027 and standard deviation of 0.000028. After normalization to the study area mean, SPAR values ranged from 0.0 to 3.7, with a mean of 0.76.

**[INSERT FIGURE 4: Choropleth map of SPAR by Navajo Nation chapter.]**  
*(Available: `outputs_maps/09_spar_combined.png` - shows all three aggregation levels)*

**[INSERT FIGURE 5: Choropleth map of SPAR by BIA agency area with facility locations overlaid.]**  
*(Available: Agency-level panel from `outputs_maps/09_spar_combined.png`)*

Agency-level SPAR scores, weighted by P_k, are summarized in Table [N]. The Tuba City agency recorded the highest mean SPAR of 1.67, followed by Chinle (1.31), Crownpoint (1.26), Shiprock (1.06), and Fort Defiance (0.94). The Fort Defiance agency recorded the lowest mean SPAR of 0.94, indicating below-average accessibility despite serving the largest share of total demand.

**[INSERT TABLE 4: Chapter- and agency-level P_k, mean SPAI, and mean SPAR.]**  
*(Available: `outputs_tables/09_chapter_agency_summary.xlsx`)*

Of the total risk-weighted Navajo Nation population, 50.8% resided in blocks with above-average access (SPAR > 1.0), 33.6% in blocks with below-average access (0 < SPAR < 1.0), and 15.7% in blocks outside all 60-minute facility catchments (SPAR = 0). Notably, 14.0% of the total Navajo Nation population (23,105 residents across 6,218 census blocks) had zero modeled dialysis access within a 60-minute drive time.

**[INSERT TABLE 5: Distribution of P_k by SPAR category.]**  
*(Available: `outputs_tables/09_access_tier_summary.csv`)*

---

## Summary Statistics Quick Reference

| Metric | Value |
|--------|-------|
| Total facilities (4-state region) | 316 |
| Facilities in F_d (60-min intersect) | 41 |
| Total dialysis stations (F_d) | 787 |
| Total risk-adjusted demand | 3,425,147 |
| Navajo Nation population (>50% in boundary) | 164,644 |
| Census blocks included | 18,476 |
| Navajo chapters | 111 |
| BIA agencies | 5 |
| SPAI range | 0.000 - 0.000131 |
| SPAR range | 0.0 - 3.7 |
| SPAR mean | 0.76 |
| Population with above-average access | 53.9% (88,796) |
| Population with below-average access | 32.0% (52,743) |
| Population with no access | 14.0% (23,105) |

---

## Correlation Analysis (Spearman's ρ, Chapter-Level)

| Relationship | ρ | p-value | Interpretation |
|-------------|---|---------|----------------|
| Demand per Capita ↔ SPAR | -0.48 | <0.001 | **Negative**: Higher disease burden = worse access |
| Population ↔ SPAR | 0.37 | <0.001 | **Positive**: Larger chapters = better access |
| Absolute Demand ↔ SPAR | 0.31 | <0.001 | **Weak positive**: More patients = slightly better access |
| SPAI ↔ SPAR | 1.00 | <0.001 | **Perfect**: Both measure E2SFCA accessibility |

**Key Finding:** The moderate negative correlation (ρ = -0.48) between demand per capita and SPAR indicates spatial health inequity—chapters with higher dialysis disease burden systematically experience worse geographic access to care.

---

## Intervention Priority Rankings

**Top 10 Priority Chapters** (by composite score: Access 50%, Demand 30%, Burden 20%):

1. Greasewood Chapter (SPAR: 0.03, Pop: 1,079)
2. Indian Wells Chapter (SPAR: 0.03, Pop: 935)
3. Piñon Chapter (SPAR: 0.03, Pop: 2,724)
4. Wide Ruins Chapter (SPAR: 0.04, Pop: 828)
5. San Juan Southern Paiute North (SPAR: 0.00, Pop: 27) — **ZERO ACCESS**
6. Forest Lake Chapter (SPAR: 0.11, Pop: 520)
7. Red Valley Chapter (SPAR: 0.27, Pop: 1,157)
8. Sanostee Chapter (SPAR: 0.31, Pop: 1,518)
9. Navajo Mountain Chapter (SPAR: 0.00, Pop: 679) — **ZERO ACCESS**
10. Two Grey Hills Chapter (SPAR: 0.22, Pop: 1,096)

**Five chapters have complete absence of modeled access (SPAR = 0.00):**
- San Juan Southern Paiute North
- Navajo Mountain Chapter
- Black Mesa Chapter
- Alamo Chapter
- LeChee Chapter

**Priority Tier Summary:**

| Tier | Chapters | Population | Mean SPAR |
|------|----------|------------|-----------|
| Tier 1 (Highest Priority) | 28 | 26,031 | 0.20 |
| Tier 2 (High Priority) | 28 | 48,926 | 0.73 |
| Tier 3 (Moderate Priority) | 27 | 33,828 | 1.10 |
| Tier 4 (Lower Priority) | 28 | 55,859 | 1.79 |

---

## Available Outputs for Manuscript Figures/Tables

### Maps
- `outputs_maps/09_isochrones.png` — F_d facility isochrones
- `outputs_maps/09_facilities_bubble.png` — Facilities with R_j bubble sizes
- `outputs_maps/09_demand_combined.png` — Demand per capita (chapter + agency levels)
- `outputs_maps/09_spai_combined.png` — SPAI (block + chapter + agency levels)
- `outputs_maps/09_spar_combined.png` — SPAR (block + chapter + agency levels)
- `outputs_maps/10_correlation_matrix.png` — Spearman correlation heatmap
- `outputs_maps/10_intervention_priorities.png` — Priority tier map

### Tables
- `outputs_tables/09_chapter_summary.csv` — Chapter-level metrics (111 rows)
- `outputs_tables/09_agency_summary.csv` — Agency-level metrics (5 rows)
- `outputs_tables/09_chapter_agency_summary.xlsx` — Combined Excel (2 sheets)
- `outputs_tables/09_access_tier_summary.csv` — Access tier distribution
- `outputs_tables/10_intervention_priorities.xlsx` — Full rankings + correlations

### Raw Data
- `data_processed/block_access_scores.geojson` — Block-level A_k, SPAR_k, P_k (18,476 blocks)
- `data_processed/chapter_results.geojson` — Chapter-level aggregations (111 chapters)
- `data_processed/agency_results.geojson` — Agency-level aggregations (5 agencies)
