# =============================================================================
# 10_intervention_priorities.R
# Purpose: Correlation analysis and intervention priority ranking
# Inputs:  data_processed/chapter_results.geojson
# Outputs: Correlation matrix, priority rankings, intervention map
# =============================================================================

library(tidyverse)
library(sf)
library(corrplot)
library(writexl)
library(ggplot2)
library(ggspatial)

sf_use_s2(FALSE)

cat("═══════════════════════════════════════════════════════════════════════\n")
cat("STEP 10: Correlation Analysis & Intervention Priorities\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

# ── 1. Load Data ──────────────────────────────────────────────────────────────
cat("Loading chapter results...\n")

chapters <- st_read("data_processed/chapter_results.geojson", quiet = TRUE)

cat("  Chapters:", nrow(chapters), "\n")
cat("  Variables: NAME, P_total, SPAR_weighted, SPAI_weighted, population\n\n")

# Calculate demand per capita
chapters <- chapters %>%
  mutate(P_per_capita = P_total / population)

# Filter to chapters with population >= 100
cat("Filtering to chapters with population >= 100...\n")
chapters_excluded <- chapters %>% filter(population < 100)
chapters <- chapters %>% filter(population >= 100)

cat("  Excluded", nrow(chapters_excluded), "chapters with very small population\n")
cat("  Analyzing", nrow(chapters), "chapters\n\n")

# ── 2. Spearman's Correlation Analysis ───────────────────────────────────────
cat("Computing Spearman's rank correlations...\n\n")

# Prepare data for correlation
cor_data <- chapters %>%
  st_drop_geometry() %>%
  select(
    `Absolute Demand` = P_total,
    `Demand per Capita` = P_per_capita,
    `Population` = population,
    `SPAR` = SPAR_weighted,
    `SPAI` = SPAI_weighted
  ) %>%
  filter(complete.cases(.))

# Compute Spearman correlation matrix
cor_matrix <- cor(cor_data, method = "spearman")

# Compute p-values for correlations
cor_pvalues <- cor.mtest(as.matrix(cor_data), method = "spearman")

# Print correlation matrix
cat("Spearman's Rank Correlation Matrix (ρ):\n")
cat("───────────────────────────────────────────────────────────────────────\n")
print(round(cor_matrix, 3))
cat("\n")

# Print key relationships
cat("Key Findings:\n")
cat("───────────────────────────────────────────────────────────────────────\n")
cat(sprintf("  Demand ↔ SPAR:             ρ = %.3f (p = %.4f)\n",
            cor_matrix["Absolute Demand", "SPAR"],
            cor_pvalues$p["Absolute Demand", "SPAR"]))
cat(sprintf("  Population ↔ SPAR:          ρ = %.3f (p = %.4f)\n",
            cor_matrix["Population", "SPAR"],
            cor_pvalues$p["Population", "SPAR"]))
cat(sprintf("  Demand per Capita ↔ SPAR:  ρ = %.3f (p = %.4f)\n",
            cor_matrix["Demand per Capita", "SPAR"],
            cor_pvalues$p["Demand per Capita", "SPAR"]))
cat(sprintf("  SPAI ↔ SPAR:               ρ = %.3f (p = %.4f)\n",
            cor_matrix["SPAI", "SPAR"],
            cor_pvalues$p["SPAI", "SPAR"]))
cat("\n")

# Interpretation
cat("Interpretation:\n")
cat("  • ρ > 0.7: Strong positive correlation\n")
cat("  • ρ 0.3-0.7: Moderate positive correlation\n")
cat("  • ρ -0.3-0.3: Weak/no correlation\n")
cat("  • ρ < -0.3: Negative correlation\n")
cat("  • p < 0.05: Statistically significant\n\n")

# Save correlation plot
png("outputs_maps/10_correlation_matrix.png", width = 8, height = 8, units = "in", res = 300)
corrplot(cor_matrix, method = "color", type = "upper",
         addCoef.col = "black", number.cex = 0.8,
         tl.col = "black", tl.srt = 45,
         col = colorRampPalette(c("#d73027", "#f7f7f7", "#4575b4"))(200),
         title = "Spearman's Rank Correlation Matrix\n(Chapter-Level Variables)",
         mar = c(0,0,2,0))
dev.off()

cat("✓ Correlation plot saved to: outputs_maps/10_correlation_matrix.png\n\n")

# ── 3. Intervention Priority Scoring ──────────────────────────────────────────
cat("Calculating intervention priority scores...\n\n")

# Create priority score components (all scaled 0-100)
chapters_priority <- chapters %>%
  st_drop_geometry() %>%
  mutate(
    # Low SPAR = higher priority (invert and scale)
    access_score = (max(SPAR_weighted, na.rm = TRUE) - SPAR_weighted) /
                   (max(SPAR_weighted, na.rm = TRUE) - min(SPAR_weighted, na.rm = TRUE)) * 100,

    # High demand = higher priority (scale)
    demand_score = (P_total - min(P_total, na.rm = TRUE)) /
                   (max(P_total, na.rm = TRUE) - min(P_total, na.rm = TRUE)) * 100,

    # High demand per capita = higher priority (scale)
    burden_score = (P_per_capita - min(P_per_capita, na.rm = TRUE)) /
                   (max(P_per_capita, na.rm = TRUE) - min(P_per_capita, na.rm = TRUE)) * 100,

    # Composite priority score (weighted average)
    # Access: 50%, Demand: 30%, Burden: 20%
    priority_score = (access_score * 0.5) + (demand_score * 0.3) + (burden_score * 0.2)
  ) %>%
  arrange(desc(priority_score)) %>%
  mutate(
    rank = row_number(),
    priority_tier = case_when(
      priority_score >= quantile(priority_score, 0.75) ~ "Tier 1: Highest Priority",
      priority_score >= quantile(priority_score, 0.50) ~ "Tier 2: High Priority",
      priority_score >= quantile(priority_score, 0.25) ~ "Tier 3: Moderate Priority",
      TRUE ~ "Tier 4: Lower Priority"
    )
  )

# Join back to spatial data
chapters <- chapters %>%
  left_join(chapters_priority %>% select(NAME, priority_score, rank, priority_tier),
            by = "NAME")

# Print top 20 priorities
cat("Top 20 Priority Chapters for Intervention:\n")
cat("───────────────────────────────────────────────────────────────────────\n")
cat(sprintf("%-4s %-30s %6s %8s %8s %8s\n",
            "Rank", "Chapter", "SPAR", "Demand", "Pop", "Score"))
cat("───────────────────────────────────────────────────────────────────────\n")

top20 <- chapters_priority %>%
  slice_head(n = 20) %>%
  select(rank, NAME, SPAR_weighted, P_total, population, priority_score)

for (i in 1:nrow(top20)) {
  cat(sprintf("%-4d %-30s %6.2f %8.0f %8.0f %8.1f\n",
              top20$rank[i],
              substr(top20$NAME[i], 1, 30),
              top20$SPAR_weighted[i],
              top20$P_total[i],
              top20$population[i],
              top20$priority_score[i]))
}
cat("\n")

# Summary by tier
cat("Intervention Priority Tiers:\n")
cat("───────────────────────────────────────────────────────────────────────\n")
tier_summary <- chapters_priority %>%
  group_by(priority_tier) %>%
  summarize(
    n_chapters = n(),
    total_population = sum(population),
    total_demand = sum(P_total),
    mean_spar = mean(SPAR_weighted),
    .groups = "drop"
  ) %>%
  arrange(desc(mean_spar))

print(tier_summary)
cat("\n")

# ── 4. Create Intervention Priority Map ──────────────────────────────────────
cat("Creating intervention priority map...\n")

# Define colors for tiers
tier_colors <- c(
  "Tier 1: Highest Priority" = "#d73027",
  "Tier 2: High Priority" = "#fc8d59",
  "Tier 3: Moderate Priority" = "#fee08b",
  "Tier 4: Lower Priority" = "#d9ef8b"
)

# Create map
p_priorities <- ggplot() +
  geom_sf(data = chapters, aes(fill = priority_tier), color = "white", size = 0.3) +
  scale_fill_manual(values = tier_colors, name = "Priority Tier") +
  annotation_scale(location = "bl", width_hint = 0.2, text_family = "Arial",
                  unit_category = "imperial", pad_x = unit(0.3, "cm"), pad_y = unit(0.3, "cm")) +
  annotation_scale(location = "bl", width_hint = 0.2, text_family = "Arial",
                  unit_category = "metric", pad_x = unit(0.3, "cm"), pad_y = unit(1.0, "cm")) +
  annotation_north_arrow(location = "tr", which_north = "true",
                        style = north_arrow_orienteering(text_family = "Arial", text_size = 10),
                        height = unit(1.0, "cm"), width = unit(1.0, "cm"),
                        pad_x = unit(0.3, "cm"), pad_y = unit(0.3, "cm")) +
  labs(title = "Dialysis Intervention Priority Areas by Chapter",
       subtitle = "Composite score: Access (50%) + Demand (30%) + Burden (20%)") +
  theme_void(base_family = "Arial") +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold", size = 14, margin = margin(b = 5)),
    plot.subtitle = element_text(hjust = 0.5, size = 10, margin = margin(b = 10)),
    legend.position = "right",
    legend.title = element_text(size = 11, face = "bold"),
    legend.text = element_text(size = 10)
  )

ggsave("outputs_maps/10_intervention_priorities.png", p_priorities,
       width = 12, height = 10, dpi = 300, bg = "white")

cat("✓ Priority map saved to: outputs_maps/10_intervention_priorities.png\n\n")

# ── 5. Export Results ─────────────────────────────────────────────────────────
cat("Exporting results to Excel...\n")

# Full chapter rankings
chapter_rankings <- chapters_priority %>%
  select(
    Rank = rank,
    Chapter = NAME,
    `Priority Tier` = priority_tier,
    `Priority Score` = priority_score,
    SPAR = SPAR_weighted,
    SPAI = SPAI_weighted,
    `Absolute Demand` = P_total,
    `Demand per Capita` = P_per_capita,
    Population = population
  )

# Correlation results
cor_results <- as.data.frame(cor_matrix) %>%
  rownames_to_column("Variable")

# Export
write_xlsx(
  list(
    "Priority Rankings" = chapter_rankings,
    "Correlation Matrix" = cor_results,
    "Tier Summary" = tier_summary
  ),
  "outputs_tables/10_intervention_priorities.xlsx"
)

cat("✓ Excel file saved to: outputs_tables/10_intervention_priorities.xlsx\n\n")

# ── 6. Summary ────────────────────────────────────────────────────────────────
cat("═══════════════════════════════════════════════════════════════════════\n")
cat("ANALYSIS COMPLETE\n")
cat("═══════════════════════════════════════════════════════════════════════\n\n")

cat("Files created:\n")
cat("  • outputs_maps/10_correlation_matrix.png\n")
cat("  • outputs_maps/10_intervention_priorities.png\n")
cat("  • outputs_tables/10_intervention_priorities.xlsx\n\n")

cat("Next steps:\n")
cat("  1. Review correlation matrix for key relationships\n")
cat("  2. Examine top-ranked chapters for intervention targeting\n")
cat("  3. Consider geographic clustering for regional interventions\n")
cat("  4. Validate priorities with stakeholder input\n")
cat("═══════════════════════════════════════════════════════════════════════\n")
