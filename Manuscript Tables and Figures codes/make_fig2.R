# make_fig2.R
# Figure 2: Caseload sweep under spread and clustered arrivals.
#   Panel A: mean resolution time vs annual caseload (spread line + clustered points)
#   Panel B: dry-well budget exceedance probability vs annual caseload (spread)
#   Panel C: attainment at 90, 180, 365 days vs annual caseload (spread)
# Panel tags A/B/C are added by patchwork at composition. No text in panels.

suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(ggplot2); library(patchwork); library(readr)
})
source("~/Library/CloudStorage/OneDrive-UniversityofCalifornia,Davis/Documents/California/Projects/Kern SGMA/Manuscript Tables and Figures codes/fig_theme.R")

mc <- read_csv("outputs/tables/mc_scenario_stats.csv", show_col_types = FALSE)

sw <- mc %>% filter(grepl("^SW", scenario_id)) %>%
  inner_join(SW_CASES, by = "scenario_id") %>% arrange(cases)
clust <- mc %>% filter(scenario_id %in% CLUST_CASES$scenario_id) %>%
  inner_join(CLUST_CASES, by = "scenario_id") %>% arrange(cases)

# ---- Panel A ----
pA <- ggplot() +
  geom_ribbon(data = sw,
              aes(x = cases, ymin = p10_mean_days_to_resolution,
                  ymax = p90_mean_days_to_resolution),
              fill = COL_SPREAD, alpha = 0.18) +
  geom_line(data = sw,
            aes(x = cases, y = mean_mean_days_to_resolution,
                color = "Spread"), linewidth = 0.9) +
  geom_point(data = sw,
             aes(x = cases, y = mean_mean_days_to_resolution,
                 color = "Spread"), size = 2.2) +
  geom_errorbar(data = clust,
                aes(x = cases, ymin = p10_mean_days_to_resolution,
                    ymax = p90_mean_days_to_resolution,
                    color = "Cluster"),
                width = 1.1, linewidth = 0.7) +
  geom_point(data = clust,
             aes(x = cases, y = mean_mean_days_to_resolution,
                 color = "Cluster"), shape = 18, size = 3.6) +
  geom_vline(xintercept = 15, linetype = "dashed",
             color = COL_THRESH, linewidth = 0.5) +
  scale_color_manual(values = c("Spread" = COL_SPREAD,
                                "Cluster" = COL_CLUSTER)) +
  scale_x_continuous(limits = c(0, 32), breaks = seq(0, 30, 10)) +
  scale_y_continuous(limits = c(0, 1600), breaks = seq(0, 1600, 200)) +
  labs(x = "Annual cases", y = "Mean resolution time (days)") +
  theme_paper()

# ---- Panel B ----
pB <- ggplot(sw, aes(x = cases, y = prob_dry_well_budget_exceeded * 100)) +
  geom_col(fill = COL_BUDGET, color = "black", linewidth = 0.3, width = 1.8) +
  geom_vline(xintercept = 15, linetype = "dashed",
             color = COL_THRESH, linewidth = 0.5) +
  scale_x_continuous(limits = c(0, 32), breaks = seq(0, 30, 10)) +
  scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 25)) +
  labs(x = "Annual cases (spread)",
       y = "Dry-well budget exceeded (%)") +
  theme_paper() +
  theme(legend.position = "none")

# ---- Panel C ----
attain_long <- sw %>%
  select(cases,
         `90 d`  = mean_pct_resolved_within_90_days,
         `180 d` = mean_pct_resolved_within_180_days,
         `365 d` = mean_pct_resolved_within_365_days) %>%
  pivot_longer(-cases, names_to = "threshold", values_to = "pct") %>%
  mutate(threshold = factor(threshold, levels = c("90 d", "180 d", "365 d")))

pC <- ggplot(attain_long, aes(x = cases, y = pct * 100,
                              color = threshold, shape = threshold)) +
  geom_line(linewidth = 0.9) +
  geom_point(size = 2.4) +
  geom_vline(xintercept = 15, linetype = "dashed",
             color = COL_THRESH, linewidth = 0.5) +
  scale_color_manual(values = c("90 d" = COL_90, "180 d" = COL_180,
                                "365 d" = COL_365)) +
  scale_shape_manual(values = c("90 d" = 16, "180 d" = 15, "365 d" = 17)) +
  scale_x_continuous(limits = c(0, 32), breaks = seq(0, 30, 10)) +
  scale_y_continuous(limits = c(0, 105), breaks = seq(0, 100, 25)) +
  labs(x = "Annual cases (spread)", y = "Cases resolved (%)") +
  theme_paper()

fig2 <- pA + pB + pC +
  plot_layout(ncol = 3) +
  plot_annotation(tag_levels = "A")

save_pub(fig2, "fig2_caseload_sweep", width = 7.2, height = 3.2)
cat("Figure 2 written to outputs/figures/\n")
