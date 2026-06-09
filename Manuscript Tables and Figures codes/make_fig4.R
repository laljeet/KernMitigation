# make_fig4.R
# Figure 4: Contractor capacity sensitivity at the 15-well caseload.
#   Panel A: summer-clustered arrivals (CQ01-CQ06)
#   Panel B: spread-even arrivals   (CQ07-CQ12)
# Panel order matches Results section 3.4 (clustered first) and the caption.
# Slot levels are 1, 2, 3, 5, 8, unlimited. Unlimited is stored as NA in the
# data; it is placed at an even categorical position (6) so the axis does not
# imply unlimited is one slot beyond 8. Shared y-axis and shared legend
# (collected across panels) identify the two arrival regimes using the same
# vermillion / blue color encoding as Figures 2 and 5. No text in panels.

suppressPackageStartupMessages({
  library(dplyr); library(ggplot2); library(patchwork); library(readr)
})
source("~/Library/CloudStorage/OneDrive-UniversityofCalifornia,Davis/Documents/California/Projects/Kern SGMA/Manuscript Tables and Figures codes/fig_theme.R")


cq <- read_csv("outputs/tables/cq_scenario_stats.csv", show_col_types = FALSE) %>%
  inner_join(CQ_SLOTS, by = "scenario_id") %>%
  mutate(regime_label = factor(if_else(regime == "cluster", "Cluster", "Spread"),
                               levels = c("Cluster", "Spread")))

cluster_data <- cq %>% filter(regime == "cluster") %>% arrange(slot_pos)
spread_data  <- cq %>% filter(regime == "spread")  %>% arrange(slot_pos)

x_breaks <- 1:6
x_labels <- c("1", "2", "3", "5", "8", "unlim")

regime_colors <- c("Cluster" = COL_CLUSTER, "Spread" = COL_SPREAD)

panel_cq <- function(d) {
  ggplot(d, aes(x = slot_pos)) +
    geom_ribbon(aes(ymin = p10_mean_days_to_resolution,
                    ymax = p90_mean_days_to_resolution,
                    fill = regime_label), alpha = 0.18, show.legend = FALSE) +
    geom_line(aes(y = mean_mean_days_to_resolution,
                  color = regime_label), linewidth = 1.1) +
    geom_point(aes(y = mean_mean_days_to_resolution,
                   color = regime_label), size = 2.8) +
    geom_hline(yintercept = 180, linetype = "dotted",
               color = COL_THRESH, linewidth = 0.5) +
    scale_color_manual(values = regime_colors, drop = FALSE) +
    scale_fill_manual(values = regime_colors, drop = FALSE) +
    scale_x_continuous(breaks = x_breaks, labels = x_labels,
                       limits = c(0.7, 6.3)) +
    scale_y_continuous(limits = c(0, 1800), breaks = seq(0, 1800, 200))
}

pA <- panel_cq(cluster_data) +
  labs(x = "Contractor slots", y = "Mean resolution time (days)") +
  theme_paper()

pB <- panel_cq(spread_data) +
  labs(x = "Contractor slots", y = NULL) +
  theme_paper() +
  theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())

# ---- Panel C: interim supply distributions ----
selected <- tibble::tribble(
  ~scenario_id, ~source, ~label,                ~ord,
  "SW06",       "mc",    "15 spread",           1,
  "S03",        "mc",    "15 cluster",          2,
  "S04",        "mc",    "20 cluster",          3,
  "CQ01",       "cq",    "15 cluster, 1 slot",  4,
  "S05",        "mc",    "30 cluster",          5
)
mc_it <- read_csv("outputs/tables/mc_iteration_results.csv", show_col_types = FALSE)
cq_it <- read_csv("outputs/tables/cq_iteration_results.csv", show_col_types = FALSE)
interim_data <- bind_rows(
  mc_it %>% filter(scenario_id %in% selected$scenario_id[selected$source == "mc"]) %>%
    select(scenario_id, max_days_on_interim_supply),
  cq_it %>% filter(scenario_id %in% selected$scenario_id[selected$source == "cq"]) %>%
    select(scenario_id, max_days_on_interim_supply)
) %>%
  inner_join(selected %>% select(scenario_id, label, ord), by = "scenario_id") %>%
  mutate(label = fct_reorder(label, ord))

col_interim_short <- c(
  "15 spread"            = "#0072B2",
  "15 cluster"           = "#E69F00",
  "20 cluster"           = "#D55E00",
  "15 cluster, 1 slot"   = "#882255",
  "30 cluster"           = "#440154"
)

# year gridlines as log-friendly breaks (1 through 10 years)
year_breaks <- c(365, 730, 1095, 1825, 3650, 7300)
year_labels <- c("1", "2", "3", "5", "10", "20")

pC <- ggplot(interim_data, aes(x = max_days_on_interim_supply, y = label,
                               fill = label)) +
  geom_vline(xintercept = year_breaks, linetype = "dotted",
             color = COL_THRESH, linewidth = 0.4) +
  geom_boxplot(outlier.shape = NA, alpha = 0.8, linewidth = 0.5) +
  scale_fill_manual(values = col_interim_short, guide = "none") +
  scale_x_log10(breaks = year_breaks, labels = year_labels) +
  coord_cartesian(xlim = c(200, 13000)) +
  labs(x = "Max interim supply\n (years, log scale)", y = NULL) +
  theme_paper()

fig4 <- pA + pB +pC+
  plot_layout(ncol = 3, guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "bottom")

save_pub(fig4, "fig4_contractor_queue", width = 7.2, height = 3.4)
cat("Figure 4 written to outputs/figures/\n")
