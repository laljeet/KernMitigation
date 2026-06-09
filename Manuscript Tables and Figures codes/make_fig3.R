# make_fig3.R
# Figure 3: Multi-year coupling under spread arrivals.
#   Panel A: mean resolution time by model year, four caseload levels
#   Panel B: mean pending cases at year end, same four levels
# Reads multiyear_year_level_stats.csv (year-level schema:
#   arrival_year, mean_resolution_days, mean_pending_at_year_end, scenario_id).
# Panel tags added by patchwork. No text in panels.

suppressPackageStartupMessages({
  library(dplyr); library(ggplot2); library(patchwork); library(readr)
})
source("C:/Users/lsangha.UCCEK/OneDrive - University of California, Davis/Documents/California/Projects/Kern SGMA/sections/fig_theme.R", echo = TRUE)

LABELS <- c("SW02" = "5/yr", "SW04" = "10/yr",
            "SW06" = "15/yr", "SW08" = "20/yr")

my <- read_csv("outputs/tables/multiyear_year_level_stats.csv",
               show_col_types = FALSE) %>%
  filter(scenario_id %in% names(LABELS)) %>%
  group_by(scenario_id) %>%
  mutate(year_index = arrival_year - min(arrival_year) + 1) %>%
  ungroup() %>%
  mutate(scenario_label = factor(LABELS[scenario_id], levels = LABELS))

names(COL_SEQ4) <- LABELS

pA <- ggplot(my, aes(x = year_index, y = mean_resolution_days,
                     color = scenario_label, group = scenario_id)) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 2.6) +
  scale_color_manual(values = COL_SEQ4) +
  scale_x_continuous(breaks = 1:5) +
  scale_y_continuous(limits = c(0, NA)) +
  labs(x = "Model year", y = "Mean resolution time (days)") +
  theme_paper()

pB <- ggplot(my, aes(x = year_index, y = mean_pending_at_year_end,
                     color = scenario_label, group = scenario_id)) +
  geom_hline(yintercept = 15, linetype = "dashed",
             color = COL_THRESH, linewidth = 0.5) +
  geom_line(linewidth = 1.0) +
  geom_point(size = 2.6) +
  scale_color_manual(values = COL_SEQ4) +
  scale_x_continuous(breaks = 1:5, limits = c(0.8, 5.2)) +
  scale_y_continuous(limits = c(0, 25), breaks = seq(0, 25, 5)) +
  labs(x = "Model year", y = "Pending cases at year end") +
  theme_paper()

fig3 <- pA + pB +
  plot_layout(ncol = 2, guides = "collect") +
  plot_annotation(tag_levels = "A") &
  theme(legend.position = "bottom")

save_pub(fig3, "fig3_multiyear", width = 7.2, height = 3.4)
cat("Figure 3 written to outputs/figures/\n")
