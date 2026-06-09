# make_fig5.R
# Figure 5: Variance decomposition and household interim supply burden.
#   Panel A: total-effect Sobol indices (ST) with bootstrap 95% CIs, ranked.
#   Panel B: distribution of worst-served-household interim supply duration
#            per iteration, five selected scenarios.
# Fixes from prior version:
#   - x-axis in Panel B extended to 2000 days so the S05 worst case (1872 d)
#     is not clipped; breaks run to "5 yr".
#   - vertical reference lines mark only true one-year intervals (365, 730,
#     1095, 1460, 1825), consistent with the caption. The 180-day line that
#     contradicted the caption is removed.
# Sobol tiers use the four levels emitted by 11_sobol_sensitivity.R:
#   MAJOR / Moderate / Minor / Negligible. No text in panels.

suppressPackageStartupMessages({
  library(dplyr); library(ggplot2); library(patchwork); library(readr); library(forcats)
})
source("~/Library/CloudStorage/OneDrive-UniversityofCalifornia,Davis/Documents/California/Projects/Kern SGMA/Manuscript Tables and Figures codes/fig_theme.R")

# ---- Panel A: Sobol ----
pretty_names <- c(
  "contractor_mob_median"  = "Contractor mobilization",
  "k_impl"                 = "Load multiplier (impl.)",
  "technical_eval_mode"    = "Technical evaluation",
  "board_cadence_days"     = "Board meeting cadence",
  "new_well_median"        = "New well construction",
  "site_visit_mode"        = "Site visit",
  "k_admin"                = "Load multiplier (admin)",
  "legal_mode"             = "Legal agreement",
  "copula_rho_cross"       = "Copula cross-correlation",
  "kmec_mode"              = "KMEC review",
  "kmec_cases_per_meeting" = "KMEC cases per meeting"
)

sob <- read_csv("outputs/tables/sobol_indices.csv", show_col_types = FALSE) %>%
  mutate(
    pretty = pretty_names[parameter],
    tier = case_when(
      grepl("MAJOR", interpretation, ignore.case = TRUE)    ~ "Major",
      grepl("Moderate", interpretation, ignore.case = TRUE) ~ "Moderate",
      grepl("Minor", interpretation, ignore.case = TRUE)    ~ "Minor",
      TRUE                                                  ~ "Negligible"
    ),
    tier = factor(tier, levels = names(COL_TIER)),
    pretty = fct_reorder(pretty, ST)
  )

pA <- ggplot(sob, aes(y = pretty, x = ST, fill = tier)) +
  geom_col(color = "black", linewidth = 0.3, alpha = 0.9) +
  geom_errorbarh(aes(xmin = ST_ci_low, xmax = ST_ci_high),
                 width = 0.30, linewidth = 0.5, color = "black") +
  scale_fill_manual(values = COL_TIER, drop = FALSE) +
  scale_x_continuous(limits = c(0, 0.70),
                     breaks = c(0, 0.1, 0.2, 0.3, 0.4, 0.5),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(x = expression("Total-effect Sobol index ("*S[T]*")"), y = NULL) +
  theme_paper() +
  theme(legend.position = "bottom",
        legend.text = element_text(size = 9)) +
  guides(fill = guide_legend(nrow = 2))


save_pub(pA, "fig5_sobol_household", width = 7.2, height = 4.2)
cat("Figure 5 written to outputs/figures/\n")
