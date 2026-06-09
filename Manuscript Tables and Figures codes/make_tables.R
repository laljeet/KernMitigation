# make_tables.R
# Produces all manuscript tables as clean CSVs for typesetting. No notes
# embedded in the data; all table notes live in manuscript captions.
#
# MAIN PAPER:
#   Table 1: headline results, spread sweep + clustered families
#   Table 2: Sobol sensitivity indices
#
# SUPPLEMENTARY:
#   Table S1: scenario design (all 25 scenarios)
#   Table S2: multi-year coupling, year-level trajectory
#   Table S3: contractor queue sensitivity
#
# Numeric columns use precomputed stats from the Monte Carlo aggregator
# (06_summarize_outputs.R and 08_monte_carlo.R) so tables and figures
# cite identical values. Interim P90 uses p90_max_days_on_interim_supply
# directly; no recompute.

suppressPackageStartupMessages({
  library(dplyr); library(readr); library(tidyr)
})

tdir_main <- "outputs/tables"
tdir_supp <- "outputs/tables/supplementary"
if (!dir.exists(tdir_main)) dir.create(tdir_main, recursive = TRUE)
if (!dir.exists(tdir_supp)) dir.create(tdir_supp, recursive = TRUE)

# =====================================================================
# Shared inputs
# =====================================================================
mc <- read_csv(file.path(tdir_main, "mc_scenario_stats.csv"), show_col_types = FALSE)
cq <- read_csv(file.path(tdir_main, "cq_scenario_stats.csv"), show_col_types = FALSE)

fmt_headline <- function(d) {
  d %>% transmute(
    Scenario = scenario_id,
    `Mean resolution (d)` = sprintf("%.0f", mean_mean_days_to_resolution),
    `P10 (d)` = sprintf("%.0f", p10_mean_days_to_resolution),
    `P90 (d)` = sprintf("%.0f", p90_mean_days_to_resolution),
    `Within 180 d (%)` = sprintf("%.0f", mean_pct_resolved_within_180_days * 100),
    `P90 max interim (d)` = sprintf("%.0f", p90_max_days_on_interim_supply),
    `Budget exceeded (%)` = sprintf("%.0f", prob_dry_well_budget_exceeded * 100),
    `Mean pending YE` = sprintf("%.1f", mean_n_unresolved_by_year_end)
  )
}

# =====================================================================
# MAIN TABLE 1: Headline results, spread sweep + clustered
# =====================================================================
sw_order  <- sprintf("SW%02d", 1:10)
clu_order <- c("S03", "S04", "S05")

t1_src <- bind_rows(
  mc %>% filter(scenario_id %in% sw_order) %>%
    mutate(Family = "A. Spread sweep", .ord = match(scenario_id, sw_order)),
  mc %>% filter(scenario_id %in% clu_order) %>%
    mutate(Family = "B. Clustered", .ord = 100 + match(scenario_id, clu_order))
) %>% arrange(.ord)

t1 <- t1_src %>%
  fmt_headline() %>%
  mutate(Family = t1_src$Family, .before = 1)
write_csv(t1, file.path(tdir_main, "table1_headline_results.csv"))
cat("Main Table 1 written.\n")

# =====================================================================
# MAIN TABLE 2: Sobol sensitivity indices
# =====================================================================
pretty_sobol <- c(
  "contractor_mob_median"  = "Contractor mobilization (median)",
  "k_impl"                 = "Load multiplier, implementation (k_impl)",
  "technical_eval_mode"    = "Technical evaluation (mode)",
  "board_cadence_days"     = "Board meeting cadence",
  "new_well_median"        = "New well construction (median)",
  "site_visit_mode"        = "Site visit (mode)",
  "k_admin"                = "Load multiplier, admin (k_admin)",
  "legal_mode"             = "Legal agreement (mode)",
  "copula_rho_cross"       = "Copula cross-block correlation",
  "kmec_mode"              = "KMEC review (mode)",
  "kmec_cases_per_meeting" = "KMEC cases per meeting"
)

sob <- read_csv(file.path(tdir_main, "sobol_indices.csv"),
                show_col_types = FALSE) %>%
  arrange(desc(ST))

t2 <- sob %>%
  transmute(
    Rank = rank,
    Parameter = pretty_sobol[parameter],
    S1 = sprintf("%.3f", S1),
    `S1 95% CI` = sprintf("[%.3f, %.3f]", S1_ci_low, S1_ci_high),
    ST = sprintf("%.3f", ST),
    `ST 95% CI` = sprintf("[%.3f, %.3f]", ST_ci_low, ST_ci_high),
    Interpretation = interpretation
  )
write_csv(t2, file.path(tdir_main, "table2_sobol.csv"))
cat("Main Table 2 written.\n")

# =====================================================================
# SUPPLEMENTARY S1: Scenario design (all 25 scenarios)
# =====================================================================
sw_design <- tibble(
  scenario_id = sprintf("SW%02d", 1:10),
  annual_cases = c(3, 5, 8, 10, 12, 15, 18, 20, 25, 30),
  arrival_pattern = "Spread even",
  contractor_slots = "3"
)
clust_design <- tibble(
  scenario_id = c("S03", "S04", "S05"),
  annual_cases = c(15, 20, 30),
  arrival_pattern = "Summer cluster",
  contractor_slots = "3"
)
cq_design <- tibble(
  scenario_id = sprintf("CQ%02d", 1:12),
  annual_cases = 15,
  arrival_pattern = c(rep("Summer cluster", 6), rep("Spread even", 6)),
  contractor_slots = rep(c("1", "2", "3", "5", "8", "unlimited"), 2)
)
multi_year_set <- c("SW02", "SW04", "SW06", "SW08")

s1 <- bind_rows(sw_design, clust_design, cq_design) %>%
  mutate(multi_year = if_else(scenario_id %in% multi_year_set, "Yes (5 yr)", "No"))
names(s1) <- c("Scenario", "Annual cases", "Arrival pattern",
               "Contractor slots", "Multi-year")
write_csv(s1, file.path(tdir_supp, "tableS1_scenario_design.csv"))
cat("Supplementary Table S1 written.\n")

# =====================================================================
# SUPPLEMENTARY S2: Multi-year coupling, year-level trajectory
# =====================================================================
my <- read_csv(file.path(tdir_main, "multiyear_year_level_stats.csv"),
               show_col_types = FALSE)
labels_my <- c("SW02" = "5 cases/yr", "SW04" = "10 cases/yr",
               "SW06" = "15 cases/yr", "SW08" = "20 cases/yr")

s2 <- my %>%
  filter(scenario_id %in% names(labels_my)) %>%
  group_by(scenario_id) %>%
  mutate(`Model year` = arrival_year - min(arrival_year) + 1) %>%
  ungroup() %>%
  transmute(
    Scenario = labels_my[scenario_id],
    `Model year`,
    `Mean resolution (d)` = sprintf("%.0f", mean_resolution_days),
    `Mean pending YE` = sprintf("%.1f", mean_pending_at_year_end),
    `P90 pending YE` = sprintf("%.0f", p90_pending_at_year_end)
  ) %>%
  arrange(factor(Scenario, levels = labels_my), `Model year`)
write_csv(s2, file.path(tdir_supp, "tableS2_multiyear.csv"))
cat("Supplementary Table S2 written.\n")

# =====================================================================
# SUPPLEMENTARY S3: Contractor queue sensitivity
# =====================================================================
cq_slots <- tibble(
  scenario_id = sprintf("CQ%02d", 1:12),
  Arrivals = c(rep("Summer cluster", 6), rep("Spread even", 6)),
  Slots = rep(c("1", "2", "3", "5", "8", "unlimited"), 2),
  .ord = 1:12
)

s3 <- cq %>%
  inner_join(cq_slots, by = "scenario_id") %>%
  arrange(.ord) %>%
  transmute(
    Scenario = scenario_id,
    Arrivals, Slots,
    `Mean resolution (d)` = sprintf("%.0f", mean_mean_days_to_resolution),
    `Within 180 d (%)` = sprintf("%.0f", mean_pct_resolved_within_180_days * 100),
    `P90 max interim (d)` = sprintf("%.0f", p90_max_days_on_interim_supply),
    `Mean pending YE` = sprintf("%.1f", mean_n_unresolved_by_year_end)
  )
write_csv(s3, file.path(tdir_supp, "tableS3_contractor_queue.csv"))
cat("Supplementary Table S3 written.\n")

cat("\nMain tables:", tdir_main, "\n")
cat("Supplementary tables:", tdir_supp, "\n")
