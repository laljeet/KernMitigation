source("R/01_load_inputs.R")
source("R/02_build_case_arrivals.R")
source("R/03_assign_case_attributes.R")
source("R/04_run_case_flow.R")
source("R/05_apply_budget_logic.R")
source("R/06_summarize_outputs.R")
source("R/07_diagnostics.R")
source("R/08_monte_carlo.R")

inputs <- load_model_inputs(data_dir = "data")

# 1. Isolate the SW06 base scenario
scenario_row <- inputs$scenario_parameters |>
  dplyr::filter(scenario_id == "SW06")

if (nrow(scenario_row) != 1) {
  stop("Could not find unique scenario SW06 in scenario_parameters.csv")
}

# 2. Force deterministic/unconstrained parameters (The Sterile Environment)
scenario_row$k_admin <- 0
scenario_row$k_impl <- 0
scenario_row$copula_rho_admin <- NA_real_
scenario_row$copula_rho_impl <- NA_real_
scenario_row$copula_rho_cross <- NA_real_
scenario_row$max_concurrent_construction <- NA_real_

cat("=== SANITY CHECK: UNCONSTRAINED BASELINE ===\n")
cat("k_admin = 0, k_impl = 0, no copula, no queue\n\n")

# 3. Run exactly one iteration
set.seed(42)
res <- run_one_stochastic_iteration(
  scenario_row = scenario_row,
  program_rules = inputs$program_rules,
  workflow_steps = inputs$workflow_steps,
  timing_assumptions = inputs$timing_assumptions,
  mitigation_options = inputs$mitigation_options,
  iteration_id = 1,
  seed = 42
)

cases <- res$cases

# 4. Trace Case 1 to verify the math
c1 <- cases[1, ]

cat("--- Case 1 Trace ---\n")
cat("Option Type:       ", c1$option_key, "\n")
cat("Intake Date:       ", as.character(c1$intake_date), "\n")
cat("Site Visit:        ", as.numeric(c1$site_visit_end - c1$site_visit_start), " days\n")
cat("Tech Eval:         ", as.numeric(c1$tech_eval_end - c1$tech_eval_start), " days\n")
cat("KMEC Ready:        ", as.character(c1$kmec_ready_date), "\n")
cat("KMEC Review:       ", as.character(c1$kmec_review_date), " (Wait: ", as.numeric(c1$kmec_review_date - c1$kmec_ready_date), " days)\n")
cat("Board Approval:    ", as.character(c1$board_approval_date), " (Wait: ", as.numeric(c1$board_approval_date - c1$kmec_review_date), " days)\n")
cat("Legal End:         ", as.numeric(c1$legal_end - c1$board_approval_date), " days\n")
cat("Contractor Mob:    ", c1$contractor_mob_days, " days\n")
cat("Construction:      ", c1$construction_days, " days\n")
cat("Resolution Date:   ", as.character(c1$resolution_date), "\n")
cat("Total Days:        ", c1$days_to_resolution, "\n\n")

# 5. The Mathematical Proof
cat("--- Verification ---\n")
expected_days <- as.numeric(c1$site_visit_end - c1$site_visit_start) +
  as.numeric(c1$tech_eval_end - c1$tech_eval_start) +
  as.numeric(c1$kmec_review_date - c1$kmec_ready_date) +
  as.numeric(c1$board_approval_date - c1$kmec_review_date) +
  as.numeric(c1$legal_end - c1$board_approval_date) +
  c1$contractor_mob_days +
  c1$construction_days

cat("Sum of individual step durations and calendar waits: ", expected_days, "\n")
cat("Actual Days to Resolution recorded in model:         ", c1$days_to_resolution, "\n")

if (expected_days == c1$days_to_resolution) {
  cat("\n[SUCCESS] Math perfectly aligns. Load multipliers and queues are successfully bypassed.\n")
  cat("The engine logic is mathematically sound.\n")
} else {
  cat("\n[FAILURE] Mismatch detected. Unidentified constraints are still active.\n")
}