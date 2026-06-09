source("R/01_load_inputs.R")
source("R/02_build_case_arrivals.R")
source("R/03_assign_case_attributes.R")
source("R/04_run_case_flow.R")
source("R/05_apply_budget_logic.R")
source("R/06_summarize_outputs.R")
source("R/07_diagnostics.R")
source("R/08_monte_carlo.R")

# 1. Load the base inputs
inputs <- load_model_inputs(data_dir = "data")

# 2. Override the primary scenario parameters with the dedicated Contractor Queue CSV
cq_file <- "data/scenario_parameters_contractor_queue.csv"

if (!file.exists(cq_file)) {
  stop("Cannot find the contractor queue CSV at: ", cq_file, "\nPlease verify the data path.")
}

cq_scenarios <- readr::read_csv(cq_file, show_col_types = FALSE)
inputs$scenario_parameters <- cq_scenarios

# Create output directory if it doesn't exist
if (!dir.exists("outputs/tables")) dir.create("outputs/tables", recursive = TRUE)

cat("=== Running Contractor Queue Sensitivity (CQ01-CQ06) ===\n")
cat("Scenarios loaded: ", nrow(cq_scenarios), "\n")
cat("Configuration: n_iter = 3000, parallel = TRUE, workers = max -2")

# 3. Run the Monte Carlo sweeps for the CQ scenarios
cq_mc_results <- run_monte_carlo_all_scenarios(
  inputs = inputs,
  n_iter = 3000,
  keep_case_results = FALSE,
  seed = 42,
  parallel = TRUE,
  workers = max(1, parallelly::availableCores() - 1)
)

# 4. Save the dedicated CQ outputs
readr::write_csv(
  cq_mc_results$scenario_stats,
  "outputs/tables/cq_scenario_stats.csv"
)

cat("\n--- Contractor Queue Scenario Summary ---\n")
print(cq_mc_results$scenario_stats |> 
        dplyr::select(scenario_id, scenario_name, mean_mean_days_to_resolution, mean_peak_concurrent_cases, mean_pct_resolved_within_180_days))
cat("\n[SUCCESS] Saved to outputs/tables/cq_scenario_stats.csv\n")

if (!dir.exists("outputs/rds")) dir.create("outputs/rds", recursive = TRUE)

readr::write_csv(
  cq_mc_results$scenario_iteration_results,
  "outputs/tables/cq_iteration_results.csv"
)
# Save the entire contractor queue raw output
saveRDS(cq_mc_results, "outputs/rds/cq_mc_results_full.rds")
cat("\nSaved raw object to outputs/rds/cq_mc_results_full.rds\n")