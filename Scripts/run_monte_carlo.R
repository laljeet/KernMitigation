source("R/01_load_inputs.R")
source("R/02_build_case_arrivals.R")
source("R/03_assign_case_attributes.R")
source("R/04_run_case_flow.R")
source("R/05_apply_budget_logic.R")
source("R/06_summarize_outputs.R")
source("R/07_diagnostics.R")
source("R/08_monte_carlo.R")

# load all model inputs
inputs <- load_model_inputs(data_dir = "data")

# create output directory
if (!dir.exists("outputs/tables")) dir.create("outputs/tables", recursive = TRUE)

# run Monte Carlo for all scenarios
mc_results <- run_monte_carlo_all_scenarios(
  inputs = inputs,
  n_iter = 3000,
  keep_case_results = FALSE,
  seed = 42,
  parallel = TRUE,
  workers = max(1, parallelly::availableCores() - 1)
)

# save scenario-level summary stats (one row per scenario)
readr::write_csv(
  mc_results$scenario_stats,
  "outputs/tables/mc_scenario_stats.csv"
)

# save iteration-level results (one row per scenario per iteration)
readr::write_csv(
  mc_results$scenario_iteration_results,
  "outputs/tables/mc_iteration_results.csv"
)

cat("\n--- Monte Carlo scenario summary ---\n")
print(mc_results$scenario_stats)

# Save the entire raw output object
if (!dir.exists("outputs/rds")) dir.create("outputs/rds", recursive = TRUE)
saveRDS(mc_results, "outputs/rds/mc_results_full.rds")
cat("\nSaved raw object to outputs/rds/mc_results_full.rds\n")
