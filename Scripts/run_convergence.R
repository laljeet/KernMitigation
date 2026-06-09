source("R/01_load_inputs.R")
source("R/02_build_case_arrivals.R")
source("R/03_assign_case_attributes.R")
source("R/04_run_case_flow.R")
source("R/05_apply_budget_logic.R")
source("R/06_summarize_outputs.R")
source("R/07_diagnostics.R")
source("R/08_monte_carlo.R")
source("R/09_convergence_check.R")

inputs <- load_model_inputs(data_dir = "data")

if (!dir.exists("outputs/tables")) dir.create("outputs/tables", recursive = TRUE)

run_sizes <- c(250, 500, 1000, 2000, 3000, 5000)

cat("=== Convergence check: SW06 (15 spread) ===\n")
conv_sw06 <- run_convergence_check(
  inputs = inputs,
  scenario_id = "SW06",
  run_sizes = run_sizes,
  seed = 42,
  parallel = TRUE,
  workers = 20
)
print(conv_sw06)

cat("\n=== Convergence check: S04 (20 clustered + contractor queue) ===\n")
conv_s04 <- run_convergence_check(
  inputs = inputs,
  scenario_id = "S04",
  run_sizes = run_sizes,
  seed = 42,
  parallel = TRUE,
  workers = 20
)
print(conv_s04)

# save
all_conv <- dplyr::bind_rows(conv_sw06, conv_s04)
readr::write_csv(all_conv, "outputs/tables/v2_convergence_check.csv")

cat("\nSaved to outputs/tables/v2_convergence_check.csv\n")
