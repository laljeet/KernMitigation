source("R/01_load_inputs.R")
source("R/02_build_case_arrivals.R")
source("R/03_assign_case_attributes.R")
source("R/04_run_case_flow.R")
source("R/05_apply_budget_logic.R")
source("R/06_summarize_outputs.R")
source("R/07_diagnostics.R")
source("R/08_monte_carlo.R")
source("R/10_multiyear_monte_carlo.R")

inputs <- load_model_inputs(data_dir = "data")

if (!dir.exists("outputs/tables")) dir.create("outputs/tables", recursive = TRUE)

# test key caseloads over 5 years
# use the sweep scenarios (spread_even, basinwide, standard distributions)
test_ids <- c("SW02", "SW04", "SW06", "SW08")  # 5, 10, 15, 20 cases

scenario_tbl <- inputs$scenario_parameters

multiyear_results <- purrr::map(
  test_ids,
  function(sid) {
    scenario_row <- scenario_tbl %>%
      dplyr::filter(scenario_id == sid)
    
    if (nrow(scenario_row) != 1) {
      warning("Skipping scenario_id: ", sid, " (not found or not unique)")
      return(NULL)
    }
    
    cat("Running multi-year MC for", sid, "(", scenario_row$scenario_name, ") ...\n")
    
    run_multiyear_monte_carlo(
      scenario_row = scenario_row,
      inputs = inputs,
      n_years = 5,
      pending_threshold = 15,
      n_iter = 3000,         # Updated from 1000 to match convergence recommendations
      seed = 42,
      parallel = TRUE,       # Engages the new furrr parallelization
      workers = max(1, parallelly::availableCores() - 1)            # Standard multi-session core allocation
    )
  }
)

# bind overall stats
overall_stats <- dplyr::bind_rows(
  purrr::compact(purrr::map(multiyear_results, "overall_stats"))
)

# bind year-level stats
year_stats_list <- purrr::compact(purrr::map(multiyear_results, "year_level_stats"))
for (i in seq_along(year_stats_list)) {
  year_stats_list[[i]]$scenario_id <- test_ids[i]
  year_stats_list[[i]]$annual_cases <- overall_stats$annual_cases[i]
}
year_level_stats <- dplyr::bind_rows(year_stats_list)

# save
readr::write_csv(overall_stats, "outputs/tables/multiyear_overall_stats.csv")
readr::write_csv(year_level_stats, "outputs/tables/multiyear_year_level_stats.csv")

cat("\n--- Multi-year overall stats ---\n")
print(overall_stats)

cat("\n--- Year-level pending stats ---\n")
print(year_level_stats)

if (!dir.exists("outputs/rds")) dir.create("outputs/rds", recursive = TRUE)

# Save the raw list of multi-year scenarios
saveRDS(multiyear_results, "outputs/rds/multiyear_results_full.rds")
cat("\nSaved raw object to outputs/rds/multiyear_results_full.rds\n")