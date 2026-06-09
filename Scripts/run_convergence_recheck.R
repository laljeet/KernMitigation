# run_convergence_recheck.R
# Re-checks whether 3,000 iterations is still adequate AFTER the median/tail
# widening. The old check validated 3,000 for the MEAN at SW06. The new
# parameters fatten the tails (consolidation P99 ~5 yr), and the paper's
# household-experience claims rest on TAIL metrics, which converge slower
# than means. So this checks:
#   - the high-variance scenarios (S05 clustered, CQ01 1-slot clustered),
#     where convergence is hardest; if 3,000 holds here it holds everywhere
#   - both the mean AND the P90 of per-iteration max interim supply
#     (the Discussion 4.3 household-experience metric)

source("R/01_load_inputs.R")
source("R/02_build_case_arrivals.R")
source("R/03_assign_case_attributes.R")
source("R/04_run_case_flow.R")
source("R/05_apply_budget_logic.R")
source("R/06_summarize_outputs.R")
source("R/07_diagnostics.R")
source("R/08_monte_carlo.R")

suppressPackageStartupMessages({ library(dplyr); library(purrr); library(tibble) })

inputs    <- load_model_inputs("data")
run_sizes <- c(500, 1000, 2000, 3000, 5000, 8000)

# S05 lives in the main scenario file; CQ01 in the contractor-queue file.
sources <- list(
  S05  = inputs$scenario_parameters,
  CQ01 = readr::read_csv("data/scenario_parameters_contractor_queue.csv",
                         show_col_types = FALSE)
)

conv_one <- function(scenario_id, scen_tbl) {
  row <- scen_tbl |> dplyr::filter(.data$scenario_id == !!scenario_id)
  if (nrow(row) != 1) stop("scenario not found/unique: ", scenario_id)

  purrr::map_dfr(run_sizes, function(n_iter) {
    inp <- inputs; inp$scenario_parameters <- row
    mc <- run_monte_carlo_for_scenario(
      scenario_row = row, inputs = inp, n_iter = n_iter,
      keep_case_results = TRUE, seed = 123, parallel = TRUE, workers = max(1, parallelly::availableCores() - 1)
    )
    # per-iteration mean resolution and per-iteration worst-household interim
    per_iter <- mc$cases_all |>
      group_by(iteration_id) |>
      summarise(iter_mean_res = mean(days_to_resolution, na.rm = TRUE),
                iter_max_interim = max(days_on_interim_supply, na.rm = TRUE),
                .groups = "drop")
    tibble(
      scenario_id   = scenario_id,
      run_size      = n_iter,
      mean_res      = mean(per_iter$iter_mean_res),
      p90_maxinterim = quantile(per_iter$iter_max_interim, 0.90, names = FALSE)
    )
  })
}

out <- purrr::imap_dfr(sources, function(tbl, sid) conv_one(sid, tbl))

# report each metric as % deviation from the largest run (8000) = asymptotic proxy
report <- out |>
  group_by(scenario_id) |>
  mutate(
    mean_res_pct_dev = 100 * (mean_res - mean_res[run_size == max(run_size)]) /
                              mean_res[run_size == max(run_size)],
    p90_pct_dev      = 100 * (p90_maxinterim - p90_maxinterim[run_size == max(run_size)]) /
                              p90_maxinterim[run_size == max(run_size)]
  ) |>
  ungroup()

cat("\n=== Convergence re-check (deviation from 8000-iter reference) ===\n")
print(as.data.frame(report |>
  mutate(across(c(mean_res, p90_maxinterim, mean_res_pct_dev, p90_pct_dev),
                ~round(.x, 2)))))

cat("\n--- READ ---\n")
cat("Look at the rows where run_size == 3000.\n")
cat("If BOTH mean_res_pct_dev and p90_pct_dev are within your tolerance (e.g. +/-2%)\n")
cat("for BOTH S05 and CQ01, then 3000 is still adequate and Methods 2.5 stands.\n")
cat("If p90_pct_dev at 3000 exceeds tolerance, the tail needs more iterations:\n")
cat("find the smallest run_size where it settles and raise the run count to that.\n")

readr::write_csv(report, "outputs/tables/v2_convergence_recheck.csv")
