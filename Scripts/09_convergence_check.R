run_convergence_check <- function(inputs,
                                  scenario_id,
                                  run_sizes = c(250, 500, 1000, 2000, 5000),
                                  seed = 123,
                                  parallel = TRUE,
                                  workers = 4) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(purrr)
    library(tibble)
  })
  
  scenario_row <- inputs$scenario_parameters |>
    dplyr::filter(.data$scenario_id == !!scenario_id)
  
  if (nrow(scenario_row) != 1) {
    stop("scenario_id not found or not unique: ", scenario_id)
  }
  
  results <- purrr::map(
    run_sizes,
    function(n_iter) {
      mc_out <- run_monte_carlo_for_scenario(
        scenario_row = scenario_row,
        inputs = inputs,
        n_iter = n_iter,
        keep_case_results = FALSE,
        seed = seed,
        parallel = parallel,
        workers = workers
      )
      
      mc_out$summary_stats |>
        dplyr::mutate(run_size = n_iter)
    }
  )
  
  dplyr::bind_rows(results) |>
    dplyr::select(
      .data$scenario_id,
      .data$scenario_name,
      .data$run_size,
      .data$mean_mean_days_to_resolution,
      .data$median_mean_days_to_resolution,
      .data$mean_n_unresolved_by_year_end,
      .data$prob_backlog,
      .data$mean_pct_resolved_within_365_days
    ) |>
    dplyr::arrange(.data$run_size)
}