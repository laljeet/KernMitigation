build_scenario_timing_check <- function(cases_all) {
  suppressPackageStartupMessages({
    library(dplyr)
  })
  
  cases_all |>
    dplyr::group_by(.data$scenario_id, .data$scenario_name) |>
    dplyr::summarise(
      first_arrival = min(.data$arrival_date, na.rm = TRUE),
      last_arrival = max(.data$arrival_date, na.rm = TRUE),
      first_resolution = min(.data$resolution_date, na.rm = TRUE),
      last_resolution = max(.data$resolution_date, na.rm = TRUE),
      n_cases = dplyr::n(),
      n_resolved = sum(.data$resolved_within_model_year, na.rm = TRUE),
      n_unresolved = sum(!.data$resolved_within_model_year, na.rm = TRUE),
      .groups = "drop"
    )
}

build_resolution_year_check <- function(cases_all) {
  suppressPackageStartupMessages({
    library(dplyr)
  })
  
  cases_all |>
    dplyr::group_by(.data$scenario_id, .data$scenario_name, .data$resolution_year) |>
    dplyr::summarise(
      n_cases = dplyr::n(),
      .groups = "drop"
    ) |>
    dplyr::arrange(.data$scenario_id, .data$resolution_year)
}


build_resolution_performance_check <- function(cases_all) {
  suppressPackageStartupMessages({
    library(dplyr)
  })
  
  cases_all |>
    dplyr::group_by(.data$scenario_id, .data$scenario_name) |>
    dplyr::summarise(
      mean_days_to_resolution = mean(.data$days_to_resolution, na.rm = TRUE),
      median_days_to_resolution = median(.data$days_to_resolution, na.rm = TRUE),
      min_days_to_resolution = min(.data$days_to_resolution, na.rm = TRUE),
      max_days_to_resolution = max(.data$days_to_resolution, na.rm = TRUE),
      pct_resolved_within_90_days = mean(.data$resolved_within_90_days, na.rm = TRUE),
      pct_resolved_within_180_days = mean(.data$resolved_within_180_days, na.rm = TRUE),
      pct_resolved_within_365_days = mean(.data$resolved_within_365_days, na.rm = TRUE),
      .groups = "drop"
    )
}