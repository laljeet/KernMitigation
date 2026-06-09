# R/02_build_case_arrivals.R

build_case_arrivals <- function(scenario_row, program_rules) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(tibble)
    library(lubridate)
  })
  
  scenario_row <- as.data.frame(scenario_row)
  
  required_cols <- c(
    "scenario_id",
    "scenario_name",
    "model_year",
    "annual_reported_cases",
    "arrival_pattern",
    "cluster_pattern",
    "gsa_focus"
  )
  
  missing_cols <- setdiff(required_cols, names(scenario_row))
  if (length(missing_cols) > 0) {
    stop(
      "Scenario row is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }
  
  n_cases <- suppressWarnings(as.integer(scenario_row$annual_reported_cases[1]))
  arrival_pattern <- as.character(scenario_row$arrival_pattern[1])
  cluster_pattern <- as.character(scenario_row$cluster_pattern[1])
  gsa_focus <- as.character(scenario_row$gsa_focus[1])
  model_year <- suppressWarnings(as.integer(scenario_row$model_year[1]))
  
  if (is.na(model_year)) {
    stop("model_year is missing or invalid in scenario_parameters.csv")
  }
  
  if (is.na(n_cases)) {
    stop(
      "annual_reported_cases is missing or invalid for scenario_id = ",
      scenario_row$scenario_id[1]
    )
  }
  
  if (n_cases <= 0) {
    return(tibble::tibble())
  }
  
  start_date <- lubridate::ymd(sprintf("%s-01-01", model_year))
  
  if (arrival_pattern == "spread_even") {
    arrival_day <- round(seq(1, 365, length.out = n_cases))
  } else if (arrival_pattern == "summer_cluster") {
    # FIX 4: Genuine uniform random clustering within the 150-250 summer window
    arrival_day <- sort(sample(150:250, n_cases, replace = TRUE))
  } else if (arrival_pattern == "single_cluster") {
    arrival_day <- rep(200, n_cases)
  } else if (arrival_pattern == "single_cluster_jitter") {
    window <- 30
    if ("cluster_window_days" %in% names(scenario_row)) {
      w <- suppressWarnings(as.numeric(scenario_row$cluster_window_days[1]))
      if (!is.na(w) && w > 0) window <- w
    }
    center <- 200
    arrival_day <- sort(round(runif(n_cases, center - window / 2, center + window / 2)))
  } else if (arrival_pattern == "two_cluster") {
    arrival_day <- c(
      rep(120, floor(n_cases / 2)),
      rep(240, ceiling(n_cases / 2))
    )
  } else if (arrival_pattern == "two_cluster_jitter") {
    window <- 30
    if ("cluster_window_days" %in% names(scenario_row)) {
      w <- suppressWarnings(as.numeric(scenario_row$cluster_window_days[1]))
      if (!is.na(w) && w > 0) window <- w
    }
    n_first <- floor(n_cases / 2)
    n_second <- ceiling(n_cases / 2)
    arrival_day <- sort(c(
      round(runif(n_first, 120 - window / 2, 120 + window / 2)),
      round(runif(n_second, 240 - window / 2, 240 + window / 2))
    ))
  } else if (arrival_pattern == "spread_random") {
    arrival_day <- sort(sample(1:365, n_cases, replace = TRUE))
  } else {
    arrival_day <- round(seq(1, 365, length.out = n_cases))
  }
  
  if (cluster_pattern == "single_gsa") {
    gsa_assign <- rep(gsa_focus, n_cases)
  } else if (cluster_pattern == "two_gsa") {
    gsa_assign <- rep(c(gsa_focus, "Other_GSA"), length.out = n_cases)
  } else {
    gsa_assign <- rep("Basinwide_Mixed", n_cases)
  }
  
  tibble::tibble(
    case_id = paste0(scenario_row$scenario_id[1], "_C", seq_len(n_cases)),
    scenario_id = scenario_row$scenario_id[1],
    scenario_name = scenario_row$scenario_name[1],
    model_year = model_year,
    arrival_day = arrival_day,
    arrival_date = start_date + lubridate::days(arrival_day - 1),
    cluster_pattern = cluster_pattern,
    arrival_pattern = arrival_pattern,
    gsa = gsa_assign
  ) |>
    dplyr::arrange(.data$arrival_date, .data$case_id)
}