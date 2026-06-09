# R/10_multiyear_monte_carlo.R

run_multiyear_iteration <- function(scenario_row,
                                    program_rules,
                                    workflow_steps,
                                    timing_assumptions,
                                    mitigation_options,
                                    n_years = 5,
                                    pending_threshold = 15,
                                    iteration_id = 1,
                                    seed = NULL) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(lubridate)
    library(purrr)
    library(tibble)
  })

  if (!is.null(seed)) set.seed(seed)

  base_year <- as.integer(scenario_row$model_year[1])

  # Sample parameters once for the entire multi-year block.
  # In v2 most timing draws happen per-case inside run_case_flow, but this call
  # propagates system_state ("copula" vs "independent") and any scenario-level
  # scalar overrides into scenario_iter.
  scenario_iter <- sample_iteration_parameters(scenario_row)

  all_cases_pre <- tibble::tibble()

  # FIX 5: Build all arrivals across the entire multi-year window FIRST.
  # Then run the core flow engine ONCE on the combined stream so that
  # year N's still-open cases naturally show up in year N+1's load
  # multiplier and contractor queue calculations.
  for (yr in seq_len(n_years)) {
    current_year <- base_year + yr - 1
    scenario_iter$model_year <- current_year

    new_cases <- build_case_arrivals(
      scenario_row = scenario_iter,
      program_rules = program_rules
    )

    new_cases <- assign_case_attributes(
      cases = new_cases,
      scenario_row = scenario_iter,
      mitigation_options = mitigation_options
    )

    new_cases$origin_year <- current_year
    new_cases$case_id <- paste0("Y", yr, "_", new_cases$case_id)

    all_cases_pre <- dplyr::bind_rows(all_cases_pre, new_cases)
  }

  # Ensure chronological ordering across all years
  all_cases_pre <- all_cases_pre |> dplyr::arrange(arrival_date, case_id)

  flow_out <- run_case_flow(
    cases = all_cases_pre,
    scenario_row = scenario_iter,
    workflow_steps = workflow_steps,
    timing_assumptions = timing_assumptions,
    program_rules = program_rules
  )

  all_cases <- flow_out$cases

  # FIX C: run_case_flow sets resolved_within_model_year using the single
  # scenario_iter$model_year value, which at this point is the LAST year of the
  # multi-year loop. Recompute it per-case against each case's own origin_year
  # so the flag means what the column name claims.
  all_cases$resolved_within_model_year <- all_cases$resolution_year <= all_cases$origin_year

  # --- Multi-year peak concurrent (weekly sampling) ---
  first_day  <- min(all_cases$intake_date, na.rm = TRUE)
  last_day   <- max(all_cases$resolution_date, na.rm = TRUE)
  check_dates <- seq.Date(first_day, last_day, by = 7)

  intake_dates     <- all_cases$intake_date
  resolution_dates <- all_cases$resolution_date

  concurrent_by_week <- vapply(
    check_dates,
    function(d) {
      sum(intake_dates <= d & resolution_dates >= d)
    },
    FUN.VALUE = integer(1)
  )

  peak_concurrent <- max(concurrent_by_week)

  threshold_hit_idx <- which(concurrent_by_week >= pending_threshold)
  if (length(threshold_hit_idx) > 0) {
    first_threshold_date <- check_dates[threshold_hit_idx[1]]
    first_threshold_year <- lubridate::year(first_threshold_date)
    years_to_threshold   <- as.numeric(first_threshold_year - base_year) + 1
  } else {
    first_threshold_date <- NA
    first_threshold_year <- NA
    years_to_threshold   <- NA
  }

  # --- Per-cohort year summary ---
  # n_unresolved_in_cohort_year: cases whose resolution year is later than
  # their own arrival/origin year. NOT the calendar-year-end pending count;
  # for that, see pending_at_year_end below.
  year_summaries <- all_cases %>%
    dplyr::mutate(arrival_year = lubridate::year(intake_date)) %>%
    dplyr::group_by(arrival_year) %>%
    dplyr::summarise(
      n_new_cases = dplyr::n(),
      mean_days_to_resolution = mean(days_to_resolution, na.rm = TRUE),
      n_resolved_same_year = sum(resolution_year <= arrival_year, na.rm = TRUE),
      n_unresolved_in_cohort_year = sum(resolution_year > arrival_year, na.rm = TRUE),
      .groups = "drop"
    )

  # Calendar-year-end pending count (the policy-relevant metric)
  year_end_pending <- vapply(
    seq_len(n_years),
    function(yr) {
      yr_end <- as.Date(paste0(base_year + yr - 1, "-12-31"))
      sum(intake_dates <= yr_end & resolution_dates > yr_end)
    },
    FUN.VALUE = integer(1)
  )

  year_summaries$pending_at_year_end <- year_end_pending
  year_summaries$iteration_id <- iteration_id

  list(
    year_summaries = year_summaries,
    overall = tibble::tibble(
      iteration_id = iteration_id,
      scenario_id = scenario_row$scenario_id[1],
      scenario_name = scenario_row$scenario_name[1],
      n_years = n_years,
      annual_cases = as.integer(scenario_row$annual_reported_cases[1]),
      peak_concurrent_all_years = peak_concurrent,
      threshold_target = pending_threshold,
      years_to_threshold = years_to_threshold,
      threshold_reached = !is.na(years_to_threshold)
    )
  )
}

run_multiyear_monte_carlo <- function(scenario_row,
                                      inputs,
                                      n_years = 5,
                                      pending_threshold = 15,
                                      n_iter = 1000,
                                      seed = 42,
                                      parallel = FALSE,
                                      workers = NULL) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(purrr)
  })

  if (parallel) {
    if (!requireNamespace("future", quietly = TRUE) ||
        !requireNamespace("furrr", quietly = TRUE)) {
      stop("Parallel mode requires the future and furrr packages.")
    }

    if (!is.null(workers)) {
      future::plan(future::multisession, workers = workers)
    } else {
      future::plan(future::multisession)
    }

    on.exit(future::plan(future::sequential), add = TRUE)

    iter_results <- furrr::future_map(
      seq_len(n_iter),
      function(i) {
        run_multiyear_iteration(
          scenario_row = scenario_row,
          program_rules = inputs$program_rules,
          workflow_steps = inputs$workflow_steps,
          timing_assumptions = inputs$timing_assumptions,
          mitigation_options = inputs$mitigation_options,
          n_years = n_years,
          pending_threshold = pending_threshold,
          iteration_id = i,
          seed = seed + i
        )
      },
      .options = furrr::furrr_options(seed = TRUE)
    )
  } else {
    set.seed(seed)
    iter_results <- purrr::map(
      seq_len(n_iter),
      function(i) {
        run_multiyear_iteration(
          scenario_row = scenario_row,
          program_rules = inputs$program_rules,
          workflow_steps = inputs$workflow_steps,
          timing_assumptions = inputs$timing_assumptions,
          mitigation_options = inputs$mitigation_options,
          n_years = n_years,
          pending_threshold = pending_threshold,
          iteration_id = i,
          seed = seed + i
        )
      }
    )
  }

  overall_all        <- dplyr::bind_rows(purrr::map(iter_results, "overall"))
  year_summaries_all <- dplyr::bind_rows(purrr::map(iter_results, "year_summaries"))

  overall_stats <- overall_all %>%
    dplyr::summarise(
      scenario_id   = dplyr::first(scenario_id),
      scenario_name = dplyr::first(scenario_name),
      annual_cases  = dplyr::first(annual_cases),
      n_years       = dplyr::first(n_years),
      n_iter        = dplyr::n(),

      mean_peak_concurrent = mean(peak_concurrent_all_years, na.rm = TRUE),
      p90_peak_concurrent  = as.numeric(stats::quantile(peak_concurrent_all_years, 0.90, na.rm = TRUE)),
      max_peak_concurrent  = max(peak_concurrent_all_years, na.rm = TRUE),

      prob_threshold_reached    = mean(threshold_reached, na.rm = TRUE),
      mean_years_to_threshold   = mean(years_to_threshold, na.rm = TRUE),
      median_years_to_threshold = median(years_to_threshold, na.rm = TRUE)
    )

  year_level_stats <- year_summaries_all %>%
    dplyr::group_by(arrival_year) %>%
    dplyr::summarise(
      mean_pending_at_year_end = mean(pending_at_year_end, na.rm = TRUE),
      p90_pending_at_year_end  = as.numeric(stats::quantile(pending_at_year_end, 0.90, na.rm = TRUE)),
      max_pending_at_year_end  = max(pending_at_year_end, na.rm = TRUE),
      mean_resolution_days     = mean(mean_days_to_resolution, na.rm = TRUE),
      .groups = "drop"
    )

  list(
    overall_stats     = overall_stats,
    year_level_stats  = year_level_stats,
    iteration_results = overall_all,
    year_summaries    = year_summaries_all
  )
}
