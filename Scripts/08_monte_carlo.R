# R/08_monte_carlo.R

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

get_scalar <- function(x, name, default = NA_real_) {
  if (!name %in% names(x)) return(default)
  val <- x[[name]][1]
  if (is.na(val)) default else val
}

rpert1 <- function(min_val, mode_val, max_val, shape = 4) {
  if (any(is.na(c(min_val, mode_val, max_val)))) {
    stop("PERT draw requires min, mode, and max.")
  }
  if (!(min_val <= mode_val && mode_val <= max_val)) {
    stop("PERT parameters must satisfy min <= mode <= max.")
  }
  if (min_val == max_val) return(min_val)
  
  alpha <- 1 + shape * ((mode_val - min_val) / (max_val - min_val))
  beta  <- 1 + shape * ((max_val - mode_val) / (max_val - min_val))
  
  min_val + stats::rbeta(1, alpha, beta) * (max_val - min_val)
}

rlnorm_from_median <- function(median_val, sdlog_val) {
  if (any(is.na(c(median_val, sdlog_val)))) {
    stop("Lognormal draw requires median and sdlog.")
  }
  if (median_val <= 0) {
    stop("median_val must be > 0 for lognormal draws.")
  }
  stats::rlnorm(1, meanlog = log(median_val), sdlog = sdlog_val)
}

# ------------------------------------------------------------
# OLD system state functions — RETAINED FOR REFERENCE
# These have been replaced by the dynamic load multiplier.
# The load multiplier is applied inside run_case_flow based
# on actual concurrent case count rather than a pre-draw dice
# roll. See k_admin and k_impl parameters.
# ------------------------------------------------------------
# draw_system_state <- function(scenario_row) { ... }
# get_state_multiplier <- function(scenario_row, state, family) { ... }

# ------------------------------------------------------------
# PERT quantile function (for copula path)
# ------------------------------------------------------------

qpert <- function(p, min_val, mode_val, max_val, shape = 4) {
  if (min_val == max_val) return(min_val)
  alpha <- 1 + shape * ((mode_val - min_val) / (max_val - min_val))
  beta  <- 1 + shape * ((max_val - mode_val) / (max_val - min_val))
  min_val + qbeta(p, alpha, beta) * (max_val - min_val)
}

# ------------------------------------------------------------
# Sample base iteration parameters
# 
# Two modes:
#   Independent (default): each step drawn independently
#   Copula: correlated draws via Gaussian copula when
#     copula_rho_admin, copula_rho_impl, copula_rho_cross
#     are present in scenario_row
# ------------------------------------------------------------

sample_iteration_parameters <- function(scenario_row) {
  scenario_row <- as.data.frame(scenario_row)
  
  # check if copula mode is requested
  use_copula <- FALSE
  rho_admin <- get_scalar(scenario_row, "copula_rho_admin", NA_real_)
  rho_impl  <- get_scalar(scenario_row, "copula_rho_impl", NA_real_)
  rho_cross <- get_scalar(scenario_row, "copula_rho_cross", NA_real_)
  
  if (!is.na(rho_admin) && !is.na(rho_impl) && !is.na(rho_cross)) {
    use_copula <- TRUE
  }
  
  # gather PERT parameters for admin steps
  sv_min  <- get_scalar(scenario_row, "site_visit_min", get_scalar(scenario_row, "site_visit_scheduling_time"))
  sv_mode <- get_scalar(scenario_row, "site_visit_mode", get_scalar(scenario_row, "site_visit_scheduling_time"))
  sv_max  <- get_scalar(scenario_row, "site_visit_max", get_scalar(scenario_row, "site_visit_scheduling_time"))
  
  te_min  <- get_scalar(scenario_row, "technical_eval_min", get_scalar(scenario_row, "technical_evaluation_duration"))
  te_mode <- get_scalar(scenario_row, "technical_eval_mode", get_scalar(scenario_row, "technical_evaluation_duration"))
  te_max  <- get_scalar(scenario_row, "technical_eval_max", get_scalar(scenario_row, "technical_evaluation_duration"))
  
  km_min  <- get_scalar(scenario_row, "kmec_min", get_scalar(scenario_row, "kmec_review_cadence_or_lag"))
  km_mode <- get_scalar(scenario_row, "kmec_mode", get_scalar(scenario_row, "kmec_review_cadence_or_lag"))
  km_max  <- get_scalar(scenario_row, "kmec_max", get_scalar(scenario_row, "kmec_review_cadence_or_lag"))
  
  le_min  <- get_scalar(scenario_row, "legal_min", get_scalar(scenario_row, "legal_agreement_completion_time"))
  le_mode <- get_scalar(scenario_row, "legal_mode", get_scalar(scenario_row, "legal_agreement_completion_time"))
  le_max  <- get_scalar(scenario_row, "legal_max", get_scalar(scenario_row, "legal_agreement_completion_time"))
  
  # gather lognormal parameters for implementation steps
  cm_med  <- get_scalar(scenario_row, "contractor_mob_median", get_scalar(scenario_row, "contractor_mobilization_time"))
  cm_sd   <- get_scalar(scenario_row, "contractor_mob_sdlog", 0.35)
  
  nw_med  <- get_scalar(scenario_row, "new_well_median", get_scalar(scenario_row, "construction_days_new_well"))
  nw_sd   <- get_scalar(scenario_row, "new_well_sdlog", 0.30)
  
  dw_med  <- get_scalar(scenario_row, "deepen_median", get_scalar(scenario_row, "construction_days_deepen_well"))
  dw_sd   <- get_scalar(scenario_row, "deepen_sdlog", 0.25)
  
  lp_med  <- get_scalar(scenario_row, "lower_pump_median", get_scalar(scenario_row, "construction_days_lower_pump"))
  lp_sd   <- get_scalar(scenario_row, "lower_pump_sdlog", 0.20)
  
  co_med  <- get_scalar(scenario_row, "consolidation_median", get_scalar(scenario_row, "construction_days_consolidation"))
  co_sd   <- get_scalar(scenario_row, "consolidation_sdlog", 0.40)
  
  tr_med  <- get_scalar(scenario_row, "treatment_median", get_scalar(scenario_row, "construction_days_treatment"))
  tr_sd   <- get_scalar(scenario_row, "treatment_sdlog", 0.20)
  
  if (use_copula) {
    # -------------------------------------------------------
    # Gaussian copula path
    # -------------------------------------------------------
    # 10 variables:
    #   1-4: admin (site_visit, tech_eval, kmec, legal)
    #   5-10: impl (contractor_mob, new_well, deepen, lower_pump, consolidation, treatment)
    #
    # Correlation structure:
    #   within admin:  rho_admin
    #   within impl:   rho_impl
    #   admin x impl:  rho_cross
    
    k <- 10
    sigma <- matrix(rho_cross, nrow = k, ncol = k)
    
    # admin block (1:4)
    for (i in 1:4) {
      for (j in 1:4) {
        if (i == j) sigma[i, j] <- 1
        else sigma[i, j] <- rho_admin
      }
    }
    
    # impl block (5:10)
    for (i in 5:10) {
      for (j in 5:10) {
        if (i == j) sigma[i, j] <- 1
        else sigma[i, j] <- rho_impl
      }
    }
    
    # draw correlated normals, transform to uniform
    z <- MASS::mvrnorm(1, mu = rep(0, k), Sigma = sigma)
    u <- stats::pnorm(z)
    
    # transform uniforms through marginal quantile functions
    scenario_row$site_visit_scheduling_time     <- round(qpert(u[1], sv_min, sv_mode, sv_max))
    scenario_row$technical_evaluation_duration   <- round(qpert(u[2], te_min, te_mode, te_max))
    scenario_row$kmec_review_cadence_or_lag      <- round(qpert(u[3], km_min, km_mode, km_max))
    scenario_row$legal_agreement_completion_time <- round(qpert(u[4], le_min, le_mode, le_max))
    
    scenario_row$contractor_mobilization_time    <- round(stats::qlnorm(u[5], log(cm_med), cm_sd))
    scenario_row$construction_days_new_well      <- round(stats::qlnorm(u[6], log(nw_med), nw_sd))
    scenario_row$construction_days_deepen_well   <- round(stats::qlnorm(u[7], log(dw_med), dw_sd))
    scenario_row$construction_days_lower_pump    <- round(stats::qlnorm(u[8], log(lp_med), lp_sd))
    scenario_row$construction_days_consolidation <- round(stats::qlnorm(u[9], log(co_med), co_sd))
    scenario_row$construction_days_treatment     <- round(stats::qlnorm(u[10], log(tr_med), tr_sd))
    
  } else {
    # -------------------------------------------------------
    # Independent draws path (original behavior)
    # -------------------------------------------------------
    scenario_row$site_visit_scheduling_time     <- round(rpert1(sv_min, sv_mode, sv_max))
    scenario_row$technical_evaluation_duration   <- round(rpert1(te_min, te_mode, te_max))
    scenario_row$kmec_review_cadence_or_lag      <- round(rpert1(km_min, km_mode, km_max))
    scenario_row$legal_agreement_completion_time <- round(rpert1(le_min, le_mode, le_max))
    
    scenario_row$contractor_mobilization_time    <- round(rlnorm_from_median(cm_med, cm_sd))
    scenario_row$construction_days_new_well      <- round(rlnorm_from_median(nw_med, nw_sd))
    scenario_row$construction_days_deepen_well   <- round(rlnorm_from_median(dw_med, dw_sd))
    scenario_row$construction_days_lower_pump    <- round(rlnorm_from_median(lp_med, lp_sd))
    scenario_row$construction_days_consolidation <- round(rlnorm_from_median(co_med, co_sd))
    scenario_row$construction_days_treatment     <- round(rlnorm_from_median(tr_med, tr_sd))
  }
  
  scenario_row$system_state <- ifelse(use_copula, "copula", "independent")
  scenario_row$admin_multiplier <- NA_real_
  scenario_row$implementation_multiplier <- NA_real_
  
  scenario_row
}

# ------------------------------------------------------------
# One stochastic iteration
# ------------------------------------------------------------

run_one_stochastic_iteration <- function(scenario_row, program_rules, workflow_steps, timing_assumptions, mitigation_options, iteration_id = 1, seed = NULL) {
  if (!is.null(seed)) set.seed(seed)
  scenario_iter <- sample_iteration_parameters(scenario_row)
  
  cases <- build_case_arrivals(scenario_row = scenario_iter, program_rules = program_rules)
  cases <- assign_case_attributes(cases = cases, scenario_row = scenario_iter, mitigation_options = mitigation_options)
  
  # --- FIX APPLIED HERE ---
  flow_out <- run_case_flow(
    cases = cases,
    scenario_row = scenario_iter,
    workflow_steps = workflow_steps,
    timing_assumptions = timing_assumptions,
    program_rules = program_rules
  )
  
  # Unpack the dataframe from the list
  cases <- flow_out$cases 
  
  cases <- apply_budget_logic(cases = cases, scenario_row = scenario_iter, program_rules = program_rules)
  
  summary_tbl <- summarize_scenario(cases = cases, scenario_row = scenario_iter, program_rules = program_rules) |>
    dplyr::mutate(iteration_id = iteration_id, system_state = scenario_iter$system_state[1])
  
  list(summary = summary_tbl, cases = cases |> dplyr::mutate(iteration_id = iteration_id, system_state = scenario_iter$system_state[1]))
}

# ------------------------------------------------------------
# Scenario-level Monte Carlo
# ------------------------------------------------------------

run_monte_carlo_for_scenario <- function(scenario_row,
                                         inputs,
                                         n_iter = 1000,
                                         keep_case_results = FALSE,
                                         seed = 123,
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
        run_one_stochastic_iteration(
          scenario_row = scenario_row,
          program_rules = inputs$program_rules,
          workflow_steps = inputs$workflow_steps,
          timing_assumptions = inputs$timing_assumptions,
          mitigation_options = inputs$mitigation_options,
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
        run_one_stochastic_iteration(
          scenario_row = scenario_row,
          program_rules = inputs$program_rules,
          workflow_steps = inputs$workflow_steps,
          timing_assumptions = inputs$timing_assumptions,
          mitigation_options = inputs$mitigation_options,
          iteration_id = i,
          seed = seed + i
        )
      }
    )
  }
  
  summary_all <- dplyr::bind_rows(purrr::map(iter_results, "summary"))
  
  summary_stats <- summary_all |>
    dplyr::summarise(
      scenario_id = dplyr::first(.data$scenario_id),
      scenario_name = dplyr::first(.data$scenario_name),
      n_iter = dplyr::n(),
      
      mean_mean_days_to_resolution = mean(.data$mean_days_to_resolution, na.rm = TRUE),
      median_mean_days_to_resolution = median(.data$mean_days_to_resolution, na.rm = TRUE),
      p10_mean_days_to_resolution = as.numeric(stats::quantile(.data$mean_days_to_resolution, 0.10, na.rm = TRUE)),
      p90_mean_days_to_resolution = as.numeric(stats::quantile(.data$mean_days_to_resolution, 0.90, na.rm = TRUE)),
      
      mean_median_days_to_resolution = mean(.data$median_days_to_resolution, na.rm = TRUE),
      p10_median_days_to_resolution = as.numeric(stats::quantile(.data$median_days_to_resolution, 0.10, na.rm = TRUE)),
      p90_median_days_to_resolution = as.numeric(stats::quantile(.data$median_days_to_resolution, 0.90, na.rm = TRUE)),
      
      mean_n_unresolved_by_year_end = mean(.data$n_unresolved_by_year_end, na.rm = TRUE),
      p90_n_unresolved_by_year_end = as.numeric(stats::quantile(.data$n_unresolved_by_year_end, 0.90, na.rm = TRUE)),
      
      prob_backlog = mean(.data$backlog_flag, na.rm = TRUE),
      prob_budget_exceeded = mean(.data$budget_exceeded_flag, na.rm = TRUE),
      prob_dry_well_budget_exceeded = mean(.data$dry_well_budget_exceeded_flag, na.rm = TRUE),
      
      mean_peak_concurrent_cases = mean(.data$peak_concurrent_cases, na.rm = TRUE),
      p90_peak_concurrent_cases = as.numeric(stats::quantile(.data$peak_concurrent_cases, 0.90, na.rm = TRUE)),
      
      mean_days_on_interim_supply = mean(.data$mean_days_on_interim_supply, na.rm = TRUE),
      p90_max_days_on_interim_supply = as.numeric(stats::quantile(.data$max_days_on_interim_supply, 0.90, na.rm = TRUE)),
      
      mean_pct_resolved_within_90_days = mean(.data$pct_resolved_within_90_days, na.rm = TRUE),
      mean_pct_resolved_within_180_days = mean(.data$pct_resolved_within_180_days, na.rm = TRUE),
      mean_pct_resolved_within_365_days = mean(.data$pct_resolved_within_365_days, na.rm = TRUE)
    )
  
  out <- list(
    summary_all = summary_all,
    summary_stats = summary_stats
  )
  
  if (keep_case_results) {
    out$cases_all <- dplyr::bind_rows(purrr::map(iter_results, "cases"))
  }
  
  out
}

# ------------------------------------------------------------
# All-scenario Monte Carlo
# ------------------------------------------------------------

run_monte_carlo_all_scenarios <- function(inputs,
                                          n_iter = 1000,
                                          keep_case_results = FALSE,
                                          seed = 123,
                                          parallel = FALSE,
                                          workers = NULL) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(purrr)
  })
  
  scenario_tbl <- inputs$scenario_parameters
  
  scenario_results <- purrr::map(
    seq_len(nrow(scenario_tbl)),
    function(i) {
      run_monte_carlo_for_scenario(
        scenario_row = scenario_tbl[i, ],
        inputs = inputs,
        n_iter = n_iter,
        keep_case_results = keep_case_results,
        seed = seed + i * 10000,
        parallel = parallel,
        workers = workers
      )
    }
  )
  
  list(
    scenario_stats = dplyr::bind_rows(purrr::map(scenario_results, "summary_stats")),
    scenario_iteration_results = dplyr::bind_rows(purrr::map(scenario_results, "summary_all")),
    scenario_case_results = if (keep_case_results) {
      dplyr::bind_rows(purrr::map(scenario_results, "cases_all"))
    } else {
      NULL
    }
  )
}



