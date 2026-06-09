# R/11_sobol_sensitivity.R
# =============================================================
# Global Sensitivity Analysis using Sobol indices (PARALLEL)
# =============================================================
#
# Purpose: Formally quantify which input parameters drive the
# variance in total resolution time. If the GSA shows that
# construction duration and board cadence dominate, then the
# uncalibrated PERT ranges for admin steps mathematically
# don't matter to the conclusion.
#
# Requires: install.packages(c("sensitivity", "future", "furrr",
#                              "parallelly"))
# =============================================================

run_sobol_analysis <- function(inputs,
                               base_scenario_id = "SW06",
                               n_sobol = 1024,
                               n_model_iter = 100,
                               seed = 42,
                               n_workers = NULL) {
  
  suppressPackageStartupMessages({
    library(sensitivity)
    library(dplyr)
    library(purrr)
    library(furrr)
    library(future)
    library(parallelly)
  })
  
  set.seed(seed)
  
  # get the base scenario row as a template
  base_row <- inputs$scenario_parameters %>%
    dplyr::filter(scenario_id == base_scenario_id)
  
  if (nrow(base_row) != 1) {
    stop("Base scenario '", base_scenario_id, "' not found or not unique.")
  }
  
  base_row <- as.data.frame(base_row)
  
  # -------------------------------------------------------
  # Define the parameter space
  # -------------------------------------------------------
  # Each parameter gets a plausible range to explore.
  # The Sobol analysis will tell us which matter.
  
  param_defs <- data.frame(
    name = c(
      # admin PERT modes (the center of each distribution)
      "site_visit_mode",
      "technical_eval_mode",
      "kmec_mode",
      "legal_mode",
      # impl lognormal medians
      "contractor_mob_median",
      "new_well_median",
      # structural parameters
      "board_cadence_days",
      "k_admin",
      "k_impl",
      "kmec_cases_per_meeting",
      # copula correlation
      "copula_rho_cross"
    ),
    min = c(
      3, 7, 7, 3,       # admin modes: low end (tech eval widened high end only)
      30, 30,            # impl medians: low end (mob, new well) - span below new baselines 75/60
      14,                # board cadence: biweekly
      0.00, 0.00,        # k values: no load effect
      3,                 # KMEC: 3 cases per meeting
      0.0                # copula cross: independent
    ),
    max = c(
      14, 35, 28, 14,    # admin modes: high end (tech eval 28 -> 35)
      150, 120,          # impl medians: high end (mob 60 -> 150, new well 60 -> 120) - baselines 75/60 now interior, spans skeptic values
      60,                # board cadence: bimonthly
      0.06, 0.10,        # k values: strong load effect
      10,                # KMEC: 10 cases per meeting
      0.6                # copula cross: strong correlation
    ),
    stringsAsFactors = FALSE
  )
  
  k_params <- nrow(param_defs)
  
  # -------------------------------------------------------
  # Set up parallel plan
  # -------------------------------------------------------
  # Leave one core free for the OS by default. User can override
  # via n_workers argument.
  
  if (is.null(n_workers)) {
    n_workers <- max(1, parallelly::availableCores() - 1)
  }
  
  cat("Parallel plan: multisession with", n_workers, "workers\n")
  future::plan(future::multisession, workers = n_workers)
  
  # ensure plan is cleaned up on exit
  on.exit(future::plan(future::sequential), add = TRUE)
  
  # -------------------------------------------------------
  # Model wrapper function (parallel)
  # -------------------------------------------------------
  # Takes a matrix X where each row is a parameter set,
  # returns a vector of mean resolution times.
  # Each row evaluated independently on a worker.
  
  model_function <- function(X) {
    n_sets <- nrow(X)
    
    # progressr handler for parallel progress
    progressr::with_progress({
      p <- progressr::progressor(steps = n_sets)
      
      results <- furrr::future_map_dbl(
        seq_len(n_sets),
        function(s) {
          # build scenario row from base + sampled parameters
          scenario_row <- base_row
          
          for (p_idx in seq_len(k_params)) {
            pname <- param_defs$name[p_idx]
            scenario_row[[pname]] <- X[s, p_idx]
          }
          
          # also adjust min/max around the sampled mode for PERT params
          # keep a fixed spread: min = mode * 0.4, max = mode * 2.5-3.0
          # (ensures min < mode < max always holds)
          scenario_row$site_visit_min     <- round(X[s, 1] * 0.4)
          scenario_row$site_visit_max     <- round(X[s, 1] * 3.0)
          scenario_row$technical_eval_min <- round(X[s, 2] * 0.5)
          scenario_row$technical_eval_max <- round(X[s, 2] * 2.5)
          scenario_row$kmec_min           <- round(X[s, 3] * 0.5)
          scenario_row$kmec_max           <- round(X[s, 3] * 3.0)
          scenario_row$legal_min          <- round(X[s, 4] * 0.4)
          scenario_row$legal_max          <- round(X[s, 4] * 3.0)
          
          # keep copula rho_admin and rho_impl at moderate fixed values
          # only vary cross-correlation (the most uncertain)
          scenario_row$copula_rho_admin <- 0.5
          scenario_row$copula_rho_impl  <- 0.6
          scenario_row$copula_rho_cross <- X[s, 11]
          
          # run a small MC (n_model_iter iterations) and take mean of means
          iter_results <- numeric(n_model_iter)
          
          for (i in seq_len(n_model_iter)) {
            scenario_iter <- sample_iteration_parameters(scenario_row)
            
            cases <- build_case_arrivals(
              scenario_row  = scenario_iter,
              program_rules = inputs$program_rules
            )
            
            cases <- assign_case_attributes(
              cases              = cases,
              scenario_row       = scenario_iter,
              mitigation_options = inputs$mitigation_options
            )
            
            flow_out <- run_case_flow(
              cases               = cases,
              scenario_row        = scenario_iter,
              workflow_steps      = inputs$workflow_steps,
              timing_assumptions  = inputs$timing_assumptions,
              program_rules       = inputs$program_rules
            )
            
            # UNPACK THE LIST:
            cases <- flow_out$cases
            
            iter_results[i] <- mean(cases$days_to_resolution, na.rm = TRUE)
          }
          
          p()  # tick progress
          mean(iter_results)
        },
        .options = furrr::furrr_options(seed = TRUE)
      )
    })
    
    results
  }
  
  # -------------------------------------------------------
  # Generate Sobol sample matrices
  # -------------------------------------------------------
  
  # two independent quasi-random samples in [0,1]
  X1_raw <- data.frame(matrix(runif(n_sobol * k_params), ncol = k_params))
  X2_raw <- data.frame(matrix(runif(n_sobol * k_params), ncol = k_params))
  
  names(X1_raw) <- param_defs$name
  names(X2_raw) <- param_defs$name
  
  # scale to parameter ranges
  scale_to_range <- function(X_raw, param_defs) {
    X_scaled <- X_raw
    for (p in seq_len(nrow(param_defs))) {
      X_scaled[[p]] <- param_defs$min[p] + X_raw[[p]] * (param_defs$max[p] - param_defs$min[p])
    }
    X_scaled
  }
  
  X1 <- scale_to_range(X1_raw, param_defs)
  X2 <- scale_to_range(X2_raw, param_defs)
  
  # -------------------------------------------------------
  # Run Sobol analysis
  # -------------------------------------------------------
  
  cat("Starting Sobol analysis with n =", n_sobol,
      "and", n_model_iter, "model iterations per evaluation\n")
  cat("Total model evaluations:", n_sobol * (k_params + 2), "\n")
  cat("Total MC iterations:", n_sobol * (k_params + 2) * n_model_iter, "\n\n")
  
  t_start <- Sys.time()
  
  sob <- sensitivity::soboljansen(
    model = model_function,
    X1    = X1,
    X2    = X2,
    nboot = 100
  )
  
  t_end <- Sys.time()
  cat("\nElapsed time:", format(round(t_end - t_start, 2)), "\n\n")
  
  # -------------------------------------------------------
  # Format results
  # -------------------------------------------------------
  
  s1 <- data.frame(
    parameter  = param_defs$name,
    S1         = sob$S$original,
    S1_ci_low  = sob$S$`min. c.i.`,
    S1_ci_high = sob$S$`max. c.i.`,
    ST         = sob$T$original,
    ST_ci_low  = sob$T$`min. c.i.`,
    ST_ci_high = sob$T$`max. c.i.`
  )
  
  s1 <- s1 %>%
    dplyr::arrange(desc(ST)) %>%
    dplyr::mutate(
      rank = row_number(),
      interpretation = case_when(
        ST > 0.20 ~ "MAJOR driver",
        ST > 0.10 ~ "Moderate driver",
        ST > 0.05 ~ "Minor driver",
        TRUE ~ "Negligible"
      )
    )
  
  list(
    sobol_object  = sob,
    indices       = s1,
    param_defs    = param_defs,
    base_scenario = base_scenario_id,
    n_sobol       = n_sobol,
    n_model_iter  = n_model_iter,
    n_workers     = n_workers,
    elapsed       = t_end - t_start
  )
}