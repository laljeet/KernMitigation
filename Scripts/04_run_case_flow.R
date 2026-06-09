# R/04_run_case_flow.R

next_board_date <- function(date_in, board_anchor_date, cadence_days) {
  board_anchor_date <- as.Date(board_anchor_date)
  date_in           <- as.Date(date_in)

  if (is.na(date_in) || is.na(board_anchor_date) || is.na(cadence_days)) {
    return(as.Date(NA))
  }

  if (date_in <= board_anchor_date) {
    return(board_anchor_date)
  }

  diff_days <- as.numeric(date_in - board_anchor_date)
  n_steps   <- ceiling(diff_days / cadence_days)
  board_anchor_date + n_steps * cadence_days
}

next_meeting_date <- function(date_in, anchor_date, cadence_days) {
  next_board_date(date_in, anchor_date, cadence_days)
}

run_case_flow <- function(cases,
                          scenario_row,
                          workflow_steps,
                          timing_assumptions,
                          program_rules,
                          slot_free_dates_in = NULL) {   
  suppressPackageStartupMessages({
    library(dplyr)
    library(lubridate)
    library(purrr)
    library(MASS)
  })

  if (nrow(cases) == 0) return(list(cases = cases, slot_free_dates = slot_free_dates_in))

  scenario_row <- as.data.frame(scenario_row)
  n <- nrow(cases)

  emergency_hours <- get_rule_value(program_rules, "R05")
  interim_hours   <- get_rule_value(program_rules, "R06")

  board_cadence_days <- suppressWarnings(as.numeric(scenario_row$board_cadence_days[1]))
  board_anchor_date  <- as.Date(scenario_row$board_anchor_date[1])
  model_year         <- suppressWarnings(as.integer(scenario_row$model_year[1]))

  # --- Setup Modifiers & Constraints ---
  k_admin <- if ("k_admin" %in% names(scenario_row) && !is.na(scenario_row$k_admin[1])) as.numeric(scenario_row$k_admin[1]) else 0
  k_impl  <- if ("k_impl" %in% names(scenario_row) && !is.na(scenario_row$k_impl[1])) as.numeric(scenario_row$k_impl[1]) else 0

  kmec_cases_per_meeting <- if ("kmec_cases_per_meeting" %in% names(scenario_row) && !is.na(scenario_row$kmec_cases_per_meeting[1])) as.integer(scenario_row$kmec_cases_per_meeting[1]) else Inf
  kmec_cadence_days      <- if ("kmec_cadence_days" %in% names(scenario_row) && !is.na(scenario_row$kmec_cadence_days[1])) as.numeric(scenario_row$kmec_cadence_days[1]) else board_cadence_days
  kmec_anchor_date       <- if ("kmec_anchor_date" %in% names(scenario_row) && !is.na(scenario_row$kmec_anchor_date[1])) as.Date(scenario_row$kmec_anchor_date[1]) else board_anchor_date
  use_kmec_batch         <- is.finite(kmec_cases_per_meeting)

  max_conc_construction <- if ("max_concurrent_construction" %in% names(scenario_row)) suppressWarnings(as.numeric(scenario_row$max_concurrent_construction[1])) else NA_real_
  use_contractor_queue  <- !is.na(max_conc_construction) && is.finite(max_conc_construction)

  # --- Phase 1: Set Up Intake Dates ---
  cases <- cases |>
    dplyr::arrange(arrival_date, case_id) |>
    mutate(
      intake_date           = as.Date(arrival_date),
      emergency_datetime    = as.POSIXct(intake_date) + lubridate::hours(emergency_hours),
      interim_datetime      = as.POSIXct(intake_date) + lubridate::hours(interim_hours)
    )

  # --- Phase 1.5: Per-Case Base Timing Draws (FIX 2: Copula moved here) ---
  # We draw base durations for every case. They are independent between cases,
  # but correlated within each case if copula is active.
  get_sc <- function(sr, name, default = NA_real_) {
    if (!name %in% names(sr)) return(default)
    val <- sr[[name]][1]
    if (is.na(val)) default else as.numeric(val)
  }

  use_copula <- !is.na(get_sc(scenario_row, "copula_rho_cross"))
  
  sv_min  <- get_sc(scenario_row, "site_visit_min", get_sc(scenario_row, "site_visit_scheduling_time"))
  sv_mode <- get_sc(scenario_row, "site_visit_mode", get_sc(scenario_row, "site_visit_scheduling_time"))
  sv_max  <- get_sc(scenario_row, "site_visit_max", get_sc(scenario_row, "site_visit_scheduling_time"))
  
  te_min  <- get_sc(scenario_row, "technical_eval_min", get_sc(scenario_row, "technical_evaluation_duration"))
  te_mode <- get_sc(scenario_row, "technical_eval_mode", get_sc(scenario_row, "technical_evaluation_duration"))
  te_max  <- get_sc(scenario_row, "technical_eval_max", get_sc(scenario_row, "technical_evaluation_duration"))
  
  le_min  <- get_sc(scenario_row, "legal_min", get_sc(scenario_row, "legal_agreement_completion_time"))
  le_mode <- get_sc(scenario_row, "legal_mode", get_sc(scenario_row, "legal_agreement_completion_time"))
  le_max  <- get_sc(scenario_row, "legal_max", get_sc(scenario_row, "legal_agreement_completion_time"))
  
  cm_med  <- get_sc(scenario_row, "contractor_mob_median", get_sc(scenario_row, "contractor_mobilization_time"))
  cm_sd   <- get_sc(scenario_row, "contractor_mob_sdlog", 0.35)
  nw_med  <- get_sc(scenario_row, "new_well_median", get_sc(scenario_row, "construction_days_new_well"))
  nw_sd   <- get_sc(scenario_row, "new_well_sdlog", 0.30)
  dw_med  <- get_sc(scenario_row, "deepen_median", get_sc(scenario_row, "construction_days_deepen_well"))
  dw_sd   <- get_sc(scenario_row, "deepen_sdlog", 0.25)
  lp_med  <- get_sc(scenario_row, "lower_pump_median", get_sc(scenario_row, "construction_days_lower_pump"))
  lp_sd   <- get_sc(scenario_row, "lower_pump_sdlog", 0.20)
  co_med  <- get_sc(scenario_row, "consolidation_median", get_sc(scenario_row, "construction_days_consolidation"))
  co_sd   <- get_sc(scenario_row, "consolidation_sdlog", 0.40)
  tr_med  <- get_sc(scenario_row, "treatment_median", get_sc(scenario_row, "construction_days_treatment"))
  tr_sd   <- get_sc(scenario_row, "treatment_sdlog", 0.20)

  base_sv_v <- numeric(n); base_te_v <- numeric(n); base_le_v <- numeric(n)
  base_cm_v <- numeric(n); base_nw_v <- numeric(n); base_dw_v <- numeric(n)
  base_lp_v <- numeric(n); base_co_v <- numeric(n); base_tr_v <- numeric(n)

  if (use_copula) {
    rho_admin <- get_sc(scenario_row, "copula_rho_admin")
    rho_impl  <- get_sc(scenario_row, "copula_rho_impl")
    rho_cross <- get_sc(scenario_row, "copula_rho_cross")
    k_vars <- 9 # We skip kmec lag here as it's governed by meeting dates
    sigma <- matrix(rho_cross, nrow = k_vars, ncol = k_vars)
    for (i in 1:3) for (j in 1:3) sigma[i, j] <- if(i==j) 1 else rho_admin
    for (i in 4:9) for (j in 4:9) sigma[i, j] <- if(i==j) 1 else rho_impl

    # Draw matrix of correlated uniforms for all n cases
    z <- MASS::mvrnorm(n, mu = rep(0, k_vars), Sigma = sigma)
    if(n == 1) z <- matrix(z, nrow=1) # Handle 1-case edge case
    u <- stats::pnorm(z)

    for (i in seq_len(n)) {
      base_sv_v[i] <- round(qpert(u[i,1], sv_min, sv_mode, sv_max))
      base_te_v[i] <- round(qpert(u[i,2], te_min, te_mode, te_max))
      base_le_v[i] <- round(qpert(u[i,3], le_min, le_mode, le_max))
      base_cm_v[i] <- round(stats::qlnorm(u[i,4], log(cm_med), cm_sd))
      base_nw_v[i] <- round(stats::qlnorm(u[i,5], log(nw_med), nw_sd))
      base_dw_v[i] <- round(stats::qlnorm(u[i,6], log(dw_med), dw_sd))
      base_lp_v[i] <- round(stats::qlnorm(u[i,7], log(lp_med), lp_sd))
      base_co_v[i] <- round(stats::qlnorm(u[i,8], log(co_med), co_sd))
      base_tr_v[i] <- round(stats::qlnorm(u[i,9], log(tr_med), tr_sd))
    }
  } else {
    for (i in seq_len(n)) {
      base_sv_v[i] <- round(rpert1(sv_min, sv_mode, sv_max))
      base_te_v[i] <- round(rpert1(te_min, te_mode, te_max))
      base_le_v[i] <- round(rpert1(le_min, le_mode, le_max))
      base_cm_v[i] <- round(rlnorm_from_median(cm_med, cm_sd))
      base_nw_v[i] <- round(rlnorm_from_median(nw_med, nw_sd))
      base_dw_v[i] <- round(rlnorm_from_median(dw_med, dw_sd))
      base_lp_v[i] <- round(rlnorm_from_median(lp_med, lp_sd))
      base_co_v[i] <- round(rlnorm_from_median(co_med, co_sd))
      base_tr_v[i] <- round(rlnorm_from_median(tr_med, tr_sd))
    }
  }

  cases$base_contractor_mob <- base_cm_v
  cases$base_construction_days <- dplyr::case_when(
    cases$option_key == "new_well"      ~ base_nw_v,
    cases$option_key == "deepen_well"   ~ base_dw_v,
    cases$option_key == "lower_pump"    ~ base_lp_v,
    cases$option_key == "consolidation" ~ base_co_v,
    cases$option_key == "treatment"     ~ base_tr_v,
    TRUE                                ~ base_nw_v
  )

  # Output vectors
  site_visit_start_v  <- as.Date(rep(NA, n))
  site_visit_end_v    <- as.Date(rep(NA, n))
  tech_eval_start_v   <- as.Date(rep(NA, n))
  tech_eval_end_v     <- as.Date(rep(NA, n))
  kmec_ready_v        <- as.Date(rep(NA, n))
  kmec_review_date_v  <- as.Date(rep(NA, n))
  board_approval_v    <- as.Date(rep(NA, n))
  legal_end_v         <- as.Date(rep(NA, n))
  contractor_ready_v  <- as.Date(rep(NA, n))
  
  applied_admin_mult_v <- numeric(n)
  applied_impl_mult_v  <- numeric(n)
  active_at_intake_v   <- integer(n)
  
  intake_dates <- cases$intake_date
  contractor_ready_tracker <- as.Date(rep(NA, n))

  # --- Phase 2: Admin Pipeline ---
  for (i in seq_len(n)) {
    this_intake <- intake_dates[i]
    active_admin <- sum(
      intake_dates[1:i] <= this_intake &
        (is.na(contractor_ready_tracker[1:i]) | contractor_ready_tracker[1:i] > this_intake)
    )

    admin_mult <- 1 + k_admin * max(0, active_admin - 1)
    applied_admin_mult_v[i] <- admin_mult
    active_at_intake_v[i]   <- active_admin

    sv_days <- round(base_sv_v[i] * admin_mult)
    te_days <- round(base_te_v[i] * admin_mult)

    site_visit_start_v[i] <- this_intake
    site_visit_end_v[i]   <- this_intake + sv_days
    tech_eval_start_v[i]  <- site_visit_end_v[i]
    tech_eval_end_v[i]    <- tech_eval_start_v[i] + te_days
    kmec_ready_v[i]       <- tech_eval_end_v[i]

    if (!use_kmec_batch) {
      kmec_review_date_v[i] <- next_meeting_date(kmec_ready_v[i], kmec_anchor_date, kmec_cadence_days)
    }
  }

  # --- Phase 2b: KMEC Batch Processing (FIX 3: Overflow Stacking Resolved) ---
  if (use_kmec_batch) {
    kmec_order <- order(kmec_ready_v)
    earliest_ready <- min(kmec_ready_v, na.rm = TRUE)
    latest_ready   <- max(kmec_ready_v, na.rm = TRUE)

    meeting_dates <- seq.Date(kmec_anchor_date, latest_ready + kmec_cadence_days * 5, by = kmec_cadence_days)
    meeting_dates <- meeting_dates[meeting_dates >= earliest_ready - kmec_cadence_days]
    slots_used    <- rep(0L, length(meeting_dates))

    for (idx in kmec_order) {
      ready    <- kmec_ready_v[idx]
      assigned <- FALSE

      for (m in seq_along(meeting_dates)) {
        if (meeting_dates[m] >= ready && slots_used[m] < kmec_cases_per_meeting) {
          kmec_review_date_v[idx] <- meeting_dates[m]
          slots_used[m] <- slots_used[m] + 1L
          assigned <- TRUE
          break
        }
      }

      while (!assigned) {
        new_meeting   <- meeting_dates[length(meeting_dates)] + kmec_cadence_days
        meeting_dates <- c(meeting_dates, new_meeting)
        slots_used    <- c(slots_used, 0L)
        m             <- length(meeting_dates)
        
        if (new_meeting >= ready && slots_used[m] < kmec_cases_per_meeting) {
          kmec_review_date_v[idx] <- new_meeting
          slots_used[m] <- slots_used[m] + 1L
          assigned <- TRUE
        }
      }
    }
  }

  # --- Phase 2c: Board & Legal ---
  for (i in seq_len(n)) {
    le_days <- round(base_le_v[i] * applied_admin_mult_v[i])
    board_approval_v[i]   <- next_board_date(kmec_review_date_v[i], board_anchor_date, board_cadence_days)
    legal_end_v[i]        <- board_approval_v[i] + le_days
    contractor_ready_v[i] <- legal_end_v[i]
    contractor_ready_tracker[i] <- contractor_ready_v[i]
  }

  cases$site_visit_start       <- site_visit_start_v
  cases$site_visit_end         <- site_visit_end_v
  cases$tech_eval_start        <- tech_eval_start_v
  cases$tech_eval_end          <- tech_eval_end_v
  cases$kmec_ready_date        <- kmec_ready_v
  cases$kmec_review_date       <- kmec_review_date_v
  cases$board_approval_date    <- board_approval_v
  cases$legal_end              <- legal_end_v
  cases$contractor_ready_date  <- contractor_ready_v
  cases$applied_admin_multiplier <- applied_admin_mult_v
  cases$active_cases_at_intake   <- active_at_intake_v

  # --- Phase 3: Construction Scheduling (FIX 1: Dynamic Impl Mult) ---
  cases <- cases |> dplyr::arrange(contractor_ready_date, case_id)
  nc <- nrow(cases)

  if (use_contractor_queue) {
    max_slots <- as.integer(max_conc_construction)
    if (!is.null(slot_free_dates_in) && length(slot_free_dates_in) == max_slots) {
      slot_free_dates <- slot_free_dates_in
    } else {
      slot_free_dates <- rep(as.Date("1900-01-01"), max_slots)
    }
  } else {
    slot_free_dates <- as.Date("1900-01-01") 
  }

  constr_start_vec          <- as.Date(rep(NA, nc))
  queue_wait_vec            <- integer(nc)
  contractor_mob_days_vec   <- integer(nc)
  construction_days_vec     <- integer(nc)

  for (i in seq_len(nc)) {
    ready_date <- cases$contractor_ready_date[i]

    if (use_contractor_queue) {
      earliest_slot_free <- min(slot_free_dates)
      actual_start       <- max(ready_date, earliest_slot_free)
    } else {
      actual_start       <- ready_date
    }

    # Estimate resolution for all cases to assess total pipeline load
    res_est <- numeric(nc)
    for(j in seq_len(nc)) {
       if (j < i) {
           res_est[j] <- as.numeric(constr_start_vec[j] + contractor_mob_days_vec[j] + construction_days_vec[j])
       } else {
           res_est[j] <- as.numeric(cases$contractor_ready_date[j] + cases$base_contractor_mob[j] + cases$base_construction_days[j])
       }
    }
    res_est_date <- as.Date(res_est, origin = "1970-01-01")

    # Count active cases anywhere in the pipeline at actual_start
    active_constr <- sum(
      cases$intake_date <= actual_start &
        (is.na(res_est_date) | res_est_date >= actual_start)
    )

    impl_mult <- 1 + k_impl * max(0, active_constr - 1)
    mob_days  <- round(cases$base_contractor_mob[i] * impl_mult)
    con_days  <- round(cases$base_construction_days[i] * impl_mult)

    applied_impl_mult_v[i]     <- impl_mult
    contractor_mob_days_vec[i] <- mob_days
    construction_days_vec[i]   <- con_days
    constr_start_vec[i]        <- actual_start
    queue_wait_vec[i]          <- as.integer(max(0, as.numeric(actual_start - ready_date)))

    if (use_contractor_queue) {
      slot_idx <- which.min(slot_free_dates)
      slot_free_dates[slot_idx] <- actual_start + mob_days + con_days
    }
  }

  cases$applied_impl_multiplier   <- applied_impl_mult_v
  cases$contractor_mob_days       <- contractor_mob_days_vec
  cases$construction_days         <- construction_days_vec
  cases$queue_wait_days           <- queue_wait_vec
  cases$contractor_start          <- constr_start_vec
  cases$contractor_mobilized_date <- constr_start_vec + contractor_mob_days_vec
  cases$construction_start        <- cases$contractor_mobilized_date
  cases$resolution_date           <- cases$construction_start + cases$construction_days

  cases <- cases |> dplyr::arrange(arrival_date, case_id)

  # --- Phase 4: Outcome Metrics ---
  cases <- cases |>
    mutate(
      days_to_resolution = as.numeric(resolution_date - intake_date),
      days_on_interim_supply = as.numeric(
        difftime(as.POSIXct(resolution_date), interim_datetime, units = "days")
      ),
      resolution_year = lubridate::year(resolution_date),
      resolved_within_model_year  = resolution_year <= model_year,
      resolved_within_90_days     = days_to_resolution <= 90,
      resolved_within_180_days    = days_to_resolution <= 180,
      resolved_within_365_days    = days_to_resolution <= 365
    )

  intake_dates_final     <- cases$intake_date
  resolution_dates_final <- cases$resolution_date

  concurrent_at_arrival <- vapply(
    seq_len(nrow(cases)),
    function(i) {
      sum(intake_dates_final <= intake_dates_final[i] &
            resolution_dates_final >= intake_dates_final[i])
    },
    FUN.VALUE = integer(1)
  )

  cases$concurrent_cases_at_arrival <- concurrent_at_arrival

  slot_free_dates_out <- if (use_contractor_queue) slot_free_dates else NULL
  
  list(cases = cases, slot_free_dates = slot_free_dates_out)
}