load_model_inputs <- function(data_dir = "data") {
  suppressPackageStartupMessages({
    library(readr)
    library(dplyr)
    library(tidyr)
    library(purrr)
    library(lubridate)
    library(stringr)
    library(tibble)
  })
  
  required_files <- c(
    "program_rules.csv",
    "workflow_steps.csv",
    "timing_assumptions.csv",
    "mitigation_options.csv",
    "historical_dry_wells.csv",
    "projected_case_inputs.csv",
    "scenario_parameters.csv"
  )
  
  missing_files <- required_files[!file.exists(file.path(data_dir, required_files))]
  
  if (length(missing_files) > 0) {
    stop(
      "Missing required files in data/: ",
      paste(missing_files, collapse = ", ")
    )
  }
  
  list(
    program_rules = readr::read_csv(file.path(data_dir, "program_rules.csv"), show_col_types = FALSE),
    workflow_steps = readr::read_csv(file.path(data_dir, "workflow_steps.csv"), show_col_types = FALSE),
    timing_assumptions = readr::read_csv(file.path(data_dir, "timing_assumptions.csv"), show_col_types = FALSE),
    mitigation_options = readr::read_csv(file.path(data_dir, "mitigation_options.csv"), show_col_types = FALSE),
    historical_dry_wells = readr::read_csv(file.path(data_dir, "historical_dry_wells.csv"), show_col_types = FALSE),
    projected_case_inputs = readr::read_csv(file.path(data_dir, "projected_case_inputs.csv"), show_col_types = FALSE),
    scenario_parameters = readr::read_csv(file.path(data_dir, "scenario_parameters.csv"), show_col_types = FALSE)
  )
}

get_rule_value <- function(program_rules, rule_id, as_numeric = TRUE) {
  val <- program_rules %>%
    dplyr::filter(.data$rule_id == !!rule_id) %>%
    dplyr::pull(.data$numeric_value)
  
  if (length(val) == 0) return(NA)
  
  if (as_numeric) {
    suppressWarnings(as.numeric(val[[1]]))
  } else {
    val[[1]]
  }
}

get_timing_value <- function(timing_assumptions, parameter_name, scenario_row) {
  row <- timing_assumptions %>%
    dplyr::filter(.data$parameter_name == !!parameter_name)
  
  if (nrow(row) == 0) return(NA_real_)
  
  # direct match: scenario_row has the exact column
  if (!is.null(scenario_row[[parameter_name]]) && !is.na(scenario_row[[parameter_name]])) {
    return(as.numeric(scenario_row[[parameter_name]]))
  }
  
  # fallback map: if scenario_row uses min/mode/max structure, use mode
  mode_lookup <- c(
    site_visit_scheduling_time = "site_visit_mode",
    technical_evaluation_duration = "technical_eval_mode",
    kmec_review_cadence_or_lag = "kmec_mode",
    legal_agreement_completion_time = "legal_mode",
    contractor_mobilization_time = "contractor_mob_median"
  )
  
  if (parameter_name %in% names(mode_lookup)) {
    mode_col <- mode_lookup[[parameter_name]]
    if (!is.null(scenario_row[[mode_col]]) && !is.na(scenario_row[[mode_col]])) {
      return(as.numeric(scenario_row[[mode_col]]))
    }
  }
  
  if (!is.na(row$base_value[[1]])) {
    return(as.numeric(row$base_value[[1]]))
  }
  
  if (!is.na(row$documented_value[[1]]) && row$documented_or_assumed[[1]] == "documented") {
    return(as.numeric(row$documented_value[[1]]))
  }
  
  return(NA_real_)
}

