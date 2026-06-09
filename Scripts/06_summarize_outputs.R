summarize_scenario <- function(cases, scenario_row, program_rules) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(tibble)
  })
  
  annual_threshold <- get_rule_value(program_rules, "R01")
  model_year <- as.integer(scenario_row$model_year[[1]])
  
  if (nrow(cases) == 0) {
    return(tibble::tibble(
      scenario_id = scenario_row$scenario_id[[1]],
      scenario_name = scenario_row$scenario_name[[1]],
      model_year = model_year,
      n_cases = 0
    ))
  }
  
  tibble::tibble(
    scenario_id = scenario_row$scenario_id[[1]],
    scenario_name = scenario_row$scenario_name[[1]],
    model_year = model_year,
    n_cases = nrow(cases),
    
    n_resolved_by_year_end = sum(cases$resolved_within_model_year, na.rm = TRUE),
    n_unresolved_by_year_end = sum(!cases$resolved_within_model_year, na.rm = TRUE),
    
    n_resolved_within_90_days = sum(cases$resolved_within_90_days, na.rm = TRUE),
    n_resolved_within_180_days = sum(cases$resolved_within_180_days, na.rm = TRUE),
    n_resolved_within_365_days = sum(cases$resolved_within_365_days, na.rm = TRUE),
    
    pct_resolved_within_90_days = mean(cases$resolved_within_90_days, na.rm = TRUE),
    pct_resolved_within_180_days = mean(cases$resolved_within_180_days, na.rm = TRUE),
    pct_resolved_within_365_days = mean(cases$resolved_within_365_days, na.rm = TRUE),
    
    mean_days_to_resolution = mean(cases$days_to_resolution, na.rm = TRUE),
    median_days_to_resolution = median(cases$days_to_resolution, na.rm = TRUE),
    max_days_to_resolution = max(cases$days_to_resolution, na.rm = TRUE),
    
    mean_days_on_interim_supply = mean(cases$days_on_interim_supply, na.rm = TRUE),
    max_days_on_interim_supply = max(cases$days_on_interim_supply, na.rm = TRUE),
    
    peak_concurrent_cases = max(cases$concurrent_cases_at_arrival, na.rm = TRUE),
    
    dry_well_budget_spent = max(cases$cumulative_dry_well_spend, na.rm = TRUE),
    treatment_budget_spent = max(cases$cumulative_treatment_spend, na.rm = TRUE),
    total_budget_spent = max(cases$cumulative_total_spend, na.rm = TRUE),
    
    threshold_exceeded_flag = nrow(cases) > annual_threshold,
    budget_exceeded_flag = any(cases$total_budget_exceeded, na.rm = TRUE),
    dry_well_budget_exceeded_flag = any(cases$dry_well_budget_exceeded, na.rm = TRUE),
    backlog_flag = sum(!cases$resolved_within_model_year, na.rm = TRUE) > 0
  )
}
