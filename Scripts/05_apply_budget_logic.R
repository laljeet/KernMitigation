apply_budget_logic <- function(cases, scenario_row, program_rules) {
  suppressPackageStartupMessages({
    library(dplyr)
  })
  
  if (nrow(cases) == 0) return(cases)
  
  dry_well_budget <- as.numeric(scenario_row$dry_well_budget[[1]])
  total_budget    <- as.numeric(scenario_row$total_budget[[1]])
  
  # order by resolution date: costs are incurred at implementation, not intake
  cases <- cases %>%
    dplyr::arrange(.data$resolution_date, .data$case_id)
  
  # separate cost streams
  is_dry_well_case <- cases$option_key != "treatment"
  
  # cumulative spend within each budget pool
  dry_well_costs <- ifelse(is_dry_well_case, cases$estimated_case_cost, 0)
  treatment_costs <- ifelse(!is_dry_well_case, cases$estimated_case_cost, 0)
  
  cases <- cases %>%
    dplyr::mutate(
      cumulative_dry_well_spend = cumsum(dry_well_costs),
      cumulative_treatment_spend = cumsum(treatment_costs),
      cumulative_total_spend = cumsum(.data$estimated_case_cost),
      
      dry_well_budget_exceeded = is_dry_well_case & .data$cumulative_dry_well_spend > dry_well_budget,
      total_budget_exceeded = .data$cumulative_total_spend > total_budget,
      funded_case = !.data$total_budget_exceeded
    ) %>%
    # restore arrival order for downstream reporting
    dplyr::arrange(.data$arrival_date, .data$case_id)
  
  cases
}
