assign_case_attributes <- function(cases, scenario_row, mitigation_options) {
  suppressPackageStartupMessages({
    library(dplyr)
    library(purrr)
  })
  
  if (nrow(cases) == 0) return(cases)
  
  pct_vec <- c(
    new_well = as.numeric(scenario_row$pct_new_well[[1]]),
    deepen_well = as.numeric(scenario_row$pct_deepen_well[[1]]),
    lower_pump = as.numeric(scenario_row$pct_lower_pump[[1]]),
    consolidation = as.numeric(scenario_row$pct_consolidation[[1]]),
    treatment = as.numeric(scenario_row$pct_treatment[[1]])
  )
  
  pct_vec[is.na(pct_vec)] <- 0
  
  if (sum(pct_vec) <= 0) {
    stop("Scenario percentages for mitigation options sum to zero.")
  }
  
  pct_vec <- pct_vec / sum(pct_vec)
  
  option_lookup <- c(
    new_well = "Construct a new well",
    deepen_well = "Deepen the well",
    lower_pump = "Modify pump equipment, including lowering the pump",
    consolidation = "Consolidation with an existing water system in the vicinity",
    treatment = "Treatment system (POU or POE)"
  )
  
  assigned_option_key <- sample(
    x = names(pct_vec),
    size = nrow(cases),
    replace = TRUE,
    prob = pct_vec
  )
  
  cases %>%
    dplyr::mutate(
      option_key = assigned_option_key,
      option_name = unname(option_lookup[assigned_option_key]),
      needs_contractor = dplyr::case_when(
        .data$option_key %in% c("new_well", "deepen_well", "lower_pump", "consolidation") ~ TRUE,
        .data$option_key == "treatment" ~ FALSE,
        TRUE ~ TRUE
      ),
      estimated_case_cost = dplyr::case_when(
        .data$option_key %in% c("new_well", "deepen_well", "lower_pump", "consolidation") ~
          as.numeric(scenario_row$default_dry_well_cost[[1]]),
        .data$option_key == "treatment" ~
          as.numeric(scenario_row$default_treatment_cost[[1]]),
        TRUE ~ as.numeric(scenario_row$default_dry_well_cost[[1]])
      )
    )
}