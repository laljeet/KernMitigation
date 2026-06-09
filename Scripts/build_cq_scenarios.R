# build_cq_scenarios.R
# =============================================================
# Generate data/scenario_parameters_contractor_queue.csv from SW06.
#
# Design (Option 3): 15 cases at the GSP threshold, two arrival
# patterns x six slot configurations = 12 scenarios.
#
#   CQ01-CQ06: summer_cluster arrivals, slots = 1, 2, 3, 5, 8, unlimited
#   CQ07-CQ12: spread_even arrivals,    slots = 1, 2, 3, 5, 8, unlimited
#
# The contrast between the two panels isolates the interaction
# between arrival timing (an environmental driver) and contractor
# capacity (an operational variable). Slot count is expected to
# matter substantially under clustered arrivals and minimally
# under spread arrivals; the paper figure shows both regimes as
# empirical bounds on the slot adequacy question.
#
# Only the slot count and arrival pattern are explicit design
# choices. Every other timing parameter is inherited from SW06.
# Re-run this script after any SW06 parameter change to refresh
# the CQ CSV automatically.
# =============================================================

source("R/01_load_inputs.R")
suppressPackageStartupMessages({
  library(dplyr)
  library(purrr)
  library(tidyr)
})

inputs <- load_model_inputs(data_dir = "data")

base <- inputs$scenario_parameters %>%
  dplyr::filter(scenario_id == "SW06")

if (nrow(base) != 1) {
  stop("Expected exactly one SW06 row; got ", nrow(base))
}

# --- Design knobs ---
slot_levels <- c(1, 2, 3, 5, 8, NA_real_)   # NA = unlimited
slot_labels <- c("1 slot", "2 slots", "3 slots", "5 slots",
                 "8 slots", "unlimited slots")

arrival_patterns <- c("summer_cluster", "spread_even")

# --- Build the 12-row scenario table ---
design_grid <- tidyr::expand_grid(
  arrival_pattern = arrival_patterns,
  slot_idx        = seq_along(slot_levels)
) %>%
  dplyr::mutate(
    slots         = slot_levels[slot_idx],
    slot_label    = slot_labels[slot_idx],
    scenario_id   = sprintf("CQ%02d", dplyr::row_number()),
    pattern_tag   = dplyr::case_when(
      arrival_pattern == "summer_cluster" ~ "summer cluster",
      arrival_pattern == "spread_even"    ~ "spread even",
      TRUE                                ~ arrival_pattern
    ),
    scenario_name = sprintf("15 cases, %s, %s", pattern_tag, slot_label)
  )

cq_scenarios <- purrr::map_dfr(seq_len(nrow(design_grid)), function(i) {
  row <- base
  row$scenario_id                 <- design_grid$scenario_id[i]
  row$scenario_name               <- design_grid$scenario_name[i]
  row$arrival_pattern             <- design_grid$arrival_pattern[i]
  row$max_concurrent_construction <- design_grid$slots[i]
  row
})

# --- Sanity checks ---
stopifnot(nrow(cq_scenarios) == length(slot_levels) * length(arrival_patterns))
stopifnot(all(cq_scenarios$annual_reported_cases == base$annual_reported_cases))
stopifnot(length(unique(cq_scenarios$scenario_id)) == nrow(cq_scenarios))

out_path <- "data/scenario_parameters_contractor_queue.csv"
readr::write_csv(cq_scenarios, out_path)

cat("Wrote", nrow(cq_scenarios), "scenarios to", out_path, "\n\n")
cat("Design summary:\n")
print(cq_scenarios %>%
        dplyr::select(scenario_id, scenario_name, annual_reported_cases,
                      arrival_pattern, max_concurrent_construction))
