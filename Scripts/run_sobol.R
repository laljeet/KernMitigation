source("R/01_load_inputs.R")
source("R/02_build_case_arrivals.R")
source("R/03_assign_case_attributes.R")
source("R/04_run_case_flow.R")
source("R/05_apply_budget_logic.R")
source("R/06_summarize_outputs.R")
source("R/07_diagnostics.R")
source("R/08_monte_carlo.R")
source("R/11_sobol_sensitivity.R")

inputs <- load_model_inputs(data_dir = "data")

if (!dir.exists("outputs/tables")) dir.create("outputs/tables", recursive = TRUE)

# -------------------------------------------------------
# Run Sobol GSA
# -------------------------------------------------------
# 
# n_sobol: number of Sobol sample points. Total model evaluations
#   = n_sobol * (k_params + 2) = 512 * 13 = 6,656 for a test run.
#   For the paper, use n_sobol = 1024 or 2048.
#
# n_model_iter: MC iterations per evaluation point. More = smoother
#   but slower. 50 is enough for a test, 100-200 for the paper.
#
# Test run: ~6,656 * 50 = 332,800 total MC iterations — a few minutes
# Paper run: ~26,624 * 200 = 5,324,800 — could take an hour+
# -------------------------------------------------------

cat("=== Sobol Sensitivity Analysis ===\n\n")
cat("For paper: increase to n_sobol=1024, n_model_iter=200\n\n")

sobol_results <- run_sobol_analysis(
  inputs = inputs,
  base_scenario_id = "SW06",
  n_sobol = 2048,        # was 512 (test) / 1024 (planned paper run)
  n_model_iter = 200,    # was 50 (test)
  seed = 42
)

# save results
readr::write_csv(
  sobol_results$indices,
  "outputs/tables/sobol_indices.csv"
)

# print formatted results
cat("\n\n=== SOBOL SENSITIVITY INDICES ===\n")
cat("Base scenario:", sobol_results$base_scenario, "\n")
cat("Output variable: mean days to resolution\n\n")

cat(sprintf("%-28s %6s %6s %6s  %s\n", 
            "Parameter", "S1", "ST", "Rank", "Interpretation"))
cat(paste(rep("-", 70), collapse = ""), "\n")

for (i in seq_len(nrow(sobol_results$indices))) {
  r <- sobol_results$indices[i, ]
  cat(sprintf("%-28s %6.3f %6.3f %4d   %s\n",
              r$parameter, r$S1, r$ST, r$rank, r$interpretation))
}

cat(paste(rep("-", 70), collapse = ""), "\n")
cat("\nS1 = First-order index (direct contribution to variance)\n")
cat("ST = Total-order index (including interactions)\n")
cat("ST > 0.20 = major driver; 0.10-0.20 = moderate; < 0.05 = negligible\n")

cat("\n\nKey question: Do the uncalibrated admin timing ranges (site_visit_mode,\n")
cat("technical_eval_mode, kmec_mode, legal_mode) have high ST values?\n")
cat("If not, their exact values don't matter for the conclusion.\n")

if (!dir.exists("outputs/rds")) dir.create("outputs/rds", recursive = TRUE)

# Save the full Sobol analysis object
saveRDS(sobol_results, "outputs/rds/sobol_results_full.rds")
cat("\nSaved raw object to outputs/rds/sobol_results_full.rds\n")