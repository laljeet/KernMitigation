# KernMitigation

Code for the manuscript *Operational Saturation Precedes Budget Exhaustion in Dry-Well Mitigation Programs*, a stochastic case-flow simulation of the Kern Subbasin dry-well mitigation program under California's Sustainable Groundwater Management Act (SGMA).

The model follows individual dry-well mitigation cases through an eight-stage workflow, from intake to restored water service, under varying caseload, arrival timing, contractor capacity, and multi-year conditions. It quantifies where the program saturates operationally and how that relates to budget exhaustion.

## Requirements

- R (version 4.4.0 or later)
- Packages: `dplyr`, `tidyr`, `readr`, `purrr`, `furrr`, `future`, `MASS`, `sensitivity`, `lubridate`, `ggplot2`, `forcats`

Install with:

```r
install.packages(c("dplyr","tidyr","readr","purrr","furrr","future",
                   "MASS","sensitivity","lubridate","ggplot2","forcats"))
```

## Repository structure

```
Scripts/                          Model pipeline and run drivers
Manuscript Tables and Figures codes/   Table and figure generation
```

## Input data

All inputs are CSV files derived from the Kern Subbasin Groundwater Sustainability Plan (GSP) and Mitigation Plan, cited in the manuscript.

| File | Contents |
|---|---|
| `scenario_parameters.csv` | Per-scenario distribution parameters, cadences, budgets, and option mix for the SW and S families |
| `scenario_parameters_contractor_queue.csv` | Scenario parameters for the CQ (contractor queue) family |
| `program_rules.csv` | Thresholds, budgets, eligibility rules, interim-supply clocks |
| `workflow_steps.csv` | Ordered eight-stage workflow |
| `timing_assumptions.csv` | Documented and assumed step timing |
| `mitigation_options.csv` | The five long-term mitigation pathways |
| `projected_case_inputs.csv` | Physical impact context inputs |

## Pipeline

The numbered scripts form the model in dependency order. They are sourced by the `run_*.R` drivers rather than executed individually.

| Script | Role |
|---|---|
| `01_load_inputs.R` | Load and validate input CSVs |
| `02_build_case_arrivals.R` | Generate case arrival streams (spread or clustered) |
| `03_assign_case_attributes.R` | Assign mitigation option and per-case attributes |
| `04_run_case_flow.R` | Core engine: per-case duration draws, copula, meeting queues, load multiplier, contractor slot queue |
| `05_apply_budget_logic.R` | Apply budget accounting |
| `06_summarize_outputs.R` | Aggregate per-iteration outcome metrics |
| `07_diagnostics.R` | Diagnostic checks |
| `08_monte_carlo.R` | Monte Carlo orchestration over iterations |
| `09_convergence_check.R` | Iteration-count convergence check |
| `10_multiyear_monte_carlo.R` | Multi-year coupled runs |
| `11_sobol_sensitivity.R` | Sobol variance decomposition |
| `build_cq_scenarios.R` | Build the contractor queue scenario file from the baseline |

## How to reproduce

Run from the repository root, with input CSVs in `data/` and outputs written to `outputs/tables/`. Adjust paths in the drivers if your layout differs.

1. **Caseload sweep and clustered families** (SW01–SW10, S03–S05):
   ```r
   source("Scripts/run_monte_carlo.R")
   ```

2. **Contractor queue family** (CQ01–CQ12). First build the scenario file, then run:
   ```r
   source("Scripts/build_cq_scenarios.R")
   source("Scripts/run_contractor_queue.R")
   ```

3. **Multi-year coupling** (SW02, SW04, SW06, SW08 over five years):
   ```r
   source("Scripts/run_multiyear.R")
   ```

4. **Convergence check**:
   ```r
   source("Scripts/run_convergence.R")
   ```

5. **Sobol sensitivity** (at the SW06 baseline; long run):
   ```r
   source("Scripts/run_sobol.R")
   ```

Each scenario family runs 3,000 Monte Carlo iterations. The Sobol analysis uses 2,048 base samples with 200 inner iterations per evaluation and is the most computationally intensive step.

## Tables and figures

After the runs complete, generate the manuscript tables and figures from the result CSVs:

```r
source("Manuscript Tables and Figures codes/make_tables.R")
```

This writes the main table (Sobol sensitivity) and the supplementary tables (scenario design, headline results, multi-year, contractor queue). Figure scripts in the same folder read the same result CSVs.

## Reproducibility notes

Monte Carlo iterations use L'Ecuyer-CMRG parallel random number streams for reproducibility across parallel workers. Results are produced as aggregated CSVs; the tables and figures read these directly, so they can be regenerated without rerunning the full simulation.

## Citation

If you use this code, please cite the associated manuscript. A versioned, archived release with a DOI accompanies the published article.

## License

See the repository for license terms.
