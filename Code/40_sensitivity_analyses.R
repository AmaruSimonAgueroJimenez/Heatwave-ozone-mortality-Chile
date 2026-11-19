# =============================================================================
# 40_sensitivity_analyses.R
# Stage 4 - Sensitivity analyses.
#
# Inputs
#   Case-crossover dataset from 21_case_crossover_dataset.R
#   Main models from 30_models_main.R
#
# Output
#   Output/Tables: estimates of every sensitivity analysis next to the main
#   result
#
# Planned analyses
#   - Alternative heatwave definitions (all definitions in config).
#   - Air pollution series version "all" instead of "opr", and the alternative
#     interpolation method.
#   - Adjustment for PM2.5 and NO2 (config: air_pollution$adjustment_pollutants).
#   - Non-linear lag structures with distributed lag non-linear models.
#
# Every analysis reuses the functions of the main analysis and changes one
# element at a time.
# =============================================================================

source(here::here("Code", "00_setup.R"))
source(here::here("Code", "01_functions.R"))

stop("40_sensitivity_analyses.R is a scaffold: the analysis code has not been written yet.")
