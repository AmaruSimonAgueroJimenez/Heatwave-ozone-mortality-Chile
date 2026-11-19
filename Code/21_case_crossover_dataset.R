# =============================================================================
# 21_case_crossover_dataset.R
# Stage 2 - Analytical dataset for the space-time-stratified case-crossover
# design.
#
# Inputs
#   Deaths from 20_mortality_deis.R
#   Exposure dataset from 12_exposure_merge_qc.R
#
# Output
#   Data/Derived: one row per case day and control day, ready for conditional
#   logistic regression
#
# Steps
#   1. For each death, take as controls the other days in the same
#      municipality, year, month and day of the week (config:
#      design$control_strata). Each case has 3 or 4 controls.
#   2. Attach exposure at each lag (config: design$lags_days) to case and
#      control days: heatwave, ozone and compound exposure.
#   3. Checks: every stratum has exactly one case day per death and 3 or 4
#      control days; no exposure is missing without being reported.
# =============================================================================

source(here::here("Code", "00_setup.R"))
source(here::here("Code", "01_functions.R"))

stop("21_case_crossover_dataset.R is a scaffold: the analysis code has not been written yet.")
