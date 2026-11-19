# =============================================================================
# 30_models_main.R
# Stage 3 - Main models and additive interaction.
#
# Input
#   Case-crossover dataset from 21_case_crossover_dataset.R
#
# Outputs
#   Output/Tables: estimates of the individual, joint and interaction models
#   Output/Figures: first figures of the interaction results
#   Output/Models: fitted model objects (not tracked by git)
#
# Steps
#   1. Conditional logistic regression for heatwave only, ozone only and
#      compound exposure, relative to days with neither exposure.
#   2. Additive interaction between heatwave and ozone, with confidence
#      intervals.
#   3. Attributable mortality burden and estimates by subgroup, as defined in
#      the analysis plan.
#
# The exposure window, the interaction measures and the subgroups are fixed in
# the analysis plan before fitting any model.
# =============================================================================

source(here::here("Code", "00_setup.R"))
source(here::here("Code", "01_functions.R"))

stop("30_models_main.R is a scaffold: the analysis code has not been written yet.")
