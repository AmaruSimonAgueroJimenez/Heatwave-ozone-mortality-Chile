# =============================================================================
# 10_exposure_temperature.R
# Stage 1 - Temperature exposure: daily heatwave indicators by municipality.
#
# Inputs
#   CR2MET daily maximum and minimum temperature by municipality
#   (config: inputs$cr2met_tmax, inputs$cr2met_tmin)
#
# Output
#   Data/Derived: one row per municipality-day with every heatwave indicator
#
# Steps
#   1. Read the CR2MET series and keep the study municipalities.
#   2. Compute municipality-specific percentiles of daily maximum temperature
#      over the reference period (config: temperature$percentile_reference_years).
#   3. Flag heatwave days for every definition in config: percentile or absolute
#      threshold, by minimum number of consecutive days, plus the Excess Heat
#      Factor.
#
# Adapted from Code/2.0 HW_process_data.R in
# https://github.com/ClimChange-NewbornHealth/HeatWave_PretermBirth_Chile
# That script computes percentiles over 1980-2021; this project uses 1990-2020.
# Validation: reproduce the published 1980-2021 reference tables first, then
# switch the reference period.
# =============================================================================

source(here::here("Code", "00_setup.R"))
source(here::here("Code", "01_functions.R"))

stop("10_exposure_temperature.R is a scaffold: the analysis code has not been written yet.")
