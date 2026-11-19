# =============================================================================
# 11_exposure_air_pollution.R
# Stage 1 - Air pollution exposure: daily ozone, PM2.5 and NO2 by municipality.
#
# Inputs
#   SINCA daily interpolated series by municipality
#   (config: inputs$sinca_opr for the main analysis, inputs$sinca_all for
#   sensitivity analyses)
#
# Output
#   Data/Derived: one row per municipality-day with the pollutant series
#   selected in config (air_pollution$interpolation_method, $min_stations)
#
# Steps
#   1. Read the series and check the municipality-day grid (no gaps, no
#      duplicates) and the coverage of each pollutant.
#   2. Apply the minimum number of monitors (columns *_n_estaciones).
#   3. Compare inverse distance weighting (*_idw) with ordinary kriging (*_ok)
#      and report where they disagree.
#
# Notes from the data author
#   - A municipality-day is NA when fewer than 3 monitors report that day.
#   - The monitoring network grows in 2009 (PM2.5 from 4 to 11 monitors, NO2
#     from 3 to 11): values before and after are not directly comparable.
#   - *_n_estaciones counts imputed monitor-days as available.
#   - Ordinary kriging can return slightly negative concentrations.
#   - Always report the series version, the method and the filter used.
#
# Method: https://github.com/ClimChange-NewbornHealth/pollution-interpolation-kriging-idw
# =============================================================================

source(here::here("Code", "00_setup.R"))
source(here::here("Code", "01_functions.R"))

stop("11_exposure_air_pollution.R is a scaffold: the analysis code has not been written yet.")
