# =============================================================================
# 12_exposure_merge_qc.R
# Stage 1 - Exposure dataset: merge temperature and air pollution, quality
# control and spatiotemporal trends.
#
# Inputs
#   Outputs of 10_exposure_temperature.R and 11_exposure_air_pollution.R
#
# Outputs
#   Data/Derived: exposure dataset, one row per municipality-day, warm seasons
#   of the study period
#   Output/Figures: municipality maps and time series of single and compound
#   exposure
#
# Steps
#   1. Build one municipality key. CR2MET identifies municipalities by code and
#      the SINCA series by name: check that the 33 study municipalities match
#      one to one before merging.
#   2. Merge and verify the temporal alignment of both sources over the warm
#      seasons of the study period.
#   3. Define single and compound exposure days (config:
#      air_pollution$ozone_exposure_definition).
#   4. Maps and time series of exposure trends.
# =============================================================================

source(here::here("Code", "00_setup.R"))
source(here::here("Code", "01_functions.R"))

stop("12_exposure_merge_qc.R is a scaffold: the analysis code has not been written yet.")
