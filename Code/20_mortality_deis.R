# =============================================================================
# 20_mortality_deis.R
# Stage 2 - Mortality: clean the DEIS deaths database.
#
# Input
#   DEIS deaths database (config: inputs$deis_deaths)
#
# Outputs
#   Data/Derived: individual deaths in the study municipalities and warm seasons,
#   with cause groups
#   Output/Tables: deaths by cause group, age, sex, municipality and year
#
# Steps
#   1. Read the deaths database and keep the study years and municipalities.
#   2. Classify causes with the ICD-10 lists in config
#      (mortality$icd10_non_accidental, mortality$icd10_cardiopulmonary).
#   3. Summary table of mortality by cause, age and sex, municipality and year.
# =============================================================================

source(here::here("Code", "00_setup.R"))
source(here::here("Code", "01_functions.R"))

stop("20_mortality_deis.R is a scaffold: the analysis code has not been written yet.")
