# =============================================================================
# 00_setup.R
# Project setup: packages, folders and study parameters.
#
# Source this file at the top of every script and report:
#   source(here::here("Code", "00_setup.R"))
# =============================================================================

# ---- Packages ---------------------------------------------------------------
# Only what the setup itself needs. Each script loads its own packages.
library(here)
library(yaml)

# ---- Folders ----------------------------------------------------------------
dir_input   <- here::here("Data", "Input")     # raw inputs: read-only, never modified
dir_derived <- here::here("Data", "Derived")    # datasets created by the scripts
dir_figures <- here::here("Output", "Figures")
dir_tables  <- here::here("Output", "Tables")
dir_models  <- here::here("Output", "Models")

# ---- Study parameters -------------------------------------------------------
cfg <- yaml::read_yaml(here::here("config", "config.yml"))

# Returns one parameter from config/config.yml, for example
#   get_param("temperature", "percentile_reference_years")
# Stops if the parameter is missing or still set to null, so an undecided
# value can never be replaced by a silent default.
get_param <- function(...) {
  keys <- c(...)
  value <- cfg
  for (key in keys) {
    if (!is.list(value) || is.null(value[[key]])) {
      stop(
        "Parameter '", paste(keys, collapse = "$"),
        "' is not set in config/config.yml.",
        call. = FALSE
      )
    }
    value <- value[[key]]
  }
  value
}

# Returns the full path of an input file registered under `inputs` in
# config/config.yml. Stops if the file is not in Data/Input.
input_path <- function(name) {
  path <- file.path(dir_input, get_param("inputs", name))
  if (!file.exists(path)) {
    stop(
      "Input file not found: ", path,
      "\nSee Data/README.md for where each input comes from.",
      call. = FALSE
    )
  }
  path
}
