# 00_setup.R
# Packages, folders and parameters shared by every script.
#
# Usage: source(here::here("Code", "00_setup.R")) at the top of each script.

library(here)
library(yaml)
library(data.table)

# ---- Folders ----------------------------------------------------------------

dir_derived <- here::here("Data", "Derived")    # datasets created by the scripts
dir_figures <- here::here("Output", "Figures")
dir_tables  <- here::here("Output", "Tables")
dir_models  <- here::here("Output", "Models")

# ---- Parameters -------------------------------------------------------------

cfg <- yaml::read_yaml(here::here("config", "config.yml"))

# Machine-specific overrides (paths only, by convention). The file is not
# tracked; when it exists its entries replace those of config.yml.
local_config_file <- here::here("config", "local.yml")
if (file.exists(local_config_file)) {
  cfg <- utils::modifyList(cfg, yaml::read_yaml(local_config_file))
  message("Using local overrides from config/local.yml")
}

# Read one parameter, for example get_param("study", "years").
# Stops when the parameter is missing or still null, so that no analysis runs
# on a value nobody chose.
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

is_absolute_path <- function(path) grepl("^(/|~|[A-Za-z]:[/\\\\])", path)

# Full path of a raw input declared under inputs: in the configuration.
# Raw inputs are read-only: scripts never write into the input folder.
input_path <- function(name) {
  relative <- get_param("inputs", name)
  root <- get_param("paths", "input_root")
  if (!is_absolute_path(root)) root <- here::here(root)
  path <- if (is_absolute_path(relative)) relative else file.path(root, relative)
  if (!file.exists(path)) {
    stop(
      "Input '", name, "' not found: ", path,
      "\nSee Data/README.md for where each input comes from.",
      call. = FALSE
    )
  }
  path
}

# Full path of a dataset written by an earlier script.
derived_path <- function(file) {
  path <- file.path(dir_derived, file)
  if (!file.exists(path)) {
    stop("Derived file not found: ", path, "\nRun the earlier scripts first.",
         call. = FALSE)
  }
  path
}

# ---- Study frame ------------------------------------------------------------

# The 33 study municipalities: INE code and the name used in the SINCA files.
study_municipalities <- function() {
  file <- here::here(get_param("study", "municipalities_file"))
  m <- data.table::fread(file, encoding = "UTF-8")
  stopifnot(
    identical(names(m), c("code", "name")),
    nrow(m) == get_param("study", "n_municipalities"),
    !anyDuplicated(m$code), !anyDuplicated(m$name)
  )
  m
}

# All calendar days of the study years. `buffer_days` adds days before the
# first study day, needed to assign lagged exposures to the first deaths.
study_dates <- function(buffer_days = 0) {
  years <- get_param("study", "years")
  seq(as.Date(sprintf("%d-01-01", years[1])) - buffer_days,
      as.Date(sprintf("%d-12-31", years[2])), by = "day")
}
