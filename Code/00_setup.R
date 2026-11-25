# 00_setup.R
# Packages, session settings, folders and parameters shared by every script.
#
# Usage: source(here::here("Code", "00_setup.R")) at the top of each script.

# One thread for the linear algebra library (BLAS). Some R installations use a
# BLAS that runs on several threads (Apple's Accelerate, OpenBLAS, MKL). The
# long steps already run in parallel worker processes (run_parallel in
# Code/01_functions.R), and a worker process that calls a multi-threaded BLAS
# can crash or hang: on a Mac with R 4.5.2 and Accelerate every worker of
# script 41 died in the matrix products of the model fits. The variables are
# read when the library starts its threads, so they are set here, before any
# matrix operation, and by Code/run_all.R before it starts each script.
Sys.setenv(VECLIB_MAXIMUM_THREADS = "1", OPENBLAS_NUM_THREADS = "1", MKL_NUM_THREADS = "1")

library(here)
library(yaml)
library(data.table)

# ---- Session settings -------------------------------------------------------

# Character values are sorted by the language settings of the computer unless
# the collation is fixed. With the C collation the order of rows and of factor
# levels is the same on every machine (accented letters sort after the others).
invisible(Sys.setlocale("LC_COLLATE", "C"))

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

# ---- Raw inputs -------------------------------------------------------------

is_absolute_path <- function(path) grepl("^(/|~|[A-Za-z]:[/\\\\])", path)

# Where an input declared under inputs: in the configuration is on this
# machine, whether or not the file is there.
input_location <- function(name) {
  relative <- get_param("inputs", name)
  root <- get_param("paths", "input_root")
  if (!is_absolute_path(root)) root <- here::here(root)
  if (is_absolute_path(relative)) relative else file.path(root, relative)
}

# The files the tracked results were produced from, with their size and MD5
# checksum: one row per file, written by Code/03_input_manifest.R. For an
# input that is a folder, `file` is the path of each file inside the folder.
input_manifest_file <- here::here("Data", "input_manifest.csv")
read_input_manifest <- function(file = input_manifest_file) {
  manifest <- data.table::fread(
    file, encoding = "UTF-8",
    colClasses = c(input = "character", file = "character", bytes = "numeric", md5 = "character")
  )
  stopifnot(
    identical(names(manifest), c("input", "file", "bytes", "md5")),
    !anyNA(manifest), !anyDuplicated(manifest[, .(input, file)])
  )
  manifest
}

# Stop unless an input is, byte for byte, the one listed in the manifest.
verify_input <- function(name, path, manifest = read_input_manifest()) {
  expected <- manifest[input == name]
  if (nrow(expected) == 0) {
    stop("Input '", name, "' is not listed in Data/input_manifest.csv.", call. = FALSE)
  }
  files <- if (dir.exists(path)) file.path(path, expected$file) else path
  if (length(files) != nrow(expected)) {
    stop("Input '", name, "' is one file, but the manifest lists ", nrow(expected),
         " files for it.", call. = FALSE)
  }
  absent <- files[!file.exists(files)]
  if (length(absent) > 0) {
    stop("Input '", name, "': file not found:\n  ", paste(absent, collapse = "\n  "),
         "\nSee Data/README.md for where each input comes from.", call. = FALSE)
  }
  differs <- file.size(files) != expected$bytes
  differs[!differs] <- unname(tools::md5sum(files[!differs])) != expected$md5[!differs]
  differs[is.na(differs)] <- TRUE               # a file that cannot be read
  if (any(differs)) {
    stop(
      "Input '", name, "' is not the file the results were produced from:\n  ",
      paste(files[differs], collapse = "\n  "),
      "\nThe expected sizes and checksums are in Data/input_manifest.csv. After a",
      "\ndeliberate change of input, write a new manifest with",
      "\n  Rscript Code/03_input_manifest.R --write",
      call. = FALSE
    )
  }
  invisible(TRUE)
}

# Full path of a raw input. Stops if the input is missing or is not the one of
# the manifest; each input is checked once in an R session.
# Raw inputs are read-only: scripts never change a file of the input folder.
verified_inputs <- new.env()
input_path <- function(name) {
  path <- input_location(name)
  if (!file.exists(path)) {
    stop(
      "Input '", name, "' not found: ", path,
      "\nSee Data/README.md for where each input comes from.",
      call. = FALSE
    )
  }
  if (is.null(verified_inputs[[name]])) {
    verify_input(name, path)
    verified_inputs[[name]] <- TRUE
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
