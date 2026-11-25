# run_all.R
# Runs the whole R pipeline with one command: gets the software ready, checks
# the inputs and the methods, runs the scripts in order, renders the R
# reports and compares the tables with their copies from before the run. It
# ends with an error if a report could not be rendered or if, after a complete
# run, an output of the project was not written by the run.
#
# Usage
#   In RStudio, with the project open: the Source button on this file, or
#     source("Code/run_all.R")
#   From a terminal, in the project folder:
#     Rscript Code/run_all.R
#
# Options
#   --from=30      start at script 30, after a run that stopped there; the
#                  inputs, the software and the checks are always done
#   --no-install   stop if an R package is missing, instead of installing it
#   In RStudio the options are given first, for example
#     run_all_arguments <- "--from=30"; source("Code/run_all.R")
#
# Nothing else has to be done by hand: a missing R package is installed from
# CRAN, and Quarto is looked for where it is usually installed. The reports
# written in Python are a separate step with its own runner, Code/run_python.py.
#
# Approximate run times on two cores: script 12 about 20 minutes, script 14
# about 70 minutes (one variogram fit per day and station) and script 30 about
# 15 minutes; every other script takes seconds to a few minutes. The three use
# up to eight cores (see n_workers() in Code/01_functions.R): with eight, the
# whole run took 14 minutes.
#
# Outputs
#   everything the scripts and the R reports write
#   Data/Derived/run_all_log.txt          what every step printed
#   Data/Derived/reproduction_check.csv   each table against its copy from
#                                         before the run

arguments <- commandArgs(trailingOnly = TRUE)
if (exists("run_all_arguments")) {
  arguments <- run_all_arguments
  rm(run_all_arguments)                # so that the next run does not inherit them
}
known <- arguments == "--no-install" | grepl("^--from=[0-9]+$", arguments)
if (!all(known)) {
  stop("Unknown argument: ", paste(arguments[!known], collapse = " "), call. = FALSE)
}
install_allowed <- !"--no-install" %in% arguments
from <- as.integer(sub("^--from=", "", c(grep("^--from=", arguments, value = TRUE), "--from=0")[1]))

# The three packages that the setup itself needs, with base R only: on a new
# computer nothing else is installed yet
if (!file.exists(file.path("Code", "00_packages.R"))) {
  stop("Run this script from the project folder: open Heatwave-ozone-mortality-Chile.Rproj ",
       "in RStudio, or start R in that folder.", call. = FALSE)
}
source(file.path("Code", "00_packages.R"))
install_missing_packages(c("here", "yaml", "data.table"), allow = install_allowed)

source(here::here("Code", "00_setup.R"))     # also asks for one BLAS thread, which the steps inherit
source(here::here("Code", "01_functions.R"))

rscript <- file.path(R.home("bin"), "Rscript")
run_started <- Sys.time()

# ---- Log --------------------------------------------------------------------

# Everything the steps print goes to the console and to this file, so that a
# run that stops can be read afterwards
log_file <- file.path(dir_derived, "run_all_log.txt")
writeLines(sprintf("Run of Code/run_all.R started %s", format(run_started, "%Y-%m-%d %H:%M")),
           log_file)
say <- function(...) {
  text <- paste0(...)
  message(text)
  cat(text, "\n", file = log_file, append = TRUE, sep = "")
}

if (length(arguments) > 0) say("Options: ", paste(arguments, collapse = " "))

# Run one step in a fresh process and return whether it succeeded. Each script
# runs in its own R session so that none depends on objects left by another.
steps_with_dead_workers <- character(0)
run_step <- function(label, command, step_arguments) {
  started <- Sys.time()
  say("\n===== ", label, " (started ", format(started, "%H:%M"), ") =====")
  output <- suppressWarnings(system2(command, step_arguments, stdout = TRUE, stderr = TRUE))
  status <- attr(output, "status")
  succeeded <- is.null(status) || status == 0
  cat(output, sep = "\n")
  cat(output, file = log_file, sep = "\n", append = TRUE)
  # run_parallel() says so when it had to run again the jobs of a dead worker
  if (any(grepl("came back without a result", output, fixed = TRUE))) {
    steps_with_dead_workers <<- c(steps_with_dead_workers, label)
  }
  say(sprintf("%s %s in %.1f minutes", label, if (succeeded) "finished" else "FAILED",
              as.numeric(difftime(Sys.time(), started, units = "mins"))))
  succeeded
}
run_script <- function(file, script_arguments = NULL) {
  if (!run_step(basename(file), rscript, c(shQuote(file), script_arguments))) {
    stop("Script failed: ", basename(file), ". Its messages are above and in ",
         "Data/Derived/run_all_log.txt.", call. = FALSE)
  }
}

# The tables and figures as they are before the run, to compare with afterwards
tables_before <- tempfile("tables_before_")
dir.create(tables_before)
stopifnot(all(file.copy(list.files(dir_tables, pattern = "\\.csv$", full.names = TRUE),
                        tables_before)))
png_files <- function() list.files(dir_figures, pattern = "\\.png$", full.names = TRUE)
figures_before <- tools::md5sum(png_files())

# ---- Preparation: software, inputs, methods ---------------------------------

run_script(here::here("Code", "04_software.R"), if (!install_allowed) "--no-install")
run_script(here::here("Code", "03_input_manifest.R"))
run_script(here::here("tests", "test_methods.R"))

# ---- Scripts ----------------------------------------------------------------

scripts <- c(
  "10_exposure_temperature.R",      # heatwave indicators from CR2MET
  "11_exposure_air_pollution.R",    # delivered interpolated SINCA series
  "12_ozone_station_series.R",      # ozone rebuilt from the hourly records
  "13_exposure_merge_qc.R",         # exposure dataset and alignment checks
  "14_interpolation_validation.R",  # leave-one-station-out cross-validation
  "15_export_geodata.R",            # geometries in open formats
  "16_reanalysis_comparison.R",     # ozone against the CAMS reanalysis
  "17_validation_summary.R",        # tables of the ozone validation
  "18_exposure_time_series.R",      # series of every municipality and of their mean
  "20_mortality_deis.R",            # deaths by cause, age and sex
  "21_case_crossover_dataset.R",    # case-crossover dataset
  "30_models_main.R",               # conditional logistic regression
  "31_attributable_burden.R",       # excess deaths by municipality, year and age
  "40_sensitivity_analyses.R",      # adjustment, reference category, ozone series
  "41_distributed_lag_models.R",    # all the lags in one model
  "50_figures_publication.R"        # publication figures from the tables
)
script_number <- as.integer(substr(scripts, 1, 2))
complete_run <- from <= min(script_number)
if (!complete_run) {
  if (!from %in% script_number) stop("No script has the number ", from, ".", call. = FALSE)
  say("\nStarting at script ", from, ": the earlier scripts are not run again.")
}
for (script in scripts[script_number >= from]) run_script(here::here("Code", script))

# ---- R reports --------------------------------------------------------------

quarto <- quarto_command()
Sys.setenv(QUARTO_R = R.home("bin"))                # the reports run in this same R
reports <- sort(list.files(here::here("docs"), pattern = "\\.qmd$"))
reports <- reports[!grepl("^py_", reports)]         # the Python reports: Code/run_python.py
rendered <- vapply(reports, function(report) {
  run_step(report, quarto, c("render", shQuote(here::here("docs", report))))
}, logical(1))

# ---- Reproduction check -----------------------------------------------------

tolerance <- get_param("reproduction", "tolerance")
check <- compare_tables(tables_before, dir_tables, tolerance)
fwrite(check, file.path(dir_derived, "reproduction_check.csv"))
figures_after <- tools::md5sum(png_files())
had_copy <- intersect(names(figures_before), names(figures_after))
same_figures <- had_copy[figures_before[had_copy] == figures_after[had_copy]]

say("\n===== reproduction check =====")
say("Tables against their copies from before the run:")
summary_lines <- utils::capture.output(print(check[, .(tables = .N), by = status]))
if (any(check$status != "identical")) {
  summary_lines <- c(summary_lines, utils::capture.output(print(check[status != "identical"])))
}
say(paste(summary_lines, collapse = "\n"))
say(sprintf("PNG figures: %d written; of the %d that had an earlier copy, %d have the same bytes.",
            length(figures_after), length(had_copy), length(same_figures)))
say("PDF figures and reports carry their date and are not compared.")
if (any(check$status == "different")) {
  say("Some tables differ beyond the tolerance of ", tolerance,
      ": see Data/Derived/reproduction_check.csv.\n",
      "When the earlier copies come from another machine this is expected for the tables\n",
      "built on the ordinary kriging computed here (series o3obs_ok and o3mda8_ok, and the\n",
      "validation tables): see \"Reproducibility\" in README.md. Any other table should not differ.")
}
if (length(steps_with_dead_workers) > 0) {
  say("\nWorker processes died in: ", paste(steps_with_dead_workers, collapse = ", "),
      ". Their jobs were run again one by one, so no result is missing; the messages are in the log.")
}
say(sprintf("\nTotal time: %.0f minutes",
            as.numeric(difftime(Sys.time(), run_started, units = "mins"))))
say("The reports written in Python are a separate step: python3 Code/run_python.py")

# ---- Every report was rendered and every output was written by this run -----

problems <- character(0)
if (!all(rendered)) {
  problems <- c(problems, paste0("Reports that could not be rendered: ",
                                 paste(reports[!rendered], collapse = ", "), "."))
}
# After a complete run, a table, figure, dataset or report older than the
# start of the run is one that no step of the pipeline writes any more.
if (complete_run && all(rendered)) {
  outputs <- c(
    list.files(dir_tables, pattern = "\\.csv$", full.names = TRUE),
    list.files(dir_figures, pattern = "\\.(pdf|png|txt)$", full.names = TRUE),
    list.files(dir_derived, pattern = "\\.(csv|geojson|txt)$", full.names = TRUE),
    list.files(here::here("docs"), pattern = "\\.html$", full.names = TRUE),
    here::here("Output", "run_environment.txt")
  )
  # Written by the comparison above, by hand (the files that the reports
  # include, whose names start with "_"), or by the Python runner
  other_origin <- basename(outputs) == "reproduction_check.csv" |
    grepl("^(_|py_|run_python_)", basename(outputs))
  outputs <- outputs[!other_origin]
  left_over <- outputs[file.mtime(outputs) < run_started]
  if (length(left_over) > 0) {
    problems <- c(problems, paste0("Files that no step of this run wrote:\n  ",
                                   paste(left_over, collapse = "\n  ")))
  }
}
if (length(problems) > 0) {
  say("\n", paste(problems, collapse = "\n"))
  stop(paste(problems, collapse = "\n"), "\nThe messages of every step are above and in ",
       "Data/Derived/run_all_log.txt.", call. = FALSE)
}
