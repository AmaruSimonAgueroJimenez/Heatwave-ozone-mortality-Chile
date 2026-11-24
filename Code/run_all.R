# run_all.R
# Runs the whole pipeline in order, then renders the reports.
#
# Usage, from the project root:  Rscript Code/run_all.R
#
# Approximate run times on a two-core laptop: script 12 about 20 minutes and
# script 14 about 75 minutes (one variogram fit per day and station); every
# other script takes seconds to a few minutes.

scripts <- c(
  "10_exposure_temperature.R",      # heatwave indicators from CR2MET
  "11_exposure_air_pollution.R",    # delivered interpolated SINCA series
  "12_ozone_station_series.R",      # ozone rebuilt from the hourly records
  "13_exposure_merge_qc.R",         # exposure dataset and alignment checks
  "14_interpolation_validation.R",  # leave-one-station-out cross-validation
  "15_export_geodata.R",            # geometries in open formats
  "16_reanalysis_comparison.R",     # ozone against the CAMS reanalysis
  "17_validation_summary.R",        # tables of the ozone validation
  "20_mortality_deis.R",            # deaths by cause, age and sex
  "21_case_crossover_dataset.R",    # case-crossover dataset
  "30_models_main.R"                # conditional logistic regression
)

for (script in scripts) {
  message("\n===== ", script, " =====")
  started <- Sys.time()
  # Each script runs in a fresh R session so that none depends on objects
  # left behind by another.
  status <- system2(file.path(R.home("bin"), "Rscript"),
                    shQuote(here::here("Code", script)))
  if (status != 0) stop("Script failed: ", script, call. = FALSE)
  message(sprintf("%s finished in %.1f minutes", script,
                  as.numeric(difftime(Sys.time(), started, units = "mins"))))
}

message("\n===== tests =====")
status <- system2(file.path(R.home("bin"), "Rscript"), shQuote(here::here("tests", "test_methods.R")))
if (status != 0) stop("Method checks failed", call. = FALSE)

message("\n===== reports =====")
status <- system2("quarto", c("render", shQuote(here::here())))
if (status != 0) stop("quarto render failed", call. = FALSE)
