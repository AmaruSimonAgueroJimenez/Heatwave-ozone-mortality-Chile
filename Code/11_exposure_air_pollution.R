# 11_exposure_air_pollution.R
# Municipal daily air pollution from the interpolated SINCA series.
#
# Inputs
#   inputs$sinca_opr   interpolated series, operational period (main analysis)
#   inputs$sinca_all   interpolated series, all modelled values (sensitivity)
# Outputs
#   Data/Derived/exposure_air_pollution.csv       main version
#   Data/Derived/exposure_air_pollution_all.csv   sensitivity version
#   Output/Tables/air_pollution_qc.csv            counts behind every QC rule
#
# For each variable the file keeps the IDW value, the ordinary kriging value
# with its variance, and the number of stations. Units as delivered: ozone
# and nitrogen dioxide in ppb, PM2.5 in ug/m3, relative humidity in percent.
# The ozone value is a 24-hour mean.

source(here::here("Code", "00_setup.R"))
source(here::here("Code", "01_functions.R"))

municipalities <- study_municipalities()
variables <- c(get_param("air_pollution", "primary_pollutant"),
               unlist(get_param("air_pollution", "adjustment_pollutants")),
               unlist(get_param("air_pollution", "other_variables")))
methods <- unlist(get_param("air_pollution", "interpolation_methods"))
min_stations <- get_param("air_pollution", "min_stations")
negative_rule <- get_param("air_pollution", "negative_values")
stopifnot(negative_rule == "set_missing")

lag_buffer <- max(unlist(get_param("design", "lags_days")))
keep_dates <- study_dates(buffer_days = lag_buffer)

clean_version <- function(version) {
  dt <- read_sinca_interpolated(
    input_path(paste0("sinca_", version)), municipalities, variables,
    date_from = min(keep_dates), date_to = max(keep_dates)
  )
  # Complete municipality-by-day grid, no repeated rows
  stopifnot(
    nrow(dt) == nrow(municipalities) * length(keep_dates),
    anyDuplicated(dt[, .(com, date)]) == 0
  )
  qc <- list()
  for (v in variables) {
    n_col <- paste0(v, "_n_stations")
    for (m in methods) {
      col <- paste0(v, "_", m)
      negative <- !is.na(dt[[col]]) & dt[[col]] < 0
      few <- !is.na(dt[[col]]) & dt[[n_col]] < min_stations
      qc[[length(qc) + 1]] <- data.table(
        version = version, variable = v, method = m,
        rows = nrow(dt), missing_delivered = sum(is.na(dt[[col]])),
        negative_set_missing = sum(negative),
        below_min_stations_set_missing = sum(few & !negative)
      )
      set(dt, which(negative | few), col, NA_real_)
      if (m == "ok") set(dt, which(negative | few), paste0(v, "_ok_var"), NA_real_)
    }
  }
  list(data = dt, qc = rbindlist(qc))
}

main <- clean_version(get_param("air_pollution", "series_version_main"))
sensitivity <- clean_version(get_param("air_pollution", "series_version_sensitivity"))

fwrite(main$data, file.path(dir_derived, "exposure_air_pollution.csv"))
fwrite(sensitivity$data, file.path(dir_derived, "exposure_air_pollution_all.csv"))
fwrite(rbind(main$qc, sensitivity$qc), file.path(dir_tables, "air_pollution_qc.csv"))

message("exposure_air_pollution.csv: ", nrow(main$data), " rows, variables: ",
        paste(variables, collapse = ", "))
print(main$qc)
