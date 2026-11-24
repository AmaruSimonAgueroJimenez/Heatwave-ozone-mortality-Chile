# 12_ozone_station_series.R
# Ozone at the monitoring stations from the hourly SINCA records, and a
# municipal series rebuilt from them with the data author's interpolation.
#
# Why: the delivered municipal series (script 11) is a 24-hour mean computed
# after trimming the hourly values at their 1st and 99th percentiles and
# filling gaps by imputation. The analysis plan names the daily maximum 8-hour
# mean (MDA8). This script rebuilds both metrics from the hourly records as
# published by SINCA, with no trimming and no imputation, and interpolates
# them with the same model and parameters. Only the hourly records with the
# statuses accepted in the configuration are used.
#
# Inputs
#   inputs$sinca_hourly_dir          hourly ozone by station (ppb)
#   inputs$station_series            station coordinates used by the data author
#   inputs$interpolation_locations   one target point per municipality
# Outputs
#   Data/Derived/ozone_station_daily.csv       station-day: mean24, mda8, valid hours
#   Data/Derived/ozone_rebuilt_municipal.csv   municipality-day: o3obs_* (24-hour
#                                              mean) and o3mda8_* (MDA8)
#   Output/Tables/ozone_station_hourly_status.csv
#   Output/Tables/ozone_rebuilt_qc.csv

suppressPackageStartupMessages({
  library(sf)
  library(sp)
  library(gstat)
  library(automap)
})
source(here::here("Code", "00_setup.R"))
source(here::here("Code", "01_functions.R"))

municipalities <- study_municipalities()
params <- get_param("interpolation")
series_rules <- get_param("station_series")
stopifnot(get_param("air_pollution", "negative_values") == "set_missing")
lag_buffer <- max(unlist(get_param("design", "lags_days")))
keep_dates <- study_dates(buffer_days = lag_buffer)

# ---- 1. Stations and target points ------------------------------------------

station_codes <- fread(here::here(series_rules$stations_file), encoding = "UTF-8")
author <- new.env()
load(input_path("station_series"), envir = author)
load(input_path("interpolation_locations"), envir = author)

station_points <- author$imputed_series[!duplicated(author$imputed_series$station), "station"]
station_points <- merge(station_points, station_codes, by = "station")
stopifnot(nrow(station_points) == nrow(station_codes),
          sf::st_crs(station_points)$epsg == params$crs_epsg)

targets <- author$interpol
stopifnot(setequal(targets$comuna, municipalities$name),
          sf::st_crs(targets)$epsg == params$crs_epsg)

# ---- 2. Daily series by station ---------------------------------------------

read_station <- function(i) {
  file <- file.path(input_path("sinca_hourly_dir"),
                    sprintf(series_rules$hourly_file_pattern, station_codes$sinca_id[i]))
  if (!file.exists(file)) stop("Hourly file not found: ", file, call. = FALSE)
  hourly <- read_sinca_hourly(file, accepted_status = unlist(series_rules$accepted_status))
  daily <- daily_from_hourly(
    hourly,
    min_hours = series_rules$min_valid_hours_day,
    mda8_min_hours = series_rules$mda8_min_hours_window,
    mda8_min_windows = series_rules$mda8_min_windows_day
  )
  status <- hourly[date %in% keep_dates, .(hours = .N), by = status]
  list(
    daily = cbind(station_codes[i], daily[date %in% keep_dates]),
    status = cbind(station_codes[i], status)
  )
}
stations <- lapply(seq_len(nrow(station_codes)), read_station)
station_daily <- rbindlist(lapply(stations, `[[`, "daily"))
hourly_status <- rbindlist(lapply(stations, `[[`, "status"))
hourly_status[is.na(status), status := "no_record"]

setorder(station_daily, station, date)
fwrite(station_daily, file.path(dir_derived, "ozone_station_daily.csv"))
fwrite(dcast(hourly_status, sinca_id + station ~ status, value.var = "hours", fill = 0),
       file.path(dir_tables, "ozone_station_hourly_status.csv"))

# ---- 3. Municipal series rebuilt with the same interpolation ----------------

setkey(station_daily, date)
metric_prefix <- c(mean24 = "o3obs", mda8 = "o3mda8")

rebuild_day <- function(day) {
  out <- data.table(municipality = targets$comuna, date = day)
  for (metric in names(metric_prefix)) {
    prefix <- metric_prefix[[metric]]
    values <- station_daily[.(day)][!is.na(get(metric)), .(station, value = get(metric))]
    out[, (paste0(prefix, "_n_stations")) := nrow(values)]
    if (nrow(values) >= params$ok_min_stations) {
      day_stations <- merge(station_points, values, by = "station")
      result <- with_warnings(interpolate_day(day_stations, targets, "value", params))
      out[, (paste0(prefix, c("_idw", "_ok", "_ok_var"))) := result$value[, .(idw, ok, ok_var)]]
      out[, (paste0(prefix, "_ok_warning")) := length(result$warnings) > 0]
      out[, (paste0(prefix, "_ok_failed")) := !is.na(result$value$ok_error[1])]
    } else {
      out[, (paste0(prefix, c("_idw", "_ok", "_ok_var"))) := NA_real_]
      out[, (paste0(prefix, "_ok_warning")) := NA]
      out[, (paste0(prefix, "_ok_failed")) := NA]
    }
  }
  out
}
# The result does not depend on the number of cores.
rebuilt_days <- parallel::mclapply(as.list(keep_dates), rebuild_day,
                                   mc.cores = parallel::detectCores())
failed <- vapply(rebuilt_days, inherits, logical(1), what = "try-error")
if (any(failed)) {
  stop("Interpolation failed on ", format(keep_dates[which(failed)[1]]), ": ",
       rebuilt_days[[which(failed)[1]]], call. = FALSE)
}
rebuilt <- rbindlist(rebuilt_days)
stopifnot(nrow(rebuilt) == nrow(municipalities) * length(keep_dates))

# Same rule as the delivered series for kriging values below zero
qc <- list()
for (prefix in metric_prefix) {
  for (m in c("idw", "ok")) {
    col <- paste0(prefix, "_", m)
    negative <- !is.na(rebuilt[[col]]) & rebuilt[[col]] < 0
    qc[[length(qc) + 1]] <- data.table(
      series = prefix, method = m, rows = nrow(rebuilt),
      missing = sum(is.na(rebuilt[[col]])), negative_set_missing = sum(negative),
      days_below_min_stations = rebuilt[get(paste0(prefix, "_n_stations")) < params$ok_min_stations,
                                        uniqueN(date)],
      days_with_kriging_warning = rebuilt[get(paste0(prefix, "_ok_warning")) == TRUE, uniqueN(date)],
      days_variogram_not_fitted = rebuilt[get(paste0(prefix, "_ok_failed")) == TRUE, uniqueN(date)]
    )
    set(rebuilt, which(negative), col, NA_real_)
    if (m == "ok") set(rebuilt, which(negative), paste0(prefix, "_ok_var"), NA_real_)
  }
}
fwrite(rbindlist(qc), file.path(dir_tables, "ozone_rebuilt_qc.csv"))

rebuilt <- merge(municipalities, rebuilt, by.x = "name", by.y = "municipality")
setnames(rebuilt, c("code", "name"), c("com", "municipality"))
setcolorder(rebuilt, c("com", "municipality", "date"))
setorder(rebuilt, com, date)
fwrite(rebuilt, file.path(dir_derived, "ozone_rebuilt_municipal.csv"))

message("ozone_station_daily.csv: ", nrow(station_daily), " station-days; ",
        "ozone_rebuilt_municipal.csv: ", nrow(rebuilt), " rows")
print(rbindlist(qc))
