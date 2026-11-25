# 14_interpolation_validation.R
# Leave-one-station-out cross-validation of the ozone interpolation.
#
# For every warm-season day of the study years each station is removed in
# turn, the data author's model (IDW and ordinary kriging, same parameters)
# is fitted to the remaining stations, and the removed station is predicted.
# The observed values are the validated 24-hour mean and MDA8 of script 12.
#
# This is the slow step of the pipeline (one variogram fit per day, station
# and metric: about 75 minutes on two cores). It only writes the predictions;
# script 17 turns them into tables.
#
# Inputs
#   Data/Derived/ozone_station_daily.csv   (script 12)
#   inputs$station_series                  station coordinates
# Output
#   Data/Derived/validation_loso_predictions.csv   one row per day, metric and
#                                                  station left out

suppressPackageStartupMessages({
  library(sf)
  library(sp)
  library(gstat)
  library(automap)
})
source(here::here("Code", "00_setup.R"))
source(here::here("Code", "01_functions.R"))

params <- get_param("interpolation")
study_years <- get_param("study", "years")
warm_months <- get_param("study", "warm_season_months")

station_daily <- fread(derived_path("ozone_station_daily.csv"), encoding = "UTF-8")
station_daily[, date := as.Date(date)]
station_daily <- station_daily[year(date) >= study_years[1] & year(date) <= study_years[2]]
station_daily[, warm_season := is_warm_season(date, warm_months)]

author <- new.env()
load(input_path("station_series"), envir = author)
station_points <- author$imputed_series[!duplicated(author$imputed_series$station), "station"]
station_points <- station_points[station_points$station %in% station_daily$station, ]

cv_days <- sort(unique(station_daily[warm_season == TRUE, date]))
setkey(station_daily, date)

loso_day <- function(day) {
  results <- list()
  for (metric in c("mean24", "mda8")) {
    values <- station_daily[.(day)][!is.na(get(metric)), .(station, value = get(metric))]
    # The fit needs ok_min_stations after one station is removed
    if (nrow(values) - 1 < params$ok_min_stations) next
    points <- merge(station_points, values, by = "station")
    for (i in seq_len(nrow(points))) {
      fit <- with_warnings(interpolate_day(points[-i, ], points[i, ], "value", params))
      results[[length(results) + 1]] <- data.table(
        date = day, metric = metric, station = points$station[i],
        observed = points$value[i], n_fit_stations = nrow(points) - 1,
        fit$value, ok_warning = length(fit$warnings) > 0
      )
    }
  }
  rbindlist(results)
}
loso_days <- run_parallel(as.list(cv_days), loso_day, "Cross-validation")
loso <- rbindlist(loso_days)
stopifnot(nrow(loso) > 0)
fwrite(loso, file.path(dir_derived, "validation_loso_predictions.csv"))

message("validation_loso_predictions.csv: ", nrow(loso), " predictions on ", length(cv_days), " days")
