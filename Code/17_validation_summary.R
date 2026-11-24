# 17_validation_summary.R
# Validation of the ozone exposure: tables.
#
# Three questions
#   A. Station inputs: how do the station series used by the data author
#      compare with the daily means computed from the validated hourly SINCA
#      records?
#   B. Interpolation model: how well do IDW and ordinary kriging predict a
#      station left out of the fit (predictions of script 14)?
#   C. Municipal series: how does the delivered series compare with the one
#      rebuilt from the validated records with the same model?
#
# Inputs
#   Data/Derived/ozone_station_daily.csv            (script 12)
#   Data/Derived/exposure_2009_2020.csv             (script 13)
#   Data/Derived/validation_loso_predictions.csv    (script 14)
#   inputs$station_series                           the data author's station series
# Outputs, all in Output/Tables
#   validation_inputs_by_station.csv   validation_inputs_by_level.csv
#   validation_loso_overall.csv        validation_loso_common.csv
#   validation_loso_by_station.csv     validation_loso_high_ozone.csv
#   validation_reproduction.csv        validation_municipal_by_level.csv
#   validation_municipal_flags.csv

source(here::here("Code", "00_setup.R"))
source(here::here("Code", "01_functions.R"))

params <- get_param("interpolation")
study_years <- get_param("study", "years")
warm_months <- get_param("study", "warm_season_months")
cutoffs <- unlist(get_param("air_pollution", "ozone_percentile_cutoffs"))
z <- stats::qnorm(1 - (1 - params$conf_level) / 2)

station_daily <- fread(derived_path("ozone_station_daily.csv"), encoding = "UTF-8")
station_daily[, date := as.Date(date)]
station_daily <- station_daily[year(date) >= study_years[1] & year(date) <= study_years[2]]

author <- new.env()
load(input_path("station_series"), envir = author)

# ---- A. Station inputs ------------------------------------------------------

author_series <- as.data.table(sf::st_drop_geometry(author$imputed_series))[
  , .(station, date, author_o3 = daily_o3)]
inputs <- merge(station_daily, author_series, by = c("station", "date"), all = TRUE)
inputs <- inputs[!is.na(author_o3) | !is.na(mean24)]
inputs[, warm_season := is_warm_season(date, warm_months)]
inputs[, status := fcase(
  !is.na(author_o3) & is.na(mean24), "no validated daily mean",
  is.na(author_o3) & !is.na(mean24), "observed, absent from the author's series",
  abs(author_o3 - mean24) < 0.005, "identical",
  default = "modified"
)]

inputs_summary <- function(dt) {
  dt[, .(
    station_days = .N,
    identical_pct = 100 * mean(status == "identical"),
    modified_pct = 100 * mean(status == "modified"),
    no_validated_mean_pct = 100 * mean(status == "no validated daily mean"),
    absent_pct = 100 * mean(status == "observed, absent from the author's series"),
    mean_observed = mean(mean24[status %in% c("identical", "modified")]),
    mean_author_same_days = mean(author_o3[status %in% c("identical", "modified")]),
    mean_change_when_modified = mean((author_o3 - mean24)[status == "modified"])
  ), by = station]
}
by_station <- rbind(
  cbind(period = "all days", inputs_summary(inputs)),
  cbind(period = "warm season", inputs_summary(inputs[warm_season == TRUE]))
)
total <- rbind(
  cbind(period = "all days", inputs_summary(copy(inputs)[, station := "All stations"])),
  cbind(period = "warm season",
        inputs_summary(copy(inputs[warm_season == TRUE])[, station := "All stations"]))
)
fwrite(rbind(by_station, total), file.path(dir_tables, "validation_inputs_by_station.csv"))

# Difference by ozone level: warm-season station-days with both values,
# grouped by tenths of the observed 24-hour mean
both <- inputs[warm_season == TRUE & status %in% c("identical", "modified")]
both[, level := cut(mean24, stats::quantile(mean24, 0:10 / 10), include.lowest = TRUE,
                    labels = paste0("D", 1:10))]
by_level <- both[, .(
  station_days = .N, observed_mean24 = mean(mean24), observed_mda8 = mean(mda8, na.rm = TRUE),
  author_mean = mean(author_o3), difference = mean(author_o3 - mean24),
  modified_pct = 100 * mean(status == "modified")
), keyby = level]
fwrite(by_level, file.path(dir_tables, "validation_inputs_by_level.csv"))

# ---- B. Leave-one-station-out cross-validation ------------------------------

loso <- fread(derived_path("validation_loso_predictions.csv"), encoding = "UTF-8")
loso[, date := as.Date(date)]

loso_long <- melt(loso, id.vars = c("date", "metric", "station", "observed", "ok_var"),
                  measure.vars = c("idw", "ok"), variable.name = "method",
                  value.name = "predicted")
# All station-days that each method could predict
overall <- loso_long[, validation_metrics(observed, predicted), by = .(metric, method)]
# Share of station-days with no prediction (IDW: no station within the search radius)
overall <- merge(overall, loso_long[, .(not_predicted_pct = 100 * mean(is.na(predicted))),
                                    by = .(metric, method)], by = c("metric", "method"))
# Share of observed values inside the nominal kriging interval
coverage <- loso[!is.na(ok), .(method = "ok", interval_coverage_pct =
  100 * mean(abs(observed - ok) <= z * sqrt(ok_var))), by = metric]
overall <- merge(overall, coverage, by = c("metric", "method"), all.x = TRUE)
fwrite(overall, file.path(dir_tables, "validation_loso_overall.csv"))

# The two methods on the same station-days: IDW gives no value when no station
# lies within its search radius, kriging always does, so the sets above differ
common <- loso[!is.na(idw) & !is.na(ok)]
common_long <- melt(common, id.vars = c("date", "metric", "station", "observed"),
                    measure.vars = c("idw", "ok"), variable.name = "method",
                    value.name = "predicted")
fwrite(common_long[, validation_metrics(observed, predicted), by = .(metric, method)],
       file.path(dir_tables, "validation_loso_common.csv"))

by_station_cv <- loso_long[, validation_metrics(observed, predicted),
                           by = .(metric, method, station)]
fwrite(by_station_cv, file.path(dir_tables, "validation_loso_by_station.csv"))

# High-ozone days: observed and predicted values are each classified against
# their own station percentile, as the exposure categories are built
high <- rbindlist(lapply(cutoffs, function(p) {
  d <- loso_long[!is.na(predicted)]
  d[, `:=`(observed_high = as.integer(observed > stats::quantile(observed, p / 100)),
           predicted_high = as.integer(predicted > stats::quantile(predicted, p / 100))),
    by = .(metric, method, station)]
  cbind(percentile = p,
        d[, classification_agreement(observed_high, predicted_high), by = .(metric, method)])
}))
fwrite(high, file.path(dir_tables, "validation_loso_high_ozone.csv"))

# ---- C. Delivered against rebuilt municipal series --------------------------

exposure <- fread(derived_path("exposure_2009_2020.csv"), encoding = "UTF-8")
exposure[, date := as.Date(date)]
exposure <- exposure[in_study_period == TRUE & warm_season == TRUE]

# Reproduction: on the days when every station value of the author's series
# equals the validated 24-hour mean, the rebuilt municipal series should equal
# the delivered one if this project runs the same interpolation.
day_status <- inputs[, .(all_identical = all(status == "identical")), by = date]
same_input_days <- day_status[all_identical == TRUE, date]
same <- exposure[date %in% same_input_days]
reproduction <- rbindlist(lapply(c("idw", "ok"), function(m) {
  difference <- abs(same[[paste0("o3_", m)]] - same[[paste0("o3obs_", m)]])
  data.table(method = m, days = uniqueN(same$date), municipality_days = sum(!is.na(difference)),
             within_0.001_ppb_pct = 100 * mean(difference < 1e-3, na.rm = TRUE),
             median_abs_difference = stats::median(difference, na.rm = TRUE),
             max_abs_difference = max(difference, na.rm = TRUE))
}))
fwrite(reproduction, file.path(dir_tables, "validation_reproduction.csv"))

municipal_level <- rbindlist(lapply(c("idw", "ok"), function(m) {
  d <- exposure[, .(com, delivered = get(paste0("o3_", m)), rebuilt = get(paste0("o3obs_", m)),
                    mda8 = get(paste0("o3mda8_", m)))]
  d <- d[!is.na(delivered) & !is.na(rebuilt)]
  d[, level := cut(rebuilt, stats::quantile(rebuilt, 0:10 / 10), include.lowest = TRUE,
                   labels = paste0("D", 1:10))]
  cbind(method = m, d[, .(
    municipality_days = .N, rebuilt_mean24 = mean(rebuilt), delivered_mean24 = mean(delivered),
    difference = mean(delivered - rebuilt), rebuilt_mda8 = mean(mda8, na.rm = TRUE)
  ), keyby = level])
}))
fwrite(municipal_level, file.path(dir_tables, "validation_municipal_by_level.csv"))

# Agreement of the high-ozone flags between series (reference: rebuilt)
flag_pairs <- list(c("o3obs", "o3"), c("o3mda8", "o3"), c("o3mda8", "o3obs"))
municipal_flags <- rbindlist(lapply(flag_pairs, function(pair) {
  rbindlist(lapply(c("idw", "ok"), function(m) rbindlist(lapply(cutoffs, function(p) {
    reference <- exposure[[paste0(pair[1], "_", m, "_gt_p", p)]]
    test <- exposure[[paste0(pair[2], "_", m, "_gt_p", p)]]
    values <- exposure[, .(a = get(paste0(pair[1], "_", m)), b = get(paste0(pair[2], "_", m)))]
    cbind(reference = pair[1], compared = pair[2], method = m, percentile = p,
          classification_agreement(reference, test),
          pearson_r = values[!is.na(a) & !is.na(b), cor(a, b)])
  }))))
}))
fwrite(municipal_flags, file.path(dir_tables, "validation_municipal_flags.csv"))

print(overall)
