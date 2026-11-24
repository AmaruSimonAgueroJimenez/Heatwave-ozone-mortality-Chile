# 13_exposure_merge_qc.R
# Exposure dataset 2009-2020: temperature, heatwaves, ozone and co-pollutants
# by municipality and day, with the checks of temporal alignment.
#
# Inputs
#   Data/Derived/exposure_temperature.csv      (script 10)
#   Data/Derived/exposure_air_pollution.csv    (script 11)
#   Data/Derived/ozone_rebuilt_municipal.csv   (script 12)
#   inputs$sinca_opr                           station temperature, alignment check only
# Outputs
#   Data/Derived/exposure_2009_2020.csv            the exposure dataset
#   Output/Tables/exposure_alignment.csv           coverage of each source
#   Output/Tables/temporal_alignment_lags.csv      CR2MET against SINCA temperature
#   Output/Tables/ozone_cutoffs.csv                percentile cut-offs by municipality
#   Output/Tables/compound_frequency_by_municipality.csv
#   Output/Tables/compound_frequency_by_year.csv
#   Output/Tables/exposure_codebook.csv
#
# The dataset holds every day of the study years plus the days before the
# first one that the lagged exposures need. `in_study_period` and
# `warm_season` mark the days of analysis (1 November to 31 March).

source(here::here("Code", "00_setup.R"))
source(here::here("Code", "01_functions.R"))

municipalities <- study_municipalities()
study_years <- get_param("study", "years")
cutoffs <- unlist(get_param("air_pollution", "ozone_percentile_cutoffs"))
scope <- get_param("air_pollution", "ozone_percentile_scope")
stopifnot(scope == "municipality")
lag_buffer <- max(unlist(get_param("design", "lags_days")))
keep_dates <- study_dates(buffer_days = lag_buffer)

read_derived <- function(file) {
  dt <- fread(derived_path(file), encoding = "UTF-8")
  dt[, date := as.Date(date)]
  dt
}
temperature <- read_derived("exposure_temperature.csv")
pollution <- read_derived("exposure_air_pollution.csv")
rebuilt <- read_derived("ozone_rebuilt_municipal.csv")

# ---- 1. Alignment: every source covers the same municipality-day grid -------

grid <- CJ(com = municipalities$code, date = keep_dates)
coverage <- function(dt, source) {
  keys <- dt[, .(com, date)]
  data.table(
    source = source, rows = nrow(dt), municipalities = uniqueN(dt$com),
    first_date = min(dt$date), last_date = max(dt$date), days = uniqueN(dt$date),
    repeated_keys = sum(duplicated(keys)),
    keys_missing_from_grid = nrow(fsetdiff(grid, keys)),
    keys_outside_grid = nrow(fsetdiff(keys, grid))
  )
}
alignment <- rbind(
  coverage(temperature, "temperature (CR2MET)"),
  coverage(pollution, "air pollution (SINCA, delivered)"),
  coverage(rebuilt, "ozone rebuilt from hourly records")
)
fwrite(alignment, file.path(dir_tables, "exposure_alignment.csv"))
stopifnot(all(alignment$repeated_keys == 0), all(alignment$keys_missing_from_grid == 0),
          all(alignment$keys_outside_grid == 0))

# ---- 2. Alignment of the calendars: CR2MET against SINCA temperature --------

# Day-to-day changes of the CR2MET maximum temperature are correlated with the
# day-to-day changes of the SINCA station temperature shifted by -2 to +2
# days. Aligned calendars give the highest correlation at shift 0. Leap years
# are looked at separately after 29 February, the day absent from CR2MET.
sinca_temp <- read_sinca_interpolated(
  input_path(paste0("sinca_", get_param("air_pollution", "series_version_main"))),
  municipalities, "temp", date_from = min(keep_dates), date_to = max(keep_dates)
)
check <- merge(temperature[, .(com, date, tmax, temp_filled)],
               sinca_temp[, .(com, date, sinca_temp = temp_idw)], by = c("com", "date"))
setorder(check, com, date)
check[, d_tmax := tmax - shift(tmax), by = com]
# Changes that involve a filled 29 February are left out
check[, original_pair := !temp_filled & !shift(temp_filled), by = com]
check[, leap_year_after_feb := (year(date) %% 4 == 0) & month(date) >= 3]
lag_table <- rbindlist(lapply(-2:2, function(k) {
  check[, d_sinca := shift(sinca_temp, n = -k) - shift(sinca_temp, n = -k + 1), by = com]
  check[original_pair == TRUE & !is.na(d_sinca),
        .(shift_days = k, n = .N, correlation = cor(d_tmax, d_sinca)),
        by = .(period = fifelse(leap_year_after_feb, "leap years, March to December",
                                "all other days"))]
}))
setorder(lag_table, period, shift_days)
fwrite(lag_table, file.path(dir_tables, "temporal_alignment_lags.csv"))
best <- lag_table[, .SD[which.max(correlation)], by = period]
if (!all(best$shift_days == 0)) {
  stop("CR2MET and SINCA calendars are not aligned: see temporal_alignment_lags.csv",
       call. = FALSE)
}

# ---- 3. Merge ---------------------------------------------------------------

exposure <- merge(temperature, pollution[, !"municipality"], by = c("com", "date"))
exposure <- merge(exposure, rebuilt[, !"municipality"], by = c("com", "date"))
stopifnot(nrow(exposure) == nrow(grid))
exposure[, in_study_period := year >= study_years[1] & year <= study_years[2]]

# ---- 4. Ozone categories by municipality-specific percentiles ---------------

# Six ozone series: delivered (o3), rebuilt 24-hour mean (o3obs) and rebuilt
# MDA8 (o3mda8), each by IDW and by ordinary kriging. Cut-offs are the
# percentiles of the warm-season days of the study years in each municipality.
ozone_series <- as.vector(outer(c("o3", "o3obs", "o3mda8"), c("idw", "ok"), paste, sep = "_"))
cutoff_tables <- list()
for (series in ozone_series) {
  cut <- exposure[in_study_period & warm_season & !is.na(get(series)),
                  as.list(stats::quantile(get(series), cutoffs / 100, type = 7, names = FALSE)),
                  by = com]
  setnames(cut, c("com", paste0("p", cutoffs)))
  cutoff_tables[[series]] <- cbind(series = series, cut)
  exposure <- merge(exposure, cut, by = "com")
  for (p in cutoffs) {
    exposure[, (paste0(series, "_gt_p", p)) := as.integer(get(series) > get(paste0("p", p)))]
  }
  exposure[, (paste0("p", cutoffs)) := NULL]
}
ozone_cutoffs <- merge(municipalities, rbindlist(cutoff_tables), by.x = "code", by.y = "com")
setnames(ozone_cutoffs, c("code", "name"), c("com", "municipality"))
setorder(ozone_cutoffs, series, com)
fwrite(ozone_cutoffs, file.path(dir_tables, "ozone_cutoffs.csv"))

# ---- 5. Frequency of compound exposure over the definition grid -------------

heatwaves <- grep("^hw_", names(exposure), value = TRUE)
ozone_flags <- grep("_gt_p[0-9]+$", names(exposure), value = TRUE)
days <- exposure[in_study_period & warm_season]
compound_counts <- function(by_column) {
  rbindlist(lapply(heatwaves, function(hw) rbindlist(lapply(ozone_flags, function(oz) {
    days[, category := compound_category(get(hw), get(oz))]
    out <- days[, .(
      days = .N, e00 = sum(category == "E00", na.rm = TRUE),
      e10 = sum(category == "E10", na.rm = TRUE), e01 = sum(category == "E01", na.rm = TRUE),
      e11 = sum(category == "E11", na.rm = TRUE), not_classified = sum(is.na(category))
    ), by = by_column]
    cbind(heatwave = hw, ozone = oz, out)
  }))))
}
by_municipality <- merge(municipalities, compound_counts("com"), by.x = "code", by.y = "com")
setnames(by_municipality, c("code", "name"), c("com", "municipality"))
fwrite(by_municipality, file.path(dir_tables, "compound_frequency_by_municipality.csv"))
fwrite(compound_counts("year"), file.path(dir_tables, "compound_frequency_by_year.csv"))

# ---- 6. Save the dataset and its codebook -----------------------------------

setcolorder(exposure, c("com", "municipality", "date", "year", "in_study_period", "warm_season"))
setorder(exposure, com, date)
fwrite(exposure, file.path(dir_derived, "exposure_2009_2020.csv"))

describe <- function(name) {
  rules <- list(
    "^com$" = "Municipality code (INE)",
    "^municipality$" = "Municipality name",
    "^date$" = "Calendar day",
    "^year$" = "Calendar year",
    "^in_study_period$" = "Day inside the study years (FALSE for the lag buffer days)",
    "^warm_season$" = "Day between 1 November and 31 March",
    "^tmax$" = "Daily maximum temperature, CR2MET v2.5 (degrees C)",
    "^tmin$" = "Daily minimum temperature, CR2MET v2.5 (degrees C)",
    "^temp_filled$" = "Temperature absent from CR2MET (29 February), filled as configured",
    "^tad$" = "Daily mean temperature: (tmax + next-day tmin) / 2 (degrees C)",
    "^hot_" = "Maximum temperature above the threshold in the name (1/0)",
    "^hw_ehf_" = "Heatwave day: excess heat factor above zero, window in the name (1/0)",
    "^hw_" = "Heatwave day: at least the n-th consecutive day above the threshold (1/0)",
    "^ehf_" = "Excess heat factor, window in the name (degrees C squared)",
    "_gt_p[0-9]+$" = "Ozone above the municipality percentile in the name, warm-season days of the study years (1/0)",
    "_ok_warning$" = "Variogram fit raised a warning that day",
    "_ok_failed$" = "Variogram could not be fitted that day (too few stations); kriging value missing",
    "_n_stations$" = "Stations with a value that day",
    "_ok_var$" = "Ordinary kriging variance",
    "^o3obs_" = "Ozone, 24-hour mean rebuilt from hourly records without trimming or imputation (ppb)",
    "^o3mda8_" = "Ozone, daily maximum 8-hour mean rebuilt from hourly records (ppb)",
    "^o3_" = "Ozone, 24-hour mean, delivered interpolated series (ppb)",
    "^pm25_" = "PM2.5, delivered interpolated series (ug/m3)",
    "^no2_" = "Nitrogen dioxide, delivered interpolated series (ppb)",
    "^hum_" = "Relative humidity, delivered interpolated series (percent)"
  )
  for (pattern in names(rules)) if (grepl(pattern, name)) return(rules[[pattern]])
  stop("No codebook entry for column: ", name, call. = FALSE)
}
codebook <- data.table(
  variable = names(exposure),
  description = vapply(names(exposure), describe, character(1)),
  missing = vapply(exposure, function(x) sum(is.na(x)), integer(1))
)
fwrite(codebook, file.path(dir_tables, "exposure_codebook.csv"))

message("exposure_2009_2020.csv: ", nrow(exposure), " rows, ", ncol(exposure), " columns; ",
        exposure[in_study_period & warm_season, .N],
        " municipality-days in the warm seasons of the study years")
