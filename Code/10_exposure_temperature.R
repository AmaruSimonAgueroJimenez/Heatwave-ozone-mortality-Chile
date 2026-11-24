# 10_exposure_temperature.R
# Heatwave indicators for the 33 study municipalities from CR2MET v2.5.
#
# Inputs
#   inputs$cr2met_tmax, inputs$cr2met_tmin   daily series by municipality, 1980-2021
# Outputs
#   Data/Derived/exposure_temperature.csv    one row per municipality and day
#   Output/Tables/temperature_thresholds.csv thresholds by municipality
#
# Definitions (analysis plan): a day is a heatwave day when it is at least the
# n-th consecutive day (n = 2, 3, 4) with maximum temperature above the 90th,
# 95th or 99th percentile of the municipality (reference period 1990-2020) or
# above 30 C, plus the excess heat factor (EHF) for the same three durations:
# 15 indicators in total.
#
# Consecutive days are counted within each municipality on a complete calendar.

source(here::here("Code", "00_setup.R"))
source(here::here("Code", "01_functions.R"))

municipalities <- study_municipalities()
ref_years <- get_param("temperature", "percentile_reference_years")
percentiles <- get_param("temperature", "percentile_thresholds")
absolute_c <- get_param("temperature", "absolute_threshold_celsius")
durations <- get_param("temperature", "min_consecutive_days")
season_rule <- get_param("temperature", "percentile_season")
warm_months <- get_param("study", "warm_season_months")
stopifnot(season_rule %in% c("all_year", "warm_season"))

# ---- 1. Read and put on a complete calendar ---------------------------------

temp <- merge(
  read_cr2met(input_path("cr2met_tmax"), "tmax", municipalities$code),
  read_cr2met(input_path("cr2met_tmin"), "tmin", municipalities$code),
  by = c("com", "date")
)
stopifnot(nrow(temp) == nrow(municipalities) * uniqueN(temp$date))

temp <- fill_calendar(temp, c("tmax", "tmin"),
                      handling = get_param("temperature", "leap_day_handling"))
temp[, year := year(date)]
temp[, warm_season := is_warm_season(date, warm_months)]

# ---- 2. Thresholds by municipality ------------------------------------------

# Days used for the percentiles: reference years, original records only (the
# filled 29 February days are left out). Both season rules are tabulated; the
# indicators use the one set in the configuration.
temp[, in_reference := year >= ref_years[1] & year <= ref_years[2] & !temp_filled]

percentile_table <- function(rule) {
  days <- if (rule == "warm_season") temp[in_reference & warm_season] else temp[in_reference == TRUE]
  out <- days[, as.list(stats::quantile(tmax, percentiles / 100, type = 7, names = FALSE)), by = com]
  setnames(out, c("com", paste0("p", percentiles)))
  out[, season_rule := rule]
  out
}
thresholds_all <- rbind(percentile_table("all_year"), percentile_table("warm_season"))
thresholds_all <- merge(municipalities, thresholds_all, by.x = "code", by.y = "com")
setnames(thresholds_all, c("code", "name"), c("com", "municipality"))
fwrite(thresholds_all, file.path(dir_tables, "temperature_thresholds.csv"))

thresholds <- thresholds_all[season_rule == get_param("temperature", "percentile_season"),
                             c("com", paste0("p", percentiles)), with = FALSE]
temp <- merge(temp, thresholds, by = "com")
setorder(temp, com, date)

# ---- 3. Heatwave indicators -------------------------------------------------

# Single hot days: maximum temperature strictly above the threshold
for (p in percentiles) {
  temp[, (paste0("hot_p", p)) := as.integer(tmax > get(paste0("p", p)))]
}
temp[, (paste0("hot_", absolute_c, "c")) := as.integer(tmax > absolute_c)]

hot_columns <- c(paste0("hot_p", percentiles), paste0("hot_", absolute_c, "c"))
for (hot in hot_columns) {
  for (n in durations) {
    indicator <- paste0(sub("^hot_", "hw_", hot), "_", n, "d")
    temp[, (indicator) := consecutive_flag(get(hot), n), by = com]
  }
}

# Excess heat factor, one indicator per duration (EHF > 0)
if (isTRUE(get_param("temperature", "include_ehf"))) {
  for (n in durations) {
    temp[, c("tad", paste0("ehf_", n, "d")) := excess_heat_factor(
      tmax, tmin, in_reference, n_days = n,
      accl_days = get_param("temperature", "ehf_acclimatisation_days"),
      prob = get_param("temperature", "ehf_reference_percentile") / 100
    ), by = com]
    temp[, (paste0("hw_ehf_", n, "d")) := as.integer(get(paste0("ehf_", n, "d")) > 0)]
  }
}

# With leap_day_handling = "missing" the indicators are empty wherever their
# window includes 29 February; nothing is filled in here.

# ---- 4. Keep the study days and save ----------------------------------------

lag_buffer <- max(unlist(get_param("design", "lags_days")))
keep_dates <- study_dates(buffer_days = lag_buffer)
exposure_temperature <- temp[date %in% keep_dates]
exposure_temperature <- merge(municipalities, exposure_temperature, by.x = "code", by.y = "com")
setnames(exposure_temperature, c("code", "name"), c("com", "municipality"))
exposure_temperature[, c("in_reference", paste0("p", percentiles)) := NULL]
setorder(exposure_temperature, com, date)

stopifnot(
  nrow(exposure_temperature) == nrow(municipalities) * length(keep_dates),
  anyDuplicated(exposure_temperature[, .(com, date)]) == 0
)

hw_columns <- grep("^hw_", names(exposure_temperature), value = TRUE)
n_expected <- length(durations) *
  (length(percentiles) + 1 + isTRUE(get_param("temperature", "include_ehf")))
stopifnot(length(hw_columns) == n_expected)

fwrite(exposure_temperature, file.path(dir_derived, "exposure_temperature.csv"))

message("exposure_temperature.csv: ", nrow(exposure_temperature), " rows, ",
        length(hw_columns), " heatwave indicators, ",
        sum(exposure_temperature$temp_filled), " municipality-days filled (29 February)")
