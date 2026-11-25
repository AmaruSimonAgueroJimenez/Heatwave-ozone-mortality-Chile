# 40_sensitivity_analyses.R
# Sensitivity analyses of the compound model (headline definitions).
#
# Inputs
#   Data/Derived/case_crossover.csv, exposure_lagged.csv   (script 21)
#   Data/Derived/exposure_2009_2020.csv                    (script 13)
#   Data/Derived/exposure_air_pollution_all.csv            (script 11)
#   Output/Tables/models_grid.csv                          (script 30)
# Outputs
#   Output/Tables/sensitivity_adjustment_or.csv            odds ratios by adjustment
#   Output/Tables/sensitivity_adjustment_interaction.csv   REOI, AP, S by adjustment
#   Output/Tables/sensitivity_reference_or.csv             three ozone levels
#   Output/Tables/sensitivity_reference_interaction.csv
#   Output/Tables/sensitivity_ozone_series.csv             alternative ozone series
#
# 1. Multi-pollutant adjustment. The compound model is refitted with PM2.5,
#    nitrogen dioxide and relative humidity as continuous covariates at the
#    same lag as the exposure, one at a time and together, and (not in the
#    plan) with the temperature of the same day. Every variant uses the same
#    deaths: those whose referent days have all the covariates.
# 2. Reference category. Three ozone levels (low, medium, high) instead of two,
#    so that the reference is "no heatwave and low ozone" as worded in the plan.
# 3. Ozone series. The same model with each ozone series of the exposure
#    dataset and with the version "all" of the delivered series.
#
# One element changes at a time; everything else is the headline model of
# script 30, built with the same functions (Code/01_functions.R).

suppressPackageStartupMessages(library(survival))
source(here::here("Code", "00_setup.R"))
source(here::here("Code", "01_functions.R"))

conf_level <- get_param("models", "conf_level")
lags <- unlist(get_param("design", "lags_days"))
outcomes <- unlist(get_param("sensitivity", "outcomes"))
heatwave_main <- get_param("models", "heatwave_main")
series_main <- get_param("air_pollution", "ozone_series_main")
method_main <- get_param("air_pollution", "interpolation_method_main")
cutoff_main <- get_param("models", "ozone_cutoff_main")
cutoffs <- unlist(get_param("air_pollution", "ozone_percentile_cutoffs"))
ozone_flag_main <- paste0(series_main, "_", method_main, "_gt_p", cutoff_main)
age_labels <- unlist(get_param("mortality", "age_labels"))
increments <- get_param("sensitivity", "adjustment_increments")
adjust_for_temperature <- get_param("sensitivity", "adjust_for_temperature")
low_cutoff <- get_param("sensitivity", "low_ozone_cutoff")
temperature <- get_param("temperature_response")
stopifnot(identical(temperature$scale, "local_percentile"), low_cutoff < cutoff_main,
          low_cutoff %in% cutoffs)

cc <- fread(derived_path("case_crossover.csv"), encoding = "UTF-8")
cc[, `:=`(date = as.Date(date), case_date = as.Date(case_date))]
lagged <- fread(derived_path("exposure_lagged.csv"), encoding = "UTF-8")
lagged[, date := as.Date(date)]
exposure <- fread(derived_path("exposure_2009_2020.csv"), encoding = "UTF-8")
exposure[, date := as.Date(date)]
warm_study <- exposure[in_study_period == TRUE & warm_season == TRUE]

# Temperature of each lagged day as a percentile of the warm-season days of
# its municipality
temperature_reference <- warm_study[, .(com, value = get(temperature$variable))]
for (lag in lags) {
  lagged[, (paste0("tpct_lag", lag)) := local_percentile(
    get(paste0(temperature$variable, "_lag", lag)), com, temperature_reference
  )]
}

model_data <- lapply(setNames(outcomes, outcomes), function(outcome) {
  prepare_model_data(cc, lagged, outcome, "all", age_labels)
})

# ---- 1. Multi-pollutant adjustment -------------------------------------------

covariates <- names(increments)
adjustments <- list("none" = character(0))
for (v in covariates) adjustments[[v]] <- v
adjustments[[paste(covariates, collapse = " + ")]] <- covariates
if (adjust_for_temperature) {
  adjustments[["temperature"]] <- "temperature"
  adjustments[[paste(c(covariates, "temperature"), collapse = " + ")]] <- c(covariates, "temperature")
}
spline_at <- function(percentiles) {
  spline_basis(percentiles, temperature$spline_knots, temperature$spline_boundary, "temp_s")
}

run_adjustment <- function(outcome, lag) {
  d <- attach_compound_exposure(model_data[[outcome]], lagged, heatwave_main, ozone_flag_main, lag)
  for (v in covariates) {
    d[, (v) := lagged[[paste0(v, "_", method_main, "_lag", lag)]][exposure_row] / increments[[v]]]
  }
  basis <- spline_at(lagged[[paste0("tpct_lag", lag)]][d$exposure_row])
  temperature_terms <- colnames(basis)
  d[, (temperature_terms) := as.data.table(basis)]
  # The same deaths in every variant: strata complete in all the covariates
  complete <- stats::complete.cases(d[, c(covariates, temperature_terms), with = FALSE])
  keep <- d[, .(ok = all(complete[.I])), by = stratum][ok == TRUE, stratum]
  deaths_before <- d[case == 1, sum(n_deaths)]
  d <- d[stratum %in% keep]
  label <- data.table(outcome = outcome, lag = lag, deaths = d[case == 1, sum(n_deaths)],
                      deaths_dropped = deaths_before - d[case == 1, sum(n_deaths)])
  or_rows <- list()
  interaction_rows <- list()
  for (name in names(adjustments)) {
    keys <- adjustments[[name]]
    terms <- c(setdiff(keys, "temperature"), if ("temperature" %in% keys) temperature_terms)
    logged <- with_warnings(compound_results(d, conf_level, adjust = terms))
    result <- logged$value
    stopifnot(result$fit$n_deaths == label$deaths)
    or <- result$or[!term %in% temperature_terms]
    if ("temperature" %in% keys) {
      # Odds ratio of a day at each contrast percentile against the reference
      weights <- sweep(spline_at(temperature$contrasts), 2, spline_at(temperature$reference)[1, ])
      contrast <- combine_coefficients(result$fit, weights, conf_level)
      contrast[, `:=`(term = paste0("temperature_p", temperature$contrasts),
                      p_value = 2 * stats::pnorm(-abs(log_or / se)))]
      or <- rbind(or, contrast, use.names = TRUE)
    }
    fit_warnings <- paste(unique(logged$warnings), collapse = " | ")
    or_rows[[name]] <- cbind(label, adjustment = name, or, fit_warnings = fit_warnings)
    interaction_rows[[name]] <- cbind(label, adjustment = name, result$interaction,
                                      fit_warnings = fit_warnings)
  }
  list(or = rbindlist(or_rows), interaction = rbindlist(interaction_rows))
}
jobs <- CJ(outcome = outcomes, lag = lags, sorted = FALSE)
adjusted <- lapply(seq_len(nrow(jobs)), function(i) run_adjustment(jobs$outcome[i], jobs$lag[i]))
adjustment_or <- rbindlist(lapply(adjusted, `[[`, "or"))
adjustment_or[, increment := vapply(term, function(x) {
  if (x %in% covariates) as.numeric(increments[[x]]) else NA_real_
}, numeric(1))]
fwrite(adjustment_or, file.path(dir_tables, "sensitivity_adjustment_or.csv"))
fwrite(rbindlist(lapply(adjusted, `[[`, "interaction")),
       file.path(dir_tables, "sensitivity_adjustment_interaction.csv"))

# ---- 2. Reference category: three ozone levels --------------------------------

high_flag <- ozone_flag_main
low_flag <- paste0(series_main, "_", method_main, "_gt_p", low_cutoff)
level_terms <- c("h0_medium", "h0_high", "h1_low", "h1_medium", "h1_high")

run_reference <- function(outcome, lag) {
  d <- copy(model_data[[outcome]])
  d[, hw := lagged[[paste0(heatwave_main, "_lag", lag)]][exposure_row]]
  d[, above_high := lagged[[paste0(high_flag, "_lag", lag)]][exposure_row]]
  d[, above_low := lagged[[paste0(low_flag, "_lag", lag)]][exposure_row]]
  complete <- d[, .(ok = !anyNA(hw) && !anyNA(above_high) && !anyNA(above_low)),
                by = stratum][ok == TRUE, stratum]
  d <- d[stratum %in% complete]
  stopifnot(d[, all(above_low >= above_high)])
  d[, level := fifelse(above_high == 1, "high", fifelse(above_low == 1, "medium", "low"))]
  for (term in level_terms) {
    parts <- strsplit(term, "_", fixed = TRUE)[[1]]
    d[, (term) := as.integer(hw == as.integer(sub("h", "", parts[1])) & level == parts[2])]
  }
  counts <- d[case == 1, .(deaths = sum(n_deaths)), by = .(hw, level)]
  logged <- with_warnings(fit_conditional_logit(d, level_terms))
  fit <- logged$value
  or <- odds_ratios(fit, conf_level)
  or[, c("heatwave", "ozone_level") := tstrsplit(sub("^h", "", term), "_", fixed = TRUE)]
  or <- merge(or, counts[, .(heatwave = as.character(hw), ozone_level = level, deaths)],
              by = c("heatwave", "ozone_level"))
  reference_deaths <- counts[hw == 0 & level == "low", deaths]
  measures <- rbindlist(lapply(c("high", "medium"), function(level_value) {
    use <- c("h1_low", paste0("h0_", level_value), paste0("h1_", level_value))
    cbind(contrast = paste(level_value, "against low ozone"),
          additive_interaction(coef(fit)[use], vcov(fit)[use, use], conf_level))
  }))
  label <- data.table(outcome = outcome, lag = lag, deaths_total = fit$n_deaths,
                      deaths_reference = reference_deaths,
                      fit_warnings = paste(unique(logged$warnings), collapse = " | "))
  list(or = cbind(label, or), interaction = cbind(label, measures))
}
reference <- lapply(seq_len(nrow(jobs)), function(i) run_reference(jobs$outcome[i], jobs$lag[i]))
fwrite(rbindlist(lapply(reference, `[[`, "or")),
       file.path(dir_tables, "sensitivity_reference_or.csv"))
fwrite(rbindlist(lapply(reference, `[[`, "interaction")),
       file.path(dir_tables, "sensitivity_reference_interaction.csv"))

# ---- 3. Ozone series ----------------------------------------------------------

# Flag of the days above a percentile of the warm-season days of the study
# years in each municipality: the rule of script 13.
flag_above <- function(table, series, percentile) {
  cut <- table[in_study_period == TRUE & warm_season == TRUE & !is.na(get(series)),
               .(cut = stats::quantile(get(series), percentile / 100, type = 7, names = FALSE)),
               by = com]
  as.integer(table[[series]] > cut$cut[match(table$com, cut$com)])
}
# The rule reproduces the flags of the exposure dataset
for (series in c(paste0(series_main, "_idw"), paste0(series_main, "_ok"))) {
  stopifnot(identical(flag_above(exposure, series, cutoff_main),
                      exposure[[paste0(series, "_gt_p", cutoff_main)]]))
}

# Version "all" of the delivered series, flagged and lagged in the same way
version_all <- fread(derived_path("exposure_air_pollution_all.csv"), encoding = "UTF-8",
                     select = c("com", "date", "o3_idw", "o3_ok"))
version_all[, date := as.Date(date)]
setnames(version_all, c("o3_idw", "o3_ok"), c("o3all_idw", "o3all_ok"))
version_all <- merge(version_all, exposure[, .(com, date, in_study_period, warm_season)],
                     by = c("com", "date"))
stopifnot(nrow(version_all) == nrow(exposure))
all_flags <- paste0(c("o3all_idw", "o3all_ok"), "_gt_p", cutoff_main)
for (series in c("o3all_idw", "o3all_ok")) {
  version_all[, (paste0(series, "_gt_p", cutoff_main)) := flag_above(version_all, series, cutoff_main)]
}
all_lagged <- lagged_exposure(version_all[, c("com", "date", all_flags), with = FALSE], all_flags, lags)
lagged <- merge(lagged, all_lagged, by = c("com", "date"), all.x = TRUE, sort = FALSE)
# The merge keeps the rows; the positions stored in the model datasets must
# still point at the same municipality-day
model_data <- lapply(setNames(outcomes, outcomes), function(outcome) {
  prepare_model_data(cc, lagged, outcome, "all", age_labels)
})

series_labels <- c(
  o3_idw = "Delivered, operational period, IDW", o3_ok = "Delivered, operational period, kriging",
  o3all_idw = "Delivered, all values, IDW", o3all_ok = "Delivered, all values, kriging",
  o3obs_idw = "Rebuilt 24-hour mean, IDW", o3obs_ok = "Rebuilt 24-hour mean, kriging",
  o3mda8_idw = "Rebuilt MDA8, IDW", o3mda8_ok = "Rebuilt MDA8, kriging"
)
series_jobs <- CJ(outcome = outcomes, series = names(series_labels), lag = lags, sorted = FALSE)
series_results <- rbindlist(lapply(seq_len(nrow(series_jobs)), function(i) {
  job <- series_jobs[i]
  flag <- paste0(job$series, "_gt_p", cutoff_main)
  d <- attach_compound_exposure(model_data[[job$outcome]], lagged, heatwave_main, flag, job$lag)
  logged <- with_warnings(compound_results(d, conf_level))
  or <- dcast(logged$value$or[, .(term, or, or_low, or_high)], . ~ term,
              value.var = c("or", "or_low", "or_high"))[, !"."]
  measures <- dcast(logged$value$interaction[, .(measure = gsub(" ", "_", tolower(measure)),
                                                 estimate, low, high)],
                    . ~ measure, value.var = c("estimate", "low", "high"))[, !"."]
  cbind(job, series_label = series_labels[[job$series]], ozone = flag, category_deaths(d),
        or, measures, fit_warnings = paste(unique(logged$warnings), collapse = " | "))
}))
# The series already in the definition grid of script 30 give the same numbers
grid <- fread(file.path(dir_tables, "models_grid.csv"))
both <- merge(series_results, grid[heatwave == heatwave_main],
              by = c("outcome", "ozone", "lag"), suffixes = c("", "_grid"))
stopifnot(nrow(both) == length(outcomes) * 6 * length(lags),
          max(abs(both$estimate_reoi - both$estimate_reoi_grid)) < 1e-8,
          max(abs(both$or_e11 - both$or_e11_grid)) < 1e-8)
fwrite(series_results, file.path(dir_tables, "sensitivity_ozone_series.csv"))

message("Sensitivity analyses written: ", nrow(adjustment_or), " adjusted odds ratios, ",
        nrow(series_results), " ozone-series fits")
print(adjustment_or[outcome == outcomes[1] & lag == get_param("models", "lag_main") &
                      term %in% c("e10", "e01", "e11"),
                    .(adjustment, term, deaths, or = round(or, 3), or_low = round(or_low, 3),
                      or_high = round(or_high, 3))])
