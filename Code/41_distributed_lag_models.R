# 41_distributed_lag_models.R
# Lag structure of the associations: all the lags in the same model.
#
# Inputs
#   Data/Derived/case_crossover.csv, exposure_lagged.csv   (script 21)
#   Data/Derived/exposure_2009_2020.csv                    (script 13)
# Outputs
#   Output/Tables/lags_compound_or.csv            E10, E01, E11 by lag and cumulative
#   Output/Tables/lags_compound_interaction.csv   REOI, AP, S of the cumulative odds ratios
#   Output/Tables/lags_grid.csv                   the same over the grid of definitions
#   Output/Tables/lags_temperature_or.csv         temperature contrasts by lag and cumulative
#   Output/Tables/lags_temperature_curve.csv      cumulative odds ratio along the temperature scale
#   Output/Tables/lags_ozone_or.csv               ozone per increment, by lag and cumulative
#
# The models of script 30 take one lag at a time. Exposures of consecutive
# days are correlated (a heatwave lasts at least two days by definition), so
# the odds ratio of one lag also carries the effect of its neighbours. Here
# the exposures of all the lags of design$lags_days enter together, with one
# coefficient per lag (unconstrained lag structure):
#
# 1. Compound exposure. Indicators of E10, E01 and E11 at every lag. The
#    cumulative odds ratio of a category is the product of its odds ratios
#    over the lags: the odds of death after being in that category on every
#    day of the window against being in E00 on every day. The additive
#    interaction is computed from the cumulative odds ratios, and also at
#    each lag from the three coefficients of that lag. The model is fitted
#    for the headline definitions and for every heatwave definition and ozone
#    cut-off of the headline ozone series. Over that grid it is fitted a
#    second time with the temperature of every lag in the model (the spline
#    of point 2), to see whether an interaction between the two indicators
#    is temperature that the heatwave threshold leaves out.
# 2. Temperature. Distributed lag non-linear model (package dlnm): natural
#    cubic spline of the temperature percentile (temperature_response) crossed
#    with the lags. Odds ratios of the contrast percentiles against the
#    reference percentile, by lag and cumulative.
# 3. Ozone. Linear term per increment at every lag, alone and together with
#    the temperature cross-basis.

suppressPackageStartupMessages({
  library(survival)
  library(dlnm)
})
source(here::here("Code", "00_setup.R"))
source(here::here("Code", "01_functions.R"))

conf_level <- get_param("models", "conf_level")
lags <- unlist(get_param("design", "lags_days"))
stopifnot(identical(as.integer(lags), 0:max(lags)))   # consecutive lags from 0
outcomes <- unlist(get_param("distributed_lags", "outcomes"))
ozone_increment <- get_param("distributed_lags", "ozone_increment_ppb")
heatwave_main <- get_param("models", "heatwave_main")
ozone_series <- paste0(get_param("air_pollution", "ozone_series_main"), "_",
                       get_param("air_pollution", "interpolation_method_main"))
ozone_flag_main <- paste0(ozone_series, "_gt_p", get_param("models", "ozone_cutoff_main"))
age_labels <- unlist(get_param("mortality", "age_labels"))
temperature <- get_param("temperature_response")
stopifnot(identical(temperature$scale, "local_percentile"))
categories <- c("e10", "e01", "e11")

cc <- fread(derived_path("case_crossover.csv"), encoding = "UTF-8")
cc[, `:=`(date = as.Date(date), case_date = as.Date(case_date))]
lagged <- fread(derived_path("exposure_lagged.csv"), encoding = "UTF-8")
lagged[, date := as.Date(date)]
exposure <- fread(derived_path("exposure_2009_2020.csv"), encoding = "UTF-8",
                  select = c("com", "date", "in_study_period", "warm_season", temperature$variable))
temperature_reference <- exposure[in_study_period == TRUE & warm_season == TRUE,
                                  .(com, value = get(temperature$variable))]
for (lag in lags) {
  lagged[, (paste0("tpct_lag", lag)) := local_percentile(
    get(paste0(temperature$variable, "_lag", lag)), com, temperature_reference
  )]
}
# One spline basis of the temperature percentile per lag
temperature_terms <- character(0)
for (lag in lags) {
  basis <- spline_basis(lagged[[paste0("tpct_lag", lag)]], temperature$spline_knots,
                        temperature$spline_boundary, paste0("temp_lag", lag, "_s"))
  lagged[, (colnames(basis)) := as.data.table(basis)]
  temperature_terms <- c(temperature_terms, colnames(basis))
}
model_data <- lapply(setNames(outcomes, outcomes), function(outcome) {
  prepare_model_data(cc, lagged, outcome, "all", age_labels)
})

# ---- 1. Compound exposure, all lags together ----------------------------------

compound_terms <- as.vector(t(outer(categories, lags, paste, sep = "_lag")))
# Weights that add the coefficients of each category over the lags
cumulative_weights <- t(sapply(categories, function(category) {
  as.numeric(sub("_lag.*", "", compound_terms) == category)
}))
colnames(cumulative_weights) <- compound_terms

# Odds ratios and REOI of one heatwave definition and one ozone flag, by lag
# and cumulative, from the model with all the lags. With `adjust_temperature`
# the temperature splines of all the lags are added to the model.
fit_compound_lags <- function(outcome, heatwave, ozone_flag, adjust_temperature = FALSE) {
  d <- copy(model_data[[outcome]])
  if (adjust_temperature) {
    d[, (temperature_terms) := lagged[exposure_row, temperature_terms, with = FALSE]]
  }
  adjust <- if (adjust_temperature) temperature_terms else character(0)
  for (lag in lags) {
    hw <- lagged[[paste0(heatwave, "_lag", lag)]][d$exposure_row]
    oz <- lagged[[paste0(ozone_flag, "_lag", lag)]][d$exposure_row]
    d[, (paste0("e10_lag", lag)) := as.integer(hw == 1 & oz == 0)]
    d[, (paste0("e01_lag", lag)) := as.integer(hw == 0 & oz == 1)]
    d[, (paste0("e11_lag", lag)) := as.integer(hw == 1 & oz == 1)]
  }
  # Every term needs deaths with and without it on the day of death
  case_days <- d[case == 1 & stats::complete.cases(d[, compound_terms, with = FALSE])]
  deaths_exposed <- vapply(compound_terms, function(term) case_days[get(term) == 1, sum(n_deaths)],
                           numeric(1))
  label <- data.table(outcome = outcome, heatwave = heatwave, ozone = ozone_flag,
                      adjustment = if (adjust_temperature) "temperature" else "none",
                      deaths = case_days[, sum(n_deaths)], smallest_cell = min(deaths_exposed))
  if (min(deaths_exposed) == 0) return(list(label = cbind(label, estimable = FALSE)))
  logged <- with_warnings(fit_conditional_logit(d, c(compound_terms, adjust)))
  fit <- logged$value
  stopifnot(fit$n_deaths == label$deaths)
  covariance <- vcov(fit)[compound_terms, compound_terms]
  by_lag <- odds_ratios(fit, conf_level)[term %in% compound_terms]
  by_lag[, c("category", "lag") := tstrsplit(term, "_lag", fixed = TRUE)]
  cumulative <- combine_coefficients(fit, cumulative_weights, conf_level)
  cumulative[, `:=`(category = categories, lag = "cumulative",
                    p_value = 2 * stats::pnorm(-abs(log_or / se)))]
  cumulative_covariance <- cumulative_weights %*% covariance %*% t(cumulative_weights)
  dimnames(cumulative_covariance) <- list(categories, categories)
  # Interaction of the cumulative odds ratios, and of the odds ratios of each lag
  measures <- function(beta, v) {
    out <- additive_interaction(beta, v, conf_level)
    g <- c(-1, -1, 1)
    log_ratio <- sum(g * beta)
    se_ratio <- sqrt(drop(t(g) %*% v %*% g))
    z <- stats::qnorm(1 - (1 - conf_level) / 2)
    rbind(out, data.table(measure = "multiplicative ratio", estimate = exp(log_ratio),
                          low = exp(log_ratio - z * se_ratio),
                          high = exp(log_ratio + z * se_ratio), null_value = 1))
  }
  interaction <- rbind(
    cbind(lag = "cumulative", measures(setNames(cumulative$log_or, categories), cumulative_covariance)),
    rbindlist(lapply(lags, function(lag) {
      use <- paste0(categories, "_lag", lag)
      cbind(lag = as.character(lag), measures(coef(fit)[use], covariance[use, use]))
    }))
  )
  label <- cbind(label, estimable = TRUE,
                 fit_warnings = paste(unique(logged$warnings), collapse = " | "))
  list(label = label, or = rbind(by_lag[, !"term"], cumulative, use.names = TRUE),
       interaction = interaction)
}

headline <- lapply(outcomes, fit_compound_lags, heatwave = heatwave_main, ozone_flag = ozone_flag_main)
stopifnot(all(vapply(headline, function(x) x$label$estimable, logical(1))))
compound_or <- rbindlist(lapply(headline, function(x) {
  cbind(x$label[, .(outcome, deaths, fit_warnings)], x$or)
}))
setcolorder(compound_or, c("outcome", "category", "lag"))
fwrite(compound_or, file.path(dir_tables, "lags_compound_or.csv"))
fwrite(rbindlist(lapply(headline, function(x) {
  cbind(x$label[, .(outcome, deaths, fit_warnings)], x$interaction[lag == "cumulative", !"lag"])
})), file.path(dir_tables, "lags_compound_interaction.csv"))

# The same model over the grid of definitions: every heatwave definition and
# every cut-off of the headline ozone series
heatwaves <- unique(sub("_lag[0-9]+$", "", grep("^hw_", names(lagged), value = TRUE)))
ozone_flags <- unique(sub("_lag[0-9]+$", "",
                          grep(paste0("^", ozone_series, "_gt_p[0-9]+_lag"), names(lagged),
                               value = TRUE)))
grid_jobs <- CJ(outcome = outcomes, heatwave = heatwaves, ozone = ozone_flags,
                adjust_temperature = c(FALSE, TRUE), sorted = FALSE)
grid_fits <- run_parallel(seq_len(nrow(grid_jobs)), function(i) {
  x <- fit_compound_lags(grid_jobs$outcome[i], grid_jobs$heatwave[i], grid_jobs$ozone[i],
                         grid_jobs$adjust_temperature[i])
  if (!x$label$estimable) return(x$label)
  or <- dcast(x$or, lag ~ category, value.var = c("or", "or_low", "or_high"))
  measures <- dcast(x$interaction[, .(lag, measure = gsub(" ", "_", tolower(measure)),
                                      estimate, low, high)],
                    lag ~ measure, value.var = c("estimate", "low", "high"))
  cbind(x$label, merge(or, measures, by = "lag"))
}, "Grid fit")
lags_grid <- rbindlist(grid_fits, fill = TRUE)
fwrite(lags_grid, file.path(dir_tables, "lags_grid.csv"))

# ---- 2 and 3. Temperature and ozone -------------------------------------------

ozone_terms <- paste0("ozone_lag", lags)
curve_grid <- 1:99
run_continuous <- function(outcome, model) {
  d <- copy(model_data[[outcome]])
  with_temperature <- model %in% c("temperature", "temperature + ozone")
  with_ozone <- model %in% c("ozone", "temperature + ozone")
  terms <- character(0)
  if (with_temperature) {
    # One row per referent day: its temperature percentile at each lag
    history <- sapply(lags, function(lag) lagged[[paste0("tpct_lag", lag)]][d$exposure_row])
    cross_basis <- crossbasis(
      history, lag = range(lags),
      argvar = list(fun = "ns", knots = temperature$spline_knots,
                    Boundary.knots = temperature$spline_boundary),
      arglag = list(fun = "integer")
    )
    basis_terms <- paste0("cb", seq_len(ncol(cross_basis)))
    d[, (basis_terms) := as.data.table(unclass(cross_basis)[, seq_len(ncol(cross_basis))])]
    terms <- c(terms, basis_terms)
  }
  if (with_ozone) {
    for (lag in lags) {
      d[, (paste0("ozone_lag", lag)) :=
          lagged[[paste0(ozone_series, "_lag", lag)]][exposure_row] / ozone_increment]
    }
    terms <- c(terms, ozone_terms)
  }
  logged <- with_warnings(fit_conditional_logit(d, terms))
  fit <- logged$value
  label <- data.table(outcome = outcome, model = model, deaths = fit$n_deaths,
                      fit_warnings = paste(unique(logged$warnings), collapse = " | "))
  out <- list()
  if (with_temperature) {
    predict_at <- function(at) {
      crosspred(cross_basis, coef = coef(fit)[basis_terms],
                vcov = vcov(fit)[basis_terms, basis_terms], model.link = "logit",
                at = at, cen = temperature$reference, ci.level = conf_level)
    }
    contrast <- predict_at(temperature$contrasts)
    by_lag <- rbindlist(lapply(seq_along(lags), function(j) {
      data.table(percentile = temperature$contrasts, lag = as.character(lags[j]),
                 or = contrast$matRRfit[, j], or_low = contrast$matRRlow[, j],
                 or_high = contrast$matRRhigh[, j])
    }))
    cumulative <- data.table(percentile = temperature$contrasts, lag = "cumulative",
                             or = contrast$allRRfit, or_low = contrast$allRRlow,
                             or_high = contrast$allRRhigh)
    out$temperature <- cbind(label, reference = temperature$reference, rbind(by_lag, cumulative))
    curve <- predict_at(curve_grid)
    out$curve <- cbind(label, reference = temperature$reference,
                       data.table(percentile = curve_grid, or = curve$allRRfit,
                                  or_low = curve$allRRlow, or_high = curve$allRRhigh))
  }
  if (with_ozone) {
    by_lag <- odds_ratios(fit, conf_level)[term %in% ozone_terms,
                                           .(lag = sub("ozone_lag", "", term), or, or_low, or_high)]
    weights <- matrix(1, nrow = 1, ncol = length(ozone_terms),
                      dimnames = list(NULL, ozone_terms))
    cumulative <- combine_coefficients(fit, weights, conf_level)[, .(lag = "cumulative", or,
                                                                    or_low, or_high)]
    out$ozone <- cbind(label, increment_ppb = ozone_increment, rbind(by_lag, cumulative))
  }
  out
}
continuous_jobs <- CJ(outcome = outcomes, model = c("temperature", "ozone", "temperature + ozone"),
                      sorted = FALSE)
continuous <- lapply(seq_len(nrow(continuous_jobs)), function(i) {
  run_continuous(continuous_jobs$outcome[i], continuous_jobs$model[i])
})
fwrite(rbindlist(lapply(continuous, `[[`, "temperature")),
       file.path(dir_tables, "lags_temperature_or.csv"))
fwrite(rbindlist(lapply(continuous, `[[`, "curve")),
       file.path(dir_tables, "lags_temperature_curve.csv"))
fwrite(rbindlist(lapply(continuous, `[[`, "ozone")),
       file.path(dir_tables, "lags_ozone_or.csv"))

message("Distributed-lag models written for: ", paste(outcomes, collapse = ", "))
print(compound_or[outcome == outcomes[1], .(category, lag, or = round(or, 3),
                                            or_low = round(or_low, 3), or_high = round(or_high, 3))])
