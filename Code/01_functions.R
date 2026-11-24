# 01_functions.R
# Functions shared by the analysis scripts. Loaded after 00_setup.R.
# Every function takes its parameters as arguments; none reads the
# configuration on its own and none has a default for a study parameter.

# ---- Calendar ---------------------------------------------------------------

# TRUE for dates inside the warm season, given its calendar months (1-12).
is_warm_season <- function(date, months) {
  stopifnot(inherits(date, "Date"), is.numeric(months), all(months %in% 1:12))
  data.table::month(date) %in% months
}

# ---- Temperature (CR2MET) ---------------------------------------------------

# Read one CR2MET municipal series (wide file: four header rows with the
# municipality code, latitude, longitude and area, then one row per day) and
# return it in long form for the requested municipality codes.
read_cr2met <- function(path, value_name, codes) {
  raw <- data.table::fread(path, header = FALSE)
  header <- raw[1:4]
  body <- raw[-(1:4)]
  stopifnot(all(as.matrix(header[, 1:3]) == -9999))
  file_codes <- as.integer(unlist(header[1, -(1:3)]))
  absent <- setdiff(codes, file_codes)
  if (length(absent) > 0) {
    stop("Municipalities missing from ", basename(path), ": ",
         paste(absent, collapse = ", "), call. = FALSE)
  }
  dates <- as.Date(sprintf("%04d-%02d-%02d", body[[1]], body[[2]], body[[3]]))
  if (anyNA(dates) || anyDuplicated(dates) > 0 || is.unsorted(dates)) {
    stop("Invalid, repeated or unsorted dates in ", basename(path), call. = FALSE)
  }
  keep <- match(codes, file_codes) + 3L
  values <- as.matrix(body[, keep, with = FALSE])
  if (anyNA(values) || any(values < -90)) {
    stop("Missing-value codes found in ", basename(path), call. = FALSE)
  }
  out <- data.table::data.table(
    com = rep(codes, each = length(dates)),
    date = rep(dates, times = length(codes)),
    value = as.vector(values)
  )
  data.table::setnames(out, "value", value_name)
  out[]
}

# Put a municipal daily series on a complete calendar. The only gaps accepted
# are isolated 29 February days (absent from CR2MET); any other gap stops the
# script. `handling` is "interpolate" (mean of the two neighbouring days) or
# "missing". The column `temp_filled` marks the days that were absent.
fill_calendar <- function(dt, value_cols, handling) {
  stopifnot(handling %in% c("interpolate", "missing"))
  full <- data.table::CJ(com = unique(dt$com),
                         date = seq(min(dt$date), max(dt$date), by = "day"))
  out <- dt[full, on = .(com, date)]
  data.table::setorder(out, com, date)
  out[, temp_filled := is.na(get(value_cols[1]))]
  gaps <- out[temp_filled == TRUE]
  if (nrow(gaps) > 0) {
    if (!all(format(gaps$date, "%m-%d") == "02-29")) {
      stop("Gaps other than 29 February in the temperature series.", call. = FALSE)
    }
    if (handling == "interpolate") {
      for (v in value_cols) {
        out[, (v) := {
          x <- get(v)
          i <- which(is.na(x))
          x[i] <- (x[i - 1L] + x[i + 1L]) / 2
          x
        }, by = com]
      }
    }
  }
  out[]
}

# ---- Heatwave definitions ---------------------------------------------------

# Day-level heatwave flag: 1 when the day is at least the n-th consecutive day
# with an exceedance, 0 otherwise (the first n - 1 days of a hot spell are not
# flagged). `exceed` is a 0/1 vector for ONE municipality on a complete
# calendar, in date order. The first n - 1 days of the series, and any window
# holding a missing value, return NA.
consecutive_flag <- function(exceed, n) {
  stopifnot(all(exceed %in% c(0L, 1L, NA)), n >= 1)
  as.integer(data.table::frollsum(exceed, n, align = "right") == n)
}

# Excess heat factor for ONE municipality on a complete calendar, following
# the formulas of the reference code (HeatWave_PretermBirth_Chile):
#   TAD      = (tmax of the day + tmin of the next day) / 2
#   EHI_sig  = mean TAD of the last n days - reference percentile of TAD
#   EHI_accl = mean TAD of the last n days - mean TAD of the last accl_days
#   EHF      = EHI_sig * max(1, EHI_accl)
# `is_reference` marks the days used for the reference percentile.
excess_heat_factor <- function(tmax, tmin, is_reference, n_days, accl_days, prob) {
  stopifnot(length(tmax) == length(tmin), length(tmax) == length(is_reference),
            prob > 0, prob < 1)
  tad <- (tmax + data.table::shift(tmin, type = "lead")) / 2
  tad_ref <- stats::quantile(tad[is_reference], prob, na.rm = TRUE, names = FALSE)
  mean_n <- data.table::frollmean(tad, n_days, align = "right")
  ehi_sig <- mean_n - tad_ref
  ehi_accl <- mean_n - data.table::frollmean(tad, accl_days, align = "right")
  list(tad = tad, ehf = ehi_sig * pmax(1, ehi_accl))
}

# ---- Interpolated air pollution (SINCA) -------------------------------------

# Read the municipal daily series delivered by the data author for the
# requested variables and dates. Returns one row per municipality-day with,
# for each variable, the IDW value, the kriging value and variance, and the
# number of stations.
read_sinca_interpolated <- function(path, municipalities, variables, date_from, date_to) {
  suffixes <- c("_idw", "_ok", "_ok_var", "_n_estaciones")
  cols <- c("comuna", "date", as.vector(t(outer(variables, suffixes, paste0))))
  dt <- data.table::fread(path, select = cols, encoding = "UTF-8")
  dt[, date := as.Date(date)]
  dt <- dt[date >= date_from & date <= date_to]
  unknown <- setdiff(unique(dt$comuna), municipalities$name)
  if (length(unknown) > 0) {
    stop("Municipality names not in config/municipalities.csv: ",
         paste(unknown, collapse = ", "), call. = FALSE)
  }
  dt <- merge(dt, municipalities, by.x = "comuna", by.y = "name")
  data.table::setnames(dt, c("code", "comuna"), c("com", "municipality"))
  data.table::setnames(dt, names(dt), sub("_n_estaciones$", "_n_stations", names(dt)))
  data.table::setcolorder(dt, c("com", "municipality", "date"))
  data.table::setorder(dt, com, date)
  dt[]
}

# Compound exposure category from a heatwave flag and a high-ozone flag:
# E00 neither, E10 heatwave only, E01 high ozone only, E11 both.
compound_category <- function(heatwave, ozone_high) {
  stopifnot(all(heatwave %in% c(0L, 1L, NA)), all(ozone_high %in% c(0L, 1L, NA)))
  factor(ifelse(is.na(heatwave) | is.na(ozone_high), NA,
                paste0("E", heatwave, ozone_high)),
         levels = c("E00", "E10", "E01", "E11"))
}

# ---- Station series from the hourly SINCA records ---------------------------

# Read one hourly SINCA export (date YYMMDD; hour HHMM; validated;
# preliminary; not validated; decimal comma). `status` is the status of the
# record. `value` is kept only for the statuses listed in `accepted_status`
# (any of "validated", "preliminary", "not_validated"); when a record has
# values under several statuses the validated one comes first.
read_sinca_hourly <- function(file, accepted_status) {
  statuses <- c("validated", "preliminary", "not_validated")
  stopifnot(length(accepted_status) >= 1, all(accepted_status %in% statuses))
  h <- data.table::fread(file, sep = ";", header = FALSE, skip = 1,
                         colClasses = "character", fill = TRUE)
  stopifnot(ncol(h) >= 5)
  to_number <- function(x) as.numeric(sub(",", ".", x, fixed = TRUE))
  values <- lapply(3:5, function(j) to_number(h[[j]]))
  names(values) <- statuses
  status <- data.table::fifelse(!is.na(values$validated), "validated",
            data.table::fifelse(!is.na(values$preliminary), "preliminary",
            data.table::fifelse(!is.na(values$not_validated), "not_validated", NA_character_)))
  for (s in setdiff(statuses, accepted_status)) values[[s]][] <- NA_real_
  out <- data.table::data.table(
    date = as.Date(h[[1]], format = "%y%m%d"),
    hour = as.integer(substr(h[[2]], 1, 2)),
    value = data.table::fcoalesce(values$validated, values$preliminary, values$not_validated),
    status = status
  )
  if (anyNA(out$date) || anyNA(out$hour) || anyDuplicated(out[, .(date, hour)]) > 0) {
    stop("Invalid or repeated timestamps in ", basename(file), call. = FALSE)
  }
  out[]
}

# Daily summaries from an hourly series of ONE station:
#   mean24: mean of the day, when it has at least `min_hours` valid hours
#   mda8:   maximum of the 8-hour running means ending on the hours of the
#           day; a running mean needs `mda8_min_hours` valid hours and the day
#           needs `mda8_min_windows` valid running means
daily_from_hourly <- function(h, min_hours, mda8_min_hours, mda8_min_windows) {
  full <- data.table::CJ(date = seq(min(h$date), max(h$date), by = "day"), hour = 0:23)
  h <- h[full, on = .(date, hour)]
  data.table::setorder(h, date, hour)
  valid <- !is.na(h$value)
  n8 <- data.table::frollsum(valid, 8, align = "right")
  s8 <- data.table::frollsum(data.table::fifelse(valid, h$value, 0), 8, align = "right")
  h[, mean8 := data.table::fifelse(n8 >= mda8_min_hours, s8 / n8, NA_real_)]
  h[, .(
    n_hours = sum(!is.na(value)),
    mean24 = if (sum(!is.na(value)) >= min_hours) mean(value, na.rm = TRUE) else NA_real_,
    mda8 = if (sum(!is.na(mean8)) >= mda8_min_windows) max(mean8, na.rm = TRUE) else NA_real_
  ), by = date]
}

# ---- Spatial interpolation (the data author's model) ------------------------

# Interpolate one day of station values to target points with inverse distance
# weighting and ordinary kriging, using the same calls as
# pollution-interpolation-kriging-idw/Code/1_interpolation.R.
#   stations, targets: sf points in a projected CRS (metres)
#   value: name of the column of `stations` to interpolate (no missing values)
#   params: list with idw_power, idw_nmax, idw_maxdist_m
# Returns one row per target with idw, ok and ok_var (kriging variance). With
# very few stations the variogram cannot always be fitted: the kriging columns
# are then missing and ok_error holds the message, so that the day is counted
# and never mistaken for a result.
interpolate_day <- function(stations, targets, value, params) {
  stopifnot(!anyNA(stations[[value]]), sf::st_crs(stations) == sf::st_crs(targets))
  model_formula <- stats::as.formula(paste(value, "~ 1"))
  idw_model <- gstat::gstat(
    formula = model_formula, locations = stations,
    nmax = as.numeric(params$idw_nmax), maxdist = as.numeric(params$idw_maxdist_m),
    set = list(idp = as.numeric(params$idw_power))
  )
  idw_pred <- predict(idw_model, targets, debug.level = 0)
  kriging <- tryCatch({
    variogram_fit <- automap::autofitVariogram(
      model_formula, methods::as(stations, "Spatial"), verbose = FALSE
    )
    ok_pred <- gstat::krige(
      model_formula, locations = stations, newdata = targets,
      model = variogram_fit$var_model, debug.level = 0
    )
    list(pred = ok_pred$var1.pred, var = ok_pred$var1.var, error = NA_character_)
  }, error = function(e) {
    list(pred = rep(NA_real_, nrow(targets)), var = rep(NA_real_, nrow(targets)),
         error = conditionMessage(e))
  })
  data.table::data.table(
    idw = idw_pred$var1.pred, ok = kriging$pred, ok_var = kriging$var, ok_error = kriging$error
  )
}

# Agreement between observed and predicted values. `r2` is the coefficient of
# determination against the 1:1 line (it can be negative), not the squared
# correlation.
validation_metrics <- function(observed, predicted) {
  use <- !is.na(observed) & !is.na(predicted)
  o <- observed[use]
  p <- predicted[use]
  error <- p - o
  data.table::data.table(
    n = length(o),
    mean_observed = mean(o),
    bias = mean(error),
    mae = mean(abs(error)),
    rmse = sqrt(mean(error^2)),
    nrmse_pct = 100 * sqrt(mean(error^2)) / mean(o),
    pearson_r = stats::cor(o, p),
    r2 = 1 - sum(error^2) / sum((o - mean(o))^2)
  )
}

# Agreement between two 0/1 classifications (reference first).
classification_agreement <- function(reference, test) {
  use <- !is.na(reference) & !is.na(test)
  r <- reference[use]
  t <- test[use]
  n <- length(r)
  observed <- mean(r == t)
  expected <- mean(r) * mean(t) + (1 - mean(r)) * (1 - mean(t))
  data.table::data.table(
    n = n,
    sensitivity = sum(r == 1 & t == 1) / sum(r == 1),
    specificity = sum(r == 0 & t == 0) / sum(r == 0),
    agreement = observed,
    kappa = (observed - expected) / (1 - expected)
  )
}

# ---- Causes of death (ICD-10) -----------------------------------------------

# Numeric key of a three-character ICD-10 category ("I21" -> 821).
icd10_key <- function(code3) {
  stopifnot(all(grepl("^[A-Z][0-9]{2}$", code3)))
  (match(substr(code3, 1, 1), LETTERS) - 1L) * 100L + as.integer(substr(code3, 2, 3))
}

# TRUE when the category falls in any of the ranges, written "I20-I25".
icd10_in_ranges <- function(code3, ranges) {
  key <- icd10_key(code3)
  hit <- rep(FALSE, length(code3))
  for (range in ranges) {
    bounds <- strsplit(range, "-", fixed = TRUE)[[1]]
    stopifnot(length(bounds) == 2)
    hit <- hit | (key >= icd10_key(bounds[1]) & key <= icd10_key(bounds[2]))
  }
  hit
}

# ---- Case-crossover design --------------------------------------------------

# Time-stratified referent selection. For each death (id, com, date) the
# referent days are all days of the same calendar year, month and day of the
# week (three or four control days besides the day of death). Returns one row
# per death and referent day: id, com, case_date, date, case (1 = day of death).
build_case_crossover <- function(deaths) {
  stopifnot(all(c("id", "com", "date") %in% names(deaths)),
            anyDuplicated(deaths$id) == 0, !anyNA(deaths$date))
  d <- deaths[, .(id, com, case_date = date)]
  d[, `:=`(yr = data.table::year(case_date), mo = data.table::month(case_date),
           wd = data.table::wday(case_date))]
  calendar <- data.table::data.table(
    date = seq(min(d$case_date) - 31, max(d$case_date) + 31, by = "day")
  )
  calendar[, `:=`(yr = data.table::year(date), mo = data.table::month(date),
                  wd = data.table::wday(date))]
  cc <- calendar[d, on = .(yr, mo, wd), allow.cartesian = TRUE]
  cc[, case := as.integer(date == case_date)]
  per_id <- cc[, .(n = .N, cases = sum(case)), by = id]
  stopifnot(all(per_id$cases == 1), all(per_id$n %in% 4:5))
  data.table::setorder(cc, id, date)
  cc[, .(id, com, case_date, date, case)]
}

# Exposure of `lag` days before each date, for the listed variables.
# Returns com, date and one column per variable named <variable>_lag<k>.
lagged_exposure <- function(exposure, variables, lags) {
  stopifnot(anyDuplicated(exposure[, .(com, date)]) == 0)
  out <- exposure[, .(com, date)]
  for (k in lags) {
    shifted <- exposure[, c("com", "date", variables), with = FALSE]
    shifted[, date := date + k]
    data.table::setnames(shifted, variables, paste0(variables, "_lag", k))
    out <- merge(out, shifted, by = c("com", "date"), all.x = TRUE)
  }
  out[]
}

# ---- Conditional logistic regression ----------------------------------------

# Collapse an individual case-crossover dataset to one row per municipality,
# day of death and referent day, with the number of deaths as weight. Deaths
# of the same municipality and day share their referent days and exposures, so
# the weighted fit equals the fit on the individual records.
collapse_case_crossover <- function(cc) {
  out <- cc[, .(n_deaths = .N), by = .(com, case_date, date, case)]
  out[, stratum := .GRP, by = .(com, case_date)]
  out[]
}

# Conditional logistic regression for the collapsed dataset. `terms` are the
# right-hand-side terms; rows with a missing value in any of them drop the
# whole stratum, so that every stratum keeps its day of death.
fit_conditional_logit <- function(data, terms) {
  vars <- all.vars(stats::as.formula(paste("~", paste(terms, collapse = " + "))))
  complete <- stats::complete.cases(data[, vars, with = FALSE])
  keep <- data[, .(ok = all(complete[.I])), by = stratum][ok == TRUE, stratum]
  d <- data[stratum %in% keep]
  d[, time_one := 1]
  # Surv() and strata() are found through the attached survival package
  stopifnot("package:survival" %in% search())
  model_formula <- stats::as.formula(paste(
    "Surv(time_one, case) ~", paste(terms, collapse = " + "), "+ strata(stratum)"
  ))
  fit <- survival::coxph(model_formula, data = d, weights = n_deaths, ties = "breslow")
  fit$n_deaths <- d[case == 1, sum(n_deaths)]
  fit
}

# Odds ratios with Wald confidence intervals from a fitted model.
odds_ratios <- function(fit, conf_level) {
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  beta <- stats::coef(fit)
  se <- sqrt(diag(stats::vcov(fit)))
  data.table::data.table(
    term = names(beta), log_or = beta, se = se,
    or = exp(beta), or_low = exp(beta - z * se), or_high = exp(beta + z * se),
    p_value = 2 * stats::pnorm(-abs(beta / se))
  )
}

# Additive interaction between two binary exposures from the log odds ratios
# of the three exposed categories (E10, E01, E11, in that order) and their
# covariance matrix. Standard errors by the delta method; the interval of the
# synergy index is built on the log scale and is only defined when both the
# joint excess and the sum of the single excesses are positive.
#   REOI = OR11 - OR10 - OR01 + 1
#   AP   = REOI / OR11
#   S    = (OR11 - 1) / ((OR10 - 1) + (OR01 - 1))
additive_interaction <- function(log_or, covariance, conf_level) {
  stopifnot(length(log_or) == 3, all(dim(covariance) == 3))
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  or10 <- exp(log_or[[1]])
  or01 <- exp(log_or[[2]])
  or11 <- exp(log_or[[3]])
  se_of <- function(gradient) sqrt(drop(t(gradient) %*% covariance %*% gradient))

  reoi <- or11 - or10 - or01 + 1
  se_reoi <- se_of(c(-or10, -or01, or11))
  ap <- reoi / or11
  se_ap <- se_of(c(-or10 / or11, -or01 / or11, (or10 + or01 - 1) / or11))
  single_excess <- or10 + or01 - 2
  joint_excess <- or11 - 1
  synergy <- joint_excess / single_excess
  if (joint_excess > 0 && single_excess > 0) {
    se_log_s <- se_of(c(-or10 / single_excess, -or01 / single_excess, or11 / joint_excess))
    s_low <- exp(log(synergy) - z * se_log_s)
    s_high <- exp(log(synergy) + z * se_log_s)
  } else {
    s_low <- NA_real_
    s_high <- NA_real_
  }
  data.table::data.table(
    measure = c("REOI", "AP", "S"),
    estimate = c(reoi, ap, synergy),
    low = c(reoi - z * se_reoi, ap - z * se_ap, s_low),
    high = c(reoi + z * se_reoi, ap + z * se_ap, s_high),
    null_value = c(0, 0, 1)
  )
}

# ---- Housekeeping -----------------------------------------------------------

# Evaluate an expression and return its value together with the warning
# messages it raised, so that warnings are recorded instead of lost.
with_warnings <- function(expr) {
  messages <- character(0)
  value <- withCallingHandlers(expr, warning = function(w) {
    messages <<- c(messages, conditionMessage(w))
    invokeRestart("muffleWarning")
  })
  list(value = value, warnings = messages)
}

# Save the R session details next to a result, for reproducibility.
write_session_info <- function(path) {
  writeLines(utils::capture.output(utils::sessionInfo()), path)
}
