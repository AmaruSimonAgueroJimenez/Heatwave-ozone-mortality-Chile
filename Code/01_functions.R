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
  # algo = "exact" adds the eight hours of each window on their own. The
  # default carries one running sum along the whole series, and its rounding
  # error depends on the processor: 8-hour means of whole numbers, which are
  # multiples of 1/8, then stop being equal to each other on some machines.
  n8 <- data.table::frollsum(valid, 8, align = "right", algo = "exact")
  s8 <- data.table::frollsum(data.table::fifelse(valid, h$value, 0), 8, align = "right",
                             algo = "exact")
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

# Standard deviation of a kriging prediction from its variance (ppb squared).
# A variance cannot be negative, but when the kriging system of a day is close
# to singular a variance of zero comes out as a tiny number of either sign,
# depending on the machine. Values between minus `rounding` and zero count as
# zero; a value further below zero stops the script.
kriging_sd <- function(variance, rounding) {
  stopifnot(length(rounding) == 1, !is.na(rounding), rounding >= 0)
  lowest <- suppressWarnings(min(variance, na.rm = TRUE))    # Inf when all are missing
  if (lowest < -rounding) {
    stop("Kriging variance below zero by more than the rounding allowed (", rounding,
         "): ", format(lowest), call. = FALSE)
  }
  sqrt(pmax(variance, 0))
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

# ---- Model datasets ---------------------------------------------------------

# Rows of the case-crossover dataset for an outcome and a subgroup. `subgroup`
# is "all", one of the age labels, "male" or "female".
select_case_rows <- function(cc, outcome, subgroup, age_labels) {
  stopifnot(outcome %in% names(cc), subgroup %in% c("all", age_labels, "male", "female"))
  rows <- cc[[outcome]] == TRUE
  if (subgroup %in% age_labels) rows <- rows & cc$age_group == subgroup
  if (subgroup %in% c("male", "female")) rows <- rows & cc$sex == subgroup
  rows & !is.na(rows)
}

# Collapsed dataset of an outcome and a subgroup, with the position of each
# referent day in the table of lagged exposures (column exposure_row).
prepare_model_data <- function(cc, lagged, outcome, subgroup, age_labels) {
  collapsed <- collapse_case_crossover(cc[select_case_rows(cc, outcome, subgroup, age_labels)])
  collapsed[, exposure_row := lagged[collapsed, on = .(com, date), which = TRUE]]
  stopifnot(!anyNA(collapsed$exposure_row))
  collapsed
}

# Attach one heatwave definition and one ozone flag at a lag, and the three
# exposed categories of the compound exposure. Only the strata where both are
# known on every referent day are kept.
#   hw, oz   heatwave and high-ozone indicators
#   e10      heatwave only        e01   high ozone only        e11   both
attach_compound_exposure <- function(collapsed, lagged, heatwave, ozone_flag, lag) {
  d <- data.table::copy(collapsed)
  d[, hw := lagged[[paste0(heatwave, "_lag", lag)]][exposure_row]]
  d[, oz := lagged[[paste0(ozone_flag, "_lag", lag)]][exposure_row]]
  complete <- d[, .(ok = !anyNA(hw) && !anyNA(oz)), by = stratum][ok == TRUE, stratum]
  d <- d[stratum %in% complete]
  d[, `:=`(e10 = as.integer(hw == 1 & oz == 0), e01 = as.integer(hw == 0 & oz == 1),
           e11 = as.integer(hw == 1 & oz == 1))]
  d
}

# Deaths by exposure category on the day of death (lagged as attached)
category_deaths <- function(d) {
  d[case == 1, .(deaths = sum(n_deaths), deaths_e00 = sum(n_deaths[hw == 0 & oz == 0]),
                 deaths_e10 = sum(n_deaths[e10 == 1]), deaths_e01 = sum(n_deaths[e01 == 1]),
                 deaths_e11 = sum(n_deaths[e11 == 1]))]
}

# Compound model (reference E00): odds ratios of E10, E01 and E11, additive
# interaction and multiplicative interaction OR11 / (OR10 x OR01). `adjust`
# adds further terms to the model; the three category terms stay first.
compound_results <- function(d, conf_level, adjust = character(0)) {
  categories <- c("e10", "e01", "e11")
  fit <- fit_conditional_logit(d, c(categories, adjust))
  or <- odds_ratios(fit, conf_level)
  beta <- stats::coef(fit)[categories]
  covariance <- stats::vcov(fit)[categories, categories]
  interaction <- additive_interaction(beta, covariance, conf_level)
  # Multiplicative interaction on the log scale
  g <- c(-1, -1, 1)
  log_ratio <- sum(g * beta)
  se_ratio <- sqrt(drop(t(g) %*% covariance %*% g))
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  interaction <- rbind(interaction, data.table::data.table(
    measure = "multiplicative ratio", estimate = exp(log_ratio),
    low = exp(log_ratio - z * se_ratio), high = exp(log_ratio + z * se_ratio), null_value = 1
  ))
  list(or = or, interaction = interaction, fit = fit)
}

# ---- Continuous exposures and linear combinations ---------------------------

# Percentile (0 to 100) of each value within the reference distribution of
# its municipality: 100 x the share of reference values at or below it.
# `reference` has the columns com and value.
local_percentile <- function(value, com, reference) {
  stopifnot(length(value) == length(com), all(c("com", "value") %in% names(reference)),
            !anyNA(reference$value), all(unique(com) %in% reference$com))
  out <- rep(NA_real_, length(value))
  for (code in unique(com)) {
    rows <- which(com == code)
    out[rows] <- 100 * stats::ecdf(reference$value[reference$com == code])(value[rows])
  }
  out
}

# Natural cubic spline basis with explicit knots (no knot is chosen from the
# data), as a matrix with columns <prefix>1, <prefix>2, ...
spline_basis <- function(x, knots, boundary, prefix) {
  stopifnot(length(boundary) == 2, all(knots > boundary[1]), all(knots < boundary[2]))
  basis <- splines::ns(x, knots = knots, Boundary.knots = boundary)
  colnames(basis) <- paste0(prefix, seq_len(ncol(basis)))
  basis
}

# Odds ratios of linear combinations of coefficients. Each row of `weights`
# is one combination; its columns are named after the coefficients it uses.
combine_coefficients <- function(fit, weights, conf_level) {
  terms <- colnames(weights)
  stopifnot(!is.null(terms), all(terms %in% names(stats::coef(fit))))
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  beta <- stats::coef(fit)[terms]
  covariance <- stats::vcov(fit)[terms, terms, drop = FALSE]
  estimate <- drop(weights %*% beta)
  se <- sqrt(rowSums((weights %*% covariance) * weights))
  data.table::data.table(
    log_or = estimate, se = se, or = exp(estimate),
    or_low = exp(estimate - z * se), or_high = exp(estimate + z * se)
  )
}

# ---- Mortality burden -------------------------------------------------------

# Population projections of INE by municipality, year, sex and single year of
# age (long format, parquet). Returns com, year, sex, age and population for
# the listed municipalities and years. The last age, `open_age`, is an open
# group (that age or more).
read_population <- function(path, codes, years, open_age) {
  if (!requireNamespace("nanoparquet", quietly = TRUE)) {
    stop("Package 'nanoparquet' is needed to read the population file.", call. = FALSE)
  }
  raw <- data.table::as.data.table(nanoparquet::read_parquet(path))
  expected <- c("cut_region", "region", "cut_provincia", "provincia", "cut_comuna", "comuna",
                "edad", "año", "población", "género")
  stopifnot(identical(enc2utf8(names(raw)), enc2utf8(expected)))
  data.table::setnames(raw, c("cut_region", "region", "cut_provincia", "province", "com",
                              "municipality", "age", "year", "population", "sex"))
  sex_labels <- c(masculino = "male", femenino = "female")
  stopifnot(all(raw$sex %in% names(sex_labels)))
  out <- raw[com %in% codes & year >= years[1] & year <= years[2],
             .(com = as.integer(com), year = as.integer(year), sex = unname(sex_labels[sex]),
               age = as.integer(age), population)]
  stopifnot(
    setequal(unique(out$com), codes), all(years[1]:years[2] %in% out$year),
    max(out$age) == open_age, !anyNA(out$population), all(out$population >= 0),
    anyDuplicated(out[, .(com, year, sex, age)]) == 0
  )
  data.table::setorder(out, com, year, sex, age)
  out[]
}

# Deaths attributable to the exposed categories of the compound exposure.
#
# `cells` has one row per group, age group and category ("e10", "e01", "e11"):
#   expected   deaths expected on the exposed days at the baseline rate
#              (baseline deaths per day x number of exposed days)
#   observed   deaths observed on the exposed days
# `models` is a list named by age group; each element holds `beta`, the log
# odds ratios named e10, e01 and e11, and their `covariance`.
#
# For every group of `by`, each category and the three together ("any"):
#   excess        expected x (OR - 1), the formula of the analysis plan
#   attributable  observed x (OR - 1) / OR, the attributable fraction among
#                 the exposed applied to the deaths observed on exposed days
# Intervals by the delta method. Only the odds ratios carry uncertainty:
# baseline rates and death counts are taken as fixed. Models of different age
# groups are independent; the three odds ratios of one model are not.
attributable_deaths <- function(cells, models, by, conf_level) {
  categories <- c("e10", "e01", "e11")
  stopifnot(
    all(c("age_group", "category", "expected", "observed", by) %in% names(cells)),
    all(cells$category %in% categories), all(unique(cells$age_group) %in% names(models)),
    !anyNA(cells$expected), !anyNA(cells$observed)
  )
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  sums <- cells[, .(expected = sum(expected), observed = sum(observed)),
                by = c(by, "age_group", "category")]
  one_group <- function(g) {
    data.table::rbindlist(lapply(c(categories, "any"), function(target) {
      include <- as.numeric(categories %in% (if (target == "any") categories else target))
      total <- c(expected = 0, observed = 0, excess = 0, attributable = 0,
                 var_excess = 0, var_attributable = 0)
      for (age in unique(g$age_group)) {
        beta <- models[[age]]$beta[categories]
        covariance <- models[[age]]$covariance[categories, categories]
        stopifnot(!anyNA(beta), !anyNA(covariance))
        or <- exp(beta)
        rows <- g[age_group == age]
        expected <- stats::setNames(numeric(3), categories)
        observed <- expected
        expected[rows$category] <- rows$expected
        observed[rows$category] <- rows$observed
        gradient_excess <- include * expected * or          # d/d beta of expected x (OR - 1)
        gradient_attributable <- include * observed / or    # d/d beta of observed x (1 - 1/OR)
        total <- total + c(
          sum(include * expected), sum(include * observed),
          sum(include * expected * (or - 1)), sum(include * observed * (1 - 1 / or)),
          drop(t(gradient_excess) %*% covariance %*% gradient_excess),
          drop(t(gradient_attributable) %*% covariance %*% gradient_attributable)
        )
      }
      data.table::data.table(
        category = target, expected = total[["expected"]], observed = total[["observed"]],
        excess = total[["excess"]],
        excess_low = total[["excess"]] - z * sqrt(total[["var_excess"]]),
        excess_high = total[["excess"]] + z * sqrt(total[["var_excess"]]),
        attributable = total[["attributable"]],
        attributable_low = total[["attributable"]] - z * sqrt(total[["var_attributable"]]),
        attributable_high = total[["attributable"]] + z * sqrt(total[["var_attributable"]])
      )
    }))
  }
  if (length(by) == 0) return(one_group(sums))
  # One call per group, keeping every column so that `by` may include age_group
  groups <- unique(sums[, by, with = FALSE])
  data.table::rbindlist(lapply(seq_len(nrow(groups)), function(i) {
    cbind(groups[i], one_group(sums[groups[i], on = by]))
  }))
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

# ---- Software ---------------------------------------------------------------

# Path of the Quarto program: on the search path, named by the variables
# QUARTO_PATH or RSTUDIO_QUARTO (set by RStudio), or in one of the folders
# where Quarto, RStudio and Positron install it on macOS and Linux.
quarto_command <- function() {
  named <- c(Sys.getenv("QUARTO_PATH"), Sys.getenv("RSTUDIO_QUARTO"))
  named <- ifelse(dir.exists(named), file.path(named, "quarto"), named)
  candidates <- c(
    Sys.which("quarto"), named,
    "/usr/local/bin/quarto", "/opt/homebrew/bin/quarto", "/opt/quarto/bin/quarto",
    "/Applications/quarto/bin/quarto",
    "/Applications/RStudio.app/Contents/Resources/app/quarto/bin/quarto",
    "/Applications/Positron.app/Contents/Resources/app/quarto/bin/quarto"
  )
  found <- candidates[nzchar(candidates) & file.exists(candidates) & !dir.exists(candidates)]
  if (length(found) == 0) {
    stop("Quarto was not found. Install it from https://quarto.org, or give the path of ",
         "its program in the environment variable QUARTO_PATH.", call. = FALSE)
  }
  unname(found[1])
}

# TRUE when the graphics device `device` (a function whose first argument is
# the file) opens. The test is to open it: R can report that cairo is available
# and still fail to load it, and then cairo_pdf() only warns and draws nothing.
device_opens <- function(device) {
  file <- tempfile(fileext = ".pdf")
  on.exit(unlink(file))
  before <- grDevices::dev.list()
  suppressWarnings(try(device(file), silent = TRUE))
  opened <- setdiff(grDevices::dev.list(), before)
  for (d in opened) grDevices::dev.off(d)
  length(opened) == 1
}

# The PDF device of the publication figures, as ggsave() takes it, with its
# name. cairo_pdf embeds the fonts; on macOS it needs the libraries of XQuartz,
# and where it does not open the standard pdf device of R is used instead.
pdf_figure_device <- function() {
  if (device_opens(grDevices::cairo_pdf)) {
    list(name = "cairo_pdf", device = grDevices::cairo_pdf)
  } else {
    list(name = "pdf", device = "pdf")
  }
}

# ---- Files of the folder inputs ---------------------------------------------

# Hourly ozone file of each station of config/stations.csv, relative to
# inputs$sinca_hourly_dir.
sinca_hourly_files <- function() {
  rules <- get_param("station_series")
  stations <- data.table::fread(here::here(rules$stations_file), encoding = "UTF-8")
  sprintf(rules$hourly_file_pattern, stations$sinca_id)
}

# Monthly CAMS files that hold every 3-hourly record of the local days of the
# study years, relative to inputs$cams_eac4_dir. Local time is UTC plus
# utc_offset_hours, so a local day ends inside the next UTC day and the last
# study day needs the first record of the month after it.
cams_month_files <- function() {
  rules <- get_param("reanalysis")
  days <- study_dates()
  first_utc <- as.POSIXct(format(min(days)), tz = "UTC") - rules$utc_offset_hours * 3600
  last_utc <- as.POSIXct(format(max(days) + 1), tz = "UTC") - rules$utc_offset_hours * 3600 - 1
  months <- seq(as.Date(format(first_utc, "%Y-%m-01", tz = "UTC")),
                as.Date(format(last_utc, "%Y-%m-01", tz = "UTC")), by = "month")
  sprintf(rules$file_pattern, format(months, "%Y%m"))
}

# ---- Parallel runs ----------------------------------------------------------

# Number of worker processes: the R option mc.cores when it is set (for
# example options(mc.cores = 2), or the environment variable MC_CORES before R
# starts), otherwise every core up to eight, so that a machine with many
# cores does not run short of memory. Windows cannot fork R processes, so
# there the jobs run one after the other.
n_workers <- function() {
  if (.Platform$OS.type == "windows") return(1L)
  loadNamespace("parallel")                    # reads MC_CORES into the option
  workers <- getOption("mc.cores", min(parallel::detectCores(), 8L))
  if (length(workers) != 1 || is.na(workers) || workers < 1) {
    stop("The number of cores could not be determined: set options(mc.cores = ...).",
         call. = FALSE)
  }
  as.integer(workers)
}

# lapply over `jobs` in parallel worker processes; `what` names the task in the
# messages. The results do not depend on the number of workers. Left alone,
# mclapply only warns when a worker process dies (for lack of memory, for
# example) and puts NULL in the place of its results, which would then be
# missing from the tables without notice. Here the jobs of a dead worker are
# run again, one by one, in the main process, and a job that fails stops the
# run with its own error message.
run_parallel <- function(jobs, fun, what) {
  # Errors are caught inside the worker so that the message names the job
  guarded <- function(job) {
    tryCatch(list(value = fun(job)), error = function(e) list(error = conditionMessage(e)))
  }
  answered <- function(x) is.list(x) && any(c("value", "error") %in% names(x))
  results <- suppressWarnings(parallel::mclapply(jobs, guarded, mc.cores = n_workers()))
  stopifnot(length(results) == length(jobs))
  lost <- which(!vapply(results, answered, logical(1)))
  if (length(lost) > 0) {
    message(what, ": ", length(lost), " of ", length(jobs), " jobs came back without a result ",
            "(their worker process died); running them again one by one.")
    results[lost] <- lapply(jobs[lost], guarded)
  }
  failed <- which(!vapply(results, function(x) "value" %in% names(x), logical(1)))
  if (length(failed) > 0) {
    first <- failed[1]
    job <- jobs[[first]]
    label <- if (is.atomic(job) && length(job) == 1) paste0(" (", format(job), ")") else ""
    stop(what, " failed on job ", first, " of ", length(jobs), label, ": ",
         results[[first]]$error, call. = FALSE)
  }
  lapply(results, `[[`, "value")
}

# ---- Reproduction check -----------------------------------------------------

# Compare the tables (.csv) of two folders, file by file. `status` is
#   identical         same bytes, or the same values in every cell (files
#                     written on Windows and on other systems end their lines
#                     differently)
#   within tolerance  same rows, columns and text; numbers differ by at most
#                     `tolerance`, relative to the larger of the two values
#                     (absolute for values below 1)
#   different         anything else
#   only in reference, only in new
# Numbers computed on another machine or with other package versions can
# differ in their last digits; `max_difference` is the largest difference found.
compare_tables <- function(reference_dir, new_dir, tolerance) {
  stopifnot(dir.exists(reference_dir), dir.exists(new_dir), tolerance >= 0)
  files <- sort(union(list.files(reference_dir, pattern = "\\.csv$"),
                      list.files(new_dir, pattern = "\\.csv$")))
  read <- function(path) data.table::fread(path, encoding = "UTF-8", integer64 = "character")
  # A column of numbers comes back as text when one of them is a whole number
  # too large for an integer (written without decimal point or exponent, as in
  # 6129602301290210000). Such a column is compared as numbers. A column of
  # whole numbers only (an identifier) stays as text and has to match exactly.
  numbers_read_as_text <- function(x, y) {
    if (!is.character(x) || !is.character(y)) return(FALSE)
    values <- c(x, y)
    values <- values[!is.na(values) & values != ""]
    length(values) > 0 && !anyNA(suppressWarnings(as.numeric(values))) &&
      !all(grepl("^[0-9]+$", values))
  }
  compare_one <- function(file) {
    reference <- file.path(reference_dir, file)
    new <- file.path(new_dir, file)
    if (!file.exists(new)) return(list(status = "only in reference", max_difference = NA_real_))
    if (!file.exists(reference)) return(list(status = "only in new", max_difference = NA_real_))
    if (tools::md5sum(reference) == tools::md5sum(new)) {
      return(list(status = "identical", max_difference = 0))
    }
    a <- read(reference)
    b <- read(new)
    if (!identical(names(a), names(b)) || nrow(a) != nrow(b)) {
      return(list(status = "different", max_difference = NA_real_))
    }
    worst <- 0
    for (column in names(a)) {
      x <- a[[column]]
      y <- b[[column]]
      if (numbers_read_as_text(x, y)) {
        x <- as.numeric(x)                                   # an empty cell becomes NA
        y <- as.numeric(y)
      }
      if (is.numeric(x) && is.numeric(y)) {
        if (!identical(is.na(x), is.na(y))) return(list(status = "different", max_difference = NA_real_))
        difference <- abs(x - y) / pmax(1, abs(x), abs(y))
        difference[!is.na(x) & x == y] <- 0                  # also equal infinite values
        difference[!is.na(x) & is.nan(difference)] <- Inf    # infinite values of opposite sign
        worst <- max(worst, difference, na.rm = TRUE)
      } else if (!identical(as.character(x), as.character(y))) {
        return(list(status = "different", max_difference = NA_real_))
      }
    }
    status <- if (worst == 0) "identical" else if (worst <= tolerance) "within tolerance" else "different"
    list(status = status, max_difference = worst)
  }
  data.table::rbindlist(lapply(files, function(file) c(list(file = file), compare_one(file))))
}
