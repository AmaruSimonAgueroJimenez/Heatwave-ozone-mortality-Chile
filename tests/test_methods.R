# test_methods.R
# Checks of the shared functions against independent calculations.
# Run from the project root:  Rscript tests/test_methods.R
# Every check stops with an error when it fails.

suppressPackageStartupMessages(library(survival))
source(here::here("Code", "00_setup.R"))
source(here::here("Code", "01_functions.R"))
set.seed(20261123)

check <- function(label, condition) {
  if (!isTRUE(condition)) stop("FAILED: ", label, call. = FALSE)
  cat("ok  ", label, "\n")
}

# ---- 1. Consecutive-day flag against run lengths ----------------------------
exceed <- rbinom(5000, 1, 0.3)
runs <- rle(exceed)
position_in_run <- unlist(lapply(runs$lengths, seq_len))
for (n in 2:4) {
  expected <- as.integer(exceed == 1 & position_in_run >= n)
  got <- consecutive_flag(exceed, n)
  check(sprintf("consecutive_flag, n = %d, equals the run-length definition", n),
        all(got[n:5000] == expected[n:5000]) && all(is.na(got[seq_len(n - 1)])))
}
with_gap <- c(1, 1, NA, 1, 1, 1)
check("consecutive_flag returns NA when the window holds a missing day",
      identical(consecutive_flag(with_gap, 2), c(NA, 1L, NA, NA, 1L, 1L)))

# ---- 2. Excess heat factor on a hand-made series ----------------------------
tmax <- c(rep(30, 40), 36, 37, 38)
tmin <- c(rep(14, 40), 18, 19, 20)
ehf <- excess_heat_factor(tmax, tmin, is_reference = rep(TRUE, 43), n_days = 3,
                          accl_days = 30, prob = 0.5)
# TAD uses the minimum of the next day: it is 22 on days 1 to 39, (30 + 18) / 2
# = 24 on day 40, (36 + 19) / 2 = 27.5 on day 41 and (37 + 20) / 2 = 28.5 on
# day 42. On day 42 the 3-day mean is (24 + 27.5 + 28.5) / 3, the reference
# (median) is 22 and the 30-day mean is (27 * 22 + 24 + 27.5 + 28.5) / 30.
mean_3d <- (24 + 27.5 + 28.5) / 3
expected_42 <- (mean_3d - 22) * max(1, mean_3d - (27 * 22 + 24 + 27.5 + 28.5) / 30)
check("excess_heat_factor matches the hand calculation",
      isTRUE(all.equal(ehf$ehf[42], expected_42)) && isTRUE(all.equal(ehf$tad[41], 27.5)))

# ---- 3. Daily ozone summaries -----------------------------------------------
# Day 1 is a flat day (the running means of the first hours of a series are
# incomplete), day 2 has eight hours at 50 ppb, day 3 has 10 valid hours.
hourly <- data.table(date = rep(as.Date("2015-01-01") + 0:2, each = 24), hour = rep(0:23, 3),
                     value = c(rep(10, 24), rep(10, 10), rep(50, 8), rep(10, 6),
                               rep(NA, 14), rep(20, 10)))
daily <- daily_from_hourly(hourly, min_hours = 12, mda8_min_hours = 6, mda8_min_windows = 18)
check("daily_from_hourly: 24-hour mean and MDA8 of a known day",
      isTRUE(all.equal(daily$mean24[2], (16 * 10 + 8 * 50) / 24)) &&
      isTRUE(all.equal(daily$mda8[2], 50)))
check("daily_from_hourly: a day with 10 valid hours has no 24-hour mean",
      is.na(daily$mean24[3]) && daily$n_hours[3] == 10)

# ---- 4. ICD-10 ranges -------------------------------------------------------
codes <- c("A00", "R99", "S00", "U07", "I21", "I20", "I26", "J44", "J45")
check("icd10_in_ranges: non-accidental A00-R99",
      identical(icd10_in_ranges(codes, "A00-R99"), c(TRUE, TRUE, FALSE, FALSE, rep(TRUE, 5))))
check("icd10_in_ranges: several ranges",
      identical(icd10_in_ranges(codes, c("I20-I25", "J41-J44")),
                c(FALSE, FALSE, FALSE, FALSE, TRUE, TRUE, FALSE, TRUE, FALSE)))

# ---- 5. Referent days and lagged exposures ----------------------------------
toy_deaths <- data.table(id = 1:3, com = c(1L, 1L, 2L),
                         date = as.Date(c("2015-01-01", "2015-01-29", "2016-02-29")))
toy_cc <- build_case_crossover(toy_deaths)
same_stratum <- toy_cc[, all(year(date) == year(case_date) & month(date) == month(case_date) &
                             wday(date) == wday(case_date))]
check("build_case_crossover: referents share year, month and weekday with the day of death",
      same_stratum && identical(toy_cc[, .N, by = id]$N, c(5L, 5L, 5L)))
check("build_case_crossover: the Thursdays of January 2015",
      identical(toy_cc[id == 1, date], as.Date("2015-01-01") + c(0, 7, 14, 21, 28)))
toy_exposure <- CJ(com = 1:2, date = as.Date("2015-01-01") + 0:9)
toy_exposure[, x := as.numeric(seq_len(.N)) + 100 * com]
toy_lagged <- lagged_exposure(toy_exposure, "x", 0:3)
check("lagged_exposure: lag k is the value k days earlier in the same municipality",
      toy_lagged[com == 2 & date == as.Date("2015-01-05"), x_lag2] ==
        toy_exposure[com == 2 & date == as.Date("2015-01-03"), x] &&
      is.na(toy_lagged[com == 2 & date == as.Date("2015-01-02"), x_lag3]) &&
      isTRUE(all.equal(toy_lagged$x_lag0, toy_exposure$x)))

# ---- 6. Additive interaction against msm::deltamethod -----------------------
b <- c(e10 = log(1.08), e01 = log(1.03), e11 = log(1.20))
V <- matrix(c(4, 1, 1.5, 1, 3, 1.2, 1.5, 1.2, 6), 3) * 1e-4
ours <- additive_interaction(b, V, conf_level = 0.95)
se_reoi <- msm::deltamethod(~ exp(x3) - exp(x1) - exp(x2) + 1, b, V)
se_ap <- msm::deltamethod(~ (exp(x3) - exp(x1) - exp(x2) + 1) / exp(x3), b, V)
se_log_s <- msm::deltamethod(~ log((exp(x3) - 1) / (exp(x1) + exp(x2) - 2)), b, V)
z <- qnorm(0.975)
check("additive_interaction: REOI = OR11 - OR10 - OR01 + 1",
      isTRUE(all.equal(ours$estimate[1], 1.20 - 1.08 - 1.03 + 1)))
check("additive_interaction: delta-method intervals equal msm::deltamethod",
      isTRUE(all.equal(ours$high[1] - ours$estimate[1], z * se_reoi)) &&
      isTRUE(all.equal(ours$high[2] - ours$estimate[2], z * se_ap)) &&
      isTRUE(all.equal(log(ours$high[3]) - log(ours$estimate[3]), z * se_log_s)))

# ---- 7. Case-crossover pipeline on simulated deaths with known effects ------
# Two binary exposures on a municipality-day grid, a seasonal baseline that the
# design must remove, and true odds ratios OR10 = 1.10, OR01 = 1.05, OR11 = 1.30.
grid <- CJ(com = 1:20, date = seq(as.Date("2011-11-01"), as.Date("2014-03-31"), by = "day"))
grid <- grid[month(date) %in% c(11, 12, 1, 2, 3)]
grid[, hw := rbinom(.N, 1, 0.15)]
grid[, oz := rbinom(.N, 1, plogis(-1 + 1.2 * hw))]          # correlated exposures
truth <- c(e10 = log(1.10), e01 = log(1.05), e11 = log(1.30))
grid[, baseline := 6 * exp(0.3 * cos(2 * pi * yday(date) / 365) + 0.1 * com / 20)]
grid[, rate := baseline * exp(truth[["e10"]] * (hw == 1 & oz == 0) +
                              truth[["e01"]] * (hw == 0 & oz == 1) +
                              truth[["e11"]] * (hw == 1 & oz == 1))]
grid[, n := rpois(.N, rate)]
deaths <- grid[rep(seq_len(.N), n), .(com, date)]
deaths[, id := .I]
cc <- build_case_crossover(deaths)
cc <- merge(cc, grid[, .(com, date, hw, oz)], by = c("com", "date"))
cc[, `:=`(e10 = as.integer(hw == 1 & oz == 0), e01 = as.integer(hw == 0 & oz == 1),
          e11 = as.integer(hw == 1 & oz == 1))]

individual <- clogit(case ~ e10 + e01 + e11 + strata(id), data = cc, method = "exact")
collapsed <- collapse_case_crossover(cc)
collapsed <- merge(collapsed, grid[, .(com, date, hw, oz)], by = c("com", "date"))
collapsed[, `:=`(e10 = as.integer(hw == 1 & oz == 0), e01 = as.integer(hw == 0 & oz == 1),
                 e11 = as.integer(hw == 1 & oz == 1))]
weighted <- fit_conditional_logit(collapsed, c("e10", "e01", "e11"))
check("collapsed weighted fit equals the individual conditional logistic fit (coefficients)",
      isTRUE(all.equal(unname(coef(weighted)), unname(coef(individual)), tolerance = 1e-6)))
check("collapsed weighted fit equals the individual fit (standard errors)",
      isTRUE(all.equal(unname(sqrt(diag(vcov(weighted)))), unname(sqrt(diag(vcov(individual)))),
                       tolerance = 1e-5)))
check("number of deaths carried by the collapsed fit", weighted$n_deaths == nrow(deaths))
z_scores <- (coef(weighted) - truth) / sqrt(diag(vcov(weighted)))
cat("     simulated deaths:", nrow(deaths), " estimates:",
    paste(sprintf("%.3f", exp(coef(weighted))), collapse = ", "),
    " truth: 1.100, 1.050, 1.300  z:", paste(sprintf("%.2f", z_scores), collapse = ", "), "\n")
check("estimates recover the true odds ratios (within 3 standard errors)",
      all(abs(z_scores) < 3))
interaction <- additive_interaction(coef(weighted), vcov(weighted), conf_level = 0.95)
true_reoi <- 1.30 - 1.10 - 1.05 + 1
check("the REOI interval covers the true value",
      interaction$low[1] <= true_reoi && true_reoi <= interaction$high[1])

cat("\nAll checks passed.\n")
