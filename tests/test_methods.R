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
# The days are compared as calendar days: R stores the dates of a daily sequence
# as whole numbers from version 4.5.0 and as doubles before, so identical() on
# the Date vectors would depend on the R version.
thursdays <- toy_cc[id == 1, date]
check("build_case_crossover: the Thursdays of January 2015",
      inherits(thursdays, "Date") &&
        identical(format(thursdays, "%Y-%m-%d"),
                  c("2015-01-01", "2015-01-08", "2015-01-15", "2015-01-22", "2015-01-29")))
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

# ---- 8. Compound model with further terms ------------------------------------
direct <- additive_interaction(coef(weighted), vcov(weighted), conf_level = 0.95)
shared <- compound_results(collapsed, conf_level = 0.95)
check("compound_results without adjustment equals the direct calculation",
      isTRUE(all.equal(shared$interaction[measure != "multiplicative ratio"], direct)) &&
      isTRUE(all.equal(unname(coef(shared$fit)), unname(coef(weighted)))))
collapsed[, noise := rnorm(.N)]
adjusted <- compound_results(collapsed, conf_level = 0.95, adjust = "noise")
check("compound_results keeps the three categories first when a covariate is added",
      identical(adjusted$or$term, c("e10", "e01", "e11", "noise")) &&
      max(abs(coef(adjusted$fit)[1:3] - coef(weighted))) < 0.01)

# ---- 9. Attributable deaths ---------------------------------------------------
# Two age groups with their own odds ratios, two municipalities.
models <- list(
  young = list(beta = c(e10 = log(1.10), e01 = log(1.02), e11 = log(1.25)),
               covariance = matrix(c(9, 2, 3, 2, 4, 2, 3, 2, 16), 3,
                                   dimnames = list(c("e10", "e01", "e11"),
                                                   c("e10", "e01", "e11"))) * 1e-4),
  old = list(beta = c(e10 = log(1.20), e01 = log(1.05), e11 = log(1.15)),
             covariance = matrix(c(16, 3, 4, 3, 6, 3, 4, 3, 25), 3,
                                 dimnames = list(c("e10", "e01", "e11"),
                                                 c("e10", "e01", "e11"))) * 1e-4)
)
cells <- CJ(com = 1:2, age_group = c("young", "old"), category = c("e10", "e01", "e11"))
cells[, expected := c(40, 300, 55, 90, 700, 130, 20, 150, 30, 60, 400, 80)]
cells[, observed := round(expected * 1.1)]
total <- attributable_deaths(cells, models, by = character(0), conf_level = 0.95)
by_com <- attributable_deaths(cells, models, by = "com", conf_level = 0.95)
by_age <- attributable_deaths(cells, models, by = "age_group", conf_level = 0.95)
or_of <- function(age, category) exp(models[[age]]$beta[[category]])
hand_excess <- cells[, sum(expected * (mapply(or_of, age_group, category) - 1))]
hand_attributable <- cells[, sum(observed * (1 - 1 / mapply(or_of, age_group, category)))]
check("attributable_deaths: excess = expected x (OR - 1), summed over cells",
      isTRUE(all.equal(total[category == "any", excess], hand_excess)))
check("attributable_deaths: attributable = observed x (OR - 1) / OR",
      isTRUE(all.equal(total[category == "any", attributable], hand_attributable)))
check("attributable_deaths: categories add up to 'any', groups add up to the total",
      isTRUE(all.equal(total[category != "any", sum(excess)], total[category == "any", excess])) &&
      isTRUE(all.equal(by_com[category == "any", sum(excess)], total[category == "any", excess])) &&
      isTRUE(all.equal(by_age[category == "any", sum(excess)], total[category == "any", excess])))
# Delta-method interval against simulation of the odds ratios
draws <- 200000
simulated <- Reduce(`+`, lapply(names(models), function(age) {
  beta <- mvtnorm::rmvnorm(draws, models[[age]]$beta, models[[age]]$covariance)
  expected <- cells[age_group == age, .(expected = sum(expected)), by = category][
    match(c("e10", "e01", "e11"), category), expected]
  drop((exp(beta) - 1) %*% expected)
}))
delta_se <- total[category == "any", (excess_high - excess_low) / (2 * qnorm(0.975))]
cat("     excess deaths:", round(hand_excess, 1), " delta-method SE:", round(delta_se, 2),
    " simulated SE:", round(sd(simulated), 2), "\n")
check("attributable_deaths: delta-method standard error within 2% of the simulated one",
      abs(delta_se / sd(simulated) - 1) < 0.02)
# The formula of the analysis plan gives the same number: the population cancels
population <- 250000
age_share <- 0.12
reference_days <- 900
reference_deaths <- 2700
exposure_days <- 140
baseline_rate <- reference_deaths / reference_days / (population * age_share)
plan <- population * age_share * baseline_rate * (1.25 - 1) * exposure_days
counts <- reference_deaths / reference_days * exposure_days * (1.25 - 1)
check("burden: Population x Age share x Baseline rate x (OR - 1) x Days does not depend on the population",
      isTRUE(all.equal(plan, counts)))

# ---- 10. Local percentiles and linear combinations ---------------------------
reference <- data.table(com = rep(1:2, each = 200), value = c(rnorm(200, 28, 3), rnorm(200, 18, 4)))
x <- c(27, 31, 15, 22)
com <- c(1L, 1L, 2L, 2L)
by_hand <- c(mean(reference[com == 1, value] <= 27), mean(reference[com == 1, value] <= 31),
             mean(reference[com == 2, value] <= 15), mean(reference[com == 2, value] <= 22)) * 100
check("local_percentile: share of the reference values of the same municipality at or below",
      isTRUE(all.equal(local_percentile(x, com, reference), by_hand)))
weights <- matrix(c(1, 1, 0, 0, 0, 1), 2, byrow = TRUE,
                  dimnames = list(NULL, c("e10", "e01", "e11")))
combined <- combine_coefficients(weighted, weights, conf_level = 0.95)
se_sum <- msm::deltamethod(~ x1 + x2, coef(weighted), vcov(weighted))
check("combine_coefficients: sum of two coefficients and its standard error",
      isTRUE(all.equal(combined$log_or[1], sum(coef(weighted)[c("e10", "e01")]))) &&
      isTRUE(all.equal(combined$se[1], se_sum)) &&
      isTRUE(all.equal(combined$or[2], unname(exp(coef(weighted)["e11"])))))

# ---- 11. Distributed lags: effects at lags 0 and 1 only ----------------------
# A persistent exposure (as a heatwave) with true odds ratios 1.15 at lag 0 and
# 1.08 at lag 1, none at lags 2 and 3. Single-lag models attribute part of it
# to the later lags; the model with all the lags does not.
grid2 <- CJ(com = 1:30, date = seq(as.Date("2012-11-01"), as.Date("2015-03-31"), by = "day"))
grid2[, x := as.integer(stats::filter(rnorm(.N), rep(1, 3), sides = 1) > 1.2), by = com]
grid2[is.na(x), x := 0L]
for (k in 0:3) grid2[, (paste0("x_lag", k)) := shift(x, k, fill = 0L), by = com]
grid2 <- grid2[month(date) %in% c(11, 12, 1, 2, 3)]
true_lag <- c(log(1.15), log(1.08), 0, 0)
grid2[, n := rpois(.N, 8 * exp(true_lag[1] * x_lag0 + true_lag[2] * x_lag1))]
deaths2 <- grid2[rep(seq_len(.N), n), .(com, date)]
deaths2[, id := .I]
cc2 <- collapse_case_crossover(build_case_crossover(deaths2))
cc2 <- merge(cc2, grid2[, c("com", "date", paste0("x_lag", 0:3)), with = FALSE],
             by = c("com", "date"))
together <- fit_conditional_logit(cc2, paste0("x_lag", 0:3))
single_lag2 <- fit_conditional_logit(cc2, "x_lag2")
z_together <- (coef(together) - true_lag) / sqrt(diag(vcov(together)))
cat("     all lags together:", paste(sprintf("%.3f", exp(coef(together))), collapse = ", "),
    " truth: 1.150, 1.080, 1.000, 1.000;  lag 2 alone:",
    sprintf("%.3f", exp(coef(single_lag2))), "\n")
check("distributed lags: the model with all the lags recovers the true lag structure",
      all(abs(z_together) < 3))
check("distributed lags: the single-lag model at lag 2 picks up the earlier lags",
      coef(single_lag2) / sqrt(vcov(single_lag2)[1, 1]) > 2)
cumulative <- combine_coefficients(together, matrix(1, 1, 4, dimnames = list(NULL, paste0("x_lag", 0:3))),
                                   conf_level = 0.95)
check("distributed lags: the cumulative interval covers the true cumulative odds ratio",
      cumulative$or_low <= 1.15 * 1.08 && 1.15 * 1.08 <= cumulative$or_high)

# ---- 12. Cross-basis of dlnm against a spline built by hand -------------------
# Temperature percentile with a non-linear effect at lags 0 and 1. The model
# fitted on the cross-basis of dlnm and the model fitted on one spline basis
# per lag span the same space: their cumulative odds ratios coincide.
suppressPackageStartupMessages(library(dlnm))
grid3 <- CJ(com = 1:25, date = seq(as.Date("2012-11-01"), as.Date("2015-03-31"), by = "day"))
grid3[, p := 100 * pnorm(as.numeric(arima.sim(list(ar = 0.6), .N)) * sqrt(1 - 0.36)), by = com]
for (k in 0:3) grid3[, (paste0("p_lag", k)) := shift(p, k), by = com]
grid3 <- grid3[month(date) %in% c(11, 12, 1, 2, 3) & !is.na(p_lag3)]
effect <- function(p) 0.004 * pmax(p - 50, 0)
grid3[, n := rpois(.N, 6 * exp(effect(p_lag0) + 0.5 * effect(p_lag1)))]
deaths3 <- grid3[rep(seq_len(.N), n), .(com, date)]
deaths3[, id := .I]
cc3 <- collapse_case_crossover(build_case_crossover(deaths3))
cc3 <- merge(cc3, grid3[, c("com", "date", paste0("p_lag", 0:3)), with = FALSE],
             by = c("com", "date"))
knots <- c(50, 90)
boundary <- c(0, 100)
history <- as.matrix(cc3[, paste0("p_lag", 0:3), with = FALSE])
cross_basis <- crossbasis(history, lag = c(0, 3),
                          argvar = list(fun = "ns", knots = knots, Boundary.knots = boundary),
                          arglag = list(fun = "integer"))
dlnm_terms <- paste0("cb", seq_len(ncol(cross_basis)))
cc3[, (dlnm_terms) := as.data.table(unclass(cross_basis)[, seq_len(ncol(cross_basis))])]
fit_dlnm <- fit_conditional_logit(cc3, dlnm_terms)
predicted <- crosspred(cross_basis, coef = coef(fit_dlnm), vcov = vcov(fit_dlnm),
                       model.link = "logit", at = c(90, 99), cen = 50)
hand_terms <- character(0)
for (k in 0:3) {
  basis <- spline_basis(cc3[[paste0("p_lag", k)]], knots, boundary, paste0("s", k, "_"))
  cc3[, (colnames(basis)) := as.data.table(basis)]
  hand_terms <- c(hand_terms, colnames(basis))
}
fit_hand <- fit_conditional_logit(cc3, hand_terms)
contrast <- sweep(spline_basis(c(90, 99), knots, boundary, "s"), 2,
                  spline_basis(50, knots, boundary, "s")[1, ])
hand_weights <- do.call(cbind, lapply(0:3, function(k) contrast))
colnames(hand_weights) <- hand_terms
hand <- combine_coefficients(fit_hand, hand_weights, conf_level = 0.95)
cat("     cumulative odds ratio at P90 and P99 against P50: dlnm",
    paste(sprintf("%.4f", predicted$allRRfit), collapse = ", "), " by hand",
    paste(sprintf("%.4f", hand$or), collapse = ", "),
    " truth", paste(sprintf("%.4f", exp(1.5 * effect(c(90, 99)))), collapse = ", "), "\n")
check("dlnm cross-basis and the hand-built spline give the same cumulative odds ratios",
      isTRUE(all.equal(unname(predicted$allRRfit), hand$or, tolerance = 1e-6)) &&
      isTRUE(all.equal(unname(predicted$allRRlow), hand$or_low, tolerance = 1e-6)))
check("the cumulative interval at P90 covers the true value",
      hand$or_low[1] <= exp(1.5 * effect(90)) && exp(1.5 * effect(90)) <= hand$or_high[1])

# ---- 13. Parallel runs: same results, and a failed job stops the run --------
square <- function(i) i^2
check("run_parallel returns the results of lapply, in the same order",
      identical(run_parallel(1:9, square, "Squares"), lapply(1:9, square)))
check("run_parallel keeps a job that returns NULL",
      identical(run_parallel(1:3, function(i) NULL, "Nothing"), list(NULL, NULL, NULL)))
message_failed <- tryCatch(
  run_parallel(as.list(as.Date("2020-01-01") + 0:5),
               function(day) if (day == as.Date("2020-01-04")) stop("no variogram") else day,
               "Interpolation"),
  error = function(e) conditionMessage(e)
)
check("run_parallel stops and names the job that failed",
      identical(message_failed, "Interpolation failed on job 4 of 6 (2020-01-04): no variogram"))
if (n_workers() > 1) {
  # A worker killed from outside, as the system does when memory runs out.
  # The job kills its process only inside a worker, not when it is run again.
  main_process <- Sys.getpid()
  survived <- suppressMessages(run_parallel(1:6, function(i) {
    if (i == 2 && Sys.getpid() != main_process) tools::pskill(Sys.getpid(), tools::SIGKILL)
    i^2
  }, "Models"))
  check("run_parallel runs again the jobs of a worker process that died, and loses none",
        identical(survived, lapply(1:6, square)))
} else {
  cat("     one worker process: the check of a dying worker does not apply\n")
}

# ---- 14. Inputs are checked against the manifest ----------------------------
input_folder <- tempfile("inputs_")
dir.create(file.path(input_folder, "months"), recursive = TRUE)
writeLines(c("a;b", "1;2"), file.path(input_folder, "single.csv"))
writeLines("first month", file.path(input_folder, "months", "m1.txt"))
writeLines("second month", file.path(input_folder, "months", "m2.txt"))
describe <- function(input, file, path) {
  data.table(input = input, file = file, bytes = file.size(path), md5 = unname(tools::md5sum(path)))
}
test_manifest <- rbind(
  describe("single", "single.csv", file.path(input_folder, "single.csv")),
  describe("months", c("m1.txt", "m2.txt"), file.path(input_folder, "months", c("m1.txt", "m2.txt")))
)
check("verify_input accepts a file and a folder that match the manifest",
      isTRUE(verify_input("single", file.path(input_folder, "single.csv"), test_manifest)) &&
      isTRUE(verify_input("months", file.path(input_folder, "months"), test_manifest)))
stopped <- function(expr) inherits(tryCatch(expr, error = function(e) e), "error")
writeLines(c("a;b", "1;3"), file.path(input_folder, "single.csv"))       # same size, other content
check("verify_input stops when a file has the same size and other content",
      stopped(verify_input("single", file.path(input_folder, "single.csv"), test_manifest)))
writeLines("second month, edited", file.path(input_folder, "months", "m2.txt"))
check("verify_input stops when one file of a folder changed",
      stopped(verify_input("months", file.path(input_folder, "months"), test_manifest)))
stopifnot(file.rename(file.path(input_folder, "months", "m1.txt"),
                      file.path(input_folder, "months", "m0.txt")))
check("verify_input stops when a file of a folder is missing",
      stopped(verify_input("months", file.path(input_folder, "months"), test_manifest)))
check("verify_input stops for an input that the manifest does not list",
      stopped(verify_input("other", file.path(input_folder, "single.csv"), test_manifest)))
manifest <- read_input_manifest()
check("the manifest lists every input the analysis needs and nothing else",
      all(manifest$input %in% names(get_param("inputs"))) &&
      all(setdiff(names(get_param("inputs")), unlist(get_param("optional_inputs"))) %in% manifest$input))
check("the manifest lists, for the folder inputs, the files the scripts read",
      setequal(manifest[input == "sinca_hourly_dir", file], sinca_hourly_files()) &&
      setequal(manifest[input == "cams_eac4_dir", file], cams_month_files()))

# ---- 15. Reanalysis months cover every local day of the study ---------------
months <- cams_month_files()
offset <- get_param("reanalysis", "utc_offset_hours")
local_days <- study_dates()
# UTC time of the first and of the last 3-hourly record of the local days
first_record <- as.POSIXct(format(min(local_days)), tz = "UTC") - offset * 3600
last_record <- as.POSIXct(format(max(local_days)), tz = "UTC") + 21 * 3600 - offset * 3600
month_of <- function(time) sprintf(get_param("reanalysis", "file_pattern"), format(time, "%Y%m", tz = "UTC"))
cat("     reanalysis files:", length(months), "from", months[1], "to", months[length(months)], "\n")
check("the months read hold the first and the last record of the local study days",
      month_of(first_record) == months[1] && month_of(last_record) == months[length(months)] &&
      !anyDuplicated(months))

# ---- 16. Sorting of text does not depend on the language of the computer ----
check("accented names sort after the others on every machine (C collation)",
      identical(sort(c("Ñuñoa", "Macul", "Zapallar")), c("Macul", "Zapallar", "Ñuñoa")) &&
      identical(Sys.getlocale("LC_COLLATE"), "C"))

# ---- 17. Comparison of tables between two runs ------------------------------
reference_dir <- tempfile("reference_")
new_dir <- tempfile("new_")
dir.create(reference_dir)
dir.create(new_dir)
base_table <- data.table(term = c("e10", "e01", "e11"), or = c(1.067, 1.032, 1.095),
                         deaths = c(1802L, 27597L, 7344L), note = c("a", NA, "c"))
write_pair <- function(file, new_table) {
  fwrite(base_table, file.path(reference_dir, file))
  fwrite(new_table, file.path(new_dir, file))
}
write_pair("same.csv", base_table)
write_pair("last_digits.csv", copy(base_table)[, or := or * (1 + 1e-12)])
write_pair("changed_number.csv", copy(base_table)[2, or := 1.033])
write_pair("changed_text.csv", copy(base_table)[1, term := "e00"])
write_pair("changed_missing.csv", copy(base_table)[3, or := NA])
write_pair("fewer_rows.csv", base_table[1:2])
fwrite(base_table, file.path(reference_dir, "windows_line_ends.csv"))
fwrite(base_table, file.path(new_dir, "windows_line_ends.csv"), eol = "\r\n")
fwrite(base_table, file.path(reference_dir, "gone.csv"))
fwrite(base_table, file.path(new_dir, "added.csv"))
# A column of numbers that comes back as text: decimals next to a whole number
# too large for an integer (the upper limit of a synergy index can be that
# large). An identifier made of whole numbers only has to match exactly.
large <- data.table(term = c("a", "b", "c"),
                    high = c(0.00923273775541214, 6129602301290210000, 2.57561610785827e+88),
                    id = c("6129602301290210001", "6129602301290210002", "6129602301290210003"))
fwrite(large, file.path(reference_dir, "large_numbers.csv"))
fwrite(copy(large)[, high := high * (1 + 1e-13)], file.path(new_dir, "large_numbers.csv"))
fwrite(large, file.path(reference_dir, "changed_identifier.csv"))
fwrite(copy(large)[3, id := "6129602301290210004"], file.path(new_dir, "changed_identifier.csv"))
compared <- compare_tables(reference_dir, new_dir, tolerance = 1e-8)
status_of <- function(name) compared$status[compared$file == paste0(name, ".csv")]
print(compared)
check("compare_tables tells identical tables, last-digit noise and real differences apart",
      status_of("same") == "identical" && status_of("windows_line_ends") == "identical" &&
      status_of("last_digits") == "within tolerance" &&
      all(vapply(c("changed_number", "changed_text", "changed_missing", "fewer_rows"),
                 status_of, character(1)) == "different") &&
      status_of("gone") == "only in reference" && status_of("added") == "only in new")
check("compare_tables reports the size of the largest difference",
      abs(compared[file == "changed_number.csv", max_difference] - 0.001 / 1.033) < 1e-9)
check("compare_tables compares as numbers a column of numbers that was read as text",
      status_of("large_numbers") == "within tolerance" &&
      abs(compared[file == "large_numbers.csv", max_difference] - 1e-13) < 1e-14)
check("compare_tables wants an identifier of whole numbers to match exactly",
      status_of("changed_identifier") == "different")

# ---- 18. Software: packages and Quarto --------------------------------------
source(here::here("Code", "00_packages.R"))
check("every package of the project is installed, and none is installed again",
      identical(install_missing_packages(project_packages, allow = FALSE), character(0)))
message_missing <- tryCatch(
  install_missing_packages(c("data.table", "a.package.that.does.not.exist"), allow = FALSE),
  error = function(e) conditionMessage(e)
)
check("a missing package stops with the command that installs it, when installing is not allowed",
      grepl("a.package.that.does.not.exist", message_missing, fixed = TRUE) &&
      grepl("install.packages(c(\"a.package.that.does.not.exist\"))", message_missing, fixed = TRUE))
used <- unique(unlist(lapply(
  c(list.files(here::here("Code"), pattern = "\\.R$", full.names = TRUE),
    list.files(here::here("docs"), pattern = "^[^p].*\\.qmd$", full.names = TRUE),
    here::here("tests", "test_methods.R")),
  function(file) {
    lines <- readLines(file, warn = FALSE)
    lines <- sub("#.*$", "", lines)
    c(unlist(regmatches(lines, gregexpr("(?<=library\\()[A-Za-z0-9.]+(?=\\))", lines, perl = TRUE))),
      unlist(regmatches(lines, gregexpr("[A-Za-z][A-Za-z0-9.]*(?=::)", lines, perl = TRUE))))
  }
)))
base_packages <- rownames(installed.packages(priority = "base"))
check("the list of packages names every package the code loads",
      all(setdiff(used, base_packages) %in% project_packages))
check("Quarto is found on this machine", file.exists(quarto_command()))

# ---- 19. Linear algebra threads, graphics devices, figure files, kriging variance
check("one thread is asked of the linear algebra library before the worker processes start",
      all(Sys.getenv(c("VECLIB_MAXIMUM_THREADS", "OPENBLAS_NUM_THREADS", "MKL_NUM_THREADS")) == "1"))
# A device that cannot load its library only warns and opens nothing, as
# cairo_pdf() does on a Mac without XQuartz
device_that_fails <- function(file) warning("failed to load cairo DLL")
devices_before <- grDevices::dev.list()
check("device_opens: the standard pdf device opens, a device that only warns does not",
      device_opens(grDevices::pdf) && !device_opens(device_that_fails) &&
      identical(grDevices::dev.list(), devices_before))
check("pdf_figure_device names a device that opens on this machine",
      pdf_figure_device()$name %in% c("cairo_pdf", "pdf") &&
      (pdf_figure_device()$name == "cairo_pdf") == device_opens(grDevices::cairo_pdf))
# write_figure() of Code/02_plot_style.R: 2 by 1 inches at 300 dpi are 600 by 300 pixels
# (width and height are the fifth and sixth four-byte numbers of a PNG file)
source(here::here("Code", "02_plot_style.R"))
figure_folder <- file.path(tempdir(), "write_figure_check")
dir.create(figure_folder, showWarnings = FALSE)
toy_plot <- ggplot(data.frame(x = 1:3, y = c(2, 1, 3)), aes(x, y)) + geom_line()
suppressMessages(write_figure(toy_plot, "toy", width_mm = 50.8, height_mm = 25.4, png_dpi = 300,
                              pdf_device = pdf_figure_device(), folder = figure_folder))
png_size <- readBin(file.path(figure_folder, "toy.png"), "integer", n = 6, size = 4, endian = "big")[5:6]
check("write_figure: a PDF and a PNG with the size and the resolution asked",
      file.size(file.path(figure_folder, "toy.pdf")) > 0 && identical(png_size, c(600L, 300L)) &&
      identical(grDevices::dev.list(), devices_before))
check("kriging_sd: a variance below zero within rounding counts as zero, missing stays missing",
      identical(kriging_sd(c(4, 0, -3.7e-08, NA), rounding = 1e-6), c(2, 0, 0, NA)))
message_variance <- tryCatch(kriging_sd(c(4, -0.5), rounding = 1e-6), error = function(e) conditionMessage(e))
check("kriging_sd stops when a variance is below zero by more than the rounding",
      grepl("below zero by more than the rounding", message_variance, fixed = TRUE))

cat("\nAll checks passed.\n")
