# 31_attributable_burden.R
# Deaths attributable to heatwave only, high ozone only and both together.
#
# Inputs
#   Data/Derived/case_crossover.csv, exposure_lagged.csv   (script 21)
#   Data/Derived/exposure_2009_2020.csv                    (script 13)
#   Output/Tables/models_main_or.csv                       (script 30)
#   inputs$population                                      INE projections
# Outputs
#   Output/Tables/burden_estimates.csv        excess deaths by level of aggregation
#   Output/Tables/burden_odds_ratios.csv      odds ratios the burden is computed with
#   Output/Tables/burden_baseline_rates.csv   baseline daily mortality per 100,000
#   Output/Tables/burden_population.csv       population by municipality and year
#
# Formula of the analysis plan, for municipality i, calendar year t, age group j
# and exposure category c (E10 heatwave only, E01 high ozone only, E11 both):
#   ED = Population x Age proportion x Baseline daily rate x (OR - 1) x Exposure days
# The baseline daily rate is the deaths per inhabitant and day on the days of
# the warm season with neither exposure (E00). The first three factors
# multiply to the deaths per day on E00 days, so the population cancels out:
#   ED = (deaths on E00 days / E00 days) x (OR - 1) x days in category c
# This is what the script computes (`excess`). The population only enters the
# rates per 100,000. tests/test_methods.R checks the identity.
#
# A second estimate (`attributable`) applies the attributable fraction among
# the exposed, (OR - 1) / OR, to the deaths observed on the exposed days. It
# does not rely on E00 days having the same baseline mortality as the exposed
# days, which fall in different weeks of the season.
#
# The odds ratios are those of the compound model of script 30, refitted here
# to obtain their covariance; the script stops if they differ from the table
# written by script 30.

suppressPackageStartupMessages(library(survival))
source(here::here("Code", "00_setup.R"))
source(here::here("Code", "01_functions.R"))

conf_level <- get_param("models", "conf_level")
lags <- unlist(get_param("design", "lags_days"))
outcomes <- unlist(get_param("burden", "outcomes"))
heatwave_main <- get_param("models", "heatwave_main")
ozone_flag_main <- paste0(get_param("air_pollution", "ozone_series_main"), "_",
                          get_param("air_pollution", "interpolation_method_main"),
                          "_gt_p", get_param("models", "ozone_cutoff_main"))
age_labels <- unlist(get_param("mortality", "age_labels"))
study_years <- unlist(get_param("study", "years"))
open_age <- get_param("population", "open_age")
rate_breaks <- unlist(get_param("population", "rate_age_breaks"))
rate_labels <- unlist(get_param("population", "rate_age_labels"))
municipalities <- study_municipalities()
categories <- c("e10", "e01", "e11")

cc <- fread(derived_path("case_crossover.csv"), encoding = "UTF-8")
cc[, `:=`(date = as.Date(date), case_date = as.Date(case_date))]
lagged <- fread(derived_path("exposure_lagged.csv"), encoding = "UTF-8")
lagged[, date := as.Date(date)]
exposure <- fread(derived_path("exposure_2009_2020.csv"), encoding = "UTF-8",
                  select = c("com", "date", "year", "in_study_period", "warm_season",
                             heatwave_main, ozone_flag_main))
exposure[, date := as.Date(date)]
main_or <- fread(file.path(dir_tables, "models_main_or.csv"))

# ---- 1. Exposure category of every warm-season day, by lag ------------------

# Category of the day `lag` days before each warm-season day of the study
all_lagged <- lagged_exposure(exposure[, c("com", "date", heatwave_main, ozone_flag_main),
                                       with = FALSE],
                              c(heatwave_main, ozone_flag_main), lags)
study_days <- exposure[in_study_period == TRUE & warm_season == TRUE, .(com, date, year)]
stopifnot(nrow(study_days) == nrow(municipalities) * uniqueN(study_days$date))
days <- rbindlist(lapply(lags, function(lag) {
  x <- all_lagged[study_days, on = .(com, date)]
  hw <- x[[paste0(heatwave_main, "_lag", lag)]]
  oz <- x[[paste0(ozone_flag_main, "_lag", lag)]]
  x[, .(com, date, year, lag = lag,
        category = fcase(hw == 0 & oz == 0, "e00", hw == 1 & oz == 0, "e10",
                         hw == 0 & oz == 1, "e01", hw == 1 & oz == 1, "e11"))]
}))
# The same values as the lagged exposures the models use
check <- merge(days[lag == lags[1]], lagged[, c("com", "date", paste0(heatwave_main, "_lag", lags[1]),
                                                paste0(ozone_flag_main, "_lag", lags[1])), with = FALSE],
               by = c("com", "date"))
setnames(check, 6:7, c("hw", "oz"))
stopifnot(nrow(check) > 0,
          check[, all((category %in% c("e10", "e11")) == (hw == 1), na.rm = TRUE)],
          check[, all((category %in% c("e01", "e11")) == (oz == 1), na.rm = TRUE)])
missing_days <- days[is.na(category), .N, by = lag]

# ---- 2. Odds ratios and expected deaths by age group -------------------------

# Deaths expected and observed on the exposed days, by municipality and year
burden_cells <- function(case_rows, lag_value) {
  deaths <- case_rows[, .(n = .N), by = .(com, date = case_date)]
  x <- merge(days[lag == lag_value], deaths, by = c("com", "date"), all.x = TRUE)
  x[is.na(n), n := 0L]
  stopifnot(sum(x$n) == nrow(case_rows))   # every death falls on a study day
  counts <- x[!is.na(category), .(days = .N, deaths = sum(n)), by = .(com, year, category)]
  reference <- counts[category == "e00", .(com, year, reference_days = days,
                                           reference_deaths = deaths)]
  out <- merge(CJ(com = municipalities$code, year = study_years[1]:study_years[2],
                  category = categories),
               counts, by = c("com", "year", "category"), all.x = TRUE)
  out[is.na(days), `:=`(days = 0L, deaths = 0L)]
  out <- merge(out, reference, by = c("com", "year"), all.x = TRUE)
  # A baseline needs days with neither exposure in every municipality and year
  stopifnot(!anyNA(out$reference_days), all(out$reference_days > 0))
  out[, `:=`(expected = reference_deaths / reference_days * days, observed = deaths,
             deaths_no_category = x[is.na(category), sum(n)])]
  out[]
}

fit_group <- function(outcome_value, subgroup_value, lag_value) {
  collapsed <- prepare_model_data(cc, lagged, outcome_value, subgroup_value, age_labels)
  d <- attach_compound_exposure(collapsed, lagged, heatwave_main, ozone_flag_main, lag_value)
  result <- compound_results(d, conf_level)
  beta <- coef(result$fit)[categories]
  # Same estimates as script 30
  reference <- main_or[outcome == outcome_value & subgroup == subgroup_value &
                         lag == lag_value & model == "compound"][match(categories, term)]
  stopifnot(nrow(reference) == 3, max(abs(reference$log_or - beta)) < 1e-8)
  case_rows <- cc[select_case_rows(cc, outcome_value, subgroup_value, age_labels) & case == 1]
  list(beta = beta, covariance = vcov(result$fit)[categories, categories],
       or = cbind(outcome = outcome_value, subgroup = subgroup_value, lag = lag_value, result$or),
       cells = cbind(outcome = outcome_value, age_group = subgroup_value, lag = lag_value,
                     burden_cells(case_rows, lag_value)),
       deaths = cbind(outcome = outcome_value, age_group = subgroup_value, lag = lag_value,
                      case_rows[, .(deaths_total = .N), by = .(com, year = year(case_date))]))
}

jobs <- CJ(outcome = outcomes, subgroup = c(age_labels, "all"), lag = lags, sorted = FALSE)
fits <- lapply(seq_len(nrow(jobs)), function(i) fit_group(jobs$outcome[i], jobs$subgroup[i],
                                                          jobs$lag[i]))
odds_table <- rbindlist(lapply(fits, `[[`, "or"))
fwrite(odds_table, file.path(dir_tables, "burden_odds_ratios.csv"))
cells <- rbindlist(lapply(fits, `[[`, "cells"))
cells <- merge(cells, municipalities, by.x = "com", by.y = "code")
setnames(cells, "name", "municipality")
# All the deaths of each outcome and age group, the denominator of the
# attributable fraction
death_totals <- rbindlist(lapply(fits, `[[`, "deaths"))
death_totals <- merge(death_totals, municipalities, by.x = "com", by.y = "code")
setnames(death_totals, "name", "municipality")

# ---- 3. Population -----------------------------------------------------------

population <- read_population(input_path("population"), municipalities$code, study_years, open_age)
population[, rate_age_group := as.character(cut(age, c(rate_breaks, Inf), labels = rate_labels,
                                                right = FALSE))]
stopifnot(!anyNA(population$rate_age_group))
population_table <- dcast(population, com + year ~ rate_age_group, value.var = "population",
                          fun.aggregate = sum)
population_table[, total := rowSums(.SD), .SDcols = rate_labels]
population_table <- merge(municipalities, population_table, by.x = "code", by.y = "com")
setnames(population_table, c("code", "name"), c("com", "municipality"))
fwrite(population_table, file.path(dir_tables, "burden_population.csv"))

# ---- 4. Excess deaths by level of aggregation --------------------------------

levels_by <- list(total = character(0), age_group = "age_group", year = "year",
                  municipality = c("com", "municipality"),
                  municipality_year = c("com", "municipality", "year"))
exposure_days <- function(lag_value, by) {
  counts <- days[lag == lag_value & category %in% categories]
  counts <- merge(counts, municipalities, by.x = "com", by.y = "code")
  setnames(counts, "name", "municipality")
  by <- setdiff(by, "age_group")
  rbind(counts[, .(exposure_days = .N), by = c(by, "category")],
        counts[, .(category = "any", exposure_days = .N), by = by])
}
person_years <- function(by) {
  by <- setdiff(by, "age_group")
  population_table[, .(population = sum(total)), by = by]
}

estimate_level <- function(outcome_value, lag_value, source, level) {
  by <- levels_by[[level]]
  if (source == "age-specific") {
    use <- cells[outcome == outcome_value & lag == lag_value & age_group %in% age_labels]
    keys <- age_labels
  } else {
    if (level == "age_group") return(NULL)
    use <- cells[outcome == outcome_value & lag == lag_value & age_group == "all"]
    keys <- "all"
  }
  models <- lapply(setNames(keys, keys), function(key) {
    fits[[which(jobs$outcome == outcome_value & jobs$subgroup == key & jobs$lag == lag_value)]][
      c("beta", "covariance")]
  })
  out <- attributable_deaths(use, models, by, conf_level)
  totals <- death_totals[outcome == outcome_value & lag == lag_value & age_group %in% keys,
                         .(deaths_total = sum(deaths_total)), by = by]
  out <- if (length(by) == 0) cbind(out, totals) else merge(out, totals, by = by, all.x = TRUE)
  out[is.na(deaths_total), deaths_total := 0L]
  out[, `:=`(excess_pct_of_deaths = 100 * excess / deaths_total,
             attributable_pct_of_deaths = 100 * attributable / deaths_total)]
  days_by <- exposure_days(lag_value, by)
  out <- merge(out, days_by, by = intersect(names(out), names(days_by)), all.x = TRUE)
  out[is.na(exposure_days), exposure_days := 0L]
  if (level != "age_group") {
    pop <- person_years(by)
    out <- if (length(by) == 0) cbind(out, pop) else merge(out, pop, by = intersect(names(out), names(pop)))
    out[, `:=`(excess_per_100k = 1e5 * excess / population,
               attributable_per_100k = 1e5 * attributable / population)]
  }
  cbind(outcome = outcome_value, lag = lag_value, or_source = source, level = level, out)
}

estimate_jobs <- CJ(outcome = outcomes, lag = lags, source = c("age-specific", "all ages"),
                    level = names(levels_by), sorted = FALSE)
estimates <- rbindlist(lapply(seq_len(nrow(estimate_jobs)), function(i) {
  estimate_level(estimate_jobs$outcome[i], estimate_jobs$lag[i], estimate_jobs$source[i],
                 estimate_jobs$level[i])
}), fill = TRUE)
setcolorder(estimates, c("outcome", "lag", "or_source", "level", "age_group", "year", "com",
                         "municipality", "category", "exposure_days", "deaths_total"))
setorder(estimates, outcome, lag, or_source, level, age_group, year, com, category, na.last = TRUE)
fwrite(estimates, file.path(dir_tables, "burden_estimates.csv"))

# ---- 5. Baseline daily mortality per 100,000 ----------------------------------

# Deaths per day on days with neither exposure, over the population of the
# age group, for the age groups that have a denominator
baseline <- rbindlist(lapply(outcomes, function(outcome_value) rbindlist(lapply(lags, function(lag_value) {
  case_rows <- cc[select_case_rows(cc, outcome_value, "all", age_labels) & case == 1 &
                    !is.na(age_years)]
  case_rows[, rate_age_group := as.character(cut(age_years, c(rate_breaks, Inf),
                                                 labels = rate_labels, right = FALSE))]
  deaths <- case_rows[, .(n = .N), by = .(com, date = case_date, rate_age_group)]
  reference_days <- days[lag == lag_value & category == "e00"]
  x <- merge(CJ(com = municipalities$code, year = study_years[1]:study_years[2],
                rate_age_group = rate_labels),
             merge(reference_days, deaths, by = c("com", "date"))[
               , .(reference_deaths = sum(n)), by = .(com, year, rate_age_group)],
             by = c("com", "year", "rate_age_group"), all.x = TRUE)
  x[is.na(reference_deaths), reference_deaths := 0L]
  x <- merge(x, reference_days[, .(reference_days = .N), by = .(com, year)], by = c("com", "year"))
  pop <- melt(population_table, id.vars = c("com", "municipality", "year"),
              measure.vars = rate_labels, variable.name = "rate_age_group",
              value.name = "population", variable.factor = FALSE)
  x <- merge(x, pop, by = c("com", "year", "rate_age_group"))
  x[, baseline_daily_rate_per_100k := 1e5 * reference_deaths / reference_days / population]
  cbind(outcome = outcome_value, lag = lag_value, x)
}))))
setcolorder(baseline, c("outcome", "lag", "com", "municipality", "year", "rate_age_group"))
fwrite(baseline, file.path(dir_tables, "burden_baseline_rates.csv"))

if (nrow(missing_days) > 0) {
  message("Warm-season days without an exposure category, by lag:")
  print(missing_days)
}
message("Deaths on days without an exposure category: ",
        cells[, max(deaths_no_category)])
lag_main <- get_param("models", "lag_main")
print(estimates[level == "total" & lag == lag_main,
                .(outcome, or_source, category, exposure_days, observed = round(observed),
                  expected = round(expected, 1), excess = round(excess, 1),
                  excess_low = round(excess_low, 1), excess_high = round(excess_high, 1),
                  attributable = round(attributable, 1))])
