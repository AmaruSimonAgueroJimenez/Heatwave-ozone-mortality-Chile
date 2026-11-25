# 30_models_main.R
# Conditional logistic regression of the case-crossover dataset: single
# exposures, joint model, four-category compound exposure and additive
# interaction (REOI, AP, S).
#
# Inputs
#   Data/Derived/case_crossover.csv    (script 21)
#   Data/Derived/exposure_lagged.csv   (script 21)
# Outputs
#   Output/Tables/models_main_or.csv             odds ratios, headline definition
#   Output/Tables/models_main_interaction.csv    REOI, AP, S, headline definition
#   Output/Tables/models_grid.csv                compound model over the definition grid
#   Output/Tables/models_ozone_continuous.csv    ozone per 10 ppb
#
# Models, for each outcome, subgroup and single-day lag (0 to 3):
#   heatwave   logit P = b1 HW
#   ozone      logit P = b2 O3high
#   joint      logit P = b1 HW + b2 O3high
#   compound   logit P = b10 E10 + b01 E01 + b11 E11        (reference E00)
# The compound model is the joint model with its interaction term written as
# categories: OR11 / (OR10 x OR01) is the multiplicative interaction.
#
# The fits use one row per municipality, day of death and referent day,
# weighted by the number of deaths; tests/test_methods.R shows this equals the
# fit on the individual records.

suppressPackageStartupMessages(library(survival))
source(here::here("Code", "00_setup.R"))
source(here::here("Code", "01_functions.R"))

conf_level <- get_param("models", "conf_level")
lags <- unlist(get_param("design", "lags_days"))
heatwave_main <- get_param("models", "heatwave_main")
ozone_main <- paste0(get_param("air_pollution", "ozone_series_main"), "_",
                     get_param("air_pollution", "interpolation_method_main"))
ozone_flag_main <- paste0(ozone_main, "_gt_p", get_param("models", "ozone_cutoff_main"))
outcomes <- names(get_param("mortality", "cause_groups"))
age_labels <- unlist(get_param("mortality", "age_labels"))

cc <- fread(derived_path("case_crossover.csv"), encoding = "UTF-8")
cc[, `:=`(date = as.Date(date), case_date = as.Date(case_date))]
lagged <- fread(derived_path("exposure_lagged.csv"), encoding = "UTF-8")
lagged[, date := as.Date(date)]
stopifnot(all(c(paste0(heatwave_main, "_lag", lags), paste0(ozone_flag_main, "_lag", lags))
              %in% names(lagged)))

# The datasets and the compound model come from Code/01_functions.R, shared
# with the burden, sensitivity and distributed-lag scripts.
prepare <- function(outcome, subgroup) {
  prepare_model_data(cc, lagged, outcome, subgroup, age_labels)
}
with_exposure <- function(collapsed, heatwave, ozone_flag, lag) {
  attach_compound_exposure(collapsed, lagged, heatwave, ozone_flag, lag)
}

# ---- 1. Headline definition: all outcomes, subgroups and lags ----------------

subgroups <- c("all", age_labels, "male", "female")
main_jobs <- CJ(outcome = outcomes, subgroup = subgroups, sorted = FALSE)

run_main <- function(i) {
  outcome <- main_jobs$outcome[i]
  subgroup <- main_jobs$subgroup[i]
  collapsed <- prepare(outcome, subgroup)
  or_list <- list()
  interaction_list <- list()
  for (lag in lags) {
    d <- with_exposure(collapsed, heatwave_main, ozone_flag_main, lag)
    counts <- category_deaths(d)
    label <- data.table(outcome = outcome, subgroup = subgroup, lag = lag)
    # A category with no deaths on the day of death has no estimable odds ratio
    estimable <- all(unlist(counts[, .(deaths_e10, deaths_e01, deaths_e11)]) > 0)
    logged <- with_warnings(list(
      single = rbind(
        cbind(model = "heatwave", odds_ratios(fit_conditional_logit(d, "hw"), conf_level)),
        cbind(model = "ozone", odds_ratios(fit_conditional_logit(d, "oz"), conf_level)),
        cbind(model = "joint", odds_ratios(fit_conditional_logit(d, c("hw", "oz")), conf_level))
      ),
      compound = if (estimable) compound_results(d, conf_level)
    ))
    fit_warnings <- paste(unique(logged$warnings), collapse = " | ")
    or_rows <- logged$value$single
    if (estimable) {
      or_rows <- rbind(or_rows, cbind(model = "compound", logged$value$compound$or))
      interaction_list[[length(interaction_list) + 1]] <- cbind(
        label, logged$value$compound$interaction, counts, fit_warnings = fit_warnings
      )
    }
    or_list[[length(or_list) + 1]] <- cbind(label, or_rows, counts, fit_warnings = fit_warnings)
  }
  list(or = rbindlist(or_list), interaction = rbindlist(interaction_list))
}
main_results <- run_parallel(seq_len(nrow(main_jobs)), run_main, "Model run")
main_or <- rbindlist(lapply(main_results, `[[`, "or"))
main_interaction <- rbindlist(lapply(main_results, `[[`, "interaction"))
main_or[, `:=`(heatwave = heatwave_main, ozone = ozone_flag_main)]
main_interaction[, `:=`(heatwave = heatwave_main, ozone = ozone_flag_main)]
fwrite(main_or, file.path(dir_tables, "models_main_or.csv"))
fwrite(main_interaction, file.path(dir_tables, "models_main_interaction.csv"))

# ---- 2. Definition grid: compound model, two outcomes, all lags --------------

heatwaves <- unique(sub("_lag[0-9]+$", "", grep("^hw_", names(lagged), value = TRUE)))
ozone_flags <- unique(sub("_lag[0-9]+$", "", grep("_gt_p[0-9]+_lag", names(lagged), value = TRUE)))
grid_outcomes <- c("non_accidental", "cardiopulmonary")
grid_jobs <- CJ(outcome = grid_outcomes, heatwave = heatwaves, ozone = ozone_flags, sorted = FALSE)
grid_data <- lapply(setNames(grid_outcomes, grid_outcomes), prepare, subgroup = "all")

run_grid <- function(i) {
  job <- grid_jobs[i]
  rbindlist(lapply(lags, function(lag) {
    d <- with_exposure(grid_data[[job$outcome]], job$heatwave, job$ozone, lag)
    counts <- category_deaths(d)
    # A category with no deaths on the day of death has no estimable odds ratio
    if (any(unlist(counts[, .(deaths_e10, deaths_e01, deaths_e11)]) == 0)) {
      return(cbind(job, lag = lag, counts, estimable = FALSE))
    }
    logged <- with_warnings(compound_results(d, conf_level))
    compound <- logged$value
    or <- dcast(compound$or[, .(term, or, or_low, or_high)], . ~ term,
                value.var = c("or", "or_low", "or_high"))[, !"."]
    measures <- dcast(compound$interaction[, .(measure = gsub(" ", "_", tolower(measure)),
                                               estimate, low, high)],
                      . ~ measure, value.var = c("estimate", "low", "high"))[, !"."]
    cbind(job, lag = lag, counts, estimable = TRUE, or, measures,
          fit_warnings = paste(unique(logged$warnings), collapse = " | "))
  }), fill = TRUE)
}
grid_results <- run_parallel(seq_len(nrow(grid_jobs)), run_grid, "Grid run")
fwrite(rbindlist(grid_results, fill = TRUE), file.path(dir_tables, "models_grid.csv"))

# ---- 3. Ozone as a continuous exposure, per 10 ppb ---------------------------

ozone_series <- as.vector(outer(c("o3", "o3obs", "o3mda8"), c("idw", "ok"), paste, sep = "_"))
continuous <- rbindlist(lapply(grid_outcomes, function(outcome) {
  collapsed <- grid_data[[outcome]]
  rbindlist(lapply(ozone_series, function(series) rbindlist(lapply(lags, function(lag) {
    d <- copy(collapsed)
    d[, ozone_10ppb := lagged[[paste0(series, "_lag", lag)]][exposure_row] / 10]
    d[, hw := lagged[[paste0(heatwave_main, "_lag", lag)]][exposure_row]]
    complete <- d[, .(ok = !anyNA(ozone_10ppb) && !anyNA(hw)), by = stratum][ok == TRUE, stratum]
    d <- d[stratum %in% complete]
    rbind(
      cbind(outcome = outcome, series = series, lag = lag, adjustment = "none",
            odds_ratios(fit_conditional_logit(d, "ozone_10ppb"), conf_level)),
      cbind(outcome = outcome, series = series, lag = lag, adjustment = heatwave_main,
            odds_ratios(fit_conditional_logit(d, c("ozone_10ppb", "hw")), conf_level))
    )[term == "ozone_10ppb"]
  }))))
}))
fwrite(continuous, file.path(dir_tables, "models_ozone_continuous.csv"))

message("Models written: ", nrow(main_or), " odds ratios (headline), ",
        nrow(grid_jobs) * length(lags), " grid fits")
