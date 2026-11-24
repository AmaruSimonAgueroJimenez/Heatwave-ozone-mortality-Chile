# 21_case_crossover_dataset.R
# Analytic dataset of the space-time-stratified case-crossover design.
#
# Inputs
#   Data/Derived/deaths_clean.csv         (script 20)
#   Data/Derived/exposure_2009_2020.csv   (script 13)
# Outputs
#   Data/Derived/case_crossover.csv       one row per death and referent day
#   Data/Derived/exposure_lagged.csv      exposures at lags 0 to 3 by municipality-day
#   Output/Tables/case_crossover_summary.csv
#
# Each non-accidental death is its own stratum. The referent days are all the
# days of the same municipality, calendar year, month and day of the week:
# the day of death (case = 1) and three or four control days (case = 0).
# Exposures are attached to any model by joining exposure_lagged.csv on the
# municipality and the day: the column <variable>_lag<k> is the exposure k
# days before that day.

source(here::here("Code", "00_setup.R"))
source(here::here("Code", "01_functions.R"))

lags <- unlist(get_param("design", "lags_days"))
stopifnot(identical(unlist(get_param("design", "control_strata")),
                    c("municipality", "year", "month", "day_of_week")))

deaths <- fread(derived_path("deaths_clean.csv"), encoding = "UTF-8")
deaths[, date := as.Date(date)]
deaths <- deaths[non_accidental == TRUE]

exposure <- fread(derived_path("exposure_2009_2020.csv"), encoding = "UTF-8")
exposure[, date := as.Date(date)]

# ---- 1. Referent days -------------------------------------------------------

cc <- build_case_crossover(deaths[, .(id, com, date)])
# Every referent day must be a day of the exposure dataset
stopifnot(nrow(fsetdiff(unique(cc[, .(com, date)]), exposure[, .(com, date)])) == 0)

person <- deaths[, !c("com", "date", "year", "municipality", "icd10")]
cc <- merge(cc, person, by = "id")
setorder(cc, id, date)
fwrite(cc, file.path(dir_derived, "case_crossover.csv"))

# ---- 2. Lagged exposures ----------------------------------------------------

exposure_variables <- c(
  grep("^hw_", names(exposure), value = TRUE),
  grep("_gt_p[0-9]+$", names(exposure), value = TRUE),
  "tmax", "tad",
  as.vector(outer(c("o3", "o3obs", "o3mda8", "pm25", "no2", "hum"), c("idw", "ok"),
                  paste, sep = "_"))
)
lagged <- lagged_exposure(exposure, exposure_variables, lags)
# Keep the municipality-days that are a referent day of at least one death
lagged <- lagged[unique(cc[, .(com, date)]), on = .(com, date)]
fwrite(lagged, file.path(dir_derived, "exposure_lagged.csv"))

# ---- 3. Summary -------------------------------------------------------------

controls <- cc[, .(controls = .N - 1L), by = id]
summary_table <- data.table(
  item = c("Deaths (strata)", "Rows (deaths and referent days)",
           "Deaths with 3 control days", "Deaths with 4 control days",
           "Municipality-days used as referent", "Lagged exposure columns"),
  value = c(uniqueN(cc$id), nrow(cc), sum(controls$controls == 3), sum(controls$controls == 4),
            nrow(lagged), ncol(lagged) - 2L)
)
fwrite(summary_table, file.path(dir_tables, "case_crossover_summary.csv"))

message("case_crossover.csv: ", nrow(cc), " rows for ", uniqueN(cc$id), " deaths")
print(summary_table)
