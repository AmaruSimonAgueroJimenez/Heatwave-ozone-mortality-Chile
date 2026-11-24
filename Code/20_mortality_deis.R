# 20_mortality_deis.R
# Deaths of residents of the study municipalities during the warm seasons of
# 2009-2020, with cause, age and sex groups.
#
# Inputs
#   inputs$deis_deaths   DEIS death records (official open-data file, or an
#                        extract of it with the same columns), semicolon
#                        separated, Latin-1
# Outputs
#   Data/Derived/deaths_clean.csv                 one row per death
#   Output/Tables/mortality_flow.csv              records kept at each step
#   Output/Tables/mortality_excluded_causes.csv   causes outside A00-R99
#   Output/Tables/mortality_by_cause_year.csv
#   Output/Tables/mortality_by_municipality_year.csv
#
# The underlying cause (DIAG1) defines the cause groups of the analysis plan.
# Deaths from injuries and external causes (S00-T98 in DIAG1, with the
# external cause V01-Y98 in DIAG2) fall outside non-accidental mortality
# (A00-R99), and so do the U codes (COVID-19 in 2020). They stay in the file,
# flagged, so that every exclusion can be counted.

source(here::here("Code", "00_setup.R"))
source(here::here("Code", "01_functions.R"))

municipalities <- study_municipalities()
study_years <- get_param("mortality", "years")
warm_months <- get_param("study", "warm_season_months")
cause_groups <- get_param("mortality", "cause_groups")
age_breaks <- unlist(get_param("mortality", "age_breaks"))
age_labels <- unlist(get_param("mortality", "age_labels"))
stopifnot(length(age_labels) == length(age_breaks))

needed <- c("FECHA_DEF", "SEXO_NOMBRE", "EDAD_TIPO", "EDAD_CANT", "COD_COMUNA",
            "DIAG1", "DIAG2", "LUGAR_DEFUNCION")
header <- names(fread(input_path("deis_deaths"), sep = ";", encoding = "Latin-1", nrows = 0))
if (!all(needed %in% header)) {
  stop("Columns missing from the DEIS file: ",
       paste(setdiff(needed, header), collapse = ", "), call. = FALSE)
}
raw <- fread(input_path("deis_deaths"), sep = ";", encoding = "Latin-1", select = needed)

# ---- 1. Selection, counted step by step -------------------------------------

flow <- list()
count_step <- function(step, dt) {
  flow[[length(flow) + 1]] <<- data.table(step = step, deaths = nrow(dt))
  invisible(dt)
}
deaths <- count_step("Records in the input file", raw)
deaths[, date := as.Date(FECHA_DEF)]
deaths <- count_step("With date of death and municipality of residence",
                     deaths[!is.na(date) & !is.na(COD_COMUNA)])
deaths <- count_step("Residents of the 33 study municipalities",
                     deaths[COD_COMUNA %in% municipalities$code])
deaths <- count_step(sprintf("Died in %d-%d", study_years[1], study_years[2]),
                     deaths[year(date) >= study_years[1] & year(date) <= study_years[2]])
deaths <- count_step("Died in the warm season (1 November to 31 March)",
                     deaths[is_warm_season(date, warm_months)])

# ---- 2. Cause, age and sex --------------------------------------------------

stopifnot(all(grepl("^[A-Z][0-9]{2}[0-9X]$", deaths$DIAG1)))
deaths[, icd10 := DIAG1]
deaths[, icd3 := substr(DIAG1, 1, 3)]
for (group in names(cause_groups)) {
  deaths[, (group) := icd10_in_ranges(icd3, unlist(cause_groups[[group]]))]
}

# Age in completed years; ages recorded in months, days or hours are under one
stopifnot(all(deaths$EDAD_TIPO %in% c(1, 2, 3, 4, NA)))
deaths[, age_years := fcase(EDAD_TIPO == 1, as.numeric(EDAD_CANT),
                            EDAD_TIPO %in% 2:4, 0)]
deaths[, age_group := cut(age_years, c(age_breaks, Inf), labels = age_labels, right = FALSE)]
deaths[, sex := fcase(SEXO_NOMBRE == "Hombre", "male", SEXO_NOMBRE == "Mujer", "female",
                      default = "undetermined")]
deaths[, place := fcase(LUGAR_DEFUNCION == "Casa habitación", "home",
                        LUGAR_DEFUNCION == "Hospital o Clínica", "hospital or clinic",
                        default = "other")]

count_step("Non-accidental cause (A00-R99)", deaths[non_accidental == TRUE])
count_step("Non-accidental, with age and sex recorded",
           deaths[non_accidental == TRUE & !is.na(age_group) & sex != "undetermined"])
flow <- rbindlist(flow)
fwrite(flow, file.path(dir_tables, "mortality_flow.csv"))

excluded <- deaths[non_accidental == FALSE, .(deaths = .N), keyby = .(
  cause = fcase(substr(icd3, 1, 1) %in% c("S", "T"), "Injuries (S00-T98)",
                substr(icd3, 1, 1) == "U", "Codes for special purposes (U), COVID-19",
                default = "Other"),
  year = year(date))]
fwrite(excluded, file.path(dir_tables, "mortality_excluded_causes.csv"))

# ---- 3. Save ----------------------------------------------------------------

deaths <- merge(deaths, municipalities, by.x = "COD_COMUNA", by.y = "code")
setnames(deaths, c("COD_COMUNA", "name"), c("com", "municipality"))
setorder(deaths, date, com, icd10, age_years, sex)
deaths[, id := .I]
deaths[, year := year(date)]
keep <- c("id", "date", "year", "com", "municipality", "sex", "age_years", "age_group",
          "place", "icd10", "icd3", names(cause_groups))
deaths_clean <- deaths[, ..keep]
fwrite(deaths_clean, file.path(dir_derived, "deaths_clean.csv"))

# ---- 4. Summary tables ------------------------------------------------------

by_cause_year <- rbindlist(lapply(names(cause_groups), function(group) {
  deaths_clean[get(group) == TRUE, .(cause = group, deaths = .N), keyby = year]
}))
setcolorder(by_cause_year, c("cause", "year", "deaths"))
fwrite(by_cause_year, file.path(dir_tables, "mortality_by_cause_year.csv"))

non_accidental <- deaths_clean[non_accidental == TRUE]
summarise_by <- function(group_type, group_column) {
  non_accidental[, .(deaths = .N), keyby = c("com", "municipality", "year", group_column)][
    , .(com, municipality, year, group_type = group_type, group = as.character(get(group_column)), deaths)]
}
by_municipality_year <- rbind(
  non_accidental[, .(group_type = "cause", group = "non_accidental", deaths = .N),
                 keyby = .(com, municipality, year)],
  rbindlist(lapply(setdiff(names(cause_groups), "non_accidental"), function(group) {
    deaths_clean[get(group) == TRUE, .(group_type = "cause", group = group, deaths = .N),
                 keyby = .(com, municipality, year)]
  })),
  summarise_by("age_group", "age_group"),
  summarise_by("sex", "sex")
)
fwrite(by_municipality_year, file.path(dir_tables, "mortality_by_municipality_year.csv"))

message("deaths_clean.csv: ", nrow(deaths_clean), " deaths, ",
        sum(deaths_clean$non_accidental), " non-accidental")
print(flow)
