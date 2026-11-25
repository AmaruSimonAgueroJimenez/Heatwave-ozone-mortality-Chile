# 18_exposure_time_series.R
# Time series of the exposures over the whole study period, for every
# municipality and for the mean of the 33 municipalities: tables and figures.
#
# Input
#   Data/Derived/exposure_2009_2020.csv   (script 13)
# Outputs, tables in Output/Tables
#   series_daily_all_municipalities.csv   one row per day: mean, lowest and highest
#                                         municipality, and municipalities with each exposure
#   series_monthly_by_municipality.csv    one row per municipality and month, and the
#                                         mean of the 33 municipalities
#   series_annual_by_municipality.csv     the same by calendar year
# Outputs, figures in Output/Figures (PDF and PNG)
#   series_1_daily_temperature_ozone      daily mean of the municipalities and range between them
#   series_2_daily_compound_exposure      municipalities by exposure category, day by day
#   series_3_monthly_by_municipality      monthly means: every municipality and their mean
#   series_4_annual_by_municipality       annual means and days of exposure: the same
#   series_5_municipalities_temperature   one panel per municipality: maximum temperature
#   series_6_municipalities_ozone         one panel per municipality: ozone
#   series_7_municipalities_heatwave_days one panel per municipality: heatwave days by year
#
# The series cover the days of analysis: 1 November to 31 March of the calendar
# years 2009 to 2020. Heatwave, ozone series and ozone cut-off are the working
# definitions of config/config.yml (models: heatwave_main, ozone_cutoff_main;
# air_pollution: ozone_series_main, interpolation_method_main).
#
# "Mean of the municipalities" is the plain mean of the 33 municipal values:
# every municipality counts the same, whatever its population or area.
#
# Every figure is drawn from one of the three tables written here.

source(here::here("Code", "00_setup.R"))
source(here::here("Code", "01_functions.R"))
source(here::here("Code", "02_plot_style.R"))

municipalities <- study_municipalities()
heatwave_main <- get_param("models", "heatwave_main")
method_main <- get_param("air_pollution", "interpolation_method_main")
ozone_main <- paste0(get_param("air_pollution", "ozone_series_main"), "_", method_main)
ozone_mda8 <- paste0("o3mda8_", method_main)
ozone_flag_main <- paste0(ozone_main, "_gt_p", get_param("models", "ozone_cutoff_main"))
png_dpi <- get_param("figures", "png_dpi")
width_mm <- get_param("figures", "width_double_mm")
mean_label <- "Mean of the 33 municipalities"
stopifnot(nrow(municipalities) == 33)

# ---- 1. Days of analysis ----------------------------------------------------

exposure <- fread(derived_path("exposure_2009_2020.csv"), encoding = "UTF-8")
exposure[, date := as.Date(date)]
needed <- c("com", "municipality", "date", "year", "tmax", ozone_main, ozone_mda8,
            heatwave_main, ozone_flag_main)
stopifnot(all(needed %in% names(exposure)))
days <- exposure[in_study_period == TRUE & warm_season == TRUE, ..needed]
setnames(days, c(ozone_main, ozone_mda8, heatwave_main, ozone_flag_main),
         c("o3", "mda8", "heatwave", "ozone_high"))
days[, category := compound_category(heatwave, ozone_high)]
days[, month := month(date)]
# Warm season of each day: November and December belong to the season that
# starts that year, January to March to the one that started the year before
days[, season_start := year - as.integer(month <= 3)]
days[, season := sprintf("%d/%02d", season_start, (season_start + 1L) %% 100L)]
n_days <- uniqueN(days$date)
stopifnot(
  nrow(days) == nrow(municipalities) * n_days,
  anyDuplicated(days[, .(com, date)]) == 0,
  !anyNA(days$tmax), !anyNA(days$heatwave)
)

# ---- 2. Tables --------------------------------------------------------------

# Daily: the municipal values of each day. Some municipality-days have no ozone
# value (too few stations with valid records): the columns *_municipalities
# count the municipalities behind each mean, and a day on which none has a
# value is left empty.
across_municipalities <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(list(NA_real_, NA_real_, NA_real_, 0L))
  list(mean(x), min(x), max(x), length(x))
}
daily <- days[, .(
  municipalities = .N,
  # Municipalities with each single exposure, then by category of the compound exposure
  with_heatwave = sum(heatwave),
  with_high_ozone = sum(ozone_high, na.rm = TRUE),
  neither = sum(category == "E00", na.rm = TRUE),
  heatwave_only = sum(category == "E10", na.rm = TRUE),
  high_ozone_only = sum(category == "E01", na.rm = TRUE),
  heatwave_and_high_ozone = sum(category == "E11", na.rm = TRUE),
  not_classified = sum(is.na(category))
), keyby = .(date, season)]
for (v in c("tmax", "o3", "mda8")) {
  across <- days[, across_municipalities(get(v)), keyby = date]
  setnames(across, c("date", paste0(v, c("_mean", "_lowest", "_highest", "_municipalities"))))
  daily <- merge(daily, across, by = "date")
}
category_counts <- c("with_heatwave", "with_high_ozone", "neither", "heatwave_only",
                     "high_ozone_only", "heatwave_and_high_ozone", "not_classified")
setcolorder(daily, c("date", "season", "municipalities",
                     setdiff(names(daily), c("date", "season", "municipalities", category_counts))))
stopifnot(
  nrow(daily) == n_days,
  all(daily$tmax_municipalities == nrow(municipalities)),
  daily[, all(neither + heatwave_only + high_ozone_only + heatwave_and_high_ozone +
                not_classified == municipalities)],
  # A municipality with a heatwave is in one of the two heatwave categories,
  # or has no ozone value that day
  daily[, all(with_heatwave >= heatwave_only + heatwave_and_high_ozone &
                with_heatwave <= heatwave_only + heatwave_and_high_ozone + not_classified)]
)
fwrite(daily, file.path(dir_tables, "series_daily_all_municipalities.csv"))

# Monthly and annual: one row per municipality and period, then the mean of
# the municipalities for each period
summarise_period <- function(by_columns) {
  by_municipality <- days[, .(
    days = .N,
    # Means over the days with a value; o3_days and mda8_days count them
    tmax = mean(tmax), o3 = mean(o3, na.rm = TRUE), mda8 = mean(mda8, na.rm = TRUE),
    o3_days = sum(!is.na(o3)), mda8_days = sum(!is.na(mda8)),
    heatwave_days = sum(heatwave),
    high_ozone_days = sum(ozone_high, na.rm = TRUE),
    heatwave_only_days = sum(category == "E10", na.rm = TRUE),
    compound_days = sum(category == "E11", na.rm = TRUE),
    not_classified_days = sum(is.na(category))
  ), by = c("com", "municipality", by_columns)]
  # The means below need at least one day with ozone in every municipality and period
  if (any(by_municipality$o3_days == 0) || any(by_municipality$mda8_days == 0)) {
    stop("A municipality has no ozone value in a whole period: the means of the ",
         "municipalities need a rule for that case.", call. = FALSE)
  }
  value_columns <- setdiff(names(by_municipality), c("com", "municipality", by_columns))
  all_municipalities <- by_municipality[, lapply(.SD, mean), by = by_columns,
                                        .SDcols = value_columns]
  all_municipalities[, `:=`(com = NA_integer_, municipality = mean_label)]
  out <- rbind(by_municipality, all_municipalities, use.names = TRUE)
  setorderv(out, c(by_columns, "com"), na.last = TRUE)
  out[]
}
monthly <- summarise_period(c("year", "month", "season"))
monthly[, month_start := as.Date(sprintf("%04d-%02d-01", year, month))]
setcolorder(monthly, c("com", "municipality", "year", "month", "month_start", "season"))
annual <- summarise_period("year")
n_months <- uniqueN(days[, .(year, month)])
stopifnot(
  nrow(monthly) == (nrow(municipalities) + 1) * n_months,
  nrow(annual) == (nrow(municipalities) + 1) * uniqueN(days$year),
  # The days of the periods add up to the days of analysis, in every municipality
  monthly[!is.na(com), sum(days), by = com][, all(V1 == n_days)],
  annual[!is.na(com), sum(days), by = com][, all(V1 == n_days)],
  # Days with a heatwave are heatwave-only days, compound days or days without ozone
  annual[!is.na(com), all(heatwave_days >= heatwave_only_days + compound_days)]
)
fwrite(monthly, file.path(dir_tables, "series_monthly_by_municipality.csv"))
fwrite(annual, file.path(dir_tables, "series_annual_by_municipality.csv"))

# ---- 3. Figures -------------------------------------------------------------

pdf_device <- pdf_figure_device()
message("PDF figures: ", pdf_device$name, " device")
save_figure <- function(plot, name, height_mm) {
  write_figure(plot, name, width_mm, height_mm, png_dpi = png_dpi, pdf_device = pdf_device,
               folder = dir_figures)
}
theme_set(theme_publication(base_size = 9))
colour_mean <- ink$primary
colour_municipality <- ink$neutral
label_o3 <- sprintf("Ozone, 24-hour mean (%s), ppb", ozone_main)
label_mda8 <- sprintf("Ozone, MDA8 (%s), ppb", ozone_mda8)
label_tmax <- "Maximum temperature, °C"
variable_labels <- c(tmax = label_tmax, o3 = label_o3, mda8 = label_mda8)
year_breaks <- as.Date(sprintf("%d-01-01", seq(2009, 2021, by = 2)))
note_lines <- "Grey lines: each municipality. Dark line: mean of the 33 municipalities."
january_only <- function(dates) ifelse(format(dates, "%m") == "01", "Jan", "")

# Figure 1. Daily series: mean of the municipalities and range between them,
# one column per warm season so that the months without data take no room
# Table: series_daily_all_municipalities.csv
daily_long <- rbindlist(lapply(names(variable_labels), function(v) {
  daily[, .(date, season, variable = variable_labels[[v]], mean = get(paste0(v, "_mean")),
            lowest = get(paste0(v, "_lowest")), highest = get(paste0(v, "_highest")))]
}))
daily_long[, variable := factor(variable, levels = variable_labels)]
figure_1 <- ggplot(daily_long, aes(date)) +
  geom_ribbon(aes(ymin = lowest, ymax = highest), fill = ramp_blue[2]) +
  geom_line(aes(y = mean), colour = ramp_blue[6], linewidth = 0.25) +
  facet_grid(variable ~ season, scales = "free", space = "free_x", switch = "y",
             labeller = labeller(variable = label_wrap_gen(28))) +
  scale_x_date(date_breaks = "1 month", labels = january_only, expand = expansion(add = 0.5)) +
  labs(x = NULL, y = NULL,
       caption = paste0("Line: mean of the 33 municipalities. Band: lowest and highest municipality of the day.\n",
                        "One column per warm season (1 November to 31 March); 2008/09 and 2020/21 are covered in part.")) +
  theme(strip.placement = "outside", strip.text.y.left = element_text(angle = 90, hjust = 0.5),
        strip.text.x = element_text(size = rel(0.7), hjust = 0.5), strip.clip = "off",
        axis.text.x = element_text(size = rel(0.75)), panel.spacing.x = grid::unit(1, "mm"))
save_figure(figure_1, "series_1_daily_temperature_ozone", 160)

# Figure 2. Municipalities by exposure category, day by day, one panel per warm season
# Table: series_daily_all_municipalities.csv
category_columns <- c(heatwave_and_high_ozone = "Heatwave and high ozone",
                      heatwave_only = "Heatwave only", high_ozone_only = "High ozone only")
compound_daily <- melt(daily, id.vars = c("date", "season"), measure.vars = names(category_columns),
                       variable.name = "category", value.name = "municipalities_in_category")
compound_daily[, category := factor(category_columns[as.character(category)],
                                    levels = rev(category_columns))]
# Position of the day inside its season, on a calendar that runs from 1 November
# to 31 March (2000 is a leap year, so every 29 February has its place)
compound_daily[, season_day := as.Date(sprintf("%d-%s", ifelse(month(date) >= 11, 1999L, 2000L),
                                               format(date, "%m-%d")))]
figure_2 <- ggplot(compound_daily, aes(season_day, municipalities_in_category, fill = category)) +
  geom_col(width = 1) +
  facet_wrap(~season, ncol = 3) +
  scale_fill_manual(values = colour_compound[category_columns], breaks = category_columns) +
  # Month names are written here: date formats follow the language of the computer
  scale_x_date(breaks = as.Date(c("1999-11-01", "1999-12-01", "2000-01-01", "2000-02-01", "2000-03-01")),
               labels = c("Nov", "Dec", "Jan", "Feb", "Mar"),
               limits = as.Date(c("1999-10-31", "2000-04-01")), expand = expansion(mult = 0.01)) +
  scale_y_continuous(limits = c(0, nrow(municipalities)), breaks = c(0, 11, 22, 33),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(x = NULL, y = "Municipalities (of 33)", fill = NULL,
       caption = sprintf("Heatwave: %s. High ozone: %s above the %sth percentile of the municipality.\n%s",
                         heatwave_label(heatwave_main), ozone_main,
                         get_param("models", "ozone_cutoff_main"),
                         "One panel per warm season; 2008/09 and 2020/21 are covered in part."))
save_figure(figure_2, "series_2_daily_compound_exposure", 210)

# Figure 3. Monthly means: every municipality and the mean of all
# Table: series_monthly_by_municipality.csv
monthly_long <- melt(monthly, id.vars = c("com", "municipality", "month_start", "season"),
                     measure.vars = names(variable_labels))
monthly_long[, variable := factor(variable_labels[as.character(variable)], levels = variable_labels)]
figure_3 <- ggplot(monthly_long[!is.na(com)], aes(month_start, value)) +
  geom_line(aes(group = interaction(com, season)), colour = colour_municipality, linewidth = 0.2) +
  geom_line(data = monthly_long[is.na(com)], aes(group = season), colour = colour_mean,
            linewidth = 0.6) +
  geom_point(data = monthly_long[is.na(com)], colour = colour_mean, size = 0.5) +
  facet_wrap(~variable, ncol = 1, scales = "free_y") +
  scale_x_date(breaks = year_breaks, date_labels = "%Y", expand = expansion(mult = 0.01)) +
  labs(x = NULL, y = NULL,
       caption = paste(note_lines, "Monthly means of the warm-season months (November to March)."))
save_figure(figure_3, "series_3_monthly_by_municipality", 170)

# Figure 4. Annual values: every municipality and the mean of all
# Table: series_annual_by_municipality.csv
annual_labels <- c(tmax = label_tmax, o3 = label_o3, mda8 = label_mda8,
                   heatwave_days = sprintf("Heatwave days (%s)", heatwave_label(heatwave_main)),
                   high_ozone_days = "High-ozone days",
                   compound_days = "Days with heatwave and high ozone")
annual_long <- melt(annual, id.vars = c("com", "municipality", "year"),
                    measure.vars = names(annual_labels))
annual_long[, variable := factor(annual_labels[as.character(variable)], levels = annual_labels)]
figure_4 <- ggplot(annual_long[!is.na(com)], aes(year, value)) +
  geom_line(aes(group = com), colour = colour_municipality, linewidth = 0.2) +
  geom_line(data = annual_long[is.na(com)], colour = colour_mean, linewidth = 0.6) +
  geom_point(data = annual_long[is.na(com)], colour = colour_mean, size = 0.6) +
  facet_wrap(~variable, ncol = 3, scales = "free") +
  scale_x_continuous(breaks = seq(2009, 2020, by = 3)) +
  labs(x = NULL, y = NULL,
       caption = paste0(note_lines, "\nCalendar years: January to March and November to December of each year."))
save_figure(figure_4, "series_4_annual_by_municipality", 125)

# Figures 5 to 7: one panel per municipality, in the order of the municipality
# codes, with the mean of all as the reference
municipality_order <- municipalities$name[order(municipalities$code)]
panel_theme <- theme(strip.text = element_text(size = 6.6),
                     axis.text = element_text(size = rel(0.8)),
                     panel.spacing = grid::unit(2.5, "mm"))
small_multiples <- function(variable, colour, y_label) {
  reference <- monthly[is.na(com), .(month_start, season, value = get(variable))]
  series <- monthly[!is.na(com), .(municipality = factor(municipality, levels = municipality_order),
                                   month_start, season, value = get(variable))]
  ggplot(series, aes(month_start, value, group = season)) +
    geom_line(data = reference, colour = ink$muted, linewidth = 0.25) +
    geom_line(colour = colour, linewidth = 0.35) +
    facet_wrap(~municipality, ncol = 6) +
    scale_x_date(breaks = as.Date(c("2010-01-01", "2015-01-01", "2020-01-01")), date_labels = "%Y") +
    labs(x = NULL, y = y_label,
         caption = paste("Coloured line: the municipality. Grey line: mean of the 33 municipalities.",
                         "Monthly means of the warm-season months.")) +
    panel_theme
}
# Table: series_monthly_by_municipality.csv
save_figure(small_multiples("tmax", ramp_orange[4], label_tmax), "series_5_municipalities_temperature", 200)
save_figure(small_multiples("o3", ramp_blue[4], label_o3), "series_6_municipalities_ozone", 200)

# Figure 7. Heatwave days by year in each municipality, split by ozone
# Table: series_annual_by_municipality.csv
heatwave_split <- c(compound_days = "Heatwave and high ozone", heatwave_only_days = "Heatwave only")
by_year <- melt(annual[!is.na(com)], id.vars = c("municipality", "year"),
                measure.vars = names(heatwave_split), variable.name = "category", value.name = "days")
by_year[, category := factor(heatwave_split[as.character(category)], levels = rev(heatwave_split))]
by_year[, municipality := factor(municipality, levels = municipality_order)]
figure_7 <- ggplot(by_year, aes(year, days)) +
  geom_col(aes(fill = category), width = 0.75) +
  geom_line(data = annual[is.na(com), .(year, days = heatwave_days)], colour = ink$primary,
            linewidth = 0.3) +
  facet_wrap(~municipality, ncol = 6) +
  scale_fill_manual(values = colour_compound[heatwave_split], breaks = heatwave_split) +
  scale_x_continuous(breaks = c(2010, 2015, 2020)) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(x = NULL, y = sprintf("Heatwave days per year (%s)", heatwave_label(heatwave_main)), fill = NULL,
       caption = "Bars: the municipality. Line: heatwave days per year, mean of the 33 municipalities.") +
  panel_theme
save_figure(figure_7, "series_7_municipalities_heatwave_days", 205)

message("Series written: ", nrow(daily), " days, ", n_months, " months, ", uniqueN(days$year),
        " years, ", nrow(municipalities), " municipalities")
