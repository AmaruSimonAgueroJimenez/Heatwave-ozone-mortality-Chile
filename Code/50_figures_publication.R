# 50_figures_publication.R
# Publication figures, each as PDF (vector) and PNG.
#
# Inputs
#   Output/Tables: models_main_or.csv, models_main_interaction.csv, models_grid.csv
#     (script 30), burden_estimates.csv (script 31), sensitivity_adjustment_or.csv
#     (script 40), lags_compound_or.csv, lags_grid.csv, lags_temperature_curve.csv
#     (script 41)
#   Data/Derived/municipality_polygons.geojson (script 15), for the map
# Outputs, in Output/Figures
#   figure_1_odds_ratios_by_lag          odds ratios of the compound categories by lag
#   figure_2_reoi_by_cause               forest plot of the REOI by cause of death
#   figure_3_interaction_matrix          REOI across heatwave and ozone definitions
#   figure_4_subgroups                   age groups and sex
#   figure_5_attributable_deaths         excess deaths by year and by municipality
#   figure_6_lag_structure               single-lag and distributed-lag estimates
#   figure_7_adjustment                  multi-pollutant adjustment
#   figure_s1_interaction_matrix_cardiopulmonary
#   figure_s2_interaction_matrix_all_lags   the same matrix from the model with all the lags
#   figure_s3_interaction_matrix_all_lags_temperature   and with temperature in that model
#
# Every figure is drawn from a saved table: no model is fitted here, and each
# number in a figure can be found in the table named in its block.

suppressPackageStartupMessages({
  library(sf)
  library(patchwork)
})
source(here::here("Code", "00_setup.R"))
source(here::here("Code", "01_functions.R"))
source(here::here("Code", "02_plot_style.R"))

png_dpi <- get_param("figures", "png_dpi")
width_double <- get_param("figures", "width_double_mm")
lag_main <- get_param("models", "lag_main")
heatwave_main <- get_param("models", "heatwave_main")
ozone_series_main <- paste0(get_param("air_pollution", "ozone_series_main"), "_",
                            get_param("air_pollution", "interpolation_method_main"))
cutoff_main <- get_param("models", "ozone_cutoff_main")
age_labels <- unlist(get_param("mortality", "age_labels"))

read_table <- function(file) {
  path <- file.path(dir_tables, file)
  if (!file.exists(path)) stop("Table not found: ", path, call. = FALSE)
  fread(path, encoding = "UTF-8")
}
as_numeric_columns <- function(dt, pattern) {
  columns <- grep(pattern, names(dt), value = TRUE)
  dt[, (columns) := lapply(.SD, as.numeric), .SDcols = columns]
  dt[]
}
# cairo_pdf where it works on this machine, otherwise the standard pdf device
# (see pdf_figure_device() in Code/01_functions.R)
pdf_device <- pdf_figure_device()
message("PDF figures: ", pdf_device$name, " device")
# write_figure() is in Code/02_plot_style.R
save_figure <- function(plot, name, width_mm, height_mm) {
  write_figure(plot, name, width_mm, height_mm, png_dpi = png_dpi, pdf_device = pdf_device,
               folder = dir_figures)
}
theme_set(theme_publication())
point_size <- 0.25
line_width <- 0.45
lag_facet <- function(lag) factor(paste("Lag", lag), levels = paste("Lag", sort(unique(lag))))

main_or <- read_table("models_main_or.csv")
main_interaction <- as_numeric_columns(read_table("models_main_interaction.csv"),
                                       "^(estimate|low|high)$")
grid <- as_numeric_columns(read_table("models_grid.csv"), "^(estimate|low|high|or)_")

# ---- Figure 1. Odds ratios by exposure category and lag ----------------------
# Table: models_main_or.csv, model "compound", all ages

figure_1_data <- main_or[subgroup == "all" & model == "compound" &
                           outcome %in% c("non_accidental", "cardiopulmonary")]
figure_1_data[, `:=`(category = category_factor(term),
                     cause = factor(cause_labels[outcome],
                                    levels = cause_labels[c("non_accidental", "cardiopulmonary")]))]
figure_1 <- ggplot(figure_1_data, aes(factor(lag), or, colour = category)) +
  geom_hline(yintercept = 1, colour = ink$axis, linewidth = 0.3) +
  geom_pointrange(aes(ymin = or_low, ymax = or_high), position = position_dodge(width = 0.55),
                  linewidth = line_width, size = point_size) +
  facet_wrap(~cause) +
  scale_colour_manual(values = colour_compound[levels(figure_1_data$category)]) +
  labs(x = "Lag (days)", y = "Odds ratio (95% CI)", colour = NULL)
save_figure(figure_1, "figure_1_odds_ratios_by_lag", width_double, 75)

# ---- Figure 2. REOI by cause of death ----------------------------------------
# Table: models_main_interaction.csv, measure "REOI", all ages

figure_2_data <- main_interaction[subgroup == "all" & measure == "REOI"]
figure_2_data[, `:=`(cause = factor(cause_labels[outcome], levels = rev(cause_labels)),
                     panel = lag_facet(lag))]
figure_2 <- ggplot(figure_2_data, aes(estimate, cause)) +
  geom_vline(xintercept = 0, colour = ink$axis, linewidth = 0.3) +
  geom_pointrange(aes(xmin = low, xmax = high), colour = ramp_blue[6], linewidth = line_width,
                  size = point_size) +
  facet_wrap(~panel, nrow = 1) +
  labs(x = "Relative excess odds due to interaction (95% CI)", y = NULL) +
  theme(panel.spacing.x = grid::unit(4, "mm"))
save_figure(figure_2, "figure_2_reoi_by_cause", width_double, 85)

# ---- Figure 3. Interaction matrix across definitions -------------------------
# Table: models_grid.csv, headline ozone series, every heatwave definition and cut-off

# `table` has one row per heatwave definition, ozone flag and lag, with the
# REOI and its interval; `lag_values` gives the panels in order
interaction_matrix <- function(table, outcome_value, lag_values, lag_labels) {
  d <- table[outcome == outcome_value & grepl(paste0("^", ozone_series_main, "_gt_p"), ozone)]
  stopifnot(nrow(d) > 0, all(d$lag %in% lag_values))
  d[, `:=`(cutoff = factor(paste0("> P", sub(".*_gt_p", "", ozone))),
           definition = factor(heatwave_label(heatwave),
                               levels = rev(heatwave_label(unique(table$heatwave)))),
           panel = factor(lag, levels = lag_values, labels = lag_labels))]
  d[, excludes_zero := estimable == TRUE & (low_reoi > 0 | high_reoi < 0)]
  d[, label := fifelse(estimable == TRUE,
                       paste0(formatC(round(estimate_reoi, 2) + 0, format = "f", digits = 2),
                              fifelse(excludes_zero, "*", "")), "n/e")]
  limit <- 0.15
  # White text on the cells at the dark ends of the colour scale
  d[, dark_cell := !is.na(estimate_reoi) & abs(estimate_reoi) > 0.8 * limit]
  ggplot(d, aes(cutoff, definition, fill = estimate_reoi)) +
    geom_tile(colour = "white", linewidth = 0.6) +
    geom_text(aes(label = label, colour = dark_cell), size = 1.9) +
    scale_colour_manual(values = c(`FALSE` = ink$primary, `TRUE` = "white"), guide = "none") +
    facet_wrap(~panel, nrow = 1) +
    scale_fill_gradient2(low = diverging[["low"]], mid = diverging[["mid"]],
                         high = diverging[["high"]], midpoint = 0, limits = c(-limit, limit),
                         oob = scales::squish, na.value = "#f0efec", name = "REOI") +
    labs(x = "Ozone cut-off (percentile of the municipality)",
         y = "Heatwave definition (threshold, minimum duration)",
         caption = paste0("* 95% confidence interval excludes zero. n/e: not estimable. ",
                          "Colour scale limited to \u00b1", limit, ".")) +
    theme(panel.grid.major = element_blank(), legend.position = "right",
          legend.justification = "center", legend.key.height = grid::unit(10, "mm"),
          legend.key.width = grid::unit(3, "mm"))
}
single_lags <- sort(unique(grid$lag))
save_figure(interaction_matrix(grid, "non_accidental", single_lags, paste("Lag", single_lags)),
            "figure_3_interaction_matrix", width_double, 120)
save_figure(interaction_matrix(grid, "cardiopulmonary", single_lags, paste("Lag", single_lags)),
            "figure_s1_interaction_matrix_cardiopulmonary", width_double, 120)
# Table: lags_grid.csv, the same definitions with all the lags in one model,
# without and with the temperature of every lag in the model
lags_grid <- as_numeric_columns(read_table("lags_grid.csv"), "^(estimate|low|high|or)_")
stopifnot(setequal(unique(lags_grid$adjustment), c("none", "temperature")))
all_lag_values <- c(single_lags, "cumulative")
all_lag_labels <- c(paste("Lag", single_lags), "Cumulative")
save_figure(interaction_matrix(lags_grid[adjustment == "none"], "non_accidental",
                               all_lag_values, all_lag_labels),
            "figure_s2_interaction_matrix_all_lags", width_double, 120)
save_figure(interaction_matrix(lags_grid[adjustment == "temperature"], "non_accidental",
                               all_lag_values, all_lag_labels),
            "figure_s3_interaction_matrix_all_lags_temperature", width_double, 120)

# ---- Figure 4. Age groups and sex ---------------------------------------------
# Tables: models_main_or.csv (E11) and models_main_interaction.csv (REOI),
# non-accidental mortality, headline lag

subgroup_labels <- c(all = "All", setNames(paste(age_labels, "years"), age_labels),
                     male = "Men", female = "Women")
figure_4_or <- main_or[outcome == "non_accidental" & model == "compound" & term == "e11" &
                         lag == lag_main,
                       .(subgroup, estimate = or, low = or_low, high = or_high,
                         panel = "Odds ratio, heatwave and high ozone", null = 1)]
figure_4_reoi <- main_interaction[outcome == "non_accidental" & measure == "REOI" & lag == lag_main,
                                  .(subgroup, estimate, low, high, panel = "REOI", null = 0)]
figure_4_data <- rbind(figure_4_or, figure_4_reoi)
figure_4_data[, `:=`(group = factor(subgroup_labels[subgroup], levels = rev(subgroup_labels)),
                     panel = factor(panel, levels = c("Odds ratio, heatwave and high ozone", "REOI")))]
figure_4 <- ggplot(figure_4_data, aes(estimate, group)) +
  geom_vline(aes(xintercept = null), colour = ink$axis, linewidth = 0.3) +
  geom_pointrange(aes(xmin = low, xmax = high), colour = ramp_blue[6], linewidth = line_width,
                  size = point_size) +
  facet_wrap(~panel, scales = "free_x") +
  labs(x = paste0("Estimate (95% CI), lag ", lag_main), y = NULL)
save_figure(figure_4, "figure_4_subgroups", width_double, 60)

# ---- Figure 5. Attributable deaths --------------------------------------------
# Table: burden_estimates.csv, non-accidental, headline lag, age-specific odds ratios

burden <- read_table("burden_estimates.csv")[outcome == "non_accidental" & lag == lag_main &
                                               or_source == "age-specific"]
by_year <- burden[level == "year" & category != "any"]
by_year[, category := category_factor(category)]
figure_5a <- ggplot(by_year, aes(year, excess, colour = category)) +
  geom_hline(yintercept = 0, colour = ink$axis, linewidth = 0.3) +
  geom_pointrange(aes(ymin = excess_low, ymax = excess_high),
                  position = position_dodge(width = 0.6), linewidth = line_width, size = point_size) +
  scale_colour_manual(values = colour_compound[levels(by_year$category)]) +
  scale_x_continuous(breaks = seq(min(by_year$year), max(by_year$year), by = 1)) +
  labs(x = NULL, y = "Excess deaths (95% CI)", colour = NULL, title = "A. By year") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))
polygons <- st_read(derived_path("municipality_polygons.geojson"), quiet = TRUE)
by_municipality <- burden[level == "municipality" & category == "any"]
map_data <- merge(polygons, by_municipality[, .(com, excess_per_100k)], by = "com")
stopifnot(nrow(map_data) == nrow(by_municipality))
figure_5b <- ggplot(map_data) +
  geom_sf(aes(fill = excess_per_100k), colour = "white", linewidth = 0.2) +
  scale_fill_magnitude(name = "Excess deaths per 100,000 inhabitants and year") +
  labs(title = "B. By municipality, any exposure") +
  theme_map(8) +
  theme(plot.background = element_rect(fill = "white", colour = NA),
        panel.background = element_rect(fill = "white", colour = NA),
        legend.title = element_text(size = 6.5), legend.position = "bottom",
        legend.key.width = grid::unit(9, "mm")) +
  guides(fill = guide_colourbar(title.position = "top"))
figure_5 <- figure_5a + figure_5b + plot_layout(widths = c(1.25, 1))
save_figure(figure_5, "figure_5_attributable_deaths", width_double, 85)

# ---- Figure 6. Lag structure ---------------------------------------------------
# Tables: models_main_or.csv (one lag per model), lags_compound_or.csv (all lags
# in one model) and lags_temperature_curve.csv

single <- main_or[outcome == "non_accidental" & subgroup == "all" & model == "compound",
                  .(category = term, lag = as.character(lag), or, or_low, or_high,
                    model = "One lag per model")]
together <- read_table("lags_compound_or.csv")[outcome == "non_accidental",
                                               .(category, lag, or, or_low, or_high,
                                                 model = "All lags in one model")]
figure_6_data <- rbind(single, together)
figure_6_data[, `:=`(category = category_factor(category),
                     lag = factor(lag, levels = c(sort(unique(single$lag)), "cumulative"),
                                  labels = c(sort(unique(single$lag)), "Cumulative")),
                     model = factor(model, levels = c("One lag per model", "All lags in one model")))]
colour_model <- c("One lag per model" = "#2a78d6", "All lags in one model" = "#eb6834")
figure_6a <- ggplot(figure_6_data, aes(lag, or, colour = model)) +
  geom_hline(yintercept = 1, colour = ink$axis, linewidth = 0.3) +
  geom_pointrange(aes(ymin = or_low, ymax = or_high), position = position_dodge(width = 0.5),
                  linewidth = line_width, size = point_size) +
  facet_wrap(~category, nrow = 1) +
  scale_colour_manual(values = colour_model) +
  labs(x = "Lag (days)", y = "Odds ratio (95% CI)", colour = NULL,
       title = "A. Exposure categories")
curve <- read_table("lags_temperature_curve.csv")[outcome == "non_accidental" &
                                                    model == "temperature"]
figure_6b <- ggplot(curve, aes(percentile, or)) +
  geom_hline(yintercept = 1, colour = ink$axis, linewidth = 0.3) +
  geom_ribbon(aes(ymin = or_low, ymax = or_high), fill = ramp_blue[2], alpha = 0.6) +
  geom_line(colour = ramp_blue[6], linewidth = 0.6) +
  scale_x_continuous(breaks = c(1, 25, 50, 75, 90, 99)) +
  labs(x = "Daily maximum temperature (percentile of the municipality, warm season)",
       y = paste0("Cumulative odds ratio, lags 0 to ", max(as.integer(single$lag))),
       title = "B. Temperature, all lags together")
figure_6 <- figure_6a / figure_6b + plot_layout(heights = c(1, 0.9))
save_figure(figure_6, "figure_6_lag_structure", width_double, 130)

# ---- Figure 7. Multi-pollutant adjustment --------------------------------------
# Table: sensitivity_adjustment_or.csv, non-accidental, headline lag

adjustment <- read_table("sensitivity_adjustment_or.csv")[outcome == "non_accidental" &
                                                            lag == lag_main &
                                                            term %in% names(category_labels)]
pollutant_labels <- c(pm25 = "PM2.5", no2 = "NO2", hum = "relative humidity",
                      temperature = "temperature")
adjustment_label <- function(x) {
  vapply(strsplit(x, " + ", fixed = TRUE), function(keys) {
    if (identical(keys, "none")) "No adjustment" else paste("+", paste(pollutant_labels[keys],
                                                                      collapse = ", "))
  }, character(1))
}
adjustment[, `:=`(category = category_factor(term),
                  adjusted_for = factor(adjustment_label(adjustment),
                                        levels = rev(adjustment_label(unique(adjustment)))))]
figure_7 <- ggplot(adjustment, aes(or, adjusted_for, colour = category)) +
  geom_vline(xintercept = 1, colour = ink$axis, linewidth = 0.3) +
  geom_pointrange(aes(xmin = or_low, xmax = or_high), linewidth = line_width, size = point_size) +
  facet_wrap(~category, nrow = 1) +
  scale_colour_manual(values = colour_compound[levels(adjustment$category)], guide = "none") +
  labs(x = paste0("Odds ratio (95% CI), lag ", lag_main), y = NULL)
save_figure(figure_7, "figure_7_adjustment", width_double, 65)

write_session_info(file.path(dir_figures, "figures_session_info.txt"))
