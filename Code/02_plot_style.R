# 02_plot_style.R
# Figure style shared by the reports: one theme, one set of colours.
#
# Colours are assigned by what they encode and stay with the same entity in
# every figure:
#   identity   fixed order (blue, orange, aqua, yellow), never recycled
#   magnitude  one hue from light to dark
#   polarity   blue and red around a neutral grey

library(ggplot2)

ink <- list(primary = "#0b0b0b", secondary = "#52514e", muted = "#898781",
            grid = "#e1e0d9", axis = "#c3c2b7", surface = "#fcfcfb", neutral = "#c3c2b7")

colour_method <- c("IDW" = "#2a78d6", "Ordinary kriging" = "#eb6834")
colour_series <- c("Delivered, 24-hour mean" = "#2a78d6",
                   "Rebuilt, 24-hour mean" = "#eb6834",
                   "Rebuilt, MDA8" = "#1baf7a")
colour_compound <- c("Neither" = "#c3c2b7", "High ozone only" = "#2a78d6",
                     "Heatwave only" = "#eb6834", "Heatwave and high ozone" = "#1baf7a")

ramp_blue <- c("#cde2fb", "#9ec5f4", "#6da7ec", "#3987e5", "#256abf", "#184f95", "#0d366b")
ramp_orange <- c("#fbe3d6", "#f7c4aa", "#f2a17b", "#eb6834", "#c4501f", "#983b13", "#6c2a0d")
diverging <- c(low = "#256abf", mid = "#f0efec", high = "#e34948")

theme_report <- function(base_size = 11) {
  theme_minimal(base_size = base_size) +
    theme(
      text = element_text(colour = ink$primary),
      plot.title = element_text(face = "bold", size = rel(1.05), hjust = 0,
                                margin = margin(b = 4)),
      plot.subtitle = element_text(colour = ink$secondary, hjust = 0, margin = margin(b = 8)),
      plot.caption = element_text(colour = ink$muted, hjust = 0, size = rel(0.8)),
      axis.text = element_text(colour = ink$secondary),
      axis.title = element_text(colour = ink$secondary, size = rel(0.9)),
      panel.grid.major = element_line(colour = ink$grid, linewidth = 0.3),
      panel.grid.minor = element_blank(),
      strip.text = element_text(colour = ink$primary, face = "bold", hjust = 0),
      legend.position = "top",
      legend.justification = "left",
      legend.title = element_text(colour = ink$secondary, size = rel(0.9)),
      plot.background = element_rect(fill = ink$surface, colour = NA),
      panel.background = element_rect(fill = ink$surface, colour = NA)
    )
}

# Map theme: no axes, legend below
theme_map <- function(base_size = 11) {
  theme_report(base_size) +
    theme(axis.text = element_blank(), axis.title = element_blank(),
          panel.grid.major = element_blank(), legend.position = "bottom",
          legend.justification = "center", legend.key.width = grid::unit(1.6, "cm"),
          legend.key.height = grid::unit(0.25, "cm"))
}

scale_fill_magnitude <- function(..., hue = c("blue", "orange")) {
  colours <- if (match.arg(hue) == "blue") ramp_blue else ramp_orange
  scale_fill_gradientn(colours = colours, ..., na.value = "#f0efec")
}

# ---- Labels shared by the reports and the publication figures ----------------

cause_labels <- c(
  non_accidental = "Non-accidental", cardiopulmonary = "Cardiopulmonary",
  cardiovascular = "Cardiovascular", ischemic_heart = "Ischemic heart disease",
  acute_myocardial_infarction = "Acute myocardial infarction",
  hypertensive = "Hypertensive diseases", cerebrovascular = "Cerebrovascular",
  ischemic_stroke = "Ischemic stroke", hemorrhagic_stroke = "Hemorrhagic stroke",
  respiratory = "Respiratory", copd = "COPD"
)

# Exposed categories of the compound exposure, in the order of the legends
category_labels <- c(e01 = "High ozone only", e10 = "Heatwave only",
                     e11 = "Heatwave and high ozone")
category_factor <- function(term) {
  stopifnot(all(term %in% names(category_labels)))
  factor(category_labels[term], levels = category_labels)
}

# "hw_p95_2d" -> "P95, 2 days"
heatwave_label <- function(definition) {
  parts <- data.table::tstrsplit(sub("^hw_", "", definition), "_", fixed = TRUE)
  threshold <- parts[[1]]
  threshold <- ifelse(grepl("^p[0-9]+$", threshold), toupper(threshold),
                      ifelse(threshold == "30c", "30 °C", toupper(threshold)))
  paste0(threshold, ", ", sub("d$", "", parts[[2]]), " days")
}

# Theme of the publication figures: the report theme on a white page
theme_publication <- function(base_size = 8) {
  theme_report(base_size) +
    theme(plot.background = element_rect(fill = "white", colour = NA),
          panel.background = element_rect(fill = "white", colour = NA),
          legend.key.size = grid::unit(3.5, "mm"), legend.margin = margin(0, 0, 0, 0),
          plot.margin = margin(4, 6, 4, 4))
}

# ---- Figure files -------------------------------------------------------------

# Write one figure as PDF (vector) and as PNG, the same drawing, in `folder`.
#   pdf_device: the device returned by pdf_figure_device() (Code/01_functions.R)
#   png_dpi: resolution of the PNG file
# Each file is drawn in a temporary folder and then copied: a device that fails
# to open only warns, and the drawing would go unnoticed to a file Rplots.pdf
# while the figure of an earlier run stayed in the folder.
write_figure <- function(plot, name, width_mm, height_mm, png_dpi, pdf_device, folder) {
  stopifnot(is.list(pdf_device), !is.null(pdf_device$device), dir.exists(folder),
            width_mm > 0, height_mm > 0, png_dpi > 0)
  devices <- list(pdf = pdf_device$device, png = ragg::agg_png)
  for (type in names(devices)) {
    drawn <- file.path(tempdir(), paste0(name, ".", type))
    unlink(drawn)
    if (type == "pdf") {
      ggsave(drawn, plot, width = width_mm, height = height_mm, units = "mm", device = devices[[type]])
    } else {
      ggsave(drawn, plot, width = width_mm, height = height_mm, units = "mm", dpi = png_dpi,
             device = devices[[type]])
    }
    if (!file.exists(drawn) || file.size(drawn) == 0) {
      stop("The ", type, " device did not write ", name, ".", type, call. = FALSE)
    }
    stopifnot(file.copy(drawn, file.path(folder, basename(drawn)), overwrite = TRUE))
    unlink(drawn)
  }
  message("Figure written: ", name)
}
