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
