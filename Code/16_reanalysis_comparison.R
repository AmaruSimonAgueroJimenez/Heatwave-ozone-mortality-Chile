# 16_reanalysis_comparison.R
# Ozone of the municipal series against the CAMS global reanalysis (EAC4).
#
# CAMS EAC4 assimilates satellite retrievals of ozone into a global chemistry
# model. Its cells measure 0.75 degrees (about 70 by 83 km here), so the whole
# study area falls in two cells: the reanalysis cannot say anything about
# differences between municipalities. It is an independent source for the
# day-to-day course of ozone over the city, which is what this script uses.
#
# Inputs
#   inputs$cams_eac4_dir                    monthly files of 3-hourly values by cell;
#                                           the months read are those of cams_month_files()
#   Data/Derived/interpolation_points.csv   (script 15)
#   Data/Derived/exposure_2009_2020.csv     (script 13)
# Outputs
#   Data/Derived/cams_ozone_daily.csv            cell-day: daily mean and afternoon value (ppb)
#   Output/Tables/validation_cams_cells.csv      cells used and municipalities in each
#   Output/Tables/validation_cams_correlation.csv
#
# Correlations are computed for the city-wide daily mean (the mean of the 33
# municipalities), on the warm-season days of the study years, in two forms:
# on the daily values, and on the anomalies from the mean of each calendar
# month, which removes the seasonal cycle shared by any two ozone series.

suppressPackageStartupMessages({
  library(sf)
  library(ncdf4)
})
source(here::here("Code", "00_setup.R"))
source(here::here("Code", "01_functions.R"))

rules <- get_param("reanalysis")
study_years <- get_param("study", "years")
warm_months <- get_param("study", "warm_season_months")
# Mass mixing ratio (kg/kg) to volume mixing ratio (ppb): molar masses of dry
# air (28.9644 g/mol) and ozone (47.9982 g/mol)
kgkg_to_ppb <- 28.9644 / 47.9982 * 1e9

# ---- 1. Cells that contain the municipal target points ----------------------

targets <- fread(derived_path("interpolation_points.csv"), encoding = "UTF-8")
lonlat <- sf::st_coordinates(sf::st_transform(
  sf::st_as_sf(targets, coords = c("x", "y"), crs = get_param("interpolation", "crs_epsg")), 4326
))
targets[, `:=`(lon = lonlat[, 1], lat = lonlat[, 2])]

# The months that hold every record of the local days of the study years,
# whatever else the folder contains
files <- file.path(input_path("cams_eac4_dir"), cams_month_files())
stopifnot(length(files) > 0, all(file.exists(files)))
first <- ncdf4::nc_open(files[1])
cells <- data.table(pixel = seq_along(ncdf4::ncvar_get(first, "pixel_id")),
                    pixel_id = as.vector(ncdf4::ncvar_get(first, "pixel_id")),
                    cell_lat = as.vector(ncdf4::ncvar_get(first, "latitude")),
                    cell_lon = as.vector(ncdf4::ncvar_get(first, "longitude")))
stopifnot(ncdf4::ncatt_get(first, "o3", "units")$value == "kg kg**-1")
ncdf4::nc_close(first)

half <- rules$cell_size_degrees / 2
targets[, pixel_id := vapply(seq_len(.N), function(i) {
  hit <- cells[abs(cell_lat - lat[i]) <= half & abs(cell_lon - lon[i]) <= half]
  stopifnot(nrow(hit) == 1)
  hit$pixel_id
}, numeric(1))]
used <- cells[pixel_id %in% targets$pixel_id]
fwrite(merge(used[, .(pixel_id, cell_lat, cell_lon)],
             targets[, .(municipalities = .N, names = paste(municipality, collapse = "; ")), by = pixel_id],
             by = "pixel_id"),
       file.path(dir_tables, "validation_cams_cells.csv"))

# ---- 2. Daily values by cell, in local time ---------------------------------

read_month <- function(file) {
  nc <- ncdf4::nc_open(file)
  on.exit(ncdf4::nc_close(nc))
  ids <- as.vector(ncdf4::ncvar_get(nc, "pixel_id"))
  stopifnot(identical(ids, cells$pixel_id))
  time_units <- ncdf4::ncatt_get(nc, "time", "units")$value
  origin <- as.POSIXct(sub("^[a-z]+ since ", "", time_units), tz = "UTC")
  step <- switch(sub(" since.*$", "", time_units), hours = 3600, seconds = 1, days = 86400)
  stopifnot(!is.null(step))
  utc <- origin + ncdf4::ncvar_get(nc, "time") * step
  ozone <- ncdf4::ncvar_get(nc, "o3")                    # pixel x time
  stopifnot(identical(dim(ozone), c(length(ids), length(utc))))
  rbindlist(lapply(used$pixel, function(p) {
    data.table(pixel_id = ids[p], utc = utc, o3_ppb = ozone[p, ] * kgkg_to_ppb)
  }))
}
cams <- rbindlist(lapply(files, read_month))
stopifnot(anyDuplicated(cams[, .(pixel_id, utc)]) == 0, !anyNA(cams$o3_ppb))
cams[, local := utc + rules$utc_offset_hours * 3600]
cams[, `:=`(date = as.Date(format(local, "%Y-%m-%d", tz = "UTC")),
            hour = as.integer(format(local, "%H", tz = "UTC")))]
afternoon <- unlist(rules$afternoon_local_hours)
stopifnot(all(afternoon %in% unique(cams$hour)))
cams_daily <- cams[, .(
  records = .N,
  cams_mean24 = mean(o3_ppb),
  cams_afternoon = mean(o3_ppb[hour %in% afternoon])
), by = .(pixel_id, date)]
# Local days of the study years: each needs its eight 3-hourly records, so
# that no day drops out of the comparison unnoticed
cams_daily <- cams_daily[date %in% study_dates()]
setorder(cams_daily, pixel_id, date)
stopifnot(nrow(cams_daily) == nrow(used) * length(study_dates()),
          all(cams_daily$records == 8))
fwrite(cams_daily, file.path(dir_derived, "cams_ozone_daily.csv"))

# ---- 3. City-wide daily series and correlations -----------------------------

exposure <- fread(derived_path("exposure_2009_2020.csv"), encoding = "UTF-8")
exposure[, date := as.Date(date)]
exposure <- exposure[in_study_period == TRUE & warm_season == TRUE]
exposure <- merge(exposure, targets[, .(com, pixel_id)], by = "com")
exposure <- merge(exposure, cams_daily, by = c("pixel_id", "date"))
stopifnot(uniqueN(exposure$date) == uniqueN(cams_daily[
  year(date) >= study_years[1] & year(date) <= study_years[2] & month(date) %in% warm_months, date]))

pairs <- data.table(
  series = c("o3_idw", "o3_ok", "o3obs_idw", "o3obs_ok", "o3mda8_idw", "o3mda8_ok"),
  cams = c(rep("cams_mean24", 4), rep("cams_afternoon", 2))
)
city <- exposure[, c(lapply(.SD, mean, na.rm = TRUE)), by = date,
                 .SDcols = c(pairs$series, "cams_mean24", "cams_afternoon")]
city[, month_id := format(date, "%Y-%m")]
correlations <- rbindlist(lapply(seq_len(nrow(pairs)), function(i) {
  d <- city[, .(date, month_id, x = get(pairs$series[i]), y = get(pairs$cams[i]))][!is.na(x)]
  d[, `:=`(x_anomaly = x - mean(x), y_anomaly = y - mean(y)), by = month_id]
  data.table(series = pairs$series[i], cams = pairs$cams[i], days = nrow(d),
             mean_series = mean(d$x), mean_cams = mean(d$y),
             correlation_daily = cor(d$x, d$y),
             correlation_anomalies = cor(d$x_anomaly, d$y_anomaly))
}))
fwrite(correlations, file.path(dir_tables, "validation_cams_correlation.csv"))
fwrite(city[, !"month_id"], file.path(dir_derived, "cams_city_daily.csv"))
print(correlations)
