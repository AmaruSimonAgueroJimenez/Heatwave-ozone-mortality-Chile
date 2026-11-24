# 15_export_geodata.R
# Geometries in open formats, for the Python reports and for any other
# software: the R inputs hold them as sf objects inside .RData files.
#
# Inputs
#   inputs$municipality_shapes       polygons of the 33 municipalities
#   inputs$interpolation_locations   one target point per municipality
#   inputs$station_series            coordinates of the ozone stations
# Outputs
#   Data/Derived/municipality_polygons.geojson   WGS 84, with the INE code
#   Data/Derived/interpolation_points.csv        target points, UTM 19S (metres)
#   Data/Derived/ozone_station_points.csv        stations, UTM 19S (metres)

suppressPackageStartupMessages(library(sf))
source(here::here("Code", "00_setup.R"))

municipalities <- study_municipalities()
epsg <- get_param("interpolation", "crs_epsg")
author <- new.env()
load(input_path("municipality_shapes"), envir = author)
load(input_path("interpolation_locations"), envir = author)
load(input_path("station_series"), envir = author)

polygons <- author$comunas_santiago[, c("cod_comuna", "comuna")]
names(polygons)[1:2] <- c("com", "municipality")
polygons$com <- as.integer(polygons$com)
stopifnot(setequal(polygons$com, municipalities$code), sf::st_crs(polygons)$epsg == 4326)
geojson <- file.path(dir_derived, "municipality_polygons.geojson")
if (file.exists(geojson)) file.remove(geojson)
sf::st_write(polygons, geojson, quiet = TRUE)

stopifnot(sf::st_crs(author$interpol)$epsg == epsg, sf::st_crs(author$imputed_series)$epsg == epsg)
targets <- data.table(municipality = author$interpol$comuna, sf::st_coordinates(author$interpol))
targets <- merge(municipalities, targets, by.x = "name", by.y = "municipality")
setnames(targets, c("name", "code", "X", "Y"), c("municipality", "com", "x", "y"))
fwrite(targets[order(com), .(com, municipality, x, y)],
       file.path(dir_derived, "interpolation_points.csv"))

station_codes <- fread(here::here(get_param("station_series", "stations_file")), encoding = "UTF-8")
first <- author$imputed_series[!duplicated(author$imputed_series$station), "station"]
stations <- data.table(station = first$station, sf::st_coordinates(first))
stations <- merge(station_codes, stations, by = "station")
setnames(stations, c("X", "Y"), c("x", "y"))
stopifnot(nrow(stations) == nrow(station_codes))
fwrite(stations, file.path(dir_derived, "ozone_station_points.csv"))

message("Geometries written to Data/Derived")
