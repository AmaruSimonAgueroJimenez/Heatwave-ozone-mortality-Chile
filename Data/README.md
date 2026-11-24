# Data

The data are not part of the repository. `Data/Input` and `Data/Derived` are ignored by git.

## Data/Input

Raw inputs, exactly as received. Scripts only read from this folder; nothing here is edited or overwritten. The layout below is the default of `config/config.yml`; a machine that keeps the files elsewhere sets its own paths in `config/local.yml` (see `config/local.example.yml`).

| Path under `Data/Input` | Content | Source |
|-------------------------|---------|--------|
| `CR2MET/CR2MET_tmax_v2.5_day_COM_TS_1980_2021.csv` | Daily maximum temperature by municipality, 1980 to 2021 | CR2MET v2.5, Center for Climate and Resilience Research (CR)2 |
| `CR2MET/CR2MET_tmin_v2.5_day_COM_TS_1980_2021.csv` | Daily minimum temperature by municipality, 1980 to 2021 | CR2MET v2.5, Center for Climate and Resilience Research (CR)2 |
| `SINCA/1_04_daily_interpolated_opr.csv` | Daily ozone, PM2.5, PM10, NO2, humidity, temperature and wind interpolated to 33 municipalities; values within each monitor's operational period | Project team, from SINCA monitors |
| `SINCA/1_04_daily_interpolated_all.csv` | Same series, also with values modelled outside each monitor's operational period | Project team, from SINCA monitors |
| `SINCA_interpolation/imputed_series.RData` | Station series and station coordinates used by the interpolation, 2009 to 2020 | <https://github.com/ClimChange-NewbornHealth/pollution-interpolation-kriging-idw> |
| `SINCA_interpolation/interpolation_locations.RData` | One target point per municipality | Same repository |
| `SINCA_interpolation/municipalities_shape.RData` | Polygons of the 33 municipalities | Same repository |
| `SINCA_hourly/o3/horario/RM_<station>_horario_2000-01-01_2026-09-03.csv` | Hourly ozone of the 11 stations listed in `config/stations.csv`, with the validation status of each record | SINCA, Ministry of the Environment, <https://sinca.mma.gob.cl> |
| `CAMS_EAC4/cams_eac4_<yyyymm>_chile_pixeles.nc` | CAMS global reanalysis (EAC4), 3-hourly ozone by native cell over Chile, December 2008 to December 2020 | Copernicus Atmosphere Monitoring Service |
| `DEIS/defunciones_RM_2009_2020_extracto.csv` | Deaths of residents of the Metropolitan Region, 2009 to 2020: rows of the official file `DEFUNCIONES_FUENTE_DEIS_1990_2023_CIFRAS_OFICIALES.csv` with all its columns | DEIS, Ministry of Health of Chile, open data |

The official DEIS file can be used in place of the extract: script 20 selects the municipalities and years it needs.

A script stops with the missing path if an input is not found.

## Data/Derived

Datasets created by the scripts in `Code/`. They can be deleted and rebuilt from the inputs at any time.

| File | Script | Content |
|------|--------|---------|
| `exposure_temperature.csv` | 10 | Temperature and heatwave indicators by municipality and day |
| `exposure_air_pollution.csv`, `exposure_air_pollution_all.csv` | 11 | Delivered interpolated series, versions `opr` and `all` |
| `ozone_station_daily.csv` | 12 | 24-hour mean and MDA8 of ozone by station and day |
| `ozone_rebuilt_municipal.csv` | 12 | Ozone rebuilt from validated hourly records, by municipality and day |
| `exposure_2009_2020.csv` | 13 | The exposure dataset |
| `validation_loso_predictions.csv` | 14 | Leave-one-station-out predictions |
| `municipality_polygons.geojson`, `interpolation_points.csv`, `ozone_station_points.csv` | 15 | Geometries in open formats |
| `cams_ozone_daily.csv`, `cams_city_daily.csv` | 16 | CAMS ozone by cell and day, and city-wide daily series |
| `deaths_clean.csv` | 20 | One row per death, with cause, age and sex groups |
| `case_crossover.csv`, `exposure_lagged.csv` | 21 | Case-crossover dataset and exposures at lags 0 to 3 |
| `py_exposure_2009_2020.csv` | `docs/py_01_exposure.qmd` | The exposure dataset rebuilt in Python |
