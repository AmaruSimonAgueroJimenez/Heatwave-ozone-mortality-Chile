# Data

The data are not part of the repository. `Data/Input` and `Data/Derived` are ignored by git; `Data/input_manifest.csv` is tracked.

## Data/Input

Raw inputs, exactly as received. Scripts only read from this folder; nothing here is edited or overwritten. The layout below is the default of `config/config.yml`; a machine that keeps the files elsewhere sets its own paths in `config/local.yml` (see `config/local.example.yml`).

| Path under `Data/Input` | Content | Source |
|-------------------------|---------|--------|
| `CR2MET/CR2MET_tmax_v2.5_day_COM_TS_1980_2021.csv` | Daily maximum temperature by municipality, 1980 to 2021 | CR2MET v2.5, Center for Climate and Resilience Research (CR)2 |
| `CR2MET/CR2MET_tmin_v2.5_day_COM_TS_1980_2021.csv` | Daily minimum temperature by municipality, 1980 to 2021 | CR2MET v2.5, Center for Climate and Resilience Research (CR)2 |
| `SINCA/1_04_daily_interpolated_opr.csv` | Daily ozone, PM2.5, PM10, NO2, humidity, temperature and wind interpolated to 33 municipalities; values within each monitor's operational period | Project team, from SINCA monitors; version received on 16 November 2026 |
| `SINCA/1_04_daily_interpolated_all.csv` | Same series, also with values modelled outside each monitor's operational period | Project team, from SINCA monitors; version received on 16 November 2026 |
| `SINCA_interpolation/imputed_series.RData` | Station series and station coordinates used by the interpolation, 2009 to 2020 | <https://github.com/ClimChange-NewbornHealth/pollution-interpolation-kriging-idw>, folder `Data`, commit `da97422` |
| `SINCA_interpolation/interpolation_locations.RData` | One target point per municipality | Same repository and commit |
| `SINCA_interpolation/municipalities_shape.RData` | Polygons of the 33 municipalities | Same repository and commit |
| `SINCA_hourly/o3/horario/RM_<station>_horario_2000-01-01_2026-09-03.csv` | Hourly ozone of the 11 stations listed in `config/stations.csv`, with the validation status of each record | SINCA, Ministry of the Environment, <https://sinca.mma.gob.cl>: hourly records of each station from 1 January 2000 to 3 September 2026 |
| `CAMS_EAC4/cams_eac4_<yyyymm>_chile_pixeles.nc` | CAMS global reanalysis (EAC4), one file per month from January 2009 to January 2021 | Copernicus Atmosphere Monitoring Service; see below |
| `DEIS/defunciones_RM_2009_2020_extracto.csv` | Deaths of residents of the Metropolitan Region, 2009 to 2020 | Cut from the public DEIS file by `Code/05_deis_extract.R`; see below |
| `INE/censo_proyecciones_ano_edad_genero.parquet` | Population projections 2002 to 2035 by municipality, sex and single year of age (0 to 79, and one group for 80 or more), long format | INE, estimates and projections of population by municipality, base 2017 |

### The inputs are checked

`Data/input_manifest.csv` lists each input file with its size and MD5 checksum: 165 files of the eleven inputs above, 843 MB, and the public DEIS file. Every script checks the inputs it reads against the manifest and stops if a file is missing or is not the listed one, so a result cannot come unnoticed from another version of a file. `Rscript Code/03_input_manifest.R` checks all the inputs at once.

After a deliberate change of input, for example a new delivery of the interpolated series, `Rscript Code/03_input_manifest.R --write` writes a new manifest. Commit it together with the results that the new input produces.

### Notes on some inputs

- **Deaths.** The extract holds the header of the public file `DEFUNCIONES_FUENTE_DEIS_1990_2023_CIFRAS_OFICIALES.csv` (open data of DEIS, Ministry of Health of Chile; 870 MB, 3,182,446 deaths) followed by its 473,568 rows with year of death 2009 to 2020 and region of residence Metropolitana de Santiago, in their original order, with all the columns and the original bytes (Latin-1, semicolons). `Code/05_deis_extract.R` cuts it and checks it; the rule is in `config/config.yml` under `deis_extract`. The public file is not needed to run the analysis: to use it, place it under `Data/Input/DEIS` or give its path as `deis_official` in `config/local.yml`.
- **Hourly ozone.** SINCA keeps validating records after they are published, so files downloaded later can differ from these. The folder may hold other pollutants and stations: the scripts read only the 11 files named by `station_series: hourly_file_pattern`.
- **CAMS reanalysis.** The files hold the 268 native cells of 0.75 degrees that intersect Chile, at the native 3-hourly UTC time steps, with no resampling. The variable used is `o3`: ozone mass mixing ratio of the full chemistry scheme (`go3`, GRIB parameter 210203), in kg/kg, analysis, on a model level that the files do not name. A local day ends three hours into the next UTC day, so the months needed for 2009 to 2020 run from January 2009 to January 2021; the scripts read exactly those 145 files, whatever else the folder holds.
- **Population.** A long-format copy of the INE projections, with columns `cut_region`, `region`, `cut_provincia`, `provincia`, `cut_comuna`, `comuna`, `edad`, `año`, `población` and `género`.
- **Interpolated series and CR2MET.** Delivered by the project team. The checksum of the manifest is what identifies the version in use.

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
| `cams_ozone_daily.csv`, `cams_city_daily.csv` | 16 | CAMS ozone by cell and day of the study years, and city-wide daily series |
| `deaths_clean.csv` | 20 | One row per death, with cause, age and sex groups |
| `case_crossover.csv`, `exposure_lagged.csv` | 21 | Case-crossover dataset and exposures at lags 0 to 3 |
| `py_exposure_2009_2020.csv` | `docs/py_01_exposure.qmd` | The exposure dataset rebuilt in Python |
| `reproduction_check.csv` | `run_all.R` | Each table of `Output/Tables` against its copy from before the run |
| `run_all_log.txt`, `run_python_log.txt` | `run_all.R`, `run_python.py` | What every step of the last run printed |

Scripts 18 and 30 to 50 write no dataset: their results are the tables of `Output/Tables` and the figures of `Output/Figures`, which are tracked in the repository.
