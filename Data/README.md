# Data

The data are not part of the repository. `Data/Input` and `Data/Derived` are ignored by git.

## Data/Input

Raw inputs, exactly as received. Scripts only read from this folder; nothing here is edited or overwritten.

| Folder | File | Content | Source |
|--------|------|---------|--------|
| `CR2MET/` | `CR2MET_tmax_v2.5_day_COM_TS_1980_2021.csv` | Daily maximum temperature by municipality, 1980 to 2021 | CR2MET v2.5, Center for Climate and Resilience Research (CR)2 |
| `CR2MET/` | `CR2MET_tmin_v2.5_day_COM_TS_1980_2021.csv` | Daily minimum temperature by municipality, 1980 to 2021 | CR2MET v2.5, Center for Climate and Resilience Research (CR)2 |
| `SINCA/` | `1_04_daily_interpolated_opr.csv` | Daily O3, PM2.5, PM10 and NO2 interpolated to 33 municipalities; monitor values within each monitor's operational period | Project team, from SINCA monitors |
| `SINCA/` | `1_04_daily_interpolated_all.csv` | Same series, also with values modelled outside each monitor's operational period | Project team, from SINCA monitors |
| `DEIS/` | to be added | Deaths database (DEFUNCIONES), 2009 to 2020 | DEIS, Ministry of Health of Chile |

File names are registered in `config/config.yml` under `inputs`. A script stops with the missing path if a file is not found.

## Data/Derived

Datasets created by the scripts in `Code/`. They can be deleted and rebuilt from `Data/Input` at any time.
