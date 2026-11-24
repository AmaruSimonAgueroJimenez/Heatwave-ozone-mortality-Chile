# Heatwaves, ozone and mortality in Santiago, Chile

Code for the study *Combined effect of extreme temperature and ozone on mortality: evidence from Chile*.

The study asks whether simultaneous exposure to heatwaves and elevated ambient ozone has a combined effect on mortality that is larger than the sum of their separate effects. It covers the urban municipalities of Santiago (Santiago Province plus Puente Alto) during the warm seasons, 1 November to 31 March, of 2009 to 2020.

Reports, once GitHub Pages is enabled for the `docs/` folder: <https://amarusimonaguerojimenez.github.io/Heatwave-ozone-mortality-Chile/>

## Design

- Space-time-stratified case-crossover: each death is compared with the other days of the same municipality, year, month and day of the week.
- Conditional logistic regression for heatwave only, ozone only and compound exposure.
- Additive interaction between heatwave and ozone (REOI, AP, S).
- Sensitivity analyses: alternative heatwave and ozone definitions, adjustment for PM2.5 and NO2, non-linear lag structures.

## Status

| Deliverable | Content | State |
|---|---|---|
| 1 | Exposure dataset 2009-2020, data checks, maps and time trends | Done: scripts 10 to 13, reports 1 and 2 |
| | Validation of the ozone exposure | Done: scripts 12, 14, 16 and 17, report 3 |
| 2 | Deaths by cause, age and sex; case-crossover dataset | Done: scripts 20 and 21, report 4 |
| 3 | Models, additive interaction, subgroups | Preliminary: script 30, report 5. Attributable burden not started |
| 4 | Sensitivity analyses, publication figures, final report | Not started: scripts 40 and 41 are scaffolds |

Every result depends on working definitions that the principal investigator has still to confirm. They are listed below and marked `TO CONFIRM` in `config/config.yml`.

## Repository structure

```
Heatwave-ozone-mortality-Chile.Rproj   RStudio project: open this file to work on the project
_quarto.yml                            Quarto options shared by all reports
config/config.yml                      Study parameters, read by every script
config/municipalities.csv              The 33 municipalities: INE code and name
config/stations.csv                    The 11 ozone stations: SINCA code and name
config/local.example.yml               Template for machine-specific paths
Code/                                  R scripts, run in numerical order
tests/test_methods.R                   Checks of the shared functions
Data/Input/                            Raw inputs (not in the repository)
Data/Derived/                          Datasets created by the scripts (not in the repository)
Output/Tables/                         Result tables, tracked in the repository
Output/Figures/  Output/Models/        Figures and fitted models
docs/                                  Reports: .qmd sources and rendered .html, served by GitHub Pages
requirements.txt                       Python packages of the Python reports
```

## Scripts

| Script | Purpose |
|--------|---------|
| `00_setup.R` | Packages, folders and study parameters. Sourced by every script. |
| `01_functions.R` | Functions shared by the scripts. |
| `02_plot_style.R` | Theme and colours of the figures. |
| `10_exposure_temperature.R` | The 15 heatwave indicators by municipality and day, from CR2MET. |
| `11_exposure_air_pollution.R` | Daily ozone, PM2.5, NO2 and humidity by municipality, from the delivered interpolated SINCA series. |
| `12_ozone_station_series.R` | Ozone rebuilt from the hourly SINCA records: 24-hour mean and MDA8 by station, interpolated to the municipalities. |
| `13_exposure_merge_qc.R` | Exposure dataset 2009-2020, alignment checks, ozone categories, compound exposure frequencies. |
| `14_interpolation_validation.R` | Leave-one-station-out cross-validation of IDW and ordinary kriging (the slow step). |
| `15_export_geodata.R` | Municipality polygons and station coordinates in open formats. |
| `16_reanalysis_comparison.R` | Ozone series against the CAMS EAC4 reanalysis. |
| `17_validation_summary.R` | Tables of the ozone validation: station inputs, cross-validation, delivered against rebuilt series. |
| `20_mortality_deis.R` | Deaths of residents of the study municipalities by cause, age and sex. |
| `21_case_crossover_dataset.R` | Case-crossover dataset and lagged exposures. |
| `30_models_main.R` | Conditional logistic regression, compound exposure and additive interaction. |
| `40_sensitivity_analyses.R` | Scaffold. |
| `41_figures_publication.R` | Scaffold. |
| `run_all.R` | Runs everything in order, then the checks and the reports. |

## Reports

| Report | Language | Content |
|--------|----------|---------|
| `docs/index` | | Overview and open decisions |
| `docs/01_exposure_qc` | R | Verification and quality control of the exposure data |
| `docs/02_exposure_trends` | R | Maps and time trends of single and compound exposure |
| `docs/03_ozone_validation` | R | Validation of the ozone exposure |
| `docs/04_mortality_case_crossover` | R | Mortality and the case-crossover dataset |
| `docs/05_models` | R | Preliminary models |
| `docs/py_01_exposure` | Python | Independent rebuild of the exposure dataset, compared with R |
| `docs/py_02_models` | Python | Independent rebuild of the mortality selection and the models, compared with R |

All code is shown folded inside each report.

## Getting started

1. Open `Heatwave-ozone-mortality-Chile.Rproj` in RStudio.
2. Install the R packages: `install.packages(c("here", "yaml", "data.table", "sf", "sp", "gstat", "automap", "survival", "ncdf4", "ggplot2", "patchwork", "knitr", "kableExtra", "msm"))`.
3. Place the inputs as described in [Data/README.md](Data/README.md), or copy `config/local.example.yml` to `config/local.yml` and point it to where they are.
4. Run `Rscript Code/run_all.R`, or the scripts of `Code/` one by one in numerical order. Scripts 12 and 14 take about 20 and 75 minutes.
5. Render the reports with `quarto render`. Each `.qmd` in `docs/` produces an `.html` next to it. The Python reports need the packages of `requirements.txt`.

To publish the reports, enable GitHub Pages for the `docs/` folder of the `main` branch in the repository settings.

## Study parameters and open decisions

All parameters are in `config/config.yml`; no script holds a parameter of its own. A parameter set to `null` stops any script that needs it. The working values still to be confirmed:

- Percentiles of temperature over all days of the year (reference code) or over the warm season only.
- Handling of 29 February, absent from the CR2MET municipal series.
- Ozone series for the main analysis: delivered 24-hour mean, 24-hour mean rebuilt from validated hourly records, or MDA8 (the metric named in the analysis plan).
- Use of validated SINCA records only.
- Interpolation method for the main analysis: inverse distance weighting or ordinary kriging.
- Percentile cut-offs of ozone computed within each municipality.
- Treatment of kriging values below zero.
- Headline heatwave definition and ozone cut-off of the models.
- Deaths coded as COVID-19 in 2020, outside non-accidental mortality as defined in the plan.

## Reproducibility

Package versions are not pinned yet. Each report ends with the session information of the run that produced it. Run `renv::init()` to pin the R packages before the final analysis.

## Related repositories

- Heatwave indicators from CR2MET: <https://github.com/ClimChange-NewbornHealth/HeatWave_PretermBirth_Chile>
- Interpolation of SINCA air pollution monitors: <https://github.com/ClimChange-NewbornHealth/pollution-interpolation-kriging-idw>

## Team

Principal investigator: Estela Blanco, Pontificia Universidad Católica de Chile.
Data analysis: Amaru Agüero Jiménez.

Funded by ANID FONDECYT 11240322.
