# Heatwaves, ozone and mortality in Santiago, Chile

Code for the study *Combined effect of extreme temperature and ozone on mortality: evidence from Chile*.

The study asks whether simultaneous exposure to heatwaves and elevated ambient ozone has a combined effect on mortality that is larger than the sum of their separate effects. It covers the urban municipalities of Santiago (Santiago Province plus Puente Alto) during the warm seasons, 1 November to 31 March, of 2009 to 2020.

Reports, once GitHub Pages is enabled for the `docs/` folder: <https://amarusimonaguerojimenez.github.io/Heatwave-ozone-mortality-Chile/>

## Design

- Space-time-stratified case-crossover: each death is compared with the other days of the same municipality, year, month and day of the week.
- Conditional logistic regression for heatwave only, ozone only and compound exposure.
- Additive interaction between heatwave and ozone (REOI, AP, S).
- Attributable mortality burden by municipality, year and age group.
- Sensitivity analyses: alternative heatwave and ozone definitions, adjustment for PM2.5, NO2 and humidity, all the lags in one model, distributed lag non-linear model of temperature.

## Status

| Deliverable | Content | State |
|---|---|---|
| 1 | Methodology, exposure dataset 2009-2020, data checks, maps and time series of every municipality | Done: scripts 10 to 13 and 18, page `docs/deliverable_1_exposure` |
| | Validation of the ozone exposure | Done: scripts 12, 14, 16 and 17, same page |
| 2 | Deaths by cause, age and sex; case-crossover dataset | Done: scripts 20 and 21, page `docs/deliverable_2_mortality` |
| 3 | Models, additive interaction, subgroups, attributable burden | Done with the working definitions: scripts 30 and 31, page `docs/deliverable_3_models` |
| 4 | Sensitivity analyses, distributed lag models, publication figures | Done with the working definitions: scripts 40, 41 and 50, page `docs/deliverable_4_sensitivity`. Final report pending |

Every result depends on working definitions that the principal investigator has still to confirm. They are listed below and marked `TO CONFIRM` in `config/config.yml`.

## Repository structure

```
Heatwave-ozone-mortality-Chile.Rproj   RStudio project: open this file to work on the project
_quarto.yml                            Quarto options shared by all reports
config/config.yml                      Study parameters, read by every script
config/municipalities.csv              The 33 municipalities: INE code and name
config/stations.csv                    The 11 ozone stations: SINCA code and name
config/local.example.yml               Template for machine-specific paths
Code/                                  R scripts, run in numerical order, and the two runners
Code/run_all.R                         Runs the whole R pipeline with one command
Code/run_python.py                     Renders the Python reports with one command
tests/test_methods.R                   Checks of the shared functions
Data/Input/                            Raw inputs (not in the repository)
Data/input_manifest.csv                Size and checksum of every input file
Data/Derived/                          Datasets created by the scripts (not in the repository)
Output/Tables/                         Result tables, tracked in the repository
Output/Figures/                        Figures written as files (PDF and PNG), tracked in the repository
Output/Models/                         Fitted models (not in the repository)
Output/run_environment.txt             Software of the machine that last ran the R pipeline
Output/run_environment_python.txt      Software of the machine that last rendered the Python reports
docs/                                  Reports: .qmd sources and rendered .html, served by GitHub Pages
renv.lock                              R version and package versions that produced the tracked results
requirements.txt                       Python packages of the Python reports, with their versions
```

## Scripts

| Script | Purpose |
|--------|---------|
| `00_packages.R` | The list of R packages of the project and the function that installs the missing ones. |
| `00_setup.R` | Packages, session settings, folders, study parameters and the check of each input against the manifest. Sourced by every script. |
| `01_functions.R` | Functions shared by the scripts. |
| `02_plot_style.R` | Theme and colours of the figures, and the function that writes a figure as PDF and PNG. |
| `03_input_manifest.R` | Checks every input against `Data/input_manifest.csv`; with `--write`, writes the manifest. |
| `04_software.R` | Installs the R packages that are missing, finds Quarto and records the versions of the machine. |
| `05_deis_extract.R` | Cuts the deaths extract from the public DEIS file. Not a step of the pipeline: it documents and checks one input. |
| `10_exposure_temperature.R` | The 15 heatwave indicators by municipality and day, from CR2MET. |
| `11_exposure_air_pollution.R` | Daily ozone, PM2.5, NO2 and humidity by municipality, from the delivered interpolated SINCA series. |
| `12_ozone_station_series.R` | Ozone rebuilt from the hourly SINCA records: 24-hour mean and MDA8 by station, interpolated to the municipalities. |
| `13_exposure_merge_qc.R` | Exposure dataset 2009-2020, alignment checks, ozone categories, compound exposure frequencies. |
| `14_interpolation_validation.R` | Leave-one-station-out cross-validation of IDW and ordinary kriging (the slow step). |
| `15_export_geodata.R` | Municipality polygons and station coordinates in open formats. |
| `16_reanalysis_comparison.R` | Ozone series against the CAMS EAC4 reanalysis. |
| `17_validation_summary.R` | Tables of the ozone validation: station inputs, cross-validation, delivered against rebuilt series. |
| `18_exposure_time_series.R` | Time series of the exposures over the whole study period, for every municipality and for their mean: three tables and seven figures (PDF and PNG). |
| `20_mortality_deis.R` | Deaths of residents of the study municipalities by cause, age and sex. |
| `21_case_crossover_dataset.R` | Case-crossover dataset and lagged exposures. |
| `30_models_main.R` | Conditional logistic regression, compound exposure and additive interaction, by cause, age and sex, and over the grid of definitions. |
| `31_attributable_burden.R` | Excess deaths attributable to heatwave only, high ozone only and both, by municipality, year and age group. |
| `40_sensitivity_analyses.R` | Adjustment for PM2.5, NO2, humidity and temperature; three ozone levels; alternative ozone series. |
| `41_distributed_lag_models.R` | All the lags in one model: exposure categories (also over the grid of definitions), temperature (dlnm) and ozone. |
| `50_figures_publication.R` | Publication figures in PDF and PNG, drawn from the result tables. |
| `run_all.R` | The R pipeline with one command: software, inputs, checks of the methods, every script in order, the R reports, and the comparison of the tables with those from before the run. |
| `run_python.py` | The Python reports with one command: prepares the Python packages and renders `docs/py_*.qmd`. Separate from the R pipeline, which does not need Python. |

## Reports

One page for each deliverable of the data analysis, with one section for each product that was requested, and three pages that repeat the analysis in Python.

| Report | Language | Content |
|--------|----------|---------|
| `docs/index` | | Overview and open decisions |
| `docs/deliverable_1_exposure` | R | Deliverable 1: methodology (sources of the data and definitions), verification and quality control of the exposure data, validation of the ozone exposure, exposure dataset 2009-2020, municipal maps, and time series of single and compound exposure for every municipality and for their mean |
| `docs/deliverable_2_mortality` | R | Deliverable 2: clean deaths dataset, mortality summary, case-crossover dataset with the exposure at lags 0 to 3 |
| `docs/deliverable_3_models` | R | Deliverable 3: main models, additive interaction, subgroups and attributable mortality burden (preliminary) |
| `docs/deliverable_4_sensitivity` | R | Deliverable 4: sensitivity analyses, all the lags in one model, publication figures (preliminary) |
| `docs/py_01_exposure` | Python | Independent rebuild of the exposure dataset, compared with R |
| `docs/py_02_models` | Python | Independent rebuild of the mortality selection and the models, compared with R |
| `docs/py_03_burden_sensitivity_lags` | Python | Independent rebuild of the burden, the adjusted models and the models with all the lags, compared with R |

All code is shown folded inside each report, and each section names the scripts and the files behind its tables and figures. Each table has a button that saves it as an Excel file and each figure a button that saves it as a PNG at 600 dpi; both are written by the browser from what the page holds, with no other program.

The files of `docs/` whose names start with `_` are shared by all the reports: `_nav.html` is the bar of links at the top, `_downloads.html` adds the download buttons, and `_prune_icons.ts` is run by Quarto after each render to remove from the page the icon classes it does not use.

## Getting started

Place the inputs under `Data/Input` as described in [Data/README.md](Data/README.md), or copy `config/local.example.yml` to `config/local.yml` and point it to where they are. Then two commands run everything. They are independent: the R pipeline does not need Python.

### The R pipeline

Open `Heatwave-ozone-mortality-Chile.Rproj` in RStudio and press Source on `Code/run_all.R`, or type `source("Code/run_all.R")`. From a terminal in the project folder the command is `Rscript Code/run_all.R`.

It needs R 4.3 or later and Quarto, which comes with RStudio. Everything else is done by the script, in this order:

1. It installs the R packages that are missing, from CRAN.
2. It checks every input against `Data/input_manifest.csv`.
3. It runs the checks of the methods.
4. It runs the scripts in numerical order, each in a fresh R session.
5. It renders the R reports.
6. It compares each table with its copy from before the run.

What every step prints is kept in `Data/Derived/run_all_log.txt`. A complete run takes about two hours on two cores; scripts 12, 14 and 30 (about 20, 70 and 15 minutes there) use up to eight cores, and on a laptop with eight it took 14 minutes. `options(mc.cores = 4)` or the environment variable `MC_CORES` sets another number.

No other program has to be installed. On macOS the PDF device of R that embeds the fonts (`cairo_pdf`) needs XQuartz; where it does not open, the PDF figures are drawn with the standard `pdf` device, and the PNG figures are the same either way.

| Option | Effect |
|---|---|
| `--from=30` | Starts at script 30, after a run that stopped there. The software, the inputs and the checks of the methods are always done. |
| `--no-install` | Stops if an R package is missing, instead of installing it. |

In RStudio the options are given before sourcing: `run_all_arguments <- "--from=30"; source("Code/run_all.R")`.

The scripts can also be run one by one from RStudio, in numerical order, and one report is rendered with `quarto render docs/deliverable_3_models.qmd`. On Windows the scripts run on one core, because R cannot fork processes there.

### The Python reports

`python3 Code/run_python.py`, from a terminal in the project folder, after the R pipeline: the Python reports compare their own results with the tables written by R.

It needs Python 3 and Quarto. If the Python that runs it has the packages of `requirements.txt` in those versions, it uses them. Otherwise it creates an environment for this project in the cache folder of the user, outside the project folder, installs the packages there, and renders the reports with it. With `--no-install` it installs nothing and stops if a package is missing. What it prints is kept in `Data/Derived/run_python_log.txt`.

To publish the reports, enable GitHub Pages for the `docs/` folder of the `main` branch in the repository settings.

## Study parameters and open decisions

All parameters are in `config/config.yml`; no script holds a parameter of its own. A parameter set to `null` stops any script that needs it. The working values still to be confirmed:

- Percentiles of temperature over all days of the year (reference code) or over the warm season only.
- Handling of 29 February, absent from the CR2MET municipal series.
- Temperature of the municipalities whose territory reaches into the Andes: the CR2MET municipal series describe the whole territory, and the rule with which they were built is to be confirmed.
- Ozone series for the main analysis: delivered 24-hour mean, 24-hour mean rebuilt from validated hourly records, or MDA8 (the metric named in the analysis plan).
- Use of validated SINCA records only.
- Interpolation method for the main analysis: inverse distance weighting or ordinary kriging.
- Percentile cut-offs of ozone computed within each municipality.
- Treatment of kriging values below zero.
- Headline heatwave definition and ozone cut-off of the models.
- Primary lag: the day of exposure (working value), the day after, or the four lags together.
- Reference category: every non-heatwave day at or below the ozone cut-off (working value), or ozone at or below the 50th percentile as worded in the plan.
- Scale of temperature in the continuous models: percentile within the municipality (working value) or degrees Celsius.
- Age groups of the rates per 100,000: the INE projections by municipality end in one group for 80 years or more, so the groups 65-84 and 85 or more of the plan have no denominator.
- Deaths coded as COVID-19 in 2020, outside non-accidental mortality as defined in the plan.

## Reproducibility

The project is built to give the same results on another computer, and to say so when it does not.

- **One folder, relative paths.** Scripts and reports find the project through the RStudio project file (`here::here()`). No script sets a working directory or holds a path of a particular computer; where the inputs are on a machine, if not under `Data/Input`, goes in `config/local.yml`, which is not tracked.
- **Parameters in one place.** Every parameter is in `config/config.yml`.
- **Checked inputs.** `Data/input_manifest.csv` gives the size and MD5 checksum of each input file. Every script and report checks the inputs it reads and stops if one is missing or is not the listed file.
- **Recorded software.** Each run writes the system, the versions of R, Quarto and the packages, and how they differ from `renv.lock`, to `Output/run_environment.txt`; the Python runner does the same in `Output/run_environment_python.txt`. They describe the machine that produced the tracked results. `renv.lock` holds the complete set of R packages of the first machine tested (R 4.3.3) and `requirements.txt` the Python versions. The runners install what is missing: `Code/run_all.R` the current CRAN version of an R package, `Code/run_python.py` the versions of `requirements.txt`. To work with exactly the R packages of `renv.lock`, run `renv::activate()` and then `renv::restore()`, which installs them in a library of the project.
- **Nothing left to the machine.** The collation is fixed, so text sorts in the same order under any language setting. Times are handled in UTC with the offset of the configuration. The analysis uses no random numbers; the checks of the methods do, with a fixed seed. The results do not depend on the number of cores; a parallel job that fails stops the run, and the jobs of a worker process that dies are run again. The linear algebra library is asked for one thread: with several, the worker processes can crash (they did on macOS with Apple's Accelerate). The 8-hour running sums are added window by window, so that they carry no rounding error that depends on the processor.
- **From the inputs, every time.** `Code/run_all.R` runs each script in a fresh R session, in numerical order. It compares each table with its copy from before the run: identical, equal within a relative tolerance of 1e-8 (`reproduction: tolerance`), or different. After a complete run it ends with an error if any table, figure, dataset or R report of the project was not written by the run.

**What has been tested.** Two machines have run the whole pipeline from the inputs.

| | First machine | Second machine |
|---|---|---|
| System | Linux, Intel-type processor (x86_64) | macOS, Apple silicon (arm64) |
| R and packages | R 4.3.3, the versions of `renv.lock` | R 4.5.2, the CRAN versions of November 2026 (`Output/run_environment.txt`) |
| Linear algebra library | reference BLAS | Apple's Accelerate |
| Worker processes, time | 2, 116 minutes | 8, 14 minutes |

On each machine a second run gives the results of the first. On the first machine, a copy of the project holding only the tracked files and the inputs rebuilt everything with `Code/run_all.R`: every dataset, table and PNG figure had the same bytes as in earlier builds made in another folder and under another language setting, and the PDF figures differed in their creation date only. Parts of the pipeline were repeated there with one worker process, one thread and another time zone, with identical tables. On the second machine, a second complete run gave the same 45 tables as the run before it, and 10 PNG figures with the same bytes.

Between the two machines, of the 45 tables:

- 15 have the same bytes;
- 17 are equal within the tolerance, the largest relative difference being 2e-11. They include the odds ratios and the interaction measures of the main models, the attributable deaths, the adjusted models and the models with all the lags;
- 13 differ beyond the tolerance, and only in the rows built on the ordinary kriging that this project computes: the rebuilt series `o3obs_ok` and `o3mda8_ok` (script 12) and the kriging predictions of the cross-validation (script 14). Every row built on the delivered series or on inverse distance weighting is within the tolerance.

Script 18 was added after these comparisons: its three tables and seven figures are not among the 45 tables and the 10 figures counted here.

**Ordinary kriging is not reproducible to the last digit between machines.** The interpolation of the data author chooses the variogram model of each day automatically. On some days it chooses a Gaussian or a smooth Matérn model without nugget; the kriging system is then close to singular, and its solution changes with rounding in the last digit, of the data or of the arithmetic. The two machines do not round in the same way: their processors and their linear algebra libraries differ. Their daily station values are the same except for 69 of 82,994, which differ by at most 1e-13 ppb, and yet the kriged municipal values differ by more than 1e-6 ppb on 37 days of each series (of 4,384), by up to 0.95 ppb; on 72 of those 74 days the station values are identical. Of the 34,655 kriging predictions of the cross-validation, 307 differ by more than 1e-6 ppb, by up to 4.0 ppb. Downstream, the odds ratios of the sensitivity models that use those two series change by up to 0.004, their percentile cut-offs by up to 0.02 ppb, and the error of the cross-validation in its fifth digit (RMSE of the 8-hour maximum 7.8934 and 7.8931 ppb). The package version is not the cause: on the first machine, versions 2.1-1 and 2.1-6 of gstat gave identical values on the 95 days tested, while reading the station values back from their file, which keeps 15 significant digits, was enough to change the kriged values of those days by up to 0.9 ppb. Inverse distance weighting gives the same values on both machines, within 1e-12 ppb, on every day.

The main analysis uses the delivered series interpolated by inverse distance weighting, and none of its results is affected.

The pipeline has not been run on Windows.

## Related repositories

- Heatwave indicators from CR2MET: <https://github.com/ClimChange-NewbornHealth/HeatWave_PretermBirth_Chile>
- Interpolation of SINCA air pollution monitors: <https://github.com/ClimChange-NewbornHealth/pollution-interpolation-kriging-idw>

## Team

Principal investigator: Estela Blanco, Pontificia Universidad Católica de Chile.
Data analysis: Amaru Agüero Jiménez.

Funded by ANID FONDECYT 11240322.
