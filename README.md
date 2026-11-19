# Heatwaves, ozone and mortality in Santiago, Chile

Code for the study *Combined effect of extreme temperature and ozone on mortality: evidence from Chile*.

The study asks whether simultaneous exposure to heatwaves and elevated ambient ozone has a combined effect on mortality that is larger than the sum of their separate effects. It covers the urban municipalities of Santiago (Santiago Province plus Puente Alto) during the warm seasons, 1 November to 31 March, of 2009 to 2020.

## Design

- Space-time-stratified case-crossover: each death is compared with the other days in the same municipality, year, month and day of the week.
- Conditional logistic regression for heatwave only, ozone only and compound exposure.
- Additive interaction between heatwave and ozone.
- Sensitivity analyses: alternative heatwave definitions, adjustment for PM2.5 and NO2, non-linear lag structures.

## Repository structure

```
Heatwave-ozone-mortality-Chile.Rproj   RStudio project: open this file to work on the project
_quarto.yml                            Quarto options shared by all reports
config/config.yml                      Study parameters, read by every script
Code/                                  R scripts, run in numerical order
Data/Input/                            Raw inputs (not in the repository)
Data/Derived/                          Datasets created by the scripts (not in the repository)
Output/Figures/  Output/Tables/        Results tracked in the repository
Output/Models/                         Fitted model objects (not in the repository)
docs/                                  Reports: .qmd sources and rendered .html, served by GitHub Pages
```

## Scripts

| Script | Stage | Purpose |
|--------|-------|---------|
| `00_setup.R` | | Packages, folders and study parameters. Sourced by every script. |
| `01_functions.R` | | Functions shared by more than one script. |
| `10_exposure_temperature.R` | 1 | Daily heatwave indicators by municipality from CR2MET. |
| `11_exposure_air_pollution.R` | 1 | Daily ozone, PM2.5 and NO2 by municipality from SINCA. |
| `12_exposure_merge_qc.R` | 1 | Exposure dataset, quality control, maps and time series. |
| `20_mortality_deis.R` | 2 | Cleaning and classification of DEIS deaths. |
| `21_case_crossover_dataset.R` | 2 | Analytical case-crossover dataset. |
| `30_models_main.R` | 3 | Main models, additive interaction and attributable burden. |
| `40_sensitivity_analyses.R` | 4 | Sensitivity analyses. |
| `41_figures_publication.R` | 4 | Publication figures. |

Scripts `10` to `41` are scaffolds: each one states its inputs, outputs and steps and stops with a message until its code is written.

## Getting started

1. Open `Heatwave-ozone-mortality-Chile.Rproj` in RStudio.
2. Install the packages used by the setup: `install.packages(c("here", "yaml"))`.
3. Copy the input files into `Data/Input` as described in [Data/README.md](Data/README.md).
4. Run the scripts in `Code/` in numerical order.
5. Render the reports from the project folder with `quarto render`. Each `.qmd` in `docs/` produces an `.html` next to it.

To publish the reports, enable GitHub Pages for the `docs/` folder of the `main` branch in the repository settings.

## Study parameters

All parameters are in `config/config.yml`. A parameter set to `null` has not been decided, and any script that needs it stops until a value is written there. Pending decisions:

- Whether temperature percentiles are computed over the whole year or the warm season only.
- Interpolation method for air pollution: inverse distance weighting or ordinary kriging.
- Minimum number of monitors behind a municipality-day.
- Definition of elevated ozone.
- ICD-10 codes for non-accidental and cardiopulmonary causes of death.
- File name of the DEIS deaths database.

## Reproducibility

Package versions are not pinned yet. Run `renv::init()` once the first analysis scripts are written and commit the resulting `renv.lock`.

## Related repositories

- Heatwave indicators from CR2MET: <https://github.com/ClimChange-NewbornHealth/HeatWave_PretermBirth_Chile>
- Interpolation of SINCA air pollution monitors: <https://github.com/ClimChange-NewbornHealth/pollution-interpolation-kriging-idw>

## Team

Principal investigator: Estela Blanco, Pontificia Universidad Católica de Chile.
Data analysis: Amaru Agüero Jiménez.

Funded by ANID FONDECYT 11240322.
