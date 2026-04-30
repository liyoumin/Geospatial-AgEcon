# Geospatial-AgEcon Project Scripts

This folder contains the main R scripts used clean_data (df.csv) to analyze, model, and visualize the state-level alfalfa–SPEI panel. The workflow supports research on drought exposure, irrigation status, alfalfa production, export value, and spatial heterogeneity across U.S. states.
---

## Project Purpose

The scripts support a geospatial agricultural economics workflow that links alfalfa production outcomes with drought indicators and irrigation conditions. The main analytical objectives are:

1. Get a state-year panel for alfalfa production, acreage, yield, price, irrigation, water use, and export value from cleandata folder.
2. Model and forecast SPEI time-series patterns using ARIMA/SARIMA methods.
4. Spatial diagnostics using variograms, cross-variograms, co-kriging, and state-level mapping.
5. Construct irrigation-share and irrigation-dummy variables from irrigated acreage and alfalfa acreage.
6. Estimate empirical models for the relationship between SPEI drought conditions, irrigation status, alfalfa yield, production, and export value.
7. Run robustness checks using fixed effects, weighted least squares, instrumental variables, 2SLS, and generalized additive models.
8. Irrigation methods yield comparsion and water efficiency caculation.
9. Generate publication-ready maps and figures for alfalfa acreage, production, export rate, SPEI change, and irrigation-related outcomes.


---

## Recommended File Structure

```text
Geospatial-AgEcon/
├── cleandata/
│   └── cleaned panel data and processed spatial/SPEI files
├── project/
│   ├── empirical.R
│   ├── geo_diag.R
│   ├── plotMapping.R
│   ├── ARIMA.R
│   ├── Robust check.R
│   └── README.md
├── output/
│   ├── figures/
│   ├── maps/
│   ├── tables/
│   └── models/
└── README.md
```

---

## Script Overview

### 1. `empirical.R`

**Purpose:** Main econometric analysis script.

This script prepares the analysis panel and estimates empirical relationships between drought conditions, irrigation, alfalfa outcomes, and export value.

**Main tasks:**

- Reads the state-year alfalfa panel.
- Calculates irrigation share:

```r
irr_share = irrigated_acres / alf_acres
```
```r
irrigation_dummy = 1 if irr_share > 0.2
irrigation_dummy = 0 if irr_share <= 0.2
NA use value from last year
```

- Estimates fixed-effects models using `fixest::feols()`.
- Runs pooled OLS models for comparison.
- Tests heteroskedasticity-robust and clustered standard errors.
- Estimates export-value models using SPEI, irrigation, yield, acreage, production, and price variables.
- Runs weighted least squares using inverse-CV weights.
- Implements an instrumental-variable model where SPEI and irrigation interactions are used as instruments for yield.
- Fits a spatial generalized additive model using longitude and latitude smooth terms.
- Compares alfalfa yield across irrigation methods using ANOVA and Tukey HSD tests.
- Computes water-use efficiency by irrigation system.

**Key packages:**

```r
fixest
lmtest
sandwich
modelsummary
readr
dplyr
tidyr
mgcv
sf
USAboundaries
agricolae
ggplot2
```

**inputs:**

- Cleaned state-year alfalfa panel, typically `df.csv`.
- Required variables may include:
  - `state`, `stusps`, `year`, `state_ansi`
  - `alf_spei_mean`
  - `irrigated_acres`, `alf_acres`
  - `alf_yield`, `alf_production`, `alf_price_usd_ton`
  - `Export_Value`
  - `midwest`, `west`
  - irrigation-method variables such as pressure and gravity irrigation yield/water use

**Expected outputs:**

- Updated `df.csv` with `irr_share` and `irrigation_dummy`.
- Regression output in the R console.
- Model summary tables.
- ANOVA/Tukey summaries for irrigation methods.
- Optional saved R workspace: `alfalfa_spei_environment.RData`.

---

### 2. `geo_diag.R` / `geo_diag_and_map.R`

**Purpose:** Spatial diagnostics and geostatistical analysis.

This script evaluates spatial patterns in SPEI, alfalfa export value, and related state-level variables. It combines state geometry with the alfalfa panel and applies variogram and co-kriging tools.

**Main tasks:**

- Loads U.S. state boundaries using `tigris`.
- Excludes non-CONUS regions such as Alaska, Hawaii, Puerto Rico, and territories where needed.
- Joins state polygons with the alfalfa panel.
- Converts state polygons to centroids for spatial diagnostics.
- Computes empirical variograms for:
  - `alf_spei_mean`
  - `Export_Value`
- Fits variogram models such as:
  - spherical
  - exponential
  - Gaussian
  - Matérn
- Computes cross-variograms between SPEI and export value.
- Fits linear models of coregionalization using `gstat`.
- Performs co-kriging prediction over a spatial grid.
- Creates state-level maps for changes in alfalfa acres, alfalfa value, and percent changes across selected years.

**Key packages:**

```r
dplyr
tibble
janitor
tidyr
lubridate
purrr
readr
ggplot2
tigris
sf
tmap
leaflet
gstat
terra
rmapshaper
exactextractr
fixest
lmtest
sandwich
modelsummary
```

**inputs:**

- Cleaned alfalfa state-year panel, usually loaded as `df`.
- State identifiers such as `state_ansi`, `state`, or `stusps`.
- Key variables:
  - `alf_spei_mean`
  - `Export_Value`
  - `hay_alf_acres`
  - `hay_alf_usd`
  - other alfalfa production/value variables

**Expected outputs:**

- Variogram plots.
- Cross-variogram plots.
- Fitted variogram or LMC model objects.
- Co-kriging prediction objects.
- State-level maps of acreage and value changes.
---

### 3. `plotMapping.R`

**Purpose:** Descriptive visualization and map production.

This script generates scatter plots and maps for changes in SPEI, export value, alfalfa acreage, irrigation production, export rate, and production changes.

**Main tasks:**

- Loads cleaned panel data.
- Standardizes numeric variables, including:
  - `Export_Value`
  - `alf_spei_mean`
  - `alf_yield`
  - `alf_production`
  - `alfalfa_price_usd_ton`
  - `irrigated_production`
  - `water_acrefeet_per_acre`
  - `alf_acres`
- Loads state boundaries with `tigris`.
- Adds state abbreviations if missing.
- Calculates 2017–2022 changes in:
  - export value
  - SPEI
  - production
- Calculates long-run average SPEI from 2005–2025.
- Creates scatter plots with state labels using `ggrepel`.
- Produces maps for:
  - average alfalfa acres
  - average irrigated alfalfa production
  - average export rate
  - alfalfa production change
- Uses `tmap` and `ggplot2` for map production.

**Key packages:**

```r
sf
tmap
tmaptools
grid
terra
rmapshaper
ggplot2
dplyr
tidyr
ggrepel
readr
viridis
tigris
```

**inputs:**

- Cleaned panel data, `df.csv`.
- State identifier variables:
  - `STUSPS`
  - `year`
- Main analytical variables:
  - `Export_Value`
  - `alf_spei_mean`
  - `alf_yield`
  - `alf_production`
  - `alf_acres`
  - `alf_price`
  - `irrigated_production`
  - `water_acrefeet_per_acre`

**outputs:**

- Scatter plot: change in export value vs. change in SPEI.
- Map: average alfalfa acres by state.
- Map: average irrigated alfalfa production.
- Map: average export rate by state.
- Map: alfalfa production change by state.

**output names:**
```text
fig_spei_export_change.png
map_avg_alfalfa_acres.png
map_avg_irrigated_production.png
map_avg_export_rate.png
map_production_change_2017_2022.png
```

---

### 4. `ARIMA.R`

**Purpose:** SPEI time-series plotting, ARIMA/SARIMA estimation, and forecast validation.

This script converts SPEI data into annual and monthly time-series formats, fits a seasonal ARIMA model, checks residual diagnostics, and evaluates forecast accuracy.

**Main tasks:**

- Reads monthly or state-level SPEI panel data.
- Creates clean SPEI fields:
  - `stusps`
  - `date`
  - `spei`
- Defines regions:
  - West
  - Midwest
  - Other
- Computes annual state-level SPEI means.
- Computes regional and national annual SPEI averages.
- Generates a descriptive annual SPEI plot.
- Converts national monthly SPEI into a monthly `ts` object with frequency 12.
- Splits the series into training and testing samples, using the last 24 months as the test set.
- Fits a seasonal ARIMA model:

```r
ARIMA(2,0,2)(1,0,2)[12]
```

- Forecasts the test period.
- Plots residual diagnostics:
  - residual time plot
  - residual ACF
  - residual histogram/density
- Plots train/test/forecast comparison.
- Runs rolling time-series cross-validation using `forecast::tsCV()`.
- Calculates forecast RMSE and MAE by horizon.
- Runs a Ljung-Box test for residual autocorrelation.

**Key packages:**

```r
dplyr
ggplot2
lubridate
forecast
patchwork
zoo
scales
```

**inputs:**

- SPEI panel data, such as `alfal_spei_panel_2005_2025.csv`.
- Required variables:
  - `stusps`
  - `year` or monthly date variable
  - `speimean` or equivalent SPEI variable

**outputs:**

```text
state_level_annual_spei.png
arima_residual_diagnostics.png
arima_cross_validation_spei.png
arima_cross_validation_spei_alt.png
```

**Important note:**

The SARIMA model should be estimated on monthly data, not annual data. Annual SPEI plots are useful for description, but monthly frequency is required for the seasonal `[12]` ARIMA structure.

---
A practical workflow is:

1. Use `empirical.R` to construct the final analysis panel and irrigation variables.
2. Use `geo_diag.R` to evaluate spatial dependence and geostatistical structure.
3. Use `plotMapping.R` to generate descriptive maps and figures.
4. Use `ARIMA.R` to analyze and forecast SPEI time-series patterns.

---

## Data Requirements

The project assumes a state-year or state-month panel with the following core variables.

| Variable | Description |
|---|---|
| `state` | State name |
| `stusps` / `STUSPS` | State abbreviation |
| `state_ansi` | State FIPS/ANSI code |
| `year` | Year |
| `date` | Monthly date, required for ARIMA/SARIMA |
| `alf_spei_mean` / `speimean` | Alfalfa-area weighted SPEI measure |
| `alf_acres` | Alfalfa harvested acres |
| `irrigated_acres` | Irrigated alfalfa acres |
| `irr_share` | Irrigated acres divided by alfalfa acres |
| `irrigation_dummy` | Indicator equal to 1 if `irr_share > 0.2` |
| `alf_yield` | Alfalfa yield |
| `alf_production` / `hay_alf_tons` | Alfalfa production |
| `alf_price_usd_ton` | Alfalfa price per ton |
| `Export_Value` | Alfalfa export value |
| `midwest`, `west` | Region indicators |
| `CV_pct` | Coefficient of variation used for WLS weights |

---

## Reproducibility Notes

Before publishing or sharing the project, consider the following revisions:

1. Move package installation commands to a separate setup script or comment them out after installation.
2. Save all figures to an `output/figures/` or `output/maps/` folder.
3. Save model tables to `output/tables/`.
4. Avoid using `save.image()` unless the full R workspace is intentionally needed.
5. Check that all intermediate spatial objects, such as `grid_sf`, `us_states`, and `states_sf`, are created before being used.
---

---

## Citation / Author

Created by Youmin Li for the Geospatial-AgEcon alfalfa drought, irrigation, and export analysis project.

When using this repository in research or presentation materials, cite the repository and describe the cleaned state-year alfalfa-SPEI panel, spatial diagnostics, and empirical modeling workflow.
