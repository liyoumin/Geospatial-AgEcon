# Alfalfa Drought, Irrigation, Production, and Export Panel

This repository contains a cleaned state-year panel for U.S. alfalfa production, drought exposure, irrigation, price, production value, and export outcomes. The final output is designed for empirical analysis of drought vulnerability, irrigation heterogeneity, and trade exposure in the alfalfa sector.

## Clean data outputs

| File | Description |
|---|---|
| `df.csv` | Final cleaned state-year panel. This is the main analysis dataset. |
| `df_missing_summary.csv` | Missing-value count by variable in the final panel. |
| `df_join_summary.csv` | Row-count diagnostics for the main source files joined into the final panel. |
| `county_state_standardized.csv` | County-derived variables aggregated to state-year level and renamed to match the state-panel naming system. |
| `county_state_added_values_detail.csv` | Row-level audit file showing which state-year-variable values were filled from the county-derived panel. |
| `county_state_added_values_summary.csv` | Summary count of county-derived values added by variable. |
| `county_state_variable_mapping.csv` | Crosswalk between `county_state_panel` variable names and final `state_panel` variable names. |
| `hay_haylage_variables_added.csv` | List of contextual hay & haylage irrigation variables added from USDA irrigation files. |

## |Code| Description |
|county.R and countytostate.R|clean county level data and aggregate to state year|
|final_df_workflow|State level data clean and merge with alf_spei from Arcpy, and join missiong valide value fom countytostate|

## Raw input files

| File | Role in workflow |
|---|---|
| `state_panel.csv` | Primary state-year alfalfa panel. This is the preferred source for core alfalfa acreage, production, irrigation, and yield variables. |
| `county_state_panel.csv` | County-level or county-derived alfalfa data aggregated to state-year level and used only to fill missing or zero values in `state_panel.csv`. |
| `alfalfa_spei_2005_2025.csv` | ArcPy-generated alfalfa-land SPEI panel by state and year. This file defines the full state-year base for the final panel. |
| `raw_export value GATS.csv` | GATS export-value file. Export values are reshaped from wide years to state-year format. |
| `raw_state_price.csv` | USDA state-level alfalfa price, production value, and sales data. |
| `raw_state_irr.csv` | USDA HAY & HAYLAGE irrigation data. These variables are added separately as contextual irrigation variables and are not used to overwrite hay-only alfalfa variables. |
| `raw_irr_methods.csv` | USDA HAY & HAYLAGE irrigation method and water-applied data. These variables are added separately as contextual irrigation variables and are not used to overwrite hay-only alfalfa variables. |

## Unit of observation

The final dataset is a state-year panel:

```text
state × year
```

Core identifiers are:

| Variable | Description |
|---|---|
| `state` | State name. |
| `stusps` | Two-letter state abbreviation. |
| `statefp` | Two-digit state FIPS code. |
| `year` | Calendar year. |

The final panel keeps all available state-year observations from the SPEI base for 2005–2025.

## SPEI-03 drought variable

The drought variable is:

| Variable | Description |
|---|---|
| `alf_spei_mean` | Mean SPEI-03 over alfalfa land within each state, averaged over April–September for each year. |

`alf_spei_mean` is generated from ArcPy. The ArcPy workflow spatially links SPEI-03 raster/grid information to alfalfa land and then calculates the state-level mean drought exposure over the alfalfa-growing season. The seasonal window is April through September. This variable is intended to measure drought exposure on alfalfa land rather than the average climate condition of the entire state.

Interpretation:

```text
Lower SPEI values = drier conditions.
Higher SPEI values = wetter conditions.
```

## Core alfalfa production and irrigation variables

These variables are hay-only alfalfa variables and are treated as the core production panel.

| Variable | Description | Unit |
|---|---|---|
| `alf_acres` | Alfalfa hay acres harvested. | Acres |
| `irrigated_acres` | Irrigated alfalfa hay acres harvested. | Acres |
| `non_irrigated_acres` | Non-irrigated alfalfa hay acres harvested. | Acres |
| `alf_production` | Total alfalfa hay production. | Tons |
| `irrigated_production` | Irrigated alfalfa hay production. | Tons |
| `non_irrigated_production` | Non-irrigated alfalfa hay production. | Tons |
| `irr_share` | Share of alfalfa hay acres that are irrigated. | Ratio |
| `alf_yield` | Total alfalfa hay yield. | Tons per acre |
| `irr_yield` | Irrigated alfalfa hay yield. | Tons per acre |
| `non_irr_yield` | Non-irrigated alfalfa hay yield. | Tons per acre |
| `haylage_acres` | Alfalfa haylage acres harvested, retained from the state panel when available. | Acres |
| `haylage_production` | Alfalfa haylage production, retained from the state panel when available. | Tons |
| `haylage_irrigated_acres` | Irrigated alfalfa haylage acres, retained from the state panel when available. | Acres |
| `haylage_irrigated_ops` | Operations with irrigated alfalfa haylage acres, retained from the state panel when available. | Count |

## County-to-state fill rule

`state_panel.csv` is the primary data source for core alfalfa variables. `county_state_panel.csv` is used only as a backup source.

Because the two panels use different variable names, the workflow first maps county-derived variables into the state-panel naming system:

| Final variable | County-state source variable |
|---|---|
| `alf_acres` | `hay_alf_acres` |
| `irrigated_acres` | `alf_irrigated_acres` |
| `non_irrigated_acres` | `alf_non_irrigated_acres_harvested` |
| `alf_production` | `hay_alf_tons` |
| `irrigated_production` | `alf_irrigated_production_tons` |
| `non_irrigated_production` | `non_irrigated_production` |
| `irr_share` | `irrigation_share` |
| `alf_yield` | `hay_alfalfa_yield_tons_per_acre` |
| `irr_yield` | `alf_irrigated_yield_tons_per_acre` |
| `non_irr_yield` | `alf_non_irrigated_yield_tons_per_acre` |

The fill rule is:

```text
If the state-panel value is valid, keep the state-panel value.
If the state-panel value is missing or zero, fill from the county-state value only when the county-state value is non-missing and nonzero.
Exception: county-state `irr_share = 0` is allowed because a zero irrigation share can be meaningful.
```

```text
state_panel valid value > county_state_panel nonzero value > NA
```

County-derived acreage and production variables are aggregated by summing county observations within a state-year. Yield and share variables are not summed. They are recalculated after aggregation:

```text
irr_share = irrigated_acres / alf_acres
alf_yield = alf_production / alf_acres
irr_yield = irrigated_production / irrigated_acres
non_irr_yield = non_irrigated_production / non_irrigated_acres
```

The workflow creates `county_state_added_values_detail.csv` to document every value filled from the county-derived panel.

## Price, production value, and sales variables

Variables from `raw_state_price.csv` are state-level USDA hay variables.

| Variable | Description | Unit |
|---|---|---|
| `alf_price_usd_ton` | Alfalfa hay price received. | Dollars per ton |
| `hay_alf_usd_usda` | Direct USDA alfalfa hay production value. | Dollars |
| `alf_sales_tons` | Alfalfa hay sales. | Tons |
| `hay_alf_usd_calc` | Calculated production value using `alf_price_usd_ton × alf_production`. | Dollars |
| `hay_alf_usd` | Final production value. Uses `hay_alf_usd_usda` first; if missing, uses `hay_alf_usd_calc`. | Dollars |

Production value priority:

```text
hay_alf_usd = direct USDA production value if available;
otherwise, hay_alf_usd = price × production.
```

## Export variables

Export data are from the GATS file and are joined by state-year.

| Variable | Description | Unit |
|---|---|---|
| `Export_Value` | GATS export value. The source file reports values in thousand dollars. | Thousand dollars |
| `export_usd` | Export value converted to dollars. | Dollars |
| `avg_exp_rate` | Export value divided by alfalfa production value. | Ratio |

Construction:

```text
export_usd = Export_Value × 1000
avg_exp_rate = export_usd / hay_alf_usd
```

The export file is not read with base `read.csv()` because the original GATS CSV contains title/header rows and repeated column structures. The workflow reads the file with `readr::read_csv()`, skips non-data rows, reshapes annual columns into long format, filters to `World Total`, and aggregates to state-year level.

## HAY & HAYLAGE contextual irrigation variables

The USDA files `raw_state_irr.csv` and `raw_irr_methods.csv` refer to `HAY & HAYLAGE`, not pure hay-only alfalfa. Therefore, they are not used to overwrite or fill the core hay-only alfalfa variables.

They are added separately with prefixes:

| Prefix | Source file | Meaning |
|---|---|---|
| `hh_alf_stateirr_` | `raw_state_irr.csv` | HAY & HAYLAGE alfalfa irrigation context variables. |
| `hh_alf_methods_` | `raw_irr_methods.csv` | HAY & HAYLAGE alfalfa irrigation method and water-applied variables. |

Examples include:

| Variable pattern | Description |
|---|---|
| `hh_irrigated_acres_total` | Total irrigated HAY & HAYLAGE alfalfa acres. |
| `hh_irrigated_acres_gravity` | Irrigated HAY & HAYLAGE alfalfa acres under gravity irrigation. |
| `hh_irrigated_acres_pressure` | Irrigated HAY & HAYLAGE alfalfa acres under pressure irrigation. |
| `hh_irrigated_acres_source_ground` | Irrigated HAY & HAYLAGE alfalfa acres using groundwater. |
| `hh_irrigated_acres_source_off_farm` | Irrigated HAY & HAYLAGE alfalfa acres using off-farm water. |
| `hh_irrigated_acres_source_on_farm_surface` | Irrigated HAY & HAYLAGE alfalfa acres using on-farm surface water. |
| `hh_water_applied_af_acre_total` | HAY & HAYLAGE alfalfa water applied per acre. |
| `hh_water_applied_af_acre_method_gravity` | Water applied per acre under gravity irrigation. |
| `hh_water_applied_af_acre_method_pressure` | Water applied per acre under pressure irrigation. |
| `hh_irr_yield_tons_per_acre_gravity` | Irrigated dry-basis yield under gravity irrigation. |
| `hh_irr_yield_tons_per_acre_pressure` | Irrigated dry-basis yield under pressure irrigation. |

These variables are useful for contextual interpretation of irrigation technology and water-use intensity, but they should be interpreted as HAY & HAYLAGE variables rather than pure hay-only alfalfa variables.

## Region variables

| Variable | Description |
|---|---|
| `west` | Dummy equal to 1 for western states: AZ, CA, CO, ID, MT, NV, NM, OR, UT, WA, WY. |
| `midwest` | Dummy equal to 1 for midwestern states: IL, IN, IA, KS, MI, MN, MO, NE, ND, OH, SD, WI. |

## Reproducible workflow

The main R workflow is:

```text
final_df_workflow_revised_v6.R
```

The workflow performs the following steps:

1. Read and standardize state identifiers.
2. Clean numeric values and convert USDA disclosure/suppression codes to missing values.
3. Build the SPEI base panel for 2005–2025.
4. Join the primary state alfalfa panel.
5. Aggregate and rename county-derived variables.
6. Fill missing or zero state-panel values using county-derived values under the backup-source rule.
7. Generate an audit file showing which values were filled from county-derived data.
8. Reshape and join GATS export data.
9. Join price, sales, and production-value data.
10. Join HAY & HAYLAGE irrigation variables separately using `hh_alf_*` prefixes.
11. Recalculate derived variables such as yield, irrigation share, production value, export value, and export rate.
12. Save final panel and diagnostics.

## Citation note
appendix:

> The analysis uses a state-year alfalfa panel constructed from USDA production, irrigation, price, and export datasets. State-level observations are retained as the primary source. Missing or zero values in the state panel are supplemented using county-derived records aggregated to the state-year level, with county values used only when the corresponding state-level value is unavailable or zero. Acreage and production variables are aggregated by summing county observations, while yield and irrigation-share variables are recalculated from aggregated totals. Drought exposure is measured using SPEI-03 averaged over alfalfa land within each state for April–September, generated through an ArcPy spatial-processing workflow. HAY & HAYLAGE irrigation variables are retained separately as contextual irrigation measures and are not used to overwrite hay-only alfalfa production variables.

## Notes
- AZ hay_harvest_acres < hay&haylage_irrigated_acres in 2018, but hay_census&survey data do not include have data in 2018; so for production we only use hay category. Some state hay acres = hay&haylage acres, there are overlap for both production.
- Zero values from the county-derived panel are not used to fill state-panel values, except for `irr_share`, where zero can represent a meaningful absence of irrigation.
- HAY & HAYLAGE irrigation variables are intentionally kept separate from hay-only alfalfa variables.
- `Export_Value` is in thousand dollars; `export_usd` is in dollars.
- Lower `alf_spei_mean` values indicate drier alfalfa-growing-season conditions.
