## Alfalfa, SPI mapping
##	Created by YOUMIN LI
## Load required libraries
pkgs <- c("sf","tmap","tmaptools","grid","terra","rmapshaper")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, repos = "https://cloud.r-project.org")
library(ggplot2)
library(sf); library(dplyr)
library(tmap)
library(tmaptools)
library(grid)
#library(raster)
library(terra)
#library(maptools)
library(rmapshaper)
library(ggrepel)
save.image("my_workspace.RData")
load("my_workspace.RData")
###################################

## Read in raster DEM of AL
alDEM <- rast("/Users/macpro/Desktop/Youmin-phd/geospatical/Mapping Tutorial/Example Data/DEM_SE_US/AL_dem_projected.tif")
## Reproject AL DEM to match shapefiles 
#st_transform(alDEM, 4326)
## Write out re-projected AL DEM
#writeRaster(alDEM, overwrite=TRUE, filename="/Volumes/KINGSTON/R Class/Example Data/DEM_SE_US/Al_dem_projected.tif")
## Create a tm package map object
m_al <- tm_shape(alDEM) + tm_raster("AL_dem_projected.tif", palette="BuGn", style="fixed", breaks=c(0,50,100,200,400,Inf), interval.closure="left", title="Elevation (m)") + 
		tm_shape(alregs, is.master=TRUE) + tm_polygons("Identifier", title = "", showNA = TRUE, alpha=0, border.col = "black", border.alpha = 1, legend.show=FALSE) +
		tm_layout(frame=TRUE, inner.margins = c(0.08, 0.035, 0.01, 0.07), legend.show=TRUE, legend.position=c(0.79, 0.85), legend.title.size=1) +
		tm_text("Identifier", shadow=TRUE, bg.color="white") + tm_grid(projection=4326, labels.size = .6, lwd=0, labels.inside.frame = FALSE)

## Create US CONUS map object with AL in red
m_usa <- tm_shape(US_cont) + tm_borders(lwd=1, col = "black", alpha = .5) + tm_fill("isAL")

## Create a view port object (inset mapping area)
vp_usa <- viewport(x = 0.65, y = 0.15, width = 0.5, height = 0.5)

## Open png graphics device
png(file = "/Users/macpro/Desktop/Youmin-phd/geospatical/Mapping Tutorial/Example Output/StudyMap_2024.png", width = 3200, height = 4500, units = 'px', res = 600)
par(bg="white")

## Switch on mapping mode
tmap_mode("plot")
## Call map object
m_al
## Print US map in viewport
print(m_usa, vp = vp_usa)
dev.off()

# Load CONUS states only
states_sf <- states(cb = TRUE) %>%
  st_as_sf() %>%
  filter(!STUSPS %in% c("AK","HI","PR","GU","AS","VI","MP"))

### scatter plot====================================================================================
# Change from 2017 to 2022 for each state
# =========================================================
# Alfalfa-SPEI state analysis and mapping
# Cleaned version of your pasted script
# =========================================================

# ---------------------------
# 0. Packages
# ---------------------------
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggrepel)
library(sf)
library(readr)
library(tmap)
install.packages("viridis")
library(viridis)
library(tigris)
options(tigris_use_cache = TRUE)

# ---------------------------
# 1. Read data
# ---------------------------
df<- read.csv("C:/Users/liyoumin/Desktop/Alfalfa-SPEI/County_alf_irr/merge/df_update.csv")

# If df is already sf, drop geometry safely; if not, keep as is
df0 <- if (inherits(df, "sf")) sf::st_drop_geometry(df) else df

# ---------------------------
# 2. Make key variables consistent
# ---------------------------
df0 <- df0 %>%
  mutate(
    Export_Value = suppressWarnings(as.numeric(Export_Value)),
    alf_spei_mean = suppressWarnings(as.numeric(alf_spei_mean)),
    hay_alfalfa_yield_tons_per_acre = suppressWarnings(as.numeric(hay_alfalfa_yield_tons_per_acre)),
    hay_alf_tons = suppressWarnings(as.numeric(hay_alf_tons)),
    hay_alfalfa_price_per_ton = suppressWarnings(as.numeric(hay_alfalfa_price_per_ton)),
    irr_ton = suppressWarnings(as.numeric(alf_irrigated_production_tons)),
    irr_water_acrefeet = suppressWarnings(as.numeric(irrigated_water_acrefeet_per_acre)),
    alf_prod_ton = readr::parse_number(as.character(hay_alf_tons)),
    alf_acres = readr::parse_number(as.character(hay_alf_acres))
  )

# ---------------------------
# 3. Add state abbreviations if missing
# ---------------------------
# state_ansi should be numeric FIPS; STUSPS is state abbreviation
states_sf <- tigris::states(cb = TRUE, year = 2022) %>%
  st_as_sf() %>%
  select(STUSPS, STATEFP, NAME, geometry) %>%
  mutate(state_ansi = as.numeric(STATEFP)) %>%
  filter(!STUSPS %in% c("AK", "HI", "PR", "AS", "GU", "MP", "VI"))

if (!"STUSPS" %in% names(df0)) {
  df0 <- df0 %>%
    left_join(
      states_sf %>% st_drop_geometry() %>% select(state_ansi, STUSPS),
      by = "state_ansi"
    )
}

# ---------------------------
# 4. Change from 2017 to 2022 for each state
# ---------------------------
changes_17_22 <- df0 %>%
  filter(year %in% c(2017, 2022)) %>%
  group_by(state_ansi, STUSPS, year) %>%
  summarise(
    Export_Value = sum(Export_Value, na.rm = TRUE),
    alf_spei_mean = mean(alf_spei_mean, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = year,
    values_from = c(Export_Value, alf_spei_mean),
    names_glue = "{.value}_{year}"
  ) %>%
  mutate(
    d_export = Export_Value_2022 - Export_Value_2017,
    d_spei   = alf_spei_mean_2022 - alf_spei_mean_2017
  )

# ---------------------------
# 5. Long-run average SPEI (2005–2025) for color
# ---------------------------
spei_avg <- df0 %>%
  filter(year >= 2005, year <= 2025) %>%
  group_by(state_ansi, STUSPS) %>%
  summarise(
    spei_avg_0525 = mean(alf_spei_mean, na.rm = TRUE),
    .groups = "drop"
  )

plot_df <- changes_17_22 %>%
  left_join(spei_avg, by = c("state_ansi", "STUSPS"))

# ---------------------------
# 6. Scatter plot: change in export vs change in SPEI
# ---------------------------
ggplot(plot_df, aes(x = d_spei, y = d_export)) +
  geom_hline(yintercept = 0, linetype = "dotted", alpha = 0.5) +
  geom_vline(xintercept = 0, linetype = "dotted", alpha = 0.5) +
  geom_abline(intercept = 0, slope = -1, linetype = "dashed") +
  geom_point(aes(color = spei_avg_0525), size = 3) +
  geom_text_repel(aes(label = STUSPS), size = 3, show.legend = FALSE) +
  scale_color_gradient2(
    name = "Avg SPEI\n(2005–2025)",
    low = "red", mid = "white", high = "blue",
    midpoint = 0
  ) +
  labs(
    x = "Change in SPEI (2022 - 2017)",
    y = "Change in Export Value (2022 - 2017)",
    title = "Change in Export Value vs SPEI by State",
    subtitle = "Color = Average SPEI (2005–2025): red = drier, blue = wetter"
  ) +
  theme_bw()

# ---------------------------
# 7. Average export value map
# ---------------------------
avg_export <- df0 %>%
  filter(year >= 2005, year <= 2025) %>%
  group_by(state_ansi, STUSPS) %>%
  summarise(
    avg_export = mean(Export_Value, na.rm = TRUE),
    .groups = "drop"
  )

alf_map <- states_sf %>%
  left_join(avg_export, by = c("state_ansi", "STUSPS"))

tmap_mode("plot")

tm_shape(alf_map) +
  tm_polygons(
    col = "avg_export",
    palette = "Greens",
    style = "fixed",
    breaks = c(0, 10, 30, 50, 200, 1000, 5000, 10000, Inf),
    labels = c("0–10", "10–30", "30–50", "50–200", "200–1000", "1000–5000", "5000–10000", "10000+"),
    colorNA = "white",
    textNA = "No Data",
    title = "Avg Export Value\n(2005–2025)"
  ) +
  tm_borders(col = "black", lwd = 0.6) +
  tm_layout(
    legend.outside = TRUE,
    legend.outside.position = "right",
    frame = FALSE,
    main.title = "Average Export Value by State (2005–2025)",
    main.title.size = 1.2
  )

ggplot(alf_map) +
  geom_sf(aes(fill = avg_export), color = "white", linewidth = 0.2) +
  scale_fill_viridis_c(
    option = "A",
    name = "Avg Export\n(2005–2025)",
    direction = -1,
    na.value = "white"
  ) +
  labs(
    title = "Average Export Value by State (2005–2025)",
    caption = "Source: USDA GATS"
  ) +
  theme_minimal()

# ---------------------------
# 8. Average alfalfa acres map
# ---------------------------
avg_acres <- df0 %>%
  filter(year >= 2005, year <= 2025) %>%
  group_by(state_ansi, STUSPS) %>%
  summarise(
    avg_acres = mean(alf_acres, na.rm = TRUE),
    .groups = "drop"
  )

map_acres <- states_sf %>%
  left_join(avg_acres, by = c("state_ansi", "STUSPS"))

# Optional manual replacements
manual_vals <- c(
  MT = 1506500,
  WY = 690000,
  NM = 26500,
  IN = 265000,
  WV = 23000,
  TN = 4000,
  ME = 1500
)

idx <- match(map_acres$STUSPS, names(manual_vals))
map_acres$avg_acres[!is.na(idx)] <- manual_vals[idx[!is.na(idx)]]

tm_shape(map_acres) +
  tm_polygons(
    "avg_acres",
    palette = "YlGn",
    style = "quantile",
    title = "Avg Acres (2005–2025)"
  ) +
  tm_text("STUSPS", size = 0.6) +
  tm_borders() +
  tm_layout(
    main.title = "Average Alfalfa Acres by State",
    legend.outside = TRUE
  )

# ---------------------------
# 9. Average irrigated production map
# ---------------------------
avg_irr <- df0 %>%
  filter(year >= 2005, year <= 2025) %>%
  group_by(state_ansi, STUSPS) %>%
  summarise(
    avg_irr = mean(irr_ton, na.rm = TRUE),
    .groups = "drop"
  )

map_irr <- states_sf %>%
  left_join(avg_irr, by = c("state_ansi", "STUSPS"))

tm_shape(map_irr) +
  tm_polygons(
    "avg_irr",
    palette = "YlGn",
    style = "quantile",
    title = "Avg Irrigated Alfalfa Production (2005–2025)"
  ) +
  tm_borders(lwd = 0.4) +
  tm_text("STUSPS", size = 0.6) +
  tm_layout(
    main.title = "Average Irrigated Alfalfa Production",
    legend.outside = TRUE
  )

# ---------------------------
# 10. Scatter: average yield vs average irrigation water
# ---------------------------
scatter_df <- df0 %>%
  filter(year >= 2005, year <= 2025) %>%
  group_by(state_ansi, STUSPS) %>%
  summarise(
    avg_prod  = mean(hay_alfalfa_yield_tons_per_acre, na.rm = TRUE),
    avg_irrig = mean(irr_water_acrefeet, na.rm = TRUE),
    avg_spei  = mean(alf_spei_mean, na.rm = TRUE),
    .groups = "drop"
  )

ggplot(scatter_df, aes(x = avg_prod, y = avg_irrig)) +
  geom_point(aes(color = avg_spei), size = 3) +
  geom_text_repel(aes(label = STUSPS), size = 3) +
  scale_color_gradient2(
    name = "Avg SPEI\n(2005–2025)",
    low = "red", mid = "white", high = "blue",
    midpoint = 0
  ) +
  labs(
    x = "Average Yield (tons/acre)",
    y = "Average Irrigation Water",
    title = "Irrigation vs Yield (2005–2025)",
    subtitle = "Point color = Average SPEI"
  ) +
  theme_bw()

# ---------------------------
# 11. Average export rate map
# ---------------------------
df0 <- df %>%
  mutate(
    Export_Value = suppressWarnings(as.numeric(Export_Value)),
    hay_alf_usd  = readr::parse_number(as.character(hay_alf_usd))
  ) %>%
  mutate(
    exp_rate = ifelse(
      is.na(Export_Value) | is.na(hay_alf_usd) | hay_alf_usd == 0,
      NA_real_,
      Export_Value * 1000 / hay_alf_usd
    )
  )

avg_exp_rate <- df0 %>%
  filter(year >= 2005, year <= 2025) %>%
  group_by(state_ansi) %>%
  summarise(
    avg_exp_rate = mean(exp_rate, na.rm = TRUE) * 100,
    .groups = "drop"
  )

map_rates <- states_sf %>%
  left_join(avg_exp_rate, by = c("state_ansi"))

tm_shape(map_rates) +
  tm_polygons(
    "avg_exp_rate",
    palette = "Blues",
    style = "cont",
    title = "Avg Export Rate (%)"
  ) +
  tm_borders() +
  tm_layout(
    legend.outside = TRUE,
    legend.outside.position = "right",
    legend.frame = TRUE,
    legend.bg.color = "white",
    legend.bg.alpha = 1
  )

ggplot(map_rates) +
  geom_sf(aes(fill = avg_exp_rate), color = "white", linewidth = 0.2) +
  geom_sf_text(aes(label = STUSPS), size = 2.5) +
  scale_fill_viridis_c(
    option = "C",
    name = "Avg Export Rate (%)",
    na.value = "grey90"
  ) +
  labs(
    title = "Average Export Rate by State (2005–2025)"
  ) +
  theme_minimal()
# ---------------------------
# 12. Production change map: 2018 to 2023
# ---------------------------
prod_change <- df0 %>%
  filter(year %in% c(2017, 2022)) %>%
  group_by(state_ansi, year) %>%
  summarise(
    prod_val = sum(hay_alf_tons, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = year,
    values_from = prod_val,
    names_glue = "prod_{year}"
  ) %>%
  mutate(
    d_prod = prod_2022 - prod_2017,
    d_prod_pct = ifelse(prod_2017 == 0, NA, (d_prod / prod_2017) * 100)
  )

map_change <- states_sf %>%
  left_join(prod_change, by = c("state_ansi"))

tm_shape(map_change) +
  tm_polygons(
    "d_prod_pct",
    palette = "-RdBu",
    style = "pretty",
    title = "Production Change (%)\n2022 - 2017"
  ) +
  tm_borders(col = "grey40", lwd = 0.6) +
  tm_text("STUSPS", size = 0.6) +
  tm_layout(
    main.title = "Alfalfa Production Change by State, 2017–2022",
    legend.outside = TRUE,
    legend.outside.position = "right",
    legend.frame = TRUE,
    legend.bg.color = "white",
    legend.bg.alpha = 1
  )

###SPEI
df0 <- df %>%
  mutate(
    alf_spei_mean = as.numeric(alf_spei_mean)
  )

# ---------------------------
# 2. State shapefile
# ---------------------------
states_sf <- tigris::states(cb = TRUE, year = 2022) %>%
  st_as_sf() %>%
  select(STUSPS, STATEFP, NAME, geometry) %>%
  mutate(state_ansi = as.numeric(STATEFP)) %>%
  filter(!STUSPS %in% c("AK", "HI", "PR", "AS", "GU", "MP", "VI")) %>%
  st_transform(5070)

# add STUSPS if missing
if (!"STUSPS" %in% names(df0)) {
  df0 <- df0 %>%
    left_join(
      states_sf %>%
        st_drop_geometry() %>%
        select(state_ansi, STUSPS),
      by = "state_ansi"
    )
}

# ---------------------------
# 3. Calculate average SPEI by state
# ---------------------------
avg_spei <- df0 %>%
  filter(year >= 2005, year <= 2025) %>%
  group_by(state_ansi, STUSPS) %>%
  summarise(
    avg_alf_spei_mean = mean(alf_spei_mean, na.rm = TRUE),
    .groups = "drop"
  )

avg_spei <- avg_spei %>%
  mutate(
    avg_alf_spei_mean = ifelse(
      is.nan(avg_alf_spei_mean) | is.infinite(avg_alf_spei_mean),
      NA,
      avg_alf_spei_mean
    )
  )

# ---------------------------
# 4. Join to map
# ---------------------------
map_spei <- states_sf %>%
  left_join(avg_spei, by = c("state_ansi", "STUSPS"))

# ---------------------------
# 5A. tmap
# ---------------------------
tmap_mode("plot")

tm_shape(map_spei) +
  tm_polygons(
    "avg_alf_spei_mean",
    palette = "-BuRd",
    midpoint = 0,
    style = "cont",
    title = "Average SPEI\n(2005–2025)",
    colorNA = "grey90",
    textNA = "No data"
  ) +
  tm_borders(col = "grey40", lwd = 0.5) +
  tm_text("STUSPS", size = 0.5) +
  tm_layout(
    main.title = "Average Alfalfa SPEI by State (2005–2025)",
    legend.outside = TRUE,
    legend.outside.position = "right",
    frame = FALSE
  )
