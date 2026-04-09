# Load libraries
library(dplyr);library(tibble);library(janitor);library(tidyr)
library(CropScapeR);library(rnassqs);library(lubridate)
library(purrr);library(tibble);library(readr);library(ggplot2);library(tigris);library(sf)
library(tmap);library(ncdf4);library(leaflet);library(gstat)
#library(raster)
library(terra)
#library(maptools)
library(rmapshaper);library(spData); library(exactextractr); library(tigris)

# Exploratory plot of SPEI vs. value, ton/acre
summary(df$irrigation_dummy == 1)
plot(irrigated_water_acrefeet_per_acre ~ Export_Value, data=df)
# Pearson's product-moment correlation
cor.test(df$hay_alfalfa_price_per_ton, df$Export_Value, use = "pairwise.complete.obs")

###------------spatial correlation - Variogram - gloabl morans --- Auto-kriging - local moran's ---- CV fitness
options(tigris_use_cache = TRUE)

states_sf <- tigris::states(cb = TRUE, year = 2022) %>%
  st_as_sf() %>%
  filter(!STUSPS %in% c("AK", "HI", "PR")) %>%   # PR not in states() usually, but safe
  transmute(
    state_ansi = as.integer(STATEFP),
    state = NAME,
    stusps = STUSPS,
    geometry
  )
alf_sf <- states_sf %>%
  left_join(df %>% mutate(state_ansi = as.integer(state_ansi)),
            by = "state_ansi")

# Explore semivariogram of elevation
st_geometry(alf_sf)
plot(st_geometry(alf_sf))
alf_spei_pts <- st_centroid(alf_sf)
alf_spei_pts <- alf_spei_pts %>%
  filter(!is.na(alf_spei_mean))
summary(alf_spei_pts$alf_spei_mean)

#####
library(gstat)
alf_spei_pts2 <- alf_spei_pts[!is.na(alf_spei_pts$Export_Value), ]
v.exp <- variogram(Export_Value ~ 1, data = alf_spei_pts2)
summary(alf_spei_pts2$Export_Value)
table(is.na(alf_spei_pts2$Export_Value))
###
v.irr <- variogram(alf_spei_mean ~ 1, data = alf_spei_pts2)
v.exp <- variogram(Export_Value ~ 1, data = alf_spei_pts2)
plot(v.irr)
plot(v.exp)

# Fit variogram model
mod <- vgm(c("Exp", "Mat", "Gau", "Sph"))
fit_ols <- fit.variogram(variogram(alf_spei_mean ~ 1, data=alf_spei_pts2), model=mod)
plot(v.irr, pl=T, model=fit_ols, main="OLS Model")

# Try REML fit
library(gstat)
library(sf)
initial_model <- vgm(psill = var(alf_spei_pts2$alf_spei_mean, na.rm = TRUE),
                     model = "Sph", range = 5e5, nugget = 0.1)
fitted_model <- fit.variogram(v.irr, model = initial_model)
plot(v.irr, fitted_model)

# Create an object with the function gstat() that contains both the variable and the covariate
e <- gstat(id="alf_spei_mean", formula=Export_Value~1, data=alf_spei_pts2)
e <- gstat(e, id="alf_spei_mean", formula=alf_spei_mean~1, data=alf_spei_pts2)
# Plot the 2 direct variograms and cross-variogram
vg <- variogram(e)
plot(vg)
# Fit linear model of co-regionalization
g <- gstat(NULL, id = "alf_spei_mean", formula = alf_spei_mean ~ 1, data = alf_spei_pts2)
g <- gstat(g, id = "Export_Value", formula = Export_Value ~ 1, data = alf_spei_pts2)
# Compute cross-variogram between SPEI and Export_Value
vg <- variogram(g)
plot(vg)
# Estimate covariance between the two variables for psill
cov_val <- cov(alf_spei_pts2$Export_Value, alf_spei_pts2$alf_spei_mean)
# Define the initial spherical model
model_init <- vgm(psill = cov_val, model = "Sph", range = 5e5, nugget = 0.1)
# Fit the model
e_sph <- fit.lmc(vg, g, model = model_init, correct.diagonal = 1.01)
# Check fitted model
e_sph
plot(vg, model = e_sph, 
     main = "Empirical & Fitted Cross-Variogram: SPEI vs Export Value")
vg_cross <- vg %>% filter(id == "alf_spei_mean.Export_Value")

plot(vg_cross$dist, vg_cross$gamma,
     type = "b", pch = 19, col = "steelblue",
     xlab = "Distance (m)",
     ylab = "Cross-semivariance",
     main = "Cross-Variogram: SPEI vs Alfalfa Export Value")
abline(h = 0, lty = 2, col = "gray40")
as.data.frame(e_sph$model)

# Gaussian model
e_gau <- fit.lmc(
  vg, e,
  model = vgm(
    psill = cov(alf_spei_pts2$alf_spei_mean, alf_spei_pts2$Export_Value, use = "complete.obs"),
    model = "Gau",
    range = 5e5,     # 500 km
    nugget = 0.1
  ),
  correct.diagonal = 1.01
)

plot(vg, model = e_gau, main = "Gaussian LMC: SPEI vs Export Value")

# Matérn model
e_mat <- fit.lmc(
  vg, e,
  model = vgm(
    psill = cov(alf_spei_pts2$alf_spei_mean, alf_spei_pts2$Export_Value, use = "complete.obs"),
    model = "Mat",
    range = 5e5,
    nugget = 0.1
  ),
  correct.diagonal = 1.01
)

plot(vg, model = e_mat, main = "Matérn LMC: SPEI vs Export Value")

# Predict# Bounding box for your state polygons
bbox <- st_bbox(alf_spei_pts2)
alf_spei_sp <- as(alf_spei_pts2, "Spatial")
grid_sp <- as(grid_sf, "Spatial")
proj4string(grid_sp) <- CRS(st_crs(alf_spei_sp)$wkt)
# Predict SPEI using the co-kriging model
k <- predict(e_sph, newdata = grid_sp)

summary(k$SPEI.pred)
summary(k$SPEI.var)

#regression
library(fixest)
library(lmtest)
library(sandwich)
library(modelsummary)

### map value changes
states_sf <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE)) %>%
  st_set_crs(4326) %>%
  rename(state = ID) %>%
  mutate(state = tolower(state))
var_acres <- "hay_alf_acres"  # change if you mean a different acres field

chg_17_22 <- df %>%
  mutate(state = tolower(state)) %>%
  filter(year %in% c(2022, 2024)) %>%
  group_by(state, year) %>%
  summarise(acres = sum(.data[[var_acres]], na.rm = TRUE), .groups = "drop") %>%
  tidyr::pivot_wider(names_from = year, values_from = acres, names_prefix = "y") %>%
  mutate(
    change_acres = y2024 - y2022,
    pct_change  = 100 * (y2024 - y2022) / y2022
  )

map_df <- states_sf %>%
  left_join(chg_17_22, by = "state")

tm_layout(legend.position = c("right", "center"))
tm_shape(map_df) +
  tm_polygons(
    col = "change_acres",
    style = "cont",
    palette = c("red", "white", "green"),
    midpoint = 0,
    title = "Acres (2024 − 2022)",
    colorNA = "grey85",
    textNA = "No data"
  ) +
  tm_borders(lwd = 0.3) +
  tm_layout(
    main.title = "Change in Alfalfa Harvested Acres (2022→2024)",
    legend.outside = TRUE,
    legend.outside.position = "right",
    frame = FALSE
  )


# 1) compute change (2022 - 2017) by STUSPS
chg_17_22 <- df %>%
  mutate(
    STUSPS = toupper(trimws(as.character(state_ansi))),
    year   = as.integer(year),
    hay_alf_usd = parse_number(as.character(hay_alf_usd))
  ) %>%
  filter(year %in% c(2017, 2022)) %>%
  group_by(STUSPS, year) %>%
  summarise(
    hay_alf_usd = sum(hay_alf_usd, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = year,
    values_from = hay_alf_usd,
    names_prefix = "y"
  ) %>%
  mutate(
    d_value    = y2022 - y2017,
    pct_change = (y2022 - y2017) / y2017 * 100
  )

# 2) join to state polygons
state_lu <- tibble::tibble(
  NAME   = c(state.name, "District of Columbia"),
  STUSPS = c(state.abb, "DC")
)

map_df <- us_states %>%
  left_join(state_lu, by = "NAME") %>%
  left_join(chg_17_22, by = "STUSPS") %>%
  filter(!STUSPS %in% c("AK","HI","PR","GU","VI","AS","MP"))
# 3) map: absolute change
tmap_mode("plot")
tm_shape(map_df) +
  tm_polygons(
    fill = "d_value",
    fill.scale = tm_scale_intervals(
      style = "quantile",
      n = 7,
      style.args = list(unique = TRUE),  # helps when many values are identical (e.g., lots of 0s)
      midpoint = 0,                      # diverging around 0
      label.format = function(x) label_number(scale_cut = cut_short_scale())(x),
      value.na = "grey85",
      label.na = "Missing"
    ),
    fill.legend = tm_legend(
      title = "Δ Alfalfa value (USD)\n2022 − 2017",
      na.show = TRUE
    ),
    col = "grey30",
    lwd = 0.7
  ) +
  tm_layout(
    frame = FALSE,
    legend.outside = TRUE,
    legend.outside.position = "right"
  )

tm_shape(map_df) +
  tm_polygons("pct_change", style="quantile", n=7,
              title="Percent change (%)\n2022 vs 2017",
              labels = label_number(accuracy = 1)) +
  tm_layout(frame=FALSE, legend.outside=TRUE) +
  tm_borders(alpha=0.4)
