####SPEI_3AND6
library(terra)
library(sf)
library(dplyr)
library(tidyr)
library(stringr)
library(tigris)

options(tigris_use_cache = TRUE)

# 1) Read NOAA SPEI-3 NetCDF
spei1 <- rast("/Users/macpro/Desktop/Youmin-phd/EIAP/Dataset/nclimgrid-spei-pearson-01.nc")
spei <- rast("/Users/macpro/Desktop/Youmin-phd/EIAP/Dataset/nclimgrid-spei-pearson-03.nc")
spei6 <- rast("/Users/macpro/Desktop/Youmin-phd/EIAP/Dataset/nclimgrid-spei-pearson-06.nc")
# Inspect layer names
names(spei)[1:10]
# 2) Download contiguous U.S. state boundaries
states_sf <- states(cb = TRUE, year = 2022) |>
  st_as_sf() |>
  dplyr::select(State = NAME, STUSPS) |>
  dplyr::filter(!State %in% c("Alaska", "Hawaii", "Puerto Rico"))

# IMPORTANT: transform with sf first to the raster CRS
states_sf <- st_transform(states_sf, crs = st_crs("OGC:CRS84"))

# then convert to terra vector
states_v <- vect(states_sf)

# 3) Extract mean SPEI for each state and month
plot(spei[[1]])
lines(states_v, col = "red")
# exact=TRUE and weights=TRUE gives area-weighted extraction
spei_extract <- terra::extract(
  spei,
  states_v,
  fun = mean,
  na.rm = TRUE
)

if ("ID" %in% names(spei_extract)) {
  spei_extract <- spei_extract |> dplyr::select(-ID)
}

spei_state <- bind_cols(
  states_sf |> st_drop_geometry() |> dplyr::select(State),
  as.data.frame(spei_extract)
)

dates <- time(spei)
head(dates)
tail(dates)

spei_long3 <- spei_state |>
  pivot_longer(
    cols = -State,
    names_to = "layer",
    values_to = "spei3"
  ) |>
  group_by(State) |>
  mutate(date = dates[row_number()]) |>
  ungroup() |>
  mutate(
    Year = as.integer(format(date, "%Y")),
    Month = as.integer(format(date, "%m"))
  )

spei_state_year <- spei_long3 |>
  group_by(State, Year) |>
  summarise(
    spei3_apr_jun = mean(spei3[Month %in% 4:6], na.rm = TRUE),
    spei3_jul_sep = mean(spei3[Month %in% 7:9], na.rm = TRUE),
    spei3_apr_sep = mean(spei3[Month %in% 4:9], na.rm = TRUE),
    .groups = "drop"
  ) |>
  mutate(
    spei3_apr_jun = ifelse(is.nan(spei3_apr_jun), NA, spei3_apr_jun),
    spei3_jul_sep = ifelse(is.nan(spei3_jul_sep), NA, spei3_jul_sep),
    spei3_apr_sep = ifelse(is.nan(spei3_apr_sep), NA, spei3_apr_sep)
  )

Alf_panel <- alf_panel |>
  mutate(
    State = as.character(State),
    Year = as.integer(Year)
  ) |>
  left_join(spei_state_year, by = c("State", "Year"))

### robust check==========================================================================================================
west_states <- c("Arizona","California","Colorado","Idaho","Montana",
                 "Nevada","New Mexico","Oregon","Utah","Washington","Wyoming")
# 1. read SPEI-12 raster
spei01 <- rast("/Users/macpro/Desktop/Youmin-phd/EIAP/Dataset/nclimgrid-spei-pearson-01.nc")
# 2. state boundaries
states_sf <- states(cb = TRUE, year = 2022) %>%
  st_as_sf() %>%
  dplyr::select(State = NAME, STUSPS) %>%
  dplyr::filter(State %in% west_states)
# important CRS transform
states_sf2 <- st_transform(states_sf, crs = st_crs("OGC:CRS84"))
states_v2  <- vect(states_sf2)
# 3. extract state mean SPEI-01 by month
spei01_extract <- terra::extract(
  spei01,
  states_v2,
  fun = mean,
  na.rm = TRUE
)
if ("ID" %in% names(spei12_extract)) {
  spei01_extract <- spei12_extract %>% dplyr::select(-ID)
}
spei01_state <- bind_cols(
  states_sf2 %>% st_drop_geometry() %>% dplyr::select(State),
  as.data.frame(spei12_extract)
)

# 4. reshape to long format
dates01 <- time(spei01)

spei01_long <- spei01_state %>%
  pivot_longer(
    cols = -State,
    names_to = "layer",
    values_to = "spei_01"
  ) %>%
  group_by(State) %>%
  mutate(date = dates01[row_number()]) %>%
  ungroup() %>%
  mutate(
    Year = as.integer(format(date, "%Y")),
    Month = as.integer(format(date, "%m"))
  )

# 5. recent 5 years
max_year <- max(spei01_long$Year, na.rm = TRUE)

west_spei01_5y <- spei01_long %>%
  filter(Year >= max_year - 4) %>%
  arrange(State, date)

library(ggplot2)
# 6. figure
west_mean_5y <- west_spei01_5y %>%
  group_by(date) %>%
  summarise(mean_spei_01 = mean(spei_01, na.rm = TRUE), .groups = "drop")

ggplot() +
  geom_line(
    data = west_spei01_5y,
    aes(x = date, y = spei_01, group = State, color = State),
    linewidth = 0.7,
    alpha = 0.7
  ) +
  geom_line(
    data = west_mean_5y,
    aes(x = date, y = mean_spei_01),
    color = "red",
    linewidth = 1.4
  ) +
  labs(
    title = "Monthly SPEI-01 for Western States (Recent 5 Years)",
    subtitle = "Red line = mean across western states",
    x = "Date",
    y = "SPEI-01",
    color = "State"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = "right",
    plot.title = element_text(face = "bold")
  )

ggplot() +
  geom_line(
    data = west_spei01_5y,
    aes(x = date, y = spei_01, group = State),
    color = "gray60",
    linewidth = 0.6,
    alpha = 0.7
  ) +
  geom_line(
    data = west_mean_5y,
    aes(x = date, y = mean_spei_01),
    color = "red",
    linewidth = 1.5
  ) +
  labs(
    title = "Monthly SPEI-01 for Western States (Recent 5 Years)",
    subtitle = "Gray lines = states; red line = western-state mean",
    x = "Date",
    y = "SPEI-01"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(face = "bold")
  )

#### seasonal changes observious#### robust check with 6 month SPEI (Apri-Sep)
library(fixest)
library(lmtest)
library(sandwich)
library(modelsummary)
## fixed effects
model <- feols(hay_alfalfa_yield_tons_per_acre ~ spei3_apr_sep + midwest + west + irrigation + (irrigation*spei3_apr_sep) | year + state_ansi, data = Alf_panel)
ols <- lm(hay_alfalfa_yield_tons_per_acre ~ spei3_apr_sep + irrigation + west + midwest + irrigation + (irrigation*spei3_apr_sep), data = Alf_panel)
modelp <- feols(log(hay_alfalfa_production_tons) ~ spei3_apr_sep + irrigation + midwest + west + irrigation + (irrigation*spei3_apr_sep)| year, data = Alf_panel)
###robast check(to account for heteroskedasticity or clustering)
summary(model, se = "hetero")
summary(model, cluster = "state_ansi")
summary(model, cluster = "year")

summary(ols, se ="hetero")
summary(ols, cluster = "state_ansi")
summary(ols, cluster = "year")
summary(modelp, se = "hetero")

#regression1 Export
model1 <- feols(Export_Value ~ spei3_apr_sep + irrigation + (spei3_apr_sep*irrigation) + hay_alfalfa_yield_tons_per_acre + hay_alfalfa_acres_harvested +
                  west + midwest| year, data = Alf_panel)
summary(model1, cluster = 'year')


#regression2
model2 <- lm(Export_Value ~ spei3_apr_sep + irrigation + (spei3_apr_sep*irrigation) + west + hay_alfalfa_price_per_ton + hay_alfalfa_production_tons + hay_alfalfa_yield_tons_per_acre, data = Alf_panel)
summary(model2)

#regression3
model3 <- feols(Export_Value ~ spei3_apr_sep + irrigation + (spei3_apr_sep*irrigation) + west + midwest + hay_alfalfa_production_tons +  hay_alfalfa_yield_tons_per_acre + hay_alfalfa_price_per_ton| year, data = Alf_panel)
summary(model3)
plot(model3)

modelsummary(list("Two-way FE (model1)" = model1,
                  "Pooled OLS (model2)" = model2),
             statistic = "std.error",
             stars = TRUE,
             gof_omit = "Adj|AIC|BIC")

## wls
alf_panel <- alf_panel |>
  mutate(
    cv_rse = CV_pct/100,
    w_inv_cv2 = 1 / (cv_rse^2 + 1e-6)  # small constant to avoid infinite weights
  )

m_wls <- feols(
  log(hay_alfalfa_production_tons) ~ spei3_apr_sep + (spei3_apr_sep*irrigation) + irrigation + midwest + west,
  data  = Alf_panel,
  weights = ~ w_inv_cv2
)
summary(m_wls)

lmp <- feols(
  log(hay_alfalfa_production_tons) ~ spei3_apr_sep + (spei3_apr_sep*irrigation) + irrigation + midwest + west | year,
  data  = Alf_panel)
summary(lmp)
# 2SLS: Export on Yield, instrumenting Yield with SPEI
# hay_alfalfa_price_per_ton + irrigation + west + midwest + hay_alfalfa_production_tons +  hay_alfalfa_yield_tons_per_acre +hay_alfalfa_acres_harvested

fs <- feols(
  hay_alfalfa_yield_tons_per_acre ~ spei3_apr_sep + (spei3_apr_sep*irrigation) + irrigation | state_ansi + year,
  data = Alf_panel
)
summary(fs)

iv_model <- feols(
  Export_Value ~ irrigation + hay_alfalfa_production_tons + west + midwest + hay_alfalfa_price_per_ton | year |  hay_alfalfa_yield_tons_per_acre ~ spei6_apr_sep ,
  data = Alf_panel
)

iv_model1 <- feols(
  Export_Value ~ hay_alfalfa_production_tons + west + midwest + hay_alfalfa_price_per_ton | year |  hay_alfalfa_yield_tons_per_acre ~ spei3_apr_sep + (spei3_apr_sep*irrigation) + irrigation ,
  data = Alf_panel
)

summary(iv_model1, stage = 1)   # first stage
summary(iv_model1, stage = 2)   # second stage (default)
etable(iv_model1, stage = 1:2)

iv_model2 <- feols(
  Export_Value ~ irrigation + west + midwest + hay_alfalfa_yield_tons_per_acre | year | hay_alfalfa_production_tons ~ spei6_apr_sep,
  data = Alf_panel
)

summary(iv_model2, stage = 1:2)

# Diagnostics (availability depends on fixest version)
fitstat(iv_model, "ivf")       # first-stage F / weak-IV related stats (if supported)

## gam function ##
library(mgcv)
library(dplyr)
library(sf)
library(USAboundaries)
# 1) state geometry
st_sf <- USAboundaries::us_states() %>%
  st_transform(4326) %>%
  transmute(state = tolower(name), geometry)

# 2) join geometry into your panel
alf_panel_sf <- Alf_panel %>%
  mutate(state = tolower(State)) %>%
  left_join(st_sf, by = "state") %>%
  st_as_sf()

# 3) centroids + lon/lat
alf_centroid <- alf_panel_sf %>%
  mutate(centroid = st_centroid(st_geometry(.))) %>%
  mutate(
    lon = st_coordinates(centroid)[, 1],
    lat = st_coordinates(centroid)[, 2]
  ) %>%
  st_drop_geometry()

# 4) clean variables
alf_clean <- alf_centroid %>%
  mutate(
    Export_Value  = as.numeric(Export_Value),
    SPEI          = as.numeric(spei3_apr_sep),
    irrigation    = as.numeric(irrigation),
    Alf_Price_ton = as.numeric(hay_alfalfa_price_per_ton),
    alf_yield_raw = as.numeric(hay_alfalfa_yield_tons_per_acre),
    lon           = as.numeric(lon),
    lat           = as.numeric(lat)
  ) %>%
  filter(
    !is.na(Export_Value),
    !is.na(SPEI),
    !is.na(irrigation),
    !is.na(Alf_Price_ton),
    !is.na(alf_yield_raw),
    !is.na(lon),
    !is.na(lat)
  )

# 5) scale variables
alf_scaled <- alf_clean %>%
  mutate(
    alf_value_usd     = readr::parse_number(as.character(hay_alfalfa_production_usd)) / 1e6,
    alf_acres         = readr::parse_number(as.character(hay_alfalfa_acres_harvested)) / 1e3,
    alf_prod_ton      = readr::parse_number(as.character(hay_alfalfa_production_tons)) / 1e3,
    Alf_Price_ton     = as.numeric(scale(Alf_Price_ton)),
    alf_yield_tonacre = as.numeric(scale(alf_yield_raw)),
    SPEI              = as.numeric(scale(SPEI)),
    lon               = as.numeric(scale(lon)),
    lat               = as.numeric(scale(lat))
  ) %>%
  filter(
    is.finite(Export_Value),
    is.finite(SPEI),
    is.finite(irrigation),
    is.finite(Alf_Price_ton),
    is.finite(alf_yield_tonacre),
    is.finite(lon),
    is.finite(lat)
  )

# 6) GAM
gam_model_spatial <- gam(
  Export_Value ~ SPEI + irrigation + SPEI:irrigation +
    alf_yield_tonacre +
    s(Alf_Price_ton, k = 5) +
    s(lon, lat, k = 10),
  data = alf_scaled,
  method = "REML"
)

summary(gam_model_spatial)
gam.check(gam_model_spatial)

# ---- Save all current R objects ----
save.image("alfalfa_spei_environment.RData")
load("alfalfa_spei_environment.RData")
