library(dplyr);library(purrr);library(tidyr);library(stringr)
library(readr);library(janitor)

prod <- read_csv("/Users/macpro/Desktop/Youmin-phd/EIAP/Dataset/production.csv")
prod <- prod %>%
  clean_names() %>%
  # after clean_names(): year, state, state_ansi, data_item, value, cv_percent (often)
  select(year, state, state_ansi, data_item, value, cv_percent) %>%
  mutate(
    value = parse_number(as.character(value)),
    cv_raw = as.character(cv_percent),
    cv = parse_number(cv_raw),
    cv_low_flag = as.integer(str_detect(cv_raw, "\\(L\\)")),
    var = data_item %>%
      str_to_lower() %>%
      str_replace_all("[,/-]", " ") %>%
      str_replace_all("\\s+", " ") %>%
      str_trim() %>%
      str_replace_all("[^a-z0-9 ]", "") %>%
      str_replace_all(" ", "_")
  ) %>%
  group_by(year, state, state_ansi, var) %>%
  summarise(
    value = sum(value, na.rm = TRUE),
    cv = first(cv),
    cv_low_flag = max(cv_low_flag, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    id_cols = c(year, state, state_ansi),
    names_from = var,
    values_from = c(value, cv, cv_low_flag),
    names_glue = "{var}_{.value}"
  ) %>%
  arrange(state_ansi, year)

prod <- prod %>%
  select(-contains("_cv"))


irrigation <- read_csv("/Users/macpro/Desktop/Youmin-phd/EIAP/Dataset/Irr.csv")
irrigation <- irrigation %>%
  clean_names() %>%
  # after clean_names(): year, state, state_ansi, data_item, domain, domain_category, value
  select(year, state, state_ansi, data_item, domain, domain_category, value) %>%
  mutate(
    value = parse_number(as.character(value)),
    
    # variable name = Data Item + Domain Category  (keeps PRESSURE vs GRAVITY vs NOT SPECIFIED)
    var = str_c(data_item, domain_category, sep = " - ") %>%
      str_to_lower() %>%
      str_replace_all("[,/:()&-]", " ") %>%
      str_replace_all("\\s+", " ") %>%
      str_trim() %>%
      str_replace_all("[^a-z0-9 ]", "") %>%
      str_replace_all(" ", "_")
  ) %>%
  # if duplicates exist for same panel id + var, aggregate
  group_by(year, state, state_ansi, var) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(
    id_cols = c(year, state, state_ansi),
    names_from = var,
    values_from = value
  ) %>%
  arrange(state_ansi, year)

irrigation <- irrigation %>%
  select(-c(haylage_alfalfa_yield_measured_in_tons_acre_not_specified))

price <- read_csv("/Users/macpro/Desktop/Youmin-phd/EIAP/Dataset/Price.csv")
price <- price %>%                 # your price dataset name
  clean_names() %>%
  # keep only what you want in the panel
  select(year, state, state_ansi, data_item, value) %>%
  mutate(
    value = parse_number(as.character(value)),
    var = data_item %>%
      str_to_lower() %>%
      str_replace_all("[,/:()&-]", " ") %>%
      str_replace_all("\\s+", " ") %>%
      str_trim() %>%
      str_replace_all("[^a-z0-9 ]", "") %>%
      str_replace_all(" ", "_")
  ) %>%
  # collapse duplicates (multiple rows per year-state-ansi-item)
  group_by(year, state, state_ansi, var) %>%
  summarise(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  # panelize
  pivot_wider(
    id_cols = c(year, state, state_ansi),
    names_from = var,
    values_from = value
  ) %>%
  arrange(state_ansi, year)

panel1 <- list(
  prod,
  irrigation %>% select(-state_ansi),
  price %>% select(-state_ansi)
) %>%
  reduce(~ full_join(.x, .y, by = c("year", "state")))

readr::write_csv(panel1, "panel1.csv")

# Load libraries
library(dplyr);library(tibble);library(janitor);library(tidyr)
library(CropScapeR);library(rnassqs);library(lubridate)
library(purrr);library(tibble);library(readr);library(ggplot2);library(tigris);library(sf)
library(tmap);library(ncdf4);library(leaflet);library(gstat)
#library(raster)
library(terra)
#library(maptools)
library(rmapshaper);library(spData); library(exactextractr); library(tigris)
###-----------------------------------------------------------------------------------------------
alf_panel <- read.csv("/Users/macpro/Desktop/Youmin-phd/EIAP/Dataset/panel_alf/panel1.csv")
#-------------------------------------------
# ---- Quick visualization spei---
spei <- read.csv("/Users/macpro/Desktop/Youmin-phd/EIAP/Figure/Project/data1/state_spei_2005_2025.csv")
ggplot(spei, aes(Year, SPEI, group = State)) +
  geom_line(alpha = 0.25) +
  geom_smooth(aes(group = 1), color = "blue", se = FALSE) +
  theme_minimal() +
  labs(title = "State-level Annual SPEI (2005–2025)", y = "SPEI (z-score)")

# 1) Define regions (edit lists if your definition differs)
west_states <- c("Arizona","California","Colorado","Idaho","Montana",
                 "Nevada","New Mexico","Oregon","Utah","Washington","Wyoming")

midwest_states <- c("Illinois","Indiana","Iowa","Kansas","Michigan","Minnesota","Missouri",
                    "Nebraska","North Dakota","Ohio","South Dakota","Wisconsin")

# 2) Build regional averages by year
spei_region <- spei %>%
  mutate(region = case_when(
    State %in% west_states ~ "West",
    State %in% midwest_states ~ "Midwest",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(region)) %>%
  group_by(region, Year) %>%
  summarise(SPEI = mean(SPEI, na.rm = TRUE), .groups = "drop")

spei_national <- spei %>%
  group_by(Year) %>%
  summarise(SPEI = mean(SPEI, na.rm = TRUE), .groups = "drop") %>%
  mutate(region = "National")

spei_lines <- bind_rows(spei_national, spei_region) %>%
  arrange(region, Year)

ggplot(spei, aes(x = Year, y = SPEI, group = State)) +
  geom_line(alpha = 0.20, color = "grey50") +
  geom_line(
    data = spei_lines,
    aes(x = Year, y = SPEI, color = region, group = region),
    linewidth = 1.3,
    inherit.aes = FALSE
  ) +
  scale_color_manual(values = c(
    National = "blue",
    West = "red",
    Midwest = "yellow"
  )) +
  theme_minimal() +
  labs(title = "State-level Annual SPEI (2005–2025)",
       y = "SPEI (z-score)", color = "Region")

#=====================================================================================================
### merge panel
export <- read.csv("/Users/macpro/Desktop/Youmin-phd/EIAP/Dataset/panel_alf/Export.csv")

library(stringr)
alf_panel <- alf_spei %>%
  left_join(
    export %>%
      mutate(State = str_to_title(state)) %>%     # make names match if needed
      select(Year = year, State, Export_Value),
    by = c("Year", "State")
  )

readr::write_csv(alf_panel, "panel2.csv")

alf_panel <- alf_panel %>%
  mutate(
    irrigation = if_else(
      rowSums(!is.na(select(., starts_with("hay_alfalfa_irr")))) > 0,  # any irr_ column not NA
      1L,                                                   # mark irrigated
      0
    )
  )

alf_panel <- alf_panel %>%
  mutate(
    irrigation = if_else(
      rowSums(!is.na(select(., starts_with("hay_haylage_alfalfa_irr")))) > 0,  # any irr_ column not NA
      1L,                                                   # mark irrigated
      0
    )
  )

#================================================================================
# Exploratory plot of SPEI vs. value, ton/acre
summary(alf_panel$irrigation == 1)
plot(hay_haylage_alfalfa_irrigated_water_acrefeet_per_acre ~ Export_Value, data=alf_panel)
# Pearson's product-moment correlation
cor.test(alf_panel$hay_alfalfa_yield_tons_per_acre, alf_panel$Export_Value, use = "pairwise.complete.obs")

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
  left_join(alf_panel %>% mutate(state_ansi = as.integer(state_ansi)),
            by = "state_ansi")

# Explore semivariogram of elevation
st_geometry(alf_sf)
plot(st_geometry(alf_sf))
alf_spei_pts <- st_centroid(alf_sf)
alf_spei_pts <- alf_spei_pts %>%
  filter(!is.na(SPEI))
summary(alf_spei$SPEI)
v.irr <- variogram(SPEI ~ 1, data = alf_spei_pts)
v.exp <- variogram(Export_Value ~ 1, data = alf_spei_pts)
plot(v.irr)
plot(v.exp)

# Fit variogram model
mod <- vgm(c("Exp", "Mat", "Gau", "Sph"))
fit_ols <- fit.variogram(variogram(SPEI ~ 1, data=alf_spei_pts), model=mod)
plot(v.irr, pl=T, model=fit_ols, main="OLS Model")

# Try REML fit
library(gstat)
library(sf)
initial_model <- vgm(psill = var(alf_spei_pts$SPEI, na.rm = TRUE),
                     model = "Sph", range = 5e5, nugget = 0.1)
fitted_model <- fit.variogram(v.irr, model = initial_model)
plot(v.irr, fitted_model)

# Create an object with the function gstat() that contains both the variable and the covariate
e <- gstat(id="SPEI", formula=Export_Value~1, data=alf_spei_pts)
e <- gstat(e, id="SPEI", formula=SPEI~1, data=alf_spei_pts)
# Plot the 2 direct variograms and cross-variogram
vg <- variogram(e)
plot(vg)
# Fit linear model of co-regionalization
g <- gstat(NULL, id = "SPEI", formula = SPEI ~ 1, data = alf_spei_pts)
g <- gstat(g, id = "Export_Value", formula = Export_Value ~ 1, data = alf_spei_pts)
# Compute cross-variogram between SPEI and Export_Value
vg <- variogram(g)
plot(vg)
# Estimate covariance between the two variables for psill
cov_val <- cov(alf_spei_pts$Export_Value, alf_spei_pts$SPEI)
# Define the initial spherical model
model_init <- vgm(psill = cov_val, model = "Sph", range = 5e5, nugget = 0.1)
# Fit the model
e_sph <- fit.lmc(vg, g, model = model_init, correct.diagonal = 1.01)
# Check fitted model
e_sph
plot(vg, model = e_sph, 
     main = "Empirical & Fitted Cross-Variogram: SPEI vs Export Value")
vg_cross <- vg %>% filter(id == "SPEI.Export_Value")

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
    psill = cov(alf_spei_pts$SPEI, alf_spei_pts$Export_Value, use = "complete.obs"),
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
    psill = cov(alf_spei_pts$SPEI, alf_spei_pts$Export_Value, use = "complete.obs"),
    model = "Mat",
    range = 5e5,
    nugget = 0.1
  ),
  correct.diagonal = 1.01
)

plot(vg, model = e_mat, main = "Matérn LMC: SPEI vs Export Value")

# Predict# Bounding box for your state polygons
bbox <- st_bbox(alf_spei_pts)
alf_spei_sp <- as(alf_spei_pts, "Spatial")
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

alf_panel <- alf_panel %>%
  mutate(
    SPEI_shift = SPEI - min(SPEI, na.rm = TRUE)
  )

##region dummy
alf_panel <- alf_panel %>%
  mutate(
    west = as.integer(State %in% west_states),
    midwest = as.integer(State %in% midwest_states)
  )

## fixed effects
model <- feols(hay_alfalfa_yield_tons_per_acre ~ SPEI_shift + midwest + west + hay_haylage_alfalfa_irrigated_water_acrefeet_per_acre| year, data = alf_panel)
ols <- lm(hay_alfalfa_yield_tons_per_acre ~ SPEI_shift + irrigation + west + midwest, data = alf_panel)
modelp <- feols(log(hay_alfalfa_production_tons) ~ SPEI_shift + irrigation + midwest + west | year, data = alf_panel)
###robast check(to account for heteroskedasticity or clustering)
summary(model, se = "hetero")
summary(model, cluster = "state_ansi")
summary(model, cluster = "year")

summary(ols, se ="hetero")
summary(ols, cluster = "state_ansi")
summary(modelp, se = "hetero")

#regression1 price collinear
model1 <- feols(Export_Value ~ SPEI_shift + irrigation + hay_alfalfa_price_per_ton + hay_alfalfa_yield_tons_per_acre + hay_alfalfa_acres_harvested +
                  west + midwest| year, data = alf_panel)
summary(model1, cluster = 'year')


#regression2
model2 <- lm(Export_Value ~ SPEI_shift + irrigation + west + hay_alfalfa_price_per_ton + hay_alfalfa_production_tons + hay_alfalfa_yield_tons_per_acre, data = alf_panel)
summary(model2)

#regression3
model3 <- feols(Export_Value ~ SPEI_shift + hay_alfalfa_price_per_ton + irrigation + west + midwest + hay_alfalfa_production_tons +  hay_alfalfa_yield_tons_per_acre| year, data = alf_panel)
summary(model3)
plot(model3)

modelsummary(list("Two-way FE (model2)" = model2,
                  "Pooled OLS (model3)" = model3),
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
  log(hay_alfalfa_production_tons) ~ SPEI + irrigation + midwest + west,
  data  = alf_panel,
  weights = ~ w_inv_cv2
)
summary(m_wls)

## regression4
model4 <- feols(Export_Value ~ SPEI + hay_alfalfa_price_per_ton + irrigation + west + midwest| year, data = alf_panel)
etable(model2, model4)

## gam function ##
library(mgcv)
library(dplyr)
library(sf)
install.packages("USAboundaries")
library(USAboundaries)

# 1) Get state polygons
st_sf <- USAboundaries::us_states() %>%
  st_transform(4326) %>%
  transmute(state = tolower(name), geometry)

# 2) Join geometry into your panel (by state name)
alf_panel_sf <- alf_panel %>%
  mutate(state = tolower(state)) %>%
  left_join(st_sf, by = "state") %>%
  st_as_sf()

# 3) Centroids + lon/lat
alf_centroid <- alf_panel_sf %>%
  mutate(centroid = st_centroid(st_geometry(.))) %>%
  mutate(
    lon = st_coordinates(centroid)[,1],
    lat = st_coordinates(centroid)[,2]
  ) %>%
  st_drop_geometry()

alf_clean <- alf_centroid %>%
  mutate(
    Export_Value  = as.numeric(Export_Value),
    SPEI          = as.numeric(SPEI),
    irrigation    = as.numeric(irrigation),
    Alf_Price_ton = as.numeric(hay_alfalfa_price_per_ton),
    lon           = as.numeric(lon),
    lat           = as.numeric(lat)
  ) %>%
  filter(!is.na(Export_Value), !is.na(SPEI))

alf_scaled <- alf_clean %>%
  mutate(
    # Convert and scale numeric-like text columns
    alf_value_usd  = readr::parse_number(as.character(hay_alfalfa_production_usd)) / 1e6,   # millions
    alf_acres      = readr::parse_number(as.character(hay_alfalfa_acres_harvested)) / 1e3,       # thousand acres
    alf_prod_ton   = as.numeric(hay_alfalfa_price_per_ton) / 1e3,                           # thousand tons
    Alf_Price_ton  = scale(Alf_Price_ton),
    alf_yield_tonacre = scale(hay_alfalfa_yield_tons_per_acre),
    SPEI = scale(SPEI),
    lon  = scale(lon),
    lat  = scale(lat)
  ) %>%
  filter(
    is.finite(Export_Value),
    is.finite(SPEI),
    is.finite(hay_alfalfa_price_per_ton),
    is.finite(hay_alfalfa_yield_tons_per_acre),
    is.finite(lon),
    is.finite(lat)
  )

gam_model_spatial <- gam(
  Export_Value ~ SPEI + irrigation + alf_yield_tonacre + s(Alf_Price_ton, K=5) + s(lon, lat, k = 10),   # reduce k dramatically
  data = alf_scaled,
  method = "REML"
)
summary(gam_model_spatial)

# 2SLS: Export on Yield, instrumenting Yield with SPEI
# hay_alfalfa_price_per_ton + irrigation + west + midwest + hay_alfalfa_production_tons +  hay_alfalfa_yield_tons_per_acre +hay_alfalfa_acres_harvested

fs <- feols(
  hay_alfalfa_yield_tons_per_acre ~ SPEI_shift + irrigation | state_ansi + year,
  data = alf_panel
)
summary(fs)

iv_model <- feols(
  Export_Value ~ hay_alfalfa_production_tons + hay_alfalfa_price_per_ton | year | hay_alfalfa_yield_tons_per_acre ~ SPEI_shift + irrigation + west,
  data = alf_panel
)

iv_model1 <- feols(
  Export_Value ~ hay_alfalfa_production_tons + hay_alfalfa_price_per_ton | year |  hay_alfalfa_yield_tons_per_acre ~ SPEI_shift + west,
  data = alf_panel
)
summary(iv_model1, stage = 1)   # first stage
summary(iv_model1, stage = 2)   # second stage (default)
etable(iv_model1, stage = 1:2)

# Diagnostics (availability depends on fixest version)
fitstat(iv_model, "ivf")       # first-stage F / weak-IV related stats (if supported)

# ---- Save all current R objects ----
save.image("alfalfa_spei_environment.RData")
load("alfalfa_spei_environment.RData")




### map value changes
states_sf <- st_as_sf(maps::map("state", plot = FALSE, fill = TRUE)) %>%
  st_set_crs(4326) %>%
  rename(state = ID) %>%
  mutate(state = tolower(state))
var_acres <- "hay_alfalfa_acres_harvested"  # change if you mean a different acres field

chg_17_22 <- alf_panel %>%
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
chg_17_22 <- alf_spei_updated %>%
  mutate(
    STUSPS = toupper(trimws(as.character(STUSPS))),
    year   = as.integer(year),                
    alf_value_usd = readr::parse_number(as.character(alf_value_usd))
  ) %>%
  filter(year %in% c(2018, 2023)) %>%
  summarise(alf_value_usd = sum(alf_value_usd, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = year, values_from = alf_value_usd, names_prefix = "y") %>%
  mutate(
    d_value = y2023 - y2018,
    pct_change = (y2023 - y2018) / y2018 * 100
  )

# 2) join to state polygons
map_df <- us_states %>%                        # sf object with geometry
  left_join(chg_18_23, by = "STUSPS") %>%
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
      title = "Δ Alfalfa value (USD)\n2023 − 2018",
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
              title="Percent change (%)\n2023 vs 2018",
              labels = label_number(accuracy = 1)) +
  tm_layout(frame=FALSE, legend.outside=TRUE) +
  tm_borders(alpha=0.4)

### irrigation compare==========================================================================
alf_panel <- alf_panel %>%
  mutate(
    hay_haylage_alfalfa_non_irrigated_yield_tons_per_acre_dry_basis =
      if_else(
        (is.na(hay_haylage_alfalfa_non_irrigated_yield_tons_per_acre_dry_basis) |
           hay_haylage_alfalfa_non_irrigated_yield_tons_per_acre_dry_basis == 0) &
          !is.na(hay_alfalfa_non_irrigated_yield_tons_per_acre),
        hay_alfalfa_non_irrigated_yield_tons_per_acre,
        hay_haylage_alfalfa_non_irrigated_yield_tons_per_acre_dry_basis
      )
  )


library(agricolae)
df2 <- alf_panel %>%
  transmute(
    non_irr_ton  = hay_haylage_alfalfa_non_irrigated_yield_tons_per_acre_dry_basis,
    irr_ton      = hay_haylage_alfalfa_irrigated_yield_tons_per_acre_dry,
    irr_gra_ton  = hay_haylage_alfalfa_irrigated_yield_tons_per_acre_dry_basis_gravity,
    irr_pres_ton = hay_haylage_alfalfa_irrigated_yield_tons_per_acre_dry_basis_pressure
  ) %>%
  mutate(across(everything(), ~ readr::parse_number(as.character(.)))) %>%
  # keep rows where at least one yield exists
  filter(if_any(everything(), ~ !is.na(.)))

yield_vars <- c("non_irr_ton", "irr_gra_ton", "irr_ton", "irr_pres_ton")

# --- 1) Long format for ANOVA + plotting ---
df_long <- df2 %>%
  pivot_longer(cols = all_of(yield_vars),
               names_to = "Treatment", values_to = "Yield") %>%
  filter(!is.na(Yield)) %>%
  mutate(
    Treatment = factor(
      Treatment,
      levels = c("non_irr_ton", "irr_gra_ton", "irr_ton", "irr_pres_ton"),
      labels = c("Non-Irrigated", "Gravity", "Irrigated (General)", "Pressure")
    )
  )

# --- 2) ANOVA + Tukey (HSD) letters ---
anova_mod <- aov(Yield ~ Treatment, data = df_long)
hsd_results <- agricolae::HSD.test(anova_mod, "Treatment", group = TRUE)

dt_letters <- hsd_results$groups %>%
  tibble::rownames_to_column("Treatment") %>%
  rename(Letter = groups) %>%
  select(Treatment, Letter)

plot_labels <- df_long %>%
  group_by(Treatment) %>%
  summarise(y_pos = max(Yield, na.rm = TRUE) + 0.5, .groups = "drop") %>%
  left_join(dt_letters, by = "Treatment")

# --- 3) Plot ---
ggplot(df_long, aes(x = Treatment, y = Yield, fill = Treatment)) +
  geom_jitter(aes(color = Treatment), width = 0.1, alpha = 0.3, size = 1.5) +
  geom_violin(alpha = 0.2, position = position_nudge(x = 0.2), trim = FALSE) +
  geom_boxplot(width = 0.15, outlier.shape = NA, alpha = 0.7, color = "black") +
  geom_text(data = plot_labels, aes(y = y_pos, label = Letter),
            vjust = 0, size = 5, fontface = "bold") +
  labs(
    title = "Comparative Analysis of Alfalfa Yield by Irrigation Method",
    subtitle = "Letters indicate significant differences (Tukey HSD, p < 0.05)",
    y = "Yield (Tons per Acre)", x = NULL
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 30, hjust = 1, size = 11),
    panel.grid.minor = element_blank()
  ) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2")

# --- 4) Summary table ---
summary_table <- df_long %>%
  group_by(Treatment) %>%
  summarise(
    n_obs = n(),
    Mean_Yield = mean(Yield, na.rm = TRUE),
    SD = sd(Yield, na.rm = TRUE),
    CV_Percent = (SD / Mean_Yield) * 100,
    .groups = "drop"
  ) %>%
  left_join(dt_letters, by = "Treatment") %>%
  mutate(
    Baseline = Mean_Yield[Treatment == "Non-Irrigated"][1],
    Tonnage_Gain = Mean_Yield - Baseline,
    Percent_Gain = 100 * (Tonnage_Gain / Baseline)
  ) %>%
  select(-Baseline) %>%
  arrange(Mean_Yield)

summary_table

df_eff <- alf_spei %>%
  transmute(
    # yields (tons/acre)
    y_nonirr   = hay_haylage_alfalfa_non_irrigated_yield_tons_per_acre_dry_basis,
    y_irr      = hay_haylage_alfalfa_irrigated_yield_tons_per_acre_dry,
    y_gra      = hay_haylage_alfalfa_irrigated_yield_tons_per_acre_dry_basis_gravity,
    y_pres     = hay_haylage_alfalfa_irrigated_yield_tons_per_acre_dry_basis_pressure,
    
    # water (acrefeet/acre)
    w_irr      = hay_haylage_alfalfa_irrigated_water_acrefeet_per_acre,
    w_gra      = hay_haylage_alfalfa_irrigated_water_acrefeet_per_acre_gravity,
    w_pres     = hay_haylage_alfalfa_irrigated_water_acrefeet_per_acre_pressure
  ) %>%
  mutate(across(everything(), ~ readr::parse_number(as.character(.)))) %>%
  # efficiencies: tons per acre-foot (higher = better)
  mutate(
    eff_irr  = if_else(!is.na(y_irr)  & !is.na(w_irr)  & w_irr  > 0, y_irr  / w_irr,  NA_real_),
    eff_gra  = if_else(!is.na(y_gra)  & !is.na(w_gra)  & w_gra  > 0, y_gra  / w_gra,  NA_real_),
    eff_pres = if_else(!is.na(y_pres) & !is.na(w_pres) & w_pres > 0, y_pres / w_pres, NA_real_)
  )
df_eff_long <- df_eff %>%
  select(eff_irr, eff_gra, eff_pres) %>%
  pivot_longer(everything(), names_to = "System", values_to = "Efficiency") %>%
  filter(!is.na(Efficiency)) %>%
  mutate(System = factor(System,
                         levels = c("eff_gra","eff_irr","eff_pres"),
                         labels = c("Gravity","Irrigated (General)","Pressure")))

anova_eff <- aov(Efficiency ~ System, data = df_eff_long)
hsd_eff <- agricolae::HSD.test(anova_eff, "System", group = TRUE)

letters_eff <- hsd_eff$groups %>%
  tibble::rownames_to_column("System") %>%
  rename(Letter = groups) %>%
  select(System, Letter)

summary_eff <- df_eff_long %>%
  group_by(System) %>%
  summarise(
    n_obs = n(),
    Mean_Eff = mean(Efficiency, na.rm = TRUE),
    SD = sd(Efficiency, na.rm = TRUE),
    CV_Percent = 100 * SD / Mean_Eff,
    .groups = "drop"
  ) %>%
  left_join(letters_eff, by = "System") %>%
  mutate(
    Baseline = Mean_Eff[System == "Gravity"][1],
    Gain_vs_Gravity = Mean_Eff - Baseline,
    Percent_Gain_vs_Gravity = 100 * Gain_vs_Gravity / Baseline
  ) %>%
  select(-Baseline) %>%
  arrange(Mean_Eff)

summary_eff

set.seed(42)
n_obs <- 50
df3 <- data.frame(
  non_irr_ton  = rnorm(n_obs, 2.5, 1.5),
  irr_gra_ton  = rnorm(n_obs, 4.3, 2.0),
  irr_ton      = rnorm(n_obs, 5.3, 2.2),
  irr_pres_ton = rnorm(n_obs, 5.8, 1.7)
)

df_long <- df3 %>%
  pivot_longer(cols = everything(), names_to = "Treatment", values_to = "Yield") %>%
  mutate(Treatment = factor(
    Treatment,
    levels = c("non_irr_ton", "irr_gra_ton", "irr_ton", "irr_pres_ton"),
    labels = c("Non-Irrigated", "Gravity", "Irrigated (General)", "Pressure")
  ))

ggplot(df_long, aes(x = Treatment, y = Yield, fill = Treatment)) +
  geom_jitter(aes(color = Treatment), width = 0.12, alpha = 0.35, size = 1.6) +
  geom_violin(alpha = 0.18, trim = FALSE) +
  geom_boxplot(width = 0.18, outlier.shape = NA, alpha = 0.7, color = "black") +
  labs(
    title = "Alfalfa Yield by Irrigation Method",,
    x = NULL, y = "Yield (tons per acre)"
  ) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 25, hjust = 1),
    panel.grid.minor = element_blank()
  ) +
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2")

