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
alf_panel <- read.csv("/Users/macpro/Desktop/Youmin-phd/Geospatical/Project/data1/alf_panle.csv")

### extract data from CDL
us_states <- tigris::states(cb = TRUE, class = "sf") %>%
  mutate(NAME = tolower(NAME))
cdl <- rast("/Users/macpro/Desktop/Youmin-phd/geospatical/project/data/2024_30m_cdls/2024_30m_cdls.tif")
print(cdl)
# USDA CDL code for Alfalfa = 36
#  Load state polygons and match projection
states <- states(cb = TRUE, resolution = "20m") |>
  st_transform(crs(cdl)) |>
  vect()
us_states <- states(cb = TRUE) %>%     # cb = simplified boundaries
  st_transform(4326)                   # WGS84 coordinate system

alf_acres <- lapply(1:nrow(states), function(i) {
  s <- states[i]
  r_crop <- crop(cdl, s)
  v <- values(r_crop, mat = FALSE, na.rm = TRUE)
  acres <- sum(v == 36) * 0.222394
  data.frame(State = s$NAME, Acres_Alfalfa = acres)
}) |> bind_rows()
write.csv(alf_acres, "cdl_alf_acres.csv", row.names = FALSE)


###cropscaprR
library(CropScapeR)
# 2-digit FIPS codes for 48 contiguous U.S. states + DC
state_fips <- sprintf("%02d", c(
  1:13, 15:42, 44:51, 53:56  # skip 14, 43, 52
))
years <- 2018:2024
alf_list <- list()
for (y in years) {
  message("Downloading CDL stats for ", y, "...")
  year_data <- lapply(state_fips, function(fip) {
    tryCatch({
      d <- GetCDLStat(aoi = fip, year = y, type = "f")
      d <- subset(d, Crop == "Alfalfa")
      d$FIPS <- fip
      d$Year <- y
      d
    }, error = function(e) {
      message("  Failed for FIPS ", fip, " in ", y)
      NULL
    })
  })
  alf_list[[as.character(y)]] <- do.call(rbind, year_data)
}

alf_state_year <- do.call(rbind, alf_list)
alf_state_year <- alf_state_year[, c("FIPS", "State", "Crop", "Acres", "Year")]
head(alf_state_year)
# Save
write.csv(alf_state_year, "cdl_alfalfa_stateyear_2018_2024.csv", row.names = FALSE)
url <- "https://nassgeodata.gmu.edu/CropScapeService/wcsserver/GetCDLStat?year=2018&fips=06&type=f"
readLines(url, n = 10)

#-------------------------------------------
### SPEI data
spei_rast <- nc_open("/Users/macpro/Desktop/Youmin-phd/geospatical/project/data/nclimgrid-spei-gamma-12.nc")
print(spei_rast)
spei <- ncvar_get(spei_rast, "spei_12")
# ----  Load raster ----
spei_rast <- rast("/Users/macpro/Desktop/Youmin-phd/geospatical/project/data/nclimgrid-spei-gamma-12.nc")
spei_sub <- spei_rast[[which(time(spei_rast) >= as.Date("2005-01-01") &
                               time(spei_rast) <= as.Date("2025-12-31"))]]
spei_lowres <- aggregate(spei_sub, fact = 6, fun = mean, na.rm = TRUE)
# ---- Restore time after aggregation ----
time(spei_lowres) <- time(spei_sub)
# after you have spei_lowres created
tvals <- time(spei_lowres)
# Recreate years from restored time
years <- year(time(spei_lowres))
# Compute annual mean SPEI
spei_annual <- tapp(spei_lowres, index = years, fun = mean, na.rm = TRUE)
spei_annual
names(spei_annual)
time(spei_annual)
# Create Date objects for each year label
time(spei_annual) <- as.Date(paste0(2005:2025, "-01-01"))
# ---- Extract mean SPEI for each state & year ----
states <- states(cb = TRUE, resolution = "20m") |>
  st_transform(crs(spei_annual)) |>
  vect()
#  Extract mean SPEI per state
state_year_spei <- terra::extract(spei_annual, states, fun = mean, na.rm = TRUE)
state_year_spei <- cbind(State = states$NAME, state_year_spei)
#  Convert to tidy format
spei_df <- state_year_spei |>
  pivot_longer(cols = starts_with("X"),
               names_to = "Year",
               values_to = "SPEI") |>
  mutate(Year = as.numeric(sub("X", "", Year))) |>
  select(State, Year, SPEI)

write.csv(spei_df, "state_spei_2005_2025.csv", row.names = FALSE)
# ---- Quick visualization ----
ggplot(spei_df, aes(Year, SPEI, group = State)) +
  geom_line(alpha = 0.25) +
  geom_smooth(aes(group = 1), color = "blue", se = FALSE) +
  theme_minimal() +
  labs(title = "State-level Annual SPEI (2005–2025)", y = "SPEI (z-score)")
table(spei_df$State)

spei_df <- read.csv("/Users/macpro/Desktop/Youmin-phd/Geospatical/Project/data1/state_spei_2005_2025.csv")
### merge panel, cdl and spei to panel
# correct data
alf_panel <- alf_panel %>%
  mutate(
    alf_prod_correct = if_else(
      !is.na(CV_prod_ton) & CV_prod_ton > 30,
      ALFALFA...PRODUCTION..MEASURED.IN.TONS * (100 - CV_prod_ton) / 100,
      ALFALFA...PRODUCTION..MEASURED.IN.TONS
    )
  )

alf_panel <- alf_panel %>%
  mutate(
    alf_acres_correct = if_else(
      !is.na(CV_alf_acres) & CV_alf_acres> 30,
      ALFALFA_HAYLAGE_ACRES * (100 - CV_prod_ton) / 100,
      ALFALFA_HAYLAGE_ACRES
    )
  )

alf_panel <- alf_panel %>%
  mutate(
    prod_per_acre = if_else(
      !is.na(alf_acres_correct) & alf_acres_correct > 0,
      alf_prod_correct / alf_acres_correct,
      NA_real_
    )
  )

alf_panel <- alf_panel %>%
  mutate(
    alf_yield_tonacre = if_else(
      is.na(alf_yield_tonacre) & prod_per_acre < 8,
      prod_per_acre,
      alf_yield_tonacre
    )
  )

alf_panel <- alf_panel %>%
  mutate(
    irrigation = if_else(
      rowSums(!is.na(select(., starts_with("irr_")))) > 0,  # any irr_ column not NA
      1L,                                                   # mark irrigated
      0
    )
  )


# ---- Merge with pesi ----
library(stringr)
alf_panel <- alf_panel %>%
  mutate(
    State = str_to_title(State),
    Year  = as.integer(Year)
  )
alf_spei <- alf_panel %>%
  left_join(spei_df, by = c("State", "Year"))

alf_spei <- alf_spei %>%
  mutate(
    irrigation = as.numeric(irrigation),
    irr_pres_water_acrefeet = as.numeric(irr_pres_water_acrefeet),
    irr_gra_water_acrefeet = as.numeric(irr_gra_water_acrefeet),
    alf_yield_tonacre = as.numeric(alf_yield_tonacre),
    irr_gra_ton = as.numeric(irr_gra_ton),
    irr_ton = as.numeric(irr_ton),
    irr_pres_ton = as.numeric(irr_pres_ton),
    irr_pres_water_acrefeet = as.numeric(irr_pres_water_acrefeet),
    irr_water_acrefeet = as.numeric(irr_gra_water_acrefeet),
    irr_gra_water_acrefeet = as.numeric(irr_gra_water_acrefeet),
    irr_acres = as.numeric(irr_acres),
    ALFALFA...PRODUCTION..MEASURED.IN.TONS = as.numeric(ALFALFA...PRODUCTION..MEASURED.IN.TONS),
    ALFALFA_HAYLAGE_ACRES = as.numeric(ALFALFA_HAYLAGE_ACRES),
    ALFALFA_IRRIG_ACRES = as.numeric(ALFALFA_IRRIG_ACRES),
    ALFALFA_IRR_YIELD_TON_ACRE =as.numeric(ALFALFA_IRR_YIELD_TON_ACRE)
  )
alf_spei <- alf_spei %>%
  mutate(
    irrigation = if_else(
      rowSums(!is.na(select(., starts_with("irr_")))) > 0,
      1, 0
    )
  )
alf_spei <- alf_spei %>%
  mutate(State = stringr::str_to_title(State))
us_states <- states(cb = TRUE) %>%
  filter(!STUSPS %in% c("AK", "HI", "PR", "GU", "VI", "AS", "MP")) %>%
  st_transform(4326)  # WGS84 CRS (good for mapping)
alf_spei <- left_join(
  st_drop_geometry(us_states),  # remove geometry first
  alf_spei,
  by = c("NAME" = "State")
)
alf_spei <- st_transform(alf_spei, 5070)  # NAD83 / Conus Albers
alf_spei %>%
  st_drop_geometry() %>%
  summarise(
    n_states = n_distinct(NAME),
    n_years = n_distinct(Year)
  )
alf_spei <- alf_spei %>%
  filter(!STUSPS %in% c("AK", "HI", "PR", "GU", "VI", "AS", "MP"))

#  Write CSV
write.csv(alf_panel, "alf_panel.csv", row.names = FALSE)
write.csv(alf_spei, "alf_panel_with_spei.csv", row.names = FALSE)

##optional transfer coordinator
#cdl <- projectRaster(cdl, crs="+proj=utm +zone=20 +ellps=clrk66 +towgs84=11,72,-101,0,0,0,0 +units=m +no_defs ")
#writeRaster(cdl, filename="/Users/macpro/Desktop/Youmin-phd/geospatical/project/data/")
#-----------------------------------------------------------------------------------------------------------------
# Exploratory plot of SPEI vs. value, ton/acre
summary(alf_spei$irrigation == 1)
plot(irr_water_acrefeet ~ Export_Value, data=alf_spei)
# Pearson's product-moment correlation
cor(alf_spei$alf_yield_tonacre, alf_spei$Export_Value, use = "pairwise.complete.obs")
cor.test(alf_spei$alf_yield_tonacre, alf_spei$SPEI, use = "pairwise.complete.obs")

###------------spatial correlation - Variogram - gloabl morans --- Auto-kriging - local moran's ---- CV fitness
# Explore semivariogram of elevation
st_geometry(alf_spei)
plot(st_geometry(alf_spei))
alf_spei_pts <- st_centroid(alf_spei)
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
model <- feols(alf_yield_tonacre ~ SPEI + irrigation | state + year, data = alf_spei)

###robast check(to account for heteroskedasticity or clustering)
summary(model, se = "hetero")
summary(model, cluster = "state")
summary(model, cluster = c("state", "year"))

#regression1 price collinear
model1 <- feols(Export_Value ~ SPEI + Alf_Price_ton + alf_yield_tonacre | state, data = alf_spei)
summary(model1, cluster = 'state')

#regression2
model2 <- feols(Export_Value ~ SPEI + irrigation | state + year, data = alf_spei)
summary(model2)

#regression3
model3 <- lm(Export_Value ~ SPEI + Alf_Price_ton + irrigation, data = alf_spei )
summary(model3)
plot(model3)

modelsummary(list("Two-way FE (model2)" = model2,
                  "Pooled OLS (model3)" = model3),
             statistic = "std.error",
             stars = TRUE,
             gof_omit = "Adj|AIC|BIC")

## regression4
model4 <- feols(Export_Value ~ SPEI + Alf_Price_ton + irrigation, data = alf_spei)
etable(model2, model4)
## gam function
library(mgcv)
alf_spei_centroid <- alf_spei %>%
  mutate(centroid = st_centroid(geometry)) %>%
  mutate(
    lon = st_coordinates(centroid)[, 1],
    lat = st_coordinates(centroid)[, 2]
  ) %>%
  st_drop_geometry()

alf_spei_clean <- alf_spei_centroid %>%
  mutate(
    Export_Value  = as.numeric(Export_Value),
    SPEI          = as.numeric(SPEI),
    irrigation    = as.numeric(irrigation),
    Alf_Price_ton = as.numeric(Alf_Price_ton),
    lon           = as.numeric(lon),
    lat           = as.numeric(lat)
  ) %>%
  filter(!is.na(Export_Value), !is.na(SPEI))


gam_model_spatial <- gam(
  Export_Value ~ SPEI + alf_yield_tonacre + s(Alf_Price_ton, k = 5) +
    s(lon, lat, k = 10),   # reduce k dramatically
  data = alf_spei_clean,
  method = "REML"
)
summary(gam_model_spatial)

alf_spei_scaled <- alf_spei_clean %>%
  mutate(
    # Convert and scale numeric-like text columns
    alf_value_usd  = readr::parse_number(as.character(alf_value_usd)) / 1e6,   # millions
    alf_acres      = readr::parse_number(as.character(alf_acres)) / 1e3,       # thousand acres
    alf_prod_ton   = as.numeric(alf_prod_ton) / 1e3,                           # thousand tons
    Alf_Price_ton  = scale(Alf_Price_ton),
    alf_yield_tonacre = scale(alf_yield_tonacre),
    SPEI = scale(SPEI),
    lon  = scale(lon),
    lat  = scale(lat)
  ) %>%
  filter(
    is.finite(Export_Value),
    is.finite(SPEI),
    is.finite(Alf_Price_ton),
    is.finite(alf_yield_tonacre),
    is.finite(lon),
    is.finite(lat)
  )



### mapping
## average SPEI

####
# ---- Save all current R objects ----
save.image("alfalfa_spei_environment.RData")
load("alfalfa_spei_environment.RData")

