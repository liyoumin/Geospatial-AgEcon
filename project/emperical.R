#### seasonal changes observious#### robust check with 6 month SPEI (Apri-Sep)
library(fixest)
library(lmtest)
library(sandwich)
library(modelsummary)
library(readr)
library(dplyr)
library(tidyr)

alf <- read.csv("C:/Users/liyoumin/Desktop/Alfalfa drought/Estimation/df.csv")

### irr share
df <- alf %>%
  arrange(state, year) %>%
  group_by(state) %>%
  mutate(
    irr_share = case_when(
      !is.na(irrigated_acres) & !is.na(alf_acres) & alf_acres > 0 ~ irrigated_acres / alf_acres,
      TRUE ~ NA_real_
    )
  ) %>%
  fill(irr_share, .direction = "downup") %>%   # if NA, use last year; if no last year, use next year
  mutate(
    irrigation_dummy = case_when(
      !is.na(irr_share) & irr_share > 0.2 ~ 1,
      !is.na(irr_share) & irr_share <= 0.2 ~ 0,
      TRUE ~ NA_real_
    ),
  ) %>%
  ungroup()

write_csv(df, "df.csv")

## fixed effects
model <- feols(alf_yield ~ alf_spei_mean + irrigation_dummy
               + midwest + west +
                 (irrigation_dummy*alf_spei_mean) | year + stusps, data = df)

ols <- lm(alf_yield ~ alf_spei_mean + irrigation_dummy + 
            west + midwest + (irrigation_dummy*alf_spei_mean), data = df)
modelp <- feols(log(alf_production) ~ alf_spei_mean + irrigation_dummy + alf_acres +
                  midwest + west + (irrigation_dummy*alf_spei_mean)| year, data = df)
###robast check(to account for heteroskedasticity or clustering)

summary(model, se = "hetero")
summary(model, cluster = "stusps")
summary(model, cluster = "year")

summary(ols, se ="hetero")
summary(ols, cluster = "stusps")
summary(ols, cluster = "year")

summary(modelp, se = "hetero")
summary(modelp, cluster = "stusps")
#regression1 Export
df$Export_Value <- ifelse(is.na(df$Export_Value), 0, df$Export_Value)
model1 <- feols(Export_Value ~  alf_spei_mean + (irrigation_dummy*alf_spei_mean) 
                + alf_yield + alf_acres + alf_production + alf_price_usd_ton+
                  west + midwest| year, data = df)
summary(model1, se = "hetero")


#regression2
model2 <- lm(Export_Value ~ alf_spei_mean + 
               alf_yield + (irrigation_dummy*alf_spei_mean)
             + west + midwest + alf_acres + alf_production + alf_price_usd_ton, 
             data = df)
summary(model2)

#regression3
model3 <- feols(Export_Value ~ alf_spei_mean + (alf_spei_mean*irrigation_dummy) +
                 alf_acres + alf_production + alf_price_usd_ton + alf_yield| stusps + year, data = df)
summary(model3)
plot(model3)

modelsummary(list("Two-way FE (model1)" = model1,
                  "Pooled OLS (model2)" = model2),
             statistic = "std.error",
             stars = TRUE,
             gof_omit = "Adj|AIC|BIC")

## wls
alf_panel <- df |>
  mutate(
    cv_rse = CV_pct/100,
    w_inv_cv2 = 1 / (cv_rse^2 + 1e-6)  # small constant to avoid infinite weights
  )

m_wls <- feols(
  log(hay_alf_tons) ~ alf_spei_mean + (alf_spei_mean * irrigation_dummy) + irrigation_dummy 
  + midwest + west|year,
  data  = alf_panel,
  weights = ~ w_inv_cv2
)
summary(m_wls)

lmp <- feols(
  log(hay_alf_tons) ~ alf_spei_mean + irrigation_dummy + (alf_spei_mean * irrigation_dummy)
   + midwest + west | year,
  data  = df)
summary(lmp)
# 2SLS: Export on Yield, instrumenting Yield with SPEI
# hay_alfalfa_price_per_ton + irrigation + west + midwest + hay_alfalfa_production_tons +  hay_alfalfa_yield_tons_per_acre +hay_alfalfa_acres_harvested

fs <- feols(
  alf_yield ~ alf_spei_mean + irrigation_dummy + 
    (alf_spei_mean*irrigation_dummy)| stusps + year,
  data = df
)
summary(fs)

iv_model <- feols(
  Export_Value ~ alf_price_usd_ton
  | year + stusps |  
    alf_yield ~ alf_spei_mean + irrigation_dummy +(alf_spei_mean*irrigation_dummy),
  data = df
)

summary(iv_model, stage = 1)   # first stage
summary(iv_model, stage = 2)   # second stage (default)
etable(iv_model, stage = 1:2)

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
alf_panel_sf <- df %>%
  mutate(state = tolower(state)) %>%
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
    SPEI          = as.numeric(alf_spei_mean),
    irrigation    = as.numeric(irrigation_dummy),
    Alf_Price_ton = as.numeric(alf_price_usd_ton),
    alf_yield_raw = as.numeric(alf_yield),
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
    alf_value_usd     = readr::parse_number(as.character(hay_alf_usd_value)) / 1e3,
    alf_acres         = readr::parse_number(as.character(alf_acres)) / 1e3,
    alf_prod_ton      = readr::parse_number(as.character(alf_production)) / 1e3,
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
  Export_Value ~ alf_spei_mean + irrigation_dummy*alf_spei_mean +
    alf_yield_tonacre + 
    s(Alf_Price_ton, k = 10) +
    s(lon, lat, k = 10),
  data = alf_scaled,
  method = "REML"
)

summary(gam_model_spatial)
gam.check(gam_model_spatial)

# ---- Save all current R objects ----
save.image("alfalfa_spei_environment.RData")
load("alfalfa_spei_environment.RData")


### irrigation compare==========================================================================
library(tidyr)
library(agricolae)
library(ggplot2)
df2 <- df %>%
  transmute(
    non_irr_ton  = non_irr_yield,
    irr_ton      = irr_yield_tons_per_acre_general,
    irr_gra_ton  = irr_yield_tons_per_acre_method_gravity,
    irr_pres_ton = irr_yield_tons_per_acre_method_pressure
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
      labels = c("Non-Irrigated", "Gravity", "Irrigated (General)", "Pressure" )
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

df_eff <- df %>%
  transmute(
    # yields (tons/acre)
    y_nonirr   = non_irr_yield,
    y_irr      = irr_yield_tons_per_acre_general,
    y_gra      = irr_yield_tons_per_acre_method_gravity,
    y_pres     = irr_yield_tons_per_acre_method_pressure,

    # water (acrefeet/acre)
    w_irr      = water_applied_af_acre_general,
    w_gra      = water_applied_af_acre_method_gravity,
    w_pres     = water_applied_af_acre_method_pressure
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
