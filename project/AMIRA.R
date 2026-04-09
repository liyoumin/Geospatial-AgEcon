# ============================================================
# REDO SPEI PLOTTING + ARIMA PROCESS IN R
# ============================================================

install.packages(c("dplyr","ggplot2","lubridate","forecast","patchwork","zoo","scales"))

library(dplyr)
library(ggplot2)
library(lubridate)
library(forecast)
library(patchwork)
library(zoo)
library(scales)

# ============================================================
# 0. INPUT DATA
# ============================================================
# Assume your monthly data frame is called df
# Required columns:
#   stusps   = state abbreviation, e.g. "CA", "TX"
#   date     = monthly date
#   spei_var = monthly SPEI value (example: SPEI-12 or alf_spei_mean)

alf_spei <- read.csv("C:/Users/liyoumin/Desktop/Alfalfa-SPEI/County_alf_irr/alfal_spei_panel_2005_2025.csv")
# ---- CHANGE THESE TO YOUR ACTUAL COLUMN NAMES ----
df_SPEI <- alf_spei %>%
  transmute(
    stusps = stusps,                       # state abbreviation
    date   = as.Date(year),               # monthly date
    spei   = as.numeric(speimean)          # or alf_spei_mean
  ) %>%
  filter(!is.na(stusps), !is.na(date), !is.na(spei)) %>%
  arrange(stusps, date)

# If your date is not already Date format, use something like:
# date = as.Date(paste0(year_month, "-01"))

# ============================================================
# 1. DEFINE REGIONS
# ============================================================
west_states <- c("AZ","CA","CO","ID","MT","NV","NM","OR","UT","WA","WY")
midwest_states <- c("IL","IN","IA","KS","MI","MN","MO","NE","ND","OH","SD","WI")

df_SPEI <- df_SPEI %>%
  mutate(
    region = case_when(
      stusps %in% west_states    ~ "West",
      stusps %in% midwest_states ~ "Midwest",
      TRUE                       ~ "Other"
    ),
    year = year(date)
  )

# ============================================================
# 2. ANNUAL STATE-LEVEL SPEI PLOT
# ============================================================

# Annual mean SPEI by state
state_annual <- df_SPEI %>%
  group_by(stusps, region, year) %>%
  summarise(spei_annual = mean(spei, na.rm = TRUE), .groups = "drop")

# Annual regional means
region_annual <- state_annual %>%
  filter(region %in% c("West", "Midwest")) %>%
  group_by(region, year) %>%
  summarise(spei_annual = mean(spei_annual, na.rm = TRUE), .groups = "drop")

# Annual national mean
national_annual <- state_annual %>%
  group_by(year) %>%
  summarise(spei_annual = mean(spei_annual, na.rm = TRUE), .groups = "drop") %>%
  mutate(region = "National")

annual_lines <- bind_rows(region_annual, national_annual)

p_annual <- ggplot() +
  geom_line(
    data = state_annual,
    aes(x = year, y = spei_annual, group = stusps),
    color = "gray50", alpha = 0.20, linewidth = 0.8
  ) +
  geom_line(
    data = annual_lines,
    aes(x = year, y = spei_annual, color = region),
    linewidth = 2.2
  ) +
  scale_color_manual(
    values = c(
      "Midwest" = "yellow2",
      "National" = "blue",
      "West" = "red"
    )
  ) +
  scale_x_continuous(breaks = seq(min(state_annual$year), max(state_annual$year), by = 5)) +
  labs(
    title = "State-level Annual SPEI (2005–2025)",
    x = "Year",
    y = "SPEI (z-score)",
    color = "Region"
  ) +
  theme_minimal(base_size = 16)

p_annual
ggsave("state_level_annual_spei.png", p_annual, width = 12, height = 10, dpi = 300)

# ============================================================
# 3. MONTHLY NATIONAL SERIES FOR ARIMA
# ============================================================
# Important:
# Your annual plot is descriptive, but SARIMA(.,.,.)(.,.,.)[12]
# should be estimated on MONTHLY data, not annual data.

national_monthly <- df_SPEI %>%
  group_by(date) %>%
  summarise(spei = mean(spei, na.rm = TRUE), .groups = "drop") %>%
  arrange(date)

# Convert to monthly ts object
spei_ts <- ts(
  national_monthly$spei,
  start = c(year(min(national_monthly$date)), month(min(national_monthly$date))),
  frequency = 12
)

# ============================================================
# 4. TRAIN / TEST SPLIT
# ============================================================
# Use last 24 months as test set
h <- 24
n <- length(spei_ts)

train_ts <- window(spei_ts, end = time(spei_ts)[n - h])
test_ts  <- window(spei_ts, start = time(spei_ts)[n - h + 1])

train_dates <- national_monthly$date[1:(n - h)]
test_dates  <- national_monthly$date[(n - h + 1):n]

# ============================================================
# 5. FIT SARIMA MODEL
# ============================================================
# Your earlier figure showed:
# ARIMA(2,0,2)(1,0,2)[12] with zero mean

fit <- Arima(
  train_ts,
  order = c(2, 0, 2),
  seasonal = list(order = c(1, 0, 2), period = 12),
  include.mean = FALSE
)

summary(fit)

# Forecast over the test horizon
fc <- forecast(fit, h = length(test_ts))

# ============================================================
# 6. RESIDUAL DIAGNOSTICS
# ============================================================
resid_vec <- as.numeric(residuals(fit))
resid_df <- data.frame(
  date = train_dates,
  residuals = resid_vec
)

# ACF data
acf_obj <- acf(resid_vec, lag.max = 36, plot = FALSE, na.action = na.pass)
acf_df <- with(acf_obj, data.frame(
  lag = lag[,1,1],
  acf = acf[,1,1]
)) %>%
  filter(lag > 0)

acf_bound <- 1.96 / sqrt(sum(!is.na(resid_vec)))

p_resid_ts <- ggplot(resid_df, aes(x = date, y = residuals)) +
  geom_line(color = "black", linewidth = 0.8) +
  geom_point(color = "black", size = 1.6) +
  labs(
    title = "Residuals from ARIMA(2,0,2)(1,0,2)[12] with zero mean",
    x = NULL, y = NULL
  ) +
  theme_minimal(base_size = 16)

p_resid_acf <- ggplot(acf_df, aes(x = lag, y = acf)) +
  geom_hline(yintercept = 0, color = "black") +
  geom_segment(aes(xend = lag, yend = 0), linewidth = 0.8) +
  geom_hline(yintercept = c(-acf_bound, acf_bound),
             color = "blue", linetype = "dashed", linewidth = 0.8) +
  scale_x_continuous(breaks = seq(0, 36, by = 12)) +
  labs(x = "Lag", y = "ACF") +
  theme_minimal(base_size = 16)

p_resid_hist <- ggplot(resid_df, aes(x = residuals)) +
  geom_histogram(aes(y = after_stat(count)), bins = 16, fill = "gray30", color = NA) +
  geom_density(aes(y = after_stat(count) * diff(range(residuals, na.rm = TRUE)) / 16),
               color = "tomato", linewidth = 1.2, adjust = 1.1) +
  geom_rug(sides = "b") +
  labs(x = "residuals", y = "df$y") +
  theme_minimal(base_size = 16)

p_diag <- p_resid_ts / (p_resid_acf | p_resid_hist)
p_diag
ggsave("arima_residual_diagnostics.png", p_diag, width = 14, height = 10, dpi = 300)

# ============================================================
# 7. TRAIN-TEST FORECAST PLOT
# ============================================================
fc_df <- data.frame(
  date = test_dates,
  forecast = as.numeric(fc$mean)
)

plot_df_train <- data.frame(date = train_dates, value = as.numeric(train_ts), series = "Train")
plot_df_test  <- data.frame(date = test_dates,  value = as.numeric(test_ts),  series = "Test")
plot_df_fc    <- data.frame(date = test_dates,  value = as.numeric(fc$mean),  series = "Forecast")

plot_df_all <- bind_rows(plot_df_train, plot_df_test, plot_df_fc)

p_cv <- ggplot(plot_df_all, aes(x = date, y = value, color = series)) +
  geom_line(linewidth = 1.1) +
  scale_color_manual(
    values = c("Forecast" = "#F04E45", "Test" = "#00A651", "Train" = "#4A7DF2")
  ) +
  labs(
    title = "ARIMA Cross-Validation (SPEI)",
    x = "Year",
    y = "SPEI-12",
    color = "Series"
  ) +
  theme_minimal(base_size = 16)

p_cv
ggsave("arima_cross_validation_spei.png", p_cv, width = 12, height = 9, dpi = 300)

# ============================================================
# 8. ALTERNATIVE VERSION WITH BLACK TRAIN LINE
# ============================================================
p_cv2 <- ggplot() +
  geom_line(data = plot_df_train, aes(date, value, color = "Train"), linewidth = 1.1) +
  geom_line(data = plot_df_test,  aes(date, value, color = "Test"), linewidth = 1.1) +
  geom_line(data = plot_df_fc,    aes(date, value, color = "Forecast"), linewidth = 1.1) +
  scale_color_manual(
    values = c("Forecast" = "#F04E45", "Test" = "#19B7C0", "Train" = "black")
  ) +
  labs(
    title = "ARIMA Cross-Validation (SPEI)",
    x = "Time",
    y = "train",
    color = "series"
  ) +
  theme_minimal(base_size = 16)

p_cv2
ggsave("arima_cross_validation_spei_alt.png", p_cv2, width = 12, height = 9, dpi = 300)

# ============================================================
# 9. ROLLING TIME-SERIES CROSS-VALIDATION
# ============================================================
# This is real time-series CV, not just one holdout split.

sarima_fc <- function(y, h) {
  fit_cv <- Arima(
    y,
    order = c(2, 0, 2),
    seasonal = list(order = c(1, 0, 2), period = 12),
    include.mean = FALSE
  )
  forecast(fit_cv, h = h)$mean
}

# initial window = 60 months
cv_errors <- tsCV(
  y = spei_ts,
  forecastfunction = sarima_fc,
  h = 12,
  initial = 60
)

cv_rmse <- sqrt(colMeans(cv_errors^2, na.rm = TRUE))
cv_mae  <- colMeans(abs(cv_errors), na.rm = TRUE)

cv_results <- data.frame(
  Horizon = 1:12,
  RMSE = cv_rmse,
  MAE = cv_mae
)

print(cv_results)

# ============================================================
# 10. OPTIONAL: Ljung-Box test
# ============================================================
Box.test(resid_vec, lag = 24, type = "Ljung-Box", fitdf = 7)

# fitdf = number of estimated ARIMA parameters here:
# AR 2 + MA 2 + seasonal AR 1 + seasonal MA 2 = 7