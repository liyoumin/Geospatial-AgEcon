############################################################
##  USDA-NASS 
##  Load data, decompose time series, fit ARIMA model and forecast, assess accuracy
##  Data available here: https://data.giss.nasa.gov/gistemp/
############################################################



############################################################
##  Load Libraries
##  ggplot2, forecast
############################################################
library(ggplot2)
library(forecast)
library(dplyr)
library(lubridate)

###########################################################
##  Set working directory
##  Load data
##  Create time series
###########################################################

path <- "/Users/macpro/Desktop/Youmin-phd/geospatical/Project/"
setwd(paste(path, "data/", sep = ""))

##  Read in csv
spi <- read.csv("SPI-Total.csv")
###transfer date
spi <- spi %>%
  mutate(
    # remove the "d_" prefix
    DATE_clean = sub("d_", "", DATE),
    # parse as date
    DATE_clean = ymd(DATE_clean),
    # format to mm/dd/yyyy
    DATE_formatted = format(DATE_clean, "%Y/%m/%d")
  )


##  Transpose columns and create vector
dought <- as.vector(t(spi$D3))

##  Create time series object from vector
dought.ts <- ts(dought, start = c(1900, 01), end = c(2025, 09), frequency = 12)

##  Plot time series
plot(dought.ts)


##########################################################
##  Explore time series
##  Decompose time series
##########################################################


##  Time series have 3 components: trend, seasonality, random error (irregular fluctuations)

##  Explore TREND
##  Fit regression to series
abline(reg = lm(dought.ts ~ time(dought.ts)), col = "red", lwd = 2)
##  Calculate annual mean
plot(aggregate(dought.ts, FUN = mean))
abline(reg = lm(dought.ts ~ time(dought.ts)), col = "red", lwd = 2)
##  Remove trend by taking first difference (change in value across months)
plot(diff(dought.ts))

##  Explore SEASONALITY
plot(dought.ts)
dought.subset <- window(dought.ts, start = c(2000, 01), end = c(2025, 09))
plot(dought.subset)
##  Create box plots for months
boxplot(dought.ts~cycle(dought.ts))
##  Try season plot from ggplot2 package
ggseasonplot(diff(dought.subset))

##  Another way of decomposing time series
dought.subset1 <- window(dought.ts, start = c(1980, 1), end = c(2024, 12))
dought.ts.decomp <- decompose(dought.subset1, type = "multiplicative")
plot(dought.ts.decomp)


##########################################################
##  Fit ARIMA model to time series
##  Perform model diagnostics
##  Forecast future using model
##########################################################

##  ARIMA - Auto Regressive Integrated Moving Average
##  Data must be stationary - trend and seasonality must be removed
##  Can do this manually using diff() for trend and log for seasonality
##  Here we will use an automated method that takes care of stationarity for us
model <- auto.arima(dought.ts)
model

##  Examine model diagnostics
##  Plot ACF of model residuals to see if autocorrelation removed
##  Further test autocorrelation using Ljung-Box test
##  Examine histogram of residuals for Gaussian distribution

checkresiduals(model)

##  Forecast using ARIMA model and plot

##  Forecast out # of time steps
f <- forecast(model, h = 60)
summary(f)
str(f)
f$mean


##  Plot

plot(f)
autoplot(f)
autoplot(f, include = 240)

##  Tidy up the plot
autoplot(f, include = 240) + ggtitle("U.S. 9 month SPI forecast") +
  xlab("Time (year)") + ylab("U.S. extreme dought(D3) 2005-2025 mean") +
  theme_bw() + theme(plot.title = element_text(hjust = 0.5)) + 
  scale_x_continuous(breaks = seq(from = 2000, to = 2025, by = 2))

##########################################################
##  Perform accuracy assessment
##########################################################

##  Assess model accuracy
accuracy(f)

##  Cross Validation
##  Create training and testing sets
train <- head(dought.ts, round(length(dought.ts) * 0.8))
t <- length(dought.ts) - length(train)
test <- tail(dought.ts, t)

## Plot training and test series
autoplot(train) + autolayer(test)

##  Re-fit ARIMA using training data only
modeltrain <- auto.arima(train)

##  Forecast through length of test set
ftrain <- forecast(modeltrain, h = length(test))

##  Assess accuracy of ARIMA model in simulating known test set
accuracy(ftrain, test)

## Plot
autoplot(train) + autolayer(ftrain) + autolayer(test)


##  Tidy up the plot a little
autoplot(ftrain$mean, ylab = "Temperatures (C) relative to 1950-1980 mean") + 
  geom_ribbon(aes(ymin = ftrain$lower[,1], ymax = ftrain$upper[,1]), alpha=0.25) +
  geom_ribbon(aes(ymin = ftrain$lower[,2], ymax = ftrain$upper[,2]), alpha=0.2) + 
  autolayer(test, series = "Test Data") + autolayer(train, series = "Training Data") + 
  autolayer(ftrain$mean, series = "Forecast") + 
  ggtitle("Global Temperatures Forecast Accuracy") + theme_bw() + 
  theme(plot.title = element_text(hjust = 0.5)) + 
  scale_color_manual(values = c("red", "blue", "black"))

#########################################################
##  Save output and time series data
#########################################################

save.image(paste(path, "output/SPI_time_series.RData", sep = ""))

##  How to re-load a RData file
rm(SPI.ts)
prior <- load(paste(path, "output/SPI_time_series.RData", sep = ""), verbose =TRUE)


############################################################
############################################################
library(zoo)
library(xts)
library(forecast)
###########################################################
###########################################################

path <- "/Users/macpro/Desktop/Youmin-phd/geospatical/Time Series Wilmott Matsuura/"
all_data <- data.frame()
setwd(paste(path, "data/", sep = ""))
files <- dir()

##  For each file in directory assign year in file name to year, 
##  read it in, assign headers, bind to all_data

for(file in files){
  year <- substr(file, 8, 11)
  year_data <- read.table(file)
  names(year_data) <- c("lon", "lat", "jan", "feb", "mar", "apr", "may", 
                        "jun", "jul", "aug", "sep", "oct", "nov", "dec", "ann_total")
  year_data[,"year"] <- as.numeric(year)
  all_data <- rbind(all_data, year_data)
  
}


###########################################################
##  Prep data for analysis
##  Subset all_data to location in Southern Africa
##  Remove non-time related columns and create time series
###########################################################


##	Extract data from a spatial subset:  -17.75 deg. S, and 23.25 deg. E:
subset_indices <- (all_data[,"lon"] == 23.25 & all_data[,"lat"] == -17.75)
subset <- all_data[subset_indices,]

##	Drop the lat/long, ann_total, and year columns from the dataset:
subset <- subset[,c(-1,-2,-15,-16)]

##  Transpose columns and create vector
subset <- as.vector(t(subset))

##  Create time series object from vector
precip.ts <- ts(subset, start = c(2000, 1), end = c(2017, 12), frequency = 12)


##########################################################
##  Perform Analysis
##  Basic Plots of time series
##  Estimate Autocorrelation Function
##########################################################

##	Perform some basic plotting and analyses:
plot(precip.ts)

##  Let's tidy up that plot a little
plot(precip.ts, xlab = "Date", ylab = "Precip. (mm)", axes = FALSE)
axis(1, at = seq(2000, 2017, by=1), cex.axis = 0.7)
axis(2, at = seq(0, 400, by=50), cex.axis = 0.7)
abline(v = seq(2000, 2017, by=1), h = seq(0, 400, by=50), col = "grey", lty = 3)
lines(precip.ts)
box()

##  Subset the time series to 2004 and plot
precip.subset <- window(precip.ts, start = c(2004,1), end = c(2004, 12))
plot(precip.subset)
plot(precip.subset, xaxt = "n")
axis(1, at =seq(2004.0, 2004.99, by=1/12), labels = c("jan", "feb", "mar", "apr", "may", 
                                                      "jun", "jul", "aug", "sep", "oct", "nov", "dec"))

##  Another (easier) way to subset a time series using xts package
precip.xts <- as.xts(precip.ts)
plot(precip.xts)
plot(precip.xts["2004"])
plot(rollmean(precip.xts, 3))

##  Sum function applied at yearly period in time series
yearly_total <- apply.yearly(precip.xts, "sum")
plot(yearly_total)

##  Estimate Autocorrelation Function  
acf(precip.xts)

#########################################################
##  Save output and time series data
#########################################################

save(precip.xts, file="precip_xts.rda")

dev.off()





