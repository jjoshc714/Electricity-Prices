# Load Required Libraries
library(zoo)
library(forecast)
library(ggplot2)
library(gridExtra)
library(tseries)
library(vars)
library(strucchange)
library(stats)
library(rugarch)
library(httr)
library(jsonlite)

# Set Working Directory
setwd("C:/Users/jjoshuac714/Desktop/Econ 144/Week 7/Project 2")

# Set Up Data
elec <- read.csv("APU000072610.csv", stringsAsFactors = FALSE)
elec$APU000072610 <- as.numeric(elec$APU000072610)
colnames(elec) <- c("date", "price") # rename columns for readability

loc <- which(elec$price %in% NA)
elec[loc,]               # locate NA value(s)

elecp <- ts(elec$price, start = c(1978, 11), end = c(2020, 01), freq = 12)
x1 <- na.StructTS(elecp) # interpolation using seasonal Kalman filter
x2 <- na.interp(elecp)   # linear interpolation of seasonally adjusted data

x <- (x1[83] + x2[83])/2 # average of methods
elecp[loc] <- x          # impute into data

# a) Plot Data, ACF, and PACF Graphs
t0 <- ts(elecp, start = c(1978, 11), end = c(2020, 01), freq = 12) # create time sequence corresponding to data
t0 <- as.numeric(time(t0))

ggplot() + geom_line(aes(x = t0, y = as.numeric(elecp))) + ggtitle("Electricity Price") + xlab("Time") + 
      ylab("$ / KWH") + scale_x_continuous(breaks = seq(1980, 2020, 5)) + scale_y_continuous(limits = c(0.04, 0.15), breaks = seq(0.04, 0.15, .02))

grid.arrange(ggAcf(elecp, lag = 25) + ggtitle("ACF") + ylab(""), 
             ggPacf(elecp, lag = 25) + ggtitle("PACF") + ylab(""))

# b) Model Creation with Trend, Seasonality, and Cyclical Components
plot(stl(elecp, s.window = "periodic"), main = "elecp") # loess decomposition of data

## split data into train (~80%) and test (~20%)
lelecp <- log(elecp) # take natural log of data

head(time(lelecp),12)
quantile(time(lelecp), probs = .8)
train <- window(lelecp, end = 2011 + 10/12)
test <- window(lelecp, start = 2011 + 11/12)

t <- ts(train, start = c(1978, 11), end = c(2011, 11), freq = 12)
t <- as.numeric(time(t))

## trend
trend1 <- lm(exp(train) ~ t)
trend2 <- lm(train ~ t)
trend3 <- lm(train ~ t + I(t^2))

summary(trend1)
summary(trend2)
summary(trend3)
AIC(trend1, trend2, trend3) ; BIC(trend1, trend2, trend3) # summary statistics and fits

## trend + seasonality
ggseasonplot(train) # seasonal visualization

season1 <- tslm(train ~ season)

plot(season1$coef, type = "l", xlab = "Season", ylab = "Seasonal Factors", lwd = 2, col = "steelblue3")
axis(side = 1, seq(1, 12, 2))

t3_season1 <- tslm(train ~ t + season) # combining trend and seasonal dummies
summary(t3_season1)

## cycle
ggplot() + geom_line(aes(x = t, y = as.numeric(train))) + 
      geom_line(aes(x = t, y = as.numeric(t3_season1$fit)), col = "red") +
      xlab("Time") + ylab("log(elecp)") + ggtitle("Electricity Prices vs Trend + Season Model")

cycle0 <- t3_season1$res # de-trended and seasonalized noise

ggplot() + geom_line(aes(x = t, y = as.numeric(cycle0))) + ylab("log(elecp)") + xlab("Time") + ggtitle("Trend + Seasonal Model Residuals")

grid.arrange(ggAcf(cycle0, lag = 36) + ggtitle("ACF") + ylab(""), ggPacf(cycle0, lag = 36) + 
                   ggtitle("PACF") + ylab(""))

Box.test(cycle0, lag = 24, type = "Ljung-Box")
Box.test(cycle0, lag = 24, type = "Box-Pierce") #reject null hypothesis that lags are an uncorrelated, thus noise (possibly cyclical) exists and needs to be accounted for.

## model selection based on residual plots
cycle1 <- arima(cycle0, order = c(1,0,1), seasonal = list(order = c(2,0,2))) # ARMA(1,1) + S-ARMA(2,2)
grid.arrange(ggAcf(cycle1$res, lag = 24) + ggtitle("cycle1 ACF") + ylab(""), ggPacf(cycle1$res, lag = 24) + 
                   ggtitle("cycle1 PACF") + ylab(""))
## function that cross-validates monthly data specified by an arima order and outputs MAPE of p-ahead forecasts and MAPE of entire rolling LOOCV forecasts.
source("mape_arima_monthly.R")

MAPE_ARIMA(lelecp, train, test, cycle1, 12, "CSS-ML") # run function on first model
MAPE1_mod <- MAPE_mod
MAPE1_12 <- MAPE_p

cycle2 <- auto.arima(cycle0)
grid.arrange(ggAcf(cycle2$res, lag = 24) + ggtitle("cycle2 ACF") + ylab(""), ggPacf(cycle2$res, lag = 24) + 
                   ggtitle("cycle2 PACF") + ylab(""))

MAPE_ARIMA(lelecp, train, test, cycle2, 12, "CSS") # run function on second model
MAPE2_mod <- MAPE_mod
MAPE2_12 <- MAPE_p

cycle3 <- arima(cycle0, order = c(5,0,5), seasonal = list(order = c(1,0,1)))
grid.arrange(ggAcf(cycle3$res, lag = 24) + ggtitle("cycle3 ACF") + ylab(""), ggPacf(cycle3$res, lag = 24) + 
                   ggtitle("cycle3 PACF") + ylab(""))

MAPE_ARIMA(lelecp, train, test, cycle3, 12, "CSS") # run function on third model
MAPE3_mod <- MAPE_mod
MAPE3_12 <- MAPE_p

AIC(cycle1, cycle2, cycle3) ; BIC(cycle1, cycle2, cycle3) # training fit results
rbind("Window LOOCV" = c(MAPE1 = MAPE1_mod, MAPE2 = MAPE2_mod, MAPE3 = MAPE3_mod), "12 step" = c(MAPE1_12, MAPE2_12, MAPE3_12)) # testing results

mod1_order <- arimaorder(cycle3)
mod1 <- arima(lelecp, order = mod1_order[1:3], seasonal = mod1_order[4:6], method = "CSS") # build arima model using cycle3 which performed the best in testing

# c) Residuals vs Fitted Values Plot
ggplot() + geom_point(aes(x = as.numeric(fitted(mod1)), y = as.numeric(mod1$res))) + ggtitle("Full Model") + xlab("Fitted Values") + ylab("Residuals") +
      geom_line(aes(x = as.numeric(fitted(mod1)), y = 0), col = "red3")

# d) ACF and PACF
grid.arrange(ggAcf(mod1$res) + ggtitle("ACF") + ylab(""), ggPacf(mod1$res) + ggtitle("PACF") + ylab(""))

Box.test(mod1$res, lag = 24, type = "Ljung-Box")
Box.test(mod1$res, lag = 24, type = "Box-Pierce") # reject null hypothesis that cycle1 lags are an uncorrelated process

# e) Recursive Cumulative Sum (CUSUM) Plot
plot(efp(mod1$res ~ 1, type = "Rec-CUSUM"), ylab = "") # the coefficients of model seem to stay stable throughout the whole time period of modeling

## This plot recursively sums the residuals over time cumulatively which reveals that the errors start to become negative at about the 1970s (possibly overestimation due to unexpected 1970s crash) and hits lows in the early 2000s (possibly due to dot-com bubble bursting and 2008 financial crisis). This shows model stability in that there was no shock that broke through the red variance bands. A note on a possible explanation on the positive spike in early 2000s is the California energy crisis in 2000/2001 which was caused by lack of electricity supply and historically high prices.   

# f) Recursive Residuals Plot
plot(recresid(mod1$res ~ 1), pch = 16, main = "Recursive Residuals", ylab = "") # The errors reflect a somewhat stable model with errors about a mean of 0 with some volatility.

# g) Model Summary and Statistics
summary(mod1)

# h) 12 Steps Ahead Forecast with Error Bands
fcst <- forecast(mod1, h = 12, lambda = 0) # 12 steps ahead forecast of model
elecp_fcst <- fcst$mean

elecp_fcst_upper95 <- exp(fcst$upper[,2]) # prediction interval bands of forecast
elecp_fcst_lower95 <- exp(fcst$lower[,2])
elecp_fcst_lower80 <- exp(fcst$lower[,1])
elecp_fcst_upper80 <- exp(fcst$upper[,1])
t2 <- as.numeric(time(fcst$mean))         # create time vector corresponding to 12-step predictions

ggplot() + geom_line(aes(x = t0, y = as.numeric(elecp))) + geom_line(aes(x = t2, y = elecp_fcst), col = "blue") +
   geom_ribbon(aes(x = t2, ymin = elecp_fcst_lower95, ymax = elecp_fcst_upper95) , alpha = 0.15) +
   geom_ribbon(aes(x = t2, ymin = elecp_fcst_lower80, ymax = elecp_fcst_upper80), alpha = 0.15) +
   scale_fill_manual(values = c("red"), name = "fill") +
   ggtitle("Electricity Prices 12-Step Ahead Forecast") + xlab("Time") + ylab("$ / KWH")

# closer look at forecast plot
ggplot() + geom_line(aes(x = t2, y = as.numeric(elecp_fcst)), col = "blue") +
   geom_ribbon(aes(x = t2, ymin = elecp_fcst_lower95, ymax = elecp_fcst_upper95) , alpha = 0.15) +
   geom_ribbon(aes(x = t2, ymin = elecp_fcst_lower80, ymax = elecp_fcst_upper80), alpha = 0.15) +
   scale_fill_manual(values = c("red"), name = "fill") +
   ggtitle("Electricity Prices 12-Step Ahead Forecast") + xlab("Time") + ylab("$ / KWH") +
   scale_y_continuous(limits = c(0.12, 0.15), breaks = seq(0.12, 0.15, 0.005))

elecp_fcst 

# i) Model Performance Comparisons
train12 <- window(lelecp, end = c(2019, 1)) # data leaving the recent 12 months out
p = length(lelecp) - length(train12) 

MOD1 <- forecast(arima(train12, order = mod1_order[1:3], seasonal = mod1_order[4:6], method = "CSS"), h = p)
ARIMA <- forecast(auto.arima(train12), h = p) # forecast using auto.arima
ETS <- forecast(ets(train12), h = p)          # forecast using Error, Trend, Seasonal (ETS) method of exponential smoothing
HW <- forecast(HoltWinters(train12), h = p)   # forecast using Holt-Winters of triple exponential smoothing

t3 <- as.numeric(time(ARIMA$mean))
last12 <- window(lelecp, start = c(2019, 2))

dat1 <- data.frame(Time = t3, lelecp = as.numeric(last12), Method = "Actual")
dat2 <- data.frame(Time = t3, lelecp = as.numeric(MOD1$mean), Method = "Mod1")
dat3 <- data.frame(Time = t3, lelecp = as.numeric(ARIMA$mean), Method = "Auto Arima")
dat4 <- data.frame(Time = t3, lelecp = as.numeric(ETS$mean), Method = "ETS")
dat5 <- data.frame(Time = t3, lelecp = as.numeric(HW$mean), Method = "Holt-Winters")
dat_all <- rbind(dat1, dat2, dat3, dat4, dat5) # data frame containing results of all forecasts

ggplot(dat_all, aes(x = Time, y = lelecp, col = Method)) + geom_line() + 
   theme(legend.position = c(0.85, 0.85)) + 
   scale_color_manual(values = c("black", "blue", "red", "orange", "green")) +
   ggtitle("12-Step Forecast Model Comparison") # plot of all forecasts including actual 

MAPES <- c(Mod1 = accuracy(MOD1, lelecp)[2, "MAPE"], "Auto Arima" = accuracy(ARIMA, lelecp)[2, "MAPE"], 
  ETS = accuracy(ETS, lelecp)[2, "MAPE"], "Holt-Winters" = accuracy(HW, lelecp)[2, "MAPE"])
MAPES # produce MAPES of all forecasts and display

# j) Forecast Combination and Comparison
fcst_combo <- (MOD1$mean + ARIMA$mean + ETS$mean + HW$mean)/4 # combined forecasts

dat6 <- data.frame(Time = t3, lelecp = as.numeric(fcst_combo), Method = "Combination")
dat_all2 <- rbind(dat_all, dat6)

ggplot(dat_all2, aes(x = Time, y = lelecp, col = Method)) + geom_line() + 
   theme(legend.position = c(0.85, 0.85)) + 
   scale_color_manual(values = c("black", "blue", "red", "orange", "green", "brown4")) +
   ggtitle("12-Step Forecast Model Comparison") # previous plot including combination forecast

MAPE_combo <- mean(abs((last12 - fcst_combo)/(last12)))*100

c(MAPES, Combination = MAPE_combo)

# k) Fit Vector Autoregression (VAR) Model
load("key.RData")
api <- key # API key to access data from U.S Energy Information Administration (EIA) 
a <- GET(api)

# Make data readable and clean for usability

data <- fromJSON(rawToChar(a$content)) # read JSON file
gas <- data$series$data[[1]]           # get relevant data
gasp <- rev(as.numeric(gas[,2]))       # reverse time order 
gasp <- gasp[complete.cases(gasp)]     # subset only complete cases (to avoid NA values)
gasp <- ts(gasp, start = c(1983, 10), end = c(2018, 12), freq = 12) # convert to time series object

ccf(gasp, elecp, type = "correlation", main = "Combined Correlation Function", lag.max = 25)

VAR_elecp <- ts(elecp, start = c(1983, 10), end = c(2018, 12), freq = 12) # subset electricity data to match time sequence of gas price data

X <- cbind(VAR_elecp, gasp)
X <- as.data.frame(X)

VARselect(X, lag.max = 15)$selection
mod_X <- VAR(X, p = 13) # choose based on AIC and SC criteria

plot(mod_X) # plot VAR fit and residuals

# l) Impulse Response Function Plot
plot(irf(mod_X)) # a shock on electricity prices leads to a small, decline in gas prices, while a shock in gas prices increases future gas prices in the short term which gradually declines in magnitude. There seems to be no significant effect of a shock on electricity prices.

# m) Granger-Causality Test
grangertest(VAR_elecp ~ gasp, order = 5) 
grangertest(gasp ~ VAR_elecp, order = 5)

plot(fevd(mod_X, n.ahead = 6)) # forecast error variance decomposition (FEVD)

# n) VAR Model Forecast
mod_VAR <- predict(object = mod_X, n.ahead = 12)
plot(mod_VAR)

fcst_VAR <- log(mod_VAR$fcst$VAR_elecp[,1])
MAPE_VAR <- mean(abs((tail(lelecp, 13)[-13] - fcst_VAR)/tail(lelecp, 13)[-13]))*100

c(MAPES, Combination = MAPE_combo, "VAR" = MAPE_VAR)

# o) Generalized Autoregressive Conditional Heteroskedasticity Model (GARCH) for Point and Variance Forecast
mod_GARCH <- ugarchspec(
      variance.model = list(model = "sGARCH", garchOrder = c(1,1)), 
      mean.model = list(armaOrder = c(5,5), include.mean = TRUE),
      distribution.model = "sstd") # set specs for GARCH(1,1) model

fit_GARCH <- ugarchfit(spec = mod_GARCH, data = lelecp) # fit data
plot(fit_GARCH, which = 2) # 1% bands
plot(fit_GARCH, which = 9) # quantile to standard normal quantile plot

fore_GARCH <- ugarchforecast(fit_GARCH, n.ahead = 12) # forecast using GARCH
plot(fore_GARCH, which = 1) # point forecasts plot
plot(fore_GARCH, which = 3) # variance forecasts plot

elecp_fcst2 <- t(exp(fore_GARCH@forecast$seriesFor))
elecp_fcst2                     # forecasts are logged values
t(fore_GARCH@forecast$sigmaFor) # variance forecasts

