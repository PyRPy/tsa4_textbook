# Lesson 4: Seasonal Models

# 4.1 Seasonal ARIMA models -----------------------------------------------
# Example 4-2: ARIMA(1,0,0) x (1,0,0)12
thepacf=ARMAacf (ar = c(.6,0,0,0,0,0,0,0,0,0,0,.5,-.30),lag.max=30,pacf=T)
plot (thepacf,type="h")


# 4.2 Identifying Seasonal Models and R Code ------------------------------

# R code for the Colorado River Analysis
library(astsa)
flow <- ts(scan("coloradoflow.dat"))
plot(flow, type="b")

# examined the ACF and PACF of the 12th differences (seasonal differencing)
diff12 <- diff(flow, 12)
acf2(diff12, 48)

# possible model
sarima(flow, 1,0,0,0,1,1,12)

# generate forecasts for the next 24 months 
sarima.for(flow, 24, 1,0,0,0,1,1,12)

# command in R code directly
themodel = arima(flow, order=c(1,0,0), seasonal=list(order=c(0,1,1), period=12))
themodel
predict(themodel, n.ahead=24) 

# plot the monthly average
flowm <- matrix(flow, ncol=12, byrow = TRUE)
col.means <- apply(flowm, 2, mean)
plot(col.means, type = "b", main = "Monthly flow means ", 
     xlab = "Month", ylab = "Mean")

# Example 4-4: Beer Production in Australia 
## use data set ausbeer from fpp2 package for now
library(fpp2)
data("ausbeer")
plot(ausbeer)
beer <- window(ausbeer, start=1960, end=c(1975, 4))

diff4 <- diff(beer, 4)
plot(diff4)
diff1and4 <- diff(diff4, 1)
acf2(diff1and4)

# detrend by subtracting an estimated linear trend from each observation as follows
dtb <- residuals(lm (diff4~time(diff4)))
acf2(dtb)

# try the following models
sarima (dtb, 0,0,1,0,0,1,4)
sarima (dtb, 1,0,0,0,0,1,4)
sarima (dtb, 0,0,0,0,0,1,4)
sarima (dtb, 1,0,0,0,0,2,4)

# use auto.arima
library(forecast)
auto.arima(beer)
sarima(beer, 2,0,2, 0,1,1, 4)
## residuals variances not ideal
