# Chapter 14 Time Series Analysis
# Ref : Model applied statistics with S, 4ed
# Warning : there are some discrepencies in the R scripts and S-Plus scripts
# the data structure may also experience updates and changes

library(MASS) # contains 'deaths' data set
data(lh)
lh

data(deaths)
head(deaths)
deaths

# 14.4 Nottingham temperature data
# plot data
nott <- window(nottem, end=c(1936, 12))
ts.plot(nott)
nott
# decomposion of seasonal components
nott.stl <- stl(nott, "period")
names(nott.stl)
plot(nott.stl)
str(nott.stl)

# class(nott.stl$time.series)
# nott.stl$time.series[, "remainder"]
ts.plot(nott.stl$time.series[, 3], nott.stl$time.series[, 1])

nott.stl <- stl(nott, 5)
ts.plot(nott.stl$time.series[, 3], nott.stl$time.series[, 1])

boxplot(split(nott, cycle(nott)), names = month.abb)

# page 407
nott[110] <- 35
nott.stl <- stl(nott, "period")
nott1 <- nott.stl$time.series[, 3] - mean(nott.stl$time.series[, 3])

acf(nott1)
acf(nott1, type="partial")
cpgram(nott1)
ar(nott1)$aic

plot(0:23, ar(nott1)$aic, xlab = "order", ylab = "AIC", main = "AIC for AR(p)")

# ARIMA model
(nott1.ar1 <- arima(nott1, order = c(1,0,0)))

nott1.fore <- predict(nott1.ar1, 36)

nott1.fore$pred <- nott1.fore$pred + mean(nott.stl$time.series[, 3]) +
  as.vector(nott.stl$time.series[1:36,"seasonal"])

ts.plot(window(nottem, 1937), nott1.fore$pred, nott1.fore$pred + 2*nott1.fore$se,
        nott1.fore$pred - 2*nott1.fore$se, lty= c(3, 1, 2, 2))

title("via seasonal decomposition")

# Box-Jenkins
acf(diff(nott, 12), 30)
acf(diff(nott, 12), 30, type = "partial")
cpgram(diff(nott, 12))
(nott.arima1 <- arima(nott, order=c(1,0,0), list(order = c(2,1,0), period = 12)))

(nott.arima2 <- arima(nott, order=c(0,0,2), list(order = c(0,1,2), period = 12)))

tsdiag(nott.arima2, gof.lag = 30)

(nott.arima3 <- arima(nott, order=c(1,0,0), list(order = c(0,1,2), period = 12)))

tsdiag(nott.arima3, gof.lag = 30)

# prediction
nott.fore <- predict(nott.arima3, 36)

ts.plot(window(nottem, 1937), nott.fore$pred, nott.fore$pred + 2*nott.fore$se,
        nott.fore$pred - 2*nott.fore$se, lty= c(3, 1, 2, 2))

title("via seasonal decomposition")
